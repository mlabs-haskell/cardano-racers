module CardanoRacers.Deposit.Contract
  ( queryRequestsWithAirdropAddress
  , consumeAndRedeemRequests
  , redeemGameAsset
  , PendingAssetRequest
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Plutus.Types.CurrencySymbol as CurrencySymbol
import Cardano.Plutus.Types.MintingPolicyHash
  ( MintingPolicyHash(MintingPolicyHash)
  )
import Cardano.Plutus.Types.Value (flattenValue) as Value
import Cardano.Plutus.Types.Value (fromCardano) as Plutus
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , Transaction
  , TransactionOutput
  )
import Cardano.Types.AssetName (mkAssetName, unAssetName)
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (hash)
import Cardano.Types.PlutusScript as PlutusScript
import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum
  , AssetRequestRedeemer(BurnRequestToken)
  )
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  )
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetNftMetadata
  , GameAssetObject
  , GameAssetType(DriverType, CarType)
  , Rarity(Epic, Rare, Common)
  , unGameAsset
  )
import CardanoRacers.Helpers (fromBIToInt, fromJSBIToBI)
import CardanoRacers.Nitro.Contract (paysNitroConstraints)
import CardanoRacers.RacersState.Contract (queryRacersRefScriptOutput)
import Common.ContractHelpers (findAuthInUtxosMap)
import Contract.Address (Address, getNetworkId, mkAddress)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.Monad (liftContractM, liftedE, liftedM)
import Contract.PlutusData (RedeemerDatum(..), fromData, toData, unitRedeemer)
import Contract.Prim.ByteArray (byteArrayToIntArray)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , createAdditionalUtxos
  , defaultBalancer
  , signTransaction
  , submit
  , withBalancedTx
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput))
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTxE)
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (TokenName)
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Reader.Trans (asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array
  ( catMaybes
  , concat
  , cons
  , drop
  , elem
  , filter
  , mapMaybe
  , snoc
  , take
  , uncons
  ) as Array
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt (toInt) as BigInt
import Data.Char (fromCharCode)
import Data.List.Lazy (replicateM)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map (fromFoldable, lookup, singleton, toUnfoldable) as Map
import Data.String.CodeUnits (fromCharArray)
import Data.TextEncoder (encodeUtf8)
import Effect.Aff (try)
import Effect.Exception (error)
import Lib.CardanoRacers.Common (mintingPolicyHash)
import Racers (Racers)

-- | Represents a request for a game NFT
type PendingAssetRequest =
  { airdropAddress :: Address
  , requestTxo :: TransactionOutput
  , requestedAssets :: Array (Rarity /\ BigInt)
  }

queryRequestsWithAirdropAddress
  :: Racers (Map TransactionInput PendingAssetRequest)
queryRequestsWithAirdropAddress = do

  depositScript <- (hash <<< unwrap) <$> mkDepositValidator

  (scriptAddress :: Address) <- lift $ mkAddress
    (wrap $ ScriptHashCredential $ depositScript)
    Nothing

  utxosAtDeposit <- lift $ utxosAt scriptAddress

  assetRequestPolicy <- mkAssetRequestPolicy
  assetRequestScript <- lift
    $ liftContractM "Could not get asset request script hash"
    $ unwrap
    <$> mintingPolicyHash assetRequestPolicy

  networkId <- lift $ getNetworkId

  let
    assetRequestSymbol = CurrencySymbol.fromScriptHash assetRequestScript
    (requestUtxos :: Array (TransactionInput /\ TransactionOutput)) =
      Array.filter
        ( Array.elem assetRequestSymbol <<< map fst
            <<< Value.flattenValue
            <<< Plutus.fromCardano
            <<< _.amount
            <<< unwrap
            <<< snd
        )
        $ Map.toUnfoldable utxosAtDeposit

    (pendingRequests :: Array (TransactionInput /\ PendingAssetRequest)) =
      Array.catMaybes $ requestUtxos <#> \(requestTxIn /\ requestTxOut) -> do
        let
          parsedRequestedAssets = Array.catMaybes
            $ map
                ( \(_ /\ tk /\ a) -> ado
                    r <- parseRequestToken (unwrap tk)
                    in
                      r /\ fromJSBIToBI a
                )
            $ Value.flattenValue
            $ Plutus.fromCardano (unwrap requestTxOut).amount
        txOutDat <- (unwrap requestTxOut).datum
        plutusDat <- outputDatumDatum txOutDat
        (addr :: PlutusAddress.Address) <-
          (\(ad :: AirdropAddressDatum) -> (unwrap ad).airdropAddress) <$>
            (fromData plutusDat :: Maybe AirdropAddressDatum)
        (airdropAddress :: Address) <- PlutusAddress.toCardano networkId addr

        pure $ requestTxIn /\
          { airdropAddress
          , requestTxo: requestTxOut
          , requestedAssets: parsedRequestedAssets
          }

  pure $ Map.fromFoldable pendingRequests
  where
  parseRequestToken :: TokenName -> Maybe Rarity
  parseRequestToken tk = do
    let
      tkBytes = unAssetName tk
      ia = byteArrayToIntArray tkBytes
    tkStr <- fromCharArray <$> traverse fromCharCode ia
    case tkStr of
      "Common" -> pure Common
      "Rare" -> pure Rare
      "Epic" -> pure Epic
      _ -> Nothing

-- | Processes a pending game asset request and airdrops the Game NFT to the
-- | depositor address.
redeemGameAsset
  :: Map Rarity AssetOption
  -> (AssetOption -> Aff String)
  -> Maybe (TransactionInput /\ TransactionOutput)
  -> Maybe (TransactionInput /\ TransactionOutput)
  -> Maybe (TransactionInput /\ TransactionOutput)
  -> Maybe (TransactionInput /\ TransactionOutput)
  -> (TransactionInput /\ UtxoMap)
  -> (TransactionInput /\ PendingAssetRequest)
  -> Racers (Transaction /\ UtxoMap /\ Array GameAssetObject)
redeemGameAsset
  availableAssets
  generateNonce
  mAssetRequestPolicyRef
  mDriverPolicyRef
  mCarPolicyRef
  mDepositRef
  (authTxi /\ additionalUtxos)
  (requestTxi /\ { airdropAddress, requestTxo, requestedAssets }) = do

  assetRequestMP <- mkAssetRequestPolicy
  assetRequestScriptHash <- lift
    $ liftContractM "Could not get script hash of asset request policy"
    $ head
    $ map PlutusScript.hash
    $ (unwrap assetRequestMP).plutusMintingPolicies

  driverAssetMp <- mkGameAssetPolicy DriverType
  driverScriptHash <- lift
    $ liftContractM "Could not get script hash of driver asset policy"
    $ head
    $ map PlutusScript.hash
    $ (unwrap driverAssetMp).plutusMintingPolicies

  carAssetMp <- mkGameAssetPolicy CarType
  carScriptHash <- lift
    $ liftContractM "Could not get script hash of car asset policy"
    $ head
    $ map PlutusScript.hash
    $ (unwrap carAssetMp).plutusMintingPolicies

  paysNitro <- do
    cs <- for requestedAssets $ \(rarity /\ count) -> do
      assetOption <- lift $ liftContractM "could not find asset option" $
        Map.lookup
          rarity
          availableAssets
      paysNitroConstraints airdropAddress (count * assetOption.nitroAmount)
    pure $ fold cs

  let
    -- Create and collect constraints to to mint game assets with metadata
    payAssetConstraintsAndMetadata
      :: Aff (Constraints.TxConstraints /\ GameAssetNftMetadata)
    payAssetConstraintsAndMetadata = do
      constraintsAndMetadata <- map join $ for requestedAssets $
        \(rarity /\ count) -> do
          assetOption <- liftMaybe (error "could not find rarity entry in map")
            $ Map.lookup rarity availableAssets
          countInt <- liftMaybe (error "could not convert BigInt to Int") $
            BigInt.toInt count
          let
            (gameAssetSymbol /\ mAssetPolicyRef) = case assetOption.assetType of
              DriverType -> driverScriptHash /\ mDriverPolicyRef
              CarType -> carScriptHash /\ mCarPolicyRef
          List.toUnfoldable <$> replicateM countInt
            ( do
                nonce <- generateNonce assetOption
                liftEffect $ mintAvailableAssetByRarity
                  ((MintingPolicyHash gameAssetSymbol /\ _) <$> mAssetPolicyRef)
                  assetOption
                  gameAssetSymbol
                  nonce
                  airdropAddress
                  rarity
            )
      pure $ foldMap fst constraintsAndMetadata /\ wrap
        (map snd constraintsAndMetadata)

    -- Constraints to ensure burning of request tokens
    burnsRequestTokensM :: Maybe (Constraints.TxConstraints)
    burnsRequestTokensM =
      fold <$> for requestedAssets \(rarity /\ count) ->
        let
          tokenNameStr = show rarity
          tkNameM = mkAssetName $ wrap $ encodeUtf8 $ tokenNameStr
          red = RedeemerDatum $ toData $ BurnRequestToken
          countI = fromBIToInt count
        in
          tkNameM <#> \tkName -> maybe
            ( Constraints.mustMintValueWithRedeemer red
                ( Mint.singleton assetRequestScriptHash tkName
                    (Int.negate countI)
                )
            )
            ( \(refTxi /\ (refTxo :: TransactionOutput)) ->
                Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
                  assetRequestScriptHash
                  red
                  tkName
                  (Int.negate countI)
                  (RefInput $ wrap { input: refTxi, output: refTxo })
            )
            mAssetRequestPolicyRef

  mintsAndPaysNft /\ allMetadata <- liftAff payAssetConstraintsAndMetadata

  burnsRequestTokens <- lift $ liftContractM "could not create token name"
    burnsRequestTokensM

  -- Constraints to ensure that the deposit script outputs are spent,
  -- favors reference script if passed, otherwise defaults to the validator
  -- script
  (depositConstraints /\ depositLookups) <- maybe
    ( do
        depositValidator <- mkDepositValidator
        pure $ Constraints.mustSpendScriptOutput requestTxi unitRedeemer
          /\ Lookups.validator (unwrap depositValidator)
    )
    ( \(refTxi /\ refTxo) ->
        pure
          $ Constraints.mustSpendScriptOutputUsingScriptRef
              requestTxi
              unitRedeemer
              (RefInput $ wrap { input: refTxi, output: refTxo })
          /\ mempty
    )
    mDepositRef

  let
    constraints :: Constraints.TxConstraints
    constraints = depositConstraints
      <> Constraints.mustSpendPubKeyOutput authTxi
      <> paysNitro
      <> mintsAndPaysNft
      <> burnsRequestTokens

    assetRequestPolicyLookups = maybe assetRequestMP (const mempty)
      mAssetRequestPolicyRef

    driverPolicyLookup = maybe driverAssetMp (const mempty) mDriverPolicyRef
    carPolicyLookup = maybe carAssetMp (const mempty) mCarPolicyRef

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs (Map.singleton requestTxi requestTxo)
      <> Lookups.unspentOutputs additionalUtxos
      <> driverPolicyLookup
      <> carPolicyLookup
      <> assetRequestPolicyLookups
      <> depositLookups

  lift do
    (unbalancedTx /\ usedUtxos) <- liftedE $ mkUnbalancedTxE lookups constraints
    let
      unbalancedTxWithMetadata = setTxMetadata unbalancedTx allMetadata
    pure
      ( unbalancedTxWithMetadata /\ usedUtxos /\ map
          (unGameAsset <<< _.asset <<< unwrap)
          (unwrap allMetadata)
      )

consumeAndRedeemRequests
  :: Int
  -> Maybe Int
  -> Map Rarity AssetOption
  -> (AssetOption -> Aff String)
  -> Racers (Array GameAssetObject)
consumeAndRedeemRequests chunkSize mMaxRequests availableAssets generateNonce =
  do
    assetRequestMP <- mkAssetRequestPolicy
    assetRequestScriptHash <- lift
      $ liftContractM "Could not get script hash of asset request policy"
      $ head
      $ map PlutusScript.hash
      $ (unwrap assetRequestMP).plutusMintingPolicies

    driverAssetMp <- mkGameAssetPolicy DriverType
    driverScriptHash <- lift
      $ liftContractM "Could not get script hash of driver asset policy"
      $ head
      $ map PlutusScript.hash
      $ (unwrap driverAssetMp).plutusMintingPolicies

    carAssetMp <- mkGameAssetPolicy CarType
    carScriptHash <- lift
      $ liftContractM "Could not get script hash of car asset policy"
      $ head
      $ map PlutusScript.hash
      $ (unwrap carAssetMp).plutusMintingPolicies

    depositValidator <- mkDepositValidator

    mAssetRequestPolicyRef <- queryRacersRefScriptOutput assetRequestScriptHash
    mDriverPolicyRef <- queryRacersRefScriptOutput driverScriptHash
    mCarPolicyRef <- queryRacersRefScriptOutput carScriptHash
    mDepositScriptRef <- queryRacersRefScriptOutput
      (hash $ unwrap depositValidator)

    pendingRequestsChunked <- chunkBy chunkSize
      <<< maybe identity Array.take mMaxRequests
      <<< (Map.toUnfoldable :: _ -> Array _)
      <$>
        queryRequestsWithAirdropAddress

    mintedAssets <- traverse
      ( \reqs -> do
          txsAndAssets <- consumeAndRedeemChained
            ( redeemGameAsset availableAssets generateNonce
                mAssetRequestPolicyRef
                mDriverPolicyRef
                mCarPolicyRef
                mDepositScriptRef
            )
            reqs
          lift do
            successfulSubmissions <- Array.mapMaybe hush <$> traverse
              (\(tx /\ asset) -> try $ submit tx <#> (_ /\ asset))
              txsAndAssets
            traverse_ awaitTxConfirmed $ fst <$> successfulSubmissions
            pure $ Array.concat $ snd <$> successfulSubmissions
      )
      pendingRequestsChunked

    pure $ Array.concat mintedAssets

  where
  -- | Process a series of PendingAssetRequests by chaining them together
  consumeAndRedeemChained
    :: ( (TransactionInput /\ UtxoMap)
         -> (TransactionInput /\ PendingAssetRequest)
         -> Racers (Transaction /\ UtxoMap /\ Array GameAssetObject)
       )
    -> Array (TransactionInput /\ PendingAssetRequest)
    -> Racers (Array (Transaction /\ Array GameAssetObject))
  consumeAndRedeemChained redeemTx pendingRequests = do
    rp <- asks _.params

    let
      loop additionalUtxos reqs acc = case Array.uncons reqs of
        Nothing -> pure acc
        Just { head: req, tail: rest } -> do
          let
            balancerConstraints =
              if null acc then mempty
              else BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos
          -- Create the unbalanced transaction by redeeming the current request.
          (unbalancedTx /\ extraUtxos /\ assets) <- do
            (authTxi /\ authTxo) <-
              liftContractM
                "could not get auth UTxO containing token (RacersAdminNFT/BotNFT) in current wallet UTxOs"
                $
                  (findAuthInUtxosMap rp additionalUtxos)
            runReaderT (redeemTx (authTxi /\ Map.singleton authTxi authTxo) req)
              { params: rp }

          withBalancedTx defaultBalancer unbalancedTx
            { balancerConstraints, extraUtxos } $ \balTx ->
            do
              balSignedTx <- signTransaction balTx
              additionalUtxos_ <- createAdditionalUtxos balSignedTx
              loop additionalUtxos_ rest
                (acc `Array.snoc` (balSignedTx /\ assets))

    -- Get the initial wallet UTXOs
    lift $ liftedM "could not get wallet utxos" getWalletUtxos >>=
      (\us -> loop us pendingRequests [])

  chunkBy :: forall a. Int -> Array a -> Array (Array a)
  chunkBy _ [] = []
  chunkBy n xs = Array.take n xs `Array.cons` chunkBy n (Array.drop n xs)
