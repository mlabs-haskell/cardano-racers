module CardanoRacers.Deposit.Contract
  ( queryRequestsWithAirdropAddress
  , consumeAndRedeemRequests
  , redeemGameAsset
  , mkDepositValidator
  ) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum
  , AssetRequestRedeemer(BurnRequestToken)
  )
import CardanoRacers.Deposit.Types (DepositScriptParams(DepositScriptParams))
import CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  )
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetNftMetadata
  , GameAssetObject
  , Rarity(Epic, Rare, Common)
  , unGameAsset
  )
import CardanoRacers.Nitro.Contract (paysNitroConstraints)
import CardanoRacers.RacersState.Contract (queryRacersRefScriptOutput)
import CardanoRacers.RacersState.Types (RacersState)
import CardanoRacers.ScriptsFFI (depositScript)
import Common.ContractHelpers (findAuthInUtxosMap)
import Contract.Address (Address, scriptHashAddress)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData
  ( OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (byteArrayFromAscii, byteArrayToIntArray)
import Contract.ScriptLookups (UnbalancedTx, mkUnbalancedTx)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , applyArgs
  , mintingPolicyHash
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , createAdditionalUtxos
  , mkTxUnspentOut
  , signTransaction
  , submit
  , withBalancedTxWithConstraints
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput))
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (TokenName)
import Contract.Value
  ( flattenValue
  , getTokenName
  , mkTokenName
  , negation
  , scriptCurrencySymbol
  , singleton
  ) as Value
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
  , snoc
  , take
  , uncons
  ) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt) as BigInt
import Data.Char (fromCharCode)
import Data.List.Lazy (replicateM)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map (fromFoldable, lookup, singleton, toUnfoldable) as Map
import Data.Profunctor.Choice (left)
import Data.String.CodeUnits (fromCharArray)
import Effect.Exception (error)
import Racers (Racers)

-- | Represents a request for a game NFT
type PendingAssetRequest =
  { airdropAddress :: Address
  , requestTxo :: TransactionOutputWithRefScript
  , requestedAssets :: Array (Rarity /\ BigInt)
  }

queryRequestsWithAirdropAddress
  :: RacersState
  -> Racers (Map TransactionInput PendingAssetRequest)
queryRequestsWithAirdropAddress st = do
  assetRequestPolicy <- mkAssetRequestPolicy
  assetRequestSymbol <- lift $ liftContractM "Could not get currency symbol"
    $ Value.scriptCurrencySymbol
    $ assetRequestPolicy

  utxosAtDeposit <- lift $ utxosAt $ scriptHashAddress (unwrap st).depositScript
    Nothing

  let
    requestUtxos =
      Array.filter
        ( Array.elem assetRequestSymbol <<< map fst <<< Value.flattenValue
            <<< _.amount
            <<< unwrap
            <<< _.output
            <<< unwrap
            <<< snd
        )
        $ Map.toUnfoldable utxosAtDeposit

    (pendingRequests :: Array (TransactionInput /\ PendingAssetRequest)) =
      Array.catMaybes $ requestUtxos <#> \(requestTxIn /\ requestTxOut) -> do
        let
          requestOutput = (unwrap requestTxOut).output
          parsedRequestedAssets = Array.catMaybes
            $ map
                ( \(_ /\ tk /\ a) -> ado
                    r <- parseRequestToken tk
                    in r /\ a
                )
            $ Value.flattenValue (unwrap requestOutput).amount
        airdropAddress <- case (unwrap requestOutput).datum of
          OutputDatum d -> (_.airdropAddress <<< unwrap) <$>
            (fromData (unwrap d) :: Maybe AirdropAddressDatum)
          _ -> Nothing

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
      tkBytes = Value.getTokenName tk
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
  -> Effect String
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> (TransactionInput /\ UtxoMap)
  -> (TransactionInput /\ PendingAssetRequest)
  -> Racers (UnbalancedTx /\ Array GameAssetObject)
redeemGameAsset
  availableAssets
  generateNonce
  mAssetRequestPolicyRef
  mGameAssetPolicyRef
  mDepositRef
  (authTxi /\ additionalUtxos)
  (requestTxi /\ { airdropAddress, requestTxo, requestedAssets }) = do
  assetRequestMP <- mkAssetRequestPolicy
  assetRequestSymbol <- lift
    $ liftContractM "could not get currency symbol of asset request policy"
    $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy
  gameAssetSymbol <- lift $ liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

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
      :: Effect (Constraints.TxConstraints Void Void /\ GameAssetNftMetadata)
    payAssetConstraintsAndMetadata = do
      constraintsAndMetadata <- map join $ for requestedAssets $
        \(rarity /\ count) -> do
          assetOption <- liftMaybe (error "could not find rarity entry in map")
            $ Map.lookup rarity availableAssets
          countInt <- liftMaybe (error "could not convert BigInt to Int") $
            BigInt.toInt count
          List.toUnfoldable <$> replicateM countInt
            ( do
                nonce <- generateNonce
                mintAvailableAssetByRarity
                  ((mintingPolicyHash gameAssetMP /\ _) <$> mGameAssetPolicyRef)
                  assetOption
                  gameAssetSymbol
                  nonce
                  airdropAddress
                  rarity
            )
      pure $ foldMap fst constraintsAndMetadata /\ wrap
        (map snd constraintsAndMetadata)

    -- Constraints to ensure burning of request tokens
    burnsRequestTokensM :: Maybe (Constraints.TxConstraints Void Void)
    burnsRequestTokensM =
      fold <$> for requestedAssets \(rarity /\ count) ->
        let
          tokenNameStr = show rarity
          tkNameM = Value.mkTokenName <=< byteArrayFromAscii $ tokenNameStr
          red = Redeemer $ toData $ BurnRequestToken
        in
          tkNameM <#> \tkName -> maybe
            ( Constraints.mustMintValueWithRedeemer red
                ( Value.negation $ Value.singleton assetRequestSymbol tkName
                    count
                )
            )
            ( \(refTxi /\ refTxo) ->
                Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
                  (mintingPolicyHash assetRequestMP)
                  red
                  tkName
                  ((BigInt.fromInt (-1)) * count)
                  (RefInput $ mkTxUnspentOut refTxi refTxo)
            )
            mAssetRequestPolicyRef

  mintsAndPaysNft /\ allMetadata <- liftEffect payAssetConstraintsAndMetadata

  burnsRequestTokens <- lift $ liftContractM "could not create token name"
    burnsRequestTokensM

  -- Constraints to ensure that the deposit script outputs are spent,
  -- favors reference script if passed, otherwise defaults to the validator
  -- script
  (depositConstraints /\ depositLookups) <- maybe
    ( do
        depositValidator <- mkDepositValidator
        pure $ Constraints.mustSpendScriptOutput requestTxi unitRedeemer
          /\ Lookups.validator depositValidator
    )
    ( \(refTxi /\ refTxo) ->
        pure
          $ Constraints.mustSpendScriptOutputUsingScriptRef
              requestTxi
              unitRedeemer
              (RefInput $ mkTxUnspentOut refTxi refTxo)
          /\ mempty
    )
    mDepositRef

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = depositConstraints
      <> Constraints.mustSpendPubKeyOutput authTxi
      <> paysNitro
      <> mintsAndPaysNft
      <> burnsRequestTokens

    assetRequestPolicyLookups = maybe (Lookups.mintingPolicy assetRequestMP)
      (const mempty)
      mAssetRequestPolicyRef

    gameAssetLookup = maybe (Lookups.mintingPolicy gameAssetMP) (const mempty)
      mGameAssetPolicyRef

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton requestTxi requestTxo)
      <> Lookups.unspentOutputs additionalUtxos
      <> gameAssetLookup
      <> assetRequestPolicyLookups
      <> depositLookups

  lift do
    unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
    unbalancedTxWithMetadata <- setTxMetadata unbalancedTx allMetadata
    pure
      ( unbalancedTxWithMetadata /\ map (unGameAsset <<< _.asset <<< unwrap)
          (unwrap allMetadata)
      )

consumeAndRedeemRequests
  :: Int
  -> Map Rarity AssetOption
  -> Effect String
  -> RacersState
  -> Racers (Array GameAssetObject)
consumeAndRedeemRequests chunkSize availableAssets generateNonce st =
  do
    assetRequestMP <- mkAssetRequestPolicy
    gameAssetMP <- mkGameAssetPolicy

    depositValidator <- mkDepositValidator

    mAssetRequestPolicyRef <- queryRacersRefScriptOutput
      (unwrap $ mintingPolicyHash assetRequestMP)
    mAssetPolicyRef <- queryRacersRefScriptOutput
      (unwrap $ mintingPolicyHash gameAssetMP)
    mDepositScriptRef <- queryRacersRefScriptOutput
      (unwrap $ validatorHash depositValidator)

    pendingRequestsChunked <- chunkBy chunkSize
      <<< (Map.toUnfoldable :: _ -> Array _)
      <$>
        queryRequestsWithAirdropAddress st

    mintedAssets <- traverse
      ( \reqs -> do
          txsAndAssets <- consumeAndRedeemChained
            ( redeemGameAsset availableAssets generateNonce
                mAssetRequestPolicyRef
                mAssetPolicyRef
                mDepositScriptRef
            )
            reqs
          lift do
            txIds <- traverse submit $ fst <$> txsAndAssets
            traverse_ awaitTxConfirmed txIds
            pure $ Array.concat $ snd <$> txsAndAssets
      )
      pendingRequestsChunked

    pure $ Array.concat mintedAssets

  where
  -- | Allows chaining of transactions together by processing 
  -- | an UnbalancedTx and returning the result in CPS.
  withChainedTx
    :: forall r
     . UnbalancedTx
    -> BalanceTxConstraints.BalanceTxConstraintsBuilder
    -> ( BalancedSignedTransaction
         -> UtxoMap
         -> Contract r
       )
    -> Contract r
  withChainedTx unbalancedTx balanceTxConstraintsBuilder k =
    do
      withBalancedTxWithConstraints unbalancedTx balanceTxConstraintsBuilder
        ( \balancedTx -> do
            balSignedTx <- signTransaction balancedTx
            additionalUtxos <- createAdditionalUtxos balSignedTx
            k balSignedTx additionalUtxos
        )

  -- | Process a series of PendingAssetRequests by chaining them together
  consumeAndRedeemChained
    :: ( (TransactionInput /\ UtxoMap)
         -> (TransactionInput /\ PendingAssetRequest)
         -> Racers (UnbalancedTx /\ Array GameAssetObject)
       )
    -> Array (TransactionInput /\ PendingAssetRequest)
    -> Racers (Array (BalancedSignedTransaction /\ Array GameAssetObject))
  consumeAndRedeemChained redeemTx pendingRequests = do
    rp <- asks _.params

    let
      loop additionalUtxos reqs acc = case Array.uncons reqs of
        Nothing -> pure acc
        Just { head: req, tail: rest } -> do
          let
            balanceTxConstraints =
              if null acc then mempty
              else BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos
          -- Create the unbalanced transaction by redeeming the current request.
          (unbalancedTx /\ assets) <- do
            (authTxi /\ authTxo) <-
              liftContractM
                "could not get auth UTxO containing token (RacersAdminNFT/BotNFT) in current wallet UTxOs"
                $
                  (findAuthInUtxosMap rp additionalUtxos)
            runReaderT (redeemTx (authTxi /\ Map.singleton authTxi authTxo) req)
              { params: rp }
          withChainedTx unbalancedTx balanceTxConstraints $
            \balSignedTx nextAdditionalUtxos ->
              loop nextAdditionalUtxos rest
                (acc `Array.snoc` (balSignedTx /\ assets))

    -- Get the initial wallet UTXOs
    lift $ liftedM "could not get wallet utxos" getWalletUtxos >>=
      (\us -> loop us pendingRequests [])

  chunkBy :: forall a. Int -> Array a -> Array (Array a)
  chunkBy _ [] = []
  chunkBy n xs = Array.take n xs `Array.cons` chunkBy n (Array.drop n xs)

mkDepositValidator
  :: Racers Validator
mkDepositValidator = do
  rp <- asks _.params

  assetRequestMP <- mkAssetRequestPolicy
  assetRequestSymbol <- lift
    $ liftContractM "Could not get currency symbol of asset request policy"
    $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy
  gameAssetSymbol <- lift
    $ liftContractM "Could not get currency symbol of game asset policy"
    $
      Value.scriptCurrencySymbol gameAssetMP

  let
    depositParams = DepositScriptParams
      { assetPolicySymbol: gameAssetSymbol
      , assetRequestPolicySymbol: assetRequestSymbol
      }

  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData depositParams ]
  pure $ Validator $ appliedScript
