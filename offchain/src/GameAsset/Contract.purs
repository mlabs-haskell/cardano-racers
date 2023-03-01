module CardanoRacers.GameAsset.Contract (mintNewCarNft, mintNewDriverNft) where

import CardanoRacers.GameAsset.Parameters (Rarity, generateUniformParameters)
import CardanoRacers.GameAsset.Types
  ( Car(Car)
  , Driver(Driver)
  , GameAssetNftMetadata(GameAssetNftMetadata)
  , GameAssetNftMetadataEntry(DriverNftMetadata, CarNftMetadata)
  )
import CardanoRacers.Nft (mintNftConstraints)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Log (logInfo')
import Contract.Metadata (mkCip25String)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prelude
  ( Effect
  , Maybe
  , bind
  , discard
  , fst
  , liftEffect
  , map
  , pure
  , show
  , ($)
  , (/\)
  , (<#>)
  , (<=<)
  , (<>)
  , (>>=)
  , (>>>)
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.Utxos (getWalletUtxos)
import Contract.Value (mkTokenName)
import Control.Monad.Error.Class (throwError)
import Data.Array (head) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable) as Map
import Effect.Exception (error)
import Random.LCG (randomSeed)

generateNewDriver :: Rarity -> Effect Driver
generateNewDriver rarity = do
  seed <- randomSeed
  let ps = generateUniformParameters seed rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    driver = Driver
      { driverId: show rarity <> "Driver"
      , aggression: BigInt.fromInt p1
      , experience: BigInt.fromInt p2
      , reflexes: BigInt.fromInt p3
      , luck: BigInt.fromInt p4
      }
  pure driver

generateNewCar :: Rarity -> Effect Car
generateNewCar rarity = do
  seed <- randomSeed
  let ps = generateUniformParameters seed rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    car = Car
      { carId: show rarity <> "Car"
      , acceleration: BigInt.fromInt p1
      , cornering: BigInt.fromInt p2
      , topSpeed: BigInt.fromInt p3
      , aerodynamics: BigInt.fromInt p4
      }
  pure car

type MintAssetNftOptions =
  { tokenNameStr :: String
  , nameStr :: String
  , image :: String
  , mediaType :: Maybe String
  , description :: Maybe String
  , rarity :: Rarity
  }

mintNewDriverNft :: MintAssetNftOptions -> Contract TransactionHash
mintNewDriverNft opts = do
  driver <- liftEffect $ generateNewDriver opts.rarity
  tk <- liftContractM "could not make token name"
    $ (mkTokenName <=< byteArrayFromAscii)
    $ opts.tokenNameStr
  txi <- liftedM "could not get first txi" $ getWalletUtxos <#>
    (_ >>= Map.toUnfoldable >>> map fst >>> Array.head)
  (cs /\ constrants /\ lookups) <- mintNftConstraints txi tk
  cip25Name <- liftContractM "could not make cip25 name" $ mkCip25String
    opts.nameStr
  let
    driverMetadata = DriverNftMetadata
      { driver
      , assetClass: cs /\ tk
      , name: cip25Name
      , image: opts.image
      , mediaType: opts.mediaType >>= mkCip25String
      , description: opts.description
      }

  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constrants
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx $ GameAssetNftMetadata
    [ driverMetadata ]
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ "Tx ID: " <> show txId
  pure txId

mintNewCarNft :: MintAssetNftOptions -> Contract TransactionHash
mintNewCarNft opts = do
  car <- liftEffect $ generateNewCar opts.rarity
  tk <- liftContractM "could not make token name"
    $ (mkTokenName <=< byteArrayFromAscii)
    $ opts.tokenNameStr
  txi <- liftedM "could not get first txi" $ getWalletUtxos <#>
    (_ >>= Map.toUnfoldable >>> map fst >>> Array.head)
  (cs /\ constrants /\ lookups) <- mintNftConstraints txi tk
  cip25Name <- liftContractM "could not make cip25 name" $ mkCip25String
    opts.nameStr
  let
    carMetadata = CarNftMetadata
      { car
      , assetClass: cs /\ tk
      , name: cip25Name
      , image: opts.image
      , mediaType: opts.mediaType >>= mkCip25String
      , description: opts.description
      }

  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constrants
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx $ GameAssetNftMetadata
    [ carMetadata ]
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ "Tx ID: " <> show txId
  pure txId
