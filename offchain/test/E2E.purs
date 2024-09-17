-- | This module is used to serve the E2E tests to the headless browser.
module CardanoRacers.Test.E2E.Serve where

import Prelude

import Contract.Config
  ( ContractParams
  , KnownWallet(Nami, Gero, Flint, Eternl, Lode, Lace, NuFi)
  , WalletSpec(ConnectToGenericCip30)
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , mainnetConfig
  , mkBlockfrostBackendParams
  , testnetConfig
  , walletName
  )
import Contract.Monad (Contract)
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console as Console
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

main :: Effect Unit
main = do
  -- Read Blockfrost API key from the browser storage.
  -- To set it up, run `npm run e2e-browser` and follow the instructions.
  mbApiKey <- getBlockfrostApiKey
  let
    connectTo wallet =
      Just $ ConnectToGenericCip30 (walletName wallet) { cip95: false }
    walletsWithBlockfrost =
      wallets `Map.union`
        if isNothing mbApiKey then Map.empty
        else
          ( Map.fromFoldable
              [ "blockfrost-nami-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo Nami }
                  /\ Nothing
              , "blockfrost-gero-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo Gero }
                  /\ Nothing
              , "blockfrost-eternl-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo Eternl }
                  /\ Nothing
              , "blockfrost-lode-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo Lode }
                  /\ Nothing
              , "blockfrost-flint-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo Flint }
                  /\ Nothing
              , "blockfrost-nufi-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo NuFi }
                  /\ Nothing
              , "blockfrost-lace-preview"
                  /\ (mkBlockfrostPreviewConfig mbApiKey)
                    { walletSpec = connectTo Lace }
                  /\ Nothing
              , "blockfrost-nami-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo Nami }
                  /\ Nothing
              , "blockfrost-gero-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo Gero }
                  /\ Nothing
              , "blockfrost-eternl-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo Eternl }
                  /\ Nothing
              , "blockfrost-lode-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo Lode }
                  /\ Nothing
              , "blockfrost-flint-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo Flint }
                  /\ Nothing
              , "blockfrost-nufi-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo NuFi }
                  /\ Nothing
              , "blockfrost-lace-preprod"
                  /\ (mkBlockfrostPreprodConfig mbApiKey)
                    { walletSpec = connectTo Lace }
                  /\ Nothing
              ] :: Map E2EConfigName (ContractParams /\ Maybe String)
          )
  addLinks walletsWithBlockfrost tests
  route walletsWithBlockfrost tests

getBlockfrostApiKey :: Effect (Maybe String)
getBlockfrostApiKey = do
  storage <- localStorage =<< window
  res <- getItem "BLOCKFROST_API_KEY" storage
  when (isNothing res) do
    Console.log
      "Set BLOCKFROST_API_KEY LocalStorage key to use Blockfrost services."
    Console.log "Run this in the browser console:"
    Console.log "  localStorage.setItem('BLOCKFROST_API_KEY', 'your-key-here');"
  pure res

wallets :: Map E2EConfigName (ContractParams /\ Maybe String)
wallets = map (map walletName) <$> Map.fromFoldable
  [ "nami" /\ testnetConfig' Nami /\ Nothing
  , "gero" /\ testnetConfig' Gero /\ Nothing
  , "lode" /\ testnetConfig' Lode /\ Nothing
  , "nami-mainnet" /\ mainnetNamiConfig /\ Nothing
  , "nami-mock" /\ testnetConfig' Nami /\ Just Nami
  , "gero-mock" /\ testnetConfig' Gero /\ Just Gero
  , "flint-mock" /\ testnetConfig' Flint /\ Just Flint
  , "lode-mock" /\ testnetConfig' Lode /\ Just Lode
  -- Testnet cluster's network ID is set to mainnet:
  , "plutip-nami-mock" /\ testnetConfig' Nami /\ Just Nami
  , "plutip-gero-mock" /\ testnetConfig' Gero /\ Just Gero
  , "plutip-flint-mock" /\ testnetConfig' Flint /\ Just Flint
  , "plutip-lode-mock" /\ testnetConfig' Lode /\ Just Lode
  ]
  where
  testnetConfig' :: KnownWallet -> ContractParams
  testnetConfig' wallet =
    testnetConfig
      { walletSpec =
          Just $ ConnectToGenericCip30 (walletName wallet) { cip95: false }
      }

  mainnetNamiConfig :: ContractParams
  mainnetNamiConfig =
    mainnetConfig
      { walletSpec =
          Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
      }

mkBlockfrostPreviewConfig :: Maybe String -> ContractParams
mkBlockfrostPreviewConfig apiKey =
  testnetConfig
    { backendParams = mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicPreviewServerConfig
        , blockfrostApiKey: apiKey
        , confirmTxDelay: Just (Seconds 30.0)
        }
    }

mkBlockfrostPreprodConfig :: Maybe String -> ContractParams
mkBlockfrostPreprodConfig apiKey =
  testnetConfig
    { backendParams = mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicPreprodServerConfig
        , blockfrostApiKey: apiKey
        , confirmTxDelay: Just (Seconds 30.0)
        }
    }

tests :: Map E2ETestName (Contract Unit)
tests = Map.fromFoldable
  [ -- "Contract" /\ Scaffold.contract
  -- Add more `Contract`s here
  ]
