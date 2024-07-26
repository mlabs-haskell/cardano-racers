module Test.CardanoRacers.Nitro.Types (suite) where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import Cardano.Plutus.Types.Address (Address(Address))
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential))
import Cardano.Plutus.Types.CurrencySymbol (mkCurrencySymbol)
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash))
import Cardano.Plutus.Types.TokenName (mkTokenName)
import Cardano.Types.BigInt (fromInt) as BigInt
import Cardano.Types.Ed25519KeyHash as Ed25519KeyHash
import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(MintNitroToken, BuyNitroToken)
  )
import CardanoRacers.RacersState.Types
  ( AssetPrices(AssetPrices)
  , RacersState(RacersState)
  , RacersStateRedeemer(SetRacersState)
  )
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Mote (TestPlanM)
import Data.Bifunctor (lmap)
import Effect.Aff (error)
import JS.BigInt (fromInt) as JSBigInt
import Mote (test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  test "Aeson Encode/Decode of RacersParams" do
    let nsp /\ encodedStr = nitroScriptParamsFixture
    show (encodeAeson nsp) `shouldEqual` encodedStr
    (decodedNsp :: RacersParams) <- liftEither $ lmap (error <<< show) $
      decodeJsonString encodedStr
    decodedNsp `shouldEqual` nsp
  test "Aeson Encode/Decode of RacersState" do
    let ns /\ encodedStr = nitroStateFixture
    show (encodeAeson ns) `shouldEqual` encodedStr
    (decodedNs :: RacersState) <- liftEither $ lmap (error <<< show) $
      decodeJsonString encodedStr
    decodedNs `shouldEqual` ns
  test "Aeson Encode/Decode of NitroPolicyRedeemer" do
    let fs = nitroPolicyRedeemerFixtures
    for_ fs \(npr /\ encodedStr) -> do
      show (encodeAeson npr) `shouldEqual` encodedStr
      (decodedNpr :: NitroPolicyRedeemer) <- liftEither $ lmap (error <<< show)
        $
          decodeJsonString encodedStr
      decodedNpr `shouldEqual` npr
  test "Aeson Encode/Decode of RacersStateRedeemer" do
    let (nsr /\ encodedStr) = nitroStateRedeemerFixture
    show (encodeAeson nsr) `shouldEqual` encodedStr
    (decodednsr :: RacersStateRedeemer) <- liftEither $ lmap (error <<< show) $
      decodeJsonString encodedStr
    decodednsr `shouldEqual` nsr

nitroScriptParamsFixture :: RacersParams /\ String
nitroScriptParamsFixture =
  let
    jsonStr =
      "{\"RacersParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"a130d78694de04aa1570706e1a4e7afa83e8d85ad1cba379b6135bb5\"},{\"unTokenName\":\"RacersStateNFT\"}],\"botToken\":[{\"unCurrencySymbol\":\"8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302\"},{\"unTokenName\":\"RacersAdminNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
    csAdmin = unsafePartial $ fromJust $ mkCurrencySymbol $
      hexToByteArrayUnsafe
        "8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302"
    tkAdmin = unsafePartial $ fromJust $ mkTokenName $ hexToByteArrayUnsafe
      "52616365727341646d696e4e4654"
    csState = unsafePartial $ fromJust $ mkCurrencySymbol $
      hexToByteArrayUnsafe
        "a130d78694de04aa1570706e1a4e7afa83e8d85ad1cba379b6135bb5"
    tkState = unsafePartial $ fromJust $ mkTokenName $ hexToByteArrayUnsafe
      "52616365727353746174654E4654"
    nsp = RacersParams
      { adminToken: csAdmin /\ tkAdmin
      , botToken: csAdmin /\ tkAdmin
      , stateToken: csState /\ tkState
      }
  in
    nsp /\ jsonStr

nitroStateFixture :: RacersState /\ String
nitroStateFixture =
  let
    jsonStr =
      "{\"RacersState\":{\"treasuryAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"operatingAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"nitroPrice\":1000000,\"assetPrices\":{\"AssetPrices\":{\"rare\":2000000,\"epic\":3000000,\"common\":1000000}}}}"
    treasuryAddress =
      Address
        { addressCredential: PubKeyCredential
            ( PubKeyHash $ unsafePartial $ fromJust $ Ed25519KeyHash.fromBech32
                "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"
            )
        , addressStakingCredential: Nothing
        }

    ns = RacersState
      { nitroPrice: JSBigInt.fromInt 1000000
      , treasuryAddress
      , operatingAddress: treasuryAddress
      , assetPrices: AssetPrices
          { common: JSBigInt.fromInt 1000000
          , rare: JSBigInt.fromInt 2000000
          , epic: JSBigInt.fromInt 3000000
          }
      }
  in
    ns /\ jsonStr

nitroPolicyRedeemerFixtures :: Array (NitroPolicyRedeemer /\ String)
nitroPolicyRedeemerFixtures =
  let
    nprBuyStr = "{\"BuyNitroToken\":100}"
    nprMintStr = "{\"MintNitroToken\":100}"
    nprBuy = BuyNitroToken $ BigInt.fromInt 100
    nprMint = MintNitroToken $ BigInt.fromInt 100
  in
    [ nprBuy /\ nprBuyStr, nprMint /\ nprMintStr ]

nitroStateRedeemerFixture :: RacersStateRedeemer /\ String
nitroStateRedeemerFixture =
  let
    ns = fst nitroStateFixture
    setNsRedStr =
      "{\"SetRacersState\":{\"RacersState\":{\"treasuryAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"operatingAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"nitroPrice\":1000000,\"assetPrices\":{\"AssetPrices\":{\"rare\":2000000,\"epic\":3000000,\"common\":1000000}}}}}"
  in
    SetRacersState ns /\ setNsRedStr
