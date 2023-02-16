module Test.CardanoRacers.Nitro.Types (nitroTypesSuite) where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(MintNitroToken, BuyNitroToken)
  , NitroScriptParams(NitroScriptParams)
  , NitroState(NitroState)
  , NitroStateRedeemer(SetNitroState)
  )
import Contract.Address (PubKeyHash(PubKeyHash))
import Contract.Credential (Credential(PubKeyCredential))
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Mote (TestPlanM)
import Contract.Value (mkCurrencySymbol, mkTokenName)
import Ctl.Internal.Plutus.Types.Address (Address(Address))
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBech32)
import Data.Bifunctor (lmap)
import Data.BigInt (fromInt) as BigInt
import Effect.Aff (error)
import Mote (test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

nitroTypesSuite :: TestPlanM (Aff Unit) Unit
nitroTypesSuite = do
  test "Aeson Encode/Decode of NitroScriptParams" do
    let nsp /\ encodedStr = nitroScriptParamsFixture
    show (encodeAeson nsp) `shouldEqual` encodedStr
    (decodedNsp :: NitroScriptParams) <- liftEither $ lmap (error <<< show) $
      decodeJsonString encodedStr
    decodedNsp `shouldEqual` nsp
  test "Aeson Encode/Decode of NitroState" do
    let ns /\ encodedStr = nitroStateFixture
    show (encodeAeson ns) `shouldEqual` encodedStr
    (decodedNs :: NitroState) <- liftEither $ lmap (error <<< show) $
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
  test "Aeson Encode/Decode of NitroStateRedeemer" do
    let (nsr /\ encodedStr) = nitroStateRedeemerFixture
    show (encodeAeson nsr) `shouldEqual` encodedStr
    (decodednsr :: NitroStateRedeemer) <- liftEither $ lmap (error <<< show) $
      decodeJsonString encodedStr
    decodednsr `shouldEqual` nsr

nitroScriptParamsFixture :: NitroScriptParams /\ String
nitroScriptParamsFixture =
  let
    jsonStr =
      "{\"NitroScriptParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"a130d78694de04aa1570706e1a4e7afa83e8d85ad1cba379b6135bb5\"},{\"unTokenName\":\"RacersNitroStateNFT\"}],\"nitroToken\":{\"unTokenName\":\"NITRO\"},\"botToken\":[{\"unCurrencySymbol\":\"8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302\"},{\"unTokenName\":\"RacersAdminNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
    csAdmin = unsafePartial $ fromJust $ mkCurrencySymbol $
      hexToByteArrayUnsafe
        "8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302"
    tkAdmin = unsafePartial $ fromJust $ mkTokenName $ hexToByteArrayUnsafe
      "52616365727341646d696e4e4654"
    csState = unsafePartial $ fromJust $ mkCurrencySymbol $
      hexToByteArrayUnsafe
        "a130d78694de04aa1570706e1a4e7afa83e8d85ad1cba379b6135bb5"
    tkState = unsafePartial $ fromJust $ mkTokenName $ hexToByteArrayUnsafe
      "5261636572734e6974726f53746174654e4654"
    nitroTk = unsafePartial $ fromJust $ mkTokenName $ hexToByteArrayUnsafe
      "4e4954524f"
    nsp = NitroScriptParams
      { adminToken: csAdmin /\ tkAdmin
      , botToken: csAdmin /\ tkAdmin
      , stateToken: csState /\ tkState
      , nitroToken: nitroTk
      }
  in
    nsp /\ jsonStr

nitroStateFixture :: NitroState /\ String
nitroStateFixture =
  let
    jsonStr =
      "{\"NitroState\":{\"treasuryAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"operatingAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"nitroPrice\":1000000}}"
    treasuryAddress =
      Address
        { addressCredential: PubKeyCredential
            ( PubKeyHash $ unsafePartial $ fromJust $ ed25519KeyHashFromBech32
                "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"
            )
        , addressStakingCredential: Nothing
        }
    ns = NitroState
      { nitroPrice: BigInt.fromInt 1000000
      , treasuryAddress
      , operatingAddress: treasuryAddress
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

nitroStateRedeemerFixture :: NitroStateRedeemer /\ String
nitroStateRedeemerFixture =
  let
    ns = fst nitroStateFixture
    setNsRedStr =
      "{\"SetNitroState\":{\"NitroState\":{\"treasuryAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"operatingAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"nitroPrice\":1000000}}}"
  in
    SetNitroState ns /\ setNsRedStr

-- nitroPolicyRedeemerFixture :: NitroPolicyRedeemer /\ String
-- nitroPolicyRedeemerFixture = "" /\ ""
