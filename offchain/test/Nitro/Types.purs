module Test.CardanoRacers.Nitro.Types (suite) where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(MintNitroToken, BuyNitroToken)
  )
import CardanoRacers.RacersState.Types
  ( RacersState(RacersState)
  , RacersStateRedeemer(SetRacersState)
  )
import Contract.Address (PubKeyHash(PubKeyHash))
import Contract.AssocMap as Map
import Contract.Credential (Credential(PubKeyCredential))
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (ValidatorHash(ValidatorHash))
import Contract.Test.Mote (TestPlanM)
import Contract.Value (mkCurrencySymbol, mkTokenName)
import Ctl.Internal.Plutus.Types.Address (Address(Address))
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashFromBech32
  , scriptHashFromBytes
  )
import Data.Bifunctor (lmap)
import Data.BigInt (fromInt) as BigInt
import Effect.Aff (error)
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
      "{\"RacersParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"a130d78694de04aa1570706e1a4e7afa83e8d85ad1cba379b6135bb5\"},{\"unTokenName\":\"RacersStateNFT\"}],\"nitroToken\":{\"unTokenName\":\"NITRO\"},\"botToken\":[{\"unCurrencySymbol\":\"8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302\"},{\"unTokenName\":\"RacersAdminNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"8028e67f22ae8dcc562eb486977642c8a273ac96f9394b5905989302\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
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
    nitroTk = unsafePartial $ fromJust $ mkTokenName $ hexToByteArrayUnsafe
      "4e4954524f"
    nsp = RacersParams
      { adminToken: csAdmin /\ tkAdmin
      , botToken: csAdmin /\ tkAdmin
      , stateToken: csState /\ tkState
      , nitroToken: nitroTk
      }
  in
    nsp /\ jsonStr

nitroStateFixture :: RacersState /\ String
nitroStateFixture =
  let
    jsonStr =
      "{\"RacersState\":{\"treasuryAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"operatingAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"nitroPrice\":1000000,\"depositScript\":\"1f6f394b032b42d1be3abb7bf6db1355ec6753f30d56d265fb826aa8\",\"assetPrices\":[]}}"
    treasuryAddress =
      Address
        { addressCredential: PubKeyCredential
            ( PubKeyHash $ unsafePartial $ fromJust $ ed25519KeyHashFromBech32
                "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"
            )
        , addressStakingCredential: Nothing
        }

    depositScriptHash :: ValidatorHash
    depositScriptHash = ValidatorHash $ unsafePartial $ fromJust
      $ scriptHashFromBytes
      $ hexToByteArrayUnsafe
          "1f6f394b032b42d1be3abb7bf6db1355ec6753f30d56d265fb826aa8"

    ns = RacersState
      { nitroPrice: BigInt.fromInt 1000000
      , treasuryAddress
      , operatingAddress: treasuryAddress
      , assetPrices: Map.empty
      , depositScript: depositScriptHash
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
      "{\"SetRacersState\":{\"RacersState\":{\"treasuryAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"operatingAddress\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb3\"}}},\"nitroPrice\":1000000,\"depositScript\":\"1f6f394b032b42d1be3abb7bf6db1355ec6753f30d56d265fb826aa8\",\"assetPrices\":[]}}}"
  in
    SetRacersState ns /\ setNsRedStr
