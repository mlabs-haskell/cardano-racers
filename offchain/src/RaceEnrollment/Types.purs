module CardanoRacers.RaceEnrollment.Types where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import CardanoRacers.Helpers (decodeWrappedAeson, wrapEncodeAeson)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , S
  , Z
  , genericFromData
  , genericToData
  )
import Contract.Scripts (ValidatorHash)
import Control.Alt ((<|>))
import Foreign.Object (Object)

type RaceHash = String -- in hex

newtype EnrollmentPolicyParams = EnrollmentPolicyParams
  { registryVHash :: ValidatorHash
  , confirmationVHash :: ValidatorHash
  }

derive instance Generic EnrollmentPolicyParams _
derive instance Newtype EnrollmentPolicyParams _
derive instance Eq EnrollmentPolicyParams

instance
  HasPlutusSchema EnrollmentPolicyParams
    ( "EnrollmentPolicyParams"
        :=
          ( "registryVHash"
              := I ValidatorHash
              :+ "confirmationVHash"
              := I ValidatorHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance Show EnrollmentPolicyParams where
  show = genericShow

instance ToData EnrollmentPolicyParams where
  toData = genericToData

instance FromData EnrollmentPolicyParams where
  fromData = genericFromData

instance EncodeAeson EnrollmentPolicyParams where
  encodeAeson = wrapEncodeAeson "EnrollmentPolicyParams" <<< unwrap

instance DecodeAeson EnrollmentPolicyParams where
  decodeAeson = decodeWrappedAeson "EnrollmentPolicyParams" \obj -> do
    registryVHash <- obj .: "registryVHash"
    confirmationVHash <- obj .: "confirmationVHash"
    pure $ EnrollmentPolicyParams { registryVHash, confirmationVHash }

data EnrollmentPolicyRedeemer
  = MintInitialSlotTokens
  | ConfirmParticipation

derive instance Generic EnrollmentPolicyRedeemer _
derive instance Eq EnrollmentPolicyRedeemer
instance
  HasPlutusSchema EnrollmentPolicyRedeemer
    ( "MintInitialSlotTokens"
        := PNil
        @@ Z
        :+ "ConfirmParticipation"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData EnrollmentPolicyRedeemer where
  toData = genericToData

instance FromData EnrollmentPolicyRedeemer where
  fromData = genericFromData

instance Show EnrollmentPolicyRedeemer where
  show = genericShow

instance EncodeAeson EnrollmentPolicyRedeemer where
  encodeAeson MintInitialSlotTokens = wrapEncodeAeson "MintInitialSlotTokens" {}
  encodeAeson ConfirmParticipation = wrapEncodeAeson "ConfirmParticipation" {}

instance DecodeAeson EnrollmentPolicyRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "MintInitialSlotTokens"
      (constMono $ pure MintInitialSlotTokens)
      aes
      <|> decodeWrappedAeson "ConfirmParticipation"
        (constMono $ pure ConfirmParticipation)
        aes
    where
    constMono :: forall a. a -> Object {} -> a
    constMono a _ = a
