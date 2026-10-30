import type { Address, TransactionHash } from "./Common";

export type RaceParams = {
  stateCurrencySymbol: ScriptHash;
  totalRewardValue: Value;
  participants: Array<Address>;
  delegates: Array<Ed25519KeyHash>;
  escrowTtl: POSIXTime;
  feePerDelegate: Value | null;
  rewardWeights: Array<FixedDecimalN5>;
};

export type StartRaceParams = {
  raceId: ByteArray;
  totalRewardValue: Value;
  rewardWeights: Array<FixedDecimalN5>;
  participants: Array<RaceParticipant>;
  delegates: Array<Ed25519KeyHash>;
  feePerDelegate: Value | null;
};

export type RaceParticipant = {
  car: AssetName;
  driver: AssetName;
  payoutAddress: Address; 
};

export type StartRaceResult = {
  txHash: TransactionHash;
  raceParams: RaceParams;
};

export type FixedDecimalN5 = {
  numerator: bigint;
};

export type AssetName = string;

export type BigNum = string;

export type ByteArray = string;

export type Ed25519KeyHash = string;

export type POSIXTime = BigNum;

export type ScriptHash = string;

export type Value = {
  lovelace: BigNum;
  tokens: MultiAsset;
};

export type MultiAsset = Array<MultiAssetEntry>;

export type MultiAssetEntry = {
  policy: ScriptHash;
  name: AssetName;
  quantity: BigNum;
};
