import './Common.ts'

// Runs in the browser, connects to a browser wallet and exposes racers
// specific functionality

class Client implements RacersQueries {
  constructor() {}

  async initClient(racersParams: RacersParams, walletId: WalletId): Promise<Client>;

  // writes
  async buyNitro(amount: Nitro): Promise<TransactionId>;
  async requestAsset(rarity: Rarity): Promise<TransactionId>;

  async registerInRace(race: Race): Promise<TransactionId>;
  async joinRace(race: Race, car: string, driver: string): Promise<TransactionId>;
}

type AssetMetadata = {
  name: Cip25String;
  assetType: AssetType;
  imageUrl: String;
  description: String;
  attributes: any;
}

type AssetType = "driver" | "car"

type Race = {
  raceId: Uint8Array;
  nitroFee: number;
}

type WalletId = "nami" | "gerowallet" | "flint" | "LodeWallet" | "eternl"
