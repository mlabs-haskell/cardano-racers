import './Common.ts'

// Runs in the browser, connects to a browser wallet and exposes racers
// specific functionality

class Client implements RacersQueries {
  // constructor() {}

  static async initClient(cfg: ContractConfig, racersParams: RacersParams, walletId: WalletId): Promise<Client>;

  // writes
  async buyNitro(amount: Nitro): Promise<TransactionHash>;
  async requestAsset(rarity: Rarity): Promise<TransactionHash>;

  async registerInRace(race: Race): Promise<TransactionHash>;
  async joinRace(race: Race, car: TokenName, driver: TokenName): Promise<TransactionHash>;
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
