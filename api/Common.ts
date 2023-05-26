export interface RacersQueries {
  // Reads
  //   Racers State
  // these can be inherited
  getNitroPrice(): Promise<Lovelace>;
  getAssetPrices(): Promise<AssetPrices>;
  getTreasuryAddress(): Promise<Address>;
  getOperatingAddress(): Promise<Address>;
  queryRaceRegistry(race: Race): Promise<RegistryEntry[]> // duplicates need to be checked by caller, otherwise a separate query is needed to get accurate available slot count
}

export type Registered = {
  address: Address;
}

export type AssetsSelected = {
  driver: string,
  car: string,
  address: Address,
}

export type RegistryEntry = Registered | AssetsSelected [car , driver, address]

export type Race = {
  raceId: UUID;
  nitroFee: Nitro;
}

export type Rarity = "common" | "rare" | "epic"

export type GameAssetType = "driver" | "car"

export type GameAsset = 
  { assetType : GameAssetType
  , attributes : any
  , imageUrl : string
  , name : string
  , tokenName : string
  , description : String
  }

export type AssetOption = { 
  name: String, // "Mustang"
  assetType: GameAssetType,
  imageUrl: String,
  description: String,
  nitroAmount: Nitro,
}

export type AssetRequest = {
  rarity: Rarity;
  address: Address;
}

export type AssetPrices = {
  common: Lovelace;
  rare: Lovelace;
  epic: Lovelace;
}

export type InitialState = {
  treasuryAddress: Address;
  operatingAddress: Address;
  nitroPrice: Lovelace; // in lovelace 1,000,000 == 1 Ada
  assetPrices: AssetPrices;
}

export type Address = string;

export type Nitro = BigInt
export type Lovelace = BigInt

export type RacersParams = string; // json string
