export interface RacersQueries {
  /**
  * @returns the current onchain price of Nitro in Lovelace
  */
  getNitroPrice(): Promise<Lovelace>;

  /**
  * @returns the current onchain price of assets by rarity class in Lovelace
  */
  getAssetPrices(): Promise<AssetPrices>;

  /**
  * @returns the current treasury address
  */
  getTreasuryAddress(): Promise<Address>;

  /**
  * @returns the current operating address
  */
  getOperatingAddress(): Promise<Address>;

  /**
  * Queries the given race script and returns registered and participated entries
  * Does not process entries in any way (duplicates are not filtered out)
  * @param {Race} race The raceId and nitroFee that identifies the specific race
  * @returns A list of all registered and participated entries
  */
  queryRaceRegistry(race: Race): Promise<RegistryEntry[]>
}


/**
 * mkWalletSpec - Creates a wallet specification object with functions to create a wallet from various inputs.
 * 
 * @returns {object} - Returns an object with the following properties:
 * - walletFromMnemonic: Function to create a wallet from a mnemonic string. Takes four arguments: mnemonic string, account index, address index, and a boolean indicating if the wallet has a stake.
 * - walletFromPrivateKey: Function to create a wallet from a private key string.
 * - walletFromPrivateKeyAndStakeKey: Function to create a wallet from a private key and a stake key strings.
 * - browserWallet: An object mapping wallet names to functions that connect to these wallets.
 * 
 * @throws Will throw an error if the provided mnemonic or private keys cannot be correctly converted into a wallet.
 */
function mkWalletSpec(): object;

/**
 * mkRacersParams - Function to create a RacersParams instance from a JSON string.
 * 
 * @param {string} rpStr - The JSON string to be converted into RacersParams.
 * 
 * @returns {RacersParams} - Returns an instance of RacersParams created from the provided JSON string.
 * 
 * @throws Will throw an error if the provided JSON string cannot be correctly decoded into a RacersParams instance.
 */
function mkRacersParams(rpStr: string): RacersParams;

export type Nitro = BigInt
export type Lovelace = BigInt
export type Address = string // bech32 encoded address string
export type TokenName = string // ASCII reperesenation of the token name

export type Rarity = "common" | "rare" | "epic"
export type AssetPrices = { [key in Rarity]: Lovelace; }

export type PubKeyHash = string;

export type TransactionHash = string; // Transaction ID as hex string

export type GameAssetType = "driver" | "car"

export type TxOutReference = string

export type Registered = {
  address: Address;
}

export type AssetSelection = {
  car: string;
  driver: string;
  address: Address,
}

export type RegistryEntry = { 'registered': PubKeyHash } | { 'assetSelection': AssetSelection }

export type Race = {
  raceId: Uint8Array;
  nitroFee: Nitro;
}

export type GameAsset = 
  { assetType: GameAssetType
  , attributes: any
  , imageUrl: string
  , name: string
  , tokenName: TokenName
  , description: string
  }

export type AssetOption = { 
  name: string, // "Mustang"
  assetType: GameAssetType,
  imageUrl: string,
  description: string,
  nitroAmount: Nitro,
}

export type AssetRequest = {
  rarity: Rarity;
  address: Address;
}

export type InitialState = {
  treasuryAddress: Address;
  operatingAddress: Address;
  nitroPrice: Lovelace; // in lovelace 1,000,000 == 1 Ada
  assetPrices: AssetPrices;
}

export type RacersParams = string; // json string
