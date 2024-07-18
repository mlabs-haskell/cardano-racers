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
  queryRaceRegistry(race: Race): Promise<RegistryEntry[]>;

  /**
   * Query available NITRO amount from the wallet
   */
  getWalletNitroBalance(): Promise<Nitro>;

  /**
   * Query available NFTs from the wallet.
   */
  getWalletNFTs(): Promise<WalletGameAsset[]>;

  /**
   * Query wallet address
   */
  getWalletAddress(): Promise<Address>;

  /**
   * Query wallet pubkeyhash (payment component of the address, used for
   * comparisons with results of `queryRaceRegistry`
   */
  getWalletPubKeyHash(): Promise<PubKeyHash>;
}

export type WalletSpec = any;

type MkWalletSpec = {
  walletFromMnemonic: (
    mnemonic: string,
    accountIndex: number,
    addressIndex: number,
    hasStake: boolean
  ) => WalletSpec;
  walletFromPrivateKey: (privateKey: string) => WalletSpec;
  walletFromPrivateKeyAndStakeKey: (
    privateKey: string,
    stakeKey: string
  ) => WalletSpec;
  browserWallet: {
    connectToNami: () => WalletSpec;
    connectToGeroWallet: () => WalletSpec;
    connectToFlint: () => WalletSpec;
    connectToEternl: () => WalletSpec;
    connectToLodeWallet: () => WalletSpec;
    connectToLace: () => WalletSpec;
    connectToVespr: () => WalletSpec;
    connectToNuFi: () => WalletSpec;
  };
};

export type ContractParams = any;

type ServerConfig = {
  port: number;
  host: string;
  secure: boolean;
  path?: string;
};

type ContractOpts = {
  networkId?: "testnet" | "mainnet";
  logLevel?: "trace" | "debug" | "info" | "warn" | "error";
};

type MkContractParams = {
  fromCtlBackend: (
    c: { ogmiosConfig: ServerConfig; kupoConfig: ServerConfig },
    o: ContractOpts
  ) => ContractParams;
  fromBlockfrostBackend: (
    b: {
      blockfrostConfig: ServerConfig;
      blockfrostApiKey: string;
      confirmTxDelay: number;
    },
    o: ContractOpts
  ) => ContractParams;
};

export interface RacersUtils {
  /**
   * Creates a wallet specification object with functions to create a wallet from various inputs.
   *
   * @returns {object} - Returns an object with the following properties:
   * - walletFromMnemonic: Function to create a wallet from a mnemonic string. Takes four arguments: mnemonic string, account index, address index, and a boolean indicating if the wallet has a stake.
   * - walletFromPrivateKey: Function to create a wallet from a private key string.
   * - walletFromPrivateKeyAndStakeKey: Function to create a wallet from a private key and a stake key strings.
   * - browserWallet: An object mapping wallet names to functions that connect to these wallets.
   *
   * @throws Will throw an error if the provided mnemonic or private keys cannot be correctly converted into a wallet.
   */
  walletSpec: MkWalletSpec;

  /**
   * Creates contract paramteres based on selected backend and options.
   *
   * @returns {object} - Returns an object with the following properties:
   *   - fromCtlBackend: Function to create contract parameters from a Ogmios and Kupo backend configuration.
   *   - fromBlockfrostBackend: Function to create contract parameters from a Blockfrost backend configuration.
   */
  contractParams: MkContractParams;

  /**
   * mkRacersParams - Function to create a RacersParams instance from a JSON string.
   *
   * @param {string} rpStr - The JSON string to be converted into RacersParams.
   *
   * @returns {RacersParams} - Returns an instance of RacersParams created from the provided JSON string.
   *
   * @throws Will throw an error if the provided JSON string cannot be correctly decoded into a RacersParams instance.
   */
  mkRacersParams: (rpStr: string) => RacersParams;
}

export type Nitro = BigInt;
export type Lovelace = BigInt;
export type Address = string; // bech32 encoded address string
export type TokenName = string; // ASCII reperesenation of the token name

export type Rarity = "common" | "rare" | "epic";
export type AssetPrices = { [key in Rarity]: Lovelace };

export type InitialState = {
  treasuryAddress: Address;
  operatingAddress: Address;
  assetPrices: AssetPrices;
  nitroPrice: Lovelace;
};

export type PubKeyHash = string;

export type TransactionHash = string; // Transaction ID as hex string

export type GameAssetType = "driver" | "car";

export type TxOutReference = string;

export type AssetSelection = {
  car: string;
  driver: string;
  address: Address;
};

export type RegistryEntry =
  | { registered: PubKeyHash }
  | { assetSelection: AssetSelection };

export type Race = {
  raceId: Uint8Array;
  nitroFee: Nitro;
};

export type WalletGameAsset = {
  assetType: GameAssetType;
  name: string;
};

export type GameAsset = {
  assetType: GameAssetType;
  attributes: any;
  imageUrl: string;
  name: string;
  tokenName: TokenName;
  description: string;
};

export type AssetOption = {
  name: string; // "Mustang"
  assetType: GameAssetType;
  imageUrl: string;
  description: string;
  nitroAmount: Nitro;
};

export type AssetRequest = {
  rarity: Rarity;
  address: Address;
};

export type RaceSlotOutput = {
  slotTxIn: string,
  slotCount: number,
  registrations: RegistryEntry[],
}

export type RacersParams = string; // json string
