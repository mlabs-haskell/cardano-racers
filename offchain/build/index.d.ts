declare module "cardano-racers" {
  export const admin: MkAdmin;
  export const bot: MkBot;
  export const client: MkClient;

  export interface MkAdmin extends RacersUtils {
    /**
     * Initializes a Contract environment for the given contract parameters and
     * wallet spec. Returns an instance of Admin when initialized.
     */
    mkAdmin: (
      cp: ContractParams,
      w: WalletSpec,
      racersParams: RacersParams
    ) => Admin;

    /**
     * initRacers - Initialize a Racers instance. Mints AdminNFT, BotNFT, StateNFT.
     * Pays StateNFT to state script. Creates reference scripts for NitroPolicy, AssetRequestPolicy, DriverAssetPolicy, CarAssetPolicy, DepositScript
     * BotNFT remains in the wallet balance and should be sent to the Bot address
     * manually.
     *
     * @param {WalletSpec} walletSpec - Specifies the wallet for the operation.
     * @param {InitialStateFFI} initialState - The initial state for the Racers instance, which includes:
     * - treasuryAddress: The address of the treasury
     * - operatingAddress: The operating address
     * - assetPrices: The initial asset prices
     * - nitroPrice: The initial price of nitro
     *
     * @returns {Promise<RacersParams>} - Returns a promise that resolves to the parameters of the created Racers instance.
     */
    initRacers: (
      cp: ContractParams,
      w: WalletSpec,
      initialState: InitialState
    ) => Promise<RacersParams>;
  }

  export interface Admin extends RacersQueries {
    /*
     * - Mints AdminNFT, BotNFT, StateNFT.
     * - Pays AdminNFT to admin address
     * - Creates reference scripts for NitroPolicy, AssetRequestPolicy,
     *   DepositScript, DriverAssetPolicy, CarAssetPolicy
     * - Sets initial racers state
     */
    // static async initRacers(cfg: ContractConfig, initialState: InitialState, credentialProvider: Wallet): Promise<RacersParams>;

    /**
     * Sets the onchain nitro price to the given value
     * @param {Lovelace} p The new nitro price in Lovelace
     * @returns The transaction hash of the set transaction
     */
    setNitroPrice(p: Lovelace): Promise<TransactionHash>;

    /**
     * Sets the onchain asset prices to the given values
     * @param {AssetPrices} ap The new asset prices by rarity class in Lovelace
     * @returns The transaction hash of the set transaction
     */
    setAssetPrices(ap: AssetPrices): Promise<TransactionHash>;

    /**
     * Sets the onchain treasury address to the given value
     * @param {Address} ta The new treasury address
     * @returns The transaction hash of the set transaction
     */
    setTreasuryAddress(ta: Address): Promise<TransactionHash>;

    /**
     * Sets the onchain operating address to the given value
     * @param {Address} oa The new operating address
     * @returns The transaction hash of the set transaction
     */
    setOperatingAddress(oa: Address): Promise<TransactionHash>;
  }

  export interface MkBot extends RacersUtils {
    /**
     * Initializes a Contract environment for the given contract parameters and
     * wallet spec. Returns an instance of Bot when initialized.
     */
    mkBot: (
      cp: ContractParams,
      w: WalletSpec,
      racersParams: RacersParams
    ) => Bot;
  }

  export interface Bot extends RacersQueries {
    // constructor(cfg: ContractConfig, racersParams: RacersParams, credentialProvider: CredentialProvider) {}

    /**
     * Query current pending asset requests
     * @returns A map of TxOutReference to AssetRequest[]
     */
    queryAssetRequests(): Promise<{ [key: TxOutReference]: AssetRequest[] }>;

    /**
     * Get current wallet's Lovelace balance
     * @returns The current wallet's Lovelace balance
     */
    getWalletLovelaceBalance(): Promise<Lovelace>;

    /**
     * Get current wallet's Nitro balance
     * @returns The current wallet's Nitro balance
     */
    getWalletNitroBalance(): Promise<Nitro>;

    /**
     * Mints the given Nitro amount to the current wallet
     * @param {Nitro} amount The amount of Nitro to mint
     * @returns The transaction hash of the mint transaction
     */
    mintNitro(amount: Nitro): Promise<TransactionHash>;

    // Due to the fact that redeeming requests can partially fail, retrying should be handled by the caller
    // RequestTxOutputReference needs to be stored with GameAsset so that client
    // can query asset attributes
    /**
     * Attempts to redeem a total of `maxRequests` pending asset using given * `assets`, chaining
     * the transactions and submitting them in chunks of `chunkBy` requests.
     * Note that large chunk sizes may cause failure of submission of the
     * transactions dependent on network congestion etc.
     * @param {[key in Rarity]:  AssetOption} assets A map of Rarity to AssetOption
     * @param {number} maxRequests The maximum number of requests to redeem
     * @param {number} chunkBy The number of trasnactions to chain the submission of together
     * @param {(requestedAsset: AssetOption) => Promise<number>} generateUniquenessNonce A function that generates a uniqueness nonce number that will be concatenated to the asset name and used as the TokenName
     * @returns A list of [RequestTxOutputReference, GameAsset] tuples
     */
    tryRedeemingPendingRequests(
      assets: { [key in Rarity]: AssetOption },
      maxRequests: number,
      chunkBy: number,
      generateUniquenessNonce: (requestedAsset: AssetOption) => Promise<number>
    ): Promise<[TxOutReference, GameAsset][]>;

    /**
     * Mints and locks additional `slots` slot tokens to the given `race`
     * @param {Race} race The raceId and nitroFee that identifies the specific race to resupply
     * @param {number} slots The number of slots to mint and lock
     * @returns The transaction hash of the mint transaction
     */
    resupplySlots(race: Race, slots: number): Promise<void>;

    // make reward payment >> burns all slots
    // must make sure rewards payment succeeded before burning slots

    /**
     * Burns all slots of the given `race` and distributes rewards based on `rewardDistribution`
     * @param {Race} race The raceId and nitroFee that identifies the specific race to close
     * @param {Map<Address, Lovelace>} rewardDistribution
     * @returns The transaction hash of the burn transaction
     */
    closeRace(
      race: Race,
      rewardDistribution: { [key: Address]: Lovelace }
    ): Promise<TransactionHash[]>;
  }

  export interface MkClient extends RacersUtils {
    /**
     * Initializes a Contract environment for the given contract parameters and
     * wallet spec. Returns an instance of Client when initialized.
     */
    mkClient: (cp: ContractParams, w: WalletSpec, rp: RacersParams) => Client;
  }

  export interface Client extends RacersQueries {
    /**
     * Attempst to buy the given amount of Nitro using the current nitro price
     * from the blockchain
     * @param {Nitro} amount The amount of Nitro to buy
     * @returns The transaction hash of the buy transaction
     */
    buyNitro(amount: Nitro): Promise<TransactionHash>;

    /**
     * Requests an asset by rarity, using the current asset prices on the
     * blockchain
     * @param {Rarity} rarity The rarity of the asset to request
     * @returns The transaction hash of the request transaction
     */
    requestAsset(rarity: Rarity): Promise<TransactionHash>;

    /**
     * Registers in the given race by burning the nitro fee and inserting
     * a 'registered' pkh entry at the race registry script
     * @param {Race} race The race to register in identified by raceId and nitroFee
     * @returns The transaction hash of the register transaction
     */
    registerInRace(race: Race): Promise<TransactionHash>;

    /**
     * Confirms asset selection for race participation by providing the token
     * names for the chosen car and driver. Looks for the assets in the current
     * wallet and modifies the users 'registered' entry into an 'assetSelection'
     * @param {Race} race The race to register in identified by raceId and nitroFee
     * @param {TokenName} car The token name of the chosen car
     * @param {TokenName} driver The token name of the chosen driver
     * @returns The transaction hash of the confirm transaction
     */
    joinRace(
      race: Race,
      car: TokenName,
      driver: TokenName
    ): Promise<TransactionHash>;
  }

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
  }

  type WalletSpec = any;

  export type MkWalletSpec = {
    walletFromMnemonic: (
      mnemonic: string,
      accountIndex: number,
      addressIndex: number,
      hasStake: boolean
    ) => Promise<WalletSpec>;
    walletFromPrivateKey: (privateKey: string) => WalletSpec;
    walletFromPrivateKeyAndStakeKey: (
      privateKey: string,
      stakeKey: string
    ) => WalletSpec;
    walletFromPrivateKeyStakeKeyAndDRepKey: (
      privateKey: string,
      stakeKey: string,
      drepKey: string
    ) => WalletSpec;
    browserWallet: {
      connectToNami: () => WalletSpec;
      connectToGeroWallet: () => WalletSpec;
      connectToFlint: () => WalletSpec;
      connectToEternl: () => WalletSpec;
      connectToLodeWallet: () => WalletSpec;
      connectToLace: () => WalletSpec;
      connectToNuFi: () => WalletSpec;
    };
  };

  type ContractParams = any;

  export type ServerConfig = {
    port: number;
    host: string;
    secure: boolean;
    path?: string;
  };

  export type ContractOpts = {
    networkId?: "testnet" | "mainnet";
    logLevel?: "trace" | "debug" | "info" | "warn" | "error";
  };

  export type MkContractParams = {
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

  export type Registered = {
    address: Address;
  };

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

  export type RacersParams = string; // json string
}
