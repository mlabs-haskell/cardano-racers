import './Common.ts'

// Runs in a server nodejs environement, requires bot credentials and exposes
// bot specific functionality

class Bot implements RacersQueries {
  constructor(cfg: ContractConfig, racersParams: RacersParams, credentialProvider: CredentialProvider) {}

  // Bot specific reads
  async queryAssetRequests(): Promise<Map<RequestTxOutputReference, AssetRequest[]>>

  async getWalletLovelaceBalance(): Promise<Lovelace>;
  async getWalletNitroBalance(): Promise<Nitro>;

  // To top up Nitro, needed for redeeming requests
  async mintNitro(amount: Nitro): Promise<TransactionHash>;

  // Due to the fact that redeeming requests can partially fail, retrying should be handled by the caller
  // RequestTxOutputReference needs to be stored with GameAsset so that client
  // can query asset attributes
  async tryRedeemingPendingRequests(
    assets: Map<Rarity, AssetOption>,
    maxRequests: number,
    chunkBy: number,
    generateUniquenessNonce: (requestedAsset: AssetOption) => Promise<number>
  ): Promise<[RequestTxOutputReference, GameAsset][]>;

  async resupplySlots(race: Race, slots: number): Promise<void>

  // make reward payment >> burns all slots
  // must make sure rewards payment succeeded before burning slots
  async closeRace(race: Race, rewardDistribution: Map<Address, Lovelace>): Promise<TransactionHash[]>
}

type ContractConfig = {
  backendParams: QueryBackendParams,
  networkId: NetworkId,
  walletSpec: Maybe WalletSpec
}

type Credential = {
  credential: PrivateKey
  stakingCredential?: PrivateKey
} | SeedPhrase
