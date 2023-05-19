import "./Common.ts"

// Runs in the browser, connects to a browser wallet and Admin functions,
// such racers instant initialization, onchain state modifications and race
// creation.

class Admin extends Bot implements RaceQueries {

  async initClient(racersParams: RacersParams, walletId: WalletId): Promise<Client>;

  /*
  * - Mints AdminNFT, BotNFT, RacersNFT.
  * - Pays AdminNFT to admin address
  * - Creates reference scripts for NitroPolicy, AssetRequestPolicy, 
  *   DepositScript, DriverAssetPolicy, CarAssetPolicy
  * - Sets initial racers state
  */
  static async initRacers(cfg: ContractConfig, initialState: InitialState, credentialProvider: CredentialProvider): Promise<RacersParams>;

  // writes (Admin only)
  async setNitroPrice(p: Lovelace): Promise<TransactionHash>;
  async setAssetPrices(ap: AssetPrices): Promise<TransactionHash>;
  async setTreasuryAddress(ta: Address): Promise<TransactionHash>;
  async setOperatingAddress(oa: Address): Promise<TransactionHash>;

  // Admin only
  async createRace(race: Race, slots: number): Promise<void>

}
