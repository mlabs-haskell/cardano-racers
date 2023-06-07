import "./Common.ts"
import { Address, AssetPrices, Lovelace, RacersQueries, TransactionHash } from "./Common";

// Runs in the browser, connects to a browser wallet and Admin functions,
// such racers instant initialization, onchain state modifications and race
// creation.

interface Admin extends RacersQueries {

  // static async initClient(cfg: ContractConfig, racersParams: RacersParams, walletId: WalletId): Promise<Admin>;

  /*
  * - Mints AdminNFT, BotNFT, StateNFT.
  * - Pays AdminNFT to admin address
  * - Creates reference scripts for NitroPolicy, AssetRequestPolicy, 
  *   DepositScript, DriverAssetPolicy, CarAssetPolicy
  * - Sets initial racers state
  */
  // static async initRacers(cfg: ContractConfig, initialState: InitialState, credentialProvider: Wallet): Promise<RacersParams>;

  // writes (Admin only)



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

  // Admin only
  // async createRace(race: Race, slots: number): Promise<void>

  // close race
}
