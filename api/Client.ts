import {
  ContractParams,
  Nitro,
  Race,
  RacersParams,
  RacersQueries,
  RacersUtils,
  Rarity,
  TokenName,
  TransactionHash,
  WalletSpec,
} from "./Common";

// Runs in the browser, connects to a browser wallet and exposes racers
// specific functionality
export interface MkClient extends RacersUtils {
  /**
   * Initializes a Contract environment for the given contract parameters and
   * wallet spec. Returns an instance of Client when initialized.
   */
  mkClient: (cp: ContractParams, w: WalletSpec, rp: RacersParams) => Client;
}

interface Client extends RacersQueries {
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
