import './Common'
import { Address, AssetOption, AssetRequest, GameAsset, Lovelace, Nitro, Race, RacersQueries, Rarity, TransactionHash, TxOutReference } from './Common';

// Runs in a server nodejs environement, requires bot credentials and exposes
// bot specific functionality

interface Bot extends RacersQueries {
  // constructor(cfg: ContractConfig, racersParams: RacersParams, credentialProvider: CredentialProvider) {}

  /**
  * Query current pending asset requests
  * @returns A map of TxOutReference to AssetRequest[]
  */
  queryAssetRequests(): Promise<{[key: TxOutReference]:  AssetRequest[];}>

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
    assets: {[key in Rarity]:  AssetOption},
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
  resupplySlots(race: Race, slots: number): Promise<void>

  // make reward payment >> burns all slots
  // must make sure rewards payment succeeded before burning slots

  /**
  * Burns all slots of the given `race` and distributes rewards based on `rewardDistribution`
  * @param {Race} race The raceId and nitroFee that identifies the specific race to close
  * @param {Map<Address, Lovelace>} rewardDistribution 
  * @returns The transaction hash of the burn transaction
  */
  closeRace(race: Race, rewardDistribution: {[key: Address]: Lovelace}): Promise<TransactionHash[]>
}
