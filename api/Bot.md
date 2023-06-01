# `Bot` Class API Documentation

## Overview
The `Bot` class in TypeScript provides a series of methods (in addition to common query methods) to perform various racers specific operations on the Cardano blockchain.

## Constructor

```typescript
constructor(cfg: ContractConfig, racersParams: RacersParams, credentialProvider: CredentialProvider) { }
```

The constructor for `Bot` takes in three parameters:

- `cfg: ContractConfig`: The configuration for the contract including network configuration, query layer and runtime information
- `racersParams: RacersParams`: JSON string of desired racers instance
- `credentialProvider: CredentialProvider`: The provider of the wallet credentials. This is used to control/access the Bot wallet.

## Methods

### `queryAssetRequests()`

```typescript
async queryAssetRequests(): Promise<Map<RequestTxOutputReference, AssetRequest[]>>
```
Queries the blockchain for asset requests. Returns a `Promise` that resolves to a map of `RequestTxOutputReference` to an array of `AssetRequest` objects.

### `getWalletLovelaceBalance()`

```typescript
async getWalletLovelaceBalance(): Promise<Lovelace>;
```
This function is included to allow the user of the bot interface to query for the wallets ADA balance programmatically.
Fetches the current Lovelace balance of the wallet. Returns a `Promise` that resolves to a `Lovelace` object.

### `getWalletNitroBalance()`

```typescript
async getWalletNitroBalance(): Promise<Nitro>;
```
This can be used to query the wallets Nitro balance programmatically. 
Fetches the current Nitro balance of the wallet. Returns a `Promise` that resolves to a `Nitro` object.

### `mintNitro(amount: Nitro)`

```typescript
async mintNitro(amount: Nitro): Promise<TransactionHash>;
```
Mints a specified amount of Nitro. Returns a `Promise` that resolves to the `TransactionHash` of the minting operation.
#### Notes
This should be used in conjunction with `getWalletNitroBalance` to ensure that the Wallets Nitro balance is maintained.
It is important to ensure that the wallets Nitro balance is kept above a threshold so that when calling `tryRedeemingPendingRequests(...)`, redemption failures due to lack of Nitro for payouts is prevented.
The threshold can be set as a very large constant, or it can also be set dynamically based on user throughput and/or number of requests per  `tryRedeemingPendingRequests(...)`

### `tryRedeemingPendingRequests(...)`

This method is used to redeem the current Asset Requests made by users at the deposit script. It should be used in conjunction with `queryAssetRequests` by the bot to ensure that user requests are redeemed in a timely manner.

```typescript
type AssetOption = { 
  name: string, // "Mustang"
  assetType: GameAssetType, // "driver" | "car"
  imageUrl: string,
  description: string,
  nitroAmount: Nitro,
}

async tryRedeemingPendingRequests(
    assets: Map<Rarity, AssetOption>,
    maxRequests: number,
    chunkBy: number, // >= 1
    generateUniquenessNonce: (requestedAsset: AssetOption) => Promise<number>
  ): Promise<[RequestTxOutputReference, GameAsset][]>;
```

Parameters:
  * `assets: Map<Rarity, AssetOption>`: `assets` is a Map of `Rarity` to `AssetOption`. This map contains information about what type of asset to mint when redeeming a specific rarity request. It includes details such as: type of asset (Car/Driver), asset name, imageUrl, description, and nitro payouts.
  * `maxRequests: number`: This parameter specifies the maximum number of requests to redeem in a single call.
  * `chunkBy: number`: This parameter enables the submission of redemption transactions in groups for more efficient processing of user requests.
  * `generateUniquenessNonce: (requestedAsset: AssetOption) => Promise<number>`: This function is used to ensure the uniqueness of token names used for minted NFTs on-chain.

#### Notes
* All results, including partial ones, must be stored every time `tryRedeemingPendingRequests` is executed. This is due to the fact that querying the chain later for retrieval of minted assets and their randomized attributes is impractical.
* `tryRedeemingPendingRequests` may fail due to submission errors or other unforeseen circumstances. In such cases, the function will return a resolved promise containing the results that succeeded. It's the caller's responsibility to ensure that the redemption is retried so that no user requests are left unfulfilled.
* The `chunkBy` value is used to determine the maximum number of transactions created and submitted in a single chain. A higher value means that more transactions are grouped in a submission, reducing the waiting time for confirmation per processed request. However, grouping too many transactions together may lead to less reliable submission, as each chained transaction relies on the successful submission of its predecessor. A failure in one could cause all subsequent transactions to fail. This parameter should be set and adjusted dynamically for optimal reliability and performance.
* The `generateUniquenessNonce` function must return unique results for every asset name across different bot instances, as the token name is represented as a concatenation of the asset name and the uniqueness nonce returned (e.g., "Mustang:1"). Since all minted assets use the same currency symbol, it's crucial to identify specific NFTs by their token names. Therefore, `generateUniquenessNonce` must return a unique value across bot instances for every asset minted.
* If minting Nitro dynamically, it is advisable to set the threshold based on user asset request throughput and Nitro payouts indicated by the `assets: Map<Rarity, AssetOption>` parameter.

### `resupplySlots(race: Race, slots: number)`

```typescript
async resupplySlots(race: Race, slots: number): Promise<void>
```

This method is used to resupply a race with a specified number of slots for a free-roll race. It returns a `Promise` that resolves when the operation is completed.

This function should be used in conjunction with `queryRaceRegistry()` from `Queries` to ensure that free-roll races always have slots available for participants to register. The caller can compute the total slots on-chain by recording the initial slot number and the `slots: number` parameter of every subsequent `resupplySlots` call. Note that `queryRaceRegistry()` only returns a collection of occupied registry entries.

Parameters:
  * `race: Race`: This is the identifier of the race.
  * `slots: number`: This is the number of additional slots to be supplied.


### `closeRace(race: Race, rewardDistribution: Map<Address, Lovelace>)`

```typescript
async closeRace(race: Race, rewardDistribution: Map<Address, Lovelace>): Promise<TransactionHash[]>
```
Closes a race and distributes rewards. Returns a `Promise` that resolves to an array of `TransactionHash` objects representing the closing and reward transactions.

* mention that offchain should handle time between race close and user
    registrations
* check the upper bound limit of number of addresses


