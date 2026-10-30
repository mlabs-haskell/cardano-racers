# Cardano Racers: Integration Overview

Cardano Racers is composed of the following components:

- **onchain** (`onchain/`): Plutus validators and minting policies. Not
  called directly. Consumed via `offchain`.

- **offchain** (`offchain/`): TypeScript packages that expose all L1
  interactions for the **frontend**, **bot**, and **admin**. Frontend
  and admin are browser bundles. The bot runs in a Node.js environment.
  Primary integration surface.

- **hydra** (`hydra/`): Long-running service run by each Hydra delegate.
  Hosts races on L2, verifies player inputs, and coordinates result
  consensus and reward-distribution signatures. Second integration
  surface (HTTP).

- **frontend** (integrator-built, typed against `api/Client.ts`):
  Player-facing client. Uses `offchain` for L1 operations and calls
  `hydra` for L2 (`/playerInput`).

- **bot** (integrator-built, typed against `api/Bot.ts`): Operator side.
  Uses `offchain` for asset-request fulfillment, race creation/close,
  `startRace`, etc. May also drive Hydra `/hostRace`.

- **admin** (integrator-built, typed against `api/Admin.ts`): One-off
  deployment and governance. Initializes a Racers instance and manages
  on-chain prices and treasury/operating addresses.

Throughout, **L1** refers to the Cardano mainchain and **L2** to a Hydra
Head (an off-chain channel). **Delegates** are Hydra participants who
jointly manage a Hydra Head, and to whom the advancement of L2 state is
delegated. **Nitro** is the in-game currency, priced in ADA and required
to enter races.

---

## 1. `hydra/`: Layer-2 race hosting service

Run **by each Hydra delegate**. Frontends and bots interact with it over
HTTP. Each instance drives its own `hydra-node`, verifies submitted
player inputs by re-running the Unity race simulator in headless mode,
and coordinates result consensus with peer delegates.

A single Hydra Head can host multiple races concurrently: new races are
brought in via incremental commits and released via decommits as they
finish, so the Head does not need to be closed and reopened between races.

### HTTP endpoints

- `POST /hostRace`: Move the L1 race state into the Hydra Head. Called by
  the bot after `startRace`.

- `POST /signCommitTx`: Sign a commit tx as a delegate. Called internally
  by peer delegates. Not relevant for integration.

- `POST /signAnnounceDistrTx`: Co-sign the announce-distribution tx.
  Called internally by peer delegates. Not relevant for integration.

- `POST /playerInput`: Submit one player's race-log CSV. Each delegate
  re-runs the simulator to verify. Called by the frontend via
  `Client.completeRace`, which sends the input to every delegate in the
  group.

- `GET /raceResults/{raceCs}`: Poll finalized results for a race. Called
  by the frontend and by other delegates for consensus.

CORS is enabled. Errors return a structured JSON body.

### Configuration

Each delegate's service is launched with a JSON config
(`hydra/config-a.json` and `hydra/config-b.json` are working two-node
examples). Notable settings: `hydra-node` startup params (keys and peer
HTTP servers), chain query backend (Blockfrost or Ogmios + Kupo), the
HTTP server port, a "head leader" flag, and the player-input submission
window in seconds.

---

## 2. Common workflow

The end-to-end lifecycle of a Racers deployment. Steps that require an
integrator call name it explicitly. The rest are internal or handled by
delegate operators.

1. **Register a Hydra group.** Delegates set up their `hydra-node` keys and
   register the group on L1.
2. **Create Racers params.** Admin runs the bootstrap:
   `MkAdmin.initRacers(cp, w, initialState)`. Returns the `RacersParams`
   JSON that identifies this deployment and is passed to every other role.
3. **Init Racers state.** Handled by `initRacers` above (locks in the given
   `treasuryAddress`, `operatingAddress`, `assetPrices`, `nitroPrice`).
   Subsequent updates are done via the `Admin.set*` methods.
4. **Participants register for a race.** Bot opens the race with
   `Bot.createRace` (and optionally `resupplySlots`). Each player queries
   `Bot.queryRaceSlotUtxos`, picks a `slotTxIn`, then calls
   `Client.registerInRace` followed by `Client.joinRace` to bind their
   chosen car/driver. Nitro fee is burned on registration.
5. **Discover the Hydra group.** The frontend obtains the delegate group's
   list of HTTP servers (from the on-chain Hydra group registry or an
   out-of-band directory). This becomes the `hydraGroupHttpServers`
   argument to `Client.completeRace` in step 8.
6. **Start the race on L1.** Bot calls `Bot.startRace(startRaceParams)` with
   participants, `totalRewardValue`, `rewardWeights`, delegates, and TTL.
   Returns `{ txHash, raceParams }`.
7. **Host the race on Layer-2.** Bot calls `POST /hostRace` on the leader
   delegate with the L1 race state ref and `raceParams`. Delegates sign
   commit transactions via `POST /signCommitTx`.
8. **Players run the simulation and submit input.** Frontend uses the Unity
   race simulator, extracts the race log as CSV, and calls
   `Client.completeRace(race, hydraGroupHttpServers, csvInput)`, which
   sends `POST /playerInput` to every delegate in the group.
9. **Delegates verify.** Each delegate independently re-runs the same Unity
   simulation in headless mode and checks the submitted result.
10. **Verified result stored.** On successful verification, each delegate
    stores the result, exposed via `GET /raceResults/{raceCs}` for peer
    consensus and frontend polling.
11. **Consensus and announce.** When the player-input submission window
    elapses, delegates cross-check results and co-sign the distribution
    announcement transaction via `POST /signAnnounceDistrTx`. The signed tx
    is posted to L1 and announces the agreed-on results.
12. **Distribute rewards on L1.** Once the announcement is on-chain, the bot
    calls `Bot.distributeRewards(raceParams)` to pay participants and
    delegate fees. On timeout, the escrow is refunded.
