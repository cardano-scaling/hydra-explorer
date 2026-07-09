# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Development

All Haskell work happens inside a Nix dev shell. Enter it with:

```sh
nix develop
```

The shell provides `cabal`, `haskell-language-server`, `fourmolu`, `yarn`, `cardano-node`, `hydra-chain-observer`, and `hydra-node`.

**Haskell:**
```sh
cd hydra-explorer
cabal build
cabal test all          # run all tests
```

**Frontend** (from `hydra-explorer/web/`):
```sh
yarn dev                # dev server
yarn build              # production build
yarn lint               # ESLint
```

**Nix artifacts:**
```sh
nix build .#hydra-explorer-web   # Next.js static site
nix build .#docker               # Docker image (load with: ./result | docker load)
```

## Formatting & Linting

```sh
# Inside hydra-explorer/
fourmolu --mode inplace src/ exe/ test/

# Frontend
cd web && yarn lint
```

GHC warnings are treated as errors (`-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints -Wunused-packages`).

## Architecture

### Data Flow

```
cardano-node (preview / preprod / mainnet)
    │  chain sync
    ▼
hydra-chain-observer  (one per network × hydra version)
    │  POST /observations/{network}/{version}   [observer-api.yaml]
    ▼
hydra-explorer
    ├── observer API  (port 8080 default) — receives POSTs
    ├── aggregator goroutine — drains bounded TBQueue (size 10), updates ExplorerState TVar
    └── client API   (port 9090 default) — serves GET /heads, GET /ticks, static files
    ▼
Next.js frontend — polls /heads and /ticks
```

### Backend (`hydra-explorer/src/Hydra/`)

| File | Role |
|---|---|
| `Explorer.hs` | Wires two Warp servers + aggregator via `race_` |
| `Explorer/ObservationApi.hs` | Servant API for observer side; backwards-compat parsing for pre-0.22 `OnChainTx` wire format |
| `Explorer/ExplorerState.hs` | Pure in-memory state; `aggregateObservation` dispatches on `HeadObservation` variants |
| `Explorer/Options.hs` | CLI flags: `--client-port` (9090), `--observer-port` (8080), `--static-path` |
| `Explorer/Env.hs` | Logging config via `Blammo`/`envparse` (e.g. `LOG_LEVEL`) |

**State model:** Entirely in-memory (`TVar ExplorerState`). `HeadState` tracks status (`Initializing/Open/Closed/Finalized/Aborted`), members, contestation info, snapshot number, and chain point. Fields not yet observed use `Observed a = Unknown | Seen a`, which serialises to JSON `null` / value.

### APIs

- `api/observer-api.yaml` — what chain observers POST to
- `api/client-api.yaml` — what the frontend and external clients GET
- `hydra-explorer/api/json-schemas` — symlink to `../../api` (required for tests)

### Test Suite (`hydra-explorer/test/`)

| Module | Type |
|---|---|
| `Hydra.Explorer.ExplorerStateSpec` | Unit tests for state aggregation |
| `Hydra.Explorer.ApiSpec` | Property tests: generated `ExplorerState` → WAI → validate against `client-api.yaml`; golden + roundtrip tests |
| `Hydra.Explorer.IntegrationSpec` | E2E: spawns real `hydra-explorer` + `hydra-chain-observer` + `cardano-node` devnet |

Golden fixtures are in `hydra-explorer/golden/`. The `OnChainTx` golden fixtures (`golden/OnChainTx/`) maintain backwards compatibility with pre-0.22 observer wire format.

### Frontend (`hydra-explorer/web/src/`)

- `providers/HeadsDataProvider/` — polls `/heads`
- `providers/NetworkProvider/` — network selection state
- `components/HeadsDashboard/`, `HeadsTable/`, `HeadDetails/`, `TickBox/` — UI
- `app/model.tsx` — TypeScript types matching `client-api.yaml`

### Deployment

Production at `explorer.hydra.family`. `docker-compose.yaml` runs one `hydra-explorer` + three `cardano-node` instances (preview/preprod/mainnet) + twelve `hydra-chain-observer` instances (four hydra versions × three networks). CI builds and pushes `ghcr.io/cardano-scaling/hydra-explorer:unstable`; CD SSH-deploys via `docker compose pull && docker compose up -d`.
