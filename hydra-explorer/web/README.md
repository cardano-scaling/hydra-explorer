# Hydra Explorer Web Interface

A Next.js frontend application designed to monitor and inspect Hydra Heads across various Cardano networks.

## Features

- **Heads Dashboard**: Displays live metrics including Total Heads, Active Heads, Total Value Locked (TVL) in ADA, and Latest Block details.
- **Search & Filter**: Find and explore specific Hydra Heads in a paginated list.
- **Network Switcher**: Switch views between Cardano networks using Network Magic identifiers.
- **Resilient Auto-Polling**: Automatically updates data with exponential backoff and error notifications when connection issues occur.
- **Offline Banner**: Informs the user when network connectivity is lost.

## Getting Started

### Prerequisites

Ensure you have [pnpm](https://pnpm.io/) installed. Alternatively, `nix develop`
in the repository root provides `pnpm` and `node`.

### Development

1. Install dependencies:

   ```bash
   pnpm install
   ```

2. Start the local development server:

   ```bash
   pnpm dev
   ```

3. Open [http://localhost:3000](http://localhost:3000) in your browser.

### Other Commands

- **Build**: `pnpm build` - Build the static site into `out/` (the app uses
  `output: "export"`, so there is no `next start` server; `out/` is served as
  static files by `hydra-explorer`).
- **Lint**: `pnpm lint` - Check codebase for style/lint issues.
- **Format**: `pnpm format` - Run Prettier to format source files.

The production artifact can also be built with Nix from the repository root:

```bash
nix build .#hydra-explorer-web
```

## Configuration

Configure the interface by creating a `.env.local` file (or set environment variables):

| Variable                    | Description                                            |
| --------------------------- | ------------------------------------------------------ |
| `NEXT_PUBLIC_EXPLORER_URL`  | The URL of the Hydra Explorer API backend.             |
| `NEXT_PUBLIC_PULL_INTERVAL` | Default polling interval in milliseconds (default: 5000). |
