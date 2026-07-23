---
title: Hydrascan UI Redesign
status: final
created: 2026-07-09
updated: 2026-07-09
---

# PRD: Hydrascan UI Redesign

## 0. Document Purpose

This PRD is for the Hydrascan frontend development team — guiding the complete redesign of the interface including UI, UX flow, design system, and i18n. The backend API (`/heads`, `/ticks`) remains unchanged. The PRD is structured by feature groups, with each feature having globally numbered FRs. The Glossary in §3 is the official source of terminology — all other sections use the terms exactly as defined.

UI/UX research has been conducted on: Etherscan, Beaconcha.in, Blockscout, Arkham Intelligence, and Dune Analytics — with patterns selectively applied to the Hydra Head explorer domain.

## 1. Vision

Hydrascan is a specialized blockchain explorer for Hydra — the Layer 2 state channel solution on Cardano. It is the single window to observe the entire lifecycle of Hydra Heads across all networks (Mainnet, Preprod, Preview).

The current interface accomplishes basic tasks but lacks the professionalism compared to other blockchain explorers in the ecosystem. The layout is flat, lacks visual hierarchy, lacks search, has a UX flow requiring too many interactions, and is not responsive — giving the impression that Hydra is not yet a serious L2 solution.

This redesign aims to elevate Hydrascan to the standards of a modern blockchain explorer: a consistent dark-first design system, a smooth UX flow optimized for technical users (operators and developers), and a clear visual hierarchy that helps users find information instantly — conveying that Hydra is a reliable and actively growing L2 infrastructure. Post-redesign, when someone visits explorer.hydra.family, the first impression must be: "this is a serious, easy-to-use, and modern L2 solution."

## 2. Target User

### 2.1 Jobs To Be Done

**Hydra Head Operator:**
- When running a Head on mainnet/testnet, needs to quickly find and view their Head's status to know if it is healthy or requires intervention.
- When a Head is contested, needs to see the contestation deadline and snapshot number to decide on the next action.

**Cardano Developer / DApp Builder:**
- When evaluating Hydra, needs to see ecosystem health (number of Heads, TVL, network distribution) to know if this is an active and investable L2.
- When building a DApp on Hydra, needs to see the detailed Head structure (Members, Commits, Version) to understand how the system operates on-chain.

### 2.2 Non-Users (v1)

- End users of DApps running on Hydra — they do not need to know Head internals.
- Traders / speculators — Hydrascan does not display token prices or trading data.

### 2.3 Key User Journeys

- **UJ-1. Operator checks Head on mainnet.** Tan, a Hydra node operator, opens Hydrascan. The Network is already Mainnet (default). Types the Head ID into the search bar, and the result appears instantly. Clicks it — a slide-in panel opens with status, members, and contestation info. Copies the Head ID using the copy button. Closes the panel, returning to the table. Entire process takes < 10 seconds.

- **UJ-2. Developer evaluates the Hydra ecosystem.** Linh, a Cardano developer evaluating Hydra for a new DApp, opens Hydrascan for the first time. The stat tiles at the top of the page show: 150 Total Heads, 45 Active, 2.5M ₳ TVL. Impressive. Switches to Preprod to see testnet activity. Filters status = "Open" to see active heads. Clicks a head, viewing members and commits. Switches the language to Japanese because a colleague in Tokyo needs to view it. Sends the deep link to the colleague.

## 3. Glossary

- **Head** — A Hydra state channel instance on Cardano L1. Lifecycle: Initializing → Open → Closed → Finalized or Aborted.
- **Head ID** — The unique on-chain Policy ID identifying a Head.
- **Seed TxIn** — The transaction input that initialized the Head, used to trace its origin.
- **Member** — A participant in the Head, identified by an on-chain ID and party verification key.
- **Commit** — The UTxO committed by a Member to the Head during initialization.
- **TVL (Total Value Locked)** — The total ADA locked in Heads with Open, Initializing, or Closed status. Heads that are Finalized or Aborted have TVL = 0.
- **Contestation** — The process of challenging a snapshot of the Head before finalization. Includes a deadline (POSIX timestamp) and the number of contests.
- **Snapshot Number** — The sequence number of the latest snapshot in the Head, reflecting off-chain state updates.
- **Network** — The Cardano network where the Head exists: Mainnet (magic 764824073), Preprod (magic 1), Preview (magic 2).
- **Network Magic** — The technical identification number for each Network.
- **Tick** — The chain sync checkpoint — the latest block observed by the explorer for each Network.
- **Head Version** — The version of the hydra-node protocol that created the Head.
- **Stat Tile** — A UI card displaying a prominent KPI metric (Total Heads, Active Heads, TVL, Latest Block).
- **Slide-in Panel** — A UI panel sliding from the right, displaying Head details while keeping the table visible behind it.
- **Design Token** — A CSS custom property representing a design value (color, spacing, radius, etc.) — the single source of truth for styling.
- **Filter Chip** — A small UI element displaying an active filter with a remove button.

## 4. Features

### 4.1 Global Layout & Navigation

**Description:** Replace the current flat layout with a clear structure inspired by Etherscan/Blockscout: fixed header, stats ribbon, content area, and footer. Navigation is centralized in the header — no sidebar is used since Hydrascan does not have enough sections. The layout constrains max-width to be readable on ultra-wide screens. Realizes UJ-1, UJ-2.

**Functional Requirements:**

#### FR-1: Fixed header bar

A sticky top header containing: Hydrascan logo (left), global search bar (middle), Network switcher + language switcher + Tick info (right).

**Consequences (testable):**
- Header is always visible when scrolling the page
- Logo click → redirects to home page
- Responsive layout: search bar collapses on mobile, shown via an icon toggle

#### FR-2: Stats ribbon

Immediately below the header, displaying 4 Stat Tiles in a horizontal row: Total Heads, Active Heads (status = Open), TVL (₳), and Latest Block (from the Tick data of the current Network).

**Consequences (testable):**
- Stat Tiles update when data polling refreshes
- Stat Tiles change values when the user switches Networks
- Each tile displays: label (muted text), large number (primary text), and unit if applicable

#### FR-3: Content area

A single-column content area below the stats ribbon containing: filter controls + Heads table + pagination. Max-width constrained (e.g., 1280px) and center-aligned.

**Consequences (testable):**
- Content does not stretch full-width on viewports > 1280px
- Responsive content area: padding reduced on mobile

#### FR-4: Footer

A footer containing: links (GitHub repo, Hydra documentation, API docs), copyright, and version info.

**Consequences (testable):**
- Footer is displayed at the bottom of the page, below the table
- Links open in a new tab

### 4.2 Dashboard Stats (KPI Tiles)

**Description:** 4 Stat Tiles replace the current 2 boxes for "Total Heads" and "Total Value Locked". The design is inspired by Etherscan's stats ribbon and Blockscout's homepage stats — each tile is a card with clear visual hierarchy. Data is calculated from filtered heads based on the current Network. Realizes UJ-2.

**Functional Requirements:**

#### FR-5: Total Heads tile

Displays the total number of Heads on the current Network.

**Consequences (testable):**
- Value = count of all heads where networkMagic matches the current Network
- Updates every polling interval

#### FR-6: Active Heads tile

Displays the number of Heads with status = Open on the current Network.

**Consequences (testable):**
- Value = count of heads where status === "Open" && networkMagic matches
- Label: "Active Heads"

#### FR-7: TVL tile

Displays the total TVL in ₳ on the current Network.

**Consequences (testable):**
- Value = sum of totalLovelaceValueLocked for all heads on the current Network, divided by 1,000,000, displayed with the ₳ symbol
- Heads with Finalized or Aborted status are not included in the TVL calculation

#### FR-8: Latest Block tile

Displays the latest block number from the Tick data for the current Network, along with a block hash link and slot number.

**Consequences (testable):**
- Main value = blockNo from TickState matching the current Network
- Sub-info: slot number displayed smaller below the block number
- Block hash link redirects to Cexplorer (retaining the existing chain sync verification function from TickBox)
- If tick data is not yet available: display a skeleton placeholder

### 4.3 Heads Table

**Description:** Redesign the Heads table with standard blockchain explorer patterns: truncated Head ID with copy button, color-coded status badges, filter chips instead of dropdowns, and improved pagination. Default sort: newest block first. Realizes UJ-1, UJ-2.

**Functional Requirements:**

#### FR-9: Table columns

8 columns: Head ID | Version | Status | Slot | Block No | Block Hash | TVL (₳) | Action.

**Consequences (testable):**
- Head ID: truncated format `{first6}...{last4}`, full value on hover tooltip
- Block Hash: truncated similarly to Head ID
- Version: text badge
- Status: color-coded badge (Initializing=blue, Open=green, Closed=amber, Finalized=slate, Aborted=red)
- TVL: right-aligned, formatted number + ₳
- Action: "View" button opens the Slide-in Panel

#### FR-10: Copy to clipboard

A copy icon button next to the Head ID and Block Hash in the table.

**Consequences (testable):**
- Click copy → full value (not truncated) copied to clipboard
- Visual feedback (tooltip "Copied!" or icon change) for 2 seconds

#### FR-11: Filter chips

Replace react-select dropdowns with Filter Chips. Each filter field has a compact dropdown trigger button above the table. Clicking a trigger → dropdown shows all options for that field. User selects an option → dropdown closes, and a filter chip appears next to it. The chip displays "{field}: {value}" + an X button. Enum fields (Status, Version) use a single-select list; text fields (Head ID, Block Hash) use a text input.

**Consequences (testable):**
- Each active filter appears as a chip: "{field}: {value}" + X button
- Click X → removes that filter, updating the table
- "Clear All" button when there is ≥ 1 active filter
- Filter fields: Status (enum, single-select), Version (enum, single-select), Head ID (text, contains match), Block Hash (text, contains match)
- Multiple filters from different fields → AND logic. Same field → only 1 active (selecting again replaces the existing one)

#### FR-12: Improved pagination

Displays "Showing X–Y of Z heads" + page navigation + page size selector.

**Consequences (testable):**
- Total count is always visible
- Page size options: 25, 50, 100
- Previous/Next buttons correctly disabled at start/end
- Current page number highlighted

#### FR-13: Table hover states

Row highlight on hover.

**Consequences (testable):**
- Hover row → background color lighter by 1 elevation level
- Cursor pointer on the row (or only on the action button)

#### FR-14: External links

Head ID links to Cexplorer mint policy page, Block Hash links to Cexplorer block page — retaining the current logic from `CardanoExplorerProvider`. URL format varies by Network: Mainnet uses `cexplorer.io/policy/{id}/mint`, Preprod/Preview uses `{network}.cexplorer.io/policy/{id}?tab=mint`. The provider also exposes `tx()`, `block()`, and `address()` helpers.

**Consequences (testable):**
- Links open in a new tab with `rel="noreferrer"`
- Visual external link indicator (icon or different color than internal links)
- Correct URL format according to the Network (subdomain + path/query differences)

#### FR-14b: Hydra Doom commemorative badge

Retain the commemorative badge (Hydra logo on yellow background) for Head ID `e1393f73096f03a2e127cdace1aad0d3332c158346d0b46efb5a9339` (Hydra Doom Final). The badge displays next to the Head ID in the table row.

**Consequences (testable):**
- Badge appears only for this single hardcoded Head ID
- Hover tooltip: "Hydra Doom Final"
- Badge styled to match the new design system (not hardcoded `bg-yellow-400`)

### 4.4 Head Detail (Slide-in Panel)

**Description:** Replace the current modal overlay with a Slide-in Panel sliding from the right. The panel displays all detailed information of a Head in a key-value grid layout, with tabs for Members and Commits. The table remains visible (dimmed) behind — allowing the user to preserve context. URL updates to support deep linking. Realizes UJ-1, UJ-2.

**Functional Requirements:**

#### FR-15: Slide-in animation

The panel slides from right to left, with a width of ~50% viewport (desktop) or full-width (mobile). A backdrop dims the table behind it.

**Consequences (testable):**
- Smooth animation (300ms ease-out)
- Click backdrop or X button → closes the panel
- Escape key → closes the panel
- No nested modals for Member commits — displayed inline within the panel

#### FR-16: Head detail content

A 2-column key-value grid displaying: Head ID (full, copy button), Head Version, Seed TxIn (Cexplorer link), Status (badge), Contestation Period, Contestations count, Snapshot Number, Contestation Deadline, Block Hash (Cexplorer link), and Slot.

**Consequences (testable):**
- Null/unknown fields display "-" instead of blank
- All hash values use a monospace font
- Copy button for Head ID, Seed TxIn, and Block Hash

#### FR-17: Members tab

The "Members" tab in the panel displays a table: On Chain ID | Party VKey | Total Committed (₳) | Expand.

**Consequences (testable):**
- Each member row is expandable → displaying commits detail inline (TxIn, Address, Value)
- VKey truncated with copy button
- Commit TxIn format is `"hash#index"` — must split on `#` to create the Cexplorer tx link from the hash portion, displaying a `#{index}` suffix
- Commit Address links to Cexplorer via the `address()` helper
- TVL per member calculated client-side: sum of `commits[].value.lovelace` / 1,000,000

#### FR-18: Deep link

Opening the panel → updates URL to `?detail={headId}`. Loading the page with this query param → auto-opens the panel for that Head. The network param `?network={magic}` must also persist in the URL (retaining current behavior).

**Consequences (testable):**
- Shared URL (e.g. `?network=1&detail=abc123`) → recipient sees correct Network + Head detail panel opened by default
- Closing the panel → `detail` param is removed, `network` param is preserved
- Head ID does not exist → panel displays a "Head not found" message
- URL params compatibility: `network`, `page`, `detail`, `search`, filter params — all are composable

### 4.5 Search

**Description:** A global search bar in the header allows users to quickly search for Heads by Head ID, Seed TxIn, or Block Hash. Searches client-side on fetched data (no new API required). Realizes UJ-1.

**Functional Requirements:**

#### FR-19: Search input

Search bar in the header with placeholder "Search by Head ID, Seed TxIn, Block Hash...". Debounced input (300ms).

**Consequences (testable):**
- Input accepts any string
- On mobile: search collapses to an icon, expanding when tapped

#### FR-20: Search results dropdown

When the user types ≥ 3 characters, a dropdown displays matching Heads (max 5 results).

**Consequences (testable):**
- Match against: headId, seedTxIn, point.blockHash (partial match / contains)
- Match priority: exact match → prefix match → contains match. Within the same tier, sort descending by blockNo (newest first)
- Each result shows: Head ID (truncated), Status badge, Network name
- Click result → opens the Slide-in Panel for that Head
- No match → "No heads found"
- Escape or click outside → closes the dropdown

#### FR-21: Search + URL integration

Pressing Enter on search → filters the table by the results. Clearing search → resets the table.

**Consequences (testable):**
- Search term reflected in the URL query param `?search={term}`
- Page load with search param → auto-filters the table

### 4.6 Visual Design System

**Description:** A dark-first design system using CSS custom properties (Design Tokens) via the shadcn/ui oklch palette already present in globals.css. Replaces current hardcoded gray classes with semantic tokens. Applied consistently across the app. Realizes UJ-1, UJ-2.

**Functional Requirements:**

#### FR-22: Color palette tokens

Design Tokens for the entire color system, defined in globals.css `:root`.

**Consequences (testable):**
- 3-level background: `--background` (page) → `--card` (card surface) → `--popover` (elevated)
- Brand accent: Cardano blue (#0033AD) mapped to `--primary`
- Semantic: `--destructive` (red), success (green), warning (amber) — used for status badges
- 3-level text: `--foreground` (primary), `--muted-foreground` (secondary), `--accent-foreground`
- Do not use hardcoded gray classes directly — all via tokens

#### FR-23: Status color system

5 status badges with consistent color mapping across the entire app.

**Consequences (testable):**
- Initializing: blue background + white text
- Open: green background + white text
- Closed: amber/yellow background + dark text
- Finalized: slate/gray background + white text
- Aborted: red background + white text
- Same colors used in the table badge, detail panel, filter chips, and stat calculations

#### FR-24: Typography system

Font tokens for 3 use cases.

**Consequences (testable):**
- UI text: Geist Sans (already installed) — bold headings, regular body
- Blockchain data: monospace font for all hashes, addresses, Head IDs, amounts, Seed TxIn, VKeys
- KPI numbers: Geist Sans, 24-36px, semibold
- Heading scale: h1 (page title), h2 (section), h3 (card title) — consistent sizing

#### FR-25: Spacing and sizing tokens

Consistent spacing system.

**Consequences (testable):**
- Spacing scale: 4, 8, 12, 16, 24, 32px mapped to CSS variables or Tailwind theme
- Border radius: sm (4px), md (8px), lg (12px) — consistent
- Max content width: 1280px
- Card padding: 16-24px consistent

#### FR-26: Component styling rules

Styling rules for all common components.

**Consequences (testable):**
- Buttons: use shadcn Button variants (default/outline/ghost/destructive)
- Cards: background `--card`, subtle border (`border-border`), radius `--radius`
- Tables: sticky header, left-aligned for text, right-aligned for numbers, monospace for hashes
- Links: internal uses `--primary` color, external adds an icon indicator
- Hover states: all interactive elements have hover transition (150ms)
- Focus states: visible ring for keyboard navigation (accessibility)

#### FR-27: Responsive breakpoints

Mobile-first responsive design.

**Consequences (testable):**
- Mobile (< 768px): single column, table horizontal scroll, search collapses, panel full-width
- Tablet (768-1024px): table visible but some columns hidden (Slot, Block Hash hidden)
- Desktop (> 1024px): full layout, panel 50% width

### 4.7 Design Tokens & Style Rules

**Description:** Design tokens and style rules system ensuring any new component added is consistent without re-reading the PRD. This is the "constitution" for visual design — the single source of truth. Realizes all features.

**Functional Requirements:**

#### FR-28: Token architecture

All visual values must be defined via CSS custom properties, no hardcoding.

**Consequences (testable):**
- No hardcoded color values in component files (grep codebase: no `bg-gray-*`, `text-gray-*` found in component code)
- All colors reference Design Tokens: `bg-background`, `text-foreground`, `bg-card`, `border-border`, etc.
- Changing a single token value → updates the entire app consistently

#### FR-29: Elevation model

A 3-layer elevation system via background lightness.

**Consequences (testable):**
- Page background: darkest (`--background`)
- Card/panel surface: 1 step lighter (`--card`)
- Popover/dropdown/tooltip: another step lighter (`--popover`)
- Do not use box-shadows for elevation — use background color differentiation (pattern from Arkham/Blockscout dark mode)

#### FR-30: Border and divider rules

Subtle borders, no heavy lines.

**Consequences (testable):**
- Border color: `--border` (oklch with low opacity, ~10%)
- Dividers between sections: 1px `--border`
- Do not use borders on every table cell — only horizontal dividers between rows
- Card border: 1px `--border` or no border (elevation is sufficient for distinction)

#### FR-31: Animation tokens

Consistent animation system.

**Consequences (testable):**
- Transition duration: 150ms for hover, 300ms for panel slide/modal
- Easing: ease-out for enter, ease-in for exit
- Skeleton loading: pulse animation with `--muted` color
- `prefers-reduced-motion` media query: disables all animations

### 4.8 Internationalization (i18n)

**Description:** Multi-language support for 3 languages: English (default), Vietnamese, and Japanese. Translates all UI labels, tooltips, status text, error messages, and placeholders. Blockchain data (hashes, addresses, amounts, Head IDs) are NOT translated. Realizes UJ-2.

**Functional Requirements:**

#### FR-32: Language switcher

Dropdown in the header (next to the Network switcher) allowing language selection.

**Consequences (testable):**
- 3 options: English, Vietnamese, Japanese
- Selecting a language → entire UI updates immediately (no reload)
- Preference saved to localStorage, persisting across sessions

#### FR-33: Translation coverage

All UI text must reside in translation files.

**Consequences (testable):**
- Labels: "Total Heads", "Active Heads", "Total Value Locked", "Latest Block", column headers, button text, footer links
- Status text: "Initializing", "Open", "Closed", "Finalized", "Aborted"
- Placeholders: search bar, filter controls
- Error messages: "Error fetching data", "Head not found", "No heads found"
- DO NOT translate: Head ID, Block Hash, Seed TxIn, VKey, addresses, amounts, version numbers

#### FR-34: i18n implementation

Compatible with Next.js static export (`output: 'export'`).

**Consequences (testable):**
- Uses client-side i18n library (e.g., `next-intl` client mode, or `react-i18next`)
- Translation files in JSON format per locale: `en.json`, `vi.json`, `ja.json`
- No server-side locale detection — client-side only
- HTML `lang` attribute updates according to the selected language

### 4.9 Polling Controls

**Description:** Retain the existing Pause/Resume function and interval selector from `IntervalSetter` + `IntervalProvider`. Users can pause the auto-refresh and adjust the polling frequency. Redesign the UI to be more compact, integrated into the header or stats ribbon instead of a separate section. Realizes UJ-1.

**Functional Requirements:**

#### FR-38: Pause/Resume toggle

A toggle button to pause/resume auto-polling.

**Consequences (testable):**
- When paused: data does not auto-refresh, button shows "Resume ▶"
- When active: data refreshes based on the interval, button shows "Pause ⏸"
- Visual indicator (e.g. subtle pulse on stat tiles) when auto-update is active

#### FR-39: Interval selector

A dropdown or button group allowing the selection of the polling interval.

**Consequences (testable):**
- Options: 1s, 5s, 10s (retained from the current implementation)
- Default: read from `NEXT_PUBLIC_PULL_INTERVAL` env var, fallback 1000ms
- Changing the interval → applied immediately without reload

### 4.10 Environment Variables & Configuration

**Description:** Document all environment variables necessary for deployment. Retain the current API contract — do not add new env vars except for the default i18n locale.

**Functional Requirements:**

#### FR-40: Environment variable contract

All current env vars must continue to function correctly after the redesign.

**Consequences (testable):**
- `NEXT_PUBLIC_EXPLORER_URL`: base URL for API calls. Fallback: relative paths (`/heads`, `/ticks`)
- `NEXT_PUBLIC_ITEMS_PER_PAGE`: default page size. Fallback: `100`. Must be parsed with `Number()`.
- `NEXT_PUBLIC_PULL_INTERVAL`: polling interval in ms. Fallback: `1000`. Must be parsed with `Number()`.
- `NETWORK_URL`: exposed via the `next.config.mjs` env block — unchanged
- Docker deployment (`docker-compose.yaml`) does not require changes to env config

### 4.11 Skeleton Loading

**Description:** Replace the current "Loading..." text with skeleton placeholders to maintain layout stability while data is being fetched. Pattern from Blockscout — each component has a skeleton shape matching its real dimensions. Realizes UJ-1, UJ-2.

**Functional Requirements:**

#### FR-41: Stat tile skeletons

When data is not yet loaded, each Stat Tile displays a skeleton placeholder.

**Consequences (testable):**
- Skeleton shape: rectangle matching label + number layout
- Pulse animation
- After data loads → skeleton is replaced by real data, no layout shift

#### FR-42: Table skeleton

The table displays skeleton rows when data is being fetched.

**Consequences (testable):**
- 5-10 skeleton rows with column widths matching real data
- Header displays normally (no skeleton)
- After data loads → skeleton rows are replaced by real rows

#### FR-43: Panel skeleton

The Slide-in Panel displays a skeleton when searching for Head data (especially when loaded from a deep link).

**Consequences (testable):**
- Key-value pairs display skeleton bars
- After data is available → real content appears

## 5. Non-Goals (Explicit)

- **Do not add new features outside the redesign scope** — the feature set remains the same (search is a UX improvement, not a new feature).
- **Do not modify the backend API** — frontend-only, using the existing `GET /heads` and `GET /ticks`.
- **Do not add light mode** — dark-first, single theme for MVP [NON-GOAL for MVP, consider for v2].
- **Do not build a mobile app** — responsive web only.
- **Do not add wallet connection** — Hydrascan is a read-only explorer.
- **Do not add real-time WebSockets** — retain the existing polling mechanism.
- **Do not change static export mode** — still `output: 'export'`.
- **Do not add data visualization (charts, sparklines)** — requires backend historical data, out of scope [NON-GOAL for MVP].
- **Do not add keyboard shortcuts** — Ctrl+K search deferred [NON-GOAL for MVP].

## 6. MVP Scope

### 6.1 In Scope

- New global layout & navigation (header, stats ribbon, content area, footer) — FR-1→4
- Dashboard KPI Stat Tiles (4 tiles) — FR-5→8
- Heads table redesign (badges, copy, filter chips, pagination, hover) — FR-9→14
- Head detail Slide-in Panel with deep link — FR-15→18
- Global search (Head ID, Seed TxIn, Block Hash) — FR-19→21
- Visual design system dark-first (color tokens, typography, spacing) — FR-22→27
- Design tokens & style rules (architecture, elevation, borders, animation) — FR-28→31
- Internationalization — EN, VI, JA — FR-32→34
- Polling controls (pause/resume, interval selector) — FR-38→39
- Environment variable contract — FR-40
- Skeleton loading — FR-41→43
- Responsive design (mobile, tablet, desktop) — FR-27

### 6.2 Out of Scope for MVP

- Data visualization (charts, sparklines in Stat Tiles) — requires backend storage of historical data. [NOTE FOR PM: high-value feature, should be v1.1 right after backend support]
- Theme toggle (light/dark) — dark-first only. [NOTE FOR PM: implement once the design system is mature]
- Keyboard shortcuts (Ctrl+K for search) — polish feature, deferred
- Advanced analytics (head lifecycle timeline, contestation history)
- User accounts / saved filters / bookmarks

## 7. Success Metrics

**Primary**

- **SM-1:** Lighthouse Performance score ≥ 90. Validates FR-27, FR-41→43 (responsive + skeleton loading = fast perceived load).
- **SM-2:** Time to First Contentful Paint < 2 seconds on simulated 3G connection. Validates FR-1→4, FR-41→43.
- **SM-3:** Core flow (view table → filter → head detail → copy hash) completed in ≤ 3 clicks. Validates FR-9→11, FR-15→16.

**Secondary**

- **SM-4:** Mobile usable — no unintended horizontal scroll on viewports ≥ 375px (table data horizontal scroll permitted). Validates FR-27.
- **SM-5:** i18n coverage 100% — all UI strings translated for all 3 languages. Validates FR-33.
- **SM-6:** Lighthouse Accessibility score ≥ 90. WCAG 2.1 AA compliance for all interactive elements (focus indicators, aria labels, color contrast ≥ 4.5:1 text, ≥ 3:1 UI components). Validates FR-22→27.

**Counter-metrics (do not optimize)**

- **SM-C1:** Bundle size increase does not exceed 30% of the current build. Counterbalances SM-1, SM-5 — avoiding over-engineering the UI or adding too many dependencies that slow initial load.

## 8. Open Questions

1. Specific monospace font for blockchain data — use system monospace, JetBrains Mono, or Fira Code? Need to evaluate bundle size impact. *[Deferred → UX phase]*
2. Search debounce timing (300ms) — is adjustment needed based on actual dataset size? *[Deferred → implementation, 300ms is a reasonable default]*
3. Default page size — 25, 50, or 100? Requires user feedback. *[Deferred → implementation, default 25]*
4. Contestation deadline display format — relative time ("in 2 hours") or absolute (UTC timestamp)? Or both like Etherscan? *[Deferred → UX phase, recommend both following Etherscan pattern]*
5. ~~Deep link format~~ — **Resolved:** use `?detail={headId}` (query param). Route-based `/heads/{headId}` not feasible with `output: 'export'` as there is no `generateStaticParams` for dynamic Head IDs.

## 9. Assumptions Index

- [ASSUMPTION] §4.1 FR-1: Max content width of 1280px is sufficient for blockchain data tables — might need adjustment if Head ID + Block Hash are extremely long.
- [ASSUMPTION] §4.3 FR-11: Filter chips with compact dropdowns replace react-select — enum fields (Status, Version) use a single-select list, text fields use input. The number of options is small enough for this pattern.
- [ASSUMPTION] §4.4 FR-15: Panel width of 50% viewport is sufficient for head details — might need 60% if the member table is wide.
- [ASSUMPTION] §4.5 FR-20: Client-side search on the entire `/heads` response is fast enough — current dataset is < 1000 heads. If it grows to 10,000+ need to reconsider.
- [ASSUMPTION] §4.8 FR-34: `next-intl` or `react-i18next` compatible with `output: 'export'` — need to verify before selecting library.
- [ASSUMPTION] §4.6 FR-22: Cardano blue (#0033AD) is an appropriate brand color — not yet confirmed with IOG/Cardano brand guidelines.
- [ASSUMPTION] §4.2 FR-7: TVL calculated client-side from `members[].commits[].value.lovelace` — not an API field. Logic is retained from `totalLovelaceValueLocked()` in `utils.ts`.

---

## Adapt-In: Aesthetic and Tone

**Visual direction:** Dark-first, professional blockchain explorer. Inspired by Blockscout's midnight theme + Etherscan's data density + Arkham's modern polish.

**Tone:** Technical yet approachable. Labels are concise and clear. No unnecessary jargon — but no dumbing down for non-technical users. Blockchain terms use the Glossary exactly.

**Anti-references:** Not flashy or marketing-heavy like landing pages. Not gamified. No neon colors. No excessive glassmorphism — subtle only.

**Visual references:** Wireframes and high-fidelity mockups are deliverables of the UX Design phase (`bmad-ux`) — not in scope for the PRD. The PRD only defines design direction and constraints; the UX spec will visualize them.

**Brand identity:** Hydrascan is a product of the Hydra ecosystem (Cardano Scaling). Visuals must convey: stability, reliability, and technical precision. Cardano blue is the anchor color.
