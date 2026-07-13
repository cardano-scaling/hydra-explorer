---
name: Hydrascan
status: final
sources:
  - _bmad-output/planning-artifacts/prds/prd-hydra-explorer-2026-07-09/prd.md
created: 2026-07-09
updated: 2026-07-09
---

# Hydrascan — Experience Spine

## Foundation

Single-surface responsive web. shadcn/ui on Next.js (static export, `output: 'export'`) with Tailwind CSS 4. `DESIGN.md` is the visual identity reference; this spine is the experience. Dark mode is the only mode — no light theme, no toggle. All data flows from two polling endpoints: `GET /heads` and `GET /ticks`; no WebSocket, no SSR. Three supported locales: English (default), Tiếng Việt, 日本語.

Key constraints that shape every interaction decision:
- **Static export** — no API routes, no server components with data; all interactivity is client-side.
- **Polling, not push** — data refreshes on interval; UI must handle stale-data gracefully.
- **`useSearchParams` requires `Suspense`** — any component reading URL params must be wrapped or use `dynamic(..., {ssr: false})`.
- **URL state is the persistence layer** — no backend for user preferences beyond `localStorage` (locale, polling interval).

→ Composition references: [`mockups/key-dashboard.html`](mockups/key-dashboard.html) · [`mockups/key-panel.html`](mockups/key-panel.html) · [`mockups/key-mobile.html`](mockups/key-mobile.html). Spine wins on conflict.

## Information Architecture

| Surface | Reached from | Purpose |
|---|---|---|
| Dashboard | App open / logo click | Stat ribbon + Heads table; the only page |
| Head Detail Panel | Table "View" button / search result click / `?detail={headId}` URL param | Full Head metadata, Members, Commits — overlays dashboard as slide-in |
| Search Dropdown | Header search bar (≥ 3 chars typed) | Live-filtered results inline; not a separate page |

Hydrascan is intentionally single-page. There is no navigation menu, no separate pages, no sidebar. All state (current network, current filter, current detail, current search term, current page) lives in URL query params so every view is shareable.

URL param vocabulary (all composable):
- `?network={magic}` — active network (764824073 / 1 / 2); persists across sessions via URL
- `?detail={headId}` — open Head Detail Panel for this Head ID
- `?search={term}` — pre-populate search and filter table
- `?page={n}` — current table page
- `?status={value}` — active status filter
- `?version={value}` — active version filter

## Voice and Tone

Microcopy. Brand voice and aesthetic posture live in `DESIGN.md`.

| Do | Don't |
|---|---|
| "No heads found on Mainnet." | "Oops! Nothing here yet. 😕" |
| "Copied." | "Copied to clipboard successfully!" |
| "Head not found." | "We couldn't find a Head with that ID." |
| "Paused." / "Live." | "Auto-refresh disabled." / "Auto-refresh enabled." |
| "Search by Head ID, Seed TxIn, or Block Hash" (placeholder) | "Enter search query here..." |
| Column header: "TVL (₳)" | "Total Value Locked in Ada" |
| Skeleton: no text — shape-only | "Loading..." text during data fetch |
| Error: "Error fetching data. Retry." with retry button | Full error stack or technical message |
| Filter chip: "Status: Open" | "Filter: Status = Open" |
| "Showing 1–25 of 143 heads" | "Page 1 of 6" |

Status text (all localized): "Initializing" · "Open" · "Closed" · "Finalized" · "Aborted". Use these exact strings — they mirror the backend enum values and the Glossary.

## Component Patterns

Behavioral. Visual specs live in `DESIGN.md.Components` and `DESIGN.md.Do's and Don'ts`.

### Header Bar (sticky)

| Slot | Content | Behavior |
|---|---|---|
| Left | Hydrascan logo + wordmark | Click → `router.replace('/')` (reset all params except network) |
| Center | Global search input | See Search component below |
| Right | Polling controls · Network switcher · Language switcher · Tick display |  |

On mobile (`< 768px`): search input collapses to a search icon button. Tap icon → search expands full-width pushing other right-slot elements into a second row or hamburger. Logo remains left.

### Stat Tiles (4 tiles, horizontal row)

Each tile is a `{components.stat-tile}`. All tiles react to network switch — values recompute from the filtered `/heads` dataset for the current `networkMagic`.

| Tile | Value | Source |
|---|---|---|
| Total Heads | Count of all heads for current network | `/heads` filtered by networkMagic |
| Active Heads | Count where `status === "Open"` | Same dataset |
| TVL (₳) | Sum of `totalLovelaceValueLocked()` / 1,000,000 | Client-side; excludes Finalized + Aborted |
| Latest Block | `blockNo` from matching TickState | `/ticks` filtered by network |

Latest Block tile sub-info: slot number (smaller, muted). Block hash: truncated data-ink link → Cexplorer block page (opens new tab).

All 4 tiles show skeleton placeholders until first data arrives. Skeletons match the label+value layout shape. After data loads, transition without layout shift.

### Heads Table

Sort order: **newest block first** (descending `blockNo`). This is fixed; no user-sortable columns in v1. [ASSUMPTION: users want to see most recent activity first, not alphabetical or status-sorted]

Columns (desktop, left→right): `Head ID` · `Version` · `Status` · `Slot` · `Block No` · `Block Hash` · `TVL (₳)` · `Actions`

Column alignment:
- Text columns (Head ID, Version, Status, Block Hash): left-aligned
- Numeric columns (Slot, Block No, TVL): right-aligned
- Actions: right-aligned

Column hiding at breakpoints:
- Tablet (`768–1023px`): hide `Slot`, `Block Hash`
- Mobile (`< 768px`): hide `Slot`, `Block Hash`, `Block No`; `TVL` moves to a sub-row below Head ID

Head ID cell anatomy: `[doom-badge?] [truncated-id] [copy-icon]`
Block Hash cell anatomy: `[truncated-hash] [copy-icon] [external-link-icon]`

All `id` and `hash` values render in data monospace (`{typography.data}`, `{colors.data-ink}`). Copy icon appears on hover (desktop) or always (touch). Tooltips show full value on hover.

Row click (anywhere except action buttons and copy buttons) → opens Head Detail Panel for that row. "View" button in Actions column does the same — the button is a redundant affordance for discoverability.

### Filter Bar (above table)

Filter trigger buttons in a horizontal row: `Status ▾` · `Version ▾`. Each is a ghost button (`{rounded.md}`) that opens a Popover on click.

- **Status Popover**: list of 5 status options with radio selection. Selecting an option closes the popover and emits a filter chip.
- **Version Popover**: list of known versions (loaded from data, deduplicated) with radio selection.

Active filter chips appear in a second row below the trigger buttons. Each chip: `"{Field}: {Value}"` + `×` remove button. "Clear All" appears at the far right when any chip is active.

Filter logic: AND across different fields. Same field → single-select (new selection replaces old chip). Filters compose with search and pagination.

No filter for Head ID or Block Hash in the filter bar — these are covered by the global search (FR-21 search term reflects in `?search` param and filters the table).

### Pagination

`"Showing {start}–{end} of {total} heads"` on the left. Page controls on the right: `← Prev · 1 · 2 · 3 … · N · Next →`. Page size selector: `25 | 50 | 100` (default 25).

[ASSUMPTION: default page size 25 is appropriate for initial load; users on slow connections benefit from fewer rows]

Pagination state persists in `?page={n}` URL param. Applying a filter resets to page 1. Network switch resets to page 1.

Pagination logic: newest-first (descending blockNo). This differs from the current implementation's reversed-index slice — the new implementation sorts the array descending by blockNo before slicing, not reverse-paginating a forward-sorted array.

### Polling Controls

Compact controls placed in the right slot of the header, left of Network switcher.

- **Pause/Resume toggle**: ghost icon button (`PauseIcon` when live, `PlayIcon` when paused). Label text: "Live" / "Paused". When live: a subtle green dot pulses beside the button.
- **Interval selector**: text dropdown showing current interval. Options: "1s · 5s · 10s". Selecting applies immediately. Default from `NEXT_PUBLIC_PULL_INTERVAL` env var.

When paused, the entire table and stat tiles freeze (no polling). A banner or muted notice: "Updates paused." is not needed — the "Paused" label on the button is sufficient.

### Network Switcher

Segmented button group (not a dropdown): `Mainnet · Preprod · Preview`. Active network: `{colors.primary-muted}` bg + `{colors.primary-vivid}` text. Inactive: ghost.

Switching network:
1. Updates `?network={magic}` in URL (via `router.replace`)
2. Re-filters all displayed data (no new fetch needed — `/heads` returns all networks)
3. Resets to page 1
4. Recomputes stat tiles

[ASSUMPTION: all three networks' head data arrives in a single `/heads` response — confirmed by current implementation which filters client-side by networkMagic]

### Language Switcher

Icon button (globe icon) → Popover with three options: "English · Tiếng Việt · 日本語". Selecting: applies immediately (no reload), saves to `localStorage('locale')`, updates `<html lang="...">`. [ASSUMPTION: next-intl client mode or react-i18next with static export; specific library chosen at implementation]

### Tick Display

In header right slot, after language switcher: small display showing `"Block #{blockNo}"` and a dim slot value for the current network. Updates every polling cycle. Clicking opens Cexplorer block link. When skeleton: a short pulse bar.

### Head Detail Panel

Rendered as a shadcn `Sheet` variant (or a custom implementation to match `{components.slide-panel}` spec). Opens from right, 50% viewport width on desktop.

Panel structure:
1. **Panel header**: Full Head ID (data-ink monospace) + copy button. Close button (×) top-right.
2. **Status badge** + Version badge inline below Head ID.
3. **Key-Value grid** (2-column, responsive 1-column on mobile):
   - Seed TxIn → data-ink link to Cexplorer tx (format: `{hash}#{index}` — split on `#`, link from hash part)
   - Contestation Period
   - Contestation Count
   - Snapshot Number
   - Contestation Deadline
   - Block Hash → data-ink link to Cexplorer block
   - Slot
4. **Tabs**: "Members" (default active) · could extend to "Raw JSON" in v2
5. **Members table** (inside Members tab): `On-Chain ID · VKey (truncated + copy) · Committed (₳) · ▾`
   - Each row is expandable (click `▾` or anywhere on row) → inline commits detail below the row
   - Commits sub-table: `TxIn (hash#index, linked) · Address (linked) · Value (₳)`
   - Null/unknown fields → `—` (em dash)

Panel open → URL updates to add `?detail={headId}` (via `router.replace`).
Panel close → URL removes `?detail` param, keeps all other params.
Load with `?detail={headId}` → panel opens automatically; if Head ID not found in dataset → "Head not found." message inside panel.

### Search

Search input in header center slot (desktop). On focus: border highlights with `ring-2 ring-primary-vivid`. Debounced 300ms.

Behavior:
- Typing ≥ 3 chars → dropdown appears below input with up to 5 matching Head results
- Match against: `headId` (exact + prefix + contains), `seedTxIn` (contains), `point.blockHash` (contains)
- Match priority: exact > prefix > contains; within tier, sort descending `blockNo`
- Each result row: truncated Head ID (data monospace) + status badge + network name
- Click result → close dropdown, open Head Detail Panel for that Head
- Press Enter → filter the table by search term, update `?search={term}` URL param, close dropdown
- Press Escape → close dropdown, clear input if empty; if has value: first Escape clears value, second Escape closes
- Click outside → close dropdown

Empty search term → show all heads (no filter active). "No heads found." when no match after 300ms debounce.

## State Patterns

| State | Surface | Treatment |
|---|---|---|
| First load (data in flight) | Stat tiles, table | Skeleton placeholders — tiles: 2 bars per tile; table: 8 skeleton rows matching column widths |
| Data loaded | All | Skeleton → real content, no layout shift |
| Network has no heads | Table | "No heads found on {Network}." centered below headers |
| Search returns no results | Table + dropdown | Dropdown: "No heads found." · Table: "No results for "{term}"." with clear link |
| Head not found (deep link) | Panel | Panel opens, shows "Head not found." with close button |
| Error fetching data | Stat tiles + table | Tiles show `—`. Table shows "Error fetching data." + "Retry" button. No auto-retry — manual only. |
| Polling paused | Global | "Paused" label on button. No other banner. Data freezes at last successful fetch. |
| Deep link load | Panel | Panel opens immediately with skeleton content; resolves when `/heads` fetch completes |
| Copy success | Copy button | Icon swaps to CheckIcon + `{colors.status-open}` for 2s; no toast |
| Filter chip applied | Filter bar + table | Chip appears, table re-filters, page resets to 1. Smooth (no loading state — client-side filter is instant) |

## Interaction Primitives

**Click / tap to act.** No drag-and-drop. No hover-required affordances on touch. Copy buttons always visible on mobile (not hover-reveal).

**Row click opens detail.** Clicking anywhere on a table row (except the copy button zone and action button) opens the Head Detail Panel. The "View" button in the Actions column is a redundant affordance — both paths do the same thing.

**URL as state.** All significant state writes to the URL via `router.replace()` (not `push()`): network, page, search, filter, detail. This means browser Back never creates a trail of filter-state history — it goes to the previous real navigation (e.g., a different site). Deep links work fully.

**Escape key hierarchy:**
1. If search dropdown open → close dropdown (keep search input focused)
2. If search input has value → clear value (keep focus on input)
3. If Head Detail Panel open → close panel
4. Otherwise → no-op

**Focus management:**
- Opening the Detail Panel → first focus to panel close button (×) or Panel heading
- Closing the Detail Panel → return focus to the table row that triggered it
- Opening search dropdown → focus stays on input (arrow keys navigate results)

**Keyboard shortcuts (minimal — not a keyboard-first product):**
- `Escape` — context-sensitive close (see hierarchy above)
- `Tab` / `Shift+Tab` — standard tab order across all interactive elements
- Arrow keys in search dropdown — navigate results; `Enter` selects

No vim-style shortcuts, no global hotkeys, no `⌘K`. [ASSUMPTION: Hydrascan users are not primarily keyboard-power users; simplicity beats completeness here]

**Banned patterns:**
- Infinite scroll — pagination only
- Auto-closing detail panel on network switch (panel stays open; content reloads for new network)
- Toast notifications for copy — inline icon feedback only
- Nested modals — commits expand inline inside the panel
- Full-page reload on any user action

## Accessibility Floor

Behavioral. Visual contrast lives in `DESIGN.md` (brand overrides verified to maintain WCAG 2.1 AA ratios).

Target: **WCAG 2.1 AA** across the full responsive web surface (SM-6 from PRD).

- **Focus rings**: inherit shadcn `ring` token. All interactive elements receive a visible focus ring on keyboard navigation. Custom focus style for data-dense elements (copy button, table row, panel close) uses `ring-2 ring-primary-vivid ring-offset-2 ring-offset-background`.
- **Contrast**: `{colors.primary-vivid}` (#4C82F0) on `--background` verified ≥ 4.5:1. Status badge text on status background tokens verified ≥ 4.5:1. Data-ink on card surface verified ≥ 3:1 (UI component, not text). [ASSUMPTION: implement-phase verify with tooling before ship]
- **Screen reader landmarks**: `<header>`, `<main>`, `<footer>` semantic elements. Stat tiles inside `<section aria-label="Network statistics">`. Table uses `<table>` with `<caption>` (visually hidden). Panel uses `role="dialog" aria-modal="true" aria-label="Head detail: {headId}"`.
- **`aria-live="polite"`** on search results dropdown — announces result count when it updates.
- **`aria-live="polite"`** on polling status — announces "Updates paused" / "Updates resumed" when toggle fires.
- **`aria-label`** on all icon-only buttons: copy button → `"Copy {fieldName}"`, close panel → `"Close panel"`, pause → `"Pause updates"` / `"Resume updates"`, search icon (mobile) → `"Open search"`.
- **Skeleton regions**: `aria-busy="true"` on the table and stat tiles region while loading; `aria-live="polite"` to announce when content arrives.
- **Tab order**: matches reading order. Panel opens → focus moves into panel. Panel closes → focus returns to triggering element.
- **Touch targets**: all interactive elements ≥ 44×44px tap area. Copy button expands touch area via padding without changing visual size.
- **`prefers-reduced-motion`**: disables slide-in animation (panel appears instantly), disables skeleton pulse, disables polling live-dot pulse. All transition durations → 0ms.
- **Locale & `lang` attribute**: `<html lang="{locale}">` updates on language switch. Screen readers announce the page language correctly.

## Responsive & Platform

| Breakpoint | Layout changes |
|---|---|
| `≥ 1024px` (desktop) | Full layout: header single row, stats ribbon 4-tile horizontal, table all columns, panel 50% width |
| `768–1023px` (tablet) | Header single row; stat tiles 2×2 grid; table hides Slot + Block Hash columns; panel 70% width |
| `< 768px` (mobile) | Header: logo left, search-icon right; stats tiles 2×2 grid; table hides Slot + Block Hash + Block No (TVL moves to sub-row); panel full-width; copy buttons always visible (no hover-reveal) |

Table on mobile: horizontal scroll within the content area is allowed for the table, but the page itself never has horizontal overflow. The table wrapper has `overflow-x: auto`. Stat tiles never overflow — they reflow to 2×2.

## Inspiration & Anti-patterns

**Lifted from Etherscan:**
- Stats ribbon pattern (4 KPI tiles in a horizontal strip at the top) — gives users the "health pulse" of the network before they look at individual entities.
- Hash truncation convention: `{first6}…{last4}` for display; full value on hover tooltip and copy. This is the de facto standard; users expect it.
- Pagination with explicit "Showing X–Y of Z" count — users need to know the total dataset size.

**Lifted from Blockscout midnight theme:**
- Background layering without shadows — `--background` (page) lighter to `--card` (content surface) lighter to `--popover` (elevated). No elevation chrome.
- Borderless table rows with row-hover as the only state indicator.
- Status-colored badges as pills — the only fully-rounded elements in the system.

**Lifted from Arkham Intelligence:**
- Single brand-color presence — one chromatic anchor (blue family) against an achromatic surface. Everything else earns its color by meaning (status) or data-type (data-ink).
- Restrained use of the brand color: only for interactive affordances and active states, never for decoration.

**Lifted from Beaconcha.in:**
- Dense table with small monospace data — 44px rows fit more content without feeling cramped.
- Copy-to-clipboard as a first-class affordance on every hash field.

**Rejected patterns:**
- **Sidebar navigation** — Hydrascan is single-surface; a sidebar creates false depth.
- **Gradient surface backgrounds** — signals "landing page," not "data instrument."
- **Toast for copy confirmation** — interrupts data-reading. Inline icon state is sufficient.
- **Auto-open panels on row hover** — creates accidental state; click-only.
- **Nested modals (commits in a separate dialog)** — inline expansion in the panel is simpler and keeps context.
- **Sticky filter bar covering table headers** — both cannot be sticky simultaneously without z-fighting; table headers are sticky, filter bar is not.
- **Infinite scroll** — breaks "share a page-state via URL" and makes it hard to navigate back to a known position.
- **Dark/light toggle** — dark-first only for MVP. Adding a toggle adds surface area with no current user need.

## Key Flows

### Flow 1 — Operator checks Head health (Tân, Hydra node operator, Tuesday morning)

1. Tân opens `explorer.hydra.family` in his browser. Network defaults to Mainnet (from `?network=764824073` saved in previous session URL or `localStorage`).
2. App loads. Four skeleton tiles appear immediately in the stats ribbon. Below, 8 skeleton table rows fill the content area. No "Loading…" text — the shape communicates "data is coming."
3. Within 1s (typical polling interval): real data replaces skeletons. Stats tiles show: **142 Total · 23 Active · 1.87M ₳ TVL · Block #12,837,443**. Table populates with 25 rows, newest block first.
4. Tân types the first 6 chars of his Head ID into the search bar in the header.
5. After 300ms debounce, a dropdown appears with 1 matching result: his Head ID (truncated) + a green "Open" badge + "Mainnet".
6. He clicks the result. The slide-in panel animates from the right. The table dims behind it. Panel shows his Head ID in full (copy button ready), status badge "Open" in green, all key-value fields.
7. He checks Snapshot Number (shows `47`) and confirms no contestation deadline — Head is healthy.
8. He clicks the copy button next to Head ID. The icon swaps to a check mark for 2 seconds. He pastes the ID into his log.
9. He presses Escape. Panel slides out. He's back at the table, which resumed live polling during the entire time the panel was open.
10. Total time: under 15 seconds.

### Flow 2 — Developer evaluates Hydra ecosystem (Linh, Cardano developer, first visit)

1. Linh opens Hydrascan with no prior session. App loads on Mainnet (default).
2. Stat tiles render: she sees **142 Total Heads, 23 Active, 1.87M ₳ TVL**. The numbers are meaningful — Hydra is clearly active.
3. She clicks "Preprod" in the network switcher (segmented buttons in the header). Stat tiles update instantly (client-side filter). Table updates. Fewer heads, lower TVL — the testnet picture.
4. She clicks the "Status ▾" filter trigger. A popover appears with 5 options. She selects "Open". A filter chip appears: "Status: Open ×". The table now shows only Open heads on Preprod.
5. She clicks on a row — panel opens. She reads: 3 members, each with committed ADA amounts. She expands Member 1's row → commits appear inline: TxIn, Address, Value. All hash values are in data monospace; each has a copy button.
6. She clicks the Block Hash link in the key-value grid — Cexplorer opens in a new tab.
7. She wants to share this Head with a colleague. She copies the current URL from her browser. URL is: `?network=1&status=Open&detail=abc123ef...`. Her colleague gets the exact view.
8. She clicks the language switcher (globe icon) → selects "日本語". All UI labels switch to Japanese immediately. The Head data (hashes, amounts) stays in original format.
9. She pastes the URL into a message to her Tokyo colleague. Done.
