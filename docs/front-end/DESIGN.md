---
name: Hydrascan
description: Dark-first blockchain explorer for Cardano Hydra L2 state channels. shadcn/ui on Next.js + Tailwind CSS 4. Specifies brand-layer delta only; shadcn dark defaults inherited wholesale unless overridden below.
status: final
sources:
  - _bmad-output/planning-artifacts/prds/prd-hydra-explorer-2026-07-09/prd.md
created: 2026-07-09
updated: 2026-07-09
colors:
  # Brand overrides on top of shadcn dark defaults.
  # All unlisted tokens (background, foreground, card, popover, muted, muted-foreground,
  # secondary, accent, destructive, border, input, ring) inherit shadcn dark values.
  primary: '#0033AD'
  primary-foreground: '#FFFFFF'
  primary-vivid: '#4C82F0'        # brightened Cardano blue for interactive elements on dark surfaces
  primary-vivid-foreground: '#FFFFFF'
  primary-muted: '#0A1F5C'        # very dark blue tint for subtle hover / active chip backgrounds

  # Status semantic palette — Head lifecycle
  status-open: '#22C55E'
  status-open-bg: '#052E16'
  status-initializing: '#60A5FA'
  status-initializing-bg: '#172554'
  status-closed: '#F59E0B'
  status-closed-bg: '#431407'
  status-finalized: '#94A3B8'
  status-finalized-bg: '#0F172A'
  status-aborted: '#F87171'
  status-aborted-bg: '#450A0A'

  # Data-ink — monospace blockchain data (hashes, addresses, IDs)
  data-ink: '#8DA3C4'             # desaturated blue-gray; reads "data", not "action"

  # Doom badge (commemorative — FR-14b)
  doom-badge-bg: '#92400E'        # warm amber-brown — distinct from status amber
  doom-badge-fg: '#FDE68A'

typography:
  # UI text: Geist Sans inherited from shadcn/Next.js defaults. No override needed.
  data:
    fontFamily: '"Geist Mono", ui-monospace, SFMono-Regular, Menlo, Monaco, monospace'
    fontSize: '0.8125rem'         # 13px — compact enough for dense tables
    lineHeight: '1.5'
  kpi:
    fontFamily: '"Geist Sans", system-ui, sans-serif'
    fontSize: '1.875rem'          # 30px — stat tile number
    fontWeight: '600'
    lineHeight: '1.1'
  kpi-sm:
    fontFamily: '"Geist Sans", system-ui, sans-serif'
    fontSize: '1.25rem'           # 20px — compact stat (mobile or secondary tile)
    fontWeight: '600'
    lineHeight: '1.2'
  label:
    fontSize: '0.75rem'           # 12px — tile label, column header, chip text
    fontWeight: '500'
    letterSpacing: '0.04em'
    textTransform: 'uppercase'

rounded:
  # Tighter than shadcn defaults (0.625rem). Hydrascan reads "precision tool", not consumer app.
  sm: 4px    # tags, inline chips, tooltip
  md: 6px    # buttons, inputs, filter trigger buttons
  lg: 8px    # cards, stat tiles, stat panel, table container
  xl: 10px   # dialogs, sheet panel, search dropdown
  full: 9999px   # status badges (pill shape)

  # Hydra Doom badge uses sm deliberately — it is not a status badge, and the square corner visually separates it from the status system.

spacing:
  # Tailwind v4 base scale inherited. Named tokens for Hydrascan layout:
  content-max-width: '1280px'
  header-height: '56px'
  stats-ribbon-height: '96px'    # desktop; collapses to 2×2 grid on mobile
  panel-width-desktop: '50%'
  panel-width-tablet: '70%'
  table-row-height: '44px'       # min-height per row; matches 44px tap target floor

components:
  button-primary:
    background: '{colors.primary-vivid}'
    foreground: '{colors.primary-vivid-foreground}'
    radius: '{rounded.md}'
    note: 'Hover: 10% lighter. Focus: ring-2 ring-primary-vivid ring-offset-2 ring-offset-background.'

  button-ghost-active:
    background: '{colors.primary-muted}'
    foreground: '{colors.primary-vivid}'
    note: 'Used for pause/resume toggle when active, network switcher selected state.'

  stat-tile:
    background: 'var(--card)'     # shadcn --card token
    border: '1px solid var(--border)'
    radius: '{rounded.lg}'
    padding: '20px 24px'
    label-color: 'var(--muted-foreground)'
    label-size: '{typography.label.fontSize}'
    value-font: '{typography.kpi}'
    value-color: 'var(--foreground)'

  badge-open:
    background: '{colors.status-open-bg}'
    foreground: '{colors.status-open}'
    radius: '{rounded.full}'
    padding: '2px 8px'
    font-size: '0.75rem'
    font-weight: '500'

  badge-initializing:
    background: '{colors.status-initializing-bg}'
    foreground: '{colors.status-initializing}'
    radius: '{rounded.full}'
    padding: '2px 8px'
    font-size: '0.75rem'
    font-weight: '500'

  badge-closed:
    background: '{colors.status-closed-bg}'
    foreground: '{colors.status-closed}'
    radius: '{rounded.full}'
    padding: '2px 8px'
    font-size: '0.75rem'
    font-weight: '500'

  badge-finalized:
    background: '{colors.status-finalized-bg}'
    foreground: '{colors.status-finalized}'
    radius: '{rounded.full}'
    padding: '2px 8px'
    font-size: '0.75rem'
    font-weight: '500'

  badge-aborted:
    background: '{colors.status-aborted-bg}'
    foreground: '{colors.status-aborted}'
    radius: '{rounded.full}'
    padding: '2px 8px'
    font-size: '0.75rem'
    font-weight: '500'

  badge-doom:
    background: '{colors.doom-badge-bg}'
    foreground: '{colors.doom-badge-fg}'
    radius: '{rounded.sm}'
    padding: '2px 6px'
    font-size: '0.6875rem'
    font-weight: '700'
    note: 'Hydra Doom Final commemorative. Not a status badge — different shape (sm not full).'

  hash-display:
    font-family: '{typography.data.fontFamily}'
    font-size: '{typography.data.fontSize}'
    color: '{colors.data-ink}'
    note: 'Used for Head ID (truncated), Block Hash (truncated), Seed TxIn, VKey. Full value on hover tooltip.'

  filter-chip-active:
    background: '{colors.primary-muted}'
    foreground: '{colors.primary-vivid}'
    border: '1px solid {colors.primary-vivid}'
    radius: '{rounded.full}'
    padding: '2px 10px'
    font-size: '{typography.label.fontSize}'
    note: 'The remove (×) icon inherits foreground color. Hover on ×: opacity 0.7.'

  slide-panel:
    background: 'var(--card)'
    border-left: '1px solid var(--border)'
    width-desktop: '{spacing.panel-width-desktop}'
    width-tablet: '{spacing.panel-width-tablet}'
    width-mobile: '100%'
    note: 'Backdrop behind panel: oklch(0 0 0 / 50%). Panel uses box-shadow only on left edge: -4px 0 24px oklch(0 0 0 / 30%).'

  search-result-row:
    background-hover: 'var(--accent)'    # shadcn accent token
    foreground: 'var(--foreground)'
    padding: '8px 12px'
    note: 'Active/keyboard-highlighted row uses {colors.primary-muted} bg + {colors.primary-vivid} left border 2px.'

  skeleton-pulse:
    background: 'var(--muted)'
    radius: '{rounded.sm}'
    note: 'CSS animation: pulse 1.5s ease-in-out infinite. Respects prefers-reduced-motion.'

  copy-button:
    color: 'var(--muted-foreground)'
    color-hover: '{colors.primary-vivid}'
    color-success: '{colors.status-open}'
    note: 'Icon-only button. On click: show check icon for 2s then revert. No text label.'

  table-row:
    background-hover: 'var(--accent)'    # subtle hover; shadcn accent ≈ slightly lighter than card
    transition: 'background-color 150ms ease'
    note: 'No alternating row colors. Hover is the only visual state change on rows.'
---

## Brand & Style

Hydrascan is a precision instrument, not a dashboard. Its job is to surface Hydra Head state immediately — one glance tells an operator whether their Head is healthy; one scan of the stat ribbon tells a developer whether Hydra is an active L2 worth building on.

The visual language follows that premise: maximum information density with minimum chrome. Dark surfaces reduce eye strain during extended monitoring sessions (operators run this all day). Typography does most of the hierarchy work. Color is reserved for meaning — status, brand anchor, data-ink — never for decoration.

Inspirations drawn selectively: Etherscan's stats ribbon density and hash truncation conventions; Blockscout midnight theme's background layering; Arkham Intelligence's restrained use of brand blue as the single chromatic presence; Beaconcha.in's table compactness. Anti-inspiration: anything that feels like a landing page, uses gradients for surface, or treats color as wallpaper.

Hydrascan inherits shadcn/ui dark defaults wholesale. This DESIGN.md specifies only the brand-layer delta — primary color family, status semantic palette, data-ink tone, typography for blockchain data and KPI numbers, and tighter corner radii. The 80% of surface that ships from shadcn is intentionally untouched.

## Colors

The Hydrascan palette has three layers of intention:

**Cardano Blue family (`{colors.primary}` / `{colors.primary-vivid}` / `{colors.primary-muted}`)** is the brand anchor. `{colors.primary}` (`#0033AD`) is the canonical Cardano blue — used in the logo wordmark, brand identity contexts, and large-format accent surfaces where the saturation reads well. On dark surfaces where smaller interactive elements need to meet WCAG 3:1 contrast against `--background`, `{colors.primary-vivid}` (`#4C82F0`) — a brightened Cardano-family blue — is used instead: primary buttons, active link underlines, focus rings, search result highlights. `{colors.primary-muted}` (`#0A1F5C`) is a very dark blue tint for subtle backgrounds: active filter chips, hovered ghost buttons. Never use all three simultaneously in one component.

**Status semantic palette** maps directly to Head lifecycle: green = Open (healthy, funds moving), blue = Initializing (in progress), amber = Closed (attention — contestation window active), slate = Finalized (done, funds released), red = Aborted (terminated). Each status has a paired background token (dark tinted version). The same tokens appear in table badges, detail panel, filter chips, and stat tile sub-labels — never let the same status appear in two different colors anywhere in the app.

**Data-ink (`{colors.data-ink}`)** is a desaturated blue-gray for blockchain data rendered in monospace: hashes, addresses, Head IDs, VKeys. It reads clearly on dark card surfaces without competing with the primary blue. It signals "this is chain data, not a label."

**Achromatic base** (all other tokens) inherits from shadcn dark defaults unchanged: near-black background, slightly lighter card, alpha-transparency borders. No chromatic flourishes in the surface vocabulary.

Avoid: more than three chromatic presences on any one screen. The rule is blue (brand/action) + one status color (relevant to current context) + data-ink. Everything else is achromatic.

## Typography

Geist Sans (already loaded via `next/font/google` in the existing app) handles all UI text — navigation labels, filter triggers, button copy, descriptions, error messages. No override; shadcn's defaults apply.

Geist Mono (falling back to system monospace) is the data font. Every piece of blockchain data — Head IDs, Block Hashes, Seed TxIn values, VKeys, addresses — renders in `{typography.data}` at 13px. The monospace treatment is a trust signal: this is raw chain data, presented exactly as it exists on-chain, not interpreted or formatted editorially. The `{colors.data-ink}` color distinguishes data-ink from UI text without requiring visual containers around every hash value.

KPI numbers (stat tiles) use `{typography.kpi}` (30px semibold Geist Sans). They are the largest text on the page by design — the stat ribbon is the first thing users read. On tablet and mobile, `{typography.kpi-sm}` (20px) applies.

Column headers and chip labels use `{typography.label}` (12px, 500 weight, 0.04em letter-spacing, uppercase). The uppercase treatment signals "meta/structural" and differentiates headers from data rows without color.

Never set blockchain data (hashes, IDs, amounts) in a sans-serif UI font. Never set UI labels in the data monospace font.

## Layout & Spacing

`{spacing.content-max-width}` (1280px) is the content constraint. On viewports wider than 1280px, content centers with `margin: 0 auto`. This prevents tables from stretching 2400px wide on ultra-wides — a readability failure common in naive blockchain explorer implementations.

Three vertical zones from top:
1. **Header** (`{spacing.header-height}`, 56px, sticky) — logo, search, controls
2. **Stats ribbon** (`{spacing.stats-ribbon-height}`, 96px desktop) — 4 stat tiles in a row
3. **Content area** — filter bar + table + pagination, with generous padding

Table row minimum height is `{spacing.table-row-height}` (44px) — this aligns with the WCAG 2.5.5 target size recommendation and makes rows comfortable to click on touch devices.

Internal spacing uses the Tailwind v4 base scale (4px unit). Canonical gaps: 4px (within components), 8px (between related elements), 16px (between components), 24px (between sections), 32px (between major zones).

## Elevation & Depth

Hydrascan uses **background-color differentiation** for depth, not shadows. Three tonal layers:

- **Page** — `--background` (`oklch(0.145 0 0)`, near-black)
- **Card / Panel** — `--card` (`oklch(0.205 0 0)`, slightly lighter)
- **Popover / Dropdown / Tooltip** — shadcn `--popover` (`oklch(0.205 0 0)`, same as card — distinguished by border and backdrop)

The slide-in panel sits at Card elevation but adds a single left-edge box-shadow (`-4px 0 24px oklch(0 0 0 / 30%)`) to physically separate it from the dimmed table behind. This is the only box-shadow in the system.

Search dropdowns and filter dropdowns use the `--popover` token with `border: 1px solid var(--border)`. No additional shadow on popovers — the border is sufficient contrast against the card surface beneath.

Never use elevation-as-chrome (shadow for decoration, shadow to "float" a header). Elevation signals "this is on top of something" — a meaningful spatial statement.

## Shapes

Slightly tighter than shadcn defaults across the board. The aesthetic is "precision data instrument" rather than "consumer app."

- `{rounded.sm}` (4px) — inline elements: tooltip, doom badge, copy-success indicator
- `{rounded.md}` (6px) — buttons, text inputs, filter trigger buttons
- `{rounded.lg}` (8px) — stat tiles, table container border, panel
- `{rounded.xl}` (10px) — dialogs, sheet/panel edges, search dropdown
- `{rounded.full}` (9999px) — **status badges only**. The pill shape signals "this is a categorical label" across the entire app. No other element uses full-round.

Hydra Doom badge uses `{rounded.sm}` deliberately — it is not a status badge, and the square corner visually separates it from the status system.

## Components

Hydrascan uses the following shadcn components unchanged: `Button`, `Card`, `Sheet`, `Input`, `Separator`, `Skeleton`, `Tooltip`, `DropdownMenu`, `Popover`, `Tabs`. Do not override shadcn's internal styling for these.

Brand-layer additions:

**Stat Tile** — Card-surface block (`{components.stat-tile}`). Uppercase label in `{typography.label}` + `--muted-foreground`. Large KPI number in `{typography.kpi}` + `--foreground`. Optional sub-value (slot number, unit) in `{typography.label}` + `--muted-foreground`. When skeleton: two `{components.skeleton-pulse}` bars (label-width and value-width). No icon — the number is the signal.

**Status Badges** — Five variants (`badge-open` through `badge-aborted`). Always pill-shaped (`{rounded.full}`). Never mix: one badge per Head, one color per status. In the detail panel the badge is the same component, not a text label.

**Hash Display** — `{components.hash-display}`. Render as `{first6}…{last4}` in the table; full value in the detail panel. Copy button (`{components.copy-button}`) immediately after. Hover tooltip shows full value. Wrapping in a `<span>` with `user-select: all` allows triple-click to select.

**Filter Chip (active)** — `{components.filter-chip-active}`. Renders as "{Field}: {Value} ×". The × is part of the chip, not a separate button. Clicking anywhere on the chip removes the filter. Chips appear in a row below the filter trigger buttons, above the table.

**Slide-in Panel** — `{components.slide-panel}`. Animates from right with `translateX(100%) → translateX(0)` in 300ms ease-out. Backdrop `oklch(0 0 0 / 50%)` behind panel (click to close). Escape closes. No nested modal inside the panel — member commit detail expands inline.

**Search Result Row** — `{components.search-result-row}`. Each row: truncated Head ID (data-ink monospace) left, status badge right, network name (label style) below ID. Keyboard-active row: primary-muted bg + 2px left border in primary-vivid.

**Copy Button** — `{components.copy-button}`. Icon-only (`ClipboardIcon` from lucide-react, 14×14px). Default state: `--muted-foreground`. Hover: `{colors.primary-vivid}`. After click: `CheckIcon` + `{colors.status-open}` for 2s, then revert. No `sonner` toast for copy — inline icon change is sufficient feedback.

**Table Row** — `{components.table-row}`. Row background transition on hover to `--accent` color.

## Do's and Don'ts

| Do | Don't |
|---|---|
| Use `{colors.primary-vivid}` for interactive elements (buttons, links, focus rings) on dark surfaces | Use `{colors.primary}` (#0033AD) for small interactive text — too dark on dark bg |
| Use `{colors.data-ink}` + `{typography.data}` for all blockchain hashes, IDs, addresses | Render chain data in the UI sans-serif font |
| Use `{rounded.full}` for status badges and status badges only | Use pill shapes for buttons, chips, or version badges |
| Status colors consistently across table badge, detail panel, filter chip, stat sub-label | Use different shades of "green" for Open in different UI contexts |
| Background differentiation for depth (card vs page vs popover) | Box-shadows for decoration; only the slide-in panel left-edge shadow is allowed |
| `{components.skeleton-pulse}` during data load — shapes match real content layout | Show spinner or text "Loading…" instead of skeletons |
| Uppercase labels at 12px for column headers, tile labels, chip text | Uppercase body text or data values |
| `prefers-reduced-motion`: disable all transitions and skeleton pulse animation | Run animations regardless of OS motion preference |
