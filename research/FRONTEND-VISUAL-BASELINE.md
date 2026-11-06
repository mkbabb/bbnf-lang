# Playground visual and interaction baseline

Measured 2026-07-18 against the live Vite app at
`http://127.0.0.1:5600/`, using the protected owner-modified
`playground/vite.config.ts`. This is a pre-transposition baseline, not a design
approval.

## Commands and surfaces

```sh
npm run dev --workspace playground -- --host 127.0.0.1
```

Vite 6.4.2 served on port 5600. The browser-reported default viewport was
1280×720 at device-pixel ratio 2; the responsive probe used 390×844. The home
and playground pages emitted no console warnings or errors during the observed
flows.

Observed flows:

1. home route at desktop width;
2. `/playground` after asynchronous editor initialization;
3. desktop grammar/output split;
4. mobile editor state;
5. mobile `Output` switch to the formatted/debug panel;
6. theme-control click and computed theme verification.

## Correct baseline behavior

- The home hero has a clear product sentence, live grammar identity, two primary
  routes, and a restrained grid-backed visual field.
- Desktop playground exposes grammar and formatted output simultaneously; the
  JSON example parses and formats without console faults.
- The mobile `Output` control is uniquely accessible and successfully swaps from
  the editor to the formatted/debug surface.
- The formatted JSON result and `gorgeous (WASM)` provenance are visible on both
  desktop and mobile.
- Route titles and the main Grammar/Formatted labels are accessible to the DOM.

## Measured defects and debts

### Layout and hierarchy

- The floating language/runtime dock overlaps the bottom of the editor at both
  1280×720 and 390×844. It obscures content and has no reserved safe area.
- At 390 pixels, grammar source wraps visually across lines. A line-numbered code
  editor must preserve source-line identity with horizontal scrolling or a
  deliberate compact projection; soft wrapping makes diagnostics and grammar
  positions ambiguous.
- The mobile formatted panel leaves most of the viewport empty while its debug
  controls and runtime/status affordances crowd the top and bottom edges.
- Desktop allocates nearly half the screen to an empty formatted canvas for the
  small example while the grammar pane is vertically dense. The split is not
  content- or task-aware.
- Home copy and secondary navigation use very low contrast against the grid.
  The editorial serif is distinctive, but small labels approach decorative
  legibility rather than tool legibility.

### Interaction and accessibility

- The debugger surface exposes several unnamed buttons in the accessibility
  snapshot, including disabled controls. Icon-only controls need stable names,
  tooltips, focus states, and an explicit disabled explanation.
- Two unnamed alerts are present in the playground DOM even in the ready state.
  Their ownership and announcement behavior must be proved or removed.
- Clicking the uniquely located `Switch to light mode` control changed its
  visible icon but did not change computed state: `html.class` remained `dark`,
  `color-scheme` remained `dark`, and the button accessible label remained
  `Switch to light mode`. The theme control is behaviorally RED on this probe.
- The desktop `Input`, `Parsed AST`, and `Debug` affordances are styled as terse
  text in panel headers; selected state is not obvious enough without reading
  the resulting content.

### Information architecture

- Product navigation, editor mode, output mode, runtime identity, language
  identity, debugger status, and a second BNF brand mark compete in one viewport.
  The repeated brand/dock treatment consumes space without adding task state.
- Raw parsed AST on the home page is extremely long in the accessibility tree.
  A compact structural summary should be the default, with exact raw data
  inspectable on demand.
- The current shell hides most route content during editor startup and then
  hydrates it. Loading ownership needs an explicit stable skeleton so the footer
  does not temporarily appear as the page body.

## Required post-transposition visual gates

1. No fixed/floating control overlaps editor, output, status, or safe-area content
   at 390×844, 768×1024, 1280×720, or 1440×900.
2. Grammar lines preserve line/column semantics; wrapping is off by default and
   any compact view is a separate named projection.
3. Every button has a stable accessible name; selected/pressed/disabled state is
   visible and programmatically correct; no idle unnamed alerts remain.
4. Theme toggling changes computed color scheme and persists across route reloads
   in both directions.
5. Mobile editor/output/debug navigation has one obvious selected mode and one
   obvious route back; headers never squeeze labels into ambiguity.
6. Runtime/language/provenance state has one compact owner rather than a floating
   duplicate brand dock.
7. Empty output space becomes useful stage evidence, diagnostics, or an intentional
   calm canvas; it is not accidental allocation.
8. Home and tool typography share the design tokens but preserve different jobs:
   expressive editorial display versus high-legibility interface/code text.
9. Browser console, Vue warnings, failed requests, and accessibility violations are
   empty for the named flows.
10. Capture desktop/mobile screenshots after each frontend wave and compare them
    against this baseline and the two-pass token/composition plan in
    `restart/skinny/tranches/sk-v25/research/FRONTEND-DESIGN-PLAN.md`.

The baseline supports a focused redesign, not a cosmetic reskin: reserve content
geometry first, give stage/debug state one owner, then apply the visual token
system to a structurally simpler shell.
