# Frontend two-pass design plan

Status: **binding tertiary tranche; implementation waits for parse-that and BBNF
semantic/structural perfection, accepted graph extraction, and sealed packed
siblings**. Baseline screenshots are evidence, not authorization to start early.

## Glass 7.0.0 authority and order

- The target is exactly `@mkbabb/glass-ui` 7.0.0. The playground currently declares
  `^3.0.0`; this is a clean major replacement, never an incremental compatibility
  layer. No 3.x alias, dual component face, fallback token path, or migration shim
  may survive the cut.
- Local reconnaissance measured Glass package version 7.0.0 at commit
  `662948389ffd0b0e7d6d78a53388bf89301dca50`. That checkout contains unrelated
  active owner changes, so it is read-only evidence. Adoption must use a sealed
  tarball from an explicitly re-pinned clean commit and prove the resolved package
  bytes; a workspace/source link is forbidden.
- Sequence is strict: parse-that locks and packed contract close; BBNF Program,
  graph, type, analysis, lowering, runtime, and artifact locks close; frontend DAG
  and source-isomorphic tests close; then Glass 7.0.0 transposition and product UX
  refinement begin. A core semantic or structural regression reopens the earlier
  lock and pauses this tranche.
- Every page assay and every correction pass must be led with the installed
  `frontend-design` skill. Its judgment is applied page-by-page to hierarchy,
  composition, typography, motion, responsiveness, and distinctiveness; shared
  findings are corrected once at their owning token or component.

The existing product already has a distinct voice: warm paper/ink, Instrument
Serif display text, Fira Code data/code, pastel semantic accents, offset card
shadows, and translucent Glass surfaces. The redesign refines that vocabulary; it
does not replace it with a generic dashboard or amplify every surface equally.

## Pass 1 — foundation tokens

### Typography

- Display/editorial: Instrument Serif for route titles, major stage/result labels,
  and the logo narrative; never for dense controls or long technical prose.
- Interface/prose: the packed Glass sans token for navigation, controls, docs body,
  diagnostics, and metadata.
- Code/data: Fira Code for grammar, parsed values, type terms, offsets, timings,
  artifact hashes, and compact badges. Tabular numerals for metrics.
- Four explicit levels: route statement, section title, instrument label, body/data.
  Delete one-off font sizes that do not map to a level.

### Color and status

- Keep warm page/card/ink neutrals from Glass and the BBNF pastel family, but map
  colors to semantic roles rather than file-local taste.
- Stable roles: source/grammar = amber, type/constraint = purple, success/value =
  green, analysis/provenance = blue/cyan, recovery/warning = pink/amber, hard
  failure = red. Every role has foreground, muted field, border, and focus form.
- Light/dark values must pass contrast independently. Color never carries state
  alone; icons/text/shape accompany it.
- Avoid new gray ladders and arbitrary opacity stacks; reuse Glass warm neutral and
  glass material tokens.

### Space, shape, depth

- Base rhythm: 4px micro, 8px control, 16px group, 24/32px section, 48/64px route.
  Component padding and gaps select from that rhythm.
- Use three radius roles: control, panel, hero/editorial. Pills are reserved for
  tags/segmented states, not every button.
- Three depth roles: page, instrument panel, transient overlay. Offset shadows mark
  interactive/editorial emphasis; blur alone does not define hierarchy.
- Borders become quieter than content; active/focus borders use semantic role color.

### Motion and input

- Fast feedback 120–160ms; local layout/state 180–240ms; route/hero 400–700ms only
  where narrative warrants it. Use packed Keyframes/Value easings after transposition.
- Motion communicates continuity, selection, or source-to-result causality. Delete
  decorative movement that competes with editor/result reading.
- `prefers-reduced-motion` removes transforms/parallax/morph while retaining direct
  opacity/state feedback. Keyboard focus is visible on every interactive surface.

### Responsive and density

- Desktop playground is a multi-instrument bench; mobile is a deliberate sequential
  source -> result -> facts flow with a persistent segmented mode switch.
- Docs use a stable reading measure and off-canvas navigation on small screens.
- Landing moves from theatrical composition to linear narrative without hiding
  capability. Touch targets remain at least 44px where practical.

## Pass 2 — composition

### App shell

- One fixed, low-noise navigation strip with preserved local HeaderRibbon behavior.
- Route identity and current artifact/revision state are legible without crowding
  the primary action. Dark mode and attribution remain keyboard reachable.
- Navigation owns no feature state; routes compose feature roots.

### Landing

- Hero states the proposition in one sentence and demonstrates grammar -> typed
  value immediately. The logo morph survives only if baseline/parity proves it
  aids continuity and reduced-motion has a direct replacement.
- Follow with three evidence-led chapters: author grammar, understand the compiler,
  ship the parser. Use one live preview and fewer competing card spectacles.
- Novelty route is visible as an explorable trace, not a marketing badge.

### Documentation

- Warm paper reading surface, restrained translucent chrome, 68–76 character body
  measure, anchored section navigation, and code/result figures as first-class
  blocks.
- Search/sidebar state is explicit; mobile drawer traps focus correctly and restores
  it. Hydrated charts/examples state loading/error without layout jumps.

### Playground

- Top control strip: authoritative example, entry rule, run/status, share/reset,
  formatter. Entry rule comes from compiler metadata.
- Main bench: source grammar/input on the left; result/diagnostics on the right;
  optional lower/side stage inspector for type, facts, lowering, and provenance.
- Values and diagnostics share qualified source highlighting. Selected stage/value
  path remains synchronized rather than opening independent debug dumps.
- Loading, stale artifact, compilation failure, parse failure, recovery, empty
  result, and budget exhaustion each have a designed state.
- Debug and walkthrough are feature-local modes, not permanent visual noise.

### Shared primitives

Use packed Glass components for generic control/surface behavior. Local components
exist only for BBNF product semantics: grammar editor, typed value tree, diagnostic
trace, stage rail, artifact seal, BBNF logo, and behavior-proved HeaderRibbon.

## Visual proof matrix

Capture before and after at 1440x1000, 1024x768, 390x844, and 320x700, light and
dark where meaningful:

- landing: initial, scrolled/morph, reduced motion;
- every docs slug/template: default, search/navigation, long code/table, missing
  slug/content failure, direct deep link, and mobile drawer;
- playground: initial, successful typed value, compile error, parse/recovery error,
  budget exhaustion, stage trace, stale-WASM rejection, every shipped example,
  share/reset/format actions, and narrow/mobile modes;
- redirect/fallback route: deterministic landing transition with no stale title,
  history, focus, or hydration residue;
- keyboard journey, focus restoration, screen-reader names, contrast, overflow,
  console/network, layout shift, bundle/load profile.

The route inventory is re-extracted immediately before the assay. Today it is `/`,
`/playground`, `/docs/:slug(.*)?`, and the catch-all redirect; newly added routes or
docs templates enter the matrix automatically. Each inventory row receives an
individual before/after assay sheet rather than being inferred from a sibling page.

Two fresh screenshot critics and a fresh adjudicator review hierarchy, alignment,
rhythm, density, state legibility, distinctiveness, and responsive transformation.
Corrections occur at the owning token/component, and two complete clean visual
audits are required.
