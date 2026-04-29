# AZ-II.cutover.G - Handwritten BBNF Bootstrap Bridge
**Opens after**: AZ-II.cutover.F close
**Agents**: up to 10 parallel
**Hard gate**: a bounded handwritten BBNF bootstrap parser breaks the regen chicken-and-egg and keeps BBNF self-parity green.
**Status**: complete_with_misses

## Scope

1. Move prior partial reports into AZ-II audit provenance.
2. Author `bootstrap_parser.rs` as a bounded bridge that parses BBNF
   source into `BbnfDocument`.
3. Route public BBNF grammar/pipeline entry points through the bridge.
4. Fix leaf-shape span emission and type-keyword filtering needed by
   the bridge.
5. Verify 56/56 BBNF self-parity and bootstrap reproducibility.
6. Document the remaining transparent-rule emitter mismatch for
   cutover.H.

## File bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/cutover.{C,E,F}-PARTIAL.md` | modify |
| `crates/core/src/grammar/bootstrap_parser.rs` | create |
| `crates/core/src/grammar/mod.rs` | modify |
| `crates/core/src/pipeline/directives.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/**` | modify |
| `crates/core/tests/bbnf_self_parity.rs` | modify |
| `docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: non-BBNF resolver activation, `Parsed<R>` deletion,
`crates/tape/` deletion, close-matrix benchmark claims. Deployment
invariant: bootstrap bridge agents use fully-contained worktrees; the
bridge must be documented as bounded and non-terminal.

## Phase sub-items

### AZ-II.cutover.G.1 Audit Provenance Move

Mechanism: move C/E/F partial reports under `audit/` and preserve links.

Files touched: `docs/tranches/AZ-II/audit/cutover.{C,E,F}-PARTIAL.md`.

Sub-gate: partial reports are no longer active wave files.

### AZ-II.cutover.G.2 Bootstrap Parser Authoring

Mechanism: implement a handwritten BBNF source parser that writes
`BbnfStructBuilder` / `BbnfDocument` directly.

Files touched: `crates/core/src/grammar/bootstrap_parser.rs`.

Sub-gate: bridge parses BBNF fixtures into document form.

### AZ-II.cutover.G.3 Entry-Point Routing

Mechanism: route BBNF grammar and directive pipeline entry points
through the bridge.

Files touched: `crates/core/src/grammar/mod.rs`,
`crates/core/src/pipeline/directives.rs`.

Sub-gate: consumers no longer depend on broken generated BBNF parse for
regen.

### AZ-II.cutover.G.4 Leaf and Type Keyword Fixes

Mechanism: fix leaf-shape span emission and type-keyword filtering
observed by the bridge path.

Files touched: `crates/core/src/backend/rust/emitter/shapes/**`,
`crates/core/src/grammar/bootstrap_parser.rs`.

Sub-gate: focused BBNF fixture parses pass.

### AZ-II.cutover.G.5 Self-Parity and Reproducibility

Mechanism: run BBNF self-parity and bootstrap reproducibility through
the bridge.

Files touched: `crates/core/tests/bbnf_self_parity.rs`.

Sub-gate: 56/56 BBNF self-parity and reproducibility gate pass.

### AZ-II.cutover.G.6 Partial Close Report

Mechanism: document the remaining transparent-rule generated-code
mismatch and route to cutover.H.

Files touched: `docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: bridge is not represented as terminal self-hosting.

## Hard gate

1. `bootstrap_parser.rs` parses BBNF into `BbnfDocument`.
2. BBNF entry points route through the bridge.
3. BBNF self-parity is green.
4. Bootstrap reproducibility remains green.
5. Remaining generated-parser mismatch is documented for cutover.H.

## Verification artefacts

- Commits `863de6a5`, `e52974a6`, `984d7535`, `caf07d96`,
  `9300e9df`.
- `docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.F
- **Blocks**: AZ-II.cutover.H, AZ-II.cutover.I

## Archaeology

cutover.G is intentionally a bootstrap bridge, not a legacy parser
floor. It exists to let regen exercise emitter repairs until the
generated BBNF self-host path is restored.
