# AZ-IV.W0 Regen Plan — Triumvirate Plan Lane

**Lane**: plan augment / synthesis (read-only on source; writes this artefact)
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen-plan`
**HARD CAP**: 15 min plan
**Research handoff**: `docs/tranches/AZ-IV/audit/REGEN-research.md`
**Halt evidence**: `docs/tranches/AZ-IV/audit/W0-regen-HALT.md`

## §1 Synthesis

The W0.3 halt names two regen regressions (R1 — HRegex int/float typed-leaf collapse; R2 — BBNF self-host parse error at byte 36). The research lane proves they are not independent: both are surface manifestations of one defect — the canonical generated parser inlines structural decorations (mapping `->` punctuator, factor modifiers `?` `*` `+` `?w`) without wrapping them in compounds, and `lower/expression/*.rs` predicate-driven detectors silently skip when the predicate fails to find the expected span text. Three sibling lowering files compose this defect: `wrap.rs::lower_mapped_factor` (arrow detection), `repeat.rs::lower_factor` (modifier recovery on grouped terms), and `alt.rs::lower_concatenation` / `lower_alternation` (iteration markers on the inner `?w` step of the grammar rule). The W3a.4 audit at `docs/benchmarks/archive/AZ-III/W3a-4-regen-path-agnostic.txt` already prescribed routes A (lowering) and B (keyword emitter); commits `ee3e6c28` and `286425d5` landed partial bridges. The full landing of route A across all three files is the unique close path that produces a self-hosting BBNF parser and unblocks `cargo xtask regen --check` 9/9. Per `feedback_typed-materialization-invariant`, every `->` in the grammar must reach the tape emitter; predicate-based detection that loses arrows on canonical-tree shape change is the lossy mechanism the wave must exorcise.

## §2 Routing decision

One redress agent lands all three lowering surfaces in one commit (`fix(lower/expression): land structural detection across wrap+repeat+alt for canonical parser tree`), with mechanical regen output as an immediate follow-on commit (`chore(grammar/generated): regen 9/9 against fixed lowering`). Rationale: the three files compose a single defect class — predicate-based detection silently dropping structural information — and a sequenced fix would leave the regen tree mid-flight (R1 closed, R2 partly open) for at least one commit, breaking the "regen --check is green live" invariant. A single-commit landing also keeps the `bbnf.rs` regen output coherent: the lowering must close on all three surfaces before generated-output is republished, otherwise the new self-host parser cannot round-trip its own grammar source. The hregex emitter (`crates/core/src/backend/rust/emitter/shapes/hregex.rs`) and shape detector (`crates/ir/src/passes/recognizers/shape_dispatch/array.rs`) are out of scope — research confirms both behave correctly given correct IR; the bug is upstream in lowering. Wave-level disjointness is preserved: W0.3 owns the lowering triad exclusively; no other W0 sub-unit touches `crates/core/src/lower/expression/`.

## §3 Exact Wave-Amendment Text

The orchestrator copies the four blocks below verbatim into `docs/tranches/AZ-IV/waves/W0.md`. Each block names the section to be replaced or appended, the exact location, and the literal markdown.

### Append to `docs/tranches/AZ-IV/waves/W0.md` §File Bounds (after the `xtask/src/main.rs` row, before the `Do NOT touch` line)

```markdown
| `crates/core/src/lower/expression/wrap.rs` | modify-carve |
| `crates/core/src/lower/expression/repeat.rs` | modify-carve |
| `crates/core/src/lower/expression/alt.rs` | modify-carve |
```

### Replace `docs/tranches/AZ-IV/waves/W0.md` §Disjointness (the entire body of that section, between `## Disjointness` and the next `##`)

```markdown
No two W0 units share a `modify` path. If doc reconciliation and source repair both need one file, sequence those commits inside the owning unit.

W0.3 owns the lowering triad `crates/core/src/lower/expression/{wrap,repeat,alt}.rs` exclusively; no other W0 sub-unit may write these files. The triad is one unit of repair (canonical-parser-tree divergence in `lower_mapped_factor`, `lower_factor` modifier recovery, and `lower_concatenation`/`lower_alternation` iteration markers) and lands in one commit so the regen output remains coherent across the wave.
```

### Replace the `### AZ-IV.W0.3 Regen Totality` agent unit body in `docs/tranches/AZ-IV/waves/W0.md` (the three bullets between the header and the next `###`)

```markdown
- Mechanism: repair strict regen drift, parser strategy binding, and the canonical-parser-tree divergence in lowering. The canonical generated `BbnfBootstrap` parser inlines mapped_factor's `->` punctuator, factor modifiers (`?`, `*`, `+`, `?w`), and inner concat/alt iteration markers without wrapping them in compounds. The current predicate-based detectors in `lower/expression/{wrap,repeat,alt}.rs` (`span_text().starts_with("->")`, `kind() == BbnfKind::Unit`, span-trim modifier match) silently skip when the predicate misses, dropping IR `Map { fn_id }` wrappers and `Repeat` nodes. Replace predicate detection with structural detection (positional / compound_kind enumeration plus source-gap fallback) across all three files in one commit; panic loudly on unmatched annotations rather than skipping silently. Regen 9/9 grammars as a follow-on mechanical commit once the lowering chain is fixed.
- Files: `xtask/src/regen.rs`, `xtask/src/main.rs`, `crates/ir/src/registry/strategy.rs`, `crates/core/src/lower/expression/wrap.rs`, `crates/core/src/lower/expression/repeat.rs`, `crates/core/src/lower/expression/alt.rs`, `crates/core/src/grammar/generated/**` (mechanical regen output only).
- Sub-gate: `cargo xtask regen --check` passes live for every manifest grammar (9/9), the synthetic grammar binding test proves no new literal parser-name arm, the regen template scrubs stale `Walker-tape compound emission` doc comments from generated `*.rs` files (post-regen `rg -n 'Walker-tape|__dta_walker_inline' crates/core/src/grammar/generated/` returns zero hits), `BbnfBootstrap::parse(grammar/bbnf/bbnf.bbnf)` round-trips cleanly (no `Syntax { offset: 36 }` error), and the IR for `int_lit`/`float_lit` emits the `Map { fn_id, Regex }` wrapper so the hregex emitter dispatches `push_leaf_with_i64`/`push_leaf_with_f64` (not the `push_leaf_with_str` fallback). Hand-coded source-side `__dta_walker_inline::run` references in `crates/core/src/backend/rust/emitter/shapes/**` and `crates/ir/src/passes/recognizers/shape_dispatch/mod.rs` are scrubbed under W2.4 (`AZ-IV.W2.4 DTA DFA Cleanup`), not W0.3 — W0.3 owns generated-template scrubs only.
```

### Append to `docs/tranches/AZ-IV/AZ-IV.md` §Orchestration Rules (after rule 13)

```markdown
14. The W0.3 lowering triad (`crates/core/src/lower/expression/{wrap,repeat,alt}.rs`) is one unit of repair landed in one commit. Predicate-driven structural detection in lowering is the underlying defect class; redress agents replace silent-skip predicates with structural detection plus loud panic on unmatched annotations (per `feedback_typed-materialization-invariant` and §13 No silent fallback). Mechanical regen output lands as a follow-on commit so the `regen --check` invariant holds across the wave.
```

## §4 Redress dispatch packet

The orchestrator pastes the prompt below verbatim into the redress dispatch.

**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen` (already exists per W0.3 halt; reuse it).
**CARGO_TARGET_DIR**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen/target/w0-regen`
**Hard cap**: `HARD CAP: 60 min. At 54 min commit, at 60 halt.` (Redress default 30 min is doubled because three lowering surfaces, IR shape verification, and a mechanical regen-and-rebuild loop must close in one landing; the research and plan lanes already absorbed root-cause work, leaving pure redress.)
**Expected commits**:
1. `fix(lower/expression): land structural detection across wrap+repeat+alt for canonical parser tree (AZ-IV.W0.3)`
2. `chore(grammar/generated): regen 9/9 against fixed lowering (AZ-IV.W0.3)`
3. (optional) `docs(az-iv/audit): land W0.3 redress evidence + replace W0-regen-HALT.md`

**File bounds**:

| File | Access |
|---|---|
| `crates/core/src/lower/expression/wrap.rs` | modify-carve |
| `crates/core/src/lower/expression/repeat.rs` | modify-carve |
| `crates/core/src/lower/expression/alt.rs` | modify-carve |
| `crates/core/src/grammar/generated/**` | modify generated |
| `xtask/src/regen.rs` | modify (only if regen template scrub demands) |
| `docs/tranches/AZ-IV/audit/W0-regen.txt` | modify (post-fix evidence) |
| `docs/tranches/AZ-IV/audit/W0-regen-HALT.md` | delete or replace with redress evidence |
| `docs/tranches/AZ-IV/audit/REGEN-redress.md` | create |

**Hard gate** (no change from W0.md §Hard Gate item 1):
- `cargo xtask regen --check` passes live 9/9; artefact saved at `docs/tranches/AZ-IV/audit/W0-regen.txt`.
- `BbnfBootstrap::parse(grammar/bbnf/bbnf.bbnf)` returns Ok with `pos == input.len()`.
- `dump_ir grammar/bbnf/bbnf.bbnf int_lit --structural` shows `Map { fn_id, Regex(...) }` (NOT bare `Regex(...)`); types-table shows `rule (int_lit) -> I64` (NOT Span).
- `dump_ir grammar/bbnf/bbnf.bbnf grammar --structural` shows `Repeat(Concat(Ref(grammar_item), OptionalWhitespace))` (NOT bare `Ref(grammar_item)`).

**Redress prompt** (≤700 words, self-contained):

> You are AZ-IV.W0.3 redress lane. Worktree `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen`. Set `CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen/target/w0-regen` before any cargo invocation. Hard cap: 60 min; at 54 min commit, at 60 halt. Read `docs/tranches/AZ-IV/audit/REGEN-research.md` and `docs/tranches/AZ-IV/audit/REGEN-plan.md` first; the root cause and exact route are isolated there.
>
> Mandate: land structural detection across the lowering triad `crates/core/src/lower/expression/{wrap,repeat,alt}.rs` in one commit, then regen 9/9 grammars as a follow-on commit. Joint root cause: the canonical generated `BbnfBootstrap` parser inlines mapped_factor's `->` punctuator, factor modifiers, and concat/alt iteration markers without wrapping them in compounds; the current predicate-based detectors silently skip on miss instead of fail loudly. Replace each predicate with structural detection (positional / compound_kind enumeration; source-gap fallback where unavoidable) and panic on unmatched annotations rather than skip. Per `feedback_typed-materialization-invariant`: every `->` in the grammar must reach the tape emitter; inference composes types, never loses them.
>
> Concrete surfaces:
>
> 1. `wrap.rs::lower_mapped_factor` (`:81`): the existing `c.span_text().trim().starts_with("->")` walks children for span-prefix; the canonical parser's `mapped_factor` body has no `->`-prefixed child because the punctuator is consumed via direct byte check `[45u8, 62u8]` and `value_expr`/`type_annotation` surface as bare compounds. Replace with structural detection — enumerate children positionally (`[factor, value_expr?, type_annotation?]`), find by `compound_kind`, and synthesise the `Map { fn_id }` wrapper. Panic if `value_expr` is present but no `Map` is built; that's the typed-materialization invariant.
>
> 2. `repeat.rs::lower_factor`: `recover_modifier` (the `ee3e6c28` source-gap scan) does NOT recover the `*` on `grammar = ( grammar_item ?w ) *`. The IR dump shows the outer Repeat is gone, so either `byte_span()` returns wrong bounds for grouped (Paren) terms, or `has_unit_marker` is unset for the outer factor. Audit `recover_modifier`'s span computation for grouped terms; make recovery bullet-proof, not heuristic. Panic on a factor whose source-text contains `?`, `*`, `+`, or `?w` after the closing paren but the IR has no Repeat/Optional wrapper.
>
> 3. `alt.rs::lower_concatenation` / `lower_alternation`: the inner `?w` modifier on `grammar_item ?w` is also lost. Audit the iteration-pair shape; ensure `OptionalWhitespace` survives concat/alt lowering. Same panic-on-loss discipline.
>
> Verify, at every step:
> - `cargo build -p bbnf` succeeds against the fix.
> - `cargo run -p bbnf-bootstrap --bin dump_ir -- grammar/bbnf/bbnf.bbnf int_lit --structural` shows `Map { fn_id, Regex(...) }` and `rule (int_lit) -> I64`.
> - `cargo run -p bbnf-bootstrap --bin dump_ir -- grammar/bbnf/bbnf.bbnf grammar --structural` shows `Repeat(Concat(...))` (not `Ref(grammar_item)`).
> - `cargo xtask regen` succeeds; second invocation also succeeds (idempotent).
> - `cargo run -p bbnf-bootstrap --bin debug_parse -- grammar/bbnf/bbnf.bbnf` returns Ok with `pos == input.len()`.
> - `cargo xtask regen --check` returns 9/9 byte-identical; tee output to `docs/tranches/AZ-IV/audit/W0-regen.txt`.
> - `cargo nextest run -p bbnf -p bbnf-core --cargo-profile ax-iter` passes (no regression).
>
> Commit discipline: use the local `commit-discipline` skill before each commit. Lowering-triad commit body cites the research artefact and explains why three files land together (one defect class). Regen commit body cites the lowering commit and confirms `regen --check` is green 9/9. Replace `W0-regen-HALT.md` with `REGEN-redress.md` (post-fix evidence: dump_ir output, regen --check output, parse round-trip output) at the same time as the regen commit.
>
> Empty-return rule: if the lowering fix reveals a fourth surface (e.g. `term.rs`, `factor_kinds.rs`, or a `BbnfKind` projection bug), halt and write `audit/W0.3-redress-HALT.md` naming the surface. Triumvirate-of-triumvirate fires if the fourth surface is required.

## §5 Risk note

Two unknowns the research lane flagged that the redress agent must resolve under cap:

1. **`recover_modifier` non-application** (research §4.2): the `ee3e6c28` source-gap scan was written precisely to handle the `*` modifier on grouped terms, yet the IR dump shows it does not fire on `grammar = ( grammar_item ?w ) *`. Two hypotheses: (a) `byte_span()` returns wrong bounds for `Paren`-wrapped terms; (b) `has_unit_marker` is not propagated through the outer factor compound. The redress agent must instrument both paths before deciding; if neither is the root cause, a third lowering surface (`term.rs` or `factor.rs`) emerges and the wave amendment underestimates the carve.

2. **`lower_concatenation` / `lower_alternation` involvement** (research §4.3): the inner `?w` modifier on `grammar_item ?w` lives at concat/alt scope, not factor scope. Research names `alt.rs` as the third surface but does not prove it; the iteration-pair shape may instead route through a different file (`crates/core/src/lower/expression/term.rs` or a sibling). The redress agent confirms by reading the call chain from `lower_factor` → `lower_concatenation` → `lower_alternation` and instrumenting where the `?w` token is lost. If the loss is in a fourth file, the wave amendment is short by one entry; halt and route to plan-of-plan rather than absorb silently.

A third minor unknown: the regen output flip in `crates/core/src/grammar/generated/bbnf.rs` is mechanical once the lowering chain is fixed, but the `bbnf-bootstrap` build cycle that produces the new tree depends on the lowering fix building cleanly first. If the lowering fix introduces a Rust type error (e.g. `lower_mapped_factor`'s return type changes), the regen invocation fails at compile rather than at `--check`. The redress prompt's "verify at every step" loop catches this; the orchestrator may extend the cap to 75 min if the build cycle takes more than 15 min wall.

## Time accounting

- Read research artefact + W3a.4 audit + AZ-II O3a-A1 exemplar: ~5 min
- Read W0.md, AZ-IV.md, ORCHESTRATION.md / SPEC.md / WAVE_SPEC.md / STYLE.md: ~4 min
- Synthesise route + draft amendment text + write artefact: ~5 min
- **Total**: ~14 min (under cap)
