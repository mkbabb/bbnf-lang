# Tranche AY — DTA Self-Host Round-Trip

AY closes the intentional-unworkability window AW-I opened at W3 and
did not fully lift at W4. The walker, lifter, and emitter are
complete per AW-I; the DTA-shaped tape correctly encodes rule
identity via W4ζ's `variant_idx` stamping. What remains is the
lowering pipeline's migration from fn-per-rule tape assumptions
(direct-child `rule_kind()` matching) to DTA's structural shape
(semantic children one Seq compound deeper). Five waves, one sub-
phase per wave, predominantly in `crates/core/src/lower/**`. At
close, `cargo test --workspace` returns green and `post-AW.json`
composes as a multi-wave history.

## Architectural thesis

The DTA walker wraps every grammar construct in a compound frame —
the substrate's pre-order property requires it so `cursor::child(0)`
degrades to `idx + 1`. Fn-per-rule consumers were written against a
tape where the rule body's children sat directly on the rule's
compound; under DTA, a Seq wrapper carrying the body's semantic
children often sits between them. The fix is a uniform shift from
`view.children().find(|c| c.rule_kind() == X)` to
`find_descendant_by_kind(view, X)` at every call site whose target
is a nested-rule output. Leaf-immediate consumers (identifier,
literal, regex direct children) keep direct-child scans.

No further bbnf-tape driver changes anticipated. AW-I's walker
fix set (`AltLinear` savepoint, `Repeat` lo..=hi, `ShuntingYard`
reducer, `Ref` resolution, counter slot release, PSI refresh,
pre-order finalise gating, `pending_variant_idx` stamping) is
load-bearing and complete.

## Invariants

1. **Consumer migration, not producer rework**. The DTA tape shape
   is settled; AY migrates consumers. If a wave surfaces a
   producer-side bug (walker/lifter/emitter), that's a carry-over
   W4δ-style issue — fold the fix into the same wave; do not defer.
2. **Descendant walks where semantic; direct walks where leaf**.
   The generic pattern is to prefer `find_descendant_by_kind`; the
   narrow exception is identifier / literal / regex whose compound
   parent can still carry them as direct children.
3. **Workspace green at every wave boundary after AY-W1 lands**.
   AW-I's unworkability window closes at AY-W1 (the first wave that
   migrates the highest-frequency `find_child_by_kind` call sites).
   W2–W5 operate with a green workspace; any wave that regresses
   gates at close-time fixes in-wave.
4. **Typed-AST parity total** (inherited from AW-I).
5. **Bootstrap regen idempotent at close** (inherited from AW-I
   invariant #6). AY-W5 confirms.
6. **No stubs, no workarounds, no shims.** If a migration reveals
   architectural gaps (e.g. a new `find_descendant_by_kind_then_peel`
   helper is needed), land it as substrate; do not ship local shims
   inside each consumer.

## Wave schedule

| Wave | Scope | Agents | Workspace at close |
|------|-------|--------|--------------------|
| W1 | `lower/expression.rs` — grouped term interior + binary_factor flattening | 1 serial | green (workspace compile restored; ≥ 80% pre-AW-I test-pass rate) |
| W2 | binary_factor operator recognition — Alt-compound recursion or lifter wrap | 1 serial | green (operator chains decode correctly; no `empty gap` panics) |
| W3 | `find_child_by_kind` audit + systematic migration across `lower/**`, `graph/**`, `types.rs` | 3 parallel (by subdirectory) | green |
| W4 | `value_expr` lowering — `->` map-expression migration | 1 serial | green |
| W5 | Round-trip verification + fresh regen + workspace-green bench matrix | 1 serial | green; `post-AW.json` composed |

## Phases

### W1 — lower/expression grouped terms

Owner: `crates/core/src/lower/expression.rs`
(`lower_grouped_term`, `dispatch_expression`, `lower_term`).

`lower_grouped_term` walks `node.children()` to find the inner
expression of `(...)`, `[...]`, `{...}`, `@{...}`. Under DTA the
inner expression is wrapped in a Seq compound that sits alongside
the walker-emitted `(` / `)` Literal leaves (walker fix in W4δ made
these leaves visible). Strategies:

- **Primary**: `find_descendant_by_kind` against the expression-layer
  rule kinds (`rhs`, `alternation`, `concatenation`, `binary_factor`,
  `mapped_factor`, `factor`, `term`). Skip Literal leaves whose span
  matches the bracket alphabet.
- **Fallback**: iterate `descendants_preorder(&node)` and stop at
  the first compound whose `rule_kind()` is a body-expression class.

Tests: every `( rhs )`, `[ rhs ]`, `{ rhs }`, `@{ rhs }` shape
across `bbnf.bbnf`, `expressions.bbnf`, `types.bbnf`. Add focused
tests under `crates/core/tests/` that drive a synthetic grammar
through `bbnf::grammar::parse` → `compile_paths_request`.

Plus folded into W1: the `lower_term: unknown leading byte ';'`
failure when lower_term is handed the rule terminator `;` Alt. The
W4ζ fix handled `rule` branch in `absorb_item`; W1 completes the
audit for directive sub-rules (`token_directive`, `debug_directive`,
`recover_directive`, `host_directive`).

Hard gate: `cargo expand -p bbnf-bootstrap --lib` runs without
panic for `BbnfBootstrap`. `cargo check --workspace` exit 0.
Workspace passes ≥ 1078 (with the walker fixes' integration the
number may shift; ≥ 1078 is the AW-I W2 baseline anchor).

### W2 — binary_factor operator recognition

Owner: `crates/core/src/lower/expression.rs::collect_binary_operands`;
IF the lifter route is chosen, also
`crates/ir/src/passes/recognizers/dta/**`.

Today the walker stamps sub-variant `branch_idx` on the AltLinear
compound for `<< | >> | -`, not the `binary_operators` rule id.
`collect_binary_operands` tests `child.rule_kind() == binary_operators`
— fails, falls through to `recover_binary_op`, sees an empty source
gap because the operator span was wrapped in the Alt compound.

Two routes:

- **Consumer**: `collect_binary_operands` recurses one level into
  Alt compounds when scanning for the operator rule kind. Lightweight;
  touches one function.
- **Producer**: lifter wraps the AltLinear in a Ref-to-`binary_operators`
  so the walker stamps the Alt compound as the expected rule entry.
  Structurally deeper; extends the stamping contract.

Pick the consumer route unless it reveals a broader problem; the
producer route risks destabilising the walker's W4ζ fix. Document
the chosen route in the commit message.

Tests: every `<<`, `>>`, `-` chain in bbnf/expressions/types;
value_expr's `add_op`, `mul_op`, `cmp_op`, `&&`, `||` chains.

Hard gate: no `binary_factor could not resolve operator` panic in
any workspace binary. Expression parsing round-trips.

### W3 — find_child_by_kind audit + systematic migration

Three parallel agents, disjoint file bounds:

#### W3.1 — `lower/**`

Owner: `crates/core/src/lower/**`.

Catalog every `find_child_by_kind` / `children().find(|c| c.rule_kind() ==)`
call site. Classify:

- **Descendant target** (semantic nested rule) — migrate to
  `find_descendant_by_kind`.
- **Leaf target** (identifier / literal / regex) — audit for
  correctness under DTA's Seq wrappers; migrate only if the leaf
  is genuinely not a direct child in the DTA shape.

Produce an audit markdown `docs/tranches/AY/audit/find-child-audit.md`
mid-wave; ship migrations as commits referencing the audit rows.

#### W3.2 — `graph/**`

Owner: `crates/core/src/graph/**`.

Same pattern as W3.1 for the dep/metadata analyzers that walk
`rule_kind` for cross-references.

#### W3.3 — `types.rs`

Owner: `crates/core/src/types.rs`.

Type-annotation decoding. Narrower scope; likely few sites.

Hard gate (all three): each sub-phase's call sites audit the `lower
_term`, `lower_call_arg`, `lower_mapped_factor`, `lower_modifier`
entry points. `cargo check --workspace` + `cargo test --workspace`
green through W3 close.

### W4 — value_expr lowering

Owner: `crates/core/src/lower/value_expr.rs`.

Migrate the `->` map-expression lowering. Ensures every `factor ->
value_expr : type_annotation?` tail decodes correctly under DTA.

Tests: every `int_lit = /regex/ -> i64`-style rule across
`expressions.bbnf` and `types.bbnf`. Typed-materialisation
invariant preserved: the payload's F64/U8/Span projection reaches
the tape emitter, and the rule's `->` annotation visible through
the `pin_sweep` + `compute_payload_layouts` pipeline.

Hard gate: grammar self-host bootstrap idempotent. First regen +
second regen (after a cache clear) produce byte-identical
`generated.rs`.

### W5 — Round-trip + bench matrix + close

Owner: orchestrator serial.

1. Clear caches, run bootstrap, capture `generated.rs` line count
   (expected ~20000-22000 with full cyclic fuse settled).
2. Clear caches again, rerun bootstrap, diff against the first —
   expect empty diff.
3. Run `cargo test --workspace --no-fail-fast`. Close any snapshot-
   delta residuals per DELETE / UPDATE / INVESTIGATE.
4. Run the 19-entry parse-bench matrix cold:
   - `json_monolithic` × {data, twitter, citm, canada, data_xl}
   - `css_l4` × {normalize, bootstrap, tailwind}
   - `google_sheets_monolithic` × {parse_simple, parse_nested, parse_stress}
   - `bbnf_monolithic` × {json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar}
5. Compose `docs/benchmarks/post-AW.json` as a multi-wave history
   rooted at AW-I's synthesized reference entry + AY-W1 through
   AY-W5 measurements. Reference point: AW-I's W2 close workspace
   baseline (1078/0/68); compare against post-AU (the last
   bench-measurable tranche close).
6. Write `docs/tranches/AY/FINAL.md`.
7. Verify hard gate 12: dedicated test asserts CSS L4 DTA
   `state_count < 2000` via `bbnf_ir::passes::recognizers::dta::
   summarise`.

Hard gate: workspace green; bootstrap idempotent; `post-AW.json`
covers the 19-entry matrix.

## Critical files

| File | Wave |
|------|------|
| `crates/core/src/lower/expression.rs` (grouped term, binary_factor, dispatch) | W1, W2 |
| `crates/core/src/lower/tape_walk.rs` (helpers; may need `find_descendant_by_kind` companion) | W1, W3 |
| `crates/core/src/lower/**` (audit + migration) | W3.1 |
| `crates/core/src/graph/**` (dep analyzers) | W3.2 |
| `crates/core/src/types.rs` (type annotation decoding) | W3.3 |
| `crates/core/src/lower/value_expr.rs` (`->` maps) | W4 |
| `crates/core/src/grammar/generated.rs` (regen) | W5 |
| `docs/tranches/AY/audit/find-child-audit.md` (new) | W3 |
| `docs/tranches/AY/FINAL.md` (new) | W5 |
| `docs/benchmarks/post-AW.json` (new) | W5 |

## Hard gates summary

### W1

1. `lower_grouped_term` handles `( rhs )`, `[ rhs ]`, `{ rhs }`, `@{ rhs }` under DTA without the `missing inner expression` panic.
2. Directive sub-rules (token/debug/recover/host) don't trip on the rule terminator.
3. `cargo check --workspace` exit 0; `cargo test --workspace` passes ≥ 1078 (+ walker-arms additions), 0 failed.

### W2

4. `collect_binary_operands` recognizes `<<`, `>>`, `-` operators in DTA-shaped tapes.
5. No `binary_factor could not resolve operator` panic.

### W3

6. `find-child-audit.md` covers every `find_child_by_kind` call site; classifications justified.
7. Every migration preserves test pass count (may improve; not decrease).

### W4

8. `value_expr` `->` lowering round-trips for every typed rule in the corpus.
9. Bootstrap idempotent (gen1 == gen2).

### W5

10. `post-AW.json` exists + covers 19-entry matrix.
11. CSS L4 `state_count < 2000` verified numerically.
12. Workspace test 0 failures.
13. `FINAL.md` exists + recaps every hard gate with evidence.

## Cross-tranche debt — inherited from AW-I

| Item | Origin | AY wave |
|------|--------|---------|
| `find_child_by_kind` → `find_descendant_by_kind` migration | AW-I.W4ζ scope-reveal | W1, W3 |
| `binary_factor` operator recognition | AW-I.W4ζ scope-reveal | W2 |
| `value_expr` `->` map lowering | AW-I.W4ζ scope-reveal | W4 |
| CSS L4 state_count < 2000 verification | AW-I gate 12 | W5 |
| Workspace 0-failed | AW-I gate 13 | Every wave (gate) + W5 (close) |
| `serialize_roundtrip::css_simple` | AW-I.W2.5 INVESTIGATE | W5 or follow-up |
| `post-AW-I.json` bench matrix | AW-I.W5 | Subsumed into AW multi-wave history at W5 close |
| `bbnf-analysis` nightly ICE | infrastructure | Ongoing — `cargo clean -p bbnf-analysis` per `docs/instructions/README.md` §Cache clearing |

## Research artefacts

AY does not open a research wave — the design space is tight
(consumer-side migration following one structural pattern). The
AW-I audits `w3-unworkable-surface.md`, `w4-close.md`, and
`w4-scope-reveal.md` supply the diagnostic context that a research
wave would otherwise produce.

## Operational posture

Inherits `docs/instructions/README.md` + `docs/instructions/TRANCHE_SPEC.md`
in full. Specific notes:

- **No bootstrap regen mid-wave.** W5 owns the regen window. Wave
  W1-W4 agents must not run `scripts/bootstrap-bbnf.sh` — the
  committed `generated.rs` stays frozen at `49656fd4`'s 21198-line
  DTA regen until W5.
- **No walker/lifter/emitter changes.** The producer surface is
  stable. Any scope-reveal requiring producer changes escalates to
  orchestrator.
- **Worktree seed mandatory.** `scripts/seed-worktree.sh` symlinks
  `data/` into every worktree; skipping produces environmental
  failures that masquerade as regressions.

Indefatigable. No deferrals. No shims. No stubs. Begin once
AW-I's FINAL-I.md lands on master.
