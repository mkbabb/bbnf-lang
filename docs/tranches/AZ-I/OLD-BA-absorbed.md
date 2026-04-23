# Tranche BA — Grammar-Derived Tape Activation

BA closes the direct-to-struct projection work end-to-end across every
production grammar. The thesis is no longer *arrange substrate for a
later payoff*; the thesis is *every grammar rule that says `-> T`
reaches the tape emitter in this tranche, under a runtime gate, with a
measured throughput delta at the same commit that lands the
substrate*. BA is the tranche in which the `project_types` IR pass
populates a real `StructRegistry`, scalar payload layouts stop
returning empty maps, the backward container pointer lands with a
consumer that reads it, and the derive cache lifts out of the source
tree into `$XDG_CACHE_HOME/bbnf-derive/` so that payload work is not
blocked on build-infra drag.

## Architectural thesis

1. **Direct-to-struct is grammar-derived, not user-declared.** A
   payload layout is the projection of a typed `->` return through IR
   type inference. The grammar tells the emitter the shape; the
   emitter does not re-derive it, and there is no hand-written
   `bbnf::json::Value` or `bbnf::css::StyleSheet`. The earlier
   hand-coded experiments (AX.W1.A / AX.W1.B landed and reverted
   −6,128 LOC) are closed territory; BA does not revisit them.
2. **The tape record is the simdjson tape record, shaped for
   bbnf.** 16-byte fixed records, opaque-payload strings on a
   scratch tape, paired open/close records for compounds. BA adds
   the *backward container pointer* so that a close record knows its
   opener in O(1), closing the one-way `child_off` asymmetry that
   has kept pointer-path work out of BB.
3. **CSS typing shape is lightningcss's shape, derived from the
   grammar instead of Rust structs.** Every CSS L4 property rule
   with a typed `->` produces a typed view accessor. lightningcss
   typed-value parity is measured node-for-node, not approximated.
   `feedback_beat-lightningcss-target` is the floor: parity first,
   then exceedance.
4. **Payload activation is a single decision surface.** There is
   one `compute_payload_layouts` pass, one `StructRegistry`, one
   emitter consumer per layout kind. The EmissionTier lattice was
   deleted for a reason (AM.1, `7608530`, ~2000 LOC dead code): it
   added a second decision axis orthogonal to payload truth.
   `feedback_no-orthogonal-codepaths` is enforced.
5. **Measurement gates substrate.** Every wave ships a runtime call
   site at the same commit as its substrate. No wave lands a table
   that is read "next wave". No wave closes ledger-only. AX
   invariant 13 is in force: a ledger-only wave is a re-plan
   trigger.

## Invariants

1. Every `->` in every grammar reaches `push_leaf_with_*`,
   `begin_compound`, or `end_compound`. An IR audit pass enforces
   100% coverage and fails the build otherwise.
2. `StructRegistry` is non-empty for every grammar with a Named
   rule. Emptiness is a hard error, not an acceptable default.
3. One payload decision surface. No re-derivation of payload layout
   in the emitter. No tier axis riding on top of payload truth.
4. No grammar-name fast paths. The shape emitter demonstrated once
   on JSON at AW-V.W3 and was lost precisely because the thesis
   required *auto-derive the sonic-rs-class inner loop from any
   BBNF grammar* — BA ships that for every grammar or ships none.
5. Substrate and consumer land in the same commit. A wave that
   lands a table without a runtime call site does not close.

## Operational posture

1. Every wave opens with a truthful measurement surface. The 17-entry
   AU-baseline matrix runs on every wave boundary with a recorded
   delta. Regressions below the declared floor trigger revert.
2. BA does not claim exceedance until every AU-baseline gate is met.
   Recovery to parity is the first gate; exceedance is the second.
3. Every substrate addition ships with a same-commit consumer and a
   same-commit bench delta. `feedback_execute-planned-architecture`
   is doctrine: do not retreat from a planned substrate, but do not
   ship a substrate without a consumer either.
4. The derive cache lift is BA's infrastructure prerequisite. It
   was deferred out of B1 and lands here because payload work
   thrashes the cache hardest. `feedback_build-infra-first` applies:
   cache lift is W0, not deferred to a later wave.
5. Canonical parity harnesses (`tests/*_parity.rs` against sonic-rs,
   lightningcss, simdjson OnDemand, serde_json, cssparser) are run
   on every wave boundary. Parity regressions block wave close.
6. Samply profiles land under `docs/benchmarks/profiles/BA/<wave>/`
   before and after each wave's substrate change. No optimisation
   claim without a profile delta — `feedback_actual-profiling`.

## Hard gates (AU-baseline anchored)

BA's waves are gated on the 17-entry AU-baseline matrix from
`AU/FINAL.md` (`5281ec23`, 2026-04-15). Current Era VI state has
twitter at 688 MB/s — 35% of AU-baseline 1967. The gap 688 → 1967
is the first-order recovery BA owes. Exceedance is a second-order
gate layered over recovery.

**Parity-recovery gates (W0 through W3):**

| Grammar / fixture | AU-baseline | BA floor | BA target |
|---|---:|---:|---:|
| JSON canada | 1231 MB/s | 1231 | 1500 |
| JSON citm | 2438 MB/s | 2438 | 2700 |
| JSON twitter | 1967 MB/s | 1967 | 2200 |
| CSS normalize | 735 MB/s | 735 | 850 |
| CSS bootstrap | 454 MB/s | 600 | 700 |
| CSS tailwind | 496 MB/s | 500 | 600 |
| Sheets parse_simple | 95 MB/s | 95 | 110 |

**Workspace gates:**

- Pass count ≥ 967
- Fail count ≤ 33
- Ignored count ≤ 30

**Coverage gates (structural, not benchmarked):**

- `grep -c 'push_leaf_with_' crates/core/**/generated.rs` ≥
  count of scalar-payload `->` in all grammars.
- `StructRegistry` non-empty for JSON (pair, value),
  CSS L4 (declaration, dimension, colour), Sheets (cell,
  formula), BBNF (rule, alt_branch).
- lightningcss typed-value parity: every `<length>` rule in CSS L4
  returns a typed Length equivalent to lightningcss's.
- simdjson-shape tape: close records carry `parent_off` (backward
  container pointer) and at least one consumer uses it.

## Reversal criteria

BA inherits AW-IV's lesson directly. AW-IV declared "every entry
exceeds post-AU" and closed with "0 entries exceed post-AU, 17/17
regressed" — 92 commits of substrate without gate alignment. BA
reversal rules:

1. **Wave-local 20% rule.** A wave that misses its own declared
   gate by more than 20% reverts its own substrate at wave close.
   The plan does not accumulate unreverted debt across waves.
2. **Parity-recovery precedence.** Any wave that regresses an
   already-passing AU-baseline entry reverts the responsible
   substrate immediately, regardless of its own gate.
3. **No hedging forward.** A wave does not route its miss to a
   later wave of BA or to BB / BC. `feedback_no-deferrals` is
   in force.
4. **Reversal is a health signal.** Reversals are first-class
   tranche outcomes, not failures. AQ.5's deletion of structural
   pre-scan (`2f7c1bd`) was the cleanest reversal in project
   history and produced the dispatch discipline that landed AP.5
   NibbleLut. BA budgets for at least one reversal per wave as
   the expected case.

## Wave structure

Five waves. Every wave has a runtime call site at its landing commit,
a same-commit bench delta, and a same-commit samply capture.

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Derive-cache lift + audit pass + measurement surface | BA open | planned |
| **W1** | [waves/W1.md](waves/W1.md) | Scalar payload activation — JSON + Sheets first | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | Aggregate / Named struct registry — CSS L4 lightningcss parity | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | Backward container pointer + first consumer (parent walks) | W2 | planned |
| **W4** | [waves/W4.md](waves/W4.md) | FINAL — AU-baseline recovery closure + samply fleet | W3 | planned |

### W0 — Infrastructure and measurement

Derive cache lifts to `$XDG_CACHE_HOME/bbnf-derive/`. The IR audit
pass (every `->` reaches the emitter) lands with a failing gate
that the subsequent waves will turn green. The 17-entry bench
matrix re-anchors on the BA branch. No payload change yet; this
wave proves the measurement surface, not the substrate.

Runtime call site: the audit pass runs on every `cargo check` via
build.rs and emits a machine-readable coverage report to
`docs/benchmarks/BA/audit/coverage.json`. The emitter does not
close the wave until the audit pass is wired.

### W1 — Scalar payload activation (JSON, Sheets)

`push_leaf_with_f64`, `push_leaf_with_i64`, `push_leaf_with_bool`,
`push_leaf_with_span` fire on every JSON number / int / bool / string
leaf and every Sheets cell. `compute_payload_layouts` returns a
non-empty map for JSON and Sheets. `StructRegistry` populates for
JSON `pair`, JSON `value`, Sheets `cell`.

Runtime call site: `NodeView::<Number>::as_f64()` reads the scalar
payload directly, bypassing the string decode path that currently
dominates twitter / canada.

Bench delta gate: JSON twitter ≥ 1967 MB/s at wave close. If not,
the substrate reverts per §Reversal criteria.

### W2 — Aggregate / Named struct registry (CSS L4)

`StructRegistry` populates for CSS L4 `declaration`, `dimension`,
`colour`, `length`, `angle`, `time`, `resolution`, and the other
typed CSS L4 rules. lightningcss parity harness passes node-for-node
on the normalize fixture.

Runtime call site: `css::StyleSheet::rules()` returns typed
`CssDeclaration { property, value: TypedValue }` without a
post-parse re-interpretation pass. Every `<length>` rule returns a
typed `Length` equivalent to lightningcss's `lightningcss::values::length::Length`.

Bench delta gate: CSS normalize ≥ 735 MB/s; bootstrap ≥ 600 MB/s.

### W3 — Backward container pointer

Close tape record carries `parent_off: u32`. The sidecar column
vs in-record widening question is resolved by the measurement from
§Open questions (1) and lands under whichever answer the profile
gives. The first consumer is a parent-walk accessor
`NodeView::parent()` that reads the pointer in O(1).

Runtime call site: at least one CSS L4 accessor
(`Declaration::parent_rule()`) uses the backward pointer; the prior
re-scan path is deleted at the same commit.

Bench delta gate: no regression on any entry; the pointer overhead
must be absorbed by the parent-walk consumer's own win. If the
pointer costs more than it saves at wave close, the substrate
reverts.

### W4 — FINAL

AU-baseline recovery audit: 17-entry matrix must be at or above
the AU baseline on every entry. The lightningcss / sonic-rs parity
harnesses must be green. The IR audit pass must report 100%
coverage. A samply fleet lands under `docs/benchmarks/profiles/BA/`
with cold-parse captures per grammar. `FINAL.md` records honest
deltas, any reversals taken, and any follow-on work routed forward
to BB (pointer paths) or BC (rewrite inference).

## External SOTA grounding

BA's substrate composes four established techniques against bbnf's
grammar-derived discipline:

- **simdjson tape record layout** — 16-byte fixed records, paired
  open/close compounds, forward + backward pointers. bbnf's
  `TapeRec` already matches at the record level; BA closes the
  backward pointer gap. See
  [simdjson tape documentation](https://simdjson.github.io/simdjson/md_doc_tape.html).
- **sonic-rs StructRegistry (morally)** — sonic-rs rejects the
  two-stage approach and populates structs directly via serde.
  BA's equivalent populates via a grammar-derived registry rather
  than serde's `Deserialize`. See
  [sonic-rs](https://github.com/cloudwego/sonic-rs).
- **lightningcss typed values** — every CSS property has a specific
  Rust type derived from the CSS spec grammar. bbnf's position is
  strictly stronger: the grammar is the source, not a Rust enum.
  See [lightningcss](https://lightningcss.dev/).
- **yyjson's dispatch-and-allocation frontier** — SIMD is not where
  the next 10% lives; key dispatch and in-place payload are. bbnf
  already has AP.4 key dispatch and AP.5 NibbleLut; BA's payload
  work is the in-place allocation partner. See
  [yyjson introduction](https://ibireme.github.io/yyjson/).

## Scope additions (absorbed from adjacent tranches)

1. **Derive cache lift to `$XDG_CACHE_HOME/bbnf-derive/`.**
   Deferred from B1 per the build-infra work done there. Payload
   activation thrashes the derive cache harder than any other
   substrate change, and lifting the cache is the cheapest way to
   keep W1 through W3 iteration times under the observed AY-II.W0'
   iter-surface floor. This is a W0 landing.

2. **IR audit pass for `->` coverage.** A new pass in
   `crates/ir/src/passes/audit/payload_coverage.rs` that enumerates
   every grammar's typed `->` and asserts the emitter produces a
   matching `push_leaf_with_*` or `begin_compound` / `end_compound`
   call. The pass runs on every `cargo check` and gates the build.

3. **Canonical parity harness CI wiring.** The existing
   `tests/*_parity.rs` suites run on every wave boundary under a
   CI matrix job; parity regressions block wave close.

## BA handoff contract

BA does not close until all of the following are true:

1. IR audit pass reports 100% `->` coverage across all production
   grammars.
2. `StructRegistry` non-empty for every Named rule in JSON / CSS /
   Sheets / BBNF.
3. 17-entry AU-baseline matrix at or above AU floor on every entry.
4. lightningcss / sonic-rs / simdjson parity harnesses green.
5. Backward container pointer lands with at least one consumer
   using it; the consumer's win exceeds the pointer overhead.
6. `FINAL.md` records deltas, reversals taken, and follow-on work.

## Defensible floor

Non-negotiable:

1. JSON twitter ≥ 1967 MB/s — the first-order recovery gate.
2. CSS bootstrap ≥ 600 MB/s.
3. `StructRegistry` non-empty on all four production grammars.
4. IR audit pass 100% coverage.
5. Workspace ≥ 967 pass, ≤ 33 fail, ≤ 30 ignored.

Anything less is Era V recurring — substrate without activation.

## Post-tranche review candidates

Decision at W4 close, not mid-wave:

- Whether the backward container pointer is a first-class column
  or a sidecar vector (depends on the W3 measurement).
- Whether `StructRegistry` should move into a persisted
  `$XDG_CACHE_HOME` artefact or regenerate on every build.
- Whether the lightningcss parity harness should absorb the CSS L4
  wasm-test matrix into a single CI job.
- Whether BB's pointer-path work should pre-open during W4 (only
  if W3 cleanly demonstrates the backward pointer).

## Indefatigability

When BA closes correctly, every grammar rule that says `-> T`
produces a `T` in the tape. The shape emitter's AW-V.W3 demonstration
stops being the one-time JSON-only peak and becomes the permanent
canonical path for every grammar. The AU-baseline is recovered and
exceeded. BB opens on a settled substrate with a backward pointer
available, and BC opens on a settled semantic surface over which
rewrite-rule inference can operate.
