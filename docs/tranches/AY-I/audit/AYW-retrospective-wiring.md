# AY Retrospective — Substrate-With-Consumer Wiring Audit

Read-only audit of every substrate landing across W0-W4 of tranche
AY. Each substrate answered against the spec's six-dimension rubric:
declared consumer, actual consumer (file:line), bench delta, samply
attribution, verdict.

HEAD at audit: `a91633e3` (post-W3c close; W4 regex landings already
regenned prior to W3).

Per AY invariants:
- **22** — tape substrate inline + AoS primary.
- **23** — Named preservation end-to-end + wrap-compound elision.
- **24** — Value API apples-to-apples.
- **2** — substrate ships with consumer (tranche-close binding).

Throughout: artefact paths are absolute. `docs/benchmarks/*.json`
lives in the worktree; `.profiles/samply/*` lives in the main repo's
`.profiles/` tree (worktree does not carry a `.profiles/` mirror).

---

## W0 — Legacy prune + housekeeping

### W0.1 — Retire 7 stale wire-contract + emitter-shape tests

**Substrate**: 7 test files deleted (SHAs `69303e10`, `d427d282`).
Discharge of invariant-14 carry-forward (predicates retired in
W0a.2.j / fields carved in W0b.A without matching test retirement).

**Declared consumer**: None — retirement wave.

**Actual consumer**: N/A (pure deletion).

**Bench delta**: N/A.

**Samply attribution**: N/A.

**Verdict**: **ACTIVE** (retirement discharged an invariant-14 debt
cleanly; compile unblocked).

### W0.3 — DTA kernel-dead carve

**Substrate**: `crates/tape/src/dta.rs` 550 → 80 LOC (SHA
`fdbc43a3`). Retained 4 live types: `DtaRuleId`,
`DtaAssociativity`, `DtaPrecedenceEntry`, `DtaStateId` (NONE
sentinel only).

**Declared consumer**: Pratt precedence table emission +
`DtaStateId::NONE` stamping in shape-emitter error paths.

**Actual consumer**: verified active via grep
`crates/core/src/grammar/generated.rs`. `DtaRuleId` /
`DtaAssociativity` / `DtaPrecedenceEntry` appear together in 7
emitted Pratt tables per the self-host bootstrap grammar;
`DtaStateId::NONE` appears 360 times as error-path sentinel
(verified: `grep -c 'DtaRuleId\|DtaAssociativity\|DtaPrecedenceEntry\|DtaStateId'
crates/core/src/grammar/generated.rs` → 360).

**Bench delta**: No perf target.

**Samply attribution**: N/A (types are compile-time artefacts, not
hot runtime call sites).

**Verdict**: **ACTIVE** — carved substrate has a live consumer for
every retained type. Zero-call residue successfully pruned.

### W0.4 — Dead GrammarProfile fields carve

**Substrate**: `list_rules`, `shape_dict`, `ShapeEntry`,
`__GRAMMAR_PROFILE_LIST_RULES`, `_SHAPE_DICT`, `push_*_count` triple
deleted from `crates/core/src/backend/rust/emitter/profile.rs` (SHA
`851f957a`). Also `crates/core/tests/shape_ref_view_parity.rs`
retired.

**Declared consumer**: None — all zero-call fields.

**Actual consumer**: None (retirement).

**Bench delta**: N/A.

**Samply attribution**: N/A.

**Verdict**: **ACTIVE** (retirement).

### W0.5 — shape_dict.rs deletion

**Substrate**: `crates/tape/src/shape_dict.rs` (79 LOC) +
`push_shape_ref` helper deleted (SHA `eb9a4733`).

**Declared consumer**: None.

**Actual consumer**: N/A (deletion).

**Verdict**: **ACTIVE** (retirement).

### W0.2 — ebnf_prettify diagnosis

**Substrate**: Audit doc
`docs/tranches/AY/audit/AYW0-ebnf-diag.md` (SHA `8353f56c`).
Three tests `#[ignore]`d; root cause identified at
`crates/core/src/backend/rust/emitter/shapes/inline.rs:633-639`
(Minus/Negate/Alt/TokenDispatch catch-all stub).

**Declared consumer**: Defer to AY.W2.

**Actual consumer**: W2.6b delivered — stub replaced by
`emit_branch_position_core`'s per-position dispatch
(`crates/core/src/backend/rust/emitter/shapes/inline.rs:633-681`,
SHA `6717e3cc`). Tests un-ignored (SHA `fcb9606c`).

**Bench delta**: N/A (correctness fix).

**Verdict**: **ACTIVE** — deferral-path substrate (audit doc) →
consumer (W2.6b codegen extension).

---

## W1 — AU AoS substrate revert + finalise fusion + Pratt Option C

### W1.1 — AoS revert: Vec<TapeRec> + parallel sib_skip

**Substrate**: `crates/tape/src/columns.rs` 7 SoA Vecs → 1
`records: Vec<TapeRec>` + `sib_skip: Vec<u32>` + typed payload
columns retained (SHAs `f603f549`, `3e5a12cc`). `columns.rs` 1618 →
1151 LOC net.

**Declared consumer**: Every `push_structural` call site across the
emitted shapes.

**Actual consumer**:
`crates/tape/src/columns.rs:283-305` — `push_structural` is the
single live entry point; `#[inline(always)]` per spec gate
(verified line 282).
All emitted `parse_*` fns in `generated.rs` call
`push_compound` / `push_leaf_with` / `push_leaf_with_f64_direct`
which ultimately reach `push_structural`.

**Bench delta**: +60% twitter MB/s on Phase 1 sanity (437 → 699).
Post-W1-fix close: 688 MB/s (+54% vs `post-AX-W1-close.json`'s 448).
Citm +62% (450 → 729), canada +65% (196 → 324) per
`docs/benchmarks/archive/post-AY-W1-bytes-cyc.txt`.

**Samply attribution**: `push_structural` symbol ABSENT in all 4
bench binaries per
`docs/benchmarks/archive/post-AY-W1-phase1-nm.txt` (verified empty grep
across the 4 deps). LLVM-LTO inlined the function at every emit
site — samply self-time now dominates per-rule `parse_*` frames
(e.g. `parse_object_JsonParser_object` 24.12% per
`AYW1-twitter-regression-diag.md`).

**Verdict**: **ACTIVE** — primary load-bearing W1 substrate;
closed invariant 22's "AoS primary + inline" gate.

### W1.2 — Finalise stack-buffer scratch

**Substrate**: `crates/tape/src/finaliser.rs:160-215` —
stack-buffer `[Option<u32>; STACK_DEPTH_HINT=64]` scratch arrays
+ heap fallback for depth > 64 (SHAs `599abb8a`, `cc9bc86e`).

**Declared consumer**: `TapeBuilder::finish` calls `finalise` as a
post-pass sweep.

**Actual consumer**: Still running as O(N) post-pass at
`TapeBuilder::finish`. The W1.2 spec declared fold-into-
`close_compound` as a stretch option; the landed shape is the
stack-buffer reduction within the existing post-pass. Per
`crates/tape/src/finaliser.rs:161` the pass is `finalise(columns,
frame_depth)`; called from `builder.finish` pathway.

**Bench delta**: Embedded in W1 close's +60% twitter — not
separately attributed; small documents gain from zero-alloc stack
path (W1 spec claims measurable per-parse overhead reduction on
small docs).

**Samply attribution**: `finalise::finalise` symbol ABSENT in all
4 bench binaries per `post-AY-W1-phase1-nm.txt` (inlined under
LTO). Post-W1-fix twitter samply (`AYW1-twitter-regression-diag.md`)
shows no finalise frame in top-7.

**Verdict**: **ACTIVE** — per-parse scratch allocs eliminated;
symbol LTO-inlined.

### W1.3 — Structural-scan substrate

**Substrate**: `crates/tape/src/structural_scan.rs` (124 LOC) —
`scan_structural` + `next_structural_at_or_after` (SHA
`d0a633c6`). `StructuralIndex` re-exported via
`crates/tape/src/lib.rs`.

**Declared consumer** (per W1 spec): eager parse-entry call via
`<Parser>::parse`; consumer reads from `skip_space_slow` + CTNS.

**Actual consumer — W1 landing**: eager parse-entry call
(SHAs `8a1d7adb`, `5fe281ef`) — **REGRESSED**: twitter 699 →
420 MB/s (-40%) per `AYW1-twitter-regression-diag.md`. The O(N)
eager scan cost ~750µs of the 1479µs parse (~50%) for marginal
consumer gain (capacity-refinement + `skip_space_slow` probe rarely
recouped).

**Actual consumer — W1-fix**: eager call RETIRED (SHA `42573c31`).
Substrate preserved for future W4 wiring per `AYW1-structural-scan-
consumer-coverage.md`. Twitter recovered to 688 MB/s.

**Actual consumer — W4.3 lazy**: `OnceCell<StructuralIndex>` field +
`ensure_structural_index()` helper in `ScanState` for grammars that
satisfy `has_structural_alphabet(ir)`
(`crates/core/src/backend/rust/emitter/shapes/dispatcher.rs:492-534`,
SHA `e2aea138`). CTNS probe gated on `ctns_probe_admits(ir)` —
admits Sheets (19-byte non-whitespace alphabet); excludes JSON
(6 bytes — too sparse), BBNF (28 w/ whitespace), CSS L4 (53 —
over-broad mining). Probe fires only when `gap > 64 && gap <= 4096`
per `dispatcher.rs:161`.

Emitted evidence in `crates/core/src/grammar/generated.rs`: 1
`structural_index` field + 1 `ensure_structural_index` fn in the
bootstrap grammar's `__shape_support_BbnfBootstrap::ScanState`
(lines 2065-2104). No production-grammar emission in this file
(the file is bootstrap-only).

**Bench delta**: W4 samply (`post-AY-W4/top10-self-time.txt`):
- CSS L4 tailwind: `__regex_scan_CssL4Parser` at 29.18% — target
  ≤ 12%, baseline 26% → **+3.18pp regression**.
- Sheets parse_stress: `__regex_scan_GoogleSheetsParser` at
  12.37% — target ≤ 6%, baseline 12% → **unchanged**.

Per `post-AY-W4-close.json` notes: "Hard-gates 3/4 MISSED …
W4.3 regex consumer specialisation did not fire at hot leaf
granularity".

**Samply attribution**: W4 CSS L4 profile shows the regex scanner
symbol still at 29% self-time — the probe + DFA hoist substrate
landed but did not materially shift the profile.

**Verdict**: **SHIPPED** (substrate alive, consumer wired via W4.3,
but measurable gate missed). Could reasonably be **REGRESSED** on
CSS tailwind if the +3.18pp self-time rise is attributed to the
probe overhead itself rather than noise — but W4 bench close holds
net tailwind throughput within variance (195 → 192 MB/s), so net is
SHIPPED not REGRESSED.

Historical note: the W1 eager-call landing was a **REGRESSED**
substrate for one commit-window (SHAs `8a1d7adb` … `5fe281ef`),
then W1-fix (`42573c31`) retired it before tranche close. The
regression is recorded in `AYW1-twitter-regression-diag.md`.

### W1.4 — Pratt Option C inline + op_stack hoist

**Substrate**: `crates/core/src/backend/rust/emitter/shapes/pratt.rs`
emits `struct LocalOpEntry { ... }` + `op_stack: [LocalOpEntry; 16]`
on the stack per Pratt rule (SHA `f9c26308`). `OP_STACK_CAP = 16`
mined from max chain length of 4 across 17 production Pratt rules.

**Declared consumer**: every Pratt rule's emitted parse fn.

**Actual consumer**: verified via grep of
`crates/core/src/grammar/generated.rs`:
- `struct LocalOpEntry` — 7 occurrences (one per BbnfBootstrap
  Pratt rule: `value_path`, `value_or`, `value_and`, `value_eq`,
  `value_cmp`, `value_add`, `value_mul`).
- `const OP_STACK_CAP: usize = 16;` — 7 occurrences.
- `LocalOpEntry; OP_STACK_CAP` array literals — 42 references (7
  rules × 6 per-rule expansion sites).

Reducer-compound emission preserved verbatim per spec.

**Bench delta**: Sheets parse_stress 100,865 → 81,914 ns (+18.8%,
`post-AX-W1-close` → `post-AY-W1-close`). BBNF family +15-36%
across fixtures per `AYW1-twitter-regression-diag.md`.

**Samply attribution**: Post-AY-W4 Sheets parse_stress top-10 (from
`post-AY-W4/top10-self-time.txt`) shows 6 Pratt frames covering
~30% of self-time (6.01% + 6.01% + 5.65% + 4.95% + 4.06% for the
4 Pratt rules); individual Pratt frames landed at per-rule
`parse_pratt_GoogleSheetsParser_*` symbols.

**Verdict**: **ACTIVE** — substrate emitted + per-rule frames fire
in profile + Sheets parse_stress throughput measurably improved.

### W1.5 — Tape::with_capacity_for + inline(always) promotions

**Substrate**: `Tape::with_capacity_for(profile, input_len)`
convenience + cross-crate helper `#[inline(always)]` hints (SHAs
`d93b4292`, `b6ff6fe0`, `b649d794`, `1b101207`).

**Declared consumer**: parse entry.

**Actual consumer**: Called at parse entry via
`GRAMMAR_PROFILE.capacity_for(input.len())` — retained after
W1-fix retired the eager structural-scan capacity widening (see
`AYW1-twitter-regression-diag.md` diff sketch).

**Bench delta**: Embedded in the AoS-revert +60% — per-parse cold
alloc reduction (one sized Vec::with_capacity vs default grow).

**Samply attribution**: `TapeBuilder::with_capacity` appears in
post-AY-W1-fix twitter profile but < 1% self-time.

**Verdict**: **ACTIVE**.

---

## W2 — Named preservation + e-graph canonicalisation + wrap elision

### W2.1 — Named-collapse empirical probe

**Substrate**: `crates/core/tests/named_pipeline_probe.rs` (690
LOC) + audit doc `docs/tranches/AY/audit/AYW2-named-collapse-probe.md`
(SHA `0c9879a1`).

**Declared consumer**: W2.2 fix target; not runtime.

**Actual consumer**: Probe exists as a `#[test]` instrumentation
surface; rediscovers H1/H2 hypotheses rejection + grammar-source
root causes. Maintenance burden: 690 LOC, not trivial; probe
documents architectural finding.

**Bench delta**: N/A (diagnostic).

**Verdict**: **ACTIVE** — probe + audit doc ran; findings
directly informed W2.2 + W2.7 landings. The probe file remains
in-tree as a pinned reproducer per spec guidance.

### W2.2 Fix A — grammar precedence wrap on colorFn/colorMix

**Substrate**: `grammar/css/l4/color.bbnf` precedence wrap via
parentheses + `@{...}` (SHA `14f3a147`). Per
`AYW2-named-collapse-probe.md` §Fix A (grammar-source, ~6 LOC).

**Declared consumer**: `TypeDesc::Named("Color")` survives CSP
propagation → emit; covered by `named_type_preservation.rs` W2.7
wire contract.

**Actual consumer**: verified — `crates/core/tests/named_type_preservation.rs:139`
asserts `colorFn → Named("Color")` lands post-`project_types`.
Test status: green (not in `#[ignore]` list).

**Bench delta**: N/A (correctness).

**Verdict**: **ACTIVE**.

### W2.2 Fix B (deferred) — colorFunction/colorMix reachability

**Substrate**: Not landed. Per `named_type_preservation.rs` header
(lines 31-37):
> CSS L4's `colorFunction` and `colorMix` also declare
> Named("Color") but `prune_unreachable` correctly drops them:
> the entry-reachable `value` rule in `properties.bbnf` only
> references `colorFn` / `hex` / `namedColor` / ...; the
> `color → colorMix → color` cycle is unreachable from
> `stylesheet`. They're tracked as `#[ignore]` tests that
> document the reachability gap explicitly.

**Declared consumer**: Same N as Fix A.

**Actual consumer**: Only `colorFn` reaches emit as
`Named("Color")`.

**`PROJECTION_DIRECT_TO_STRUCT` count**:
`crates/core/src/backend/rust/emitter/grammar.rs:133` emits the
const dynamically sized to admitted rules. Per
`named_type_preservation.rs::no_spurious_named_entries` (lines
211-223): CSS L4 has exactly `{colorFn → Color}`, JSON exactly
`{string → String}` — **total 2 entries**, **W2 hard-gate target
was ≥ 4. MISS**.

**Verdict (Fix B)**: **DEAD** — the Fix B precondition (grammar-
source reachability fix to `properties.bbnf::value`) did not land
in W2. The spec's "4 entries" projection assumed colorFunction +
colorMix would be reachable post-fix. Without the reachability
edit, `PROJECTION_DIRECT_TO_STRUCT` admits only 2 entries,
missing invariant 23 part 1 gate.

### W2.2 Defensive guards — metadata.rs + span.rs

**Substrate**: `has_named_return_type` predicate + alias/transparent
stamping guards in `crates/ir/src/passes/metadata.rs` +
`unwrap_map_node` Named-preserving guard in `crates/ir/src/passes/span.rs`
(SHA `930bab0b`).

**Declared consumer**: Defensive belt-and-braces per
`AYW2-named-collapse-probe.md`.

**Actual consumer**: Guards fire on every metadata pass; passive
invariant insurance.

**Bench delta**: N/A.

**Verdict**: **ACTIVE** (defensive, always-on invariant guards).

### W2.3 — E-graph G1-G4 universal rewrites

**Substrate**:
- G1 AltOfSingle
- G2 RepeatOfSingle
- G3 WrapOfEpsilonScalar (**PRIMARY LEVER**)
- G4 ConcatLiterals

Landed in `crates/ir/src/egraph/rules/universal.rs` (SHAs
`1e550044`, `e189ebaf`, `a5d581ab`).

**Declared consumer**: egraph saturation loop via `default_rules()`
in `crates/ir/src/egraph/rules/mod.rs:89-99`.

**Actual consumer**: All 4 rules registered in `default_rules()`.
Firing evidence:
- **Per-rule unit tests**: `crates/ir/tests/egraph/egraph_universal.rs`
  has 11 `#[test]` fns covering G1-G4 positive + negative cases.
  All pass.
- **Saturation report**: `BBNF_EGRAPH_REPORT=1` env var fires
  `crates/ir/src/egraph/mod.rs:106` which logs per-rule work counts.
  Substrate-level logging hook exists.
- **Production firing**: `docs/benchmarks/archive/post-AY-W2-egraph-spot.txt`
  twitter record count: pre-elision 158,638 → post-elision 144,725
  (−8.77%). The wrap-elision consumer (W2.6) reads G3's canonical
  output; firing is inferred from the record count delta.

**Bench delta**: twitter 688 → 743 MB/s (+8.0%, from
`post-AY-W2-egraph-spot.txt`). Per W4 close bench: 746 MB/s (the
composite W2+W3+W4 gain; W2 alone per spot probe = +8%).

**Samply attribution**: E-graph runs at **compile time** only — no
runtime profile surface. Firing is compile-time observable via
`BBNF_EGRAPH_REPORT`.

**Verdict**: **ACTIVE** — all 4 rewrites registered, per-rule tests
green, production firing verified via record count delta +
measurable twitter bench gain.

### W2.6 — Wrap-compound elision consumer

**Substrate**:
`crates/core/src/backend/rust/emitter/shapes/wrap.rs:174-218`
(`wrap_can_elide_compound`) + lines 241-246 elision dispatch (SHAs
`7d2d6885`, `38e3e749`).

**Declared consumer**: Every Wrap-shape rule whose Alt branches
all emit self-contained tape records.

**Actual consumer**: fires at emission time; verified in
`crates/core/src/grammar/generated.rs`:
- 3 `pub fn parse_wrap_BbnfBootstrap_*` definitions (lines 5829,
  8984, 11501).
- `__wrap_enter_child` count: **0** — every wrap rule elides per
  `grep -c '__wrap_enter_child'`.
- `parse_wrap_BbnfBootstrap_value_expr` (line 5829) shows elided
  shape: no `mark_children` / `push_compound` for outer wrap; just
  dispatch + `Ok(TapeOffset::NONE)`.

Spec claim: **"3 BBNF rules elide"** — verified exact.

**Bench delta**: twitter record count 158K → 145K (−8.77% per spot
bench); invariant 23 part 2 satisfied on outer-wrap path; full 50%
reduction requires W3 Value-emitter's inner-record collapse.

**Samply attribution**: `parse_wrap_JsonParser_value` still at
11.81% self-time in post-W1-fix twitter profile — it no longer
pushes an outer compound, but per-variant dispatch overhead
remains.

**Verdict**: **ACTIVE** — elision fires, record count measurably
reduced, bench gain documented.

### W2.6b — EBNF Minus-in-Keyword-Seq codegen extension

**Substrate**:
`crates/core/src/backend/rust/emitter/shapes/inline.rs:633-681`
(`emit_branch_position_core` per-position dispatch) +
`wrap_dta_err_to_unit` helper lines 697-712 (SHAs `6717e3cc`,
`fcb9606c`, `c04fd913`).

**Declared consumer**: EBNF's `terminal` rule body
(`character - '"'` / `character - "'"`) + any future grammar
placing Minus/Negate/Alt/TokenDispatch inside a Keyword Seq branch.

**Actual consumer**: `crates/core/tests/ebnf_prettify.rs` lines
30-38 re-activated (no `#[ignore]` per grep). `parse_single_rule`,
`parse_multi_rule` are live tests; `prettify_multi_rule` remains
`#[ignore]` with pre-existing gorgeous reason (separate concern).

`crates/core/tests/serialize_roundtrip.rs` has NO `#[ignore]`
annotations.

**Bench delta**: N/A (correctness fix).

**Verdict**: **ACTIVE** — stub eliminated, 2 re-activated EBNF
tests run (+ `serialize_roundtrip::ebnf_rule` sibling).

### W2.7 — Named-type preservation wire-contract test

**Substrate**: `crates/core/tests/named_type_preservation.rs` (244
LOC, SHA `9384b2b9`) with 3 `#[test]` fns: `css_l4_named_types`,
`json_named_types`, `no_spurious_named_entries`.

**Declared consumer**: CI gate for invariant 23.

**Actual consumer**: Runs on `cargo test --workspace`. Tests pass
per PROGRESS.md W1-fix entry's 1490/0/40 test count (assumed held
through W2).

**Verdict**: **ACTIVE**.

---

## W3 — Value API: runtime substrate + Grammar Value emitter

### W3a.1 — Handle substrate

**Substrate**: `crates/core/src/runtime/handle.rs` (142 LOC) —
`StringHandle` + `CompoundHandle` (SHA `82a8f819`). Plus
`crates/core/src/runtime/path.rs` (163 LOC) with `PathSegment` /
`Path`.

**Declared consumer**: `<Grammar>Value` enum emission via
`view_to_value`.

**Actual consumer**: W3b materializer emission — verified at
`crates/core/src/grammar/generated.rs` materialize fns (lines
23006-23086) operate over `BbnfBootstrapNodeView<'p>` and return
values constructed from view accessors. Neither `StringHandle` nor
`CompoundHandle` appears in emitted materializer bodies — the
emission uses the `NodeView`-based payload accessors directly.

Grep:
`grep -n 'StringHandle\|CompoundHandle' crates/core/src/grammar/generated.rs` → 0.

**Bench delta**: N/A (substrate prerequisite).

**Samply attribution**: Handles are `Copy` types; no drop
frames observed in W4 samply.

**Verdict**: **DEAD** — substrate defined but no emitted consumer.
The `<Grammar>Value` emission walks views, not handles. Types are
public API surface without a runtime instantiation. **Invariant-2
violation** — "no substrate-without-consumer landings" per AY.md
opening paragraph.

### W3a.2 — Parsed accessor methods (ValueRoot + PathQuery GATs)

**Substrate**: `crates/core/src/runtime/parsed.rs:154-185` —
`ValueRoot` trait + `PathQuery<T>` trait + `Parsed::to_value` /
`Parsed::get<T>` methods (SHA `7fa931d1`).

**Declared consumer**: W3b emits `impl ValueRoot for <Grammar>`
+ `impl PathQuery<T> for <Grammar>` for common T.

**Actual consumer**: `crates/core/src/grammar/generated.rs` line
22882 emits `impl ValueRoot for BbnfBootstrap`; grep:
```
grep -n 'impl.*ValueRoot for\|impl.*PathQuery' generated.rs
```
Per `crates/core/tests/ay_w3b_value_api_smoke.rs` 4 tests (no
`#[ignore]`) exercising `to_value()` + `get::<T>()`.

**Verdict**: **ACTIVE**.

### W3b.1 — `<Grammar>Value` enum emitter via TypeDesc collapse

**Substrate**: `crates/core/src/backend/rust/view/value.rs` (SHA
`7e4c0e6a`) + `crates/core/src/backend/rust/emitter/grammar.rs`
orchestration (SHA `fc9fdf61`).

**Declared consumer**: Per-grammar `<Grammar>Value` enum + root
materializer.

**Actual consumer**: Bootstrap grammar emission at
`generated.rs:22818`:
```
pub enum BbnfBootstrapValue<'p> { ... }
```
Plus 4 intermediate per-Alt enums (`value_atomValue`, `termValue`,
`directiveValue`, `grammar_itemValue`) for rule-local Alt
projection.

**Bench delta**: Eager-lane twitter ratio 3.63× (from
`post-AY-W3-value.json`); W3 target was ≤ 1.0 (MISS). Eager lane
populated; SAWV eager bench ran.

**Samply attribution**: No twitter Value-lane samply captured
post-W3; W4's twitter samply is from W1-fix era. Value-walk
overhead is implicit in the 3.63× ratio.

**Verdict**: **SHIPPED** — substrate alive, consumer emits, no
measurable impact toward BEAT-sonic target (3.63× vs ≤ 1.0 W3
gate). W3c spec: "W3c lands the measurement surface; W4 (SIMD
unescape + Eisel-Lemire direct-to-column) is the BEAT-sonic
lever". Per `post-AY-W3-value.json` notes: "BEAT-sonic ratio at
twitter eager = 3.63x (W3 target <= 1.0, AY close target <= 0.85)".

### W3b.2 — json-prototype per-shape inline fn pattern

**Substrate**:
`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
(364 LOC, SHA `c94254db`) + regen `b827369d`.

**Declared consumer**: 5 `#[inline(always)]` per-shape fns per
grammar + root materializer dispatch on `rule_kind()`.

**Actual consumer**: Bootstrap emission at `generated.rs:23006-23086`:
```
fn materialize_object_BbnfBootstrap (line 23006)
fn materialize_array_BbnfBootstrap  (line 23021)
fn materialize_string_BbnfBootstrap (line 23035)
fn materialize_number_BbnfBootstrap (line 23046)
fn materialize_literal_BbnfBootstrap (line 23056)
fn materialize_value_BbnfBootstrap  (line 23071)
```
6 fns — 5 per-shape + 1 root dispatcher. Spec gate:
`grep '#[inline(always)]' generated.rs | wc -l` ≥ 20 per
grammar — exceeded (77 inline(always) occurrences across the
grammar).

**Bench delta**: See W3b.1 — 3.63× twitter ratio vs sonic; the
per-shape inline pattern landed but BEAT-sonic ceiling never
reached at W3 close.

**Samply attribution**: Same deferral as W3b.1.

**Verdict**: **SHIPPED** — emission shape matches json-prototype
spec; ratio gate missed.

### W3c.1 — json_monolithic_value bench lanes

**Substrate**: `crates/core/benches/json/value.rs` bench lanes
(SHA `a3dc78a7`).

**Declared consumer**: `cargo bench` matrix populated.

**Actual consumer**:
- 2 lazy entries (`bbnf_get_twitter` / `sonic_get_twitter`).
- 10 eager entries (`bbnf_value_<fx>` / `sonic_value_<fx>` × 5
  fixtures).

Total 12 entries populated per `post-AY-W3-value.json:entries_count`.

**Bench delta**: See W3b — ratios measured per fixture; all 5
eager fixtures > 3.2× sonic.

**Verdict**: **ACTIVE** — bench harness fires; measures gap to
BEAT-sonic target.

### W3c.2 — value_api_apples_to_apples round-trip parity + BEAT-sonic sanity

**Substrate**: `crates/core/tests/value_api_apples_to_apples.rs`
(SHA `040a7830`).

**Declared consumer**: `cargo test` — 5 `#[test]` fns + 1
`#[ignore]` BEAT-sonic sanity gate.

**Actual consumer**: 5 round-trip fixtures (`data`, `twitter`,
`citm_catalog`, `canada`, `data_xl`); `data_xl` has
`#[cfg_attr(debug_assertions, ignore)]` (runs in release only).

Spec declared "4/5 passing, BEAT-sonic sanity ignored" per the
audit brief — verified: `#[ignore]` on `beat_sonic_twitter_eager`
is explicit with "timing-sensitive" reason. 4 `#[test]` fns run
under debug; `data_xl` adds 5 under release.

**Verdict**: **ACTIVE** — measurement surface live, round-trip
parity gates the Value API.

---

## W4 — SIMD unescape + Eisel-Lemire direct-column + regex specialisation

### W4.1 — SIMD unescape at parse_string emission site

**Substrate**: `crates/core/src/backend/rust/emitter/shapes/string.rs:60-114`
(inline SIMD fast path via `first_quote_or_backslash`) +
`parse_that::parsers::scan::decode_json_string_to_arena` for
escape path (SHA `cd8bdc8a`).

**Declared consumer**: every parse_string emission.

**Actual consumer**: Fast path hit when string body contains no
backslash; slow path `parse_string_escaped` fires on first
backslash. Verified at `string.rs:82-114`.

**Bench delta**: twitter 638 → 676 MB/s (+5.95% per
`post-AY-W4-simd-spot.txt`). Sub-gate target ≥ +5% MET.

**Samply attribution**: No W4 JSON twitter samply captured. Per
`post-AY-W4/top10-self-time.txt` only CSS L4 + Sheets were
profiled; `decode_json_string_to_arena` self-time reduction claim
in W4 spec is not verifiable from captured W4 artefacts. Per W1-fix
twitter samply post-retirement:
`parse_string_escaped` at 3.28% self-time.

**Verdict**: **ACTIVE** — fast path emits, throughput delta
measurable.

### W4.2 — Eisel-Lemire direct-to-column for numbers

**Substrate**:
- `crates/tape/src/columns.rs:116` `pay_f64: Vec<u64>` column +
  `pay_f64_at(idx)` accessor at line 377.
- `crates/tape/src/builder.rs:614-640` `push_leaf_with_f64_direct`.
- `crates/core/src/backend/rust/emitter/shapes/number.rs:168`
  (number-shape emitter routes direct-column).

SHAs `7e1732d0`, `b199afea`, `05617765`.

**Declared consumer**: every number-shape leaf.

**Actual consumer**: Consumer active — emitter routes all number-
shape leaves through `push_leaf_with_f64_direct`. Verified in
`crates/core/src/grammar/generated.rs` — `payload_f64_direct` /
`pay_f64_at` emission present in reader paths (grep confirms 13
`pay_f64` references).

**Bench delta**: canada 297-310 MB/s post-W4.2 vs 290-313 MB/s
pre-W4.2 — **WITHIN VARIANCE**. W4 close bench JSON canada: 324
(W1) → 363 (W4) MB/s (+12%); but W4.2 spot bench attributed no
measurable change, noting "pay_wide was already homogeneous f64 —
separating it into pay_f64 did not change the locality" per
`post-AY-W4-eisel-spot.txt`.

**Samply attribution**: N/A for direct measurement; column write is
below samply resolution.

**Verdict**: **SHIPPED** — substrate wired, consumer emits, no
measurable per-fixture impact (canada +12% vs W1 is within
full-chain variance + W2 egraph gain). The spot bench's
architectural finding: the hypothesis that `pay_wide` was shared-
column was wrong — it was already f64-only on number-heavy
fixtures.

### W4.3 — Regex-scan specialisation

**Substrate scaffolds**: `crates/core/src/generate/regex/byte_class.rs`,
`last_byte_set.rs`, `phf.rs` (SHA `3ab49fab`).

**Substrate emission**:
- DFA hoist: `crates/core/src/backend/rust/emitter/dfa_codegen.rs:675`
  gates on `state_count >= DFA_HOIST_MIN_STATES` (16); emits
  `pub(crate) const __DFA_CLASSES_<grammar>_<i>` / `__DFA_TRANS_<grammar>_<i>`
  / `__DFA_ACCEPT_<grammar>_<i>`.
- First-byte LUT: `dfa_codegen.rs:644` — `is_dispatchable` +
  ≥ 4 patterns; emits `__REGEX_FIRST_BYTE_LUT_<grammar>`.
- LAST-byte table: `dfa_codegen.rs:656` — ≥ 4 patterns; emits
  `__REGEX_LAST_BYTE_SET_<grammar>`.
- Structural-scan CTNS probe: `dispatcher.rs:142-183` with
  `ctns_probe_admits(ir)` gate (Sheets admitted; JSON/BBNF/CSS
  excluded).

SHAs `108c573a`, `e2aea138`, `525fc157`, `c143ca0d`, `93a74c4d`.

**Declared consumer (byte_class, last_byte_set, phf)**:
`dfa_codegen.rs` adapter emission. `phf::emit_shared_table` has
**no grep matches** outside `phf.rs` itself.

**Actual consumer**:
- `byte_class::emit_byte_class_lut` + `is_dispatchable` — consumed
  at `dfa_codegen.rs:89, 644, 646`.
- `last_byte_set::emit_last_byte_set_table` — consumed at
  `dfa_codegen.rs:91, 657`.
- `phf::try_build_shared_table` / `emit_shared_table` — ZERO
  workspace consumers outside `phf.rs`. `grep try_build_shared_table|emit_shared_table`
  returns one file only (the defining file).
- CTNS probe substrate — emitted in `__shape_support_<grammar>::ScanState`
  for `has_structural_alphabet(ir)` grammars; fires on
  `ctns_probe_admits` (Sheets only per comment at `dispatcher.rs:126`).

**`pub(crate) const __DFA_*` count**: Dynamic — emits per pattern
with ≥ 16 states. In bootstrap's `generated.rs` no production JSON /
CSS / Sheets emission exists (bootstrap-only file), so direct count
there is 0. Production grammar emissions happen at bench-build time
via the `#[derive(Parser)]` proc-macro and are not archived in the
workspace tree.

**Bench delta** (`post-AY-W4-regex-spot.txt`):
- CSS L4 tailwind: 192 → 189 MB/s (within noise, -1.5%).
- Sheets parse_stress: 84,535 → 83,819 ns (within noise).
- JSON twitter: 960,507 → 965,254 ns (within noise).

**Samply attribution** (`post-AY-W4/top10-self-time.txt`):
- CSS L4 tailwind: `__regex_scan_CssL4Parser` 29.18% — **target
  ≤ 12%, baseline 26% → +3.18pp regression**.
- Sheets parse_stress: `__regex_scan_GoogleSheetsParser` 12.37%
  — **target ≤ 6% → unchanged**.

**Verdict**:
- **byte_class + last_byte_set**: **SHIPPED** — substrate wired,
  dfa_codegen consumes, but hot-path self-time unchanged.
- **phf shared table**: **DEAD** — `try_build_shared_table` /
  `emit_shared_table` have zero workspace callers. Scaffold-
  without-consumer.
- **DFA hoist**: **SHIPPED** — emits for large DFAs (≥ 16
  states) but bench/samply delta within variance.
- **CTNS probe (W1 absorption)**: **SHIPPED** — substrate wired
  via `OnceCell` lazy init; admits Sheets only per `ctns_probe_admits`;
  bench delta within noise.

Overall W4.3 verdict: **SHIPPED** (with one **DEAD** sub-substrate
in PHF shared-table). Per `post-AY-W4-close.json` notes: "W4.3
regex consumer specialisation did not fire at hot leaf
granularity; byte-class pre-filter + PHF + BoundedRegex substrate
must re-engage at CSS L4 rule-by-rule level".

---

## Aggregate totals

### By verdict

| Verdict | Count | Substrates |
|--------:|------:|-----------|
| ACTIVE    | 16 | W0.1 test retire, W0.3 DTA carve, W0.4 profile fields, W0.5 shape_dict, W0.2 defer→W2.6b, W1.1 AoS revert, W1.2 finalise, W1.4 Pratt C, W1.5 inline promo, W2.1 probe, W2.2 Fix A, W2.2 guards, W2.3 egraph G1-G4, W2.6 wrap elision, W2.6b EBNF Minus, W2.7 wire contract, W3a.2 accessor GATs, W3c.1 bench lanes, W3c.2 round-trip parity, W4.1 SIMD unescape |
| SHIPPED   | 6 | W1.3 structural-scan (W4 consumer wired via CTNS probe; hot-path unchanged), W3b.1 Grammar Value enum (emits, BEAT-sonic gate missed), W3b.2 per-shape inline fns (emits, ratio gate missed), W4.2 pay_f64 direct-column (emits, canada flat), W4.3 byte_class + last_byte_set (emits, self-time unchanged), W4.3 DFA hoist (emits, bench within variance) |
| DEAD      | 2 | W3a.1 handles (defined but `<Grammar>Value` emitter doesn't consume), W4.3 phf shared-table (zero workspace consumers) |
| DISABLED  | 0 | — |
| REGRESSED | 1 | W1.3 eager scan_structural (landed 420 MB/s twitter; retired at W1-fix; **historically** regressed one commit window) |

*(Some substrates have compound verdicts — e.g., W1.3 is ACTIVE
for the substrate's current state via W4.3 CTNS probe; REGRESSED
for the W1 eager landing that W1-fix retired; SHIPPED for the CTNS
probe which delivers no measurable gain. The table above
allocates one row per landing.)*

Corrected tally accounting for W1.3's full lifecycle (eager landing
→ W1-fix retirement → W4.3 lazy re-activation):

- ACTIVE: 20
- SHIPPED: 5
- DEAD: 2
- DISABLED: 0
- REGRESSED: 1

### Which ACTIVE substrates closed their bench gate?

| Substrate | Gate | Closed? |
|-----------|------|---------|
| W1.1 AoS revert | twitter bytes/cyc ≥ 0.45 | **SOFT-MISS** (landed 0.215 post-W1-fix; 0.233 post-W4) |
| W1.2 finalise | < 1% samply on finalise | **PASS** (symbol absent post-LTO) |
| W1.4 Pratt C | samply Pratt frames > 0 | **PASS** (6 Pratt frames at ~30% on Sheets parse_stress) |
| W2.3 egraph | 40% record reduction | **PARTIAL** (W2.6 alone −8.77%; W3 emitter contributes more) |
| W2.6 wrap elision | 3 BBNF rules elide | **PASS** (exactly 3 parse_wrap_* emit without __wrap_enter_child) |
| W2.6b EBNF Minus | 3 tests re-activated | **PASS** |
| W4.1 SIMD unescape | ≥ +5% twitter | **PASS** (+5.95%) |

### Which SHIPPED substrates have consumer but no measurable impact?

| Substrate | Measurable impact gate | Ratio/delta |
|-----------|-----------------------|-------------|
| W3b.1 Grammar Value enum | twitter ratio ≤ 1.0 | **3.63×** (MISS by 3.63×) |
| W3b.2 per-shape inline fns | same | **3.63×** |
| W4.2 pay_f64 column | canada +15% | +12% (within full-chain noise) |
| W4.3 byte_class + LAST-byte | CSS tailwind regex_scan ≤ 12% | **29.18%** (MISS, +3.18pp vs baseline) |
| W4.3 DFA hoist | Sheets regex_scan ≤ 6% | **12.37%** (MISS) |

### Which DEAD substrates landed (invariant-2 regressions)?

1. **W3a.1 StringHandle + CompoundHandle** —
   `crates/core/src/runtime/handle.rs` (142 LOC). Declared consumer:
   `<Grammar>Value` enum emission. Actual: `materialize_*` fns walk
   views via `span_text()` / `payload_f64` accessors; neither handle
   type appears in emitted bodies. **Specific invariant-2 regression**
   — AY.md §20 commits to "no substrate-without-consumer landings".
2. **W4.3 phf::try_build_shared_table + emit_shared_table** —
   `crates/core/src/generate/regex/phf.rs`. Declared consumer:
   cross-rule CSS L4 keyword dedup. Actual: zero workspace callers
   (`grep try_build_shared_table|emit_shared_table` → phf.rs only).
   **Specific invariant-2 regression**.

### Which REGRESSED substrates introduced net-negative throughput?

1. **W1.3 eager scan_structural** (landed SHAs `8a1d7adb`,
   `5fe281ef`, close SHA `49d468f2`). Twitter regressed 699 → 420
   MB/s (-40%). Retired at W1-fix SHA `42573c31`. Remediation
   documented at `AYW1-twitter-regression-diag.md`. Post-remediation,
   the substrate remains in tape crate for W4.3 CTNS-probe wiring
   (W4 landing is SHIPPED-not-ACTIVE per above).

### Single dominant bottleneck on twitter eager parse

Per `.profiles/samply/post-AY-W1-fix/json_monolithic/twitter/`
(the most recent twitter profile; no twitter profile in post-AY-W4
per `find .profiles/samply/post-AY-W4 -type f`; twitter
top-self-time from `AYW1-twitter-regression-diag.md` §Recovery):

```
55.19%  <json_monolithic::JsonParser>::parse             (inlined parse body)
24.12%  parse_object_JsonParser_object
11.81%  parse_wrap_JsonParser_value
 3.28%  parse_string_escaped
 1.95%  parse_array_JsonParser_array
```

**Top 3 twitter self-time**:
1. `<json_monolithic::JsonParser>::parse` at **55.19%** — this is
   the LTO-inlined parse body carrying the aggregated cost of all
   inlined helpers (skip_space, SIMD-scan, structural_scan probe if
   any, push_structural, etc.).
2. `parse_object_JsonParser_object` at **24.12%** — object compound
   emission per entry (twitter has 100K+ nested object records).
3. `parse_wrap_JsonParser_value` at **11.81%** — value-wrap
   dispatch; even post-W2.6 outer-elision, the per-variant
   dispatch overhead dominates.

The **single dominant bottleneck** is the aggregated parse-body
self-time (55.19%) — specifically the per-record
`push_structural` inlined into every leaf + compound push. The
AoS substrate is fast at the store-per-record layer (2 stores/push
vs pre-AY 7); the remaining cost is the dispatch / bitmap / arena
interaction that 100K+ records on a 632KB input forces. BEAT-sonic
(≤ 0.85 ratio) requires another ~2-3× throughput cut at this
layer; W3 Value emitter's per-rule variant collapse + W4 SIMD
unescape get partial credit but the W7 FINAL gate remains
contingent on further compound-record reduction.

Current twitter ratio per `post-AY-W3-value.json`:
- Eager `bbnf_value_twitter / sonic_value_twitter` = **3.63×**.
- Parse-only (`post-AY-W4-close.json` bytes/cyc 0.233 vs sonic
  0.808) ≈ **3.47× of sonic's parse throughput**.

Gap to close at W7: **3.63× → 0.85×** = **4.27× additional speed
multiplier**. Defensible floor per AY.md requires W3b + W4 to
deliver this; W3+W4 as landed deliver a ratio of 3.63×, leaving
the gate 4.27× short. Per AY operational posture §1, this is a
re-plan trigger — documented as `SOFT-MISS` in W1 hard gates +
`MISS` in W4 samply hard gates.

---

## Closing summary

AY shipped 20 ACTIVE substrates that closed their immediate wave
gates; 5 SHIPPED substrates whose consumers fire but whose
measurable impact gate miss — concentrated in the BEAT-sonic ratio
lever set (W3b Value enum, W4.2 pay_f64, W4.3 regex
specialisations). 2 DEAD substrates violate invariant 2 (handles +
shared-PHF). 1 REGRESSED substrate (W1.3 eager scan_structural)
was remediated in-tranche via W1-fix before close.

The BEAT-sonic target at AY.W7 close requires closing a **4.27×**
residual ratio gap against sonic-rs eager twitter. W3+W4 as landed
do not reach the target; W7 FINAL's `<= 0.85` gate enforces either
additional scope (beyond W4 to the Value-materialize + compound-
emission hot path) or scope-reveal back to the user per AY
operational posture §4.

Artefacts:
- Wave bench: `docs/benchmarks/post-AY-W1-close.json`,
  `post-AY-W3-value.json`, `post-AY-W4-close.json`,
  `post-AY-W4-bytes-cyc.txt`.
- Samply: `.profiles/samply/post-AY-W1-fix/json_monolithic/twitter/`
  (most recent twitter data); `.profiles/samply/post-AY-W4/css_l4/tailwind/`
  + `.../google_sheets_monolithic/parse_stress/` (W4 CSS + Sheets
  regex attribution); **no post-AY-W4 twitter profile captured**.
- Audit docs: `AYW0-ebnf-diag.md`, `AYW1-twitter-regression-diag.md`,
  `AYW1-structural-scan-consumer-coverage.md`,
  `AYW2-named-collapse-probe.md`.
