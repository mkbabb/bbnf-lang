# Archaeological Dig — Tranches AS · AT · AU · AV

Scope: the four consecutive pre-restart tranches spanning
`docs/tranches/{AS,AT,AU,AV}/`. This is the "structural activation → the
Flattening" arc of the original campaign: the era in which the project built
an entire grammar-fingerprinted parsing substrate (dispatch automaton,
precomputed structural index, columnar store, shape dictionary, Eisel-Lemire /
simdjson decode) and then shipped it *dormant*, routing every runtime consumer
forward one tranche at a time until the campaign diverged. All four tranches
are closed; git confirms the close-out commits (`be4b22b12` AV, `5281ec231`
AU, `74ade4c6c` AT, `a6ec7b346` AS).

---

## 1. What each tranche attempted

### AS — Scanner Truth, Direct Projection, CSS Activation

Opened on the back of AR's audit, which had just closed a ten-tranche
bootstrap-loop deferral via modifier recovery in `lower_factor` (idempotent
gen1 == gen2). AS.md declares five phases (`AS/AS.md:87-224`): (1) activate CSS
L4 parsing, (2) admit `TypeDesc::Span` as a scalar payload plus a named-struct
projection scaffold, (3) "scanner truth" — dispatch on `Identifier` sub-flags
and kill six hand-rolled regex string parsers, (4) profile and close the
sonic-rs gap, (5) collapse the regex-miner passthroughs. Nine hard gates
(`AS/AS.md:225-236`).

### AT — Projection Truth, Regression Redress, Bench Parity

AT.md's premise (`AT/AT.md:22-108`) is a blunt audit of AS: five "critical
failures" where the payload infrastructure existed but never fired — f64,
bool, KvPair for JSON, Span admitted-but-unused, StructRegistry always empty —
plus a JSON throughput regression (AQ→AR, −14% to −39%) and the sonic-rs
apples-to-oranges problem. Seven phases (`AT/AT.md:109-486`): a *general*
inlined-node type resolver (`resolve_branch_type`) rather than per-type
patches; regression redress (SIMD number-scanner guard + folding the parallel
`meta: Vec<u8>` into `TapeRec`); a JSON string-decode kernel with an honest
`json_monolithic_value` bench; profile-driven optimization; structural test
validation; named-struct ABI; and CSS spec parity (`|=` attribute-selector
ambiguity, non-ASCII identifiers). Eighteen hard gates.

### AU — Projection Activation, Regression Redress, Scanner Truth (re-run)

AU re-runs the same three themes at larger scope across seven waves
(`AU/FINAL.md:28-241`). Phase 1 finally fires projection (`branch_pushes_children`
was returning `true` for inlined `Ref` nodes, forcing `push_compound` on every
alt branch — every typed capture was a dead store, `AU/FINAL.md:36-60`). Phase
2 is CSS typed parity; Phase 3 string decode; Phase 4 debt elimination
(`ParsedGrammar` deleted — an 11-tranche deferral — and `StructRegistry`
deleted as a dead scaffold); Phase 5 profiling; Phase 6 cross-bench generality
(unified arena on AoS, `.map(|_| ())` elimination 309→0, push-fingerprint
capacity); Phase 7 a columnar (SoA) substrate prototype. Twenty-four hard
gates.

### AV — "The Flattening"

AV is the pivotal tranche and the one that names the campaign's terminal
failure mode. Its charter (`AV/AV.md:1-40`) folds AU's typed-materialisation
debt and *ships the dispatch automaton + PSI pipeline + columnar substrate +
shape dictionary as one coherent architecture*, then aims for sonic-rs and
lightningcss parity. Ten waves V0–V10. Critically, AV.md pre-authorizes broken
intermediate state: "the *workspace* may be intentionally unworkable between
waves 3 and 10" (`AV/AV.md:29-35`). The architectural thesis: "The grammar's
static fingerprint is a first-class **codegen output channel**, not merely an
optimiser input" (`AV/AV.md:66-68`).

---

## 2. What shipped versus what died on the vine

### Landed and real

- CSS L4 parse activation via `Identifier` sub-flag dispatch — the
  `-webkit-*` / vendor-prefix failure was a `RegexClass::Identifier { .. }`
  catch-all discarding `allows_leading_dash` (`AS/PROGRESS.md:22-52`).
- Hand-rolled regex string-parser removal: −263 LOC (`AS/PROGRESS.md:120-129`).
- `resolve_branch_type` — the general inlined-node type resolver that surfaces
  a `TypeDesc` for Map/Constant/FnDescriptor nodes regardless of inlining
  (`AT/PROGRESS.md:79-94`). This is the correct generalized-projection design.
- Regression redress: SIMD number-scanner guard (parse-that `44ae43b`) and
  the `meta: Vec<u8>` fold into `TapeRec`'s kind_meta byte, 5 bits
  (`AT/PROGRESS.md:96-117`). Recovered canada +34% (1089→1464).
- `ParsedGrammar` deletion (`688d6ea`) and `StructRegistry` deletion
  (`ab8588a`) — two long-carried debts (`AU/FINAL.md:141-160`).
- Unified arena on AoS: every `payload_*: Vec<_>` side-car collapsed into one
  `arena: Vec<u8>`; ten+ `push_leaf_with_*` methods collapsed to one
  `push_leaf_with(kind, PayloadData)` (`AU/FINAL.md:205-211`).
- `.map(|_| ())` value-discard eliminated at 309 sites → 0 (`AU/FINAL.md:199-201`).
- The two systemic codegen bugs (Bug 1, Bug 2, Bug 2b) *fixed* in AV.0 for
  BBNF/JSON/Sheets (`AV/FINAL.md:214-219`; all seven BBNF `pinned_*_drops_payload`
  flipped).
- The substrate consts: `GRAMMAR_PROFILE`, `DTA_TABLE`, `SHAPE_DICT`,
  `BBNF_SHAPE_DICT` emitted per-grammar into every `generated.rs`
  (`AV/FINAL.md:63-197`).
- Microbenchmark-validated primitives: Eisel-Lemire 2.1× compute_f64;
  simdjson-style decode 2.46–3.65×; 4-lane reordered visitor 3.3×
  (`AV/FINAL.md:250-255`).

### Died on the vine / deferred forward to AW

Everything that would have turned the substrate into speed:

- The **DTA runtime driver** — the const `DTA_TABLE` is emitted into every
  grammar but *nothing reads it*. Legacy fn-per-rule deletion (AV.3.6) was NOT
  MET; "deleting it without the V4 PSI driver would break `parse()` outright"
  (`AV/PROGRESS.md:916-923`, `AV/FINAL.md:198-213`).
- **PSI rayon parallel payload fill** — gate-closed but never activated
  end-to-end (V6 → AW, `AV/FINAL.md:204-208`).
- **ShapeRef runtime dispatch** — substrate present (`TapeKind::ShapeRef` slot
  13, CSP-selected dictionary), activation deferred (V8 → AW).
- **PHF + SIMD selector classifier** (V7 → AW); **bloom+GADT dedup** (V8 → AW);
  **document-level parallel parse** (V6 → AW); **walker/reader migration
  closure** (V9 → AW) — the entire back half of the ten-wave plan
  (`AV/FINAL.md:198-213`).
- **SoA columnar migration** — prototype hit 1.94× on sum-all-f64 (gate was
  ≥5×), so the actual pivot did not fire; the identified lever (4-lane
  reordered unrolling → 6.64×) routed to AW (`AU/FINAL.md:224-240`).
- **Colour-function aggregates ≥33 B** (AU.2.6 → AV → AW); **Pratt
  precedence-tower lowering** (AU.6.3 → AV → AW); **NEON fractional scan**
  (AR.8.1 → AV → AW).

---

## 3. Root causes of divergence (with evidence)

### 3.1 The substrate-without-activation local minimum — the central pathology

AV built an enormous substrate and shipped *all of it dormant*. Its own FINAL
is explicit: "Every entry is below the post-AU baseline… the AV substrate adds
correctness work… that the legacy fn-per-rule hot path now carries. The
compensating perf wins… all sit in AW V6+ scope" (`AV/FINAL.md:242-261`). An
entire tranche produced **negative net throughput** plus a table of
emitted-but-unread `pub const`s, and was declared complete. The runtime
consumer (the DTA driver) that would justify every added write was deferred by
the tranche's own logic: the fn-per-rule path could not be deleted because
nothing yet consumed the table meant to replace it (`AV/PROGRESS.md:916-930`).
This is `build-infra-first` inverted into a terminal state — infrastructure
that never gets its activation because activation is always "next tranche."

### 3.2 The chronic-deferral cascade / the "AW mirage"

Each FINAL closes by routing its misses to the next letter, and the deferred
ledger only ever grows. `AU/FINAL.md:364-386` is a 20-row deferred-forward
table. Named-struct ABI chained AS.2.3 → AT.6.1 → AU (deleted as dead).
`ParsedGrammar` elimination waited 11 tranches. Colour aggregates, Pratt
lowering, SoA, NEON scan all crossed two or three tranche boundaries without
landing. This directly violates the campaign's own `no-deferrals` precept, and
it is the mechanism by which scope outran delivery.

### 3.3 The projection system never fired — the invariant that never held

For most of the arc the headline value proposition ("every `->` reaches the
tape") was dead infrastructure. AT's own profiling key insight: "the
projection system does not fire for ANY grammar. Every grammar uses >85%
`push_compound` with near-zero typed payloads. The `.map(|_| ())` pattern is
universal (10-202 per grammar)" (`AT/PROGRESS.md:73-75`). Even after AU/AV
fired projection, the cross-grammar audit found **two systemic codegen bugs**
that dropped payloads on a substantial fraction of annotations:

- **Bug 1 — alt-payload first-branch loss**: for Alt-bodied rules, only branch
  0 carried the payload-write block because `next_aggregate_field` advanced
  monotonically across siblings; branches 1+ fell through
  (`AU/typed-parity-audit.md:45-101`, root cause in `AV/PROGRESS.md:220-233`).
  `bool = "true" -> true | "false" -> false` materialized `false` correctly and
  `true` with `has_payload=false`.
- **Bug 2 — `-> Span` lowers to `push_compound`**: BBNF token rules and Sheets
  `-> input : Span` rules reached IR as `TypeDesc::Named("Span")` which the
  emitter never routed to `PayloadData`; **4 sites in source, 0 firing at
  runtime** for the entire BBNF family (`AU/typed-parity-audit.md:102-137,
  196-217`).

`AU/FINAL.md:296-347` records that invariants 2 and 7 ("every `->` reaches the
tape"; "type-descriptor coverage is total") *do not hold*. The value prop was
asserted for tranches before it was structurally true.

### 3.4 Correctness-versus-speed regression whipsaw

canada.json throughput fell monotonically as correctness improved:
AQ 1796 → AT 1483 → AU 1231 MB/s (`AU/profiling-2.md:19-30`). The cause is not
a bug — it is that AU finally *stored* the Eisel-Lemire f64 that AQ had
computed then discarded, and the consuming fast path (DTA) that would amortize
that store never landed. The regression is the direct symptom of §3.1: real
projection is a perf loss until the flattened driver consumes it.

### 3.5 Benchmark honesty crisis (repeatedly self-corrected, never resolved)

The JSON bench was discovered — three separate times — to be not
apples-to-apples with sonic-rs: a tape of spans + payloads versus a fully
materialized `sonic_rs::Value` with decoded strings (`AS/PROGRESS.md:169-178`,
`AT/AT.md:92-99`, `AU/FINAL.md:114-132`). The cssparser comparison was
"misleading": its bench's `parse_declarations() -> false` tokenizes and counts,
it does not build a typed AST, so the "beats cssparser 1.1-1.4×" and "28 GB/s
selectors" figures were structurally dishonest (`AT/AT.md:449-455`). Honest
numbers, once forced, plateaued far below aspiration: value-path sonic ratios
0.77-0.85 (`AU/profiling-2.md:74-80`), lightningcss 3.76-6.30× (typed-vs-typed,
`AU/profiling-2.md:34-38`). Crucially, the gap between aspiration and reality
*grew* as honesty increased — AV's opening gate table demanded canada 1800,
tailwind 1200, sheets 250 against a reality of 1231, 496, 95 (`AV/AV.md:44-63`),
gaps of 1.5×–2.6×.

### 3.6 Institutionalized breakage + masked failures

AV's "wave-failure policy" licensed a seven-wave window of non-green workspace
(`AV/AV.md:29-35`, `AV/PROGRESS.md:920-923`). Compounding it, cargo's
stop-at-first behaviour hid failures: AU's default `cargo test --workspace`
exits 101 on one pre-existing failure while `--no-fail-fast` reveals
967 pass / 33 fail / 30 ignored — "32 failures were hidden by cargo's
stop-at-first" (`AU/FINAL.md:19-27`). A pre-existing "Session-1 18-tests-fail
family" (closures, debug, analysis, graph, gorgeous, lsp, lower, recover) rode
un-triaged across the entire AS→AV span (`AU/FINAL.md:386`).

### 3.7 Agent orchestration races on shared files

AV.0 collapsed a five-agent wave to four "because AV.0.5 + AV.0.6 both touched
`crates/bbnf-tape/src/builder.rs`; the no-shared-writes invariant won the
conflict" (`AV/FINAL.md:34-41`). Agent B's respawn is the cautionary tale: it
hand-patched `generated.rs` to pass `cargo check`, violating clean-regen
discipline; the patch was reverted, and its "broader materialization override
was off-plan" (`AV/PROGRESS.md:159-201`). Multiple cherry-picks required manual
conflict resolution (`AV/PROGRESS.md:981, 1026-1029`).

### 3.8 The per-grammar-constant tension (correctly resolved, but unread)

The campaign's ambient risk — hardcoded grammars, per-grammar backend files —
was actually *beaten* on the emitter side: at AV close, "no hand-written
grammar-specific constants in the emitter" (`grep … const … returns 0`,
`AV/FINAL.md:222`). The resolution was to move all grammar specialization into
*generated* per-grammar `pub const` tables. That is the right instinct. The
failure was that those tables were never consumed at runtime.

---

## 4. Worth resurrecting for the totality plan

### 4.1 The six architecture research designs (`AV/research/01-06`) — crown jewels

These are verbatim architecture deliverables, and with the sole exception of
their substrate scaffolding they never received runtime activation. They are
the highest-value resurrection targets:

- **06 — PSI / DTA / skeleton-payload fission** (`AV/research/06`): the
  architectural spine. Stage-A grammar-fingerprinted DFA-plus-counter emits the
  tape skeleton in one linear pass (no rule-body calls); stage-B is an
  embarrassingly-parallel payload refiner; stage-C is an O(N) prefix-scan span
  close. The "moat" argument is load-bearing for the whole project thesis: a
  grammar fingerprint is *uncopyable* by simdjson/sonic-rs/YYJSON, which have no
  static grammar (`AV/research/06:121`). Honest risks catalogued: small-input
  overhead floor, DTA-generation complexity for nested-optional-with-lookahead,
  thread-startup tax, and the recoverable-errors/diagnostics tension.
- **01 — grammar-parameterised structural bitmap** (`AV/research/01`): one SIMD
  kernel per grammar (NEON `vqtbl1q` nibble-LUT + digraph detection → 64-bit
  structural word; scalar CTZ/PEXT driver, never re-scanning). Carries a
  priceless negative-result archaeology (`AV/research/01:126-137`): this exact
  capability existed and was *deleted* at AQ.5 (`2f7c1bd`) for an economic
  reason (−190 µs citm regression driven by four integration bugs — scalar
  `filter_quote_parity`, duplicated Alt arms, unsaved structural cursor,
  disabled WS elision). v2 avoids all four by shipping scanner-only. **This must
  not be re-learned the hard way.**
- **04 — kind-partitioned columnar SoA** (`AV/research/04`): sibling-skip
  replaces first-child pointer; first-child is mechanically `idx+1` under
  pre-order; `pay_f64.iter().sum()` auto-vectorises. The **measured negative
  result is the value**: a naive SoA pivot only reaches 1.94× because strict-IEEE
  f64 left-fold blocks LLVM reordering; the necessary additional lever is a
  4-lane reordered accumulator (6.64×) emitted as a first-class codegen pass
  (`AU/FINAL.md:224-240`, `AV/FINAL.md:418-427`). SoA is necessary-but-not-sufficient.
- **03 — ShapeDictionary** (`AV/research/03`): e-graph `EClassFacts` identify
  fixed shapes, structural mining names them, a CSP selects which to admit under
  a 256-entry byte budget, the tape materializes `ShapeRef` leaves, the view
  reconstitutes lazily. One integrated system reusing the existing CSP solver
  (`OptimizationMode::MinimizeCost`) via a single new constraint file.
- **05 — content-addressed parse DAG** (`AV/research/05`): bloom-gated hashcons
  (GADT keyed on `(rule_id, body_hash)`) fuses structurally-equal subtrees;
  dedup-eligible rules are statically classified (zero payload-bearing nodes).
  Worked cold-bench estimates for Bootstrap (0.35 dedup factor → 578→780 MB/s)
  and the Sheets precedence tower.
- **02 — FDMP cache locality** (`AV/research/02`, not deeply excavated here):
  the field-decomposed materialisation-pass framing behind the columnar work.

### 4.2 Hard-won measured facts that must anchor any perf plan

- The **profiling-2 hotspot matrix** (`AU/profiling-2.md`): JSON `__value`
  83.3%, CSS `__compoundSelector` 40.0% + `__declaration` 25.7%, Sheets
  precedence tower 86.3%, BBNF `__mapped_factor` 35-41%. The bottleneck is
  always *inside generated rule bodies*, not at scanner/tape boundaries — which
  is precisely why a flattened DTA (not more scanner tuning) is the right lever.
- The **SIMD-number-scanner threshold truth** (`AT/PROGRESS.md:59-71`): NEON
  costs ~16 cycles for 2 digits vs ~10 for scalar SWAR, and 85.6% of canada
  numbers have ≤2 integer digits. Guard SIMD on remaining-byte count; keep it
  only for long fractional runs.
- The **meta_idx fold** (`AT/PROGRESS.md:104-105`): the parallel `meta: Vec<u8>`
  side-channel was a measurable per-record cache tax; packing it into
  `TapeRec` recovered throughput. Keep the tape a single cache-dense record.

### 4.3 Design lessons on the emitter itself

- **`resolve_branch_type`** (`AT/PROGRESS.md:79-94`) is the correct general
  approach to projection — resolve type from the node, never per-type patches.
- **Bug 1's fix sketch** (`AU/typed-parity-audit.md:276-299`,
  `AV/PROGRESS.md:220-233`): the per-branch payload-write must be *structurally*
  hoisted onto every alt branch, not consumed monotonically. The totality plan
  should make "every `->` reaches the tape" a structural guarantee of the alt
  composer, not an audit that keeps finding leaks.
- **The empty-compound `has_payload=true` quirk** (`AU/typed-parity-audit.md:291-298`):
  `push_compound` must write `TapeOffset::NONE` on an empty children-run.

---

## 5. Meta-process assessment

The close-out artefacts themselves are a *success* of process: `AU/FINAL.md`
and `AV/FINAL.md` are honest, evidence-cited, self-critical gate tables that
name their own misses and route them forward. The dig was possible precisely
because these documents did not paper over failure.

The process *failure* is structural, not documentary:

1. **"Substrate now, activation later" became the escape valve** that let a
   whole tranche (AV) ship negative net performance and call itself complete.
   Any future tranche gate that can be satisfied by "emitted the const / built
   the API" without "a consumer reads it in the hot path" repeats the trap.
2. **Over-scoping into forward-routing**: AV opened with 24 inherited gates and
   a 10-wave plan, then cut V6–V9 (the entire perf-activation half) to a
   boundary scope decision (`AV/FINAL.md:1-19`). Plans consistently promised a
   frontier and delivered a substrate.
3. **Institutionalized breakage** (wave-failure policy) plus **masked failures**
   (cargo stop-at-first) let real regressions and a 23-test pre-existing family
   ride across the whole span.
4. **Honesty widened the gap it revealed**: as bench framing became honest, the
   distance to the SOTA targets (sonic, lightningcss) grew rather than shrank —
   a signal that the campaign's aspiration was set against dishonest baselines
   and the real frontier was 1.5×–2.6× away.

The single most important lesson for SK-V21: the DTA/PSI/columnar/shape-dict
substrate is *designed and largely built* — the research is excellent, the
microbenchmarks validate, the negative results (structural-bitmap deletion at
AQ.5; naive-SoA 1.94×) are documented. What the campaign never did was *close
the loop* by activating the flattened runtime driver and deleting the
fn-per-rule path in the same motion. A totality plan must treat activation and
its regression proof as the *first* deliverable of any substrate wave, never the
last — or it will re-enter the same local minimum under a new letter.
