# AW-IV Path Forward — Profile + Brainstorm Synthesis

Ten agents ran in parallel against master HEAD `f457b4df` (post-AW-IV-W2
close):

**Profile wave (6 samply + static-audit agents):**
- `aw4-profile-p1-json-monolithic.md`
- `aw4-profile-p2-css-l4.md`
- `aw4-profile-p3-sheets.md`
- `aw4-profile-p4-bbnf.md`
- `aw4-profile-p5-bbnf-vs-sonic.md`
- `aw4-profile-p6-begotten-code-audit.md`

**Brainstorm wave (4 design agents):**
- `aw4-b1-prototype-architecture.md`
- `aw4-b2-bbnf-tape-arch.md`
- `aw4-b3-bbnf-simd-scan-arch.md`
- `aw4-b4-generalisation-strategy.md`

## 1. The unified diagnosis

1. **DTA is architecturally viable.** P2 explicitly: no separate route
   needed for CSS. The substrate (`bbnf-tape` + `bbnf-simd-scan`) is
   complete in algorithmic content. The consumer is inverted.

2. **`try_branch` is the dominant residual interpreter surface.** It is
   a cross-crate helper containing an inlined copy of `dispatch_one` +
   a runtime `match table.states[N]`. Every AltLinear branch attempt
   re-interprets the state table at the cross-crate boundary:
   - CSS L4: 70.9–78.9% self-time (P2)
   - Sheets: 52–72% self-time (P3)
   - BBNF: ~70% self-time (P4)
   - JSON: not dominant (JSON Alts compile as `ByteDispatch`, not AltLinear)
   
   CSS L4's 153.9 KB walker is **0.1–0.2% self-time** (P2) — the hot
   path never reaches the walker's outer `match cur` because it is
   trapped inside `try_branch`.

3. **JSON has four un-inlined helpers** that W2.1's inline-list
   missed, consuming 26–34% of every parse (P1):
   - `advance_or_pop_with` (10–17%)
   - `finaliser::finalise` (11–14%, flat across inputs)
   - `psi::write_decoded` (0–6%, workload-dependent)
   - `FrameStack::nearest_variant_frame` (1.7–2.6%)
   
   Plus `dec2flt::lemire::compute_float<f64>` (canada-only, 6%).

4. **Bench walks the tape twice.** P5 noted `walk_cursor` at 12–14%
   self-time — the JSON bench parses into the tape then traverses the
   tape to materialise `Value`. sonic-rs fuses these into one pass via
   a monomorphised `DocumentVisitor`.

5. **CSS L4 walker overflows L1 i-cache.** P6: walker symbol 153.9 KB
   (128 KB Apple M L1 i-cache) with 2,283 `bl` calls to `#[cold]
   __cold_state_N` helpers inside the walker module. W1.4-aggressive's
   inline DFA bodies pushed each arm past the single-function-inlining
   budget; the emitter spilled state arms to `#[cold]` siblings called
   via BL. Every state visit pays a cold-call boundary.

6. **BBNF's wire-contract pipeline dropped data.** P4 + P6: BBNF's
   `GRAMMAR_PROFILE` literal is `&[]` across every field (singletons,
   digraphs, quote_classes, keyword_tables, shape_dict, active_columns,
   etc.) despite the IR mining succeeding for BBNF. The W1.γ
   wire-contract fix didn't reach the BBNF emit path.

7. **Five consumer substrates emit `&[]`** in every bench binary (P6):
   `KEYWORD_PHF`, `CLASSIFY_TABLE_*`, `ACTIVE_COLUMNS`, `BRANCH_PRIORS`,
   `DEDUP_ELIGIBLE_RULES`, `LIST_RULES`. Substrate-without-consumer.

8. **sonic-rs achieves 9.7–13.6× our JSON throughput** with 2
   monomorphised symbols (`parse_object::<DocumentVisitor>`,
   `parse_array::<DocumentVisitor>`) covering 81–88% of self-time — zero
   cross-crate calls, SIMD inlined, visitor-monomorphised (P5).

## 2. What every brainstorm agent converged on

One shape recurs across B1–B4: **per-shape monomorphic inline loops
with a compile-time-monomorphised visitor, SIMD kernels spliced inline,
direct materialisation instead of a second tape-walk pass.** That is
sonic-rs's shape; it is replicable atop our existing substrate.

### B1 — the prototype

- New workspace crate `crates/bbnf-json-prototype/`, depends only on
  `bbnf-tape` + `bbnf-simd-scan`.
- Entry: `pub fn parse_json<V: JsonVisitor>(input: &[u8], visitor: &mut V) -> Result<(), ParseError>`.
- Five `#[inline(always)]` shape functions: `parse_value`, `parse_object`,
  `parse_array`, `parse_string`, `parse_number`.
- Zero `dispatch_one`/`try_branch`/`advance_or_pop_with`/`DtaState`/`FrameStack`.
  Recursive descent via the CPU stack.
- SIMD: inline `bbnf-simd-scan::parity::{prefix_xor_64, escape_mask_64}`
  + a new `nospace64` bitmap cache + a republished `first_quote_or_backslash`
  primitive.
- Inline Eisel-Lemire reclaims canada's 10% f64 tax.
- Two visitors ship:
  - `ValueVisitor` — materialises into a `sonic_rs::Value`-shaped enum
    (sonic parity; this is the speed-ceiling validation).
  - `TapeVisitor` — emits into `bbnf_tape::Columns` (AW-IV substrate
    preservation; proves the visitor abstraction covers both).
- Projected: **~1.2 cyc/byte ⇒ ~2900 MB/s on twitter** (within 10% of
  sonic-rs's 1.51 cyc/B measured in P5).
- Modelled on `sonic-rs-0.3.17/src/parser.rs:400–555`.

### B2 — `bbnf-tape` prerequisites

Four surgical changes, all preserving AX's cold-path replay surface:

1. **`bbnf-tape-codegen` subcrate** exposes TokenStream body fragments
   for the 4 residual helpers (`advance_or_pop_with`, `nearest_variant_frame`,
   `write_decoded`, `finalise`). Walker emitter splices inline; cold-path
   `dispatch_one` keeps named fns.
2. **Finaliser folded into per-shape close_compound** emit sites
   (span_hi/child_off/sib_skip known at emit time). Runtime `finalise()`
   retained only for `dta_run_cold`.
3. **`Columns::push_scalar_payload_*`** for F64/U8/Bool/HexU32/I64
   direct-write on the hot path; rayon PSI stays for String + AggregateLarge
   only.
4. **Monomorphic `Visitor` trait in `bbnf-tape`** — single trait; two
   default consumers (`TapeVisitor`, `ValueVisitor`); monomorphised at
   each call site; no dyn dispatch. Second `walk_cursor` pass collapses
   on the `ValueVisitor` path.

### B3 — `bbnf-simd-scan` prerequisites

Substrate is **already complete** in algorithmic content: nibble / wide /
multi-cmp classifiers at `neon.rs:59–407`; CLMUL-or-shift-XOR parity at
`parity.rs:40–204`; tzcnt compaction at `compaction.rs:27–67`;
`StructuralIndex` wire type.

**One addition needed**: `pub mod emit` submodule returning TokenStream
body-fragments (~300 LOC). No kernel rewrites.

Stage-1 IS amortising on CSS (~0.22 cyc/B per AW3-R1 §3). Its savings
aren't collected because `try_branch` dominates — not because stage-1
is wrong. Pluggable per-compound: IR-cardinality gate `prefer_inline_in_loop`
at `recognizers/kernel_shape.rs` chooses stage-1-index vs sonic-style
inline-SIMD-in-loop per compound. Grammar-name-blind.

Projected: **~0.84 cyc/B on JSON twitter** (vs sonic's 1.51 cyc/B
measured). Eliminates 6.18 cyc/B of downstream tape/walker cost.

### B4 — the generalisation

One new IR mining pass `shape_mining.rs` classifies each rule into 7
shape categories (Object, Array, String, Number, Keyword, Pratt,
Unordered). Detectors ground in existing miner outputs
(`delim_scan_configs`, `shape_dict_templates`, `operator_chain_entries`,
`disjoint_first_tables`, `keyword_branches`, `pattern_alphabets`).
No grammar-name branches.

Per-shape emitter modules at `crates/core/src/backend/rust/emitter/shapes/
{object,array,string,number,keyword,pratt,unordered}.rs` emit
`parse_<shape>_<grammar>_<rule><V: ShapeVisitor>` as tight mutually-
recursive inline loops. Dispatch at call sites via compile-time
byte-match over existing `DisjointFirstMiner` tables.

Rules with no shape match fall back to `__dta_walker_inline::run`
per-rule — AW-III/IV substrate + AX replay preserved. Coverage:
**JSON 100%, Sheets 92%, CSS L4 78%, BBNF 75%** by hot-path visit
frequency (≥ 80% on average; no grammar needs a special route).

§6 generalisation invariant preserved: OUTPUT varies per grammar
because IR varies; MECHANISM (8 detectors + 8 emitters + 7 shape
traits) does not.

## 3. Phased execution plan

Five phases; each gated on verification per the operational protocol.

### Phase 1 — Prototype build + bench (1 tranche, ~AW-V scope)

- B2 §1-4 + B3 `pub mod emit` land first as substrate enablers in
  `bbnf-tape-codegen` + `bbnf-simd-scan::emit`. These are prerequisites
  for *both* the prototype and the shape-emitter path; they do not
  affect the current AW-IV hot path.
- Build `crates/bbnf-json-prototype/` per B1's architecture.
- `benches/json_value.rs` isomorphic to the existing
  `crates/core/benches/json/value.rs`.
- **Gate**: each of {data_s, twitter, citm, canada, data_xl} within
  10% of sonic-rs's ns/iter on the twin-pair bench.

Outcome: validates that the substrate supports sonic-rs-class throughput
via per-shape inline emission.

### Phase 2 — Shape-mining IR pass + JSON emitter-lift

- New IR pass `crates/ir/src/passes/recognizers/shape_mining.rs`
  classifies each rule into 7 shape categories.
- Per-shape emitter modules at
  `crates/core/src/backend/rust/emitter/shapes/`.
- Emit JSON using the shape-mining output; verify bench matches the
  hand-written prototype ± 5%.
- Rules without shape match continue to fall through to
  `__dta_walker_inline::run` (no regression on untouched grammars).

### Phase 3 — CSS L4 shape-mining coverage

- Shape-mining covers CSS rules where applicable (78% of hot path):
  Object-shape for declarations, Array-shape for selector lists,
  Keyword-shape for namedColor via PHF, Pratt-shape for calc/min/max
  bodies, Unordered-shape for compound selectors.
- Remaining 22% stays on `__dta_walker_inline::run` — but with
  reduced i-cache pressure because most hot arms moved out of the
  walker symbol.
- **Gate**: CSS bootstrap ≥ 1500 MB/s; sonic-parity-equivalent
  improvement on tailwind / normalize.

### Phase 4 — Sheets shape-mining coverage (92%)

- 6-rung Pratt tower via Pratt-shape emitter.
- Function-name PHF activated for the 150 Sheets functions.
- Sheets small-input amortisation still bounded by fixed setup-floor
  (P3's linear regression analysis); post-shape-mining target parity–1.5×
  post-AU on the three parse entries.

### Phase 5 — BBNF shape-mining coverage (75%)

- Fix the wire-contract pipeline for BBNF's `GRAMMAR_PROFILE`
  emission (P4's regression).
- Directive dispatch via Keyword-shape + PHF.
- **Gate**: BBNF self-host bench ≥ 500 MB/s.

## 4. What this plan relationship to AW-IV-in-flight

The live AW-IV orchestrator is executing W3 / W4 / W5 of the current
plan. This synthesis proposes AW-V (a new tranche) for the prototype +
shape-mining work, rather than reopening AW-IV. Rationale:

- AW-IV's W3 (consumer activations) + W4 (granular SIMD) + W5
  (reduce_column + parity harnesses) are *complementary* to the
  shape-mining path — they close un-inlined helpers + i-cache
  pathology + parity-harness gating that are needed regardless.
- The shape-mining approach is an *additional* codegen path that
  complements the monolithic walker; AW-V's work consumes the AW-IV
  substrate without invalidating it.
- Sequencing AW-V after AW-IV lets each tranche's gate be clean:
  AW-IV closes on the current walker + consumer activations; AW-V
  opens with the prototype + shape-mining.

Alternatively, the orchestrator may fold the B2 §1-4 surgical changes
into AW-IV's remaining waves (they compose cleanly with W3's consumer
activations); the shape-mining + prototype then opens AW-V as
standalone work.

## 5. The single decision

Two options:

**(A) Open AW-V after AW-IV close.** AW-IV runs its planned W3–W6;
AW-V opens with Phase 1 (prototype) as its W1. This sequences
cleanly per the no-deferrals invariant.

**(B) Fold Phase 1 substrate-enablers (B2 §1-4, B3 emit) into AW-IV's
W3.** AW-IV W3's scope already includes "consumer activations"; the
B2/B3 surgical changes are substrate for those consumers. Prototype
build becomes AW-V.W1; shape-mining becomes AW-V.W2+.

Both are protocol-compliant. (B) is slightly faster overall because
the substrate enablers land in parallel with AW-IV's W3 consumer work
rather than sequentially after W6.

Recommend (B). The live orchestrator dispatches W3 agents in parallel
with B2/B3 substrate agents; AW-V opens on a substrate that already
includes the enablers.

## 6. Prototype location + isolation contract

Per user directive, the prototype lives in an isolated worktree:

- Sibling worktree `bbnf-wt-aw5-prototype` (or `bbnf-wt-json-proto`) at
  HEAD `f457b4df` or later.
- Prototype crate `crates/bbnf-json-prototype/` added at that worktree.
- Bench scaffolding mirrors the main repo's `crates/core/benches/json/`
  layout.
- Does NOT modify any existing crate until the prototype passes the
  10%-of-sonic gate.
- Cherry-picks onto master only after gate passes and AW-V phase
  planning locks in the generalisation roadmap.

This is the "proper isolation" the user specified. The live AW-IV
execution orchestrator owns master; the prototype worktree is
read-only WRT master until the AW-V gate is authorised.

---

Indefatigable. DTA viable. Consumer inverted. Prototype first; generalise
second; every shape under the same emitter mechanism.
