# Tranche AX — The RD Reckoning

AX is the reckoning tranche. AW-V lands fn-per-rule recursive descent over shape templates; the shape-emitter W3.2/W4/W5 work generalises the W2.1 prototype across JSON / CSS / Sheets / BBNF. What AW-V does not do: delete the DTA scaffolding, deploy the full novel-lever portfolio, land simdjson-class lazy materialisation, land sonic-rs + lightningcss structural parity harnesses, or amortise cores via document-parallel fork. AX is where those ship. AX also absorbs AW-VI's scope — the fork is no longer a successor tranche, it is an AX wave — and every viable item from `aw5-n1-egraph-rewrite-codegen.md` + `aw5-n2-novel-parsing-approaches.md` + the earlier AW lever catalogue (Levers 3, 5, 6, 7 from AW-V.md §Novel-levers table). Recovery, replay, and incremental re-parse move forward to **AY** (sibling tranche, opens post-AX); AX does NOT carry legacy hooks or shims for them — AY rebuilds those consumers fresh on AX's substrate.

The thesis: **RD + shape templates + tape substrate is the correct architectural primitive**; AW-V proved it on JSON. AX removes every competing primitive still in the codebase, deploys every novelty lever that compounds with RD, and proves structural parity against the two comparators we claim to beat in generality — sonic-rs on JSON, lightningcss on CSS. When AX closes, the codebase has one codegen path, one hot-loop shape, one tape substrate (SoA primary, AoS sidecar per §Value-API below), and CI-gated structural-parity harnesses against both comparators.

## Architectural thesis

AX is organised around five propositions each validated or refuted by a specific wave:

1. **The interpreter is architectural debt. It deletes entirely, not to cold-path.** `dta_walker/` (3,875 LOC), `emitter/dta.rs` (935 LOC), `driver.rs::dispatch_one|try_branch|advance_or_pop_with` (~2,800 LOC), and the DtaTable-as-runtime-const pattern survive in AW-V's "cold-path replay" hedge. AX deletes them. No "cold-path replay substrate" survives into AX — **AY rebuilds replay on RD decision logs from first principles, not on DTA state-id traces**. Net −7,610 LOC of hot-path-capable interpreter, −500 LOC of vestigial cold-path replay, −500 LOC of scaffolding around them.

2. **The tape's access API shapes the ceiling more than the tape's storage layout does.** SoA Columns is fast on pure scans; AoS packed records are fast on single-record reads (visitor hot path, lazy value extraction). The Value API the USER writes visitor-facing code against is the load-bearing surface. AX ships a first-class `Value` API structurally isomorphic to sonic-rs's `sonic_rs::Value` and a CSS equivalent isomorphic to lightningcss's `StyleSheet`; storage is SoA primary with an AoS sidecar populated lazily per access pattern.

3. **Novel levers compound only when they share a substrate.** The AW-V research catalogue (six R-docs + aw5-n1's 12 SEIR rewrites + aw5-n2's 7 novel approaches + the AW-V §Novel-levers table's Levers 1-7) each need hooks in the shape emitter, the tape substrate, and the visitor layer. AX lands every viable lever in a wave gated by its specific measurable outcome; levers that don't move a gate after substrate deployment retire with rationale recorded.

4. **Parallelism is an amortisation multiplier over single-thread exceed, not a single-thread lever.** Document-parallel fork over the stage-1 structural index (the AW-VI item) lands as an AX wave, gated on ≥ 1 MB input size, measured honestly against single-thread exceed numbers from AW-V.

5. **Parity is the generality claim, not a correctness claim.** Structural-parity harnesses assert byte-identical-OR-field-equivalent output against sonic-rs (JSON) and lightningcss (CSS) on the full corpus. They are CI-gated; they are the operational meaning of "beats sonic-rs in generality."

**What AX explicitly does NOT do** (routed forward to AY):
- Incremental re-parse (100 KB CSS edit ≤ 200 µs gate)
- Structural-default recovery + `@recover` grammar-directive audit
- Decision-log replay CLI + test-case minimiser + log-round-trip harness
- Parse-step debugger

AY's thesis: *recovery/replay/incremental are consumer tooling on top of the AX substrate; they deserve their own tranche rather than coupling to the performance reckoning*. AY.md is authored at AX close; no substrate hook is pre-landed inside AX that AY could consume — AY builds its hooks cleanly on the shape emitter's decision points, which is a more direct formulation than retrofitting into AX's hot path.

## Invariants (refined from AW)

1. **One codegen path — no hybrid, no fallback, no `dta_run_cold` "cold-path replay."** Any rule that fails shape classification is a wave-close blocker, not a routing-to-interpreter escape. W0 verifies the invariant mechanically (`grep -r 'dispatch_one\|dta_run' crates/` returns zero hits on compile).

2. **Tape Value API is monomorphised at the user's target type, not at a tagged-union runtime Value.** `Value` as a first-class type exists (§Phase 1) for API-parity with sonic-rs; but a visitor that writes into a user struct takes the monomorphised path, skipping the tape entirely. Both paths land in one emitter; the difference is codegen-time visitor substitution, not runtime dispatch.

3. **All unsafe is concentrated in kernels, not scattered in drivers.** `crates/bbnf-simd-scan/` + `crates/bbnf-tape/columns.rs` + `crates/parse-that/.../simd/` own the `unsafe` blocks; emitter-produced code uses these via `#[inline(always)]` + safe wrappers. No `unsafe` in the per-shape emitter output — the kernels own correctness; the emitter owns composition. `unreachable_unchecked` at proven-dead dispatcher defaults is the one emitter-inserted `unsafe` primitive.

4. **No workload-profile inputs unless grammar-author-declared.** Grammar authors may annotate `@hint(integer_digits: small)` (or equivalent) for profile-derivable decisions per the `aw5-r2` analysis. No runtime workload mining at parse time unless the user opts in via `ParseOptions::tune_online`.

5. **Parity harnesses are binary: pass or fail.** No "within 5% of lightningcss" — either the typed projection is field-equivalent or the harness fails. Divergence is documented per grammar rule with explicit rationale.

6. **Document-parallel fork is opt-in via `ParseOptions::parallel_threshold`** — default single-thread. The bench matrix reports single-thread entries AND parallel entries separately; no conflation.

7. **Wire-contract end-to-end tests per IR-derived emitter output.** Every `pub const` the emitter produces (shape-transition matrix, kind-separated bitmap table, per-rule kernel strategy bitmap, multi-visitor pair registry, PHF keyword tables, LazyRef shape-tag registry, bloom+GADT dedup fingerprints, headword fold tables, alt-reorder frequency tables) carries a wire-contract test asserting the full pipeline: IR mining → pass → const → runtime consumer.

8. **No legacy code, no shims, no forward hooks for AY.** AX ships the substrate AY needs (shape emitter decision points, snapshot-able shape-stack, deterministic bytes-to-tape mapping); AX does NOT pre-wire hooks for AY's consumers. AY authors its hooks freshly when the tranche opens.

9. **Wave discipline carries from AW.** No scope creep mid-wave; scope-reveal triggers re-plan-with-more-agents per README.md. Waves with substrate and consumer as separable work ship them in the same wave or declare the split at plan time.

## Scope

1. **W0 — Interpreter exorcism + AW-V cleanup.** Delete `dta_walker/`, `emitter/dta.rs`, `driver.rs` interpreter machinery, `dta_run_cold`, DtaTable-as-runtime-const consumer. Revert the three AW-V overfit artefacts (Lever 4 `push_compound_fused_v32`, "17-digit NEON lever" projection, cold-path-replay line in AW-V.md §invariants). Rewrite AW-V.md §§invariants + wave-schedule + delete-manifest in RD language.

2. **W1 — First-class Value API + hybrid tape layout.** New typed `Value` per grammar, isomorphic to the grammar's parity comparator. JSON: `bbnf::json::Value` ↔ `sonic_rs::Value`. CSS: `bbnf::css::StyleSheet` ↔ `lightningcss::StyleSheet`. SoA Columns remain primary storage; per-access-pattern AoS sidecar populated lazily (hybrid). `VisitorKind::ValueVisitor<T>` for monomorphised user-struct path; `VisitorKind::TapeVisitor` for SoA Columns substrate; `VisitorKind::LazyVisitor<T>` for Phase 4 gradient.

3. **W2 — SIMD + dispatch lever deployment** (SIMD catalogue + n2.3/n2.4 + aw5-n1 R2/R6 + Levers 5-6). TBL-4 kinded bitmap classifier, `vpaddq_u8` movemask cascade, `vdotq_s32` packadd, PMULL verification, paired-column `stp`, `unreachable_unchecked` sweep, per-pattern alphabet narrowing (`BoundedRegex` lifter + `GrammarProfile.prefer_inline_in_loop`), kind-separated stage-1 position streams (n2.3), SIMD-speculative Alt-branch prefix-match for Unordered shape (n2.4), scan-kernel fusion (aw5-n1 R2 — fuse back-to-back SIMD passes), AltReorder by corpus frequency (aw5-n1 R6), bloom+GADT dedup fingerprint filter on compound-emit, `SHAPE_DICT` ShapeRef consumer activation in tape close_compound.

4. **W3 — Gradient / LazyValue / on-demand materialisation** (aw5-n2.2 + Lever 6 LazyRef). Simdjson-style. Per-compound materialisation budget + `LazyRef` tape kind + shape-cached re-entry via `parse_into::<V>()`. Per-grammar ergonomic wrappers.

5. **W4 — Speculative parsing + shape-transition Markov predictor** (aw5-n2.1). Per-compound predicted-next-shape speculation with rollback scratchpad via shape snapshot. Predictor mined at codegen from corpus samples OR online per `ParseOptions::tune_online`.

6. **W5 — Document-parallel fork over stage-1 structural index** (AW-VI absorbed). The amortisation multiplier. Split at top-level structural delimiters; fork walker per chunk; merge Columns.

7. **W6 — E-graph grammar rewriting** (R6-depart + aw5-n1's cost-aware subset). Two sub-waves: W6α ships the 4 universal rewrites (G1 Alt-flatten, G2 Seq-flatten, G3 KwWs fusion, G4 PHF-dispatch inference); W6β ships the 4 per-shape rewrites (G5 Ref-to-leaf inline with leaf-predicate guardrail, G6 PhfLoop, G7 ClassifyByteLoop, G8 OperatorChain) + HeadwordFolding (aw5-n1 R4: factor common Alt lead-words). Extraction subsumes shape classification; 779 LOC of detectors retire.

8. **W7 — Runtime CPU-capability autotune + PMC-feedback adaptive kernels** (aw5-n2.5 + aw5-n2.6). Five-variant kernel dispatch + `cpu_variant::Auto` runtime selection + PMC counters for online tuning.

9. **W8 — Cranelift JIT per-schema** (aw5-n2.7). Opt-in per-grammar JIT compilation + cache with grammar-AST-hash key.

10. **W9 — Multi-visitor monomorphisation + multi-key SIMD compare** (Lever 7 + Lever 3). `#[derive(Visitor)] #[emit_paired_with(V2)]` macro; emitter monomorphises per declared pair; L1-budget guard. Multi-key SIMD compare `vceqq_u8 × N` / `_mm256_cmpeq_epi8 × N` codegen option for Object-shape emitters when visitor declares known key set.

11. **W10 — Structural-parity harnesses** (sonic-rs + simdjson OnDemand + serde_json for JSON; lightningcss + cssparser for CSS; self-parity for Sheets + BBNF). CI-gated; zero divergence on corpus.

12. **W11 — Subsystem closure ledger** (inherited from old AX). Closure language-feature closure (5 tests), analysis structural-mode (4 tests), gorgeous fixtures (3 tests), pprint-vm hints (2 tests), imports `test_selective_transitive_unfurling` (conditional per AW-V.W5.5 disposition).

13. **W12 — FINAL + bench matrix.** 19-entry single-thread matrix + parallel matrix + parity results. Cold per-parse single-thread exceeds sonic-rs + lightningcss on every relevant entry; parallel entries documented separately with ≥ 2.5× ratios on ≥ 1 MB inputs.

## Wave schedule

Thirteen waves. Each wave is dispatch-gated; the bench-checkpoint contract carries from AW. Wave openings happen only after the prior wave's gate is verified on master, per README.md §wave-verification-ledger.

| Wave | Scope | Agents | Opens after | Hard gate |
|------|-------|--------|-------------|-----------|
| **W0** Exorcism + AW-V cleanup | Delete `dta_walker/` + `emitter/dta.rs` + `driver.rs` interpreter + `dta_run_cold`; revert Lever 4; strike "17-digit"; rewrite AW-V.md | 2 serial (deletion, doc-rewrite) | AW-V.W6 close | `nm` bench binaries for every grammar: zero `dispatch_one|try_branch|advance_or_pop_with|dta_run|DtaTable|DtaState|FrameStack` symbols. `grep` returns zero hits. Cold-parse bench unchanged from AW-V close ± 2%. `cargo test --workspace` green. |
| **W1** Value API + hybrid tape | `bbnf::{json,css,sheets,bbnf}::Value` types + `VisitorKind` variants + AoS sidecar in Columns | 4 parallel (JSON Value, CSS StyleSheet, Sheets/BBNF AST, tape hybrid) | W0 | Each grammar's `Value` is structurally isomorphic to its parity comparator (field-name equivalence OR explicit variance documented per field); hybrid-tape benchmarks show AoS-sidecar access ≥ 1.3× SoA-only access on single-record reads; `ValueVisitor<T>` monomorphised path compiles for `T = sonic_rs::Value` and `T = lightningcss::StyleSheet`. |
| **W2** SIMD + dispatch levers | TBL-4 kinded, `vpaddq_u8`, `vdotq_s32`, PMULL verify, paired `stp`, `unreachable_unchecked`, `BoundedRegex`, kind-separated stage-1 streams, SIMD-speculative Alt prefix, scan fusion, AltReorder, bloom+GADT dedup, ShapeRef consumer | 5 parallel (kernels, emitter sweep, IR passes, dedup, stage-1 streams) | W1 | Every lever verified via `cargo asm` or wire-contract test. CSS normalize ≥ 1500 MB/s; CSS bootstrap record-count drops ≥ 30% from ShapeRef dedup on repetition-heavy workloads; Unordered-shape CSS compoundSelector dispatch ≥ 2× linear-try. JSON twitter unchanged from W2.1. |
| **W3** Gradient / LazyValue | `LazyRef` tape kind + `should_descend` visitor hook + per-grammar `*LazyValue` wrappers | 4 parallel (core, JSON, CSS, Sheets + BBNF) | W2 | Ignore-keys twitter workload 2–3× baseline; `lazy_ref.parse_into::<V>()` O(subtree) per bench; per-grammar fixture coverage. Simdjson OnDemand parity harness ships (W10 pre-wire). |
| **W4** Speculative + predictor | Shape-transition matrix at codegen + `ParseOptions::tune_online` + rollback scratchpad via snapshot | 3 parallel (IR pass, predictor, orchestration) | W3 | CSS bootstrap hot-predict hit-rate ≥ 0.75; twitter rollback ≤ 5%; on-correct speedup ≥ 1.05× vs baseline. Per-grammar transition matrix emitted + wire-contract test. |
| **W5** Document-parallel fork | Split at stage-1 structural delimiters; rayon fork walker; merge Columns | 3 parallel (split detection, fork orchestration, merge) | W4 | 4-core scaling: canada 2.5–4×, citm 2.5–4×, tailwind 2.5–4×, data_xl 3–4× on ≥ 1 MB inputs; small inputs (≤ 1 MB) within noise of single-thread; `ParseOptions::parallel_threshold` default 1 MB. |
| **W6α** E-graph universal rewrites | G1 Alt-flatten, G2 Seq-flatten, G3 KwWs fusion, G4 PHF-dispatch inference | 4 parallel (one per rewrite) | W5 | Bootstrap regen idempotent; CSS L4 e-graph saturation ≤ 1 s per grammar (instrumented via `BBNF_EGRAPH_REPORT=1`); extraction produces KwWs / PhfDispatch nodes where expected. |
| **W6β** E-graph per-shape rewrites + detector retirement | G5 Ref-to-leaf inline (leaf-predicate guardrail), G6 PhfLoop, G7 ClassifyByteLoop, G8 OperatorChain, G9 HeadwordFolding; delete `shape_dispatch/*.rs` detectors (779 LOC → 150 LOC extraction reader) | 5 parallel (one per rewrite + detector retirement) | W6α | Shape tags flow from e-graph extraction, not hand-coded detectors; regression harness asserts every rule's W3.1-classified tag matches the new extraction tag; bootstrap idempotent; no perf regression vs W6α close. |
| **W7** Runtime autotune + PMC-feedback | `ParseOptions::cpu_variant` + five-variant emit; PMC counter path for Apple M; `tune_online` hook | 2 parallel (codegen variants, counter + dispatcher) | W6β | Ice Lake Xeon: citm +10–25% vs baseline; Graviton 3: twitter +5–15%; Apple M4 Max: no regression from W6β baseline. `cpu_variant::Auto` picks correctly at runtime per `is_aarch64_feature_detected!` + cpuid. |
| **W8** Cranelift JIT | Opt-in JIT-compiled per-grammar parser; cache + invalidate policy | 2 parallel (JIT emitter, cache) | W7 | Canada JIT-compiled 1.10–1.25× AOT-compiled; JIT compile ≤ 5 ms per grammar; SHA-based cache keyed on grammar AST hash. `ParseOptions::jit = true` default off. |
| **W9** Multi-visitor pairs + multi-key SIMD | `#[emit_paired_with]` macro + per-grammar example visitors + multi-key SIMD compare codegen option for Object-shape | 3 parallel (derive macro, multi-key codegen, examples) | W8 | Macro compiles two pair combinations per primary grammar; over-L1 rejected with diagnostic; JSON serde-compat example matches sonic-rs; CSS lightningcss-compat example within 10% of lightningcss; multi-key SIMD Object-shape ≥ 1.1× linear-compare on twitter (≥ 16 known-keys workload). |
| **W10** Parity harnesses | sonic-rs + simdjson-OnDemand + serde_json (JSON); lightningcss + cssparser (CSS); CI-gated; field-equivalence suite | 3 parallel (JSON parity, CSS parity, Sheets + BBNF self-parity) | W9 | Zero divergence on corpus; per-grammar field-equivalence suite ≥ 200 cases; CI blocks PR on harness failure. |
| **W11** Subsystem closures | 5 closure tests, 4 analysis structural-mode, 3 gorgeous, 2 pprint-vm, 1 imports (conditional) | 3 parallel (per domain) | W10 | All ignored tests un-ignore OR delete with explicit rationale; ignored count at AX close = 0. |
| **W12** FINAL + bench matrix | `FINAL.md`, `post-AX.json`, aggregator, summary | 1 serial | W11 | 19-entry single-thread matrix + parallel matrix + parity results; every parse entry ≥ post-AU single-thread; document-parallel entries ≥ 2.5× on ≥ 1 MB inputs; no `#[ignore]` additions in AX. AY.md authored in the same wave. |

## Phases

### Phase 0 — Exorcism + AW-V cleanup (W0)

Two agents, serial.

#### AX.0.1 — Interpreter deletion

Owner: `crates/bbnf-tape/src/driver.rs`; `crates/bbnf-tape/src/dta.rs`; `crates/core/src/backend/rust/emitter/dta_walker/`; `crates/core/src/backend/rust/emitter/dta.rs`; `crates/ir/src/passes/recognizers/dta.rs` (partial — keep the lifter's SHAPE_DICT + GRAMMAR_PROFILE mining; delete only the `DtaState`-emission path).

Delete manifest (committed per milestone — one commit per file):

| File | LOC | Action |
|---|---:|---|
| `crates/bbnf-tape/src/driver.rs` | 3,323 | Retain `emit_leaf` / `push_compound_fused` / `trim_with_pattern` (~500 LOC, used by shape emitter output). Delete the remaining ~2,800 LOC (`dispatch_one`, `try_branch`, `advance_or_pop_with`, `handle_repeat_failure`, `nearest_variant_frame`, frame stack interpreter machinery). |
| `crates/bbnf-tape/src/dta.rs` | 550 | Delete (`DtaState`, `DtaTable`, `DtaStateId` — not consumed outside deleted driver). |
| `crates/core/src/backend/rust/emitter/dta_walker/` | 3,875 | Delete entirely (nine files). |
| `crates/core/src/backend/rust/emitter/dta.rs` | 935 | Delete. |
| `crates/ir/src/passes/recognizers/dta.rs` | ~1,513 | Retain SHAPE_DICT + GRAMMAR_PROFILE mining (~700 LOC). Delete DtaState-lifting path (~813 LOC). |
| **Total** | **~7,923** | **Deleted** |

Replace consumers of deleted symbols:
- `emit_grammar_impl::route_uncovered_rules_to_dta_run` → delete, compile error on uncovered rules — this forces W4 closure. If a rule is uncovered, AW-V has not closed, not AX.
- Every remaining `use bbnf_tape::DtaState` / `bbnf_tape::dispatch_one` — delete.

#### AX.0.2 — AW-V doc + lever cleanup

Owner: `docs/tranches/AW/AW-V.md`; `crates/bbnf-tape/src/columns.rs`; `docs/tranches/AW/PROGRESS.md`.

Three actions:

1. **Revert `push_compound_fused_v32` (Lever 4)**, commit `1cf69a69`. Replace with `push_span_pair_stp` per R4 §6 Design A (aarch64 `stp` over `span_lo`/`span_hi`, x86 AVX `_mm_storeu_si128` pair).
2. **Strike "17-digit NEON lever"** from `AW-V.md:684,692` projected-performance table. Replace with `vdotq_s32` canada fraction packadd (R4 §3) as the canada-specific entry.
3. **Rewrite AW-V.md §Invariants.3 + §Wave-schedule W5/W6 + §Delete-manifest + §Successor-chain** in RD language per `SYNTHESIS-5-AW-V-RECKONING.md` §4. Retire "compile DTA into hot-path code" branding. The thesis becomes *"fn-per-rule over shape templates; DTA-era facts feed the shape emitter; the scaffold comes down as the consumer emerges."* Successor chain: AW-V → AX (RD reckoning) → AY (replay/recovery/incremental). No AW-VI.

**Hard gate** (W0): `nm target/release/deps/{json_monolithic,css_l4,google_sheets_monolithic,bbnf_monolithic}-*` shows zero symbols matching `dispatch_one|try_branch|advance_or_pop_with|dta_run|DtaTable|DtaState|FrameStack`. `grep -rE 'dispatch_one|DtaState|dta_run' crates/ src/` returns zero hits on Rust files (docs/comments excluded). `cargo test --workspace` green. Cold-parse bench regression < 2% vs AW-V close.

### Phase 1 — First-class Value API + hybrid tape (W1)

Four agents, parallel.

#### AX.1.1 — `bbnf::json::Value` (sonic-rs-isomorphic)

Owner: `crates/core/src/backend/rust/view/json/value.rs` (new).

Structurally isomorphic to `sonic_rs::Value`:

```rust
pub enum Value<'input> {
    Null,
    Bool(bool),
    Number(Number),                     // same shape as sonic_rs::Number
    String(Cow<'input, str>),           // borrowed when escape-free, arena-owned otherwise
    Array(Vec<Value<'input>>),
    Object(IndexMap<Cow<'input, str>, Value<'input>>),
}

pub struct Number {
    // identical f64 + preserved integer-precision split as sonic-rs
}
```

W10 parity harness asserts `bbnf::json::Value::from(json_bytes) ≈ sonic_rs::Value::from(json_bytes)` for every corpus input.

The packed `bbnf_json_prototype::Value` (from W2.1, 24-byte variant-packed) survives as the internal tape-Value representation; `bbnf::json::Value` is the user-facing API. Conversion is zero-copy for borrow-safe strings, arena-backed otherwise.

#### AX.1.2 — `bbnf::css::StyleSheet` (lightningcss-isomorphic)

Owner: `crates/core/src/backend/rust/view/css/stylesheet.rs` + `crates/core/src/backend/rust/view/css/{rules,declarations,values,selectors}/mod.rs` (new).

Mirrors lightningcss's `StyleSheet<'i>` → `CssRule` → `{StyleRule, MediaRule, SupportsRule, …}` tree. The canonical reference is the upstream `lightningcss` crate (dev-dep); AX.1.2 agent reads the lightningcss upstream source for every CSS AST variant and produces an equivalent BBNF typed projection.

Per `docs/tranches/AW/research/aw5-h1-shape-taxonomy-audit.md`, CSS L4 has ~165 rules across 11 shapes; the StyleSheet has ~50-80 distinct AST variants in the lightningcss upstream. Every variant maps to a grammar rule + shape; every variant has an isomorphic BBNF typed projection.

**The parity harness (W10) enforces the isomorphism.** Divergence per-variant is a wave-close blocker. Acceptable divergence (documented per-field with rationale): vendor-prefix handling variance, custom-property representation, comment preservation (we do not preserve comments; lightningcss optionally does — documented divergence).

#### AX.1.3 — `bbnf::sheets::Ast` + `bbnf::bbnf::Ast`

Owner: `crates/core/src/backend/rust/view/sheets/ast.rs` + `crates/core/src/backend/rust/view/bbnf/ast.rs` (new).

Sheets and BBNF have no standard external comparator. The "parity" there is self-parity (round-trip correctness): `Ast::from(parse(serialize(ast))) == ast`. Sheets AST: `Expr::{Number, String, CellRef, RangeRef, FnCall, BinOp, UnaryOp}`. BBNF AST: `Rule::{Seq, Alt, Repeat, Ref, Regex, Literal, Kw, Ws, Mark, Map, Host}`.

#### AX.1.4 — Hybrid tape layout

Owner: `crates/bbnf-tape/src/columns.rs`; `crates/bbnf-tape/src/packed.rs` (new).

Per R4 §5, the SoA Columns primary + AoS sidecar hybrid:

```rust
pub struct Columns {
    // SoA primary — hot on scans, writes
    pub rule_kind:    Vec<u16>,
    pub tape_kind:    Vec<u8>,
    pub span_lo:      Vec<u32>,
    pub span_hi:      Vec<u32>,
    pub child_off:    Vec<u32>,
    pub variant_idx:  Vec<u16>,
    pub sib_skip:     Vec<u16>,

    // AoS sidecar — populated lazily on first random-access read
    packed_cache: OnceCell<Vec<PackedRecord>>,  // 32 B × len
}

#[repr(C, align(32))]
pub struct PackedRecord {
    pub rule_kind:   u16,
    pub tape_kind:   u8,
    pub variant_idx: u16,
    pub _pad0:       [u8; 3],
    pub span_lo:     u32,
    pub span_hi:     u32,
    pub child_off:   u32,
    pub sib_skip:    u16,
    pub _pad1:       [u8; 10],
}
```

- **Sequential SoA scans**: walk columns natively.
- **Random AoS reads**: first access populates `packed_cache` via column gather; subsequent reads are single 32-byte loads at cache-line alignment.
- **Writes** always go to SoA primary; any prior `packed_cache` is invalidated (`OnceCell::take()` + re-populate on next random read).

Paired-column `stp` (R4 §6 Design A) on aarch64 writes `span_lo[i]` + `span_hi[i]` in one `stp w0, w1, [x]` instruction. Replaces the self-aliased `push_compound_fused_v32` (W0 reverted).

**Hard gate** (W1): `bbnf::json::Value::from(twitter.json)` structurally equals `sonic_rs::Value::from(twitter.json)` per `PartialEq` deep-compare; `bbnf::css::StyleSheet::from(bootstrap.css)` structurally equals `lightningcss::StyleSheet::from(bootstrap.css)` per field-by-field comparison (~200 cases in fixture); hybrid tape's `packed_cache` access is ≥ 1.3× SoA-column access on single-record reads (microbench); `ValueVisitor<T>` monomorphised for `T = sonic_rs::Value` + `T = lightningcss::StyleSheet`.

### Phase 2 — SIMD + dispatch levers (W2)

Five agents, parallel. This wave absorbs every SIMD lever the research catalogue surfaced plus the dispatch-shape improvements from aw5-n1.

#### AX.2.1 — Novel SIMD kernels

Owner: `crates/bbnf-simd-scan/src/{neon,avx2,parity,compaction,str2int}.rs`; `crates/bbnf-simd-scan/src/emit.rs` (extensions).

Per R4 §3 + §6:

1. **TBL-4 kinded bitmap classifier** (`vqtbl4q_u8` on aarch64, `_mm512_permutexvar_epi8` on AVX-512, two-`_mm256_shuffle_epi8` fallback on AVX2 baseline). Four-register 64-entry LUT produces per-byte kind code in one SIMD operation. Emitted inline via `bbnf-simd-scan::emit::tbl4_kinded_scan`.
2. **`vpaddq_u8` movemask cascade** ported from `sonic-simd-0.1.4/src/neon.rs:151-165`. 3 cyc critical path replaces 24 cyc `vaddv_u8` path.
3. **`vdotq_s32` canada fraction packadd**. 4-lane accumulator with `[1, 10, 100, 1000]` pattern.
4. **PMULL gate verification**. Shift-XOR becomes dead code on M-series; retain only as x86 fallback at `cfg(target_arch = "x86_64")`.
5. **Paired-column `stp`** (`bbnf-tape::columns::push_span_pair_stp`) replaces Lever 4.

#### AX.2.2 — Emitter `unsafe` sweep + scan-kernel fusion + AltReorder

Owner: `crates/core/src/backend/rust/emitter/shapes/*.rs`; `crates/core/src/backend/rust/emitter/dispatcher.rs`.

Three sub-items:

1. **`unreachable_unchecked` at proven-dead dispatcher defaults**. Every shape emitter whose `DisjointFirstMiner` output shows exhaustive FIRST-set emits `Some(_) => unsafe { core::hint::unreachable_unchecked() }` at the dispatcher default. LLVM eliminates the branch.
2. **Scan-kernel fusion** (aw5-n1 R2): back-to-back SIMD passes (`skip_space` followed by `first_quote_or_backslash` at the same position) fuse into one stripe-level pass producing both bitmaps. Eliminates duplicate byte loads. Applied at per-shape emit sites where the emitter detects adjacent scan calls over the same input range.
3. **AltReorder by corpus frequency** (aw5-n1 R6): Alt dispatchers emit branches in frequency-descending order from grammar-corpus statistics (mined at codegen). CPU BTB and branch predictor favour the high-frequency arm; mispredict rate drops measurably on skewed workloads (JSON `value`'s string-majority dispatch, CSS `atRule`'s `@media`-majority dispatch).

#### AX.2.3 — Kind-separated stage-1 position streams (aw5-n2.3)

Owner: `crates/bbnf-simd-scan/src/kind_streams.rs` (new); `crates/ir/src/passes/recognizers/stage1_kinds.rs` (new).

Refines Mison's per-kind query bitmaps into per-kind position streams. Stage-1 produces N separate `Vec<u32>` per-kind position streams instead of one unified structural index with kind annotations. Per-shape walker consumes only the streams its rule needs — saves ~4 cycles per per-shape dispatch per aw5-n2.3 §measurability.

Wire-contract: per-grammar `STAGE1_KIND_STREAMS` const registry names which kinds emit separate streams; wire test asserts emitted code consults the declared streams only.

#### AX.2.4 — SIMD-speculative Alt-branch prefix-match (aw5-n2.4)

Owner: `crates/core/src/backend/rust/emitter/shapes/unordered.rs` (new); `crates/bbnf-simd-scan/src/multi_compare.rs` (new kernel).

Natural implementation of Unordered-shape for CSS `compoundSelector` (5-way independent Alt branches). Rather than linear-try per branch, parallel-compare all N branch prefixes in one SIMD operation:

- aarch64: N parallel `vceqq_u8` ops + OR-reduce → one movemask identifying which branches matched.
- x86_64: N parallel `_mm256_cmpeq_epi8` ops + OR-reduce.

On match, select first set bit via `tzcnt` (or NEON equivalent) for branch index. Eliminates N-ary linear try on hot Unordered dispatches.

#### AX.2.5 — `BoundedRegex` IR lifter + per-rule kernel strategy

Owner: `crates/ir/src/passes/recognizers/pattern_alphabet.rs` (extend); `crates/ir/src/passes/recognizers/kernel_strategy.rs` (new); `crates/bbnf-tape/src/profile.rs` (extend `GrammarProfile`); `crates/core/src/backend/rust/emitter/shapes/{string,number,scalar,hregex}.rs`.

Per R1 §5.1:

1. Extend `pattern_alphabet.rs` to emit per-regex-pattern `last_byte_set: [u64; 4]` + `first_byte_set: [u64; 4]` (256-bit bitmap, 32 bytes each).
2. New IR pass `kernel_strategy.rs` computes per-rule `prefer_inline_in_loop: bool` via R1 §4 criterion.
3. `GrammarProfile.prefer_inline_in_loop: &'static [RuleId]` bitmap-compressed; `GrammarProfile.pattern_alphabets: &'static [PatternAlphabet]` slice.
4. Shape emitters consult the profile at codegen; `rule_id ∈ prefer_inline_in_loop`: splice inline SIMD fragments. `rule_id ∉ prefer_inline_in_loop` AND has `BoundedRegex` witness: consume grammar-wide structural index via `ConsumeToNextStructural` inline jump.

#### AX.2.6 — Bloom+GADT dedup + ShapeRef consumer activation

Owner: `crates/bbnf-tape/src/dedup.rs` (extend); `crates/core/src/backend/rust/emitter/shapes/*.rs` (compound-emit call sites); `crates/bbnf-tape/src/shape_dict.rs`.

Per the AW-V Lever 6 catalogue (`SHAPE_DICT.lookup(shape_hash)`) and the earlier AW-III bloom+GADT substrate:

1. **Bloom filter at compound-emit**: before `SHAPE_DICT.lookup(shape_hash)` (~8-cycle hash + probe), check a bloom filter (`BLOOM_SIGNATURES: &'static [u64; 4]` per grammar) for the fingerprint. Filter-miss → skip lookup (common case on non-repetition workloads); filter-hit → proceed with exact lookup. Measured on CSS bootstrap (43× `ws:ws` compound, 42× `!important`): filter-hit rate matches exact-dedup rate within 1%.
2. **ShapeRef consumer activation**: the W3.1 substrate exposes `SHAPE_REF_DICT_IDX` per mineable Seq arm; `close_compound` currently falls through to `push_compound_fused` (per AW-V.W3.1 PROGRESS ledger noting "out of W3.1 file bounds"). AX.2.6 lands the promotion path: when the compound's shape matches an interned entry, `close_compound` emits `TapeKind::ShapeRef { dict_idx }` instead of a fresh compound record. Bootstrap record count drops ≥ 30% on CSS (measured pre-AW-IV).

#### AX.2.7 — Per-rule bitmap wire-contract test

Owner: `crates/core/tests/kernel_strategy_wire.rs` (new); `crates/core/tests/bloom_dedup_wire.rs` (new); `crates/core/tests/kind_streams_wire.rs` (new); `crates/core/tests/alt_reorder_wire.rs` (new).

Per wire-contract invariant: fixture grammar + assertion chain from IR mining → pass → `GrammarProfile.*` literal → emitter consults literal → generated parser exhibits expected shape via `cargo expand` inspection.

**Hard gate** (W2): all seven levers verified via `cargo asm` or wire-contract test (paired `stp`; PMULL; TBL-4; `vpaddq_u8`; `vdotq_s32`; `BoundedRegex` lifter active; `unreachable_unchecked` at dispatchers; bloom+GADT filter populated; ShapeRef consumer activated; kind-separated streams emitted; SIMD-speculative Alt-branch lands for Unordered). CSS normalize ≥ 1500 MB/s; CSS bootstrap record-count drops ≥ 30%; Unordered-shape CSS compoundSelector ≥ 2× linear-try; JSON twitter unchanged from W2.1.

### Phase 3 — Gradient / LazyValue (W3)

Four agents, parallel.

#### AX.3.1 — `LazyRef` tape kind + visitor budget hook

Owner: `crates/bbnf-tape/src/tape.rs` (extend `TapeKind` with `LazyRef`); `crates/bbnf-tape/src/visitor.rs` (extend `GrammarVisitor` with `should_descend`); `crates/core/src/backend/rust/emitter/shapes/*.rs`.

```rust
pub enum TapeKind {
    // existing variants ...
    LazyRef { input_start: u32, input_end: u32, shape_tag: ShapeTag },
}

pub trait GrammarVisitor {
    fn should_descend(&mut self, path: &VisitorPath) -> bool { true }
}
```

At compound open:
```rust
if !visitor.should_descend(&path) {
    let close_pos = seek_matching_close(input, pos, shape_tag);
    visitor.lazy_compound(input_start, close_pos, shape_tag);
    pos = close_pos;
    return Ok(());
}
```

`seek_matching_close` uses the stage-1 kind-separated streams when available OR a per-shape bracket-balance scan (`prefix_xor_64` over `{`/`}` positions on JSON).

#### AX.3.2 – AX.3.5 — Per-grammar LazyValue wrappers

Owner: `crates/core/src/backend/rust/view/{json,css,sheets,bbnf}/lazy.rs` (new).

```rust
pub struct JsonLazyValue<'input> { input: &'input [u8], start: u32, end: u32, shape_tag: ShapeTag }
impl<'input> JsonLazyValue<'input> {
    pub fn parse_into<V: JsonVisitor>(&self, visitor: &mut V) -> Result<(), ParseError>;
    pub fn as_raw_bytes(&self) -> &'input [u8];
    pub fn shape_tag(&self) -> ShapeTag;
}
```

Equivalents for CSS (`CssLazyDeclaration`, `CssLazyRule`), Sheets (`SheetsLazyFormula`), BBNF (`BbnfLazyRule`). Simdjson OnDemand analog, grammar-agnostic via shape-tag re-entry.

**Hard gate** (W3): `LazyRef` emitted when `should_descend` returns false; `parse_into::<V>()` O(subtree) per bench; per-grammar `*LazyValue` wrappers ship. Twitter-ignore-keys ≥ 2× vs full-materialisation.

### Phase 4 — Speculative parsing + shape-transition Markov predictor (W4)

Three agents, parallel. (aw5-n2.1.)

#### AX.4.1 — Shape-transition matrix at codegen

Owner: `crates/ir/src/passes/recognizers/shape_transitions.rs` (new).

Per grammar, mine shape-to-shape transition frequency from `grammar/<grammar>/corpus/*` samples. Emit `pub const SHAPE_TRANSITIONS: &[[f32; 11]; 11]` per grammar into `generated.rs`.

#### AX.4.2 — Online predictor (opt-in)

Owner: `crates/bbnf-tape/src/predictor.rs` (new).

```rust
pub struct ShapePredictor {
    base_matrix: &'static [[f32; 11]; 11],
    online_observations: Option<[[u32; 11]; 11]>,  // Some if tune_online
    recent_shape: ShapeTag,
}
```

On compound close, update `recent_shape`. On compound open, consult `base_matrix[recent_shape]`. If `tune_online`, blend with online observations (EWMA α = 0.01).

#### AX.4.3 — Rollback scratchpad + speculation orchestration

Owner: `crates/bbnf-tape/src/speculative.rs` (new).

Per-compound speculation:
1. Predictor returns top-1 next-shape with confidence.
2. If confidence > 0.6, open speculative `Columns` scratchpad + save `ShapeSnapshot`.
3. Speculatively parse predicted shape's open pattern.
4. When actual next-shape resolves, match → commit; mismatch → `Columns::rewind(snapshot.columns_len)` + re-parse.

**Hard gate** (W4): CSS bootstrap hot-predict hit-rate ≥ 0.75; twitter rollback ≤ 5%; on-correct speedup ≥ 1.05× vs W3 baseline.

### Phase 5 — Document-parallel fork (W5)

Three agents, parallel. Folds AW-VI into AX.

#### AX.5.1 — Split-point detection

Owner: `crates/bbnf-tape/src/fork.rs` (new); `crates/ir/src/passes/recognizers/fork_boundaries.rs` (new).

Per grammar, "safe fork boundaries" mined from grammar structure:
- **JSON**: top-level array commas (root is array); comma-separated top-level object fields on demand.
- **CSS**: top-level ruleset boundaries.
- **Sheets**: formula-line newlines.
- **BBNF**: rule-end semicolons at top level.

#### AX.5.2 — Fork orchestration

Owner: `crates/bbnf-tape/src/fork.rs` (extend); rayon-backed parallel walker.

#### AX.5.3 — Merge

Owner: `crates/bbnf-tape/src/fork.rs` (extend).

Offset-shift each chunk's `span_lo`/`span_hi`/`child_off`/`sib_skip` by chunk base; concatenate columns; reconstruct top-level list.

**Hard gate** (W5): 4-core Apple M4 Max: canada 2.5–4×, citm 2.5–4×, tailwind 2.5–4×, data_xl 3–4× on ≥ 1 MB; ≤ 1 MB inputs ≤ 2% regression from single-thread; `ParseOptions::parallel_threshold` default 1 MB.

### Phase 6 — E-graph grammar rewriting (W6α + W6β)

Per `docs/tranches/AW/research/aw5-r6-depart-egraph-compile.md`. Retires the shape_dispatch detectors in favor of extraction predicates over a saturated e-graph. W6β adds the aw5-n1 R4 HeadwordFolding rewrite (G9).

#### W6α — Universal rewrites (4 parallel)

Owner: `crates/ir/src/egraph/rules/grammar_{alt_flatten,seq_flatten,kwws_fusion,phf_dispatch}.rs` (new).

G1 Alt flatten, G2 Seq flatten, G3 KwWs fusion, G4 PHF-dispatch inference — all grammar-universal, strictly node-count-decreasing, ~50 LOC + scheduler registration + cost-model extension each.

`GrammarENode` variants added: `KwWs(StringId)`, `PhfDispatch(Box<[StringId]>)`.

**Hard gate** (W6α): bootstrap regen idempotent; CSS L4 e-graph saturation ≤ 1 s per grammar; extraction produces `KwWs` nodes where authored grammar has `"literal" ?w`; extraction produces `PhfDispatch` nodes where Alt has ≥ 3 bounded-width literal branches.

#### W6β — Per-shape rewrites + HeadwordFolding + detector retirement (5 parallel)

Owner: `crates/ir/src/egraph/rules/grammar_{ref_to_leaf_inline,phf_loop,classify_byte_loop,operator_chain,headword_fold}.rs` (new).

G5 Ref-to-leaf inline (leaf-predicate guardrail), G6 PhfLoop, G7 ClassifyByteLoop, G8 OperatorChain, **G9 HeadwordFolding** (per aw5-n1 R4: detect N Alt branches sharing common lead-literal, factor the lead-literal to a single scan followed by N-1 tail-dispatchers).

G9 fires on CSS `atRule` (`@media` / `@supports` / `@font-face` all start with `@`), on Sheets function names (common `=A1*` leading pattern). Cost delta: `N × (literal_scan + tail_cost)` → `literal_scan + N × tail_cost`.

**Detector retirement**: after W6β rules land, extraction produces canonical-shape nodes. The 779 LOC of bespoke shape-detectors retire; replaced with ~150 LOC tag-reading in `shape_dispatch/mod.rs::classify_shape(egraph, root) → ShapeTag`.

**Hard gate** (W6β): shape tags flow from e-graph extraction; regression harness asserts every rule's W3.1-era tag matches the new extraction tag; bootstrap idempotent; `shape_dispatch/{object,array,string,…}.rs` detector files deleted; no perf regression from W6α baseline.

### Phase 7 — Runtime autotune + PMC-feedback (W7)

Two agents, parallel. (aw5-n2.5 + aw5-n2.6.)

#### AX.7.1 — Five-variant codegen + `cpu_variant` option

Owner: `crates/core/src/backend/rust/emitter/cpu_variants.rs` (new); `crates/bbnf-tape/src/parse_options.rs` (extend).

Five variants per entry: `Baseline`, `NeonPmull`, `Avx2`, `Avx512Vbmi2`, `Sme` (deferred until stable Rust SME intrinsics). `cpu_variant::Auto` resolves at runtime via `is_aarch64_feature_detected!` / `is_x86_feature_detected!`. Binary-size cost bounded: kernel-bearing fns only; ~1.5–2× per grammar per N2.5 §measurability.

#### AX.7.2 — PMC-feedback adaptive kernels

Owner: `crates/bbnf-tape/src/pmc.rs` (new; aarch64-apple-darwin + linux-perf paths).

Apple M: `mach_thread_policy` + `PL_HW_PMC_*` for counter sampling. Linux: `perf_event_open`. First N parses (N=3 default), sample IPC + branch-miss-rate + L1-miss-rate; select best-IPC variant for subsequent parses. Gate: `ParseOptions::tune_online = true` (default false).

**Hard gate** (W7): Ice Lake Xeon citm ≥ +10% with `Avx512Vbmi2`; Graviton 3 twitter ≥ +5% with `NeonPmull`; Apple M4 Max no regression from W6β baseline; `cpu_variant::Auto` correct selection.

### Phase 8 — Cranelift JIT per-schema (W8)

Two agents, parallel. (aw5-n2.7.)

#### AX.8.1 — JIT emitter

Owner: `crates/bbnf-jit/` (new workspace member); feature flag + `ParseOptions::jit = true`.

Cranelift IR emission from the same shape-emitter substrate that drives AOT. Per-grammar JIT at first-parse (~5 ms); subsequent parses use JIT'd function pointer. Specialisations: workload-profile-informed (after N=100 parses with PMC feedback, re-JIT with branch-bias hints); input-shape-specialised (twitter's top-level-array → array-specialised root).

#### AX.8.2 — Cache + invalidate policy

Owner: `crates/bbnf-jit/src/cache.rs`.

SHA cache key on grammar AST hash + rustc version + Cranelift version + kernel revision. Grammar change → invalidate. Cached artefacts at `target/.bbnf-jit-cache/<hash>.bin`; loaded via `memmap2`.

**Hard gate** (W8): canada JIT-compiled 1.10–1.25× AOT-compiled; JIT compile ≤ 5 ms per grammar; cache cold-start < 1 µs per grammar; `jit = true` default off.

### Phase 9 — Multi-visitor pairs + multi-key SIMD compare (W9)

Three agents, parallel. Lever 7 + Lever 3 from AW-V §Novel-levers table.

#### AX.9.1 — `#[derive(Visitor)]` macro with `#[emit_paired_with]` attribute

Owner: `crates/derive/src/visitor.rs` (new derive macro).

```rust
#[derive(Visitor)]
#[emit_paired_with(SchemaValidator)]
struct AppVisitor { ... }
```

Emitter monomorphises shape emitters for the `(AppVisitor, SchemaValidator)` pair. L1-budget guard per `aw5-h2-visitor-monomorphisation.md`:
```
error: visitor pair (AppVisitor, SchemaValidator, CustomLogger) exceeds L1 i-cache budget for grammar 'css_l4':
         projected 647 KB, budget 128 KB.
       reduce visitor pair size or remove the third visitor.
```

#### AX.9.2 — Multi-key SIMD compare for Object-shape (Lever 3)

Owner: `crates/core/src/backend/rust/emitter/shapes/object.rs` (extend); `crates/bbnf-simd-scan/src/multi_compare.rs` (reuse W2.4 kernel).

When a visitor's `#[derive(Visitor)]` declares known keys (`#[keys("name", "id", "version", ...)]` attribute), emitter emits SIMD-parallel key-compare at object dispatch:

- aarch64: `vceqq_u8` over up to 16 packed 16-byte key prefixes per stripe + OR-reduce + movemask + `ctz` → matched-key index.
- x86_64: `_mm256_cmpeq_epi8` over 32 packed keys per stripe.

`vpmovmskb + tzcnt` selects first match; direct visitor-method dispatch via jump table keyed on matched index. N-key dispatch becomes one SIMD compare; ~16× fewer instructions vs linear compare.

Gate: twitter with ≥ 16 declared keys workload ≥ 1.1× vs linear-compare equivalent; ≤ 8 keys: no regression (the SIMD cost floor matches the linear floor).

#### AX.9.3 — Per-grammar examples

Owner: `crates/core/examples/{json_serde_visitor,css_lightningcss_visitor,bbnf_ast_visitor,sheets_formula_visitor}.rs` (new).

Each example declares a visitor targeting an output type (serde::Value for JSON, lightningcss::StyleSheet for CSS) and ships as an example + bench-pair entry.

**Hard gate** (W9): macro compiles ≥ 2 pair combinations per primary grammar; over-L1 rejected with diagnostic; JSON serde-compat matches sonic-rs; CSS lightningcss-compat within 10% of lightningcss; multi-key SIMD Object-shape ≥ 1.1× linear-compare on ≥ 16-keys workload.

### Phase 10 — Structural-parity harnesses (W10)

Three agents, parallel. The generality claim made operational.

#### AX.10.1 — JSON parity (sonic-rs + simdjson OnDemand + serde_json)

Owner: `crates/core/tests/parity/json.rs` (new); CI integration.

Three comparators:
- **sonic-rs**: deep PartialEq on `bbnf::json::Value` vs `sonic_rs::Value`.
- **simdjson OnDemand**: `JsonLazyValue::parse_into::<SchemaV>()` matches simdjson's OnDemand (via `simd-json` crate bindings).
- **serde_json**: round-trip through `serde_json::Value`.

Fixture count: ≥ 200 per comparator.

#### AX.10.2 — CSS parity (lightningcss + cssparser)

Owner: `crates/core/tests/parity/css.rs` (new).

Two comparators:
- **lightningcss**: field-equivalent StyleSheet per per-variant equivalence. Divergences documented per field.
- **cssparser**: token-level parity.

Fixture count: ≥ 200.

#### AX.10.3 — Sheets + BBNF self-parity

Owner: `crates/core/tests/parity/{sheets,bbnf}.rs` (new).

Self-parity: `Ast::from(serialize(Ast::from(parse(input)))) == Ast::from(parse(input))` for every corpus input.

**Hard gate** (W10): zero divergence on every corpus input per every comparator; CI blocks PR on failure; per-grammar divergence documentation ships in `docs/tranches/AX/parity/`.

### Phase 11 — Subsystem closures (W11)

Three agents, parallel. Inherited from old AX.

1. **Closure language-feature closure** (5 tests).
2. **Analysis structural-mode** (4 tests).
3. **Gorgeous fixtures + pprint-vm hints** (5 tests).
4. **Imports** (`test_selective_transitive_unfurling`, conditional per AW-V.W5.5).

**Hard gate** (W11): all ignored tests un-ignore OR delete with explicit rationale; ignored count at AX close = 0.

### Phase 12 — FINAL + bench matrix + AY authoring (W12)

One serial agent.

`docs/tranches/AX/FINAL.md` + `docs/benchmarks/post-AX.json` + **`docs/tranches/AY/AY.md` first draft** (replay/recovery/incremental tranche plan — see `docs/tranches/AY/AY.md` skeleton authored at AX open for reference).

19-entry single-thread matrix + parallel matrix + parity results.

**Hard gate** (W12):
- Every single-thread parse entry ≥ post-AU.
- Parallel entries ≥ 2.5× single-thread on ≥ 1 MB inputs.
- Zero parity divergence.
- Zero `#[ignore]` additions in AX.
- Zero DTA symbols (`nm` + `grep` verification).
- AY.md exists with its own invariants + wave-schedule + thesis.

## Critical files

| File | Phase |
|------|-------|
| `crates/bbnf-tape/src/driver.rs` (interpreter excision) | 0 |
| `crates/bbnf-tape/src/dta.rs` (**delete**) | 0 |
| `crates/core/src/backend/rust/emitter/dta_walker/` (**delete entirely**) | 0 |
| `crates/core/src/backend/rust/emitter/dta.rs` (**delete**) | 0 |
| `crates/ir/src/passes/recognizers/dta.rs` (partial — keep mining, delete lifter) | 0 |
| `docs/tranches/AW/AW-V.md` (§invariants + §wave-schedule + §delete-manifest rewrite) | 0 |
| `crates/bbnf-tape/src/columns.rs` (Lever 4 revert; paired `stp`; AoS sidecar) | 0, 1, 2 |
| `crates/core/src/backend/rust/view/json/value.rs` (**new**) | 1 |
| `crates/core/src/backend/rust/view/css/{stylesheet,rules,declarations,values,selectors}/mod.rs` (**new**) | 1 |
| `crates/core/src/backend/rust/view/{sheets,bbnf}/ast.rs` (**new**) | 1 |
| `crates/bbnf-tape/src/packed.rs` (**new** — AoS sidecar) | 1 |
| `crates/bbnf-simd-scan/src/{neon,parity,compaction,str2int}.rs` (kernel replacements) | 2 |
| `crates/bbnf-simd-scan/src/emit.rs` (kernel fragment exports) | 2 |
| `crates/bbnf-simd-scan/src/kind_streams.rs` (**new** — n2.3) | 2 |
| `crates/bbnf-simd-scan/src/multi_compare.rs` (**new** — n2.4 + Lever 3) | 2, 9 |
| `crates/ir/src/passes/recognizers/{pattern_alphabet,kernel_strategy,stage1_kinds,alt_frequency,fork_boundaries}.rs` | 2, 5 |
| `crates/bbnf-tape/src/{profile,dedup,shape_dict}.rs` (extend) | 2 |
| `crates/core/src/backend/rust/emitter/shapes/*.rs` (`unreachable_unchecked`, BoundedRegex, Unordered, scan-fusion, AltReorder, Object multi-key) | 2, 9 |
| `crates/bbnf-tape/src/tape.rs` (extend `TapeKind` with `LazyRef`, `ShapeRef`) | 2, 3 |
| `crates/bbnf-tape/src/visitor.rs` (`should_descend`) | 3 |
| `crates/core/src/backend/rust/view/{json,css,sheets,bbnf}/lazy.rs` (**new**) | 3 |
| `crates/ir/src/passes/recognizers/shape_transitions.rs` (**new**) | 4 |
| `crates/bbnf-tape/src/{predictor,speculative}.rs` (**new**) | 4 |
| `crates/bbnf-tape/src/fork.rs` (**new**) | 5 |
| `crates/ir/src/egraph/rules/grammar_*.rs` (**new** — 9 rewrite rules) | 6α, 6β |
| `crates/ir/src/egraph/node.rs` (extend `GrammarENode`) | 6α, 6β |
| `crates/ir/src/passes/recognizers/shape_dispatch/*.rs` (**delete after W6β**) | 6β |
| `crates/core/src/backend/rust/emitter/cpu_variants.rs` (**new**) | 7 |
| `crates/bbnf-tape/src/pmc.rs` (**new**) | 7 |
| `crates/bbnf-tape/src/parse_options.rs` (extend) | 5, 7, 8 |
| `crates/bbnf-jit/` (**new workspace member**) | 8 |
| `crates/derive/src/visitor.rs` (**new**) | 9 |
| `crates/core/examples/{json_serde,css_lightningcss,bbnf_ast,sheets_formula}_visitor.rs` (**new**) | 9 |
| `crates/core/tests/parity/{json,css,sheets,bbnf}.rs` (**new**) | 10 |
| `docs/tranches/AX/parity/{json,css,sheets,bbnf}_divergence.md` (**new**) | 10 |
| Closure / analysis / gorgeous / pprint-vm / imports (per old AX) | 11 |
| `docs/tranches/AX/{PROGRESS,FINAL}.md` + `docs/benchmarks/{post-AX,post-AX-W{0..12}}.json` | 0–12 |
| `docs/tranches/AY/AY.md` (first draft, authored at W12) | 12 |

## Hard gates summary

### W0 — Exorcism
1. Zero `dispatch_one|try_branch|advance_or_pop_with|dta_run|DtaTable|DtaState|FrameStack` in any bench binary (`nm`).
2. Zero `grep` hits in `crates/` for those symbols.
3. `cargo test --workspace` green.
4. Cold-parse bench regression < 2% vs AW-V close.
5. `push_compound_fused_v32` reverted; paired `stp` shipped.
6. "17-digit NEON lever" struck.
7. AW-V.md rewritten in RD language.

### W1 — Value API
8. `bbnf::json::Value == sonic_rs::Value` deep PartialEq on twitter.
9. `bbnf::css::StyleSheet` field-equivalent to `lightningcss::StyleSheet` on bootstrap (~200 variants).
10. Hybrid tape: `packed_cache` ≥ 1.3× SoA single-record.
11. `ValueVisitor<T>` monomorphises for T ∈ {sonic_rs::Value, lightningcss::StyleSheet}.

### W2 — SIMD + dispatch levers
12. `cargo asm` shows PMULL fires on aarch64-apple-darwin.
13. `vpaddq_u8` cascade replaces `vaddv_u8` (microbench ≥ 1.3×).
14. `vdotq_s32` canada fraction packadd lands.
15. TBL-4 kinded bitmap shipped.
16. Paired `stp`: one `stp` instruction per compound write.
17. `BoundedRegex` lifter active; per-rule `prefer_inline_in_loop` populated.
18. `unreachable_unchecked` at every emitter dispatcher default.
19. Kind-separated stage-1 streams emitted + wire-contract test.
20. SIMD-speculative Alt-branch for Unordered (CSS compoundSelector ≥ 2× linear-try).
21. Scan-kernel fusion active at adjacent per-shape sites.
22. AltReorder by corpus frequency applied to dispatchers.
23. Bloom+GADT dedup filter populated; filter-hit rate ≥ 99% of exact-dedup on bootstrap.
24. ShapeRef consumer activated; bootstrap record count drops ≥ 30%.
25. CSS normalize ≥ 1500 MB/s; JSON twitter unchanged from W2.1.

### W3 — Gradient / LazyValue
26. `LazyRef` tape kind emitted by every primary compound shape.
27. Per-grammar `*LazyValue` wrappers ship with fixtures.
28. Twitter-ignore-keys ≥ 2× baseline.
29. `parse_into::<V>()` O(subtree).

### W4 — Speculative
30. CSS bootstrap hot-predict ≥ 0.75.
31. Twitter rollback ≤ 5%.
32. On-correct speedup ≥ 1.05×.
33. Per-grammar transition matrix emitted + wire-contract test.

### W5 — Parallel fork
34. 4-core: canada 2.5–4×, citm 2.5–4×, tailwind 2.5–4×, data_xl 3–4× on ≥ 1 MB.
35. ≤ 1 MB: ≤ 2% regression.
36. `parallel_threshold` default 1 MB.

### W6α + W6β — E-graph rewriting
37. Bootstrap regen idempotent after each rule.
38. CSS L4 saturation ≤ 1 s per grammar.
39. All 9 rewrites active; wire-contract tests pass.
40. `shape_dispatch/*.rs` detectors deleted.
41. No perf regression vs W5.

### W7 — CPU autotune + PMC
42. Ice Lake Xeon citm +10–25%.
43. Graviton 3 twitter +5–15%.
44. Apple M4 Max no regression.
45. `cpu_variant::Auto` correct selection.

### W8 — Cranelift JIT
46. Canada JIT 1.10–1.25× AOT.
47. JIT compile ≤ 5 ms per grammar.
48. SHA cache with grammar-AST-hash key.
49. `jit = true` default off.

### W9 — Multi-visitor + multi-key SIMD
50. Macro compiles ≥ 2 pair combinations per primary grammar.
51. Over-L1 rejected with diagnostic.
52. JSON serde-compat matches sonic-rs.
53. CSS lightningcss-compat within 10% of lightningcss.
54. Multi-key SIMD Object-shape ≥ 1.1× linear-compare on ≥ 16-keys.

### W10 — Parity
55. Zero divergence: `bbnf::json::Value` vs `sonic_rs::Value`.
56. Zero divergence: `bbnf::css::StyleSheet` vs `lightningcss::StyleSheet`.
57. Zero divergence: LazyValue vs simdjson OnDemand.
58. Zero divergence: token-level parity with cssparser.
59. Per-grammar divergence documentation.
60. CI gates PR on failure.

### W11 — Subsystem closures
61. 5 closure tests un-ignore.
62. 4 analysis structural-mode un-ignore.
63. 3 gorgeous un-ignore OR delete.
64. 2 pprint-vm hint un-ignore.
65. `test_selective_transitive_unfurling` un-ignore OR conditional.

### W12 — FINAL
66. Every single-thread parse entry ≥ post-AU.
67. Parallel entries ≥ 2.5× single-thread on ≥ 1 MB.
68. Zero parity divergence across all comparators.
69. Zero `#[ignore]` additions.
70. Zero DTA symbols.
71. `docs/tranches/AY/AY.md` exists with thesis + invariants + wave-schedule.

## Indefatigability

AX is the architectural reckoning. When AX closes:

- One codegen path. No fallback. No interpreter.
- First-class Value API with structural parity vs sonic-rs + lightningcss.
- SoA primary tape with AoS sidecar for hot random-access reads.
- Every viable novel lever from the AW research catalogue deployed or explicitly rejected with rationale.
- Structural-parity CI gate on sonic-rs + lightningcss + simdjson OnDemand + serde_json + cssparser.
- Document-parallel fork as an amortisation multiplier on ≥ 1 MB inputs.
- E-graph grammar rewriting subsumes shape classification.
- Subsystem ledger zero.
- `cargo test --workspace` green, zero ignored.
- AY.md drafted; replay/recovery/incremental tranche opens cleanly on AX substrate without any carried-forward hook.

Post-AX, the codebase is at its performance terminus. AY lands developer tooling (replay, recovery, incremental) on top of the AX substrate, freshly — no legacy hooks, no shims carried forward from the DTA era.
