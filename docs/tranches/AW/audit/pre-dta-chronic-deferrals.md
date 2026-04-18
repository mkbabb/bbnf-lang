# Pre-DTA Chronic Deferrals — Audit of W through AO

## Angle headline

AP–AW-V (covered by `last10-slowdown-census.md`) showed deferrals compounding into a 0.082× post-AU regression. The pre-DTA arc (W → AO, 2026-03 → 2026-04-12, spanning the fn-per-rule RD substrate from the `factor_literal_prefixes` cliff demolition through AO's "0%-impact" structural-dispatch stubs) carries the same disease. Of 18 tranches, **only three close ceremonially** (W, Y, Z) — none have `FINAL.md`, `PROGRESS.md`, `post-{LETTER}.json`, or audit artefacts (tranche-directory edict landed 2026-04-15, `36945f60`). Five genuinely chronic deferrals recur across ≥3 tranches without landing; three of those five are **cold deferrals whose architectural hook still matches the AX RD+shape substrate**; two are dead-lettered by DTA/tape evolution.

## Per-tranche deferral inventory (W → AO)

Citations are plan doc line numbers and AW/audit retros. No FINAL.md exists for any tranche below; "final status" is drawn from the chain the SYNTHESIS ledger names.

| Tranche | Explicitly-deferred items | Origin ref | Subsequently attempted in | Final status |
|---|---|---|---|---|
| **W** | Ten grammar-tier + five HIR-tier e-graph rewrites (V→W deferral); cross-grammar HIR caching; `@cost` directive; `alloc_slice_copy<T: Copy>` for TS/WASM | `W/W.md:436-442` | Y.11/Y.12 partial; AX.W10–W11 | Landed partial (suffix only); full set AX scope |
| **X** | `backend/patterns/` delete (X.8h); SharedHelper activation; JSON authoritative RegexEngine; JSON shared helper hoisting; surgical `scan_number_mantissa` | `X/X.md:535-548` | Y.1/Y.2/Y.6b/Y.8; AW-V | `patterns/` deleted Y.1; SharedHelper substrate only |
| **Y** | Full DAG sub-expression hoisting; global cost objective; HIR SIMD; `core::simd` migration; `seq/repeat/ref_strategy` cost-weight upgrade; dispatch-eligibility + type-proj `MinimizeCost` upgrade | `Y/Y.md:491-500` | Z.5/Z.5b Ref+Repeat CSP; AX indirect | Ref/Repeat CSP landed Z.5/Z.5b; dispatch/type never |
| **Z** | Tier-based structural indexing; AC-3 lattice; memo-strategy CSP; Seq CSP; Sp-method CSP; prefix-factoring + dispatch-eligibility as CSP vars | `Z/Z.md:381-386` | None (AA absorbed tape) | All silently dropped at AA pivot |
| **AA** | TS/WASM TaggedUnion; profile-guided cost feedback (AC.16); multi-threaded parsing; ILP extractor backend; gorgeous rewrite; `@utf8/@lazy/@input_size` directives; unified super-e-graph; pclmulqdq string bitmap | `AA/AA.md:1477-1496` | AW-IV.W4.4 partial for threading | Threading healed AW-IV (+131%); others untouched |
| **AB** | `DirectProjection` parser output; `TapeCompact` side arenas; structural-bitmap SIMD; FCH perfect-hash dispatch; cross-tier TapeView export; profile-guided cost feedback; HIR-tier materialization lattice; `OptimizableTier` trait | `AB/AB.md:710-729` | AG.2/AG.3 (Tier B/Lazy — deferred); AX.W4/W14 | Tier B deferred AG.2/AG.3; PHF landed AP.4 + AW-IV |
| **AC** | Tier B/Lazy emitter (AF.6 sub-tranche); full DTA codegen outside Rust; legacy clone of pre-tape paths in TS/WASM | `AC/AC.md` (implicit in AF/AG status); `AG/AG.md:97-104` | AI.1 wire; AW-III/IV | Substrate landed, consumer deferred through AW-V |
| **AE** | Schema cst_directives emitter rework; heterogeneous Alt sub-variant coercion out of gated phase; variant_idx reconciliation | `AE/AE.md:225-260` | AF.0/AF.0b span-text dispatch | AF.0b deferred codegen regen; hand-patched generated.rs carried |
| **AF** | Tier B emitter (AF.6); CSP tier-variable consumer; view-layer DirectSlot; AF.0b codegen regen | `AF/AF.md:292-338` | AG.5 activation; AI.1–AI.6 wire | Still not emitting Tier B at AW-V; `ir.emission_tier` populated unread |
| **AG** | AG.2 Tier B emitter; AG.3 DirectSlot view; AG.4b self-hosting regen | `AG/AG.md:97-103` | AI.1 wire; bootstrap hand-patched | Deferred to AI; at AI.1 scope, no production Direct rules |
| **AH** | Audit-only; no deliverables (folded into AI) | `AH/AH.md:50-63` | AI.2–AI.6 | Subsumed |
| **AI** | Floor gates only; widen Tier B eligibility, e-graph emission bonus | (plan whole-text) | AI.6 ghost cleanup; AJ hot-path | Zero Direct-tier rules fire; Tape universal |
| **AJ** | None plan-declared; targeted surgery only | `AJ/AJ.md` 75-line plan | — | Closed with AK |
| **AK** | No plan-declared; scaffolding obsoleted by `__branch_idx` | `AK-retro.md:47-57` | AM.1 deletes | EmissionTier deleted |
| **AL** | No plan existed (4 prototypes, no reconciliation); AL.1 a scoped correctness fix reused under AN Phase 1.1 | `AL-retro.md:1-22` | AM.1/AM.3/AN.0 | Scope absorbed AM + AN |
| **AM** | AM.6 cost-model grid sweep; AM.4 SIMD routing; AM.5 structural-bitmap codegen integration; AM.2 payload activation | `AM/AM.md:88-94, 160-181`; `AM-retro.md` | AN.6/AO.4.1/AP.6.4/AQ.9.4 → AX | **Chronic** — cost-grid never landed |
| **AN** | AN.0.5 CSS L4 tailwind; AN.2 scanner generalization; AN.3 single-pass string scan; AN.5 32-byte SIMD; AN.6 CSP instrumentation (all silent) | `AN-retro.md:22-31` | AO Phases 2/3.1/4.3 → AP | Mostly AP/AQ landings; AN.5 AVX2 still ledger-only |
| **AO** | Phases 1, 3, 4, 5 (padded buffer, SIMD widening, cost-grid + global CSP, correctness/polish — **never committed**); Phase 0 infrastructure "code complete, never exercised" | `AO/AO.md:1-19`; `AO-retro.md:16-27` | AP.6.5 global CSP; AP.0.2 tailwind; AQ.5 deletes structural dispatch | Phase 0 entirely dead-on-arrival |

## Chronic deferrals (≥3 tranches, never landed)

### 1. Cost-model grid sweep (AM.6 origin)
**Chain**: AM.6 → AO.4.1 → AP.6.4 → AQ.9.4 → AW-IV.W5.3 null-result → AX scope. **History**: grid sweeps proposed every tranche with concrete knob ranges (`dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`); never executed because maximizing geomean under "no bench regresses >1%" produced no Pareto move. **Applicability to AX**: **CANDIDATE** — AX.W10/W11 add e-graph variants with emitter routes that react to `CostWeights`. A grid sweep after W11 would close the loop the prior five tranches punted. Place at **AX.W13.5** (between CPU autotune and multi-visitor, after all weight-consuming rewrites land).

### 2. Global CSP solve (AL/Z.5 origin)
**Chain**: AL prototype → Z.5 per-component only → AO.4.2 → AP.6.5 → AQ.9.5 → AW ledger. **History**: Y.5 `UnionFind` substrate, dormant through Y/Z/AA/AB/AC/AE/AF — first production consumer AF.3 per-component solve; global joint objective punted every tranche. **Applicability to AX**: **PARTIAL FOLD** — AX.W10/W11 rewrites fire in a saturation loop that subsumes the CSP-joint objective by construction (egraph extraction IS the global cost minimiser). Separate "global CSP solve" item is OBSOLETE under the e-graph-classifier substrate.

### 3. Scanner-architecture generalization (AN.2 → AO.2 cluster)
**Chain**: AN.2 silent-deferred → AO.2.1–2.6 planned never committed → AP partial for CSS SIMD → AR.6.x / AS.5.x → AW ledger. Concrete items: delete CSS re-exports, consolidate number scanners, parameterize `WhitespaceConfig`/quote delimiter, delete SpanParser wrappers, dedup nibble-LUT SIMD. **Applicability to AX**: **CANDIDATE** — AX.W4 scan-fusion and W5 TBL-4 kinded bitmap consume generalized scanners. The `scan_class_prefix_then_class` generalisation (RegexClass-driven dispatch) is the exact shape AX's kinded-bitmap stream wants. Place at **AX.W4.0** (refactor pass before scan-fusion lever lands).

### 4. Structural-bitmap codegen integration (AM.5 origin)
**Chain**: AM.5 "infrastructure only" → AO.0 Phase 0 unexercised → AP.1 gated off → AQ.5 **deleted ~400 LOC**. **Applicability to AX**: **SUPERSEDED** by the per-rule bounded-regex design (per-pattern `last_byte_set` narrowing from `pattern_alphabet.rs`; CSS declaration-value = `[^;}]*` scans with 2-byte termination set). NOT the universal grammar-wide pre-pass; that framing is dead. The narrow per-rule form is AX.W0's structural-scan lever.

### 5. PHF / perfect-hash dispatch (W Phase 5c origin)
**Chain**: W Phase 5c deferred → X.7c residual contingent → AB.7 non-goal → AP.4 **LANDED** CSS bootstrap +50% → AW-IV.W3.2 threshold=3. **Applicability to AX**: **ALREADY HIGH-CONFIDENCE** — lever-efficacy rank MEDIUM `RELIABLE`; AX W11 G6 PhfLoop variant extends to `Repeat(Alt([Lit,…]))`. No new action; confirms AX reuses a validated AP-era lever.

## Architectural patterns worth carrying forward

1. **`CostWeights` as cross-tier substrate (W Phase 3a; `W/W.md:196-215`)**. The `egraph::cost_weights.rs` shared struct with both `GrammarCostModel` and `RegexExtractionCost` embedding it is the pattern AX.W6 cost-model retires the per-shape detectors against. Invariant holds. **Carry forward**: any new cost axis (e.g. parallelism score in W9) lives on the shared struct, not a sibling.

2. **Analysis<N> + EClassFacts as monotone lattice (AA Act I; `AA/AA.md:99-122`)**. The `EClassFacts` (`first_set`, `nullable`, `width`, `elision_safe`, `closure_free`, `is_fixed_shape`, `all_descendants_elidable`) survived AB→AI and now powers `classify_materialization`. This is the pattern AX.W10/W11 rewrite preconditions read from. **Carry forward**: new rewrites read preconditions off EClassFacts, don't re-mine.

3. **Consumer-invariant grep test (Y.13; `Y/Y.md:326-332`)**. Exhaustive-match walk over `AltMode`/`WrapMode`/`RegexEngine` variants asserting ≥1 consumer. Extended through Z.5/Z.5b/AB/AF. **Carry forward**: AX.W6 `gate_predicate_wire_contract.rs` is a direct descendant; extend to every new `GrammarENode` variant W10/W11 adds.

4. **Substrate-break sub-tranche (AE/AF.0b pattern; `AF/AF.md:98-129`)**. When a regen-blocking codegen bug surfaces, isolate as sub-tranche letter rather than block the broader optimization work. The `no_subvariant_refs.rs` grep-gate is the load-bearing invariant. **Carry forward**: AX.W0b DTA deletion precisely replicates this pattern.

## AX wave additions proposed

1. **New AX.W4.0 — parse-that scanner generalization (from AN.2/AO.2 cluster).** Before W4's SIMD levers consume scanners, collapse `css_ident_fast` / `css_ws_comment_fast` / `css_string_fast` / three `scan_{digits,alnum,hex}_mut` into `RegexClass`-dispatched primitives. Keeps W4 levers drop-in-replaceable per RegexClass. 1 agent, 2 days. Chain: AN.2 → AO.2 → AR.6.x → AS.5.x.

2. **New AX.W13.5 — cost-model grid sweep (from AM.6).** Execute the chronic. Grid over `dispatch_bonus ∈ {-4, -3, -2, -1}`, `call_overhead ∈ {2, 4, 6, 8}`, `inline_body_size_penalty ∈ {0.25, 0.5, 1.0}`, plus new W10/W11 weights. Maximize geomean across 17 entries with no single regressing >1%. Attribution via `BBNF_COST_TRACE=1`. 1 agent, 1 day. Fires AFTER all cost-consuming levers land so the optimum is meaningful.

3. **Retire dormant from "chronic" status explicitly.** AX.W15 FINAL.md lists cost-grid (W13.5) and scanner-gen (W4.0) as CLOSED chronics; lists structural-bitmap as SUPERSEDED-by-per-rule-bounded-regex; lists global-CSP-solve as SUBSUMED by egraph-classifier per §egraph-substrate-audit.

## Dead-letter pile — do NOT resurrect

- **Universal grammar-wide structural-dispatch bitmap (AM.5 / AO Phase 0 / AP.1 / Z.3b)**. The *universal* pre-pass is dead (dense-alphabet collapse); the *per-rule bounded* form is alive in AX.W0 per `structural-scan-working-approach.md`. Distinguish carefully.
- **Global CSP joint objective (AL prototype / AO.4.2 / AP.6.5)**. Subsumed by e-graph extraction; re-implementing as CSP is a parallel subsystem violating "one codegen path" / "fold into existing systems" invariants.
- **AB `DirectProjection` parser output / `TapeCompact` side arenas**. The "one parser ABI" commitment settled this; accessor-fusion-over-tape (AC follow-up) landed at AI.5 as view layer. The projection-at-parser shape is architecturally dead.
- **AA `@utf8`/`@lazy`/`@input_size` directives**. AX.W1 Value API + W7 LazyRef+should_descend absorbs the user-facing capability without directive surface; the directive path is dead.
- **AA pclmulqdq string-interior bitmap**. AM.4 SIMD carry-less multiply landed with neutral impact; regressed CSS. Refuted.
- **AB structural-bitmap SIMD vectorization**. Same dead-letter class as AM.5.
- **AC.14 memoization strategy CSP**. Signal rank 1/5; never met the tranche bar. No substrate dependency.

## Key findings

The pre-DTA arc produced **two chronic items worth folding into AX** (cost-grid, scanner-gen) and **one MEDIUM that's historically reliable but already integrated** (PHF, now AX.W11 G6). Everything else chronically deferred through W–AO is either superseded by newer architecture (universal structural-bitmap → per-rule bounded-regex) or subsumed (global-CSP-objective by e-graph extraction, DirectProjection by AI.5 view-layer). The pre-DTA tranches primarily leave the AX plan with **governance guardrails** — substrate-without-consumer as #1 failure mode appears 9 times in W–AO, directly motivating AX invariants 9, 14, 16 and the wave-verification ledger in the README. The three CSP/e-graph architectural patterns (CostWeights substrate, EClassFacts monotone lattice, consumer-invariant exhaustive match) all survived the pre-DTA → DTA → post-DTA transitions and form the core substrate AX.W10/W11 rely on.
