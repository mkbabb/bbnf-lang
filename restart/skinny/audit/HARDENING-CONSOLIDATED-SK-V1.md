# Skinny Hardening Consolidation - SK-V1

## §1 Target identification

| Field | Value |
|---|---|
| Target | SKINNY-SUITE |
| Cycle | SK-V1 |
| Output | `restart/skinny/audit/HARDENING-CONSOLIDATED-SK-V1.md` |
| Target paths | `restart/skinny/INDEX.md`, `SUBSTRATE.md`, `COMPILER.md`, `BENCH.md`, `WORKSPACE.md` |
| Authority paths | `restart/prompts/HARDENING.md`, `restart/prompts/ORCHESTRATOR.md`, `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/14-LOCKS.md`, `restart/corpora/SOTA.md`, `docs/precepts/instructions/STYLE.md`, `LESSONS-LEARNED.md` |
| Lines audited | INDEX 1-79; SUBSTRATE 1-562; COMPILER 1-707; BENCH 1-1424; WORKSPACE 1-593 |
| Cycle role | Pre-implementation prior-validation gate |

The suite is coherent enough to amend, not rewrite. The load-bearing faults are narrow: arena policy contradicts compiler payload emission, the host-call and cost-model cuts are asserted rather than measured, the bench matrix has shadowed rows and a conditional SIMD failure that exits green, and the bench crate layout contradicts the closed ten-crate workspace. Those faults affect Lens L and Lens M directly, so SK-V1 cannot dispatch implementation until the punch list lands.

## §2 Cohort verdict

| Lens | Verdict | KEEP | REINVENT | DISCARD | Skinny signal | Recommendation |
|---|---|---:|---:|---:|---|---|
| Lane 1 - Lock adherence | REINVENT | 8 | 6 | 0 | Lock 1/5/8 mostly hold; Lock 13/14 need skinny fences | Apply P1, P4, P5, P7 |
| Lane 2 - Sequencing | KEEP | 1 | 0 | 0 | N/A for single-wave skinny | Keep N/A record |
| Lane 3 - Cohesion | REINVENT | 4 | 7 | 0 | Cross-quadrant contradictions remain | Apply P1, P4, P6, P8 |
| Lane 4 - SOTA anchoring | REINVENT | 5 | 4 | 0 | Anchors present; baseline/API pinning needs repair | Apply P2, P3, P7 |
| Lane 5 - Grammar authority | REINVENT | 4 | 3 | 0 | JSON-only exception needs deletion gates | Apply P5, P6 |
| Lane 6 - LOC budget | REINVENT | 4 | 4 | 0 | 31,400 LOC is named; Track 2 and generated JSON gates drift | Apply P4, P10 |
| Lane 7 - Friction forecast | REINVENT | 3 | 3 | 0 | Key user confusions surfaced but not all error text is bound | Apply P9 |
| Lane 8 - Carry/deferral | REINVENT | 6 | 4 | 0 | Receivers mostly named; blockers/gates thin in omissions | Apply P2, P3, P6 |
| Lane 9 - Greenfield discipline | REINVENT | 5 | 3 | 0 | The skinny is suitably small; two shortcuts mask risk | Apply P1, P3, P5 |
| Lens F - LLM bias | REINVENT | 3 | 4 | 0 | Several "none for SOTA" claims outrun evidence | Apply P1, P2, P3 |
| Lens G - Overfitting | REINVENT | 2 | 5 | 0 | JSON overfit is acknowledged, not always bounded | Apply P3, P5, P8 |
| Lens H - Provenance | REINVENT | 4 | 5 | 0 | Some citations are right; PASS and external claims need local proof | Apply P2, P7, P8 |
| Lens I - Skinny contrivance | KEEP-MODIFY | 5 | 2 | 0 | Apparatus is mostly small enough | Apply P6, P10 |
| Lens J - Host-language use | KEEP-MODIFY | 5 | 1 | 0 | Rust facilities are used well; manual greps need typed checks | Apply P5 |
| Lens K - Meta-grammar | REINVENT | 4 | 3 | 0 | JSON exception is tolerable only with a removal gate | Apply P5, P6 |
| Lens L - Premise fidelity | MASKING | 5 | 4 | 0 | Host-call, arena, and plan-selection cuts can over-predict V1 | Apply P1, P2, P3 |
| Lens M - Falsifiability | REINVENT | 5 | 5 | 0 | NO-GO exists; classifier has shadowed/green conditional failures | Apply P7 |
| Lens N - Mechanicality | MECHANICAL-WITH-NARROW-AMENDMENT | 5 | 3 | 0 | Graduation is plausible after path and wrapper fixes | Apply P4, P5, P6 |

Final decision: **SK-AMENDMENT-REQUIRED-NARROW**.

## §3 Lane 1 - Lock adherence

Lane standard: every skinny quadrant must honor the settled locks while admitting documented, mechanically closed deviations.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/INDEX.md:49` | Tape/direct as one substrate | The index binds tape plus direct-to-struct to Lock 1. | Matches Lock 1 at `restart/locks/14-LOCKS.md:34` and ARCH runtime union at `restart/ARCHITECTURE.md:1373-1409`. | The bench must ensure Track 2 uses the same substrate. | Track 2 can cheat if it lives as an independent per-grammar crate. | KEEP |
| `restart/skinny/BENCH.md:66-80` | Track 2 crate path | Bench places hand-coded JSON in `crates/runtime-json-handcoded`. | Gives a visible substrate ceiling probe. | WORKSPACE closes the crate set at ten crates and excludes this crate at `restart/skinny/WORKSPACE.md:24-38`. | A hand-coded probe outside the closed workspace is either unbudgeted or a stealth eleventh crate. | REINVENT |
| `restart/skinny/BENCH.md:42-59` | Generated runtime crate path | Bench uses `crates/runtime-json`, while WORKSPACE and SUBSTRATE use `crates/runtime/src/grammars/json`. | It isolates Track 1 for measurement. | It contradicts ARCH generated runtime path at `restart/ARCHITECTURE.md:1411-1425` and WORKSPACE metadata at `restart/skinny/WORKSPACE.md:142-161`. | A separate runtime crate can become a second per-grammar runtime surface. | REINVENT |
| `restart/skinny/COMPILER.md:461-483` | BIR-only Rust lowerer | Compiler says lowerer walks Backend IR. | Honors Lock 5 and ARCH lowerer contract at `restart/ARCHITECTURE.md:1427-1445`. | The source-load phase elsewhere says use the V1 parser as-is. | The lowerer boundary survives; parser sourcing is a separate cohesion fault. | KEEP |
| `restart/skinny/WORKSPACE.md:191-227` | Samply-resolvable profiles | Release and bench keep debug symbols and do not strip. | Honors the profiling discipline and keeps SOTA failures diagnosable. | Thin LTO may differ from final V1 tuning. | Skinny is a prior, not the final J.W1 close; thin LTO is acceptable if recorded. | KEEP |
| `restart/skinny/BENCH.md:528-541` | SOTA matrix | Matrix names sonic-rs/simd-json anchors and explicit multipliers. | Honors Lock 8 and MASTER SOTA rows at `restart/MASTER-PLAN.md:131-138`. | Outcome L contradicts BENCH §4.3 by letting SIMD floor failure exit conditionally. | A structural-scan floor failure cannot be green in a SOTA prior gate. | REINVENT |
| `restart/skinny/WORKSPACE.md:237-391` | Directory child counts | Workspace lists 4-10 children for each crate and promotes parse-that-regex subtrees. | Honors Lock 13 at `restart/locks/14-LOCKS.md:58`. | `xtask/src/main.rs` gets a one-file carveout. | The carveout is dev-only and explicit; acceptable. | KEEP |
| `restart/skinny/COMPILER.md:405-429` | JSON hand-curated recognizer in `passes` | The recognizer is grammar-specific code in a generic crate. | Skinny needs one grammar to run cheaply. | Lock 14 forbids grammar-specific code in generic crates at `restart/locks/14-LOCKS.md:60`. | A disposable skinny module is acceptable only if fenced and deleted at graduation. | REINVENT |
| `restart/skinny/COMPILER.md:645-658` | HM hierarchy inversion | Skinny admits HM runs top-level while V1 makes it a layout subroutine. | The contradiction is named. | The file paths still use `passes/src/types/...` at `restart/skinny/COMPILER.md:347-354`, conflicting with WORKSPACE `passes/src/layout/` at `restart/skinny/WORKSPACE.md:286-295`. | Graduation needs wrapper movement named now. | REINVENT |
| `restart/skinny/WORKSPACE.md:181-184` | `wasm = false` | Metadata marks WASM off. | Honors Lock 5 V1 Rust-only boundary at `restart/locks/14-LOCKS.md:42`. | V1 validator must accept false while rejecting true. | This is a schema rule, not an architecture change. | KEEP |

Lane verdict: **REINVENT**. Count: KEEP 5, REINVENT 5, DISCARD 0.

## §4 Lane 2 - Sequencing discipline

Lane standard: V1 wave sequencing is N/A because skinny is a single vertical slice.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/INDEX.md:67-77` | Decision protocol | The index gives one ordered implementation path, then bench, then GO/NO-GO. | It wires substrate, compiler, Track 2, parity, results, and dispatch in one sequence. | It does not split waves, so Lane 2 cannot apply literally. | The single-wave nature is the point of skinny; no sequencing fault. | KEEP |

Lane verdict: **KEEP/N-A**. Count: KEEP 1, REINVENT 0, DISCARD 0.

## §5 Lane 3 - Cohesion

Lane standard: each quadrant claim must agree with the other quadrants and be verifiable from the artefacts named.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/SUBSTRATE.md:175-196` + `restart/skinny/COMPILER.md:76-87` | Payload arena policy | SUBSTRATE says numbers/strings borrow or lazy-parse with zero arena writes; COMPILER says regex post-passes write decoded strings and f64 slots into the arena. | Both are plausible JSON strategies. | They cannot both be the hot path measured by Track 1/2. | A bench result cannot validate "zero arena allocations" if generated code eagerly writes decoded payloads. | REINVENT |
| `restart/skinny/SUBSTRATE.md:343-365` + `restart/skinny/COMPILER.md:449-450` | Typed scalar views | SUBSTRATE exposes `JsonString::as_str()` and `JsonNumber::as_f64()` as lazy accessors; COMPILER shape facts include decoded `ArenaHandle` and `f64`. | Shape facts help codegen. | They pre-decide materialization and contradict lazy payload policy. | The skinny must choose parse-throughput lazy shape and move decoded fields behind accessors. | REINVENT |
| `restart/skinny/SUBSTRATE.md:252-258` + `restart/skinny/COMPILER.md:192` | Structural scan mode | SUBSTRATE says JSON structural alphabet uses Exact mode; COMPILER emits `SimdScan { mode: Prefilter }`. | Both use scalar validation somewhere. | Exact and prefilter have different correctness and cost contracts. | The bench cannot compare SIMD parity if the compiler and runtime disagree on scan mode. | REINVENT |
| `restart/skinny/WORKSPACE.md:24-38` + `restart/skinny/BENCH.md:1189-1192` | Closed crate list vs bench crates | WORKSPACE closes ten crates; BENCH adds `runtime-json` and `runtime-json-handcoded`. | Track separation is valuable. | The workspace budget and Lock 13 layout no longer describe the bench. | Put generated JSON under `runtime/src/grammars/json` and Track 2 under `bbnf-bench/src/track2/json.rs`. | REINVENT |
| `restart/skinny/WORKSPACE.md:137-139` + `restart/skinny/WORKSPACE.md:591` | Competitor dependencies | WORKSPACE lists sonic-rs/simd-json under workspace dependencies, then says they are not workspace dependencies. | The intended owner is BENCH. | The TOML sketch contradicts the prose. | Exact competitor pins belong only in `bbnf-bench` dev-dependencies. | REINVENT |
| `restart/skinny/COMPILER.md:385-386` + `restart/skinny/WORKSPACE.md:51` | Parser dependency | COMPILER says use the V1 `bbnf` parser and depends on `parse-that`; WORKSPACE says `parse-that` is skipped and grammar is partial. | Reusing parser code would save time. | It violates the closed skinny dependency set. | A partial grammar parser is sufficient for `json.bbnf`; no hidden parse-that dependency. | REINVENT |
| `restart/skinny/SUBSTRATE.md:117` + `restart/skinny/WORKSPACE.md:28` | `parse_owned` | SUBSTRATE omits `parse_owned`; WORKSPACE facade includes it for JSON. | V1 API family is preserved. | The skinny cannot both omit and expose the method without a wrapper contract. | Keep the facade method only if it is explicitly a cold wrapper over `parse`. | REINVENT |
| `restart/skinny/BENCH.md:310-319` + `restart/skinny/BENCH.md:767-803` | Parity oracle timing | Prose says 1-in-100 parity sample inside timed region; sketch runs parity before timing only. | Correctness gate is mandatory. | Serialization inside timed samples would swamp parse timings. | Keep parity outside timing and run a separate drift bench if needed. | REINVENT |

Lane verdict: **REINVENT**. Count: KEEP 0, REINVENT 8, DISCARD 0.

## §6 Lane 4 - SOTA anchoring

Lane standard: every throughput gate cites competitor, dataset, platform, and materialization mode; the threshold must be calibrated against MASTER parity/beat semantics.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/BENCH.md:300-307` | Per-corpus targets | Bench records twitter/citm/canada beat and parity floors. | Matches MASTER rows at `restart/MASTER-PLAN.md:131-135`. | It uses both fixed floors and matrix multipliers; the two can diverge. | Matrix should compute from observed competitor rows and report fixed floors as explanatory. | KEEP-MODIFY |
| `restart/skinny/BENCH.md:519-524` | `S = min(sonic-rs, simd-json)` | The matrix uses the faster Rust JSON competitor per corpus. | More adversarial than sonic-rs-only. | SOTA source does not specify simd-json API; BENCH admits this at `restart/skinny/BENCH.md:1329-1338`. | Run both simd-json APIs or stop using simd-json as the threshold minimum. | REINVENT |
| `restart/corpora/SOTA.md:52-56` + `restart/skinny/BENCH.md:1340-1347` | sonic-rs mode | SOTA marks sonic-rs numbers as unchecked; BENCH uses checked default and different feature flags. | BENCH records the feature choice. | The threshold anchor and measured competitor may no longer be the same competitor row. | Either use unchecked mode to match the anchor or refresh the baseline inside RESULTS. | REINVENT |
| `restart/skinny/BENCH.md:342-418` | Structural scan microbench | Bench measures GB/s and scalar parity. | Honors MASTER `simd/structural_scan` row at `restart/MASTER-PLAN.md:138`. | It uses twitter only; canada scale is inferred. | Twitter is acceptable for SIMD scan only if the parse matrix still worst-cases canada. | KEEP |
| `restart/skinny/BENCH.md:1251-1254` + `restart/skinny/BENCH.md:705-742` | Allocator pin | BENCH says `mimalloc` is default, but the Cargo.toml snippet omits it. | Allocator drift is a real SOTA risk. | The mitigation is not actually specified in the crate manifest. | Add the dependency/features or remove the claim. | REINVENT |
| `restart/skinny/BENCH.md:872-888` | CI discounts | CI thresholds are discounted; local run remains authoritative. | Avoids false NO-GO on noisy CI. | Conditional GO exits green at `restart/skinny/BENCH.md:861-870`. | CI green must not authorize dispatch for conditional or SIMD-fail outcomes. | REINVENT |

Lane verdict: **REINVENT**. Count: KEEP 2, REINVENT 4, DISCARD 0.

## §7 Lane 5 - Grammar-authoritative discipline

Lane standard: the skinny may be JSON-only, but every JSON-specific mechanism in a generic crate needs a bounded lifetime and V1 receiver.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/INDEX.md:47` | One grammar | The suite declares JSON-only at the top. | Honest scope. | JSON assumptions can leak into generic crates. | The audit must police every leak. | KEEP |
| `restart/skinny/COMPILER.md:407-429` | `json_handcurated.rs` | A JSON recognizer lives under `passes/src/recognizers`. | Cheap and direct. | Generic `passes` gains grammar-named code. | Fence it as `passes::skinny_json` with a deletion gate or move it to bench fixtures. | REINVENT |
| `restart/skinny/COMPILER.md:433-452` | `shapes_for_json` | Hand-curated JSON shapes replace schema mining. | Keeps the HM slice small. | Generic `passes::shapes` gets a grammar-specific table. | Same fence/deletion requirement as the recognizer. | REINVENT |
| `restart/skinny/SUBSTRATE.md:529-547` | Generated JSON runtime under runtime | SUBSTRATE puts JSON modules under `runtime/src/grammars/json`. | Matches ARCH generated module path. | BENCH contradicts it with `crates/runtime-json`. | Keep SUBSTRATE shape; amend BENCH. | KEEP |
| `restart/skinny/WORKSPACE.md:559-568` | Host and recovery omissions | WORKSPACE states JSON-specific omissions and impact. | Good prior-validation prose. | The host-fn-free row says zero impact for JSON while ARCH marks JSON host fns. | Mark host-fn-free as deliberate deviation with microbench receiver. | REINVENT |

Lane verdict: **REINVENT**. Count: KEEP 2, REINVENT 3, DISCARD 0.

## §8 Lane 6 - Generated-code and LOC budget

Lane standard: the 31,400 handwritten LOC ceiling and <=4,000 generated JSON LOC ceiling are binding, with every unbudgeted source surface counted somewhere.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/INDEX.md:3` | Top-level budget | Index names 31,400 handwritten and <=4,000 generated LOC. | Binding number exists. | It depends on all crates matching WORKSPACE. | Bench-only runtime crates break the arithmetic. | KEEP-MODIFY |
| `restart/skinny/WORKSPACE.md:63-77` | Per-crate LOC table | Workspace decomposes the LOC by crate. | Good hardening surface. | Track 2 hand-coded parser is absent unless inside `bbnf-bench`. | Put Track 2 under `bbnf-bench` and count it. | REINVENT |
| `restart/skinny/BENCH.md:1213-1227` | Bench LOC split | BENCH gives an internal `bbnf-bench` split. | Useful. | It totals about 1,010 while WORKSPACE budgets 2,000; fine but should include Track 2 if moved. | Add `track2/json.rs <= 500` to the split. | REINVENT |
| `restart/skinny/BENCH.md:960-966` | No LOC budget bench | BENCH says generated LOC is not a skinny NO-GO. | Throughput is the central purpose. | The skinny itself has <=4,000 generated JSON LOC as a binding claim. | Add a skinny generated-LOC check; defer only nine-grammar scale to V1. | REINVENT |
| `restart/skinny/WORKSPACE.md:81-87` | `passes` budget contradiction | The spec treats a `passes` overrun as a binding scope signal. | Excellent falsifiability for buildability. | It needs a command to measure current LOC. | Add `cargo xtask lint-loc` or `tokei` gate to the build loop. | KEEP-MODIFY |

Lane verdict: **REINVENT**. Count: KEEP 1, REINVENT 4, DISCARD 0.

## §9 Lane 7 - Friction forecast

Lane standard: every user-facing point of confusion gets a concrete diagnostic or cookbook owner.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/WORKSPACE.md:66` | Unsupported directives | Grammar parser rejects non-skinny directives with `BBNF-DIRECTIVE-NOT-IN-SKINNY`. | Good error-code start. | No verbatim message says where the V1 receiver lives. | Add "JSON skinny accepts only @import/@token; @host fn lands in tranche D." | REINVENT |
| `restart/skinny/BENCH.md:1156-1178` | Track 2 review checklist | The checklist tells reviewers how to reject cheating. | Strong friction control for bench trust. | Grep checks are weak for structural equivalence. | Add a compile-time trait/call-site proof alongside greps. | REINVENT |
| `restart/skinny/BENCH.md:1022-1031` | RESULTS verdict sentence | The first line is machine-rendered. | Readers see outcome immediately. | Conditional outcomes can read as dispatch permission while action says hold a tranche. | Split "GO", "CONDITIONAL", "INVALID", and "NO-GO" exit semantics in text. | REINVENT |
| `restart/skinny/SUBSTRATE.md:553-560` | Open questions | Token split, NodeKindId width, Arc<Tape>, whitespace policy are surfaced. | Good engineering handoff. | No diagnostic names. | These are implementer questions, not user friction; acceptable. | KEEP |

Lane verdict: **REINVENT**. Count: KEEP 1, REINVENT 3, DISCARD 0.

## §10 Lane 8 - Carry and deferral audit

Lane standard: every omission names receiver, blocker, and receiving gate.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/INDEX.md:31-41` | Not-tested table | Index maps omissions to V1 owners. | Strong receiver naming. | Some rows lack blocker and receiving gate. | Add gate names for cost model, recovery, path, generated LOC. | REINVENT |
| `restart/skinny/INDEX.md:58-63` | Open contradictions | Four deviations get V1 closures. | Good Lens N source. | The prompt lists five deviations; INDEX lists four and omits `wasm = false`. | Add the metadata flag to INDEX or remove it from the Lens N mandatory list. | REINVENT |
| `restart/skinny/WORKSPACE.md:508-525` | Migration parity | Every skinny crate maps to V1 destination. | Good graduation map. | It does not assign LOC closure cost per deviation. | Add per-deviation closure-cost estimates in the audit receiver rows. | KEEP-MODIFY |
| `restart/skinny/COMPILER.md:640-671` | Source-authority conflicts | Compiler names layout and host-fn conflicts. | Honest. | Host-fn-free closure lacks a measurement gate. | Add BENCH host-call microbench receiver. | REINVENT |
| `restart/skinny/BENCH.md:936-983` | Bench omissions | CSS, incremental, LOC, WASM, Pratt/SIMD detection omissions are explicit. | Good scope hygiene. | No generated-LOC skinny gate despite top-level budget. | Enforce skinny JSON LOC locally; defer only multi-grammar scale. | REINVENT |

Lane verdict: **REINVENT**. Count: KEEP 1, REINVENT 4, DISCARD 0.

## §11 Lane 9 and lenses F-N

### Lane 9 - Greenfield discipline

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/WORKSPACE.md:40-57` | Skipped V1 crates | Workspace cuts the large V1 crate set aggressively. | This is the right skinny instinct. | Inlined shims can grow into hidden crates. | The 500 LOC shim rule at `restart/skinny/WORKSPACE.md:506` controls it. | KEEP |
| `restart/skinny/SUBSTRATE.md:60-66` | 16-byte token union | Token density is measured, not assumed final. | Good measurement posture. | Token split perturbation is deferred. | Since the bench owns the perturbation, this is acceptable. | KEEP |
| `restart/skinny/COMPILER.md:574-575` | Cost/egraph stubs | The spec says cost model is not a recovery lever if skinny misses SOTA. | Keeps scope small. | It overstates: an alternate JSON plan could bound whether plan selection matters. | Add a non-egraph alternate-plan bench before making the claim. | REINVENT |

### Lens F - LLM bias

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/SUBSTRATE.md:7` | "If JSON cannot reach SOTA-parity, no other grammar will either" | Strong premise claim. | It commits. | It exceeds evidence; CSS and Sheets stress host/layout/Pratt paths JSON omits. | Rewrite to "JSON failure is strong negative evidence for JSON-class tape/SIMD throughput." | REINVENT |
| `restart/skinny/COMPILER.md:100-102` | Host dispatch below noise floor | Claim says direct vs registry call cost is negligible. | Plausible. | No measurement backs it. | Add host-call microbench or delete the claim. | REINVENT |
| `restart/skinny/COMPILER.md:399-400` | "None for SOTA on JSON" recognizer/egraph | The text asserts perfect recognizer choice. | The structural alphabet is obvious. | Plan-selection cost remains unbounded. | Mark FAITHFUL only after alternate-plan stub. | REINVENT |
| `restart/skinny/BENCH.md:1233-1237` | Hand-coded parser fits easily | It estimates Track 2 implementation cost. | The LOC seems plausible. | "Easily" is editorial; no proof. | Replace with a 500 LOC cap plus review fail if exceeded. | REINVENT |

### Lens G - Overfitting

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/COMPILER.md:405-429` | JSON-specific recognizer | It deliberately overfits one grammar. | That's the skinny design. | The result cannot validate the V1 recognizer miner. | Label outcome as JSON-plan evidence and assign miner validation to H.W2/H.W3. | REINVENT |
| `restart/skinny/WORKSPACE.md:561-562` | Type-system and optimization risks | WORKSPACE admits JSON does not validate CSS/Sheets type-system cost. | Good caveat. | It does not name a bench-recoverable bound. | Add caveat to RESULTS probability update. | KEEP-MODIFY |
| `restart/skinny/SUBSTRATE.md:560` | Whitespace policy | JSON drops whitespace tokens; CSS will differ under layout. | Honest. | It means substrate risk does not fully collapse for CSS. | The index must say "JSON-class grammars," which it already does at `restart/skinny/INDEX.md:19`. | KEEP |

### Lens H - Hallucination and provenance

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/COMPILER.md:33-41` | ARCH §12.1 JSON claim | Compiler later corrects that ARCH §12.1 is YAML, not JSON. | The contradiction is surfaced at `restart/skinny/COMPILER.md:660-671`. | The earlier section still states the wrong authority. | Rewrite §1 to cite the later conflict, not ARCH §12.1. | REINVENT |
| `restart/skinny/SUBSTRATE.md:13-15` | PASS citations | SUBSTRATE cites PASS-2/PASS-3 despite skinny prompt warning against over-reading unless cited. | The target itself cites them, so reading is allowed. | PASS line numbers may drift because those files are dirty in this worktree. | Keep ARCH/locks as authority; use PASS only as illustrative. | KEEP-MODIFY |
| `restart/skinny/BENCH.md:1329-1347` | External API uncertainty | BENCH admits simd-json and sonic-rs feature uncertainty. | Good provenance hygiene. | The threshold still depends on those uncertain rows. | Refresh baselines during fixture setup and record exact APIs in RESULTS. | REINVENT |

### Lens I - Skinny contrivance

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/WORKSPACE.md:34-35` | `parse-that-regex` plus `simd-scan` | Both are real hot-path pieces for JSON. | Not cargo-culted V1 apparatus. | `avx512/` and `wasm/` are carried dead in simd-scan. | Since simd-scan is kept verbatim and cfg-gated, this is acceptable. | KEEP |
| `restart/skinny/WORKSPACE.md:47-55` | Inlined stubs | Host/source/error/pipeline shims replace full crates. | Correct skinny simplification. | Shims can accrete. | 500 LOC shim rule keeps them skinny. | KEEP |
| `restart/skinny/BENCH.md:1156-1178` | Manual signatures | Human signatures guard Track 2 fairness. | Reviewable. | A structural compile-time proof is stronger than a signature. | Keep checklist, add automated trait proof. | REINVENT |

### Lens J - Host-language use

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/SUBSTRATE.md:119` | `PhantomData<fn() -> K>` | Rust auto-trait posture is handled by Rust types. | Good host-language use. | None. | Keep. | KEEP |
| `restart/skinny/SUBSTRATE.md:175-177` | Lazy number/string access | Rust `Cow` and lazy parse model carry ownership cheaply. | Honors Lock 9. | Compiler must stop eager payload writes. | Keep substrate, amend compiler. | KEEP-MODIFY |
| `restart/skinny/BENCH.md:1305-1310` | Grep-based Track 2 proof | Grep detects missing imports. | Cheap. | Grep cannot prove call equivalence. | Add a trait-bound API proof so Track 2 cannot construct a parallel root. | REINVENT |

### Lens K - Meta-grammar discipline

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/COMPILER.md:570-580` | Stubbed V1 crates | Compiler cuts optimizer, VM, LSP, path crates. | Good parser-generator focus. | Cost-model cut needs measurement bound. | Add alternate-plan stub and keep the rest cut. | REINVENT |
| `restart/skinny/WORKSPACE.md:525` | Fresh over legacy core | Workspace rejects old `core` walker pattern. | Honors greenfield discipline. | It mines `csp-solver` ABI shapes while skipping CSP. | Since only V1 graduation uses those shapes, acceptable. | KEEP |

### Lens L - Premise fidelity

| Site | Omission/deviation | Classification | Bench-recoverable signal | Challenge | Verdict |
|---|---|---|---|---|---|
| `restart/skinny/COMPILER.md:244-256` | DK13, bidirectional, CSP, GADT omitted | FAITHFUL with V1-grammar caveat | JSON monomorphic HM result; caveat names CSS/Sheets in WORKSPACE at `restart/skinny/WORKSPACE.md:561`. | The spec must say JSON-FAITHFUL, not skinny-universal. | KEEP-MODIFY |
| `restart/skinny/COMPILER.md:399-403` | Recognizer mining/egraph/cost extraction omitted | MASKING until bounded | None yet; single plan is asserted. | Cost-driven rewrites can select dispatch-tree vs jump-table or exact vs prefilter variants that affect throughput. | REINVENT |
| `restart/skinny/COMPILER.md:94-113` | `CallHost` omitted | MASKING until measured | None yet. | Direct substrate decode may hide registry-dispatch cost V1 pays for JSON host fns. | REINVENT |
| `restart/skinny/SUBSTRATE.md:167-196` | Empty payload arena | MASKING due contradiction | Bench can count arena writes, but compiler currently writes payloads. | The claim is invalid until compiler and bench agree. | REINVENT |
| `restart/skinny/SUBSTRATE.md:476-486` | Recovery, path, PHF, eager parse omitted | FAITHFUL | Valid-input parse benches and visitor path cover this. | PHF affects lookup, not parse. | KEEP |
| `restart/skinny/WORKSPACE.md:569-572` | Multi-grammar/generated-LOC scale omitted | FAITHFUL with V1 caveat | Skinny generated JSON LOC gate should exist; nine-grammar scale defers. | Throughput prior does not predict generator scale. | KEEP-MODIFY |

### Lens M - Falsifiability

| Site | Threshold/claim | Falsifiability test | Surgery | Verdict |
|---|---|---|---|---|
| `restart/skinny/BENCH.md:528-541` | Matrix rows A-L | Rows G/I/K give real NO-GO branches. | Keep NO-GO rows. | KEEP |
| `restart/skinny/BENCH.md:533-534` | D and E codegen gaps | E is shadowed by D if classifier checks rows in order because `>1.50` is a subset of `>1.20`. | Change D to `1.20 < T1/T2 <= 1.50`, E to `>1.50`, and define classifier precedence. | REINVENT |
| `restart/skinny/BENCH.md:536-537` | G and H substrate gap | H is shadowed by G because G says Track 1 irrelevant for `T2 > S*1.10`. | Merge H into G or classify H before G with distinct action. | REINVENT |
| `restart/skinny/BENCH.md:386-388` + `restart/skinny/BENCH.md:541` | SIMD floor failure | §4.3 says below floor is NO-GO; row L says CONDITIONAL and exit 0. | Make SIMD throughput failure exit non-zero or force F/G classification before dispatch. | REINVENT |
| `restart/skinny/BENCH.md:502-509` | Schema enforcement | Missing fields fail before threshold comparison. | Keep. | KEEP |
| `restart/skinny/BENCH.md:861-870` | Conditional GO exit 0 | CI green authorizes conditional outcomes. | Return a distinct non-zero advisory exit for E/L or mark CI pass as non-dispatching. | REINVENT |

### Lens N - Graduation mechanicality

| Site | Deviation | Closure path | Cost | Challenge | Verdict |
|---|---|---|---:|---|---|
| `restart/skinny/COMPILER.md:645-658` | HM hierarchy inversion | Move Algorithm-W under `passes::layout::types` now; V1 adds bidirectional/DK13/CSP siblings and layout wrapper calls HM as subroutine. | 80-150 LOC wrapper/path change | Current `passes/src/types` budget path makes this less mechanical. | MECHANICAL after P6 |
| `restart/skinny/COMPILER.md:94-113` | Host-fn-free JSON | V1 adds `@host fn` and `CallHost`; skinny keeps direct decode only if microbench bounds cost. | 200-400 LOC plus bench row | Without microbench, graduation can reveal hidden overhead. | MECHANICAL after P2 |
| `restart/skinny/WORKSPACE.md:328-342` | parse-that-regex layout | V1 inherits promoted `hir/`, `nfa/`, `dfa/`, `vm/`, `literal/`. | 0 LOC beyond current layout | Trivial. | MECHANICAL |
| `restart/skinny/WORKSPACE.md:79-87` | HM-only `passes` budget | Add DK13/GADT/CSP under layout without rewriting `algorithm_w`. | 3,000-5,000 LOC V1 growth | Requires current HM file path to be nested under layout. | MECHANICAL after P6 |
| `restart/skinny/WORKSPACE.md:181-184` | `wasm = false` flag | V2 flips/adds backend columns; V1 rejects `wasm = true`, accepts false. | 20-50 LOC schema rule | INDEX omits this fifth deviation. | MECHANICAL after INDEX row |
| `restart/skinny/BENCH.md:1189-1192` | Separate runtime JSON crates | Graduation would have to move crates back into `runtime`. | 500-900 LOC churn if left | Preempt by aligning BENCH with WORKSPACE now. | ANTI-MECHANICAL until P4 |

### Per-target supplement - INDEX

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/INDEX.md:3` | Five-quadrant prior device | The top line states one grammar, dual track, 2-4 weeks, 31,400 handwritten LOC. | Gives a compact contract for the suite. | The phrase `~10 partial crates` conflicts with WORKSPACE's exact ten crates plus `xtask`. | Treat the workspace as authoritative; index can say "10 crates plus xtask." | REINVENT |
| `restart/skinny/INDEX.md:17-27` | Outcome summary | The index states three high-level outcomes before BENCH expands them. | Good reader entry point. | It uses sonic-rs only while BENCH uses `S = min(sonic-rs, simd-json)`. | Mirror BENCH's threshold notation or explicitly say this is mnemonic. | REINVENT |
| `restart/skinny/INDEX.md:31-41` | Not-tested table | Omissions name V1 owners. | Strong carry discipline. | Cost-model/egraph row says Tranche C/H body; ARCH puts cost-driven rewrites under H body at `restart/ARCHITECTURE.md:1461-1466`. | Split legality/normalization receivers from cost-driven receiver. | REINVENT |
| `restart/skinny/INDEX.md:47` | Host-fn-free invariant | All quadrants assume host-fn-free JSON. | Makes the deviation visible. | ARCH JSON row has host fns at `restart/ARCHITECTURE.md:1597`. | Add "deliberate deviation, measured by BENCH host-call probe." | REINVENT |
| `restart/skinny/INDEX.md:50` | Single-plan extraction | The index says no CSP/egraph/cost model selection. | Honest cut. | It can become Lens L masking without alternate-plan bound. | Add pointer to BENCH alternate-plan stub once amended. | REINVENT |
| `restart/skinny/INDEX.md:54-63` | Open contradictions | Four deviations are listed. | Good audit source. | The skinny hardening prompt requires five, including `wasm = false`; INDEX omits it. | Add the metadata flag or remove it from Lens N's mandatory list. | REINVENT |
| `restart/skinny/INDEX.md:65-77` | Decision protocol | Implementation order ends in RESULTS and GO/NO-GO. | Falsifiable in shape. | It says if GO then A.W0 dispatches; BENCH has conditional GO classes that hold F. | Route conditional outcomes explicitly through amendment or partial dispatch. | REINVENT |
| `restart/skinny/INDEX.md:79` | Falsifiability claim | The closing line asserts measurable/falsifiable. | Good posture. | Matrix defects currently weaken the assertion. | Keep after P7; until then it is aspirational. | REINVENT |

### Per-target supplement - SUBSTRATE

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/SUBSTRATE.md:7` | Substrate-side risk collapses | JSON parity is said to collapse substrate risk for all tranches. | Strong premise. | It overreaches for CSS layout and Sheets Pratt/host paths. | Rewrite to collapse "JSON-class tape/SIMD substrate risk." | REINVENT |
| `restart/skinny/SUBSTRATE.md:26-66` | 16-byte token | The token packs kind, flags, offsets, and payload/skip. | Load-bearing SOTA hypothesis. | 32-bit offsets cap hot path at <=4 GiB; payload/skip branch cost is unknown. | Bench perturbation row is enough; keep the design. | KEEP |
| `restart/skinny/SUBSTRATE.md:70-98` | `Tape` carries payload arena | The substrate keeps arena shape even for empty JSON hot path. | Mechanical V1 graduation. | Empty arena can still allocate if constructed as `Vec`. | Add bench allocation counter or use zero-capacity constructor proof. | REINVENT |
| `restart/skinny/SUBSTRATE.md:113-119` | Lifetime discriminant | `'doc`/`'input` discriminate borrowed/arena/owned forms. | Honors Lock 9. | `parse_owned` is omitted later. | State facade wrapper story or set metadata `owned_document=false` in skinny. | REINVENT |
| `restart/skinny/SUBSTRATE.md:167-198` | Payload arena policy | JSON scalars borrow or parse lazily; arena stays empty. | SOTA-aligned with lazy parse. | Contradicted by COMPILER decoded payload rows. | P1 resolves. | REINVENT |
| `restart/skinny/SUBSTRATE.md:202-258` | SIMD scan contract | Exact structural and prefilter string-content routes are named. | Good split. | Compiler emits structural `Prefilter`. | Align BIR mode. | REINVENT |
| `restart/skinny/SUBSTRATE.md:260-275` | Scalar parity hash | SIMD/scalar mismatch demotes row in bench. | Strong correctness guard. | Runtime trusts SIMD after corpus validation only; production corpus drift still possible. | Accept for skinny; V1 can extend corpus. | KEEP |
| `restart/skinny/SUBSTRATE.md:290-368` | Direct-to-struct overlay | Typed views point into tape. | Honors Lock 1. | Sketch has `cursor_at(self.cursor.tape, 1)` without `self.` in code; illustrative only. | Non-blocking spec sketch issue. | KEEP-MODIFY |
| `restart/skinny/SUBSTRATE.md:393-408` | Identity proof | Public nodes have `(TapeId, index, payload_class)`. | Matches ARCH identity invariant at `restart/ARCHITECTURE.md:1393-1401`. | Bench parity must ignore `TapeId`. | BENCH says modulo `TapeId` at `restart/skinny/SUBSTRATE.md:520`; fine. | KEEP |
| `restart/skinny/SUBSTRATE.md:472-488` | Omission table | Omissions are mostly parse-throughput orthogonal. | Good Lens L base. | `OwnedDocument` and arena pressure interact with allocation profile if benchmark returns owning wrappers. | Keep only if BENCH parse signature stays borrowed. | KEEP-MODIFY |
| `restart/skinny/SUBSTRATE.md:500-503` | `build_tape_for_json` takes arena | The parity API takes `PayloadArena`. | Track 2 has same substrate. | Signature suggests borrowed external arena while `Tape` owns `PayloadArena` at lines 72-90. | Clarify ownership: builder borrows mutable arena then `finish` moves/seals it. | REINVENT |

### Per-target supplement - COMPILER

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/COMPILER.md:31-80` | Host-free grammar sketch | JSON grammar is stripped to regex/literal/ref forms. | Keeps compile path tiny. | Early authority claim about ARCH §12.1 is corrected only later. | Rewrite source authority inline. | REINVENT |
| `restart/skinny/COMPILER.md:82-88` | Regex routes | Number/string/ws routes use lazy DFA or take-while. | Concrete. | Number/string rows eagerly write decoded payloads. | Match SUBSTRATE lazy policy. | REINVENT |
| `restart/skinny/COMPILER.md:94-113` | Host-fn decision | Removes host/csp-solver path from skinny. | Big scope cut. | Dispatch cost is asserted below noise. | Host-call probe required. | REINVENT |
| `restart/skinny/COMPILER.md:135-145` | Grammar IR skipped variants | Per-skip impact table exists. | Good Lens L structure. | `Call(kind: Map)` says moving scalar decode to access time favors skinny, but earlier regex rows eagerly decode. | Resolve payload policy. | REINVENT |
| `restart/skinny/COMPILER.md:181-199` | Exercised BIR variants | Lists 14 variants and JSON sites. | Strong codegen contract. | `SimdScan` mode says Prefilter for structural alphabet. | Change to Exact if SUBSTRATE remains authoritative. | REINVENT |
| `restart/skinny/COMPILER.md:212-223` | BIR construction discipline | Invariants include no Grammar IR imports by lowerers. | Honors Lock 5. | VM replay not enforced. | VM omission is orthogonal to throughput. | KEEP |
| `restart/skinny/COMPILER.md:244-256` | HM omitted mechanisms | DK13/GADT/CSP cuts are JSON-faithful. | Correct premise. | It should name V1 grammars where they load-bear. | Add CSS L4/Sheets/BBNF-self caveat in-row. | KEEP-MODIFY |
| `restart/skinny/COMPILER.md:315-336` | `LayoutFacts` pass-through | Public boundary name is preserved. | Good mechanicality. | File path budget later says `passes/src/types`, not layout-nested. | P6 resolves. | REINVENT |
| `restart/skinny/COMPILER.md:381-393` | Skinny pipeline | Eight-phase compile path. | Good small pipeline. | Source load/parser row imports V1 parser/parse-that despite WORKSPACE skip. | Amend to partial parser path. | REINVENT |
| `restart/skinny/COMPILER.md:395-403` | Skipped phases | Recognizer/egraph/cost omissions are documented. | Good. | "None for JSON" overstated for cost extraction. | Alternate-plan stub required. | REINVENT |
| `restart/skinny/COMPILER.md:553-566` | Regen check | Emitted source equality and BIR snapshot are specified. | Honors Lock 6. | Command uses `bbnf-cli`, while WORKSPACE replaces it with `xtask`. | Use one command family. | REINVENT |
| `restart/skinny/COMPILER.md:598-636` | Compile/test loop | Build, regen, parity, bench sequence is explicit. | Useful for implementation. | Bench name `sota_json` conflicts with BENCH `json_parity`/`simd_scan`. | Align command names. | REINVENT |

### Per-target supplement - BENCH

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/BENCH.md:26-113` | Dual-track contract | Track 1/2 deltas diagnose codegen vs substrate. | Load-bearing skinny design. | Crate paths contradict WORKSPACE. | P4 resolves without losing dual-track. | REINVENT |
| `restart/skinny/BENCH.md:115-127` | Track 2 is not a strawman | Checklist guards cheating. | Good falsifiability. | Human review plus greps are insufficient alone. | Add typed API proof. | REINVENT |
| `restart/skinny/BENCH.md:130-232` | Competitor baselines | sonic-rs, simd-json, serde_json are specified. | Anchored to Lock 8. | sonic-rs unchecked/feature mismatch and simd-json API uncertainty remain. | Refresh or match baselines. | REINVENT |
| `restart/skinny/BENCH.md:236-291` | Corpus sourcing | Fixtures include URLs, sizes, hashes. | Reproducible. | URLs use GitHub web paths in prose and raw paths in cheat sheet. | Manifest should bind raw URL and SHA after acquisition. | KEEP-MODIFY |
| `restart/skinny/BENCH.md:310-338` | Parity oracle | Track outputs are canonicalized and compared. | Correctness gate. | Canonical serialization can hide structural/tape identity divergences. | Also compare token streams or `(kind, span, payload_class)` rows. | REINVENT |
| `restart/skinny/BENCH.md:342-418` | SIMD microbench | Structural scan floor is separated. | Good substrate diagnosis. | Only twitter input. | Keep for scan GB/s; parse matrix covers other corpora. | KEEP |
| `restart/skinny/BENCH.md:422-509` | Repro schema | Missing fields fail before threshold. | Strong Lens M pass. | RESULTS template omits full per-row metadata table. | Link or include metadata paths in RESULTS. | KEEP-MODIFY |
| `restart/skinny/BENCH.md:528-541` | Matrix | Outcome set includes GO, conditional, invalid, NO-GO. | Falsifiable in intent. | Shadowed D/E and G/H; SIMD fail exits green. | P7 resolves. | REINVENT |
| `restart/skinny/BENCH.md:660-685` | Iteration setup | simd-json clone is untimed; bbnf parses by `&str`. | Fairer comparison. | `std::str::from_utf8` in bench sketch at lines 769/773 is inside timed closure. | Precompute `&str` outside timed path or charge competitors equivalent validation. | REINVENT |
| `restart/skinny/BENCH.md:703-742` | Cargo.toml | Bench manifest is concrete. | Useful. | Missing `mimalloc` despite allocator drift mitigation. | Add feature/dependency. | REINVENT |
| `restart/skinny/BENCH.md:840-870` | Gate binary | Classifies outcomes and writes RESULTS. | Good automation. | Conditional outcomes return success. | Separate "CI regression pass" from "dispatch authorized." | REINVENT |
| `restart/skinny/BENCH.md:987-996` | Memory report-only | Peak RSS is recorded, not gated. | Keeps skinny throughput-centered. | A 10x memory pass can still over-predict V1 viability. | Add warning threshold in RESULTS even if no NO-GO. | KEEP-MODIFY |

### Per-target supplement - WORKSPACE

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/skinny/WORKSPACE.md:22-38` | Exactly ten crates plus xtask | Workspace closes scope. | Strong buildability discipline. | BENCH's runtime-json crates violate it. | P4 resolves. | REINVENT |
| `restart/skinny/WORKSPACE.md:40-57` | Skipped crates and shims | Deletions are explicit. | Good skinny cut. | `host_stubs` exists despite host-fn-free; this is compatibility, not feature. | Keep with <=50 LOC cap. | KEEP |
| `restart/skinny/WORKSPACE.md:59-77` | LOC table | Per-crate budget totals 31,400. | Strong gate. | Needs command enforcement. | Add LOC gate. | REINVENT |
| `restart/skinny/WORKSPACE.md:79-87` | `passes` contradiction | Budget overrun becomes scope signal. | Good falsifiability. | It lacks automatic measurement. | Add command. | KEEP-MODIFY |
| `restart/skinny/WORKSPACE.md:89-227` | Workspace TOML | Profiles and metadata are concrete. | Useful scaffold. | Competitor deps appear under workspace deps; BENCH says they should not. | P8 resolves. | REINVENT |
| `restart/skinny/WORKSPACE.md:163-184` | JSON runtime metadata | `owned_document = true`, `wasm = false`. | V1-shaped metadata. | `parse_owned` omitted in SUBSTRATE. | Clarify cold wrapper or set owned false for skinny. | REINVENT |
| `restart/skinny/WORKSPACE.md:237-391` | Directory layout | Child counts are checked by prose. | Honors Lock 13. | `xtask` single-file exception is untested. | Accept as dev-only exception. | KEEP |
| `restart/skinny/WORKSPACE.md:393-440` | Build/test commands | Developer loop is copy-pasteable. | Good. | Bench command names differ from BENCH. | Align command names and profiles. | REINVENT |
| `restart/skinny/WORKSPACE.md:486-506` | Shim policy | Shims migrate or become crates at 500 LOC. | Excellent buildability guard. | Needs enforcement. | Add LOC gate. | KEEP-MODIFY |
| `restart/skinny/WORKSPACE.md:508-525` | Migration parity | Every crate has a V1 destination. | Good Lens N foundation. | Does not list closure cost per deviation. | Add a small deviation closure table. | KEEP-MODIFY |
| `restart/skinny/WORKSPACE.md:553-574` | What skinny omits | Omissions and risks are named. | Good prior scope. | Host fns row says JSON has none, conflicting with ARCH. | Mark as deliberate skinny deviation, not fact about V1 JSON. | REINVENT |
| `restart/skinny/WORKSPACE.md:576-591` | Closure conditions | Buildability conditions are explicit. | Strong final checklist. | No condition for bench classifier correctness. | Add "BENCH matrix classifier has no shadowed rows." | REINVENT |

## §12 Punch list

| # | Target | Surgery | Source verdict | Owner | Scope | Lanes |
|---:|---|---|---|---|---|---|
| P1 | `restart/skinny/SUBSTRATE.md:167-196`, `restart/skinny/COMPILER.md:76-87`, `restart/skinny/COMPILER.md:449-450` | Choose one JSON payload policy. Recommended: preserve SUBSTRATE lazy policy; remove parse-time decoded `ArenaHandle`/`f64` shape facts; add a bench counter asserting zero arena writes on Track 1 and Track 2 hot paths. | REINVENT / MASKING | substrate + compiler + bench | Narrow | 3, 9, L |
| P2 | `restart/skinny/COMPILER.md:94-113`, `restart/skinny/BENCH.md` | Add a `CallHost` vs direct decode microbench or one-host-fn JSON variant. Gate: if registry dispatch changes parse median by more than a named threshold, RESULTS marks host-fn-free as MASKING and lowers V1 SOTA-beat probability. | REINVENT / MASKING | compiler + bench | Narrow | 4, 8, L, N |
| P3 | `restart/skinny/COMPILER.md:399-403`, `restart/skinny/COMPILER.md:574-575`, `restart/skinny/WORKSPACE.md:562` | Add an alternate-plan stub bench without egraph: structural-index plan vs scalar recursive descent, and dispatch table vs direct `match`. Use it only to bound the cost-driven-rewrite tail. | REINVENT / MASKING | compiler + bench | Narrow | 4, 8, 9, L |
| P4 | `restart/skinny/BENCH.md:42-83`, `restart/skinny/BENCH.md:1189-1192`, `restart/skinny/WORKSPACE.md:24-38` | Remove `crates/runtime-json` and `crates/runtime-json-handcoded` from BENCH. Track 1 lives under `runtime/src/grammars/json`; Track 2 lives under `bbnf-bench/src/track2/json.rs` or another counted bench module. | REINVENT / ANTI-MECHANICAL | bench + workspace | Narrow | 1, 3, 6, N |
| P5 | `restart/skinny/COMPILER.md:407-429`, `restart/skinny/COMPILER.md:433-452` | Fence JSON-specific recognizer and shape tables as skinny-only with a deletion gate, or move them to bench fixtures. Name V1 receiver: `passes::recognizers` miner and `passes::shapes` telemetry miner. | REINVENT | compiler | Narrow | 1, 5, G, K |
| P6 | `restart/skinny/COMPILER.md:347-354`, `restart/skinny/COMPILER.md:645-658`, `restart/skinny/WORKSPACE.md:286-295` | Make the HM checker path mechanical now: `passes/src/layout/types/algorithm_w.rs` or equivalent. State V1 adds DK13/GADT/CSP siblings without rewriting Algorithm-W. | REINVENT / MECHANICAL | compiler + workspace | Narrow | 1, 3, N |
| P7 | `restart/skinny/BENCH.md:528-541`, `restart/skinny/BENCH.md:861-870` | Fix classifier ranges and exits: D excludes E; H is not shadowed by G; SIMD throughput failure is non-green or promotes to F/G; conditional outcomes do not authorize dispatch in CI. | REINVENT | bench | Narrow | 4, M |
| P8 | `restart/skinny/BENCH.md:1329-1347`, `restart/skinny/WORKSPACE.md:137-139`, `restart/skinny/WORKSPACE.md:591` | Align competitor pins. Keep exact pins only in `bbnf-bench` dev-dependencies; match sonic-rs unchecked anchor or refresh baselines in RESULTS; bench both simd-json borrowed/owned if `S` uses simd-json. | REINVENT | bench + workspace | Narrow | 3, 4, H |
| P9 | `restart/skinny/WORKSPACE.md:66`, `restart/skinny/BENCH.md:1022-1031` | Add verbatim diagnostics for unsupported directives and conditional bench outcomes. Example: `BBNF-DIRECTIVE-NOT-IN-SKINNY: @host fn is outside the JSON skinny; tranche D restores host functions.` | REINVENT | workspace + bench | Narrow | 7 |
| P10 | `restart/skinny/BENCH.md:960-966`, `restart/skinny/WORKSPACE.md:61-77` | Add skinny-local LOC gates: handwritten total <=31,400, generated JSON <=4,000, Track 2 <=500 if moved into `bbnf-bench`. Defer only nine-grammar generated scale to V1. | REINVENT | workspace + bench | Narrow | 6, I |

## §13 Final readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> The skinny suite is directionally sound: one JSON grammar, one Rust backend, a tape/direct substrate, and dual-track measurement are the right prior-validation device for the V1 SOTA premise. The dominant result is Lens L plus Lens M: three cuts currently mask V1 cost rather than merely omitting orthogonal machinery, and the threshold matrix has classifier defects that weaken NO-GO authority. Lens N is mostly favorable once BENCH stops inventing per-grammar runtime crates and the HM checker path is made layout-nested now. The amendments are narrow because they add probes, align paths, and tighten gates; they do not require re-authoring the skinny quadrants.
>
> Hereupon: amendment-dispatch agent for P1-P10, then SK-V2 verification rerun before skinny implementation dispatch per `restart/skinny/INDEX.md` §"Decision protocol".
