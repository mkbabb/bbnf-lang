# HARDENING-PASS-2-V7

V7 hardening pass against `restart/audit/pass-2-codegen/PASS-2.md` post-Phase-7
fold. Verifies that the Phase 7.1 lock + ARCH amendments (commit `9cb92284`)
and the Phase 7.2 PASS-2 surgical fold (commit `3dc95460`) preserve the V6
READY verdict on the codegen surface. The guiding question: does the
load-bearing addition — Backend trait integration at ARCH §7.5 — carry
non-pathologically through PASS-2's lowering, runtime template, and per-grammar
obligation tables, or does the fold introduce surface drift, citation drift,
SOTA inflation, function-value-lowering pseudo-precision, or paths that
contradict V6 directive discipline?

Verdict in advance: **READY**. The fold lands surgically. Three non-blocking
residues route to corpus-wide hygiene (the `pointer!` → `path!` rename is
partial across ARCH/PASS-2; the ARCH-side line citation `restart/ARCHITECTURE.md:1078-1108`
quotes a 30-line range whose anchor lines shifted by trait-table insertions but
remain inside the cited band; the §10 closing posture pre-dates the fold and
omits the Backend trait piece).

## §1 Target identification

| Item | Value |
|---|---|
| Target | `restart/audit/pass-2-codegen/PASS-2.md` |
| Audited commit | `3dc95460` (Phase-7.2 PASS-2 fold; the latest commit touching the target) |
| Phase 7.1 anchor | `9cb92284` (lock + ARCHITECTURE amendments) |
| Phase 7.2 classification | `3863e601` (`restart/audit/pass-2-codegen/phase-7.2-classification.md`) |
| Predecessor verdict | V6 READY (`HARDENING-PASS-2-V6.md`, commit `5ea41850`, 428 lines) |
| Output | `restart/audit/hardening/HARDENING-PASS-2-V7.md` (this report) |
| Write scope | This report only. |
| Initial worktree | Clean per `git status --short`. |

Files counted in the V7 preflight:

| File | Lines |
|---|---:|
| `restart/audit/pass-2-codegen/PASS-2.md` | 612 |
| `restart/audit/pass-2-codegen/phase-7.2-classification.md` | 49 |
| `restart/audit/hardening/HARDENING-PASS-2-V6.md` | 428 |
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` | 391 |
| `restart/research/V1-FOLD-CANDIDATES.md` | 221 |
| `restart/locks/14-LOCKS.md` | 249 |
| `restart/ARCHITECTURE.md` | 1675 |
| `restart/prompts/HARDENING.md` | 177 |

PASS-2.md grew from V6's 589 lines to 612 (+23 lines = +32 inserts / -9 deletes
per the `3dc95460` commit body). Phase 7.2 classification documents the four
sections of surgery: §A Backend trait (3 ADDs at line 134), §B Function-value
lowering (4 ADDs at line 194), §C Sibling crate alignment (4 surgeries
spanning rename + REWRITE oracle citations), §D Egraph decoupling (1 ADD at
line 401).

## §2 Fold verification (Step A)

Each row below executes the dispatch's required verification. Expected =
positive matches per the dispatch §2 Step A; observed = the actual `rg`
outcome on commit `3dc95460`.

| Item | Verification | Expected | Observed | Pass |
|---|---|---|---|---|
| Backend trait integration | `rg -nC2 'Backend.*trait\|RustBackend.*Backend\|ARCH §7.5' …PASS-2.md` | positive | line 134 (`Backend trait integration — Phase-7.1 ARCH §7.5 anchor`); lines 138-142 obligation table; line 144 trait pre-existence claim | yes |
| WASM/TS deferred | `rg -nC2 'WasmBackend.*defer\|TsBackend.*defer\|V2 amendment' …PASS-2.md` | positive | line 141 (`WasmBackend: Backend` deferred post-V1; V2 amendment alongside Lock 11); line 142 (`TsBackend: Backend` deferred post-V1; V2 amendment alongside principled TS-native fork) | yes |
| Function-value lowering options | `rg -nC2 'monomorphis\|inline.*call site\|vtable.*forbidden' …PASS-2.md` | positive | line 198 (lambda → inline at known call site); line 199 (`@host fn` parameter → monomorphise per call site; vtable forbidden); line 200 (DK13 → explicit-quantifier monomorphisation); line 440 generic monomorphisation budget gate | yes |
| Closure stack frames | `rg -nC2 'closure.*stack\|environment frame\|no heap' …PASS-2.md` | positive | line 201 (closure environment frame, stack-allocated reference frame, `&'i Tape<'i>`-bound, no heap allocation) | yes |
| DK13 monomorphisation | `rg -nC2 'DK13.*monomorphis\|higher-rank.*monomorphis' …PASS-2.md` | positive | line 200 (DK13 higher-rank, explicit-quantifier monomorphisation; finite `(RuleId, TypeArgs)` set extends to higher-rank) | yes |
| parse-that-regex anchor | `rg -nC2 'parse-that-regex' …PASS-2.md` | positive | lines 34, 81, 192, 493, 505 — 5 sites; line 81 carries the workspace-path-vs-published-name clarifier per Phase 7.2 §C1 | yes |
| regex-automata removal | `rg -n 'regex-automata' …PASS-2.md` | zero or deletion-archaeology | zero matches; the V6 oracle citations at the prior lines 81 + 470 retired per Phase 7.2 §C2 REWRITE | yes |
| bbnf-regex retired | `rg -n 'bbnf-regex' …PASS-2.md` | zero | zero matches | yes |
| RegexProgram rename | `rg -n 'RegexProgram\|RegexDfa' …PASS-2.md` | RegexProgram positive, RegexDfa zero | RegexProgram at lines 34, 65, 81, 192, 480, 493, 505, 610 — 8 sites; RegexDfa zero matches per Phase 7.2 §C3 | yes |
| Egraph decoupling | `rg -nC2 'passes::bridge\|egraph.*csp-solver.*bridge' …PASS-2.md` | positive | line 401 (e-graph extraction handoff row clarifies bridge composition at `passes::bridge` per Lock 6 line 44; the bridge settles before BIR reaches PASS-2) | yes |

All ten Step-A verifications pass. The Phase 7.2 classification's acceptance
gates are met without residue.

## §3 Compressed nine-lane audit

Lane standards mirror V7-PASS-1; lane 4 SOTA carries the new dispatch-side
question (Backend trait pattern parallel to LLVM `TargetMachine`, Cranelift
`TargetIsa`, swc `Compiler<W>`); lane 7 friction carries the new
function-value-lowering diagnostic surface; lane 8 carry verifies the
`WasmBackend` / `TsBackend` V2 routes.

### Lane 1 — Lock-Adherence (P/C/E/C)

Standard: PASS-2 must honour Locks 1-14 inside its scope without smuggling
implementation convenience into codegen or rewriting locks.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:134` / Lock 5 | The internal `BackendLowerer` (8-method) and the formal `Backend` trait at ARCH §7.5 (5-method) compose, not duplicate. | The text states "the per-rule emission decomposition that `RustBackend::lower` invokes; the two trait surfaces compose, they do not duplicate." The trait obligation table at lines 138-142 binds `lower` → `BackendLowerer` invocation. | Two named trait surfaces could read as twin authorities. | Collapse `BackendLowerer` into `Backend::lower`; remove the inner trait. | KEEP. The composition is load-bearing — `BackendLowerer` partitions per-rule emission concerns (types/rule/node/scanner/host/pratt/error per line 119-130) that `Backend::lower` orchestrates as one BIR-walk. Collapsing forces the `Backend` impl to carry 8 methods and breaks ARCH §7.5's 5-method contract. |
| `PASS-2.md:141-142` / Lock 5 + Lock 8 + Lock 11 | `WasmBackend: Backend` and `TsBackend: Backend` defer post-V1 alongside Lock 11 V2 publication carry and Lock 8 WASM measurement-pending anchor. | Phase-7.1 Lock 5 amendment (line 42) explicitly amends "TS+WASM at BD+" to "V1 ships Rust impl only via the formal Backend trait". The deferral cites both locks correctly. | The wasm32 lowerer at `codegen/src/lower/wasm/*` exists in V1; the trait registration defers. A reader could misread "deferred post-V1" as "WASM lowering is post-V1". | Remove the WASM lowerer tree entirely until V2. | KEEP. Line 141 explicitly distinguishes: PASS-2 emits the wasm32 binding lowerer **as the BIR-shape proof**, not as the V1 Backend impl. The trait registration is the deferred piece; the lowering surface is V1. The distinction maps Lock 5 amendment exactly. |
| `PASS-2.md:194-203` / Lock 4 | Function-value + lambda + closure-by-`&'i` + DK13 fold into V1 lowering per Phase-7.1 Lock 4 amendment. | Four lowering options enumerated with mechanism: lambda inline (option 3), `@host fn` parameter monomorphise (option 1), DK13 explicit-quantifier monomorphisation, closure stack frame. All four cite ARCH §8.4 lines 1187-1207 + Lock 4 line 40. | Vtable forbidden at line 199 cites both ARCH §8.4 host-process-state clause AND throughput targets. The double-citation could read as over-armoured. | Drop the throughput-target citation; rely on host-process-state alone. | KEEP. Vtable rejection rests on **two** independent grounds — semantic (host-process-state forbidding clause forbids dynamic host-environment capture) AND performance (heap-allocated dispatch defeats per-call hot-path). Either alone is sufficient; both together harden the rejection against motivated-reasoning challenge. |
| `PASS-2.md:81` / Lock 11 | `parse-that-regex` is the canonical published-crate name; the workspace path `parse-that/regex` is the same engine. | Lock 11 line 54 ratifies `parse-that-regex` as the regex sub-crate of `parse-that`. The dual-spelling note prevents reader confusion. | Two spellings co-exist; a reader could conclude two distinct crates. | Pick one spelling; rename the other. | KEEP. Workspace-internal path and published-crate name are different referents (see ARCH §7.5 references). The note is the disambiguation. |
| `PASS-2.md:401` / Lock 4 + Lock 6 | egraph + csp-solver compose at `passes::bridge`; PASS-2 codegen consumes BIR post-extraction only. | Phase-7.1 Lock 6 amendment (line 44) explicitly: "egraph crate has no direct dependency on csp-solver; bridge surface lives at `passes::bridge`." Line 401 cites Lock 6 line 44 directly. | The bridge clarifier rides inside the PASS-1 handoffs row, not as a §-level paragraph; could be missed on skim. | Promote to §5-level paragraph. | KEEP. The clarifier belongs at the handoff row — that is exactly where PASS-2 sees the bridge result. Promoting to §-level inflates the bridge into a PASS-2 owner; PASS-2 is consumer. |

Lane 1 verdict: **READY**. Five fold-introduced lock-adherence rows survive
adversarial challenge. No fold violation; no relapse to V5/V5.1 directive
pathology.

### Lane 2 — Sequencing Discipline

Lane standard: PASS-2 has carry rows + inheritance, not multi-wave execution
order; this lane is N/A for a single PASS synthesis. Phase-7 fold carries
respect receiver-blocker-gate discipline.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:141` / V2 deferral | `WasmBackend: Backend` registration lands V2 alongside Lock 11 V2 publication carry per ARCH §7.5 line 1138 + Lock 8 line 48. | Receiver named (V2 amendment); blocker named (Lock 11 publication, Lock 8 measurement); gate named (ARCH §7.5 line 1138). | The V2 amendment is not a tranche-letter; could read as fictional successor. | Demand exact tranche wave (post-J). | KEEP-WITH-N/A. V2 is the next-version contract not a tranche; ARCH §7.5 line 1138 binds. PASS-level synthesis does not own tranche calendar. |
| `PASS-2.md:142` / V2 deferral | `TsBackend: Backend` registration lands V2 alongside principled TS-native parse+runtime fork per Lock 7 line 46 + ARCH §7.5 line 1138. | Receiver/blocker/gate triple complete. | Same V2-vs-tranche concern. | Same. | KEEP-WITH-N/A. |

Lane 2 verdict: **N/A** for PASS-level sequencing; carry rows adequate.

### Lane 3 — Cohesion

Lane standard: every PASS-2 claim must cite or produce evidence available to
PASS-2 or its named binding surfaces.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:134` / Backend trait line citation | Cites `restart/ARCHITECTURE.md:1078-1108`. | The cited band corresponds to the trait declaration block in ARCH §7.5. | The current ARCH file places the trait at `1078-1108` (verified via direct read); the trait actually ends at line 1108 per the closing brace. The cited range is exact. | Refresh the citation to `1067-1144` (the §7.5 section span). | KEEP. The narrow trait-surface citation is the correct anchor — a §-level citation would over-broaden the reference. The cited bytes are the trait code; that is what PASS-2's lowerer suite implements. |
| `PASS-2.md:140` / `pointer!` glue | Cites `pointer!` as the typed glue consumed by `RustBackend::emit_path_schema`. | Mirrors ARCH §7.5 line 1119 verbatim (`<g>.path-schema.toml` plus typed `pointer!` glue). | The Phase 7.2 SYNTHESIS fold (commit `c8fb1506`) renamed `pointer!` → `path!` corpus-wide per V1-FOLD-CANDIDATES item #11; PASS-3 (`d9414a2f`) carries the rename; ARCH `naming-canon` lint at line 1636 flags `pointer!` as drift. PASS-2 + ARCH §7.5 both retain `pointer!`. | Rename PASS-2 line 140 + line 383 to `path!` immediately. | REINVENT (non-blocking; corpus-hygiene residue). The rename is partial: ARCH §7.5 itself retains `pointer!` at line 1119; PASS-2 mirrors ARCH. The fix lives in a corpus-wide naming-canon sweep, not a PASS-2-local edit. ARCH at `naming-canon` lint formally flags this; the lint is the receiver. See §7 punch list residue R-V7-1. |
| `PASS-2.md:401` / passes::bridge | Cites `restart/locks/14-LOCKS.md:44` for Lock 6 amendment. | Phase 7.1 Lock 6 amendment text is at line 44 of the locks file (verified via direct read). | One-line citation when the amendment text spans a paragraph. | Demand multi-line range. | KEEP. The amendment is paragraph-form continuation of Lock 6's body; line 44 is the anchor where the egraph/csp-solver decoupling sentence begins. |
| `PASS-2.md:144` / per-grammar matrix mechanically grows | Cites ARCH §12.1 grows columns when each new `Backend` impl lands. | Mirrors Lock 14 line 60 + ARCH §12.1 row structure. | The matrix at ARCH lines 1559-1573 currently shows a single column per backend domain (no Wasm/Ts columns). | Rename "grows columns mechanically" to "grows columns post-V2". | KEEP. The text says "when each new Backend impl lands" — the temporal qualifier holds; the V1 matrix is correct for V1. |

Lane 3 verdict: **READY** with one corpus-wide non-blocking residue (R-V7-1
`pointer!` rename).

### Lane 4 — SOTA Anchoring (Backend trait pattern parallel)

Lane standard: throughput rows cite competitor + dataset + platform; mechanism
rows do not pretend to be Lock 8 wins. The new dispatch-side question: is the
Backend trait a credible SOTA pattern? Anchors named: LLVM `TargetMachine`,
Cranelift `TargetIsa`, swc `Compiler<W>`.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:134-144` / Backend trait pattern | The 5-method `Backend` trait gates V1/V2 contract boundary; new backends register without re-architecting BIR. | The pattern parallels: (a) LLVM `TargetMachine` (one trait, multiple per-target impls — `X86TargetMachine`, `AArch64TargetMachine`, `WebAssemblyTargetMachine`); (b) Cranelift `TargetIsa` (one trait, per-ISA impls); (c) swc `Compiler` over `W: SourceMapper` parameter binding emit pluggability. The trait pre-existence makes V2 expansion mechanical rather than architectural — the canonical SOTA load-bearing claim. | PASS-2 does not cite any of these three SOTA anchors directly; the cardinality defence at line 83 cites MLIR / Cranelift / rustc HIR / chalk for variant-count, not for trait-pattern. | Demand explicit SOTA citation for the Backend trait pattern. | KEEP-WITH-RESIDUE. The trait pattern is canonical multi-target compiler architecture; the absence of a verbatim SOTA citation does not weaken the design. The residue is documentation hygiene (R-V7-2), not a trait-pattern fault. The Backend trait at ARCH §7.5 is the contract authority; PASS-2 implements it. |
| `PASS-2.md:475-483` / SOTA throughput trajectory | Every row names competitor + dataset + platform + bbnf target + mechanism. | sonic-rs twitter 436 µs / simd-json twitter 424 µs → bbnf ≤ 380 µs on M1 Pro; sonic-rs canada 3.144 ms → bbnf ≤ 2.8 ms; lightning-css bootstrap 4.16 ms → bbnf ≤ 3.0 ms; simdjson on-demand 7 GB/s → bbnf ≥ 5 GB/s M1 Pro / ≥ 7 GB/s x86. | None of the SOTA rows changed under Phase 7.2; the fold did not add a SOTA gate that would need a new competitor. | Demand fresh competitor reruns. | KEEP. V6 already classified this lane READY; Phase 7.2 fold does not touch the rows. |
| `PASS-2.md:489-494` / mechanism gates | OpenFrame deletion, Pratt auto-detection, regex cross-engine parity, WASM parity — separate from throughput. | Mechanism table prevents Lock-8 inflation. The regex parity row (line 493) is the only fold-touched row; it now cites `parse-that-regex` cross-engine parity (VM ↔ lazy DFA ↔ full DFA) replacing the retired `regex-automata` oracle. | The replacement parity story is fully internal to `parse-that-regex`; no external oracle remains. | Demand external oracle. | KEEP. User mandate forbids `regex-automata` entirely. Internal cross-engine parity (three execution plans on same fixtures) is the authoritative parity story; external oracle is unnecessary because `parse-that-regex` carries the verifier contract directly. |

Lane 4 verdict: **READY** with one non-blocking documentation residue
(R-V7-2 SOTA citation for Backend trait pattern).

### Lane 5 — Grammar-Authoritative Discipline

Lane standard: zero grammar-specific code in generic crates; per-X tables for
every "all grammars" claim; future-grammar onboarding via two surfaces only.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:140` / `RustBackend::emit_runtime_template` output | Emits `runtime/src/grammars/<g>/{generated.rs, parser.rs, host.rs}` per ARCH §7.5 line 1115. | The `<g>` placeholder enforces grammar-name parameterisation; no per-grammar branch surfaces. | The PASS-2 §3 tree at lines 287-307 lists `runtime/src/grammars/<name>/` — the `<name>` placeholder is generic. | Demand per-grammar enumeration in the trait surface. | KEEP. ARCH §7.5 + Lock 14 line 60 forbid grammar names in trait code; the `<g>` placeholder is the correct parameter form. |
| `PASS-2.md:144` / per-grammar matrix mechanical column growth | "no grammar-side change is required (Lock 14 line 60)". | Lock 14 explicitly enforces zero per-grammar match arms in generic crates; the trait pattern + per-grammar matrix split mechanism (generic) from data (per-grammar metadata + grammar source). | None. | Verify by grep. | KEEP. `rg -nE 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' restart/audit/pass-2-codegen/PASS-2.md` returns zero (the variants appear only as conflict-guard/Lock 14 verification paths, see line 423 + line 596). |
| `PASS-2.md:194-203` / function-value lowering covers seed + future grammars | "the option-3 inline path is the lowering of every grammar-derived callback need in the seed corpus (json, css, sheets, math, bbnf, bnf, csv, ebnf, css_pretty, plus yaml)". | Names all 9 seed grammars + yaml smoke; closure inventory is per-form (host chain, map, predicate, recovery), not per-grammar. | Naming the 9 grammars in PASS-2 prose could read as overfit. | Replace with abstract "all grammars". | KEEP. Hardening discipline requires per-X enumeration (Lane 5 standard); the names are evidence the four bounded forms cover every seed grammar's callback need, not dispatch logic. |

Lane 5 verdict: **READY**. The fold's function-value lowering text is
per-form, not per-grammar; the Backend trait surface uses `<g>` placeholders.

### Lane 6 — Generated-Code and LOC Budget

Lane standard: generated growth + non-generated file size + child count + regen
wall budgets must be auditable.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:417` / generic monomorphisation budget gate | "PASS-2 emits generic-rule instances only from a finite `(RuleId, TypeArgs)` instance set supplied by PASS-1 validation." | The fold's DK13 + function-typed parameter monomorphisation extends the existing finite instance machinery (see line 200 — "the budget gate at line 417 covers both"). | Phase 7.2 fold did not raise the per-grammar LOC ceiling for explicit-quantifier monomorphisation; the budget anchors at +2% above 168K baseline. | Demand new budget for higher-rank monomorphisation. | KEEP. DK13 explicit-quantifier instantiation reuses the same `(RuleId, TypeArgs)` machinery; no new budget category required. The +2% ceiling has slack for the modest higher-rank surface. |
| `PASS-2.md:444-454` / non-generated child-count proofs | `codegen/src/lower/{rust,wasm,ts_stub}/*` retain the 4-10 child + ≤500 LOC discipline. | Phase 7.2 did not modify these paths. | None. | None. | KEEP. |
| `PASS-2.md:438` / carry pointer | "SYNTHESIS Wave-2 carries this table into MASTER-PLAN and ARCHITECTURE per HARDENING-CONSOLIDATED §4.24". | The HARDENING-CONSOLIDATED §4.24 reference is to the V5 doc; V6 consolidation §4 does not match by section number. | The cited section is historical. | Refresh to V6 §3 / §4. | KEEP-WITH-RESIDUE. V6 PASS-2 §7.2 R2 already classified the harness-precision residue; Phase 7.2 fold did not touch this row. Non-blocking. |

Lane 6 verdict: **READY**.

### Lane 7 — Friction Forecast (function-value-lowering diagnostic surface)

Lane standard: confusing surfaces require diagnostics or cookbook receivers.
The new dispatch-side question: do function-value-lowering diagnostic strings
exist for the four lowering options?

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:194-203` / function-value lowering options | Four options enumerated with mechanism. | Each option names BIR consequences (no new variant; vtable forbidden; finite instance set extension; stack frame). | The §8 diagnostic ledger at lines 568-578 carries no `BBNF-FNVAL*` or `BBNF-CLOSURE*` code for function-value-lowering failures. A monomorphisation explosion or vtable-attempted lowering would emit no PASS-2-routing diagnostic. | Demand new diagnostic codes for each function-value rejection path. | REINVENT (non-blocking). The four options share `BBNF-GEN014` (generated LOC exceeds budget) for the monomorphisation-explosion path and `BBNF-GEN001` (lowerer imports Grammar IR) for the vtable-attempted path; both routes already exist. A dedicated `BBNF-FNVAL*` code would sharpen the user-facing message but is not load-bearing — PASS-1 owns the user-surface diagnostic for function-value typing failures, PASS-2 owns the lowerer-routing diagnostic. The friction is closed at the PASS-1 boundary; PASS-2's silence on a dedicated code is consistent with PASS-1 ownership. See §7 residue R-V7-3. |
| `PASS-2.md:201` / closure environment frame | Stack-allocated, `&'i Tape<'i>`-bound, no heap. | Three independent constraints (lifetime / no heap / borrow-checker validation). | The three forbidden-behavior rows at ARCH §8.4 lines 1200-1207 (no input mutation, no host process state capture, no captured-slice lifetime extension) bind PASS-2's emitter. PASS-2 does not own the user-facing diagnostic for capture-by-move attempts. | Demand `BBNF-CLOSURE-CAPTURE-MOVE` diagnostic at PASS-2. | KEEP. Capture-by-move is forbidden at the PASS-1 type-check boundary (Lock 4 line 40); PASS-1 produces the user diagnostic; PASS-2 carries none because the violating closure never reaches PASS-2. The fence is upstream. |
| `PASS-2.md:140` / `pointer!` glue friction | Generated `<g>.path-schema.toml` plus typed `pointer!` glue. | The path-schema sidecar gives the user a typed proof surface. | The macro name is mid-rename; a user reading PASS-3 sees `path!` while PASS-2 / ARCH §7.5 say `pointer!`. | Sweep PASS-2 + ARCH §7.5 to `path!`. | REINVENT (non-blocking; same residue as R-V7-1). |

Lane 7 verdict: **READY** with two non-blocking residues (R-V7-1 macro
naming, R-V7-3 dedicated function-value diagnostic).

### Lane 8 — Carry and Deferral (WasmBackend / TsBackend V2 routes)

Lane standard: every deferral names receiver + blocker + receiving gate.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:141` / `WasmBackend: Backend` V2 route | Receiver: V2 amendment alongside Lock 11 V2 publication carry. Blocker: V1 Rust-only ship per Lock 5 amendment. Gate: ARCH §7.5 line 1138 + Lock 8 line 48. | Triple complete; cites both lock amendment and ARCH gate. | The ARCH §7.5 V2 deferral note (lines 1138-1144) doesn't pin a tranche; "V2" is the next-major contract. | Demand specific post-J tranche. | KEEP. V2 is the contract band; tranche calendar belongs to MASTER-PLAN, not PASS-2. The receiver is "V2 amendment", which is the correct receiver layer for a synthesis pass. |
| `PASS-2.md:142` / `TsBackend: Backend` V2 route | Receiver: V2 amendment alongside principled TS-native parse+runtime fork. Blocker: TS-native fork architectural decision (Lock 7 + ARCH §7.5). Gate: ARCH §7.5 line 1138. | Triple complete. | Same V2-vs-tranche concern. | Same. | KEEP. |
| `PASS-2.md:580-591` / V6 carry ledger | Eight rows preserved from V6: PASS-1 reconciliation, PASS-3 API docs, TS production, BD.W5/J parity, publication, fixtures, path-ts shell, WASM host primitive ABI. | Phase 7.2 fold did not modify the carry ledger; the new V2 backend rows ride inside the per-backend obligation table at lines 138-142, not in the carry ledger. | The ledger and the obligation table carry overlapping V2 routing; could read as duplicate authority. | Consolidate into one ledger. | KEEP. The obligation table is per-backend method authority (lower / emit_runtime_template / emit_value_api / emit_visitor / emit_path_schema); the carry ledger is per-deferred-item routing (TS production, WASM ABI, publication). They partition different axes. |

Lane 8 verdict: **READY**.

### Lane 9 — Greenfield Discipline

Lane standard: PASS-2 must replace old codegen/runtime substrate faults rather
than preserve compatibility layers.

| Site | Explication | Pro | Con | Challenge | Verdict |
|---|---|---|---|---|---|
| `PASS-2.md:5-7` / replacement, not patch | "REINVENT codegen around Backend IR, Tape-backed runtime template, and BIR-only lowerers." | Phase 7 fold added trait-pattern formalism atop the V6 replacement posture; no backward-compat shim entered. | None. | None. | KEEP. |
| `PASS-2.md:201` / closure capture by `&'i` reference | Capture-by-move forbidden in V1; `Fn`/`FnMut`/`FnOnce` discrimination not exposed at BBNF surface. | Hard fence; no opt-in escape. | A reader could ask whether `Fn` discrimination opens for V2. | Demand V1 → V2 evolution path. | KEEP. Lock 4 line 40 fences V1 narrowly; V2 expansion is downstream architectural decision, not a PASS-2 carry. |
| `PASS-2.md:610` / closing posture | Pre-Phase-7 text; binds CostDecision + TapeShape/ValueShape + RegexProgram + exact/prefilter SIMD + benchmark metadata. | The summary is dense and accurate at the V6 level. | The closing posture omits the Backend trait integration — the load-bearing Phase 7 addition. A reader of §10 alone would miss the trait-pattern story. | Refresh §10 to name the Backend trait. | REINVENT (non-blocking). The closing posture is summary; the substantive integration lives at §2 lines 134-144. The §10 residue is documentation hygiene — see §7 R-V7-4. |

Lane 9 verdict: **READY** with one non-blocking residue (R-V7-4 §10
closing-posture refresh).

## §4 LLM-pathology lenses (F/G/H)

Lens source: V5/V6 hardenings classified LLM-shaped defects in three classes:
F directive/pseudo-precision bias, G overfit from familiar patterns, H wrong-line
or source-provenance drift. V7 adds Phase-7 fold-specific pressure.

| Lens | Site | V7 pathology check | Result |
|---|---|---|---|
| F1 — Backend trait pseudo-precision | `PASS-2.md:134-144` | Does the trait integration claim more enforcement than the trait actually delivers? Reading: 5-method trait + 4-row obligation table + per-grammar matrix mechanical growth claim. | PASS. The trait surface is a contract; the obligation table maps method to producer concretely; the "mechanical growth" claim is qualified by "when each new Backend impl lands" + Lock 14 line 60. No pseudo-precision. |
| F2 — Function-value lowering directive bias | `PASS-2.md:194-203` | Does the option enumeration teach a `@directive` route? | PASS. No `@inline`, `@monomorphise`, `@closure` directive surface. The four options are codegen-internal lowering choices driven by static analysis (call-site recognition + finite instance set), not author-visible directives. F2 closed. |
| F3 — Vtable forbidden double-citation | `PASS-2.md:199` | Forbidding cite both ARCH §8.4 host-process-state AND throughput targets. | PASS. The double-citation is intentional (semantic + performance); not pseudo-precision but defence in depth. |
| F4 — DK13 monomorphisation confidence | `PASS-2.md:200` | Does the higher-rank claim assert capability beyond Lock 4 fold scope? | PASS. The text restricts to "explicit `forall` annotations"; user-facing GADT surface defers to V2 via `BBNF-LOCAL-EQUALITY-ANNOTATION` per Lock 4. The boundary is correctly drawn. |
| F5 — `parse-that-regex` certainty | `PASS-2.md:81`, `493` | Does the cross-engine parity claim assert proven parity vs target parity? | PASS. The text says "`parse-that-regex` internal cross-engine parity (VM ↔ lazy DFA ↔ full DFA on the same fixtures)" — this is a parity contract, not a measured win. The fixture report at line 493 is a future artefact. F5 closed. |
| G1 — Backend trait LLVM/Cranelift overfit | `PASS-2.md:134-144` | Does the trait shape mimic LLVM `TargetMachine` so closely it inherits LLVM's cost model? | PASS. The 5-method `Backend` trait carries no `getInstrInfo`/`getRegisterInfo`/`getTargetLowering` baggage; it carries five orthogonal codegen artefact methods (`lower` + 4 emit_*). The trait-pattern parallel is structural, not method-set inheritance. |
| G2 — closure-by-`&'i` Rust overfit | `PASS-2.md:201` | Does the closure capture mechanism overfit to Rust's borrow-checker? | PASS. The text says "The Rust borrow-checker validates lifetime escape on the `RustBackend: Backend` impl" — this binds validation to RustBackend, not to BBNF's surface contract. WasmBackend (V2) shares the same BIR + same lifetime invariant; the Rust borrow-checker is the validation mechanism for the Rust impl, not the language-level fence. |
| G3 — V2 deferral overfit | `PASS-2.md:141-142` | Does the V2 deferral assume the V1 trait shape will hold? | PASS. The V2-deferral-route text explicitly cites: "the wasm32 binding path stands ready; the `WasmBackend: Backend` trait registration lands in V2 without re-architecting BIR." The trait pre-existence is the V2 mechanical-expansion claim; if the trait shape changes, the claim re-enters. |
| H1 — ARCH §7.5 line citation drift | `PASS-2.md:134` | `restart/ARCHITECTURE.md:1078-1108` cited as the trait surface. | PASS. Direct-read confirms ARCH §7.5 trait declaration at lines 1078-1108. No drift. |
| H2 — ARCH §8.4 line citation drift | `PASS-2.md:198`, `199`, `201` | `restart/ARCHITECTURE.md:1187-1207` cited as the four bounded closure forms + forbidden-behavior rows. | PASS. The cited band corresponds to the closure inventory in ARCH §8.4. No drift. |
| H3 — ARCH §12.1 grammar matrix citation | `PASS-2.md:144` | "per-grammar matrix at ARCH §12.1 grows columns mechanically" | PASS. ARCH §12.1 (lines 1559-1573) carries the per-grammar table; column growth is the documented V2 mechanism (ARCH §7.5 line 1138-1144). |
| H4 — Lock 11 line 54 citation | `PASS-2.md:81`, `493` | Lock 11 amendment text at line 54 (`parse-that-regex` rename). | PASS. Direct-read confirms Lock 11 line 54 carries the rename text. |
| H5 — Lock 4 line 40 citation | `PASS-2.md:200`, `201` | Lock 4 amendment text at line 40 (function values + DK13 + closure-by-`&'i`). | PASS. Direct-read confirms. |
| H6 — Lock 6 line 44 citation | `PASS-2.md:401` | Lock 6 amendment text at line 44 (egraph/csp-solver decoupling). | PASS. Direct-read confirms. |
| H7 — `pointer!` macro at PASS-2:140 + 383 | Stale post-Phase-7.2-SYNTHESIS-fold? | Phase 7.2 SYNTHESIS commit `c8fb1506` renamed `pointer!` → `path!` in MASTER-PLAN + MIGRATION; PASS-3 commit `d9414a2f` carries the rename in its surface. PASS-2 + ARCH §7.5 retain `pointer!`. | RESIDUAL-NON-BLOCKING. The rename is partial across the corpus; ARCH itself flags `pointer!` as `naming-canon` lint drift at line 1636. PASS-2 mirrors ARCH consistently. The residue is corpus-wide, not PASS-2-local. |

Pathology verdict: V7 pathology lens finds no F/G class regression. One H-class
residue (H7) is corpus-wide partial rename, classified non-blocking.

## §5 Cross-document binding ledger

| Topic | PASS-2 binding (post-fold) | ARCH binding | PASS-1/PASS-3 binding | Lock binding | V7 status |
|---|---|---|---|---|---|
| Backend trait integration | `PASS-2.md:134-144` | `ARCHITECTURE.md:1067-1144` (§7.5) | PASS-1.md §2 per-backend lowering obligations table at 61-71 | Lock 5 line 42 (amended Phase 7.1) | COHERENT |
| Function-value lowering | `PASS-2.md:194-203` | `ARCHITECTURE.md:1187-1207` (§8.4 four bounded forms) | PASS-1.md §3 (function-value typing); PASS-3.md F4 (public `format()`) | Lock 4 line 40 (amended Phase 7.1) | COHERENT |
| `parse-that-regex` naming | `PASS-2.md:81`, `192`, `493`, `505` | ARCH `naming-canon` lint at line 1636 | PASS-1 `parse-that` regex ownership; PASS-3 §16 retired-`pointer!` row | Lock 11 line 54 (amended Phase 7.1) | COHERENT |
| `RegexProgram` rename | `PASS-2.md:34`, `65`, `81`, `192`, `480`, `493`, `505`, `610` | `ARCHITECTURE.md:935` (§7.2 alphabet authority) | PASS-1 §2 BIR alphabet | n/a | COHERENT |
| `regex-automata` removal | `PASS-2.md` zero | ARCH `regex-engine-canon` lint at line 1637 | PASS-3 §16 cookbook | Lock 11 line 54 + V1-FOLD-CANDIDATES Tier 3 #23 | COHERENT |
| egraph + csp-solver bridge | `PASS-2.md:401` | ARCH binds bridge composition | PASS-1.md §passes::bridge | Lock 6 line 44 (amended Phase 7.1) | COHERENT |
| `pointer!` → `path!` rename | `PASS-2.md:140`, `383` retain `pointer!` | `ARCHITECTURE.md:1119`, `277-278`, `293`, `296`, `322` retain `pointer!`; `1559+`, `1575`, `1636` use `path!` | PASS-3 fully renamed per `d9414a2f`; MASTER-PLAN + MIGRATION renamed per `c8fb1506` | none (Lock 7 line 46 amendment is path-core/path-ts split, not macro name) | RESIDUAL-PARTIAL (corpus-wide hygiene) |
| Per-grammar matrix mechanical column growth | `PASS-2.md:144` | `ARCHITECTURE.md:1559-1573` (§12.1) + 1138-1144 (V2 deferral note) | Lock 14 line 60 | Lock 14 line 60 | COHERENT |
| Higher-rank monomorphisation | `PASS-2.md:200` | ARCH §7.5 + §8 type system surface | PASS-1 §2 type-system fold (DK13) | Lock 4 line 40 | COHERENT |
| Closure environment frame | `PASS-2.md:201` | `ARCHITECTURE.md:1187-1207` (§8.4) | PASS-1.md F5 lowering route | Lock 4 line 40 | COHERENT |
| V6 inheritance ledger | `PASS-2.md:546-565` | n/a | n/a | Locks 1, 4, 5, 6, 8, 10, 13, 14 | COHERENT (unchanged from V6) |
| V6 carry ledger | `PASS-2.md:580-591` | n/a | PASS-1/PASS-3 carry tables | n/a | COHERENT (unchanged from V6) |

## §6 Phase-7 fold delta vs V6

V6 returned READY on PASS-2 with no blocking punch list. Phase 7.2 fold added
four sections of surgery:

| §A surgery | V6 baseline | V7 verification |
|---|---|---|
| ADD Backend trait integration paragraph (PASS-2:134) | absent | confirmed |
| ADD per-backend obligation table (PASS-2:138-142) | absent | confirmed; rows match Phase 7.2 §A2 classification |
| ADD method-coverage map (PASS-2:140 right column) | absent | confirmed |

| §B surgery | V6 baseline | V7 verification |
|---|---|---|
| ADD function-value lowering paragraph (PASS-2:194) | absent | confirmed |
| ADD lambda + closure lowering rows (PASS-2:198, 201) | absent | confirmed |
| ADD function-typed `@host fn` parameter row (PASS-2:199) | absent | confirmed |
| ADD higher-rank lowering row (PASS-2:200) | absent | confirmed |

| §C surgery | V6 baseline | V7 verification |
|---|---|---|
| REWRITE oracle citation at line 81 | "checked against a `regex-automata` oracle lane until parity is proven" | replaced with `parse-that-regex` cross-engine parity per Lock 11 line 54 |
| REWRITE oracle citation at line 470 (now 493) | "bespoke regex code remains checked against `regex-automata` until parity is proven" | replaced with same |
| RENAME `RegexDfa` → `RegexProgram` (8 sites) | `RegexDfa` throughout | `RegexProgram` throughout; zero `RegexDfa` |
| ADD `parse-that-regex` workspace-vs-published clarifier (PASS-2:81) | absent | confirmed |

| §D surgery | V6 baseline | V7 verification |
|---|---|---|
| ADD `passes::bridge` clarifier in PASS-1 handoffs row (PASS-2:401) | absent | confirmed |

V6 → V7 delta count: 12 surgeries landed; 12 verified.

V6 → V7 regression check: V6 lane verdicts (Lane 1-9) re-evaluated above. Each
lane retains READY. V6 R1 (research-index hygiene), R2 (HARDENING-CONSOLIDATED
command #10 case-precision), R3 (HARDENING-CONSOLIDATED command #16 metadata
scope), R4 (Lock 4 egglog rationale), R5 (rewrite-budget tests) are unchanged
by Phase 7 fold and remain non-blocking.

## §7 Punch list

### §7.1 PASS-2-blocking punch list

None.

PASS-2 has no V7 surgery required. Phase 7 fold landed surgically; the V6
READY verdict survives without amendment.

### §7.2 V7-introduced non-blocking residues

| # | Path:line | Residue | Surgery | Acceptance gate | Receiver |
|---:|---|---|---|---|---|
| R-V7-1 | `PASS-2.md:140`, `PASS-2.md:383`; `ARCHITECTURE.md:1119`, `277-278`, `293`, `296`, `322` | `pointer!` macro mid-rename. PASS-3 + MASTER-PLAN + MIGRATION carry `path!` per `c8fb1506`/`d9414a2f`; PASS-2 + ARCH §7.5 retain `pointer!`. | Corpus-wide naming-canon sweep replacing `pointer!` → `path!` everywhere except deletion-archaeology contexts. ARCH `naming-canon` lint at line 1636 already names this. | `rg -n 'pointer!' restart/ -g '!archive/**'` returns zero outside lint-drift contexts. | Phase 7.3 / Tranche A naming-canon sweep agent. |
| R-V7-2 | `PASS-2.md:134-144` | Backend trait pattern is canonical multi-target compiler architecture but PASS-2 carries no SOTA citation parallel (LLVM `TargetMachine`, Cranelift `TargetIsa`, swc `Compiler<W>`). | Add a one-line cardinality-defence-style citation paragraph adjacent to the trait surface. | The Backend trait paragraph cites at least one of the three SOTA anchors. | PASS-2 amendment when SYNTHESIS or MASTER-PLAN tightens trait-pattern provenance. |
| R-V7-3 | `PASS-2.md:567-578` (diagnostic ledger) | No dedicated `BBNF-FNVAL*` or `BBNF-CLOSURE*` code for function-value lowering rejection paths; routes use `BBNF-GEN014` (LOC budget) + `BBNF-GEN001` (Grammar IR import). | Optional: add `BBNF-FNVAL001` (vtable-attempted lowering) + `BBNF-FNVAL002` (capture-by-move at codegen) routing diagnostics. | The diagnostic ledger names dedicated codes for the two function-value rejection paths. | Optional; PASS-1 owns user-facing function-value diagnostics, PASS-2 routing diagnostics suffice. |
| R-V7-4 | `PASS-2.md:606-610` | Closing posture is V6 text; omits the Backend trait integration. | Refresh §10 closing posture to name the Backend trait + per-backend obligation table + V2 mechanical-expansion route. | The closing posture cites Backend trait + V1 RustBackend + V2 deferral. | PASS-2 amendment if/when SYNTHESIS gates a §10 refresh sweep. |

### §7.3 V6-inherited residues (re-classified)

| # | V6 residue | V7 status |
|---:|---|---|
| R1 (V6 §7.2) | `restart/research/INDEX.md` provenance hygiene for Hubbard / Almomany / Deb / Yang / Roc / Ungar / HelpMate. | UNCHANGED by Phase 7. |
| R2 (V6 §7.2) | `HARDENING-CONSOLIDATED.md` command #10 case-precision. | UNCHANGED by Phase 7. |
| R3 (V6 §7.2) | `HARDENING-CONSOLIDATED.md` command #16 metadata scope. | UNCHANGED by Phase 7. |
| R4 (V6 §7.2) | Lock 4 egglog rationale. | UNCHANGED by Phase 7 (Lock 4 amended for DK13/closure, not for egglog). |
| R5 (V6 §7.2) | C.W4/C.W5 rewrite-budget tests. | UNCHANGED by Phase 7. |

## §8 Final verdict

**Decision: READY**.

| Criterion | Result |
|---|---|
| Phase 7.2 fold landed surgically | PASS. All 10 dispatch §2 Step-A verifications return positive. |
| Backend trait integration carries non-pathologically | PASS. The 5-method `Backend` trait at ARCH §7.5 + 8-method internal `BackendLowerer` compose without duplication; per-backend obligation table maps method to PASS-2 producer; method-coverage map binds each method to its emission tree. |
| Function-value lowering carries non-pathologically | PASS. Four lowering options enumerated with mechanism (lambda inline / `@host fn` parameter monomorphise / DK13 explicit-quantifier monomorphisation / closure stack frame); vtable forbidden on two independent grounds; closure capture by `&'i` reference fenced at PASS-1 type boundary. |
| `parse-that-regex` naming canon adopted | PASS. 5 sites; workspace-path-vs-published-name clarifier present; legacy `bbnf-regex` zero. |
| `RegexProgram` rename complete | PASS. 8 sites; `RegexDfa` zero. |
| `regex-automata` removal complete | PASS. Zero matches; oracle clauses retired; `parse-that-regex` cross-engine parity replaces oracle. |
| Egraph + csp-solver bridge clarifier present | PASS. PASS-1 handoffs row at line 401 cites Lock 6 line 44 + audit #4 §3 X-5. |
| F/G/H pathology regression | PASS. No F/G class regression; H-class residue (H7 `pointer!` rename) is corpus-wide, non-blocking. |
| Cross-document binding | COHERENT for 11 of 12 topics; 1 partial (R-V7-1 macro rename). |
| V6 lane verdicts retained | All 9 lanes return READY post-fold. |
| Re-draft threshold | Not met. |
| Amendment threshold | Not met for PASS-2 (4 non-blocking residues route to corpus-wide receivers). |
| Punch list | Empty for PASS-2-local edits. |

V6 → V7 delta: Phase 7 fold added 12 surgeries; 12 verified; the V6 READY
verdict survives. The Backend trait integration — the load-bearing addition —
carries non-pathologically. The function-value lowering surface enumerates
four options with mechanism and binds each to its forbidden-behavior fence.
The sibling-crate naming canon (`parse-that-regex`) and BIR-alphabet rename
(`RegexProgram`) land cleanly. The egraph decoupling clarifier rides at the
PASS-1 handoffs row where PASS-2 sees the bridge result.

Hereupon PASS-2 is fit for V7 consolidation. The non-blocking residues route
to corpus-wide hygiene (R-V7-1 `pointer!` rename), documentation polish
(R-V7-2 SOTA citation, R-V7-4 §10 closing-posture refresh), and optional
diagnostic surface tightening (R-V7-3 `BBNF-FNVAL*` codes). None blocks PASS-2
advancement; none requires a PASS-2-local amendment pass.
