# PASS-2 Synthesis: Codegen + Runtime + Backends

## §1 Verdict Ledger

PASS-2 is viable only as a replacement of the current codegen/runtime wiring, not a patch of it. The settled scope assigns this pass Backend IR, Rust V1, deferred V2 backend proof for WASM/TS, runtime template, SIMD scanner kernels, Pratt/SIMD auto-detection, and regen equality (`restart/prompts/ORCHESTRATOR.md:54-69`, `restart/locks/LOCKS.md:42`). Lock 5 requires an IR plus per-backend lowerers and forbids direct grammar walking by codegen (`restart/locks/LOCKS.md:42`). Current source violates that boundary because the shared driver says it walks `GrammarIR` (`crates/core/src/backend/driver/mod.rs:1-6`) and the broad `Emitter` trait is invoked from that grammar-walking driver (`crates/core/src/backend/emitter.rs:1-13`).

Verdict: **REINVENT codegen around Backend IR, Tape-backed runtime template, and BIR-only lowerers.** Keep the useful implementation knowledge in current Rust/WASM emitters, PHF/Pratt tables, `simd-scan`, xtask content-equality writing, and BB cohort template work. Discard stale ParseStream naming, grammar-level Unicode sets, rewrite-mode walker, per-grammar declaration crates as default, OpenFrame checkpointing, and direct Grammar IR consumers in lowerers.

Conflict ledger:

| Conflict | Stale authority | Settled resolution |
|---|---|---|
| Tape name | Inheritance says tape name dies (`restart/inheritance/INDEX.md:65-66`). | Tape is the substrate, unioned with direct-to-struct typed values (`restart/locks/LOCKS.md:34`, `restart/README.md:285-314`). |
| Rewrite mode | Inheritance and retired prompt archaeology mention rewrite-mode (`restart/inheritance/INDEX.md:34`, `restart/audit/pass-1-substrate/agent-5-grammar-extension-designer.md:39-42`). | Rewrite mode is rejected; Visitor covers transformations (`restart/README.md:123-129`). |
| Unicode set surface | Retired prompt archaeology names Unicode-set pressure; live README routes it below BBNF. | Grammar-level Unicode class algebra is not added; regex literals and `parse-that/regex` carry Unicode (`restart/README.md:131-143`). |
| Per-grammar declaration crates | MASTER-PLAN contains 9 declaration crates (`restart-archive-2026-05-04/audit/master-plan/MASTER-PLAN.md:79-89`), and Lock 14 still names an optional escape (`restart/locks/LOCKS.md:60`). | No declaration crate for the 9 extant grammars; two onboarding surfaces plus generic host primitives or `@host fn` (`restart/README.md:13-25`). |
| TS production | BD.W1 plans production TS (`docs/tranches/BD/waves/W1.md:10-24`). | PASS-2 keeps TS-lowerability as BIR-shape proof only; production TS defers to the V2 `TsBackend: Backend` impl (`restart/locks/LOCKS.md:42`). |

Sub-agent dispatch status:

| Agent | Lens | Output | Verdict |
|---|---|---|---|
| 1 | Backend IR Architect | `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md` | 19-variant BIR (Phase-8.4 α3 fold), Tape shapes, host chains. |
| 2 | Rust Lowerer Architect | `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md` | Rust lowerer consumes BIR, emits TapeBuilder checkpoints. |
| 3 | WASM Lowerer + SIMD Architect | `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md` | wasm32 Rust binding path, raw WAT demoted to smoke fixture. |
| 4 | Runtime Template Architect | `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md` | Generated grammar modules under `runtime/src/grammars/<name>/`. |
| 5 | Pratt + SIMD Auto-Detection | `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md` | Auto-detected Pratt/PHF/SIMD with decision logs. |
| 6 | Codegen Coherence Auditor | `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md` | Boundary, regen, genericity, budget gates. |

## §2 Commitments

1. **Backend IR is the PASS-2 boundary.** PASS-1 produces it after parse, validate, type inference, shape mining, e-graph, cost extraction, and lower-to-BIR (`restart/README.md:188-217`). `passes::extract` is the choice point: it emits selected BIR alternatives plus `CostDecision` and bridge-justification evidence. Rust, WASM, and TS-stub lowerers consume BIR and evidence snapshots; no lowerer imports Grammar IR or replays e-graph/CSP search.

2. **The upstream BIR alphabet count is 20 variants: 19 semantic variants plus `Return`** post Phase-8.4 α3 fold (`restart/ARCHITECTURE.md:913-940`). The PASS-1 BIR base of 22 variants collapses three semantically-redundant pairs — `(DispatchAlt, SpeculativeAlt) → `Alt { mode: Dispatch | Speculative }`, `(LayoutPush, LayoutPop) → `LayoutScope { kind: Push | Pop }`, `(CallHost, HostChain) → `CallHost` (chains express as `Seq`-of-`CallHost`) — landing on 19 semantic variants plus `Return`. PASS-1 + ARCH §7.2 own the alphabet; PASS-2 cites without re-owning. PASS-2 keeps Unicode inside the regex-program payload carried by `RegexProgram`. The `RegexProgram` spelling is the canonical BIR alphabet entry at `restart/ARCHITECTURE.md:919`; it does not require full-DFA codegen for every regex (the payload is a regex-program contract; VM, lazy-DFA, full-DFA, and prefilter are all legal execution plans).

3. **Tape/direct-to-struct is one materialisation plan.** Every rule has a `TapeShape` and `ValueShape`. `TapeShape` owns token kind, span class, payload class, traversal skip policy, and scalar-cache policy. `ValueShape` owns generated field/enum projection over the same tape identity and node id. Typed documents/views borrow `&'i Tape<'i>` plus node id; any scalar cache is declared by one of those shapes, not by a second authoritative tree or parallel substrate. This follows Lock 1 (`restart/locks/LOCKS.md:34`) and avoids the prior OpenFrame checkpoint clone that dominated samples (`restart/corpora/RESTART-SKETCH.md:154-184`).

4. **Rust V1 is the primary production lowerer.** The lowerer emits parser functions, TapeBuilder operations, typed views, scanner constants, Pratt tables, host chain calls, diagnostics, and generated registry data from BIR.

5. **WASM and TS are V2 backend impls.** Current raw WAT remains a smoke fixture and wasm32 Rust binding notes remain archaeology/proof for the future `WasmBackend: Backend` impl. V1 emits Rust through `RustBackend` only; V2 owns WASM binding ABI, npm/browser packaging, and TS-native runtime work (`restart/locks/LOCKS.md:42`, `restart/ARCHITECTURE.md:1095-1097`).

6. **SIMD scanner remains generic.** `simd-scan` already has NEON, AVX2, AVX512, WASM, and scalar modules (`crates/simd-scan/src/lib.rs:19-29`). BIR emits data-only `StructuralAlphabet` constants; no grammar code enters `simd-scan`.

7. **Runtime template emits all per-grammar runtime files.** `runtime/src/grammars/<name>/{generated.rs, parser.rs, host.rs}` is generated from grammar source plus metadata. Current manual runtime module listings are discarded (`crates/core/src/runtime/mod.rs:8-72`).

8. **Regen equality is a hard gate.** Lock 6 requires xtask-emitted committed source (`restart/locks/LOCKS.md:44`). PASS-2 keeps content-equality writing from current regen (`xtask/src/regen.rs:400-461`) but splits the module and adds BIR/runtime budget checks.

9. **No per-grammar declaration crates by default.** Amendment 01 retracts them (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-24`), and README makes two onboarding surfaces authoritative (`restart/README.md:13-25`).

PASS-2 payload-refiner table mapped to ARCH §7.2 Backend IR:

| # | ARCH §7.2 variant | PASS-2 payload refinement | Generation site | Rust V1 lowering | Deferred backend note |
|---|---|---|---|---|---|
| 1 | `Entry` | symbol, params, `ValueShape`, `TapeShape`, payload policy, traversal policy | PASS-1 BIR lower | parse fn plus typed view | V2 backends expose host-idiomatic entry wrappers. |
| 2 | `Seq` | child ids, field map | rule lowering | ordered child calls | Same semantic order. |
| 3 | `Alt { mode: Dispatch \| Speculative }` | dispatch keys, arms, checkpoint policy | `passes::extract` using `CostDecision` evidence from `cost-model` | match/PHF/scan dispatch or cursor/tape checkpoint | Same bounded rollback rule; no clone stack. |
| 4 | `RepeatLoop` | body, separator, bounds | rule lowering | loop with progress guard | Same progress invariant. |
| 5 | `OptionalBranch` | body, absence policy | rule lowering | optional branch | Same absence policy. |
| 6 | `CallRule` | target rule, args | rule lowering | rule call | Same BIR call shape. |
| 7 | `ByteLiteral` | bytes, case policy | rule lowering | byte compare | Backend chooses host-native byte/string comparison. |
| 8 | `RegexProgram` | regex program handle, execution plan, Unicode metadata, verifier contract | regex compiler | regex call or verifier | V2 reuses `parse-that-regex` program semantics. |
| 9 | `SimdScan` | scan mode, structural alphabet, fallback, verifier route | shape miner | `simd-scan` call or scalar fallback | V2 WASM may use `simd128`; TS may stay scalar/host-native. |
| 10 | `PrattSpine` | operator table, associativity, precedence | Pratt detector | Pratt loop | Same operator-spine semantics. |
| 11 | `CallHost` | single host call, generics, error policy | host inference | `host` registry call | V2 owns ABI/marshalling descriptors. |
| 12 | `LayoutScope` | layout kind, policy, scope id | `@layout` analysis | layout consume | Same layout facts; renderer differs by host. |
| 13 | `ErrorRecover` | strategy, diagnostic | `@error` analysis | diagnostic/recovery edge | Same recovery facts; host diagnostics differ. |

Layout canon — Lock 2 vocabulary at the BIR boundary: PASS-1's `passes::layout` produces the `LayoutFacts` side-table; PASS-2's `LayoutScope` BIR variant consumes it via `LayoutSink`. The producer/side-table/consumer triple is the single source of truth for layout lowering across the BIR boundary; the `@layout` analysis surface mentioned in the variant table feeds `passes::layout`, and both the §7 per-construct contribution plan and the §7 runtime emission table bind runtime layout lowering to `LayoutFacts` consumed through `LayoutSink`.

| 14 | `SpanMark` | span projection and source-map slot | rule lowering | span value | Offset/length projection per backend. |
| 15 | `TapeEmit` | tape kind, payload class, scalar cache policy | BIR builder | tape event | Linear-memory or host-object representation in V2. |
| 16 | `DirectBuild` | generated field projection, owner type | type lower | typed direct view | Host-native object/view shape in V2. |
| 17 | `ValueProject` | projection expression and result slot | type/e-graph lower | typed conversion | Same value-shape semantics. |
| 18 | `PathEval` | path plan hook, schema key | path schema lower | typed `path!` glue | V2 `path-ts` consumes the same schema after `TsBackend` lands. |
| 19 | `DebugMark` | source map marker | debug config | optional metadata | Backend-specific debug sidecar. |
| 20 | `Return` | return slot and value policy | rule lowering | return from entry/rule | Same return semantics. |

Regex execution-plan note: `RegexProgram` is the canonical BIR alphabet spelling at `restart/ARCHITECTURE.md:919`, with its payload row at `restart/ARCHITECTURE.md:950`; PASS-2 consumes the alphabet, never re-owns it. Its payload is a regex-program contract: VM, lazy-DFA, full-DFA, literal prefilter, and Unicode-table plans are legal execution choices when `parse-that-regex` proves their size and verifier constraints. Full DFA construction is therefore an implementation plan, not a mandatory lowering for every regex. Regex algorithms are proved by `parse-that-regex` internal cross-engine parity (VM ↔ lazy DFA ↔ full DFA on the same fixtures), per Lock 11 line 54 and audit #4 §4 — the user mandate forbids `regex-automata` entirely, including as oracle. The published-crate name `parse-that-regex` is the regex sub-crate of `parse-that`; the workspace path `parse-that/regex` and the published name refer to the same engine substrate.

Cardinality defence: BC's research anchors put the useful compiler-IR band at roughly 20-30 variants, comparing MLIR `arith` at 60, Cranelift `InstructionData` at 40, rustc HIR `ExprKind` at 35, rustc HIR `ItemKind` at 16, and chalk `TyKind` at 23 (`docs/tranches/BC/audit/research-anchors.md:12-18`, `docs/tranches/BC/audit/W0-typed-ir-variant-table.md:319-329`). The upstream 20-row alphabet (19 semantic variants plus `Return`, post Phase-8.4 α3 fold) sits inside that band. swc is kept as backend-separation inheritance rather than a cardinality bound because local README cites swc for future WASM/codegen pipeline shape (`restart/README.md:369`); swc compiles JavaScript AST into per-domain enums (`Stmt` / `Expr`) with different cardinality pressure than parser IR (`restart/corpora/SOTA.md:186` — parol's typed-AST cardinality reference is the closest auditable corpus line for the AST-cardinality argument; the swc rustdoc URL citation is retired in favour of corpus path:line discipline).

Payload-refiner contract — PASS-2's role in the BIR contract:

PASS-2 is **payload refiner, not BIR re-owner**. The variant alphabet, the variant inventory, and the producer-side semantics belong upstream at PASS-1 + Architecture §7 (PASS-1.md:55, "PASS-2 may sharpen field types ... PASS-2 may not introduce new variants, retire variants, or redefine the alphabet"). PASS-2 may sharpen the payload of every variant and may add lower-time evidence; PASS-2 may not bypass or re-own Backend IR.

| Refinement scope (PASS-2 may sharpen) | Refinement floor (PASS-2 may not touch) |
|---|---|
| Payload field widths, alignment, and packing for each variant. | The 20-row upstream alphabet itself (19 semantic variants plus `Return`, post Phase-8.4 α3 fold); new variants and retirements return to PASS-1. |
| Layout-tag specialisation (e.g., `Alt { mode }` Dispatch-vs-Speculative selection at lower time). | Producer-side semantics (typed grammar IR; e-graph; cost-model trait; CSP solver). |
| Cost-derived dispatch shape (`match` vs PHF vs scan tree), including scalarized fast-path score plus objective vector/profile evidence. | Lower-time invariants stated at PASS-1.md:43-53 (no OpenFrame clone stack; regex owns Unicode; auto-detection only). |
| SIMD-vs-scalar kernel selection from `KernelShape` evidence. | Diagnostic-string surface owned by PASS-1 (§2 diagnostic strings owns `BBNF-LOOKBEHIND-WIDTH`, `BBNF-PATTERN-NONEXHAUSTIVE`, etc., per Phase-8.4 β1 human-readable canon). |
| Pratt LUT and operator-spine state machine layout. | Grammar IR variants and side tables; `passes::extract` is the only consumer. |
| `StructuralAlphabet` constants from BIR `SimdScan` payload. | Backend IR variant ordering and stable id keys. |
| Per-variant span/source-map metadata in payload tail. | Cross-pass hand-off contracts owned by SYNTHESIS. |
| Per-payload runtime template parameters (per the §2 schema table). | The refiner-vs-re-owner boundary itself. |

Per-payload-category lowering test gates owned by PASS-2 — every gate references the Rust V1 obligation and any deferred-backend descriptor at PASS-1.md:59-71:

| Payload category | Lowering test gate (PASS-2 owned) | Backend obligation source |
|---|---|---|
| Entry/control | `cargo test -p codegen --test entry_lowering` — Rust basic-block shape and V2 backend descriptor snapshot. | PASS-1.md:61 (entry/control row). |
| Dispatch/speculation | `cargo test -p codegen --test dispatch_lowering` — bounded-rollback proof; no clone stack. | PASS-1.md:62 (dispatch/speculation row). |
| Terminal/scanner | `cargo test -p codegen --test scanner_lowering` — slice-compare + regex + `simd-scan` parity, including SIMD false-positive discard, no false-negative proof, scalar offset-vector equality for exact scans, and verifier-before-tape emission for prefilters. | PASS-1.md:63 (terminal/scanner row). |
| Pratt/SIMD | `cargo test -p codegen --test pratt_simd_lowering` — Pratt LUT + SIMD-vs-scalar selection with objective profile and target legality evidence. | PASS-1.md:64 (Pratt/SIMD row). |
| Host/layout/error | `cargo test -p codegen --test host_layout_error_lowering` — `host::call_<name>` dispatch + `@error` recovery shells + V2 host-ABI descriptor shape. | PASS-1.md:65 (host/layout/error row). |
| Tape/direct/value | `cargo test -p codegen --test tape_value_lowering` — `TapeEmit` + `DirectBuild` projection over one node identity, payload class, and scalar-cache policy. | PASS-1.md:66 (tape/direct/value row). |
| Debug/path | `cargo test -p codegen --test debug_path_lowering` — source-map sidecar + `DebugMark` cfg-gate + backend-neutral sidecar schema. | PASS-1.md:67 (debug/path row). |

WASM host primitive route (V2): host primitives are a lowerer/runtime ABI concern. PASS-2 records exported function names, host-call shape, marshalling descriptors, and scalar/SIMD parity evidence as a deferred `WasmBackend: Backend` receiver contract; BBNF source keeps the existing `@host fn` body form and gains no primitive annotation or force directive.

The hand-off contract is precise: PASS-1 owns variants + alphabet + invariants + producer-side semantics + diagnostic strings (§2 invariants, variant ownership, and diagnostic strings); PASS-2 owns payload refinement + per-backend lowering obligations + emission tests (this section, the §3 lowerer trees, the §6 generated-LOC budgets); PASS-3 owns tape ABI + visitor + path metadata consumption (§4 hand-off). Cross-pass conflict on a payload returns to SYNTHESIS for reconciliation, not to a unilateral edit on either side.

Emitter public API:

```rust
pub trait BackendLowerer {
    type Files;
    fn emit_module(&mut self, module: &BackendModule) -> Result<()>;
    fn emit_types(&mut self, module: &BackendModule) -> Result<()>;
    fn emit_rule(&mut self, rule: RuleId) -> Result<()>;
    fn emit_node(&mut self, node: NodeId) -> Result<LoweredNode>;
    fn emit_scanner_tables(&mut self, module: &BackendModule) -> Result<()>;
    fn emit_host_table(&mut self, module: &BackendModule) -> Result<()>;
    fn emit_registry(&mut self, module: &BackendModule) -> Result<()>;
    fn finish(self) -> Result<Self::Files>;
}
```

This is the concrete collapse of the current broad trait (`crates/core/src/backend/emitter.rs:31-566`) into the 8-method shape PASS-B forecast (`restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`).

Backend trait integration — Phase-7.1 ARCH §7.5 anchor (post Phase-8.4 α1 fold): PASS-2's lowerer suite is the V1 `RustBackend: Backend` impl per the formal trait at `restart/ARCHITECTURE.md` §7.5. The `Backend` trait carries two methods — `lower(bir, ctx)` and `emit_artefacts(grammar, schemas) -> ArtefactSet` — that gate the V1/V2 contract boundary. `lower` produces the parse-function source; `emit_artefacts` co-emits the typed `Value` enum, the `Visitor` trait + `VisitTypes` bitflag, the `<g>.path-schema.toml` + typed `path!` glue, and the runtime-template module tree from a single grammar+schema input. The four artefacts share input metadata (tape kinds + view structs + grammar metadata + value/visitor/path schemas); per-method dispatch was contrivance — the four were always co-emitted from the same input. Phase-8.4 α1 collapses them. The internal `BackendLowerer` (8-method) shape below is the per-rule emission decomposition that `RustBackend::lower` invokes; the two trait surfaces compose, they do not duplicate. Lock 5 (`restart/locks/LOCKS.md:42`) commits to per-backend lowerers as the contract boundary; the formal trait at ARCH §7.5 is what enforces that boundary in V1 and what gates seamless post-V1 expansion to `WasmBackend` and `TsBackend` impls without re-architecting BIR or codegen.

Internal-trait clarification — Phase-8.4 α7 fold: the 8-method `BackendLowerer` trait above carries no V1 polymorphism; only `RustLowerer` implements it. The 8-method method set is per-rule emission decomposition (types / rule / node / scanner / host / pratt / error / registry), not a contract gate. Future per-backend lowerer impls — `WasmLowerer` for wasm32 codegen, `TsLowerer` for the TS-native fork — inherit the same trait shape without polymorphism widening; the trait carries one V1 impl now, and admits two more post-V1 without method-set changes. Trait dispatch under a single live impl monomorphises away under rustc; the cost of the partition is documentation, not runtime.

Per-backend obligation table — V1 ships one active `Backend` impl; the wasm32 + TS lowerer scaffolds prove BIR-shape adequacy without registering as V1 `Backend` impls:

| Backend impl | V1 status | Post-V1 receiver | Trait surface |
|---|---|---|---|
| `RustBackend: Backend` | V1 active — this PASS-2's lowerer suite. Emits committed `.rs` artefact tree under `crates/runtime/src/grammars/<g>/` per ARCH §7.5. | n/a | `lower` → §3 `codegen/src/lower/rust/*`; `emit_artefacts(grammar, schemas)` → one `ArtefactSet { runtime_template, value_api, visitor, path_schema }` co-emitted from grammar+schema bundle: `runtime_template` covers §3 `runtime_template/*` tree + §6 generated LOC table; `value_api` covers generated `value.rs`; `visitor` covers `visitor_bitflags` parameter (line 154) + generated `visitor.rs`; `path_schema` covers `<g>.path-schema.toml` + typed `path!` glue. |
| `WasmBackend: Backend` | Carried post-V1; PASS-2 emits the wasm32 binding lowerer at §3 `codegen/src/lower/wasm/*` as the BIR-shape proof, not as the V1 `Backend` impl. | Lands alongside the Lock 11 post-V1 publication carry + Lock 8 post-V1 WASM SOTA gate. The `wasm32` binding path stands ready; the `WasmBackend: Backend` trait registration lands without re-architecting BIR. | `lower` → wasm32 lowering of the same `BackendIR` (`WasmRustSource`); `emit_artefacts` mirrors the Rust artefact set through the wasm32 binding shell. |
| `TsBackend: Backend` | Carried post-V1; PASS-2 keeps the TS scaffold at §3 `codegen/src/lower/ts_stub/` to prove the BIR shape is TS-lowerable without freezing the emitted source. | Lands alongside the principled TS-native parse+runtime fork per Lock 7 line 46. | `lower` → committed `.ts` artefact tree (`TsSource`); `emit_artefacts` mirrors the Rust artefact set in the TS namespace. |

The per-grammar matrix at ARCH §12.1 grows columns mechanically when each new `Backend` impl lands; no grammar-side change is required (Lock 14 line 60). The trait pre-existence is the load-bearing piece that makes the post-V1 expansion mechanical rather than architectural.

Runtime template parameter schema:

| Parameter | Required source | Generated consumer |
|---|---|---|
| `grammar_ident` | workspace metadata | module names and diagnostics |
| `kind_enum` | BIR tape kind table | `TapeNode.kind` and visitor |
| `value_enum` | BIR value shapes | generated view/value API |
| `document_struct` | metadata naming policy | parse return |
| `view_structs` | BIR field maps | field accessors |
| `parse_fn_signatures` | PASS-3 API contract | `parser.rs` |
| `leaf_kinds` | BIR literal/regex/scanner nodes | leaf payload projection |
| `host_fn_table` | metadata plus `@host fn` | generated `host.rs` |
| `simd_alphabet` | PASS-1 SIMD plan | scanner constants |
| `layout_policy` | `@layout` analysis | layout skipping |
| `error_policy` | `@error` analysis | diagnostics |
| `pratt_tables` | Pratt detection | Pratt loop data |
| `budget` | xtask metadata | LOC gate |
| `visitor_bitflags` | BIR view shapes | generated `visitor.rs` impl |
| `bump_arena` | PASS-3 API contract | `parse_in` signature lowering |
| `incremental_marker` | cost model | optional source-map sidecar |

The parameter set extends the BB cohort template list (`docs/tranches/BB/audit/W2-cohort-template-spec.md:8-22`) and keeps byte-identical regeneration (`docs/tranches/BB/audit/W2-cohort-template-spec.md:40-61`).

SIMD coverage matrix:

| Platform | Kernel | Current inheritance | PASS-2 obligation |
|---|---|---|---|
| aarch64 | NEON | module exists in `simd-scan` dispatch (`crates/simd-scan/src/lib.rs:19-29`). | parity with scalar on emitted alphabets. |
| x86_64 | AVX2 | module exists in dispatch (`crates/simd-scan/src/lib.rs:19-29`). | parity plus throughput benchmark. |
| x86_64 | AVX512 | module exists in dispatch (`crates/simd-scan/src/lib.rs:19-29`). | compile-gated parity when host supports it. |
| wasm32 | wasm-simd128 | WASM module exists (`crates/simd-scan/src/wasm.rs:1-14`). | scalar fallback and SIMD128 parity. |
| all | scalar | dispatch fallback exists (`crates/simd-scan/src/lib.rs:70-114`). | reference implementation for all parity. |

Detection thresholds:

| Decision | Select when | Reject when | Evidence |
|---|---|---|---|
| Pratt | recursive expression family has operator-bearing prefix/infix/postfix alternatives and a total precedence order. | recursion lacks operator partition or width/progress proof. | Lock 10 auto-detects Pratt (`restart/locks/LOCKS.md:52`). |
| SIMD | structural byte alphabet is non-empty, the scan mode is exact with scalar parity or prefilter with verifier route, target legality holds, and the selected objective profile beats scalar for expected input length. | candidate is illegal for the target, the regex summary cannot expose a safe prefilter, alphabet is Unicode-semantic or tiny, verifier route is missing, or setup/code-size cost dominates runtime gain under the selected objective profile. | `KernelShape` categories exist in `simd-scan` (`crates/simd-scan/src/alphabet.rs:98-125`). |
| PHF | literal/keyword set is large enough that hash dispatch beats match-tree under cost model. | small sets or prefix-overlap make branch tree cheaper. | current Rust emission already has PHF keyword table path (`crates/core/src/backend/rust/emitter/grammar.rs:155-163`). |
| Lookbehind | predicate width is fixed or bounded by PASS-1 analysis. | unbounded lookbehind. | lookbehind is grammar-level V1 (`restart/README.md:125-129`). |

Lookbehind co-amendment — codegen-side ratification of the BBNF surface:

PASS-2 ratifies the canonical `|<` grammar-level lookbehind syntax that PASS-1 owns in §6, "BBNF Grammar Formal Specification". Regex-style `(?<=...)` lookbehind stays inside regex literals only; grammar-level lookbehind is `|<` and reaches BIR through the `RegexProgram` / guard path represented in the §2 payload-refiner table and the ARCH §7.2 payload rows. The codegen-side legality contract is finite-width-only: PASS-1's width analysis annotates the bound; PASS-2 lowering accepts `Bounded(n)` and rejects unbounded predicates at the lowering boundary, before any source emission. The diagnostic surface composes — PASS-1 owns the user-facing code `BBNF-LOOKBEHIND-WIDTH`; PASS-2 owns the routing diagnostic `BBNF-LOOKBEHIND-UNBOUNDED-AT-BIR` in the §8 diagnostic ledger when an unbounded `Lookbehind` reaches BIR validation. The two diagnostics are produced together: `BBNF-LOOKBEHIND-WIDTH` reaches the user through the PASS-3 diagnostic surface; `BBNF-LOOKBEHIND-UNBOUNDED-AT-BIR` halts codegen close before any lowerer emits a parser file. Lowering emits a reverse predicate with the bound encoded as a compile-time constant; V2 `WasmBackend` inherits the same BIR payload and finite-width invariant when it lands.

Unified cursor + byte-skip obligation — Lock 3 ratification at the codegen-side: Rust V1 lowerer emits one parse implementation; cursor consultation generates a byte-skip when consult returns `Skip`; the empty-path case (`__EAGER_EMPTY_PATH`) elides cursor calls. The unified path is realized by `CallRule`, `ByteLiteral`, the `RegexProgram` regex-program payload, and `SimdScan` BIR variants; `PrattSpine` and `SimdScan` carry their own dispatch and elide cursor consultation in the inner loop. V2 `WasmBackend` honours the same obligation when it lands, sharing the BIR payload and the structural snapshot consumed by Rust V1; the cursor-vs-byte-skip decision is a lowering choice, not a substrate split.

Function-value lowering — Phase-7.1 Lock 4 fold (`restart/locks/LOCKS.md:40`) folded function values + lambda literals (`|x| body`) + closure capture by `&'i` reference + function types `fn(T) -> U` in the `Type` non-terminal into V1. PASS-2 lowering follows audit #6 §7.4 V1 recommendation:

| Surface | Lowering option | Mechanism |
|---|---|---|
| Lambda literal `\|x\| body` (F4) | Option 3 — inline at known call site | Closure form is admitted only when the call site is statically known at codegen time. The closure body is inlined into the call site as straight-line BIR. No new BIR variant; no environment heap-frame; no vtable. The four bounded forms (host chain, map, predicate, recovery) at ARCH §8.4 (`restart/ARCHITECTURE.md:1357-1362`) are the V1 closure inventory; lambda literals lower into one of these forms based on consumer-site type. |
| Function-typed `@host fn` parameter `f: fn(T) -> U` (F3) | Option 1 — monomorphise per call site | When the function value is statically known at the call site, the call monomorphises against the finite `(RuleId, TypeArgs)` instance set machinery described by the §6 generic monomorphisation budget gate. One `@host fn map<T, U>(f: fn(T) -> U, ...)` becomes one BIR rule per concrete `(T, U, f)` instantiation. Per Phase-8.4 γ8, bbnf emits monomorphised Rust source from the finite instance set; rustc completes the Rust-side substitution and codegen. The bbnf-side budget gate audits emitted-LOC growth from the finite instance set; it does not duplicate rustc's monomorphisation work. Vtable dispatch (option 2) is forbidden in V1 (`restart/ARCHITECTURE.md:1366-1373` forbids host-process-state and rewrite-mode closure escapes; the throughput targets at `restart/audit/pass-2-codegen/PASS-2.md:472-486` reject hot-path heap allocation). |
| Higher-rank `forall`-quantified parameter (DK13) | Option 1 — explicit-quantifier monomorphisation | Phase-7.1 Lock 4 (line 40) folded DK13 algorithmic completeness into V1; explicit `forall` annotations in `@host fn` signatures instantiate monomorphically at the call site, identically to rank-1 generics. The finite `(RuleId, TypeArgs)` instance set extends to carry explicit-quantifier instantiations alongside rank-1 instances; the §6 generic monomorphisation budget gate covers both. The CSP solver provides the GADT substrate; the user-facing GADT surface lands V1 via pattern-match branch-local-equality refinements (`Pattern @ where T = U -> Block` per Lock 4 + the §6 BBNF grammar amendment), with `BBNF-LOCAL-EQUALITY-ANNOTATION` emitted when a match-arm refinement annotation is missing or ill-typed. |
| Closure environment frame (F5) | Stack-allocated reference frame; rustc handles lifetime soundness | Closures capture lexically by `&'i` reference (Lock 4 line 40); capture-by-move is forbidden in V1; `Fn`/`FnMut`/`FnOnce` discrimination is not exposed at the BBNF surface. The captured environment lowers to a stack-allocated frame whose lifetime is bound by `&'i Tape<'i>`; no heap allocation (incompatible with the per-call hot-path targets at `restart/audit/pass-2-codegen/PASS-2.md:472-486`). Per Phase-8.4 γ1, bbnf emits the lowered closure Rust source and rustc validates lifetime escape at downstream `cargo check`; PASS-2 carries no closure-lifetime audit machinery beyond emission. The arena substrate is orthogonal: `parse_in(input, &bump)`'s arena lifetime bounds input-data extension only (Lock 9), and per Phase-8.4 ε3 closures never escape into `bump` regardless of arena entry point. The two memory regions partition cleanly — bumpalo carries input-borrowing artefacts; closures stay stack-bound on the parser frame. The forbidden-behavior rows at ARCH §8.4 (`restart/ARCHITECTURE.md:1366-1373`) carry the V1 fences: closures may not mutate parser input, may not capture arbitrary host process state, and may not encode rewrite-mode sugar; PASS-1's type-check time enforces the fences upstream, and rustc rejects any escape that reaches emission. |

The cost-model evidence binding for option 3 + option 1: each inlined closure body contributes to the parent rule's `CostDecision` exactly once; each monomorphised call-site instance carries its own scalar score + objective vector record per the existing §5 cost-model handoff evidence shape. No new evidence shape is required. The four-form bounded closure inventory is the V1 covered surface; the option-3 inline path is the lowering of every grammar-derived callback need in the seed corpus (json, css, sheets, math, bbnf, bnf, csv, ebnf, css_pretty, plus yaml).

## §3 Per-Crate Trees

`ir` Backend IR ownership:

```text
ir/src/backend_ir/
  mod.rs
  module.rs
  node.rs
  shape.rs
  host.rs
  layout.rs
  lookbehind.rs
  pratt.rs
  simd.rs
  snapshot.rs
```

PASS-2 ratifies Backend IR type-definition + variant-alphabet ownership at `ir/src/backend_ir/`, per the upstream declaration at PASS-1.md:41 ("type definitions and the variant alphabet live under `ir/src/backend_ir/`"). PASS-2 names no `codegen/src/backend_ir/` ownership path; the `codegen` crate's role is **lowerer + adapter + snapshot + emission-test consumer** — it imports `ir::backend_ir::*`, never defines or extends the BIR node alphabet, and never re-owns variant definitions. The lowerers (`codegen/src/lower/{rust,wasm,ts_stub}/`) consume; the adapters (`codegen/src/runtime_template/`) consume; the snapshot printer at `codegen/src/runtime_template/files.rs` produces stable BIR snapshots for the regen-equality gate; the emission tests under `codegen/tests/` exercise BIR-to-source equivalence. New variants, retired variants, and alphabet edits return upstream to PASS-1 + Architecture §7 and rerun the hardening gate before they land (per PASS-1.md:55).

`codegen`:

```text
codegen/src/
  backend_ir/
    README.md
  lower/
    mod.rs
    rust/
      mod.rs
      types.rs
      rule.rs
      node.rs
      scanner.rs
      host.rs
      pratt.rs
      error.rs
    wasm/
      mod.rs
      abi.rs
      bindgen.rs
      host.rs
      simd.rs
      smoke_wat.rs
    ts_stub/
      mod.rs
  runtime_template/
    mod.rs
    files.rs
    tape.rs
    grammar.rs
    host.rs
    budgets.rs
```

Rationale: the current `Emitter` surface spans hundreds of lines (`crates/core/src/backend/emitter.rs:31-566`). PASS-B already called for collapsing it to 8-10 methods (`restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`). Backend IR types live in `ir`, because Lock 5 makes BIR the cross-crate contract. `codegen/src/backend_ir/README.md` is documentation only: it records the import boundary and points contributors to `ir::backend_ir`.

Import-deny floor:

| Gate | Required assertion | Failure means |
|---|---|---|
| Rust lowerer imports | `codegen::lower::*` imports `ir::backend_ir::*` and does not import Grammar IR modules. | Lowerer is walking source grammar. |
| V2 WASM lowerer descriptor imports | `codegen::lower::wasm::*` imports the same BIR module as Rust when the deferred backend proof is compiled. | WASM has forked the lowering contract. |
| Runtime template imports | Template parameters are serializable BIR snapshots or runtime metadata, not Grammar IR nodes. | Template has become a hidden compiler pass. |
| Snapshot gate | `cargo xtask bbnf bir --all --check` emits stable BIR snapshots before lowerers run. | The BIR boundary is not externally inspectable. |

Verbatim deny command — the codegen close gate:

```text
# scan the whole codegen tree; documentation surface
# (crates/codegen/src/backend_ir/README.md) is the only
# legal carrier of the GrammarIR token within this tree.
rg -n "GrammarIR" crates/codegen/src/
```

Expected output: zero matches. Any non-zero result fails codegen close, emits diagnostic `BBNF-CODEGEN-IMPORT-DENY` (per Phase-8.4 β1 retire of `BBNF-GEN001`), and blocks the regen-equality gate (`xtask regen --check`) downstream. The only crate exempt from this deny is `passes` — specifically the BIR producer pass under `passes/extract/` that consumes Grammar IR from the typed/shape-mined/e-graph-extracted upstream and emits Backend IR for `ir::backend_ir`. PASS-1.md:41 names the producer-side exemption: "only the BIR producer pass under `passes` may import Grammar IR; lowerers walk Backend IR alone." `codegen` has no such exemption; every codegen lowerer consumes BIR and never reaches behind it. The gate runs at every PR check, every codegen close, and every regen-equality verification; it is not a one-shot audit.

`runtime`:

```text
runtime/src/
  tape/
    mod.rs
    node.rs
    payload.rs
    checkpoint.rs
  value/
    mod.rs
  error/
    mod.rs
  visitor/
    mod.rs
  layout/
    mod.rs
  owned/
    mod.rs
  grammars/
    <name>/
      mod.rs
      generated.rs
      parser.rs
      host.rs
```

Rationale: Lock 1 places tape at `runtime/src/tape/` (`restart/locks/LOCKS.md:34`). Lock 13 rejects the current mixed runtime god directory (`restart/locks/LOCKS.md:58`). Every `<name>` subdir is generated and structurally identical.

`host`:

```text
host/src/
  mod.rs
  primitive.rs
  registry.rs
  chain.rs
  signature.rs
  wasm.rs
```

Rationale: host functions decompose through generic primitives, metadata, or `@host fn` (`restart/README.md:13-25`, `restart/README.md:145-157`). No default per-grammar declaration crate is part of PASS-2.

Rare declaration crates stay behind the Architecture/Lock 11 incubation fence: PASS-2 may emit generated host adapters only from approved metadata or `@host fn` bodies, and generic codegen/runtime crates never import a declaration crate. Registry promotion belongs to the stability owner after the review form records failure proof, deletion path, reviewer, and receiving gate.

`simd-scan`:

```text
simd-scan/src/
  alphabet.rs
  scalar.rs
  neon.rs
  avx2.rs
  avx512.rs
  wasm.rs
  lib.rs
```

Rationale: MODULES calls `simd-scan` clean and KEEP-AS-IS (`restart/corpora/MODULES.md:47-69`). PASS-2 keeps the crate and adds BIR-fed alphabets and parity tests, not grammar-specific code.

`xtask` regen split:

```text
xtask/src/regen/
  mod.rs
  plan.rs
  metadata.rs
  backend_ir.rs
  runtime.rs
  write.rs
  check.rs
  budget.rs
  registry.rs
```

Rationale: current `xtask/src/regen.rs` is a single large module; CENSUS lists it at 849 LOC (`restart/corpora/CENSUS.md:321-354`). Existing content-equality writes are kept (`xtask/src/regen.rs:400-461`).

## §4 PASS-3 Handoffs

PASS-3 receives these contracts:

| Contract | PASS-2 producer | PASS-3 consumer |
|---|---|---|
| Parse signatures | runtime template `parser.rs` | public API and docs |
| Document/view types | generated runtime modules | selectors, visitor, owned escape |
| Visitor hooks | `runtime/src/visitor/` plus generated view impls | rewrite/transform API, replacing rewrite mode |
| Error vocabulary | BIR `ErrorRecover` and runtime `error` | user diagnostics |
| Host table metadata | `host` registry plus generated `host.rs` | package integration and user host bindings |
| WASM ABI descriptor | `codegen/lower/wasm/abi.rs` | npm/browser packaging |
| Materialisation cost table | BIR decision log | API docs and performance explanation |
| Consumer proof | generated metadata for paths, visitors, diagnostics, and host tables | `path-core`, `runtime`, and language-server smoke tests |

BD.W5 parity is not a PASS-2 close gate. It remains downstream: 9 grammars times at least 3 fixtures times 3 backends, for at least 81 cells (`docs/tranches/BD/waves/W5.md:181-217`).

PASS-3 consumer acceptance gates — every contract carries a named verification command before PASS-2 may close:

| Contract | Consumer acceptance gate (PASS-2 owned) | Failure means |
|---|---|---|
| Emitted parse signatures compile under PASS-3 API wrappers | `cargo test -p bbnf --test parse_signature_compile` — every generated `parse_<grammar>` and `parse_owned_<grammar>` signature is wrappable from `crates/bbnf/src/parse/` without trait-object adaptors. | PASS-3 cannot import the emitted signatures without re-parsing. |
| Document/view metadata feeds visitor + selectors | `cargo test -p runtime --test view_metadata_visitor` plus `cargo test -p path --test view_metadata_selector` — generated `Document` and view structs implement the visitor and selector entry traits with no hand-written impl per grammar. | Metadata-driven visitor/selector wiring fails; PASS-3 hand-writes per-grammar visitor code. |
| Materialisation cost table is generated and documented | `cargo xtask bbnf cost-table --check` emits `target/codegen/cost-table.md` byte-identical to the committed snapshot at `runtime/src/grammars/<name>/cost.md`. | API docs cannot reproduce the materialisation cost story without prose-only hand-offs. |
| Path-schema metadata reaches `path` and `path-core` | `cargo test -p path-core --test grammar_schema_load` — every emitted runtime exposes the path schema descriptor consumed by typed `path!` compilation. | Path inference cannot bind grammar segments at compile time. |
| Diagnostic vocabulary reaches PASS-3 user surface | `cargo test -p bbnf --test diagnostic_vocabulary` — the BIR `ErrorRecover` and PASS-1 human-readable diagnostic codes (per Phase-8.4 β1 retire of numeric-suffix aliases; the PASS-1 §2 catalogue carries the canonical names) round-trip through PASS-3's user-facing error type. | User errors lose codes, spans, or severities at the PASS-2/PASS-3 boundary. |
| V2 WASM ABI descriptor remains coherent | `cargo test -p codegen --test wasm_abi_descriptor` — `codegen/lower/wasm/abi.rs` records a descriptor shape for the future `WasmBackend: Backend`, without registering a V1 backend impl. | V2 WASM packaging would require hand-written glue. |

These gates close the prose-only handoff: PASS-3 cannot accept the contract on prose-only language; every contract is either backed by a named verification command or it is not in the contract. PASS-2 must run all six gates before the §4 codegen close gate fires.

## §5 PASS-1 Handoffs

PASS-2 assumes PASS-1 will provide:

| PASS-1 Product | PASS-2 Use |
|---|---|
| Grammar IR variants and node ids | source-to-BIR traceability |
| Type layouts and generics | `ValueShape`, host signatures, view fields |
| Cost model decisions | `CostDecision` records with scalar score, objective vector, selected profile, legality, stable child/e-class/BIR ids, selected alternative, rejected alternatives, and dominated/Pareto evidence. |
| Opaque regex cost summaries | `RegexCostSummary` evidence for regex paths, consumed without importing regex HIR, NFA, DFA, VM, Unicode, or prefilter internals. |
| Cost model trait and scores | alt dispatch, PHF, SIMD, Pratt choices, with scalar Cost allowed only as a fast extraction path when the full evidence record survives. The trait + score machinery is owned upstream at the `cost-model` crate (`restart/corpora/MODULES.md` registers `cost-model`) with `CostFacts` produced by `passes::extract` per ARCH §10.1 (`restart/ARCHITECTURE.md` §10.1 rewrite-budget categories + §7.3 `CostFacts` row); per Phase-8.4 ε2 PASS-2 consumes `CostDecision` records and never re-owns the trait. The cross-substrate sharing claim — parser cost-model and `parse-that-regex` cost-model share trait shape — is structural at V1 (one trait, two impls); semantic composition (cross-substrate cost decisions) is post-V1 generality with no V1 receiver. |
| Shape mining outputs | seq/alt/repeat materialisation and scanner plans |
| E-graph extraction | simplified rule bodies before BIR plus `BridgeJustification` records keyed by stable Grammar IR node ids, e-class ids, CSP variables, and proof refs. The egraph + csp-solver compose at `passes::bridge` per Lock 6 (`restart/locks/LOCKS.md:44`); the bridge settles before BIR reaches PASS-2. PASS-2 codegen never imports either crate directly and consumes BIR post-extraction only — Lock 4 per-domain orthogonality holds at the dependency-graph level (audit #4 §3 X-5). Per Phase-8.4 ε4, the rewrite-category cardinality and per-category classification (legality / normalization / cost-driven / simplification) live at ARCH §10.1 (`restart/ARCHITECTURE.md` §10.1); PASS-2 is consumer and cites the inventory without restating. |
| Lookbehind width analysis | bounded `Lookbehind` BIR node |
| Layout and error annotations | `LayoutScope` and `ErrorRecover` nodes |
| Host function inference | `CallHost` chains |

The PASS-1 synthesis names these products as hand-offs (`restart/audit/pass-1-substrate/PASS-1.md:41-57`). In the original parallel Phase 1 run, PASS-1 output was intentionally unavailable; SYNTHESIS reconciles final PASS-1 artefacts before tranche execution.

Future grammar onboarding smoke:

| Surface | PASS-2 proof |
|---|---|
| Grammar source | `grammars/yaml.bbnf` lowers to BIR without hand-written Rust. |
| Metadata | workspace metadata registers yaml and feeds template parameters. |
| Runtime emission | `runtime/src/grammars/yaml/*` is generated from BIR/runtime template only. |
| Registry | generated registry sees yaml through metadata, not grammar-name dispatch. |
| Gate | `cargo xtask bbnf build yaml --check && cargo test -p runtime future_grammar_yaml_runtime`. |
| Two-surface invariant | Author input consists only of `grammars/yaml.bbnf` and one `[workspace.metadata.bbnf.grammars.yaml]` block in `Cargo.toml`. Generated runtime/path/visitor/host/diagnostic/budget files may be committed as xtask output, but they are not author inputs. Verify with `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/{ir,codegen,runtime,host,passes}/src/` returns zero and `find crates/runtime/src/grammars/yaml -mindepth 1 -maxdepth 1` returns the generated subdir only. |
| Rejected onboarding path | Manual Rust registry edits, hand-written yaml runtime files, fixture-only admission, and a yaml declaration crate fail onboarding before codegen close. |

## §6 Generated LOC

Generated Rust output starts from PASS-B's 168,750 LOC baseline across 9 grammars (`restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`). Lock 14's budget block starts at 168K and requires budget checks (`restart/locks/LOCKS.md:118-125`). PASS-2 sets an initial +2% ceiling while the template transition lands. Each grammar carries a per-grammar xtask wall ceiling drawn from §6's regen-cycle budget (`single grammar regen ≤ 4s for cohort, ≤ 12s for CSS L4`) and an explicit baseline category:

| Grammar | Current generated_loc | PASS-2 max | Disposition | xtask wall ceiling | Baseline |
|---|---:|---:|---|---:|---|
| bbnf | 21,503 | 21,933 | KEEP-MODIFY | ≤ 4s | observed (PASS-B audit) |
| bnf | 3,290 | 3,356 | KEEP-MODIFY | ≤ 4s | observed (PASS-B audit) |
| csv | 1,693 | 1,727 | KEEP-MODIFY | ≤ 4s | observed (PASS-B audit) |
| css_l4 | 107,138 | 109,281 | REINVENT hotspot | ≤ 12s | observed (PASS-B audit) |
| css_pretty | 9,021 | 9,201 | KEEP-MODIFY | ≤ 4s | observed (PASS-B audit) |
| ebnf | 7,646 | 7,799 | KEEP-MODIFY | ≤ 4s | observed (PASS-B audit) |
| google_sheets | 14,088 | 14,370 | REINVENT host/Pratt surface | ≤ 4s | observed (PASS-B audit) |
| json | 3,500 | 3,570 | KEEP-MODIFY | ≤ 4s | observed (PASS-B audit) |
| math | 871 | 888 | KEEP | ≤ 4s | observed (PASS-B audit) |
| yaml (smoke) | 0 | ≤ 4,000 | future-grammar | ≤ 4s | provisional (owner: SYNTHESIS Wave-2) |
| total | 168,750 | 172,125 | Budget gate | ≤ 22s aggregate | observed for 9 + provisional for yaml |

Carry pointer: SYNTHESIS Wave-2 carries this table into `restart/MASTER-PLAN.md` and `restart/ARCHITECTURE.md` per HARDENING-CONSOLIDATED §4.24; the architecture-side authoritative copy must remain row-for-row identical, with PASS-2 staying the producer-side reference.

Generic monomorphisation budget gate: PASS-2 emits generic-rule instances only from a finite `(RuleId, TypeArgs)` instance set supplied by PASS-1 validation. The lowerers may not discover or instantiate new generic shapes during emission. `cargo xtask bbnf generated-loc-budget --check` records generated LOC by instance set and fails if a generic-cycle diagnostic or missing finite-instance report reaches codegen close.

Generated files are exempt from the per-file LOC cap but not from this budget. Non-generated files still obey Lock 13's 500 LOC cap (`restart/locks/LOCKS.md:58`).

Non-generated budget, child-count floor, and per-area enforcement command. Lock 13 owns the 500 LOC + 4-10 sibling rule (`restart/locks/LOCKS.md:58`); each non-generated area binds the rule to a sibling-count proof and an enforcing command:

| Area | LOC budget | Child-count proof | Enforcing command |
|---|---|---|---|
| `ir/src/backend_ir/*` | No handwritten file > 500 LOC | 4-10 immediate children at `ir/src/backend_ir/`, partitioned by variant family. | `find crates/ir/src/backend_ir -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; `find crates/ir/src/backend_ir -name '*.rs' -exec wc -l {} +` returns no row > 500. |
| `codegen/src/lower/rust/*` | No handwritten file > 500 LOC | 4-10 children partitioned by emitted concern (types, rule, node, scanner, host, pratt, error), not by grammar. | `find crates/codegen/src/lower/rust -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; `find … -name '*.rs' -exec wc -l {} +` returns no row > 500. |
| `codegen/src/lower/wasm/*` | No handwritten file > 500 LOC | 4-10 children sharing BIR tests with Rust; binding path partitioned by ABI/bindgen/host/simd/smoke concern. | `find crates/codegen/src/lower/wasm -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; `find … -name '*.rs' -exec wc -l {} +` returns no row > 500. |
| `codegen/src/runtime_template/*` | No handwritten file > 500 LOC | 4-10 children, concern-split (files, tape, grammar, host, budgets), grammar-agnostic. | `find crates/codegen/src/runtime_template -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; per-file LOC check as above. |
| `runtime/src/*` (handwritten only) | No handwritten file > 500 LOC | 4-10 children: `tape/`, `value/`, `error/`, `visitor/`, `layout/`, `owned/`, `grammars/` (the last is generated, not handwritten). | `find crates/runtime/src -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; `find crates/runtime/src -name '*.rs' -not -path 'crates/runtime/src/grammars/*' -exec wc -l {} +` returns no row > 500. |
| `host/src/*` | No handwritten file > 500 LOC | 4-10 children: `mod.rs`, `primitive.rs`, `registry.rs`, `chain.rs`, `signature.rs`, `wasm.rs`. | `find crates/host/src -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; per-file LOC check as above. |
| `xtask/src/regen/*` | Split before new generation paths land; `regen.rs` does not grow further. | 4-10 children: `mod.rs`, `plan.rs`, `metadata.rs`, `backend_ir.rs`, `runtime.rs`, `write.rs`, `check.rs`, `budget.rs`, `registry.rs`. | `find crates/xtask/src/regen -mindepth 1 -maxdepth 1 \| wc -l` ∈ [4,10]; per-file LOC check as above. |

Generated subdirs (`runtime/src/grammars/<name>/`) are exempt from the 500 LOC cap by Lock 13 (`restart/locks/LOCKS.md:58`). They remain bound by the per-grammar generated LOC table above.

Regen-cycle wall-time budget. Each row carries a baseline category (`observed` against current source, or `provisional (owner)` while measurement lands):

| Cycle | Budget | Baseline | Reason |
|---|---:|---|---|
| `cargo xtask regen --check` after metadata-only change | ≤ 22s | observed (BC iter-gate measurement, `docs/tranches/BC/BC.md:114-118`) | BC sets an iter gate and per-crate check discipline. |
| single grammar regen, cohort grammars | ≤ 4s | observed (PASS-B audit, `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`) | Cohort grammars carry small generated LOC. |
| single grammar regen, css_l4 | ≤ 12s | observed (PASS-B audit) | CSS L4 owns most generated LOC, with 107,138 current generated lines. |
| BIR snapshot print for all 9 grammars | ≤ 5s | provisional (owner: PASS-2 amendment agent; receiver: SYNTHESIS Wave-2 measurement gate) | snapshots are analysis output, not formatting-heavy source generation; baseline lands when the BIR producer pass is implementable enough to measure. |
| write phase | content-equality skip preserves mtime | observed (current regen `xtask/src/regen.rs:400-461`) | regen already skips identical writes; mtime preservation gates downstream cargo invalidation. |
| yaml smoke regen (future grammar) | ≤ 4s | provisional (owner: SYNTHESIS Wave-2; receiver: Tranche G yaml onboarding gate at runtime publication) | smoke is bounded by the cohort budget; baseline lands at first onboarding execution. |

## §7 Perf Gate Trajectory

Performance gates come from README: JSON twitter ≤ 380 µs, canada ≤ 2.8 ms, citm ≤ 750 µs, bootstrap ≤ 3.0 ms, animate ≤ 1.6 ms, and simdjson-class structural scan around 7 GB/s (`restart/README.md:324-340`). SOTA supplies the comparison anchors: sonic-rs measured twitter 436 µs, citm 854 µs, canada 3.144 ms on M1 Pro (`restart/corpora/SOTA.md:50-58`); simd-json reports twitter 424 µs, citm 831 µs, canada 3.27 ms (`restart/corpora/SOTA.md:50-72`); simdjson on-demand sits at 7 GB/s class (`restart/corpora/SOTA.md:73-89`); lightning-css bootstrap/animate are 4.16 ms / 1.97 ms (`restart/corpora/SOTA.md:130-136`).

Throughput trajectory — every parse-throughput row names competitor, dataset, platform, and bbnf target (per HARDENING-CONSOLIDATED §4.29):

| Competitor | Dataset | Platform | bbnf target | PASS-2 mechanism | Evidence artefact |
|---|---|---|---:|---|---|
| sonic-rs `436 µs` / simd-json `424 µs` | JSON twitter | M1 Pro | ≤ 380 µs | `SimdScan` BIR + `simd-scan` structural index | `cargo bench -p bbnf-bench --bench sota_json -- twitter` |
| sonic-rs `854 µs` / simd-json `831 µs` | JSON citm | M1 Pro | ≤ 750 µs | PHF dispatch + tape/direct object traversal | `cargo bench -p bbnf-bench --bench sota_json -- citm` |
| sonic-rs `3.144 ms` / simd-json `3.27 ms` | JSON canada | M1 Pro | ≤ 2.8 ms | array-heavy TapeBuilder + scanner constants | `cargo bench -p bbnf-bench --bench sota_json -- canada` |
| lightning-css `4.16 ms` | CSS bootstrap | M1 Pro | ≤ 3.0 ms | `LayoutScope`, `RegexProgram`, `CallHost`, `SimdScan` | `cargo bench -p bbnf-bench --bench sota_css -- bootstrap` |
| lightning-css `1.97 ms` | CSS animate | M1 Pro | ≤ 1.6 ms | recognizer facts + layout lowering | `cargo bench -p bbnf-bench --bench sota_css -- animate` |
| simdjson on-demand `7 GB/s` | structural scan | M-series | ≥ 5 GB/s | data-only `StructuralAlphabet` + NEON kernel parity | kernel parity + index throughput report |
| simdjson on-demand `7 GB/s` | structural scan | x86 (AVX2/AVX512) | ≥ 7 GB/s | data-only `StructuralAlphabet` + AVX2/AVX512 kernel parity | kernel parity + index throughput report |

Benchmark metadata floor: every PASS-2 benchmark artefact records objective profile, validation mode, source ownership mode, materialisation mode, scalar-cache policy, parse entry point, competitor parse flags, and input hash beside CPU, OS, compiler flags, competitor version, bbnf commit, warmup, and sample policy. `parse(&str)` rows record Rust prevalidation before entry; byte/file parse rows record the bbnf validation path before any `&str`-typed value is exposed. In-situ, destructive, or non-validating competitor modes are not comparable rows unless those modes are named in the metadata.

Mechanism gates — non-throughput rows promoted to mechanism-only proof, distinct from the parse-throughput SOTA gates above:

| Mechanism | PASS-2 obligation | Evidence artefact |
|---|---|---|
| OpenFrame deletion | TapeBuilder length checkpoints + BIR builder-frame replace the cloned-frame substrate; the prior `Vec<OpenFrame>::clone` is the deletion target, not a substrate to preserve. | samply on every emitted parser confirms no `Vec<OpenFrame>::clone` symbol. |
| Pratt auto-detection | Operator-bearing recursive expression families lower to `PrattSpine` LUT and operator-spine state machine. | operator table snapshot + formula fixture under `cargo test -p codegen --test pratt_simd_lowering`. |
| Regex cross-engine parity | `RegexProgram` payloads record VM/lazy-DFA/full-DFA/prefilter execution plan and verifier contract; `parse-that-regex` internal cross-engine parity (VM ↔ lazy DFA ↔ full DFA on the same fixtures) replaces the retired `regex-automata` oracle lane per Lock 11 line 54 + audit #4 §4. | seed regex fixture report for Unicode class algebra, no-capture DFA, lazy-DFA, VM, and prefilter candidate discard. |
| V2 WASM parity | wasm32 binding path with scalar/SIMD-128 scan parity, sharing the Rust BIR. | V2 `WasmBackend` smoke + WASM bench handoff to packaging. |

PASS-2 should not claim final perf wins until generated parsers run the corpus, but it defines the only mechanisms by which those gates can be met.

Per-construct contribution plan:

| Construct | Expected contribution | Anchor |
|---|---|---|
| `Alt { mode: Dispatch }` | Removes speculative checkpoints for byte-disjoint alts. | old sketch says byte-disjoint alts still paid wasted checkpoint cost (`restart/corpora/RESTART-SKETCH.md:201-217`). |
| `Alt { mode: Speculative }` | Converts rollback from cloned frames to length truncation. | old checkpoint clone dominated samples (`restart/corpora/RESTART-SKETCH.md:154-184`). |
| `SimdScan` | Moves delimiter discovery toward simdjson-style structural scan. | simdjson structural index and On-Demand anchor (`restart/corpora/SOTA.md:73-89`). |
| `RegexProgram` | Keeps Unicode work in regex engine and off grammar scanner. | README regex Unicode decision (`restart/README.md:131-143`). |
| `PrattSpine` | Replaces layered recursive descent for expression families with loop/table dispatch. | current generated Rust already carries Pratt LUT inheritance (`crates/core/src/backend/rust/emitter/grammar.rs:194-202`). |
| `CallHost` | Moves chained host functions into typed generic calls. | README host fn and chaining scope (`restart/README.md:145-166`). |
| `LayoutScope` | Centralizes skip policy and prevents repeated whitespace scanning. | `@layout` in V1 (`restart/README.md:176-178`). |

Runtime emission table — per-grammar runtime files plus emission source. Every cell is template-emitted or data-only; hand-written runtime files are forbidden (Lock 14 generic-fleet posture, `restart/locks/LOCKS.md:60`):

| Grammar | `generated.rs` | `parser.rs` | `host.rs` | host source | layout source | error source | Pratt/SIMD source |
|---|---|---|---|---|---|---|---|
| bbnf | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + `@host fn` blocks in `bbnf.bbnf` | `@layout` analysis output | `@error` analysis output | `PrattSpine` LUT for grammar operator family |
| bnf | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table (empty) | metadata only | `@layout` analysis output | `@error` analysis output | none (no Pratt/SIMD) |
| csv | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + escape host fns | `@layout` analysis output | `@error` analysis output | `SimdScan` for delimiter alphabet |
| css_l4 | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + colour/length host fns | `@layout` analysis output | `@error` analysis output | `SimdScan` for structural alphabet |
| css_pretty | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + format host fns | `@layout` analysis output | `@error` analysis output | none |
| ebnf | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata only | `@layout` analysis output | `@error` analysis output | none |
| google_sheets | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + formula host chains | `@layout` analysis output | `@error` analysis output | `PrattSpine` for operator precedence |
| json | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + numeric/string host fns | `@layout` analysis output | `@error` analysis output | `SimdScan` for structural alphabet (twitter/citm/canada hot path) |
| math | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + numeric host fns | `@layout` analysis output | `@error` analysis output | `PrattSpine` for operator precedence |
| yaml (smoke) | BIR snapshot + tape kinds + view structs | parse fn signatures + entry control | host-fn dispatch table | metadata + `@host fn` only | `@layout` analysis output | `@error` analysis output | auto-detected from grammar shape |

Hand-written prohibition: every column is generated by `cargo xtask regen --check`; `rg -n "// hand-written" crates/runtime/src/grammars/` returns zero outside generated headers. Any per-grammar runtime file that escapes the template returns to PASS-2 amendment, not to a one-shot patch.

Required smoke per grammar (kept from prior table):

| Grammar | Smoke gate |
|---|---|
| bbnf | parse grammar corpus and emit metadata. |
| bnf | parse canonical BNF fixtures. |
| csv | parse row/quote fixtures. |
| css_l4 | parse bootstrap/animate fixtures. |
| css_pretty | parse pretty-print fixtures. |
| ebnf | parse EBNF corpus. |
| google_sheets | parse formula fixtures and host chains. |
| json | parse twitter/citm/canada fixtures. |
| math | parse Pratt expression fixtures. |
| yaml | future-grammar smoke from source + metadata only. |

## §8 Inheritance Ledger

| Inheritance | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| PASS-B codegen/runtime audit | Emitter collapse, runtime-template pivot, generated LOC baseline (`restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`, `restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`). | Apply to Tape/direct-to-struct and current no-prefix workspace. | Direct-only/no-tape language. |
| Amendment 01 | Zero per-grammar crates and metadata-driven onboarding (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-32`). | Host functions through `host` generic primitives and generated host glue. | 9 declaration-crate workspace. |
| BC typed IR | Variant discipline and lowering rules (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:28-290`). | Add lookbehind, tape shape, host chains. | Unicode grammar node and stale per-grammar host namespace. |
| BB cohort template | Parameter table and byte-equality regen (`docs/tranches/BB/audit/W2-cohort-template-spec.md:8-22`, `docs/tranches/BB/audit/W2-cohort-template-spec.md:40-61`). | Expand to all 9 grammars and Tape-backed views. | Cohort special mode. |
| `simd-scan` | Clean crate, arch dispatch, data-driven alphabet (`restart/corpora/MODULES.md:47-69`, `crates/simd-scan/src/alphabet.rs:1-18`). | BIR-fed alphabets and parity snapshots. | Grammar-specific scan code. |
| Current source | Existing PHF/Pratt/scanner implementation knowledge (`crates/core/src/backend/rust/emitter/grammar.rs:155-202`). | Split across BIR, lowerer, runtime template. | Direct Grammar IR walk and runtime god module. |

Wave-by-wave carries:

| Wave | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| BC.W0 | Typed IR boundary and variant-table discipline (`docs/tranches/BC/waves/W0.md:10-28`). | Expand final BIR to the 20-row upstream alphabet (19 semantic variants plus `Return`, post Phase-8.4 α3) plus PASS-2's payload-refiner table including Tape shapes. | 7-variant placeholder. |
| BC.W1 | Rust emitter consumes typed IR and regen equality (`docs/tranches/BC/waves/W1.md:10-64`). | Rust lowerer consumes BIR only and writes TapeBuilder code. | Stale tape-residue/no-tape language in BC.W1 (`docs/tranches/BC/waves/W1.md:82-88`). |
| BC.W2 | TS/WASM scaffold compile and smoke discipline (`docs/tranches/BC/waves/W2.md:10-58`). | Keep TS scaffold, turn WASM toward wasm32 binding path. | Treat TS production as PASS-2. |
| BC.W3 | Crate split and Lock 13/5/6 attention (`docs/tranches/BC/waves/W3.md:10-87`). | Re-anchor to current README crate names and Tape runtime. | Old `bbnf-` internal crate naming. |
| BD.W0 | TS/NAPI path inheritance and graceful host-fn failure (`docs/tranches/BD/waves/W0.md:10-15`, `docs/tranches/BD/waves/W0.md:95-101`). | Carry as PASS-3 packaging handoff. | PASS-2 production TS claim. |
| BD.W1 | TS shape table and host resolution inheritance (`docs/tranches/BD/waves/W1.md:28-71`). | BIR remains TS-capable. | TS runtime activation in PASS-2. |
| BD.W2 | wasm-bindgen production path and host import table (`docs/tranches/BD/waves/W2.md:38-62`, `docs/tranches/BD/waves/W2.md:165-183`). | Apply to wasm32 binding layer with Tape core. | Raw WAT as production path. |
| BD.W3 | Publication order and semver check discipline (`docs/tranches/BD/waves/W3.md:8-27`, `docs/tranches/BD/waves/W3.md:79-96`). | Route to PASS-3/package release work. | Publishing claims in PASS-2. |
| BD.W4 | Fleet fixture and matrix shape (`docs/tranches/BD/waves/W4.md:8-27`, `docs/tranches/BD/waves/W4.md:187-197`). | Use as downstream parity fixtures. | PASS-2 fixture fleet implementation. |

Diagnostic ledger:

Per Phase-8.4 β1, codes carry human-readable names only; the prior numeric-suffix aliases (`BBNF-GEN014`, `BBNF-CODEGEN021`, `BBNF-CODEGEN033`, `BBNF-LIFE009`, `BBNF-SEM040`, `BBNF-OPT001`, `BBNF-OPT002`) retire in favour of the names below. ARCH §7.4 catalogue carries the deletion archaeology for any reader following the old aliases.

| Code | Trigger | PASS-2 producer | Verbatim string |
|---|---|---|---|
| `BBNF-GRAMMAR-IR-IN-CODEGEN` | lowerer imports Grammar IR or source AST. | import-deny check. | `"lowerer at {file} imports Grammar IR; codegen consumes Backend IR only"` |
| `BBNF-GENERATED-LOC-OVER-BUDGET` | generated LOC exceeds per-grammar or total budget. | regen budget check. | `"grammar {name} generated_loc {actual} exceeds budget {max}; ratchet upstream"` |
| `BBNF-BIR-SNAPSHOT-DRIFT` | BIR snapshot changed without committed generated output. | regen equality. | `"BIR snapshot for {grammar} drifted; rerun cargo xtask regen --check and commit the diff"` |
| `BBNF-RUNTIME-TEMPLATE-METADATA-MISSING` | runtime template lacks path/visitor/diagnostic metadata. | metadata consumer smoke. | `"runtime template for {grammar} omits {metadata}; PASS-3 consumer cannot bind"` |
| `BBNF-LIFETIME-CONSTRUCTOR-MISMATCH` | emitted owned/borrowed constructor violates lifetime surface. | runtime compile tests. | `"emitted constructor for {rule} returns {actual} but rule annotation {annot} requires {expected}; check @layout(...) hint or grammar -> projection"` |
| `BBNF-LOOKBEHIND-UNBOUNDED-AT-BIR` | unbounded lookbehind reaches BIR. | BIR validation. | `"lookbehind in rule {rule} reaches BIR with unbounded width; PASS-1 BBNF-LOOKBEHIND-WIDTH should have caught upstream"` |
| `BBNF-PRATT-NOT-APPLIED` | optimizer rejects an apparent operator-chain candidate. | cost-model decision. | `"rule {rule} resembles an operator chain (left-recursive with operator-bearing alts at {line}) but {reason}; objective profile {profile} selected {fallback}; PrattSpine was not auto-selected; add stable precedence metadata or restructure the rule"` |
| `BBNF-SIMD-NOT-SELECTED` | optimizer rejects an apparent SIMD candidate. | cost-model decision. | `"rule {rule} has structural alphabet {alpha} but kernel-shape evidence is {shape}; falling back to scalar or regex verifier-first because SIMD cost or exactness evidence did not win under objective profile {profile}; metadata may disable unsupported kernels but cannot force SIMD"` |

Carry ledger — every deferral carries Receiver, Blocker, and Receiving gate per HARDENING-CONSOLIDATED §4.39:

| Item | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| PASS-1 reconciliation: Grammar IR to BIR handoff | SYNTHESIS Wave-2 and Tranche E (typed-IR consolidation) | PASS-1 final variant schema and side-table layout differ from the 20-row upstream alphabet (19 semantic variants plus `Return`, post Phase-8.4 α3) projected through PASS-2's payload-refiner table at §2. | Stable BIR snapshot for every extant grammar plus yaml smoke; `cargo xtask bbnf bir --all --check` returns identical bytes against the committed snapshot. |
| PASS-3 API docs and metadata consumption | SYNTHESIS Wave-2 and Tranche G (PASS-3 runtime publication) | Runtime template omits path/visitor/diagnostic metadata or PASS-3 hand-writes wrappers per grammar. | PASS-3 consumer acceptance gates listed at the close of §4 (`parse_signature_compile`, `view_metadata_visitor`, `view_metadata_selector`, `cost-table --check`, `grammar_schema_load`, `diagnostic_vocabulary`, V2 `wasm_abi_descriptor`). |
| TS production | V2 `TsBackend: Backend` impl | TS production defers post-V1; BIR shape supports TS lower without retrofit when scope opens. | TS scaffold smoke remains proof-only at PASS-2; TS production gate lands in V2 with the same BIR snapshot consumed by Rust V1. |
| V2 parity matrix | V2 backend cycle and Tranche J final close | Rust V1 runs the seed grammar parity matrix on the V1 line; WASM/TS parity waits for V2 `WasmBackend: Backend` / `TsBackend: Backend` registration. | V1 Rust/VM parity matrix plus J.W1 final-close numeric SOTA gate; V2 owns cross-backend parity. |
| Publication (`bbnf` aggregator + `bbnf-cli` + `bbnf-language-server`) | Tranche BD.W3 (publication) and SYNTHESIS package routing | Workspace crate names are bound; package-name details are not yet routed. | A.W1 / J.W3 publication gate per HARDENING-CONSOLIDATED §4.22; PASS-2 supplies emitted runtime modules and parse signatures. |
| Fixtures (post-onboarding parity, not onboarding surface) | Tranche BD.W4 (fleet fixtures) and downstream parity gates | Lock 14 onboarding accepts only grammar source + workspace metadata; fixtures land separately to avoid third-surface inflation. | BD.W4 fleet-fixture gate (`docs/tranches/BD/waves/W4.md:8-27`); PASS-2 emits the runtime modules that fixtures exercise. |
| `path-ts` proc-macro shell | V2 `TsBackend: Backend` cycle | Rust toolchain forbids proc-macro path-dep sharing; `path-ts` lives outside V1 because TS production defers post-V1 (`restart/locks/LOCKS.md:42`, `restart/locks/LOCKS.md:46`). | V2 `path-ts` builds against the same `path-core` AST + compile logic that `path` consumes; PASS-2 has no V1 `path-ts` obligation. |
| WASM host primitive ABI descriptor + npm packaging | V2 `WasmBackend: Backend` impl | WASM host primitives are emitted as lowerer/runtime ABI descriptors, not grammar annotations; packaging surface is downstream. | V2 wasm-bindgen production path consumes exported function names, host-call shape rows, marshalling descriptors, and scalar/SIMD parity evidence without runtime trait dispatch. |

## §9 Punch List

1. Implement `ir::backend_ir` with the 20-row upstream alphabet (19 semantic variants plus `Return`, post Phase-8.4 α3) projected through PASS-2's payload-refiner table and the snapshot printer.
2. Add PASS-1 to PASS-2 handoff tests once PASS-1 artefacts exist: Grammar IR to BIR, cost plan, host signatures, layout/error annotations.
3. Replace the broad `Emitter` trait with BIR consumer APIs and enforce import-deny checks.
4. Build `runtime/src/tape/` and TapeBuilder checkpoints; delete OpenFrame-style runtime builders before migration begins. The OpenFrame substrate has no preserved role in PASS-2 generic runtime/codegen plan text; only the deletion-pathology archaeology survives, and TapeBuilder + BIR builder-frame replaces every checkpoint surface.
5. Emit generated grammar modules under `runtime/src/grammars/<name>/` from one template.
6. Split xtask regen and add per-grammar generated LOC budgets.
7. Wire `simd-scan` through BIR `StructuralAlphabet` constants, exact-scan scalar parity fixtures, and prefilter verifier-before-tape fixtures.
8. Build Rust V1 lowerer first; keep WASM/TS scaffolds as V2 backend proof only.
9. Add conflict guard checks for `ParseStream`, rewrite-mode walker, grammar-level Unicode sets, and default per-grammar declaration crates.
10. Leave cross-backend 81-cell parity and public package surfaces to PASS-3/BD.

## §9b Phase 8.4 PASS-2 Fold Classification

Phase 8.4 absorbs the V8 ledger surgeries routed to PASS-2 (`restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` §3 + `restart/audit/hardening/HARDENING-PASS-2-V8.md` §6). Each PASS-2-scoped item is classified ACCEPT or ROUTE before edits land:

| V8 item | Tier | Verdict | PASS-2-side surgery |
|---|---|---|---|
| α1 Backend trait 5 → 2 (`emit_artefacts`) | architectural cardinality | ACCEPT | Recast §A integration prose to two-method trait — `lower(bir, ctx)` + `emit_artefacts(grammar, schemas) -> ArtefactSet`; obligation table folds the four `emit_*` rows into one `emit_artefacts` row. The ARCH §7.5 trait surgery is SYNTHESIS-fold scope; PASS-2 mirrors the post-fold trait. |
| α3 BIR alphabet 22 → 20 (three pair-collapses plus `Return`) | architectural cardinality | ACCEPT | The three pairs per PASS-1 V8 surfacing: (`AltDispatch`, `AltSpeculative`) → `Alt { mode }`; (`LayoutPush`, `LayoutPop`) → `LayoutScope { kind }`; (`CallHost`, `HostChain`) → `CallHost` (chains express as `Seq`-of-`CallHost`). PASS-2's refiner table maps the resulting 19 semantic variants plus `Return`. Authoritative alphabet count (post-fold) = 20; PASS-2 cites the upstream count without re-owning. |
| α7 Internal `BackendLowerer` clarification | architectural cardinality | ACCEPT | Clarify in §A that the internal trait carries no V1 polymorphism (single `RustLowerer` impl); future V2 `WasmLowerer` / `TsLowerer` impls inherit the same trait without polymorphism widening. The 8-method shape is per-rule emission decomposition, not contract gate. |
| β1 Retire diagnostic numeric alias system (PASS-2 ledger) | diagnostic vocabulary | ACCEPT | Drop `BBNF-GEN014`, `BBNF-CODEGEN021`, `BBNF-CODEGEN033`, `BBNF-LIFE009`, `BBNF-SEM040`, `BBNF-OPT001`, `BBNF-OPT002` numeric-suffix forms. Rename to human-readable codes — `BBNF-GENERATED-LOC-OVER-BUDGET`, `BBNF-BIR-SNAPSHOT-DRIFT`, `BBNF-RUNTIME-TEMPLATE-METADATA-MISSING`, `BBNF-LIFETIME-CONSTRUCTOR-MISMATCH`, `BBNF-LOOKBEHIND-UNBOUNDED-AT-BIR`, `BBNF-PRATT-NOT-APPLIED`, `BBNF-SIMD-NOT-SELECTED`. The last two carry alphabetic-only spellings; the alias forms in ARCH §7.4 stay as deletion archaeology pointing at the human-readable canon. |
| γ1 Closure capture: Rust borrow checker leverage | host-language leverage | ACCEPT | §2 function-value lowering row reframes the closure-frame validation as rustc-delegated; PASS-2 emits the lowered closure source, and rustc proves lifetime soundness at downstream `cargo check`. PASS-2 carries no closure-lifetime audit machinery beyond emission. |
| γ8 Generic monomorphisation: rustc leverage | host-language leverage | ACCEPT | §2 function-value lowering row + §6 generic monomorphisation budget gate reframe to: PASS-2 emits monomorphised Rust source from the finite `(RuleId, TypeArgs)` instance set; rustc completes the Rust-side monomorphisation. The bbnf-side budget gate audits LOC growth from the finite instance set; rustc handles substitution. |
| ε1 23 vs 24 alphabet count reconciliation | hygiene | SUPERSEDED-BY-α3 | The 23-vs-24 reconciliation dissolves under α3 because both sides converge on the post-fold 20-row alphabet (19 semantic variants plus `Return`; PASS-2's historical `Layout` collapse + ARCH's `LayoutScope` collapse name the same single variant). |
| ε2 Cost-model trait upstream-owner citation | hygiene | ACCEPT | §5 PASS-1 handoffs row "Cost model trait and scores" cites the upstream owner explicitly: `crates/cost-model/` per `restart/corpora/MODULES.md` + ARCH §10 `CostFacts` producer; PASS-2 consumes `CostDecision` records without re-owning the trait. |
| ε3 `parse_in` arena vs closure-frame | hygiene | ACCEPT | §2 closure-frame row carries an explicit clarification: `parse_in(input, &bump)`'s arena lifetime bounds input-data extension only; closure-environment frames stay stack-bound regardless of arena entry point. The two memory regions partition cleanly. |
| ε4 E-graph rewrite-category cardinality | hygiene | ACCEPT-AS-CONSUMER | §5 PASS-1 handoffs row cites the cardinality at ARCH §10.1 without restating the per-category classification. PASS-2 is consumer; PASS-1 + ARCH §10 own the inventory. |
| Tier δ (post-V1 surfaces) | meta-grammar | NONE-IN-SCOPE | PASS-2 carries no post-V1-routed meta-grammar surface; the §A obligation-table column header retires the prior post-V1-routing framing per α1 fold and now names the receiver as "Post-V1 receiver" instead. |

The fold is bounded strictly to `restart/audit/pass-2-codegen/PASS-2.md`. ARCH §7.5 trait collapse + ARCH §7.2 alphabet collapse + ARCH §7.4 catalogue retire are SYNTHESIS-fold scope; PASS-1 and PASS-3 ledger β1 retires run in their own fold agents.

## §10 Closing Posture

PASS-2 closes on a clear middle-layer architecture: Backend IR is the only codegen input; Tape is the only runtime substrate; typed values borrow into Tape; Rust V1 lowerers consume BIR now; V2 WASM/TS lowerers consume the same BIR when their `Backend` impls land; `simd-scan` stays generic; Pratt/SIMD/PHF are auto-detected; runtime modules are template-emitted from grammar source plus metadata; regen is byte-identical and budgeted.

The research-fold grounding is binding: `CostDecision` and bridge evidence drive extraction before lowerers run; `TapeShape` and `ValueShape` project one tape identity; `RegexProgram` is a verifier-bound regex-program payload, not a full-DFA mandate; exact SIMD scans prove scalar parity, prefilters verify before tape emission, and SOTA benchmark rows carry validation/source-ownership metadata.

The main inherited contradictions are resolved, not carried. ParseStream is stale. Rewrite mode is out. Grammar-level Unicode sets are out. Per-grammar declaration crates are not default and are unused for the 9 existing grammars. TS production is deferred. The remaining work is implementation and cross-pass reconciliation with PASS-1 and PASS-3, not another architecture fork.
