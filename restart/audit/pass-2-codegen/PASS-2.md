# PASS-2 Synthesis: Codegen + Runtime + Backends

## §1 Verdict Ledger

PASS-2 is viable only as a replacement of the current codegen/runtime wiring, not a patch of it. The prompt assigns this pass Backend IR, Rust V1, WASM V1, TS deferral, runtime template, SIMD scanner kernels, Pratt/SIMD auto-detection, and regen equality (`restart/prompts/PASS-2-CODEGEN.md:3`). Lock 5 requires an IR plus per-backend lowerers and forbids direct grammar walking by codegen (`restart/locks/14-LOCKS.md:42`). Current source violates that boundary because the shared driver says it walks `GrammarIR` (`crates/core/src/backend/driver/mod.rs:1-6`) and the broad `Emitter` trait is invoked from that grammar-walking driver (`crates/core/src/backend/emitter.rs:1-13`).

Verdict: **REINVENT codegen around Backend IR, Tape-backed runtime template, and BIR-only lowerers.** Keep the useful implementation knowledge in current Rust/WASM emitters, PHF/Pratt tables, `simd-scan`, xtask content-equality writing, and BB cohort template work. Discard stale ParseStream naming, grammar-level Unicode sets, rewrite-mode walker, per-grammar declaration crates as default, OpenFrame checkpointing, and direct Grammar IR consumers in lowerers.

Conflict ledger:

| Conflict | Stale authority | Settled resolution |
|---|---|---|
| Tape name | PASS-2 prompt says Lock 1 honours a ParseStream union (`restart/prompts/PASS-2-CODEGEN.md:81`); inheritance says tape name dies (`restart/inheritance/INDEX.md:65-66`). | Tape is the substrate, unioned with direct-to-struct typed values (`restart/locks/14-LOCKS.md:34`, `restart/README.md:285-314`). |
| Rewrite mode | Inheritance and prompt still mention rewrite-mode (`restart/inheritance/INDEX.md:34`, `restart/prompts/PASS-2-CODEGEN.md:33`). | Rewrite mode is rejected; Visitor covers transformations (`restart/README.md:123-129`). |
| Unicode set surface | PASS-2 prompt names Unicode-set (`restart/prompts/PASS-2-CODEGEN.md:33`). | Grammar-level Unicode class algebra is not added; regex literals and `parse-that/regex` carry Unicode (`restart/README.md:131-143`). |
| Per-grammar declaration crates | MASTER-PLAN contains 9 declaration crates (`restart-archive-2026-05-04/audit/master-plan/MASTER-PLAN.md:79-89`), and Lock 14 still names an optional escape (`restart/locks/14-LOCKS.md:60`). | No declaration crate for the 9 extant grammars; two onboarding surfaces plus generic host primitives or `@host fn` (`restart/README.md:13-25`). |
| TS production | BD.W1 plans production TS (`docs/tranches/BD/waves/W1.md:10-24`). | PASS-2 has TS scaffold capability only; production TS is deferred by the PASS-2 prompt (`restart/prompts/PASS-2-CODEGEN.md:3`). |

Sub-agent dispatch status:

| Agent | Lens | Output | Verdict |
|---|---|---|---|
| 1 | Backend IR Architect | `restart/audit/pass-2-codegen/agent-1-backend-ir-architect.md` | 23-variant BIR, Tape shapes, host chains. |
| 2 | Rust Lowerer Architect | `restart/audit/pass-2-codegen/agent-2-rust-lowerer-architect.md` | Rust lowerer consumes BIR, emits TapeBuilder checkpoints. |
| 3 | WASM Lowerer + SIMD Architect | `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md` | wasm32 Rust binding path, raw WAT demoted to smoke fixture. |
| 4 | Runtime Template Architect | `restart/audit/pass-2-codegen/agent-4-runtime-template-architect.md` | Generated grammar modules under `runtime/src/grammars/<name>/`. |
| 5 | Pratt + SIMD Auto-Detection | `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md` | Auto-detected Pratt/PHF/SIMD with decision logs. |
| 6 | Codegen Coherence Auditor | `restart/audit/pass-2-codegen/agent-6-codegen-coherence-auditor.md` | Boundary, regen, genericity, budget gates. |

## §2 Commitments

1. **Backend IR is the PASS-2 boundary.** PASS-1 produces it after parse, validate, type inference, shape mining, e-graph, cost extraction, and lower-to-BIR (`restart/README.md:188-217`). Rust, WASM, and TS-stub lowerers consume it. No lowerer imports Grammar IR.

2. **The BIR variant set is 23 nodes.** The BC 22-variant table is kept as the base (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:28-290`). PASS-2 adds `Lookbehind`, folds multi-function chaining into `HostCall`, and keeps Unicode inside `RegexDfa`.

3. **Tape/direct-to-struct is one materialisation plan.** Every rule has a `TapeShape` and `ValueShape`. Typed documents/views borrow `&'i Tape<'i>` plus node id. This follows Lock 1 (`restart/locks/14-LOCKS.md:34`) and avoids the prior OpenFrame checkpoint clone that dominated samples (`restart/corpora/RESTART-SKETCH.md:154-184`).

4. **Rust V1 is the primary production lowerer.** The lowerer emits parser functions, TapeBuilder operations, typed views, scanner constants, Pratt tables, host chain calls, diagnostics, and generated registry data from BIR.

5. **WASM V1 is wasm32 Rust plus binding layer.** Current raw WAT remains a smoke fixture; production lowering emits wasm32-compatible Rust parser core and binding ABI. BD.W2's inheritance supports wasm-bindgen source as the production path rather than raw WAT (`docs/tranches/BD/waves/W2.md:38-62`).

6. **SIMD scanner remains generic.** `simd-scan` already has NEON, AVX2, AVX512, WASM, and scalar modules (`crates/simd-scan/src/lib.rs:19-29`). BIR emits data-only `StructuralAlphabet` constants; no grammar code enters `simd-scan`.

7. **Runtime template emits all per-grammar runtime files.** `runtime/src/grammars/<name>/{generated.rs, parser.rs, host.rs}` is generated from grammar source plus metadata. Current manual runtime module listings are discarded (`crates/core/src/runtime/mod.rs:8-72`).

8. **Regen equality is a hard gate.** Lock 6 requires xtask-emitted committed source (`restart/locks/14-LOCKS.md:44`). PASS-2 keeps content-equality writing from current regen (`xtask/src/regen.rs:400-461`) but splits the module and adds BIR/runtime budget checks.

9. **No per-grammar declaration crates by default.** Amendment 01 retracts them (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-24`), and README makes two onboarding surfaces authoritative (`restart/README.md:13-25`).

Backend IR final variant table:

| # | Variant | Payload | Generation site | Rust lowering | WASM lowering | TS status |
|---|---|---|---|---|---|---|
| 1 | `Rule` | name, params, value shape, tape kind | PASS-1 BIR lower | parse fn plus typed view | wasm32 parse export wrapper | scaffold shape |
| 2 | `Seq` | child ids, field map | rule lowering | ordered child calls | same core | scaffold |
| 3 | `AltDispatch` | dispatch keys, arms | cost model | match/PHF/scan dispatch | same core | scaffold |
| 4 | `AltSpeculative` | arms, checkpoint policy | cost model | cursor/tape checkpoint | same core | scaffold |
| 5 | `Repeat` | body, separator, bounds | rule lowering | loop with progress guard | same core | scaffold |
| 6 | `Optional` | body, absence policy | rule lowering | optional branch | same core | scaffold |
| 7 | `Ref` | target rule, args | rule lowering | rule call | same core | scaffold |
| 8 | `Lit` | bytes, case policy | rule lowering | byte compare | same core | scaffold |
| 9 | `Keyword` | keyword set, case policy | keyword detector | branch/PHF | same core | scaffold |
| 10 | `CharClass` | byte class | rule lowering | predicate | same core | scaffold |
| 11 | `Scanner` | scan plan | shape miner | scanner callback | scanner callback | scaffold |
| 12 | `RegexDfa` | compiled regex, Unicode metadata | regex compiler | regex call | wasm-compatible regex call | scaffold |
| 13 | `Span` | span projection | rule lowering | span value | offset/len | scaffold |
| 14 | `Layout` | skip policy | `@layout` analysis | layout consume | same core | scaffold |
| 15 | `MapExpr` | projection expression | type/e-graph lower | typed conversion | same core | scaffold |
| 16 | `HostCall` | host chain, generics, error policy | host inference | `host` registry call | primitive or extern import | scaffold |
| 17 | `FoldResult` | accumulator, fold op | rule lowering | accumulator state | same core | scaffold |
| 18 | `EnumDiscriminator` | enum arm/table | type lower | enum tag | numeric tag | scaffold |
| 19 | `PrattSpine` | operator table, associativity | Pratt detector | Pratt loop | same core | scaffold |
| 20 | `SimdScan` | structural alphabet, kernel shape | SIMD detector | `simd-scan` call | wasm/scalar scan | scaffold |
| 21 | `Lookbehind` | bounded predicate | lookbehind analysis | reverse predicate | reverse predicate | scaffold |
| 22 | `ErrorRecovery` | strategy, diagnostic | `@error` analysis | diagnostic/recovery edge | same core | scaffold |
| 23 | `DebugMarker` | source map marker | debug config | optional metadata | optional metadata | scaffold |

Cardinality defence: BC's research anchors put the useful compiler-IR band at roughly 20-30 variants, comparing MLIR `arith` at 60, Cranelift `InstructionData` at 40, rustc HIR `ExprKind` at 35, rustc HIR `ItemKind` at 16, and chalk `TyKind` at 23 (`docs/tranches/BC/audit/research-anchors.md:12-18`, `docs/tranches/BC/audit/W0-typed-ir-variant-table.md:319-329`). PASS-2's 23 variants stay inside that band. swc is kept as backend-separation inheritance rather than a cardinality bound because local README cites swc for WASM/codegen pipeline shape (`restart/README.md:369`), and current rustdoc shows JavaScript AST statement/expression sums with different domain pressure (`https://rustdoc.swc.rs/swc_ecma_ast/enum.Stmt.html`, `https://rustdoc.swc.rs/swc_ecma_ast/enum.Expr.html`).

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
| Pratt | recursive expression family has operator-bearing prefix/infix/postfix alternatives and a total precedence order. | recursion lacks operator partition or width/progress proof. | Lock 10 auto-detects Pratt (`restart/locks/14-LOCKS.md:52`). |
| SIMD | structural byte alphabet is non-empty, kernel shape is not `Empty`, and cost score beats scalar for expected input length. | alphabet is Unicode-semantic, tiny, or scanner setup cost wins. | `KernelShape` categories exist in `simd-scan` (`crates/simd-scan/src/alphabet.rs:98-125`). |
| PHF | literal/keyword set is large enough that hash dispatch beats match-tree under cost model. | small sets or prefix-overlap make branch tree cheaper. | current Rust emission already has PHF keyword table path (`crates/core/src/backend/rust/emitter/grammar.rs:155-163`). |
| Lookbehind | predicate width is fixed or bounded by PASS-1 analysis. | unbounded lookbehind. | lookbehind is grammar-level V1 (`restart/README.md:125-129`). |

## §3 Per-Crate Trees

`codegen`:

```text
codegen/src/
  backend_ir/
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

Rationale: the current `Emitter` surface spans hundreds of lines (`crates/core/src/backend/emitter.rs:31-566`). PASS-B already called for collapsing it to 8-10 methods (`restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`). The new tree makes Backend IR a crate-local contract and lowerers shallow consumers.

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

Rationale: Lock 1 places tape at `runtime/src/tape/` (`restart/locks/14-LOCKS.md:34`). Lock 13 rejects the current mixed runtime god directory (`restart/locks/14-LOCKS.md:58`). Every `<name>` subdir is generated and structurally identical.

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
| Error vocabulary | BIR `ErrorRecovery` and runtime `error` | user diagnostics |
| Host table metadata | `host` registry plus generated `host.rs` | package integration and user host bindings |
| WASM ABI descriptor | `codegen/lower/wasm/abi.rs` | npm/browser packaging |
| Materialisation cost table | BIR decision log | API docs and performance explanation |

BD.W5 parity is not a PASS-2 close gate. It remains downstream: 9 grammars times at least 3 fixtures times 3 backends, for at least 81 cells (`docs/tranches/BD/waves/W5.md:181-217`).

## §5 PASS-1 Handoffs

PASS-2 assumes PASS-1 will provide:

| PASS-1 Product | PASS-2 Use |
|---|---|
| Grammar IR variants and node ids | source-to-BIR traceability |
| Type layouts and generics | `ValueShape`, host signatures, view fields |
| Cost model trait and scores | alt dispatch, PHF, SIMD, Pratt choices |
| Shape mining outputs | seq/alt/repeat materialisation and scanner plans |
| E-graph extraction | simplified rule bodies before BIR |
| Lookbehind width analysis | bounded `Lookbehind` BIR node |
| Layout and error annotations | `Layout` and `ErrorRecovery` nodes |
| Host function inference | `HostCall` chains |

The PASS-1 prompt names these products as hand-offs (`restart/prompts/PASS-1-SUBSTRATE.md:57-70`). In this parallel Phase 1 run, PASS-1 output is intentionally unavailable; SYNTHESIS must reconcile final PASS-1 artefacts later.

## §6 Generated LOC

Generated Rust output starts from PASS-B's 168,750 LOC baseline across 9 grammars (`restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`). Lock 14's budget block starts at 168K and requires budget checks (`restart/locks/14-LOCKS.md:118-125`). PASS-2 sets an initial +2% ceiling while the template transition lands:

| Grammar | Current | PASS-2 max | Disposition |
|---|---:|---:|---|
| bbnf | 21,503 | 21,933 | KEEP-MODIFY |
| bnf | 3,290 | 3,356 | KEEP-MODIFY |
| csv | 1,693 | 1,727 | KEEP-MODIFY |
| css_l4 | 107,138 | 109,281 | REINVENT hotspot |
| css_pretty | 9,021 | 9,201 | KEEP-MODIFY |
| ebnf | 7,646 | 7,799 | KEEP-MODIFY |
| google_sheets | 14,088 | 14,370 | REINVENT host/Pratt surface |
| json | 3,500 | 3,570 | KEEP-MODIFY |
| math | 871 | 888 | KEEP |
| total | 168,750 | 172,125 | Budget gate |

Generated files are exempt from the per-file LOC cap but not from this budget. Non-generated files still obey Lock 13's 500 LOC cap (`restart/locks/14-LOCKS.md:58`).

Regen-cycle wall-time budget:

| Cycle | Budget | Reason |
|---|---:|---|
| `cargo xtask regen --check` after metadata-only change | ≤ 22s | BC sets an iter gate and per-crate check discipline (`docs/tranches/BC/BC.md:114-118`). |
| single grammar regen | ≤ 4s for cohort, ≤ 12s for CSS L4 | CSS L4 owns most generated LOC, with 107,138 current generated lines (`restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`). |
| BIR snapshot print for all 9 grammars | ≤ 5s | snapshots are analysis output, not formatting-heavy source generation. |
| write phase | content-equality skip preserves mtime | current regen already skips identical writes (`xtask/src/regen.rs:400-461`). |

## §7 Perf Gate Trajectory

Performance gates come from README: JSON twitter ≤380us, canada ≤2.8ms, citm ≤750us, bootstrap ≤3.0ms, animate ≤1.6ms, and simdjson-class structural scan around 7 GB/s (`restart/README.md:324-340`). SOTA supplies the comparison anchors: sonic-rs measured twitter 436us, citm 854us, canada 3.144ms on M1 Pro (`restart/corpora/SOTA.md:50-58`), simdjson on-demand is around 7 GB/s (`restart/corpora/SOTA.md:73-89`), and lightning-css bootstrap/animate are 4.16ms/1.97ms (`restart/corpora/SOTA.md:130-136`).

Trajectory:

| Gate | PASS-2 mechanism | Evidence artefact |
|---|---|---|
| Remove OpenFrame checkpoint cost | TapeBuilder length checkpoints | samply confirms no `Vec<OpenFrame>::clone`; old pathology cited at `restart/corpora/RESTART-SKETCH.md:154-184` |
| JSON structural scan | `SimdScan` BIR plus `simd-scan` kernels | kernel parity and index throughput |
| CSS scanner/layout | `Layout`, `RegexDfa`, `HostCall`, `SimdScan` | bootstrap/animate benchmark rows |
| Pratt expressions | `PrattSpine` auto-detected | operator table snapshot and formula fixtures |
| WASM | wasm32 binding path with scalar/SIMD scan parity | JSON smoke and twitter WASM bench handoff |

PASS-2 should not claim final perf wins until generated parsers run the corpus, but it defines the only mechanisms by which those gates can be met.

Per-construct contribution plan:

| Construct | Expected contribution | Anchor |
|---|---|---|
| `AltDispatch` | Removes speculative checkpoints for byte-disjoint alts. | old sketch says byte-disjoint alts still paid wasted checkpoint cost (`restart/corpora/RESTART-SKETCH.md:201-217`). |
| `AltSpeculative` | Converts rollback from cloned frames to length truncation. | old checkpoint clone dominated samples (`restart/corpora/RESTART-SKETCH.md:154-184`). |
| `SimdScan` | Moves delimiter discovery toward simdjson-style structural scan. | simdjson structural index and On-Demand anchor (`restart/corpora/SOTA.md:73-89`). |
| `RegexDfa` | Keeps Unicode work in regex engine and off grammar scanner. | README regex Unicode decision (`restart/README.md:131-143`). |
| `PrattSpine` | Replaces layered recursive descent for expression families with loop/table dispatch. | current generated Rust already carries Pratt LUT inheritance (`crates/core/src/backend/rust/emitter/grammar.rs:194-202`). |
| `HostCall` | Moves chained host functions into typed generic calls. | README host fn and chaining scope (`restart/README.md:145-166`). |
| `Layout` | Centralizes skip policy and prevents repeated whitespace scanning. | `@layout` in V1 (`restart/README.md:176-178`). |

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
| BC.W0 | Typed IR boundary and variant-table discipline (`docs/tranches/BC/waves/W0.md:10-28`). | Expand final BIR to 23 variants and Tape shapes. | 7-variant placeholder. |
| BC.W1 | Rust emitter consumes typed IR and regen equality (`docs/tranches/BC/waves/W1.md:10-64`). | Rust lowerer consumes BIR only and writes TapeBuilder code. | Stale tape-residue/no-tape language in BC.W1 (`docs/tranches/BC/waves/W1.md:82-88`). |
| BC.W2 | TS/WASM scaffold compile and smoke discipline (`docs/tranches/BC/waves/W2.md:10-58`). | Keep TS scaffold, turn WASM toward wasm32 binding path. | Treat TS production as PASS-2. |
| BC.W3 | Crate split and Lock 13/5/6 attention (`docs/tranches/BC/waves/W3.md:10-87`). | Re-anchor to current README crate names and Tape runtime. | Old `bbnf-` internal crate naming. |
| BD.W0 | TS/NAPI path inheritance and graceful host-fn failure (`docs/tranches/BD/waves/W0.md:10-15`, `docs/tranches/BD/waves/W0.md:95-101`). | Carry as PASS-3 packaging handoff. | PASS-2 production TS claim. |
| BD.W1 | TS shape table and host resolution inheritance (`docs/tranches/BD/waves/W1.md:28-71`). | BIR remains TS-capable. | TS runtime activation in PASS-2. |
| BD.W2 | wasm-bindgen production path and host import table (`docs/tranches/BD/waves/W2.md:38-62`, `docs/tranches/BD/waves/W2.md:165-183`). | Apply to wasm32 binding layer with Tape core. | Raw WAT as production path. |
| BD.W3 | Publication order and semver check discipline (`docs/tranches/BD/waves/W3.md:8-27`, `docs/tranches/BD/waves/W3.md:79-96`). | Route to PASS-3/package release work. | Publishing claims in PASS-2. |
| BD.W4 | Fleet fixture and matrix shape (`docs/tranches/BD/waves/W4.md:8-27`, `docs/tranches/BD/waves/W4.md:187-197`). | Use as downstream parity fixtures. | PASS-2 fixture fleet implementation. |

## §9 Punch List

1. Implement `codegen::backend_ir` with the 23-variant table and snapshot printer.
2. Add PASS-1 to PASS-2 handoff tests once PASS-1 artefacts exist: Grammar IR to BIR, cost plan, host signatures, layout/error annotations.
3. Replace the broad `Emitter` trait with BIR consumer APIs and enforce import-deny checks.
4. Build `runtime/src/tape/` and TapeBuilder checkpoints; delete OpenFrame-style runtime builders during migration.
5. Emit generated grammar modules under `runtime/src/grammars/<name>/` from one template.
6. Split xtask regen and add per-grammar generated LOC budgets.
7. Wire `simd-scan` through BIR `StructuralAlphabet` constants and parity fixtures.
8. Build Rust V1 lowerer first, then WASM binding lowerer; keep TS scaffold only.
9. Add conflict guard checks for `ParseStream`, rewrite-mode walker, grammar-level Unicode sets, and default per-grammar declaration crates.
10. Leave cross-backend 81-cell parity and public package surfaces to PASS-3/BD.

## §10 Closing Posture

PASS-2 closes on a clear middle-layer architecture: Backend IR is the only codegen input; Tape is the only runtime substrate; typed values borrow into Tape; Rust and WASM lowerers consume the same BIR; `simd-scan` stays generic; Pratt/SIMD/PHF are auto-detected; runtime modules are template-emitted from grammar source plus metadata; regen is byte-identical and budgeted.

The main inherited contradictions are resolved, not carried. ParseStream is stale. Rewrite mode is out. Grammar-level Unicode sets are out. Per-grammar declaration crates are not default and are unused for the 9 existing grammars. TS production is deferred. The remaining work is implementation and cross-pass reconciliation with PASS-1 and PASS-3, not another architecture fork.
