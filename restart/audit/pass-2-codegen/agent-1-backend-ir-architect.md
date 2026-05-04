# PASS-2 Agent 1: Backend IR Architect

## §1 Scope + Framing

Lens: define the Backend IR as PASS-2's only codegen substrate. The PASS-2 prompt assigns this pass the codegen IR contract, Rust V1, WASM V1, TS deferral, runtime template, SIMD kernels, Pratt/SIMD auto-detection, and regen equality (`restart/prompts/PASS-2-CODEGEN.md:3`). Lock 5 requires an IR plus per-backend lowerers and forbids codegen walking grammar directly (`restart/locks/14-LOCKS.md:42`). The current source violates that boundary: the shared backend driver says it walks `GrammarIR` and delegates to `Emitter` (`crates/core/src/backend/driver/mod.rs:1-6`), while the current `Emitter` trait says emitters are called by a driver walking `GrammarIR` (`crates/core/src/backend/emitter.rs:1-13`).

The authority conflict is material. PASS-2 text still says Lock 1 honours a "ParseStream union" (`restart/prompts/PASS-2-CODEGEN.md:81`), and inheritance says the tape name dies (`restart/inheritance/INDEX.md:65-66`). The settled authority for this pass is the opposite: Tape is the substrate, with typed values borrowing into `&'i Tape<'i>` plus an index, and no parallel substrate (`restart/locks/14-LOCKS.md:34`). README also states the tape lives at `runtime/src/tape/` and is unioned with direct-to-struct typed values (`restart/README.md:285-314`). This report resolves all "ParseStream" residues to Tape and records the conflict.

PASS-1 output is unavailable by dispatch design. The consumed PASS-1 contract is the prompt, not PASS-1 artefacts: PASS-1 owns Grammar IR, type system, CSP/e-graph, cost model, extensions, and coherence (`restart/prompts/PASS-1-SUBSTRATE.md:27-32`). Its prompt names the hand-off shape: Grammar IR variants, Backend IR variants, Backend IR consumer interface, cost-model trait, and e-graph plugin registry (`restart/prompts/PASS-1-SUBSTRATE.md:57-70`). This Backend IR report assumes those products land and defines the PASS-2 consumer side.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Disposition |
|---|---|---|---|---|---|
| Backend IR as the only codegen input | Lock 5 mandates IR plus lowerers, and PASS-2 owns the codegen IR contract (`restart/locks/14-LOCKS.md:42`, `restart/prompts/PASS-2-CODEGEN.md:3`). | Current source uses a driver that walks `GrammarIR` (`crates/core/src/backend/driver/mod.rs:1-6`). | `codegen::backend_ir::Module` must be the type consumed by Rust, WASM, and TS-stub lowerers. | A same-wave consumer gate must compile the Rust lowerer only from Backend IR. | KEEP-REINVENT: keep boundary, replace source wiring. |
| 22-variant BC table | The BC table already enumerates rule, seq, alt, repeat, scanner, host, Pratt, SIMD, error, regex, and enum shapes (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:28-290`). | It lacks lookbehind as a grammar-level surface and carries stale host namespace assumptions (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:160-170`). | Use the table as the floor, not the final set; PASS-2 needs 23 variants. | Reject variant inflation beyond the one missing grammar surface. | KEEP-REINVENT. |
| Tape materialisation contract | Lock 1 and README make Tape authoritative (`restart/locks/14-LOCKS.md:34`, `restart/README.md:285-314`). | Prior corpses argue against tape based on old failures (`restart/corpora/CENSUS.md:20-36`). | The BIR must name `TapeShape` and `ValueShape` as one materialisation plan, not two substrates. | Every lowerer checkpoint must save cursor plus tape/payload lengths, not heap-stack clones. | KEEP-REINVENT. |
| `UnicodeSet` BIR node | Regex Unicode belongs in `parse-that/regex`, not grammar-level BBNF (`restart/README.md:131-143`). | PASS-2 prompt names "Unicode-set" in Agent 2's lens (`restart/prompts/PASS-2-CODEGEN.md:33`). | Do not add a grammar-level BIR node; `RegexDfa` carries Unicode metadata after regex compilation. | Cite the prompt conflict in synthesis. | DISCARD. |
| `RewriteModeWalker` BIR node | README rejects rewrite mode and says Visitor covers it (`restart/README.md:123-129`). | PASS-2 prompt names "Rewrite-mode-walker" (`restart/prompts/PASS-2-CODEGEN.md:33`). | No rewrite node; runtime Visitor and PASS-3 API carry transformation. | Generated code must not reserve APIs for rewrite mode. | DISCARD. |
| Host function model | README says host functions decompose through generic primitives, workspace metadata, or `@host fn` (`restart/README.md:13-25`, `restart/README.md:145-157`). | BC variant table names a per-grammar host namespace (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:160-170`). | `HostCall` contains a non-empty chain of generic host steps plus typed arguments; no default declaration crate. | Rare escape hatch must be documented outside the 9 extant grammars. | REINVENT. |
| TS backend | PASS-2 owns TS deferral per Q28 (`restart/prompts/PASS-2-CODEGEN.md:3`). | BD waves plan production TS (`docs/tranches/BD/waves/W1.md:10-24`). | PASS-2 BIR remains TS-capable, but emitted TS is scaffold-only. Production TS is PASS-3/BD material. | Parity matrix must not be claimed in PASS-2. | KEEP-MODIFY. |

## §3 Architectural Commitments Ratified

1. **Backend IR module shape.** `BackendModule` contains grammar metadata, arena/tape layout descriptors, rule table, scanner tables, Pratt tables, host table, error table, and generated-LOC budgets. This aligns with README's two IRs, where Backend IR is the Rust/TS/WASM codegen contract (`restart/README.md:104-117`).

2. **Variant set.** PASS-2 ratifies 23 Backend IR variants:

| # | Variant | Materialisation | Backend obligation |
|---|---|---|---|
| 1 | `Rule` | declares result `ValueShape` and `TapeKind` | emits parse fn, view accessor, error boundary |
| 2 | `Seq` | child tape span plus struct field map | emits ordered child calls |
| 3 | `AltDispatch` | enum discriminator | emits byte/keyword dispatch |
| 4 | `AltSpeculative` | checkpointed alternative | emits cursor + tape length checkpoint |
| 5 | `Repeat` | child list payload | emits loop with progress guard |
| 6 | `Optional` | optional child index | emits zero-or-one parser |
| 7 | `Ref` | references rule output | emits direct rule call |
| 8 | `Lit` | leaf node | emits byte literal compare |
| 9 | `Keyword` | leaf node or enum arm | emits PHF or branch table |
| 10 | `CharClass` | leaf span | emits byte class predicate |
| 11 | `Scanner` | structural scan | emits scanner callback |
| 12 | `RegexDfa` | leaf span and payload | emits regex engine call; Unicode lives here |
| 13 | `Span` | source span projection | emits span constructor |
| 14 | `Layout` | layout skip policy | emits layout-consumption edge |
| 15 | `MapExpr` | typed projection | emits field/value conversion |
| 16 | `HostCall` | typed payload or validation edge | emits generic host chain |
| 17 | `FoldResult` | accumulator payload | emits fold state |
| 18 | `EnumDiscriminator` | enum tag | emits closed enum representation |
| 19 | `PrattSpine` | expression tree | emits Pratt loop |
| 20 | `SimdScan` | structural-index side table | emits `simd-scan` call |
| 21 | `Lookbehind` | zero-width predicate | emits bounded reverse predicate |
| 22 | `ErrorRecovery` | recovery marker and diagnostic | emits recovery edge |
| 23 | `DebugMarker` | no runtime materialisation | emits optional source map metadata |

The BC table's 22 variants remain recognizable (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:28-290`); PASS-2 adds `Lookbehind`, folds multi-function chaining into `HostCall`, and keeps Unicode in `RegexDfa`. The table avoids grammar names, consistent with the BC invariant that codegen cannot depend on grammar names (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:331-339`).

Cardinality defence:

| Comparator | Local prior-art reading | PASS-2 conclusion |
|---|---|---|
| MLIR `arith` | 60 ops, but the local research anchor says this is the wrong scale for grammar-form IR (`docs/tranches/BC/audit/research-anchors.md:12-18`). | 23 BIR variants remain far below dialect-op scale. |
| Cranelift `InstructionData` | 40 variants, with memory/control/function-call families not all relevant to grammar lowering (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:323-325`). | BIR collapses memory/control concerns into alt, scan, and tape shapes. |
| rustc HIR `ExprKind` | 35 variants and closest expression-form analogue (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:323-329`). | 23 is smaller because BBNF has fewer expression forms, but adds scanner and recovery anchors. |
| rustc HIR `ItemKind` | 16 module-level variants (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:323-327`). | BIR is rule-body codegen IR, not module item IR, so 16 is too low. |
| chalk `TyKind` | 23 type-form variants (`docs/tranches/BC/audit/research-anchors.md:16-18`). | 23 BIR variants are defensible because BIR is grammar-form plus typed projection. |
| swc `Expr`/`Stmt` | Local README uses swc as transformer/codegen separation evidence (`restart/README.md:369`); current rustdoc shows `Stmt` has 19 variants and `Expr` is a larger JavaScript expression sum (`https://rustdoc.swc.rs/swc_ecma_ast/enum.Stmt.html`, `https://rustdoc.swc.rs/swc_ecma_ast/enum.Expr.html`). | swc supports the separation lesson, while BIR cardinality stays in the 20-30 compiler-IR band already ratified by BC. |

3. **Producer/consumer split.** PASS-1 lowers Grammar IR into Backend IR after type inference, shape mining, e-graph, and cost extraction; README names that pass order (`restart/README.md:188-217`). PASS-2 consumes only the extracted Backend IR and must reject direct `GrammarIR` reads in lowerer modules.

4. **Tape/direct-to-struct union.** A rule result includes both `TapeKind` and `ValueShape`. Direct typed records borrow into tape instead of bypassing it. This resolves the old direct-projection-only inheritance against current Tape authority (`restart/README.md:285-314`, `restart/corpora/RESTART-SKETCH.md:367-379`).

## §4 New Facilities Proposed

| Facility | Explication | Gate |
|---|---|---|
| `codegen::backend_ir` | Typed module, node arena, variant payloads, shape table, cost annotations, source spans. | `cargo check -p codegen` with no lowerer reading Grammar IR. |
| `BackendConsumer` trait | Narrow consumer API: `emit_module`, `emit_types`, `emit_rule`, `emit_node`, `emit_scanner_tables`, `emit_host_table`, `emit_finish`. | Replace the current 500+ LOC trait surface (`crates/core/src/backend/emitter.rs:31-566`). |
| BIR snapshot tests | Pretty-printed BIR for each of 9 grammars, plus byte-identical regen check. | `cargo xtask regen --check` produces no source diff, matching Lock 6 (`restart/locks/14-LOCKS.md:44`). |
| Host chain descriptor | `HostCall { steps: NonEmpty<HostStep>, generics, error_policy }`. | `rg` proves no per-grammar host match arms in generic crates, as Lock 14 demands (`restart/locks/14-LOCKS.md:60`). |
| Tape shape descriptor | `TapeShape { kind, span_policy, payload_layout, sib_skip }`. | Runtime template emits accessors borrowing `&'i Tape<'i>` plus node index (`restart/locks/14-LOCKS.md:34`). |

## §5 Cross-Cuts To PASS-1 / PASS-3

PASS-1 hand-off: Backend IR requires stable Grammar IR node ids, type layouts, cost scores, shape-mining decisions, Pratt eligibility, SIMD alphabet extraction, lookbehind width analysis, and host signature inference. The PASS-1 prompt assigns those upstream responsibilities (`restart/prompts/PASS-1-SUBSTRATE.md:57-70`).

PASS-3 hand-off: Backend IR does not define the user-facing API. It hands PASS-3 a runtime module shape and materialisation cost table. PASS-3 owns API and ecosystem while PASS-2 owns runtime template and lowerers (`restart/prompts/PASS-2-CODEGEN.md:3`, `restart/prompts/PASS-2-CODEGEN.md:54`).

Cross-pass risk: prompt text from inheritance still states ParseStream and rewrite-mode (`restart/inheritance/INDEX.md:34`, `restart/inheritance/INDEX.md:65-66`). PASS-2 synthesis must name these conflicts so SYNTHESIS does not inherit stale language.

## §6 Risk + Mitigation Table

| Risk | Impact | Mitigation |
|---|---|---|
| BIR grows into a second Grammar IR | Lowerers regain grammar knowledge | Keep BIR variants codegen-shaped, and keep type/e-graph details as annotations from PASS-1. |
| Tape/direct-to-struct becomes two paths again | Recreates Lock 1 failure | Encode `TapeShape` and `ValueShape` on the same rule result and make checkpoints truncate tape and payload arenas together. |
| Host chains hide per-grammar logic | Violates Lock 14 | Resolver accepts only generic primitives, metadata-declared composition, or `@host fn`; any declaration-crate escape is outside the 9 grammar baseline (`restart/README.md:13-25`). |
| Unicode set sneaks into grammar BIR | Conflicts with README | Gate BIR variants against the 23-row table; Unicode appears only inside compiled regex payloads. |
| TS production claims enter PASS-2 | Cross-tranche drift | TS lowerer is scaffold-only until PASS-3/BD consumes it; BD.W1 production text is inheritance, not PASS-2 scope (`docs/tranches/BD/waves/W1.md:10-24`). |

## §7 Inheritance Ledger

| Source | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| BC typed IR table | Cardinality discipline, per-variant lowering, scanner/Pratt/SIMD entries (`docs/tranches/BC/audit/W0-typed-ir-variant-table.md:28-290`). | Add `Lookbehind`, host chains, tape shapes. | Grammar-level Unicode and stale per-grammar host namespace. |
| PASS-B | Emitter trait collapse to 8-10 methods and runtime-template pivot (`restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`). | Apply to Tape/direct-to-struct, not direct-only. | Old no-tape posture. |
| Amendment 01 | Zero per-grammar crates for the 9 grammars (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-24`). | Re-anchor crate names to current README's no-prefix internal workspace. | Mandatory per-grammar declaration crates from MASTER-PLAN (`restart-archive-2026-05-04/audit/master-plan/MASTER-PLAN.md:79-89`). |
| SOTA | simdjson tape insight and sonic-rs direct structs are both useful (`restart/corpora/SOTA.md:73-89`, `restart/corpora/SOTA.md:30-44`). | Union the ideas through Tape-backed direct typed values. | Old corpus anti-tape verdict (`restart/corpora/SOTA.md:202-214`). |
