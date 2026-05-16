# PASS-2 Agent 6: Codegen Coherence Auditor

## §1 Scope + Framing

Lens: verify the PASS-2 plan is coherent across Backend IR, lowerers, runtime template, SIMD, Pratt, and regen equality. PASS-2 prompt assigns this agent the boundary check, regen equality, generated LOC budget, Lock 14 checks, and convergent-pivot identity (`restart/prompts/PASS-2-CODEGEN.md:37`). The worktree currently has unrelated untracked PASS-3 audit files under `restart/audit/pass-3-runtime/`; this PASS-2 audit writes only `restart/audit/pass-2-codegen/`.

The current codebase confirms PASS-2 must replace wiring, not patch it. `crates/core/src/backend/` contains a broad backend tree, and current source inspection found large files such as `struct_direct.rs` at 1033 LOC, `regex_scan_adapter.rs` at 786 LOC, and `emitter.rs` at 566 LOC. CENSUS independently lists these god-module surfaces and flags backend/runtime module size problems (`restart/corpora/CENSUS.md:321-354`). Runtime has nine manual grammar modules (`crates/core/src/runtime/mod.rs:8-23`), and Lock 14 forbids that shape in generic crates (`restart/locks/LOCKS.md:60`).

## §2 Per-Item Table

| Coherence Item | Pro | Con | Explication | Challenge | Disposition |
|---|---|---|---|---|---|
| Backend IR boundary | Required by Lock 5 (`restart/locks/LOCKS.md:42`). | Current driver walks Grammar IR (`crates/core/src/backend/driver/mod.rs:1-6`). | Lowerers import `backend_ir` only. | Add import-deny check. | REINVENT. |
| Regen equality | Lock 6 requires generated committed source (`restart/locks/LOCKS.md:44`). | Current `xtask/src/regen.rs` is 849 LOC per CENSUS (`restart/corpora/CENSUS.md:321-354`). | Split regen into plan, metadata, emit, write, budget, and check modules. | `xtask regen --check` must return zero source diff. | KEEP-REINVENT. |
| Generated LOC budget | Lock budget starts at 168K across 9 grammars (`restart/locks/LOCKS.md:118-125`). | Current generated tree is 168,750 LOC by PASS-B (`restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`). | PASS-2 budget caps initial Rust generated output at +2% until template reductions land. | Gate per grammar, not aggregate only. | KEEP-MODIFY. |
| Lock 14 genericity | README says two onboarding surfaces and no Rust crate or match arm (`restart/README.md:13-25`). | Current runtime and registry code have grammar names. | Generic crates fail CI on grammar-specific matches and public types. | Generated subdirs are allowed only under runtime grammars. | REINVENT. |
| Tape naming | Lock 1 and README now say Tape (`restart/locks/LOCKS.md:34`, `restart/README.md:285-314`). | Prompt/inheritance still say ParseStream (`restart/prompts/PASS-2-CODEGEN.md:81`, `restart/inheritance/INDEX.md:65-66`). | PASS-2 output must name conflict and resolve to Tape. | No new ParseStream type. | DISCARD stale text. |
| Cross-pass boundary | PASS-1 output unavailable by Phase 1 dispatch. | PASS-2 prompt says consume PASS-1 output (`restart/prompts/PASS-2-CODEGEN.md:8-21`). | Read PASS-1 prompt and README; state assumptions and handoffs. | SYNTHESIS reconciles later. | KEEP-MODIFY. |

## §3 Architectural Commitments Ratified

1. **Commit discipline and scoped writes.** Local instructions require commit-discipline before committing, status inspection, preserving unrelated staged work, and staging only intended slices. This was followed as process input; unrelated PASS-3 files are not PASS-2 scope.

2. **No broad claims without tables.** WAVE_SPEC requires docs-only work to run checks and inspect diff (`docs/precepts/instructions/tranche/WAVE_SPEC.md:112-119`), and lessons reject empty agent returns (`docs/precepts/instructions/LESSONS-LEARNED.md:126-136`). PASS-2 synthesis must include the six agent outputs and per-crate/per-budget tables.

3. **Convergent pivot is staggered closure.** Inheritance says the old convergent pivot sharpens to staggered closures, but also contains stale ParseStream language (`restart/inheritance/INDEX.md:65-66`). PASS-2 coherence is: Lock 5 closes with Backend IR, Lock 1 closes in runtime template plus TapeBuilder, Lock 13 closes through crate trees, and Lock 14 closes through metadata/template-only grammar onboarding.

4. **Generated output must remain greppable and committed.** `xtask/src/regen.rs` already writes output files with content-equality behavior (`xtask/src/regen.rs:400-461`). PASS-2 keeps content-equality but splits the module and adds BIR/runtime-template budgets.

## §4 New Facilities Proposed

Coherence gate set:

```text
cargo check -p codegen
cargo check -p runtime
cargo test -p simd-scan
cargo xtask regen --check
rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template
rg -nE "JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser" crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,bbnf-regex,parse-that,simd-scan,analysis,lsp}/src/
```

The `rg "GrammarIR"` command is allowed to find `BackendIrLowerer` producer code, but not lowerer consumers. The grammar-name command must return zero outside generated outputs, matching Lock 14 verification (`restart/locks/LOCKS.md:60`).

Generated LOC budget:

| Grammar | Current generated LOC | PASS-2 initial max (+2%) | Citation |
|---|---:|---:|---|
| bbnf | 21,503 | 21,933 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| bnf | 3,290 | 3,356 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| csv | 1,693 | 1,727 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| css_l4 | 107,138 | 109,281 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| css_pretty | 9,021 | 9,201 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| ebnf | 7,646 | 7,799 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| google_sheets | 14,088 | 14,370 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| json | 3,500 | 3,570 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| math | 871 | 888 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |
| total | 168,750 | 172,125 | `restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101` |

## §5 Cross-Cuts To PASS-1 / PASS-3

PASS-1: PASS-2 cannot block on PASS-1 artefacts in this parallel phase. The hand-off assumptions are recorded against the PASS-1 prompt (`restart/prompts/PASS-1-SUBSTRATE.md:57-70`). SYNTHESIS must reconcile actual PASS-1 output with this BIR plan.

PASS-3: PASS-2 emits runtime module shape and materialisation cost. PASS-3 owns user API, package layout, selectors, docs, and BD parity gates. BD.W5's 81-cell parity belongs downstream (`docs/tranches/BD/waves/W5.md:181-217`).

## §6 Risk + Mitigation Table

| Risk | Impact | Mitigation |
|---|---|---|
| Another pass stages unrelated files first | Commit mixes tranches | Commit-discipline staging by explicit path only: `git add restart/audit/pass-2-codegen/`. |
| Regen check updates files outside PASS-2 in audit-only run | Scope breach | This audit does not run generating commands that write source; it specifies future gates only. |
| PASS-2 docs inherit stale authority text | SYNTHESIS relitigates settled points | Synthesis has a dedicated conflict ledger for Tape, rewrite mode, Unicode, and declaration crates. |
| Budget uses aggregate only | CSS L4 can hide growth | Gate per grammar and total. |
| Lowerer and runtime template split creates cyclic crates | Workspace check fails | `codegen` emits source, `runtime` contains substrate; runtime never depends on codegen. |

## §7 Inheritance Ledger

| Source | KEEP | REINVENT | DISCARD |
|---|---|---|---|
| Lock 5/6/13/14 | Boundary, generated source, directory cohesion, genericity (`restart/locks/LOCKS.md:42-60`). | Add concrete commands and per-grammar budgets. | Any direct Grammar IR lowerer consumer. |
| CENSUS | God-module evidence and per-grammar runtime inventory (`restart/corpora/CENSUS.md:321-354`, `restart/corpora/CENSUS.md:435-527`). | Convert inventory to greenfield crate tree. | Manual runtime module re-exports. |
| PASS-B | Generated LOC baseline and emitter collapse (`restart-archive-2026-05-04/audit/passes/PASS-B.md:91-101`, `restart-archive-2026-05-04/audit/passes/PASS-B.md:181-186`). | Apply to Tape and no per-grammar crates. | Old direct-only/no-tape wording. |
| BD parity waves | Production parity test shape (`docs/tranches/BD/waves/W5.md:181-217`). | Keep as PASS-3/BD handoff. | Claiming it at PASS-2 close. |

## Wave 2 correction note

This agent's codegen close gate enumeration (agent-6 §1, lines 39-43 cited by HARDENING-PASS-2 punch items 2 and 6) is augmented by PASS-2's expanded non-generated LOC + child-count budget table at PASS-2.md §6, which now binds `runtime/src/*` and `host/src/*` budgets, child-count proofs, and per-area enforcing commands. The verbatim deny command `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template` remains the codegen close gate at PASS-2.md:232; the BIR ownership ratification at PASS-2.md:184 corrects the ownership path. The carry ledger at PASS-2.md §8 carries the eight Receiver/Blocker/Receiving-gate triples per HARDENING-CONSOLIDATED §4.39.
