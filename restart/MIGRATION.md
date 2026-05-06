# Restart Migration

This document is the Phase 2 per-file disposition contract for the greenfield
restart. It does not patch the current workspace in place. It tells tranche
authors which current modules are kept, moved, replaced, deleted, or archived,
and it names the gates that prove the migration did not carry forward stale
architecture.

## 0. Scope And Authority

The migration follows the resolved Phase 2 authority:

| Source | Migration consequence |
|---|---|
| README says onboarding is grammar source plus workspace metadata, without Rust crate or per-grammar match arms (`restart/README.md:11-25`). | Current grammar-name registries and runtime shims are not preserved as handwritten code. |
| README fixes the 24-crate workspace and crate naming (`restart/README.md:29-60`). | Existing `core`, `analysis`, `lsp`, `bbnf-path`, `bbnf-path-ts`, `bootstrap`, `ser`, and `gorgeous` do not carry over as-is. |
| Lock 1 says tape is the substrate unioned with direct-to-struct (`restart/locks/14-LOCKS.md:34`). | Old anti-tape notes and ParseStream language are migration conflicts, not goals. |
| Lock 5 says lowerers consume Backend IR, not grammar source (`restart/locks/14-LOCKS.md:42`). | Current backend walkers are mined for behavior and replaced at the architecture boundary. |
| Lock 13 sets tree and LOC discipline (`restart/locks/14-LOCKS.md:58`). | Current god modules are split or replaced. |
| Lock 14 forbids grammar switches/types/modules/features in generic crates (`restart/locks/14-LOCKS.md:60`). | Current hardcoded grammar tables become deletion gates. |
| PASS-2 says codegen/runtime wiring must be replaced, not patched (`restart/audit/pass-2-codegen/PASS-2.md:5-8`). | Migration is a greenfield rebuild with mined implementation knowledge. |

The current repository contains 13 Rust crate directories under `crates/`.
The older module corpus counted 824 Rust files and 21/23 oversized handwritten
files after generated exemptions (`restart/corpora/MODULES.md:1295-1303`).
This synthesis inventory counted 834 current Rust files with `find crates -name
'*.rs' -type f`, so tranche gates must use the live count, not only the prior
corpus.

## 1. Disposition Alphabet

| Fate | Meaning |
|---|---|
| KEEP-OUTRIGHT | Carry the file/module with only path/name updates and normal formatting. |
| KEEP-MODIFY | Preserve the implementation idea, but adapt API boundaries, names, tests, or ownership. |
| ABROGATE-MOVE | Move the code to a new crate/module and delete the old path. |
| ABROGATE-REPLACE | Mine behavior and tests, then implement the new architecture instead of carrying the code body. |
| ABROGATE-DELETE | Delete from production without replacement. Archive only if useful for reference. |
| GENERATED-REPLACE | Replace generated source with new template output and equality/budget gates. |
| ARCHIVE | Move out of production workspace before tranche A.W0. |

## 2. Aggregate Disposition

The current 834-file inventory is disposed as follows. These are migration
planning counts, not a promise that line counts survive exactly.

| Fate | Files | Main owners | Net effect |
|---|---:|---|---|
| KEEP-OUTRIGHT | 121 | `simd-scan`, generic pieces of `csp-solver`, generic pieces of `egraph`, fixtures/tests that remain useful. | Keeps proven generic code. |
| KEEP-MODIFY | 224 | `ir` concepts, analysis diagnostics, path parser pieces, CSP/egraph integrations, selected runtime helpers. | Updates ownership and contracts. |
| ABROGATE-MOVE | 96 | Source/span/import modules, VM utilities, LSP document logic, CLI/debug helpers. | Moves to new crate tree. |
| ABROGATE-REPLACE | 315 | `core` backend walkers, old lowering, current runtime strategy, path registries, grammar-specific shims. | Rebuilds around Backend IR and tape/direct. |
| ABROGATE-DELETE | 78 | Stale serialize paths, hardcoded registries, old fallback/legacy paths, dead adapters. | Removes old architecture. |
| GENERATED-REPLACE | Included above | Generated parsers and per-grammar runtime files. | New template output under `runtime/src/grammars/<name>`. |
| ARCHIVE | Included above | `ser`, `gorgeous`, legacy tranche docs. | Kept for reference, not production. |
| Total | 834 | Workspace-wide | Matches current synthesis file inventory. |

The important migration fact is not the exact file count. It is the direction:
generic solver/scanner/egraph pieces survive; grammar-name runtime and backend
plumbing are replaced; old archive crates leave the production workspace.

## 3. Current Crates To Restart Crates

| Current crate | Restart fate | Restart destination |
|---|---|---|
| `crates/core` | ABROGATE-REPLACE plus selective ABROGATE-MOVE. | Split across `grammar`, `source`, `pipeline`, `passes`, `codegen`, `runtime`, `host`, `bbnf`, `bbnf-cli`, `bbnf-bench`. |
| `crates/ir` | KEEP-MODIFY plus ABROGATE-MOVE. | `ir`, `passes`, `vm`, `cost-model`, and bridge modules. |
| `crates/analysis` | ABROGATE-MOVE/REPLACE. | `bbnf-language-server`, `error`, `source`, `grammar`, `pipeline`. |
| `crates/lsp` | ABROGATE-MOVE/REPLACE. | `bbnf-language-server`. |
| `crates/bbnf-path` | KEEP-MODIFY/REPLACE. | `path` plus `path-core`. |
| `crates/bbnf-path-ts` | ABROGATE-MOVE deferred to V2. | TS surface defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5; legacy `crates/bbnf-path-ts` archives at A.W0 alongside `ser`/`gorgeous` and is reconstituted as `path-ts` in V2. The `path-core` extraction (Rust-line) lands in V1 from `crates/bbnf-path`. |
| `crates/csp-solver` | KEEP-MODIFY. | `csp-solver`; generic API remains. |
| `crates/egraph` | KEEP-MODIFY. | `egraph`; bridge logic moves to `passes`. |
| `crates/egraph-derive` | KEEP-MODIFY. | `egraph-derive`. |
| `crates/simd-scan` | KEEP-OUTRIGHT/KEEP-MODIFY. | `simd-scan`. |
| `crates/bootstrap` | ABROGATE-REPLACE. | `bbnf-cli`, `pipeline`, bootstrap fixtures. |
| `crates/ser` | ARCHIVE. | `restart-archive`/legacy reference only. |
| `crates/gorgeous` | ARCHIVE. | `restart-archive`/legacy reference only. |

The corpus already classifies `ser` and `gorgeous` as archive-only
(`restart/corpora/MODULES.md:165-212`), and Lock 12 requires that archive before
implementation starts (`restart/locks/14-LOCKS.md:56`).

### 3.1 Current Inventory By Crate

This synthesis counted the current crate tree before writing the migration
document. The prior corpus count remains cited evidence; the live count is the
working target for tranche A.

| Current crate | Rust files | Current LOC | Primary fate |
|---|---:|---:|---|
| `crates/analysis` | 46 | 5,241 | Consolidate into `bbnf-language-server`, `error`, `source`. |
| `crates/bbnf-path` | 3 | 918 | Split into `path` and `path-core`. |
| `crates/bbnf-path-ts` | 6 | 1,280 | Split into `path-ts` and `path-core`. |
| `crates/bootstrap` | 4 | 465 | Replace with CLI/pipeline bootstrap commands. |
| `crates/core` | 432 | 248,077 | Split, replace, and regenerate. |
| `crates/csp-solver` | 50 | 9,686 | Keep generic core, split oversized files. |
| `crates/egraph` | 18 | 2,762 | Keep generic core, move BBNF bridge. |
| `crates/egraph-derive` | 1 | 343 | Keep with egraph. |
| `crates/gorgeous` | 17 | 1,441 | Archive. |
| `crates/ir` | 224 | 51,957 | Mine and reorganize into IR/passes/vm/cost. |
| `crates/lsp` | 13 | 4,123 | Consolidate into `bbnf-language-server`. |
| `crates/ser` | 5 | 530 | Archive. |
| `crates/simd-scan` | 15 | 3,389 | Keep and wire to BIR. |
| Total | 834 | 330,212 | Live file count target; LOC includes generated code. |

The exact current LOC total is not a planning invariant because generated files
dominate `core`. The migration invariant is fate by directory and gate, not
preserving current LOC.

#### 3.1.1 Mixed-Fate Crosswalk

Every current crate that distributes files across more than one fate bucket
appears below. The crosswalk audits the 834-file disposition by family rather
than by individual file; the per-family row counts must match the live tree
when tranche A starts.

| Current crate | Mixed | Family bucket | File count (current) | New location | Owner tranche |
|---|---|---|---:|---|---|
| `crates/analysis` | yes | Diagnostics + report helpers. | ~14. | `error/`, `bbnf-language-server/diagnostics/`. | A/I. |
| `crates/analysis` | yes | Semantic index. | ~18. | `bbnf-language-server/semantic/`. | I. |
| `crates/analysis` | yes | Document snapshot/edit helpers. | ~10. | `source/snapshot/`, `bbnf-language-server/document/`. | A/I. |
| `crates/analysis` | yes | Grammar-specific assumptions. | ~4. | none (ABROGATE-DELETE). | A. |
| `crates/bbnf-path` | yes | Macro entrypoint. | ~1. | `path/src/macro_impl/`. | G. |
| `crates/bbnf-path` | yes | Parser/evaluator logic. | ~2. | `path-core/src/parse/`, `path-core/src/eval/`. | G. |
| `crates/bbnf-path-ts` | yes | TS emitter/schema. | ~3. | `path-ts/src/schema/`, `path-ts/src/emit/`. | G. |
| `crates/bbnf-path-ts` | yes | Hardcoded grammar registries. | ~1. | none (ABROGATE-DELETE). | A/G. |
| `crates/bbnf-path-ts` | yes | Fixture duplicates. | ~2. | `test-fixtures/`. | A. |
| `crates/bootstrap` | yes | IR dump/debug commands. | ~2. | `bbnf-cli/debug/`, `vm/debug/`. | E. |
| `crates/bootstrap` | yes | Bootstrap parse command. | ~1. | `grammar/bootstrap/`, `pipeline/`. | A. |
| `crates/bootstrap` | yes | Standalone crate shell. | ~1. | none (ABROGATE-DELETE). | A. |
| `crates/core` | yes | Generated grammars. | ~9 (one per seed grammar). | `runtime/src/grammars/<name>/generated.rs`. | F. |
| `crates/core` | yes | Generated registry JSON. | 1. | none (ABROGATE-DELETE). | A. |
| `crates/core` | yes | Grammar AST/parser helpers. | ~80. | `grammar/src/*`. | A/D. |
| `crates/core` | yes | Imports, source maps, spans. | ~40. | `source/src/*`. | A. |
| `crates/core` | yes | Lower / normalization. | ~30. | `passes/`, `ir/`, `codegen/`. | C/E/F. |
| `crates/core` | yes | Backend walkers (`backend/**`). | ~80. | `codegen/src/*`. | E/F. |
| `crates/core` | yes | Runtime support. | ~30. | `runtime/src/document/`, `runtime/src/support/`. | B. |
| `crates/core` | yes | Per-grammar runtime modules. | ~120. | `runtime/src/grammars/<name>/**` (GENERATED-REPLACE). | F. |
| `crates/core` | yes | Path executor. | ~5. | `path-core/`, `runtime/`. | G. |
| `crates/core` | yes | CSS types and host shims. | ~10. | `host/`, metadata, generated `host.rs`. | D/F. |
| `crates/core` | yes | Generate/serialize. | ~5. | none (ABROGATE-DELETE; `ser` archive). | A. |
| `crates/core` | yes | Old tests bound to grammar names. | ~25. | `test-fixtures/` plus owner crates. | A/G. |
| `crates/ir` | yes | IR IDs and types. | ~30. | `ir/src/grammar_ir/`, `ir/src/backend_ir/`. | C/E. |
| `crates/ir` | yes | Strategy registry. | ~3. | none (ABROGATE-DELETE). | A. |
| `crates/ir` | yes | Type / shape / recognizer / cost facts. | ~80. | `passes/src/*`, `ir/src/side_tables/`. | C. |
| `crates/ir` | yes | VM and debug. | ~40. | `vm/`. | E. |
| `crates/ir` | yes | Egraph/CSP bridge. | ~25. | `passes/src/bridge/`. | C. |
| `crates/ir` | yes | Other (shared utilities). | ~46. | `ir/src/util/` plus `passes/`. | C. |
| `crates/csp-solver` | partly | Generic core retained. | ~40. | `csp-solver/`. | A. |
| `crates/csp-solver` | partly | BBNF-specific adapters. | ~5. | `passes/src/bridge/`. | C. |
| `crates/csp-solver` | partly | Oversized modules. | ~5. | `csp-solver/` split. | A. |
| `crates/egraph` and `crates/egraph-derive` | partly | Generic core retained. | ~17. | `egraph/`, `egraph-derive/`. | A. |
| `crates/egraph` and `crates/egraph-derive` | partly | BBNF terms/adapters. | ~2. | `passes/src/bridge/`. | C. |
| `crates/lsp` | yes | LSP protocol server. | ~6. | `bbnf-language-server/protocol/`. | I. |
| `crates/lsp` | yes | Diagnostics bridge. | ~4. | `bbnf-language-server/diagnostics/`. | I. |
| `crates/lsp` | yes | Incremental parser glue. | ~3. | `bbnf-language-server/document/`, `pipeline/`. | I. |
| `crates/simd-scan` | partly | Generic core retained. | ~13. | `simd-scan/`. | A/H. |
| `crates/simd-scan` | partly | BBNF-specific recognizer wiring. | ~2. | `passes/src/recognizers/`. | C/H. |
| `crates/gorgeous` | no | Whole crate ARCHIVE. | 17. | `restart-archive`/legacy reference only. | A. |
| `crates/ser` | no | Whole crate ARCHIVE. | 5. | `restart-archive`/legacy reference only. | A. |

The per-family row counts are approximate and refine to exact per-file numbers
during tranche A.W2 when the migration manifest crystallises. Aggregate row
counts must reconcile to the 834-file total before A.W2 closes.

### 3.2 Per-Crate Disposition Tables

The following rows are abbreviated by uniform directory where that is the
truthful unit. Tranche implementation must refine any row that mixes fates
before editing files.

#### `crates/analysis`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Diagnostics and report helpers | `error`, `bbnf-language-server/diagnostics` | ABROGATE-MOVE/KEEP-MODIFY | Same diagnostic codes must serve CLI and LSP. | PASS-3 recovery/LSP contract (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| Semantic index logic | `bbnf-language-server/semantic` | KEEP-MODIFY | Useful editor behavior, new ownership. | MODULES old analysis keep note (`restart/corpora/MODULES.md:509-565`). |
| Document snapshot/edit helpers | `source/snapshot`, `bbnf-language-server/document` | ABROGATE-MOVE | Shared with incremental parser. | README incremental rule (`restart/README.md:344-348`). |
| Grammar-specific assumptions | none | ABROGATE-DELETE | Violates Lock 14 if present. | CENSUS grammar leaks (`restart/corpora/CENSUS.md:103-122`). |

#### `crates/lsp`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| LSP protocol server | `bbnf-language-server/protocol` | KEEP-MODIFY | Protocol work remains useful. | PASS-3 ecosystem tree (`restart/audit/pass-3-runtime/PASS-3.md:160-289`). |
| Diagnostics bridge | `bbnf-language-server/diagnostics` | KEEP-MODIFY | Reuse behavior over new diagnostic types. | PASS-3 diagnostics handoff (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| Incremental parser glue | `bbnf-language-server/document` and `pipeline` | ABROGATE-REPLACE | Must use `DocumentSnapshot` and `ReparsePlan`. | PASS-3 commitments (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |

#### `crates/bbnf-path`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Macro entrypoint | `path/src/macro_impl` | KEEP-MODIFY | Public Rust macro surface remains. | README path API (`restart/README.md:272-318`). |
| Parser/evaluator logic | `path-core/src/parse`, `path-core/src/eval` | ABROGATE-MOVE/KEEP-MODIFY | Shared by Rust and TS. | Lock 7 path split (`restart/locks/14-LOCKS.md:46`). |
| Proc-macro `syn::ParseStream` use | `path/src/macro_impl` | KEEP-MODIFY | This is not runtime ParseStream. | PASS-3 stale runtime term resolution (`restart/audit/pass-3-runtime/PASS-3.md:14-23`). |

#### `crates/bbnf-path-ts`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| TS emitter/schema | `path-ts/src/schema`, `path-ts/src/emit` | KEEP-MODIFY | Keep TS surface, consume shared semantics. | Lock 7 (`restart/locks/14-LOCKS.md:46`). |
| Hardcoded grammar registries | none | ABROGATE-DELETE | Generic path package cannot name grammars. | CENSUS path leaks (`restart/corpora/CENSUS.md:103-122`). |
| Fixture duplicates | `test-fixtures` | ABROGATE-MOVE | Shared parity fixture ownership. | BD inheritance via index (`restart/inheritance/INDEX.md:29-40`). |

#### `crates/bootstrap`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| IR dump/debug commands | `bbnf-cli/debug`, `vm/debug` | ABROGATE-MOVE/REPLACE | Debug survives over new IRs. | README VM debug/replay (`restart/README.md:344-348`). |
| Bootstrap parse command | `grammar/bootstrap`, `pipeline` | KEEP-MODIFY | Bootstrap remains needed, not as crate. | MODULES bootstrap slim (`restart/corpora/MODULES.md:216-228`). |
| Standalone crate shell | none | ABROGATE-DELETE | Not in final 24-crate workspace. | README crate table (`restart/README.md:29-60`). |

#### `crates/core`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| `grammar/generated/*.rs` | `runtime/src/grammars/<name>/generated.rs` | GENERATED-REPLACE | New template output from BIR. | PASS-2 runtime template (`restart/audit/pass-2-codegen/PASS-2.md` §7). |
| `grammar/generated/.registry.json` | none | ABROGATE-DELETE | Metadata is source of truth. | README two-surface onboarding (`restart/README.md:11-25`). |
| `grammar` AST/parser helpers | `grammar/src/*` | ABROGATE-MOVE/KEEP-MODIFY | Grammar crate owns BBNF parsing. | PASS-1 crate tree (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `imports`, source maps, spans | `source/src/*` | ABROGATE-MOVE | Shared source substrate. | README pipeline (`restart/README.md:188-207`). |
| `lower` and normalization | `passes`, `ir`, `codegen` | ABROGATE-REPLACE | Split semantic passes from backend lowering. | Lock 5 (`restart/locks/14-LOCKS.md:42`). |
| `backend/**` | `codegen/src/*` | ABROGATE-REPLACE | BIR-only lowerers replace grammar walkers. | PASS-2 (`restart/audit/pass-2-codegen/PASS-2.md:5-8`). |
| `runtime/mod.rs` generic support | `runtime/src/document`, `runtime/src/support` | KEEP-MODIFY | Useful support under tape/direct contract. | PASS-3 runtime (`restart/audit/pass-3-runtime/PASS-3.md:96-135`). |
| `runtime/<grammar>/**` | `runtime/src/grammars/<name>/**` | GENERATED-REPLACE | Template-emitted per grammar modules. | PASS-2 template schema (`restart/audit/pass-2-codegen/PASS-2.md` §7). |
| `path` executor | `path-core`, `runtime` | ABROGATE-REPLACE | Shared path semantics and runtime view integration. | README path API (`restart/README.md:272-318`). |
| `css_types.rs` and host shims | `host`, metadata, generated `host.rs` | ABROGATE-REPLACE | Host functions are generic/fenced. | Lock 14 (`restart/locks/14-LOCKS.md:60`). |
| `generate/serialize` | none | ABROGATE-DELETE | `ser` is archive-only. | MODULES ser archive (`restart/corpora/MODULES.md:165-184`). |
| Old tests bound to grammar names | `test-fixtures` plus owner crates | KEEP-MODIFY/REPLACE | Preserve fixtures, replace assumptions. | CENSUS duplicate runtime cohort (`restart/corpora/CENSUS.md:435-527`). |

#### `crates/ir`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| IR IDs/types | `ir/src/grammar_ir`, `ir/src/backend_ir` | KEEP-MODIFY/REPLACE | Two IRs are final architecture. | README two IRs (`restart/README.md:104-118`). |
| Strategy registry | none | ABROGATE-DELETE | Hardcoded grammar strategy violates Lock 14. | CENSUS leaks (`restart/corpora/CENSUS.md:103-122`). |
| Type facts | `passes/src/layout` (subroutine), `ir/src/side_tables` (`LayoutFacts`) | ABROGATE-MOVE/KEEP-MODIFY | HM + bidirectional + CSP run inside layout lowering per Lock 2; `TypeFacts` is internal scratch, `LayoutFacts` is the public side-table. | README type system (`restart/README.md:258-268`); Lock 2 (`restart/locks/14-LOCKS.md:36`). |
| Shape facts | `passes/src/shapes`, `ir/src/side_tables` | ABROGATE-MOVE/KEEP-MODIFY | Direct/value/path consumers. | PASS-1 side-table contract (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| Recognizer facts | `passes/src/recognizers` | ABROGATE-MOVE/KEEP-MODIFY | Pratt/SIMD auto-detection. | Lock 10 (`restart/locks/14-LOCKS.md:52`). |
| VM/debug | `vm` | ABROGATE-MOVE/REPLACE | VM replays BIR, not old IR. | PASS-1 VM scope (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| Egraph/CSP bridge | `passes/src/bridge` | ABROGATE-MOVE/KEEP-MODIFY | Bridge, not fused hypergraph. | Lock 4 (`restart/locks/14-LOCKS.md:40`). |

#### `crates/csp-solver`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Generic domains/constraints | `csp-solver` | KEEP-OUTRIGHT/KEEP-MODIFY | Generic solver survives. | MODULES csp-solver (`restart/corpora/MODULES.md:73-132`). |
| BBNF-specific adapters | `passes/src/bridge` | ABROGATE-MOVE | Keep solver generic. | Lock 4/11 (`restart/locks/14-LOCKS.md:40`, `restart/locks/14-LOCKS.md:52-56`). |
| Oversized modules/tests | `csp-solver` split modules | KEEP-MODIFY | Lock 13 file size. | Lock 13 (`restart/locks/14-LOCKS.md:58`). |

#### `crates/egraph` And `crates/egraph-derive`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Generic egraph core | `egraph` | KEEP-MODIFY | Useful sister crate. | MODULES egraph (`restart/corpora/MODULES.md:136-162`). |
| Derive macro | `egraph-derive` | KEEP-MODIFY | Keep with egraph. | MODULES egraph derive (`restart/corpora/MODULES.md:136-162`). |
| BBNF terms/adapters | `passes/src/bridge` | ABROGATE-MOVE | Generic crate stays grammar-neutral. | Lock 14 (`restart/locks/14-LOCKS.md:60`). |

#### `crates/simd-scan`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Scalar scanner | `simd-scan/scalar` | KEEP-OUTRIGHT | Reference implementation. | MODULES simd-scan (`restart/corpora/MODULES.md:47-69`). |
| NEON/AVX scanner files | `simd-scan/neon`, `simd-scan/avx2`, `simd-scan/avx512` | KEEP-MODIFY | Wire to `SimdScan` BIR. | PASS-2 SIMD matrix (`restart/audit/pass-2-codegen/PASS-2.md` §3). |
| Dispatch API | `simd-scan/dispatch` | KEEP-MODIFY | Runtime/codegen consumer boundary. | PASS-2 detector commitments (`restart/audit/pass-2-codegen/PASS-2.md` §3). |

#### `crates/ser` And `crates/gorgeous`

| File or family | New location | Bucket | Rationale | Source finding |
|---|---|---|---|---|
| Entire `ser` crate | archive | ARCHIVE | No production caller. | MODULES ser (`restart/corpora/MODULES.md:165-184`). |
| Entire `gorgeous` crate | archive | ARCHIVE | Mostly per-grammar shims. | MODULES gorgeous (`restart/corpora/MODULES.md:188-212`). |

## 4. Root Workspace Migration

Current root metadata names the nine grammars and an old strategy table
(`Cargo.toml:18-29`, `Cargo.toml:41-56`). The strategy table names parser types,
builders, documents, and modules, which violates the future grammar contract.

Disposition:

| Current artifact | Fate | Replacement |
|---|---|---|
| `[workspace].members` old crate list | ABROGATE-REPLACE | 24-crate greenfield member list from `restart/ARCHITECTURE.md`. |
| `[workspace.metadata.bbnf].grammars = [...]` array | KEEP-MODIFY | Per-grammar tables under `[workspace.metadata.bbnf.grammars.<name>]`. |
| `[workspace.metadata.bbnf-strategy]` | ABROGATE-DELETE | Auto-detected strategy facts in `passes` and generated runtime metadata. |
| Comments requiring `PRODUCTION_MANIFEST_TABLE` edits | ABROGATE-DELETE | Future grammar gate that forbids Rust edits. |
| Current dev profile and dependencies | KEEP-MODIFY | Re-evaluate under new crates; keep only active shared dependencies. |

Hard gate:

```sh
rg "bbnf-strategy|PRODUCTION_MANIFEST_TABLE|JsonParser|CssL4Parser|GrammarAuditTag" Cargo.toml crates
```

The command must not find production hardcoded grammar dispatch in generic
crates after tranche A closes.

## 5. `crates/core` Disposition

The module corpus shows `core` is the largest current crate, with handwritten
logic mixed with generated parsers, backend code, runtime code, path execution,
serialization, and per-grammar modules (`restart/corpora/MODULES.md:589-999`).
The restart does not preserve that crate boundary.

### 5.1 Grammar And Generated Parsers

| Current path | Fate | Replacement |
|---|---|---|
| `crates/core/src/grammar/generated/*.rs` | GENERATED-REPLACE | `runtime/src/grammars/<name>/generated.rs` emitted from Backend IR templates. |
| `crates/core/src/grammar/generated/.registry.json` | ABROGATE-DELETE | Workspace metadata and generated manifest derived by pipeline. |
| `crates/core/src/grammar/*` handwritten AST/parser helpers | ABROGATE-MOVE/KEEP-MODIFY | `grammar/src/ast`, `grammar/src/parse`, `grammar/src/validate`. |
| Bootstrap parser helpers | KEEP-MODIFY | `grammar/src/bootstrap` and `pipeline` bootstrap tests. |

The corpus records nine generated grammars in the old layout
(`restart/corpora/MODULES.md:609-629`). The future layout keeps nine generated
grammar modules initially, but they are emitted from templates and are not
hand-edited.

### 5.2 Backend And Lowering

| Current path | Fate | Replacement |
|---|---|---|
| `crates/core/src/backend/**` | ABROGATE-REPLACE | `codegen/src/lower`, `codegen/src/rust`, `codegen/src/wasm`, `codegen/src/templates`. |
| Old emitter driver walking Grammar IR | ABROGATE-DELETE | BIR-only lowerer contract. |
| backend template fragments | KEEP-MODIFY | New template system after removing grammar-name assumptions. |
| tests proving emitted behavior | KEEP-MODIFY | `codegen::verify`, `vm`, `test-fixtures`. |

PASS-2 says lowerers consume Backend IR and commit regenerated output
(`restart/audit/pass-2-codegen/PASS-2.md:32-49`). Any current backend file that
walks Grammar IR is therefore replaced, even if its local formatting logic is
mined.

### 5.3 Runtime

| Current path | Fate | Replacement |
|---|---|---|
| `crates/core/src/runtime/mod.rs` and generic support | KEEP-MODIFY | `runtime/src/document`, `runtime/src/builder`, `runtime/src/support`. |
| `crates/core/src/runtime/<grammar>/**` | GENERATED-REPLACE | `runtime/src/grammars/<name>/**` generated template output. |
| OpenFrame/checkpoint-heavy fallback logic | ABROGATE-REPLACE | Tape builder with bounded checkpoints. |
| direct struct builders | KEEP-MODIFY/REPLACE | Direct builders scheduled with tape emission. |

The restart sketch measured `Vec<OpenFrame>::clone` at 86.07 percent inclusive
samples in the current path (`restart/corpora/RESTART-SKETCH.md:154-184`).
The new runtime must prove OpenFrame clone stacks are gone.

### 5.4 Source, Imports, And Pipeline

| Current path | Fate | Replacement |
|---|---|---|
| `source`/span helpers | ABROGATE-MOVE | `source/src/file`, `source/src/span`. |
| import graph helpers | ABROGATE-MOVE/KEEP-MODIFY | `source/src/include` and `grammar::metadata`. |
| pipeline drivers | ABROGATE-MOVE/REPLACE | `pipeline/src/stages` following README pass order. |

### 5.5 Host And Grammar-Specific Shims

| Current path | Fate | Replacement |
|---|---|---|
| `css_types.rs` and similar host shims | ABROGATE-REPLACE | `host` generic primitives, metadata, and `@host fn`. |
| grammar-name match arms | ABROGATE-DELETE | Metadata-driven dispatch. |
| per-grammar runtime host files | GENERATED-REPLACE | Template-emitted `host.rs` under generated grammar module. |

CENSUS names `css_types.rs`, strategy registries, path registries, and
grammar-specific runtime shims as current generalization leaks
(`restart/corpora/CENSUS.md:103-122`).

### 5.6 Serialize And Legacy Fallbacks

| Current path | Fate | Replacement |
|---|---|---|
| `generate/serialize` and ser adapters | ABROGATE-DELETE | No production replacement; `ser` archive only. |
| legacy/fallback markers | ABROGATE-DELETE | Fail-explicit diagnostics or removed path. |

CENSUS counts legacy/fallback markers and fail-explicit rows that must be
retired during restart migration (`restart/corpora/CENSUS.md:170-215`).

## 6. `crates/ir` Disposition

Current `ir` has useful raw material but not the final architecture. The corpus
calls the structure sound while identifying many large files and misplaced
responsibilities (`restart/corpora/MODULES.md:264-505`).

| Current area | Fate | Replacement |
|---|---|---|
| Grammar-like IR types | KEEP-MODIFY | `ir/src/grammar_ir`. |
| Backend/output IR pieces | ABROGATE-REPLACE | `ir/src/backend_ir` with PASS-2 23 variants. |
| Strategy registries with grammar names | ABROGATE-DELETE | Metadata-derived profiles and side tables. |
| Type/checking facts | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/layout` (HM + bidirectional + CSP subroutine), `ir/src/side_tables` (`LayoutFacts`). |
| Shape/mining facts | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/shapes`, `passes/src/recognizers`. |
| Egraph bridge code | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/bridge`, generic `egraph`. |
| CSP-facing strategy code | KEEP-MODIFY/ABROGATE-MOVE | `passes/src/extract`, `csp-solver`, `cost-model`. |
| VM/debug execution | ABROGATE-MOVE/REPLACE | `vm`. |

Hard gates:

```sh
rg "Json|Css|Csv|Google|Math|Bnf|Ebnf" crates/ir/src
rg "emit.*Grammar|walk.*Grammar" crates/ir/src crates/codegen/src
```

After migration, grammar names are not part of generic IR logic, and emitters
do not walk Grammar IR.

## 7. `analysis` And `lsp` Disposition

`analysis` and `lsp` carry diagnostics, document state, semantic indexing, and
editor behavior. They do not survive as separate production crates because the
README workspace names one `bbnf-language-server` crate
(`restart/README.md:29-60`), and PASS-3 explicitly routes error recovery,
incremental parsing, LSP, playground, and DAP into the runtime/user surface
handoff (`restart/audit/pass-3-runtime/PASS-3.md:137-158`).

| Current area | Fate | Replacement |
|---|---|---|
| Diagnostics | KEEP-MODIFY/ABROGATE-MOVE | `error`, `bbnf-language-server/diagnostics`. |
| Document snapshots | KEEP-MODIFY/ABROGATE-MOVE | `source/snapshot`, `bbnf-language-server/document`. |
| Incremental parse hooks | ABROGATE-REPLACE | `DocumentSnapshot`, `ReparsePlan`, pipeline incremental entry. |
| LSP protocol code | KEEP-MODIFY | `bbnf-language-server/protocol`. |
| Analysis-only grammar assumptions | ABROGATE-DELETE | Metadata and Grammar IR facts. |

Gate:

```sh
cargo test -p bbnf-language-server diagnostics incremental
```

## 8. Path Crates Disposition

Lock 7 consolidates old path crates into `path`, `path-core`, and `path-ts`
(`restart/locks/14-LOCKS.md:46`). The module corpus already identifies
duplication and registry problems in the current path crates
(`restart/corpora/MODULES.md:232-260`).

| Current area | Fate | Replacement |
|---|---|---|
| Rust macro parser | KEEP-MODIFY | `path/src/macro_impl` plus shared parser in `path-core`. |
| Shared path AST/evaluator ideas | KEEP-MODIFY | `path-core/src/ast`, `path-core/src/eval`. |
| TypeScript hardcoded registry/docs | ABROGATE-REPLACE | `path-ts` generated schema from `path-core` facts. |
| Grammar-specific path mirrors | ABROGATE-DELETE | Runtime views and generated metadata. |
| Fixture duplicates | ABROGATE-MOVE | `test-fixtures`. |

Gate:

```sh
rg "json|css_l4|css_pretty|google_sheets|math" crates/path crates/path-core crates/path-ts
```

Path crates may use fixture names in tests, but not production registries.

## 9. Sister Crates Disposition

### 9.1 `csp-solver`

The corpus classifies `csp-solver` as generic and worth keeping, while calling
out large files and split work (`restart/corpora/MODULES.md:73-132`).
It remains a finite-domain choice solver, not the owner of HM equality
unification; `passes::layout` produces internal type obligations before any
CSP-backed finite choice is solved.

Fate:

| Area | Fate |
|---|---|
| Generic domain/constraint/solve APIs | KEEP-OUTRIGHT/KEEP-MODIFY |
| BBNF-specific bridge code | ABROGATE-MOVE to `passes::bridge`, keyed by stable Grammar IR node IDs and e-class IDs rather than chosen e-node representatives. |
| Oversized modules | KEEP-MODIFY split under Lock 13 |
| Tests | KEEP-MODIFY |

### 9.2 `egraph` And `egraph-derive`

The corpus says `egraph` and `egraph-derive` are extracted together and useful
as generic crates (`restart/corpora/MODULES.md:136-162`).
The migration keeps generic arena/rewrite/extract/explain code while moving
bridge justifications, rewrite guards, and CSP legality facts into `passes`.

Fate:

| Area | Fate |
|---|---|
| Generic egraph arena/rewrite/extract/explain | KEEP-MODIFY |
| Derive macro | KEEP-MODIFY |
| BBNF bridge terms | ABROGATE-MOVE to `passes::bridge` |

### 9.3 `simd-scan`

The corpus marks `simd-scan` clean and KEEP-AS-IS, while noting the NEON
intrinsics file as split-exempt in the old audit (`restart/corpora/MODULES.md:47-69`).

Fate:

| Area | Fate |
|---|---|
| Scalar/NEON/AVX kernels | KEEP-OUTRIGHT/KEEP-MODIFY |
| Dispatch API | KEEP-MODIFY for `SimdScan` BIR integration, with `Exact` scans proving scalar offset parity and `Prefilter` scans routing candidate offsets through `RegexProgram` or scalar verification before tape emission. |
| Tests/fixtures | KEEP-MODIFY |

PASS-2 requires SIMD coverage across scalar, NEON, AVX2, AVX512, and WASM SIMD
paths (`restart/audit/pass-2-codegen/PASS-2.md` §3).

Research-source hygiene: this migration surface relies on the local corpora and
PASS citations above for disposition evidence. Unverified research-index leads
such as Hubbard's JSON comparison row, Almomany cost-model wording, the exact
Deb bibliography variant, Ungar/Adams, and HelpMate remain bibliography
receivers, not migration evidence.
Regex and SIMD migration gates therefore compare behavior against verified
local corpora, PASS contracts, and `parse-that-regex` internal cross-engine
parity (NFA vs lazy DFA vs full DFA vs VM) rather than unverified catalogue
leads.

## 10. Archive Crates

| Crate | Fate | Reason |
|---|---|---|
| `ser` | ARCHIVE | Module corpus says no production caller and archive-only (`restart/corpora/MODULES.md:165-184`). |
| `gorgeous` | ARCHIVE | Module corpus says it is mostly per-grammar shims and archive-only (`restart/corpora/MODULES.md:188-212`). |

Archive procedure:

```sh
git mv crates/ser restart-archive-2026-05-04/crates/ser
git mv crates/gorgeous restart-archive-2026-05-04/crates/gorgeous
```

The exact archive destination belongs to tranche A. This Phase 2 document only
sets the disposition.

## 11. Generated Code And Runtime Template

PASS-2 sets the runtime template schema and generated output tree
(`restart/audit/pass-2-codegen/PASS-2.md` §7). It also sets generated LOC
budget tracking and a +2 percent ceiling (`restart/audit/pass-2-codegen/PASS-2.md` §6).

Generated migration:

| Current generated source | Replacement |
|---|---|
| `crates/core/src/grammar/generated/<name>.rs` | `runtime/src/grammars/<name>/generated.rs`. |
| Handwritten per-grammar runtime builders | Template-emitted builder/view/value/visitor files. |
| `.registry.json` | Generated manifest from metadata, not committed as source of truth. |
| Production parser manifest tables | Removed. |

Generated output rules:

1. Generated files are committed.
2. Generated files carry a header with grammar source hash, metadata hash, and
   Backend IR hash.
3. Regeneration must be byte-for-byte equal unless the tranche explicitly
   updates expected output.
4. Generated LOC budgets are tracked by grammar and by total workspace.
5. Generated files may exceed 500 LOC; handwritten files may not.

## 12. Tests And Fixtures

The restart creates `test-fixtures` because fixture and parity work is shared
by runtime, codegen, CLI, language server, and bench crates. The legacy BD
fixture spec used a shared worktree fixture package and parity matrix as close
gates; the inheritance index keeps BD as the source for parity/publication
discipline (`restart/inheritance/INDEX.md:29-40`).

Migration:

| Current tests | Fate |
|---|---|
| Generic solver/scanner/egraph tests | KEEP-MODIFY in owner crates. |
| Core generated parser tests | GENERATED-REPLACE and move fixtures to `test-fixtures`. |
| Backend golden tests | KEEP-MODIFY around Backend IR and VM replay. |
| Path duplicate fixtures | ABROGATE-MOVE to `test-fixtures`. |
| Inline tests in oversized modules | KEEP-MODIFY into module tests or integration tests. |

CENSUS identifies inline test violations that must be cleaned while splitting
files (`restart/corpora/CENSUS.md:383-399`).

## 13. New Facilities

These facilities do not exist as clean production crates today. They are not
optional; they are the replacement architecture.

| New path | Facility | First owner | Source |
|---|---|---|---|
| `crates/bbnf` | Public library facade. | A/B | README crate table (`restart/README.md:29-60`). |
| `crates/bbnf-cli` | Public CLI. | A/F/I | README crate table (`restart/README.md:29-60`). |
| `crates/bbnf-language-server` | Consolidated LSP. | A/I | PASS-3 runtime tree (`restart/audit/pass-3-runtime/PASS-3.md:160-289`). |
| `crates/bbnf-bench` | SOTA and fixture bench harness. | A/H/J | README SOTA targets (`restart/README.md:322-340`). |
| `crates/error` | Shared diagnostics. | A/I | PASS-3 recovery contract (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| `crates/source` | Source files, spans, snapshots. | A/I | README pipeline/incremental (`restart/README.md:188-207`, `restart/README.md:344-348`). |
| `crates/grammar` | BBNF AST/parser/validation. | A/D | PASS-1 crate tree (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `crates/pipeline` | Stage orchestration and artifact verification. | A/F | README pass order (`restart/README.md:188-207`). |
| `crates/passes` | Type/shape/recognizer/extract/bridge passes, including HM equality obligations, expected checking, bounded coercion, finite CSP choices, stable bridge IDs, and extraction-time legality. | C/H | PASS-1 commitments (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| `crates/vm` | Backend IR replay/debug. | E/I | README VM debug/replay (`restart/README.md:344-348`). |
| `crates/codegen` | BIR-only lowerers and templates. | E/F/H | PASS-2 (`restart/audit/pass-2-codegen/PASS-2.md` §2-§7). |
| `crates/runtime` | Tape/direct runtime, payload policy, snapshot-scoped tape identity, typed projections, and generated grammar modules. | B/F | Lock 1 (`restart/locks/14-LOCKS.md:34`). |
| `crates/host` | Generic host primitive/registry system. | D/F | README host decisions (`restart/README.md:160-182`). |
| `crates/cost-model` | `CostDecision` facts, objective profiles, Pareto/frontier evidence, solver-backed extraction adapters, LOC budgets. | C/H/J | PASS-1 cost model (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `crates/path-core` | Shared path semantics. | G | Lock 7 (`restart/locks/14-LOCKS.md:46`). |
| `crates/parse-that` | Parser combinator family below BBNF, paired with the regex sub-crate `crates/parse-that-regex` (renamed from legacy `bbnf-regex` per Lock 11). Grammar-owned HIR/verifier integration; cross-engine parity (NFA, lazy DFA, full DFA, VM) is internal to `parse-that-regex`; no third-party regex oracle is cited. | D/H | README Unicode routing (`restart/README.md:131-143`); Lock 11 (`restart/locks/14-LOCKS.md:54`). |
| `crates/test-fixtures` | Shared fixtures and parity matrix. | A/G/J | Inheritance map (`restart/inheritance/INDEX.md:29-40`). |

## 14. LOC Trajectory

The restart controls generated source because generated code currently
dominates the workspace. PASS-2 records a generated LOC baseline and a +2
percent ceiling for emitted runtime source (`restart/audit/pass-2-codegen/PASS-2.md` §6).

| Phase | Expected movement | Gate |
|---|---|---|
| Pre-A | Current generated and handwritten code remain untouched except archives. | Clean status before branch/tag. |
| A | `ser` and `gorgeous` leave production workspace; crate skeletons add small handwritten LOC. | `cargo metadata`, tree lint. |
| B | Runtime tape/direct handwritten support appears; no generated explosion yet. | Runtime tests. |
| C | IR/pass side-table code grows; old `ir` large files are split or replaced. | IR/pass tests and Lock 13. |
| D | BBNF parser/type extension LOC grows; rewrite-mode stays absent. | Extension parser tests. |
| E | Backend IR and VM LOC grows; old backend walkers still not carried over. | BIR/VM tests. |
| F | Generated runtime output lands for seed grammars; old generated layout retires. | Regen equality and LOC budget. |
| G | Path/value/visitor LOC grows; hardcoded path registries retire. | Future grammar test. |
| H | SIMD/WASM/Pratt LOC grows under feature gates. | Platform and SOTA tests. |
| I | LSP/recovery LOC consolidates `analysis` and `lsp`. | LSP incremental parity. |
| J | Final docs and publication metadata settle; no new architecture. | Full parity and docs checks. |

The steady-state goal is not “least LOC.” It is less handwritten duplication,
bounded generated growth, and no grammar-name maintenance cost.

## 15. Commit-Chain Disposition

Implementation should preserve the old workspace history while making the
greenfield change obvious in Git.

| Step | Action | Evidence |
|---|---|---|
| 1 | Tag the pre-restart state as `pre-restart-2026-05-04`. | `git rev-parse pre-restart-2026-05-04`. |
| 2 | Create `master-greenfield-2026-05-04` or the user-approved equivalent. | Branch exists and points to restart base. |
| 3 | Archive legacy-only crates in one body-bearing commit. | Diff contains only archive membership and workspace removal. |
| 4 | Create skeleton crates in dependency order. | `cargo metadata` and `cargo check --workspace`. |
| 5 | Move kept code with `git mv` where useful. | Diffs show moves rather than unrelated rewrites. |
| 6 | Replace architecture-conflicting code with tests first where possible. | Commit bodies name why, what, evidence, routed remainder. |
| 7 | Land generated runtime output only after equality and budget gates exist. | Generated commit includes budget evidence. |

The exact branch operation is future implementation work; this synthesis commit
does not create branches or tags.

Branch/tag routing floor:

| Artifact | Status | Owner | Evidence command |
|---|---|---|---|
| `pre-restart-2026-05-04` tag | Required history marker before implementation source edits. | A.W0. | `git rev-parse pre-restart-2026-05-04` resolves to a commit. |
| `master-greenfield-2026-05-04` branch | Suggested implementation branch unless the user selects another branch name. | A.W0. | `git rev-parse --verify master-greenfield-2026-05-04` resolves; `git symbolic-ref refs/heads/master-greenfield-2026-05-04` shows the tracked branch. |
| Workspace skeleton commit | Body-bearing workspace genesis with `[workspace.members]` named per Architecture §1. | A.W1. | `git diff --name-only A.W0..A.W1` lists only `Cargo.toml`, crate `Cargo.toml`, and `lib.rs` stubs. |
| Archive commits | Body-bearing and narrow to workspace/archive membership. | A.W0. | `git diff --stat A.W0~..A.W0` shows only archive-membership files; `cargo metadata --no-deps` lists no archive crate. |
| Generated commits | Body-bearing with equality, generated LOC, and routed remainder evidence. | F. | `cargo xtask bbnf build --all && git diff --exit-code crates/runtime/src/grammars`. |
| Branch operation enforcement | No tranche after A.W0 may create or rename branches/tags without an explicit migration commit. | All. | `git reflog --date=iso master` shows no rewrite-history operations after A close. |

## 16. Legacy BA-BD Inheritance

The legacy plan-set is not discarded. It is mined. The inheritance index maps
old BA/BB/BC/BD into new tranches A-J (`restart/inheritance/INDEX.md:29-40`).

| Legacy source | Keep | Do not keep |
|---|---|---|
| BA | Archive ceremony, god-module pressure, grammar generalization, close discipline. | Old anti-tape scrub and direct-only substrate. |
| BB | Optimization, Pratt/SIMD, path/visitor pressure, template thinking. | Topic-only waves and any grammar registry carry-forward. |
| BC | Backend ABI, typed IR, parity pressure. | Emitters walking grammar source and stale IR counts when PASS-2 differs. |
| BD | Fixture package, cross-backend matrix on the Rust line, publication order. | Premature TypeScript or WASM production: TS + WASM defer post-V1 as a principled architectural fork; V2 `TsBackend: Backend` and `WasmBackend: Backend` per `restart/ARCHITECTURE.md` §7.5 own the V2 carry. |

`docs/tranches/BA/BA.md` describes BA as a surgical foundation tranche with
archive and close gates (`docs/tranches/BA/BA.md:5-40`). That discipline is
kept, while its old direct-only substrate is superseded by Lock 1.

## 17. Tranche-Level Migration Sequence

The migration is sequenced by dependency and consumer gates, not by topic.
Lessons Learned says same-wave consumer gates are mandatory and split waves by
dependency, not topic (`docs/precepts/instructions/LESSONS-LEARNED.md:1-34`).

| Tranche | Migration work |
|---|---|
| A | Workspace genesis, archive crates, Cargo metadata schema, source/error/grammar skeleton, tree lint gates. |
| B | Tape/direct runtime substrate, value/document API, generated runtime template shell. |
| C | Grammar IR, internal type obligations, shape facts, stable CSP/egraph bridge facts, objective cost evidence, extraction legality. |
| D | BBNF extension parser/typing for lookbehind, rank-1 generics, host definitions/chains, bounded coercion sites, error/layout; regex Unicode below BBNF. |
| E | Backend IR, VM, extraction, lowerer contract. |
| F | Rust lowerer, runtime template output, regen equality. |
| G | Path/path-core/path-ts split, visitor, mutation API, future grammar gate. |
| H | Pratt, verifier-bound exact/prefilter SIMD, `parse-that-regex` internal cross-engine parity, Rust-line SOTA early gates. WASM defers post-V1 alongside the V2 `WasmBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. |
| I | Error recovery, snapshot/reuse-map incremental parsing, language server, playground/debug surfaces. |
| J | Parity, benchmarks, docs, publication readiness, close. |

This sequence keeps the tranche set at stub level. Full per-wave drafting
belongs to the next phase.

## 18. Greenfield Mechanics Summary

The synthesis prompt authorizes a greenfield restart with a commit-chain
disposition rather than hand-patching old code in place. The migration plan is:

1. Preserve the current state with a tag or branch before implementation.
2. Archive legacy-only crates and docs as the first tranche A action.
3. Replace the root workspace and crate tree in dependency order.
4. Bring forward kept code through intentional moves, not broad copy/paste.
5. Regenerate runtime output after Backend IR and lowerers exist.
6. Keep each tranche reversible through commits with body-bearing rationale.

Commit discipline comes from the local precept: broad, generated, deletion,
gate/status, benchmark, profiling, no-verify, and history-relevant commits need
bodies with why, what landed, evidence, and routed remainder
(`docs/precepts/instructions/LESSONS-LEARNED.md:56-72`).

## 19. Migration Gates

### 19.1 Generalization

```sh
cargo xtask lint-grammar-generalization
rg "JsonParser|CssL4Parser|CssPrettyParser|CsvParser|BbnfParser|EbnfParser|BnfParser|GoogleSheetsParser|MathParser" crates
rg "PRODUCTION_MANIFEST_TABLE|GrammarAuditTag|bbnf-strategy" Cargo.toml crates
```

Expected result: no production hits in generic crates.

### 19.2 Tree Shape

```sh
cargo xtask lint-tree
cargo xtask lint-loc --handwritten-max 500
```

Expected result: 4-10 children per handwritten source directory and no
handwritten Rust file over 500 LOC.

### 19.3 Backend Boundary

```sh
rg "GrammarIr|GrammarIR|grammar_ir" crates/codegen/src
rg "use .*grammar_ir|crate::grammar_ir|ir::grammar_ir" crates/codegen/src
cargo test -p codegen backend_ir_only
cargo test -p vm replay_all_backend_ir_variants
```

Expected result: codegen can name Backend IR and side-table types, but lowerers
do not walk Grammar IR.

### 19.4 Runtime Substrate

```sh
rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src
cargo test -p runtime tape_direct_union
cargo test -p runtime __EAGER_EMPTY_PATH
cargo test -p runtime cursor_decision_skip
cargo test -p runtime tape_identity_payload_projection
cargo bench -p bbnf-bench --bench sota_json
```

Expected result: no old OpenFrame clone stack or ParseStream runtime concept.
`ParseStream` may remain only in proc-macro code that uses `syn`.
Runtime rows prove one `(TapeId, node id, payload class)` identity, direct
scalar caches over declared payload slots, validation/source ownership metadata,
and verifier-before-tape behavior for any SIMD prefilter path.

### 19.5 Generated Equality

```sh
cargo xtask bbnf build --all
git diff --exit-code crates/runtime/src/grammars
cargo xtask generated-loc-budget --max-growth 1.02
```

Expected result: regenerated output is equal and within budget.

### 19.6 Future Grammar

```sh
git diff --exit-code -- grammars/yaml.bbnf Cargo.toml
cargo xtask bbnf check yaml
cargo xtask bbnf build yaml
git diff -- crates ':!crates/runtime/src/grammars/yaml'
rg "yaml|Yaml" crates/*/src
```

Expected result: yaml enters through only `grammars/yaml.bbnf` plus workspace
metadata. Runtime output may be generated; generic crate source may not learn a
yaml name.

### 19.7 Diagnostic And Carry Proof

```sh
cargo test -p error diagnostic_codes_are_stable
cargo test -p bbnf-language-server diagnostics_match_cli
cargo xtask migration-carry --check
```

Expected result: migration does not drop receiver/blocker/gate rows for
deferred work, and public diagnostics are shared by CLI and LSP.

## 20. Unresolved Migration Punch List

Migration-implementation receivers are tracked at `restart/MASTER-PLAN.md` §24
(Carry and Friction Ledger) with `Source: migration` or
`Source: synthesis + migration` tags. The migration-sourced items — exact
generated header fields, declaration-crate review form, benchmark host
hardware profiles, archive destination for `ser`/`gorgeous`, PASS-2 BIR
snapshots, and Lock 3 cursor gates — appear in that consolidated ledger;
this section retains its heading for cross-document anchoring but no longer
carries a separate table. The `path-ts` publication timing and the WASM
exported ABI defer post-V1 alongside the V2 `TsBackend: Backend` and
`WasmBackend: Backend` impls per `restart/ARCHITECTURE.md` §7.5; both route
to V2 amendment and no longer occupy V1 carry rows. The single carry-truth
principle holds: one ledger, two sources, one set of receivers.

## 21. Migration Close

The restart migration keeps generic, tested infrastructure and removes the old
grammar-name architecture. It archives `ser` and `gorgeous`, splits `core`,
consolidates `analysis` and `lsp`, replaces backend/runtime generation around
Backend IR, preserves generic `csp-solver`, `egraph`, and `simd-scan`, and
creates the public/user crates required by the README.

The migration is done only when a new grammar can be added through `.bbnf` plus
metadata, runtime output regenerates equally, lowerers consume Backend IR only,
and no generic crate carries grammar-name dispatch.
