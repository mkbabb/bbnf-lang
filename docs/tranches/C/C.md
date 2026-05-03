# Tranche C — Parse + IR Foundation

## Gestalt

Tranche C lands the parser-front substrate proper. The four crates that emerged conceptually from Pass A's Proposal 1 + Proposal 2 — `bbnf-grammar`, `bbnf-parse`, `bbnf-ir`, `bbnf-passes` — receive substantive content, retiring the prior `crates/core/src/{lower, imports, types, grammar/{mod, host, schema}}` and `crates/ir/src/{types, dag, registry, passes}` to their new homes. The Lock 2 substantive fold lands: `TypeDesc` → `Layout`; `StructLayout` → `Layout`; `StructRegistry` → `LayoutRegistry`; `TypeMap` retires (folds into `Layout`'s representation per Pass A §7 W2.13); the `LayoutSink` trait lands per Pass A facility #8. The thirteen god-module SPLIT obligations identified by Pass A §1.7 land in this tranche — the 1530-LOC `grammar_facts.rs`, the 1361-LOC `csp_strategy/mod.rs`, the 843-LOC `materialization/classify.rs`, the 786-LOC `passes/types/mod.rs`, and the rest distribute per the master plan §4.6 sub-tree. The `bbnf-vm/` extraction lands per Pass A Proposal 2. The `bbnf-host/` extraction from `grammar/host.rs` lands per Pass A facility #2 + Lock 14 generality. The `path-core/` substrate consolidates per Pass A Proposal 3 + Lock 7.

This is the largest tranche by file-count surface. The parser-front audit's punch list (Pass A §7) sequences W0 through W8; the master plan absorbs that sequencing here as C.W0 through C.W6. Substantive Layout-vocabulary fold (W2) and god-module SPLITs (W6) coexist because the symbol renames touch the same files the SPLITs reshape; landing them sequentially within one tranche avoids cross-tranche merge conflicts.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| `bbnf-grammar` content migration complete | C.W0 | `crates/bbnf-grammar/src/{ast, parse, imports}/` populated per master plan §4.3; `parse_grammar(input)` smoke-tests one grammar |
| `bbnf-parse` content migration complete | C.W1 | `crates/bbnf-parse/src/{source, scanner, lower}/` populated per master plan §4.4; lower passes round-trip BBNF source |
| Lock 2 substantive fold complete | C.W2 | `rg -nE '\b(TypeDesc\|StructLayout\|StructRegistry\|TypeMap)\b' crates/bbnf-ir/src/ crates/bbnf-passes/src/` returns 0; downstream consumers reference `Layout`, `LayoutRegistry`, `LayoutSink` |
| `bbnf-ir` content migration complete | C.W3 | `crates/bbnf-ir/src/{types, dag, registry, cost_config}/` per master plan §4.5; `LayoutSink` trait lands |
| `bbnf-passes` content migration complete | C.W4 | `crates/bbnf-passes/src/` per master plan §4.6; god-module SPLITs land |
| God-module SPLITs complete (13 obligations) | C.W6 | `find crates/{bbnf-parse,bbnf-grammar,bbnf-ir,bbnf-passes,bbnf-host,bbnf-vm}/src -name "*.rs" -not -path "*generated*" \| xargs wc -l \| awk '$1 > 500 { print }'` returns 0 |
| `bbnf-vm` extraction | C.W3 | `find crates/bbnf-vm/src/` populated; relocated from `crates/ir/src/vm/`; tests pass |
| `bbnf-host` extraction from `grammar/host.rs` | C.W1 | `crates/bbnf-host/src/` populated; the 584-LOC `host.rs` SPLIT into `host/{mod, observational, pipeline, directives}.rs` |
| `path-core` substrate consolidation | C.W6 | `crates/path-core/src/{ast, lex, lower, validate, type_check, registry, runtime}/` populated; `path-core/src/runtime/` carries the executor; `crates/path-ts/src/{compile, fixture}.rs` retired |
| `crates/core/` fracture (the largest single surgery) | C.W5 | `find crates/core/src/` returns nothing; `crates/bbnf/` is a thin re-export aggregator only |
| FAIL-EXPLICIT at `simple_kinds.rs:185` (no defensive fallbacks) | C.W6 | `rg 'fn kind_for.*\.\..*\|fn .*classify.*default\(\)' crates/bbnf-passes/src/` returns 0; defensive fallbacks panic |
| Inverse-layout-audit pass lands | C.W4 | `crates/bbnf-passes/src/audit/inverse_layout/` populated; build-fail on missing `Layout` for compound-typed rules |
| Inline-test moves complete | C.W6 | `rg '#\[cfg\(test\)\]' crates/{bbnf-parse,bbnf-ir,bbnf-passes,path-core}/src/` returns 0 (tests in `tests/`) |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| C.W0 — `bbnf-grammar` content migration | Move `crates/core/src/{types, grammar/{mod, host, schema}}` to `bbnf-grammar/src/`; SPLIT host.rs (584 LOC) into `host/{mod, observational, pipeline, directives}.rs` | 3 parallel | bbnf-grammar compiles; bbnf-host extracted; parse_grammar smoke-tests |
| C.W1 — `bbnf-parse` content migration + lower SPLITs | Move `crates/core/src/{lower, imports, pipeline}` to bbnf-parse; SPLIT `lower/expression/{mod, wrap}.rs` (539 + 731 LOC) and `lower/value_expr/atom.rs` (590 LOC) | 4 parallel | bbnf-parse compiles; lower passes round-trip BBNF |
| C.W2 — Lock 2 substantive fold | Rename `TypeDesc` → `Layout`, `StructLayout` → `Layout`, `StructRegistry` → `LayoutRegistry`; `TypeMap` retires (folds into `Layout`); `LayoutSink` trait lands | 2 parallel | Lock 2 verification grep passes; downstream consumers unchanged at API surface (rust-analyzer rename guides) |
| C.W3 — `bbnf-ir` content migration + `bbnf-vm` extraction | Move `crates/ir/src/{types, dag, registry, cost_config}` to bbnf-ir; move `crates/ir/src/vm/` to bbnf-vm; SPLIT `types/grammar.rs` (584 LOC) | 3 parallel | bbnf-ir + bbnf-vm compile; LayoutSink trait wired |
| C.W4 — `bbnf-passes` content migration + god-module SPLITs (12 surgeries) + inverse-layout-audit | Move `crates/ir/src/passes/` to bbnf-passes; SPLIT `csp_strategy/mod.rs` (1361), `recognizers/grammar_facts.rs` (1530), `materialization/classify.rs` (843), `passes/layout/mod.rs` (786), `passes/layout/registry.rs` (510), `csp_domains.rs` (500), `payload/layout.rs` (514), `audit/payload_coverage.rs` (585); land `audit/inverse_layout/` | 5 parallel | every god module < 500 LOC; inverse-layout-audit gate passes |
| C.W5 — `crates/core/` fracture | Eliminate `crates/core/`; `crates/bbnf/` becomes thin re-export aggregator; `cargo check --workspace` passes | 2 parallel | `find crates/core` returns nothing; `cargo check` green |
| C.W6 — `path-core` consolidation + remaining cleanup | `path-core/src/` consolidates per master plan §4.13; `path-ts/src/{compile, fixture}.rs` retire; legacy `crates/core/src/runtime/path.rs` retires; FAIL-EXPLICIT at simple_kinds.rs:185; inline-test moves | 3 parallel | path-core compiles; mirror eliminated (~−720 LOC); legacy alphabet eliminated (~−163 LOC); FAIL-EXPLICIT verified |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| `bbnf-error` ready for cross-crate consumption | B | B.W0 |
| `bbnf-pipeline` ready for parser orchestration | B | B.W2 |
| Lock 2 directory rename complete; naming canon list ready | B | B.W3 |
| Skeletal bbnf-grammar/parse/ir/passes/vm/host crates | A | A.W2 |
| `crates/{analysis, lsp}` consolidated to `bbnf-language-server/` | A | A.W1 |
| Lock 14 retirement complete (bbnf-ir grammar idents removed) | A | A.W3 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| `bbnf-grammar`, `bbnf-parse` ready for codegen-IR consumption | D | D.W0 |
| `bbnf-ir` Layout vocabulary + `LayoutSink` trait | D | D.W0 |
| `bbnf-passes` (every transformation pass) | D, E | D.W2, E.W0 |
| `path-core` ready for proc-macro shells (Lock 7) | (G if pointer macro lands then; else continuous) | G.W2 |
| Inverse-layout-audit gate fires per-grammar | E | E.W2 |
| `bbnf-vm` ready for VM-driven testing | F | F.W4 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | substantive-honoured | C.W6 (typed-IR consolidation eliminates final tape narrative residue at the IR level) |
| 2 — Layout canon | honoured | C.W2 (substantive fold complete) |
| 3 — Cursor + byte-skip | partial | C.W6 (path-core consolidation); substantive completion at E.W4 |
| 4 — Per-domain orthogonal | substrate-prep | (bbnf-passes carries each domain's pass module independently) |
| 5 — IR + per-backend | partial | C.W4 (bbnf-passes/audit/inverse_layout/ enforces IR-as-contract); substantive completion at D.W3 |
| 6 — xtask source emit | n/a | — |
| 7 — `crates/path/` consolidated | honoured | C.W6 |
| 8 — Surpass SOTA | n/a (pre-codegen) | — |
| 9 — Slice-borrow primary | n/a | — |
| 10 — Pratt + SIMD auto-detected | n/a | — |
| 11 — Path-deps for sister crates | honoured | (continuous) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | substantive-honoured | C.W6 (every god module < 500 LOC) |
| 14 — Full grammar generalisation | substrate-honoured | (bbnf-passes carries no grammar-named module post-C; verification gate fires per-tranche) |
| `feedback_typed-materialization-invariant` | honoured | C.W4 (inverse-layout-audit) |
| `feedback_no-inline-tests` | honoured | C.W6 |
| `feedback_no-workarounds` | honoured | C.W6 (FAIL-EXPLICIT at simple_kinds.rs:185) |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| Lock 2 rename breaks downstream compile due to missed reference | C.W2 staged: directory rename first (B.W3 mechanical), then symbol rename (rustc errors guide), then verification scan; per master plan §13 R5 |
| god-module SPLIT mid-tranche introduces compilation breakage | C.W6 per-SPLIT: pre-split `cargo check`, split file at module-boundary commit, post-split `cargo check`; rejection triggers triumvirate; per §13 R6 |
| `crates/core/` fracture leaves orphan re-exports breaking downstream consumers | C.W5 sequenced after substantive content moves; `cargo doc` smoke confirms public-API surface re-exports through `crates/bbnf/src/lib.rs` aggregator; per §13 R3 |
| `bbnf-vm` extraction introduces VM-IR coupling | C.W3 audit: `bbnf-vm` consumes `bbnf-ir` types but bbnf-ir does not consume bbnf-vm; cargo-tree confirms one-way dependency |
| Inverse-layout-audit pass false-positives on unfinished migration | C.W4 audit pass runs as a smoke test post-W3; build-failure mode activates only post-W4 close |
| `path-core/src/runtime/` consolidation breaks runtime executor consumers | C.W6 staged: path-core skeleton compiles + executor relocates + downstream consumers (per-grammar declaration crates, when they land in E) updated |
| Wave count (7) exceeds typical tranche budget | C is the largest tranche; risk ratified — alternative (split into C-I + C-II) was considered; one-tranche execution preferred per Pass A §7 punch list ordering |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| Per-wave `cargo check --workspace` | ≤ 90s incremental on M1 Pro | per wave close |
| god-module SPLIT rebuild time | ≤ 30s per file (incremental) | per SPLIT |
| Generated-LOC budget | C.exit: 166,750 LOC (-2K) | per master plan §12.2 |

## Voice locks

Per master plan §14. Tranche C's prose register: unpretentious-academic; lilt at ~5% (the IR substrate fold invites domain verbiage from compiler theory).

## Closing posture

Tranche C closes with the parser-front substrate consolidated. The Lock 2 substantive fold settles the IR vocabulary; the god-module SPLITs settle the IR cohesion; the path-core consolidation settles the path crate triplet; the bbnf-vm extraction settles the VM substrate. Tranche D's codegen IR contract receives the foundation it path-deps on.

The greenfield mandate carries: every god module retires through SPLIT (no carry-forward of mixed-concern files); every Lock 2 retired term scrubs (no carry-forward of `TypeDesc` / `StructLayout` / `TypeMap`); the `crates/core/` god directory dissolves entire. The substrate's bone structure tightens.
