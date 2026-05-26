# SK-V14 W5-D: Lock 14 Governance Surface

Date: 2026-05-26.
Wave: W5.
Phase: research.
Agent: Chandrasekhar.
Scope: read-only inspection of W5 governance docs and Lock 14 gate.

## Question

Extract W5 acceptance criteria, preblocked routes, and contradictions before
implementation.

## Acceptance Criteria

W5 is a structural Lock 14 refactor:

- W4 ledger PRUNE is closed;
- no `runtime/` edits are made in W5;
- provider dispatch is trait/data driven rather than `RuntimeProvider` enum
  matching;
- eight per-grammar provider modules collapse to one grammar-agnostic provider
  path;
- seven CSS template directories are deleted in the same replacement slice;
- `regen_css.rs` consumes the new provider path;
- Lock 14 baseline enforces the post-W5 forward invariant;
- `regen-css` plus all seven CSS companion checks pass;
- table rows remain within +/-1.0%.

Post-W5 forward invariant: adding a grammar under
`workspace.metadata.bbnf.grammars.{name}` must produce zero new `.rs` files in
`skinny/crates/{codegen,runtime,passes,bbnf,grammar}/src/` and zero new
directories in `crates/core/src/runtime/`.

## Current Baseline

Current read-only counts are still pre-W5:

- `RuntimeProvider::Json|JsonGrammar|parse_json_grammar` grep count: 5;
- per-grammar provider modules: 8;
- CSS L4 template dirs: 7;
- Pattern H file count: 67.

`lock14_baseline.rs` is also pre-W5:

- generic scans exclude provider modules;
- `per_grammar_provider` and `per_grammar_template` are still accepted entry
  labels;
- there is no SK-V14 W5 parent-diff authorization.

## Preblocked Routes

- grammar-name branches in generic crates;
- per-grammar provider modules in generic codegen;
- deleting CSS provider/template files before replacement exists;
- preserving the eight providers for compatibility;
- JSON policy in generic crates;
- renamed JSON helpers;
- fake generated headers;
- gate relabel as admit;
- Track 1 / Track 2 coupling.

## Documentation Contradictions

- SPEC §8 references a `RuntimeProvider` enum in `passes/src/lib.rs`; current
  HEAD has it in `codegen/src/grammar_profile.rs`.
- P3-C still says W4 deletes CSS templates, while amended SPEC §7 makes W4
  ledger-only and moves deletion to W5.
- P3-C says W5 emits regen-derived runtime for every grammar, while SPEC §8
  says W5 must not touch `runtime/`; W6 owns runtime collapse.
- Some docs still suggest W8/W9/W10 can proceed independently of W5; the
  active prune-before-rebuild chain blocks them until PRUNE-1 through
  PRUNE-5 close.

These contradictions do not authorize a shortcut implementation. They support
a W5 REDRESS if W5 cannot honestly satisfy the source-consuming generator
contract.
