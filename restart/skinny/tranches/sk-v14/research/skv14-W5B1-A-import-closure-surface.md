# SK-V14 W5B.1 A: Import Closure Surface

Date: 2026-05-26.
Scope: W5B.1 IMPORT-CLOSURE source inspection.
Output: this file.

## §1 — Findings

`skinny/crates/grammar/src/lib.rs` currently has W5A runtime source fact
scanning. `parse_runtime_source_facts` computes a request source digest and
records construct offsets, while `scan_runtime_source` classifies `@import` as
`RuntimeConstructKind::Import`.

No request-local import graph exists yet. The scanner records import presence
but not the imported target string, the importer path, the resolved request-map
path, or a DAG/cycle result.

`parse_grammar` already treats `@import` as an accepted public directive for the
skinny grammar parser, but that parser path does not resolve imports. W5B.1
should add a separate runtime frontend closure API for the request-owned codegen
path, leaving public parser semantics unchanged.

## §2 — Recommendations

W5B.1 should add:

- `FrontendClosure` keyed by request source path and stable source hash;
- `FrontendImport` edges from importer path to resolved request-map path;
- relative import resolution against the importer directory;
- fail-closed missing-import and import-cycle errors.

The source slice is `skinny/crates/grammar/src/lib.rs` only.

## §3 — Risks

Do not resolve imports through the filesystem or committed generated output.
Resolution must be bounded to the provided `RuntimeSource<'_>` request map.

Do not change provider/template topology or public directive semantics in W5B.1.
Those are separate W5C/W5D and parser-language concerns.

## §4 — Sources

- `skinny/crates/grammar/src/lib.rs`
- `restart/skinny/tranches/sk-v14/SPEC.md` §8B W5B.1.
