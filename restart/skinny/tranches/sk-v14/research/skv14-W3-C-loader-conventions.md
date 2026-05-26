# SK-V14 W3-C: Loader Conventions

Date: 2026-05-26.
Scope: corpus loader shape for W3.
Output: this file.

## Section 1 - Findings

Existing JSON corpus loading uses manifest-backed file resolution and validates
size and SHA-256 before benchmark use. W3 needs the same fail-closed shape for
CSS L4: a stable roster, relative file paths, declared byte sizes, declared
hashes, and a runtime loader that reads the staged files from the skinny
workspace.

`bbnf-bench` is the right crate for the W3 loader because SPEC Section 6 names
`skinny/crates/bbnf-bench/src/css_l4_corpus.rs`, and W8 will later consume the
same production corpus through the bench layer.

## Section 2 - Recommendations

Add `bbnf_bench::css_l4_corpus` with:

- `CSS_L4_SK_V14_CORPORA`: four static specs.
- `CSS_L4_SK_V14_MIN_BYTES`: 800 KiB floor.
- `load_all()`: loads every staged file.
- a unit test validating count, total bytes, per-file bytes, SHA-256, and UTF-8.

Expose the module from `bbnf-bench/src/lib.rs`.

## Section 3 - Risks

Do not wire the loader into existing tiny fixture constants in
`nonjson_css_l4.rs`; that is W8 work. W3 should prove corpus availability, not
change admission rows.

## Section 4 - Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 6.
- `skinny/crates/test-fixtures/src/lib.rs`.
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`.
