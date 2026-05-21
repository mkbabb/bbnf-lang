# SK-V12 W1b-2a Plan V3 - Lightningcss Comparator Row

Date: 2026-05-20.
Phase: W1b-2 revised plan after CHALLENGE V2.
Scope: SPEC Section 7.1, `G-W1b-2a-CSS-L4-LIGHTNINGCSS-COMPARATOR`.

## Selected Sub-Wave

W1b-2 is split into two sub-waves. This plan dispatches W1b-2a only:

```text
W1b-2a = dependency + fixture-limited comparator + strict equality artifacts + Criterion row
W1b-2b = SOTA report + admission gate + JSON guard
```

W1b-2a cannot admit CSS SOTA and cannot move `skinny/RESULTS.md`. It records
REDRESS comparator evidence and routes admission to W1b-2b.

## Intervention

For row:

```text
css_l4/declaration_values/direct_to_struct/main
```

and output plane:

```text
css_l4_declaration_value_fact_stream
```

add a lightningcss-gated source-sidecar fact emitter:

1. parse the frozen fixture with lightningcss;
2. verify fixture shape fail-closed;
3. validate the lightningcss declaration/property/importance/depth projection;
4. emit source-sidecar facts from original input bytes;
5. require byte-identical equality across Track 1, cssparser Track 2, and
   lightningcss-gated sidecar facts;
6. add Criterion row `lightningcss_same_plane_fact_stream` with sample count
   >= 30.

Do not claim lightningcss public APIs expose raw token facts or byte offsets.

## Fixture Limits

Fail closed unless the input is the W1b frozen fixture:

- SHA-256 `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`;
- 187 bytes;
- seven declarations;
- thirteen value tokens;
- one nested `@media` block;
- no declaration after an important declaration in the same block;
- no comments inside declaration values;
- no strings, URLs, custom properties, or duplicate-property cascade cases.

## Owner Paths

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/Cargo.lock`
- `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/track1-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/oracle-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/strict-equality.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt`
- `skinny/REDRESS.md`

## Dependency

Add only:

```toml
lightningcss = { version = "=1.0.0-alpha.71", default-features = false }
```

If dependency resolution or first compile consumes the cap, record
`BLOCKED/FAIL` with `/tmp/skv12-waveW1b-2a-rejected.patch`; do not broaden
owner paths.

## Commands

Run from `skinny/`:

```sh
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture

RUSTFLAGS="-C target-cpu=native" \
cargo bench -p bbnf-bench --bench nonjson_css_l4 -- --sample-size 30
```

W1b-2a intentionally does not run the SOTA report/gate command. That belongs
to W1b-2b after Criterion artifacts exist.

## Exit Gate

`G-W1b-2a-CSS-L4-LIGHTNINGCSS-COMPARATOR` passes only when:

- lightningcss dependency is comparator-local and lockfile evidence is present;
- fixture-shape checks fail closed;
- Track 1, cssparser Track 2, and lightningcss-gated sidecar facts are
  byte-identical;
- all three fact artifacts and equality artifacts are written;
- Criterion includes `lightningcss_same_plane_fact_stream` at sample count
  >= 30;
- REDRESS 124 states this is not CSS ADMIT and routes W1b-2b.

## REDRESS

Use REDRESS 124 for W1b-2a.

Accepted REDRESS wording must include:

- `PASS-COMPARATOR`, not `PASS-ADMIT-CANDIDATE`;
- fixture limits and fail-closed status;
- lightningcss version and `skinny/Cargo.lock` package checksum;
- equality artifact paths;
- Criterion command and artifact path for the third row;
- routed remainder: W1b-2b must implement `sk-v12-css-l4-sota-v1`, consume
  Criterion estimates, enforce the no-write report flag, and run JSON guards
  against `/tmp/skv12-w1a-json-guard-criterion` or a fresh populated JSON root.

Rejected patch path:

```text
/tmp/skv12-waveW1b-2a-rejected.patch
```
