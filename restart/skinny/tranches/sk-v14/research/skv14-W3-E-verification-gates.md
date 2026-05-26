# SK-V14 W3-E: Verification Gates

Date: 2026-05-26.
Scope: executable checks for W3 close.
Output: this file.

## Section 1 - Findings

W3 has no performance-admit gate. The falsifiability surface is corpus size,
manifest provenance, loader resolution, and invariant preservation.

## Section 2 - Recommendations

Run these checks:

```sh
du -sh skinny/corpora/css-l4-sk-v14
test "$(du -sk skinny/corpora/css-l4-sk-v14 | awk '{print $1}')" -ge 800
find skinny/corpora/css-l4-sk-v14 -maxdepth 1 -type f -print | sort
wc -c skinny/corpora/css-l4-sk-v14/*.css
shasum -a 256 skinny/corpora/css-l4-sk-v14/*.css
cargo test -p bbnf-bench css_l4_sk_v14_corpora_resolve_and_match_manifest -- --nocapture
cargo xtask gate-json --check-results --skv14-existing-results-capture
grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

## Section 3 - Risks

`cargo xtask gate-json` should remain a maintain check only. Any change in
RESULTS or rolling delta would be out of scope for W3 unless a later plan
explicitly upgrades W3, which this wave should not do.

## Section 4 - Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 6 and Section 2 W3 row.
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`
  Section 2.3.
