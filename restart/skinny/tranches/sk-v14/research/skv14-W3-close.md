# SK-V14 Wave W3 Close: Production CSS L4 Corpora

Date: 2026-05-26.
Wave: W3.
Phase: redress close.
Disposition: ADMITTED.

## Scope

W3 staged the SK-V14 production CSS L4 corpus set and added the
`bbnf-bench` loader that resolves the corpus files at runtime.

W3 does not admit any CSS L4 performance row. It does not edit
`skinny/RESULTS.md`, `skinny/REDRESS.md`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, generated CSS runtime output, or
`crates/core/src/runtime/css_l4/`.

## Landed Files

- `skinny/corpora/css-l4-sk-v14/bootstrap-5.3.3.min.css`
- `skinny/corpora/css-l4-sk-v14/tailwindcss-0.2.0.min.css`
- `skinny/corpora/css-l4-sk-v14/material-components-web-14.0.0.min.css`
- `skinny/corpora/css-l4-sk-v14/animate-4.1.1.min.css`
- `skinny/corpora/css-l4-sk-v14/manifest.md`
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs`
- `skinny/crates/bbnf-bench/src/lib.rs`

## Corpus Telemetry

| Family | Version pin | File | Bytes | SHA-256 |
|---|---|---|---:|---|
| Bootstrap | `bootstrap@5.3.3` | `bootstrap-5.3.3.min.css` | 232803 | `3c8f27e6009ccfd710a905e6dcf12d0ee3c6f2ac7da05b0572d3e0d12e736fc8` |
| Tailwind CSS | `tailwindcss@0.2.0` | `tailwindcss-0.2.0.min.css` | 179631 | `e463dd783548584666e5e50c47c305def32607a9a2dd64e7593908fc1839ee73` |
| Material Components Web | `material-components-web@14.0.0` | `material-components-web-14.0.0.min.css` | 495454 | `60f82e183aa0e791c1f3eb5bac905b5ae885f49f9708aeec8ec71a8b014c4f12` |
| Animate.css | `animate.css@4.1.1` | `animate-4.1.1.min.css` | 71750 | `5fbaeb9f8e25d7e0143bae61d4b1802c16ce7390b96ceb2d498b0d96ff4c853f` |

Total corpus bytes: 979638.

## Executable Evidence

Commands run at HEAD during W3 redress:

```sh
du -sh skinny/corpora/css-l4-sk-v14
test "$(du -sk skinny/corpora/css-l4-sk-v14 | awk '{print $1}')" -ge 800
find skinny/corpora/css-l4-sk-v14 -maxdepth 1 -type f -print | sort
wc -c skinny/corpora/css-l4-sk-v14/*.css
shasum -a 256 skinny/corpora/css-l4-sk-v14/*.css
cargo test -p bbnf-bench css_l4_sk_v14_corpora -- --nocapture
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --check -- skinny/crates/bbnf-bench/src/css_l4_corpus.rs skinny/crates/bbnf-bench/src/lib.rs skinny/corpora/css-l4-sk-v14/manifest.md
cargo xtask gate-json --check-results --skv14-existing-results-capture
```

Observed results:

- Corpus directory size: `964K`.
- Byte floor: >= 800 KiB.
- CSS byte counts: 71750, 232803, 495454, 179631; total 979638.
- SHA-256 values match the W3 plan and manifest.
- `bbnf-bench` corpus tests: 2 passed, 0 failed.
- Lock count invariant: 16.
- Pattern H runtime-file invariant: 67.
- `gate-json` maintained the existing-results capture gate.

## Exit Gate

W3 satisfies R5's production-corpus staging requirement and gives W4/W8 a
manifested corpus loader without changing admit status. W4 PRUNE-2 is now the
next executable wave.
