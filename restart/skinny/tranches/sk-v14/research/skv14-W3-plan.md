# SK-V14 Wave W3 Plan: Production CSS L4 Corpora

Date: 2026-05-26.
Wave: W3.
Phase: plan.
Inputs:
- `skv14-W3-A-spec-obligations.md`
- `skv14-W3-B-production-source-selection.md`
- `skv14-W3-C-loader-conventions.md`
- `skv14-W3-D-provenance-manifest.md`
- `skv14-W3-E-verification-gates.md`
- `skv14-W3-F-preblocks-redress.md`

## Intervention

Stage a pinned production CSS L4 corpus set and add a fail-closed bench loader
that proves all four corpus files resolve with declared byte counts and hashes.

W3 does not admit any CSS L4 row. It only closes R5 and prepares W4/W8.

## Owner Paths

Authorized redress paths:

- `skinny/corpora/css-l4-sk-v14/`
- `skinny/corpora/css-l4-sk-v14/manifest.md`
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs`
- `skinny/crates/bbnf-bench/src/lib.rs`
- `restart/skinny/tranches/sk-v14/research/skv14-W3-close.md`
- `restart/skinny/tranches/sk-v14/HANDOFF.md`

Forbidden paths:

- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `skinny/REDRESS.md` unless W3 rejects
- `crates/core/src/runtime/css_l4/`
- generated CSS runtime output and codegen providers

## Corpus Selection

Stage these exact files:

| Family | Version pin | File | Source URL | Bytes |
|---|---|---|---|---:|
| Bootstrap | `bootstrap@5.3.3` | `bootstrap-5.3.3.min.css` | `https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css` | 232803 |
| Tailwind CSS | `tailwindcss@0.2.0` | `tailwindcss-0.2.0.min.css` | `https://cdn.jsdelivr.net/npm/tailwindcss@0.2.0/dist/tailwind.min.css` | 179631 |
| Material Components Web | `material-components-web@14.0.0` | `material-components-web-14.0.0.min.css` | `https://cdn.jsdelivr.net/npm/material-components-web@14.0.0/dist/material-components-web.min.css` | 495454 |
| Animate.css | `animate.css@4.1.1` | `animate-4.1.1.min.css` | `https://cdn.jsdelivr.net/npm/animate.css@4.1.1/animate.min.css` | 71750 |

Target total: 979638 bytes.

## Falsifiability Gate

- `du -sh skinny/corpora/css-l4-sk-v14` reports at least 800 KB.
- `manifest.md` cites all four source URLs, version pins, byte counts,
  SHA-256 hashes, freshness stamps, and licenses.
- `wc -c skinny/corpora/css-l4-sk-v14/*.css` totals 979638 bytes.
- `shasum -a 256 skinny/corpora/css-l4-sk-v14/*.css` matches manifest values.
- `cargo test -p bbnf-bench css_l4_sk_v14_corpora_resolve_and_match_manifest -- --nocapture` passes.
- `cargo xtask gate-json --check-results --skv14-existing-results-capture` passes.
- Invariants remain: 16 locks and 67 Pattern H runtime files.

## Hard Cap

Redress cap: 90 minutes. Commit on gate pass. Revert and record REDRESS if the
corpus floor, provenance, or loader check cannot pass.

## Revert Protocol

If any W3 gate fails, delete `skinny/corpora/css-l4-sk-v14/`, revert
`css_l4_corpus.rs` and `lib.rs`, preserve the research/plan artefacts, and add
a `skinny/REDRESS.md` entry naming the failed floor/provenance/loader check.
W4 remains blocked.

## Same-Wave Consumer

`bbnf-bench::css_l4_corpus::load_all()` is the same-wave consumer. Its unit
test resolves every staged corpus file at runtime and validates all declared
byte counts and SHA-256 hashes.

## Pre-Blocked Routes

- No tiny research fixtures as production corpus.
- No synthetic padding.
- No CSS L4 SOTA or row admit from W3.
- No byte-equality fixture short-circuit.
- No root runtime or generated-output edits.
