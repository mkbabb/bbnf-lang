# SK-V14 W3-A: Spec Obligations

Date: 2026-05-26.
Scope: W3 entry and exit gates from the SK-V14 SPEC.
Output: this file.

## Section 1 - Findings

W3 is an infrastructure wave, not an admit wave. `SPEC.md` Section 6 binds W3
to `skinny/corpora/css-l4-sk-v14/`, `manifest.md`, and
`skinny/crates/bbnf-bench/src/css_l4_corpus.rs`.

Entry is satisfied by amended W2 close at `45568e669` and close packet
`skv14-W2R-close.md`: W2 admitted skinny-only `regen-css`, and root
`crates/core/src/runtime/css_l4/` remains W6.0.

Exit requires four production CSS sources named as Bootstrap, Tailwind,
Material, and Animate; stable source URLs plus version or commit pins; total
working set at least 800 KB; per-corpus size, checksum, and freshness telemetry;
and runtime loader resolution for all four files.

## Section 2 - Recommendations

Name W3 gates explicitly:

- `G-W3-SOURCE-FOUR`: exactly four named production source families.
- `G-W3-SIZE-FLOOR`: `du -sk skinny/corpora/css-l4-sk-v14` >= 800.
- `G-W3-PROVENANCE`: manifest carries URL, version pin, bytes, SHA-256, HTTP
  freshness stamp, and license.
- `G-W3-LOADER`: `bbnf-bench::css_l4_corpus::load_all()` resolves all files and
  validates sizes/checksums in tests.

## Section 3 - Risks

Do not treat W3 corpora as CSS admission evidence. W8 owns CSS L4 readmit after
W2 through W7 close. Do not inflate the directory past the approximate 960 KB
target with synthetic padding. Do not add byte-equality short-circuits like
`CANONICAL_FIXTURE` or `CAPTURED_W2_INPUT`.

## Section 4 - Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 6.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` R5 and P-3.
- `restart/skinny/tranches/sk-v14/research/skv14-W2R-close.md`.
