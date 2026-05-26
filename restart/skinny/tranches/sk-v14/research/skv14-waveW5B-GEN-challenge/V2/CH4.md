# SK-V14 W5B-GEN CHALLENGE V2 CH4 Cost

Date: 2026-05-26.
Lens: CH4 Cost.
Disposition: ACCEPT.

## Findings

The V2 fold satisfies CH4. V1 asked for explicit LOC/time caps,
generated-output accounting, and a split-again clause. The folded plan assigns
W5B-FRONTEND <=1.0k source/test LOC, W5C-GEN <=1.0k source/test LOC, and
W5D-DELETE <=400 source/test LOC, each with a <=90 minute redress ceiling. It
requires another split before dispatch if the frontend/import/IR slice cannot
fit. The corrective packet mirrors those caps.

Generated-output accounting is explicit: generated output is uncounted only
when produced by fresh regen through the active generator and diff-audited.

The W5B-GEN rejection under <=1.0k remains justified. SPEC requires
provider-free CSS/JSON generation from grammar source plus metadata and forbids
providers/templates, generated-output-as-source, grammar-name branches, and
W5C/W6 borrowing. Current parser accepts only `@import` and `@token`, while
live runtime emission still delegates to provider/template routes. Template LOC
alone is 5,889 total and 4,740 CSS-only before frontend/import/IR work.

## Sources

- `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-GEN-challenge/V1/CH4.md:23`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:127`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:131`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:64`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:68`
- `restart/skinny/tranches/sk-v14/SPEC.md:725`
- `restart/skinny/tranches/sk-v14/SPEC.md:746`
- `restart/skinny/tranches/sk-v14/SPEC.md:752`
- `skinny/crates/grammar/src/lib.rs:320`
- `skinny/crates/codegen/src/grammar_provider.rs:77`
- `skinny/crates/codegen/src/lib.rs:180`
- `skinny/crates/codegen/src/lib.rs:233`
