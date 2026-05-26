# SK-V14 W5B-GEN CHALLENGE V1 CH4 Cost

Date: 2026-05-26.
Lens: CH4 Cost.
Disposition: REVISE.

## Findings

The W5B-GEN rejection under the current <=1.0k cap is justified. SPEC §8B
requires one provider-free source/metadata generator, forbids providers,
templates, generated-output-as-source, and borrowing W5C/W6 budget. Current CSS
L4 cannot flow through the skinny parser, and the live path still reaches
`RuntimeProvider` and provider/template code. LOC evidence supports the cost
call: CSS template dirs alone are 4,740 LOC; all template dirs are 5,889 LOC,
before real frontend/import/IR work.

The V7 corrective route needs explicit cap handling. The plan and corrective
packet define W5B-FRONTEND/W5C-GEN/W5D-DELETE scopes and label risk HIGH, but
they do not assign per-wave LOC/time caps or generated-output accounting.

## Required Fold

- Keep the W5B-GEN rejection.
- Add explicit LOC and 90-minute redress caps for W5B-FRONTEND, W5C-GEN, and
  W5D-DELETE in the proposed V7 route.
- Add a clause that any frontend/import/IR slice exceeding that envelope must
  split again before dispatch.
- State generated-output accounting for the future route: generated files are
  uncounted only when produced by fresh regen through the active generator and
  diff-audited.

## Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:725`
- `restart/skinny/tranches/sk-v14/SPEC.md:746`
- `restart/skinny/tranches/sk-v14/SPEC.md:752`
- `restart/skinny/tranches/sk-v14/SPEC.md:253`
- `restart/skinny/tranches/sk-v14/SPEC.md:276`
- `skinny/crates/grammar/src/lib.rs:320`
- `skinny/crates/codegen/src/grammar_provider.rs:42`
- `skinny/crates/codegen/src/lib.rs:180`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:94`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:50`
