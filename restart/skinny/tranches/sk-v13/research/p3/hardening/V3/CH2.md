# SK-V13 S-P3 V3 CH2 Generality / Lock 14

Pass: S-P3 Synthesis-Plan.
Cycle: V3 CHALLENGE.
Lens: CH2 Generality / Lock 14.
Commit under review: HEAD `eb80510167464d30f5d0cf55ac2c80c60d0445d1`; S-P3 folded packet `9f8bbfce5`; prior accepted S-P3 V2 packet `b5f58b755`.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/V3/CH2.md`.

## Verdict

ACCEPT.

The S-P3 CH2 fold remains sound at HEAD. The live S-P3 authority still requires
CSS L4 plus both Sheets and BBNF-self witnesses before any fleet-wide
generic-crate grammar-neutral claim, scopes CSS-plus-one witness to named
grammars, keeps JSON/CSS policy out of generic crates, and treats `grammar_id`
as telemetry only.

## Compact Evidence

- CH2 authority is Lock 14: no grammar-name leak and interventions must work
  for CSS L4, Sheets, and BBNF-self, not only JSON
  (`restart/prompts/ORCHESTRATOR.md:81`-`:85`); no JSON code in generic crates
  is a CH2 enforcement rule (`restart/prompts/ORCHESTRATOR.md:197`-`:205`).
- PASS-3 requires the SPEC Section 2.1 generality gate and non-JSON proof for
  every generic-crate edit; any wave that lets JSON policy into a generic crate
  fails CH2 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116`-`:120`).
- SPEC Section 2.1 gates public API, grammar-branch, primitive/table, and
  template/provider boundaries, then requires strict CSS L4 plus both Sheets
  and BBNF-self witnesses for fleet-wide claims; CSS L4 plus only one of those
  witnesses is scoped and cannot use fleet-wide/universal/grammar-neutral
  closure wording (`restart/skinny/tranches/sk-v13/SPEC.md:370`-`:390`).
- DISPATCH repeats the two-witness packet requirement
  (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:65`-`:78`) and the Lock
  14 rule barring generic branches on grammar/corpus/CSS/JSON roles while
  routing grammar-specific behavior through generated modules, tables,
  templates, or opaque facts (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:133`-`:146`).
- P3-C binds decision-engine generic edits to the same two-witness rule
  (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:240`-`:244`);
  P3-E binds REDRESS 121 gate feed to no grammar-name control flow and the same
  CSS L4 plus Sheets plus BBNF-self fleet-wide closure rule
  (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:105`-`:111`).
- P3-D defines `grammar_id` values as telemetry keys only and forbids generic
  crate behavior from branching on them
  (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:120`-`:125`);
  SPEC carries `grammar_id`, `lock14_status`, and `lock16_status` as required
  telemetry/gate fields (`restart/skinny/tranches/sk-v13/SPEC.md:133`-`:185`).
- The V2 consolidated hardening packet accepted CH2 with the same load-bearing
  finding: fleet-wide generic claims require CSS L4 plus both Sheets and
  BBNF-self, and one-witness claims stay row-scoped
  (`restart/skinny/tranches/sk-v13/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:18`-`:31`).

## Required Fold Items

None for CH2.

## Verification

- `git diff --name-status b5f58b755..HEAD -- restart/skinny/tranches/sk-v13 restart/prompts/ORCHESTRATOR.md restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` returned no output, so the post-V2 HEAD movement did not change S-P3 or CH2 authority files.
- `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V3/CH2.md` passes.
