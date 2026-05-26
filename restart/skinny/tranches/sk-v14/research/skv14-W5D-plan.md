# SK-V14 W5D-DELETE Plan

Date: 2026-05-26.

Disposition: PLAN-ADMIT for redress.

## Scope

W5D deletes old provider/template residue after W5C-GEN made the runtime
generator body load-bearing. It does not replace or delete `json_templates/`
because W5C did not prove the JSON template dependency retired.

## Sequence

1. Route-first Lock 14 patch.
   - Edit `skinny/crates/bbnf-bench/src/lock14_baseline.rs` only.
   - Add W5D owner paths and subject routing.
   - Add focused tests proving W5D admits only deletion-owned paths and W5C
     still rejects deletion.
   - Keep provider/template topology expectations at the pre-delete values.

2. Provider/template deletion.
   - Delete the eight provider modules and seven CSS L4 template directories
     listed in `skv14-W5D-research.md`.
   - Update the Lock 14 topology gate to the post-W5 shape:
     provider count `0`; CSS template directory count `0`;
     `json_templates/` retained while production-consumed.
   - Add tests for the post-W5 topology helpers.

3. Verification and close.
   - Run the W5D exact Lock 14 tests.
   - Run provider/template counts and provider reachability greps.
   - Run `regen-css`, all seven CSS companion checks, `check-json`, and
     `gate-json --check-results --skv14-existing-results-capture`.
   - Record W5D close packet and route W6.0.

## Constraints

- No compatibility alias period.
- No generated output hand patching.
- No JSON template deletion in W5D.
- No W6 root-runtime collapse before W5D admits.
