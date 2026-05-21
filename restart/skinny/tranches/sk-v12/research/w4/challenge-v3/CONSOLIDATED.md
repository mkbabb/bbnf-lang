# SK-V12 W4 CHALLENGE V3 - Consolidated Disposition

Verdict: REVISE.

Accepted lenses: CH1 correctness, CH2 generality/Lock 14, CH5 hidden coupling,
CH6 anti-paper-close.

Rejected lenses: CH3 regression/REDRESS, CH4 cost.

PLAN-V3 makes the correct architectural move: split the cheap delimiter
member-find microbench from any production CSS wiring. That removes the hidden
production branch and gives W4 a credible way to record a measured ASM-gen
attempt without creating a new orphan. PLAN-V4 is still required before redress
because the default branch's evidence contract and command surface remain
ambiguous.

## Required PLAN-V4 Changes

1. Define the default branch as a pre-production microbench-only measured
   reject branch. It may record an ASM-gen attempt for REDRESS/FIXPOINT
   evidence, but it must not claim a same-wave production consumer or strict
   fact-stream equality. Those requirements move to the rare production split
   after a passing microbench.

2. Normalize orphan close vocabulary. Every orphan row's `final_disposition`
   must be `consumed`, `removed`, or `inventory_demoted_with_evidence`.
   Implementation facts such as `production_reachable_scalar_delegate` remain
   evidence details, not final dispositions.

3. Trim default verification to touched paths and write root-executable
   commands. The default branch should run the new caller checkasm/microbench
   with `cargo --manifest-path skinny/Cargo.toml ...`, plus a JSON/report/gate
   no-touch proof. It should not run Lock 14, full JSON gate, or unrelated
   release checkasm unless those roots move.

4. Name the microbench artefact producer, including the environment variable
   that writes
   `restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json`.

5. Add a hard LOC cap for the new caller test/microbench writer. The cap should
   be small enough to prevent the default branch from silently growing into a
   production implementation.

If PLAN-V4 applies these changes, the delimiter member-find route remains
eligible for redress under the default measured-reject branch or, on an
unexpected microbench pass, a routed production split.
