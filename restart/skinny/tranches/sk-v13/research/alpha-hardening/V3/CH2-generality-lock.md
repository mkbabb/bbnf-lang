# SK-V13 Alpha Hardening V3 CH2 - Generality / Lock

Verdict: **ACCEPT**.

Scope: CH2 rechecks the V3 Alpha packet after the fold for Lock 14 and Lock 16
generality, no SPEC-local authorization of public trait/directive/BIR/
`BackendShape`/public substrate surfaces, G-Omega pre-W0 blocking,
decision-engine fallback fail-closed behavior, and the new
`G-SIMD-GRAMMAR-POLICY` gate.

## Findings

1. **ACCEPT - SPEC-local public-surface authorization remains blocked.**
   Alpha-E keeps the V2 correction: any public trait, new directive, new BIR
   variant, new `BackendShape`, public `UnionTape`-style substrate, or
   grammar-specific generic behavior is `REJECT`; S-P3 may narrow owner paths
   and gates only, and only a user re-pin outside SPEC can change scope
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:176-179`).
   The top-level contract repeats that no SPEC clause may authorize a new
   directive, BIR variant, `BackendShape`, public substrate API, or
   grammar-specific generic behavior
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:223-225`). The handoff makes
   the same condition an explicit downstream refusal trigger
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:153-154`).

2. **ACCEPT - Lock 14 is still a consumed generality gate, not a blanket
   GrammarConfig pass.** E2 is framed as per-grammar value/config/sink expansion
   without a public `GrammarConfig` trait or substrate API, with a same-wave
   generated grammar consumer requirement
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:138-162`).
   Its gate requires strict CSS parity, JSON guard preservation, and no generic
   branch on grammar name, corpus name, JSON object/array role, field name,
   string role, or layout role
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:172-175`).
   Alpha-D also preserves the correct caveat: W1a GrammarConfig/config-module
   work is valid but partial, and any claim that GrammarConfig/Lock 14 is fully
   solved is invalid
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md:300-314`).

3. **ACCEPT - G-Omega remains a hard pre-W0 gate for Lock 14 and Lock 16.**
   SYNTHESIS requires Totality V1.1 and G-Omega before Wave 0, including
   GrammarConfig/Lock 14 evidence, REDRESS-119/120/121-127 lessons, Lock 16
   SIMD/checkasm discipline, non-JSON telemetry schema, and zero-orphan
   evidence
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:112-122`). HANDOFF says no
   implementation Wave 0, source edit wave, or RESULTS/REDRESS-writing wave may
   proceed before G-Omega, and lists REDRESS-121 Lock 14 evidence and
   REDRESS-122 Lock 16 escape-mask prerequisite in the required Omega fold
   surface
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:54-71`,
   `restart/skinny/tranches/sk-v13/HANDOFF.md:78-88`).

4. **ACCEPT - decision-engine fallback fails closed.** Alpha-E states that
   after the resolver wave, the hardcoded P1-P8 cascade is not an admissible
   production fallback for JSON, CSS, Sheets, or BBNF-self rows, and any
   retained compatibility path must fail closed with visible row rejection or
   non-admission
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:247-254`).
   SYNTHESIS and HANDOFF repeat the same rule as contract/refusal language, so
   silent fallback to the old cascade is not admission evidence
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:232-235`,
   `restart/skinny/tranches/sk-v13/HANDOFF.md:155-157`).

5. **ACCEPT - V3 closes the SIMD grammar-policy gap.** Alpha-E now makes
   `G-SIMD-GRAMMAR-POLICY` a prerequisite for any wave that wires `bbnf-simd`
   into CSS, union, JSON `parse_only`, or shared generated code. The gate
   requires the selected classifier to use the consuming grammar's
   quote/escape/control policy or a no-string policy; scalar parity and
   checkasm/differential coverage for JSON policy, CSS identifier/string
   policy, and delimiter-only/no-string policy; same-wave measured row
   consumption; no public substrate API or public `GrammarConfig` trait; and no
   retained sidecar classifier state
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:417-428`).
   SYNTHESIS carries the same requirement into S-P3 constraints and pre-blocks
   non-JSON/shared consumers of alphabet-only SIMD classifier dispatch unless
   the gate proves JSON quote/escape/control constants cannot leak
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:226-231`,
   `restart/skinny/tranches/sk-v13/SYNTHESIS.md:247-250`). HANDOFF makes the
   same missing gate a refusal condition
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:156-161`).

## Residual Watch

Older scoping files still contain illustrative or aggressive public
`GrammarConfig` sketches and public-trait options
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:218-238`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:547-551`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:601`).
This is not a V3 blocker because SYNTHESIS, HANDOFF, and Alpha-E override weaker
scoping prose and explicitly reject SPEC-local authorization of those surfaces.
S-P3 should cite only the practical `pub(crate)` generated-module route.

## Required Fixes

None for CH2. The V3 packet is acceptable for the generality/Lock challenge:
Lock 14 and Lock 16 carry through G-Omega, public-surface loopholes are closed,
decision-engine fallback fails closed, and `G-SIMD-GRAMMAR-POLICY` closes the
V2 SIMD alphabet-policy gap.
