# SK-V13 Alpha Hardening V4 CH2 - Generality / Lock

Verdict: **ACCEPT**.

Scope: V4 re-runs CH2 against the current SK-V13 Alpha packet with V3
CONSOLIDATED as context. This check confirms that Lock 14 and Lock 16 remain
sound, SPEC-local text still cannot authorize public trait/directive/BIR/
`BackendShape`/public substrate surfaces, G-Omega remains pre-W0, the
decision-engine fallback remains fail-closed, and `G-SIMD-GRAMMAR-POLICY`
remains binding.

## Findings

1. **ACCEPT - V3 CH2 remains a valid baseline.** V3 consolidated six-of-six
   ACCEPT and specifically recorded CH2 as coherent for Lock 14/16,
   SPEC-local public-surface blocking, G-Omega pre-W0, decision-engine
   fail-closed behavior, and `G-SIMD-GRAMMAR-POLICY`
   (`restart/skinny/tranches/sk-v13/research/alpha-hardening/V3/CONSOLIDATED.md`).
   No current packet file weakens those clauses.

2. **ACCEPT - Lock 14 remains partial but sound.** The packet continues to
   treat GrammarConfig/config-module work as a validated partial bridge, not a
   completed generality proof. Alpha-D explicitly invalidates any claim that
   GrammarConfig or Lock 14 is fully solved, while Alpha-E requires every
   per-grammar policy expansion to be consumed by a generated grammar row in
   the same wave and forbids generic branches on grammar/corpus/JSON role,
   field, string role, or layout role
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md`,
   `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md`).
   This is the correct Lock 14 posture: legal route, consumed evidence, no
   blanket public abstraction.

3. **ACCEPT - Lock 16 remains prerequisite-gated, not blanket SIMD admission.**
   Alpha-D carries the `escape_mask_64` fix only as a resolved correctness
   prerequisite; new SIMD/ASM waves still require scalar reference,
   checkasm/differential parity, corpus/equality proof, and same-wave
   production consumers. Alpha-E repeats that every new SIMD kernel must move a
   measured row or be rejected, with zero aarch64 orphans preserved
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-D-validated-invalidated.md`,
   `restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md`).

4. **ACCEPT - SPEC-local public-surface authorization remains closed.** The
   current contract keeps the V3 correction: no SPEC clause may authorize a new
   directive, BIR variant, `BackendShape`, public substrate API, or
   grammar-specific generic behavior. Alpha-E additionally rejects any public
   trait, public `UnionTape`-style substrate, public `GrammarConfig` trait, or
   same-purpose public substrate escape unless the user re-pins scope outside
   SPEC. Older scoping sketches therefore remain non-binding examples, not
   authority.

5. **ACCEPT - G-Omega remains hard pre-W0.** SYNTHESIS requires Totality V1.1
   and G-Omega before SK-V13 Wave 0, and HANDOFF blocks implementation waves,
   source edits, generated runtime changes, gate/report code changes, and
   RESULTS/REDRESS writes until G-Omega closes. The required Omega fold surface
   still includes Lock 14 GrammarConfig evidence, Lock 16 escape-mask/SIMD
   discipline, non-JSON telemetry, and zero-orphan evidence
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md`,
   `restart/skinny/tranches/sk-v13/HANDOFF.md`).

6. **ACCEPT - decision-engine fallback remains fail-closed.** Alpha-E,
   SYNTHESIS, and HANDOFF all say that after the resolver wave the hardcoded
   P1-P8 cascade is not an admissible production fallback for JSON, CSS,
   Sheets, or BBNF-self rows. Any retained compatibility path must visibly
   reject or record non-admission; silent fallback cannot count as admission
   evidence.

7. **ACCEPT - `G-SIMD-GRAMMAR-POLICY` remains sound and binding.** Alpha-E
   requires this gate before wiring `bbnf-simd` into CSS, union, JSON
   `parse_only`, or shared generated code. The gate still requires the selected
   classifier to use the consuming grammar's quote/escape/control policy or a
   no-string policy; scalar parity and checkasm/differential coverage for JSON,
   CSS, and delimiter-only/no-string policies; same-wave measured row
   consumption; no public substrate API or public `GrammarConfig` trait; and no
   retained sidecar classifier state. SYNTHESIS and HANDOFF carry the same
   refusal condition, so the V2 alphabet-only SIMD leak remains closed.

## Residual Watch

The same residual watch from V3 still applies: older scoping files contain
aggressive or illustrative public `GrammarConfig`/public-trait sketches. They
remain subordinate to SYNTHESIS, HANDOFF, Alpha-D, and Alpha-E. S-P3 should cite
only codegen-private, consumed, `pub(crate)`-style routes and should not promote
those sketches into SPEC authority.

## Required Fixes

None for CH2.

V4 confirms the V3 CH2 ACCEPT result. Lock 14 and Lock 16 remain sound under
the packet's consumed-evidence gates; SPEC-local public-surface authorization is
closed; G-Omega remains pre-W0; decision-engine fallback fails closed; and
`G-SIMD-GRAMMAR-POLICY` remains a required gate for shared or non-JSON SIMD
consumers.
