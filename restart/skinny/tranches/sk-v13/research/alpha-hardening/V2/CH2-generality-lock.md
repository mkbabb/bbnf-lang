# SK-V13 Alpha Hardening V2 CH2 - Generality / Lock

Verdict: **ACCEPT**.

Scope: CH2 reviews V2 for Lock 14 grammar generality, Lock 16 carry-through,
single-tape/no-sidecar preservation, no SPEC-local override of forbidden
surfaces, and decision-engine fallback behavior.

## Findings

1. **ACCEPT - V2 removes the SPEC-local override loophole.** Alpha-E now states
   that any public trait, new directive, new BIR variant, new `BackendShape`,
   public `UnionTape`-style substrate, or grammar-specific generic behavior is
   `REJECT`; S-P3 may only narrow owner paths and gates, and only user re-pin
   outside SPEC can change scope
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:176-179`).
   The top-level contract repeats the same rule: no SPEC clause may authorize a
   new directive, BIR variant, `BackendShape`, public substrate API, or
   grammar-specific generic behavior; union is unblocked only for same-tape,
   codegen-private, row-consumed variants
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:224-225`). The handoff makes
   SPEC-local authorization of those surfaces a refusal condition
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:153-154`).

2. **ACCEPT - Lock 14 grammar generality is preserved as a gate, not prose.**
   The decision-engine fold must preserve JSON behavior without introducing
   grammar-specific branches in generic crates
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:61-65`). E2's gate requires no
   generic branch on grammar name, corpus name, JSON object/array role, field
   name, string role, or layout role
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:172-175`).
   The value/config scoping remains compatible because its practical SK-V13
   route avoids public API expansion and uses `pub(crate)` generated
   per-grammar modules instead
   (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:234-238`).

3. **ACCEPT - Lock 14 and Lock 16 constraints carry into G-Omega before W0.**
   `SYNTHESIS.md` requires Totality V1.1 and G-Omega before Wave 0, including
   GrammarConfig/Lock 14 evidence, Lock 16 SIMD/checkasm discipline, non-JSON
   telemetry schema, and zero-orphan evidence
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:112-121`). The handoff's
   G-Omega block names REDRESS-121 Lock 14 evidence, REDRESS-122 Lock 16
   escape-mask prerequisite, non-JSON telemetry, zero-orphan, and
   same-wave-consumer discipline as required fold surface
   (`restart/skinny/tranches/sk-v13/HANDOFF.md:56-71`). The scoping doc still
   marks the Lock 14 per-wave gate and Lock 16 checkasm/escape-mask amendments
   as critical G-Omega fold requirements
   (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md:226-246`).

4. **ACCEPT - No-sidecar and single-tape constraints are explicit enough for
   S-P3.** E4 is limited to legal same-tape union work, with scalar reference
   defined as the current single-tape `OffsetTape`/`EventTape` behavior
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:302-312`).
   Its falsifiability gate rejects public substrate APIs and retained sidecar
   class columns, vectors, lists, or cursors
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:326-329`).
   This preserves the CH5 substrate discipline while still allowing
   codegen-private, row-consumed material differentials.

5. **ACCEPT - Decision-engine fallback now fails closed.** Alpha-E requires
   `choose_backend_shape()` to stop being the live selection path and states
   that, after the resolver wave, the hardcoded P1-P8 cascade is not an
   admissible production fallback for JSON, CSS, Sheets, or BBNF-self rows; any
   retained compatibility path must fail closed with visible row
   rejection/non-admission
   (`restart/skinny/tranches/sk-v13/research/alpha/alpha-E-candidate-shortlist.md:247-254`).
   `SYNTHESIS.md` and `HANDOFF.md` repeat that silent fallback to the old
   cascade is not admission evidence and is a refusal condition
   (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:226-229`,
   `restart/skinny/tranches/sk-v13/HANDOFF.md:155-157`).

## Residual Watch

The value/API scoping still contains an illustrative public `GrammarConfig`
trait sketch before immediately selecting the non-public practical route
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:216-238`).
That is not a V2 blocker because Alpha-E, `SYNTHESIS.md`, and `HANDOFF.md`
override it and forbid SPEC-local public-trait/substrate authorization. S-P3
should cite the practical route only.

## Required Fixes

None for CH2. V2 is acceptable for S-P3 planning after G-Omega, with the
existing hard pre-W0 block intact.
