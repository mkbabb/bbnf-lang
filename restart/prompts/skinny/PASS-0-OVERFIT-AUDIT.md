# Pass S-P0 — Overfit Audit (skinny track)

Date authored: 2026-05-22.
Authority: USER directive 2026-05-22, formalized after the SK-V13
overfit audit at `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`.

S-P0 is the FIRST pass in every skinny tranche. It runs BEFORE the
Pass Alpha bracket from the prior tranche has its §0 goalset
finalized — Pass Alpha must consume S-P0's prune list.

## Why this pass exists

SK-V13 closed with 24 CSS L4 ADMITTED rows and 5 JSON parse_only
ADMITTED rows whose admissions did not survive an integrity audit:
the CSS "generators" were hand-written templates included via
`include_str!()`; the JSON parse_only admits changed only gate
infrastructure and used a misnamed comparator. The pre-restart pattern
Lock 14 was authored to prevent had recurred at category scale (64
hand-written per-grammar runtime files in `crates/core/src/runtime/`).

S-P0 forces every tranche to audit the prior tranche's outputs + the
current codebase BEFORE adding new waves. The campaign cannot move
forward on top of fake admits.

## Scope

S-P0 is bound to the **skinny** track. (The totality track has its own
T-P0 analogue — to be authored separately if needed; for now Pass
Omega + Lock 14 enforcement handles totality overfit, but escalating
findings may demand a T-P0.)

S-P0 covers six audit axes, each owned by one sub-agent:

| Axis | What it audits | Pass criterion |
|---|---|---|
| **A1 Measurement integrity** | Corpora sizes, identical-number clusters, throughput plausibility vs SOTA, comparator same-run discipline. | Every ADMITTED row in the prior tranche's ROLLING-SOTA-DELTA has (a) representative corpus ≥ 1 KB, (b) per-row distinct measurement, (c) plausible Mbps relative to byte-throughput ceiling. |
| **A2 Admit-mechanism integrity** | For every admitted row, the source diff that achieved the admission, the comparator binding, the per-iteration equality oracle. | No admit lands by gate-relabel; every admit cites a parser/codegen/SIMD source change; comparator is strict-vs-strict on the same plane. |
| **A3 Lock 14 generic-crate scan** | Grammar-name leaks (string literals, byte literals, function/struct names, enum match arms) in nominally-generic crates. | Zero CRITICAL or HIGH violations in skinny generic crates. |
| **A4 Generator-vs-hand-curated** | For every generated module: round-trip test (delete + regen produces byte-equivalent). For every claimed grammar-derived parser: locate the grammar source + emission command. | 100% round-trip pass; every "generated.rs" comes from a real `cargo xtask regen` command + a real `.bbnf` grammar source. |
| **A5 Decision-engine fold integrity** | The CSP+egraph+cost resolver wired into compile; per-grammar policy + union substrate wired to actual runtime. | The resolver drives emission; no scaffold-only wave admits a row. |
| **A6 Pre-restart pattern recurrence** | Hardcoded grammars, hand-coded "generated" code, hand-written per-grammar runtime modules, renamed pre-restart code, backend-specific code in shared crates, combinator/monolithic mixes, backwards-compat shims. | Zero CRITICAL Pattern H violations; every other pattern reads CLEAN. |

## Procedure

S-P0 runs as a 6-agent fan-out, each agent owning one axis (A1-A6).
Each agent:

- Reads the prior tranche's authority docs + the current codebase.
- Produces ONE artefact at `restart/skinny/tranches/sk-v{N}/audit-overfit/sk-v{N}-audit-overfit-{axis}.md`.
- Commits with subject `docs(sk-v{N}-audit-overfit): {axis}`.
- Hard cap 25 min. At 22 commit, at 25 halt.

After the 6 agents converge:

1. Orchestrator authors `SYNTHESIS-AUDIT-OVERFIT.md` in the same dir,
   consolidating the per-axis findings into a single prune list.
2. CH1-CH6 challenge applied to each axis's findings + the synthesis.
3. §3Z convergence under the orchestrator (≥95% across CH1-CH7 for
   two consecutive cycles where CH7 is the new lens — see below).
4. The prune list feeds Pass Alpha's §0 goalset for SK-V{N+1}.

## CH7 — Overfit-Prune lens (new)

In addition to the standing CH1 Correctness, CH2 Generality, CH3
Regression, CH4 Cost, CH5 Hidden Coupling, CH6 Anti-Paper-Close, every
plan and redress is now lensed against:

### CH7 Overfit-Prune

The plan/redress shows that:

- Every new code added is grammar-derived (template + grammar metadata
  + emission command) — never hand-written under a `// @generated`
  header.
- Lock 14 generic-crate compliance is preserved (no JSON/CSS/Sheets
  string literals, byte literals, function names, enum variants, or
  match arms in nominally-generic code).
- Every admit lands via a real parser/codegen/SIMD source change,
  measured against a strict-vs-strict comparator on the same plane,
  with a per-iteration equality oracle.
- Every "generated" output passes a round-trip test (delete + regen ⇒
  byte-equivalent).
- No SCAFFOLD-ONLY landing (research artefacts without source wiring)
  counts as an admit.

CH7 REJECT triggers immediate plan revise OR redress revert with
REDRESS entry. CH7 cannot be carried as "acknowledged but not blocking".

## Inputs (binding read list)

Every S-P0 sub-agent reads:

- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- The prior tranche's `CAMPAIGN-CLOSE-SK-V{N-1}-V{N-1}.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/locks/LOCKS.md` (especially Lock 14 enforcement clause)
- `restart/prompts/ORCHESTRATOR.md`
- This file
- The prior tranche's complete research/ directory

Pass Alpha for the new tranche cannot finalize §0 goalset until S-P0
has converged.

## Outputs

Per tranche:

- 6 per-axis audit artefacts under
  `restart/skinny/tranches/sk-v{N}/audit-overfit/`
- 1 synthesis: `SYNTHESIS-AUDIT-OVERFIT.md` consolidating prune list.
- Prune-wave entries seeded into the SK-V{N} SPEC's wave manifest
  (Pass Alpha incorporates these).
- A CH7 lens directive carried into every wave's CHALLENGE phase.

## Hard caps

- Per sub-agent: 25 min research, 25 min total wall.
- Synthesis: 30 min.
- Total S-P0 wall: ~60 min concurrent (6 agents in parallel).

## When S-P0 may be skipped

NEVER. S-P0 runs every tranche. If the prior tranche closed CLEAN
(zero CRITICAL or HIGH findings across A1-A6), S-P0 still runs to
verify the cleanliness held; that run is fast (≤30 min total).

## Failure mode

If S-P0 finds CRITICAL violations the campaign halts forward motion
until the prune waves complete. The tranche's behavior waves do not
dispatch until the prune list converges. This is the inverse of
SK-V13's pattern (build first, audit never).

## Standing SK process loop (post-2026-05-22)

  Pass Alpha bracket SK-V{N-1} → SK-V{N}
    └── consumes S-P0 prune list from prior tranche end OR runs S-P0 first
  S-P0 Overfit Audit (NEW)              ← this pass
  S-P1 Profile
  S-P2 Research (6 cohorts; CH1-CH7 lensed)
  S-P3 Synthesis-Plan (wave manifest; CH7 lens binding)
  Waves W0..Wn (each CH1-CH7 lensed at plan + redress)
  Pass Omega V1.{X} → V1.{X+1} (concurrent; doc-only)
  Pass Alpha close → bracket SK-V{N+1}
    └── continue indefatigably per addendum

Pass Omega's totality fold work proceeds concurrent with the skinny
loop. SK-V14 onward executes this loop with S-P0 binding every
tranche.
