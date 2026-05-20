# SK-V12 S-P3 Hardening V1 Consolidated

Pass: S-P3 Synthesis-Plan.
Cycle: V1.
Date: 2026-05-20.
Status: REVISE.

## Dispositions

| Lens | Disposition | Blocking findings |
|---|---|---|
| CH1 Correctness | REVISE | Gate-name drift; non-JSON row-id vocabulary drift; W1 threshold weakening; companion telemetry field-name drift. |
| CH2 Generality | ACCEPT | No fold required. |
| CH3 Regression | REVISE | REDRESS 28/33 and REDRESS 70/71 not consistently enumerated in W2/W1 typed-route pre-blocks. |
| CH4 Cost | REVISE | Promoted manifests drop risk and cap structure; W1 fallback can hide multiple redress attempts; W2 proof slices lack cost breakdown. |
| CH5 Hidden Coupling | REVISE | Provider/host-schema wording leaves hidden parser-policy escape hatch; sidecar vocabulary is too broad at exit-gate level. |
| CH6 Anti-Paper-Close | ACCEPT | No fold required. |

ACCEPT rate: 2/6 = 33.3%. No REJECT. No critical defect blocks a V2 fold.

## Required V2 Fold

1. Canonicalize gate names across P3-C, P3-F, SPEC, and DISPATCH:
   `G-W2-SELECTED-NONJSON-INTERVENTION` and
   `G-W3-CONDITIONAL-JSON-COMPANION`.
2. Canonicalize generated non-JSON row ids to the P3-D workload vocabulary:
   `css_l4/declaration_values/{direct_to_struct|real_typed_struct}/main`,
   `sheets/formula/{direct_to_struct|real_typed_struct}/main`, and
   `bbnf_self/grammar/{direct_to_struct|real_typed_struct}/main`.
3. Restore the W1 concrete baseline threshold everywhere: generated Track 1
   `>= 1 Mbps`, independent Track 2/oracle `>= 1 Mbps`, strict equality PASS,
   and `sample_count >= 30` unless S-P3 sets a stricter floor.
4. Make SPEC Section 0.4 quote or exactly align with P3-D's
   `sk-v12-nonjson-generated-v1` companion field vocabulary, including
   `schema_id`, `generated_track1_source_path`, `track1_artifact`,
   `oracle_status`, `baseline_mbps`, `threshold_mbps`,
   `benchmark_artifact_path`, and `gate_status`.
5. Add REDRESS 28/33 to P3-E, P3-B, P3-F, SPEC, and DISPATCH wherever
   bounded-string, string-block, tiny-string, or TBL/NEON active dispatch can
   re-enter.
6. Add REDRESS 70/71 to P3-E, P3-F, SPEC, and DISPATCH as the typed-output
   boundary: typed-equivalent baselines require generated DirectBuild or
   schema-source facts, independent oracle equality, and gate consumption; no
   hand-authored typed sink, direct digest proof, hidden directive/BIR
   extension, or benchmark-private Track 1 parser.
7. Add risk class to the promoted P3-F, SPEC, and DISPATCH wave manifests and
   normalize caps to separate wall cap from redress cap. Redress is `<=75 min`
   for every wave; W0/W4 may carry `<=90 min wall` only for gate/docs overhead.
8. Tighten W1 fallback mechanics: W1 plan-time preflight may evaluate CSS,
   Sheets, and BBNF-self, but redress attempts exactly one selected target. A
   failed selected target records REDRESS BLOCKED/REJECTED and does not fall
   through inside the same redress.
9. Require W2 plans to include a five-part cost table: scalar reference LOC,
   parity/checkasm LOC, microbench LOC, generated consumer LOC, and report/gate
   LOC. If the selected family cannot fit `<=430` non-generated LOC and
   `<=75 min` redress, the plan returns REVISE before source work or S-P3 must
   split a new wave under the bracket ceiling.
10. Tighten provider/host-schema wording: grammar-specific inputs are grammar
    source, workspace metadata, tests, fixtures, independent oracle code, and
    optional per-grammar declaration-crate host functions explicitly named by
    the W1 plan and gate-consumed. Templates are shared grammar-neutral
    generator code; per-grammar providers/templates must not carry handwritten
    parser policy. Host/API schema facts cannot supply parser control,
    generated Track 1 output, or admission shortcut.
11. Expand the sidecar/substrate ban in SPEC, P3-E, P3-F, and DISPATCH to name:
    parser-owned structural projection, retained structural cursor or cursor
    list, aux density table, aux projection column, event side vector,
    whitespace bitmap, retained class lane, structural-position vector,
    decoded-byte sidecar, and renamed scanner retaining facts outside the
    single tape/direct sink contract.

## Accepted Surfaces To Preserve

- The generated non-JSON baseline remains the first material target.
- W2 remains same-row and thresholded against W1.
- W3 remains closed by default and conditional on a material REDRESS 114-119
  reopen gate.
- JSON direct and typed guard floors are correct and must not change.
- The 10-outcome enum remains `A C G I J K L M N-direct S`.
- Companion report evidence must be consumed by a same-wave executable gate.

## Verdict

S-P3 V1 does not converge. Fold the required revisions into V2 and rerun the
six-lens CHALLENGE. The pass may not advance to W0 until two consecutive
cycles meet the `ORCHESTRATOR.md` Section 3Z convergence rule.
