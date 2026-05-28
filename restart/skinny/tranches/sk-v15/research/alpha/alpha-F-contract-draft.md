# Alpha-F — Contract Draft — SK-V15 V1

Pass: Pass Alpha. Cycle: SK-V14 -> SK-V15.
Date: 2026-05-27.
Scope: `SYNTHESIS.md` + `HANDOFF.md` draft basis.
Output: this file.

## Draft Close Condition

SK-V15 closes only when all of these hold:

1. JSON 51/51 remains admitted with no regression from the SK-V14 close
   (`skinny/RESULTS.md:139`).
2. CSS L4 is either honestly collapsed to one aggregate diagnostic row or
   independently measured per feature; no 24-row broadcast remains.
3. `CSS_GENERATED_RS` and `CssFullParseSummary` are retired from the live
   generated CSS parser path.
4. CSS exposes typed value/document/view/visitor surfaces with parity to
   the JSON Value API.
5. CSS admission compares same-workload typed output against cssparser and
   lightningcss only after CSSOM parity.
6. Admission and SIMD claims are native Apple M5 Max / aarch64 only; x86
   and AVX-512 rows are diagnostics, not anchors.
7. Lock 14 and Lock 16 scan the full generic surface and report exclusions as
   findings rather than hiding them; self-exempting grep/checkasm gates reject
   close.
8. Pattern H count remains 67, but all 67 files carry generated provenance
   and generator-owned round-trip proof.
9. Decision Engine is load-bearing: at least one e-graph rewrite, non-
   tautological CSP, grammar-neutral provider facts, and real lowerings for
   all five BackendShape paths.
10. W11L/W11N/W11O FNV closed-enum products remain bench-only and are
   guarded from production runtime migration.
11. Codegen neutrality forbids per-grammar regen enum/match fanout,
    JSON/CSS runtime mode splits, CSS profile control matches, and generic
    pass JSON-byte recognizers.
12. Every close row cites HEAD command output, generated artefacts or diffs
    where relevant, and cold per-parse evidence; documentation-only close is
    rejected.
13. PASS-IMPL V2 returns ACCEPT on every axis or documents a row-level
    intrinsic-block proof.

## Telemetry Binding

SK-V15 inherits `PASS-ALPHA.md:77` telemetry and adds CSS anti-broadcast
columns: `measurement_row_id`, `measurement_origin`, `value_plane`,
`css_comparator_workload`, `generator_source`, `lock14_scan_scope`, and
`lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and
`broadcast_group_id`. The successor gate must reject missing gate-exclusion
telemetry and self-exempting reports.

## Receiver List

S-P3 must convert five candidate packages into exact `SPEC.md` waves:

- PRUNE-WAVE-A: CSS contrivance retirement.
- PRUNE-WAVE-B: Lock 14 / Lock 16 gate coverage plus exclusion findings.
- PRUNE-WAVE-C: codegen leak abrogation.
- PRUNE-WAVE-D: Pattern H discipline.
- REBUILD-WAVE-E: CSS Value API.
- REBUILD-WAVE-F: Decision Engine activation.
- REBUILD-WAVE-G: bench-only FNV quarantine.

Hard caps are inherited by every wave or sub-wave: research <=20 minutes,
plan <=15 minutes, redress <=30 minutes; commit at 0.9N and halt at N. S-P3
must also emit a dependency table for every retired/deleted artefact:
artefact, delete/retire wave, rebuild provider, proof command, and proof
that the provider lands no later than the delete/retire wave. CSS parser
retirement is coupled to typed CSS value proof or blocked.

## Gate Posture

`PASS-ALPHA.md` and `ORCHESTRATOR.md` still describe G-Alpha as mandatory
(`restart/prompts/pass-contracts/PASS-ALPHA.md:167`,
`restart/prompts/pass-contracts/PASS-ALPHA.md:203`,
`restart/prompts/ORCHESTRATOR.md:167`). The controlling handoff pin says
only G-Omega is mandatory and all other gates auto-pass
(`restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md:11`). SK-V15 records
the conflict explicitly: G-Alpha auto-passes only under the explicit user
pin; without that pin, G-Alpha would block.

## Next Move

After this Alpha packet hardens, S-P0 consumes PASS-IMPL V1, then S-P1,
S-P2, S-P3, totality T-P1/T-P2/T-P3, Pass Omega V5, and wave execution
proceed. G-Omega remains the only mandatory user stop.
