ACCEPT

# SK-V11 S-P1 Hardening V3 CH5 Hidden Coupling

Scope: `restart/prompts/skinny/PASS-1-PROFILE.md` Section 3 CH5,
`restart/prompts/ORCHESTRATOR.md` Sections 3W and 3Z, the folded S-P1
packet P1-A through P1-F at HEAD after `2e988a6a`, W0 baseline,
`skinny/RESULTS.md`, `skinny/REDRESS.md` through REDRESS 110, and the S-P1
hardening V1/V2 consolidations.

## Findings

1. V3 inherits the same CH5 boundary that V1 and V2 accepted. ORCHESTRATOR CH5
   rejects parallel substrates, sidecar producers, renamed scanner violations,
   and Track 1 == Track 2 dishonesty; Section 3Z requires V2 findings to fold
   before V3. The V2 consolidation records only a narrow Lock 14 wording fold,
   says CH5 had no required fold, and states that V3 preserves the V2 capture
   provenance, REDRESS pre-block matrix, row classifications, gate floors,
   RESULTS state, and source/capture artifacts.

2. Track 1 and Track 2 remain separated. P1-B defines `T1` as generated
   Track 1 and `T2` as the independent hand-coded Track 2, then keeps generated
   direct, hand direct, typed, Track 2 parse, and serde leaves as evidence
   members under canonical primitives. P1-E uses the same vocabulary split, and
   `skinny/RESULTS.md` states that Track 1 is
   `runtime::generated_json::parse` while Track 2 is an independent hand-coded
   parser over `runtime::tape` that never calls generated runtime parse.

3. Direct, typed, and oracle evidence stay in distinct product lanes. W0 makes
   `direct_to_struct` the primary closure surface and `real_typed_struct` a
   guard surface. P1-B treats typed Track 2 profiles as serde/oracle evidence,
   P1-E says typed Track 2 leaves are comparator/oracle hot leaves rather than
   generated typed Track 1 behavior, and P1-F extracts direct and typed rows in
   separate tables. No direct digest row is used to prove a typed product row,
   and no typed guard row is used to close a direct residual.

4. W0-clamped rows remain fenced. W0, P1-B, P1-C, P1-E, P1-F, and
   `skinny/RESULTS.md` all keep `instruments`, `numbers`, and `unicode_mixed`
   as `N-direct / NO-GO` planning evidence even when one or both fresh
   throughput numbers clear a computed floor. V3 does not admit any row from W0
   floor math alone.

5. Parse evidence stays diagnostic. P1-A profiles parse-only coverage and says
   no parse row is an SK-V11 SOTA target; P1-C uses parse Mbps only as a probe
   denominator; P1-F folds `canada/parse_only` as `L / NO-GO` and treats `S` as
   a parse-only diagnostic enum. REDRESS 102 firewall-closes parse-only movement
   and prevents parse diagnostics from becoming direct or typed admissions.

6. PMU, cycles, structural scan, masking probes, lazy-tape facts, and sidecar
   freshness stay nonproducer evidence. P1-D supplies real `ri_cycles` and
   `ri_instructions` for parse, direct, and typed guard lanes, but states that
   no PMU row changes `skinny/RESULTS.md`. P1-C reports structural scan below
   the aarch64 floor and marks structural-scan-only, masking, and lazy-tape
   rows as diagnostic. P1-E keeps those signals in separate context and
   pre-block tables, while P1-F records
   `structural_scan+masking_probes+pmu+cycles:nonproducer` as the manifest
   contract. Samply `.json.syms.json` sidecars are symbol maps, not self-time or
   admission authority.

7. REDRESS through 110 keeps the rejected substrate routes rejected. REDRESS 51
   and 53 reject cursor/second-scanner forms, REDRESS 96 and 97 falsify the
   class-column and streaming-cursor W3 implementations, REDRESS 98 retires
   `G-W3-UNION-SUBSTRATE`, and REDRESS 102 blocks parse-only row movement.
   Later direct and typed admissions require their own measured row gates,
   same-wave consumers, and independent Track 2/oracle evidence; REDRESS 110 is
   close accounting only. The V3 packet names these routes as cautions, not as
   implementation options.

## Required Fold

None. Carry forward the V2 CH5 lane boundaries unchanged: generated Track 1,
hand Track 2, typed guards, serde/oracle paths, W0-clamped rows, parse
diagnostics, PMU/cycles, structural scan, masking/lazy-tape facts, and sidecar
freshness are planning evidence only unless a later converged pass supplies its
own measured product consumer and row gate.
