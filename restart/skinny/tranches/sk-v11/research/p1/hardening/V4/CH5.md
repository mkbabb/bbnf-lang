ACCEPT

# SK-V11 S-P1 Hardening V4 CH5 Hidden Coupling

Scope: `restart/prompts/skinny/PASS-1-PROFILE.md` Section 3 CH5,
`restart/prompts/ORCHESTRATOR.md` Section 3Z, the folded S-P1 packet
P1-A through P1-F at HEAD `cc8656b8`, W0 baseline, `skinny/RESULTS.md`,
`skinny/REDRESS.md` through item 110, and the S-P1 hardening V1/V2/V3
consolidations.

## Findings

1. Track 1 and Track 2 remain separate. P1-A profiles generated parse and
   independent Track 2 parse as distinct lanes; P1-B defines `T1` as generated
   Track 1 and `T2` as independent hand-coded Track 2; P1-E keeps generated,
   hand, typed, serde/oracle, and core-helper symbols as evidence members under
   canonical primitives. `skinny/RESULTS.md` still states that Track 1 is
   `runtime::generated_json::parse` and Track 2 is the independent hand-coded
   parser over `runtime::tape` that never calls generated runtime parse.

2. Direct, typed, and oracle evidence stay fenced. W0 keeps `direct_to_struct`
   as the primary closure surface and `real_typed_struct` as a guard surface.
   P1-B and P1-E treat typed Track 2 leaves as serde/oracle comparator evidence,
   not generated typed Track 1 behavior. P1-F extracts direct and typed rows in
   separate tables. No direct digest row is used to close a typed row, and no
   typed guard row is used to close a direct residual.

3. W0-clamped rows remain non-admissions. W0, P1-B, P1-C, P1-E, P1-F, and
   `skinny/RESULTS.md` all keep `instruments`, `numbers`, and `unicode_mixed`
   as `N-direct / NO-GO` planning evidence even where Track 1, or both tracks,
   clear a computed floor. The packet does not admit a row from W0 floor math
   alone.

4. Parse evidence remains diagnostic. P1-A says parse-only is not an SK-V11
   SOTA target; P1-C uses parse Mbps only as a probe denominator; P1-F records
   `canada/parse_only` as `L / NO-GO` and `S` as a parse-only diagnostic enum.
   REDRESS 102 keeps parse-only movement firewalled from direct or typed
   admission.

5. PMU and cycles remain nonproducer evidence. P1-D supplies real
   `ri_cycles` and `ri_instructions` rows for parse, direct, and typed guard
   lanes, but states that no PMU row changes `skinny/RESULTS.md`. P1-E uses PMU
   only for c/B shape, and P1-F preserves
   `structural_scan+masking_probes+pmu+cycles:nonproducer`.

6. Structural-scan, masking-probe, lazy-tape, and sidecar-freshness evidence
   stays separate from behavior producers. P1-C reports structural scan below
   the 40000 Mbps aarch64 floor and marks masking/lazy-tape rows as diagnostic.
   P1-E keeps structural scan, sparse flags, lazy tape, and eager decode in
   context and pre-block tables. P1-F treats historical or absent sidecars as
   planning signals only. Samply `.json.syms.json` files remain symbol maps,
   not self-time or admission authority.

7. No rejected substrate route is normalized. REDRESS 50, 51, and 53 reject
   sidecar/cursor forms; REDRESS 96 and 97 falsify the class-column and
   streaming-cursor W3 implementations; REDRESS 98 retires
   `G-W3-UNION-SUBSTRATE`; REDRESS 102 blocks parse-only row movement; REDRESS
   109 admits only an SK-V10 direct residual row under its own measured gate; and
   REDRESS 110 is close accounting. The V4 CH5 packet names these as cautions,
   not implementation options.

8. V1, V2, and V3 already accepted CH5, and V3 was the first all-ACCEPT cycle.
   Section 3Z requires folds before advancing; the only V2-to-V3 fold was Lock
   14 wording, not a CH5 lane change. V4 confirms the folded packet still keeps
   Track 1, Track 2, oracle, typed, W0-clamped, parse, PMU, and structural-scan
   evidence separated.

## Required Fold

None. Carry forward the existing CH5 lane boundary unchanged: generated
Track 1, independent hand Track 2, typed guards, serde/oracle paths,
W0-clamped rows, parse diagnostics, PMU/cycles, structural scan,
masking/lazy-tape facts, and sidecar freshness remain planning evidence only
unless a later converged pass supplies its own measured product consumer and
row gate.
