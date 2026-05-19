ACCEPT

# SK-V11 S-P1 V2 CH5 Hidden Coupling

Scope: `restart/prompts/skinny/PASS-1-PROFILE.md` Section 3 CH5,
`restart/prompts/ORCHESTRATOR.md` Sections 3W and 3Z, the folded S-P1 packet
under `restart/skinny/tranches/sk-v11/research/p1/`, W0 baseline,
`skinny/RESULTS.md`, `skinny/REDRESS.md` through REDRESS 110, and
`HARDENING-S-P1-V1-CONSOLIDATED.md`.

## Findings

1. Generated Track 1 and hand Track 2 remain separated. P1-B defines `T1` as
   generated Track 1 and `T2` as the independent hand-coded Track 2, then maps
   generated direct, hand direct, Track 2 parse, typed, and serde leaves as
   evidence members instead of one shared parser. P1-E keeps the same split in
   its hot-leaf vocabulary, and `skinny/RESULTS.md` states Track 1 is
   `runtime::generated_json::parse` while Track 2 is the independent hand-coded
   parser that never calls generated runtime parse.

2. Typed guards and serde/oracle evidence are not folded into direct closure.
   P1-B and P1-E treat `real_typed_struct` as a guard surface, with generated
   typed Track 1 separated from serde_json or structurally independent Track 2
   oracle symbols. Direct digest rows remain a separate product plane; no P1
   table uses direct digest evidence to prove a typed product row.

3. W0-clamped rows remain fenced. W0, P1-B, P1-C, P1-E, and P1-F all keep
   `instruments`, `numbers`, and `unicode_mixed` as `N-direct / NO-GO`
   planning evidence even when one or both fresh throughput numbers clear a
   computed floor. V2 does not admit any row from W0 floor math alone.

4. Parse diagnostics remain non-product evidence. P1-A is explicitly
   parse-only diagnostic coverage, P1-F folds `canada/parse_only` as
   `L / NO-GO`, and REDRESS 102 firewall-closes parse-only movement. No parse
   row is used as a SK-V11 direct or typed admission.

5. PMU/cycles evidence stays diagnostic. P1-D supplies real `ri_cycles` and
   `ri_instructions` PMU rows for parse, direct, and typed guards, but states
   that no PMU row changes `skinny/RESULTS.md`. P1-F keeps
   `structural_scan+masking_probes+pmu+cycles:nonproducer` as the report
   contract.

6. Structural scan, masking probes, lazy-tape facts, and sidecar freshness stay
   separate from behavior producers. P1-C reports structural scan and masking
   probes as diagnostic only; P1-E keeps structural scan, lazy tape, sparse
   flags, and eager decode in a separate context table; P1-F treats historical
   or absent sidecars as planning signals only. Samply `.json.syms.json`
   sidecars are symbol maps, not self-time authority.

7. No retired sidecar or parallel substrate route is normalized. P1-E's
   pre-block matrix carries REDRESS 50, 51, 53, 54, 55, 60-69, 72, 80, 82-84,
   88-90, 96-98, and 102 as hard cautions. REDRESS 96 and 97 falsify the
   class-column and streaming-cursor W3 implementations, REDRESS 98 retires
   `G-W3-UNION-SUBSTRATE`, and REDRESS 102 keeps parse-only diagnostics out of
   product admission. V2 names these as rejected routes, not as implementation
   options.

## Required Fold

None. Carry forward the V2 lane boundaries unchanged: generated Track 1, hand
Track 2, typed guards, serde/oracle paths, W0-clamped rows, parse diagnostics,
PMU/cycles, structural scan, masking/lazy-tape facts, and sidecar freshness are
evidence for later planning only unless a later converged pass supplies its own
measured product consumer and row gate.
