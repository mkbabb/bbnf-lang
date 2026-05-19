ACCEPT

# SK-V11 S-P1 Hardening V4 CH6 Confirmation

Scope: CH6 anti-paper-close only. Read `PASS-1-PROFILE.md` Section 3 CH6,
`ORCHESTRATOR.md` Section 3Z, folded S-P1 packet P1-A through P1-F at HEAD
`cc8656b8`, W0 baseline, `skinny/RESULTS.md`, live `/tmp/skv11-p1` artifact
roots, and S-P1 hardening V1/V2/V3 consolidations.

## Findings

1. CH6 remains satisfied after the V3 all-ACCEPT cycle. PASS-1 CH6 requires
   profile claims to have citable artifacts and resolvable symbol evidence, and
   treats unexplained `unprofiled` or `n/a` cells as paper-close. Section 3Z
   requires every challenge disposition to fold before advancement. V1 and V2
   accepted CH6 with no fold, and V3 accepted all six lenses with no required
   fold.

2. No placeholder or unprofiled S-P1 cell remains in the folded packet. A scan
   of P1-A through P1-F, W0, and `skinny/RESULTS.md` found no `unprofiled`,
   `placeholder`, `TODO`, `TBD`, `not profiled`, or `no profile` terms. The
   `n/a` values in `skinny/RESULTS.md` are source-qualified comparator or
   historical-baseline absences, not S-P1 profile holes.

3. Live artifacts are present under `/tmp/skv11-p1`. Current counts match the
   packet claims: 34 parse, 34 direct, and 14 typed samply `.json.gz` files,
   each with matching `.json.syms.json` sidecars; 34 parse and 48 product Time
   Profiler trace bundles; 34 parse and 48 product CPU Counter trace bundles;
   34 parse and 48 product xctrace symbol exports; 34 parse PMU rows and 48
   product PMU rows. `capture_status.tsv` reports `rc=0` for all 34
   `samply-parse`, 34 `samply-direct`, 14 `samply-typed`, 34 `pmu-parse`, and
   48 `pmu-direct` capture rows.

4. The xctrace and samply caveats are honest. P1-A says the exact per-row
   samply shell transcript is not embedded and therefore treats saved samply
   profiles as artifact-only flame evidence while using xctrace exports as the
   self-time percentage authority. P1-A and P1-B disclose that saved samply JSON
   reports `symbolicated=false` while sidecar symbol maps exist. P1-D and P1-E
   disclose the xctrace `rc=54` time-limit path and use retained trace/export
   artifacts without claiming those xctrace rows were clean exits.

5. No row is closed from PMU, cycles, parse-only, structural-scan, masking, or
   W0-clamped evidence. P1-D states that PMU/cycles do not change any
   `skinny/RESULTS.md` outcome. P1-E keeps Criterion Mbps and W0 gate state as
   row authority and uses PMU only for c/B shape. P1-A and P1-C keep parse-only
   and structural-scan/masking probe facts diagnostic. P1-F preserves
   `structural_scan+masking_probes+pmu+cycles:nonproducer` and does not admit
   direct or typed rows from those lanes.

6. W0-clamped throughput remains fenced. W0, P1-B, P1-C, P1-E, P1-F, and
   `skinny/RESULTS.md` keep `instruments`, `numbers`, and `unicode_mixed` as
   `N-direct / NO-GO` non-admissions even where a computed floor is cleared.
   Treating those rows as closed would still be a paper-close.

## Required Fold

None. Carry forward the existing CH6 guard unchanged: future consumers must
require artifact-backed profile evidence and W0/RESULTS row authority, and must
not convert PMU/cycles, parse-only wins, structural scan, masking probes, or
W0-clamped throughput into row admission.
