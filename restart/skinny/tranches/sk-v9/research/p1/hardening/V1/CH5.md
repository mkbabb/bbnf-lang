# SK-V9 S-P1 Hardening V1 CH5: Hidden Coupling / Substrate

Disposition: REVISE.
Confidence: 87%.
Lens: CH5 Hidden Coupling / Substrate.
Date: 2026-05-18.

## Verdict

The six P1 artifacts are mostly honest about absence: they do not invent fresh
SK-V9-open samply captures, PMU counters, sidecar manifests, Apache/CITM measured
typed rows, or strict comparator admission. That prevents a REJECT on CH5.

The set is not acceptable yet because structural-scan, masking-probe, and
cycles-per-byte surfaces are not fenced hard enough as diagnostic observations.
Several places describe second source scans or probe producers as future manifest
work without requiring a Lock 1 label and non-producer metadata. That is the
exact substrate drift CH5 exists to catch.

## Accepted Substrate Facts

- P1-A records W0 as the measured row authority and keeps fresh SK-V9 samply
  coverage at 0/17, with no invented symbol/self-time rows
  (`p1a-samply-mode-1.md:7-11`, `p1a-samply-mode-1.md:151-156`).
- P1-B preserves the direct digest guard plane and keeps Apache/CITM typed rows
  as source/product parity gaps rather than measured row progress
  (`p1b-samply-mode-2.md:91-96`, `p1b-samply-mode-2.md:154-165`).
- P1-F flags run identity, sidecar freshness, hot-leaf gaps, and strictness
  deferral instead of treating W0 as SK-V9-open evidence
  (`p1f-results-delta.md:36-42`, `p1f-results-delta.md:105-111`).
- The SK-V9 contract itself forbids comparator sidecars from producing parser
  data, row output, substrate, or strict admission
  (`SYNTHESIS.md:49-52`; `HANDOFF.md:47-51`).
- The tape/direct union is named correctly in P1-E: direct values borrow from
  tape identity, and tape symbols must be attributed as substrate
  (`p1e-hot-leaf-attribution.md:265-269`; PASS-1 `PASS-1.md:54`).

## Defects

### D-CH5-1 - Structural-scan evidence is not fenced as a Lock 1 observation

Severity: high.

P1-C says `simd_scan` iterates every fixture for scalar/SIMD parity and
structural-scan benchmarking (`p1c-samply-mode-3.md:44`) and later says the
structural-scan path computes scalar and SIMD offsets, hashes them, and asserts
equality (`p1c-samply-mode-3.md:101-103`). The source confirms that this is a
separate offset-vector scan over the fixture bytes
(`skinny/crates/bbnf-bench/benches/simd_scan.rs:16-26`, `:29-37`).

That is valid profiling evidence only if it is explicitly labeled as a Lock 1
observation of a second source scan. It must not become a retained cursor, event
vector, or parser-owned fact slot. P1-C partially protects this by saying it does
not propose a cursor or retained route (`p1c-samply-mode-3.md:105-107`), but the
per-row table and source ledger still normalize structural-scan status as a
profile surface without a `Lock1 diagnostic non-producer` label.

Fold requirement: P1-C must add a CH5 substrate fence for structural-scan-only:
`producer_class=diagnostic_nonproducer`, `substrate_relation=Lock1 observation`,
`may_feed_row_admission=false`, `may_feed_tape_or_cursor=false`, and
`may_define_ValueRef_contract=false`.

### D-CH5-2 - P1-D makes `cycles_per_byte` look like an admitted workload

Severity: high.

P1-D's fold requirement asks for a SK-V9-open manifest for all admitted workloads
including masking probes and `cycles_per_byte`
(`p1d-pmu-cycles.md:271-275`). The same artifact correctly refuses to derive
c/B without same-run cycles (`p1d-pmu-cycles.md:71-73`, `:280-284`), but CH5
needs one more fence: `cycles_per_byte` backed by structural-scan metadata is a
diagnostic PMU surface, not a parser substrate, product plane, or row-moving
producer.

The gate source validates SIMD scan metadata as `track=SimdScan`,
`workload=cycles_per_byte`, `materialisation=structural_offsets`, and
`output_plane=offset bitmap` (`skinny/crates/bbnf-bench/src/bin/gate.rs:1389-1419`).
That is not the same plane as Track 1 generated runtime or Track 2 independent
parser output.

Fold requirement: P1-D must replace "all admitted workloads" for probes and
`cycles_per_byte` with "diagnostic manifest sections". The report/gate contract
must reject any attempt to use `cycles_per_byte`, `structural_offsets`, or
`offset bitmap` as Track 1, Track 2, strict admission, direct product proof, or
Apache/CITM measured-row evidence.

### D-CH5-3 - Capacity scans are an unreviewed hidden coupling surface

Severity: medium-high.

P1-E describes retained parse as `ParserState` plus `attach_structural_index`
plus tape finish (`p1e-hot-leaf-attribution.md:92-97`). The source shows
`attach_structural_index` is currently inert (`generated.rs:14-17`), but
`ParserState::new` calls `structural_capacity_for` before parse
(`parser.rs:16-24`). That helper can use exact or one-shot SIMD structural scans
to size the tape (`scan.rs:47-52`).

This is not necessarily a violation. It is a hidden coupling risk: if a profile
attributes hot time to `scan_structurals`, `structural_capacity_for`, or
capacity planning, the artifact must classify it as tape-substrate capacity
work, not as an independent structural producer.

Fold requirement: P1-E must add capacity-planning to the tape substrate class.
Any `structural_capacity_for`, `scan_structurals`, `exact_structural_count`, or
capacity-plan symbol must fold into `tape/capacity` unless a future accepted
route explicitly proves a different consumer. It cannot create a retained
cursor, event vector, row output, or sidecar substrate by implication.

### D-CH5-4 - Masking probes need non-producer metadata before rendering

Severity: medium.

The report can render probe rows with Mbps, ns/iter, vs-Track-1 ratio, and
signal (`report.rs:612-625`), and the gate populates probes including
`host_call_eager_decode`, `alternate_scalar_plan`, and `cold_first_parse`
(`gate.rs:1500-1540`). P1-C and P1-E correctly state current measurements are
absent and do not claim main-row hot leaves (`p1c-samply-mode-3.md:66`,
`p1e-hot-leaf-attribution.md:232-244`).

The remaining risk is future rendering: `alternate_scalar_plan` is an external
`serde_json::Value` parse (`json_parity.rs:407-412`), while eager decode parses
Track 1 and then walks the tape-backed view (`json_parity.rs:399-405`,
`:440-455`). Those are diagnostic probes, not alternate same-run producers for
Track 1 or Track 2.

Fold requirement: probe report rows must carry explicit metadata:
`producer_class=probe_nonproducer`, `track_role=none`, `substrate_output=none`,
and `strict_admission=false`. Probe rows may name masking signals for S-P2, but
must not populate Track 1/Track 2 columns, row hot-leaf replacement cells, or
same-run sidecar proof.

### D-CH5-5 - Typed direct Track 2 language needs a narrower label

Severity: medium.

P1-B keeps Apache/CITM source/product parity out of measured rows, which is
correct (`p1b-samply-mode-2.md:93-96`). The measured typed rows still carry a
Track 2 Mbps column (`p1b-samply-mode-2.md:91-99`), while the W0 result text
labels typed Track 2 as a structural oracle, not the SOTA gate
(`skinny/RESULTS.md:7`, `:18`, `:21`, `:28`). That distinction is load-bearing
because CH5 forbids Track 1/Track 2 symbol-path conflation
(`PASS-1-PROFILE.md:148-153`).

Fold requirement: typed direct rows must label Track 2 as
`typed_oracle_independent` or equivalent, not as a hand-parser performance lane.
The fold must state that typed Track 2 is not a sidecar same-run producer, not a
row-moving comparator, and not a substitute for generated Track 1 DirectBuild.

## Non-Defects

- No P1 artifact claims C++ sidecars are same-run strict anchors. P1-B and P1-F
  call them historical or absent (`p1b-samply-mode-2.md:162-165`;
  `p1f-results-delta.md:110`), matching the W0 note (`skinny/RESULTS.md:141`).
- No P1 artifact claims Apache/CITM are measured typed rows. The artifacts keep
  them source/product only and cite REDRESS 91.
- No P1 artifact proposes `UnionTape`, `BackendShape`, `tape_vs_tape`, a public
  substrate API, or a parser-owned cursor/fact slot. The handoff keeps those
  pre-blocked (`HANDOFF.md:95-97`).
- No fresh SK-V9-open throughput or PMU row is fabricated from W0 sample cost.

## Required Fold Summary

1. Add a `diagnostic_nonproducer` fence to P1-C structural-scan-only evidence.
2. Reword P1-D so masking probes and `cycles_per_byte` are diagnostic manifest
   sections, not admitted workloads.
3. Add capacity-planning symbols to P1-E's tape substrate class.
4. Add non-producer metadata to future masking-probe report rows before any
   rendered SK-V9-open manifest consumes them.
5. Narrow typed direct Track 2 wording to `typed_oracle_independent`.

After those folds, CH5 can move to ACCEPT unless a later P1 cycle introduces
fresh profile data that creates a new sidecar, cursor, or substrate producer.
