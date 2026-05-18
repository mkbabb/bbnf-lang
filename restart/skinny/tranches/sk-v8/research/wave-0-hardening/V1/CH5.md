# SK-V8 W0 Hardening V1 CH5: Comparator Evidence And Sidecar Provenance

## Decision

ACCEPT.

## Acceptance Probability

93%.

## Blocking Findings

None.

I did not find a Wave 0 comparator-provenance defect that admits historical
sidecars, C++ sidecar artifacts, sonic-rs lossy measurements, stale evidence, or
plane-mismatched comparator evidence as strict admission.

## Nonblocking Findings

1. Historical C++ sidecars remain visible in the legacy 26-column RESULTS table
   and deltas, for example `skinny/RESULTS.md:5`, but the gate-consumed W0
   manifest marks populated C++ slots as
   `freshness=historical:sk-v7-sidecar-profile` and
   `source=sidecar-profile:sk-v7-cpp:...` (`skinny/RESULTS.md:48`), and the W0
   note states that C++ sidecars are historical or absent and never strict
   anchors (`skinny/RESULTS.md:265`). This is readable enough for W0, but later
   waves should avoid presenting first-table sidecar deltas without the manifest
   freshness context.

2. The generic strict-admission helper exists and rejects historical/absent/stale
   comparator evidence (`skinny/crates/bbnf-bench/src/gate.rs:133`,
   `skinny/crates/bbnf-bench/src/gate.rs:155`), but W0 report validation does
   not need to invoke it because W0 is a telemetry baseline and parse rows are
   forced to substrate-guard non-admission. Later strict-admission work must
   ensure row consumers call that helper or equivalent gate logic, not just W0
   manifest validation.

3. Direct and real-typed rows preserve existing GO/NO-GO labels in the opening
   table while keeping `Strictness=deferred` and `parse_utf8=view-boundary`
   (`skinny/RESULTS.md:6`, `skinny/RESULTS.md:7`). This is not a W0 blocker
   because SPEC W0 requires parse-only rows to be non-admission and W0 to admit
   only the baseline profile/telemetry lock (`restart/skinny/tranches/sk-v8/SPEC.md:362`),
   but future behavior waves must not treat these W0 labels as fresh strict
   admission.

## Evidence Inspected

- Commit `6d8cb701` is current `HEAD`, and the worktree was clean before this
  review artifact was written.
- Commit summary and evidence: `6d8cb701 feat(sk-v8-wave0): enforce telemetry
  manifest gate`; the commit body claims W0 telemetry manifest validation,
  strict-admission parsing/validation, parse-row non-admission, Lock 14
  validation, regenerated RESULTS, and no behavior/generation drift evidence.
- Governing W0 requirements: same-run strict anchors only where output plane and
  measured validation match (`restart/skinny/tranches/sk-v8/SPEC.md:63`),
  required comparator/sidecar telemetry fields consumed by `gate-json`
  (`restart/skinny/tranches/sk-v8/SPEC.md:101`), W0 exit requirements including
  parse-only non-admission, sidecar freshness, malformed-manifest rejection, and
  no behavior/codegen/generated-output change
  (`restart/skinny/tranches/sk-v8/SPEC.md:362`), and Section 10's block on
  sidecar/permissive/lossy/stale comparator evidence as strict admission
  (`restart/skinny/tranches/sk-v8/SPEC.md:769`).
- DISPATCH W0 protocol: telemetry/gate validation only, focused bench/xtask
  tests, W0-updated `gate-json`, no behavior/generated-output change
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:78`), and non-negotiable
  strict-vs-strict comparator gates (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:186`).
- Parse rows are explicitly converted to `SSubstrateGuardNonAdmission` unless
  they hit hard correctness/schema/SIMD-parity failures
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:317`), and current RESULTS parse
  rows render as `S / NO-GO` (`skinny/RESULTS.md:5`, `skinny/RESULTS.md:8`,
  `skinny/RESULTS.md:10`).
- Native comparator evidence is rendered from same-run Criterion artifacts:
  `sonic_rs_strict` and `serde_json` get `freshness=same-run-native`;
  `sonic_rs_lossy` is rendered only when present and is marked
  `strictness=permissive` (`skinny/crates/bbnf-bench/src/bin/gate.rs:449`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:480`). The benchmark metadata also
  classifies sonic lossy as permissive
  (`skinny/crates/bbnf-bench/src/metadata.rs:422`).
- Direct and real-typed comparator planes are not native-plane lies in current
  code: direct sonic/serde benchmarks call digest comparators
  (`skinny/crates/bbnf-bench/benches/json_parity.rs:225`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs:412`), and real-typed
  sonic/serde benchmarks call typed consumers
  (`skinny/crates/bbnf-bench/benches/json_parity.rs:310`,
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs:202`,
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs:222`).
- C++ sidecar values are produced only from the hardcoded sidecar profile table
  and converted into W0 comparator evidence as `historical:sk-v7-sidecar-profile`
  when populated, or `absent:not-collected-for-<workload>` when absent
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:491`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:499`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:721`).
- W0 manifest validation requires every row to have SK-V8 telemetry, a
  non-placeholder profile/sample, `gate_only` consumer class, parse-only
  `K`/`S`, and comparator evidence
  (`skinny/crates/bbnf-bench/src/report.rs:275`). Report-level W0 validation
  requires the exact SK-V8-open row count, unique known row ids, and Track 1/2
  values within 1% of the embedded opening baseline
  (`skinny/crates/bbnf-bench/src/report.rs:489`).
- Comparator evidence validation rejects duplicate comparator ids, blank
  plane/strictness/freshness/source fields, invalid Mbps, populated sidecar
  values marked absent, sidecar values lacking historical or same-run sidecar
  freshness, absent sidecars lacking `absent:<reason>`, and missing sidecar slots
  (`skinny/crates/bbnf-bench/src/report.rs:862`).
- Search checks found no current `sonic_rs_lossy[...]strictness=strict` evidence
  in `skinny/RESULTS.md`; populated C++ sidecar entries are marked historical,
  and absent C++ slots carry `absent:not-collected-for-...` reasons.

## Exact Remediation If Rejected

Not applicable; this CH5 review accepts W0 for the comparator evidence and
sidecar provenance lens.

If a later reviewer rejects despite the current evidence, the exact remediation
would be to keep `skinny/RESULTS.md` and the gate-generated manifest but add a
W0-only negative test that constructs: a populated C++ sidecar with
`sidecar_freshness=absent:*`, a populated C++ sidecar with
`freshness=same-run-native`, and `sonic_rs_lossy` with `strictness=strict`, then
asserts `validate_sk_v8_w0()` or the strict-admission gate rejects each case.
