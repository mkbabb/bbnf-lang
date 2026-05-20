# SK-V11 S-P3 V3 CH5 - Hidden Coupling / Lock 1

Verdict: ACCEPT

Acceptance percentage: 100%

Scope audited: S-P3 V3 packet after the V2 CH1 required folds, with emphasis on
W1a/W1b/W2 hidden dependencies, gate-json schema versus rendered results fields,
generated baseline ownership, same-wave consumers, and implicit
substrate/directive/BIR coupling.

## Coupling Audit

### 1. W1a/W1b/W2 now have one-way authority, not circular authority

ACCEPT. The V3 packet keeps the non-JSON lane split that V2 CH5 accepted and
removes the V2 CH1 drift that had left P3-A able to imply W0/P3-D floor
authority. The topological contract is explicit: W1a blocks W1b because
non-JSON telemetry must be gate-consumed before a generated baseline row can
become authority; W1b blocks W2 because the first non-JSON baseline and
independent oracle must exist before an intervention can claim improvement; W2
then blocks later generic C1-C7 waves because SK-V11 requires exercised
non-JSON generality, not prose
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:80-86`).

The gate rows match that topology. W1a may only establish the non-JSON
telemetry/gate harness and may not admit behavior or claim baseline authority;
W1b may create exactly one generated non-JSON baseline with independent
Track 2/oracle and may not admit an intervention; W2 consumes the W1b baseline
and must hit `ceil(W1b_css_baseline_mbps * 1.01)`, and if W1b cannot produce the
baseline W2 is REVISE before redress
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:101-106`,
`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:78-80`).
SPEC and DISPATCH carry the same split: W1a has no parser row movement, W1b
has no intervention admission, and W2 may not create the first measurable
non-JSON row
(`restart/skinny/tranches/sk-v11/SPEC.md:283-320`,
`restart/skinny/tranches/sk-v11/SPEC.md:326-377`,
`restart/skinny/tranches/sk-v11/SPEC.md:397-425`,
`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-69`).

### 2. Gate-json/results schema coupling is visible and fail-closed

ACCEPT. P3-D states that the live rendered main table is the 26-column
`SCHEMA_V3_HEADER`, while the full telemetry authority is the union of rendered
schema fields, manifest fields, and comparator-evidence fields; SK-V11 adds no
required column in P3-D, only allowed-value obligations for non-JSON rows that
must move with a same-wave gate extension
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:19-42`,
`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:58-94`).
That matches the live code: `SCHEMA_V3_HEADER` renders the 26 columns
(`skinny/crates/bbnf-bench/src/report.rs:8-9`), `SkV8Telemetry` and
`SkV8ComparatorEvidence` hold the manifest/comparator identifiers
(`skinny/crates/bbnf-bench/src/report.rs:33-68`), and `render_markdown` emits the
manifest fields including row id, grammar, domain, run id, cost facts, substrate,
consumer, Track 2 status, diagnostic nonproducer status, and comparator evidence
(`skinny/crates/bbnf-bench/src/report.rs:618-652`; rendered header in
`skinny/RESULTS.md:47-49`).

The current gate is JSON-only, which is acceptable because W1a exists precisely
to change that before W1b. The live validator rejects non-JSON rows today by
requiring `grammar_id == "json"` and `domain == "json_bench"` and by parsing row
ids only in `json/<corpus>/<workload>/main` form
(`skinny/crates/bbnf-bench/src/report.rs:327-333`,
`skinny/crates/bbnf-bench/src/report.rs:1675-1684`). V3 does not hide that
dependency: W1a's task is to add failing and passing fixtures for grammar id,
domain, output plane, comparator/oracle, Track 2/oracle independence, run id,
host, feature mask, same-wave consumer class, and producer-only telemetry
rejection, while keeping JSON `gate-json --with-cost-facts --check-results`
green (`restart/skinny/tranches/sk-v11/SPEC.md:299-315`). P3-D's fail-closed
rule also rejects producer-only fields, validator-only fields, unconsumed
non-JSON oracle data, and any field not read by `validate_schema_v3`,
`validate_sk_v8_w0`, `validate_strict_admission`, or the same-commit gate
extension
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:195-222`).

### 3. Generated baseline ownership is narrow enough for CH5

ACCEPT. W1b has broad owner paths, but the authority is bounded before redress:
CHALLENGE selects exactly one non-JSON target and names the independent
oracle/Track 2 path; W1b must stand up exactly one generated non-JSON direct or
typed parser baseline row, prove strict output equality, prove gate consumption,
and prove the live `json_provider` path does not leak JSON policy into the
selected generated parser
(`restart/skinny/tranches/sk-v11/SPEC.md:331-355`). The exit gate then requires
the generated Track 1 baseline, independent oracle/Track 2, strict equality,
rendered run/host/flags/sample/output-plane/oracle status, no generic JSON
policy outside generated per-grammar modules, no behavior admission, and no JSON
row movement (`restart/skinny/tranches/sk-v11/SPEC.md:357-367`).

The generated-output ownership rule is not implicit. SPEC non-negotiables state
that generated output may be committed only as regenerated output from named
generator/schema input and that every generic/codegen/runtime-outside-JSON edit
needs same-wave CSS L4, Sheets, or BBNF-self proof
(`restart/skinny/tranches/sk-v11/SPEC.md:176-179`). The Lock 14 gate repeats
that generated output is regenerated from named inputs, never hand-patched, and
that proof failure reverts generic/codegen/runtime edits as one slice
(`restart/skinny/tranches/sk-v11/SPEC.md:229-244`). DISPATCH carries the same
load-bearing facts
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:154-160`).

### 4. Same-wave consumers are not deferred

ACCEPT. The governing skinny contract rejects orphan kernels: every primitive,
kernel, or new generated path must wire the hot-path caller in the same commit,
bench the named rows, and show the consumer in the profile path
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177-186`). The S-P3 prompt
requires P3-A to name a same-wave consumer per candidate and P3-C to gate on it
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:253-256`).

V3 carries that rule into every relevant surface. P3-A's shared hard gate
requires same-wave consumers in generated direct, generated typed, or generated
non-JSON product paths and requires `gate-json` or the named non-JSON gate to
consume every emitted field
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:423-430`).
P3-C rejects plans missing a same-wave hot-path consumer or missing a gate
consumer for emitted telemetry fields
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:187-200`).
SPEC non-negotiables reject any primitive, SIMD kernel, generated path, codegen
shape, or host sink without same-wave consumer and measured gate
(`restart/skinny/tranches/sk-v11/SPEC.md:172-175`). W2 specifically wires exactly
one SK-V11 primitive family into the generated non-JSON consumer and consumes the
W1b baseline instead of creating one in the intervention wave
(`restart/skinny/tranches/sk-v11/SPEC.md:402-409`).

### 5. Substrate, directive, BIR, and Track 1/Track 2 coupling remain blocked

ACCEPT. The CH5 lens asks whether the plan introduces parallel substrate,
sidecar producer, renamed scanner, or Track 1 == Track 2 dishonesty
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:116-123`,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134-138`). V3's close condition
and non-negotiables directly forbid W3 union/event substrate, class columns,
structural-position vectors, streaming cursors, class lanes, sidecar producers,
parse-plane substrate repair, cascade-lock through W3, new directives, BIR
variants, public substrate APIs, parser-owned sidecars/fact slots, generic JSON
policy, and second retained substrates
(`restart/skinny/tranches/sk-v11/SPEC.md:38-55`,
`restart/skinny/tranches/sk-v11/SPEC.md:163-181`).

The same prohibitions are replicated at the candidate and ledger layers. P3-A
rejects C6 on JSON-provider policy in generic codegen, new directive/BIR/backend
variant, generic-crate grammar names, hidden sidecar, Track 1/Track 2 shared
implementation, prose-only non-JSON proof, or absence of a W1b baseline
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:350-361`).
P3-C pre-blocks W3 substrate repair, sidecars, parser-owned projections,
structural-position vectors, aux density tables, object/key/value-byte carry,
new directives, BIR variants, `BackendShape`, public substrate APIs, and
benchmark-private parsers
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:212-229`).
P3-E's global ledger blocks new directives, BIR variants, `BackendShape`,
`UnionTape`, public substrate APIs, parser-owned fact slots, retained sidecars,
structural-position vectors, alternate retained tapes, Track 1 == Track 2,
benchmark-private parsers, hidden hand sinks, shared parser evidence, and
generic JSON policy in `parse-that-regex`, `bbnf-simd`, IR, codegen, or runtime
outside grammar-local code
(`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:68-72`,
`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:216-220`).

### 6. V2 residual CH5 watch items are folded or still explicitly guarded

ACCEPT. V2 CH5's residual watch items were W1b owner breadth, companion non-JSON
report consumption, and W7 output-sink sensitivity. V3 narrows W1b through
exactly-one target selection and named oracle path before redress
(`restart/skinny/tranches/sk-v11/SPEC.md:345-347`), keeps companion non-JSON
reports non-closing unless their gate runs in the same wave and their admitted
row is reconciled into SPEC/HANDOFF/REDRESS
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:168-172`),
and keeps C8/output digest as oracle or host sink only, not parser vocabulary
(`restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md:21-27`,
`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:198-200`).

## Verdict

ACCEPT. V3 exposes, gates, and sequences the hidden-coupling risks instead of
burying them. W1a owns schema/gate consumption only, W1b owns exactly one
generated non-JSON baseline plus independent oracle, and W2 consumes that
baseline for the first admitted intervention. The gate-json/results mismatch is
explicitly the W1a job and is fail-closed today; generated output and baseline
authority are tied to named inputs and revert slices; same-wave consumers are
required before production evidence counts; and the substrate/directive/BIR/
Track 1-Track 2 coupling surfaces remain hard pre-blocked.

## Sources

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v11/research/p3/hardening/V2/CH5-hidden-coupling.md`
- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/src/report.rs`
