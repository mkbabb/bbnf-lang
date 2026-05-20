# SK-V11 S-P3 V4 CH5 - Hidden Coupling / Lock 1

Verdict: ACCEPT

Acceptance percentage: 100%

Scope audited: S-P3 V4 stability packet after the V3 6/6 ACCEPT cycle, with
focus on whether V4 preserved the accepted V3 hidden-coupling boundaries:
one-way W1a/W1b/W2 authority, visible gate-json/schema coupling,
same-wave consumers, and no hidden substrate/directive/BIR coupling.

## Stability Check

V4 is the required stability cycle, not a new semantic fold. `ORCHESTRATOR.md`
requires pass cycles to continue until two consecutive cycles reach at least 95%
ACCEPT with zero open critical defects and no unresolved REVISE
(`restart/prompts/ORCHESTRATOR.md:104-122`). The S-P3 plan repeats that every
CHALLENGE disposition must fold into the next cycle and that S-P3 advances only
after two consecutive clean cycles (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:151-166`).
The V3 consolidation explicitly made V4 a stability cycle: V3 had 6/6 ACCEPT,
no open critical defects, no open REVISE dispositions, and asked V4 to preserve
the V3 semantics and rerun the lenses
(`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:11-22`,
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`).
The V3 CH5 boundary being preserved was explicit: W1a gates/reporting, W1b
baseline/oracle authority, and W2 intervention authority are one-way
(`restart/skinny/tranches/sk-v11/research/p3/hardening/V3/CH5-hidden-coupling.md:14-40`).

## Coupling Audit

### 1. W1a/W1b/W2 authority remains one-way

ACCEPT. V4 preserves the V3 topology. P3-B keeps W1a and W1b CHALLENGE-gated
because they establish the first non-JSON gate/report and baseline authority,
then states the order directly: W1a blocks W1b because non-JSON telemetry must
be gate-consumed first, W1b blocks W2 because the first non-JSON baseline and
independent oracle must exist first, and W2 blocks later generic C1-C7 waves
because generality must be exercised, not asserted
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:57-62`,
`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:80-86`).

The gates carry the same authority split. W1a may only create the non-JSON
gate/report lane and cannot claim generated baseline authority
(`restart/skinny/tranches/sk-v11/SPEC.md:283-320`). W1b may create exactly one
generated non-JSON baseline plus independent oracle, but cannot admit an
intervention or move a row
(`restart/skinny/tranches/sk-v11/SPEC.md:326-377`). W2 has the intervention
gate, but only after W1b closes, and it must consume the W1b baseline rather
than create the first measurable non-JSON row
(`restart/skinny/tranches/sk-v11/SPEC.md:397-425`). DISPATCH repeats the same
dependency order (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-72`).

P3-A and P3-C preserve the same boundary on the candidate and gate surfaces:
P3A-C6 names W1a as the gate/report lane, W1b as the baseline row, and W2 as
the first possible admit at `ceil(W1b_css_baseline_mbps * 1.01)`
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:351-361`).
P3-C rejects W2 if it creates the first non-JSON baseline and requires the
W1b threshold before admission
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:78-80`,
`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:171-175`).

### 2. Gate-json/schema coupling stays visible and fail-closed

ACCEPT. P3-D still says it is schema binding, not source authority, and that
required fields must be emitted and consumed by `gate-json` in the same wave
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:16-26`).
It preserves the live 26-column schema-v3 table, adds no SK-V11 required column
at P3-D, and makes any non-JSON allowed-value extension a same-wave gate
extension rather than a producer-only schema fork
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:28-42`,
`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:91-94`,
`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:154-172`).

The rejection rules are explicit: missing schema or manifest fields,
producer-only fields, validator-only fields, unconsumed non-JSON oracle data,
Track 2 coupling, parse-only SOTA claims, W3 reopen claims, and stale strict
anchors all fail closed (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:195-222`).
SPEC carries the same required identifier set and same-wave consumer rule
(`restart/skinny/tranches/sk-v11/SPEC.md:81-114`), and DISPATCH repeats that a
wave emitting a new field or companion report must update every report,
fixture, gate, and consumer in the same commit
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:190-201`).

The live code remains aligned with that visible split: `SCHEMA_V3_HEADER` is
the rendered schema header (`skinny/crates/bbnf-bench/src/report.rs:8`),
`validate_schema_v3` and `validate_sk_v8_w0` are the current gate consumers
(`skinny/crates/bbnf-bench/src/report.rs:220-331`,
`skinny/crates/bbnf-bench/src/report.rs:499-528`), and the manifest render
includes grammar, domain, run id, cost facts, substrate, consumer, Track 2, and
diagnostic nonproducer fields (`skinny/crates/bbnf-bench/src/report.rs:620-651`).
V4 does not hide today's JSON-only gate shape; W1a owns extending it before W1b.

### 3. Same-wave consumers are preserved

ACCEPT. The governing P3 prompt still requires every primitive to land the
hot-path consumer in the same commit, with P3-A naming it and P3-C gating it
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:249-256`). P3-A's V4 shared
gate requires same-wave consumers in generated direct, generated typed, or
generated non-JSON product paths and requires `gate-json` or a named non-JSON
gate to consume every emitted field
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:421-430`).

P3-C turns missing same-wave consumers into a pre-redress reject rather than a
deferral (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:187-205`).
SPEC makes the same rule a non-negotiable for primitives, SIMD kernels,
generated paths, codegen shapes, and host sinks
(`restart/skinny/tranches/sk-v11/SPEC.md:172-177`), then repeats it in the
micro-prove-first gate (`restart/skinny/tranches/sk-v11/SPEC.md:213-227`).
DISPATCH also requires plan agents to name owner paths, row thresholds, revert
protocol, same-wave consumer, and pre-blocked routes before redress
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:88-96`).

### 4. No hidden substrate, directive, BIR, or Track 1/Track 2 coupling

ACCEPT. V4 keeps the CH5 pre-blocks hard. SPEC close conditions forbid W3
union/event substrate, class columns, structural-position vectors, streaming
cursors, class lanes, sidecar producers, parse-plane substrate repair,
cascade-lock through W3, new directives, BIR variants, public substrate APIs,
parser-owned sidecars, generic JSON policy, and second retained substrates
(`restart/skinny/tranches/sk-v11/SPEC.md:38-57`). The non-negotiables also
forbid parser-owned sidecar/fact slots, generic JSON policy outside generated
per-grammar modules, orphan kernels, producer-only telemetry, and Track 2/oracle
calls into generated Track 1 or hidden benchmark-private parser code
(`restart/skinny/tranches/sk-v11/SPEC.md:163-183`,
`restart/skinny/tranches/sk-v11/SPEC.md:246-251`).

P3-E's hard blocks match that surface: no new directives, BIR variants,
`BackendShape`, public substrate APIs, sidecars, structural-position vectors,
Track 1 == Track 2, hidden hand sinks, shared parser evidence, or generic JSON
policy in generic crates/runtime outside generated grammar-local code
(`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:64-72`,
`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:214-220`).
P3-C carries the same pre-blocks into gate review
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:207-229`),
and P3-F carries them into the draft SPEC/dispatch layer
(`restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md:103-123`).

### 5. Generated baseline ownership remains narrow

ACCEPT. V4 does not let broad W1b owner paths become broad authority. W1b's
entry gate requires W1a closed plus CHALLENGE selection of exactly one
non-JSON target and named independent oracle/Track 2 path
(`restart/skinny/tranches/sk-v11/SPEC.md:345-347`). Its tasks and exit gate are
limited to exactly one generated non-JSON direct or typed parser baseline row,
strict equality, gate consumption, no generic JSON policy leakage, no behavior
admission, and no JSON row movement
(`restart/skinny/tranches/sk-v11/SPEC.md:349-367`). P3-E says W1b is
baseline-only and may not land intervention, row admission, coupled oracle, or
generic JSON policy (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:160-170`).

Generated output ownership is likewise explicit rather than hidden. SPEC allows
generated output only as regenerated output from named generator/schema input
and requires proof failure to revert generic/codegen/runtime edits as one slice
(`restart/skinny/tranches/sk-v11/SPEC.md:176-179`,
`restart/skinny/tranches/sk-v11/SPEC.md:229-244`). DISPATCH repeats that
generated output may be committed only from named inputs
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:154-160`).

## Verdict

ACCEPT. V4 preserved the V3 CH5 semantics. W1a owns gate/report schema
visibility only; W1b owns exactly one generated non-JSON baseline plus
independent oracle; W2 consumes that baseline for the first possible admitted
intervention. Gate-json/schema coupling is visible and fail-closed; emitted
fields and non-JSON companion reports require same-wave consumers; and the
substrate/directive/BIR/Track 1-Track 2 coupling routes remain explicitly
pre-blocked. No REVISE item is required for CH5.

## Sources

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v11/research/p3/hardening/V3/CH5-hidden-coupling.md`
- `restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`
- `skinny/crates/bbnf-bench/src/report.rs`
