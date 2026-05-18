# SK-V9 S-P1 Hardening V1 - CH2 Generality

Date: 2026-05-18.
Lens: CH2 Generality / Lock 14.
Reviewed commit: `00499f13` (`docs(sk-v9-p1): archive opening profile extraction cohort`).
Disposition: ACCEPT.
Confidence: 94%.

## Read Set

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/audit/pass-1-substrate/PASS-1.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Verdict

The S-P1 V1 packet is CH2-clean enough to accept. It measures and extracts the
current JSON engine state, but it does not turn JSON row paths into generic
primitives, does not prescribe a JSON-only primitive, does not authorize a new
directive/BIR/substrate surface, and routes the absent SK-V9-open profiling/PMU
work through the existing W0 telemetry-lock boundary.

This is an ACCEPT with required folds because the packet contains two phrases
that are easy to over-read downstream: P1-E's source-supported eligible classes
are not hot-leaf attribution, and P1-D's W0 telemetry-lock requirement is a
JSON-instance report/gate need, not a generic report schema or substrate change.

## Findings

### F1 - Grammar-Neutral Attribution Holds

Disposition: ACCEPT.

S-P1 CH2 requires profile attribution to grammar-neutral primitives such as
scanner, classifier, tape, or dispatch rather than JSON-named code paths
(`restart/prompts/skinny/PASS-1-PROFILE.md:129-135`). The P1 artifacts do not
claim resolved SK-V9-open hot leaves. P1-A marks all fresh samply artifacts and
symbol/self-time cells absent and treats historical `parse_value_at` entries as
stale fused proxy evidence only
(`restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md:49-71`,
`:99-105`). P1-B and P1-C likewise avoid top-symbol claims
(`restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md:83-87`,
`restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:66-67`).

P1-E is the decisive artifact: it classifies main rows as
`GAP:not-classified` because there is no resolved samply symbol or self-time
percentage, and it explicitly separates source-supported eligible classes from
actual attribution
(`restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:72-88`,
`:154-159`, `:181-187`, `:248-260`). P1-F carries the same distinction as the
`HL` telemetry flag: current hot-leaf cells are Criterion slope-profile
bindings, not symbol-plus-percentage attribution
(`restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md:36-42`,
`:103-108`).

### F2 - No JSON-Only Primitive Is Prescribed

Disposition: ACCEPT.

No P1 artifact proposes an intervention. P1-A warns that historical profile
proxies should not drive S-P2 design by themselves
(`restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md:103-105`).
P1-C records mode-III and structural-scan absences without adjudicating or
claiming a new primitive
(`restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md:40-44`,
`:99-107`). P1-D refuses to derive cycles-per-byte from wall time, ns/B,
throughput, CPU model, or frequency
(`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md:71-73`,
`:271-284`).

The controlling SK-V9 Lock 14 gate forbids a JSON-only primitive, byte-class
table, comparator registry, or CostFacts rule from becoming a generic default
without non-JSON proof, and requires CSS L4 / Sheets / BBNF-self proof or
per-grammar JSON-only demotion before implementation planning
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:122-143`). Alpha-E repeats that
generic CostFacts, codegen, runtime, SIMD, tape, parser-template, report, and
gate edits require the full Lock 14 proof
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:47-50`).

### F3 - No New Directive, BIR, Or Substrate Surface Is Introduced

Disposition: ACCEPT.

The P1 packet is documentation and extraction only. It does not edit grammar,
IR, codegen, runtime, or gate source, and it does not introduce a directive,
BIR variant, `BackendShape`, `UnionTape`, public substrate API, sidecar
substrate, or parser-owned cursor/fact slot.

This matches the live ceilings. Synthesis blocks new public substrate API,
directive, BIR variant, `BackendShape`, `UnionTape`, and grammar-specific role
leakage (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:128-129`), and its
pre-block list repeats the sidecar-substrate / parser-owned-slot / new-surface
ban (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-328`). HANDOFF carries the
same boundary (`restart/skinny/tranches/sk-v9/HANDOFF.md:95-99`). Alpha-C and
Alpha-E keep REDRESS 92 / SC-6-L1-R1 routed unless a later plan proves one
substrate, no new directive/BIR/API, and same-wave production consumption
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:111-131`,
`:223-234`, `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:187-191`,
`:225-232`).

PASS-1 substrate context also supports this read: tape/direct/value stays one
substrate with direct values borrowing from tape identity
(`restart/audit/pass-1-substrate/PASS-1.md:54`), PASS-2 may not introduce new
BIR variants (`restart/audit/pass-1-substrate/PASS-1.md:57`), the V1 directive
set is complete (`restart/audit/pass-1-substrate/PASS-1.md:261`), and future
grammar onboarding must not require a generic-crate registry edit
(`restart/audit/pass-1-substrate/PASS-1.md:275-283`). P1-E correctly routes tape
symbols as substrate rather than as a separable producer
(`restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md:265-270`).

### F4 - W0 Gap Routing Preserves Lock 14

Disposition: ACCEPT.

All six P1 artifacts route missing fresh profiles, PMU counters, sidecar
freshness, and SK-V9-open deltas to a W0 telemetry-lock gap rather than
inventing evidence. That is Lock 14-preserving because the live contract makes
the SK-V9-open telemetry/gate refresh gate-only and behavior-frozen: it cannot
move throughput cells, admit Apache/CITM measured rows, or alter parser,
scanner, SIMD, asm, codegen, generated output, or product behavior
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-52`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:116-117`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:50-65`).

Alpha-E's W0 refresh requires same-wave `gate-json` production and consumption,
keeps W0 negative tests, rejects producer-only telemetry, rejects run-id/input
drift, and forbids strict admission from deferred/view-boundary rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:447-475`).
The telemetry binding also says `gate-json` is the JSON instance of a
grammar-aware report contract, and generic report/gate code must not encode
JSON comparator policy as the universal schema
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-248`). Therefore the P1 W0 gap
route preserves Lock 14 if folded under that exact report/gate boundary.

## Defects

No critical CH2 defect.

Non-blocking fold defects:

1. P1-E's "source-supported eligible classes" could be misread as hot-leaf
   attribution if the consolidation drops the `GAP:not-classified` qualifier.
   This would let JSON-generated function names become S-P2 primitive
   antecedents without fresh SK-V9-open symbol/self-time evidence.
2. P1-D's W0 telemetry-lock checklist is written in the JSON row vocabulary.
   If copied into generic report/gate code without the Synthesis Section 4.3
   grammar-aware boundary, it would become a Lock 14 leak.
3. PMU/cycles fields in P1-D are report/gate telemetry requirements only. They
   must not be folded as a public substrate API, parser-owned facts, or a new
   CostFacts default.

## Required Folds

1. In the S-P1 consolidated hardening, state that P1-E source eligibility is not
   hot-leaf attribution. S-P2 may consume only fresh SK-V9-open symbol plus
   self-time plus file:line attribution, and the primitive name must be
   grammar-neutral.
2. Fold the W0 telemetry-lock gap under the existing SK-V9 Alpha boundary:
   `gate-json` is the JSON instance of a grammar-aware report contract; generic
   report/gate code must not encode JSON comparator policy as universal schema.
3. Keep W0 refresh gate-only: same-wave produced and consumed by `gate-json`,
   no throughput movement, no Apache/CITM measured-row admission, no behavior
   change, and no strict admission from deferred/view-boundary rows.
4. Preserve the no-new-surface ceiling verbatim: no new directive, BIR variant,
   `BackendShape`, `UnionTape`, public substrate API, parser-owned cursor/fact
   slot, sidecar substrate, or `tape_vs_tape` production consumer.

## Blockers

No CH2 blocker to S-P1 consolidation remains after the folds above are carried
into the consolidated hardening record.
