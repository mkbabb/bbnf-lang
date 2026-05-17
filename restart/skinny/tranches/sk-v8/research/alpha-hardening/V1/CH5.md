# CH5 Hidden Coupling Challenge: SK-V8 Alpha V1

Date: 2026-05-17.
Lens: CH5 Hidden Coupling.
Scope: Alpha A-F artifacts for SK-V8, SK-V7 W7-W9 research/plan/redress,
`skinny/REDRESS.md`, and `restart/prompts/pass-contracts/PASS-ALPHA.md`.

Overall disposition: REVISE.

The Alpha packet is materially honest about SK-V7's measured close, but the
draft SK-V8 contract still has hidden-coupling openings. The extraction and
ledger artifacts mostly preserve the single-substrate and output-plane
boundaries. The candidate shortlist and contract draft need explicit negative
gates so SK-V8 cannot admit a renamed second scanner, a sidecar producer with
no gate consumer, a Track 1 == Track 2 repeat, or a product-plane result that is
only the old digest stressor under a new name.

## CH5 Rules Applied

1. Lock 1: the structural projection is the tape, not a retained sidecar. A
   SIMD mask stream is transient only when consumed in the same parser/tape or
   SinkOnly loop. No retained aux table, StructuralIndex, EventCursor,
   ParserState cursor, density table, or second source scan may be admitted as a
   parallel substrate.
2. PASS-ALPHA same-wave consumer rule: every primitive, kernel, schema, sidecar
   manifest, profile field, or CostFacts producer must have the gate or hot-path
   consumer in the same wave. A producer-only artifact is research evidence, not
   admitted source.
3. Track 1 / Track 2 honesty: Track 1 must be generated from the grammar and
   declared host/API facts. Track 2 must be structurally independent. It may
   share input and correctness checksum, but not the generated parser, the same
   SinkOnly lowerer, or a benchmark-private helper.
4. Lock 14 / renamed-scanner honesty: generic names do not make a JSON scanner
   or JSON policy generic. The W7/W8 cleanup only holds if JSON policy remains
   confined to grammar input, generated JSON output, and declared host/API
   schema facts.
5. Product-plane gates: `real_typed_struct` is a typed output-plane claim only.
   It is not parse-only proof, and it is not a direct-digest close. Product rows
   require explicit host/API schema facts, generated Track 1, independent
   Track 2 or strict same-plane comparators, and no skip-only or digest-stressor
   substitution.

## Disposition Matrix

| Artifact | Disposition | CH5 finding | Required fix |
|---|---|---|---|
| Alpha-A results extraction | ACCEPT | It reports the current output planes, deferred strictness, missing sidecar values, and Track 1/Track 2 rows without collapsing them. It does not propose a new substrate. | Preserve its caveats in the final contract: parse rows remain borrowed-view-vs-DOM, direct rows remain digest guard rows, and typed rows remain typed direct product rows. Do not promote any Alpha-A row across planes. |
| Alpha-B competitor deltas | ACCEPT | It distinguishes same-run sonic/serde rows from C++ sidecar values and marks lossy/permissive rows as flaw probes. This avoids sidecar-as-anchor dishonesty. | The final SPEC must keep sidecar values as planning signals until a same-run manifest plus `gate-json` freshness consumer exists. No SK-V8 wave may count stale simdjson/yyjson/RapidJSON/asmjson sidecars as strict anchors. |
| Alpha-C redress digest | ACCEPT | It correctly pre-blocks the side-table, EventCursor, structural cursor, sink-local string materializer, W5 StringBlock16, W6 value-byte, PMULL, and CTZ/bulk routes. | Carry the pre-block list verbatim into SPEC/HANDOFF. A reopened route must state why it is not REDRESS 50, 53, 54, 55, 72, 82, 83, 84, 88, or 89 under a new name. |
| Alpha-D validated/invalidated ledger | ACCEPT | It demotes typed rows to product-plane wins, demotes B6 to canary hardening only, and keeps Lock 14 work non-performance. | Preserve the demotions. Do not use W7/W8 neutrality, W9 CostFacts, or W10c B6 as throughput evidence. |
| Alpha-E candidate shortlist | REVISE | Candidates 1-3 are salvageable with stronger negative gates. Candidate 4 is not acceptable as written because it permits a benchmark-only asm artifact and a density-gated side predicate that could become a sidecar producer. | Apply the candidate-specific fixes below before Alpha consolidation. |
| Alpha-F contract draft | REVISE | The draft has the right cautionary posture, but W1 product gates, W2 parse ownership, W3 digest/product separation, and W0 telemetry consumers are not structurally tight enough. | Apply the Alpha-F fixes below before G-Alpha. |

## Alpha-E Candidate Dispositions

### E1. Twitter yyjson residual fusion-quality retained parser refactor

Disposition: REVISE.

This candidate has a same-wave retained parse consumer and explicitly excludes
PMULL, CTZ, and sidecar-only helpers. That is the right shape. The hidden
coupling risk is that "fusion-quality" can become REDRESS 51/53 under a new
name: a second scanner, cursor, or mask producer in front of unchanged
source-byte recursive descent.

Required fixes:

- Move this candidate behind the W0 profile lock. It may not dispatch until W0
  names the exact hot leaf and proves the candidate consumes that leaf rather
  than adding a second scan.
- Add a hard negative gate: no `StructuralIndex`, `Vec<JsonEvent>`, retained
  aux column, parser-owned structural cursor, whitespace bitmap sidecar,
  density table, or second source-byte scan.
- If `scan.rs` is touched, the scanner output must be consumed in the same
  loop that writes the tape/event stream or SinkOnly output. A scan result
  cached for later parser use is a Lock 1 failure.
- The wave evidence must include before/after profile attribution showing that
  the named hot leaf moved. Throughput movement alone is not enough to prove
  the route is not a renamed scanner.

### E2. RESULTS schema completion and sidecar freshness gate

Disposition: REVISE.

Telemetry completion is admissible only if the new metadata is consumed by the
gate. Alpha-E already says `gate-json` must consume the schema fields; the
final contract must make that structural, not aspirational.

Required fixes:

- Any sidecar manifest, profile artifact, build identity, hardware identity,
  or freshness field emitted in W0 must be validated by `gate-json` in the same
  wave.
- `gate-json` must reject stale or malformed sidecar manifests in at least one
  focused test.
- The flagged sidecar values must remain marked `sidecar` unless they are
  same-run under the manifest rule. A manifest cannot relabel old data as a
  strict anchor.
- W0 may not change parser, scanner, codegen, SIMD, or asm behavior. If any
  throughput cell moves beyond the telemetry-only budget, split the wave.

### E3. Remaining Lock 14 template-residue boundary audit and relocation

Disposition: REVISE.

The audit is aligned with W7/W8, but relocation can hide JSON coupling behind a
generic provider name. That would be a renamed-scanner/renamed-template version
of the same Lock 14 problem.

Required fixes:

- If no production generic residue remains, land only the audit and grep-backed
  gate. Do not churn source to create motion.
- If relocation is real, the receiving provider must be explicitly
  per-grammar. Generic codegen may consume grammar-derived facts, not a
  generic module that embeds JSON parser policy under neutral names.
- Add a grep gate for renamed JSON policy, not just old symbol names. The gate
  must cover template includes, renderer rosters, schema-direct allowlists,
  emitted `Json*` names in generic paths, and grammar-specific public APIs.
- Generated JSON output and `skinny/RESULTS.md` must stay byte-identical.

### E4. Bitmap asm bodies under changed density-gated measurement framing

Disposition: REJECT as written.

The candidate is too permissive for CH5. It allows an asm body to remain as a
"benchmark-only artifact" if the target parse row does not improve, and it
allows a density predicate without saying whether that predicate is live scan
state or a sidecar producer. W10 and W10b already proved that correct primitive
bodies can lose whole-report JSON throughput. CH5 cannot admit a producer-only
asm body or a sidecar density classifier.

Required replacement framing:

- No production source for PMULL, CTZ, bulk emit, or a density selector lands
  unless the same wave wires it into a production JSON scan consumer and passes
  the full row gate.
- A checkasm-only or benchmark-only body may exist only as rejected research
  evidence. It must not be admitted source.
- The density predicate must be derived from live state already computed by the
  canonical scan/tape path. It may not pre-scan the input, allocate a density
  table, retain per-stripe metadata, or feed a later parser sidecar.
- The scalar path remains default outside the predicate. The gate must prove
  both selected-density improvement and rejected-density fallback.
- If the parse row target fails, revert runtime selection and source body
  changes together, then record REDRESS. Do not keep an orphan primitive.

## Alpha-F Required Revisions

### F1. W0 telemetry must be a consumed gate

Disposition: REVISE.

Alpha-F correctly makes W0 profile-bound, but it must close the sidecar
producer hole.

Required text:

```text
W0 admits only if every new telemetry field is consumed by `gate-json` in the
same wave. A profile artifact path, sidecar manifest, CostFacts id, run id, or
freshness field that is emitted but not validated is a producer-only artifact
and fails W0. W0 must include at least one malformed-manifest rejection test.
```

### F2. W1 typed product plane must define structural independence

Disposition: REVISE.

Alpha-F says "Track 1 is generated and Track 2/oracle is structurally
different", but "oracle" is too loose. It could permit Track 1 and Track 2 to
share the same generated DirectBuild engine and only compare checksums.

Required text:

```text
For every `real_typed_struct` admission, Track 1 is generated from grammar
facts plus an explicit host/API schema. Track 2 is either an independent hand
parser, a serde/sonic typed comparator, or another named oracle that does not
call the generated Track 1 parser, generated SinkOnly lowerer, generated typed
helpers, or a shared benchmark-private parser. The row must publish symbol-path
or call-path evidence for Track 1 and Track 2. A checksum equality proves
correctness only; it does not prove independence.
```

Also add:

```text
Direct digest rows remain guard rows. They cannot be counted as typed product
plane SOTA proof and cannot substitute for real host/API typed output rows.
```

### F3. W2 parse candidate must name exact paths and ban renamed scanners

Disposition: REVISE.

Alpha-F defers W2 owner paths until after W0. That is acceptable, but the final
SPEC must not dispatch W2 with open-ended scanner/parser authority.

Required text:

```text
W2 owner paths are finalized after W0 and before implementation. If a path under
`scan.rs`, parser templates, `bbnf-simd`, or parse-that string/number scanners
is included, the plan must prove the change is consumed in the same parser/tape
or SinkOnly loop. Any second scan over the same source bytes, retained cursor,
aux table, density cache, sidecar event vector, or parser-owned structural
projection is a Lock 1 failure, even if the API names are grammar-neutral.
```

### F4. W3 direct guard triage must not become product-plane laundering

Disposition: REVISE.

The draft says W3 either closes selected `N-direct` rows or routes them as
guard residuals. CH5 needs the output-plane line stronger.

Required text:

```text
`direct_to_struct` digest closure is a digest guard outcome, not a product-plane
SOTA outcome. A W3 row may improve or close the digest guard, but the SK-V8
product close can cite only `real_typed_struct` rows generated from explicit
host/API schema facts with independent Track 2 or same-plane strict comparator
evidence.
```

### F5. CostFacts must remain evidence substrate, not route permission

Disposition: REVISE.

W9 made CostFacts a producer/carrier/consumer/exporter with no generated-output
diff. Alpha-F should preserve that exact role.

Required text:

```text
CostFacts records chosen shapes, rejected alternatives, evidence, wave ids, and
REDRESS references. It does not by itself reopen a rejected route or change a
runtime path. Any CostFacts-selected runtime behavior change needs a separate
same-wave hot-path consumer and row gate.
```

## Blocking Defects Before G-Alpha

1. Alpha-E.4 must be removed or rewritten under the replacement framing above.
   As written, it admits a benchmark-only primitive and an underspecified
   density side predicate.
2. Alpha-F W1 must replace "Track 2/oracle" with a concrete structural
   independence rule and call-path evidence.
3. Alpha-F W2 must add a renamed-scanner Lock 1 gate before any parser or
   scanner owner path can dispatch.
4. Alpha-F W0 must say that telemetry fields and sidecar manifests are invalid
   unless `gate-json` consumes and rejects them.
5. Alpha-F W3 must state that digest guard closure is not typed product-plane
   closure.

## Acceptance Conditions For CH5

CH5 can move to ACCEPT only after the consolidated Alpha packet contains all of
the following:

- A single-substrate rule covering parser fusion, scan predicates, sidecar
  manifests, and bitmap density work.
- A same-wave consumer rule for every new producer, including telemetry,
  CostFacts, profile artifacts, primitive bodies, and schema facts.
- Track 1 / Track 2 call-path evidence for direct and typed product rows.
- A product-plane rule that separates `direct_to_struct` digest guard rows from
  `real_typed_struct` host/API schema rows.
- A pre-blocked-route rule that treats renamed REDRESS 50/53/54/55/72/82/83/84/
  88/89 shapes as rejected unless CHALLENGE accepts a structurally different
  current-baseline route.

Until those revisions land, G-Alpha must not close.
