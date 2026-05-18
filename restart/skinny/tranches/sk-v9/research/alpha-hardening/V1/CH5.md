# CH5 - Hidden Coupling Review

Verdict: ACCEPT

Confidence: 96%

## Scope

CH5 reviews the SK-V9 Alpha artifacts for hidden coupling: parallel substrate,
sidecar producer, renamed-scanner Lock 1 violation, Track 1/Track 2 dishonesty,
typed source/product ambiguity, direct-vs-real-typed laundering, and performance
claims from digest or proxy evidence. This is the PASS-ALPHA CH5 lane defined in
`restart/prompts/pass-contracts/PASS-ALPHA.md:35-49` and
`restart/prompts/ORCHESTRATOR.md:81-88`.

## Findings

### CH5-1 - Typed source/product rows are not laundered into measured rows

Disposition: ACCEPT.

Alpha-A and Alpha-D preserve the load-bearing distinction: the measured SK-V8
authority is the W0 `skinny/RESULTS.md` table with four measured
`real_typed_struct A / GO` rows, while Apache/CITM are W2 source/product parity
only (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:66-73`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:93-112`).
REDRESS 91 confirms the same boundary: Apache/CITM are not measured rows in the
current W0 manifest, W2 leaves `skinny/RESULTS.md` unchanged, and W2 does not
claim six measured real-typed rows (`skinny/REDRESS.md:2622-2657`).

The SK-V9 contract carries that boundary forward. Section 0.2 requires fresh
measured-row evidence before Apache/CITM can count, and Section 4.1 names their
current state as source/product parity absent from `RESULTS.md`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:69-75`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:161-167`). The Alpha-E typed
candidate also blocks source-only admission and keeps `canada/real_typed_struct`
routed until full-fixture parity exists
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:35-41`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:75-95`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:107-114`).

Required fold: none.

### CH5-2 - Direct digest rows are kept separate from real typed product rows

Disposition: ACCEPT.

The current table distinguishes planes in the measured rows: direct rows are
`digest`, while real typed rows are `typed direct`
(`skinny/RESULTS.md:6-7`, `skinny/RESULTS.md:9`, `skinny/RESULTS.md:18`,
`skinny/RESULTS.md:21`, `skinny/RESULTS.md:27-28`,
`skinny/RESULTS.md:38`). Alpha-A calls the three current direct GO rows
digest-plane guard rows, not product-plane typed rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:75-90`).
REDRESS 93 makes the same direct boundary binding: remaining direct misses route
to a later direct-output-contract or control-path tranche, and digest evidence is
guard-plane only, not product proof (`skinny/REDRESS.md:2694-2729`).

The SK-V9 contract and shortlist do not promote digest rows into typed rows. The
cycle close condition forbids treating direct digest as product proof without a
direct output contract or control-path tranche
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:76-80`), and Alpha-E's direct
candidate explicitly requires the signal to remain guard/control-path if the
plane stays `digest`; a digest-only row cannot satisfy the typed-product escape
hatch (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:209-215`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:251-271`).
Alpha-F also tells challenge to reject any draft that treats direct digest as
product proof (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:77-89`).

Required fold: none.

### CH5-3 - Track 1 and Track 2 independence is preserved

Disposition: ACCEPT.

The current W0 report states that Track 1 is
`runtime::generated_json::parse` while Track 2 is an independent hand-coded
parser over `runtime::tape`; the Track 2 checklist says it never calls Track 1
(`skinny/RESULTS.md:138-141`). Existing typed rows also describe Track 2 as a
structurally different oracle, not the SOTA gate (`skinny/RESULTS.md:7`,
`skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).

The Alpha-E candidates keep that separation. Typed row-table admission requires
checksum parity across generated Track 1, serde_json Track 2/oracle, and the
sonic typed lane, while Track 2 remains structural-oracle evidence rather than a
SOTA speed floor
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:55-61`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:75-91`).
Direct control work requires stable Track 1/Track 2/report identities and
prevents coupling Track 2 to generated SinkOnly or generated Track 1
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:231-236`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:256-267`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:284-291`).
The SK-V9 telemetry schema also makes Track 2 independence status required
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:231-235`).

Required fold: none.

### CH5-4 - No parallel substrate or renamed scanner is authorized

Disposition: ACCEPT.

REDRESS 92 remains the hard boundary for structural parse work: W3 rejected the
Tier A route because scanner structural positions and retained tape events were
not isomorphic, and it explicitly did not reopen sidecar producers,
parser-owned structural cursors/facts, `tape_vs_tape` as production consumer,
`UnionTape`, new `BackendShape`, new BIR, new directive, or public substrate API
(`skinny/REDRESS.md:2661-2690`). The SK-V8 SPEC's non-negotiables match that
boundary: no new directive, BIR, `BackendShape`, `UnionTape`, substrate surface,
public substrate API, parser-owned structural facts, parallel substrate, or
sidecar substrate (`restart/skinny/tranches/sk-v8/SPEC.md:191-208`).

The SK-V9 Alpha artifacts preserve the same block. Alpha-C pre-blocks sidecar
producers, parser-owned facts, aux tables, `UnionTape`, new `BackendShape`, BIR,
directive, and public substrate API, and requires any future route to replace
scalar rediscovery inside one retained tape
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:109-129`).
Alpha-E's retained grammar candidate requires the class/event grammar to account
for structural classes without a second tape, sidecar, `UnionTape`, new
`BackendShape`, BIR variant, directive, or public substrate API
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:156-170`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:200-207`).
The SK-V9 contract repeats those blocks in the pre-blocked route list
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-258`).

Required fold: none.

### CH5-5 - No candidate claims performance from digest, sidecar, CostFacts, or other proxy evidence

Disposition: ACCEPT.

Alpha-B classifies competitor deltas as planning evidence unless strictness,
freshness, and output-plane compatibility are satisfied; it explicitly excludes
lossy, historical sidecar, absent sidecar, and parse DOM-vs-borrowed-view plane
mismatches from strict wins
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-39`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:116-144`).
Alpha-C likewise says CostFacts and comparator ids are evidence substrate, not
performance proof, and it blocks strict admission from telemetry-only rows,
CostFacts-only evidence, and `tape_vs_tape` production consumption
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:46-55`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:219-231`).

Alpha-E's sidecar manifest candidate is telemetry/report gating, not behavior:
sidecar cells require a structured manifest, DOM sidecars cannot admit digest or
typed-direct rows, and parser/scanner/generated throughput cells may not move
unless a later separate behavior wave is dispatched
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:293-300`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:335-353`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:364-371`).
The W0 telemetry refresh candidate is also behavior-frozen and forbids source or
product behavior drift
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:373-443`).

Required fold: none.

## Required Folds

None for CH5. The Alpha artifacts already carry the hidden-coupling guardrails
that this lane requires.

## Blockers To G-Alpha

None from CH5. G-Alpha may proceed from this lane after the full Alpha
challenge/consolidation requirements are satisfied. This ACCEPT does not waive
other challenge lanes or the user-controlled G-Alpha boundary in
`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182` and
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:49-65`.
