# SK-V11 S-P3 V3 CH6: Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: V3 CHALLENGE.
Lens: CH6 anti-paper-close / next-tranche impact.
Date: 2026-05-20.
Scope: challenge whether the V3 S-P3 packet can close only on measured row
evidence, with same-wave consumers, row floors, strict comparator/oracle
binding, kernel micro-proofs, and measured fixpoint/uncloseable proofs.

## Verdict

ACCEPT.

V3 is dispatchable under CH6. It does not permit a proposed admit to close by
"wired", "integrated", proof-only, producer-only telemetry, W0 clamp, or future
wave promise. The packet binds admits to same-wave consumers, named row floors,
strict comparator/oracle evidence, micro-prove-first for kernels, and measured
REDRESS proofs for fixpoint or uncloseable rows.

## Evidence

### Contract Baseline

ACCEPT. The governing CH6 contract asks whether the plan has a revert protocol,
same-wave consumer, and pre-blocked routes
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112-125`). Its
load-bearing rule says a primitive/kernel/generated path must ship with scalar
reference, checkasm/parity, same-commit consumer, bench rows, and samply-visible
consumer path; omitted consumer is REJECT with REDRESS, no exception
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177-186`). S-P3 also
requires P3-A candidates to carry owner paths, scalar/checkasm state,
same-wave consumer, and named corpus rows plus Mbps thresholds, and P3-C to
reject unmeasurable gates (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:56-63`).
The specialized CH6 lens explicitly rejects "wired" or "integrated" without a
bench-row threshold and asks whether every candidate names its same-wave
consumer (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:140-145`).

### Accepted Inputs

ACCEPT. S-P1 converged with two consecutive all-ACCEPT cycles and fixes the
profile authority: direct residuals are the primary closure surface, W0-clamped
rows remain non-admissions until behavior measurement, diagnostic facts do not
admit rows, and non-JSON proof remains required
(`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:19-20`,
`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:34-55`).
S-P2 converged with V2 and V3 6/6 ACCEPT, leaving C1-C7 as parser primitives,
C8 as oracle/host sink only, C9 as accounting only, proof/support surfaces not
standalone row movers, W3 substrate REDRESS-closed, and non-JSON generality
measured through a generated direct/typed parser
(`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:7-33`).

### Candidate Admits

ACCEPT. P3-A states that every candidate is an intervention packet naming owner
paths, scalar-reference state, parity state, micro-prove-first state,
same-wave consumer, output planes, and row floors; rows move per-row only, not
by analogy (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:69-74`).
It drops standalone C7/C8/C9/proof-only/inventory surfaces from the admit pool
(`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:76-88`).

Candidate-specific anti-paper-close checks pass:

| Candidate | Same-wave consumer | Row floor / oracle / reject evidence |
|---|---|---|
| C1 dispatch/container | generated SinkOnly direct Track 1 plus independent Track 2/oracle (`p3a-candidate-shortlist.md:120-124`) | selected rows must clear 13403/10059/7878/10637/8675/8969 and reject on Track 2 coupling or no row floor (`p3a-candidate-shortlist.md:125-132`) |
| C2 string span | generated direct string/key paths and optional typed fields with independent same-plane Track 2/oracle (`p3a-candidate-shortlist.md:161-169`) | direct floors plus unicode residual floors when selected; reject primitive-only proof or no row floor (`p3a-candidate-shortlist.md:170-180`) |
| C3 escaped segment | new direct/typed/non-JSON escaped-segment consumer, not existing `unescape_string` wrapper (`p3a-candidate-shortlist.md:210-218`) | unicode floors plus strict x4 checkasm; reject existing-consumer reuse or no selected row floor (`p3a-candidate-shortlist.md:219-226`) |
| C4 digit span | generated direct numeric sinks, independent Track 2 numeric digest, optional typed fields (`p3a-candidate-shortlist.md:253-260`) | numeric floors 10637/8675/2425/8969 and reject Track 2 divergence or no selected row floor (`p3a-candidate-shortlist.md:261-269`) |
| C5 byte-set layout | generated direct whitespace/value entry points and generated non-JSON layout consumer if generic crates are touched (`p3a-candidate-shortlist.md:299-306`) | direct floors 13740/7878/2658/8969/10059 and reject no selected row floor (`p3a-candidate-shortlist.md:307-314`) |
| C6 non-JSON dispatch | generated non-JSON direct/typed parser benchmark row with independent oracle (`p3a-candidate-shortlist.md:342-350`) | W1b creates baseline; W2 admits only at `ceil(W1b_css_baseline_mbps * 1.01)` with strict oracle equality; reject absent W1b baseline (`p3a-candidate-shortlist.md:351-361`) |
| C7 typed guard | generated typed parser plus independent serde/oracle for the same row (`p3a-candidate-shortlist.md:383-388`) | typed guard floors 17385/29928/8308/11633/11613/9214/11552; any new typed row needs same-run sonic typed strict before redress (`p3a-candidate-shortlist.md:389-398`) |

The consolidated candidate table repeats the same row-floor discipline and fixes
the V2 CH1 stale-floor issue: P3-D binds fields but does not own the non-JSON
performance floor, W1b owns the baseline, and W2 owns the 1% strict-equality
admit (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:400-412`).
P3-A also makes micro-prove-first binding for all candidates: scalar oracle,
strict parity for AArch64 product bodies, caller microbench, same-wave product
consumer, and same-wave gate consumption (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:421-430`).

### Wave Gates

ACCEPT. P3-B sequences W0, W1a, W1b, W2-W9 in 11 waves with one spare split,
keeps C9 accounting non-row-moving, and leaves proof-only/inventory surfaces
support-only unless a later wave names source delta, scalar oracle, strict
parity/checkasm, feature/fallback, same-wave consumer, and measured row gate
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:43-53`).
Its manifest makes W1a schema-only, W1b baseline-only, W2 the first generated
non-JSON intervention consuming W1b, W3-W7 row-moving/product-sink waves, W8
direct residual fixpoint by measured evidence, and W9 close after dispositions
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:64-76`).
P3-B's gate table assigns concrete row floors to W2-W8 and blocks W9 if W8
leaves any direct row without `A / GO` or REDRESS proof
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:101-113`).
It also states every primitive wave inherits scalar reference, strict
checkasm/differential where applicable, feature/fallback, same-host caller
microbench, same-wave hot-path consumer, and samply-visible consumer path;
missing consumer is REJECT, not deferral
(`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:115-119`).

P3-C makes the gate surface executable. W1a and W1b explicitly prohibit row
admission; W2 requires generated Track 1, independent Track 2/oracle, strict
equality, primitive self-time, and the W1b +1% floor; W3-W7 carry selected-row
floors and guard floors; W8 requires per-row uncloseable proof; W9 blocks when
direct rows or the non-JSON axis lack evidence
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:75-87`).
P3-C's direct table says a direct row admits only when generated Track 1 and
independent Track 2 both clear the floor under one same-run strict direct
comparator (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:106-126`).
Its track/oracle section binds direct, typed, non-JSON, SIMD/ASM, and scalar-only
admissions to same-output oracle/comparator evidence, same-wave gate
consumption, caller microbench, row gates, and guard floors
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:161-185`).
Its unmeasurable-gate rule rejects a wave before redress if it lacks named row,
threshold, generated Track 1, independent Track 2/oracle, strict same-plane
comparator/oracle, scalar reference, checkasm plan, same-wave consumer, guard
block, gate consumer, or revert protocol; wording such as "wired",
"integrated", PMU-only, parse-only, or checkasm-only cannot repair the gate
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:187-205`).

### Telemetry And Strict Oracles

ACCEPT. SPEC §0 makes the close condition measured: every residual direct row
must become strict same-run `A / GO` on generated Track 1 and independent
Track 2/oracle or receive a per-row measured REDRESS proof
(`restart/skinny/tranches/sk-v11/SPEC.md:26-32`). It separately requires one
admitted benchmarked non-JSON generated direct/typed parser intervention
(`restart/skinny/tranches/sk-v11/SPEC.md:42-44`), micro-prove-first for every
kernel/substrate-adjacent/SIMD/generic intervention (`restart/skinny/tranches/sk-v11/SPEC.md:47-50`),
strict direct/typed comparators on the matching plane (`restart/skinny/tranches/sk-v11/SPEC.md:51-53`),
and same-wave telemetry consumption (`restart/skinny/tranches/sk-v11/SPEC.md:56-57`).
SPEC required telemetry includes `same_wave_consumer_class` and
`track2_independence_status` (`restart/skinny/tranches/sk-v11/SPEC.md:81-107`),
and non-JSON rows may enter `RESULTS.md` only if the same wave updates every
consumer or writes a companion gate-consumed report with generated Track 1,
independent oracle, strict equality, run id, host, flags, samples, and no-sidecar
proof (`restart/skinny/tranches/sk-v11/SPEC.md:109-114`).

P3-D matches that by rejecting producer-only fields, stale/absent strict
anchors, strict-plane mismatches, deferred validation admission, wrong strict
comparators, unconsumed non-JSON oracles, parse-only SOTA claims, direct digest
as typed proof, W3 reopen claims, and Track 2 coupling
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:195-218`).
It states every field emitted to `RESULTS.md` must be consumed by a validator or
same-commit gate extension; there is no emit-now-consume-later route
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:219-222`).
For the non-JSON close axis, P3-D requires grammar id, workload,
comparator/oracle, Track 1, Track 2/oracle, profile artifact, strict output
proof, and same-wave consumer to be gate-consumed
(`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:257-261`).

### Kernel Micro-Proofs

ACCEPT. SPEC §2.1 requires every W2-W7 plan to record scalar reference or exact
product oracle, strict checkasm when SIMD/ASM is used, same-host microbench,
observed value/threshold/run/host/flags/samples/feature gate, same-wave consumer
path, row gate, fallback, and REDRESS-tied reject boundary before redress
(`restart/skinny/tranches/sk-v11/SPEC.md:213-227`). P3-C sets the SIMD/ASM
caller microbench floor at median `>= 1.08x` with no selected slice below
`0.99x`, while still making the production row gate decisive
(`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:176-181`).
P3-E keeps `HEX_QUARTET_X4_PROOF`, PMULL/CTZ/EOR3/BCAX/cache hints, PMU/cycles,
structural scans, lazy tape bytes, parse-only rows, CostFacts, and telemetry
schemas proof-only/inventory/evidence-consumer surfaces until a wave supplies
source delta, scalar oracle, strict parity/checkasm, caller microbench,
same-wave consumer, and row gate
(`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:92-97`).

### Fixpoint And Uncloseable Rows

ACCEPT. W8 is not a narrative close. SPEC W8 enters only after W3-W7 measured
dispositions and W2 non-JSON admission or BLOCKED route; every remaining direct
row must have a named candidate or candidate-exhaustion proof plan
(`restart/skinny/tranches/sk-v11/SPEC.md:709-711`). Its tasks admit only rows
meeting §0.4 on both generated Track 1 and independent Track 2/oracle and
record misses in REDRESS with attempted candidate, measured tracks, comparator,
floor, and guard status (`restart/skinny/tranches/sk-v11/SPEC.md:713-721`).
Its exit gate requires all §0.4 rows to be `A / GO` or backed by measured
REDRESS proof, W0-clamped provenance, guard floors, and no unaccepted source
route (`restart/skinny/tranches/sk-v11/SPEC.md:723-732`).

W9 is likewise fail-closed. It may start only after W8 closes or escalates and
every W1a-W8 wave has admitted, proof-closed, or rejected with measurement
(`restart/skinny/tranches/sk-v11/SPEC.md:752-753`). Its exit gate requires every
residual direct row to be `A / GO` or have an uncloseable proof naming attempted
intervention, Track 1, Track 2/oracle, comparator, floor, guard result, and
routed remainder; it also requires one admitted benchmarked non-JSON generated
intervention unless Close escalates `BLOCKED` for grammar-generalization
fixpoint (`restart/skinny/tranches/sk-v11/SPEC.md:755-763`). The dispatch prompt
confirms that convergence requires W1a-W8 and W9 to admit/proof-close/reject
with measurement and that close cannot waive the non-JSON benchmarked
intervention axis without a `BLOCKED` verdict
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:214-222`). That is a failure
disposition, not a paper close.

P3-E independently pre-blocks paper close, routed residuals without thresholds,
W0-clamped admission without behavior provenance, direct fixpoint without
per-row proof, missing REDRESS for failed waves, non-JSON closed by prose, and
G-Alpha presentation while any W1a-W8 wave lacks admitted/rejected/measured
status (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:202-212`).

### V2 CH6 Regression Check

ACCEPT. V2 CH6 already accepted the anti-paper-close shape: measurable gates,
non-admitting W1a/W1b, W2 consuming W1b, same-wave telemetry consumption, direct
and non-JSON evidence, and W8 source split discipline
(`restart/skinny/tranches/sk-v11/research/p3/hardening/V2/CH6-anti-paper-close.md:11-20`,
`restart/skinny/tranches/sk-v11/research/p3/hardening/V2/CH6-anti-paper-close.md:24-102`).
The V2 consolidated REVISE was not a CH6 defect; it required V3 folds for stale
P3-A typed guard floors, the P3-A C6 baseline/floor wording, and SPEC W5 Unicode
"guard" wording (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:8-20`,
`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:35-48`).
V3 folds those CH1 items: P3-A's typed guard floors now match the P3-C/SPEC
authority (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:54-67`),
C6 now states W1b creates the concrete non-JSON baseline and W2 owns the 1%
strict-equality performance floor (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:351-357`,
`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:411-412`),
and SPEC W5 now treats Unicode rows as residuals unless selected, not admitted
guards (`restart/skinny/tranches/sk-v11/SPEC.md:566-577`).

## Residual Notes

- W1a and W1b remain non-admitting prerequisites. That is acceptable because
  their gates fail closed and cannot move rows.
- W7 C8 is only a product host-sink/oracle route. It cannot close parser
  semantics or enter generic parser crates.
- A `BLOCKED` grammar-generalization fixpoint is not an ACCEPT close; it is an
  escalation state that prevents silent waiver of the non-JSON axis.

## File Changed

- `restart/skinny/tranches/sk-v11/research/p3/hardening/V3/CH6-anti-paper-close.md`
