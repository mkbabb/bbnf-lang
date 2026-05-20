# SK-V12 Pass Alpha - Alpha-D Validated / Invalidated Ledger

Pass: Pass Alpha.
Agent: alpha-D.
Date: 2026-05-20.
Scope: SK-V11 -> SK-V12 validated / invalidated / demoted / still-open ledger.
Output: this file only.

## Contract Boundary

Alpha-D carries forward the prior skinny cycle's validated wins, invalidated
hypotheses, demotions, and unresolved axes for the next skinny contract. The
source authority is the SK-V11 measured close: `G-W9-CLOSE-SK-V11` passes as a
measured fixpoint under REDRESS 120, not as overall direct `GO` and not as a
grammar-generalization admission.

The current measured surface is unchanged from the SK-V11-open W0 capture:

| Family | Close state | Authority |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | `skinny/RESULTS.md:5-44`, REDRESS 120 |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | `skinny/RESULTS.md:6-45`, REDRESS 119/120 |
| `real_typed_struct` | 7 `A / GO` | `skinny/RESULTS.md:7-31`, REDRESS 120 |
| Overall | `N-direct / NoGo` | `skinny/RESULTS.md`, REDRESS 120 |

SK-V12 must read this as a measured-fixpoint ledger. Direct residual rows are
exhausted inside SK-V11 absent new material evidence; the non-JSON generated
baseline remains the primary unresolved axis; typed/direct admitted guards must
be held.

## Commit Anchors

| Evidence | Commit | Classification |
|---|---|---|
| SK-V11 W0 opening telemetry lock | `9c8da194` | baseline authority; no row admission |
| SK-V11 W1a non-JSON gate/report lane | `be45d32b` | validated non-admitting evidence lane |
| SK-V11 W1b generated CSS L4 baseline rejection | `5dba63aa` | invalidated generated non-JSON baseline route |
| SK-V11 W2 generated CSS L4 intervention block | `c8b8c1b4` | blocked because no W1b baseline existed |
| SK-V11 W3 numeric direct slot rejection | `85d15ddf` | invalidated numeric direct helper route |
| SK-V11 W4 container-tail direct helper rejection | `1f2df230` | invalidated generated dispatch / byte-set route |
| SK-V11 W5 bounded string span block | `581121a3` | blocked before source dispatch |
| SK-V11 W6 escaped segment digest fold block | `121eb557` | blocked before source dispatch; reopened prior sink route |
| SK-V11 W7 output digest host-sink block | `ebf16418` | blocked before source dispatch |
| SK-V11 W8 direct residual fixpoint | `eca0eb94` | direct residual rows exhausted in SK-V11 |
| SK-V11 W9 close and Alpha feedback | `db2c999b` | measured fixpoint close; SK-V12 routed remainder |

Prior banked wins that still carry into SK-V12 include SK-V9 typed admissions
(`54c00ec7`), SK-V10 direct/typed admissions (`a25ab5ce`, `5379b0e6`,
`c16cc915`), the strict comparator-plane repair (`ed923615`), and the parse-only
firewall / W3 retirement route (`2ab8d707`, `020b8e4c`, `8b9c8aef`,
`4eb259d8`). SK-V11 did not add a behavior row movement commit.

## Validated Ledger

### V1 - Typed Product Plane Remains Banked

The typed product plane carries forward as seven guarded `A / GO` rows. These
are product-plane wins, not direct-digest proof, and must remain maintain gates
for SK-V12.

| Corpus | Track 1 Mbps | Track 2/oracle Mbps | sonic typed Mbps | Strictness | Evidence |
|---|---:|---:|---:|---|---|
| `twitter` | 17740 | 15912 | 15010 | deferred | `skinny/RESULTS.md:7` |
| `citm_catalog` | 30539 | 17675 | 20726 | deferred | `skinny/RESULTS.md:10` |
| `apache_builds` | 8478 | 6892 | 8106 | deferred | `skinny/RESULTS.md:15` |
| `github_events` | 11871 | 12275 | 12224 | strict measured-row | `skinny/RESULTS.md:18` |
| `update_center` | 11851 | 10358 | 12467 | deferred | `skinny/RESULTS.md:21` |
| `mesh` | 9403 | 7897 | 8923 | deferred | `skinny/RESULTS.md:24` |
| `marine_ik` | 11788 | 10096 | 9010 | deferred | `skinny/RESULTS.md:31` |

Carry-forward rule: SK-V12 may not admit typed work from direct digest evidence.
Any new typed admission needs generated Track 1 typed output, independent Track
2/oracle, sonic-rs typed, serde_json typed, checksum parity, same-run measured
evidence, and gate consumption.

### V2 - Direct Guard Surface Remains Banked, But Narrowed To Four Rows

The current SK-V11 close has four direct `A / GO` rows. These are the direct
guard rows SK-V12 must hold.

| Corpus | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | Strictness | Evidence |
|---|---:|---:|---:|---|---|
| `citm_catalog` | 18563 | 17787 | 15530 | deferred | `skinny/RESULTS.md:9` |
| `apache_builds` | 11254 | 10189 | 10995 | strict measured-row | `skinny/RESULTS.md:14` |
| `marine_ik` | 8938 | 9437 | 8473 | deferred | `skinny/RESULTS.md:30` |
| `unicode_basic` | 2299 | 2227 | 2353 | deferred | `skinny/RESULTS.md:41` |

SK-V10's `instruments/direct_to_struct` and `numbers/direct_to_struct` cannot
be carried as current SK-V11 admitted rows. The SK-V11 W0/REDRESS 119 authority
classifies `instruments` as W0-clamped despite numeric floor clearance and
classifies `numbers` as residual because Track 2 misses its floor. SK-V12 must
not re-admit either row by docs-only accounting.

Carry-forward rule: direct admitted guards must be held, and any future direct
movement still needs strict same-run sonic direct evidence, generated Track 1,
independent Track 2, output-plane match, provenance, and gate consumption.

### V3 - Non-JSON Gate/Report Lane Is Validated Only As A Lane

REDRESS 111 admits the W1a companion non-JSON evidence lane. It validates a
schema/report path for `css_l4`, `sheets`, or `bbnf_self` with strict
non-admitting `S / NO-GO` semantics and source-provenance checks.

Carry-forward rule: W1a is useful infrastructure, not a generated non-JSON
baseline and not a grammar-generalization admission. SK-V12 may consume the
lane, but the first material task is still to create one real generated
non-JSON direct or typed parser baseline with an independent oracle.

### V4 - Strict Comparator And Output-Plane Guards Remain Validated

SK-V11 preserved the strict-vs-strict comparator discipline, parse-only
firewall, direct-vs-typed output-plane separation, and Track 2 independence
guards. W9 verified `skinny/RESULTS.md` did not move and the SK-V11-open gate
still reports unchanged `N-direct / NoGo`.

Carry-forward rule: no permissive comparator, absent sidecar, parse-only win,
or output-plane relabel may count as SOTA evidence in SK-V12.

## Invalidated Ledger

### I1 - Generated Non-JSON Baseline Via Existing JSON-Profiled Codegen Failed

REDRESS 112 rejects W1b's selected `css_l4/declaration_values/direct/main`
baseline. The blocker is structural: skinny codegen still routes direct and
typed emission through `json_provider::ensure_runtime_profile`, and the runtime
does not contain a generated CSS L4 grammar module. No generated CSS L4 Track 1
baseline or independent oracle was admitted.

SK-V12 implication: grammar generalization cannot be claimed by prose, by W1a's
non-admitting lane, or by a same-wave intervention that first creates its own
baseline. The generated non-JSON baseline is the primary unresolved axis.

### I2 - CSS Generated Intervention Before Baseline Is Blocked

REDRESS 113 blocks W2 before implementation dispatch because W1b did not
produce `W1b_css_baseline_mbps`. W2 could not create the first measurable
non-JSON row and then claim a generated intervention admission in the same wave.

SK-V12 implication: the first non-JSON wave should be baseline-first. Only after
a generated non-JSON baseline exists can SK-V12 target a measurable generated
non-JSON intervention threshold.

### I3 - Numeric Direct Slot Refactor Is Rejected

REDRESS 114 rejects the scalar `number_span_emit_slot` route. Pre-measurement
semantic and gate checks passed, but Criterion falsified
`mesh/direct_to_struct`: Track 1 3835 Mbps and Track 2 3614 Mbps versus the
8675 Mbps floor.

SK-V12 implication: numeric residual rows may not reopen this helper or a
renamed equivalent without a material differential beyond REDRESS 114 and fresh
profile / micro-proof evidence.

### I4 - Container-Tail Direct Dispatch / Byte-Set Route Is Rejected

REDRESS 115 rejects `container_tail_next`. Probe-first measurement falsified
`random/direct_to_struct`: Track 1 3518 Mbps and Track 2 3498 Mbps versus the
7878 Mbps floor.

SK-V12 implication: JSON container-tail dispatch should remain pre-blocked
unless a new route proves it is not the rejected scalar helper family.

### I5 - Bounded String Span Route Is Blocked Before Source

REDRESS 116 blocks W5 after CHALLENGE V2 kept malformed parity and Track 2 cost
at REVISE. No source patch was attempted and no reusable scalar proof was
admitted.

SK-V12 implication: string-span work is not banked. It cannot be used as a
direct residual repair unless a future pass supplies the missing malformed
fixtures, independent Track 2 cost mechanism, and row-gate proof.

### I6 - Escaped Segment Digest Fold Is Blocked And REDRESS-Adjacent

REDRESS 117 blocks W6. CH3 found the proposed `JsonDigestSink::*_source`
decoded-byte fold reopened REDRESS 54 with REDRESS 55/66/69 adjacency. Other
lenses required fixture, probe, guard, Track 1 / Track 2 independence, sampled
consumer, and negative x4 evidence revisions.

SK-V12 implication: decoded-byte source-method digest folds, x4 production
claims, and escaped-segment repairs remain pre-blocked unless a materially new
source route clears the REDRESS 54/55/66/69 adjacency.

### I7 - Output Digest / Host-Sink Route Is Blocked

REDRESS 118 blocks W7. All six lenses accepted the no-source entry block:
there was no legal residual row/source/consumer/oracle candidate, no generated
non-JSON host-sink baseline, and no visible-bucket cost path that could clear
both Track 1 and Track 2 floors.

SK-V12 implication: output digest/hash host-sink work has no admitted source
authority and no rejected-but-reusable scalar oracle.

### I8 - Direct Residual JSON-Only Campaign Reached Fixpoint

REDRESS 119 closes W8 as a measured direct fixpoint and admits no row. W8
selected no source intervention, no W8a split, no gate semantic change, and no
`skinny/RESULTS.md` row movement.

SK-V12 implication: direct residual rows are exhausted inside SK-V11 absent new
material evidence. They can re-enter only with a named differential beyond
REDRESS 114-119, fresh profile evidence, a proof or micro-proof tied to the
current hot leaf, same-wave consumer, and both Track 1 / Track 2 row gates.

### I9 - SK-V11 Close Is Not Direct GO Or Grammar-Generalization Admission

REDRESS 120 closes SK-V11 as measured fixpoint only. It made no behavior source,
generated runtime, benchmark body, gate semantic, or `skinny/RESULTS.md`
change. The non-JSON generated-intervention axis remains blocked by REDRESS 112
and 113.

SK-V12 implication: the next cycle starts from unresolved generated non-JSON
baseline work, not from another JSON-only micro-wave.

## Demoted Ledger

| Item | Demotion | Evidence | SK-V12 handling |
|---|---|---|---|
| Direct residual JSON rows | From live SK-V11 frontier to exhausted measured fixpoint | REDRESS 119/120 | Do not reopen without material evidence beyond REDRESS 114-119. |
| `instruments/direct_to_struct` | From prior direct banked row to W0-clamped residual in SK-V11 | `skinny/RESULTS.md:33`, REDRESS 119 | No docs-only admission; needs new behavior provenance if reconsidered. |
| `numbers/direct_to_struct` | From prior direct banked row to residual in SK-V11 | `skinny/RESULTS.md:35`, REDRESS 119 | Track 2 still misses floor; no docs-only admission. |
| W1a non-JSON lane | From possible grammar proof to non-admitting evidence lane | REDRESS 111/112 | Useful gate/report substrate only; not a generated baseline. |
| Generated CSS L4 intervention | From SK-V11 axis to blocked until baseline exists | REDRESS 112/113 | Baseline-first in SK-V12. |
| Parse-only | Diagnostic only, never SOTA close evidence | 16 `S / NO-GO`, 1 `L / NO-GO`; REDRESS 102/120 | Keep out of close target. |
| String/escape/digest families | From candidate families to blocked/rejected unless materially reframed | REDRESS 116-118 plus SK-V10 REDRESS 106-108 | No replay without new source differential and proof. |
| W3 / structural union substrate | Hard pre-block, not a hidden rescue route | REDRESS 96/97/98, REDRESS 102 | Do not reopen by renaming. |

## Direct Residual Fixpoint Table

These rows remain `N-direct / NO-GO`. REDRESS 119 is the row authority; the
table is carried forward so SK-V12 does not treat them as unexamined backlog.

| Row | Track 1 | Track 2 | sonic direct | Floor | SK-V11 proof / routed remainder |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | W5 string-span blocked; W7 digest blocked; no W8a source candidate. |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | W3 numeric route rejected on sibling `mesh`; larger Track 2 gap. |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | W5 blocked; W7 digest math cannot close both tracks. |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | W5 blocked; W7 digest route floor-insufficient. |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | W3 measured 3835 / 3614 against 8675 and was reverted. |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | W4 probe measured 3518 / 3498 against 7878; W5/W7 blocked. |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | Movemask/string-scan residual; no accepted source authority remains. |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | Numerically above floor but W0-clamped; docs-only admission pre-blocked. |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | Track 2 misses floor and row is W0-clamped; W3 numeric rejected. |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Track 2 misses floor; W6 decoded-source route blocked. |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | Unicode escape route blocked by W5/W6 and SK-V10 proof-only limits. |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | W5 string route blocked; W7 digest bucket insufficient. |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | Unicode/string route blocked by W5/W6 and prior proof-only limits. |

Explicit SK-V12 rule: this table is not a candidate list. It is a pre-blocked
fixpoint ledger unless a future pass names fresh material evidence beyond the
listed REDRESS entries.

## Still-Open Ledger For SK-V12

### O1 - Primary Axis: Generated Non-JSON Baseline

The primary unresolved axis is a real generated non-JSON direct or typed parser
baseline, preferably the smallest grammar whose oracle is independent and whose
output plane can be benchmarked without JSON policy leakage. W1a gives SK-V12 a
non-admitting report lane; REDRESS 112 and 113 prove that the generated baseline
itself is still missing.

Minimum admissible baseline:

- grammar id is `css_l4`, `sheets`, or `bbnf_self`;
- generated Track 1 exists outside `json_provider::ensure_runtime_profile`;
- independent Track 2/oracle is not a shared source;
- strictness, output plane, comparator/parity source, hot leaf, and Mbps are
  rendered by a gate/report consumer;
- no JSON-only policy is introduced in a generic crate; and
- the baseline is not counted as an intervention win in the same wave.

### O2 - Guard Hold: Typed And Direct Admissions

SK-V12 must hold the seven typed rows and four current direct rows listed above.
Typed/direct admitted guards are part of the close condition. Any SK-V12
candidate that weakens them rejects or must revert.

### O3 - Direct Residual Rows Only Under Fresh Material Evidence

The 13 direct residual rows are not the first SK-V12 axis. They may re-enter
only after the non-JSON baseline problem is solved or if Alpha/S-P1/S-P2 names
new material evidence unavailable to REDRESS 114-119. The required differential
is fresh profile data, proof or micro-proof, exact same-wave consumer,
independent Track 2/oracle, strict same-run sonic direct comparator evidence,
and both-track floor clearance.

### O4 - Aarch64 SIMD/ASM Remains Proof-First Only

Existing SIMD/ASM evidence remains proof-scoped. SK-V10 W8's hex proof is not a
new production admission, SK-V10 W9 rejected already-wired reuse, and SK-V11 W6
blocked the escaped-segment digest fold. Any SK-V12 kernel needs scalar
reference, strict parity/checkasm where applicable, same-host microbench,
feature/fallback gate, representative JSON and non-JSON slices, and a same-wave
consumer.

### O5 - Structural / Parse-Only Routes Stay Pre-Blocked

REDRESS 96/97/98 and REDRESS 102 remain binding. Parse-only rows are diagnostic,
and W3/union/event/class-column/streaming-cursor/class-lane/sidecar substrates
cannot be reopened by renaming. This includes any attempt to count permissive or
parse-only comparator wins as SOTA evidence.

## Alpha-D Recommendation To Alpha-F / S-P1

1. Make the SK-V12 first material problem the generated non-JSON baseline.
2. Preserve the seven typed `A / GO` rows and four direct `A / GO` rows as
   maintain gates.
3. Treat direct residual rows as exhausted inside SK-V11 unless fresh material
   evidence beyond REDRESS 114-119 exists.
4. Keep W0-clamped direct admission pre-blocked by docs-only accounting.
5. Keep strict-vs-strict comparator discipline and output-plane separation.
6. Do not spend another JSON-only micro-wave before the generated non-JSON
   baseline exists.

## Alpha-D Disposition

ACCEPT as SK-V12 Alpha-D input. SK-V11 closed cleanly as a measured fixpoint,
but it did not close the generated non-JSON baseline axis and did not admit any
new behavior row. The next contract should lead with baseline-first grammar
generalization while holding the typed/direct admitted guards and preserving the
direct residual fixpoint ledger.
