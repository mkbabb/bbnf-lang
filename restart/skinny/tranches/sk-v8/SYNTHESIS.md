# SK-V8 Grand Synthesis

Date: 2026-05-17.

Authority:

- `skinny/RESULTS.md` after SK-V7 W10c.
- `skinny/REDRESS.md` items 77-90.
- SK-V7 commit chain through `56e66ef5 feat(sk-v7-wave10c): admit B6 stack-canary Stage 1`.
- Alpha cohort under `restart/skinny/tranches/sk-v8/research/alpha/`.
- Alpha hardening V1 under
  `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/`.

This synthesis is the post-SK-V7 Pass Alpha output for the SK-V8 tranche. It is
not an SK-V8 implementation dispatch by itself. G-Alpha user sign-off is
required, and that sign-off authorizes only W0 unless the user explicitly signs
off on later post-W0 plan augmentations.

## Measured Opening State

The current gate authority is `skinny/RESULTS.md`. Its own close note says the
overall outcome remains `N-direct / NoGo`.

Current main-table state:

| Family | Current state |
|---|---|
| `parse_only` | 17 rows, all `K / NO-GO` |
| `direct_to_struct` | 6 `A / GO`, 11 `N-direct / NO-GO` |
| `real_typed_struct` | 4 `A / GO` |

Every current main row records `Strictness=deferred`,
`parse_utf8=view-boundary`, and `escape_complete=yes`. Parse rows use the
output plane `borrowed view over offset tape vs DOM`; direct rows use `digest`;
real typed rows use `typed direct`.

The schema-v3 surface exists, but its evidence is incomplete for prescribing
new kernels. `Delta vs SK-V6` is non-derivable in every current main row, and
the `Hot leaf` column says `unprofiled in W0b; no kernel prescription from this
row`. SK-V8 therefore starts by making row evidence executable rather than
guessing from stale profiles.

## Corrected SK-V7 Diagnosis

SK-V7 produced useful architecture work, but the original close forecast did
not hold.

Validated SK-V7 outcomes:

- W0 `ed923615` admitted strict sonic-rs comparator repair, recorded by
  REDRESS 77.
- W0b `0d2fab3f` admitted schema-v3 telemetry row construction, recorded by
  REDRESS 78.
- W1 `89f29768` admitted the descriptor-preserving TapeKind rename, recorded by
  REDRESS 79.
- W3 `41ecf187` admitted capacity-hinted numeric Vec real-typed expansion,
  recorded by REDRESS 81. Current `mesh/real_typed_struct` is `9466` Track 1
  Mbps versus `8696` sonic strict, and `marine_ik/real_typed_struct` is
  `12020` Track 1 Mbps versus `8750` sonic strict.
- W7 `f786e597` and W8 `7c6837b8` admitted Lock 14 neutralization phases,
  recorded by REDRESS 85 and 86.
- W9 `51d8c8be` admitted the CostFacts substrate projection, recorded by
  REDRESS 87.
- W10c `56e66ef5` admitted B6 stack-canary Stage 1 only, recorded by REDRESS
  90.

Rejected or demoted SK-V7 outcomes:

- W0 strict comparator repair did not flip the forecast rows. REDRESS 77 says
  `instruments` and `unicode_basic` stayed W0-era `G / NO-GO`; current
  schema-v3 rows classify both as `K / NO-GO`.
- W2 `78d83497` rejected the zero-fallback mantissa-widen assumption; REDRESS
  80 found no current canada fallback pool to eliminate.
- W4 `17bd39b1` rejected the single-quartet Unicode escape classifier;
  REDRESS 82 records correctness-green but row-failing measurements.
- W5 `db761873` rejected the generated-retained StringBlock16 tiny probe;
  REDRESS 83 records regressions on the six named parse rows.
- W6 `58479e29` rejected object-pair value-byte control compaction; REDRESS 84
  records the missed citm/instruments thresholds.
- W10 `db913136` rejected consumed AArch64 bitmap bodies; REDRESS 88 records
  PMULL prefix-XOR as a default production hot body regressing JSON rows.
- W10b `0cd00886` rejected CTZ/bulk production consumption; REDRESS 89 records
  six Track 1/2 row drops over the maintain invariant.

The original W10 bitmap body-fill gate was not green. SK-V7 closes honestly:
B6 Stage 1 landed, while PMULL and CTZ/bulk remain rejected for SK-V7 and
routed only as reserve evidence for a future, profile-proven frame.

## Substrate-Ceiling Finding (S-P2 Cohort)

A new-lens skinny S-P2 research cohort — six agents SC-1 through SC-6 under
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/` — interrogated a
single hypothesis: that no SK-V7 micro-kernel could win because the offset-tape
substrate itself imposes an irreducible per-structural-element cost. The cohort
converged, independently, on a sharper and more damning finding.

The offset-tape is not a ceiling by write amplification. SC-1 found that
allocated-tape-byte amplification anti-correlates with loss — the highest-ratio
corpora (canada 0.47x, mesh 0.72x, marine_ik 0.70x) are exactly the corpora
bbnf wins by roughly +50%, and the payload arena is provably untouched (`0/0`
writes on every corpus).

The real defect is structural duplication. bbnf already produces a SOTA-grade
stage-1 SIMD structural index — `scan_structurals` runs near 69 Gbps — and then
discards it: `attach_structural_index` is a no-op, and the index `Vec<u32>` is
used only to size an allocation. The generated recursive-descent parser then
re-discovers every structural byte and every string boundary itself, in a
scalar pass whose hot leaves (`consume_structural`,
`match_tiny_plain_string`/`match_string_at_quote`) account for roughly 75% of
self-time on the loss corpora. bbnf is a two-pass parser whose fast pass is
dead code; it pays for structural scanning twice.

This explains every SK-V7 rejection at once. W2, W4, W5, W6, W10, and W10b each
optimised the scalar rediscovery pass — a pass that should not exist. SC-4
showed a strong JSON planning signal in string-quote density, but V3 demotes the
"knee" to diagnostic telemetry. Same-run strict evidence must stay separate
from simdjson/yyjson sidecars and historical SK-V6 rows, and JSON quote-count
share is not a generic selector policy. No kernel closes the string plane under
a substrate that re-discovers string bounds per element, but any string-boundary
admission gate still needs a later executable row set, formula, numeric target,
maintain budget, and pass/fail rule.

The fix is subtractive, not additive. SC-2 and SC-3 designed the union the
original SK-V8 brief named: retain the stage-1 SIMD scan output and add a
co-allocated, index-aligned structural-class column produced from the same
branch-free classify mask. V3 splits this into Tier A and Tier B. Tier A is the
S-P3-ready structural-class cursor migration: the parser consumes scan-written
opaque class ordinals from the single retained `Tape`, deleting structural
rediscovery while preserving existing scalar/string-boundary behavior. Tier B is
the separate string-boundary / quote-backslash-parity / CostFacts-template
union. The structural projection IS the tape only when it replaces the old
offset append path; if it runs beside that path, it is a sidecar and fails Lock
1.

SC-5 adjudicated the uniform `parse_only` K-classification: it is partly correct
because bbnf's offset-tape emit plane is not a DOM builder, and partly
overloaded because one enum hides strict residuals and strict guard wins. V3
corrects the comparator columns: twitter is -25.1% versus same-run sonic strict
and -35.8% versus simdjson DOM; citm_catalog is +24.6% versus same-run sonic
strict and -11.3% versus simdjson DOM. The adjudication is to retire
`parse_only` from the SOTA scoreboard, keep the 17 rows as substrate-guard
non-admission telemetry (`K` now, `S` if W0 amends the schema), add
executable strict-admission refusal rules, and route `tape_vs_tape` only to
W0/W1 gate-binding telemetry. `tape_vs_tape` is not a W3 production same-wave
consumer.

SC-6 confirmed the union satisfies Lock 1 only if it replaces the offset-tape
rather than running alongside it; the deciding test is substrate cardinality. It
proposes one Lock 1 refinement (SC-6-L1-R1, a Pass Omega candidate) promoting
the structural projection from transient mask stream to retained substrate while
keeping the parallel-sidecar case forbidden. V3 removes any `UnionTape` node
option: the admitted fold is representation replacement of `OffsetTape` and
retained `EventTape`, not a new substrate node, `BackendShape`, BIR variant,
directive, or public substrate type. Grammar neutrality is via generated byte
sets plus opaque structural-class ordinals and opaque fact ids; event-role,
recovery, layout, record-boundary, and indentation meaning lives only inside
generated grammar modules keyed by parser state plus class/byte.

## SK-V8 Thesis

SK-V8 is an observability-bound tranche before it is a performance tranche.

The main failure mode in SK-V7 was not lack of clever kernels. Several correct
kernels or plausible local rewrites lost whole-row throughput. The current
RESULTS surface has enough strict comparator data to identify residual rows,
but not enough hot-leaf and per-row attribution to prescribe another parser
intervention.

The S-P2 substrate-ceiling cohort named the leading structural hypothesis ahead
of W0. W0 telemetry still stands: it confirms or falsifies the finding
executably, with run ids, profile artifacts, and per-row hot-leaf columns rather
than a research claim. The lead W3 hypothesis is the tape ⊕
structural-projection union, but V3 makes the candidate narrower: Tier A wires
the discarded stage-1 index into the retained parser cursor and deletes
structural rediscovery; Tier B owns string-boundary / quote-backslash-parity /
CostFacts-template closure. That hypothesis is not selected by S-P2; W3 still
requires W0/W1 closure, a fresh S-P3/W3 plan, exact owner paths, same-wave
production consumer, revert protocol, measurement thresholds, measured-path
validation proof, and challenge acceptance.

The SK-V8 thesis:

1. Create a trustworthy `SK-V8-open` baseline with hot leaf, profile artifact,
   cycles-per-byte or equivalent sample cost, run id, CostFacts ids, and
   grammar-aware comparator metadata.
2. Bind CostFacts into the gate before behavior waves can claim route quality.
3. Prefer generated typed product-plane expansion where host/API schema facts
   exist, and keep direct digest rows as guard rows.
4. Allow parse or direct behavior work only after W0 evidence names exact owner
   paths, hot leaves, thresholds, and pre-block differences.
5. Preserve Lock 14 and Lock 15 on every wave.

Generic-crate changes must carry non-JSON proof. JSON row data is the opening
benchmark surface, not proof that a change is grammar-general. CostFacts,
codegen, runtime, SIMD, or parser-template edits must show that CSS L4, Sheets,
and BBNF-self do not need JSON structural roles to compile, lower, cost, or
run. The S-P2 fold requires any structural alphabet to use generated
per-grammar byte-set tables plus fixed neutral structural-role ordinals; grammar
meaning stays in generated grammar modules, not generic substrate code.

## Comparator Posture

SK-V8 uses three comparator classes:

- Same-run strict anchors: sonic-rs strict and serde_json where populated by
  the current gate.
- Same-run flaw probes: sonic-rs lossy and any permissive row.
- Sidecar planning signals: simdjson, yyjson, RapidJSON, asmjson, and any
  future domain comparator unless refreshed under the wave's same-run rules.

Twitter parse remains the visible JSON residual: current Track 1 is `15752`
Mbps versus `21020` sonic-rs strict, `24522` simdjson DOM, and `30931` yyjson.
But current hot-leaf attribution is explicitly unprofiled, so twitter fusion
work is a post-W0 candidate, not a W0 prescription.

## Candidate Posture

Pass Alpha V1 retained four candidate families, but challenge narrowed their
dispatch rights:

| Candidate family | SK-V8 status |
|---|---|
| Baseline profile and telemetry lock | Mandatory W0, dispatchable only after G-Alpha. |
| CostFacts gate binding | W1, required before behavior waves. |
| Typed product-plane expansion | Provisional W2, post-W0/W1 only. |
| Parse and direct behavior candidates | Provisional W3/W4, exact paths chosen only after W0. |
| Bitmap asm bodies | Rejected as default; reserve research only after fresh profile evidence and challenge acceptance. |
| Lock 14 audit | Per-wave gate plus W5 reconciliation. |

## Pass Omega Posture

Pass Omega remains queued but does not block G-Alpha for SK-V8 W0. Omega owns
top-level CRUD, lock amendments, broad path cleanup, and non-skinny canonical
surface refresh. SK-V8 may cite those needs, but it must not mix broad Omega
CRUD into performance waves. Omega may add enforcement or clarification; it
cannot weaken Lock 14 or authorize generic JSON policy leaks.

The S-P2 cohort surfaces one concrete Omega input: the Lock 1 refinement
SC-6-L1-R1, drafted in
`research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`. It
promotes the structural projection from a transient mask stream to a retained
substrate, keeping the parallel-sidecar case forbidden. The discriminant is
cardinality: if the old offset append path survives beside a retained
structural index, Lock 1 fails. The W3 union hypothesis either consumes this
refinement once Omega ratifies it, or the W3 challenge proves the union
satisfies Lock 1 as presently written by deleting the offset-tape's scalar
rediscovery, leaving one retained `Tape`, and routing the Omega residual
explicitly. Omega ratification is the cleaner route; the SK-V8 wave does not
amend `LOCKS.md` itself.

## G-Alpha Posture

This packet may be presented for G-Alpha with the following constraint:

- `G-Alpha closed` authorizes SK-V8 W0 only.
- W1-W6 require W0 closure plus plan augmentation before dispatch.
- `G-Alpha revise` returns to Alpha hardening with named revisions.
