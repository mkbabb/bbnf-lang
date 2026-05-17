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

## SK-V8 Thesis

SK-V8 is an observability-bound tranche before it is a performance tranche.

The main failure mode in SK-V7 was not lack of clever kernels. Several correct
kernels or plausible local rewrites lost whole-row throughput. The current
RESULTS surface has enough strict comparator data to identify residual rows,
but not enough hot-leaf and per-row attribution to prescribe another parser
intervention.

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
run.

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

## G-Alpha Posture

This packet may be presented for G-Alpha with the following constraint:

- `G-Alpha closed` authorizes SK-V8 W0 only.
- W1-W6 require W0 closure plus plan augmentation before dispatch.
- `G-Alpha revise` returns to Alpha hardening with named revisions.
