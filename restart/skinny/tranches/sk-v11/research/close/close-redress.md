# SK-V11 Close Redress - Measured Fixpoint And Alpha Feedback

Date: 2026-05-20.

Gate: `G-W9-CLOSE-SK-V11`.

Disposition: PASS as measured fixpoint close; BLOCKED for grammar-generalized
admission. REDRESS: 120.

## Scope

W9 made no behavior source, generated runtime, benchmark body, gate semantic,
or `skinny/RESULTS.md` change. It reconciles the already-landed SK-V11 wave
dispositions into close authority and routes the remaining work to Pass Alpha.

Verification:

- `git diff --exit-code -- skinny/RESULTS.md`
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`

Both checks preserved the unchanged SK-V11-open result surface:

| Family | Close state |
|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO`; diagnostic only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO`; measured fixpoint |
| `real_typed_struct` | 7 `A / GO`; guarded product-plane wins |
| Overall | `N-direct / NoGo` |

## Wave Dispositions

| Wave | Disposition | REDRESS | Row movement |
|---|---|---:|---|
| W0 | SK-V11-open telemetry lock | - | baseline only |
| W1a | non-JSON gate/report lane admitted | 111 | none |
| W1b | generated non-JSON baseline rejected | 112 | none |
| W2 | CSS generated intervention blocked | 113 | none |
| W3 | numeric direct slice rejected | 114 | none |
| W4 | generated dispatch / byte-set slice rejected | 115 | none |
| W5 | bounded string span blocked | 116 | none |
| W6 | escaped segment route blocked | 117 | none |
| W7 | output digest host-sink route blocked | 118 | none |
| W8 | direct residual measured fixpoint | 119 | none |
| W9 | close and Alpha feedback | 120 | none |

W8 is the load-bearing direct close proof. Every residual direct row has a
per-row uncloseable/fixpoint entry naming the relevant attempted or blocked
route, measured Track 1, Track 2, strict comparator, floor, and routed
remainder. W9 does not restate that full table; REDRESS 119 remains the direct
row authority.

## Close Finding

`G-W9-CLOSE-SK-V11` passes only in the measured-fixpoint sense allowed by SPEC
Section 13:

- Every direct residual row is either admitted or has a REDRESS proof. In
  SK-V11 the result is the latter for all 13 residual rows.
- Existing direct and typed guard rows remain unchanged.
- Parse-only remains diagnostic; REDRESS 96/97/98 and REDRESS 102 stay closed.
- The non-JSON generated-intervention axis is not admitted. It is explicitly
  BLOCKED by REDRESS 112 and 113: the bracket could not stand up a generated
  non-JSON baseline and W2 could not create the first measurable baseline row
  inside its intervention wave.

Therefore SK-V11 closes as a measured fixpoint, not as overall direct `GO` and
not as a grammar-generalization win.

## Pass Alpha Feedback

SK-V12 should not spend another JSON-only micro-wave before the generated
non-JSON baseline problem is solved. The material routed remainder is:

- create one real generated non-JSON direct or typed parser baseline first,
  preferably the smallest grammar whose oracle is independent and whose output
  plane can be benchmarked without JSON policy leakage;
- treat the 13 SK-V11 direct residual rows as exhausted unless a future pass
  names a material differential beyond REDRESS 114-119 with fresh profile and
  micro-proof evidence;
- keep W0-clamped row admission pre-blocked by docs-only accounting;
- preserve strict-vs-strict comparator discipline and reject absent sidecars or
  parse-only wins as SOTA evidence.

This closes SK-V11 and triggers the SK-V11 -> SK-V12 Pass Alpha bracket.
