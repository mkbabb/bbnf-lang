# SK-V8 W6 Research - Close And Alpha Feedback Reconciliation

Date: 2026-05-18.

Status: W6 research active. This artifact admits no source change, no
generated-output change, no `skinny/RESULTS.md` change, and no
`skinny/REDRESS.md` change by itself.

## Entry State

W6 enters only because W0-W5 now have admitted, rejected, or routed status:

| Wave | Authority | Disposition | Ledger effect |
|---|---|---|---|
| W0 | `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md` | Closed by V11+V12 challenge convergence as a telemetry/report/gate close | `skinny/RESULTS.md` contains `SK-V8-open` telemetry for 38 current main rows. |
| W1 | commit `c6345e4d`, then `restart/skinny/tranches/sk-v8/HANDOFF.md` W1 closure record | CostFacts and strict comparator ids admitted into the gate path | No parser, generated output, product-plane, or `skinny/RESULTS.md` change. |
| W2 | `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V5/HARDENING-W2-V5-CONSOLIDATED.md` and REDRESS 91 | Source/product parity admitted for Apache and CITM typed rows; benchmark row-table admission rejected | `skinny/RESULTS.md` remains W0-only for measured real-typed rows. |
| W3 | `restart/skinny/tranches/sk-v8/research/wave-3-hardening/V1/HARDENING-W3-V1-CONSOLIDATED.md` and REDRESS 92 | Tier A tape plus structural-projection implementation rejected/routed before source redress | No source patch, no rejected patch artifact, no row-table admission. |
| W4 | `restart/skinny/tranches/sk-v8/research/wave-4-hardening/V4/HARDENING-W4-V4-CONSOLIDATED.md` and REDRESS 93 | Track 2 scalar-parent fold rejected/routed after selected-row falsification | Rejected patch remains `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`; `skinny/RESULTS.md` unchanged. |
| W5 | `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md` | Named Lock 14 provider-boundary cleanup admitted | Source cleanup only; no generated output, row-table, or `skinny/RESULTS.md` change. |

## Results Ledger

`skinny/RESULTS.md` is still the W0-rendered report authority:

- `skinny/RESULTS.md:46-85` contains 38 `SK-V8-open` telemetry rows.
- `skinny/RESULTS.md:138-141` keeps the report outcome and Track 2 authority:
  overall outcome remains `N-direct / NoGo`.
- The four measured `real_typed_struct A / GO` rows remain the W0 rows:
  `twitter`, `update_center`, `mesh`, and `marine_ik`.
- W2's `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct`
  source/product rows are admitted only as source/product parity rows. They are
  not measured `skinny/RESULTS.md` rows in SK-V8.
- Direct digest rows remain guard-plane rows and never become product-plane
  proof in W6.

The close research found no reason to edit `skinny/RESULTS.md` in W6.

## REDRESS Ledger

The SK-V8 behavior-wave REDRESS entries are aligned:

- REDRESS 91 (`skinny/REDRESS.md:2620-2659`) admits W2 source/product parity
  for Apache and CITM, rejects `canada/real_typed_struct`, rejects W2 benchmark
  row-table admission, and records the checked-report metadata fold.
- REDRESS 92 (`skinny/REDRESS.md:2661-2690`) rejects/routes W3 Tier A because
  the scanner structural index and retained tape event stream are not
  isomorphic.
- REDRESS 93 (`skinny/REDRESS.md:2692-2729`) rejects/routes W4 scalar-parent
  folding after Criterion falsified the selected row gate.

W5 does not need a REDRESS entry because it admitted a named Lock 14
provider-boundary cleanup with hardening authority, no performance claim, and
no row-table update. W6 adds no REDRESS entry unless challenge finds a concrete
close mismatch.

## Residual Routes

The close research routes remaining work without dispatching SK-V9:

- SK-V9 candidate: a typed benchmark row-table tranche that can admit Apache
  and CITM only after it owns run-id/metadata validation and fresh measured row
  evidence.
- SK-V9 candidate: retained class/event grammar and `ValueRef` cursor contract
  proof before any renewed structural-heavy parse wave.
- SK-V9 candidate: direct output contract or control-path tranche before any
  renewed direct digest guard triage.
- Pass Omega residual: SC-6-L1-R1 Lock 1 amendment/generalisation. SK-V8 did
  not ratify or prove that amendment under Lock 1 as written.
- Pass Omega residual: broad lock amendments, canonical path cleanup, and
  top-level CRUD/surface refresh that would exceed W6's close-only role.

## Falsifiers

W6 must reject close if any of these appears:

- A wave lacks admitted/rejected/routed status.
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
  `restart/skinny/tranches/sk-v8/HANDOFF.md` disagree on W2, W3, W4, or W5.
- W2's Apache/CITM rows are presented as measured `RESULTS.md` rows.
- W3 or W4 is presented as an implementation admission.
- W5 is presented as a performance or row-table admission.
- SC-6-L1-R1 is silently treated as ratified.
- A source acceptance lacks its profile artifact, row threshold, REDRESS id
  when required, Lock 14 proof, or same-wave consumer proof.
- W6 edits source, generated output, benchmark data, or row tables.

## Research Disposition

W6 can proceed to a close plan. The current evidence supports a document-only
close plus alpha-feedback routing, with `skinny/RESULTS.md` and
`skinny/REDRESS.md` unchanged unless hardening finds a specific mismatch.
