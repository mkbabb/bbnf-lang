# SK-V10 Grand Synthesis

Date: 2026-05-19.

Status: Pass Alpha closed for SK-V9 -> SK-V10 under
`G-ALPHA-SK-V10`; S-P1 Profile closed under
`HARDENING-S-P1-V1-CONSOLIDATED`; S-P2 Research closed under
`HARDENING-S-P2-V1-CONSOLIDATED`; S-P3 Synthesis-Plan closed through V3
confirmation challenge. W0 telemetry freeze is closed under REDRESS 99. W1
direct contract is closed under REDRESS 100. W2 direct row-table reclamation is
closed under REDRESS 101. W3 parse-only firewall is closed under REDRESS 102.
W4 `instruments` typed product admission is rejected under REDRESS 103. W5
root-type typed generalization proof is closed under REDRESS 104. W6
`github_events` root typed row admission is closed under REDRESS 105. W7
full-string primitive micro-proof is rejected under REDRESS 106. W8 hex escape
micro-proof is closed under REDRESS 107. SK-V10 implementation is dispatched
by `SPEC.md` and `DISPATCH-PROMPT.md`; W9 is the next live wave for the exact
accepted C6 `unescape_string` proof.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/SPEC.md`
- `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-G-dispatch-sk-v10.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/alpha-hardening/V1/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/g-alpha/G-ALPHA-PRESENTATION.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 94-107

## Section 0 - Alpha Close Condition

Alpha closes only when:

1. Alpha-A through Alpha-F exist under `restart/skinny/tranches/sk-v10/`.
2. Alpha CHALLENGE accepts the corrected diagnosis with no critical defects.
3. The contract records W3 as retired by REDRESS 98.
4. No parse-only row is used as a SOTA admission while it remains `S / NO-GO`.
5. The candidate shortlist names direct-plane work as the primary JSON frontier,
   typed-plane generalization as a bounded product-plane extension, and
   existing-substrate W4 kernels only behind a micro-prove-first gate.
6. The orchestrator records `G-ALPHA-SK-V10` closed before S-P1/S-P2/S-P3
   dispatch. This is closed by
   `research/g-alpha/G-ALPHA-PRESENTATION.md`.

## Section 1 - Current Measured State

The current result authority is `skinny/RESULTS.md`, W2-rendered over the
frozen `SK-V9-open` run id.

| Family | Count | State |
|---|---:|---|
| `parse_only` | 17 | all `S / NO-GO`; planning evidence only |
| `direct_to_struct` | 17 | 5 `A / GO` digest rows, 12 `N-direct / NO-GO`; primary SK-V10 JSON frontier |
| `real_typed_struct` | 7 | all `A / GO`; primary product-plane SOTA surface |

Typed product rows:

| Corpus | Track 1 Mbps | sonic typed Mbps | Delta |
|---|---:|---:|---:|
| `twitter` | 18302 | 15866 | +15.3% |
| `citm_catalog` | 35102 | 22058 | +59.1% |
| `apache_builds` | 8174 | 8110 | +0.8% |
| `update_center` | 11847 | 12501 | -5.2%, still GO under 1.10 ns slack |
| `mesh` | 10032 | 9270 | +8.2% |
| `marine_ik` | 10728 | 8105 | +32.4% |
| `github_events` | 12827 | 12695 | +1.0% |

## Section 2 - Corrected Diagnosis

The SK-V8 -> SK-V9 substrate-ceiling thesis was tested and falsified. REDRESS
96 and REDRESS 97 made the union-substrate hypothesis measurable, then missed
every W3 must-improve row and every W10b maintain row. REDRESS 98 retires
`G-W3-UNION-SUBSTRATE`.

The typed product plane is validated under the current deferred/view-boundary
typed-product gate. W1 measured real typed rows, and the largest live same-run
typed comparator delta is `citm_catalog` at +59.1% versus sonic-rs typed
strict. That is not a strict-admission claim for bbnf until `gate-json`
consumes a measured-row strictness and validation-path change.

The parse-plane SOTA target is retired for SK-V10. `parse_only` stays a
diagnostic substrate-guard family while rows remain `S / NO-GO`; it is not a
close target and cannot be used to claim SOTA. The direct plane is the largest
remaining JSON pool with a plausible product boundary: 14 direct rows are still
`N-direct / NO-GO`, while 3 digest rows already beat sonic strict.

## Section 3 - SK-V10 Goalset

1. Preserve the six current typed `A / GO` rows and the three direct digest
   `A / GO` guard rows.
2. Profile and plan the `direct_to_struct` plane first. Its 14 NO-GO rows are
   the primary JSON frontier for SK-V10.
3. Add typed product rows only through full generated/serde/sonic parity and
   same-run typed comparator gates.
4. Investigate `instruments` as the first typed admission candidate, then root
   schema generalization for `github_events` and `gsoc-2018`; do not infer
   admission from current parse/direct rows.
5. Re-plan unicode/string kernels only against existing call sites and only
   after an isolated same-host micro-benchmark proves the primitive can win.
6. Keep parse-only honest: it stays `S / NO-GO` and out of the SOTA scoreboard.
7. Maintain REDRESS 98 as a hard pre-block.
8. Route the substrate-ceiling falsification to Pass Omega as a lock amendment,
   so future skinny cycles do not re-derive the dead W3 hypothesis.
9. Route non-JSON grammar generalization to the totality track; JSON-only wins
    are not enough to prove the generator thesis.

## Section 4 - Candidate Shortlist

| Candidate | Type | Row effect before S-P3 |
|---|---|---|
| Direct output/control-path contract | behavior frontier | may move direct digest rows after product/control contract and direct-specific profiling |
| `instruments` typed product admission | behavior | may add one measured `real_typed_struct A / GO` row |
| Root-type typed generalization | codegen/schema | may unlock `github_events` / `gsoc-2018` typed rows |
| Existing-substrate unicode/string kernel pair | behavior/kernel | may help direct/typed unicode-heavy rows only after micro-prove-first |
| Comparator and telemetry refresh | gate-only | no behavior movement without same-wave row gate |

Every candidate carries its initial LOC budget, hard cap, same-wave consumer,
and failure disposition in Alpha-E. S-P2 V1 hardening adds
`p2g-candidate-ledger.md` as the canonical post-CHALLENGE candidate-pool
authority: aliases outside that ledger are inventory-only for SK-V10. S-P3 may
tighten budgets and gates; it may not loosen them without CHALLENGE.

## Section 5 - Pre-Blocked Routes

- W3 union/event substrate under any renamed framing.
- W4 cascade-lock through W3.
- Canada typed row without full-fixture generated/serde/sonic parity proof.
- Apache/CITM-style admission by analogy for any other row.
- Parse-only SOTA close under borrowed-view/deferred-strictness rows.
- Direct digest relabeled as typed product proof.
- JSON policy leaks in generic crates, codegen, or runtime outside JSON; any
  such edit requires a grammar-neutral proof and named CSS L4 / Sheets /
  BBNF-self non-JSON evidence.
- PMU, cycles, structural-scan, or masking probes as producers.
- Any substrate/kernel wave entering S-P3 without an isolated same-host
  micro-benchmark proving the primitive gain first.

## Section 6 - Telemetry Binding

SK-V10 inherits the SK-V9 36-field report schema until S-P3 deliberately changes
it and `gate-json` consumes the change in the same wave. Any new typed row must
render workload, output plane, comparator id, comparator strictness, comparator
freshness, Track 1/Track 2 Mbps, same-run run id, measured validation path,
same-wave consumer class, and REDRESS entry.

W0 closed the opening telemetry freeze without row movement. W6 refreshed the
current `RESULTS.md` authority over one coherent native Criterion root with run
id `sk-v9-open:criterion-fnv64-91b28e519f0fea1d`. The inherited opening
snapshot remains a 40-row surface, and the cost-facts snapshot gate now accepts
that surface plus the single W6 github_events typed row.

W1 closed the direct output/control-path contract. Direct row movement now has
an executable report predicate: baseline `N-direct / NO-GO` rows may move only
with strict digest-plane measured-row evidence, independent Track 2 status,
REDRESS provenance, non-gate-only consumer, and same-run native direct
comparator sources.

W2 closed direct row-table reclamation. `apache_builds` and `numbers` moved to
direct `A / GO` because both generated Track 1 and independent Track 2 cleared
their Section 0.2 floors. The remaining twelve direct rows stay
`N-direct / NO-GO`.

W3 closed as a parse-only firewall. No row moved. The active packet keeps W3
aliases and parse-only SOTA claims as refusal routes.

W4 rejected `instruments/real_typed_struct` after the independent Track 2 typed
oracle missed the W4 floor: Track 1 20678 Mbps, Track 2 12127 Mbps, sonic-rs
typed 15940 Mbps, floor 14491 Mbps. No row moved.

W5 closed the root-type proof. Typed DirectBuild roots now carry
`DirectTypeRef`, and synthetic generated array/map-entry roots pass
serde_json/sonic checksum parity. No row moved.

W6 closed root typed row admission. `github_events/real_typed_struct` moved to
typed `A / GO`: Track 1 12827 Mbps and independent Track 2/oracle 12645 Mbps
both clear the W6 floor of 11541 Mbps under the same-run sonic-rs typed
comparator at 12695 Mbps. The row is strict measured-row evidence consumed by
`gate-json` under REDRESS 105.

W7 rejected the full-string primitive micro-proof. Scalar/reference parity and
strict checkasm parity passed, but the caller-level microbench measured
aggregate `0.774x` versus the required `1.08x` on `unicode_mixed`,
`unicode_escapes`, and `unicode_basic`. The rejected proof patch is saved at
`/tmp/skv10-waveW7-rejected.patch`; no production caller or `RESULTS.md` row
changed.

W8 closed the hex escape micro-proof. The accepted C6 proof measured current
`unescape_string` through `unescape_four_unicode_escapes` against a scalar-only
mirror and cleared the `1.08x` threshold with aggregate `1.268x` on eligible
fixed-width Unicode escape slices. `unicode_mixed` was recorded as zero
eligible because its `\u` text is escaped-backslash data, not JSON Unicode
escape syntax. No production caller or `RESULTS.md` row changed.

## Section 7 - Micro-Prove-First Gate

No substrate or kernel intervention reaches S-P3 wave-scoping until it has a
same-host isolated micro-benchmark proving the primitive or call-site change can
win before integration. The micro proof must name:

1. scalar reference and benchmark harness;
2. exact host flags and feature gates;
3. input slices representative of the target corpus rows;
4. expected integrated consumer;
5. failure threshold that rejects the candidate before source redress.

Profile-first remains necessary for target selection. It is no longer
sufficient for dispatch. W7 is the live example: primitive parity was green,
but caller-level throughput failed, so the route is not available for W9.
W8 is the corresponding accepted route: caller-level throughput cleared the
threshold, but production row movement is still deferred to W9.

## Section 8 - Omega And Totality Routing

Pass Omega should receive a lock amendment: after SK-V7, SK-V8 W3, and SK-V9
W3, profile-derived structural/substrate rewrites are pre-blocked unless the
micro-prove-first gate survives on the same host and the output plane is a live
close target.

The totality track should dispatch a non-JSON T-P1 after SK-V10 Alpha closes.
CSS L4 / Sheets / BBNF-self generalization is the unexercised thesis risk.

## Section 9 - Dispatch Boundary

This file does not independently authorize source work. `G-ALPHA-SK-V10`, S-P1
Profile, S-P2 Research, S-P3 Synthesis-Plan, W0, W1, W2, W3, W5, and W6 are
closed; W8 is proof-closed; W4 and W7 are rejected with measurement. Wave
implementation proceeds only through the current `SPEC.md` and
`DISPATCH-PROMPT.md` entry gates.
