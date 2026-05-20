# SK-V11 W4-R6: Adversarial Pre-Blocked Ledger

Date: 2026-05-20.
Scope: research-only pre-block ledger for W4, "Generated Dispatch And Byte-Set
Control Slice".
Output: this file only.

## 1. Upstream Facts

- SK-V11 SPEC Section 1 is binding: no parse-only SOTA admission, no W3
  union/event/class-column/streaming-cursor/class-lane substrate repair, no new
  directive/BIR/`BackendShape`/public substrate API/parser-owned sidecar, no
  orphan kernel, and no Track 2/oracle path that calls generated Track 1 or
  generated helpers (`SPEC.md:164-184`).
- SK-V11 SPEC Section 8 authorizes only C6 generated FIRST/prefix/lookahead
  dispatch, P2-D D1 `container_tail_next`, and P2-D D2 `direct_slot_dispatch`.
  The entry gate requires W3 to admit or reject with REDRESS, CHALLENGE to select
  exactly one scalar generated dispatch shape, at most three target rows, and a
  proof that no directive/BIR/substrate change is introduced (`SPEC.md:489-537`).
- W2 is not a non-JSON proof for W4 in the live ledger. REDRESS 113 records W2
  as BLOCKED because W1b admitted no generated non-JSON baseline. W4 may proceed
  only as direct-plane closure/fixpoint work while carrying the blocked non-JSON
  axis forward (`skinny/REDRESS.md:3340-3355`).
- W3 has now rejected with measurement. REDRESS 114 falsified the scalar
  `number_span_emit_slot` route on `mesh/direct_to_struct`: Track 1 3835 Mbps,
  Track 2 3614 Mbps, floor 8675 Mbps. W4 may dispatch under Section 8, but it
  must not launder that numeric route as generated dispatch work
  (`skinny/REDRESS.md:3357-3380`; `research/w3/redress/w3-redress-rejection.md`).
- Adjacent W4 research lanes add two gate/owner hazards: generated direct
  dispatch is emitted by `skinny/crates/codegen/src/sink_direct.rs`, which is
  not in SPEC Section 8's owner list, and the existing direct gate has W2/W10
  admission paths but no W4 decision path or shared SK-V11 Section 0.4 floor
  helper yet (`research/w4/w4-R1-generated-dispatch-lowering.md`,
  `research/w4/w4-R4-gate-report-consumption.md`).
- SK-V9 W3 is retired, not deferred. REDRESS 96 and 97 were correctness-green
  implementations of the class-column/structural-index and streaming-cursor
  hypotheses; both missed every W3 must-improve row and every W10b maintain row.
  REDRESS 98 retires `G-W3-UNION-SUBSTRATE`; REDRESS 102 later closes parse-only
  movement as proof-only and forbids W4 from naming W3 as a consumer/substrate
  dependency (`skinny/REDRESS.md:2795-2949`, `3040-3058`).

## 2. W4 Authorization Envelope

Owner surface from SPEC Section 8:

- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Adversarial owner gap: D1/D2 generated direct dispatch cannot honestly be
implemented by editing only `sink_only.rs` plus `json_templates/generated.rs`.
The current generated direct parser is appended by `codegen/src/sink_direct.rs`.
A W4 implementation plan must either get CHALLENGE/SPEC authority to include
that path or reject the generated direct-dispatch route as outside owner scope.

Target rows and floors, copied from the SK-V11 direct residual table for the
Section 8 target set:

| Row | W4 floor Mbps |
|---|---:|
| `canada/direct_to_struct` | 10637 |
| `mesh/direct_to_struct` | 8675 |
| `random/direct_to_struct` | 7878 |
| `update_center/direct_to_struct` | 10059 |
| `github_events/direct_to_struct` | 13403 |
| `twitter/direct_to_struct` | 13740 |

Direct guard floors:

| Row | Track 1 floor | Track 2 floor |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard floors, if W4 touches typed report/gate/output surfaces:

| Row | Track 1 floor | Track 2/oracle floor |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Admission invariant: a selected W4 row admits only if generated Track 1 and
independent Track 2 both meet the row floor, same-output proof passes, strict
same-run direct comparator evidence is current, guard floors hold, and the gate
consumes W4 provenance in the same wave.

## 3. Exact Routes W4 Must Reject

| Route family | Exact rejected route | W4 rejection rule |
|---|---|---|
| Object carry | REDRESS 63 admitted only the narrow retained-array `ContainerNext` next-byte carry. REDRESS 65 rejected object next-key carry. REDRESS 84 rejected object-pair value-byte control compaction. | Do not generalize REDRESS 63 into object carry. Reject any object key quote carry, first-value-byte carry, object-pair compaction, or parent/control-byte carry that crosses a generated direct/typed call boundary. |
| Pair/token churn | REDRESS 16 rejected pair-token fusion; REDRESS 18 rejected skipless 12-byte tape tokens; REDRESS 25 keeps width churn rejected. | W4 may not change retained tape token shape, pair cursor pairing, `payload_or_skip`, subtree skip derivation, or tape-width economics. Section 8 is direct dispatch, not tape substrate. |
| Function dispatch replay | REDRESS 17 rejected the real 256-entry function-pointer dispatch table; REDRESS 25 keeps dispatch-table/function-pointer alternates rejected. | Reject generated function-pointer tables, benchmark-only alternates, or dispatch rows that duplicate canonical generated Track 1. D2 may factor generated scalar dispatch code shape only inside existing `SinkOnly`/`DirectBuild` lowering. |
| Separator/SWAR transfer | REDRESS 25 rejects separator elision and generic SWAR whitespace skipper transfer; REDRESS 59 rejects prescribing old UTF-8/string fusion classes without same-row gates. | Reject separator elision, generic SWAR whitespace, UTF-8 fusion, or byte-set changes that do not name W4 rows, hot symbol boundary, same-output proof, and gate-consumed Track 1/Track 2 measurements. |
| Sidecars and parser cursors | REDRESS 50 rejected parse-time aux side tables; REDRESS 51 rejected byte-class `EventCursor`; REDRESS 53 rejected parser-local structural-mask cursor. | Reject aux columns, sparse side tables, whitespace cursors, structural cursors, event cursors, retained position vectors, hidden bitmaps, and any second scanner. Transient masks are acceptable only when consumed immediately in the same direct/typed loop and not retained. |
| W3 substrate | REDRESS 92 routed W3 before source; REDRESS 96 rejected class-column + structural-vector W3; REDRESS 97 rejected streaming cursor; REDRESS 98 retired `G-W3-UNION-SUBSTRATE`; REDRESS 102 forbids W4-through-W3 cascade lock. | Reject `UnionTape`, class columns, class lanes, structural-position vectors, streaming cursors, parser-owned projections, class-lane-only proof, parse-only row claims, and any W4 plan whose consumer is W3. |
| Track 1/Track 2 coupling | REDRESS 34 identified shared bench-private `SinkParser` dishonesty; REDRESS 48 closed source authority with BIR-lowered generated Track 1; REDRESS 93 rejected Track 2-only scalar-parent folding; REDRESS 100/101/109 define the direct movement contract. | Reject Track 1 == Track 2 substitution, hidden shared parser, Track 2 calling generated Track 1 or generated helpers, Track 2-only admission, direct digest as typed proof, or row movement without both tracks above floor. |
| Numeric laundering | REDRESS 114 rejected W3 `number_span_emit_slot`; W3 R5 pre-blocks REDRESS 31/39/46/80 numeric policy routes. | If W4 selects `mesh` or `canada`, the material differential must be container/dispatch/byte-set control. Reject any W3 numeric slot helper, f64 fallback, mantissa widening, UDOT proof, or digit microkernel relabeled as W4 dispatch. |
| Non-JSON closure by prose | REDRESS 113 blocks W2 because no generated non-JSON baseline exists. | Reject generic-code changes that rely on "W2 proof remains valid" unless CHALLENGE explicitly supersedes REDRESS 113. W4 may only touch JSON generated/local dispatch surfaces without leaking JSON policy into generic crates. |
| Producer-only telemetry | SPEC Section 0.3 makes `gate-json` fail on producer-only telemetry and W3 reopen claims. P3-E treats C9 as accounting only. | Reject sidecar dispatch facts, report fields, PMU/cycles/cost facts, or microbench-only signals as row closers unless the same wave's gate consumes them and selected product rows clear floors. |

## 4. Acceptable Material Differentials

The following are the only W4 material differentials that can clear the pre-block
ledger. Each must be named before redress and bound to the W4 row gate.

### 4.1 D1 `container_tail_next`

Acceptable differential:

- A scalar helper over current source bytes and the current local cursor that
  classifies configured separator/close sets and returns a local cursor or close
  offset.
- Consumed by generated direct `SinkOnly` loops or a typed product loop in the
  same source slice.
- No retained cursor, no class lane, no object-key quote carry, no first-value
  byte carry, no parser-owned side table, no generic JSON container policy.

Mandatory proof:

- Differential generated Track 1 against current output, independent Track 2,
  serde digest, and sonic digest.
- At most three W4 target rows, selected before implementation.
- Selected rows meet floors on both tracks; direct and typed guards hold.
- If Track 2 is edited for parity coverage, it remains structurally independent
  and does not call the new generated helper.

Reject if:

- The helper carries an object key/value byte across boundaries.
- The path is retained-parse-only or W3-dependent.
- The selected rows do not clear floors even if probes improve.

### 4.2 D2 / C6 `direct_slot_dispatch`

Acceptable differential:

- A generated code-shape refactor inside existing `SinkOnly`/`DirectBuild`
  lowering: factor duplicate root/object/array direct dispatch into one scalar
  emitted shape or equivalent per-slot emission.
- Existing `BackendShape`, BIR, directives, and public codegen contract remain
  unchanged.
- Track 1 remains generated runtime code emitted from the existing lowerer;
  Track 2 remains the independent hand direct parser/oracle path.

Mandatory proof:

- Generated source regeneration or a no-regeneration proof, depending on the
  touched owner path.
- Explicit owner authority for `skinny/crates/codegen/src/sink_direct.rs`, or a
  proof that the selected D2 shape is achievable without touching the renderer
  that actually emits `parse_direct`.
- Same-output digest equality across generated Track 1, Track 2, serde, and
  sonic for selected rows.
- `gate-json --with-cost-facts --check-results` consumes W4 provenance and
  rejects below-floor or coupled evidence.

Reject if:

- It is a function-pointer table, table-only dispatch experiment, hidden static
  JSON template, or benchmark-private parser.
- It adds a `BackendShape`, BIR variant, directive, public substrate, or generic
  JSON policy.
- It moves only report/schema text with no selected row movement.

### 4.3 Byte-Set / Mask Support Inside W4

Acceptable differential:

- A transient byte-set or mask calculation used only inside the selected D1/D2
  direct/typed loop, with scalar reference and same-output proof.
- Grammar-specific separator/close/whitespace sets remain generated metadata;
  generic crates do not learn JSON object policy.

Mandatory proof:

- Scalar parity for every selected byte set and boundary case.
- AArch64 support, if any, is feature-gated with scalar fallback and strict
  differential/checkasm before product rows count.
- Caller-level measurement, not primitive-only throughput.

Reject if:

- The mask is retained as a bitmap/sidecar/class column.
- It becomes generic SWAR whitespace policy or JSON-specific generic code.
- It is a standalone SIMD row mover without a same-wave product consumer.

### 4.4 Gate/Report/Track 2 Updates

Acceptable differential:

- Gate/report edits may add W4 provenance, selected-row floors, rejection tests,
  or direct-contract checks.
- Track 2 edits may preserve parity coverage or independent measurement only.

Mandatory proof:

- Negative fixtures for coupled Track 2, missing W4 provenance, below-floor row
  movement, wrong comparator plane, guard regression, W3 reopen claims, and
  producer-only telemetry.
- One shared W4 selected-row floor authority consumed by both producer and
  validator. Do not admit by one table while `report.rs` validates by another.
- Direct row movement follows the REDRESS 100/101/109 pattern: digest output
  plane, strict row semantics, measured-row validation, independent Track 2,
  REDRESS provenance, non-gate-only consumer, non-opening wave id, and same-run
  native direct comparator.

Reject if:

- Track 2 shares generated Track 1 code or a hidden sidecar.
- Gate-only consumer metadata is used as behavior evidence.
- A row admits on one track while the other track misses the floor.

## 5. W4 Adversarial Checklist

Before redress, the W4 plan must answer all of these with concrete paths and
rows:

1. Which single scalar dispatch shape is selected: D1 `container_tail_next`, D2
   `direct_slot_dispatch`, or a byte-set support body inside one of those?
2. Which one to three target rows are selected from `canada`, `mesh`, `random`,
   `update_center`, `github_events`, and `twitter`, and what are their exact
   floors?
3. What makes the route materially different from REDRESS 16/17/18/25,
   50/51/53, 63/65/84, 92/96/97/98/102, 93, and 114?
4. What exact same-wave consumer consumes the new shape?
5. How is Track 2 independent, and where is that independence gate consumed?
6. How are W2 BLOCKED and W3 REJECTED carried forward without claiming non-JSON
   closure or W3 substrate authority?
7. What source/generated/gate/report/RESULTS slice reverts together on row-floor
   miss, output mismatch, guard regression, or Lock 1/14 violation?

## 6. Top Risks

- The easiest accidental violation is laundering object carry: REDRESS 63 is a
  narrow retained-array admission, not permission to carry object key/value bytes
  in W4. REDRESS 65 and 84 make the object forms explicit rejects.
- D1/D2 have an owner-scope hole: `sink_direct.rs` emits the generated direct
  parser but is not named by SPEC Section 8. Treat that as a pre-redress blocker,
  not as an implicit permission.
- Gate floor drift is a row-admission risk. W4 needs a shared SK-V11 Section 0.4
  selected-row floor helper before any `RESULTS.md` movement.
- W2's live state contradicts any casual "non-JSON proof remains valid" reading
  of SPEC Section 8. W4 should avoid generic policy edits unless CHALLENGE first
  repairs the REDRESS 113 block.
- Selecting `mesh` after REDRESS 114 is dangerous: W4 must prove a
  dispatch/control differential, not repackage the failed W3 numeric slot route.
- A generated-only C6 refactor is unlikely to admit a row by itself because the
  W4 gate is both-track. Track 2 must either move independently in the same
  wave or the row remains `N-direct / NO-GO`.
- Gate/report work can easily become producer-only telemetry. The W4 source
  change, selected rows, both-track floors, and guard rows must all be consumed
  by `gate-json` in the same wave.

## 7. Sources

- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-C-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v9/research/p3/skv9-p3-E-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/w3/w3-R5-numeric-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/w3/w3-plan-number-span-emit-slot.md`
- `restart/skinny/tranches/sk-v11/research/w3/redress/w3-redress-rejection.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R1-generated-dispatch-lowering.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R2-json-generated-runtime.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R3-direct-oracles.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R4-gate-report-consumption.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R5-row-floors.md`
