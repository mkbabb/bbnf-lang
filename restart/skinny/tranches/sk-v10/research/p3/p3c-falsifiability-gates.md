# SK-V10 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-19.
Scope: concrete falsifiability gates for plausible SK-V10 waves.
Output: this file.
Authority: folds PASS-3 runtime/SOTA gate posture, S-P1 V1 profile
hardening, S-P2 V1 candidate ledger hardening, Alpha-E floors, and current
`skinny/RESULTS.md`.

## Section 1 - Gate Synthesis

P3-C is a gate-binding artifact, not an implementation dispatch. SK-V10 source
work remains blocked until S-P3 produces the final `SPEC.md` and
`DISPATCH-PROMPT.md`. This file defines what those documents must make
measurable before any wave can move behavior rows.

The candidate pool is `p2g-candidate-ledger.md`. Aliases missing from that
ledger are `inventory-only` for SK-V10. Disposition is load-bearing:

| Disposition | Allowed close |
|---|---|
| `row-gated` | May move a row only with named corpus floor, same-run comparator/oracle evidence, same-run run id, same-wave consumer, and revert protocol. |
| `proof-only` | May close correctness, codegen, scalar, checkasm, or microbench proof. It cannot edit `RESULTS.md` or claim SOTA. |
| `gate-only` | May change evidence/reporting only when the new field is consumed by `gate-json` in the same wave. It cannot move behavior rows. |
| `maintain-only` | May land only when exact caller and maintain floors are named; a standalone primitive does not close. |
| `inventory-only` / `rejected` | Not dispatchable for SK-V10 behavior. |

No parse-only row is a SK-V10 SOTA admission while it remains `S / NO-GO`.
Parse-only throughput may select investigation targets, but it cannot satisfy a
row gate, comparator gate, or close condition.

Every row-moving wave inherits these predicates:

- Same-run strict comparator evidence is required on the same output plane:
  direct rows use sonic-rs direct digest strict rows; typed rows use sonic-rs
  typed strict rows plus serde_json typed evidence.
- Generated Track 1 and the independent Track 2/oracle must be present under
  the same run id for the target row. Track 2 may not call generated Track 1,
  generated SinkOnly helpers, generated typed helpers, or benchmark-private
  shared parser code.
- The committed report must render row id, workload, output plane, comparator
  id, comparator strictness, comparator freshness, Track 1/Track 2 Mbps,
  validation path, sample metadata, host/build flags, same-run run id,
  same-wave consumer class, and REDRESS/disposition.
- `gate-json` must reject the row if any required comparator, run-id,
  freshness, parity, checksum, or consumer field is missing or mixed.
- C++ sidecars and historical comparator cells are planning signals only unless
  a same-run sidecar manifest is emitted and consumed by `gate-json` in that
  same wave.
- A row below floor fails. "Near", "hot leaf improved", "same shape", "future
  consumer", "advisory only", or "parse-only won" are unmeasurable gates and
  must be rejected.

## Section 2 - Numeric Floors

### Direct Row Movement Floors

Direct row movement uses the Alpha-E / P2-G direct matrix. A selected
`direct_to_struct` row moves only if both generated Track 1 and independent
Track 2/oracle meet the floor under the same run id and strict direct
comparator plane.

| Corpus | Floor Mbps |
|---|---:|
| `twitter` | 13840 |
| `canada` | 10977 |
| `apache_builds` | 10020 |
| `github_events` | 14364 |
| `update_center` | 10160 |
| `mesh` | 8916 |
| `random` | 7734 |
| `gsoc-2018` | 20980 |
| `instruments` | 11086 |
| `numbers` | 11788 |
| `unicode_mixed` | 9314 |
| `unicode_escapes` | 12527 |
| `distinct_values` | 10022 |
| `y_string_unicode` | 8027 |

Existing direct GO guard rows must preserve their same-plane direct gate if a
wave touches direct output or the report table:

| Corpus | Direct maintain floor Mbps |
|---|---:|
| `citm_catalog` | 18145 |
| `marine_ik` | 7575 |
| `unicode_basic` | 7841 |

### Typed Product Maintain Floors

Existing typed `A / GO` rows remain admitted only while generated Track 1 keeps
the typed A/GO gate against the same-run sonic typed comparator. The current
seed floors are:

| Corpus | Typed Track 1 maintain floor Mbps |
|---|---:|
| `twitter` | 14424 |
| `citm_catalog` | 20053 |
| `apache_builds` | 7373 |
| `update_center` | 11365 |
| `mesh` | 8428 |
| `marine_ik` | 7369 |

New typed rows require same-wave generated typed Track 1, independent Track 2
checksum oracle, serde_json typed row, sonic-rs typed row, full-fixture checksum
parity, same-run Criterion metadata, and the row floor
`ceil(sonic_typed / 1.10)` for both generated Track 1 and the independent
Track 2/oracle unless the final SPEC tightens the requirement.

### W10b Maintain Floors

Any aarch64 SIMD, string, unescape, number, whitespace, byte-class, movemask,
or parse-loop production wiring must keep the inherited W10b maintain block:

| Corpus | Floor Mbps |
|---|---:|
| `canada` | 15866 |
| `citm_catalog` | 28630 |
| `instruments` | 15865 |
| `marine_ik` | 11831 |
| `mesh` | 12186 |
| `numbers` | 17596 |

If a production kernel also targets a direct row, the relevant direct floor
above applies in addition to this maintain block.

## Section 3 - Plausible Wave Gates

### W0 - Comparator And Telemetry Refresh

Candidate ids: `C12-telemetry-refresh`; gate-only.

Entry gate: S-P3 chooses to refresh report identity, comparator freshness, or
run metadata before behavior waves.

Exit gate:

- All current `skinny/RESULTS.md` row dispositions are preserved unless a
  separate same-wave behavior gate admits or rejects a row.
- Every new telemetry field is consumed by `gate-json` in the same wave that
  emits it.
- Same-run sidecar freshness, if added, carries corpus identity, comparator id,
  binary/build identity, plane, strictness, freshness, run id, and sample
  metadata. It remains evidence only.
- Missing sidecars render explicit `absent:<reason>` evidence.

Negative gates:

- A freshness manifest, run id, profile path, or sidecar field cannot move a
  parser, direct, or typed row by itself.
- Any emitted but unconsumed field rejects the wave.

Revert protocol: revert report/schema/gate field changes as one slice, leave
`RESULTS.md` dispositions unchanged, and record REDRESS naming the missing
consumer or malformed evidence.

### W1 - Direct Output/Control-Path Contract

Candidate ids: `C1-direct-output-contract`; contract-only or row-gated.

Entry gate: the wave names direct output equivalence, independent Track 2/oracle
status, comparator anchor semantics, exact owner paths, selected corpus rows,
and whether the wave is contract-only or row-moving.

Contract-only close gate:

- Direct output/control equivalence is executable in `gate-json`.
- Track 1 generated direct, independent Track 2/oracle, sonic direct strict,
  run id, comparator plane, and validation path are rendered and rejected when
  missing.
- No `RESULTS.md` row moves.

Row-moving close gate:

- Every selected direct row meets its Section 2 direct floor on both Track 1
  and Track 2/oracle under the same run id.
- Existing direct GO guard rows keep their Section 2 direct maintain floors.
- Existing typed GO rows keep their Section 2 typed maintain floors if the
  measured report is refreshed.
- `gate-json` consumes the direct contract and rejects any digest movement that
  lacks same-run strict direct comparator evidence.

Negative gates:

- Direct digest movement never admits typed product rows.
- Apache/numbers positive deltas from the current report do not move unless the
  fresh direct contract and row floors are satisfied.
- Any output-plane equivalence, Track 2 independence, comparator, or run-id
  ambiguity rejects the wave.

Revert protocol: contract-only failure reverts contract/report/gate changes.
Row-moving failure reverts direct behavior, generated output, bench wiring,
`RESULTS.md`, and gate changes as one slice unless the failed row is explicitly
left disabled with rejected status; record REDRESS with target rows and missed
floors.

### W2 - `instruments` Typed Product Admission

Candidate ids: `C2-instruments-typed-admission`; row-gated.

Entry gate: the wave names `instruments/real_typed_struct`, generated typed
output path, independent Track 2 checksum oracle, serde_json typed comparator,
sonic-rs typed comparator, schema facts, run-id source, and rollback boundary.

Exit gate:

- Generated Track 1, independent Track 2/oracle, serde_json typed, and sonic-rs
  typed rows exist for the full fixture under one coherent run id.
- All typed checksums match.
- Generated Track 1 and independent Track 2/oracle both meet
  `ceil(sonic_typed / 1.10)` from the same-run sonic typed row.
- Existing typed maintain rows keep the Section 2 typed maintain floors.
- Direct digest evidence for `instruments` remains direct-plane evidence only.

Negative gates:

- Missing schema, missing checksum parity, mixed run id, missing serde/sonic
  typed row, or missing Track 2/oracle rejects the wave.
- A generated typed row that passes while Track 2/oracle or comparator evidence
  is absent is not partial admission.

Revert protocol: revert schema, generated typed code, bench wiring, gate
changes, and `RESULTS.md` as one slice; preserve the rejected patch and checksum
table in research; record REDRESS with `instruments` still absent from admitted
`real_typed_struct` rows.

### W3 - Root-Type Typed Generalization

Candidate ids: `C3-root-typed-generalization`; proof-only unless paired with a
typed row gate.

Entry gate: the wave names the root model/codegen paths and target proof roots:
`github_events` top-level array and `gsoc-2018` numeric-string-keyed map root.

Proof-only close gate:

- `DirectRootSchema` or successor root model represents `Vec<T>` and map-entry
  roots without JSON-specific policy in generic code.
- Generated roots preserve full-fixture generated/serde/sonic checksum parity.
- Lock 14 proof names non-JSON effect, or proves no generic behavior changed.
- No `RESULTS.md` row moves.

Paired row-moving gate:

- A same-wave typed product row for `github_events` or `gsoc-2018` must satisfy
  the new typed row rule from Section 2: generated Track 1, independent Track
  2/oracle, serde_json typed, sonic-rs typed, full-fixture parity, coherent run
  id, and `ceil(sonic_typed / 1.10)` floor for Track 1 and Track 2/oracle.
- The proof-only root model may be kept only if S-P3 pre-authorized proof-only
  close independent of the row attempt.

Negative gates:

- Root arrays or map-entry roots implemented through JSON policy in generic
  code reject the wave.
- Throughput movement without same-wave typed comparator rows rejects the
  row-moving slice.

Revert protocol: for proof-only failure, revert root-model/codegen/test changes
and record the blocker. For paired row failure, keep only the pre-authorized
proof-only slice; otherwise revert root-model, generated output, bench/gate,
and `RESULTS.md` together and record REDRESS.

### W4 - Primitive Micro-Proof Waves

Candidate ids: `C4-tiny-string-proof`, `C5-full-string-proof`,
`C6-hex-escape-proof`, `C7-string-segment-fold`, `C8-digit-number-proof`, and
`C9-whitespace-class-skip`.

Entry gate: one primitive family, one call-site proof, and one consumer plane
are named. The plan must name scalar oracle, checkasm target where applicable,
host feature gate, representative corpus slices, caller microbench, failure
threshold, and intended production consumer.

Proof-only close gate:

- Scalar reference passes.
- Checkasm/parity passes for any SIMD/ASM or lane-class primitive.
- Caller microbench on representative slices clears the declared threshold.
- The proof artifact records host triple, CPU, target flags, feature gate,
  corpus slices, sample metadata, run id, scalar oracle, and failure threshold.
- No production caller wiring and no `RESULTS.md` movement occur.

Per-candidate constraints:

| Candidate | Proof-only boundary |
|---|---|
| `C4` tiny string | One call site at a time; generic API returns offsets/classes under caller-owned delimiter/control policy. Generated direct cap 8, typed parse cap 32, typed skip cap 96, retained cap 16 excluded unless explicitly targeted. |
| `C5` full string | Caller-supplied class policy and current direct/typed string caller required. Parse-only rows cannot admit SOTA. |
| `C6` hex escape | Hex decode/classify only. Grammar templates own slash/introducer, `\\u`, surrogate policy, CSS termination, and Sheets quote-doubling. |
| `C7` segment fold | One output plane only. Direct digest movement cannot admit typed rows; typed evidence cannot move direct rows. |
| `C8` digit/number | Split grammar-neutral digit masks/accumulators from generated grammar-owned number policy. Canada typed remains blocked without full-fixture proof. |
| `C9` whitespace/class skip | Maintain-only unless paired with exact current caller, caller-owned class table, and the Section 2 W10b maintain floors. |

Negative gates:

- A profile hot leaf is not proof.
- A generic primitive with no same-wave caller is an orphan and rejects.
- W3/retained structural cursor consumption is rejected.
- Parse-only improvement cannot satisfy SOTA admission.

Revert protocol: proof-only failure leaves no source behavior. Production
wiring attempted without a prior proof must be reverted and recorded as
REDRESS before any row is considered.

### W5 - Existing-Substrate Production Kernel Wiring

Candidate ids: production follow-on for `C4`, `C5`, `C6`, `C7`, `C8`, or `C9`;
row-gated only after W4 proof.

Entry gate: the relevant W4 proof has passed, and the wave names exactly one
current production caller such as `match_string_at_quote_trusted_utf8`,
`validate_unicode_escape_run`, `decode_unicode_escape`, `unescape_string`, a
generated direct number caller, or an exact whitespace caller. W3 is not a
consumer.

Exit gate:

- The production caller is the same consumer proven in W4.
- Scalar/reference and checkasm parity remain green after integration.
- Target direct rows meet their Section 2 direct floors if row movement is
  claimed.
- The Section 2 W10b maintain block holds for every aarch64 SIMD/string/
  unescape/number/whitespace production wiring.
- Existing typed and direct GO rows keep their Section 2 maintain floors when
  report rows are refreshed.
- Unicode rows that miss direct floors are recorded honestly as `NO-GO`; proof
  correctness alone is not row admission.

Negative gates:

- Missing production caller, scalar mismatch, checkasm failure, maintain miss,
  or run-id/comparator mismatch rejects the production patch.
- A passing microbench with no full-fixture row gate cannot update
  `RESULTS.md`.
- Sidecar, retained, or Track 2-only movement cannot admit Track 1 behavior.

Revert protocol: revert production source, generated output, bench wiring,
gate/report changes, and `RESULTS.md` as one slice; save the rejected patch in
research; record REDRESS with target rows, maintain rows, and exact failing
threshold.

### W6 - Rejected And Inventory Routes

Candidate ids: `C10-byte-class-movemask`, `C11-tape-economy-contract`,
`C13-x86-secondary-isa`, `C14-redress-blocked-structural`, and
`C15-rejected-product-shortcuts`.

Close gate:

- `C10` and `C13` may appear only as inventory unless paired to a direct/typed
  caller through W4/W5 gates on the current host.
- `C11` is invariant/proof-only; capacity pre-scans remain diagnostic/env-only
  and do not move rows.
- `C14` and `C15` are rejected and must not be shortlisted.

Negative gates:

- x86 ISA tables are not SK-V10 behavior authority on Apple aarch64.
- W3 union/event substrate, retained class column, structural cursor, default
  PMULL/CTZ rewires, and Canada typed shortcut remain pre-blocked.
- Any plan that says "future host", "future consumer", or "paper proof" for a
  behavior row rejects as unmeasurable.

Revert protocol: no source behavior is authorized. If one of these routes
appears in a source plan, S-P3 must block dispatch and record a REDRESS or
pre-block citation instead of landing code.

## Section 4 - Revert And REDRESS Protocol

Every behavior wave must define the rollback slice before dispatch:

| Failure class | Required action |
|---|---|
| Comparator, run id, freshness, checksum, or Track 2 evidence missing | Revert row/report/gate changes; keep research evidence; REDRESS as missing evidence. |
| Row below floor | Revert behavior and `RESULTS.md`; REDRESS with measured row, floor, observed Mbps, run id, and target caller. |
| Maintain floor miss | Revert production patch; REDRESS with affected maintain rows and whether proof-only artifact can remain. |
| Lock 14 or JSON-policy leak | Revert generic/codegen/runtime leak; REDRESS with non-JSON proof gap. |
| Orphan primitive or future consumer | Revert primitive wiring; proof-only artifact may remain only if pre-authorized and behavior-free. |
| Gate-only emitted-but-unconsumed field | Revert field/schema/report change; REDRESS with missing `gate-json` consumer. |

Partial admission is forbidden unless the SPEC names independent row slices
before dispatch. Otherwise a selected set admits only when every selected row
and every maintain gate passes.

## Section 5 - Unmeasurable Gate Rejection

S-P3 must reject any proposed gate that cannot be checked from one of:

- a named `skinny/RESULTS.md` row plus same-run Criterion metadata;
- a `gate-json` rejection/acceptance path;
- a scalar/checkasm/caller microbench artifact with run id and host flags;
- a checksum/parity fixture over the full target corpus;
- a grep/API negative gate with explicit allowlist.

Rejected gate examples:

- "Use parse-only win as SOTA evidence."
- "Admit if the hot leaf looks smaller."
- "Admit if sidecar freshness is present."
- "Admit direct digest as typed proof."
- "Land primitive now and find the consumer later."
- "Use historical C++ sidecar as strict anchor."
- "Treat `instruments` direct row as typed product evidence."
- "Keep behavior because proof passed, despite a maintain-floor miss."

## Section 6 - Sources

- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Section 7 - Self-Verdict

Verdict: ACCEPT.

Confidence: 91%.

Blockers: none for P3-C gate production. Implementation remains blocked until
S-P3 folds these gates into `SPEC.md` and `DISPATCH-PROMPT.md`.

Residual risks:

- The `instruments`, `github_events`, and `gsoc-2018` typed floors are formula
  gates until same-wave sonic typed rows exist.
- Primitive production wiring may need to split W4 proof from W5 row movement
  if scalar/checkasm/caller microbench plus full fixture gate exceeds the skinny
  redress cap.
- Comparator refresh can improve evidence quality but must remain gate-only
  unless a same-wave behavior gate independently admits a row.
