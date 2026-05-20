# SK-V11 W4 CH6 Challenge: Anti-Paper-Close

Date: 2026-05-20.
Scope: CH6 challenge for W4 generated dispatch and byte-set control.
Owned artifact: `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH6-anti-paper-close.md`.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 8.
- `restart/skinny/tranches/sk-v11/HANDOFF.md`.
- `skinny/REDRESS.md` REDRESS 113 and 114.
- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R1-generated-dispatch-lowering.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R3-direct-oracles.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R4-gate-report-consumption.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R5-row-floors.md`.
- `restart/skinny/tranches/sk-v11/research/w4/w4-R6-preblocked-ledger.md`.

## Verdict

ACCEPT.

W4 may proceed as a measured one-row D1 `container_tail_next` redress attempt
for `random/direct_to_struct`. It cannot close by paper, by SPEC text, by
gate/report metadata alone, or by future-wave promise. Positive W4 closure
requires selected-row movement: generated Track 1 and independent Track 2 both
must meet the `random/direct_to_struct` floor of 7878 Mbps in a fresh native
Criterion capture, with same-output proof, guard floors, and same-wave
`gate-json` provenance consumption. Without that row movement, W4 has only a
measured REDRESS rejection path.

## Challenge Findings

### 1. Row Movement

W4 cannot positively close without row movement. SPEC Section 8 requires every
selected W4 direct row to meet its Section 0.4 floor on both Track 1 and Track 2,
with Track 2 independence, same-output proof, and guard floors. The plan selects
only `random/direct_to_struct`, whose W0 baselines are 7693 Mbps Track 1 and
6949 Mbps Track 2 against a 7878 Mbps floor. That means the plan still needs a
real Track 1 lift and a larger Track 2 lift; a helper refactor, probe result,
or gate schema change cannot substitute for the full product row.

If the fresh W4 capture misses the floor on either track, W4 must reject with
measured evidence. It may record the attempt in REDRESS 115, but it must not
move `skinny/RESULTS.md` or claim direct closure.

### 2. SPEC Correction

The SPEC Section 8 correction is justified, not scope creep, if kept to the
stated owner repair. W4 research shows the generated direct parser is emitted by
`skinny/crates/codegen/src/sink_direct.rs`; editing only `lower/sink_only.rs`
or `json_templates/generated.rs` cannot honestly implement D1 or D2 in the
generated direct sink path. Current SPEC Section 8 now names `sink_direct.rs`
inside the W4 owner table, which makes the implementation surface explicit
instead of relying on an implicit source-owner exception.

The correction also properly replaces stale W2 language with the live REDRESS
113 state: W2 is blocked and is not proof for generic edits. This is a ledger
correction, not extra W4 scope. It would become scope creep only if used to add
a directive, BIR variant, public substrate, generic JSON policy, retained
cursor, sidecar, class lane, or non-JSON generated-baseline claim.

### 3. Same-Wave Gate Consumption

The plan has an acceptable same-wave consumer requirement, but the consumer is a
close predicate, not pre-existing evidence. W4 must add a W4-specific direct
decision path before the W0 clamp and matching report validation, using existing
telemetry fields:

- `same_wave_consumer_class=gate_json_direct_contract`
- `wave_id=SK-V11-W4`
- `redress_entry=REDRESS-115`
- a W4-specific direct delta
- strict measured-row validation on the digest output plane
- independent Track 2 and same-run native direct comparator evidence

No new report field is needed. The producer and validator must share the same
W4 selected-row floor authority, and `gate-json --with-cost-facts --check-results`
must consume the W4 Criterion home before any `RESULTS.md` movement. Missing W4
provenance, gate-only metadata, producer-only fields, stale wave ids, wrong
comparator plane, Track 2 coupling, direct guard misses, or one-track-only floor
passes are CH6 rejects.

### 4. Non-JSON Block

The REDRESS 113 non-JSON block must be carried forward. W4 is authorized only as
JSON direct-plane closure or fixpoint work after W3's measured rejection. A W4
direct admission does not close the SK-V11 grammar-generalization axis, does not
create generated CSS L4/Sheets/BBNF-self baseline authority, and does not prove
Lock 14 generality by prose.

Any later SK-V11 close must either carry the REDRESS 113 BLOCKED route
explicitly or be superseded by a later contract that creates a generated
non-JSON baseline wave with explicit owner authority.

### 5. Reject Path

The plan has a clear reject path. The source/generated/gate/report/results slice
must revert as one unit on any of:

- `random/direct_to_struct` Track 1 or Track 2 floor miss
- output mismatch across generated Track 1, independent Track 2, serde_json, or
  sonic-rs evidence
- direct or required typed guard regression
- Track 2 coupling to generated Track 1 or generated helpers
- owner-path, Lock 1, or Lock 14 violation
- missing same-wave W4 gate/report provenance consumption
- CHALLENGE or SPEC owner correction rejection

On reject, the reverted patch is saved to `/tmp/skv11-waveW4-rejected.patch`
and REDRESS 115 records the measured evidence. That is a real fail-closed path,
not a paper-close fallback.

## Required Close Predicates

W4 remains accepted only under these predicates:

1. The selected scalar shape is D1 `container_tail_next`.
2. The selected target set is exactly `random/direct_to_struct` unless a later
   CHALLENGE revises the row set.
3. `sink_direct.rs` is used only as the generated direct renderer owner for the
   W4 helper, not as permission for generic policy or substrate work.
4. Track 2 implements its own independent local helper or equivalent logic and
   does not call generated Track 1 or generated helper code.
5. Fresh native Criterion rows show `random/direct_to_struct` Track 1 and Track
   2 both at or above 7878 Mbps.
6. Direct guards hold; typed guards hold if measured or required by report-wide
   validation.
7. `gate-json` consumes W4 provenance in the same wave before `RESULTS.md`
   changes.
8. REDRESS 113's non-JSON block is carried forward and is not converted into a
   W4 generality proof.

DISPOSITION: ACCEPT
