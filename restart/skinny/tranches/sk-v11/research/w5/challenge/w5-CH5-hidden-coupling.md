# SK-V11 W5 CH5 Hidden Coupling

Date: 2026-05-20.

Scope: CH5 challenge review of the W5 bounded string span plan, focused on
Track 1 / Track 2 independence, generated-output freshness, shared-helper bug
risk, stale gate/report provenance, and hidden sidecar or cursor coupling.

Output: this file only.

Disposition: **ACCEPT for redress**.

## Verdict

W5 is acceptable for redress only as the JSON-local generated direct plan
described in `w5-plan-string-span-implementation.md`: one generated
`parse_string_direct` consumer, cap 8, `random/direct_to_struct/main`, and W5
gate/report provenance. This is not closure and not permission to add a public
parse-that span helper consumed by both tracks.

The acceptance turns on one boundary: generated Track 1 may get the new bounded
span shape through `sink_direct.rs` and regenerated
`runtime/src/grammars/json/generated.rs`; direct Track 2 must remain the local
hand parser in `direct_struct.rs`. If implementation instead moves the W5 helper
into `parse-that-regex` and calls that new helper from both Track 1 and Track 2,
CH5 flips to REVISE/BLOCKED because parity would be testing shared parser code,
not an independent oracle.

## Materials Read

- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-string-span-implementation.md`
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-gate-risk-matrix.md`
- `restart/skinny/tranches/sk-v11/research/w5/w5-R1-parse-that-string-span.md`
- `restart/skinny/tranches/sk-v11/research/w5/w5-R2-generated-consumers.md`
- `restart/skinny/tranches/sk-v11/research/w5/w5-R4-row-gates-measurement.md`
- `restart/skinny/tranches/sk-v11/research/w5/w5-R5-grammar-neutral.md`
- `restart/skinny/tranches/sk-v11/research/w5/w5-R6-preblocked-risk.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

## Findings

### 1. Track 2 independence

ACCEPT with a hard boundary.

The current direct split is visible in source: Track 1 calls
`runtime::generated_json::parse_direct`, while Track 2 calls
`hand::sink_digest`. The W5 plan preserves that split by keeping the selected
helper JSON-local and generated-direct-specific, and by forbidding parse-that,
SIMD, typed generated, or runtime-outside-JSON behavior changes for this packet.

Existing shared low-level parse-that fallback helpers are a baseline fact. A new
W5 parse-that helper shared by both tracks is not. Redress must keep source-level
checks that direct Track 2 does not call `runtime::generated_json`, generated
SinkOnly helpers, generated span symbols, or any new W5 shared parser helper.

### 2. Shared-helper bugs hidden by parity

ACCEPT.

Track 1 / Track 2 digest equality alone would be insufficient because both
paths can make the same cap, cursor, escape/control, or unterminated-string
mistake. The plan correctly requires malformed string fixtures where generated
Track 1, hand Track 2, serde_json, and sonic-rs all reject, separate from valid
digest parity. The required coverage includes non-zero offsets, cap-boundary
closes, close just outside cap, escape before close, control before close,
unterminated strings, raw non-ASCII trusted UTF-8, object keys, and string
values.

### 3. Generated-output freshness

ACCEPT.

The plan rejects hand-patched generated output: `sink_direct.rs` is the source
owner, `generated.rs` is regenerated output, and `regen-json` plus `check-json`
are mandatory. Any `generated.rs` diff without the renderer/source diff is
inadmissible. `generated_real_typed.rs` and real-typed schema paths stay out of
the selected packet unless CHALLENGE reopens the plan.

### 4. Gate/report stale provenance

ACCEPT, implementation-blocking if skipped.

Current gate/report code still contains older direct-contract meanings and a
stale `sk_v10_direct_floor("random") == 7734` path. W5 cannot reuse that as
admission authority. The plan fixes this at the plan level by requiring
`random/direct_to_struct/main` to use the W5 floor 7878, `wave_id=SK-V11-W5`,
`same_wave_consumer_class=gate_json_direct_contract`,
`redress_entry=REDRESS-116` if the ledger has not advanced, digest output, and
independent Track 2 status.

Redress must add negative tests for stale `SK-V9-open`, `SK-V10-W2`,
`SK-V10-W10`, `SK-V11-W4`, `REDRESS-113`, `REDRESS-114`, `REDRESS-115`,
`gate_only`, deferred validation, wrong output plane, coupled Track 2, missing
REDRESS, unselected W5 rows, and the false-accept band below 7878 Mbps. Passing
Mbps with stale provenance must still fail.

### 5. Hidden sidecar or cursor coupling

ACCEPT.

The selected helper returns only offsets and `needs_unescape=false` for the
bounded plain path, then leaves fallback string semantics with the existing full
matcher. The plan forbids decoded scratch, retained string side tables,
semantic string facts, source-hook hashes, byte-output materializers,
parser-owned decoded state, `StringBlock16` wrappers, 64-byte retained scans,
SIMD production wiring, and primitive-only production. No sidecar, retained
cursor, class lane, or parser-owned fact slot is authorized by W5.

## Redress Gate

CH5 authorizes W5 redress only if implementation preserves these constraints:

1. The W5 span helper remains generated JSON direct code emitted from
   `sink_direct.rs`; `parse-that-regex` remains behavior-read-only for this
   packet.
2. Direct Track 2 remains local hand parser code and does not call generated
   Track 1, generated helpers, or a new shared W5 parse-that helper.
3. Malformed-input rejection proof is separate from valid-row digest parity and
   uses serde_json plus sonic-rs as same-run strict comparators.
4. `generated.rs` is regenerated from source and verified with `check-json`; no
   runtime generated hand patch counts.
5. Gate/report use W5 selected-row authority for `random/direct_to_struct/main`
   at 7878 Mbps and reject stale or wrong-wave direct provenance.
6. The implementation adds no sidecar, retained cursor, decoded scratch,
   semantic string fact, output hash shortcut, typed consumer, SIMD production
   body, or generic non-JSON behavior.
7. REDRESS 113 remains carried forward; W5 does not close the non-JSON axis.

DISPOSITION: ACCEPT for redress.
