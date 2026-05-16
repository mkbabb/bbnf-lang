# SK-V7 W6 Plan - Object-Pair Value-Byte Control Compaction

Date: 2026-05-16.

Status: Phase 2 synthesis. W6 research is archived in
`wave-6-r1-generated-control-key.md`,
`wave-6-r2-profile-evidence.md`, `wave-6-r3-simd-key-scan.md`,
and `wave-6-r4-bench-gate.md`.

## Intervention Name

Object-Pair Value-Byte Control Compaction.

## Wave Authority

`restart/skinny/tranches/sk-v7/SPEC.md` Section 8 assigns W6 to B6
control/key compaction for `citm_catalog` and `instruments`.
The named hot surface is retained generated JSON control/key bookkeeping,
with a possible AArch64 key-byte primitive only if fresh profile evidence
shows the byte-run scan as the edited bottleneck.

The research cohort found no admissible reason to add a SIMD primitive in
this first W6 cycle. The current retained parser already omits comma and
colon tape offsets, and the dominant candidate is the object pair boundary:

```text
parse_key_colon -> parse_value_at -> dispatch_value
```

The planned edit compacts that boundary to:

```text
parse_key_colon_value_byte -> dispatch_value
```

The helper keeps key quote emission, tiny/full key scan, `HAS_ESC` patching,
colon validation, post-colon whitespace handling, and error kinds in the
current source order, then returns the first value byte to the caller.

## Owner Paths

- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

`bbnf-simd/src/aarch64/` is intentionally out of scope for this W6 cycle.
`wave-6-r3-simd-key-scan.md` found that an AArch64 key-byte scan is not yet
falsifiable without first reducing the already-measured Rust control boundary.

## Planned Source Shape

1. In the generated template and checked-in generated runtime mirror,
   change `parse_pair` so it calls a key/colon helper that returns the
   first value byte, then calls `dispatch_value(state, byte)` directly.
2. Preserve cursor/error ordering:
   - missing key quote stays an `ExpectedValue` error at the current key
     cursor;
   - missing colon stays `ExpectedColon` from the raw string-end cursor;
   - missing value after colon reports `ExpectedValue` from the post-colon
     value cursor.
3. In `bbnf-bench` Track2 hand JSON, extract a local `dispatch_value(byte)`
   owner from `parse_value_at`, then apply the same object-pair value-byte
   compaction. If the edit remains small and tests confirm parity, mirror the
   generated array continuation shape so Track2 avoids the same redundant
   value-entry load after array commas.
4. Do not alter tape offset shape. Object/array opens, string opening quotes,
   scalar starts, and container closes remain the only retained offsets.
   Commas and colons remain non-emitting.

The Track2 hand parser is included because W6's falsifiability gate requires
`citm_catalog` parse Track2 to close. A generated-only change cannot satisfy
that written gate even if Track1 improves.

## Falsifiability Gate

Run the W6 correctness and measurement loop from
`wave-6-r4-bench-gate.md`:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo test --workspace
cargo bench -p bbnf-bench --bench simd_scan -- 'simd/structural_scan/(citm_catalog|instruments)/(simd|scalar)$'
cargo bench -p bbnf-bench --bench json_parity -- 'json/(citm_catalog|instruments)/(track1_generated|track2_handcoded|sonic_rs_anchor|sonic_rs_lossy|simd_json_borrowed|simd_json_owned|serde_json|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

W6 admits only if the refreshed same-run rows satisfy all of:

- fresh profile evidence still names the edited container/key path;
- no hard schema, parity, or SIMD metadata failure on focused rows;
- `citm_catalog` parse Track2 / sonic-rs strict >= 0.90;
- `instruments` parse Track1 / sonic-rs strict >= 1.00;
- `instruments` direct Track1 / sonic-rs direct >= 1.00;
- `citm_catalog` direct remains a guard-row PASS;
- no focused Track1 or Track2 value falls below its 0.97 no-regression floor.

If this exact control-boundary compaction misses any named threshold, the
cycle rejects even if local microbenchmarks improve.

## Same-Wave Consumer Declaration

The same-wave consumers are the generated retained JSON parser and the
independent Track2 hand JSON parser. The edited helper is called from object
pair parsing immediately in both consumers. No orphan primitive or unused
substrate is introduced.

## Pre-Blocked Routes

Per `restart/skinny/tranches/sk-v7/HANDOFF.md` Section 3 and the
`skinny/REDRESS.md` ledger, this plan does not reopen:

- REDRESS 28+33 Class A tiny-string wiring as a parse-G fix;
- REDRESS 50-55 SK-V5 UTF-8 fusion routes;
- REDRESS 60-72 SK-V6 retained/direct materialization routes, including
  object next-key carry, parser-owned decoded scratch, and direct
  source-hook materialization;
- function-pointer dispatch tables;
- capacity prescans;
- generic SWAR whitespace;
- separator elision;
- pair-token fusion;
- EventCursor or sidecar structural prepasses;
- new BBNF directives, new BIR variants, or a new substrate.

The planned object-pair value-byte return is not object next-key carry: it
does not carry the next key across loop iterations or change object
continuation ownership. It only removes the redundant post-colon value-entry
boundary inside the same pair.

## Revert Protocol

On failure:

1. Save the rejected source/status patch to
   `/tmp/skv7-wave-6-control-key-rejected.patch`.
2. Revert the W6 source changes and any generated `skinny/RESULTS.md`
   refresh that came only from the failed candidate.
3. Add a `skinny/REDRESS.md` entry naming the failed threshold, the focused
   measurement table, and the next candidate shape.
4. Commit:
   `docs(sk-v7-wave6-redress): reject object-pair value-byte control compaction`.

On success:

1. Keep the source and `skinny/RESULTS.md` gate refresh.
2. Add a `skinny/REDRESS.md` admit entry with the refreshed same-run
   measurement table.
3. Commit:
   `feat(sk-v7-wave6): admit object-pair value-byte control compaction`.

## Hard Cap

165 minutes total for W6 redress. At 0.9x cap, commit the current evidence
and close as admit or reject. No deferral is permitted.
