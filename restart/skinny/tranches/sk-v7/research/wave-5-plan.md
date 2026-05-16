# SK-V7 Wave 5 Plan - Generated-Retained StringBlock16 Tiny Probe

## Intervention

Generated-Retained StringBlock16 Tiny Probe.

Replace the generated retained `match_tiny_plain_string_with_cap::<16>` scalar
loop with a guarded AArch64 16-byte quote/backslash/control probe backed by the
existing `bbnf-simd::aarch64::string_block` primitive. The direct
`match_tiny_plain_string_direct::<8>` path remains scalar.

## Owner Paths

- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
  - Add a JSON-specific scalar wrapper and AArch64 wrapper over the existing
    `scan_string_special_block` masks.
- `skinny/crates/bbnf-simd/tests/checkasm_string_block.rs`
  - Add dedicated wrapper parity over alignment, lane, control-boundary,
    precedence, non-ASCII, and randomized JSON-ish cases.
- `skinny/xtask/src/main.rs`
  - Add the new checkasm file to `primitive-checkasm`.
- `skinny/crates/runtime/src/grammars/json/generated.rs`
  - Wire only the retained `CAP=16` generated tiny-string helper to the wrapper
    under a full 16-byte readability guard.
- `skinny/crates/codegen/src/json_templates/generated.rs`
  - Mirror the runtime generated helper so regeneration remains byte-stable.
- `skinny/RESULTS.md` and `skinny/REDRESS.md`
  - Refresh gate output and record admit/reject evidence.

## Falsifiability Gate

Per `restart/skinny/tranches/sk-v7/SPEC.md` §7:

- `cargo run -p xtask --release -- primitive-checkasm` passes.
- `cargo test --workspace` passes.
- Bench the six W5 parse rows against same-run strict sonic:
  `twitter`, `update_center`, `unicode_basic`, `random`, `unicode_mixed`, and
  `distinct_values`.
- At least four of the six Track 1 parse rows meet their same-run threshold:
  `twitter >=90%`, `update_center >=90%`, `unicode_basic >=100%`,
  `random >=85%`, `unicode_mixed >=85%`, and `distinct_values >=85%` of
  `sonic-rs strict`.
- No named W5 parse row regresses by at least 3% on Track 1 or Track 2 versus
  current `skinny/RESULTS.md`.

Focused measurement command:

```bash
cargo bench -p bbnf-bench --bench json_parity -- 'json/(twitter|update_center|unicode_basic|random|unicode_mixed|distinct_values)/(track1_generated|track2_handcoded|sonic_rs_anchor|sonic_rs_lossy|simd_json_borrowed|simd_json_owned|serde_json|track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
cargo run -p bbnf-bench --bin gate --release -- --advisory
```

## Same-Wave Consumer

The same-wave consumer is the generated retained parser:

- `parse_key_colon` -> `match_tiny_plain_string` -> `match_tiny_plain_string_with_cap::<16>`
- `parse_string` -> `match_tiny_plain_string` -> `match_tiny_plain_string_with_cap::<16>`

This is intentionally not a checkasm-only primitive. The direct
`match_tiny_plain_string_direct::<8>` helper and hand Track 2 parser are guard
surfaces, not W5 consumers.

## Pre-Blocked Routes

The candidate must not reopen the routes named in
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3:

- REDRESS 28+33: old Class A `match_tiny_plain_string_neon` wiring as a broad
  parse-G fix.
- REDRESS 50-55: SK-V5 UTF-8 fusion/materializer routes.
- REDRESS 60-72: SK-V6 retained-parse and direct-materialization routes,
  including parse-that full-string scanner widening, Unicode escape run
  validation, direct string/materialization paths, and global cap changes.

Concrete no-touch surfaces:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs`
- `skinny/crates/codegen/src/json_typed_direct.rs`
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`

## Revert Protocol

If correctness, checkasm, workspace tests, or the W5 row gate fails:

1. Save the candidate source/results diff at
   `/tmp/skv7-wave-5-b2-rejected.patch`.
2. Revert only W5 source, generated/template, test, xtask, and `RESULTS.md`
   edits.
3. Add a `skinny/REDRESS.md` rejection entry naming the failure mode, measured
   rows, guard regressions if any, and the next candidate shape.
4. Commit `docs(sk-v7-wave5-redress): reject generated-retained stringblock16 tiny probe`.

If the gate passes, add the REDRESS admit entry and commit
`feat(sk-v7-wave5): admit generated-retained stringblock16 tiny probe` with a
measurement table in the body.

## Hard Cap

165 minutes total for W5. At 0.9x cap, commit the current admit/reject evidence.
At cap, halt W5 as a measured rejection rather than expanding into the
pre-blocked full-string or materialization families.
