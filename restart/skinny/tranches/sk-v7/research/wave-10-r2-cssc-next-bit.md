# SK-V7 W10 R2 - CSSC CTZ next-set-bit research

Date: 2026-05-16.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: research-only for `BITMAP_NEXT_SET_BIT`; no source files were changed.

## Findings

- W10 explicitly owns `bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`, the
  `bbnf-simd/tests/` checkasm surface, and same-wave runtime consumer wiring
  (`restart/skinny/tranches/sk-v7/SPEC.md:366`,
  `restart/skinny/tranches/sk-v7/SPEC.md:368`,
  `restart/skinny/tranches/sk-v7/SPEC.md:370`,
  `restart/skinny/tranches/sk-v7/SPEC.md:371`,
  `restart/skinny/tranches/sk-v7/SPEC.md:372`). Its task text specifically
  asks for a CSSC CTZ body that emits `ctz` under `-C target-cpu=native`
  (`restart/skinny/tranches/sk-v7/SPEC.md:374`,
  `restart/skinny/tranches/sk-v7/SPEC.md:376`), and its exit gate requires
  both primitive admission and same-wave consumer wiring
  (`restart/skinny/tranches/sk-v7/SPEC.md:381`,
  `restart/skinny/tranches/sk-v7/SPEC.md:382`).

- The current scalar executable spec is already the correct semantic oracle:
  `cursor == 64` returns 64, otherwise the mask is shifted by `cursor`, zero
  shifted masks return 64, and nonzero masks return `cursor +
  shifted.trailing_zeros()` (`skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1`,
  `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:4`,
  `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:7`,
  `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:8`,
  `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:11`). The
  AArch64 body is only a scalar forwarder, so there is no independent CSSC
  body in source today
  (`skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:2`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:3`).

- Dispatch is already positioned to admit a real body: the primitive table has
  a `bitmap_next_set_bit` function pointer
  (`skinny/crates/bbnf-simd/src/dispatch.rs:49`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:53`), AArch64 selects
  `aarch64::bitmap_next_set_bit::bitmap_next_set_bit_neon`
  (`skinny/crates/bbnf-simd/src/dispatch.rs:63`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:64`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:70`), and the public primitive
  wrapper calls through that table
  (`skinny/crates/bbnf-simd/src/lib.rs:244`,
  `skinny/crates/bbnf-simd/src/lib.rs:245`,
  `skinny/crates/bbnf-simd/src/lib.rs:246`). This means an AArch64 source
  replacement can be tested without changing the public API.

- The current dedicated checkasm test is parity-focused and healthy. It sweeps
  boundary masks across cursors `0..=64`
  (`skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:5`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:7`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:8`) and
  performs 4096 random masks across the same cursor range
  (`skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:17`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:20`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:22`).
  Candidate calls are wrapped by `guarded_call`
  (`skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:10`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:24`), whose
  current helper uses a stack canary and compares it after the candidate call
  (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:33`,
  `skinny/crates/bbnf-simd/tests/checkasm_common.rs:46`,
  `skinny/crates/bbnf-simd/tests/checkasm_common.rs:51`).

- The advertised same-wave consumer is not actually wired to
  `BITMAP_NEXT_SET_BIT` today. `CHECKASM-REPORT.md` names `compact_mask`
  structural projection emit as the consumer
  (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:235`,
  `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:239`), and the JSON AArch64
  scanner does import and call `compact_mask`
  (`skinny/crates/runtime/src/grammars/json/scan.rs:200`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:203`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:266`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:267`). But
  `compact_mask` currently reserves output and calls
  `prim::bulk_emit_positions_64`, not `prim::bitmap_next_set_bit`
  (`skinny/crates/bbnf-simd/src/lib.rs:208`,
  `skinny/crates/bbnf-simd/src/lib.rs:213`,
  `skinny/crates/bbnf-simd/src/lib.rs:217`,
  `skinny/crates/bbnf-simd/src/lib.rs:218`). The actual current bit-seeking
  consumer is the bulk emitter's scalar loop, which has its own
  `mask.trailing_zeros()` call
  (`skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1`,
  `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:4`,
  `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:5`,
  `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`,
  `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:3`).

- The local hardware supports CSSC, but `trailing_zeros()` cannot be relied on
  to lower to CSSC CTZ under the current `-C target-cpu=native` proof. Local
  probes: `sysctl -n hw.optional.arm.FEAT_CSSC` returned `1`; `rustc --print
  target-cpus` says native currently selects `apple-m4`; `rustc -C
  target-cpu=native --print cfg` does not include `target_feature="cssc"`.
  Correspondingly, `RUSTFLAGS='-C target-cpu=native' cargo asm -p bbnf-simd
  --lib bitmap_next` shows the current AArch64 body as `lsr`, `rbit`, `clz`,
  and `csel`, not `ctz`. With `RUSTFLAGS='-C target-cpu=apple-m5'`, the same
  `cargo asm` query shows `lsr` followed by `ctz` for
  `bitmap_next_set_bit_neon`. This corrects the older A3 assumption that
  native would expose CSSC on this host
  (`restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:47`,
  `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:56`,
  `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:73`,
  `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:236`).

- Rust's CSSC target feature surface is not a low-risk source dependency yet.
  This workspace is pinned to nightly 2026-04-11 / rustc 1.96.0-nightly
  (`rust-toolchain.toml:11`, `rust-toolchain.toml:13`,
  `rust-toolchain.toml:38`, `rust-toolchain.toml:39`), while the skinny
  workspace package still declares `rust-version = "1.78"`
  (`skinny/Cargo.toml:17`, `skinny/Cargo.toml:21`). A direct
  `#[target_feature(enable = "cssc")]` probe failed with
  `E0658: the target feature cssc is currently unstable`; a crate-wide
  `-C target-feature=+cssc` probe compiles and emits `ctz` but warns that
  `cssc` is unstable. Therefore source should not require unsupported CSSC
  target-feature compilation on non-CSSC or older-toolchain hosts.

- B6 Stage 1 is not closed by the current next-bit test alone. W10 requires
  stack-canary XOR-fold hardening
  (`restart/skinny/tranches/sk-v7/SPEC.md:379`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:127`), but the current common
  helper uses a fixed byte canary plus full-array `assert_eq!`
  (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:46`,
  `skinny/crates/bbnf-simd/tests/checkasm_common.rs:47`,
  `skinny/crates/bbnf-simd/tests/checkasm_common.rs:51`). The B6 design calls
  for a randomized canary, XOR-fold compare, and first-bad-byte diagnostic
  (`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:130`,
  `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:132`,
  `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:133`,
  `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:140`,
  `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:149`).

## Recommended intervention/gate

- Do not claim "CSSC CTZ under `-C target-cpu=native`" on this host until the
  native CPU model exposes `target_feature="cssc"` and `cargo asm` proves the
  target function contains `ctz`. The current native proof is `rbit; clz`, so
  W10 should either record the native CSSC route as blocked on toolchain CPU
  modelling or use an explicit CSSC proof mode.

- If W10 still wants a source intervention, keep the scalar spec as the oracle
  and add only a compile-time CSSC-specialized AArch64 block with scalar
  fallback. The safe shape is `#[cfg(all(target_arch = "aarch64",
  target_feature = "cssc"))]` around any `asm!("ctz ...")` or CSSC-only
  implementation, plus `#[cfg(not(all(target_arch = "aarch64",
  target_feature = "cssc")))]` fallback to the scalar `trailing_zeros()`
  implementation. Do not use `#[target_feature(enable = "cssc")]` in normal
  source until Rust stabilizes that feature for the pinned toolchain policy.

- Treat instruction proof as an opt-in local gate, not a cross-host default
  test. A portable proof script should first check the build target and feature
  availability, then skip rather than fail on unsupported hosts. Example gate:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny

if rustc --print target-cpus | rg -q 'apple-m5'; then
  RUSTFLAGS='-C target-cpu=apple-m5' \
    cargo asm -p bbnf-simd --lib bitmap_next |
    awk '/bitmap_next_set_bit_neon:/,/^$/' |
    rg '\bctz\b'
fi

if rustc -C target-cpu=native --print cfg | rg -q 'target_feature="cssc"'; then
  RUSTFLAGS='-C target-cpu=native' \
    cargo asm -p bbnf-simd --lib bitmap_next |
    awk '/bitmap_next_set_bit_neon:/,/^$/' |
    rg '\bctz\b'
else
  echo 'native CSSC CTZ proof skipped: rustc native cfg lacks target_feature="cssc"'
fi
```

- Keep the semantic parity gate ordinary and cross-host:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
RUSTFLAGS='-C target-cpu=apple-m5' \
  cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
```

  Both commands passed locally. A `RUSTFLAGS='-C target-cpu=native -C
  target-feature=+cssc'` parity run also passed locally, but it emits the
  Rust unstable-feature warning and should remain an opt-in proof route.

- Before W10 closes, resolve the consumer mismatch. Either wire a real
  same-wave user of `prim::bitmap_next_set_bit`, or re-scope the CTZ admission
  to the currently consumed `bulk_emit_positions_64` loop where structural
  projection actually performs next-bit extraction. A checkasm-only
  `BITMAP_NEXT_SET_BIT` body remains an orphan relative to the W10 exit gate.

- The W10 gate should be:
  `cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit`,
  the conditional `cargo asm` CTZ proof above,
  `cargo run -p xtask --release -- primitive-checkasm`, and a source grep that
  proves the non-test same-wave consumer call path is present. The xtask gate
  already includes `checkasm_bitmap_next_set_bit`
  (`skinny/xtask/src/main.rs:292`,
  `skinny/xtask/src/main.rs:293`,
  `skinny/xtask/src/main.rs:299`,
  `skinny/xtask/src/main.rs:304`,
  `skinny/xtask/src/main.rs:306`,
  `skinny/xtask/src/main.rs:308`).

## Risks/pre-blocked routes

- The largest immediate risk is an overclaim: Rust `trailing_zeros()` is a
  good semantic source form, but CTZ emission is target-feature and CPU-model
  dependent. On this host, `target-cpu=native` currently selects `apple-m4`
  and emits `rbit; clz`, so an implementation note that says "native proves
  CSSC" would be false for the pinned local toolchain.

- Do not make the general test suite fail on non-CSSC hosts. CSSC assembly
  should be cfg-fenced, and instruction proof should skip when the requested
  CPU/feature is unavailable. Parity tests should remain scalar-vs-public
  primitive and should not require `target_feature="cssc"`.

- Do not reopen pre-blocked parser routes to manufacture a consumer.
  HANDOFF keeps function-pointer dispatch tables, generic SWAR whitespace,
  separator elision, raw f64 shortcuts, EventCursor prepasses, Class A
  tiny-string rewiring, and SK-V5/SK-V6 retained/direct materialization routes
  blocked (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:71`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:75`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:81`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:84`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:87`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:89`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:90`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:91`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:93`).

- W5 and W6 failures remain binding. The W5 generated-retained StringBlock16
  route regressed all named parse rows and blocks compensating by widening
  parse-that or materialization paths (`skinny/REDRESS.md:2318`,
  `skinny/REDRESS.md:2332`, `skinny/REDRESS.md:2347`,
  `skinny/REDRESS.md:2350`). The W6 object-pair value-byte compaction failed
  its focused gates and blocks object next-key carry, separator elision,
  function-pointer dispatch, generic SWAR whitespace, EventCursor sidecars,
  and W5 string-leaf retries (`skinny/REDRESS.md:2358`,
  `skinny/REDRESS.md:2373`, `skinny/REDRESS.md:2386`,
  `skinny/REDRESS.md:2389`, `skinny/REDRESS.md:2390`,
  `skinny/REDRESS.md:2392`).

- If the intervention targets `bulk_emit_positions_64` instead of
  `bitmap_next_set_bit`, do not count that as admitting the named W10
  `BITMAP_NEXT_SET_BIT` primitive unless the public primitive also receives a
  body and a same-wave non-test consumer. W10 close condition requires two new
  primitive bodies with same-wave consumers, not only local instruction
  improvements (`restart/skinny/tranches/sk-v7/HANDOFF.md:114`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:116`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:127`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:128`).

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md`
- `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/xtask/src/main.rs`
- `rust-toolchain.toml`
- Local probes: `sysctl -n hw.optional.arm.FEAT_CSSC`; `rustc --version
  --verbose`; `rustc --print target-cpus`; `rustc -C target-cpu=native --print
  cfg`; `cargo asm -p bbnf-simd --lib bitmap_next` under
  `RUSTFLAGS='-C target-cpu=native'` and `RUSTFLAGS='-C target-cpu=apple-m5'`;
  `cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit`.
