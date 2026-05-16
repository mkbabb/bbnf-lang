# SK-V7 W10 R4 - same-wave consumers, benches, and no-regression gate

## Findings

- W10 is not a checkasm-only wave. SPEC §12 names two AArch64 body fills, the
  tests directory, and runtime same-wave wiring as owner paths, then makes
  "checkasm parity tests + same-wave OffsetTape consumer in scan path" and "No
  row regresses" exit requirements (`restart/skinny/tranches/sk-v7/SPEC.md:366`,
  `restart/skinny/tranches/sk-v7/SPEC.md:368`,
  `restart/skinny/tranches/sk-v7/SPEC.md:374`,
  `restart/skinny/tranches/sk-v7/SPEC.md:378`,
  `restart/skinny/tranches/sk-v7/SPEC.md:381`,
  `restart/skinny/tranches/sk-v7/SPEC.md:384`). HANDOFF repeats that W10 admits
  PMULL + CSSC CTZ only with same-wave consumers, and W10 entry is blocked until
  W9 CostFacts plus a same-wave OffsetTape consumer are present
  (`restart/skinny/tranches/sk-v7/HANDOFF.md:58`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:59`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:111`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:112`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:125`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:128`).

- W9 appears landed in the current tree, so W10 should consume the existing
  substrate rather than reopen it. The W9 plan defined the same-wave consumer
  shape as `passes::compile()` producing CostFacts, `codegen::lower_to_rust()`
  receiving them, and `xtask gate-json --with-cost-facts` serializing them
  (`restart/skinny/tranches/sk-v7/research/wave-9-plan.md:60`,
  `restart/skinny/tranches/sk-v7/research/wave-9-plan.md:63`). Current REDRESS
  records those same facts as implemented and keeps unflagged `gate-json`
  behavior unchanged (`skinny/REDRESS.md:2473`, `skinny/REDRESS.md:2480`,
  `skinny/REDRESS.md:2481`, `skinny/REDRESS.md:2486`).

- The W10 "bbnf.asm body" wording is architectural, not an instruction to add
  AArch64 assembly files. `bbnf-simd` build.rs only assembles authored `.asm` or
  `.S` sources on x86_64; AArch64 returns before assembler invocation
  (`skinny/crates/bbnf-simd/build.rs:37`, `skinny/crates/bbnf-simd/build.rs:40`,
  `skinny/crates/bbnf-simd/build.rs:43`, `skinny/crates/bbnf-simd/build.rs:49`).
  The owner paths for W10 are the AArch64 Rust wrapper modules, which currently
  delegate directly to scalar references (`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:4`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`,
  `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:4`).

- PMULL has a real same-wave consumer already available if the AArch64 wrapper
  body is replaced. `bbnf_simd::prefix_xor_64` dispatches through
  `prim::bitmap_prefix_xor_64` (`skinny/crates/bbnf-simd/src/lib.rs:170`,
  `skinny/crates/bbnf-simd/src/lib.rs:172`), the primitive table selects the
  AArch64 prefix-XOR body on AArch64 (`skinny/crates/bbnf-simd/src/dispatch.rs:49`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:55`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:63`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:74`), and JSON scan uses
  `prefix_xor_64` to propagate quote-mask state before emitting structural
  offsets (`skinny/crates/runtime/src/grammars/json/scan.rs:200`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:203`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:239`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:267`). A3 calls this out as
  the intended W10 consumer (`restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:71`).

- CSSC CTZ is currently at risk of being orphaned. `prim::bitmap_next_set_bit`
  exists and dispatches through the primitive table (`skinny/crates/bbnf-simd/src/lib.rs:245`,
  `skinny/crates/bbnf-simd/src/lib.rs:247`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:52`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:53`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:69`,
  `skinny/crates/bbnf-simd/src/dispatch.rs:70`), and it has scalar and checkasm
  coverage (`skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1`,
  `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:13`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:5`,
  `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:29`). But the
  runtime scan path calls `compact_mask`, and `compact_mask` calls
  `prim::bulk_emit_positions_64`, not `prim::bitmap_next_set_bit`
  (`skinny/crates/bbnf-simd/src/lib.rs:208`,
  `skinny/crates/bbnf-simd/src/lib.rs:223`). The AArch64 bulk-emitter also
  delegates to the scalar emitter (`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`,
  `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:4`), whose hot
  loop uses `mask.trailing_zeros()` directly (`skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1`,
  `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:12`). Therefore
  a CSSC body in `bitmap_next_set_bit_neon` is not enough; W10 must wire
  `bulk_emit_positions_64_neon` or `compact_mask` through that primitive.

- The parser default does not consume the SIMD structural index. `ParserState`
  only uses the scan layer to choose initial tape capacity (`skinny/crates/runtime/src/grammars/json/parser.rs:16`,
  `skinny/crates/runtime/src/grammars/json/parser.rs:24`), `CapacityPlan` defaults
  to `GrowOnly` (`skinny/crates/runtime/src/tape/assembler.rs:21`,
  `skinny/crates/runtime/src/tape/assembler.rs:28`), and the generated
  `attach_structural_index` hook is currently a no-op
  (`skinny/crates/runtime/src/grammars/json/generated.rs:12`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:17`). The safe W10
  consumer is the existing JSON scan function used by bench and optional
  capacity plans, not a parser-shape rewrite.

- The bench and gate surfaces already measure the right consumer path. The SIMD
  scan bench compares scalar and SIMD offsets for every fixture, writes SIMD
  metadata, and benches `bbnf_bench::scan::structural_offsets_simd`
  (`skinny/crates/bbnf-bench/benches/simd_scan.rs:16`,
  `skinny/crates/bbnf-bench/benches/simd_scan.rs:27`,
  `skinny/crates/bbnf-bench/benches/simd_scan.rs:29`,
  `skinny/crates/bbnf-bench/benches/simd_scan.rs:37`). `bbnf-bench` exposes both
  `json_parity` and `simd_scan` benches (`skinny/crates/bbnf-bench/Cargo.toml:29`,
  `skinny/crates/bbnf-bench/Cargo.toml:35`), and both Criterion configs use
  five-second measurement, 100 samples, and 0.02 noise threshold
  (`skinny/crates/bbnf-bench/benches/json_parity.rs:511`,
  `skinny/crates/bbnf-bench/benches/json_parity.rs:519`,
  `skinny/crates/bbnf-bench/benches/simd_scan.rs:78`,
  `skinny/crates/bbnf-bench/benches/simd_scan.rs:86`).

- `gate-json` already validates SIMD parity and reads SIMD scan metadata, but
  only the Canada structural floor is a numeric scan-floor gate. The gate hashes
  scalar vs SIMD structural offsets, requires the SIMD metadata hash to match,
  passes `simd_parity_ok` and Canada scan speed into classification, and writes
  the Markdown schema-v3 report (`skinny/crates/bbnf-bench/src/bin/gate.rs:31`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:56`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:63`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:68`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:73`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:221`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:230`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:681`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:695`). W10 no-regression therefore
  needs both the normal gate and an explicit before/after row comparison.

- Current `xtask` has the required runners. `bench-json` runs the full
  `bbnf-bench` bench suite and then calls `gate-json` on a full run
  (`skinny/xtask/src/main.rs:208`, `skinny/xtask/src/main.rs:238`), unflagged
  `gate-json` forwards to `bbnf-bench --bin gate` (`skinny/xtask/src/main.rs:241`,
  `skinny/xtask/src/main.rs:257`), `--with-cost-facts` is a separate JSON
  sidecar path (`skinny/xtask/src/main.rs:259`, `skinny/xtask/src/main.rs:290`),
  and `primitive-checkasm` already includes both W10 primitive tests
  (`skinny/xtask/src/main.rs:292`, `skinny/xtask/src/main.rs:315`).

- B6 Stage 1 is not exactly landed as specified. The shared checkasm helper
  currently compares a full `[0xDE; 1024]` canary before/after
  (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:41`,
  `skinny/crates/bbnf-simd/tests/checkasm_common.rs:52`), while the B6 design
  specifically calls for a stack-canary XOR-fold compare and says this is the
  first, smallest hardening primitive (`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:413`,
  `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:432`).
  The older `byte_class_from_eq_set_64` private helper still only prefills and
  volatile-reads the stack (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:121`,
  `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:138`),
  so W10 should claim only Stage 1 unless it also migrates that private harness.

## Recommended same-wave consumer plan and exact verification commands

1. Implement only the two W10 AArch64 bodies plus the minimal consumer route.
   For `bitmap_prefix_xor_64_neon`, use the existing scalar reference as the
   oracle and make the AArch64 fast path compute the low 64 bits of
   `vmull_p64(mask, u64::MAX)`, then invert the result when `carry_in` is true.
   Keep a scalar fallback when `target_feature = "aes"` is absent, matching the
   proven feature-gated PMULL shape already present in the non-skinny SIMD scan
   crate (`crates/simd-scan/src/parity.rs:140`, `crates/simd-scan/src/parity.rs:185`).

2. For `bitmap_next_set_bit_neon`, inline the cursor-shift + `trailing_zeros`
   logic in the AArch64 module instead of delegating to the scalar module. The
   CSSC claim must be tied to `-C target-cpu=native` or `+cssc` assembly proof,
   because A3 says baseline AArch64 emits RBIT+CLZ while CSSC emits CTZ
   (`restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:73`).

3. Wire CSSC through the existing OffsetTape scan consumer by changing
   `aarch64/bulk_emit_positions_64.rs` to enumerate set bits via
   `bitmap_next_set_bit_neon(mask, cursor)` and write `base + bit`. The runtime
   call chain then becomes:
   `scan_structurals -> neon::scan -> compact_mask -> prim::bulk_emit_positions_64 -> aarch64::bulk_emit_positions_64_neon -> bitmap_next_set_bit_neon`.
   This satisfies the same-wave consumer rule without changing generated parser
   semantics or the default tape capacity plan.

4. Land B6 Stage 1 as a shared helper change: replace
   `checkasm_common::stack_canary_then` with the XOR-fold compare from the B6
   design. Do not claim the broader B6 hardening axes unless W10 also lands raw
   ABI shims, x86_64 register sentinels, and cycle counters.

5. Prove the body and consumer wiring before measurement:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny

# Build and test the primitive layer.
cargo test -p bbnf-simd --release --test checkasm_bitmap_prefix_xor_64
cargo test -p bbnf-simd --release --test checkasm_bitmap_next_set_bit
cargo test -p bbnf-simd --release --test checkasm_bulk_emit_positions_64
cargo run -p xtask --release -- primitive-checkasm

# Prove the CSSC consumer is not orphaned.
rg -n 'bitmap_next_set_bit(_neon)?|prim::bitmap_next_set_bit' \
  crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs \
  crates/bbnf-simd/src/lib.rs \
  crates/runtime/src/grammars/json/scan.rs

# Prove PMULL and CTZ are emitted on the native AArch64 target.
rm -rf /tmp/skv7-w10-r4-asm
CARGO_TARGET_DIR=/tmp/skv7-w10-r4-asm \
RUSTFLAGS="-C target-cpu=native -C target-feature=+cssc,+aes" \
  cargo rustc -p bbnf-simd --release --lib -- --emit=asm
rg -n '\bpmull\b' /tmp/skv7-w10-r4-asm/release/deps/*.s
rg -n '\bctz\b' /tmp/skv7-w10-r4-asm/release/deps/*.s
```

6. Capture a same-host baseline before source edits, then compare W10 after
   implementation. Use the same `CARGO_TARGET_DIR` so Criterion can compare
   against the saved baseline:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv7-w10-r4-bench
rm -rf "$CARGO_TARGET_DIR"
cp RESULTS.md /tmp/skv7-w10-r4-before.RESULTS.md

# Pre-edit structural scan baseline for all fixtures.
cargo bench -p bbnf-bench --bench simd_scan -- --save-baseline skv7-w10-r4-pre
```

After implementation:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv7-w10-r4-bench

cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- primitive-checkasm

# Criterion statistical comparison for the exact W10 scan consumer.
cargo bench -p bbnf-bench --bench simd_scan -- --baseline skv7-w10-r4-pre

# Full suite refresh and schema-v3 gate. Advisory mode is appropriate for W10
# local no-regression because the current full report already has pre-existing
# N-direct / NoGo rows; formal SK-V7 close still uses the non-advisory SPEC run.
cargo run -p xtask --release -- bench-json --advisory
cargo run -p xtask --release -- gate-json --advisory
cp RESULTS.md /tmp/skv7-w10-r4-after.RESULTS.md
```

7. Compare all published row Mbps and verdicts. This command fails on any
   Verdict downgrade and on any Track 1 or Track 2 drop beyond the configured
   threshold. The default threshold is 2.0% to match the Criterion noise
   threshold; set `MAX_DROP_PCT=0` after three same-host reruns if the wave owner
   wants a literal no-drop median gate.

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
MAX_DROP_PCT=2.0 python3 - /tmp/skv7-w10-r4-before.RESULTS.md RESULTS.md <<'PY'
import os
import sys

before_path, after_path = sys.argv[1], sys.argv[2]
max_drop = float(os.environ.get("MAX_DROP_PCT", "2.0")) / 100.0
verdict_rank = {
    "GO": 0,
    "GO-WITH-FOCUS": 1,
    "CONDITIONAL": 2,
    "NO-GO": 3,
    "INVALID": 4,
}

def parse(path):
    rows = {}
    header = None
    with open(path, encoding="utf-8") as handle:
        for line in handle:
            if not line.startswith("|"):
                continue
            cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
            if cells and cells[0] == "Corpus":
                header = cells
                continue
            if not header or not cells or cells[0].startswith("---"):
                continue
            if len(cells) != len(header):
                continue
            row = dict(zip(header, cells))
            if "Corpus" in row and "Workload" in row:
                rows[(row["Corpus"], row["Workload"])] = row
    return rows

def number(value):
    try:
        return float(value)
    except ValueError:
        return None

before = parse(before_path)
after = parse(after_path)
failures = []

for key, old in sorted(before.items()):
    if key not in after:
        failures.append(f"{key}: missing after row")
        continue
    new = after[key]
    old_v = old.get("Verdict", "")
    new_v = new.get("Verdict", "")
    if verdict_rank.get(new_v, 99) > verdict_rank.get(old_v, 99):
        failures.append(f"{key}: verdict downgraded {old_v} -> {new_v}")
    for column in ("Track 1 Mbps", "Track 2 Mbps"):
        old_n = number(old.get(column, ""))
        new_n = number(new.get(column, ""))
        if old_n is None or new_n is None or old_n <= 0:
            continue
        if new_n < old_n * (1.0 - max_drop):
            pct = (new_n / old_n - 1.0) * 100.0
            failures.append(f"{key}: {column} {old_n:.0f} -> {new_n:.0f} ({pct:.2f}%)")

if failures:
    print("W10 no-regression failures:")
    for failure in failures:
        print(" - " + failure)
    sys.exit(1)

print(f"W10 no-regression gate passed with MAX_DROP_PCT={max_drop * 100:.2f}")
PY
```

8. Run the formal SPEC protocol only when the wave owner is ready to classify
   against the full current goalset:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

## Risks/pre-blocked routes

- Do not land `bitmap_next_set_bit_neon` as body + checkasm only. Current code
  does not route `compact_mask` through it, despite older ledger prose saying
  `BITMAP_NEXT_SET_BIT` is consumed by `compact_mask` (`skinny/REDRESS.md:1243`,
  `skinny/REDRESS.md:1248`). The live source is authoritative here:
  `compact_mask` calls `bulk_emit_positions_64`, and the AArch64 bulk emitter
  currently delegates to scalar.

- Do not make `CapacityPlan::OneShotSimd` the default or turn
  `attach_structural_index` into a parser-side prepass just to create a
  consumer. HANDOFF pre-blocks capacity prescan and EventCursor-style sidecars
  (`restart/skinny/tranches/sk-v7/HANDOFF.md:84`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:93`), and the parser default is
  deliberately grow-only today (`skinny/crates/runtime/src/tape/assembler.rs:21`,
  `skinny/crates/runtime/src/tape/assembler.rs:28`).

- Do not reopen the contract-only `FSM_DISPATCH_THREADED`,
  `FRAME_PUSH_BOUNDED`, or `FRAME_POP_BOUNDED` bodies. HANDOFF pre-blocks
  REDRESS 50-72, REDRESS 28+33, and EventCursor/parallel-prepass families
  (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:83`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:84`,
  `restart/skinny/tranches/sk-v7/HANDOFF.md:93`), and REDRESS says primitives
  without consumers cannot be credited toward SOTA (`skinny/REDRESS.md:1255`,
  `skinny/REDRESS.md:1267`).

- Do not over-claim CSSC without assembly proof. A3 explicitly ties the CTZ
  claim to `target-cpu=native` or the CSSC target feature
  (`restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md:73`).
  Release profile settings alone do not name CSSC; the verification commands
  must compile with native/+cssc and grep emitted assembly.

- Do not treat a non-advisory `gate-json` NoGo as a W10-specific failure unless
  the row comparison shows W10 caused it. Current `RESULTS.md` is already
  `N-direct / NoGo`, while the report also defines Track 1 and Track 2 semantics
  and sidecar provenance (`skinny/RESULTS.md:216`, `skinny/RESULTS.md:219`).
  W10 no-regression should be judged by checkasm, SIMD scan Criterion compare,
  advisory hard-failure gate, and the before/after RESULTS diff.

- Do not claim full B6 hardening from Stage 1. The B6 design defers raw
  `sigsetjmp` recovery, forced tier dispatch hooks, and FFI ABI shims
  (`restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:73`,
  `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md:81`).
  W10 should land the XOR-fold canary compare because SPEC names Stage 1, then
  leave the broader harness rewrite to a later scoped hardening wave.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- `restart/skinny/tranches/sk-v7/research/wave-9-plan.md`
- `restart/skinny/tranches/sk-v7/research/skv7-A3-dav1d-esoterica.md`
- `restart/skinny/tranches/sk-v7/research/skv7-B6-checkasm-hardening.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/benches/simd_scan.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `crates/simd-scan/src/parity.rs`
