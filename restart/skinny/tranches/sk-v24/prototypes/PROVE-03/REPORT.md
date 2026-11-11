# PROVE-03 — STRING KERNELS (F1): fused single-pass trusted string scan

**Family** F1 (kernel fusion). **Verdict GO.** **Gate** measurable cyc/B win on
twitter via `json_canon_bench` paired vs unmodified HEAD, zero parity failures —
**MET.** Host Apple Silicon aarch64 (NEON). Worktree `wf_386900ed-462-3`,
baseline HEAD `d2b32f6e3`.

## What I built

The JSON parse path (`parse_tape` product and the `fused`/`parse_direct` sink)
scans string bodies through `parse-that-regex`'s **trusted** kernel
(`match_string_at_quote_trusted_utf8`; generated parsers receive a `&str` whose
UTF-8 is already a precondition). Its inner leaf, `skip_string_plain_trusted`,
located the first terminator/escape/control byte with **two passes over the same
bytes**:

1. `memchr::memchr2(b'"', b'\\', …)` — find closing quote or escape;
2. `first_control_byte(…)` — a **separate** SWAR walk over the same span for raw
   control bytes (`< 0x20`), which JSON forbids in string bodies.

That second walk is the "separate walk" the brief targets. I fused control-byte
validation into the same stride that finds the quote/escape:

- **New NEON kernel** `bbnf_simd::aarch64::string_block::first_string_terminal`
  — ORs the terminator/escape/control compares into one predicate vector, narrows
  the 16-byte compare to a 64-bit nibble-mask with a single `vshrn_n_u16::<4>`
  (the simdjson/Lemire idiom), and extracts the first index via
  `trailing_zeros() >> 2`. No full `movemask` (no stack store, no scalar
  reduce-loop). Scalar parity anchor `first_string_terminal_scalar` beside it.
- **Rewired** `skip_string_plain_trusted` to one fused pass: NEON-16 →
  SWAR-8 (existing `string_special_mask`) → scalar tail. Deleted
  `first_control_byte` and the now-dead `control_byte_mask`; dropped the `memchr`
  dependency (its only caller).

Bit-identical by construction: old code returned the first index that is a
quote/escape or a control byte before it; the fused pass returns the first index
in `{quote, escape, control}` — the same position.

### The decisive detail (measured refutation → fix)

A first cut ORed the compares but narrowed through the shared
`movemask_u8x16`, which **stores to a stack array and runs an 8-iteration scalar
reduce loop**. Measured: instructions dropped but CPI rose 0.21→0.29 and cyc/B
**regressed** (parse_only 1.33→1.68). Replacing that with the direct
`vshrn`→u64→`tz>>2` idiom (I only need the first index, never the full mask)
flipped it to the win below. The lesson is load-bearing: the win is the *cheap
narrow*, not merely "one pass".

### Fused UTF-8 validation (honest scoping)

On the gate path UTF-8 is a `&str` precondition, so the validation that actually
rides the scan is the **control-byte** check (fused above). The untrusted,
UTF-8-validating path (`match_string(StringMode::Utf8)`) already fuses
`validate_block` into its scan loop; the corpus below proves it correct across
multibyte / surrogate-escape / malformed / invalid-UTF-8. A full branchless
Keiser–Lemire block validator would speed only that untrusted path and cannot
move the twitter (trusted) number, so it is out of the measured gate.

## Diff summary (patch.diff)

- `bbnf-simd/src/aarch64/string_block.rs` +`first_string_terminal` (NEON) +
  `first_string_terminal_scalar` + `NO_TERMINAL`. [+62 LOC]
- `parse-that-regex/src/lib.rs` `skip_string_plain_trusted` → fused single pass;
  `first_control_byte` + `control_byte_mask` deleted. [net +29/−30]
- `parse-that-regex/Cargo.toml` drop unused `memchr`.
- `bbnf-simd/tests/checkasm_string_terminal_block.rs` NEON↔scalar parity oracle
  (5 tests, 200k-block fuzz).
- `parse-that-regex/tests/prove03_string_kernels.rs` correctness corpus (7 tests:
  multibyte, surrogate-escape, malformed, invalid-UTF-8, 20k-body fuzz vs an
  independent scalar reference).

## Parity — ZERO failures

```
checkasm_string_terminal_block: 5 passed (incl. 200k fuzz blocks NEON==scalar)
prove03_string_kernels:         7 passed (incl. 20k fuzz bodies kernel==reference)
```
All parse lanes parse every corpus without error in the release bench.

## Measured — twitter (631,515 B), cold, N per below, interleaved paired

**PMU cyc/B + ins/B — 4 interleaved windows (BASE/ARM back-to-back), loadavg ~16.
cyc/B is the certified metric; ins/B is deterministic (load-independent).**

| lane | BASE cyc/B | ARM cyc/B | ratio | BASE ins/B | ARM ins/B | ins ratio |
|------|-----------:|----------:|------:|-----------:|----------:|----------:|
| parse_tape (product) | 1.564 | 1.455 | **0.931×** (−6.9%) | 9.970 | 9.065 | 0.909× |
| fused | 1.989 | 1.677 | **0.843×** (−15.7%) | 12.393 | 10.920 | 0.881× |
| parse_only | 1.338 | 1.285 | 0.960× | 6.241 | 5.089 | 0.815× |

ARM ran in the *worse* (second, higher-load) slot of every pair yet still won on
every lane in every window — the confound works against the arm; the true win is
larger. ins/B is rock-stable across the 4 windows (parse_tape BASE 5036.9/5036.5/
5039.9/5035.0 M; ARM 4579.7/4584.8/4576.9/4576.2 M).

**Paired ratios (load-certified, N=300×3 interleaved windows, loadavg ~13).**
Ratio = lane_ns / sonic_ns per sample; sonic is identical code in both binaries,
so it anchors both runs. Lower = skinny faster.

| ratio | BASE (w2,w3) | ARM | relative |
|-------|-------------:|----:|---------:|
| twitter tape/sonic | 0.505, 0.520 | 0.427, 0.444, 0.463 | ~0.87× (parse_tape ~13% faster) |
| twitter fused/sonic | 0.646, 0.642 | 0.554, 0.564, 0.550 | ~0.865× (fused ~13.5% faster) |

CIs non-overlapping for fused/sonic in all windows and for tape/sonic in w2/w3
(BASE w1 was a load-spike outlier, CI 0.43–0.82). Both certified metrics — PMU
cyc/B and paired ratio — agree: measurable twitter win on the product and fused
lanes. **Gate MET.**

## Disassembly — the fused kernel inlined into the hot loop (otool -tvV)

```
0000385c  mov      x9, x0
00003860  add      x0, x0, #0x10          ; cursor += 16
00003864  cmp      x0, x19
00003868  b.hi     0x100003898           ; fall to SWAR-8 tail
0000386c  ldr      q0, [x20, x9]          ; load 16 bytes
00003870  cmeq.16b v1, v0, v3            ; byte == '"'   (terminator)
00003874  cmeq.16b v2, v0, v4            ; byte == '\\'  (escape)
00003878  cmhi.16b v0, v5, v0            ; 0x20 > byte   (control)
0000387c  orr.16b  v1, v1, v2            ; term | esc
00003880  orr.16b  v0, v0, v1            ; term | esc | ctrl
00003884  shrn.8b  v0, v0, #0x4          ; vshrn narrow -> nibble mask
00003888  fmov     x8, d0                ; -> GPR
0000388c  cbz      x8, 0x10000385c       ; no hit -> next 16B block
00003890  rbit     x8, x8                ; first index via rbit+clz
```

One 16-byte NEON stride, one `shrn`, one `fmov`; no store, no scalar reduce loop.
The `eor #0x2222…` SWAR-8 fallback follows immediately below (label 0x3898).

## Integration cost

Near-zero. The change lands entirely inside the existing string-kernel seam
(`parse-that-regex` + `bbnf-simd`) already consumed by the generated JSON parser
via `match_string_at_quote_trusted_utf8`. No generator/codegen change, no `.bbnf`
edit, no regen; the new kernel mirrors the existing `scan_string_special_block`
shape. Deletes more than it adds on the parse-that side (drops `memchr`,
`first_control_byte`, `control_byte_mask`). Seam: swap in `first_string_terminal`
at `skip_string_plain_trusted`; ship the two test files.

## Follow-on (not in this gate)

`parse_tape`/`fused` short strings still route through the **generated scalar**
`match_tiny_plain_string_with_cap` (a byte-at-a-time loop in
`grammars/json/generated.rs`), which bypasses these kernels — routing it to a
NEON `first_string_terminal` (a generator/template change + regen) is the next
increment and would compound the parse_tape win.
