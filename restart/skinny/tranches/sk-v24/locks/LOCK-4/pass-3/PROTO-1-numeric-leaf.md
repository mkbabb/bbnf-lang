# PROTO-1 (numeric-leaf) — LOCK-4 pass-3 probe 1: the numeric-leaf election lever — bring the emitted f64 path to incumbent parity on canada (family 4B COMPTIME-FUSION)

**Agent:** PROTOTYPE, pass 3, LOCK-4 (THE COMBINATOR CORE), probe 1.
**Relation to prior deliverable:** this RESOLVES the open lever named at the end of the
baseline `PROTO-1.md` ("the go/no-go rests on whether the emitted `-> f64` path is brought to
incumbent parity"). Delivered under a distinct filename so the baseline PROTO-1/PROTO-2 are
not clobbered.
**Measured HEAD (bbnf-lang worktree):** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
  base-pin `ce51c36fc` (XL-H). Worktree HEAD == base-pin exactly; `git merge-base
  --is-ancestor ce51c36fc HEAD` → GREEN. **Wrong-artifact-risk: NONE.**
**Incumbent core (parse-that, READ-ONLY):** shared via `.cargo/config.toml`
  `parse_that = { path = "../parse-that/rust/parse_that" }` — emitted `JsonParser` and
  incumbent `json_parser()` link the *identical* parse_that core → true apples-to-apples.
**Date:** 2026-07-16. **Toolchain:** cargo 1.96.0-nightly (eb94155a9).

---

## QUESTION

The baseline PROTO-1 proved the Futamura codegen route generalizes to recursive JSON with 0
SmallBox / 0 dyn on the hot path, but **regresses 1.50–1.60× vs the SHIPPED `json_parser()`
on canada (all-f64)**, the loss localized entirely to the emitted `number -> f64` leaf. The
incumbent's tuned leaf uses eisel_lemire + an 8-digit SWAR mantissa fold (simdjson
`parse_eight_digits`) + a 16-byte SIMD digit stripe + a pure-integer fast path; the emitted
leaf walked the mantissa **one byte per iteration** and always routed through eisel_lemire.
**Is this single numeric leaf the whole lever?** If bringing the emitted f64 leaf to
incumbent parity clears canada to ≤ 1.05× without regressing data/twitter/citm, 4B is a
uniform Rust fast-face beat and electable; if not, the residual numeric gap is 4B's named
ceiling and the codegen route stays workload-conditional.

## METHOD

Changed the emitter number-leaf lowering (`crates/core/src/backend/rust/emitter/shapes/
number.rs`), then regen `json.rs` (`cargo xtask regen --grammar json`, exit 0; sha
`5cbe7e5f9…`→`b692ba7e1…`, only `json.rs` changed). Two changes, both faithful ports of the
incumbent's `scan_number_strict_f64` / `number_parts_to_f64`:

1. **SWAR 8-digit chunking** for the integer AND fractional digit runs — port
   `parse_eight_digits` (simdjson multiply-shift fold) + `all_eight_are_ascii_digits` (SWAR
   digit classification) from `parse_that::parsers::scan::number`, emitted once per grammar
   beside `parse_number_fallback`. The emitted parser's input is **unpadded**
   (`input.as_bytes()`), unlike parse_that's NUL-padded `PaddedView`, so every 8-byte read is
   bounds-guarded `*p + 8 <= len`. The **16-byte SIMD stripe** (`scan_digits_simd`) was
   deliberately NOT ported: it *requires* the padded view to read up to 16 bytes past the
   public tail, which the unpadded emitted input cannot supply without re-architecting the
   whole `parse()` input contract (a padded buffer shifts `len`/EOF semantics across every
   leaf — object/array/string). The parsimonious probe is the SWAR fold that is safe on an
   unpadded slice.
2. **Pure-integer fast path** — a pure integer of ≤ 18 digits is exactly representable, so
   cast `mantissa as f64` directly and skip eisel_lemire (mirrors `number_parts_to_f64`).

eisel_lemire (`compute_f64`) was ALREADY called by the emitted leaf; its mantissa/exponent
inputs are bit-identical before/after, so numeric results are unchanged — a pure scan-speed
change. Bench: `cargo bench -p bbnf --features competitor --bench json_monolithic --bench
json_parse_that` under `[profile.bench]` (lto = "fat", codegen-units = 1). Corpora: data
(35 KB), twitter (631 KB), citm (1.7 MB), canada (2.2 MB all-f64). Two windows, both binary
orders. **Fastest-sample** statistic (contention-robust). loadavg 4.9–5.9 both windows.

## WHAT RAN

- Base-pin ancestry verify — GREEN (HEAD == pin).
- Emitter edit + `cargo xtask regen --grammar json` — exit 0; regenerated `json.rs` carries
  `all_eight_are_ascii_digits` / `parse_eight_digits_swar`, both digit loops rewritten to
  SWAR, and the `is_pure_integer` fast-path gate (verified by grep).
- `cargo bench … --bench json_monolithic --bench json_parse_that` fat-LTO, both benches
  compiled clean (1 warning), both benches' pre-timing correctness asserts passed on every
  corpus file (running-parse proof for BOTH arms). Two windows RAN, exit 0.

---

## NUMBERS — window 1 (canonical order: monolithic then parse_that; loadavg 5.95→4.91)

Emitted = `JsonParser::parse`, Incumbent = `json_parser()`. **fastest** sample (µs/ms):

| corpus | dominant | Emitted fastest | Incumbent fastest | ratio (emit/inc) | gate | verdict |
|---|---|---|---|---|---|---|
| data (35 KB) | small/mixed | 20.04 µs | 27.83 µs | **0.720×** | ≤ 1.0 | PASS (emit WINS) |
| twitter (631 KB) | strings/struct | 385.0 µs | 452.9 µs | **0.850×** | ≤ 1.0 | PASS (emit WINS) |
| citm (1.7 MB) | mixed | 1.099 ms | 1.065 ms | 1.032× | ≤ 1.05 | PASS (≈tied) |
| canada (2.2 MB) | **all f64** | 3.759 ms | 2.409 ms | **1.560×** | ≤ 1.05 | **FAIL** |

## NUMBERS — window 2 (reversed order: parse_that then monolithic; loadavg 4.86→5.27)

| corpus | Emitted fastest | Incumbent fastest | ratio (emit/inc) | gate | verdict |
|---|---|---|---|---|---|
| data | 18.99 µs | 26.99 µs | **0.704×** | ≤ 1.0 | PASS (WINS) |
| twitter | 369.2 µs | 442.8 µs | **0.834×** | ≤ 1.0 | PASS (WINS) |
| citm | 1.067 ms | 1.063 ms | 1.004× | ≤ 1.05 | PASS (tied) |
| canada | 3.771 ms | 2.410 ms | **1.565×** | ≤ 1.05 | **FAIL** |

**Both windows × both orders agree within noise: data 0.70–0.72×, twitter 0.83–0.85×, citm
1.00–1.03×, canada 1.56×.** Compared to the baseline PROTO-1 (canada 1.50–1.60×), **canada is
UNCHANGED** — the SWAR mantissa fold + integer fast path did NOT move it.

---

## WHY THE LEVER DID NOT FIRE (the finding)

The 8-digit SWAR fold is inert on canada because **canada's floats have short digit runs.**
Canada is a wall of coordinates like `-73.981824` / `43.420273`: integer part 1–3 digits,
fractional part typically 6–9 digits, each number terminated by `,` `]` or whitespace. The
`*p + 8 <= len` guarded 8-byte probe therefore almost always straddles the delimiter, so
`all_eight_are_ascii_digits` returns false and the scan immediately falls to the
bounds-checked scalar tail — the same byte-at-a-time loop as before. The integer fast path
never fires on canada (every value is a float). Net effect on canada: ~0.

The incumbent's canada advantage lives in exactly the two things this probe could NOT port
safely:
- The **16-byte SIMD digit stripe** (`number_simd::scan_digits_simd`) that scans a canada
  fractional run in one vector op — it reads a full 16-byte window at any offset `≤ len`,
  which is only sound on parse-that's **NUL-padded `PaddedView`**. The emitted parser feeds
  an unpadded `input.as_bytes()`; supplying padding is not a leaf swap but a `parse()`
  input-contract change that re-bases `len`/EOF for every leaf (string/object/array).
- **`get_unchecked` scalar indexing** throughout the incumbent's `scan_number_mantissa` vs
  the emitted leaf's checked `input[*p]` on the short-run tail — a per-byte bounds check that
  dominates precisely when the SWAR/SIMD wide path does not engage (i.e. canada).

## VERDICT

**RAN_MEASURED — FAIL on the strict election gate. The numeric-leaf lever, as the parsimonious
SWAR-fold + integer-fast-path port, is NOT sufficient; canada stays at 1.56× (gate ≤ 1.05×).**

- data/twitter **PASS** (emit WINS, 0.70–0.85×); citm **PASS** (tied, ≤ 1.03×); **canada
  FAIL** (1.56×, unchanged from baseline).
- **4B's named numeric ceiling:** the canada residual is NOT the mantissa fold — it is the
  16-byte SIMD digit stripe + unchecked padded-view indexing, both gated on giving the
  emitted parser a **padded input buffer**. That is a larger architectural move than a leaf
  swap (it re-bases the `parse()` input contract across every shape), so it is out of scope
  for a single-lever probe and is recorded here as 4B's ceiling.
- **Election consequence:** the codegen route stays **workload-conditional** — a uniform beat
  on string/structure-heavy (data −28/−30%, twitter −15/−17%) and mixed (citm tied), a
  standing ~1.56× loss on number-dense f64 (canada). 4B is electable as the Rust fast face
  for everything EXCEPT number-dominated workloads; canada parity requires the padded-input
  re-architecture (SIMD stripe + `get_unchecked`), which is the next probe, not this one.
