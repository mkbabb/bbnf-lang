# PROVE-10 — Prefilter-tier acceleration for the bbnf-regex DFA (F5)

**Verdict: GO.** A start-state memchr prefilter productionised into a new unanchored
`Dfa::find` delivers **9.6x-29x match-throughput** on prefilterable patterns over the
pristine byte-by-byte scan, with **zero regression** on non-prefilterable patterns
(0.99x), **neutral compile time** (the untouched axis), and **44/44 bbnf-regex tests
green**. Cold, N=200, paired lanes in one binary.

Target engine: `bbnf-regex` (`parse-that/rust/regex`, scratch copy per R3 section 8).

---

## 1. What was built (the ONE deepest win: option (b), prefilter tier)

Per the brief's parsimony edict I chose **one** win — the prefilter tier from R7 section 4
(regex-automata's memchr/rare-byte prefilter ladder), not lazy determinization. Rationale:
it is the cleanest GO with *guaranteed* correctness (the DFA is untouched; the prefilter
only decides **where** to attempt an anchored match) and *guaranteed* zero cost on the
compile axis (a one-time O(256) derivation).

Mechanism, in three parts:

1. **`automata/prefilter.rs`** (new, 97 LOC) — `Prefilter::from_dfa` reads the compiled
   DFA's start-state transition row: every byte whose equivalence class has a non-dead
   transition out of state 0 is an *entry byte* (the only bytes that can begin a match).
   When that set is 1/2/3 bytes it becomes `Byte1/Byte2/Byte3`, dispatched to
   `memchr`/`memchr2`/`memchr3`; otherwise `None`. This reuses the exact `Memchr1/2/3`
   tiering vocabulary already present in `accel.rs`, applied at match *entry* instead of at
   a self-loop state. If the start state accepts (empty match) -> `None`.

2. **`Dfa` gains a `prefilter` field**, derived once in `from_nfa` after the transition
   table is flattened (Step 6, O(256)).

3. **`Dfa::find`** (new, unanchored leftmost search) — branches once on the prefilter:
   inactive -> the pristine `find_at`-across-every-offset scan (byte-identical work);
   active -> `memchr` skips runs that cannot begin a match, then an anchored `find_at`
   confirm at each candidate. Marked `#[inline]` so the inactive path fuses into the caller
   with zero function-boundary cost.

Colocation: `prefilter.rs` is a sibling module of `dfa.rs`/`accel.rs`/`nfa.rs` inside the
`automata/` directory-module — the established pattern (accel.rs is the precedent), not a
flat dump.

### Diff summary (patch.diff, 245 insertions, 6 files, additive-only)

| File | delta | What |
|---|---|---|
| `automata/prefilter.rs` | +97 (new) | `Prefilter` enum + `from_dfa` + `next_candidate` |
| `automata/dfa.rs` | +46 | `prefilter` field, Step-6 derivation, `Dfa::find` |
| `benches/regex_dfa_compile.rs` | +96 | skip-heavy corpus + paired scan lanes + correctness assert |
| `automata/mod.rs`, `lib.rs` | +2 | module registration + `pub use Prefilter` |
| `regex/Cargo.toml` | +4 | `memchr = "2.5"` (aarch64 NEON backend; scalar elsewhere) |

No @generated file touched. No x86. No stub/shim/legacy alias.

---

## 2. Measured results

Harness: divan, `--bench regex_dfa_compile`, `[profile.bench]` = pristine fat-LTO / cgu=1 /
O3 (the repo's real bench profile — a foreign `lto=false` edit that appeared in the shared
scratch tree was discarded). Cold per-parse, **N=200**, throughput via divan `BytesCount`.
Corpus = one synthetic **~131 KB** skip-heavy document: lowercase lorem prose (carrying
neither `#` nor digits) with a target token every few hundred bytes, so a single-byte-prefix
pattern's start byte is genuinely rare. **Lanes** `scan_scalar` (pristine byte-by-byte
`find_at` loop) vs `scan_prefilter` (`Dfa::find`) run back-to-back in the **same binary** —
paired, same load. The two lanes assert an **identical match count** at setup (correctness
gate) before timing.

### 2a. Match throughput — scan lane (corpus = 131 KB skip-heavy, N=200, loadavg 11.04)

| Pattern | Prefilter tier | scalar median | prefilter median | **ratio** | prefilter GB/s |
|---|---|---:|---:|---:|---:|
| `0x[0-9a-fA-F]+` | Byte1(`0`) | 100.2 us | 3.458 us | **28.98x** | 37.96 |
| `https?://[a-zA-Z0-9./-]+` | Byte1(`h`) | 110.0 us | 6.958 us | **15.81x** | 18.86 |
| `#[0-9a-fA-F]{3,6}` | Byte1(`#`) | 106.2 us | 6.916 us | **15.36x** | 18.98 |
| `(?:foo\|bar)[a-z]*` | Byte2(`f`,`b`) | 116.2 us | 12.12 us | **9.59x** | 10.82 |
| `[a-z]+` (control) | None | 126.0 us | 127.3 us | **0.99x** | — |
| `\w+` (control) | None | 126.1 us | 128.0 us | **0.99x** | — |

`h` is present in the prose (every candidate `h` costs a cheap 4-byte confirm), so `https?`
wins less than `0x`/`#` whose start bytes are absent — an honest spread, not a rigged rare
byte. The two `None` controls are the zero-regression proof: `Dfa::find` on a
non-prefilterable pattern is parity with the hand loop (the `#[inline]` fuse makes the
inactive branch byte-identical).

### 2b. Compile axis (the *other* axis) — paired, same load window (N=200, loadavg 12.58)

| Lane | fastest | median | mean |
|---|---:|---:|---:|
| pristine `dfa_compile` (no prefilter) | 54.08 us | 55.12 us | 61.07 us |
| modified `dfa_compile` (+ prefilter) | 54.24 us | 55.99 us | 60.29 us |
| **ratio (mod/pris)** | **1.003x** | 1.016x | 0.987x |

Run back-to-back from the surviving pristine binary vs the modified binary. The prefilter's
O(256) derivation is sub-microsecond on a ~55 us compile; the deltas are load noise (mean is
*faster*). **Zero regression on the compile axis.** `dfa_find_at` (anchored, code untouched)
is unchanged within noise (~0.8 us both).

### 2c. Visual verification — memchr NEON hot path (from `count_prefilter`, cargo asm)

`Dfa::find` inlines; `memchr::memchr` inlines into it. Disassembly of the bench's
`count_prefilter` shows the textbook aarch64 NEON byte-search (16-B lanes, `vshrn #4`
movemask idiom from R7 section 1) — no x86, no scalar fallback taken:

```asm
LBB30_30:                                  ; memchr('0') scan, tail-aligned probe
    bl  <*const u8 as memchr::ext::Pointer>::distance
    ldr  q0, [x10]
    ldr  q6, [sp, #64]                      ; v6 = splat(target byte)
    cmeq.16b v0, v6, v0                      ; 16-byte compare-equal
    shrn.8b  v0, v0, #4                      ; vshrn narrow -> 64-bit movemask
    fmov x8, d0
    ands x8, x8, #0x8888888888888888
    b.ne LBB30_48
    ...
LBB30_34:                                  ; 64-B/iter unrolled main loop
    ldr  q0, [x9, #-48]!
    ldp  q1, q4, [x8, #-32]
    ldr  q5, [x8]
    cmeq.16b v3, v6, v0                      ; x4 compare against splat
    cmeq.16b v2, v6, v1
    cmeq.16b v1, v6, v4
    cmeq.16b v0, v6, v5
    orr.16b  v4, v2, v3
    orr.16b  v5, v1, v0
    orr.16b  v4, v4, v5
    umaxp.16b v4, v4, v4                      ; horizontal reduce
    fmov x10, d4
    cbnz x10, LBB30_63
    add  x8, x8, #64                          ; advance 64 B
```

---

## 3. Verdict vs gate

> GATE: >=5x compile-time win on large patterns OR a measured match-throughput win on
> prefilterable patterns, with zero regression on the other axis and all ~40 bbnf-regex
> tests green.

Claiming the **match-throughput clause**: **9.59x-28.98x** on four prefilterable patterns
(far above "a measured win"); **0.99x** on non-prefilterable controls; **compile axis
neutral (1.003x fastest)**; **44/44 bbnf-regex tests green**; the paired lanes assert
identical match sets. **All conditions met -> GO.**

### Tests

`cargo test -p bbnf-regex` -> **44 passed, 0 failed** (algebra 3, egraph rules 12, egraph
simplify 3, info 11, path_lexer 15). The `parse_that --test regex` bridge (which houses the
49 dfa subtests) fails to *compile* on an unresolved `parse_that::regex::parser` import —
**verified pre-existing**: it reproduces identically with my changes stashed (it is a
parse_that-side re-export drift, orthogonal to bbnf-regex). DFA correctness is instead
covered here by the in-bench assertion that `count_scalar == count_prefilter` over 131 KB x
6 patterns, which passes.

---

## 4. Integration cost & the exact seam

Trivial and self-contained. `Prefilter` and `Dfa::find` are additive public API on the
existing `bbnf-regex` engine — no call site changes, no behavior change to `compile`,
`find_at`, or the tape. The seam into bbnf's scanning plane (F5's "one classify/DFA/prefilter
substrate serves both repos"): wherever the generator emits an unanchored regex-leaf scan it
calls `dfa.find(bytes, pos)` instead of hand-rolling a `find_at` loop, inheriting the
prefilter for free. Cost: one dep line (`memchr`, already in the parse_that side of the
workspace) + the derivation is paid once at DFA build.

## 5. Caveats (honest)

- **PMU cyc/B unavailable**: macOS restricts userspace PMU; the certified metric here is the
  divan wall-clock **ratio** of paired same-binary lanes (per protocol "ratios certified,
  absolutes directional"). GB/s are divan `BytesCount`, directional.
- **Load**: the machine ran at loadavg 11-17 (sibling provers). Ratios are paired and
  same-window, so robust; absolutes inflate under load.
- **Scratch pollution**: a sibling process wrote `parse_that/**` + `simd128.rs` and a
  `[profile.bench]` `lto=false` edit into the shared scratch tree. None link into the
  bbnf-regex bench; all were excluded from `patch.diff` (verified: 0 `parse_that` lines) and
  the profile edit was reverted so measurement used the real fat-LTO profile.
- **Scope**: only 1/2/3-byte entry sets are prefiltered (the memchr tier). Larger entry sets
  fall to `None` (parity), leaving the Teddy/shufti multi-byte tier as future work — out of
  scope for this one-win prover.
