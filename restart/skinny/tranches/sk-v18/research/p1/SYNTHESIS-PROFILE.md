# SK-V18 S-P1 — SYNTHESIS-PROFILE (the binding profile ledger for S-P2)

Consolidates A0 (`a0-rebaseline-ledger.md`), A1 (`a1-json-hot-leaves.md`), A2
(`a2-css-hot-leaves.md`) into one profile-first ledger. Source captures live under
`restart/skinny/tranches/sk-v18/research/p1/raw/` (`css_canon_n200.txt`, `css_sample.txt`,
`json_sample.txt`, `capture.log`). The `capture.log` header stamps `0fbee121f`, but that is
the **S-P0 audit-convergence SHA only** — the `track1_rich` rows it carries could not have
come from `0fbee121f` (the `css_canon_bench.rs` harness with `track1_rich` did not exist
there; it was created by the bit-rot fix `784ceb418`, the current HEAD). The benched binary
is **`784ceb418`** (A0). Host: aarch64 / Apple M5 Max only — x86 is a
prune target and was not measured.

## 0. LOAD HONESTY (read first)

`capture.log` line 2 records **host_loadavg 4.35 6.03 5.70** (1m/5m/15m) — this entire pass
ran under **concurrent-session machine load**. Today's lightningcss medians
(402/299/475/480 Mbps) sit far below the W0-LOCKED quiet bars
(bootstrap 1112.393 | tailwindcss 841.332 | material-components-web 1292.260 | animate
1218.685), confirming a uniform load depression that hits our parser and lightningcss alike.

**Absolute Mbps this pass is DIRECTIONAL/depressed and is NOT re-locked as a baseline.** The
two load-bearing, load-robust outputs — invariant to a uniform host slowdown — are:
(1) the **same-run >SOTA ratios** `track1_rich/lcss` and `track1_full_parse/lcss`, and
(2) the **relative hot-leaf RANK**. A quiet re-capture is required before any absolute claim.

A canonical-harness **bit-rot fix** is recorded (committed `784ceb418`): the W1-pruned
`track1_fact_stream` String workload no longer compiled and was replaced with
`track1_rich = parser::rich_summary` (9 materialized fields). Per the source-of-truth comment
(`css_l4_declaration_values/generated.rs:297-304`) `rich_summary` is **lazy-rich** — it
re-derives every field from `(source, offset)` spans, writing nothing to the arena
("lazy, not eager"). The honest H1 framing (matching A0 + §125 below) is therefore
**full-value-materialization, lazy-rich** vs the lightningcss full CSSOM — equal-depth typed
value work, not a count-only structural probe; this closes residual R14/H1 for the canonical
harness.

## 1. DIRECTIONAL >SOTA RE-CONFIRMATION (load-robust — VERDICT: PASS on all 4 corpora)

The rich typed-CSSOM projection (`track1_rich`, the >SOTA product) BEATS the lightningcss
full-CSSOM bar same-run on every corpus, and the recognizer (`track1_full_parse`) stays
ahead everywhere:

| corpus                  | track1_rich / lcss | track1_full_parse / lcss | verdict |
|-------------------------|-------------------:|-------------------------:|:-------:|
| bootstrap               |              2.190 |                    2.338 | PASS    |
| tailwindcss             |              3.375 |                    3.387 | PASS    |
| material-components-web |        1.658 (min)  |                    1.894 | PASS    |
| animate                 |              2.101 |                    1.992 | PASS    |

Smallest margin is material-components-web (rich 1.658×) — still a comfortable beat. JSON's
>SOTA position (beats sonic-rs strict) is carried forward from the W0 lock; this pass did not
re-bench JSON throughput, only its hot leaves. tailwindcss shows pathological load-tail
dispersion (track1_full_parse stddev 159.8) but medians remain robust.

## 2. JSON HOT LEAVES + G1/G5 MAPPING

Workload = `profile_direct` mode `track1` → `direct_struct::track1_digest` → the
direct-to-struct `JsonDigestSink` path (the SinkOnly projection; NOT the tape recognizer).
Total leaf samples = 6169.

| # | share  | leaf | obligation |
|---|-------:|------|-----------|
| 1 | 79.82% | `parse_object_value_at_direct<…JsonDigestSink>` | **G1 MUST-preserve** |
| 2 | 11.70% | `parse_array_element_at_direct<…JsonDigestSink>` | **G1 MUST-preserve** |
| 3 |  3.45% | `parse_that_regex::unescape_string` | G1 projection helper (keep `\`-free Cow borrow) |
| 4 |  1.57% | `mach_absolute_time` | harness timer — exclude |
| 5 |  1.13% | `_platform_memmove` | G1-adjacent (string materialization) |
| 6–7 | 1.17% (top two: `_xzm_free` 0.62% + `_xzm_xzone_malloc_tiny` 0.55%) | malloc/free family | G1-adjacent (sink alloc pressure; **full** `libsystem_malloc` family across all 6 entries = **1.70%**, per A1) |
| 8 |  0.53% | `materialize_u64` | G1 projection helper (integer fast-path) |

- **G1**: leaves 1+2 = **91.52%** combined — the fused, scan-free, monomorphized-sink
  byte-dispatch + inlined recursion that IS the >SOTA JSON product. The grammar-driven
  generator MUST re-emit equivalent bodies (same `inline(always)` shape, same sink call sites);
  any devirtualization/indirection regression here directly costs the JSON win.
- **G5 (NEON classifier)**: **NO hot leaf on this path.** The direct `track1_digest` path is
  entirely scan-free (`json/generated.rs` has no `scan_structurals`/`StructuralIndex`).
- **`json/scan.rs` (S-P0 R12 holdout)**: appears **ZERO times** in the sample. Its NEON
  scanner lives only on tape/`parse_only`/`structural_scan_only` probe paths.
  **Verdict: CHEAP-TO-NEUTRALIZE, not a G5 target** — generalizing/retiring it costs nothing
  on the measured product.

## 3. CSS HOT LEAVES + G6 WIRE-OR-RETIRE

Driver = `css_track1_profile.rs` looping `parse_full` = the `CssFullParser` structural
recognizer that underlies BOTH `track1_full_parse` and `track1_rich` (`rich_summary` runs
this scan then projects 9 fields). Parser denom = 4379 (the `main` corpus loop, 1770 samples /
28.8% total, is harness overhead — excluded).

| rank | leaf | parser-share | role |
|---|---|---:|---|
| 1 | `find_component_delim` | **79.5%** | scalar byte-at-a-time delimiter scan |
| 2 | `consume_balanced_at`  | 14.6%     | inner-recursive half of the same scan |
| 3 | `parse_block`          | 4.2%      | block driver |
| 4 | `parse_declaration`    | 1.4%      | |
| 5 | `parse_at_rule`        | 0.3%      | |

> **SCALAR-SCAN SHARE = 4121/4379 = 94.1%** of parser self-time (`find_component_delim` +
> `consume_balanced_at` — the same scalar delimiter/balanced-skip machine; generated.rs:662–680
> and 693–713). `find_component_delim` alone = 79.5%.

**G6 VERDICT = WIRE** (numeric basis: 94.1% ≫ ~8% wire threshold; #1 leaf alone 79.5%), under
dav1d discipline: scalar reference FIRST, checkasm-style differential parity over the corpora,
**aarch64 NEON only**. RETIRE is rejected — deleting a kernel that targets a 94%-share path is
the wrong move.

**Sharp caveat (R7, verified by inspection):** the existing dead NEON kernels
`runtime_simd::find_css_significant` and `find_comment_close` have ZERO live callers (only
`#[cfg(test)] mod tests` in `runtime/src/lib.rs`). They were written for a DIFFERENT/flatter
function than the hot leaf: `find_css_significant` is a flat stop-at-delimiter skip, whereas the
hot `find_component_delim`+`consume_balanced_at` machine recurses through nested `()[]{}` and
skips strings/comments. So the NEON does NOT cover the dominant hot path as written — it must be
**RETARGETED to the live generated scan**, not wired as-is. Wiring is the honest action;
retire only `find_comment_close` if retargeting to the balanced-consume recursion proves unsafe.
**Grammar-neutrality constraint (Lock-14 / G3):** `find_component_delim` is replicated
byte-identically across **7 `css_l4_*/generated.rs`** files (the P3-collapse replicas), so the
retargeted NEON must land as a **shared grammar-neutral runtime primitive the generated scan
CALLS** (the `runtime_simd` surface), NOT bespoke vector code re-emitted per-grammar into each
`generated.rs`. The WIRE consumes the **P3-collapsed single CSS scan**; the emitter calls the
shared primitive, the generator does not author the kernel — else it re-forks the very shape G3
un-forks. This makes the G6 WIRE sequencing-dependent on P3 (collapse first, then wire the
singular call site).

## 4. WHAT S-P2 (RESEARCH) MUST GROUND THE GENERATOR PROJECTION IN

The grammar-driven generator must re-emit, from the grammar's `->` typed materialization, the
exact hot bodies the profile attributes the >SOTA win to:

- **JSON SinkOnly projection leaves** — `parse_object_value_at_direct` +
  `parse_array_element_at_direct` (91.5%): regenerate the fused, scan-free,
  monomorphized-sink byte-dispatch + inlined recursion; keep `unescape_string` (Cow borrow)
  and `materialize_u64` fast paths. No structural pre-scan on this path.
- **CSS rich-projection leaves** — the recognizer-only sample did NOT surface projection
  symbols (driver calls `parse_full`, not `rich_summary`), so **S-P2 MUST sample
  `rich_summary` directly**. The projection surface to ground: `rich_summary` (generated.rs:305,
  9-field node loop), `nodes()` lazy span re-walk (:307, preserve-rich-ast: lazy not eager),
  `CssDeclaration::typed_value`→`CssTypedValue::classify` (:201/:229, per-declaration value-head
  byte classification — the rich-plane hot candidate), `selector_count`/`is_at_rule` (:316/:309).
  Expect `classify` + `nodes()` re-walk to dominate once the shared scalar scan is NEON-accelerated.
- **Scalar-scan disposition** — the CSS shared scan (94.1%) is the G6 NEON retarget target
  (WIRE after retargeting to `find_component_delim`); the JSON `json/scan.rs` is off the product
  path (cheap-to-neutralize). Two opposite dispositions, both grounded in measured share.

## 5. SEQUENCING REMINDER

S-P1 is a **HARD DEPENDENCY** of G5/G6: no orphan kernel may be authored without a
profile-anchored hot leaf to justify it. **Profile-first.** The measured shares above are the
mandate: JSON G5 has no hot leaf (do not author a JSON classifier); CSS G6 has a 94.1% scalar
hot path (author the NEON retarget). Any kernel without a corresponding entry in §2/§3 is an
orphan and is out of scope for the generalization.

---
**Next move: ready-for-S-P2.**
