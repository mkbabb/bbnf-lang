# SK-V18 S-P1 — A1: JSON Hot-Leaf Attribution

## Provenance & honesty caveats

- **Capture**: `restart/skinny/tranches/sk-v18/research/p1/raw/json_sample.txt` —
  macOS `sample` of `profile_direct` (pid 72210), 1 ms interval.
- **Host load**: `capture.log` records `host_loadavg: 4.35 6.03 5.70` — this sample ran
  under **concurrent-session machine load**. Absolute Mbps elsewhere in this pass is
  DIRECTIONAL/depressed vs the W0 baseline. This A1 analysis depends only on the
  **relative hot-leaf RANK**, which is load-robust; no absolute throughput is asserted here.
- **Workload driven** (confirmed by reading `profile_direct.rs`): mode `track1` →
  `run_once` line 156 → `bbnf_bench::direct_struct::track1_digest` → the
  **direct-to-struct `JsonDigestSink`** path. The sample's hottest stack
  (`profile_direct.rs:156` → `direct_struct.rs:423` → `parse_object_value_at_direct<…JsonDigestSink>`)
  matches exactly. This is the SinkOnlyProgram / direct-projection path, NOT the
  tape/`parse_only` recognizer.

## Top-8 self-time leaves (per-leaf self-sample ranking)

Source: the `Sort by top of stack, same collapsed (when >= 5)` block (lines 332–345).
Self-share = leaf count / sum-of-all-leaf-counts. Total leaf samples = **6169**.

| # | self | share | demangled function | module / role |
|---|------|-------|--------------------|----------------|
| 1 | 4924 | **79.82%** | `runtime::generated_json::generated::parse_object_value_at_direct::<…,JsonDigestSink>` | generated grammar parser — object-value byte dispatch + inlined object/array recursion |
| 2 | 722 | **11.70%** | `runtime::generated_json::generated::parse_array_element_at_direct::<…,JsonDigestSink>` | generated grammar parser — array-element byte dispatch |
| 3 | 213 | **3.45%** | `parse_that_regex::unescape_string` | string/escape handling (Cow unescape; fast `\`-free borrow path) |
| 4 | 97 | 1.57% | `mach_absolute_time` (libsystem_kernel) | timer syscall — harness/measurement noise, NOT product |
| 5 | 70 | 1.13% | `_platform_memmove` (libsystem_platform) | bulk byte copy (string materialization / sink moves) |
| 6 | 38 | 0.62% | `_xzm_free` (libsystem_malloc) | allocator free |
| 7 | 34 | 0.55% | `_xzm_xzone_malloc_tiny` (libsystem_malloc) | allocator small-alloc |
| 8 | 33 | 0.53% | `parse_that_regex::number::materialize_u64` | number parse — integer fast-path (mantissa shortcut, ≤19 digits) |

Tail (<0.2% each): `_free` 0.16%, `<deduplicated_symbol>` 0.15%, `_malloc_zone_malloc`
0.13%, `_xzm_xzone_malloc` 0.10%, `DYLD-STUB$$memcpy` 0.08%.

### Concentration

- The two generated parsers (`parse_object_value_at_direct` + `parse_array_element_at_direct`)
  hold **91.52%** combined self-time. These functions inline the full inner loop
  (`parse_string_direct`, `consume_literal_direct`, number dispatch, recursion into
  `parse_object_direct`/`parse_array_direct`) under `inline(always)` — they ARE the
  byte-at-a-time scanner + value dispatcher + sink-call site fused into one body.
- `libsystem_malloc` family combined = 1.70% — allocator pressure is small but nonzero
  (sink string materialization), a secondary G1 concern, not a leaf to chase.
- `mach_absolute_time` (1.57%) is pure measurement overhead from the harness loop; exclude
  from any product attribution.

## SK-V18 generalization mapping (G1 preserve / G5 cover)

**G1 = grammar-driven JSON projection** (the typed sink/CSSOM-equivalent product that the
generated generator must reproduce). **G5 = grammar-NEUTRAL NEON classifier** (the shared
structural-scan layer that must be backend-agnostic).

| leaf | obligation | rationale |
|------|-----------|-----------|
| `parse_object_value_at_direct` (79.82%) | **G1 — MUST preserve** | This is the per-value byte-dispatch + recursion + sink-emit that the grammar-driven generator emits today. Its fused, scan-free, branch-on-first-byte shape is *the* >SOTA product. The new generator must regenerate an equivalent body (same inlining, same sink call sites) — any indirection/devirtualization regression here directly costs the JSON win. |
| `parse_array_element_at_direct` (11.70%) | **G1 — MUST preserve** | Same dispatch family for array context. Preserve fused inline + monomorphized-sink shape. |
| `unescape_string` (3.45%) | **G1 — preserve (projection helper)** | String materialization on the typed projection. Hot because the digest realizes string bytes. Generator must keep the fast `\`-free `Cow::Borrowed` early-out; this is a projection-side cost, not a classifier candidate. |
| `materialize_u64` (0.53%) | **G1 — preserve (projection helper)** | Number value materialization fast-path (mantissa shortcut). Tiny but on the product path; keep the integer fast-path intact. |
| `_platform_memmove` (1.13%) | **G1-adjacent** | Bulk copy under sink string materialization; reduce via fewer owned copies, but it follows from the projection, not a neutralizable scanner. |
| malloc family (1.70%) | **G1-adjacent** | Sink allocation pressure; an allocator-discipline target on the projection path, not G5. |
| `mach_absolute_time` (1.57%) | **neither** | Harness timer; exclude. |

### json/scan.rs (S-P0 R12 non-neutral holdout) verdict

`runtime/src/grammars/json/scan.rs` exists and provides a NEON structural scanner
(`scan_structurals` → `neon::scan`, the `bbnf_simd` classify-tbl4 path). **It does NOT
appear as a hot leaf — it appears ZERO times anywhere in `json_sample.txt`** (no `scan`,
`structural`, `neon`, or `StructuralIndex` symbol in the capture).

Confirmed by reading the generated direct path: the `track1_digest` →
`parse_direct` (generated.rs:760) → `parse_object_value_at_direct` (:823) body **never
calls the NEON pre-scanner `scan_structurals`** (defined `json/scan.rs:22`, invoked only
from `json/scan.rs:51` capacity-planning and `:296` test — never from a parse path) and
**never calls the `attach_structural_index` hook** (generated.rs:12, which is itself a
**no-op** `let _ = state;` and is reached only from the tape `parser.rs:49`, not the direct
path). The `structural`-named symbols that DO appear in `json/generated.rs` —
`consume_structural` (:290) and `parse_only_take_structural` (:638) — are single-byte
literal matchers (consume one `{`/`[`/`,`/`:` structural byte), **not** a SIMD pre-scan; so
the direct path is **scan-free** (pure byte-at-a-time dispatch in
`parse_object_value_at_direct`). The NEON `json/scan.rs` (`StructuralIndex`/`neon::scan`) is
only on the tape/structural-scan probe path (`structural_scan_only` masking probe,
`parse_only` tape recognizer), not on the profiled >SOTA product.

**Therefore the R12 holdout is CHEAP-TO-NEUTRALIZE, not a G5 generalization target.**
Because the >SOTA JSON direct path does not touch `json/scan.rs`, generalizing/retiring its
non-neutral bespoke shape costs nothing on the measured product — there is no hot-leaf to
preserve through G5 on this path. (If a future S-P-pass moves the direct path onto a
structural pre-scan, `json/scan.rs` would re-enter the hot set and become a real G5 target;
today it does not.)

## Bottom line

- The JSON >SOTA product is **91.5% concentrated in two generated grammar parsers**
  (`parse_object_value_at_direct`, `parse_array_element_at_direct`) — both **G1 MUST-preserve**:
  the grammar-driven generator has to re-emit the same fused, scan-free, monomorphized-sink
  byte-dispatch body or the win regresses.
- The only non-grammar product leaves are `unescape_string` (3.45%) and `materialize_u64`
  (0.53%) — G1 projection helpers; keep their fast paths.
- No G5 (NEON classifier) leaf is hot on this path. The bespoke `json/scan.rs` R12 holdout is
  **absent from the profile** → cheap-to-neutralize, not a generalization target for S-P1.
