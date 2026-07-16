# PROVE-06 — PATH API + STOP-EARLY (F4)

**Verdict: GO.** Skinny's stop-early `get` beats sonic-rs `get_from_str` on **2 of 3**
path classes cold-paired (shallow ~1.32×, missing ~1.45×; deep loses ~0.47×), robust
across 5 runs (loadavg 12–24). Byte-exact parity with sonic on every path.

Worktree base aligned to recon HEAD `7f3be27c2` (the base the R1/R2/R5/R6 pointers
were verified against; the worktree was minted 42 commits stale at `d2b32f6e3`).

---

## 1. What was built

A tape-free, stop-early JSON pointer `get` on the F4 lazy seam. Additive, grammar-side,
zero name-gates, sheets not carried.

| File | Δ | Concern |
|---|---|---|
| `crates/runtime/src/grammars/json/scan.rs` | +~115 | **skip engine**: `skip_container` (NEON jump-to-matching-bracket) + `skip_tail` scalar; factored `neon::block_emit_mask` out of the dormant `neon::scan` (shared escape-aware string-suppression — DRY, parity-preserved) |
| `crates/runtime/src/grammars/json/path.rs` | +176 (new) | **path get**: `Seg`, `Located`, `get(&str,&[Seg])->Option<Located>`; object/array member walk, `skip_value` over the skip engine, escape-aware key compare |
| `crates/codegen/src/json_templates/view.rs` | +18 | `JsonValue::pointer(&[Seg])` — eager-tape navigation over the `ValueRef`/`Cursor` seam (contrast lane + oracle) |
| `crates/codegen/src/runtime_generator.rs` | +2 | `JSON_MOD_RS`: `pub mod path;` + `pub use path::{get, Located, Seg};` (regen wiring, not a hand-patch) |
| `crates/bbnf-bench/src/bin/json_path_bench.rs` | +~330 (new) | cold N≥200 paired + PMU cyc/get harness vs sonic-rs |

`json_templates/view.rs` is the include_str template; `grammars/json/{mod,view}.rs` were
**regenerated** (`cargo xtask regen-json`), never hand-edited. `path.rs`/`scan.rs` are
hand-authored runtime siblings (not in the emitted roster, like `sink.rs`).

**The mechanism.** `get` walks the source bytes directly, resolving each segment and
stopping the instant the target is located — it never builds the offset tape. Subtrees
it steps over are jumped with `scan::skip_container`, which re-activates the **dormant
NEON structural scanner** (`scan.rs`, neutralized since SK-V18) as a jump-to-matching-
bracket primitive: the proven 64-B block loop (lo6 `tbl` classify + escape-aware quote
suppression) produces the structural-emit mask; a scalar depth-count over the sparse
bracket bits returns the matching close. String bounds come from `match_string`, scalars
from a delimiter scan.

---

## 2. Measured results — cold paired, N=200, twitter (631 515 B)

Paired per-sample interleave (mine burst vs sonic burst in one sample; load hits both
sides of every ratio). **Ratio = mine / sonic; <1 ⇒ skinny faster.** Median of 5 runs:

| class | pointer | mine ns | sonic ns | **ratio** | ×faster | verdict |
|---|---|--:|--:|--:|--:|:--:|
| shallow | `statuses[0]` | 1249 | 1690 | **0.755** | 1.32× | **WIN** |
| deep | `statuses[0].user.screen_name` | 1033 | 511 | 2.108 | 0.47× | loss |
| missing | `statuses[0].nonexistent` | 2022 | 3007 | **0.688** | 1.45× | **WIN** |

Per-run paired ratios (loadavg): shallow `0.770 / 1.008 / 0.755 / 0.754 / 0.745`
(12/16/24/23/23) — win 4/5, one load-16 outlier tie; deep `2.07/2.13/2.07/2.12/2.11`
— loss 5/5; missing `0.698/0.781/0.682/0.688/0.668` — win 5/5.

**PMU cyc/get (certified HW counter, iters=20 000, corroboration):**

| class | mine cyc | sonic cyc | ratio | vs sonic get_unchecked |
|---|--:|--:|--:|--:|
| shallow | 5247 / 7722 | 6958 / 7866 | 0.75 / 0.98 | 0.74 / 1.00 |
| deep | 4552 / 5319 | 2035 / 2559 | 2.24 / 2.08 | 3.42 / 3.09 |
| missing | 8185 / 11121 | 12153 / 13758 | 0.67 / 0.81 | 1.03 / 1.28 |

PMU direction matches paired on every class. (Against the *unchecked* unsafe path sonic
reclaims missing to a tie; the gate targets `get_from_str`, the checked variant.)

**Stop-early proof.** Prefix `statuses[0].id` resolves at **byte 204 / 631 515 =
0.032 %** of the document scanned — the bounded parse never touches the tail.

**Contrast (why tape-navigation is not the lane):** eager `tape_pointer` (full
`parse` + `JsonValue::pointer`) costs **~1.26 M cyc (2.0 cyc/B)** for *every* path —
150×–1700× the stop-early `get` (734–8185 cyc). Building the tape to answer one query
loses to sonic `get` catastrophically; the tape-free stop-early lane is the win.

### Parity (correctness oracle)
`get(...).as_raw_str()` is **byte-identical** to `sonic_rs::get_unchecked(...).as_raw_str()`
on all four paths (shallow object, deep string `"ayuu0123"`, missing→None, prefix number
`505874924095815681`); `JsonValue::pointer` presence agrees. Gate asserts parity before
timing.

---

## 3. Analysis — where the reused NEON skip wins

The wins are **skip-bound** classes (shallow = one bulk container skip; missing = exhaust
a member list, jumping the nested `user`/`entities` objects) — exactly where
`skip_container`'s NEON jump dominates. The losses are **descent-bound** (deep/prefix
walk keys with little to skip); there scalar `match_string` + scalar key compare trail
sonic's SIMD key scanner. Coherent and honest: the dormant scanner, re-activated as the
skip engine, carries the path classes it was built to carry.

---

## 4. Visual verification — NEON hot path (`otool -tvV`, `scan::neon::block_emit_mask`)

```
movi.16b  v0, #0x3f          ; lo6 mask (0x3f)
movi.16b  v16, #0x22         ; '"' terminator
cmeq.16b  v7,  v18, v16      ; quote compare, block lane
shrn.8b   v7,  v7,  #0x4     ; vshrn mask-narrow idiom  (movemask.rs spine)
ushr.8b   v7,  v7,  #0x3
tbl.8b    v29, { v29 }, v27  ; lo6 structural classify (classify_tbl4)
ushl.2d   v29, v29, v21      ; prefix-xor string suppression
```

`skip_container` and `block_emit_mask` survive LTO as named symbols
(`runtime::generated_json::scan::…`); the `tbl`/`cmeq`/`shrn`/`ushr` kernel is the reused
dormant scanner, live on the get hot path.

---

## 5. Verdict vs gate & integration

**Gate:** beat sonic-rs get on ≥2 of 3 path classes, cold paired → **shallow + missing
win (deep loses) = 2/3 → GO.**

**Integration cost — low, additive.** Consumer seam: `runtime::generated_json::{get,
Seg, Located}` — one free function `get(&str, &[Seg]) -> Option<Located>` returning a
raw-slice handle (`as_raw_str`). No parser change, no tape change, no name-gate; the
skip engine is an additive `pub fn` on the existing (dormant) `scan` module and the
`pointer` verb rides the untouched `ValueRef`/`Cursor` lazy seam. Wired into the JSON
roster through the codegen template + one `regen-json`. Sheets untouched (R5 carry).
Remaining gap: deep-descent key matching is scalar — a SIMD key scanner (future) would
close the descent-bound loss.
