# PROVE-07 — DIRECT-TO-STRUCT FUSED (F4)

**Verdict: PARTIAL** (measured; gate compound-unmet). The direct-to-struct typed
lane is now a first-class, parity-verified, cold/paired/PMU measured lane in
`json_canon_bench`. It **beats both SOTA typed deserializers on every corpus**
(sonic-rs typed by 1.30–1.65×, serde_json typed by 1.26–1.92× cyc/B), so the
gate's sonic arm (≥1.0× on twitter) **passes at 1.30×** — but the serde arm
(≥2.0× on twitter) is unmet: direct-to-struct is only **1.26×** faster than
serde_json on twitter. The 2× bar is reached only on citm (1.92×).

Host: arm64 (aarch64 NEON + scalar), 18 cores. Base commit `d2b32f6e3`
(sk-v22 graduation baseline). System allocator (no mimalloc in the canon bin).

---

## 1. What I built

The direct-to-struct emitter, its hand-authored `DirectSchemaSet` schema, the
generated single-pass struct-fillers (`generated_real_typed::parse_twitter_search`
et al.), the typed struct set (`real_typed_struct::{TwitterSearch,Tweet,…}`), and
the strict-product parity/checksum harness (`direct_struct::{track1,serde,sonic}_
strict_product`, `assert_direct_strict_product_parity`) **already exist at HEAD**.
None of that is new. What was missing was a *measured lane* — the sk-v22 JSON cold
instrument (`json_canon_bench.rs`) times only the digest lanes (parse_only /
parse_tape / fused / sonic / serde), never the typed struct product.

PROVE-07 makes the direct-to-struct product a first-class arm of that instrument
(the R2 §6 recipe: new lane fns + `emit_paired` calls), across all three modes:

- **Three typed lanes** (`json_canon_bench.rs`), corpus-keyed so the fixture
  resolves internally, reusing the existing strict-product wrappers:
  - `direct_struct` → `direct_struct::track1_strict_product` → `generated_real_typed::parse_*` (the fused single-pass typed struct-filler — the competitive SINK lane, **not** fold-over-cursor; A1/R5 refuted the cursor fold 2.3–4.9×).
  - `serde_typed` → `serde_strict_product` → `serde_json::from_slice::<T>`.
  - `sonic_typed` → `sonic_strict_product` → `sonic_rs::from_slice::<T>`.
- **Parity FIRST**: a preflight in `main()` runs `assert_direct_strict_product_parity`
  for every fixture corpus before *any* timing; a timed lane that materializes the
  wrong struct is worthless. Prints `# PARITY corpus=… direct==serde==sonic OK`.
  (Byte-identical `typed_checksum` across all three — verified for twitter/canada/citm.)
- **PMU mode**: appends the three typed lanes to the per-corpus cyc/byte loop
  (guarded by `supports_strict_product`).
- **Paired mode**: interleaves `direct` / `serde_typed` / `sonic_typed` inside one
  sample and emits the certified gate ratios `direct/serde_typed` and
  `direct/sonic_typed` (load hits both sides of each ratio equally).
- **Cold table**: adds the three typed lanes (`sample` refactored to a closure so
  the digest and typed families share the stats logic — net DRY, not sprawl).

### Diff summary

Single file, purely additive to the bench harness — **zero** runtime / codegen /
grammar changes. `patch.diff` captures the whole 401-line
`crates/bbnf-bench/src/bin/json_canon_bench.rs` (298 restored from the sk-v22
worktree `wf_e940452a` verbatim — it was never git-committed — + **~103 lines** of
PROVE-07 additions: the `TypedFn`/`TYPED_WORKLOADS` table + 3 lane fns, the parity
preflight, the three per-mode typed sub-blocks, the `sample` closure refactor, and
a `print_row` helper).

---

## 2. Measured results (twitter = gate corpus)

### 2a. PMU cyc/byte — the CERTIFIED metric (load-robust per-process counter)

`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, iters=800, **mean of 3 windows**
(canada mean of 2), loadavg 15–18 (18-core host). Lower = faster.

| corpus | direct_struct | serde_typed | sonic_typed | **direct vs serde** | **direct vs sonic** |
|---|--:|--:|--:|--:|--:|
| **twitter** | **1.858** | 2.334 | 2.410 | **1.26×** | **1.30×** |
| citm_catalog | **1.015** | 1.953 | 1.634 | **1.92×** | **1.61×** |
| canada | **8.165** | 10.816 | 13.437 | **1.32×** | **1.65×** |

Per-window twitter direct_struct: 2.034 / 1.821 / 1.718 cyc/B (windows tight; the
1.26×/1.30× ratios reproduce every window).

### 2b. Paired interleaved wall ratios — LOAD-CERTIFIED (N=200, one heavy window)

ratio = direct_ns / baseline_ns, <1 ⇒ direct faster; 95% CI (normal approx).
loadavg 22.24 (18-core host — deliberately heavy; the paired ratio nulls drift).

| corpus | direct/serde_typed [95% CI] | direct/sonic_typed [95% CI] |
|---|--:|--:|
| **twitter** | **0.817** [0.806, 0.827] → 1.22× | **0.787** [0.777, 0.796] → 1.27× |
| canada | 0.730 [0.722, 0.738] → 1.37× | 0.595 [0.588, 0.601] → 1.68× |
| citm_catalog | 0.567 [0.560, 0.574] → 1.76× | 0.657 [0.647, 0.666] → 1.52× |

### 2c. Cold wall table (N=200, median ns/byte — directional)

| corpus | direct | serde_typed | sonic_typed | direct vs serde | direct vs sonic |
|---|--:|--:|--:|--:|--:|
| twitter | 0.4735 | 0.5860 | 0.6183 | 1.24× | 1.31× |
| canada | 2.0387 | 2.8222 | 3.8248 | 1.38× | 1.88× |
| citm_catalog | 0.3075 | 0.5884 | 0.5781 | 1.91× | 1.88× |

**All three methods concur**: on twitter direct-to-struct is ~1.24–1.30× faster
than both serde and sonic typed; on citm it reaches ~1.9×.

---

## 3. Verdict vs gate

Gate: **≥2× vs serde_json AND ≥1.0× vs sonic-rs typed on twitter, cold paired PMU.**

- ≥1.0× vs sonic-rs typed → **1.30× (PMU) / 1.27× (paired) — PASS.**
- ≥2.0× vs serde_json → **1.26× (PMU) / 1.22× (paired) — FAIL** (bar is 2.0×).

Compound gate → one arm unmet → **PARTIAL**. This is a measured shortfall, not
noise: the twitter serde CIs are tight and every PMU window agrees on ~1.26×.

**Why twitter under-performs the 2× serde bar (honest structural read):** the
`TwitterSearch { statuses: Vec<Tweet { id, text }> }` struct set is a *thin*
projection — for each of ~100 tweets only `id` and `text` are captured and the
rest of every tweet object is structurally skipped (`DirectParser::skip_value` →
`skip_object`/`skip_string_raw`). serde_json with `#[serde(default)]` skips the
same bulk via `IgnoredAny`, which is comparably tight. So the direct lane's edge
over serde collapses to the ~1.26× of avoiding serde's `Deserialize`/`Visitor`
dispatch, **not** a materialization win. Where the struct captures more of the
document and the skip/materialize asymmetry is larger — citm (1.92×) — the lane
approaches the bar. Against sonic-rs (SIMD skipper) the direct lane still wins
1.30× on twitter because sonic pays SIMD setup on a 631 KB doc that the scalar
single-pass amortizes better on this thin projection.

The lane is nonetheless a genuine, reproducible win: **faster than both SOTA
typed deserializers on all three corpora, by 1.26–1.92×.**

---

## 4. Integration cost + exact seam

**Cost: ~103 lines, one file, zero runtime/codegen touch.** The emitter
(`codegen/json_typed_direct.rs`), schema bridge (`lower/schema_direct.rs`),
hand-authored `DirectSchemaSet` (`xtask/real_typed_schema.rs`), generated fillers
(`bbnf-bench/generated_real_typed.rs`), and the strict-product parity harness
(`bbnf-bench/direct_struct.rs`) are all pre-existing and unchanged. The seam is
exactly R2 §6: add `fn lane(...) -> u64` beside the existing lanes, register it,
and add an `emit_paired(...)` call.

**The one open wire (out of PROVE-07 scope, = PROVE-08):** the schema is
hand-authored, not grammar-derived — `Project.ty` is dropped at
`passes/src/lib.rs:203` and `DirectBuildField.target` is always `None`. Threading
that unlocks `emit_typed_from_source` deriving the schema from `->` projections;
until then a new struct set needs a hand-written `DirectSchemaSet` row.

---

## 5. Visual verification — hot-path disassembly

`#[inline(always)]` + fat-LTO fold the twitter chain
(`parse_twitter_search`→`parse_type_twitter_search`→`parse_type_tweet`→
`parse_string`/`parse_u64`/`skip_value`) into the one surviving symbol
`bbnf_bench::real_typed_struct::track1_typed`. `otool -tV` of its head shows the
fixture enum lowered to a **jump table** and the whitespace-skip inlined as a
straight-line **SWAR 8-byte scan** — no tape, no cursor indirection, no call into
a value tree:

```
_..track1_typed:
  ...
  and   x8, x1, #0xff                ; fixture discriminant
  adrp  x9, 0x100271000
  add   x9, x9, #0x7ee               ; jump-table base
  ldrh  w11, [x9, x8, lsl #1]        ; table[fixture]
  add   x10, x10, x11, lsl #2
  br    x10                          ; dispatch → Twitter arm (single-pass filler)
  ...
  ; --- inlined skip_ascii_whitespace: SWAR 8-byte whitespace classify ---
  ldr   x11, [x21, x19]              ; load 8 source bytes
  eor   x12, x11, #0x2020202020202020
  add   x12, x12, x8                 ; x8 = -0x0101…0102
  bic   x11, x12, x11
  and   x11, x11, #0x8080808080808080
  cmp   x11, x9                      ; x9 = SWAR ws-mask constant
  b.eq  <advance 8 and continue>
  eor   x10, x11, #0x8080808080808080
  rbit  x10, x10
  clz   x10, x10                     ; first non-ws byte offset
  ...
  cmp   w10, #0x20 / #0xd / #0x9 / #0xa   ; scalar ws tail (space/CR/TAB/LF)
```

Straight-line scalar + SWAR, exactly the fused single-pass sink the family targets.

---

## 6. Reproduce

```
# from <worktree>/skinny — restore the (untracked) canon bin first if needed:
cp .../wf_e940452a-431-7/skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs \
   crates/bbnf-bench/src/bin/json_canon_bench.rs     # then apply patch.diff
cargo build --release -p bbnf-bench --bin json_canon_bench
./target/release/json_canon_bench paired 200   # certified gate ratios (+ parity preflight)
./target/release/json_canon_bench pmu 800      # certified cyc/byte (mean 3 windows)
./target/release/json_canon_bench 200          # cold wall table
```

Raw captures: `/tmp/prove07_{paired,pmu,pmu2,pmu3,cold}.txt`.
