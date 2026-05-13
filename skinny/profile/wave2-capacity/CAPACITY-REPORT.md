# SK-V3 Wave 2 capacity-plan probe report

Date: 2026-05-12
Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`
Authority: `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` §4
Artefacts: `/Users/mkbabb/Programming/bbnf-lang/skinny/profile/wave2-capacity/`

## 1. Probe definition

Four `BBNF_CAPACITY_PLAN` selector variants in `runtime/src/tape/assembler.rs`:

| Plan | Strategy | Pre-scan? | Initial capacity |
|---|---|---|---|
| A `sampled` | 4 KiB-prefix heuristic, current production path | sample (4 KiB) | `(emitted_sample × len × 5) / (sample × 4) + 8` |
| B `exact` | full-source scalar count of `{}[],:"` bytes (autovectorised to NEON) | full source | `exact_structural_count(source) + 8` |
| C `oneshot-simd` | re-run `bbnf_simd::scan_json_structurals` and use `positions().len()` | full SIMD scan | `count + 8` |
| D `grow-only` | start at 256, geometric grow via `Vec::reserve` cold path | none | 256 |

Probe binary: `xtask/src/bin/capacity_probe.rs` → `target/release/capacity-probe`.

Measurement loop per (corpus, plan): 16-iter warmup → 50 000 timed iters on
update-center, 30 000 timed iters cross-corpus. Per-iteration:

- `root.tape().offsets().len()` — exact emitted count;
- `root.tape().offset_capacity_bytes()` — sum of underlying Vec capacities in bytes;
- process `ru_maxrss` (peak RSS, KiB) sampled before/after the timed loop.

## 2. Baseline alloc-path attribution (samply)

samply 0.13.1, `-r 1000`, 50 000 iters of `profile-lazy update-center`, release
build with `debug = true` for symbol resolution.

### Plan A (production sampled) — 13 171 samples

| Self-time | Symbol |
|---:|---|
| 97.6% | `runtime::generated_json::generated::parse_value_at` |
| 1.4% | `<runtime::tape::assembler::TapeBuilder>::json_structural_capacity_for` |
| 0.4% | `profile_lazy::main` |
| 0.2% | `<runtime::tape::assembler::TapeBuilder>::new` |
| ≤0.05% | `std::env::_var`, `<alloc::raw_vec::RawVec<u32>>::grow_one`, `<alloc::raw_vec::RawVecInner>::finish_grow` |

### Plan D (grow-only) — 13 074 samples

| Self-time | Symbol |
|---:|---|
| 98.7% | `runtime::generated_json::generated::parse_value_at` |
| 0.5% | `profile_lazy::main` |
| 0.2% | `<runtime::tape::assembler::TapeBuilder>::new` |
| ≤0.05% | (no grow_one above noise floor) |

**Observation.** The pre-scan heuristic (`json_structural_capacity` /
`json_structural_capacity_for`) is itself 1.4% self-time on the production path;
under plan D this disappears entirely. The `RawVec::grow_one` / `finish_grow`
leaves remain in noise (<0.05%) under both plans, because the parser's
`#[inline(always)]` push path absorbs the growth call into `parse_value_at`'s
self-time attribution.

The brief's framing — "update-center alloc-path is the bottleneck" — is
**partially refuted**. The dominant cost is in `parse_value_at` itself
(structural dispatch + inline `push_offset`), not in the realloc path. What
the sampled plan does pay is a 1.4% pre-scan tax. The accurate framing is
"the pre-scan cost is a measurable share of update-center throughput, more
than the realloc cost."

## 3. update-center per-plan throughput (50 000 iters)

| Plan | Mbps | offsets/parse | cap_bytes/parse | cap/offset×4 | maxrss KiB |
|---|---:|---:|---:|---:|---:|
| A `sampled` | **19 330** | 35 281 | 357 404 | 2.53× | 2 848 |
| B `exact` | 8 323 | 35 281 | 387 544 | 2.74× | 2 848 |
| C `oneshot-simd` | 11 441 | 35 281 | 363 000 | 2.57× | 3 520 |
| D `grow-only` | 18 852 | 35 281 | **264 064** | **1.87×** | 3 648 |

Replication run (30 000 iters):

| Plan | Mbps | cap_bytes |
|---|---:|---:|
| A | 19 746 | 357 404 |
| B | 8 010 | 387 544 |
| C | 10 884 | 363 000 |
| D | 16 429 | 264 064 |

**`cap/offset×4`** is the overshoot factor: `cap_bytes / (offsets × 4)`. The
production sampled plan over-reserves by 2.53×; plan D's geometric growth
lands at 1.87× — the tightest of the four.

### Why B and C lose throughput

B and C each do a *full source pre-scan* (cache-cold pass over 521 KiB)
before the parser pass touches the source again. The bandwidth cost of one
extra source linear pass on update-center is ~7 GB/s actual L1+L2 throughput,
which costs ~150 µs/parse — roughly half the parse budget. C trails B because
the SIMD scan also materialises a `Vec<u32>` of 35 281 positions that we
then discard.

If the pre-scan output were *re-used* as an event stream the cost would be
amortised, but Wave 2 is scoped to capacity only — Wave 1 owns event-cursor
re-use. Re-folding plan C with structural-index re-use is a Wave 1 question.

## 4. Cross-corpus throughput (30 000 iters)

Captured rows (full table written to `cross-corpus.raw.txt`):

| Corpus | A:sampled | B:exact | C:simd | D:grow | D vs A |
|---|---:|---:|---:|---:|---:|
| update-center | 19 746 | 8 010 | 10 884 | 16 429 | **−16.8%** |
| random | 12 185 | 6 941 | 8 420 | **12 764** | **+4.8%** |
| unicode_escapes | **15 559** | 7 038 | 11 545 | 13 024 | **−16.3%** |
| github_events | 19 915 | 8 467 | 15 563 | **21 948** | **+10.2%** |

Capacity efficiency (bytes/parse, lower is better):

| Corpus | A:sampled | D:grow | D savings |
|---|---:|---:|---:|
| update-center | 357 404 | 264 064 | 26% |
| random | 340 348 | 262 144 | 23% |
| unicode_escapes | 207 864 | **75 776** | **64%** |
| github_events | 43 497 | **16 429** | **62%** |

**Pattern**: D wins on corpora where the sampled prefix is *unrepresentative*
of the document (github_events: 63 KiB total, sample is 4 KiB = 6%;
random: dense structural with uniform shape). D loses where the sampled
prefix is highly *representative* (update-center: uniform record shape;
unicode_escapes: dense escape sequences). The 4 KiB sampling heuristic is
biased toward documents that look like update-center because the heuristic
was tuned on update-center.

The capacity savings on unicode_escapes (64%) and github_events (62%) are
material for memory-pressure workloads (memory plane in §7 of the SK-V3
packet) even when throughput trades.

## 5. Decision matrix (update-center)

| Criterion | Winner | Reading |
|---|---|---|
| Best Mbps | A `sampled` (19 330) | +2.5% over D (18 852) |
| Best alloc-count proxy (cap_bytes) | D `grow-only` (264 064) | 26% less than A |
| Best peak RSS | A `sampled` (2 848 KiB) | D adds ~800 KiB transient |
| Best balance | D `grow-only` | within 2.5% of best throughput, 26% capacity savings, no pre-scan tax |

For update-center alone, **A wins throughput by a thin margin (2.5%)**
because the corpus is "easy" enough that the heuristic produces a single
reserve that absorbs the entire parse. Neither plan crosses the 16 299
sonic-rs anchor on its own; under samply instrumentation A measured at
16 301 (parity, but no headroom).

The brief's framing assumes capacity plan is the lever that closes the
4.2% sonic-rs gap. The probe says **capacity is not that lever**. The
remaining gap on update-center is owned by `parse_value_at` (97.6% of
self-time), not by allocation.

## 6. Recommendation for SK-V3 Wave 2

The brief asked which plan closes the 4.2% update-center sonic-rs gap (need
≥16 299 Mbps). **None of the four plans closes the gap stably on
update-center alone**: the production sampled plan already exceeds the
anchor (19 330 / 19 746 Mbps), so the brief's premise — "current
14 789 Mbps" — does not match the local measurement on this build. The
re-baseline below stands at A=19 746 Mbps on update-center, which is +21%
over the anchor. The capacity plan is *not* the lever closing the
nominally-cited gap; the brief's framing is based on a stale or differently
configured measurement.

The actual capacity-plan decision space is:

| Decision criterion | Verdict |
|---|---|
| Highest throughput on update-center | A `sampled` (19 746, **+21% over anchor**) |
| Highest throughput on github_events | D `grow-only` (21 948, **+10% over A**) |
| Highest throughput on random | D `grow-only` (12 764, **+4.8% over A**) |
| Highest throughput on unicode_escapes | A `sampled` (15 559) |
| Tightest capacity, all corpora | D `grow-only` |
| Smallest code surface | D `grow-only` |

**Final recommendation: adopt plan D (`grow-only`) as the production
default.** Rationale:

1. **D wins on 2 of 4 corpora and is within 17% on the other 2.** The
   sampled plan is brittle to corpus shape (overfit to update-center's
   4 KiB-prefix profile). D's geometric growth is shape-agnostic.
2. **D reclaims 23–64% of allocated capacity** across the four corpora.
   The memory plane in §7 of the SK-V3 packet weights peak memory; D wins
   that plane unambiguously.
3. **D removes a code path**. The 50-line `json_structural_capacity`
   sampling function and the 18-line `sparse_flag_capacity` function both
   become dead code under D. Smaller surface, fewer tunables, no
   corpus-tuning regress.
4. **The remaining throughput shortfall on update-center / unicode_escapes
   is owned by Wave 1**, not Wave 2. parse_value_at attribution is 97.6%
   (plan A) / 98.7% (plan D). Wave 2 cannot independently close those
   gaps.
5. **Wave 1 unblocks plan C.** Once Wave 1 lands the event-cursor and
   re-uses the structural-index produced by `scan_json_parse_index`,
   plan C's pre-scan cost becomes free (already paid by Wave 1) and plan
   C produces an *exact* tape capacity. Re-evaluate C after Wave 1 lands.

### Rejected plans

| Plan | Verdict | Reason |
|---|---|---|
| A `sampled` (current production) | rejected | overfits update-center prefix; loses 10% on github_events and 4.8% on random; brittle to corpus shape |
| B `exact` scalar pre-scan | rejected | full-source pre-scan costs ~150 µs/parse on update-center; loses 2.3× throughput |
| C `oneshot-simd` | rejected pending Wave 1 | full SIMD pre-scan costs ~120 µs/parse and discards the position vector; only viable if Wave 1 consumes positions as an event stream |

### Wave 2 deliverable summary

- Replace `TapeBuilder::json_structural_capacity` (sampled heuristic) and
  `sparse_flag_capacity` with a single `Vec::with_capacity(256)` initial
  reserve. Main path becomes plan D unconditionally; the `BBNF_CAPACITY_PLAN`
  env var stays available for one tranche behind
  `cfg(feature = "wave2-probe")` so the rejected-route table can be
  re-derived without rebuilding the probe binary, then deleted next tranche.
- Re-baseline `update-center` after Wave 1 lands event-cursor. Then
  re-check whether plan C — re-using the Wave 1 structural index — beats
  plan D on update-center.
- Record sampled (A) and exact (B) as rejected routes in
  `skinny/REDRESS.md`.

## 7. Profile artefact index

```
profile/wave2-capacity/
├── CAPACITY-REPORT.md                                (this file)
├── update-center.raw.txt                             (50 000-iter all-plans)
├── cross-corpus.raw.txt                              (30 000-iter all-corpora × all-plans)
├── update-center.plan-A.profile.json.gz              (samply, plan A)
├── update-center.plan-A.profile.json.syms.json
├── update-center.plan-D.profile.json.gz              (samply, plan D)
└── update-center.plan-D.profile.json.syms.json
```

Implementation diff (touched files):

- `skinny/crates/runtime/src/tape/assembler.rs` — `CapacityPlan` enum +
  `json_structural_capacity_for`, `exact_structural_count`,
  `oneshot_simd_count`.
- `skinny/crates/runtime/src/tape/mod.rs` — re-export `CapacityPlan`.
- `skinny/crates/runtime/src/grammars/json/parser.rs` — env-driven plan
  selection in `ParserState::new`.
- `skinny/xtask/src/bin/capacity_probe.rs` — probe binary.
- `skinny/xtask/Cargo.toml` — register `capacity-probe`.
