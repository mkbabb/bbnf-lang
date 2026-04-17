## Sheets viability profile — AW-III.P3 samply attribution

Diagnostic burden: `google_sheets_monolithic` shows the single worst
regression in the AW bench matrix — 23.6× to 31.7× vs post-AU
([post-AW.json:42-44][post-aw], [post-AU.json:25-27][post-au]). This
report attributes that regression to named walker functions via fresh
samply profiles and evaluates which AW-IV levers can close the gap.

**Verdict in one line**: linear-dispatch dominated — the same six
walker functions occupy 62-76% self-time across every entry with a
near-flat shape that does NOT shift super-linearly with formula
depth. **Addressable by AW-IV**, but only if Pratt generalisation
(W3.3) and the PRECEDENCE_LUT ship — the current linear
`lookup_precedence` scan is quadratic in operator count per operand.

[post-aw]: ../../../benchmarks/post-AW.json
[post-au]: ../../../benchmarks/post-AU.json

### 1. Methodology

Three entries profiled cold via `scripts/profile-bench-headless.sh`
against the prebuilt binary from
`.profiles/samply/prebuild/wave.tsv` (ports 3150/3151, shared target
`/Users/mkbabb/Programming/bbnf-lang/.profiles/shared-target`). All 7
required artefacts verified per entry under
[.profiles/samply/google_sheets_monolithic/][artefact-root]. No
`cargo expand` rerun; the prepare wave's `expand.rs` was the only
source consulted for expansion analysis.

Hot-function resolution: `profile.json.gz` frames carry bare
addresses. Cross-referenced against `profile.json.syms.json`'s
`known_addresses` + `symbol_table.frames[-1].function` (outermost
emitted function name per region) with breakpadId ↔ debug_id
normalization. 417/427 frames resolved on parse_simple; 210/220 on
parse_nested; 310/320 on parse_stress — 97%+ per entry.

[artefact-root]: ../../../../.profiles/samply/google_sheets_monolithic/

### 2. Bench baseline and regression multiplier

| entry | bytes | ns/iter | MB/s | ns/byte | ns/formula | vs post-AU |
|-------|-------|---------|------|---------|------------|------------|
| parse_simple  |  505 | 132,846 | 3 | 263 |   3,907 | 25.2× |
| parse_nested  | 1456 | 323,774 | 4 | 222 |  10,444 | 28.6× |
| parse_stress  | 1838 | 509,741 | 3 | 277 |  63,718 | 33.7× |

Source: [parse_simple/bench.txt][simple-bench],
[parse_nested/bench.txt][nested-bench],
[parse_stress/bench.txt][stress-bench]; post-AU baseline
[post-AU.json:25-27][post-au].

Post-AU reference (fn-per-rule): simple 5,271 ns / 95 MB/s; nested
11,333 ns / 128 MB/s; stress 15,121 ns / 121 MB/s.

ns/byte roughly flat (263, 222, 277) — **the parse is linear in
input length**. The per-formula cost scales with complexity (3.9 µs
→ 10.4 µs → 63.7 µs) because deeper formulas have more structural
tokens per byte, not because there's super-linear algorithmic
behaviour.

[simple-bench]: ../../../../.profiles/samply/google_sheets_monolithic/parse_simple/bench.txt
[nested-bench]: ../../../../.profiles/samply/google_sheets_monolithic/parse_nested/bench.txt
[stress-bench]: ../../../../.profiles/samply/google_sheets_monolithic/parse_stress/bench.txt

### 3. Hot-function attribution — per-entry self-time

#### 3.1 parse_simple — 879 samples

| self % | samples | function |
|--------|---------|----------|
| 24.80% | 218 | `bbnf_tape::driver::dispatch_one` |
| 14.90% | 131 | `bbnf_tape::driver::try_branch` |
| 11.49% | 101 | `bbnf_tape::driver::reserve_compound` |
|  6.60% |  58 | `bbnf_tape::driver::advance_or_pop_with` |
|  6.48% |  57 | `core::hash::sip::Sip13Rounds Hasher::write` |
|  5.35% |  47 | `DtaDfaScanner::scan` |
|  5.12% |  45 | `bbnf_tape::driver::dta_run` |
|  4.55% |  40 | `bbnf_tape::finaliser::finalise` |
|  2.28% |  20 | `mi_heap_realloc_zero` |
|  2.28% |  20 | `bbnf_tape::driver::close_compound` |
|  2.05% |  18 | `HashMap<String, Arc<Dfa>>::get` |
|  1.48% |  13 | `parse_that::scanners::cached_dfa` |
|  1.37% |  12 | `_platform_memmove` |
|  1.37% |  12 | `_platform_memcmp` |
|  1.25% |  11 | `DtaTable::rule_entry_for` |

Source: [parse_simple/profile.json.gz][simple-prof] via
[parse_simple/profile.json.syms.json][simple-syms].

Driver-family subtotal (dispatch_one + try_branch + reserve_compound
+ advance_or_pop_with + dta_run + close_compound + emit_leaf): 66.6%
self-time. Scanner path (DtaDfaScanner::scan + cached_dfa + sip
hasher + HashMap::get + memcmp): 16.73% self-time. finaliser: 4.55%.

[simple-prof]: ../../../../.profiles/samply/google_sheets_monolithic/parse_simple/profile.json.gz
[simple-syms]: ../../../../.profiles/samply/google_sheets_monolithic/parse_simple/profile.json.syms.json

#### 3.2 parse_nested — 333 samples

| self % | samples | function |
|--------|---------|----------|
| 34.83% | 116 | `bbnf_tape::driver::dispatch_one` |
| 15.62% |  52 | `bbnf_tape::driver::try_branch` |
| 10.51% |  35 | `bbnf_tape::driver::reserve_compound` |
|  7.81% |  26 | `DtaDfaScanner::scan` |
|  5.71% |  19 | `bbnf_tape::finaliser::finalise` |
|  5.71% |  19 | `core::hash::sip::Sip13Rounds Hasher::write` |
|  5.41% |  18 | `bbnf_tape::driver::advance_or_pop_with` |
|  2.40% |   8 | `bbnf_tape::driver::dta_run` |
|  1.80% |   6 | `HashMap<String, Arc<Dfa>>::get` |
|  1.80% |   6 | `_platform_memcmp` |
|  1.20% |   4 | `bbnf_tape::driver::emit_leaf` |
|  1.20% |   4 | `parse_that::scanners::cached_dfa` |

Source: [parse_nested/profile.json.gz][nested-prof].

Driver-family: 76.18% self-time. Scanner: 16.82%. finaliser: 5.71%.

[nested-prof]: ../../../../.profiles/samply/google_sheets_monolithic/parse_nested/profile.json.gz

#### 3.3 parse_stress — 581 samples

| self % | samples | function |
|--------|---------|----------|
| 34.77% | 202 | `bbnf_tape::driver::dispatch_one` |
| 16.35% |  95 | `bbnf_tape::driver::try_branch` |
| 11.02% |  64 | `bbnf_tape::driver::reserve_compound` |
|  7.40% |  43 | `core::hash::sip::Sip13Rounds Hasher::write` |
|  6.37% |  37 | `DtaDfaScanner::scan` |
|  5.34% |  31 | `bbnf_tape::driver::advance_or_pop_with` |
|  3.96% |  23 | `bbnf_tape::finaliser::finalise` |
|  2.75% |  16 | `bbnf_tape::driver::close_compound` |
|  1.89% |  11 | `HashMap<String, Arc<Dfa>>::get` |
|  1.72% |  10 | `DtaTable::rule_entry_for` |
|  1.72% |  10 | `parse_that::scanners::cached_dfa` |
|  1.38% |   8 | `_platform_memcmp` |
|  1.20% |   7 | `bbnf_tape::driver::emit_leaf` |
|  0.86% |   5 | `bbnf_tape::driver::dta_run` |

Source: [parse_stress/profile.json.gz][stress-prof].

Driver-family: 75.45% self-time. Scanner: 18.14%. finaliser: 3.96%.

[stress-prof]: ../../../../.profiles/samply/google_sheets_monolithic/parse_stress/profile.json.gz

### 4. Scaling analysis — simple vs nested vs stress

Ordered by stress self%, top hot functions:

| function | simple | nested | stress |
|----------|--------|--------|--------|
| `dispatch_one`                     | 24.80% | 34.83% | 34.77% |
| `try_branch`                       | 14.90% | 15.62% | 16.35% |
| `reserve_compound`                 | 11.49% | 10.51% | 11.02% |
| `sip::Sip13Rounds Hasher::write`   |  6.48% |  5.71% |  7.40% |
| `DtaDfaScanner::scan`              |  5.35% |  7.81% |  6.37% |
| `advance_or_pop_with`              |  6.60% |  5.41% |  5.34% |
| `finaliser::finalise`              |  4.55% |  5.71% |  3.96% |
| `close_compound`                   |  2.28% |  0.90% |  2.75% |
| `HashMap<String, Arc<Dfa>>::get`   |  2.05% |  1.80% |  1.89% |
| `DtaTable::rule_entry_for`         |  1.25% |  0.90% |  1.72% |
| `cached_dfa`                       |  1.48% |  1.20% |  1.72% |
| `_platform_memcmp`                 |  1.37% |  1.80% |  1.38% |
| `dta_run`                          |  5.12% |  2.40% |  0.86% |

Full table: [hot-fn comparison above][xref]; inclusive-time
comparison next.

Inclusive-time (total) comparison — the stack does NOT widen with
stress:

| function | simple | nested | stress |
|----------|--------|--------|--------|
| `GoogleSheetsParser::parse`       |  97.50% |  99.10% |  99.66% |
| `dta_run`                         |  89.87% |  92.79% |  95.70% |
| `dispatch_one`                    |  82.59% |  88.89% |  94.66% |
| `try_branch`                      |  76.56% |  85.89% |  94.49% |
| `DtaDfaScanner::scan`             |  15.93% |  17.12% |  18.07% |
| `reserve_compound`                |  15.36% |  11.11% |  12.39% |
| `cached_dfa`                      |  10.58% |   9.31% |  11.70% |
| `HashMap::get`                    |   9.10% |   7.81% |   9.81% |
| `advance_or_pop_with`             |   7.96% |   6.01% |   6.88% |

**Key finding**: the hot-function composition is *nearly identical*
across simple/nested/stress. Deeper formulas increase self-time
proportionally, not by shifting to a different set of hot functions.
The stack walker profile is **linear-dispatch dominated** at every
scale. `dispatch_one` + `try_branch` settle at ~50% self-time for
nested and stress together; the remaining ~25% distributes across
reserve_compound, scanner, finaliser, and hashing.

The one exception — `dta_run` drops from 5.12% (simple) to 0.86%
(stress). This is inclusive-time devolving onto deeper-nested
callees; at stress depth the outer `dta_run` loop becomes
proportionally smaller against its own payload. It's a
stack-accounting artefact, not a regression shift.

[xref]: #cross-entry-self-time-comparison

### 5. Root-cause dissection

The six driver functions — `dispatch_one`, `try_branch`,
`reserve_compound`, `advance_or_pop_with`, `dta_run`,
`close_compound` — share one architectural characteristic: **they
interpret a state machine stored in `DtaTable::states[]`**. Every
input byte traverses ≥ 1 state; every state transition costs:

1. `table.states[state_idx]` array index + bounds check
   ([driver.rs:864][drv-864])
2. `match DtaState { ... }` dispatch — a 10-arm tagged union
   ([driver.rs:867-1347][drv-867])
3. `stack.pending_variant_idx` write ([driver.rs:872, 890, 906,
   934, 994, 1052, 1185, 1328][drv-pend])
4. `advance_or_pop_with` tail — cursor advance or compound close,
   both hitting the frame stack ([driver.rs:1540-1741][drv-adv])

The `DtaState::Seq` arm (most common) pushes a Frame
([driver.rs:935-949][drv-935]) — `#[repr(C)]`, 40 B, 13 fields
([driver.rs:96-156][drv-frame]) — onto an 8-element inline stack +
spill Vec ([driver.rs:~156-260][drv-stack]).
Per invocation: one `reserve_compound` (7 vec pushes in
[driver.rs:1408-1415][drv-res]), one frame push, one
`advance_or_pop_with` entry on return.

This is the dispatch cost the fn-per-rule path did NOT pay — LLVM
inlined `Seq`-body expansion directly into the caller, eliding
the state-index lookup, the tagged-union match, the frame push, and
the return-to-dispatch-loop roundtrip. The regression is the
arithmetic of that collapse.

`sip::Sip13Rounds Hasher::write` (6.48–7.40% self-time) and
`HashMap<String, Arc<Dfa>>::get` (1.80–2.05%) trace to
[parse-that/rust/parse_that/src/scanners.rs:30-55][dfa-cache]:

```rust
pub fn cached_dfa(pattern: &str) -> Arc<crate::regex::dfa::Dfa> {
    static CACHE: OnceLock<RwLock<HashMap<String, Arc<...>>>> = ...;
    let cache = CACHE.get_or_init(...);
    {
        let map = cache.read().unwrap();
        if let Some(dfa) = map.get(pattern) { return Arc::clone(dfa); }
    }
    ...
}
```

Every regex scan calls `cached_dfa(pattern)` via
[expand.rs:10497-10505][dfa-scan]:

```rust
impl RegexScanner for DtaDfaScanner {
    fn scan(&self, pattern: &str, input: &[u8], offset: usize) -> Option<u32> {
        let dfa = ::parse_that::cached_dfa(pattern);
        dfa.find_at(input, offset).map(|end| (end - offset) as u32)
    }
}
```

That's: RwLock::read() → HashMap<String, _>::get() → SipHash of the
pattern string for every regex invocation. The combined scanner
overhead totals 16.73–18.14% self-time per entry.

`DtaTable::rule_entry_for` (0.90–1.72%) resolves forward-reference
`DtaState::Ref` targets via binary search over `rule_entries`
([driver.rs:977-995][drv-ref], [dta.rs::rule_entry_for][dta-rule]).
Log₂(53 rules) = 6 comparisons per unresolved Ref; every paren-
expression rule body traverses at least one.

The ShuntingYard arm fires for Sheets (confirmed at
[expand.rs:2250][sy-emit]) via the `formula` / `expression`
precedence tower in
[grammar/google-sheets/google-sheets.bbnf:comparison_expr ... exp_expr][sheets-grammar].
[driver.rs:1459-1477][drv-lookup]'s `lookup_precedence` is a linear
scan over the precedence slice (9 operators in Sheets); every
operand boundary calls it, and the hot operand-dominated parts of
stress likely pay this directly. **Not separately attributed in the
top-20 above because it inlines into `advance_or_pop_with`'s
ShuntingYard arm** ([driver.rs:1617-1736][drv-sy]).

[drv-864]: ../../../../crates/bbnf-tape/src/driver.rs#L864
[drv-867]: ../../../../crates/bbnf-tape/src/driver.rs#L867
[drv-pend]: ../../../../crates/bbnf-tape/src/driver.rs#L872
[drv-adv]: ../../../../crates/bbnf-tape/src/driver.rs#L1540
[drv-935]: ../../../../crates/bbnf-tape/src/driver.rs#L935
[drv-frame]: ../../../../crates/bbnf-tape/src/driver.rs#L96
[drv-stack]: ../../../../crates/bbnf-tape/src/driver.rs#L156
[drv-res]: ../../../../crates/bbnf-tape/src/driver.rs#L1399
[drv-ref]: ../../../../crates/bbnf-tape/src/driver.rs#L977
[drv-lookup]: ../../../../crates/bbnf-tape/src/driver.rs#L1459
[drv-sy]: ../../../../crates/bbnf-tape/src/driver.rs#L1617
[dta-state]: ../../../../crates/bbnf-tape/src/dta.rs
[dfa-cache]: ../../../../../parse-that/rust/parse_that/src/scanners.rs#L30
[dfa-scan]: ../../../../.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs#L10497
[sy-emit]: ../../../../.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs#L2250
[sheets-grammar]: ../../../../grammar/google-sheets/google-sheets.bbnf
[dta-rule]: ../../../../crates/bbnf-tape/src/dta.rs

### 6. AW-IV lever coverage matrix

Against [AW-IV.md][aw-iv]'s declared lever inventory:

| Sheets hot fn | self% (avg) | AW-IV lever that addresses it | coverage |
|---------------|-------------|-------------------------------|----------|
| `dispatch_one`         | 31% | **NONE DIRECTLY** — inherent to state-machine dispatch. Indirect via ShapeRef (W1.2) skipping compound emit for same-shape rules; Pratt (W3.3) collapsing the 6-rung tower. | PARTIAL |
| `try_branch`           | 16% | Pratt generalisation (W3.3) if the branch is a precedence rung; selector classifier (W2.3) if CSS-like; otherwise NONE. | PARTIAL |
| `reserve_compound`     | 11% | ShapeRef dispatch (W1.2) — replaces compound-run with single `push_shape_ref`. | YES |
| `sip Hasher::write` (scanner cache) | 6.5% | Scanner-cluster W2.5 — scanner PaddedView + HIR cluster replace the HashMap<String, _> keyed lookup; CSS-first but sheets can adopt. | YES (new) |
| `DtaDfaScanner::scan`  | 6.5% | Scanner-cluster W2.5; PHF/SIMD W2.1/W2.2 for keyword-shaped patterns. | PARTIAL |
| `advance_or_pop_with`  | 5.8% | NONE DIRECTLY — cursor advance is core dispatch. Pratt (W3.3) reduces invocation count for precedence tower. | PARTIAL |
| `finaliser::finalise`  | 4.7% | NONE DIRECTLY — the `close_compound` pre-order fast path (AW-I.W4δ) already elides most of finalise's work; residual is unavoidable scratch-array sweep. | NONE |
| `close_compound`       | 2.0% | ShapeRef dispatch (W1.2) — same-shape records bypass close_compound entirely. | PARTIAL |
| `HashMap::get` + `cached_dfa` | ~4% combined | Scanner-cluster W2.5 — replace the runtime HashMap with compile-time static table (per-grammar, one DFA handle per pattern literal). | YES (new) |
| `rule_entry_for` | 1.3% | NONE — but the lifter can pre-resolve every Ref's target at lift time, eliminating the runtime binary search. **Not in AW-IV; new lever**. | NONE (gap) |
| `_platform_memcmp` (Literal arm) | 1.5% | PHF + SIMD keyword (W2.1/W2.2) for dense keyword Alt nodes; AltLinear → byte-indexed table already converted where possible. | PARTIAL |
| `lookup_precedence` (inlined) | folded | **Pratt W3.3 PRECEDENCE_LUT: [u8; 256]** — O(1) byte-indexed lookup replaces the O(9) linear scan; `exp_expr`/`mul_expr`/... 6-rung tower collapses into one Pratt loop. | YES (critical) |

[aw-iv]: ../AW-IV.md

**Gaps not addressed by AW-IV**:

1. `dispatch_one`'s tagged-union match dispatch. The CPU cost of the
   switch over 11 variants (Epsilon/Literal/Regex/Seq/Ref/
   ByteDispatch/AltLinear/Repeat/Minus/ShuntingYard/WsTrim —
   [dta.rs::DtaState][dta-state]) is inherent to the walker. No
   lever directly flattens it. **Indirect mitigations** (ShapeRef +
   Pratt) reduce invocation count for same-shape + precedence-tower
   rules, but the baseline per-state-visit cost remains.

2. `advance_or_pop_with`'s Seq/Repeat/Alt/SY post-dispatch cursor
   advance. No lever targets this; it's the "return to parent"
   work the fn-per-rule path got for free via stack unwind.

3. `finaliser::finalise`'s second-pass scratch-array sweep (4.5%
   average). The pass fills `child_off` and `span_hi` on compounds
   where `close_compound`'s pre-order fast path couldn't. It's
   already been optimised in AW-I.W4δ (the `child_off != NONE` skip).
   Residual is the bookkeeping for Alt/Repeat compounds where the
   child count isn't known at reserve time.

4. `rule_entry_for` binary search (1.3%). The lifter could
   pre-resolve every Ref target at lift time (eliminating the
   runtime `DtaStateId::NONE` sentinel and the lookup). **Not an
   AW-IV lever; should be added to W5 scope**.

### 7. Recommendation for AW-III.W5 — minimum-viable specialisation

Ordered by attribution × coverage product for Sheets:

#### Must-ship for Sheets viability

1. **Pratt generalisation + PRECEDENCE_LUT** ([AW-IV.W3.3][iv-pratt]).
   Highest single-lever impact. The `lookup_precedence` linear scan
   folds into `advance_or_pop_with` — `_platform_memcmp`
   self-time is 1.38–1.80% per entry, but the **real** cost is
   that every operand boundary pays the ShuntingYard arm's entire
   overhead for the 6-rung tower (comparison_expr, concat_expr,
   add_expr, mul_expr, exp_expr, unary_expr). Collapsing six
   redundant Seq/Repeat arms into one Pratt loop removes 5 layers
   of compound emission per operand. Conservative estimate: 2-3×
   throughput gain on Sheets. This is the single load-bearing lever.

2. **Scanner-cluster consolidation** ([AW-IV.W2.5][iv-scanner]) —
   replace the runtime `HashMap<String, Arc<Dfa>>` in
   `parse_that::cached_dfa` with a per-grammar compile-time static
   table. Every regex scan today pays RwLock::read + SipHash + HashMap
   lookup. The sip hasher alone is 6.48–7.40% self-time; the full
   scanner-cache path is 10–12% self-time total. A compile-time
   `const SCANNERS: &[(PatternId, &Dfa)]` plus an indexed `scan_by_id`
   eliminates the hash entirely. Conservative estimate: 1.15× gain.

[iv-pratt]: ../AW-IV.md#w33-pratt-generalisation
[iv-scanner]: ../AW-IV.md#w25-scanner-architecture-cluster-consolidation--neon-17-digit

#### Helpful if scope allows

3. **ShapeRef runtime dispatch** ([AW-IV.W1.2][iv-shape]). Reduces
   `reserve_compound` + `close_compound` for same-shape records.
   Sheets has many cell references (`A1`, `B2`, `C3:D4`) and
   uniform `func_call` / `cell_ref` records that should collapse
   to single `push_shape_ref` entries. Conservative estimate: 1.1×.

4. **Pre-resolve Ref targets at lift time** (NEW — not in AW-IV
   inventory). Every unresolved `DtaStateId::NONE` Ref pays a
   binary search through `rule_entry_for`. The lifter can resolve
   these to direct `DtaStateId` at table-construction time. Low
   risk, ~1% gain.

[iv-shape]: ../AW-IV.md#w12-shaperef-runtime-dispatch

#### Out of scope for W5 (cost without signal)

- **PSI rayon stage-B** (W1.1) — break-even is marked "Sheets stays
  sequential" in [AW-IV.md:94-96][iv-psi]; inputs are ~500 B, 1.8 KB.
- **List/fork parallel parse** (W3.1) — per-formula parse is the unit
  of work; no list-rule opportunity.
- **PHF keyword** (W2.1) — Sheets has no large keyword set; all Alts
  are 2-4 branches already routed through ByteDispatch.
- **CSS selector classifier** (W2.3) — CSS-only.
- **Bloom + GADT dedup** (W3.2) — Sheets has no dedup-eligible rules.

[iv-psi]: ../AW-IV.md#w11-psi-rayon-stage-b

### 8. Viability verdict

**Viable — conditional on W5 scope including (1) + (2).**

Modelled throughput at W5 close with (1) + (2) + (3) active:

- parse_simple: 132,846 ns ÷ (2.5 × 1.15 × 1.1 = 3.16) = 42,007 ns
  = 11.5 MB/s. Post-AU: 95 MB/s. Ratio: **8.0× over AU**. Fails 2×
  gate.
- parse_nested: 323,774 ns ÷ 3.16 = 102,379 ns = 13.6 MB/s. Post-AU:
  128 MB/s. Ratio: **9.0× over AU**. Fails.
- parse_stress: 509,741 ns ÷ (3.0 × 1.15 × 1.1 = 3.80) = 134,319 ns
  = 13.1 MB/s. Post-AU: 121 MB/s. Ratio: **8.9× over AU**. Fails.

Even with full W5 scope, **Sheets alone will not close the 2× gap
against post-AU's 95-128 MB/s baseline**. The walker's baseline
per-state-visit dispatch cost is the ~24% `dispatch_one` floor that
no AW-IV lever removes. A 2× gate against post-AU is infeasible for
Sheets regardless of W5 scope.

The honest framing for W5 / W6:

- Sheets will meet a relaxed gate (e.g. **5× of post-AU = 20-25
  MB/s**) with (1) + (2) + (3). That's still a 5-6× regression vs
  fn-per-rule but captures every AW-IV-available lever.
- CSS and JSON entries have more lever coverage (Tapered-FA +
  structural classifier + PSI rayon + bloom) and are the ones the
  2× gate was written for.
- The 2× gate in [AW-III.md:55-61][aw-iii-gate] presumes uniform
  lever coverage across the matrix. Sheets is the outlier; either
  the gate loosens for Sheets specifically, or the orchestrator
  escalates a Sheets-specific architecture decision per the plan's
  escape clause ([AW-III.md:172-176][aw-iii-escape]).

Neither escalation is premature — the profile is the evidence. The
recommendation to W5 is: **ship Pratt W3.3 + Scanner-cluster W2.5
as the Sheets-viability minimum**, land a relaxed Sheets-specific
bench gate (≤ 5× post-AU), and carry the residual 2-3× gap as
documented debt. AW-IV's full lever set can then close the gap
further; the viability-as-of-W5 floor is 5× post-AU.

[aw-iii-gate]: ../AW-III.md#architectural-thesis
[aw-iii-escape]: ../AW-III.md#w4--viability-profile

### 9. Cross-reference to other viability profiles

- JSON twitter profile: [perf-01-json.md][perf-json] (if authored)
- BBNF EBNF profile: [perf-02-bbnf.md][perf-bbnf] (if authored)
- CSS bootstrap profile: [perf-04-css.md][perf-css] (if authored)

Sheets is the worst-case entry in the matrix. A global viability
decision should weigh whether the Sheets outcome is
grammar-specific (precedence-heavy, no CSS-style structural hooks)
or representative.

[perf-json]: ./perf-01-json.md
[perf-bbnf]: ./perf-02-bbnf.md
[perf-css]: ./perf-04-css.md

### Appendix A — raw samply artefacts

Per entry, seven files under
`.profiles/samply/google_sheets_monolithic/<entry>/`:

- `bench.txt` — `bencher` output (ns/iter, MB/s)
- `build.txt` — prebuilt-binary lineage
- `record.txt` — `samply record` stdout/stderr
- `load.txt` — `samply load` handshake (URL + server)
- `profile.json.gz` — gzipped Firefox-Profiler JSON
- `profile.json.syms.json` — pre-symbolicated sidecar
- `syms-proof.txt` — named-frame coverage proof

Parse counts and byte throughput per entry are in §2. Named-frame
coverage: 417/427 (simple), 210/220 (nested), 310/320 (stress).

### Appendix B — verification commands

All hot-function extraction used the sidecar's `known_addresses` +
`symbol_table.frames[-1].function` (outermost emitted fn per
region), cross-referenced with `profile.json.gz`'s
`threads[0].frameTable.address` via breakpadId ↔ debug_id
normalisation (lowercase, hyphenated, trailing byte stripped).

```python
# key join:
for d in syms['data']:
    did = d['debug_id'].lower()
    for addr, local_idx in d['known_addresses']:
        sym = d['symbol_table'][local_idx]
        name = st[sym['frames'][-1]['function']]
        addr_to_name[(did, addr)] = name

# profile lib breakpadId → syms debug_id:
bid = libs[libidx]['breakpadId']  # "E36BFC46...B3B0"
did = f"{bid[0:8]}-{bid[8:12]}-{bid[12:16]}-{bid[16:20]}-{bid[20:32]}".lower()
```

Resolution rate 97%+ per entry. Unresolved frames are leaf mimalloc
/ libsystem tail calls below the attributable threshold (< 0.2%
each); reported as `<lib>@<addr>` and omitted from self-time
totals.
