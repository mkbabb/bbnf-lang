# Research perf-04 — `bbnf_monolithic` samply attribution

Tranche AW-III, phase P4. Every claim grounded in saved artefacts
at `.profiles/samply/bbnf_monolithic/<entry>/` — the profile was
captured against the prebuilt binary
`/Users/mkbabb/Programming/bbnf-lang/.profiles/shared-target/release/deps/bbnf_monolithic-28a8299f8b1a6981`.

The `bbnf_monolithic` bench is the most direct dogfood test in the
suite: the DTA-driven `BbnfBootstrap::parse` parses the BBNF
*grammar* representations of json, ebnf, css/pretty, google-sheets,
bbnf itself, and the 15-file CSS L4 grammar. If the DTA walker is
inefficient at parsing grammar text — the substrate that every
other bench ultimately flows through — the substrate's activation
cost is not amortising against bench-side work; it IS the work.

## Methodology

Each of the six entries was driven by
`scripts/profile-bench-headless.sh` against the prebuilt bench
binary on ports 3160/3161 sequentially, with
`CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-lang/.profiles/shared-target`.
Seven artefacts per entry: `bench.txt`, `build.txt`, `record.txt`,
`load.txt`, `profile.json.gz`, `profile.json.syms.json`,
`syms-proof.txt`. All present and non-empty; every entry passed
samply's named-frame proof.

Self-time attribution was derived from the samples' top-of-stack
frames in `profile.json.gz`, cross-walked against the
`profile.json.syms.json` RVA → symbol table (the profile's
`stringArray` carries unresolved `0x...` addresses for inline
frames). Inclusive counts walk each sample's stack to the bottom.

## Per-entry table

| entry | bytes | ns/iter | MB/s | post-AU MB/s | regression | samples |
|-------|------:|--------:|-----:|-------------:|-----------:|--------:|
| `json`            |    537 |    53,951 |  9.0 | 283 | 31.4× | 290 |
| `ebnf`            |  1,453 |   218,196 |  6.7 | 223 | 37.2× | 291 |
| `css_pretty`      |  2,558 |   122,252 | 20.9 | 647 | 32.4× | 1,893 |
| `google_sheets`   |  7,492 |   247,258 | 30.3 | 858 | 28.6× | 1,695 |
| `bbnf_self`       |  5,127 |   404,616 | 12.7 | 394 | 32.8× | 292 |
| `css_l4_grammar`  | 54,385 | 2,762,762 | 19.7 | 496 | 26.1× | 1,156 |

Bench numbers from each entry's `bench.txt`. Post-AU MB/s from the
tranche plan (`AW.md` §Bench delta table, cited to `post-AU.json`).
All six entries regressed 26–37× versus post-AU — a uniform
ratio. The 54 KB CSS L4 grammar and the 537 B json grammar
regress within a factor of ~1.5 of each other, which is the first
signal that the regression is a per-byte overhead, not a per-rule
startup cost.

Sample-count disparity reflects bench duration at 1 ms sampling —
tiny entries (`json`, `ebnf`, `bbnf_self`) iterate the harness
loop in the few hundred micros range; only the longer entries
(`css_pretty`, `google_sheets`, `css_l4_grammar`) cross the few-ms
threshold and accumulate 1,000+ samples.

## Hot-function attribution — union across entries

All six entries share the same top-4 self-time functions in the
same order:

| rank | symbol | json | ebnf | css_pretty | google_sheets | bbnf_self | css_l4_grammar |
|------|--------|-----:|-----:|-----------:|--------------:|----------:|---------------:|
| 1 | `bbnf_tape::driver::dispatch_one`   | 35.5% | 40.5% | 38.5% | 35.3% | 36.3% | 38.8% |
| 2 | `bbnf_tape::driver::try_branch`     | 19.7% | 17.5% | 15.9% | 15.8% | 20.9% | 16.6% |
| 3 | `bbnf_tape::driver::reserve_compound`| 18.6% | 12.7% | 14.9% | 16.8% | 14.4% | 14.5% |
| 4 | `bbnf_tape::driver::advance_or_pop_with` | 8.6% | 7.9% | 6.9% |  6.1% |  6.2% |  6.3% |

The top-4 union accounts for **75–80 % of self-time** on every
entry. Citations:

- `dispatch_one` body at `crates/bbnf-tape/src/driver.rs:852-1398`.
- `try_branch` at `crates/bbnf-tape/src/driver.rs:809-850` — an
  inner loop over `dispatch_one` bounded by `stop_depth`.
- `reserve_compound` at `crates/bbnf-tape/src/driver.rs:1399-1417`
  — seven unconditional `Vec::push` calls (six `Columns` vectors
  + `frame_depth`) per compound emission.
- `advance_or_pop_with` at `crates/bbnf-tape/src/driver.rs:1540-1660+`
  — the compound-close/pop/advance state machine driven by the
  frame stack.

Inclusive coverage confirms the same story: `dta_run` → `dispatch_one`
carries **91–95 %** of every entry's stack on every sample:

| entry | `dta_run` inclusive | `dispatch_one` inclusive | `try_branch` inclusive |
|-------|-----:|-----:|-----:|
| `json`           | 95.2% | 94.8% | 93.8% |
| `ebnf`           | 93.1% | 93.1% | 92.4% |
| `css_pretty`     | 94.5% | 93.7% | 92.2% |
| `google_sheets`  | 93.2% | 92.2% | 91.2% |
| `bbnf_self`      | 93.5% | 93.2% | 91.8% |
| `css_l4_grammar` | 91.3% | 91.3% | 90.3% |

(Per entry's `profile.json.gz`, see `/tmp/attr-<entry>.txt` generated
during this investigation.) Allocation overhead (mimalloc
`alloc_zeroed`) is a secondary cost at 4–5 % on the longer-running
entries — the Vec regrowths that `reserve_compound`'s seven
pushes trigger when the `GRAMMAR_PROFILE.capacity_for` hint
under-allocates.

### Supporting hotspots

- `<DtaDfaScanner as RegexScanner>::scan` —
  `crates/core/src/grammar/generated.rs:14087-14096`. 2.7–7.8 %
  self-time across entries. **Each call invokes
  `::parse_that::cached_dfa(pattern)`**, which takes a
  read-locked `HashMap<String, Arc<Dfa>>` and hashes the pattern
  string (`parse-that/rust/parse_that/src/scanners.rs:30-55`).
  The DFA lookup hash + string comparison dominates the Regex
  state's dispatch cost — the DFA's own `find_at`
  (`parse-that/rust/regex/src/automata/dfa.rs:116-139`) is a
  tight byte-class table walk that is fast in isolation.

- `_platform_memcmp` — 2.6–4.4 % self-time on every entry. Drops
  out of `DtaState::Literal`'s `&input[start..end] != bytes`
  check at `driver.rs:879`. Small prefix checks on short literal
  patterns.

- `<mimalloc::MiMalloc as ...>::alloc_zeroed` at 4.1–4.6 % on the
  three longer entries. Vec growth driven by the 7-push
  `reserve_compound` hot path (per `Columns::with_capacity`'s
  single-record-per-input-byte hint at `columns.rs:136-149` and
  `GRAMMAR_PROFILE.compounds_per_input_byte: 1f32` at
  `generated.rs:36`).

- Secondary self-time cost on `google_sheets` (`close_compound`
  2.4 %) and `ebnf` (`core::cmp::max_by<u8>` 4.5 %) — these are
  within normal dispatcher bookkeeping, not structural
  outliers.

## Rule-count scaling analysis

`css_l4_grammar` is the cleanest scaling probe: 15 files,
~257 rule declarations across them, 54 KB input. `json.bbnf` is
~10 rules × 537 B; `bbnf.bbnf` (the self-host test) is 53 rules
across 3 files for 5.1 KB.

Per-byte cost, the most rule-count-sensitive measure, **does not
track rule count**:

| entry | rules | bytes | ns/byte |
|-------|------:|------:|--------:|
| `ebnf`            |  15 |  1,453 | 150.2 |
| `json`            |  10 |    537 | 100.5 |
| `bbnf_self`       |  53 |  5,127 |  78.9 |
| `css_l4_grammar`  | 257 | 54,385 |  50.8 |
| `css_pretty`      |  20 |  2,558 |  47.8 |
| `google_sheets`   |  39 |  7,492 |  33.0 |

Inverse pattern: the grammar with the **fewest** rules
(`ebnf`, 15) has the highest per-byte cost (150 ns/B); the
grammar with the **most** rules (`css_l4_grammar`, 257) clocks
in at 51 ns/B — faster than `json` (10 rules, 100 ns/B). Rule
count is not the cost driver; **alt-branch count per dispatch**
is.

The `ebnf.bbnf` grammar (`grammar/ebnf/ebnf.bbnf:1-8`) contains a
52-branch `letter = "A" | "B" | ... | "z"` alternation and a
25-branch `symbol = "[" | "]" | ...` alternation. Every character
scan in an identifier body hits `AltLinear` with 52 branches —
the walker savepoints, tries branch 0, restores on failure, tries
branch 1, and so on. That explains both the 40.5 % `dispatch_one`
self-time on this entry (highest of the six) and the 150 ns/byte
— the linear-scan cost.

The json probe hits an analogous `AltLinear` inside
`value = object | array | string | number | bool | null`
(`grammar/json/json.bbnf:16`) but only 6 branches deep; its
per-byte cost is commensurately lower.

Rule-count scaling in the driver itself arises in:

1. **`DtaTable::rule_entry_for`** — `driver.rs:977-981` looks up a
   Ref's target by binary-searching `rule_entries` (the sorted
   rule table). `log₂(257) ≈ 8` vs `log₂(10) ≈ 3` is a 2.5×
   comparison count; but the function appears at 0.3 % inclusive
   only on `json` and disappears from longer entries' top-25.
   Rule dispatch is not the bottleneck.

2. **`cached_dfa` string-keyed HashMap lookup** — 257 distinct
   regex patterns would take 257 cache keys. The CSS L4 grammar
   declares at most 14 `DtaState::Regex` states (per the
   generated table variant-distribution grep), so the pattern
   cache's scaling is governed by `Regex` variant count, not rule
   count. 14 entries lookup at O(1)-amortised is cheap; the per-
   scan hash cost is what shows up in `scan`'s self-time.

3. **`AltLinear` fan-out** — no rule-count dependency; the walker
   tries branches in grammar-declaration order regardless of how
   many rules precede or follow the Alt.

**Conclusion**: walker scaling is linear in `dispatch_one`
invocations, which is linear in *DtaState transitions*, which is
itself bounded by the grammar's Alt-branch structure and the
input's byte count × per-byte transition count (∝ grammar
complexity). Rule count, in isolation, contributes log factors
to Ref resolution and pattern-cache lookup — both sub-1 % of
total self-time on every profile.

## Does `css_l4_grammar` have different hot fns than `json`?

**No — the top-7 self-time functions are nearly identical, only
the percentages shift**:

| symbol | json | css_l4_grammar | delta |
|--------|-----:|---------------:|------:|
| `dispatch_one`      | 35.5% | 38.8% | +3.3 |
| `try_branch`        | 19.7% | 16.6% | −3.1 |
| `reserve_compound`  | 18.6% | 14.5% | −4.1 |
| `advance_or_pop_with`| 8.6% |  6.3% | −2.3 |
| `DtaDfaScanner::scan`| 2.8% |  4.6% | +1.8 |
| `_platform_memcmp`   | 3.1% |  3.1% |   0 |
| `MiMalloc::alloc_zeroed` | — | 4.6% |  new |

Two second-order shifts worth noting:

- **`MiMalloc::alloc_zeroed` is a top-7 hot function on
  `css_l4_grammar` (4.6 %) but does not appear in `json`'s top
  20.** This is the 54 KB input's Vec-regrowth cost: the
  `compounds_per_input_byte: 1f32` capacity hint is roughly
  accurate, but the six structural columns plus `frame_depth`
  each regrow independently, and amortised doubling beyond the
  initial 54 K capacity still triggers ~17 regrowths per parse.
  `mi_page_retire` and `mi_heap_realloc_zero` appear in the
  inclusive stacks of the larger entries (`syms-proof.txt`
  line 1 for `json` vs the corresponding file for `css_l4_grammar`).

- **`DtaDfaScanner::scan` inclusive is 7.6 % on `css_l4_grammar`
  vs 5.5 % on `json`.** Colour/gradient/transform grammars
  compose more regex states than the json grammar's single
  number pattern; per-regex scan count therefore scales with
  grammar Regex-state count, which at 257 rules is higher than
  json's 10.

The walker is the same dispatcher doing the same work at the same
rate; the larger grammar gives it more work.

## BBNF-specific lever hypothesis

The attribution points to a single structural cost — the walker's
per-state dispatch overhead. Levers listed in
`docs/tranches/AW/research/05-bench-checkpoint-protocol.md §4`,
ranked by expected `bbnf_monolithic` yield:

1. **`shape_ref`** (AW.W2.3 SHAPE_DICT runtime dispatch) —
   highest expected yield. `dispatch_one`'s 35–40 % self-time is
   an `enum DtaState { Literal, Regex, Seq, … }` match-dispatch;
   every rule body eventually hits a Ref, and every Ref incurs a
   `rule_entry_for` binary search + a dispatch back into
   `dispatch_one`. Collapsing same-shape Refs to a direct `Seq`
   or `Literal` transition (what `shape_ref` does at runtime)
   eliminates a `dispatch_one` round-trip per Ref. 53 rules in
   `bbnf.bbnf` × n Refs per rule body ≈ the call count that
   drives `try_branch`'s 17–21 % self-time.

2. **`phf_keyword`** (AW.W3.1 PHF for Alt dispatch) — second
   highest. `ebnf`'s 40.5 % `dispatch_one` self-time and
   150 ns/byte are direct indicators of `AltLinear`'s 52-branch
   linear scan for `letter`. A PHF collapses a 52-branch probe
   to a single hash + array lookup. Expected yield
   disproportionately concentrated on `ebnf` and `json` (the
   small grammars whose per-byte cost is dominated by `AltLinear`
   fan-out).

3. **`simd_compare`** (AW.W3.2 SIMD compare on ≤16-entry Alts) —
   complementary to PHF. The `value` Alt in `json.bbnf` (6
   branches) and the `term` Alt in `bbnf.bbnf` (8 branches) are
   in the SIMD-addressable range. Short linear Alts are common;
   a packed SIMD-compare lowers the 6-way dispatch to one vector
   compare + lane-to-index.

4. **`psi_rayon`** (AW.W2.1 parallel PSI) — **low yield on this
   bench**. `GRAMMAR_PROFILE.parallel_break_even_bytes: 0u32`
   for BbnfBootstrap, meaning no parallel threshold is set and
   the bench inputs are too small for Rayon to amortise its
   setup cost. The PSI jobs on the 6 entries are dominated by
   f64-from-Regex emission (the psi hot path does not appear in
   any top-20 except `google_sheets`'s number-heavy formulas).
   `bbnf_self`'s PSI count is near-zero — BbnfBootstrap emits
   mostly Span payloads, which are stored in PSI but not
   computation-heavy.

5. **`scanner_padded`** (AW.W3.4 find_next_structural) — moderate
   yield. `trim_ascii_ws` at `driver.rs:638-647` runs at every
   dispatch loop iteration when the generated table lacks
   explicit `WsTrim` states. The current `generated.rs` DOES
   carry 102 explicit `DtaState::WsTrim` states
   (`generated.rs` count from grep), so the `ws_fallback` branch
   is dead; `DtaState::WsTrim` itself is a byte-by-byte scan at
   `driver.rs:1234-1241`. A SIMD or find_next_structural
   implementation of the inner WsTrim loop would amortise the
   per-token whitespace cost on every grammar — BBNF grammars
   are whitespace-rich.

### Zero-yield lever for this bench

- **`pratt_lower`** (AW.W4.6) — no ShuntingYard-dominated BBNF
  grammar in the six entries; `google_sheets.bbnf` has the only
  precedence tower, and it's not the dominant cost on that
  entry.

- **`visitor_simd_pack` / `visitor_reduce`** — these are W6
  visitor-side levers; `bbnf_monolithic` measures parse alone, no
  visitor traversal.

### Non-lever opportunity

The `reserve_compound` 12–19 % self-time is not addressable by
any AW-IV-listed lever. Seven `Vec::push` calls per compound
emission is architecturally fixed by the Columns-per-record
layout at `columns.rs:136-149`. Options outside the current lever
list:

- **Struct-of-arrays → interleaved record layout** on the hot
  path: a single 24-byte record with the six structural fields
  packed, pushed as one `Vec::push` instead of seven. Preserves
  the columnar output at finalise time; only the hot append is
  interleaved. Would halve to ~25 % of `reserve_compound`'s
  per-push cost (1 bounds check and grow instead of 7).

- **Pre-sized exact capacity** from the grammar profile: the
  `compounds_per_input_byte: 1f32` hint is roughly right on
  average but the columns start over-allocated by 7× the
  `expected` count, not `7 × expected`. Audit
  `Columns::with_capacity` to pre-size every column vector with
  the specific per-column estimate (`kinds`, `flags` at
  `expected`; `pay_agg` at `expected / 8 * 8`, etc.).

Both are outside `research/05`'s attribution enum; flagging for
the AW-IV wave plan or a follow-on tranche. They are noted here
because they would collectively move the `reserve_compound` hot
path off the top-3 list — a bigger lever than most of the
enumerated ones.

## Artefact map

All under `.profiles/samply/bbnf_monolithic/`:

```
json/            bench.txt (9 MB/s), profile.json.gz (6.3 KB), syms-proof.txt
ebnf/            bench.txt (6 MB/s), profile.json.gz (6.4 KB), syms-proof.txt
css_pretty/      bench.txt (20 MB/s), profile.json.gz (24 KB), syms-proof.txt
google_sheets/   bench.txt (30 MB/s), profile.json.gz (23 KB), syms-proof.txt
bbnf_self/       bench.txt (12 MB/s), profile.json.gz (7.0 KB), syms-proof.txt
css_l4_grammar/  bench.txt (19 MB/s), profile.json.gz (18 KB), syms-proof.txt
```

Attribution inputs: `/tmp/attr-<entry>.txt` (six files) generated
by walking `profile.json.gz`'s stack table against the
`profile.json.syms.json` RVA symbol map — not retained in
`.profiles/`; regenerate with `scripts/profile-bench-headless.sh`
+ the attribution one-liner cited in the methodology.

## Feed-forward

Primary seed for AW-IV P5 (lever activation):

- **Activate `shape_ref` first** — its `dispatch_one` yield is
  universal across all six entries.
- **Activate `phf_keyword` + `simd_compare` as a pair** — their
  yields are concentrated on the small-grammar entries (`ebnf`,
  `json`) where `AltLinear` branch counts dominate.
- **`psi_rayon` and `pratt_lower` are zero-yield on this bench**
  — do not gate `bbnf_monolithic` regression closure on them.
- **`reserve_compound`'s 7-push hot path is not addressable by
  enumerated levers** — flag for a dedicated Columns layout
  tranche; expected yield 4–5 % across all entries if the seven
  pushes collapse to one.

## Non-lever findings worth capturing

1. The generated `BbnfBootstrap` table carries 102 explicit
   `DtaState::WsTrim` states, 114 Literals, 112 Seqs, 90 Refs, 36
   Repeats, 27 AltLinears, 14 Regexes, 1 ShuntingYard — 496 total
   states. `ws_fallback` at `driver.rs:591` is dead (the `all(...
   !matches!(WsTrim))` predicate returns false). The fallback
   branch's three `trim_ascii_ws` call sites are not hot; the
   real WsTrim hot path is `DtaState::WsTrim`'s byte loop at
   `driver.rs:1234-1241`.

2. `GRAMMAR_PROFILE.parallel_break_even_bytes: 0u32` across the
   BbnfBootstrap profile means PSI rayon is disabled for this
   grammar. This is consistent with `psi_rayon` being in a later
   wave (AW W2.1).

3. `DtaDfaScanner::scan` is a 2-line adapter at
   `generated.rs:14087-14096` that re-hashes the pattern string
   on every scan via `parse_that::cached_dfa(pattern)`. Caching
   the `Arc<Dfa>` at state-lift time (one Arc lookup per regex
   state in the table, reused on every dispatch) would lift the
   per-scan HashMap+hash cost out of the hot path. This is a
   parse-that/scanner-adapter change, not a walker change — in
   scope per the crate-ownership directive, but outside the
   enumerated levers.
