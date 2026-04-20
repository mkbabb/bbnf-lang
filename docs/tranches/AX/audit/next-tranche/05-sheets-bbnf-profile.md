# AX Next-Tranche Audit — Sheets + BBNF Bench + Profile (W1a.A5)

Scope: Google Sheets and BBNF self-hosting parse paths at master HEAD
`ededfc7c`. Self-baseline vs `docs/benchmarks/post-AX-W0a-close.json`
(`5dab5175`, 2026-04-19). Cold per-parse only. Read-only bench +
samply; no source edits.

## 1. Bench matrix — current vs W0a-close baseline

Raw outputs: `/tmp/a5-bench-sheets.txt`, `/tmp/a5-bench-bbnf.txt`.
Both runs: release profile, mimalloc global allocator, single-iteration
`bencher::bench_with_timeout`, `CARGO_BUILD_JOBS=4`.

| bench                            | W0a ns/iter | A5 ns/iter | Δ      | MB/s A5 |
|----------------------------------|-------------|------------|--------|---------|
| google_sheets::parse_simple      |      31 425 |     32 569 | +3.6%  |      15 |
| google_sheets::parse_nested      |      71 514 |     74 467 | +4.1%  |      19 |
| google_sheets::parse_stress      |      99 167 |    103 150 | +4.0%  |      17 |
| bbnf_monolithic::json            |       7 427 |      7 462 | +0.5%  |      71 |
| bbnf_monolithic::ebnf            |      31 578 |     31 621 | +0.1%  |      45 |
| bbnf_monolithic::css_pretty      |      16 549 |     17 045 | +3.0%  |     150 |
| bbnf_monolithic::google_sheets   |      36 097 |     37 106 | +2.8%  |     201 |
| bbnf_monolithic::bbnf_self       |      57 359 |     59 535 | +3.8%  |      86 |
| bbnf_monolithic::css_l4_grammar  |     439 010 |    453 861 | +3.4%  |     123 |

No entry crosses the ±5% reportable threshold. Sheets is uniformly
~3-4% slower and BBNF monolithic is ~0-4% slower; the deltas are
within run-to-run jitter for a single-sample `bencher` harness. No
improvements to report. Interpretation: no behaviour changed in the
Sheets/BBNF hot paths between W0a-close and HEAD `ededfc7c` — the
intervening commits (AX.W1r.3a/4a/5/6/7) landed parity harnesses,
CSS `@pretty`, and typed-accessor audits that do not touch the
tape-emit/regex-scan inner loop.

## 2. Top-10 self-time per profiled entry

Artefacts: `.profiles/samply/{bbnf_monolithic/css_l4_grammar,
google_sheets_monolithic/parse_stress}/profile.json.gz`. Symbol
resolution via companion `profile.json.syms.json` rva→name tables.

### `bbnf_monolithic::css_l4_grammar` (4 984 leaf samples)

```
32.28%  <tape::columns::Columns>::push_structural
20.41%  tape::finaliser::finalise
12.14%  <bbnf::grammar::generated::BbnfBootstrap>::parse
 7.91%  __regex_scan_BbnfBootstrap
 7.66%  parse_flat_BbnfBootstrap_mapped_factor
 2.43%  parse_wrap_BbnfBootstrap_rhs
 1.81%  parse_pratt_BbnfBootstrap_binary_factor
 1.73%  parse_flat_BbnfBootstrap_big_comment
 1.54%  <tape::builder::TapeBuilder>::push_leaf_with
 1.26%  parse_wrap_BbnfBootstrap_grammar_item
```

Tape bookkeeping (push_structural + finalise + push_leaf_with) is
54.23% of self-time. Regex scan is 7.91%. All remaining named frames
are grammar-emitted `parse_*` bodies.

### `google_sheets_monolithic::parse_stress` (4 436 leaf samples)

```
27.71%  <tape::columns::Columns>::push_structural
15.71%  tape::finaliser::finalise
12.11%  __regex_scan_GoogleSheetsParser
 8.12%  parse_wrap_GoogleSheetsParser_primary
 6.88%  <google_sheets_monolithic::GoogleSheetsParser>::parse
 2.50%  parse_pratt_GoogleSheetsParser_concat_expr
 2.39%  <tape::columns::Columns>::truncate
 2.28%  parse_flat_GoogleSheetsParser_unary_expr
 1.98%  parse_pratt_GoogleSheetsParser_comparison_expr
 1.76%  parse_pratt_GoogleSheetsParser_mul_expr
```

Tape bookkeeping 45.81% (push_structural + finalise + truncate +
push_leaf_with at 1.0%). Regex scan 12.11%. Pratt/flat arithmetic
ladder (concat, add, mul, exp, unary, comparison) sums to ~12% across
6 stratified emitted arms — the pratt descent width is the dominant
non-tape cost.

## 3. Cross-grammar hotspot union

A3 (json) and A4 (css_l4) deliverables were not yet on disk at read
time. To produce the universal lever analysis this agent additionally
captured fresh self-time histograms at HEAD `ededfc7c` for
`json_monolithic::canada` and `css_l4::tailwind`; those profiles live
at `.profiles/samply/{json_monolithic/canada-az-a5,css_l4/tailwind-az-a5}`.
The `-az-a5` suffix distinguishes them from the stale Apr-17
W0a-baseline profiles in the same parent directory. A3/A4, when they
land, own the primary attribution for those benches; the histograms
below are cross-reference material for the universal-lever argument.

Ranked by presence across four grammar profiles:

| symbol                                    | sheets | bbnf | json | css_l4 | grammars |
|-------------------------------------------|--------|------|------|--------|----------|
| `tape::columns::Columns::push_structural` | 27.7%  | 32.3%| 40.4%| 28.3%  | 4/4      |
| `tape::finaliser::finalise`               | 15.7%  | 20.4%| 22.5%| 14.7%  | 4/4      |
| `__regex_scan_<Parser>`                   | 12.1%  |  7.9%|   —  | 25.9%  | 3/4      |
| `tape::builder::TapeBuilder::push_leaf_with` | 1.0% |  1.5%|  3.6%|  2.7%  | 4/4      |
| `<Parser>::parse` (dispatcher shell)      |  6.9%  | 12.1%| 11.4%|  7.3%  | 4/4      |
| `tape::columns::Columns::truncate`        |  2.4%  |   —  |   —  |  1.6%  | 2/4      |

The three universal hotspots — appearing in every grammar at
non-trivial self-time and together accounting for 43-66% of total
time — are:

1. **`tape::columns::Columns::push_structural`** — 28-40% across
   every grammar. The single most concentrated universal cost.
2. **`tape::finaliser::finalise`** — 15-22% across every grammar.
   A second-pass linear sweep over tape columns.
3. **`TapeBuilder::push_leaf_with` + `<Parser>::parse` dispatcher** —
   the parse-entry and leaf-emission shells, each 1-12% and always
   present.

`__regex_scan_<Parser>` is a strong hotspot for token-rich grammars
(css_l4 at 25.9%, sheets at 12.1%) but trivial or absent for
grammars whose bench corpus is AST-dense rather than lex-dense
(json.bbnf at ~0%). It is a 3/4 universal, not a pure universal.

## 4. Tape-first validation (AX invariants 20/21)

Per-bench `nm | grep -E 'dta_walker|dta_run|dispatch_one'`:

```
bbnf_monolithic-1cce73194c2882ba       : 0 matches
google_sheets_monolithic-55ac0b2aebfb253e : 0 matches
```

Parser entry points are present (demangled via `rustfilt`):

```
bbnf_monolithic         : <bbnf::grammar::generated::BbnfBootstrap>::parse  (0x8218)
google_sheets_monolithic: <google_sheets_monolithic::GoogleSheetsParser>::parse  (0x1326c)
```

The samply symbol tables corroborate: both profile.json.syms.json
entries are dominated by `__<parser>_emit_impl::parse_*` symbols —
no walker, no DTA runtime, no indirect dispatch. AX.W0a's
tape-first invariant holds for Sheets and BBNF at HEAD `ededfc7c`.

## 5. Universal lever proposals

The three universal hotspots determine next-tranche scope. Proposals
are ordered by estimated cumulative self-time reclaim across all
five grammar benches.

### Lever U1 — inline + specialise `Columns::push_structural`

Every grammar's single hottest symbol. Current implementation
(`crates/tape/src/columns.rs`) presumably grows a side-column per
structural emit with generic `Vec::push` semantics. Concrete sub-work:

- Verify via `nm` whether `push_structural` is a cross-crate call
  boundary in each bench binary (universal perf memo: cross-crate
  helpers that do not inline = dispatch by another name).
- Introduce `#[inline(always)]` on the hot path OR per-grammar inline
  emission of the function body; verify post-wave with `nm`.
- Consider capacity pre-reservation per grammar via emitter-known
  column-size budgets — every grammar has an upper-bound tape width
  knowable at codegen time.

Expected reclaim: 25-40% of self-time on every grammar.

### Lever U2 — fuse `finaliser::finalise` with emit

Second-universal at 15-22%. A linear post-pass that could fuse into
the emit loop by maintaining invariants incrementally rather than
rebuilding them. Concrete sub-work:

- Characterise what `finalise` computes (width-prefix, offset index,
  column headers) — check `crates/tape/src/finaliser.rs`.
- For each computable invariant, move the update into the emit
  `push_structural` / `push_leaf_with` path.
- If full fusion is infeasible, at minimum replace
  `<Iterator>::max` over a column (visible in the Sheets profile
  symbol table) with a running-max maintained during emit — the
  `core::cmp::Ord::cmp` fold appears in both profiles' symbol
  tables but not the top-10 self-time.

Expected reclaim: 10-20% of self-time on every grammar.

### Lever U3 — parse-entry shell elision

`<Parser>::parse` is 6.9-12.1% self-time across grammars despite
being a thin shell. This strongly suggests inlining has not
fired — the shell allocates the column vectors, sets up state,
and dispatches to the root arm. Concrete sub-work:

- `nm` on every bench binary to confirm the shell is a real symbol,
  not an inlined wrapper. (A5 confirmed the symbol is present in
  both Sheets and BBNF bench binaries.)
- If the shell is a real call, either `#[inline(always)]` it
  (simple) or split the capacity-reservation from the dispatch so
  the reservation amortises over many parses of the same grammar.
- The per-parse reservation calls into `Columns::with_capacity` →
  `Vec::with_capacity_in` → `mimalloc::alloc` (visible in both
  symbol tables). This is the allocation churn behind the
  `_mi_page_retire` / `mi_malloc_aligned` / `mi_free` tail visible
  at 1-3% each in the Sheets and BBNF profiles.

Expected reclaim: 5-10% of self-time per grammar plus reduction of
mimalloc traffic.

Sheets-specific and BBNF-specific micro-tuning is deferred per the
orchestrator's "universal levers only" instruction. The pratt-ladder
width in Sheets and the `parse_flat_BbnfBootstrap_mapped_factor`
hotspot in BBNF are grammar-local and should not drive next-tranche
planning.

## 6. BBNF bootstrap compile-time observation

Clean rebuild of `bbnf` + `bbnf_monolithic` bench binary (derive
macro fresh, `.bbnf-cache/` cleared):

```
Compiling bbnf_derive v0.2.11
Compiling gorgeous    v0.1.10
Compiling bbnf        v0.2.11
Finished `release` profile [optimized + debuginfo] target(s) in 39.88s
real   39.9 s    user 112.0 s    cpu 287%
```

The `bbnf_monolithic` bench binary itself does **not** invoke the
derive macro — it re-uses `BbnfBootstrap` exported from the `bbnf`
crate. The 40-second cost is paid upstream during `bbnf` crate
compilation (derive-macro expansion of `bbnf.bbnf` against itself,
plus LLVM codegen of the emitted dispatcher). `bbnf_self` the
*bench entry* parses every `.bbnf` file in `grammar/bbnf/` at
runtime using the already-compiled dispatcher; its 59 µs/iter
captures runtime parse cost, not proc-macro cost. A6 cross-
reference: proc-macro compile-time attribution belongs to the
`bbnf` crate build (+`bbnf_derive`), not the bench. The 40 s is the
baseline a next-tranche compile-time lever (e.g. `.bbnf-cache`
activation verification, or a derive-output size-reduction pass)
would improve.

## Artefacts

- `/tmp/a5-bench-{sheets,bbnf}.txt` — raw bench output.
- `.profiles/samply/bbnf_monolithic/css_l4_grammar/` — seven
  samply artefacts (bench, build, load, record, profile.json.gz,
  profile.json.syms.json, syms-proof).
- `.profiles/samply/google_sheets_monolithic/parse_stress/` — same.
- `.profiles/samply/json_monolithic/canada-az-a5/` —
  cross-reference only; A3 owns primary.
- `.profiles/samply/css_l4/tailwind-az-a5/` — cross-reference
  only; A4 owns primary.
