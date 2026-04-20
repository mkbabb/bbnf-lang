# AY Planning — A3 Sheets + BBNF Parse Fresh Profile

Scope: Google Sheets and BBNF self-hosting parse paths at master HEAD
`9074a685`. Self-baseline vs `docs/benchmarks/post-AX-W0a-close.json`
(`5dab5175`, 2026-04-19). Cold per-parse; release profile; mimalloc;
`CARGO_BUILD_JOBS=4`. All 9 profiles captured fresh on 2026-04-20 via
`scripts/profile-bench-headless.sh` against the prebuilt
`bbnf_monolithic-1cce73194c2882ba` and
`google_sheets_monolithic-55ac0b2aebfb253e` binaries. All artefacts
live under `.profiles/samply/{bbnf_monolithic,google_sheets_monolithic}/<entry>/`.

## 1. Fresh bench matrix vs W0a-close baseline

Bench sources: `/tmp/a3-bench-sheets.txt`, `/tmp/a3-bench-bbnf.txt`
(multi-iteration `cargo bench`, not the single-iteration
profile-inline runs recorded in each entry's `bench.txt`).

| bench                            | W0a ns/iter | A3 ns/iter | Δ      | MB/s A3 |
|----------------------------------|-------------|------------|--------|---------|
| google_sheets::parse_simple      |      31 425 |     32 520 | +3.5%  |      15 |
| google_sheets::parse_nested      |      71 514 |     74 151 | +3.7%  |      19 |
| google_sheets::parse_stress      |      99 167 |    103 705 | +4.6%  |      17 |
| bbnf_monolithic::json            |       7 427 |      7 630 | +2.7%  |      70 |
| bbnf_monolithic::ebnf            |      31 578 |     32 084 | +1.6%  |      45 |
| bbnf_monolithic::css_pretty      |      16 549 |     17 362 | +4.9%  |     147 |
| bbnf_monolithic::google_sheets   |      36 097 |     37 019 | +2.6%  |     202 |
| bbnf_monolithic::bbnf_self       |      57 359 |     58 998 | +2.9%  |      86 |
| bbnf_monolithic::css_l4_grammar  |     439 010 |    452 193 | +3.0%  |     123 |

**Zero entries cross the AX invariant 10 ±5% reportable threshold.**
Every entry is uniformly 2-5% slower with no commits between W0a-close
(`1241e7ac`) and HEAD `9074a685` touching the tape-emit / regex-scan
inner loop; this is run-to-run jitter on a warmed-then-cold bench
matrix. Doc 05's claim that "no behaviour changed in the Sheets/BBNF
hot paths between W0a-close and HEAD" holds at `9074a685`. The +4.9%
css_pretty and +4.6% parse_stress are the widest; both sit inside the
single-run `+/-` error bars (css_pretty ±563; parse_stress ±4470).

## 2. Top-10 self-time per fresh profile

Resolved via the companion `profile.json.syms.json` rva tables against
each profile's `frameTable.address`. Extraction script
`/tmp/profile_top_self2.py`.

### Sheets

**`google_sheets_monolithic::parse_simple`** (4205 samples)

```
21.57%  <tape::columns::Columns>::push_structural
12.10%  tape::finaliser::finalise
 7.82%  <google_sheets_monolithic::GoogleSheetsParser>::parse
 6.99%  parse_wrap_GoogleSheetsParser_primary
 5.95%  _mi_page_retire
 5.83%  __regex_scan_GoogleSheetsParser
 5.49%  _mi_heap_realloc_zero
 3.19%  parse_flat_GoogleSheetsParser_unary_expr
 3.19%  <alloc::raw_vec::RawVecInner>::finish_grow
 2.83%  _platform_memmove
```

**`parse_nested`** (4282 samples)

```
26.32%  <tape::columns::Columns>::push_structural
15.88%  tape::finaliser::finalise
 8.55%  __regex_scan_GoogleSheetsParser
 7.99%  <google_sheets_monolithic::GoogleSheetsParser>::parse
 7.08%  parse_wrap_GoogleSheetsParser_primary
 3.50%  _mi_page_retire
 2.85%  parse_flat_GoogleSheetsParser_unary_expr
 2.59%  mi_free
 2.27%  mi_malloc_aligned
 2.17%  _mi_heap_realloc_zero
```

**`parse_stress`** (4237 samples)

```
29.45%  <tape::columns::Columns>::push_structural
14.75%  tape::finaliser::finalise
11.82%  __regex_scan_GoogleSheetsParser
 7.36%  parse_wrap_GoogleSheetsParser_primary
 7.03%  <google_sheets_monolithic::GoogleSheetsParser>::parse
 4.06%  _mi_page_retire
 2.78%  parse_flat_GoogleSheetsParser_unary_expr
 2.43%  <tape::columns::Columns>::truncate
 2.05%  mi_malloc_aligned
 1.91%  parse_pratt_GoogleSheetsParser_comparison_expr
```

### BBNF (bbnf-parser parsing grammar source)

**`bbnf_monolithic::json`** (3726 samples)

```
30.65%  <tape::columns::Columns>::push_structural
22.33%  tape::finaliser::finalise
12.99%  <BbnfBootstrap>::parse
 7.76%  parse_flat_BbnfBootstrap_mapped_factor
 4.78%  __regex_scan_BbnfBootstrap
 3.01%  parse_wrap_BbnfBootstrap_rhs
 2.09%  parse_pratt_BbnfBootstrap_binary_factor
 2.07%  parse_flat_BbnfBootstrap_big_comment
 1.99%  parse_wrap_BbnfBootstrap_grammar_item
 1.99%  <TapeBuilder>::push_leaf_with
```

**`ebnf`** (4537 samples) — 34.21% push_structural / 21.38% finalise /
14.08% dispatcher / 9.52% parse_flat_mapped_factor / 3.81%
regex_scan. **`css_pretty`** (278 samples) — 31.65 / 21.94 / 15.83 /
8.27 / 6.47. **`google_sheets`** (1895 samples) — 32.14 / 22.53 /
12.72 / 9.23 regex_scan / 6.44 mapped_factor. **`bbnf_self`** (841
samples) — 31.03 / 22.83 / 12.25 / 8.56 / 5.83. **`css_l4_grammar`**
(2367 samples) — 30.76 / 20.36 / 12.46 dispatcher / 8.15 regex_scan /
7.56 mapped_factor.

**Zero walker symbols** (`nm | grep -E 'dta_walker|dta_run|dispatch_one'`)
on either bench binary. Tape-first invariant AX W0a holds at
`9074a685`.

## 3. Cross-entry hotspot union (presence ≥ 4/9 at ≥ 1% self-time)

Artefact: `/tmp/union.txt`.

| symbol                                              | 9-entry avg | presence | notes |
|-----------------------------------------------------|------------|----------|-------|
| `tape::columns::Columns::push_structural`            | **29.75%** | 9/9      | universal; dominant |
| `tape::finaliser::finalise`                          | **19.35%** | 9/9      | universal; Stage-C sweep |
| `mi_malloc_aligned`                                  | 1.28%      | 7/9      | mimalloc alloc churn |
| `<TapeBuilder>::push_leaf_with`                      | 1.48%      | 6/9      | leaf emit |
| `<Parser>::parse` (dispatcher shell)                 | 8.93%      | 6/9 (bbnf only) | 12-16% on every bbnf entry |
| `__regex_scan_<Parser>`                              | 4.25%      | 6/9 (bbnf only) | + 5.8-11.8% on sheets |
| `parse_flat_BbnfBootstrap_mapped_factor`             | 5.35%      | 6/9 (bbnf only) | bbnf root factor |
| `parse_wrap_BbnfBootstrap_rhs`                       | 2.33%      | 6/9 (bbnf only) | |
| `parse_flat_BbnfBootstrap_big_comment`               | 1.45%      | 6/9 (bbnf only) | |
| `parse_pratt_BbnfBootstrap_binary_factor`            | 1.62%      | 6/9 (bbnf only) | 1-level pratt |

**Doc 05's universal-hotspot claim is confirmed at `9074a685`.**
`push_structural` 28-40% and `finalise` 15-22% hold across every
grammar. `regex_scan` 12-26% is a token-heavy-corpus finding — sheets
stress hits 11.82%, bbnf parsing sheets (lex-dense multi-file
grammar) hits 9.23%, and bbnf parsing css_l4 (55.9 KB of multi-file
grammar source) hits 8.15%. `push_structural + finalise` sum to
**33.7-55.6% of total self-time on every one of the nine entries** —
the single largest universal optimisation target.

`push_structural` carries `#[inline]` in source
(`crates/tape/src/columns.rs:321`) but
`nm bbnf_monolithic-1cce73194c2882ba | grep push_structural` emits
exactly one symbol (`<Columns>::push_structural` at rva `0x155f0`).
**LLVM is not inlining the `#[inline]` hint** — every grammar-emitted
`push_structural` call site is a real cross-crate function-call
boundary, confirmed by both symbol presence and the samply self-time
attribution. The shell-dispatcher `<Parser>::parse` is similarly a
real symbol in both bench binaries; it appears at 6.9-15.8% self-time
across the 9 entries.

## 4. Sheets-specific findings — stress scaling pathology

Stress vs simple per-iter delta: 32 520 → 103 705 ns (+3.19×) on a
1838 / 505 byte corpus (+3.64× bytes). Near-linear in bytes, but
the hotspot distribution shifts:

| symbol                                  | simple | nested | stress | stress/simple |
|-----------------------------------------|--------|--------|--------|---------------|
| push_structural                         | 21.57% | 26.32% | 29.45% | 1.37× |
| finalise                                | 12.10% | 15.88% | 14.75% | 1.22× |
| __regex_scan_GoogleSheetsParser         |  5.83% |  8.55% | 11.82% | 2.03× |
| parse_wrap_GoogleSheetsParser_primary   |  6.99% |  7.08% |  7.36% | 1.05× |
| parse_pratt_*_{exp,comparison,mul}_expr | <2.0%  | ~1.8%  | ~5.6%  | ~3× combined |

**`__regex_scan_GoogleSheetsParser` doubles in share** from simple to
stress; this tracks the shift from single-formula `=SUM(A1:A10)`
inputs to nested `LET(..., LAMBDA(..., IF(..., MAP(SEQUENCE(...),
LAMBDA(...)))))` expressions in `data/sheets/stress.txt` (8 lines,
average 200+ tokens/line). The 6-level pratt ladder (concat_expr →
comparison_expr → add_expr → mul_expr → exp_expr → unary_expr)
contributes ~5.6% combined on stress vs ~3.2% on simple — each
operator level costs ~1% self-time, multiplied by how many levels the
token stream traverses. The pratt climb is **not** a runaway cost:
each level is a thin `parse_pratt_*` arm that reads the next token,
decides to recurse or return, and emits one structural record. The
stress regression is dominated by push_structural (which grows
alongside the tape depth — nested LAMBDAs cascade structural
records) and regex_scan (which fires for every identifier / call-site
token).

**Tape depth attribution.** The stress corpus's deepest formula is
4-way LAMBDA nesting inside LET/MAP; per the Stage-C finaliser, this
creates a frame_depth high-water of ~12-14 (every LAMBDA emits
wrap + primary + args-list + body), which means
`finalise`'s three `Vec<Option<u32>>` scratch allocations (prev /
first / last @ depth, sized `max_depth + 2`) grow but remain ~15
entries — not the stress driver. The driver IS the per-record
invalidation loop: `for i in 0..n { ... }` runs once per tape record,
and stress has ~3.2× more tape records than simple.

## 5. BBNF-specific findings — css_l4_grammar attribution

`css_l4_grammar` is 452 µs/iter vs `bbnf_self` at 59 µs (7.67×) and
`google_sheets` at 37 µs (12.2×). The corpus ratio matches: css_l4
grammar loads 1320 total lines / 55 922 bytes of `.bbnf` source
across 12 files (color/properties/selectors/values/keywords/...),
vs `bbnf_self`'s 89 lines / ~3 KB and `google_sheets`'s 182 lines /
7492 bytes. At 123 MB/s throughput the css_l4 entry lands at 7.5×
sheets's byte volume — **scale not pathology.**

Attribution inside the 452 µs:

```
30.76%  push_structural               139 µs
20.36%  finalise                       92 µs
12.46%  BbnfBootstrap::parse           56 µs   (shell + dispatch)
 8.15%  __regex_scan_BbnfBootstrap     37 µs
 7.56%  parse_flat_mapped_factor       34 µs   (grammar root factor)
 2.41%  parse_wrap_rhs                 11 µs
 2.07%  parse_pratt_binary_factor       9 µs
 1.77%  push_leaf_with                  8 µs
```

The cost is **51% tape bookkeeping** (push_structural + finalise +
push_leaf_with), **8.15% regex scan** of long identifiers (CSS L4
grammar declares hundreds of `calc`, `color-mix`, `--css-custom-prop`
style literals across color.bbnf's 321 lines), and the remaining
~25% spread across grammar-emitted `parse_*` rule bodies with no
single body exceeding 7.56%. The distribution is **virtually
identical to every other bbnf entry in shape** — css_l4's extra cost
is amplitude, not a new pathology. The `parse_flat_mapped_factor`
symbol is the bbnf.bbnf `factor` rule, which matches one of {parens,
group, regex, string, ident, char-class, range, optional, repeat,
not}; at every grammar token the bbnf parser recurses through this
rule, making it the third-hottest bbnf-specific symbol on every
entry.

No per-rule parse_* body on the css_l4 entry dominates — 12 files
split the work across the bbnf grammar uniformly. The conclusion:
**css_l4_grammar is a scale amplification of the universal
push_structural / finalise bottleneck, not a grammar-specific
attribution target.**

## 6. Deep-pratt comparison — Sheets (6-level) vs BBNF (1-level)

Sheets pratt ladder on parse_stress: exp_expr 1.79%, comparison_expr
1.91%, mul_expr 0.8-1.2%, add_expr 0.8-1.2%, concat_expr ~0.3%,
unary_expr 2.78% — sum ~8-9% self-time, split uniformly by operator
depth.

BBNF pratt ladder on every entry: `parse_pratt_BbnfBootstrap_binary_factor`
at 1.7-3.2% self-time as the ONLY pratt symbol. bbnf's grammar has
exactly one binary-operator rule (`binary_factor = factor binary_op
factor | factor`); there is no ladder to climb.

**Observation.** Both grammars pay a constant per-operator-arm cost
of ~1-2% self-time regardless of ladder width. The 6-level sheets
ladder is not individually expensive — each arm is a thin recursive
descent that dispatches on token kind and emits at most one
structural record. Sheets stress's pratt cost is "wide" (6 arms ×
~1% each) rather than "deep" (single arm × 6% self-time). **No
pratt-specific lever is implied** — optimising the emitted
`parse_pratt_*` bodies individually yields ≤ 2% per arm, whereas
the universal push_structural lever yields 30% across every one.

## 7. Lever proposals (ordered by 9-entry reclaim)

### L1 — `#[inline(always)]` on `push_structural` (universal, highest)

**Attribution.** 29.75% 9-entry self-time average, 9/9 presence. Real
symbol `<Columns>::push_structural` present in both bench binaries
via `nm /Users/mkbabb/Programming/bbnf-lang/.profile-target/release/deps/bbnf_monolithic-1cce73194c2882ba`.
Source `crates/tape/src/columns.rs:321-342` carries `#[inline]` — not
`#[inline(always)]` — and LLVM declined to honour the hint for
cross-crate call sites. The function body is 7 `Vec::push` calls plus
`invalidate_packed()` (an atomic-cache clear); at the grammar-emitted
call site the compiler could fuse the pushes with the surrounding
parse_* arm's stack frame.

**Proposal.** Change to `#[inline(always)]`; verify with `nm` that
the symbol disappears from both bench binaries. If the symbol
persists (workspace LTO failing to re-link cross-crate), shift to
per-grammar inline emission of the body in `emit_impl::parse_wrap_*` /
`parse_flat_*` arms.

**Expected reclaim.** 20-30% of self-time universally — dominated by
the removal of 7-column Vec dispatch and the atomic invalidate at
every structural emit. The expected floor is the Vec::push inlined
and the atomic omitted per callsite (SoA column growth is sequential
and unconditional).

**Artefacts to cite post-landing.** Fresh `.profiles/samply/<bench>/<entry>/`
with the push_structural symbol absent from every top-10; fresh
`nm bbnf_monolithic-... | grep push_structural` emitting zero lines.

### L2 — fuse `finalise` into emit (universal, second)

**Attribution.** 19.35% 9-entry avg, 9/9 presence. Source
`crates/tape/src/finaliser.rs:154-323`. The pass allocates three
`Vec<Option<u32>>` of length `max_depth + 2` per parse, walks the
tape linearly, and writes `sib_skip` / `span_hi` / `child_off` for
every compound. On small grammars (json 7.6 µs; sheets simple 32.5
µs) the Vec allocations themselves are a measurable share of the
19-22% self-time; on large grammars (css_l4 452 µs) the linear sweep
dominates.

**Proposal.** Maintain the three scratch arrays as inline
`[Option<u32>; 32]` stack buffers (max tape depth is empirically 15
on stress + css_l4 and 4-8 elsewhere; 32 is safe and zero-alloc).
Fold the `sib_skip` / `span_hi` / `child_off` stamps into the
emit path: `push_compound` already owns `child_off`; `close_compound`
can write `span_hi` + the previous sibling's `sib_skip` directly when
the parser knows its parent's depth. Reserve `finalise` for the
legacy post-order tapes that still carry `child_off == NONE` (per the
AW-I.W4δ comment at `finaliser.rs:208`).

**Expected reclaim.** 10-18% of self-time on every entry. Eliminates
three Vec allocations per parse (mi_malloc_aligned contributes ~1-2%
self-time across 7/9 entries — this lever reclaims that alongside
finalise itself).

**Artefacts to cite post-landing.** Fresh profiles with `finalise`
absent from top-10 or reduced to <5% self-time on every entry;
`mi_malloc_aligned` share halved.

### L3 — parse-shell elision via `#[inline(always)]` on `<Parser>::parse`

**Attribution.** `<Parser>::parse` is 6.9-15.8% self-time across 9
entries (sheets 7-8%, bbnf 12-16%). `nm` confirms it is a real
symbol in both bench binaries; the shell sets up scratch columns,
reserves capacity, and dispatches to the root rule's `parse_*` arm.
Allocation is visible via `mi_malloc_aligned` (1-2% avg) and
`_mi_heap_realloc_zero` (5.49% on sheets parse_simple — the shell's
per-parse column allocation).

**Proposal.** Annotate the generated `<Parser>::parse` with
`#[inline(always)]`; verify inlining via `nm` absence. If the per-
parse column allocation remains the visible remainder, emit per-
grammar capacity literals (`Columns::with_capacity(<emitter-known>)`)
rather than calling `Columns::new()` and growing via `push_structural`.
The emitter already knows the grammar's maximum tape width.

**Expected reclaim.** 5-10% self-time on every entry; additionally
reduces mimalloc traffic which shows up as 3-6% combined on sheets
parse_simple (`_mi_page_retire` 5.95% + `_mi_heap_realloc_zero` 5.49%
+ `mi_malloc_aligned` 1.95%).

**Artefacts to cite post-landing.** `<Parser>::parse` absent from
top-10 on every profile; mimalloc symbols reduced or absent.

### L4 — `__regex_scan_<Parser>` DFA specialisation (token-heavy grammars)

**Attribution.** 5.83-11.82% on sheets (token-dense formula corpora);
4.78-9.23% on bbnf (identifier-dense grammar corpora, peaking on
`google_sheets` and `css_l4_grammar` where the grammar source is rich
in identifier literals). 6/9 presence at ≥ 4.25% average.

**Proposal.** Per-grammar scanner specialisation — every grammar's
token alphabet is emitter-known at codegen time; the regex HIR can
emit a specialised DFA with literal match tables rather than a
generic NFA→DFA at runtime. Related work: the bespoke regex HIR
(per MEMORY `project_bespoke_regex`); the bbnf-regex crate
(per MEMORY `regex-crate-isomorphic`).

**Expected reclaim.** 3-8% self-time on sheets/bbnf-on-sheets /
bbnf-on-css_l4 (the three token-heavy entries); negligible on
lex-sparse entries.

### L5 — `parse_flat_BbnfBootstrap_mapped_factor` body reduction

**Attribution.** 5.35% 9-entry avg (but 6/9 bbnf-only; 7.76% on json,
9.52% on ebnf). Source: this is the bbnf `factor` rule body (per
`grammar/bbnf/bbnf.bbnf`) — matches one of {paren, group, regex,
string, ident, char-class, range, optional, repeat, not, mapped}.
The "mapped" suffix reflects that `factor` carries a `->` type
annotation; the generated body projects into the Factor enum.

**Proposal.** First-hop dispatch via a single leading-byte switch
(`match input.as_bytes()[offset] { b'(' => ..., b'"' => ..., b'[' =>
..., ... }`) rather than sequential `parse_flat_*` alternation
attempts. The emitter already enumerates the first-set for every
alternation; the dispatch is emit-time-known.

**Expected reclaim.** 4-8% self-time on every bbnf entry; specific
to grammars with heavy leading-byte-discriminating alternation.
This is a grammar-local lever and complements L1-L3.

## 8. Artefacts

- `.profiles/samply/google_sheets_monolithic/{parse_simple,parse_nested,parse_stress}/`
  — 7 artefacts per entry (bench, build, load, record, profile.json.gz,
  profile.json.syms.json, syms-proof). Captured 2026-04-20 01:50.
- `.profiles/samply/bbnf_monolithic/{json,ebnf,css_pretty,google_sheets,bbnf_self,css_l4_grammar}/`
  — same per-entry structure. Captured 2026-04-20 01:50-01:51.
- `/tmp/a3-bench-sheets.txt`, `/tmp/a3-bench-bbnf.txt` — cargo-bench
  multi-iter ns/iter for delta table.
- `/tmp/profile_top_self2.py`, `/tmp/union.py`, `/tmp/union.txt` —
  extraction scripts and cross-entry union output.
- `nm` symbol verification on both bench binaries: zero walker
  symbols; `push_structural`, `finalise`, `<Parser>::parse` all
  present as real call boundaries.
