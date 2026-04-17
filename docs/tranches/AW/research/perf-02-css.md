# perf-02 — `css_l4` samply attribution (AW-III.P2)

samply attribution across `css_l4` × {normalize, bootstrap, tailwind}
against the DTA walker binary `css_l4-30ae315e94356387`
(`code_id=FD3D1F594DF23027878CBEF695FAEB90`) produced by
`scripts/prepare-profile-wave.sh` into `.profiles/shared-target`.

## Methodology

`scripts/profile-bench-headless.sh` — one invocation per entry with
`--record-port 3140 --load-port 3141`, prebuilt binary handed in via
`--bin`. Hot-loop self-time extracted from `profile.json.gz` by
joining `threads[0].samples.stack` → `frameTable.address` →
`resourceTable.lib` → `libs[].debugName` → `profile.json.syms.json`
`symbol_table[].rva` ranges (the Firefox Profiler gzip has only
RVA-addressed frames; the syms JSON owns the name table). Self-time
is the stack-leaf timestamp delta between adjacent samples;
rolled-up entries agree with the named-frame coverage in each
`syms-proof.txt`.

Artefact roots:
`.profiles/samply/css_l4/{normalize,bootstrap,tailwind}/`.
Every claim below cites one of the seven required files.

## Artefact coverage

| Entry | bench.txt | build.txt | record.txt | load.txt | profile.json.gz | profile.json.syms.json | syms-proof.txt | Status |
|-------|-----------|-----------|------------|----------|-----------------|------------------------|----------------|--------|
| normalize | Apr 17 | Apr 17 | Apr 17 | Apr 17 | Apr 17 | Apr 17 | Apr 17 | valid — current binary |
| bootstrap | Apr 17 | Apr 17 | Apr 17 | Apr 17 | Apr 17 | Apr 17 | Apr 17 | valid — current binary (truncation artefact, see §3) |
| tailwind  | Apr 17 | Apr 17 | Apr 15 | Apr 15 | Apr 15 | Apr 15 | Apr 15 | **profile invalid** — see §4 |

Fresh Apr 17 files were produced by the profile script against the
shared-target binary `css_l4-30ae315e94356387`. The Apr 15 files in
the tailwind artefact dir are residue from a prior wave against
binary `css_l4-dbc952b9298151f8`
(`code_id=2BBF5F06577F3689A2F069C732438A79`, per
`tailwind/profile.json.syms.json` `data[1].code_id`) — an obsolete
architecture (pre-DTA fn-per-rule codegen). These files remain
on disk only because the Apr 17 bench step panicked and `set -e`
aborted the script before the samply record step overwrote them.
See §4.

## 1. normalize

**Corpus**: `data/css/normalize.css`, 6 138 B, 34 `{` tokens,
29 commas — tiny reset stylesheet.

**bench.txt** (Apr 17 00:23:39): `5,875 ns/iter (+/- 237) = 1044 MB/s`
over 1 866 samples, `total: 1 866.55 ms` wall in the record window.

Top-20 self-time attribution (extracted from `profile.json.gz`
joined against `profile.json.syms.json`, user+system frames):

| Rank | Self % | Self (ms) | Function |
|------|--------|-----------|----------|
|  1 | 56.35 | 1051.78 | `DtaDfaScanner::scan` |
|  2 | 18.15 |  338.87 | `bbnf_tape::driver::dispatch_one` |
|  3 |  7.18 |  134.04 | `bbnf_tape::driver::try_branch` |
|  4 |  6.05 |  112.85 | `bbnf_tape::driver::reserve_compound` |
|  5 |  2.64 |   49.21 | `SipHasher13<…>::write` |
|  6 |  1.57 |   29.33 | `bbnf_tape::driver::dta_run` |
|  7 |  1.34 |   25.00 | `HashMap<String, Arc<Dfa>>::get::<str>` |
|  8 |  1.02 |   18.99 | `bbnf_tape::driver::advance_or_pop_with` |
|  9 |  0.92 |   17.12 | `_platform_memcmp` |
| 10 |  0.85 |   15.93 | `parse_that::scanners::cached_dfa` |
| 11 |  0.77 |   14.34 | `bbnf_tape::finaliser::finalise` |
| 12 |  0.52 |    9.78 | `bbnf_tape::driver::close_compound` |
| 13 |  0.32 |    6.00 | `bbnf_tape::driver::pop_and_release` |
| 14 |  0.31 |    5.84 | `Columns::truncate` |
| 15 |  0.22 |    4.02 | `[css_l4+76b00]` (unsymbolicated inline frame) |
| 16 |  0.11 |    2.07 | `mi_heap_malloc_zero_aligned_at_generic` |
| 17 |  0.11 |    2.01 | `mach_absolute_time` |
| 18 |  0.11 |    2.00 | `DtaTable::rule_entry_for` |
| 19 |  0.11 |    2.00 | `TapeBuilder::finish` |
| 20 |  0.11 |    2.00 | `mi_find_page` |

**Bottleneck classification**:

- **Scanner layer** — `DtaDfaScanner::scan` + `cached_dfa` +
  `HashMap::get` + `SipHasher::write` + `_platform_memcmp` =
  **60.49%**. Per-regex-match the scanner does a `HashMap<String,
  Arc<Dfa>> lookup keyed by the pattern string` before running the
  DFA (see `parse-that/rust/parse_that/src/scanners.rs:30-55`; the
  ZST `DtaDfaScanner` in expand.rs:162223 defers pattern → DFA
  resolution per call). The hash cost alone is 4% of runtime; the
  DFA execution is the remaining ~56%.
- **Driver dispatch** — `dispatch_one` + `try_branch` +
  `dta_run` + `advance_or_pop_with` = **27.92%**. Every state
  transition goes through the `match table.states[state_idx]`
  arm-selector (driver.rs:867); there is no fused dispatch over
  state sequences.
- **Compound emit / frame bookkeeping** — `reserve_compound` +
  `close_compound` + `pop_and_release` + `Columns::truncate` +
  `finalise` = **8.47%**. `reserve_compound` (driver.rs:1399)
  pushes seven parallel vectors per compound frame open — each
  push carries a bounds-check and amortised grow.
- **Allocator / noise** — `mi_*` + `mach_absolute_time` +
  `_platform_memcmp` + dyld = rest.

## 2. bootstrap

**Corpus**: `data/css/bootstrap.css`, 280 311 B, 2 671 `{`, 656 commas.

**bench.txt** (Apr 17 00:23:55):
`196,100 ns/iter (+/- 10,683) = 1429 MB/s` — 1 429 MB/s on 280 KB
reports **faster than a cache-cold full parse can physically be**;
by comparison normalize (6 KB, L1-resident) reports 1 044 MB/s and
normalize's sample set is dominated by a realistic DFA sweep. The
throughput anomaly is load-bearing: it signals the parser is
**not parsing the full input**.

The grammar root `stylesheet = ruleList ?w ;` where
`ruleList = (ruleItem ?w) *` (grammar/css/l4/stylesheet.bbnf:37-38)
is zero-or-more. When the first `ruleItem` cannot match, the outer
Repeat's `lo=0` accepts zero iterations and `stylesheet` trivially
closes with an empty record. The pre-flight brief confirms 9 records
total emitted — 2 662 `{` tokens (99.66%) of the input never entered
the AST. The `1 429 MB/s` headline number is therefore a **correctness
regression disguised as performance**, not a bench result; it must
not be folded into any DTA-viability verdict.

Top-20 self-time attribution (`profile.json.gz` +
`profile.json.syms.json`, 4 627 samples, 4 628.05 ms total):

| Rank | Self % | Self (ms) | Function |
|------|--------|-----------|----------|
|  1 | 97.46 | 4510.67 | `DtaDfaScanner::scan` |
|  2 |  0.56 |   25.99 | `bbnf_tape::driver::dispatch_one` |
|  3 |  0.32 |   14.97 | `bbnf_tape::driver::try_branch` |
|  4 |  0.30 |   13.98 | `mach_absolute_time` |
|  5 |  0.24 |   11.21 | `bbnf_tape::driver::reserve_compound` |
|  6 |  0.19 |    9.00 | `HashMap<String, Arc<Dfa>>::get::<str>` |
|  7 |  0.17 |    7.98 | `bbnf_tape::driver::dta_run` |
|  8 |  0.17 |    7.97 | `__open` |
|  9 |  0.09 |    3.99 | `read` |
| 10 |  0.06 |    2.99 | `_platform_memmove` |
| 11 |  0.06 |    2.99 | `SipHasher13<…>::write` |
| 12 |  0.04 |    2.00 | `bbnf_tape::driver::advance_or_pop_with` |
| 13 |  0.04 |    1.99 | `mi_segment_span_allocate` |
| 14 |  0.03 |    1.24 | `mi_page_free_list_extend` |
| 15 |  0.02 |    1.08 | `mi_malloc_aligned` |
| 16 |  0.02 |    1.01 | `parse_that::scanners::cached_dfa` |
| 17 |  0.02 |    1.00 | `mi_large_huge_page_alloc` |
| 18 |  0.02 |    1.00 | `mi_segment_commit_mask` |
| 19 |  0.02 |    1.00 | `mi_segment_span_free` |
| 20 |  0.02 |    1.00 | `alloc::raw_vec::RawVecInner::finish_grow` |

**Interpretation**: 97.46% `DtaDfaScanner::scan` does **not** mean
the scanner is intrinsically 97% of a real parse — it means the
bench iteration spent almost the entire wall window re-running the
same short scan sequence against bootstrap's opening bytes. With 9
records emitted per iteration and the iteration running in 196 µs,
each iteration covers a few dozen scanner calls; across thousands
of iterations (`bencher`'s auto-bench schedule) the sampler
naturally concentrates on `scan` and `alloc`. The distribution
here is **not a valid snapshot of end-to-end CSS parse hotspots**;
its only signal is that the scan path is hit, which every entry
confirms.

**Action**: do not cite bootstrap's self-time percentages in any
AW-IV lever-sizing argument. Use normalize for DTA walker attribution
on small inputs. Use tailwind only when the parse truncation is
resolved (see §4).

## 3. tailwind (profile invalid)

**Corpus**: `data/css/tailwind.css`, 3 642 321 B, 39 150 `{`,
14 482 commas.

**bench.txt** (Apr 17 00:24:09):

```
running 1 test
test tailwind  ...
thread 'main' (…) panicked at crates/core/benches/css/l4.rs:170:1:
tailwind.css: parse failed: Syntax { offset: 3633741, rule: None }
```

The bench macro's outer unwrap (l4.rs:156-158) panics before the
timed loop opens. `scripts/profile-bench-headless.sh` executes the
binary inside `bench.txt` first, then under `samply record` second;
the pre-bench panic makes the shell exit on `set -e`, so the
samply-record step never runs against the current binary.

The Apr 15 residues (`record.txt`, `load.txt`, `profile.json.gz`,
`profile.json.syms.json`, `syms-proof.txt`) survived because nothing
overwrote them. Their `code_id=2BBF5F06577F3689A2F069C732438A79`
and symbol set — `CssL4Parser::__compoundSelector`,
`CssL4Parser::__declaration`, `CssL4Parser::__ruleItem`,
`CssL4Parser::__value`, `CssL4Parser::__complexSelector`,
`CssL4Parser::__ruleBlock`, `CssL4Parser::__selectorList`,
`CssL4Parser::__funcBody`, `parse_that::parsers::scan::ws_comment::
scan_ws_block_comments_slow`, etc. (list extracted from
`tailwind/profile.json.syms.json`) — belong to the pre-AW-I
per-rule combinator codegen, which has been deleted from master.

The current binary carries no `CssL4Parser::__*` functions (see
`normalize/profile.json.syms.json`, which lists only
`CssL4Parser::parse` plus the shared DTA walker). **The stale
profile cannot attribute anything about the current architecture**
and must not be cited in AW-IV lever-sizing.

**Action**: the tailwind entry is blocked on the offset-3633741
parse failure. AW-IV's viability judgement on CSS cannot rely on
tailwind self-time until the grammar extension that closes offset
3633741 lands (likely a missing `!important` / vendor-prefixed-at
construct near the tail; the offset is 7 980 bytes shy of
end-of-file). Once the parse succeeds, re-profile against the
same prebuilt binary under ports 3140/3141.

## 4. Cross-entry synthesis

Normalize is the only css_l4 entry whose profile samples faithfully
represent the steady-state DTA walker. The hot-path triad on
normalize is:

```
DtaDfaScanner::scan               56.35 %   (DFA body + HashMap + hasher)
driver::dispatch_one              18.15 %   (state-arm dispatch)
driver::try_branch                 7.18 %   (Alt branch loop + backtrack)
driver::reserve_compound           6.05 %   (7-vector compound open)
```

This triad captures **87.73 %** of runtime. The remaining mass is
split between `advance_or_pop_with`, `close_compound`,
`pop_and_release`, `finalise`, plus allocator and dyld overhead.

Compared against JSON (perf-01, referenced for context from the
sibling wave), CSS leans considerably more on **try_branch /
AltLinear backtracking**: JSON is dominated by a short list of
literal-prefix dispatches, whereas CSS's compound-selector
grammar opens a 5-way literal Alt per compound token, and the
`pseudoClass` rule opens an 8-way function-name Alt. The CSS
scanner also invokes a broader pattern set per byte (ident
regexes, number regexes, attribute-value regexes, wqName escapes)
each paying the `cached_dfa` HashMap tax.

## 5. CSS-specific AW-IV lever hypotheses

Cited against normalize's Apr 17 profile
(`normalize/profile.json.gz` + `normalize/profile.json.syms.json`)
and the grammar source at
`grammar/css/l4/{selectors.bbnf,values.bbnf,color.bbnf}`.

### H1. Replace `cached_dfa` per-call HashMap with pattern-indexed DFA array

**Evidence**: rows 1+7+10 in normalize = `scan` + `HashMap::get` +
`cached_dfa` = 57.04 % (1 093 ms of 1 867 ms). Row 5 SipHasher =
2.64 %. Together the cached_dfa indirection is ≥ 4.6 % of runtime
as pure overhead on top of DFA execution.

**Proposal**: emit a `static DFA_TABLE: &[&'static Dfa]` indexed by
a compile-time pattern id assigned by the emitter. `DtaState::Regex`
holds the id, not the pattern string; scanner trait becomes
`fn scan(&self, pattern_id: u32, input: &[u8], offset: usize)`.
Zero hash, zero interning at parse time; one indirect deref.

**Expected impact**: recovers the 4.6 % hash/lookup overhead
outright and opens inlining of DFA body into `dispatch_one`'s
Regex arm (the `Arc<Dfa>` barrier currently blocks it).

### H2. Compound-selector classifier table

**Evidence**: rows 2+3 in normalize = `dispatch_one` + `try_branch`
= 25.33 %. The `DtaState::AltLinear` arm (driver.rs:1017-1136) runs
a linear `for (branch_idx, &branch) in branches.iter()` loop;
grammar `compoundSelector = (classSelector | idSelector |
attrSelector | colonSelector | typeSelector)+` (selectors.bbnf:87)
opens five branches per simple-selector token. Each failed branch
re-runs `scanner.scan()` from scratch after `columns.truncate`,
`frame_depth.truncate`, `psi.truncate`, `stack.restore` (lines
1113-1117).

**Proposal**: add a `DtaState::ClassifyByte { table: &'static
ByteClass, targets: &'static [DtaStateId] }` variant that looks up
the next byte in a 256-entry LUT → branch index → direct jump.
The emitter already has FIRST-set analysis
(grammar/css/l4/color.bbnf:6-14 notes dispatch requirements);
the classifier turns that FIRST set into a single byte LUT indexed
by the input byte at the current offset.

For `compoundSelector`: `.` → classSelector, `#` → idSelector,
`[` → attrSelector, `:` → colonSelector, else → typeSelector.
One LUT probe replaces up to five scan+truncate rounds.

**Expected impact**: the 25.33 % spent in `dispatch_one + try_branch`
is largely **`try_branch`'s savepoint-restore loop on failed
branches** — a classifier reduces the attempted-branch count to 1
for the common case, and the savepoint machinery disappears when
there is no retry.

### H3. PHF keyword dispatch for named-set Alts

**Evidence**: grammar branch counts —
`color.bbnf`: 163 literal-keyword branches (`namedColor`,
all CSS Color L4 named colors);
`keywords.bbnf`: 72 branches;
`properties.bbnf`: 92 branches; `pseudoClass` (selectors.bbnf:71):
8-way function-name alt. Each keyword Alt today is a linear
`AltLinear` whose branches are `DtaState::Literal { text }` —
literal `_platform_memcmp` per branch (row 9 normalize: 0.92 %,
concentrates in the hot color/keyword lookups on larger corpora).

**Proposal**: emit a `DtaState::PhfKeyword { keys: &'static Phf,
payloads: &'static [u32], fall_through: DtaStateId }` variant. The
emitter collects each Literal-Alt subset where every branch is
`Literal { text }` with no subsequent Seq prefix, generates a
perfect hash (rust-phf), and the driver runs: (a) DFA-scan an
identifier regex once, (b) PHF-lookup the matched slice, (c)
commit the branch's payload. One scan + one probe replaces N
scan-literal attempts.

**Expected impact**: tailwind's color-value hot path is the
canonical candidate — tailwind ships ~1 800 instances of named-color
tokens (estimated from tailwind.css grep patterns). Each saved
scan+compare per color = N × 156-byte literal ladder collapse.
Cannot quantify against current profiles because bootstrap is
truncated and tailwind parse fails; normalize has few named colors.
The lever's impact must be re-measured post-tailwind-unblock.

### H4. Pratt / ShuntingYard activation for `calc`/`min`/`max`/`clamp`

**Evidence**: `grammar/css/l4/values.bbnf:49-55` —
```
mathProduct = mathValue , ( ("*" | "/") >> mathValue ) * ;
mathExpr    = mathProduct , ( ("+" | "-") >> mathProduct ) * ;
calcFunction  = "calc" , "(" >> mathExpr << ")" ;
…
```
These rules are Pratt-eligible but lower to nested `Seq` + `Repeat`
+ `AltLinear` (the walker re-enters `dispatch_one` once per
operator occurrence, plus once per operand). The driver's
`DtaState::ShuntingYard` variant exists (driver.rs:1302) but the
emitter never selects it for these rules — confirmed by the absence
of any ShuntingYard frame in the normalize sample set
(0 ms self-time, 0 samples; cf. driver.rs:1302's ShuntingYard arm
which would surface under `dispatch_one`'s match-arm attribution).

**Proposal**: extend the emitter's rule-shape classifier to emit
`DtaState::ShuntingYard { head, precedence }` for any rule of the
shape `left , (op >> right) *` where `op` is a literal-Alt over a
fixed operator set. The Pratt reducer produces a single binary
compound per operator level instead of a Seq+Repeat nest; the walker
transitions per operator are a push/pop on the auxiliary stack
(driver.rs:158, `OpStackEntry`) rather than a full Frame.

**Expected impact**: neither normalize nor bootstrap exercises
calc-heavy tails, so normalize's profile shows 0 % attribution to
any `calcFunction` / `mathExpr` frames. Tailwind uses calc()
extensively (utility-first CSS emits `calc(var(--tw-...) * N)`
patterns); this lever moves with H3 — both are bound to the
tailwind unblock.

### H5. `reserve_compound` column-batch fusion

**Evidence**: row 4 normalize = 6.05 % self-time on
`reserve_compound`. The function pushes seven parallel Vecs
(driver.rs:1408-1415: `kinds`, `flags`, `extra`, `span_lo`,
`span_hi`, `sib_skip`, `child_off`, plus `frame_depth`) — each push
performs a bounds-check + occasional `RawVec::grow_one` (row 20
normalize: `RawVec::finish_grow` 1.00 ms).

**Proposal**: ahead-of-compound capacity reservation — the
`GRAMMAR_PROFILE.capacity_for(input.len())` estimate
(expand.rs:162260) already bounds column growth, but individual
pushes still bounds-check. Replace the seven `push` calls with a
single `push_compound(kind, flags, extra, span_lo, span_hi, depth)`
method that writes through `as_mut_ptr().add(len)` after a single
`reserve(1)` precheck; the `frame_depth` push folds into the same
precheck.

**Expected impact**: the 6.05 % breaks down into bounds-checks
(~½) and amortised grows (~½). A single-check fused push reclaims
~3 %.

### H6. ShapeRef collapse for the declaration compound

**Evidence**: research/02-shaperef-runtime-dispatch.md notes
bootstrap has 6 014 colons (declarations) — each fans to 5-7 tape
records today. normalize at 6 KB/34 `{` has ≈ 120 declarations
assuming 3.5 declarations per rule block (typical normalize.css
shape), which under ShapeRef collapses to 120 records instead of
600-840. The measured impact on normalize is modest (it is
scanner-bound), but on bootstrap (once un-truncated) and tailwind
(once un-blocked) the compound-emit mass moves from the current
`reserve_compound + close_compound = 6.57 %` baseline into the
decision point for **total record count**, which in turn drives
`Columns::truncate` (row 14 normalize: 0.31 %) and `finalise`
(row 11: 0.77 %).

**Expected impact**: dependent on ShapeRef lowering (AW-IV.W3);
directly adjacent to H5's compound emit fusion.

## 6. Summary

- normalize is the only valid current-architecture css_l4 profile.
  Hot-path triad = scanner (57 %) + driver dispatch (27 %) +
  compound emit (7 %).
- bootstrap's 1 429 MB/s headline is the artefact of a 9-record
  truncation; the samply profile is scanner-saturated from repeated
  short scans and carries no end-to-end signal.
- tailwind's samply profile on disk is from an obsolete binary
  (`code_id=2BBF5F06…`); the current binary panics at offset
  3633741 before samply recording starts.
- Six AW-IV levers cited with artefact grounding — four (H1, H2,
  H3, H5) directly reduce normalize's measured hot-path mass;
  two (H4, H6) require the bootstrap / tailwind unblock to be
  measurable against the full CSS corpora.
- Pre-conditions for full CSS lever-sizing: (a) the grammar
  extension that closes bootstrap's 9-record truncation (restoring
  a valid full-corpus samply), (b) the grammar extension that
  closes tailwind's offset-3633741 parse failure.
