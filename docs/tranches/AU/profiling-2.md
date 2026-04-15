# Tranche AU — Profiling Wave 2

Full-matrix follow-up to `profiling-1.md`. Where wave 1 sampled one
representative entry per bench, wave 2 covers every (bench, entry)
pair: five JSON datasets, three CSS L4 stylesheets, three Sheets
formula packs, six BBNF grammar files, ten bbnf-vs-sonic parity
entries — 27 runs across five dedicated port pairs, one prepared
binary per bench, one shared `CARGO_TARGET_DIR` for the wave.

All MB/s figures are cold per-parse on the prebuilt release binary
under `samply record --unstable-presymbolicate`. Every claim below is
anchored to a file on disk under `.profiles/samply/`. No bench re-run,
no `cargo expand` re-run — the prepared artifacts were reused.

## Throughput matrix (cold parse)

### JSON (bbnf tape-first)

| dataset | bench MB/s | record MB/s | AQ baseline | AT baseline |
|---------|-----------:|------------:|------------:|------------:|
| data    | 2045 | 2106 | 1939 | 1944 |
| twitter | 2283 | 2261 | 2086 | 2193 |
| citm    | 2690 | 2713 | 2698 | 2661 |
| canada  | 1293 | 1271 | **1796** | 1483 |
| data_xl | 1219 | 1324 | 1348 | 1228 |

Post-AU.1 payload-activation picture: data/twitter/citm recovered to
or past AT; canada and data_xl remain depressed versus AQ because
each `__value` number branch now keeps the Eisel–Lemire `f64` in the
tape, and these two datasets are >80% numeric by sample share.

### CSS L4

| stylesheet | bench MB/s | record MB/s | AT | vs cssparser | vs lightningcss |
|------------|-----------:|------------:|---:|-------------:|----------------:|
| normalize  | 1044 | 1084 | 999 | 1.46× | 3.76× |
| bootstrap  | 578  | 572  | 513 | 1.24× | 4.51× |
| tailwind   | 608  | 644  | 579 | 1.37× | 6.30× |

All three stylesheets improved over AT (4.5–12.7%) from the AU.1
projection fix. Bootstrap is still 4% short of the AU.2 hard gate
(≥600 MB/s) because CSS scanner activation has not landed.

### Google Sheets

| formula pack | bytes | lines | bench MB/s | record MB/s | ns/formula |
|--------------|------:|------:|-----------:|------------:|-----------:|
| simple       |   505 |    34 |  93 |  99 |  158 |
| nested       |  1456 |    31 | 124 | 122 |  377 |
| stress       |  1838 |     8 | 116 | 112 | 1968 |

Simple is the floor because per-parse setup cost (allocator thrash
through the tape `with_capacity` hint) dominates sub-200 ns formulas.
Nested and stress amortize this cost and settle at the precedence-
tower steady-state near 110–125 MB/s.

### BBNF self-hosting bootstrap

| grammar         | bytes   | bench MB/s | record MB/s | shape |
|-----------------|--------:|-----------:|------------:|-------|
| json            |    483  | 266 | 262 | single file |
| ebnf            |  1 453  | 217 | 213 | single file, comment-heavy |
| css_pretty      |  2 558  | 631 | 649 | single file, mapping-heavy |
| google_sheets   |  4 210  | 591 | 579 | single file |
| bbnf_self       |  5 053  | 394 | 393 | 3-file `@import` |
| css_l4_grammar  | 40 716  | 439 | 439 | 15-file `@import` |

Throughput scales with mapping density and comment density, not
with total byte count. ebnf is the slowest because it is mapping-
heavy *and* comment-heavy; css_pretty and google_sheets are mapping-
heavy but comment-light and clear 600 MB/s.

### json_value head-to-head against sonic-rs

| dataset | bbnf MB/s | sonic MB/s | bbnf ÷ sonic |
|---------|----------:|-----------:|-------------:|
| data    | 1931 | 2494 | 0.77 |
| twitter | 2189 | 2744 | 0.80 |
| citm    | 2646 | 3097 | 0.85 |
| canada  | 1343 | 1574 | 0.85 |
| data_xl | 1240 | 1524 | 0.82 |

bbnf is at 77–85% of sonic's throughput on every dataset **while
doing strictly less semantic work** — the bbnf path never decodes
strings, never unescapes `\uXXXX`, never materialises a typed value
tree. The raw ratio will fall once the Phase 3 decode path lands;
that is the point at which the comparison becomes honest.

## Per-bench emitted-code fingerprints

Counts below are verbatim from the saved expand artifacts under
`.profiles/samply/prebuild/expand/<bench>/expand.rs` (BBNF dispatches
through `crates/core/src/grammar/generated.rs`; the bench expand is a
thin harness).

| bench | push_compound | push_leaf | push_leaf_with_* | `.map(\|_\| ())` |
|-------|--------------:|----------:|-----------------:|---------------:|
| json_monolithic         |   8 |  1 |  3 (f64, bool, u8) |   9 |
| css_l4                  | 234 | 22 |  7 (u32 ×1, u8 ×6) | 206 |
| google_sheets_monolithic|  37 |  0 |  0                 |  50 |
| bbnf_monolithic (gen'd) |  90 | 15 |  0                 | 106 |
| json_value              |   8 |  1 |  3 (f64, bool, u8) |   9 |

## JSON: scalar projection firing, string decode still discarded

`JsonParser::__value` at expand.rs:2478–2684 emits scalar leaves
for null / bool / number:

- line 2508 — string branch `scan_quoted_string_strict(state).map(|_| ())`, falls through to `push_leaf` at line 2677
- line 2638 — `push_leaf_with_u8` (null branch, `__payload_tag = 1`)
- line 2651 — `push_leaf_with_bool` (true/false branch, tag 2)
- line 2664 — `push_leaf_with_f64` (number branch, tag 3, fed by `scan_number_strict_f64`)
- `decode_json_string_to_arena`: 0 references anywhere in expand.rs
- `push_leaf_with_string`: not a method on `TapeBuilder` (builder.rs has `push_leaf_with_Span`, `push_leaf_with_aggregate`, `push_leaf_with_scalar<T>` and ten typed specialisations — no string-arena variant)

Profile top frames (canada, the tightest number-only loop):

| % | frame |
|--:|-------|
| 83.3 | `JsonParser::__value` |
| 11.5 | `eisel_lemire::compute_f64` |
|  4.1 | `TapeBuilder::push_compound` |

Number-decode SIMD is partially firing. `scan_number_mantissa`
(parse-that `number.rs:93`) uses SWAR 8-digit chunks for the
integer part by design (short runs dominate) and calls
`number_simd::scan_digits_simd` for the fractional part at
`number.rs:193`. The remaining `compute_f64` self-time on canada
(489 samples) is the Eisel–Lemire bridge, not scalar digit scan.

For string-heavy datasets (twitter, data, data_xl) the profile is
instead dominated by `memchr::memchr::{closure#0}` (7–19%) and
`trim_leading_whitespace_scan_and_cache` (4–12%) driven by whitespace
skip between pairs.

## CSS L4: compound churn is the headline, scanner activation is incomplete

Hotspot pattern is uniform across all three stylesheets:

| % leaf samples | symbol                            | normalize | bootstrap | tailwind |
|---------------:|-----------------------------------|----------:|----------:|---------:|
| 33–43          | `CssL4Parser::__compoundSelector` |    43.3   |    33.5   |   40.0   |
| 17–31          | `__declaration`                   |    17.4   |    31.5   |   25.7   |
| 11–13          | `scan_ws_block_comments_slow`     |    13.1   |    11.1   |   11.6   |
|  4– 7          | `__value`                         |     6.6   |     5.5   |    4.3   |

Compound bookkeeping in `__compoundSelector` and `__declaration`
bodies (nested Alt dispatch, per-branch offset backtracks,
`__has_children` bookkeeping) accounts for 33–43% + 17–31% = 50–75%
of self-time. `TapeBuilder::push_compound` itself is ≤ 0.63% — the
overhead is inside the rule bodies, not at the tape boundary.

Whitespace-with-comment scanning is 11–13% of self-time across all
three stylesheets. The call is the canonical `scan_ws_block_comments`,
which IS the fused scanner; the "zero fused CSS scanners" claim from
the pre-AU audit was wrong. The profile resolves most of its samples
to the `_slow` tail, which tells us the SIMD fast inner loop is
short-circuited on these inputs often enough that the byte-wise tail
dominates. This is an inner-loop tuning lever, not a missing-kernel
lever.

Number payload retention is **not** firing — 20 `scan_number_f64(...)
.map(|_| ())` discard sites remain. Activating `-> f64` on the
`number` rule in `grammar/css/l4/value-unit.bbnf` converts all 20
into typed leaves at zero new decode cost.

Hex color conversion is **not** firing. `parse_hex_color` exists as
a function declaration at expand.rs:61 but has zero call sites. The
`hex` rule still emits `push_compound`. The `push_leaf_with_u32` at
expand.rs:103160 belongs to `__namedColor` (line 97235) — keyword-
to-u32 constants like `yellowgreen → 0x9ACD32FF`. Hex retention is a
separate codegen gap.

Ident routing: 7 of 8 `scan_ident` call sites use
`DEFAULT_IDENT_CONFIG`; 1 uses `CSS_IDENT_CONFIG`. The prior audit's
"fix ident routing" claim stands.

## Sheets: 100% compound, precedence tower is the floor

The bench expand (lines 8437–10807) shows a six-level left-recursive
tower: `__comparison_expr → __concat_expr → __add_expr → __mul_expr
→ __exp_expr → __unary_expr`, each level emitting exactly
`mark_children` + `push_compound(Repeat)` + `push_compound(Rule)`
unconditionally, whether or not an operator at that precedence level
actually appeared.

Per-entry self-time:

|                     | simple | nested | stress |
|---------------------|-------:|-------:|-------:|
| `__unary_expr`      |  21.6  |  32.1  |  38.1  |
| `__comparison_expr` |   7.4  |   8.3  |  10.8  |
| `__mul_expr`        |   7.3  |   9.8  |   9.9  |
| `__exp_expr`        |   7.4  |   7.5  |   9.5  |
| `__concat_expr`     |   6.2  |   8.4  |   9.1  |
| `__add_expr`        |   6.5  |   7.0  |   8.9  |
| **tower total**     | **56.4** | **73.1** | **86.3** |
| allocator thrash (mi_*, memmove, grow_one) | 21.8 | 15.4 | 9.9 |

The tower cost scales linearly with input length at ≈ 9 ns/byte,
which caps throughput at ~110–125 MB/s regardless of formula size.
The allocator thrash is the other mass: the tape `with_capacity`
heuristic `input.len() / 2 + 2` is calibrated for JSON's scalar
density; a Sheets formula averages ≈ 1 tape record per input byte,
so the initial reserve is 6–8× short. On `=A1+B1` (14 bytes) we
reserve 7×16 B = 112 B of tape but need 15×16 B = 240 B — the first
record triggers `RawVec::grow_one` + `_mi_heap_realloc_zero` before
parsing finishes.

There is zero scalar projection: `push_leaf_with_* = 0`, `push_leaf = 0`,
every one of 37 terminal emissions is `push_compound`.

## BBNF: mapping-heavy rule bodies dominate

Generated parser (`crates/core/src/grammar/generated.rs`) push
counts: 90 compound, 15 leaf, 0 leaf_with. All 106 `.map(|_| ())`
discards are tied to child-value dropping inside Repeat/Opt frames.
Hot rules and their line positions in generated.rs:

| rule                 | starts | dominant work |
|----------------------|-------:|---------------|
| `__big_comment`      | 14143  | `memchr(b'*')` + `trim_leading_whitespace_mut` pair, one `push_compound(Rule)` |
| `__mapped_factor`    | 15181  | `__factor ; ("->" __value_expr __type_annotation?)?`, three nested `push_compound(Repeat)` emissions even when `->` is absent |
| `__binary_factor`    | 15407  | `__mapped_factor (op __mapped_factor)*` — two `push_compound` sites |
| `__rhs`              | 15896  | `__closure | __alternation` — `push_compound(Rule)` when children, dead `push_leaf` fallback otherwise |
| `__directive`        | 17569  | flat Alt over 8 directive branches (`@import … @host`) — each branch has its own `mark_children` |

Per-entry leaf-sample share of the top six rules:

| rule              | json | ebnf | css_pretty | google_sheets | bbnf_self | css_l4_grammar |
|-------------------|-----:|-----:|-----------:|--------------:|----------:|----------------:|
| `__mapped_factor` | 27.9 | **40.7** | 31.5 | 31.7 | 35.7 | 35.4 |
| `__rhs`           | 11.3 | 14.6 | 10.9 | 11.8 | 14.4 | 10.2 |
| `__directive`     | 16.7 |  9.6 | 17.2 | 18.7 | 13.0 |  7.3 |
| `__big_comment`   |  9.8 | 14.9 | 12.0 | 10.8 | 11.4 |  9.3 |
| `__binary_factor` |  8.2 | 12.1 |  8.7 |  8.2 | 10.9 |  9.1 |
| `parse` setup     |  8.3 |  4.2 | 12.8 | 13.1 |  8.4 |  9.3 |

No token-shaped rule (ident, string literal, int literal, regex)
emits a leaf payload. Consumers re-slice the source via
`identifier_text()` helpers against `input[lo..hi]`.

## json_value: honest parity requires Phase 3

`crates/core/benches/json/value.rs` constructs the bench this way:

- bbnf side: `JsonParser::parse(black_box(&input)).unwrap()`; the
  returned `Parsed<'_>` is immediately `black_box`-ed and dropped.
  No `.view()` call, no tape walk, no decoded string materialised.
- sonic side: `sonic_rs::from_str::<sonic_rs::Value>(&input)`, which
  allocates a padded input buffer, runs SIMD `parse_object` /
  `parse_array` with `DocumentVisitor`, unescapes every string into
  owned UTF-8, decodes every number into native `f64`/`i64`, and
  builds a full `Value` node tree before returning.

Paired canada hotspots illustrate the asymmetry:

| work                    | bbnf canada             | sonic canada |
|-------------------------|-------------------------|--------------|
| Dispatch / recursion    | 83.3% `__value`         | 79.4% `parse_array::<DocumentVisitor>` |
| Number decode           | 11.5% `compute_f64`     | inlined into `parse_array` |
| Tree coalesce / tape    | 4.1%  `push_compound`   | 9.8% `visit_container_end` |
| String decode           | 0%                      | 0% measurable on canada (number-only dataset) |
| Padding / input handling| 0%                      | amortised into `parse_*` |

Once `decode_json_string_to_arena` wires into a new
`push_leaf_with_string`, twitter/data/data_xl throughput will drop
~15–35% as string decode becomes work on the bbnf side; citm and
canada will barely shift because they are structural / numeric
respectively. The post-landing ratios we should steer toward:

| dataset  | current ratio | after honest decode (est.) | sonic-side constant |
|----------|--------------:|---------------------------:|--------------------:|
| data     | 0.77          | ≈ 0.65                     | — |
| twitter  | 0.80          | ≈ 0.60                     | — |
| citm     | 0.85          | ≈ 0.75                     | — |
| canada   | 0.85          | ≈ 0.85                     | numeric, string-free |
| data_xl  | 0.82          | ≈ 0.68                     | — |

After that baseline is established, the remaining levers (below) aim
at closing the 15–35% honest gap back toward parity.

## Cross-bench themes

### What is firing

- **JSON scalar projection** — f64, bool, u8 all emit typed leaves; payload stored; readers work.
- **CSS whitespace+comment scanner** — `scan_ws_block_comments` is active at every whitespace site; SIMD inner loop short-circuits to a slow byte tail on dense inputs.
- **CSS named-color → u32** — `push_leaf_with_u32` fires for color keywords; one call site, dozens of color keyword Alt arms.
- **BBNF delimiter scan** — `memchr` on comment terminator, pointer-cast 2-byte literal matches on `"/*"` / `"*/"`.
- **Fractional-digit SIMD** — `number_simd::scan_digits_simd` is wired through `scan_number_mantissa` for the fractional part.

### What is not firing

- **JSON string decode** — every `scan_quoted_string_strict` discards the span; `decode_json_string_to_arena` exists but has zero call sites; `push_leaf_with_string` does not exist on `TapeBuilder`.
- **CSS number → f64 payload** — 20 `scan_number_f64(…).map(|_| ())` discards remain; the `number` rule in `grammar/css/l4/value-unit.bbnf` still lacks `-> f64`.
- **CSS hex → u32 payload** — `parse_hex_color` is declared but never called; HexConvert codegen does not reach the tape-first emitter.
- **CSS ident config routing** — 7 of 8 `scan_ident` sites still use `DEFAULT_IDENT_CONFIG`; the CSS-specific config is chosen only for `selectorIdent`.
- **BBNF token leaves** — every ident / string-literal / int-literal / regex rule emits `push_compound(Rule)` wrapping a Span rather than `push_leaf_with(...)`.
- **64-byte input padding** — `ParserState::new` does not pad the source, so every SIMD tail pays a bounds-check cost that sonic avoids via `PaddedSliceRead`.
- **Integer-digit SIMD** — deliberately scalar; the 8-digit SWAR chunker is the integer path by design (`number.rs:128-131`).

### Shared shape: compound churn, scanner inner loops, capacity mis-tunes

Every grammar's dominant self-time sits inside large generated rule
bodies (`__value`, `__compoundSelector`, `__declaration`,
`__unary_expr`, `__mapped_factor`) rather than at the tape or
scanner boundary. `TapeBuilder::push_compound` is ≤ 5% of leaf
samples on every bench we ran. The levers with the most reach are
therefore inside codegen (dispatch shape, optional-branch fast
paths, per-grammar capacity hints) and inside scanner inner loops
(SIMD short-circuit, padded-input prefetch), not inside the tape
builder.

## Optimisation proposals, ranked by cross-bench leverage

The ordering here is by *number of benches plus estimated impact on
the bbnf-vs-sonic ratio after Phase 3 lands*. Per-bench levers
follow.

1. **Wire `decode_json_string_to_arena` through a new
   `push_leaf_with_string`** (JSON, json_value). Removes the single
   largest semantic-parity omission. Activates honest comparison
   against sonic and replaces `scan_quoted_string_strict(…).map(|_|
   ())` at expand.rs:2508 with a direct-to-arena decode. Prerequisite
   to every further JSON perf claim that references sonic.

2. **Pad parser input to a 64-byte boundary in `ParserState::new`**
   (JSON, CSS, Sheets, BBNF — all). Unlocks unguarded SIMD tails
   in `scan_quoted_string_simd` (`quoted_simd.rs:31`),
   `scan_digits_simd`, `scan_ws_block_comments` SIMD inner loop,
   and every `u8x16` scan. Expected uniform +3–6% across every
   dataset; highest marginal yield on small inputs where the tail
   fraction is largest (data, parse_simple, json.bbnf). Sonic's
   `PaddedSliceRead` is in every sonic frame name for a reason.

3. **Activate `-> f64` on the CSS `number` rule** in
   `grammar/css/l4/value-unit.bbnf`. 20 call sites already run
   Eisel–Lemire and discard the result; activation is free. Closes
   the AU.2 hard gate (bootstrap ≥ 600 MB/s) by capturing value
   retention without adding scan work.

4. **Route HexConvert through the tape-first emitter**
   (`crates/core/src/backend/rust/emitter/map_value.rs::emit_hex_convert`).
   Wire the `hex` rule's `-> parse_hex_color(input) : u32` mapping
   into `push_leaf_with_u32`. Stops emitting `push_compound` for
   every hex color in every CSS file. AU.2.4 as originally scoped,
   now confirmed unlanded.

5. **Per-grammar tape capacity heuristic driven by the codegen
   fingerprint** (Sheets, JSON object-heavy, CSS compound-heavy).
   Replace the universal `input.len() / 2 + 2` with a grammar-
   specific divisor derived from the ratio of push_compound : bytes
   measured at codegen time — Sheets wants `len + 8`, CSS wants
   roughly `len / 6`, JSON wants `len / 8` for canada-shape and
   `len / 3` for twitter-shape. Eliminates the `RawVec::grow_one`
   / `_mi_heap_realloc_zero` tail which is 10–22% of parse_simple
   samples and up to 9% of json.bbnf inclusive.

6. **Flatten the Sheets precedence tower into a single Pratt-style
   loop** inside `__expr` (Sheets). Six dedicated rules each
   emitting unconditional `mark_children` + `push_compound(Repeat)`
   + `push_compound(Rule)` collapses into one rule that emits a
   compound only when the operator at that precedence level
   actually appears. Directly targets the 56–86% precedence-tower
   self-time band. Pattern generalises to any left-recursive
   operator chain.

7. **Optional-branch fast exit in `__mapped_factor`** (BBNF). Peek
   the next byte for `'-'` before entering the optional `->`
   block; skip all three `push_compound(Repeat)` emissions and
   both `trim_leading_whitespace_mut` calls on factors without
   maps. ≈ 40% of factor calls lack `->`; savings scale with the
   27.9–40.7% self-time `__mapped_factor` currently takes.

8. **Activate leaf payload on BBNF token rules** — `ident`,
   `string_lit`, `int_lit`, `regex`, `big_comment`. Today each
   emits `push_compound(Rule)` wrapping a span that consumers re-
   slice; lowering to `push_leaf_with_Span` / aggregate keeps the
   source range in the tape and removes the compound wrapper.
   Drops tape record count ~25% on ident-heavy grammars, cuts
   `__big_comment`'s 10–15% leaf-sample share on every BBNF
   entry.

9. **Fix ident-config routing for CSS** — 7 of 8 `scan_ident`
   sites use `DEFAULT_IDENT_CONFIG`. The shape-detection in
   `scanner_plan.rs` needs to recognise the CSS ident regex
   (leading `-`, double-dash prefix) and select `CSS_IDENT_CONFIG`.

10. **Eliminate remaining `.map(|_| ())` discards at codegen** —
    9 in JSON, 206 in CSS, 50 in Sheets, 106 in BBNF. Each is a
    computed-then-thrown value; projecting child `TapeOffset`
    upward removes the Option epilogue and shrinks hot-path
    icache. Enforces the `no-value-discard` invariant uniformly.

### Levers specific to JSON number-heavy workloads

- Widen the integer-mantissa fastpath from 18 to 19 digits
  (parse-that `number_f64.rs:14`) via a 128-bit accumulator.
  Canada's 11.5% `compute_f64` share drops to ~8% if more values
  take the integer path.
- Consider collapsing `__value → __array → push_compound` into a
  single inlined container loop, mirroring sonic's
  `parse_array::<DocumentVisitor>` which fuses dispatch and tape
  emit into one function (4–5% recoverable on canada/citm).

### Levers specific to rule-dispatch-heavy grammars

- **`__directive` keyword prefix dispatch** (BBNF). Replace the
  copy-paste 8-branch Alt with a single `scan_ident_at_at` that
  peeks `@` + keyword and selects the branch in O(1) before
  entering `mark_children`. 17% of json/css_pretty leaf samples
  currently sit in this dispatch.
- **Alt-level `mark_children` hoisting** — every Alt branch calls
  `mark_children` then backtracks `state.offset = __cp` on
  failure, leaving the mark. A single `mark_children` outside the
  branches serves every arm.

## Orchestrator friction fixes shipped with this wave

The wave-2 scaffold also cleared three operational blockers that
would otherwise have made the 27-entry matrix impractical:

1. `scripts/prepare-profile-wave.sh` now enumerates every (bench,
   entry) pair and writes 27 rows into `wave.tsv`, one port pair
   per bench (agents reuse their ports sequentially across
   entries). Prior version hard-coded one entry per bench.

2. `scripts/profile-bench-headless.sh` replaced the `rg` calls in
   `wait_for_record_artifacts`, `wait_for_load_ready`, and the
   final syms-proof extraction with `grep -E`. The ripgrep binary
   was not on bash's `PATH` in this environment (it existed only
   as a shell alias), so the wait loops silently timed out after
   60 seconds even when the profile artifacts were already on
   disk. The fix is a drop-in; every agent invocation in this
   wave used the patched script.

3. **Known remaining friction** — the bencher 0.1.5 `--bench`
   filter is a substring match (`test.desc.name.contains(...)`).
   Running `--bench data` against `json_monolithic` executes both
   `data` and `data_xl`; the `profile.json.gz` for the
   `data`/`bbnf_data`/`sonic_data` entries therefore contains
   samples from the `_xl` variant interleaved (which, because
   `data_xl` is ~1000× larger, dominates the profile to >99%).
   The `bench.txt` line matched against the entry name is still
   correct. Future waves should either rename the small-variant
   benches to break the prefix overlap (`data` → `data_s`) or
   switch to criterion; both are out of scope for wave 2.

## Artifacts cited

- `.profiles/samply/prebuild/wave.tsv` — 27 (bench, entry, ports,
  dirs) rows for this wave
- `.profiles/samply/prebuild/binaries.tsv` — one prebuilt release
  binary per bench, shared under `/.profiles/targets/au-wave/`
- `.profiles/samply/prebuild/expand/<bench>/expand.rs` — cargo
  expand artifacts for every bench
- `.profiles/samply/<bench>/<entry>/bench.txt` — throughput (2
  measurements for substring-overlapping entries, 1 otherwise)
- `.profiles/samply/<bench>/<entry>/profile.json.gz` — samply
  profile (contaminated for the three substring-overlap entries)
- `.profiles/samply/<bench>/<entry>/profile.json.syms.json` — all
  resolved frames with RVAs
- `.profiles/samply/<bench>/<entry>/syms-proof.txt` — named-frame
  grep match
- `crates/core/src/grammar/generated.rs` — the real BBNF parser
  source, lines 14143 / 15181 / 15407 / 15896 / 17569 for
  `__big_comment` / `__mapped_factor` / `__binary_factor` /
  `__rhs` / `__directive`
- `crates/core/benches/json/value.rs` — the json_value bench
  harness defining bbnf-vs-sonic measurement semantics
- `crates/bbnf-tape/src/builder.rs` — confirms `push_leaf_with_*`
  surface; no `push_leaf_with_string`
- `parse-that/rust/parse_that/src/parsers/scan/number.rs:93,193`
  — scan_number_mantissa and the fractional-digit SIMD gate
- `parse-that/rust/parse_that/src/parsers/scan/decode.rs:35` —
  `decode_json_string_to_arena`, declared, unused
