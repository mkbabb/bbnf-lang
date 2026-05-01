# AZ-III REAUDIT 2026-04-30 - Lane 2 - Future SOTA Targets

Read-only audit of the SOTA targets that AZ-III, BA, and BB must hit
before terminal close. Lane 1 owns cargo invocations; this lane reads
existing artifacts only and reports truth posture.

Master HEAD at audit: `d5179b8a` (`docs(az-iii.W0): add build and test
baseline to quarantine ledger`).

---

## 1. 17-entry matrix status

The 17-entry close matrix is the load-bearing measurement surface. Its
canonical archive is `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AZ-II.json`
(2026-04-28 23:22, commit `a29a1265`). The matrix is **PARTIAL** at
master. Per-entry posture below; cite paths are absolute.

### 1.1 Matrix entries and evidence status

| Bench / fixture | Status | mb_per_s | Evidence | Posture |
|---|---|---:|---|---|
| `json_monolithic::canada` | FRESH (cutover.H Phase 6, but wave-relative) | 551 | `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AZ-II.json` line 12; raw run at `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AY-AZ-II-close-json.txt` lines 304-310 | Captured under `[profile.bench]` in worktree `/private/tmp/bbnf-worktrees/cutover-H2` on 2026-04-28 13:32. Pre-O2/O3/O4. |
| `json_monolithic::citm` | FRESH | 1454 | post-AZ-II.json line 13 | Same provenance as canada. |
| `json_monolithic::data_s` | FRESH | 1507 | post-AZ-II.json line 14 | Same provenance. |
| `json_monolithic::data_xl` | FRESH | 741 | post-AZ-II.json line 15 | Same provenance. |
| `json_monolithic::twitter` | FRESH | 1407 | post-AZ-II.json line 16 | Same provenance. |
| `css_l4::bootstrap` | SIGABRT | n/a | post-AZ-II.json line 20 | Recursive-descent stack overflow under fat-LTO; pre-existing per W2-act.recovery. Bench harness lacks 64 MiB stack mitigation that the test path uses. |
| `css_l4::normalize` | NOT_MEASURED | n/a | post-AZ-II.json line 21 | Bench halted by bootstrap SIGABRT before normalize ran; sequential bench order blocks. |
| `css_l4::tailwind` | NOT_MEASURED | n/a | post-AZ-II.json line 22 | Same. |
| `google_sheets_monolithic::format_simple` | PLACEHOLDER (cutover.E) | 48.09 | post-AZ-II.json line 26 | Marked `cutover.E placeholder` in source archive; carried unchanged from AZ-I close. |
| `google_sheets_monolithic::format_stress` | PLACEHOLDER (cutover.E) | 50.82 | post-AZ-II.json line 27 | Same. |
| `google_sheets_monolithic::parse_simple` | SIGABRT | n/a | post-AZ-II.json line 30 | Sheets recursive-descent stack overflow under fat-LTO. |
| `google_sheets_monolithic::parse_nested` | SIGABRT | n/a | post-AZ-II.json line 31 | Same root cause. |
| `google_sheets_monolithic::parse_stress` | NOT_MEASURED | n/a | post-AZ-II.json line 32 | Halted before run by parse_simple/parse_nested SIGABRTs. |
| `bbnf_monolithic::bbnf_self` | PARSE_FAILED | n/a | post-AZ-II.json line 36 | `BbnfBootstrap::parse` fails on `bbnf.bbnf` source at offset 80 (Syntax error). Codegen-emitted parser does not yet self-host real BBNF input. The bench harness invokes `BbnfBootstrap::parse` directly and is not yet routed through `bbnf::grammar::parse`. |
| `bbnf_monolithic::css_l4_grammar` | PLACEHOLDER (cutover.E) | 111 | post-AZ-II.json line 37 | -77.6% vs AU. |
| `bbnf_monolithic::css_pretty` | PLACEHOLDER (cutover.E) | 147 | post-AZ-II.json line 38 | -77.3% vs AU. |
| `bbnf_monolithic::ebnf` | PLACEHOLDER (cutover.E) | 42 | post-AZ-II.json line 39 | -81.2% vs AU. |
| `bbnf_monolithic::google_sheets` | PLACEHOLDER (cutover.E) | 202 | post-AZ-II.json line 40 | -76.5% vs AU. |
| `bbnf_monolithic::json` | PLACEHOLDER (cutover.E) | 66 | post-AZ-II.json line 41 | -76.7% vs AU. |
| `compile_pipeline::compile_bbnf` | PLACEHOLDER (cutover.E) | n/a | post-AZ-II.json line 45 | ns_per_iter 2.732 ms; no AU baseline column. |
| `compile_pipeline::compile_css_l4` | PLACEHOLDER (cutover.E) | n/a | post-AZ-II.json line 46 | 26.21 ms. |
| `compile_pipeline::compile_ebnf` | PLACEHOLDER (cutover.E) | n/a | post-AZ-II.json line 47 | 625 us. |
| `compile_pipeline::compile_json` | PLACEHOLDER (cutover.E) | n/a | post-AZ-II.json line 48 | 218 us. |
| `compile_pipeline::compile_sheets` | PLACEHOLDER (cutover.E) | n/a | post-AZ-II.json line 49 | 11.42 ms. |

**Counts.** 5 fresh JSON (out of 17 close-matrix data-grammar rows), 11
placeholder, 5 SIGABRT/NOT_MEASURED, 1 PARSE_FAILED. The 5 fresh JSON
rows are themselves wave-relative (Stage A worktree, 2026-04-28 13:32),
not master-relative.

### 1.2 Critical: "fresh" JSON numbers are not master-relative

The `cutover.H Phase 6 fresh bench` JSON entries in post-AZ-II.json
were captured in worktree `/private/tmp/bbnf-worktrees/cutover-H2` -
NOT master. The provenance is in the raw bench output:
`/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AY-AZ-II-close-json.txt`
line 2 onward shows `Compiling csp-solver v0.1.0
(/private/tmp/bbnf-worktrees/cutover-H2/...)`. They reflect Stage A
state on a sibling worktree, before O2 (EBNF activation), O3 (generated
view purge), O4 (`Parsed<R>` / `TapeDirect` deletion), or O5 (tape
crate deletion) had landed on master. They cannot be cited as the
"current truth" of what master measures today (2026-04-30, commit
`d5179b8a`, post-O4 with active O5 regen drift).

A sibling capture at
`/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`
(2026-04-29 15:18, profile `profiling-prep`, ax-iter substrate) shows
canada at 219.3 ms median (`~10 MB/s`), data_xl exceeding the 1 s
wall-clock limit. That is a 50× divergence from the post-AZ-II.json
4.078 ms/551 MB/s claim. The two artifacts are not directly
comparable - profile differs, instrumentation differs - but the
delta confirms: until W4 measures `[profile.bench]` against master
HEAD, no JSON throughput number can be cited as binding.

### 1.3 Concrete CSS L4 bench surface

`/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/css/l4.rs`
(178 lines) does have CSS L4 bench targets, but the bench binary
SIGABRTs on bootstrap because the recursive-descent path overflows the
default thread stack under fat-LTO. The same parser inside the
`tests/css_l4` test path uses a 64 MiB spawned-thread mitigation that
the bench harness has not adopted. AZ-III.W4 must either harness this
mitigation or close the underlying recursion. This is a harness
defect, not a substrate defect; ignoring it leaves CSS L4 unmeasured
through the AZ runway.

---

## 2. Competitor parity

Five external-parity surfaces are in scope. Status posture per peer
follows. Path citations are absolute.

### 2.1 JSON peers

**Bench harness file**:
`/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/json/competitors.rs`
(515 lines, 8 peers x 5-6 fixtures = 48 functions per the doc).

| Peer | Source line citation (file above) | Crate version | Status |
|---|---|---|---|
| serde_json | line 27-46, `bench_serde!` | `crates/core/Cargo.toml` line 91: `serde_json = "1"` | Harness intact; numbers stale (no post-AZ-II refresh exists). |
| serde_json_borrow | line 49-72, `bench_serde_borrow!` | Cargo.toml line 95: `serde_json_borrow = "0.9"` | Same. |
| sonic-rs | line 73-94, `bench_sonic!` | Cargo.toml line 88: `sonic-rs = "0.5"` | Same. The AZ-III.W2.1 hard gate runs `cargo test -p bbnf sonic_rs_parity --profile ax-iter`; the corresponding parity test (`tests/sonic_rs_parity.rs`) currently has 2 failing (`sonic_rs_parity_twitter`, `sonic_rs_parity_data_xl`) per O3a-test-failures.txt lines 67, 84. |
| simd-json | line 95-120, `bench_simd!` | (uses `simd-json` crate; declared in Cargo.toml) | Same; harness uses `.to_vec()` per iteration (inherent library cost). |
| jiter | line 121-142, `bench_jiter!` | (declared) | Same. |
| nom | (further down, per benchmarks.md group `bench_nom`) | (declared) | Same; combinator tier. |
| winnow | (further down, group `bench_winnow`) | (declared) | Same. |
| pest | (further down, group `bench_pest`) | (declared) | Same. |
| **parse_that** | `/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/json/parse_that.rs` (54 lines) | `parse_that` workspace dep | Standalone bench. Combinator-class baseline. |
| **TS** | `/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/json/ts.rs` (59 lines) | (codegen-only, not a parser) | Measures grammar-to-TS source codegen throughput, not parse throughput. The TS native parse bench is at `crates/core/benches/ts/json_bench.mjs` (a Node script per the file's own doc comment). |

**JSON parity test gates** (`/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`):

- `sonic_rs_parity_twitter` - FAIL (line 67).
- `sonic_rs_parity_data_xl` - FAIL (line 84, 3.36 s timeout-class run).
- `json_value_parity` - 6 cases failing (simdjson_parity_*, json_parses_bools/nested_object) lines 23-27.
- `json_parity` - 4 cases failing (`bool_*_materialises_to_bool`, `every_declared_leaf_reaches_the_document`, `nested_object_preserves_typed_payloads`) lines 16-19.
- `json_canonical_parity::canonical_parity_twitter` - FAIL line 20.
- `json_parity_struct::native_parity_serde_twitter_json` - FAIL line 22.
- `json_parity_struct::native_parity_serde_canada_json` - FAIL line 29.

JSON sonic-rs parity is **NOT GREEN** at master. AZ-III.W2.1 hard gate
explicitly requires green sonic-rs parity (`cargo test -p bbnf
sonic_rs_parity --profile ax-iter`).

### 2.2 CSS peers

**Bench harness file**:
`/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/css/competitors.rs`
(202 lines).

| Peer | Source line citation | Status |
|---|---|---|
| cssparser (Mozilla/Servo) | competitors.rs line 19-127, `bench_cssparser!` macro at line 129; entries `cssparser_normalize`, `cssparser_bootstrap`, `cssparser_tailwind` | L0-L1 visitor / callback parser. Harness intact. |
| lightningcss (Parcel) | competitors.rs line 150-194, `lightningcss_normalize`/`bootstrap`/`tailwind` divan benches | L2 full semantic parse. The tailwind branch (line 184-187) skips on parse error. |

**CSS parity test gates** (per O3a-test-failures.txt):

- `lightningcss_parity::lightningcss_parity_bootstrap` - FAIL line 28.
- `lightningcss_parity::lightningcss_parity_tailwind` - FAIL line 43.
- `css_l4` - 7 cases failing (hex_color_*, parse_bootstrap_css, named_color_aliceblue_fires_inline_u32) lines 2-13.
- `css_l4_named_color_parity::white_materialises` - FAIL line 6.
- `css_l4_named_color_parity::every_named_color_materialises_its_u32_payload` - FAIL line 10.
- `css_l4_parity` - 8 cases failing (dir_pseudo_*, hex_color_*, selector_parses_without_payload_loss) lines 7-14.

CSS lightningcss parity is **NOT GREEN** at master. AZ-III.W2.2 hard
gate requires `cargo test -p bbnf lightningcss_parity --profile
ax-iter` green plus `css_l4_*` parity green. The "BEAT lightningcss in
every metric" directive (`feedback_beat-lightning`) cannot be cited
until parity itself closes; throughput exceedance over a parser the
project disagrees with semantically is meaningless.

### 2.3 Sheets peers

No external-parity peer is referenced in the project. Sheets is
measured against AU-baseline only. The Sheets parity surface is
self-parity (`sheets_parity`, `sheets_self_parity`) plus
serializer-roundtrip:

- `sheets_parity` - 12 failing cases (`error_literal_*_branch_fires_payload`, `boolean_first_branch_*`, `operator_branches_parse`, `range_ref_parses_*`, `unary_prefix_first_branch_fires_0u8`) lines 33-46.
- `sheets_self_parity` - 21 failing cases (corpus_*, serialize_roundtrip_*) lines 47-66.

Sheets is **NOT GREEN**. AZ-III.W2.3 hard gate requires Sheets parity
green.

### 2.4 BBNF peers

Self-parity only (no external peer; BBNF is bbnf's own grammar).
`bbnf_self_parity` reports 56/56 fixtures passing per AZ-II FINAL.md
line 38. The BBNF self-parse bench
(`bbnf_monolithic::bbnf_self`) PARSE_FAILED in post-AZ-II.json line
36 because `BbnfBootstrap::parse` does not self-host - the bench
harness routes through the codegen-emitted parser, not the canonical
`bootstrap_parser.rs`. AZ-III.W2.4 hard gate makes generated
self-host canonical or names `bootstrap_parser.rs` as a terminal
blocker.

### 2.5 No simdjson direct peer

The project documents simdjson as a "shape inversion" influence
(GESTALT.md line 786-793) but does not run a simdjson bench in
`crates/core/benches/json/competitors.rs`. simdjson OnDemand is
referenced in BA's lazy-path comparison (BA.md line 16, "the laziness
discipline is simdjson [OnDemand]") and as a parity target
(`tests/path_parity.rs` per BA.md line 376), but the live competitor
bench surface is the eight peers in §2.1.

---

## 3. Performance binding posture

### 3.1 GESTALT.md derived numbers

GESTALT.md cites the AU-baseline matrix at lines 982-990. Those numbers
are from `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AU.json`
(2026-04-15 09:17, commit `5281ec23` per
`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AU/FINAL.md`).

| Cell | Value | Binding posture |
|---|---:|---|
| JSON canada AU baseline | 1,231 MB/s | BINDING - this is the "recover or document why not" floor for AZ-I.W2 / AZ-II / AZ-III. |
| JSON citm AU baseline | 2,438 MB/s | BINDING (same) |
| JSON twitter AU baseline | 1,967 MB/s | BINDING (same; the load-bearing "twitter recovery" gate cited in B5/AY-II/AZ-I throughout) |
| CSS normalize AU baseline | 735 MB/s | BINDING |
| CSS bootstrap AU baseline | 454 MB/s | BINDING |
| CSS tailwind AU baseline | 496 MB/s | BINDING |
| Sheets parse_simple AU baseline | 95 MB/s | BINDING |

The AU-baseline column is BINDING because it is the historical anchor
the project committed to recovering. AZ-II.md hard gates (lines
173-181) re-state these as floors:

| AZ-II hard-gate floor | Value | Posture |
|---|---:|---|
| JSON canada | 1231 | BINDING (floor); 1500 target ASPIRATIONAL |
| JSON citm | 2438 | BINDING; 2700 ASPIRATIONAL |
| JSON twitter | 1967 | BINDING; 2200 ASPIRATIONAL |
| CSS normalize | 735 | BINDING; 850 ASPIRATIONAL |
| CSS bootstrap | 600 | BINDING (above AU; AZ tightens); 700 ASPIRATIONAL |
| CSS tailwind | 500 | BINDING; 600 ASPIRATIONAL |
| Sheets parse_simple | 95 | BINDING; 110 ASPIRATIONAL |
| BBNF self-parse | >= AU | BINDING (10% rule applies; not 20%) |

### 3.2 RISK-PERF-MATRIX.md derived numbers

`/Users/mkbabb/Programming/bbnf-lang/docs/RISK-PERF-MATRIX.md`
"Performance marks per grammar per juncture" (lines 414-477).

Notable cells:

- AZ-I.W2 close JSON twitter `≥ 1967 MB/s` (line 433): BINDING per
  AZ-I.md (the load-bearing AU recovery gate; never met at AZ-I close
  per post-AZ-I.json line 16, twitter delivered 1402 MB/s; routed to
  BB.close).
- `AZ-II.cutover.O FINAL JSON ≥ 1300 / 2500 / 2000` (line 434):
  ASPIRATIONAL - the matrix labels this "BEAT AU slightly". AZ-II is
  closed as continuation handoff; the AZ-II FINAL targets are not
  binding on AZ-III. The line 28 directive is explicit: "No
  optimization tranche may use stale AZ-I/AZ-II numbers as a baseline.
  The next publishable performance baseline is the post-`cutover.O`
  17-entry matrix." (post-AZ-III.json per AZ-III.W4 hard gate).
- `BA W1 (lazy 3-field) ≥ 3000 (citm) / ≥ 2400 (twitter)` (line 436):
  BINDING for BA per BA.md line 159 ("Lazy-path micro-bench suite
  beats sonic-rs `pointer!` by ≥ 20%"); cannot be measured before
  AZ-III closes.
- `AZ-I.W3 close CSS normalize ≥ 735 / bootstrap ≥ 600 / tailwind ≥
  496` (line 448): BINDING (AU recovery / lightningcss parity gate).
- `AZ-II.cutover.O FINAL CSS ≥ 800 / 650 / 550` (line 449):
  ASPIRATIONAL - "BEAT AU"; routed to AZ-III.W4 baseline.

### 3.3 "BEAT lightningcss in every metric"

The directive is project memory `feedback_beat-lightning`: BEAT
lightningcss in every metric, not just approach. It is BINDING REPO
POLICY. GESTALT.md line 1374 restates: "a direct-to-struct runtime
parser that beats lightningcss, sonic-rs, and simdjson OnDemand at
their own games — parity first, exceedance second, every `->`
reaching a struct field, one substrate". The phrasing is not soft; it
is the project's terminal target for the runway. RISK-PERF-MATRIX.md
line 28 anchors it: "stale AZ-I/AZ-II numbers" do not count as a
baseline for the BEAT directive. Any AZ-III close that does not
publish post-cutover lightningcss-vs-bbnf rows on `normalize` /
`bootstrap` / `tailwind` is incomplete.

### 3.4 STALE numbers cited as binding

| Number | Cited in | Source | STALE because |
|---|---|---|---|
| JSON twitter "688 MB/s ~35% of AU" | GESTALT.md line 992-993; RISK-PERF-MATRIX.md line 429 | AY-I.W1 column-revert | Post-AZ-II.json claims 1407 MB/s for twitter (a 2× fresh-bench discrepancy). The "688 MB/s" reading is from a different substrate / different commit. Both cannot be live. |
| AY-I value-path "538 / 2151 sonic-rs delta" | REMAINING-TRAJECTORY.md line 388 | AY-I Audit D | The note itself flags "refresh in B1" / "sonic figure ... comes from AY-I Audit D". That refresh has not occurred. |
| BBNF self-parse 87 MB/s vs AU 394 MB/s = -77.9% | post-AZ-I.json line 36, mirrored to AZ-II FINAL.md table at lines 96-104 | AZ-I close worktree | The post-AZ-II "fresh bench" did not refresh BBNF self-parse (PARSE_FAILED). The -77.9% remains the live published delta but is wave-relative. |

---

## 4. Stale-evidence index

### 4.1 Files >30 days old still cited as truth

- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AU.json`
  (2026-04-15) - 15 days old. Cited as the AU-baseline anchor
  throughout. STILL VALID as historical anchor; the AU-baseline does
  not get re-measured.
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AZ-I.json`
  (2026-04-28 12:15) - 2 days old. Cited as wave-prior comparison in
  post-AZ-II.json (`az_i_close_mb_per_s` column). NOT STALE per age
  but its CSS / Sheets / BBNF entries are themselves SIGABRT or
  worktree-local placeholders.
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-AZ-II.json`
  (2026-04-28 23:22) - 2 days old. **Mixed**: 5 JSON entries fresh
  (worktree, pre-O2/O3/O4), 11 entries placeholder, 5 entries
  SIGABRT/missing.

### 4.2 Placeholder values being treated as binding

post-AZ-II.json contains 11 `cutover.E placeholder` rows (Sheets
format, BBNF compile pipeline x6, all five compile_pipeline rows).
These rows have been on master at `cutover.E` substrate state since
2026-04-28; subsequent O2/O3/O4 commits substantially changed BBNF
emission and `Parsed<R>` deletion. The rows were not refreshed.

If any downstream consumer reads post-AZ-II.json and treats the
`bbnf_monolithic::*` -76% to -81% deltas as the live JSON / CSS /
EBNF / Sheets compile-grammar performance posture, they would be
reading a stale wave-prior state. BB cost-model rule inference must
NOT consume these rows; AZ-III.W4 hard-gate item 4 explicitly forbids
`post-AZ-II` references in `post-AZ-III.json`.

### 4.3 Worktree-not-master bench captures

| File | Worktree path | Master? |
|---|---|---|
| `docs/benchmarks/post-AY-AZ-II-close-json.txt` | `/private/tmp/bbnf-worktrees/cutover-H2` | No |
| `docs/benchmarks/post-AY-AZ-II-close-bbnf.txt` | `/private/tmp/bbnf-worktrees/cutover-C` | No |
| `docs/benchmarks/post-AY-AZ-II-close-sheets.txt` | `/private/tmp/bbnf-worktrees/cutover-C` | No |
| `docs/benchmarks/post-AY-AZ-II-close-css.txt` | `/private/tmp/bbnf-worktrees/cutover-H2` | No |
| `docs/benchmarks/post-AY-AZ-II-close-compile.txt` | (worktree-class capture) | No |

Five out of five close-matrix raw-output files were captured on
sibling worktrees, not master. The post-AZ-II.json that cites them as
"cutover.H Phase 6 fresh bench" is therefore citing wave-relative
state, not master-relative state.

### 4.4 The 4-day live matrix mtime

Master has accumulated 40+ commits since post-AZ-II.json was authored
(2026-04-28 23:22). Per `git log --since=2026-04-28 --until=2026-04-30`
output, those commits include cutover.O2 (EBNF activation), cutover.O3
(generated view purge), cutover.O4 (Parsed/TapeDirect deletion), and
~30 cleanup / refactor commits. The matrix is NOT 30 days old, but it
is wave-stale: every named O substage that landed after 2026-04-28
23:22 invalidates one or more rows.

### 4.5 AZ-III.PROGRESS / FINAL audit notes

`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/PROGRESS.md`
line 19 says: "no refreshed green O5 close packet exists" and line 27
notes "Root tests, root clippy, parse-that tests/clippy, and pprint
clippy are red". The W0 ledger
(`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/audit/W0-state-ledger.txt`)
is the current truth-of-state for AZ-III opening; bench truth is W4.

---

## 5. AZ-III W4 obligation - what MUST happen before BA / BB

Per AZ-III.W4
(`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/waves/W4.md`)
hard gates 1-5, the following refreshes are **mandatory** before BA
or BB.close opens:

### 5.1 Workspace truth (W4.1)

- `cargo fmt --all -- --check` archived (W4 hard gate 1).
- `cargo clippy --workspace --all-targets --profile ax-iter` archived.
- Workspace test command archived with pass/fail counts.
- Outputs to `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/AZ-III/W4-workspace.txt`.

### 5.2 Structural audits (W4.1)

- payload coverage,
- StructRegistry completeness,
- IR coverage,
- `cargo xtask regen --check` (CURRENTLY FAILS per O5 scan; 9 of 9
  grammars drift),
- no-legacy / no-tape-residue grep scans,
- archived to `docs/benchmarks/AZ-III/W4-structural-audits.txt`.

### 5.3 Bench harness preflight (W4.2)

- All 17-entry matrix binaries must compile under `[profile.bench]`.
- `crates/core/benches/css/l4.rs` and
  `crates/core/benches/google_sheets/monolithic.rs` SIGABRT
  remediation (64 MiB stack threading or recursion-depth cap or
  iterative re-shape).
- `crates/core/benches/bbnf/monolithic.rs::bbnf_self` must route
  through `bbnf::grammar::parse` (i.e., `bootstrap_parser.rs`) instead
  of the codegen-emitted `BbnfBootstrap::parse`, OR the codegen-emitted
  parser must self-host (W2.4 disposition decides).

### 5.4 Serialized measurement (W4.3)

- 17-entry matrix executed sequentially (`feedback_bench-sequential-regression`,
  `feedback_no-warm-benches`, `feedback_iter-profile-always`,
  `feedback_single-cargo-per-target` all in force).
- No placeholder / NOT_MEASURED / SIGABRT row may remain.
- W4 gate 4: `rg -n "NOT_MEASURED|placeholder|post-AZ-II|TBD" docs/benchmarks/post-AZ-III.json` returns zero hits.

### 5.5 Profile truth (W4.4)

- samply captures for every regression vs AU-baseline.
- Stored at `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/profiles/AZ-III/`.
- `feedback_samply-symbol-resolution`: needs `debug = true` and
  interactive `samply record` (not `--save-only`).
- `feedback_actual-profiling`: run actual profiler; do not guess from
  static analysis.

### 5.6 Competitor parity refresh (W2 prerequisite, W4 measurement)

- JSON sonic-rs parity green (W2.1) AND sonic-rs throughput
  comparison row in post-AZ-III.json on `canada/citm/twitter/data_xl`.
- CSS lightningcss parity green (W2.2) AND lightningcss throughput
  comparison row on `normalize/bootstrap/tailwind`. The
  feedback_beat-lightning directive demands these rows show bbnf
  exceeding lightningcss; until parity closes, the BEAT comparison is
  premature.
- Sheets parity green (W2.3); no external peer.
- BBNF self-host canonical (W2.4) OR named blocker.

### 5.7 Compile pipeline rebench

The five `compile_pipeline::compile_*` rows must be re-measured. They
have not been refreshed since cutover.E (2026-04-28). The B5 close
report cites `compile_bbnf` 2.806 ms median (REMAINING-TRAJECTORY.md
line 200) which is post-B5 substrate-true; AZ-III.W4 must verify that
hold or document the cumulative regression from B5 substrate through
O0-O4.

### 5.8 Discipline notes (project memory)

- `feedback_no-warm-benches`: cold per-parse only (mimalloc, divan
  sample_size = 1 x 100 samples).
- `feedback_bench-sequential-regression`: never interleaved.
- `feedback_iter-profile-always`: every iteration-loop cargo bench
  carries `--profile ax-iter` explicitly.
- `feedback_bench-single-run`: no separate sequential cargo
  invocations.
- `feedback_accurate-perf-narrative`: post-AZ-III.json must
  reconstruct actual timeline from commits; no fabricated rows.

---

## 6. B0..B7 roadmap and SOTA-target references

B0 / B1 / B2 / B3 / B4 / B5 / B6 / B7 are all CLOSED (2026-04-24
through 2026-04-27 per
`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/REMAINING-TRAJECTORY.md`
lines 154-159). They are infrastructure tranches:

| Tranche | Headline (per `<tranche>/B<n>.md` opener / FINAL.md) | Perf gate? |
|---|---|---|
| B0 | "AY Execution Runway" - bounded prelude annex | None; structural prelude. |
| B1 | "dev-loop truth + proof-surface hardening" - rust-toolchain pin, divan port, nextest, scripts | Records post-B1 17-entry matrix (W3 close obligation per RISK-PERF-MATRIX.md line 138) but no perf gate. |
| B2 | build-time codegen transposition; `bbnf_derive` proc-macro retired | Cold-regen wall reduction (12:43 vs 80 min); no runtime perf gate. |
| B3 | parser-baseline restoration | `compile_bbnf` 2.831 ms median (single bench gate). |
| B4 | codegen `syn::parse2` emit-correctness | Workspace clean; no runtime perf gate. |
| B5 | substrate restoration | `compile_bbnf` 2.806 ms median (0.9% under B4). Workspace nextest 1477/1477. |
| B6 | Dev-Loop Expedite (W0 mtime cycle 192x speedup) | Build-cycle perf only. |
| B7 | Cross-Repo Modernization Annex | Infrastructure-only; cross-repo divan/nextest unification. |

B0-B7 do not carry the SOTA gates. They are predecessor tranches that
make AZ-III's measurement surface trustworthy. Their JSON archives
(`post-B0.json`, `post-B1.json`, ..., `post-B7-W0-walls.txt`) record
build-cycle and compile-pipeline numbers, not 17-entry parser
throughput.

### 6.1 Tranches that reference unrefreshed numbers

- BA (`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/BA.md`):
  cites sonic-rs `pointer!` parity at line 16, "≥ 20% win over
  sonic-rs on 3-field citm.json extraction" at line 431, AU-baseline
  twitter target at REMAINING-TRAJECTORY.md line 393. **All three
  reference numbers that AZ-III.W4 must publish before BA can act.**
  BA is BLOCKED on AZ-III.W5 close per
  `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/AZ-III.md`
  line 35: "BA and BB remain blocked until AZ-III publishes terminal
  evidence."
- BB (`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/BB.md`):
  cites lightningcss / sonic-rs / simdjson parity harnesses at line
  382 as a precondition. BB is BLOCKED on AZ-III + AY-II close per
  REMAINING-TRAJECTORY.md line 51 dependency graph.
- BC: separate plan exists per `docs/tranches/BC/`; not in this audit's
  scope.

### 6.2 Chronically deferred items

| Item | Origin | Current location | AZ-III routing |
|---|---|---|---|
| 17-entry close-matrix bench refresh | AZ-II cutover.H Phase 6 | post-AZ-II.json placeholder | AZ-III.W4 |
| JSON sonic-rs parity refresh | AZ-II.O6 | not run | AZ-III.W2.1 |
| CSS lightningcss parity refresh | AZ-II.O6 | not run | AZ-III.W2.2 |
| Sheets parity green | pre-existing | 33 failing in O3a | AZ-III.W2.3 |
| BBNF generated self-host | AZ-II cutover.G/H/I | bridged by `bootstrap_parser.rs` | AZ-III.W2.4 |
| `crates/tape/` no-default-features green | AZ-II cutover.O5 | FAIL: missing `crates/core/src/lower/tape_walk.rs` | AZ-III.W1 |
| `cargo xtask regen --check` clean | AZ-II cutover.O5 | FAIL: 9/9 grammars drift | AZ-III.W1 |
| Profile truth (samply) | not enforced since AY-II | absent | AZ-III.W4.4 |
| CSS / Sheets bench SIGABRT remediation | pre-existing | absent | AZ-III.W4.2 |
| Compile-pipeline rebench (5 rows) | cutover.E placeholder | post-AZ-II.json | AZ-III.W4.3 |
| `iai-callgrind` instruction-count CI | B1 (per `iai-baselines/`) | unclear if active | AZ-III.W4 |

---

## 7. One-page SOTA target sheet

Each grammar x fixture row anchors AU baseline, the JSON-fresh-only
AZ-II close cell, the AZ-III target, and the binding flag. The "AZ-II
close" column is the post-AZ-II.json published value (note: 5 JSON
fresh, others placeholder/missing). The competitor column is the
external SOTA peer for that grammar. AU baseline values per
post-AU.json + GESTALT.md / RISK-PERF-MATRIX.md.

| Grammar | Fixture | Competitor | AU baseline | AZ-I close (per post-AZ-I.json) | AZ-II close (per post-AZ-II.json) | AZ-III target | Binding? | Notes |
|---|---|---|---:|---:|---:|---:|---|---|
| JSON | canada | sonic-rs | 1231 MB/s | 547 | 551 (fresh*) | >= 1231 floor; >= 1300 BEAT-AU; sonic-rs+ for BEAT | BINDING (AU floor); ASPIRATIONAL (BEAT) | *worktree-relative, not master |
| JSON | citm | sonic-rs | 2438 | 1476 | 1454 (fresh*) | >= 2438 floor; >= 2500 BEAT | BINDING / ASPIRATIONAL | * |
| JSON | twitter | sonic-rs | 1967 | 1402 | 1407 (fresh*) | >= 1967 floor; >= 2000 BEAT; >= 2400 BA lazy | BINDING (AU floor; load-bearing); ASPIRATIONAL (BEAT, BA) | The "twitter recovery" gate. * |
| JSON | data_s | sonic-rs | 1746 | 1503 | 1507 (fresh*) | >= AU floor | BINDING | * |
| JSON | data_xl | sonic-rs | 1179 | 747 | 741 (fresh*) | >= AU floor | BINDING | * (note: profiling-prep capture timed out at >2.5s for data_xl) |
| CSS L4 | normalize | lightningcss | 735 | NOT_MEASURED (CSS bootstrap SIGABRT before run) | NOT_MEASURED | >= 735 floor; >= 800 BEAT-AU; lightningcss+ on every metric | BINDING (AU floor; lightningcss BEAT directive) | feedback_beat-lightning |
| CSS L4 | bootstrap | lightningcss | 454 | SIGABRT | SIGABRT | >= 600 floor (AZ tightens above AU); >= 650 BEAT | BINDING | Bench SIGABRT; W4.2 must mitigate |
| CSS L4 | tailwind | lightningcss | 496 | NOT_MEASURED | NOT_MEASURED | >= 500 floor; >= 550 BEAT | BINDING | * |
| Sheets | parse_simple | (none) | 95 | SIGABRT | SIGABRT | >= 95 floor; >= 110 BEAT | BINDING | Bench SIGABRT; W4.2 must mitigate |
| Sheets | parse_nested | (none) | (not in 17-entry) | SIGABRT | SIGABRT | post-AZ-III only | ASPIRATIONAL | Not in original AU-matrix |
| Sheets | parse_stress | (none) | (not in 17-entry) | NOT_MEASURED | NOT_MEASURED | post-AZ-III only | ASPIRATIONAL | Not in original AU-matrix |
| Sheets | format_simple | (none) | 42 | 48.09 (PLACEHOLDER) | 48.09 (PLACEHOLDER) | refresh under [profile.bench] | BINDING | Cutover.E placeholder; not refreshed |
| Sheets | format_stress | (none) | 52 | 49.23 (PLACEHOLDER) | 50.82 (PLACEHOLDER) | refresh | BINDING | Same |
| BBNF | self-parse | (self) | 394 (build-time) | 87 | PARSE_FAILED | >= 394 - 10% per W2.4 disposition (10% rule applies) | BINDING-WITH-RELAXED-RULE | feedback_no-warm-benches still applies; 10% rule per AZ-II.md line 419 |
| BBNF | css_l4_grammar | (self) | 496 | 111 | 111 (PLACEHOLDER) | refresh; >= AU per parity | BINDING | Compile-grammar throughput |
| BBNF | css_pretty | (self) | 647 | 147 | 147 (PLACEHOLDER) | refresh | BINDING | Same |
| BBNF | ebnf | (self) | 223 | 42 | 42 (PLACEHOLDER) | refresh post-O2 EBNF activation | BINDING; substrate change post-cutover.E | EBNF flipped to StructDirect at O2 |
| BBNF | google_sheets | (self) | 858 | 202 | 202 (PLACEHOLDER) | refresh | BINDING | |
| BBNF | json | (self) | 283 | 66 | 66 (PLACEHOLDER) | refresh | BINDING | |
| BBNF | (5 compile_*) | (self) | (not in AU) | per AZ-I close | PLACEHOLDER | refresh; B5 baseline 2.806 ms compile_bbnf is the comparison anchor | BINDING (cumulative regression watch) | B5 baseline is AZ-III.W4 reference |
| EBNF (newly StructDirect) | self-parse | (none) | 223 (BBNF row) | 42 | 42 (PLACEHOLDER) | post-O2 substrate change is the load-bearing measurement | BINDING (substrate verification) | EBNF activated at O2; first measurement gates the activation |

`*` = fresh number is wave-relative (sibling worktree), not master.

### 7.1 Aggregate posture

- 7 BINDING AU-floor cells (3 JSON canada/citm/twitter, 3 CSS, 1 Sheets parse_simple).
- 3 BINDING aspirational BEAT cells (lightningcss + sonic-rs + simdjson).
- 1 BINDING-with-relaxed-rule cell (BBNF self-parse, 10% rule).
- 11 PLACEHOLDER cells requiring refresh.
- 5 SIGABRT/NOT_MEASURED cells requiring harness mitigation OR
  underlying recursion fix.
- 1 PARSE_FAILED cell (BBNF self-parse via codegen-emitted parser).

### 7.2 BEAT directive enforcement

`feedback_beat-lightning`: the project has committed to BEATING
lightningcss on every metric (not approaching). Until AZ-III.W2.2
closes lightningcss parity AND AZ-III.W4.3 publishes
lightningcss-vs-bbnf rows for normalize / bootstrap / tailwind, the
BEAT directive is unverified. BB cost-model rule inference cannot
operate against unverified ground truth.

GESTALT.md line 1374-1376 states the runway's terminal target as a
parser that "beats lightningcss, sonic-rs, and simdjson OnDemand at
their own games — parity first, exceedance second". The order is
binding: AZ-III.W2 closes parity; AZ-III.W4 measures exceedance; BA /
BB act only after.

---

## 8. Master HEAD live-state caveats

- `cargo xtask regen --check` is RED at master per O5 scan: 9/9
  grammars drift. Until W1 closes, every generated-vs-source claim is
  in dispute.
- `cargo build -p bbnf --no-default-features --profile ax-iter` is RED
  per O5 scan: missing `crates/core/src/lower/tape_walk.rs` while
  `lower/mod.rs` line 18 still declares `mod tape_walk`. Hard gate 2
  of AZ-III is not green.
- 84 nextest failures + 25 skipped per O3a-test-failures.txt summary
  line 85 ("1645 tests run: 1561 passed, 84 failed, 25 skipped").
- AZ-III.W0 is in_progress per
  `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/PROGRESS.md`
  line 27; main worktree is dirty with restored AZ-II implementation
  slice; commit history was rewritten message-only with backup branch
  `codex/az-history-before-reword-20260430-114057`.

---

## 9. Conclusion - SOTA truth posture

**The 17-entry matrix is wave-stale.** Five JSON rows are fresh but
captured on a sibling worktree pre-O2/O3/O4; eleven rows are
cutover.E placeholders unchanged across O0-O4 substrate; five rows
are SIGABRT or PARSE_FAILED. There is no master-relative,
post-master-cutover bench truth at HEAD.

**Competitor parity is RED.** sonic-rs (2 fail), lightningcss (2
fail), Sheets (33 fail), BBNF self-host (codegen-emitted parser does
not self-host) are blocked. The "BEAT lightningcss in every metric"
directive cannot fire until parity closes (AZ-III.W2) and exceedance
is measured (AZ-III.W4).

**Performance binding posture.** AU-baseline cells are BINDING (the
recovery floor). AZ-II FINAL targets are ASPIRATIONAL and explicitly
deferred to AZ-III per RISK-PERF-MATRIX.md line 28. AZ-III.W4 hard
gate 4 explicitly forbids carrying `post-AZ-II` references as truth.

**Stale-evidence priority risk.** The greatest risk is post-AZ-II.json
being read as binding by BB cost-model work. AZ-III.W4 must publish
post-AZ-III.json before BB.close acts; otherwise BB consumes
wave-relative + worktree-relative + cutover.E placeholder data and
its inferred-rule cost model anchors against fiction.

**Recommended W4 sequence**:
1. Workspace gates (W4.1) - confirm regen --check green, no-default
   build green.
2. Bench harness preflight (W4.2) - mitigate CSS L4 and Sheets
   SIGABRTs; route BBNF self-parse bench through `bootstrap_parser`
   or canonicalize generated parser per W2.4.
3. Serialized 17-entry measurement (W4.3) - all rows compiled,
   measured, no placeholder allowed.
4. Profile truth (W4.4) - samply for every regression vs AU.
5. Competitor parity rows added: sonic-rs (canada, citm, twitter,
   data_xl), lightningcss (normalize, bootstrap, tailwind).

**The BEAT directive is binding repo policy** and is non-negotiable
per `feedback_beat-lightning`. AZ-III close is incomplete until BEAT
is verified or the gap is documented with profiler-evidence root
cause.
