# R2 — Benchmark & profiling harness map (SK-V24)

Recon cartography. Every path verified via `ls`/`rg` at HEAD `7f3be27c2`. Paths
are repo-absolute-relative from `/Users/mkbabb/Programming/bbnf-lang`. Read-only
survey; no build was run.

---

## 0. The one blocking gotcha — READ FIRST

**`json_canon_bench.rs` is NOT in the git-tracked tree.** It is the cold JSON
graduation instrument the sk-v22 baseline certifies, but it exists ONLY inside a
detached git worktree:

- Present: `.claude/worktrees/wf_e940452a-431-7/skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs` (298 lines, worktree at commit `d2b32f6e3`)
- Absent at HEAD: `git ls-files | grep canon_bench` returns only `css_canon_bench.rs`. `git log --all -- .../json_canon_bench.rs` is empty — it was never committed.

To run any JSON cold/PMU/paired bench, an implementer MUST first copy that file
into `skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs`. It compiles against
HEAD's runtime unchanged: it calls `runtime::generated_json::{parse, parse_only}`
and `bbnf_bench::direct_struct::{track1_digest, sonic_digest, serde_digest}` plus
`bbnf_bench::real_typed_struct::locate_fixture`, all of which still exist at HEAD
(verified §3, §4). No `[[bin]]` stanza is needed — `src/bin/*.rs` autodiscovers
(the crate's only explicit `[[bin]]` is `css_track1_profile`; `css_canon_bench`
also relies on autodiscovery, see `skinny/crates/bbnf-bench/Cargo.toml:28-30`).

---

## 1. The two harness families

The crate `bbnf-bench` (`skinny/crates/bbnf-bench`) holds two disjoint families:

**A. Cold canon-bench binaries — the graduation instrument (system allocator).**
This is what sk-v22 §04-graduation-baseline certifies. Cold-per-parse N=200 +
PMU cyc/byte + paired interleaving. NO `#[global_allocator]` in these bins →
system allocator (faithful to PROTO-A1; deliberately NOT mimalloc).
- `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` (416 lines, git-tracked) — CSS vs lightningcss/cssparser.
- `json_canon_bench.rs` (worktree-only, §0) — JSON `parse_tape` vs sonic-rs/serde.

**B. Criterion WARM benches (git-tracked, mimalloc global allocator).** These are
the parity oracles + warm throughput, NOT the cold truth-floor. Per MEMORY
`no-warm-benches`, warm/criterion numbers are disingenuous for the >SOTA claim —
use family A for any certified figure. Family B is where the parity assertions
(`assert_direct_struct_parity`, `assert_parity`) live and is useful to prove
correctness of any new arm before timing it.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` (`harness = false`, criterion; `#[global_allocator] mimalloc` at line 8-9). Lanes: `parse_only`, sonic_rs_skipper (`json_parity.rs:97`), sonic_rs_lossy (`:113`), sonic_rs_direct_to_struct (`:274`/`:285`), sonic_rs_real_typed_struct (`:391`), serde. Declared `skinny/crates/bbnf-bench/Cargo.toml:35-37`.
- `skinny/crates/bbnf-bench/benches/simd_scan.rs` (`harness = false`, declared `Cargo.toml:39-41`).

---

## 2. Datasets — exact file locations (all verified present)

**JSON** — canon bench resolves via `real_typed_struct::locate_fixture(name)`
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:1372`), which walks
`CARGO_MANIFEST_DIR` ancestors trying `crates/test-fixtures/corpus/json/{name}.json`
then `test_data/{name}.json`. `CORPORA = ["twitter","canada","citm_catalog"]`
(`json_canon_bench.rs:116`). Files (both loci exist; corpus/json is primary):
- `skinny/crates/test-fixtures/corpus/json/twitter.json`
- `skinny/crates/test-fixtures/corpus/json/canada.json`
- `skinny/crates/test-fixtures/corpus/json/citm_catalog.json`
- (mirror) `skinny/test_data/{twitter,canada,citm_catalog}.json`
- Manifest w/ sha256+sizes: `skinny/crates/test-fixtures/corpus/json/manifest.toml` (twitter 631515 B, citm 1727204 B, canada 2251051 B).

**CSS** — `css_l4_corpus::load_all()` reads from
`corpus_dir() = CARGO_MANIFEST_DIR/../../corpora/css-l4-sk-v14`
(`skinny/crates/bbnf-bench/src/css_l4_corpus.rs:56-58, load_one :73-74`). Specs
in `css_l4_corpus.rs:22-54`. Files verified in `skinny/corpora/css-l4-sk-v14/`:
- `bootstrap-5.3.3.min.css` (232803 B) · `tailwindcss-0.2.0.min.css` (179631 B) · `material-components-web-14.0.0.min.css` (495454 B) · `animate-4.1.1.min.css` (71750 B)
- Hashes/pins: `skinny/corpora/css-l4-sk-v14/manifest.md`.

---

## 3. How the metrics are captured

**PMU cycles/byte + instructions/byte** — via macOS `proc_pid_rusage` with
`RUSAGE_INFO_V5`, reading `ri_cycles` and `ri_instructions` deltas around a
tight `iters`-count loop. Identical struct + FFI in both bins:
- CSS: `css_canon_bench.rs:44-102` (struct `RusageInfoV5`, `const RUSAGE_INFO_V5=5`, `extern "C" fn proc_pid_rusage`, `read_rusage_v5()`), consumed in the PMU block `css_canon_bench.rs:224-260` (cyc = `ri1.ri_cycles - ri0.ri_cycles`, `cycles_per_byte = cyc / (bytes*iters)`).
- JSON: `json_canon_bench.rs:24-83` (byte-identical struct/FFI) consumed at `json_canon_bench.rs:190-221`.
- `ri_cycles`/`ri_instructions` are per-process counters → load-robust; the
  baseline treats cyc/byte + ins/byte as the CERTIFIED columns.

**Cold-per-parse N=200 (wall)** — one out-of-band touch to page in the source,
then N timed single parses with fresh state, output `black_box`-dropped:
- JSON `sample()` `json_canon_bench.rs:140-175` — records `secs*1e9/nbytes` per sample; reports median/min ns/byte, median/max Mbps (`8000/ns_per_byte`), stddev. Default N=200, hard-gated `N>=50` (`:266-267`).
- CSS `sample()` `css_canon_bench.rs:159-190` — same discipline, reports Mbps median/min/max/stddev; gate `N>=50` at `css_canon_bench.rs:263`.
- The single-touch-then-loop pattern: JSON `:143-150`, CSS `:165-172`. Timed region is exactly one `parse(...)` call, black-boxed. This is the `no-warm-benches` contract.

**Paired per-sample interleaving (load-CERTIFIED)** — JSON ONLY, `paired` mode,
`json_canon_bench.rs:226-262`. Per corpus, per sample, it times the four lanes
`parse_tape`, `fused`, `sonic`, `serde` back-to-back inside ONE loop iteration
(`:242-255`), pushing each lane's `ns/byte` into parallel vectors. Then
`emit_paired()` (`:289-298`) computes the per-sample ratio `A_ns/B_ns`, its mean,
and a 95% CI (normal approx, `mean ± 1.96·sqrt(var/n)`, sample var with `n-1`).
Ratios emitted: `tape/sonic`, `fused/sonic`, `tape/serde`, `fused/serde`
(`:257-260`). Ratio < 1 ⇒ skinny faster; load hits both sides of each ratio in
the same sample, nulling drift. (CSS has NO paired mode.)

`loadavg()` is stamped into every JSON header via `sysctl -n vm.loadavg`
(`json_canon_bench.rs:177-183`). `mach_absolute_time` is NOT used — timing is
`std::time::Instant`; the load-robust axis is the rusage cycle counter.

---

## 4. The lanes / arms (what each measures)

**JSON** `WORKLOADS` const `json_canon_bench.rs:109-115`, each `fn(&str,&[u8])->u64`:
- `parse_only` (`:87`) → `runtime::generated_json::parse_only(input)` — structural validation, no tape. HEAD def: `runtime/src/grammars/json/generated.rs:395`.
- `parse_tape` (`:91`) → `runtime::generated_json::parse(input)` — THE live lazy-view product (builds offset tape → `JsonRoot`). HEAD def: `runtime/src/grammars/json/parser.rs:47`, re-exported `runtime/src/grammars/json/mod.rs:12`. (`generated_json` = `grammars/json/mod.rs` via `runtime/src/lib.rs:14` `#[path]`; aliased `json` at `lib.rs:28`.)
- `fused` (`:96`) → `bbnf_bench::direct_struct::track1_digest(input)` — `parse_direct` sink + eager digest. Def `skinny/crates/bbnf-bench/src/direct_struct.rs:421` (calls `runtime::generated_json::parse_direct`, HEAD `generated.rs:521`).
- `sonic` (`:100`) → `direct_struct::sonic_digest(bytes)` — sonic-rs 0.5.8 `from_slice`. Def `direct_struct.rs:436`.
- `serde` (`:104`) → `direct_struct::serde_digest(bytes)` — serde_json `from_slice`. Def `direct_struct.rs:432`.
- Digest struct `JsonDirectDigest` fields (objects/arrays/strings/string_bytes/numbers/fingerprint) `direct_struct.rs:16-28`; lanes XOR-fold them to a `u64` sink.

**CSS** `WORKLOADS` const `css_canon_bench.rs:136-141`, each `fn(&str)->u64`:
- `track1_full_parse` (`:106`) → `css_decl::parser::parse_full` (HEAD `runtime/src/grammars/css_l4_declaration_values/parser.rs:41`).
- `track1_rich` (`:111`) → `css_decl::parser::rich_summary` (HEAD `parser.rs:33`) — the eager rich typed CSSOM, the >SOTA product vs lightningcss.
- `lightningcss` (`:126`) → `StyleSheet::parse` full-CSSOM (lightningcss `=1.0.0-alpha.71`, `Cargo.toml:16`).
- `cssparser` (`:131`) → token-scan probe `cssparser_full_parse` (`:295`, impls at `:307-416`).

---

## 5. EXACT commands (build + run)

Workspace root is `skinny/` (holds the release profile). Run all cargo from
there. Binaries land in `skinny/target/release/`. NOTE the root
`.cargo/config.toml` (rustc-wrapper=sccache, `[patch.crates-io]`) applies via
upward search; the release profile is `skinny/Cargo.toml:78-85` (`lto="fat"`,
`codegen-units=1`, `debug=true`, `strip=false`, `split-debuginfo="packed"`,
`panic="abort"`). Use `--release` (NOT `--profile ax-iter` — cold discipline
requires fat-LTO).

**(pre-req) Restore the JSON bin (§0):**
```
cp .claude/worktrees/wf_e940452a-431-7/skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs \
   skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs
```

**(a) Build both cold bins:**
```
cd skinny && cargo build --release -p bbnf-bench --bin json_canon_bench --bin css_canon_bench
```

**(b1) One paired JSON bench (the load-certified ratios):**
```
./target/release/json_canon_bench paired 200
```
Other JSON modes: `./target/release/json_canon_bench 200` (cold table),
`./target/release/json_canon_bench pmu 800` (cyc/byte). Env equivalents:
`JSON_CANON_PAIRED=1`, `JSON_CANON_PMU=1`.

**(b2) One CSS bench (cold) + its PMU:**
```
./target/release/css_canon_bench 200
CSS_CANON_PMU=1 ./target/release/css_canon_bench 800
```
CSS profiling driver (samply attribution, one workload tight loop):
`CSS_CANON_PROFILE=track1_rich ./target/release/css_canon_bench 5000`
(`css_canon_bench.rs:196-221`).

**(warm parity, family B):** `cd skinny && cargo bench -p bbnf-bench --bench json_parity`.

---

## 6. How to add a new arm/variant

**JSON** (e.g. a new candidate parse lane or a simd-json competitor): write a
`fn newlane(input: &str, bytes: &[u8]) -> u64` beside the lanes at
`json_canon_bench.rs:87-107` (return a `black_box`'d fold so it isn't DCE'd),
then add `("newname", newlane)` to `WORKLOADS` `:109-115`. It auto-flows through
cold, `pmu`, and (for the four hard-coded ratios) you'd also add an
`emit_paired(...)` call at `:257-260` referencing its per-sample vector built in
the `:242-255` block. New corpus ⇒ add its stem to `CORPORA` `:116` and drop the
`.json` where `locate_fixture` looks (§2) — `real_typed_struct.rs:1394-1408`
`candidate_names` handles hyphen/underscore aliases. `simd-json 0.13.11` is
already a dep (`Cargo.toml:20`) but unused by the canon bin — a `simd_json` arm
is a drop-in.

**CSS**: write `fn newwork(input: &str) -> u64` beside `:106-134`, add to
`WORKLOADS` `:136-141`. New corpus ⇒ append a `CssL4CorpusSpec` to
`css_l4_corpus.rs:22-54` and place the file in `skinny/corpora/css-l4-sk-v14/`.

Always run the family-B parity oracle on a new JSON arm first
(`direct_struct::assert_direct_struct_parity`, invoked `json_parity.rs:19`)
before trusting its timing.

---

## 7. Binding baseline numbers (from sk-v22/research/04-graduation-baseline.md)

Certified = PMU cyc/byte (mean of 3 windows, iters=800, loadavg 10.2/10.2/12.2);
`parse_tape` is the product. Source table lines `04-graduation-baseline.md:60-77`.

**JSON PMU cyc/byte (certified):**
| corpus | parse_only | parse_tape (product) | fused | sonic | serde |
|---|--:|--:|--:|--:|--:|
| twitter | 1.358 | **1.607** (0.48× sonic ⇒ 2.10× faster) | 1.981 | 3.371 | 4.643 |
| canada | 1.831 | **2.081** (0.71× ⇒ 1.41× faster) | 3.531 (1.21× SLOWER) | 2.930 | — |
| citm | 1.005 | **1.138** (0.48× ⇒ 2.06× faster) | 1.311 | 2.347 | — |

**JSON cold wall** (median ns/byte · median Mbps; `04:82-96`): twitter parse_tape
0.374 ns/B · 21,388 Mbps; canada parse_tape 0.487 · 16,416; citm parse_tape
0.267 · 29,952. sonic: twitter 0.771/10,373, canada 0.730/10,959, citm
0.545/14,687.

**JSON paired ratios (load-certified, N=200×3 windows; `04:107-116`):** twitter
tape/sonic 0.515 (**1.94×**); canada tape/sonic 0.726 (**1.38×**); citm
tape/sonic 0.529 (**1.89×**). The one honest loss: **canada fused/sonic 1.199
(0.83× — fused LOSES), confirmed all 3 windows** (eager `materialize_f64` float
tax; the lazy `parse_tape` product does NOT pay it).

**CSS PMU cyc/byte (certified; `04:174-181`):**
| corpus | track1_full | track1_rich | lightningcss | cssparser | track1 vs lcss |
|---|--:|--:|--:|--:|--:|
| bootstrap | 15.02 | 15.13 | 35.32 | 12.47 | **2.35×** |
| tailwindcss | 13.29 | 13.10 | 46.16 | 21.86 | **3.47×** |
| material | 13.48 | 14.42 | 30.39 | 11.02 | **2.25×** |
| animate | 14.56 | 13.24 | 30.08 | 13.76 | **2.07×** |

**CSS cold wall Mbps median (directional; `04:203-210`):** bootstrap 2039 vs lcss
929; tailwind 2437 vs 689; material 2241 vs 1008; animate 2052 vs 1117
(1.8–3.5×). Headline: `parse_tape` beats sonic-rs 2.10×/1.41×/2.06× and serde
2.75×/2.51×/2.75× cyc/byte cold; CSS track1 beats lightningcss 2.07–3.47×.

**Attribution (`04:128-160`):** scalar structural walk is the floor & bulk
(55–68% of product cycles); tape emit+alloc +13–19% cyc; eager number
materialise is the largest incremental (+1.48 cyc/B on canada — the one lever,
localised to `materialize_f64`; lazy product never pays it). Leaf ranking:
string span scan ~23% (single largest) > structural nav ~21% > ws skip ~14% >
number scan ~5% > alloc ~5-6% > tape emit ~4%. Dead one-shot NEON scan absent
(DELETE stands).

---

## 8. Method-of-record pointers (deeper protocol)

- `restart/skinny/tranches/sk-v21/prototypes/PROTO-A1-bench-adjudication.md` — the paired-adjudication protocol.
- `restart/skinny/tranches/sk-v21/research/33-cold-truth-profiling.md` — cold-truth PMU + xctrace leaf attribution.
- Raw sk-v22 capture windows referenced (`/private/tmp/skv22_w1.txt`,`_w2.txt`,`_w3.txt`,`_css.txt`) — ephemeral, likely gone; not needed to reproduce.
- Frozen JSON parse path (unchanged since): `runtime/src/grammars/json/{generated.rs,parser.rs,scan.rs}`, `tape/assembler.rs`; lazy view number decode `runtime/src/grammars/json/view.rs`.
