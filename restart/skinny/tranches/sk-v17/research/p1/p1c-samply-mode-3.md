# SK-V17 P1-C: samply mode III — masking-probe + structural-scan-only path (CSS-tape subject)

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-29.
Scope: P1-C mode III = the masking-probe + structural-scan-only counterpart, retargeted
to the SK-V17 CSS-tape subject. Profile the BENCHED skinny CSS path on the benched
skinny tree (`skinny/crates/`); resolve every hot leaf to a symbol+file:line; report
median/min/max/stddev for track1 / lightningcss / cssparser per corpus from the ONE
pass-canonical harness (`css_canon_bench.rs`).
Output: this file.
Baseline: SK-V17-open (`6496fecae706c5ffb1b80b82ea5dcfa6f7ff0e33`, master HEAD).
Host triple: aarch64-apple-darwin (Apple M5 Max, 18 cores, Darwin 25.4.0 / xnu T6050).
Build flags: `skinny/Cargo.toml` `[profile.release]` opt-level=3, lto="fat",
codegen-units=1, panic="abort", debug=true, strip=false. No `target-cpu=native` override
(default aarch64 baseline; NEON present unconditionally on the platform).
Profile tool: samply 0.13.1 (`--no-open --save-only --rate 4000`); atos arm64
symbolication at PIE base 0x100000000; rustfilt v0 demangle. rustc 1.96.0-nightly
(02c7f9bec 2026-04-10).
Corpus coverage: 4/4 benched CSS L4 corpora (bootstrap, tailwindcss,
material-components-web, animate) per `css_l4_corpus.rs:21-54`. The 17 JSON corpora of
`PASS-1-PROFILE §2.1` do NOT apply: SK-V17's subject is CSS L4 (HANDOFF.md "What SK-V17
Opens"). Lock 14 anti-overfit is satisfied — both the regular close-criterion rows
(animate, bootstrap) and the dense rows (tailwind, material) are covered, and the load-
bearing finding holds on every one.

> **V2 harness-convergence note (CROSS X2 / CH4-3 fold).** This pass authored five
> divergent "canonical N>=50" harness bins in V1 (`css_cold_harness`, `css_canon_bench`,
> `css_cold_bench`, `css_cold_canonical`, `css_track1_profile`), yielding ~20–29%
> same-plane Mbps dispersion confounding host noise with harness divergence. Per the
> orchestrator designation, **`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` is the
> single canonical harness** (most-cited, carries the PMU mode, and the only one with a
> code-enforced `assert!(n >= 50)` at `:250`). Every absolute number in this V2 is produced
> by `css_canon_bench` and by nothing else; the V1 `css_cold_bench` figures are retired.
> Absolute Mbps differ by harness / allocator state / CPU-flag set — only the *within-
> harness ratio* (track1-vs-lightningcss, plane-vs-plane) is load-bearing across the pass.

> **Cycles-per-byte posture (CROSS X1' / CH4-4 fold — the ONE pass-wide posture, adopted
> verbatim across all six P1 artefacts; settled for two consecutive cycles V2→V3 per CH3 V3
> §2-6 and held into V4 with zero open REVISE).**
>
> > "instr/byte (`ri_instructions`) is the sole load-bearing cost density and is reliable
> > to <0.5%. The sub-1.0 CPI from `ri_cycles` is PHYSICAL (IPC 3.7–6.4 on the M5's ~8-wide
> > core), NOT impossible; however `proc_pid_rusage.ri_cycles` cannot be disambiguated as
> > dynamic core-cycles vs a wall-proportional scaled tick from the rusage interface alone,
> > so cyc/byte is reported RAW and non-load-bearing. No conclusion rests on it."
>
> The harness computes `cyc/ins` (`css_canon_bench.rs:241`), so the measured 0.16–0.28 is
> CPI; **CPI 0.16 ⇔ IPC 6.4, CPI 0.27 ⇔ IPC 3.7** — both inside the M5 Max P-core's ~8-wide
> issue width and the expected signature of a wide superscalar retiring a tight,
> well-predicted scan loop. The figure is therefore not "impossible" and not "falsified"
> (the earlier "physically impossible CPI" framing — struck at the V2→V3 CHALLENGE — confused
> CPI with IPC). It is set aside as **non-disambiguable, non-load-bearing**: the rusage
> interface gives no way to distinguish a real-cycle counter from a wall-proportional tick, so
> any wall-derived GHz reading (e.g. a "steady 4.27 GHz" derivation) is observationally
> identical for both models and proves neither. S-P2 grounds cost on
> **instructions-per-byte (i/B)** alone (`ri_instructions` retirement is exactly counted,
> allocator-/clock-/contention-independent); cyc/byte is co-reported RAW with IPC explicit so
> no reader mis-reads sub-1.0 CPI as a defect.

---

## §1 — Method (commands run; verbatim, reproducible)

### §1.1 — Benched-surface reconciliation (what "mode III for CSS" resolves to)

P1-C's JSON masking probes (`host_call_eager_decode`, `alternate_scalar_plan`,
`cold_first_parse`, `structural_scan_only`; `profile_direct.rs:172-178,249-278`) are
JSON-only and route through `runtime::generated_json::parse`. For the SK-V17 CSS subject
the "structural-scan-only" + masking analogue is the pair of benched CSS Track-1 planes,
both exercised as named workloads by `css_canon_bench.rs:123-128`:

- **Canonical benched Track 1 = `emit_fact_stream`** (`css_canon_bench` workload
  `track1_fact_stream`, `:108`) — the orchestrator's named leaf. `track1_facts`
  (`nonjson_css_l4.rs:596`) → `css_decl::parser::parse`
  (`runtime/src/grammars/css_l4_declaration_values/parser.rs:5`) →
  `generated::emit_fact_stream` (`generated.rs:5`). This is what the benched harness
  `benches/nonjson_css_l4.rs:19` exercises. It is a **byte-walk fact-stream String
  emitter** (`emit_declarations` → `emit_tokens`, `generated.rs:411,472`).
- **Structural-scan-only plane = `emit_full_parse`** (`css_canon_bench` workload
  `track1_full_parse`, `:103`) — the W8 corpus harness leaf. `css_decl::parser::parse_full`
  (`parser.rs:17`) → `generated::emit_full_parse` (`generated.rs:61`) →
  `CssFullParser::parse_stylesheet` (`generated.rs:118`). This is the
  **delimiter/balance structural scanner** carrying `find_component_delim`
  (`generated.rs:288`) and `consume_balanced_at` (`generated.rs:320`) — the exact leaves
  the HANDOFF next-move §2 tags `S-P1-re-confirm-on-benched-path`.

The two are genuinely different code paths over the same grammar module; profiling both
is the load-bearing P1-C finding (see §4 anomaly A1).

### §1.2 — Canonical N>=50 cold harness (designated, not authored this cycle)

`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` is the pass-canonical harness. It
**enforces N>=50 in code** — `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty
gate)")` at `css_canon_bench.rs:250` (the V1 `css_cold_bench` only *commented* the floor;
this is the CH6/§1.2-V1 REVISE fold — the contract is now code-enforced, not promised).
Cold discipline (`PASS-1-PROFILE §8.1`, `no-warm-benches`): `sample()`
(`css_canon_bench.rs:146-177`) does one untimed touch of the corpus (page-in the source
buffer only), then for each of N samples times exactly one `parse(black_box(input))` with
an `Instant`, `black_box`-es the result, and drops it — no parser state reused, output
freshly allocated and freed per sample. Median/min/max/stddev are taken over the N
per-sample Mbps.

```
cd skinny
cargo build --release -p bbnf-bench --bin css_canon_bench   # release: lto=fat, debug=true
./target/release/css_canon_bench 64                          # all 4 workloads, N=64 (run 1)
./target/release/css_canon_bench 64                          # repeat (run 2, stability)
CSS_CANON_PMU=1 ./target/release/css_canon_bench 2000        # PMU: cycles+instructions/byte
```

The four measured workloads (`css_canon_bench.rs:101-128`):
- `track1_full_parse`  → `css_decl::parser::parse_full` (= `emit_full_parse`, delim-scan).
- `track1_fact_stream` → `css_decl::parser::parse`      (= `emit_fact_stream`, String).
- `lightningcss`       → `lightningcss::stylesheet::StyleSheet::parse` (full L2 CSSOM,
  pin `=1.0.0-alpha.71`); checksum `sheet.rules.0.len()`.
- `cssparser`          → `cssparser` (`0.34`) `StyleSheetParser` full walk via
  `CssparserFullParseProbe` (`css_canon_bench.rs:282-403`; token-scan, materializes
  nothing).

Corpus source: `bbnf_bench::css_l4_corpus::load_all` reads `skinny/corpora/css-l4-sk-v14/`
(present on disk, sha256-pinned `css_l4_corpus.rs:28,36,44,52`). `css_canon_bench`
iterates the **4 corpora individually**; it does NOT synthesize a concatenated aggregate
corpus. The aggregate figures in §2.1 are therefore a **byte-weighted derived** roll-up
over the 4 per-corpus medians (time-additive: `Σbytes / Σ(bytes_i/Mbps_i)`), NOT a fifth
parse. The corpus footprint is the **raw sum 979 638 bytes**
(232803+179631+495454+71750 = **979 638**; CH/X-fold reconciliation — V1 wrote 979 642,
which was wrong; the five sibling artefacts use 979 638). A literal concatenation with 3
`\n` separators would be 979 641 bytes, but no benched path concatenates the corpora, so
979 638 is the figure of record.

### §1.3 — samply hot-leaf attribution (on the canonical harness)

`css_canon_bench`'s `CSS_CANON_PROFILE=<workload>` driver (`css_canon_bench.rs:183-208`)
runs a tight uninstrumented loop of one workload over all 4 corpora (no `Instant`, no
stats) for clean parse self-time. samply records:

```
CSS_CANON_PROFILE=track1_fact_stream samply record --no-open --save-only --rate 4000 \
    -o /tmp/skv17-p1c-v2/fact.json.gz -- ./target/release/css_canon_bench 1500
CSS_CANON_PROFILE=track1_full_parse  samply record --no-open --save-only --rate 4000 \
    -o /tmp/skv17-p1c-v2/full.json.gz -- ./target/release/css_canon_bench 6000
```

samply 0.13.1 with `--save-only` stores frame addresses as lib-relative offsets and leaves
`nativeSymbols.length=0`; symbolication is done offline:
`atos -o ./target/release/css_canon_bench -arch arm64 -l 0x100000000 <0x100000000+offset>`
then `rustfilt` (scripts `/tmp/skv17-p1c-v2/selftime.py`, `symb.py`, `byfunc.py`).
Self-time = the leaf frame of each sample. fact_stream = 70 031 leaf samples; full_parse =
84 966 leaf samples. System-dylib leaves (libsystem_malloc / kernel / platform) are
attributed at the **resource level only** (the on-disk dylibs differ from the runtime dyld
shared cache, so per-symbol atos on them is unreliable and is NOT claimed; the resource
bucketing IS reliable — it is the same dyld-image partition samply records natively).

---

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

### §2.1 — Canonical cold throughput, N=64 (Mbps), run 1 / run 2 (css_canon_bench)

`track1_fact_stream` (= `emit_fact_stream`) is the orchestrator-named benched Track-1
plane. Aggregate row is the §1.2 byte-weighted derived roll-up over 979 638 bytes (not a
parse).

| Workload | corpus | bytes | median r1 | median r2 | min (r1) | max (r1) | stddev (r1) |
|---|---|---|---|---|---|---|---|
| **track1_fact_stream** | bootstrap | 232803 | **813.53** | 835.34 | 709.65 | 843.41 | 28.67 |
| lightningcss (full CSSOM) | bootstrap | 232803 | 954.10 | 1103.93 | 725.22 | 1111.62 | 97.63 |
| cssparser (token-scan) | bootstrap | 232803 | 2601.45 | 2859.31 | 2122.32 | 2833.30 | 170.24 |
| **track1_fact_stream** | tailwindcss | 179631 | **567.96** | 566.00 | 437.00 | 602.63 | 23.85 |
| lightningcss | tailwindcss | 179631 | 817.51 | 803.59 | 581.13 | 851.44 | 39.98 |
| cssparser | tailwindcss | 179631 | 1691.60 | 1706.96 | 1325.64 | 1742.76 | 97.05 |
| **track1_fact_stream** | material-components-web | 495454 | **852.99** | 866.64 | 596.30 | 925.25 | 59.91 |
| lightningcss | material-components-web | 495454 | 1229.18 | 1251.04 | 995.24 | 1291.56 | 65.98 |
| cssparser | material-components-web | 495454 | 3219.36 | 3264.77 | 2700.63 | 3290.12 | 113.66 |
| **track1_fact_stream** | animate | 71750 | **755.04** | 745.41 | 635.63 | 796.58 | 28.72 |
| lightningcss | animate | 71750 | 1218.85 | 1222.25 | 871.62 | 1258.20 | 50.35 |
| cssparser | animate | 71750 | 2615.53 | 2645.41 | 2124.29 | 2649.24 | 71.62 |
| **track1_fact_stream** | aggregate (derived) | 979638 | **770.72** | — | — | — | — |
| lightningcss | aggregate (derived) | 979638 | 1081.08 | — | — | — | — |
| cssparser | aggregate (derived) | 979638 | 2640.03 | — | — | — | — |

track1_fact_stream / lightningcss median ratio (run 1): bootstrap 0.853, tailwind 0.695,
material 0.694, animate 0.620, aggregate 0.713. **`emit_fact_stream` is below lightningcss
on every corpus by ~1.17–1.61×** — NOT the ~14× the prior SK-V16 canonical narrative
reported. The ~14× (≈70 Mbps) figure belonged to a *different plane* (typed-retime / W8
broadcast), not the benched `emit_fact_stream` String emitter (see §3, §4 A2).

### §2.2 — Structural-scan-only plane: `track1_full_parse` cold throughput, N=64 (Mbps)

| Workload | corpus | bytes | median r1 | median r2 | min (r1) | max (r1) | stddev (r1) |
|---|---|---|---|---|---|---|---|
| **track1_full_parse** (emit_full_parse) | bootstrap | 232803 | **2125.15** | 2296.87 | 1758.32 | 2417.95 | 147.45 |
| **track1_full_parse** | tailwindcss | 179631 | **2471.73** | 2562.06 | 1821.55 | 2818.20 | 206.41 |
| **track1_full_parse** | material-components-web | 495454 | **2537.03** | 2538.01 | 2212.83 | 2622.97 | 94.42 |
| **track1_full_parse** | animate | 71750 | **2488.44** | 2310.25 | 1590.39 | 2557.27 | 177.79 |
| **track1_full_parse** | aggregate (derived) | 979638 | **2438.01** | — | — | — | — |

**The benched delimiter/balance structural scanner already BEATS lightningcss on all 4
corpora** (track1_full_parse / lightningcss median ratio run 1: bootstrap 2.23×, tailwind
3.02×, material 2.06×, animate 2.04×) **and is on par with or above cssparser token-scan.**
It is recognition-only — `emit_full_parse` counts rules/at-rules/qualified/declarations
into a `CssFullParseSummary` and emits a one-line summary String (`generated.rs:91-99`); it
materializes NO rich CSSOM. This is the load-bearing fact for S-P2: the scalar structural
scan is *not* the wall; the wall is rich materialization + String/alloc.

### §2.3 — Hot-leaf self-time, `emit_fact_stream` (canonical track1_fact_stream), 70 031 leaf samples

By resource (`/tmp/skv17-p1c-v2/fact.json.gz`):

| Self-time | Resource | Classification |
|---|---|---|
| 35.56% | css_canon_bench (own code) | String-build + hash + int-format |
| 29.10% | libsystem_kernel.dylib | alloc/page-fault syscalls (madvise/munmap on String free) |
| 28.15% | libsystem_malloc.dylib | allocator (String grow/free) |
| 7.20% | libsystem_platform.dylib | `_platform_memmove` (String copy) |

**Syslib floor = 64.45%** (kernel+malloc+platform); own-code = 35.56%. Own-code leaves
(symbolicated, % of all 70 031 leaf samples; the leaf is anchored at the function header
`generated.rs:5` — the intra-function `:26`/`:45` figures are the **source-verified
inclusive call sites** inside `emit_fact_stream`, retained for attribution, not asserted as
independent functions; CH4-2 fold):

| Self-time | Symbol | file:line | Class |
|---|---|---|---|
| 23.89% | `generated::emit_fact_stream` | `generated.rs:5` (fn header) | tape/serialize |
| — of which 15.45% | inclusive at `generated.rs:45` | the `emit_declarations(input, &mut out)` push-str call site (source-verified: `:45`) | tape/serialize |
| — of which 8.45% | inclusive at `generated.rs:26` | the `push_hex64(&mut out, fnv64(input.as_bytes()))` call (source-verified: `:26`) | hash (FNV) |
| 8.35% | `generated::push_ascii_lower_hex` | `generated.rs:628-634` (body `:630,:631,:633`) | string (hex-encode property/lexeme bytes) |
| 1.30% | `<u32>::_fmt` | `num.rs:185` (libcore int→decimal for `input.len().to_string()` etc.) | string |
| ~1.7% | DYLD-STUB$$free/memcpy/malloc + `__rdl_alloc/dealloc` | `alloc.rs:450,463` | alloc shim |

Inclusive: `track1_fact_stream` → `emit_fact_stream` (`generated.rs:5`) carries the whole
own-code 35.56%; the `RawVecInner` grow/`finish_grow` frames feed the malloc resource. This
is a textbook **eager-String-serialization + per-parse-alloc floor**: zero structural-scan
cost (this plane uses the `emit_declarations` byte walk at `generated.rs:411`, not the
delim-scan), all cost is String emission, FNV hashing, int formatting, and the
allocator/page-fault tax of building+freeing the multi-KB fact-stream String each cold
parse. The reliable-counter restatement (X1' posture): fact_stream executes **215–366 i/B**
vs full_parse **46–58 i/B** (§2.5) — a **~4.4–7.1× instruction-per-byte tax**, all of which
is String/hash/format/alloc, none of it recognition.

### §2.4 — Hot-leaf self-time, `emit_full_parse` (structural-scan-only), 84 966 leaf samples

By resource: css_canon_bench 99.86%, syslib 0.14% (kernel 0.04 / malloc 0.08 / platform
0.02). The alloc floor is **entirely absent** — `emit_full_parse` emits only a tiny summary
String.

Collapsed self-time by function (`/tmp/skv17-p1c-v2/full.json.gz`, % of 84 966 leaf):

| Self-time | Function | file:line of leaf lines | **Class** |
|---|---|---|---|
| **58.59%** | `CssFullParser::find_component_delim` | `generated.rs:298,295,294,307,296,311,288,293` | **scan** (byte-class-membership inner loop) |
| 27.93% | `track1_full_parse` (harness frame) | `css_canon_bench.rs:104` | scaffold (`out.len()` + `black_box` + LTO-inlined `parse_full` return; pure measurement scaffold, no second pass) |
| **9.98%** | `CssFullParser::consume_balanced_at` | `generated.rs:327,323,336,340,325,320,328` | **structural recursion OVER the scan primitive** (paren/bracket/brace balance; shares the `find_component_delim` byte-membership inner loop — one NEON target) |
| 2.38% | `CssFullParser::parse_block` | `generated.rs:204,191,189,195,192` | structural (dispatch) |
| 0.87% | `CssFullParser::parse_declaration` | `generated.rs:243,247,261` | structural |
| 0.10% | `CssFullParser::parse_at_rule` | `generated.rs:140` | structural |

**Re-classification (CH/X fold).** `find_component_delim` is classed **scan**, not
"structural": its source body is the byte-class-membership test `delimiters.contains(&byte)`
(`generated.rs:295`) over a delimiter set, walking the buffer byte-at-a-time
(`generated.rs:293-308`). This is the **same byte-class-membership primitive JSON runs
through `select_classifier` / `PrimitiveKernels`** (verified `json/scan.rs:219`) — a
grammar-neutral scan leaf, not a CSS-named structural one (CH2 generality). The hot lines
within it (corrected line attribution): `:295` is `delimiters.contains(&byte)` (the
membership test, 17.16% self), `:298` is the `match byte` dispatch (27.90% self), `:294` is
the `self.bytes[pos]` load (4.52%), `:307` is the `_ => pos + 1` advance (4.31%), `:293` is
the `while pos < len` loop test (0.14%). `consume_balanced_at` is **structural recursion
over** that same scan primitive — it re-runs the identical byte-membership/dispatch inner
loop against a single close-delimiter (`generated.rs:322-337`), so it shares
`find_component_delim`'s NEON target rather than being a separate kernel.

Discounting the 27.93% harness-loop scaffold, of the *parse* self-time the scan leaf
`find_component_delim` + `consume_balanced_at` ≈ **95%**. As fractions of total leaf
samples: `find_component_delim` **58.59%**, `consume_balanced_at` **9.98%**.

**RE-CONFIRMED on the benched skinny path** (the HANDOFF §2 `S-P1-re-confirm` obligation):
the architecture profile's inherited core-tree figures `find_component_delim ~56%` /
`consume_balanced_at ~10%` (`sk-v16-css-sota-tape-architecture.md:255-256`) reproduce
within noise on the benched skinny tree on the canonical harness at **58.59% / 9.98%**.

---

## §2.5 — Reliable per-byte cost ledger (PMU, instructions-per-byte; CSS_CANON_PMU, iters=2000)

`CSS_CANON_PMU=1 ./target/release/css_canon_bench 2000`, `proc_pid_rusage` V5
(`css_canon_bench.rs:211-247`); CPI = `cyc/ins` (`css_canon_bench.rs:241`). **i/B is the
reliable, load-bearing per-byte cost** (X1' posture). `cycles_per_byte` and CPI are
co-reported RAW and **non-load-bearing**: the sub-1.0 CPI is PHYSICAL — `CPI = 1/IPC`, so
CPI 0.16–0.28 ⇔ IPC 3.6–6.4, inside the M5 Max ~8-wide issue width (NOT impossible, NOT a
falsified counter; reading sub-1.0 CPI as impossible confuses CPI with IPC). The reason it is
set aside is disambiguability, not physics: `proc_pid_rusage.ri_cycles` cannot be
distinguished as dynamic core-cycles vs a wall-proportional scaled tick from this interface
alone. S-P2 grounds cost on i/B only.

| corpus | workload | **i/B (reliable)** | c/B `ri_cycles` (RAW, non-load-bearing) | CPI = 1/IPC (RAW; 0.16–0.28 ⇒ IPC 3.6–6.4, physical) | Mbps |
|---|---|---|---|---|---|
| bootstrap | track1_full_parse | **53.71** | 15.32 | 0.285 | 2225.5 |
| bootstrap | track1_fact_stream | **235.00** | 41.39 | 0.176 | 810.9 |
| bootstrap | lightningcss | **160.30** | 33.34 | 0.208 | 1001.6 |
| bootstrap | cssparser | **68.32** | 12.24 | 0.179 | 2754.5 |
| tailwindcss | track1_full_parse | **51.52** | 13.72 | 0.266 | 2472.4 |
| tailwindcss | track1_fact_stream | **366.10** | 63.16 | 0.172 | 538.7 |
| tailwindcss | lightningcss | **236.61** | 41.73 | 0.176 | 820.3 |
| tailwindcss | cssparser | **126.12** | 19.84 | 0.157 | 1728.8 |
| material-components-web | track1_full_parse | **46.46** | 13.21 | 0.284 | 2595.0 |
| material-components-web | track1_fact_stream | **215.15** | 40.49 | 0.188 | 833.6 |
| material-components-web | lightningcss | **137.59** | 26.98 | 0.196 | 1272.0 |
| material-components-web | cssparser | **60.86** | 10.65 | 0.175 | 3214.8 |
| animate | track1_full_parse | **57.73** | 14.62 | 0.253 | 2331.8 |
| animate | track1_fact_stream | **279.26** | 45.54 | 0.163 | 753.2 |
| animate | lightningcss | **155.32** | 27.92 | 0.180 | 1228.0 |
| animate | cssparser | **79.58** | 13.15 | 0.165 | 2599.9 |

Reliable-counter readings (i/B) load-bearing for S-P2:
- **fact_stream / full_parse i/B ratio = 4.38× (bootstrap) … 7.11× (tailwind).** The
  fact-stream String plane executes ~4.4–7.1× more *retired instructions per byte* than the
  same-grammar structural scan. This is the load-bearing restatement of the §2.3 String tax
  (the non-load-bearing `ri_cycles` c/B reads this same ratio as ~2.7–4.6×; the reliable
  instruction retirement reads it as 4.4–7.1×, and i/B is the figure S-P2 grounds on). S-P2
  should size the tape-activation lever against **i/B**, not the non-load-bearing c/B.
- **full_parse i/B (46–58) < cssparser i/B (61–126).** The benched scan plane retires
  *fewer* instructions per byte than cssparser's token-scan — corroborating §2.2 that the
  scalar structural scan is genuinely cheap on the recognition plane.
- **fact_stream i/B (215–366) > lightningcss i/B (138–237).** The String fact-stream
  retires *more* instructions/byte than lightningcss builds a full CSSOM — the String
  emission is strictly more expensive per byte than CSSOM materialization, which is the
  whole reason it loses §2.1.

---

## §3 — Delta vs SK-V16 close (per row; Mbps + classification)

Prior SK-V16 canonical (contract / alphaB:112-115): bbnf CSS Track 1 **~70 Mbps full
corpus (51–164 per corpus)**, lightningcss **~974 Mbps canonical (793–833 cited)**,
cssparser **~2539 (2476–2529 cited)**. The 24 falsified `css_l4/*/direct_to_struct/main`
RESULTS rows (lines 112-135) carry the single broadcast tuple `2319.041/2362.037/929.281`.

| Plane | SK-V16 reported | SK-V17 P1-C measured (css_canon_bench) | Δ / classification |
|---|---|---|---|
| Track1 `emit_fact_stream` (canonical benched) | conflated as "~70 Mbps / ~14× slow" | **568–853 Mbps/corpus; 0.62–0.85× lightningcss** | **G — plane reconciled.** The ~70 figure was NOT the benched `emit_fact_stream`; it was the typed-retime/broadcast plane. The benched fact-stream plane is ~8–12× faster than ~70 and is only ~1.2–1.6× below lightningcss, not ~14×. |
| Track1 `emit_full_parse` (structural-scan-only) | unprofiled on benched path | **2125–2537 Mbps/corpus; 2.0–3.0× lightningcss** | **A — new measured truth.** Beats lightningcss on all 4 corpora (recognition-only). |
| lightningcss full-CSSOM (same-run bar) | ~974 canonical / 793–833 cited | **804–1251 Mbps/corpus; 1081 derived-agg** | within/above the cited 793–833 band; the 974 "canonical" is not reproduced same-run (matches alphaB:114 "no single committed number is 974"). |
| cssparser token-scan | ~2539 / 2476–2529 cited | **1692–3265 Mbps/corpus; 2640 derived-agg** | within/above band. |
| find_component_delim self-time | ~56% (core-tree, inherited) | **58.59% (benched skinny, canon harness)** | **re-confirmed, +2.6pp** — within profiler noise; re-classed **scan** (§2.4). |
| consume_balanced_at self-time | ~10% (core-tree, inherited) | **9.98% (benched skinny, canon harness)** | **re-confirmed, −0.0pp** — structural recursion over the scan primitive. |
| syslib alloc/copy floor | 68.7% (Investigation 5, rich-AST plane) | **64.45% (benched emit_fact_stream)** | **re-confirmed, −4.2pp** — same structural floor on the benched fact-stream plane. |

The W6 `sample_count=1` / W8 `W8_PROFILE_ITERS=8`-single-loop harness is retired by the
code-enforced N>=50 (here N=64, two runs) median/min/max/stddev `css_canon_bench` harness.
No prior CSS row carried a stddev; all per-corpus rows now do.

---

## §4 — Anomalies + masking signals (flagged for S-P2)

- **A1 (plane bifurcation — load-bearing).** "Benched CSS Track 1" is two distinct code
  paths over one grammar module: `emit_fact_stream` (`parse`, the bench-harness leaf,
  String fact-stream, alloc-bound) and `emit_full_parse` (`parse_full`, the W8 corpus
  leaf, delim-scan, compute-bound). They have *disjoint* hot-leaf sets:
  `emit_fact_stream` = 64% syslib + String-build, ZERO `find_component_delim`;
  `emit_full_parse` = 95% (of parse self) `find_component_delim`+`consume_balanced_at`,
  ZERO alloc floor. S-P2 must not conflate them. The SK-V17 tape activation (HANDOFF lever
  1) replaces the `emit_fact_stream` String with a `TapeBuilder` append — so it removes the
  §2.3 floor; the NEON pre-scan (lever 3) targets the §2.4 `find_component_delim` **scan**
  leaf — which only becomes the wall once the structural scanner has to *also* feed a rich
  tape (today `emit_full_parse` is recognition-only at ~2.4 Gbps and is already past
  lightningcss).
- **A2 (masking probe — `emit_full_parse` beats lightningcss but is wrong-plane).** This is
  the CSS analogue of a masking probe (`PASS-1-PROFILE §8.5`): a stripped path
  (recognition-only summary, no CSSOM) that beats the >SOTA bar by 2.0–3.0× names the
  structural inefficiency as *materialization*, not *scanning*. The scalar delim-scan is
  not the bottleneck for crossing lightningcss; the bottleneck is doing it while building a
  rich, lazily-accessible CSSOM. The reliable-counter sizing (§2.5): the scan plane retires
  46–58 i/B while lightningcss retires 138–237 i/B — the headroom S-P2's tape/projection
  design may spend on materialization is ~80–180 i/B before parity, ~90 i/B at the regular
  rows. S-P2's tape model must carry the §2.4 scan speed into a rich-output path without
  re-incurring the §2.3 String/alloc floor (which costs +160–310 i/B over the scan).
- **A3 (FNV in the benched hot path — diagnostic-only).** `emit_fact_stream` spends ~8.45%
  inclusive self-time at `generated.rs:26` on `push_hex64(&mut out, fnv64(...))`, and
  `push_ascii_lower_hex` (`generated.rs:628`) a further 8.35%. **FNV and the hex encoder are
  FNV-diagnostic primitives with NO CSS-semantic value** (consistent with P1-A §4.3, P1-B
  §4.3): they exist only to stamp an input-hash witness into the fact-stream diagnostic
  String that SK-V17 retires. This cost vanishes with tape activation. Flagged so S-P2 does
  not carry FNV/hex-encode into the tape emitter (REDRESS "FNV stays bench-only").
- **A4 (cold first-touch min outliers).** Per-corpus mins (e.g. animate run-2 min 1244.7
  vs median 2310.2 on track1_full_parse) are single cold first-touch samples paying the
  page-in tax; the medians/stddev are robust. Not a harness defect — it is the honest cold
  first-touch the `no-warm-benches` discipline mandates we keep. The `sample()` untimed
  pre-touch (`css_canon_bench.rs:152`) pages the *source* buffer only; the parse output
  allocation is fresh and cold per sample.
- **A5 (no NEON in the benched leaves yet).** Neither benched plane touches
  `bbnf-simd/src/dispatch.rs` `select_classifier` — the structural scan is scalar
  byte-at-a-time (`generated.rs:293-308`). The NEON union (HANDOFF lever 3) is unbuilt; the
  `find_component_delim` 58.59% **scan** leaf is the profile-first antecedent that justifies
  it, gated behind tape activation (there is no structural index until the tape decodes
  CSS). Because `consume_balanced_at` shares the same byte-membership inner loop (§2.4),
  one NEON byte-class-membership kernel covers ~68.6% of the structural-scan plane's
  self-time, not just the 58.59% leaf.

**Pre-blocked-route check (CH3).** §4 proposes nothing. It observes that tape activation
(HANDOFF lever 1/2) removes the §2.3 floor and NEON (lever 3) targets the §2.4 scan leaf —
both are SYNTHESIS-sanctioned levers, not REDRESS re-opens. The CSS fact-stream String
plane (`emit_fact_stream`) is REDRESS-pre-blocked as a *live admission plane* (HANDOFF
"Pre-Blocked Routes"); this profile measures it as diagnostic only and does NOT propose
admitting it. No REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89 route is implied.

---

## §5 — Sources (every artefact path + run id)

- Canonical harness (pass-designated; NOT authored this cycle):
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` — N>=50 code-enforced (`assert`
  `:250`), cold per-parse `sample()` (`:146`), workloads `:101-128`, `CSS_CANON_PMU`
  (`:211`), `CSS_CANON_PROFILE` (`:183`), cssparser probe (`:282-403`). The V1 P1-C bin
  `css_cold_bench.rs` and the other three divergent bins are retired (CROSS X2 fold).
- Benched CSS path under profile: `runtime/src/grammars/css_l4_declaration_values/`
  `parser.rs:5` (`parse`→`emit_fact_stream`), `parser.rs:17` (`parse_full`→
  `emit_full_parse`), `generated.rs:5,61,118,288,295,298,320,411,472,619,628,636`.
- Throughput + PMU log: `/tmp/skv17-p1c-v2/canon.txt` (N=64 ×2 cold, then
  `CSS_CANON_PMU=1 … 2000`). The §2.1/§2.2/§2.5 tables are verbatim from this log.
- samply flame profiles (binary, not committed):
  `/tmp/skv17-p1c-v2/fact.json.gz` (emit_fact_stream, 70 031 leaf samples, rate 4000,
  `CSS_CANON_PROFILE=track1_fact_stream … 1500`);
  `/tmp/skv17-p1c-v2/full.json.gz` (emit_full_parse, 84 966 leaf samples,
  `CSS_CANON_PROFILE=track1_full_parse … 6000`). samply run log `/tmp/skv17-p1c-v2/samply.log`.
- Symbolication scripts: `/tmp/skv17-p1c-v2/selftime.py` (resource bucketing + addr dump),
  `/tmp/skv17-p1c-v2/symb.py` (atos+rustfilt by line, base 0x100000000),
  `/tmp/skv17-p1c-v2/byfunc.py` (collapse by function).
- Host: Apple M5 Max, aarch64-apple-darwin, Darwin 25.4.0 (xnu T6050), 18 cores. rustc
  1.96.0-nightly (02c7f9bec 2026-04-10). samply 0.13.1. atos /usr/bin/atos. rustfilt.
  lightningcss `=1.0.0-alpha.71`, cssparser `0.34`.
- Baseline commit: `6496fecae706c5ffb1b80b82ea5dcfa6f7ff0e33` (master HEAD).
