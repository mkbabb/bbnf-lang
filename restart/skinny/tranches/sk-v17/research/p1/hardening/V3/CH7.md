# CH7 OVERFIT-PRUNE (V3) — S-P1 PROFILE hardening review

Lens: CH7 OVERFIT-PRUNE. Cycle: V3. Date: 2026-05-29.
Reviewer scope (PASS-1-PROFILE §3 lens-extension; ORCHESTRATOR §3W): the canonical
bench is honest — N>=50, no single-sample, no broadcast, no fixture short-circuit;
lightningcss is the fair full-CSSOM bar; no wrong-tree (crates/core) measurement; the
"BEATS lightningcss" headline must not masquerade as a typed-gate win.
Subject artefacts: `restart/skinny/tranches/sk-v17/research/p1/p1{a..f}.md` (all now V3).
Baseline: SK-V17-open (`6496fecae`). Host: aarch64-apple-darwin, Apple M5 Max.

Method: re-read PASS-1-PROFILE §2.1/§3/§8 + the SK-V17 contract; re-read the V2 CH7
(11/11 ACCEPT) and the V2 CH4-4 / CH6-REJECT / X1' dispositions that the V3 p1f fold
log claims to resolve; read the canonical harness source end-to-end
(`css_canon_bench.rs`, all 403 lines); read the benched grammar leaf
(`css_l4_declaration_values/generated.rs:288-311`), the corpus loader
(`css_l4_corpus.rs`), the conflated comparator (`nonjson_css_l4.rs:636
lightningcss_facts`), and the retired W8 broadcast loop (`css_l4_w8.rs:217`).
Independently rebuilt (`cargo build --release -p bbnf-bench --bin css_canon_bench`,
exit 0), fired the N<50 gate (panics at `:250`), re-ran the harness at N=60, and
byte-counted the four pinned corpora on disk. Verified the two V2 REVISE/REJECT items
folded into the V3 dispatch of their owning agent (p1f).

---

## §1 — Disposition table (path:line + concrete fix)

| # | Artefact:section | Concern | Disposition | Fix |
|---|---|---|---|---|
| 1 | p1f §1.1:91-97 (sample) / `css_canon_bench.rs:146-177,250` | N>=50 honesty, cold-per-parse, no warm | **ACCEPT** | none — code-verified + N<50 gate fires + reproduced |
| 2 | p1f §2.1:221-238 (16 rows) / `css_canon_bench.rs:261-277` | no broadcast; per-corpus×per-workload split | **ACCEPT** | none — verified loop, one ROW per corpus×workload, no aggregate tuple |
| 3 | p1f §1.1:107-112 / `css_canon_bench.rs:113-116` | lightningcss = standalone full-CSSOM, not the conflated facts path | **ACCEPT** | none — verified standalone `StyleSheet::parse`; `:636 lightningcss_facts` IS the fixture-shaped projection, correctly excluded |
| 4 | p1f §2.3:405-425 | lightningcss comparator genuinely materializes | **ACCEPT** | none — ~38% cssparser tokenizer + ~30% typed build/drop |
| 5 | (all six) | wrong-tree crates/core measurement | **ACCEPT** | none — zero crates/core measurement citations (grep clean) |
| 6 | p1f §3.2:475-496 (recognition-only) | "BEATS lightningcss" plane-mask laundering | **ACCEPT** | none — uniformly recognition-only / "does not by itself discharge the typed gate" |
| 7 | p1f §1.1 frontmatter:26-30 (4/4 CSS coverage) | §2.1 17-corpus mandate vs CSS subject | **ACCEPT** | none — correctly scoped to CSS-tape subject; 4 regular shipping corpora |
| 8 | p1f §1.1.1:128-165 (X2 single-harness) | five competing "canonical" harnesses | **ACCEPT** | none — single canonical harness designated, comparability caveat stated + demonstrated |
| 9 | p1f §3.2:455-487 ("~70/14×" falsification) | prior narrative laundered as benched truth | **ACCEPT** | none — classified N-direct, no fresh antecedent |
| 10 | **V2 CH4-4 / CH6-REJECT / X1'** — p1f §2.2.1:313-344 (CPI physics) | "physically impossible CPI" was wrong physics; cross-artefact authority root | **ACCEPT** | resolved — V3 strikes "impossible", adopts IPC 3.5-6.4 PHYSICAL + non-disambiguable framing exactly per the CH4-4 prescribed fix |
| 11 | **V2 CH5-V2-R1** — p1f §2.3:354,360-363 (wrapper line cite) | row-2 cite `:43` (PMU struct) vs `:103-105` (wrapper fn) | **ACCEPT** | resolved — V3 cites `:103-105` (`track1_full_parse` fn), source-verified; correction annotated |

Counts: **ACCEPT 11, REVISE 0, REJECT 0** (11 disposed sections). CH7 ACCEPT rate
this artefact-set: **11/11 = 100%** (V1 7/9 = 77.8%, V2 11/11 = 100%, V3 11/11 = 100%).

---

## §2 — ACCEPT findings (verified, load-bearing)

**A1 — The bench is statistically honest (PRIMARY lens obligation).**
`css_canon_bench.rs:250` carries `assert!(n >= 50, "N must be >= 50 (SK-V17
telemetry-honesty gate)")` — and I fired it: `./target/release/css_canon_bench 10`
panics at `css_canon_bench.rs:250` with that message. The gate is real, not
decorative. Default N=200 (`:249`). `sample()` (`:146-177`) takes N cold per-parse
samples: each sample times exactly one `parse(black_box(input))` call
(`Instant::now()` … `elapsed()`, `:154-156`), black-boxes and drops the result
(`:157`), reuses no parser state across samples. The single touch outside the timed
window (`:152`) defeats first-touch page-fault contamination of the SOURCE buffer
only — the parse itself is cold per sample, satisfying the `no-warm-benches`
cold-per-parse contract. Median/min/max/stddev reported (`:160-176`). The W6
`W6_SAMPLE_COUNT=1` single-sample harness and the W8 broadcast loop
(`css_l4_w8.rs:217`, verified: one `time_loop` over `W8_PROFILE_ITERS × TRACK1_PROFILES
× sources → one elapsed → one aggregate tuple) are explicitly retired (p1f §1.1:82-85,
§3.1:438-453). **Independently reproduced at N=60** this review (2026-05-29, Apple M5
Max, host=arm64): full_parse BEATS lightningcss on all 4 corpora (bootstrap
2159/1035=2.09×, tailwind 2269/786=2.89×, material 2306/1184=1.95×, animate
2046/1167=1.75×); fact_stream BELOW lightningcss on all 4 (0.52-0.78×). My absolute
medians run ~5-12% below p1f's N=200 (host scheduling noise) but every ratio and every
verdict holds — exactly the within-harness comparability caveat p1f §1.1.1 names.

**A2 — No broadcast.** `css_canon_bench.rs:261-277` is a `for corpus in &corpora { for
(name, parse) in WORKLOADS { let s = sample(...) } }` nest emitting one `ROW` per
corpus×workload (`:265-275`). Sixteen distinct rows (4 corpora × 4 workloads), each
with its own N median/min/max/stddev — verified in my N=60 run, which emits 16 ROW
lines, no aggregate. The W8R 24-row broadcast (one `time_loop` tuple, N effectively 1)
is retired and faithfully reproduced per-corpus (p1f §3.1:444-453).

**A3 — No fixture short-circuit.** `track1_full_parse` → `parse_full` → `emit_full_parse`
(`generated.rs:61`) runs a real `while pos < self.bytes.len()` byte scan
(`find_component_delim` :288-311, source-verified this review: `:293` loop test,
`:294` byte load, `:295` `delimiters.contains(&byte)`, `:298` `pos = match byte`,
`:307` `_ => pos + 1`). `track1_fact_stream` → `parse` → `emit_fact_stream`
(`generated.rs:5`) runs a real per-declaration scan with hex-encoding. Both are
content-dependent (my N=60 rates vary per-corpus: full_parse 2046-2306, fact_stream
524-818). The `.expect(...)` on every workload (`css_canon_bench.rs:104,109,114,119`)
means a parse error panics — none did, so all 4 corpora parse cleanly on all 4
workloads (the comparator is not spuriously fast from early-erroring). Crucially, the
canonical harness's lightningcss workload does NOT call `validate_fixture_shape`
(contrast `nonjson_css_l4.rs:637`, which gates the conflated path on the fixture shape)
— so there is no fixture short-circuit on the bar. The corpus loader reads real
sha256-pinned files from `corpora/css-l4-sk-v14/` (verified on disk: animate 71750,
bootstrap 232803, material 495454, tailwind 179631 = **979638**, matching p1f §1.3:208).

**A4 — lightningcss is the fair >SOTA bar, NOT the conflated facts path.**
`css_canon_bench.rs:113-116` calls `StyleSheet::parse(input, ParserOptions::default())`
+ `black_box(sheet.rules.0.len())` — the standalone full-CSSOM materialization. I
read the conflated alternative end-to-end: `nonjson_css_l4.rs:636 lightningcss_facts`
does `validate_fixture_shape` → CSSOM build → `collect_lightningcss_declarations`
into a projection Vec → compares against `expected_fixture_projection()` →
`fixture_sidecar_facts` — a fixture-shaped, projection-walking, sidecar-emitting path
that would NOT be a fair bar. p1f §1.1:109-112 correctly names this distinction and
the canonical harness times the CSSOM build only. The flame attribution (p1f
§2.3:405-425) proves genuine materialization: ~38% cssparser tokenizer + ~30%
building+dropping typed `Property`/`Selector`/`CssRule` nodes — a true full-CSSOM
build, the correct bar per SYNTHESIS §0.6.

**A5 — No wrong-tree (crates/core) measurement.** `grep crates/core` across all six
V3 artefacts returns ZERO measurement citations. Every benched symbol resolves to
`skinny/crates/...` (runtime grammar `generated.rs`, bbnf-bench harness, tape
assembler). crates/core is the SK-V18 totality-fold target and is correctly NOT
profiled here, per the BENCHED-SURFACE contract.

**A6 — The "BEATS lightningcss ~2.0-3.0×" headline is NOT laundered into a typed-gate
win (the central overfit hazard, contained).** `track1_full_parse` materializes only
a 4-field `CssFullParseSummary` (rules/at_rules/qualified_rules/declarations,
`generated.rs:53-59`); the harness wrapper black-boxes `out.len()` — a
count-the-braces recognition scan, no CSSOM, no value tree. p1f marks this uniformly
and explicitly: §1.1:103 ("Emits a 4-field summary"), §3.2:475-477 ("the
recognition-only full-parse plane already BEATS lightningcss; … SK-V17's task is to
land a TYPED plane (preserve-rich-ast)"), §3.2 outcome table:484 ("A (admit-shaped) —
already > bar; but it is recognition-only (4-field summary), NOT preserve-rich-ast,
so it does not by itself discharge the SK-V17 typed gate"), §4.1:495-496. The
discipline is uniform across all six artefacts (verified V2; unchanged V3). No agent
claims the typed win from the recognition number. This is the model behaviour the
OVERFIT-PRUNE lens exists to enforce.

**A7 — 4/4 CSS corpus coverage is correctly scoped, not an overfit.** PASS-1-PROFILE
§2.1's 17-corpus mandate binds the JSON profiling subject; SK-V17's subject is the
CSS-tape plane, whose benched corpus set is fixed at 4 real-world production sheets
(bootstrap/tailwind/material/animate, `css_l4_corpus.rs:21-54`, sha256-pinned) per
SYNTHESIS §0.5. These are regular shipping corpora — "no contrivance, >=1 regular
corpus" satisfied. Not a float-heavy or string-light cherry-pick; all four exercise
the same delimiter/balance scan (the 59% `find_component_delim` leaf holds across all
four, and my N=60 full_parse rates are tight 2046-2306 across the set). p1f frontmatter
:26-30 states the scoping justification. JSON 51/51 is kept as a guard tripwire (§4.7),
not a P1-F measurement.

**A8 — X2 single-harness convergence is resolved honestly.** p1f §1.1.1:128-165, as
the bench/measurement authority, designates `css_canon_bench.rs` as THE single
canonical harness on objective criteria (the only one that simultaneously asserts
N>=50 at `:250` grep-verified, carries the PMU instr/byte mode, carries the samply
driver, and is cited with correct line numbers) and declares the other four
superseded. It states and DEMONSTRATES the comparability caveat (§2.1.1:250-267): only
within-harness same-run ratios are load-bearing; absolute Mbps drift ≤17% across runs
by host noise. My independent N=60 re-run confirms this — the ratios held, the
absolutes drifted ~5-12% below p1f's N=200. The gate signal is the same-run ratio, not
the absolute median.

**A9 — The "~70 Mbps / ~14×" prior narrative is falsified, not laundered.** p1f §3.2
classifies the contract-supplied "~70 Mbps, ~14× slower than lightningcss" as
**N-direct (no fresh benched antecedent)**: the benched full-parse plane is
2327-2727 Mbps (BEATS), the fact-stream plane 549-882 (below), neither is ~70; the
only ~3 Mbps figure is the pre-blocked eager-typed retime (AZ-IV, SYNTHESIS §0.4).
This is the profile-first non-negotiable (ORCHESTRATOR §8) applied correctly.

---

## §3 — V2 REVISE/REJECT items: folded + verified (orphan-free)

**V2 CH4-4 / CH6-REJECT / X1' (p1f §2.2.1 "physically impossible CPI") — RESOLVED.**
This was the load-bearing V2 cross-artefact defect: p1f §2.2.1 originated the claim
"A retired-instruction CPI below 1.0 is physically impossible on M5," which four
sibling artefacts (P1-A/B/C and the consolidated narrative) cited as authority, while
P1-D §3.1 (correctly) rebutted it but over-corrected into "ri_cycles is a proven
4.27 GHz counter." CH4-4 / CH6 prescribed the exact fix: strike "impossible", state
"CPI 0.16-0.28 ⇒ IPC 3.5-6.4 is PHYSICAL on the M5's ~8-wide core; non-load-bearing
not because impossible but because `proc_pid_rusage.ri_cycles` cannot be disambiguated
as dynamic core-cycles vs a wall-proportional scaled tick from this interface alone."
The V3 p1f §2.2.1:322-344 now reads verbatim that resolution: "The V2 characterization
of this as 'physically impossible' was WRONG PHYSICS and is withdrawn here (this row
originated it; the correction is load-bearing). A sub-1.0 CPI is simply IPC > 1 …
IPC of 3.5-6.4 is entirely physical on the Apple M5 Max's ~8-wide out-of-order
P-core … cyc/byte stays NON-LOAD-BEARING … NOT because it is impossible, but because
it is non-disambiguable." The §2.2 posture (:274-284), §4.5 anomaly (:528-540), and
the V3 fold log (:32-46) all carry the corrected ONE-posture reading (instr/byte
primary, cyc/byte co-reported with IPC explicit + non-load-bearing). This is the CH4-4
prescribed fix applied exactly; the cross-artefact contradiction is dissolved and the
pass now carries one consistent c/B posture. ACCEPT.

**V2 CH5-V2-R1 (p1f §2.3 row-2 wrapper line cite `:43` → `:103-105`) — RESOLVED.**
V2 found the `track1_full_parse` wrapper-bucket row cited `css_canon_bench.rs:43` — the
`RusageInfoV5` PMU struct — instead of the wrapper fn. The V3 p1f §2.3:354 now cites
`css_canon_bench.rs:103-105` and adds the correction note (:360-363): "`:43` is the
`RusageInfoV5` PMU struct; the `track1_full_parse` wrapper fn is `:103-105` …
grep-verified. The `:43` cite in V2 was fabricated-precision." I source-verified:
`css_canon_bench.rs:103-106` IS `fn track1_full_parse(input: &str) -> u64 { let out =
css_decl::parser::parse_full(input).expect(...); black_box(out.len() ...) }`, and `:43`
IS `struct RusageInfoV5`. The corrected cite is exactly right; symbol + %self were
unchanged. ACCEPT.

Both V2 dispositions folded into the V3 dispatch of their owning agent (p1f) with the
correction annotated in the fold log — no orphan REVISE/REJECT carries from V2.

---

## §4 — Cross-lens / convergence notes

- **No REJECT, no REVISE this cycle.** All 11 disposed sections ACCEPT. The two
  load-bearing V2 items (the CPI physics root + the wrapper line cite) are surgically
  resolved with the corrected text in place and source-verified.
- **CH7 ACCEPT rate this cycle: 11/11 = 100%** (V1 77.8%, V2 100%, V3 100%). This is
  the THIRD CH7 cycle and the SECOND consecutive at 100% — CH7 clears the §3Z
  two-consecutive-cycle ≥95% bar on its own axis. The orchestrator must confirm the
  same across the consolidated CH1-CH7 set; CH7 alone does not satisfy the pass-level
  criterion, and the V2 CH4-4/CH6 root was a CROSS-artefact defect whose resolution
  this cycle should be re-checked by CH4/CH6 at V3 (it lived in p1f §2.2.1 + four
  sibling cites; this lens verified the p1f origin is fixed, but the sibling cites in
  p1a/p1b/p1c that referenced the false "impossible" framing must be confirmed
  re-worded by CH4/CH6 — flagged for the consolidator).
- **Overlap with CH1 (CORRECTNESS):** the wrapper line-cite accuracy (V2 CH5-V2-R1) is
  CH1/CH5-adjacent; flagged for the consolidator to dedupe — it is resolved, so no
  double-count of an open defect.
- **Overfit verdict:** the S-P1 CSS profile is honest on every CH7 axis. The single
  largest overfit hazard — reporting the recognition-only `parse_full` "BEATS
  lightningcss" as the SK-V17 typed result — remains uniformly and explicitly contained
  across all six artefacts (A6). The bench is N>=50 cold (code-enforced — the assert
  FIRES — and reproduced), non-broadcast (per-corpus×per-workload, 16 ROWs verified in
  my run), non-short-circuiting (real scan, `.expect` on all corpora, no
  `validate_fixture_shape` on the bar), the comparator is fair standalone full-CSSOM
  (verified NOT the fixture-shaped `lightningcss_facts:636` projection path), there is
  zero crates/core wrong-tree leakage, the five-harness ambiguity is resolved to one
  canonical harness with a demonstrated comparability caveat, the prior "~70/14×"
  narrative is falsified rather than laundered, and the V2 CPI physics error that
  threatened the cost-density honesty is withdrawn with correct physics. No honesty
  defect remains.

---

## §5 — Sources

- Harness source (read end-to-end, 403 lines): `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
  — `:146-177` sample loop (cold per-parse), `:250` N>=50 gate (FIRED this review at
  N=10), `:261-277` per-corpus×per-workload loop (no broadcast), `:103-106`
  `track1_full_parse` wrapper, `:108-111` `track1_fact_stream`, `:113-116` lightningcss
  standalone full-CSSOM, `:118-121` cssparser token-scan, `:43` `RusageInfoV5` struct.
- Conflated comparator (verified excluded): `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:636
  lightningcss_facts` — `validate_fixture_shape` + CSSOM build + projection walk +
  `expected_fixture_projection` compare + `fixture_sidecar_facts` (fixture-shaped, NOT
  the canonical bar); `:596 track1_facts` (the benched fact-stream path).
- Retired broadcast: `skinny/crates/bbnf-bench/src/css_l4_w8.rs:217` — one `time_loop`
  over `W8_PROFILE_ITERS × TRACK1_PROFILES × sources` → one aggregate tuple.
- Benched grammar leaf: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/
  generated.rs:288-311` `find_component_delim` (source-verified: `:293` loop test,
  `:294` byte load, `:295` `delimiters.contains(&byte)` scan leaf, `:298` `pos = match
  byte` dispatch, `:307` `_ => pos + 1`); `:5` emit_fact_stream, `:53-59`
  CssFullParseSummary 4-field, `:61` emit_full_parse.
- Corpus on disk: `skinny/corpora/css-l4-sk-v14/` — animate 71750, bootstrap 232803,
  material-components-web 495454, tailwindcss 179631 = **979638** aggregate (`wc -c`
  verified this review).
- Independent reproduction: `cargo build --release -p bbnf-bench --bin css_canon_bench`
  (exit 0) + `./target/release/css_canon_bench 60` (this review, 2026-05-29, Apple M5
  Max, host=arm64): 16 ROWs emitted; full_parse BEATS lightningcss on all 4 (bootstrap
  2.09×, tailwind 2.89×, material 1.95×, animate 1.75×); fact_stream below on all 4
  (0.52-0.78×); `css_canon_bench 10` panics at `:250` (N>=50 gate fires).
- V2 fold verification: `p1f-bench-canonical.md:32-46` (V3 fold log), `:313-344` (§2.2.1
  CPI physics corrected), `:354,360-363` (§2.3 row-2 wrapper cite corrected).
- V2 disposition source: `hardening/V2/CH4.md:125,128,137-138,158`, `hardening/V2/CH6.md:50-53`,
  `hardening/V2/CH7.md` (11/11 ACCEPT, two V1 items resolved).
- Reviewed artefacts: `restart/skinny/tranches/sk-v17/research/p1/p1{a..f}.md` (all V3).
