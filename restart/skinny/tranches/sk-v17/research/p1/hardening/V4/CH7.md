# CH7 OVERFIT-PRUNE (V4) — S-P1 PROFILE hardening review

Lens: CH7 OVERFIT-PRUNE. Cycle: V4. Date: 2026-05-29.
Reviewer scope (PASS-1-PROFILE §3 lens-extension; ORCHESTRATOR §3W): the canonical
bench is honest — N>=50, no single-sample, no broadcast, no fixture short-circuit;
lightningcss is the fair full-CSSOM bar; no wrong-tree (crates/core) measurement; the
"BEATS lightningcss" headline must not masquerade as a typed-gate win.
Subject artefact (bench/measurement row, dispatch-specialised): `restart/skinny/
tranches/sk-v17/research/p1/p1f-bench-canonical.md` (now V4); cross-checked against
the five sibling profile artefacts `p1{a..e}.md`.
Baseline: SK-V17-open (`6496fecae`). Host: aarch64-apple-darwin, Apple M5 Max.

Method: re-read PASS-1-PROFILE §2.1/§3/§8 + the SK-V17 contract (SYNTHESIS §0.2-0.6);
re-read the V3 CH7 (11/11 ACCEPT) and confirmed the V4 fold log carries every V3
posture and re-grounds on a FRESH V4 measurement run (the profile-first discipline:
re-verify, do not inherit). Re-read the canonical harness source end-to-end
(`css_canon_bench.rs`), the benched grammar leaf (`css_l4_declaration_values/
generated.rs`), the recognition-only summary emitter (`emit_full_parse`,
`generated.rs:61`), and the conflated comparator (`nonjson_css_l4.rs:636
lightningcss_facts`). **Independently FIRED the N<50 gate (panics at `:250`),
independently RE-RAN the harness at N=60 this cycle** (`/tmp/skv17-p1/ch7_v4_repro.txt`),
and re-grepped the four pinned corpora on disk + the crates/core leakage check.

---

## §1 — Disposition table (path:line + concrete fix)

| # | Artefact:section | Concern | Disposition | Fix |
|---|---|---|---|---|
| 1 | p1f §1.1:115-126 (sample) / `css_canon_bench.rs:146-176,250` | N>=50 honesty, cold-per-parse, no warm | **ACCEPT** | none — code-verified + N<50 gate FIRES this cycle (panic at `:250`) + N=60 reproduced |
| 2 | p1f §2.1:261-278 (16 rows) / `css_canon_bench.rs:262-277` | no broadcast; per-corpus×per-workload split | **ACCEPT** | none — verified `for corpus { for (name,parse) in WORKLOADS }` nest; my N=60 run emits exactly 16 ROWs, no aggregate tuple |
| 3 | p1f §1.1:138-145 / `css_canon_bench.rs:113-115` | lightningcss = standalone full-CSSOM, not the conflated facts path | **ACCEPT** | none — verified standalone `StyleSheet::parse`; canon harness has ZERO `validate_fixture_shape` (grep clean); `:636 lightningcss_facts` correctly excluded |
| 4 | p1f §2.3:456-476 | lightningcss comparator genuinely materializes | **ACCEPT** | none — ~38% cssparser tokenizer + ~30% typed Property/Selector build+drop; a true full-CSSOM bar |
| 5 | p1f (whole) + siblings | wrong-tree crates/core measurement | **ACCEPT** | none — `grep crates/core p1f` returns ZERO; every benched symbol resolves to `skinny/crates/...` |
| 6 | p1f §3.2:533-538 + §4.1 (recognition-only) | "BEATS lightningcss" plane-mask laundering | **ACCEPT** | none — `emit_full_parse` verified 4-field summary (`generated.rs:54-58`); marked "A (admit-shaped)... does not by itself discharge the SK-V17 typed gate" |
| 7 | p1f frontmatter:26-30 (4/4 CSS coverage) | §2.1 17-corpus mandate vs CSS subject | **ACCEPT** | none — correctly scoped to CSS-tape subject; 4 regular shipping corpora, not a cherry-pick |
| 8 | p1f §1.1.1:162-199 (X2 single-harness) | five competing "canonical" harnesses | **ACCEPT** | none — single canonical harness designated on objective criteria, comparability caveat stated + demonstrated across THREE runs |
| 9 | p1f §3.2:506-538 ("~70/14×" falsification) | prior narrative laundered as benched truth | **ACCEPT** | none — classified N-direct, no fresh antecedent (ORCHESTRATOR §8 applied) |
| 10 | p1f §2.2.1:360-391 (CPI physics) | the V2-rooted "physically impossible CPI" overfit-of-narrative | **ACCEPT** | none — V3 struck "impossible"; V4 re-confirms IPC 3.6-6.3 PHYSICAL + non-disambiguable on a FRESH PMU run |
| 11 | p1f §2.2:332-358 (instr/byte reproducibility) | is the sole cost-density figure stable, not overfit to one run? | **ACCEPT** | none — V4 re-run reproduces every V3 instr/byte to <0.5% (bootstrap full 53.70=53.70; tailwind fact 364.51 vs 363.76); reproducibility independently plausible |

Counts: **ACCEPT 11, REVISE 0, REJECT 0** (11 disposed sections). CH7 ACCEPT rate
this artefact-set: **11/11 = 100%** (V1 7/9 = 77.8%, V2 11/11 = 100%, V3 11/11 = 100%,
V4 11/11 = 100%). This is the FOURTH CH7 cycle and the THIRD consecutive at 100%.

---

## §2 — ACCEPT findings (independently verified this cycle, load-bearing)

**A1 — The bench is statistically honest (PRIMARY lens obligation), re-fired fresh.**
`css_canon_bench.rs:250` carries `assert!(n >= 50, "N must be >= 50 (SK-V17
telemetry-honesty gate)")` — and I FIRED it this cycle:
`./target/release/css_canon_bench 10` panics at `css_canon_bench.rs:250` with that
exact message. The gate is real, not decorative. `sample()` (`:146-176`) takes N cold
per-parse samples: one untimed touch (`:152`) defeats SOURCE-buffer first-fault, then
each timed sample times exactly one `parse(black_box(input))` (`Instant::now()` at
`:154`, `start.elapsed()` at `:156`), black-boxes and drops the result (`:157`), reuses
no parser state across samples — the parse itself is cold per sample, satisfying the
`no-warm-benches` cold-per-parse contract. The W6 `W6_SAMPLE_COUNT=1` single-sample
harness and the W8 broadcast loop (`css_l4_w8.rs:217`) are explicitly retired (p1f
§1.1:110-113, §3.1:489-504). **Independently reproduced at N=60 this review**
(`/tmp/skv17-p1/ch7_v4_repro.txt`, 2026-05-29, Apple M5 Max): full_parse BEATS
lightningcss on all 4 corpora (bootstrap 2114.688/1096.701 = **1.93×**, tailwind
2658.431/819.181 = **3.25×**, material 2458.576/1272.179 = **1.93×**, animate
2495.652/1210.864 = **2.06×**); fact_stream BELOW lightningcss on all 4 (bootstrap
844.366/1096.701 = 0.77×, tailwind 0.69×, material 0.68×, animate 0.60×). My absolute
medians sit within the host-noise band of p1f's N=200 (§2.1: 2.05/3.09/2.05/2.01×) and
every ratio + every verdict holds — exactly the within-harness comparability caveat
p1f §1.1.1 names. The V4 "BEATS" headline is a REAL same-run ratio, not a fabricated one.

**A2 — No broadcast (re-verified by row-count).** `css_canon_bench.rs:262-277` is the
`for corpus in &corpora { for (name, parse) in WORKLOADS { let s = sample(...) } }`
nest emitting one `ROW` per corpus×workload (`:266-276`). My N=60 run emits exactly
**16 distinct ROW lines** (`grep -c '^ROW'` = 16), 4 corpora × 4 workloads, each with
its own N median/min/max/stddev — no aggregate tuple. The W8R 24-row broadcast (one
`time_loop` over `total_bytes × 7 grammars × 8 iters` → one tuple, N effectively 1) is
retired and faithfully reproduced per-corpus (p1f §3.1). The X2 single-harness verdict
(§1.1.1) names `css_canon_bench.rs` as THE harness on objective criteria
(asserts N>=50, carries the PMU + samply modes, cited with correct lines) and declares
the four competing "canonical" harnesses superseded.

**A3 — No fixture short-circuit (re-verified).** `track1_full_parse` → `parse_full` →
`emit_full_parse` (`generated.rs:61`) runs a real `while pos < self.bytes.len()` byte
scan: `find_component_delim` (`:288-310`, source-verified this cycle — `:293` loop
test, `:294` byte load, `:295` `if delimiters.contains(&byte)` scan leaf, `:298`
`pos = match byte` dispatch, `:307` `_ => pos + 1` advance). `track1_fact_stream` →
`parse` → `emit_fact_stream` (`generated.rs:5`) runs a real per-declaration scan with
hex emission. Both are content-dependent (my N=60 rates vary per corpus: full_parse
2114-2658, fact_stream 566-868). Every workload `.expect(...)`s
(`css_canon_bench.rs:104,109,114,119`) — a parse error would panic; none did, so all 4
corpora parse cleanly on all 4 workloads (no comparator is spuriously fast from
early-erroring). Crucially the canonical harness's lightningcss workload does **NOT**
call `validate_fixture_shape` (grep `validate_fixture_shape|expected_fixture_projection|
fixture_sidecar` on `css_canon_bench.rs` returns NONE) — contrast `nonjson_css_l4.rs:637`,
which gates the conflated path on the fixture shape. There is no fixture short-circuit
on the bar. Corpus loader reads the real sha256-pinned files from
`corpora/css-l4-sk-v14/` (verified on disk: animate 71750, bootstrap 232803, material
495454, tailwind 179631 = **979638**; the `total` 981623 folds the 1985-byte
`manifest.md`, p1f §1.3 states this correctly).

**A4 — lightningcss is the fair >SOTA bar, NOT the conflated facts path.**
`css_canon_bench.rs:113-115` calls `StyleSheet::parse(input, ParserOptions::default())`
+ `black_box(sheet.rules.0.len())` — standalone full-CSSOM materialization. The
conflated alternative `nonjson_css_l4.rs:636 lightningcss_facts` does
`validate_fixture_shape` → CSSOM build → projection-Vec walk → fixture-projection
compare → sidecar-emit, which would NOT be a fair bar; p1f §1.1:138-143 correctly names
this distinction and times the CSSOM build only. The flame attribution (p1f
§2.3:456-476) proves genuine materialization: ~38% cssparser tokenizer + ~30%
building+dropping typed `Property`/`Selector`/`CssRule` nodes — a true full-CSSOM build,
the correct bar per SYNTHESIS §0.6.

**A5 — No wrong-tree (crates/core) measurement.** `grep crates/core
p1f-bench-canonical.md` returns ZERO. Every benched symbol resolves to
`skinny/crates/...` (runtime grammar `generated.rs`, bbnf-bench harness, tape
assembler). crates/core is the SK-V18 totality-fold target and is correctly NOT
profiled here, per the BENCHED-SURFACE contract.

**A6 — The "BEATS lightningcss ~2.0-3.1×" headline is NOT laundered into a typed-gate
win (the central overfit hazard, contained).** `track1_full_parse` materializes only a
4-field `CssFullParseSummary` (`generated.rs:54-58`: rules/at_rules/qualified_rules/
declarations); `emit_full_parse` (`:91-98`) serializes those four counts and the
harness black-boxes `out.len()` — a count-the-rules recognition scan, no CSSOM, no
value tree, no preserve-rich-ast. p1f marks this uniformly: §1.1:133 ("Emits a 4-field
summary"), §3.2 outcome table:535 ("A (admit-shaped) — already > bar; but it is
recognition-only (4-field summary), NOT preserve-rich-ast, so it does not by itself
discharge the SK-V17 typed gate"), §4.1:544-550 ("The full-parse plane proves the
*scanner* is not the bottleneck for >SOTA; the fact-stream plane proves the *String
emission* is"). No agent claims the typed win from the recognition number. This is the
exact behaviour the OVERFIT-PRUNE lens exists to enforce, and it is contained.

**A7 — 4/4 CSS corpus coverage is correctly scoped, not an overfit.**
PASS-1-PROFILE §2.1's 17-corpus mandate binds the JSON profiling subject; SK-V17's
subject is the CSS-tape plane, whose benched set is fixed at 4 real-world production
sheets (bootstrap/tailwind/material/animate, `css_l4_corpus.rs:21-54`, sha256-pinned)
per SYNTHESIS §0.5 — "no contrivance, >=1 regular corpus" satisfied. Not a float-heavy
or string-light cherry-pick; all four exercise the same delimiter/balance scan (the 59%
`find_component_delim` leaf holds across all four, and my N=60 full_parse rates are
tight 2114-2658 across the set). p1f frontmatter :26-30 states the scoping. JSON 51/51
is kept as a guard tripwire (§4.7), not a P1-F measurement.

**A8 — `tape_activated = false` for CSS is the honest baseline, not a contrivance.**
`grep -rln "TapeBuilder|ValueRef|PayloadArena|crate::tape"
crates/runtime/src/grammars/css_l4_declaration_values/` returns ZERO this cycle — the
benched CSS planes touch NO tape symbol, confirming p1f §4.4. The "BEATS" headline is
therefore NOT a tape-activated number borrowed from a not-yet-landed substrate; it is
the recognition-only scanner on the bare fact-stream tree. This is the correct
close-gate baseline, and it removes the hazard of crediting SK-V17's unbuilt tape lever
with a number it did not earn.

**A9 — The "~70 Mbps / ~14×" prior narrative is falsified, not laundered.** p1f §3.2
classifies the contract-supplied "~70 Mbps, ~14× slower than lightningcss" as
**N-direct (no fresh benched antecedent)**: the benched full-parse plane is 2273-2590
Mbps (BEATS), fact-stream 559-875 (below), neither is ~70; the only ~3 Mbps figure is
the pre-blocked eager-typed retime (`sk-v16-w6-speed-report.md:164`, AZ-IV, SYNTHESIS
§0.4). This is the profile-first non-negotiable (ORCHESTRATOR §8) applied correctly —
the embarrassing prior number is not silently inherited and is not laundered into the
new result.

---

## §3 — V3 dispositions: carry-forward + fresh re-verification (orphan-free)

V3 CH7 returned **11/11 = 100% ACCEPT, 0 REVISE, 0 REJECT** against this artefact-set;
the whole-pass V3 consolidation returned 42/42 = 100% (V4 fold log p1f:32-58). There
is therefore **NO open REVISE or REJECT against this artefact to fold**. The CH7
obligation this cycle is to confirm V4 did not silently inherit V3's ACCEPT but
re-grounded on a fresh measurement (profile-first discipline). Verified:

- **Fresh measurement run.** V4 §2.1 cites `/tmp/skv17-p1/css_canon_n200_v4.txt` and
  §2.2 cites `…_pmu_v4.txt` — distinct V4 run ids, not the V2/V3 files. My independent
  N=60 run this cycle (a third independent firing) reproduces the BEATS ratios and the
  16-ROW shape, confirming the V4 numbers are live, not transcribed.
- **The V2-rooted CPI-physics correction holds in V4.** §2.2.1:360-391 re-confirms
  "sub-1.0 CPI is high IPC (3.6-6.3), PHYSICAL on the ~8-wide M5 P-core ... cyc/byte
  NON-LOAD-BEARING because `proc_pid_rusage.ri_cycles` is non-disambiguable, NOT because
  it is impossible." This was the load-bearing V2 cross-artefact defect (the
  "physically impossible" framing four siblings cited); V3 struck it; V4 carries the
  corrected physics on a fresh PMU run. From the OVERFIT-PRUNE axis this matters because
  an impossible-counter claim, left standing, would have been an overfit-of-narrative
  (a fabricated physical bound used to dismiss a real measurement). It is correctly
  withdrawn.
- **The row-2 wrapper line cite stays correct.** §2.3:406,412-414 cites
  `css_canon_bench.rs:103-105` for the `track1_full_parse` wrapper (source-verified this
  cycle: `:103 fn track1_full_parse`, body :103-106; `:43 struct RusageInfoV5`). The
  V2 `:43` fabricated-precision cite remains corrected.

No orphan REVISE/REJECT carries from any prior cycle.

---

## §4 — Cross-lens / convergence notes

- **No REJECT, no REVISE this cycle.** All 11 disposed sections ACCEPT. Every
  load-bearing honesty claim independently re-verified against source + a fresh N=60
  run, not inherited from V3.
- **CH7 ACCEPT rate this cycle: 11/11 = 100%** (V1 77.8%, V2 100%, V3 100%, V4 100%).
  This is the THIRD consecutive CH7 cycle at 100% — CH7 clears the §3Z
  two-consecutive-cycle ≥95% bar on its own axis with margin. The orchestrator must
  confirm the same across the consolidated CH1-CH7 set; CH7 alone does not satisfy the
  pass-level criterion.
- **Overlap notes for the consolidator:** the recognition-only containment (A6) is
  CH2-GENERALITY-adjacent (the plane must name the grammar-neutral primitive, which it
  does: `find_component_delim` is a byte-class delimiter scan, the same class as
  JSON `scan.rs:219`); the no-sidecar / no-second-substrate confirmation (p1f §4.6) is
  CH5-HIDDEN-COUPLING territory — flagged for dedupe, both resolved, no double-count of
  an open defect.
- **Overfit verdict:** the S-P1 CSS profile is honest on every CH7 axis, fresh-verified.
  The single largest overfit hazard — reporting the recognition-only `parse_full` "BEATS
  lightningcss" as the SK-V17 typed result — is uniformly and explicitly contained (A6),
  and is reinforced by `tape_activated=false` (A8: the number is not borrowed from an
  unbuilt substrate). The bench is N>=50 cold (code-enforced — the assert FIRES — and
  reproduced at N=60), non-broadcast (16 ROWs verified in my run), non-short-circuiting
  (real scan, `.expect` on all corpora, no `validate_fixture_shape` on the bar), the
  comparator is fair standalone full-CSSOM (verified NOT the fixture-shaped
  `lightningcss_facts:636` path), there is zero crates/core wrong-tree leakage, the
  five-harness ambiguity is resolved to one canonical harness with a demonstrated
  comparability caveat across three runs, the prior "~70/14×" narrative is falsified
  rather than laundered, and the V2 CPI-physics overfit-of-narrative is withdrawn with
  correct physics on a fresh run. No honesty defect remains.

---

## §5 — Sources

- Harness source (read end-to-end): `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
  — `:146-176` sample loop (cold per-parse), `:250` N>=50 gate (**FIRED this review at
  N=10**, panic confirmed), `:262-277` per-corpus×per-workload loop (no broadcast),
  `:103-106` `track1_full_parse` wrapper, `:108-110` `track1_fact_stream`, `:113-115`
  `lightningcss_full_cssom` standalone `StyleSheet::parse`, `:118-120` cssparser
  token-scan, `:123-127` WORKLOADS array, `:43` `RusageInfoV5` struct; grep of
  `validate_fixture_shape|expected_fixture_projection|fixture_sidecar` returns NONE.
- Recognition-only emitter: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/
  generated.rs:54-58` `CssFullParseSummary` (4 fields), `:61` `emit_full_parse`,
  `:91-98` summary serialization; `:5` `emit_fact_stream`.
- Benched scan leaf (source-verified): same file `:288-310` `find_component_delim`
  (`:293` loop test, `:294` byte load, `:295` `delimiters.contains(&byte)` scan leaf,
  `:298` `pos = match byte`, `:307` `_ => pos + 1`); `:628` `push_ascii_lower_hex`
  (`push_hex` at `:633`).
- tape inactive for CSS: `grep -rln "TapeBuilder|ValueRef|PayloadArena|crate::tape"
  crates/runtime/src/grammars/css_l4_declaration_values/` = ZERO.
- Conflated comparator (verified excluded): `skinny/crates/bbnf-bench/src/
  nonjson_css_l4.rs:636 lightningcss_facts` (fixture-shaped); `:596 track1_facts`.
- Corpus on disk: `skinny/corpora/css-l4-sk-v14/` — animate 71750, bootstrap 232803,
  material-components-web 495454, tailwindcss 179631 = **979638** aggregate.
- Independent reproduction: `cargo build` artefact present;
  `./target/release/css_canon_bench 60` → `/tmp/skv17-p1/ch7_v4_repro.txt` (16 ROWs;
  full_parse BEATS lightningcss on all 4 — bootstrap 1.93×, tailwind 3.25×, material
  1.93×, animate 2.06×; fact_stream below on all 4 — 0.60-0.77×); `css_canon_bench 10`
  panics at `:250` (N>=50 gate fires).
- Reviewed artefact: `restart/skinny/tranches/sk-v17/research/p1/p1f-bench-canonical.md`
  (V4); prior `restart/skinny/tranches/sk-v17/research/p1/hardening/V3/CH7.md`
  (11/11 ACCEPT).
