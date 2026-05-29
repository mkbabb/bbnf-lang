# CH7 OVERFIT-PRUNE (V2) — S-P1 PROFILE hardening review

Lens: CH7 OVERFIT-PRUNE. Cycle: V2. Date: 2026-05-29.
Reviewer scope (PASS-1-PROFILE §3 lens-extension; ORCHESTRATOR §3W): the canonical
bench is honest — N>=50, no single-sample, no broadcast, no fixture short-circuit;
lightningcss is the fair full-CSSOM bar; no wrong-tree (crates/core) measurement; the
"BEATS lightningcss" headline must not masquerade as a typed-gate win.
Subject artefacts: `restart/skinny/tranches/sk-v17/research/p1/p1{a..f}.md` (all now V2).
Baseline: SK-V17-open (`6496fecae`). Host: aarch64-apple-darwin, Apple M5 Max.

Method: re-read PASS-1-PROFILE §2.1/§3/§8 + the SK-V17 contract; read the canonical
harness source end-to-end (`css_canon_bench.rs`), the benched grammar `generated.rs`
(`find_component_delim`, `emit_full_parse`, `emit_fact_stream`, `emit_declarations`,
`push_ascii_lower_hex`), the corpus loader (`css_l4_corpus.rs`); independently rebuilt
(`cargo build --release -p bbnf-bench --bin css_canon_bench`, exit 0) and re-ran the
harness at N=60; grepped all six V2 artefacts for the overfit signatures this lens
catches (wrong-tree crates/core, broadcast, single-sample, fixture short-circuit,
plane-mask headline laundering); verified the two V1 CH7 REVISE items folded.

---

## §1 — Disposition table (path:line + concrete fix)

| # | Artefact:section | Concern | Disposition | Fix |
|---|---|---|---|---|
| 1 | p1f:59-101 (harness §1.1) / `css_canon_bench.rs:146-177,250` | N>=50 honesty, cold-per-parse, no warm | **ACCEPT** | none — code-verified + reproduced |
| 2 | p1f §2.1:200-217 (16 rows) / `css_canon_bench.rs:261-264` | no broadcast; per-corpus×per-workload split | **ACCEPT** | none — verified loop, no aggregate tuple |
| 3 | p1f:86-91 / `css_canon_bench.rs:113-116` | lightningcss = standalone full-CSSOM, not conflated facts | **ACCEPT** | none — verified standalone `StyleSheet::parse` |
| 4 | p1f §2.3:363-383 | lightningcss comparator genuinely materializes | **ACCEPT** | none — ~38% tokenizer + ~30% typed build/drop |
| 5 | (all six) | wrong-tree crates/core measurement | **ACCEPT** | none — zero crates/core measurement citations |
| 6 | p1a:115/174, p1c:188/352, p1e:162/299, p1f:298/442 | "BEATS lightningcss" plane-mask laundering | **ACCEPT** | none — uniformly marked recognition-only / masking |
| 7 | p1f:26-30, p1a:11, p1e:18 (4/4 CSS coverage) | §2.1 17-corpus mandate vs CSS subject | **ACCEPT** | none — correctly scoped to CSS-tape subject; 4 regular corpora |
| 8 | p1f §1.1.1:107-144 (X2 single-harness) | five competing "canonical" harnesses | **ACCEPT** | none — single canonical harness designated, comparability caveat stated + demonstrated |
| 9 | p1f §3.2:413-445 ("~70/14×" falsification) | prior narrative laundered as benched truth | **ACCEPT** | none — classified N-direct, no fresh antecedent |
| 10 | **V1 R1** — p1d §2.3 "metadata-only" fact-stream | misstated benched-plane cost class | **ACCEPT** | resolved — p1d:235-241 now states fact_stream IS the most-expensive plane |
| 11 | **V1 R2** — p1f `find_component_delim` line swap | transposed scan-leaf line cites | **ACCEPT** | resolved — p1f:316 + §1.3:174-184 now correct, source-verified |

Counts: **ACCEPT 11, REVISE 0, REJECT 0** (11 disposed sections). CH7 ACCEPT rate
this artefact-set: **11/11 = 100%**.

---

## §2 — ACCEPT findings (verified, load-bearing)

**A1 — The bench is statistically honest (PRIMARY lens obligation).**
`css_canon_bench.rs:250` carries `assert!(n >= 50, "N must be >= 50 (SK-V17
telemetry-honesty gate)")`; default N=200 (`:249`). `sample()` (`:146-177`) takes N
cold per-parse samples: each sample times exactly one `parse(black_box(input))` call
(`Instant::now()` … `elapsed()`, `:154-156`), black-boxes and drops the result
(`:157`), reuses no parser state across samples. The single touch outside the timed
window (`:152`) defeats first-touch page-fault contamination of the SOURCE buffer
only — the parse itself is cold per sample, satisfying the `no-warm-benches`
cold-per-parse contract. Median/min/max/stddev reported (`:160-176`). The W6
`W6_SAMPLE_COUNT=1` single-sample harness and the W8 broadcast loop
(`css_l4_w8.rs:217`, one timed loop over `total_bytes × 7 grammars × 8 iters → ONE
tuple`) are explicitly retired (p1f:61-64). **Independently reproduced at N=60** this
review (2026-05-29, Apple M5 Max): full_parse BEATS lightningcss on all 4 corpora
(bootstrap 2029/940=2.16×, tailwind 2265/706=3.21×, material 2310/1129=2.05×, animate
2016/1070=1.88×); fact_stream BELOW lightningcss on all 4 (0.62-0.75×). My absolute
medians run lower than p1f's N=200 (host scheduling noise) but every ratio and every
verdict holds — exactly the within-harness comparability caveat p1f §1.1.1 names.

**A2 — No broadcast.** `css_canon_bench.rs:261-264` is a `for corpus in &corpora { for
(name, parse) in WORKLOADS { let s = sample(...) } }` nest emitting one `ROW` per
corpus×workload (`:265`). Sixteen distinct rows (4 corpora × 4 workloads), each with
its own N median/min/max/stddev — no single aggregate tuple, no broadcast of one
measurement across corpora. The W8R 24-row broadcast (one tuple, N effectively 1
timed loop) is retired and faithfully reproduced per-corpus (p1f §3.1:402-410).

**A3 — No fixture short-circuit.** `track1_full_parse` → `parse_full` →
`emit_full_parse` (`generated.rs:61`) runs a real `while pos < self.bytes.len()` byte
scan (`find_component_delim` :293-310, `consume_balanced_at` :320-340).
`track1_fact_stream` → `parse` → `emit_fact_stream` → `emit_declarations`
(`generated.rs:5→45→411`) runs a real per-declaration scan with hex-encoding
(`push_ascii_lower_hex` :628). Both are content-dependent (my N=60 rates vary
per-corpus). The `.expect(...)` on every workload (`css_canon_bench.rs:104,109,114,
119`) means a parse error panics — none did, so all 4 corpora parse cleanly on all 4
workloads (the comparator is not spuriously fast from early-erroring). The corpus
loader reads real sha256-pinned files from `corpora/css-l4-sk-v14/`
(`css_l4_corpus.rs:27-52`: 232803/179631/495454/71750 = 979638, `ls -la` confirmed
on disk; aggregate matches p1f §1.3:185-187).

**A4 — lightningcss is the fair >SOTA bar, NOT the conflated facts path.**
`css_canon_bench.rs:113-116` calls `lightningcss::stylesheet::StyleSheet::parse(input,
ParserOptions::default())` + `black_box(sheet.rules.0.len())` — the standalone
full-CSSOM materialization. This is verified NOT the criterion harness's conflated
`lightningcss_facts` (`nonjson_css_l4.rs:636`, which builds CSSOM AND then walks it
into a projection String); the harness imports `use lightningcss::stylesheet::{...
StyleSheet}` (`:35`) and times the CSSOM build only (p1f:86-91). The flame
attribution (p1f §2.3:363-383) proves genuine materialization: ~38% cssparser
tokenizer + ~30% building+dropping typed `Property`/`Selector`/`CssRule` nodes — a
true full-CSSOM build, the correct bar per SYNTHESIS §0.6.

**A5 — No wrong-tree (crates/core) measurement.** `grep crates/core` across all six
V2 artefacts returns ZERO measurement citations. Every benched symbol resolves to
`skinny/crates/...` (runtime grammar `generated.rs`, bbnf-bench harness, tape
assembler). crates/core is the SK-V18 totality-fold target and is correctly NOT
profiled here, per the BENCHED-SURFACE contract.

**A6 — The "BEATS lightningcss 2.0-3.6×" headline is NOT laundered into a typed-gate
win (the central overfit hazard, contained).** `track1_full_parse` materializes only
a 4-field `CssFullParseSummary` (rules/at_rules/qualified_rules/declarations,
`generated.rs:53-59`); it allocates no CSSOM, retains no value tree — a
count-the-braces recognition scan. Every artefact that cites the headline marks it
explicitly and uniformly:
- p1a:115 ("materializes NO AST … the recognition skeleton, not the rich typed
  CSSOM"), :174 ("MASKING … if S-P2 reports the recognition number as the typed
  result, that is the exact W6 'summary lane retains nothing' error").
- p1c:188/352-354 ("recognition-only", "masking probe — wrong-plane").
- p1e:162-163 ("recognition-only … NOT preserve-rich-ast"), :299-301 ("a *masking
  probe* … fails preserve-rich-ast"), :348 (rejects "ship recognition-only as
  admission").
- p1f:298 ("recognition-only (4-field summary), NOT preserve-rich-ast, so it does not
  by itself discharge the SK-V17 typed gate"), §3.2 table:442 (Outcome A "admit-
  shaped … but recognition-only").
The discipline is uniform; no agent claims the win. This is the model behaviour the
OVERFIT-PRUNE lens exists to enforce.

**A7 — 4/4 CSS corpus coverage is correctly scoped, not an overfit.** PASS-1-PROFILE
§2.1's 17-corpus mandate binds the JSON profiling subject; SK-V17's subject is the
CSS-tape plane, whose benched corpus set is fixed at 4 real-world production sheets
(bootstrap/tailwind/material/animate, `css_l4_corpus.rs:21-54`, sha256-pinned) per
SYNTHESIS §0.5. These are regular shipping corpora — "no contrivance, >=1 regular
corpus" satisfied. Not a float-heavy or string-light cherry-pick; all four exercise
the same delimiter/balance scan (the 59% `find_component_delim` leaf holds across all
four, p1f §2.3). Every artefact states the scoping justification (p1f:26-30, p1a:11,
p1e:18). JSON 51/51 is kept as a guard tripwire (p1f §4.7), not a P1-F measurement.

**A8 — X2 single-harness convergence is resolved honestly.** V1 surfaced five
competing "canonical N>=50" harnesses (p1f §1.1.1:112-118). p1f, as the bench/
measurement authority, designates `css_canon_bench.rs` as THE single canonical
harness on objective criteria (the only one that simultaneously asserts N>=50 at
`:250` grep-verified, carries the PMU instr/byte mode, carries the samply driver, and
is cited with correct line numbers) and declares the other four superseded
(:126-136). Critically, it states and DEMONSTRATES the comparability caveat: only
within-harness same-run ratios are load-bearing; absolute Mbps drift ≤17% across
runs by host noise (§2.1.1:229-246). My independent N=60 re-run confirms this — the
ratios held, the absolutes drifted ~12-15% below p1f's N=200. This is the honest
posture: the gate signal is the same-run ratio, not the absolute median.

**A9 — The "~70 Mbps / ~14×" prior narrative is falsified, not laundered.** p1f §3.2
classifies the contract-supplied "~70 Mbps, ~14× slower than lightningcss" as
**N-direct (no fresh benched antecedent)**: the benched full-parse plane is
2327-2727 Mbps (BEATS), the fact-stream plane 549-882 (below), neither is ~70; the
only ~3 Mbps figure is the pre-blocked eager-typed retime (AZ-IV, SYNTHESIS §0.4).
This is the profile-first non-negotiable (ORCHESTRATOR §8) applied correctly — the
prior number is not carried as truth without a fresh hot-leaf antecedent, and the
honest two-planed benched reality is stated instead.

---

## §3 — V1 REVISE items: folded + verified (orphan-free)

**V1 R1 (p1d "metadata-only" fact-stream mischaracterization) — RESOLVED.** V1 found
p1d §2.3 wrongly called the benched fact-stream plane "metadata-only … does not even
run the scan … even cheaper." The V2 p1d (Cycle: V2) now reads at :235-241:
"`track1_fact_stream` (= `parser::parse` = `emit_fact_stream`, `generated.rs:5`) is
NOT metadata-only and IS a full per-declaration byte scan. [CORRECTED from V1 §2.3,
which wrongly stated it 'does not even run the scan' / 'is metadata-only'.] … is
**234-370 i/B** (§3.2) — the **most expensive of the four planes**." The fact-stream
plane is now correctly characterized as the most-expensive String-tax plane
throughout (p1d:408, :436, :479-480). The fix is folded with the V1 correction
explicitly annotated. ACCEPT.

**V1 R2 (p1f `find_component_delim` line swap) — RESOLVED.** V1 found the scan-leaf
line cites transposed (`:298` vs `:295`). The V2 p1f §2.3:316 now reads "hot at **:295
`delimiters.contains(&byte)`** (the byte-membership scan leaf), `:293` loop test,
`:294` byte load, `:298` `pos = match byte` dispatch, `:307` `_ => pos + 1`," with a
V2 fold-log entry (:33-38) and a §1.3 grep verification (:174-184). I source-verified
against `generated.rs:288-311`: `:293` IS `while pos < self.bytes.len()`, `:294` byte
load, `:295` IS `if delimiters.contains(&byte)`, `:298` IS `pos = match byte`, `:307`
IS `_ => pos + 1`. The corrected attribution is exactly right. ACCEPT.

Both V1 REVISE items folded into the V2 dispatch of their owning agents (p1d, p1f)
with the correction annotated — no orphan REVISE carries from V1.

---

## §4 — Cross-lens / convergence notes

- **No REJECT, no REVISE this cycle.** All 11 disposed sections ACCEPT. The two V1
  REVISE items are surgically resolved with the corrected text in place and
  source-verified.
- **CH7 ACCEPT rate this cycle: 11/11 = 100%** (V1 was 7/9 = 77.8%). The lens
  converges — two consecutive cycles at ≥95% is the §3Z bar; this is the second
  cycle for CH7 and clears 95%. The orchestrator should confirm the first-consecutive
  precondition across the consolidated set (CH7 alone reaching 100% does not by
  itself satisfy the two-consecutive-cycle pass-level criterion).
- **Overlap with CH1 (CORRECTNESS):** R2/R1 line-attribution accuracy is CH1-adjacent;
  flagged for the consolidator to dedupe — both are resolved, so no double-count of an
  open defect.
- **Overfit verdict:** the S-P1 CSS profile is honest on every CH7 axis. The single
  largest overfit hazard — reporting the recognition-only `parse_full` "BEATS
  lightningcss" as the SK-V17 typed result — is uniformly and explicitly contained
  across all six artefacts (A6). The bench is N>=50 cold (code-enforced + reproduced),
  non-broadcast (per-corpus×per-workload), non-short-circuiting (real scan, .expect on
  all corpora), the comparator is fair standalone full-CSSOM (not the conflated facts
  path), there is zero crates/core wrong-tree leakage, the five-harness ambiguity is
  resolved to one canonical harness with a demonstrated comparability caveat, and the
  prior "~70/14×" narrative is falsified rather than laundered. No honesty defect
  remains.

---

## §5 — Sources

- Harness source (read end-to-end): `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`
  — `:146-177` sample loop (cold per-parse), `:250` N>=50 gate, `:261-264` per-corpus
  ×per-workload loop (no broadcast), `:113-116` lightningcss standalone full-CSSOM,
  `:35` lightningcss import, `:118-121,282-303` cssparser token-scan probe.
- Benched grammar: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/
  generated.rs:5` (emit_fact_stream), `:45` (emit_declarations call), `:53-59`
  (CssFullParseSummary 4-field), `:61` (emit_full_parse), `:288-311`
  (find_component_delim; loop test :293, byte load :294, scan leaf :295
  `delimiters.contains`, dispatch :298, advance :307 — source-verified this review),
  `:320-340` (consume_balanced_at), `:411` (emit_declarations scan), `:628`
  (push_ascii_lower_hex).
- Corpus loader: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-54` (4 sha256-pinned
  corpora); files present on disk under `skinny/corpora/css-l4-sk-v14/`
  (232803/179631/495454/71750 = 979638 aggregate, verified).
- Independent reproduction: `cargo build --release -p bbnf-bench --bin css_canon_bench`
  (exit 0) + `./target/release/css_canon_bench 60` (this review, 2026-05-29, Apple M5
  Max, arch=arm64): full_parse BEATS lightningcss on all 4 (bootstrap 2.16×, tailwind
  3.21×, material 2.05×, animate 1.88×); fact_stream below on all 4 (0.62-0.75×).
- V1 fold verification: `p1d-pmu-cycles.md:235-241` (R1 corrected), `p1f-bench-
  canonical.md:33-38,316,174-184` (R2 corrected).
- Reviewed artefacts: `restart/skinny/tranches/sk-v17/research/p1/p1{a..f}.md` (all V2).
