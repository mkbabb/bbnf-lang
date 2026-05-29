# CH7 OVERFIT-PRUNE (V1) — S-P1 PROFILE hardening review

Lens: CH7 OVERFIT-PRUNE. Cycle: V1. Date: 2026-05-29.
Reviewer scope (PASS-1-PROFILE §3 lens-extension; ORCHESTRATOR §3W): the canonical
bench is honest — N>=50, no single-sample, no broadcast, no fixture short-circuit;
lightningcss is the fair full-CSSOM bar; no wrong-tree (crates/core) measurement;
the "BEATS lightningcss" headline is not allowed to masquerade as a typed-gate win.
Subject artefacts: `restart/skinny/tranches/sk-v17/research/p1/p1{a..f}.md`.
Baseline: SK-V17-open (`6496fecae`). Host: aarch64-apple-darwin, Apple M5 Max.

Method: independently rebuilt and re-ran the canonical harness
(`cargo build --release -p bbnf-bench --bin css_canon_bench`;
`./target/release/css_canon_bench 60`), read the harness source, the benched
grammar `generated.rs`, the corpus loader, and grepped all six artefacts for the
overfit signatures this lens exists to catch (wrong-tree, broadcast, single-sample,
fixture short-circuit, plane-mask headline laundering).

---

## §1 — Disposition table (path:line + concrete fix)

| # | Artefact:line | Concern | Disposition | Fix |
|---|---|---|---|---|
| 1 | p1f-bench-canonical.md:34-75 (harness §1.1) | N>=50 honesty | **ACCEPT** | none — verified |
| 2 | p1f §2.1 table:113-130 (16 rows) | no broadcast / per-corpus split | **ACCEPT** | none — verified |
| 3 | p1f:60-66 / css_canon_bench.rs:113-116 | lightningcss = fair full-CSSOM | **ACCEPT** | none — verified |
| 4 | p1f:219-239 (lightningcss attribution) | comparator genuinely materializes | **ACCEPT** | none — verified |
| 5 | (all six) | wrong-tree crates/core measurement | **ACCEPT** | none — zero crates/core cited |
| 6 | p1a:95/154, p1c:151/255, p1d:170-202, p1e:140/264, p1f:298 | "BEATS lightningcss" plane-mask | **ACCEPT** | none — uniformly flagged recognition-only |
| 7 | p1f:24, p1a:11, p1b:25, p1c:18, p1d:26, p1e:18 | 4/4 CSS coverage vs §2.1 17-corpus mandate | **ACCEPT** | none — correctly scoped to CSS-tape subject |
| 8 | **p1d-pmu-cycles.md:185-188** | mischaracterizes benched fact-stream cost | **REVISE** | factual correction (see §3) |
| 9 | **p1f-bench-canonical.md:185** | `find_component_delim` line swap | **REVISE** | line attribution fix (see §3) |

Counts: **ACCEPT 7, REVISE 2, REJECT 0** (9 disposed sections).

---

## §2 — ACCEPT findings (verified, load-bearing)

**A1 — The bench is statistically honest (PRIMARY lens obligation).**
`css_canon_bench.rs:250` carries `assert!(n >= 50, "N must be >= 50 (SK-V17
telemetry-honesty gate)")`; default N=200 (`:249`). `sample()` (`:146-177`) takes
N cold per-parse samples — each sample times exactly one `parse(black_box(input))`
call (`Instant::now()` … `elapsed()`), black-boxes and drops the result, reuses no
parser state across samples (`:153-159`). It reports median/min/max/stddev
(`:160-176`). The single touch outside the timed window (`:152`) defeats first-touch
page-fault contamination of the SOURCE buffer only — the parse itself is cold per
sample. This is the `no-warm-benches` cold-per-parse contract. The W8 broadcast
(one timed loop over `total_bytes × 7 grammars × 8 iters → ONE tuple`,
`css_l4_w8.rs:217`) and the W6 `W6_SAMPLE_COUNT=1` single-sample harness are
explicitly retired — the canonical harness emits 16 distinct per-corpus×workload
rows, no broadcast. **Independently reproduced** at N=60: full_parse beats
lightningcss on all 4 corpora (bootstrap 1805/1113=1.6×, tailwind 2806/785=3.6×,
material 2456/1292=1.9×, animate 2382/1234=1.9×); fact_stream below on all 4. The
qualitative claims hold; the N=200 medians sit inside my N=60 spread.

**A2 — No fixture short-circuit.** `track1_full_parse` → `parse_full` →
`emit_full_parse` → `CssFullParser::new(input).parse_stylesheet()`
(`generated.rs:61-62`) runs a real `while pos < bytes.len()` byte scan
(`find_component_delim` :293, `consume_balanced_at` :322). `track1_fact_stream` →
`parse` → `emit_fact_stream` → `emit_declarations` (`generated.rs:45→411`) runs a
real per-declaration scan with hex-encoding. Both are content-dependent (my N=60
rates vary per-corpus proportional to structure); neither returns a constant. The
`.expect(...)` on every workload (`css_canon_bench.rs:104,109,114,119`) means a
parse error would panic — none did, so all 4 corpora parse cleanly on all 4
workloads (the comparator is not spuriously fast from early-erroring).

**A3 — lightningcss is the fair >SOTA bar.** `css_canon_bench.rs:113-116` calls
`StyleSheet::parse(input, ParserOptions::default())` + `black_box(sheet.rules.0.len())`
— full-CSSOM materialization, NOT the conflated `lightningcss_facts`
(`nonjson_css_l4.rs:636`, which builds CSSOM AND then walks it into a projection
String). The harness times the CSSOM build only (p1f:60-66). The lightningcss
flame attribution (p1f:219-239) proves genuine materialization: ~38% cssparser
tokenizer + ~30% typed `Property`/`Selector`/`CssRule` build+drop — a true
full-CSSOM build, the correct bar per SYNTHESIS §0.6.

**A4 — No wrong-tree (crates/core) measurement.** `grep crates/core` across all six
artefacts returns ZERO measurement citations (only `core::ptr::drop_in_place`,
`core::str` library leaves in flame tables, which are correct attribution). Every
benched symbol resolves to `skinny/crates/...` (runtime grammar, bbnf-bench, tape).
crates/core is the SK-V18 totality-fold target and is correctly NOT profiled here.

**A5 — The "BEATS lightningcss 2.0-3.6×" headline is NOT laundered into a typed-gate
win (the central overfit hazard, contained).** `track1_full_parse` materializes only
a 4-field `CssFullParseSummary` (rules/at_rules/qualified_rules/declarations,
`generated.rs:53-59`); it allocates no CSSOM, retains no value tree — a
count-the-braces recognition scan. Every artefact that cites the headline marks it:
p1a:95 ("materializes NO AST … recognition skeleton, not the rich typed CSSOM"),
p1a:154 ("MASKING — recognition plane masks the typed cost … if S-P2 reports the
recognition number as the typed result, that is the exact W6 'summary lane retains
nothing' error"), p1c:255-257 ("masking probe"), p1d:170-202 ("WRONG-PLANE …
NOT admissible: `css_rich_ast_preserved` + `css_comparator_plane==full-cssom`
required"), p1e:264-267 ("masking probe"), p1f:298 ("recognition-only (4-field
summary), NOT preserve-rich-ast, so it does not by itself discharge the SK-V17 typed
gate"). The discipline is uniform and explicit. This is the model behaviour the
OVERFIT-PRUNE lens exists to enforce; no agent claims the win.

**A6 — 4/4 CSS corpus coverage is correctly scoped, not an overfit.** PASS-1-PROFILE
§2.1's 17-corpus mandate binds the JSON profiling subject; SK-V17's subject is the
CSS-tape plane, whose benched corpus set is fixed at 4 real-world production
sheets (bootstrap/tailwind/material/animate, `css_l4_corpus.rs:21-58`, sha256-pinned)
per SYNTHESIS §0.5. These are regular (non-contrived) shipping corpora — "no
contrivance, >=1 regular corpus" is satisfied. Every artefact states the scoping
justification (p1a:11, p1b:25, p1c:18, p1d:26, p1e:18, p1f:24). NOT a float-heavy
or string-light cherry-pick; all four exercise the same delimiter/balance scan.

---

## §3 — REVISE findings (orphan-free; each carries a concrete fix)

**R1 — p1d-pmu-cycles.md:185-188 mischaracterizes the benched fact-stream plane as
"metadata-only" — REVISE.** The text reads: "The sibling entry `track1::parser::parse`
(= `emit_fact_stream`, `generated.rs:5`…) emits config metadata + `fnv64(input)`
and does **not even run the scan** — it is metadata-only and even cheaper." This is
factually wrong and self-contradicts the rest of the S-P1 corpus:
- `emit_fact_stream` (`generated.rs:5`) calls `emit_declarations(input, &mut out)`
  at `generated.rs:45`, which runs a full `while pos < bytes.len()` declaration scan
  (`generated.rs:411-442`) emitting per-decl + per-token rows with hex-encoding
  (`push_ascii_lower_hex`, `:425`).
- It is the **most expensive** benched plane, not the cheapest: 234-364 instr/byte
  (p1f §2.2:160-168), ~505-784 Mbps (p1f §2.1) vs full_parse's 46-58 i/B. p1f §2.3
  attributes it ~64% system alloc + ~34% String emission — clearly running and
  String-building, not metadata-only.
- Fix: replace lines 185-188 with an accurate statement, e.g. "The sibling entry
  `track1::parser::parse` (= `emit_fact_stream`, `generated.rs:5`) is the benched
  fact-stream plane; it runs the full per-declaration scan (`emit_declarations`,
  `generated.rs:45→411`) and is the MOST expensive plane (234-364 i/B), NOT cheaper
  than full_parse. Its cost is dominated by the per-token String/hex emission, the
  tax SK-V17 tape activation removes." p1d's wrong-plane conclusion about
  `emit_full_parse` is unaffected; only the fact-stream characterization is corrected.

**R2 — p1f-bench-canonical.md:185 swaps the `find_component_delim` hot-line
attribution — REVISE.** The cell reads "hot at :298 `delimiters.contains`, :295 loop
test, :307 `pos+1`". In `generated.rs`: `:295` IS `if delimiters.contains(&byte)`,
`:298` is `pos = match byte` (the dispatch), `:307` is `_ => pos + 1`. The
`delimiters.contains` and loop-test line numbers are transposed.
- Fix: ":295 `delimiters.contains`, :293 loop test (`while pos < self.bytes.len()`),
  :298 byte dispatch, :307 `pos + 1`". (Cosmetic line-attribution accuracy; the
  59% self-time figure and the symbol are correct.)

Note (NOT a disposition, sub-threshold): p1d:177 / p1a cite
`CssFullParser::parse_stylesheet` at `generated.rs:117`; the `fn` signature is at
`:118` (off-by-one). Below the REVISE threshold — left for the agent's discretion.

---

## §4 — Cross-lens / convergence notes

- **No REJECT.** No artefact section is unsalvageable; both REVISE items are
  surgical corrections leaving every measured number and conclusion intact.
- **No orphan REVISE.** Both R1 and R2 carry a concrete textual fix at a precise
  path:line; both fold cleanly into the V2 dispatch of their owning agents (p1d, p1f).
- **CH7 ACCEPT rate this artefact-set: 7/9 = 77.8%.** Below the §3Z 95% bar — the two
  REVISE items must fold into V2 before this lens converges. The defects are
  localized (one sentence in p1d, one cell in p1f); a V2 with both folded is expected
  to reach ACCEPT.
- **Overlap with CH1 (CORRECTNESS):** R2 (line attribution) and the :117/:118 note are
  CH1-adjacent (symbol/file:line accuracy); R1 (fact-stream mischaracterization) is
  primarily a CH7 honesty defect (it misstates a benched workload's cost class) but
  also touches CH2 (the "metadata-only" framing misnames the primitive). Flagged for
  the consolidator to dedupe against CH1/CH2.
- **Overfit verdict:** the S-P1 CSS profile is honest. The single largest overfit
  hazard — reporting the recognition-only `parse_full` "BEATS lightningcss" as the
  SK-V17 typed result — is uniformly and explicitly contained across all six
  artefacts. The bench is N>=50 cold, non-broadcast, non-short-circuiting; the
  comparator is fair full-CSSOM; no crates/core wrong-tree leakage. The only honesty
  defect is p1d's incorrect "metadata-only" claim about the fact-stream plane (R1).

---

## §5 — Sources

- Harness source: `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:146-177,250`
  (sample loop + N>=50 gate), `:113-116` (lightningcss full-CSSOM), `:282-403`
  (cssparser probe).
- Benched grammar: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/
  generated.rs:5` (emit_fact_stream), `:45` (emit_declarations call), `:53-59`
  (CssFullParseSummary 4-field), `:61-62` (emit_full_parse → parse_stylesheet),
  `:288-311` (find_component_delim), `:320-340` (consume_balanced_at), `:411-442`
  (emit_declarations scan), `:625-634` (push_ascii_lower_hex).
- Corpus loader: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:21-58` (4 sha256-pinned
  corpora), committed under `skinny/corpora/css-l4-sk-v14/`.
- Independent reproduction: `cargo build --release -p bbnf-bench --bin css_canon_bench`
  + `./target/release/css_canon_bench 60` (this review, 2026-05-29, Apple M5 Max) —
  full_parse beats lightningcss on all 4 corpora; fact_stream below on all 4.
- Reviewed artefacts: `restart/skinny/tranches/sk-v17/research/p1/p1{a..f}.md`.
