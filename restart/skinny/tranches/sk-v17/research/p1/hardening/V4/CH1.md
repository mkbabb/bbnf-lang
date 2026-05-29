# SK-V17 S-P1 CHALLENGE — CH1 CORRECTNESS (V4)

Lens: CH1 CORRECTNESS. Pass: S-P1 Profile. Cycle: V4. Date: 2026-05-29.
Reviewer scope: every hot leaf resolves to a real symbol (samply/atos); every Mbps is
N>=50 cold median with stddev; comparator planes correct (lightningcss = materializing
full-CSSOM, cssparser = token-scan). Per PASS-1-PROFILE §3 CH1 + ORCHESTRATOR §3W.
Artefacts under review: `research/p1/{p1a-samply-mode-1, p1b-samply-mode-2,
p1c-samply-mode-3, p1d-pmu-cycles, p1e-hot-leaf-attribution, p1f-bench-canonical}.md`
(all dated Cycle V4 / 2026-05-29).
Baseline verified: master HEAD `6496fecae706c5ffb1b80b82ea5dcfa6f7ff0e33`
(`git rev-parse HEAD` confirmed; SK-V16 close `1c5bd7a25`).

Disposition vocabulary: ACCEPT / REVISE / REJECT. One disposition per artefact-level roll,
plus the cross-artefact CH1 dispositions that bind the whole cycle.

V3 CH1 returned **6/6 = 100% ACCEPT, 0 REVISE, 0 REJECT** with the dominant V2 open item
(X1′ — the `ri_cycles` posture inversion) FOLDED across all six. V4 is a fresh re-emission
of the six artefacts (all re-labelled Cycle V4) on a FRESH measurement run
(`css_canon_n200_v4.txt`, `css_canon_pmu_v4.txt`). This V4 review re-verifies every CH1
core obligation against on-disk source and the fresh V4 data — it does NOT inherit V3's
verdict blindly (profile-first discipline, ORCHESTRATOR §8).

---

## §0 — Verification performed (what CH1 actually checked this cycle, not asserted)

1. **Baseline + harness on disk.** `git rev-parse HEAD` = `6496fecae706…` (matches every
   artefact frontmatter). `css_canon_bench.rs` present (403 LOC, `wc -l` confirmed) —
   the prior V3 review cited 403 and the count is unchanged.
   `generated.rs` 646 LOC (`wc -l` confirmed).

2. **Hot-leaf symbol resolution — re-read against source this cycle, EVERY cited line.**
   `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`:
   - `emit_fact_stream` `:5`; `push_hex64(&mut out, fnv64(input.as_bytes()))` `:26`;
     `emit_declarations(input, &mut out)` `:45`; `emit_full_parse` `:61`.
   - `parse_stylesheet` `:118`, `parse_at_rule` `:137`, `parse_qualified_rule` `:170`,
     `parse_block` `:189`, `parse_block_item` `:209` (first scan
     `find_component_delim(self.pos, b"{};")` `:211`), `find_colon_before` call `:219`,
     `parse_declaration` `:242` (value scan `find_component_delim(self.pos, b";}")` `:247`),
     `skip_ws_comments` `:263`.
   - `find_component_delim` `:288`; `:293` `while pos < self.bytes.len()` (loop test),
     `:294` `let byte = self.bytes[pos]` (byte load), `:295`
     `if delimiters.contains(&byte)` (byte-membership SCAN leaf), `:298` `pos = match byte`
     (dispatch), `:307` `_ => pos + 1` (advance). `find_colon_before` `:313`
     (`find_component_delim(start, b":{};")` `:314`). `consume_balanced_at` `:320`
     (`:322` loop, `:323` byte load, `:324` `byte == close`, `:327` `pos = match byte`,
     `:336` `_ => pos + 1`, `:340` unclosed-block error). `consume_comment_at` `:342`,
     `consume_string_at` `:353`.
   - FNV/hex diagnostics: `push_ascii_lower_hex` `:628` (V4 P1-F refreshed from V3's
     `:625-634` to body `:628-634`; `push_hex` call at `:633`).
   ALL EXACT. No hot-leaf citation in any V4 artefact resolves to a wrong line or a
   missing symbol. The fine-grained within-`find_component_delim` per-line split
   (`:295` membership vs `:298` dispatch vs `:307` advance) and the
   `consume_balanced_at` split (`:324`/`:327`/`:336`) are source-consistent.

3. **atos symbol resolution is REAL (not a paper-close).** `/tmp/skv17-p1d/atos_v2.txt`
   = 29104 B, 199 resolved lines (`wc -l`; 199/199 carry `(in …)` frames). Spot-read:
   `_RNvMNt…7runtime35generated_css_l4_declaration_values9generated…13CssFullParser20find_component_delim
   (in css_track1_profile) (generated.rs:298)` — and `:295`, `:294` — i.e. the mangled
   symbol demangles to
   `runtime::generated_css_l4_declaration_values::generated::CssFullParser::find_component_delim`
   and resolves to the EXACT source lines the artefacts cite. The V1 0-byte
   `atos_out.txt` paper-close stays resolved (the 0-byte file remains on disk but is not
   the cited artefact). Flame artefacts present:
   `/tmp/skv17-p1/{full_parse,fact_stream,lightningcss}.json.gz` + the per-corpus splits;
   `/tmp/skv17-p1c-v2/{full,fact}.json.gz`; `/tmp/skv17-p1e/{full_parse,fact_stream}.json.gz`.

4. **N>=50 gate is CODE-ASSERTED + the harness is genuinely cold-per-parse.**
   `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` at
   `css_canon_bench.rs:250` (read in source). `fn sample` `:146` times exactly one
   `parse(black_box(input))` per window (`Instant::now()` … `elapsed()`, result
   black-boxed + dropped, `:153-159`); the ONLY pre-touch is one untimed
   `black_box(parse(black_box(input)))` outside the loop `:152` (source-buffer page-fault
   amortisation, NOT a warm parser-state cache — each timed parse re-allocates its own
   output). This honours `no-warm-benches`. `fn mbps` `:138`, `WORKLOADS` table
   `:123-128`, `CSS_CANON_PROFILE` driver `:183-207`, `CSS_CANON_PMU` mode `:211-247`,
   the `cpi` column literally `cyc as f64 / ins as f64` at `:241` — ALL read at the exact
   lines. Every §2 table in all six artefacts carries median/min/max/stddev with N>=50
   (A re-emits V3 N=200; B N=200; C N=64×2; D N=64×2; E N=100+N=60 reproducibility;
   F N=200×3-run). No single-sample, no warm number, in any §2 table.

5. **Mbps numbers are faithful to on-disk run files (not fabricated) — spot-checked to
   the decimal.**
   - P1-F §2.1 vs `/tmp/skv17-p1/css_canon_n200_v4.txt`: bootstrap full 2272.923 /
     fact 851.021 / lcss 1110.169 / cssparser 2900.407; tailwind lcss 833.786; material
     full 2590.116 / lcss 1261.148 (min 160.300 outlier); animate full 2493.164 — ALL
     match the table verbatim.
   - P1-E §2.1 vs `/tmp/skv17-p1e-canon-n100.txt`: bootstrap full 2006.429 / fact
     719.604 / lcss 909.951 — match (P1-E rounds 909.951→"910" in the ratio line, exact).
   - P1-C §2 vs `/tmp/skv17-p1c-bench-run1.txt`: track1 bootstrap median 850.406 — match.
   - P1-A / P1-B source `css_canon_n200_v2.txt` + `css_canon_pmu_v2.txt` /
     `css_canon_n200.txt` + `css_canon_pmu.txt` (V2/V3-run files): ALL present on disk
     (2409–2754 B). P1-A's V4-FOLD NOTE explicitly states it re-emits V3 content with
     citations re-verified and carries the V2/V3-run figures forward under the X2
     comparability caveat — disclosed, not concealed (see X4 below).

6. **PMU instr/byte derivations verified to the decimal against the fresh V4 PMU log.**
   `/tmp/skv17-p1/css_canon_pmu_v4.txt` present. Recomputed `ri_instructions/(bytes·2000)`:
   bootstrap full 25003802730/(232803·2000) = 53.70 ✓ (P1-F §2.2 = 53.70); bootstrap
   fact 109366683700/… = 234.89 ✓; bootstrap lcss 160.14 ✓; tailwind fact 364.51 ✓;
   material fact 214.56 ✓; animate fact 279.28 ✓. Every §2.2 instr/byte cell I sampled
   reproduces. instr/byte is the load-bearing cost density (PMU-derived, NOT estimated).

7. **Comparator planes CORRECT — re-verified against source + profile.**
   - **lightningcss = materializing full-CSSOM.** `lightningcss_full_cssom` at
     `css_canon_bench.rs:113-116` calls `StyleSheet::parse(input,
     ParserOptions::default())` and returns `sheet.rules.0.len()` (read in source). P1-F
     §2.3 profiles it (13583 leaf samples) and resolves ~38% cssparser tokenizer + ~30%
     typed node build+drop (`parcel_selectors::parser::parse_selector` 5.04%,
     `lightningcss::PropertyId::from_name_and_prefix` 2.39%,
     `drop_in_place::<cssparser::Token>` 3.95%, `Property::parse`, `TokenList::parse_into`,
     `drop_in_place::<Property>`) — the load-bearing PROOF the comparator genuinely builds
     and drops the typed CSSOM. The measured lightningcss median (tailwind 833.786 Mbps)
     matches the contract's "~833 Mbps full-CSSOM" anchor.
   - **cssparser = token-scan.** `cssparser_token_scan` `:118-121` drives
     `CssparserFullParseProbe` (`css_canon_bench.rs:282-403`) — read in source: the probe
     types are all `()` (`type Declaration = ()`, `type AtRule = ()`,
     `type QualifiedRule = ()`); `consume_component_values` walks tokens via
     `next_including_whitespace_and_comments` and recurses into nested blocks, building
     NOTHING. P1-C labels it "(token-scan, materializes nothing)" at `css_canon_bench.rs:282-403`
     — source-correct.
   - All six artefacts name lightningcss "full-CSSOM" and cssparser "token-scan" (grep
     counts: F 11/8, D 12/6, B 7/5, C 2/5, A 2/1, E 2/3). Both planes correct in every
     artefact.

8. **The wrong-plane (`track1_full_parse` BEATS lightningcss) is CORRECTLY DISCLOSED as
   recognition-only across all six — the load-bearing CH1 honesty obligation.** The V4
   data shows track1_full_parse beats lightningcss 2.0-3.1× on every corpus (P1-F §2.1.2:
   bootstrap 2.05×, tailwind 3.09×, material 2.05×, animate 2.01×). This is the
   recognition-only structural-summary plane (`emit_full_parse` increments a 4-field
   `CssFullParseSummary` at `generated.rs:91-99`, materializes no AST), NOT preserve-rich-ast.
   Every artefact discloses it: P1-F §3 classifies it **A** but states it "does not by
   itself discharge the SK-V17 typed gate" (`p1f:535`); P1-A §4.1 (`p1a:115`) "it
   materializes NO AST … the recognition skeleton, not the rich typed CSSOM SK-V17 must
   ship"; P1-E §2.4 (`p1e:162-164`) "NOT preserve-rich-ast — it counts
   rules/at_rules/qualified/decls and materializes nothing"; P1-C §3 (`p1c:201-204`)
   "recognition-only … the wall is rich materialization"; P1-D §4 (`p1d:461`) "NoGo for
   the >SOTA gate — wrong plane (no typed CSSOM)". The HONEST typed comparison — the
   `track1_fact_stream` String plane (the live benched typed surface, `track1_facts ->
   Result<String,_>`) — sits BELOW lightningcss on every corpus (0.60-0.77×, P1-F §2.1.2),
   correctly classified **L (loss)**. No artefact reports the recognition number as the
   typed result; the recognition-vs-materialization masking is named for S-P2 (P1-A §4.1
   MASKING, P1-C A2).

9. **`tape_activated = false` for CSS — re-verified fresh this cycle.** `grep -rln
   "TapeBuilder|ValueRef|PayloadArena|crate::tape" skinny/crates/runtime/src/grammars/
   css_l4_*/` returns ZERO; the JSON grammar (`grammars/json/{parser,scan,value,view}.rs`)
   DOES reference the tape. P1-F §4.4 / P1-A §4.5 substrate-union claim is empirically
   correct: the benched CSS planes touch no tape symbol.

10. **NEON / primitive symbol cites resolve.** `select_classifier` `dispatch.rs:42`
    (read), `push_plain_offset` `assembler.rs:71` (grep-confirmed, branchless u32 write),
    JSON byte-class primitive
    `classify_structural_terminator_block_from_table` `json/scan.rs:219` (grep-confirmed).
    The CH2-adjacent "same byte-class-membership primitive as JSON" attribution is
    source-correct.

11. **Aggregate byte count.** Four benched corpora: animate 71750, bootstrap 232803,
    tailwindcss 179631, material-components-web 495454, raw sum **979638** (`wc -c`
    re-confirmed). All six use 979638. The `wc -c total` line reads 981623 only because
    it folds the 1985-byte `manifest.md`; P1-F §1.3 explicitly states the load-bearing
    4-corpus sum is 979638. Correct.

---

## §1 — Per-artefact dispositions

### P1-A (`p1a-samply-mode-1.md`) — ACCEPT (V4 re-emission; no open V3 item)

- **§1 / §2.1 / §2.1b / §2.2 / §2.3 / §3 / §4 / §5 — ACCEPT.** Every cited generated.rs
  hot leaf (`:5,:61,:103,:118,:189,:242,:288,:295,:320,:628`) resolves at the exact line.
  The COST-SURFACE POSTURE (`p1a:17`) carries the single pass-wide X1′ posture verbatim
  (instr/byte sole load-bearing; sub-1.0 CPI = IPC 3.5-6.2 PHYSICAL not impossible;
  cyc/byte RAW non-load-bearing because `ri_cycles` non-disambiguable). §2.1b instr/byte
  is the authoritative density (fact 234-364 i/B vs full 53.7-57.7 = 4.36-7.07× String
  tax). The recognition-only disclosure (§4.1, §4.7) is explicit and load-bearing. Both
  comparator planes named full-CSSOM / token-scan. No V3 REVISE/REJECT existed against
  P1-A to fold; V4 re-verifies every citation fresh.
- **Minor (non-blocking, NOT a defect):** the frontmatter line 10 retains the V3 label
  "V3 folds CH4-5: the authoritative §2.1 Mbps table is now sourced from `css_canon_bench`"
  while the artefact is Cycle V4; §2.1/§2.1b source from `css_canon_n200_v2.txt` /
  `css_canon_pmu_v2.txt` (the V2/V3 run), NOT the fresh V4 run P1-F uses. This is a
  cosmetic cycle-label staleness, fully disclosed by the V4-FOLD NOTE (`p1a:15`: "re-emits
  the V3 content with every load-bearing citation re-verified") and harmless under the X2
  comparability caveat (within-harness ratios load-bearing; the v2-run files exist on disk
  and reproduce the cited numbers). Tracked as cross-artefact note X4 — no disposition
  change; CH1 does not REVISE on a disclosed-and-stable source-run choice.

### P1-B (`p1b-samply-mode-2.md`) — ACCEPT (V4 re-emission; no open V3 item)

- **§1 / §2.1 / §2.2-§2.4 / §3 / §4 / §5 — ACCEPT.** The c/B PROVENANCE note carries X1′
  verbatim. `find_component_delim` 56.55% / `consume_balanced_at` 11.51% resolve at
  `:288`/`:320`. Both comparator planes named (full-CSSOM 7×, token-scan 5×). Cited source
  files `css_canon_n200.txt` + `css_canon_pmu.txt` present on disk (2409/2754 B). The
  String-tax re-derived from the reliable instr/byte (fact 214-364 i/B vs full 46-58 i/B).
  The CH2-adjacent "same byte-class-membership primitive as JSON `json/scan.rs:219`" call
  is source-correct. No V3 item to fold.

### P1-C (`p1c-samply-mode-3.md`) — ACCEPT (V4 re-emission; no open V3 item)

- **§1 / §2.1-§2.5 / §3 / §4 / §5 — ACCEPT.** The cycles-per-byte posture block quotes the
  single pass-wide posture verbatim. The cssparser plane is precisely labelled
  "(token-scan, materializes nothing)" at the EXACT source span `css_canon_bench.rs:282-403`
  (verified: the probe's associated types are all `()`). lightningcss = "full L2 CSSOM"
  `StyleSheet::parse` (`p1c:114`). Per-line self-time at `:288,:293-:298,:307` and
  `:320-:340` resolves; A1 plane-bifurcation and A2 recognition-vs-materialization masking
  are correct. Cited flame artefacts (`/tmp/skv17-p1c-v2/{full,fact}.json.gz`) present.
  X3 (979638) reconciled. No V3 item to fold.

### P1-D (`p1d-pmu-cycles.md`) — ACCEPT (the physics-correct PMU authority)

- **§1 / §2.1-§2.5 / §3.1 / §3.2 / §3.3 / §4 / §5 — ACCEPT.** P1-D is the artefact that
  adjudicated X1′ correctly and (in V3) withdrew its own over-claim; V4 carries both
  corrections. §3.1 states the dual correction (sub-1.0 CPI = IPC 3.7-6.4 physical AND the
  "proven 4.27 GHz / supersedes A/B/F" over-claim withdrawn). instr/byte is the sole
  grounded cost density. The §2.4 atos artefact (`atos_v2.txt`, 29104 B, 199 resolved
  lines) is non-empty and resolves `find_component_delim` to `generated.rs:294/295/298`.
  Recognition-plane NoGo disclosed (§4 `:461`: "wrong plane — no typed CSSOM"). PMU table
  verbatim-faithful (instr/byte spot-checked §0.6). Both comparator planes named
  (full-CSSOM 12×, token-scan 6×). No V3 item to fold.

### P1-E (`p1e-hot-leaf-attribution.md`) — ACCEPT (V1 fabricated-line REJECT stays CLEARED)

- **§1 / §2.1-§2.5 / §3 / §4 / §5 — ACCEPT.** Every `css_canon_bench.rs` line P1-E now
  cites — `:103` (`track1_full_parse`), `:146-159` (`sample`), `:123-128` (`WORKLOADS`),
  `:183-207` (profile driver) — matches source EXACTLY. The previously-flagged fabricated
  cites (`303` / `:150` / `:84-116`) are GONE (grep returns zero). §2.1 numbers match
  `css_canon_n100.txt` to the decimal; the N=60 reproducibility set
  (`css_canon_n60.txt`) is present. The recognition-only disclosure (§2.4 `:162-164`) is
  explicit. The hot-leaf classification (scan-dominant, no number/unicode/dispatch/tape
  leaf on the CSS path) is correct. The V1 fabricated-line REJECT stays CLEARED.

### P1-F (`p1f-bench-canonical.md`) — ACCEPT (the bench/measurement authority; X1′ originator, retracted)

- **§1 / §1.1 / §1.2 / §1.3 / §2.1 / §2.1.1 / §2.2 / §2.2.1 / §2.3 / §3 / §4 / §5 — ACCEPT.**
  P1-F owns the canonical-harness verdict (X2): `css_canon_bench.rs` is THE single harness
  (only binary with the N>=50 assert `:250` + PMU mode + samply driver). §2.1 matches the
  fresh `css_canon_n200_v4.txt` verbatim; §2.2 instr/byte matches `css_canon_pmu_v4.txt`
  verbatim (spot-checked to the decimal). §2.2.1 retracts the V2 "physically impossible"
  framing it originated. The comparator-plane discharge is the cleanest in the pass: §2.3
  proves lightningcss materializes (30% typed node build+drop) and labels cssparser
  token-scan. §3 classifies full-parse A (recognition-only, does not discharge the typed
  gate), fact-stream L (0.60-0.77× lightningcss), eager-typed K (pre-blocked), the "~70
  Mbps / ~14×" narrative N-direct (no fresh benched antecedent) — the honest two-planed
  truth. §4.4 `tape_activated=false` grep re-verified. The line-cite refresh
  (`push_ascii_lower_hex` `:628`, `push_hex` `:633`) is source-correct.

---

## §2 — Cross-artefact CH1 dispositions (bind the whole cycle)

### X1′ — ri_cycles posture — RESOLVED (stays folded; ACCEPT)

The single c/B posture (instr/byte primary + load-bearing; cyc/byte co-reported with IPC
explicit + non-load-bearing because `ri_cycles` is non-disambiguable, NOT because sub-1.0
CPI is impossible) is carried verbatim across all six V4 artefacts. CH1 independently
re-adjudicated the physics from the FRESH V4 PMU log: `cpi = cyc/ins` (`:241`); the 16 V4
rows span CPI [0.158, 0.277] ⇔ IPC 3.6-6.3, physical on the ~8-wide M5 P-core. The fresh
run reproduces V3 instr/byte to <0.5%. Stays RESOLVED.

### X2 — single canonical harness — RESOLVED (stays folded; ACCEPT)

`css_canon_bench.rs` is THE harness (P1-F §1.1.1 authority, echoed by all five). The
comparability caveat (absolute Mbps harness/alloc/CPU-flag/host-noise dependent; only
within-harness same-run ratios load-bearing) is stated by all six and demonstrated by P1-F
§2.1.1 across THREE independent runs (full BEATS lightningcss on all four every run; fact
below on all four every run). Stays RESOLVED.

### X3 — aggregate byte count — RESOLVED (stays folded; ACCEPT)

All six use 979638; `wc -c` of the four corpora = 979638 re-confirmed; the 981623
`wc -c total` divergence (manifest.md) is explained by P1-F §1.3. Stays RESOLVED.

### X4 — source-run heterogeneity (P1-A/B/E v2/n100 runs vs P1-F v4 run) — ACCEPT (note, not REVISE)

P1-F sources the fresh V4 run; P1-A/P1-B source the V2/V3 run files
(`css_canon_n200_v2.txt`, `css_canon_pmu_v2.txt`, `css_canon_n200.txt`,
`css_canon_pmu.txt`); P1-E sources its own N=100/N=60 runs (`css_canon_n100.txt`,
`css_canon_n60.txt`). All cited run files exist on disk and reproduce their cited numbers
to the decimal. The absolute-Mbps drift across runs (the host-scheduling band the X2
caveat names) does NOT flip any verdict — full-parse > lightningcss and fact-stream <
lightningcss hold in every run. This is the disclosed, contract-permitted comparability
posture, not a fabrication or a wrong-plane error. CH1 does NOT REVISE on it. The ONLY
residual is cosmetic: P1-A's frontmatter line 10 retains a "V3" label on a Cycle-V4
artefact — a non-load-bearing cycle-label staleness, flagged for the aggregator's optional
tidy, not a CH1 defect.

---

## §3 — Counts + summary

Dispositioned: 6 artefact-level rolls + 4 cross-artefact (X1′, X2, X3, X4) = **10 CH1
dispositions**.

| Disposition | Count | Items |
|---|---:|---|
| ACCEPT | 10 | P1-A, P1-B, P1-C, P1-D, P1-E, P1-F (all artefact-level, every section); X1′ (ri_cycles posture — stays folded, physics re-verified from fresh V4 counters); X2 (single canonical harness — stays folded + 3-run demonstrated); X3 (aggregate 979638 — wc-verified); X4 (source-run heterogeneity — disclosed, stable, contract-permitted) |
| REVISE | 0 | — none. The cosmetic V3→V4 cycle-label staleness in P1-A frontmatter is flagged as an optional aggregator tidy, NOT a REVISE (no claim is wrong; the artefact discloses it carries V3 content forward). |
| REJECT | 0 | — none. No fabricated symbol, no unresolvable hot leaf, no warm/single-sample number, no mis-planed comparator, no fabricated PMU/Mbps number (all run files on disk reproduce cited values to the decimal). |

**ACCEPT rate (artefact-level): 6/6 = 100% clean.** Every CH1 core obligation re-verified
against source + fresh V4 data this cycle:
- Every hot leaf resolves to a real symbol at the exact cited `file:line` (generated.rs
  `:5,:26,:45,:61,:118,:137,:170,:189,:209,:211,:219,:242,:247,:263,:288,:293-:298,:307,
  :313,:314,:320,:322-:340,:411,:628,:633` all exact; atos_v2.txt resolves
  `find_component_delim` to generated.rs:294/295/298 with real demangled symbols).
- Every Mbps is an N>=50 cold median with min/max/stddev; the N>=50 gate is code-asserted
  (`css_canon_bench.rs:250`); the harness is genuinely cold-per-parse (`sample` `:146`,
  one timed parse, output dropped, only untimed source pre-touch).
- Both comparator planes are correct: lightningcss = `StyleSheet::parse` full-CSSOM,
  PROVEN materializing by P1-F §2.3 (30% typed node build+drop), measured ~833 Mbps on
  tailwind = the contract anchor; cssparser = `CssparserFullParseProbe` token-scan
  (`css_canon_bench.rs:282-403`, all `()` types, materializes nothing).
- The wrong-plane (track1_full_parse beats lightningcss) is correctly disclosed as
  recognition-only across all six — classified A but explicitly does NOT discharge the
  preserve-rich-ast typed gate; the honest typed plane (fact_stream) is L (below
  lightningcss).
- instr/byte is PMU-derived (not estimated), verified to the decimal; cyc/byte correctly
  reported RAW non-load-bearing per the single X1′ posture.
- All cited flame + atos + PMU + bench run files exist on disk; the V1 atos paper-close
  stays closed (atos_v2.txt 29104 B); the V1 P1-E fabricated-line REJECT stays cleared
  (cites now `:103/:146-159/:123-128/:183-207`, all source-exact).

**Convergence read:** zero REJECT, zero REVISE, zero orphan. CH1 does NOT block on the
JSON-roster skip (correct CSS-subject override per SYNTHESIS §0.5) nor on the
recognition-plane ">beats lightningcss" numbers (correctly disclosed as wrong-plane —
recognition-only, no typed CSSOM). The only residual is a cosmetic cycle-label staleness
in P1-A's frontmatter (flagged for optional tidy). **At V4, CH1 returns 6/6 = 100%
ACCEPT** — clearing the >=95% ACCEPT bar with zero orphan REVISE for this lens, the
SECOND consecutive cycle at 100% (V3 was also 6/6), meeting the per-lens ORCHESTRATOR
§3Z convergence condition (>=95% for two consecutive cycles, zero open critical defect,
no orphan unresolved REVISE).
