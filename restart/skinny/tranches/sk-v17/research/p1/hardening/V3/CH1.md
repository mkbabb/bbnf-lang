# SK-V17 S-P1 CHALLENGE — CH1 CORRECTNESS (V3)

Lens: CH1 CORRECTNESS. Pass: S-P1 Profile. Cycle: V3. Date: 2026-05-29.
Reviewer scope: every hot leaf resolves to a real symbol (samply/atos); every Mbps is
N>=50 cold median with stddev; comparator planes correct (lightningcss=materializing
full-CSSOM, cssparser=token-scan). Per PASS-1-PROFILE §3 CH1 + ORCHESTRATOR §3W.
Artefacts under review: `research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md` (all dated V3 / 2026-05-29).
Baseline verified: master HEAD `6496fecae706c5ffb1b80b82ea5dcfa6f7ff0e33` (confirmed
`git rev-parse HEAD`; SK-V16 close `1c5bd7a25`).

Disposition vocabulary: ACCEPT / REVISE / REJECT. One disposition per artefact-level roll,
plus the cross-artefact CH1 dispositions that bind the whole cycle.

V2 CH1 left a single dominant open item — **X1′ (the `ri_cycles` posture inversion:
P1-D corrected the physics, the other five still shipped the false "falsified /
physically impossible CPI" framing, so the pass contradicted itself 5-vs-1 and the
majority held the false claim)** — plus the V1-folded items X2, X3 and the cleared V1
P1-E REJECT. This V3 review (1) re-adjudicates X1′ from the on-disk counters, (2)
confirms whether the five folded, (3) re-verifies the CH1 core obligations against
source, and (4) re-disposes against fresh state.

---

## §0 — Verification performed (what CH1 actually checked, not asserted)

1. **Hot-leaf symbol resolution — re-verified against source this cycle.** Every cited
   hot leaf re-read at its claimed `file:line` in
   `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
   (646 LOC, `wc -l` verified). All EXACT:
   - `emit_fact_stream` `:5`; `push_hex64(&mut out, fnv64(input.as_bytes()))` `:26`;
     `emit_declarations(input, &mut out)` `:45`; `emit_full_parse` `:61`; summary
     `out.push_str("full_parse\tstatus=accepted\trules=")` `:91`.
   - `parse_stylesheet` `:118`, `parse_at_rule` `:137`, `parse_block` `:189`,
     `parse_block_item` `:209` (first scan `find_component_delim(self.pos, b"{};")` `:211`),
     `find_colon_before` call `:219`, `parse_declaration` `:242` (value scan
     `find_component_delim(self.pos, b";}")` `:247`), `skip_ws_comments` `:263`.
   - `find_component_delim` `:288`; `:293` `while pos < self.bytes.len()`, `:294`
     `let byte = self.bytes[pos]`, `:295` `if delimiters.contains(&byte)`, `:298`
     `pos = match byte`, `:307` `_ => pos + 1`. `find_colon_before` `:313`
     (`find_component_delim(start, b":{};")` `:314`). `consume_balanced_at` `:320`
     (`:322` loop, `:323` byte load, `:324` `byte == close`, `:327` `pos = match byte`,
     `:336` `_ => pos+1`).
   - `emit_declarations` `:411`, `fnv64` `:619`, `push_ascii_lower_hex` `:628` with
     `Vec::with_capacity(text.len())` `:629`, `push_hex64` `:636`, `push_hex` `:640`.
   No hot-leaf citation in any V3 artefact resolves to a wrong line or a missing symbol.
   The per-line within-`find_component_delim` attributions (`:295` membership vs `:298`
   dispatch vs `:307` advance) and within-`consume_balanced_at` (`:324`/`:327`/`:336`)
   are all source-consistent. CH1's primary obligation is met by all six.

2. **Canonical harness exists + asserts N>=50 — re-verified.** `css_canon_bench.rs`
   on disk; `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` at
   `:250` VERIFIED by grep. `fn mbps` `:138`, `fn sample` `:146`, `CSS_CANON_PROFILE`
   driver `:183`, `CSS_CANON_PMU` `:211`, workloads `track1_full_parse:103` /
   `track1_fact_stream:108` / `lightningcss_full_cssom:113` (`StyleSheet::parse(input,
   ParserOptions::default())` `:114`) / `cssparser_token_scan:118`, `WORKLOADS` table
   `:124-127` — ALL VERIFIED at the exact lines. **The `cpi` column is literally
   `cyc as f64 / ins as f64` at `:241`** (grep-confirmed) — i.e. CPI = cycles/instruction,
   which is the load-bearing fact for the X1′ adjudication below.

3. **N>=50 cold median with stddev.** All six carry N>=50 with median/min/max/stddev
   per row (A: N=200 §2.1 + iters=2000 PMU; B: N=200; C: N=64×2; D: N=64×2 + iters=2000;
   E: N=100+60; F: N=200×2 + iters=2000). No single-sample number, no warm number, in
   any §2 table. Cold discipline (`sample()` `:146`, one `parse(black_box(input))` per
   timed window, untimed source pre-touch only) is honoured and described by all six.

4. **PMU logs are verbatim-faithful (not fabricated) — spot-checked to the decimal.**
   `/tmp/skv17-p1d-v2-pmu.txt` present (2754 B). bootstrap track1_full_parse row:
   `cycles=6737349921 instructions=25010386205 cycles_per_byte=14.4701 cpi=0.2694
   mbps=2367.607`. P1-D §3.2 reports `instr/byte 53.72` (= 25010386205/(232803·2000) =
   53.71 ✓), `cyc/byte 14.47` ✓, `IPC 3.71` (= 1/0.2694 ✓), `CPI 0.269` ✓. P1-F §2.2
   cites the SEPARATE V2 re-run `css_canon_pmu_v2.txt` (same row cpi=0.2824, cyc/byte
   15.17) — both files on disk, both reproduce their cited values, the delta is the
   disclosed run-to-run host noise (instr/byte stable to <0.5%: 53.70→53.72). Not a defect.

5. **Comparator planes correct.** lightningcss = `StyleSheet::parse(input,
   ParserOptions::default())` returning `sheet.rules.0.len()` (`css_canon_bench.rs:114`)
   — full-CSSOM build, VERIFIED. P1-F §2.3 profiles it (13,583 leaf samples) and resolves
   ~38% cssparser tokenizer + ~30% typed `Property`/`Selector`/`CssRule` build+drop
   (`parse_selector`, `PropertyId::from_name_and_prefix`, `drop_in_place::<Property>`,
   `drop_in_place::<cssparser::Token>`) — the load-bearing PROOF the comparator genuinely
   materializes. cssparser = `cssparser_token_scan` (`:118`) → `CssparserFullParseProbe`,
   materializing nothing — VERIFIED token-scan flaw probe. Both planes correct.

6. **Corpus coverage + aggregate bytes — re-verified by `wc -c`.** All four benched CSS
   corpora present at `skinny/corpora/css-l4-sk-v14/`: animate 71750, bootstrap 232803,
   tailwindcss 179631, material-components-web 495454, `total 979638` (wc -c) =
   71750+232803+179631+495454. All six artefacts use **979638**. X3 stays RESOLVED. The
   §2.1 17-JSON mandate is correctly overridden for the CSS-tape subject per SYNTHESIS
   §0.5; every artefact justifies it. CH1 does NOT reject on the JSON-roster skip.

7. **Profile artefacts exist on disk + the V1 paper-close stays closed.** Every cited
   `.json.gz` + `.txt` + `.syms.json` present under `/tmp/skv17-p1/`, `/tmp/skv17-p1d{,-v2}/`,
   `/tmp/skv17-p1c-v2/`, `/tmp/skv17-p1e/`. **P1-D's `atos_v2.txt` is 29104 B, NON-empty**
   (199 resolved `<symbol> (file:line)` lines) — the V1 0-byte `atos_out.txt` paper-close
   stays resolved (the old 0-byte file remains on disk but is no longer the cited artefact).
   Tape-unwired-for-CSS empirically confirmed: `grep -rln "TapeBuilder|ValueRef|
   PayloadArena|crate::tape" …/css_l4_declaration_values/` returns ZERO (P1-F §4.4).
   NEON/primitive cites resolve: `select_classifier` `dispatch.rs:42`,
   `push_plain_offset` `assembler.rs:71`, JSON byte-class primitive
   `classify_structural_terminator_block_from_table` `json/scan.rs:219`.

8. **The X1′ adjudication — physics re-checked directly from the on-disk counters.**
   The harness `cpi = cyc/ins` (`:241`, confirmed). CPI < 1.0 ⇔ IPC > 1.0 — NORMAL on a
   wide superscalar, NOT physically impossible. From `skv17-p1d-v2-pmu.txt`: bootstrap
   track1_full_parse cpi=0.2694 ⇒ IPC 3.71; tailwind cssparser cpi=0.1570 ⇒ IPC 6.37;
   all 16 rows CPI ∈ [0.157, 0.285] ⇒ IPC 3.5–6.4 — entirely physical on the M5 Max's
   ~8-wide P-core for tight, well-predicted scan/String loops. The V1/V2 "falsified /
   physically impossible CPI" framing was wrong physics (it confused CPI with IPC).
   Equally, the "proven 4.27 GHz / supersedes A/B/F" over-claim (P1-D V2) was unearned:
   `ri_cycles/wall` is observationally identical for a fixed-frequency real-cycle counter
   and a wall-proportional scaled tick (since `wall` is itself loop-derived), and
   `hw.tbfrequency` 24 MHz confirms a scaled reference clock exists on the platform —
   so `ri_cycles` is non-disambiguable from the rusage interface alone. CH1's independent
   reading: instr/byte is the sole defensible cost density; cyc/byte is a valid-but-
   non-disambiguable counter, correctly reported RAW and non-load-bearing.

---

## §1 — Per-artefact dispositions

### P1-A (`p1a-samply-mode-1.md`) — ACCEPT (X1′ folded)

- **§1 / §2.1 / §2.1b / §2.2 / §2.3 / §3 / §4 / §5 — ACCEPT.** The X1′ posture is folded
  verbatim: the COST-SURFACE POSTURE note (`p1a:15`) now states "The sub-1.0 CPI … is
  **PHYSICAL, NOT impossible** … IPC 3.5-6.2 … The earlier V1/V2 characterization … was
  **wrong physics** … and is retracted here" and reports cyc/B RAW/struck-through/non-
  load-bearing because `ri_cycles` is non-disambiguable, citing the same single pass-wide
  posture. instr/byte is the §2.1b authoritative density (animate 279.83 / bootstrap 234.18
  fact-stream vs 57.75 / 53.72 full — the 4.4–7.1× String tax). Both samply planes resolve
  at the cited lines; the `mach_absolute_time`-via-libmalloc caller attribution
  (25591/25640 leaves from `0x2b483` ∈ libsystem_malloc) is a sound stack-prefix walk.
  Masking signals cite REDRESS pre-blocks. The single V2 REVISE (X1′) is discharged.

### P1-B (`p1b-samply-mode-2.md`) — ACCEPT (X1′ folded)

- **§1 / §2.1 / §2.2-§2.4 / §3 / §4 / §5 — ACCEPT.** The c/B PROVENANCE note (`p1b:40-55`)
  folds X1′ verbatim: "The V1/V2 characterization of this as 'physically impossible / a
  reference-clock tick / falsified' was wrong physics and is withdrawn … CPI 0.16 ⇔ IPC
  6.4 … `ri_cycles` is … a valid core-cycle counter … not byte-faithfully disambiguable
  … so cyc/byte is non-load-bearing." §3 re-derives the String tax from the reliable
  instr/byte (fact 214–364 i/B vs full 46–58 i/B = 4.36–7.06×), not the withdrawn cyc/byte
  inference. `find_component_delim` 56.55% / `consume_balanced_at` 11.51% resolve; the
  CH2-adjacent "same byte-class-membership primitive as JSON `json/scan.rs:219`" call is
  source-correct. The single V2 REVISE (X1′) is discharged.

### P1-C (`p1c-samply-mode-3.md`) — ACCEPT (X1′ folded; X3 stays resolved)

- **§1 / §2.1-§2.5 / §3 / §4 / §5 — ACCEPT.** The V3 cycles-per-byte posture block
  (`p1c:37-57`) quotes the single pass-wide posture verbatim and states "The V2 reading
  here was wrong physics: it confused CPI with IPC … set aside as **non-disambiguable,
  non-load-bearing** … P1-D §3.1's 'steady 4.27 GHz' derivation is observationally
  identical for both models and proves neither." §2.5 PMU ledger labels the CPI column
  "CPI = 1/IPC … 0.16–0.28 ⇒ IPC 3.6–6.4, physical" and marks cyc/B RAW/non-load-bearing.
  X3 stays folded: §1.2 reconciles to 979638 with the explicit join-rule note (wc -c
  re-confirmed this cycle). Per-line self-time at `:288,293-298,307` and `:320-340`
  resolves; A1 plane-bifurcation and A2 recognition-vs-materialization masking are correct.
  The single V2 REVISE (X1′) is discharged.

### P1-D (`p1d-pmu-cycles.md`) — ACCEPT (the physics-correct authority; over-claim withdrawn)

- **§1 / §2.1-§2.5 / §3.1 / §3.2 / §3.3 / §4 / §5 — ACCEPT.** P1-D is the artefact that
  adjudicated X1′ correctly AND, in V3, withdrew its own V2 over-claim. §3.1 now states
  the dual correction explicitly: it rebuts the "physically impossible" framing (sub-1.0
  CPI = IPC 3.7–6.4, physical) AND withdraws the "proven 4.27 GHz / supersedes A/B/F"
  over-claim ("the GHz derivation was circular, so 'proven' was not earned and the
  unilateral 'supersedes' was a per-artefact assertion the pass cannot make from one
  section"). The posture block (`p1d:376-382`) is the verbatim text the other five adopt.
  Medians/min/max/stddev correct vs `skv17-p1d-v2-cold64.txt`/`-run2`; the §2.4 atos
  artefact (`atos_v2.txt`, 29104 B, 199 resolved lines, 20377 leaf samples) is non-empty;
  per-line `find_component_delim` breakdown (`:298` 27.88%, `:295` 17.24%) resolves; the
  redundant 2-3× overlapping re-scan (`parse_block_item:211`→`find_colon_before:219/:314`
  →`parse_declaration:247`) is a real verifiable structural finding bounded against
  REDRESS-51/53. PMU table verbatim-faithful (spot-checked §0.4). No defect.

### P1-E (`p1e-hot-leaf-attribution.md`) — ACCEPT (X1′ folded; V1 REJECT stays cleared)

- **§1 / §2.1-§2.5 / §3 / §4 / §5 — ACCEPT.** The c/B posture (`p1e:288-302`) folds X1′:
  "the sub-1.0 CPI … is **NOT physically impossible** … so the earlier 'falsified /
  physically-impossible CPI' characterization was WRONG physics and is withdrawn …
  the residual caveat is only that `ri_cycles` is non-disambiguable … and therefore
  non-load-bearing." The V1 fabricated-line-number REJECT stays CLEARED: every
  `css_canon_bench.rs` line P1-E cites (`:146`, `:250`, `:138-142`, `:160-169`,
  `:183-207`, `:211-247`) matches source. The 91.44%-of-syslib-from-`emit_fact_stream`
  caller attribution is the cleanest allocation-origin proof in the pass; the C4b
  digit-kernel "no benched CSS antecedent → stays orphan-blocked" call and the
  "no number/unicode/dispatch/tape hot leaf" classification are correct; the material
  lightningcss min=121.52 outlier correctly justifies the median statistic. The single
  V2 REVISE (X1′) is discharged.

### P1-F (`p1f-bench-canonical.md`) — ACCEPT (X1′ folded; it ORIGINATED the false framing and now retracts it)

- **§1 / §1.1.1 / §2.1 / §2.1.1 / §2.2 / §2.2.1 / §2.3 / §3 / §4 / §5 — ACCEPT.** P1-F
  was the V1/V2 originator of the "falsified / physically impossible CPI" framing and now
  explicitly retracts it: the V3 fold log (`p1f:33-39`) and §2.2.1 (`p1f:313-344`) state
  "The V2 characterization of this as 'physically impossible' was WRONG PHYSICS and is
  withdrawn here (this row originated it; the correction is load-bearing) … CPI 0.157 ⇔
  IPC 6.4 … cyc/byte stays non-load-bearing … NOT because it is impossible, but because
  it is non-disambiguable." §2.2 reports instr/byte primary + raw cyc/byte. The X2
  authority section (§1.1.1) names `css_canon_bench` THE single canonical harness with
  the comparability caveat, demonstrated by §2.1.1 (two-run ratio stability). The V2
  line-swap and the CH5-V2-R1 wrapper-line cite (`:43`→`:103-105`, grep-verified) are
  folded. lightningcss full-CSSOM breakdown (§2.3) discharges the comparator-plane
  obligation. `tape_activated=false` grep proof (§4.4) re-verified. The single V2 REVISE
  (X1′) is discharged.

---

## §2 — Cross-artefact CH1 dispositions (bind the whole cycle)

### X1′ — ri_cycles posture inversion — RESOLVED (folded; ACCEPT)

The dominant V2 CH1 finding is FOLDED. The pass now carries ONE c/B posture, adopted
verbatim across all six artefacts:

> instr/byte (`ri_instructions`) is the sole load-bearing cost density and is reliable
> to <0.5%. The sub-1.0 CPI from `ri_cycles` is PHYSICAL (IPC 3.7–6.4 on the M5's
> ~8-wide core), NOT impossible; however `proc_pid_rusage.ri_cycles` cannot be
> disambiguated as dynamic core-cycles vs a wall-proportional scaled tick from the
> rusage interface alone, so cyc/byte is reported RAW and non-load-bearing. No
> conclusion rests on it.

CH1 independently re-adjudicated the physics from the on-disk counters (§0.8): `cpi =
cyc/ins` (`:241`); CPI ∈ [0.157, 0.285] ⇔ IPC 3.5–6.4, physical on the ~8-wide M5 core.
The posture is now CORRECT on both halves — it rebuts the false "impossible" framing
(A/B/C/E/F's V2 under-claim) AND withdraws the false "proven 4.27 GHz / supersedes"
framing (P1-D's V2 over-claim). The 5-vs-1 contradiction is gone; the pass is internally
consistent and the cost density S-P2 will key on (instr/byte) is correctly grounded.
No conclusion changed — instr/byte rankings, the 4.4–7.1× fact-stream tax, the >SOTA
ratios, and the recognition-plane masking are all unaffected; only the framing was
corrected. X1′ is RESOLVED.

### X2 — harness comparability — RESOLVED (stays folded; ACCEPT)

`css_canon_bench.rs` is the single canonical harness (P1-F §1.1.1 authority row, echoed
by all five); the comparability caveat (absolute Mbps harness/alloc/CPU-flag-dependent,
only within-harness same-run ratios load-bearing) is stated by all six and demonstrated
by P1-F §2.1.1. P1-A V3 additionally closed the V2 residual (its §2.1 Mbps now sourced
from `css_canon_bench` N=200, not `css_cold_harness`). RESOLVED.

### X3 — aggregate byte count — RESOLVED (stays folded; ACCEPT)

All six use 979638; `wc -c total` = 979638 re-confirmed this cycle. RESOLVED.

---

## §3 — Counts + summary

Dispositioned: 6 artefact-level rolls + 3 cross-artefact (X1′, X2, X3) = **9 CH1 dispositions**.

| Disposition | Count | Items |
|---|---:|---|
| ACCEPT | 9 | P1-A, P1-B, P1-C, P1-D, P1-E, P1-F (all artefact-level, every section); X1′ (ri_cycles posture inversion — folded across all six, physics now correct on both halves); X2 (harness comparability — stays folded + demonstrated); X3 (aggregate bytes 979638 — wc-verified) |
| REVISE | 0 | — none. The single V2 load-bearing REVISE (X1′) folded into all six. |
| REJECT | 0 | — none. No fabricated symbol, no unresolvable hot leaf, no warm/single-sample number, no mis-planed comparator, no fabricated PMU number (logs verbatim-faithful to the decimal). |

**ACCEPT rate (artefact-level): 6/6 clean.** Every CH1 core obligation is met by all six
and re-verified against source this cycle: every hot leaf resolves to a real symbol at
the exact cited `file:line` (generated.rs `:5,:26,:45,:61,:91,:118,:137,:189,:209,:211,
:219,:242,:247,:263,:288,:293-:298,:307,:313,:314,:320,:322-:327,:336,:340,:411,:619,
:628,:629,:636,:640` all exact); every Mbps is an N>=50 cold median with min/max/stddev;
the N>=50 gate is code-asserted (`:250`); both comparator planes are correct
(lightningcss `StyleSheet::parse` full-CSSOM proven materializing by its §2.3 profile,
cssparser token-scan); the PMU logs are verbatim-faithful; the V1 P1-D atos paper-close
stays closed (`atos_v2.txt` 29104 B); the V1 P1-E fabricated-line REJECT stays cleared.

**Convergence read:** zero REJECT, zero REVISE, zero orphan. The dominant V2 open item
(X1′) is folded across all six with the physics independently re-verified by CH1 against
the on-disk counters; X2 and X3 stay resolved. CH1 does NOT block on the JSON-roster skip
(correct CSS-subject override per SYNTHESIS §0.5) nor on the recognition-plane "beats
lightningcss" numbers (correctly disclosed as wrong-plane — recognition-only, no typed
CSSOM — by P1-D §2.3, P1-E §4.1, P1-F §3/§4.1, P1-A §4.1, P1-C A2). **At V3, CH1 returns
6/6 = 100% ACCEPT** — clearing the >=95% ACCEPT bar with zero orphan REVISE for this lens.
