# SK-V17 S-P1 CHALLENGE — CH1 CORRECTNESS (V2)

Lens: CH1 CORRECTNESS. Pass: S-P1 Profile. Cycle: V2. Date: 2026-05-29.
Reviewer scope: every hot leaf resolves to a real symbol (samply/atos); every Mbps is
N>=50 cold median with stddev; comparator planes correct (lightningcss=materializing
full-CSSOM, cssparser=token-scan). Per PASS-1-PROFILE §3 CH1 + ORCHESTRATOR §3W.
Artefacts under review: `research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md` (all dated V2 / 2026-05-29).
Baseline verified: master HEAD `6496fecae` (per artefact frontmatter; SK-V16 close `1c5bd7a25`).

Disposition vocabulary: ACCEPT / REVISE / REJECT. One disposition per artefact-level roll,
plus the cross-artefact CH1 dispositions that bind the whole cycle.

V1 CH1 raised three REVISE items (X1 ri_cycles c/B contradiction, X2 harness comparability,
X3 P1-C aggregate-byte 979642). This V2 review (1) confirms whether those folded, and
(2) re-disposes against fresh state. **A NEW cross-artefact contradiction (X1′) was
introduced in V2 and is the dominant CH1 finding.**

---

## §0 — Verification performed (what CH1 actually checked, not asserted)

1. **Hot-leaf symbol resolution.** Every cited hot leaf re-read at its claimed `file:line`
   in `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
   (646 LOC, verified):
   - `emit_fact_stream` `:5`, `push_hex64(&mut out, fnv64(...))` call at `:26`,
     `emit_declarations(input, &mut out)` call at `:45`, `emit_full_parse` `:61`,
     summary `out.push_str("full_parse\tstatus=accepted\trules=")` at `:91` (counts to `:99`)
     — ALL VERIFIED at the exact lines.
   - `parse_stylesheet` `:118`, `parse_at_rule` `:137`, `parse_block` `:189`,
     `parse_block_item` `:209` (first scan `find_component_delim(self.pos, b"{};")` `:211`),
     `find_colon_before` call `:219`, `parse_declaration` `:242` (value scan
     `find_component_delim(self.pos, b";}")` `:247`), `skip_ws_comments` `:263` — ALL VERIFIED.
   - `find_component_delim` `:288`; inner body VERIFIED: `:293` `while pos < self.bytes.len()`,
     `:294` `let byte = self.bytes[pos]`, `:295` `if delimiters.contains(&byte)`,
     `:298` `pos = match byte`, `:307` `_ => pos + 1`. `find_colon_before` `:313`
     (`find_component_delim(start, b":{};")` `:314`). `consume_balanced_at` `:320`
     (`:323` byte load, `:324` `byte == close`, `:327` `pos = match byte`, `:336` `_ => pos+1`).
   - `emit_declarations` `:411`, `emit_tokens` `:472`, `fnv64` `:619`,
     `push_ascii_lower_hex` `:628` with `Vec::with_capacity(text.len())` at `:629`,
     `push_hex64` `:636`, `push_hex` `:640` — ALL VERIFIED.
   No hot-leaf citation in any V2 artefact resolves to a wrong line or a missing symbol.
   CH1's primary obligation is met by all six. **The V1 CH7 line-swap REVISE (p1f:185;
   `:295` vs `:298`) is FOLDED** — P1-F §2.3/§1.3 and P1-C §2.4 now cite `:295` as the
   `delimiters.contains` membership leaf and `:298` as the `match byte` dispatch, matching
   source. **The V1 CH7 fact-stream-mischaracterization REVISE (p1d:185-188) is FOLDED** —
   P1-D §2.3 now explicitly corrects "metadata-only", states `emit_fact_stream` walks the
   whole source via `emit_declarations` (`:45`→`:411`), and is the most expensive plane.

2. **Canonical harness exists + asserts N>=50.** `css_canon_bench.rs` verified on disk
   (403 lines). `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` at
   `:250` — VERIFIED. `fn mbps` `:138` (`bytes*8/(secs*1e6)`), `fn sample` `:146` (one
   `parse(black_box(input))` per timed window), `CSS_CANON_PROFILE` samply driver `:183`,
   `CSS_CANON_PMU` `:211`, `read_rusage_v5` `:86`, workloads `track1_full_parse:103` /
   `track1_fact_stream:108` / `lightningcss_full_cssom:113` / `cssparser_token_scan:118`,
   `WORKLOADS` table `:124-127` — ALL VERIFIED at the exact lines.
   **The V1/CH6 P1-E "fabricated-precision line numbers" REJECT (P1-F §1.1.1 cited
   `303`/`:150`/`:84-116`) is FOLDED:** P1-E V2 §1.1/§5 now cites `:146`, `:250`, `:138-142`,
   `:160-169`, `:183-207`, `:211-247` — every one VERIFIED against source. P1-E is no longer
   a REJECT.

3. **N>=50 cold median with stddev.** All six carry N>=50 with median/min/max/stddev per
   row (A: N=64+80; B: N=200; C: N=64×2; D: N=64×2; E: N=100+60; F: N=200×2). The PMU
   tables (A §2.1b, B §2.1, C §2.5, D §3.2, F §2.2) are reproduced verbatim against the
   on-disk source logs `/tmp/skv17-p1/css_canon_pmu.txt`, `/tmp/skv17-p1/css_canon_pmu_v2.txt`,
   `/tmp/skv17-p1d-v2-pmu.txt` (all present, 2754 B each; checked to the decimal —
   e.g. bootstrap track1_full `instructions=25010386205 / cycles=6737349921 / cpi=0.2694`
   ⇒ P1-D §3.2 `53.72 i/B, cyc/byte 14.47, IPC 3.71, CPI 0.269` is exact). No single-sample
   number, no warm number, survives in any §2 table.

4. **Comparator planes correct.** lightningcss = `StyleSheet::parse(input,
   ParserOptions::default())` returning `sheet.rules.0.len()` (`css_canon_bench.rs:113`) —
   full-CSSOM build, VERIFIED. P1-F §2.3 profiles it (13,583 samples) and resolves ~38%
   cssparser tokenizer + ~30% typed `Property`/`Selector`/`CssRule` build+drop
   (`parse_selector`, `PropertyId::from_name_and_prefix`, `drop_in_place::<Property>`) — the
   load-bearing PROOF the comparator genuinely materializes. cssparser =
   `cssparser_token_scan` (`:118`) → `CssparserFullParseProbe`, materializing nothing —
   VERIFIED token-scan flaw probe. CH1 accepts both planes (unchanged from V1; correctly
   carried into V2).

5. **Corpus coverage.** All four benched CSS corpora (bootstrap 232803, tailwindcss 179631,
   material 495454, animate 71750; on-disk sizes pinned at `css_l4_corpus.rs:21-54`) + the
   aggregate. The §2.1 17-JSON mandate is correctly overridden for the CSS-tape subject per
   SYNTHESIS §0.5; every artefact justifies the override. CH1 does NOT reject on the
   JSON-roster skip. **The V1 X3 aggregate-byte REVISE is FOLDED:** all six now use 979638
   (raw sum 71750+232803+179631+495454), and P1-C §1.2 explicitly reconciles its V1 979642
   to 979638 (and states the 3-`\n`-separator concat = 979641 is not the figure of record
   because no benched path concatenates). X3 is RESOLVED.

6. **The decisive PMU adjudication (the X1′ crux — physics checked, not asserted).**
   The `cpi` column emitted by the harness is `cycles/instructions` (`css_canon_bench.rs:240`:
   `if ins == 0 { 0.0 } else { cyc as f64 / ins as f64 }`). CPI is cycles-per-instruction;
   **CPI < 1.0 ⇔ IPC > 1.0**, which is NORMAL on a wide superscalar, NOT physically
   impossible. CH1 derived the M5 P-core frequency from the raw counters to test whether
   `ri_cycles` is a genuine cycle counter:
   - bootstrap track1_full: cycles=6.737e9, instructions=25.010e9 ⇒ IPC 3.71 (CPI 0.269).
     Wall = `bytes*8*iters/(mbps*1e6)` = 232803·8·2000/2367.6e6 = 1.573 s ⇒
     6.737e9/1.573 = **4.28 GHz** = the M5 Max P-core clock.
   - bootstrap track1_fact_stream: cycles=19.105e9, instructions=110.570e9 ⇒ **IPC 5.79**;
     19.105e9 / (852.5 Mbps-derived 4.48 s)… the per-workload `ri_cycles/wall` is steady at
     4.19–4.29 GHz across all 16 rows (P1-D §3.1 derivation, independently re-checked here).
   `ri_cycles` IS a real core-cycle counter; the sub-1.0 CPI is high IPC (3.7 on the
   branch-y scan loop, 5.8–6.4 on the inst-dense String/token planes) — entirely physical on
   an ~8-wide Apple core. **This makes P1-D's V2 §3.1 correct and the "falsified /
   physically-impossible" framing in P1-A, P1-B, P1-C, P1-E, P1-F wrong.** See X1′.

---

## §1 — Per-artefact dispositions

### P1-A (`p1a-samply-mode-1.md`) — REVISE (X1′ only)

- **§1 Method / §2.2-§2.3 hot leaves / §3 / §4 / §5 — ACCEPT.** Verbatim samply commands
  (`--save-only --unstable-presymbolicate -r 9999`, `.syms.json` sidecar); every symbol
  resolves at the cited line; the `mach_absolute_time`-via-libmalloc caller attribution
  (25591/25640 leaves from `0x2b483` ∈ libsystem_malloc) is a sound stack-prefix walk; the
  fact-stream "scan masked under alloc" finding is corroborated by the sidecar lacking
  `find_component_delim`. Masking signals cite REDRESS pre-blocks correctly.
- **§2.1 / §2.1b cost surface — REVISE (X1′).** P1-A FOLDED V1-X1: it strikes the
  `ri_cycles` c/B from the authoritative table (struck-through `~~…~~`), adds the §2.1b
  instr/byte table, and the COST-SURFACE NOTE (`p1a:15`) caveats `ri_cycles`. But the
  caveat asserts the falsity claim — `p1a:15`: "the `ri_cycles` surface is **falsified** …
  physically impossible … CPI >= ~0.25 only on idealised wide issue, and never the sub-0.2
  values seen". **This is the wrong physics.** Sub-0.2 CPI is IPC > 5, which the fact-stream
  / cssparser planes genuinely hit on this wide core (verified §0.6). Fix (`p1a:15`,
  `p1a:71`, `p1a:100`): replace "falsified / physically impossible CPI" with the correct
  reading — `ri_cycles` is a valid ~4.27 GHz cycle counter; CPI 0.16–0.28 = IPC 3.6–6.4
  (high IPC on a wide core), NOT a counter fault. instr/byte remains the cleanest
  plane-ranking density (allocator/clock-independent), so the §2.1b adoption stands and the
  conclusions are unaffected; only the JUSTIFICATION is wrong and must be corrected to match
  P1-D §3.1 (the one artefact that got the physics right).

### P1-B (`p1b-samply-mode-2.md`) — REVISE (X1′ only)

- **§1 / §2.2-§2.4 / §3 (re-derived String-tax) / §4 / §5 — ACCEPT.** `--save-only` + atos
  at `0x100000000` correctly invokes the samply-resolution discipline; symbols resolve;
  `find_component_delim` 56.55% / `consume_balanced_at` 11.51% consistent with the other
  four. The V1-X1 "~3× cycles" inference is FOLDED — §3 re-derives the String tax from the
  reliable instr/byte (fact_stream 214–364 i/B vs full_parse 46–58 i/B = 4.4–7.1×), which is
  the correct quantity.
- **§2.1 / c/B-PROVENANCE note — REVISE (X1′).** Same defect as P1-A: `p1b:39-45` and
  `p1b:120-122` call `ri_cycles` "physically-impossible sub-1.0 CPI … a reference-clock tick,
  NOT retired core cycles". Wrong physics. Fix: re-word to "valid cycle counter; sub-1.0 CPI
  = IPC > 1 (high IPC on a wide core)"; keep instr/byte as the load-bearing density. The
  strike of cyc/byte from the authoritative surface is fine; the REASON given is false.

### P1-C (`p1c-samply-mode-3.md`) — REVISE (X1′ only; X3 RESOLVED)

- **§1 / §2.1-§2.4 / §3 / §4 / §5 — ACCEPT.** Per-line self-time at
  `generated.rs:288,293-298,307` and `:320-340` resolves; `find_component_delim` 58.59% /
  `consume_balanced_at` 9.98% consistent. The plane-bifurcation A1 and the recognition-vs-
  materialization masking probe A2 are correct. **X3 FOLDED** — §1.2 reconciles 979642→979638
  with the explicit join-rule statement; CH1's V1-X3 is RESOLVED.
- **§2.5 PMU ledger — REVISE (X1′).** `p1c:38-44` and the §2.5 caption call CPI≪1.0
  "physically impossible … indicates the `ri_cycles` counter is not counting core cycles as
  labelled." Wrong physics (the §2.5 table itself shows IPC 3.6–6.4 in disguise). Fix:
  re-label the CPI column as "CPI = 1/IPC; the IPC 3.6–6.4 is high but physical on a wide
  core; `ri_cycles` is a valid counter"; i/B stays the reliable cost figure.

### P1-D (`p1d-pmu-cycles.md`) — ACCEPT (the physics-correct artefact; one consistency note)

- **§1 / §2.1-§2.5 / §3.2 / §3.3 / §4 / §5 — ACCEPT.** Medians/min/max/stddev correct
  (re-checked vs `/tmp/skv17-p1d-v2-pmu.txt`); run-2 stability disclosed; the §2.4 atos
  artefact (`atos_v2.txt`, 199 resolved `<symbol> (file:line)` lines, 20377 leaf samples)
  closes the V1 0-byte-`atos_out.txt` paper-close (CH6-V1); per-line `find_component_delim`
  breakdown (`:298` 27.88%, `:295` 17.24%) resolves; the redundant 2-3× overlapping re-scan
  (`parse_block_item:211` → `find_colon_before:219/:314` → `parse_declaration:247`) is a real
  verifiable structural finding, correctly bounded against REDRESS-51/53.
- **§3.1 PMU posture — ACCEPT (exemplary, and the correct adjudication).** P1-D is the ONLY
  artefact that gets the PMU physics right: it derives `ri_cycles/wall_s` = a steady
  4.19–4.29 GHz across all 16 rows, shows that is the M5 P-core clock, and reads sub-1.0 CPI
  as IPC 3.6–6.4 (not a counter fault). CH1 independently confirmed this derivation (§0.6).
  This is the posture the pass must adopt — and it is the one P1-A/B/C/E/F contradict.
- **Consistency note (NOT a P1-D defect): §3.1 correctly flags that it "supersedes the
  ri_cycles unreliable line P1-A/P1-B/P1-F carried" — i.e. P1-D itself names the X1′
  contradiction. The fix belongs in the five, not in P1-D.** P1-D's §3.1 sentence
  "That falsification is itself incorrect, and this pass corrects it" is the load-bearing
  correction; CH1 endorses P1-D as the authoritative c/B posture.

### P1-E (`p1e-hot-leaf-attribution.md`) — REVISE (X1′ only; V1 REJECT cleared)

- **§1 / §2.1-§2.5 roll-up / §3 (planes) / §4 / §5 — ACCEPT.** The V1 fabricated-line-number
  REJECT (P1-F §1.1.1) is CLEARED: every `css_canon_bench.rs` line P1-E cites (`:146`, `:250`,
  `:138-142`, `:160-169`, `:183-207`, `:211-247`) now matches source. The 91.44%-of-syslib-
  from-`emit_fact_stream` caller attribution is the cleanest allocation-origin proof in the
  pass; the "no number/unicode/dispatch/tape hot leaf" classification and the C4b digit-kernel
  "no benched CSS antecedent → stays orphan-blocked" call are correct and well-grounded; the
  material lightningcss min=121.52 outlier is correctly used to justify the median statistic.
- **§3 c/B posture — REVISE (X1′).** `p1e:288-294`: "the `proc_pid_rusage` `ri_cycles` surface
  is **falsified** … physically-impossible CPI 0.16–0.28". Wrong physics. Fix: adopt the P1-D
  §3.1 reading (valid cycle counter, sub-1.0 CPI = high IPC); P1-E carries no c/B column so the
  edit is one sentence — but it must not assert the falsity, because that is the contradiction.

### P1-F (`p1f-bench-canonical.md`) — REVISE (X1′ only)

- **§1 / §1.1.1 (harness verdict) / §2.1 / §2.1.1 / §2.3 / §3 / §4 / §5 — ACCEPT.** P1-F is the
  X2 authority and FOLDED it cleanly: §1.1.1 names `css_canon_bench.rs` THE single canonical
  harness (the four others superseded), states the comparability caveat, and §2.1.1
  demonstrates within-harness ratio stability across two runs. The V1 line-swap REVISE is
  FOLDED (§1.3 grep-verifies `:295`/`:298`/`:307`). The lightningcss full-CSSOM breakdown
  (§2.3) discharges the comparator-plane obligation for the whole pass. The `tape_activated=false`
  grep proof (§4.4) is a clean empirical close-gate baseline. atos RVAs verified
  (`0x215848 → emit_fact_stream :45`, `0x215dc0 → push_ascii_lower_hex :633`).
- **§2.2 / §2.2.1 PMU posture — REVISE (X1′).** P1-F was the V1 artefact that ORIGINATED the
  "falsified ri_cycles" framing, and V2 doubles down: `p1f:298-302` "Every one of the 16 rows
  reports CPI in [0.157, 0.285] — uniformly sub-1.0. A retired-instruction CPI below 1.0 is
  physically impossible on M5 (it would require retiring >3.5 instructions per core cycle
  sustained, exceeding the decode/retire width)." **The premise is wrong: >3.5 instr/cycle
  is NOT beyond an ~8-wide M5 core; it is exactly the IPC such a core delivers on a tight
  loop.** Fix (`p1f:289-307`, `p1f:486-493`): replace the falsification with the P1-D §3.1
  reading. instr/byte remains the cleanest density and the §2.2 ranking is unaffected; the
  cyc/byte column should be retained as a VALID counter (IPC-explained), not struck as
  "unreliable". This is the single substantive REVISE on P1-F.

---

## §2 — Cross-artefact CH1 dispositions (bind the whole cycle)

### X1′ — ri_cycles POSTURE INVERSION: 5-vs-1 split, the majority physics is WRONG — REVISE (must fold)

This is the dominant CH1 finding of V2 and a regression in correctness relative to the V1
intent. V1-X1 asked the pass to adopt ONE c/B posture. V2 produced TWO, in direct
contradiction:

- **P1-D §3.1 (correct):** `ri_cycles` is a valid ~4.27 GHz core-cycle counter; sub-1.0 CPI
  = IPC 3.6–6.4, high but physical on a wide core; cyc/byte is a co-reported validated figure.
- **P1-A §2.1/note, P1-B §2.1/note, P1-C §2.5/note, P1-E §3, P1-F §2.2/§2.2.1 (incorrect):**
  `ri_cycles` is "falsified / physically impossible / a reference-clock tick, not core cycles".

CH1 adjudicated the physics directly from the on-disk counters
(`/tmp/skv17-p1d-v2-pmu.txt`, `/tmp/skv17-p1/css_canon_pmu{,_v2}.txt`, all present):
`cpi = cyc/ins` (harness `:240`); CPI < 1.0 ⇔ IPC > 1; `ri_cycles/wall` = 4.19–4.29 GHz
steady (the M5 P-core clock). **The five are wrong; P1-D is right.** "CPI below 1.0 is
physically impossible" confuses CPI with IPC — an ~8-wide superscalar retiring 3.7–6.4
instructions per cycle on a hot, well-predicted scan/string loop yields CPI 0.16–0.27 as a
matter of course. This is not a stylistic disagreement: it is a factual-correctness defect,
and the MAJORITY of the pass ships the false claim. S-P2 keys cost on this pass — five
artefacts telling it "the cycle counter is broken" when it is a valid 4.27 GHz counter is
exactly the kind of mis-grounding CH1 exists to stop.

**Fix:** the five artefacts (A/B/C/E/F) adopt the P1-D §3.1 posture verbatim — `ri_cycles`
is a valid cycle counter; sub-1.0 CPI = high IPC (3.6–6.4), not a counter fault. The pass
adopts ONE posture: **instr/byte is the primary plane-ranking density (allocator/clock-
independent); cyc/byte is a co-reported, counter-measured, wall-cross-validated figure with
IPC reported explicitly so no reader mis-reads sub-1.0 as a defect.** Note: this does NOT
change any conclusion — instr/byte rankings, the 4.4–7.1× fact-stream tax, the >SOTA ratios,
and the recognition-plane masking are all unaffected. Only the false "falsified counter"
framing must go. Orphan-REVISE risk: if X1′ is not folded, S-P2 inherits a pass that calls
its own valid cycle counter broken, and the cyc/byte the `gate-json` consumer wants is left
mis-labelled "unreliable" when it is in fact usable.

### X2 — harness comparability — RESOLVED (folded; ACCEPT)

V1-X2 (five harness binaries, no comparability caveat) is FOLDED across the pass:
`css_canon_bench.rs` is designated THE single canonical harness by P1-F §1.1.1 (the
bench-authority row), echoed by P1-A note, P1-B §2.4, P1-C V2-note, P1-D §1.2, P1-E §1.1;
the comparability caveat (absolute Mbps harness/alloc/CPU-flag-dependent, only within-harness
same-run ratios load-bearing) is stated in all six and DEMONSTRATED by P1-F §2.1.1 (two-run
ratio stability). CH1 ACCEPTs the X2 fold. (Minor residue, sub-REVISE: absolute medians still
differ across A=N64-cold-harness / B=N200 / C=N64 / D=N64-mimalloc / E=N100 / F=N200, because
each agent ran a different N / allocator / CPU-flag on `css_canon_bench` or a sibling — but
every artefact now explicitly says only the within-harness ratio is load-bearing, so the
dispersion is disclosed, not hidden. Not a CH1 defect.)

### X3 — aggregate byte count — RESOLVED (folded; ACCEPT)

V1-X3 (P1-C used 979642) is FOLDED: P1-C §1.2 reconciles to 979638 with the explicit
join-rule note; all six artefacts now use 979638. CH1 ACCEPTs.

---

## §3 — Counts + summary

Dispositioned: 6 artefact-level rolls + 3 cross-artefact (X1′, X2, X3) = **9 CH1 dispositions**.

| Disposition | Count | Items |
|---|---:|---|
| ACCEPT | 3 | P1-D (artefact-level, every section — the physics-correct PMU posture); X2 (harness comparability, folded + demonstrated); X3 (aggregate bytes, folded) |
| REVISE | 6 | P1-A §2.1/note (X1′); P1-B §2.1/note (X1′); P1-C §2.5/note (X1′); P1-E §3 (X1′); P1-F §2.2/§2.2.1 (X1′); X1′ cross-artefact ri_cycles posture inversion |
| REJECT | 0 | — none. No fabricated symbol, no unresolvable hot leaf, no warm/single-sample number, no mis-planed comparator. The V1 P1-E line-number REJECT is CLEARED. |

**ACCEPT rate (artefact-level): 1/6 clean (P1-D); 5/6 carry the SAME single REVISE (X1′).**
The 5/6 figure looks worse than V1's 3/6, but every REVISE is the IDENTICAL one-cause defect
(the false "ri_cycles falsified" physics), and three of the V1 dispositions (X1 original, X2,
X3) plus both V1-CH7 REVISEs plus the V1 P1-E REJECT all FOLDED cleanly. The CH1 *core*
obligations — every hot leaf to a real symbol at the cited file:line, every Mbps an N>=50
cold median with stddev, both comparator planes correct/verified, N>=50 code-asserted — are
MET by all six.

**The single load-bearing REVISE (X1′)** is the ri_cycles posture inversion: P1-D corrected
the physics (sub-1.0 CPI = high IPC, valid counter) but the other five still ship the V1
"falsified / physically impossible" framing, so the pass now contradicts itself 5-vs-1 and
the majority holds the FALSE claim. This must fold into V3 (all five adopt P1-D §3.1) before
S-P1 converges. It is the one CH1 defect with downstream consequence (S-P2 grounds cost
density on this pass; the cyc/byte the `gate-json` consumer wants is mis-labelled unreliable).
No conclusion changes — only the false framing must be removed.

**Convergence read:** zero REJECT. X2 and X3 RESOLVED. X1′ is the only open CH1 item and it
is orphan-free (one concrete fix: five artefacts adopt the P1-D physics). CH1 does NOT
block on the JSON-roster skip (correct CSS-subject override) nor on the recognition-plane
"beats lightningcss" numbers (correctly disclosed as wrong-plane by P1-D §2.3, P1-E §4.1,
P1-F §3, P1-A §4.1, P1-C A2). At V3, with X1′ folded, CH1 reaches >=95% ACCEPT.
