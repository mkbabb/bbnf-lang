# SK-V17 S-P1 CHALLENGE — CH1 CORRECTNESS (V1)

Lens: CH1 CORRECTNESS. Pass: S-P1 Profile. Cycle: V1. Date: 2026-05-29.
Reviewer scope: every hot leaf resolves to a real symbol (samply/atos); every Mbps
is N>=50 cold median with stddev; comparator planes correct (lightningcss=materializing
full-CSSOM, cssparser=token-scan). Per PASS-1-PROFILE §3 CH1 + ORCHESTRATOR §3W.
Artefacts under review: `research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md`.
Baseline verified: master HEAD `6496fecae` (confirmed `git rev-parse HEAD`).

Disposition vocabulary: ACCEPT / REVISE / REJECT. One disposition per artefact
section, plus three cross-artefact CH1 dispositions that bind the whole cycle.

---

## §0 — Verification performed (what CH1 actually checked, not asserted)

1. **Hot-leaf symbol resolution (the core CH1 obligation).** Every cited hot-leaf
   symbol was read at its claimed `file:line` in
   `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
   (646 LOC, verified):
   - `find_component_delim` `generated.rs:288` — VERIFIED; hot inner
     `delimiters.contains(&byte)` is at `:295` (every artefact's `:295` claim is
     exact), per-byte `match byte` dispatch at `:298` VERIFIED.
   - `consume_balanced_at` `generated.rs:320` — VERIFIED.
   - `emit_fact_stream` `:5`, `emit_full_parse` `:61`, `parse_stylesheet` `:118`,
     `parse_block` `:189`, `parse_declaration` `:242`, `parse_at_rule` `:137` — ALL
     VERIFIED at the exact lines.
   - `push_ascii_lower_hex` `:628` with `Vec::with_capacity(text.len())` at `:629`
     (P1-B `:629` per-token-alloc claim) — VERIFIED.
   - `emit_declarations` `:411`, `emit_tokens` `:472`, `fnv64` `:619`,
     `push_hex64` `:636`, `push_hex` `:640`, and the `emit_declarations(...)` call
     site at `:45` (P1-C/P1-F `generated.rs:45` self-time line) — ALL VERIFIED.
   No hot-leaf citation in any artefact resolves to a wrong line or a missing symbol.
   CH1's primary obligation is met by all six.

2. **Profile artefacts exist on disk (CH6-adjacent but CH1 confirms symbol
   resolvability).** `/tmp/skv17-p1/{fact,full}-{bootstrap,tailwind}.json.gz` +
   `.syms.json` sidecars, `/tmp/skv17-p1/{fact_stream,full_parse,lightningcss}.json.gz`,
   `/tmp/skv17-p1d/track1.json.gz` (165,122 B), `/tmp/skv17-p1e/{fact_stream,full_parse}.json.gz`
   all present. P1-A's `fact-bootstrap.json.syms.json` sidecar contains
   `mach_absolute_time`, `emit_fact_stream`, `push_ascii_lower_hex`, `reserve` and
   correctly does NOT contain `find_component_delim` (scan masked on the fact-stream
   plane) — the symbol table backs the attribution.

3. **N>=50 cold median with stddev.** All six carry N>=50 (A:64/80, B:50/60,
   C:100/50, D:64, E:100/60, F:200/100), the `median` statistic, and min/max/stddev
   per row. Both harness asserts confirmed in source: `css_canon_bench.rs:250` and
   `css_cold_harness.rs:316` (`assert!(n >= 50, ...)`), `css_cold_canonical.rs:35`
   (`const SAMPLES = 64`). The §2.1 tables were checked verbatim against their source
   logs (P1-A's table == `/tmp/skv17-p1/cold-N64.txt` to the decimal). No single-sample
   number, no warm number, survives in any §2 table.

4. **Comparator planes correct (the explicit CH1 ask).**
   - lightningcss = `StyleSheet::parse(input, ParserOptions::default())` returning
     `sheet.rules.0.len()` (`css_canon_bench.rs:113-115`) — full-CSSOM build, VERIFIED
     materializing. P1-F §2.3 profiles it (13,583 samples) and resolves ~38% cssparser
     tokenizer + ~30% typed `Property`/`Selector`/`CssRule` build+drop
     (`parse_selector`, `PropertyId::from_name_and_prefix`, `drop_in_place::<Property>`)
     — this is the load-bearing PROOF the comparator genuinely materializes and is the
     correct >SOTA bar, not a token-scan. CH1 accepts the lightningcss plane.
   - cssparser = `StyleSheetParser` + `RuleBodyParser` walking all component values
     (`css_canon_bench.rs:282-303`), materializing nothing — VERIFIED token-scan flaw
     probe. CH1 accepts the cssparser plane.

5. **Corpus coverage.** All four benched CSS corpora (bootstrap 232803, tailwindcss
   179631, material 495454, animate 71750; on-disk sizes VERIFIED against
   `css_l4_corpus.rs:21-58` pins) + the aggregate. The §2.1 17-JSON mandate is
   correctly overridden for the SK-V17 CSS-tape subject per SYNTHESIS §0.5; every
   artefact justifies the override explicitly. CH1 does NOT reject on the JSON-roster
   skip — the subject is CSS L4, and 4/4 CSS is the correct anti-overfit set here.

---

## §1 — Per-artefact dispositions

### P1-A (`p1a-samply-mode-1.md`)

- **§1 Method — ACCEPT.** Verbatim commands, `samply 0.13.1 --save-only
  --unstable-presymbolicate` with `.syms.json` sidecar resolution; build flags
  (`--profile bench`, debug=true, lto fat) named. Reproducible.
- **§2.1 cold table — REVISE.** The Mbps medians/min/max/stddev are correct and match
  `cold-N64.txt` verbatim. DEFECT: the `c/B` column (track1_full ~13-15, lightningcss
  ~29-33) is the `ri_cycles`-delta / bytes figure from `proc_pid_rusage(RUSAGE_INFO_V5)`,
  presented as authoritative — but P1-F §2.2 and P1-D §3 both prove this very surface is
  UNRELIABLE on M5 (CPI 0.16-0.28, sub-1.0 = physically impossible for retired-cycle CPI;
  `ri_cycles` is a reference-clock tick, not core cycles). P1-A's c/B ~14.7 for
  track1_full is numerically identical to P1-F's raw-cyc/byte 14.42 marked `⚠ UNRELIABLE`.
  Fix (`p1a.md:69-90`): mark the `c/B` column UNRELIABLE with the same CPI<1.0 caveat
  P1-F/P1-D carry, or drop it and cite instr/byte; do not present `ri_cycles` c/B as a
  trustworthy cost figure when sibling artefacts in the same pass falsify it.
- **§2.2/§2.3 hot leaves — ACCEPT.** All symbols resolve at cited lines; the
  `mach_absolute_time`-via-libsystem_malloc caller attribution (`p1a.md:121,129`) is a
  legitimate, well-reasoned stack-prefix walk (25591/25640 leaves from a libmalloc
  caller) and the sidecar contains the symbol. The fact-stream "scan masked under alloc"
  finding is corroborated by the sidecar lacking `find_component_delim`.
- **§3/§4/§5 — ACCEPT.** Deltas honest; masking signals cite REDRESS pre-blocks
  correctly; sources enumerated.

### P1-B (`p1b-samply-mode-2.md`)

- **§1 Method — ACCEPT.** `--save-only` + atos post-symbolication at `0x100000000`,
  correctly invokes the `samply-symbol-resolution` discipline.
- **§2.1 cold table — REVISE.** Same `c/B` defect as P1-A: the `c/B` column
  (`p1b.md:89-110`, track1_full 14.0-15.5, lightningcss 28-44) is the unreliable
  `ri_cycles` figure (PMU surface explicitly `profile_direct.rs:55` `ri_cycles`,
  `p1b.md:23`). §3 (`p1b.md:239`) even draws an inference FROM this c/B ("~3x of the
  fact-stream cycles are the String building") that is built on the falsified counter.
  Fix: caveat the c/B column UNRELIABLE per the CPI<1.0 finding, or re-derive the "~3x"
  claim from the reliable instr/byte (P1-F §2.2 has it: fact_stream 234-364 i/B vs
  full_parse 46-58 i/B = ~4.4x, NOT ~3x — the reliable counter even changes the number).
- **§2.2/§2.3/§2.4 hot leaves — ACCEPT.** Symbols resolve; `find_component_delim` 56.55%
  / `consume_balanced_at` 11.51% consistent with the other four profilers.
- **§3/§4/§5 — ACCEPT.**

### P1-C (`p1c-samply-mode-3.md`)

- **§1 Method — ACCEPT.** `--no-open --rate 4000` (correctly NOT `--save-only` per the
  memory), atos+rustfilt at `0x100000000`. Reproducible.
- **§2.1/§2.2 cold tables — REVISE.** Mbps medians/stddev correct and match
  `skv17-p1c-bench-run1.txt`. DEFECT (minor, but a CH1 numeric-correctness error): the
  aggregate byte count is stated as **979 642** (`p1c.md:82,129,82` "232803+179631+495454+71750+4 separators")
  whereas the raw sum is **979 638** and four files joined with `\n` give THREE
  separators = 979 641, not 979 642 — and every other artefact (P1-A/B/D/E/F) uses
  979 638. The aggregate Mbps is bytes-normalized, so a 4-byte error on ~980 KB is
  <0.0005% on throughput (negligible to the medians) but the byte count itself is
  arithmetically wrong and inconsistent cross-pass. Fix (`p1c.md:82`): reconcile to
  979 638 (raw concat, no trailing/extra separator) OR state the exact join rule and
  correct the arithmetic (3 separators → 979 641); align with the other five artefacts.
- **§2.3/§2.4 hot leaves — ACCEPT.** Per-line self-time at `generated.rs:288,294-298,307,311`
  and `:320,323,327,336,340` resolve; `find_component_delim` 58.11% / `consume_balanced_at`
  10.51% consistent.
- **§3/§4/§5 — ACCEPT.** A4 aggregate-min 157.81 outlier honestly flagged as cold
  first-touch (correct — median robust).

### P1-D (`p1d-pmu-cycles.md`)

- **§1 Method — ACCEPT.** Two bins (`css_cold_canonical.rs`, `css_track1_profile.rs`),
  N=64, verbatim build/run.
- **§2.1/§2.2 cold tables — ACCEPT.** Medians/min/max/stddev correct; run-2 concurrency
  contamination honestly disclosed.
- **§2.3 plane caveat — ACCEPT (exemplary).** The "track1 beats lightningcss is
  WRONG-PLANE" framing is the most correct plane statement in the pass: it names that
  `emit_full_parse` is recognition+count only and the apparent 2-2.7x margin is a
  plane-mismatch, not a >SOTA admit. This is exactly the CH1/CH(plane) discipline.
- **§2.4/§2.5 hot leaves — ACCEPT.** Per-line `find_component_delim` breakdown (`:295`
  `slice::contains` 17.07%, `:298` match 30.40%) resolves; the redundant 2-3x overlapping
  re-scan finding (`parse_block_item:211` → `find_colon_before:219/:314` →
  `parse_declaration:247`) is a real, verifiable structural-correctness observation.
- **§3 c/B — ACCEPT.** P1-D is the artefact that handles c/B CORRECTLY: it declines the
  `ri_cycles` PMU surface ("no kperf entitlement"), derives c/B from wall-time with an
  EXPLICIT caveat that it is wall-derived not PMU-counter, and flags the gap. This is the
  correct posture the c/B in P1-A/B should adopt.
- **§4/§5 — ACCEPT.**

### P1-E (`p1e-hot-leaf-attribution.md`)

- **§1 Method — ACCEPT.** `css_canon_bench` N=100, atos at `0x100000000`,
  syslib-caller stack-walk attribution (`caller.py`) is sound.
- **§2.1 cold table — ACCEPT.** Medians/stddev correct; material lightningcss min=121.52
  outlier correctly flagged as a single page-fault window with the median robust — the
  textbook justification for the median statistic.
- **§2.3/§2.4/§2.5 hot-leaf roll-up — ACCEPT (the load-bearing P1-E deliverable).**
  Every benched-CSS hot leaf resolved to symbol + %self + file:line + class
  (scan/structural/string/tape/alloc). The 91.44%-of-syslib-from-`emit_fact_stream`
  caller attribution is the cleanest allocation-origin proof in the pass. The "no
  number/unicode/dispatch/tape hot leaf" classification and the C4b digit-kernel
  "no benched CSS antecedent → stays orphan-blocked" call are correct and well-grounded.
- **§3/§4/§5 — ACCEPT.**

### P1-F (`p1f-bench-canonical.md`)

- **§1 Method — ACCEPT.** `css_canon_bench` N=200 (highest N in the pass), atos
  resolution verified to specific RVAs (`0x215848 → emit_fact_stream generated.rs:45`,
  `0x215dc0 → push_ascii_lower_hex generated.rs:633`) — the `:45` call site VERIFIED in
  source; the comparator-plane disclosure (lightningcss CSSOM-build-only, NOT the
  conflated `lightningcss_facts` projection at `nonjson_css_l4.rs:636`) is precise.
- **§2.1/§2.2 — ACCEPT.** Medians/stddev correct. §2.2 is the artefact that gets PMU
  RIGHT: it reports instr/byte as the RELIABLE counter and marks every `ri_cycles`
  cyc/byte cell `⚠ UNRELIABLE` with the CPI<1.0 proof. This is the correct treatment;
  P1-A/B must converge to it.
- **§2.3 hot leaves — ACCEPT.** The lightningcss full-CSSOM breakdown is the materialize
  proof that discharges the CH1 "comparator plane correct" obligation for the whole pass.
- **§3/§4/§5 — ACCEPT.** The "~70 Mbps / ~14x" narrative correctly classified N-direct
  (no fresh benched antecedent); the `tape_activated=false` grep proof
  (`p1f.md:331-336`) is a clean empirical close-gate baseline.

---

## §2 — Cross-artefact CH1 dispositions (bind the whole cycle)

### X1 — `ri_cycles` c/B contradiction across artefacts — REVISE (must fold)

P1-A §2.1 and P1-B §2.1 present `ri_cycles`-derived **cycles-per-byte** as authoritative
columns; P1-F §2.2 and P1-D §3 independently PROVE that exact surface is unreliable
(CPI 0.16-0.28, sub-1.0 impossible; `ri_cycles` is a reference-clock tick, confirmed in
`/tmp/skv17-p1/css_canon_pmu.txt` — e.g. bootstrap track1_full cpi=0.2686). The same
PMU number (14.42 raw) appears UNCAVEATED in P1-A (c/B 14.70) and CAVEATED-as-unreliable
in P1-F. A profile pass cannot ship the same counter as both trustworthy and falsified.
This is a CH1 correctness contradiction, not a stylistic one — S-P2 keys c/B off this
pass. **Fix:** P1-A/P1-B mark the c/B column UNRELIABLE (or replace with wall-derived
c/B + caveat per P1-D, or instr/byte per P1-F); the pass adopts ONE c/B posture — the
reliable instr/byte (P1-F §2.2) plus a wall-derived c/B with explicit caveat (P1-D §3).
Orphan-REVISE risk: if X1 is not folded, S-P2 inherits a c/B the pass itself falsifies.

### X2 — five harness binaries, large same-plane dispersion, no comparability caveat — REVISE

The six artefacts authored FIVE distinct harness bins (`css_cold_harness`,
`css_canon_bench`, `css_cold_bench`, `css_cold_canonical`, `css_track1_profile`; all
present in `crates/bbnf-bench/src/bin/`). For the SAME plane/corpus the medians diverge
well beyond each artefact's own stddev band — e.g. tailwind `track1_full` median across
A/B/C/D/E = 2334 / 2222 / 2870 / 2656 / 2414 Mbps (a ~29% spread, each with stddev
~80-190); bootstrap `track1_fact` = 696 / 736 / 850 / — / 720 (P1-C's 850 is ~15% above
P1-A's 696). The medians are each internally correct (verified vs source logs) and the
RATIO to lightningcss is stable (~0.6-0.8x fact, ~2-3.6x full across all five), so the
QUALITATIVE conclusions hold — but no artefact states that absolute Mbps are
NOT directly comparable across the five harnesses (different alloc — mimalloc in P1-D vs
default elsewhere; different `target-cpu=native` in P1-A/D vs host-portable in P1-F/B;
different sample drivers). **Fix:** the consolidation (and ideally each §2) must (a) name
the ONE canonical harness S-P2/S-P3 binds (the pass proposes `css_canon_bench` —
N>=50, used by P1-E/P1-F), and (b) carry a one-line caveat that cross-artefact absolute
Mbps differ by harness/alloc/CPU-flag and only the within-harness ratio is load-bearing.
This is a correctness-of-comparability gap, not a wrong number.

### X3 — aggregate byte-count inconsistency — REVISE (P1-C only)

P1-C uses 979 642 for the aggregate; P1-A/B/D/E/F use 979 638 (the raw concat sum,
verified arithmetic). Throughput impact is negligible (<0.0005%), but the byte count is
the denominator of a reported Mbps and must be consistent and arithmetically correct
across the pass. **Fix:** P1-C reconciles to 979 638 (or states + corrects the
separator rule; 4 files / 3 `\n` = 979 641, still not 979 642).

---

## §3 — Counts + summary

Sections dispositioned: 6 artefacts × (§1–§5) treated as 6 artefact-level rolls + 3
cross-artefact dispositions = **9 CH1 dispositions**.

| Disposition | Count | Items |
|---|---:|---|
| ACCEPT | 4 | P1-D, P1-E, P1-F (artefact-level, every section); plus all hot-leaf-symbol + comparator-plane + N>=50-median obligations pass-wide |
| REVISE | 5 | P1-A §2.1 c/B (X1); P1-B §2.1 c/B + §3 derived-claim (X1); P1-C §2.1/2.2 aggregate bytes (X3); X1 cross-artefact c/B contradiction; X2 cross-artefact harness-comparability caveat |
| REJECT | 0 | — none. No fabricated symbol, no unresolvable hot leaf, no warm/single-sample number, no mis-planed comparator. |

**ACCEPT rate (artefact-level):** 3/6 artefacts clean (P1-D, P1-E, P1-F); 3/6 carry a
REVISE (P1-A, P1-B on the shared c/B defect; P1-C on aggregate bytes). No artefact is
rejected. The CH1 *core* obligations — every hot leaf to a real symbol at the cited
file:line, every Mbps an N>=50 cold median with stddev, both comparator planes correct
and verified-materializing/token-scanning — are MET by all six.

**The single load-bearing REVISE (X1)** is the `ri_cycles` c/B contradiction: P1-A/P1-B
present as authoritative the exact PMU counter P1-D/P1-F prove unreliable. This must fold
into V2 (one c/B posture pass-wide) before S-P1 converges; it is the one CH1 defect with
downstream consequence (S-P2 grounds primitive cost on this pass's c/B). X2 (harness
comparability caveat) and X3 (P1-C bytes) are lower-severity REVISEs.

**Convergence read:** zero REJECT and no orphan REVISE provided X1/X2/X3 fold into V2.
CH1 does not block on the JSON-roster skip (correct CSS-subject override) nor on the
recognition-plane "beats lightningcss" numbers (correctly disclosed as wrong-plane by
P1-D §2.3, P1-E §4.1, P1-F §3, P1-A §4.1).
