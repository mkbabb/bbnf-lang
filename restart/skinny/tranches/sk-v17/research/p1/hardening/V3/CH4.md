# SK-V17 S-P1 CHALLENGE — CH4 COST (V3)

Lens: CH4 COST. Cycle: V3. Date: 2026-05-29.
Charter (dispatched): every named hot leaf carries a measured % self-time + the
candidate primitive it grounds; no speculative kernel without a profiled
antecedent. (This is the COST/hot-leaf-grounding charter the orchestrator
dispatched; distinct from PASS-1-PROFILE §3's CH4 "reproducibility" text —
reproducibility is folded in as `[repro-as-cost]` where it bears on whether a
cost number is real.)
Subject: SK-V17 S-P1 PROFILE artefacts
`restart/skinny/tranches/sk-v17/research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md`.
Baseline re-verified against benched source at master HEAD `6496fecae` (working tree).
Prior cycles: `hardening/V1/CH4.md` (87.8% ACCEPT; 5 REVISE — CH4-1 ×3, CH4-2, CH4-3);
`hardening/V2/CH4.md` (90.5% ACCEPT; 4 REVISE — all single-rooted CH4-4).
Output: this file.

Disposition vocabulary: ACCEPT / REVISE / REJECT. One row per artefact §-section.

---

## §0 — Source verification performed (the cost-grounding floor, re-run this cycle)

Every load-bearing hot-leaf symbol + line cited across the six V3 artefacts was
re-resolved against the benched tree this cycle. All resolve exactly:

| Cited symbol / claim | Cited line | Verified against source | Status |
|---|---|---|---|
| `emit_fact_stream` header | generated.rs:5 | `pub fn emit_fact_stream(input: &str) -> Result<String, …>` :5 | OK |
| `push_hex64(fnv64(input))` (P1-C/P1-F `:26`) | generated.rs:26 | `push_hex64(&mut out, fnv64(input.as_bytes()));` :26 | OK |
| `emit_declarations(input,&mut out)` (P1-C/P1-F `:45`) | generated.rs:45 | `emit_declarations(input, &mut out);` :45 | OK |
| `emit_full_parse` | generated.rs:61 | `pub fn emit_full_parse(…)` :61 | OK |
| `find_component_delim` body | generated.rs:288-311 | fn :288; `let byte = self.bytes[pos]` :294; `if delimiters.contains(&byte)` :295; `return Ok(Some(...))` :296; `pos = match byte` :298; `_ => pos + 1` :307 | OK (every per-line cite exact) |
| `consume_balanced_at` body | generated.rs:320-340 | fn :320; `let byte=self.bytes[pos]` :323; `if byte == close` :324; `pos = match byte` :327; `_ => pos + 1` :336 | OK |
| **shared-inner-loop collapse claim** | :293-308 ≡ :322-338 | the two `match byte` arms are BYTE-IDENTICAL (`'\''|'"'`→string, `/`→comment, `(`/`[`/`{`→`consume_balanced_at`, closing→err, `_`→`pos+1`); differ ONLY at the membership test (`delimiters.contains` :295 vs `byte == close` :324) | **OK — collapse is SOURCE-TRUE** |
| `push_ascii_lower_hex` + per-token alloc | generated.rs:628-634 | fn :628; `let mut buf = Vec::with_capacity(text.len());` :629; lowercase loop :630-632; `push_hex(out,&buf)` :633 | OK |
| `TapeBuilder` / `push_plain_offset` | assembler.rs:42 / :71 | :42 / :71 | OK |
| `select_classifier` / `lo6_table_admissible` | dispatch.rs:42 / :101 | `pub fn select_classifier(alphabet: &'static [u8;64])` :42; `fn lo6_table_admissible` :101 | OK |
| **`byte_class_index_64` / `to_bitmask64`** (the to-build NEON kernel) | dispatch.rs (named "to-build") | `grep -rn` returns ZERO defs in `bbnf-simd/src/` | **OK — correctly named as to-build, NOT a fabricated extant symbol** |
| `parse_4_digits_dotprod` (udot orphan, C4b) | digit_mac.rs:27 | `pub unsafe fn parse_4_digits_dotprod(bytes: [u8;4]) -> u32` :27 | OK |
| JSON byte-class primitive (CH2 anchor) | json/scan.rs:219 | `classify_structural_terminator_block_from_table(` :219 | OK |
| canon harness `assert!(n>=50)` | css_canon_bench.rs:250 | `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)");` :250 | OK |
| canon harness `fn sample` | css_canon_bench.rs:146 | `fn sample(parse: ParseFn, input: &str, n: usize) -> Stats` :146 | OK |
| harness CPI = `cyc/ins` | css_canon_bench.rs:241 | `if ins == 0 { 0.0 } else { cyc as f64 / ins as f64 }` :241 | OK |
| `track1_full_parse` wrapper (P1-F V3 `:103-105`) | css_canon_bench.rs:103 | `fn track1_full_parse(input: &str) -> u64 {` :103 | **OK (P1-F's CH5-V2-R1 `:43`→`:103` correction is right)** |

**Cost-grounding verdict on the named leaves (charter substance — MET, and now
internally consistent).** Each benched-CSS hot leaf carries a measured %self-time
on ≥3 independent profiles AND grounds a named candidate primitive whose antecedent
is the measured leaf:
- `find_component_delim` 56.52–65.05% (+ `consume_balanced_at` 9.98–11.51% folded —
  SOURCE-VERIFIED identical inner loop) = ~68% of recognition self-time → ONE NEON
  byte-class-membership scan kernel (`byte_class_index_64`/`to_bitmask64` to-build,
  routed through `select_classifier` dispatch.rs:42), gated behind tape activation.
- fact-stream alloc floor ~58–80% + `emit_fact_stream` 19.59–25.13% +
  `push_ascii_lower_hex` 7.13–9.11% → `TapeBuilder::push_plain_offset` tape append
  (assembler.rs:71). Reliable magnitude restated on instr/byte: fact_stream 214–366
  i/B vs full_parse 46–58 i/B = 4.4–7.1× (the String tax).
- lightningcss comparator plane (`cssparser::consume_name` 8.92%, `skip_whitespace`
  5.88%, `drop_in_place::<Token>` 3.95%, P1-F §2.3) → grounds the FAIRNESS of the
  >SOTA bar as a measured cost claim (the comparator demonstrably materializes the
  CSSOM; it is not a token-scan strawman).

No leaf is named without a number; no proposed primitive lacks a profiled antecedent;
the one kernel without a CSS antecedent (the udot/i8mm digit kernel `digit_mac.rs:27`,
C4b) is explicitly orphan-blocked by P1-E §4.4 with a named re-admission condition.

---

## §1 — V2 REVISE fold audit (the convergence test)

V2 carried exactly five dispositions touching cost-grounding: four single-rooted
CH4-4 REVISEs and one ACCEPT-advisory CH4-5. All are folded:

| V2 finding | Required fold | V3 status |
|---|---|---|
| **CH4-4** (P1-C §intro/§2.5 false "physically impossible") | replace with the agreed posture (physical = high IPC; non-disambiguable; non-load-bearing) | **FOLDED.** P1-C `:37-57` blockquote carries the agreed verbatim posture; `:47-49` states "wrong physics: it confused CPI with IPC … CPI 0.16 ⇔ IPC 6.4"; §2.5 `:286-293` re-headed "sub-1.0 CPI is PHYSICAL … set aside … disambiguability, not physics." No "impossible" survives. |
| **CH4-4** (P1-D §3.1 non-probative "proven 4.27 GHz" + "supersedes A/B/F") | withdraw the over-claim; adopt the agreed posture | **FOLDED.** P1-D §3.1 `:376-393` prints the agreed posture verbatim, then `:386-388`: "withdraws this artefact's prior V2 over-claim that `ri_cycles` is a 'proven 4.27 GHz counter' superseding A/B/F — the GHz derivation was circular, so 'proven' was not earned." The unilateral "supersedes" sentence is struck. §3.2 cyc/B column relabelled "RAW, non-load-bearing." |
| **CH4-4** (P1-F §2.2.1 false "CPI < 1.0 physically impossible on M5") | strike "impossible"; state physical-but-non-disambiguable | **FOLDED + ATTRIBUTED.** P1-F §2.2.1 `:313-344` re-headed "sub-1.0 CPI is high IPC (PHYSICAL), not impossible"; `:323-325` "The V2 characterization of this as 'physically impossible' was WRONG PHYSICS and is withdrawn here (this row originated it; the correction is load-bearing)." The V3 fold log `:32-39` records the strike at the originating line `p1f:299`. |
| **CH4-4** (P1-A §2.1 / P1-B §2.1 inherited "falsified") | change "falsified" → "non-disambiguable, non-load-bearing"; align to agreed posture | **FOLDED.** P1-A `:15` COST-SURFACE POSTURE blockquote carries the agreed reading verbatim ("wrong physics (it confused CPI with IPC) and is retracted here … reported RAW and non-load-bearing"); §2.1 cyc/B column struck-through + labelled "RAW-non-load-bearing." P1-B `:40-55` c/B PROVENANCE note: "The V1/V2 characterization … was wrong physics and is withdrawn"; §3 String-tax conclusion re-grounded on instr/byte (4.36–7.06×, `:309-319`), not cyc/byte. |
| **CH4-5** (ACCEPT-advisory — P1-A authoritative Mbps from `css_cold_harness`, not canon) | reproduce §2.1 Mbps from the designated binary | **FOLDED.** P1-A `:10`/`:13`/§2.1 `:71-94` now source the authoritative §2.1 Mbps table from `css_canon_bench` N=200 (`css_canon_n200_v2.txt`); the legacy `css_cold_harness` runs are explicitly demoted to "cross-harness stability check, no longer authoritative per CH4-5." The V2 residual is closed. |

**ALL V2 dispositions FOLDED, zero orphan.** The single CH4-4 root is now resolved
identically across all six artefacts: each prints the same agreed posture (instr/byte
sole load-bearing; sub-1.0 CPI is PHYSICAL = IPC 3.5–6.4, not impossible; `ri_cycles`
non-disambiguable from rusage → cyc/byte RAW non-load-bearing; no conclusion rests on
it). The pass no longer contains the V2 direct contradiction (five sections "impossible"
vs P1-D "proven"). This lens re-adjudicated the physics against the host: an Apple M5
Max P-core is ~8-wide; IPC 3.5–6.4 is within issue width — the agreed posture is
factually correct on both the physics (not impossible) and the provenance (rusage
cannot disambiguate, so non-load-bearing). No residual COST defect.

---

## §2 — Per-artefact dispositions

### P1-A `p1a-samply-mode-1.md`

| § | Disposition | Basis |
|---|---|---|
| §0 canonical + cost-surface posture | ACCEPT | CANONICAL-HARNESS NOTE installs the ONE canon binary end-to-end (CH4-5 folded); COST-SURFACE POSTURE carries the agreed X1' reading verbatim. |
| §2.1 throughput (cyc/B struck) | ACCEPT | Mbps median/min/max/stddev now from `css_canon_bench` N=200; cyc/B column struck-through + "RAW-non-load-bearing." CH4-4 + CH4-5 discharged. |
| §2.1b instr/byte | ACCEPT | Reliable counter; fact/full 4.36–7.07×; IPC column substantiates the posture (3.51–6.23, all physical). Grounded. |
| §2.2 recognition hot leaves | ACCEPT | `find_component_delim` 58.41/65.05%, `consume_balanced_at` 10.79/0.15%; %self-time + file:line; grounds NEON. |
| §2.3 fact-stream hot leaves | ACCEPT | Alloc family attributed; `mach_absolute_time`←libmalloc caller-walk (25591/25640) sound; grounds tape append; `push_ascii_lower_hex` flagged FNV-diagnostic (no primitive). |
| §3 delta / §4 anomalies | ACCEPT | N-direct honest; NEON gated behind tape; no orphan kernel. |

### P1-B `p1b-samply-mode-2.md`

| § | Disposition | Basis |
|---|---|---|
| §intro c/B PROVENANCE note | ACCEPT | Agreed posture adopted; "wrong physics … withdrawn"; conclusions on instr/byte. |
| §1 method | ACCEPT | `--save-only` + atos disclosed; `[repro-as-cost]` satisfied. |
| §2.1 throughput + instr/B | ACCEPT | N=200 medians; instr/B reliable, cyc/B no longer printed as cost. |
| §2.2 fact-stream hot leaves | ACCEPT | kernel/malloc/`emit_fact_stream`/`push_ascii_lower_hex` each %self-time + file:line. |
| §2.3 recognition hot leaves | ACCEPT | `find_component_delim` 56.55%, `consume_balanced_at` 11.51%; grounds NEON; CH2 generality callout cites json/scan.rs:219. |
| §3 String tax (re-grounded) | ACCEPT | V1 "~3× cycles" inference re-grounded on instr/byte 4.36–7.06× (the CH4-1/CH4-4 remedy carried correctly). |
| §2.4 / §4 | ACCEPT | canon harness named; masking-shift sequencing sound; no orphan kernel. |

### P1-C `p1c-samply-mode-3.md`

| § | Disposition | Basis |
|---|---|---|
| §intro V3 c/B posture blockquote | **ACCEPT (CH4-4 cleared)** | `:37-57` carries the agreed verbatim posture; explicitly "supersedes the V2 'falsified / physically impossible' characterization this artefact carried"; `:47` "wrong physics: it confused CPI with IPC." The false impossibility claim is gone. |
| §2.3 hot-leaf line attribution | ACCEPT | `:26`/`:45` source-verified by this lens (FNV hash / `emit_declarations`); tagged "source-verified inclusive call sites." |
| §2.4 recognition hot leaves | ACCEPT | line cites within :288–:338 verified; `find_component_delim` 58.59%, `consume_balanced_at` 9.98% re-classed scan; collapse to ONE NEON target stated. |
| §2.5 PMU table | **ACCEPT (CH4-4 cleared)** | re-headed "sub-1.0 CPI is PHYSICAL"; cyc/B + CPI co-reported RAW non-load-bearing; i/B load-bearing. The false "counter falsified" header is gone. |
| §3 / §4 | ACCEPT | G plane reconciliation; A3 FNV diagnostic (vanishes with tape, no kernel); no orphan kernel. |

### P1-D `p1d-pmu-cycles.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | `css_canon_bench` PMU + xctrace commands verbatim; xctrace explicitly framed as wall cross-check, NOT a `ri_cycles` disambiguator. `[repro-as-cost]` satisfied. |
| §2.4 hot-leaf line split | ACCEPT — best-grounded | intra-leaf split (:298 27.88%, :295 17.24%, :307 2.99%) each verified against the :288–:311 body; the tightest cost decomposition in the pass. |
| §2.5 redundant re-scan | ACCEPT | triple-scan structurally true; REDRESS-51/53-vs-53 boundary cited (CH3 in-charter as a cost-target boundary). |
| §3.1 **PMU posture (the V2 load-bearing defect)** | **ACCEPT (CH4-4 cleared)** | §3.1 `:376-393` prints the agreed posture verbatim AND explicitly WITHDRAWS the V2 "proven 4.27 GHz / supersedes A/B/F" over-claim ("the GHz derivation was circular, so 'proven' was not earned"). It now corrects A/B/C/F's false "impossible" without over-correcting into a non-probative "proven." The exact V2 REVISE is resolved at root. |
| §3.2 PMU table | ACCEPT | i/B load-bearing; cyc/B column relabelled RAW-non-load-bearing; IPC internally consistent (instr/cyc = stated IPC every row). |
| §3.3 / §4 | ACCEPT | per-corpus lightningcss re-baseline; udot orphan named never-reached; zero-SIMD by symbol-table absence; no orphan kernel. |

### P1-E `p1e-hot-leaf-attribution.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | exact source-line discipline (`:146`/`:250`/`:138-142`), correcting the V1 paper-citation; caller-walk syslib attribution sound. |
| §2.1/§2.2 throughput + delta | ACCEPT | Mbps only; no cyc/B column → no CH4-4 exposure; ratios load-bearing. |
| §2.3/§2.5 hot-leaf table | ACCEPT — exemplary | `find_component_delim` 56.52% + `consume_balanced_at` 11.05% folded into ONE NEON target on the SOURCE-VERIFIED shared inner loop (:293-308 ≡ :322-338). Tightest primitive attribution in the pass: ~68% is ONE primitive, not two — S-P2 grounds one kernel. Every leaf: symbol + %self + file:line + class + candidate primitive. |
| §2.4 fact-stream attribution | ACCEPT | syslib-caller causal attribution (91.44% reached FROM `emit_fact_stream`) ties the alloc floor to the named leaf. |
| §3 c/B posture | ACCEPT | adopts the agreed X1' posture for pass consistency though it carries no cyc/B column. |
| §4.4 **orphan-block (C4b udot kernel)** | ACCEPT — exemplary (charter keystone) | orphan-blocks `digit_mac.rs:27` for ZERO CSS digit-parse self-time and names the re-admission condition (typed lazy-`ValueRef` re-profile after W1/W2). The precise "no speculative kernel without a profiled antecedent" discharge. |

### P1-F `p1f-bench-canonical.md`

| § | Disposition | Basis |
|---|---|---|
| §V3 fold log | ACCEPT | records the CH4-4 strike at the originating line `p1f:299` + the CH5-V2-R1 `:43`→`:103-105` line correction (this lens verified `:103 fn track1_full_parse`). |
| §1 method + §1.1.1 X2 | ACCEPT | canon harness named THE single canonical; comparability caveat load-bearing; `[repro-as-cost]` satisfied. |
| §2.1 throughput | ACCEPT | N=200 medians; within-harness ratio stability demonstrated across two runs (§2.1.1). |
| §2.2 instr/byte (primary) | ACCEPT | reliable, load-bearing; reproduced <0.5% across V1/V2 runs. |
| §2.2.1 **ri_cycles posture** | **ACCEPT (CH4-4 root cleared)** | the section that ORIGINATED the false "impossible" claim now strikes it ("WRONG PHYSICS and is withdrawn here; this row originated it"); states CPI 0.16–0.28 ⇒ IPC 3.5–6.4 physical; cyc/byte non-load-bearing because non-disambiguable. The root of the V2 contradiction is resolved at source. |
| §2.3 hot-leaf attribution (3 planes) | ACCEPT | recognition + fact-stream + the lightningcss-plane attribution (`cssparser::consume_name` 8.92%, `drop_in_place::<Token>` 3.95%, ~38% tokenizer + ~30% typed-node build/drop) — the only artefact profiling the comparator to PROVE full-CSSOM materialization; grounds fairness-as-cost. Row-2 line corrected to `:103-105`. |
| §3 / §4 | ACCEPT | W8R broadcast reproduced per-corpus; "~70/~14×" N-direct; eager-typed K (pre-blocked). |

---

## §3 — Cross-artefact COST findings

**CH4-4 (the V2 load-bearing defect) — RESOLVED, no longer open.** This lens
re-adjudicated the physics and the provenance against the host and confirms the agreed
posture is correct on both axes: (a) sub-1.0 CPI is PHYSICAL — an Apple M5 Max P-core
is ~8-wide, so IPC 3.5–6.4 (= CPI 0.16–0.28) is within issue width and is the normal
signature of a tight, branch-friendly scan loop; the V2 "physically impossible" claim
(originated at P1-F §2.2.1, propagated to A/B/C) was wrong physics and is struck
everywhere; (b) `proc_pid_rusage.ri_cycles` cannot be disambiguated as dynamic
core-cycles vs a wall-proportional scaled tick from the rusage interface alone (the
steady-GHz derivation is circular — `wall_s` is loop-derived — so it proves neither
model; `hw.tbfrequency` 24 MHz confirms a scaled reference clock exists), so cyc/byte
is RAW non-load-bearing; P1-D's V2 "proven 4.27 GHz / supersedes A/B/F" over-claim is
withdrawn. All six artefacts now carry the SAME verbatim posture and ground every cost
conclusion on instr/byte alone. The pass no longer contains two mutually exclusive
"the ONE c/B posture" declarations.

**CH4-5 (V2 advisory) — RESOLVED.** P1-A's authoritative §2.1 Mbps is now sourced
from `css_canon_bench` (the one designated binary), end-to-end with the §2.1b
instr/byte. The cross-harness residual is closed.

**[repro-as-cost] — clean.** Every §1 method block carries verbatim, sequential,
single-invocation commands (`css_canon_bench <N>` + `CSS_CANON_PMU=1` + the
`CSS_CANON_PROFILE` samply driver), the host triple (Apple M5 Max, aarch64-apple-darwin),
build flags (release + debug=true), tool versions (samply 0.13.1, rustc 1.96.0-nightly,
atos), and run-id'd profile artefact paths under `/tmp/skv17-p1*/`. The cost numbers
are reproducible by a third party.

**No REJECT.** No artefact proposes a speculative kernel without a profiled antecedent.
The one kernel lacking a CSS antecedent — the udot digit kernel (C4b, `digit_mac.rs:27`,
verified `unsafe fn parse_4_digits_dotprod`, never reached on the CSS path) — is
explicitly orphan-blocked by P1-E §4.4 with its re-admission condition named. The NEON
byte-class-scan primitive (`byte_class_index_64`/`to_bitmask64`, verified ABSENT as
extant symbols → correctly named to-build, routed through `select_classifier`
dispatch.rs:42) is grounded in the measured 56–68% `find_component_delim` +
`consume_balanced_at` leaf, which P1-C/P1-D/P1-E/P1-F correctly collapse to ONE
primitive on a SOURCE-VERIFIED identical inner loop (:293-308 ≡ :322-338), gated behind
tape activation. Correct profile-first discipline throughout.

---

## §4 — Counts + dispositions

Sections dispositioned: **42** (same section census as V2; the four V2 CH4-4 REVISE
rows convert to ACCEPT).

| Disposition | Count |
|---|---:|
| ACCEPT | 42 |
| REVISE | 0 |
| REJECT | 0 |

ACCEPT rate: 42/42 = **100%** (≥95% convergence gate MET for CH4).

REVISE list: **none.** All four V2 CH4-4 REVISEs and the V2 CH4-5 advisory are folded
with zero orphan (audit §1). No new COST defect surfaced this cycle.

V-trajectory for CH4: V1 87.8% (5 REVISE: CH4-1×3, CH4-2, CH4-3) → V2 90.5% (4 REVISE:
single-rooted CH4-4) → **V3 100% (0 REVISE)**. The progression is monotone and each
prior root was source-verified-cleared, not paper-folded: CH4-1 (cyc/B vs instr/byte)
→ instr/byte adopted pass-wide; CH4-2 (`:26`/`:45` sub-line) → source-verified; CH4-3
(5 harnesses) → ONE canonical designated; CH4-4 (impossible-vs-proven contradiction)
→ ONE agreed physical-but-non-disambiguable posture; CH4-5 (P1-A Mbps source) → canon
binary end-to-end.

Cost-grounding verdict: the benched-CSS hot leaves are fully grounded (measured
%self-time + named candidate primitive + source-verified antecedent); the
two-leaves-into-one NEON fold (P1-C/D/E/F) and the lightningcss-plane fairness
attribution (P1-F) are exemplary; the cyc/byte counter is now consistently
characterized as physical-but-non-load-bearing across all six; no speculative kernel
survives (C4b orphan-blocked with named re-admission condition). The CH4 COST charter
is fully discharged — every named hot leaf carries a measured % self-time and the
candidate primitive it grounds, and no kernel is proposed without a profiled antecedent.
