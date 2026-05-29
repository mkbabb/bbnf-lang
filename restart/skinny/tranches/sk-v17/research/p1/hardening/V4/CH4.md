# SK-V17 S-P1 CHALLENGE — CH4 COST (V4)

Lens: CH4 COST. Cycle: V4. Date: 2026-05-29.
Charter (dispatched): every named hot leaf carries a measured % self-time + the
candidate primitive it grounds; no speculative kernel without a profiled antecedent.
(This is the COST/hot-leaf-grounding charter the orchestrator dispatched; distinct from
PASS-1-PROFILE §3's CH4 "reproducibility" text — reproducibility is folded in as
`[repro-as-cost]` where it bears on whether a cost number is real.)
Subject: SK-V17 S-P1 PROFILE artefacts
`restart/skinny/tranches/sk-v17/research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md` (all at
Cycle V4 frontmatter; p1f is `p1f-bench-canonical.md`).
Baseline re-verified against benched source at master HEAD `6496fecae` (working tree).
Prior cycles: `V1/CH4.md` (87.8% ACCEPT; 5 REVISE — CH4-1×3, CH4-2, CH4-3);
`V2/CH4.md` (90.5% ACCEPT; 4 REVISE — single-rooted CH4-4); `V3/CH4.md` (100% ACCEPT;
0 REVISE).
Output: this file.

Disposition vocabulary: ACCEPT / REVISE / REJECT. One row per artefact §-section.

---

## §0 — Source verification re-run this cycle (the cost-grounding floor)

CH4's charter is substantive, not procedural: a hot leaf is grounded only when its
%self-time number is real AND it resolves to a source symbol AND that symbol grounds a
named candidate primitive. V4 re-emits V3-accepted content; this lens does not trust
"unchanged" — it re-resolved every load-bearing symbol + line against the benched tree
this cycle. All resolve exactly:

| Cited symbol / claim | Cited line | Verified against source | Status |
|---|---|---|---|
| `emit_fact_stream` header | generated.rs:5 | `pub fn emit_fact_stream(input: &str) -> Result<String, CssFactError>` :5 | OK |
| `emit_full_parse` header | generated.rs:61 | `pub fn emit_full_parse(input: &str) -> Result<String, CssFactError>` :61 | OK |
| `find_component_delim` body | generated.rs:288 | fn :288; `while pos < self.bytes.len()` :293; `let byte = self.bytes[pos]` :294; `if delimiters.contains(&byte)` :295; `return Ok(Some((byte, pos)))` :296; `pos = match byte` :298; `_ => pos + 1` :307; `Ok(None)` :311 | OK (every per-line cite across A/C/D/F exact) |
| `consume_balanced_at` body | generated.rs:320 | fn :320; `let mut pos = start + 1` :321; `let byte = self.bytes[pos]` :323; `if byte == close` :324; `pos = match byte` :327; `_ => pos + 1` :336 | OK |
| **shared-inner-loop collapse claim** | :293-308 ≡ :322-338 | the `pos = match byte` arms are BYTE-IDENTICAL across both fns (`'\''\|'"'`→string, `/`*→comment, `(`/`[`/`{`→`consume_balanced_at`, closing→err, `_`→`pos+1`); the ONLY difference is the membership test (`delimiters.contains(&byte)` :295 vs `byte == close` :324) | **OK — collapse-to-ONE-NEON-target is SOURCE-TRUE** |
| `push_ascii_lower_hex` + per-token alloc | generated.rs:628 | fn :628; `let mut buf = Vec::with_capacity(text.len())` :629; lowercase loop :630-632; `push_hex(out, &buf)` :633 | OK |
| `select_classifier` (NEON dispatch vehicle) | dispatch.rs:42 | `pub fn select_classifier(alphabet: &'static [u8; 64]) -> SelectedClassifier` :42 | OK |
| `push_plain_offset` (tape append, lever-2 target) | assembler.rs:71 | `pub fn push_plain_offset(&mut self, offset: usize) -> u32` :71 | OK |
| **`byte_class_index_64` / `to_bitmask64`** (the to-build NEON kernel) | named "to-build" | `grep -rn` in `bbnf-simd/src/` returns ZERO defs | **OK — correctly named to-build everywhere; NOT claimed extant** |
| `parse_4_digits_dotprod` (udot orphan, C4b) | digit_mac.rs:27 | `pub unsafe fn parse_4_digits_dotprod(bytes: [u8; 4]) -> u32` :27 | OK |
| canon harness `assert!(n >= 50)` | css_canon_bench.rs:250 | `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` :250 | OK |
| canon harness `fn sample` | css_canon_bench.rs:146 | `fn sample(parse: ParseFn, input: &str, n: usize) -> Stats` :146 | OK |
| canon harness `fn mbps` | css_canon_bench.rs:138-142 | `(bytes as f64 * 8.0) / (secs * 1_000_000.0)` | OK |
| `track1_full_parse` / `track1_fact_stream` workload wrappers | css_canon_bench.rs:103 / :108 | `fn track1_full_parse(input: &str) -> u64` :103; `fn track1_fact_stream(input: &str) -> u64` :108 | OK |

**Cost-grounding verdict on the named leaves (charter substance — MET).** Each
benched-CSS hot leaf carries a measured %self-time on ≥3 independent profiles AND
grounds a named candidate primitive whose antecedent is the measured leaf:

- **Recognition scan:** `find_component_delim` 56.52% (p1e) / 58.41–65.05% (p1a) /
  56.55% (p1b) / 58.59% (p1c) / 57.40–79.64% (p1d) / 59.24% (p1f) + `consume_balanced_at`
  9.98–11.51% — SOURCE-VERIFIED to share ONE byte-class-membership inner loop, collapsed
  to ONE NEON `byte_class_index_64`/`to_bitmask64` candidate routed through
  `select_classifier` (dispatch.rs:42), gated behind tape activation.
- **Fact-stream allocation floor:** ~58–80% syscall+heap (p1e 57.63%, p1a ~75–80%,
  p1c 64.45%) + `emit_fact_stream` 19.59–25.13% + `push_ascii_lower_hex` 7.13–9.11% →
  grounds the lever-1 (kill String) / lever-2 (`push_plain_offset` tape append,
  assembler.rs:71). Reliable magnitude restated on instr/byte (the load-bearing
  counter): fact_stream 4.36–7.1× full_parse i/B (the String tax).
- **lightningcss comparator plane** (p1f §2.3: `cssparser::consume_name` 8.92%,
  `drop_in_place::<Token>` 3.95%, ~38% tokenizer + ~30% typed-node build/drop) →
  grounds the FAIRNESS of the >SOTA bar as a measured cost claim (the comparator
  demonstrably materializes the CSSOM; not a token-scan strawman).

No leaf is named without a number; no proposed primitive lacks a profiled antecedent;
the one kernel without a CSS antecedent (the udot/i8mm digit kernel
`parse_4_digits_dotprod`, digit_mac.rs:27, C4b) is explicitly orphan-blocked by P1-E §4.4
with a named re-admission condition (re-profile the typed lazy-`ValueRef` path after
W1/W2), and confirmed never-reached empirically by P1-D §4 (no `bbnf_simd` frame in
20,377 samples).

---

## §1 — V3 disposition fold audit (the convergence test)

V3 CH4 returned **42/42 = 100% ACCEPT, 0 REVISE, 0 REJECT**. With V2 (90.5%) below the
gate but V3 at 100%, CH4's two-consecutive-≥95% gate is NOT yet met on this lens alone
(V2 < 95%); V4 is the second qualifying cycle. There are therefore **no orphan REVISE
dispositions to fold** — the convergence test reduces to: does V4 preserve every V3
ACCEPT basis under fresh source verification, with no regression?

**Answer: yes, with one minor non-blocking consistency observation (CH4-6, advisory).**

| V3 root (already cleared) | V4 status |
|---|---|
| CH4-1 (cyc/B vs instr/byte as cost) | **HELD.** All six artefacts ground cost on instr/byte; cyc/B co-reported RAW with IPC made explicit. p1a §2.1b, p1b §2.1, p1c §2.5, p1d §3.2, p1f §2.2 all consistent. |
| CH4-2 (`:26`/`:45` sub-line cites) | **HELD.** p1c §2.3 / p1f line cites re-verified against source this cycle. |
| CH4-3 (5 harnesses → ONE canonical) | **HELD.** `css_canon_bench` is the sole designated harness across all six; the comparability caveat (p1e §2.1, p1c §2.2) explicitly retires the other bins' numbers. |
| CH4-4 (impossible-vs-proven c/B contradiction) | **HELD.** The ONE agreed posture (sub-1.0 CPI = high IPC, PHYSICAL on the ~8-wide M5 Max P-core; `ri_cycles` non-disambiguable from rusage → cyc/B non-load-bearing) is carried verbatim in p1a:15, p1c §intro/§2.5, p1d §3.1, p1e §3, p1f §2.2.1. No "impossible", no "proven 4.27 GHz supersedes A/B/F" survives anywhere. Re-verified physics: IPC 3.5–6.4 (CPI 0.16–0.28) is within issue width — correct. |
| CH4-5 (P1-A Mbps from canon, not `css_cold_harness`) | **HELD.** p1a:10/§2.1 source the authoritative table from `css_canon_bench` N=200; legacy bins demoted. |

**The V4 re-emission discipline is sound.** p1a carries an explicit `V4-FOLD NOTE`
(p1a:15) recording the V3 100%-ACCEPT carry and re-verifying every load-bearing citation
fresh against source on HEAD `6496fecae`. p1f carries an equivalent V4 fold log (one
source-line refresh at `push_ascii_lower_hex` :628, no claim change; plus the V3-correct
`find_component_delim` 59% attribution re-verified). This lens independently re-resolved
the same symbols (§0) and confirms them exact — the re-emission is source-true, not a
paper-carry.

---

## §2 — Per-artefact dispositions

### P1-A `p1a-samply-mode-1.md`

| § | Disposition | Basis |
|---|---|---|
| frontmatter + V4-FOLD NOTE (:15) | ACCEPT | Records V3 100% carry; re-verifies `generated.rs:{61,103,118,189,242,288,295,320,628}`, `css_canon_n200_v2.txt`/`css_canon_pmu_v2.txt` byte-identity, IPC 3.51–6.23, `emit_fact_stream` signature — all re-confirmed by this lens. Zero orphan REVISE. |
| §2.1 throughput (cyc/B struck) | ACCEPT | Mbps median/min/max/stddev from `css_canon_bench` N=200; cyc/B struck-through RAW-non-load-bearing. CH4-4 + CH4-5 held. |
| §2.1b instr/byte | ACCEPT | Reliable counter; fact/full 4.4–7.1×; IPC substantiates the posture; grounded. |
| §2.2 recognition hot leaves | ACCEPT | `find_component_delim` 58.41/65.05% + `consume_balanced_at` 10.79/0.15%; %self + file:line; grounds NEON. The corpus-dependent `consume_balanced_at` caveat (:177) is an honest cost observation. |
| §2.3 fact-stream hot leaves | ACCEPT | Alloc family attributed; `mach_absolute_time`←libmalloc caller-walk (25591/25640) sound; grounds tape append; `push_ascii_lower_hex` flagged FNV-diagnostic (no primitive). |
| §3 delta / §4 anomalies | ACCEPT | N-direct honest; NEON gated behind tape; fact-stream-as-admission pre-block cited not re-opened; no orphan kernel. |

### P1-B `p1b-samply-mode-2.md`

| § | Disposition | Basis |
|---|---|---|
| §intro c/B PROVENANCE note | ACCEPT | Agreed posture adopted; conclusions on instr/byte. |
| §1 method | ACCEPT | `--save-only` + atos disclosed; `[repro-as-cost]` satisfied. |
| §2.1 throughput + instr/B | ACCEPT | N=200 medians; instr/B reliable; cyc/B not printed as cost. |
| §2.2 fact-stream hot leaves | ACCEPT | `emit_fact_stream` 23.80/19.59% + `push_ascii_lower_hex` 8.98/7.53% each %self + file:line; FNV-diagnostic annotation consistent with A/C. |
| §2.3 recognition hot leaves | ACCEPT | `find_component_delim` 56.55% + `consume_balanced_at` 11.51% = 68.1% in-binary; grounds NEON `byte_class_index_64`/`to_bitmask64` (named to-build, :241); CH2 generality callout cites the JSON byte-class scan (:426). |
| §3 String tax (re-grounded) | ACCEPT | re-grounded on instr/byte 4.36–7.06×; ~70-Mbps prior corrected as W8-broadcast-diluted (:297). |
| §2.4 / §4 | ACCEPT | canon harness named; masking-shift sequencing sound; no orphan kernel. |

### P1-C `p1c-samply-mode-3.md`

| § | Disposition | Basis |
|---|---|---|
| §intro V3 c/B posture | ACCEPT | Agreed posture carried; the false "falsified/impossible" characterization gone. |
| §2.3 hot-leaf line attribution | ACCEPT | `emit_fact_stream` 23.89% (:5) + `push_ascii_lower_hex` 8.35% (:628); `:26`/`:45` source-verified by this lens (FNV hash / `emit_declarations`); inclusive call sites disclosed. |
| §2.4 recognition hot leaves | ACCEPT | `find_component_delim` 58.59% + `consume_balanced_at` 9.98% within :288–:340 verified; re-classed scan/structural-over-scan; collapse to ONE NEON target stated; G-plane reconciliation §3 (+2.6pp / −0.0pp / −4.2pp re-confirm). |
| §2.5 PMU table | ACCEPT | re-headed "sub-1.0 CPI is PHYSICAL"; cyc/B + CPI co-reported RAW; i/B load-bearing. |
| §3 / §4 (A3 FNV diagnostic) | ACCEPT | A3 FNV/hex diagnostic (vanishes with tape, no kernel); the `find_component_delim` 58.59% scan leaf named the profile-first antecedent (:395); no orphan kernel. |

### P1-D `p1d-pmu-cycles.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | `css_canon_bench` PMU + atos line-level symbolication; `proc_pid_rusage` V5 re-verified on disk (:345); `[repro-as-cost]` satisfied. |
| §2.4 hot-leaf line split | ACCEPT — best-grounded | `find_component_delim` 79.64% (parse-self) + `consume_balanced_at` 15.72% = 95.36%; intra-leaf per-line split (:298, :295, :294, :307) verified against the :288–:311 body; tightest cost decomposition in the pass. |
| §2.5 redundant re-scan | ACCEPT | triple-scan per declaration body structurally true (`:211`/`:314`/`:247`); a cost-target boundary, not a re-proposal (CH3-adjacent, in COST charter as the cost antecedent). |
| §3.1 PMU posture | ACCEPT | the V2 over-claim ("proven 4.27 GHz / supersedes A/B/F") remains withdrawn; agreed posture carried; A/B/C/F's "impossible" corrected without over-correcting into "proven." |
| §3.2 PMU table | ACCEPT | i/B load-bearing; cyc/B relabelled RAW-non-load-bearing; IPC internally consistent. |
| §3.3 / §4 | ACCEPT | per-corpus lightningcss re-baseline; zero-SIMD by symbol-table absence (no `bbnf_simd` frame); udot orphan `digit_mac.rs:27` named never-reached (:547-548, :600); no orphan kernel. |

### P1-E `p1e-hot-leaf-attribution.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | exact source-line discipline (`:146`/`:250`/`:138-142` re-verified by this lens); caller-walk syslib attribution sound; cold per-parse + pre-touch-outside-window honest (`no-warm-benches`). |
| §2.1 N=100 canon table + comparability caveat | ACCEPT | median/min/max/stddev per corpus; the material `min=121.52` first-window outlier flagged (:139) → justifies the median statistic; within-harness ratio declared the load-bearing figure (CH4-3 fold). |
| §2.2 delta vs lightningcss | ACCEPT | full_parse 2.0–3.6× (recognition, masking probe, NOT admission) / fact_stream 0.62–0.79× (the real starting line); SK-V16 ~14× corrected as wrong-plane single-sample. |
| §2.3 recognition hot-leaf table | ACCEPT — exemplary | `find_component_delim` 56.52% + `consume_balanced_at` 11.05% folded into ONE NEON target on the SOURCE-VERIFIED shared inner loop (:293-308 ≡ :322-338); every leaf carries symbol + %self + file:line + class + candidate primitive. ~68% is ONE primitive, not two. |
| §2.4 fact-stream attribution | ACCEPT | 57.63% syscall+heap floor, 91.44% reached FROM `emit_fact_stream` (caller-walk); `emit_fact_stream` 24.59% + `push_ascii_lower_hex` 9.11% each %self + file:line; ties the floor to the named leaf. |
| §2.5 classification roll-up | ACCEPT | the §2 P1-E synthesis deliverable; no number/unicode/dispatch/tape leaf hot on either plane — provably (zero `Tape`/`ValueRef`/`select_classifier` samples), confirming the substrate unwired-for-CSS finding empirically. |
| §3 c/B posture | ACCEPT | adopts the agreed X1' posture for pass consistency (no cyc/B column → no CH4-4 exposure). |
| §4.4 **orphan-block (C4b udot kernel)** | ACCEPT — charter keystone | orphan-blocks `digit_mac.rs:27` for ZERO CSS digit-parse self-time and names the re-admission condition (re-profile the typed lazy-`ValueRef` path after W1/W2; do NOT inherit a CSS digit-kernel hypothesis from here). The precise "no speculative kernel without a profiled antecedent" discharge. |

### P1-F `p1f-bench-canonical.md`

| § | Disposition | Basis |
|---|---|---|
| §V4 fold log | ACCEPT | records the `push_ascii_lower_hex` :628 source-line refresh (no claim change) + re-verification of the V3-correct `find_component_delim` 59% / `:295` hot-line; `emit_fact_stream` signature re-verified (:51, :231). |
| §1 method + X2 | ACCEPT | canon harness named THE single canonical; `assert!(n >= 50)` grep-re-verified (:125); comparability caveat load-bearing; `[repro-as-cost]` satisfied. |
| §2.1 throughput | ACCEPT | N=200 medians; within-harness ratio stability. |
| §2.2 instr/byte (primary) | ACCEPT | reliable, load-bearing; the cost-density figure foregrounded over wall-derived %self (:449). |
| §2.2.1 ri_cycles posture | ACCEPT | the section that ORIGINATED the V2 false "impossible" claim still strikes it; CPI 0.16–0.28 ⇒ IPC 3.5–6.4 physical; cyc/byte non-load-bearing. |
| §2.3 hot-leaf attribution (3 planes) | ACCEPT | recognition (`find_component_delim` 59.24% + `consume_balanced_at` 10.31% = ~69%, per-line cites :295/:293/:294/:298/:307 verified) + fact-stream (`emit_fact_stream` 25.01% + `push_ascii_lower_hex` 8.98%) + the lightningcss plane (the only artefact profiling the comparator to PROVE full-CSSOM materialization); grounds fairness-as-cost. |
| §3 / §4 | ACCEPT | W8R broadcast reproduced per-corpus; "~70/~14×" N-direct corrected; eager-typed K (pre-blocked, AZ-IV / SYNTHESIS §0.4 cited, :537); fact-stream-as-admission pre-block cited not re-opened (:454); no orphan kernel. |

---

## §3 — Cross-artefact COST findings

**CH4-4 (the V2 load-bearing defect) — HELD RESOLVED.** All six artefacts carry the
SAME verbatim posture (sub-1.0 CPI is PHYSICAL = IPC 3.5–6.4 on the ~8-wide M5 Max
P-core; `ri_cycles` non-disambiguable from rusage → cyc/byte RAW non-load-bearing; every
cost conclusion grounded on instr/byte). This lens re-adjudicated the physics: IPC
3.5–6.4 is within issue width — the posture is correct on both physics and provenance.
No regression.

**[repro-as-cost] — clean.** Every §1 method block carries verbatim, sequential,
single-invocation commands (`css_canon_bench <N>` + `CSS_CANON_PMU=1` + the
`CSS_CANON_PROFILE` samply driver), the host triple (Apple M5 Max, aarch64-apple-darwin),
build flags (release + debug=true + packed split-debuginfo), tool versions (samply 0.13.1,
rustc 1.96.0-nightly, atos), and run-id'd profile artefact paths under `/tmp/skv17-p1*/`.
The cost numbers are reproducible by a third party.

**No speculative kernel survives (charter core — MET).** Every proposed primitive has a
profiled antecedent:
- NEON `byte_class_index_64`/`to_bitmask64` (verified ABSENT as extant symbols →
  correctly named to-build, routed through `select_classifier` dispatch.rs:42) is
  grounded in the measured 56–80% `find_component_delim` + `consume_balanced_at` leaf,
  collapsed to ONE primitive on a SOURCE-VERIFIED identical inner loop (:293-308 ≡
  :322-338), gated behind tape activation.
- `TapeBuilder::push_plain_offset` (assembler.rs:71) lever-1/2 is grounded in the
  measured ~58–80% fact-stream allocation floor (91.44% from `emit_fact_stream`).
- The ONE kernel lacking a CSS antecedent — the udot digit kernel (`parse_4_digits_dotprod`,
  digit_mac.rs:27, C4b) — is orphan-blocked by P1-E §4.4 with a named re-admission
  condition and confirmed never-reached by P1-D §4. Correct profile-first discipline.

**CH4-6 (NEW — advisory, non-blocking).** p1e and p1d do NOT carry an explicit
`V4-FOLD NOTE` blockquote analogous to p1a:15 / p1f's V4 fold log recording the V3
100%-ACCEPT carry and the fresh-re-verify discipline; p1b/p1c likewise re-emit V3 content
under a V4 frontmatter without an explicit per-artefact fold-carry note. This is a
**consistency advisory, not a COST defect**: V3 returned zero REVISE, so there is nothing
to fold and no orphan; every cost claim in these artefacts is independently source-verified
by this lens (§0). The advisory is for pass-cohesion uniformity only (so a reader can see
at each artefact head that V4 is a verified re-emission of a 100%-ACCEPT V3). It does NOT
gate convergence and carries **no REVISE** — recorded for the consolidator's optional
pass-cohesion fold, dispositioned ACCEPT-advisory.

**No REJECT.** No artefact proposes a speculative kernel; every named hot leaf carries a
measured %self-time and the candidate primitive it grounds.

---

## §4 — Counts + dispositions

Sections dispositioned: **42** (same section census as V3).

| Disposition | Count |
|---|---:|
| ACCEPT | 42 |
| REVISE | 0 |
| REJECT | 0 |

ACCEPT rate: 42/42 = **100%** (≥95% convergence gate MET for CH4).

REVISE list: **none.** One ACCEPT-advisory (CH4-6, pass-cohesion uniformity — explicit
per-artefact V4 fold-carry note on p1b/p1c/p1d/p1e), which gates nothing and carries no
fold obligation (V3 had zero REVISE → nothing to orphan).

V-trajectory for CH4: V1 87.8% (5 REVISE) → V2 90.5% (4 REVISE: single-rooted CH4-4) →
V3 100% (0 REVISE) → **V4 100% (0 REVISE)**. Two consecutive cycles ≥95% with zero
orphan REVISE — the per-lens convergence condition (`ORCHESTRATOR.md` §3Z) is met for
CH4.

Cost-grounding verdict: the benched-CSS hot leaves are fully grounded (measured
%self-time + named candidate primitive + source-verified antecedent, re-resolved fresh
this cycle against HEAD `6496fecae`); the two-leaves-into-one NEON fold (P1-C/D/E/F) on a
SOURCE-VERIFIED identical inner loop and the lightningcss-plane fairness attribution
(P1-F) are exemplary; the cyc/byte counter is consistently physical-but-non-load-bearing
across all six; no speculative kernel survives (C4b orphan-blocked with named re-admission
condition, confirmed never-reached). The CH4 COST charter is fully discharged — every
named hot leaf carries a measured % self-time and the candidate primitive it grounds, and
no kernel is proposed without a profiled antecedent.
