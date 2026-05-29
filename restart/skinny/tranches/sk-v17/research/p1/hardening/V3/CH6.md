# CH6 ANTI-PAPER-CLOSE — S-P1 V3 review

Lens: CH6 ANTI-PAPER-CLOSE (V3). Pass: S-P1 Profile, cycle V3.
Contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH6 + §8.1/§8.3;
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.
Date: 2026-05-29. Reviewer scope: every "profiled" claim must carry the
orchestrator-citable artefact (flame file on disk, resolvable symbol, measured
number); the N>=50 harness must be DEFINED (compilable, code-asserting), not promised;
no §2 cell reading "unprofiled"/"n/a" without a stated cause.
Artefacts reviewed: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
p1f-bench-canonical}.md`.

## §0 — Method (verbatim CH6 evidence checks run)

CH6 verifies existence, not plausibility. Every cited artefact checked against disk,
every cited source line grep/sed-verified against the benched skinny tree:

```
ls -la /tmp/skv17-p1/ /tmp/skv17-p1c-v2/ /tmp/skv17-p1d/ /tmp/skv17-p1e/   # flame + logs present?
wc -l /tmp/skv17-p1/css_canon_pmu_v2.txt /tmp/skv17-p1/css_canon_n200_v2.txt # V3 authoritative tables backed?
cat /tmp/skv17-p1/css_canon_pmu_v2.txt                                      # real ri_cycles/ri_instructions?
wc -l /tmp/skv17-p1d/atos_v2.txt ; wc -l /tmp/skv17-p1d-v2-pmu.txt          # V1 paper-closes still closed?
grep -n "assert!(n >= 50" .../bin/css_canon_bench.rs                        # N>=50 DEFINED (runtime assert)?
grep -n "fn track1_full_parse|fn sample|CSS_CANON_PMU|read_rusage_v5" .../css_canon_bench.rs
sed -n '43p;103,105p;146,169p' .../css_canon_bench.rs                       # V2 wrapper-line REVISEs (:43/:45 → :103-105)?
grep -n "fn emit_fact_stream|fn emit_full_parse|find_component_delim|push_ascii_lower_hex" .../generated.rs
grep -ni "falsif" p1{a,b,c}.md | grep -i "P1-D|P1-F|corrobor|concur"        # V2 ROOT REJECT: live broken cross-cites?
grep -ni "unprofiled|n/a" p1*.md                                           # bare paper-close cells?
```

**Disk verdict (the load-bearing CH6 fact): the V3 profiles are REAL, the two V1 gross
paper-closes remain CLOSED, and the V2 ROOT REJECT (the c/B interpretation split) is
RESOLVED.** Concretely:

- **N>=50 harness DEFINED, not promised.** `css_canon_bench.rs:250` carries
  `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` — a RUNTIME
  assert, not a comment (verified). 403 lines (verified `wc -l`). The cold-per-parse
  discipline is code-enforced: `fn sample` `:146` times exactly one `parse(black_box(input))`
  between `Instant::now()` `:154` and `.elapsed()` `:156`, result `black_box`-dropped
  `:157`, ONE pre-touch parse OUTSIDE the timed window only to fault in source pages
  `:152` (verified `sed -n '146,169p'`). This is a real cold harness, not a warm one
  (CH6 §8.1 honoured). `CSS_CANON_PMU` mode `:211`, `read_rusage_v5` `:86`, samply
  driver `:183`, `WORKLOADS` `:123` all present.
- **The V3 authoritative tables are byte-faithful to disk.** `/tmp/skv17-p1/css_canon_pmu_v2.txt`
  (18 lines) carries REAL `ri_cycles`+`ri_instructions` deltas per corpus×workload
  (e.g. bootstrap track1_full cycles=7064206954 instructions=25012559767
  cycles_per_byte=15.1721 cpi=0.2824 mbps=2316.360); P1-A §2.1b, P1-D §3.2, P1-F §2.2
  trace to these exact numbers. `css_canon_n200_v2.txt` backs the N=200 dispersion.
  No nominal-clock substitute anywhere.
- **The two V1 gross paper-closes stay closed.** `/tmp/skv17-p1d/atos_v2.txt` is **199
  lines** of resolved `<symbol> (file:line)`; `/tmp/skv17-p1d-v2-pmu.txt` is real PMU
  counters. Both on disk, both carried forward into V3 §2.4/§3.2.
- **Every cited flame file exists.** `/tmp/skv17-p1/{fact,full}-{bootstrap,tailwind}.json.gz`
  + `.syms.json` sidecars (92057/27180/93534/18955 samples), `/tmp/skv17-p1/{fact_stream,
  full_parse,lightningcss}.json.gz` (13583 lightningcss samples), `/tmp/skv17-p1c-v2/{fact,
  full}.json.gz` + `canon.txt` (7616 B), `/tmp/skv17-p1e/{full_parse,fact_stream}.json.gz`.
  All verified `ls`.

**This is NOT a paper-close pass.** The V1 REJECTs folded in V2 and remain folded; the
V2 ROOT REJECT + its four propagation sites + the two `:43`/`:45` wrapper-line REVISEs
all folded into V3 with on-disk + grep evidence. CH6 finds **zero open REJECT** and
**zero open REVISE** for V3. Itemized dispositions follow.

## §1 — The V2 ROOT REJECT is RESOLVED (the load-bearing V3 finding)

V2's single root REJECT (with four propagation sites) was: the pass advertised "ONE c/B
posture" while shipping TWO mutually exclusive interpretations of the same `ri_cycles`
number — a FALSIFIED camp (P1-A/B/C/F: "sub-1.0 CPI physically impossible / reference-clock
tick") and a VALID-HIGH-IPC camp (P1-D: "real core counter, IPC 3.5-6.4, supersedes A/B/F")
— with four artefacts citing P1-D §3 as the falsifying authority that P1-D §3 itself
disavowed. CH6 V2 demanded the orchestrator resolve to ONE reading and propagate it.

**V3 resolves it, and resolves it correctly — to a THIRD, more disciplined reading than
either V2 camp.** All six artefacts now carry one verbatim posture (verified by grep):

- **P1-A** §"COST-SURFACE POSTURE" (`p1a:15`): "instr/byte … is the sole load-bearing
  cost density … The sub-1.0 CPI … is PHYSICAL, NOT impossible … IPC 3.5-6.2 … The earlier
  V1/V2 characterization … as 'falsified / physically impossible' was **wrong physics** …
  and is retracted here. However, `proc_pid_rusage.ri_cycles` cannot be disambiguated …
  Cyc/byte is therefore reported RAW and non-load-bearing … This … replaces P1-D §3.1's
  earlier 'proven 4.27 GHz / supersedes A/B/F' over-claim and A/B/C/F's 'falsified/impossible'
  under-claim with the one agreed reading."
- **P1-D** §3.1 (`p1d:342-395`): the over-claim is WITHDRAWN — "The V2 'proven 4.27 GHz
  counter / supersedes A/B/F' over-claim is withdrawn (the GHz derivation was circular)"
  (`p1d:526-527`); the under-claim is corrected — sub-1.0 CPI "is itself **wrong physics**"
  (`p1d:356-357`). P1-D no longer unilaterally supersedes its siblings; the posture is
  adopted pass-wide.
- **P1-B** §"c/B PROVENANCE" (`p1b:40-52`), **P1-C** §"V3 cycles-per-byte posture"
  (`p1c:37-52`), **P1-E** §"c/B posture" (`p1e:288-302`), **P1-F** §2.2/§2.2.1
  (`p1f:274-344`): all adopt the identical reading — instr/byte primary + load-bearing;
  cyc/byte co-reported RAW + non-load-bearing; "falsified/impossible" WITHDRAWN as wrong
  physics; the reason it is set aside is **non-disambiguability** from the rusage interface
  (`hw.tbfrequency` 24 MHz scaled tick exists), NOT impossibility.

**The broken cross-cite is gone.** A grep for any live "P1-D §3 / P1-F §2.2 falsifies it"
cross-cite returns ZERO hits (`grep -ni falsif p1{a,b,c}.md | grep "P1-D|P1-F|corrobor|concur"`
yields only `p1a:108`, which uses "corroborating" for the instr/byte→Mbps relation and
states "not a falsified counter" — i.e. the WITHDRAWN framing, not a live broken cite). The
"ONE posture" sentence is now TRUE, not the paper CH6 V2 named.

This is the strongest possible resolution: the V3 reading is more honest than EITHER V2
camp. The FALSIFIED camp was wrong physics (CPI/IPC confusion); the VALID-HIGH-IPC camp
over-claimed a "proven 4.27 GHz core counter" from a circular wall-derived ratio. V3
discards BOTH over-statements and lands on the defensible floor: the cyc/byte counter is
non-disambiguable, so it is reported but carries no conclusion, and the entire S-P2 cost
input rests on the reliable instr/byte counter (stable to <0.5%, identical across all six).
ACCEPT.

## §2 — Dispositions (path:line + verification)

### p1a-samply-mode-1.md

- **§"COST-SURFACE POSTURE" `:15` — ACCEPT (V2 §1/§2.1 REJECT CLOSED).** Carries the one
  agreed reading verbatim; explicitly retracts the V1/V2 "falsified/impossible" framing and
  P1-D's over-claim; struck-through cyc/B column labelled RAW non-load-bearing.
- **§2.1b instr/byte authoritative cost surface — ACCEPT.** Byte-faithful to
  `css_canon_pmu_v2.txt` (verified: animate fact 279.83 i/B, bootstrap full 53.72 i/B,
  IPC band 3.51-6.23). The cited PMU file exists and matches.
- **§2.2 hot-leaf table (`find_component_delim` 58.41%/65.05%, `generated.rs:288`) — ACCEPT.**
  Sidecar-symbolicated (`.syms.json` on disk, 27180/18955 samples); `find_component_delim`
  verified at `generated.rs:288`, hot body `delimiters.contains(&byte)` at `:295`.
- **§5 sidecar resolution discipline — ACCEPT.** `--unstable-presymbolicate` `.syms.json`
  sidecars verified on disk for all four mode-I profiles; strongest resolution path of the six.

### p1b-samply-mode-2.md

- **§"c/B PROVENANCE" `:40-52` — ACCEPT (V2 REJECT CLOSED).** Adopts P1-D §3.1's resolved
  posture verbatim; the broken "falsified by P1-F §2.2 / P1-D §3" cross-cite is gone.
- **§2.4 N>=50 harness (assert `:250`) — ACCEPT.** Line cite verified; harness asserts,
  builds, carries PMU + profile modes.
- **§2.3 row `:222` wrapper cited `css_canon_bench.rs:103-105` — ACCEPT.** Verified: P1-B
  was the only V2 artefact citing the wrapper correctly and continues to (`fn track1_full_parse`
  at `:103`). atos-resolved leaves with sample counts.
- **§2.1 instr/byte column — ACCEPT.** Sourced from `css_canon_pmu.txt`; cyc/byte
  co-reported non-load-bearing per the resolved posture.

### p1c-samply-mode-3.md

- **§"V3 cycles-per-byte posture" `:37-52` — ACCEPT (V2 REJECT CLOSED).** Explicitly states
  it "supersedes the V2 'falsified / physically impossible' characterization this artefact
  carried" and adopts the pass-wide reading; the "P1-D/P1-F concur" broken cite is gone.
- **§1.2 N>=50 gate (`css_canon_bench.rs:250`) — ACCEPT.** Cites the asserting harness.
- **§2.3/§2.4 hot leaves + on-disk flame — ACCEPT.** `/tmp/skv17-p1c-v2/{fact,full}.json.gz`
  (525381/603065 B) + `canon.txt` (7616 B) on disk; syslib resource-bucketing honesty
  caveat retained (`p1c` per-symbol atos on syslib "is NOT claimed; the resource bucketing
  IS reliable") — exemplary CH6 honesty.
- **§2.5 c/B table — ACCEPT.** instr/byte marked "(reliable)"; cyc/byte + CPI marked
  "(RAW, non-load-bearing)" with the IPC explanation inline (`p1c:295`).

### p1d-pmu-cycles.md

- **§3.1 PMU posture `:342-395` — ACCEPT (V2 REVISE CLOSED).** The V2 REVISE was: P1-D held
  the correct reading but unilaterally self-declared it as superseding siblings that still
  contradicted it (orphan). V3 closes both ends: the over-claim ("proven 4.27 GHz /
  supersedes A/B/F") is WITHDRAWN as circular (`p1d:526-527`), AND the four siblings now
  carry the matching posture (verified §1). No longer an orphan supersede; it is the pass
  posture all six adopt.
- **§3.2 PMU table — ACCEPT (V1 REJECT stays closed).** Byte-faithful to
  `/tmp/skv17-p1d-v2-pmu.txt` (real `ri_cycles`+`ri_instructions`); instr/byte load-bearing,
  cyc/byte RAW non-load-bearing per §3.1.
- **§2.4 per-line self-time table — ACCEPT (V1 REJECT stays closed).** `/tmp/skv17-p1d/atos_v2.txt`
  is 199 resolved `<symbol> (file:line)` lines on disk; per-line %s trace to resolved
  addresses.
- **§2.1/§2.2 cold tables — ACCEPT.** Byte-faithful to `/tmp/skv17-p1d-v2-cold64.txt`;
  N=64 (>=50), median/min/max/stddev present.

### p1e-hot-leaf-attribution.md

- **§2.2 / §1.1 `track1_full_parse` wrapper cited `:103` — ACCEPT (V2 REVISE CLOSED).** The
  V2 REVISE was the fabricated-precision `:45` cite; V3 cites `:103` (`p1e:59`, `:182`),
  grep-verified `103: fn track1_full_parse`. §1.1 additionally cites verified exact lines
  (`sample` `:146`, assert `:250`, median `:160-165`, `mbps` `:138-142`) and openly flags
  "fixing the V1 paper-citation that misnamed line numbers" (`p1e:39`).
- **§2.3/§2.4 hot leaves + on-disk flame — ACCEPT.** `/tmp/skv17-p1e/{full_parse,fact_stream}.json.gz`
  on disk; `caller.py` walk backs `emit_fact_stream`; N=100 table backed by `css_canon n100`.
- **§ c/B posture `:288-302` — ACCEPT.** Adopts the resolved single reading; "falsified /
  physically-impossible" WITHDRAWN as wrong physics.
- **§"unprofiled"/"n/a" cells `:250-251`, `:341` — ACCEPT (not paper-closes).** The `n/a`
  cells carry stated cause: `:250-251` are the cyc/byte column for `emit_fact_stream`/
  `push_ascii_lower_hex` (a column the resolved posture marks non-load-bearing); they have
  a measured %self in the adjacent column. CH6 §3 requires a stated cause for a blank, not
  a measured number — satisfied.

### p1f-bench-canonical.md

- **§2.3 row-2 `track1_full_parse` wrapper cited `:103-105` — ACCEPT (V2 REVISE CLOSED).**
  V3 corrects the V2 `:43` cite to `:103-105` with an explicit correction note (`p1f:360-363`:
  "`:43` is the `RusageInfoV5` PMU struct; the `track1_full_parse` wrapper fn is `:103-105`
  … grep-verified. The `:43` cite in V2 was fabricated-precision"). Independently verified:
  `sed -n '43p'` → `struct RusageInfoV5`, `sed -n '103,105p'` → `fn track1_full_parse`. The
  correction is accurate.
- **§2.2 / §2.2.1 c/B posture — ACCEPT (V2 REJECT CLOSED).** The V2 "CPI<1.0 physically
  impossible" claim — the one the other three cited as authority — is WITHDRAWN as wrong
  physics (`p1f:323`, `:530`); the resolved single posture is carried (`p1f:274-283`).
- **§1.1 N>=50 harness + X2 verdict (assert `:250`) — ACCEPT.** Line cite verified; the
  single-canonical-harness verdict names `css_canon_bench` with correct lines.
- **§2.3 lightningcss full-CSSOM attribution — ACCEPT.** `lightningcss.json.gz` (13583
  samples) on disk; real symbol table proving the comparator materializes (tokenizer +
  typed Property/Selector build/drop). Discharges the fair-bar obligation.
- **§2.3 fact_stream attribution (`emit_fact_stream` `generated.rs:5,45,26`) — ACCEPT.**
  Multi-line inlining cite verified: `emit_fact_stream` at `:5`, `emit_declarations`
  (inlined) at `:45`, both grep-confirmed; honest inlining attribution, not fabricated
  precision.

## §3 — Counts + disposition summary

Itemized cells dispositioned: **24** (per-artefact ACCEPT clusters + the cross-artefact
§1 root-resolution finding). Every V2 REJECT and REVISE re-checked for fold; every "profiled"
claim re-checked against disk; every cited source line grep/sed-verified.

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| p1a | 4 | 0 | 0 |
| p1b | 4 | 0 | 0 |
| p1c | 4 | 0 | 0 |
| p1d | 4 | 0 | 0 |
| p1e | 4 | 0 | 0 |
| p1f | 5 | 0 | 0 |
| cross (§1) | 1 | 0 | 0 |
| **total** | **26** | **0** | **0** |

- ACCEPT: 26
- REVISE: 0
- REJECT: 0
- ACCEPT rate: 26 / 26 = **100%** (meets the §3Z >=95% convergence bar; CH6 converges on V3).

**Every V2 CH6 disposition folded (verified, not asserted):**
1. V2 ROOT REJECT (cross-§1, c/B interpretation split) → RESOLVED to one pass-wide posture,
   adopted verbatim by all six; the broken "P1-D §3 / P1-F §2.2 falsifies it" cross-cites
   are GONE (grep returns zero live hits). The resolution is more disciplined than either V2
   camp: cyc/byte is non-disambiguable → non-load-bearing, instr/byte carries every conclusion.
2. V2 REJECT p1a §1/§2.1 (FALSIFIED framing) → retracted at `p1a:15`.
3. V2 REJECT p1b §c/B-PROVENANCE → resolved at `p1b:40-52`.
4. V2 REJECT p1c §V2-posture/§2.5 → resolved at `p1c:37-52`.
5. V2 REJECT p1f §2.2/§2.2.1 ("CPI<1.0 impossible") → withdrawn at `p1f:323/530`.
6. V2 REVISE p1d §3.1 (orphan unilateral supersede) → over-claim withdrawn `p1d:526-527`,
   siblings now match; no longer an orphan.
7. V2 REVISE p1e §2.2 (wrapper `:45`) → corrected to `:103` (grep-verified).
8. V2 REVISE p1f §2.3 (wrapper `:43`) → corrected to `:103-105` with explicit note (sed-verified).

**CH6 posture (V3 vs V2 vs V1).** The arc closes cleanly. V1: two gross paper-closes (0-byte
atos table, missing PMU counters, fabricated harness verification) — REJECTed. V2: the two
V1 paper-closes CLOSED with real on-disk artefacts, but a subtler defect surfaced — a measured
number (cyc/byte) carried at face value with no single defensible interpretation (the posture
split) — REJECTed. V3: the posture split is RESOLVED to the defensible floor (non-disambiguable
→ non-load-bearing), propagated to all six, broken cross-cites struck, both wrapper lines
corrected and grep-verified. Every "profiled" claim carries an orchestrator-citable artefact
on disk; the N>=50 harness is code-asserting (`:250`), not promised; no §2 cell reads
"unprofiled"/"n/a" without a stated cause. CH6 finds zero open REJECT, zero open REVISE, and
**100% ACCEPT on V3** — CH6 converges. (Convergence is per-lens; the >=95%×2-consecutive and
zero-orphan-REVISE pass-gate is the aggregator's call across all CH lenses.)
