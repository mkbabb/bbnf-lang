# CH6 ANTI-PAPER-CLOSE — S-P1 V4 review

Lens: CH6 ANTI-PAPER-CLOSE (V4). Pass: S-P1 Profile, cycle V4.
Contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH6 + §8.1/§8.3;
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.
Date: 2026-05-29. Master HEAD `6496fecae`.
Reviewer scope: every "profiled" claim must carry the orchestrator-citable artefact
(flame file on disk, resolvable symbol, measured number); the N>=50 harness must be
DEFINED (compilable, code-asserting), not promised; no §2 cell reading "unprofiled"/"n/a"
without a stated cause.
Artefacts reviewed: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
p1f-bench-canonical}.md` (current on-disk, re-written 15:50–15:56).

## §0 — Method (verbatim CH6 evidence checks run, V4)

CH6 verifies existence, not plausibility. Every cited artefact re-checked against disk
fresh this cycle; every cited source line grep/sed/wc-verified against the benched skinny
tree on master HEAD `6496fecae`:

```
ls -la /tmp/skv17-p1/ /tmp/skv17-p1c-v2/ /tmp/skv17-p1d/ /tmp/skv17-p1e/   # flame + logs present?
wc -l /tmp/skv17-p1d/atos_v2.txt ; grep -c "(" /tmp/skv17-p1d/atos_v2.txt  # resolved-symbol table intact?
cat /tmp/skv17-p1/css_canon_pmu_v4.txt                                     # fresh V4 PMU: real ri_cycles/ri_instructions?
diff /tmp/skv17-p1/css_canon_pmu_v2.txt /tmp/skv17-p1/css_canon_pmu_v4.txt # v2→v4 run delta on the load-bearing counter?
grep -n "assert!(n >= 50\|fn sample\|Instant::now\|elapsed\|fn track1_full_parse\|RusageInfoV5\|fn main" \
   skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs                     # N>=50 DEFINED (runtime assert)?
grep -ni "unprofiled\|n/a\|TODO\|TBD\|will measure\|promised" p1*.md       # bare paper-close cells?
grep -ni "physically impossible\|falsifies it\|falsified" p1*.md          # live broken authority cross-cite?
for f in p1a..p1f: grep -o "css_canon_pmu_v[0-9]*\.txt" $f                 # PMU-snapshot provenance per artefact
```

**Disk verdict (the load-bearing CH6 fact): the V4 profiles are REAL, the N>=50 harness is
code-asserting (not promised), every cited flame/log file exists on disk, and there is ZERO
bare "unprofiled"/"n/a" cell and ZERO live broken "falsified" cross-cite.** The V1 gross
paper-closes (0-byte atos, missing PMU) and the V2 ROOT REJECT (the c/B interpretation split)
remain CLOSED. One benign cross-artefact provenance divergence surfaces in V4 (two PMU
snapshots coexist: P1-A cites `_v2`, P1-F cites a fresh `_v4`); it is a cosmetic traceability
nit, NOT a paper-close — the load-bearing instr/byte counter is stable across both snapshots
to <0.2% and no conclusion shifts. Itemized below.

Concretely verified this cycle:

- **N>=50 harness DEFINED, not promised.** `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:250`
  carries `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` — a RUNTIME
  assert, not a comment (grep-verified). Cold discipline is code-enforced: `fn sample` `:146`
  times exactly one `parse(black_box(input))` between `Instant::now()` `:154` and
  `.elapsed()` `:156`, result `black_box`-dropped `:157`; ONE pre-touch parse OUTSIDE the
  timed window `:152` faults in source pages only. `fn track1_full_parse` `:103`, `read_rusage_v5`
  `:86`, `RusageInfoV5` `:43`, `fn main` `:179` all present and grep-confirmed. Real cold
  harness, CH6 §8.1 honoured.
- **The atos resolved-symbol table is intact and real.** `/tmp/skv17-p1d/atos_v2.txt` is **199
  lines, all 199 carrying a resolved `<mangled-symbol> (in css_track1_profile) (file:line)`**
  (grep-verified `grep -c "("` = 199). Top entries resolve `CssFullParser::find_component_delim`
  to `generated.rs:{294,295,298}` — the hot byte-class scan loop. No 0-byte table (the V1 gross
  paper-close stays closed).
- **A FRESH V4 PMU run exists with real counters.** `/tmp/skv17-p1/css_canon_pmu_v4.txt`
  (15:52, 16 data rows) carries REAL `cycles`+`instructions` per corpus×workload
  (e.g. `bootstrap track1_full_parse cycles=6913856543 instructions=25003802730
  cycles_per_byte=14.8492 cpi=0.2765 mbps=2300.094`). `diff` against `_v2` shows the expected
  run-to-run cycle/Mbps wobble; **the load-bearing `instructions` counter is stable** (bootstrap
  full 25012559767→25003802730 = 0.035% delta; animate fact 40155914822→40076384636 = 0.20%
  delta — both inside the "<0.5%" stability P1-A §2.1b itself claims). No nominal-clock
  substitute anywhere.
- **Every cited flame file exists on disk.** `/tmp/skv17-p1/{fact,full}-{bootstrap,tailwind}.json.gz`
  + `.syms.json` sidecars, `/tmp/skv17-p1/{fact_stream,full_parse,lightningcss}.json.gz`,
  `/tmp/skv17-p1c-v2/{fact,full}.json.gz` + `canon.txt` (7616 B), `/tmp/skv17-p1e/{full_parse,
  fact_stream}.json.gz`, `/tmp/skv17-p1d/{track1,track1-v2}.json.gz`. All verified `ls`.

## §1 — The V3 convergence holds; no new paper-close opened in V4

V3 CH6 converged at 100% (26/26 ACCEPT). V4 re-verifies that the V3 folds did not regress and
that the V4 re-emission introduced no new paper-close. Three load-bearing re-checks:

1. **No bare "unprofiled"/"n/a"/"TODO"/"TBD"/"will measure" cell.** The grep returns SIX hits
   across the six artefacts; **every one carries a stated cause** (CH6 §3 requires a stated
   cause for a blank, not a measured number in the blank):
   - `p1a:164` `n/a on benched skinny` — explicit cause: "the ~3 Mbps figure is the core-tree
     eager `OpenFrame` path; it is NOT a benched skinny CSS plane." Stated cause. ACCEPT.
   - `p1b:222` `n/a (harness-local)` — explicit cause: "pure timing scaffold, NOT a
     retained/second parse pass". Stated cause. ACCEPT.
   - `p1b:296` `n/a — NOT the benched parse path` — explicit cause: "a separate retime
     workload … the 269x/14x slowdown source". Stated cause. ACCEPT.
   - `p1c:342` `unprofiled on benched path` — this is the **SK-V16-prior column** of a
     reconciliation table; the adjacent P1-C measured column carries `2125–2537 Mbps/corpus;
     2.0–3.0× lightningcss` with classification "A — new measured truth." The cell describes
     the prior unmeasured *state* now resolved to a measured number. Stated cause + measured
     resolution. ACCEPT.
   - `p1e:250` / `p1e:251` `n/a` — the cyc/byte column for `emit_fact_stream`/
     `push_ascii_lower_hex`; the resolved posture (§2) marks cyc/byte non-load-bearing, and the
     adjacent column carries the measured %self-time (24.59%, 9.11%). Stated cause (posture
     column) + measured number in the load-bearing column. ACCEPT.
   The two `p1c:95`/`:343`/`:344`/`:431` hits are prose, tool-version, and same-run-bar
   statements, NOT table-cell paper-closes (`:95` explicitly reads "code-enforced, not
   promised"). No bare paper-close cell exists in V4.

2. **No live broken "falsified / physically impossible" authority cross-cite.** The grep
   returns hits in p1a/p1b/p1c/p1d/p1e/p1f, and **every single one is the WITHDRAWN/RETRACTED
   framing**, not a live authority cite: `p1a:17` "was wrong physics … is retracted here";
   `p1b:43` "was wrong physics"; `p1c:51` "struck at the V2→V3 CHALLENGE"; `p1d:531` quoting the
   retired framing; `p1e:292` "is NOT physically impossible"; `p1f:62/90/323/370/581` all
   "WRONG PHYSICS … WITHDRAWN". The V2 ROOT REJECT cross-cite (one artefact citing another's
   "falsified" claim as live authority) returns ZERO live hits. The V3 resolution holds.

3. **The single pass-wide COST-SURFACE POSTURE is carried verbatim by all six** (instr/byte
   load-bearing + reliable; cyc/byte RAW + non-load-bearing because non-disambiguable from the
   `proc_pid_rusage` interface, NOT impossible). Verified at `p1a:17`, `p1b:43`, `p1c:51`,
   `p1d:531`, `p1e:292`, `p1f:323/370`. No artefact rests a conclusion on cyc/byte.

## §2 — Dispositions (path:line + on-disk verification)

### p1a-samply-mode-1.md

- **§0 V4-FOLD NOTE `:15` — ACCEPT.** Honestly states V4 re-emits V3 content with every
  load-bearing citation re-verified fresh on master `6496fecae`: hot-leaf lines
  `generated.rs:{61,103,118,189,242,288,295,320,628}` "confirmed"; `css_canon_n200_v2.txt` +
  `css_canon_pmu_v2.txt` "byte-identical to §2.1/§2.1b"; IPC 3.51–6.23 "recomputed from on-disk
  `cpi`". This is a self-auditing fold-note, the opposite of a paper-close.
- **§2.1b instr/byte authoritative cost surface (`:102–110`) — ACCEPT.** Cited values are
  byte-faithful to `css_canon_pmu_v2.txt` (verified: animate fact 279.83 i/B, bootstrap full
  53.72 i/B, IPC band recomputed). The cited PMU file exists; the load-bearing counter is
  stable to the fresh V4 re-run (≤0.2%).
- **§2.2 hot-leaf table (`find_component_delim` 58.41%/65.05%, `generated.rs:288`) — ACCEPT.**
  Sidecar-symbolicated (`.syms.json` on disk); `find_component_delim` verified at the resolved
  atos lines `generated.rs:{294,295,298}` and the `:288` fn head. Real symbol, real %self.
- **§5 sidecar resolution discipline (`:191`) — ACCEPT.** `--unstable-presymbolicate`
  `.syms.json` sidecars on disk for all four mode-I profiles; strongest resolution path.
- **PMU-snapshot provenance (`:49`/`:100`/`:191`, cites `css_canon_pmu_v2.txt`; struck cyc/B
  `15.17` at `:84`) — ACCEPT with NOTE (cosmetic, non-blocking).** P1-A cites the `_v2` PMU
  snapshot while P1-F (§2.2, `:152/:317`) cites a fresher `_v4` snapshot taken this cycle
  (15:52). The struck cyc/B `15.17` at `p1a:84` corresponds to `_v2`; the `_v4` value is
  `14.85`. **This is NOT a paper-close and NOT a load-bearing defect:** (a) the cited `_v2`
  file exists on disk and matches the cells; (b) the divergent number is the *struck-through,
  RAW, non-load-bearing* cyc/B column, on which the posture explicitly rests no conclusion;
  (c) the load-bearing instr/byte is stable v2→v4 to ≤0.2% (verified). It is a within-pass
  traceability nit: two PMU snapshots coexist without a single note reconciling them. If a V5
  cycle runs, the deft fix is one line in P1-A §2.1b/§5 — "the V4 re-run `css_canon_pmu_v4.txt`
  reproduces these instr/byte to ≤0.2%; the struck `15.17` cyc/B is the `_v2` snapshot,
  non-load-bearing." It does not block convergence; CH6 ACCEPTs the cell because the artefact
  on disk exists and the load-bearing claim is stable.

### p1b-samply-mode-2.md

- **§"c/B PROVENANCE" `:43` — ACCEPT.** Carries the resolved posture verbatim ("was wrong
  physics"); no live broken cross-cite.
- **§2.3 wrapper cited `css_canon_bench.rs:103-105` (`:222`) — ACCEPT.** `fn track1_full_parse`
  verified at `:103`; the `n/a (harness-local)` cyc/B carries its stated cause inline.
- **§4.3 retime plane `:296` (`3.093` Mbps, `n/a — NOT the benched parse path`) — ACCEPT.**
  Stated cause; the 3 Mbps figure correctly attributed to the non-benched typed-retime plane,
  preventing the SK-V16 "~70 Mbps / 14× slow" conflation from re-entering as benched truth.
- **§2.1 instr/byte column — ACCEPT.** Sourced from the canonical PMU run; cyc/byte
  co-reported non-load-bearing per the resolved posture.

### p1c-samply-mode-3.md

- **§1.2 N>=50 gate `:95` — ACCEPT.** Explicitly "the contract is now code-enforced, not
  promised" — the exact CH6 distinction, satisfied by the `:250` assert.
- **§"V3 cycles-per-byte posture" `:51` — ACCEPT.** Adopts the pass-wide reading; the
  "physically impossible" framing is named as "struck at the V2→V3 CHALLENGE", not live.
- **§2.3 reconciliation table `:341–346` (incl. `:342` "unprofiled on benched path") — ACCEPT.**
  The "unprofiled" cell is the SK-V16-prior column; the P1-C measured column resolves it to
  `2125–2537 Mbps` with on-disk flame backing (`/tmp/skv17-p1c-v2/{fact,full}.json.gz` + 7616-B
  `canon.txt`). Stated prior state + measured resolution.
- **§2.4/§2.5 hot leaves — ACCEPT.** Syslib resource-bucketing honesty caveat retained
  (per-symbol atos on syslib "is NOT claimed; the resource bucketing IS reliable") — exemplary
  CH6 honesty. Tool versions `:431` (rustc 1.96.0-nightly, samply 0.13.1, atos) present.

### p1d-pmu-cycles.md

- **§3.1 PMU posture `:531` — ACCEPT.** The over-claim ("proven 4.27 GHz / supersedes A/B/F")
  stays withdrawn; the under-claim ("physically impossible / falsified") quoted only as the
  retired framing; `proc_pid_rusage.ri_cycles` non-disambiguability stated.
- **§3.2 PMU table — ACCEPT.** Byte-faithful to `/tmp/skv17-p1d-v2-pmu.txt` (real `ri_cycles`+
  `ri_instructions`); instr/byte load-bearing, cyc/byte RAW non-load-bearing.
- **§2.4 per-line self-time table — ACCEPT (V1 REJECT stays closed).** `/tmp/skv17-p1d/atos_v2.txt`
  is 199 resolved `<symbol> (file:line)` lines on disk (verified `wc -l` + `grep -c`).
- **§2.1/§2.2 cold tables — ACCEPT.** Byte-faithful to `/tmp/skv17-p1d-v2-cold64.txt`; N=64
  (≥50), median/min/max/stddev present.

### p1e-hot-leaf-attribution.md

- **§2.5 hot-leaf roll-up `:245–254` — ACCEPT.** Every leaf carries symbol + file:line + %self
  + class; `find_component_delim generated.rs:288 56.52%`, `emit_fact_stream generated.rs:5
  24.59%`, `push_ascii_lower_hex generated.rs:628 9.11%`. The two `n/a` cells (`:250`/`:251`)
  are the non-load-bearing cyc/byte column with the measured %self in the adjacent column —
  stated cause, not a paper-close.
- **§ c/B posture `:292` — ACCEPT.** "NOT physically impossible" — the retracted framing.
- **§2.x on-disk flame — ACCEPT.** `/tmp/skv17-p1e/{full_parse,fact_stream}.json.gz` on disk;
  `caller.py` walk backs the `emit_fact_stream` attribution.

### p1f-bench-canonical.md

- **§1 method `:152` + §2.2 source `:317` (cites fresh `css_canon_pmu_v4.txt`) — ACCEPT.** P1-F
  re-ran `CSS_CANON_PMU=1 css_canon_bench 2000 > css_canon_pmu_v4.txt` this cycle; the file
  exists on disk (15:52) with real counters. The §2.2 table values (bootstrap full 53.70 i/B,
  14.85 cyc/B, 2300.1 Mbps `:334`) are byte-faithful to `_v4`. Fresh measured truth, fully
  traceable.
- **§2.2/§2.2.1 c/B posture `:323/:370/:581` — ACCEPT.** The V2 "CPI<1.0 physically impossible"
  claim is withdrawn as wrong physics throughout; resolved single posture carried.
- **§1.1 N>=50 harness + X2 verdict (assert `:250`) — ACCEPT.** Line cite verified against the
  live harness; the single-canonical-harness designation names `css_canon_bench` correctly.
- **§2.3 lightningcss full-CSSOM + fact_stream attribution — ACCEPT.** `lightningcss.json.gz`
  on disk proving the comparator materializes full CSSOM (fair bar); `emit_fact_stream`
  inlining cite `generated.rs:5` verified.

## §3 — Counts + disposition summary

Itemized cells dispositioned: **25** (per-artefact ACCEPT clusters + the cross-artefact §1
no-paper-close finding). Every cited "profiled" claim re-checked against disk fresh this
cycle; every cited source line grep/sed/wc-verified; the fresh V4 PMU run diffed against the
v2 snapshot to confirm load-bearing stability.

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| p1a | 5 | 0 | 0 |
| p1b | 4 | 0 | 0 |
| p1c | 4 | 0 | 0 |
| p1d | 4 | 0 | 0 |
| p1e | 3 | 0 | 0 |
| p1f | 4 | 0 | 0 |
| cross (§1) | 1 | 0 | 0 |
| **total** | **25** | **0** | **0** |

- ACCEPT: 25
- REVISE: 0
- REJECT: 0
- ACCEPT rate: 25 / 25 = **100%** (meets the §3Z ≥95% convergence bar; CH6 converges on V4,
  second consecutive cycle ≥95% after V3's 100% — the per-lens two-consecutive condition is met).

**V4 CH6 posture.** No regression from the V3 convergence. The N>=50 harness is code-asserting
at `css_canon_bench.rs:250` (DEFINED, not promised); the 199-line atos resolved-symbol table is
intact; a fresh V4 PMU run exists on disk with real `instructions`/`cycles`; every cited flame
file is on disk; every "n/a"/"unprofiled" cell carries a stated cause with a measured number in
the load-bearing column; every "physically impossible / falsified" string is the WITHDRAWN
framing, never live authority. The single non-ACCEPT-blocking observation is a cosmetic
provenance nit (P1-A cites the `_v2` PMU snapshot, P1-F a fresher `_v4`); CH6 ACCEPTs both
because each cites a real on-disk file and the load-bearing instr/byte counter is stable across
the two snapshots to ≤0.2% — no conclusion shifts. CH6 finds **zero open REJECT, zero open
REVISE, 100% ACCEPT on V4** — CH6 converges. (Convergence is per-lens; the ≥95%×2-consecutive
and zero-orphan-REVISE pass-gate is the aggregator's call across all CH lenses.)
