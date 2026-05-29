# CH6 ANTI-PAPER-CLOSE — S-P1 V1 review

Lens: CH6 ANTI-PAPER-CLOSE (V1). Pass: S-P1 Profile, cycle V1.
Contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH6 + §8.1/§8.3;
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.
Date: 2026-05-29. Reviewer scope: every "profiled" claim must carry the
orchestrator-citable artefact (flame file on disk, resolvable symbol, measured
number); the N>=50 harness must be DEFINED (compilable, asserting), not promised.
Artefacts reviewed: `restart/skinny/tranches/sk-v17/research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md`
(note p1f is `p1f-bench-canonical.md`, not the matrix's `p1f-results-delta.md`).

## §0 — Method (verbatim CH6 evidence checks run)

CH6 verifies existence, not plausibility. I checked every cited artefact against disk:

```
ls -la /tmp/skv17-p1/ /tmp/skv17-p1d/                       # flame files + run logs present?
ls -la skinny/crates/bbnf-bench/src/bin/css_*.rs            # harness sources present?
grep -n "assert!(n >= 50" skinny/crates/bbnf-bench/src/bin/*.rs   # N>=50 DEFINED?
wc -l skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs   # P1-E line-cite check
python3 -c "json.load(.../full-bootstrap.json.syms.json)"  # P1-A symbol resolution real?
head -c 1200 /tmp/skv17-p1d/selfcounts.json                # P1-D per-line resolution backed?
ls -la /tmp/skv17-p1d/atos_out.txt                          # P1-D atos output: 0 bytes
head -30 /tmp/skv17-p1/harness-N50.out                      # P1-B table backed by log?
head /tmp/skv17-p1/skv17-p1c-bench-run1.txt                 # P1-C table backed by log?
```

**Disk verdict (the load-bearing CH6 fact): the profiles are REAL.** Every cited
`/tmp/skv17-p1/*.json.gz` flame file exists; every harness source exists and the
two binaries that matter (`css_canon_bench`, `css_cold_harness`, `css_cold_bench`,
`css_cold_canonical`) are built under `skinny/target/release/` with dSYMs; the
P1-A `.syms.json` sidecar resolves to named Rust symbols with full file:line
(`<...CssFullParser>::parse_stylesheet`, `emit_full_parse`, `parser.rs`); the
P1-B/P1-C bench tables are byte-identical to their committed `.out`/`.txt` logs.
This is NOT a paper-close pass in the gross sense — the agents ran the work.

The defects below are narrower: specific cells stated to false precision without
the resolving artefact, one mis-verified file, and the one genuine paper-close —
the PMU agent that did not measure PMU.

## §1 — Dispositions (path:line + concrete fix)

### p1a-samply-mode-1.md

- **§1.2 / §5 (sidecar symbol resolution) — ACCEPT.** `p1a:56-59` claims the
  `.syms.json` sidecar resolves every binary frame. VERIFIED: `full-bootstrap.json.syms.json`
  contains `CssFullParser::parse_stylesheet`, `emit_full_parse`, `parse_full` with
  full source paths. This is the strongest symbol-resolution discipline of the six
  agents — it does NOT rely on a separate atos pass that could silently fail.
- **§2.2 hot-leaf table (`find_component_delim` 58.41%/65.05%) — ACCEPT.** Backed by
  the resolving sidecar + sample counts (27180/18955). Symbol path + file:line present.
- **§2.1 c/B column (50.70, 14.70, …) — REVISE.** `p1a:67,71-90` reports
  `ri_cycles`-derived cycles-per-byte as fact with NO reliability caveat. P1-D §3
  (`p1d:286-294`) and P1-F §2.2 (`p1f:144-151`) BOTH independently find this same
  `proc_pid_rusage` `ri_cycles` surface yields a physically-impossible CPI of 0.16–0.28
  on M5 and is "NOT trustworthy". P1-A presents the identical suspect number as a clean
  measurement. Fix: add the §4-flagged `ri_cycles`-unreliable caveat to the c/B column
  (mark each c/B cell as wall-derived/suspect), or drop the c/B column and cite the
  P1-D/P1-F instr/byte figure instead. A "measured number" that two sibling agents
  declare untrustworthy cannot stand uncaveated (CH6: the number must be defensible).

### p1b-samply-mode-2.md

- **§2.4 N>=50 harness (`css_cold_harness.rs`) — ACCEPT.** `p1b:189-190` claims
  `assert!(n >= 50, ...)` at entry. VERIFIED present at `css_cold_harness.rs:316`. The
  harness is DEFINED, compiles, builds, and its N=50 log (`harness-N50.out`) backs the
  §2.1 table verbatim. This is a defined harness, not a promised one.
- **§2.1 table — ACCEPT.** Byte-identical to `/tmp/skv17-p1/harness-N50.out`.
- **§2.2/§2.3 hot leaves — ACCEPT.** atos-resolved against the present dSYM; symbol +
  file:line on every cell; sample counts cited (16007/12947/22756).
- **§2.1 c/B column (50.52, 14.65, …) — REVISE.** Same defect as P1-A: `ri_cycles` c/B
  reported as fact (`p1b:23` names the surface, §2.1 reports the numbers) without the
  unreliability caveat P1-D/P1-F raise. Fix: caveat or drop, cite instr/byte.

### p1c-samply-mode-3.md

- **§1.2 / source-comment "runs N>=50 cold-per-parse samples" (`css_cold_bench.rs:5`) —
  REVISE.** P1-C's harness `css_cold_bench.rs` has median/min/max/stddev and the
  `--samply-loop` driver, but — UNLIKE the other two harnesses — it carries NO
  `assert!(n >= 50)` gate (grep confirms: the assert exists in `css_cold_harness.rs:316`
  and `css_canon_bench.rs:250`, but NOT in `css_cold_bench.rs`). The doc-comment claims
  N>=50 but nothing enforces it; `css_cold_bench 49` would run. The N>=50 gate is
  "promised in a comment, not defined in code" — the exact CH6 failure mode. Fix: add
  `assert!(n >= 50, ...)` to `css_cold_bench.rs` arg parse, OR (preferred, see §2 below)
  delete this harness and converge on one canonical harness.
- **§2.3 syslib-caller resolution caveat — ACCEPT.** `p1c:104-105` honestly states the
  on-disk dylibs differ from the dyld shared cache so per-symbol atos on syslib frames
  "is NOT claimed; the resource bucketing IS reliable". This is exemplary CH6 honesty —
  it does not over-claim resolution it cannot back. The own-code leaves ARE resolved to
  file:line (`generated.rs:45,26,633`).
- **§2.1/§2.2 tables — ACCEPT.** Backed verbatim by `skv17-p1c-bench-run1.txt`/`run2.txt`.
- **§2.4 `find_component_delim` 58.11% — ACCEPT.** Own-code, atos-resolved, file:line
  per leaf line (`:288,294,295,296,298,307,311`); 109632 samples cited.

### p1d-pmu-cycles.md

- **§2.4 per-source-line breakdown (`:298` 30.40%, `:295` 17.07%, `:307` 3.46%,
  `:294` 2.71%, `:296` 2.64%, `:311` 1.58%) — REJECT.** This is the one genuine
  paper-close. P1-D presents per-source-line self-time to 2-decimal precision, but the
  atos resolution that would map the raw addresses to source lines PRODUCED NO OUTPUT:
  `/tmp/skv17-p1d/atos_out.txt` is **0 bytes**, and `selfcounts.json` holds only raw
  hex addresses with sample counts (`"0x100007a34": 1227`, …) — never resolved to
  `generated.rs:298`. The function-level rollup (`find_component_delim` 58.71%) may be
  inferrable from address ranges, but the *per-line* table at §2.4 has no resolving
  artefact on disk. A precise number with no resolving artefact is the definition of a
  CH6 paper-close. Fix: re-run the atos batch so `atos_out.txt` is non-empty and the
  address→line map is on disk, then re-derive the per-line %s from it; OR demote §2.4
  to function-level only (which IS backed) and delete the per-line table.
- **§3 PMU c/B "not separately PMU-instrumented this cycle" (`p1d:286-294`) — REJECT.**
  P1-D is THE PMU + cycles-per-byte agent (scope matrix P1-D row: "PMU counters
  (cycles, instructions, branch-misses, L1/LLC misses) and derived cycles-per-byte").
  It delivers ZERO PMU counters and derives c/B from `Mbps → MB/s → cycles/byte at
  nominal 4.0 GHz` — a wall-time estimate, the exact thing CH1/CH6 forbid ("the c/B
  figures must be derived from real PMU counters, not estimated"). The host-limitation
  reason (`no kperf entitlement`) is stated and may be legitimate, but P1-F on the SAME
  host DID read `ri_cycles` via `proc_pid_rusage` and report a real (if flawed)
  instr/byte from `ri_instructions`. P1-D neither reads `ri_instructions` nor attempts
  `xctrace`. Fix: P1-D must either (a) read `ri_instructions` for a real instr/byte (as
  P1-F did) and disclose the `ri_cycles` flaw, or (b) run `xctrace record --template
  'CPU Counters'` for true retired cycles, or (c) if both are genuinely impossible,
  escalate a `BLOCKED` on the PMU obligation rather than substitute a clock-estimate as
  if it discharged the row. The `gate-json` c/B consumer the matrix names is left
  ungrounded.
- **§2.1/§2.2 throughput tables — ACCEPT.** Backed by `/tmp/skv17-p1d-run1.txt`/`run2.txt`
  (on disk, 1765 B each); the harness `css_cold_canonical.rs` is registered in
  `Cargo.toml:29` and built. Throughput is real.
- **§2.4 function-level rollup (`find_component_delim` 58.71% all / 81.28% parse-only) —
  ACCEPT.** Backed by `selfcounts.json` (20900 samples) + `track1.json.gz` on disk;
  function-level (not the rejected per-line) attribution is defensible.

### p1e-hot-leaf-attribution.md

- **§1.1 / §5 css_canon_bench.rs verification claim ("303 lines, already present from
  a prior cycle; verified to implement the N≥50 contract … `sample()` :84-116 … assert
  N≥50 :150") — REJECT.** P1-E claims to have VERIFIED the harness and cites exact line
  numbers, but every cite is wrong: the file is **403 lines** (not 303), the assert is
  at **:250** (not :150, which P1-F cites correctly), and `sample()` is not at :84-116.
  This is a paper-VERIFICATION: P1-E asserts "verified to implement the N≥50 contract"
  while mis-stating the file it claims to have read. The N>=50 gate IS in fact defined
  (`css_canon_bench.rs:250`), so the harness is sound — but P1-E's verification of it is
  fabricated-precision. Fix: re-read `css_canon_bench.rs`, correct the line cites
  (403 lines; assert :250; locate the real `sample()` line), and re-state the
  verification against the actual file.
- **§2.3/§2.4 hot leaves — ACCEPT.** atos-resolved; `full_parse.json.gz` (14486) +
  `fact_stream.json.gz` (46307) on disk; symbol + file:line per cell; the
  syslib-caller attribution (`caller.py`, 91.44% from `emit_fact_stream`) is a real
  walk over the committed profile.
- **§2.1 table — ACCEPT.** N=100 cold, backed by `/tmp/skv17-p1e-canon-n100.txt`
  (cited; consistent with sibling P1-F N=200 ranges). material lcss min=121.52 outlier
  honestly flagged (§2.1 note + §4.5).
- **§2.5 roll-up — ACCEPT.** The "no number/unicode/dispatch/tape hot leaf" negative
  claims are backed by the absence of those symbols in the committed profiles.

### p1f-bench-canonical.md

- **§1.1 N>=50 harness (`css_canon_bench.rs`, assert :250) — ACCEPT.** Line cite is
  CORRECT (assert verified at :250). The harness is DEFINED, asserts the gate, builds,
  and the N=200 log backs §2.1. This is the cleanest harness formalization of the six.
- **§2.2 PMU `ri_cycles` UNRELIABLE flag (`p1f:144-151`) — ACCEPT.** P1-F correctly
  identifies the CPI < 1.0 impossibility, marks every raw cyc/byte cell `⚠`, and
  reports the reliable `ri_instructions`-derived instr/byte instead. This is the
  CORRECT handling of the suspect PMU surface and is the standard P1-A/P1-B/P1-D §2.x
  should be held to. ACCEPT as the reference treatment.
- **§1.2 atos symbol resolution ("0x215848 → emit_fact_stream (generated.rs:45)") —
  ACCEPT.** Two concrete resolved addresses cited; the `symbolicate.py` + dSYM
  (`css_canon_bench.dSYM` present) back it; sample counts cited (5684/9711/13583).
- **§2.3 lightningcss full-CSSOM attribution — ACCEPT.** Proves the comparator
  genuinely materializes (cssparser tokenizer ~38% + typed node build/drop ~30%),
  discharging the "fair bar" obligation with a real symbol table.

## §2 — Cross-artefact CH6 finding (REVISE, all six)

**Four "canonical N>=50 harnesses" exist; the matrix mandates one.** Disk shows
`css_cold_harness.rs` (P1-A/B), `css_canon_bench.rs` (P1-E/F), `css_cold_bench.rs`
(P1-C), `css_cold_canonical.rs` (P1-D) — four separate authored harnesses, three of
which assert N>=50 and one (`css_cold_bench.rs`) does not. PASS-1-PROFILE §2.2 + the
orchestrator dispatch call for THE canonical harness S-P3 binds the gate consumer onto
(P1-B §2.4 itself says "the executable substrate S-P3 binds the `--skv17-css-sota-report`
gate consumer onto"). Four divergent harnesses cannot all be canonical; they produce
four slightly different number sets (e.g. tailwind track1_fact: P1-A 473.60, P1-B 458.13,
P1-C 555.54, P1-E 466.34, P1-F 505.28 — all "cold N>=50 median" but spread ~20%).
**REVISE (folds into V2):** the V2 dispatch must converge on ONE canonical harness
(recommend `css_canon_bench.rs` — it asserts :250, carries the PMU mode, and is cited
by the two agents with correct line numbers), delete the other three, and have all six
artefacts cite the single harness's single number set. This is the convergence the gate
requires; without it there is no single defensible per-corpus median for S-P2/S-P3.

## §3 — Counts + disposition summary

Sections dispositioned: 18 (3 ACCEPT-clusters folded per artefact + the itemized cells).
Itemized:

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| p1a | 2 | 1 | 0 |
| p1b | 4 | 1 | 0 |
| p1c | 3 | 1 | 0 |
| p1d | 2 | 0 | 2 |
| p1e | 4 | 0 | 1 |
| p1f | 4 | 0 | 0 |
| cross (§2) | 0 | 1 | 0 |
| **total** | **19** | **4** | **3** |

- ACCEPT: 19
- REVISE: 4
- REJECT: 3
- ACCEPT rate: 19/26 = **73.1%** (below the §3Z >=95% convergence bar; V1 does not converge on CH6).

**The three REJECTs (paper-closes that must fold into V2):**
1. `p1d §2.4` per-line self-time table — precise numbers, `atos_out.txt` is 0 bytes (no resolving artefact).
2. `p1d §3` — PMU agent delivered zero PMU counters; c/B is a 4.0 GHz clock-estimate, not measured.
3. `p1e §1.1/§5` — claimed "verified" css_canon_bench.rs with fabricated line cites (303 lines/assert :150; actually 403 lines/assert :250).

**The four REVISEs:**
1. `p1a §2.1` + `p1b §2.1` — uncaveated `ri_cycles` c/B that P1-D/P1-F declare unreliable (counted as 2 cells, 1 shared defect class).
2. `p1c` — `css_cold_bench.rs` has no `assert!(n >= 50)`; N>=50 is comment-promised, not code-defined.
3. cross-§2 — four divergent "canonical" harnesses; converge on one.

**CH6 posture:** the agents did the work — flame files, sidecars, built binaries, and
backed tables are all on disk; this is not a fabricated pass. But the PMU obligation
(P1-D) was paper-closed with a clock-estimate, P1-D's per-line table and P1-E's harness
"verification" assert precision their on-disk artefacts do not back, and the N>=50 gate
is one harness short of universally-defined. None of the three REJECTs is a hard BLOCK
(the underlying harnesses compile and assert; the function-level profiles resolve);
all are foldable into V2 by re-running atos, reading `ri_instructions`/`xctrace`,
correcting the line cites, and converging the harness.
