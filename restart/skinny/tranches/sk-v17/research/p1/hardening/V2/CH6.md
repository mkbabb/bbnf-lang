# CH6 ANTI-PAPER-CLOSE — S-P1 V2 review

Lens: CH6 ANTI-PAPER-CLOSE (V2). Pass: S-P1 Profile, cycle V2.
Contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH6 + §8.1/§8.3;
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.
Date: 2026-05-29. Reviewer scope: every "profiled" claim must carry the
orchestrator-citable artefact (flame file on disk, resolvable symbol, measured
number); the N>=50 harness must be DEFINED (compilable, asserting), not promised.
Artefacts reviewed: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
p1f-bench-canonical}.md`.

## §0 — Method (verbatim CH6 evidence checks run)

CH6 verifies existence, not plausibility. Every cited artefact checked against disk:

```
ls -la /tmp/skv17-p1/ /tmp/skv17-p1d/ /tmp/skv17-p1c-v2/ /tmp/skv17-p1e/   # flame + logs present?
wc -l /tmp/skv17-p1d/atos_v2.txt ; head /tmp/skv17-p1d/atos_v2.txt          # V1 0-byte paper-close closed?
head /tmp/skv17-p1d-v2-pmu.txt /tmp/skv17-p1d-v2-cold64.txt                 # P1-D PMU+cold tables backed?
head /tmp/skv17-p1/css_canon_pmu.txt                                        # P1-A/B/C/F PMU table backed?
grep -n "assert!(n >= 50" .../bin/css_canon_bench.rs                        # N>=50 DEFINED?
grep -n "read_rusage_v5|ri_cycles|ri_instructions|fn sample|WORKLOADS" .../css_canon_bench.rs
wc -l .../css_canon_bench.rs ; grep -n "fn track1_full_parse" ...           # P1-E/P1-F line cites
python3 -c "for cpi in (.157,.173,.198,.269,.285): print(1/cpi)"            # IPC physics of the c/B dispute
```

**Disk verdict (the load-bearing CH6 fact): the V2 profiles are REAL and the two V1
gross paper-closes are CLOSED.** The V1 `p1d` 0-byte `atos_out.txt` per-line table is
resolved: `/tmp/skv17-p1d/atos_v2.txt` is **199 lines** of `<symbol> (file:line)`,
every leaf resolving to `CssFullParser::find_component_delim (generated.rs:298/295/294/
307/296)` / `consume_balanced_at (:327/336/323)`. The V1 PMU paper-close is closed:
`/tmp/skv17-p1d-v2-pmu.txt` now carries **real `ri_cycles`+`ri_instructions` counters**
(e.g. bootstrap track1_full cycles=6737349921 instructions=25010386205), and P1-D §3.2
is byte-faithful to it. `css_canon_bench.rs:250` asserts `n >= 50` (verified); the
harness builds, carries `read_rusage_v5` (`:86`), `WORKLOADS` (`:123`), `sample`
(`:146`), `CSS_CANON_PMU` (`:211`), `CSS_CANON_PROFILE` (`:183`). Every cited flame
file exists: `/tmp/skv17-p1/{fact,full}-{bootstrap,tailwind}.json.gz` + `.syms.json`,
`/tmp/skv17-p1c-v2/{fact,full}.json.gz`, `/tmp/skv17-p1e/{full_parse,fact_stream}.json.gz`,
`/tmp/skv17-p1/{fact_stream,full_parse,lightningcss}.json.gz`. The N>=50 gate is now
universally code-enforced on the ONE canonical harness `css_canon_bench`. This is NOT a
paper-close pass; the V1 CH6 REJECTs folded.

The V2 defects below are narrower but one is structural: the pass advertises "ONE c/B
posture" while shipping **two mutually exclusive interpretations of the same measured
ri_cycles number**, with four artefacts citing the fifth as authority for a claim the
fifth (P1-D V2) explicitly disavows. A measured number whose meaning the pass cannot
agree on is presented to false certainty — the "ONE posture" sentence is the paper.

## §1 — Cross-artefact CH6 finding (REJECT — the load-bearing V2 defect)

**The "ONE c/B posture" is two contradictory postures; four artefacts cite an
authority (P1-D) that now asserts the opposite. (REJECT — folds into V3.)**

Every artefact carries a banner asserting the pass adopts ONE cycles-per-byte posture.
But the content splits cleanly into two camps over the SAME `ri_cycles` measurement
(identical numbers in every table; only the *interpretation* differs):

- **Camp FALSIFIED** — `p1a:15` ("That counter is **falsified** … physically impossible
  … corroborated by P1-D §3 and P1-F §2.2"), `p1a:100`, `p1b:39-45` ("reference-clock
  tick, NOT retired core cycles, and P1-F §2.2 / P1-D §3 both falsify it"),
  `p1c:37-45` ("UNRELIABLE … physically impossible … P1-D/P1-F concur"), `p1c:276`,
  `p1f:253-260` + `p1f:289-307` ("Sub-1.0 CPI is impossible for retired-instruction CPI
  on M5 … `ri_cycles` is NOT reporting retired core cycles — it is a reference-clock /
  fixed-frequency tick").
- **Camp VALID-HIGH-IPC** — `p1d:351-380` ("**That falsification is itself incorrect,
  and this pass corrects it.** … `ri_cycles` IS a real core-cycle counter ticking at the
  M5 Max P-core clock. The sub-1.0 CPI is **not a counter error**: it is high IPC … IPC
  of 3.6-6.4 is entirely physical on the Apple M5 Max's ~8-wide … core"), and
  `p1d:378-380` explicitly: "It **also supersedes** the 'ri_cycles unreliable' line
  P1-A/P1-B/P1-F carried."

This is a true contradiction, not a phrasing nuance:
1. `p1a:15` and `p1b:42` cite **"P1-D §3"** as the corroborating falsifier. P1-D §3 (V2)
   is the artefact that disavows the falsification. The citation is now circular-broken:
   the authority cited for "falsified" is the artefact asserting "not falsified."
2. The physics favors P1-D, which deepens the defect rather than excusing it. The Apple
   M5 P-core is ~8-wide decode/dispatch; the claimed CPI band 0.157-0.285 ⇒ **IPC
   3.51-6.37** (computed: 0.157→6.37, 0.173→5.78, 0.198→5.05, 0.269→3.72, 0.285→3.51).
   Sustained IPC 3.5-6.4 on a tight, well-predicted byte-scan loop is plausible on a
   wide OoO core; P1-D's xctrace cross-check (`/tmp/skv17-p1d/xc-test.trace`, 2.298 s for
   N=60) and its steady 4.19-4.29 GHz derivation across inst-dense and inst-sparse
   workloads are a stronger argument than the four artefacts' bare "IPC>3.5 impossible"
   assertion (`p1f:299-301`, `p1c:39`). So the four FALSIFIED-camp artefacts most likely
   carry the WRONG interpretation AND a stale cross-cite.
3. CH6 does not adjudicate which physics reading is correct here; it rejects shipping
   BOTH to "false precision without a defensible interpretation." The cyc/byte cells in
   `p1a §2.1` (struck), `p1c §2.5`, `p1f §2.2` are presented as measured numbers whose
   single agreed meaning the pass asserts it has ("ONE posture") and does not have.

**Fix (folds into V3):** the orchestrator must resolve the c/B interpretation to ONE
reading and propagate it to all six. Given the physics + the xctrace cross-validation,
the defensible resolution is P1-D's: `ri_cycles` is a valid fixed-frequency core counter,
sub-1.0 CPI = high IPC, instr/byte remains the PRIMARY plane-ranking density and
cyc/byte is a co-reported validated counter. P1-A/§2.1 (struck c/B column with "falsified"
caption), P1-B §"c/B PROVENANCE"/§2.1/§3/§4.6/§5, P1-C §"V2 c/B posture"/§2.5, and P1-F
§2.2/§2.2.1/§4.5 must be rewritten to the single resolved reading, and every "P1-D §3 /
P1-F §2.2 falsify it" cross-cite struck or re-pointed. Until then there is no single
defensible cost-density interpretation for S-P2 to ground on — the precise inverse of
what CH6 protects. NOTE: this is an interpretation/narrative REJECT, not a data REJECT —
the underlying `ri_cycles`/`ri_instructions` counters on disk are real and reproducible
(`p1f:285` <0.5% across runs); the instr/byte column (the actual S-P2 input) is sound and
identical across all six. The REJECT is on the unresolved meaning of the co-reported
cyc/byte number, not on the measurement.

## §2 — Dispositions (path:line + concrete fix)

### p1a-samply-mode-1.md

- **§1.2 / §5 sidecar symbol resolution — ACCEPT.** `p1a:60-63,193-197` claims the
  `--unstable-presymbolicate` `.syms.json` sidecars resolve every binary frame. VERIFIED:
  `/tmp/skv17-p1/full-bootstrap.json.syms.json` + 3 siblings on disk; this remains the
  strongest resolution discipline of the six (no separate atos pass that can silently
  fail).
- **§2.2 hot-leaf table (`find_component_delim` 58.41%/65.05%) — ACCEPT.** Backed by the
  sidecar + sample counts (27180/18955); symbol + file:line present.
- **§2.1b instr/byte authoritative cost surface — ACCEPT.** The V1 REVISE is folded:
  the `ri_cycles` c/B column is struck-through (`~~50.70~~`) and explicitly marked
  UNRELIABLE; instr/byte (§2.1b) is the authoritative surface, sourced from
  `css_canon_pmu.txt` (on disk). This is the correct V1-disposition fold for the
  reliability question.
- **§1 banner / §2.1 caption "falsified … corroborated by P1-D §3" — REJECT.** Per §1:
  P1-A cites P1-D §3 as the falsifier; P1-D §3 (V2) disavows the falsification. The
  cross-cite is broken and the "falsified" interpretation is most likely the wrong one.
  Fix: re-point to the resolved single posture (§1 fix); strike the "physically
  impossible / P1-D §3 corroborates" framing.

### p1b-samply-mode-2.md

- **§2.4 N>=50 harness (`css_canon_bench.rs`, assert :250) — ACCEPT.** `p1b:239` claims
  `assert!(n >= 50)` at `:250`; VERIFIED at `css_canon_bench.rs:250`. The §2.1 table is
  attributed to `css_canon_n200.txt` (on disk); the harness is DEFINED, asserts, builds.
- **§2.3 hot leaves + `:103-105` wrapper cite — ACCEPT.** `p1b:209` cites the
  `track1_full_parse` wrapper at `css_canon_bench.rs:103-105`; VERIFIED (`fn
  track1_full_parse` is at `:103`). P1-B is the ONLY artefact that cites this wrapper
  line correctly (cf. the p1e/p1f REVISE below). atos-resolved leaves; 16007/12947/22756
  sample counts cited.
- **§2.1 instr/byte column — ACCEPT.** cycles/byte struck; instr/byte from
  `css_canon_pmu.txt` is the reported density. Correct reliability fold.
- **§"c/B PROVENANCE" §39-45 / §3 §295-304 / §4.6 / §5 §422-425 "reference-clock tick,
  falsified by P1-F §2.2 / P1-D §3" — REJECT.** Same broken cross-cite as P1-A: P1-B
  names P1-D §3 as a falsifier that disavows the falsification. Fix per §1.

### p1c-samply-mode-3.md

- **§1.2 N>=50 gate (`css_canon_bench.rs:250`) — ACCEPT.** `p1c:80-82` explicitly folds
  the V1 REVISE: "the V1 `css_cold_bench` only *commented* the floor; this is the CH6/
  §1.2-V1 REVISE fold — the contract is now code-enforced." VERIFIED: P1-C now cites
  `css_canon_bench.rs:250` (the asserting harness), and abandons its V1 `css_cold_bench.rs`
  (which lacked the assert). The V1 P1-C REVISE is resolved.
- **§2.3/§2.4 hot leaves + on-disk flame — ACCEPT.** `/tmp/skv17-p1c-v2/{fact,full}.json.gz`
  (525381/603065 B) on disk; 70031/84966 leaf samples; `canon.txt` (7616 B) backs the
  §2.1/§2.2/§2.5 tables. The syslib resource-bucketing honesty caveat (`p1c:137-139`:
  per-symbol atos on syslib "is NOT claimed; the resource bucketing IS reliable") is
  retained — exemplary CH6 honesty.
- **§2.4 line re-attribution (`:295` membership, `:298` dispatch, `:294` load,
  `:307` advance) — ACCEPT.** Consistent with the atos resolution and with p1f §1.3's
  grep-verified source lines.
- **§"V2 c/B posture" §37-45 / §2.5 "ri_cycles falsified, P1-D/P1-F concur" — REJECT.**
  `p1c:42` asserts "P1-D/P1-F concur" on the falsification; P1-D V2 does not concur — it
  reverses. Same broken cross-cite. Fix per §1.

### p1d-pmu-cycles.md

- **§2.4 per-line self-time table — ACCEPT (V1 REJECT CLOSED).** The V1 paper-close
  (0-byte `atos_out.txt` under precise per-line %s) is resolved: `/tmp/skv17-p1d/atos_v2.txt`
  is 199 resolved `<symbol> (file:line)` lines on disk, every per-line % in §2.4 traces
  to a resolved address (`:298` dispatch, `:295` membership, `:307` advance, etc.).
  Backed.
- **§3.2 PMU table (cyc/byte + instr/byte + IPC) — ACCEPT (V1 REJECT CLOSED).** The V1
  paper-close (PMU agent delivered zero PMU counters; c/B was a 4.0 GHz wall estimate) is
  resolved: `/tmp/skv17-p1d-v2-pmu.txt` carries real `ri_cycles`+`ri_instructions`
  deltas, and §3.2 is byte-faithful (bootstrap track1_full cyc=6737349921 ins=25010386205
  → 14.47 c/B / 53.72 i/B / IPC 3.71). Real counters, no nominal-clock substitute.
- **§2.1/§2.2 cold tables — ACCEPT.** Byte-faithful to `/tmp/skv17-p1d-v2-cold64.txt`
  and `-run2.txt`; N=64 (>=50), median/min/max/stddev present.
- **§3.1 "the V1 falsification is itself incorrect … this supersedes the ri_cycles
  unreliable line P1-A/P1-B/P1-F carried" — REVISE (the correct read, unilaterally
  declared).** P1-D's interpretation is the defensible one (IPC physics + xctrace
  cross-validation back it). The DEFECT is that P1-D declares the supersede unilaterally
  inside its own artefact while the four siblings still carry — and cross-cite P1-D for —
  the opposite. P1-D cannot "supersede P1-A/P1-B/P1-F" by assertion; the pass-level
  posture is contradictory until the orchestrator propagates one reading to all six
  (§1). Fix: P1-D's reading should become the pass posture in V3, AND P1-A/B/C/F must be
  rewritten to match and their P1-D cross-cites repaired. As shipped, P1-D's §3.1 self-
  supersede is an orphan that the siblings contradict.

### p1e-hot-leaf-attribution.md

- **§1.1 / §5 css_canon_bench.rs verification — ACCEPT (V1 REJECT CLOSED).** The V1
  REJECT (claimed "verified … 303 lines … assert :150 … sample() :84-116", all wrong) is
  resolved: P1-E now states `css_canon_bench.rs` is "**403 lines**" (`p1e:30`, VERIFIED),
  assert `:250` (`p1e:47`, VERIFIED), `sample()` `:146` (`p1e:41`, VERIFIED), and openly
  flags the fix ("fixing the V1 paper-citation that misnamed line numbers", `p1e:39`).
  The fabricated-precision verification is corrected against the actual file.
- **§2.3/§2.4 hot leaves + on-disk flame — ACCEPT.** `/tmp/skv17-p1e/{full_parse,
  fact_stream}.json.gz` (125230/389687 B) on disk; symbol + %self + file:line per cell;
  `caller.py` walk backs the `emit_fact_stream` attribution. N=100 table backed by
  `/tmp/skv17-p1e-canon-n100.txt`.
- **§2.2 `track1_full_parse` wrapper cited at `bin/css_canon_bench.rs:45` — REVISE.**
  `p1e:182` cites the harness wrapper frame at `:45`. The actual `fn track1_full_parse`
  is at `css_canon_bench.rs:103` (grep-verified: `103: fn track1_full_parse`); `:45` is
  not that function (and `:15` is only the docstring mention). This is a fabricated-
  precision line cite of the same class as the V1 P1-E REJECT, on a different symbol — a
  precise number whose on-disk artefact does not back it. Fix: cite `:103` (as P1-B
  correctly does). Lower severity than the V1 REJECT because the symbol name + %self are
  correct and on-disk-backed; only the line is wrong.

### p1f-bench-canonical.md

- **§1.1 / §1.1.1 N>=50 harness + X2 verdict (assert :250) — ACCEPT.** Line cite CORRECT
  (`:250`); the X2 single-canonical-harness verdict is the cleanest of the six and names
  `css_canon_bench` with correct lines. The harness builds, asserts, carries PMU +
  profile modes.
- **§2.1/§2.1.1 N=200 cold + ratio-stability — ACCEPT.** Backed by `css_canon_n200.txt`
  + `css_canon_n200_v2.txt` (both on disk); the two-run ratio-stability demonstration is
  a genuine X2 comparability proof.
- **§2.3 lightningcss full-CSSOM attribution — ACCEPT.** Real symbol table proving the
  comparator materializes (cssparser tokenizer ~38% + typed Property/Selector build/drop
  ~30%); `lightningcss.json.gz` (13583 samples) on disk. Discharges the fair-bar bar.
- **§2.3 `track1_full_parse` wrapper cited at `css_canon_bench.rs:43` — REVISE.**
  `p1f:317` cites the wrapper at `:43`; the function is at `:103` (grep-verified). Same
  class as the p1e REVISE. Fix: cite `:103`.
- **§2.2 / §2.2.1 / §4.5 "ri_cycles falsified, CPI<1.0 physically impossible, reference-
  clock tick" — REJECT.** `p1f:299-301` asserts "A retired-instruction CPI below 1.0 is
  physically impossible on M5 (it would require retiring >3.5 instructions per core cycle
  sustained, exceeding the decode/retire width)." This is the claim P1-D V2 directly
  refutes with the M5's ~8-wide core, and the IPC math (3.5-6.4) does not exceed an
  8-wide retire width. P1-F is most likely the wrong physics, and it is the artefact the
  others cite as the "P1-F §2.2 falsifies it" authority — so its error propagates. Fix
  per §1: re-derive to the resolved single posture; strike the "physically impossible"
  assertion.

## §3 — Counts + disposition summary

Itemized cells dispositioned: **24** (per-artefact ACCEPT clusters + the itemized REVISE/
REJECT cells + the cross-artefact §1 finding).

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| p1a | 3 | 0 | 1 |
| p1b | 3 | 0 | 1 |
| p1c | 3 | 0 | 1 |
| p1d | 3 | 1 | 0 |
| p1e | 2 | 1 | 0 |
| p1f | 3 | 1 | 1 |
| cross (§1) | 0 | 0 | 1 |
| **total** | **17** | **3** | **5** |

- ACCEPT: 17
- REVISE: 3
- REJECT: 5
- ACCEPT rate: 17 / 25 = **68.0%** (below the §3Z >=95% convergence bar; V2 does not
  converge on CH6).

**The five REJECTs all reduce to ONE root defect (the c/B interpretation split) + its
four propagation sites:**
1. cross-§1 — the pass advertises "ONE c/B posture" but ships two mutually exclusive
   interpretations of the same `ri_cycles` measurement; the root REJECT.
2. `p1a §1/§2.1` — carries the FALSIFIED interpretation; cites P1-D §3 (which disavows it).
3. `p1b §c/B-PROVENANCE/§3/§5` — same; cites P1-D §3 / P1-F §2.2.
4. `p1c §V2-posture/§2.5` — same; asserts "P1-D/P1-F concur" (they do not).
5. `p1f §2.2/§2.2.1` — the "CPI<1.0 physically impossible" claim that is most likely the
   wrong physics and is cited by the other three as authority.

**The three REVISEs:**
1. `p1d §3.1` — the correct c/B reading, but unilaterally self-declared as superseding
   siblings that still contradict it (orphan until propagated).
2. `p1e §2.2` — `track1_full_parse` wrapper cited at `:45`; actual `:103`.
3. `p1f §2.3` — same wrapper cited at `:43`; actual `:103`.

**CH6 posture (V2 vs V1).** The two V1 gross paper-closes are CLOSED with real
on-disk artefacts: P1-D's 0-byte `atos_out.txt` is now `atos_v2.txt` (199 resolved
lines), P1-D's missing PMU is now real `ri_cycles`+`ri_instructions` counters, P1-E's
fabricated harness verification is corrected (403 lines / assert :250), and the N>=50
gate is universally code-enforced on the ONE canonical harness `css_canon_bench`. CH6's
gross-existence checks all pass — the agents did the work and the work is on disk. The
V2 defect is subtler and is the exact CH6 failure CH6 V1 anticipated would resurface: a
**measured number (cyc/byte) carried at face value with no single defensible
interpretation** — here because the pass's own re-interpretation (P1-D V2) was applied to
one artefact and not propagated, leaving four siblings citing the disavowing artefact as
their falsifying authority. None of the five REJECTs is a hard BLOCK: the underlying
counters are real and reproducible, the instr/byte column (the actual S-P2 cost input) is
sound and identical across all six, and the fix is editorial — resolve one reading
(P1-D's, per the physics + xctrace cross-validation), propagate it to all six, repair the
broken cross-cites, and correct the two `:43`/`:45` wrapper lines to `:103`. All five
fold into V3.
