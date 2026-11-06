# Archaeological Dig — Tranche AW ("The Activation") and its five-part balloon

Scope: `docs/tranches/AW/` — 91 files, ~28 K lines of tranche prose, 102 git-tracked
commits to the directory. AW is the single largest tranche directory in the pre-restart
corpus and the terminal act of the original campaign's performance arc. It is where the
DTA (Deterministic Tape Automaton) architecture was activated, measured, and — after five
sub-tranches and roughly a thousand commits of effort — proven to be the wrong machine.
The dig below establishes what AW attempted, what shipped versus what died, the root
causes of the divergence, the work worth resurrecting, and the meta-process pathologies
that AW both inherited and diagnosed.

---

## 1. What AW set out to do

AW is "the Activation" tranche. Its premise is stated flatly at `AW.md:30-40`: the
predecessor tranche AV had emitted every emission channel — `pub const DTA_TABLE`,
`GRAMMAR_PROFILE`, `SHAPE_DICT`, `BBNF_SHAPE_DICT`, the `PayloadStream` + Stage-C finaliser
API, the `TapeKind::ShapeRef` cursor expansion, the visitor reordered-unrolling codegen —
"but every one of those constants is consulted by no consumer. The runtime parse path
remains the AU-era recursive `fn __<rule>` descent." AW's thesis: *delete the legacy
recursive descent in its entirety and light the substrates* (`AW.md:112-116`).

The honest opening state was a regression. AV's substrate work had cost 2.5–4.5×
throughput across every bench versus the post-AU baseline (`AW.md:44-53`; matrix at
`AW.md:84-102`). The regression was framed as substrate "carrying weight without yet doing
the work that substrate enables" (`AW.md:47-49`). AW's job was to reverse it and clear the
post-AU floor by activating the DTA driver, the PSI runtime, ShapeRef dispatch, PHF + SIMD
keyword classifiers, and bloom + GADT runtime dedup, with bench checkpoints between every
wave (`AW.md:50-53`).

Two architectural commitments were declared non-negotiable at plan time:

- **SoA stays and DTA activates.** "AW keeps SoA. AW activates DTA. Both are
  non-negotiable" (`AW.md:157`). The SoA column layout's per-push cost was argued to
  amortise once the DTA drives batch column appends (`AW.md:121-157`).
- **No legacy code, no fallbacks.** "AW *deletes* the recursive fn-per-rule path in W1.
  There is one parse path post-W1" (`AW.md:184-188`).

The original plan was six waves (W0 cleanup → W1 DTA activation → … → W6 bench close) with
a hard throughput gate: strict-better-than post-AU on ≥ 15 of 19 bench entries
(`AW.md:17-18`).

---

## 2. The balloon — one tranche became AW-I…V (plus AW-VI/AX hand-off)

AW never executed as one tranche. It fractured under repeated scope-reveals into a chain —
**AW-I → AW-II → AW-III → AW-IV → AW-V**, each a full six-wave tranche with its own FINAL,
each handing an unmet throughput gate forward. The split is documented in the AW.md header
block (`AW.md:1-28`) and the successor-chain rationale at `FINAL.md:328-350`.

| Sub-tranche | Charter | Close state (tests) | Throughput gate |
|---|---|---|---|
| AW-I | W0 cleanup + walker completion, legacy `fn __<rule>` deletion, `parse()` swap, fuse/inline | (green) | deferred |
| AW-II | Lowering migration off fn-per-rule assumptions onto DTA Seq-wrapped layer | 1050 / 50 / 67 | not measured; viability question raised (`FINAL.md:14`) |
| AW-III | Fused correctness + architectural transposition (flatten DTA into tape automaton) | 1469 / 0 / 54 | **MISS — 0/17 parse entries** (`FINAL-III.md:57`) |
| AW-IV | Interpreter abrogation (emit hot logic into per-grammar walker) | 1412 / 0 / 36 | **MISS — 0/17, geomean 0.071 (~7% of AU)** (`FINAL.md:379,397`) |
| AW-V | Compile DTA/PSI into hot-path shape emitter | 1597 / 0 / 36 | **MISS — 0/17, geomean 0.082** (`FINAL.md:430`) |

The cross-tranche synthesis counts the whole W→AW-V arc as **~1500 commits across 24
tranches** rebuilding **~19 substrates** on adjacent foundations
(`audit/cross-tranche-learnings.md:1-5`). The AW-V reckoning names the pathology
outright: "The 1000-commit anti-pattern is real and present"
(`research/SYNTHESIS-5-AW-V-RECKONING.md:108`).

### The "0/17 not 0/19" correction, established

The SK-V20 verifier's correction resolves cleanly against the AW artefacts. The original
gate was framed against a **19-entry matrix** (`AW.md:17`, `AW.md:84-102`). That matrix is
**17 parse entries + 2 format entries**: 5 `json_monolithic` (canada, twitter, data_s,
data_xl, citm) + 3 `css_l4` (bootstrap, normalize, tailwind) + 3 `google_sheets` + 6
`bbnf_monolithic` = 17 parse, plus 2 format benches = 19. Every close-out that reached the
bench measured the parse subset and reported **0 of 17**, with the 2 format entries "within
noise":

- AW-III gate 12: "strict-better than post-AU on ≥ 15/19 … 0 of 17 parse entries; 2 format
  within noise" (`FINAL-III.md:57`).
- AW-IV W6: "**0/17 parse entries exceed post-AU**" (`FINAL.md:379`).
- AW-V W6: "17 parse benches + 2 format benches … **0/17 exceed post-AU**" (`FINAL.md:430`,
  `FINAL.md:460`).

The gate *ballooned to 17 measurable entries* only after AW-II blocked 5 of the 19 behind
"Cluster A" parse failures (measuring 14/19, `FINAL.md:149`, `AW.md`/AW-II gate 10 at
`FINAL.md:232`); those parse failures healed by AW-III, restoring the full 17-parse
measurement. So the honest reading is: **0/17 across three consecutive close-outs
(AW-III, AW-IV, AW-V)**, never 0/19. Any "0/19" figure conflates the gate's framing
denominator with the parse-measurable numerator.

---

## 3. What actually shipped vs what died on the vine

### Shipped and verifiable (the substrate did land)

- **The architectural transposition is structurally real.** By AW-III close,
  `dispatch_one` (the interpreter's per-byte tagged-union dispatch) is at 0% self-time on
  JSON twitter and `nm`-absent from the bench binary; stage-1 SIMD is active at 12.13%
  self-time; `push_compound_fused` replaces `reserve_compound` everywhere; the
  bbnf-simd-scan crate ships NEON + AVX2 + AVX-512 + WASM + scalar kernels
  (`FINAL-III.md:28-40`, gates 6/7/9 at `FINAL-III.md:51-54`).
- **AW-IV abrogated the interpreter into the walker** — 15 interpreter symbols
  (`dispatch_one`, `RegexScanner`, `try_branch`, `compute_f64`, `scan_quoted_string_simd`,
  etc.) confirmed 0-occurrence in the JSON bench binary (`FINAL.md:381-383`).
- **AW-V's shape classifier landed**: an IR pass + 12-variant `ShapeTag`, 12 detectors and
  12 functional emitters, CSS L4 coverage 86.1%, Sheets 80.6%, BBNF 78%
  (`FINAL.md:435`). This is the one structural survivor the successor plan keeps.
- **Parity harnesses landed and held**: sonic-rs 5/5, lightningcss 4/4 (including
  color-channel sub-test), CI-gated (`FINAL.md:438`).
- **Bootstrap idempotency held** across all sub-tranches — byte-identical `generated.rs` on
  consecutive clean-cache regens (`FINAL.md:139`, `FINAL-III.md:24`).
- **The GRAMMAR_PROFILE wire-contract** is the one substrate that consistently crossed
  mine→emit→read correctly (`audit/cross-tranche-learnings.md:19`).

### The decisive thing that shipped — and killed the architecture

- **AW-V.W2.1: a hand-written recursive-descent JSON prototype that beats sonic-rs.**
  `crates/bbnf-json-prototype/` — 2,246 LOC of recursive descent with inline SIMD kernels +
  a single monomorphic visitor, **no interpreter, no tagged-union state machine**. It hit
  JSON twitter **2577 MB/s (1.31× post-AU, 0.89× sonic-rs)** and beats sonic-rs 5/5 on
  every JSON entry (0.89–0.94× sonic ns/iter); `nm` is clean of every DTA symbol; samply
  shows 91.15% self-time on one monomorphised `parse_value::<ValueVisitor>`
  (`research/SYNTHESIS-5-AW-V-RECKONING.md:9-13`, `FINAL.md:434`). This prototype is the
  empirical proof that the answer was fn-per-rule RD all along — and that the entire DTA
  edifice was a detour.

### Died on the vine

- **The DTA interpreter itself.** Queued for wholesale deletion — `~12 K LOC of
  hot-reachable machinery` in Tier 1 alone, `~74 K LOC total` once generated.rs walker
  output regenerates (`audit/dead-code-manifest.md:5,79`). Breakdown:
  `dta_walker/` 4,360 LOC, `driver.rs` interpreter carve ~2,873 LOC, `dta.rs` 550,
  `emitter/dta.rs` 935, IR lifter path ~813, `bbnf-tape-codegen/` entire crate 672
  (zero external consumers, `dead-code-manifest.md:16`).
- **The stage-1 SIMD structural pre-pass** — tried and reverted six times across the arc
  (AO.0.4 first attempt `7198c974`; AQ.5 deleted ~1500 LOC `2f7c1bd4`; re-landed
  driver-consumed at AW-III.W5.d `91df0809` where twitter regressed 192→170 MB/s;
  `research/SYNTHESIS-5-AW-V-RECKONING.md:21,32`). A *universal* pre-pass never once beat
  the no-pre-pass path.
- **Lever 4 `push_compound_fused_v32`** — a self-aliased `vld1q_u8 → vst1q_u8` load/store
  tautology that LLVM MemorySSA elides; zero emitter-side consumer; the W1.3 landing
  ledger itself acknowledged the pathology (`SYNTHESIS-5:24,36`,
  `dead-code-manifest.md:18`).
- **The "17-digit NEON float lever"** — an overfit *projection* in `AW-V.md:684,692` that
  was never implemented in-tree (`SYNTHESIS-5:37`).
- **Substrate slots emitted `&[]` for all four grammars**: `active_columns`,
  `branch_priors`, `reorder_unroll_visitors` — projection wired, IR miner never shipped
  (`dead-code-manifest.md:25-27`). AV.2.5's visitor reordered-unrolling kernels landed with
  no `Tape::reduce_column` consumer ever written (`audit/SYNTHESIS.md:52`).

---

## 4. Root causes of the divergence

### 4.1 The DTA is not the architecture it claimed to be

The load-bearing diagnosis is in `research/arch-comparison.md`. simdjson and sonic-rs are a
two-stage "SIMD structural scan + tape walker" family; the DTA branded itself as the same
family but "**is NOT that architecture. It's a byte-at-a-time state-machine interpreter
that happens to emit SoA columns**" (`arch-comparison.md:14-16`). The real bottlenecks
named: no structural pre-pass, unfused multi-column writes, and **interpreted tagged-union
dispatch per byte** (`arch-comparison.md:18-20`).

The reason it could not be compiled away into fast code is the R3 finding: **the DTA's 14
state-variant bodies are non-uniform (40–400 LOC each)**, whereas bbnf-regex DFAs compile
because their state bodies are uniform (~20–40 bytes of `match b` per state). "The 154 KB
walker pathology is non-uniform-body-size, not LLVM inline budget"
(`SYNTHESIS-5:23`). Automata compile when states are uniform; the DTA's states are not, so
splicing `advance_or_pop_with`'s 400-LOC body per arm was structurally rejected by LLVM
(`FINAL-IV.md` carry-forward, `SYNTHESIS-5:33,83`). This is the terminal architectural
root cause: the machine was chosen for generality and could never be made fast because its
per-state work is heterogeneous.

### 4.2 Substrate-without-consumer — the #1 chronic pattern

The retrospective synthesis of the twelve tranches AK→AV (`audit/SYNTHESIS.md`) identifies
**"substrate-without-activation" as the #1 chronic anti-pattern**: "Emission lands;
consumer doesn't; hard gate closes on 'code exists'; runtime never fires"
(`audit/SYNTHESIS.md:27-29`). It cites a ten-tranche chronic — AK EmissionTier leftovers
(~2000 LOC demolished at AM), AM.2/AM.5 no-consumer buffers, AO's full structural-dispatch
chain landing at **0% parse impact**, AP.1's `structural_mode = false` gate-off
(~400 LOC deleted at AQ.5), AQ's Phase-6 typed-payload system with zero payload writes in
six grammars, AS.2.3 StructRegistry never populated, AT.1's `branch_pushes_children`
misclassifying every typed capture as a dead store, AV.2.5's visitor kernels with no
consumer API (`audit/SYNTHESIS.md:30-53`). AW is the terminal instance: the entire AW-I…V
arc *was* the attempt to finally wire the AV substrate to a consumer — and the consumer,
when finally built, exposed the substrate as the wrong machine.

The generalised lesson: the architectural constant that makes a substrate *stick* is the
pipeline **"mined IR fact → pluggable cost model → emitter selects body shape → consumer
reads the emitted const," all four hops closed in one wave**; when any hop is deferred, the
substrate rots (`audit/cross-tranche-learnings.md:22`, restated at 44-55).

### 4.3 Bench dishonesty and late measurement

AV shipped its **first bench at V10** — the 2.5–4.5× regression across every entry was
invisible until tranche close (`audit/SYNTHESIS.md:184-186`). Within AW-V, the W4 detector
widening that broke JSON's visitor path was hidden across three wave boundaries
(`cross-tranche-learnings.md:31,55`). The FINAL-V "compounding engagement" framing of the
gate miss was explicitly flagged as misleading by the overfit audit
(`cross-tranche-learnings.md:53`, citing `V-audit-overfit.md:63`). Corrective codified:
per-wave or aggregated bench checkpoints, silent omission is a violation
(`audit/SYNTHESIS.md:193-195`).

### 4.4 God-module balloon in generated code

`generated.rs` reached **82,929 lines** at AW-IV close (`FINAL.md:411`), **72% of which is
DTA walker output** (`dead-code-manifest.md:13`). The DTA emission machinery sprawled
across `dta_walker/` (5 files, 4,360 LOC), `driver.rs` (3,323 LOC), `emitter/dta.rs`,
`bbnf-tape-codegen/`, and the IR lifter. The scale is itself evidence of the divergence:
the deletion manifest reclaims ~74 K LOC by removing one architectural choice
(`dead-code-manifest.md:79`).

---

## 5. Worth resurrecting for the totality plan

These are the algorithms, measurements, and negative results AW paid for that the next plan
must inherit rather than re-learn.

1. **fn-per-rule recursive descent + monomorphic visitor + inline SIMD kernels is the
   winning architecture.** The `bbnf-json-prototype` (2,246 LOC) beats sonic-rs 0.89–0.94×
   on every JSON entry with 91.15% self-time on one monomorphised symbol
   (`SYNTHESIS-5:9-13`, `FINAL.md:434`). Keep the crate as the reference tape-value rep
   (`dead-code-manifest.md:55`).

2. **Shape classification as the generalisation mechanism.** The IR pass + 12-variant
   `ShapeTag` mechanically generalises fn-per-rule across grammars; R2 established that
   10/15 hand-tuning decisions are IR-derivable, 3 profile-derivable, 2 backend-layout
   (`SYNTHESIS-5:22`, `cross-tranche-learnings.md:17`). The shape-emitter is fn-per-rule
   with IR-selected body templates — call it what it is, not "DTA compilation"
   (`SYNTHESIS-5:56`).

3. **The four-hop substrate pipeline as an orchestration invariant**, not a per-tranche
   hope: mine → cost model → emit → consumer-reads-const, all in one wave, with an
   activation gate verified by runtime evidence (bench delta / `cargo expand` / samply /
   tape-walk), never source-grep (`audit/SYNTHESIS.md:54-59`,
   `cross-tranche-learnings.md:22`).

4. **Negative results that must not be re-attempted:**
   - A *universal* stage-1 SIMD pre-pass regresses — six independent attempts, six losses
     (`SYNTHESIS-5:21,81`). Per-compound opt-in only, gated on the grammar property
     `Regex("[^S]*") ∧ S ⊊ single_bytes ∧ ConsumeToNextStructural lifter exists`
     (`SYNTHESIS-5:32,92`).
   - `push_compound_fused_v32` is a tautology — replace with paired-column `stp`
     (`SYNTHESIS-5:36`).
   - Unbounded `inline_acyclic` without a leaf-predicate guardrail failed at
     `bfa50f25` (2026-04-08) — the cross-rule normalise cascade "architecturally cannot be
     expressed in one-pass saturation" (`SYNTHESIS-5:35`, `cross-tranche-learnings.md:13`).

5. **Correct SOTA facts** (three factual errors corrected by the AW-V reckoning): DTA/DFA
   state counts are JSON 51, **CSS L4 83 (not 800)**, BBNF bootstrap 496
   (`SYNTHESIS-5:23,104`). Small-input "per-invocation overhead" was a false claim: on a
   500-byte input setup is <5% of a 5μs parse; small-input cost is per-byte, not
   per-invocation (`arch-comparison.md:21-24`).

6. **Real SIMD kernels worth shipping** (R4): `vpaddq_u8` movemask cascade (3–4× over
   `vaddv_u8`), `vdotq_s32` canada-fraction packadd (1.5×), TBL-4 kinded bitmap that
   eliminates the walker byte-load; verify PMULL fires on M4 (`SYNTHESIS-5:24,46`).

7. **Document-parallel fork** — the one AW-IV bright spot: tailwind 16→37 MB/s (2.24×) via
   depth-0 brace partitioning + byte-balanced cuts + 1 MiB threshold (`FINAL.md:389`).
   Amortization multiplier, not an exceed-sonic lever — apply above a size threshold only.

8. **E-graph grammar rewriting** (R6-depart), deferred but clean: 8 algebraic rewrites
   collapse 779 LOC of hand-written detectors to ~150 LOC of tag-reading via extraction;
   substrate ready in `crates/egraph/`; guardrail is the leaf-predicate G5 (inline only when
   the body extracts to a terminal) (`SYNTHESIS-5:26,48`).

9. **Backend-type-tables, not a central StructRegistry**, for named-type projection (the
   Color view-layer accessor). The registry shape was wrong and deleted at AU.4.2; the
   projection requirement is mandatory (`audit/SYNTHESIS.md:22`, `AW.md:214-233`).

10. **Parity harnesses paid directly against competitors** (sonic-rs, lightningcss) are the
    correctness invariant that held throughout — keep them CI-gated
    (`FINAL.md:438`, `cross-tranche-learnings.md:18`).

---

## 6. Meta-process pathologies

AW's own audit corpus is the most valuable process artefact in the dig: twelve parallel
retrospective agents catalogued **ten recurring anti-patterns** across AK→AV
(`audit/SYNTHESIS.md:26-207`), each with a corrective mapped to `TRANCHE_SPEC.md`:

1. Substrate-without-activation (the #1 chronic; §4.2 above).
2. Silent deferrals — plan promises vanish without escape clause (AN five items, AP ~10
   sub-phases despite "No deferrals," AV V6–V9 cut "per user direction")
   (`audit/SYNTHESIS.md:61-83`).
3. Plan-only / ceremonial tranche docs — PROGRESS/FINAL missing, plan edited post-hoc to
   match what landed (AK plan authored *after* implementation; AM/AO/AP no
   PROGRESS/FINAL/bench) (`audit/SYNTHESIS.md:86-107`).
4. Label / document discipline violations — new work under old letters; mid-tranche pivots
   not promoted to a new letter (AR, AS) (`audit/SYNTHESIS.md:109-125`).
5. Gate-off commits — feature behind `false` default with no same-tranche flip (AP.1
   `structural_mode = false`) (`audit/SYNTHESIS.md:128-139`).
6. Hard-gate-via-grep — gate closes on source pattern while runtime never fires (AT.1 dead
   stores) (`audit/SYNTHESIS.md:142-154`).
7. Ceremonial wave structure — multi-wave × multi-agent plan, linear solo execution (AQ,
   AS) (`audit/SYNTHESIS.md:157-167`).
8. Commit-before-parallelise violations — duplicate/conflicting commits (AR)
   (`audit/SYNTHESIS.md:170-179`).
9. Bench omission — no intra-tranche bench, regressions invisible until close (AV V10)
   (`audit/SYNTHESIS.md:182-195`).
10. Research edict skipped — inheriting predecessor's research (AV's "April 2026"
    deliverables are AU-era) (`audit/SYNTHESIS.md:198-206`).

The **bright spot** the audit holds up as the template is **AU**: re-plan on scope-reveal
rather than defer; the samply-trio tooling (shared `CARGO_TARGET_DIR` + `wave.tsv` +
`profile-bench-headless.sh`); pre-wave friction fixes; dated PROGRESS at wave boundaries;
co-located profiling docs (`audit/SYNTHESIS.md:208-227`). The four traits shared by
*successfully-closed* waves: one named substrate + one named consumer in the same wave;
bench/samply attribution cited with artefact path; plan-time floor analysis on numeric
gates; re-plan-not-defer on scope reveal (`cross-tranche-learnings.md:42-47`).

**AW's own signature meta-pathology is the balloon.** A six-wave "activation" tranche
fractured into five full sub-tranches (AW-I…V) plus an AW-VI/AX hand-off, each closing on
an unmet throughput gate handed forward with a fresh viability question. The scope-reveal
cascade was *procedurally* clean each time (new letters opened, FINALs written, debt ledgers
carried), which is precisely why it was so expensive: the process discipline was mature
enough to keep chasing a dead architecture honestly for a thousand commits. The AW-V
reckoning's closing counsel is the meta-lesson for the totality plan: "commit to
fn-per-rule, commit to strike the two bad levers, commit to interpreter deletion, ship all
shapes in one wave or stop and re-plan. No more 'compile the DTA' narrative"
(`SYNTHESIS-5:109`).

---

## 7. Bottom line for the totality plan

AW is the empirical death certificate of the DTA/tape-automaton line and, in the same
breath, the empirical birth certificate of the winning line: **generated fn-per-rule
recursive descent, shape-classified from IR, monomorphic visitor, inline SIMD kernels,
document-parallel above a size threshold** — proven by `bbnf-json-prototype` to beat
sonic-rs on every JSON entry. The totality plan should treat the entire interpreted-automaton
family as settled-negative, resurrect the four-hop substrate-with-consumer invariant as a
hard orchestration gate, inherit the six named negative results verbatim, and never again
let procedural cleanliness sustain a wrong architecture across tranche boundaries.
