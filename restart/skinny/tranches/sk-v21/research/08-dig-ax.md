# Dig AX — The RD Reckoning: excavation and salvage

Tranche AX is the pre-restart campaign's penultimate substantive tranche (successor
chain `AX → AY → BA → BB → BC`). It opened 2026-04-18 against AW-V's 0/17 close and
closed 2026-04-20 at master `411eabfd`. It set out to be the "reckoning" that repaired
a regression, deleted an interpreter, and deployed a nineteen-wave optimisation
portfolio. It shipped the repair and the deletion; the entire optimisation arc died on
the vine and was reframed, at close, into a successor tranche. AX is the clearest
single case study in the corpus of the failure mode the restart was called to escape:
substrate accreting faster than consumers, benchmarks measured against a moving
baseline that hides absolute regression, and a value-API attempt that overfit to
JSON/CSS via hand-coded per-grammar duplicates.

Sources excavated exhaustively: `docs/tranches/AX/{AX.md, FINAL.md, PROGRESS.md}`;
`docs/tranches/AX/waves/W{0a,0a.close,0b,0c,1}.md`; `docs/tranches/AX/audit/{SYNTHESIS,
R1-waste-patterns,R2-instructions-redress,R3-infra-speedups,R4-plan-redress,W1r1-diag,
W1r3a-diag}.md`; and the twelve `audit/next-tranche/*.md` planning artefacts (00, 01,
02, 03, 05, 06, 07, 11, 12, A1–A10). Git cross-checks on the `9f8aed90..411eabfd`
window.

---

## (a) What was attempted — the charter and waves

AX declared itself an augmentation of "the 400-commit work, not a fork" (`AX.md:4`).
Six architectural propositions (`AX.md:9-16`): repair the regression before deleting the
interpreter; the interpreter is architectural debt (~78,500 LOC reclaim); the tape's
*access* API shapes the ceiling more than storage layout; novel levers compound only
atop a shared substrate with a demonstrable floor; parallelism is an amortisation
multiplier not a single-thread lever; parity *is* the generality claim. Twenty-one
invariants (`AX.md:23-45`) codified prior lessons — one codegen path, no new grammar
directives ever, no stubs, mid-wave bench-checkpoints, frozen gate predicates,
"architectural-transposition-complete-throughput-next-wave is not a closeable wave"
(invariant 17), "ledger-only wave = re-plan trigger" (invariant 13).

Nineteen waves (`AX.md:57-77`), two blocks:

- **Block A — correctness + API**: W0a (gate repair + non-Alt-rooted `parse()` routing +
  `gate_predicate_wire_contract.rs`), W0a.close (pre-W0b bench baseline), W0b (interpreter
  deletion + substrate-without-consumer purge + crate renames), W0c (AW-V doc rewrite),
  W1 (grammar-derived view surface + canonical parity).
- **Block B — optimisation portfolio (W4–W14)**: JSON/CSS SIMD levers, per-pattern
  `last_byte_set` narrowing + CTNS, LazyValue/gradient materialization, speculative
  parsing + Markov predictor, document-parallel fork, e-graph rewrites G1–G9, detector
  retirement, CPU autotune + PMC, multi-visitor pairs. W15 = FINAL.

A seven-condition AX→AY handoff contract (`AX.md:81-89`): zero DTA symbols, every
grammar routed through shape dispatcher, all 17 bench entries ≥ post-AU, gate predicates
*deleted* (not narrowed), all 9 e-graph rewrites active + 12 detectors deleted, zero
`#[ignore]`.

---

## (b) What shipped vs what died

**Executed under AX's letter (6 of 19 waves), git-verified in `9f8aed90..411eabfd`:**

| Wave | Close commit | What actually landed |
|------|--------------|----------------------|
| W0a | `6b03dd53` | Gate repair (`has_w4_classified` narrowed to `Pratt\|Unordered`, `9f8aed90`) + an **eight-sub-wave emitter cascade** (W0a.1 → W0a.2.r) that widened admission and fixed ~nine classes of shape-emission/payload defect. 6 semantic parity harnesses green; bootstrap idempotent at 98,270 lines. |
| W0a.close | `1241e7ac` | 18-entry bench baseline (`post-AX-W0a-close.json`) — the intra-AX attribution anchor. |
| W0b | `0adabb23` | Interpreter deletion + `simd-scan/emit` purge + crate renames (`bbnf-tape`→`tape` etc.) + test carve. |
| W0c | `db9c4e06` | AW-V.md rewritten "in RD language". |
| W1r | `ab7c218d` | Eight sub-waves (W1r.0–W1r.7): grammar-derived `NodeView`+`TapeCursor`+typed accessors, canonical-serialisation parity vs sonic-rs/lightningcss, `@pretty` directives, twitter lazy-field bench. |

**Died on the vine — the entire Block B optimisation arc (W2–W15).** `FINAL.md:16-23`,
`405-431`: "Block B did not execute under AX's letter." Every handoff condition except
"zero DTA symbols" was missed (`FINAL.md:404-431`): bench matrix does *not* exceed
post-AU (condition 4); gate predicates *narrowed*, not deleted (condition 5); zero of 9
e-graph rewrites active, zero detectors deleted (condition 6); five wire-contract test
files + two emitter-shape test files compile-*fail* at AX HEAD (`FINAL.md:410-416`), so
`cargo test --workspace` is not even green at close. The `ebnf_prettify` recognizer
fails at offset 0 on valid EBNF (`FINAL.md:417-420`). AX's FINAL claims 247 passed / 1
ignored (`FINAL.md:195`) — but that count is the *parity subset*, not the workspace,
which does not compile.

The A7 absorption audit (`A7-ax-unfinished-absorption.md:70-90`) then triaged all 14
dead waves: 6 FOLD, 2 NEW-AY, **3 RETIRE**, 1 AZ, 4 POST-AZ, 0 landed. The three retired
outright — JSON SIMD micro-kernels, CSS SIMD cluster, speculative parsing — were retired
because fresh profiling proved they target symbols that fire at ~0–12% self-time
(`A7:118-154`). Roughly a third of AX's declared plan was, in hindsight, aimed at the
wrong hotspot.

---

## (c) Root causes — where and how divergence began

### 1. The central regression AX never actually repaired: SoA 7-column write + wrap-compound emission

AX opened to "repair the regression" (`AX.md:11`) but repaired the *routing* gate, not
the *substrate* regression underneath it. The true chain, reconstructed at AX close
(`12-au-archaeology-beat-sonic.md:64-80`, `01-prior-tranche-archaeology.md:51-63`):

- AU close (`3b8b757`) held twitter at **0.615 bytes/cyc = 76% of sonic-rs**, citm at
  0.762 = 94% of sonic — via a flat AoS `Vec<TapeRec>` write path + unified
  `push_leaf_with(PayloadData)` + per-grammar push_fingerprint.
- AV/AW-I pivoted to a **SoA 7-column tape** (`kinds, flags, extra, span_lo, span_hi,
  sib_skip, child_off` — seven heterogeneous `Vec` pushes per structural record LLVM
  cannot fuse; `12-au:86-90`). Regression "masked as parse failures masked as bootstrap
  win" (`01-archaeology:56`).
- AW-V.W6's `has_w4_classified` over-admission stacked **wrap-compound emission on every
  scalar-leaf JSON value** — a second ~2× (`12-au:75`).

Net at AX close (`12-au:29-45`): twitter **0.137 bytes/cyc = 17% of sonic = a 4.5×
regression from AU**; canada −6.6×; sheets_stress −7.1×; every grammar 3–8× below AU.
Both regressing pivots "landed without perf gating. Parity tests pass; perf tests didn't
exist" (`12-au:80`). AX's W0a routing fix restored CSS/Sheets/BBNF 2.7–8.5× *relative to
the already-regressed AW-V baseline* — but left JSON at AW-V's level and never touched
the substrate. The reckoning repaired a symptom.

### 2. Bench dishonesty by baseline omission (doc/code inversion)

AX's own bench delta table (`FINAL.md:213-231`) compares against `post-AW-V.json` and
`post-AX-W0a-close.json` — **both already 4.5× below AU**. The absolute-vs-AU comparison
appears nowhere in AX's wave ledgers; it surfaces only in the A1–A10 audits authored *at
close*. `post-AU.json` exists on disk (`docs/benchmarks/post-AU.json`, confirmed) and
was available the entire tranche — AX chose the regressed baseline. The consequence:
every W0a close report reads as a win (CSS "8.5×", Sheets "2.7×") while the parser sat at
17–28% of its own historical high-water. Invariant 10 ("mid-wave bench-checkpoint") and
the operational posture's mid+close bench mandate (`AX.md:48`) were satisfied *in form*
(numbers captured) but *inverted in substance* (against a baseline that hides the
regression the tranche was named to reckon with). R1 confirms W0a produced **zero**
`post-AX-W0a*.json` files despite the mandate, and W0a.md hard-gate #1 (`json_monolithic_
value ≥ 0.98× prototype`) never ran (`R1:156-159`).

### 3. Walker-parity treated as the correctness oracle — ~70% wasted chase

The dominant waste pattern (`R1:5-13`, `SYNTHESIS:19-30`): W0a.2.d–W0a.2.h (five
sub-waves, ≈20 agent-hours) chased byte-for-byte equivalence between shape-emitter tape
and the *walker* tape — a scaffold artefact scheduled for deletion in the very next wave.
The W0a.2.h pivot (`AX.md:43`, invariant 20) retired walker-parity as oracle
retroactively, making shape emission tape-authoritative. R1's split: **~30%
fundamentally-latent (surfaceable only under contact); ~70% walker-parity chasing the
pivot erases** (`R1:143-144`, `SYNTHESIS:48-50`). Quantified waste (`SYNTHESIS:32-46`,
`R1:15-116`): ≥14 bootstrap regens at 5–10 min each; 4 independent unclassified-Ref BFS
probes producing the identical `43→0` observation; 4 verbatim copies of the 7×3
predicate table across diag docs; 5 re-reads of `shapes/mod.rs:331`; 6 touches on the
same gate predicate; 47 AX commits with zero runtime activation of the stipulated W0a
hard gate.

### 4. Sub-wave proliferation instead of new-letter escalation

W0a opened as "2-serial" (`waves/W0a.md:4`) and became **eight serial sub-waves**
(`W0a.1` → `W0a.2.r`), each handing a ~180-line diag restating the same decomposition to
the next (`R1:11-13`, `R1:99-110`). This is precisely the SPEC violation invariants 12
and 17 forbid — "mid-tranche pivots open a new letter," with AR/AS named as the
anti-example the sub-wave chain "has exceeded" (`R1:218-229`). Every W0a.2.{d,e,f,g}
close ended with a "re-plan suggestion for the next sub-wave" — a direct invariant-17
violation (`R1:165-168`). R1's verdict on the invariant regime: **aspirational prose,
not enforcement at sub-wave close** — close ledgers self-report "Met under narrow
predicate" rather than cite bench artefacts (`R1:169-172`).

### 5. The build wall — 26 GB RSS OOM (build-infra-first violated)

W0a.2.d wired the `shapes/inline.rs` inline helpers into all 5 derive-Parser sites; the
aggregate `tape_parity` test binary linked five derive expansions + inline Alt positions
+ `debuginfo=2` into one translation unit and **peaked at 26 GB RSS in a single rustc
process before a pre-OOM kill** (`PROGRESS.md:131-137`, `R1:79-84`, `R3:67`). `cargo
clean` reclaimed 17.4 GB. The per-grammar `tape_parity` split that resolved it
(`8048fb41`) landed *after* the OOM — a build-infra-first inversion the R1 audit calls
out explicitly (`R1:82-95`, redress `R1:187-194`). This wall directly forced the memory
discipline (`CARGO_BUILD_JOBS=4`, worktree `target/` symlink) and the `ax-iter` profile
(see (d)/ax-iter below).

### 6. Substrate-without-consumer — the chronic anti-pattern, AX day one

`shapes/inline.rs` shipped 1239 lines of substrate at W0a.2.d with **consumer wiring
reverted under contact** (`PROGRESS.md:121-149`, `R1:66-72`) — "the SYNTHESIS §1
substrate-without-consumer anti-pattern, AX day one." The pattern is 11-tranche chronic
(`01-archaeology:54-63`): the Named-projection chain spanned AS.2.3 → AU.4.2 → AW.0.5 →
AW-III.W6.4 → AW-IV.W3.5a → AX.W1r.1 across **five tranches with zero runtime consumers**
(`01-archaeology:14`), ending in W1r.1's discovery that `resolve_named_type` returns
`None` on every grammar. simd-scan/structural-scan, CTNS, ShapeRef, and Lever-4
(`push_compound_fused_v32`) are all the same shape: substrate landed, consumer never
wired (`A9:65-108`, `A8:363-393`, `01-archaeology:57`).

### 7. Two in-tranche reverts from out-of-scope architectural calls

- **W0a.2.k** (`015d02af`.. reverted `f585ce37`..): an agent, fixing Pratt LUT coverage,
  made an out-of-scope call to **flatten the Pratt tape and remove reducer-compound
  emission**, breaking `css_l4_parity` 11/16 and `sheets_parity` 14/25
  (`PROGRESS.md:352-381`, `A9:276-300`). Revert-to-green + W0a.2.l redid the correct
  parts (per-rule LUT + 1-byte arena API) without the tape change.
- **W1.A/W1.B**: hand-coded `bbnf::json::Value` iso sonic_rs and `bbnf::css::StyleSheet`
  iso lightningcss with **22 `TypeOnly` stubs** — violating invariants 4 (no new
  directives), 11 (no per-grammar prototypes), 18 (no placeholder surfaces). Reverted
  −6,128 LOC at W1r.0 (`3429aaba`); invariant 21 (grammar-derived view surface) added in
  response (`FINAL.md:125-134`, `01-archaeology:44`). This is the JSON/CSS-overfitting
  divergence — hand-coded per-grammar value duplicates — reproduced *inside the very
  tranche meant to reckon with it*, and caught only by revert.

### 8. Honest close, at the cost of admitting the plan was mostly unexecutable

The redeeming counter-current: rather than ship silent deferral, AX ran ten
fresh-attribution audits (A1–A10) at close, measured the 4.5× regression honestly, and
reframed Block B wholesale into AY under SPEC §Scope-reveal "new letter" response
(`FINAL.md:371-403`). The invariant-by-invariant reconciliation (`FINAL.md:247-333`) is a
genuine close-out discipline. But the honesty is retrospective: it names, at close, a
regression that a mid-wave bench against post-AU would have surfaced at W0a.

---

## The ~78,500 LOC reclaim — audited honestly

The headline "~78,500 LOC total reclaim" (`AX.md:12`, `W0b.md:5`) is **honest in
aggregate but conflates machine-generated output with maintained source**. Git-measured
breakdown of the W0b commit series:

| Component | LOC | Kind |
|-----------|----:|------|
| `generated.rs` walker output evaporation (regen `bc550d2ce`) | **68,438 net shrink** (98,270 → 29,832; charter claims 57,481) | **machine-generated** — reclaimed by *not emitting*, not by deleting maintained code |
| `dta_walker/` + `emitter/dta.rs` (`a206b9622`) | 5,302 | source |
| tape/ir surgical carves + 7 profile slots + Lever 4 (`b7aa41c02`) | 5,225 | source |
| `bbnf-tape-codegen` + csp_strategy aliases (`51e77d015`) | 822 | source |
| `simd-scan/emit/` purge (`e4121fdca`) | 1,548 | source |
| 8 DTA-coupled test suites (`e839378cd`) | 1,489 | test |
| `tape_parity_*` walker oracles (`0d730c8fd`) | 565 | test |
| W0a diagnostic probe harnesses (`6854f18b7`) | 860 | test |
| cleanup: walker_arms/aq5/tape_basic/etc (`0adabb23`) | 2,735 | test |
| **Genuine hand-maintained source + test reclaim** | **≈18,546** | |

The whole-window `git diff --shortstat 9f8aed90..411eabfd` = 271 files, +40,159 −96,866.
So **~73% of the celebrated 78,500 is generated `generated.rs` walker output** that any
regen would drop automatically — the interpreter *source* itself is ~12,000 LOC (as the
task brief notes), and genuine hand-maintained deletion is ~18,500 LOC. Worse, the figure
**drifted upward in later audits**: A7 restates it as "~85 kLOC" (`A7:17`), an over-claim
against both the 78,500 charter figure and the ~76,012 git-measured W0b deletion. Any
totality-plan reclaim accounting must separate generated-output-that-evaporates from
maintained-code-deleted, and must not inherit the inflated 85k.

---

## The ax-iter profile — born here

The `[profile.ax-iter]` the SK-V21 charter references originates in AX's R3 infrastructure
wave (`R3:67-79`, commit `62d40d906`), authored as a **direct response to the 26 GB OOM**:
`inherits = "dev"`, `debug = 0`, `split-debuginfo = "off"`, `strip = "debuginfo"` — peak
rustc RSS falls ~3× and link time ~2× on aggregate binaries, `dev` stays default so
samply binaries keep DWARF (`R3:69-79`). It was later consolidated to a single source of
truth in `.cargo/config.toml` (commit `b1b34f2c3`, per the current `Cargo.toml:136-139`
comment attributing consolidation to AZ-III.W0p.2; the block now reads `debug =
"line-tables-only"`, `codegen-units = 256`, `incremental = true`, `opt-level = 0`). The
companion R3 scripts (`test-tier.sh {leaf,grammar,workspace}`, `worktree-status.sh`,
`kill-all-rust.sh`, `seed-worktree.sh --target`) also date from here (`R3:90-97`).

---

## (d) Deferred / abandoned work WORTH RESURRECTING

### D1. The AU load-bearing primitives — the restoration target (highest value)

`12-au-archaeology-beat-sonic.md:82-114` names exactly what to restore to recover the
4.5×:

- **Flat AoS `Vec<TapeRec>` primary** + optional `PackedRecord` 32-byte sidecar read-cache
  (W1.D landed the sidecar). Collapse the 7-column SoA. One 16-byte store per push, not
  seven heterogeneous Vec pushes (`12-au:84-90`).
- **Unified `push_leaf_with(kind, PayloadData)`** — the 5-variant enum `{None,
  InlineScalar(u32), WideScalar(u64), Aggregate(&[u8]), Bytes(&[u8])}` (`12-au:92-104`)
  that collapsed 10+ `push_leaf_with_*` methods. It still *exists* at
  `crates/tape/src/builder.rs:275` but now delegates to the slow SoA push; restoring AoS
  primary makes the entry point fast again.
- **Per-grammar push_fingerprint** — `compute_push_fingerprint` IR pass →
  `Vec::with_capacity(len * numer / denom)` at parse entry (JSON canada +18%, BBNF
  json.bbnf +49%; `12-au:106-110`). Verify `Tape::with_capacity` threads it to *all* Vec
  allocations post-revert.

### D2. json-prototype — the speed-ceiling oracle (do not delete prematurely)

`crates/json-prototype/` (2,246 LOC, AW-V.W2.1) is a hand-tuned JSON parser hitting
**0.89–0.94× of sonic-rs on all 5 fixtures** (`12-au:116-128`, `01-archaeology:42`),
`nm`-verified free of walker residue. Its shape (`12-au:130-136`): single `parse_json`
over a user visitor; **five `#[inline(always)]` shape functions monomorphised at the call
site**; inline Eisel-Lemire via `compute_f64`; borrow-safe no-escape string leaves. It is
the standing proof that the grammar substrate *can* reach sonic parity — the regression
is entirely emitter-shape + tape-layer, not algorithmic. The resurrection lever
(`12-au:138-146`): grammar-*derive* this shape from IR's TypeDesc + shape classification;
`<Grammar>Value` collapse + 5 inline fns per grammar + `parsed.to_value()` inlining the
whole tree-build. Target trajectory (`12-au:148-159`): AoS revert → ~0.45; wrap-elision
(G3) cutting record count 50% to match sonic node count → ~0.85; per-shape inline →
~1.00; SIMD unescape + Eisel-Lemire direct-column → **1.15–1.40× = BEAT sonic**. A8
recommends retaining the crate as archival comparator until `<Grammar>Value` lands
(`A8:176-192, 409-411`), then deleting.

### D3. CTNS activation — a live substrate one line from firing

`A9:96-108`: `ConsumeToNextStructural` is fully mined but wired as
`ctns_lifts: HashSet::new()` — **an unconditionally empty set** at
`crates/core/src/lower/mod.rs:236`. Routing every mined `CtnsLiftSet` node through a
`StructuralIndex` bitmap jump skips the byte-by-byte DFA walk for patterns whose match
boundary is a structural byte (the canonical CSS property-value `[^;)}]+` case). Expected
**10–13% self-time on CSS L4, 4–6% on Sheets, ~0% on JSON** (`A9:168-181`). Critical
negative-result dependency: the pre-pass allocates `Vec<u32>+Vec<u8>` proportional to
structural density; it **only nets a win if the tape columns are already inlined (D1)** —
land AoS-revert first or take a net regression (`A9:182-196`).

### D4. Pratt Option C (inline, NOT flatten) — the negative result costs a revert if re-learned

`A9:324-368`: the *only* KISS-compatible Pratt optimisation is inline the existing tape
primitives + hoist op-stack capacity from the mined per-rule chain-depth + write the
1-byte op discriminant directly into the payload column (skip the arena round-trip).
Folds into the D1 push-inline umbrella. **Option A (hybrid) and Option B (full flat +
consumer rewrite) are both REJECTED** — flat-tape breaks the reducer-compound consumer
contract (`collect_binary_operands` / `lower_binary_factor`), exactly the W0a.2.k revert
(`A9:276-300`). This must not be re-attempted.

### D5. The A7 lever-triage — a pre-computed answer key for Block B

`A7:70-90` already ran the fresh-attribution triage the totality plan needs:

- **RETIRE** (wrong symbol, cited): JSON SIMD micro-kernels vpaddq_u8/vdotq_s32/PMULL/
  scan-fusion/AltReorder — `__regex_scan_JsonParser` is ~0% self-time; JSON's cost is
  `push_structural` 36–43% + `finalise` 23–27% (`A7:118-131`, `A1` cited). CSS SIMD
  cluster (TBL-4/kind-streams/SIMD-Alt/Bloom+GADT) — bbnf *already beats* lightningcss
  0.60–0.81× (`A7:132-143`). Speculative parsing + Markov — dispatch is ≤12% on every
  grammar; a 35%-prior lever against a 12% ceiling (`A7:144-154`).
- **FOLD** (still valuable): L1/L2 miner inheritance (PHF dispatch), scanner
  generalization, BoundedRegex per-rule termination, structural-scan pre-pass,
  document-parallel fork (demoted to a late wave — amortisation multiplier only, does not
  close the single-thread JSON gap).

### D6. The DTA-residue inventory + live-survivor list (A8)

`A8:295-393` is a surgical carve map: ~458 LOC kernel-dead in `crates/tape/src/dta.rs`
(the 13-variant `DtaState`, `DtaTable`, `DtaDiagnostic`, `DtaFrameKind`,
`DtaCounterOptional`, `SeqPromote`, `LiteralPayload`); `shape_dict.rs` (79 LOC,
`BBNF_SHAPE_DICT` never emitted); dead `GrammarProfile` slots (`list_rules`,
`shape_dict`, three push-counts). **Live survivors that must NOT be carved**:
`DtaPrecedenceEntry`+`DtaAssociativity`+`DtaRuleId` (Pratt tables, runtime-consumed by
`tape::driver::lookup_precedence`), the `DtaError` return surface, `capacity_for`, the
PSI `parallel_break_even_bytes` gate (`A8:363-393`). The IR-side `dta.rs` lifter (1,611
LOC) is *still called* as a regex-pattern + precedence *miner* — only its state-machine
lifting half is dead; extract the harvest before deleting (`A8:353-361`).

### D7. Negative results that must not be re-learned

- Hand-coded per-grammar `Value` duplicates violate generalization; parity proves via
  canonical-serialisation byte equality on both parser sides, never `From<third-party>` /
  `PartialEq<third-party>` bridges (`01-archaeology:71-73`, invariant 21).
- lightningcss `PrinterOptions{minify:false}` performs `calc()` arithmetic, position-pair
  commutativity, and shorthand reordering **no symmetric byte normaliser can invert**
  (`W1r3a-diag`, `FINAL.md:171-177`) — byte-parity is impossible on prettified CSS at
  scale; declare per-comparator×corpus which parity tier (byte / canonical-form /
  scale+interop) applies at plan time (`01-archaeology:82-83`).
- `TypeDesc::Named` collapses in the Rust `prepare_grammar → analyze_grammar →
  project_types` union; `prune_unreachable` is the culprit; **no commit introduces the
  collapse — it is emergent** (`01-archaeology:36`, `A7:48-52`). Attack it at the upstream
  pass with a preservation pass + a consumer test that asserts the call *fires* (samply /
  nm), not a grep gate.
- `grep` + `cargo expand` are supplementary, not load-bearing: AT.1 passed its grep gate
  while every typed capture was a dead store; AW-IV.W3.5a emitted a
  `PROJECTION_DIRECT_TO_STRUCT` const with zero runtime fires (`01-archaeology:79`).

---

## (e) Meta-process pathologies

**Failures:**

1. **Invariants as aspiration, not enforcement.** 21 invariants authored; 9/12/16/17
   unenforced at sub-wave close. Close ledgers self-reported "Met under narrow predicate"
   instead of citing bench artefacts (`R1:169-172`, `R1:205-216`). The mechanical fix R1
   proposed — orchestrator sub-wave close requires either a bench JSON with the full
   matrix or a plan-declared interim-state escape, no narrative "partial close" — is the
   process lever worth carrying.
2. **Sub-wave letter-suffix proliferation** (`.a`..`.h`..`.i`..`.r`) instead of the
   new-letter escalation the SPEC mandates. The scope-reveal "absorb vs new letter"
   discriminator kept resolving to "absorb" long past the AR/AS anti-example threshold
   (`R1:161-168, 218-229`).
3. **Bench-against-moving-baseline** normalised regression into apparent progress (root
   cause 2). The mid-wave bench mandate was met in form, inverted in substance.
4. **Substrate-first landings passed hard gates** ("code exists") while runtime never
   fired — the chronic 11-tranche pattern (`01-archaeology:54-63`).

**Successes worth keeping:**

1. **The W0a.close bench-baseline wave** — a dedicated single-agent wave capturing a
   pre-deletion attribution anchor so downstream deltas attribute to their own lever, not
   walker-death cost (`AX.md:60`, `SYNTHESIS:145-149`). Good pattern; its flaw was
   choosing the regressed anchor.
2. **The R1–R4 + SYNTHESIS mid-tranche retrospective** — four disjoint-scope read-only
   agents converged independently on the same root cause (walker-parity-as-oracle) and
   produced the `ax-iter`/`test-tier` infra + the SPEC edicts that unblocked the restart
   (`SYNTHESIS:14-30`). Mid-tranche process correction, done well.
3. **The A1–A10 honest close** — ten fresh-attribution audits that measured the 4.5×
   regression and reframed Block B into a successor rather than shipping silent deferral
   (`FINAL.md:371-403`). The FINAL invariant-by-invariant reconciliation (`FINAL.md:247-333`)
   is strong close-out discipline.

The meta-lesson for the totality plan: AX's *process instruments* (bench-baseline wave,
retrospective triumvirate, fresh-attribution close) were sound; its *enforcement* was
absent, and its *baseline choice* hid the one number that mattered. Carry the
instruments; make close gates cite absolute-vs-high-water bytes/cyc, not
relative-vs-last-close MB/s.
