# SK-V17 S-P1 CHALLENGE — CH3 REGRESSION (V3)

Lens: CH3 REGRESSION. Pass: S-P1 Profile. Cycle: V3. Date: 2026-05-29.
Reviewer scope: `restart/skinny/tranches/sk-v17/research/p1/{p1a..p1f}.md` §4
(+ P1-C §2.5, P1-D §2.5, P1-F §3.2).
Authority: `PASS-1-PROFILE.md` §3 (CH3 `:137-141`) + ORCHESTRATOR §3W/§3Z.
Baseline: master HEAD `6496fecae` (working tree; p1 directory uncommitted).
Prior cycles: V1 CH3 = 36/37 ACCEPT (97.3%), 1 REVISE (R-CH3-1); V2 CH3 = 41/41
ACCEPT (100%), 0 REVISE, 0 REJECT.

## §0 — CH3 mandate (what this lens dispositions)

Per `PASS-1-PROFILE.md` §3 CH3: "does any anomaly flagged in §4 silently
re-propose a route already in `skinny/REDRESS.md`? S-P1 proposes nothing, but a §4
'this hot leaf suggests X' that points at a pre-blocked route (REDRESS 50-55,
60-72, 80, 82-84, 88, 89) must cite the entry and mark it pre-blocked, not
implicitly re-open it." Orchestrator focus: the **fact-stream-String cost must be
named honestly as the de-fact-stream target**, not laundered into a re-admission
of the pre-blocked `emit_fact_stream` plane.

### §0.1 — The pre-blocked route families (RE-RESOLVED against REDRESS.md + HANDOFF.md this cycle)

| Family | REDRESS items | Route REJECTED | The thin-ice adjacency for S-P1 |
|---|---|---|---|
| Retained projection / parse-time aux side table | 50 | dense/sparse per-cursor aux columns on the offset tape (`REDRESS.md:715`) | a tape lever that writes aux columns at parse time |
| Event-cursor / parser-local second scanner | 51, 53 | `JsonEventCursor` / `JsonStructuralCursor` — a second structural scan bolted onto recursive descent (`REDRESS.md:740,784`); ADMISSIBLE route = "scanner writes the tape/event stream and generated lowering consumes that stream directly" (`:807-813`) | a NEON pre-scan or "tokenize-once" framed as a parser-owned cursor over source bytes |
| Decoded-string stats sink / quote-source fused materializer | 54, 55 | sink-local exact-stats and one-pass streaming-hash String materializers (`REDRESS.md:815,846`) | a tape lever framed as a String/sink materializer |
| Direct-string alloc / receiver / byte-writing / semantic-string-fact | 60-72 | the SK-V6 direct-materialization family | retiring the fact-stream String by replacing it with another eager String/fact emitter |
| Mantissa-widen / Unicode-quartet / StringBlock16 / value-byte compaction | 80, 82, 83, 84 | numeric + string micro-kernels rejected on measured regression | a CSS digit/string kernel proposed without a fresh benched antecedent |
| PMULL prefix-XOR / CTZ bulk consumer | 88, 89 | consumed-bitmap-body SIMD shapes rejected | a NEON bitmap-consumer proposed as a route, not a gated antecedent |

HANDOFF `Pre-Blocked Routes` (`HANDOFF.md:148-185`, RE-READ this cycle) additionally
binds: the `emit_fact_stream` fact-stream plane is **diagnostic-only**
(`:157-159`, cites `generated.rs:5`); **no second substrate** — no introduced
`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef`,
the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`
(`:171-174`); AZ-IV eager-value-tree pre-block (`:152-154`); StructRegistry hot-path
indirection (`:155-156`); FNV bench-only (`:165`); brace-counter CSS admission and
wrong-plane comparator admission (`:167-169`). Inherited REDRESS families line
(`:177-178`) carries `50-55, 60-72, 80, 82-84, 88, 89` verbatim.

### §0.2 — Benched-surface ground truth (RE-VERIFIED FRESH this cycle, not inherited)

The p1 artefacts changed between V2 and V3 (section line numbers shifted), so every
load-bearing citation was re-resolved against the working tree this cycle:

- `emit_fact_stream(input: &str) -> Result<String, CssFactError>` opens
  `let mut out = String::new(); out.push_str(config::FACT_SCHEMA)`
  (`generated.rs:5-7`, re-read) — the un-presized String fact-stream the
  de-fact-stream target retires. **Confirmed verbatim.**
- `TapeBuilder::push_plain_offset(&mut self, offset: usize) -> u32`
  (`assembler.rs:71-78`, re-read) is one bounds-checked `u32` push into the EXISTING
  `self.offsets` vec (`reserve_offsets_cold` only at capacity) — NOT a new builder
  type, honouring HANDOFF `:171-174`. **Confirmed.**
- REDRESS 53 `:807-813` reads verbatim: "structural projection must be the parser's
  single substrate, not a second scanner bolted onto source-byte recursive descent.
  Either the scanner writes the tape/event stream and generated lowering consumes
  that stream directly, or a `CollapsedStage` / `SinkOnly` lowering consumes live
  masks in the same loop. A `ParserState`-owned structural cursor over source bytes
  is non-canonical…" — the exact admissible-vs-rejected boundary P1-C §2.5 / P1-D
  §4.4 cite. **Confirmed.** REDRESS 53 head `:784` "SK-V5 structural-mask
  parser-local cursor is REJECTED." **Confirmed.**
- REDRESS 50 `:710-715` (parse-time aux side tables REJECTED), 51 `:740` (event-cursor
  REJECTED), 54 `:815` (decoded-string stats sink), 55 `:846` (quote-source fused
  materializer) — anchors re-resolved. **Confirmed.**

Every artefact's "fact-stream is a `String`, the tape append replaces it" claim is
factually grounded against fresh source; the de-fact-stream target is named honestly
across all six. No artefact proposes a String-sink materializer, an aux side-table,
an eager value tree, or a parser-local second cursor.

### §0.3 — Prior-REVISE fold status (carried-forward check)

V1 CH3's lone REVISE, **R-CH3-1**, asked P1-D's tokenize-once structural-rewrite
suggestion to cite REDRESS 51/53 and mark the admissible-vs-rejected boundary. V2
confirmed the fold complete. RE-VERIFIED present in the V3 artefacts:

- **P1-D §2.5 (`p1d-pmu-cycles.md:325-336`)** carries the headed paragraph
  **"REDRESS 51/53 boundary (citation discipline; CH3 §3)"**: single-pass admissible
  ONLY as the REDRESS-53 single-substrate shape (`:807-813`), REJECTED as a
  parser-local SECOND scanner / retained `ParserState` cursor over source bytes
  (`:784-805, 807-813`), naming item 53 measured that shape as a regression. Closes
  "S-P1 proposes nothing; this is flagged as the admissible-vs-rejected boundary."
- **P1-D §4-4 (`:506-514`)** appends inline the same admissible(`:807-813`)/rejected
  (`:784-805`) boundary, "Flagged with the boundary, not re-opened."
- **P1-C §2.5** likewise carries the boundary disambiguation in its own
  redundant-rescan note.

Both P1-D citations resolve to the verbatim REDRESS lines re-verified in §0.2. **No
orphan REVISE carried into V3 — R-CH3-1 remains RESOLVED.**

---

## §1 — Per-artefact disposition (path:line + concrete fix)

### P1-A `p1a-samply-mode-1.md` — §4 (lines 170-181)

- §4.1 (`:172`) recognition-plane masking — names the honest target as living
  BETWEEN track1_full (~2574 aggregate, no AST) and track1_fact (~800, String);
  ends "NOT a re-propose of any route; an observation." **ACCEPT.**
- §4.2 (`:174-175`) `find_component_delim` NEON antecedent — names the
  grammar-neutral `find_ascii_set_member64` / `byte_class_index_64` primitive shape,
  routes to `select_classifier` (`dispatch.rs:42`), states "pre-blocked behind tape
  activation … flagged, NOT re-opened." Corpus-dependence note added. Gated per
  REDRESS 51/53 admissible boundary. **ACCEPT.**
- §4.3 (`:177`) fact-stream allocation lever — names `emit_fact_stream` String
  growth (`RawVecInner::reserve`) as the de-fact-stream target, routes to the
  EXISTING `assembler.rs:71 push_plain_offset`. `push_ascii_lower_hex` correctly
  called FNV-diagnostic-only ("disappears entirely when the fact-stream String is
  retired … must not be carried into S-P2 as a primitive"), matching FNV-bench-only
  (HANDOFF `:165`). Also corrects the inherited `emit_* ~34%` recognition-plane
  figure (cross-refs P1-D §4-3). **ACCEPT.**
- §4.4 (`:179`) explicit: "not a re-open of REDRESS (the fact-stream-as-admission
  pre-block); it is the empirical case for retiring it." Honest de-fact-stream
  framing, cites SYNTHESIS §0.4. **ACCEPT.**
- §4.5 (`:181`) Lock-1 no-second-substrate / no-sidecar; notes substrate UNWIRED on
  CSS path. **ACCEPT.**

Disposition P1-A §4: **ACCEPT** (5/5). No silent re-open.

### P1-B `p1b-samply-mode-2.md` — §4 (lines 321-382)

- §4.1 (`:323`) orthogonal-leaf lever-sequencing MASKING signal (scan masked under
  allocation on the fact-stream plane), no route. **ACCEPT.**
- §4.2 (`:335`) NEON antecedent re-confirmed on benched path (56.55%/11.51%), gated;
  notes post-W2 typed plane must be re-profiled. **ACCEPT.**
- §4.3 (`:344`) `push_ascii_lower_hex` per-token alloc + diagnostic encode —
  explicitly distinguished from AZ-IV eager-value-tree ("builds a hex String, not a
  `Box<CssColor>` tree") and named "the tape retires WHOLESALE … its ~9% fact-stream
  self-time is pure diagnostic tax, removed not deferred." The AZ-IV disambiguation
  is the exact CH3 citation. **ACCEPT.**
- §4.4 (`:358`) `core::fmt::num` overhead — observation, tape stores raw `u32`.
  **ACCEPT.**
- §4.5 (`:362`) the dedicated CH3 check — names the full list "REDRESS 50-55 / 60-72
  / 80 / 82-84 / 88 / 89 / 127" and asserts "no second substrate, no sidecar, no
  registry, no eager value tree, no fixture/FNV contrivance, no x86." **ACCEPT.**
- §4.6 (`:371`) harness-dependent absolute Mbps; only within-harness ratio
  load-bearing — measurement note (CH4 territory, no route). **ACCEPT.**

Disposition P1-B §4: **ACCEPT** (6/6). The §4.5 explicit pre-block check remains the
model the set follows.

### P1-C `p1c-samply-mode-3.md` — §2.5 (boundary note) + §4 (lines 354-405)

- §2.5 redundant-rescan note carries the REDRESS 51/53 admissible-vs-rejected
  disambiguation. **ACCEPT.**
- A1 (`:356`) plane bifurcation — observation, disjoint hot-leaf sets named.
  **ACCEPT.**
- A2 (`:368`) masking-probe read — names materialization (not scanning) as the
  structural inefficiency, sizes the ~80-180 i/B headroom, no route. **ACCEPT.**
- A3 (`:378`) FNV in the hot path — "FNV and the hex encoder are FNV-diagnostic
  primitives with NO CSS-semantic value … Flagged so S-P2 does not carry FNV/hex
  -encode into the tape emitter (REDRESS 'FNV stays bench-only')." Cites the
  pre-block; matches HANDOFF `:165`. **ACCEPT.**
- A4 (`:385`) cold first-touch min outliers — measurement honesty
  (`no-warm-benches`). **ACCEPT.**
- A5 (`:391`) zero NEON on CSS path — antecedent, gated behind tape ("no structural
  index until the tape decodes CSS"); notes the shared byte-membership inner loop
  makes it ONE kernel. **ACCEPT.**
- Pre-blocked-route check (`:400-405`) — dedicated CH3 paragraph: "§4 proposes
  nothing," names "REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89," states the
  fact-stream plane is measured as diagnostic only, "does NOT propose admitting it."
  **ACCEPT.**

Disposition P1-C §4: **ACCEPT** (§2.5 + 5 items + dedicated check = 7/7).

### P1-D `p1d-pmu-cycles.md` — §2.5 (lines 313-336) + §4 (lines 468-550)

- §2.5 (`:313-336`) redundant overlapping re-scan + **the REDRESS 51/53 boundary
  paragraph** (`:325-336`, the V1 REVISE fix) — admissible ONLY as REDRESS-53
  single-substrate (`:807-813`), REJECTED as parser-local cursor (item 53,
  `:784-805`). Both lines re-verified verbatim (§0.2). Thin-ice marker present.
  **ACCEPT.**
- §4.1 (`:470`) recognition-plane masking — names the honest 300-600 Mbps band; "the
  eager floor (~3) is what lazy projection must NOT regress to." Observation.
  **ACCEPT.**
- §4.2 (`:481`) NEON antecedent re-confirmed (79.6%/15.7%), routes to
  `byte_class_index_64` over `select_classifier`. Gated. **ACCEPT.**
- §4.3 (`:492`) **the strongest de-fact-stream-honesty item in the set.** Corrects
  the inherited `emit_* ~34%` figure: on the recognition (`emit_full_parse`) plane
  the String emit is <0.1%, so "S-P2 should NOT over-index the String lever *on the
  recognition plane*" — while affirming "the String-emit pre-block remains a correct
  forward concern" because the `emit_fact_stream` plane DOES pay the full 217-370 i/B
  String tax. Names the fact-stream-String as the de-fact-stream target *on the
  fact-stream plane* and prevents a future wave from "retiring" a cost on the wrong
  plane. **ACCEPT.**
- §4.4 (`:506-514`) redundant 2-3× overlapping scan + tokenize-once candidate **with
  the inline REDRESS-53-admissible / REDRESS-51/53-rejected boundary citation** (the
  V1 fix). "Flagged with the boundary, not re-opened." Lands on the admissible
  single-substrate side. **ACCEPT.**
- §4.5 (`:516`) PMU instr/byte measured; cyc/byte RAW + non-load-bearing; the V2
  "proven 4.27 GHz counter / supersedes" over-claim explicitly WITHDRAWN as
  circular. Measurement honesty — no route. **ACCEPT.**
- §4.6 (`:530`) per-line attribution now artefact-backed. Measurement honesty.
  **ACCEPT.**
- §4.7 (`:536`) zero SIMD on CSS path — "the gated-behind-tape W4 lever's empirical
  antecedent; NOT a REDRESS-blocked re-proposal." Names `digit_mac.rs:27` udot orphan
  never reached — honours REDRESS 80/82 no-free-kernel discipline. **ACCEPT.**
- §4.8 (`:545`) harness convergence (X2) — reproducibility. **ACCEPT.**

Disposition P1-D: **ACCEPT** (§2.5 + 8/8 §4 items = 9/9). The R-CH3-1 fix holds; no
new re-open. P1-D §4-3 remains the load-bearing de-fact-stream-honesty correction
for the whole pass.

### P1-E `p1e-hot-leaf-attribution.md` — §4 (lines 304-358)

- §4.1 (`:306`) recognition-plane mask — "MUST NOT be read as a >SOTA admission (it
  fails preserve-rich-ast, `SYNTHESIS.md §0.1`)." **ACCEPT.**
- §4.2 (`:315`) fact-stream allocator floor → tape append on EXISTING
  `assembler.rs:71` "one branchless u32 write"; "NOT a re-proposal — it is the
  measured ground for the contract's own pre-declared route." Honest de-fact-stream
  naming, no second substrate. **ACCEPT.**
- §4.3 (`:324`) NEON antecedent re-confirmed; explicitly orders "lever-1/2 (tape)
  FIRST, then NEON on the surviving scan. S-P2 must not invert this." Gated.
  **ACCEPT.**
- §4.4 (`:337`) **honest orphan-block of the C4b digit kernel** — "the udot/i8mm
  digit kernel (`digit_mac.rs:27`, C4b) has no benched CSS antecedent … C4b stays
  orphan-blocked on the current planes … S-P2 must re-profile the typed path after
  W1/W2, not inherit a CSS digit-kernel hypothesis from here." Exemplary handling of
  REDRESS 80/82 + ORCHESTRATOR §8 profile-first. **ACCEPT.**
- §4.5 (`:348`) material lightningcss min outlier — measurement honesty (CH6).
  **ACCEPT.**
- §4.6 (`:351-358`) dedicated CH3 pre-block check: "Nothing in §4 re-proposes a
  REDRESS-blocked route," explicitly disclaiming AZ-IV eager-value-tree, single
  non-generic `TapeBuilder` (no StructRegistry indirection), and brace-counter CSS
  admission (`SYNTHESIS.md §0.4`). **ACCEPT.**

Disposition P1-E §4: **ACCEPT** (6/6 + dedicated check). The C4b orphan-block (§4.4)
remains the strongest single CH3-correct handling in the set.

### P1-F `p1f-bench-canonical.md` — §3.2 (K-classification) + §4 (lines 491-561)

- §3.2 (`:462-487`) K-classification — the CSS eager-typed plane (3.093 Mbps) tagged
  **K (pre-blocked)** "AZ-IV, SYNTHESIS §0.4"; the fact-stream plane tagged **L
  (loss)**; the "~70 / ~14×" narrative tagged **N-direct** (no fresh benched
  antecedent). A correct refusal to admit the eager path; SK-V17's task framed as
  landing a TYPED plane WITHOUT the eager-tree regression. **ACCEPT.**
- §4.1 (`:493`) plane-dependent gap (not flat 14×) — observation. **ACCEPT.**
- §4.2 (`:501`) fact-stream 64% alloc + 4.4× instr/byte → tape append on EXISTING
  `assembler.rs:71`; "(NOT a re-proposal — it is the contract's own lever, grounded
  here.)" Honest de-fact-stream naming. **ACCEPT.**
- §4.3 (`:511`) NEON antecedent re-confirmed (59%/10%), gated behind tape ("not a
  route re-opening"). **ACCEPT.**
- §4.4 (`:521`) `tape_activated = false` verified empirically (grep returns zero tape
  symbols on CSS path) — Lock-1 / no-second-substrate baseline. **ACCEPT.**
- §4.5 (`:528`) PMU `ri_cycles` non-load-bearing; V2 "physically impossible" framing
  WITHDRAWN; instr/byte is the sole grounded cost density. Measurement honesty — no
  route. **ACCEPT.**
- §4.6 (`:542`) no second substrate / no sidecar (Lock 1 / CH5) — the 26.74% wrapper
  bucket named as PURE measurement scaffold, "not a retained or second pass." Track 1
  and comparators keep separate symbol paths. **ACCEPT.**
- §4.7 (`:552`) JSON 51/51 guard untouched — read-only except the bench bin.
  **ACCEPT.**
- §4.8 (`:557`) X2 harness convergence — reproducibility. **ACCEPT.**

Disposition P1-F §4: **ACCEPT** (§3.2 + 8/8 §4 items = 9/9).

---

## §2 — Cross-cutting CH3 findings

1. **De-fact-stream honesty: HELD across all six (sharpest at P1-D §4-3).** Every
   artefact names the `emit_fact_stream` `String` (`generated.rs:5-7`, re-verified
   fresh this cycle) as the de-fact-stream target and routes its retirement to the
   EXISTING `TapeBuilder::push_plain_offset` (`assembler.rs:71`, re-verified
   branchless u32 push into `offsets`), never to a new `TapeStructBuilder`/`TapeCursor`
   (HANDOFF `:171-174`). P1-D §4-3 corrects the inherited ~34% emit figure and warns
   S-P2 not to mis-attribute the String lever to the recognition plane — preventing a
   future wave from "retiring" a cost on the wrong plane. The fact-stream plane is
   uniformly measured as a **diagnostic** (HANDOFF `:157-159`), never re-proposed for
   admission. Orchestrator focus met.

2. **Tape lever is NOT a REDRESS 50-55/60-72 re-open.** The rejected SK-V5/SK-V6
   families (50-55, 60-72) are String-*sink*/*materializer*/*aux-column* shapes
   (parse-time aux side table 50, event-cursor 51, decoded-stats sink 54, quote-source
   fused hash 55, semantic-string-fact 60-72) that failed on measured JSON regression.
   The SK-V17 lever is an offset-tape append on the EXISTING substrate (one `u32`
   push), explicitly distinguished from the eager-value-tree (AZ-IV; P1-B §4-3, P1-E
   §4-6) and from a String materializer. No artefact proposes a String-sink
   materializer or an aux side-table.

3. **NEON observation is gated per REDRESS 53's admissible boundary; the prior
   REVISE remains folded.** All six gate `find_component_delim` NEON behind tape
   activation ("no structural index to pre-scan into until the tape decodes CSS"),
   REDRESS 53's admissible single-substrate route. The tokenize-once structural
   rewrite (P1-C §2.5, P1-D §2.5 + §4-4) carries the verbatim admissible (`:807-813`)
   vs rejected (item 53, `:784-805`) boundary — re-verified against source this cycle.
   The set is uniform: every NEON / tokenize-once mention marks the thin ice.

4. **No fresh-antecedent-free kernel carried (REDRESS 80/82/83/84/88/89).** P1-E §4-4
   is exemplary: refuses to carry the C4b digit kernel without a benched CSS hot-leaf
   antecedent (zero digit-parse self-time on either plane) and defers re-profiling to
   the post-W2 typed path. P1-D §4-7 names the `digit_mac.rs:27` udot orphan as never
   reached. No PMULL/CTZ bitmap consumer (88/89) re-proposed.

5. **No silent re-admission of the recognition plane or brace-counter CSS.** Every
   artefact treats the recognition (`emit_full_parse`) plane's >lightningcss margin as
   a *masking signal* (wrong plane, no AST, fails preserve-rich-ast), never an admit;
   P1-E §4-1, P1-F §3.2 (K-classification), P1-A §4-1 mark recognition-only /
   eager-typed as non-admissible. P1-F §3.2 additionally classifies the "~70 / ~14×"
   narrative N-direct (no fresh antecedent) — a correct refusal to inherit an
   unsubstantiated number, honouring ORCHESTRATOR §8 profile-first.

6. **One measurement divergence noted, NOT a CH3 finding.** The `ri_cycles`
   load-bearing posture is settled this cycle (P1-D §4-5 and P1-F §4-5 now AGREE:
   instr/byte sole load-bearing, cyc/byte RAW + non-load-bearing, the V2 GHz
   over-claim withdrawn). The earlier V2 A/B/F-vs-D divergence is closed; whatever
   residue remains is a CH4/CH7 measurement-reconciliation item. Neither framing
   proposes a blocked route — out of CH3 scope. Flagged so the consolidator does not
   mis-route it.

---

## §3 — Counts + dispositions

| Artefact | §4 sections (incl. dedicated check / §2.5 / §3.2) | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-A | 5 | 5 | 0 | 0 |
| P1-B | 6 | 6 | 0 | 0 |
| P1-C | 7 (§2.5 + 5 + dedicated check) | 7 | 0 | 0 |
| P1-D | 9 (§2.5 + 8 §4) | 9 | 0 | 0 |
| P1-E | 6 | 6 | 0 | 0 |
| P1-F | 9 (§3.2 + 8 §4) | 9 | 0 | 0 |
| **Total** | **42** | **42** | **0** | **0** |

**ACCEPT rate: 42/42 = 100%.**

REVISE list: **none.** (V1's R-CH3-1 folded completely; re-verified present in V3 at
P1-D §2.5 `:325-336` and §4-4 `:506-514`, and mirrored at P1-C §2.5.)

REJECT list: **none.**

## §4 — Verdict

CH3 REGRESSION returns **ACCEPT 100% (42/42)**, **0 REVISE**, **0 REJECT**. The
fact-stream-String cost (`emit_fact_stream -> Result<String, _>`, `generated.rs:5-7`,
re-verified fresh this cycle) is named honestly as the de-fact-stream target across
all six artefacts — the orchestrator focus is met, and P1-D §4-3 sharpens it by
preventing a plane mis-attribution that would let a future wave "retire" a cost on
the wrong plane. The tape lever routes to the EXISTING `push_plain_offset`
(`assembler.rs:71`), never a second substrate (HANDOFF `:171-174`); the NEON and
tokenize-once observations are gated per REDRESS 53's admissible single-substrate
boundary; no fresh-antecedent-free kernel is carried (REDRESS 80/82-84/88/89); the
eager-typed plane is K-classified pre-blocked and the "~70 / ~14×" narrative is
N-direct. None of REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89 is re-opened. The V1
REVISE (R-CH3-1) remains RESOLVED. Zero orphan REVISE.

Above the §3Z 95% threshold; combined with V2's 100%, CH3 has now returned ≥95% for
**two consecutive cycles** (V2 100%, V3 100%) with zero open REVISE — the per-lens
convergence condition for CH3 is met.

## §5 — Sources

- Pass contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH3 `:137-141`).
- ORCHESTRATOR: `restart/prompts/ORCHESTRATOR.md` §3W (CH3), §3Z (convergence).
- REDRESS families (re-verified verbatim this cycle): `skinny/REDRESS.md` items 50
  (`:710-715`), 51 (`:740`), 53 (`:784` head, admissible route `:807-813` "scanner
  writes the tape/event stream … A `ParserState`-owned structural cursor over source
  bytes is non-canonical"), 54 (`:815`), 55 (`:846`), 60-72, 80, 82-84, 88, 89.
- HANDOFF pre-blocks (re-read): `restart/skinny/tranches/sk-v17/HANDOFF.md:148-185`
  (AZ-IV `:152-154`; StructRegistry `:155-156`; fact-stream diagnostic-only
  `:157-159`; FNV bench-only `:165`; brace-counter / wrong-plane `:167-169`;
  no-second-substrate `:171-174`; inherited REDRESS families `:177-178`).
- Benched-surface re-verification (this cycle): `generated.rs:5-7`
  (`emit_fact_stream -> Result<String, CssFactError>`, `String::new()`);
  `assembler.rs:71-78` (`push_plain_offset`, branchless u32 push into `offsets`,
  cold reserve only at capacity).
- Prior cycles: `…/research/p1/hardening/V1/CH3.md` (R-CH3-1), `…/V2/CH3.md`
  (41/41 ACCEPT).
- Artefacts dispositioned: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
  p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
  p1f-bench-canonical}.md` §4 (+ P1-C §2.5, P1-D §2.5, P1-F §3.2).
