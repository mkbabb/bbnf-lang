# SK-V17 S-P1 CHALLENGE — CH3 REGRESSION (V4)

Lens: CH3 REGRESSION. Pass: S-P1 Profile. Cycle: V4. Date: 2026-05-29.
Reviewer scope: `restart/skinny/tranches/sk-v17/research/p1/{p1a..p1f}.md` §4
(+ P1-C §2.5, P1-D §2.5, P1-F §3.2).
Authority: `PASS-1-PROFILE.md` §3 (CH3 `:137-141`) + ORCHESTRATOR §3W/§3Z.
Baseline: master HEAD `6496fecae` (working tree; p1 directory uncommitted, untracked).
Prior cycles: V1 CH3 = 36/37 ACCEPT (97.3%), 1 REVISE (R-CH3-1); V2 CH3 = 41/41
ACCEPT (100%); V3 CH3 = 42/42 ACCEPT (100%), 0 REVISE, 0 REJECT.

## §0 — CH3 mandate (what this lens dispositions)

Per `PASS-1-PROFILE.md` §3 CH3: "does any anomaly flagged in §4 silently re-propose a
route already in `skinny/REDRESS.md`? S-P1 proposes nothing, but a §4 'this hot leaf
suggests X' that points at a pre-blocked route (REDRESS 50-55, 60-72, 80, 82-84, 88, 89)
must cite the entry and mark it pre-blocked, not implicitly re-open it." Orchestrator
focus: the **fact-stream-String cost must be named honestly as the de-fact-stream
target**, not laundered into a re-admission of the pre-blocked `emit_fact_stream` plane.

### §0.1 — The pre-blocked route families (RE-RESOLVED against REDRESS.md this cycle)

| Family | REDRESS items | Route REJECTED | Thin-ice adjacency for S-P1 |
|---|---|---|---|
| Retained projection / parse-time aux side table | 50 | dense/sparse per-cursor aux columns on the offset tape (`REDRESS.md:715`) | a tape lever that writes aux columns at parse time |
| Event-cursor / parser-local second scanner | 51, 53 | `JsonEventCursor` / structural-mask cursor — a second scan bolted onto recursive descent (`REDRESS.md:742,784`); ADMISSIBLE = "scanner writes the tape/event stream and generated lowering consumes that stream directly" (`:807-813`) | a NEON pre-scan or tokenize-once framed as a parser-owned cursor over source bytes |
| Decoded-string stats sink / fused materializer | 54, 55 | sink-local exact-stats and one-pass streaming-hash String materializers (`REDRESS.md:815,846`) | a tape lever framed as a String/sink materializer |
| Direct-string alloc / receiver / semantic-string-fact | 60-72 | the SK-V6 direct-materialization family | retiring the fact-stream String by replacing it with another eager String/fact emitter |
| Mantissa-widen / Unicode-quartet / StringBlock16 / value-byte compaction | 80, 82-84 | numeric + string micro-kernels rejected on measured regression | a CSS digit/string kernel proposed without a fresh benched antecedent |
| PMULL prefix-XOR / CTZ bulk consumer | 88, 89 | consumed-bitmap-body SIMD shapes rejected | a NEON bitmap-consumer proposed as a route, not a gated antecedent |

HANDOFF `Pre-Blocked Routes` (`HANDOFF.md:148-185`) additionally binds: `emit_fact_stream`
is **diagnostic-only** (`:157-159`, cites `generated.rs:5`); **no second substrate** — no
introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the landed
`Tape`/`ValueRef`, the projection generator emits accessors over the EXISTING
`Tape`/`ValueRef` (`:171-174`); AZ-IV eager-value-tree pre-block (`:152-154`); StructRegistry
hot-path indirection (`:155-156`); FNV bench-only (`:165`); brace-counter CSS admission and
wrong-plane comparator admission (`:167-169`). Inherited REDRESS families (`:177-178`)
carries `50-55, 60-72, 80, 82-84, 88, 89` verbatim.

### §0.2 — Benched-surface ground truth (RE-VERIFIED FRESH this cycle against source)

The p1 artefacts are V4 revisions (re-grounded on a fresh measurement run per the
profile-first discipline; P1-F V4 fold log `p1f:32-58`), so every load-bearing citation
was re-resolved against the working tree this cycle:

- `emit_fact_stream(input: &str) -> Result<String, CssFactError>` opens
  `let mut out = String::new(); out.push_str(config::FACT_SCHEMA)`
  (`generated.rs:5-7`, re-read) — the un-presized String fact-stream the de-fact-stream
  target retires. **Confirmed verbatim.**
- `TapeBuilder::push_plain_offset(&mut self, offset: usize) -> u32` (`assembler.rs:71-78`,
  re-read) is one bounds-checked `u32` push into the EXISTING `self.offsets` vec
  (`reserve_offsets_cold` only at capacity) — NOT a new builder type, honouring HANDOFF
  `:171-174`. **Confirmed.**
- REDRESS 53 `:807-813` reads verbatim: "structural projection must be the parser's single
  substrate, not a second scanner bolted onto source-byte recursive descent. Either the
  scanner writes the tape/event stream and generated lowering consumes that stream
  directly, or a `CollapsedStage` / `SinkOnly` lowering consumes live masks in the same
  loop. A `ParserState`-owned structural cursor over source bytes is non-canonical…" — the
  exact admissible-vs-rejected boundary P1-C §4 A5 / P1-D §2.5 + §4.4 cite. **Confirmed.**
  REDRESS 53 head `:784` "SK-V5 structural-mask parser-local cursor is REJECTED."
  **Confirmed.** REDRESS 50 `:715`, 51 `:742`, 54 `:815`, 55 `:846` anchors re-resolved.
  **Confirmed.**

Every artefact's "fact-stream is a `String`, the tape append replaces it" claim is
factually grounded against fresh source; the de-fact-stream target is named honestly
across all six. No artefact proposes a String-sink materializer, an aux side-table, an
eager value tree, or a parser-local second cursor. The micro-kernel families (REDRESS
80/82-84/88/89) and their tokens (StringBlock16, mantissa, PMULL, prefix-XOR, quartet,
consumed-bitmap, CTZ) return **zero hits** across all six artefacts — none is invoked.

### §0.3 — Prior-REVISE fold status (carried-forward check)

V1 CH3's lone REVISE, **R-CH3-1**, asked P1-D's tokenize-once structural-rewrite
suggestion to cite REDRESS 51/53 and mark the admissible-vs-rejected boundary. V2 and V3
confirmed the fold complete. RE-VERIFIED present in the V4 artefacts:

- **P1-D §2.5 (`p1d-pmu-cycles.md:325-336`)** carries the headed paragraph **"REDRESS
  51/53 boundary (citation discipline; CH3 §3)"**: single-pass admissible ONLY as the
  REDRESS-53 single-substrate shape (`:807-813`), REJECTED as a parser-local SECOND
  scanner / retained `ParserState` cursor over source bytes (`:784-805, 807-813`), naming
  item 53 measured that shape as a regression, closing "S-P1 proposes nothing; this is
  flagged as the admissible-vs-rejected boundary."
- **P1-D §4.4 (`:514-522`)** appends inline the same admissible(`:807-813`)/rejected
  (`:784-805`) boundary, "Flagged with the boundary, not re-opened."
- **P1-C §4 A5 (`:392-399`)** and **P1-E §4.3 (`:331-335`)** likewise order tape-FIRST,
  then NEON on the surviving scan, gated on "no structural index until the tape decodes
  CSS" — the REDRESS-53 single-substrate posture.

The two `ri_cycles`-related V2 REVISEs (CH4-4 / CH6 / X1') are CH4/CH6/CH7 measurement
items, not CH3; P1-F §0 V4 fold log `:60-66` records them as resolved (the "physically
impossible CPI" framing struck, cyc/byte non-load-bearing on disambiguability grounds).
Out of CH3 scope; flagged only so the consolidator does not mis-route them. **No orphan
REVISE carried into V4 — R-CH3-1 remains RESOLVED.**

---

## §1 — Per-artefact disposition (path:line + concrete fix)

### P1-A `p1a-samply-mode-1.md` — §4 (lines 172-183)

- §4.1 (`:174`) recognition-plane masking — names the honest target as living BETWEEN
  track1_full (~2574 aggregate, no AST) and track1_fact (~800, String); "NOT a re-propose
  of any route; an observation." **ACCEPT.**
- §4.2 (`:176-177`) `find_component_delim` NEON antecedent — names the grammar-neutral
  byte-set-membership scan, routes to `select_classifier` (`dispatch.rs:42`), states
  "pre-blocked behind tape activation … flagged, NOT re-opened"; corpus-dependence note
  on `consume_balanced_at`. Gated per REDRESS 53 admissible boundary. **ACCEPT.**
- §4.3 (`:179`) fact-stream allocation lever — names `emit_fact_stream` String growth
  (`RawVecInner::reserve`) as the de-fact-stream target, routes to the EXISTING
  `assembler.rs:71 push_plain_offset`. `push_ascii_lower_hex` (`generated.rs:628`)
  correctly called FNV-diagnostic-only ("disappears entirely when the fact-stream String
  is retired … must not be carried into S-P2 as a primitive"), matching FNV-bench-only
  (HANDOFF `:165`). Corrects the inherited `emit_* ~34%` recognition-plane figure
  (cross-refs P1-D §4.3). **ACCEPT.**
- §4.4 (`:181`) explicit: "not a re-open of REDRESS (the fact-stream-as-admission
  pre-block); it is the empirical case for retiring it." Honest de-fact-stream framing.
  **ACCEPT.**
- §4.5 (`:183`) Lock-1 no-second-substrate / no-sidecar; notes substrate UNWIRED on the
  CSS path. **ACCEPT.**

Disposition P1-A §4: **ACCEPT (5/5).** No silent re-open.

### P1-B `p1b-samply-mode-2.md` — §4 (lines 321-383)

- §4.1 (`:323-333`) orthogonal-hot-leaf MASKING signal — tape lever attacks the
  fact-stream floor, NEON attacks the recognition scan; explicitly "the W4 NEON lever
  becomes load-bearing only AFTER W2 unmasks it." Lever ordering honest, no re-open.
  **ACCEPT.**
- §4.2 (`:335-342`) NEON antecedent re-confirmed on the benched path (56.55% / 11.51%);
  notes the typed-tape plane "must be re-profiled again" — refuses to inherit a kernel
  hypothesis across planes (ORCHESTRATOR §8). **ACCEPT.**
- §4.3 (`:344-356`) `push_ascii_lower_hex` FNV/hex diagnostic with NO CSS-semantic value;
  explicitly distinguishes it from AZ-IV eager-value-tree ("it builds a hex String, not a
  `Box<CssColor>` tree"); "the tape retires WHOLESALE … removed not deferred." Honest;
  no AZ-IV re-open. **ACCEPT.**
- §4.4 (`:358-360`) `core::fmt::num` Display ~1.4% — observation; tape stores raw u32
  offsets, no formatting. **ACCEPT.**
- §4.5 (`:362-369`) dedicated CH3 pre-block check: "none of the §4 observations re-propose
  a blocked route"; names SYNTHESIS §0.4 pre-block #3 DIRECTION (retire the String), NEON
  gated behind tape activation; "No REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89 / 127
  route is implied: no second substrate, no sidecar, no registry, no eager value tree, no
  fixture/FNV contrivance, no x86." Exemplary. **ACCEPT.**
- §4.6 (`:371-382`) harness-dependent absolute Mbps / within-harness ratio load-bearing —
  CH4 measurement item, no route. **ACCEPT.**

Disposition P1-B §4: **ACCEPT (6/6).** No silent re-open.

### P1-C `p1c-samply-mode-3.md` — §2.5 (lines 284-328) + §4 (lines 355-406)

- §2.5 (`:284-328`) reliable per-byte cost ledger — i/B load-bearing, cyc/byte RAW +
  non-load-bearing on disambiguability grounds; sizes the tape-activation lever against
  i/B. Pure measurement; CH4/CH7 cost posture, no route. **ACCEPT.**
- §4 A1 (`:357-368`) plane bifurcation; tape activation removes the §2.3 floor, NEON
  targets the §2.4 scan; "S-P2 must not conflate them." No re-open. **ACCEPT.**
- §4 A2 (`:369-378`) masking probe — recognition-only beats SOTA but names
  *materialization* as the inefficiency, not *scanning*; sizes headroom in i/B. Honest
  masking signal. **ACCEPT.**
- §4 A3 (`:379-385`) FNV + hex encoder diagnostic-only, "vanishes with tape activation …
  S-P2 does not carry FNV/hex-encode into the tape emitter (REDRESS 'FNV stays
  bench-only')." Cites the pre-block. **ACCEPT.**
- §4 A4 (`:386-391`) cold first-touch min outliers — honours `no-warm-benches`. CH6
  item, no route. **ACCEPT.**
- §4 A5 (`:392-399`) no NEON in benched leaves yet; the 58.59% scan leaf is the
  profile-first antecedent, "gated behind tape activation (there is no structural index
  until the tape decodes CSS)" — the REDRESS-53 single-substrate posture. **ACCEPT.**
- §4 dedicated CH3 check (`:401-406`) "§4 proposes nothing"; fact-stream String is
  "REDRESS-pre-blocked as a *live admission plane* … this profile measures it as
  diagnostic only and does NOT propose admitting it. No REDRESS 50-55 / 60-72 / 80 /
  82-84 / 88 / 89 route is implied." Exemplary. **ACCEPT.**

Disposition P1-C §2.5 + §4: **ACCEPT (7/7).** No silent re-open.

### P1-D `p1d-pmu-cycles.md` — §2.5 (lines 313-336) + §4 (lines 476-558)

- §2.5 (`:313-336`) redundant 2-3× overlapping re-scan — names tokenize-once as an S-P2
  target, IMMEDIATELY followed by the headed "REDRESS 51/53 boundary" paragraph citing
  the admissible (`:807-813`) vs rejected (`:784-805`) shapes verbatim, "the tape/event
  stream IS the substrate, not a bolted-on second cursor." The R-CH3-1 fold, re-verified
  against source §0.2. **ACCEPT.**
- §4.1 (`:478-487`) recognition-plane masking; honest feasibility band (300-600 Mbps),
  eager floor named as the floor lazy projection must NOT regress to. No re-open.
  **ACCEPT.**
- §4.2 (`:489-498`) NEON antecedent re-confirmed (79.6% / 15.7% of parse self-time),
  discharges the SYNTHESIS NEON-gate re-profile obligation; routes to `select_classifier`
  (`dispatch.rs:42`). Profile-justified, not a route re-open. **ACCEPT.**
- §4.3 (`:500-512`) correction of the inherited `emit_* ~34%` recognition-plane figure;
  sharpens that the String tax is real on the fact-stream plane (217-370 i/B) but
  negligible on recognition; "S-P2 should NOT over-index the String lever *on the
  recognition plane*." Prevents a plane mis-attribution. **ACCEPT.**
- §4.4 (`:514-522`) redundant scan tokenize-once — carries the verbatim REDRESS-53
  admissible (`:807-813`) vs REDRESS-51/53 rejected (`:784-805`) boundary; "Flagged with
  the boundary, not re-opened." **ACCEPT.**
- §4.5 (`:524-536`) instr/byte measured, cyc/byte RAW + non-load-bearing; V2 GHz
  over-claim withdrawn. CH4/CH7 measurement item, no route. **ACCEPT.**
- §4.6 (`:538-542`) per-line attribution artefact-backed (`atos_v2.txt`). CH6 item.
  **ACCEPT.**
- §4.7 (`:544-551`) no SIMD on CSS path confirmed; `digit_mac.rs:27` udot orphan "is
  never reached … NOT a REDRESS-blocked re-proposal" — the gated W4 antecedent, not a
  kernel route. **ACCEPT.**
- §4.8 (`:553-558`) harness convergence (CROSS X2). CH4 item, no route. **ACCEPT.**

Disposition P1-D §2.5 + §4: **ACCEPT (9/9).** No silent re-open.

### P1-E `p1e-hot-leaf-attribution.md` — §4 (lines 304-357)

- §4.1 (`:306-313`) recognition path as a masking probe; "MUST NOT be read as a >SOTA
  admission (it fails preserve-rich-ast)." Honest. **ACCEPT.**
- §4.2 (`:315-322`) fact_stream floor ~58% allocator; tape activation routes to the
  EXISTING `assembler.rs:71 push_plain_offset`; "NOT a re-proposal — it is the measured
  ground for the contract's own pre-declared route." **ACCEPT.**
- §4.3 (`:324-335`) ONE NEON byte-class target; explicitly orders "lever-1/2 (tape) FIRST,
  then NEON on the surviving scan. S-P2 must not invert this." Gated. **ACCEPT.**
- §4.4 (`:337-346`) no number/unicode/dispatch/tape hot leaf — REFUSES to carry the C4b
  udot/i8mm digit kernel (`digit_mac.rs:27`, REDRESS 80-class) without a benched CSS
  antecedent ("zero digit-parse self-time … C4b stays orphan-blocked … S-P2 must
  re-profile the typed path after W1/W2, not inherit a CSS digit-kernel hypothesis from
  here"). Exemplary profile-first refusal. **ACCEPT.**
- §4.5 (`:348-349`) material lightningcss min outlier — CH6 statistic justification, no
  route. **ACCEPT.**
- §4.6 (`:351-357`) dedicated CH3 pre-block check — explicitly does NOT re-open AZ-IV
  eager-value-tree (tape stays lazy, no per-leaf eager payload) nor StructRegistry
  (single non-generic `TapeBuilder`); recognition headroom is NOT a brace-counter
  admission (`SYNTHESIS.md §0.4`). Exemplary. **ACCEPT.**

Disposition P1-E §4: **ACCEPT (6/6).** No silent re-open.

### P1-F `p1f-bench-canonical.md` — §3.2 (lines 506-538) + §4 (lines 542-612)

- §3.2 (`:506-538`) FALSIFIES the "~70 Mbps / ~14×" narrative as the benched-track1
  number; classifies it **N-direct** (no fresh benched antecedent — honours ORCHESTRATOR
  §8); classifies the eager-typed plane (3 Mbps) **K (pre-blocked, AZ-IV, SYNTHESIS
  §0.4)**, the fact-stream plane **L (loss)**. Refuses to inherit an unsubstantiated
  number; pre-blocks the eager plane. **ACCEPT.**
- §4.1 (`:544-550`) headline gap is plane-dependent, not a flat 14×; the tape-activation
  empirical floor. No re-open. **ACCEPT.**
- §4.2 (`:552-560`) fact-stream 64% alloc + 4.4× full-parse i/B; tape append
  (`push_plain_offset`, `assembler.rs:71`) removes the per-token push_str/hex; "NOT a
  re-proposal — it is the contract's own lever, grounded here." **ACCEPT.**
- §4.3 (`:562-570`) NEON antecedent re-confirmed (59% / 10%); "NEON is gated behind tape
  activation (no structural index to pre-scan into until the tape decodes CSS), so this
  is a flagged antecedent, not a route re-opening." **ACCEPT.**
- §4.4 (`:572-577`) `tape_activated = false` for CSS verified fresh (grep returns zero);
  close-gate baseline. Observation, no route. **ACCEPT.**
- §4.5 (`:579-591`) `ri_cycles` valid but non-load-bearing on disambiguability grounds;
  V2 "physically impossible" framing withdrawn. CH4/CH7 measurement item, no route.
  **ACCEPT.**
- §4.6 (`:593-601`) dedicated Lock-1 / CH5 check — no second substrate, no sidecar, no
  retained cursor; the 26.74% wrapper bucket is "PURE measurement scaffold, NOT a
  retained or second pass"; Track 1 and comparators keep separate symbol paths. **ACCEPT.**
- §4.7 (`:603-606`) JSON 51/51 guard untouched — tripwire. No route. **ACCEPT.**
- §4.8 (`:608-612`) X2 harness convergence. CH4 item, no route. **ACCEPT.**

Disposition P1-F §3.2 + §4: **ACCEPT (9/9).** No silent re-open.

---

## §2 — Cross-artefact regression posture (the orchestrator focus)

1. **The de-fact-stream target is named honestly across all six.** Each artefact names
   `emit_fact_stream -> Result<String, CssFactError>` (`generated.rs:5`, re-verified
   verbatim §0.2) as the COST to RETIRE, never as a plane to RE-ADMIT. The tape lever
   routes uniformly to the EXISTING `TapeBuilder::push_plain_offset` (`assembler.rs:71`,
   one branchless u32 push into `self.offsets`, cold reserve only at capacity — re-read
   §0.2), never a new `StructLayout`/`TapeStructBuilder`/`TapeCursor` (HANDOFF `:171-174`).
   The orchestrator focus is met. P1-D §4.3 sharpens it by preventing a plane
   mis-attribution that would let a future wave "retire" a String cost on the wrong
   (recognition) plane where it does not appear.

2. **The FNV/hex diagnostic is correctly quarantined.** P1-A §4.3, P1-B §4.3, P1-C A3 all
   name `push_ascii_lower_hex` / `fnv64` as FNV-diagnostic-only with NO CSS-semantic
   value, "removed not deferred" by tape activation, "must not be carried into S-P2 as a
   primitive." This honours FNV-bench-only (HANDOFF `:165`) and is the correct posture: it
   is not a string-materializer route (REDRESS 54/55), it is diagnostic emission the tape
   eliminates wholesale. No re-open.

3. **Every NEON / tokenize-once observation marks the thin ice.** All four NEON antecedent
   mentions (P1-A §4.2, P1-B §4.2, P1-C A5, P1-D §4.2, P1-E §4.3, P1-F §4.3) gate
   `find_component_delim` behind tape activation ("no structural index to pre-scan into
   until the tape decodes CSS") — REDRESS 53's admissible single-substrate route. The two
   tokenize-once mentions (P1-D §2.5 `:322`, §4.4 `:514`) carry the verbatim admissible
   (`:807-813`) vs rejected (item 53, `:784-805`) boundary, re-verified against source.
   The set is uniform: every NEON / tokenize-once mention marks the thin ice.

4. **No fresh-antecedent-free kernel carried (REDRESS 80/82-84/88/89).** Zero hits for
   StringBlock16 / mantissa / PMULL / prefix-XOR / quartet / consumed-bitmap / CTZ across
   all six. P1-E §4.4 is exemplary: refuses to carry the C4b digit kernel without a
   benched CSS hot-leaf antecedent (zero digit-parse self-time on either plane) and defers
   re-profiling to the post-W2 typed path. P1-D §4.7 names the `digit_mac.rs:27` udot
   orphan as never reached. No PMULL/CTZ bitmap consumer (88/89) re-proposed.

5. **No silent re-admission of the recognition plane or brace-counter CSS.** Every
   artefact treats the recognition (`emit_full_parse`) plane's >lightningcss margin as a
   *masking signal* (wrong plane, no AST, fails preserve-rich-ast), never an admit;
   P1-E §4.1, P1-F §3.2 (A/K classification), P1-A §4.1, P1-C A2 mark recognition-only /
   eager-typed as non-admissible. P1-F §3.2 classifies the "~70 / ~14×" narrative
   N-direct (no fresh antecedent) — a correct refusal to inherit an unsubstantiated
   number, honouring ORCHESTRATOR §8 profile-first.

6. **The samply `.syms.json` sidecar is a profiling artefact, not a parser sidecar.**
   P1-A `:10,64,67,198` uses "sidecar" for the symbolication tooling
   (`--unstable-presymbolicate` → `.syms.json` → `symbolicate.py`). This is correctly a
   profiler concern, not a Lock-1 parallel substrate — every parser-substrate "sidecar"
   reference (P1-B §4.5, P1-F §4.6, P1-E §4.6) explicitly states none is introduced. No
   conflation; out of CH3 scope.

7. **One measurement divergence noted, NOT a CH3 finding.** The `ri_cycles` posture is
   settled this cycle (P1-D §4.5 and P1-F §4.5 AGREE: instr/byte sole load-bearing,
   cyc/byte RAW + non-load-bearing; the V2 "physically impossible" framing withdrawn). The
   residue is a CH4/CH7 measurement-reconciliation item. Neither framing proposes a
   blocked route — out of CH3 scope. Flagged so the consolidator does not mis-route it.

---

## §3 — Counts + dispositions

| Artefact | §4 sections (incl. dedicated check / §2.5 / §3.2) | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-A | 5 | 5 | 0 | 0 |
| P1-B | 6 | 6 | 0 | 0 |
| P1-C | 7 (§2.5 + 5 A-items + dedicated check) | 7 | 0 | 0 |
| P1-D | 9 (§2.5 + 8 §4) | 9 | 0 | 0 |
| P1-E | 6 | 6 | 0 | 0 |
| P1-F | 9 (§3.2 + 8 §4) | 9 | 0 | 0 |
| **Total** | **42** | **42** | **0** | **0** |

**ACCEPT rate: 42/42 = 100%.**

REVISE list: **none.** (V1's R-CH3-1 folded completely; re-verified present in V4 at
P1-D §2.5 `:325-336` and §4.4 `:514-522`, mirrored at P1-C §4 A5 and P1-E §4.3.)

REJECT list: **none.**

## §4 — Verdict

CH3 REGRESSION returns **ACCEPT 100% (42/42)**, **0 REVISE**, **0 REJECT**. The
fact-stream-String cost (`emit_fact_stream -> Result<String, CssFactError>`,
`generated.rs:5-7`, re-verified fresh this cycle) is named honestly as the de-fact-stream
target across all six artefacts — the orchestrator focus is met, and P1-D §4.3 sharpens
it by preventing a plane mis-attribution. The tape lever routes to the EXISTING
`push_plain_offset` (`assembler.rs:71-78`, re-read), never a second substrate (HANDOFF
`:171-174`); the NEON and tokenize-once observations are gated per REDRESS 53's admissible
single-substrate boundary (`:807-813`, re-verified verbatim); no fresh-antecedent-free
kernel is carried (REDRESS 80/82-84/88/89 — zero hits); the eager-typed plane is
K-classified pre-blocked and the "~70 / ~14×" narrative is N-direct. None of REDRESS
50-55 / 60-72 / 80 / 82-84 / 88 / 89 is re-opened. The V1 REVISE (R-CH3-1) remains
RESOLVED. Zero orphan REVISE.

Above the §3Z 95% threshold; combined with V2 (100%) and V3 (100%), CH3 has now returned
≥95% for **three consecutive cycles** with zero open REVISE — the per-lens convergence
condition for CH3 is met and held across the V4 re-grounding.

## §5 — Sources

- Pass contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH3 `:137-141`).
- ORCHESTRATOR: `restart/prompts/ORCHESTRATOR.md` §3W (CH3), §3Z (convergence).
- REDRESS families (re-verified verbatim this cycle): `skinny/REDRESS.md` items 50
  (`:715`), 51 (`:742`), 53 (`:784` head, admissible route `:807-813` "scanner writes the
  tape/event stream … A `ParserState`-owned structural cursor over source bytes is
  non-canonical"), 54 (`:815`), 55 (`:846`), 60-72, 80, 82-84, 88, 89.
- HANDOFF pre-blocks (re-read): `restart/skinny/tranches/sk-v17/HANDOFF.md:148-185`
  (AZ-IV `:152-154`; StructRegistry `:155-156`; fact-stream diagnostic-only `:157-159`;
  FNV bench-only `:165`; brace-counter / wrong-plane `:167-169`; no-second-substrate
  `:171-174`; inherited REDRESS families `:177-178`).
- Benched-surface re-verification (this cycle):
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5-7`
  (`emit_fact_stream -> Result<String, CssFactError>`, `String::new()`);
  `skinny/crates/runtime/src/tape/assembler.rs:71-78` (`push_plain_offset`, branchless u32
  push into `offsets`, cold reserve only at capacity).
- Prior cycles: `…/research/p1/hardening/V1/CH3.md` (R-CH3-1), `…/V2/CH3.md` (41/41),
  `…/V3/CH3.md` (42/42).
- Artefacts dispositioned: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
  p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
  p1f-bench-canonical}.md` §4 (+ P1-C §2.5, P1-D §2.5, P1-F §3.2).
