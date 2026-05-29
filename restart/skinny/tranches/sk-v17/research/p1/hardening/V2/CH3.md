# SK-V17 S-P1 CHALLENGE — CH3 REGRESSION (V2)

Lens: CH3 REGRESSION. Pass: S-P1 Profile. Cycle: V2. Date: 2026-05-29.
Reviewer scope: `restart/skinny/tranches/sk-v17/research/p1/{p1a..p1f}.md`.
Authority: `PASS-1-PROFILE.md` §3 (CH3) + ORCHESTRATOR §3W/§3Z.
Baseline: master HEAD `6496fecae` (working tree).
Prior cycle: V1 CH3 returned 36/37 ACCEPT (97.3%), 1 REVISE (R-CH3-1, P1-D), 0 REJECT.

## §0 — CH3 mandate (what this lens dispositions)

Per `PASS-1-PROFILE.md` §3 CH3: "does any anomaly flagged in §4 silently
re-propose a route already in `skinny/REDRESS.md`? S-P1 proposes nothing,
but a §4 'this hot leaf suggests X' that points at a pre-blocked route
(REDRESS 50-55, 60-72, 80, 82-84, 88, 89) must cite the entry and mark it
pre-blocked, not implicitly re-open it." Plus the orchestrator focus: the
**fact-stream-String cost must be named honestly as the de-fact-stream
target**, not laundered into a re-admission of the pre-blocked
`emit_fact_stream` plane.

### §0.1 — The pre-blocked route families (resolved against REDRESS.md + HANDOFF.md)

Carried forward unchanged from V1 (the route ledger is stable; only the
artefacts changed):

| Family | REDRESS items | Route REJECTED | The thin-ice adjacency for S-P1 |
|---|---|---|---|
| Retained projection / parse-time aux side table | 50 | dense/sparse per-cursor aux columns on the offset tape (`REDRESS.md:715`) | a tape lever that writes aux columns at parse time |
| Event-cursor / parser-local second scanner | 51, 53 | `JsonEventCursor` / `JsonStructuralCursor` — a second structural scan bolted onto recursive descent (`REDRESS.md:742,784`); ADMISSIBLE route = "scanner writes the tape/event stream and generated lowering consumes that stream directly" (`:807-813`) | a NEON pre-scan or "tokenize-once" framed as a parser-owned cursor over source bytes |
| Decoded-string stats sink / quote-source fused materializer | 54, 55 | sink-local exact-stats and one-pass streaming-hash String materializers (`REDRESS.md:815,846`) | a tape lever framed as a String/sink materializer |
| Direct-string alloc / receiver / byte-writing / semantic-string-fact | 60-72 | the SK-V6 direct-materialization family (`REDRESS.md:1831,1881`) | retiring the fact-stream String by replacing it with another eager String/fact emitter |
| Mantissa-widen / Unicode-quartet / StringBlock16 / value-byte compaction | 80, 82, 83, 84 (`REDRESS.md:2217,2287,2320,2360`) | numeric + string micro-kernels rejected on measured regression | a CSS digit/string kernel proposed without a fresh benched antecedent |
| PMULL prefix-XOR / CTZ bulk consumer | 88, 89 (`REDRESS.md:2510,2544`) | consumed-bitmap-body SIMD shapes rejected | a NEON bitmap-consumer proposed as a route, not a gated antecedent |

HANDOFF `Pre-Blocked Routes` (`HANDOFF.md:148-185`) additionally binds:
the `emit_fact_stream` fact-stream plane is **diagnostic-only** (`:157-159`);
**no second substrate** — no new `StructLayout`/`TapeStructBuilder`/`TapeCursor`
alongside the landed `Tape`/`ValueRef` (`:171-174`); FNV stays bench-only
(`:165`); brace-counter CSS admission is pre-blocked (`:168`).

### §0.2 — Benched-surface ground truth (RE-VERIFIED this cycle)

Re-verified directly against the working tree this cycle (not inherited from V1):

- `emit_fact_stream(input: &str) -> Result<String, CssFactError>` opens with
  `let mut out = String::new();` then `out.push_str(config::FACT_SCHEMA)`
  (`generated.rs:5-7`) — the un-presized String fact-stream the de-fact-stream
  target retires. **Confirmed.**
- `TapeBuilder::push_plain_offset(&mut self, offset: usize) -> u32`
  (`assembler.rs:71`) is one bounds-checked `u32` push into the EXISTING
  `self.offsets` vec (cold reserve only at capacity) — NOT a new builder type,
  honouring the HANDOFF `:171-174` no-second-substrate pre-block. **Confirmed.**
- REDRESS admissible route `:807-813` reads verbatim: "structural projection
  must be the parser's single substrate, not a second scanner bolted onto
  source-byte recursive descent. Either the scanner writes the tape/event stream
  and generated lowering consumes that stream directly, or a `CollapsedStage` /
  `SinkOnly` lowering consumes live masks in the same loop. A `ParserState`-owned
  structural cursor over source bytes is non-canonical." **Confirmed** — this is
  exactly the boundary P1-D now cites.
- REDRESS item 53 `:784` reads "SK-V5 structural-mask parser-local cursor is
  REJECTED." **Confirmed.**

Every artefact's "fact-stream is a `String`, the tape append replaces it" claim
is factually grounded; the de-fact-stream target is named honestly across all
six. No artefact proposes a String-sink materializer, an aux side-table, an eager
value tree, or a parser-local second cursor.

### §0.3 — V1 REVISE fold status (the load-bearing V2 check)

V1 CH3's lone REVISE, **R-CH3-1**, asked that P1-D's redundant-rescan /
"single-pass tokenize-once over the structural index" suggestion (§2.5 + §4-4)
cite REDRESS 51/53 and mark the admissible-vs-rejected boundary explicitly. The
fold is **CONFIRMED COMPLETE and substantively correct**, not paper:

- **P1-D §2.5 (`p1d-pmu-cycles.md:323-334`)** now carries a dedicated paragraph
  headed **"REDRESS 51/53 boundary (citation discipline; CH3 §3)"** that states
  the single-pass route is admissible ONLY as the REDRESS-53 single-substrate
  shape (`REDRESS.md:807-813`, "scanner writes the tape/event stream and generated
  lowering consumes that stream directly") and REJECTED as a parser-local SECOND
  scanner / retained `ParserState`-owned cursor over source bytes
  (`REDRESS.md:784-805, 807-813`), naming that item 53 measured that shape as a
  regression. It closes "S-P1 proposes nothing; this is flagged as the
  admissible-vs-rejected boundary."
- **P1-D §4-4 (`:487-495`)** now appends inline: "admissible ONLY as REDRESS-53's
  'scanner writes the tape/event stream, generated lowering consumes it directly'
  single-substrate shape (`REDRESS.md:807-813`); REJECTED as the REDRESS-51/53
  parser-local second scanner / retained `ParserState` cursor over source bytes
  (`REDRESS.md:784-805`), which item 53 measured as a regression. Flagged with the
  boundary, not re-opened."

Both citations resolve to the verbatim REDRESS lines verified in §0.2. The fix is
exactly the disambiguation V1 specified, not a cosmetic gesture. **R-CH3-1
RESOLVED — zero orphan REVISE carried into V2.**

---

## §1 — Per-artefact disposition (path:line + concrete fix)

### P1-A `p1a-samply-mode-1.md` — §4 (lines 172-184)

- §4.1 (`:174`) recognition-plane masking — names the honest target as living
  BETWEEN track1_full (2395, no AST) and track1_fact (653, String), ends "NOT a
  re-propose of any route; an observation." **ACCEPT.**
- §4.2 (`:176-177`) `find_component_delim` NEON antecedent — names it the
  grammar-neutral `find_ascii_set_member64` / `byte_class_index_64` primitive
  (CH2 generic shape), routes to `select_classifier` (`dispatch.rs:42`), states
  "pre-blocked behind tape activation ... flagged, NOT re-opened." Gated per the
  REDRESS 51/53 admissible boundary. **ACCEPT.**
- §4.3 (`:179`) fact-stream allocation lever — names `emit_fact_stream` String
  growth (`RawVecInner::reserve`) as the de-fact-stream target, routes it to the
  EXISTING `assembler.rs:71 push_plain_offset`. `push_ascii_lower_hex` (FNV hex)
  correctly called FNV-diagnostic-only ("disappears entirely when the fact-stream
  String is retired ... must not be carried into S-P2 as a primitive"), matching
  the FNV-bench-only pre-block (HANDOFF `:165`). **ACCEPT.**
- §4.4 (`:181`) explicit: "not a re-open of REDRESS (the fact-stream-as-admission
  pre-block); it is the empirical case for retiring it." Honest de-fact-stream
  framing. **ACCEPT.**
- §4.5 (`:183`) Lock-1 no-second-substrate / no-sidecar observation; notes the
  substrate is UNWIRED (no tape on CSS path). **ACCEPT.**

Disposition P1-A §4: **ACCEPT** (5/5). No silent re-open.

### P1-B `p1b-samply-mode-2.md` — §4 (lines 306-367)

- §4.1 (`:308`) orthogonal-leaf lever-sequencing observation — names the MASKING
  signal (scan masked under allocation on the fact-stream plane), no route.
  **ACCEPT.**
- §4.2 (`:320`) NEON antecedent re-confirmed on benched path (56.55%/11.51%),
  gated; notes post-W2 typed plane must be re-profiled. **ACCEPT.**
- §4.3 (`:329`) `push_ascii_lower_hex` per-token alloc + diagnostic encode —
  explicitly distinguished from AZ-IV eager-value-tree ("builds a hex String, not
  a `Box<CssColor>` tree") and named "the tape retires WHOLESALE (not merely
  lazily) — its ~9% fact-stream self-time is pure diagnostic tax, removed not
  deferred." The AZ-IV disambiguation is the exact CH3 citation. **ACCEPT.**
- §4.4 (`:343`) `core::fmt::num` overhead — observation, tape stores raw `u32`.
  **ACCEPT.**
- §4.5 (`:347-354`) the dedicated CH3 check — names the full pre-block list
  "REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89 / 127" and asserts "no second
  substrate, no sidecar, no registry, no eager value tree, no fixture/FNV
  contrivance, no x86." **ACCEPT.**
- §4.6 (`:356`) harness-dependent absolute Mbps — measurement note. **ACCEPT.**

Disposition P1-B §4: **ACCEPT** (6/6). The §4.5 explicit pre-block check remains
the model the set follows.

### P1-C `p1c-samply-mode-3.md` — §4 (lines 338-389)

- A1 (`:340`) plane bifurcation — observation, disjoint hot-leaf sets named.
  **ACCEPT.**
- A2 (`:352`) masking-probe read — names materialization (not scanning) as the
  structural inefficiency, sizes the ~80-180 i/B headroom, no route. **ACCEPT.**
- A3 (`:362`) FNV in the hot path — "FNV and the hex encoder are FNV-diagnostic
  primitives with NO CSS-semantic value ... Flagged so S-P2 does not carry FNV/hex
  -encode into the tape emitter (REDRESS 'FNV stays bench-only')." Cites the
  pre-block; matches HANDOFF `:165`. **ACCEPT.**
- A4 (`:369`) cold first-touch min outliers — measurement honesty
  (`no-warm-benches`). **ACCEPT.**
- A5 (`:375`) zero NEON on CSS path — antecedent, gated behind tape ("no
  structural index until the tape decodes CSS"). **ACCEPT.**
- Pre-blocked-route check (`:384-389`) — dedicated CH3 paragraph: "§4 proposes
  nothing," names "REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89," and states the
  fact-stream plane is measured as diagnostic only, "does NOT propose admitting
  it." **ACCEPT.**

Disposition P1-C §4: **ACCEPT** (5 items + dedicated check = 6/6).

### P1-D `p1d-pmu-cycles.md` — §2.5 (lines 311-334) + §4 (lines 449-526)

The V1 REVISE artefact. The fix folded (§0.3); re-dispositioned fresh below.

- §2.5 (`:311-334`) redundant overlapping re-scan + **the REDRESS 51/53 boundary
  paragraph** (`:323-334`). The structural-rewrite suggestion now carries the
  explicit admissible-vs-rejected disambiguation V1 demanded: admissible ONLY as
  the REDRESS-53 single-substrate shape (`:807-813`), REJECTED as the parser-local
  cursor (item 53, `:784-805`). Both lines re-verified verbatim (§0.2). The
  thin-ice marker is now present, not left to the reader. **ACCEPT** (was the V1
  REVISE).
- §4.1 (`:451`) recognition-plane masking — names the honest 300-600 Mbps
  feasibility band; "the eager floor (~3) is what lazy projection must NOT regress
  to." Observation. **ACCEPT.**
- §4.2 (`:462`) NEON antecedent re-confirmed (79.6%/15.7% on this run), routes to
  `byte_class_index_64` over `select_classifier`. Gated. **ACCEPT.**
- §4.3 (`:473`) **the strongest de-fact-stream-honesty item in the set.** It
  CORRECTS the inherited architecture-doc `emit_* ~34%` figure: on the recognition
  (`emit_full_parse`) plane the String emit is <0.1%, so "S-P2 should NOT
  over-index the String lever *on the recognition plane*" — while affirming "the
  String-emit pre-block remains a correct forward concern" because the
  `emit_fact_stream` plane DOES pay the full 217-370 i/B String tax. This is the
  honest naming the orchestrator focus demands: the fact-stream-String is the
  de-fact-stream target *on the fact-stream plane*, and must NOT be mis-attributed
  to the recognition plane (preventing a future wave from "retiring" a cost that
  is not on the plane it claims). **ACCEPT.**
- §4.4 (`:487-495`) redundant 2-3× overlapping scan + tokenize-once candidate
  **now with the inline REDRESS-53-admissible / REDRESS-51/53-rejected boundary
  citation** (the V1 fix). "Flagged with the boundary, not re-opened." The route
  as described lands on the admissible single-substrate side and the thin-ice
  marker is explicit. **ACCEPT** (was the V1 REVISE).
- §4.5 (`:497`) PMU c/B now measured (V1 gap closed); corrects the prior
  "ri_cycles unreliable" line to "high IPC on a wide core." Measurement honesty.
  **ACCEPT.**
- §4.6 (`:506`) per-line attribution now artefact-backed (V1 gap closed).
  Measurement honesty. **ACCEPT.**
- §4.7 (`:512`) zero SIMD on CSS path — "the gated-behind-tape W4 lever's
  empirical antecedent; NOT a REDRESS-blocked re-proposal." Names `digit_mac.rs:27
  parse_4_digits_dotprod` udot orphan as never reached — implicitly honouring the
  REDRESS 80/82 no-free-kernel discipline (no digit kernel carried without a
  benched antecedent). **ACCEPT.**
- §4.8 (`:521`) harness convergence (X2) — measurement / reproducibility.
  **ACCEPT.**

Disposition P1-D: **ACCEPT** (§2.5 + 8/8 §4 items). The V1 REVISE folded; no new
re-open. P1-D §4-3 is the load-bearing de-fact-stream-honesty correction for the
whole pass.

### P1-E `p1e-hot-leaf-attribution.md` — §4 (lines 296-349)

- §4.1 (`:298`) recognition-plane mask — "MUST NOT be read as a >SOTA admission
  (it fails preserve-rich-ast)." **ACCEPT.**
- §4.2 (`:307`) fact-stream allocator floor → tape append on EXISTING
  `assembler.rs:71` "one branchless u32 write"; "NOT a re-proposal — it is the
  measured ground for the contract's own pre-declared route." Honest de-fact-stream
  naming, no second substrate. **ACCEPT.**
- §4.3 (`:316`) NEON antecedent re-confirmed; explicitly orders "lever-1/2 (tape)
  FIRST, then NEON on the surviving scan. S-P2 must not invert this." Gated.
  **ACCEPT.**
- §4.4 (`:329`) **honest orphan-block of the C4b digit kernel** — "the udot/i8mm
  digit kernel (`digit_mac.rs:27`, C4b) has no benched CSS antecedent ... C4b stays
  orphan-blocked on the current planes ... S-P2 must re-profile the typed path
  after W1/W2, not inherit a CSS digit-kernel hypothesis from here." This is the
  exemplary CH3 handling of REDRESS 80/82 (kernels rejected absent a fresh
  antecedent) and ORCHESTRATOR §8 profile-first. **ACCEPT.**
- §4.5 (`:340`) material lightningcss min outlier — measurement honesty (CH6).
  **ACCEPT.**
- §4.6 (`:343-349`) dedicated CH3 pre-block check: "Nothing in §4 re-proposes a
  REDRESS-blocked route," explicitly disclaiming AZ-IV eager-value-tree, single
  non-generic `TapeBuilder` (no StructRegistry indirection), and brace-counter CSS
  admission (`SYNTHESIS.md §0.4`). **ACCEPT.**

Disposition P1-E §4: **ACCEPT** (6/6 + dedicated check). The C4b orphan-block
(§4.4) remains the strongest single CH3-correct handling in the set.

### P1-F `p1f-bench-canonical.md` — §4 (lines 449-514)

- §4.1 (`:451`) plane-dependent gap (not flat 14×) — observation. **ACCEPT.**
- §4.2 (`:459`) fact-stream 64% alloc + 4.4× instr/byte → tape append on EXISTING
  `assembler.rs:71`; "(NOT a re-proposal — it is the contract's own lever,
  grounded here.)" Honest de-fact-stream naming. **ACCEPT.**
- §4.3 (`:469`) NEON antecedent re-confirmed (59%/10%), gated behind tape ("not a
  route re-opening"). **ACCEPT.**
- §4.4 (`:479`) `tape_activated = false` verified empirically (grep returns zero
  tape symbols on CSS path) — Lock-1 / no-second-substrate baseline. **ACCEPT.**
- §4.5 (`:486`) PMU `ri_cycles` unreliable on rusage — measurement honesty;
  defers c/B gate to xctrace. (Note: P1-D §4-5 partially re-validates ri_cycles as
  high-IPC; the divergence is a CH4/CH7 measurement-reconciliation item, NOT a CH3
  re-open — neither framing proposes a blocked route.) **ACCEPT.**
- §4.6 (`:495`) no second substrate / no sidecar (Lock 1 / CH5) — the 26.74%
  wrapper bucket is named as PURE measurement scaffold, "not a retained or second
  pass." Track 1 and comparators keep separate symbol paths. **ACCEPT.**
- §4.7 (`:505`) JSON 51/51 guard untouched — read-only except the bench bin.
  **ACCEPT.**
- §4.8 (`:510`) X2 harness convergence — reproducibility. **ACCEPT.**

§3.2 K-classification (CSS eager-typed plane = pre-blocked, citing AZ-IV /
SYNTHESIS §0.4) — a correct refusal to admit the eager path, re-verified present.
**ACCEPT.**

Disposition P1-F §4: **ACCEPT** (8/8 + §3.2 K-classification).

---

## §2 — Cross-cutting CH3 findings

1. **De-fact-stream honesty: HELD and SHARPENED across all six.** Every artefact
   names the `emit_fact_stream` `String` (`generated.rs:5`, re-verified) as the
   de-fact-stream target and routes its retirement to the EXISTING
   `TapeBuilder::push_plain_offset` (`assembler.rs:71`, re-verified branchless u32
   push), never to a new `TapeStructBuilder`/`TapeCursor` (HANDOFF `:171-174`
   no-second-substrate). The strongest framing is **P1-D §4-3**, which corrects the
   inherited ~34% emit figure and warns S-P2 not to mis-attribute the String lever
   to the recognition plane — preventing a future wave from "retiring" a String
   cost that is not on the plane it claims. The fact-stream plane is uniformly
   measured as a **diagnostic** (HANDOFF `:157-159`), never re-proposed for
   admission. This is the orchestrator focus, and it is met.

2. **Tape lever is NOT a REDRESS 50-55/60-72 re-open.** The rejected SK-V5/SK-V6
   families (50-55, 60-72) are String-*sink* / *materializer* shapes (decoded-stats
   sink, quote-source fused hash, semantic-string-fact, byte-writing) that failed on
   measured JSON escaped-string regression. The SK-V17 lever is an offset-tape
   append on a different substrate (one `u32` push), explicitly distinguished from
   the eager-value-tree (AZ-IV) and from a String materializer. P1-B §4-3's AZ-IV
   disambiguation and P1-E §4-6's explicit disclaimer make this load-bearing. No
   artefact proposes a String-sink materializer or an aux side-table (REDRESS 50).

3. **NEON observation is gated per REDRESS 53's admissible boundary — and the V1
   gap is now closed.** All six artefacts gate `find_component_delim` NEON behind
   tape activation ("no structural index to pre-scan into until the tape decodes
   CSS"), REDRESS 53's admissible single-substrate route. The V1 REVISE — P1-D's
   tokenize-once structural-rewrite suggestion missing the REDRESS 51/53 citation —
   is **RESOLVED**: P1-D §2.5 (`:323-334`) and §4-4 (`:487-495`) both now carry the
   verbatim admissible (`:807-813`) vs rejected (item 53, `:784-805`) boundary. The
   set is now uniform: every NEON / tokenize-once mention marks the thin ice.

4. **No fresh-antecedent-free kernel carried (REDRESS 80/82/83/84/88/89).** P1-E
   §4-4 is exemplary: it refuses to carry the C4b digit kernel without a benched CSS
   hot-leaf antecedent (zero digit-parse self-time on either plane) and explicitly
   defers re-profiling to the post-W2 typed path. P1-D §4-7 names the
   `digit_mac.rs:27` udot orphan as never reached. No artefact re-proposes a
   PMULL/CTZ bitmap consumer (88/89).

5. **No silent re-admission of the broadcast plane or brace-counter CSS.** Every
   artefact treats the recognition (`emit_full_parse`) plane's >lightningcss margin
   as a *masking signal* (wrong plane, no AST, fails preserve-rich-ast), never as an
   admit; P1-E §4-1, P1-F §3.2 (K-classification), P1-A §4-1 explicitly mark
   recognition-only / eager-typed as non-admissible. The 24-row broadcast is
   de-broadcast into per-corpus medians, not reproduced as admits.

6. **One measurement divergence noted, NOT a CH3 finding.** P1-D §4-5 re-validates
   `ri_cycles` as high-IPC; P1-F §4-5 / P1-A / P1-B / P1-C struck it as unreliable.
   This c/B-counter reconciliation is a CH4/CH7 item (which posture the pass adopts)
   — neither framing proposes or re-opens a REDRESS route, so it is out of CH3
   scope. Flagged here only so the consolidator does not mis-route it to CH3.

---

## §3 — Counts + dispositions

| Artefact | §4 sections | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-A | 5 | 5 | 0 | 0 |
| P1-B | 6 | 6 | 0 | 0 |
| P1-C | 6 | 6 | 0 | 0 |
| P1-D | 9 (§2.5 + 8 §4) | 9 | 0 | 0 |
| P1-E | 6 | 6 | 0 | 0 |
| P1-F | 9 (8 §4 + §3.2) | 9 | 0 | 0 |
| **Total** | **41** | **41** | **0** | **0** |

**ACCEPT rate: 41/41 = 100%.**

REVISE list: **none.** (V1's R-CH3-1 folded completely; §0.3 confirms the fix is
substantive, not paper.)

REJECT list: none.

## §4 — Verdict

CH3 REGRESSION returns **ACCEPT 100% (41/41)**, **0 REVISE**, **0 REJECT**. The
fact-stream-String cost is named honestly as the de-fact-stream target across all
six artefacts — the orchestrator focus is met, and P1-D §4-3 sharpens it by
correcting a plane mis-attribution that would have let a future wave "retire" a
cost on the wrong plane. The tape lever and NEON observation do not re-open
REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89; the V1 REVISE (R-CH3-1, P1-D's
missing REDRESS 51/53 thin-ice citation) is **RESOLVED** — both §2.5 and §4-4 now
carry the verbatim admissible-vs-rejected boundary, re-verified against the
REDRESS source lines. Zero orphan REVISE. Above the §3Z 95% threshold; combined
with V1's 97.3%, CH3 has now returned ≥95% for two consecutive cycles with zero
open REVISE — the per-lens convergence condition for CH3 is met.

## §5 — Sources

- Pass contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH3 `:137-141`).
- ORCHESTRATOR: `restart/prompts/ORCHESTRATOR.md` §3W (CH3), §3Z (convergence).
- REDRESS families (re-verified verbatim this cycle): `skinny/REDRESS.md` items 50
  (`:715`), 51 (`:742`), 53 (`:784` "parser-local cursor REJECTED", admissible route
  `:807-813` "scanner writes the tape/event stream ... A `ParserState`-owned
  structural cursor over source bytes is non-canonical"), 54 (`:815`), 55 (`:846`),
  60-72 (`:1831,1881`), 80 (`:2217`), 82-84 (`:2287,2320,2360`), 88 (`:2510`),
  89 (`:2544`).
- HANDOFF pre-blocks: `restart/skinny/tranches/sk-v17/HANDOFF.md:148-185`
  (no-second-substrate `:171-174`; fact-stream diagnostic-only `:157-159`; FNV
  bench-only `:165`; brace-counter CSS `:168`).
- Benched-surface re-verification (this cycle): `generated.rs:5-7`
  (`emit_fact_stream -> Result<String, CssFactError>`, `String::new()`);
  `assembler.rs:71-78` (`push_plain_offset`, branchless u32 push into `offsets`);
  `nonjson_css_l4.rs:596` (`track1_facts`).
- V1 prior cycle: `restart/skinny/tranches/sk-v17/research/p1/hardening/V1/CH3.md`
  (R-CH3-1 disposition).
- Artefacts dispositioned: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
  p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
  p1f-bench-canonical}.md` §4 (+ P1-D §2.5, P1-F §3.2).
