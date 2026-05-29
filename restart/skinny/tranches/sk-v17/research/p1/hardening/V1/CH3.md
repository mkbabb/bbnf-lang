# SK-V17 S-P1 CHALLENGE — CH3 REGRESSION (V1)

Lens: CH3 REGRESSION. Pass: S-P1 Profile. Cycle: V1. Date: 2026-05-29.
Reviewer scope: `restart/skinny/tranches/sk-v17/research/p1/{p1a..p1f}.md`.
Authority: `PASS-1-PROFILE.md` §3 (CH3) + ORCHESTRATOR §3W/§3Z.
Baseline: master HEAD `6496fecae` (working tree).

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

The cited entry numbers are REDRESS global-item numbers (the prose-embedded
`item N` scheme, not the per-section list ordinals). Resolved:

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

### §0.2 — Benched-surface ground truth (verified, load-bearing for the honesty test)

`emit_fact_stream(input: &str) -> Result<String, CssFactError>` opens with
`String::new()` + `out.push_str(...)` (`generated.rs:5-8`); `track1_facts`
(`nonjson_css_l4.rs:596`) calls `track1::parser::parse` → this String emitter.
The sanctioned tape receiver `TapeBuilder::push_plain_offset` (`assembler.rs:71`)
is one branchless `u32` push into the EXISTING `offsets` vec — not a new builder
type. Verified directly this pass. Every artefact's "fact-stream is a `String`,
the tape append replaces it" claim is factually grounded; the de-fact-stream
target is named honestly across all six.

---

## §1 — Per-artefact disposition (path:line + concrete fix)

### P1-A `p1a-samply-mode-1.md` — §4 (lines 152-164)

- §4.1 (`:154`) recognition-plane masking — observation, ends "NOT a re-propose
  of any route; an observation." **ACCEPT.**
- §4.2 (`:156-157`) `find_component_delim` NEON antecedent — names it the
  grammar-neutral `find_ascii_set_member64` primitive shape and states "The NEON
  route is pre-blocked behind tape activation ... flagged, NOT re-opened." Gated
  per REDRESS 51/53's admissible boundary (scanner writes tape, not a parser-local
  second scanner). **ACCEPT.**
- §4.3 (`:159`) fact-stream allocation lever — names `emit_fact_stream` String
  growth as the de-fact-stream target, routes it to the EXISTING
  `assembler.rs:71 push_plain_offset`. `push_ascii_lower_hex` (FNV hex) correctly
  called diagnostic-only ("disappears ... when the fact-stream String is retired"),
  consistent with the FNV-bench-only pre-block. **ACCEPT.**
- §4.4 (`:161`) explicit: "not a re-open of REDRESS (the fact-stream-as-admission
  pre-block); it is the empirical case for retiring it." Honest de-fact-stream
  framing. **ACCEPT.**
- §4.5 (`:163`) Lock-1 no-second-substrate observation. **ACCEPT.**

Disposition P1-A §4: **ACCEPT** (5/5 sections). No silent re-open. The CH3
pre-block discipline is explicit on every flagged item.

### P1-B `p1b-samply-mode-2.md` — §4 (lines 242-292)

- §4.1 (`:244`) orthogonal-leaf masking — sequencing observation, no route.
  **ACCEPT.**
- §4.2 (`:256`) NEON antecedent re-confirmed, gated. **ACCEPT.**
- §4.3 (`:265`) `push_ascii_lower_hex` per-token `Vec::with_capacity` alloc —
  explicitly distinguished from AZ-IV eager-value-tree ("builds a hex String, not
  a `Box<CssColor>` tree"), framed as the per-token alloc the lazy `ValueRef`
  projection retires. The AZ-IV disambiguation is exactly the CH3 citation CH3
  wants. **ACCEPT.**
- §4.4 (`:274`) `core::fmt::num` overhead — observation. **ACCEPT.**
- §4.5 (`:278-285`) the dedicated CH3 check, naming the full pre-block list
  "REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89 / 127" and asserting "no second
  substrate, no sidecar, no registry, no eager value tree, no fixture/FNV
  contrivance, no x86." **ACCEPT.**
- §4.6 (`:287`) run-to-run variance — measurement note. **ACCEPT.**

Disposition P1-B §4: **ACCEPT** (6/6 sections). The §4.5 explicit pre-block
check is the model the other artefacts should match.

### P1-C `p1c-samply-mode-3.md` — §4 (lines 242-285)

- A1 (`:244`) plane bifurcation — observation. **ACCEPT.**
- A2 (`:255`) masking-probe read (recognition beats SOTA, wrong plane) — names
  the budget, no route. **ACCEPT.**
- A3 (`:265`) FNV in the hot path — "FNV is bench-only/diagnostic per the
  pre-blocked routes ... Flagged so S-P2 does not carry FNV into the tape emitter
  (REDRESS 'FNV stays bench-only')." Cites the pre-block. Matches HANDOFF
  `:165`. **ACCEPT.**
- A4 (`:270`) aggregate-min outlier — measurement honesty. **ACCEPT.**
- A5 (`:274`) zero NEON on CSS path — antecedent, gated behind tape. **ACCEPT.**
- Pre-blocked-route check (`:280-285`) — dedicated CH3 paragraph, names
  "REDRESS 50-55 / 60-72 / 80 / 82-84 / 88 / 89," asserts the fact-stream plane
  is measured as diagnostic only and not proposed for admission. **ACCEPT.**

Disposition P1-C §4: **ACCEPT** (6/6 sections + dedicated check).

### P1-D `p1d-pmu-cycles.md` — §2.5 (lines 244-254) + §4 (lines 298-357)

This is the **one artefact carrying a REVISE**, isolated to §2.5 / §4-item-4.

- §4.1 (`:300`) recognition-plane masking — observation, names the 300-600 Mbps
  feasibility band. **ACCEPT.**
- §4.2 (`:311`) NEON antecedent re-confirmed; routes to `select_classifier`;
  notes "CSS is the non-JSON exercise grammar for the kernel." Gated. **ACCEPT.**
- §4.3 (`:322`) **the strongest de-fact-stream-honesty item in the set.** It
  CORRECTS the inherited architecture-doc `emit_* ~34%` figure: on the
  recognition (`emit_full_parse`) plane the String emit is <0.2%, so "S-P2 should
  not over-index the String-emit lever" — while affirming "the contract's
  String-emit pre-block is still correct as a forward concern" once track1 emits a
  typed CSSOM. This is the honest naming the orchestrator focus demands: the
  fact-stream-String is the de-fact-stream target *on the fact-stream plane*, and
  it must NOT be mis-attributed to the recognition plane. **ACCEPT.**
- §4.4 (`:331`) — **REVISE.** "Redundant 2-3× overlapping scan
  (`parse_block_item`:211 → `find_colon_before`:219/:314 → `parse_declaration`:247)
  multiplies the dominant hot leaf. Single-pass tokenization over the structural
  index is a named S-P2 candidate." This (and its §2.5 antecedent, `:244-254`) is
  a structural-rewrite *suggestion* — "tokenize-once over the NEON-produced
  structural index, rather than three overlapping scalar walks" (`:253-254`). A
  "single-pass tokenize-once" that yields a consumable structural stream is
  precisely the boundary **REDRESS 51/53** governs: single-substrate tokenize
  where "the scanner writes the tape/event stream and generated lowering consumes
  that stream directly" is ADMISSIBLE (`REDRESS.md:807-813`), but a parser-local
  second scanner / retained cursor over source bytes is REJECTED (`:784-805`). The
  observation lands on the admissible side, but unlike every other NEON item in
  this artefact set, §2.5/§4-4 does NOT cite REDRESS 51/53 and does NOT mark the
  thin-ice boundary. Per CH3's literal rule ("must cite the entry and mark it
  pre-blocked"), an un-cited structural-rewrite suggestion adjacent to a rejected
  family is an implicit re-open risk.
  - **Concrete fix:** in `p1d-pmu-cycles.md:254` (end of §2.5) and `:333` (§4-4),
    append: "This single-pass tokenize-once is the REDRESS-53 ADMISSIBLE route
    (`REDRESS.md:807-813`: scanner writes the tape/event stream, lowering consumes
    it directly) — explicitly NOT the REDRESS-51/53 REJECTED parser-local second
    cursor over source bytes (`:742,784`). Gated behind tape activation; flagged as
    a structural target, not a route re-opening." This makes the disambiguation
    explicit rather than leaving it to the reader.
- §4.5 (`:336`) PMU c/B gap (CH4 cross-note) — measurement honesty. **ACCEPT.**
- §4.6 (`:344`) concurrency contamination — measurement honesty. **ACCEPT.**
- §4.7 (`:349`) zero SIMD on CSS path — "NOT a re-proposal of a REDRESS-blocked
  route; it is the gated-behind-tape W4 lever's empirical antecedent." **ACCEPT.**

Disposition P1-D: **REVISE** (1 section: §2.5 + §4-4). 6 of 7 §4 items ACCEPT;
the redundant-rescan / tokenize-once suggestion needs the explicit REDRESS 51/53
adjacency citation. Note: this is a citation-discipline REVISE, not a substantive
re-open — the route as described is admissible; it is the missing thin-ice marker
that fails the literal CH3 rule.

### P1-E `p1e-hot-leaf-attribution.md` — §4 (lines 262-313)

- §4.1 (`:264`) recognition-plane mask — observation, "MUST NOT be read as a
  >SOTA admission." **ACCEPT.**
- §4.2 (`:273`) fact-stream allocator floor → tape append; "NOT a re-proposal —
  it is the measured ground for the contract's own pre-declared route." Honest
  de-fact-stream naming. **ACCEPT.**
- §4.3 (`:282`) NEON antecedent re-confirmed; explicitly orders "lever-1/2 (tape)
  FIRST, then NEON on the surviving scan. S-P2 must not invert this." Gated.
  **ACCEPT.**
- §4.4 (`:293`) **honest orphan-block of the digit kernel (C4b)** — "the
  udot/i8mm digit kernel (`digit_mac.rs:27`, C4b) has no benched CSS antecedent ...
  C4b stays orphan-blocked on the current planes." This is the correct CH3
  handling of REDRESS 80/82 (numeric/string kernels rejected absent a fresh
  antecedent): it refuses to carry the kernel hypothesis without a benched hot
  leaf. **ACCEPT.**
- §4.5 (`:304`) outlier note. **ACCEPT.**
- §4.6 (`:307-313`) dedicated CH3 pre-block check: "Nothing in §4 re-proposes a
  REDRESS-blocked route," explicitly disclaiming AZ-IV eager-value-tree and
  StructRegistry indirection, and disclaiming brace-counter CSS admission for the
  recognition headroom. **ACCEPT.**

Disposition P1-E §4: **ACCEPT** (6/6 sections + dedicated check). The C4b
orphan-block (§4.4) is the strongest single CH3-correct handling in the set.

### P1-F `p1f-bench-canonical.md` — §4 (lines 305-356)

- §4.1 (`:307`) plane-dependent gap — observation. **ACCEPT.**
- §4.2 (`:314`) fact-stream 64% alloc → tape append on EXISTING
  `assembler.rs:71`; "(NOT a re-proposal — it is the contract's own lever,
  grounded here.)" Honest de-fact-stream naming. **ACCEPT.**
- §4.3 (`:322`) NEON antecedent re-confirmed, gated behind tape ("not a route
  re-opening"). **ACCEPT.**
- §4.4 (`:331`) `tape_activated = false` verified empirically — Lock-1 / no-second-
  substrate baseline. **ACCEPT.**
- §4.5 (`:338`) PMU `ri_cycles` unreliable — measurement honesty. **ACCEPT.**
- §4.6 (`:345`) no second substrate / no sidecar (Lock 1 / CH5) — observation.
  **ACCEPT.**
- §4.7 (`:352`) JSON 51/51 guard untouched. **ACCEPT.**

Also §3.2 (`:296-301`) classifies the CSS eager-typed plane as
**K (pre-blocked)** citing AZ-IV / SYNTHESIS §0.4 — a correct refusal to admit
the eager path. **ACCEPT.**

Disposition P1-F §4: **ACCEPT** (7/7 sections + §3.2 K-classification).

---

## §2 — Cross-cutting CH3 findings

1. **De-fact-stream honesty: HELD across all six.** Every artefact names the
   `emit_fact_stream` `String` (`generated.rs:5`) as the de-fact-stream target and
   routes its retirement to the EXISTING `TapeBuilder::push_plain_offset`
   (`assembler.rs:71`), never to a new `TapeStructBuilder`/`TapeCursor` (the
   HANDOFF `:171-174` no-second-substrate pre-block). The strongest framing is
   P1-D §4-3, which corrects the inherited ~34% emit figure and warns S-P2 not to
   mis-attribute the String lever to the recognition plane — this prevents a future
   wave from "retiring" a String cost that is not on the plane it claims. The
   fact-stream plane is uniformly measured as a **diagnostic** (HANDOFF `:157-159`),
   never re-proposed for admission.

2. **Tape lever is NOT a REDRESS 50-55/60-72 re-open.** The rejected SK-V5/SK-V6
   families (50-55, 60-72) are String-*sink* / *materializer* shapes (decoded-stats
   sink, quote-source fused hash, semantic-string-fact, byte-writing) that failed on
   measured JSON escaped-string regression. The SK-V17 lever is an offset-tape
   append on a different substrate (one `u32` push), explicitly distinguished from
   the eager-value-tree (AZ-IV) and from a String materializer. P1-B §4-3's AZ-IV
   disambiguation and P1-E §4-6's explicit disclaimer make this distinction load-
   bearing. No artefact proposes a String-sink materializer.

3. **NEON observation is gated per REDRESS 53's admissible boundary.** Five of six
   artefacts (P1-A/B/C/E/F) explicitly gate `find_component_delim` NEON behind tape
   activation ("no structural index to pre-scan into until the tape decodes CSS"),
   which is REDRESS 53's admissible single-substrate route (scanner writes tape,
   lowering consumes), NOT the rejected parser-local second scanner. The lone gap is
   P1-D §2.5/§4-4 (the REVISE) where the "tokenize-once" structural-rewrite
   suggestion omits the REDRESS 51/53 citation.

4. **No fresh-antecedent-free kernel carried (REDRESS 80/82/83/84/88/89).** P1-E
   §4-4 is exemplary: it refuses to carry the C4b digit kernel without a benched CSS
   hot-leaf antecedent (zero digit-parse self-time on either plane), honoring
   ORCHESTRATOR §8 profile-first and the REDRESS-80/82 rejected-kernel discipline.
   No artefact re-proposes a PMULL/CTZ bitmap consumer (88/89).

5. **No silent re-admission of the broadcast plane or brace-counter CSS.** Every
   artefact treats the recognition (`emit_full_parse`) plane's >lightningcss margin
   as a *masking signal* (wrong plane, no AST), never as an admit; P1-D §2.3,
   P1-E §4-1, P1-F §3.2 explicitly mark recognition-only / eager-typed as
   non-admissible. The 24-row broadcast is de-broadcast, not reproduced as admits.

---

## §3 — Counts + dispositions

| Artefact | §4 sections | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-A | 5 | 5 | 0 | 0 |
| P1-B | 6 | 6 | 0 | 0 |
| P1-C | 6 | 6 | 0 | 0 |
| P1-D | 7 (+§2.5) | 6 | 1 | 0 |
| P1-E | 6 | 6 | 0 | 0 |
| P1-F | 7 | 7 | 0 | 0 |
| **Total** | **37** | **36** | **1** | **0** |

**ACCEPT rate: 36/37 = 97.3%.**

REVISE list (1, non-orphan — concrete fix supplied):
- **R-CH3-1 — P1-D `p1d-pmu-cycles.md:254` (§2.5) + `:333` (§4-4):** the redundant
  2-3× rescan / "single-pass tokenize-once over the structural index" suggestion is
  adjacent to the REDRESS 51/53 rejected family (parser-local second scanner /
  retained cursor) but does not cite the entry or mark the admissible-vs-rejected
  boundary, as CH3 §3 literally requires. Fix: append the REDRESS-53-admissible /
  REDRESS-51/53-rejected disambiguation given in the P1-D disposition above. The
  route as described is admissible; this is a citation-discipline REVISE, not a
  substantive re-open.

REJECT list: none.

## §4 — Verdict

CH3 REGRESSION returns **ACCEPT 97.3% (36/37)**, **1 REVISE (R-CH3-1, non-orphan,
fix supplied)**, **0 REJECT**. The fact-stream-String cost is named honestly as
the de-fact-stream target across all six artefacts; the tape lever and NEON
observation do not re-open REDRESS 50-55/60-72/80/82-84/88/89; the single REVISE
is a missing thin-ice citation on an otherwise-admissible P1-D structural-rewrite
suggestion. Above the §3Z 95% threshold for this lens; R-CH3-1 must fold into V2
(append two citations) to clear the zero-orphan-REVISE convergence condition.

## §5 — Sources

- Pass contract: `restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH3 `:137-141`).
- ORCHESTRATOR: `restart/prompts/ORCHESTRATOR.md` §3W (`:85` CH3), §3Z (`:104-128`).
- REDRESS families: `skinny/REDRESS.md` items 50 (`:715`), 51 (`:742`), 53 (`:784`,
  admissible route `:807-813`), 54 (`:815`), 55 (`:846`), 60-72 (`:1831,1881`),
  80 (`:2217`), 82-84 (`:2287,2320,2360`), 88 (`:2510`), 89 (`:2544`).
- HANDOFF pre-blocks: `restart/skinny/tranches/sk-v17/HANDOFF.md:148-185`
  (no-second-substrate `:171-174`; fact-stream diagnostic-only `:157-159`;
  inherited families `:176-177`).
- Benched-surface verification: `generated.rs:5` (`emit_fact_stream -> Result<String>`),
  `nonjson_css_l4.rs:596` (`track1_facts`), `assembler.rs:71` (`push_plain_offset`).
- Artefacts dispositioned: `restart/skinny/tranches/sk-v17/research/p1/{p1a-samply-mode-1,
  p1b-samply-mode-2,p1c-samply-mode-3,p1d-pmu-cycles,p1e-hot-leaf-attribution,
  p1f-bench-canonical}.md`.
