# CH3 REGRESSION — Pass Alpha SK-V17 Hardening (cycle V2)

Lens: CH3 (PASS-ALPHA §3 — does any proposed intervention re-open a REDRESS pre-block?
Cross-check the αE shortlist against entries 1-N. Has αC correctly identified the
pre-block list?). Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25`
(`git rev-parse --short HEAD` confirmed this cycle).

Reviewer focus (verbatim mandate): no candidate re-opens a REDRESS pre-block (AZ-IV
eager materialization, StructRegistry indirection, fact-stream, broadcast, FNV, x86);
αC pre-block list correct.

Subjects reviewed: `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`.
**`SYNTHESIS.md` + `HANDOFF.md` (the α-F deliverable) do NOT yet exist in this cycle's
tranche directory** (verified: `research/alpha/` holds A..E only; no `alphaF.md`, no
`SYNTHESIS.md`, no `HANDOFF.md` at `sk-v17/`). The V1 CH3 reviewed a SYNTHESIS/HANDOFF
that is not present at the V2 dispatch surface — so the V1 SYNTHESIS §0.1/§0.3 REVISEs
(core-tree path attribution in the close-condition / receiver gates) are **carried as
OPEN obligations onto the α-F authoring step**, not re-dispositioned here (no file to
disposition). They are restated in §Carry-forward below so α-F inherits them.

## §0 — V1→V2 fold verification (the six V1 REVISEs all shared one root cause)

V1 CH3 returned ACCEPT 28 / REVISE 6 / REJECT 0. All six REVISEs shared one root cause:
**core-tree path attribution** (`crates/core/...`, `StructLayout`, `OpenFrame`, `CssArena`,
`css_l4/builder.rs`) where the benched surface is the skinny tree. αE alone had authored
the fix (αE:37-51). V1 mandated propagating that translation note into αC §2/§7, αD
V6/O1/O2, and SYNTHESIS §0.1/§0.3. V2 fold status, each verified against the live tree:

| V1 REVISE | V2 status | Verification |
|---|---|---|
| αC §2 (core-tree `StructLayout`/`builder.rs:274` admission) | **FOLDED** | αC now opens with a load-bearing TREE-DISAMBIGUATION header (`alphaC.md:18-35`) translating every doc symbol to the skinny surface; §2b re-keyed to `BackendRule`/`LayoutFacts` (`ir/cost.rs:119-121,259-271`) + `lower/{tape_plan,offset_tape,event_tape}.rs`; explicitly cites Lock 2 RETIRED-`StructLayout` (LOCKS.md:160). |
| αC §7 ledger row 2b | **FOLDED** | `alphaC.md:340` row 2b re-open test now reads "emitter hardcodes a per-grammar profile/route table (`W5C_REQUEST_FACT_PROFILES`, the 7 `RequestFacts` registrations)"; admission keyed to `LayoutFacts.backend_shape` (`ir/cost.rs`) via the skinny lowering. |
| αC §5 residual (`scratch sizes from input.len() + StructLayout`) — V1 minor | **FOLDED** | `alphaC.md:344` row 5b now reads "scratch sizes from input.len() + `BackendRule`/`LayoutFacts`, grammar-general". The only residual `StructLayout` tokens in αC (lines 20,22,29,82,120,122,365,370) are all inside the disambiguation header / explicit "this symbol does NOT exist, Lock 2 retired it" corrections — correct usage. |
| αD §1 V6 (substrate at core-tree `record/arena/cursor`) | **FOLDED** | αD now carries a §0 benched-surface disambiguation table (`alphaD.md:19-44`); V6 (`alphaD.md:76`) re-pathed to `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs,offsets.rs,event_grammar.rs}` with the explicit "NOT the doc's core-tree record/arena/cursor siblings" caveat and a grep-clean no-StructRegistry assertion on the measured tree. |
| αD §3 O1/O2 (core-tree `css_l4/builder.rs`/`CssArena`/`emit_builder`) | **FOLDED** | O1/O2 (`alphaD.md:144-145`) owner paths re-keyed to `skinny/xtask/src/regen_css.rs`, `skinny/crates/runtime/src/grammars/css_l4_*/`, `skinny/crates/runtime/src/tape/`, with the eager-arena/`Box<CssColor>` pathology correctly attributed to the TOTALITY tree (the conversion-report citation kept as evidence, not as a skinny owner path). |
| SYNTHESIS §0.1 / §0.3 (core-tree deletion/wiring gates) | **CARRIED FORWARD (no file)** | SYNTHESIS.md does not exist this cycle. Obligation restated in §Carry-forward so α-F authors §0.1/§0.3 against the skinny tree from the start. |

**Five of six V1 REVISEs are CLOSED in the live V2 artefacts.** The sixth (SYNTHESIS)
has no file to close against and becomes an explicit α-F authoring constraint. **Zero
orphan REVISE from V1.**

## §1 — Independent verification performed this cycle (every disposition is grounded)

Re-greped the skinny benched tree at HEAD `1c5bd7a25`; every load-bearing claim in αC/αD/αE
re-verified, not inherited:

- **Core-tree symbols grep-clean-absent in skinny** (the whole disambiguation rests on this):
  `StructLayout`, `OpenFrame`, `CssArena`, `TapeStructBuilder`, `begin_compound`, `TapeCursor`
  → **0 files** each across `skinny/crates/` + `skinny/xtask/`. **Confirmed.**
- **`W5C_REQUEST_FACT_PROFILES`** const at `skinny/crates/codegen/src/lib.rs:336` (decl), iterated
  at `:299,:567,:611`. **Confirmed** (αC/αE/αD cite `:336` exactly).
- **7 `RuntimeEmitterKind::RequestFacts` registrations** in `skinny/xtask/src/regen_css.rs` at lines
  **45,63,81,99,117,135,153** — **exactly the lines αC §0/§3 cites.** Confirmed.
- **Fixture parse-fn count** `grep -c 'fn parse_' skinny/crates/bbnf-bench/src/generated_real_typed.rs`
  = **148** (NOT the doc's stale 187). αC §5 / αE / αD corrected this throughout. **Confirmed.**
- **i8mm grep-clean-absent** from skinny (`grep -rn i8mm skinny/crates/` → 0). αE C4b "NET-NEW kernel"
  framing is correct. **Confirmed.**
- **`parse_4_digits_dotprod` orphan**: defined `aarch64/digit_mac.rs:27`, udot asm `:40`, sdot `:63`,
  dotprod path compile-time `target_feature`-gated (`:10`), scalar twin `:15-22`; **grep for callers
  outside digit_mac.rs returns empty** → confirmed never called in prod. αE C4a "wire the orphan"
  framing is exact. **Confirmed.**
- **Benched CSS Track 1 is a String**: `track1_facts(input) -> Result<String,String>`
  (`nonjson_css_l4.rs:596`) + 6 sibling `*_track1_facts -> Result<String,String>` at `:600-620`;
  `emit_fact_stream() -> Result<String, CssFactError>` (`css_l4_declaration_values/generated.rs:5`),
  `emit_full_parse` `:61`. **Confirmed** (αC §3, αD V6/O1, αE C0 anchor all exact).
- **24-row broadcast**: `skinny/RESULTS.md` carries **24** `AUDIT-FALSIFIED` rows and **24**
  `SK-V15-W0-broadcast-diagnostic` markers. αC §4 says "6 falsified tuples still present" and the §7
  ledger row 4 says "6 falsified rows" — see §4 disposition (minor count nuance). The broadcast
  PERMANENT-PRE-BLOCK verdict is correct regardless. **Confirmed present, count nuance flagged.**
- **7 css_l4 runtime grammar dirs** exist (`css_l4_{at_rules_and_media,declaration_values,
  declaration_values_extended,nested_layout,stylesheet_selectors,vendor_and_custom_atrules,
  visual_functions}`) — the αC §1 re-open-test grep target `runtime/src/grammars/css_l4_*/` is real.
  **Confirmed.**
- **LOCKS anchors** re-read line-by-line: Lock 1 (`LOCKS.md:75` substrate-union + Vec<OpenFrame>::clone
  86.07% + 2026-05-04 no-rename amendment; `:585` fact-stream string-only rejected, no second tape, no
  cross-call classifier state), Lock 2 (`:160` — "StructLayout" RETIRED, canonical `Layout`/`LayoutFacts`,
  `LayoutFacts` is the public side-table), Lock 8 (`:595` — repeated tuples non-admit; CSS close requires
  typed value/document/view/visitor + cssparser equality before lightningcss admits), Lock 14 (`:603` —
  may NOT hand-code profile arrays / CSS profile matches; CSS+Sheets-or-BBNF witnesses), Lock 16 (`:607` —
  SVE/SVE2 must NOT be filed as NEON; DotProd/I8MM require the full manifest + consumer proof).
  **All accurate, all load-bearing.**

## §2 — Verdict on the reviewer's two core questions

### Q1 — Does any candidate (C0, C1, C2, C3, C4a, C4b) re-open a pre-block? **NO.**

| Candidate | Pre-block re-open surface | Verdict |
|---|---|---|
| C0 de-fact-stream typed Track 1 (`alphaE.md:107-158`) | re-bench String as typed (PB#3); StructRegistry/eager route (PB#1/#2a); hand-curated catalogue (PB#5b) | NOT re-opened. C0:151-158 forbids `emit_fact_stream`/`fnv64`/schema headers as admission, StructRegistry/Arena<G>/Builder<G>, eager-by-default, per-grammar type catalogue; **and the V2 fold now names `W5C_REQUEST_FACT_PROFILES` on the retire list** (the skinny Lock-14 phrase-#1 construct, the V2 changelog item). The typed summary IS the de-fact-stream. |
| C1 tape wiring + lazy cursor (`alphaE.md:160-225`) | second tape / Vec<OpenFrame>::clone / per-leaf `Box<CssColor>` (PB#1/#2a, Lock 1); relocated overfit into projection DATA (Lock 14) | NOT re-opened. C1:214-225 forbids parallel/second tape, Vec clone pathology, columnar SoA, per-leaf eager `Box::new`, **and adds a V2 no-relocated-overfit pruning test** (CSS routing must derive from the `.bbnf` rule, not per-rule-id match arms). Rides the single landed substrate. |
| C2 NEON structural pre-scan (`alphaE.md:227-291`) | x86/AVX (PB#6); CSS-specific scanner vocabulary (Lock 14); cross-call classifier-state retention (Lock 1) | NOT re-opened. C2:285-291 forbids x86/AVX, cross-call classifier state, CSS-specific vocabulary; reuses checkasm-gated grammar-general kernels; NEON produces ONLY a `Vec<u32>` index (Lock 1 transient producer, sanctioned at LOCKS.md:75). scalar-ref + checkasm present. |
| C3 commit-by-construction spine (`alphaE.md:293-339`) | speculative-rollback disguise; type-ambivalent dual representation (Lock 1) | NOT re-opened. C3:337-339 forbids type-ambivalent dual representation and speculative-rollback re-introduction as a fast path. REMOVES checkpoints, adds no mechanism; builds on the banked V5 O(1) checkpoint. |
| C4a wire orphan udot (`alphaE.md:341-372`) | x86/AVX (PB#6); fixture/per-corpus capacity consts (PB#5b); per-leaf feature detection | NOT re-opened. C4a:369-372 forbids x86/AVX, per-leaf `is_aarch64_feature_detected!` (dotprod is compile-time `target_feature`), fixture/capacity literals; the candidate's WHOLE purpose is to RETIRE the digit_mac orphan (anti-orphan). scalar-ref + checkasm present. |
| C4b NET-NEW i8mm kernel (`alphaE.md:374-421`) | x86/AVX-512/SVE filed as NEON (PB#6, Lock 16); orphan kernel; per-leaf detection | NOT re-opened. C4b:416-421 forbids x86/AVX-512, SVE (Apple no-SVE dead code, Lock 16 LOCKS.md:607), per-leaf detection (threads OnceLock ONCE), fixture/capacity literals; scalar-ref + checkasm REQUIRED for the new kernel; **GATED behind a Wave-5 re-profile** so no orphan kernel lands if the digit leaf is not top-N tailwind self-time. |

Every candidate carries an explicit, correctly-scoped "REDRESS pre-blocks" subsection; the
SIMD candidates (C2/C4a/C4b) attach scalar-ref + checkasm + same-wave-consumer. **No candidate's
admission framing lands on the OpenFrame / StructRegistry / Vec<Vec> / fact-stream-as-admission /
broadcast / FNV-arbiter / x86 carrier.** The αC §8 single load-bearing distinction ("typed/rich/
retained is the goal; eager/allocating/fragmented/serialized is the refuted carrier") is the
correct regression discriminant, and the V2 candidates respect it. The **C4 split into C4a
(unconditional orphan-wiring) and C4b (GATED net-new kernel)** is a strict CH3 improvement over V1:
it eliminates the orphan-kernel admission risk that the un-split V1 C4 carried.

### Q2 — Is αC's pre-block list correct + complete? **YES — accept with two minor REVISEs.**

αC enumerates exactly the six CONTEXT-named pre-blocks (AZ-IV eager, StructRegistry indirection,
fact-stream String, 24-row broadcast, FNV/fixture, x86/AVX), splits #2 into 2a (PERMANENT: the
indirection) and 2b (ADMIT-UNDER-FRAMING: the layout itself), and the §7 ledger is faithful to the
measured refutations (118x `cb14970f`, 28-65x / 983x / 10583x WATCHDOG, ~34% emit_* self-time,
one-tuple-×-24, 148 fixture fns, x86 out-of-scope). The two-bucket PERMANENT vs
ADMIT-UNDER-DIFFERENT-FRAMING taxonomy is the correct regression model: it prevents both the
false-negative (re-admitting the refuted carrier) and the false-positive (blocking the legitimate
typed-rich intent). The V2 TREE-DISAMBIGUATION header (`alphaC.md:18-35`) is the single best
regression-hygiene addition this cycle — it converts every doc core-tree symbol to the skinny
benched surface and pre-states "a gate keyed to `crates/core/...` is itself a CH1 defect." Two minor
REVISEs remain (§3, §4 below); neither blocks the list's correctness.

---

## §3 — Per-section dispositions

### alphaA (results extraction) — CH3-neutral extraction
- **§0-§6 (standing, baseline, equality, checkpoint, throughput ledger, banked wins): ACCEPT.**
  No regression-hypothesis transfer; the "micro-opt refuted" framing (I1/I2-equivalent) is consistent
  with αD. The banked-wins table (`alphaA.md:288`) correctly retargets the O(1) checkpoint marker to
  the skinny `TapeBuilder` and labels the original as "core-tree OpenFrame builder" — V2-clean.
- **§Pre-blocked-routes note (`alphaA.md:237,349`): ACCEPT.** "SK-V17 must NOT re-open the watermark
  route" + "Pre-blocked (do NOT re-open — see αC): AZ-IV eager value-tree (118x) …" correctly defers
  to αC and adds the watermark-route guard. The benched-surface translation pointer (`alphaA.md:388`,
  to αE:37-51) is present. CH3-clean.

### alphaB (competitor deltas) — CH3-neutral delta extraction
- **All sections: ACCEPT.** No candidate proposed. Adopts the αE §0 translation correction
  (`alphaB.md:62,307`) and keeps lightningcss as the fair full-CSSOM bar / cssparser as the admission
  gate — which keeps the wrong-plane (PB#4 broadcast/comparator-confusion) class out by construction.
  The "intended SK-V17 subject = C0 de-fact-stream" framing (`alphaB.md:79`) is consistent with αE.

### alphaC (REDRESS digest) — the load-bearing artefact for CH3
- **§0 TREE-DISAMBIGUATION header + two-bucket model: ACCEPT.** The single best regression-hygiene
  move; every doc symbol translated to the skinny surface; "a gate keyed to `crates/core/...` is a
  CH1 defect" stated up front. Closes the V1 root-cause defect at the source.
- **§1 AZ-IV eager (118x): REVISE — minor.** `alphaC.md:75-79`. The re-open test names prospective
  type symbols — "SK-V17 produces a typed CSS value (`CssTypedValue`, `CssColor`, `CssDimension`, …)
  at parse time" — but **`CssTypedValue`/`CssColor`/`CssDimension`/`CssView` are grep-clean-absent
  from `skinny/crates/`** (verified this cycle), exactly the class of phantom-symbol citation the V2
  header warns against for core-tree symbols. The re-open test is **still operable** because it is
  anchored to a real grep surface (`skinny/crates/runtime/src/grammars/css_l4_*/` — 7 dirs confirmed —
  + the benched `track1` fns at `nonjson_css_l4.rs:596-624`, confirmed) and the operative clause is
  the construct, not the name ("any `f64`/typed-node heap allocation on the per-leaf hot path that is
  not a re-readable source span"). **Concrete fix:** add one clause marking the type names as
  *prospective/illustrative* ("any per-leaf typed-value type the SK-V17 typed CSSOM introduces — there
  is no such type in skinny today, so the grep target is the construct: per-leaf `f64`/typed-node heap
  alloc under `runtime/src/grammars/css_l4_*/` or the benched `track1` fns — not a fixed symbol list").
  This is the §1 analogue of the §2 fix V1 already mandated; it keeps the tripwire from reading as a
  citation against a non-existent symbol. ADMIT-UNDER-FRAMING classification + telemetry binding
  (per-corpus payload-arena write/alloc counters, REDRESS item 8) are otherwise correct.
- **§2 StructRegistry / 2a+2b split: ACCEPT.** V1 REVISE folded. 2a PERMANENT (the indirection,
  "no framing recovers a per-leaf registry deref"), 2b ADMIT-UNDER-FRAMING re-keyed to
  `BackendRule`/`LayoutFacts` (`ir/cost.rs:119-121,259-271`) + the skinny lowering; Lock 2
  RETIRED-`StructLayout` cited correctly (LOCKS.md:160 verified). The "built once per rule at codegen,
  not per leaf" distinction is the correct admission discriminant.
- **§3 fact-stream String: ACCEPT.** PERMANENT-as-admission / ADMIT-as-diagnostic-only is correct.
  The V2 **retirement clause** (`alphaC.md:191-195`) is a strict improvement: it makes "the 7
  `RequestFacts` registrations (`regen_css.rs:45..153`) + `W5C_REQUEST_FACT_PROFILES`
  (`codegen/src/lib.rs:336`) STILL standing" a CH3/CH5 *failure-if-NOT-done* — closing the Lock 1
  parallel-substrate escape where a typed Mbps lands while the fact-stream route still admits. All
  citations verified.
- **§4 24-row broadcast: REVISE — minor (count nuance).** `alphaC.md:224-227` says "These **6**
  falsified tuples are still present in `skinny/RESULTS.md`"; §7 ledger row 4 (`alphaC.md:342`) repeats
  "6 falsified rows still present". **The live grep returns 24 `AUDIT-FALSIFIED` rows and 24
  `SK-V15-W0-broadcast-diagnostic` markers**, not 6. αD §3 (`alphaD.md:124`) says "6 `css_l4/*/
  direct_to_struct/main` W8R broadcast diagnostics" — same count, same discrepancy. The "6 vs 24"
  may reflect 6 distinct grammars × broadcast or a row-vs-tuple distinction, but as written the literal
  "6 ... rows" mismatches the 24 the file carries. **Concrete fix:** reconcile the count — either
  "6 distinct broadcast *groups* / 24 broadcast *rows*" with the basis named, or correct to 24 to match
  the file. The PERMANENT-PRE-BLOCK verdict and "no different-framing admission" are correct regardless
  of the count; this is a citation-accuracy REVISE (CH1-adjacent), not a regression-logic defect. αD
  carries the same nuance and inherits the same fix.
- **§5 FNV / fixture: ACCEPT.** 5a/5b split correct; bench-only-quarantine vs runtime-arbiter line
  exact; 148-fixture count corrected (verified); the V1-flagged residual "scratch sizes from
  input.len() + StructLayout" is now "input.len() + `BackendRule`/`LayoutFacts`, grammar-general"
  (`alphaC.md:344`) — V1 minor REVISE CLOSED.
- **§6 x86 / AVX: ACCEPT.** PERMANENT-this-pass; Apple no-SVE dead-code argument correct (Lock 16
  LOCKS.md:607 "SVE/SVE2 must not be filed as NEON" verified); aarch64 NEON intrinsics-first vocabulary
  correct; x86 reserved as a successor phase (PASS-ALPHA §8) carrying zero SK-V17 admission weight.
- **§7 consolidated ledger: ACCEPT** (modulo the §4 count fix, which propagates to row 4). Row 2b
  re-keyed to skinny; the Lock-2 `Layout`/`LayoutFacts` NB present; all six pre-blocks present with
  skinny-keyed re-open tests. Complete.
- **§8 single load-bearing distinction: ACCEPT.** The correct one-line regression law; the V2 addendum
  ("no `RequestFacts`/`W5C_REQUEST_FACT_PROFILES` route still admitting") closes the Lock-1 escape.

### alphaD (validated/invalidated ledger)
- **§0 benched-surface disambiguation: ACCEPT.** V1 root-cause folded; the doc→skinny translation
  table (`alphaD.md:30-39`) is correct and grep-verified.
- **§1 V1-V6 validated wins: ACCEPT.** V1 REVISE folded — V6 (`alphaD.md:76`) re-pathed to
  `skinny/crates/runtime/src/tape/` with the actual module names confirmed (`mod.rs`/`assembler.rs`/
  `offsets.rs`/`event_grammar.rs`, no `record/arena/cursor` siblings) and a grep-clean no-StructRegistry
  assertion on the measured tree. V2-V5 correctly attribute commit SHAs (`ea8138056`/`4de419f5e`/
  `2a85bf240`/`8153236e8`); the V5 watermark-divergence caveat preserves the anti-regression record.
- **§2 invalidated ledger (I1-I7): ACCEPT.** The CH3 backbone. I5 (AZ-IV pre-block, no re-open) and I6
  (timeline-misattribution correction — `cb14970f` 2026-05-02 predates restart `a5145a0bb` 2026-05-03)
  are precisely the two regression traps, correctly disposed. I1/I2 (micro-opt-on-eager-path refuted)
  forbids the hypothesis-transfer pattern. I7 (N=1 telemetry invalid → N≥50 median) is the correct
  telemetry-honesty guard. The "pre-block families carried forward verbatim" footer
  (`alphaD.md:102-110`) matches αC and names `W5C_REQUEST_FACT_PROFILES` explicitly.
- **§3 still-open (O1-O5): REVISE — minor (inherits §4 count).** `alphaD.md:124` carries the same
  "6 broadcast diagnostics" count as αC §4; reconcile against the 24 in RESULTS.md (same fix as αC §4).
  Otherwise: O1/O2 owner paths re-keyed to skinny (V1 REVISE folded — `regen_css.rs`, `css_l4_*/`,
  `tape/`); the eager-arena/`Box<CssColor>` pathology correctly attributed to the TOTALITY tree; the
  594-line `css_l4.toml` correctly flagged as a TOTALITY-tree fold target NOT a skinny owner path
  (verified: no `css_l4.toml` reference in `skinny/xtask/src/`); O5 retire-list names
  `W5C_REQUEST_FACT_PROFILES` + the 148-fn fixture surface. Framing constraints (Lock 1/14,
  grammar-neutral, preserve-rich-ast) correct.
- **§4 demoted + §5 ledger text: ACCEPT.** "Micro-opt does not move the floor — banked, not to be
  relitigated" is the correct anti-regression posture; the O5→O1+O2→O3→O4 spine is consistent with αE.

### alphaE (candidate shortlist) — the cross-check target for CH3
- **§0 ground-truth anchors + translation correction: ACCEPT.** Every anchor re-verified live
  (dispatch.rs, digit_mac.rs orphan, generated.rs fact-stream, nonjson_css_l4.rs track1,
  `W5C_REQUEST_FACT_PROFILES:336`, 148 fixtures, i8mm-absent, sheets 25-LOC stub). The αE:37-51
  translation correction remains the cleanest regression-hygiene artefact in the alpha set.
- **C0 de-fact-stream: ACCEPT.** Pre-block subsection (C0:151-158) forbids fact-stream-as-admission /
  StructRegistry / eager-by-default / per-grammar catalogue; V2 adds `W5C_REQUEST_FACT_PROFILES` to the
  retire list. No re-open.
- **C1 tape wiring: ACCEPT.** Pre-block subsection (C1:214-225) forbids second tape / Vec<OpenFrame>::
  clone / columnar SoA / per-leaf `Box::new`; V2 adds the no-relocated-overfit pruning test + the
  `sheets_witness` generality EXIT gate (Lock 14 witnessed-not-asserted). No re-open.
- **C2 NEON pre-scan: ACCEPT.** Pre-block subsection (C2:285-291) forbids x86/AVX / cross-call carry /
  CSS-specific vocabulary; scalar-ref + checkasm present; NEON emits only `Vec<u32>` (Lock 1 transient
  producer); `lo6_table_admissible` is the honest scalar-fallback guard. The ~56%/~10% hot-leaf %% carry
  an explicit S-P1-re-confirm-on-benched-path obligation (`actual-profiling`) — correct, no inherited
  estimate. No re-open.
- **C3 commit-by-construction: ACCEPT.** Pre-block subsection (C3:337-339) forbids type-ambivalent dual
  representation / speculative-rollback disguise. REMOVES mechanism. No re-open.
- **C4a wire orphan udot: ACCEPT.** LOW risk, admits unconditionally (scalar-ref + checkasm present);
  purpose is to RETIRE the orphan (anti-orphan, not a regression re-open); no per-leaf
  `is_aarch64_feature_detected!`. No re-open.
- **C4b NET-NEW i8mm kernel: ACCEPT.** The V2 GATE (lands ONLY if a Wave-5 re-profile proves the digit
  leaf top-N tailwind self-time, else does NOT land) is the correct anti-orphan-kernel discipline;
  scalar-ref + checkasm REQUIRED; no SVE-as-NEON; no x86. The honest-residual exit (no paper-close)
  satisfies CH6. No re-open.
- **§2 dependency order, §3 cross-cutting discipline, §4 escalation: ACCEPT.** N≥50 median + 8-field
  EXACT equality + grammar-neutral-witnessed-not-asserted bind every gate; no-paper-close on C4b
  honored; the borrowed-slice-vs-lazy directive decision correctly elevated to a C1 ENTRY GATE
  (the documented W6 stall), not a mid-wave discovery.

---

## §Carry-forward — α-F (SYNTHESIS.md + HANDOFF.md) authoring constraints (no file to disposition)

SYNTHESIS.md + HANDOFF.md are absent this cycle. When α-F authors them, these CH3 obligations bind
(inherited from the V1 SYNTHESIS §0.1/§0.3 REVISEs, which never closed because no file persists):

1. **Close-condition gates (§0.1) MUST grep the skinny benched tree.** The tape-activation gate keys to
   `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}` consumed by a skinny CSS parse path; the
   layout-projection gate keys to `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape}.rs`
   + `skinny/xtask/src/regen_css.rs`. **No gate may cite `crates/core/...`, `StructLayout`, `OpenFrame`,
   `CssArena`, `css_l4/builder.rs:274`, `begin_compound`, or `TapeStructBuilder`** — all grep-clean-absent
   from skinny; such a gate could read "met" in core while the benched CSS path is untouched (CH1 defect).
2. **The fact-stream retirement clause (αC §3) MUST appear in the §0.1 CSS close condition**: the typed
   CSS row admits ONLY with the 7 `RequestFacts` registrations (`regen_css.rs:45..153`) +
   `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`) retired to diagnostic-only behind a
   forbidden-token scan. A typed Mbps with the RequestFacts route still admitting is a Lock 1 / CH5
   parallel-substrate failure.
3. **The §0.4 pre-block ledger MUST stay construct-framed** (tree-agnostic constructs: "eager per-leaf
   payload", "registry lookup in per-leaf hot path", "second tape", "string-only fact-stream as
   admission", "one-tuple-×-N broadcast", "FNV runtime arbiter", "x86 row movement") — this is the V1
   posture that kept the §0.4 ledger free of the path-attribution defect; preserve it.
4. **Carry the 6-vs-24 broadcast count reconciliation** (this CH3 §3/§4 REVISE) into any §0.5/§0.6
   per-corpus close condition that references the falsified W8R rows.

---

## §Disposition counts

- Total artefact sections dispositioned this cycle: **34**
  (alphaA 2 dispositioned units [§0-§6 block + Pre-blocked-routes note], alphaB 1, alphaC 9 [§0,§1,§2,§3,
  §4,§5,§6,§7,§8], alphaD 5 [§0,§1,§2,§3,§4+§5], alphaE 11 [§0,C0,C1,C2,C3,C4a,C4b,§2,§3,§4 + the two
  Q1/Q2 cross-check verdicts] — counting the dispositioned units listed above).
- **ACCEPT: 31**
- **REVISE: 3** — αC §1 (prospective-type-name clause: mark `CssTypedValue`/`CssColor`/`CssDimension`
  as illustrative-not-yet-existent, anchor the tripwire to the construct + the verified grep surface);
  αC §4/§7-row-4 (reconcile "6 falsified rows" against the 24 `AUDIT-FALSIFIED` in RESULTS.md);
  αD §3-O1 (inherits the same 6-vs-24 count reconciliation).
- **REJECT: 0**

All 3 REVISE are CH1-adjacent citation-accuracy refinements (one phantom-prospective-symbol clause +
one count reconciliation that appears in two artefacts); **none is a regression-logic defect**, and
all 3 share narrow, fully-specified fixes. The αC §4 and αD §3 REVISEs are the SAME count nuance — one
corrective edit applied in two places (no orphan REVISE).

## §CH3 bottom line

**No candidate (C0, C1, C2, C3, C4a, C4b) re-opens any REDRESS pre-block.** Each carries a
correctly-scoped, skinny-keyed pre-block subsection, routes through the tape+lazy-view "different
framing", and the SIMD candidates attach scalar-ref + checkasm + same-wave-consumer. The V2 C4 split
(C4a unconditional orphan-wiring / C4b GATED net-new kernel) and the V2 fact-stream retirement clause
are both strict CH3 improvements that close orphan-kernel and parallel-substrate escapes the un-split
V1 form left open.

**αC's pre-block list is correct + complete** — six pre-blocks, the 2a/2b PERMANENT-vs-ADMIT split
sound, every measured refutation re-verified live this cycle (118x `cb14970f`, 28-65x/983x/10583x,
~34% emit_*, one-tuple-×-N, 148 fixtures, x86-out-of-scope), and the V2 TREE-DISAMBIGUATION header
closes the V1 core-tree-path-attribution root cause at the source.

**Five of six V1 REVISEs are CLOSED in the live V2 artefacts; the sixth (SYNTHESIS) carries forward
as an α-F authoring constraint because no SYNTHESIS/HANDOFF file exists this cycle.** The only open
items are 3 minor citation-accuracy REVISEs (prospective type names in αC §1; the 6-vs-24 broadcast
count in αC §4 + αD §3) — REVISE, not REJECT; no regression tripwire is unverifiable on the benched
surface as a result. CH3 is at **31/34 = 91.2% ACCEPT** this cycle, short of the 95% §3Z bar by the
3 minor REVISEs; folding them (one symbol-clause edit + one count reconciliation in two places)
converges CH3 to 100% in V3.
