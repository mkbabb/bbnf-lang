# CH3 REGRESSION — Pass Alpha SK-V17 Hardening (cycle V3)

Lens: CH3 (PASS-ALPHA §3 — does any proposed intervention re-open a REDRESS pre-block?
Cross-check the αE shortlist against entries 1-N. Has αC correctly identified the
pre-block list?). Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25`
(`git rev-parse --short HEAD` confirmed this cycle).

Reviewer focus (verbatim mandate): no candidate re-opens a REDRESS pre-block (AZ-IV
eager materialization, StructRegistry indirection, fact-stream, broadcast, FNV, x86);
αC pre-block list correct.

Subjects reviewed (V3): `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`
**+ `SYNTHESIS.md` + `HANDOFF.md` (the α-F deliverable, which NOW EXISTS this cycle —
absent at V1/V2 dispatch).** The V1/V2 SYNTHESIS §0.1/§0.3 + §0.4 obligations carried
as α-F authoring constraints in V2 §Carry-forward are dispositioned here for the first
time against a live file.

## §0 — V2→V3 fold verification (both V2 REVISEs + the four carry-forward obligations)

V2 CH3 returned ACCEPT 31 / REVISE 3 / REJECT 0 — short of the §3Z 95% bar. The three
REVISEs were two distinct nuances (one applied in two places): (i) αC §1 prospective
type names; (ii) the 6-vs-24 broadcast count in αC §4/§7-row-4 + αD §3-O1. Plus four
carry-forward obligations onto the then-absent SYNTHESIS/HANDOFF. V3 fold status, each
re-verified against the live tree this cycle:

| V2 item | V3 status | Verification |
|---|---|---|
| αC §1 prospective-type-name clause (`CssTypedValue`/`CssColor`/`CssDimension` cited as if extant) | **FOLDED** | `alphaC.md:75-83` now states the type names are "**prospective/illustrative only — grep-clean-absent from `skinny/crates/` at HEAD `1c5bd7a25` (verified this cycle), so the gate must NOT key on them as extant symbols**"; the tripwire is anchored to the construct ("any per-leaf typed/`f64`/`Box` allocation under `runtime/src/grammars/css_l4_*/` + the benched `track1` fns at `nonjson_css_l4.rs:596-624`"). Live grep confirms `CssTypedValue`/`CssColor`/`CssDimension` = 0 files in `skinny/crates/`. The V2 §1 REVISE is CLOSED. |
| αC §4 + §7-row-4 broadcast count "6" → "24" | **FOLDED** | `alphaC.md:228-235` now reads "**24 falsified rows still present in `skinny/RESULTS.md` (lines 112-135, grep-verified)**" with an explicit basis note: "this is the single 24-row broadcast — one measured tuple projected across 24 conceptual feature row-ids — NOT six. Any sibling artefact citing '6 …' undercounts; the grep-verified count is 24, range 112-135." §7 row 4 (`:350`) re-keyed to "all 24 falsified rows still present … (lines 112-135, grep-verified)". Live grep: `AUDIT-FALSIFIED` = 24, `css_l4/.*/direct_to_struct/main` = 25 (24 broadcast rows + 1 distinct W6 typed at :154). CLOSED. |
| αD §3-O1 broadcast count (inherited same nuance) | **FOLDED** | `alphaD.md:130,183,185` now read "grep-verified count = 24" / "only 24 falsified W8R broadcast diagnostics" (`skinny/RESULTS.md:112-135`). The V2 inherited-count REVISE is CLOSED in αD. |
| Carry-fwd #1: close-condition gates MUST grep skinny tree, never `crates/core/`/`StructLayout`/`OpenFrame`/`CssArena`/`begin_compound`/`TapeStructBuilder` | **DISCHARGED** | SYNTHESIS `§0.1:97-99` states every surface citation is the benched skinny tree "verifiable by grepping `skinny/crates/`, not `crates/core/`"; the Tape-activation gate (`SYNTHESIS.md:104`) + the telemetry `tape_activated` column (`:363`) carry the explicit "**NOT satisfiable by a grep in `crates/core/`**" guard; HANDOFF Next-Move #4 (`:213-216`) repeats it ("`tape_activated` is satisfied ONLY when the benched `track1::parser::parse` emits into the skinny runtime `Tape` … NOT by a grep returning non-zero in `crates/core/` (wrong-tree dishonesty is REJECTed)"). The Benched-surface note (`SYNTHESIS.md:25-62`) lists the six totality symbols as grep-clean-absent. Live grep confirms all six = 0 in skinny. |
| Carry-fwd #2: fact-stream retirement clause MUST appear in the §0.1 CSS close condition | **DISCHARGED** | SYNTHESIS Tape-activation gate + Layout-driven-projection gate (`:104-105`) both require `W5C_REQUEST_FACT_PROFILES` retired + `emit_fact_stream` retired; §0.4 carries it as a dedicated pre-block bullet (`:196-201`); the receiver goalset Tape-activation row (`:172`) keys the seam-flip to the seven `RequestFactsProfile` literals (`regen_css.rs:45..153`) + the two `for profile in …` consumers (`lib.rs:567,611`). A typed Mbps with the RequestFacts route still admitting is named a Lock-1/CH5 failure. Live grep confirms the 7 registrations + `W5C_REQUEST_FACT_PROFILES:336,299,567,611`. |
| Carry-fwd #3: §0.4 pre-block ledger MUST stay construct-framed (tree-agnostic constructs) | **DISCHARGED** | SYNTHESIS §0.4 (`:178-232`) is construct-framed: "eager per-leaf payload", "registry lookup in the per-leaf hot path", "second tape", "fact-stream String serialization as a live admission output plane", "one CSS timing tuple projected into N conceptual admits", "FNV production selector/arbiter", "x86/AVX/SVE". HANDOFF Pre-Blocked Routes (`:144-181`) mirrors it. The path citations (`generated.rs:5`, `lib.rs:336`, `RESULTS.md:112-135`) are evidence anchors, not the gate keys. |
| Carry-fwd #4: 6-vs-24 reconciliation into any §0.5/§0.6 per-corpus close condition | **DISCHARGED** | SYNTHESIS §0.2 (`:139-157`) carries a dedicated "Cross-artefact reconciliation note" naming the grep-verified 24 and the prior undercounts; §0.4 (`:202-204`) and HANDOFF (`:48-53`) both cite 24 at lines 112-135. |

**Both V2 REVISEs are CLOSED. All four V2 carry-forward obligations are DISCHARGED in
the now-extant α-F deliverable. Zero orphan REVISE from V2.**

## §1 — Independent verification performed this cycle (every disposition is grounded)

Re-greped the skinny benched tree at HEAD `1c5bd7a25`; every load-bearing claim
re-verified live, not inherited:

- **Core-tree symbols grep-clean-absent in skinny** (the disambiguation rests on this):
  `StructLayout`, `OpenFrame`, `CssArena`, `begin_compound`, `TapeStructBuilder`,
  **and the prospective typed symbols `CssTypedValue`, `CssColor`, `CssDimension`** →
  **0 files each** across `skinny/crates/` + `skinny/xtask/`. **Confirmed.** (The V2 §1
  REVISE turned on `CssColor`/`CssDimension` being non-extant — confirmed = 0.)
- **24-row broadcast:** `grep -c AUDIT-FALSIFIED skinny/RESULTS.md` = **24**;
  `grep -c 'css_l4/.*/direct_to_struct/main'` = **25** (24 broadcast + 1 distinct W6
  typed row at :154). αC §4/§7, αD §3, SYNTHESIS §0.2, HANDOFF all reconciled to 24.
  **Confirmed; V2 count REVISE closed.**
- **`W5C_REQUEST_FACT_PROFILES`**: decl `codegen/src/lib.rs:336`; selected `:299`;
  iterated `:567,:611`. αC/αD/αE/SYNTHESIS/HANDOFF all cite these exactly. **Confirmed.**
- **7 `RuntimeEmitterKind::RequestFacts` registrations** in `skinny/xtask/src/regen_css.rs`
  at **45,63,81,99,117,135,153** — exactly the lines αC §0/§3 + αE C0/C1 + SYNTHESIS
  §0.3 cite. **Confirmed.**
- **Fixture parse-fn count** `grep -c 'fn parse_' skinny/crates/bbnf-bench/src/generated_real_typed.rs`
  = **148** (NOT the doc's stale 187). αC §5 / αE / αD / SYNTHESIS corrected throughout.
  **Confirmed.**
- **i8mm grep-clean-absent** from skinny (`grep -rn i8mm skinny/crates/` → 0). αE C4b
  "NET-NEW kernel" framing correct. **Confirmed.**
- **`parse_4_digits_dotprod` orphan**: defined `aarch64/digit_mac.rs:27` (udot asm :40);
  the ONLY callers outside `digit_mac.rs` are tests (`tests/aarch64_primitives.rs:170,174,182`)
  and the module decl (`aarch64/mod.rs:18`) — **no production parse-path caller** →
  confirmed never called in prod. αE C4a "wire the orphan" (anti-orphan) framing exact.
  **Confirmed.**
- **Benched CSS Track 1 is a String**: `track1_facts(input) -> Result<String,String>`
  (`nonjson_css_l4.rs:596`); `emit_fact_stream` (`css_l4_declaration_values/generated.rs:5`).
  αC §3, αD V6/O1, αE C0, SYNTHESIS benched-surface note all exact. **Confirmed.**
- **`assert_lightningcss_strict_equality`** def at `nonjson_css_l4.rs:776`, call sites
  `:1057,:3460`. SYNTHESIS §0.1:110 (the V2 CH1-R2 citation fold) is exact. **Confirmed.**
- **`sheets_witness`** = `event_grammar_witness.rs` (24 LOC) + `mod.rs` (1 LOC) = 25 LOC,
  no `.bbnf`; codegen fail-closes `google_sheets`/`bbnf` at `lib.rs:1075-1090` (verified).
  The αE/αD/SYNTHESIS/HANDOFF "sheets_witness is NOT a projection target, deferred to
  SK-V18" framing rests on this. **Confirmed.** (Bears on CH2, not CH3 regression, but
  the SK-V18-fold demotion correctly keeps a phantom generality claim out of the contract.)
- **LOCKS anchors** re-read: Lock 1 (`LOCKS.md:75` substrate-union + no-rename amendment;
  `:585` fact-stream string-only rejected, no second tape, no cross-call classifier state),
  Lock 2 (`:160` `StructLayout` RETIRED → `Layout`/`LayoutFacts`), Lock 8 (`:595` repeated
  tuples non-admit), Lock 14 (`:386-387` witnessed-not-asserted; `:380-387` no hand-coded
  profile arrays), Lock 16 (`:607` SVE/SVE2 must NOT be filed as NEON). **All load-bearing,
  all accurate.**

## §2 — Verdict on the reviewer's two core questions

### Q1 — Does any candidate (C0, C1, C2, C3, C4a, C4b) re-open a pre-block? **NO.**

| Candidate | Pre-block re-open surface | Verdict |
|---|---|---|
| C0 de-fact-stream typed Track 1 (`alphaE.md:130-181`) | re-bench String as typed (PB#3); StructRegistry/eager route (PB#1/#2a); hand-curated catalogue (PB#5b) | NOT re-opened. C0:174-181 forbids `emit_fact_stream`/`fnv64`/schema headers as admission, StructRegistry/Arena<G>/Builder<G>, eager-by-default, per-grammar catalogue; names BOTH skinny fingerprints (148-fn `generated_real_typed.rs` + `W5C_REQUEST_FACT_PROFILES`) on the RETIRE/derive-from-grammar list. The typed summary IS the de-fact-stream. |
| C1 tape wiring + lazy cursor (`alphaE.md:183-263`) | second tape / Vec<OpenFrame>::clone / per-leaf `Box<CssColor>` (PB#1/#2a, Lock 1); relocated overfit into projection DATA (Lock 14) | NOT re-opened. C1:252-263 forbids parallel/second tape, Vec-clone pathology, columnar SoA, per-leaf eager `Box::new`, relocated-overfit-into-projection-data; carries a derive-from-`.bbnf` pruning test. "No new cursor/builder type — the existing `Tape`/`ValueRef`/`TapeBuilder` is the single substrate." Rides the single landed substrate. |
| C2 NEON structural pre-scan (`alphaE.md:265-329`) | x86/AVX (PB#6); CSS-specific scanner vocabulary (Lock 14); cross-call classifier-state retention (Lock 1) | NOT re-opened. C2:323-329 forbids x86/AVX, cross-call classifier state, CSS-specific vocabulary; reuses checkasm-gated grammar-general kernels; NEON produces ONLY a `Vec<u32>` index (Lock 1 transient producer, LOCKS.md:75). scalar-ref + checkasm present; the ~56%/~10% hot-leaf %% carry an explicit S-P1-re-confirm-on-benched-path obligation (actual-profiling). |
| C3 commit-by-construction spine (`alphaE.md:331-377`) | speculative-rollback disguise; type-ambivalent dual representation (Lock 1) | NOT re-opened. C3:375-377 forbids type-ambivalent dual representation and speculative-rollback re-introduction as a fast path. REMOVES checkpoints, adds no mechanism; builds on the banked SK-V16 O(1) checkpoint. |
| C4a wire orphan udot (`alphaE.md:379-410`) | x86/AVX (PB#6); fixture/per-corpus capacity consts (PB#5b); per-leaf feature detection | NOT re-opened. C4a:407-410 forbids x86/AVX, per-leaf `is_aarch64_feature_detected!` (dotprod is compile-time `target_feature`), fixture/capacity literals; the candidate's WHOLE purpose is to RETIRE the digit_mac orphan (anti-orphan). scalar-ref + checkasm present. |
| C4b NET-NEW i8mm kernel (`alphaE.md:412-459`) | x86/AVX-512/SVE filed as NEON (PB#6, Lock 16); orphan kernel; per-leaf detection | NOT re-opened. C4b:454-459 forbids x86/AVX-512, SVE (Apple no-SVE dead code, Lock 16 LOCKS.md:607), per-leaf detection (threads OnceLock ONCE); scalar-ref + checkasm REQUIRED for the new kernel; **GATED behind a Wave-5 re-profile** so no orphan kernel lands if the digit leaf is not top-N tailwind self-time. |

Every candidate carries an explicit, correctly-scoped "REDRESS pre-blocks" subsection;
the SIMD candidates (C2/C4a/C4b) attach scalar-ref + checkasm + same-wave-consumer.
**No candidate's admission framing lands on the OpenFrame / StructRegistry / Vec<Vec> /
fact-stream-as-admission / broadcast / FNV-arbiter / x86 carrier.** The αC §8 single
load-bearing distinction ("typed/rich/retained is the goal; eager/allocating/
fragmented/serialized is the refuted carrier") is the correct regression discriminant,
and the V3 candidates respect it. The C4a/C4b split (unconditional orphan-wiring vs
GATED net-new kernel), banked from V1→V2, holds in V3 and eliminates the orphan-kernel
admission risk. The V3 SUPERSESSION of the V2 "emit-for-`sheets_witness`" clause
(αE:45-48, struck) is a CH2 generality repair, but it has a CH3 benefit: it removes a
phantom-symbol generality assertion that could have been read as a relocated-overfit
escape hatch.

### Q2 — Is αC's pre-block list correct + complete? **YES.**

αC enumerates exactly the six CONTEXT-named pre-blocks (AZ-IV eager, StructRegistry
indirection, fact-stream String, 24-row broadcast, FNV/fixture, x86/AVX), splits #2
into 2a (PERMANENT: the indirection) and 2b (ADMIT-UNDER-FRAMING: the layout itself
re-keyed to `BackendRule`/`LayoutFacts`), and the §7 ledger is faithful to the measured
refutations (118x `cb14970f`; 28-65x / 983x / 10583x WATCHDOG; ~34% emit_* self-time;
one-tuple-×-24; 148 fixture fns; x86 out-of-scope). The two-bucket PERMANENT vs
ADMIT-UNDER-DIFFERENT-FRAMING taxonomy is the correct regression model — it prevents
both the false-negative (re-admitting the refuted carrier) and the false-positive
(blocking the legitimate typed-rich intent). The V2 TREE-DISAMBIGUATION header
(`alphaC.md:18-46`) + the fact-stream retirement clause (`:195-199`) + the §1
prospective-type-name correction (`:75-83`, V3 fold) make every re-open test
greppable on the skinny benched surface. **No pre-block missing, no over-block.**

---

## §3 — Per-section dispositions

### alphaA (results extraction) — CH3-neutral extraction
- **§0-§6 + Pre-blocked-routes note: ACCEPT.** No regression-hypothesis transfer.
  The banked-wins table retargets the O(1) checkpoint marker to the skinny `TapeBuilder`;
  the broadcast count is stated correctly as 24 (`alphaA.md:124` was the only antecedent
  artefact that had it right at V2, per SYNTHESIS §0.2 reconciliation note). The
  "Pre-blocked (do NOT re-open — see αC)" deferral + watermark-route guard are CH3-clean.

### alphaB (competitor deltas) — CH3-neutral delta extraction
- **All sections: ACCEPT.** No candidate proposed. Keeps lightningcss as the fair
  full-CSSOM bar / cssparser as the admission gate (PB#4 comparator-confusion out by
  construction). The per-corpus endpoints (animate↔164, tailwind↔51, material↔60) are
  self-flagged INFERRED-from-corpus-character, not cited measurements — SYNTHESIS §0.5
  correctly carries them as UNMEASURED-PENDING (`SYNTHESIS.md:277-280`), so they cannot
  become a falsified per-corpus admit (anti-broadcast hygiene). CH3-clean.

### alphaC (REDRESS digest) — the load-bearing artefact for CH3
- **§0 TREE-DISAMBIGUATION header + two-bucket model: ACCEPT.** Every doc symbol
  translated to the skinny surface; "a gate keyed to `crates/core/...` is a CH1 defect"
  stated up front. The single best regression-hygiene artefact in the alpha set.
- **§1 AZ-IV eager (118x): ACCEPT** (V2 REVISE folded). `alphaC.md:75-83` now marks
  `CssTypedValue`/`CssColor`/`CssDimension` as prospective/illustrative (grep-clean-absent,
  verified = 0 this cycle) and anchors the tripwire to the construct + the verified grep
  surface (`runtime/src/grammars/css_l4_*/` + `nonjson_css_l4.rs:596-624`). ADMIT-UNDER-
  FRAMING + payload-arena write/alloc telemetry binding correct.
- **§2 StructRegistry / 2a+2b split: ACCEPT.** 2a PERMANENT ("no framing recovers a
  per-leaf registry deref"), 2b ADMIT-UNDER-FRAMING re-keyed to `BackendRule`/`LayoutFacts`
  (`ir/cost.rs:119-121,259-271`); Lock 2 RETIRED-`StructLayout` cited correctly (LOCKS.md:160).
- **§3 fact-stream String: ACCEPT.** PERMANENT-as-admission / ADMIT-as-diagnostic-only
  correct; the retirement clause (`alphaC.md:195-199`) makes "the 7 `RequestFacts`
  registrations + `W5C_REQUEST_FACT_PROFILES` STILL standing" a CH3/CH5 failure-if-NOT-done,
  closing the Lock 1 parallel-substrate escape. All citations verified.
- **§4 24-row broadcast: ACCEPT** (V2 REVISE folded). `alphaC.md:228-235` reads "24
  falsified rows still present … lines 112-135, grep-verified" with the explicit basis
  note "this is the single 24-row broadcast … NOT six." Live grep = 24. PERMANENT-PRE-BLOCK
  verdict correct.
- **§5 FNV / fixture: ACCEPT.** 5a/5b split correct; 148-fixture count (verified); the
  V1-flagged residual is now "input.len() + `BackendRule`/`LayoutFacts`, grammar-general".
- **§6 x86 / AVX: ACCEPT.** PERMANENT-this-pass; Apple no-SVE dead-code argument correct
  (Lock 16 LOCKS.md:607 verified); x86 reserved as a successor phase (PASS-ALPHA §8),
  zero SK-V17 admission weight.
- **§7 consolidated ledger: ACCEPT** (row 4 now reads 24, V2 REVISE folded). Row 2b
  re-keyed to skinny; Lock-2 `Layout`/`LayoutFacts` NB present; all six pre-blocks with
  skinny-keyed re-open tests. Complete.
- **§8 single load-bearing distinction: ACCEPT.** The correct one-line regression law;
  the "no `RequestFacts`/`W5C_REQUEST_FACT_PROFILES` route still admitting" addendum
  closes the Lock-1 escape.

### alphaD (validated/invalidated ledger)
- **§0 benched-surface disambiguation: ACCEPT.** doc→skinny translation table grep-verified.
- **§1 V1-V6 validated wins: ACCEPT.** V6 re-pathed to `skinny/crates/runtime/src/tape/`;
  V2-V5 attribute commit SHAs; the V5 watermark-divergence caveat preserves the
  anti-regression record.
- **§2 invalidated ledger (I1-I7): ACCEPT.** The CH3 backbone. I5 (AZ-IV pre-block,
  no re-open) and I6 (timeline-misattribution: `cb14970f` 2026-05-02 predates restart
  `a5145a0bb` 2026-05-03) are the two regression traps, correctly disposed. I1/I2
  (micro-opt-on-eager-path refuted) forbids hypothesis-transfer. I7 (N=1 invalid →
  N≥50 median) is the telemetry-honesty guard. The pre-block footer names
  `W5C_REQUEST_FACT_PROFILES` explicitly.
- **§3 still-open (O1-O5): ACCEPT** (V2 count REVISE folded). `alphaD.md:130,183,185`
  now read "grep-verified count = 24"; O1/O2 owner paths re-keyed to skinny; the
  eager-arena/`Box<CssColor>` pathology attributed to the TOTALITY tree; the 594-line
  `css_l4.toml` flagged as a TOTALITY fold target (grep-clean-absent from skinny);
  O5 retire-list names `W5C_REQUEST_FACT_PROFILES` + the 148-fn surface.
- **§4 demoted + §5 ledger text: ACCEPT.** "Micro-opt does not move the floor — banked,
  not to be relitigated" is the correct anti-regression posture.

### alphaE (candidate shortlist) — the cross-check target for CH3
- **§0 ground-truth anchors + translation correction: ACCEPT.** Every anchor re-verified
  live (dispatch.rs, digit_mac.rs orphan never-called-in-prod, generated.rs fact-stream,
  nonjson_css_l4.rs:596 track1, `W5C_REQUEST_FACT_PROFILES:336`, 148 fixtures, i8mm-absent,
  sheets_witness 25-LOC stub). The αE:94-118 translation correction remains the cleanest
  regression-hygiene artefact.
- **C0 de-fact-stream: ACCEPT.** Pre-block subsection (C0:174-181) forbids
  fact-stream-as-admission / StructRegistry / eager-by-default / per-grammar catalogue;
  names both skinny fingerprints on the retire list. No re-open.
- **C1 tape wiring: ACCEPT.** Pre-block subsection (C1:252-263) forbids second tape /
  Vec-clone / columnar SoA / per-leaf `Box::new`; carries the no-relocated-overfit pruning
  test + the JSON+CSS EXIT gate. The V3 sheets_witness-struck repair (option b′) removes
  a non-dischargeable phantom generality target. No re-open.
- **C2 NEON pre-scan: ACCEPT.** Pre-block subsection (C2:323-329) forbids x86/AVX /
  cross-call carry / CSS-specific vocabulary; scalar-ref + checkasm present; NEON emits
  only `Vec<u32>` (Lock 1 transient producer); `lo6_table_admissible` is the honest
  scalar-fallback guard; the ~56%/~10% %% carry an S-P1-re-confirm obligation. No re-open.
- **C3 commit-by-construction: ACCEPT.** Pre-block subsection (C3:375-377) forbids
  type-ambivalent dual representation / speculative-rollback disguise. REMOVES mechanism.
  No re-open.
- **C4a wire orphan udot: ACCEPT.** LOW risk, admits unconditionally (scalar-ref +
  checkasm present); purpose is to RETIRE the orphan (anti-orphan); no per-leaf
  `is_aarch64_feature_detected!`. No re-open.
- **C4b NET-NEW i8mm kernel: ACCEPT.** The GATE (lands ONLY if a Wave-5 re-profile proves
  the digit leaf top-N tailwind self-time) is the correct anti-orphan-kernel discipline;
  scalar-ref + checkasm REQUIRED; no SVE-as-NEON; no x86; honest-residual exit (no
  paper-close). No re-open.
- **§2 dependency order, §3 cross-cutting discipline, §4 escalation: ACCEPT.** N≥50 median
  + 8-field EXACT equality + grammar-neutral-witnessed-not-asserted (JSON+CSS only, Sheets
  deferred SK-V18) bind every gate; the borrowed-slice-vs-lazy directive decision elevated
  to a C1 ENTRY GATE (the documented W6 stall).

### SYNTHESIS.md (α-F deliverable — dispositioned for the first time this cycle)
- **Benched-surface note (`:25-62`): ACCEPT.** Lists the six totality symbols as
  grep-clean-absent (verified); states "any close-condition gate keyed on them … is
  wrong-tree dishonesty and is REJECTed." Discharges V2 carry-fwd #1 at the source.
- **§0.1 Close condition (`:96-114`): ACCEPT.** Every gate greps `skinny/crates/`. The
  Tape-activation gate (`:104`) requires `Tape`/`ValueRef`/`TapeBuilder` in the benched
  CSS parse path + `PayloadArena` write/alloc counters + "No new cursor/builder type … no
  second tape (Lock 1)". The Layout-driven-projection gate (`:105`) retires
  `W5C_REQUEST_FACT_PROFILES` + derives routing from `.bbnf`/`BackendRule` (Lock 14 no-
  relocated-overfit). The preserve-rich-ast gate (`:107`) forbids per-leaf `Box::new` /
  eager value tree (PB#1). The CSS>SOTA gate (`:108`) keys to lightningcss full-CSSOM
  (PB#4 comparator-plane correct). Discharges carry-fwd #1 + #2.
- **§0.2 Starting state (`:116-157`): ACCEPT.** The 24-count reconciliation note
  (`:139-157`) is the V2 carry-fwd #4 discharge — names the grep-verified 24 + the prior
  undercounts; "zero ADMITTED TYPED CSS rows; the only CSS rows are these 24 falsified
  broadcast diagnostics." No SK-V16 per-corpus admitted typed-CSS row to delta against
  (correct — prevents a broadcast row being lifted as a baseline).
- **§0.3 Receiver goalset (`:159-176`): ACCEPT.** Every owner path is the benched skinny
  tree; the totality `emit_builder`/`OpenFrame`/`css_l4/builder.rs:274` are named as the
  SK-V18 fold target "NOT SK-V17 owner paths; a receiver editing them would burn LOC on
  an un-benched tree." The seam-flip site is the seven `RequestFactsProfile` literals
  (`regen_css.rs:45..153`, verified) + the two consumer loops (`lib.rs:567,611`, verified).
- **§0.4 Pre-blocks (`:178-257`): ACCEPT.** Construct-framed (carry-fwd #3 discharged).
  All six CONTEXT pre-blocks present verbatim + the `W5C_REQUEST_FACT_PROFILES` retire
  bullet + the "No second substrate" Lock-1 type-ambivalence bullet (`:229-232`) that
  pre-blocks an introduced skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`. The
  Generality clause (`:234-257`) correctly scopes the witness to JSON+CSS, defers
  Sheets/BBNF-self to SK-V18, and does NOT claim the Lock 14 CSS+Sheets minimum is met
  (LOCKS.md:386-387) — removing a phantom generality assertion that would be a CH2 defect
  and a relocated-overfit risk.
- **§0.5 Per-corpus close conditions (`:259-294`): ACCEPT.** Benched corpus set fixed
  (`{bootstrap, tailwindcss, material-components-web, animate}`, no `normalize`); per-corpus
  endpoints UNMEASURED-PENDING (`:277-280`) — "no wave exit-gate may key on an inferred
  per-corpus endpoint until the N≥50 harness emits the per-corpus split" (anti-broadcast).
  Tailwind explicitly allowed to land short with honest residual (CH6 no-paper-close).
- **§0.6 Strict comparator gate (`:296-313`): ACCEPT.** lightningcss full-CSSOM the fair
  bar; cssparser token-scan a flaw probe ("beating cssparser is NOT a >SOTA claim"); the
  W6 fact-stream comparator (`assert_lightningcss_strict_equality` against a fact stream,
  def `:776` verified) retired — the comparator must build CSSOM. PB#4 closed.
- **§Section 1/2/3 (`:315-420`): ACCEPT.** The telemetry schema (`:347-369`) carries
  `tape_activated` ("NOT satisfiable by a grep in `crates/core/`"), `w5c_profile_array_retired`,
  `projection_generality_exercise` (`sheets_witness` explicitly NOT a valid value),
  `simd_non_json_exercise=css_l4` — every regression tripwire is a gate column. The gate
  rejects `sample_count==1` or one-tuple-across-corpus-rows (the W8R tripwire, `:386-387`).

### HANDOFF.md (α-F deliverable — dispositioned for the first time this cycle)
- **Benched-substrate disclosure (`:9-20`): ACCEPT.** Mirrors the SYNTHESIS note;
  totality symbols grep-clean-absent, SK-V18 fold target.
- **Current State (`:22-53`): ACCEPT.** The 24-count is stated correctly (`:48-53`,
  grep-verified); "ZERO admitted typed CSS rows." Honest W6 close.
- **What SK-V17 Opens + Generality scope (`:55-100`): ACCEPT.** The four-lever route
  keyed to skinny paths; the S-P1 re-confirm-on-benched-path obligation on the ~56%/~10%
  hot leaf (actual-profiling); the JSON+CSS-only generality scope with Sheets deferred.
- **Gate Posture + CH7 scope (`:120-142`): ACCEPT.** CH7 overfit-prune is correctly
  framed as a pass-added monotonic extension (not elevated into the §3W mandatory CH1-CH6
  canon); its scan targets `W5C_REQUEST_FACT_PROFILES` retirement + no-relocated-overfit
  + `.bbnf`-derivation + regen-array-trends-to-JSON. CH3-aligned.
- **Pre-Blocked Routes (`:144-181`): ACCEPT.** All six pre-blocks + the W5C bullet + the
  "No second substrate" Lock-1 bullet + the hidden-coupling escape list (cross-call
  classifier-state retention named). Binding on S-P0..S-P3.
- **Next Move (`:183-232`): ACCEPT.** The `tape_activated` gate (`:213-216`) repeats
  "NOT by a grep returning non-zero in `crates/core/` (wrong-tree dishonesty is REJECTed)";
  C4a unconditional / C4b gated-behind-re-profile preserved (`:205-208`); revert protocol /
  hard caps / triumvirate sanctioned-deferred to S-P3 per PASS-ALPHA §4.4.

---

## §Disposition counts

- Total artefact sections dispositioned this cycle: **40**
  (alphaA 2 [§0-§6 block + Pre-blocked-routes note], alphaB 1, alphaC 9 [§0,§1,§2,§3,§4,
  §5,§6,§7,§8], alphaD 5 [§0,§1,§2,§3,§4+§5], alphaE 11 [§0,C0,C1,C2,C3,C4a,C4b,§2,§3,§4
  + the Q1/Q2 cross-check verdicts], SYNTHESIS 8 [benched-note,§0.1,§0.2,§0.3,§0.4,§0.5,
  §0.6,§1-3 block], HANDOFF 5 [benched-disclosure,Current State,Opens+Generality,Gate
  Posture+CH7,Pre-Blocked+Next-Move]).
- **ACCEPT: 40**
- **REVISE: 0**
- **REJECT: 0**

**Both V2 REVISEs are folded (αC §1 prospective-type clause; the 6-vs-24 broadcast count
in αC §4/§7 + αD §3). All four V2 carry-forward obligations are discharged in the
now-extant SYNTHESIS + HANDOFF. Zero orphan REVISE.**

## §CH3 bottom line

**No candidate (C0, C1, C2, C3, C4a, C4b) re-opens any REDRESS pre-block.** Each carries
a correctly-scoped, skinny-keyed pre-block subsection, routes through the tape+lazy-view
"different framing", and the SIMD candidates attach scalar-ref + checkasm +
same-wave-consumer. The C4a/C4b split and the fact-stream retirement clause close the
orphan-kernel and parallel-substrate escapes.

**αC's pre-block list is correct + complete** — six pre-blocks, the 2a/2b
PERMANENT-vs-ADMIT split sound, every measured refutation re-verified live this cycle
(118x `cb14970f`; 28-65x/983x/10583x; ~34% emit_*; one-tuple-×-24; 148 fixtures;
x86-out-of-scope). The TREE-DISAMBIGUATION header + fact-stream retirement clause + the
V3 prospective-type-name correction make every re-open test greppable on the benched
skinny surface.

**The α-F deliverable (SYNTHESIS + HANDOFF) now exists and discharges all four V2
carry-forward CH3 obligations**: §0.1 gates grep the skinny tree (never `crates/core/`),
the fact-stream retirement clause is in the §0.1 CSS close condition + §0.4, the §0.4
pre-block ledger is construct-framed, and the 6-vs-24 broadcast count is reconciled in
§0.2 + HANDOFF. The telemetry schema makes every regression tripwire a gate column
(`tape_activated` not-grep-`crates/core/`, `w5c_profile_array_retired`, sample_count≥50,
no one-tuple-across-rows).

CH3 is at **40/40 = 100% ACCEPT** this cycle. The V2→V3 fold (one symbol-clause edit +
one count reconciliation in three artefacts + the α-F authoring against the carry-forward
constraints) converges CH3. **No regression tripwire is unverifiable on the benched
surface; no candidate re-opens a pre-block; no orphan REVISE carries forward.**
