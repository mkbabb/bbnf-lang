# CH3 REGRESSION — Pass Alpha SK-V17 Hardening (cycle V1)

Lens: CH3 (PASS-ALPHA §3 — does any proposed intervention re-open a REDRESS pre-block?
Cross-check the αE shortlist against entries 1-N. Has αC correctly identified the
pre-block list?). Host: aarch64 Apple M5 Max only. HEAD of record `1c5bd7a25`.

Reviewer focus (verbatim mandate): no candidate re-opens a REDRESS pre-block (AZ-IV
eager materialization, StructRegistry indirection, fact-stream, broadcast, FNV, x86);
αC pre-block list correct.

Subjects reviewed: `research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md` + `SYNTHESIS.md`
+ `HANDOFF.md`. (There is no `alphaF.md`; the α-F deliverable is `SYNTHESIS.md` + `HANDOFF.md`
per PASS-ALPHA §2/§6 — this is correct, not a gap.)

## Verification performed (every disposition below is grounded)

- **αC pre-block citations** spot-checked against source: arch doc `sk-v16-css-sota-tape-architecture.md:46-66`
  (118x canada 1.83→215.7ms; 28-65x bbnf/sheets; 983x css bootstrap 606.4ms; 10583x tailwind
  77.6s WATCHDOG; "StructBuilder/OpenFrame appear nowhere in skinny/"; "Recover the A-series
  uniform flat-tape + lazy view, do not recover StructRegistry/Arena<G>/Builder<G>") and `:21-26`
  (intra-A-series timeline correction `cb14970f` 2026-05-02 predates restart `a5145a0bb` 2026-05-03)
  — **all accurate**.
- **LOCKS citations** verified: Lock 1 (`LOCKS.md:75` substrate-union, Vec<OpenFrame>::clone
  86.07% samply; `:585` fact-stream string-only rejected), Lock 8 (`:595` broadcast non-admit +
  "CSS close requires generated typed value/document/view/visitor + cssparser equality before
  lightningcss CSSOM pressure"), Lock 14 (`:603` grammar-neutrality), Lock 16 (`:607` aarch64-only,
  "SVE/SVE2 must not be filed as NEON") — **all accurate, load-bearing**.
- **W6 numbers** verified: typed Track 1 3.093 Mbps (`sk-v16-w6-speed-report.md:57,164`); the 2331
  Mbps / "2.46x" String-summary explicitly refuted as not-the-typed-result (`:83-87`).
- **REDRESS item 6** (zero payload bytes/writes/allocs, `skinny/REDRESS.md:136`) and the inherited
  family IDs (183/184/209 confirmed real REDRESS entries at `REDRESS.md:5092,5105,5173`) —
  **accurate**; the 118x/28-65x numbers correctly attributed to `cb14970f`/post-AZ-IV.json, NOT to
  a skinny REDRESS entry (skinny REDRESS is the JSON cycle; the CSS-regression numbers live in the
  arch doc — αC/αD cite them there correctly).
- **αE anchors** verified in the live tree: `select_classifier`/`PrimitiveKernels`/`primitive_kernels`
  OnceLock/`lo6_table_admissible` (`bbnf-simd/src/dispatch.rs:42,50,58,101`); `parse_4_digits_dotprod`
  with dotprod gating + scalar fallback (`aarch64/digit_mac.rs:10-27`, scalar twin `:18-22`);
  `emit_fact_stream() -> Result<String, CssFactError>` (`css_l4_declaration_values/generated.rs:5`);
  `track1_facts() -> Result<String,String>` (`nonjson_css_l4.rs:596`); `is_aarch64_feature_detected!`
  **grep-clean across skinny/crates** (confirms αE's "ZERO such detection in skinny, verified" for C4).
- **Path-attribution audit (the one real CH3-adjacent defect):** grep-confirmed that in skinny
  `StructLayout`, `OpenFrame`, `CssArena`, `crates/core/src/runtime/tape/`, and `css_l4/builder.rs`
  **do not exist**. The tape substrate is at `skinny/crates/runtime/src/tape/`. αE §39-51 caught this
  exactly; αC/αD/SYNTHESIS did not fully translate. Detailed below.

---

## Verdict on the reviewer's two core questions

### Q1 — Does any candidate (C0-C4) re-open a pre-block? **NO.**

| Candidate | Pre-block re-open risk surface | Verdict |
|---|---|---|
| C0 de-fact-stream typed Track 1 | could re-bench String as typed result (PB#3); could route StructRegistry/eager (PB#1/#2a) | NOT re-opened — C0:102-109 explicitly forbids emit_fact_stream as admission surface, StructRegistry/Arena<G>/Builder<G>, eager-by-default, and per-grammar type catalogue. The typed summary IS the de-fact-stream. |
| C1 tape wiring + lazy cursor | second tape / Vec<OpenFrame>::clone / per-leaf Box<CssColor> (PB#1/#2a, Lock 1) | NOT re-opened — C1:152-155 forbids parallel substrate, second tape, Vec<OpenFrame>::clone, columnar SoA, per-leaf eager Box::new. Rides the landed single substrate. |
| C2 NEON structural pre-scan | x86/AVX (PB#6); CSS-specific scanner vocabulary (Lock 14); cross-call carry retention (Lock 1 v+1) | NOT re-opened — C2:205-213 forbids x86/AVX, cross-call classifier-state retention, CSS-specific vocabulary; reuses checkasm-gated grammar-general kernels; NEON produces ONLY a Vec<u32> index (transient producer, Lock 1 sanctioned). |
| C3 commit-by-construction spine | speculative-rollback disguise; type-ambivalent dual representation (Lock 1) | NOT re-opened — C3:255-257 forbids type-ambivalent dual representation and speculative-rollback re-introduction as a fast path. REMOVES checkpoints, adds no mechanism. |
| C4 tailwind tuning (udot + i8mm) | x86/AVX-512/SVE (PB#6); fixture/per-corpus capacity consts (PB#5b); per-leaf feature detection | NOT re-opened — C4:299-304 forbids x86/AVX-512, SVE (Apple no-SVE dead code), per-leaf is_aarch64_feature_detected, fixture/per-corpus capacity literals; tuning is a generic delimiter-density heuristic; detection threads the OnceLock ONCE. |

Every candidate carries an explicit, correctly-scoped "REDRESS pre-blocks" subsection, and the
hot-leaf/SIMD candidates (C2/C4) attach scalar-ref + checkasm + same-wave-consumer. No candidate's
admission framing lands on the OpenFrame/StructRegistry/Vec<Vec>/fact-stream/broadcast/FNV/x86
carrier. The αC §8 single load-bearing distinction ("typed/rich/retained is the goal; eager/
allocating/fragmented/serialized is the refuted carrier") is the correct regression discriminant and
the candidates respect it.

### Q2 — Is αC's pre-block list correct? **YES — accept with one refinement.**

αC enumerates exactly the six CONTEXT-named pre-blocks, splits #2 into 2a (PERMANENT: the indirection)
and 2b (ADMIT-UNDER-FRAMING: the StructLayout itself), and the §7 ledger is faithful to the measured
refutations. The two-bucket PERMANENT vs ADMIT-UNDER-DIFFERENT-FRAMING model is the correct regression
taxonomy: it prevents both false-negative (re-admitting the refuted carrier) and false-positive
(blocking the legitimate typed-rich intent). The one refinement (REVISE αC §2, §7 row 2b) is the
core-tree-vs-skinny path attribution — see disposition below.

---

## Per-section dispositions

### alphaA (results extraction)
- **§0-§6 (standing, baseline, equality, checkpoint, throughput ledger, banked wins): ACCEPT.**
  No regression hypothesis transfer; the I1/I2-equivalent "micro-opt refuted" framing is consistent
  with αD. CH3-neutral and correctly cited.
- **§7 goalset seed: ACCEPT.** Hands a tape/lazy-projection seed to αE/αF without re-asserting any
  refuted route.
- **§8 citation ledger: ACCEPT.**

### alphaB (competitor deltas)
- **§0-§6 + verification ledger: ACCEPT.** CH3-neutral (no candidate proposed here). The plane
  taxonomy correctly keeps cssparser as a flaw-probe and lightningcss as the fair bar — this prevents
  the broadcast/wrong-plane class of regression (PB#4) from re-entering via comparator confusion.

### alphaC (REDRESS digest) — the load-bearing artefact for CH3
- **§0 (what the digest does) + two-bucket model: ACCEPT.** Correct regression discriminant.
- **§1 AZ-IV eager (118x): ACCEPT.** Citations exact; ADMIT-UNDER-FRAMING classification correct;
  re-open test (per-leaf typed/f64 heap alloc) and telemetry binding (per-corpus payload-arena
  write/alloc counters, REDRESS item 8) are measurable.
- **§2 StructRegistry/StructLayout: REVISE.** `alphaC.md:93-102, 276 (ledger row 2b)`. The 2a/2b split
  is correct, but §2b grounds the admission on **core-tree** symbols (`StructLayout`, `LayoutKind`,
  `FieldSource`, `css_l4/builder.rs:274` "~40-arm match layout.rule_id", `bbnf_ir::registry::struct.rs`,
  `classify_body` at `crates/ir/src/passes/types/registry.rs:140`). **These do not exist in the benched
  skinny tree** (grep-clean for `StructLayout`/`OpenFrame`/`CssArena` across `skinny/crates/`; verified).
  The arch doc itself states "StructBuilder/OpenFrame appear nowhere in skinny/" (sk-v16-arch:58) —
  αC reproduces the symbol but does not flag that the re-open test ("builder hardcodes match rule_id")
  and the different-framing admission ("builder reads layout.fields") are written against TOTALITY
  symbols, not the surface SK-V17 actually benches.
  **Concrete fix:** add a one-paragraph translation note (mirror αE §39-51): in skinny the layout
  equivalent is the codegen lowering `BackendRule` + `lower/tape_plan.rs` (`TapeFlavor`/`render_rule`/
  `TapeEmit`/`SpanMark`) + `lower/{offset_tape,event_tape,eager_tape}.rs`; the "match rule_id" re-open
  test maps to `skinny/xtask/src/regen_css.rs resolve_builder_routes` / the eager `CssStructBuilder`
  route strings. Restate the 2b re-open test and admission against those skinny paths so the CHALLENGE
  gate can actually grep the benched tree. Without this, the 2b admission/re-open test is unverifiable
  on the benched surface (the regression tripwire fires on a tree that is not measured).
- **§3 fact-stream String: ACCEPT.** PERMANENT-as-admission/ADMIT-as-diagnostic-only correct;
  `emit_fact_stream`/`track1_facts -> Result<String,...>` verified; ~34% emit_* self-time and the
  3.09-vs-2331 distinction cited correctly.
- **§4 24-row broadcast: ACCEPT.** PERMANENT, "no different-framing admission" is the correct verdict;
  Lock 8 + per-corpus N≥50 median replacement is the right telemetry binding.
- **§5 FNV/fixture: ACCEPT.** 5a/5b split correct; bench-only-quarantine vs runtime-arbiter line is
  exact; "scratch sizes from input.len() + StructLayout, grammar-general" admission — note the same
  core-tree-`StructLayout` wording recurs (`alphaC.md:228,280 row 5b`); minor, folds into the §2 REVISE
  translation note (sizing in skinny derives from `input.len()` + the BackendRule/tape-plan shape).
- **§6 x86/AVX: ACCEPT.** PERMANENT-this-pass; Apple no-SVE dead-code argument correct; aarch64 NEON
  intrinsics-first vocabulary correct.
- **§7 consolidated ledger: REVISE.** Row 2b carries the same core-tree symbol — fix with §2's
  translation note (one ledger-row edit: re-key 2b's re-open test/admission to skinny `regen_css.rs`/
  `tape_plan.rs`).
- **§8 single load-bearing distinction: ACCEPT.** This is the correct one-line regression law.

### alphaD (validated/invalidated ledger)
- **§1 validated wins (V1-V6): REVISE.** `alphaD.md:32 (V6)`. V6 cites the substrate at
  `crates/core/src/runtime/tape/{record,arena,cursor,mod}.rs` (core tree) and the unwired grep at
  `crates/core/src/grammar/generated/`. The benched skinny substrate is `skinny/crates/runtime/src/tape/`
  (verified: `assembler.rs`, `event_grammar.rs`, `mod.rs`, `offsets.rs` — note: no `record.rs`/
  `arena.rs`/`cursor.rs` siblings as named). **Concrete fix:** re-path V6 to `skinny/crates/runtime/
  src/tape/` and confirm the actual module names; the "no StructRegistry/Arena<G>/Builder<G>" claim is
  correct but must be asserted against the skinny tree to be the regression guard.
- **§2 invalidated ledger (I1-I7): ACCEPT.** This is the CH3 backbone and it is excellent — I5
  (AZ-IV pre-block, no re-open) and I6 (timeline-misattribution correction) are precisely the two
  regression traps, correctly disposed. I1/I2 (micro-opt-on-eager-path refuted) correctly forbids the
  hypothesis-transfer pattern PASS-ALPHA §9.5 / §3 guards against. The §2 footer "pre-block families
  carried forward verbatim" matches αC.
- **§3 still-open (O1-O5): REVISE.** `alphaD.md:79-83`. O2 cites `css_l4/builder.rs OpenFrame` +
  `CssArena Vec<Vec<...>>` and O1 the core-tree `regen_css.rs emit_builder/emit_view/emit_document/
  emit_arena` — `css_l4/builder.rs` and `CssArena` do not exist in skinny (the eager arena/Box<CssColor>
  pathology the conversion report cites is at `crates/core/src/runtime/css_l4/arena.rs:60-68,127-133`,
  the TOTALITY tree). **Concrete fix:** same translation note as αC §2; re-path O1/O2's targets to the
  skinny `regen_css.rs` (`skinny/xtask/src/regen_css.rs`, confirmed present) and the skinny generated
  CSS runtime modules (`skinny/crates/runtime/src/grammars/css_l4_*/`). The framing constraints (Lock
  1/14, grammar-neutral, preserve-rich-ast) are correct; only the path attribution regresses to the
  wrong tree.
- **§4 demoted + §5 ledger text: ACCEPT.** The O5→O1+O2→O3→O4 spine and "micro-opt does not move the
  floor — banked, not to be relitigated" is the correct anti-regression posture.

### alphaE (candidate shortlist) — the cross-check target for CH3
- **§0 ground-truth anchors: ACCEPT.** Every anchor verified live (dispatch.rs, digit_mac.rs,
  generated.rs, nonjson_css_l4.rs). The **architecture-doc translation correction (αE:37-51) is the
  single best regression-hygiene move in the entire alpha set** — it explicitly converts the doc's
  core-tree symbols (`crates/core/...`, `StructLayout`, `OpenFrame`, `CssArena`) to skinny surfaces
  (`BackendRule` + `lower/tape_plan.rs` + `lower/{offset_tape,event_tape,eager_tape}.rs`; `regen_css.rs`)
  and pre-emptively states "CH1 will reject any goalset citing core-tree paths as the benched surface."
  This is exactly the defect that αC §2/§7, αD V6/O1/O2, and SYNTHESIS §0.1/§0.3 commit; αE alone is clean.
- **C0 de-fact-stream: ACCEPT.** Pre-block subsection (C0:102-109) correctly forbids emit_fact_stream
  admission / StructRegistry / eager-by-default / per-grammar type catalogue. No re-open.
- **C1 tape wiring: ACCEPT.** Pre-block subsection (C1:152-155) correctly forbids second tape /
  Vec<OpenFrame>::clone / columnar SoA / per-leaf Box::new. Skinny-pathed. No re-open.
- **C2 NEON pre-scan: ACCEPT.** Pre-block subsection (C2:205-213) forbids x86/AVX / cross-call carry /
  CSS-specific vocabulary; scalar-ref + checkasm present; NEON emits only Vec<u32> (Lock 1 transient
  producer). No re-open.
- **C3 commit-by-construction: ACCEPT.** Pre-block subsection (C3:255-257) forbids type-ambivalent dual
  representation / speculative-rollback disguise. REMOVES mechanism. No re-open.
- **C4 tailwind tuning: ACCEPT.** Pre-block subsection (C4:299-304) forbids x86/AVX-512/SVE / fixture
  capacity consts / per-leaf feature detection; scalar-ref + checkasm REQUIRED for the new i8mm kernel;
  the candidate's stated purpose is to RETIRE the digit_mac orphan (anti-orphan, not anti-regression
  re-open). No re-open.
- **§2 dependency order, §3 cross-cutting discipline, §4 escalation: ACCEPT.** N≥50 median + 8-field
  EXACT equality + grammar-neutral binds every gate; no-paper-close on C4 honored.

### SYNTHESIS.md (α-F output)
- **§0.1 close condition: REVISE.** `SYNTHESIS.md:55-56`. The "Tape activation" and "Layout-driven
  projection" gates cite `crates/core/src/runtime/tape/`, `bbnf_ir::registry::struct.rs` (`LayoutKind`/
  `FieldSource`), and `css_l4/builder.rs:274 ~40-arm match` as the deletion/wiring targets. These are
  **TOTALITY core-tree symbols absent from the benched skinny tree** (verified grep-clean). The
  regression hazard is concrete: a close condition instructing "the ~40-arm `match rule_id` is deleted"
  and "TapeStructBuilder/TapeCursor appear in the CSS parse path" cannot be verified by grepping the
  benched skinny tree, and the gate could pass against a tree that is not measured (or fail to fire on
  the skinny eager route). **Concrete fix:** adopt αE's translation (αE:37-51) verbatim into §0.1 —
  re-key the tape-activation gate to `skinny/crates/runtime/src/tape/` consumed by a skinny CSS parse
  path, and the layout-projection gate to the skinny codegen lowering (`tape_plan.rs`/`offset_tape.rs`/
  `regen_css.rs`), not `css_l4/builder.rs`.
- **§0.3 receiver goalset: REVISE.** `SYNTHESIS.md:84-85`. "Write the layout-walk accessor generator in
  `crates/core/src/backend/rust/emitter/`" and "Rewrite `regen_css.rs emit_builder` to select
  `TapeStructBuilder` (DELETE the OpenFrame template + the match rule_id begin_compound)". Same core-tree
  mis-attribution; `OpenFrame` is grep-absent from skinny. **Concrete fix:** re-path the generator to the
  skinny emitter location and re-key the "DELETE OpenFrame template" obligation to the skinny eager
  `CssStructBuilder` route in `skinny/xtask/src/regen_css.rs`. (Note: §0.3 says "The CSS parser becomes
  generic over `B: StructBuilder`" — confirm against the skinny trait, since the conversion report says
  "Generated parse fns are monomorphized to CssStructBuilder by name", `w6tape-conversion-report.md:55`.)
- **§0.4 pre-blocks: ACCEPT.** This is the canonical six-item pre-block ledger + inherited REDRESS
  families + hidden-coupling escapes, carried verbatim from CONTEXT and matching αC. Complete and correct
  for CH3. (Note: §0.4 stays correctly framed in terms of constructs — "eager per-leaf payload",
  "registry lookup in the per-leaf hot path", "second tape" — which are tree-agnostic, so the pre-block
  ledger itself does NOT suffer the path-attribution defect; only the close-condition/receiver gates do.)
- **§0.5 per-corpus close + §0.6 comparator gate: ACCEPT.** Per-corpus independent N≥50 median directly
  replaces the broadcast (PB#4); lightningcss-full-CSSOM-not-cssparser keeps the wrong-plane class out.
- **§1 ledger, §2 telemetry, §3 trajectory: ACCEPT.** Telemetry schema rejects sample_count<50 /
  non-median / fact-stream plane — the broadcast + N=1 regression tripwires are bound to the gate.

### HANDOFF.md (α-F output)
- **Current State + What SK-V17 Opens + Authority + Gate Posture: ACCEPT** (CH3-neutral framing).
  (Note: §Current State references the substrate location; if it cites core-tree, fold into the
  SYNTHESIS §0.1 path REVISE — the HANDOFF should inherit whatever §0.1 settles on.)
- **§Pre-Blocked Routes: ACCEPT.** `HANDOFF.md:82-109` reproduces the six pre-blocks + inherited REDRESS
  families + hidden-coupling escapes verbatim and binds them on S-P0..S-P3. Construct-framed (tree-agnostic),
  so no path defect. Complete.
- **§Next Move: ACCEPT.**

---

## Disposition counts

- Total artefact sections dispositioned: **34**
  (alphaA 8, alphaB 7, alphaC 8, alphaD 5, alphaE 9 [§0+C0..C4+§2+§3+§4 collapsed to the listed rows],
  SYNTHESIS 5 [§0.1, §0.3, §0.4, §0.5+§0.6, §1+§2+§3], HANDOFF 3 — counting the dispositioned units above).
- **ACCEPT: 28**
- **REVISE: 6** (alphaC §2, alphaC §7-row-2b, alphaD §1-V6, alphaD §3-O1/O2, SYNTHESIS §0.1, SYNTHESIS §0.3)
- **REJECT: 0**

All 6 REVISE share **one root cause**: core-tree path attribution (`crates/core/...`, `StructLayout`,
`OpenFrame`, `CssArena`, `css_l4/builder.rs`) where the benched surface is the skinny tree
(`skinny/crates/runtime/src/tape/`, `skinny/crates/codegen/src/lower/`, `skinny/xtask/src/regen_css.rs`).
αE already authored the fix (αE:37-51). The single corrective action is to propagate αE's translation
note into αC §2/§7, αD V6/O1/O2, and SYNTHESIS §0.1/§0.3.

## CH3 bottom line

**No candidate re-opens any REDRESS pre-block.** C0-C4 each carry a correctly-scoped pre-block
subsection, route through the tape+lazy-view "different framing", and the SIMD candidates attach
scalar-ref + checkasm + same-wave-consumer. **αC's pre-block list is correct and complete** (six
pre-blocks, PERMANENT vs ADMIT-UNDER-FRAMING split sound, citations verified). The §0.4 SYNTHESIS /
HANDOFF pre-block ledgers are construct-framed and tree-agnostic, so the regression tripwires
(eager per-leaf alloc, registry-in-hot-path, fact-stream admission, broadcast, FNV-arbiter, x86) are
correctly load-bearing. The **only** regression-relevant defect is path attribution: the deletion/
wiring GATES in αC §2, αD V6/O1/O2, and SYNTHESIS §0.1/§0.3 are written against the TOTALITY core tree,
which makes those specific tripwires unverifiable on the benched skinny surface — REVISE, not REJECT,
fully resolved by adopting αE's already-authored translation. No orphan REVISE: all six fold into one
corrective edit.
