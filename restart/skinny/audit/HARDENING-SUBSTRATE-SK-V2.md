# HARDENING-SUBSTRATE-SK-V2

## §1 — Target identification

- **Target**: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/SUBSTRATE.md`
- **Lines audited**: 1-594 (full file, post-iteration state)
- **Commit window**: after the user landed the 18-item SK-V1 redress and conducted three measured iterations (12-byte revert, dispatch-table revert, eager-decode masking surfacing). RESULTS.md final run dated 2026-05-09; REDRESS.md dated 2026-05-09.
- **Cycle**: SK-V2 (substrate); predecessor SK-V1 returned `SK-AMENDMENT-REQUIRED-NARROW` with 18 SUBSTRATE-side punch items + 2 cross-quadrant items (C6, C13) intersecting SUBSTRATE.
- **Lens stack applied**: Lanes 1-9 (Lane 2 N/A — single-wave) + Lenses F/G/H/I/J/K + Lenses L/M/N. Particular foci: Lens L (premise fidelity against measured NO-GO), Lens N (lazy-tape graduation mechanicality), Lens H (every cost claim must cite RESULTS.md row or REDRESS.md item).
- **Cross-quadrant cross-checks**: REDRESS.md items 13 (skipless reverted), 17 (dispatch table reverted), 18 (12-byte reverted), 19 (host-call split eager-decode MASKING); RESULTS.md outcome G across all three corpora; ARCHITECTURE.md §1433 (12-byte rejection landing) and §1411-1418 (sealing + token-economy gate).
- **Sister-quadrant deltas this cycle**: INDEX.md rows 6, 7 (Box<[T]>→private-Vec; HM hierarchy) landed; COMPILER.md §1.3 names two host-call probes; BENCH.md §3.4 materialization gate.
- **Time consumed**: 36 minutes audit + 4 minutes commit (within 40-minute cap).

The user's iteration evidence is settled: dispatch-table alternate INVALID (regressed); 12-byte skipless mixed (twitter regressed, citm improved, canada noise) and reverted to canonical 16-byte; host-call eager-decode is MASKING; lazy-offset tape remains the only honest untested structural lever. SK-V2 audits whether the spec text correctly reflects this empirical state.

## §2 — Cohort verdict

| Lane / Lens | Verdict | KEEP/FAITHFUL/MECHANICAL | REINVENT/CAVEAT | DISCARD/MASKING/ANTI-MECH | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 — Lock adherence | honoured-with-narrow-residue | 6 | 2 | 0 | SK-V1 #3 (parse-signature) and #5 (PAYLOAD_CLASS roster) not yet landed in SUBSTRATE.md |
| Lane 2 — Sequencing | N/A | — | — | — | Single-wave |
| Lane 3 — Cohesion | violated-narrow | 3 | 4 | 0 | SK-V1 #1 (arena counter API on `Tape`) STILL-OPEN; §3.4 mentions "BENCH compares" but no `Tape::payload_arena_writes()` declared |
| Lane 4 — SOTA anchoring | honoured | 4 | 0 | 0 | Lock 8 anchors unchanged |
| Lane 5 — Grammar-authoritative | violated-narrow | 3 | 2 | 0 | SK-V1 #7 (`STRING_*` flag rename) STILL-OPEN; §3.3 Prefilter verifier still JSON-bleeds |
| Lane 6 — LOC budget | silent-must-add | 0 | 1 | 0 | SK-V1 #14 STILL-OPEN — no per-section budget echo |
| Lane 7 — Friction | partial | 2 | 2 | 0 | SK-V1 #2 (`JsonRoot` overload) and §1.1 RECOVERY_KIND bit-layout STILL-OPEN |
| Lane 8 — Carry & deferral | violated | 3 | 3 | 1 | SK-V1 #11/#12/#13 STILL-OPEN; NEW: lazy-offset tape is the named-but-ungated structural lever (§589) |
| Lane 9 — Greenfield | honoured | 5 | 1 | 0 | Iteration evidence well-folded for token-economy gate; one apparatus residue (RECOVERY_KIND bits) |
| Lens F — LLM bias | violated-narrow | 3 | 3 | 0 | SK-V1 #6 (UTF-8 SAFETY discharge) STILL-OPEN; §317 wording "canonical skinny tape remains" elides the NO-GO state |
| Lens G — Overfitting | partial | 2 | 3 | 0 | `STRING_*` flag names STILL-OPEN; Prefilter verifier description still substrate-resident |
| **Lens H — Provenance** | **violated** | **2** | **3** | **2** | **NEW load-bearing fault.** §1.1, §1.2, §3.6, §7 cost claims now refutable by RESULTS.md but SUBSTRATE.md cites neither RESULTS.md nor REDRESS.md anywhere. The §313 amendments landed without provenance back-pointers. |
| Lens I — Contrivance | partial | 5 | 1 | 1 | SK-V1 #8 (RECOVERY_KIND bits) and #9 (visitor surface) STILL-OPEN |
| Lens J — Host leverage | violated-narrow | 3 | 1 | 0 | SK-V1 #6 (`from_utf8_unchecked` soundness) STILL-OPEN |
| Lens K — Meta-grammar discipline | honoured-with-residue | 2 | 1 | 0 | §3.3 string-content prefilter still substrate-bleeds JSON shape |
| **Lens L — Premise fidelity** | **MASKING-RESIDUAL** | **9** | **3** | **2** | **NEW load-bearing fault.** The eager-tape substrate is now empirically FAITHFUL-with-known-ceiling (12515/12988/8951 vs 21234/23238/13915 sonic, all NO-GO), but SUBSTRATE.md §0 still reads "if JSON cannot reach SOTA-parity through this substrate, that is strong negative evidence" — present tense, as if the verdict were undetermined. The substrate has been measured against the SOTA gate and missed by 1.5×-1.9×. The spec must classify the cut accordingly. |
| Lens M — Falsifiability | N/A | — | — | — | SUBSTRATE owns no thresholds |
| **Lens N — Graduation mechanicality** | **MECHANICAL with NEW underspecification** | **4** | **2** | **0** | The SK-V1 named inversions (Box<[T]>→private-Vec; HM hierarchy) survived steelman and landed in INDEX. NEW: the lazy-offset tape is now the empirically-required graduation surface, but SUBSTRATE.md §589 names it as "the untested structural lever" without classifying its V1 closure path (additive new field set? inversion of `payload_or_skip`?). Lens N classifies the lazy-offset deviation as **MECHANICAL-IF-PRE-EMPTED**: if V1 ships lazy-offset, the skinny's eager-offset `payload_or_skip` field becomes unused (additive). If lazy-offset requires breaking `TokenFlags` bit layout, the migration costs every consumer. The spec must name which. |

**Final readiness signal: SK-AMENDMENT-REQUIRED-NARROW.** SUBSTRATE.md survives the SK-V2 challenge structurally — every settled cut still defeats steelman, no MASKING is structural (only premise-classification), and the lazy-offset path is identifiable. But the iteration evidence has falsified one specific premise framing (the §0 "if JSON cannot reach SOTA-parity" hypothetical is now answered NO and not ack'd in-spec) and introduced a new graduation surface (lazy-offset) that the spec leaves under-classified. Six SK-V1 punch items are STILL-OPEN.

The amendment scope: classify the eager-tape substrate as FAITHFUL-with-known-ceiling against the measured NO-GO; promote lazy-offset tape from §10 open-question to §1.2 amendment-surface with named V1-closure; cite RESULTS.md / REDRESS.md per Lens H; close the six STILL-OPEN SK-V1 items.

## §3 — SK-V1 → SK-V2 disposition table

For each SUBSTRATE-relevant SK-V1 item, classify CLOSED / SUPERSEDED / STILL-OPEN / NEW with iteration citation.

| SK-V1 # | Item | Disposition | Evidence |
|---:|---|---|---|
| 1 | `Tape::payload_arena_writes()` API surface mismatch | **STILL-OPEN** | SUBSTRATE.md §2 still exposes `PayloadArena::write_count()` only; no method on `Tape`. RESULTS.md confirms "Track 1 0/0 writes/allocations" so BENCH side calls *something* that works, but the API drift between SUBSTRATE.md §2 and BENCH §3.4 is not visibly closed in the spec. |
| 2 | `JsonRoot` identifier overload (kind marker vs typed view) | **STILL-OPEN** | SUBSTRATE.md §1.3:129 still declares `pub enum JsonRoot {}` as kind marker; §4.1:331 still declares `pub struct JsonRoot<'doc, 'input>` as typed view. No `JsonRootKind` rename. Cohort C13 (cross-quadrant signature) updated the COMPILER side; the SUBSTRATE-side overload persists. |
| 3 | Parse-API signature trifurcation | **PARTIALLY-CLOSED** | SUBSTRATE.md §1.3:117 still reads `parse(&'a [u8])`; §4.3:420 reads `parse(&'a str)`. The internal contradiction persists. C13 (consolidated) named `parse<'i>(input: &'i str)` as the settled signature; SUBSTRATE has not propagated. |
| 4 | `JsonDocument` vs `JsonRoot` return type | **STILL-OPEN** | §1.4:151 keeps `JsonDocument`; §4.1:331 keeps `JsonRoot` typed view. The redress between them is unspecified. |
| 5 | `INLINE_STRING_BORROW` payload class undefined in §1.1 enum | **STILL-OPEN** | §1.1:50-57 enumerates `INLINE_BOOL_NULL`, `INLINE_NUMBER_FAST`, `ARENA_OFFSET`, `SIBLING_SKIP`. §2:181 still references `INLINE_STRING_BORROW`. Vocabulary still incomplete. |
| 6 | `unsafe { str::from_utf8_unchecked }` soundness | **STILL-OPEN** | §4.1:382 retains `unsafe { std::str::from_utf8_unchecked(raw) }` with the same "SAFETY: JSON parser validated UTF-8 boundaries during the structural scan's verifier route" comment. The SIMD structural scan does not validate UTF-8; the SAFETY discharge is incorrect. |
| 7 | `STRING_BORROWS_SOURCE` / `STRING_NEEDS_UNESCAPE` flag names overfitted | **STILL-OPEN** | §1.1:38-41 unchanged. JSON-shaped names persist on grammar-neutral token. |
| 8 | `RECOVERY_KIND` token bits dead in skinny | **STILL-OPEN** | §1.1:44 still declares `RECOVERY_KIND (2)` bits in skinny TokenFlags. §7 still omits recovery. The 2 bits remain dead. |
| 9 | Visitor §6 surface over-shipped | **STILL-OPEN** | §6:445-491 still emits full `JsonVisitor` trait with default `walk_*` and `visit_*` methods. No `for_each_value` collapse. |
| 10 | `decode_string` graduation path underspecified | **PARTIALLY-CLOSED-by-iteration; NEW concern** | REDRESS item 19 surfaces the host-call split: dispatch overhead PASSES (≤50ns), eager-decode is MASKING (>1.15× T1 twitter, >1.08× citm, >1.02× canada). The host-fn-free skinny is now empirically FAITHFUL only if V1 keeps decode lazy. SUBSTRATE.md §4.1:374 keeps `Cow<'input, str>` lazy — consistent with the empirical finding — but does not name the V1 closure constraint. The FAITHFUL-conditional posture is in COMPILER.md §1.3, not SUBSTRATE.md. |
| 11 | `OwnedDocument` V1 receiver crate | **STILL-OPEN** | §1.4:167 still says "cold OwnedDocument wrapper"; no `runtime/src/owned/` reference. |
| 12 | `Option<Box<PayloadArena>>` trigger gate | **STILL-OPEN** | §2:219 keeps the speculative paragraph; no bench-outcome trigger named. |
| 13 | `parse-that` number kernel V1 receiver | **STILL-OPEN** | §4.1:396 still reads "parse-that's number kernel here post-skinny; for the skinny, std::str::FromStr" — same hedge. |
| 14 | LOC budget echo from WORKSPACE | **STILL-OPEN** | §9:559-577 module layout still carries no per-file LOC commitment. |
| 15 | `JsonObjectOpen → 0u16` illustrative clarification | **CLOSED-substantively** | §1.1:71 added a sentence "JSON close kinds remain reserved in the generated kind table for diagnostics/recovery and V1 grammars that need explicit close events, but the SOTA JSON tape emits zero close tokens." Close-kind framing is now correct; the "illustrative" disclaimer was not added inline but the surrounding prose ("the mapping ... is generated per grammar; the substrate never sees the names") at §1.1:67 still anchors Lock 14 correctness. Audit-acceptable. |
| 16 | `ReparsePlan` citation drift | **PARTIALLY-CLOSED-by-INDEX** | INDEX.md row 6 anchors the sealing deviation; the inline ARCH §3.3 cite was dropped in favor of "incremental reuse map (`ReparsePlan`)" without the line cite in the redress note at §1.2:101. Citation drift no longer asserted; provenance gap silently closed. Acceptable. |
| 17 | `build_tape_for_json` Lock 14 home | **STILL-OPEN** | §8:531 unchanged: "`build_tape_for_json` ... Public to the workspace; gated by `#[doc(hidden)]`". Home crate still ambiguous between `runtime/src/tape/builder.rs` (Lock 14 violation: per-grammar in generic crate) and `bbnf-bench/src/track2/json.rs`. |
| 18 | §3.3 string-content prefilter location | **STILL-OPEN** | §3.3:282 still carries the JSON-specific prefilter verifier ("looking for the closing `\"` past escapes") in the grammar-neutral SIMD section. |

**Cross-quadrant SK-V1 items affecting SUBSTRATE**:

| C# | Item | Disposition |
|---:|---|---|
| C6 | TapeBuilder cite from BENCH §1.2 | OUT-OF-SUBSTRATE-SCOPE; SUBSTRATE side is correct |
| C13 | Parse signature drift | **STILL-OPEN at SUBSTRATE.md** — SK-V1 #3 above. C13 named `parse<'i>(input: &'i str)` settled; SUBSTRATE.md has not adopted. |

**SUPERSEDED items (the iteration invalidated the SK-V1 finding):**

None. The user's iteration touched orthogonal axes (token width, dispatch shape, host-call split) — none of the 18 SUBSTRATE-side SK-V1 items were targeted by the iteration and none are invalidated.

**NEW items (SK-V2 surfaces; SK-V1 did not catch):**

| N# | Item | Source |
|---|---|---|
| N1 | **§0 SOTA-viability premise framing is now empirically refuted but still reads as hypothetical** | RESULTS.md outcome G across all three corpora; §0:7 reads "if JSON cannot reach SOTA-parity through this substrate, that is strong negative evidence." Verdict is now in; spec must classify the eager-tape cut as FAITHFUL-with-known-ceiling. Lens L. |
| N2 | **Lazy-offset tape is now the named architectural amendment surface but classified only as a §10 open question** | REDRESS items 13, 17, 18 + summary line "Lazy-offset tape remains the untested structural lever"; SUBSTRATE.md §589 mentions it as an open question. After three failed perturbations (dispatch-table, pair-token fusion, 12-byte skipless), it is the ONLY honest remaining honest substrate move. Lens N requires classifying its V1-closure path. |
| N3 | **Provenance gap: every §1, §3.6, §7 cost claim is now refutable by RESULTS.md but SUBSTRATE.md cites neither RESULTS.md nor REDRESS.md** | `grep -n "RESULTS\|REDRESS"` returns zero matches in SUBSTRATE.md. Per the user-stated SK-V2 Lens H tightening, hand-waved "expected" claims need measurement cites. Lens H. |
| N4 | **Eager-decode MASKING posture absent from SUBSTRATE.md** | REDRESS item 19 documents the empirical finding; COMPILER.md §1.3 carries it. SUBSTRATE.md §2 payload-arena policy claims "zero arena allocations on the JSON hot path" without acknowledging that V1's `decode_json_string_to_arena` host-fn would FALSIFY that claim. Substrate spec must encode the FAITHFUL-conditional. Lens L. |
| N5 | **§317 wording elides NO-GO state** | "the canonical skinny tape remains the 16-byte token stream with explicit pair tokens until such a perturbation is authorized." The wording suggests stasis ("remains") with conditional revision ("until authorized"). The actual state is: three perturbations measured, three rejected, NO-GO holds; lazy-offset is the only remaining authorized lever. The wording should encode that the canonical substrate is currently *the substrate that produced NO-GO*. Lens F (hedging where commitment is needed). |

## §4 — Lane 1 — Lock adherence

| Site | Lock | Item | Verdict | Notes |
|---|---|---|---|---|
| §1.2:73-101 (with redress note) | Lock 1 | Tape is THE substrate; private-Vec semantic sealing | **KEEP** | Redress note at §1.2:101 names the V1 graduation path; INDEX row 6 ratifies. Steelman ("private-Vec hides allocation residency") defeated at §1.2:101 ("the bench reports both logical tape bytes and allocated tape bytes so this throughput win does not hide memory residency"). Iteration evidence (RESULTS.md: twitter logical 1.03× / allocated 1.69×; citm 0.83× / 1.36×; canada 1.19× / 1.59×) confirms the residency is reported. |
| §1.3:117 vs §4.3:420 | Lock 1 (silent) + Lock 9 | Parse-API signature | **REINVENT** | SK-V1 #3 STILL-OPEN. C13 settled at `&str`; SUBSTRATE has not propagated. |
| §1.4:155-167 | Lock 9 | `parse_in` shape silent | **STILL-CARRY** | SK-V1 #11 (`OwnedDocument` receiver). The iteration did not exercise this axis. |
| §3.5:303-310 | Lock 8 | SIMD throughput targets | **KEEP** | ARCH §11:1519 anchors unchanged. Steelman defeated. |
| §1.2:101 + INDEX row 6 | Lock 13 | Sealing graduation | **KEEP** | The Box→private-Vec inversion is correctly graduation-mechanical per Lens N. |
| §9 module layout | Lock 13 | Directory shape | **KEEP** | 6 children under `tape/`; passes child-count rule. |
| §3.1:243-256 | Lock 14 | ISA dispatch only, no grammar arms | **KEEP** | Unchanged. |
| §1.1:67-71 | Lock 14 (narrow) | NodeKindId mapping clarification | **CLOSED** | SK-V1 #15 substantively closed by the close-kind reservation sentence. |

Lane 1 verdict: **honoured-with-narrow-residue**. Two STILL-OPEN items: parse signature (#3) and parse_in shape (#11/#12). No new locks-relevant faults from the iteration.

## §5 — Lane 3 — Cohesion

| Site | Item | Verdict | Notes |
|---|---|---|---|
| §2:217 vs BENCH §3.4 | `Tape::payload_arena_writes()` API | **REINVENT** | SK-V1 #1 STILL-OPEN. RESULTS.md row 34 reports "Track 1 0/0 writes/allocations" — the assertion fires somehow, but the API in SUBSTRATE.md §2 (`PayloadArena::write_count()`) is not the surface BENCH §3.4 calls (per SK-V1 audit: `t1.payload_arena_writes()`). The spec drift persists. Falsifiability of the zero-arena pillar remains brittle. |
| §1.3:127 + §4.1:331 | Two `JsonRoot` types | **REINVENT** | SK-V1 #2 STILL-OPEN. |
| §1.4:151 vs §4.1:331 | `JsonDocument` vs `JsonRoot` return | **REINVENT** | SK-V1 #4 STILL-OPEN; C13 partial-close on signature does not address return type. |
| §1.1:46-57 vs §2 table | `INLINE_STRING_BORROW` undefined | **REINVENT** | SK-V1 #5 STILL-OPEN. |
| §317 vs REDRESS items 13, 16, 17, 18 | Iteration evidence properly folded | **KEEP** | §313-§317 amendment lands clean: close-token elision adopted (REDRESS 13), pair-token fusion rejected (REDRESS 16), dispatch-table rejected (REDRESS 17), 12-byte skipless rejected (REDRESS 18). The reverts are documented; this is the audit's highest-quality redress landing. |
| §1.2:101 vs RESULTS.md tape bytes | Sealing redress reports both logical and allocated | **KEEP** | Iteration evidence (twitter 1.03×/1.69×, etc.) is materialized in RESULTS.md per spec promise. Provenance complete by reference even if not cited. |
| §8:531 home crate undeclared | `build_tape_for_json` Lock 14 ambiguity | **REINVENT** | SK-V1 #17 STILL-OPEN. |

Lane 3 verdict: **violated-narrow**. Three SK-V1 cohesion items (#1, #2, #4) are STILL-OPEN. The §313-§317 redress for the iteration evidence is exemplary: every rejected perturbation is documented inline with mixed-result evidence. That redress alone would have been a satisfactory SK-V2 close — but the SK-V1 punch list was supposed to land in parallel.

## §6 — Lane 4 — SOTA anchoring

| Site | Item | Verdict |
|---|---|---|
| §0:11, §0:17, §3.5 | sonic-rs / simdjson / lightning-css; ≥56000 Mbps AVX2; ≥40000 Mbps NEON | **KEEP** — unchanged from SK-V1; Lock 8 anchored; RESULTS.md confirms scan floor met (canada 66565 Mbps > 40000 floor) |
| §3.4 parity hash | mandatory | **KEEP** |

Lane 4 verdict: **honoured**. The iteration evidence confirms scan-floor compliance; the substrate's SOTA-anchored claims survive.

## §7 — Lane 5 — Grammar-authoritative discipline

| Site | Item | Verdict |
|---|---|---|
| §1.1:35-44 `STRING_BORROWS_SOURCE`, `STRING_NEEDS_UNESCAPE` | JSON-shaped flag names | **REINVENT** — SK-V1 #7 STILL-OPEN |
| §3.3:282 string-content Prefilter description | JSON-specific verifier in substrate section | **REINVENT** — SK-V1 #18 STILL-OPEN |
| §4.1:382 `unescape_json` in per-grammar view.rs | Lock 14 honoured (per §9 path) | **KEEP** |
| §3.1 ISA dispatch (no grammar arms) | **KEEP** | Unchanged |
| §1.1:67-71 close-kind reservation | **KEEP** | Closure note for non-JSON grammars now explicit |

Lane 5 verdict: **violated-narrow**. Two SK-V1 items STILL-OPEN.

## §8 — Lane 6 — LOC budget

SK-V1 #14 STILL-OPEN. SUBSTRATE.md §9 module layout still echoes no per-file LOC commitment from WORKSPACE §2. **silent-must-add.**

## §9 — Lane 7 — Friction forecast

| Surface | Verdict |
|---|---|
| `JsonRoot` overload | **REINVENT** — SK-V1 #2 |
| `'doc`/`'input` discriminant | KEEP-with-cookbook-carry |
| `payload_or_skip` discriminator | KEEP (commentary present at §1.1:65 in redress) |
| parse signature ambiguity | **REINVENT** — SK-V1 #3 |
| **NEW**: §0:7 "if JSON cannot reach SOTA-parity ... that is strong negative evidence" reading hypothetically when the answer is known | The reader who has read RESULTS.md (NO-GO) and then reads SUBSTRATE.md §0 will mistake the substrate spec for pre-iteration scope. Implementor friction is HIGH because the spec does not encode the empirical answer. Friction surface = "the §0 framing tells me to discover whether the substrate is SOTA-faithful; I have already discovered it isn't." Verbatim suggested rephrase: "The substrate has been measured against the SOTA gate (`skinny/RESULTS.md`) and lands NO-GO at 53-74% of sonic-rs across twitter/citm/canada. The substrate cut is therefore FAITHFUL-with-known-ceiling: the eager tape's materialization cost is the measured bottleneck. The remaining honest perturbation lever is the lazy-offset tape (see §1.2 amendment surface)." | **REINVENT** |

Lane 7 verdict: **partial — two new friction surfaces beyond SK-V1**.

## §10 — Lane 8 — Carry & deferral

| Site | Carry | Verdict |
|---|---|---|
| SK-V1 #11 `OwnedDocument` receiver | STILL-OPEN | **REINVENT** |
| SK-V1 #12 `Option<Box<PayloadArena>>` trigger | STILL-OPEN | **REINVENT** |
| SK-V1 #13 `parse-that` number kernel | STILL-OPEN | **REINVENT** |
| §10 open-question list (4 entries) | The framing is honest open-question | **KEEP** — but see §11 below |
| **NEW**: §589 lazy-offset tape carry | "Lazy-offset tape remains the untested structural lever" | **REINVENT** — see §11 |

The lazy-offset carry merits separate treatment.

## §11 — Lane 8 amplified — the lazy-offset tape carry (NEW)

REDRESS.md summary line: "Lazy-offset tape remains the untested structural lever." REDRESS items 13, 17, 18 record three measured rejections (skipless 12-byte; dispatch table; pair-token fusion) and item 16 records pair-token fusion rejection. The empirical fact: after four perturbations of the eager tape, NO-GO holds. The remaining honest move is lazy-offset.

SUBSTRATE.md §10:589 names this as an open question:

> "**Skipless narrow token.** A 12-byte token that drops `payload_or_skip` and derives subtree skips from spans was tested in the prototype. It reduced logical tape bytes but did not produce a clean throughput win, so it is not the canonical skinny substrate. Lazy-offset tape remains the untested structural lever."

The framing is half-correct. The skipless rejection is documented; lazy-offset is named. But the lazy-offset lever is the **architectural amendment surface** that the iteration empirically pointed at — it deserves §1.2 status as a candidate substrate, not §10 open-question status.

Steelman: "the spec is honest — open questions belong in §10; promoting lazy-offset to §1.2 would be premature commitment without measurement."

**Defeat**: §10's purpose is "open questions surfaced for the orchestrator ... the bench result will turn them into commit-able decisions." The bench result has already turned the eager-tape three perturbations into rejections; the remaining bench question is whether lazy-offset beats the gate. That makes lazy-offset *the* commit-able-pending-measurement amendment, not one of four equal open questions. Spec should:

1. Promote lazy-offset to a §1.2 amendment-surface paragraph (alongside the existing Box→private-Vec amendment note).
2. Classify its V1-closure cost per Lens N (see §17 below).
3. Re-classify the other three §10 open questions (union vs split fields, NodeKindId width, Arc<Tape>) as orthogonal-to-current-NO-GO.

This is the load-bearing SK-V2 architectural recommendation.

Lane 8 verdict: **violated**. 3 STILL-OPEN carries + 1 NEW under-classified carry = 4 REINVENT items.

## §12 — Lane 9 — Greenfield discipline

| Site | Item | Verdict |
|---|---|---|
| §313-§317 token-economy gate | The iteration evidence (close-token elision adopted, pair-token rejected, 12-byte rejected, dispatch-table rejected) is folded as authoritative substrate amendment | **KEEP** — exemplary greenfield landing |
| §1.2:101 sealing redress note | Box→private-Vec graduation path named; trade-off steelmanned | **KEEP** |
| §1.1:38-41 RECOVERY_KIND bits | Dead apparatus | **REINVENT** — SK-V1 #8 |
| §6:445-491 visitor surface | Over-shipped | **REINVENT** — SK-V1 #9 |
| §317 wording "remains ... until such a perturbation is authorized" | Hedges around the NO-GO state | **REINVENT** — see Lens F |

Lane 9 verdict: **honoured-narrow**. The iteration evidence is well-folded for what landed (§313-§317); the SK-V1 apparatus residues remain.

## §13 — Lens F — LLM bias

| Site | Pathology | Verdict |
|---|---|---|
| §4.1:382 SAFETY comment | Confident generality (the discharge is wrong) | **REINVENT** — SK-V1 #6 STILL-OPEN |
| §1.1:58 "the SOTA target lives or dies by token-cache density" | Buzzword reliance | SK-V1 noted; STILL-OPEN |
| §1.2:80 "improves codegen for traversal" | Unfalsifiable | SK-V1 noted; STILL-OPEN |
| §0:7 "if JSON cannot reach SOTA-parity through this substrate, that is strong negative evidence" | Hedging where commitment is needed — the empirical answer (NO-GO at 53-74%) is in; the hypothetical framing reads as if it were not | **NEW REINVENT** (Lens F + Lens L) |
| §317 "remains ... until such a perturbation is authorized" | Hedging where commitment is needed — three perturbations are rejected; the spec should say "until lazy-offset tape or another named amendment lands" | **NEW REINVENT** |
| §2:217 "if the bench shows arena cache pressure...field becomes `Option<Box<PayloadArena>>` behind a feature gate" | Hedging where commitment is needed — bench has run; arena counters are 0/0; the conditional is moot for current iteration | **REINVENT** — SK-V1 #12 STILL-OPEN; iteration evidence settles "zero pressure" |

Lens F verdict: **violated-narrow**. Three SK-V1 items + two new framings drift from commitment.

## §14 — Lens G — Overfitting

Unchanged from SK-V1. `STRING_*` flag names and §3.3 prefilter description still substrate-bleed. **violated — SK-V1 #7, #18 STILL-OPEN.**

## §15 — Lens H — Hallucination + provenance (LOAD-BEARING FOR SK-V2)

The user's SK-V2 dispatch named Lens H as load-bearing: "every spec claim about substrate cost must cite RESULTS.md row or REDRESS.md log entry. Hand-waved 'expected' claims are now refutable; they must cite measurement."

| Site | Claim | Required citation | Status |
|---|---|---|---|
| §0:7 "if JSON cannot reach SOTA-parity through this substrate, that is strong negative evidence" | The premise statement | RESULTS.md (outcome G; T1 53-74% of sonic) | **VIOLATED — STILL-HYPOTHETICAL** |
| §1.1:58 "the SOTA target lives or dies by token-cache density" | Cost claim | RESULTS.md materialization rows (twitter logical 1.03×, allocated 1.69× — measured token-cache pressure) | **VIOLATED — uncited** |
| §1.1:65 "carrying both eagerly costs a cache line per token-pair" | Refutable cost claim | A microbench / theoretical calc (cache-line per token-pair = 64 bytes / 32 bytes = 2 vs 1 token; the +50% cost is mechanism but cite ARCH or measurement) | **VIOLATED — uncited** |
| §1.2:80 "Box<[T]> ... improves codegen for traversal" | Cost claim | REDRESS item 15 inverted this — private-Vec sealing wins by AVOIDING shrink/copy; the SK-V1 Lens F flag is now stronger because the iteration shifted from Box to private-Vec | **VIOLATED — invert claim by iteration; cite REDRESS item 15** |
| §1.2:101 sealing redress note | Iteration cause | REDRESS item 15 | **VIOLATED — uncited; cite required per Lens H tightening** |
| §2:219 "zero arena allocations and zero arena writes on the JSON hot path" | Measurable claim | RESULTS.md row "Track 1 0/0 writes/allocations" for all three corpora | **VIOLATED — uncited though confirmed by measurement** |
| §3.6:317 token-economy gate redress | Three perturbations rejected | REDRESS items 13 (close-token), 16 (pair-token fusion), 17 (dispatch-table), 18 (12-byte skipless) | **PARTIALLY-VIOLATED — the perturbations are described inline but RESULTS.md/REDRESS.md not cited as the evidence locus** |
| §7 omissions table | Each "why omitting it does not compromise the SOTA test" claim | Either RESULTS.md (for cuts the bench did exercise) or "orthogonal-by-construction" (for cuts the bench did not touch) | **VIOLATED — none cite RESULTS.md; eager-decode MASKING (REDRESS item 19) particularly should be referenced for the host-fn-free row** |
| §10:589 "Lazy-offset tape remains the untested structural lever" | Implicit citation | REDRESS items 13, 16, 17, 18 (the four perturbations that converged on this) | **VIOLATED — the convergence-on-lazy-offset is the spec's most important provenance and is uncited** |

**Lens H verdict: VIOLATED.** This is the load-bearing SK-V2 fault. SUBSTRATE.md folded the iteration evidence at §313-§317 *as content* but did not cite the iteration evidence *as provenance* — the result is a spec that has been updated to match measurement but still reads as if the measurement were forthcoming. Surgery: add a "Iteration Evidence" note at §0 (or alongside it) with a paragraph mapping §1.2 (sealing), §3.6 (token economy), §7 (host-fn-free) claims to specific RESULTS.md rows and REDRESS.md item numbers.

This is the most consequential SK-V2 finding. The iteration has settled facts the spec author still describes as conjecture.

## §16 — Lens I — Contrivance

SK-V1 #8 (RECOVERY_KIND), #9 (visitor surface) STILL-OPEN. No new contrivance from iteration. **partial.**

## §17 — Lens J — Host-language leverage

SK-V1 #6 (`from_utf8_unchecked`) STILL-OPEN. Iteration did not exercise this axis. **violated-narrow.**

## §18 — Lens K — Meta-grammar discipline

SK-V1 §3.3 string-content prefilter substrate-bleed STILL-OPEN. **honoured-with-residue.**

## §19 — Lens L — Premise fidelity (LOAD-BEARING)

The user's SK-V2 dispatch named Lens L load-bearing: "Verify SUBSTRATE.md now ACCURATELY classifies the eager-tape substrate as FAITHFUL-with-known-ceiling rather than FAITHFUL-unconditionally. The 12-byte-token revert must be documented as a measured deviation rejection, not a silent reversal."

| Cut | SK-V1 verdict | SK-V2 verdict | Iteration evidence |
|---|---|---|---|
| 16-byte token (eager `payload_or_skip`) | FAITHFUL | **FAITHFUL-WITH-KNOWN-CEILING — must reclassify in §1.1 and §0** | RESULTS.md outcome G (53-74% of sonic across three corpora) is the measured ceiling. SK-V1 classified this FAITHFUL on the assumption parity would hold; the parity did not hold. The cut is still FAITHFUL (the substrate did test the V1 SOTA premise) but the test verdict is NO-GO. The spec must encode the FAITHFUL-with-measured-ceiling state. |
| Recovery omission | FAITHFUL | **FAITHFUL — KEEP** | Valid-input bench did not exercise; no change |
| Closure environment | FAITHFUL-with-V1-grammar-caveat | **KEEP** | No change |
| `OwnedDocument` cold wrapper | FAITHFUL | **KEEP** | No change |
| `JsonObject::get` PHF cache omission | FAITHFUL | **KEEP** | No change |
| Eager number parse omission | FAITHFUL | **KEEP** | No change |
| Multi-grammar tape kind sharing | FAITHFUL | **KEEP** | One grammar; no change |
| `@layout` omission | FAITHFUL-with-V1-grammar-caveat | **KEEP** | No change |
| Visitor §6 over-shipping | FAITHFUL (Lens I SIMPLIFY) | **KEEP** | No change |
| **NEW**: host-fn-free + eager-decode | (not classified in SK-V1; iteration surfaced) | **FAITHFUL-CONDITIONAL on lazy-decode V1 graduation** | REDRESS item 19: eager-decode is MASKING; dispatch is FINE. The host-fn-free cut is FAITHFUL ONLY IF V1 keeps decode lazy. SUBSTRATE.md §2 (zero-arena claim) is true under this condition; the condition is not encoded. |
| **NEW**: dispatch-table substitute | (not in SK-V1) | **FAITHFUL by iteration** | REDRESS item 17: real function-pointer table regressed; canonical match dispatch holds. The lever is empirically refuted; spec correctly retains match. SUPERSEDES C7 (consolidated SK-V1 cross-platform plan divergence). |
| **NEW**: 12-byte skipless token | (not in SK-V1; was a §10 open question) | **FAITHFUL by iteration** | REDRESS item 18: mixed (twitter regressed, citm improved, canada noise); reverted. SUBSTRATE.md §317 and §589 document the rejection — correctly. |
| **NEW**: lazy-offset tape | (not in SK-V1) | **UNTESTED, GRADUATION-CANDIDATE** | REDRESS summary line. The remaining structural lever; classification deferred. See §17 below. |

**Lens L verdict: MASKING-RESIDUAL — premise framing is one structural amendment short.** Every substrate cut survives steelman against the iteration evidence; the structure of the substrate is sound; the test it ran is FAITHFUL. The spec text's *framing* of the premise as undetermined is the residual fault.

The §0 ("0. Scope and stance") paragraph must be amended to encode:
1. The eager-tape substrate has been measured.
2. Outcome G / NO-GO at T1 53-74% of sonic-rs is the measured verdict.
3. The substrate cut is therefore **FAITHFUL-with-known-ceiling**: the test ran honestly; the ceiling is the empirical bottleneck.
4. The remaining honest structural amendment is lazy-offset tape (forward-reference to §1.2 amendment surface).

The 12-byte revert IS documented (§317, §589) — SK-V2 Lens L confirms this is not a silent reversal. The dispatch-table revert IS documented (§317). The pair-token fusion rejection IS documented (§317). The iteration's structural rejections are spec-correctly folded.

The fault is exclusively in the premise *framing* (§0), not in the substrate's mechanical surfaces.

## §20 — Lens M — Falsifiability

N/A for SUBSTRATE.md — falsifiability is BENCH's lens. Note: SK-V1's "falsifiability of zero-arena-writes broken by API mismatch" survives as SK-V1 #1 STILL-OPEN.

## §21 — Lens N — Graduation mechanicality (LOAD-BEARING SK-V2)

The user's SK-V2 dispatch named Lens N as the architectural amendment surface: "the lazy-tape route is now the architectural amendment surface. Verify SUBSTRATE.md correctly identifies which V1 graduation moves are MECHANICAL vs ANTI-MECHANICAL given the empirical ceiling."

| Deviation | V1 closure | Cost | Verdict |
|---|---|---|---|
| Box<[T]> → private-Vec sealing | INDEX row 6 ratifies; iteration validated; read API unchanged | ~50 LOC additive; the Vec → Box→Vec migration is internal | **MECHANICAL with named inversion — KEEP** (survives steelman: "private-Vec hides allocation" defeated by the bench reporting both logical and allocated bytes) |
| HM hierarchy inversion | INDEX row 7 ratifies; COMPILER §9.1 names the wrapper | ~20-30 LOC | **MECHANICAL with named inversion — KEEP** |
| `payload_or_skip` union | PASS-3 §4 admits illustrative; spec defends with discriminator argument | 0 LOC for consumers (all go through PAYLOAD_CLASS) | **MECHANICAL — KEEP** |
| `parse-that-regex` directory promotion | Trivial | 0 LOC | **MECHANICAL trivial — KEEP** |
| `wasm = false` metadata | V2 flag flip | 0 LOC | **MECHANICAL trivial — KEEP** |
| `passes` HM-only constraint | Additive | KEEP | |
| `@host fn` decode-string add | Per-grammar; lock 14 honoured | Iteration evidence (REDRESS 19) imposes FAITHFUL-conditional: V1 must keep decode lazy | **MECHANICAL-CONDITIONAL** — graduation is mechanical IFF V1 emits `decode_json_string` as a lazy call (matching skinny §4.1:382's Cow), not as a parse-time `decode_json_string_to_arena`. The spec must encode the conditional. |
| **NEW: lazy-offset tape** | Spec §589: "the untested structural lever" | Unclassified | **MECHANICAL-IF-PRE-EMPTED** — see analysis below |

**Lazy-offset tape Lens N analysis:**

The lazy-offset tape replaces the eager `start: u32 + end: u32 + payload_or_skip: u32` triplet with a tape that stores only `kind + flags` per token and recovers `start`/`end`/`skip` from a side-table of structural offsets at projection time. The graduation analysis:

| V1 closure axis | Mechanical-if | Anti-mechanical-if |
|---|---|---|
| TapeToken field set | additive: lazy-offset is a NEW TapeToken variant or NEW Tape struct (e.g., `LazyTape<'input>` alongside `Tape<'input>`); skinny eager Tape survives unchanged for grammars where eager is faster | rewrite: lazy-offset REPLACES the 16-byte token; every consumer of `tokens[i].start` must change |
| ValueRef cursor | unchanged if lazy-offset exposes the same `start()`/`end()` methods (projection from side-table) | breaks if lazy projects through a different cursor type |
| TapeBuilder | additive: new `LazyTapeBuilder` alongside `TapeBuilder` | rewrite: TapeBuilder must change emission shape |
| BENCH parity oracle | unchanged if `(TapeId, index)` identity invariant holds | breaks if token indices differ between eager and lazy tapes |

The fault: SUBSTRATE.md §589 names the lever without specifying which closure axis it pre-empts. The spec must commit to: "lazy-offset tape is an additive `LazyTape<'input>` substrate; the eager `Tape<'input>` is preserved as a per-grammar choice; lazy-offset is MECHANICAL by construction." Or commit to: "lazy-offset REPLACES eager Tape; eager 16-byte becomes a 0-LOC deprecation; the migration is one-commit because consumers go through `ValueRef::start()` methods that absorb the projection." Either commitment is acceptable; the under-specification is not.

**SK-V2 surgery for Lens N**: SUBSTRATE.md §1.2 (or new §1.5) adds:

> "**Lazy-offset tape amendment surface.** After the iteration's four structural perturbations (close-token elision adopted; pair-token fusion rejected per REDRESS item 16; dispatch-table rejected per REDRESS item 17; 12-byte skipless rejected per REDRESS item 18), the remaining honest substrate move is lazy-offset tape: TapeToken collapses to `(kind, flags)` and `start`/`end`/`skip` lazily project from a side table of structural offsets. Lens N classification: ADDITIVE-MECHANICAL — lazy-offset ships as `LazyTape<'input>` alongside the eager `Tape<'input>`; per-grammar metadata selects which substrate the parser emits against; the `ValueRef<'doc, 'input, K>` cursor type and its `.start()`/`.end()` methods are unchanged because they go through trait projection. V1 closure cost: ~400-800 LOC additive (new struct + builder + projection trait impl), zero rewrite of `Tape`/`ValueRef`/typed views. Bench-condition for graduation: lazy-offset must beat eager Mbps on twitter/citm/canada by ≥10% on a future SK-V3 bench row."

**Lens N verdict: MECHANICAL with NEW under-classified amendment surface.** Surgery is one paragraph and one INDEX deviation ledger row.

## §22 — Cross-cutting fault summary

| # | Fault | Disposition | Severity |
|---|---|---|---|
| F1 | Lens H: zero RESULTS.md / REDRESS.md cites for refutable cost claims | NEW; SK-V2 load-bearing | high — spec-evidence drift |
| F2 | Lens L: §0 premise framing reads hypothetical against measured NO-GO | NEW; SK-V2 load-bearing | high — premise-classification drift |
| F3 | Lens N: §589 lazy-offset tape is the empirically-required amendment surface but unclassified | NEW; SK-V2 load-bearing | high — architectural under-specification |
| F4 | SK-V1 #1 `Tape::payload_arena_writes()` API | STILL-OPEN | high — falsifiability of zero-arena pillar |
| F5 | SK-V1 #6 `from_utf8_unchecked` soundness | STILL-OPEN | high — soundness |
| F6 | SK-V1 #3 parse-API signature | STILL-OPEN; cohort C13 settled at COMPILER but not propagated to SUBSTRATE | high — three-way drift |
| F7 | SK-V1 #4 `JsonDocument` vs `JsonRoot` return | STILL-OPEN | medium |
| F8 | SK-V1 #5 `INLINE_STRING_BORROW` undefined | STILL-OPEN | medium |
| F9 | SK-V1 #2 `JsonRoot` identifier overload | STILL-OPEN | medium |
| F10 | SK-V1 #7 `STRING_*` flag names | STILL-OPEN | low |
| F11 | SK-V1 #8 `RECOVERY_KIND` bits dead | STILL-OPEN | low |
| F12 | SK-V1 #9 visitor surface over-shipped | STILL-OPEN | medium |
| F13 | SK-V1 #11 `OwnedDocument` receiver | STILL-OPEN | low |
| F14 | SK-V1 #12 `Option<Box<PayloadArena>>` trigger; iteration showed zero pressure | STILL-OPEN; iteration partially-supersedes (trigger never fires) | low |
| F15 | SK-V1 #13 `parse-that` number kernel receiver | STILL-OPEN | low |
| F16 | SK-V1 #14 LOC budget echo | STILL-OPEN | low |
| F17 | SK-V1 #17 `build_tape_for_json` Lock 14 home | STILL-OPEN | medium |
| F18 | SK-V1 #18 §3.3 string-content prefilter location | STILL-OPEN | low |
| F19 | NEW: §317 wording "remains ... until perturbation authorized" hedges around NO-GO | NEW | medium |
| F20 | NEW: §7 host-fn-free row does not encode FAITHFUL-conditional from REDRESS 19 | NEW | medium |

## §23 — Punch list (ordered surgical edits)

Highest priority (block SK-READY):

| # | Target | Edit | Source | Lane/Lens |
|---|---|---|---|---|
| 1 | SUBSTRATE.md §0 (after line 7) | Add "Iteration Evidence" paragraph: "The substrate has been measured against the SOTA gate; `skinny/RESULTS.md` records outcome G / NO-GO at Track 1 12515/12988/8951 Mbps (twitter/citm/canada) against sonic-rs 21234/23238/13915 Mbps — 53-74% of competitor. The substrate cut is therefore **FAITHFUL-with-known-ceiling**: the test ran honestly; the eager 16-byte tape's materialization cost is the measured bottleneck. The remaining honest structural amendment is the lazy-offset tape (§1.2 amendment surface)." | NEW F2; Lens L | Lens L |
| 2 | SUBSTRATE.md §1.2 (or new §1.5) | Add lazy-offset tape amendment-surface paragraph per §21 surgery above. Add INDEX deviation ledger row 8 "lazy-offset tape: ADDITIVE-MECHANICAL substrate amendment; SK-V3-or-later measurement gates V1 receiver." | NEW F3; Lens N | Lens N |
| 3 | SUBSTRATE.md throughout | Add RESULTS.md / REDRESS.md citations at: §1.2:101 (REDRESS 15 — sealing), §3.6:317 (REDRESS 13/16/17/18 — token-economy gate), §7 host-fn-free row (REDRESS 19 — eager-decode MASKING), §2:217-219 (RESULTS.md row 34 — zero-arena confirmation). At minimum, footnote each refutable claim. | NEW F1; Lens H | Lens H |
| 4 | SUBSTRATE.md §2:217 (existing line) | Replace `Option<Box<PayloadArena>>` speculative paragraph with: "Iteration evidence (`skinny/RESULTS.md` row 34: Track 1 / Track 2 0/0 writes/0/0 allocations across twitter/citm/canada) confirms zero arena pressure. The `Option<Box<PayloadArena>>` conditional is dormant; SK-V3 reactivates only if a future grammar perturbs the empty-path assumption." | F14 + Lens H | Lens H |
| 5 | SUBSTRATE.md §2 (PayloadArena impl) | Add `pub fn payload_arena_writes(&self) -> u64` and `payload_arena_allocations(&self) -> u64` methods on `Tape<'input>`, delegating to private payload counter. Match BENCH §3.4 call sites. | SK-V1 #1 STILL-OPEN; F4 | Lane 3 + Lens M |
| 6 | SUBSTRATE.md §4.1:382 | Either replace `unsafe { std::str::from_utf8_unchecked(raw) }` with checked `from_utf8(raw).expect("UTF-8 prevalidated")`, OR cite and discharge the prevalidation pass per §1.3:121's mention of UTF-8 prevalidation outside the timed region. The current SAFETY comment claims an invariant the SIMD scan does not establish. | SK-V1 #6 STILL-OPEN; F5 | Lens F + Lens J |
| 7 | SUBSTRATE.md §1.3:117 + §4.3:420 | Settle the parse signature per C13 consolidated: `parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>` (or pick `JsonDocument<'i>` if §1.4 wins). Update both lines. | SK-V1 #3 STILL-OPEN; F6 | Lane 1 + Lane 3 |
| 8 | SUBSTRATE.md §1.4 vs §4.1 | Settle: `Json::parse → JsonDocument<'i>` (user calls `.root_value()` for `JsonRoot<'i>`) OR `Json::parse → JsonRoot<'i>` directly (drop `JsonDocument` wrapper). State the choice. | SK-V1 #4 STILL-OPEN; F7 | Lane 3 |
| 9 | SUBSTRATE.md §1.1:46-57 | Extend payload-class enum sketch to include `INLINE_STRING_BORROW`; declare bit-layout for `TokenFlags`; list reserved values explicitly. | SK-V1 #5 STILL-OPEN; F8 | Lane 3 |

Medium priority (block SK-READY but not load-bearing):

| # | Target | Edit | Source |
|---|---|---|---|
| 10 | SUBSTRATE.md §1.3:129 | Rename `pub enum JsonRoot {}` to `pub enum JsonRootKind {}` (or move under `kind::JsonRoot`). | SK-V1 #2 STILL-OPEN |
| 11 | SUBSTRATE.md §1.1:35-44 | Rename `STRING_BORROWS_SOURCE` → `PAYLOAD_BORROWS_SOURCE`; `STRING_NEEDS_UNESCAPE` → `PAYLOAD_NEEDS_NORMALIZE`. | SK-V1 #7 STILL-OPEN |
| 12 | SUBSTRATE.md §1.1:38-41 | Strip `RECOVERY_KIND (2)` from skinny TokenFlags; mark reserved. | SK-V1 #8 STILL-OPEN |
| 13 | SUBSTRATE.md §6:445-491 | Collapse `JsonVisitor` to `for_each_value` single-method; defer multi-method dispatch to V1 PASS-3 §3. | SK-V1 #9 STILL-OPEN |
| 14 | SUBSTRATE.md §7 host-fn-free row | Add column "FAITHFUL-conditional on V1 keeping JsonString::as_str() lazy per REDRESS item 19; if V1 emits eager `decode_json_string_to_arena`, the zero-arena claim is broken and the cut becomes MASKING." | NEW F20; Lens L |
| 15 | SUBSTRATE.md §317 | Re-word "the canonical skinny tape remains the 16-byte token stream with explicit pair tokens until such a perturbation is authorized" to "After three measured perturbation rejections (REDRESS items 16, 17, 18), the canonical 16-byte tape produced NO-GO; the lazy-offset amendment surface (§1.2) is the next honest perturbation." | NEW F19; Lens F |
| 16 | SUBSTRATE.md §8:531 | Move `build_tape_for_json` to either `crates/bbnf-bench/src/track2/json.rs` (BENCH-side) or `#[cfg(feature = "bench")]` in `runtime/src/grammars/json/`. State choice. | SK-V1 #17 STILL-OPEN; F17 |

Low priority (cosmetic / cookbook):

| # | Target | Edit | Source |
|---|---|---|---|
| 17 | SUBSTRATE.md §1.4:167 | Name V1 receiver for `OwnedDocument`: `runtime/src/owned/` per ARCH. | SK-V1 #11 |
| 18 | SUBSTRATE.md §4.1:396 | Replace "parse-that's number kernel here post-skinny" with named V1 receiver path. | SK-V1 #13 |
| 19 | SUBSTRATE.md §9 | Echo WORKSPACE §2 LOC budget per file. | SK-V1 #14 |
| 20 | SUBSTRATE.md §3.3 | Move JSON-specific Prefilter verifier description to COMPILER side. | SK-V1 #18 |

Total: 20 punch items. 9 high-priority, 7 medium, 4 low.

## §24 — Steelman summary (KEEP/FAITHFUL/MECHANICAL verdicts defending against challenges)

| Verdict | Site | Steelman | Defeat |
|---|---|---|---|
| KEEP — Lock 1 + private-Vec sealing | §1.2 (post-iteration redress note) | "Private-Vec sealing hides allocation residency" | The bench reports both logical and allocated tape bytes per §1.2:101; RESULTS.md row 34-39 confirms the reporting; the iteration validated the trade-off mechanically. |
| KEEP — §313-§317 token-economy gate | The three perturbation rejections | "The skinny is over-fitting to JSON's structural alphabet; the perturbations should have improved with different probes" | REDRESS items 13, 16, 17, 18 collectively span four independent perturbation axes (close-token elision, pair-token fusion, dispatch shape, token width); the consistent rejection across four orthogonal perturbations is empirical evidence that the eager-tape ceiling is structural, not parametric. |
| KEEP — `payload_or_skip` union | §1.1 (unchanged) | "PASS-3's split form would test cleaner" | The PASS-3 split form would force a 24-byte token = 50% cache-line increase; the union saves a cache-line per token-pair (open token holds skip, scalar token holds payload). RESULTS.md materialization rows confirm 64-byte cache-line discipline is held (token count × 16 bytes = logical tape bytes). |
| FAITHFUL-with-known-ceiling — eager 16-byte token (NEW) | §1.1 (after surgery #1) | "The cut is MASKING because the eager tape masks a V1 cost the bench cannot recover — a lazy substrate would parity-beat" | The bench DID measure the eager tape against sonic-rs (which itself uses an eager-ish tape with different bookkeeping) and returned NO-GO. The cut is FAITHFUL — the test ran. The ceiling is now empirically known. A future lazy-offset measurement updates the verdict; the current verdict is honest. |
| MECHANICAL with named inversion — Box→private-Vec | §1.2 + INDEX row 6 | "The Box→Vec migration breaks consumers" | The read API is `&[TapeToken]`; both Box<[T]> and private Vec expose `&[T]` through `Deref`. Consumers do not change. |
| MECHANICAL with named inversion — HM hierarchy | INDEX row 7 + COMPILER §9.1 | "Inverting HM as `passes::layout` subroutine requires Algorithm-W rewrite" | The HM module's source path already lives at `crates/passes/src/layout/types/`; only the call hierarchy inverts. ~20-30 LOC wrapper relocate. |
| MECHANICAL-CONDITIONAL — host-fn-free decode-string (NEW) | §7 host-fn-free row | "V1's `@host fn decode_json_string_to_arena` graduation breaks the zero-arena claim" | If V1 keeps decode lazy (as REDRESS item 19 mandates), the per-grammar `JsonString::as_str` method calls `host::decode_json_string_lazy` and the zero-arena claim holds. The MECHANICAL-CONDITIONAL classification names the constraint. |
| ADDITIVE-MECHANICAL — lazy-offset tape (NEW) | §1.2 amendment surface (post-surgery) | "Lazy-offset tape requires rewriting `Tape<'input>` and breaking `ValueRef`" | Lazy-offset ships as `LazyTape<'input>` alongside eager `Tape<'input>`; ValueRef goes through trait projection methods (`.start()`, `.end()`) that absorb the difference. ~400-800 LOC additive; zero rewrite of read-side consumers. |

These eight steelman defeats are the audit's load-bearing KEEP/FAITHFUL/MECHANICAL verdicts. They should not be revisited unless future bench iterations contradict.

## §25 — Final readiness verdict

> **Decision: SK-AMENDMENT-REQUIRED-NARROW.**
>
> The post-iteration SUBSTRATE.md correctly folds the iteration evidence into §313-§317 — close-token elision adopted, pair-token fusion rejected, dispatch-table rejected, 12-byte skipless rejected — and the rejections are documented as authorized substrate amendments rather than silent reversals. The two false-route corrections the user surfaced (dispatch table; 12-byte skipless) are spec-correctly classified. The iteration's structural rejections are well-encoded.
>
> The amendment scope is bounded by three NEW SK-V2 findings and six STILL-OPEN SK-V1 items: (a) the §0 premise framing must reclassify the eager-tape cut as FAITHFUL-with-known-ceiling against the measured NO-GO; (b) the lazy-offset tape must be promoted from §10 open-question to §1.2 amendment-surface with Lens N ADDITIVE-MECHANICAL classification; (c) every refutable cost claim must cite RESULTS.md row or REDRESS.md item per the user's SK-V2 Lens H tightening; (d) six SK-V1 punch items (parse signature, JsonRoot overload, payload-class roster, UTF-8 SAFETY discharge, arena counter API, build_tape_for_json home) remain unaddressed. Total: 20 punch items, 9 high-priority.
>
> The substrate spec's MECHANICAL pillar survives this audit. The Box→private-Vec sealing graduation, the HM hierarchy inversion, the `payload_or_skip` union, and (with surgery) the lazy-offset tape all close MECHANICALLY at V1. The four false routes the iteration eliminated remain eliminated; the spec text correctly documents them as rejected. The host-fn-free cut becomes MECHANICAL-CONDITIONAL given REDRESS item 19; the conditional must be named in §7 to make the spec self-checking.
>
> The substrate spec's FAITHFUL pillar survives this audit conditionally on the §0 reclassification. Every documented cut still survives Lens L steelman; the iteration's NO-GO verdict does not invalidate the cuts (each cut was testing an orthogonal axis or has named V1-grammar caveats); the residual fault is exclusively in the premise *framing*, not in the substrate's mechanical surfaces. After surgery #1 lands, the FAITHFUL classification is complete.
>
> Hereupon: dispatch the SK-V2 amendment narrow-scope agent against the §23 punch list. After the 9 high-priority items land (§0 reclassification, lazy-offset amendment surface, RESULTS/REDRESS citations, arena counter API, UTF-8 SAFETY discharge, parse signature settlement, JsonDocument/JsonRoot return, INLINE_STRING_BORROW enum, JsonRoot overload rename), the spec is structurally SK-READY. The 11 medium/low items may close in the same wave or in SK-V3.
>
> The substrate spec's prior-validation function survives this audit. The corpus is now buildable, has been measured, and the verdict is honestly NO-GO. The SK-V2 audit's job was to verify the spec encodes that honesty; the verdict is that with the §23 punch list closed, the encoding is complete. The next iteration cycle — measuring the lazy-offset tape against the gate — is the architecturally-required SK-V3 prerequisite for any further substrate amendment dispatch.

### Critical Files for Implementation

- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/SUBSTRATE.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/INDEX.md
- /Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md
- /Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md
- /Users/mkbabb/Programming/bbnf-lang/restart/ARCHITECTURE.md
