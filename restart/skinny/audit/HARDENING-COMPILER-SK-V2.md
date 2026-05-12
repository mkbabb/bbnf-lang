# HARDENING-COMPILER-SK-V2 — Skinny Compiler Quadrant Audit (Post-Iteration)

## §1 Target Identification

| Field | Value |
|---|---|
| **Target** | `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/COMPILER.md` |
| **Cycle** | SK-V2 (post-iteration; predecessor SK-V1 returned `SK-AMENDMENT-REQUIRED-NARROW`) |
| **Lines audited** | 1–723 (full post-iteration file) |
| **Predecessor audit** | `restart/skinny/audit/HARDENING-COMPILER-SK-V1.md` (11-item punch list) |
| **Iteration evidence** | `skinny/RESULTS.md`, `skinny/REDRESS.md` (19 implemented items; 3 measured-and-rejected false routes settled) |
| **Sister-quadrant cross-refs** | `restart/skinny/BENCH.md` §7.8.1 (lines 968–1013), §7.8.2 (lines 1015–1055); `restart/skinny/SUBSTRATE.md` §1.2 (Box-sealing inversion → private-Vec), §2 (lazy string/number policy at lines 179–181) |
| **V1 anchors** | ARCH §6, §7.1, §7.2, §8.2, §10.1, §12.1, §12.2 |
| **Wall budget consumed** | ~32 minutes of the 40-minute hard cap |

This audit verifies SK-V1 disposition of C7, C8, C10, C16, C20 (the COMPILER-relevant cross-quadrant items) against the post-iteration corpus, and re-runs the full lens stack against the post-iteration §1.3, §2.2, §3.2, §5.3, §7 surfaces.

---

## §2 Cohort Verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | FAITHFUL | MASKING | MECHANICAL | ANTI-MECH | Recommendation |
|---|---|---:|---:|---:|---:|---:|---:|---:|---|
| Lane 1 — Lock-Adherence | KEEP | 6 | 0 | 0 | — | — | — | — | C10 closed: path drift settled at `passes/src/layout/types/`. Lock 14 waiver still implicit at §5.4/§5.5; remains a SK-V1 carry. |
| Lane 2 — Sequencing | N/A | — | — | — | — | — | — | — | Skinny single-wave. |
| Lane 3 — Cohesion | requires-amendment | 5 | 3 | 0 | — | — | — | — | C20 **not closed**: §2.2 row 155 still carries the pre-redress "2% median" + "one-host-fn JSON variant" wording, contradicting §1.3 (rewritten to two probes) and BENCH §7.8.1 (two probes). §3.2 row 219 likewise. New: §1.3 bullet 1 internal contradiction — claims "expected delta on twitter: 5-15%" while §1.3 itself now records prototype measurement at "exceeds the expected bands". |
| Lane 4 — SOTA Anchoring | KEEP | 4 | 0 | 0 | — | — | — | — | Quadrant continues to delegate SOTA to BENCH correctly. The post-iteration NO-GO outcome G is BENCH-owned reporting; COMPILER §10 closing language is unchanged and survives — ceiling-set-by-substrate framing holds under measurement. |
| Lane 5 — Grammar-Authoritative | KEEP | 3 | 1 | 0 | — | — | — | — | SK-V1 punch items 5 and 6 (Lock 14 waiver cite at §5.4/§5.5) carry-forward unclosed. |
| Lane 6 — Generated-LOC | KEEP | 3 | 0 | 0 | — | — | — | — | Per-file budgets at §4.5 + §6.2 still match. Pull-through from RESULTS (which doesn't fault emitted LOC) confirms the budget envelope holds. |
| Lane 7 — Friction Forecast | requires-amendment | 2 | 2 | 0 | — | — | — | — | SK-V1 punch items 10 (verbatim diagnostic) + 11 (ARCH §7.4 cite) carry-forward unclosed. New: §6.1 row 489 buries the dispatch-table rejection inside the `Alt` cell — friction surface for the implementor reading the cell. |
| Lane 8 — Carry & Deferral | requires-amendment | 4 | 1 | 0 | — | — | — | — | SK-V1 punch item 9 (VM-replay V1 receiver) carry-forward unclosed. New: §5.3 row "recognizer mining" still cites "dispatch-table envelope" as confirmatory bound while REDRESS item 17 removed the dispatch-table probe entirely. |
| Lane 9 — Greenfield | KEEP | 3 | 0 | 0 | — | — | — | — | Iteration ratifies the no-workaround discipline: dispatch-table rejected after measurement, not papered; 12-byte token rejected after measurement, not papered. |
| Lens F — LLM Bias | requires-amendment | 4 | 2 | 0 | — | — | — | — | "Potentially masking until..." tic — SK-V1 noted 6 sites; iteration removed two via measurement (egraph and cost-model now have measured non-dominance), but **§2.2 row 155, §3.2 row 219, §5.3 row 413 ("recognizer mining") still carry the hedge** without the discriminator surgery SK-V1 prescribed. |
| Lens G — Overfitting | KEEP | 3 | 1 | 0 | — | — | — | — | SK-V1 punch item 8 (Pierce-Turner V1-grammar caveat at §4.2 row 254) carry-forward unclosed. |
| Lens H — Hallucination | KEEP | 4 | 0 | 0 | — | — | — | — | §9.2 still self-surfaces the ARCH §12.1/§12.2 source-authority conflict; clean. |
| Lens I — Contrivance | KEEP | 4 | 1 | 0 | — | — | — | — | LayoutFacts empty-policies field unchanged; SK-V1 verdict survives — V1-shape-preservation defeats the steelman. |
| Lens J — Host-Language | KEEP | 2 | 0 | 0 | — | — | — | — | §6.1 row 489 explicitly cites "LLM owns branch-table/jump-table lowering" — direct host-leverage; iteration ratifies via measurement (real function-pointer table regressed). |
| Lens K — Meta-Grammar | KEEP | 3 | 1 | 0 | — | — | — | — | SK-V1 punch item 7 (meta-grammar surface clarification at §1.3) carry-forward unclosed. |
| **Lens L — Premise Fidelity** | **AMENDMENT-REQUIRED-NARROW** | — | — | — | **6** | **2** | — | — | C8 disposition: COMPILER §1.3 now records empirical MASKING signal but **does not propagate to §2.2 row 155 / §3.2 row 219**. C7 disposition: **SUPERSEDED by REDRESS item 17** (dispatch-table invalidated empirically; alternate_pext_mask_plan remains the cross-platform divergence probe but is now narrower). New: §1.3's "expected delta on twitter: 5-15%" is now contradicted by REDRESS evidence at twitter (57.6% T1 ratio = 42% delta). |
| **Lens M — Falsifiability** | KEEP | 1 | 0 | 0 | — | — | — | — | Regen-equality + BIR-snapshot gates unchanged. Compiler-side falsifiability remains binary. |
| **Lens N — Graduation Mechanicality** | KEEP | — | — | — | — | — | **4** | **0** | All three SK-V1 mechanical closures survive. **New MECHANICAL row**: REDRESS item 19 promotes the host-fn-free deviation from "optional MASKING-pending" to "constraint on V1 closure" (V1 JSON must keep string decode lazy). This is a sharpening, not a re-architecting; the closure path is still additive. |

**Cohort verdict**: **SK-AMENDMENT-REQUIRED-NARROW**.

Counts: KEEP=48, REINVENT=11, DISCARD=0, FAITHFUL=6, MASKING=2, MECHANICAL=4, ANTI-MECHANICAL=0. KEEP-fraction (excluding L/N): ~71% — within healthy band.

---

## §3 SK-V1 Punch-List Disposition (Direct Carry Audit)

The SK-V1 audit produced 11 punch-list items. SK-V2 disposition per item:

| SK-V1 # | Site | SK-V1 surgery | SK-V2 disposition | Justification |
|---|---|---|---|---|
| 1 | `COMPILER.md:147` (§2.2 `Call (kind: Host)`) | Replace "2% median" with two-probe structure | **STILL-OPEN** | Row 155 still reads verbatim the pre-redress 2% median text. Iteration did not propagate. |
| 2 | `COMPILER.md:211` (§3.2 `CallHost`) | Replace "one-host-fn measurement variant" with two-probe | **STILL-OPEN** | Row 219 still uses singular "variant" pre-redress phrasing. |
| 3 | `COMPILER.md:246` (§4.1 row "TypeFacts output") | Change `passes::types` → `passes::layout::types` | **STILL-OPEN** | Row 254 still reads truncated `passes::types`. §4.3 + §4.5 use the longer path; row inconsistent. |
| 4 | `COMPILER.md:371–385` (§5.1 phase count) | Reconcile "8" vs 9-arrow diagram | **STILL-OPEN** | Heading says 8 phases; diagram lists 9 arrows. |
| 5 | `COMPILER.md:413–415` (§5.4 recognizer Lock 14 waiver) | Append waiver cite | **STILL-OPEN** | Unchanged. |
| 6 | `COMPILER.md:443` (§5.5 shapes Lock 14 waiver) | Append waiver cite | **STILL-OPEN** | Unchanged. |
| 7 | `COMPILER.md:117` (§1.3 closing — meta-grammar surface) | Append clarification | **STILL-OPEN** | Unchanged. |
| 8 | `COMPILER.md:254` (§4.2 Pierce-Turner row) | Append V1-grammar caveat | **STILL-OPEN** | Row still reads "The skinny's HM is pure synth." with no CSS L4 / Sheets caveat. |
| 9 | `COMPILER.md:227` (§3.3 invariant 5) | Append "V1 receiver: Tranche I" | **STILL-OPEN** | Row unchanged. |
| 10 | `COMPILER.md:392` (§5.2 BBNF-parse row) | Append verbatim diagnostic | **STILL-OPEN** | Row unchanged. |
| 11 | `COMPILER.md:564` (§6.4 regen-equality) | Append ARCH §7.4 cite | **STILL-OPEN** | Unchanged. |

**Tally**: 0 of 11 SK-V1 punch-list items closed. The iteration directed effort at the runnable prototype, not at audit-surfaced text edits. SK-V1 surgical edits remain outstanding.

This is the dominant signal of SK-V2. The iteration did substantive evidence-gathering work — it disposed C7 by measurement, hardened §1.3 with prototype data, and ratified the dispatch-table rejection in §6.1 row 489 — but the SK-V1 surgical edits did not land.

---

## §4 Cross-Quadrant Item Disposition (C7, C8, C10, C16, C20)

### C7 — Cross-platform plan divergence (Lens L)

**SK-V1 surgery**: BENCH §10.3 probability mapping row addition for `alternate_pext_mask_plan < canonical × 0.90 on x86_64`.

**SK-V2 disposition**: **SUPERSEDED**. The disposition lens has shifted by measurement:
- REDRESS item 17 invalidated `alternate_dispatch_table_plan` (the duplicate probe was a false signal; the real function-pointer table regressed). BENCH §7.8.2 now marks the row INVALID until distinct implementation lands.
- The remaining `alternate_scalar_plan` returned 47.9–61.8% of canonical per RESULTS — confirmatory pass.
- `alternate_pext_mask_plan` is still missing in RESULTS rows.

The single-plan-extraction cut at COMPILER §5.3 row "egraph rewrite" (line 414) and §7 row `cost-model` (line 588) is FAITHFUL at the canonical-vs-scalar boundary on M1 Pro; the x86_64 PEXT divergence remains an unobserved-but-named tranche-H carry. COMPILER itself does not need an edit for C7; the divergence row lives in BENCH §10.3.

**Verdict**: SUPERSEDED for COMPILER quadrant scope.

### C8 — Eager-decode band rationale ambiguous (Lens L)

**SK-V1 surgery**: Clarify §7.8.1 Probe B per-corpus bands as "the additional cost of registry-routed eager decode atop the eager-decode work itself".

**SK-V2 disposition**: **STILL-OPEN but transformed**. The iteration did three things here:
1. REDRESS item 19 hardened §1.3 to record the empirical finding that eager decode exceeded the expected band on all three corpora (twitter ~42% delta, citm ~23%, canada ~18%).
2. BENCH §7.8.1 Probe B added a "Prototype redress note" clarifying that the eager-decode row is valid as an *eager-decode-work* bound.
3. The dispositional consequence — **lazy string decode is now a CONSTRAINT on V1 closure, not an optional optimisation** — landed in COMPILER §1.3 bullet 1.

The ambiguity SK-V1 surfaced has been partially resolved by separating the questions empirically; but **COMPILER §1.3 itself now carries an internal contradiction**: predictive 5-15% language sits adjacent to empirical "exceeds the expected bands" language without resolution. The constructive fix: rewrite §1.3 bullet 1 to lead with the empirical finding.

**Verdict**: STILL-OPEN; the C8 disposition surface has migrated to COMPILER §1.3 itself.

### C10 — `passes::layout/types` path drift (Lane 1)

**SK-V1 surgery**: Settle on `passes/src/layout/types/`.

**SK-V2 disposition**: **CLOSED** (partial). §4.3 and §4.5 use the long path. **STILL-OPEN** at §4.1 row "TypeFacts output" line 254, which retains "Internal to `passes::types`" — a one-word edit deferred.

### C16 — JSON hand-curated recognizer Lock 14 fence (Lane 1)

**SK-V1 surgery**: Add explicit Lock 14 waiver cite to §5.4/§5.5.

**SK-V2 disposition**: **STILL-OPEN**. Unchanged.

### C20 — COMPILER §2.2 + §3.2 redress propagation gap (cross-quadrant)

**SK-V1 surgery**: Match §2.2 row 147 and §3.2 row 211 to the §1.3 + BENCH §7.8.1 two-probe structure.

**SK-V2 disposition**: **STILL-OPEN**. Row 155 reads: "Potentially masking until BENCH's one-host-fn JSON variant proves the `CallHost` registry path stays within 2% median of the direct SUBSTRATE path on all three corpora." This contradicts:
- §1.3 lines 100–115 (two probes, no 2% median threshold)
- BENCH §7.8.1 (two probes, per-corpus bands, no flat 2%)
- RESULTS bench rows (which empirically refute the 2% predicate)

**This is the dominant SK-V2 fault**. The stale text actively contradicts measured outcomes: §2.2 row predicts ≤ 2% median while measurements show 18–42% deltas. **Elevated to load-bearing**.

---

## §5 New SK-V2 Items (Surfaced by the Iteration)

### N1 — §1.3 internal empirical contradiction (Lens L)

§1.3 bullet 1 reads in two voices: predictive ("expected delta on twitter: 5-15%") and empirical ("gross eager string decode exceeds the expected bands"). Both are present; neither is marked as authoritative.

**Surgery**: rewrite §1.3 bullet 1 to make the post-measurement finding load-bearing and demote the prior expectation to history.

**Verdict**: REINVENT — Lens L + Lens A.

### N2 — §5.3 row "recognizer mining" cites obsolete dispatch-table envelope (Lane 8 + Lens L)

§5.3 row "recognizer mining" reads: "...stays within the expected dispatch-table envelope." REDRESS item 17 invalidated the dispatch-table probe; the envelope check no longer exists.

**Surgery**: drop the dispatch-table envelope reference; keep the scalar-fallback comparison and the cross-platform PEXT carry.

**Verdict**: REINVENT — Lane 8 + Lens L.

### N3 — §6.1 Alt cell buries dispatch-table rejection (Lane 7)

§6.1 row 489 buries the rejection rationale inside a per-BIR-variant lowering cell. An implementor scanning §6.1 will see the rejection without context.

**Verdict**: REINVENT — Lane 7 (minor).

### N4 — §3.3 invariant 4 partially refuted by §3.1 row 200 (Lane 3)

§3.3 invariant 4 ("SIMD is mined, not syntax-directed") aliases with §3.1 row 200's codegen-time byte table. Naming clarification welcome but not load-bearing.

**Verdict**: KEEP.

### N5 — §5.3 row "egraph rewrite" hedge survives despite measurement (Lens F)

§5.3 row reads "Potentially masking until bounded" — but measurement has now occurred. RESULTS shows alternate_scalar_plan at 48–62% of canonical; no inverted dominance. The cut is empirically FAITHFUL on M1 Pro.

**Surgery**: harden the hedge to "Empirically FAITHFUL on M1 Pro per `skinny/RESULTS.md`".

**Verdict**: REINVENT — Lens F + Lens L promotion.

### N6 — §7 row `cost-model` hedge survives despite measurement (Lens F)

§7 row carries "Potentially masking" language while RESULTS reports NO-GO outcome G (canonical misses SOTA, but no alternate stub succeeds within the implementation envelope).

**Verdict**: REINVENT — Lens F + Lens L.

---

## §6 Lens L — Premise Fidelity (load-bearing, focal lens this cycle)

For each documented omission, classification against post-iteration evidence:

| Site | Omission | Classification | Iteration evidence | Verdict |
|---|---|---|---|---|
| `COMPILER.md:155` (§2.2 `Call (kind: Host)`) | Host-fn-free | **MASKING (text-stale) + MASKING (empirical)** | RESULTS Probe B 57.6/77.2/81.9% T1 → eager decode MASKING; SK-V1 stale-text propagation gap intact (C20). | **MASKING-stale-text + MASKING-empirical-disposed-elsewhere** |
| `COMPILER.md:219` (§3.2 `CallHost`) | Host-fn-free | **MASKING (text-stale)** | Same as §2.2 row 155 | MASKING-stale-text |
| `COMPILER.md:413` (§5.3 recognizer mining) | Hand-curated structural-alphabet | **EMPIRICALLY FAITHFUL on M1 Pro** | alternate_scalar_plan 48–62% (canonical wins by 38–52%) | FAITHFUL with row-text update (N2) |
| `COMPILER.md:414` (§5.3 egraph rewrite) | Pick canonical | **EMPIRICALLY FAITHFUL on M1 Pro within alternate envelope** | alternate-plan probes return non-inverted | FAITHFUL with row-text update (N5) |
| `COMPILER.md:585–588` (§7 cost-model row) | Hedge | **EMPIRICALLY FAITHFUL on M1 Pro** | Same as egraph row | FAITHFUL with row-text update (N6) |
| `COMPILER.md:262` (§4.2 Pierce-Turner) | No annotations | JSON-FAITHFUL — needs CSS/Sheets caveat | SK-V1 punch item 8 unclosed | FAITHFUL with V1-grammar caveat |

**Summary**:
- **FAITHFUL** (without iteration-driven update needed): 16 rows
- **FAITHFUL with row-text update needed** (iteration evidence converts MASKING-pending → empirical FAITHFUL): 3 rows
- **MASKING-stale-text**: 2 rows (§2.2 row 155 + §3.2 row 219)
- **MASKING-empirical**: §1.3 bullet 1 (disposed but with internal contradiction)

**Steelman of the host-fn-free cut**: the skinny's claim was never that eager decode is cheap. It was that the skinny defers decode entirely to the lazy substrate/view path. The Probe B finding refutes a *parse-time-eager-decode* variant; it ratifies the *lazy-decode* skinny exactly because eager would be that expensive. The skinny premise survives — but **only if V1 JSON keeps decode lazy** (SUBSTRATE §2 + Lock 9 commit this).

**Lens L verdict**: **AMENDMENT-REQUIRED-NARROW**. The premise survives steelman; the row-level propagation gap is the load-bearing fault.

---

## §7 Lens N — Graduation Mechanicality (load-bearing)

The four deviations:

| Site | Deviation | V1 closure path | Verdict |
|---|---|---|---|
| §4.4 + §9.1 layout inversion | HM at top-level skinny; V1 inverts | Wrapper at `passes/src/layout/mod.rs`; 150–300 LOC | MECHANICAL with named inversion |
| §1.3 + §9.2 host-fn-free | No `@host fn` for JSON skinny | Tranche D adds `@host fn`. **Iteration sharpening**: V1 JSON must keep substrate decode lazy or accept SOTA loss | **MECHANICAL with constraint** |
| §5.4 + §5.5 hand-curated fixtures | JSON-named files in `passes/` | V1 graduation deletes fixtures when miners can nominate | MECHANICAL with named delete |
| Iteration-surfaced: lazy string decode as V1 constraint | V1 JSON must implement `JsonString::as_str()` via `Cow<'input, str>` lazy unescape | Lock 9 already commits the `Cow` model; iteration ratifies the lock is binding for V1 JSON throughput | MECHANICAL (substrate already commits) |

**Lens N verdict**: **honoured (all MECHANICAL)**. The graduation cost remains comfortably ≤ 500 LOC additive + 120 LOC delete + 0 LOC for the lazy-decode constraint.

---

## §8 Punch List (SK-V2)

15 surgical edits. Items 1–11 inherit from SK-V1 (still open); items 12–15 are SK-V2 new.

| # | File:line | Verbatim edit summary | Source verdict | Lane(s) |
|---|---|---|---|---|
| 1 | `COMPILER.md:155` (§2.2 `Call (kind: Host)` row) | Replace with empirical MASKING disposition citing RESULTS + V1 lazy-decode constraint | REINVENT | Lane 3, Lens F, Lens L, C20 |
| 2 | `COMPILER.md:219` (§3.2 `CallHost` row) | Replace with two-probe disposition language | REINVENT | Lane 3, Lens F, Lens L, C20 |
| 3 | `COMPILER.md:254` (§4.1 row "TypeFacts output") | Change "Internal to `passes::types`" to "Internal to `passes::layout::types`" | REINVENT | Lane 3, C10 residue |
| 4 | `COMPILER.md:379` (§5.1 pipeline heading) | Reconcile heading "8 phases" with 9-arrow diagram | REINVENT | Lane 3 |
| 5 | `COMPILER.md:425` (§5.4) | Append Lock 14 waiver cite + deletion gate | REINVENT | Lane 5, Lens N, C16 |
| 6 | `COMPILER.md:449` (§5.5) | Append Lock 14 waiver cite | REINVENT | Lane 5, Lens N, C16 |
| 7 | `COMPILER.md:127` (§1.3 closing) | Append meta-grammar surface clarification | REINVENT | Lens K, Lens N |
| 8 | `COMPILER.md:262` (§4.2 Pierce-Turner) | Append V1-grammar caveat | REINVENT | Lens G, Lens L |
| 9 | `COMPILER.md:235` (§3.3 invariant 5) | Append "V1 receiver: Tranche I" | REINVENT | Lane 8 |
| 10 | `COMPILER.md:400` (§5.2 BBNF-parse row) | Append verbatim diagnostic | REINVENT | Lane 7 |
| 11 | `COMPILER.md:572` (§6.4 regen-equality) | Append ARCH §7.4 cite | REINVENT | Lane 7 |
| **12** | `COMPILER.md:100–115` (§1.3 bullet 1) | Rewrite to lead with empirical finding | REINVENT | Lens L, Lens A, C8 |
| 13 | `COMPILER.md:413` (§5.3 "recognizer mining") | Drop dispatch-table envelope reference | REINVENT | Lane 8, Lens L, N2 |
| 14 | `COMPILER.md:414` (§5.3 "egraph rewrite") | Harden hedge to "Empirically FAITHFUL on M1 Pro" | REINVENT | Lens F, Lens L, N5 |
| 15 | `COMPILER.md:588–589` (§7 `cost-model`) | Harden hedge to "Empirically FAITHFUL on M1 Pro" | REINVENT | Lens F, Lens L, N6 |

Total: 15 surgical edits. None DISCARD. All REINVENT — text exists but carries stale, contradictory, or under-propagated content.

---

## §9 Final Readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> The COMPILER quadrant survives the SK-V2 audit at the architectural level. The iteration evidence in `skinny/REDRESS.md` and `skinny/RESULTS.md` materially sharpens the Lens L disposition: the two false routes (alternate_dispatch_table_plan and 12-byte token) are settled empirically; the host-call probe split has produced a clean disposition (dispatch passes; eager-decode MASKING); and the egraph + cost-model + recognizer-mining cuts are now empirically FAITHFUL on M1 Pro within the alternate-plan implementation envelope. The single-plan extraction cut (SK-V1's C7) is SUPERSEDED for COMPILER scope.
>
> The dominant SK-V2 fault is **propagation**: the 11-item SK-V1 punch list returned zero closures in the iteration cycle (the iteration directed effort at the runnable prototype, not at audit-surfaced text edits). The empirical evidence that landed in §1.3 + §6.1 reduces the per-row hedging that the cohort needs, but the rows themselves still carry the pre-redress text. Specifically: §2.2 row 155 + §3.2 row 219 still reference "2% median" / "one-host-fn JSON variant" — text now empirically refuted at 18–42% deltas.
>
> Lens N MECHANICAL classifications all survive, with one sharpening: the host-fn-free deviation now closes mechanically **only under the V1 constraint that string decoding stays lazy** (SUBSTRATE §2 + Lock 9 commit this).
>
> The 15-item SK-V2 punch list is mechanical text-propagation. The 11 SK-V1 carries are unchanged from prior surgery. The 4 SK-V2 new items convert iteration evidence into row-level dispositions.
>
> Hereupon: dispatch the amendment agent against the 15-item punch list before any further SK-V2 cycle. COMPILER's load-bearing claim — "every cut in this spec is a cut to compiler-side machinery whose absence cannot lower the ceiling" — survives the iteration steelman with sharper teeth. The NO-GO outcome G is a SUBSTRATE-quadrant verdict, not a COMPILER verdict.

---

**Audit time**: ~32 minutes of the 40-minute hard cap. Lens totals: KEEP=48, REINVENT=15, DISCARD=0, FAITHFUL=6 (3 with V1-grammar caveats), MASKING-stale-text=2, MASKING-empirical-disposed-elsewhere=1, MECHANICAL=4, ANTI-MECHANICAL=0. KEEP-fraction (excl. L/N): ~71% — within healthy band.
