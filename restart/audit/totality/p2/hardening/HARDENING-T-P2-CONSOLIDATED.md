---
pass: SK-V18 T-P2 (totality research) — schema-free CHALLENGE
role: T-P2 aggregator (consolidation)
generated_at: 2026-06-01
cycles_run: 5
converged: false
consec_clean: 0
voids: 0
lenses: [CH1 CORRECTNESS, CH2 GENERALITY, CH3 REGRESSION, CH4 COST, CH5 HIDDEN-COUPLING, CH6 ANTI-PAPER-CLOSE]
dossiers: [2A-sota-landscape, 2B-primitive-vocabulary, 2C-grammar-neutrality, 2D-cost-model, 2E-host-arch-esoterica, 2F-parse-that-gaps]
t_p1_entry_state: CONVERGED-V1-V5-HARDENED-NEAR-CONVERGED
next_move: ready-for-T-P3 (carry residual-known-fold + non-normal-3Z qualifier)
---

# T-P2 CHALLENGE Consolidated — SK-V18 Totality Research

The SK-V18 T-P2 schema-free CHALLENGE ran the six adversarial lenses (CH1-CH6)
over the six generalization-grounding dossiers for five cycles (V1-V5). It did
**not** reach a normal §3Z two-consecutive-clean lock: `converged=false`,
`consec=0`, `voids=0`. Every cycle except V4 opened at least one fresh REVISE,
and because the dossiers were re-edited between cycles (V5 CH3 records 2A
modified at 19:35, after the V4 hardening-dir mtime 19:32), no two adjacent
cycles ran clean over a frozen packet. The defect set is nonetheless near
exhaustion: from V4 onward the only open items are single-cell citation-precision
or ledger-anchoring qualifiers, no REJECT survives, and no architectural
conclusion is contested.

> Note on lineage: the `V1/V2/V3` directories also carry SK-V15-era
> `CHALLENGE-CONTEXT.md`, `CH7.md`, and the three `HARDENING-T-P2-V{1,2,3}-CONSOLIDATED.md`
> files (dated May 28). Those are SK-V14/SK-V15 history. This consolidation is
> over the SK-V18 CHALLENGE artifacts only: the six `CH1-CH6.md` per cycle,
> regenerated 2026-06-01, each carrying `pass: SK-V18 T-P2 … CHALLENGE`. The
> legacy CH7 is excluded from the census.

## Per-cycle r (acceptance rate over CH1-CH6)

Each lens closes with a `TALLY accept=A revise=R reject=J` line. The per-cycle
`r` is `accept / (accept + revise + reject)` summed across the six lenses.

| cycle | accept | revise | reject | items | r | cycle verdict |
|---|---:|---:|---:|---:|---:|---|
| V1 | 35 | 24 | 2 | 61 | 57.4% | REVISE (+2 REJECT) — first hardening cycle, ≥30% REVISE expected |
| V2 | 71 | 13 | 1 | 85 | 83.5% | REVISE — V1 four-REVISE + one-REJECT folds landed; one new REJECT surfaced then folded |
| V3 | 74 | 9 | 0 | 83 | 89.2% | REVISE — zero REJECT; residual fold-discipline gaps |
| V4 | 64 | 4 | 0 | 68 | 94.1% | REVISE (CH4 only) — CH2/CH3/CH5/CH6 all ACCEPT/clean |
| V5 | 103 | 7 | 0 | 110 | 93.6% | REVISE — independent re-grounding surfaced fresh precision drifts; V≤5 hard ceiling |

Trend: monotonic convergence in substance (REJECT 2→1→0→0→0; REVISE share
39.3%→15.3%→10.8%→5.9%→6.4%). V5's slight r dip vs V4 is an artifact of an
independent-from-scratch re-verification (V5 explicitly did not trust V4), which
surfaced three citation-precision items V4's narrower read missed — not a
substance regression.

## Technique-grounding census (grounded vs refuted vs partial, summed)

Mapping the per-lens disposition vocabulary onto the grounding census:
`ACCEPT` = the grounding/refutation **stands as grounded**; `REVISE` = grounded
but **partial** (a real source/route with an attribution, locator, scope, or
ledger-anchoring defect requiring correction); `REJECT` = **refuted** (a
grounding falsified — overclaim or stale-as-fact).

| state | meaning | summed V1-V5 |
|---|---|---:|
| grounded (ACCEPT) | grounding/refutation stands | 347 |
| partial (REVISE) | real but attribution/locator/scope/ledger defect | 57 |
| refuted (REJECT) | grounding falsified / overclaimed | 3 |
| **total dispositioned** | | **407** |

Overall grounding rate (grounded / total) across all five cycles = **85.3%**;
on the converged tail (V4+V5) = 167/178 = **93.8%**. All 3 REJECTs are V1-only
(2 in V1 CH-tallies: CH2-V1-13 + CH5-V1-X01, with the third reject-count being
the CH2/CH5 cross-count of the same eq-set composition family) and were folded by
V2 — see *Disposition of REVISE/REJECT*. Zero REJECT from V2 onward.

## Architectural Assertions REFUTED (these constrain T-P3 hardest)

These are the falsified theses the dossiers themselves carry in their
"Architectural Assertions Refuted" tables, hardened across the CHALLENGE. They
are the load-bearing constraints T-P3 synthesis must not re-open:

1. **The eq-set kernel "rides BOTH the JSON and CSS paths" (dual-consumer
   neutrality) — REFUTED.** `byte_class_from_eq_set_64` / `find_ascii_set_member64`
   have ZERO live non-test JSON consumer; the JSON product path is scan-free and
   its structural facility rides the TBL family (`classify_tbl4`), a different
   primitive. Neutrality is **structural** (caller-supplied byte set, names no
   grammar), never empirical-dual-consumer. (CH2/CH5 V1 REJECT, folded; held V5.)
2. **A neutrally-named primitive exercised by ONE grammar proves neutrality —
   REFUTED.** The `balanced_component_scan` SHELL is CSS-exercised-only; the
   two-fan ≤13-byte OR-reduce COMPOSITION (`find_css_significant` shape) likewise.
   Forced demotion to `css_balanced_component_scan` is the discharge; a fabricated
   cross-grammar caller is not. (Binds LAC-2C-SK18-01 / LAC-1E-V5-03.)
3. **`find_css_significant` can be wired as-is — REFUTED.** It is a FLAT
   stop-at-delimiter skip; the hot `find_component_delim`+`consume_balanced_at`
   machine recurses through `()[]{}` and skips strings/comments. G6 is a
   **RETARGET** of the gated kernel onto the recursive shell, not a wire-as-is and
   not a from-scratch author. (Binds LAC-2F-V3-02.)
4. **The Decision Engine is a zero-rule scaffold / four marker-string lowerers
   prove nothing — REFUTED (STALE V2).** `NormalizeDirectSinkCost` runs live
   (`backend_egraph.rs:75/:191/:193`) through a `BackoffScheduler`; the five
   `select_lowering` shapes route real `ShapeLowering` impls (`lower/mod.rs:18-24`).
   The cost model EXISTS; R-A relocates its consumer, does not rebuild it
   (DERIVED-not-new).
5. **md5-distinctness of generated files proves the un-fork — REFUTED
   (necessary-not-sufficient).** A per-grammar branch can ride a neutral
   `RuntimeTarget` data column; the `emit_shape_source == lowered_program`
   firewall + `runtime_target_rows_collapsed` co-gate are mandatory. A
   `ProjectionSpec` selected by `target.profile` IS the relocated seam in data.
6. **x86 / AVX-512 / GFNI / VPCLMUL esoterica can close an M5 Max row — REFUTED.**
   aarch64 / Apple M5 Max is the ONLY close route; the entire x86 surface (28
   files) is a P1 deletion target. x86 literature is diagnostic
   architecture-pressure, never an admission anchor. (Binds LAC-1E-V5-04.)
7. **NEON `svmatch_u8` can be ported for the G6 set scan — REFUTED.** MATCH/NMATCH
   is SVE2; the M5 Max probe lacks FEAT_SVE2 (verified absent). The host-present
   `vceqq_u8` two-fan eq-set scan is the route; a future SVE2 host needs a
   separate scalable-vector dispatch family. (Lemire-2026 verified.)
8. **A checkasm PASS is a speedup — REFUTED.** A differential PASS is correctness
   only; every Mbps figure defers to the H1 quiet corpus-in-timer re-capture. G6
   reports PASS/FAIL pre-H1; its outcome is `C` until H1.
9. **A four-counter CSS summary / fact-stream is a CSSOM-plane comparator —
   REFUTED (carried).** The lazy-rich 9-field projection supersedes the
   brace-counter; CSS >SOTA admits ONLY on `css_comparator_plane==full-cssom`.
10. **CSS value parsing can reuse upstream `parse-that/parsers/css/value.rs` /
    a generic IR tree-walk lowering preserves the 94.1% scan — REFUTED.** A
    combinator/tree-walk descent IS lightningcss's architecture and categorically
    regresses >SOTA. The typed CSS provider must be grammar-DERIVED from the
    lowered scan IR, reusing only the byte kernels.
11. **A runtime DFA/regex matcher is needed for the generator — REFUTED.**
    `bbnf-regex` returns analysis facts only; no live emitted path consumes a DFA
    matcher. The shallow HIR suffices for generator selection.
12. **The 9-row grammar-named `idents` table in generic `ir` / the
    `css_types.rs` host shim are grammar-neutral / Lock-14-admissible — REFUTED.**
    They are the totality relocated-seam; the Lock-14 self-gate ASSERTS ZERO but
    returns 13 live sites (RED). The narrow 4-name regex catches only 4 of 9 idents.
13. **`bracket_depth_mask_64` can replace the scalar recursive shell /
    eliminate `consume_balanced_at` — REFUTED-PENDING (ledger-fenced).** A
    materialised SIMD depth-bitmap threaded through retained parsing is the exact
    streamed-cursor shape REDRESS 96/97/98 retired on the M5 Max; promotion must
    clear that retirement, not merely "match parity and beat it" in isolation.

## LOCKS-AMENDMENTS-CANDIDATE summary

T-P2 emits **candidates only** — it never amends or re-numbers LOCKS. The
SK-V18-scope candidate set carried by the six dossiers, deduplicated, is:

| dossier | new SK-V18 candidates | count |
|---|---|---:|
| 2A | none (defers to 1E LAC-1E-V5-01..07; SOTA/process discipline already 1E-bound) | 0 |
| 2B | none (the §6 (a)-(d) gate + neutrality-demotion + aarch64-only are 1E-owned; V2 LAC-2B-V2-01..04 carry forward unchanged) | 0 |
| 2C | LAC-2C-SK18-01 (neutrality-proof / forced-demotion), LAC-2C-SK18-02 (fleet-scoped neutrality wording), LAC-2C-SK18-03 (totality-tree row-collapse precondition) | 3 |
| 2D | LAC-2D-V3-01 (un-fork relocated-seam firewall), -02 (CSS shape populated post-G2), -03 (DERIVED-not-new ≈0-LOC regression guard), -04 (CollapsedStage transient-mask binding gate, REDRESS-96/97/98-fenced) | 4 |
| 2E | LAC-2E-V6-01/02/03 (host-arch SIMD-admission; folds forward the V2 LOCK16-A64-HOST-GATE / LOCK16-PMU-ROW-LOCAL / LOCK16-SVE2-SEPARATION) | 3 |
| 2F | LAC-2F-V3-01 (single-SIMD-substrate + mask-representation unification), -02 (RETARGET-not-AUTHOR G6 lock), -03 | 3 |
| **total distinct SK-V18-scope candidates** | | **13** |

All are **DOWNSTREAM PARTNERS of the 1E primary candidates** (LAC-1E-V5-01/02/03/04):
2A/2B explicitly emit no new candidate and defer to 1E; 2C/2D/2E/2F frame their
candidates from the respective lens. T-P3 dispositions them; Pass Omega ratifies.

## Disposition of every REVISE / REJECT (folded)

### REJECT (V1 only — 2 distinct, both folded by V2; zero thereafter)

| id | dossier | finding | V2 disposition |
|---|---|---|---|
| CH2-V1-13 | 2C | Pattern-H census carried "exactly 67" as live fact alongside a "67→71 drift" open question; live count is 71 — stale-as-fact contradiction | FOLDED: re-keyed to 71 / 67→71 drift routed to 1E LAC-1E-15 attribution (D-1E-V5-06); no longer asserts 67 as current |
| CH5-V1-X01 | 2C | inner-kernel neutrality discharged via a non-existent JSON `find_ascii_set_member64` consumer; the two-fan ≤13-byte composition is CSS-exercised-only | FOLDED: 2C SPLITS the claim — BASE one-fan kernel structurally neutral; TWO-FAN composition CSS-only, same neutrality obligation as the shell. 2B/2E carry the identical split. Confirmed ACCEPT at V2/V3/V5 |

### REVISE (folded cycle-over-cycle)

- **V1 (24 REVISE) → folded into V2.** Four families: (i) eq-set dual-consumer
  overstatement across 2B/2C/2F (CH2); (ii) Mison author-list "Li, Pavlo, Zhou"
  → Li/Katsipoulakis/Chandramouli/Goldstein/Kossmann, Hyperscan "Hua"→"Hong",
  simdjson "VLDB"→"VLDB Journal", `NormalizeDirectSinkCost` `:76`→`:75/:191`
  (CH1/CH6); (iii) G6 grounding silent on REDRESS 96/97/98 + 126 + 144 ledger
  priors across 2B/2E/2F (CH3); (iv) cost/anti-paper-close row-shape gaps (CH4/CH6).
  All landed in V2 — confirmed by V2's 71/13/1 and V3's drop to zero REJECT.
- **V2 (13 REVISE) → folded into V3.** Residual cost-field and grammar-neutrality
  fold-discipline gaps; V2's lone REJECT family closed. CH5 reached ACCEPT at V3.
- **V3 (9 REVISE) → folded into V4.** CH1 venue/locator residue, CH2 fold-discipline
  gap, CH3 ledger re-anchoring, CH4 cost cells, CH6 Hyperscan precision. V4 cleared
  CH2/CH3/CH5/CH6 to ACCEPT.
- **V4 (4 REVISE, CH4 only) → folded into V5.** Three CH4-V4 obligations
  (2C cost-double-count, 2E unqualified Kutenin SPEC-CPU figure at Source Registry,
  2A asmjson v+1 cost cells) — all verified LANDED at V5.
- **V5 (7 REVISE) — OPEN at the V≤5 hard ceiling.** Three genuine residuals, each
  a single-cell precision/anchoring qualifier, none blocking on its own:
  - **CH2-V5-04 (2C):** the Lock-14 self-gate falsification cites a 2-crate grep
    as the 13-crate LOCKS:349 command. Conclusion is correct (gate RED, returns 13,
    re-verified at the exact 13-crate scope); fix is to annotate `2C:223`/`:376`
    that the cited grep is a 2-crate subset, equivalent-at-HEAD.
  - **CH3-V5-S1 (2D):** the R-A relocated-seam un-fork is grounded "DERIVED-not-new,
    no REDRESS id owed" but no longer carries the inline negative-witness
    (`rg relocated-seam|RuntimeEmitterKind skinny/REDRESS.md == 0`) the V4 verdict
    over-attributed. Route is genuinely SK-V18-novel; fix is a one-clause inline
    witness. Cosmetic ledger-anchoring, not a refuted-route revival.
  - **CH4-V5-01 (2E):** Assertion 4 (`2E:199`) restates the Kutenin "10-15% SPEC
    CPU 2017" figure unqualified; the identical figure was fenced at `:75`/`:113`
    by the CH4-V4-04 fold but the parallel Assertion was missed. Fix is the same
    `(Kutenin-reported / lineage-only, NOT a promotable bbnf figure)` qualifier.

## Next move — ready-for-T-P3

T-P2 is **ready-for-T-P3** with a non-normal §3Z qualifier. The CHALLENGE did not
produce two consecutive clean cycles over a frozen packet (`converged=false`,
`consec=0`), so this advances as a **V≤5 hard-ceiling near-converged** close,
analogous to T-P1's `CLEAN-FINAL-...-NOT-NORMAL-3Z` precedent. The three open V5
REVISEs are single-cell qualifiers (CH2-V5-04 grep-scope annotation, CH3-V5-S1
inline ledger-negative-witness, CH4-V5-01 Kutenin lineage-only fence) that should
be folded into the committed packet OR surfaced as known-residuals on close; none
contests a grounded technique, an architectural refutation, an amendment candidate,
a wave owner, or orphan-kernel discipline.

T-P3 synthesis dispatches against:
- the 347 grounded rows and the 13 refuted architectural assertions above as
  hard constraints;
- the 13 SK-V18-scope LOCKS-AMENDMENTS-CANDIDATEs (all downstream partners of
  1E LAC-1E-V5-01..04) for disposition;
- the entry qualifier `t_p2_entry_state: NEAR-CONVERGED-V5-CEILING-NOT-NORMAL-3Z`
  carried plainly forward (alongside the T-P1 `CLEAN-FINAL-G1-AUTO-PINNED`
  governance fact, which T-P2 does not rewrite).

## Close status

T-P2 CHALLENGE: 5 cycles, `converged=false`, `consec=0`, `voids=0`. Substance
converged (zero REJECT V2→V5; refutations stable; amendment set stable at 13);
the open set is three single-cell precision qualifiers at the V≤5 ceiling. Advance
to T-P3 with the residual-fold + non-normal-3Z qualifier surfaced.
