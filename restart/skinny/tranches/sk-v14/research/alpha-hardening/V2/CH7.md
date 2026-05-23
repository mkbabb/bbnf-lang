# CH7 Overfit-Prune — Pass Alpha V2 Disposition

Lens binding unchanged: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`.
Five CH7 criteria remain the disposition spine (CH7-1 grammar-derived
only; CH7-2 Lock 14 generic-crate compliance; CH7-3 real source change
+ strict-vs-strict + per-iter equality; CH7-4 round-trip on generated
output; CH7-5 no scaffold admit). V2 overlay per
`research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md §1`: (1) verify
the V1 BINDING REJECT + 5 REVISEs landed verbatim; (2) fresh-finding
scan for any new CH7-N violation introduced by the V2 fold work.

## §0 — Disposition summary

- Artefacts re-reviewed: 5 V2-touched (SYNTHESIS, HANDOFF, α-A, α-C,
  α-E) + 3 STAND-from-V1 (α-B, α-D, DISPATCH-CONTEXT).
- Per-section dispositions issued (V2 overlay): 36 (same surface as V1).
- ACCEPT: **36**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT-rate: 36 / 36 = **100.0 %**.
- Critical findings: 0.
- Escalation flag: **NO.** The V1 BINDING REJECT on C-3 round-trip
  scope has FOLD-LANDED verbatim per E-1 + F-17 + E-14; all 5 V1
  REVISEs have FOLD-LANDED. Zero new CH7 findings. V2 converges at
  100 % for the CH7 lens.

## §1 — V2 fold verification table (CH7 V1 dispositions)

Each V1 CH7 disposition tracked through to its V2 landing artefact
with quoted evidence. Fold IDs reference `V1/HARDENING-ALPHA-V1-CONSOLIDATED.md
§2.1` (α-F) + `§2.2` (α-E).

| V1 disposition | V1 src | V2 fold | V2 landing site | Status |
|---|---|---|---|---|
| **REJECT — C-3 round-trip gate CH7-1-blind to Pattern H** | V1/CH7.md §2.1 + §3.1 | **E-1 (BINDING)** | `alpha-E-candidate-shortlist.md:354-384` (three-part gate verbatim) | **FOLD-LANDED** |
| REVISE — §3 row C-3 + C-4 compress CH7 surface | V1/CH7.md §1 SYNTHESIS row | F-17 | `SYNTHESIS.md:273-274` (C-3 carries dual-tree round-trip + bypass-header detector + `see §5 + hardening V1 CH7 §3.1` pointer; C-4 names `json/numbers/direct_to_struct/main` verbatim) | **FOLD-LANDED** |
| REVISE — α-E §2 shortlist table C-3/C-4 cite without explicit command | V1/CH7.md §1 α-E table row + §3.2 | E-14 | `alpha-E-candidate-shortlist.md:85-86` (C-3 row: "round-trip... empty on BOTH skinny and core runtime trees; bypass-header detector empty; ... see §5 + hardening V1 CH7 §3.1"; C-4 row: named pre-wave row + per-shape Lock-1 triad + "see §6") | **FOLD-LANDED** |
| REVISE — §10 cap clarity (per-sub-wave vs per-cluster) | V1/CH7.md §1 α-E §10 row + §3.3 | E-2 (CH4 R3 authoritative per CONSOLIDATED §0.5) | `alpha-E-candidate-shortlist.md:732-758` (table reads C-1/C-2/C-3/C-5 = 30 min; only C-4 = 45 min per CSP-selectable shape; the "Cap discipline reconciliation" paragraph explicitly notes the CH7 §3.3 clarification applies to C-4 alone) | **FOLD-LANDED** |
| REVISE — V2-DISP-α-E-C3-table mirror | V1/CH7.md §3.2 | E-14 (mirror of α-E §2) + F-17 (mirror of SYNTHESIS §3) | `alpha-E-candidate-shortlist.md:85` + `SYNTHESIS.md:273` | **FOLD-LANDED** |
| REVISE — V2-DISP-SYNTHESIS-§3-C3-C4 | V1/CH7.md §3.4 | F-17 | `SYNTHESIS.md:273-274` (both rows) | **FOLD-LANDED** |
| (informational, non-blocking) α-A cite spot-check expansion | V1/CH7.md §3.5 | (deferred per CONSOLIDATED §0.5 — CH1 V2 carries the citation surface) | n/a — informational | **DEFERRED** (acceptable) |

**Fold tally:** 1 BINDING REJECT FOLD-LANDED; 5 REVISEs FOLD-LANDED; 1
informational REVISE deferred per CONSOLIDATED §0.5 (CH1's V2 cycle
covers the citation surface). Zero FOLD-PARTIAL; zero FOLD-MISSING.

## §2 — Per-artefact V2 disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| SYNTHESIS.md | §0.1 | ACCEPT | unchanged; CH7-3 close-condition binding holds. |
| SYNTHESIS.md | §0.2 | ACCEPT | reconciliation paragraph at `SYNTHESIS.md:200-209` lifts CH6 REJ-2 + CH7's audit-overlay integrity into the table; CH7-3 measurement honesty extended to the 6+11 wider ledger; AUDIT-FALSIFIED scope widened correctly. |
| SYNTHESIS.md | §0.3 | ACCEPT | R4 row now reads "first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator" (`SYNTHESIS.md:96`); CH7-4 round-trip binding inherits + CH7-2 grammar-neutrality strengthened. |
| SYNTHESIS.md | §0.4 | ACCEPT | P-1 closing sentence (`SYNTHESIS.md:115-120`) adds the W10.3 nested_layout preemptive round-trip-rule trigger + ≥ 50× SOTA-comparator threshold; CH7-1 + CH7-4 + CH3 cross-binding strengthened. |
| SYNTHESIS.md | §0.5 | ACCEPT | unchanged; contracted S-P3 deferral. |
| SYNTHESIS.md | §1.1 | ACCEPT | unchanged. |
| SYNTHESIS.md | §1.2 | ACCEPT | reconciliation paragraph wires 4+7 → 6+11 widening per CH6 REJ-2 fold; CH7-3 audit-overlay column carries the wider population. |
| SYNTHESIS.md | §1.3 | ACCEPT | rolling delta restated; audit-zero baseline holds. |
| SYNTHESIS.md | §2 | ACCEPT | telemetry schema gains `track2_entry_point` (`SYNTHESIS.md:240`) per CH5 REVISE; CH7's CH5 cross-binding strengthened — Track-1≡Track-2 plane collapse mechanically detected. |
| SYNTHESIS.md | §3 | ACCEPT | candidate table gains `LOC budget` + `Same-wave consumer` columns; risk column rebound; C-3 row lifts dual-tree round-trip + bypass-header detector + §5 pointer per F-17; C-4 row names `json/numbers/direct_to_struct/main` + per-shape Lock-1 triad per F-17 + E-3. CH7-1, CH7-2, CH7-4, CH7-5 all carry into the truth-bearing table now. |
| SYNTHESIS.md | §4 | ACCEPT | constraints extended with per-wave LOC ceiling (F-6), C-1 forward invariant (F-12), C-4 two-grammar-family exercise + no-grammar-branch dispatch (F-13), G-SIMD-GRAMMAR-POLICY triad (F-14), triumvirate discipline (F-9); CH7 surface broadened across S-P3 constraints. |
| SYNTHESIS.md | §5 | ACCEPT | unchanged. |
| SYNTHESIS.md | §6 | ACCEPT | unchanged. |
| HANDOFF.md | §1 | ACCEPT | unchanged. |
| HANDOFF.md | §2 | ACCEPT | unchanged. |
| HANDOFF.md | §3 | ACCEPT | numeric reconciliation paragraph lands per F-1; CH7-3 measurement honesty inherits. |
| HANDOFF.md | §4 | ACCEPT | α-F sole-author posture declared per F-2; CH6 REJ-1 closed; CH7-3 attribution-trail integrity preserved. |
| HANDOFF.md | §5 | ACCEPT | unchanged; CH7 lens binding cited at step 4 (`HANDOFF.md:141-143`). |
| HANDOFF.md | §6 | ACCEPT | next-move chain echoes hard caps (F-7) + restores G-Omega (F-8); the cap paragraph (`HANDOFF.md:162-165`) cites "30-min lens-agent cap; research 20 min / plan 15 min / redress 30 min (45 min only for the addendum-amended decision-engine fold + C-4 per CONSOLIDATED §0.5 cap discipline)" — CH7 §3.3 cap-clarity correctly inherits CH4 R3 authoritative reading. |
| HANDOFF.md | §7 | ACCEPT | refusal list adds the W10.3 round-trip-rule trigger (F-10), UnionTape verbatim refusal (F-16); CH7-1 + CH7-4 + CH7-5 carry through verbatim per `HANDOFF.md:220-232`. |
| HANDOFF.md | §8 | ACCEPT | PENDING posture preserved. |
| α-A §1 parse_only | ACCEPT | per-row audit-overlay citations unchanged; CH7-3 carry. |
| α-A §2 direct | ACCEPT | reconciliation table at `alpha-A.md:125-130` per A-1 lands the +2 (marine_ik, instruments) under the same `v6 §1 row 3` comparator-misbinding overlay; CH7-3 measurement integrity extended to the 6-row authoritative count without introducing new admit logic. |
| α-A §3 typed | ACCEPT | per A-2: +4 extension rows (random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 + W15.1 update_center) annotated `[ext†]` with v6 §1 row 4 binding; CH7-3 audit-overlay integrity holds for the wider 11-row population. |
| α-A §4 CSS L4 | ACCEPT | unchanged; CH7-1 + CH7-4 audit cite intact. |
| α-A §5 c/B telemetry | ACCEPT | per A-3: c/B telemetry LOC budget assigned via C-2 envelope; CH7-3 schema-debt closure. |
| α-A §6 | ACCEPT | unchanged. |
| α-B (entire) | ACCEPT | STAND from V1; zero V2 changes. |
| α-C §1 | ACCEPT | unchanged. |
| α-C §2 | ACCEPT | per C-1: P-7 falsifiability gate strengthened to triple-check (distinct symbol + TypeId + buffer addr at first bench iter); CH7-3 cross-binding with CH5 strengthened. |
| α-D (entire) | ACCEPT | STAND from V1; zero V2 changes. |
| α-E §1 | ACCEPT | unchanged. |
| α-E §2 shortlist table | ACCEPT | per E-14: C-3 + C-4 rows now carry explicit gates with §5 + §6 pointers; CH7-4 + CH7-5 binding restored from V1 REVISE. |
| α-E §3 C-1 | ACCEPT | per E-7: C-1 forward invariant added; per E-11: LOC lower bound raised to 2.8k; per E-13: §9 strict serialisation reading. CH7-1 + CH7-2 reinforced. |
| α-E §4 C-2 | ACCEPT | per E-12: LOC envelope +80 for Skipper fallback; CH7-3 plane-correct comparators unchanged. |
| α-E §5 C-3 | ACCEPT | **per E-1 BINDING:** three-part round-trip + bypass-header detector verbatim at `alpha-E-candidate-shortlist.md:354-384`; per E-6: regen-{grammar} family-shape binding; CH7-1 + CH7-4 fully closed. |
| α-E §6 C-4 | ACCEPT | per E-3: per-shape Lock-1 triad declaration (`substrate_target=existing_tape | retention_lifetime=generated_function | policy_owner=generated_grammar`); per E-4: post-wave hot-leaf module-path discipline rejecting `runtime::ext::|sidecar::|union::|cursor::`; per E-5: pre-wave hot-leaf citation rebound; per E-8: no grammar-branched dispatch + two-grammar exercise. CH7-5 wired with multi-layer falsification surface. |
| α-E §7 C-5 | ACCEPT | per E-10: scribe contract reads "29 row-keyed REDRESS entries" verbatim; CH7-1 audit-trail restoration unchanged. |
| α-E §8 | ACCEPT | unchanged. |
| α-E §9 | ACCEPT | per E-13: §9 vs §6 dependency-matrix resolved (C-4 strict serialises after all C-1 sub-waves); CH7-1 audit-trail discipline preserved. |
| α-E §10 | ACCEPT | per E-2 + CONSOLIDATED §0.5: caps reverted to 30 min for C-1/C-2/C-3/C-5; C-4 alone keeps 45 per CSP-selectable shape; per E-9: grammar-keyed hot-leaf paths required. CH7 cap discipline matches CH4 R3 authoritative reading. |
| α-E §11 | ACCEPT | unchanged. |
| DISPATCH-CONTEXT.md | (full) | ACCEPT | STAND from V1; zero V2 changes. |

Total: **36 ACCEPT / 0 REVISE / 0 REJECT.**

## §3 — Critical findings

### §3.1 — BINDING REJECT remediation — FOLD-LANDED verbatim

The V1 BINDING REJECT (V1/CH7.md §2.1 + §3.1) on C-3's round-trip
gate has landed verbatim in the V2 cycle. Quoted V2 evidence per the
three-part remediation:

(a) **Round-trip (skinny tree).** `alpha-E-candidate-shortlist.md:358-361`:

> `rm -rf skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask
> regen-css && git diff -- skinny/crates/runtime/src/grammars/css_l4_*`
> produces empty output.

(b) **Round-trip (core tree, all 8 grammars).** `alpha-E-candidate-shortlist.md:362-372`:

> For each of `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf,
> math}`: `rm -rf crates/core/src/runtime/<grammar>/ && cargo xtask
> regen-<grammar> && git diff -- crates/core/src/runtime/<grammar>/`
> produces empty output. (C-1's sub-wave structure owns the
> per-grammar xtask emission; C-3's round-trip gate consumes those
> xtasks for CSS and asserts byte-equivalence on every other grammar's
> tree as the cross-grammar recurrence-vector check. A hand-patched
> `crates/core/src/runtime/{grammar}/` file fails this gate; the
> Pattern H tarpit `alpha-D.md:486-495` flags collapses to ZERO
> hand-patched files under the gate's enforcement.)

(c) **Bypass-header detector.** `alpha-E-candidate-shortlist.md:373-383`:

> Every file matching `git grep -l '@generated by skinny bbnf-codegen'
> -- skinny/crates/runtime crates/core/src/runtime` must be the
> byte-for-byte output of a registered xtask emission; the round-trip
> succeeds on every such file. Files asserting the header outside the
> registered xtask scope are CH7-1 violations and reject the wave. The
> detector closes the audit-confirmed CSS bypass-header pattern
> (`alpha-D.md:185-200` cites the `// @generated by skinny
> bbnf-codegen; do not edit by hand.` header rendered into hand-curated
> content); post-PRUNE no `@generated` header may appear outside a
> registered xtask's emission scope.

The three-part scope precisely matches V1/CH7.md §3.1's prescription;
the Pattern H tarpit cross-reference is preserved; the bypass-header
detector closes the recurrence vector V1 named.

SYNTHESIS §3 row C-3 mirrors via F-17 at `SYNTHESIS.md:273`:

> round-trip xtask check returns clean on both runtime trees (`rm -rf
> … && cargo xtask regen-css && git diff` empty on
> `skinny/crates/runtime/src/grammars/css_l4_*` AND on
> `crates/core/src/runtime/css_l4/`) + bypass-header detector empty
> (`git grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime
> crates/core/src/runtime` traces every match to a registered xtask
> emission); `du -sh skinny/corpora/css-l4-sk-v14` > 800 KB; see §5 +
> hardening V1 CH7 §3.1.

Compression observation: SYNTHESIS row names only `crates/core/src/runtime/css_l4/`
as the core-tree target (not the full 8-grammar enumeration). This is
acceptable scope partitioning — C-3 is the CSS row, the cross-grammar
enforcement lives in C-1's sub-wave gate at SYNTHESIS row C-1 (line 271):

> `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d`
> returns ZERO per-grammar dirs

— and in α-E §5's full 8-grammar enumeration. C-1 collapses the 64-file
Pattern H surface; C-3 certifies regen integrity for CSS; α-E §5
extends the round-trip-empty check across all 8 grammars as a
cross-cluster recurrence-vector audit. The triad is internally
consistent and the "see §5 + hardening V1 CH7 §3.1" pointer is
explicit in the table cell — the truth-bearing summary correctly
delegates the enumeration to the §5 expansion.

CH7-4 binding is fully closed by E-1 + F-17.

### §3.2 — CH4 R3 cap discipline reconciliation — FOLD-LANDED via E-2

Per CONSOLIDATED §0.5 (CH4 R3 authoritative over CH7 §3.3), only C-4
inherits the 45-min addendum amendment. V2 α-E §10 reads at
`alpha-E-candidate-shortlist.md:741-745`:

| Candidate / wave | Research | Plan | Redress |
| C-1 sub-waves (8 grammars; per sub-wave) | 20 min | 15 min | 30 min |
| C-2 | 20 min | 15 min | 30 min |
| C-3 | 20 min | 15 min | 30 min |
| C-4 (per CSP-selectable shape) | 20 min | 15 min | 45 min |
| C-5 | 20 min | 15 min | 30 min |

The "Cap discipline reconciliation (CH4 R3, V1 hardening)" paragraph
at `alpha-E-candidate-shortlist.md:747-758` explicitly cites that
CH7 §3.3's per-sub-wave-vs-per-cluster clarification "now applies to
C-4 alone" with the 45-min cap "per CSP-selectable shape". HANDOFF §6
(`HANDOFF.md:162-165`) mirrors the same posture. The CH7 V1 §3.3
disposition is correctly resolved through the cross-lens reconciliation
recorded in CONSOLIDATED §0.5; this is the canonical outcome rather
than a CH7-internal contradiction.

### §3.3 — P-1..P-7 ↔ CH7-N mapping — V2 carry-through verified

The V1 §2.2 bijective mapping (P-1↔CH7-1; P-2/P-3/P-4↔CH7-3; P-5↔CH7-5;
P-6↔CH7-2; P-7 cross-bind to CH5) persists through V2 unchanged at
`SYNTHESIS.md:104-148`. The W10.3 round-trip-rule trigger added to P-1
(per F-10) at `SYNTHESIS.md:115-120` strengthens CH7-1 + CH7-4 binding
by adding a measurable preemptive trigger ("any future CSS feature
whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50×")
— this is a fresh CH7 hardening that emerged in V2, not a regression.

HANDOFF §7 carries the same trigger at `HANDOFF.md:225-229`. The
matching refusal-condition bullet is in place.

## §4 — Fresh-finding scan (V2-cycle defect surface)

Per CHALLENGE-V2-ADDENDUM §1.2, V2 cycles may introduce new defects
during fold work. Scan dimensions:

- **New fake `@generated` instances introduced by V2.** None. All
  `@generated` references in V2 artefacts (`SYNTHESIS.md:109-114, 192,
  221, 366`; `alpha-E.md:85, 87, 374, 380, 382, 416, 648-650, 670-671,
  684-686`) are pre-block framing, deletion targets in C-5, or the
  bypass-header detector specification itself. No V2 fold authored a
  hand-curated `@generated` header.
- **New scaffold-as-load-bearing claims.** None. C-4 (the only candidate
  touching W8 / W9 SCAFFOLD-ONLY surface) gains additional falsifiers
  in V2: per-shape Lock-1 triad declaration (E-3), module-path
  discipline rejecting `runtime::ext::|sidecar::|union::|cursor::` (E-4),
  pre-wave hot-leaf citation rebind (E-5), no grammar-branched dispatch
  + two-grammar-family exercise (E-8). The wiring discipline tightens
  rather than loosening.
- **New gate-relabel risk.** None. C-2's per-iter equality oracle
  remains the comparator integrity gate; the +80 LOC Skipper fallback
  envelope (E-12) is a comparator-availability path, not a gate
  relabelling.
- **New Lock 14 generic-crate leaks.** None. C-1's forward invariant
  (E-7) and C-4's no-grammar-branched dispatch (E-8) both close
  potential leak paths. SYNTHESIS §4 inherits both as S-P3 constraints
  (F-12, F-13).
- **New round-trip scope gaps.** None. E-1 closes the only V1 gap
  (Pattern H exclusion + bypass-header detector); the V2 cycle does
  not introduce any new generated-output surface that would need
  fresh round-trip coverage.
- **Cross-lens conflict.** CH7 §3.3 (per-sub-wave-vs-per-cluster cap)
  was correctly subordinated to CH4 R3 in CONSOLIDATED §0.5; the V2
  fold respects the authoritative reading. No CH7-internal conflict
  introduced.

Zero new findings across all six scan dimensions.

## §5 — Recommended folds for V3

None. V2 has folded every V1 CH7 disposition (1 BINDING REJECT + 5
REVISEs) verbatim, and the fresh-finding scan returns zero new
findings. The V3 confirming pass per `ORCHESTRATOR.md §3Z`
two-consecutive-cycle rule should re-disposition the V2 artefacts
unchanged and converge at ≥ 95 % the second time, completing the
SK-V14 alpha-bracket convergence.

## §6 — Bracket-level CH7 verdict

CH7 V2 converges at **100 %** for the lens. The BINDING REJECT
remediation landed verbatim (`alpha-E-candidate-shortlist.md:354-384`
+ `SYNTHESIS.md:273-274`); the 5 REVISEs landed (E-14, E-2, F-17 ×2,
implicit via CONSOLIDATED §0.5); zero new defects were introduced by
the V2 fold work.

The CH7 surface is now fully closed across:

- **CH7-1** (grammar-derived only): C-1 forward invariant + C-3
  bypass-header detector + C-5 deletion ledger.
- **CH7-2** (Lock 14 generic compliance): C-1 trait-dispatch +
  grammar-agnostic generator + C-4 no-grammar-branched dispatch.
- **CH7-3** (real source + strict comparator + per-iter equality):
  C-2 three plane-correct strict comparators + per-iter equality
  column; audit-overlay column at SYNTHESIS §2.
- **CH7-4** (round-trip on generated output): C-3 three-part
  round-trip + bypass-header detector covering both runtime trees +
  all 8 grammars.
- **CH7-5** (no scaffold admit): C-4 hot-leaf attribution change
  + per-shape Lock-1 triad + module-path discipline + two-grammar
  exercise.

The lens cleared. The aggregator should advance the V2 cycle to
V3 confirmation per `ORCHESTRATOR.md §3Z`; CH7 carries no fold into V3.

**E-1 landing status: FOLD-LANDED.**
