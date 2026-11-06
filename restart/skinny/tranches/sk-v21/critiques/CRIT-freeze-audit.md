# CRIT-freeze-audit — SK-V21 pass-3 freeze audit

Date 2026-07-05. Method: apply the freeze criterion — *the tranche plan may be declared converged
ONLY IF every remaining blocker is a wave-owned execution item AND no open research or design
question remains* — by walking (a) the CONVERGENCE-LEDGER pass-2 blockers, (b) the charter
deliverables, (c) the SK-V20 chronic items C1-C6, (d) the coordination seams, (e) TW0's internal
breadth. Evidence: `SPEC-DRAFT-P1.md` (680 lines, pass-2-amended, mtime 21:47), the two v2
critiques (arch 82 / honesty 86, mtimes 21:38/21:39), `CRIT-blocker-audit.md` (80, 21:37),
`CONVERGENCE-LEDGER.md` pass-2 (21:49), SK-V20 `DEFERRED-LEDGER.md`.

**Governing temporal fact:** the spec was last amended at 21:47 — AFTER all three pass-2 critiques
(21:37-21:39). The critiques scored 82/86/80 and each held points back for ONE reason only: the
545-line text they read had not yet folded the pass-2 corrections (N1/Defects A/B/C). Both v2
critiques say verbatim the corrections "must be reconciled before freeze" and "one more short loop
clears it." **That loop is the 21:47 amendment.** This audit verifies the loop landed.

---

## (a) CONVERGENCE-LEDGER pass-2 blockers — the three "What blocks 100" items

1. **The pass-3 verification re-score** — ledgered as "the only remaining non-execution blocker"
   (LEDGER:143-147). It is not a research or design question; it is the verification *this audit
   performs*: does each 21:47 fold-in match its mandate? Walked below — all match. **DISCHARGED by
   this audit.**
2. **The freeze decision** (LEDGER:148-151) — the decision this audit renders. Its own predicate
   ("if the re-score confirms the fold-ins, every remaining blocker is a wave-owned deliverable")
   is now satisfied.
3. **TW0 internal sequencing** (R3 residue, LEDGER:152-154) — explicitly "non-blocking"; an
   execution-plan ordering task owned by TW0. Wave-owned.

**Fold-in verification (every pass-2 correction the v2 critiques + blocker-audit demanded):**

| Correction (source) | Landed at | Verdict |
|---|---|---|
| N1 / Defect B — "one-substrate fold-over-cursor" REFUTED; eager = tapeless fused sink, distinct lane | §0:46, §2:118-122, §4:244-255, TW3 headline 268-275 | FOLDED — "stronger claim" retracted, sink is the benched default, cursor its own lane |
| Defect A — asm "intrinsic cannot survive" obviated | §0:38-42, §5.4:328-341 | FOLDED — P5 exception REFUTED by door(a) |
| Defect C / N2 — "Sampled buys nothing" too strong | §2:130-133 | FOLDED — "deleted for KISS, not for a flat null" |
| FLIP-3 — movemask already vaddv on live path | §5.1:306-312, TW0:546 | FOLDED — TW0 sweep item WITHDRAWN, cost-model cell |
| Step-0 root cause (view.rs:141, not literals.rs) | §6:376-384 | FOLDED — B3 loci corrected |
| NEW-R6 — 1080/960 counts are regen-owned | §4:231-234, TW1b:571, TW1c:578 | FOLDED — "95.1%/88.5% regen-owned, true surface ~40-60 sites" |
| NEW-R7 — TW1b gate can't meet §1.1 ≥3% | TW1b:573-575 | FOLDED — "liveness-AND-no-regression, not the ≥3%-win clause" |
| N3 — WART C is a two-site fold | §9:471-475 | FOLDED — runtime_generator.rs:98 AND json_sink_direct/mod.rs:98 |
| N4 — criterion re-path is TW-S.2 | §3:188-189 | FOLDED |
| Divan warm/cold scope; reconstruction-tax two constructs | §3:190-192; §2:110-111 | FOLDED |

**Nothing the critiques flagged before-freeze is unfolded.** The re-score the ledger owed is
dischargeable clean; a v3 critique round would clear the held-back points (both v2s said "into the
90s were the text current").

## (b) Charter deliverables (PASS-ALPHA.md:76-81)

- `SPEC.md` — present as `SPEC-DRAFT-P1.md`; **not yet renamed/finalized.**
- `GESTALT-SYNTHESIS.md` — **absent** (only SK-V20 has one).
- `DEFERRED-LEDGER.md` (v21 fold-in) — **absent** at top level; the fold-in census lives at
  `research/35-deferred-fold.md`, referenced throughout the spec (§10).
- `CONVERGENCE-LEDGER.md` — present, pass-1 + pass-2; needs the pass-3 entry.
- The forward prompt — **absent.**

These are the charter's "deliverables **at** 100% convergence" — the OUTPUT of the freeze pass, not
preconditions for the convergence DECISION. Their absence is a packaging/authoring task, not a
research or design question. It does not block declaring convergence; it is what declaring
convergence triggers. **Flagged honestly: freeze is not "done" until these four are authored — but
none is an open question.**

## (c) SK-V20 chronic items C1-C6 — each with named wave AND gate

| Item | Disposition | Close gate |
|---|---|---|
| C1 AoS/SoA, Lock 1 | §4 → TW1b (AoS hardening, NOT columnar pivot) | Lock 1 green, falsified two ways: grep-zero `TapeStructBuilder` + runtime single-owner `frame_depth` assert (TW1b:576-577) |
| C2 C fold / one value generator | §5,§9 → TW2+TW3 | "ONE emit path serves all 9 grammars" (TW2:588); every `->` reaches emitter (TW3:599) |
| C3 BackendShape / decision engine | §5 → TW4 (right-sized: greedy Extractor default + measured KernelCostTable) | cutover-gate + Lock-14 full-system (TW4.2:609-612) |
| C4 scanner unification | §5 → TW4.2 (lo/hi nibble two-LUT) | per-kernel differential parity + bench (TW4.2:608) |
| C5 Lock-16 single-movemask co-gate | §5 → TW4.2 (vshrn single-pack) | structural co-gate (TW4.2:608) |
| C6 D-G pre-block reconcile | §7,§11 → TW0 FIRST commit, made executable | doc-only commit: 4-id ledger entry + family meanings + `sk-v19-frozen` tag + supersession pointer (TW0:527-538) |

**All six dispositioned with a named wave and a gate.** C6 — the one that "gates all substrate
waves" — is the single most-improved: pass-2 rewrote it from a hidden zero-LOC gate into a fully
enumerated first-commit spec (REDRESS 51/53 retained-shortcut family, 246/247 fixed-shape/Unicode
family, all named so no wave reopens them by renaming).

## (d) Coordination seams — each dated/owned

- **parse-that LR/packrat NO-GO** — §7.1, **dated 2026-07-05**; TW0 records the outbound letter;
  GO-conditional fold drops to a note. DATED + OWNED.
- **csp-solver re-vendor** — §7.2; 0.2.0 API break pre-staged mechanically (drop `backjumping:false`,
  add `..Default::default()`, sweep three pins to "0.2"); Lock-4 fix re-vendors against csc411
  upstream (never forks) with a diff-against-upstream checkpoint preceding; owned by TW0 (§11:542).
  STAGED + OWNED.
- **bbnf-regex two-crate collision (N7)** — §7.3; TW2 deletes the skinny 322-line facade + the
  ad-hoc FIRST scanner onto ONE canonical home; the parse-that 1.0.0 cut is TS-only, Rust tree
  unaffected; no smallbox re-import (A3 D2 discharged, count=0). OWNED (TW2).

All three dated and/or wave-owned. No open coordination question.

## (e) TW0's ~14 workstreams — sequenced, or a pile?

Enumerated from §11:527-556: (1) D-G reconcile, (2) 24-crate supersession doc, (3) five-lock
falsifier audit + meta-lock, (4) Y.13 gate-template adoption, (5) Lock-15 + target-cpu, (6) Lock-4
+ csc411 checkpoint, (7) W5d x86/wasm deletion, (8) inline-test relocation (7+21), (9) MASTER-PLAN
§13.6 reconcile, (10) oracle hygiene / W3C rekey, (11) truth-floor quiet bench session, (12)
parse-that NO-GO letter, (13) playground junk ignore. ~13 items.

**Partially sequenced, not a raw pile.** A fixed first commit (D-G) and a 5-anchor spine are given
(§13.2: "D-G reconcile → supersession docs → lock audit → Lock-15 → truth-floor session"). The
remaining ~8 are unordered, deferred to the execution plan — §13.2 states the implementation tranche
"sequences TW0 internally … rather than re-lettering," and the ledger classes this R3 residue
"non-blocking." **This is the single least-crisp residual** — a spine with a first commit, not a
fully-ordered list — but it is an execution-plan task owned by TW0, not an open design question.

---

## What is NOT wave-owned

**Nothing that is a research or design question.** The candidates and their resolution:

- **§13.1 CSS SIMD tokenization severity** — the only surviving frontier uncertainty (confirmed-by-
  absence, real comment/`url()` correctness risk). The spec consciously ring-fences it: NOT scheduled
  as TW4 implementation scope; "it enters only via a costed per-grammar decision once the TW0
  measurement loop is live … a scheduled decision point, not a design unknown." The MECHANISM (a
  costed cost-model decision inside TW4's measurement loop) is defined; only the OUTCOME awaits
  measurement. This is a decide-when-measured posture consistent with the profile-driven law (§1.3),
  i.e. wave-owned to TW4 — not an open design question left dangling.
- **The final packaged deliverables** (SPEC.md rename, GESTALT-SYNTHESIS, top-level DEFERRED-LEDGER,
  forward prompt) — freeze-pass authoring output, not questions.

**Every remaining blocker of consequence is an execution-tranche deliverable owned by a named wave:**
TW0 quiet (loadavg<2) re-certification of the directional absolutes (the sole remaining MEASUREMENT
of consequence, incl. the un-confounded nightly/stable channel ratio); TW-S cold-parity re-capture;
TW5 step-0 width-fix re-land; TW6 regen wiring + CSV flip. These are exactly the four the freeze
criterion enumerates — all present and gated in §11.

---

## Verdict

**READY-TO-FREEZE.** The 21:47 amendment folded every pass-2 correction the v2 critiques and the
blocker-audit demanded before freeze; this audit verified each fold-in against its mandate and each
matches. C1-C6 each carry a named wave and a gate; the three coordination seams are dated/owned;
TW0 has a fixed first commit and a 5-anchor spine with the tail deferred to the execution plan. No
open research or design question survives — the lone frontier uncertainty (CSS SIMD tokenization) is
consciously ring-fenced as a costed TW4 decision, and every measurement/build residual is a named
execution wave (TW0/TW-S/TW5/TW6).

**residuals_wave_owned = true.**

**Convergence: 95/100.** Held below 100 by three formalities, none a research question: (1) the
final deliverables (SPEC.md, GESTALT-SYNTHESIS, DEFERRED-LEDGER, forward prompt) are freeze-pass
output not yet authored; (2) the pass-3 re-score is discharged by THIS audit's fold-in verification
rather than a ledgered independent arch-v3/honesty-v3 numeric round; (3) TW0's tail (~8 of 13
workstreams) is spine-ordered but not fully sequenced. The freeze pass should: rename the spec,
author the three missing companions + forward prompt, ledger the pass-3 entry, then commit the D-G
reconcile and open TW0.
