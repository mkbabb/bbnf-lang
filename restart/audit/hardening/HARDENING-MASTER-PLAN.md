# Hardening — MASTER-PLAN (greenfield restart, master-plan target)

Date: 2026-05-03
Hardener: Stage-1 hardening agent under `restart/prompts/HARDENING.md`,
target=MASTER-PLAN.
Authoritative override consulted: Amendment 01.
Hard cap: 60 minutes; six-phase incremental commit cadence.

---

## §1 — Target identification

The audited target is the synthesizer's master plan composed of two
documents:

| Item | Path | Lines | Commit |
|---|---|---:|---|
| Master plan | `restart/audit/master-plan/MASTER-PLAN.md` | 1 418 | `a9a85f45` |
| Amendment 01 | `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` | 161 | `a5145a0b` |
| Total audited | | 1 579 | |

The amendment is authoritative; where master plan and amendment disagree,
amendment wins. Amendment 01 retracts the per-grammar declaration crate
default (33-member workspace down to 24); every per-grammar-crate site in
the master plan body must reconcile against that retraction.

Sister Stage-1 hardening reports (cumulative):

| Report | Lines | Commit | Punch-list size |
|---|---:|---|---:|
| `HARDENING-PASS-A.md` | 909 | `54018ac3` | 25 |
| `HARDENING-PASS-B.md` | 759 | `70fc372e` | 30 |
| `HARDENING-PASS-C.md` | 782 | `72c906cb` | 30 |
| Cumulative | 2 450 | | 85 |

This master-plan audit reconciles those 85 cumulative findings against
the master plan itself and adds master-plan-specific surgeries
(workspace-shape consistency, tranche allocation, locks-honoured table,
generated-LOC trajectory, carry-tag receivers) on top.

---

## §2 — Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | partial | 9 | 4 | 1 | Reconcile Lock 14 with Amendment 01; reframe Lock 1 OpenFrame; gate Lock 8 across non-perf tranches; surface Lock 13 generated subdir footnote |
| 2 Sequencing | partial | 8 | 2 | 0 | Tranche E carry-down to F/G/H is real; Tranche A.W3 Lock-14 retirement before Tranche E template-emit is a same-wave-substrate fault unless A.W3 is restated as IR-side scrub only |
| 3 Cohesion | partial | — | 7 | 1 | Orphan claims at §1, §3.2.9, §5.2 (Tranche E) re-anchor against Amendment 01; deciding-lock citations strengthened; verdict-bucket commentary §2 elaborates on REINVENT/REPLACE collisions |
| 4 SOTA Anchoring | partial | 4 | 3 | 0 | Tranche-J close gates name competitor numbers but mid-tranche perf milestones (E.W*, G.W*, H.W*) lack baselines; Lock 8 honoured-by-tranche needs per-wave anchors |
| 5 Grammar-Authoritative | violated | 2 | 28 | 5 | 33 → 24 reconciliation surgery: every per-grammar-crate site re-anchors; future-grammar test absent from master plan body; per-X table for "all 9 grammars" claims missing in §3, §4.20, §6.1, §11, §12 |
| 6 Generated-Code Budget | partial | 6 | 4 | 0 | §12.3 windows are present but §12.2 trajectory rests on per-grammar declaration crate distribution that Amendment 01 retracts; redistribute trajectory under template-emitted subdirs |
| 7 Friction Forecast | violated | 0 | 6 | 0 | No friction-surface enumeration; no verbatim error messages; no `pointer!` / `parse_in` / layout-lowering / Pratt-misfire / migration / new-grammar-onboarding cookbook commits |
| 8 Carry & Deferral | partial | 5 | 8 | 1 | "Pass C is silent on this" + "synthesizer adjudicates" + "user-gated" without receiver-blocker-gate triple; bbnf-cli / bbnf-py defers carry no receiving gate; OpenFrame retiral defers from D to E without same-wave consumer |
| 9 Greenfield Discipline | partial | 6 | 3 | 1 | Per-grammar declaration crate proliferation is a quick-solution disguise; specialised cohort `specialised/` module is a workaround; OR-disposition language ("rename OR merge") leaves quick-solution path open |

**Aggregate cohort tabulation (master-plan-specific items, not cumulative
with sister reports):**

- KEEP: 40
- REINVENT: 65
- DISCARD: 8
- Total master-plan items audited: 113

**Final decision: requires amendments — reissue as MASTER-PLAN-V2 after
punch list applies.**

The master plan's substantive shape (10-tranche allocation; convergent
pivot at E; commit-chain Option-3 ratification; docs re-do; greenfield
calendar) survives the lanes. What requires amendment is the *body*:
every per-grammar-crate reference re-anchors per Amendment 01; the
24-member workspace materialises everywhere; SOTA gates land per-wave
not just per-tranche-close; friction surfaces enumerate; carries name
receivers + blockers + gates; the future-grammar onboarding test
materialises in the tranche-E gate set. The pivot itself — Lock 1 +
Lock 13 + Lock 14 retiring as one architectural movement via
template-emit + direct-projection + Emitter coarsening — survives the
amendment in full. The 33-crate proliferation was overfitting on the
Lock-14 escape valve.

---

## §3 — Lane 1: Lock-Adherence

Standard: walk the 14 locks at `restart/locks/14-LOCKS.md` against the
master plan + Amendment 01. Per-lock cell + verdict.

(Per-lock content forthcoming in Phase 4.)

---

## §4 — Lane 2: Sequencing Discipline

Standard: every wave deliverable must land with same-wave or next-wave
consumer per `era-V-dta-psi-rut.md`. The master plan names 10 tranches
(A-J) totalling 53 waves. Per-wave audit.

(Wave-by-wave content forthcoming in Phase 4.)

---

## §5 — Lane 3: Cohesion

Standard: every claim verifiable from artefacts the master plan
produces or cites. Identify orphan claims and orphan deliverables.

(Cohesion content forthcoming in Phase 4.)

---

## §6 — Lane 4: SOTA Anchoring

Standard: every parse-throughput gate cites competitor + dataset +
platform. Non-throughput engineering gates do NOT claim Lock 8 honour.

(SOTA content forthcoming in Phase 4.)

---

## §7 — Lane 5: Grammar-Authoritative Discipline (Lock 14 deep dive)

Standard: zero `match grammar { Json => ... }` in proposed generic
crates; per-X tables for every "all grammars" claim; future-grammar
onboarding test verifying two-surface ceremony.

(Site-by-site sweep + future-grammar test forthcoming in Phase 4.)

---

## §8 — Lane 6: Generated-Code Budget

Standard: per-tranche per-wave per-grammar LOC delta projection;
xtask regen-cycle budget; baselines stated.

(Trajectory critique forthcoming in Phase 5.)

---

## §9 — Lane 7: Friction Forecast

Standard: enumerate user-API surfaces likely to confuse; verbatim error
messages; cookbook commitments.

(Friction surface enumeration forthcoming in Phase 5.)

---

## §10 — Lane 8: Carry & Deferral Audit

Standard: every "deferred to" / "carries to" / "future" / "TBD" /
"user adjudicates" names receiver, blocker, receiving gate.

(Carry table forthcoming in Phase 5.)

---

## §11 — Lane 9: Greenfield Discipline

Standard: no quick solutions; no workarounds; no legacy survives
uncontested; idiomatic gestalt; architectural transpositions.

(Greenfield critique forthcoming in Phase 5.)

---

## §12 — Punch list

Ordered surgical edits for V2. ~50-70 entries forthcoming in Phase 6.
Per entry: target file:line, verbatim edit, source verdict (REINVENT /
DISCARD), owner, scope, lanes producing the surgery.

(Punch list forthcoming in Phase 6.)

---

## §13 — Final readiness

(Decision + 3-5 sentence summary forthcoming in Phase 6.)
