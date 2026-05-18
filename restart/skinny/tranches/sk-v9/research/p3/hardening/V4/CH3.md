# SK-V9 S-P3 Hardening — CH3 REGRESSION — V4

Lens: CH3 REGRESSION (REDRESS-route closure / no-regression-envelope
integrity). Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-18.
V4 surface: the `docs(sk-v9-p3-v4)` commit `cef745d2` — a two-hunk,
single-file diff to `skv9-p3-A-candidate-shortlist.md` (8 insertions,
4 deletions), moving `gsoc-2018` from the W3 must-improve exit-gate
list to a no-regression-only clause.
Convergence rule: ≥95% × 2 consecutive cycles (`ORCHESTRATOR.md` §3Z).

V3 CH3 cleared 100%. The CH3 question for V4: does moving gsoc-2018
off the W3 must-improve list reopen any REDRESS route, weaken any
no-regression envelope, or perturb the typed-GO / direct-GO
protections?

---

## §1 — V4-diff regression audit

The V4 diff is two hunks, both in P3-A:

- **Hunk 1 (P3-A §2.2 C3 falsifiability gate, lines ~276-285).**
  Removes `gsoc-2018 ≥ 41198` from the must-improve enumeration;
  inserts a no-regression-only clause for gsoc-2018 at W3.
- **Hunk 2 (P3-A §4 classification table, C3 row, line ~734).**
  Removes `gsoc-2018 (partial)` from the C3 must-improve cell;
  appends "`gsoc-2018` no-regression-only (gap exceeds the
  per-delimiter budget — does not bind the W3 gate)".

The regression-lens concern is twofold: (a) whether *relaxing* a row
from must-improve to no-regression reopens a REDRESS route the SPEC
had closed, and (b) whether the W10b maintain block and the
typed-GO / direct-GO protections are perturbed. Neither hunk touches
the W10b block, the typed-GO rows, the direct-GO rows, the REDRESS
pre-block list (P3-A:312-327), the W3 hot-leaf falsifiers, or any
revert protocol. The audit confirms the move is an *alignment* of P3-A
to the honest S-P1/S-P2 finding, not a relaxation that uncovers risk.

---

## §2 — V4 dispositions

Verified against: `git show cef745d2`; P3-A §2.2 C3 + §4 table +
REDRESS pre-block list (`:312-327`); F-spec §2 W3 (`:134`), §6
(`:628-737`); P3-C §2 W3, §"W4b-2" (`:299`-region, `:353-367`);
`skinny/RESULTS.md:24`.

| # | Claim under review | Artefact | Verdict | Evidence |
|--:|---|---|---|---|
| 1 | Moving gsoc-2018 to no-regression-only at W3 does NOT reopen a REDRESS route | P3-A §2.2 C3, F-spec §6 | ACCEPT | The V4 framing — "its throughput gap exceeds the per-delimiter budget, so the union substrate alone cannot lift it" — *aligns* P3-A with the honest finding F-spec §6 §"OLS fit" row (`:134`: "Four LOSS rows exceed 130-460% of the per-byte budget — delimiter-only intervention is insufficient") and P2-E §6.4 ("zero of … cross the gate on the codec alone"). The REDRESS routes in play are 92 / 50 / 51 / 53 / 60-72 / 28+33 / 82-89 (P3-A:312-327) — all are *mechanism* pre-blocks (no parser-owned cursor, no UnionTape, no aux side tables, etc.). gsoc-2018's gate *status* is a row-threshold question, orthogonal to every one of those mechanism pre-blocks. Relaxing a row threshold cannot reopen a mechanism REDRESS. |
| 2 | The V4 edit closes — not opens — the cohort gap that a stale must-improve figure represented | P3-A vs F-spec §6 / P3-C §2 | ACCEPT | Before V4, P3-A claimed gsoc-2018 must reach `≥ 41198` at W3; F-spec §6 and P3-C §2 said it does not bind W3. A row claimed must-improve in one artefact and no-regression in another is itself a latent regression hazard — a redress agent reading P3-A would chase a 41198 target the SPEC disowns, then either over-build (scope creep) or fail the gate falsely. V4 removes that hazard. The edit *reduces* regression surface. |
| 3 | The W10b six-row maintain block in P3-A §2.2 is unchanged | P3-A §2.2 C3 | ACCEPT | Hunk 1's lower context boundary is `Hot-leaf: consume_structural ≤ 5%`; the W10b block (`canada ≥ 15866`, `citm_catalog ≥ 28630`, `instruments ≥ 15865`, `marine_ik ≥ 11831`, `mesh ≥ 12186`, `numbers ≥ 17596`, P3-A:287-291) sits below the diff window — byte-identical to V3. The six floors and the uniform `floor(today × 0.98)` convention are intact. |
| 4 | The W10b six-row maintain block is unchanged in F-spec §2 / §6 and P3-C §2 | F-spec, P3-C | ACCEPT | The V4 commit touches only P3-A. F-spec §6 clause 2 (`:693-705`) and P3-C §2 W3 maintain envelope retain the identical six rows / six floors. No W10b surface anywhere was perturbed. |
| 5 | The four typed-GO protections are unchanged | F-spec §2 W1 maintain (`:122`) | ACCEPT | The four typed-GO rows (`twitter`, `update_center`, `mesh`, `marine_ik` `real_typed_struct` Track 1, each must hold `A / GO`, no-regression vs SK-V9-open typed baseline `report.rs:718-724,795-801,810-816,853-859`) live in F-spec §2 W1 — untouched by the V4 commit. The V4 diff is W3-only and P3-A-only; it does not reach the W1 typed-GO maintain envelope. |
| 6 | The three direct-GO protections are unchanged | F-spec §2 W1/W4a/W4b-2 maintain | ACCEPT | The three direct-GO rows (`citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct A / GO`, no delta beyond noise — F-spec §2 W1 `:122`, W4a `:152`, W4b-2 `:170`) are unaffected: the V4 diff touches no direct codepath, no W1/W4a/W4b-2 surface, and no maintain-envelope clause. |
| 7 | gsoc-2018's W4b-2 no-regression basis (`≥ 21963`) is untouched and still honest | F-spec §2a, P3-C §"W4b-2", P3-A C5 | ACCEPT | F-spec §2a W4b-2 (`:169`) and P3-C §"W4b-2" (`:315` projection table row, `:353` clause 3) retain `gsoc-2018 Track 1 ≥ 21963` (`ceil(22184×0.99)`, `RESULTS.md:24`) as a no-regression-only admission ("A W4b-2 gate that claims gsoc-2018 closure is a paper-close", P3-C `:360`). The V4 W3-gate edit and the W4b-2 no-regression basis are mutually consistent — gsoc-2018 is no-regression at *both* waves; the V4 edit makes P3-A say at W3 what P3-C already said at W4b-2. No regression-envelope value moved. |
| 8 | The gsoc-2018 no-regression *floor* at W3 is implied, not weakened to "no floor" | P3-A §2.2 C3, F-spec §6 | ACCEPT | The V4 prose says gsoc-2018 "carries a no-regression-only clause at W3 and its partial improvement is recorded, not gated." A no-regression clause is not the *absence* of a gate — gsoc-2018 cannot regress below its today baseline (22184). F-spec §6 §"gsoc-2018 does NOT bind W3" (`:723-726`) confirms: W3 falsifies only on the structural hot leaf, and a *partial* close is the residual handed to W4 — i.e. gsoc-2018 still cannot fall, it simply is not required to *rise* to 41198. The protective floor is retained; only the aspirational ceiling is dropped. The honest S-P1/S-P2 posture (gap exceeds the delimiter budget) is preserved. |
| 9 | The W3 hot-leaf falsifiers are unchanged — gsoc-2018's relaxation does not weaken the structural falsification | P3-A §2.2 C3, F-spec §6, P3-C §2 | ACCEPT | W3's *binding* falsifier is `consume_structural ≤ 5%` on twitter/apache_builds + `JsonNodeKind::at_cursor ≤ 1%` (P3-A:285-286, F-spec §6 `:690-692` / `:726`, P3-C §2 W3 `:134`). The V4 edit retained the `Hot-leaf:` clause verbatim (it merely moved to a fresh line). gsoc-2018 was never a falsifier — F-spec §6 `:725-726`: "W3 falsifies only if the structural-rediscovery hot leaf does not drop to ≤ 5%." The V4 edit aligns P3-A with that; the structural falsification is undisturbed. |
| 10 | The C3 REDRESS pre-block list in P3-A §2.2 is unchanged | P3-A §2.2 C3 | ACCEPT | The C3 "REDRESS pre-blocks" paragraph (P3-A:312-327 — REDRESS 92/50/51/53/60-72/28+33/82-89 + the blanket pre-blocks) sits well below hunk 1's window. It is byte-identical to V3. No REDRESS-route closure is reopened or weakened. |
| 11 | The C3 risk class and dependency clauses are unchanged | P3-A §2.2 C3 | ACCEPT | The C3 "Risk class — MEDIUM … HIGH if folded in whole" (P3-A:300-307) and "Dependency — Depends on C2" (P3-A:308-311) clauses are below the diff window — untouched. The V4 edit does not perturb the W3 risk posture or the C2→C3 ordering. |
| 12 | The classification table's other rows (C1/C2/C4/C5/C6/C7) are unchanged | P3-A §4 | ACCEPT | Hunk 2 is a single-line replacement of the C3 row only. The C1/C2/C4/C5/C6/C7 rows — including C4's pre-existing "`gsoc-2018` no-regression-only" note (`:735`) and C5's "contributes … to `gsoc-2018`" note (`:736`) — are untouched. The V4 edit makes the C3 row *agree* with the C4 row on gsoc-2018; it introduces no new cross-row contradiction. |

---

## §3 — Aggregate verdict

**12 dispositions: 12 ACCEPT, 0 REVISE, 0 REJECT.**

ACCEPT rate = 12 / 12 = **100%.**

This **clears** the §3Z 95% threshold. The V4 gsoc-2018 W3-gate move
is a pure *alignment* edit, not a relaxation that uncovers risk:

- It does **not** reopen a REDRESS route. Every REDRESS route in
  C3's pre-block list (92/50/51/53/60-72/28+33/82-89 + blanket) is a
  *mechanism* pre-block; gsoc-2018's gate *status* is a row-threshold
  question orthogonal to all of them. A row-threshold relaxation
  cannot reopen a mechanism REDRESS.
- It **aligns** P3-A with the honest S-P1/S-P2 finding — gsoc-2018's
  51% gap exceeds the per-delimiter budget; the union substrate alone
  cannot lift it (F-spec §6 OLS-fit row, P2-E §6.4). Before V4, P3-A
  claimed a `≥ 41198` must-improve target the SPEC disowns — itself a
  latent regression hazard (a redress agent would chase a phantom
  target). V4 *reduces* regression surface.
- gsoc-2018's protective no-regression floor is **retained** — the
  edit drops the aspirational ceiling, not the floor; the row still
  cannot fall below its 22184 baseline.
- The W10b six-row maintain block, the four typed-GO protections, the
  three direct-GO protections, the W3 hot-leaf falsifiers, the C3
  REDRESS pre-block list, the C3 risk/dependency clauses, and the
  gsoc-2018 W4b-2 no-regression basis (`≥ 21963`) are **all
  unchanged** — the V4 diff is two lines, W3-gate-only, P3-A-only.

**CH3 V4 = 100%.** Paired with V3 CH3 at 100%, CH3 has two
consecutive ≥95% cycles — CH3 is converged on the V4 surface.
