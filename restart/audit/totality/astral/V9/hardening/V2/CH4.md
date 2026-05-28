# Pass Omega V9 Hardening V2 CH4

Date: 2026-05-28.
Lens: CH4 CRUD/gate/scope audit.
Scope: folded V9 packet after the V1 CH4/CONSOLIDATED defects.
Output: `restart/audit/totality/astral/V9/hardening/V2/CH4.md`.

## Verdict

ACCEPT.

The folded V9 packet resolves the V1 CH4 blockers. G-Omega V9 is mandatory
before V9 CRUD application, CRUD scope is explicit and document-bounded,
CRUD-2 is MASTER-PLAN only, the handoff directive now names master-plan edit
operations rather than a malformed master/spec diff, and CRUD-3 LOCKS remains
G-Omega-gated with a concrete addendum.

## Required Checks

| Check | Disposition | Evidence |
|---|---|---|
| G-Omega V9 remains mandatory before CRUD. | ACCEPT | Omega-F says Pass Omega V9 converges and presents G-Omega first, then after G-Omega closes the authorized V1 corpus CRUD patches apply (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:21`-`25`). It repeats that G-Omega V9 is mandatory before any V9 patches make V1 surfaces authoritative (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:154`-`158`) and refuses bypass/optional treatment (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:194`-`203`). Omega-D and `master-plan-diff.md` also require G-Omega before MASTER CRUD (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:92`-`101`; `restart/audit/totality/astral/V9/master-plan-diff.md:1`-`4`). No folded V9 source reviewed directs CRUD application before convergence/authorization. |
| CRUD scope is consolidated and explicit. | ACCEPT | `master-plan-diff.md` carries the consolidated touch scope: V9 CRUD may touch only the listed V1 spec surfaces, six skinny corpus docs, and V9 audit logs; it may not touch source, generated output, gates, `skinny/RESULTS.md`, `skinny/REDRESS.md`, or SK-V15 SPEC/DISPATCH (`restart/audit/totality/astral/V9/master-plan-diff.md:17`-`23`). Omega-F independently says Omega-F itself does not authorize source edits, generated output movement, `RESULTS.md`, `REDRESS.md`, gate implementation changes, or runtime deletion (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:63`-`68`). |
| CRUD-2 is MASTER-PLAN only. | ACCEPT | Omega-D states the patch should not edit SK-V15 `SPEC.md`; the required update is a MASTER-PLAN authority repair (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:30`-`35`). The proposed summary lists MASTER edits and leaves SK-V15 SPEC/DISPATCH unchanged (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:87`-`101`). `master-plan-diff.md` is now a mechanically consumable operation list for CRUD-2, not a unified diff, and contains no extractable `diff --git` block (`restart/audit/totality/astral/V9/master-plan-diff.md:12`-`15`). Its read-no-op section says no Omega-D/V9 edit is proposed for SK-V15 SPEC/DISPATCH and CRUD-2 must treat them as read-only unless a later G-Omega packet explicitly authorizes a new diff (`restart/audit/totality/astral/V9/master-plan-diff.md:202`-`215`). |
| Handoff directive uses master-plan edit operations. | ACCEPT | Omega-F's handoff directive now requires presenting G-Omega V9 with the cycle summary, consolidated verdict, locks diff, master-plan edit operations, and CRUD operation list (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:170`-`178`). It no longer asks for a malformed master/spec diff or CRUD/SPEC patches; it says authorized V1 corpus CRUD patches exactly as authorized and explicitly says not to edit SK-V15 SPEC/DISPATCH (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:174`-`178`). |
| CRUD-3 LOCKS is G-Omega-gated and explicit. | ACCEPT | `locks-diff.md` is a proposed amendment, G-Omega gated (`restart/audit/totality/astral/V9/locks-diff.md:1`-`3`). The amendment is concrete: one addendum before `## v+1 Governance Boundary`, preserving 16 locks and five BackendShape variants and adding no new directive, BIR variant, substrate, public API, sidecar, lock, retirement, or sixth shape (`restart/audit/totality/astral/V9/locks-diff.md:5`-`11`, `:38`-`:47`). Omega-F maps CRUD-3 to `restart/locks/LOCKS.md`, marks it G-Omega-gated, and preserves 16 locks/five-shape canon unless Omega-C's concrete diff authorizes an amendment (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:161`-`166`). |

## V1 CH4 Defect Closure

V1 CH4 required local budgets/scope separation and no hidden implementation
authority (`restart/audit/totality/astral/V1/hardening/CH4.md:38`-`46`), and
the V1 consolidated record blocked advancement until CH4 fold actions landed
(`restart/audit/totality/astral/V1/hardening/CONSOLIDATED.md:17`-`22`,
`:49`-`:52`). The V9 V1 consolidated defect was narrower: remove MASTER/SPEC
ambiguity, make MASTER mechanically consumable, and add consolidated CRUD scope
(`restart/audit/totality/astral/V9/hardening/CONSOLIDATED.md:38`-`56`).

Those V9 V1 requirements are folded:

- `master-plan-diff.md` is an operation list for MASTER-PLAN only, not an
  applyable pseudo-diff (`restart/audit/totality/astral/V9/master-plan-diff.md:12`-`15`).
- SK-V15 SPEC/DISPATCH are read-only for V9 (`restart/audit/totality/astral/V9/master-plan-diff.md:202`-`215`;
  `restart/audit/totality/astral/V9/ΩF-migration-handoff.md:23`-`25`).
- The consolidated CRUD scope forbids source, generated output, gates,
  `skinny/RESULTS.md`, `skinny/REDRESS.md`, and SK-V15 SPEC/DISPATCH movement
  (`restart/audit/totality/astral/V9/master-plan-diff.md:17`-`23`).

## Residual Risk

The repository's live top-level `restart/HANDOFF.md` is still stale SK-V14/V8
authority (`restart/HANDOFF.md:5`-`28`). That is the intended CRUD-4 target, not
a V2 CH4 defect: folded V9 keeps it proposal-only until G-Omega V9 authorizes
the V1 corpus CRUD patch.
