# Pass Omega V9 Hardening V2 CH6 - Source-Map / Evidence Hygiene

Date: 2026-05-28.
Worker: CH6.
Head audited: `9d336c6062898b0ce70b4df6787c3538aa7f74b9`.
Scope: folded V9 Omega-A through Omega-F, Omega-E source/corpus map,
`locks-diff.md`, `master-plan-diff.md`, current T-P1/T-P2/T-P3 consolidated
authorities, SK-V15 SPEC/DISPATCH, and V1 CH6.
Write path: `restart/audit/totality/astral/V9/hardening/V2/CH6.md`.

## Verdict

ACCEPT.

The V2 fold resolves the V1 CH6 hygiene risks. Current authority files exist,
the commit anchors named by the V9 packet resolve, active V9 source files no
longer cite absent T-P2 V5/V4 authority tokens, the Lock 14 source-map
correction is routed to CRUD-6 audit cleanup without turning the stale
`skinny/xtask` path into a source-edit requirement, and the cited evidence
commands are reproducible at HEAD.

No source-map, citation-only, or evidence-hygiene issue should block G-Omega if
CH1 through CH5 are otherwise clean.

## Required Checks

| Check | Result | Evidence |
|---|---:|---|
| Current authority files exist | ACCEPT | Present at HEAD: `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`, `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md`, `restart/skinny/tranches/sk-v15/SPEC.md`, and `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`. |
| Commit anchors resolve | ACCEPT | `git rev-parse --verify` resolves `17e7248fe`, `8e7378025`, `cbafeb566`, `cafb95682`, `77b6e9fd7`, and `6f1dd8aae` as commits. |
| No absent T-P2 authority tokens in active V9 source files | ACCEPT | `rg -n "HARDENING-T-P2-V5|T-P2 V5|T-P2-V5|HARDENING-T-P2-V4|T-P2 V4|T-P2-V4" restart/audit/totality/astral/V9/Ω*.md restart/audit/totality/astral/V9/locks-diff.md restart/audit/totality/astral/V9/master-plan-diff.md` returns no hits. The old V1 hardening files still record the prior defect as history; they are not active source packet authority. |
| Lock 14 source-map correction is preserved | ACCEPT | Omega-A records the stale PASS-IMPL path `skinny/xtask/src/lock14_baseline.rs:2370-2379` only as a negative source-map example and corrects it to `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379` (`restart/audit/totality/astral/V9/ΩA-coherence-audit.md:300`-`315`). The live corrected file exists and line 2370 begins `GENERIC_SCAN_ROOTS`; the stale `skinny/xtask/src/lock14_baseline.rs` file is absent. Omega-A routes this to CRUD-6 audit/source-map cleanup, not source CRUD. |
| Evidence commands reproduce at HEAD | ACCEPT | `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returns `16`; `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` returns `67`; the line-1 generated/provenance scan over those 67 files returns `0`; the extracted `locks-diff.md` patch passes `git apply --check -`. |

## V2 Fold Hygiene

`master-plan-diff.md` is now explicitly a mechanically consumable operation
list, not a malformed unified diff: it says it intentionally contains no
`diff --git` block, lists exact operations for MASTER Section 13/25, and gives
post-CRUD verification commands (`restart/audit/totality/astral/V9/master-plan-diff.md:13`-`16`,
`:25`-`236`). It also adds the consolidated authorized touch scope and forbids
source, generated output, gates, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
SK-V15 SPEC/DISPATCH movement (`restart/audit/totality/astral/V9/master-plan-diff.md:17`-`23`).

SK-V15 SPEC/DISPATCH are no longer ambiguous V9 CRUD targets.
`master-plan-diff.md` states no Omega-D/V9 edit is proposed for those files and
CRUD-2 must treat them as read-only unless a later G-Omega packet explicitly
authorizes a new diff (`restart/audit/totality/astral/V9/master-plan-diff.md:202`-`215`).
Omega-F carries the same rule: after G-Omega, apply authorized V1 corpus CRUD
patches while SK-V15 SPEC/DISPATCH stay read-only for V9
(`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:21`-`26`,
`:162`-`168`).

V1 CH6 was directionally correct but used now-stale "CRUD/SPEC patches" wording.
The V2 source packet has tightened that wording, so CH6 no longer has a
citation-only or source-map objection to G-Omega.

## Close

ACCEPT. Route any remaining source-map transcript cleanup to CRUD-6 only. Do
not expand this into a source, generated-output, gate, SK-V15 SPEC/DISPATCH, or
skinny results/redress edit requirement.
