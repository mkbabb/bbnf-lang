# Pass Omega V9 CH4 Cost

Date: 2026-05-28.
Lens: CH4 cost, propagation, and CRUD scope.
Worker: Pass Omega V9 CH4 Cost hardening.
Scope: source packet at commit `17e7248fe` under PASS-OMEGA Section 3 CH4.
Write path: `restart/audit/totality/astral/V9/hardening/CH4.md`.

Evidence abbreviations below resolve to packet files at commit `17e7248fe`:
`ΩA` = `restart/audit/totality/astral/V9/ΩA-coherence-audit.md`;
`ΩB` = `restart/audit/totality/astral/V9/ΩB-skinny-lessons.md`;
`ΩD` = `restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md`;
`ΩE` = `restart/audit/totality/astral/V9/ΩE-skinny-corpus.md`;
`ΩF` = `restart/audit/totality/astral/V9/ΩF-migration-handoff.md`;
`SPEC` = `restart/skinny/tranches/sk-v15/SPEC.md`.

## Verdict

REVISE.

The proposed V9 amendments are cost-realistic as document-authority work: the
LOCKS addendum is a single small insertion, the MASTER change is one Section
13/25 authority repair, HANDOFF/MIGRATION are top-level authority replacements,
skinny corpus updates are status/provenance alignment, and ARCHITECTURE is an
implementation-status correction. The packet does not place source, generated
runtime, gate, RESULTS, REDRESS, or runtime deletion work into Omega/CRUD.

Revision is required because the MASTER/SPEC patch representation leaves an
avoidable propagation ambiguity: Omega-D says SK-V15 SPEC is unchanged, but
`master-plan-diff.md` includes a fake no-op SPEC diff block, and Omega-F uses
"CRUD/SPEC patches" / "SK-V15 SPEC/dispatch surfaces if authorized" wording.
Before CRUD, this must be folded to a strict read/no-op statement for
`restart/skinny/tranches/sk-v15/SPEC.md` and `DISPATCH-PROMPT.md`, unless a
later challenge artifact supplies an explicit conflicting source artifact.

## Cost Findings

1. LOCKS addendum size and location are acceptable.

   `locks-diff.md` inserts one addendum immediately before
   `## v+1 Governance Boundary` (`locks-diff.md:44-74`), matching the current
   boundary at `restart/locks/LOCKS.md:581` in the packet. The addendum preserves
   16 numbered locks and the exact five `BackendShape` variants while adding no
   directive, BIR variant, substrate, public substrate API, retained sidecar,
   lock, lock retirement, or sixth shape (`locks-diff.md:5-11`,
   `:45-71`). This is a bounded governance addendum, not implementation work.

2. MASTER Section 13/25 cost is reasonable, but the diff must be made
   unambiguous.

   `ΩD` limits the live edit to MASTER authority repair: mark SK-V14 /
   MP-NW blocks historical, add Section 13.5 for SK-V15 W0-W11, update Section
   25, and leave SK-V15 SPEC unchanged (`ΩD:30-35`, `ΩD:87-100`). The
   proposed MASTER content also blocks W12, challenge-time implementation
   overflow, documentation-only proof, stale CSS proof, x86/AVX-512 close, and
   SK-V16 deferral (`master-plan-diff.md:60-119`, `:123-145`). However, the
   checked-in diff uses range-less `@@` hunks and includes a no-op SPEC diff
   block (`master-plan-diff.md:148-164`). CRUD needs either an applyable patch
   or explicit insertion instructions plus a read/no-op SPEC statement.

3. HANDOFF/MIGRATION replacement is document-authority only.

   `ΩF` says the migration impact is a document-authority migration and does
   not directly authorize source edits, generated output movement, RESULTS,
   REDRESS, gate implementation changes, or runtime deletion
   (`ΩF:62-67`). Its receiver table routes deletes, retirements, provider
   replacement, lowerer work, FNV quarantine, and close proof to SK-V15 W0-W11
   dependency rows (`ΩF:75-96`, `ΩF:117-129`). This is the right propagation
   boundary for cost: CRUD-4 replaces stale top-level authority; implementation
   remains in later skinny waves.

4. Skinny corpus updates are broad but still doc-bounded.

   `ΩE` touches six skinny docs and names status-only updates for
   `INDEX`, `WORKSPACE`, `HARDENING`, `COMPILER`, `BENCH`, and limited
   `SUBSTRATE` alignment (`ΩE:50-60`). It explicitly says it does not edit
   live surfaces, change locks, modify `skinny/RESULTS.md` or
   `skinny/REDRESS.md`, stage, commit, or reopen substrate mechanics beyond
   authority/status alignment (`ΩE:316-321`). Phrases about `bbnf-bench`
   / gate fields should remain documentation requirements for W0/W1/W5/W6/W10,
   not CRUD-time code changes (`ΩE:126-135`, `ΩE:231-250`).

5. ARCHITECTURE implementation-status fixes are necessary and cost-bounded.

   `ΩA` routes ARCHITECTURE to CRUD-1 for current-authority repair, CSS /
   SinkOnly downgrade, Decision scaffold status, lowerer admission gating, host
   admission tightening, and FNV quarantine (`ΩA:96-111`, `ΩA:172-190`,
   `ΩA:212-254`, `ΩA:256-298`, `ΩA:317-326`). `ΩB` gives the same narrow
   implementation-status replacement: JSON is scoped guard evidence; CSS is
   diagnostic/open; Pattern H is 67 files with 0/67 provenance; Decision is
   scaffold; SIMD/primitive admission is Apple M5 Max/aarch64 only
   (`ΩB:51-54`). No ARCHITECTURE fold requires source movement.

## Required Folds

1. Replace the SK-V15 SPEC pseudo-diff in `master-plan-diff.md:148-164` with
   prose: "No Omega-D V9 diff is proposed for
   `restart/skinny/tranches/sk-v15/SPEC.md`; CRUD must treat SPEC and
   DISPATCH-PROMPT as read/no-op unless a later challenge artifact explicitly
   authorizes a conflicting diff."

2. Tighten `ΩF` wording at `ΩF:152-166` and `ΩF:170-179`: replace
   "CRUD/SPEC patches" and "SK-V15 SPEC/dispatch surfaces if authorized" with
   "authorized V1 corpus CRUD patches; SK-V15 SPEC/DISPATCH are read-only for
   V9." This preserves `ΩD`'s SPEC-unchanged rule and prevents hidden
   propagation into the skinny contract.

3. Make the MASTER Section 13/25 patch mechanically consumable before CRUD:
   either provide normal unified diff hunk ranges or restate it as explicit
   insert/replace operations. Do not use this fold to change SK-V15 SPEC.

4. Add one consolidated CRUD scope line before G-Omega: "V9 CRUD may touch only
   `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
   `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, `restart/MIGRATION.md`,
   `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md`,
   and V9 audit logs; it may not touch source, generated output, gates,
   `skinny/RESULTS.md`, `skinny/REDRESS.md`, or SK-V15 SPEC/DISPATCH."

## Confirmed Boundaries

- No hidden implementation work in Omega/CRUD: real implementation is routed to
  SK-V15 W0-W11 after G-Omega (`ΩF:21-34`, `master-plan-diff.md:95-119`).
- No source/generated/RESULTS/REDRESS movement: explicitly blocked by
  `ΩF` and `ΩE` (`ΩF:62-67`, `ΩE:316-321`).
- No W12 or challenge-time implementation overflow: blocked by SK-V15 SPEC and
  MASTER (`SPEC:165-170`, `master-plan-diff.md:95-96`).
- No doc-only implementation gate: SK-V15 SPEC rejects documentation-only close
  and requires HEAD command output, generated artifacts/diffs where relevant,
  strict parity/checkasm where relevant, cold measurements, and PASS-IMPL V2 or
  row-level intrinsic-block proof (`SPEC:76-84`, `SPEC:146`).
- SK-V15 SPEC remains unchanged per `ΩD` unless a later source artifact
  explicitly conflicts (`ΩD:30-35`, `ΩD:99-100`,
  `master-plan-diff.md:148-170`).
