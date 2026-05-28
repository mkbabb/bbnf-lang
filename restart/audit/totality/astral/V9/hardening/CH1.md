# Pass Omega V9 CH1 Correctness Hardening

Date: 2026-05-28.
Worker: Pass Omega V9 CH1 correctness hardening.
Scope: source packet at commit `17e7248fe` under `PASS-OMEGA.md` Section 3
CH1 correctness.

## Verdict

REVISE.

The V9 packet is substantively coherent around SK-V15 authority, G-Omega
gating, 16-lock preservation, five-`BackendShape` preservation, Pattern H 67,
and the PRUNE-then-REBUILD W0-W11 receiver. The required repository-level
checks pass for the packet commit, whitespace, lock count, Pattern H count, and
the proposed `LOCKS.md` diff.

The blocker is mechanical: `restart/audit/totality/astral/V9/master-plan-diff.md`
is labelled an exact proposed diff, but the extracted patch is malformed and
does not apply. A second correctness fold is required if the "no nonexistent
T-P2 V5 reference remains" condition is read literally: the V9 packet still
contains literal `HARDENING-T-P2-V5-CONVERGED.md` strings, although Omega-A uses
them only as a negative finding against live `restart/HANDOFF.md`.

## Required Run Checks

`git show --stat --oneline 17e7248fe -- restart/audit/totality/astral/V9`
returned:

```text
17e7248fe docs(omega-v9): add SK-V15 totality source packet
 restart/audit/totality/astral/V9/locks-diff.md     |  77 +++++
 restart/audit/totality/astral/V9/master-plan-diff.md   | 171 +++++++++++
 restart/audit/totality/astral/V9/ΩA-coherence-audit.md | 333 +++++++++++++++++++++
 restart/audit/totality/astral/V9/ΩB-skinny-lessons.md  | 103 +++++++
 restart/audit/totality/astral/V9/ΩC-locks-amendments.md | 137 +++++++++
 restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md | 124 ++++++++
 restart/audit/totality/astral/V9/ΩE-skinny-corpus.md | 321 ++++++++++++++++++++
 restart/audit/totality/astral/V9/ΩF-migration-handoff.md | 214 +++++++++++++
 8 files changed, 1480 insertions(+)
```

`git diff --check 17e7248fe^ 17e7248fe -- restart/audit/totality/astral/V9`
returned no output and exited 0.

`awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/locks-diff.md | git apply --check -`
returned no output and exited 0.

`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` returned `16`.

Pattern H count checks returned:

```text
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
      67

line-1 generated/provenance scan over those 67 files
       0
```

## Findings

### CH1-V9-001 - `master-plan-diff.md` is malformed and non-applyable

Severity: blocking revise.

Evidence:

- `restart/audit/totality/astral/V9/master-plan-diff.md:17-23` labels the
  content as `MASTER-PLAN Exact Proposed Diff`, then starts a unified diff with
  bare `@@` hunk headers.
- Extracting proposed diffs with the same `diff --git` extraction shape used
  for `locks-diff.md` fails:

```text
awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/master-plan-diff.md | git apply --check -
error: patch with only garbage at line 4
```

- The first extracted hunk line is `@@` at
  `restart/audit/totality/astral/V9/master-plan-diff.md:23`; `git apply`
  requires ranged hunk headers such as `@@ -751,... +751,... @@`.
- The file also includes a proposed SK-V15 SPEC diff despite saying no SPEC text
  change is proposed. `restart/audit/totality/astral/V9/master-plan-diff.md:148-164`
  contains a second diff that would add `# no Omega-D V9 changes` if made
  syntactically applyable. That is incoherent with
  `restart/audit/totality/astral/V9/master-plan-diff.md:12-15` and
  `restart/audit/totality/astral/V9/master-plan-diff.md:150-156`, which state
  that SK-V15 SPEC needs no text change.

Minimal required fold:

1. Replace the pseudo-diff hunks in `master-plan-diff.md` with a real unified
   patch against current `restart/MASTER-PLAN.md`, including ranged hunk
   headers and enough context to apply at current lines 751, 837, 1218, and
   1241.
2. Remove the SK-V15 SPEC no-op diff block entirely, or keep only prose saying
   no SPEC diff is proposed. Do not include an applyable hunk that adds a
   comment to the locked SPEC.
3. Re-run and record `git apply --check` for the extracted `master-plan-diff.md`
   patch.

### CH1-V9-002 - Literal nonexistent T-P2 V5 tokens remain in the V9 packet

Severity: revise if the V9 acceptance criterion is literal; otherwise advisory.

Evidence:

- `rg -n "T-P2 V5|HARDENING-T-P2-V5|T-P2-V5" restart/audit/totality/astral/V9`
  returns five hits in `ΩA-coherence-audit.md`.
- The hits at `restart/audit/totality/astral/V9/ΩA-coherence-audit.md:44-45`
  and `:81-89` are correctly framed as a negative finding: live
  `restart/HANDOFF.md:91-94` names a nonexistent T-P2 V5 authority, while
  current T-P2 authority is
  `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15-19`.
- The task condition says to verify that no nonexistent T-P2 V5 reference
  remains in the V9 packet. Under that literal reading, these strings still
  remain even though they are not used as authority.

Minimal required fold:

Replace the literal nonexistent path strings in the V9 packet with wording that
cites only the live surface and current authority, for example: "HANDOFF item 6
names an absent T-P2 V5 file at `restart/HANDOFF.md:91-94`; replace it with
`HARDENING-T-P2-V3-CONSOLIDATED.md`." This preserves the finding without making
the V9 packet itself carry a nonexistent path token.

## Accepted Checks

Commit anchors: `git rev-parse --verify 17e7248fe^{commit}` resolves to
`17e7248fe96a9346970b4d9bdd02f84fc08a2c88`. The V9 packet's explicit commit
anchors `8e7378025`, `cbafeb566`, `cafb95682`, `77b6e9fd7`, and `6f1dd8aae`
also resolve.

Explicit file-line scan: a mechanical scan of explicit V9 `path:line` citations
found 158 refs, 156 current-tree resolving refs after accounting for the
intentional shorthand `LOCKS.md` cases, and no out-of-range current authority
citations. The only missing literal path with a line was the negative
source-map example at `restart/audit/totality/astral/V9/ΩA-coherence-audit.md:305`,
which states that PASS-IMPL cited stale
`skinny/xtask/src/lock14_baseline.rs:2370-2379` and immediately corrects it to
the live path `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2370-2379`.

LOCKS diff: `restart/audit/totality/astral/V9/locks-diff.md:5-11` proposes an
addendum before `## v+1 Governance Boundary`, and the current target location
is `restart/locks/LOCKS.md:581`. The extracted diff applies cleanly, adds no
numbered lock heading, and therefore preserves the live `16` lock count. The
diff text also preserves the five-shape canon at
`restart/audit/totality/astral/V9/locks-diff.md:23-30`, matching live code at
`skinny/crates/ir/src/lib.rs:339-346` and
`skinny/crates/ir/src/cost.rs:333-340`.

Omega-A CRUD routing: OA-01 through OA-13 each have an owning route. The summary
at `restart/audit/totality/astral/V9/ΩA-coherence-audit.md:319-326` assigns
CRUD-1 for architecture/status, CRUD-2 for MASTER, CRUD-3 for LOCKS preservation,
CRUD-4 for HANDOFF/MIGRATION, CRUD-5 for skinny corpus, and CRUD-6 for audit
cleanup/citation scrub.

Current V1 drift claims sampled cleanly:

- `restart/HANDOFF.md:3-28` still routes current work through Pass Omega V8 /
  SK-V14 W5B.0, while `restart/skinny/tranches/sk-v15/SPEC.md:172-185` defines
  W0-W11 and `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:63-68`
  routes Pass Omega V9 / G-Omega next.
- `restart/ARCHITECTURE.md:19-27` still says SK-V14 / T-P3 V4 authority, while
  T-P1 V5, T-P2 V3, and T-P3 V5 authority resolves at
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21-28`,
  `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15-19`,
  and `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:16-21`.
- `restart/MASTER-PLAN.md:751-827` is still the active-looking SK-V14 receiver
  block, and `restart/MASTER-PLAN.md:1218-1242` still says SK-V13 W0 is blocked
  pending Pass Omega/G-Omega; the proposed V9 direction to replace active
  dispatch with SK-V15 W0-W11 is content-correct, but the patch file must be
  made applyable.
- `restart/MIGRATION.md:30-52` and `restart/MIGRATION.md:129-144` still route
  receivers through Pass Omega V2..V8 / SK-V14, matching Omega-F's proposed
  CRUD-4 update need.

No source, generated output, `RESULTS.md`, `REDRESS.md`, locks surface, MASTER,
HANDOFF, MIGRATION, ARCHITECTURE, or skinny corpus file was edited by this CH1
worker.
