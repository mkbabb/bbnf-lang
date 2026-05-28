# CH1 CORRECTNESS - T-P3 V1

Verdict: REVISE

Scope: audited `restart/audit/totality/p3/3A-architecture-synthesis.md`,
`3B-master-plan-reconciliation.md`, `3C-locks-crystallisation.md`,
`3C-locks-v+1-diff.md`, `3D-skinny-fold.md`,
`3E-grammar-generalisation.md`, and `3F-migration-handoff.md` against
`restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md:57`-`60`.

## Evidence Commands And Outputs

```sh
$ git show --stat --oneline 0a0508acd -- restart/audit/totality/p3
0a0508acd docs(sk-v15-t-p3): add V1 synthesis packet
 .../audit/totality/p3/3A-architecture-synthesis.md |  92 ++++++++++++
 .../totality/p3/3B-master-plan-reconciliation.md   | 167 +++++++++++++++++++++
 .../audit/totality/p3/3C-locks-crystallisation.md  | 114 ++++++++++++++
 restart/audit/totality/p3/3C-locks-v+1-diff.md     |  76 ++++++++++
 restart/audit/totality/p3/3D-skinny-fold.md        |  93 ++++++++++++
 .../audit/totality/p3/3E-grammar-generalisation.md | 145 ++++++++++++++++++
 restart/audit/totality/p3/3F-migration-handoff.md  | 120 +++++++++++++++
 7 files changed, 807 insertions(+)
```

```sh
$ git diff --check 0a0508acd^ 0a0508acd -- restart/audit/totality/p3
# no output; exit 0
```

```sh
$ awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
$ git apply --check /tmp/tp3-locks-v1.diff
# no output; exit 0
```

```sh
$ grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
16

$ find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
      67
```

```sh
$ rg -o '^\\| (LAC-1E-[A-Z0-9-]+|T2A-V1-LAC-[0-9]+|LAC-2B-[A-Z0-9-]+|LAC-2C-[A-Z0-9-]+|LAC-2D-[0-9]+|LOCK16-[A-Z0-9-]+|LAC-2F-[A-Z0-9-]+)' \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p2/2A-sota-landscape.md \
  restart/audit/totality/p2/2B-primitive-vocabulary.md \
  restart/audit/totality/p2/2C-grammar-neutrality.md \
  restart/audit/totality/p2/2D-cost-model.md \
  restart/audit/totality/p2/2E-host-arch-esoterica.md \
  restart/audit/totality/p2/2F-parse-that-gaps.md | sed -E 's/.*\\| //' | sort -u > /tmp/ch1-live-lacs.txt
$ rg -o '^\\| (LAC-1E-[A-Z0-9-]+|T2A-V1-LAC-[0-9]+|LAC-2B-[A-Z0-9-]+|LAC-2C-[A-Z0-9-]+|LAC-2D-[0-9]+|LOCK16-[A-Z0-9-]+|LAC-2F-[A-Z0-9-]+)' \
  restart/audit/totality/p3/3C-locks-crystallisation.md | sed -E 's/.*\\| //' | sort -u > /tmp/ch1-3c-lacs.txt
$ wc -l /tmp/ch1-live-lacs.txt /tmp/ch1-3c-lacs.txt
42 /tmp/ch1-live-lacs.txt
42 /tmp/ch1-3c-lacs.txt
$ comm -23 /tmp/ch1-live-lacs.txt /tmp/ch1-3c-lacs.txt
# no output
$ comm -13 /tmp/ch1-live-lacs.txt /tmp/ch1-3c-lacs.txt
# no output
```

```sh
$ perl -ne 'while(/`([^`]+):(\\d+)`/g){print "$ARGV:$.:$1:$2\n"} close ARGV if eof' \
  restart/audit/totality/p3/3A-architecture-synthesis.md \
  restart/audit/totality/p3/3B-master-plan-reconciliation.md \
  restart/audit/totality/p3/3C-locks-crystallisation.md \
  restart/audit/totality/p3/3C-locks-v+1-diff.md \
  restart/audit/totality/p3/3D-skinny-fold.md \
  restart/audit/totality/p3/3E-grammar-generalisation.md \
  restart/audit/totality/p3/3F-migration-handoff.md |
  while IFS=: read -r src srcline refpath line; do
    if [ ! -f "$refpath" ]; then echo "MISSING $src:$srcline -> $refpath:$line";
    else n=$(wc -l < "$refpath" | tr -d ' ');
      if [ "$line" -gt "$n" ]; then echo "OOB $src:$srcline -> $refpath:$line (max $n)"; fi;
    fi;
  done
OOB restart/audit/totality/p3/3C-locks-v+1-diff.md:34 -> restart/audit/totality/p2/2F-parse-that-gaps.md:518 (max 122)
```

```sh
$ rg -n '2F-parse-that-gaps\.md:518|ORCHESTRATOR-PROMPT\.md' \
  restart/audit/totality/p3 restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md restart/locks/LOCKS.md
restart/locks/LOCKS.md:578:    `restart/audit/totality/p2/2F-parse-that-gaps.md:518`,
restart/audit/totality/p3/3C-locks-v+1-diff.md:34:     `restart/audit/totality/p2/2F-parse-that-gaps.md:518`,
restart/audit/totality/p3/3F-migration-handoff.md:117:| CH1 | `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md` names `restart/skinny/tranches/sk-v15/ORCHESTRATOR-PROMPT.md` as an SK-V15 authority (`restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:34`), but that file is absent in the current checkout. Should Pass Omega V5 CRUD create it, remove the reference, or route to `DISPATCH-PROMPT.md` only? | Receiver: Pass Omega V5 Omega-A/Omega-F. Blocker: absent authority file. Gate: CH1 path-resolution. |
```

```sh
$ ls restart/skinny/tranches/sk-v15
DISPATCH-PROMPT.md
HANDOFF.md
SPEC.md
SYNTHESIS.md
audit-overfit
research
```

```sh
$ git diff --name-only -- restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/locks/LOCKS.md restart/HANDOFF.md restart/MIGRATION.md
# no output
$ git diff --cached --name-only -- restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/locks/LOCKS.md restart/HANDOFF.md restart/MIGRATION.md
# no output
$ git show --name-only --format= 0a0508acd | rg '^(restart/ARCHITECTURE.md|restart/MASTER-PLAN.md|restart/locks/LOCKS.md|restart/HANDOFF.md|restart/MIGRATION.md)$' || true
# no output
```

## Findings

| id | severity | target artifact line | conflicting evidence | finding | repair directive | owner |
|---|---|---|---|---|---|---|
| CH1-V1-001 | medium | `restart/audit/totality/p3/3C-locks-v+1-diff.md:34` | `restart/audit/totality/p2/2F-parse-that-gaps.md:122` is the current file end; live inherited context at `restart/locks/LOCKS.md:578` cites non-existent `:518`. | The proposed diff includes an out-of-range path:line citation in its hunk context. The diff applies cleanly and the bad cite is inherited, but CH1's path-resolution rule applies to cited paths present in the target artifact. | Regenerate `3C-locks-v+1-diff.md` so the hunk anchors on valid context around `## v+1 Governance Boundary` and does not restate the stale `2F-parse-that-gaps.md:518` citation, or explicitly add a V2 note that the stale context cite is inherited and not proposed evidence. | 3C |
| CH1-V1-002 | low | `restart/audit/totality/p3/3F-migration-handoff.md:117` | `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:34` names `ORCHESTRATOR-PROMPT.md`; `restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md:29` names the extant SK-V15 evidence base without that file. | 3F correctly notices the missing authority file, but leaves the path-resolution decision as an open CH1 question. A V1 correctness packet should not carry an unresolved authority path when the existing `DISPATCH-PROMPT.md` path is present. | In V2, answer the question in 3F: route SK-V15 authority to `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` unless a separate orchestrator task creates `ORCHESTRATOR-PROMPT.md`; do not leave the absent path as a CH1-open blocker. | 3F / dispatch-context owner |

## Accepted Checks

- Every live 1E and 2A-2F lock-amendment candidate is represented in the 3C
  disposition matrix: 42 live candidates, 42 matrix candidates, no missing or
  extra ids. This satisfies the singularity requirement in
  `restart/prompts/totality/PASS-3-SYNTHESIS.md:50`-`52`.
- The extracted `3C-locks-v+1-diff.md` patch applies to the current
  `restart/locks/LOCKS.md` with `git apply --check`.
- The live lock count is still 16 and the current Pattern H runtime file count
  is still 67, matching the invariants in
  `restart/audit/totality/p3/hardening/V1/CHALLENGE-CONTEXT.md:93`-`95`.
- The target packet at `0a0508acd` touched only the seven T-P3 artifacts under
  `restart/audit/totality/p3/`; no live V1 spec surfaces were edited by the
  target packet or by the current dirty worktree.
- The proposed deltas in 3A, 3B, 3C, 3D, 3E, and 3F carry path:line evidence to
  T-P1, T-P2, PASS-IMPL V1, SK-V15, or V1 surfaces. The two findings above are
  path-resolution hygiene defects, not evidence-chain absence for the main
  delta set.

## Repair Directives

1. 3C: emit a revised proposed diff whose context contains only resolving
   citations, while preserving clean `git apply --check` against current
   `restart/locks/LOCKS.md`.
2. 3F / dispatch-context owner: resolve the absent
   `restart/skinny/tranches/sk-v15/ORCHESTRATOR-PROMPT.md` reference by routing
   to the extant `DISPATCH-PROMPT.md` or by adding the missing file through an
   explicit owning task before it is cited as authority.

## Residual Risk

The worktree has unrelated dirty runtime, skinny, docs-precepts, and xtask
files. The CH1-required invariants still read 16 locks and 67 Pattern H runtime
files, and the live V1 spec surfaces are clean. Because the path-resolution
defects are in a diff context line and an explicitly surfaced open question,
the packet is not substantively wrong enough for REJECT; it is not clean enough
for ACCEPT.
