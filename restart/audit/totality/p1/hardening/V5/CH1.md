---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V5
disposition: ACCEPT
fold_commit_under_review: af809cf27
current_head_verified: 919c25021
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
historical_auxiliaries_not_live:
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH1 - CORRECTNESS (SK-V15 T-P1 V5)

## Verdict

ACCEPT.

The V4 CH1 correctness defects were folded into the live inventories by
`af809cf27` (`docs(sk-v15-t-p1): fold CH1 V4 correctness fixes into V5
inventories`). All six live inventories named by the V5 context are unchanged
from that fold commit in the current worktree, the V4 residual shorthand checks
now return zero output, stale V3 self-description is gone, and the 1A / 1F
frontmatter count tables still reconcile after the FNV transcript addition.

No REJECT or REVISE finding is open for CH1.

## Evidence

CH1's authority is citation correctness: ORCHESTRATOR CH1 requires every claim
to resolve to file:line, commit SHA, RESULTS row, or REDRESS entry at
`restart/prompts/ORCHESTRATOR.md:81-84`; PASS-1 requires every
spec-claim/implementation row to carry resolving path:line evidence at
`restart/prompts/totality/PASS-1-EXCAVATION.md:104-108`.

V5 reviews the six live inventories after fold commit `af809cf27`, per
`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:3-19`. The same
context says `1F-anti-pattern.md` and `1F-past-corpora.md` are historical and
superseded at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:21-24`.
The CH1 V5 lens focus is exactly the folded CH1 surface: current live
inventories, residual shorthand greps, stale V3 self-description, and 1A / 1F
count reconciliation at
`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:63-68`.

V4's fold roster required four repairs: expand JSON scan/sink citations, expand
COH-016 generated/FNV citations, replace stale V3 prose, and re-run mechanical
checks at
`restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:34-41`.
V4 CH1 recorded the concrete failing shorthand and stale-prose outputs at
`restart/audit/totality/p1/hardening/V4/CH1.md:76-112` and
`restart/audit/totality/p1/hardening/V4/CH1.md:136-159`.

Currentness after `af809cf27`:

```text
$ git show --stat --oneline af809cf27 -- \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
af809cf27 docs(sk-v15-t-p1): fold CH1 V4 correctness fixes into V5 inventories
 restart/audit/totality/p1/1A-substrate-evidence.md |  4 ++--
 restart/audit/totality/p1/1B-codegen-evidence.md   |  2 +-
 restart/audit/totality/p1/1F-coherence-scan.md     | 16 ++++++++++++++--

$ git diff --name-status af809cf27 -- \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
(no output)
```

All six live inventories carry current `cycle: V4` frontmatter for the SK-V15
T-P1 V5 challenge review:
`restart/audit/totality/p1/1A-substrate-evidence.md:4`,
`restart/audit/totality/p1/1B-codegen-evidence.md:4`,
`restart/audit/totality/p1/1C-runtime-evidence.md:4`,
`restart/audit/totality/p1/1D-skinny-lessons.md:4`,
`restart/audit/totality/p1/1E-locks-evidence.md:4`, and
`restart/audit/totality/p1/1F-coherence-scan.md:4`.

Mechanical CH1 checks over the six live inventories:

```sh
rg -n -o '`[^`]*(?:\.md|\.rs|\.toml):[0-9][^`]*`' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md \
  | rg -v ':`(restart|skinny|crates|xtask)/|:`Cargo\.toml:|:`skinny/Cargo\.toml:'

rg -n -o '`:[0-9][0-9]*(?:-[0-9][0-9]*)?`' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md

rg -n '`[^`]*\{[^`]*\}[^`]*\.(rs|md|toml):[0-9][^`]*`' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md

rg -n 'Cycle is V3|this V3 inventory|cycle: V3' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

All four commands returned zero output. Re-running the V4 glob form for the
first three checks over `restart/audit/totality/p1/1*.md` also returned zero
output.

Folded citation repairs resolve in the live files. The 1A JSON scan/sink row now
uses repo-root citations at
`restart/audit/totality/p1/1A-substrate-evidence.md:79`. The underlying files
resolve at `skinny/crates/runtime/src/grammars/json/scan.rs:1` and
`skinny/crates/runtime/src/grammars/json/sink.rs:1`. 1A and 1B now describe the
current inventory as V4 at `restart/audit/totality/p1/1A-substrate-evidence.md:56`
and `restart/audit/totality/p1/1B-codegen-evidence.md:37`; the stale V3 strings
from V4 CH1 are absent.

The COH-016 FNV row now cites repo-root generator template lines at
`restart/audit/totality/p1/1F-coherence-scan.md:89`, and the transcript lists
all seven generated CSS runtime files with root-resolving `generated.rs:25`,
`generated.rs:71`, and `generated.rs:619` paths at
`restart/audit/totality/p1/1F-coherence-scan.md:91-101`. Spot verification
found the same line positions in live source; for example, the template emits
`source\tinput_fnv64` at `skinny/crates/codegen/src/runtime_generator.rs:737` and
`skinny/crates/codegen/src/runtime_generator.rs:783`, and `fn fnv64` at
`skinny/crates/codegen/src/runtime_generator.rs:1331`.

1A count reconciliation still matches frontmatter. Frontmatter says
`spec_claims_implemented: 6`, `spec_claims_unimplemented: 7`,
`impl_exceeds_spec: 4`, and `unknown: 5` at
`restart/audit/totality/p1/1A-substrate-evidence.md:22-26`. Counting the 22 table
rows at `restart/audit/totality/p1/1A-substrate-evidence.md:65-86` gives five
`implemented` plus one `partial / JSON-example implemented`, seven
`unimplemented`, four `impl_exceeds_spec`, and three `unknown` plus two
`partial / UNKNOWN routed`.

1F count reconciliation also matches frontmatter after COH-016. Frontmatter says
`3 / 11 / 0 / 2` at
`restart/audit/totality/p1/1F-coherence-scan.md:58-62`. The spec-claim table at
`restart/audit/totality/p1/1F-coherence-scan.md:74-89` contains three
`implemented` rows, eleven `unimplemented` rows, and two `unknown` rows:
`COH-012` at `restart/audit/totality/p1/1F-coherence-scan.md:85` and `COH-016`
at `restart/audit/totality/p1/1F-coherence-scan.md:89`.

## Findings

| id | disposition | severity | finding | evidence |
|---|---|---|---|---|
| CH1-V5-001 | ACCEPT | none | The six live inventories are current after fold commit `af809cf27`; no live inventory has working-tree drift from that fold. | V5 live roster at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:14-19`; fold commit at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:3-4`; `git diff --name-status af809cf27 -- <six live inventories>` returned zero output. |
| CH1-V5-002 | ACCEPT | none | Residual non-root shorthand, colon-only shorthand, and brace-path citations from V4 CH1 are gone. | Zero-output greps above; 1A repair at `restart/audit/totality/p1/1A-substrate-evidence.md:79`; 1F repair at `restart/audit/totality/p1/1F-coherence-scan.md:89` and `restart/audit/totality/p1/1F-coherence-scan.md:91-101`. |
| CH1-V5-003 | ACCEPT | none | No stale V3 self-description remains in the six live inventories. | Zero-output stale-V3 grep above; current 1A wording at `restart/audit/totality/p1/1A-substrate-evidence.md:56`; current 1B wording at `restart/audit/totality/p1/1B-codegen-evidence.md:37`. |
| CH1-V5-004 | ACCEPT | none | 1A frontmatter counts match the 1A spec-claim table. | Frontmatter `6 / 7 / 4 / 5` at `restart/audit/totality/p1/1A-substrate-evidence.md:22-26`; counted table rows at `restart/audit/totality/p1/1A-substrate-evidence.md:65-86`. |
| CH1-V5-005 | ACCEPT | none | 1F frontmatter counts match the 1F table after the COH-016 FNV transcript addition. | Frontmatter `3 / 11 / 0 / 2` at `restart/audit/totality/p1/1F-coherence-scan.md:58-62`; COH table at `restart/audit/totality/p1/1F-coherence-scan.md:74-89`; COH-016 unknown row at `restart/audit/totality/p1/1F-coherence-scan.md:89`. |
| CH1-V5-006 | ACCEPT | none | Historical 1F auxiliary files were not treated as live SK-V15 inventories. | V5 context marks `1F-anti-pattern.md` and `1F-past-corpora.md` historical at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:21-24`; this review's live roster is the six files at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:14-19`. |
