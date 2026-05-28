# CH5 HIDDEN COUPLING - T-P3 V5

Verdict: ACCEPT

Target packet: `77b6e9fd7` (`docs(sk-v15-t-p3): repair V4 citation finding`).

No CH5 defect is present. The V5 target packet repairs one 3A citation and does
not change the substantive hidden-coupling posture. The packet keeps Lock 1,
Lock 10, Lock 14, and Lock 16 aligned: no parallel substrate, retained sidecar
producer, renamed-scanner owner drift, Track 1 == Track 2 sidecar, FactStream
shape leak, runtime regex/DFA substrate, or x86 close evidence is admitted.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 77b6e9fd7 -- restart/audit/totality/p3` | `77b6e9fd7 docs(sk-v15-t-p3): repair V4 citation finding`; 1 file changed, 1 insertion, 1 deletion, limited to `restart/audit/totality/p3/3A-architecture-synthesis.md`. |
| `git diff --check 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3` | Clean; no output. |
| Extract fenced diff from `3C-locks-v+1-diff.md` and run `git apply --check` | Clean; no output. I used an stdin pipeline equivalent to the required `/tmp/tp3-locks-v5.diff` command to preserve this CH5 single-output-file ownership. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the locked invariant. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H runtime-file invariant. |
| Required stale-pattern `rg` scan | No matches, exit 1. |

## CH5 Audit

| risk | audit result |
|---|---|
| Parallel substrate / sidecar producer | Clean. 3A preserves no new public substrate API, retained sidecar, or sixth shape (`restart/audit/totality/p3/3A-architecture-synthesis.md:30`-`35`), 3B rejects hidden sidecar/parallel substrate routes (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:73`), 3D requires transient masks to be consumed into existing tape/direct/fact output (`restart/audit/totality/p3/3D-skinny-fold.md:66`), and 3C Lock 1 text rejects retained cursor/list/class-column/sidecar, parser-owned structural streams, public `UnionTape`, second tape, and cross-call classifier state unless G-Omega amends Lock 1 (`restart/audit/totality/p3/3C-locks-v+1-diff.md:42`). |
| Renamed-scanner Lock 1 violation | Clean. 3A names `parse-that-regex` as the canonical compile-time regex/HIR facts owner and makes `skinny/crates/bbnf-regex` a temporary legacy path only, not an admissible future owner (`restart/audit/totality/p3/3A-architecture-synthesis.md:72`). 3C repeats the same owner split in the Lock 16 clause and disposition matrix (`restart/audit/totality/p3/3C-locks-crystallisation.md:57`, `restart/audit/totality/p3/3C-locks-crystallisation.md:120`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). |
| Track 1 == Track 2 dishonesty | Clean. 3B explicitly keeps H.W4 partial and states W9 owns SinkOnly equality/all-five evidence with "no Track 1 == Track 2 sidecar" (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:91`). CSS rows are demoted to diagnostic unless row-local typed measurements exist (`restart/audit/totality/p3/3D-skinny-fold.md:60`; `restart/audit/totality/p3/3E-grammar-generalisation.md:70`). |
| Fact-stream shape leak | Clean. 3A clarifies `admitted_fact_output` as a `SubstrateTarget` / output-plane classification, never a `BackendShape` value or CSS Value API proof (`restart/audit/totality/p3/3A-architecture-synthesis.md:65`). 3C keeps `FactStream` out of `BackendShape` (`restart/audit/totality/p3/3C-locks-crystallisation.md:46`, `restart/audit/totality/p3/3C-locks-crystallisation.md:82`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:42`). 3E preserves the five-shape matrix without adding `FactStream` (`restart/audit/totality/p3/3E-grammar-generalisation.md:72`, `restart/audit/totality/p3/3E-grammar-generalisation.md:86`). |
| Runtime regex/DFA substrate | Clean. 3A rejects runtime regex/DFA engines as runtime substrate unless a prior G-Omega amendment changes Lock 1, with manifest plus consumer proof necessary but never sufficient (`restart/audit/totality/p3/3A-architecture-synthesis.md:72`, `restart/audit/totality/p3/3A-architecture-synthesis.md:109`). 3C mirrors the same rule in Lock 1 and Lock 16 (`restart/audit/totality/p3/3C-locks-crystallisation.md:46`, `restart/audit/totality/p3/3C-locks-crystallisation.md:121`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:42`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). |
| x86 close evidence | Clean. 3A requires Apple M5 Max/aarch64 primitive admission and treats x86/AVX-512 as diagnostic only (`restart/audit/totality/p3/3A-architecture-synthesis.md:71`). 3B keeps CollapsedStage diagnostic unless 2E supplies an aarch64 route (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:133`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:163`). 3C folds wrong-host evidence into aarch64-only CollapsedStage/primitive admission (`restart/audit/totality/p3/3C-locks-crystallisation.md:53`, `restart/audit/totality/p3/3C-locks-crystallisation.md:116`-`117`). 3E states no CollapsedStage admission for current SK-V15 grammars on M5 Max/aarch64 and leaves x86 diagnostic only (`restart/audit/totality/p3/3E-grammar-generalisation.md:99`). |

## Defects

None.

## Residual Risk

The current worktree contains unrelated dirty runtime, research, and xtask files.
No required invariant mismatch depended on dirty state, and no non-CH5 file was
modified for this verdict. The V5 target packet itself is citation-only in 3A,
so the prior CH5 hidden-coupling acceptance remains valid after the V4 citation
repair (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V4-CONSOLIDATED.md:32`).
