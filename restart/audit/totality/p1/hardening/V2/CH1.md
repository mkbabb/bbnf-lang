# CH1 - CORRECTNESS (SK-V15 T-P1 V2)

Verdict: REVISE

Scope checked: `restart/prompts/ORCHESTRATOR.md` 3W/3Z,
`restart/prompts/totality/PASS-1-EXCAVATION.md`, V1 consolidated hardening,
and current T-P1 packet files `1A` through `1F` at HEAD
`2fcbc1dc87d91db725671f84fefce62ece0f503e`.

CH1 standard: claim citations must resolve from repo root, executable evidence
must be measurable, generated-provenance claims must not close on headers
alone, V1 REVISE folds must be visible in V2, and no orphan unresolved REVISE
may remain.

## Findings

| id | severity | file | line | evidence | required fold |
|---|---|---|---:|---|---|
| CH1-V2-001 | high | `restart/audit/totality/p1/1C-runtime-evidence.md`; `restart/audit/totality/p1/1E-locks-evidence.md`; `restart/audit/totality/p1/1F-coherence-scan.md` | 60, 76-81, 106, 116; 77, 85, 100, 131, 135; 74, 82-83, 113, 124, 152 | V1 consolidated fold `T-P1-V2-F02` required root-shorthand citation repair. V2 still contains non-root anchors such as ``:260``-``304``, ``:35``-``43``, bare `css_l4_declaration_values_extended/config.rs:1`-`9`, `forward lens :148-150`, and root file continuations like ``:445``-``:456``. These do not independently resolve from repo root under PASS-1 CH1. | Expand every shorthand to a full repo-root `path:line` or `path:line-line` citation. Avoid brace/glob citation forms for cited evidence. |
| CH1-V2-002 | high | `restart/audit/totality/p1/1A-substrate-evidence.md` | 17-22, 61-82 | Frontmatter lists 22 first-cycle IDs, but `divergence_count` totals 21: implemented 6, unimplemented 6, impl_exceeds 4, unknown 5. The table itself has 6 implemented rows, 7 unimplemented rows (`1A-SUB-009` through `1A-SUB-015`), 4 impl_exceeds rows, and 5 unknown/partial rows. This leaves the V1 `1A-SUB-022` count fold only partially discharged. | Reconcile the frontmatter count to the table, or reclassify a row explicitly. As written the count should be 6/7/4/5. |
| CH1-V2-003 | medium | `restart/audit/totality/p1/1F-coherence-scan.md` | 53-58, 69-83 | Frontmatter says implemented 2, unimplemented 9, impl_exceeds 0, unknown 1. The table classes show implemented 3 (`COH-010`, `COH-011`, `COH-013`), unimplemented 11 (`COH-001` through `COH-009`, `COH-014`, `COH-015`), and unknown 1 (`COH-012`). | Reconcile frontmatter counts to the V2 table, or change table classifications so the counts are mechanically auditable. |
| CH1-V2-004 | medium | `restart/audit/totality/p1/1A-substrate-evidence.md`; `restart/audit/totality/p1/1B-codegen-evidence.md` | 52; 34 | Both files have `cycle: V2` frontmatter, but 1A says "this V1 inventory" and 1B says "Cycle is V1 per user dispatch". That is stale self-description in a V2 packet and weakens freshness claims after commit `2fcbc1dc8`. | Replace stale V1 prose with V2 wording, or state explicitly that only the carried finding IDs are V1-origin rows. |

## Spot Checks

Passed:

- `git rev-parse HEAD` resolves to `2fcbc1dc87d91db725671f84fefce62ece0f503e`.
- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  returns 67, and generated-header grep under that root returns 0.
- `find skinny/crates/runtime/src/grammars -type f -name '*.rs' | wc -l`
  returns 48, and line-1 skinny generated-header grep returns 43.
- `1F-anti-pattern.md` and `1F-past-corpora.md` are explicitly marked
  `status: superseded-historical-auxiliary`, and 1F coherence lines 85-90
  identify `1F-coherence-scan.md` as the live SK-V15 1F inventory.
- Generated provenance is mostly fenced correctly: 1A keeps generated-comment
  evidence UNKNOWN, and 1C states that headers do not prove clean regeneration.

Failed:

- Root-resolving citation repair is not complete.
- At least two frontmatter count blocks do not reconcile with their own V2
  tables.

## Orphan-REVISE Check

OPEN. V1 consolidated folds `T-P1-V2-F01`, `F03`, `F04`, `F05`, `F06`,
`F07`, `F08`, and `F09` have visible V2 carriers. `T-P1-V2-F02` remains
orphan/partial because unresolved root-shorthand citations persist and the 1A
frontmatter count still fails to reconcile after the `1A-SUB-022` fold.

No CH1 REJECT finding is warranted; the packet is directionally fresh SK-V15
evidence. It still needs a V3 correction pass before CH1 can accept.
