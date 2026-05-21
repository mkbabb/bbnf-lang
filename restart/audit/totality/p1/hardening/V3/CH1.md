# T-P1 V3 CHALLENGE - CH1 CORRECTNESS

Disposition: REVISE.

Scope: CH1 correctness audit of the folded T-P1 V3 inventories under `restart/audit/totality/p1/1A..1F` against `restart/prompts/totality/PASS-1-EXCAVATION.md`, `restart/prompts/ORCHESTRATOR.md` CH1, and `restart/audit/totality/p1/hardening/HARDENING-T-P1-V2-CONSOLIDATED.md`.

CH1 requires every claim to resolve to a cited file:line, and V3 folding must address the prior V2 required fold before the pass can count toward convergence (`restart/prompts/ORCHESTRATOR.md:83`; `restart/prompts/ORCHESTRATOR.md:110-117`; `restart/prompts/totality/PASS-1-EXCAVATION.md:104-108`; `restart/prompts/totality/PASS-1-EXCAVATION.md:146-153`). The targeted V2 CH1 defects are materially folded. The remaining correctness defect is artifact identity/provenance: two inventories still declare `cycle: V2` in a V3 challenge set.

## Findings

| ID | Disposition | Finding | Evidence | Required fold |
|---|---|---|---|---|
| CH1-V3-001 | REVISE | `1A-substrate-evidence.md` is being audited as folded V3, but its frontmatter still declares `cycle: V2`. PASS-1's output schema requires each inventory to emit the cycle field, and the cycle protocol folds V{N} dispositions into V{N+1}. This metadata makes the artifact identity false for V3 even if the body contains accepted V2 corrections. | Inventory declares `cycle: V2` at `restart/audit/totality/p1/1A-substrate-evidence.md:4`; PASS-1 frontmatter schema requires `cycle: V{N}` at `restart/prompts/totality/PASS-1-EXCAVATION.md:64`; V3 folding requirement is governed by `restart/prompts/totality/PASS-1-EXCAVATION.md:146-153`. | Change the current folded inventory frontmatter to V3 and ensure its fold provenance reflects V3 inputs, without changing source evidence claims unless separately required. |
| CH1-V3-002 | REVISE | `1B-codegen-evidence.md` is likewise still labelled `cycle: V2` and lists only V1 hardening inputs in its frontmatter, so the current V3 evidence set does not unambiguously attest the V2-to-V3 fold for 1B. | Inventory declares `cycle: V2` at `restart/audit/totality/p1/1B-codegen-evidence.md:4`; its hardening inputs list V1 consolidated and V1 CH files only at `restart/audit/totality/p1/1B-codegen-evidence.md:7`; PASS-1 requires V{N+1} folds to address prior dispositions at `restart/prompts/totality/PASS-1-EXCAVATION.md:149-153`. | Relabel the artifact as V3 and add V2 fold provenance or an explicit no-CH1-change note if 1B had no V2 CH1-required correction. |

## V2 CH1 Required Fold Checks

| V2 required fold | V3 result | Evidence |
|---|---|---|
| `1C-runtime-evidence.md`: command-derived runtime file/test/LOC claims must be captured or downgraded. | ACCEPT. The file marks exact counts/test status as scan-derived or UNKNOWN, and adds verify actions for command capture instead of using uncaptured command output as closure evidence. | `restart/audit/totality/p1/1C-runtime-evidence.md:8`; `restart/audit/totality/p1/1C-runtime-evidence.md:23`; `restart/audit/totality/p1/1C-runtime-evidence.md:35`; `restart/audit/totality/p1/1C-runtime-evidence.md:41-45`; `restart/audit/totality/p1/1C-runtime-evidence.md:113-114`; `restart/audit/totality/p1/1C-runtime-evidence.md:123`; `restart/audit/totality/p1/1C-runtime-evidence.md:127`. |
| `1E-locks-evidence.md`: Lock 14 substantive rule must cite `restart/locks/LOCKS.md:78`; `restart/locks/LOCKS.md:1-17` may be used only for the scoped SK-V9 allowance. | ACCEPT. Substantive Lock 14 rows cite `:78`; the `:1-17` citation appears only in the scoped allowance candidate. | Fold statement at `restart/audit/totality/p1/1E-locks-evidence.md:45`; substantive rows at `restart/audit/totality/p1/1E-locks-evidence.md:75`, `restart/audit/totality/p1/1E-locks-evidence.md:91`, `restart/audit/totality/p1/1E-locks-evidence.md:106`; scoped allowance only at `restart/audit/totality/p1/1E-locks-evidence.md:109`; lock source text at `restart/locks/LOCKS.md:1-17` and `restart/locks/LOCKS.md:78`. |
| `1E-locks-evidence.md`: Lock 13 `wc -l` counts must be downgraded/captured. | ACCEPT. The inventory no longer asserts exact `wc` counts; it marks exact LOC as UNKNOWN pending a committed transcript or equivalent artifact. | Fold statement at `restart/audit/totality/p1/1E-locks-evidence.md:46`; Lock 13 row at `restart/audit/totality/p1/1E-locks-evidence.md:74`; divergence row at `restart/audit/totality/p1/1E-locks-evidence.md:90`; amendment candidate at `restart/audit/totality/p1/1E-locks-evidence.md:105`. |
| `1D-skinny-lessons.md`: substantive hardening citations must be replaced by primary evidence; single-substrate lesson must be narrowed to JSON evidence plus grammar-neutral candidate unless non-JSON substrate evidence is added. | ACCEPT. Hardening remains only fold provenance in frontmatter/summary, while substantive rows cite REDRESS, RESULTS, SK-V13, pass-contract, and live code. The single-substrate row is narrowed to "proved for JSON; grammar-neutral rule candidate." | Fold statement at `restart/audit/totality/p1/1D-skinny-lessons.md:37-38`; single-substrate row at `restart/audit/totality/p1/1D-skinny-lessons.md:44`; primary evidence rows at `restart/audit/totality/p1/1D-skinny-lessons.md:59-65` and `restart/audit/totality/p1/1D-skinny-lessons.md:71-80`; divergence rows at `restart/audit/totality/p1/1D-skinny-lessons.md:103-108`. |

## Notes

Mechanical citation resolution over the eight current inventories found no missing files or out-of-range line citations. This does not prove each cited line semantically supports every claim, but the V2 CH1 target defects now resolve to corrected language and cited primary evidence.

The CH1 disposition remains REVISE because V3 convergence cannot rely on an inventory set whose 1A and 1B artifacts still identify themselves as V2.
