# SK-V15 T-P1 V2 Hardening Consolidated

Cycle: T-P1 Excavation V2.
Date: 2026-05-28.
Input inventories: `restart/audit/totality/p1/1A-substrate-evidence.md`
through `1F-coherence-scan.md`, plus superseded 1F auxiliary files.
Inventory fold commit: `2fcbc1dc8`.
Hardening root: `restart/audit/totality/p1/hardening/V2/`.

## Verdict

ACCEPT-RATE: 3 / 7 = 42.9%.

Cycle verdict: REVISE. V2 correctly folds the major V1 blocker classes,
but CH1, CH2, CH4, and CH6 found bounded residual work. No lens returned
REJECT.

## Lens Dispositions

| Lens | Disposition | Output | Fold surface |
|---|---|---|---|
| CH1 CORRECTNESS | REVISE | `V2/CH1.md` | Finish root-resolving citations, reconcile 1A and 1F frontmatter counts, and remove stale V1 self-description from V2 inventories. |
| CH2 GENERALITY | REVISE | `V2/CH2.md` | Integrate `P1-1B-D9` / `P1-1B-D10` into 1D's grammar-neutral findings table and add a compact Lock 14 owner/receiver leak map. |
| CH3 REGRESSION | ACCEPT | `V2/CH3.md` | `NEW-CH3-V5-01` delete/rebuild dependency rule is carried; REDRESS-183/184/209..213 remain pre-blocked. |
| CH4 COST | REVISE | `V2/CH4.md` | Re-key 1E cost/wave carriers, add LAC wave alignment, split broad implementation buckets, and add primitive-to-consumer proof rows. |
| CH5 HIDDEN COUPLING | ACCEPT | `V2/CH5.md` | CSS broadcast, string-literal generator, fact-stream/value boundary, root structural sidecar, and gate-exclusion carriers are present. |
| CH6 ANTI-PAPER-CLOSE | REVISE | `V2/CH6.md` | Downgrade broad Lock 1 / runtime ownership wording that still says `honoured` or `implemented` while evidence is partial. |
| CH7 OVERFIT-PRUNE | ACCEPT | `V2/CH7.md` | No CH7-specific contrivance remains; CSS/FNV/x86/sidecar/header-only risks are blocked or routed. |

## Deduplicated V3 Fold Roster

| id | required fold | target files |
|---|---|---|
| T-P1-V3-F01 | Expand root-shorthand citations to full repo-root `path:line` form in runtime, locks, and coherence rows. | `1C-runtime-evidence.md`, `1E-locks-evidence.md`, `1F-coherence-scan.md` |
| T-P1-V3-F02 | Reconcile 1A frontmatter counts to 6 implemented / 7 unimplemented / 4 impl_exceeds / 5 unknown, unless rows are explicitly reclassified. | `1A-substrate-evidence.md` |
| T-P1-V3-F03 | Reconcile 1F frontmatter counts to its V2 table or alter table classifications so the count is mechanically auditable. | `1F-coherence-scan.md` |
| T-P1-V3-F04 | Replace stale V1 self-description in V2 files with V2 wording or explicit "V1-origin finding ID" language. | `1A-substrate-evidence.md`, `1B-codegen-evidence.md` |
| T-P1-V3-F05 | Add a 1D grammar-neutral findings row for `P1-1B-D9` / `P1-1B-D10`: pass-layer recognizer mining and materialization role mining as non-JSON-specific Lock 14 failures with Sheets or BBNF-self proof receivers. | `1D-skinny-lessons.md` |
| T-P1-V3-F06 | Add compact cross-inventory Lock 14 leak owner/receiver map: surface, leak, classification, owner inventory, downstream receiver, proof expected. | `1F-coherence-scan.md` or `1C-runtime-evidence.md` |
| T-P1-V3-F07 | Re-key 1E cost/wave carrier so each row budgets the same divergence or LAC id it names; add wave/cost/risk/hard-cap alignment for every LAC candidate. | `1E-locks-evidence.md` |
| T-P1-V3-F08 | Split broad cost buckets such as Pattern H `10,000+`, ten-claim implementation groups, and `per receiver` rows into bounded owner-path receivers or sub-waves. | `1C-runtime-evidence.md`, `1D-skinny-lessons.md` |
| T-P1-V3-F09 | Add primitive/kernel receiver table naming primitive, wave, consumer path or row, proof command, and disposition if no consumer exists. | `1D-skinny-lessons.md`, `1E-locks-evidence.md`, or `1F-coherence-scan.md` |
| T-P1-V3-F10 | Downgrade 1E Lock 1 `honoured` wording to partial / JSON-tape-only until CSS fact-stream schema and substrate-target wording are disposed. | `1E-locks-evidence.md` |
| T-P1-V3-F11 | Scope 1A-SUB-001 from broad `implemented` to JSON/example implementation or partial because CSS lacks full retained value/view/visitor roster. | `1A-substrate-evidence.md` |

## Non-Findings

- No lens returned REJECT.
- V2 successfully demoted stale 1F auxiliary files and made
  `1F-coherence-scan.md` the live 1F authority.
- V2 successfully carries CSS audit-demotion, generated-provenance
  UNKNOWN routing, EventTape / typed-event-cursor fences, wave-graph
  delete/rebuild precedent, broadcast detection, and gate-exclusion
  reporting.
- JSON remains guard evidence only; it is not used as CSS or
  generalization closure.

## Next Dispatch

Fold the V3 roster into the T-P1 inventories before T-P2 dispatch. After
that fold, run a fresh CH1-CH7 hardening cycle over the V3 packet. T-P1
can lock only after two consecutive >=95% hardening cycles with zero
orphan REVISEs.
