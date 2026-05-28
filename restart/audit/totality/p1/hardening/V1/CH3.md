# CH3 REGRESSION - T-P1 V1 (SK-V15)

## Lens Contract

CH3 is the T-P1 regression lens. Per `restart/prompts/ORCHESTRATOR.md:74-88`
and `restart/prompts/totality/PASS-1-EXCAVATION.md:116-119`, this audit checks
that T-P1 does not reopen rejected `skinny/REDRESS.md` routes, that 1D and 1E
identify the rejected-route pre-block list, and that admitted REDRESS rows are
not silently catalogued as unimplemented. Per `ORCHESTRATOR.md:104-116`, this
V1 challenge output is a fold-input for V2, not an implementation change.

SK-V15 adds `NEW-CH3-V5-01` at
`restart/skinny/tranches/sk-v15/SYNTHESIS.md:98-106`: any wave deleting or
retiring artefact X must prove the rebuild provider for X no later than that
same wave. The relevant REDRESS precedent is the SK-V14 wave-graph cycle line:
REDRESS-183, REDRESS-184, REDRESS-209..212, and REDRESS-213.

## Evidence Read

- `restart/prompts/ORCHESTRATOR.md` section 3W and 3Z.
- `restart/prompts/totality/PASS-1-EXCAVATION.md` section 3 CH3.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md` section 0.5.
- T-P1 inventories `1A`, `1B`, `1C`, `1D`, `1E`, and current `1F-coherence-scan.md`.
- Optional 1F sidecar files were read as stale carry-over context, not as the current SK-V15 V1 authority.
- `skinny/REDRESS.md` and `skinny/RESULTS.md`.

## Findings

| ID | Disposition | Finding | Evidence | Required V2 action |
|---|---|---|---|---|
| CH3-V1-001 | ACCEPT | T-P1 does not propose reopening the major rejected JSON W10/W11 routes. 1D keeps rejected parse_only/product attempts pre-blocked unless a future wave proves a fresh material differential. | 1D `J-7` at `restart/audit/totality/p1/1D-skinny-lessons.md:123`; REDRESS rejects include W10U at `skinny/REDRESS.md:5473-5489`, W10X/W10Y-W10Z at `:5534-5564`, W10AA at `:5672-5698`, W11Q/W11T/W11V at `:6101-6252`. | None. |
| CH3-V1-002 | ACCEPT | T-P1 does not reopen retained sidecar / structural-stream substrate routes. 1D blocks retained structural indexes, streaming cursors, class columns, and second substrates; 1F keeps sidecar absence as a verify-before-close question rather than a close claim. | 1D `G-7` at `1D-skinny-lessons.md:135`; 1D open question at `:181`; 1F sidecar verifier at `restart/audit/totality/p1/1F-coherence-scan.md:101` and `:132`; REDRESS structural-stream reject at `skinny/REDRESS.md:6176-6211`. | None for 1D/1F; see CH3-V1-007 for 1A/1B/1C cross-cite hardening. |
| CH3-V1-003 | ACCEPT | Admitted JSON REDRESS rows are preserved, not miscatalogued as unimplemented. 1D records JSON parse_only 17/17, direct 17/17, and typed 17/17 as the validated guard baseline, while separately routing FNV closed-enum products to quarantine. | 1D rows at `1D-skinny-lessons.md:92-96`, digest rows `:117-121`; RESULTS notes at `skinny/RESULTS.md:139-149`; W11W admit at `skinny/REDRESS.md:6254-6284`; W11A admit at `:5853-5872`; W11U supersession/admit note at `:6213-6220`. | None. |
| CH3-V1-004 | ACCEPT | CSS admitted rows are not silently treated as ordinary unimplemented work. T-P1 preserves the current REDRESS/RESULTS fact that CSS L4 reached ADMITTED/AUDIT-SUSTAINED under SK-V14 W8R, but correctly scopes SK-V15's use of those rows as audit-demoted, not as independent V1 evidence. The unimplemented item is the CSS Value API, not the historical REDRESS row. | REDRESS-215 W8R moves all 24 CSS rows to `AUDIT-SUSTAINED` / `ADMITTED` at `skinny/REDRESS.md:5314-5344`; RESULTS records CSS 24/24 at `skinny/RESULTS.md:139-149`; SK-V15 reopens/audit-demotes CSS at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:57-68`; 1D preserves this split at `1D-skinny-lessons.md:98-101` and `:143-148`; 1E calls CSS over-stated, not absent, at `restart/audit/totality/p1/1E-locks-evidence.md:89`, `:103`, and `:150`. | None. |
| CH3-V1-005 | REVISE | 1D identifies the wave-graph cycle only at pattern level, not as the explicit REDRESS pre-block list required by SK-V15 CH3. 1F carries the exact REDRESS-183/184/209..212/213 list, but CH3's pass contract names 1D and 1E as the pre-block owners. | 1D `C-6` says CSS retirement is coupled to provider proof at `1D-skinny-lessons.md:148`, and 1D records the new addenda at `:156`, but it does not name REDRESS-183, 184, 209..212, or 213. 1F does name them at `1F-coherence-scan.md:109`. REDRESS precedent: `skinny/REDRESS.md:5090-5101`, `:5103-5118`, `:5171-5293`. | Add a 1D pre-block row/table naming REDRESS-183, 184, 209..212, and 213, with the NEW-CH3-V5-01 columns: artefact, delete/retire wave, rebuild provider wave, proof command, and evidence that the provider lands no later than deletion. |
| CH3-V1-006 | REVISE | 1E's LAC ledger covers broadcast admission, gate exclusions, Pattern H provenance, and SIMD traceability, but it lacks a dedicated amendment candidate for delete/retire-before-rebuild cycles. That leaves NEW-CH3-V5-01 as a 1F coherence finding instead of a lock/gate candidate. | 1E LAC table at `1E-locks-evidence.md:122-135` has LAC-1E-V1-07 broadcast detection, LAC-1E-V1-11 gate-exclusion reporting, LAC-1E-V1-12 Pattern H generated headers, and LAC-1E-V1-14 SIMD traceability; none binds the REDRESS-183/184/209..213 cycle. SK-V15 addendum requires it at `SYNTHESIS.md:102-106`; Alpha-F repeats the dependency-table gate at `restart/skinny/tranches/sk-v15/research/alpha/alpha-F-contract-draft.md:64-69`. | Add a 1E LAC for "delete/retire wave must be no later than rebuild-provider proof" or explicitly route it to a named lock/gate amendment. |
| CH3-V1-007 | REVISE | 1A/1B/1C mention a shared typed event cursor and EventTape gaps without local REDRESS pre-block cross-references. Because old EventCursor/structural-stream routes are rejected, these rows need an inline fence saying future work is a lowering/runtime abstraction only, not retained parser sidecar revival. | 1A `1A-SUB-012` / G1 at `1A-substrate-evidence.md:68` and `:96`; 1B EventTape rows at `1B-codegen-evidence.md:57` and `:83`; 1C EventTape rows at `1C-runtime-evidence.md:60`, `:98`, and `:118`; 1D/1F provide the global fence at `1D-skinny-lessons.md:103`, `:135`, and `1F-coherence-scan.md:101`. REDRESS rejected structural-stream driver at `skinny/REDRESS.md:6176-6211`. | Add local cross-cites in 1A/1B/1C to the retained-sidecar pre-block so T-P2 does not rediscover EventCursor/structural-stream as a fresh route. |
| CH3-V1-008 | ACCEPT | The FNV closed-enum admitted-row caveat is correctly catalogued. 1D and 1F treat W11L/W11N/W11O as admitted JSON residual evidence with bench-only quarantine, not as unimplemented rows and not as production runtime authority. | 1D row at `1D-skinny-lessons.md:95`; 1D digest row `:120`; 1D gap row `:170`; 1F pre-block row at `1F-coherence-scan.md:75`; SK-V15 close condition at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:48`. | None. |

## Verdict

Disposition counts: ACCEPT 5, REVISE 3, REJECT 0.

CH3 verdict: REVISE.

No immediate T-P1 inventory reopens a rejected REDRESS route, and the admitted
JSON/CSS REDRESS rows are not silently catalogued as ordinary unimplemented
work. The required V2 fold is pre-block propagation: carry the NEW-CH3-V5-01
wave-graph cycle precedent explicitly into 1D/1E, and add local REDRESS fences
where 1A/1B/1C mention EventTape or typed event cursor work.
