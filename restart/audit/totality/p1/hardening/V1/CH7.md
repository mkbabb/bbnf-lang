# SK-V15 T-P1 V1 CH7 - OVERFIT-PRUNE / GATE-EXCLUSION

Verdict: REVISE.

## Scope

CH7 applies `NEW-CH7-V5-03`: Lock 14 / Lock 16 gates must scan and report their own exclusion lists (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:100`-`110`). For T-P1 this also means stale auxiliary inventories cannot sit beside the current V1 packet and silently define the scan universe.

## Findings

### CH7-T-P1-001 - Stale 1F auxiliary files remain in the pass root

`1F-coherence-scan.md` is current SK-V15 V1 and carries the gate-exclusion finding as `COH-004` and `COH-005` (`restart/audit/totality/p1/1F-coherence-scan.md:1`-`5`, `:69`-`:70`). But `1F-anti-pattern.md` still declares `cycle: V6`, audits SK-V14 surfaces, and carries SK-V14 S-P0 inputs rather than the SK-V15 PASS-IMPL V1 state (`restart/audit/totality/p1/1F-anti-pattern.md:1`-`19`). `1F-past-corpora.md` likewise remains a prior auxiliary ledger while T-P1 V1 now uses `1F-coherence-scan.md` as the committed SK-V15 coherence packet.

Required fold: either regenerate both auxiliary files as SK-V15 V2 1F outputs with current frontmatter and current scan roots, or explicitly demote/archive them so CH1/CH2/CH7 do not treat them as live V1 inventory surfaces.

### CH7-T-P1-002 - Gate-exclusion candidate exists, but the fold target must name scan roots and exclusions

1E correctly surfaces `LAC-1E-V1-11`, requiring every Lock 14/16 grep gate to scan and report its own exclusions (`restart/audit/totality/p1/1E-locks-evidence.md:120`-`135`). 1F also identifies that the live Lock 14 roots omit leak-bearing codegen roots and that the forbidden token universe remains too narrow (`restart/audit/totality/p1/1F-coherence-scan.md:69`-`70`). That is sufficient excavation evidence, but V2 must make the scan-root/exclusion report shape explicit for downstream T-P2/T-P3: target gate, included roots, excluded roots, reason for exclusion, and proof that the gate scans its own exclusion list.

Required fold: add a small CH7 carrier table to 1E or 1F V2 tying `COH-004`, `COH-005`, and `LAC-1E-V1-11` together.

### CH7-T-P1-003 - EventTape / typed-event cursor rows need REDRESS fences before they become wave candidates

1A records typed event cursor and EventTape gaps (`restart/audit/totality/p1/1A-substrate-evidence.md:68`-`69`, `:84`-`:100`). 1B records EventTape selector/lowerer gaps (`restart/audit/totality/p1/1B-codegen-evidence.md:43`-`45`, `:56`-`:59`, `:83`-`:84`). These are valid totality divergences, but they are adjacent to historical EventCursor / sidecar-prepass rejections; V2 must attach the same REDRESS fence CH3 identified so the rows cannot be misread as permission to revive a retained sidecar.

Required fold: where 1A/1B/1C mention EventTape, typed event cursor, or event witness work, add a local note that any implementation candidate must remain within Lock 1 and not reopen EventCursor sidecars or retained scanner/class streams.

## ACCEPTED Surfaces

- The current core 1E packet does not hide the Lock 14 / Lock 16 gate problem; it proposes a candidate amendment with concrete evidence (`restart/audit/totality/p1/1E-locks-evidence.md:94`-`97`, `:128`-`:135`).
- The current core 1F packet names SK-V15 gate-exclusion drift directly (`restart/audit/totality/p1/1F-coherence-scan.md:69`-`70`).
- 1A and 1B are evidence inventories, not implementation plans; CH7's REVISE is about adding fences and current auxiliary status, not rejecting the divergences they catalogued.

## Fold Directives

| id | target | required V2 fold |
|---|---|---|
| CH7-FOLD-001 | `1F-anti-pattern.md`, `1F-past-corpora.md`, `1F-coherence-scan.md` | Regenerate or explicitly demote the two stale auxiliary files; the live 1F packet must state which 1F files are authoritative for SK-V15 V2. |
| CH7-FOLD-002 | `1E-locks-evidence.md` or `1F-coherence-scan.md` | Add a CH7 scan-root/exclusion carrier table for Lock 14/16 gates. |
| CH7-FOLD-003 | `1A-substrate-evidence.md`, `1B-codegen-evidence.md`, `1C-runtime-evidence.md` | Add REDRESS / Lock 1 fences around EventTape, typed event cursor, and event witness rows. |
