# T-P1 V2 CH2 Generality

Verdict: REVISE.

V2 fixes the stale 1F auxiliary problem and gives live carriers for most V1
folds, but CH2 cannot ACCEPT while the pass-layer JSON-shape leak fold is only
self-reported in 1D. `P1-1B-D9` and `P1-1B-D10` are named in 1D frontmatter and
the V2 fold table, but the actual grammar-neutral findings table still does not
cite those rows or assign a downstream receiver.

## Findings

| id | disposition | finding | evidence | required V3 fold |
|---|---|---|---|---|
| CH2-V2-001 | ACCEPT | V1 consolidated fold roster has live V2 carriers for stale 1F demotion, EventTape/typed-cursor fences, wave-graph cycle precedent, broadcast/CSS sidecar handling, gate-exclusion carriers, LOC/risk metadata, UNKNOWN verify_action rows, and SK-V15 JSON/CSS leak reconciliation. | V1 roster `HARDENING-T-P1-V1-CONSOLIDATED.md:28-45`; carriers in `1A-substrate-evidence.md:84-91`, `1C-runtime-evidence.md:94-100`, `1D-skinny-lessons.md:164-172`, `1E-locks-evidence.md:141-176`, and `1F-coherence-scan.md:85-99`. | None. Preserve these carriers. |
| CH2-V2-002 | ACCEPT | 1F auxiliary demotion is coherent. The live SK-V15 authority is `1F-coherence-scan.md`; the two auxiliary files are marked `superseded-historical-auxiliary` and deny live LOC/symbol/provider use. | `1F-coherence-scan.md:85-90`; `1F-anti-pattern.md:6-23`; `1F-past-corpora.md:6-28`. | None. Do not cite auxiliary files as live evidence. |
| CH2-V2-003 | REVISE | `P1-1B-D9` / `P1-1B-D10` propagation into 1D is incomplete. 1B correctly defines them as grammar-neutral pass-layer leaks, but 1D carries them only in metadata/fold self-report, not in the grammar-neutral findings table where CH2 required the receiver. | 1B rows at `1B-codegen-evidence.md:96-97`; 1D self-report at `1D-skinny-lessons.md:43` and `1D-skinny-lessons.md:168`; missing from 1D grammar-neutral table `1D-skinny-lessons.md:129-141`. | Add or expand a 1D grammar-neutral row that cites `P1-1B-D9` and `P1-1B-D10`, classifies them as non-JSON-specific Lock 14 failures, and names the receiver: pass-layer facts/role mining repair with Sheets or BBNF-self fixture proof. |
| CH2-V2-004 | REVISE | CH2-FOLD-003's cross-inventory Lock 14 leak map is still inferred across 1B/1C/1E/1F instead of carried as a compact owner/receiver map. This leaves receiver discipline uneven for pass-layer leaks, runtime-root leaks, Pattern H, gate exclusions, and Decision Engine grammar facts. | V1 directive `hardening/V1/CH2.md:70-73`; current carrier fragments in `1B-codegen-evidence.md:49-50`, `1C-runtime-evidence.md:84-92`, `1E-locks-evidence.md:161-167`, and `1F-coherence-scan.md:92-99`. | Add a compact Lock 14 leak map in 1F or 1C with columns: surface, leak, classification, owner inventory, downstream receiver, and proof expected. |

## Orphan-REVISE Check

| V1 REVISE / fold | V2 carrier | orphan status |
|---|---|---|
| T-P1-V2-F01 / CH2-FOLD-001 / CH2-FOLD-002 | `1F-coherence-scan.md:85-90`; auxiliary frontmatter/status rows | Clear. |
| T-P1-V2-F02 | `1A-substrate-evidence.md:17` and `1A-substrate-evidence.md:88` count `1A-SUB-022`; citation repair is outside CH2 scope. | Clear for CH2. |
| T-P1-V2-F03 | EventTape and typed-event cursor fences in `1A-substrate-evidence.md:72-73`, `1B-codegen-evidence.md:67`, `1C-runtime-evidence.md:62`, and `1C-runtime-evidence.md:98-100`. | Clear. |
| T-P1-V2-F04 | `1D-skinny-lessons.md:153`, `1E-locks-evidence.md:139`, and `1F-coherence-scan.md:71`. | Clear. |
| T-P1-V2-F05 | `1D-skinny-lessons.md:147-152`, `1D-skinny-lessons.md:171-172`, `1E-locks-evidence.md:131-132`, and `1F-coherence-scan.md:83`. | Clear. |
| T-P1-V2-F06 | `1E-locks-evidence.md:161-167` and `1F-coherence-scan.md:92-99`. | Clear. |
| T-P1-V2-F07 | 1D LOC/risk table and 1E cost/wave carrier exist at `1D-skinny-lessons.md:155-162` and `1E-locks-evidence.md:141-159`. | Clear enough for CH2; CH4 may still judge candidate-row granularity. |
| T-P1-V2-F08 | UNKNOWN / verify_action rows exist at `1E-locks-evidence.md:168-176` and `1F-coherence-scan.md:147-154`. | Clear. |
| T-P1-V2-F09 | Current leak evidence is SK-V15 scoped in `1B-codegen-evidence.md:49-50`, `1C-runtime-evidence.md:55-64`, and `1F-coherence-scan.md:63-83`. | Clear. |
| CH2-FOLD-003 | Fragmentary carriers exist, but no compact owner/receiver leak map. | Orphan risk remains. |
| CH2-FOLD-004 | `1D-skinny-lessons.md:168` self-reports the fold, but `1D-skinny-lessons.md:129-141` does not integrate `P1-1B-D9` / `P1-1B-D10`. | Orphan REVISE remains. |

Net orphan status: two CH2-specific V1 fold items remain unresolved. V2 should
not converge until 1D receives the D9/D10 grammar-neutral row and 1F or 1C
carries the cross-inventory Lock 14 owner/receiver map.
