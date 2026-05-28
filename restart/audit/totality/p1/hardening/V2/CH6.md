---
agent: CH6
pass: T-P1-excavation
cycle: V2
lens: ANTI-PAPER-CLOSE
disposition: REVISE
inventory_fold_commit: 2fcbc1dc8
generated_at: 2026-05-28
inputs_audited:
  - restart/prompts/ORCHESTRATOR.md section 3W
  - restart/prompts/ORCHESTRATOR.md section 3Z
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md
  - restart/audit/totality/p1/T-P1-DISPATCH-CONTEXT.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH6 Anti-Paper-Close - SK-V15 T-P1 V2

## Verdict

REVISE.

The V2 packet is much more honest than V1: generated headers are treated as
file-state evidence, not regeneration provenance; CSS L4 is audit-demoted while
JSON remains the guard baseline; and UNKNOWN rows mostly carry concrete
verify_action routes. The remaining CH6 problem is bounded but material:
some rows still use closure words (`implemented`, `honoured`) for surfaces whose
own notes or sibling rows say the evidence is partial.

## Findings

| disposition | target | evidence | required fold |
|---|---|---|---|
| REVISE | `1E-locks-evidence.md` L01 | L01 says `honoured` for "Tape substrate / no parallel substrate" at `restart/audit/totality/p1/1E-locks-evidence.md:85`, but the note scopes that to "JSON/tape shape" and says CSS fact-stream schema still needs an amendment candidate. The same file also catalogues CSS `FactStream` as a string-returning runtime path under L01/L08/L14 at `:108`. | Change L01 to `partially honoured / JSON-tape-only` or equivalent. Do not count the whole Lock 1 surface as honoured until CSS fact-stream schema and substrate-target wording are disposed. |
| REVISE | `1A-substrate-evidence.md` 1A-SUB-001 | 1A-SUB-001 says `implemented` for runtime ownership of tape, direct-to-struct support, generated grammar modules, visitors, and document views at `restart/audit/totality/p1/1A-substrate-evidence.md:61`, while the note admits CSS fact-stream modules do not expose the full retained-view roster. The same inventory later marks the generated per-grammar runtime roster unimplemented at `:74`. | Scope the row to "implemented for JSON/examples" or downgrade to partial. The CSS retained-view gap must not be hidden behind the broad runtime-row closure word. |
| ACCEPT | Generated-header vs provenance proof | 1A keeps generated-runtime provenance UNKNOWN at `restart/audit/totality/p1/1A-substrate-evidence.md:82` and provides a manifest/roster verify_action at `:164`. 1C says headers are present but provenance is unverified at `restart/audit/totality/p1/1C-runtime-evidence.md:57`, repeats that generated headers are file-state evidence only at `:98`, and records that no regen round-trip was run at `:130`. | Preserve this wording. No provenance close until a regen/check transcript or manifest proof is cited. |
| ACCEPT | JSON guard vs CSS invalidated status | 1D separates JSON guard proof from CSS audit demotion at `restart/audit/totality/p1/1D-skinny-lessons.md:72`-`:82`, marks CSS admits disproved/audit-demoted at `:102`-`:105`, and keeps CSS rebuild gates in UNKNOWN rows at `:178`-`:185`. 1F repeats the split at `restart/audit/totality/p1/1F-coherence-scan.md:63`, `:70`, `:77`, and `:79`. | Preserve the split. JSON guard evidence must not be reused as CSS or generalization closure. |
| ACCEPT | UNKNOWN routing | 1A UNKNOWN rows carry commands or explicit disposition actions at `restart/audit/totality/p1/1A-substrate-evidence.md:162`-`:166`; 1B carries verify_action rows at `restart/audit/totality/p1/1B-codegen-evidence.md:115`-`:116`; 1C at `restart/audit/totality/p1/1C-runtime-evidence.md:136`-`:137`; 1D at `restart/audit/totality/p1/1D-skinny-lessons.md:178`-`:198`; 1E at `restart/audit/totality/p1/1E-locks-evidence.md:172`-`:176`; and 1F at `restart/audit/totality/p1/1F-coherence-scan.md:151`-`:153`. | Keep UNKNOWNs routed. Policy choices may stay as T-P3/T-P3-style dispositions only when they are not counted as source-fact closure. |
| ACCEPT | Superseded 1F auxiliaries | `1F-anti-pattern.md` and `1F-past-corpora.md` are explicitly marked superseded historical auxiliaries and point to `1F-coherence-scan.md` as authoritative live inventory. | No CH6 issue as long as later packets do not cite those auxiliary files for current live LOC, symbol, or provider claims. |

## Closure Note

No REJECT. The packet has the right SK-V15 baseline and the right non-close
routes. V3 should only need wording/count cleanup for the partial Lock 1 and
runtime-roster rows above.
