# T-P2 V4 CH5 Hidden Coupling / Substrate Ownership

Pass: T-P2 Research.
Cycle: V4.
Lens: CH5 hidden coupling / Lock 1.
Date: 2026-05-21.
Agent: CH5.

## Verdict

ACCEPT.

V4 preserves the V3 CH5 acceptance. The V4 executable delta does not introduce
a new substrate surface; it either binds rows back to the V3 executable ledger
or marks them non-shortlist until strict parity, same-wave consumers, and row
gates exist. The new REDRESS-slice table is routing and ownership metadata over
existing candidate rows, not an independent admission path. It keeps reopen
slices disjoint by owner and row gate, and it blocks the high-risk routes that
would otherwise imply retained sidecars, public substrate APIs, new BIR /
`BackendShape` variants, or parser-owned runtime streams.

## Evidence

| id | disposition | evidence |
|---|---|---|
| CH5-V4-01 | ACCEPT | The V4 addendum explicitly supplements the V2/V3 Lock 1 and REDRESS contracts without weakening them, then scopes its changes to counted-source repair, executable-ledger deltas, non-shortlist blockers, and REDRESS-slice ownership (`T-P2-V4-FOLD-ADDENDUM.md:11`-`22`). The V3 consolidated finding already accepted the substrate fields as blocking retained sidecars, public substrates, new BIR / `BackendShape`, and parser-owned cursor streams (`HARDENING-T-P2-V3-CONSOLIDATED.md:30`). |
| CH5-V4-02 | ACCEPT | The V4 non-shortlist rows do not admit sidecars. `escape_mask_64`, `tbl_tbx_escape_decode_batch`, `digit_run_accumulate_udot`, `pmull_cssc_structural_union_emit64`, `string_context_64`, and `cache_hint_prefetch_store` are either tied to generated JSON/CSS consumers or held non-shortlist until missing strict commands, combined matrices, exact consumers, grammar policy, or hot callers exist (`T-P2-V4-FOLD-ADDENDUM.md:43`-`58`). |
| CH5-V4-03 | ACCEPT | The V3 ledger remains the substrate authority for the V4 delta. Every shared 2B/2E/2F candidate row carries `substrate_target`, `retention_lifetime`, and `policy_owner`, and the dangerous routes are fenced as `local_temp_only`, `existing_tape`, `direct_sink`, or `admitted_fact_output` rather than retained public substrates (`T-P2-V3-FOLD-ADDENDUM.md:86`-`101`). |
| CH5-V4-04 | ACCEPT | The REDRESS-slice table is not a hidden dispatch license. It says a slice reaches S-P3 only if its blocker is cleared; otherwise it remains research evidence (`T-P2-V4-FOLD-ADDENDUM.md:60`-`64`). The slice rows then bind JSON direct, string/unicode, number, PMULL/CSSC/union, source-present, CSS run-skip, and parse-that reopen paths to owners, first consumers, row gates, rollback, abrogate thresholds, and blockers (`T-P2-V4-FOLD-ADDENDUM.md:66`-`79`). |
| CH5-V4-05 | ACCEPT | The highest coupling-risk rows are blocked at the right boundary. `RS-UNION-PMULL-CSSC` and source-present PMULL/CSSC rows have no V4 consumer and remain non-shortlist or candidate-only behind material-differential gates (`T-P2-V4-FOLD-ADDENDUM.md:74`-`75`). `RS-SOURCE-PRESENT-cache-hints` is delete-or-wire only, with no support-only substrate admission (`T-P2-V4-FOLD-ADDENDUM.md:77`). |
| CH5-V4-06 | ACCEPT | Parse-that import ownership stays disjoint. The V4 parse-that slice abrogates on retained runtime stream and is candidate-only after snapshot/license/HIR mapping closes without new BIR/API (`T-P2-V4-FOLD-ADDENDUM.md:79`). 2F separately requires HIR/regex facts to map into current `BackendExpr` or an approved fact side table, forbids runtime parser-owned DFA state, persistent mask/class/cursor streams, sidecars, scanner caches, and `UnionTape`-like substrates, and requires runtime scanner outputs to declare the three Lock 1 fields (`2F-parse-that-gaps.md:162`-`186`). |
| CH5-V4-07 | ACCEPT | `BackendShape` and decision-engine ownership remain inside the existing surface. 2D requires `backend_expr_language` to pass JSON/CSS equality with no new BIR or `BackendShape` variant without G-Omega (`2D-cost-model.md:73`), and its substrate-kind rules reject retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, and second tapes (`2D-cost-model.md:92`-`108`). |
| CH5-V4-08 | ACCEPT | The owner summaries in 2B, 2E, and 2F do not create competing authority. Each explicitly says the V3/V4 addenda are authoritative for normalized state, blockers, non-shortlist rows, and REDRESS-slice ownership (`2B-primitive-vocabulary.md:143`-`149`; `2E-host-arch-esoterica.md:142`-`147`; `2F-parse-that-gaps.md:190`-`196`). |
| CH5-V4-09 | ACCEPT | The remaining generality and fact-stream boundaries still point to generated or caller-owned surfaces. 2A keeps structural masks `local_temp_only` unless consumed into `existing_tape`, `direct_sink`, or `admitted_fact_output` (`2A-sota-landscape.md:58`-`63`); 2C keeps fact streams as admitted output-plane contracts and rejects hidden retained sidecars (`2C-grammar-neutrality.md:128`-`145`). |

## Required Repairs

None for CH5 before V4 consolidation.

T-P3 must carry the V3 ledger fields plus the V4 slice blockers forward as hard
manifest inputs. Any downstream wave that retains mask/class/cursor streams,
adds parser-owned runtime state, exposes a public substrate API, introduces a
second tape or `UnionTape`, or adds a new BIR / `BackendShape` variant remains
REVISE or REJECT unless the relevant lock is explicitly amended.

## Evidence Read

- `restart/prompts/totality/PASS-2-RESEARCH.md`
- `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`
