# T-P2 V3 CH4 Cost / Executability

Pass: T-P2 Research.
Cycle: V3.
Lens: CH4 COST.
Date: 2026-05-21.

## Verdict

REVISE.

V3 materially improves the V2 cost/executability posture. It centralizes the
2B/2E/2F admission ledger, normalizes `admissibility_state` away from
disposition labels, and replaces V2's elastic e-graph / CSP / stale-cost /
generated-LOC gates with numeric or dereferenceable caps. It does not yet pass
CH4 because some supposedly executable ledger cells still defer the exact
checkasm/parity command or production consumer to S-P3, and REDRESS reopen
ownership remains candidate-family-local rather than slice-local.

## Findings

1. The shared ledger exists, but not every row has row-local executable values.

   `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:84-101` creates one
   owner ledger for 2B, 2E, and 2F with the required CH4 columns:
   `scalar_reference`, `checkasm_or_parity_command`,
   `same_wave_consumer_path`, expected gate, LOC budget, risk, rollback,
   abrogate threshold, state, blocker, substrate target, retention lifetime,
   and policy owner. Strong rows are now copyable into a wave plan; for example
   `ascii_set_member64_css_delimiter` names a scalar reference, strict cargo
   command, CSS generated path, row gate, 80-140 LOC budget, rollback, and abort
   criteria (`T-P2-V3-FOLD-ADDENDUM.md:91`).

   Several rows remain elastic. `tbl_tbx_escape_decode_batch` says the strict
   parity command is "to be supplied by S-P3 if selected"
   (`T-P2-V3-FOLD-ADDENDUM.md:93`). `escape_mask_64`, `digit_run_accumulate_udot`,
   `pmull_cssc_structural_union_emit64`, and `string_context_64` name generic
   JSON/CSS row families or "row-local" consumers rather than a concrete
   first consumer path (`T-P2-V3-FOLD-ADDENDUM.md:92-96`). `cache_hint_prefetch_store`
   uses "named store/prefetch hot caller only" instead of either naming that
   caller or marking the row non-shortlist until a caller exists
   (`T-P2-V3-FOLD-ADDENDUM.md:97`). These are valid blockers, but they are not
   fully executable ledger cells.

2. The admissibility vocabulary is normalized in the authoritative source.

   V3 restricts `admissibility_state` to `source_backed`, `scalar_backed`,
   `checkasm_backed`, `micro_proven`, `production_wired`, `row_admitted`,
   `measured_rejected`, and `architectural_block`, and moves labels such as
   `conditional`, `inventory`, `partial`, `ADMITTED-EVIDENCE`, and
   `NOT-VALIDATED` into `disposition_or_blocker`
   (`T-P2-V3-FOLD-ADDENDUM.md:56-82`). The shared ledger follows that split
   (`T-P2-V3-FOLD-ADDENDUM.md:89-101`).

   2B, 2E, and 2F explicitly make the V3 addendum authoritative for normalized
   state, LOC/risk/rollback, and blocker values
   (`2B-primitive-vocabulary.md:141-146`,
   `2E-host-arch-esoterica.md:134-137`,
   `2F-parse-that-gaps.md:188-192`). Residual local summaries still use prose
   "state" language, but CH4 treats them as non-authoritative summaries rather
   than ledger state.

3. Decision-engine abrogate caps are now CH4-acceptable.

   The V3 addendum caps e-graph saturation at `<= 50_000` e-nodes,
   `<= 10_000` e-classes, `<= 30` iterations, and `<= 512 MiB` resident memory
   per grammar; CSP solve at `<= 1s` per grammar; stale/static fallback at
   `<= 30%` per grammar and output plane; generated LOC growth at the candidate
   ledger `loc_budget` upper bound or a stricter named SPEC budget; and any row
   regression or parity/equality failure as reject
   (`T-P2-V3-FOLD-ADDENDUM.md:103-115`). 2D repeats matching thresholds and
   gives resolver-row LOC budgets (`2D-cost-model.md:82-90`,
   `2D-cost-model.md:139-149`). This resolves the V2 CH4 cap defect.

4. REDRESS slice ownership is improved, but still not mechanically executable.

   V3 assigns candidate owners in the shared ledger (`T-P2-V3-FOLD-ADDENDUM.md:91-101`),
   and the dossiers keep the prior REDRESS route warnings: 2B requires a
   PMULL/CSSC/union material-differential checklist before shortlist use
   (`2B-primitive-vocabulary.md:240-254`), 2E says source-present primitives
   must wire, delete, stay scalar-delegate, or record architectural block
   (`2E-host-arch-esoterica.md:160-172`), and 2F blocks parse-that imports
   until snapshot/license/HIR mapping and same-wave consumers close
   (`2F-parse-that-gaps.md:171-181`).

   What is still missing is the bridge from a REDRESS row or family to a
   concrete owner row. The V2 REDRESS-119 reopen matrix and the PMULL/CSSC
   material-differential checklist remain binding, but V3 does not provide a
   table that maps each reopened direct row, union/structural slice,
   source-present primitive, string/digit route, or parse-that import slice to
   `candidate_id`, owner, first consumer path, expected gate, LOC budget,
   rollback, and abrogate threshold. Without that mapping, S-P3 still has to
   infer ownership from broad candidate families.

## Required V4 Repairs

1. Replace deferred ledger cells with executable values, or mark the candidate
   non-shortlist until the cell exists. At minimum, provide an exact strict
   checkasm/parity command for `tbl_tbx_escape_decode_batch`, and concrete first
   consumer paths or explicit non-admit blockers for `escape_mask_64`,
   `digit_run_accumulate_udot`, `pmull_cssc_structural_union_emit64`,
   `string_context_64`, and `cache_hint_prefetch_store`.

2. Add a REDRESS-slice ownership table with:
   `redress_slice_id | prior REDRESS row(s) | candidate_id | owner |
   first_consumer_path | expected_row_gate | loc_budget | rollback_path |
   abrogate_threshold | blocker`. Every reopened REDRESS-119 direct row,
   PMULL/CSSC/union family, source-present primitive family, string/digit route,
   and parse-that import route must resolve to one row or to an explicit
   `architectural_block`.

3. Keep `admissibility_state` reserved for the normalized enum. Rename
   non-authoritative summary columns that still carry prose state labels to
   `summary_status`, `disposition`, or `blocker` so downstream agents cannot
   consume them as ledger state.

## Evidence Checked

- `restart/prompts/totality/PASS-2-RESEARCH.md`.
- `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/2A-sota-landscape.md` through
  `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- `restart/audit/totality/p2/hardening/V2/CH4.md`.
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`.
