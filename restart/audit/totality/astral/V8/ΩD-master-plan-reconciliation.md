# Omega-D Master-Plan Reconciliation - Pass Omega V8 W5B-FRONTENDR

Date: 2026-05-26.
Scope: MASTER-PLAN and SK-V14 SPEC wave-graph reconciliation after REDRESS-212.
Disposition: ACCEPT-WITH-PATCH.

## Delta

`W5B-FRONTEND` is rejected under the current one-wave SPEC cap shape. V7's
semantic ordering remains correct, but REDRESS-212 proves that the frontend
closure cannot honestly fit as one capped W5B wave while still closing the full
import/layout/discard/pretty/span/projection/request-consumer surface and
unblocking W5C-GEN.

The amended graph is:

```text
W5A
  -> W5B.0 LOCK14-GATE
  -> W5B.1 IMPORT-CLOSURE
  -> W5B.2 LAYOUT-DISCARD
  -> W5B.3 PRETTY-SPAN-PROJECTION
  -> W5B.4 REQUEST-CONSUMER
  -> W5C-GEN
  -> W5D-DELETE
  -> W6
  -> W7
  -> W8/W9/W10
```

`W5B-FRONTEND` closes only after W5B.0 through W5B.4 all admit. W5C-GEN remains
blocked until that aggregate W5B close.

## Proposed Master/SPEC Amendment

- Supersede V7 one-shot `W5B-FRONTEND PRUNE-3B` wording in
  `restart/MASTER-PLAN.md` Section 13.3 and
  `restart/skinny/tranches/sk-v14/SPEC.md` Section 2 / Section 8B with formal
  W5B sub-waves.
- Preserve V7 scope ownership: W5B owns generic BBNF grammar-source
  frontend/import/IR closure; W5C-GEN owns the provider-free runtime generator
  body; W5D-DELETE owns provider/template deletion and Lock 14 baseline close.
- Replace the W5B-FRONTEND row with an aggregate row naming W5B.0 through
  W5B.4. The row remains PRUNE-3B and remains a non-admit capability wave.
- Keep W5C-GEN Section 8C scope unchanged, but change its entry gate from
  one-shot W5B-FRONTEND close to aggregate W5B-FRONTEND close after W5B.4.
- Keep W5D-DELETE blocked until W5C-GEN close, W6 blocked until W5D-DELETE
  close, and W7 plus W8/W9/W10 blocked by the full PRUNE chain.

| Wave | Scope | Entry gate | Exit gate |
|---|---|---|---|
| W5B.0 LOCK14-GATE | Lock 14 W5B-FRONTEND owner-path roster, parent-diff routing, modified-provider/template rejection tests, all-template guard, and generic owner-path leak census. No grammar/codegen/xtask frontend source edits. | W5A admitted; REDRESS-211 closed; V7 CRUD applied; REDRESS-212 accepted as V8 amendment input. | Lock 14 routing admits only W5B paths; W5C/W5D subjects reject; provider/template modification tests reject; all `_templates` paths are guarded; owner-path leak census passes. |
| W5B.1 IMPORT-CLOSURE | Request-local import DAG resolution from request source maps, stable source hashing, missing-import fail-closed behavior, and import-cycle fail-closed behavior. | W5B.0 admitted. | Exact grammar tests prove import graph resolution, missing-import rejection, and import-cycle rejection; no public syntax, provider/template, or generator-body change. |
| W5B.2 LAYOUT-DISCARD | Lower `@ws`, `?w`, `>>`, and `<<` into request-local frontend facts without public syntax revival. | W5B.1 admitted. | Exact grammar tests prove layout/discard lowering and public-retirement/fail-closed behavior for those constructs; no provider/template topology change. |
| W5B.3 PRETTY-SPAN-PROJECTION | Lower `@pretty`, `@{...}` span capture, `->` projection metadata, and typed projections into request-local facts. | W5B.2 admitted. | Exact grammar tests prove lowering and malformed-input rejection for pretty/span/projection constructs; no new public directive, no new BIR/BackendShape/substrate variant. |
| W5B.4 REQUEST-CONSUMER | `emit_runtime_from_request` consumes the frontend closure; JSON/Sheets/BBNF proof carry, `regen-css`, seven CSS companions, provider/template topology, and exact maintain evidence close W5B-FRONTEND. | W5B.3 admitted. | Same-commit consumer evidence closes W5B-FRONTEND: frontend closure consumed before provider rendering; JSON unchanged-output proof holds; Sheets/BBNF-self proof holds; `regen-css` and all CSS companions pass; provider/template counts unchanged. |

## CH1 Exactness Fold

V8 must carry the V2 CH1 exactness blockers into SPEC Section 8B, not leave
them as broad "exact tests" prose.

Every W5B lowering row must have the following columns:

| Construct | Owner file/type | Target representation | Exact positive test | Exact fail-closed test |
|---|---|---|---|---|
| `@import` | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `imports` DAG keyed by request source path and stable source hash | `w5b_frontend_import_graph_resolves_request_sources` | `w5b_frontend_missing_import_fails_closed`; `w5b_frontend_import_cycle_fails_closed` |
| `@ws` | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `layout.whitespace_directive` request fact | `w5b_frontend_layout_contract_lowers_to_request_facts` | `w5b_frontend_public_ws_remains_retired` |
| `?w` | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `layout.whitespace_modifier` request fact attached to source span | `w5b_frontend_layout_contract_lowers_to_request_facts` | `w5b_frontend_malformed_whitespace_modifier_fails_closed` |
| `>>` / `<<` | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `discard_operator` request facts attached to source spans | `w5b_frontend_discard_operators_lower_to_request_facts` | `w5b_frontend_malformed_discard_operator_fails_closed` |
| `@pretty` | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `pretty_directive` request fact | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | `w5b_frontend_unknown_pretty_payload_fails_closed` |
| `@{...}` | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `host_capture` request span fact | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | `w5b_frontend_host_capture_unterminated_fails_closed` |
| `->` projection | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `projection` request fact preserving raw target text | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | `w5b_frontend_projection_malformed_target_fails_closed` |
| typed projection | `skinny/crates/grammar/src/lib.rs::FrontendClosure` | `typed_projection` request fact preserving raw type text | `w5b_frontend_pretty_span_projection_lower_to_request_facts` | `w5b_frontend_typed_projection_malformed_type_fails_closed` |

W5B.0 Lock 14 exact tests:

- `w5b_lock14_frontend_owner_paths_admit`
- `w5b_lock14_frontend_rejects_w5c_subject`
- `w5b_lock14_frontend_rejects_w5d_subject`
- `w5b_lock14_frontend_rejects_modified_provider`
- `w5b_lock14_frontend_rejects_modified_template`
- `w5b_lock14_frontend_all_templates_guard_counts_8`
- `w5b_lock14_frontend_allows_grammar_provider_exception`
- `w5b_lock14_frontend_generic_owner_leak_census`

Every exact W5B test command must write to a dedicated
`/tmp/skv14-w5b-<test-name>.log`, and each log must be checked with:

```sh
rg "test result: ok\\. [1-9][0-9]* passed" /tmp/skv14-w5b-<test-name>.log
```

Wildcard aggregate log greps are rejected. Redress report edits and reject-only
`skinny/REDRESS.md` edits count in W5B LOC accounting whenever touched.

## Caps And Close Semantics

- Each W5B.N sub-wave carries the REDRESS-212 dispatch-hard-cap discipline:
  HARD CAP 30 min; at 27 min commit/write safe evidence; at 30 min halt.
- Aggregate W5B-FRONTEND implementation/redress cap is <=150 min across W5B.0
  through W5B.4.
- W5B-FRONTEND remains within the existing V7 C-1 part-A source/test envelope
  unless CHALLENGE authorizes narrower per-sub-wave LOC partitions. Generated
  output remains uncounted only when named, diff-audited, and included in the
  revert slice.
- No W5B sub-wave may borrow time, source/test LOC, verification debt, or
  same-wave consumer evidence from W5C-GEN, W5D-DELETE, W6, or any new-admit
  wave.
- W5B.0 through W5B.3 are not W5B close points. They may admit only as internal
  W5B progress. W5B-FRONTEND closes only at W5B.4 after all same-commit
  consumer gates pass.

## W5B Maintain Gate

Replace W5B-FRONTEND's V7 `+/-1.0%` full-table maintain gate with exact no-diff
maintain for this non-admit capability wave:

- `skinny/RESULTS.md` is byte-identical to pre-redress HEAD.
- `restart/skinny/ROLLING-SOTA-DELTA.md` is byte-identical to pre-redress HEAD.
- Generated runtime outputs are byte-identical unless regenerated by the active
  request path, in which case they must be named and diff-audited.
- Protected grammar/source inputs remain byte-identical unless the active W5B.N
  sub-wave explicitly owns them.
- `cargo xtask gate-json --skv14-existing-results-capture` remains
  schema/freshness evidence, not row-admission or benchmark-maintain evidence.

If Omega rejects exact no-diff maintain, W5B.4 must instead require fresh
SK-V14-open full-table maintain evidence. A prose substitution is not an
acceptable close.

## Required Folds

- `restart/MASTER-PLAN.md` Section 13.3: add Pass Omega V8 W5B-FRONTENDR
  wording, replace the one-shot W5B-FRONTEND row with aggregate W5B.0..W5B.4,
  and keep W5C-GEN blocked until aggregate W5B close.
- `restart/skinny/tranches/sk-v14/SPEC.md` Section 2: replace the one-slot
  W5B-FRONTEND cap with a formal aggregate sub-wave declaration, <=30 min per
  W5B.N, and <=150 min aggregate.
- `restart/skinny/tranches/sk-v14/SPEC.md` Section 8B: replace one-shot tasks,
  exit gate, rerun/maintain wording, and revert protocol with W5B.0 through
  W5B.4 entry/exit gates and final close semantics.
- `restart/skinny/tranches/sk-v14/SPEC.md` Section 8C: change the W5C-GEN
  entry gate to aggregate W5B-FRONTEND close; do not change W5C-GEN ownership.
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md`,
  `ORCHESTRATOR-PROMPT.md`, `DISPATCH-PROMPT.md`, and tranche handoff surfaces:
  dispatch W5B.0 first and forbid treating W5B.0 through W5B.3 as W5B close.

## Preserved Blocks

- No provider/template deletion in W5B.
- No provider-free generator body replacement in W5B.
- No public `@ws` revival.
- No new public directive set, BIR variant, BackendShape variant, substrate
  surface, or lock.
- No grammar-name branches in generic crates.
- No W5C-GEN dispatch until aggregate W5B-FRONTEND close.
