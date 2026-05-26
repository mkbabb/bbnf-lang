# Pass Omega V8 Master-Plan / SPEC Diff

Disposition: proposed. Do not apply until G-Omega V8 authorization.

## Summary

REDRESS-212 rejects W5B-FRONTEND under the current SPEC cap shape. The amended
wave graph keeps V7's semantic ordering but formalizes W5B-FRONTEND as an
aggregate sub-wave sequence:

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

W5B-FRONTEND closes only after W5B.0 through W5B.4 admit. W5C-GEN remains
blocked until the aggregate W5B-FRONTEND close.

## MASTER-PLAN

- Supersede V7 one-shot W5B-FRONTEND wording in §13.3 with V8
  W5B-FRONTENDR.
- Replace the W5B-FRONTEND row with an aggregate row naming W5B.0 through
  W5B.4.
- Keep W5B-FRONTEND's source/test LOC envelope at <=1.0k C-1 part-A unless a
  later CHALLENGE proves the formal sub-waves require narrower per-sub-wave
  LOC partitions.
- Replace W5B-FRONTEND's single <=90 minute redress cap with five capped
  redress sub-waves: each W5B.N carries HARD CAP 30 min, at 27 min commit safe
  evidence, at 30 min halt. Aggregate W5B-FRONTEND cap <=150 min.
- Keep W5C-GEN blocked until aggregate W5B-FRONTEND close.
- Keep W5D-DELETE blocked until W5C-GEN close.
- Keep W6 blocked until W5D-DELETE close.
- Keep W7 and W8/W9/W10 blocked by the full PRUNE chain.

## SK-V14 SPEC

- Update Section 2 wave manifest:
  - Replace W5B-FRONTEND's one-slot cap with an aggregate sub-wave declaration.
  - Name W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD,
    W5B.3 PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER.
  - Record <=30 min per W5B.N sub-wave and <=150 min aggregate.
  - Preserve W5B-FRONTEND <=1.0k C-1 part-A source/test LOC unless CRUD applies
    a narrower CHALLENGE-authorized partition.
- Update Section 8B:
  - W5B.0 LOCK14-GATE: owner-path roster, parent-diff routing,
    modified-provider/template rejection tests, all-template guard, and generic
    owner-path leak census. No grammar/codegen/xtask frontend source edits.
  - W5B.1 IMPORT-CLOSURE: request-local import DAG resolution, source-map
    closure, missing-import fail-closed test, and import-cycle fail-closed test.
  - W5B.2 LAYOUT-DISCARD: `@ws`, `?w`, `>>`, and `<<` lower into request-local
    facts; public syntax remains retired.
  - W5B.3 PRETTY-SPAN-PROJECTION: `@pretty`, `@{...}` span capture, `->`
    projection metadata, and typed projections lower into request-local facts.
  - W5B.4 REQUEST-CONSUMER: `emit_runtime_from_request` consumes the frontend
    closure; JSON unchanged-output proof, Sheets/BBNF-self proof, `regen-css`,
    seven CSS companions, provider/template topology checks, and final maintain
    proof close W5B-FRONTEND.
- Add the CH1 exactness table to Section 8B. Every construct row must carry
  owner file/type, target representation, exact positive test, and exact
  fail-closed test. Prose fail-closed cells are not sufficient:

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

- Add exact W5B.0 Lock 14 tests:
  - `w5b_lock14_frontend_owner_paths_admit`
  - `w5b_lock14_frontend_rejects_w5c_subject`
  - `w5b_lock14_frontend_rejects_w5d_subject`
  - `w5b_lock14_frontend_rejects_modified_provider`
  - `w5b_lock14_frontend_rejects_modified_template`
  - `w5b_lock14_frontend_all_templates_guard_counts_8`
  - `w5b_lock14_frontend_allows_grammar_provider_exception`
  - `w5b_lock14_frontend_generic_owner_leak_census`
- Add the CH1 nonzero-proof rule: every exact W5B test command must tee to a
  dedicated `/tmp/skv14-w5b-<test-name>.log` and must be paired with a
  dedicated `rg "test result: ok\\. [1-9][0-9]* passed"
  /tmp/skv14-w5b-<test-name>.log`. Wildcard aggregate log greps do not satisfy
  W5B close evidence.
- Add the CH1 LOC-accounting rule: any touched redress report and reject-only
  `skinny/REDRESS.md` edits count in W5B LOC accounting alongside source/test
  edits. Generated output remains uncounted only when named, diff-audited, and
  included in the revert slice.
- Replace W5B's full-table maintain gate with exact no-diff for this non-admit
  capability wave:
  - `skinny/RESULTS.md` byte-identical to pre-redress HEAD.
  - `restart/skinny/ROLLING-SOTA-DELTA.md` byte-identical to pre-redress HEAD.
  - generated runtime outputs byte-identical unless regenerated by the active
    request path and diff-audited.
  - `cargo xtask gate-json --skv14-existing-results-capture` remains
    schema/freshness evidence, not row-admission evidence.
- Preserve V7 prohibitions:
  - no provider/template deletion in W5B.
  - no provider-free generator body replacement in W5B.
  - no public `@ws` revival.
  - no grammar-name branches in generic crates.
  - no borrowing from W5C-GEN, W5D-DELETE, W6, or new-admit waves.

## Tranche Surfaces

- `restart/skinny/tranches/sk-v14/SYNTHESIS.md`: update R3/R10 wording only
  where it enumerates PRUNE-3, preserving V7 ordering and R10 close semantics.
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`: update R3
  PRUNE-3B wording to name W5B.0 through W5B.4 and keep W5C-GEN blocked until
  aggregate W5B-FRONTEND close.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`: add V8 guard,
  next-dispatch directive for W5B.0 LOCK14-GATE, hard cap per sub-wave, and
  challenge routing; forbid treating W5B.0 through W5B.3 as W5B close.
- `restart/skinny/tranches/sk-v14/HANDOFF.md`: record REDRESS-212 and next
  dispatch W5B.0 LOCK14-GATE.

## V1 Handoff/Migration + Skinny Corpus

- `restart/HANDOFF.md`: record Pass Omega V8 pending/closed state and next
  dispatch W5B.0 LOCK14-GATE.
- `restart/MIGRATION.md`: add the Pass Omega V8 receiver block.
- `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md`: limited alignment
  for active W5B sub-wave authority.
- `restart/skinny/{BENCH,SUBSTRATE}.md`: read/no-op.
