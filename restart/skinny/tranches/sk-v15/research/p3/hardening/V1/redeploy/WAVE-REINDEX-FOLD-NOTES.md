# SK-V15 S-P3 V2 Redeploy: Wave Reindex Fold Notes

Owned output:
`restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/WAVE-REINDEX-FOLD-NOTES.md`

Scope read: current S-P3 `p3b-wave-sequencing.md`,
`p3c-falsifiability-gates.md`, `p3f-spec-draft.md`, `SPEC.md`,
`DISPATCH-PROMPT.md`, and V1 hardening CH1, CH3, CH4, CH6, CH7.

Disposition: V2 must reindex the packet from W0-W9 to W0-W11. The
reindex is not optional. CH4 found old W5 and old W7 too broad for the
30 minute redress cap, and CH1/CH3/CH6/CH7 all found stale gate or
overfit routes tied to the current W0-W9 fold.

## Resulting W0-W11 Map

| New wave | Transitional name | Old wave source | Receiver | Required fold summary |
|---|---|---|---|---|
| W0 | W0 | old W0 | Baseline and telemetry lock | Keep baseline role, but normalize P3-B telemetry to the exact P3-D/SPEC field names: `measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, `broadcast_group_id`. Alias-only telemetry rejects unless a schema bump and gate-consumed mapping are explicit. |
| W1 | W1 | old W1 | PRUNE-A CSS admission honesty | Keep as CSS broadcast admit retirement. W8R metrics are diagnostic negative fixtures only; no typed-admission threshold may reuse `2319.041`, `2362.037`, or `929.281` as a live floor. |
| W2 | W2 | old W2 | PRUNE-B Lock 14 / Lock 16 gate restoration | Keep receiver. Promote the gate-exclusion report table into SPEC and DISPATCH with included roots, excluded roots, exclusion reasons, and gate consumption. |
| W3 | W3 | old W3 | PRUNE-C codegen leak abrogation | Keep receiver. Ensure dependency-table rows are visible in final dispatch surfaces for every neutralize/delete/retire action. |
| W4 | W4 | old W4 | PRUNE-D Pattern H generated discipline | Keep receiver. Preserve non-writing regen/check proof before destructive root runtime deletion. |
| W5 | W5A | old W5 first half | REBUILD-E.1 CSS typed Value provider | Build typed CSS value, document, view, and visitor provider surfaces. Same-wave consumer is a gate-consumed typed provider proof or one diagnostic aggregate row. W5 must not retire live CSS proof unless it also satisfies W6-grade retiming and retirement proof in the same wave. |
| W6 | W5B | old W5 second half | REBUILD-E.2 CSS same-workload retime and old-proof retirement | Capture fresh same-run `cssparser` typed-value/document comparator after Track 1 emits typed CSS output. Retire `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter live proof only with same-wave typed provider proof. Update RESULTS, REDRESS, gates, and optional independent feature rows. |
| W7 | W6 | old W6 | REBUILD-F.1 Decision Engine spine | Shift old W6 to W7. Keep e-graph/CSP spine only. Name the exact decision gate/test/generated-selection fixture that consumes e-graph and CSP output and fails against the old scaffold. |
| W8 | W7A | old W7 first half | REBUILD-F.2A lowerer fixture harness plus EagerTape/OffsetTape | Split old lowerer wave. Implement scaffold-failing fixture harness and EagerTape plus OffsetTape lowerers or gate-consumed rejected alternatives. EventTape is not touched here unless the V2 plan proves it still fits without exceeding the cap. |
| W9 | W7B | old W7 second half | REBUILD-F.2B EventTape/SinkOnly/CollapsedStage plus all-five gate | Complete EventTape, SinkOnly, and CollapsedStage lowerers plus the all-five lowerer gate. Counter-bind EventTape as a BackendShape lowering only: no sixth shape, public substrate API, sidecar event vector, retained parser-owned stream, or alternate document projection. |
| W10 | W8 | old W8 | REBUILD-G FNV quarantine | Shift old FNV quarantine to W10. Preserve bench-only quarantine and strict-product adversarial fixtures. |
| W11 | W9 | old W9 | Close reconciliation and PASS-IMPL V2 handoff | Shift old close to W11. State that SK-V16 routing is routed remainder only after PASS-IMPL V2 acceptance or row-level intrinsic-block proof; it is not SK-V15 close evidence. |

This map stays within the 12-wave ceiling. No extra top-level wave can be
added without removing or folding an existing one.

## Old Section Mapping

| Surface | Old section or row | New destination |
|---|---|---|
| `p3b-wave-sequencing.md` section 1 wave order | `W0 -> ... -> W9` | Replace with `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11`. Explain W5A/W5B and W7A/W7B as transitional names only; final dispatch uses integer W0-W11. |
| `p3b-wave-sequencing.md` deliverable table W0-W4 | old W0-W4 | Keep wave numbers and receivers; add cost/risk/LOC columns and exact row gates. |
| `p3b-wave-sequencing.md` deliverable table W5 | old W5 CSS typed Value | Split to new W5 and W6. New W5 owns provider construction and diagnostic aggregate consumer. New W6 owns same-workload retime, old-proof retirement, RESULTS/REDRESS/gate updates, and optional independent feature rows. |
| `p3b-wave-sequencing.md` deliverable table W6 | old W6 Decision Engine spine | Shift to new W7. Remove lowerer obligations from this row and add exact executable consumer for e-graph/CSP output. |
| `p3b-wave-sequencing.md` deliverable table W7 | old W7 BackendShape lowerers | Split to new W8 and W9. New W8 owns fixture harness plus EagerTape/OffsetTape. New W9 owns EventTape/SinkOnly/CollapsedStage plus all-five gate. |
| `p3b-wave-sequencing.md` deliverable table W8 | old W8 FNV quarantine | Shift to new W10. |
| `p3b-wave-sequencing.md` deliverable table W9 | old W9 close | Shift to new W11. |
| `p3b-wave-sequencing.md` NEW-CH3 table | delete/retire rows naming W5, W6, W7, W8, W9 | Re-key rows: CSS live proof retire to W6, Decision Engine scaffold spine retire to W7, lowerer scaffold retire to W8/W9 by shape, FNV quarantine to W10, close orphan check to W11. |
| `p3c-falsifiability-gates.md` section 1 | stale "P3-B does not exist" statement | Delete. Replace with a statement that P3-C is folded to final W0-W11 after V1 hardening. |
| `p3c-falsifiability-gates.md` W0-W4 gates | old W0-W4 | Keep receivers, but add `SK-V15-open` row universe and threshold tables in each gate. |
| `p3c-falsifiability-gates.md` W5 gate | old W5 CSS typed Value | Split to new W5 provider gate and new W6 retime/retire gate. Remove W8R broadcast values from live thresholds; require fresh same-run typed cssparser comparator data in W6. |
| `p3c-falsifiability-gates.md` W6 gate | old combined Decision Engine/lowerers | New W7 only for e-graph/CSP spine. Lowerer proof moves to W8/W9. |
| `p3c-falsifiability-gates.md` W7 gate | old FNV quarantine | Shift to W10. |
| `p3c-falsifiability-gates.md` close material | implicit or absent close reconciliation | Add new W11 close gate with PASS-IMPL V2 acceptance or row-level intrinsic-block proof; no future-phase close language. |
| `p3f-spec-draft.md` section 1 input fold | W0-W9 and prose remap of P3-C | Replace with W0-W11 fold and state P3-C itself is rewritten, not merely remapped in prose. |
| `p3f-spec-draft.md` section 2 | "Sections 3 through 12 for W0 through W9" | Replace with "Sections 3 through 14 for W0 through W11"; later sections shift accordingly. |
| `SPEC.md` dispatch lock | W0 first legal implementation wave; W1-W9 gated | Update to W0 first legal wave; W1-W11 gated. |
| `SPEC.md` close condition item 10 | Decision Engine and all five lowerers as one close item | Keep close condition, but cross-reference W7 for spine and W8/W9 for lowerers. |
| `SPEC.md` section 2 manifest | W0-W9 | Replace with W0-W11 table including LOC/risk/cost columns. |
| `SPEC.md` section 8 | old W5 CSS typed Value API | Split into new section 8 W5 provider and section 9 W6 retime/retire. |
| `SPEC.md` section 9 | old W6 Decision Engine spine | Shift to new section 10 W7. |
| `SPEC.md` section 10 | old W7 lowerers | Split to new section 11 W8 and section 12 W9. |
| `SPEC.md` section 11 | old W8 FNV | Shift to new section 13 W10. |
| `SPEC.md` section 12 | old W9 close | Shift to new section 14 W11. |
| `SPEC.md` sections 13-14 | pre-blocks and dispatch posture | Shift to sections 15-16. Normalize REDRESS pre-block list across P3-B/P3-C/P3-E/P3-F/SPEC/DISPATCH. |
| `DISPATCH-PROMPT.md` status and authority | W0-W9 | Update to W0-W11 and keep same authority order unless P3-C/P3-F paths change. |
| `DISPATCH-PROMPT.md` challenge triggers | W2-W8 mandatory candidates | Update to W2-W10 mandatory candidates, with W5/W6/W7/W8/W9/W10 named explicitly. |
| `DISPATCH-PROMPT.md` per-wave envelopes W5-W9 | old W5-W9 | Split/shift to new W5-W11 following the map above. |
| `DISPATCH-PROMPT.md` same-wave consumer mandate | generic | Add explicit W6 cssparser typed comparator consumer, W7 decision fixture consumer, W8/W9 per-lowerer fixture consumers, and W10 strict-product quarantine consumer. |

## Exact Surfaces That Must Be Updated

Mandatory S-P3 packet edits:

- `restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v15/SPEC.md`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`

Likely companion S-P3 edits required by the hardening findings:

- `restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md`
  to remove W8R broadcast metrics from live typed CSS floors and leave them
  only as diagnostic negative fixtures.
- `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md`
  only if the final P3-B fold needs an explicit alias-rejection or schema
  bump note next to the canonical field list.
- `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md`
  to normalize the shared REDRESS list with 28+33, 50-55, 60-72, 80,
  82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV
  closed-enum production migration.

Hardening and consolidation surfaces after the fold:

- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/CH1.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/CH3.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/CH4.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/CH6.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/CH7.md`
- `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`

No implementation source should be touched by this S-P3 V2 fold. The
fold is a contract repair before dispatch.

## V2 Fold Requirements By Lens

CH1 correctness:

- Rewrite P3-C to the final W0-W11 topology.
- Add measurable row gates for every wave: 51 JSON rows against
  `SK-V15-open`, CSS Appendix A or aggregate rows where applicable, and
  strict-product/FNV rows where applicable.
- Remove W8R broadcast metrics from live CSS typed-output floors.
- Add a candidate-gate table rebinding P3-A candidates to `SK-V15-open`
  formulas, same-wave consumers, scalar/oracle/parity requirements, and
  reject/demotion actions.

CH3 regression:

- Promote the NEW-CH3-V5-01 dependency table from P3-B into SPEC and
  DISPATCH.
- Fail every delete/retire action without a matching dependency-table
  row and same-wave or prior-wave provider proof.
- Normalize REDRESS pre-block clusters across every S-P3 surface.
- Preserve W2R/W4R anti-cycle language as dependency-row invariants.

CH4 cost:

- Add `Risk class`, `Manual source/test LOC budget`,
  `Generated output budget/status`, `Docs/ledger LOC budget`,
  `Phase caps`, `Split trigger`, and `Same-wave consumer` columns to
  P3-B and SPEC section 2.
- Mirror practical cost summaries in DISPATCH envelopes.
- Keep the SK-V15 Alpha cap explicit: research <=20 minutes, plan <=15
  minutes, redress <=30 minutes.
- Add a cap practicality rule: if the plan estimate exceeds either LOC
  budget or 30 minutes redress, split before redress or record intrinsic
  block. Challenge time is not implementation overflow.

CH6 anti-paper-close:

- W7 must name an executable decision gate/test/generated-selection
  fixture that consumes e-graph/CSP output and fails against the old
  scaffold.
- W8 and W9 must name regenerate/test fixtures for the specific
  BackendShape lowerers they own, or a gate-consumed rejected alternative.
- W11 must state SK-V16 routing is routed remainder after proof, not the
  evidence that closes SK-V15.

CH7 overfit-prune / gate-exclusion:

- Normalize telemetry vocabulary to the P3-D/SPEC field names and reject
  alias-only telemetry.
- Keep W8R CSS metrics only as diagnostic negative fixtures.
- Promote the gate-exclusion report table into SPEC and DISPATCH.
- Counter-bind EventTape as a BackendShape lowering only; no sidecar
  event vector, sixth shape, public substrate API, retained parser-owned
  stream, or alternate document projection.
- Carry the S-P2 rejected-route set unchanged through W0-W11.

## Reindex Guardrails

1. Final dispatch names must be integer waves W0-W11. W5A/W5B and
   W7A/W7B are explanatory aliases only.
2. No top-level wave may be added after W11 without deleting or folding a
   wave first, because W0-W11 consumes the full 12-wave ceiling.
3. Every shifted dependency row must use the new integer wave id.
4. Every old cross-reference to W5/W6/W7/W8/W9 must be audited. The
   dangerous stale references are:
   - W5 as both CSS provider and retime/retire.
   - W6 as combined Decision Engine plus lowerers.
   - W7 as either lowerers or FNV.
   - W8 as FNV after the split.
   - W9 as close after the split.
5. A V2 packet that only describes the map in P3-F but leaves P3-C,
   SPEC, or DISPATCH stale remains REVISE under CH1, CH3, CH4, CH6, and
   CH7.
