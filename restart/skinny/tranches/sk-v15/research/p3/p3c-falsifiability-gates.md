# SK-V15 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V2.
Date: 2026-05-28.
Scope: measurable falsifiability gates for the final SK-V15 W0..W11 wave set.
Output: this file.
Pass Alpha goalset: SK-V15 closes only after JSON remains 51/51 strict same-plane, CSS no longer carries a 24-row broadcast admit, CSS live admission retires `CSS_GENERATED_RS` / `CssFullParseSummary` / fact-stream-only `parse()` / brace-counter admission, CSS typed Value API exists and is timed on typed output, cssparser is the near-term same-workload comparator, native Apple M5 Max/aarch64 is the only admission host, Lock 14/16 gates self-report exclusions, codegen neutrality and Pattern H hold, Decision Engine lowerers are real, W11L/W11N/W11O FNV closed-enum products stay bench-only, and executable close evidence replaces paper close.
Candidate pool: `research/p2/` after `HARDENING-S-P2-V3-CONSOLIDATED.md`; only V2/V3 survivor classes may feed implementation waves.

## Section 1 - Synthesis

P3-C is the canonical gate source for the final S-P3 V2 topology. It is
not a dispatch or implementation plan. The final wave set is:

`W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11`

No implementation wave may close on "wired", "integrated", "advisory",
"future consumer", or "next wave will measure". A behavior wave closes on
same-run measurements and strict equality/comparator proof. A ledger/gate
wave closes on explicit no-behavior proof and gate-consumed reports.

Global comparators and row rules:

- JSON admission remains strict same-plane only. The 51 JSON rows named
  by `skinny/RESULTS.md` lines 5-55 must stay admitted and compare only
  to same-run strict anchors on the matching output plane.
- CSS admission cannot use the SK-V14 W8R 24-row broadcast. The existing
  24 CSS rows in `skinny/RESULTS.md` lines 112-135 are diagnostic inputs,
  not live admits, until rebuilt as one explicit aggregate diagnostic row
  or independently measured typed-output rows with distinct
  `measurement_row_id`s.
- The W8R tuple `track1_mbps=2319.041`, `cssparser_mbps=2362.037`, and
  `lightningcss_mbps=929.281` is a diagnostic negative fixture only. It
  is never a typed-admission floor.
- CSS same-workload comparator plane means Track 1 and comparator emit
  the same typed CSS value/document facts. `cssparser` is the near-term
  comparator. `lightningcss` counts only when Track 1 emits comparable
  CSSOM/value output and strict equality is proven on that plane.
- Any primitive, kernel, generator path, or new API surface needs scalar
  reference or executable oracle, parity/checkasm where relevant, and the
  same-wave hot-path consumer named below.

Maintain budgets used below:

| Budget | Applies to | Rule |
|---|---|---|
| `M0` telemetry/gate-only | W0 and ledger/gate-only waves | No production parser/codegen/runtime behavior diff; no `skinny/RESULTS.md` Mbps change except the explicitly targeted ledger demotion/collapse; if a bench is rerun, every non-target JSON Track 1/Track 2 cell stays within +/-1.0% of `SK-V15-open` and no verdict downgrades. |
| `M1` behavior wave | W5, W6, or any wave that changes generated/runtime behavior | Every selected target row meets its same-run comparator threshold; every non-target JSON Track 1 and Track 2 cell stays >=98.0% of `SK-V15-open`; all 51 JSON rows remain `A / GO / strict / measured-row`; every existing or rebuilt CSS live row keeps its declared comparator-plane status. |
| `M-css-prune` | W1 only | The 24 current CSS feature rows may lose live-admit status because they are the targeted false admits; JSON 51/51 must obey `M0`; no replacement 24-row admit is allowed unless each row has a distinct measurement. |

### Section 1.1 - Candidate Rebinding Table

P3-A owns the candidate shortlist; P3-C owns the final gate formulas. W0
creates `SK-V15-open`, then every candidate floor below is rebound before
redress. Missing row movement rejects or demotes the candidate to
scalar-only with REDRESS evidence.

| candidate | row universe | final threshold formula | same-wave consumer | proof command shape | fail action |
|---|---|---|---|---|---|
| 1 byte-set run skipper | `apache_builds/parse_only` target; JSON guard rows named in P3-A | `max(P3-A floor, SK-V15-open target * 1.03)`; non-target JSON >=98% | Generated JSON whitespace / colon-comma sites plus CSS/Sheets/BBNF trivia or FIRST-set consumer | scalar loop proof, checkasm/table parity, generated consumer test, cold row measurement | Revert SIMD/generator route or REDRESS as unwired primitive. |
| 2 byte-class/TBL4/movemask | At least two structural/direct target rows from P3-A; JSON guard rows | `max(P3-A floor, SK-V15-open target * 1.00)` for selected targets; non-target JSON >=98% | Generated direct `skip_value` / FIRST-set dispatch or non-JSON FIRST-set scanner | scalar alphabet reference, local-wrapper parity, generated consumer test, cold row measurement | Revert helper or scalar-delegate with REDRESS. |
| 3 bounded literal span | `canada/real_typed_struct` or `unicode_basic/direct_to_struct`; string guard rows | `max(P3-A floor, SK-V15-open target * 1.00)`; non-target JSON >=98% | JSON string end sites plus CSS strings, Sheets quoted strings, or BBNF literal/regex span | scalar delimiter/escape/control oracle, literal checkasm, generated consumer test | Revert SIMD body or REDRESS tiny-string replay. |
| 4 UTF-8 run validator | Unicode target rows from P3-A; JSON guard rows | `max(P3-A floor, SK-V15-open target * 1.00)`; non-target JSON >=98% | JSON string matcher or byte-backed non-JSON literal validation consumer | run-level scalar oracle, checkasm/parity over boundary/tail cases, cold row measurement | Revert hot routing or scalar-delegate. |
| 5 escaped literal segments | Unicode escape target rows from P3-A; JSON guard rows | `max(P3-A floor, SK-V15-open target * 1.00)`; non-target JSON >=98% | Generated JSON escape/string consumer or CSS/Sheets/BBNF escape consumer | segment visitor oracle, escape parity, generated consumer test, cold row measurement | Revert or REDRESS materializer/decoded-string relapse. |
| 6 direct cursor / FIRST-set templates | At least two direct cursor target rows from P3-A; JSON guard rows | `max(P3-A floor, SK-V15-open target * 1.00)`; non-target JSON >=98% | Generated direct `skip_value` / FIRST-set caller in same wave | corpus equality gate, optional byte-class checkasm, generated diff proof | Revert template extraction or REDRESS retained cursor relapse. |
| 7 same-tape capacity/sparse flags | Materialization and parse/typed rows named in P3-A | Maintain listed floors and prove materialization ratio no worse than current diagnostic ratio unless explicitly improved | Existing tape, retained view, and generated accessors consuming opaque flags | offset/flag/value traversal parity, materialization ratio report, cold row measurement if behavior changes | Revert or REDRESS sidecar/second-tape route. |
| 8 same-tape fact projection / mask-to-tape | JSON structural targets from P3-A or CSS typed rows after W5/W6 | JSON: `max(P3-A floor, SK-V15-open target * 1.00)`; CSS: fresh W6 same-run cssparser typed comparator only, never W8R tuple | Existing tape/direct sink/fact output, generated CSS typed value/document output, or generated JSON retained parser | offset/flag/view/fact equality, scalar-vs-SIMD mask parity, typed CSS equality if CSS | Revert, demote to diagnostic, or REDRESS retained sidecar/broadcast route. |

### Section 1.2 - Canonical Telemetry Fields

W0 creates the carrier and every later wave consumes these fields through
`gate-json` or its SK-V15 successor:

| Field | Gate meaning |
|---|---|
| `measurement_row_id` | Stable identity of the timing row that produced the row's Track 1 / comparator tuple. |
| `measurement_origin` | Command, artifact, TSV row, corpus slice, and run id for the timing tuple. |
| `value_plane` | Semantic output plane measured by Track 1. |
| `css_comparator_workload` | CSS comparator plane and workload; `n/a:not-css` for JSON. |
| `generator_source` | Runtime/provider provenance, including grammar source and generator id. |
| `lock14_scan_scope` | Exact roots scanned by Lock 14 plus reported exclusions. |
| `lock16_status` | Primitive/SIMD/ASM status: wired, scalar-delegated, deleted, blocked, strict-checkasm admitted, or not applicable. |
| `checkasm_or_parity_status` | Executable parity/checkasm/oracle command or explicit non-applicable reason. |
| `gate_exclusion_report` | Machine-readable exclusions and their disposition. |
| `broadcast_group_id` | Broadcast/aggregate classification; empty means independent measurement. |

Producer-only telemetry rejects. Hidden one-to-N measurement stamps reject.

### Section 1.3 - Lock 14 / Lock 16 Exclusion Report Schema

Every gate that scans generic roots, generated outputs, SIMD/ASM
primitives, or parity manifests emits and consumes this schema:

| Column | Meaning |
|---|---|
| Included roots | Exact files/directories scanned. |
| Excluded roots | Exact files/directories not scanned. |
| Reason | Why each exclusion exists. |
| Owner | Wave or subsystem responsible for clearing or justifying it. |
| Self-scan status | Proof the validator saw its own exclusion list and scan config. |
| Primitive status | Wired, scalar-delegated, deleted, source-present unwired, strict-checkasm admitted, or blocked. |
| Gate consumer | Command/test that consumes the report before close. |
| Affected rows | Rows or close axes that depend on the scan. |
| Disposition | Admit, non-admission diagnostic, REDRESS, revert, delete, or intrinsic block. |

Silent allowlists, self-exempting grep/checkasm rules, and scan reports
not consumed by the gate are REJECT.

### Section 1.4 - Non-JSON Proof Receiver Matrix

Generic edits need surface-specific receivers, not a generic "non-JSON"
claim. A plan touching one of these surfaces must name the minimum receiver
set, proof shape, and intrinsic-block handling before redress.

| Generic surface | Minimum non-JSON receivers | Proof shape | Intrinsic-block handling |
|---|---|---|---|
| `grammar_provider.rs` | CSS L4 plus Sheets or BBNF-self | Provider-free metadata path emits or checks both receivers; Lock 14 scan has no grammar-family roster. | Block with scan output naming the missing receiver. |
| `runtime_generator.rs` | CSS L4 plus one of Sheets, BBNF-self, CSV, or math | Non-writing regen/check or named generated diff for each receiver. | Block if output is committed-template replay or receiver lacks generator source. |
| Backend lowerers | CSS L4 plus Sheets or BBNF-self | Fixture fails old scaffold and emits runtime-relevant diff or gate-consumed rejected alternative. | Block shape-specific lowerer; no all-five claim. |
| `backend_egraph.rs` | CSS L4 plus one non-CSS receiver | E-graph rewrite changes selected shape or generated selection fixture. | Block decision-driven row movement. |
| `decision_csp.rs` | CSS L4 plus one non-CSS receiver | Removing a required fact changes satisfiability or selection. | Block CSP close as tautological. |
| CostFacts / `cost.rs` | CSS L4 plus Sheets or BBNF-self | Cost fact changes are visible in decision report without grammar-named facts. | Block if cost is advisory-only. |
| `xtask` regen/check | CSS L4 plus at least one non-CSS generated receiver | Command proves no hidden generator/provider branch for both receivers. | Block with command output and owner path. |
| gate/report code (`gate.rs`, `report.rs`) | JSON guard plus CSS L4 diagnostic/typed rows | Gate consumes emitted schema, rejects missing fields, and reports exclusions. | Block producer-only telemetry or self-exempting report. |

## Section 2 - W0..W11 Gates

### W0 - Baseline And Telemetry Lock

Entry gate: S-P3 convergence and G-Omega authorization.

Exit gate:

- JSON 51 rows are captured as `SK-V15-open`; if rerun, every Track 1 and
  Track 2 cell remains within +/-1.0% of the current seed and no row loses
  `A / GO / strict / measured-row`.
- CSS W8R rows are diagnostic or audit-demoted and carry
  `broadcast_group_id=SK-V14-W8R-css-l4-full-parse` plus
  `measurement_origin=diagnostic-broadcast`.
- The ten SK-V15 telemetry fields are present and gate-consumed.
- Host telemetry is native Apple M5 Max / aarch64.

Same-wave consumer: the row gate consumes the new telemetry carrier before
W0 closes.

### W1 - CSS Broadcast Admit Retirement

Entry gate: W0 admitted.

Exit gate:

- No CSS live admit remains from the W8R broadcast.
- Each current CSS row is `AUDIT-DEMOTED`/`NO-GO` with diagnostic origin,
  or the 24 rows collapse to one explicit aggregate diagnostic row.
- An independently admitted CSS feature row needs a unique
  `measurement_row_id`, a unique `measurement_origin`, typed output, and
  same-workload comparator evidence.
- JSON 51/51 obeys `M-css-prune`.

Same-wave consumer: CSS report/gate rendering consumes
`broadcast_group_id` and rejects the 24-row clone pattern.

### W2 - Lock 14 / Lock 16 Gate Restoration

Entry gate: W0 admitted and W1 admitted or CSS explicitly blocked as
non-admission.

Exit gate:

- Lock 14 and Lock 16 reports include included roots, excluded roots,
  reasons, owner, self-scan status, primitive status, gate consumer,
  affected rows, and disposition.
- Previously omitted leak files, generated roots, bench/report/gate roots,
  and checkasm/parity manifests are scanned or reported as blockers.
- Source-present primitives are wired, scalar-delegated, deleted, blocked,
  or strict-checkasm admitted.

Same-wave consumer: the gate reads the exclusion report produced by the
scan; a report-only file not consumed by the gate rejects.

### W3 - Codegen Leak Abrogation

Entry gate: W2 admitted.

Exit gate:

- One coherent generic leak family is removed: grammar-family runtime
  modes, `RuntimeStyle` fanout, hardcoded CSS profile tables, per-grammar
  regen enum/match fanout, or generic pass JSON-byte recognizers.
- Generic crates do not branch on `Json`, `CssL4`, Sheets, corpus names,
  JSON structural roles, or CSS profile names.
- Changed generator paths are exercised by same-wave regen/check output.
- JSON-adjacent generation changes rerun JSON 51/51.

Same-wave consumer: the changed generator path is consumed by `xtask` /
regen / check output in W3.

### W4 - Pattern H Generated Discipline

Entry gate: W2 admitted and W3 admitted/routed.

Exit gate:

- `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`
  remains exactly `67`.
- All 67 files carry true line-1 generator provenance.
- The generator can reproduce the provenance-bearing files or records a
  row-level intrinsic block for unreproducible files.
- Header-only generated status rejects.

Same-wave consumer: the Pattern H gate consumes the provenance metadata
and non-writing regen/check proof.

### W5 - CSS Typed Value Provider

Entry gate: W1-W4 admitted/routed with no open delete dependency.

Exit gate:

- CSS exposes typed value, document, view, and visitor provider surfaces
  comparable to JSON Value API capability.
- Track 1 emits typed CSS value/document facts, not a fact stream,
  four-counter summary, or brace-counter proxy.
- Old CSS proof paths remain diagnostic until W6 proves fresh
  same-workload typed output.
- Any generic generator/provider edit also proves CSS plus Sheets or
  BBNF-self stability, or records intrinsic block.

Same-wave consumer: typed CSS provider tests or gate rows consume the new
provider output. No W8R numeric tuple is accepted as a floor.

### W6 - CSS Same-Workload Retime And Old-Proof Retirement

Entry gate: W5 admitted.

Exit gate:

- Fresh same-run cssparser typed-value/document comparison exists after
  Track 1 emits typed CSS output.
- Track 1 meets or beats the fresh same-run cssparser row on the same
  workload. The floor is derived from that fresh typed run, not W8R.
- `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`,
  and brace-counter proof are retired only with same-wave typed proof.
- JSON 51/51 maintains `M1` if behavior changed.

Same-wave consumer: CSS bench/report/gate consumes typed output equality,
fresh cssparser measurements, and old-proof retirement evidence.

### W7 - Decision Engine Spine

Entry gate: W6 admitted or intrinsically routed.

Exit gate:

- At least one asserted e-graph rewrite is observed by the gate.
- CSP is non-tautological; removing a required fact can change
  satisfiability or selection.
- Generic decision facts are grammar-neutral; no `json_*` or `css_*`
  facts drive generic selection.
- Generated selection/report evidence consumes the decision result.

Same-wave consumers:

- `cargo test -p passes decision_egraph_rewrite_changes_selected_shape -- --exact`
- `cargo test -p passes decision_csp_rejects_missing_required_fact -- --exact`
- `cargo test -p codegen decision_spine_changes_generated_selection_fixture -- --exact`

Equivalent successor commands are allowed only if the plan proves they
exercise the same facts.

### W8 - Lowerer Harness, EagerTape, And OffsetTape

Entry gate: W7 admitted.

Exit gate:

- Lowerer fixtures fail against old label-string scaffolds.
- EagerTape and OffsetTape emit runtime-relevant generated output or a
  gate-consumed rejected alternative.
- CSS L4 plus Sheets or BBNF-self exercise the generic lowerer path when
  the generic path is touched.

Same-wave consumers:

- `cargo test -p codegen backend_lowerer_fixture_rejects_label_string_scaffold -- --exact`
- `cargo test -p codegen lower_eager_tape_emits_runtime_relevant_diff -- --exact`
- `cargo test -p codegen lower_offset_tape_emits_runtime_relevant_diff -- --exact`

### W9 - EventTape, SinkOnly, CollapsedStage, And All-Five Gate

Entry gate: W8 admitted.

Exit gate:

- EventTape, SinkOnly, and CollapsedStage emit runtime-relevant generated
  output or gate-consumed rejected alternatives.
- The all-five BackendShape gate covers `{EagerTape, OffsetTape,
  EventTape, SinkOnly, CollapsedStage}` and no sixth variant.
- EventTape is only an existing BackendShape lowering. It is never a
  sidecar vector, sixth shape, retained parser-owned stream, public
  substrate API, or alternate document projection.

Same-wave consumers:

- `cargo test -p codegen lower_event_tape_emits_runtime_relevant_diff -- --exact`
- `cargo test -p codegen lower_sink_only_emits_runtime_relevant_diff -- --exact`
- `cargo test -p codegen lower_collapsed_stage_emits_runtime_relevant_diff -- --exact`
- all-five lowerer gate through `cargo xtask gate-json --check-results --skv15-backend-lowerers-report <path>` or successor.

### W10 - FNV Quarantine

Entry gate: W9 admitted/routed; if W9 blocks, W10 needs independence
proof.

Exit gate:

- W11L/W11N/W11O FNV closed-enum products are bench-only.
- FNV cannot act as runtime selector, production arbiter, or correctness
  proof.
- Strict-product differential fails closed when Track 1 and Track 2 share
  a closed-enum/FNV sidecar or benchmark-private product helper.
- Production roots contain no FNV migration, or every hit is routed to
  REDRESS with a new contract.

Same-wave consumer: strict-product gate consumes quarantine metadata,
production FNV scan, and adversarial semantic fixtures.

### W11 - Close Reconciliation And PASS-IMPL V2 Handoff

Entry gate: W1-W10 admitted, reverted, redressed, or intrinsically
blocked with row-level proof.

Exit gate:

- RESULTS, REDRESS, rolling delta, HANDOFF, dependency table, generated
  diffs, strict manifests, and cold measurements agree at HEAD.
- PASS-IMPL V2 consumes the close packet.
- PASS-IMPL V2 closes SK-V15 only when each axis is ACCEPT at HEAD or is
  recorded as a row-level intrinsic block with HEAD command output,
  generated diffs/manifests where relevant, strict parity/checkasm where
  relevant, and cold measurements where behavior changed.
- SK-V16 routing is routed remainder after that proof; it is not close
  evidence and cannot substitute for an SK-V15 repair.

Same-wave consumer: PASS-IMPL V2 consumes the W11 close packet before
SK-V15 close can be claimed.

## Section 3 - Forbidden Couplings And Pre-Blocks

Every wave must carry the shared pre-block list:

`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration`

Reject retained structural/cursor/class streams, parser-owned sidecars,
density tables, second tapes, public `UnionTape`, x86 admission anchors,
PMULL hot-body promotion, CSSC CTZ production promotion, numeric/digit
routes without fresh P1 evidence, and any route that cites the W8R CSS
broadcast as positive proof.

Every delete, retirement, demotion, or neutralization must have a visible
dependency row naming retired artifact, delete/retire wave, rebuild
provider wave, proof command, provider-no-later-than-delete status, and
disposition.

## Appendix A - CSS Row Set

The current CSS rows in `skinny/RESULTS.md` lines 112-135 are diagnostic
inputs until W5/W6 rebuild the typed CSS output plane. They must not be
broadcast-admitted.

## Appendix B - JSON Guard Row Set

The JSON guard set is all 51 rows in `skinny/RESULTS.md` lines 5-55:
17 corpora crossed with `parse_only`, `direct_to_struct`, and
`real_typed_struct`.

W0 captures their exact `SK-V15-open` Track 1 and Track 2 Mbps cells.
Gate-only waves use zero-drift/no-diff proof unless a rerun is required;
behavior waves use the `M1` >=98.0% full-table maintain budget.

## Section 5 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v15/HANDOFF.md`.
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v15/research/p3/hardening/V1/redeploy/`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md`.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
