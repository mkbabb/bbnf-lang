# SK-V15 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-28.
Scope: measurable falsifiability gates for the expected SK-V15 W0 plus PRUNE-WAVE-A through REBUILD-WAVE-G wave set.
Output: this file.
Pass Alpha goalset: SK-V15 closes only after JSON remains 51/51 strict same-plane, CSS no longer carries a 24-row broadcast admit, CSS live admission retires `CSS_GENERATED_RS` / `CssFullParseSummary` / fact-stream-only `parse()` / brace-counter admission, CSS typed Value API exists and is timed on typed output, cssparser is the near-term same-workload comparator, native Apple M5 Max/aarch64 is the only admission host, Lock 14/16 gates self-report exclusions, codegen neutrality and Pattern H hold, Decision Engine lowerers are real, W11L/W11N/W11O FNV closed-enum products stay bench-only, and executable close evidence replaces paper close.
Candidate pool: `research/p2/` after `HARDENING-S-P2-V3-CONSOLIDATED.md`; only V2/V3 survivor classes may feed implementation waves.

## §1 - Synthesis

P3-C is a gate file, not a dispatch or implementation plan. P3-B does not exist at this authoring point, so this file binds the expected wave set from `SYNTHESIS.md` §0.3 plus the mandatory S-P3 W0 baseline: W0, PRUNE-WAVE-A, PRUNE-WAVE-B, PRUNE-WAVE-C, PRUNE-WAVE-D, REBUILD-WAVE-E, REBUILD-WAVE-F, and REBUILD-WAVE-G. If P3-B changes that wave set, this file must be revised before any wave dispatch.

Global comparators and row rules:

- JSON admission remains strict same-plane only. The 51 JSON rows named by `skinny/RESULTS.md` lines 5-55 must stay admitted and must compare only to same-run strict anchors on the matching output plane.
- CSS admission cannot use the SK-V14 W8R 24-row broadcast. The existing 24 CSS rows in `skinny/RESULTS.md` lines 112-135 may be diagnostic inputs, but they are not live admits until rebuilt as either one explicit aggregate row or independently measured feature rows with distinct `measurement_row_id`s.
- CSS same-workload comparator plane means the bbnf Track 1 row and comparator emit the same typed CSS value/document facts. `cssparser` is the near-term comparator. `lightningcss` counts only when Track 1 emits comparable CSSOM/value output and strict equality is proven on that plane.
- Every emitted telemetry field is gate-consumed in the same wave. Producer-only telemetry is a failed gate.
- Any primitive, kernel, generator path, or new API surface needs scalar reference or executable oracle, parity/checkasm where relevant, and the same-wave hot-path consumer named below. An unwired primitive is rejected even if correctness tests pass.
- No wave closes on "wired", "integrated", "advisory", "future consumer", or "next wave will measure". A behavior wave closes on Mbps rows; a ledger/gate-only wave closes on explicit no-behavior proof.

Maintain budgets used below:

| Budget | Applies to | Rule |
|---|---|---|
| `M0` telemetry/gate-only | W0 and ledger/gate-only waves | No production parser/codegen/runtime behavior diff; no `skinny/RESULTS.md` Mbps change except the explicitly targeted ledger demotion/collapse; if a bench is rerun, every non-target JSON Track 1/Track 2 cell stays within +/-1.0% of `SK-V15-open` and no verdict downgrades. |
| `M1` behavior wave | W5 or any wave that changes generated/runtime behavior | Every selected target row meets its Mbps threshold; every non-target JSON Track 1 and Track 2 cell stays >= 98.0% of `SK-V15-open`; all 51 JSON rows remain `A / GO / strict / measured-row`; every existing or rebuilt CSS live row keeps its declared comparator-plane status. |
| `M-css-prune` | PRUNE-WAVE-A only | The 24 current CSS feature rows are allowed to lose live-admit status because they are the targeted false admits; JSON 51/51 must obey `M0`; no replacement 24-row admit is allowed unless each row has a distinct measurement. |

## §2 - Deliverable

### W0 - SK-V15-open baseline and telemetry lock

Entry gate: S-P2 is locked by V2/V3 7/7 ACCEPT; `skinny/RESULTS.md` is still byte-identical to SK-V14 close for row metrics; no behavior wave has dispatched.

Named rows and thresholds:

| Row set | Threshold |
|---|---|
| 51 JSON rows, `json/*/{parse_only,direct_to_struct,real_typed_struct}/main` | Capture as `SK-V15-open`. If W0 reruns measurement, every Track 1 and Track 2 cell remains within +/-1.0% of the current `skinny/RESULTS.md` seed and no row loses `A / GO / strict / measured-row`. |
| 24 CSS rows listed in Appendix A | Mark as audit-demoted or diagnostic, not live-admitted. Current broadcast values `track1_mbps=2319.041`, `cssparser_mbps=2362.037`, and `lightningcss_mbps=929.281` may be retained only with `broadcast_group_id=SK-V14-W8R-css-l4-full-parse` and `measurement_origin=diagnostic-broadcast`. |
| Full table | `M0`: no production behavior source change and no Mbps movement outside W0 capture variance. |

Exit gate:

- `gate-json` or its SK-V15 successor rejects any row missing `measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, or `broadcast_group_id` when required.
- CSS rows cannot count toward close while `broadcast_group_id` names the W8R one-to-24 measurement.
- Host telemetry is native Apple M5 Max / aarch64. x86 and AVX-512 fields are diagnostic only.
- Same-wave consumer: the row gate consumes the new telemetry fields before W0 closes.

Revert/REDRESS protocol: revert only W0 telemetry/report/gate/RESULTS edits, preserve the failed render or gate output in the W0 research artifact, add a REDRESS entry naming the missing field or row drift, and block PRUNE-WAVE-A through REBUILD-WAVE-G until W0 closes.

### W1 / PRUNE-WAVE-A - CSS broadcast admit retirement

Entry gate: W0 admitted and the `SK-V15-open` telemetry lock exists.

Named rows and thresholds:

| Row set | Threshold or proof |
|---|---|
| 24 CSS rows in Appendix A | No live admit may remain from the W8R broadcast. Each row must be either `AUDIT-DEMOTED`/`NO-GO` with `measurement_origin=diagnostic-broadcast`, or replaced by an independently measured row with a unique `measurement_row_id` and same-workload comparator evidence. |
| Optional aggregate CSS diagnostic row | If the 24 rows collapse, the only live replacement before W5 is one diagnostic aggregate row, not 24 admits. It must carry the broadcast values only as diagnostic: Track 1 `2319.041`, cssparser `2362.037`, lightningcss `929.281`, verdict not `GO`. |
| 51 JSON rows | `M-css-prune`: no JSON Track 1/Track 2 cell moves by more than +/-1.0% of `SK-V15-open`; no verdict downgrade. |

Exit gate:

- `gate-json` rejects a 24-row CSS admit set if more than one row shares the same `measurement_row_id`, `run_id`, profiled bytes, and Mbps tuple.
- `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()` output, and brace-counter admission are not cited as live CSS admission proof. They may remain as providers until W5 only if every admission path marks them diagnostic.
- Same-workload comparator plane is enforced: cssparser is accepted only if it measures the same CSS production corpus and output plane; lightningcss is planning-only unless comparable CSSOM/value output exists.
- Same-wave consumer: the CSS report/gate renderer consumes `broadcast_group_id` and rejects the 24-row clone pattern in this same wave.

Revert/REDRESS protocol: if any CSS row can still close from the broadcast, revert the CSS RESULTS/report/gate edits, record REDRESS with the offending row ids and duplicate `measurement_row_id`, and block W5. Do not delete CSS providers in this wave unless W5's typed rebuild has already closed.

### W2 / PRUNE-WAVE-B - Lock 14 / Lock 16 exclusion gate repair

Entry gate: W0 admitted; W1 admitted or explicitly blocked with CSS rows still non-admission.

Named rows and thresholds:

| Row set | Threshold or proof |
|---|---|
| 51 JSON rows plus current CSS diagnostic aggregate/rows | Explicit non-performance proof: no production parser/runtime/codegen output diff and no `skinny/RESULTS.md` Mbps diff. If a source move forces measurement, apply `M0`. |
| Gate scan roots | Lock 14 and Lock 16 scans include previously excluded leak files, every exclusion is itself reported, and self-exempting grep/checkasm gates fail closed. |

Exit gate:

- Gate output includes `lock14_scan_scope`, `lock16_status`, and `gate_exclusion_report` for every checked row.
- A scan that omits its own exclusion list, uses a silent allowlist, or excludes generated leak files without reporting them rejects close.
- Generic crates gain no JSON/CSS runtime mode split, no grammar-name policy branch, and no JSON byte recognizer under a neutral name.
- Same-wave consumer: the gate reads the exclusion report produced by the scan; a report-only file not consumed by the gate is rejected.

Revert/REDRESS protocol: revert Lock 14/16 gate/report edits as one slice, save the failing gate output, add REDRESS naming the omitted root or self-exempting exclusion, and block W3/W5/W6 if generic scan coverage is not fail-closed.

### W3 / PRUNE-WAVE-C - codegen leak abrogation

Entry gate: W0 and W2 admitted.

Named rows and thresholds:

| Row set | Threshold or proof |
|---|---|
| 51 JSON rows plus CSS diagnostic rows | Explicit non-performance proof by default: generated output either stays byte-identical or any intentional generated-output diff is paired with `M1` measurement. |
| Codegen leak surfaces | Zero matches for grammar-family runtime modes, root `RuntimeStyle`, hardcoded CSS profile tables, per-grammar regen enum/match fanout, and generic pass JSON-byte recognizers. |

Exit gate:

- Codegen consumes grammar metadata without branching on `Json`, `CssL4`, Sheets, corpus names, JSON structural roles, or CSS profile names in generic crates.
- Any generated runtime diff names the consuming grammar and passes same-wave non-JSON proof for CSS L4, Sheets, or BBNF-self as applicable.
- Same-wave consumer: the changed codegen path is exercised by `xtask`/regen/check output in the same wave; a cleanup that only renames code without a consumer command is paper-close.

Revert/REDRESS protocol: revert codegen/generator/gate edits together, preserve the leak grep and generated-output diff, add REDRESS with the leak token and owner path, and keep W5/W6 blocked from citing the cleanup.

### W4 / PRUNE-WAVE-D - Pattern H generated discipline

Entry gate: W0 admitted; W2 admitted; W3 admitted or routed if W3 owns the generator leak that would invalidate Pattern H.

Named rows and thresholds:

| Row set | Threshold or proof |
|---|---|
| 51 JSON rows plus CSS diagnostic rows | Explicit non-performance proof: Pattern H is a provenance/generator discipline wave. No production behavior diff and no Mbps diff by default; any generated behavior change requires `M1`. |
| Runtime root files | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` remains exactly `67`; all 67 files carry true generator provenance at line 1. Header-only or fake-generated status rejects. |

Exit gate:

- The generator provenance gate verifies line 1 for all 67 files and fails on missing, stale, or hand-written provenance.
- The wave proves the generator can reproduce the provenance-bearing files or records a REDRESS block for any file it cannot truthfully generate.
- Same-wave consumer: provenance metadata is consumed by the Pattern H gate in the same wave.

Revert/REDRESS protocol: revert provenance/generator/report edits, save the failing 67-file inventory, add REDRESS naming each file that is fake-generated or unreproducible, and block any later wave from using Pattern H as close evidence.

### W5 / REBUILD-WAVE-E - CSS typed Value API and typed-output retiming

Entry gate: W0 admitted; W1 admitted; W2 admitted. W3/W4 must be admitted or explicitly routed if their codegen/provenance cleanup is on the CSS provider path. `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()` output, and brace-counter admission can be retired only here or after this wave has already proven the replacement.

Named rows and thresholds:

| Row set | Threshold |
|---|---|
| Default aggregate CSS row `css_l4/value_api_full_table/direct_to_struct/main` | Track 1 typed Value API Mbps must be >= same-run cssparser typed-value/document Mbps. On the current production corpus seed, the minimum explicit floor is `2362.037 Mbps` if the comparator workload remains the W8R full production corpus. |
| Optional per-feature CSS rows in Appendix A | If independently admitted, every row must have a distinct `measurement_row_id`; Track 1 Mbps must be >= that row's same-run cssparser typed-value/document Mbps. Reusing `2319.041 / 2362.037 / 929.281` across the 24 rows is an automatic reject. |
| Lightningcss comparator | May only support admission after comparable CSSOM/value output equality; if valid, Track 1 must also be >= `lightningcss_mbps + 1.0`. With the W8R seed this diagnostic floor is `930.281 Mbps`, but cssparser's same-workload floor is controlling unless a new same-plane comparator run says otherwise. |
| 51 JSON rows | `M1`: every JSON non-target Track 1 and Track 2 cell remains >= 98.0% of `SK-V15-open`, no verdict downgrade. |

Exit gate:

- CSS exposes typed value, document, view, and visitor surfaces with JSON Value API parity of capability, not just a fact stream or parse summary.
- Retired live-admission paths are absent from close evidence: `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()` output, and brace-counter admission cannot be referenced as the passing row.
- Track 1, cssparser, and lightningcss if used emit comparable typed value/document facts and strict equality proofs.
- Scalar/oracle and parity proof exists for any primitive used from the S-P2 survivor pool.
- Same-wave consumer: the generated CSS typed parser/value API and bench row consume any new byte-set, string, UTF-8, escape, tape, or fact primitive in this same wave.

Revert/REDRESS protocol: if the typed CSS row misses cssparser, emits the wrong plane, reuses the broadcast, or regresses JSON beyond `M1`, revert CSS runtime/codegen/bench/gate/RESULTS edits and any provider retirement as one slice. Save `/tmp/skv15-W5-css-value-api-rejected.patch`, add REDRESS naming the missed CSS row and JSON guard failures, and keep old CSS admission retired/demoted rather than restoring the 24-row broadcast as proof.

### W6 / REBUILD-WAVE-F - Decision Engine activation and BackendShape lowerers

Entry gate: W0 admitted; W2/W3 admitted; W4 admitted or routed if Pattern H applies to generated lowerer outputs.

Named rows and thresholds:

| Row set | Threshold or proof |
|---|---|
| 51 JSON rows plus any W5 admitted CSS row | Explicit non-performance proof by default. If generated runtime output changes, apply `M1` to the full table. |
| Decision Engine gate rows | `egraph_rewrite_count >= 1`; CSP is non-tautological; grammar-named CSP facts are zero; all five `BackendShape` lowerers are invoked and emit non-placeholder output. |

Exit gate:

- The Decision Engine is not a no-op scaffold: at least one e-graph rewrite is observed by the gate, and the CSP has a real constraint set whose satisfiability would change if a required fact is removed.
- All five BackendShape lowerers produce executable generated output or a gate-consumed rejected alternative; no lowerer may be a `todo!`, pass-through placeholder, or string-only shell.
- Grammar-specific policy stays in generated grammar metadata, not generic lowerer branches.
- Same-wave consumer: compile/lower/regenerate commands consume the decision results and lowerers in the same wave.

Revert/REDRESS protocol: revert Decision Engine/lowerer/gate/generated-output edits, preserve the failed e-graph/CSP/lowerer report, add REDRESS naming the placeholder lowerer or tautological fact, and block W7 and close from citing Decision Engine activation.

### W7 / REBUILD-WAVE-G - FNV quarantine and strict-product differential hardening

Entry gate: W0 admitted; W6 admitted or explicitly routed if Decision Engine output affects strict-product rows.

Named rows and thresholds:

| Row | Gate |
|---|---|
| `json/gsoc-2018/direct_to_struct/main` | Gate-only zero-drift unless source behavior changes; current guard cells are Track 1 `7228.198`, Track 2 `6036.352`, sonic strict `6669.742` Mbps. |
| `json/gsoc-2018/real_typed_struct/main` | Gate-only zero-drift unless source behavior changes; current guard cells are Track 1 `7176.742`, Track 2 `6233.927`, sonic strict `6627.652` Mbps. |
| `json/unicode_mixed/direct_to_struct/main` | Gate-only zero-drift unless source behavior changes; current guard cells are Track 1 `5903.562`, Track 2 `3275.337`, sonic strict `5340.219` Mbps. |
| `json/unicode_mixed/real_typed_struct/main` | Gate-only zero-drift unless source behavior changes; current guard cells are Track 1 `5837.942`, Track 2 `3247.472`, sonic strict `5309.589` Mbps. |
| `json/y_string_unicode/direct_to_struct/main` | Gate-only zero-drift unless source behavior changes; current guard cells are Track 1 `5493.522`, Track 2 `3333.663`, sonic strict `4263.646` Mbps. |
| `json/y_string_unicode/real_typed_struct/main` | Gate-only zero-drift unless source behavior changes; current guard cells are Track 1 `5361.584`, Track 2 `3993.709`, sonic strict `4266.896` Mbps. |
| Full table | If source behavior changes, apply `M1`; otherwise prove `skinny/RESULTS.md` has no Mbps diff. |

Exit gate:

- W11L/W11N/W11O FNV closed-enum products are marked bench-only and cannot act as live strict-product admission proof.
- The strict-product differential fails closed when Track 1 and Track 2 share a closed-enum/FNV sidecar or benchmark-private product helper.
- Any report or gate field that cites FNV must also cite `fnv_quarantine=bench-only` and a strict-product comparator proof.
- Same-wave consumer: the strict-product gate consumes the quarantine metadata and negative coupling tests in this same wave.

Revert/REDRESS protocol: revert FNV quarantine/gate/report edits, save the failing strict-product differential output, add REDRESS naming the coupled row and sidecar/helper, and block SK-V15 close from citing W11L/W11N/W11O FNV products.

## §3 - Falsifiability binding

Every wave gate is falsifiable by one of two mechanisms:

1. Performance waves must produce same-run Mbps evidence against the named row thresholds above and full-table `M1` maintain.
2. Ledger/gate-only waves must prove no behavior drift with exact command output, row diffs, scan reports, and gate consumption. A ledger/gate-only wave that edits runtime/parser/codegen behavior silently becomes a behavior wave and must satisfy `M1`.

CSS-specific rejection rules:

- Reject if 24 CSS rows share one measurement and claim 24 admits.
- Reject if cssparser measures parse/full-parse while Track 1 emits fact stream or vice versa.
- Reject if lightningcss is used before Track 1 emits comparable CSSOM/value output.
- Reject if `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, or brace-counter admission is the live proof after W5.

Primitive-specific rejection rules:

- Reject any primitive without scalar reference or oracle.
- Reject SIMD/ASM without parity/checkasm before hot-path wiring.
- Reject any primitive without same-wave consumer named in the wave.
- Reject retained structural/cursor/class streams, parser-owned sidecars, density tables, second tapes, public `UnionTape`, x86 admission anchors, PMULL hot-body promotion, CSSC CTZ production promotion, numeric/digit routes without fresh P1 evidence, and any route blocked by REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, or 96-98.

## §4 - Pre-blocked routes

Pre-blocks every wave must carry:

| REDRESS route | Block |
|---|---|
| 28+33 | Tiny-string/Class A NEON replay and parity-green but row-regressive string wiring. |
| 50-55 | Parse-time side tables, byte-class cursor sidecars, parser-local structural cursors, decoded string stats, quote-source materializers. |
| 60-72 | Retained parse shortcuts and eager/direct string materializer families. |
| 80 | Canada mantissa-widen/f64 fallback and numeric rescue without current P1 hot leaf. |
| 82-84 | One-quartet unicode classifier, StringBlock16 tiny probe, object-pair value-byte compaction. |
| 88 | PMULL prefix-XOR as production hot body. |
| 89 | CSSC CTZ/bulk consumer production promotion. |
| 96-98 | Retained class columns, streaming structural cursors, union-substrate/class-lane replay. |
| 210-213 | Provider/template deletion or CSS runtime projection before a real rebuild provider exists. |
| 215 | CSS W8/W8R broadcast and wrong-plane/fact-stream/full-parse confusion. |
| 242-247 | Decoded-string, decoded-codepoint, fixed-shape unicode floor, indexed-string, structural-stream, and string64 retries without fresh material differential. |

## Appendix A - CSS row set

The current CSS rows are diagnostic inputs until W5 rebuilds the typed CSS output plane. These rows must not be broadcast-admitted:

| Row |
|---|
| `css_l4/declaration_values/direct_to_struct/main` |
| `css_l4/declarations/direct_to_struct/main` |
| `css_l4/stylesheet_root/direct_to_struct/main` |
| `css_l4/selectors/direct_to_struct/main` |
| `css_l4/at_rules_keyframes/direct_to_struct/main` |
| `css_l4/nested_rules/direct_to_struct/main` |
| `css_l4/css_variables/direct_to_struct/main` |
| `css_l4/calc_expressions/direct_to_struct/main` |
| `css_l4/var_url_functions/direct_to_struct/main` |
| `css_l4/color_functions/direct_to_struct/main` |
| `css_l4/gradients/direct_to_struct/main` |
| `css_l4/transforms/direct_to_struct/main` |
| `css_l4/filters/direct_to_struct/main` |
| `css_l4/easing_functions/direct_to_struct/main` |
| `css_l4/media_queries/direct_to_struct/main` |
| `css_l4/vendor_prefixes/direct_to_struct/main` |
| `css_l4/custom_at_rules/direct_to_struct/main` |
| `css_l4/pseudo_classes/direct_to_struct/main` |
| `css_l4/pseudo_elements/direct_to_struct/main` |
| `css_l4/attribute_selectors/direct_to_struct/main` |
| `css_l4/logical_properties/direct_to_struct/main` |
| `css_l4/grid/direct_to_struct/main` |
| `css_l4/flexbox/direct_to_struct/main` |
| `css_l4/typed_property_groups/direct_to_struct/main` |

## Appendix B - JSON guard row set

The JSON guard set is all 51 rows in `skinny/RESULTS.md` lines 5-55: each of the 17 corpora `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, and `y_string_unicode` crossed with `parse_only`, `direct_to_struct`, and `real_typed_struct`.

W0 captures their exact `SK-V15-open` Track 1 and Track 2 Mbps cells. Gate-only waves use zero-drift/no-diff proof unless a rerun is required; behavior waves use the `M1` >= 98.0% full-table maintain budget.

## §5 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v15/HANDOFF.md`.
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v15/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md`.
- `restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md`.
- `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md`.
- `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md`.
- `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md`.
- `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md`.
- `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md`.
- `restart/skinny/tranches/sk-v8/SPEC.md`.
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
