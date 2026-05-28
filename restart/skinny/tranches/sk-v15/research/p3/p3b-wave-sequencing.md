# SK-V15 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: V15.
Date: 2026-05-28.
Scope: Order the SK-V15 Pass Alpha goalset and locked S-P2 survivor boundary into W0..W9, with W0 baseline/telemetry and PRUNE-before-REBUILD sequencing.
Output: this file.
Pass Alpha goalset: JSON 51/51 guard preserved; CSS broadcast and parser contrivance pruned; Lock 14/16 gates restored; codegen leaks pruned; Pattern H provenance established; CSS Value API, Decision Engine, and FNV quarantine rebuilt.
Candidate pool: `restart/skinny/tranches/sk-v15/research/p2/` post-CHALLENGE survivors.
P3-B hard cap: 45 minutes.

## §1 - Synthesis

P3-B is a planning artifact. It is read-only against `skinny/` and against implementation sources; the only owned output for this pass is this file. The dispatch bracket cannot start until S-P3 converges under the orchestrator rule and G-Omega authorizes execution. G-Alpha remains auto-passed under the SK-V15 pin and is not a stop condition.

The wave order is:

`W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9`

This is 10 waves, below the orchestrator escalation limit of 12. W0 is mandatory baseline and telemetry lock. W1..W4 are prune waves, ordered as CSS contrivance prune, gate restoration, codegen leak prune, then Pattern H discipline. W5..W8 are rebuild or quarantine waves, ordered as CSS Value, Decision Engine, Decision Engine lowerer completion, then FNV quarantine. W9 is close reconciliation.

The split of REBUILD-F across W6 and W7 is intentional. Alpha requires exact-owner waves and caps; the Decision Engine fact/rewrite/CSP spine and the generated lowerer implementations have different owner path families and falsifiability surfaces. Splitting them preserves the same receiver while keeping each redress slice cap-valid.

Every implementation wave uses the SK-V15 Alpha cap, not the broader default triumvirate cap:

| Phase | Hard cap | Rule |
| --- | ---: | --- |
| Research | <=20 minutes | Read-only; may end with intrinsic block. |
| Plan | <=15 minutes | One intervention, owner paths, falsifiability, revert route. |
| Redress | <=30 minutes | Implement, measure, and either admit, redress, revert, or block. |

If a wave cannot fit its cap, it must be split before redress. Challenge is mandatory only when the triumvirate first-of-class, substrate, or kernel rules are triggered; it is not a loophole for oversize implementation.

No wave may introduce or retain a primitive, provider, or generated-path claim without a same-wave consumer and falsifiable proof. If the consumer is absent, the wave must delete, demote, scalar-delegate, route to REDRESS, or block the claim in that same wave.

The governing dependency rule is NEW-CH3-V5-01: no deletion or retirement before rebuild proof. Diagnostic demotion of a claim may occur before a rebuild only when it removes an admission claim rather than deleting the live provider. Live parser, provider, runtime, or generated artifacts cannot be removed until the replacement provider and proof command land no later than the delete or retire wave.

The S-P2 survivor boundary remains in force. Eligible families are grammar-neutral byte-set/classifier/movemask, grammar-neutral string/literal/UTF-8 scalar/parity work, per-grammar escape/segment template work that is not generic JSON, same-tape capacity/sparse flag/fact projection/local mask-to-tape, and direct cursor/FIRST-set templates without retained state. Numeric/digit runs, EOB, PMULL hot-body, CSSC bulk consumer, x86 promotion, retained structural/cursor/class streams, schema builders, harness hashes, CSS broadcast proof, and stale CSS witnesses remain outside the shortlist unless a fresh Alpha/P1 bridge reopens them.

SK-V15 does not dispatch a standalone P2 primitive performance wave before the prune/rebuild tranche closes. P2 survivor families may appear inside W5-W7 only as rebuild providers or same-wave consumers that obey the S-P2 boundary; otherwise they remain queued after W9 for a separate bracket.

## §2 - Deliverable Wave Table

Common entry gate for every wave: S-P3 convergence is achieved, G-Omega authorizes execution, the dirty worktree is inspected before staging, unrelated work is preserved, and the wave plan names all touched owner path families. Each wave must include a falsifiable exit command set in its plan and must route misses into REDRESS rather than paper-closing.

| Wave | Receiver | Conditional status | Entry gate | Owner path family | Hard cap | Exit and dependency rule |
| --- | --- | --- | --- | --- | --- | --- |
| W0 | Baseline and telemetry lock | First dispatch after S-P3 convergence plus G-Omega. No behavior change. | Current SK-V15 state captured: JSON parse/direct/typed 51/51 guard rows; CSS 24-row state audit-demoted; existing redress blockers listed. | `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/crates/bbnf-bench/src/report.rs`, telemetry manifests. | Research <=20m, Plan <=15m, Redress <=30m. | Gate consumes SK-V8 telemetry fields plus SK-V15 anti-broadcast fields: `measurement_row_id`, `broadcast_group_id`, `sample_count`, `row_claim_scope`, `comparator_workload_id`, `producer_path`, `generator_source_id`, `semantic_output_kind`, and `strictness_source`. Missing, producer-only, reused, or broadcast rows reject. No provider deletion. |
| W1 | PRUNE-A: CSS contrivance prune | Conditional dispatch after W0. It may demote/reclassify claims but cannot delete live CSS providers. | W0 proves gate has baseline and anti-broadcast telemetry. CSS rows are known repeated-tuple, comparator-mismatched, and hand-string parser evidence. | CSS bench/report/gate surfaces: `skinny/crates/bbnf-bench/src/css_l4_w8.rs`, `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/RESULTS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`. | Research <=20m, Plan <=15m, Redress <=30m. | CSS admission becomes diagnostic unless each row has distinct measurement identity and same-workload comparator evidence. `CSS_GENERATED_RS`, `CssFullParseSummary`, and fact-stream parse artifacts are not deleted here; their delete or retire wave is W5 unless a same-wave typed provider proof lands earlier. |
| W2 | PRUNE-B: Lock 14/16 gate restoration | Conditional dispatch after W0 and W1, before any later generic cleanliness claim. | W0 telemetry locked; W1 has removed CSS broadcast pressure from admission. | Gate and manifest surfaces: `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/xtask/src/main.rs`, `skinny/crates/bbnf-simd/tests/checkasm_*`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`, and scan roots under `skinny/crates/codegen/`, `skinny/crates/passes/`, `skinny/crates/runtime/`, `skinny/crates/bbnf-bench/src/json_*`, and `skinny/crates/codegen/src/json_templates/`. | Research <=20m, Plan <=15m, Redress <=30m. | Lock 14 scans the leak roots that were previously omitted, including `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, and JSON templates. Lock 16 has a strict source-present primitive manifest under `BBNF_SIMD_STRICT=1`; each primitive is wired, scalar-delegated, deleted with proof, or blocked. Gate-exclusion reports are visible and fail closed. |
| W3 | PRUNE-C: Codegen leak prune | Conditional dispatch after W2; blocks W4 if codegen still contains grammar-family workarounds. | Restored gates can see leak paths; JSON 51/51 guard is green before any JSON-adjacent change. | `skinny/crates/codegen/src/grammar_profile.rs`, `skinny/crates/codegen/src/grammar_provider.rs`, `skinny/crates/codegen/src/runtime_generator.rs`, `skinny/crates/codegen/src/json_sink_direct.rs`, `skinny/crates/codegen/src/json_typed_direct.rs`, `skinny/crates/codegen/src/json_templates/`, `skinny/crates/passes/src/lib.rs`, `skinny/crates/ir/src/cost.rs`, `skinny/xtask/src/main.rs`. | Research <=20m, Plan <=15m, Redress <=30m. | No `RuntimeGenerationMode` family split, no static CSS roster, no JSON/CSS recognizers in generic codegen, and no per-grammar workaround disguised as a neutral primitive. If JSON codegen is touched, JSON 51/51 reruns in the same wave. CSS provider/template deletion remains blocked until W5 typed rebuild proof. |
| W4 | PRUNE-D: Pattern H discipline | Conditional dispatch after W2 and W3. It may add generated provenance and non-writing checks, but destructive root runtime deletion is blocked until proof. | Gate roots restored; codegen family leaks pruned; Pattern H count is still 67 but first-line generated ownership is absent. | `skinny/xtask/src/main.rs`, `skinny/xtask/src/regen_simple_runtime.rs`, `skinny/xtask/src/regen_css.rs`, `skinny/xtask/runtime-projections/*.toml`, root runtime files under `crates/core/src/runtime/**`, and generated runtime projections under `skinny/crates/runtime/src/grammars/**`. | Research <=20m, Plan <=15m, Redress <=30m. | All 67 Pattern H files either have first-line generated provenance or are removed from the Pattern H count with proof. A non-writing check and a delete-plus-regen proof must exist before any destructive root runtime delete. CSS `LegacyPath` or similar shims cannot be removed until root runtime projection generation proves the replacement. |
| W5 | REBUILD-E: CSS Value and typed parser rebuild | Conditional dispatch after W1-W4 close or block with no unresolved deletion dependency. This is the first wave allowed to retire live CSS parser/provider artifacts. | CSS claims are diagnostic, gates can see exclusions, codegen leaks are pruned, Pattern H generator ownership is established. | `grammar/css/l4/**`, `skinny/crates/codegen/src/runtime_generator.rs`, `skinny/crates/codegen/src/grammar_provider.rs`, `skinny/crates/runtime/src/grammars/css_l4_*`, `skinny/crates/bbnf-bench/src/css_l4_w8.rs`, CSS tests, CSS gate/report surfaces, `skinny/RESULTS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`. | Research <=20m, Plan <=15m, Redress <=30m. | Typed CSS Value/document/view/visitor output replaces summary/fact-stream evidence. `cssparser` comparison is same-workload. `CSS_GENERATED_RS`, `CssFullParseSummary`, brace-counter summaries, and `Result<String, CssFactError>` live parser contracts may be deleted or retired only in this wave and only if the typed provider proof command lands in the same wave. |
| W6 | REBUILD-F.1: Decision Engine fact, rewrite, and CSP spine | Conditional dispatch after W5. Blocks W7 if facts remain non-driving or grammar-named. | CSS Value rebuild is no longer depending on scaffold facts; gate exclusions and codegen neutrality are restored. | `skinny/crates/passes/src/backend_egraph.rs`, `skinny/crates/passes/src/decision_csp.rs`, `skinny/crates/passes/src/lib.rs`, `skinny/crates/ir/src/cost.rs`, Decision Engine tests, generated-runtime diff fixtures. | Research <=20m, Plan <=15m, Redress <=30m. | E-graph performs at least one asserted rewrite that changes generated runtime behavior or measurable plan selection. CSP is non-tautological and can reject or alter a selected candidate. Facts are grammar-neutral and substrate-backed; no `json_*` or `css_*` fact names drive generic selection. Scaffold status fields cannot close the wave. |
| W7 | REBUILD-F.2: Decision Engine backend lowerers | Conditional dispatch after W6. Blocks W8 only if FNV quarantine evidence depends on Decision Engine production routing. | W6 proves non-driving facts are gone and the selection spine can affect generation. | `skinny/crates/codegen/src/lower/*.rs`, `skinny/crates/codegen/src/lib.rs`, backend shape tests, generated runtime fixtures for eager tape, offset tape, event tape, sink-only, and collapsed-stage or equivalent local lowerers. | Research <=20m, Plan <=15m, Redress <=30m. | All five BackendShape lowerers named by Alpha are real implementation paths, not label strings. Each lowerer has a runtime-relevant generated diff, a test that would fail on the previous scaffold, and no row movement claim without executable proof. |
| W8 | REBUILD-G: FNV quarantine | Conditional dispatch after W7 admission. If W7 intrinsically blocks, W8 needs an updated plan proving independence before dispatch. | JSON 51/51 still green; W11L/N/O FNV evidence remains bench-only and cannot arbitrate runtime equality or selection. | `skinny/xtask/src/real_typed_schema.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, `skinny/crates/codegen/src/json_typed_direct.rs`, bench-only hash helpers, adversarial fixtures. Production roots under `crates/core/src/runtime/**` are scan-only unless a plan proves no migration. | Research <=20m, Plan <=15m, Redress <=30m. | FNV is quarantined to bench/xtask bookkeeping and cannot be a production arbiter, runtime selector, or closed-enum correctness proof. Adversarial fixtures distinguish hash equality from typed semantic equality. Any production FNV use routes to REDRESS unless independently justified by a new Alpha bridge. |
| W9 | Close reconciliation and PASS-IMPL V2 handoff | Conditional dispatch after W1-W8 are admitted, reverted, redressed, or intrinsically blocked. | All dependency-table rows are closed or explicitly blocked; no pending delete/retire row lacks a provider. | `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`, `restart/skinny/tranches/sk-v15/HANDOFF.md`, PASS-IMPL V2 close packet. | Research <=20m, Plan <=15m, Redress <=30m. | RESULTS, REDRESS, and HANDOFF agree on admitted, diagnostic, retired, deleted, and blocked states. PASS-IMPL V2 cites HEAD command output, generated artifacts/diffs, strict checkasm/manifests, and cold evidence. No docs-only close is accepted. |

## §3 - NEW-CH3-V5-01 Dependency Table Obligations

Every wave that deletes or retires an artifact must carry a dependency table with these columns:

| Required column | Meaning |
| --- | --- |
| Retired/deleted artefact | The exact source, generated file, provider, runtime path, result row class, or admission claim being removed. |
| Delete/retire wave | The wave that performs the removal or changes the artifact from live/admitted to retired/diagnostic. |
| Rebuild provider wave | The wave that lands the replacement provider or proof substrate. |
| Proof command | The command that proves the provider exists and the old artifact is no longer live. |
| Evidence provider lands no later than delete/retire | A yes/no assertion with the same-wave or prior-wave provider path named. |
| Conditional status | `allowed`, `blocked`, `diagnostic-only`, or `intrinsic-block`. |

The initial SK-V15 dependency obligations are:

| Retired/deleted artefact | Delete/retire wave | Rebuild provider wave | Proof command | Evidence provider lands no later than delete/retire | Conditional status |
| --- | --- | --- | --- | --- | --- |
| CSS 24-row full-parse admission claim built from one timing tuple | W1 | W1 gate/report telemetry | `gate-json --check-results` or successor gate over rows with `measurement_row_id`, `broadcast_group_id`, `sample_count`, and `row_claim_scope` | Yes; W1 is diagnostic demotion, not provider deletion. | Allowed as diagnostic-only. |
| `CSS_GENERATED_RS` live parser evidence and static CSS companion body | W5 | W5 typed CSS Value provider | `rg -n "CSS_GENERATED_RS|CssFullParseSummary|Result<String, CssFactError)"` over live parser contracts plus CSS typed-value tests and same-workload bench | Must be yes in W5. | Blocked until W5 proof. |
| `CssFullParseSummary`, fact-stream `parse()`, and brace-counter full-parse summary | W5 | W5 typed CSS Value/document/view/visitor provider | CSS semantic tests plus same-workload `cssparser` comparison and live-contract `rg` scan | Must be yes in W5. | Blocked until W5 proof. |
| CSS provider/template/static profile roster and `RuntimeGenerationMode`-style family fanout | W3 for neutralization; W5 for any live CSS provider/template deletion | W3 generic codegen contract plus W5 typed CSS provider for deletion | Lock 14 scan over codegen roots; JSON 51/51 rerun if JSON-adjacent; W5 CSS typed proof for provider deletion | W3 may neutralize fanout; deletion must wait for W5. | Blocked for deletion before W5. |
| Root Pattern H runtime files lacking generated first-line provenance | W4 for provenance retirement; destructive deletion only after W4 proof | W4 root runtime generator/check path | Pattern H count scan, first-line provenance scan, and non-writing root regen check or delete-plus-regen proof | Must be yes before destructive delete. | Allowed for provenance fix; destructive delete blocked until proof. |
| CSS `LegacyPath` or equivalent root runtime shim | W4 only if replacement root runtime projection proof lands; otherwise W5+ | W4 root runtime projection generator/check or W5 typed CSS provider, depending on owner path | `rg -n "LegacyPath|LegacySegment"` over root runtime plus root regen check | Must be yes in same wave. | Blocked without generated replacement proof. |
| Decision Engine scaffold status fields and label-string lowerers | W6 for spine scaffolds; W7 for lowerer scaffolds | W6 e-graph/CSP provider and W7 real lowerers | Decision Engine tests, generated runtime diff, and `rg` scan for scaffold-only status strings | Must be yes in W6/W7. | Conditional on executable diff. |
| W11L/N/O FNV closed-enum or hash-sidecar correctness claim | W8 | W8 adversarial semantic fixtures and bench-only quarantine | `rg -n "fnv|FNV"` over production runtime roots plus adversarial typed-equality tests | Yes for quarantine; no production migration allowed. | Allowed only as quarantine. |

No later wave may erase a dependency-table row by moving it to prose. If the proof command cannot be stated, the delete/retire action is not eligible for dispatch.

## §4 - Wave Falsifiability Gates

W0 fails if the baseline cannot distinguish JSON admitted rows from CSS diagnostic rows, or if gate telemetry does not consume both SK-V8 required fields and SK-V15 anti-broadcast fields.

W1 fails if any CSS admission row still reuses the 24-row broadcast tuple as strict evidence, if comparator workloads remain mismatched, or if provider/parser artifacts are deleted without W5-grade typed proof.

W2 fails if Lock 14 omits known leak roots, if Lock 16 has source-present primitives without strict manifest disposition, or if gate exclusions are still silent.

W3 fails if generic codegen retains grammar-family fanout, static CSS rosters, JSON/CSS recognizers, or per-grammar workarounds. If JSON-adjacent code changes and JSON 51/51 is not rerun in the same wave, the wave fails.

W4 fails if Pattern H closes on header-only edits, if fewer than 67 intended generated files have first-line generated provenance, if root runtime regeneration has no non-writing check path, or if destructive root deletion occurs before delete-plus-regen proof.

W5 fails if CSS Value output is still a string, summary, fact stream, brace count, or CSSOM/workload mismatch. It also fails if old CSS parser artifacts are retired before the typed provider proof lands in the same wave.

W6 fails if the e-graph has zero asserted rewrites, if CSP only preserves a preselected index, if facts are grammar-named, or if no generated runtime behavior can change.

W7 fails if any BackendShape lowerer remains a label string, if tests would pass against the old scaffold, or if row movement is claimed without generated runtime evidence.

W8 fails if FNV is used as a production runtime arbiter, selector, or correctness proof, or if adversarial semantic fixtures cannot distinguish hash equality from typed equality.

W9 fails if RESULTS, REDRESS, HANDOFF, and PASS-IMPL V2 disagree; if any dependency-table row remains unproven; or if close evidence is docs-only rather than HEAD command output, generated diffs, strict manifests, and measurements.

## §5 - Pre-Blocked Routes

The following routes are pre-blocked for SK-V15 dispatch unless a future Alpha bridge reopens them with fresh antecedent evidence:

| Blocked route | Reason |
| --- | --- |
| Deleting CSS providers/templates before typed CSS Value proof | Violates REDRESS-184, REDRESS-209..212, and NEW-CH3-V5-01. |
| Destructive root CSS runtime regeneration before root generator/check proof | Violates REDRESS-183 and REDRESS-213. |
| Reusing CSS SK-V14 24-row full-parse evidence as strict admission | The rows are audit-demoted because one measurement was broadcast and comparator workloads differed. |
| Treating `CSS_GENERATED_RS`, `CssFullParseSummary`, or brace-counter output as typed parser proof | These are the contrivances W1/W5 must retire or replace. |
| Silent Lock 14/16 exclusions | W2 must make exclusions visible and fail closed. |
| Header-only Pattern H closure | Pattern H requires generated first-line provenance and regeneration proof, not marker-only edits. |
| Decision Engine scaffold closure | W6/W7 require executable rewrites, non-tautological CSP, grammar-neutral facts, and real lowerers. |
| FNV production migration | FNV may remain bench/xtask bookkeeping only unless a new bridge proves a production need. |
| Numeric/digit run, EOB, PMULL hot-body, CSSC bulk consumer, x86 promotion | S-P2 V3 rejected these for the current shortlist. |
| Retained structural/cursor/class streams, sidecar lanes, schema builders, harness hashes, stale CSS witnesses | S-P2 V3 excludes these as current survivor surfaces. |
| REDRESS clusters 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96, 97, 98 | These remain rejected or retired unless fresh Alpha/P1 evidence reopens them. |

## §6 - Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`: P3-B scope, W0 requirement, S-P3 output shape, CH lenses, SK-V telemetry binding, wave count, and same-wave consumer rules.
- `restart/prompts/ORCHESTRATOR.md`: §3W critique lenses, §3Z convergence, V<=5 ceiling, and >12-wave escalation.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md`: G-Omega-only close, Alpha receiver goalset, PRUNE-before-REBUILD order, dependency-table obligations, anti-broadcast fields, and gate-exclusion rules.
- `restart/skinny/tranches/sk-v15/HANDOFF.md`: exact SK-V15 blockers, gate posture, and no deletion/retirement before rebuild proof.
- `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`: locked S-P2 survivor boundary and pre-blocked candidate families.
- `restart/skinny/tranches/sk-v15/research/p1/`: P1 parse/direct/typed baselines, PMU ledger, attribution ledger, and CSS audit-demotion inheritance.
- `restart/skinny/tranches/sk-v15/research/p2/`: P2 SOTA, process, host-arch, substrate/tape, parse-that gap, and consolidated candidate verdicts.
- `restart/skinny/tranches/sk-v15/research/alpha-E-candidate-shortlist.md`, `alpha-F-contract-draft.md`, `alpha-C-redress-digest.md`, and `alpha-A-results-extraction.md`: Alpha candidate rows, caps, contracts, redress routing, and CSS/FNV caveats.
- `restart/skinny/tranches/sk-v15/audit-overfit/`: CSS broadcast, lock omissions, codegen/Pattern H, Decision Engine, and FNV overfit audits.
- `skinny/RESULTS.md` and `skinny/REDRESS.md`: current admitted/diagnostic state, historical delete-before-provider rejections, and REDRESS clusters.
- `restart/audit/totality/p1/hardening/V1/CH3.md`: NEW-CH3-V5-01 dependency-table precedent.
- `restart/skinny/tranches/sk-v8/SPEC.md` and `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`: SPEC and P3-B shape.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`: role separation, caps, challenge triggers, same-wave consumer rule, and close discipline.
