# SK-V15 S-P3 V2 Fold Notes - NEW-CH3-V5-01 Dependency And Pre-Block Contract

Role: redeployed CH3 fold support.
Owned output: this file only.
Input packet: S-P3 V1 P3-B, P3-C, P3-E, P3-F, SPEC, DISPATCH, and V1 CH3.
Verdict this note supports: V1 CH3 REVISE -> V2 fold.

## 1. Fold Objective

V2 must make `NEW-CH3-V5-01` executable from the final dispatch surfaces, not merely documented in P3-B. A wave agent reading only `SPEC.md` plus `DISPATCH-PROMPT.md` must see:

1. the canonical W0-W9 wave graph;
2. the canonical dependency table schema;
3. the initial dependency rows;
4. the shared REDRESS/FNV pre-block list;
5. the exact wave exit gates that consume those rows.

P3-B can remain the source ledger, but it cannot be the only place where the table exists.

## 2. Canonical Dependency Table Columns

Promote this exact table into `SPEC.md` after Section 1 or before the wave manifest, and into `DISPATCH-PROMPT.md` before per-wave envelopes:

| Column | Required value |
|---|---|
| `row_id` | Stable identifier, for example `DEP-W1-CSS-BROADCAST` or `DEP-W5-CSS-PARSER-RETIRE`. |
| `retired_or_deleted_artifact` | Exact source, generated file, provider, runtime path, result row class, admission claim, or proof class being demoted, retired, or deleted. |
| `delete_or_retire_wave` | The wave that performs deletion, retirement, diagnostic demotion, or live-admission removal. |
| `rebuild_provider_wave` | The wave that lands the replacement provider, proof substrate, or gate-consumed quarantine. |
| `provider_path_or_claim` | Concrete replacement path, generated provider class, gate proof, or quarantine class. No "future provider" prose. |
| `proof_command` | The command or command family that proves the provider exists and the old artifact is no longer live/admitted. |
| `provider_lands_no_later` | `yes:same-wave`, `yes:prior-wave`, `diagnostic-demotion-only`, or `no:block`. |
| `conditional_status` | One of `allowed`, `blocked`, `diagnostic-only`, `quarantine-only`, or `intrinsic-block`. |
| `consuming_exit_gates` | Wave exits that must check this row before closing. |
| `preblock_cluster` | REDRESS/FNV cluster that would be reopened if the row is ignored. |

`DISPATCH-PROMPT.md` should add a pre-dispatch rejection rule: if a plan deletes, retires, diagnostic-demotes, or removes a live admission claim and no matching dependency row exists in `SPEC.md`, the wave does not dispatch.

## 3. Initial Canonical Rows

These rows should replace or expand the current P3-B table and be copied into SPEC/DISPATCH.

| row_id | retired_or_deleted_artifact | delete_or_retire_wave | rebuild_provider_wave | provider_path_or_claim | proof_command | provider_lands_no_later | conditional_status | consuming_exit_gates | preblock_cluster |
|---|---|---|---|---|---|---|---|---|---|
| `DEP-W1-CSS-BROADCAST` | CSS 24-row live admission claim built from one W8R timing tuple | W1 | W0/W1 telemetry gate | Gate-consumed `measurement_row_id`, `measurement_origin`, `broadcast_group_id`, `value_plane`, and CSS comparator fields | `gate-json` successor over RESULTS/manifest; duplicate measurement signature scan | `diagnostic-demotion-only` | `diagnostic-only` | W0, W1, W5, W9 | REDRESS 215 |
| `DEP-W5-CSS-GENERATED-RS` | `CSS_GENERATED_RS` string-literal parser evidence and byte-identical generated CSS bodies as live parser proof | W5 only | W5 typed CSS Value provider | CSS typed value/document/view/visitor output generated from grammar/provider-free source | `rg -n "CSS_GENERATED_RS|hand_written:CSS_GENERATED_RS"` over live admission paths plus CSS typed tests | `yes:same-wave` | `blocked` before W5 | W1, W3, W5, W9 | REDRESS 183/184/209-213/215 |
| `DEP-W5-CSS-SUMMARY-FACT-STREAM` | `CssFullParseSummary`, brace-counter summary, and fact-stream-only `parse()` as live CSS admission proof | W5 only | W5 typed CSS Value provider | Typed CSS value/document facts on same workload as comparator | CSS semantic equality tests, same-workload `cssparser` run, and `rg -n "CssFullParseSummary|Result<String, CssFactError>|fact_stream|brace"` over CSS live proof paths | `yes:same-wave` | `blocked` before W5 | W1, W5, W9 | REDRESS 215 |
| `DEP-W3-W5-CSS-PROVIDER-TEMPLATE` | CSS provider/template/static profile roster and `RuntimeGenerationMode`/runtime-style family fanout | W3 neutralization; W5 deletion | W3 generic contract; W5 typed provider for deletion | Provider-free grammar metadata generator, then typed CSS provider | Lock 14 scan over codegen roots, generated-output diff, JSON 51/51 rerun if JSON-adjacent, W5 CSS typed proof for deletion | `yes:same-wave` for neutralization; `no:block` for deletion before W5 | `blocked` for deletion before W5 | W2, W3, W5, W9 | REDRESS 184/209-213 |
| `DEP-W4-PATTERN-H-PROVENANCE` | Pattern H root runtime files lacking true line-1 generated provenance | W4 provenance repair; destructive delete only with proof | W4 root runtime generator/check | Non-writing root regen/check or delete-plus-regen generator proof | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l`; line-1 provenance scan; non-writing regen check | `yes:same-wave` for provenance proof; `no:block` for destructive delete without proof | `allowed` for truth repair, `blocked` for fake/header-only close | W4, W9 | REDRESS 183/213 |
| `DEP-W4-W5-CSS-LEGACY-RUNTIME-SHIM` | CSS `LegacyPath`, `LegacySegment`, or equivalent root runtime shim | W4 only with replacement proof; otherwise W5+ | W4 root runtime projection or W5 typed CSS provider | Generated root runtime projection or typed CSS runtime replacement | `rg -n "LegacyPath|LegacySegment"` over root runtime plus root regen/check or typed CSS proof | `yes:same-wave` or `no:block` | `blocked` without replacement proof | W4, W5, W9 | REDRESS 183/213/215 |
| `DEP-W6-DECISION-SPINE` | Decision Engine scaffold status fields, zero-rule e-graph, tautological CSP, and grammar-named facts as load-bearing proof | W6 | W6 e-graph/CSP provider | At least one asserted rewrite, non-tautological CSP, grammar-neutral facts | Decision Engine tests; gate report with `egraph_rewrite_count >= 1`; CSP removal/alteration test; `rg` scan for `json_*`/`css_*` generic facts | `yes:same-wave` | `blocked` until executable diff/proof | W6, W7, W9 | REDRESS 96-98 guard against retained substrate relabel |
| `DEP-W7-BACKEND-LOWERERS` | BackendShape label-string/pass-through lowerer scaffolds | W7 | W7 lowerer implementation | Real EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage lowerers or gate-consumed rejected alternatives | Lowerer tests that fail on old scaffold; generated runtime diff fixtures; `rg` scan for placeholder strings/todo/pass-through shells | `yes:same-wave` | `blocked` until generated runtime proof | W7, W9 | Lock 10/14 guard plus REDRESS 96-98 |
| `DEP-W8-FNV-QUARANTINE` | W11L/W11N/W11O FNV closed-enum or hash-sidecar correctness claim | W8 quarantine only | W8 adversarial semantic fixtures and bench-only metadata | Bench/xtask-only FNV bookkeeping, independent non-enum typed semantic equality | `rg -n "fnv|FNV"` over production runtime/generic codegen roots plus adversarial typed-equality tests | `yes:same-wave` for quarantine; production migration is `no:block` | `quarantine-only` | W8, W9 | FNV closed-enum production migration |
| `DEP-W9-CLOSE-NO-ORPHANS` | Any dependency row without proof, REDRESS route, revert evidence, or intrinsic-block proof | W9 | W1-W8 row owners | Close ledger with admitted/diagnostic/retired/deleted/blocked state for every dependency row | PASS-IMPL V2 audit plus SPEC dependency-row checklist | `yes:prior-wave` or `intrinsic-block` | `blocked` if orphan remains | W9 | All clusters |

## 4. REDRESS Pre-Block Normalization

Use one shared list in P3-A/P3-B/P3-C/P3-E/P3-F/SPEC/DISPATCH. The same labels and cluster boundaries must appear in each surface.

| Canonical cluster | Normalized block text |
|---|---|
| REDRESS 28+33 | No tiny-string/Class A NEON replay, no scalar early-out deletion, and no old tiny-string route under a new helper name. |
| REDRESS 50-55 | No parser-owned side tables, byte-class cursor sidecars, decoded stats, quote-source materializers, fused decoded hashes, or second scanners. |
| REDRESS 60-72 | No retained parse shortcuts, eager/direct materializer repeats, wide-string shortcut relabels, object next-key carry, or global Track 2/direct cap changes. REDRESS 72 remains only its specific scalar cap-16 retained split, not a new authorization. |
| REDRESS 80 | No mantissa-widen, f64 fallback, UDOT digit, or number-policy route without fresh P1 evidence naming a BBNF-side numeric hot leaf and same-wave consumer. |
| REDRESS 82-84 | No one-quartet Unicode production promotion, StringBlock16 tiny probe replay, or object-pair/value-byte compaction route under old framing. |
| REDRESS 88 | No PMULL prefix-XOR production hot-body promotion from ISA/checkasm availability alone. |
| REDRESS 89 | No CSSC CTZ or bulk-consumer production promotion from ISA/checkasm availability alone. |
| REDRESS 96-98 | No retained class columns, streaming structural cursors, public `UnionTape`, density/whitespace sidecars, sidecar projection, second tape, or union-substrate relabel. Same-call local masks that write the existing tape/sink remain eligible only with scalar/parity and same-wave row movement. |
| REDRESS 183/184/209-213 | No provider/runtime/template deletion before replacement proof. No static centralization of committed generated text as "generation". No CSS runtime projection deletion before root generator/check proof. |
| REDRESS 215 | No CSS one-measurement-to-24-row broadcast admission, brace-counter/full-parse-summary admission, fact-stream-only Value API claim, or wrong-plane cssparser/lightningcss comparison. |
| REDRESS 242-247 | No decoded-string, decoded-codepoint, fixed-shape Unicode floor, indexed-string, structural-stream, or string64 retry under old framing. These must appear in P3-B and P3-E, not only SPEC/P3-F. |
| FNV migration | No W11L/W11N/W11O FNV-keyed closed-enum arbiter, closed-enum sidecar, digest relabel, or production runtime/generic codegen migration. Bench-only quarantine requires independent non-enum adversarial fixtures. |

P3-C currently names 210-213 in the provider-deletion row; fold it to `183/184/209-213`. P3-E currently omits 242-247 from the main binding list; add it in Section 1, global pre-blocks, relevant wave pre-blocks, and Section 3 fail conditions. P3-B currently omits 242-247 from its pre-block table; add it there and cite it in W5/W8/W9 where relevant.

## 5. Required Wave Exit-Gate Consumption

The table must be consumed by wave exits, not only by pre-dispatch prose.

| Wave | Required dependency/pre-block consumption |
|---|---|
| W0 | Must initialize dependency-row tracking and telemetry fields used by `DEP-W1-CSS-BROADCAST`: `measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and `broadcast_group_id`. Exit rejects alias-only P3-B field names unless schema mapping is deliberately versioned and gate-consumed. |
| W1 | Must consume `DEP-W1-CSS-BROADCAST`; may demote or collapse CSS admission claims but cannot delete `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream `parse()`, or live CSS providers unless the W5-grade typed provider proof already exists in the same wave, which should normally be false. |
| W2 | Must consume pre-block clusters for REDRESS 88, 89, 96-98, and gate-exclusion language. Exit rejects any Lock 14/16 scan that hides included roots, excluded roots, exclusion reasons, or the exclusion list itself. |
| W3 | Must consume `DEP-W3-W5-CSS-PROVIDER-TEMPLATE`. Exit may neutralize generic fanout, but any live CSS provider/template deletion remains blocked until W5 typed proof. Also consumes REDRESS 183/184/209-213 and 242-247 if string/structural generation routes are touched. |
| W4 | Must consume `DEP-W4-PATTERN-H-PROVENANCE` and `DEP-W4-W5-CSS-LEGACY-RUNTIME-SHIM`. Exit rejects header-only generated status, fake provenance, and destructive root runtime delete without root generator/check or typed-provider replacement proof. |
| W5 | Must consume `DEP-W5-CSS-GENERATED-RS`, `DEP-W5-CSS-SUMMARY-FACT-STREAM`, `DEP-W3-W5-CSS-PROVIDER-TEMPLATE`, `DEP-W4-W5-CSS-LEGACY-RUNTIME-SHIM`, and REDRESS 215. Exit rejects any old CSS proof still serving as live admission and any typed CSS admit that reuses W8R broadcast numbers as a floor rather than a diagnostic negative fixture. |
| W6 | Must consume `DEP-W6-DECISION-SPINE` plus REDRESS 96-98. Exit rejects scaffold facts, grammar-named generic facts, retained substrate relabels, and CSP/e-graph proof that cannot alter selection or generated behavior. |
| W7 | Must consume `DEP-W7-BACKEND-LOWERERS` plus REDRESS 96-98. EventTape must be explicitly bound as a BackendShape lowerer over accepted substrate only; it cannot create a sixth shape, sidecar event vector, retained parser stream, public substrate API, or alternate document projection. |
| W8 | Must consume `DEP-W8-FNV-QUARANTINE` and the FNV migration pre-block. Exit rejects production FNV hits unless routed under a future contract and rejects strict-product proof coupled to Track 1's closed enum. |
| W9 | Must consume every dependency row. Exit rejects any row whose `conditional_status` is not closed by proof, REDRESS, revert evidence, or intrinsic-block proof. PASS-IMPL V2 must cite the table state. |

## 6. Surface-Specific Fold Recipe

| Surface | Required V2 fold |
|---|---|
| P3-B | Keep as source ledger, but replace W0 alias telemetry names with exact P3-D/SPEC fields. Add rows/columns above. Add REDRESS 242-247 and FNV migration to the pre-block table. |
| P3-C | Rebase from old expected wave set to W0-W9. Split old REBUILD-F into W6 Decision Engine spine and W7 BackendShape lowerers; move FNV to W8; add W9 close. Add a rule in W1/W3/W4/W5/W9 exits that delete/retire/demotion fails without a matching dependency row. |
| P3-E | Add 242-247 to the binding historical list, global table, Section 3 fail conditions, and fresh-P1 table. Normalize `183/184/209..213` spelling to `183/184/209-213`. |
| P3-F | State that SPEC and DISPATCH now carry the dependency table, not only the rule. Mention W0-W9, normalized pre-blocks, and table-consuming exits. |
| SPEC | Add the canonical dependency table and row set. In Sections 3-12, append explicit "Dependency rows consumed" bullets using the wave list above. Keep Section 13's pre-block list identical to P3-E. |
| DISPATCH | Add the canonical dependency table schema or a compact mirror plus a pointer to SPEC. Add a pre-dispatch fail condition and per-wave envelope checks requiring the relevant rows. |

## 7. V2 CH3 Accept Check

CH3 can accept the V2 fold only if all of these greps/read checks pass:

```text
rg -n "P3-B does not exist|expected wave set" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
rg -n "DEP-W1-CSS-BROADCAST|DEP-W5-CSS-GENERATED-RS|DEP-W8-FNV-QUARANTINE" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "242-247" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "183/184/209-213|REDRESS 183, 184, 209-213" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/SPEC.md
rg -n "EventTape.*sidecar|sixth shape|retained parser" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

The first command should return no stale-wave hits. The remaining commands must show the dependency rows, normalized pre-blocks, and EventTape anti-sidecar binding in final dispatch surfaces.
