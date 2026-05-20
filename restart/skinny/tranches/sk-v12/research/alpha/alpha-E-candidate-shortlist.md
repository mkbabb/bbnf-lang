# SK-V12 Alpha-E Candidate Shortlist

Pass: Alpha SK-V11 -> SK-V12, alpha-E.
Date: 2026-05-20.
Output: candidate intervention shortlist only.

## Read Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v11/research/close/close-redress.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- Supporting cohort context from SK-V11 P2/P3 and W1b/W8 research

## Routing Summary

SK-V11 closed as a measured fixpoint under REDRESS 120, not as direct `GO` and
not as a grammar-generalization win. REDRESS 119 is now the authority for the
13 JSON direct residual rows. REDRESS 112 and REDRESS 113 are the material
blockers for SK-V12: W1b could not create a generated non-JSON baseline because
runtime emission remains JSON-profiled through `json_provider`, and W2 could
not create the first measurable non-JSON row inside an intervention wave.

Therefore the shortlist prioritizes generated non-JSON baseline/codegen/runtime
proof before any JSON micro-wave. No numeric, string, escape, digest, or
container-tail JSON retry is shortlisted as a standalone candidate. The only
JSON direct entry below is conditional and evidence-gated: it may dispatch only
after a generated non-JSON baseline/proof has landed and only with fresh
post-REDRESS-119 material evidence that is not the rejected REDRESS 114-118
route family under a new name.

## Alpha-Level Cost, Cap, And Revert Matrix

This matrix is a Pass Alpha seed for S-P3; it does not create `SPEC.md`.
S-P3 may tighten these caps, but widening them requires CHALLENGE and user
escalation before behavior redress.

| Candidate | Intended wave slot | LOC budget | Risk | Plan cap | Redress cap | Same-wave consumer | Split-before-dispatch rule | Revert protocol |
|---|---|---:|---|---:|---:|---|---|---|
| E1 CSS L4 baseline | W1 preferred baseline | <=520 handwritten + selected generated output | high | 30 min | 75 min | generated CSS row + non-JSON gate report | split if selected-grammar preflight cannot prove generated emission seam, runtime target, oracle, fixture corpus, compile/equality smoke, and gate consumption | revert codegen/runtime/bench/report/gate/RESULTS as one slice; save `/tmp/skv12-waveW1-rejected.patch`; REDRESS blocks E4 until a baseline admits |
| E2 Sheets baseline | W1 fallback baseline | <=480 handwritten + selected generated output | medium-high | 30 min | 75 min | generated Sheets row + non-JSON gate report | same preflight; dispatch only if CSS is blocked or rejected with evidence | revert selected Sheets codegen/runtime/bench/report/gate/RESULTS as one slice; save rejected patch; dependent intervention uses Sheets variant only after admit |
| E3 BBNF-self baseline | W1 fallback baseline | <=460 handwritten + selected generated output | medium-high | 30 min | 75 min | generated BBNF-self row + non-JSON gate report | same preflight; dispatch only if CSS and Sheets are blocked or rejected, or S-P1 proves smaller runnable path | revert selected BBNF-self codegen/runtime/bench/report/gate/RESULTS as one slice; save rejected patch; dependent intervention uses BBNF-self variant only after admit |
| E4 selected-baseline intervention | W2 after E1/E2/E3 admit | <=430 handwritten + selected generated output | high | 30 min | 75 min | same generated non-JSON row + non-JSON gate report | reject before redress if no admitted baseline row id and baseline Mbps exist | revert intervention codegen/runtime/bench/report/gate/RESULTS as one slice; save rejected patch; preserve baseline REDRESS evidence |
| E5 JSON direct companion | W3+ conditional companion | <=300 handwritten + selected generated JSON output | high | 30 min | 75 min | selected JSON direct Track 1 + independent Track 2 + `gate-json` | hard REJECT if scheduled before E4 admits or blocks the non-JSON priority; split if more than one JSON row is selected without pre-dispatch microbench proof | revert JSON source/generated/bench/report/gate/RESULTS as one slice; save rejected patch; never demote non-JSON evidence |

Every failed behavior wave records a numbered REDRESS entry and preserves the
rejected patch. Any candidate that exceeds its LOC budget or 75-minute redress
cap returns REVISE before behavior dispatch.

## Baseline Entry Preflight

Before S-P3 may dispatch E1, E2, or E3 as a behavior redress wave, the selected
grammar must pass a read-only or proof-only preflight:

- a named generated-emission seam or generated per-grammar runtime path exists
  for exactly one selected grammar;
- the path does not route through JSON-only
  `json_provider::ensure_runtime_profile` as generality proof;
- one fixture corpus and independent same-plane oracle are runnable;
- compile/equality smoke can pass without behavior source edits;
- the REDRESS 111 non-JSON report lane can consume the row evidence without
  producer-only telemetry.

If this preflight fails, S-P3 must split into a generator/runtime unblock wave
followed by a baseline-report wave, or record a measured `BLOCKED` route.

## Shortlist

### E1: CSS L4 Generated Baseline And Oracle Lane

- **Priority:** 1.
- **Purpose:** create the missing generated non-JSON baseline authority that
  REDRESS 112 proved absent.
- **Owner paths:**
  - `skinny/crates/codegen/src/lib.rs`
  - `skinny/crates/codegen/src/lower/`
  - `skinny/crates/codegen/src/direct_schema.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  - `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
  - `skinny/crates/bbnf-bench/src/metadata.rs`
  - `skinny/crates/bbnf-bench/benches/`
  - `grammar/css/l4/values.bbnf`
  - `grammar/css/l4/value-unit.bbnf`
  - `grammar/css/l4/color.bbnf`
  - `restart/skinny/tranches/sk-v12/research/w1-css-baseline/`
- **Scalar/oracle status:** not currently admitted. W1a proved only the
  non-JSON gate/report schema. This candidate must add generated CSS L4 Track
  1 plus an independent same-plane oracle over declaration-value fact bytes;
  the oracle must not call generated Track 1, generated SinkOnly helpers,
  generated typed helpers, `sheets_witness`, or benchmark-private parser code.
- **Checkasm/parity status:** scalar-only by default; no checkasm required
  unless the implementation introduces a SIMD helper. Strict product parity is
  required: generated fact bytes must equal the independent oracle bytes for
  every fixture in the selected corpus.
- **Same-wave consumer:** the generated CSS L4 benchmark row and
  `bbnf-bench --bin gate` non-JSON report consumer must land in the same wave.
  A report that merely produces fields without gate consumption rejects.
- **Falsifiability gate:**

  | Row | Threshold |
  |---|---:|
  | `css_l4/declaration_values/direct/main` generated Track 1 | finite positive Mbps |
  | `css_l4/declaration_values/direct/main` independent oracle/Track 2 | finite positive Mbps |
  | `css_l4/declaration_values/direct/main` strict equality | 100% fixture equality |
  | `css_l4/declaration_values/direct/main` sample count | recorded from bench artifact |

  Additional gate conditions: the report records run id, host, flags, feature
  mask, output plane, oracle source, and source provenance; JSON `skinny/RESULTS.md`
  rows do not move; `gate-json --with-cost-facts --check-results` remains green.
- **LOC budget:** <= 520 handwritten source/test/gate LOC, plus regenerated
  output only for the selected CSS L4 runtime module.
- **Hard cap / revert:** see Alpha-Level Cost, Cap, And Revert Matrix.
- **Risk:** high. This is first-of-class runtime/codegen proof and directly
  crosses the REDRESS 112 blocker.
- **Pre-blocked route adjacency:** REDRESS 111 may be reused only as schema
  consumption, not baseline authority; REDRESS 112 blocks routing through
  `json_provider::ensure_runtime_profile` as a generality proof; REDRESS 113
  blocks combining first baseline creation with the intervention wave; Lock 14
  blocks JSON policy in generic crates or runtime outside generated per-grammar
  modules.

### E2: Sheets Formula Generated Baseline Fallback

- **Priority:** 2, fallback if E1 cannot make a positive generated CSS L4
  baseline inside the SK-V12 budget.
- **Purpose:** preserve the Pass Alpha preferred grammar order while giving
  SK-V12 a second generated non-JSON baseline target that is not a JSON direct
  micro-wave.
- **Owner paths:**
  - `skinny/crates/codegen/src/lib.rs`
  - `skinny/crates/codegen/src/lower/`
  - `skinny/crates/codegen/src/direct_schema.rs`
  - `skinny/crates/runtime/src/grammars/sheets_formula/`
  - `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
  - `skinny/crates/bbnf-bench/benches/`
  - `grammar/google-sheets/google-sheets.bbnf`
  - `restart/skinny/tranches/sk-v12/research/w1-sheets-baseline/`
- **Scalar/oracle status:** not currently admitted. Existing
  `sheets_witness` inventory is not generated Track 1 authority. This
  candidate must generate one Sheets formula direct baseline and an
  independent formula oracle on the same output plane.
- **Checkasm/parity status:** scalar-only by default. Product parity requires
  exact output equality against the independent oracle. If a byte-set or
  string helper is routed, strict scalar differential/checkasm becomes
  mandatory before row evidence counts.
- **Same-wave consumer:** generated Sheets formula row plus the non-JSON gate
  report consumer in the same wave; no producer-only report and no witness-only
  admission.
- **Falsifiability gate:**

  | Row | Threshold |
  |---|---:|
  | `sheets/formula/direct/main` generated Track 1 | finite positive Mbps |
  | `sheets/formula/direct/main` independent oracle/Track 2 | finite positive Mbps |
  | `sheets/formula/direct/main` strict equality | 100% fixture equality |
  | `sheets/formula/direct/main` sample count | recorded from bench artifact |

  The row must validate under the non-JSON gate with `grammar_id=sheets` and
  `domain=sheets_bench`; JSON rows remain unchanged.
- **LOC budget:** <= 480 handwritten source/test/gate LOC, plus regenerated
  output only for the selected Sheets formula module.
- **Hard cap / revert:** see Alpha-Level Cost, Cap, And Revert Matrix.
- **Risk:** medium-high. It avoids the CSS-specific blocker but still has the
  same generated runtime/codegen boundary risk as REDRESS 112.
- **Pre-blocked route adjacency:** REDRESS 112 blocks `sheets_witness` or
  JSON-provider emission as baseline authority; REDRESS 113 blocks creating
  the baseline inside a later intervention wave; REDRESS 118 blocks treating a
  non-JSON host-sink or digest-only row as generated parser evidence.

### E3: BBNF-Self Generated Baseline Fallback

- **Priority:** 3, fallback if CSS L4 and Sheets cannot produce one generated
  non-JSON baseline with an independent oracle.
- **Purpose:** use the project grammar itself as a small generated-parser
  baseline target while still satisfying Lock 14 through measured non-JSON
  Track 1 rather than prose.
- **Owner paths:**
  - `skinny/crates/codegen/src/lib.rs`
  - `skinny/crates/codegen/src/lower/`
  - `skinny/crates/runtime/src/grammars/bbnf_self/`
  - `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
  - `skinny/crates/bbnf-bench/benches/`
  - `grammar/bbnf/bbnf.bbnf`
  - `grammar/bbnf/expressions.bbnf`
  - `grammar/bbnf/types.bbnf`
  - `restart/skinny/tranches/sk-v12/research/w1-bbnf-self-baseline/`
- **Scalar/oracle status:** open. The wave must define a generated BBNF-self
  direct Track 1 output plane and an independent grammar-file oracle that does
  not share generated parser code.
- **Checkasm/parity status:** scalar/product parity only; checkasm is N/A
  unless a byte classifier or SIMD scan is added, which should be avoided in
  the baseline wave.
- **Same-wave consumer:** generated BBNF-self parser benchmark plus non-JSON
  report/gate consumer in the same wave.
- **Falsifiability gate:**

  | Row | Threshold |
  |---|---:|
  | `bbnf_self/grammar/direct/main` generated Track 1 | finite positive Mbps |
  | `bbnf_self/grammar/direct/main` independent oracle/Track 2 | finite positive Mbps |
  | `bbnf_self/grammar/direct/main` strict equality | 100% fixture equality |
  | `bbnf_self/grammar/direct/main` sample count | recorded from bench artifact |

  The row must validate under the non-JSON gate with
  `grammar_id=bbnf_self` and `domain=bbnf_self_bench`; JSON rows remain
  unchanged.
- **LOC budget:** <= 460 handwritten source/test/gate LOC, plus regenerated
  output only for the selected BBNF-self module.
- **Hard cap / revert:** see Alpha-Level Cost, Cap, And Revert Matrix.
- **Risk:** medium-high. Smaller grammar surface may lower runtime work only if
  S-P1 proves a materially smaller runnable generated Track 1 and independent
  oracle path; otherwise it crosses the same REDRESS 112 codegen/runtime
  boundary as E1/E2.
- **Pre-blocked route adjacency:** REDRESS 112 blocks treating inventory or
  grammar parser internals as generated Track 1; REDRESS 113 blocks baseline
  creation inside an intervention wave; Lock 1 blocks hidden sidecar/parser
  substrate; Lock 14 blocks generic JSON policy.

### E4: Selected Generated FIRST/Prefix Intervention

- **Priority:** 4, only after E1, E2, or E3 admits a concrete generated
  non-JSON baseline row and records `W1_selected_baseline_mbps`.
- **Purpose:** convert the first generated non-JSON baseline into an admitted
  generated non-JSON intervention using grammar metadata rather than JSON role
  policy.
- **Owner paths:**
  - `skinny/crates/codegen/src/lower/sink_only.rs`
  - `skinny/crates/codegen/src/lower/schema_direct.rs`
  - `skinny/crates/codegen/src/direct_schema.rs`
  - `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
  - `skinny/crates/runtime/src/grammars/sheets_formula/`
  - `skinny/crates/runtime/src/grammars/bbnf_self/`
  - `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
  - `skinny/crates/bbnf-bench/benches/`
  - `grammar/css/l4/values.bbnf`
  - `grammar/css/l4/value-unit.bbnf`
  - `grammar/css/l4/color.bbnf`
  - `grammar/google-sheets/google-sheets.bbnf`
  - `grammar/bbnf/bbnf.bbnf`
  - `restart/skinny/tranches/sk-v12/research/w2-selected-intervention/`
- **Scalar/oracle status:** depends on the admitted baseline. The selected
  baseline's generated Track 1 plus independent oracle become the
  scalar/product reference. E4 may add FIRST-set, prefix-trie, or lookahead
  dispatch only from grammar metadata.
- **Checkasm/parity status:** N/A for scalar generated dispatch. If E4 consumes
  byte-set or movemask support, strict scalar differential/checkasm is required
  for the exact same-wave consumer before benchmark evidence counts.
- **Same-wave consumer:** the selected generated non-JSON direct or typed
  parser row and the non-JSON gate/report consumer. The intervention cannot be
  a helper-only change.
- **Variants:**
  - CSS L4 declaration values: preferred if E1 admitted.
  - Sheets formula: fallback if E2 admitted; threshold uses
    `W1_sheets_baseline_mbps`.
  - BBNF-self grammar: fallback if E3 admitted; threshold uses
    `W1_bbnf_self_baseline_mbps`.
- **Falsifiability gate:**

  | Row | Threshold |
  |---|---:|
  | selected generated non-JSON row Track 1 | >= `ceil(W1_selected_baseline_mbps * 1.01)` |
  | selected generated non-JSON row independent oracle/Track 2 | finite positive Mbps |
  | selected generated non-JSON row strict equality | 100% fixture equality |

  If JSON reports are refreshed as companions, all direct guard floors from
  SK-V11 SPEC Section 0.5 hold and no JSON residual row is admitted by
  analogy.
- **LOC budget:** <= 430 handwritten source/test/gate LOC, plus regenerated
  output only for the selected non-JSON module.
- **Hard cap / revert:** see Alpha-Level Cost, Cap, And Revert Matrix.
- **Risk:** high. It is the first actual non-JSON generated intervention and
  must avoid folding W1 baseline creation and W2 intervention into one paper
  close.
- **Pre-blocked route adjacency:** REDRESS 113 blocks dispatch before a
  positive baseline exists; REDRESS 112 blocks JSON-provider emission as proof;
  REDRESS 111 blocks schema-only evidence as intervention evidence; W3/W4/W5
  JSON micro-wave results do not count for this non-JSON admission.

### E5: Conditional JSON Direct Companion From A Non-JSON-Proven Template

- **Priority:** 5, conditional and lowest priority. This is the only JSON
  direct candidate in the shortlist.
- **Purpose:** allow a JSON direct row to be reconsidered only if the successful
  E1/E4 non-JSON generated template creates fresh material evidence beyond
  REDRESS 114-119.
- **Fresh material evidence required before dispatch:**
  - a post-E4 profile showing the same grammar-neutral generated template is a
    hot direct consumer on JSON, not `number_span_emit_slot`,
    `container_tail_next`, bounded string span, escaped source fold, or
    output-digest host sink under a new name;
  - same-host caller microbench on the selected JSON row showing the template
    can plausibly close both Track 1 and independent Track 2 floors before
    production wiring;
  - proof that the same source delta has already been consumed by a generated
    non-JSON row, so this is not a JSON-only micro-wave.
- **Owner paths:**
  - `skinny/crates/codegen/src/lower/sink_only.rs`
  - `skinny/crates/codegen/src/lower/schema_direct.rs`
  - `skinny/crates/runtime/src/grammars/json/generated.rs`
  - `skinny/crates/bbnf-bench/src/direct_struct.rs`
  - `skinny/crates/bbnf-bench/src/track2/json.rs`
  - `skinny/crates/bbnf-bench/src/bin/gate.rs`
  - `skinny/crates/bbnf-bench/src/report.rs`
  - `skinny/crates/bbnf-bench/benches/json_parity.rs`
  - `skinny/RESULTS.md`
  - `skinny/REDRESS.md`
  - `restart/skinny/tranches/sk-v12/research/w-json-template-companion/`
- **Scalar/oracle status:** current generated JSON direct and independent
  Track 2 are the references, but REDRESS 119 treats the residual rows as
  exhausted. This candidate needs the fresh non-JSON-proven template proof
  above before those references can be used for a new JSON row attempt.
- **Checkasm/parity status:** scalar/product parity by default. SIMD/checkasm
  is disallowed unless the non-JSON wave already consumed the same SIMD helper
  with strict parity and the JSON consumer repeats strict scalar differential
  on its row.
- **Same-wave consumer:** generated JSON direct Track 1 plus independent Track
  2 for the same selected row, with `gate-json` consuming provenance and row
  movement in the same wave.
- **Falsifiability gate:**

  | Row | Threshold |
  |---|---:|
  | `github_events/direct_to_struct` | Track 1 and Track 2 >= 13403 Mbps |
  | `update_center/direct_to_struct` | Track 1 and Track 2 >= 10059 Mbps |
  | `random/direct_to_struct` | Track 1 and Track 2 >= 7878 Mbps |
  | `canada/direct_to_struct` | Track 1 and Track 2 >= 10637 Mbps |

  Select at most one row unless the pre-dispatch microbench shows both tracks
  clearing floor on multiple rows. Existing direct and typed guard floors from
  SK-V11 SPEC Section 0.5 hold.
- **LOC budget:** <= 300 handwritten source/test/gate LOC, plus regenerated
  JSON output only for selected generated callers.
- **Hard cap / revert:** see Alpha-Level Cost, Cap, And Revert Matrix.
- **Risk:** high. It is adjacent to every SK-V11 direct rejection and should
  be rejected at plan time if the fresh evidence is missing.
- **Pre-blocked route adjacency:** REDRESS 114 blocks numeric slot retries;
  REDRESS 115 blocks container-tail/direct-dispatch retry without material
  difference; REDRESS 116 blocks bounded string span without accepted parity
  and Track 2 cost evidence; REDRESS 117 blocks decoded source-fold routes;
  REDRESS 118 blocks digest/host-sink recovery; REDRESS 119 blocks docs-only
  direct row reclamation; REDRESS 120 prioritizes generated non-JSON baseline
  first.

## Explicitly Not Shortlisted

- JSON-only numeric, string, escape, dispatch, and digest micro-waves without
  fresh non-JSON-proven material evidence.
- Parse-only SOTA movement.
- x86 implementation work.
- JSON-provider emission as generic runtime proof.
- Old hand non-JSON runtimes, witness modules, sidecars, or gate fixtures as
  generated Track 1 baseline authority.
- Any telemetry field that is not consumed by a same-wave gate/report consumer.
