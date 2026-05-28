---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V4
disposition: ACCEPT
generated_at: 2026-05-28T06:12:11Z
files_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V3/CH5.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
score: "5/5 ACCEPT"
---

# CH5 Hidden Coupling - SK-V15 T-P1 V4

## Verdict

ACCEPT.

V4 discharges the V3 CH5 fold. The live `1F-coherence-scan.md` now carries a
current-source FNV production/hash coupling row for generated CSS runtimes and
the codegen template, and it classifies that surface as hash-sidecar /
telemetry coupling, not CSS Value API proof, retained document identity,
same-substrate evidence, or a production equality arbiter
(`restart/audit/totality/p1/1F-coherence-scan.md:89`). The expanded sidecar
guard includes `input_fnv64`, `stream_fnv64`, `fn fnv64`, and `fnv64(` before
any substrate or CH5 close claim (`restart/audit/totality/p1/1F-coherence-scan.md:128`,
`restart/audit/totality/p1/1F-coherence-scan.md:173`,
`restart/audit/totality/p1/1F-coherence-scan.md:184`).

This is an ACCEPT because the coupling is no longer hidden or laundered. It is
not a closure proof: W10 still owns the quarantine and production guard work
(`restart/audit/totality/p1/1D-skinny-lessons.md:181`,
`restart/audit/totality/p1/1D-skinny-lessons.md:230`).

## Evidence

- CH5 authority is Lock 1 / hidden-coupling review: no parallel substrate,
  sidecar producer, renamed-scanner violation, or Track 1 == Track 2
  dishonesty may pass uncatalogued (`restart/prompts/totality/PASS-1-EXCAVATION.md:125`,
  `restart/prompts/ORCHESTRATOR.md:87`).

- The V4 dispatch specifically asks CH5 to verify the FNV current-source census
  and expanded sidecar/hash grep guard against generated CSS runtimes and
  codegen template sites without laundering hashes into CSS Value API,
  substrate, or equality proof
  (`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:65`-`68`).

- V3 required exactly this fold: add a current-source FNV production-coupling
  row for generated CSS runtime `input_fnv64` / `fnv64` surfaces and template
  sites, classify the fields, and extend the guard to
  `input_fnv64|stream_fnv64|fn fnv64|fnv64(`
  (`restart/audit/totality/p1/hardening/V3/CH5.md:149`,
  `restart/audit/totality/p1/hardening/V3/CH5.md:153`-`169`;
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:29`,
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:40`-`41`).

- `1F` records the V4 fold in frontmatter and table state:
  `CH5-V3-F04` / `COH-016-current-CSS-FNV-runtime-hash-coupling`
  (`restart/audit/totality/p1/1F-coherence-scan.md:40`,
  `restart/audit/totality/p1/1F-coherence-scan.md:57`) and later states the
  fold landed (`restart/audit/totality/p1/1F-coherence-scan.md:183`-`184`).

- The current-source census resolves across all seven generated CSS runtime
  files and the template:

  | surface | `input_fnv64` rows | `fnv64` helper |
  |---|---|---|
  | `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs:619` |
  | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:619` |
  | `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs:619` |
  | `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs:619` |
  | `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:619` |
  | `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs:619` |
  | `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs` | `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:25`, `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:619` |
  | template | `skinny/crates/codegen/src/runtime_generator.rs:737`, `skinny/crates/codegen/src/runtime_generator.rs:783` | `skinny/crates/codegen/src/runtime_generator.rs:1331` |

- The widened guard reaches the source paths that matter. Running
  `rg -n "input_fnv64|stream_fnv64|fn fnv64|fnv64\\(" skinny/crates/runtime/src/grammars skinny/crates/codegen/src/runtime_generator.rs`
  finds the generated runtime and template lines above. Running the broader
  `1F` guard over `skinny/crates crates/core/src` also catches comparator
  source-sidecars at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:648`,
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:1082`, and
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:2691`, plus hash fixture
  / stream surfaces such as
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:131`,
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:144`,
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:2994`,
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3072`, and
  `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3412`.

- The inventories do not launder the hash surface into a CSS admit. CSS typed
  value/document/view/visitor remains unimplemented
  (`restart/audit/totality/p1/1D-skinny-lessons.md:157`), 1C keeps CSS as a
  partial admitted output plane with unresolved schema rather than a value API
  (`restart/audit/totality/p1/1C-runtime-evidence.md:65`,
  `restart/audit/totality/p1/1C-runtime-evidence.md:112`), and 1E keeps Lock 1
  partial / JSON-tape-only while saying string-only CSS fact streams are not
  value API evidence (`restart/audit/totality/p1/1E-locks-evidence.md:90`,
  `restart/audit/totality/p1/1E-locks-evidence.md:130`).

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH5-V4-001 | ACCEPT | The V3 current-source FNV census fold landed in the live V4 inventory. | `1F` adds `COH-016` and cites generated CSS runtime `input_fnv64` / `fnv64` plus template sites (`restart/audit/totality/p1/1F-coherence-scan.md:89`). Source verification finds all seven generated CSS `generated.rs` files with `input_fnv64` at `:25` and `:71` and `fn fnv64` at `:619`, plus template sites `skinny/crates/codegen/src/runtime_generator.rs:737`, `:783`, and `:1331`. |
| CH5-V4-002 | ACCEPT | The sidecar/hash grep guard is expanded far enough to catch generated CSS runtime hashes, codegen template hashes, comparator source-sidecars, and stream-hash fixtures. | `1F` requires `rg -n 'EventCursor|generated_eventcursor|structural_offsets|TapeAssembler|fixture_sidecar_facts|same-plane-source-sidecar|input_fnv64|stream_fnv64|fn fnv64|fnv64\\(' skinny/crates crates/core/src` before CH5/substrate close (`restart/audit/totality/p1/1F-coherence-scan.md:128`, `restart/audit/totality/p1/1F-coherence-scan.md:173`, `restart/audit/totality/p1/1F-coherence-scan.md:184`). That scope includes both `skinny/crates/runtime/src/grammars/.../generated.rs` and `skinny/crates/codegen/src/runtime_generator.rs`. |
| CH5-V4-003 | ACCEPT | The FNV hashes are not laundered into CSS Value API, retained document identity, same-substrate evidence, or production equality proof. | `1F` states those negatives explicitly (`restart/audit/totality/p1/1F-coherence-scan.md:89`) and routes the owner/receiver to W10 quarantine (`restart/audit/totality/p1/1F-coherence-scan.md:142`, `restart/audit/totality/p1/1F-coherence-scan.md:165`). `1D` requires production scan plus adversarial strict-product fixtures and says the disposition is bench-only quarantine or delete, with no production equality arbiter (`restart/audit/totality/p1/1D-skinny-lessons.md:181`, `restart/audit/totality/p1/1D-skinny-lessons.md:211`). |
| CH5-V4-004 | ACCEPT | Lock 1 / substrate-union coupling remains scoped and open where evidence is partial; no inventory claims FNV or CSS fact-stream hashes prove the substrate union. | 1A keeps the direct/tape union partial and shared schedule UNKNOWN (`restart/audit/totality/p1/1A-substrate-evidence.md:67`, `restart/audit/totality/p1/1A-substrate-evidence.md:84`, `restart/audit/totality/p1/1A-substrate-evidence.md:172`-`174`). EventTape remains unimplemented and fenced against retained sidecar resurrection (`restart/audit/totality/p1/1A-substrate-evidence.md:76`-`77`; `restart/audit/totality/p1/1C-runtime-evidence.md:64`, `restart/audit/totality/p1/1C-runtime-evidence.md:110`). |
| CH5-V4-005 | ACCEPT | The live 1F scan catches current sidecar families instead of relying on superseded 1F auxiliaries. | The V4 context says `1F-anti-pattern.md` and `1F-past-corpora.md` are historical only (`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:19`-`22`). Live 1F owns root structural sidecar, CSS comparator sidecar, and CSS FNV runtime-hash rows (`restart/audit/totality/p1/1F-coherence-scan.md:87`-`89`) and states no new retained substrate is proven while CSS fact-stream/hash telemetry still needs fences (`restart/audit/totality/p1/1F-coherence-scan.md:128`). |

No Required Fold.

No source edit, inventory edit, staging, or commit was performed for this CH5 report.
