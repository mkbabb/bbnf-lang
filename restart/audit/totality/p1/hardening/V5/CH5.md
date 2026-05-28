---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V5
disposition: ACCEPT
generated_at: 2026-05-28T06:24:35Z
files_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V4/CH5.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
historical_auxiliaries_not_live:
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH5 Hidden Coupling - SK-V15 T-P1 V5

## Verdict

ACCEPT.

The V5 fold strengthens CH5's current-source coverage for generated CSS FNV
hashes: `1F-coherence-scan.md` now carries a seven-profile, root-resolving FNV
line-position transcript for every generated CSS L4 runtime, plus the generator
template sites (`restart/audit/totality/p1/1F-coherence-scan.md:89`,
`restart/audit/totality/p1/1F-coherence-scan.md:91`-`101`). That discharges the
V4 CH5 concern without hiding the coupling: `1F` still classifies the surface as
hash-sidecar / telemetry coupling and routes it to W10 FNV quarantine, not to CSS
Value API proof, retained document identity, same-substrate proof, or production
equality (`restart/audit/totality/p1/1F-coherence-scan.md:89`,
`restart/audit/totality/p1/1F-coherence-scan.md:154`,
`restart/audit/totality/p1/1F-coherence-scan.md:177`).

This is a lens-level ACCEPT only. It does not declare T-P1 converged or locked:
the V5 dispatch context says V4 was REVISE and a clean V5 does not by itself
create two consecutive clean cycles (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:26`-`31`).

## Evidence

- CH5's authority is the hidden-coupling firewall: no parallel substrate,
  sidecar producer, renamed-scanner Lock 1 violation, or Track 1 == Track 2
  dishonesty may pass uncatalogued (`restart/prompts/totality/PASS-1-EXCAVATION.md:125`-`128`,
  `restart/prompts/ORCHESTRATOR.md:87`). V5 focuses this lens on the seven-profile
  FNV transcript and the no-laundering boundary (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:77`-`79`).

- The V4 consolidated fold required root-resolving COH-016 citations and
  explicitly preserved the rule that FNV/hash evidence is telemetry/quarantine
  only, not CSS Value API, retained identity, same-substrate, or production
  equality proof (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:38`-`40`,
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:53`-`55`).

- The current V5 context marks `1F-anti-pattern.md` and `1F-past-corpora.md` as
  historical and superseded, so this report treats `1F-coherence-scan.md` as the
  live 1F authority (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:21`-`24`).

- Current source confirms the seven CSS generated runtime files and the template
  have the claimed FNV surfaces:

| profile / template | input-hash output | helper |
|---|---|---|
| `css_l4_at_rules_and_media` | `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs:619` |
| `css_l4_declaration_values` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:619` |
| `css_l4_declaration_values_extended` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs:619` |
| `css_l4_nested_layout` | `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs:619` |
| `css_l4_stylesheet_selectors` | `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs:619` |
| `css_l4_vendor_and_custom_atrules` | `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs:619` |
| `css_l4_visual_functions` | `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:25`; `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:71` | `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs:619` |
| generator template | `skinny/crates/codegen/src/runtime_generator.rs:737`; `skinny/crates/codegen/src/runtime_generator.rs:783` | `skinny/crates/codegen/src/runtime_generator.rs:1331` |

- The representative generated runtime confirms the meaning of those lines:
  CSS fact-stream emission writes policy/source rows and `source\tinput_fnv64`
  (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5`-`47`,
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:25`-`26`,
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:71`-`72`),
  with the local `fnv64` helper at
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:619`-`625`.
  The template emits the same surfaces at
  `skinny/crates/codegen/src/runtime_generator.rs:737`-`738`,
  `skinny/crates/codegen/src/runtime_generator.rs:783`-`784`, and
  `skinny/crates/codegen/src/runtime_generator.rs:1331`-`1336`.

- The no-laundering boundary remains intact across the live inventories. `1C`
  says CSS L4 fact streams are admitted fact output, not retained substrate or a
  sixth `BackendShape`, and cannot close as CSS Value API proof
  (`restart/audit/totality/p1/1C-runtime-evidence.md:65`,
  `restart/audit/totality/p1/1C-runtime-evidence.md:112`). `1D` keeps CSS Value
  API missing and CSS equality non-equivalent (`restart/audit/totality/p1/1D-skinny-lessons.md:156`-`157`)
  and routes FNV through W10 quarantine with no production equality arbiter
  (`restart/audit/totality/p1/1D-skinny-lessons.md:181`,
  `restart/audit/totality/p1/1D-skinny-lessons.md:211`,
  `restart/audit/totality/p1/1D-skinny-lessons.md:230`). `1E` keeps Lock 1
  partial / JSON-tape-only and says string-only CSS fact streams are not value
  API evidence (`restart/audit/totality/p1/1E-locks-evidence.md:90`,
  `restart/audit/totality/p1/1E-locks-evidence.md:130`).

- Substrate closure remains open where it must. `1A` says the fact-stream plane
  must stay classified as a substrate target, not a backend shape, and net Lock 1
  is not yet proven as one typed event cursor plus one `TapeEmit` / `DirectBuild`
  schedule (`restart/audit/totality/p1/1A-substrate-evidence.md:154`-`174`). `1F`
  repeats that generated CSS runtime hashes are telemetry and that the broad
  sidecar/hash guard must be rerun before any CH5 or substrate-close claim
  (`restart/audit/totality/p1/1F-coherence-scan.md:140`,
  `restart/audit/totality/p1/1F-coherence-scan.md:185`).

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH5-V5-001 | ACCEPT | The V5 COH-016 transcript strengthens current-source coverage. It names all seven generated CSS runtime files plus the generator template with root-resolving path:line citations. | Live `1F` COH-016 and transcript at `restart/audit/totality/p1/1F-coherence-scan.md:89` and `restart/audit/totality/p1/1F-coherence-scan.md:91`-`101`; source verification table above. |
| CH5-V5-002 | ACCEPT | The FNV surface is still classified as hash-sidecar / telemetry coupling, not proof. | `1F` says telemetry-only unless W10 proves otherwise and explicitly rejects CSS Value API, retained identity, same-substrate, and production equality proof at `restart/audit/totality/p1/1F-coherence-scan.md:89`; owner/receiver map routes to W10 FNV quarantine at `restart/audit/totality/p1/1F-coherence-scan.md:154`; missing-production-quarantine gap remains open at `restart/audit/totality/p1/1F-coherence-scan.md:177`. |
| CH5-V5-003 | ACCEPT | CSS Value API and equality proof remain blocked, not silently upgraded by the hash transcript. | `1D` marks CSS equality non-equivalent and CSS typed value/document/view/visitor absent at `restart/audit/totality/p1/1D-skinny-lessons.md:156`-`157`; `1D` routes FNV to bench-only quarantine/delete with no production equality arbiter at `restart/audit/totality/p1/1D-skinny-lessons.md:181` and `restart/audit/totality/p1/1D-skinny-lessons.md:211`; the open FNV production guard remains at `restart/audit/totality/p1/1D-skinny-lessons.md:230`. |
| CH5-V5-004 | ACCEPT | The substrate union is not paper-closed by the FNV transcript. | `1A` keeps the fact-stream plane as a substrate target and says Lock 1 is only partly honored because one typed event cursor plus one `TapeEmit` / `DirectBuild` schedule is not proven (`restart/audit/totality/p1/1A-substrate-evidence.md:154`-`174`); `1C` keeps CSS fact output as a partial output plane, not a sixth shape or CSS Value API close (`restart/audit/totality/p1/1C-runtime-evidence.md:112`). |
| CH5-V5-005 | ACCEPT | The live 1F authority is used correctly; stale 1F auxiliaries are not treated as current-source proof. | V5 context marks `1F-anti-pattern.md` and `1F-past-corpora.md` historical only (`restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:21`-`24`), while `1F-coherence-scan.md` owns COH-016, the transcript, the guard, and the W10 receiver (`restart/audit/totality/p1/1F-coherence-scan.md:89`-`101`, `restart/audit/totality/p1/1F-coherence-scan.md:140`, `restart/audit/totality/p1/1F-coherence-scan.md:154`). |

No Required Fold.

No source edit, inventory edit, staging, or commit was performed for this CH5
report.
