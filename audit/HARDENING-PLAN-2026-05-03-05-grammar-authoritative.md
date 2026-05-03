# Hardening Plan Audit 05 — Grammar-Authoritative Discipline

Date: 2026-05-03
Standard: no grammar-specific code in supposedly generic crates; all CENSUS §2 violations must have a wave owner.

## Census §2 Closure

| CENSUS violation | Ground truth | Plan owner | Verdict |
|---|---|---|---|
| CSS L4 host fn in core root | `audit/CENSUS-2026-05-03.md:105-109` says `crates/core/src/css_types.rs` is CSS-specific and must move to grammar host. | BA.W0 moves it to `crates/core/src/host/css_types.rs` at `docs/tranches/BA/BA.md:30` and `docs/tranches/BA/waves/W0.md:26-31`. | violated-with-rec-G05-1 |
| `bbnf-ir` strategy table hardcodes grammar idents | `audit/CENSUS-2026-05-03.md:113-115`. | BA.W1 metadata resolver at `docs/tranches/BA/waves/W1.md:29-41`; BA-G7 at `docs/tranches/BA/BA.md:21`. | honored |
| `GrammarAuditTag::{Json,CssL4,Sheets,Bbnf}` | `audit/CENSUS-2026-05-03.md:116`. | BA.W1.M2 at `docs/tranches/BA/waves/W1.md:43-48`. | honored |
| `shape_dict_bbnf.rs` recogniser | `audit/CENSUS-2026-05-03.md:117`. | BA.W1.M3/M5 at `docs/tranches/BA/waves/W1.md:50-69`. | honored |
| `bbnf_shape_templates` field | `audit/CENSUS-2026-05-03.md:118`. | BA.W1.M4 at `docs/tranches/BA/waves/W1.md:57-62`. | honored |
| `bbnf-path` registry grammar arms and TS fixture | `audit/CENSUS-2026-05-03.md:143-149`. | BA.W3.M3 at `docs/tranches/BA/waves/W3.md:48-53`. | honored after Lock-7 path-name surgery |
| Gorgeous per-grammar shims | `audit/CENSUS-2026-05-03.md:151-162`. | Pre-BA archive at `docs/tranches/BA/BA.md:44-49`. | honored |

## Faults

| ID | Site | Fault | Surgery |
|---|---|---|---|
| G05-1 | `docs/tranches/BA/waves/W0.md:26-31` | Moving CSS L4 host code to `crates/core/src/host/css_types.rs` keeps a CSS-specific module inside the generic core host root. CENSUS prescribed `crates/core/src/grammar/host/css_l4.rs` or a grammar-host crate at `audit/CENSUS-2026-05-03.md:109`. | Change W0.M1 target to `crates/core/src/grammar/host/css_l4.rs` or `crates/core/src/host/css_l4/css_types.rs`; rewrite generated references to `crate::grammar::host::css_l4::parse_hex_color` or the chosen per-grammar host namespace. |
| G05-2 | `docs/tranches/BA/waves/W1.md:21` | W1 examples prescribe BBNF/CSS recogniser blocks in per-grammar TOML, but the generic IR registry still needs a typed schema for unknown recognisers. The plan names concrete `color_function_miner`, `dimension_unit_miner`, and BBNF miners without defining how the generic crate loads a miner absent grammar match arms. | Add W1.M4 schema fields: `recognizer.name`, `recognizer.crate`, `recognizer.entrypoint`, `recognizer.output_kind`. The IR loads by registry entry, not by stringly known built-ins. |
| G05-3 | `docs/tranches/BA/waves/W1.md:67` | The fallback option creates `crates/bbnf-host-tests/`; if this becomes a production dependency, it creates another per-grammar crate without a receiving ownership rule. | Add "test-only dev-dependency; no production member may depend on it" to W1.M5, or require the synthetic generic fixture path. |
| G05-4 | `docs/tranches/BC/BC.md:141-150` | BC's typed IR alphabet includes `TypedHost`/`TypedMap` backend host resolution but does not state where per-grammar host mappings live after G05-1. | In BC.W0 contract, add "host mappings are read from per-grammar host metadata emitted by bbnf-parse; bbnf-codegen never hardcodes grammar names." |

## Ratified Surfaces

| Site | Why it holds |
|---|---|
| `docs/tranches/BA/waves/W1.md:71-76` | The IR source sweep covers `JsonParser`, `CssL4Parser`, `BbnfBootstrap`, `GoogleSheetsParser`, and related grammar idents. |
| `docs/tranches/BA/waves/W3.md:51-53` | The path registry match arms delete and registry lookup flows through BA.W1 metadata. |
| `docs/tranches/BB/BB.md:49` | BB.W3 extensions reference grammars only through `&str` ident from workspace metadata. |
| `docs/tranches/BC/BC.md:49-54` | BC carries grammar-agnostic optimizer and direct-to-struct contracts from BB without adding grammar arms. |

## Lane Verdict

| Status | Count |
|---|---:|
| honored | 6 |
| violated-with-recommendation | 4 |
| silent | 0 |

The main wound is the CSS host move: `host/css_types.rs` is cleaner than crate root, but it is not grammar-authoritative enough for this lock.
