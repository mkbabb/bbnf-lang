# Hardening Plan Audit 06 — Generated-Code Budget

Date: 2026-05-03
Starting point: 168,750 LOC across generated grammars at `docs/tranches/BA/BA.md:108-121`, sourced from `audit/MODULES-2026-05-03.md:619-628`.

## BA Budget Walk

| Wave | Generated impact | Plan budget | Verdict | Surgery |
|---|---|---|---|---|
| BA.W0 | Comment scrub and CSS host path regen may touch generated CSS L4 and emitted comments. | Baseline artefact at `docs/tranches/BA/waves/W0.md:61-66`; BA table at `docs/tranches/BA/BA.md:110-121`. | honored | No surgery. |
| BA.W1 | Metadata resolver and recogniser registry should not regen generated parsers. | No wave-specific generated LOC gate in `docs/tranches/BA/waves/W1.md:78-93`. | silent-must-add | Add gate: generated LOC total remains 168,750 +/- 0.5%; any delta must be explained by metadata sidecar only. |
| BA.W2 | God-module splits should redistribute source, not generated output. | W2 budget artefact at `docs/tranches/BA/waves/W2.md:94-99`. | honored | No surgery. |
| BA.W3 | Path-core extraction and registry fixture deletion may alter registry sidecars, not generated parser LOC. | No generated budget gate in `docs/tranches/BA/waves/W3.md:76-96`. | silent-must-add | Add gate: `crates/core/src/grammar/generated/*.rs` LOC unchanged from W2; `.registry.json` schema diff recorded separately. |
| BA.W4 | Generated `parse_with` for all nine grammars likely grows parser files. | W4 only gates parse_with count at `docs/tranches/BA/waves/W4.md:79-81`; no LOC window. | violated | Add per-grammar W4 windows: `json.rs <= 3,700`, `bbnf.rs <= 22,000`, `css_l4.rs <= 110,000`, aggregate <= +5% from W2. |
| BA.W5 | JSON direct-to-struct should shrink `json.rs`. | W5 gates `json.rs <= 2,200` at `docs/tranches/BA/waves/W5.md:106-111`; BA table targets ~2,100 at `docs/tranches/BA/BA.md:112`. | honored | Align W5 and BA close: choose either <=2,100 or <=2,200; do not carry two ceilings. |
| BA.W6 | Final BA generated table. | W6 gates total <=163,855 at `docs/tranches/BA/waves/W6.md:54-59`. | honored | No surgery. |

## BB Budget Walk

| Wave | Generated impact | Plan budget | Verdict | Surgery |
|---|---|---|---|---|
| BB.W0 | Path-dep emigration should not change generated parser LOC. | No W0 budget row; tranche table requires a budget artefact at `docs/tranches/BB/BB.md:115`. | silent-must-add | Add W0 gate: generated parser LOC unchanged from BA close. |
| BB.W1 | Direct-to-struct across CSS L4, BBNF, and Sheets is a large generated rewrite. | BB-G11 aggregate at `docs/tranches/BB/BB.md:25`; per-grammar close targets at `docs/tranches/BB/BB.md:100-113`. | violated | Add W1-specific windows: `bbnf.rs <= 20,500`, `css_l4.rs <= 98,000`, `google_sheets.rs <= 13,500`, `json.rs <= 2,200`; non-special cohort unchanged until W2. |
| BB.W2 | Cohort template emission shrinks simple grammars. | BB-G5 runtime shrink at `docs/tranches/BB/BB.md:19`; generated table gives cohort targets at `docs/tranches/BB/BB.md:108-112`; W2 closer measures `runtime/<g>` template LOC at `docs/tranches/BB/waves/W2.md:70-76`. | partial | Add two W2 gates: each cohort generated parser <= table target, and runtime-template LOC <=50 separately. Do not let runtime shrinkage mask generated parser growth. |
| BB.W3 | Pratt/SIMD/rank/tier can grow generated dispatch code. | No wave-specific generated budget in `docs/tranches/BB/BB.md:34`. | silent-must-add | Add W3 window: specialised grammar net delta <= +5% from W2; each Pratt/SIMD expansion row names rule and emitted LOC delta. |
| BB.W4 | `parse_in` / `parse_owned` surfaces grow entry functions. | BB.W4 says per-wave budget verified at `docs/tranches/BB/BB.md:35`, but no figures. | violated | Add W4 budget artefact with per-file delta <= +2% from W3; owned/bumpalo wrappers must be shared helpers, not per-rule clones. |
| BB.W5 | Visitor and `LazyValue` surfaces grow generated methods. | BB table attributes +5% JSON and CSS visitor growth at `docs/tranches/BB/BB.md:104-107`. | partial | Add W5 gate: visitor-generated LOC <= table deltas; method count = record count, not rule count x backend count. |
| BB.W6 | Close table. | Aggregate target ~134,700 at `docs/tranches/BB/BB.md:113`. | honored | No surgery. |

## BC Budget Walk

| Wave | Generated impact | Plan budget | Verdict | Surgery |
|---|---|---|---|---|
| BC.W0 | IR contract spec should not regen generated parsers. | BC says W0 regenerates nothing at `docs/tranches/BC/BC.md:100`. | honored | Add explicit W0 gate: generated LOC unchanged from BB close. |
| BC.W1 | Rust emitter refactor requires regen-equality. | Gate at `docs/tranches/BC/BC.md:31`; generated impact described at `docs/tranches/BC/BC.md:100`. | honored | No surgery. |
| BC.W2 | TS/WASM scaffolds land outside generated Rust. | BC says scaffolds are separate paths at `docs/tranches/BC/BC.md:100`. | honored | No surgery. |
| BC.W3 | Crate split relocates files, no regen. | Stated at `docs/tranches/BC/BC.md:100`; xtask output moves to `crates/bbnf-parse/src/parse/generated/` at `docs/tranches/BC/waves/W3.md:81-87` and `docs/tranches/BC/waves/W3.md:105-111`. | partial | Add W3 budget gate: generated bytes and LOC are unchanged, but the budget path switches from `crates/core/src/grammar/generated/` to `crates/bbnf-parse/src/parse/generated/`; stale old-path references must fail. |
| BC.W4 | Visitor formalisation grows generated Rust. | BC budget table gives +1.1% to +2.3% at `docs/tranches/BC/BC.md:106-119`; BC-G10 ceiling at `docs/tranches/BC/BC.md:24`. | violated | `json.rs` target +2.3% breaches BC-G10 net-delta wording if read per-file. Change BC-G10 to aggregate <=+2% and per-file <=+2.5%, or reduce JSON visitor growth to <=+2%. |
| BC.W5 | API freeze no generated Rust change. | No explicit wave gate. | silent-must-add | Add gate: generated LOC unchanged from BC.W4. |
| BC.W6 | Close table. | Aggregate +1.9% at `docs/tranches/BC/BC.md:119-121`. | honored | No surgery. |

## Fault Table

| ID | Target | Required edit |
|---|---|---|
| G06-1 | `docs/tranches/BA/waves/W1.md` | Add generated LOC unchanged gate. |
| G06-2 | `docs/tranches/BA/waves/W3.md` | Add generated parser LOC unchanged gate and separate registry sidecar delta. |
| G06-3 | `docs/tranches/BA/waves/W4.md` | Add W4 generated LOC windows before all-grammar `parse_with` emits. |
| G06-4 | `docs/tranches/BB/BB.md:31-36` | Add W0-W5 wave-specific generated LOC windows, not only tranche-close budget. |
| G06-5 | `docs/tranches/BC/BC.md:24`, `docs/tranches/BC/BC.md:110` | Reconcile BC-G10 with JSON +2.3% row. |
| G06-6 | `docs/tranches/BC/waves/W3.md:81-87`, `docs/tranches/BC/waves/W3.md:105-111`, `docs/tranches/BC/waves/W6.md:170` | Add generated-output relocation budget: W3 is path-only, byte-identical, and every post-W3 budget gate uses `crates/bbnf-parse/src/parse/generated/`; delete the stale W6 `crates/core/src/grammar/generated/` closure row. |
| G06-7 | `docs/tranches/BB/waves/W2.md:70-76`, `docs/tranches/BB/BB.md:108-112`, `audit/MODULES-2026-05-03.md:619-628` | Split parser-output LOC accounting from runtime-template LOC accounting. They answer different risks and cannot share one budget row. |

## Lane Verdict

| Status | Count |
|---|---:|
| honored | 8 |
| partial | 3 |
| violated | 4 |
| silent-must-add | 7 |

The tranche-level budgets are useful. The wave gates are too sparse exactly where generated code is most likely to balloon: BA.W4, BB.W1, BB.W3, and BB.W5.
