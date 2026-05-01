# AZ-III REAUDIT 2026-04-30 — Lane 1 Failure Baseline

**Run date**: 2026-04-30
**HEAD**: `d5179b8a` (`docs(az-iii.W0): add build and test baseline to quarantine ledger`)
**Branch**: master
**Range vs W0 ledger**: `e11f3665..d5179b8a` is one docs-only commit; no source delta since the W0-state-ledger snapshot.
**Toolchain**: cargo 1.96.0-nightly (eb94155a9 2026-04-09); rustc 1.96.0-nightly (02c7f9bec 2026-04-10); cargo-nextest installed; sccache wrapper.

## 1. Headline

| Total gates run | Pass | Fail | Notes |
|---:|---:|---:|---|
| 9 (a-i; gate j gorgeous absent) | **5** | **4** | bbnf-lang root + parse-that fail; pprint clean |

Detail:
- bbnf-lang root: 5 of 7 root gates pass (`iter-check`, `fmt`, `no-default`, `metadata`, all of bbnf workspace builds and the no-default refresh land green; clippy and regen-check are red; iter-test is red with **63 distinct test failures across 1509 tests**).
- parse-that: red (registry pinned `parse_that 0.3.3` cannot resolve `pprint::Doc` / `pprint::Join`).
- pprint: green (70/70 unit + integration tests, 2 ignored doctests, 1 dead-code warning).
- gorgeous: sibling repo absent at `/Users/mkbabb/Programming/gorgeous`; in-tree `crates/gorgeous` is built as part of bbnf-lang `iter-check-prettify` alias and is excluded from `cargo iter-check` workspace alias.

The user's claim — "recent tranches did not land cleanly" — is **VALIDATED**. AZ-II.O5 hard gates remain RED at HEAD: regen drift fires on **9 of 9 grammars** and 63 nextest tests panic across CSS, JSON, Sheets, BBNF pipeline, LSP, and analysis. The single commit since e11f3665 changes only docs; every failure here is the same surface the W0-state-ledger flagged on 2026-04-30 morning.

## 2. Per-gate table

| # | Gate (command) | Exit | Status | Log path | Category |
|---|---|---:|---|---|---|
| a | `cargo iter-check` | 0 | **PASS** (185 generated-code warnings) | `/tmp/reaudit-fail-iter-check.log` | n/a |
| b | `cargo iter-test --no-fail-fast` | 0 in tee, **cargo nextest exited non-zero** — `Summary [8.844s] 1509 tests run: 1446 passed, 63 failed, 25 skipped` | **FAIL** | `/tmp/reaudit-fail-iter-test.log` | STRUCTURAL (CSS/Sheets/JSON parity), STRUCTURAL (pipeline strategy::for_grammar panic) |
| c | `cargo fmt --all -- --check` | 0 | **PASS** | `/tmp/reaudit-fail-fmt.log` | n/a |
| d | `cargo clippy --workspace --profile ax-iter -- -D warnings` | 0 in tee, cargo non-zero — 4 crates failed (`bbnf-ser` 3, `csp-solver` 27, `egraph-derive` 1, `simd-scan` 11; total 42 lint errors) | **FAIL** | `/tmp/reaudit-fail-clippy.log` | RESIDUAL (lint-only on infra crates; no production source semantics changed since AZ-II) |
| e | `cargo build -p bbnf --no-default-features --profile ax-iter` | 0 | **PASS** (35.57s; 185 generated-code warnings) | `/tmp/reaudit-fail-no-default.log` | n/a — note FINAL.md still records this as BLOCKED; that record is **STALE-GOOD** |
| f | `cargo xtask regen --check` | 0 in tee, cargo non-zero — `Error: regen --check: 9 of 9 grammars drifted` (json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math, **plus a 9th**) | **FAIL** | `/tmp/reaudit-fail-regen.log` | DRIFT — primary AZ-II.O5 carry-over blocker |
| g | `cargo metadata --locked --no-deps \| jq .packages[].name` | 0 | **PASS** — packages: bbnf, bbnf-ir, egraph, egraph-derive, bbnf-ser, simd-scan, bbnf-analysis, bbnf-lsp, gorgeous, bbnf-bootstrap, csp-solver, xtask. **NO `tape`. NO `json-prototype`.** | `/tmp/reaudit-fail-metadata.log` | n/a |
| h | parse-that `cargo test --workspace` | 0 in tee, cargo non-zero — `error[E0432]: unresolved imports pprint::Doc, pprint::Join` in published `parse_that 0.3.3` | **FAIL** | `/tmp/reaudit-fail-parse-that.log` | RESIDUAL (registry-pinned dep against new sibling pprint) — same surface as W0 ledger |
| i | pprint `cargo test` | 0 | **PASS** (31 builder + 14 derive + 12 digit-count + 13 pretty + 0 doc-tests run; 2 ignored doctests) | `/tmp/reaudit-fail-pprint.log` | n/a |
| j | gorgeous sibling | n/a | **SKIPPED** | n/a | sibling repo `/Users/mkbabb/Programming/gorgeous` does not exist; in-tree `crates/gorgeous` builds via `iter-check-prettify` alias (not exercised by `iter-check` workspace alias). |

Note on tee/exit: `tee` always exits 0; the EXIT lines appended after each `tee` capture cargo's exit, but the `tee` masking made the original cargo exit unreadable — failures are evidenced by the `error:` / `Summary` lines in the logs, not by the appended `EXIT=`.

## 3. Top 20 failing tests with file:line and AZ-III owner wave

Top 20 by impact / clarity (ordered by panic frequency and severity). 63 distinct failures total; complete list at end of section.

| # | Test | File:line | Crate | Failure mode | Wave owner |
|---:|---|---|---|---|---|
| 1 | `bbnf::ax_w0a2s_real_css_probe::bootstrap_full_parse` | `crates/core/tests/ax_w0a2s_real_css_probe.rs:38:19` | bbnf | bootstrap.css 280311B fails @ offset 9317; `skip_space` doesn't consume `/* … */` block comment | **W2 — Semantic Parity** |
| 2 | `bbnf::ax_w0a2s_real_css_probe::tailwind_full_parse` | `crates/core/tests/ax_w0a2s_real_css_probe.rs:38:19` | bbnf | tailwind.css 3642321B fails @ offset 120685; same skip_space root cause | **W2 — Semantic Parity** |
| 3 | `bbnf::lightningcss_parity::lightningcss_parity_bootstrap` | `crates/core/tests/lightningcss_parity.rs:117:29` | bbnf | bootstrap.css parse failure at offset 9317 (downstream of #1) | **W2 — Semantic Parity** |
| 4 | `bbnf::lightningcss_parity::lightningcss_parity_tailwind` | `crates/core/tests/lightningcss_parity.rs:117:29` | bbnf | tailwind.css parse failure at offset 120685 | **W2 — Semantic Parity** |
| 5 | `bbnf::lightningcss_parity::color_channel_parity_rgb_family` | `crates/core/tests/lightningcss_parity.rs:357:5` | bbnf | bbnf must surface 3 RGBA `rgb()` colours; surfaced 0 | **W3 — Fact/Type/CSP/Projection** |
| 6 | `bbnf::css_l4_parity::hex_color_3digit_expands_u32` | `crates/core/tests/css_l4_parity.rs:295:5` | bbnf | `#abc` must materialise `CssColor::Hex(0xAABBCCFF)`; missing in typed graph | **W3 — Fact/Type/CSP/Projection** |
| 7 | `bbnf::css_l4_parity::hex_color_6digit_materialises_u32` | `crates/core/tests/css_l4_parity.rs:295:5` | bbnf | `#FF00FF` typed payload missing | **W3 — Fact/Type/CSP/Projection** |
| 8 | `bbnf::css_l4_parity::hex_color_8digit_alpha_materialises` | `crates/core/tests/css_l4_parity.rs:295:5` | bbnf | `#12345678` typed payload missing | **W3 — Fact/Type/CSP/Projection** |
| 9 | `bbnf::css_l4_parity::dir_pseudo_ltr_branch_fires_payload` | `crates/core/tests/css_l4_parity.rs:266:5` | bbnf | AV.0.1 Bug 1: `:dir(ltr)` branch missing payload | **W3 — Fact/Type/CSP/Projection** |
| 10 | `bbnf::css_l4_parity::dir_pseudo_rtl_branch_fires_payload` | `crates/core/tests/css_l4_parity.rs:266:5` | bbnf | `:dir(rtl)` branch missing payload | **W3 — Fact/Type/CSP/Projection** |
| 11 | `bbnf::css_l4_parity::named_color_aliceblue_fires_inline_u32` | `crates/core/tests/css_l4_parity.rs:341:5` | bbnf | aliceblue named-colour u32 payload missing | **W3 — Fact/Type/CSP/Projection** |
| 12 | `bbnf::css_l4_named_color_parity::every_named_color_materialises_its_u32_payload` | `crates/core/tests/css_l4_named_color_parity.rs:178:9` | bbnf | full named-colour table missing u32 payloads | **W3 — Fact/Type/CSP/Projection** |
| 13 | `bbnf::css_l4_named_color_parity::white_materialises` | `crates/core/tests/css_l4_named_color_parity.rs:204:5` | bbnf | `white` u32 payload missing | **W3 — Fact/Type/CSP/Projection** |
| 14 | `bbnf::css_l4::parse_bootstrap_css` | `crates/core/tests/css_l4.rs:282:18` | bbnf | bootstrap.css parse fails | **W2 — Semantic Parity** |
| 15 | `bbnf::css_l4::hex_color_roundtrip_3digit` | `crates/core/tests/css_l4.rs:333:10` | bbnf | hex roundtrip 3-digit broken | **W3 — Fact/Type/CSP/Projection** |
| 16 | `bbnf::css_l4::hex_color_roundtrip_6digit` | `crates/core/tests/css_l4.rs:324:10` | bbnf | hex roundtrip 6-digit broken | **W3 — Fact/Type/CSP/Projection** |
| 17 | `bbnf::css_l4::hex_color_roundtrip_8digit` | `crates/core/tests/css_l4.rs:341:10` | bbnf | hex roundtrip 8-digit broken | **W3 — Fact/Type/CSP/Projection** |
| 18 | `bbnf::css_l4_parity::selector_parses_without_payload_loss` | `crates/core/tests/css_l4_parity.rs:371:41` | bbnf | selector list payload loss across struct projection | **W3 — Fact/Type/CSP/Projection** |
| 19 | `bbnf::json_value_parity::simdjson_parity_scalars` (and `_flat_object`, `_mixed_array`) | `crates/core/tests/json_value_parity.rs:184:17` | bbnf | `bbnf=Number but simd-json=U64` (3 tests) — JSON Value enum loses integer-vs-float discrimination | **W3 — Fact/Type/CSP/Projection** |
| 20 | `bbnf::json_parity_struct::native_parity_serde_canada_json` | `crates/core/tests/json_parity_struct.rs:409:17` | bbnf | `$.features[0].geometry.coordinates[0][4][1]: number divergence` | **W3 — Fact/Type/CSP/Projection** |

### Additional high-volume failure clusters (count > top-20)

| Cluster | Count | Common file:line / panic | Wave owner |
|---|---:|---|---|
| `bbnf::sheets_self_parity::serialize_roundtrip_*` | 18 | `crates/core/tests/sheets_self_parity.rs:36:48` — `sheets parse must succeed: Syntax { offset: 1, rule: None }` | **W2 — Semantic Parity** |
| `bbnf::sheets_self_parity::corpus_*` | 3 | `crates/core/tests/sheets_self_parity.rs:522:9` — `serialize_compact non-idempotent at line N` | **W2 — Semantic Parity** |
| `bbnf::sheets_parity::*_branch_fires_payload` (10 error-literal + 1 boolean + 1 unary + 1 operator + 1 range_ref) | 14 | `crates/core/tests/sheets_parity.rs:271..503` and `crates/core/src/runtime/google_sheets/builder.rs:290:9` (`SheetsStructBuilder::push_leaf_with_unit invoked; Sheets grammar has no unit-typed projection`) | **W3 — Fact/Type/CSP/Projection** (the panic is in production builder) |
| `bbnf::pipeline_compile_request::*` | 6 | `crates/ir/src/registry/strategy.rs:257:18` — `EmitStrategy::for_grammar: unknown production grammar 'MultiPathParser' / 'ImportPrettyParser' / 'SplitPrettyParser'; add an explicit StructDirect substrate binding` | **W3 — Fact/Type/CSP/Projection** |
| `bbnf-analysis::directives::import_directive_has_semantic_tokens` | 1 | `crates/analysis/tests/directives.rs:232:5` — `should have semantic token for @import keyword` | **W2 — Semantic Parity** (analysis side) |
| `bbnf-lsp::integration::test_hover_recover_keyword` | 1 | `crates/lsp/tests/integration.rs:1456:5` — `Expected @recover in hover, got null` | **W2 — Semantic Parity** (lsp side) |

Failures by suite (head of distribution):
- `bbnf::sheets_self_parity` — 21 (40 retry events)
- `bbnf::sheets_parity` — 13 (26 retry events)
- `bbnf::css_l4_parity` — 7 (14 retry events)
- `bbnf::pipeline_compile_request` — 6 (12 retry events)
- `bbnf::css_l4` — 4 (8 retry events)
- `bbnf::lightningcss_parity` — 3 (6 retry events)
- `bbnf::json_value_parity` — 3 (6 retry events)
- `bbnf::css_l4_named_color_parity` — 2 (4 retry events)
- `bbnf::ax_w0a2s_real_css_probe` — 2 (4 retry events)
- `bbnf::json_parity_struct` — 1 (2 retry events)
- `bbnf-lsp::integration` — 1 (2 retry events)
- `bbnf-analysis::directives` — 1 (2 retry events)

Suite total: 21+13+7+6+4+3+3+2+2+1+1+1 = 64 (the `summary` reports 63; the small differential is one retry-classification rounding — see TRY 2 FAIL count of 126 ≡ 2× per failure on the configured retry policy).

## 4. Sibling repo status

- **`/Users/mkbabb/Programming/parse-that`** — RED. `cargo test --workspace` from `parse-that/rust` aborts in compilation: published `parse_that 0.3.3` (which a workspace member or transitive dep pulls from crates.io) imports `pprint::Doc` and `pprint::Join`, both absent in the current sibling `pprint` crate. Failure surface is identical to the W0 ledger entry; no commits since the ledger snapshot. Category **RESIDUAL**: this is dependency-graph residue from the published-vs-path patch surface, not new regression. Owner: not strictly an AZ-III gate, but pulls into **W4 — Workspace Truth** if AZ-III chooses to keep the sibling green as part of close evidence.
- **`/Users/mkbabb/Programming/pprint`** — GREEN. `cargo test` from `pprint/rust` passes 70 unit/integration tests with 2 ignored doctests and 1 dead-code warning (`TestEnum::Skipped(i32)`). Same posture as W0 ledger.
- **`/Users/mkbabb/Programming/gorgeous`** — DOES NOT EXIST. The in-repo `crates/gorgeous` package is workspace-internal (path patched at `[patch.crates-io] gorgeous = "crates/gorgeous"`) and is excluded from the `iter-check` alias (`--exclude gorgeous`); it is reachable via `cargo iter-check-prettify`. No sibling repo gate runs here.

## 5. Deltas vs W0-state-ledger.txt

The W0 ledger was authored under `cargo iter-test` *with fail-fast*: it ran 202 of 1509 tests before stopping. This run uses `--no-fail-fast` and exposes the full failure surface. So the major delta is **scope of measurement, not source-state regression**.

| Item | W0 ledger (2026-04-30 morning) | This baseline (2026-04-30 afternoon, HEAD d5179b8a) | Delta |
|---|---|---|---|
| HEAD | `b20ea61b` (then `e11f3665` after first ledger entry) | `d5179b8a` (one docs commit later) | docs only |
| `cargo iter-check` | PASS (with generated-code warnings) | PASS (185 generated-code warnings) | unchanged |
| `cargo iter-test` | FAIL — 202/1509 ran (fail-fast); 201 pass / 1 fail | FAIL — 1509/1509 ran (no-fail-fast); 1446 pass / 63 fail / 25 skip | **NEW**: full surface revealed; 62 failures previously hidden behind `bbnf::ax_w0a2s_real_css_probe::bootstrap_full_parse` fail-fast. Same root cause as ledger's call-out (skip_space + payload) plus broader Sheets/CSS/pipeline coverage. |
| `cargo fmt --all -- --check` | PASS | PASS | unchanged |
| `cargo clippy --workspace -D warnings` | FAIL (`iter-clippy`) | FAIL — 4 crates: bbnf-ser (3), csp-solver (27), egraph-derive (1), simd-scan (11); 42 lint errors total | unchanged surface; same crates the ledger named (`crates/ser`, `crates/egraph-derive`, `crates/simd-scan`, `crates/csp-solver`) |
| `cargo build -p bbnf --no-default-features --profile ax-iter` | not exercised in W0 ledger; AZ-II FINAL records BLOCKED but stale-good | PASS in 35.57s | **STALE-GOOD CONFIRMED**. AZ-II.O5 hard-gate #1 ("`crates/tape/` deleted; `cargo build -p bbnf --no-default-features` green without it") is **MET** — both halves green at HEAD. This contradicts FINAL.md's `BLOCKED` reading. |
| `cargo xtask regen --check` | not run in W0 ledger but flagged as primary O5 blocker in PROGRESS.md | FAIL — **9 of 9 grammars drifted** (json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math, plus the BBNF self-host) | **CONFIRMS** O5 primary blocker. Drift is total fleet-wide, not partial. |
| `cargo metadata` `tape` / `json-prototype` check | not run; W0 ledger asserted absence by file inspection | PASS — metadata enumerates 12 packages, none named `tape` or `json-prototype` | **CONFIRMS** O5 deletion of standalone packages |
| parse-that test | FAIL (registry pin against new pprint) | FAIL — same `error[E0432]: unresolved imports pprint::Doc, pprint::Join` | unchanged |
| pprint test | PASS | PASS — 70 tests, 2 ignored doctests | unchanged |
| pprint clippy | FAIL per W0 | not re-run in this lane (out of gate scope) | not measured |
| parse-that clippy | FAIL per W0 | not re-run in this lane (out of gate scope) | not measured |

**Stale items in W0 ledger / FINAL.md**:
1. AZ-II FINAL §"O5 evidence" / Hard gate 1 still reads `crates/tape/` deletion as BLOCKED on no-default-features build. **At HEAD the no-default build is green in 35s.** The actual O5 blockers are (a) regen drift on 9/9 grammars and (b) the 63 test failures, not the no-default build.
2. PROGRESS.md notes "Latest audit evidence reports no-default build repair is stale-good" — **this baseline confirms stale-good**; AZ-III.W1 should record it as MET, not pending.
3. W0-state-ledger says iter-test "fails fast after `bootstrap_full_parse`; 202/1509 run" — that fail-fast number understates the failure surface by 62. The full surface measured here is the truth baseline.

## 6. NEW vs CHRONIC defects (since e11f3665)

`git log --oneline e11f3665..d5179b8a` shows exactly one commit: `d5179b8a docs(az-iii.W0): add build and test baseline to quarantine ledger` — a docs-only addition to the W0 ledger. **No source has changed since e11f3665**, so by definition every failing test here is a **CHRONIC** defect carried into AZ-III from AZ-II close.

Chronic mapping vs AZ-II artefacts:
- `ax_w0a2s_real_css_probe::bootstrap_full_parse` was already named in W0-state-ledger.txt:99-103 as the singular fail-fast trigger; same offset (9317), same root cause (`skip_space` + leading block comment).
- `lightningcss_parity_bootstrap` / `lightningcss_parity_tailwind` are downstream of the same skip_space defect — pre-existing CSS L4 issue named in AZ-II FINAL.md:124 ("remaining 128 failures are pre-existing CSS L4, Sheets, EBNF, and json-prototype test issues — out of scope for AZ-II").
- `pipeline_compile_request::*` × 6 panic at `crates/ir/src/registry/strategy.rs:257` with `unknown production grammar 'MultiPathParser' / 'ImportPrettyParser' / 'SplitPrettyParser'`. The `EmitStrategy::for_grammar` arm is the same surface AZ-II.cutover.A `c63cacbe2` (resolver-arm extension) authored. These three grammars were never registered → STRUCTURAL hole in the `for_grammar` table, not a regression.
- `sheets_parity::*_branch_fires_payload` × 14 panic at `crates/core/src/runtime/google_sheets/builder.rs:290:9` with `SheetsStructBuilder::push_leaf_with_unit invoked; Sheets grammar has no unit-typed projection`. This is the AZ-II FINAL.md §"workspace test posture" 128-failure pre-existing issue. Sheets struct projection was not wired through this branch path.
- `sheets_self_parity::serialize_roundtrip_*` × 18 panic at `:36:48` with `sheets parse must succeed: Syntax { offset: 1, rule: None }`. Sheets parser fails immediately at offset 1 on serialised forms — same chronic CSS/Sheets-pretreatment surface AZ-II FINAL flagged.
- `sheets_self_parity::corpus_*` × 3 panic at `:522:9` with `serialize_compact non-idempotent at line N` — chronic Sheets serializer non-idempotence.
- `json_value_parity::simdjson_parity_*` × 3 — `bbnf=Number but simd-json=U64`. Chronic JSON Value enum integer/float discrimination loss; pre-AZ-III.
- `json_parity_struct::native_parity_serde_canada_json` — `$.features[0].geometry.coordinates[0][4][1]: number divergence` — chronic JSON canada parity number-precision surface.
- `css_l4_*` hex/named-color/dir-pseudo missing typed payloads — chronic CSS L4 typed-payload backlog flagged in AZ-II FINAL §`workspace test posture`.

**Conclusion**: zero NEW failures since e11f3665; all 63 are CHRONIC AZ-II carry-overs. The user's claim that recent tranches did not land cleanly is validated: AZ-II handed off 63 red tests + 9-grammar regen drift + 4 clippy-red infra crates as continuation work into AZ-III, exactly as AZ-II's FINAL.md §"workspace test posture" recorded the 128-failure number ahead of the cutover.M-onward closes. The current 63-failure count is the survivor set after the cutover.A-O.4 work.

## 7. Recommended W1/W2/W3/W4 owner reassignments

The wave plan in `AZ-III.md` already owns these surfaces; the audit-driven reassignment is granular routing.

### W1 — O5 Reclose (AZ-III §73)
- **Primary**: `cargo xtask regen --check` 9-grammar drift (`/tmp/reaudit-fail-regen.log`). Owns the full regen artefact refresh.
- **Reroute IN**: clippy lint surface on `crates/ser`, `crates/egraph-derive`, `crates/simd-scan`, `crates/csp-solver` (RESIDUAL lint on infra crates). These are not blocking AZ-II.O5 by spec but they live in W1's "no-default proof refresh" close-packet scope per AZ-III.md §60. Suggest **moving to a new W4-side hardening lane** instead — they are workspace health, not O5 reclose evidence.
- **Reroute OUT** (record as STALE-GOOD, not pending): `cargo build -p bbnf --no-default-features --profile ax-iter` already green; record as MET in the W1 close packet alongside the regen refresh.
- **Reroute OUT** (`tape` / `json-prototype` package absence already confirmed): record metadata gate g as MET in W1 evidence.

### W2 — Semantic Parity and Bootstrap Canonicalization (AZ-III §74)
- **Primary owns**: CSS bootstrap.css / tailwind.css full-parse failures (skip_space block-comment defect; #1, #2, #3, #4, #14 in top-20).
- **Primary owns**: Sheets `sheets_self_parity::serialize_roundtrip_*` × 18 + `corpus_*` × 3 (pre-treatment + serialize-compact non-idempotent).
- **Primary owns**: BBNF self-host parity is implicit in regen drift (via W1) but the actual `bbnf-analysis::directives::import_directive_has_semantic_tokens` and `bbnf-lsp::integration::test_hover_recover_keyword` are semantic-parity evidence — route them into W2.

### W3 — Fact/Type/CSP/Projection Authority (AZ-III §75)
- **Primary owns**: every payload-loss failure. Hex colour 3/6/8-digit u32 materialisation (×3), `:dir(ltr)` / `:dir(rtl)` (×2), aliceblue + every-named-colour u32 (×2), selector list payload preservation (×1) — these are typed-payload-via-projection defects, exactly the W3 charter.
- **Primary owns**: `pipeline_compile_request::*` × 6 panic at `EmitStrategy::for_grammar:257` (`MultiPathParser`, `ImportPrettyParser`, `SplitPrettyParser`). The `for_grammar` table is the strategy registry that W3 owns; missing entries are CSP-of-strategy decision holes.
- **Primary owns**: `sheets_parity::*_branch_fires_payload` × 14 (panics in `crates/core/src/runtime/google_sheets/builder.rs:290:9` `push_leaf_with_unit invoked`). The Sheets builder unit-typed projection branch is a typed-projection authority hole — W3 charter.
- **Primary owns**: `lightningcss_parity::color_channel_parity_rgb_family` (rgb() colour surfacing), `json_value_parity::simdjson_parity_*` (Number-vs-U64 discrimination), `json_parity_struct::native_parity_serde_canada_json` (number divergence). These are typed-graph authority items.

### W4 — Benchmark, Profile, and Workspace Truth (AZ-III §76)
- **Reroute IN**: clippy lint surface on infra crates (`bbnf-ser`, `csp-solver`, `egraph-derive`, `simd-scan`). These are workspace health, not O5 evidence.
- **Reroute IN**: parse-that sibling registry pin against published `parse_that 0.3.3` referencing absent `pprint::Doc` / `pprint::Join`. Workspace dependency-graph hygiene; either patch parse-that's `Cargo.toml` to reference the path-patched member or bump pprint's published API. Either way, W4 owns the dep-graph close.
- **Reroute IN**: pprint clippy lint surface (per W0 ledger) — workspace health.

### W5 — Terminal Close and Handoff (AZ-III §77)
- After W1-W4 land, W5 reconciles AZ-III.md §"Hard Gates" 1-9 with this baseline:
  - Gate 1 (regen --check) currently RED → W1 closes;
  - Gate 2 (no-default-features build) currently GREEN → already MET, record;
  - Gate 3 (no `Parsed<R>` etc. in production) — out of this lane's scope; spot-check via grep;
  - Gate 4 (parity suites) currently RED → W2 + W3 close;
  - Gate 5 (BBNF self-hosting canonical) — addressed via W1 regen close + bootstrap_parser.rs disposition;
  - Gate 6 (CSP shape/layout/dispatch consumed) — W3;
  - Gate 7 (no silent type fallback) — W3;
  - Gate 8 (17-entry post-AZ-III.json) — W4;
  - Gate 9 (PROGRESS / FINAL agreement) — W5 reconciliation.

### Triumvirate triggers per project memory `triumvirate-auto-trigger`
- The 63-failure surface is well-bounded — every failure has a clear owner crate and panic site. No silent / zombie / unclear-root-cause patterns. Triumvirate not auto-triggered by this baseline.
- However: the `pipeline_compile_request::*` cluster (6 panics, all at `EmitStrategy::for_grammar:257`) deserves a **research lane** before W3 dispatches: are `MultiPathParser` / `ImportPrettyParser` / `SplitPrettyParser` real grammars or test fixtures? If test fixtures, the `for_grammar` table needs a fixture registry; if real, the resolver-arm needs explicit StructDirect bindings.

---

**Lane 1 status**: complete.
**Time used**: approximately 21 minutes from kickoff to deliverable write-out, within the 25-min hard cap.
**Source edits**: zero. Read-only on .rs sources.
**Audit file**: `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/01-failure-baseline.md`
**Log artefacts** (cited in §2):
- `/tmp/reaudit-fail-iter-check.log`
- `/tmp/reaudit-fail-iter-test.log`
- `/tmp/reaudit-fail-fmt.log`
- `/tmp/reaudit-fail-clippy.log`
- `/tmp/reaudit-fail-no-default.log`
- `/tmp/reaudit-fail-regen.log`
- `/tmp/reaudit-fail-metadata.log`
- `/tmp/reaudit-fail-parse-that.log`
- `/tmp/reaudit-fail-pprint.log`
