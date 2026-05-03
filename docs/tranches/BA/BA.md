# BA — Surgical Foundation

## Gestalt

BA is the foundation tranche: hereupon the substrate is rebuilt from first principles, the grammar-specific leaks excised from supposedly-generic crates, the god modules and god directories partitioned per Lock 13, and JSON's direct-to-struct codegen lands as the demonstration of the new path. The tape's residue — comments, doc strings, the dead `generate/serialize/` directory — is scrubbed wholesale; the cursor-parse and byte-skip implementations unify into one parse function with `__EAGER_EMPTY_PATH` electing the fast path; the eight inline `#[cfg(test)]` violations migrate to `tests/`; the path-crate triplet (`path`, `path-core`, `path-ts`) consolidates per Lock 7; JSON `parse(twitter.json)` beats sonic-rs's M1 Pro 436 µs by ≥ 8% (`audit/SOTA-2026-05-03.md:50-58`).

All nine grammars — JSON, CSS L4, BBNF, Sheets, and the five-grammar template cohort (BNF, CSV, EBNF, CSS Pretty, Math) — migrate to direct-to-struct emission within BA, across five sub-waves W5a..W5e. Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` the substrate-identity decision is option (a): all-grammar migration owned by BA. Lock 1 is **honoured at BA close**, with W5a (JSON) → W5b (CSS L4) → W5c (BBNF) → W5d (Sheets) → W5e (cohort) as the sequential per-grammar migrations. The thesis is surgical excision begotten of full demonstration: prove the path on JSON, then carry the discipline through CSS L4's 14-variant `OpenFrame` taxonomy, BBNF's self-host stress, Sheets' arena-fallback complexity, and the cohort's five mechanical replicas; retire the substrate that begets the 86.07% `Vec<OpenFrame>::clone` pathology (`audit/RESTART-SKETCH-2026-05-03.md:154-220`); BA closes with `rg -n 'enum OpenFrame' crates/core/src/runtime/` returning 0 across all nine grammars.

## Cross-tranche impact

Under option (a), BA owns the all-grammar migration; BB.W1's per-grammar OpenFrame migration receivers retire. The synthesis pass reconciles cross-tranche: BB.W1 reorients to consume the post-BA all-grammar direct-to-struct foundation (focusing on emitter generalisation patterns, Pratt + SIMD auto-detection per Lock 10, e-graph + CSP + cost-model per Lock 4); BB.W2 absorbs W5e's five hand-written cohort modules into one parameterised cohort template per gap D (`docs/tranches/BB/audit/W2-cohort-template-spec.md`).

## Hard gates

Every **parse-throughput** gate cites a specific competitor + dataset + platform per Lock 8. Non-throughput engineering gates are separately labelled and do NOT claim Lock 8 honour.

### Parse-throughput gates (SOTA-anchored)

| ID | Gate | SOTA anchor |
|---|---|---|
| BA-G1a | `JsonParser::parse(twitter.json)` ≤ 400 µs on M1 Pro, beating sonic-rs's 436 µs by ≥ 8% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs `benchmark_aarch64` twitter row; simd-json 424 µs) |

### Engineering gates (non-SOTA, internal-progress per surgery #11)

| ID | Gate | Rationale |
|---|---|---|
| BA-G1b | CSS L4 bootstrap.css parse internal-progress: post-W5b regression-bound (no slowdown vs pre-W5b baseline; variance ≤ 5%). Lightning-css's 4.16 ms remains the BB-tranche SOTA target per BB-G1 | `audit/SOTA-2026-05-03.md:130` (lightning-css bootstrap row) |
| BA-G1c | BBNF self-host parse-and-format roundtrip passes; regression-bound (post-W5c ≤ pre-W5c × 1.05). No SOTA — no external competitor self-hosts a grammar language | regression-only, no SOTA |
| BA-G1d | Sheets parse_simple regression-bound (post-W5d ≤ pre-W5d × 1.05). No SOTA — no external competitor at this scale | regression-only, no SOTA |
| BA-G1e | Cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math): each parses fixture without regression (post-W5e ≤ pre-W5e × 1.05). No SOTA | regression-only, no SOTA |
| BA-G2 | `JsonParser::parse(twitter.json)` consumes ≤ 2 heap allocations per parse-call (1 arena + 1 root-vec); zero `Vec<OpenFrame>::clone` sites detectable in samply 5K-sample profile | `audit/RESTART-SKETCH-2026-05-03.md:154-220` (the 86.07% pathology) |
| BA-G3 | Toolchain: `cargo xtask regen --check` ≤ 30 s on M1 Pro; CSS L4 `compile_paths_request` ≤ 25 s | Internal halving gate; pre-BA baseline 59.98 s + 52.53 s |
| BA-G4 | All 13 locks honoured-or-deferred-with-receiver at BA close per the §13-Lock cross-reference table; `crates/ir/tests/substrate_audit.rs` green | Lock cross-reference is the contract |
| BA-G5 | God-module count (>500 LOC outside `generated/`, `crates/*/src/` only) drops from 23 → 0 per `audit/CENSUS-2026-05-03.md:319-353` | Lock 13 |
| BA-G6 | `crates/core/src/` reorganised into the 9-directory layout from `audit/MODULES-2026-05-03.md:1107-1118`; no `pipeline.rs` + `pipeline/` co-existence | Lock 13, `feedback_directory_modules` |
| BA-G7 | Zero grammar-specific arms in `bbnf-ir`: `crates/ir/src/registry/strategy.rs:130-185` reads from workspace metadata; `crates/ir/src/passes/audit/payload_coverage.rs:69` enum carries only `Custom(&'static str)`; `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` deleted; CSS L4 host fn relocated to `crates/core/src/grammar/host/css_l4.rs` (per surgery #15) | `audit/CENSUS-2026-05-03.md:103-122`, Lane 5 grammar-authoritative discipline |
| BA-G8 | Eight inline `#[cfg(test)]` blocks moved to `tests/`; `rg -nE '^#\[cfg\(test\)\]' crates/*/src/` returns zero | `audit/CENSUS-2026-05-03.md:381-399`, `feedback_no_inline_tests` |
| BA-G9 | Internal ratio: `JsonDocument::get<T>(twitter.json, &path)` ≤ 5× the BA-G1 eager parse cost on M1 Pro. Marked **non-SOTA** until SOTA admits a sonic-rs `get_unchecked(twitter, pointer![...])` measurement (per `audit/SOTA-2026-05-03.md:50-58` extension); BA closes the 4196× gap to ≤ 5× as internal regression evidence | `audit/RESTART-SKETCH-2026-05-03.md:31-41` (the BA `bbnf_get_twitter` 4196× gap) |
| BA-G10 | Generated-file LOC accounting: per-grammar pre-/post-BA LOC table emitted; per-wave windows (W1: ±0.5%; W3: unchanged; W4: `json.rs ≤ 3,700`, `bbnf.rs ≤ 22,000`, `css_l4.rs ≤ 110,000`, aggregate ≤ +5% from W2; W5a: `json.rs ≤ 2,200`; W5b: `css_l4.rs ≤ 100,000`; W5c: `bbnf.rs ≤ 19,000`; W5d: `google_sheets.rs ≤ 12,000`; W5e: `bnf.rs ≤ 3,000`, `csv.rs ≤ 1,500`, `ebnf.rs ≤ 7,000`, `css_pretty.rs ≤ 8,500`, `math.rs ≤ 800`); no overflow without justified cause | Lane 06 generated-code budget |

## Wave summary

The wave structure splits per directive §5 + the W5 sub-wave split per option (a) (per `docs/tranches/BA/audit/W5-substrate-identity-decision.md`): W3 → W3a (path triplet rename) + W3b (path-core extraction) + W3c (runtime relocation); W4 → W4a (private parse core + cursor elision) + W4b (public wrappers) + W4c (parse_with deletion, formerly W3.M5); W5 → W5a (JSON) + W5b (CSS L4) + W5c (BBNF) + W5d (Sheets) + W5e (cohort). BA tranche grows from 7 waves to 13 waves.

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BA.W0 | Layered re-org of `crates/core/src/` into the 9-directory layout (`source/`, `parse/`, `lower/`, `codegen/`, `runtime/`, `path/`, `pipeline/`, `host/`, `lib.rs`); `pipeline.rs` collapses INTO `pipeline/mod.rs`; `crates/core/src/css_types.rs` relocates to `crates/core/src/grammar/host/css_l4/css_types.rs` (per surgery #15); tape-residue comment scrub (~50 hits); `generate/serialize/` directory deleted (156 LOC); 8 inline tests migrated. | Layer-name canon (Lock 2 precursor); no god directories (Lock 13); no metalanguage docs. | `cargo check --workspace` green; `rg -n 'TapeRec\|TapeCursor\|TapeBuilder\|TapeOffset' crates/*/src/` returns zero outside `tests/`; per-grammar LOC table baseline emitted. |
| BA.W1 | Grammar-leak excision: `crates/ir/src/registry/strategy.rs:130-185` reads from `[workspace.metadata.bbnf-strategy]` per `docs/tranches/BA/audit/W1-workspace-metadata-schema.md`; recogniser plugin schema fields added (per surgery #16); `GrammarAuditTag` collapses to `Custom(&'static str)`; `shape_dict_bbnf.rs` deleted; BBNF shape mining generalises to data-driven recogniser registry. | bbnf-ir knows zero grammar idents (Lock 5 IR contract precursor). | `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/` returns zero matches; generated parser LOC unchanged ±0.5% (per surgery #G06-1). |
| BA.W2 | God-module splits: 23 files >500 LOC outside `generated/` partition per `audit/CENSUS-2026-05-03.md:319-353`. Layout-lowering rename: every `type_projection`/`type_collapsing`/`TypeMap`/`StructLayout`/`TypeDesc`/`schema_synthesis` reference rewrites to `layout_lowering`/`Layout`/`LayoutSink`. Transitional aliases DELETED in-wave (no `pub use` exception per surgery #3). Inverse-layout-audit gate (per surgery #17) — every compound-typed rule, including `->`-less rules, has `Layout` and reaches emitted fields. Fail-explicit table per `docs/tranches/BA/audit/W2-fail-explicit-table.md` (per surgery #18). BBNF aggregator `pub use bbnf::*` deleted (per `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md` and surgery #19). | Lock 2 (canon) + Lock 13 (no god modules). | `find crates -name '*.rs' ! -path '*/generated/*' -path '*/src/*' \| xargs wc -l \| awk '$1>500'` returns empty per `docs/tranches/BA/audit/W2-file-size-distribution.md`; layout-lowering term canonical; fail-explicit table green. |
| BA.W3a | Path crate rename: `crates/bbnf-path/` → `crates/path/`; `crates/bbnf-path-ts/` → `crates/path-ts/` (per surgery #6). Package names retained as `bbnf-path` and `bbnf-path-ts` (Cargo.toml `name` field unchanged); directory rename only. Every `cargo -p` gate cites the package name accordingly. | Lock 7 directory canon. | `crates/path/` and `crates/path-ts/` exist; `crates/bbnf-path*/` do not exist; `cargo check -p bbnf-path -p bbnf-path-ts` succeeds. |
| BA.W3b | Path-core extraction: `crates/path-core/` created with shared `compile`/`lex`/`lower`/`validate` logic; `bbnf-path` and `bbnf-path-ts` path-dep on `path-core`; ~500 LOC mirror eliminated; synthetic fixtures at `crates/path-ts/src/fixture.rs` (248 LOC) and `crates/path/src/registry.rs` (201 LOC) deleted. | Lock 7. | `crates/path-core/` exists with `compile.rs`, `lex.rs`, `lower.rs`, `validate.rs`; `wc -l crates/path/src/path_macro.rs` and `crates/path-ts/src/compile.rs` each ≤ 200 LOC. |
| BA.W3c | Runtime relocation: `crates/core/src/runtime/path.rs` (163 LOC, legacy borrowed alphabet) DELETED; `crates/core/src/path/` survives as the typed-path runtime per CENSUS:244 (NOT moved to `crates/path/src/runtime/` — surgery #7's "move into `crates/path/src/runtime/`" is rejected because `crates/core/src/path/` is the runtime executor consumed by Rust-emitter generated parsers; the path crate triplet is proc-macro / cdylib / shared-core, not runtime executor). | Lock 7. Three crate names. | `rg -n 'use crate::runtime::path::' crates/core/src/` returns zero; `test ! -f crates/core/src/runtime/path.rs`. |
| BA.W4a | Private parse core: codegen-time `__EAGER_EMPTY_PATH` constant-fold per W4a.M0 (cursor consultations elided when path is statically empty); cursor's `Skip` decision generates byte-skip code at codegen, not at runtime (W4a.M4). The four `runtime/<g>/parse_with.rs` legacy lowering passes (formerly W3.M5) are NOT yet deleted — that is W4c, scheduled after the unified surface lands. | Lock 3 precursor (cursor branch elided when path empty). | Generated eager path has zero `cursor.decide`/`cursor.current_kind`/`cursor.match_field` calls; samply cursor inclusive < 0.5% on eager path. |
| BA.W4b | Public wrappers + `Document::get<T>` reroute: emitter rewrites every grammar's entry to `pub fn parse_with(input: &str, path: &TypedPath<G>) -> Result<...>` and `pub fn parse(input: &str) -> Result<...> { parse_with(input, &EMPTY_PATH) }`. `JsonDocument::get<T>` reroutes through `parse_with(input, path)`; the post-parse linear walk vanishes. | Lock 3 (one parse impl, eager elides cursor). | BA-G9 met (`get<T>` ≤ 5× full parse); `rg -n 'pub fn parse_with' crates/core/src/grammar/generated/` returns ≥ 9. |
| BA.W4c | Legacy lowering deletion: the four `runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs` files (~480 LOC across four files) DELETE per `audit/CENSUS-2026-05-03.md:262`; the unified `parse_with` surface from W4b replaces them. (This is the formerly-W3.M5 surgery, repositioned per surgery #9 to land AFTER the unified surface, not before.) | No legacy alphabet survives. | `for f in json bbnf css_l4 google_sheets; do test ! -f crates/core/src/runtime/$f/parse_with.rs; done`. |
| BA.W5a | JSON direct-to-struct codegen first cut. Direct-projection emit replaces `OpenFrame` stack + `Vec<OpenFrame>::clone` for JSON per `docs/tranches/BA/audit/W5-generated-parser-shape.md`. Typed enum + per-rule generated `parse_<rule>(...)`; byte-disjoint Alt emits direct `match first { ... }`. `OpenFrame` deleted from JSON path. | Lock 1 (JSON path); Lock 3 (one parse impl); Lock 9 (slice-borrow primary). | BA-G1a (≤ 400 µs); BA-G2 (≤ 2 heap allocs/parse); samply post-W5a shows `Vec<OpenFrame>::clone` retired; `rg -n 'enum OpenFrame' crates/core/src/runtime/json/` returns 0. |
| BA.W5b | CSS L4 direct-to-struct migration. 14-variant typed-enum lands; per-variant emission for declaration / color / color_function / color_mix / selector_list / hex_color / etc. Lightning-css parity tests pass. CSS L4 OpenFrame retires. | Lock 1 (CSS L4 path); rich AST preserved. | BA-G1b (regression-bound bootstrap.css); `css_l4.rs ≤ 100,000`; `rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns 0. |
| BA.W5c | BBNF direct-to-struct migration. Pratt operator-chain emission for grammar's own operators. Self-host parse-and-format roundtrip passes. BBNF aggregator `pub use bbnf::*` deletion (surgery #19) closes. BBNF OpenFrame retires. | Lock 1 (BBNF path); rich AST preserved; no asymmetry. | BA-G1c (self-host roundtrip regression-bound); `bbnf.rs ≤ 19,000`; `rg -n 'enum OpenFrame' crates/core/src/runtime/bbnf/` returns 0; aggregator deleted. |
| BA.W5d | Google Sheets direct-to-struct migration. Per-leaf direct emission (cell_ref, identifier, sheet_prefix, error). cssparser-class parity asserted. Arena-fallback complexity (surgery #18 Sheets-side) resolves. Sheets OpenFrame retires. | Lock 1 (Sheets path); fail-explicit (no silent fallback). | BA-G1d (parse_simple regression-bound); `google_sheets.rs ≤ 12,000`; `rg -n 'enum OpenFrame' crates/core/src/runtime/google_sheets/` returns 0; arena-fallback resolved. |
| BA.W5e | Five-grammar cohort direct-to-struct migration (BNF, CSV, EBNF, CSS Pretty, Math). Five hand-written direct-to-struct modules per `feedback_no_deferrals` (BB.W2 consolidates into one parameterised template per gap D). `SimpleStructBuilder` template retires. Strategy resolver collapses (`EmitStrategy::OpenFrame` variant deletes). Lock 1 honoured at BA close. | Lock 1 (final); no orthogonal codepaths (one strategy). | BA-G1e (each cohort grammar regression-bound); per-grammar LOC ≤ targets; `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 across all 9 grammars. |
| BA.W6 | BA close: PROGRESS / FINAL; lock cross-reference table verified (Lock 1 honoured at BA close, not deferred); per-grammar generated-LOC table; carry ledger to BB named explicitly. | Lock-honoured-at-every-gate; carry-tags BA→BB explicit. | `cargo nextest run -p bbnf -p bbnf-ir -p bbnf-analysis` 100% pass for BA-owned surfaces; `substrate_audit` green; BA close artefacts emitted. |

## Pre-BA cleanup ceremony (precondition)

Lock 12 makes this ceremony a precondition for BA.W0; interleaving is a fault.

| Action | Detail | Reference |
|---|---|---|
| Move `crates/ser/` → `archive/ser/` | source preserved verbatim; workspace member removed | `audit/MODULES-2026-05-03.md:165-184` |
| Move `crates/gorgeous/` → `archive/gorgeous/` | source preserved verbatim; workspace member removed; per-grammar `prettify_<g>` shims travel with archive | `audit/MODULES-2026-05-03.md:188-212`, `audit/CENSUS-2026-05-03.md:151-162` |
| Edit root `Cargo.toml` workspace `members = [...]` | strike `crates/ser`, `crates/gorgeous` | `Cargo.toml:1-2` (current `members` array) |
| Single ceremony commit | message: `chore(workspace): archive ser + gorgeous (Lock 12)` | — |

**Verification gate** (must pass before BA.W0 dispatch): `cargo metadata --format-version 1 | jq '.workspace_members | length'` decreases by 2; `archive/ser/` and `archive/gorgeous/` exist on disk; `cargo check --workspace` succeeds. This is NOT a tranche. No PROGRESS.md, no FINAL.md, no waves. One commit and a verification.

## Carry-tags FROM prior tranche

None. BA is the restart; the pre-BA ceremony above is the precondition, not a tranche.

## Carry-tags TO BB

Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` option (a), BA owns the all-grammar OpenFrame migration; the per-grammar receivers BA→BB.C1a/b/c/d retire (Lock 1 is honoured at BA close, not deferred). The renumbered C1' carry covers cohort hand-written → BB.W2 template-consolidation only.

| Tag | Owner wave | Receiving wave | Description | Receiving gate |
|---|---|---|---|---|
| BA→BB.C1' | BA.W5e | BB.W2 | BA writes 5 hand-written direct-to-struct cohort modules at W5e (BNF, CSV, EBNF, CSS Pretty, Math); BB.W2 consolidates into 1 parameterised cohort template per gap D (`docs/tranches/BB/audit/W2-cohort-template-spec.md`). | BB.W2 cohort template byte-equal to W5e hand-written modules at first commit; `xtask regen --check` re-emits byte-identical output. |
| BA→BB.C2 | BA.W2 | BB references `Layout`/`LayoutSink` only | Layout-lowering rename canonises the IR pass name; BB references `Layout`/`LayoutSink` only. | `rg -nE 'TypeDesc\|StructLayout' crates/ir/src/passes/layout/` returns 0 |
| BA→BB.C3 | BA.W4b | BB.W2 | Cursor-unified `parse_with` + `__EAGER_EMPTY_PATH` substrate; BB extends invariants. Already met at BA close (every grammar carries `pub fn parse_with`). | `rg -n 'pub fn parse_with' crates/core/src/grammar/generated/` returns ≥ 9 (verified at BA close). |
| BA→BB.C4 | BA.W3b | BB.W5 | `path-core` crate exists; BB's `pointer!["a","b",1]` macro at `crates/path/` reuses it without proc-macro/cdylib mirror. | `cargo check -p path-core` passes; BB.W5 macro consumes |
| BA→BB.C5 | BA.W1 | BB.W3 | Grammar-agnostic `bbnf-ir`; BB's CSP/e-graph/miner extensions reference grammars only via `&str` ident through workspace metadata. | `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap' crates/ir/src/` returns 0 |
| BA→BB.C6 (Lock 4 deferral) | n/a | BB.W3 | Per-domain orthogonal optimisation: BB.W3 lands CSP → e-graph → miners → cost-model output-piping; no unified hypergraph. | BB-G10 |
| BA→BB.C7 (Lock 10 deferral) | n/a | BB.W3 | Pratt + SIMD auto-detected. (Note: BA.W5c lands BBNF Pratt and BA.W5e lands Math Pratt as per-grammar emission; BB.W3 generalises auto-detection across all grammars.) | BB-G6 |
| BA→BB.C8 (Lock 11 deferral) | n/a | BB.W0 | Path-deps for incubating sister crates: egraph + egraph-derive + csp-solver + bbnf-regex + parse-that. | BB.W0 path-dep gate |

## Carry-tags TO BC (skipping BB)

| Tag | Owner wave | Receiving wave | Description |
|---|---|---|---|
| BA→BC.C1 | BA.W2 | BC.W0 | Layout-lowering canon supports the IR contract spec BC.W0 codifies. |
| BA→BC.C2 | BA.W5a..W5e | BC.W0/BC.W1 | Direct-to-struct emitter pattern (one IR walker, leaf emission per backend) is the precursor to BC's `Emitter` trait formalisation across Rust/TS/WASM. The pattern is exercised across all 9 grammars at BA close. |

## 13-Lock honoured

Every cell names the wave that addresses the lock; deferred locks name the receiving wave + receiving gate per directive §10 honesty discipline.

| Lock | Wave | Notes |
|---|---|---|
| L1. Tape + columnar dead | W0 (residue scrub); W5a (JSON OpenFrame retiral); W5b (CSS L4 OpenFrame retiral); W5c (BBNF OpenFrame retiral); W5d (Sheets OpenFrame retiral); W5e (cohort OpenFrame retiral + strategy resolver collapse) | **Honoured at BA close** per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` option (a). All nine grammars retire `OpenFrame`. Closer-gate `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 at BA.W6.M5. Each sub-wave's contribution: W5a JSON (BA-G1a/G2 closer-gates); W5b CSS L4 (14-variant retiral; lightning-css parity); W5c BBNF (Pratt + self-host; aggregator deletion surgery #19); W5d Sheets (per-leaf direct emission; arena-fallback resolution surgery #18); W5e cohort (5 hand-written modules; `EmitStrategy::OpenFrame` deletion). |
| L2. Layout lowering canon | W2 (rename pass; transitional aliases DELETED in-wave per surgery #3) | Old terms (`type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis`) survive only in archived docs after BA.W2. BB references `Layout`/`LayoutSink` only via carry BA→BB.C2. |
| L3. Cursor + byte-skip unified | W4a (private parse core + eager elision); W4b (public wrappers); W4c (legacy deletion) | `__EAGER_EMPTY_PATH` LazyLock at BA.W4a is the unification point; W4b reroutes `parse` and `Document::get<T>`; W4c retires the four legacy `parse_with.rs` files. Already met at BA close — every grammar carries `pub fn parse_with`. |
| L4. Per-domain orthogonal optimisation | **Deferred-with-receiver**: BB.W3 (per BA→BB.C6) | BB.W3 lands CSP → e-graph → miners → cost-model output-piping; no unified hypergraph. BA does not touch the optimiser pipeline. |
| L5. IR + per-backend lower | W5a..W5e demonstrate the pattern across all 9 grammars (Rust emitter consumes IR-shape per grammar) | The Rust emitter consumes IR-shape in BA.W5a..W5e; BC.W0 codifies the IR contract for cross-backend (TS/WASM) lowering. |
| L6. xtask emits committed source | W0 (regen check ≤ 30 s; committed regen artefacts) | `bbnf-path`, `bbnf-path-ts` proc-macro shells are SEPARATE per Lock 7; not the codegen substrate. |
| L7. `crates/path/` consolidation | W3a (rename); W3b (path-core extraction); W3c (runtime path.rs deletion) | Three crate names (path, path-core, path-ts) only; no fourth proc-macro shell. BB.W5 lands `pointer!` macro production surface; BC.W5 reconciles `bbnf-regex` endpoint. |
| L8. Surpass sonic-rs / simdjson / lightning-css | G1a (≤ 400 µs twitter beating sonic-rs 436 µs) | The only SOTA parse-throughput gate at BA close is BA-G1a; BA-G1b/c/d/e are internal-progress (non-SOTA) per surgery #11. BA-G9's `get<T>` ratio is internal (non-SOTA) until SOTA admits sonic-rs `get_unchecked` measurement. BB and BC tighten the gates further (lightning-css 4.16 ms parity is BB-G1; etc.). |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | W5a (JSON `JsonValue<'p>`); W5b (CSS L4 `CssTypedValue<'p>` + `CowArcStr`); W5c (BBNF `BbnfValue<'p>`); W5d (Sheets `SheetsValue<'p>`); W5e (cohort `<G>Value<'p>`) | Default surface is `&'i str` slice + `Cow<'i, str>` per `audit/SOTA-2026-05-03.md:122-123` for every grammar. The arena is opaque collection storage; bumpalo opt-in via `parse_in` routes to BB.W4. |
| L10. Pratt + SIMD auto-detected | **Deferred-with-receiver**: BB.W3 (per BA→BB.C7) | No grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. BA does not touch operator-chain or structural-alphabet recognisers. |
| L11. Path-deps for incubating sister crates | **Deferred-with-receiver**: BB.W0 (per BA→BB.C8) | egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as path-deps; simd-scan + bootstrap + analysis + lsp stay workspace-internal. BC.W5 freezes sister-crate APIs. |
| L12. ser + gorgeous archive BEFORE BA.W0 | Pre-BA ceremony (precondition) | Verification: `archive/{ser,gorgeous}/` exist; `Cargo.toml` workspace members reduced by 2. Not a tranche; one ceremony commit + a verification gate. |
| L13. No god directories; cohesive encapsulation at every level | W0 (9-directory layered re-org); W2 (god-module splits, 23 → 0 production source); W3a/b/c (path crate triplet); W5b (CSS L4 14-variant per-frame split consumed) | sonic-rs / lightningcss / simdjson cohesion is the standard. Files >500 LOC outside `generated/` (under `crates/*/src/`) forbidden after BA.W2 per `docs/tranches/BA/audit/W2-file-size-distribution.md`. >10-children dirs mixing concerns forbidden after BA.W0. Test fixtures DEFER per CENSUS:122. |

## Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BA.W2 god-module splits cascade into BA.W5a..W5e codegen breakage (renames pull through emitter) | Medium | BA.W2 lands renames first; W4 cursor unification lands second; W5a..W5e regen cycles last per per-sub-wave gates; `cargo xtask regen --check` between every sub-wave. |
| BA.W5b CSS L4 14-variant migration breaks lightning-css parity tests | High | Per-variant migration: each of the 14 variants migrates independently; `cargo nextest run -p bbnf -E 'test(css_l4) + test(parse_with_css_l4)'` between each variant; rollback isolates the offending variant. |
| BA.W5c BBNF self-host roundtrip regresses because the post-W5c parser produces a different IR shape vs pre-W5c | High | Per `feedback_typed_materialization_invariant` every `->` reaches the emitter; if any rule's typed-equivalence breaks, root-cause via the IR-diff test (`bbnf-bootstrap dump_ir.rs` per MODULES:221). The migration order (W5a → W5b → W5c) ensures W5c executes against post-W5b emitter foundation. |
| BA.W5d arena-fallback resolution at M3 deletes a load-bearing arm | Medium | Per `audit/CENSUS-2026-05-03.md:193` the comment instructs INVESTIGATE; M3's verification reads each arm's reachability via codegen + runtime trace; `cargo expand`-equivalent inspection. |
| BA.W5e hand-written direct-to-struct emission for five grammars introduces per-grammar inconsistencies | Medium | Per `feedback_no_workarounds` the five modules are mechanical replicas of W5a/b/c/d patterns; M8's per-grammar parameter-table extract surfaces inconsistencies; BB.W2 consolidation enforces uniformity. |
| BA.W3a/b path consolidation breaks `pointer!` callers across `crates/core/tests/` | Medium | `path-core` is non-breaking (path-dep added); package names `bbnf-path` and `bbnf-path-ts` retained in Cargo.toml; the rename on disk is a directory change, not an API change. |
| Tape-residue scrub introduces parser regressions (some tape comments tied to lifetime narratives in safe-untouchable code) | Low | Comment-only scrub; zero functional source changes; `cargo nextest` between BA.W0 sub-commits. |
| Cursor-unified parse impl loses path-driven test coverage that exercises non-eager paths | Medium | BA.W4a/W4b retain `parse_with(input, path)` for non-eager; only the eager-fast-path constant-folds out; `tests/parse_with_*.rs` pass-rate is gated. |
| 13-wave tranche calendar grows from 7-wave estimate to 1.5-2x per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` §2 | Accepted (not mitigated) | The cost of honouring Lock 1 at BA close (option (a)) is real per-wave iter-time impact; each W5 sub-wave is bounded ≤ 75 s wall; aggregate W5 phase ~5 min. The cumulative tranche calendar grows; accepted as the cost of substrate honesty. |

## Build/iter time gate

BA shrinks generated/* LOC substantially under option (a). The tape-comment scrub eliminates repeated narrative blocks; the all-grammar `OpenFrame` retiral shrinks every grammar's generated module by mechanism per `audit/RESTART-SKETCH-2026-05-03.md:585-595`. BA.W2 god-module splits redistribute, not grow, the LOC mass. Estimated net delta to `crates/core/src/grammar/generated/` post-W5e: **−10.7%** per the table below.

xtask iteration-time gate (BA-G3): `cargo xtask regen --check` ≤ 30 s; CSS L4 `compile_paths_request` ≤ 25 s. Pre-BA baseline 59.98 s + 52.53 s per `audit/HARDENING-PLAN-2026-05-03-04-sota-anchoring.md` S04-2; halving is the BA close requirement. Per-W5 sub-wave: each sub-wave's regen is ≤ 30 s (per-grammar isolation). Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` §2 the cumulative W5 phase contributes ~5 min wall.

### Generated-LOC budget table (pre-BA baseline → post-BA target)

| Grammar | Pre-BA LOC | Post-BA LOC | Net Delta | Source |
|---|---:|---:|---:|---|
| `json.rs` | 3,500 | ~2,100 | −40% (W5a OpenFrame retiral) | `audit/MODULES-2026-05-03.md:621` |
| `bbnf.rs` | 21,503 | ~19,000 | −12% (W0 comment scrub + W5c OpenFrame retiral) | same:619 |
| `css_l4.rs` | 107,138 | ~100,000 | −7% (W0 scrub + W5b 14-variant retiral) | same:622 |
| `google_sheets.rs` | 14,088 | ~12,000 | −15% (W0 scrub + W5d OpenFrame retiral) | same:624 |
| `css_pretty.rs` | 9,021 | ~8,500 | −6% (W0 scrub + W5e OpenFrame retiral) | same:623 |
| `ebnf.rs` | 7,646 | ~7,000 | −8% (W0 scrub + W5e OpenFrame retiral) | same:625 |
| `bnf.rs` | 3,290 | ~3,000 | −9% (W0 scrub + W5e OpenFrame retiral) | same:626 |
| `csv.rs` | 1,693 | ~1,500 | −11% (W0 scrub + W5e OpenFrame retiral) | same:627 |
| `math.rs` | 871 | ~800 | −8% (W0 scrub + W5e OpenFrame retiral + Pratt) | same:628 |
| **TOTAL** | **168,750** | **~150,900** | **−10.7%** | aggregate |

Per-wave windows (per BA-G10 + surgery G06-1/G06-2/G06-3):

| Wave | Per-grammar window | Aggregate window |
|---|---|---|
| BA.W0 | comment-only scrub; ±0.5% per file | ±0.5% aggregate |
| BA.W1 | metadata sidecar only; no parser regen | ±0.5% aggregate |
| BA.W2 | source restructure only; no parser regen | unchanged |
| BA.W3a/b/c | path-crate restructure; no parser regen | unchanged |
| BA.W4a/b/c | parse_with rewrite per grammar | `json.rs ≤ 3,700`, `bbnf.rs ≤ 22,000`, `css_l4.rs ≤ 110,000`, aggregate ≤ +5% from W2 |
| BA.W5a | JSON direct-to-struct | `json.rs ≤ 2,200` |
| BA.W5b | CSS L4 direct-to-struct | `css_l4.rs ≤ 100,000` |
| BA.W5c | BBNF direct-to-struct | `bbnf.rs ≤ 19,000` |
| BA.W5d | Sheets direct-to-struct | `google_sheets.rs ≤ 12,000` |
| BA.W5e | Cohort direct-to-struct | `bnf.rs ≤ 3,000`, `csv.rs ≤ 1,500`, `ebnf.rs ≤ 7,000`, `css_pretty.rs ≤ 8,500`, `math.rs ≤ 800` |

Each wave's commit body MUST include a per-file `## Generated-LOC Budget` table. Overflow without justification blocks the wave.

## Phase-4 surgery ledger

| Surgery # | Description | Landed at | Verification |
|---|---|---|---|
| 1 | Delete CSS L4/BBNF/Sheets `OpenFrame` preservation; replace W5.M6 with `rg -n 'enum OpenFrame' crates/core/src/runtime/` returning 0 | BA.W5a + W5b + W5c + W5d + W5e (option (a) — full all-grammar migration within BA) | per-sub-wave closer-gates; BA.W6.M1 verifies aggregate `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 |
| 2 | Lock 1 substrate identity decision: option (a) (was option (b) per `docs/tranches/BA/audit/W5-substrate-identity-decision.md`); W5 split into W5a..W5e per-grammar | BA.md §13-Lock + W5-substrate-identity-decision.md + W5a..W5e | per-sub-wave closer-gates |
| 3 | Remove `TypeDesc`/`StructLayout` aliases; W2 close grep zero | BA.W2.M0 | W2.M0 exit-criteria |
| 6 | `crates/path`, `crates/path-core`, `crates/path-ts` directory canon | BA.W3a + W3b | W3a/W3b closer-gates |
| 7 | `crates/core/src/path/` is the typed-path runtime; `runtime/path.rs` retires | BA.W3c | W3c closer-gate |
| 9 | Move parse_with deletion from W3 to W4 | BA.W4c (was W3.M5) | W4c closer-gate |
| 10 | Split BA.W4 into W4a (private core) + W4b (public wrappers) | BA.W4a + W4b | W4a/W4b closer-gates |
| 11 | "Every parse-throughput gate cites..." + non-SOTA engineering table; per-grammar engineering gates BA-G1b/c/d/e marked internal-progress | BA.md §Hard gates rewrite | BA.md §Hard gates |
| 12 | BA-G9 marked non-SOTA OR add sonic-rs `get_unchecked` measurement | BA-G9 marked non-SOTA | BA.md §Hard gates row BA-G9 |
| 15 | CSS host fns to per-grammar host namespace | BA.W0.M1 (relocate to `crates/core/src/grammar/host/css_l4/css_types.rs`); BA.W5b host-fn path verification | W0.M1 exit-criteria; W5b.M1 |
| 16 | Recogniser plugin schema fields | BA.W1.M0 (per `docs/tranches/BA/audit/W1-workspace-metadata-schema.md` §4) | W1.M0 exit-criteria |
| 17 | Inverse-layout-audit gate | BA.W2.M5 + W5b.M1 (CSS L4 14-variant verification) | W2.M5 + W5b.M1 exit-criteria |
| 18 | Fail-explicit table; Sheets arena-fallback rows resolved | BA.W2.M4 + `docs/tranches/BA/audit/W2-fail-explicit-table.md` + BA.W5d.M3 (Sheets-side closure) | W2.M4 + W5d.M3 exit-criteria |
| 19 | BBNF aggregator `pub use bbnf::*` deletion | BA.W2.M4 + `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md` + BA.W5c.M0 (closure) | W2.M4 + W5c.M0 exit-criteria |
| 20 | Generated LOC gates per W1/W3/W4/W5a/W5b/W5c/W5d/W5e | per-wave §1 / §3 / §7 windows in W1, W3a/b/c, W4a/b/c, W5a/b/c/d/e | per-wave gates |
| 24 | BA.md C1 receiver retires per-grammar OpenFrame migration scope under option (a); renumbered C1' is cohort hand-written → BB.W2 template-consolidation only | BA.md §Carry-tags TO BB rewrite | §Carry-tags TO BB row BA→BB.C1' |
| 27 | Delete "slow-burn carry" or name receiver | BA.W1 §1 paragraph rewrite (no slow-burn carry; test fixture DEFER per CENSUS:121-122 named explicitly) | W1.md §1 |

## Voice locks

§V1. Voice is archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant", "extant"). Not corporate. Per `feedback_archaic_diction_is_voice`.

§V2. No metalanguage. Documents do NOT reference commits, conversation history, or the plan's draft history. Cite path:line. Per `feedback_no_metalanguage_docs`.

§V3. State the deliverable. State the gate. Move on. Per `feedback_no_workarounds`. No "consider", "may", "might".

§V4. Citations are path:line, not paraphrase. `audit/CENSUS-2026-05-03.md:104` not "the audit cites a CSS host fn issue".

§V5. Tables are liberal; markdown tables for every multi-row enumeration.

§V6. Per-X tables for every "all-X" claim. Per Operational Rule 1.

## Closing posture

Hereupon BA opens. The substrate is the typed-enum + slice-borrow across all nine grammars; the demonstration grammars are JSON (W5a) → CSS L4 (W5b) → BBNF (W5c) → Sheets (W5d) → cohort (W5e); the carry to BB is the renamed canonical surface (Layout/LayoutSink), the cursor-unified parse, the consolidated path-crate triplet, the grammar-agnostic IR, the all-grammar direct-to-struct emitter foundation, the cohort hand-written modules awaiting consolidation, and the three deferred-with-receiver locks (Locks 4, 10, 11). The 13 locks are settled; BA verifies the BA-owned cells in 13 waves (W0, W1, W2, W3a, W3b, W3c, W4a, W4b, W4c, W5a, W5b, W5c, W5d, W5e, W6) — the option (a) Lock 1 honour is the close — and hands BB a ratified foundation. Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` Lock 1 is honoured at BA close; the substrate dies in BA.
