# BB — Generality + Optimisation

## Gestalt

BB is the generality tranche: hereupon BA.W5's JSON direct-to-struct demonstration generalises to the eight remaining grammars across three carefully sub-waved courses; the five trivial cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) compress from ~2,000 LOC of mechanical instantiation to ~250 LOC by template emission; the four specialised grammars (BBNF, JSON, Sheets, CSS L4) reduce to typed enum + per-rule generated parse fns only. Every per-domain optimiser — CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, cost model — lives in its own crate (`csp-solver`, `egraph`, `egraph-derive`, `bbnf-regex`, `parse-that` as path-deps until APIs stabilise per Lock 11). Optimiser layering composes by output-piping; no unified hypergraph, per Lock 4. Pratt and SIMD auto-detect from grammar shape (left-recursive operator chains → Pratt; leaf-pattern shape → SIMD scanner) — no `@pratt` or `@simd` directives, per Lock 10. Slice-borrow becomes the primary API; `parse_in(input, &bump)` and `parse_owned(input)` are the lifetime escape hatches per Lock 9.

The sonic-class `pointer!["a","b",1]` macro at `crates/path/` lands the path-API surface; `LazyValue<'a>` borrowed views beget lazy materialisation; chumsky-style `.as_<T>()` projection on demand; lightning-css-style `Visitor<'i, T>` with `VisitTypes` bitflag for tree transforms (`audit/SOTA-2026-05-03.md:104-118`). CSS L4 surpasses lightningcss's 4.16 ms bootstrap parse by ≥ 15% (`audit/SOTA-2026-05-03.md:131-136`) and 43.37 ms tailwind parse by ≥ 18% on M1 Pro after local re-measurement of the parse-only surface lands per surgery 13. JSON tightens to ≤ 800 µs on citm_catalog and ≤ 3.0 ms on canada (`audit/SOTA-2026-05-03.md:50-58`). Per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-127`, the rank/tier rewrites do NOT land at BB.W0 — they move to BB.W3c with their consumer in the same wave, structurally precluding the Era V substrate-first/consumer-later anti-pattern (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:8-12`).

The BB.W0 sister-crate emigration further sub-divides per BB02-1 of `audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:39-41`: BB.W0a creates the path-deps + workspace metadata; BB.W0b fires a minimal optimiser smoke pass through the path-deps in the SAME wave so the substrate-first concern is structurally precluded. The BB.W1 specialised-grammar generalisation sub-divides per surgery 25 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:53-54` (resolving the "all eight remaining grammars" overclaim of `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:28`): BB.W1a (CSS L4), BB.W1b (BBNF), BB.W1c (Sheets) — each grammar gets its own gate.

## Hard gates

Every parse-throughput gate cites a specific competitor + dataset + platform per Lock 8; non-throughput engineering gates are separately labelled per S04-1 + S04-5 of `audit/HARDENING-PLAN-2026-05-03-04-sota-anchoring.md:21-31`. Zero "AU baseline" or "≥ pre-W3" gates appear in any cell.

### Parse-throughput gates (Lock 8)

| ID | Gate | Anchor |
|---|---|---|
| BB-G1 | `parse(bootstrap.css)` ≤ 3.5 ms on M1 Pro, beating local-re-measured lightningcss parse-only of bootstrap.css by ≥ 15%; the parse-only re-measurement lands at BB.W0a verification artefact `W0a-lightningcss-parse-only.md` per surgery 13 | `audit/SOTA-2026-05-03.md:131-136` (lightningcss bench table) |
| BB-G2 | `parse(tailwind.css)` ≤ 35 ms on M1 Pro, beating local-re-measured lightningcss parse-only of tailwind.css by ≥ 18%; if BB-G2 misses at BB.W3c close, the gap routes to BC.W5 with named rewrite hypothesis per surgery 32 — no silent carry | same; `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md` C03-13 |
| BB-G3 | `parse(citm_catalog.json)` ≤ 800 µs on M1 Pro, beating sonic-rs's 854 µs by ≥ 6% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs benchmark_aarch64 citm_catalog row) |
| BB-G4 | `parse(canada.json)` ≤ 3.0 ms on M1 Pro, beating sonic-rs's 3.144 ms by ≥ 4.5% | `audit/SOTA-2026-05-03.md:55` |

### Engineering gates (non-Lock-8)

| ID | Gate | Anchor |
|---|---|---|
| BB-G5 | Five cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) compress to ≤ 50 LOC of per-grammar mechanical instantiation each in `runtime/<g>/`; total `runtime/<g>/` shrinkage ≥ 1,500 LOC across the 5-grammar cohort | `audit/CENSUS-2026-05-03.md:507-528` |
| BB-G6 | Pratt + SIMD auto-detection: no grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. Tests assert that BBNF's `binary_factor` operator chain lowers via Pratt; JSON's structural alphabet (`{`,`}`,`[`,`]`,`,`,`:`) drives SIMD scanner emission | Lock 10 |
| BB-G7 | `pointer!(Json, ["a","b",1])` (mandatory marker form) and `pointer!["a","b",1]` (typed-context inference form) both resolve to `JsonPath<...>` with terminal type known at compile time per the BB.W5a syntax decision; runtime evaluation produces `LazyValue<'a>`; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand | Lock 9 + sonic-rs LazyValue surface, `audit/SOTA-2026-05-03.md:33-42`; surgery 35 |
| BB-G8 | `parse_in(input, &bump)` opt-in surface lands; `parse_owned(input)` opt-in surface lands; default `parse(input)` is slice-borrow `&'i str` | Lock 9 |
| BB-G9 | Visitor surface lands per grammar: `Visitor<'i, T>` trait + `VisitTypes` bitflag bitmask; CSS L4's `visit_color`, `visit_length`, `visit_url`, `visit_property` exposed; JSON's `visit_string`, `visit_number`, `visit_object`, `visit_array` exposed | `audit/SOTA-2026-05-03.md:104-118` (lightningcss visitor reference) |
| BB-G10 | Optimiser composition is output-piped: CSP infers types → e-graph rewrites → miners populate facts → cost model selects strategies; no unified hypergraph; each crate is a path-dep boundary | Lock 4, Lock 11 |
| BB-G11 | Generated-file LOC budget: per-wave windows enforced (table below); aggregate post-BB `crates/core/src/grammar/generated/` net delta ≤ +5% on specialised grammars; ≤ −60% on the 5-grammar cohort; aggregate ≤ −10% from BA close | Lane 06, Lock 13; surgery 21 |
| BB-G12 | Cookbook + diagnostic gates: `docs/cookbook/path-macro.md`, `docs/cookbook/lifetime-surfaces.md`, `docs/cookbook/visitors.md`, `docs/optimizer/pratt-simd-detection.md` lands; per-grammar docstrings cite by path:line | Lane 07, surgery 34 |

## Wave summary (with sub-waves per surgery 25 + BB02-1)

The 7-wave backbone (W0-W6) is preserved; sub-waves carry letter suffixes per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:241-256` §5.

| Wave | Deliverable | Invariant | Closer-gate | LOC window |
|---|---|---|---|---|
| BB.W0a | Sister-crate path-deps + workspace metadata: `crates/egraph/`, `crates/egraph-derive/`, `crates/csp-solver/`, `parse-that/rust/bbnf-regex/` (renamed from `regex`), `parse-that` itself emigrate to path-dep status; `[workspace.metadata.bbnf-incubators]` records canonical endpoints. **No rank/tier rewrites here** per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-115`. | Lock 11 (path-deps); precondition for BB.W0b same-wave consumer. | `cargo check --workspace --profile ax-iter` green; path-deps resolve; `Cargo.toml` declares no in-tree workspace-member duplicates; `test ! -f crates/ir/src/rewrites/rank.rs && test ! -f crates/ir/src/rewrites/tiering.rs`. | Generated parser LOC unchanged from BA close (W0a is structural). |
| BB.W0b | Same-wave consumer per BB02-1: a minimal optimiser smoke pass exercising csp-solver type inference + egraph saturation + bbnf-regex compilation on a representative grammar fixture, run through the BB.W0a path-deps. The pass is wired through the existing `crates/ir/src/passes/` pipeline (no new files); it produces a smoke-output JSON that BB.W3c verifies as fed-forward. | Era V abrogation (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:8-12`); Lock 4 precondition. | `cargo nextest run -p bbnf-ir --test sister_crate_smoke` 100% pass; smoke output written to `docs/tranches/BB/audit/W0b-sister-smoke.json`. | Generated parser LOC unchanged from W0a; W0b adds ≤ 50 LOC test fixture. |
| BB.W1a | CSS L4 direct-to-struct: 14-variant `OpenFrame` builder retires from `crates/core/src/runtime/css_l4/builder.rs`; per-variant migration with `tests/css_l4_parity.rs` lightningcss canonical-form parity at each step. | Lock 1 (direct-to-struct visible-and-internal for CSS L4); Lock 5 (per-backend lower demonstrated). | `rg -n 'enum OpenFrame' crates/core/src/runtime/css_l4/` returns zero; `tests/css_l4_parity.rs::full_canonical_form` passes; `cargo bench -p bbnf -- css_l4_bootstrap` ≤ 4.0 ms intermediate. | `css_l4.rs ≤ 98,000` (target) — net delta from W0 ≤ −7%; specialised-grammar window. |
| BB.W1b | BBNF direct-to-struct: `OpenFrame` retires from `crates/core/src/runtime/bbnf/builder.rs`; bounds-recording extension survives as per-rule emission concern. | Lock 1 (BBNF); Lock 5. | `rg -n 'enum OpenFrame' crates/core/src/runtime/bbnf/` returns zero; `tests/parse_with_bbnf.rs` 100% pass; `cargo nextest run -p bbnf-analysis` 100% pass (LSP bounds-reading consumer). | `bbnf.rs ≤ 20,500` (target) — net delta from W0 ≤ −2%; specialised-grammar window. |
| BB.W1c | Sheets direct-to-struct: `OpenFrame` retires from `crates/core/src/runtime/google_sheets/builder.rs`; specialised leaf-deposit logic survives as per-rule host fns at `crates/core/src/grammar/host/google_sheets.rs` (per surgery 15 + G05-9 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:32`). | Lock 1 (Sheets); Lock 5; G05-9 grammar-authoritative discipline. | `rg -n 'enum OpenFrame' crates/core/src/runtime/google_sheets/` returns zero; `tests/parse_with_google_sheets.rs` 100% pass; `find crates/core/src/host -name 'sheets.rs' \| wc -l` returns 0 (sheets host code lives at per-grammar namespace). | `google_sheets.rs ≤ 13,500` (target) — net delta from W0 ≤ −2%; specialised-grammar window. |
| BB.W2a | Cohort template specification + emit: the 5-grammar cohort (BNF, CSV, EBNF, CSS Pretty, Math) `runtime/<g>/{document,view,kind,value,mod}.rs` emit from a single codegen template at xtask-regen time; specification at `docs/tranches/BB/audit/W2-cohort-template-spec.md`; byte-equality precondition gate before deletion of hand-written files. | Lock 13 (cohesion); `feedback_pluggable_components`. | BB-G5 met (≥ 1,500 LOC saved); `crates/core/src/codegen/runtime_template.rs` exists; `diff -r crates/core/src/runtime/<cohort> <(xtask regen --grammar <cohort> --emit-only)` returns zero diff. | Cohort runtime ≤ 250 LOC aggregate (BB-G5); generated parser LOC ≤ −60% per cohort grammar. |
| BB.W2b | Cursor unification across all 9 grammars per C03-13 of `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:27`: each grammar's eager `parse(input)` rewrites as `parse_with(input, &__EAGER_EMPTY_PATH)`; the fast path elides cursor consultation per Lock 3. | Lock 3 (cursor unified across 9 grammars). | `tests/parse_with_<g>.rs` passes for all 9 grammars; samply trace shows zero cursor calls on each eager path; `rg -n 'cursor.decide\|cursor.current_kind\|cursor.match_field' crates/core/src/grammar/generated/` returns zero on eager paths. | No generated-LOC delta (W2b is a routing change). |
| BB.W2c | Byte-equal regression artefact: per surgery 25, the cohort template emission produces byte-identical output to the hand-written cohort modules at first commit; the artefact `docs/tranches/BB/audit/W2c-byte-equal-evidence.md` records pre/post diffs. | Migration evidence; Lock 6. | `git diff --stat HEAD~1..HEAD -- crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math}/` shows file deletions only, no content drift; the byte-equality precondition documented per BB.W2a M2. | Same as W2a; verification only. |
| BB.W3a | CSP layout passes path-dep wiring: the existing `crates/ir/src/passes/layout/` (renamed from `passes/types/` per BA→BB.C2 carry; surgery 5 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:49`) wires through `csp-solver` path-dep; output piped to W3b. | Lock 2 (Layout canon); Lock 4 (output-piping precondition). | `cargo nextest run -p bbnf-ir --test layout_pipe` 100% pass; `rg -n 'passes/types/' crates/ir/src/` returns zero; `rg -n 'TypeDesc\|StructLayout\|LayoutDesc' crates/ir/src/` returns zero. | +0% from W2 (renaming). |
| BB.W3b | E-graph + miners: `crates/egraph/` (path-dep) + miners (`recognizers/operator_chain.rs`, `passes/sets/structural_alphabet.rs`) feed the cost model. The miners are existing files per `audit/MODULES-2026-05-03.md:1218`; W3b wires them as facts producers. | Lock 4 (e-graph as own crate; output-piping). | `cargo nextest run -p bbnf-ir --test miner_facts` 100% pass; `crates/egraph/src/` is path-dep'd (verified by `cargo metadata`). | +0% from W3a (re-wiring). |
| BB.W3c | Pratt + SIMD detection + rank/tier rewrites with same-wave consumer per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:118-127`. `crates/ir/src/rewrites/{rank.rs,tiering.rs}` create here; the consumer (cost-model + CSP-strategy pipeline at `crates/ir/src/passes/csp_strategy/mod.rs`) wires in the SAME commit. Pratt detection at `recognizers/operator_chain.rs` and SIMD detection at `passes/sets/structural_alphabet.rs` extend with cost-model integration. F4 Tailwind disposition: if BB-G2 misses, the gap routes to BC.W5 with named path-shape rewrite hypothesis per surgery 32 + D08-13 of `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:22`. | Lock 4 + Lock 10 + Era V abrogation. | BB-G6 + BB-G1 + BB-G2 + BB-G3 + BB-G4 met (or BB-G2 routed); `git log -1 --stat -- crates/ir/src/rewrites/rank.rs` shows the consumer modified in the SAME commit; `crates/ir/src/passes/tests/substrate_audit.rs` green. | Specialised grammars +1 to +3% from W3a (Pratt + SIMD dispatch emit); cohort grammars +0%. |
| BB.W4a | `parse / parse_in / parse_owned` trait + surface: each grammar emits the three signatures; the inner per-rule parse fn is generic over `'i`; the three surfaces specialise the lifetime parameter differently. | Lock 9 (slice-borrow primary; bumpalo + owned escape hatches). | BB-G8 met; `cargo doc -p bbnf` shows the three surfaces × 9 grammars = 27 signatures with consistent docstring shape; `cargo bench -p bbnf -- json_twitter_parse_in` ≤ 420 µs (≤ +5% vs `parse`); `cargo bench -p bbnf -- json_twitter_parse_owned` ≤ 600 µs. | Specialised grammars wrapper delta ≤ +2% per surgery 21 (`docs/PHASE-4-DIRECTIVE-2026-05-03.md:52`); cohort same. |
| BB.W4b | Per-grammar test + cookbook + lifetime-surfaces.md gate: trybuild test fixtures verify the verbatim error message at lifetime mismatch; `docs/cookbook/lifetime-surfaces.md` lands; per-grammar docstrings cite the cookbook. | Lock 9 + friction-forecast Lane 7 (F07-2 of `audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:36`). | `test -f docs/cookbook/lifetime-surfaces.md`; `cargo doc -p bbnf 2>&1 \| grep -c 'cookbook/lifetime-surfaces'` ≥ 27; `cargo nextest run -p bbnf --test error_messages` 100% pass with verbatim text. | +0% (documentation only). |
| BB.W5a | `pointer!` macro + LazyValue: the `pointer!(Json, ["a","b",1])` mandatory-marker form and `pointer!["a","b",1]` typed-context inference form per the syntax decision at `docs/tranches/BB/audit/W5-pointer-syntax-decision.md`. Runtime evaluation produces `LazyValue<'a>`; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand. | Lock 7 (path crate consolidation) + Lock 9 (slice-borrow). | BB-G7 met; `cargo nextest run -p bbnf-path --test pointer_macro` 100% pass; `cargo bench -p bbnf -- lazy_value_twitter` ≤ 50 µs (≤ 0.1× full-parse, sonic-rs ratio). | Specialised grammars +0.5 to +1% (LazyValue surface). |
| BB.W5b | Visitor + VisitTypes per surgery 30 (`docs/PHASE-4-DIRECTIVE-2026-05-03.md:54`) — Visitor receiver is BC.W4 not BC.W5. Per-grammar `Visitor<'i, T>` + `VisitTypes` bitflag emits at xtask regen time; CSS L4 exposes `visit_color`, `visit_length`, `visit_url`, `visit_property`; JSON exposes `visit_string`, `visit_number`, `visit_object`, `visit_array`. The per-record method count is bounded by record count per surgery 21 of `audit/HARDENING-PLAN-2026-05-03-06-generated-code-budget.md` G06-4. | Lock 5 (per-backend lower precursor); G05-8 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:31` (typed pointer terminal). | BB-G9 met; `cargo doc -p bbnf 2>&1 \| grep -c 'pub trait .*Visitor'` returns 9; `cargo bench -p bbnf -- visitor_bootstrap_css` ≤ 5 ms (≤ 1.4× of parse, lightningcss ratio). | CSS L4 ≤ +5%; JSON +1%; others +1-2%; method-count = record-count. |
| BB.W5c | Cookbook + diagnostic gates per surgeries 34, 35: `docs/cookbook/path-macro.md` lands per F07-1 (`audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:35`); `docs/cookbook/visitors.md` lands per F07-6; `docs/optimizer/pratt-simd-detection.md` lands per F07-4. Each ≥ 200 LOC content (sections §1-§5). Verbatim `pointer!` ambiguity error message committed as trybuild fixture. | Friction-forecast Lane 7; surgery 34. | BB-G12 met; `test -f docs/cookbook/path-macro.md && test -f docs/cookbook/visitors.md && test -f docs/optimizer/pratt-simd-detection.md`; trybuild test fixtures verify the verbatim text. | +0% (documentation only). |
| BB.W6 | BB close: perf gates BB-G1..G4 met (or routed); PROGRESS / FINAL; cohort-grammar generated-LOC budget verified; carry ledger to BC.W0 named explicitly per `docs/tranches/BB/audit/W6-bc-carry-ledger.md`. | Lock-honoured at every gate; perf trajectory hits BC entry conditions. | `cargo nextest run -p bbnf -p bbnf-ir -p bbnf-analysis -p path -p path-core -p path-ts` 100% pass per C03-5 of `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:14` (the path-crate names corrected from `bbnf-path` to `path/path-core/path-ts`); bench harness produces post-BB.json archetype; the 13-lock cross-reference closes. | Aggregate net delta within forecast (-10% to -15%). |

## Per-grammar BB.W1 surgery 25 table

Per surgery 25 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:53`, the "all eight remaining grammars" overclaim of the prior draft (`audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:28`, fault C03-14) splits into per-grammar gates:

| Grammar | Sub-wave | Pre-W1 LOC (`runtime/<g>/builder.rs`) | OpenFrame variants | Receiving wave for parity test | Per-grammar gate |
|---|---|---:|---:|---|---|
| CSS L4 | BB.W1a | 1,014 (`audit/CENSUS-2026-05-03.md:464`) | 14 | `tests/css_l4_parity.rs` | `tests/css_l4_parity.rs::full_canonical_form` 100% pass |
| BBNF | BB.W1b | 243 (`audit/MODULES-2026-05-03.md:978`) | 8 | `tests/parse_with_bbnf.rs` + `cargo nextest run -p bbnf-analysis` | `enum OpenFrame` extinct from `runtime/bbnf/`; LSP bounds-reading consumer 100% pass |
| Sheets | BB.W1c | 357 (`audit/MODULES-2026-05-03.md:963`) | 6 | `tests/parse_with_google_sheets.rs` | `enum OpenFrame` extinct from `runtime/google_sheets/`; `crates/core/src/grammar/host/google_sheets.rs` exists per G05-9 |
| BNF | BB.W2a | (cohort templated, no OpenFrame) | n/a | byte-equality + `tests/parse_with_bnf.rs` | template-emitted; ≤ 50 LOC per cohort target |
| CSV | BB.W2a | same | n/a | `tests/parse_with_csv.rs` | same |
| EBNF | BB.W2a | same | n/a | `tests/parse_with_ebnf.rs` | same |
| CSS Pretty | BB.W2a | same | n/a | `tests/parse_with_css_pretty.rs` | same |
| Math | BB.W2a | same | n/a | `tests/parse_with_math.rs` | same |
| JSON | (already migrated at BA.W5) | (BA close = direct-to-struct) | 0 | `tests/parse_with_json.rs` | regression-free post-W0 path-dep relocation |

The table closes the cohort/specialised distinction: BB.W1{a,b,c} for the three remaining specialised grammars; BB.W2a for the 5-grammar cohort; JSON inherits from BA.W5 close. Each row carries its own gate; no "all eight" claim survives.

## Carry-tags FROM BA

Per `docs/tranches/BA/BA.md:55-63` and the synthesis-pass receiver corrections per surgery 24 (`docs/PHASE-4-DIRECTIVE-2026-05-03.md:44`).

| Tag | Owner-wave-in-BA | Description-of-what-BB-consumes | BB-side receiving wave |
|---|---|---|---|
| BA→BB.C1 | BA.W5 | Direct-to-struct codegen path for JSON; the emitter scaffolding is grammar-agnostic; only per-grammar typed value sums + parse fns regenerate. | BB.W1a (CSS L4), BB.W1b (BBNF), BB.W1c (Sheets), BB.W2a (5-grammar cohort) |
| BA→BB.C2 | BA.W2 | Layout-lowering rename canonises the IR pass name; BB references `Layout`/`LayoutSink` only — no `type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis` references survive in any BB document or source. | BB.W3a (path-dep wiring); referenced throughout |
| BA→BB.C3 | BA.W4 | Cursor-unified `parse_with` + `__EAGER_EMPTY_PATH` substrate; BB.W2b extends the unification to all 9 grammars in the same wave per C03-13. | BB.W2b |
| BA→BB.C4 | BA.W3 | `path-core` crate exists; BB.W5a's `pointer!` macro at `crates/path/` consumes it directly without proc-macro/cdylib mirror — the lex/lower/validate logic lives in `path-core`. | BB.W5a |
| BA→BB.C5 | BA.W1 | Grammar-agnostic `bbnf-ir`; BB.W3{a,b,c}'s CSP/e-graph/miner extensions reference grammars only via `&str` ident through workspace metadata. No `JsonParser`, `CssL4Parser`, `BbnfBootstrap`, `GoogleSheetsParser` arms in any BB-touched IR file. | BB.W3a, BB.W3b, BB.W3c |

## Carry-tags TO BC

Per `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:32-35` (ratified) and surgery 30 (Visitor receiver correction).

| Tag | Owner-wave-in-BB | Description-of-what-BC-consumes | Receiving wave in BC |
|---|---|---|---|
| BB→BC.C1 | BB.W3c | Optimiser composition (CSP → e-graph → miners → cost model) is output-piped. BC's IR contract specifies the contract between optimiser stages and the per-backend lowerer. | BC.W0 |
| BB→BC.C2 | BB.W1{a,b,c} + BB.W2a | Direct-to-struct emit shape is grammar-agnostic across the nine-grammar fleet. BC formalises this as the IR-input/typed-IR-output contract for the per-backend lowerer. | BC.W0 + BC.W1 |
| BB→BC.C3 | BB.W5b | Visitor + `VisitTypes` bitflag pattern is the per-backend lowerer's traversal API. BC's TS + WASM emitter scaffolds consume this contract via the `Emitter` trait. **Receiver is BC.W4 not BC.W5** per surgery 30. | BC.W4 |
| BB→BC.C4 | BB.W0a | Sister crates (egraph, egraph-derive, csp-solver, bbnf-regex, parse-that) are path-deps in workspace. BC.W5 promotes any whose API stabilises to crates.io candidates per L11 + D08-12 of `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:21`. | BC.W5 |

## 13-lock honoured

Every cell names the wave that addresses the lock; empty cells are faults. Notes column flags weak adherence or carry-deferral.

| Lock | Wave | Notes |
|---|---|---|
| L1. Tape + columnar dead | W1a (CSS L4 OpenFrame retiral); W1b (BBNF); W1c (Sheets); W2a (cohort template emission, no `OpenFrame` instantiation) | Era V columnar (`docs/tranches/AV/research/04-columnar-soa.md`) explicitly rejected; `OpenFrame` is the legacy substrate that retires across all nine grammars at BB close. JSON `OpenFrame` retired at BA.W5 (carry BA→BB.C1). |
| L2. Layout lowering canon | W3a (`passes/layout/` wiring; references `Layout`/`LayoutSink` only) | Old terms (`type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis`) survive only in archived docs; BA.W2 retired the aliases per BA02-1 of `audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:22`. Carried from BA.W2 (carry BA→BB.C2). |
| L3. Cursor + byte-skip unified | W2b (cursor-unified extends to all 9 grammars) | `__EAGER_EMPTY_PATH` LazyLock at BA.W4 is the unification point; BB.W2b generalises across the cohort + specialised. Carried from BA.W4 (carry BA→BB.C3). |
| L4. Per-domain orthogonal optimisation | W3a (CSP-via-csp-solver) → W3b (egraph + miners) → W3c (output-piped to cost model) | No unified hypergraph; each optimiser is its own crate (Lock 11). Each path-dep boundary is a structural seam. |
| L5. IR + per-backend lower | W1{a,b,c} generalises across 4 specialised grammars; W2a across the cohort; W3c IR contract precursor (rank/tier rewrites land with consumer); W5b Visitor as per-backend traversal API | The IR contract spec lands at BC.W0 (BB→BC.C2 carry); BB demonstrates the pattern at scale. |
| L6. xtask emits committed source | W1{a,b,c} + W2a (regen pipelines through xtask only; no proc-macro façade) | `crates/path/`, `crates/path-core/`, `crates/path-ts/` proc-macro shells are SEPARATE per Lock 7; not the codegen substrate. BB.W5a lands `pointer!` macro production surface at `crates/path/`. |
| L7. `crates/path/` consolidation | W5a (`pointer!` macro lands; LazyValue surface at `crates/path/`) | Three crate names (path, path-core, path-ts) only; no fourth proc-macro shell. Carried from BA.W3 (carry BA→BB.C4). BC.W5 reconciles `bbnf-regex` endpoint per L11 freeze + L7 consolidation; the routing was corrected from BC.W4 to BC.W5 per `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md` C03-10 + surgery 28. |
| L8. Surpass sonic-rs / simdjson / lightning-css | G1 (lightningcss bootstrap, parse-only re-measured); G2 (lightningcss tailwind, parse-only re-measured); G3 (sonic-rs citm); G4 (sonic-rs canada) | Zero AU references; every parse-throughput gate names competitor + dataset + platform. The lightningcss surface-mismatch fault per surgery 13 + S04-7 closes by local M1 Pro parse-only re-measurement at W0a. |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | W4a (the three surfaces: parse / parse_in / parse_owned); W5a (sonic-class LazyValue API) | Default surface is `&'i str` slice + `Cow<'i, str>` per `audit/SOTA-2026-05-03.md:122-123`. BA.W5 demonstrates the slice-borrow contract on JSON (carry BA→BB.C1). |
| L10. Pratt + SIMD auto-detected | W3c (operator_chain miner + structural_alphabet miner; cost model decides SIMD threshold) | No grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. Tests assert the absence of both directives in every grammar source under `grammar/`. |
| L11. Path-deps for incubating sister crates | W0a (egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as path-deps) | simd-scan + bootstrap + analysis + lsp stay workspace-internal. BC.W5 freezes APIs (carry BB→BC.C4). |
| L12. ser + gorgeous archive BEFORE BA.W0 | (carried-forward; precondition closed in pre-BA ceremony) | Verification: `archive/{ser,gorgeous}/` exist; `Cargo.toml` workspace members reduced by 2 at BA open. BB does not touch this surface. |
| L13. No god directories; cohesive encapsulation at every level | W2a (cohort template emission ≤ 50 LOC each); W3{a,b,c} (per-domain optimiser crates) | sonic-rs / lightningcss / simdjson cohesion is the standard. Files >500 LOC outside `generated/` forbidden after BA.W2 (carried). BB shrinks the cohort runtime mass; specialised grammars retain hand-written ≤500 LOC modules per file. |

## Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BB.W1a CSS L4 14-variant builder migration regresses lightningcss parity | High | Per-variant migration; `tests/css_l4_parity.rs` runs after each variant retiral; canonical-form bench against lightningcss's emit-CSS surface gates each step. |
| BB.W2a cohort template emission drops a behaviour the hand-written cohort modules silently exercised | Medium | Template emission generates byte-identical output to the existing hand-written 5-grammar files at first commit (artefact `W2c-byte-equal-evidence.md`); regression tests assert byte-equality before deletion of the hand-written files. |
| BB.W3c Pratt auto-detection misfires (classifies a non-Pratt rule as Pratt) | Medium | Fallback to non-Pratt emitter is the ground truth; the optimiser's classification has a false-positive cost that the cost model accounts for; tests enumerate all 9 grammars' rules and verify that only known operator chains route to Pratt. |
| BB.W3c SIMD auto-detection adds dispatch overhead on small inputs | Medium | Cost model has a `simd_threshold_bytes` parameter (no `@simd` directive); the threshold is grammar-derived from FIRST set + structural-alphabet density. |
| BB.W3c rank/tier rewrites land before consumer at the same-wave commit cadence (Era V anti-pattern resurgence) | Low | The amendment moves rank.rs + tiering.rs creation INTO the same commit as the consumer in BB.W3c; no skeleton-only commit in W0a per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-127`. The W3c verification artefact `W3c-rank-tier-with-consumer.md` records the atomic commit history. |
| BB.W5a `pointer!` macro depends on per-grammar registry being JSON-sidecar-stable | Low | BA.W3 finalised the registry sidecar (carry BA→BB.C4); BB.W5a consumes only. |
| BB.W4a three-surface API (parse / parse_in / parse_owned) confuses grammar authors choosing between bumpalo and owned | Medium | Per Lane 7 friction-forecast, the cookbook lands at `docs/cookbook/lifetime-surfaces.md` at BB.W4b with a decision flowchart; per-grammar docstrings cite the right surface for typical use; trybuild fixtures verify the verbatim error text. |
| BB.W2b cursor unification regresses 9-grammar test pass rate because the eager fast path interacts with grammar-specific behaviour | Low | `tests/parse_with_<g>.rs` exists for all 9 grammars; M3 of W2b runs each; samply trace verifies zero cursor calls on each eager path; per-grammar regression detection is mechanical. |

## Build/iter time gate

BB shrinks the cohort runtime by ~1,500 LOC; the specialised grammars (CSS L4, BBNF, Sheets, JSON) regenerate with direct-to-struct shape — net regen LOC may grow modestly (typed enum variants explode some payloads — for instance, CSS L4's `CssTypedValue` may grow as variants become explicit) but per-rule parse fns shrink (no `OpenFrame` ladder). Estimated net delta to `crates/core/src/grammar/generated/`: **+0% to +5%** on specialised grammars; **−60% to −70%** on cohort grammars. Aggregate net delta: **−10% to −15%** of total generated LOC.

xtask iteration-time gate: `cargo xtask regen --check` ≤ 25 s on M1 Pro. (BA close: ≤ 30 s; BB close: ≤ 25 s.) CSS L4 `compile_paths_request` ≤ 22 s.

### Per-wave generated-LOC window (surgery 21)

Per surgery 21 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:52` and G06-4 of `audit/HARDENING-PLAN-2026-05-03-06-generated-code-budget.md:49`, BB enforces wave-level (not just tranche-level) generated LOC windows:

| Wave | Generated parser LOC delta vs prior wave | Runtime template LOC delta | Notes |
|---|---:|---:|---|
| BB.W0a | unchanged | unchanged | Structural workspace change only. |
| BB.W0b | unchanged | +50 LOC test fixture | Smoke-pass test addition. |
| BB.W1a | `css_l4.rs` ≤ 98,000 (from BA close ~104,800; net ≤ −7%) | unchanged | CSS L4 OpenFrame retiral. |
| BB.W1b | `bbnf.rs` ≤ 20,500 (from BA close ~20,860; net ≤ −2%) | unchanged | BBNF OpenFrame retiral; bounds-recording survives. |
| BB.W1c | `google_sheets.rs` ≤ 13,500 (from BA close ~13,800; net ≤ −2%) | unchanged | Sheets OpenFrame retiral; host fns relocated to per-grammar namespace. |
| BB.W2a | cohort `<g>.rs` ≤ −60% per grammar | cohort runtime/<g>/ ≤ 50 LOC each (BB-G5) | Generated parser and runtime-template budgets separated per G06-7 of `audit/HARDENING-PLAN-2026-05-03-06-generated-code-budget.md:52`. |
| BB.W2b | unchanged | unchanged | Routing-only change. |
| BB.W2c | unchanged | hand-written file deletions (~−1,500 LOC) | Byte-equality precondition; deletions only after gate. |
| BB.W3a | unchanged | unchanged | Path-dep wiring only. |
| BB.W3b | unchanged | unchanged | Re-wiring miners as facts producers. |
| BB.W3c | specialised +1 to +3% (Pratt + SIMD dispatch emit); cohort +0% | +200 LOC test fixtures | Per-grammar Pratt + SIMD delta rows recorded in commit body. |
| BB.W4a | wrapper delta ≤ +2% per surgery 21 | unchanged | Three-surface API; the inner per-rule parse fn is shared. |
| BB.W4b | unchanged | unchanged | Cookbook + docstrings only. |
| BB.W5a | specialised +0.5 to +1% (LazyValue surface) | unchanged | `pointer!` macro lands at `crates/path/`. |
| BB.W5b | CSS L4 ≤ +5%; JSON +1%; others +1-2%; **method-count = record-count** per surgery 21 ("visitor delta bounded by record count") | unchanged | Visitor + VisitTypes per grammar. |
| BB.W5c | unchanged | unchanged | Documentation only. |
| BB.W6 | aggregate ≤ −10% from BA close | aggregate ≤ −1,500 LOC from BA close | Close-wave verification. |

### Per-grammar generated-LOC delta table (BA close → BB close)

| Grammar | BA-close LOC | BB-close LOC | Net Delta | Source-of-mechanism |
|---|---:|---:|---:|---|
| `json.rs` | ~2,100 | ~2,200 | +5% (Visitor + parse_in/parse_owned signatures) | `audit/SOTA-2026-05-03.md:33-42` |
| `bbnf.rs` | ~20,860 | ~19,800 | −5% (BBNF `OpenFrame` retiral, per `audit/CENSUS-2026-05-03.md:494-500`) net of +2% rank/tier emit | BB.W1b + W3c |
| `css_l4.rs` | ~104,800 | ~93,000 | −11% (CSS L4 14-variant `OpenFrame` retiral) net of +5% Visitor + typed enum explosion | `audit/CENSUS-2026-05-03.md:463-469`; BB.W1a + W5b |
| `google_sheets.rs` | ~13,800 | ~13,200 | −4% (Sheets `OpenFrame` retiral) net of +2% Visitor | `audit/CENSUS-2026-05-03.md:484-493`; BB.W1c |
| `css_pretty.rs` | ~8,930 | ~3,500 | −61% (cohort template emission) | `audit/CENSUS-2026-05-03.md:507-528`; BB.W2a |
| `ebnf.rs` | ~7,570 | ~3,000 | −60% (cohort template emission) | same |
| `bnf.rs` | ~3,257 | ~1,300 | −60% (cohort template emission) | same |
| `csv.rs` | ~1,676 | ~700 | −58% (cohort template emission) | same |
| `math.rs` | ~862 | ~350 | −59% (cohort template emission) | same |
| **TOTAL** | **~163,855** | **~134,700** | **−18%** | aggregate |

Each wave's commit body MUST include a per-file `## Generated-LOC Budget` table. Overflow without justification blocks the wave per Lane 06.

## Voice locks

§V1. Voice is archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant", "extant", "in fine", "thereafter"). Not corporate. Per `feedback_archaic-diction-is-voice`.

§V2. No metalanguage. Documents do NOT reference commits, conversation history, or the plan's draft history. Cite path:line. Per `feedback_no-metalanguage-docs`.

§V3. State the deliverable. State the gate. Move on.

§V4. Citations are path:line, not paraphrase. `audit/CENSUS-2026-05-03.md:507` not "the audit cites a cohort row".

§V5. Tables are liberal; markdown tables for every multi-row enumeration.

## Per-grammar perf trajectory through BB

| Grammar | Dataset | BA-close target | BB-close target | Competitor anchor |
|---|---|---|---|---|
| **JSON** | twitter.json | ≤ 400 µs (BA-G1) | ≤ 390 µs (BB tightens) | sonic-rs 436 µs `audit/SOTA-2026-05-03.md:53` |
| **JSON** | citm_catalog.json | (no BA gate) | ≤ 800 µs (BB-G3) | sonic-rs 854 µs `audit/SOTA-2026-05-03.md:54` |
| **JSON** | canada.json | (no BA gate) | ≤ 3.0 ms (BB-G4) | sonic-rs 3.144 ms `audit/SOTA-2026-05-03.md:55` |
| **CSS L4** | bootstrap.css | (no BA gate) | ≤ 3.5 ms (BB-G1; parse-only re-measurement at W0a) | lightningcss 4.16 ms `audit/SOTA-2026-05-03.md:133` |
| **CSS L4** | tailwind.css | (no BA gate) | ≤ 35 ms (BB-G2; parse-only re-measurement at W0a; route-to-BC.W5 if missed) | lightningcss 43.37 ms `audit/SOTA-2026-05-03.md:135` |
| **CSS L4** | animate.css | (no BA gate) | ≤ 1.7 ms (mineable) | lightningcss 1.97 ms `audit/SOTA-2026-05-03.md:134` |

The BBNF and Sheets perf rows are removed from this table per surgery 14 + S04-4 of `audit/HARDENING-PLAN-2026-05-03-04-sota-anchoring.md:26`: those grammars have no concrete external SOTA. They appear only in the cohort engineering gates (BB-G5, BB-G11), not in the parse-throughput table.

The BA flagship is JSON (the simplest schema with the highest per-byte parse cost); the BB flagship is CSS L4 (the largest grammar) + cohort templating; the BC flagship will be cross-grammar stability + IR contract codification.

## Sequencing discipline check (Lane 2 preview)

Per `docs/HARDENING-PLAN-PROMPT.md:70-80`, every wave's deliverable must have a same-wave or next-wave consumer; substrate-first/consumer-later is structurally precluded.

| Wave | Produces | Consumer | Same-wave or next-wave | Verdict |
|---|---|---|---|---|
| BB.W0a | Sister-crate path-deps | BB.W0b consumes via minimal optimiser smoke pass through the path-deps in the SAME wave | Same-wave (W0a → W0b) | OK (BB02-1 closed) |
| BB.W0b | Sister-crate smoke-output JSON | BB.W3c verifies feed-forward as cost-model input | Next-wave | OK |
| BB.W1a | CSS L4 direct-to-struct emit | Same wave: CSS L4 parity tests + bench | Same-wave | OK |
| BB.W1b | BBNF direct-to-struct emit | Same wave: BBNF parity + LSP bounds tests | Same-wave | OK |
| BB.W1c | Sheets direct-to-struct emit + host fn relocation | Same wave: Sheets parity tests; per-grammar host namespace verified | Same-wave | OK |
| BB.W2a | Cohort template emission; 5 cohort grammars compress | Same wave: byte-equality regression gate (W2c artefact); subsequent waves' tests run against the templated cohort | Same-wave | OK |
| BB.W2b | Cursor unification across all 9 grammars | Same wave: per-grammar parse_with tests; samply traces verify zero cursor calls on eager paths | Same-wave | OK |
| BB.W2c | Byte-equal evidence + hand-written deletions | Same wave: file deletions gated by W2c artefact; subsequent waves run against templated runtime | Same-wave | OK |
| BB.W3a | Layout-pass path-dep wiring | Same wave: layout_pipe test; W3b consumes layout output | Same-wave + next-wave | OK |
| BB.W3b | E-graph + miner facts | Same wave: miner_facts test; W3c consumes facts | Same-wave + next-wave | OK |
| BB.W3c | rank.rs + tiering.rs + Pratt + SIMD detection — all created with consumer in SAME commit | Same wave: cost-model + CSP-strategy pipeline consumes; perf gates verify | Same-wave | OK (Era V abrogation closed) |
| BB.W4a | parse / parse_in / parse_owned three-surface API | Same wave: API tests; BB.W4b cookbook + trybuild fixtures verify | Same-wave + next-wave | OK |
| BB.W4b | Lifetime cookbook + trybuild fixtures | Same wave: docstring citations + verbatim error message tests | Same-wave | OK |
| BB.W5a | `pointer!` macro + LazyValue | Same wave: per-grammar pointer tests; BC.W4 consumes the path-API surface | Same-wave + cross-tranche | OK |
| BB.W5b | Visitor + VisitTypes | Same wave: per-grammar visitor tests; BC.W4 consumes the visitor contract per surgery 30 | Same-wave + cross-tranche | OK |
| BB.W5c | Cookbook + diagnostic gates | Same wave: trybuild fixtures verify the verbatim text; cookbook content gates | Same-wave | OK |
| BB.W6 | BB close artefacts; carry-ledger to BC | BC.W0 entry preflight | Next-wave | OK |

No substrate-without-consumer in BB. The BB.W0 rank/tier skeleton concern (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-115`) is structurally precluded by W3c same-wave consumer rule. The BB.W0a sister-crate emigration concern (`audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:39-41` BB02-1) is closed by W0b's same-wave minimal optimiser smoke pass.

## Closing posture

Hereupon BB closes the generality gap. JSON's direct-to-struct demonstration begotten of BA.W5 generalises across all nine grammars in three sub-courses (specialised at W1{a,b,c}; cohort at W2a; cursor unification at W2b); the five-grammar cohort compresses to template emission; the per-domain optimisers compose by output-piping with rank/tier rewrites and consumer landing in the same wave at W3c; the slice-borrow / bumpalo / owned trifecta forms the lifetime escape hatch at W4a; the sonic-class `pointer!` + `LazyValue` lands at W5a; the lightning-css-class `Visitor` + `VisitTypes` lands at W5b; the cookbook + diagnostic surfaces land at W5c. The carry to BC is the typed-IR contract precursor (BB→BC.C2), the optimiser-pipe contract (BB→BC.C1), the visitor-traversal contract (BB→BC.C3, receiving wave BC.W4), and the path-dep'd sister crates ready for API freeze (BB→BC.C4, receiving wave BC.W5). The 13 locks remain settled; BB extends the foundation BA laid into a production-class generality surface.
