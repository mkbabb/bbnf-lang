# BB — Generality + Optimisation

## Gestalt

BB is the generality tranche: hereupon BA's JSON direct-to-struct path generalises to the remaining eight grammars; the five trivial cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) compress from ~2,000 LOC of mechanical instantiation to ~250 LOC by template emission; the four specialised grammars (BBNF, JSON, Sheets, CSS L4) reduce to typed enum + per-rule generated parse fns only. Every per-domain optimiser — CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, cost model — lives in its own crate (`csp-solver`, `egraph`, `egraph-derive`, `bbnf-regex`, `parse-that` as path-deps until APIs stabilise per Lock 11). Optimiser layering composes by output-piping; no unified hypergraph, per Lock 4. Pratt and SIMD auto-detect from grammar shape (left-recursive operator chains → Pratt; leaf-pattern shape → SIMD scanner) — no `@pratt` or `@simd` directives, per Lock 10. Slice-borrow becomes the primary API; `parse_in(input, &bump)` and `parse_owned(input)` are the lifetime escape hatches per Lock 9.

The sonic-class `pointer!["a","b",1]` macro at `crates/path/` lands the path-API surface; `LazyValue<'a>` borrowed views beget lazy materialisation; chumsky-style `.as_<T>()` projection on demand; lightning-css-style `Visitor<'i, T>` with `VisitTypes` bitflag for tree transforms (`audit/SOTA-2026-05-03.md:104-118`). CSS L4 surpasses lightningcss's 4.16 ms bootstrap parse by ≥ 15% (`audit/SOTA-2026-05-03.md:131-136`) and 43.37 ms tailwind parse by ≥ 18% on M1 Pro. JSON tightens to ≤ 800 µs on citm_catalog and ≤ 3.0 ms on canada (`audit/SOTA-2026-05-03.md:50-58`). Per `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-125`, the rank/tier rewrites do NOT land at BB.W0 — they move to BB.W3 with their consumer in the same wave, structurally precluding the Era V substrate-first/consumer-later anti-pattern (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:8-12`).

## Hard gates

Every gate cites a specific competitor + dataset + platform per Lock 8. Zero "AU baseline" or "≥ pre-W3" gates appear in any cell.

| ID | Gate | Anchor |
|---|---|---|
| BB-G1 | `parse(bootstrap.css)` ≤ 3.5 ms on M1 Pro, beating lightningcss's 4.16 ms by ≥ 15% | `audit/SOTA-2026-05-03.md:131-136` (lightningcss bench table) |
| BB-G2 | `parse(tailwind.css)` ≤ 35 ms on M1 Pro, beating lightningcss's 43.37 ms by ≥ 18% | same |
| BB-G3 | `parse(citm_catalog.json)` ≤ 800 µs on M1 Pro, beating sonic-rs's 854 µs by ≥ 6% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs benchmark_aarch64 citm_catalog row) |
| BB-G4 | `parse(canada.json)` ≤ 3.0 ms on M1 Pro, beating sonic-rs's 3.144 ms by ≥ 4.5% | `audit/SOTA-2026-05-03.md:55` |
| BB-G5 | Five cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) compress to ≤ 50 LOC of per-grammar mechanical instantiation each in `runtime/<g>/`; total `runtime/<g>/` shrinkage ≥ 1,500 LOC across the 5-grammar cohort | `audit/CENSUS-2026-05-03.md:507-528` |
| BB-G6 | Pratt + SIMD auto-detection: no grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. Tests assert that BBNF's `binary_factor` operator chain lowers via Pratt; JSON's structural alphabet (`{`,`}`,`[`,`]`,`,`,`:`) drives SIMD scanner emission | Lock 10 |
| BB-G7 | `pointer!["a","b",1]` macro at `crates/path/` resolves type-checked against the generated `pub const REGISTRY: StructRegistry`; runtime evaluation produces `LazyValue<'a>`; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand | Lock 9 + sonic-rs LazyValue surface, `audit/SOTA-2026-05-03.md:33-42` |
| BB-G8 | `parse_in(input, &bump)` opt-in surface lands; `parse_owned(input)` opt-in surface lands; default `parse(input)` is slice-borrow `&'i str` | Lock 9 |
| BB-G9 | Visitor surface lands per grammar: `Visitor<'i, T>` trait + `VisitTypes` bitflag bitmask; CSS L4's `visit_color`, `visit_length`, `visit_url`, `visit_property` exposed; JSON's `visit_string`, `visit_number`, `visit_object`, `visit_array` exposed | `audit/SOTA-2026-05-03.md:104-118` (lightningcss visitor reference) |
| BB-G10 | Optimiser composition is output-piped: CSP infers types → e-graph rewrites → miners populate facts → cost model selects strategies; no unified hypergraph; each crate is a path-dep boundary | Lock 4, Lock 11 |
| BB-G11 | Generated-file LOC budget: post-BB `crates/core/src/grammar/generated/` net delta ≤ +5% on specialised grammars; ≤ −60% on the 5-grammar cohort; aggregate ≤ −10% from BA close | Lane 06, Lock 13 |

## Wave summary

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BB.W0 | Sister-crate emigration: `crates/egraph/`, `crates/egraph-derive/`, `crates/csp-solver/`, `crates/bbnf-regex/` (currently `parse-that/rust/regex` → `parse-that/rust/bbnf-regex`), `parse-that` itself become path-deps in the workspace; documented as "incubating" until APIs stabilise. Workspace metadata records the canonical endpoints per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175`. simd-scan + bootstrap + analysis + lsp stay workspace-internal. **No rank/tier rewrites here** per `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-125`. | Lock 11 (path-deps); Lock 4 (optimiser-crate isolation precondition). | `cargo check --workspace` green; path-deps resolve; `Cargo.toml` declares no in-tree workspace-member duplicates; no `crates/ir/src/rewrites/rank.rs` or `tiering.rs` files exist. |
| BB.W1 | Direct-to-struct generalisation: extends BA.W5's JSON pattern to all eight remaining grammars. CSS L4, BBNF, Sheets each emit typed enum + per-rule generated parse fns; the existing 14-variant `OpenFrame` builder for CSS L4 retires; the 4-variant `OpenFrame` builder for JSON is already dead at BA.W5 close; BBNF and Sheets retire their `OpenFrame` variants here. | Lock 1 (direct-to-struct visible-and-internal across all grammars); Lock 5 (per-backend lower demonstrates IR shape across nine grammars). | `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns zero; per-grammar LOC budget per BB-G11 met; `cargo nextest run -p bbnf` 100% pass. |
| BB.W2 | Five-grammar cohort template emission: `runtime/{bnf,csv,ebnf,css_pretty,math}/{document,view,kind,value,mod}.rs` emit from a single codegen template at xtask-regen time. Each cohort grammar's `runtime/<g>/` shrinks from ~350-450 LOC to ≤ 50 LOC of mechanical instantiation. The four specialised grammars (BBNF, JSON, Sheets, CSS L4) keep hand-written modules. Cursor-unification (BA→BB.C3) extends to all grammars in the same wave. | Lock 13 (cohesion at every level); `feedback_pluggable_components`. | BB-G5 met (≥ 1,500 LOC saved); `crates/core/src/runtime/<simple>/{arena,builder,document,view,kind,value,mod}.rs` are template-emitted; cohort byte-equality regression tests pass before deletion of hand-written files. |
| BB.W3 | Optimiser pipeline per Lock 4. CSP type inference (`crates/ir/src/passes/types/`) → e-graph saturation (`crates/ir/src/egraph/`) → recogniser miners (`crates/ir/src/passes/recognizers/`) → CSP strategy selection (`crates/ir/src/passes/csp_strategy/`) → cost-model extraction (`crates/egraph/src/extract.rs`). Each is a path-dep crate; output piped, not fused. Pratt auto-detection lands as a recogniser (`recognizers/operator_chain.rs`); SIMD auto-detection lands as a structural-alphabet miner (`passes/sets/structural_alphabet.rs`) → simd-scan kernel selector. **Rank/tier rewrites land HERE in W3, with consumer**, per `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-125` — the substrate-first/consumer-later anti-pattern is structurally precluded. | Lock 4 + Lock 10. | BB-G6 (Pratt + SIMD auto-detection) met; tests assert no `@pratt` / `@simd` in any grammar source; `crates/ir/src/rewrites/{rank,tiering}.rs` exist + consumed in same wave; substrate_audit green. |
| BB.W4 | Slice-borrow primary API + escape hatches. Default `parse(input: &'i str) -> Result<<G>Value<'i>, ParseErr>`. `parse_in(input: &'i str, bump: &'i bumpalo::Bump) -> Result<<G>Value<'i>, ParseErr>` opt-in. `parse_owned(input: &str) -> Result<<G>OwnedValue, ParseErr>` opt-in. The three are surfaces over the same parse implementation; lifetime parameter is the discriminant. | Lock 9. | BB-G8 met; `cargo doc` shows the three surfaces with consistent docstring shape; per-wave generated-LOC budget verified. |
| BB.W5 | Sonic-class path API + visitor surface. `pointer!["a","b",1]` proc-macro at `crates/path/`; runtime evaluation produces `LazyValue<'a>`; `.as_str()`, `.as_i64()`, `.as_<T>()` per chumsky's pattern. lightning-css-style `Visitor<'i, T>` trait + `VisitTypes` bitflag + per-record `visit_<Name>(&mut self, &mut T)` methods. CSS L4 exposes `visit_color`, `visit_length`, `visit_url`, `visit_property`; JSON exposes `visit_string`, `visit_number`, `visit_object`, `visit_array`. | Lock 9 + sonic-class API anchor. | BB-G7 + BB-G9 met; `crates/path/src/path_macro.rs` (after split per BA.W2) emits `compile_path` resolving against `pub const REGISTRY`. |
| BB.W6 | BB close: perf gates BB-G1..G4 met; PROGRESS / FINAL; cohort-grammar generated-LOC budget verified; carry ledger to BC.W0 named explicitly. | Lock-honoured at every gate; perf trajectory hits BC entry conditions. | `cargo nextest run -p bbnf -p bbnf-ir -p bbnf-analysis -p bbnf-path` 100% pass; bench harness produces post-BB.json archetype; the 13-lock cross-reference closes. |

## Carry-tags FROM BA

Per `docs/tranches/BA/BA.md:55-63` and `docs/tranches/PLAN-INPUT-2026-05-03.md:80-86`.

| Tag | Owner-wave-in-BA | Description-of-what-BB-consumes |
|---|---|---|
| BA→BB.C1 | BA.W5 | Direct-to-struct codegen path for JSON; BB.W1 extends the emitter scaffolding to CSS L4, BBNF, Sheets, and BB.W2 to the 5-grammar template cohort. The emitter is grammar-agnostic; only per-grammar typed value sums + parse fns regenerate. |
| BA→BB.C2 | BA.W2 | Layout-lowering rename canonises the IR pass name; BB references `Layout`/`LayoutSink` only — no `type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis` references survive in any BB document or source. |
| BA→BB.C3 | BA.W4 | Cursor-unified `parse_with` + `__EAGER_EMPTY_PATH` substrate; BB.W2 extends the unification to all grammars, so every cohort grammar's eager `parse` rewrites as `parse_with(input, &EMPTY_PATH)`. |
| BA→BB.C4 | BA.W3 | `path-core` crate exists; BB.W5's `pointer!["a","b",1]` macro at `crates/path/` consumes it directly without proc-macro/cdylib mirror — the lex/lower/validate logic lives in `path-core`. |
| BA→BB.C5 | BA.W1 | Grammar-agnostic `bbnf-ir`; BB.W3's CSP/e-graph/miner extensions reference grammars only via `&str` ident through workspace metadata. No `JsonParser`, `CssL4Parser`, `BbnfBootstrap`, `GoogleSheetsParser` arms in any BB-touched IR file. |

## Carry-tags TO BC

Per `docs/tranches/PLAN-INPUT-2026-05-03.md:152-158`.

| Tag | Owner-wave-in-BB | Description-of-what-BC-consumes |
|---|---|---|
| BB→BC.C1 | BB.W3 | Optimiser composition (CSP → e-graph → miners → cost model) is output-piped. BC's IR contract specifies the contract between optimiser stages and the per-backend lowerer. |
| BB→BC.C2 | BB.W1 | Direct-to-struct emit shape is grammar-agnostic across the nine-grammar fleet. BC formalises this as the IR-input/typed-IR-output contract for the per-backend lowerer. |
| BB→BC.C3 | BB.W5 | Visitor + `VisitTypes` bitflag pattern is the per-backend lowerer's traversal API. BC's TS + WASM emitter scaffolds consume this contract via the `Emitter` trait. |
| BB→BC.C4 | BB.W0 | Sister crates (egraph, egraph-derive, csp-solver, bbnf-regex, parse-that) are path-deps in workspace. BC.W4 promotes any whose API stabilises to crates.io candidates. |

## 13-lock honoured

Every cell names the wave that addresses the lock; empty cells are faults. Notes column flags weak adherence or carry-deferral.

| Lock | Wave | Notes |
|---|---|---|
| L1. Tape + columnar dead | W1 (CSS L4 + BBNF + Sheets `OpenFrame` retiral); W2 (cohort template emission, no `OpenFrame` instantiation) | Era V columnar (`docs/tranches/AV/research/04-columnar-soa.md`) explicitly rejected; `OpenFrame` is the legacy substrate that retires across all nine grammars at BB close. JSON `OpenFrame` retired at BA.W5 (carry BA→BB.C1). |
| L2. Layout lowering canon | W3 references `Layout`/`LayoutSink` only | Old terms (`type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis`) survive only in archived docs. Carried from BA.W2 (carry BA→BB.C2). |
| L3. Cursor + byte-skip unified | W2 (cursor-unified extends to all grammars) | `__EAGER_EMPTY_PATH` LazyLock at BA.W4 is the unification point; BB.W2 generalises across the cohort. Carried from BA.W4 (carry BA→BB.C3). |
| L4. Per-domain orthogonal optimisation | W3 (CSP → e-graph → miners → cost-model output-piped) | No unified hypergraph; each optimiser is its own crate (Lock 11). Each path-dep boundary is a structural seam. |
| L5. IR + per-backend lower | W1 generalises across 9 grammars; W3 IR contract precursor (rank/tier rewrites land with consumer) | The IR contract spec lands at BC.W0 (BB→BC.C2 carry); BB demonstrates the pattern at scale. |
| L6. xtask emits committed source | W1 W2 (regen pipelines through xtask only; no proc-macro façade) | `crates/path/`, `crates/path-core/`, `crates/path-ts/` proc-macro shells are SEPARATE per Lock 7; not the codegen substrate. BB.W5 lands `pointer!` macro production surface at `crates/path/`. |
| L7. `crates/path/` consolidation | W5 (`pointer!` macro lands; LazyValue surface) | Three crate names (path, path-core, path-ts) only; no fourth proc-macro shell. Carried from BA.W3 (carry BA→BB.C4). BC.W4 reconciles `bbnf-regex` endpoint per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175`. |
| L8. Surpass sonic-rs / simdjson / lightning-css | G1 (lightningcss bootstrap); G2 (lightningcss tailwind); G3 (sonic-rs citm); G4 (sonic-rs canada) | Zero AU references; every gate names competitor + dataset + platform. BC tightens further. |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | W4 (the three surfaces: parse / parse_in / parse_owned); W5 (sonic-class LazyValue API) | Default surface is `&'i str` slice + `Cow<'i, str>` per `audit/SOTA-2026-05-03.md:122-123`. BA.W5 demonstrates the slice-borrow contract on JSON (carry BA→BB.C1). |
| L10. Pratt + SIMD auto-detected | W3 (operator_chain miner + structural_alphabet miner; cost model decides SIMD threshold) | No grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. Tests assert the absence of both directives in every grammar source under `grammar/`. |
| L11. Path-deps for incubating sister crates | W0 (egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as path-deps) | simd-scan + bootstrap + analysis + lsp stay workspace-internal. BC.W4 freezes APIs (carry BB→BC.C4). |
| L12. ser + gorgeous archive BEFORE BA.W0 | (carried-forward; precondition closed in pre-BA ceremony) | Verification: `archive/{ser,gorgeous}/` exist; `Cargo.toml` workspace members reduced by 2 at BA open. BB does not touch this surface. |
| L13. No god directories; cohesive encapsulation at every level | W2 (cohort template emission ≤ 50 LOC each); W3 (per-domain optimiser crates) | sonic-rs / lightningcss / simdjson cohesion is the standard. Files >500 LOC outside `generated/` forbidden after BA.W2 (carried). BB shrinks the cohort runtime mass; specialised grammars retain hand-written ≤500 LOC modules per file. |

## Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BB.W1 CSS L4 14-variant builder migration regresses lightningcss parity | High | Per-variant migration; `tests/css_l4_parity.rs` runs after each variant retiral; canonical-form bench against lightningcss's emit-CSS surface gates each step. |
| BB.W2 cohort template emission drops a behaviour the hand-written cohort modules silently exercised | Medium | Template emission generates byte-identical output to the existing hand-written 5-grammar files at first commit; regression tests assert byte-equality before deletion of the hand-written files. |
| BB.W3 Pratt auto-detection misfires (classifies a non-Pratt rule as Pratt) | Medium | Fallback to non-Pratt emitter is the ground truth; the optimiser's classification has a false-positive cost that the cost model accounts for; tests enumerate all 9 grammars' rules and verify that only known operator chains route to Pratt. |
| BB.W3 SIMD auto-detection adds dispatch overhead on small inputs | Medium | Cost model has a `simd_threshold_bytes` parameter (no `@simd` directive); the threshold is grammar-derived from FIRST set + structural-alphabet density. |
| BB.W3 rank/tier rewrites land before consumer at the same-wave commit cadence (Era V anti-pattern resurgence) | Low | The amendment moves rank.rs + tiering.rs creation INTO the same commit as the consumer in BB.W3; no skeleton-only commit in W0 per `audit/HARDENING-SYNTHESIS-2026-05-03.md:107-127`. |
| BB.W5 `pointer!` macro depends on per-grammar registry being JSON-sidecar-stable | Low | BA.W3 finalised the registry sidecar (carry BA→BB.C4); BB.W5 consumes only. |
| BB.W4 three-surface API (parse / parse_in / parse_owned) confuses grammar authors choosing between bumpalo and owned | Medium | Per Lane 7 friction-forecast, the cookbook lands at `docs/cookbook/lifetime-surfaces.md` with a decision flowchart; per-grammar docstrings cite the right surface for typical use. |

## Build/iter time gate

BB shrinks the cohort runtime by ~1,500 LOC; the specialised grammars (CSS L4, BBNF, Sheets, JSON) regenerate with direct-to-struct shape — net regen LOC may grow modestly (typed enum variants explode some payloads — for instance, CSS L4's `CssTypedValue` may grow as variants become explicit) but per-rule parse fns shrink (no `OpenFrame` ladder). Estimated net delta to `crates/core/src/grammar/generated/`: **+0% to +5%** on specialised grammars; **−60% to −70%** on cohort grammars. Aggregate net delta: **−10% to −15%** of total generated LOC.

xtask iteration-time gate: `cargo xtask regen --check` ≤ 25 s on M1 Pro. (BA close: ≤ 30 s; BB close: ≤ 25 s.) CSS L4 `compile_paths_request` ≤ 22 s.

### Per-grammar generated-LOC delta table (BA close → BB close)

| Grammar | BA-close LOC | BB-close LOC | Net Delta | Source-of-mechanism |
|---|---:|---:|---:|---|
| `json.rs` | ~2,100 | ~2,200 | +5% (Visitor + parse_in/parse_owned signatures) | `audit/SOTA-2026-05-03.md:33-42` |
| `bbnf.rs` | ~20,860 | ~19,800 | −5% (BBNF `OpenFrame` retiral, per `audit/CENSUS-2026-05-03.md:494-500`) net of +2% rank/tier emit | BB.W1 + W3 |
| `css_l4.rs` | ~104,800 | ~93,000 | −11% (CSS L4 14-variant `OpenFrame` retiral) net of +5% Visitor + typed enum explosion | `audit/CENSUS-2026-05-03.md:463-469`; BB.W1 + W5 |
| `google_sheets.rs` | ~13,800 | ~13,200 | −4% (Sheets `OpenFrame` retiral) net of +2% Visitor | `audit/CENSUS-2026-05-03.md:484-493`; BB.W1 |
| `css_pretty.rs` | ~8,930 | ~3,500 | −61% (cohort template emission) | `audit/CENSUS-2026-05-03.md:507-528`; BB.W2 |
| `ebnf.rs` | ~7,570 | ~3,000 | −60% (cohort template emission) | same |
| `bnf.rs` | ~3,257 | ~1,300 | −60% (cohort template emission) | same |
| `csv.rs` | ~1,676 | ~700 | −58% (cohort template emission) | same |
| `math.rs` | ~862 | ~350 | −59% (cohort template emission) | same |
| **TOTAL** | **~163,855** | **~134,700** | **−18%** | aggregate |

Each wave's commit body MUST include a per-file `## Generated-LOC Budget` table. Overflow without justification blocks the wave per Lane 06.

## Voice locks

§V1. Voice is archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant", "extant", "in fine", "thereafter"). Not corporate. Per `feedback_archaic_diction_is_voice`.

§V2. No metalanguage. Documents do NOT reference commits, conversation history, or the plan's draft history. Cite path:line. Per `feedback_no_metalanguage_docs`.

§V3. State the deliverable. State the gate. Move on.

§V4. Citations are path:line, not paraphrase. `audit/CENSUS-2026-05-03.md:507` not "the audit cites a cohort row".

§V5. Tables are liberal; markdown tables for every multi-row enumeration.

## Per-grammar perf trajectory through BB

Per `docs/tranches/PLAN-INPUT-2026-05-03.md:329-342`:

| Grammar | Dataset | BA-close target | BB-close target | Competitor anchor |
|---|---|---|---|---|
| **JSON** | twitter.json | ≤ 400 µs (BA-G1) | ≤ 390 µs (BB tightens) | sonic-rs 436 µs `audit/SOTA-2026-05-03.md:53` |
| **JSON** | citm_catalog.json | (no BA gate) | ≤ 800 µs (BB-G3) | sonic-rs 854 µs `audit/SOTA-2026-05-03.md:54` |
| **JSON** | canada.json | (no BA gate) | ≤ 3.0 ms (BB-G4) | sonic-rs 3.144 ms `audit/SOTA-2026-05-03.md:55` |
| **CSS L4** | bootstrap.css | (no BA gate) | ≤ 3.5 ms (BB-G1) | lightningcss 4.16 ms `audit/SOTA-2026-05-03.md:133` |
| **CSS L4** | tailwind.css | (no BA gate) | ≤ 35 ms (BB-G2) | lightningcss 43.37 ms `audit/SOTA-2026-05-03.md:135` |
| **CSS L4** | animate.css | (no BA gate) | ≤ 1.7 ms (mineable) | lightningcss 1.97 ms `audit/SOTA-2026-05-03.md:134` |
| **BBNF (self-host)** | bbnf.bbnf | (no BA gate) | (no specific gate; cohort optimiser benefits apply) | (no external SOTA — bbnf is the self-grammar) |
| **Sheets** | sheets-stress.bbnf | (no BA gate) | (≤ ½ sheet parse time vs current; mineable target) | cssparser ~600 MB/s `audit/SOTA-2026-05-03.md:122` |
| **Cohort** | grammar-author fixtures | (no BA gate) | (≤ 50 LOC instantiation each per BB-G5) | n/a |

The BA flagship is JSON (the simplest schema with the highest per-byte parse cost); the BB flagship is CSS L4 (the largest grammar) + cohort templating; the BC flagship will be cross-grammar stability + IR contract codification.

## Sequencing discipline check (Lane 2 preview)

Per `docs/HARDENING-PLAN-PROMPT.md:70-80`, every wave's deliverable must have a same-wave or next-wave consumer; substrate-first/consumer-later is structurally precluded.

| Wave | Produces | Consumer | Same-wave or next-wave | Verdict |
|---|---|---|---|---|
| BB.W0 | Sister-crate emigration to path-deps | BB.W3 (optimiser pipeline consumes the path-dep'd egraph + csp-solver + bbnf-regex) | Next-wave; workspace cargo check is the same-wave gate | OK |
| BB.W1 | Direct-to-struct emit for CSS L4, BBNF, Sheets | Same wave: per-grammar test + bench gates verify | Same-wave | OK |
| BB.W2 | Cohort template emission; 5 cohort grammars compress | Same wave: byte-equality regression gate; subsequent waves' tests run against the templated cohort | Same-wave | OK |
| BB.W3 | CSP + e-graph + miners + cost-model output-piping; Pratt + SIMD auto-detection; rank/tier rewrites | Same wave: perf gates (BB-G1..G4 + auto-detection tests); rank/tier rewrites consumed in same wave per `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-127` | Same-wave | OK (substrate-first concern closed by in-wave consumer) |
| BB.W4 | parse / parse_in / parse_owned three-surface API | Same wave: API tests; BB.W5 consumes the three surfaces in macro emission | Same-wave + next-wave | OK |
| BB.W5 | `pointer!` macro + LazyValue + Visitor surface | Same wave: per-grammar visitor tests; BC.W5 consumes the visitor surface for parity tests against lightningcss | Same-wave + cross-tranche | OK |
| BB.W6 | BB close artefacts; carry-ledger to BC | BC.W0 entry preflight | Next-wave | OK |

No substrate-without-consumer in BB. The previously-flagged BB.W0 rank/tier skeleton concern (`audit/HARDENING-SYNTHESIS-2026-05-03.md:107-115`) is resolved by the amendment that moves rank.rs + tiering.rs creation INTO BB.W3 with same-wave consumer.

## Closing posture

Hereupon BB closes the generality gap. JSON's direct-to-struct demonstration begotten of BA.W5 generalises across all nine grammars; the five-grammar cohort compresses to template emission; the per-domain optimisers compose by output-piping with rank/tier rewrites and consumer landing in the same wave; the slice-borrow / bumpalo / owned trifecta forms the lifetime escape hatch; the sonic-class `pointer!` + `LazyValue` and lightning-css-class `Visitor` lands. The carry to BC is the typed-IR contract precursor (BB→BC.C2), the optimiser-pipe contract (BB→BC.C1), the visitor-traversal contract (BB→BC.C3), and the path-dep'd sister crates ready for API freeze (BB→BC.C4). The 13 locks remain settled; BB extends the foundation BA laid into a production-class generality surface.
