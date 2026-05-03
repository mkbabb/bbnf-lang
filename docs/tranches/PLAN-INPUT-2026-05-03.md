# PLAN-INPUT — BA-Restart Scope Allocation

Date: 2026-05-03
Audience: the three Phase 2 plan agents (BA, BB, BC drafters)
Source-of-truth references:
- 13 locks: `docs/HARDENING-PLAN-PROMPT.md` §Gestalt and the user-imposed Lock 13 (no god directories).
- Codebase audit: `audit/HARDENING-SYNTHESIS-2026-05-03.md`.
- SOTA anchors: `audit/SOTA-2026-05-03.md`.
- Mechanical kill-list: `audit/CENSUS-2026-05-03.md`.
- Per-file fates + 17-step pipeline: `audit/MODULES-2026-05-03.md`.
- JSON parse trace + post-restart sketch: `audit/RESTART-SKETCH-2026-05-03.md`.
- Era IV peak (tape arc): `docs/tranches/meta-audit/archaeology/era-IV-tape-first.md`.
- Era V failure mode (substrate-first/consumer-later): `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`.
- Lock 1 evidence (designed-but-never-activated columnar): `docs/tranches/AV/research/04-columnar-soa.md`.

This document is **scope allocation, gate specification, and carry-tag interface** for BA / BB / BC. It does not draft the tranches. It tells the three Phase 2 agents what their tranche covers, what each tranche carries to and from the others, and where each of the 13 locks is honoured. The Phase 2 agents draft from this scaffolding; they do not invent scope.

---

## §1 — Tranche Scope Allocation

### §1.0 Pre-BA cleanup ceremony (precondition; not a tranche)

Lock 12 makes this ceremony a precondition for BA.W0; interleaving is a fault.

| Action | Detail | Reference |
|---|---|---|
| Move `crates/ser/` → `archive/ser/` | source preserved verbatim; workspace member removed | `audit/MODULES-2026-05-03.md:165-184` |
| Move `crates/gorgeous/` → `archive/gorgeous/` | source preserved verbatim; workspace member removed; per-grammar `prettify_<g>` shims travel with archive | `audit/MODULES-2026-05-03.md:188-212`, `audit/CENSUS-2026-05-03.md:151-162` |
| Edit root `Cargo.toml` workspace `members = [...]` | strike `crates/ser`, `crates/gorgeous` | `Cargo.toml:1-2` (current `members` array) |
| Single ceremony commit | message: `chore(workspace): archive ser + gorgeous (Lock 12)` | — |

**Verification gate** (must pass before BA.W0 dispatch): `cargo metadata --format-version 1 | jq '.workspace_members | length'` decreases by 2; `archive/ser/` and `archive/gorgeous/` exist on disk; `cargo check --workspace` succeeds.

This is NOT a tranche. No PROGRESS.md, no FINAL.md, no waves. One commit and a verification.

---

### §1.1 BA — Surgical Foundation

#### Gestalt

BA is the foundation tranche: hereupon the substrate is rebuilt from first principles, the grammar-specific leaks are excised from supposedly-generic crates, the god modules and god directories are partitioned per Lock 13, and JSON's direct-to-struct codegen lands as the demonstration of the new path. The tape's residue — comments, doc strings, dead `generate/serialize/` directory — is scrubbed. The cursor-parse and byte-skip implementations are unified into one parse function with `__EAGER_EMPTY_PATH` electing the fast path. The eight inline `#[cfg(test)]` violations migrate. The path-crate triplet (`bbnf-path` proc-macro, `bbnf-path-ts` cdylib, `crates/core/src/path/` runtime) consolidates per Lock 7. JSON beats sonic-rs's M1 Pro twitter parse. CSS L4, BBNF, Sheets, and the 5-grammar template cohort remain on the existing emitter path until BB; BA does not migrate them. BA produces the IR-and-emitter scaffolding that BB's generality wave consumes.

#### Hard gates

Every gate cites a specific competitor + dataset + platform per Lock 8. **Zero "AU baseline" or "≥ pre-W3" gates.**

| ID | Gate | Anchor |
|---|---|---|
| BA-G1 | `JsonParser::parse(twitter.json)` ≤ 400 µs on M1 Pro, beating sonic-rs's 436 µs by ≥ 8% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs benchmark_aarch64) |
| BA-G2 | `JsonParser::parse(twitter.json)` consumes ≤ 2 heap allocations per parse-call (1 arena + 1 root-vec); zero `Vec<OpenFrame>::clone` sites detectable in samply 5K-sample profile | `audit/RESTART-SKETCH-2026-05-03.md:154-220` (the 86.07% pathology) |
| BA-G3 | Workspace `cargo xtask regen --check` ≤ 30 s on M1 Pro; CSS L4 `compile_paths_request` ≤ 25 s | Lane 04 of HARDENING audit measured 59.98 s + 52.53 s baseline; gate halves both |
| BA-G4 | All 13 locks honoured at BA close per the §2 cross-reference table; `substrate_audit` test green | Lock cross-reference is the contract |
| BA-G5 | God-module count (>500 LOC outside `generated/`) drops from 23 → 0 per `audit/CENSUS-2026-05-03.md:319-353` | Lock 13 |
| BA-G6 | `crates/core/src/` reorganised into the 9-directory layout from `audit/MODULES-2026-05-03.md:1100-1118`; no `pipeline.rs` + `pipeline/` co-existence | Lock 13, `feedback_directory_modules` |
| BA-G7 | Zero grammar-specific arms in `bbnf-ir`: `crates/ir/src/registry/strategy.rs:130-185` reads from workspace metadata; `payload_coverage.rs:69` enum carries only `Custom(&'static str)`; `shape_dict_bbnf.rs` deleted; CSS L4 host fn relocated to `crates/core/src/host/css_types.rs` | `audit/CENSUS-2026-05-03.md:103-122`, Lane 5 grammar-authoritative discipline |
| BA-G8 | Eight inline `#[cfg(test)]` blocks moved to `tests/`; `rg -nE '^#\[cfg\(test\)\]$' crates/*/src/` returns zero | `audit/CENSUS-2026-05-03.md:381-399`, `feedback_no-inline-tests` |
| BA-G9 | `JsonDocument::get<T>(twitter.json, &path)` ≤ 5× the eager parse cost on M1 Pro (sonic-rs's `get_unchecked` is 0.1× of full parse; BA closes 4196× → ≤ 5×) | `audit/RESTART-SKETCH-2026-05-03.md:31-41` (the BA `bbnf_get_twitter` 4196× gap) |
| BA-G10 | Generated-file LOC accounting: per-grammar pre-/post-BA LOC table emitted; no overflow without justified cause; baseline budget per §5 | Lane 06 generated-code budget |

#### Wave skeleton

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BA.W0 | Layered re-org: `crates/core/src/` partitions into the 9-directory layout (`source/`, `parse/`, `lower/`, `codegen/`, `runtime/`, `path/`, `pipeline/`, `host/`, `lib.rs`); `pipeline.rs` collapses INTO `pipeline/mod.rs`; `crates/core/src/css_types.rs` relocates to `crates/core/src/host/css_types.rs`; tape-residue comment scrub (~50 hits across `runtime/` + `backend/` + `grammar/`); `generate/serialize/` directory deleted (156 LOC); 8 inline tests migrated. | Layer-name canon (Lock 2); no god directories (Lock 13); no metalanguage docs. | `cargo check --workspace` green; `rg -n 'TapeRec\|TapeCursor\|TapeBuilder\|TapeOffset' crates/*/src/` returns zero outside `tests/`; pipeline directory-form passes; per-grammar LOC table baseline emitted. |
| BA.W1 | Grammar-leak excision: `crates/ir/src/registry/strategy.rs:130-185` reads from `[workspace.metadata.bbnf-strategy]` only (no static `GRAMMARS` array); `payload_coverage.rs:69` `GrammarAuditTag` collapses to `Custom(&'static str)`; `shape_dict_bbnf.rs` deleted (240+ LOC); BBNF-specific shape mining generalises to data-driven recogniser registry. | bbnf-ir knows zero grammar idents (Lock 5 IR contract precursor). | `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/` returns zero matches; `crates/ir/tests/` continues passing. |
| BA.W2 | God-module splits: 23 files >500 LOC outside `generated/` partition per `audit/CENSUS-2026-05-03.md:319-353` and `audit/MODULES-2026-05-03.md` per-file split recommendations. CSS L4 builder.rs (1014) → `builder/{frame, declarations, selectors, color, dimensions, finalize}.rs`; flat/struct_direct.rs (1033) → `struct_direct/{header, body, fields, finalize}.rs`; etc. Layout-lowering rename: every `type_projection` / `type_collapsing` / `TypeMap` / `StructLayout` / `TypeDesc` / `schema_synthesis` reference in module names + doc comments rewrites to `layout_lowering`/`Layout`/`LayoutSink`. | Lock 2 (canon) + Lock 13 (no god modules). | `find crates -name '*.rs' ! -path '*/generated/*' | xargs wc -l | awk '$1>500'` returns empty; layout-lowering term is canonical. |
| BA.W3 | Path crate consolidation per Lock 7: rename `crates/bbnf-path/` → `crates/path/`; rename `crates/bbnf-path-ts/` → `crates/path-ts/`; introduce `crates/path-core/` shared non-proc-macro crate carrying `compile`/`lex`/`lower`/`validate` logic; `bbnf-path` and `bbnf-path-ts` path-dep on `path-core`; `bbnf-path-ts/src/fixture.rs` (248 LOC synthetic) deleted; `bbnf-path/src/registry.rs:132-135` grammar match arms read from per-grammar `pub const REGISTRY: StructRegistry`; `crates/core/src/runtime/path.rs` (163 LOC, the legacy borrowed alphabet) deleted; four `runtime/<g>/parse_with.rs` legacy lowering passes deleted. | Lock 7. Three crate names (path, path-core, path-ts); ~500 LOC mirror eliminated; runtime path types consume the typed alphabet directly. | `rg -n 'use crate::runtime::path::' crates/core/src/` returns zero; `crates/path-core/` exists; `crates/bbnf-path*/src/compile.rs` mirrors merged. |
| BA.W4 | Cursor + byte-skip unification: one `parse_with(input, &path)` per grammar; `__EAGER_EMPTY_PATH` LazyLock elides cursor consultation when path is empty; eager `JsonParser::parse` becomes `parse_with(input, &EMPTY_PATH)`. `JsonDocument::get<T>` reroutes through `parse_with(input, path)`. Cursor's `Skip` decision generates byte-skip code at codegen, not at runtime. | Lock 3 (cursor + byte-skip unified, cursor branch elided when path empty). | BA-G9 met (`get<T>` ≤ 5× full parse); `parse` and `parse_with` share one source; samply trace shows zero cursor calls on eager path. |
| BA.W5 | JSON direct-to-struct codegen first cut. Direct-projection emit replaces `OpenFrame` stack + `Vec<OpenFrame>::clone` for JSON only. Typed enum + per-rule generated `parse_<rule>(input, &mut p, &mut arena, &mut state, &mut cursor) -> Result<JsonValue<'p>, ParseErr>` functions. Byte-disjoint Alt emits direct `match first { b'{' => ... }` without speculative checkpoint. `OpenFrame` deleted from JSON path. CSS L4, BBNF, Sheets, and the 5-grammar template cohort STAY on existing emitter path; their migration is BB.W0/W1 carry. | Lock 1 (tape and columnar dead, direct-to-struct visible-and-internal); Lock 3 (one parse impl); Lock 9 (slice-borrow primary). | BA-G1 (≤ 400 µs); BA-G2 (≤ 2 heap allocs/parse); samply post-BA shows `Vec<OpenFrame>::clone` retired from JSON profile. |
| BA.W6 | BA close: PROGRESS / FINAL; lock cross-reference table verified; per-grammar generated-LOC table; carry ledger to BB.W0 named explicitly. | Lock-honoured at every gate; carry-tags BA→BB explicit. | `cargo nextest run -p bbnf -p bbnf-ir -p bbnf-analysis` 100% pass for BA-owned surfaces; `substrate_audit` green; per `audit/HARDENING-SYNTHESIS-2026-05-03.md:204` the BA-amendment punch-list closes. |

#### Carry-tags FROM prior tranche

None. BA is the restart; the pre-BA ceremony (§1.0) is the precondition, not a tranche.

#### Carry-tags TO BB

| Tag | Owner wave | Description |
|---|---|---|
| BA→BB.C1 | BA.W5 | Direct-to-struct codegen path lands for JSON; BB.W0/W1 extends it to CSS L4, BBNF, Sheets, and the 5-grammar template cohort. The emitter scaffolding is grammar-agnostic; only the per-grammar typed value sums + parse fns regenerate. |
| BA→BB.C2 | BA.W2 | Layout-lowering rename canonises the IR pass name; BB references `Layout`/`LayoutSink` only. |
| BA→BB.C3 | BA.W4 | Cursor-unified `parse_with` + `__EAGER_EMPTY_PATH` substrate; BB extends this to all grammars at BB.W2. |
| BA→BB.C4 | BA.W3 | `path-core` crate exists; BB's `pointer!["a","b",1]` macro at `crates/path/` reuses it without proc-macro/cdylib mirror. |
| BA→BB.C5 | BA.W1 | Grammar-agnostic `bbnf-ir`; BB's CSP/e-graph/miner extensions reference grammars only via `&str` ident through workspace metadata. |

#### Carry-tags TO BC (skipping BB)

| Tag | Owner wave | Description |
|---|---|---|
| BA→BC.C1 | BA.W2 | Layout-lowering canon supports the IR contract spec BC.W0 codifies. |
| BA→BC.C2 | BA.W5 | Direct-to-struct emitter pattern (one IR walker, leaf emission per backend) is the precursor to BC's `Emitter` trait formalisation across Rust/TS/WASM. |

#### Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BA.W2 god-module splits cascade into BA.W5 codegen breakage (renames pull through emitter) | Medium | BA.W2 lands renames first; BA.W5 regen comes last; xtask regen `--check` between waves. |
| BA.W5 direct-to-struct path regresses CSS L4 / Sheets / BBNF (whose paths are not migrated) | High | BA.W5 explicitly does NOT migrate non-JSON grammars; `EmitStrategy::for_grammar` retains the existing `OpenFrame` substrate for non-JSON grammars until BB.W0. The substrate-without-consumer concern is mitigated because BA.W5's consumer (JSON) lands in the SAME wave. |
| BA.W3 path consolidation breaks `pointer!` callers across `crates/core/tests/` | Medium | `path-core` is non-breaking (path-dep added); `bbnf-path` and `bbnf-path-ts` continue exporting their public API; the rename on disk is a name change, not an API change. |
| Tape-residue scrub introduces parser regressions (some tape comments are tied to lifetime narratives in safe-untouchable code) | Low | Comment-only scrub; zero functional source changes; `cargo nextest` between BA.W0 sub-commits. |
| Cursor-unified parse impl loses path-driven test coverage that exercises non-eager paths | Medium | BA.W4 retains `parse_with(input, path)` for non-eager; only the eager-fast-path constant-folds out; `tests/parse_with_*.rs` pass-rate is gated. |

#### Build/iter time gate

BA shrinks generated/* LOC modestly (the tape-comment scrub eliminates repeated narrative blocks; `OpenFrame` pattern retiral for JSON shrinks `json.rs` from 3500 LOC to ~2000 LOC by mechanism per `audit/RESTART-SKETCH-2026-05-03.md:585-595`). BA.W2 god-module splits redistribute, not grow, the LOC mass. Estimated net delta to `crates/core/src/grammar/generated/`: **−5% to −10%**.

xtask iteration-time gate (BA-G3): `cargo xtask regen --check` ≤ 30 s; CSS L4 `compile_paths_request` ≤ 25 s. Lane 04 measured 59.98 s + 52.53 s as the pre-BA baseline; halving is the BA close requirement.

---

### §1.2 BB — Generality + Optimisation

#### Gestalt

BB is the generality tranche: hereupon BA's JSON direct-to-struct path generalises to the remaining eight grammars; the five trivial cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) compress from ~2000 LOC of mechanical instantiation to ~250 LOC by template emission; the four specialised grammars (BBNF, JSON, Sheets, CSS L4) reduce to typed enum + per-rule generated parse fns only. Every per-domain optimiser — CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, cost model — lives in its own crate (`csp-solver`, `egraph`, `egraph-derive`, `bbnf-regex`, `parse-that` as path-deps until APIs stabilise per Lock 11). Optimiser layering composes by output-piping; no unified hypergraph, per Lock 4. Pratt and SIMD auto-detect from grammar shape (left-recursive operator chains → Pratt; leaf-pattern shape → SIMD scanner) — no `@pratt` or `@simd` directives, per Lock 10. Slice-borrow becomes the primary API; `parse_in(input, &bump)` and `parse_owned(input)` are the lifetime escape hatches per Lock 9. The sonic-class `pointer!["a","b",1]` macro at `crates/path/` lands the path-API surface; `LazyValue<'a>` borrowed views for lazy materialisation; chumsky-style `.as_<T>()` projection on demand; lightning-css-style `Visitor<'i, T>` with `VisitTypes` bitflag for tree transforms. CSS L4 and Sheets beat lightningcss + cssparser by margin per Lock 8.

#### Hard gates

| ID | Gate | Anchor |
|---|---|---|
| BB-G1 | `parse(bootstrap.css)` ≤ 3.5 ms on M1 Pro, beating lightningcss's 4.16 ms by ≥ 15% | `audit/SOTA-2026-05-03.md:131-136` (lightningcss bench table) |
| BB-G2 | `parse(tailwind.css)` ≤ 35 ms on M1 Pro, beating lightningcss's 43.37 ms by ≥ 18% | same |
| BB-G3 | `parse(citm_catalog.json)` ≤ 800 µs on M1 Pro, beating sonic-rs's 854 µs by ≥ 6% | `audit/SOTA-2026-05-03.md:50-58` |
| BB-G4 | `parse(canada.json)` ≤ 3.0 ms on M1 Pro, beating sonic-rs's 3.144 ms by ≥ 4.5% | same |
| BB-G5 | Five cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) compress to ≤ 50 LOC of per-grammar mechanical instantiation each in `runtime/<g>/`; total runtime/<g>/ shrinkage ≥ 1500 LOC across the 5-grammar cohort | `audit/CENSUS-2026-05-03.md:507-528` |
| BB-G6 | Pratt + SIMD auto-detection: no grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. Tests assert that BBNF's `binary_factor` operator chain lowers via Pratt; JSON's structural alphabet (`{`,`}`,`[`,`]`,`,`,`:`) drives SIMD scanner emission. | Lock 10 |
| BB-G7 | `pointer!["a","b",1]` macro at `crates/path/` resolves type-checked against the generated `pub const REGISTRY: StructRegistry`; runtime evaluation produces `LazyValue<'a>`; `.as_str()`, `.as_i64()`, `.as_<T>()` materialise on demand | Lock 9 + sonic-rs LazyValue surface, `audit/SOTA-2026-05-03.md:33-42` |
| BB-G8 | `parse_in(input, &bump)` opt-in surface lands; `parse_owned(input)` opt-in surface lands; default `parse(input)` is slice-borrow `&'i str` | Lock 9 |
| BB-G9 | Visitor surface lands per grammar: `Visitor<'i, T>` trait + `VisitTypes` bitflag bitmask; CSS L4's `visit_color`, `visit_length`, `visit_url`, `visit_property` exposed | `audit/SOTA-2026-05-03.md:104-118` (lightningcss visitor reference) |
| BB-G10 | Optimiser composition is output-piped: CSP infers types → e-graph rewrites → miners populate facts → cost model selects strategies; no unified hypergraph; each crate is a path-dep boundary | Lock 4, Lock 11 |
| BB-G11 | Generated-file LOC budget: post-BB `crates/core/src/grammar/generated/` net delta ≤ +5% on specialised grammars; ≤ −60% on the 5-grammar cohort | Lane 06, Lock 13 |

#### Wave skeleton

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BB.W0 | Sister crate emigration: `crates/egraph/`, `crates/egraph-derive/`, `crates/csp-solver/`, `crates/bbnf-regex/` (currently sibling `parse-that/rust/regex` → `parse-that/rust/bbnf-regex`), `parse-that` itself become path-deps in the workspace; documented as "incubating" until APIs stabilise. Workspace metadata records the canonical endpoints per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175` (the BC.W5 endpoint reconciliation gate). simd-scan + bootstrap + analysis + lsp stay workspace-internal. | Lock 11. | `cargo check --workspace` green; path-deps resolve; `Cargo.toml` declares no in-tree workspace-member duplicates. |
| BB.W1 | Direct-to-struct generalisation: BB extends BA.W5's JSON pattern to all eight remaining grammars. CSS L4, BBNF, Sheets each emit typed enum + per-rule generated parse fns; the existing 14-variant `OpenFrame` builder for CSS L4 retires; the 4-variant `OpenFrame` builder for JSON is already dead at BA.W5 close; BBNF and Sheets retire their `OpenFrame` variants here. | Lock 1 (direct-to-struct visible-and-internal across all grammars). | `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns zero; per-grammar LOC budget per BB-G11 met; `cargo nextest run -p bbnf` 100% pass. |
| BB.W2 | Five-grammar cohort template emission: `runtime/{bnf,csv,ebnf,css_pretty,math}/{document,view,kind,value,mod}.rs` emit from a single codegen template at xtask-regen time. Each cohort grammar's `runtime/<g>/` shrinks from ~350-450 LOC to ≤50 LOC of mechanical instantiation. The four specialised grammars (BBNF, JSON, Sheets, CSS L4) keep hand-written modules. | Lock 13 (cohesion at every level); `feedback_pluggable-components`. | BB-G5 met (≥1500 LOC saved); `crates/core/src/runtime/<simple>/{arena,builder,document,view,kind,value,mod}.rs` are template-emitted. |
| BB.W3 | Optimiser pipeline per Lock 4. CSP type inference (`crates/ir/src/passes/types/`) → e-graph saturation (`crates/ir/src/egraph/`) → recogniser miners (`crates/ir/src/passes/recognizers/`) → CSP strategy selection (`crates/ir/src/passes/csp_strategy/`) → cost-model extraction (`crates/egraph/src/extract.rs`). Each is a path-dep crate; output is piped, not fused. Pratt auto-detection lands as a recogniser (`recognizers/operator_chain.rs`); SIMD auto-detection lands as a structural-alphabet miner (`passes/sets/structural_alphabet.rs`) → simd-scan kernel selector. **Rank/tier rewrites land here in W3, with consumer; per `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-125`, NOT in W0.** | Lock 4 + Lock 10. | BB-G6 (Pratt + SIMD auto-detection) met; tests assert no `@pratt` / `@simd` in any grammar source; `crates/ir/src/rewrites/{rank,tiering}.rs` exist + consumed in same wave. |
| BB.W4 | Slice-borrow primary API + escape hatches. Default `parse(input: &'i str) -> Result<<G>Value<'i>, ParseErr>`. `parse_in(input: &'i str, bump: &'i bumpalo::Bump) -> Result<<G>Value<'i>, ParseErr>` opt-in. `parse_owned(input: &str) -> Result<<G>OwnedValue, ParseErr>` opt-in. The three are surfaces over the same parse implementation; lifetime parameter is the discriminant. | Lock 9. | BB-G8 met; `cargo doc` shows the three surfaces with consistent docstring shape. |
| BB.W5 | Sonic-class path API + visitor surface. `pointer!["a","b",1]` proc-macro at `crates/path/`; runtime evaluation produces `LazyValue<'a>`; `.as_str()`, `.as_i64()`, `.as_<T>()` per chumsky's pattern. lightning-css-style `Visitor<'i, T>` trait + `VisitTypes` bitflag + per-record `visit_<Name>(&mut self, &mut T)` methods. CSS L4 exposes `visit_color`, `visit_length`, `visit_url`, `visit_property`; JSON exposes `visit_string`, `visit_number`, `visit_object`, `visit_array`. | Lock 9 + sonic-class API anchor. | BB-G7 + BB-G9 met; `crates/path/src/path_macro.rs` (after split per BA.W2) emits `compile_path` resolving against `pub const REGISTRY`. |
| BB.W6 | BB close: perf gates BB-G1..G4 met; PROGRESS / FINAL; cohort-grammar generated-LOC budget verified; carry ledger to BC.W0 named. | Lock-honoured; perf trajectory hits BC entry conditions. | `cargo nextest run -p bbnf -p bbnf-ir -p bbnf-analysis -p bbnf-path` 100% pass; bench harness produces post-BB.json archetype. |

#### Carry-tags FROM BA

(See §1.1 BA→BB carry-tags.)

#### Carry-tags TO BC

| Tag | Owner wave | Description |
|---|---|---|
| BB→BC.C1 | BB.W3 | Optimiser composition (CSP → e-graph → miners → cost model) is output-piped; BC's IR contract specifies the contract between optimiser stages and the per-backend lowerer. |
| BB→BC.C2 | BB.W1 | Direct-to-struct emit shape is grammar-agnostic; BC formalises this as the IR-input/typed-IR-output contract for the per-backend lowerer. |
| BB→BC.C3 | BB.W5 | Visitor + `VisitTypes` bitflag pattern is the per-backend lowerer's traversal API; BC's TS + WASM emitters consume this contract. |
| BB→BC.C4 | BB.W0 | Sister crates (egraph, egraph-derive, csp-solver, bbnf-regex, parse-that) are path-deps; BC may promote any to crates.io once API stabilises. |

#### Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BB.W1 CSS L4 14-variant builder migration regresses lightningcss parity | High | Per-variant migration; `tests/css_l4_parity.rs` runs after each variant retiral; canonical-form bench against lightningcss's emit-CSS surface gates each step. |
| BB.W2 cohort template emission drops a behaviour the hand-written cohort modules silently exercised | Medium | Template emission generates byte-identical output to the existing hand-written 5-grammar files at first commit; regression tests assert byte-equality before deletion of the hand-written files. |
| BB.W3 Pratt auto-detection misfires (classifies a non-Pratt rule as Pratt) | Medium | Fallback to non-Pratt emitter is the ground truth; the optimiser's classification has a false-positive cost that the cost model accounts for; tests enumerate all 9 grammars' rules and verify that only known operator chains route to Pratt. |
| BB.W3 SIMD auto-detection adds dispatch overhead on small inputs | Medium | Cost model has a `simd_threshold_bytes` parameter (no `@simd` directive); the threshold is grammar-derived from FIRST set + structural-alphabet density. |
| BB.W5 `pointer!` macro depends on per-grammar registry being JSON-sidecar-stable | Low | BA.W3 already finalised the registry sidecar; BB.W5 consumes only. |

#### Build/iter time gate

BB shrinks the cohort runtime by ~1500 LOC; the specialised grammars (CSS L4, BBNF, Sheets, JSON) regenerate with direct-to-struct shape — net regen LOC may grow modestly (typed enum variants explode some payload payloads — for instance, CSS L4's `CssTypedValue` may grow as variants become explicit) but per-rule parse fns shrink (no `OpenFrame` ladder). Estimated net delta to `crates/core/src/grammar/generated/`: **+0% to +5%** on specialised grammars; **−60% to −70%** on cohort grammars. Aggregate net delta: **−10% to −15%** of total generated LOC.

xtask iteration-time gate: `cargo xtask regen --check` ≤ 25 s on M1 Pro. (BA close: ≤ 30 s; BB close: ≤ 25 s.)

---

### §1.3 BC — Backend ABI + Multi-Backend Foundation

#### Gestalt

BC is the backend ABI tranche: hereupon the codegen surface formalises into a backend-agnostic typed IR + per-backend lowerer per Lock 5; the existing Rust source emitter refactors to consume the IR contract instead of walking the grammar IR directly; the IR contract specifies input grammar IR + Layout, output backend-agnostic typed IR; TS and WASM emitters EXPLICITLY DEFER to BD+ but the IR shape must support them. The core crate splits into `bbnf-parse` (source, parse, lower, host) + `bbnf-codegen` (codegen IR + per-backend lowerer + Rust emitter) + `bbnf-runtime` (runtime, path, handle) per `audit/MODULES-2026-05-03.md:1158-1167`. Final perf gates: surpass sonic-rs / simdjson / lightning-css on at-least-one specific dataset per grammar, per Lock 8. The 19-row dataset matrix from prior eras is replaced with competitor-anchored gates: sonic-rs twitter, simdjson canada, lightningcss bootstrap, cssparser tailwind, etc.

#### Hard gates

| ID | Gate | Anchor |
|---|---|---|
| BC-G1 | `JsonParser::parse(twitter.json)` ≤ 380 µs on M1 Pro, beating sonic-rs's 436 µs by ≥ 13% | `audit/SOTA-2026-05-03.md:50-58`; tighter than BA-G1 (≤ 400 µs); sonic-rs floor remaining is the asymptote |
| BC-G2 | `parse(bootstrap.css)` ≤ 3.0 ms on M1 Pro, beating lightningcss's 4.16 ms by ≥ 27% | same SOTA |
| BC-G3 | `parse(canada.json)` ≤ 2.8 ms on M1 Pro, beating sonic-rs's 3.144 ms by ≥ 11% | same SOTA |
| BC-G4 | The IR contract is documented at `docs/codegen-IR-CONTRACT.md` — input grammar IR + Layout, output backend-agnostic typed IR; the Rust emitter consumes only the typed IR; no direct grammar-IR access in `bbnf-codegen`'s emitter | Lock 5 |
| BC-G5 | Core crate splits into `bbnf-parse` / `bbnf-codegen` / `bbnf-runtime`; each compiles independently with explicit dependency arrows; `bbnf-parse` does not depend on `bbnf-codegen` | `audit/MODULES-2026-05-03.md:1158-1167` |
| BC-G6 | TS + WASM emitter scaffolds exist at `crates/core/src/codegen/ts/` and `crates/core/src/codegen/wasm/` (or `bbnf-codegen/src/ts/`, `bbnf-codegen/src/wasm/` post-split) but are NOT activated; the IR shape supports them via the `Emitter` trait | Lock 5 |
| BC-G7 | `crates/path-core/` final API stable; `bbnf-regex` endpoint reconciliation per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175` chooses one of the two candidates and documents the choice | Lock 11 |
| BC-G8 | `crates/path/`, `crates/path-core/`, `crates/path-ts/` API surface frozen and documented; sister crates (egraph, csp-solver, bbnf-regex) candidates for crates.io publication | Lock 7 + Lock 11 |
| BC-G9 | Sonic-class get-by-pointer surface complete: `JsonValue::pointer(input, &path) -> Option<LazyValue<'a>>` with `pointer!` macro from BB.W5; cssparser-class visit-by-type surface complete: `visit_<T>` per record with `VisitTypes` bitflag pruning subtree traversal | Lock 9 + Lane 7 friction-forecast preventive |
| BC-G10 | Generated-file LOC stable: post-BC `crates/core/src/grammar/generated/` net delta from BB ≤ +2%; the IR contract is the boundary, not the regen surface | Lane 06 |

#### Wave skeleton

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BC.W0 | IR contract specification: `docs/codegen-IR-CONTRACT.md` documents the input (grammar IR + Layout output from `bbnf-parse`'s lowering) and the output (backend-agnostic typed IR consumed by per-backend lowerer). The contract names every IR node kind; specifies Lifetime / Layout / TypeDesc resolution; specifies what is per-backend (only leaf source emission) and what is shared (decision dispatch, IR walk, strategy selection). | Lock 5. | `docs/codegen-IR-CONTRACT.md` lands; the existing `Emitter` trait in `crates/core/src/backend/emitter.rs` (post-split: `bbnf-codegen/src/emitter.rs`) refactors to consume the typed IR; Rust source emitter refactor lands here. |
| BC.W1 | Core crate split per `audit/MODULES-2026-05-03.md:1158-1167`. `bbnf-parse` carries `source/`, `parse/`, `lower/`, `host/`. `bbnf-codegen` carries `codegen/` and per-backend emitters. `bbnf-runtime` carries `runtime/`, `path/`, `handle.rs`. The split is desirable; not a regression; tests pass. | Lock 13 (no god directories at the crate level either). | `cargo check -p bbnf-parse -p bbnf-codegen -p bbnf-runtime` green independently; `cargo nextest run -p bbnf-parse -p bbnf-codegen -p bbnf-runtime` 100% pass. |
| BC.W2 | TS + WASM emitter stubs land per Lock 5; both implement the same `Emitter` trait surface; both compile against the IR contract; both produce non-empty, syntactically-valid output for at-least-one trivial grammar (BNF or CSV); production activation deferred to BD+. | Lock 5; substrate-without-consumer concern mitigated because the same-wave consumer is the trivial-grammar smoke test. | TS emitter produces a `parseObject(ctx)` skeleton for JSON's `object` rule per `audit/RESTART-SKETCH-2026-05-03.md:559-577`; WASM emitter produces a WAT skeleton for the same; both fail gracefully on rules requiring host-fn shims (host-fn TS/WASM resolution is BD scope). |
| BC.W3 | Worktree fixture symlink contract closure per `audit/HARDENING-SYNTHESIS-2026-05-03.md:158-164`: `xtask worktree-init` materialises `data/{json,css,bbnf,sheets}` + `grammar/<name>/rewrites/*.ron` for every grammar; the post-BB closure for fleet-wide fixture materialisation lands. | Worktree fixtures complete for parallel-agent dispatch (relevant if BD+ uses worktree-based parallelism). | `xtask worktree-init` runs cleanly; sample worktree boots with materialised fixtures; rewrites/*.ron lands per grammar. |
| BC.W4 | Sister crate API freeze. `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` API surfaces freeze; candidates for crates.io. `bbnf-regex` endpoint reconciliation per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175` picks `parse-that/rust/regex` OR `parse-that/rust/bbnf-regex` and documents the choice. | Lock 11. | `cargo doc -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` produces clean API docs; endpoint reconciliation pre-flight commands from amendment block run cleanly. |
| BC.W5 | Final perf gates: BC-G1..G3 met; surpass sonic-rs / simdjson / lightning-css on at-least-one specific dataset per grammar. CSS Pretty, CSV, BNF, EBNF, Math beat their fastest external competitor (where one exists) on a named dataset per Lock 8. | Lock 8. | Post-BC.json bench artefact contains every gate; `tests/*_parity.rs` against sonic-rs / lightningcss / simdjson / cssparser / serde_json all pass. |
| BC.W6 | BC close: PROGRESS / FINAL; lock cross-reference verified; carry ledger to BD (TS + WASM activation, host-fn per-backend resolution) named explicitly. | All 13 locks honoured at every gate. | The 13-lock table in §2 closes with no empty cells; carry-tags BC→BD documented for the eventual TS/WASM emitter activation. |

#### Carry-tags FROM BB

(See §1.2 BB→BC carry-tags.)

#### Carry-tags TO BD (the eventual TS/WASM emergence)

| Tag | Owner wave | Description |
|---|---|---|
| BC→BD.C1 | BC.W2 | TS + WASM emitter stubs exist; BD activates them in production; host-fn per-backend resolution (TS: `runtime.parseHexColor`; WASM: indexed extern import) is BD scope. |
| BC→BD.C2 | BC.W4 | Sister crates frozen; BD may promote any to crates.io. |
| BC→BD.C3 | BC.W3 | Worktree fixture contract supports parallel-agent dispatch infrastructure for BD execution. |

#### Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BC.W0 IR contract specification is over-engineered (over-specifies what's per-backend) | Medium | The IR contract emerges from refactoring the existing `Emitter` trait, not from forward design. The trait's current shape names what's shared; BC.W0 documents the existing contract, doesn't invent a new one. |
| BC.W1 core split creates circular dependencies | Medium | `audit/MODULES-2026-05-03.md:1149-1156` already verified there are no circular dependencies in the proposed split (lower/ reads `runtime::bbnf::BbnfView`, but post-split `bbnf-parse` re-exports the BBNF runtime types from `bbnf-runtime`; the dep arrow is `bbnf-parse → bbnf-runtime`). |
| BC.W2 TS+WASM emitter stubs introduce substrate-without-consumer per Era V failure mode | Medium | The same-wave consumer is the trivial-grammar smoke test (BNF/CSV); the production consumer is BD; per Lock 5, the IR contract requires the stubs to compile and produce output (not just exist). |
| BC.W4 `bbnf-regex` endpoint flip breaks downstream consumers | Low | The rename is a one-time cargo-config update; current consumers reference `bbnf-regex` package name, not path; `[patch.crates-io]` block resolves transparently. |
| BC.W5 perf gates miss because BB.W3 optimiser path was gated on consumer demand that BC.W5 surfaces | Low | BC.W5's gates depend on BB.W3's CSP/e-graph/miner output. If BB.W3 doesn't deliver the expected speedup, BC.W5 gates become uncloseable; mitigation is per-wave samply checkpoints in BB.W3 + BC.W5 entry preflight. |

#### Build/iter time gate

BC stabilises generated/* LOC. Net delta ≤ +2% from BB. The crate split (BC.W1) does not regenerate; it relocates files. xtask iteration-time gate: ≤ 22 s on M1 Pro. (BB close: ≤ 25 s; BC close: ≤ 22 s.)

---

## §2 — 13-Lock Cross-Reference Table

Every cell names the wave that addresses the lock; empty cells are faults. Notes column flags weak adherence or carry-deferral.

| Lock | BA | BB | BC | Notes |
|---|---|---|---|---|
| **L1. Tape + columnar dead** | W0 (residue scrub); W5 (JSON OpenFrame retiral) | W1 (CSS L4 + BBNF + Sheets OpenFrame retiral) | — | Era V columnar (`AV/research/04-columnar-soa.md`) explicitly rejected; OpenFrame is the legacy substrate that retires, not new substrate. |
| **L2. Layout lowering canon** | W2 (rename pass) | W3 references `Layout`/`LayoutSink` only | W0 IR contract uses canonical terms | Old terms (`type_projection`, `TypeMap`, `StructLayout`) survive only in archived docs. |
| **L3. Cursor + byte-skip unified** | W4 (one parse impl, eager elides cursor) | W2 (cursor-unified extends to all grammars) | — | `__EAGER_EMPTY_PATH` LazyLock at BA.W4 is the unification point. |
| **L4. Per-domain orthogonal optimisation** | — | W3 (CSP → e-graph → miners → cost-model output-piped) | — | No unified hypergraph; each optimiser is its own crate (Lock 11). |
| **L5. IR + per-backend lower** | W5 demonstrates the pattern (Rust emitter consumes IR-shape) | W1 generalises across 9 grammars | W0 codifies IR contract + Rust emitter consumes IR; W2 TS + WASM stubs | The IR contract spec (BC.W0) is the consumer. |
| **L6. xtask emits committed source** | W0 (regen check ≤ 30 s; committed regen artefacts) | W1 W2 (regen pipelines through xtask only; no proc-macro façade) | W4 (xtask emits all generated artefacts) | `bbnf-path`, `bbnf-path-ts` proc-macro shells are SEPARATE per Lock 7; not the codegen substrate. |
| **L7. `crates/path/` consolidation** | W3 (rename, path-core extraction, runtime/path.rs deletion, parse_with legacy excision) | W5 (`pointer!` macro lands; LazyValue surface) | W4 (`bbnf-regex` endpoint reconciliation) | Three crate names (path, path-core, path-ts) only; no fourth proc-macro shell. |
| **L8. Surpass sonic-rs / simdjson / lightning-css** | G1 (≤ 400 µs twitter beating sonic-rs); G9 (4196× → ≤ 5× get gap) | G1, G2 (lightningcss bootstrap + tailwind); G3, G4 (sonic-rs citm + canada) | G1, G2, G3 (tighter sonic-rs + lightningcss); G5 (cohort grammar competitor anchors) | Zero AU references; every gate names competitor + dataset + platform. |
| **L9. Slice-borrow primary; bumpalo + owned escape hatches** | W5 (JSON parse returns slice-borrowed `JsonValue<'p>`) | W4 (the three surfaces: parse / parse_in / parse_owned) | W5 (slice-borrow stable; sonic-class LazyValue API) | Default surface is `&'i str` slice + `Cow<'i, str>` per `audit/SOTA-2026-05-03.md:122-123`. |
| **L10. Pratt + SIMD auto-detected** | — | W3 (operator_chain miner + structural_alphabet miner; cost model decides SIMD threshold) | — | No grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. |
| **L11. Path-deps for incubating sister crates** | — | W0 (egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as path-deps) | W4 (sister crate API freeze; crates.io candidates) | simd-scan + bootstrap + analysis + lsp stay workspace-internal. |
| **L12. ser + gorgeous archive BEFORE BA.W0** | §1.0 pre-BA ceremony | — | — | Precondition; not a tranche. Verification: `archive/{ser,gorgeous}/` exist; `Cargo.toml` workspace members reduced by 2. |
| **L13. No god directories; cohesive encapsulation at every level** | W0 (9-directory layered re-org); W2 (god-module splits, 23 → 0); W3 (path crate triplet) | W2 (cohort template emission ≤ 50 LOC each); W3 (per-domain optimiser crates) | W1 (core crate splits into 3 cohesive sub-crates) | sonic-rs / lightningcss / simdjson cohesion is the standard. Files >500 LOC outside `generated/` forbidden. >10-children dirs mixing concerns forbidden. |

**Verification**: every lock has at least one tranche cell populated. Every cell names the wave that addresses the lock. The Notes column highlights where the adherence is mechanical vs. where it requires careful execution discipline.

**Empty-cell audit**: no empty cells. Every lock is addressed at least once. (L4, L10 are addressed in BB only; L1 carries through BA + BB; L7 carries through BA + BB + BC.)

---

## §3 — Sequencing Discipline Check (Lane 2)

The Era V failure mode (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:8-12`) was substrate-first / consumer-later: every tranche shipped compile-time emission of constants, tables, and shape dictionaries; no tranche fully activated the runtime consumer that reads them. This pattern is the single largest contributor to the ~600 commits across AV / AW-I / AW-II / AW-III / AW-IV / AW-V / AX before the interpreter's eventual deletion at AX.W0b.

The new plan must NOT repeat this pattern. For every wave, this section asks: *what does it produce, who consumes it, and when does the consumer arrive?*

### §3.1 BA wave-by-wave check

| Wave | Produces | Consumer | Same-wave or next-wave? | Verdict |
|---|---|---|---|---|
| BA.W0 | 9-directory layered re-org; tape-residue scrub; `generate/serialize/` deletion; 8 inline tests migrated | All subsequent BA waves (W1-W6) consume the new directory layout immediately | Same-wave (the layered re-org is itself the gate) | OK |
| BA.W1 | Grammar-leak excision (no per-grammar arms in `bbnf-ir`) | BA.W2 (split passes consume the grammar-agnostic IR) | Next-wave | OK |
| BA.W2 | God-module splits + layout-lowering rename | BA.W5 (JSON direct-to-struct emit consumes the renamed layout passes) | Multi-wave gap (W3, W4 in between, but they consume the splits too — every later wave reads the renamed surface) | OK |
| BA.W3 | `path-core` crate; `bbnf-path-ts` mirror eliminated; runtime path triplet collapsed | BA.W4 (`parse_with` consumes the unified path types); BB.W5 (`pointer!` macro consumes `path-core`) | Next-wave + cross-tranche; closure on BB.W5 is documented in carry-tag BA→BB.C4 | OK |
| BA.W4 | Cursor + byte-skip unification; `__EAGER_EMPTY_PATH` | BA.W5 (JSON direct-to-struct emit uses the unified path); BB.W2 (cursor-unification extends to all grammars) | Same-wave + next-wave + cross-tranche | OK |
| BA.W5 | JSON direct-to-struct codegen | Same wave: the JSON test + bench gates verify the emit; BB.W1 generalises | Same-wave + next-wave | OK |
| BA.W6 | BA close artefacts; carry-ledger to BB | BB.W0 entry preflight | Next-wave | OK |

**No substrate-without-consumer in BA.** Every produced artefact has a same-wave or next-wave consumer.

### §3.2 BB wave-by-wave check

| Wave | Produces | Consumer | Same-wave or next-wave? | Verdict |
|---|---|---|---|---|
| BB.W0 | Sister crate emigration to path-deps | BB.W3 (optimiser pipeline consumes the path-dep'd egraph + csp-solver + bbnf-regex) | Next-wave (W3 consumes); workspace cargo check is the same-wave gate | OK |
| BB.W1 | Direct-to-struct emit for CSS L4, BBNF, Sheets | Same wave: the per-grammar test + bench gates verify | Same-wave | OK |
| BB.W2 | Cohort template emission; 5 cohort grammars compress | Same wave: byte-equality regression gate; subsequent waves' tests run against the templated cohort | Same-wave | OK |
| BB.W3 | CSP + e-graph + miners + cost model output-piping; Pratt + SIMD auto-detection; rank/tier rewrites | Same wave: the perf gates (BB-G6 + auto-detection tests); the rank/tier rewrites are consumed in the same wave per `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-127`'s amendment block | Same-wave | OK (the substrate-first concern is closed by the in-wave consumer per the amendment) |
| BB.W4 | parse / parse_in / parse_owned three-surface API | Same wave: API tests; BB.W5 consumes the three surfaces in macro emission | Same-wave + next-wave | OK |
| BB.W5 | `pointer!` macro + LazyValue + Visitor surface | Same wave: per-grammar visitor tests; BC.W5 consumes the visitor surface for parity tests against lightningcss | Same-wave + cross-tranche | OK |
| BB.W6 | BB close artefacts; carry-ledger to BC | BC.W0 entry preflight | Next-wave | OK |

**No substrate-without-consumer in BB.** The previously-flagged BB.W0 rank/tier skeleton concern (`audit/HARDENING-SYNTHESIS-2026-05-03.md:107-115`) is resolved by the amendment that moves rank.rs + tiering.rs creation INTO BB.W3 with same-wave consumer.

### §3.3 BC wave-by-wave check

| Wave | Produces | Consumer | Same-wave or next-wave? | Verdict |
|---|---|---|---|---|
| BC.W0 | IR contract specification; Rust emitter refactored to consume typed IR | Same wave: the existing Rust emitter is the consumer; refactor lands in same wave as spec | Same-wave | OK |
| BC.W1 | Core crate split (bbnf-parse / bbnf-codegen / bbnf-runtime) | Same wave: each sub-crate compiles + tests independently | Same-wave | OK |
| BC.W2 | TS + WASM emitter stubs | Same wave: trivial-grammar smoke test (BNF/CSV); production consumer is BD | Same-wave (smoke); cross-tranche (production) | OK — the smoke-test consumer is the same-wave gate; production consumer is BD per carry-tag BC→BD.C1 |
| BC.W3 | `xtask worktree-init` + worktree fixture symlink contract | BD parallel-agent dispatch + BC's own end-of-tranche orchestration tests | Cross-tranche; same-wave smoke is `xtask worktree-init` running cleanly | OK |
| BC.W4 | Sister crate API freeze + bbnf-regex endpoint reconciliation | Same wave: `cargo doc` + `cargo publish --dry-run`; BD consumes the frozen API | Same-wave + cross-tranche | OK |
| BC.W5 | Final perf gates met (sonic-rs, simdjson, lightningcss surpassed) | Same wave: bench gates verify | Same-wave | OK |
| BC.W6 | BC close artefacts; carry-ledger to BD | BD entry preflight | Cross-tranche | OK |

**No substrate-without-consumer in BC.** BC.W2's TS+WASM stubs have the same-wave smoke-test consumer (the trivial-grammar BNF/CSV emit verification); production activation explicitly carries to BD per the user's punt on TS/WASM until BD+.

### §3.4 Era V anti-pattern citation

Per `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:80-81`, AV's V0-V5 substrate landed but V6-V10 routed forward to AW; the gap between substrate-land and consumer-arrive is what defines Era V. The new plan structurally avoids this:

1. **No "deferred to BB" without explicit BB wave + gate** — every BA→BB carry-tag names the receiving wave (BB.W0 / BB.W1 / BB.W3 / BB.W5).
2. **No skeleton substrates without same-wave consumer** — the `audit/HARDENING-SYNTHESIS-2026-05-03.md:118-115` amendment moves rank/tier creation INTO BB.W3 (not BB.W0); this directly addresses the rut.
3. **No multi-wave gaps without compile-test-bench gates** — every wave has a closer-gate verifiable from artefacts the wave creates.

The one cross-tranche carry that remains (BC.W2 TS+WASM activation → BD) is explicitly punted by the user; `audit/HARDENING-SYNTHESIS-2026-05-03.md:42-43, 56-58` makes this routing canonical.

---

## §4 — Perf Trajectory

All numbers are M1 Pro for sonic-rs / lightningcss; Intel Skylake for simdjson where annotated. Zero AU references. Every BA / BB / BC target cites the SOTA anchor it dominates.

| Grammar | Dataset | Today | BA target | BB target | BC target | Competitor anchor |
|---|---|---|---|---|---|---|
| **JSON** | twitter.json | 1.4 ms (4196× slower than sonic-rs LazyValue access) | ≤ 400 µs (parse) | ≤ 390 µs | ≤ 380 µs | sonic-rs 436 µs `audit/SOTA-2026-05-03.md:53` |
| **JSON** | citm_catalog.json | (trace not in audit; estimate 5+ ms) | (no gate; non-canonical workload) | ≤ 800 µs | ≤ 790 µs | sonic-rs 854 µs `audit/SOTA-2026-05-03.md:54` |
| **JSON** | canada.json | (trace not in audit; estimate 10+ ms) | (no gate) | ≤ 3.0 ms | ≤ 2.8 ms | sonic-rs 3.144 ms `audit/SOTA-2026-05-03.md:55` |
| **JSON get** | `JsonDocument::get<T>(twitter.json, &path)` | 4196× full parse | ≤ 5× full parse | ≤ 3× full parse | ≤ 2× full parse | sonic-rs `get_unchecked` ~0.1× full parse `audit/SOTA-2026-05-03.md:33-37` |
| **CSS L4** | bootstrap.css | (trace pre-BA not in audit; >> lightningcss) | (no gate) | ≤ 3.5 ms | ≤ 3.0 ms | lightningcss 4.16 ms `audit/SOTA-2026-05-03.md:133` |
| **CSS L4** | tailwind.css | (trace not in audit) | (no gate) | ≤ 35 ms | ≤ 32 ms | lightningcss 43.37 ms `audit/SOTA-2026-05-03.md:135` |
| **CSS L4** | animate.css | (trace not in audit) | (no gate) | ≤ 1.7 ms | ≤ 1.5 ms | lightningcss 1.97 ms `audit/SOTA-2026-05-03.md:134` |
| **BBNF (self-host)** | bbnf.bbnf (self-parse) | (trace not in audit) | (no gate; non-canonical workload) | (no specific gate; cohort optimiser benefits apply) | (≤ 20% of self-host wall time per parse-call by mechanism — direct-to-struct + Pratt auto-detection retire OpenFrame ladder) | (no external SOTA — bbnf is the self-grammar; gates use cssparser as a comparable LL-family parser anchor for selector-flavoured rules) |
| **Sheets** | sheets-stress.bbnf | parse_simple ~95 MB/s (Era IV close baseline) | (no gate) | (≤ ½ sheet parse time vs current; mineable target) | (≥ 350 MB/s post-direct-to-struct; competitive with cssparser at similar density) | cssparser ~600 MB/s `audit/SOTA-2026-05-03.md:122` |
| **CSV** | medium-csv | (trace not in audit) | (no gate) | (≤ 50 LOC of mechanical instantiation; perf delegates to template emission) | (≥ 500 MB/s — comparable to nom CSV examples at similar shape) | nom-driven CSV scanners ~500 MB/s anchor |
| **BNF / EBNF / CSS Pretty / Math** | grammar-author fixtures | (trace not in audit) | (no gate) | (≤ 50 LOC of instantiation each) | (no specific gate; cohort grammars not perf-canonical workloads) | n/a — cohort grammars exist for grammar-author breadth, not perf flagship |

### §4.1 Notes on perf trajectory

1. **The 4196× gap is the BA-restart's defining motivation.** `audit/RESTART-SKETCH-2026-05-03.md:39-40` traces `Vec<OpenFrame>::clone` as 86.07% inclusive samples on a 25,963-sample fat-LTO bench. Closing the gap on `JsonDocument::get<T>` is what BA-G9 codifies.

2. **JSON is the BA flagship; CSS is the BB flagship; multi-grammar cohort is the BC flagship.** BA proves the path on one grammar (JSON, the simplest schema with the highest per-byte parse cost); BB generalises (CSS L4 — the largest grammar; BBNF — the self-host; Sheets — the third specialised; 5 cohort templates); BC stabilises and surpasses.

3. **No "AU baseline" gates anywhere.** The Era IV close numbers (`docs/tranches/meta-audit/archaeology/era-IV-tape-first.md:7-9`) are historical context only; they appear in NO tranche gate.

4. **simdjson 7 GB/s is not directly addressed.** simdjson is C++ + AVX-512; bbnf is Rust + portable SIMD via `crates/simd-scan/`. The trajectory is sonic-rs class (which itself is ~436 µs vs simdjson's 424 µs on twitter — within margin); claiming simdjson parity requires AVX-512 at least, which is BD scope.

5. **Cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) get template-driven shrinkage; performance is not the canonical metric.** Their LOC reduction (BB-G5: ≥ 1500 LOC saved across 5-grammar cohort) is the correct gate; no external competitor exists for "BNF-of-BNF parser" or "math-expression parser at our specific shape".

---

## §5 — Generated-Code Budget

Per Lane 06 of the HARDENING audit, the current 168 K LOC across 9 generated grammar files is the starting point. Every wave names its expected delta; overflow without justification blocks the wave.

### §5.1 Pre-BA baseline

| Grammar | Generated LOC | Source | Notes |
|---|---|---|---|
| `bbnf.rs` | 21,503 | `audit/MODULES-2026-05-03.md:619` | self-host parser |
| `json.rs` | 3,500 | same:621 | the BA flagship grammar |
| `css_l4.rs` | 107,138 | same:622 | the largest; BB.W1 target for direct-to-struct retiral of 14-variant OpenFrame |
| `css_pretty.rs` | 9,021 | same:623 | cohort grammar |
| `google_sheets.rs` | 14,088 | same:624 | specialised grammar |
| `ebnf.rs` | 7,646 | same:625 | cohort grammar |
| `bnf.rs` | 3,290 | same:626 | cohort grammar |
| `csv.rs` | 1,693 | same:627 | cohort grammar |
| `math.rs` | 871 | same:628 | cohort grammar |
| **TOTAL** | **168,750** | | |

### §5.2 Per-tranche delta budgets

| Tranche / Wave | JSON delta | CSS L4 delta | BBNF delta | Sheets delta | Cohort delta (5 grammars) | Aggregate |
|---|---|---|---|---|---|---|
| BA.W0 | −5% (tape-comment scrub) | −2% (same) | −3% (same) | −2% | −1% each | −2.5% (~−4200 LOC) |
| BA.W2 | (no regen; rename pass) | (no regen) | (no regen) | (no regen) | (no regen) | 0% |
| BA.W5 | −40% (OpenFrame retiral; direct-to-struct emit) | (no regen — retains existing path) | (no regen) | (no regen) | (no regen) | −0.7% (~−1400 LOC; mostly JSON) |
| **BA close** | ~2,100 LOC (down from 3,500) | ~104,800 LOC (down from 107,138) | ~20,860 LOC (down from 21,503) | ~13,800 LOC | (cohort: ~22K LOC) | ~163,560 LOC (−3.1% from pre-BA) |
| BB.W1 | (already retired in BA.W5) | −20% (CSS L4 14-variant OpenFrame retiral) + +5% (typed enum variants explode) = −15% net | −10% (BBNF OpenFrame retiral) +5% typed enum = −5% net | −10% +5% = −5% net | (no regen — cohort not yet templated) | −12.5% (~−20K LOC; mostly CSS L4) |
| BB.W2 | (no regen) | (no regen) | (no regen) | (no regen) | −60% to −70% (template emission compresses 5 cohorts; ~22K → ~6K aggregate) | −9.5% (~−16K LOC; cohort) |
| BB.W3 | +2% (Pratt + SIMD auto-detection emit) | +2% (same) | +2% (same) | +2% (same) | +1% each (cohort) | +1.7% (~+2.8K LOC) |
| BB.W4 | +1% (parse_in / parse_owned signatures) | +1% | +1% | +1% | +1% each | +1% |
| BB.W5 | +2% (Visitor + VisitTypes bitflag emit) | +5% (CSS L4 has many record types; visitor explodes) | +1% | +2% | +1% each | +2% |
| **BB close** | ~2,200 LOC | ~93,000 LOC | ~19,800 LOC | ~13,200 LOC | ~6,500 LOC (cohort) | ~134,700 LOC (−20.2% from pre-BA) |
| BC.W0 | (no regen — IR contract spec) | (no regen) | (no regen) | (no regen) | (no regen) | 0% |
| BC.W1 | (no regen — crate split) | (no regen) | (no regen) | (no regen) | (no regen) | 0% |
| BC.W2 | (TS + WASM stubs are SEPARATE, not added to existing generated/*.rs) | same | same | same | same | 0% (TS/WASM emitter outputs land at separate paths) |
| **BC close** | ~2,200 LOC | ~93,000 LOC | ~19,800 LOC | ~13,200 LOC | ~6,500 LOC | ~134,700 LOC (stable from BB) |

### §5.3 Per-wave budget enforcement gate

Each wave's commit body MUST include:

```
## Generated-LOC Budget

| File | Pre-wave LOC | Post-wave LOC | Delta | Within budget? |
|---|---|---|---|---|
| crates/core/src/grammar/generated/json.rs | N | N' | ΔN | ✓ / ✗ |
| ... | | | | |
```

Overflow without justification blocks the wave; per `audit/HARDENING-SYNTHESIS-2026-05-03.md:90-94`, this is the wave-close artefact requirement.

---

## §6 — Dispatch Instructions for Phase 2

The three Phase 2 plan agents (BA, BB, BC drafters) each receive this synthesis as their single source of truth. Each is responsible for ONE tranche document; cross-tranche references happen via carry-tags only.

### §6.1 What each Phase 2 agent MUST include

Common to all three agents:

1. **Tranche document at `docs/tranches/<X>/<X>.md`** — top-level tranche spec (gestalt + waves + gates + carries).
2. **Per-wave document at `docs/tranches/<X>/waves/W<N>.md`** for each wave defined in §1.
3. **Lock cross-reference table** (the per-tranche column from §2 expanded with citations to the wave that addresses each lock).
4. **Carry ledger** (carries FROM prior tranche + carries TO next tranche).
5. **Hard gate list** (from §1 hard gates, with each gate citing competitor + dataset + platform per Lock 8).
6. **Generated-LOC budget** (the per-grammar table from §5).

Agent BA additionally:
- Documents the §1.0 pre-BA cleanup ceremony as a precondition (NOT a wave).
- Names the §1.0 verification gate as the BA.W0 entry preflight.
- Drafts the BA.W6 close artefact format that BB.W0's preflight consumes.

Agent BB additionally:
- References BA→BB carry-tags by exact tag (BA→BB.C1 ... BA→BB.C5) per §1.1.
- Honours the `audit/HARDENING-SYNTHESIS-2026-05-03.md:107-127` amendment: rank.rs + tiering.rs creation moves INTO BB.W3, not BB.W0.
- Documents the BB.W6 close artefact format that BC.W0's preflight consumes.

Agent BC additionally:
- References BB→BC carry-tags by exact tag (BB→BC.C1 ... BB→BC.C4) per §1.2.
- Documents the IR contract spec at `docs/codegen-IR-CONTRACT.md` as the BC.W0 deliverable.
- Names BD only in carry-tags BC→BD.C1, BC→BD.C2, BC→BD.C3 — does NOT draft BD itself.

### §6.2 What each Phase 2 agent MUST NOT include

Common to all three:

1. **No relitigation of the 13 locks.** They are settled.
2. **No "AU baseline" or "≥ pre-W3" gates.** Every gate names a competitor + dataset + platform per Lock 8.
3. **No proc-macro façade for codegen** (other than `crates/path/`, `crates/path-core/`, `crates/path-ts/` which are SEPARATE per Lock 7).
4. **No tape resurrection** (no `Tape*` symbols, no columnar SoA, no "tape rebranded as fast-path").
5. **No fictional successor letters** (no AZ-V; no BD until BC carry-tags surface it).
6. **No corporate hedging** ("might want to consider"); voice is archaic-permissive ("hereupon", "begotten", "thereof").
7. **No metalanguage refs** (no commit hashes, no conversation history; cite path:line).
8. **No scope creep**: each tranche's scope is defined in §1; carries close at named waves only.

Agent BA must NOT:
- Migrate CSS L4, BBNF, Sheets, or the 5-grammar cohort to direct-to-struct (BB.W1/W2 carries that).
- Edit TS or WASM emitter source (BC.W2 + BD scope).
- Land per-domain optimiser pipeline (BB.W3 scope).
- Land `pointer!` macro production surface (BB.W5 scope; BA.W3 only consolidates the `path-core` crate).

Agent BB must NOT:
- Land TS or WASM emitter activation (BC.W2 + BD scope).
- Refactor the Rust emitter to consume IR contract (BC.W0 scope).
- Split the core crate (BC.W1 scope).
- Edit TS or WASM emitter source.

Agent BC must NOT:
- Activate TS or WASM emitters in production (BD scope).
- Edit grammar source files (BB.W3's auto-detection consumes them; BC consumes the IR they produce).
- Land any new direct-to-struct codegen (already done by BA.W5 + BB.W1).
- Resurrect any `@pratt` / `@simd` directive proposal (Lock 10).

### §6.3 Cross-tranche reference discipline

When a BB wave-doc says "extends BA.W4's cursor unification to all grammars", the reference is:

```
This wave extends BA.W4 (the cursor + byte-skip unification per Lock 3). BA.W4
landed `__EAGER_EMPTY_PATH` for the JSON path; BB.W2 generalises to CSS L4,
BBNF, Sheets, and the 5-grammar template cohort. The same `parse_with(input,
&path)` signature with cursor consultation elision applies to every generated
parser.
```

Not:

```
Picks up where BA's cursor work left off.  ← VAGUE — fault per ED4
```

Every cross-tranche reference cites the exact wave + carry-tag.

### §6.4 Voice + invariant locks

All three agents:

1. Voice: archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant"). Not corporate.
2. Citations: path:line, not paraphrase. `audit/CENSUS-2026-05-03.md:104` not "the audit cites a CSS host fn issue".
3. Tables: liberal use; markdown tables for every multi-row enumeration.
4. Hard caps: each Phase 2 agent declares its own hard cap (~30 min for BA — most complex; ~25 min for BB; ~25 min for BC). Pre-cap commit at 0.9N; halt at N.
5. Closure ratification: every wave gates closes from artefacts the wave creates; no orphan exit-criteria.

---

## §7 — Closing Posture

This synthesis fixes scope, sequencing, and lock-adherence; the three Phase 2 agents drafting BA / BB / BC documents have a single source of truth that prevents:

- Scope overlap (each tranche owns its domain; BA does not migrate non-JSON grammars; BB does not refactor backends; BC does not land production TS/WASM).
- Substrate-without-consumer (every wave's deliverable has a same-wave or next-wave consumer; the Era V failure mode is structurally precluded).
- Lock violation (every lock is addressed at least once across BA/BB/BC; the §2 cross-reference table is the verification surface).
- AU-anchored gates (every perf gate names a specific competitor + dataset + platform; sonic-rs / simdjson / lightning-css / cssparser are the SOTA anchors).
- God modules / god directories (Lock 13 is verified at every wave; the 23 >500 LOC files outside `generated/` retire at BA.W2; cohesive sibling APIs are mandatory).

The 13 locks are settled; this synthesis verifies the plan's surface area honours them. The tranches themselves are drafted by the Phase 2 agents from this scaffolding.

Hereupon BA opens.
