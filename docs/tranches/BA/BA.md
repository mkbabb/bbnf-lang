# BA — Surgical Foundation

## Gestalt

BA is the foundation tranche: hereupon the substrate is rebuilt from first principles, the grammar-specific leaks excised from supposedly-generic crates, the god modules and god directories partitioned per Lock 13, and JSON's direct-to-struct codegen lands as the demonstration of the new path. The tape's residue — comments, doc strings, the dead `generate/serialize/` directory — is scrubbed wholesale; the cursor-parse and byte-skip implementations unify into one parse function with `__EAGER_EMPTY_PATH` electing the fast path; the eight inline `#[cfg(test)]` violations migrate to `tests/`; the path-crate triplet (`bbnf-path` proc-macro, `bbnf-path-ts` cdylib, `crates/core/src/path/` runtime) consolidates per Lock 7; JSON `parse(twitter.json)` beats sonic-rs's M1 Pro 436 µs by ≥ 8% (`audit/SOTA-2026-05-03.md:50-58`).

CSS L4, BBNF, Sheets, and the five-grammar template cohort (BNF, CSV, EBNF, CSS Pretty, Math) remain on the existing emitter path until BB; BA does not migrate them, lest the layered re-org and god-module split cascade through nine grammars at once. BA produces the IR-and-emitter scaffolding that BB's generality wave consumes; the carry to BB is the demonstration-grammar (JSON) plus the renamed canonical pass surface (`Layout`/`LayoutSink`) plus the unified cursor substrate (`__EAGER_EMPTY_PATH`). The thesis is surgical excision begotten of demonstration: prove the path on JSON, retire the substrate that begets the 86.07% `Vec<OpenFrame>::clone` pathology (`audit/RESTART-SKETCH-2026-05-03.md:154-220`), and hand BB an emitter substrate that generalises by mechanism.

## Hard gates

Every gate cites a specific competitor + dataset + platform per Lock 8. Zero "AU baseline" or "≥ pre-W3" gates appear in any cell.

| ID | Gate | Anchor |
|---|---|---|
| BA-G1 | `JsonParser::parse(twitter.json)` ≤ 400 µs on M1 Pro, beating sonic-rs's 436 µs by ≥ 8% | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs benchmark_aarch64 twitter row) |
| BA-G2 | `JsonParser::parse(twitter.json)` consumes ≤ 2 heap allocations per parse-call (1 arena + 1 root-vec); zero `Vec<OpenFrame>::clone` sites detectable in samply 5K-sample profile | `audit/RESTART-SKETCH-2026-05-03.md:154-220` (the 86.07% pathology) |
| BA-G3 | Workspace `cargo xtask regen --check` ≤ 30 s on M1 Pro; CSS L4 `compile_paths_request` ≤ 25 s | Lane 04 of the HARDENING audit measured 59.98 s + 52.53 s baseline; gate halves both |
| BA-G4 | All 13 locks honoured at BA close per the §13-Lock cross-reference table; `crates/ir/src/passes/tests/substrate_audit.rs` green | Lock cross-reference is the contract |
| BA-G5 | God-module count (>500 LOC outside `generated/`) drops from 23 → 0 per `audit/CENSUS-2026-05-03.md:319-353` | Lock 13 |
| BA-G6 | `crates/core/src/` reorganised into the 9-directory layout from `audit/MODULES-2026-05-03.md:1107-1118`; no `pipeline.rs` + `pipeline/` co-existence | Lock 13, `feedback_directory_modules` |
| BA-G7 | Zero grammar-specific arms in `bbnf-ir`: `crates/ir/src/registry/strategy.rs:130-185` reads from workspace metadata; `crates/ir/src/passes/audit/payload_coverage.rs:69` enum carries only `Custom(&'static str)`; `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` deleted; CSS L4 host fn relocated to `crates/core/src/host/css_types.rs` | `audit/CENSUS-2026-05-03.md:103-122`, Lane 5 grammar-authoritative discipline |
| BA-G8 | Eight inline `#[cfg(test)]` blocks moved to `tests/`; `rg -nE '^#\[cfg\(test\)\]$' crates/*/src/` returns zero | `audit/CENSUS-2026-05-03.md:381-399`, `feedback_no-inline-tests` |
| BA-G9 | `JsonDocument::get<T>(twitter.json, &path)` ≤ 5× the eager parse cost on M1 Pro (sonic-rs's `get_unchecked` is ~0.1× of full parse; BA closes the 4196× gap to ≤ 5×) | `audit/RESTART-SKETCH-2026-05-03.md:31-41` (the BA `bbnf_get_twitter` 4196× gap) |
| BA-G10 | Generated-file LOC accounting: per-grammar pre-/post-BA LOC table emitted; no overflow without justified cause; baseline budget per the §Build/iter time gate | Lane 06 generated-code budget |

## Wave summary

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BA.W0 | Layered re-org of `crates/core/src/` into the 9-directory layout (`source/`, `parse/`, `lower/`, `codegen/`, `runtime/`, `path/`, `pipeline/`, `host/`, `lib.rs`); `pipeline.rs` collapses INTO `pipeline/mod.rs`; `crates/core/src/css_types.rs` relocates to `crates/core/src/host/css_types.rs`; tape-residue comment scrub (~50 hits across `runtime/` + `backend/` + `grammar/`); `generate/serialize/` directory deleted (156 LOC); 8 inline tests migrated. | Layer-name canon (Lock 2); no god directories (Lock 13); no metalanguage docs. | `cargo check --workspace` green; `rg -n 'TapeRec\|TapeCursor\|TapeBuilder\|TapeOffset' crates/*/src/` returns zero outside `tests/`; pipeline directory-form passes; per-grammar LOC table baseline emitted. |
| BA.W1 | Grammar-leak excision: `crates/ir/src/registry/strategy.rs:130-185` reads from `[workspace.metadata.bbnf-strategy]` only (no static `GRAMMARS` array); `crates/ir/src/passes/audit/payload_coverage.rs:69` `GrammarAuditTag` collapses to `Custom(&'static str)`; `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` deleted (240+ LOC); BBNF-specific shape mining generalises to a data-driven recogniser registry. | bbnf-ir knows zero grammar idents (Lock 5 IR contract precursor). | `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/` returns zero matches; `crates/ir/tests/` continues passing. |
| BA.W2 | God-module splits: 23 files >500 LOC outside `generated/` partition per `audit/CENSUS-2026-05-03.md:319-353` and `audit/MODULES-2026-05-03.md` per-file split recommendations. CSS L4 `runtime/css_l4/builder.rs` (1014) → `builder/{frame,declarations,selectors,color,dimensions,finalize}.rs`; `flat/struct_direct.rs` (1033) → `struct_direct/{header,body,fields,finalize}.rs`; etc. Layout-lowering rename: every `type_projection` / `type_collapsing` / `TypeMap` / `StructLayout` / `TypeDesc` / `schema_synthesis` reference in module names + doc comments rewrites to `layout_lowering`/`Layout`/`LayoutSink`. | Lock 2 (canon) + Lock 13 (no god modules). | `find crates -name '*.rs' ! -path '*/generated/*' \| xargs wc -l \| awk '$1>500'` returns empty; layout-lowering term is canonical. |
| BA.W3 | Path crate consolidation per Lock 7: rename `crates/bbnf-path/` → `crates/path/`; rename `crates/bbnf-path-ts/` → `crates/path-ts/`; introduce `crates/path-core/` shared non-proc-macro crate carrying `compile`/`lex`/`lower`/`validate` logic; `bbnf-path` and `bbnf-path-ts` path-dep on `path-core`; `bbnf-path-ts/src/fixture.rs` (248 LOC synthetic) deleted; `bbnf-path/src/registry.rs:132-135` grammar match arms read from per-grammar `pub const REGISTRY: StructRegistry`; `crates/core/src/runtime/path.rs` (163 LOC, the legacy borrowed alphabet) deleted; four `runtime/<g>/parse_with.rs` legacy lowering passes deleted. | Lock 7. Three crate names (path, path-core, path-ts); ~500 LOC mirror eliminated; runtime path types consume the typed alphabet directly. | `rg -n 'use crate::runtime::path::' crates/core/src/` returns zero; `crates/path-core/` exists; `crates/bbnf-path*/src/compile.rs` mirrors merged. |
| BA.W4 | Cursor + byte-skip unification: one `parse_with(input, &path)` per grammar; `__EAGER_EMPTY_PATH` LazyLock elides cursor consultation when path is empty; eager `JsonParser::parse` becomes `parse_with(input, &EMPTY_PATH)`. `JsonDocument::get<T>` reroutes through `parse_with(input, path)`. Cursor's `Skip` decision generates byte-skip code at codegen, not at runtime. | Lock 3 (cursor + byte-skip unified, cursor branch elided when path empty). | BA-G9 met (`get<T>` ≤ 5× full parse); `parse` and `parse_with` share one source; samply trace shows zero cursor calls on eager path. |
| BA.W5 | JSON direct-to-struct codegen first cut. Direct-projection emit replaces `OpenFrame` stack + `Vec<OpenFrame>::clone` for JSON only. Typed enum + per-rule generated `parse_<rule>(input, &mut p, &mut arena, &mut state, &mut cursor) -> Result<JsonValue<'p>, ParseErr>` functions. Byte-disjoint Alt emits direct `match first { b'{' => ... }` without speculative checkpoint. `OpenFrame` deleted from JSON path. CSS L4, BBNF, Sheets, and the 5-grammar template cohort STAY on existing emitter path; their migration is BB.W0/W1 carry. | Lock 1 (tape and columnar dead, direct-to-struct visible-and-internal); Lock 3 (one parse impl); Lock 9 (slice-borrow primary). | BA-G1 (≤ 400 µs); BA-G2 (≤ 2 heap allocs/parse); samply post-BA shows `Vec<OpenFrame>::clone` retired from JSON profile. |
| BA.W6 | BA close: PROGRESS / FINAL; lock cross-reference table verified; per-grammar generated-LOC table; carry ledger to BB.W0 named explicitly. | Lock-honoured at every gate; carry-tags BA→BB explicit. | `cargo nextest run -p bbnf -p bbnf-ir -p bbnf-analysis` 100% pass for BA-owned surfaces; `substrate_audit` green; per `audit/HARDENING-SYNTHESIS-2026-05-03.md:204` the BA-amendment punch-list closes. |

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

| Tag | Owner wave | Description |
|---|---|---|
| BA→BB.C1 | BA.W5 | Direct-to-struct codegen path lands for JSON; BB.W0/W1 extends it to CSS L4, BBNF, Sheets, and the 5-grammar template cohort. The emitter scaffolding is grammar-agnostic; only the per-grammar typed value sums + parse fns regenerate. |
| BA→BB.C2 | BA.W2 | Layout-lowering rename canonises the IR pass name; BB references `Layout`/`LayoutSink` only. |
| BA→BB.C3 | BA.W4 | Cursor-unified `parse_with` + `__EAGER_EMPTY_PATH` substrate; BB extends this to all grammars at BB.W2. |
| BA→BB.C4 | BA.W3 | `path-core` crate exists; BB's `pointer!["a","b",1]` macro at `crates/path/` reuses it without proc-macro/cdylib mirror. |
| BA→BB.C5 | BA.W1 | Grammar-agnostic `bbnf-ir`; BB's CSP/e-graph/miner extensions reference grammars only via `&str` ident through workspace metadata. |

## Carry-tags TO BC (skipping BB)

| Tag | Owner wave | Description |
|---|---|---|
| BA→BC.C1 | BA.W2 | Layout-lowering canon supports the IR contract spec BC.W0 codifies. |
| BA→BC.C2 | BA.W5 | Direct-to-struct emitter pattern (one IR walker, leaf emission per backend) is the precursor to BC's `Emitter` trait formalisation across Rust/TS/WASM. |

## 13-Lock honoured

Every cell names the wave that addresses the lock; empty cells are faults. The Notes column flags weak adherence or carry-deferral.

| Lock | Wave | Notes |
|---|---|---|
| L1. Tape + columnar dead | W0 (residue scrub); W5 (JSON OpenFrame retiral) | Era V columnar (`docs/tranches/AV/research/04-columnar-soa.md`) explicitly rejected; OpenFrame is the legacy substrate that retires, not new substrate. CSS L4 + BBNF + Sheets OpenFrame retiral routes to BB.W1. |
| L2. Layout lowering canon | W2 (rename pass) | Old terms (`type_projection`, `TypeMap`, `StructLayout`, `TypeDesc`, `schema_synthesis`) survive only in archived docs after BA.W2. BB references `Layout`/`LayoutSink` only via carry BA→BB.C2. |
| L3. Cursor + byte-skip unified | W4 (one parse impl, eager elides cursor) | `__EAGER_EMPTY_PATH` LazyLock at BA.W4 is the unification point. BB.W2 extends to all grammars via carry BA→BB.C3. |
| L4. Per-domain orthogonal optimisation | (deferred to BB.W3) | BB.W3 lands CSP → e-graph → miners → cost-model output-piping; no unified hypergraph. BA does not touch the optimiser pipeline. |
| L5. IR + per-backend lower | W5 demonstrates the pattern (Rust emitter consumes IR-shape) | BB.W1 generalises across 9 grammars; BC.W0 codifies the IR contract. The Rust emitter consumes IR-shape in BA.W5; BC.W0 documents the contract. |
| L6. xtask emits committed source | W0 (regen check ≤ 30 s; committed regen artefacts) | `bbnf-path`, `bbnf-path-ts` proc-macro shells are SEPARATE per Lock 7; not the codegen substrate. |
| L7. `crates/path/` consolidation | W3 (rename, path-core extraction, runtime/path.rs deletion, parse_with legacy excision) | Three crate names (path, path-core, path-ts) only; no fourth proc-macro shell. BB.W5 lands `pointer!` macro production surface; BC.W4 reconciles `bbnf-regex` endpoint. |
| L8. Surpass sonic-rs / simdjson / lightning-css | G1 (≤ 400 µs twitter beating sonic-rs 436 µs); G9 (4196× → ≤ 5× get gap) | Zero AU references; every gate names competitor + dataset + platform. BB and BC tighten the gates further. |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | W5 (JSON parse returns slice-borrowed `JsonValue<'p>`) | Default surface is `&'i str` slice + `Cow<'i, str>` per `audit/SOTA-2026-05-03.md:122-123`. Three-surface API (parse / parse_in / parse_owned) routes to BB.W4. |
| L10. Pratt + SIMD auto-detected | (deferred to BB.W3) | No grammar declares `@pratt` or `@simd`; the optimiser mines and emits accordingly. BA does not touch operator-chain or structural-alphabet recognisers. |
| L11. Path-deps for incubating sister crates | (deferred to BB.W0) | egraph + egraph-derive + csp-solver + bbnf-regex + parse-that as path-deps; simd-scan + bootstrap + analysis + lsp stay workspace-internal. BC.W4 freezes sister-crate APIs. |
| L12. ser + gorgeous archive BEFORE BA.W0 | Pre-BA ceremony (precondition) | Verification: `archive/{ser,gorgeous}/` exist; `Cargo.toml` workspace members reduced by 2. Not a tranche; one ceremony commit + a verification gate. |
| L13. No god directories; cohesive encapsulation at every level | W0 (9-directory layered re-org); W2 (god-module splits, 23 → 0); W3 (path crate triplet) | sonic-rs / lightningcss / simdjson cohesion is the standard. Files >500 LOC outside `generated/` forbidden after BA.W2. >10-children dirs mixing concerns forbidden after BA.W0. |

## Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BA.W2 god-module splits cascade into BA.W5 codegen breakage (renames pull through emitter) | Medium | BA.W2 lands renames first; BA.W5 regen comes last; xtask regen `--check` between waves. |
| BA.W5 direct-to-struct path regresses CSS L4 / Sheets / BBNF (whose paths are not migrated) | High | BA.W5 explicitly does NOT migrate non-JSON grammars; `EmitStrategy::for_grammar` retains the existing `OpenFrame` substrate for non-JSON grammars until BB.W0. The substrate-without-consumer concern is mitigated because BA.W5's consumer (JSON) lands in the SAME wave. |
| BA.W3 path consolidation breaks `pointer!` callers across `crates/core/tests/` | Medium | `path-core` is non-breaking (path-dep added); `bbnf-path` and `bbnf-path-ts` continue exporting their public API; the rename on disk is a name change, not an API change. |
| Tape-residue scrub introduces parser regressions (some tape comments are tied to lifetime narratives in safe-untouchable code) | Low | Comment-only scrub; zero functional source changes; `cargo nextest` between BA.W0 sub-commits. |
| Cursor-unified parse impl loses path-driven test coverage that exercises non-eager paths | Medium | BA.W4 retains `parse_with(input, path)` for non-eager; only the eager-fast-path constant-folds out; `tests/parse_with_*.rs` pass-rate is gated. |

## Build/iter time gate

BA shrinks generated/* LOC modestly. The tape-comment scrub eliminates repeated narrative blocks; `OpenFrame` pattern retiral for JSON shrinks `json.rs` from 3,500 LOC to ~2,000 LOC by mechanism per `audit/RESTART-SKETCH-2026-05-03.md:585-595`. BA.W2 god-module splits redistribute, not grow, the LOC mass. Estimated net delta to `crates/core/src/grammar/generated/`: **−5% to −10%** (~−4,200 to −8,400 LOC).

xtask iteration-time gate (BA-G3): `cargo xtask regen --check` ≤ 30 s; CSS L4 `compile_paths_request` ≤ 25 s. Lane 04 of the HARDENING audit measured 59.98 s + 52.53 s as the pre-BA baseline; halving is the BA close requirement.

### Generated-LOC budget table (pre-BA baseline → post-BA target)

| Grammar | Pre-BA LOC | Post-BA LOC | Net Delta | Source |
|---|---:|---:|---:|---|
| `json.rs` | 3,500 | ~2,100 | −40% (BA.W5 OpenFrame retiral) | `audit/MODULES-2026-05-03.md:621` |
| `bbnf.rs` | 21,503 | ~20,860 | −3% (W0 comment scrub) | same:619 |
| `css_l4.rs` | 107,138 | ~104,800 | −2% (W0 comment scrub) | same:622 |
| `google_sheets.rs` | 14,088 | ~13,800 | −2% (W0 comment scrub) | same:624 |
| `css_pretty.rs` | 9,021 | ~8,930 | −1% (W0 comment scrub) | same:623 |
| `ebnf.rs` | 7,646 | ~7,570 | −1% (W0 comment scrub) | same:625 |
| `bnf.rs` | 3,290 | ~3,257 | −1% (W0 comment scrub) | same:626 |
| `csv.rs` | 1,693 | ~1,676 | −1% (W0 comment scrub) | same:627 |
| `math.rs` | 871 | ~862 | −1% (W0 comment scrub) | same:628 |
| **TOTAL** | **168,750** | **~163,855** | **−2.9%** | aggregate |

Each wave's commit body MUST include a per-file `## Generated-LOC Budget` table. Overflow without justification blocks the wave.

## Voice locks

§V1. Voice is archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant", "extant"). Not corporate. Per `feedback_archaic_diction_is_voice`.

§V2. No metalanguage. Documents do NOT reference commits, conversation history, or the plan's draft history. Cite path:line. Per `feedback_no_metalanguage_docs`.

§V3. State the deliverable. State the gate. Move on.

§V4. Citations are path:line, not paraphrase. `audit/CENSUS-2026-05-03.md:104` not "the audit cites a CSS host fn issue".

§V5. Tables are liberal; markdown tables for every multi-row enumeration.

## Closing posture

Hereupon BA opens. The substrate is the typed-enum + slice-borrow; the demonstration grammar is JSON; the carry to BB is the renamed canonical surface, the cursor-unified parse, the consolidated path-crate triplet, the grammar-agnostic IR, and the direct-to-struct emitter scaffolding. The 13 locks are settled; BA verifies them in seven waves and hands BB a ratified foundation.
