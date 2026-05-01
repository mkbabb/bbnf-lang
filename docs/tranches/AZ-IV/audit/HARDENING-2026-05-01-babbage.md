# AZ-IV Hardening Pass 3 — Substrate Activation Matrix (Babbage)

Read-only audit. Worktree: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-babbage`. Base commit `db8b00ad`. CARGO_TARGET_DIR `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-babbage/target/babbage`.

The mandate is the substrate-with-consumer matrix Mencius narrowed in HARDENING-SYNTHESIS-2026-05-01 §Accepted Optimization Claims and Averroes hardened in LOSS-PREVENTION-SYNTHESIS-2026-05-01 §Loss Risks Found item 4. Each row classifies the substrate as **CONSUMED**, **WIRED-NOT-CONSUMED**, **UNDERUTILIZED**, **DEAD**, or **PARTIAL** with site citations. The W2 file bounds and W2 hard-gate enumeration are then cross-checked against the matrix.

## 1. Audit Disposition

| Verdict | Count | Names |
|---|---:|---|
| CONSUMED | 9 | Pratt (BBNF only), Shape variants (Wrap/Flat/HRegex/AltDispatch/Object/Array/String/Number/Keyword/Scalar), 17 IR passes (15 named + 2 deleted/missing), `view::named_types`, `runtime::view`, `simd_scan` (5 grammars), egraph default rewrites, FactAuthority (1 production caller), shape recognizer→CSP→backend chain |
| PARTIAL | 4 | CSP authority (sidecar still re-overrides for KeyDispatch on `key_dispatch_configs.contains_key`), regex HIR (engine decision read by `cost_model.rs`, but `recognizer_decision_consumption.rs` test stream confirms not-yet-emitted-as-chosen), DTA (`dfa_codegen` — `emit_regex_scan_adapter` live; `emit_dfa_inline_body` orphaned), `view::peel` (consumed by `view::named_types` only) |
| WIRED-NOT-CONSUMED | 5 | Ruler enumeration / oracle / residue (NO production caller; tests + `examples/ruler_smoke.rs` only), `RuleSet`/`options.rewrites` load (only an `eprintln` under `BBNF_PIPELINE_REPORT=1`), `shape_dict_templates` (mined + selected, NEVER read by any backend emitter), `shape_dict_selection` (same), `type_obligations` (populated and round-tripped to docs but no codegen-emitted diagnostic stream) |
| DEAD | 2 | `emit_dfa_inline_body` (defined `dfa_codegen.rs:562`, NO callers — confirms Meitner narrowing), `merge_regex_alts` pass (deleted Tranche H-7, still listed in `docs/codegen-paths.md` IR-pass enumeration as a 17th op) |
| UNDERUTILIZED | 4 | Pratt (5/9 grammars emit `PRECEDENCE_ENTRIES: &[]` — only `bbnf` populates 7 LUTs; `json/csv/ebnf/bnf/math` emit empty Pratt scaffolding), `simd-scan` (5/9 grammars; tailwind/CSS L4 path narrower than docs imply per Mencius), CVC enumeration alphabet sources (`Bool` smoke only), egraph rewrite family (regex-algebra family + universal/suffix; no grammar-imported rules participate) |

## 2. Substrate Activation Matrix

| # | Substrate | Definition site | Registration site | Invocation site | Output consumer | Active production grammars | Verdict |
|---:|---|---|---|---|---|---|---|
| 1 | CSP **shape::install** (`6f386ec2`) | `crates/ir/src/passes/csp_strategy/constraints/shape.rs` | `crates/ir/src/passes/csp_strategy/constraints/mod.rs` install loop | `passes::solve_grammar_components` → `pipeline/compile.rs:?` (~`compile.rs:885`) | `crates/core/src/backend/strategy/alt_strategy.rs:140-188` reads `recognizer_decisions[id].alt_mode`; `crates/core/src/backend/driver/wrap.rs:105-111` reads `wrap_mode` | All 9 (decision lattice runs grammar-wide) | **PARTIAL** — alt_strategy.rs:163-165 and :180-184 still re-override CSP via `ir.key_dispatch_configs.contains_key(id)`; CSP output is consumed but its authority is overridden when the legacy detector disagrees |
| 2 | CSP **layout::install** (`c6140556`) | `crates/ir/src/passes/csp_strategy/constraints/layout.rs` | same install loop | same solver entry | `wrap.rs:105-111` → `csp_wrap_mode` reads `recognizer_decisions[id].wrap_mode` for `BalancedScan`/`SepBy`/`Generic` | All 9 | **CONSUMED** — but the `wrap.rs:97-104` block still cross-checks `delim_scan_configs.contains_key(id)` as a sidecar fall-through when CSP returns no fact |
| 3 | CSP **dispatch::install** (`7d4eaa53`) | `crates/ir/src/passes/csp_strategy/constraints/dispatch.rs` | same install loop | same solver entry | `alt_strategy.rs::decide_alt_strategy` reads `KeyDispatch`/`ByteDispatch`/`Checkpoint` | All 9 | **PARTIAL** — see row 1: the override on `key_dispatch_configs.contains_key` is the exact "consumer reads from sidecar" anti-pattern the W3b dispatch installer was supposed to retire. The pin lands; the sidecar is then re-consulted |
| 4 | **Strategy aligner** (`4432d7b1`) — `csp-solver` `add_equals` delegation | `crates/csp-solver/src/lib.rs:184` (`add_equals`) | the three install fns (rows 1-3) | runs each compile when CSP runs | tests in `crates/ir/tests/lattices/csp_authority.rs` (5 PASS); production reachability via rows 1-3 | All 9 | **CONSUMED** at the API boundary; alignment lives or dies with rows 1-3 — re-override pattern still present |
| 5 | **Egraph default rewrites** (`crates/ir/src/egraph/rules/`) | `regex.rs` (`DeduplicateAltBranches`, `FuseAltRegexBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`), `suffix.rs` (`CommonSuffixFactor`), `universal.rs` (`AltOfSingle`, `ConcatLiterals`, `RepeatOfSingle`, `WrapOfEpsilonScalar`) | `crates/ir/src/egraph/rules/mod.rs::default_rules` | `crates/ir/src/egraph/mod.rs::build_and_saturate` (called from `pipeline/compile.rs:741`) | `write_back_optimized` mutates `ir.rules[i].body` | All 9 | **CONSUMED** — production saturation runs; but the rewrite *family* is hard-coded — no grammar-imported `RuleSet` participates |
| 6 | **`Map { fn_id }` extraction** (AZ-III C2 carry) | `crates/ir/src/egraph/cost.rs:130` (`Map { inner, fn_id: _ } => structural + child_cost(*inner)`); `crates/ir/src/egraph/write_back.rs:311` rebuild | egraph extractor | post-saturation extract → `pipeline/compile.rs:743` `write_back_optimized` | `crates/core/src/backend/rust/emitter/shapes/{hregex,flat,alt_dispatch}` walk extracted `Map { fn_id }` | All 9 | **PARTIAL** — Mencius narrowed to "extraction can choose the cheaper inner node if semantics are not pinned." The `fn_id: _` discard in `cost.rs:130` is the locus: Map cost = structural + inner only; no fn-payload signal. The W0 carry-condition "named test fails if extraction strips typed payloads" is the open gate. Concern is **not** wider than `cost.rs` — write_back already preserves `fn_id` |
| 7 | **Ruler / CVC enumeration** (`crates/egraph/src/ruler/enumerate.rs`, `oracle.rs`, `residue.rs`) | per-file in `crates/egraph/src/ruler/` | `crates/egraph/src/ruler/mod.rs` | tests + `crates/egraph/examples/ruler_smoke.rs:126` | NONE in `crates/core` or `crates/ir` production code | none | **WIRED-NOT-CONSUMED** — the enumerator, oracle, residue filter, and ranker all exist as substrate but are invoked only by `egraph/tests/ruler_*.rs` and the smoke example. Production `pipeline/compile.rs` does not call them; `pipeline.rs` only logs `eprintln!("[pipeline] rewrites: {} rule(s) loaded for cost-config")` under `BBNF_PIPELINE_REPORT=1` |
| 8 | **`RuleSet` / `options.rewrites`** | `crates/ir/src/rewrites/mod.rs` (load + storage + RON I/O) | `crates/core/src/pipeline.rs:37` (`pub rewrites: Option<bbnf_ir::rewrites::RuleSet>`) | `pipeline/compile.rs:560-573` (eprint only) | NONE — neither egraph nor codegen reads it | none | **WIRED-NOT-CONSUMED** — comment on line ~565 reads "consumes them lands in BB.scaffold.B"; today the pipeline drops the loaded `RuleSet` after counting it. There are no `grammar/**/rewrites/**` directories on disk |
| 9 | **`shape_dict_templates`** (ShapeDictMiner output) | `crates/ir/src/passes/recognizers/shape_dict.rs` (miner) → `crates/ir/src/types/grammar.rs:226` storage | populated in `mine_recognizers` (`pipeline/compile.rs:850`) | `solve_shape_dict_selection` (`pipeline/compile.rs:859-861`) | NONE — no `crates/core/src/backend/**` reads `shape_dict_templates` or `shape_dict_selection` | none | **WIRED-NOT-CONSUMED** — miner runs, solver picks, but `crates/core/tests/shape_dict_css.rs` is the sole reader. Mencius narrowed: "Generic shape dictionary runtime consumption is not substantiated." Confirmed: zero non-test consumers |
| 10 | **`shape_dict_selection`** | `crates/ir/src/types/grammar.rs:231` | `csp_strategy::constraints::shape_dict::solve_shape_dict_selection` (`pipeline/compile.rs:861`) | same | NONE — no backend emitter reads | none | **WIRED-NOT-CONSUMED** — sibling of row 9 |
| 11 | **`simd-scan` / structural scan** (`crates/simd-scan/`) | `crates/simd-scan/src/{scalar,avx2,avx512,neon,wasm}.rs` + `alphabet.rs`, `index.rs`, `parity.rs` | `crates/core/src/pipeline/compile.rs:847` (`compute_structural_alphabet`) | embedded into generated source via the per-grammar `::simd_scan::scan_structural(input, &alphabet)` calls | `crates/core/src/grammar/generated/{json,bbnf,google_sheets,csv,ebnf}.rs` | 5/9 grammars (json, bbnf, google_sheets, csv, ebnf — math/bnf/css_l4/css_pretty absent) | **UNDERUTILIZED** — Mencius narrowed: "wired but gated and narrower than older comments imply." Confirmed: only 5/9 grammars carry the structural-scan adapter. CSS L4 / CSS Pretty / math / bnf do not get scan_structural wired even though their lower output runs through `compute_structural_alphabet` |
| 12 | **Regex HIR** (bbnf side) | `crates/core/src/generate/regex/{cost_model,byte_class,last_byte_set,phf,emit/,patterns/,mod}.rs` | `crates/core/src/backend/driver/analysis.rs:12` (`use crate::generate::regex`) | inline emission per Map+Regex; `RegexEngine` decisions extracted at `pipeline/compile.rs:907` | `cost_model.rs:175` reads `ir.regex_engine_decisions.get(&sid).cloned()`; `dfa_codegen.rs` and `shapes/hregex.rs` consume the chosen scanner adapter | All 9 | **PARTIAL** — egraph saturation runs over IR `Regex(StringId)` nodes, NOT over the parse-that bespoke HIR; the HIR-egraph connection is in `bbnf-regex` (out of audit scope) but is referenced by `default_rules` regex-family doc. Mencius: "Regex engine decisions are consulted, but not always emitted as the exact chosen engine path" — `crates/core/tests/regex_engine_authoritative.rs:54-87` proves the gap |
| 13 | **Pratt / `PatternAnnotations`** | `crates/ir/src/passes/recognizers/shape_dispatch/pratt.rs` (recognizer); `crates/ir/src/passes/patterns/mod.rs` (`PatternAnnotations`) | per-rule populate during `mine_recognizers` | `crates/core/src/backend/rust/emitter/shapes/pratt.rs::emit_parse_pratt` (`shapes/mod.rs:267`); `crates/core/src/backend/rust/emitter/precedence.rs` | per-grammar `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` consts, then aggregate `PRECEDENCE_ENTRIES` | bbnf only (7 populated LUTs) — json/csv/ebnf/bnf/math/google_sheets/css_l4/css_pretty all emit `PRECEDENCE_ENTRIES: &[]` | **UNDERUTILIZED** + Pratt CONSUMED via bbnf only. `PatternAnnotations` is a recognizer fact (per Meitner's narrowing in §Accepted Source Claims #7: "legacy and under-consumed but still read by Pratt detection") — kept alive by `pratt.rs:90` |
| 14 | **`view::named_types`** | `crates/core/src/backend/rust/view/named_types.rs` | `crates/core/src/backend/rust/view/mod.rs` | `crates/core/src/backend/driver/analysis.rs:149` (`RustNamedTypes::from_ir(ir)`) | resolver passed into driver state | All 9 | **CONSUMED** |
| 15 | **`view::peel`** | `crates/core/src/backend/rust/view/peel.rs::unwrap_structural_wrappers` | `crates/core/src/backend/rust/view/mod.rs` | `crates/core/src/backend/rust/view/named_types.rs:57` only | row 14's named-types resolver | All 9 (transitively) | **PARTIAL/CONSUMED** — single non-test caller; legitimate but tightly coupled |
| 16 | **`view::color`** | `crates/core/src/backend/rust/view/color.rs` (`Color`, `ColorSpace`, `COLOR_PAYLOAD_BYTES`) | `crates/core/src/runtime/view.rs:35` (`pub use crate::backend::rust::view::color::{...}`) | re-exported only — Meitner narrowed "compatibility/test-used; runtime `CssColor` is current" | `crates/core/tests/css_*` | runtime CSS uses `runtime::css_l4::CssColor` instead | **DEAD-COMPAT** (a hybrid) — re-export shim with no runtime consumer; tests still reference. Per AZ-IV §Hard Gate 10: "old color compatibility … deleted" |
| 17 | **`runtime/view.rs`** (live shape) | `crates/core/src/runtime/view.rs` | `crates/core/src/runtime/mod.rs` | runtime accessors via re-exports | row 16 colors + payload bytes | All 9 | **CONSUMED** |
| 18 | **FactAuthority** (`d8f43633`) | `crates/ir/src/passes/facts/authority.rs` (renamed `passes/facts/mod.rs:68` per current tree) | `crates/ir/src/passes/mod.rs:44` (`pub use facts::FactAuthority`) | `crates/ir/src/passes/recognizers/shape_dispatch/alt_dispatch.rs:82` (`FactAuthority::new(ir)`) | branch admissibility decisions for AltDispatch shape | All 9 (gated on shape match) | **CONSUMED** but **PARTIAL on coverage** — only ONE production caller (alt_dispatch). The 5 disconnect tests in `tests/passes/fact_authority.rs` exercise `node_fact`, `alt_dispatch_admissible`, `has_family_recognizers`, `rule_shape`, `shape_assignments`, but only `rule_shape`/`alt_dispatch_admissible` reach a production reader |
| 19 | **`type_obligations`** (UnresolvedCompoundRef + HeterogeneousAltJoin) | `crates/ir/src/passes/types/obligation.rs` | `crates/ir/src/types/grammar.rs::type_obligations: Vec<TypeObligation>` | populated by `project_types` CSP | `crates/core/src/backend/rust/ir_types.rs:346` and `crates/core/src/backend/ts/projection.rs:40` ONLY mention via comment; `crates/core/src/grammar/schema/build.rs:312` matches `TypeDesc::HeterogeneousAltJoin(_)` (not the obligation itself); `analysis/src/state/diagnostics/ir_analysis.rs:229` is the sole reader of `TypeDesc::HeterogeneousAltJoin` for diagnostics | All 9 (vector empty for many) | **WIRED-NOT-CONSUMED for `type_obligations` Vec** — codegen reads the `TypeDesc::HeterogeneousAltJoin` projection (consumed) but not the obligation stream itself. Tests in `tests/types_heterogeneous_alt.rs` are the only readers of `ir.type_obligations` |
| 20 | **17 IR passes** (`docs/codegen-paths.md` enumeration) | `crates/ir/src/passes/{transform,sets,types,recognizers,...}` | `crates/ir/src/passes/mod.rs` | `crates/core/src/pipeline/compile.rs:614-781` (the structural normalizer loop + post-loop passes) | mutates `GrammarIR` | All 9 (AOT) | **PARTIAL** — actual count per `compile.rs`: `compute_first_sets`, `eliminate_indirect_lr`, `eliminate_direct_lr`, `compute_aliases`, `compute_transparent`, `canonicalize_aliases`, `compute_scc`, `prune_unreachable`, `inline_acyclic`, `prune_unreachable`, `compute_scc`, `fuse_single_use`, `prune_unreachable`, `eliminate_epsilon`, `merge_literals`, `factor_common_prefixes`, then `hoist_recurring_patterns`, `egraph saturate+writeback`, `refine_span_eligibility`, `compute_follow_sets`, `factor_regex_with_lookahead`, `mine_recognizers`, `compute_structural_alphabet`, `solve_grammar_components`, `extract_regex_engine_decisions`, `solve_shape_dict_selection`, `generate_dispatch_tables`, `project_types`, `compute_payload_layouts`. **`force_inline` and `merge_regex_alts` are NOT invoked** in production; the doc lists `merge_regex_alts` but it was deleted Tranche H-7 (see `crates/ir/src/egraph/rules/regex.rs:228`). VM path (`crates/ir/src/vm/compiler/mod.rs::compile`) does NOT run any IR passes — it consumes pre-passed IR |
| 21 | **VM path pass coverage** | `crates/ir/src/vm/compiler/mod.rs::compile` | — | called downstream of AOT pipeline | bytecode emission | VM-WASM consumers | **CONSUMED via shared AOT** — no separate VM IR pass invocation. The codegen-paths.md "Same passes as AOT" is correct because there is only one pipeline; VM diverges only at compile-vs-emit |
| 22 | **`emit_dfa_inline_body`** | `crates/core/src/backend/rust/emitter/dfa_codegen.rs:562` | `crates/core/src/backend/rust/emitter/mod.rs:13` (`pub mod dfa_codegen`) | NO callers in the source tree (test or production) | NONE | none | **DEAD** — confirms Pauli/Meitner accepted source claim #1: "internally orphaned." `emit_regex_scan_adapter` is the live entry. AZ-IV §Hard Gate 10 names this surface for deletion |
| 23 | **`merge_regex_alts` pass** | DELETED in Tranche H-7 per `crates/ir/src/egraph/rules/regex.rs:228` and `crates/ir/src/passes/mod.rs:26` | — | — | — | none | **DEAD** — but `docs/codegen-paths.md` IR-pass enumeration still lists it; counting it brings the doc to "17 ops" but the live pipeline runs ~28 named timer spans, with `merge_regex_alts` and `force_inline` not among them |
| 24 | **`force_inline`** | NOT FOUND in `crates/ir/src/passes/` | — | — | — | none | **DEAD or non-existent** — listed in `docs/codegen-paths.md`. Either renamed (no obvious successor) or never landed; pipeline calls only `inline_acyclic` |
| 25 | **Span eligibility / FOLLOW sets / SCC detection** | `crates/ir/src/passes/sets/`, `passes/lr.rs`, `passes/recognizers/scc.rs` | various | `pipeline/compile.rs:700,704,773,778-779` | dispatch tables and FIRST/FOLLOW sidecars | All 9 | **CONSUMED** |
| 26 | **`regex_engine_decisions`** | `crates/ir/src/passes/recognizers/regex_engine.rs` | `pipeline/compile.rs:907` `extract_regex_engine_decisions` | `crates/core/src/generate/regex/cost_model.rs:175` reader | scanner_plan resolution | All 9 (where Regex nodes exist) | **CONSUMED** but PARTIAL per row 12 — chosen variant not always emitted as exact chosen engine |

## 3. Cross-Check Against W2 File Bounds

W2 file bounds (W2.md §File Bounds) cover:

- `crates/ir/src/rewrites/**` — covers row 7 substrate (rewrite tier base), but does not enumerate consumers
- `crates/egraph/src/ruler/**` — covers row 7 ruler substrate
- `crates/ir/src/egraph/**` — covers rows 5, 6
- `crates/ir/src/passes/csp_strategy/**` — covers rows 1-4
- `crates/core/src/backend/strategy/**`, `backend/driver/**` — covers row 1, 2 consumers
- `crates/core/src/generate/regex/**` — covers row 12, 26
- `crates/core/src/backend/rust/emitter/dfa_codegen.rs` — covers rows 22, 24
- `crates/core/src/backend/rust/emitter/shapes/**` — covers row 13
- `crates/simd-scan/**` — covers row 11
- `crates/core/src/view/**`, `runtime/**` — covers rows 14-17

**MISSING from W2 file bounds**:

1. `crates/core/src/pipeline/compile.rs:560-573` — the load-and-drop site for `options.rewrites` (row 8). W2.1 ("Rewrite Ruler Chain") names `pipeline/compile.rs` but the AZ-IV.W2 bounds list it as **modify** — sufficient.
2. **`crates/ir/src/passes/types/obligation.rs`** — row 19's `type_obligations` Vec has no production reader. W2 file bounds do NOT cover `crates/ir/src/passes/types/**`. The substrate sits orphan-by-omission. W3 §Verification covers it for "type-inference parity," but the **decision to drain or delete** the obligation stream falls outside W2's stated bounds and outside W3's mandate.
3. **`crates/ir/src/passes/recognizers/shape_dict.rs`** — rows 9-10. The miner's templates and the solver's selection are NEVER read by any backend emitter. W2.3 ("Shape SIMD Consumption") names `csp_strategy/**` and `shapes/**` but does not name `passes/recognizers/shape_dict.rs` even though the miner's output is the load-bearing artefact whose consumer absence W2 must close.
4. **`crates/core/src/backend/rust/emitter/shapes/inline/`** — row 13 sub-substrate. The inline-shape mod doc reads "intentionally has one live entry point: emit_seq_branch_structural_struct_direct" but is not surfaced in W2's denominator artefact.

## 4. Cross-Check Against W2 Hard Gates

| W2 Hard Gate | Substrate covered (row) | Substrate missing |
|---|---|---|
| 1. Every non-empty loaded rewrite/ruler ruleset proves the full production chain | rows 7, 8 | — |
| 2. Ruler enumeration, egraph residue, VM oracle, ranker/tiering, schema/provenance, grammar rewrite dirs | row 7, 8 | grammar-colocated rewrite dirs (none on disk per `find grammar -name rewrites`) — denominator zero |
| 3. BB numeric floors | rows 5, 7 | — |
| 4. CSP decisions are consumer-authoritative or removed | rows 1-3 | re-override pattern still active in `alt_strategy.rs:163-184` — gate text says "removed" but the surface is still present |
| 5. Regex engine decisions emit the chosen scanner class and resolve Tailwind timeout | rows 12, 26 | — |
| 6. shape_dict/SIMD/structural-scan decisions have runtime evidence for every active selected fact or are retired | rows 9, 10, 11 | row 9, 10 must produce evidence or be deleted. Row 11 underutilized (5/9 grammars only) |
| 7. DTA/dfa stale runtime claims are deleted; dead inline code is gone | rows 22, 23, 24 | — |
| 8. Pratt/view consumers are proven grammar-general or explicitly typed | rows 13, 14-17 | row 13 underutilized (1/9 grammars populates non-empty entries) — gate says "grammar-general" but Pratt reaches grammar-generality only in `bbnf` |
| 9. `W2-substrate-denominator.md` covers every active mined fact | matrix rows 1-26 | rows 9, 10, 19 (the WIRED-NOT-CONSUMED triplet) MUST be in the denominator with disposition; row 7 (ruler) MUST be in the denominator; rows 22, 24 MUST be in the deletion ledger |

## 5. Hard Findings (numbered)

1. **WIRED-NOT-CONSUMED — `egraph::ruler` family**. `crates/egraph/src/ruler/{enumerate,oracle,residue}.rs` have ZERO production callers in `crates/core` or `crates/ir`. The only invocations are `crates/egraph/tests/ruler_*.rs` (3 tests) and `crates/egraph/examples/ruler_smoke.rs`. AZ-IV W2 hard-gate 1 cannot close without either a production caller from `pipeline/compile.rs` or explicit retirement.
2. **WIRED-NOT-CONSUMED — `RuleSet` load**. `crates/core/src/pipeline.rs:37` exposes `pub rewrites: Option<RuleSet>` but `pipeline/compile.rs:560-573` only emits `eprintln!("[pipeline] rewrites: {} rule(s) loaded for cost-config")` under `BBNF_PIPELINE_REPORT=1`. The loaded `RuleSet` then goes out of scope. Egraph saturation at `compile.rs:741` calls `bbnf_ir::egraph::build_and_saturate(&ir)` — note: `&ir`, not `&ir, &options.rewrites`. The signature does not even accept user rules.
3. **WIRED-NOT-CONSUMED — `shape_dict_templates` / `shape_dict_selection`**. The miner runs (`compile.rs:850`) and the CSP solver picks a subset (`compile.rs:861`), but no `crates/core/src/backend/**` file reads these fields. The only readers are `crates/core/tests/shape_dict_*.rs`. AZ-IV W2.3 hard-gate 6 ("runtime evidence for every active selected fact or … retired") is open.
4. **WIRED-NOT-CONSUMED — `ir.type_obligations`**. The `TypeObligation::UnresolvedCompoundRef` and `HeterogeneousAltJoin` enum populates a `Vec<TypeObligation>` on `GrammarIR` but no production code drains it. The `TypeDesc::HeterogeneousAltJoin(branches)` projection IS consumed (`grammar/schema/build.rs:312`, `backend/rust/ir_types.rs:342`, `backend/ts/projection.rs:37`, `analysis/.../ir_analysis.rs:229`), but the obligation stream itself is read only by `crates/core/tests/types_heterogeneous_alt.rs`. Status either drain-into-diagnostic-emit or delete.
5. **DEAD — `emit_dfa_inline_body`**. Defined `dfa_codegen.rs:562`. Zero callers. AZ-IV §Hard Gate 10 names it explicitly for deletion. W2.4 sub-gate covers this.
6. **DEAD — `merge_regex_alts` pass and `force_inline`** in `docs/codegen-paths.md`. The 17-pass list overstates the live pipeline. `merge_regex_alts` is documented as deleted in Tranche H-7 (`crates/ir/src/egraph/rules/regex.rs:228`); `force_inline` is not present in `crates/ir/src/passes/`. Doc must reflect actual ~28-span pipeline. This is a **W0** doc-truth gate concern that bleeds into W2 (the "every active substrate ... has generated/runtime evidence" gate cannot close while `docs/codegen-paths.md` mis-counts the substrate).
7. **PARTIAL — CSP authority not yet authoritative**. `alt_strategy.rs:163-184` still reads `ir.key_dispatch_configs.contains_key(id)` as a re-override of the CSP-chosen `AltMode`. The W3b dispatch installer commit body explicitly identifies this anti-pattern; the installer was supposed to **retire** it, but the override surface remains. AZ-IV W2.2 hard-gate 4 ("CSP decisions are consumer-authoritative or removed") is open.
8. **UNDERUTILIZED — Pratt 5/9 grammars empty**. `crates/core/src/grammar/generated/{json,csv,ebnf,bnf,math}.rs` all emit `pub const PRECEDENCE_ENTRIES: &[PrattEntry] = &[]`. Only `bbnf.rs` populates 7 LUTs. The aggregate `PRECEDENCE_LUT` const + `PRECEDENCE_ENTRIES` slice is generated for every grammar regardless of whether any rule was Pratt-classified, producing dead aggregates in 5/9. This is a generator-side code-size concern (per `feedback_generated_size_budget`) and a W2.5 grammar-generality concern.
9. **UNDERUTILIZED — `simd-scan` 5/9 grammars**. Generated `scan_structural` adapter present in `json,bbnf,google_sheets,csv,ebnf` only. CSS L4, CSS Pretty, math, bnf do not get the structural-scan adapter even though `compute_structural_alphabet` runs unconditionally. Mencius's narrowing ("wired but gated and narrower than older comments imply") is precisely correct.
10. **PARTIAL — Map fn_id extraction**. The W0-carry concern is local to `crates/ir/src/egraph/cost.rs:130` — `Map { inner, fn_id: _ } => structural + child_cost(*inner)` ignores fn_id during cost. The egraph extractor will choose the cheaper inner node when an equivalent un-wrapped form exists in the same e-class. The fix is a fn_id cost penalty or a hard-pin that any e-class containing a Map node must extract through Map. Issue is **NOT wider** than `cost.rs`; `write_back.rs:311` already preserves `fn_id` correctly post-extraction.

## 6. Exact Wave-Amendment Text

Add these substrates to W2.md hard-gate 9 enumeration ("§Hard Gate item 9 — `docs/tranches/AZ-IV/audit/W2-substrate-denominator.md` covers every active mined fact/sidecar/rule/template/shape/scan/Pratt/view/regex/CSP/egraph surface"):

```diff
 9. `docs/tranches/AZ-IV/audit/W2-substrate-denominator.md` covers every active mined fact/sidecar/rule/template/shape/scan/Pratt/view/regex/CSP/egraph surface.
+   The denominator MUST list dispositions for the following WIRED-NOT-CONSUMED
+   substrates identified in AZ-IV.HARDENING-2026-05-01-babbage:
+     a. `egraph::ruler::{enumerate,oracle,residue}` — production caller in
+        `crates/core/src/pipeline/compile.rs` or explicit retirement.
+     b. `crates/core/src/pipeline.rs::CompileOptions::rewrites` and
+        `pipeline/compile.rs:560-573` — the `RuleSet` load site is currently
+        an `eprintln`-only sink; consumer wiring or deletion.
+     c. `GrammarIR::shape_dict_templates` and `GrammarIR::shape_dict_selection` —
+        miner output and CSP-selected subset are NEVER read by any backend
+        emitter; emitter consumer or retirement.
+     d. `GrammarIR::type_obligations` (`Vec<TypeObligation>`) — drain the stream
+        through codegen-emitted diagnostic surface or delete the field.
+     e. `dfa_codegen::emit_dfa_inline_body` — dead public helper for deletion.
+     f. `docs/codegen-paths.md` IR-pass list overstates the live pipeline:
+        `merge_regex_alts` is deleted (Tranche H-7), `force_inline` is not
+        present in `crates/ir/src/passes/`. Doc must reflect actual span set.
```

Add this row to W2.md File Bounds:

```diff
 | `crates/core/src/view/**` | modify-carve |
 | `crates/core/src/runtime/**` | modify-carve |
+| `crates/ir/src/passes/types/obligation.rs` | modify (row 19 substrate denominator) |
+| `crates/ir/src/passes/recognizers/shape_dict.rs` | modify (rows 9, 10 substrate denominator) |
+| `crates/ir/src/rewrites/**` already listed; clarify covers `pipeline.rs` `options.rewrites` field | (no change; clarification only) |
```

Amend W2 §Triumvirate Dispatch with:

```diff
 - denominator enumeration reveals more active substrates than W2 file bounds can own.
+- WIRED-NOT-CONSUMED count (from substrate matrix) exceeds 5 at audit start, OR
+  CONSUMED count is less than 60% of total substrates audited at W2 mid-point.
```

Amend W3 §Hard Gates (cross-cited from W2) — already covered by Hard Gate 9 of AZ-IV.md.

## 7. Status Summary

- Substrates audited: 26 rows
- CONSUMED: 9 (rows 4, 5, 14, 15, 17, 18, 21, 25, 26 — note row 18 is partial-coverage)
- PARTIAL: 6 (rows 1, 2, 3, 6, 12, 15, 20)
- WIRED-NOT-CONSUMED: 5 (rows 7, 8, 9, 10, 19)
- DEAD: 3 (rows 22, 23, 24)
- UNDERUTILIZED: 4 (rows 11, 13, plus generator-empty Pratt for 5/9 grammars and ruler-alphabet single-Bool sources)
- Top 5 substrate gaps: ruler family unconsumed (row 7), `RuleSet` load eprint-only (row 8), `shape_dict_templates`/`shape_dict_selection` not read by any backend (rows 9, 10), `type_obligations` Vec un-drained (row 19), CSP authority re-override remaining in `alt_strategy.rs` (rows 1, 3)
