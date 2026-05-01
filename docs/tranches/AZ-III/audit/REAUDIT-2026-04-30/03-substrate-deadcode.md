# AZ-III REAUDIT Lane 3 — Substrate / Dead Code / Legacy Audit

**Date:** 2026-04-30
**Repo HEAD:** d5179b8a (master)
**Scope:** all `crates/*` plus sibling repos `parse-that`, `pprint`, `gorgeous`,
`bbnf-buddy`. Read-only; method = `rg` over the workspace.
**Posture:** ZERO tolerance. The user has stated repeatedly (memory:
`feedback_no_workarounds`, `feedback_no_backward_compat`, `feedback_no_orthogonal_codepaths`,
`feedback_substrate_with_consumer`) that **substrate without consumer**, **shim/bridge**,
**legacy alias**, and **orthogonal codepath** are all violations of equal weight.

The dominant signal: the AZ-II `tape::*` substrate was deleted, but its
**type-mirrors** (`bbnf_ir::dta::*`, IR-side `RegexPayloadKind` mirroring
`tape::PayloadKind`, `recognizer_plan` "unification bridge") survived the cull.
They are now substrate without consumer — the audit's primary finding.

---

## 1. Production hits of forbidden surfaces (AZ-III Hard Gate 3)

Hard Gate 3: "No production source or generated Rust exposes `Parsed<R>`,
`TapeDirect`, generated tape views, `ValueRoot`, `TapeOffset`, or a public
tape runtime."

**Production source hits — all forbidden tokens, scoped to non-test, non-generated:**

| token | live production hit? | evidence |
|---|---|---|
| `tape::` | **NO** (33 textual hits, all in `///` doc-comments and `//!` module heads) | `rg "tape::" crates/ \| rg -v '//\|/\*\|\* '` returns one entry: `crates/core/tests/struct_direct_snapshots.rs:46` (deny-list literal) |
| `TapeDirect` | NO | `rg "TapeDirect" crates/ --type rust` returns 0 |
| `Parsed<R>` | NO | `rg "Parsed<R>" crates/ --type rust` returns 0 |
| `ValueRoot` | NO (5 hits, all in tests/comments) | hit list confined to `crates/core/tests/{ay_w3b_value_api_smoke.rs:25, typed_accessor_surface.rs:20, runtime_root.rs:4, regen_shape_goldens.rs:71}` and one comment in `backend/rust/emitter/grammar.rs:113` |
| `TapeOffset` | NO (live constants/types — but only in test deny-strings + 2 doc-comments in `crates/core/src/backend/rust/trace.rs:7,42`) | `rg "TapeOffset" crates/ --type rust` returns 12 hits, all comments or test denylists |
| `json-prototype` | NO production refs | one `Cargo.toml` survives at `docs/tranches/AZ-II/archive/json-prototype/Cargo.toml` (correctly archived); zero workspace references |
| `crates/tape` package | NO | directory does not exist |

**Verdict: Hard Gate 3 holds at the symbol level.** However, two textual residue
classes still carry the substrate's archaeology and should be cleaned in W1:

1. **Doc-comment archaeology in `crates/ir/src/passes/{payload,profile,recognizers,csp_strategy/constraints}/*.rs`** —
   28 comments still narrate the IR-side type as the "mirror of" or "counterpart
   of" `tape::*`. The mirror's consumer is **gone**; the prose advertises a
   ghost. Per `feedback_no_metalanguage_docs` (memory) these doc-strings should
   be rewritten to describe their own purpose.
2. **`crates/core/src/backend/rust/trace.rs`** has TWO doc-comments claiming the
   rule function returns `Option<::tape::TapeOffset>`. The struct-direct flip
   changed that return; the comment is now lying about the runtime contract.

---

## 2. Dead-code allowance index

`#[allow(dead_code)]` and `#[allow(unused…)]` non-generated hits: **35**
(generated CSS L4 has 30+ more, all keyword-dispatch helpers — these are
correctly emitted and `#[allow]`'d because the cross-shape dispatcher only
calls a subset; do not flag generated files).

| location | verdict | rationale |
|---|---|---|
| `crates/core/src/css_types.rs:18` (`#![allow(dead_code)]` module-level) | **Wrong allow — file IS used; allow is over-broad** | `parse_hex_color` is called from generated `css_l4.rs`. The allow is module-wide; should narrow or remove. |
| `crates/core/src/runtime/css_l4/document.rs:451` (`#[allow(dead_code)] &'a CssArena<'p>`) | **Actually dead — the field is held but never read** | Marker for "we held the arena lifetime but the value carries no live read." Replace with `PhantomData<&'a CssArena<'p>>` to avoid the allow. |
| `crates/core/src/runtime/google_sheets/builder.rs:69` (`handle_token: u64` allow) | Actually used in debug-only `debug_assert` | Kept for ordering invariant; OK |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs:146` (per-fn `#[allow(dead_code)]`) | **Generated emission token — emitted because cross-shape dispatch calls a subset** | Codegen-correct |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs:374` (`pub fn make_alphabet`) | **Truly dead — public surface with zero workspace consumers** | `rg "make_alphabet" crates/` returns only the definition line. Delete. |
| `crates/ir/src/passes/recognizers/shape_dispatch/alt_dispatch.rs:125` (`#[allow(dead_code)] const _: fn(ShapeTag) -> bool = ShapeTag::is_classified;`) | **Static-assert pattern, verifies trait bound at compile** | Idiomatic; keep |
| `crates/core/src/generate/regex/phf.rs:148,151,160` (3 hits — generated/synthetic table guards) | OK; codegen-driven | Keep |
| `crates/core/src/generate/regex/byte_class.rs:129` (`#[allow(dead_code)]`) | OK; conditional shape | Keep |
| `crates/core/src/generate/regex/last_byte_set.rs:103` | OK; conditional | Keep |
| `crates/egraph/src/rewrite.rs:57` (`#[allow(unused_variables)]` on default trait fn `should_apply`) | OK; default impl | Keep |
| `crates/egraph/tests/csp_scheduler.rs:254` (`fn _force_imports`) | Actually dead — the underscore prefix advertises intent | Test-only; keep |
| `crates/core/benches/json/competitors.rs:{161,293,409}` (3 enum allows) | Generated competitor types via macro | Keep |
| `crates/core/examples/test_l4.rs:2` + `crates/core/tests/{14 sites with mod css_types}` | **Each test re-defines `mod css_types { pub fn parse_hex_color(...) ... }`** | **Duplicated 14× across tests despite a "single source of truth" claim in `crates/core/src/css_types.rs:15`.** See §5 — orthogonal codepath. |

**Allow-attribute count breakdown:**
- 35 non-generated `#[allow(dead_code)]` / `#[allow(unused…)]`
- ≈30 generated keyword-dispatch helpers in `crates/core/src/grammar/generated/css_l4.rs` (codegen-correct)

**True dead substrate (`#[allow(dead_code)]` is correct, code should be deleted):**

1. `crates/ir/src/passes/recognizers/pattern_alphabet.rs:374-385` — `make_alphabet` (pre-W3.5c API; no consumers)

---

## 3. Shim / legacy / compat surfaces

| file:line | symbol | nature | AZ-III owner |
|---|---|---|---|
| `crates/core/src/grammar/mod.rs:79` | `pub fn parse_with_state(source: &str)` | "retained for API compatibility with pre-AC call sites as a thin alias over `parse`; **it will be audited and likely removed during AC.3**" | W1 (AC.3 was 4 tranches ago — this is overdue) |
| `crates/core/src/grammar/bootstrap_parser.rs` (1505 LOC) | hand-written BBNF parser | Cutover.G bypass for the chicken-and-egg between codegen and self-host | **W2** — explicit AZ-III Hard Gate 5 blocker |
| `crates/analysis/src/directives/pretty.rs` (6 LOC) | `pub use crate::state::pretty::*;` | "Re-exports the original `state::pretty` logic for backward compatibility" — **ZERO consumers** for the directives::pretty path | W1 — delete the file |
| `crates/core/src/backend/recognizer_plan.rs` (159 LOC) | `ScannerPlanRecord`, `scanner_plan_for`, `plan_for_id`, `RecognizerFamily`, `EmitHint` | "downstream consumer count is currently zero, but the struct is small enough that the cost of keeping the bridge alive is negligible" — **explicit no-consumer self-confession** (X.8f bridge) | W3 |
| `crates/core/src/backend/rust/ir_types.rs:278-320` | "Prettify compatibility stubs" — `scratch_index_for_elem`, `scratch_accessor`, `collect_accessor`, `recovered_static_ident`, `recover_sentinel` | Comment claims "the prettify emitter still calls" them; **`rg` shows ZERO call sites** anywhere in the workspace | W3 |
| `crates/core/src/backend/rust/trace.rs` (54 LOC) | `emit_depth_counter`, `emit_trace_entry`, `emit_trace_exit` (gated `#[cfg(feature = "parser-trace")]`) | **Feature-flag corpse:** `parser-trace` is referenced 4× in source but **declared in NO `Cargo.toml`** — gate is unreachable, two of three functions have no callers, and the doc-comments still describe `Option<::tape::TapeOffset>` returns | W1 — delete |
| `crates/core/src/generate/regex/emit/mod.rs:71` | `is_fused_number_regex` | "This shim exists for call sites where the caller only has the pattern string; it pays a full HIR parse" — duplicate of `is_fused_number_regex_cached` | W3 — collapse to one |
| `crates/core/src/generate/regex/emit/simd.rs:264-274` | `emit_negated_scan_plus`, `emit_negated_scan_star` | "wrapper for backwards call-site compatibility" — both routes through `emit_negated_scan(.., quantifier)` with 4 callers in one file | W3 — inline the dispatch |
| `crates/ir/src/vm/mod.rs:5` | `vm` directory + `pub use vm::*` re-exports in `crates/ir/src/lib.rs:39-42` | "All public types are re-exported from the crate root **for backward compatibility**" — every consumer reaches via `bbnf_ir::interpreter::*` (the re-export), not `vm::interpreter::*` | W3 — drop one of (`vm::`, `pub use`) — they ARE the same surface |
| `crates/ir/src/passes/recognizers/mod.rs:229,237,252` | `legacy_annotations: HashMap` | local literal named `legacy_annotations`; comment "Phase 1: legacy per-rule annotations" — fed into `ir.pattern_annotations` | W3 — rename or merge with Phase 2 facts |
| `crates/core/tests/common/css_normalize.rs:201,1117` | `legacy_media_range` | normalization step in CSS test harness; consumer-only | test-only; W4 |
| `crates/core/src/grammar/mod.rs:60-69` | comment trail "**deferred follow-up**", "cutover.H Phase 1 retains the bootstrap parser as the canonical entry point" | matches Hard Gate 5 | W2 |
| `crates/ir/src/dta/mod.rs` + `crates/ir/src/dta/types.rs` | `DtaStateId`, `DtaRuleId`, `DtaAssociativity`, `DtaPrecedenceEntry` | "Hoisting them out of `tape::dta` retires the workaround crate dependency edge" — but **the tape crate is gone** and `rg "use bbnf_ir::dta"` returns ZERO hits. The hoist outlived its consumer. | **W1 — delete entire `dta/` subdirectory** |
| `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:134` | `pub fn install(...) -> usize { 0 }` | "Returns the number of constraints installed (always zero today)" — no-op constraint hook | W3 — explicit AZ-III Carried Work Ledger item ("CSP `shape_dict` no-op / under-consumed") |
| `crates/core/src/runtime/view.rs:27-35` | `pub use crate::backend::rust::view::color::{COLOR_PAYLOAD_BYTES, Color, ColorSpace};` | "Backwards re-export of `crate::backend::rust::view::color`" | W5 — collapse to single canonical path |
| `crates/core/src/lib.rs:13-18` + `crates/core/tests/common/css_types.rs` + 14 in-test `mod css_types {...}` | host-shim duplication (see §5) | "Single source of truth (per feedback_no_workarounds): one host shim, one resolution path, no test-side duplicate copies" — claim falsified by 14 in-tree duplicates | W1 |

**Pattern: 16 distinct shim/compat surfaces.** Most carry self-documentation
admitting they are dead-or-near-dead. Only `bootstrap_parser.rs` has the AZ-III
spec already sequencing its removal (Hard Gate 5 / W2).

---

## 4. God modules (>500 LOC mod.rs/utils.rs/common.rs/helpers.rs)

`utils.rs`/`common.rs`/`helpers.rs`/`misc.rs` search returns **zero results in
`crates/`**. Sibling `parse-that` has `parse_that/src/utils.rs` at 20 LOC — not
a god module. Good discipline at the file-name level.

`mod.rs` >500 LOC:

| file | LOC | verdict |
|---|---:|---|
| `crates/ir/src/passes/csp_strategy/mod.rs` | **1278** | **God module.** Hosts `AltMode` / `WrapMode` / `RegexEngine` decision-domain enums, `StrategyDomain`, `partition_by_call_graph` orchestration, `solve_per_component`, fallback strategy heuristics, AND constraint-installer dispatch. Should split into `decision_domain.rs`, `solve.rs`, `fallback.rs`. |
| `crates/ir/src/passes/types/mod.rs` | 641 | Borderline. Type inference orchestration + structural-type rebuild + per-node `compute_structural_types_for_node` recursion. Consider splitting "structural types" into its own file. |
| `crates/core/src/lower/expression/mod.rs` | 529 | Borderline. Concentrated lowering switchboard; arguably appropriate for an entry point. |
| `crates/core/src/generate/regex/emit/dfa/mod.rs` | 480 | Acceptable — DFA emission orchestrator with sub-modules. |
| `crates/lsp/src/dap/mod.rs` | 402 | Acceptable. |

**Conclusion:** one true god module (`csp_strategy/mod.rs`). All other large
`mod.rs` files concentrate orchestration appropriately.

---

## 5. Orthogonal codepaths (same logic, multiple sites)

### 5.1 `parse_hex_color` host shim — duplicated 16 times

Despite `crates/core/src/css_types.rs:15` declaring "Single source of truth (per
`feedback_no_workarounds`): **one** host shim, one resolution path, no test-side
duplicate copies", `rg "fn parse_hex_color"` returns **16 distinct definitions**,
each with the same body:

| site | role |
|---|---|
| `crates/core/src/css_types.rs:20` | claimed canonical |
| `crates/core/tests/common/css_types.rs` | test-common module |
| `crates/core/tests/lightningcss_parity.rs:52` | inline `mod css_types` |
| `crates/core/tests/typed_accessor_surface.rs:67` | inline `mod css_types` |
| `crates/core/tests/css_l4.rs:7` | inline |
| `crates/core/tests/css_l4_color_view.rs:34` | inline |
| `crates/core/tests/css_l4_comment_probe.rs:21` | inline |
| `crates/core/tests/css_l4_named_color_parity.rs:11` | inline |
| `crates/core/tests/css_l4_canonical_parity.rs:32` | inline |
| `crates/core/tests/css_l4_dimensions.rs` | inline |
| `crates/core/tests/css_l4_parity.rs:40` | inline |
| `crates/core/tests/named_type_preservation.rs:79` | inline (with comment "Duplicated from `typed_accessor_surface.rs` so this test compiles hermetically") |
| `crates/core/tests/ax_w0a2s_real_css_probe.rs:14` | inline |
| `crates/core/examples/test_l4.rs:4` | inline |
| `crates/core/benches/css/l4.rs:9` | inline |

The "hermetic compilation" rationale was the original justification (so each
test crate's generated parser could resolve `crate::css_types::*`), but per
`crates/core/src/css_types.rs:6-13` the codegen now emits at the workspace
library root — every test could reach `bbnf::css_types::parse_hex_color`. The
duplication is now **purely vestigial**.

**Canonical:** `crates/core/src/css_types.rs:20`. **Delete:** all 15 in-test copies.

### 5.2 IR-side `RegexPayloadKind` mirror of deleted `tape::PayloadKind`

`crates/ir/src/passes/recognizers/dta.rs:148-181` defines `enum
RegexPayloadKind` with the doc-string "AW-III.W1 — IR-side mirror of
`tape::PayloadKind` for the…" The tape crate is gone. The mirror is now a
freestanding type with no upstream — but it is genuinely consumed by codegen
(`dta.rs:1211, 1239, 1246, 1341` and many emitter sites). The `tape::*` mirror
*claim* is stale; the type itself is alive. Action: rewrite docstring to
describe its current authoritative role; do not delete.

### 5.3 `compile_grammar` / `compile_ast` (VM-target) vs `compile_grammar_request` / `compile_ast_request` (typed)

`crates/core/src/pipeline/compile.rs:78-110, 125-148` carries two parallel
function pairs. The VM-targeted versions are thin shims that Just dispatch to
the typed versions and stringify the error. With ~28 production consumers
(benches, tests, lsp), the VM wrapper is still alive. **Verdict:** keep — but
note the duplication is doubled error type (typed vs `String`); a future tranche
should pick one.

### 5.4 `is_fused_number_regex` vs `is_fused_number_regex_cached`

`crates/core/src/generate/regex/emit/mod.rs:71-99` defines both. Cached is the
fast path; uncached is a "shim" for callers without IR access. Should be one
function with an `Option<&IR>` parameter. AZ-III W3 owner.

### 5.5 `emit_negated_scan_plus` / `emit_negated_scan_star` wrappers

`crates/core/src/generate/regex/emit/simd.rs:266-274` — two one-liner wrappers
over `emit_negated_scan(targets, ScanQuantifier::Plus|Star)` for "backwards
call-site compatibility". Four total callers (`emit/mod.rs:185-186, 328-330`).
Inline the quantifier at the four call sites.

### 5.6 `vm::*` modules vs `pub use vm::*` at crate root

`crates/ir/src/lib.rs:39-42` re-exports `bytecode`, `compiler`, `debug`,
`interpreter` from `vm::`. Real consumers all use the re-exported path
(`bbnf_ir::interpreter::*`). The `vm` directory exists ONLY to satisfy
`pub use` and to host the comment "for backward compatibility". Either:
- delete `vm/mod.rs` and hoist the four sub-modules to `bbnf_ir::*` directly;
- or remove the re-exports and force consumers through `vm::interpreter::*`.

Status quo is the worst of both worlds.

### 5.7 `silent BoxedEnum` fallback (AZ-III Hard Gate 7 violation)

`crates/ir/src/passes/types/constraint/reference.rs:74` and
`crates/ir/src/passes/types/constraint/revise.rs:123` both hit
`TypeDesc::BoxedEnum` as a silent compound-type fallback for unsolved-or-
heterogeneous Refs/Alts. Per AZ-III Hard Gate 7 ("**No silent fallback**.
Unsolved cycles, heterogeneous alternations, unsupported StructDirect variants,
and bootstrap bridges must produce a named error or a same-wave grammar-general
implementation"), these must surface as errors. **W3 — Carried Work Ledger
already names this**.

### 5.8 `format_value` / `format_ir` in gorgeous

`crates/gorgeous/src/vm.rs:29` defines `format_ir` as a "legacy alias" for
`format_value`. The only caller is `gorgeous/tests/vm.rs:294-297`, which exists
to assert the alias produces identical output. Test-only consumer for an alias
that should be deleted; W4.

---

## 6. Under-consumed substrate (no production consumer or test gate)

| substrate | LOC | consumers | verdict |
|---|---:|---|---|
| `crates/ir/src/dta/{mod.rs, types.rs}` | 17 + 73 = 90 | **ZERO** (`rg "use bbnf_ir::dta"` empty; types appear only in test deny-strings) | **DELETE — top deletion target** |
| `crates/core/src/backend/recognizer_plan.rs` | 159 | ZERO non-self consumers (only re-exported by `backend/mod.rs`) | DELETE |
| `crates/core/src/backend/rust/trace.rs` | 54 | only `emit_depth_counter` is called once (`emitter/grammar.rs:201`); `emit_trace_entry` and `emit_trace_exit` have **zero call sites** AND the `parser-trace` feature is undeclared | **DELETE entire file** |
| `crates/core/src/backend/rust/ir_types.rs:278-332` (prettify stubs region — ~55 LOC) | 55 | ZERO direct callers anywhere (`scratch_*`, `collect_*`, `recovered_static_ident`) | DELETE block |
| `crates/analysis/src/directives/pretty.rs` | 6 | ZERO consumers (`directives::pretty` not imported anywhere) | DELETE file |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs:374-385` (`make_alphabet`) | 12 | ZERO consumers, marked `#[allow(dead_code)]` | DELETE function |
| `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:134-136` (`install` no-op) | 3 | one call site that ignores its return value | DELETE function (and its unused `ConstraintCtx` import gate) |
| `crates/gorgeous/src/vm.rs:28-36` (`format_ir`) | 9 | one test asserting it equals `format_value` | DELETE function (and its single test) |

**Total estimated deletable LOC across these eight items: ~388 LOC**, plus the
1505 LOC of `bootstrap_parser.rs` once Hard Gate 5 closes — **~1893 LOC of dead
substrate** identified by this lane.

---

## 7. Sibling-repo posture

| repo | published? | consumed by bbnf-lang? | role | dead/legacy? |
|---|---|---|---|---|
| `parse_that` (crates.io 0.4) | yes | yes — `crates/core/Cargo.toml:15`, `crates/ir/Cargo.toml:13` (and competitor bench at `core:85`) | parser-combinator runtime + regex HIR | clean: 0 `TODO/FIXME/HACK/workaround/legacy/stub` markers in `src/`. `utils.rs` is a 20-LOC bounds extractor — not a god module. |
| `pprint` (crates.io 0.3.6) | yes | yes — `crates/core/Cargo.toml:14, 84`, `crates/analysis/Cargo.toml:12` | pretty-printer | clean: 0 markers in `src/`. `target/package/pprint-0.3.{0..6}` directories are CARGO BUILD ARTIFACTS, not source duplication. |
| `gorgeous` | published-but-empty as a sibling repo (`/Users/mkbabb/Programming/gorgeous` is empty) | yes — but lives **inside `bbnf-lang/crates/gorgeous`**, not as a sibling | grammar-driven pretty printer | one legacy alias (`format_ir` — see §5.8). `crates/gorgeous/src/vm.rs` 324 LOC; tests well-scoped. |
| `bbnf-buddy` | private (`"private": true`) | NOT consumed by bbnf-lang Rust workspace; pure Vue/SVG project | mascot UI | irrelevant to AZ-III scope; clean. |
| `atree` | python-only repo (`atree/{btree.py, rtree.py, utils}`) | NOT consumed by bbnf-lang | unrelated python repo | n/a |

**Sibling integrity is strong.** No cross-repo dead substrate; no published
sibling crate orphaned from the consumer set; no path-dependency rot. The
single in-repo "gorgeous-as-sibling" naming confusion is harmless because the
empty `/Users/mkbabb/Programming/gorgeous` directory has no Cargo manifest —
it can be safely deleted.

---

## 8. Top-30 deletion targets (ranked by violation severity × LOC)

Severity legend: **CRIT** = AZ-III Hard Gate violation; **HIGH** = explicit
"feature_no_workarounds" violation; **MED** = orthogonal codepath / shim;
**LOW** = doc-comment archaeology only.

| # | severity | file:line | LOC est | wave |
|---:|---|---|---:|---|
| 1 | CRIT | `crates/core/src/grammar/bootstrap_parser.rs` | 1505 | W2 (Hard Gate 5) |
| 2 | CRIT | `crates/ir/src/passes/types/constraint/reference.rs:74` (silent BoxedEnum on unsolved compound Ref) | 1 | W3 (Hard Gate 7) |
| 3 | CRIT | `crates/ir/src/passes/types/constraint/revise.rs:123` (silent BoxedEnum on heterogeneous Alt join) | 1 | W3 (Hard Gate 7) |
| 4 | HIGH | `crates/ir/src/dta/{mod.rs,types.rs}` (entire DTA module — zero consumers) | 90 | W1 |
| 5 | HIGH | `crates/core/src/backend/recognizer_plan.rs` (entire file, "consumer count is currently zero") | 159 | W3 |
| 6 | HIGH | `crates/core/src/backend/rust/trace.rs` (entire file — phantom feature, dead callers) | 54 | W1 |
| 7 | HIGH | `crates/core/src/backend/rust/ir_types.rs:278-320` (prettify compatibility stubs) | 43 | W3 |
| 8 | HIGH | `crates/core/src/grammar/mod.rs:71-81` (`parse_with_state` "likely removed during AC.3") | 11 | W1 |
| 9 | HIGH | `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs:134-136` (no-op `install`) + Carried-Work-Ledger callout for `shape_dict_selection` consumer truth | 3 (+ design follow-up) | W3 |
| 10 | HIGH | `crates/analysis/src/directives/pretty.rs` (back-compat re-export, zero consumers) | 6 | W1 |
| 11 | MED | 14 in-test `mod css_types {…}` duplications (sites enumerated in §5.1) | ~150 cumulatively | W1 |
| 12 | MED | `crates/core/src/runtime/view.rs:27-35` ("Backwards re-export of `crate::backend::rust::view::color`") | 9 | W5 |
| 13 | MED | `crates/core/src/generate/regex/emit/simd.rs:264-274` (`emit_negated_scan_{plus,star}` wrappers) | 11 | W3 |
| 14 | MED | `crates/core/src/generate/regex/emit/mod.rs:71-86` (`is_fused_number_regex` shim — collapse with cached) | 16 | W3 |
| 15 | MED | `crates/ir/src/passes/recognizers/pattern_alphabet.rs:374-385` (`make_alphabet`) | 12 | W3 |
| 16 | MED | `crates/gorgeous/src/vm.rs:28-36` (`format_ir` legacy alias) + asserting test | 9 (+ test trim) | W4 |
| 17 | MED | `crates/ir/src/vm/mod.rs` + `crates/ir/src/lib.rs:39-42` (back-compat re-export pattern; pick one canonical path) | refactor | W3 |
| 18 | MED | `crates/ir/src/passes/recognizers/mod.rs:229-252` ("legacy_annotations") — rename Phase 1 substrate, fold into Phase 2 facts surface | refactor | W3 |
| 19 | MED | `crates/ir/src/passes/csp_strategy/mod.rs` god-module (1278 LOC) — split into `decision_domain.rs`, `solve.rs`, `fallback.rs` | refactor | W3 |
| 20 | MED | `crates/core/src/runtime/css_l4/document.rs:451` (`#[allow(dead_code)] &'a CssArena<'p>` — replace with `PhantomData`) | 1 | W3 |
| 21 | LOW | 28 doc-comments in `crates/ir/src/passes/{payload,profile,recognizers,csp_strategy/constraints}/*.rs` referencing `tape::*` mirrors | rewrite | W1 |
| 22 | LOW | `crates/core/src/backend/rust/trace.rs:7,42` (lying doc-strings about return type) | 2 | W1 (with #6 deletion) |
| 23 | LOW | `crates/core/tests/common/css_normalize.rs:1117` (`legacy_media_range`) | rename or comment | W4 |
| 24 | LOW | `crates/core/src/css_types.rs:18` (`#![allow(dead_code)]` module-wide) — narrow or remove | 1 | W3 |
| 25 | LOW | 4× `Pre-W2-act this file walked` test prologues (`sheets_parity.rs:24`, `sheets_self_parity.rs:31`, `json_decode.rs:10`, `json_parity.rs:13`) — rewrite to describe current behavior | rewrite | W4 |
| 26 | LOW | `crates/core/src/lib.rs:13-18` host-shim docstring (claims "no test-side duplicate copies" — falsified, see §5.1) | rewrite after #11 | W1 |
| 27 | LOW | `crates/ir/src/passes/csp_strategy/mod.rs:184-188` ("`peer_group` retained for future use") + symmetric `Y.2`/`Z.5` ghost-variant headstones — verify deletion is final, drop the comments | rewrite | W3 |
| 28 | LOW | `crates/core/tests/payload_layouts.rs:181` ("These tests are #[ignore]d until Phase 2 lands") — close or delete the ignore | gate | W4 |
| 29 | LOW | empty `/Users/mkbabb/Programming/gorgeous` sibling directory | rmdir | n/a |
| 30 | LOW | `feature = "parser-trace"` declaration: declare in `crates/core/Cargo.toml` AND wire a real consumer, or delete the gate | choose-one | W1 (with #6) |

---

## Top-5 most egregious findings

1. **`crates/ir/src/dta/` module — entire DTA type group is orphaned substrate (~90 LOC, zero consumers).**
   The hoist's stated rationale was "tape now consumes the IR's authoritative
   type via `bbnf_ir::dta::*`". The tape crate has been deleted (per AZ-III
   Hard Gate 2). `rg "use bbnf_ir::dta"` returns zero hits; the four types
   appear ONLY in test deny-strings (asserting they are NOT in generated code).
   This is the textbook "substrate without consumer" violation.

2. **`crates/core/src/grammar/bootstrap_parser.rs` — 1505-LOC hand-written BBNF
   parser still routed at the canonical `parse` entry.**
   The codegen self-host is "deferred" (Hard Gate 5 explicitly names it as a
   blocker). Largest single legacy file in the workspace, and `parse_with_state`
   is its façade.

3. **Silent `BoxedEnum` fallbacks at `reference.rs:74` and `revise.rs:123`.**
   AZ-III Invariant 7 forbids silent fallbacks. Both sites currently swallow
   compound-type Refs and heterogeneous Alt joins respectively, returning
   `BoxedEnum` with no diagnostic. The Carried-Work-Ledger names this; the
   audit confirms two source lines remain to fix.

4. **`crates/core/src/backend/recognizer_plan.rs` — 159 LOC, "consumer count
   currently zero" by its own admission.**
   The X.8f "unification bridge" never grew the unifying consumer; the doc-
   string actually argues for keeping it because "the cost of keeping the
   bridge alive is negligible." That argument is exactly the violation
   `feedback_no_workarounds` forbids: cost-of-keep is not a justification for
   substrate without consumer.

5. **`parse_hex_color` host shim duplicated 16 times across the test crate
   despite a "single source of truth" claim in `crates/core/src/css_types.rs`.**
   The claim was based on a plausible-but-falsifiable architectural fact (the
   xtask emits at the lib root); each test that bothered to verify simply
   pasted the function inline rather than importing. The codebase silently
   carries 15 stale copies of a 30-line function. This is the highest-volume
   orthogonal-codepath violation in the audit.

---

## Notes for the redress wave

- Lane 1 owns cargo runs; this lane is read-only.
- Items 1, 6, 8, 10 in §8 are independent file/region deletions safe to land
  in a single W1 commit — none of them have production consumers per the
  workspace `rg`.
- Items 2, 3 are 2-line surgical edits — emit a typed error (`UnresolvedCompoundRef`,
  `HeterogeneousAltJoin`) instead of `TypeDesc::BoxedEnum`.
- Item 5 (`recognizer_plan.rs`) requires a consumer audit because the file's
  re-export surface in `crates/core/src/backend/mod.rs:27-29` is public — any
  consumer that imports the names through `bbnf::backend::ScannerPlanRecord`
  will surface as a build error if removed. None found in this lane's pass,
  but a worktree cargo check is the proof step.
- Item 11 (16× `parse_hex_color` duplication) lands as a single mechanical
  edit per test — replace `mod css_types { ... }` with `use bbnf::css_types;`.
