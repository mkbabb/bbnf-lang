# BA-Restart Mechanical Census — 2026-05-03

Mechanical kill-list input for the BA-restart. Targets rot, dupes, dead code,
grammar-specific code in supposedly-generic crates, dynamic typing abuse,
god modules, and miscellaneous archaeology. Complements the HARDENING audit
synthesis (2026-05-03) by enumerating everything-that-is-not-first-principles-
correct, with paths and FATE.

FATE legend:
- `DELETE` — rip out wholesale; no successor needed.
- `MERGE-WITH-X` — fold into the listed sibling; eliminate the duplicate.
- `MOVE-TO-X` — relocate; the symbol survives but lives elsewhere.
- `FAIL-EXPLICIT` — codegen/runtime must panic on the unhandled path
  rather than silently fall through.
- `KEEP` — first-principles correct; no action.
- `DEFER-WITH-RATIONALE` — out of BA-restart scope; reason inline.

---

## 1. Tape archaeology

The tape substrate severed across all eight non-bootstrap grammars
(JSON, Sheets, CSS L4 in W2-act; BBNF, CSV, EBNF, BNF, Math,
CSS Pretty in cutover.A/E). Production code for tape execution is
gone. Residue is exclusively comment/doc strings + one live function
in `generate/serialize/`.

### 1.1 Live tape functions in src/

| Site | Symbol | FATE |
|---|---|---|
| `crates/core/src/generate/serialize/serialize.rs:35` | `pub fn rule_pushes_tape_record` | DELETE — confirmed dead. Verified zero external callers via `grep -rn 'generate_serialize_methods\|generate_dispatch_arms'` — the two `pub fn`s in `generate/serialize/` are only consumed by themselves. The whole directory is dead. |
| `crates/core/src/generate/serialize/serialize.rs:24` | `if rule.meta.is_transparent || !rule_pushes_tape_record(ir, rule)` | DELETE — internal call. |
| `crates/core/src/generate/serialize/mod.rs:15, 27, 45, 72` | `pub fn generate_serialize_methods` + internal calls | DELETE entire `generate/serialize/` directory (156 LOC). |
| `crates/core/src/generate/mod.rs` | likely `pub mod serialize` | DELETE the export. |
| `crates/core/src/lib.rs:26` | `pub use generate::*` re-exports `serialize::*` | becomes mostly empty after the directory is deleted. |

### 1.2 Comment/doc tape residue (DELETE wholesale during regen)

`crates/core/src/runtime/{bbnf,bnf,csv,css_l4,css_pretty,ebnf,
google_sheets,json,math}/mod.rs` each open with a doc comment whose
purpose is to advertise the absence of `TapeBuilder / TapeRec /
TapeCursor` — the substrate-departure assertion is now rote. After
BA-restart no module will ever have carried a `Tape*` symbol.

| Path:line | Context (3-line) | FATE |
|---|---|---|
| `crates/core/src/runtime/google_sheets/mod.rs:7-11` | "...the tape substrate is severed... no `TapeBuilder` / `TapeRec` / `TapeCursor` symbol appears" | DELETE — substrate-departure boilerplate. |
| `crates/core/src/runtime/css_l4/mod.rs:7-9` | same boilerplate | DELETE |
| `crates/core/src/runtime/bbnf/mod.rs:9-17` | same boilerplate | DELETE |
| `crates/core/src/runtime/json/mod.rs:6-8` | same boilerplate | DELETE |
| `crates/core/src/runtime/csv/mod.rs:9-17` | same boilerplate + "...regens CSV onto the struct-direct path and then deletes the tape crate..." | DELETE — references defunct followup. |
| `crates/core/src/runtime/google_sheets/document/mod.rs:143-145` | "cursor-backed `tape::TapeCursor`; that emitter retired alongside the tape substrate" | DELETE |
| `crates/core/src/runtime/google_sheets/document/canonical.rs:14-15` | same | DELETE |
| `crates/core/src/runtime/css_l4/document.rs:168-169` | "...etc.) reach the document graph — the post-tape equivalent of the pre-W2-act tape-walk parity surface" | DELETE — meta-language. |
| `crates/core/src/runtime/json/document.rs:14, 132` | "...older tape projection built a `<Grammar>Value` enum..." | DELETE — meta-language. |
| `crates/core/src/runtime/json/document.rs:316` | "Mirrors `tape::TapeKind` for the cursor-backed surface; the..." | DELETE |
| `crates/core/src/runtime/bbnf/document.rs:315` | "...the implicit `Repeat` shape that tape walkers produced. Under..." | DELETE |
| `crates/core/src/path/ascent.rs:61` | "callback that traverses the tape, while the W2.5 bench harness" | DELETE — incorrect after substrate severing. |
| `crates/core/src/path/variant_select.rs:21` | "The resolver is layout-only — it does not consult the runtime tape" | DELETE |
| `crates/core/src/types.rs:90` | "lowering pipeline now walks the bootstrap tape directly into" | DELETE — no longer accurate (struct-direct everywhere). |
| `crates/core/src/grammar/mod.rs:3, 7, 17` | "tape-first bootstrap parser... `host` — tape walkers..." | DELETE — bootstrap is also struct-direct. |
| `crates/core/src/pipeline/compile/mod.rs:62` | "Tape-direct ingress: walk the bootstrap tape straight into" | DELETE |
| `crates/core/src/pipeline/compile/pipeline.rs:163` | "...payload layouts, grammar roundtrips, tape" | DELETE |
| `crates/core/src/runtime/builder.rs:4, 7, 48` | "...emitters target on grammars that have severed the tape substrate... selection between tape and struct happens at codegen time... functions on grammars whose tape has been severed." | REWRITE — drop "tape" mention; the substrate-selection branch is now monocular. |
| `crates/core/src/graph/deps.rs:14` | "tape walker's `variant_idx = 0` collided with the leaf-shape" | DELETE |
| `crates/core/src/grammar/generated/json.rs:1154`, `css_l4.rs:16208, 27788` | "for compositional uniformity with the tape-path" | regenerated by xtask — fix in emitter source string |
| `crates/core/src/grammar/schema/build.rs:26` | "per-rule `<Rule>View` family under the tape-first AC.2" | DELETE — schema is grammar-substrate-agnostic; mention is incorrect. |
| `crates/core/src/grammar/schema/model.rs:20` | "the tape-first AC.2 emitter it's the root grammar marker" | DELETE |
| `crates/core/src/grammar/schema/emit/rust/shared.rs:3, 17` | "Post-Tranche AC.2 rewrite: schema helpers emit impls on tape-backed... Rule records carry `variant_idx` on their tape record" | DELETE — schema does not target the tape. |
| `crates/core/src/backend/driver/alt.rs:11, 32, 36, 87` | "Under tape-first emission... AM.3 per-branch tape surgery..." | DELETE — pre-substrate description. |
| `crates/core/src/backend/driver/seq.rs:11` | "Under tape-first emission..." | DELETE |
| `crates/core/src/backend/driver/analysis.rs:168` | "specific `Tape<R>::with_capacity` divisor" | DELETE |
| `crates/core/src/backend/types/mod.rs:88-89` | "AM.3 per-branch tape surgery: `true` when the branch's codegen pushes child tape records" | DELETE |
| `crates/core/src/backend/rust/ir_types.rs:125, 138` | "...prettify emitter; tape-first rule emission... Tranche AC.2: under tape-first, the legacy `<Grammar>Enum`..." | DELETE |
| `crates/core/src/backend/rust/emitter/grammar.rs:113, 135, 288, 294-295, 384` | tape-related historical notes including `TapeVisitor` reference | DELETE |
| `crates/core/src/backend/rust/emitter/profile.rs` | (per grep) tape mention | DELETE |
| `crates/core/src/backend/rust/emitter_types.rs` | tape mention | DELETE |
| `crates/core/src/backend/rust/ir_enums.rs` | tape mention | DELETE |
| `crates/core/src/backend/rust/emitter/shapes/number.rs:12, 17, 179` | "pushes a `TapeKind::Span` leaf carrying...", "...leaf carries `TapeRec::PAYLOAD_F64_DIRECT_BIT`...", "tape column write, no `TapeKind::Span` tag (the typed..." | DELETE — emit code already doesn't reference tape symbols; comments lie. |
| `crates/core/src/backend/rust/emitter/shapes/arglist.rs`, `alt_dispatch/mod.rs`, `object.rs`, `flat/mod.rs:24` | tape mentions in shape emitters | DELETE |
| `crates/core/src/runtime/bbnf/view.rs:28-33` | "tape-direct `child(i)` accessor", "discriminator (replaces the tape-era `variant_idx`)" | DELETE |
| `crates/core/src/runtime/bbnf/serialize.rs` | tape mention | DELETE |

### 1.3 Tape residue in tests/

Tests reference the absent tape substrate either as historical
exposition or as gates that no longer mean anything. ~50 hits across
`crates/core/tests/`. The pattern is uniform: doc comments saying
"pre-W2-act this walked the tape; post-flip the tape substrate is
severed". FATE: scrub during BA-restart's test triage. Each test that
asserts on tape-record counts (`wrap_compound_elision.rs:13-15, 62,
94-95`, `sheets_parity.rs:78-122`, etc.) should either be
re-expressed against struct-tree node counts or DELETED outright.

Strict negative-assertion tests that should survive (KEEP):

| `crates/core/tests/struct_direct_snapshots.rs:45-53` | asserts `runtime::tape`, `::tape::`, `TapeOffset`, `TapeCursor`, `TapeRec`, `tape.push`, `tape.` are absent from generated code — this is the tape-departure regression gate. KEEP. |
| `crates/core/tests/regen_shape_goldens.rs:69, 105` | similar negative gate. KEEP. |

---

## 2. Grammar-specific code in supposedly-generic crates

### 2.1 `crates/core/src/css_types.rs` — confirmed

| Site | Issue | FATE |
|---|---|---|
| Whole file (66 LOC, `pub fn parse_hex_color`) | Lives in the bbnf library crate root. CSS L4 grammar references `crate::css_types::parse_hex_color` from its `hex` rule's map annotation. Per `feedback_no_workarounds`, "single source of truth: one host shim, one resolution path" — but the resolution path lives in a generic crate while the host fn is grammar-specific. | MOVE-TO-`crates/core/src/grammar/host/css_l4.rs` (or grammar-host crate sibling). The CSS L4 grammar's `crate::css_types::parse_hex_color` reference must rewrite to the grammar-host path, or the codegen must emit absolute paths into a per-grammar host module. |

### 2.2 `crates/ir/` — multiple per-grammar leaks

| Site | Issue | FATE |
|---|---|---|
| `crates/ir/src/registry/strategy.rs:130-185` | Hard-coded grammar idents table: `JsonParser`, `JsonGrammar`, `GoogleSheetsParser`, `CssL4Parser`, `BbnfBootstrap`, `BbnfParser`, `CsvParser`, `MathParser`, `BnfParser`, `EbnfParser`, `CssPrettyParser`, plus `rust_builder_path: "crate::runtime::css_l4::CssStructBuilder"`, `rust_document_path: "crate::runtime::css_l4::CssDocument"`. | MOVE-TO — grammar registry must read from `[workspace.metadata.bbnf.grammars]` (Cargo.toml) at xtask-regen time, NOT a hardcoded `static GRAMMARS` table compiled into `bbnf-ir`. The IR crate must not know any grammar's name. |
| `crates/ir/src/passes/audit/payload_coverage.rs:69-90` | `enum GrammarAuditTag { Json, CssL4, Sheets, Bbnf, Custom(&'static str) }` with named arms PER GRAMMAR. | MERGE-WITH `Custom(&'static str)` — drop the named arms; every grammar is `Custom` keyed by its identifier from the workspace metadata. |
| `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` (entire file, 240+ LOC) | "BBNF-specific shape template recognition" — explicitly named after one grammar, mining `big_comment` and `mapped_factor` patterns from `grammar/bbnf/bbnf.bbnf`. The IR test `crates/core/tests/shape_dict_bbnf.rs` consumes `BbnfShapeKind`, `compute_bbnf_shape_hash`, `mine_bbnf_shape_templates` directly via `bbnf_ir::passes::recognizers::shape_dict_bbnf::*`. | DELETE — generalise as a structural-shape miner with a `[recognizer]` config in the per-grammar TOML, OR move to a per-grammar recognizer crate at BA. The IR's recognizer pipeline must be data-driven. |
| `crates/ir/src/passes/profile.rs:26, 108` | `bbnf_shape_templates: Vec<BbnfShapeTemplate>` field on the universal `GrammarProfile` struct + `mine_bbnf_shape_templates` import. | DELETE the field; arrives via the generalised recogniser pipeline. |
| `crates/ir/src/types/grammar.rs:142, 489` | comments referencing "classify_regex survives only as the fall-through" + meta-language about CSS L4 perf | KEEP code, scrub comments. |
| `crates/ir/src/passes/csp_strategy/mod.rs:113-115, 359` | doc comments name CSS L4 as the perf-sensitive grammar. | KEEP — narrative. |
| `crates/ir/tests/structural_alphabet_extended.rs:292, 491, 918, 1020, 1022, 1212` etc. | `fn css_l4_fixture()`, `fn css_l4_stress_fixture()`, etc. — hand-rolled grammar fixtures for one named grammar. | DEFER-WITH-RATIONALE — tests are allowed to instantiate concrete grammars; tests are not generic crates. |
| `crates/ir/tests/payload_coverage_audit.rs:239, 312, 376-378, 478, 486` | `css_l4_fixture` + `GrammarAuditTag::CssL4` references in test fixtures. | DEFER — test surface; cleanup follows the IR-side fix in §2.2 row 2. |

### 2.3 `crates/analysis/`

The analysis crate is BBNF-grammar-specific by construction (it
implements LSP for BBNF source files). The cohort/spec analysis is
not "generic infrastructure" — it's the BBNF grammar's analysis.
Hits flagged below are neutral; they describe BBNF's own surface.

| Site | Issue | FATE |
|---|---|---|
| `crates/analysis/src/features/formatting.rs:6-8` | `use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView}` | KEEP — analysis crate is the BBNF grammar's LSP. |
| `crates/analysis/src/features/selection_range.rs:6-7` | similar | KEEP |
| `crates/analysis/src/directives/hints.rs` | hard-coded `@pretty` hint catalog (BBNF-specific directive). | KEEP |

The analysis crate ARGUABLY belongs under a `bbnf-grammar` family
alongside `bbnf-bootstrap`. Naming-wise, `bbnf-analysis` is fine; but
when BB renders BBNF's runtime surface, the formatting analysis should
move to its grammar-host module. DEFER-WITH-RATIONALE: cohort move is
out of BA-restart kill-list scope.

### 2.4 `crates/bbnf-path/` and `crates/bbnf-path-ts/`

| Site | Issue | FATE |
|---|---|---|
| `crates/bbnf-path/src/registry.rs:132-135` | `match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }` | MERGE-WITH the workspace metadata table. The macro must consume a `RegistryDescriptor` per grammar (this is already noted in the lib.rs doc comment as a W2.5/W4 followup) — BA-restart must finish that closure. |
| `crates/bbnf-path-ts/src/fixture.rs` (248 LOC) | "Per-grammar fixture registry" — synthetic fixture mirror of the proc-macro's. Doc says "When T4 lands per-grammar `pub const REGISTRY: StructRegistry`, both frontends swap their fixture lookup for the production const". | DELETE — finish the T4 closure during BA-restart. |
| `crates/bbnf-path-ts/src/compile.rs` (474 LOC) | Mirror of `bbnf-path/src/path_macro.rs` (639 LOC) — proc-macro and cdylib cannot share path-deps so the macro's lex/lower/validate logic is mirrored verbatim. | MERGE-WITH-`bbnf-path` — the validate/lower logic must move to a non-proc-macro crate (`bbnf-path-core`?) that BOTH the proc-macro and the cdylib path-dep, eliminating the mirror. |

### 2.5 `crates/gorgeous/src/`

Gorgeous is a per-grammar prettify CLI; its per-grammar files are
~10-15 LOC each (`json.rs`, `bnf.rs`, `bbnf.rs`, etc.) and do nothing
but `pub use` the generated marker + a `prettify_<g>` wrapper. The
match in `builtin.rs:9-22` is data — but DRY says the data should
live in workspace metadata, not in source.

| Site | FATE |
|---|---|
| `crates/gorgeous/src/{json,bbnf,bnf,ebnf,css,google_sheets}.rs` | DELETE — replace with one generic `prettify_grammar(grammar_ident, input, config)` function whose dispatch is metadata-driven. |
| `crates/gorgeous/src/builtin.rs:9-22` | DELETE — match on grammar names; replace with metadata lookup. |

### 2.6 `crates/simd-scan/` — PASS

No grammar identifiers found; no hits.

---

## 3. Legacy / deprecated / workaround markers

Aggregate counts (src/ only, excluding generated/ and tests/):

| Marker | Hits |
|---|---|
| `fall through` / `fallthrough` / `fall-through` | ~60 |
| `legacy` (in src/) | ~12 |
| `shim` (in src/) | ~8 |
| `fallback` (in src/) | ~70 (mostly KEEP — alt-dispatch fallback indices, regex fallthrough scanners — these are legitimate compiler decision-tree fallbacks) |
| `for now` / `temporary` | ~3 |
| `TODO` / `FIXME` / `HACK` (in src/, non-generated) | ~5 |

### 3.1 Excise candidates (legitimate kill-list)

| Path:line | Context | FATE |
|---|---|---|
| `crates/core/src/grammar/host.rs:387` | "keyword-strip wildcard fallback (`text.strip_prefix("@debug")`)" | INVESTIGATE — the wildcard catch-all violates `feedback_no-silent-epsilon` if it silently swallows unrecognised keywords. FAIL-EXPLICIT or DELETE. |
| `crates/core/src/grammar/host.rs:558` | "dedicated child compound (legacy bootstrap_parser shape)" | DELETE comment + verify the post-bootstrap shape is the only path. |
| `crates/core/src/runtime/google_sheets/value.rs:76, 92` | "grammar's TODO note", "AU.6.7 TODO note; today the leaf carries the borrowed span" | RESOLVE during BA-restart — close the TODO or excise. |
| `crates/core/src/runtime/css_l4/builder.rs:306` | "feedback_no-workarounds." (cited as gate) | KEEP — anti-workaround citation. |
| `crates/core/src/runtime/css_l4/builder.rs:713` | "without a parsed unit fall through to unitless." | INVESTIGATE — silent unit fallback may mask grammar bugs; FAIL-EXPLICIT. |
| `crates/core/src/runtime/css_l4/builder.rs:992` | "Wrap-frame fall-through is structural" | KEEP — structural Wrap dispatch. |
| `crates/core/src/runtime/google_sheets/arena.rs:38, 40, 103, 153` | repeated "fallback" comments on default arena arms | INVESTIGATE — confirm each arm is structurally reachable. FAIL-EXPLICIT on unreachable arms. |
| `crates/core/src/backend/emitter.rs:96, 125, 332, 469` | `_fallback: ...` underscored params on emitter trait methods | DELETE — the underscore prefix says they're unused. Either consume the fallback or remove the parameter. |
| `crates/core/src/backend/kernels/prefix_class.rs:21-23, 42` | "legacy `emit_call` wrapper that fell back to `scan_ident`... `fall through to Unrecognized`" | DELETE legacy doc-comment narrative. |
| `crates/core/src/backend/kernels/charclass.rs:32` | "The legacy `emit_call` wrapper that fell back to..." | DELETE |
| `crates/core/src/backend/types/mod.rs:5-7` | "buried under `backend/patterns/` as the lone non-shim survivor — an entire legacy directory alive for one file." | DELETE — the legacy directory is gone; comment is meta-language. |
| `crates/core/src/backend/mod.rs:4-8` | "the only non-shim file in that directory — moved to backend/types/decisions.rs. Every other file was a re-export shim..." | DELETE — meta-language about a deleted directory. |
| `crates/core/src/grammar/generated/mod.rs:14-23` | "The BBNF self-host re-exports `BbnfBootstrap` directly at this aggregator path for back-compat with consumers (most notably `bbnf-bootstrap` crate's `pub use ::bbnf::grammar::generated:: BbnfBootstrap`). Other grammars require namespaced access via their per-grammar module to avoid marker-struct collisions across grammars" | KEEP code (the re-export); DELETE the "back-compat" framing — under no-backward-compat, BBNF should also be namespaced. Actual: `pub use bbnf::*` aggregator is asymmetric. FAIL-EXPLICIT — make BBNF namespaced too. |
| `crates/core/src/runtime/{bbnf,json,google_sheets,css_l4}/parse_with.rs` (all four files) | `LegacyPath`, `LegacySegment`, "Lower a typed-path segment into the legacy borrowed alphabet" | MERGE-WITH typed `path::ir::Path` directly. The "legacy" alphabet is `runtime::path::PathSegment` (see §4.1); kill the duplicate alphabet. |
| `crates/core/src/runtime/bbnf/arena.rs:220` | "`None` when the codegen did not record bounds (legacy emission paths or non-BBNF" | INVESTIGATE — codegen is now the only path; the "legacy emission" disjunct is dead. DELETE that arm. |
| `crates/core/src/runtime/bbnf/view.rs:206` | "recorded bounds are absent (legacy emission paths or non-BBNF" | DELETE legacy disjunct. |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:281` | "Ref for now — sub-rule's emitter carries" | INVESTIGATE — "for now" is a TODO. |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs:118` | "legacy Alt-dispatch body (pre-W4 pattern preserved for" | DELETE legacy body. |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:85` | "the legacy `push_leaf_with_unit()` for callers that have not" | DELETE — the legacy callers should not exist. |
| `crates/core/src/backend/rust/emitter/shapes/array/mod.rs:35` | "legacy record stream fallback is selected by this module" | INVESTIGATE — the fallback may be the wrong default; re-derive from first principles. |
| `crates/core/src/backend/rust/emitter/shapes/substrate.rs:70-73` | "13 (No silent fallback), the resolver does NOT route a parse... `JsonStructBuilder` fallback emitted code that drove JSON's" | KEEP — anti-fallback narrative is the gate. |
| `crates/core/src/backend/rust/emitter/grammar.rs:4` | "`emit_rule_function_impl` is retained as an empty shim so the" | DELETE empty shim. |
| `crates/core/src/backend/rust/emitter/grammar.rs:112` | "do not emit the legacy" | KEEP — anti-legacy narrative. |
| `crates/core/src/backend/rust/emitter/profile.rs:50` | "alphabet's `digraph_pairs` without a shim layer" | KEEP narrative. |
| `crates/core/src/backend/ts/projection.rs:113` | "`declare function …` shim emitted at the top" | INVESTIGATE — TS shim emission shape. |
| `crates/core/src/lower/value_expr/simple_kinds.rs:185` | "Defensive fallback — descend through any value-layer" | FAIL-EXPLICIT — defensive fallbacks are workarounds. Either prove the case is unreachable and `unreachable!()`, or fix the upstream pass. |
| `crates/core/src/backend/rust/emitter/shapes/unordered.rs:288` | "Defensive fallback: a malformed Unordered rule under" | FAIL-EXPLICIT |
| `crates/gorgeous/src/vm.rs:217` | ">8-byte separators: fall through to default (shouldn't happen in practice)" | FAIL-EXPLICIT — "shouldn't happen in practice" is a TODO. |

### 3.2 Generated-code residue (regen-clean)

`crates/core/src/grammar/generated/{json,google_sheets,bbnf,css_pretty,css_l4}.rs`
have multiple `linear-try fallback (the byte-dispatch arms are still`
hits — these are emitter-source TODOs that landed in 8 generated
files. FATE: scrub the emitter's docstring template, then regen.
DELETE the residue.

`crates/core/src/grammar/generated/json.rs:667, 1357, 1365` —
`fn parse_number_fallback(bytes: &[u8]) -> f64` — fallback path for
number parsing that ALSO appears at `crates/core/src/grammar/generated/
css_l4.rs:16208, 27788`. The emitter source is `crates/core/src/
backend/rust/emitter/shapes/number.rs:190` (`emit_number_fallback_helper`).
The fallback exists because `lexical-core` rejects edge-case numbers.
KEEP — legitimate edge handling — but rename `parse_number_fallback`
to `parse_number_lexical_overflow` to remove the workaround framing.

---

## 4. Duplicated effort

### 4.1 Three "path" implementations

| Crate / Module | LOC | Purpose | Public surface |
|---|---|---|---|
| `crates/bbnf-path/` (proc-macro) | 918 | Compile-time `path!(GrammarMarker, "seg", 0, "seg")` macro that lifts a path literal into a `TypedPath<G, T>` at Rust compile time. | `pub fn path(input: TokenStream) -> TokenStream` |
| `crates/bbnf-path-ts/` (cdylib) | 1012 | TS/wasm twin of the proc-macro. Cdylib exposes `compile_path` + `execute_path` to TS template-tag. The validate/lower logic is **mirrored verbatim** from `bbnf-path/src/path_macro.rs` because proc-macro crates cannot be path-deps. | `compile_path`, `execute_path`, `compile_path_native`, `execute_path_native` |
| `crates/core/src/path/` (typed-path IR + executor) | 1300+ | Typed-path runtime: `PathSegment`, `Path`, `TypedPath<G, T>`, `OwnedPathSegment`, `IntoPathSegment`, `PathError`, `PathSchema`, `PathCursor`, `PathExecutor`, `AscentStrategy`, `select_variant`. Defines `PathSegment<'a> { Field(&'a str), Index(usize), Wildcard }` (typed surface). | `pub use ir::{IntoPathSegment, OwnedPathSegment, Path, PathSegment, TypedPath}`, plus `cursor::*`, `executor::*`, `schema::*`, etc. |
| `crates/core/src/runtime/path.rs` (legacy borrowed alphabet) | 163 | Older `PathSegment<'a> { Field(&'a str), Index(usize) }` and `Path<'a>(pub &'a [PathSegment<'a>])` — the borrowed alphabet that runtime documents' `*PathQuery` traits consume. | `PathSegment`, `Path`, `IntoPathSegment` |

**Critical duplication:** `crates/core/src/path/ir.rs` AND
`crates/core/src/runtime/path.rs` BOTH define types named `PathSegment<'a>`
and `Path<'a>` with overlapping but non-identical shape. The four
parse_with files (`runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs`)
manually lower the typed alphabet down to the legacy alphabet:

```
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};
let mut legacy: Vec<LegacySegment<'_>> = Vec::with_capacity(path.len());
for owned in path.owned_segments() { legacy.push(lower(&owned.as_borrowed())?); }
doc.get::<T>(LegacyPath::new(&legacy))
```

| Component | FATE |
|---|---|
| `crates/core/src/runtime/path.rs` | DELETE — replace with `use crate::path::ir::{Path, PathSegment, IntoPathSegment}`. The runtime documents' path-query traits should consume the typed alphabet directly. |
| `runtime/{bbnf,json,css_l4,google_sheets}/parse_with.rs` legacy lowering | DELETE — once `runtime/path.rs` is unified, the manual lower vanishes. |
| `crates/bbnf-path-ts/src/compile.rs` mirror of `bbnf-path/src/path_macro.rs` | MERGE — extract the shared validate/lower logic into a non-proc-macro `bbnf-path-core` crate; both `bbnf-path` (proc-macro) and `bbnf-path-ts` (cdylib) path-dep on it. Eliminates ~500 LOC of mirrored code. |
| `crates/bbnf-path/src/registry.rs` and `crates/bbnf-path-ts/src/fixture.rs` synthetic fixtures | DELETE — finish the T4 closure: emit `pub const REGISTRY: StructRegistry` per grammar in the xtask-emitted source (the workspace already writes `<grammar>.registry.json` sidecar — emit `REGISTRY` as a build-time `serde_json::from_str` const or `static`). Both frontends consume the production const. |

### 4.2 Three-layer builder

| Layer | Path | LOC | Purpose |
|---|---|---|---|
| Trait | `crates/core/src/runtime/builder.rs` | 141 | `pub trait StructBuilder` — the per-shape emitter target surface. |
| Generic template | `crates/core/src/runtime/builder_template.rs` | 286 | `SimpleStructBuilder<V, A, C>` — generic instantiation for the simple-grammar cohort (BNF, EBNF, CSV, CSS Pretty, Math). Per-grammar `builder.rs` is a thin instantiation. |
| Per-grammar concrete | `runtime/<g>/builder.rs` ×9 | varies (54-1014) | Concrete `<G>StructBuilder` instances. Five (BNF, EBNF, CSV, CSS Pretty, Math) are 54-LOC trivial template instantiations. Four (JSON 382, BBNF 243, CSS L4 1014, Sheets 357) have specialised per-grammar logic. |

Three layers is JUSTIFIED — the trait + template + per-grammar
instance is the right shape (`feedback_pluggable-components`). FATE: KEEP.

But: `runtime/css_l4/builder.rs` at 1014 LOC is a god module (see §5).

### 4.3 `crates/core/src/pipeline.rs` AND `crates/core/src/pipeline/`

`pipeline.rs` (105 LOC) is a "thin facade" over the `pipeline/` directory.
`pipeline/{compile,directives,validate}/` contains the actual logic.
The facade defines `CompileTarget`, `CompileRequest`, `CompileOutput`,
`CompileError`, `PipelineOptions`. The `pipeline/` directory has its own
`mod.rs` is missing; instead `pipeline.rs` is the file-form module.

| FATE |
|---|
| RESTRUCTURE — convert `pipeline.rs` into `pipeline/mod.rs` per `feedback_directory-module-structure` ("Splits use directory modules (`hir/`), not flat siblings (`hir_leaf.rs`)"). The `pipeline/` directory and `pipeline.rs` cannot legitimately co-exist; this violates the directory-module discipline. |

### 4.4 `crates/analysis/` vs `crates/core/src/`

The HARDENING audit (lane 5: cohort validation) covers analysis-vs-core
duplication. From this census's vantage:

- `crates/analysis/src/` is the LSP-facing analysis (BBNF-grammar-specific).
- `crates/core/src/lower/`, `crates/core/src/pipeline/compile/` is the
  production-facing analysis (the IR pipeline).

No DUPLICATED logic visible from the mechanical scan. The two crates
genuinely separate concerns (LSP-time vs. compile-time analysis).
FATE: KEEP separation.

### 4.5 `crates/core/src/types.rs` (top-level) — OK

136 LOC; defines AST + `RuleEntry` + `ImportDirective`. Re-exported
via `pub use types::*` at `lib.rs:8`. No duplication.

### 4.6 `crates/core/src/graph/` — OK

`graph/{deps,scc,metadata}.rs` with `mod.rs` re-exporting `deps::*` and
`scc::*` plus `metadata::find_aliases`. KEEP.

### 4.7 `crates/core/src/runtime/handle.rs` and `runtime/view.rs` — OK

Small (139 + 76 LOC). Owns `CompoundHandle`, `StringHandle`, `RuntimeView`.
KEEP.

---

## 5. God modules (>500 LOC, excluding generated/)

| Path | LOC | Recommended split |
|---|---|---|
| `crates/ir/src/passes/recognizers/grammar_facts.rs` | 1530 | Split per recognizer family: alt_classifier / chain_facts / branch_uniqueness / ... — one sub-module per fact-mining variant. |
| `crates/ir/src/passes/csp_strategy/mod.rs` | 1361 | Already has `constraints/` sub-directory. Extract more: solver wiring, domain construction, materialization-tie-in. |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | 1033 | Split per leaf-projection family: f64 path, span path, named-projection path, MapExpr-input path. |
| `crates/core/src/runtime/css_l4/builder.rs` | 1014 | OpenFrame variant explosion (14 variants). Split into per-variant frame modules under `builder/<variant>.rs` (declaration, color, color_function, color_mix, selector_list, hex_color, etc.). |
| `crates/ir/tests/shape_dispatch.rs` | 1438 | Test split per shape family. |
| `crates/ir/tests/structural_alphabet_extended.rs` | 1410 | Test split per alphabet probe. |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs` | 902 | Split per dispatch helper family. |
| `crates/ir/src/passes/materialization/classify.rs` | 843 | Split per classifier rule. |
| `crates/core/src/runtime/css_l4/value.rs` | 852 | Split typed-value variants per family: color, length-unit, selector, declaration, function-call, at-rule. |
| `crates/core/src/backend/rust/emitter/regex_scan_adapter.rs` | 786 | Split: HIR-to-DFA assembly, transition-table emission, byte-class hoisting, compile-time lookup. |
| `crates/ir/src/passes/types/mod.rs` | 786 | Split: type-vars solver, projection, lifetime calculus, registry mediation. |
| `crates/core/src/lower/expression/wrap.rs` | 731 | Split: wrap-shape detection, MapExpr lowering, payload deduction. |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` | 622 | Split: byte-dispatch emission, linear-try fallback emission, MapExpr projection. |
| `crates/core/src/lower/value_expr/atom.rs` | 590 | Split: literal lowering, projection lowering, type lowering. |
| `crates/core/src/grammar/host.rs` | 584 | Split per host-fn family. |
| `crates/ir/src/passes/audit/payload_coverage.rs` | 585 | Split: probe surface, walker, report generator. |
| `crates/ir/src/types/grammar.rs` | 584 | Split: GrammarIR struct + impls into per-concern files. |
| `crates/core/src/backend/emitter.rs` | 566 | Split: trait surface, default-impl glue, dispatch helpers. |
| `crates/core/src/runtime/css_l4/document.rs` | 541 | Split: typed-value walking, child-iteration helpers. |
| `crates/core/src/lower/expression/mod.rs` | 539 | Split per expression-kind lowering. |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` | 534 | Split per keyword-payload variant. |
| `crates/core/src/backend/rust/emitter/shapes/array/mod.rs` | 514 | Split: prefix-classifier emission, per-element loop emission. |
| `crates/ir/src/passes/payload/layout.rs` | 514 | Split: layout calculation, payload projection. |
| `crates/ir/src/passes/types/registry.rs` | 510 | Split: registry probes, layout admission, named-resolution. |
| `crates/ir/src/passes/csp_domains.rs` | 500 | Split per domain-constructor family. |

23 files >500 LOC outside `generated/`. Per `feedback_no-god-modules`,
every level (crate, module, file) separates concerns; "utils" /
"helpers" / "common" kitchen sinks are god modules in gestation.

---

## 6. Dynamic typing abuse

| Site | Form | FATE |
|---|---|---|
| `crates/ir/src/passes/transform/{inline,fuse}.rs` | `&mut dyn TraceSink` | KEEP — TraceSink is the canonical recording channel per `feedback_clean-instrumentation`; `dyn` here is a legitimate trait-object plug-in point. |
| `crates/ir/src/passes/recognizers/mod.rs:193, 265` | `miners: &[&dyn RecognizerMiner]` | KEEP — pluggable recognizer slice. |
| `crates/egraph/src/scheduler.rs:45, 71` + `csp_scheduler.rs:205` + `rewrite.rs:73` | `&[&dyn RewriteFn<N, A>]` | KEEP — the egraph rewrite trait is the public extension point; `dyn` is the right shape. |
| `crates/core/src/path/cursor.rs:115-122, 147, 178, 294` | `&'p dyn AscentStrategy`, `dyn Fn(u32, SegmentKind, usize) -> Decision + 'p` | KEEP — pluggable ascent. |
| `crates/core/src/lower/view_walk.rs:69` | `Box<dyn Iterator<Item = BbnfView<'a, 'p>> + 'a>` | INVESTIGATE — if the iterator's concrete type is statically known (likely under static dispatch + `impl Iterator`), the box is gratuitous. |
| `crates/core/src/runtime/css_l4/document.rs:176` | `Box<dyn Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a>` | INVESTIGATE — replace with `impl Iterator` if the upstream caller can take a generic iter. |
| `crates/csp-solver/src/constraint/dispatch.rs:20` | `Custom(Box<dyn Constraint<D>>)` | KEEP — constraint plug-in alphabet. |
| `crates/csp-solver/src/constraint/lambda.rs:8` | `pub(crate) type CheckerFn<D> = Box<dyn Fn(...) -> bool>` | KEEP — constraint lambda alphabet. |
| `crates/csp-solver/src/solver/optimize.rs:139, 181, 214, 250` + `lib.rs:457` | `&dyn DomainCostEval<D>` | KEEP — pluggable cost evaluator. |
| `crates/ir/src/vm/debug.rs:64` | `Box<dyn FnMut(&DebugSnapshot) -> DebugAction>` | KEEP — debugger break callback alphabet. |
| `crates/ir/src/recognizer/mod.rs:156, 258` | `&'a dyn RecognizerInfo` | KEEP — recognizer plug-in. |
| `crates/ir/src/egraph/rules/mod.rs:78` | `Vec<Box<dyn RewriteFn<...>>>` | KEEP — rule alphabet. |
| `crates/lsp/benches/bench_lsp.rs:197` | `Box<dyn Fn(usize) -> String>` | KEEP — bench input alphabet. |
| `crates/analysis/src/state/parsing.rs:81-83` | `panic_info.downcast_ref::<String>()` | KEEP — std panic API. |
| `crates/core/tests/emit_strategy.rs:66, 68` | same panic-payload downcast | KEEP. |

No abuses found in IR walkers, optimizer passes, or codegen visitors
beyond the legitimate plug-in trait objects above. The visitors are
statically typed (per `feedback_no-ts-ir`-style guidance) and use enums.

---

## 7. Inline tests in src/

User invariant: tests live in `tests/` only, never inline. Violations:

| Path:line | FATE |
|---|---|
| `crates/core/src/path/cursor.rs:313-314` | MOVE-TO `crates/core/tests/path_cursor.rs` |
| `crates/core/src/path/executor.rs:65-66` | MOVE-TO `crates/core/tests/path_executor.rs` |
| `crates/core/src/path/schema.rs:130-131` | MOVE-TO `crates/core/tests/path_schema.rs` |
| `crates/core/src/runtime/google_sheets/parse_with.rs:83-84` | MOVE-TO `crates/core/tests/parse_with_google_sheets.rs` (already exists) |
| `crates/core/src/runtime/css_l4/parse_with.rs:84-85` | MOVE-TO `crates/core/tests/parse_with_css_l4.rs` (already exists) |
| `crates/core/src/runtime/bbnf/parse_with.rs:99-100` | MOVE-TO `crates/core/tests/parse_with_bbnf.rs` (already exists) |
| `crates/core/src/runtime/json/parse_with.rs:105-106` | MOVE-TO `crates/core/tests/parse_with_json.rs` (already exists) |
| `crates/core/src/backend/rust/analysis/inline/mod.rs:37` | MOVE-TO `crates/core/tests/inline_analysis.rs` |

Eight violations. Per `feedback_no-inline-tests`, these are mechanical
fixes; ZERO inline `#[cfg(test)]` blocks must survive BA-restart.

---

## 8. Dead exports

### 8.1 Glob re-exports (audit each entry)

| Re-export site | Surface |
|---|---|
| `crates/core/src/lib.rs:8` `pub use types::*` | AST + RuleEntry + directive types (top-level). KEEP. |
| `crates/core/src/lib.rs:26` `pub use generate::*` | INVESTIGATE — generate::* is the codegen module surface; downstream callers reach codegen via `bbnf::generate::*` aggregation. Verify each item has external callers. |
| `crates/core/src/lib.rs:31` `pub use graph::*` | re-exports `deps::*` + `scc::*` + `metadata::find_aliases`. KEEP — graph types are widely consumed. |
| `crates/core/src/grammar/generated/mod.rs:35` `pub use bbnf::*` | INVESTIGATE — only BBNF is glob-aggregated; other grammars are namespaced. Asymmetry per §3.1 row "back-compat". FAIL-EXPLICIT — drop the BBNF aggregation; require namespaced access uniformly. |
| `crates/core/src/grammar/schema/mod.rs:21` `pub use model::*` | KEEP — schema model surface. |
| `crates/core/src/graph/mod.rs:7-9` `pub use deps::*` + `pub use scc::*` | KEEP. |
| `crates/core/src/backend/mod.rs:26` `pub use types::*` | KEEP. |
| `crates/core/src/backend/prettify/mod.rs:15` `pub use types::*` | KEEP. |

Generated-file `pub use __<g>parser_emit_impl::*` re-exports (8 of them)
are mandatory glob re-exports of the per-grammar emit-module surface.
KEEP.

### 8.2 Dead `#[allow(dead_code)]` annotations in src/

Outside generated/, only ONE: `crates/core/src/runtime/css_l4/document.rs:451`
`Value(&'a CssTypedValue<'p>, #[allow(dead_code)] &'a CssArena<'p>)`.
The arena reference IS held but never read; it's a phantom-typed lifetime
anchor. KEEP — explicit dead-code annotation is the right shape.

In generated/, ~25 `#[allow(dead_code)]` annotations on emitted types.
Per `feedback_clean-regen-discipline`, these are emitter-source patterns;
fix at the emitter, regen, scrub.

---

## 9. The 9-grammar runtime/<g>/ inventory

Full LOC inventory; structural duplicates flagged in the FATE column.

| Grammar | File | LOC | Purpose | FATE |
|---|---|---|---|---|
| **bbnf** | `arena.rs` | 341 | Compound slab arena + `BbnfCompoundId` | KEEP — instantiation of arena_template + bounds-recording extension (W1.9) |
| **bbnf** | `builder.rs` | 243 | Concrete `BbnfStructBuilder` w/ bounds-recording | KEEP — distinct from SimpleStructBuilder template (records compound bounds) |
| **bbnf** | `document.rs` | 453 | `BbnfDocument`, `BbnfPathQuery` impl | KEEP |
| **bbnf** | `mod.rs` | 51 | Module entry | KEEP |
| **bbnf** | `parse_with.rs` | 120 | Typed-path → legacy-path lowering | DELETE per §4.1 |
| **bbnf** | `serialize.rs` | 442 | BBNF self-grammar serializer | KEEP |
| **bbnf** | `value.rs` | 96 | `BbnfValue` enum + impls | KEEP |
| **bbnf** | `view.rs` | 280 | `BbnfView` cursor | KEEP |
| **bnf** | `arena.rs` | 54 | trivial template instantiation | MERGE-WITH a generic grammar runtime template |
| **bnf** | `builder.rs` | 54 | trivial SimpleStructBuilder instantiation | MERGE |
| **bnf** | `document.rs` | 171 | `BnfDocument`, path query | MERGE — generate per-grammar from a template |
| **bnf** | `kind.rs` | 55 | `BnfCompoundKind` enum | MERGE |
| **bnf** | `mod.rs` | 18 | trivial | MERGE |
| **bnf** | `value.rs` | 23 | trivial `BnfValue` enum | MERGE |
| **bnf** | `view.rs` | 64 | trivial | MERGE |
| **csv** | `arena.rs` | 55 | trivial | MERGE |
| **csv** | `builder.rs` | 54 | trivial | MERGE |
| **csv** | `document.rs` | 237 | path query | MERGE |
| **csv** | `kind.rs` | 66 | trivial | MERGE |
| **csv** | `mod.rs` | 49 | trivial | MERGE |
| **csv** | `value.rs` | 57 | trivial | MERGE |
| **csv** | `view.rs` | 80 | trivial | MERGE |
| **css_l4** | `arena.rs` | 390 | typed-value slab arena (specialised) | KEEP |
| **css_l4** | `builder.rs` | 1014 | 14-variant OpenFrame builder | KEEP-but-SPLIT (god module) |
| **css_l4** | `document.rs` | 541 | path query | KEEP-but-SPLIT |
| **css_l4** | `mod.rs` | 79 | re-exports | KEEP |
| **css_l4** | `parse_with.rs` | 113 | typed→legacy lowering | DELETE per §4.1 |
| **css_l4** | `value.rs` | 852 | typed-value enum (specialised) | KEEP-but-SPLIT |
| **css_l4** | `view.rs` | 137 | KEEP |
| **css_pretty** | `arena.rs` | 54 | trivial | MERGE |
| **css_pretty** | `builder.rs` | 55 | trivial | MERGE |
| **css_pretty** | `document.rs` | 174 | trivial | MERGE |
| **css_pretty** | `kind.rs` | 67 | trivial | MERGE |
| **css_pretty** | `mod.rs` | 18 | trivial | MERGE |
| **css_pretty** | `value.rs` | 23 | trivial | MERGE |
| **css_pretty** | `view.rs` | 64 | trivial | MERGE |
| **ebnf** | `arena.rs` | 54 | trivial | MERGE |
| **ebnf** | `builder.rs` | 54 | trivial | MERGE |
| **ebnf** | `document.rs` | 171 | trivial | MERGE |
| **ebnf** | `kind.rs` | 61 | trivial | MERGE |
| **ebnf** | `mod.rs` | 18 | trivial | MERGE |
| **ebnf** | `value.rs` | 23 | trivial | MERGE |
| **ebnf** | `view.rs` | 64 | trivial | MERGE |
| **google_sheets** | `arena.rs` | 332 | specialised arena | KEEP |
| **google_sheets** | `builder.rs` | 357 | specialised builder | KEEP |
| **google_sheets** | `document/canonical.rs` | 411 | canonical-form proj | KEEP |
| **google_sheets** | `document/mod.rs` | 150 | path query | KEEP |
| **google_sheets** | `document/path_query.rs` | 114 | path | KEEP — could also MERGE per template |
| **google_sheets** | `document/view.rs` | 135 | KEEP |
| **google_sheets** | `mod.rs` | 56 | KEEP |
| **google_sheets** | `parse_with.rs` | 114 | typed→legacy lowering | DELETE per §4.1 |
| **google_sheets** | `value.rs` | 189 | KEEP |
| **google_sheets** | `view.rs` | 95 | KEEP |
| **json** | `arena.rs` | 186 | specialised arena | KEEP |
| **json** | `builder.rs` | 382 | 4-variant OpenFrame builder | KEEP |
| **json** | `document.rs` | 456 | path query | KEEP |
| **json** | `mod.rs` | 53 | KEEP |
| **json** | `parse_with.rs` | 133 | typed→legacy lowering | DELETE per §4.1 |
| **json** | `value.rs` | 121 | `JsonValue` enum | KEEP |
| **json** | `view.rs` | 96 | KEEP |
| **math** | `arena.rs` | 54 | trivial | MERGE |
| **math** | `builder.rs` | 54 | trivial | MERGE |
| **math** | `document.rs` | 183 | trivial | MERGE |
| **math** | `kind.rs` | 46 | trivial | MERGE |
| **math** | `mod.rs` | 18 | trivial | MERGE |
| **math** | `value.rs` | 47 | trivial | MERGE |
| **math** | `view.rs` | 65 | trivial | MERGE |

### 9.1 Structural-duplicate cohort

**Five trivial cohort grammars** (BNF, CSV, EBNF, CSS Pretty, Math) each
have 7 files (`arena, builder, document, kind, mod, value, view`)
totalling 350-450 LOC each. These are byte-near-identical except for
type-name substitutions. The total surface is `7 × 5 = 35` files with
`~2000 LOC of mechanical instantiation`.

`crates/core/src/runtime/builder_template.rs` (286 LOC, AZ-IV.W5.3)
already factors the `StructBuilder` impl shared by these five.
`crates/core/src/runtime/arena_template.rs` (134 LOC) factors the
arena. The `document.rs` / `view.rs` / `kind.rs` / `value.rs` / `mod.rs`
are NOT yet templated.

| FATE |
|---|
| MERGE — emit per-grammar `<g>Document`, `<g>View`, `<g>Kind`, `<g>Value`, `<g>::mod` from a SINGLE codegen template at xtask-regen time. The five trivial cohort grammars become 5 × 1 thin instantiation each (~50 LOC), saving ~1500 LOC. The four specialised grammars (BBNF, JSON, Sheets, CSS L4) keep their hand-written modules. |
| The "per-grammar `mod.rs`" boilerplate that ALL nine carry (substrate-departure assertion, see §1.2) becomes one generic module template; the four specialised grammars carry a custom `mod.rs`. |

### 9.2 `crates/core/src/runtime/{arena_template,builder_template}.rs`

KEEP — these are the right shape. Per `feedback_pluggable-components`,
templates expose the typed alphabet (`SimpleValue`, `SimpleCompound`)
each grammar instantiates.

---

## 10. Top-line summary

### 10.1 Hard-deletes

- `crates/core/src/generate/serialize/` (entire directory, 156 LOC) — the
  `rule_pushes_tape_record` predicate is misnamed and appears dead under
  the post-substrate world; verify and excise.
- `crates/core/src/runtime/path.rs` (163 LOC) — duplicate of `path::ir`.
- `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs`
  legacy-lowering (~480 LOC across four files).
- `crates/core/src/css_types.rs` (66 LOC) — relocate to grammar-host.
- `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` (240 LOC) —
  per-grammar mining in IR crate; generalise via metadata-driven
  recognizer registry.
- `crates/bbnf-path-ts/src/fixture.rs` + `crates/bbnf-path/src/registry.rs`
  synthetic fixtures (~450 LOC) — finish the T4 closure.
- `crates/gorgeous/src/{json,bnf,bbnf,ebnf,css,google_sheets}.rs` (~75 LOC)
  — replace with metadata-driven dispatch.
- ~50 historical "tape" comments across runtime/ + backend/ — meta-language
  per `feedback_no-metalanguage-docs`.

### 10.2 Hard-merges

- The five trivial cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) →
  template-emitted document/view/kind/value modules (~1500 LOC saved).
- `bbnf-path` proc-macro + `bbnf-path-ts` cdylib → shared `bbnf-path-core`
  non-proc-macro path-dep (~500 LOC mirror eliminated).

### 10.3 Hard-restructures

- `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/` → directory
  module per `feedback_directory-module-structure`.
- 23 god modules (>500 LOC) outside generated/ → split per concern (§5).
- 8 inline `#[cfg(test)]` blocks → `tests/` per `feedback_no-inline-tests`.

### 10.4 First-principles violations to FAIL-EXPLICIT

- `crates/core/src/lower/value_expr/simple_kinds.rs:185` "Defensive fallback"
- `crates/core/src/backend/rust/emitter/shapes/unordered.rs:288` "Defensive fallback"
- `crates/gorgeous/src/vm.rs:217` "shouldn't happen in practice"
- `crates/core/src/runtime/google_sheets/arena.rs:38, 40, 103, 153` — fallback arena arms
- `crates/core/src/runtime/css_l4/builder.rs:713` — silent unitless fallback
- `crates/core/src/grammar/host.rs:387` — wildcard `@debug` strip-prefix
- `crates/core/src/grammar/generated/mod.rs:35` — BBNF aggregator asymmetry
- `crates/core/src/backend/emitter.rs:96, 125, 332, 469` — `_fallback` unused params
- `crates/core/src/backend/rust/emitter/grammar.rs:4` — empty shim function

### 10.5 BA-restart kill-list size estimate

Net effect:
- ~3000 LOC deleted (mostly five-grammar boilerplate + path duplication).
- ~500 LOC mirror-eliminated (bbnf-path-ts/bbnf-path).
- ~30 god-module splits.
- ~50+ tape-residue comments scrubbed.
- 8 inline test blocks moved.
- 1 generic-crate purge: `bbnf-ir` ↛ `GrammarAuditTag::{Json,CssL4,Sheets,Bbnf}`.
- 1 generic-crate purge: `bbnf-ir` ↛ `shape_dict_bbnf` recognizer.
- 1 generic-crate purge: `crates/core/src/css_types.rs` (CSS L4 host fn
  in core).

---

## Appendix A. HARDENING audit cross-reference

This census is mechanical; the HARDENING-2026-05-03-* lanes are
narrative. Cross-references:

- HARDENING-01 spec-friction → §3 (legacy markers) + §10.4 (FAIL-EXPLICIT)
- HARDENING-02 edict-adherence → §2 (grammar-specific in generic crates),
  §3 (workaround markers), §6 (dyn use)
- HARDENING-03 spec-drift → §1 (tape archaeology), §4 (duplicates)
- HARDENING-04 toolchain-forecast — out of census scope
- HARDENING-05 cohort-validation → §9 (runtime cohort), §4.4 (analysis
  cohort)
- HARDENING-06 tranche-archaeology → §1 (tape comments) + §3.1
  (tranche-named comment metabolism)
- HARDENING-07 appurtenant — out of census scope
- HARDENING-08 abrogation → §10.1 (hard-deletes)

The synthesis lane (HARDENING-SYNTHESIS-2026-05-03.md) collates lane
findings; this census is the kill-list mechanical projection.
