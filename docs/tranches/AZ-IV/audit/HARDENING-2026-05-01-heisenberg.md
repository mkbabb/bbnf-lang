# AZ-IV Hardening — Heisenberg (2026-05-01)

Read-only legacy and naming surface census. Extends the Meitner narrowed
findings in HARDENING-SYNTHESIS §Accepted Source Claims and §Narrowed Claims
without redoing them. No source code modified.

## Summary

The legacy denominator the user flagged is real and concentrated in three
zones. First, the regex/operator/precedence emitter in
`crates/core/src/backend/rust/emitter/dfa_codegen.rs` is the biggest
naming lie in the workspace: it is named for DFA generation but its only
production export is `emit_regex_scan_adapter`. The file's headline
`emit_dfa_inline_body` is a 17-line wrapper around a private body
emitter with **zero external callers** outside its own module — Meitner's
prior claim is upheld. The whole file should be relocated and renamed
`regex_scan_adapter.rs`; the orphan stays only as a deletion target.

Second, the DTA naming debt has metastasised. The runtime walker is gone
and `runtime/dta.rs` is correctly deleted, but **935 occurrences of
"walker"/"tape"/"DTA_TABLE"** still appear across the workspace,
including `dta_walker_table` as the lifted-fact variable name in
`grammar.rs:145` and several hundred stale "Walker-tape compound
emission" doc comments in `crates/core/src/grammar/generated/*.rs`. The
rename is mechanical but large.

Third, `crates/core/src/backend/rust/view/` is a 572-line view module
whose `color.rs` exists only to feed two test files; production CSS
flows entirely through `runtime::css_l4::CssColor`. The whole color
sub-module is test-support that escaped into the production crate via
the `runtime::view::*` re-export. `peel.rs` is a 42-line helper used
only by `named_types.rs`; it should be folded into named_types.

Lower-priority but real: 36 `#[ignore]`d tests are deferred carries; 5
`*_legacy` symbols in `recognizers/node_facts.rs` are still the only
thing populating `pattern_annotations`; `substrate_path` silently falls
back to `JsonStructBuilder` on parse failure
(`shapes/substrate.rs:76`); 13 `eprintln!` instrumentation sites violate
`feedback_clean_instrumentation` in production lib code (5 in
`pipeline/compile.rs`, 2 in `egraph/mod.rs`, 3 in
`csp_strategy/mod.rs`, 1 each in `subvariants.rs`,
`vm/interpreter/mod.rs`, `lsp/dap/mod.rs`).

## Evidence path

- ripgrep, no cargo build/test invoked
- read-only inspection of source files via Read tool
- worktree `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-heisenberg`
- HEAD `db8b00ad`

## Findings

### F1 — `dfa_codegen.rs` is 95% misnamed: it is the regex-scan-adapter emitter — REFACTOR

The file's name implies general DFA codegen. Its actual exports
(`crates/core/src/backend/rust/emitter/dfa_codegen.rs`):

- `regex_scan_adapter_ident` (line 106) — public, used by 5 other emitter
  modules to construct the adapter ident.
- `emit_dfa_inline_body` (line 562) — public, **0 external callers**.
  Confirmed via `rg -n 'emit_dfa_inline_body' crates/ wasm/ xtask/`:
  every hit is either inside the file itself or a doc reference.
- `emit_regex_scan_adapter` (line 597) — public, called once at
  `grammar.rs:147`. This is the only consumer-facing entry point.

The public surface is therefore: **one ident helper + one adapter
emitter**. The "DFA inline body" is an internal helper for the adapter's
dispatch arms (`emit_dfa_body_for_pattern`, line 280, called at lines
689 and 692). `emit_dfa_inline_body` (line 562) was the original public
entry that hot-path walker arms spliced inline; the walker is gone,
so the public function is dead.

The file's module docstring (lines 1–79) still describes the W1.4
"zero-fn-call walker hot path" architecture that no longer exists — it
should be rewritten to describe the adapter's role in cold-path replay.

**Citation file:line list**:
- `crates/core/src/backend/rust/emitter/dfa_codegen.rs:1-79` — stale
  walker-hot-path module docstring.
- `crates/core/src/backend/rust/emitter/dfa_codegen.rs:562` —
  `emit_dfa_inline_body` orphan public function.
- `crates/core/src/backend/rust/emitter/dfa_codegen.rs:280-578` —
  internal helpers; folded into the file via the splice contract.
- `crates/core/src/backend/rust/emitter/grammar.rs:145` —
  `let dta_walker_table = bbnf_ir::passes::lift_dta(ir);` — variable
  name is wrong; should be `regex_facts_table` or similar.

**Disposition**: REFACTOR — rename file to `regex_scan_adapter.rs`,
DELETE `emit_dfa_inline_body`, rewrite module docstring, rename the
`dta_walker_table` variable at every emitter callsite (5 sites:
`grammar.rs:145,147,194,206,207`, plus uses in `shapes/hregex.rs`,
`shapes/wrap/struct_direct.rs`, `shapes/inline/structural_branch.rs`,
`shapes/flat/struct_direct.rs`, `shapes/alt_dispatch/branches.rs`).

### F2 — `dta.rs` module docstring is stale; 935 walker/tape references litter the workspace — REFACTOR

`crates/ir/src/passes/recognizers/dta.rs:1-72` declares: "the DTA
replaces the recursive-descent-per-rule codegen for a grammar's hot
path with a flat counter-DFA + frame stack + shunting-yard loop driven
by byte-class dispatch." This is no longer architecturally true — the
hot path is StructDirect, and the DTA produces facts consumed by:

1. `dfa_codegen::emit_regex_scan_adapter` (regex pattern enumeration)
2. `recognizers::operator_chain::collect_operator_chains` (precedence)
3. `core::backend::rust::emitter::grammar` (pattern statics)

The actual current role is **regex/operator/precedence fact source**,
matching Meitner's narrowing exactly.

**Citation grep** (`rg -n 'walker|tape|DTA_TABLE' crates/ wasm/`):
- 935 total hits, the largest concentration is in
  `crates/core/src/grammar/generated/`:
  - css_l4.rs: 233 hits
  - bbnf.rs: 57 hits
  - google_sheets.rs: 30 hits
  - css_pretty.rs: 25 hits
  - ebnf.rs: 15 hits
  - bnf.rs: 9 hits
  - csv.rs: 7 hits
  - json.rs: 5 hits
  - math.rs: 1 hit
  Hundreds of these are doc comments like "Walker-tape compound emission
  is replaced by typed …" that describe a transition that is fully
  complete. Per `feedback_no-metalanguage-docs` and AZ-IV invariant 7,
  these should be deleted in the regen template.

**Hand-coded source-side stale wording** (must be carved separately):
- `crates/ir/src/types/grammar.rs:267,352,368,386,545` — "walker" /
  "tape" comments still describe runtime behavior that no longer exists.
- `crates/ir/src/passes/recognizers/shape_dispatch/mod.rs:19,85,135` —
  `__dta_walker_inline::run` references in a contract that has no
  consumer.
- `crates/core/src/backend/rust/emitter/keyword_dispatch.rs:177` —
  references `dta_walker::walker_fn_ident` which does not exist.
- `crates/core/src/backend/rust/emitter/shapes/mod.rs:70,108,125,
  132-133,142,214` — multiple "fall back to `__dta_walker_inline::run`"
  references; the fallback is gone.
- `crates/core/src/backend/rust/emitter/shapes/flat/mod.rs:44` — same
  fallback claim.
- `crates/ir/src/passes/recognizers/shape_dict.rs:530` — references
  `dta_walker::lower_state::emit_seq_arm` which no longer exists.

**Disposition**: REFACTOR — rewrite `dta.rs:1-72` module docstring to
describe the regex/operator-chain fact source; rename the file to
`crates/ir/src/passes/recognizers/grammar_facts.rs` or
`regex_dta.rs`; mass-scrub stale "walker"/"tape" wording from generated
templates (one regen pass owns this); scrub source-side stale comments
under W2.4.

### F3 — `runtime/dta.rs` deletion verified — KEEP-WITH-DOC

`find crates/core/src/runtime -name 'dta.rs'` returns nothing.
`rg -n 'crate::runtime::dta|runtime::dta' crates/ wasm/` returns nothing.
The deletion landed cleanly per AZ-III FINAL.

**Disposition**: NO ACTION — verification only.

### F4 — `backend/rust/view/color.rs` is shim, only test-consumed — DELETE WITH MIGRATION

`crates/core/src/backend/rust/view/color.rs` (290 lines) defines
`Color`, `ColorSpace`, `COLOR_PAYLOAD_BYTES`. Only consumers found via
`rg -n 'view::Color|view::ColorSpace|view::COLOR_PAYLOAD_BYTES'`:

- `crates/core/src/runtime/view.rs:35` — re-export only:
  `pub use crate::backend::rust::view::color::{COLOR_PAYLOAD_BYTES, Color, ColorSpace};`
  The re-export comment at line 27 says explicitly: "preserved so
  generated `the proc-macro derive (retired B2)` output continues to
  reach the colour types via the stable `crate::runtime::view::*`
  path." — but the proc-macro is retired and the path no longer feeds
  generated code.
- `crates/core/tests/lightningcss_parity.rs:42,177,337` — test only.

Production CSS uses `crate::runtime::css_l4::CssColor` /
`CssColorSpace` / `CssColorType` (verified at `css_l4/builder.rs:41-42`,
`css_l4/value.rs:357`). The legacy `view::Color` is read by NO
production codegen path.

The `runtime::view::*` re-export at `runtime/view.rs:35` is a stub
preserving an extinct API.

**Disposition**: DELETE — relocate `color.rs` decoder to test support
(`crates/core/tests/common/legacy_color_payload.rs` or fold its decode
into `lightningcss_parity.rs` directly), delete the `runtime::view`
re-export at line 35, delete `backend/rust/view/color.rs`, retire the
W2-act compatibility comment block.

### F5 — `backend/rust/view/peel.rs` is named-types-only — REFACTOR (fold)

`crates/core/src/backend/rust/view/peel.rs` (42 lines, single function
`unwrap_structural_wrappers`). Consumed only by:
- `crates/core/src/backend/rust/view/named_types.rs:57,132,142,206`

Per `feedback_no-god-modules` and module discipline, a 42-line file
that exists for one consumer is a separation that's not earning its
keep — it's a stale split from earlier deduplication (the docstring at
line 1-15 names `peel_body` in `view/value.rs` and
`unwrap_structural_wrappers` in `view/named_types.rs` and
`view/mod.rs`; both other modules are gone).

**Disposition**: REFACTOR — fold `unwrap_structural_wrappers` into
`view/named_types.rs` as a private helper, delete `view/peel.rs`.

### F6 — `view/named_types.rs` is the only live view surface; survives — KEEP-WITH-DOC

`crates/core/src/backend/rust/view/named_types.rs` (226 lines)
implements `bbnf_ir::passes::NamedTypeResolver` for the Rust backend.
Consumers:
- `crates/core/src/backend/driver/analysis.rs:149` — production.
- `crates/core/tests/css_color_parity.rs:12` — test.
- `crates/core/tests/css_l4_color_view.rs:339,407` — test.

Has a real production consumer (the analysis driver). Module docstring
is current and architecturally accurate.

**Disposition**: KEEP — but after F4 + F5, this should be promoted out
of `backend/rust/view/` to `backend/rust/named_types.rs` (no remaining
sibling files justify the directory).

### F7 — `substrate_path` JSON-builder fallback hides codegen errors — REFACTOR (hard-fail)

`crates/core/src/backend/rust/emitter/shapes/substrate.rs:70-78`:

```rust
fn substrate_path(path: &'static str) -> TokenStream {
    match syn::parse_str::<syn::Path>(path) {
        Ok(parsed) => quote! { #parsed },
        Err(_) => quote! { ::bbnf::runtime::JsonStructBuilder },
    }
}
```

The `Err(_) => quote! { ::bbnf::runtime::JsonStructBuilder }` arm is the
"fallback to JSON" finding Meitner flagged. Comment says "Falls back to
JsonStructBuilder on parse failure so codegen never silently emits
broken paths" — but this IS silent emission of a wrong path; the
generated parse fn for a CSS L4 grammar would silently use
`JsonStructBuilder`, producing a type error far from the actual cause.

Per AZ-IV invariants 1 ("One parse path") and 8 ("Evidence closes
gates"), this fallback violates fail-loud discipline.

**Disposition**: REFACTOR — replace the `Err(_)` arm with `panic!` or
return `Result<TokenStream, ParseError>`. The hardcoded paths in
`crates/ir/src/registry/strategy.rs:158-261` are guaranteed-valid `&'static str`
literals; a parse failure means a programmer typo at strategy
authoring time, not a runtime data error.

### F8 — `recognize_*_legacy` are the only PatternAnnotations producers — KEEP-WITH-DOC

`crates/ir/src/passes/recognizers/node_facts.rs:170-199` defines
`recognize_body`, `recognize_seq_legacy`, `recognize_alt_legacy`. These
populate `PatternAnnotations`, called once at
`crates/ir/src/passes/recognizers/mod.rs:235`. Read by Pratt detection
at `crates/ir/src/passes/recognizers/shape_dispatch/pratt.rs:101-110`
(verified primary consumer).

The `_legacy` suffix on these functions is misleading — they aren't
deprecated, they're the only implementation. Per Meitner's narrowing,
PatternAnnotations is "legacy and under-consumed" but the consumer
chain runs.

**Disposition**: REFACTOR — rename `recognize_seq_legacy` /
`recognize_alt_legacy` to `recognize_seq_pattern` /
`recognize_alt_pattern`, delete the "Legacy per-rule recognition" doc
comment at line 170 (or rewrite it). The W2.5 unit's
`PatternAnnotations migration/deletion status is explicit` sub-gate
should land Pratt detection on `node_facts` directly so
`pattern_annotations` can retire.

### F9 — Bootstrap crate is a 465-line dev-bin shim — KEEP-WITH-DOC + Cargo metadata

`crates/bootstrap/Cargo.toml` declares `bbnf-bootstrap` with description
"Bootstrap: generates self-hosted BBNF grammar parser via cargo expand"
— but `src/lib.rs:28` is one line: `pub use ::bbnf::grammar::generated::BbnfBootstrap;`
The crate's actual current role is **dev binaries** (totals 465 lines):
- `src/bin/cost_grid_sweep.rs` (127 lines) — cost-config tuning tool.
- `src/bin/debug_parse.rs` (125 lines) — bootstrap-parser debug tool.
- `src/bin/dump_ir.rs` (185 lines) — IR introspection dumper.
- `src/lib.rs` (28 lines) — re-export shim with extensive plan-history metalanguage.

Consumers of `bbnf_bootstrap::BbnfBootstrap` outside the crate:
- `xtask/src/main.rs:50` — CI gate naming reference only.
- `crates/core/tests/bbnf_bootstrap_reproducibility.rs` — reproducibility test.

The Cargo.toml description is wrong — the proc-macro retired at B2 and
no `cargo expand` substrate exists.

**Disposition**: KEEP-WITH-DOC — fix `Cargo.toml` description to "Dev
binaries (cost_grid_sweep, debug_parse, dump_ir) and a thin re-export
of bbnf::grammar::generated::BbnfBootstrap"; rewrite
`src/lib.rs:1-27` module docstring to drop the proc-macro/cargo-expand
narrative. (Per `feedback_no-metalanguage-docs`, drop the B2 / B6
references too.)

### F10 — 36 `#[ignore]`d tests; ~10 are AV.0.11 carry, ~6 are AY/AZ deferrals — REFACTOR (forward-tickets)

`rg -n '#\[ignore' crates/` returns 36 hits. Sample classification:

| Count | Category | Example |
|---:|---|---|
| 10 | AV.0.11 Category A — pre-existing forward-tickets | `crates/core/tests/recover.rs:130` |
| 6 | csp-solver GAC alldiff infrastructure gap | `crates/csp-solver/tests/solver.rs:1442+` |
| 4 | gorgeous prettify codegen first-rule bug | `crates/gorgeous/tests/ebnf.rs:32` |
| 3 | analysis-mode rework | `crates/analysis/tests/directives.rs:169,196` |
| 3 | LSP analysis-mode dependency | `crates/lsp/tests/analyze.rs:177,195` |
| 1 | Sheets recursive walker stack overflow | `crates/gorgeous/tests/google_sheets.rs:28` |
| 1 | AY.W2.2 `named_types` heterogeneous Alt deferral | `crates/core/tests/css_l4_color_view.rs:291` |
| 1 | regen-only golden refresh | `crates/core/tests/regen_shape_goldens.rs:115` |
| 7 | other (Pratt const-fold, payload layouts, no_subvariant_refs, etc.) | various |

Per AZ-IV §Carry Ledger, every `#[ignore]` is a deferred carry that
must either close or be routed to a named successor. The 6 csp-solver
tests are gated on infrastructure (GAC alldiff) that may legitimately
defer; the analysis/LSP tests should land in W1 or be explicitly
retired.

**Disposition**: REFACTOR — W3 close cannot land while ignored test
counts > 0 without an explicit ledger row per ignore.

### F11 — 13 `eprintln!` instrumentation sites in production lib code — REFACTOR

Per `feedback_clean_instrumentation`, production library code must not
carry `eprintln!`-based timing/profiling.

**Production lib violations** (excludes test/bin/example):

- `crates/core/src/pipeline/compile.rs:65,66,68,70,567` — pipeline timing
  CSV emission + rewrites diagnostic, gated on `BBNF_PIPELINE_REPORT=1`.
- `crates/ir/src/egraph/mod.rs:106,119` — egraph rule diagnostic.
- `crates/ir/src/passes/csp_strategy/mod.rs:584,614,634` — CSP
  strategy diagnostics.
- `crates/ir/src/passes/types/subvariants.rs:169` — subvariant
  diagnostic.
- `crates/ir/src/vm/interpreter/mod.rs:154` — interpreter diagnostic.
- `crates/lsp/src/dap/mod.rs:40` — DAP request error log.

Total 13 production lib sites. xtask and bin crates are exempt (they're
the harness): `xtask/src/regen.rs` 6 sites,
`crates/gorgeous/src/main.rs` 18 sites,
`crates/bootstrap/src/bin/*` several sites — all CLI/runner UI.

**Disposition**: REFACTOR — production diagnostics should funnel
through a `tracing` subscriber or a structured `report` API the test
harness reads, not raw `eprintln!`.

### F12 — `_legacy`/`_v2` naming markers — REFACTOR

`rg -n '_v2\b|_compat\b|_legacy\b|_old\b|_deprecated\b' crates/`:

- `crates/csp-solver/tests/optimize.rs:393` — `_v2` is a test variable
  name (`_v2 = csp.add_variable(d2)`), benign.
- `crates/ir/src/passes/recognizers/node_facts.rs:174,175,180,193` —
  `recognize_seq_legacy`, `recognize_alt_legacy` (covered by F8).

Total 5 hits — small, all flagged in F8 above.

**Disposition**: REFACTOR (covered in F8).

### F13 — TODO/FIXME/HACK/XXX is sparse — KEEP

`rg -n 'TODO|FIXME|HACK|XXX' crates/ xtask/ wasm/` returns 6 hits, of
which 4 are `\uXXXX` literals in JSON escape parsing (false positive)
and 2 are real:

- `crates/core/src/runtime/google_sheets/value.rs:76,92` — author note
  on borrowed-span lifetime.
- `crates/ir/src/types/grammar.rs:487` — `XXX` is a literal `XXX(id)`
  placeholder in a doc comment.

Architectural debt is not concentrated here; the markers are cosmetic.

**Disposition**: NO ACTION.

### F14 — 73 `#[allow(dead_code)]` annotations — KEEP-WITH-DOC

`rg -n '#\[allow\(dead_code\)\]' crates/` returns 73 hits. Concentration:

- `crates/core/src/grammar/generated/css_l4.rs` carries dozens (the
  generated emitter is liberally muted because not every codegen
  product is consumed in every grammar).
- `crates/core/src/generate/regex/{phf,byte_class,last_byte_set}.rs`
  — debug pretty-printers.
- `crates/core/benches/json/competitors.rs:161,293,409` — competitor
  bench integrations.
- `crates/core/src/runtime/google_sheets/builder.rs:69` — one in
  production runtime; should be checked for actual deadness.

**Disposition**: KEEP-WITH-DOC — generated-code allows are appropriate
(the generator can't always know which codegen products land); audit
the production-runtime allows in W1 to check the
`google_sheets/builder.rs` site.

### F15 — Old AZ-II compat doc comments — REFACTOR

Per `feedback_no-metalanguage-docs`, doc comments must not reference
plans/commits/conversation history. Sample:

- `crates/core/src/backend/rust/view/mod.rs:3` — "AZ-II O5 removed the
  legacy tape-backed generated view surface from this module."
- `crates/core/src/runtime/view.rs:1,27,32` — multiple AZ-I.W2-act
  references plus "the proc-macro derive (retired B2)" boilerplate.
- `crates/ir/src/passes/recognizers/dta.rs:1537-1547` — long
  AZ-II.cutover.A reference block.
- Hundreds of `// AZ-II.cutover.*` and `// AW-IV.W*` references in
  source comments.

**Disposition**: REFACTOR — landed under W2.4 DTA cleanup as part of
the comment scrub. Generated-code metalanguage scrubs in the regen
template.

### F16 — `dta_walker_table` variable name in production emitter — REFACTOR

`crates/core/src/backend/rust/emitter/grammar.rs:145` defines
`let dta_walker_table = bbnf_ir::passes::lift_dta(ir);`. The variable
holds a `DtaTable` that is not the runtime walker table but a
fact-mining intermediate. The variable name is the most visible
walker-residue in the active emitter. Same variable propagates
through:
- `grammar.rs:147` (`emit_regex_scan_adapter` call site)
- `grammar.rs:194` (`collect_operator_chains` call site)

**Disposition**: REFACTOR — rename to `regex_facts_table` or
`grammar_facts_table` at the binding site and propagate. Rolled into
F1 / F2.

## Top 5 deletion-priority surfaces

| Rank | Surface | Disposition | Lines | Owner unit |
|---:|---|---|---:|---|
| 1 | `dfa_codegen::emit_dfa_inline_body` (orphan public function) | DELETE | 17 | W2.4 |
| 2 | `backend/rust/view/color.rs` + `runtime/view.rs:35` re-export | DELETE | ~300 | W1.1 |
| 3 | `backend/rust/view/peel.rs` (fold into named_types.rs) | DELETE-FOLD | 42 | W1.1 |
| 4 | `dfa_codegen.rs` rename + module-doc rewrite | RENAME-REWRITE | 827 | W2.4 |
| 5 | Generated-code `Walker-tape` doc-comment scrub (regen template) | RENAME | hundreds of lines | W0.3 (regen) or W2.4 |

## Plan-amendment summary

Two additional surfaces enter the W2.4 (DTA DFA Cleanup) and W1.1
(Runtime Path Surface) file bounds:

- W2.4 must rename `dfa_codegen.rs` → `regex_scan_adapter.rs` and
  rename `dta_walker_table` variable in `grammar.rs:145+`. Sub-gate
  text gains explicit "no `dta_walker_table` identifier in source"
  language. The generated-template Walker-tape scrub may be owned by
  W0.3 (regen) instead because it's a single template change.
- W1.1 owns the `runtime::view::*` color re-export deletion + the
  `view/color.rs` + `view/peel.rs` deletions. The W1 plan already
  declares `crates/core/src/backend/rust/view/**` as `modify/delete`;
  the sub-gate should explicitly name `view/color.rs` and `view/peel.rs`
  for deletion, plus the `runtime/view.rs:35` re-export retirement.

## Exact Wave-Amendment Text

### W2.md insertion — under §AZ-IV.W2.4 DTA DFA Cleanup

Replace the current `Sub-gate` paragraph with:

```markdown
- Sub-gate: symbol-level search shows no public dead helper
  (`emit_dfa_inline_body` deleted) and no active source/generated
  `DTA_TABLE`, walker, tape, or DTA runtime wording outside explicitly
  archived docs. The file `crates/core/src/backend/rust/emitter/dfa_codegen.rs`
  is renamed to `crates/core/src/backend/rust/emitter/regex_scan_adapter.rs`
  and its module docstring no longer describes a runtime walker. The
  emitter callsites (`grammar.rs:145+`, `shapes/hregex.rs`,
  `shapes/wrap/struct_direct.rs`, `shapes/inline/structural_branch.rs`,
  `shapes/flat/struct_direct.rs`, `shapes/alt_dispatch/branches.rs`)
  use `regex_facts_table` (or equivalent) at the `lift_dta` binding
  site, not `dta_walker_table`. The `dta.rs` file inside
  `crates/ir/src/passes/recognizers/` is renamed to `grammar_facts.rs`
  (or split into `regex_facts.rs` + `operator_facts.rs`) and its
  module docstring describes the regex/operator-chain fact source, not
  a runtime walker. `recognize_seq_legacy` / `recognize_alt_legacy`
  in `node_facts.rs` are renamed to `recognize_seq_pattern` /
  `recognize_alt_pattern`. `pattern_annotations` retires after Pratt
  detection migrates to `node_facts`-based reads, or its retirement
  decision is documented with a successor field-set. `substrate_path`
  in `shapes/substrate.rs:76` no longer falls back to
  `JsonStructBuilder` on parse failure — it `panic!`s with the
  invalid `&'static str` and the strategy authoring path.
```

### W1.md insertion — under §AZ-IV.W1.1 Runtime Path And Projection Surface

Replace the current `Sub-gate` paragraph with:

```markdown
- Sub-gate: path traversal is zero-allocation, type-inference checked,
  and same-harness 3-field JSON access beats sonic-rs `pointer!` by at
  least 20%; 30-field access is parity-or-better.
  `crates/core/src/backend/rust/view/color.rs` is deleted; the
  `Color`/`ColorSpace`/`COLOR_PAYLOAD_BYTES` decoder migrates to
  `crates/core/tests/common/legacy_color_payload.rs` (test support
  only) or is folded into `crates/core/tests/lightningcss_parity.rs`
  directly. `crates/core/src/runtime/view.rs:35`'s
  `pub use crate::backend::rust::view::color::*` re-export is deleted.
  `crates/core/src/backend/rust/view/peel.rs` is folded into
  `crates/core/src/backend/rust/view/named_types.rs` as a private
  helper and the standalone file is deleted. `view/named_types.rs` is
  promoted to `crates/core/src/backend/rust/named_types.rs` (no
  remaining sibling files justify the directory). Production CSS
  continues to flow through `crate::runtime::css_l4::CssColor`.
```

### W0.md insertion — under §AZ-IV.W0.3 Regen Totality

Append to the `Sub-gate` paragraph:

```markdown
The regen template scrubs stale `Walker-tape compound emission` doc
comments from generated `*.rs` files (post-regen
`rg -n 'Walker-tape|__dta_walker_inline' crates/core/src/grammar/generated/`
returns zero hits). Hand-coded source-side `__dta_walker_inline::run`
references in `crates/core/src/backend/rust/emitter/shapes/**` and
`crates/ir/src/passes/recognizers/shape_dispatch/mod.rs` are scrubbed
under W2.4, not W0.3 — W0.3 owns generated-template scrubs only.
```

### W2.md insertion — under §AZ-IV.W2.4 DTA DFA Cleanup, append to Files row

Add to the `Files` line (currently
`crates/core/src/backend/rust/emitter/dfa_codegen.rs, crates/ir/src/passes/recognizers/dta.rs, Rust emitter docs/tests`):

```markdown
- Files: `crates/core/src/backend/rust/emitter/dfa_codegen.rs` (rename
  to `regex_scan_adapter.rs`), `crates/core/src/backend/rust/emitter/grammar.rs`
  (rename `dta_walker_table` variable), `crates/core/src/backend/rust/emitter/shapes/{hregex,wrap/struct_direct,inline/structural_branch,flat/struct_direct,alt_dispatch/branches,mod,flat/mod}.rs`
  (rename references + scrub stale `__dta_walker_inline` comments),
  `crates/core/src/backend/rust/emitter/keyword_dispatch.rs:177` (scrub
  stale walker_fn_ident comment), `crates/core/src/backend/rust/emitter/shapes/substrate.rs`
  (replace `JsonStructBuilder` fallback with `panic!`),
  `crates/ir/src/passes/recognizers/dta.rs` (rename + module-docstring
  rewrite), `crates/ir/src/passes/recognizers/node_facts.rs` (rename
  `recognize_*_legacy`), `crates/ir/src/passes/recognizers/shape_dispatch/mod.rs`
  (scrub `__dta_walker_inline::run` contract references),
  `crates/ir/src/passes/recognizers/shape_dict.rs:530` (scrub
  `dta_walker::lower_state` reference), `crates/ir/src/types/grammar.rs`
  (scrub walker/tape comments at lines 267,352,368,386,545).
```

## Commit hash

To be added on commit at end of run.
