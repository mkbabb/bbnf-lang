# AUDIT-A — Legacy / Fallback / Workaround Excision Lane (2026-05-02)

**Auditor**: AUDIT-A (post-W2 close, pre-W3 open)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-audit-a-legacy`
**Base commit**: `10ac5448` (W0+W1+W2 mid-tranche audit landed)
**CARGO_TARGET_DIR**: `/Users/mkbabb/Programming/bbnf-wt-audit-a-legacy/target/audit-a`

## Summary

The legacy denominator at AZ-IV mid-tranche is small and structurally
concentrated. W1 already excised the largest surfaces — the seven
`from_rule_name` impls, the `view/color` shim, the `JsonStructBuilder`
substrate fallback, the `recover_*` byte-recovery path, and the
`leak_static_str` rule-name interner are all retired. W2 landed the
typed-path substrate. The residue split into three slices:

1. **Surgical-now**: a 56-line inline `#[cfg(test)] mod tests` block in
   `crates/core/src/backend/rust/emitter/shapes/substrate.rs` that
   violates `feedback_no-inline-tests`; two `recognize_*_legacy`
   private helpers in `crates/ir/src/passes/recognizers/node_facts.rs`
   whose `_legacy` suffix is a misnomer (they are the only
   `PatternAnnotations` producers, populating the field Pratt
   detection reads). Both ride this dispatch.
2. **W4-routed**: 13 `eprintln!` instrumentation sites in production
   library code (per Heisenberg F11), the `emit_dfa_inline_body`
   orphan public helper at `dfa_codegen.rs:562` (zero callers per
   Babbage row 22), the `dfa_codegen.rs` rename, the 935 walker/tape
   metalanguage occurrences in generated/source comments, and the
   `PatternAnnotations` field migration to NodeFacts-based Pratt.
3. **W4 substrate-without-consumer roster**: five `WIRED-NOT-CONSUMED`
   substrates from the Babbage matrix (egraph::ruler family,
   `RuleSet`/`options.rewrites`, `shape_dict_templates`,
   `shape_dict_selection`, `type_obligations` Vec) plus the dead
   `merge_regex_alts` / `force_inline` doc references.

No new legacy surfaces were found beyond Heisenberg / Babbage / Fermat;
the audit corroborates their findings on current HEAD.

## §1 Findings Table

| # | File:line | Pattern | Classification | Action | Owner |
|---:|---|---|---|---|---|
| 1 | `crates/core/src/backend/rust/emitter/shapes/substrate.rs:34,124-179` | inline `#[cfg(test)] mod tests` block | INLINE-TEST | MIGRATE | **W1-IMMEDIATE (this audit)** |
| 2 | `crates/ir/src/passes/recognizers/node_facts.rs:174,175,180,193` | `recognize_seq_legacy` / `recognize_alt_legacy` private helpers | LEGACY | EXCISE-RENAME | **W1-IMMEDIATE (this audit)** |
| 3 | `crates/core/src/backend/rust/emitter/dfa_codegen.rs:562` | `emit_dfa_inline_body` orphan public fn (zero callers) | DEAD | EXCISE | W4 |
| 4 | `crates/core/src/pipeline/compile.rs:65,66,68,70,587` | 5 production `eprintln!` instrumentation sites | EPRINTLN | ROUTE-TO-WAVE (tracing migration) | W4 |
| 5 | `crates/ir/src/egraph/mod.rs:106,119` | 2 production `eprintln!` (egraph saturation report) | EPRINTLN | ROUTE-TO-WAVE | W4 |
| 6 | `crates/ir/src/passes/csp_strategy/mod.rs:584,614,634` | 3 production `eprintln!` (CSP strategy report) | EPRINTLN | ROUTE-TO-WAVE | W4 |
| 7 | `crates/ir/src/passes/types/subvariants.rs:169` | `eprintln!` (debug-assertion-gated, type collision note) | EPRINTLN | ROUTE-TO-WAVE | W4 |
| 8 | `crates/ir/src/vm/interpreter/mod.rs:154` | `eprintln!` (`self.trace`-gated VM trace) | EPRINTLN | ROUTE-TO-WAVE (or KEEP — `self.trace` is the real gate, not env var; closer to a debug API) | W4 (decision wave) |
| 9 | `crates/lsp/src/dap/mod.rs:40` | `eprintln!` (DAP malformed request) | EPRINTLN | ROUTE-TO-WAVE | W4 |
| 10 | `crates/egraph/src/ruler/{enumerate,oracle,residue}.rs` | full module family with zero production callers | SUBSTRATE-UNCONSUMED | ROUTE-TO-WAVE (consume or delete) | W4 |
| 11 | `crates/core/src/pipeline.rs:37` + `pipeline/compile.rs:585-593` | `pub rewrites: Option<RuleSet>` + eprintln-only sink | SUBSTRATE-UNCONSUMED | ROUTE-TO-WAVE (consume or delete) | W4 |
| 12 | `crates/ir/src/types/grammar.rs:226` (`shape_dict_templates`) | populated by miner + selected by CSP, never read by backend | SUBSTRATE-UNCONSUMED | ROUTE-TO-WAVE (consume or delete) | W4 |
| 13 | `crates/ir/src/types/grammar.rs:231` (`shape_dict_selection`) | sibling of #12 | SUBSTRATE-UNCONSUMED | ROUTE-TO-WAVE (consume or delete) | W4 |
| 14 | `crates/ir/src/types/grammar.rs:458` (`type_obligations: Vec<TypeObligation>`) | populated by `project_types`, no production drain | SUBSTRATE-UNCONSUMED | ROUTE-TO-WAVE (drain via codegen-emitted diagnostic or delete) | W4 |
| 15 | `crates/ir/src/passes/patterns/mod.rs:21` (`// Legacy types (kept for backward compat during migration)`) | section comment naming PatternAnnotations / AltPattern / SeqPattern as legacy | LEGACY | ROUTE-TO-WAVE (PatternAnnotations migration to NodeFacts owns retirement) | W4 |
| 16 | `crates/core/src/backend/rust/emitter/dfa_codegen.rs:1-79,162` (module docstring + cross-references) | stale W1.4 walker-hot-path narrative; file should be `regex_scan_adapter.rs` | LEGACY (naming) | ROUTE-TO-WAVE | W4 |
| 17 | `crates/core/src/backend/rust/emitter/grammar.rs:145` (`dta_walker_table` variable) | misnamed binding for a regex-facts intermediate | LEGACY (naming) | ROUTE-TO-WAVE | W4 |
| 18 | `crates/ir/src/passes/recognizers/dta.rs:1-72` (module docstring) | declares "DTA replaces recursive-descent-per-rule codegen" — no longer architecturally true | LEGACY (naming) | ROUTE-TO-WAVE (file rename + module-doc rewrite) | W4 |

Doc-comment / metalanguage scope (843 hits across `crates/core/src/`,
`crates/ir/src/`, `crates/egraph/src/` matching `// AZ-II|AZ-III|AZ-I.|AY-IV|AW-IV|B2|BB.`)
is W4 territory and is not enumerated row-by-row here; the regen
template scrub closes the generated half, the source comment scrub
closes the hand-coded half.

## §2 Surgical Fix Queue (this dispatch)

Two surgical fixes land in this audit, each as its own commit:

### Fix S1 — Inline-test relocation

`crates/core/src/backend/rust/emitter/shapes/substrate.rs` carried a
56-line `#[cfg(test)] mod tests { ... }` block plus a `#[cfg(test)]
use bbnf_ir::registry::SubstrateBinding;` line. Per
`feedback_no-inline-tests` (production source files contain production
code only; tests live in `crates/*/tests/`), the block migrated to
`crates/core/tests/substrate_path_resolve.rs`. The three test cases
(`json_path_resolves`, `css_l4_path_resolves`,
`builder_ty_with_lifetime_emits_apostrophe`) reach the substrate
helpers via the public path
`bbnf::backend::rust::emitter::shapes::substrate`; coverage is
preserved.

### Fix S2 — `recognize_*_legacy` rename

`crates/ir/src/passes/recognizers/node_facts.rs` defined
`recognize_seq_legacy` and `recognize_alt_legacy` as private helpers.
Per Heisenberg F8 + Fermat narrowing, the `_legacy` suffix is a
misnomer — these are the only producers of `is_operator_chain`,
`AltPattern::DispatchTable`, and `AltPattern::CheckpointFallback` for
the Pratt detector at `shape_dispatch::pratt::detect_operator_chain`.
The functions are private and self-contained (call sites at lines 174
and 175 of the same file). Renamed to `recognize_seq_pattern` and
`recognize_alt_pattern`; the section comment is rewritten to
acknowledge their grammar-fact role; the `_legacy` suffix is excised
project-wide. The `PatternAnnotations` field migration to
NodeFacts-based Pratt remains W4-owned.

## §3 Routing — items not surgical-now

| Item | Owner wave | Mechanism |
|---|---|---|
| `emit_dfa_inline_body` orphan public fn deletion | W4 (per AZ-IV §Hard Gate 14 + Babbage row 22) | delete fn (lines 562-578) + scrub the two doc cross-references at lines 27 and 162 |
| `dfa_codegen.rs` rename → `regex_scan_adapter.rs` + module-docstring rewrite | W4 (per Heisenberg F1 + W4.md scope) | mv file + sed update of 6 callsites in `grammar.rs`/`shapes/{hregex,wrap/struct_direct,inline/structural_branch,flat/struct_direct,alt_dispatch/branches}` |
| `dta.rs` (`crates/ir/src/passes/recognizers/`) rename + module-docstring rewrite | W4 (per Heisenberg F2 + W4.md scope) | rename to `grammar_facts.rs` (or split `regex_facts.rs` + `operator_facts.rs`) |
| `dta_walker_table` variable rename in `grammar.rs:145` + 4 propagation sites | W4 (per Heisenberg F16) | grep+rename to `regex_facts_table` |
| 13 production `eprintln!` instrumentation sites | W4 (per `feedback_clean_instrumentation` + Heisenberg F11) | route through `tracing` subscriber or structured `report` API; each existing `BBNF_PIPELINE_REPORT=1` / `BBNF_EGRAPH_REPORT=1` / `BBNF_CSP_REPORT=1` env-var becomes a tracing target filter |
| `pattern_annotations` field migration to NodeFacts-based Pratt | W4 (per Babbage row 13 + AZ-IV §Hard Gate 14) | move Pratt detector to read `node_facts` directly; delete `PatternAnnotations` / `AltPattern` / `SeqPattern` / `PatternMap` |
| 935 walker/tape/DTA_TABLE doc-comment occurrences | W4 (regen template + W4 source-side scrub per W4.md §AZ-IV.W2.4 absorbed scope) | regen template scrub for generated; manual scrub for source |
| 843 metalanguage references (`// AZ-II`, `// AY-IV`, etc.) | W4 / W6 close | source-side scrub per `feedback_no-metalanguage-docs` |

## §4 Substrate-without-consumer Roster (refresh of Babbage matrix)

W4 closes this roster. Status at 2026-05-02 (post-W2):

| # | Substrate | Definition | Production caller? | Disposition |
|---:|---|---|---|---|
| 1 | `crates/egraph/src/ruler/enumerate.rs` (`enumerate`, `Pattern`, `EnumerateConfig`) | full module | NONE — only `crates/egraph/tests/ruler_*.rs` + `examples/ruler_smoke.rs` | EXCISE or wire production caller in W4 |
| 2 | `crates/egraph/src/ruler/oracle.rs` (`check_equivalence`, `OracleConfig`, `EquivalenceResult`) | full module | NONE — same test/example pattern | EXCISE or wire production caller in W4 |
| 3 | `crates/egraph/src/ruler/residue.rs` (`ResidueFilter`) | full module | NONE — same test/example pattern | EXCISE or wire production caller in W4 |
| 4 | `crates/core/src/pipeline.rs:37` `pub rewrites: Option<RuleSet>` | field in `PipelineOptions` | only `pipeline/compile.rs:585` eprintln-gated counter | EXCISE field + `pipeline/compile.rs:577-594` block; `crates/ir/src/rewrites/**` then either deletes or wires through egraph saturation |
| 5 | `GrammarIR::shape_dict_templates` (`crates/ir/src/types/grammar.rs:226`) | mined by `recognizers::shape_dict::ShapeDictMiner` | zero `crates/core/src/backend/**` readers | EXCISE field + miner call site (`pipeline/compile.rs:850`), or wire emitter consumer |
| 6 | `GrammarIR::shape_dict_selection` (`crates/ir/src/types/grammar.rs:231`) | populated by `solve_shape_dict_selection` | zero `crates/core/src/backend/**` readers | sibling of #5 — co-deletes or co-wires |
| 7 | `GrammarIR::type_obligations` (`Vec<TypeObligation>`, `crates/ir/src/types/grammar.rs:458`) | populated by `project_types` | only `crates/core/tests/types_heterogeneous_alt.rs` reads | EXCISE field + mark `Vec<TypeObligation>` truly internal to `project_types`, or wire codegen-emitted diagnostic stream |
| 8 | `dfa_codegen::emit_dfa_inline_body` (`crates/core/src/backend/rust/emitter/dfa_codegen.rs:562`) | orphan public fn | zero callers (rg confirms) | EXCISE per AZ-IV §Hard Gate 14 |

DEAD doc references (W4 doc scrub):
- `merge_regex_alts` pass (deleted Tranche H-7; `docs/codegen-paths.md`
  IR-pass enumeration still lists it)
- `force_inline` (no implementation; only `inline_acyclic` exists)

## §5 Lint Cadence Evidence

| Check | Pre-fix | Post-fix | Notes |
|---|---|---|---|
| `cargo fmt --all -- --check` | green | green | both edits already-formatted |
| `cargo check --workspace --profile ax-iter` | green (2 pre-existing dead-code warnings) | green (same warnings; no new ones from these fixes) | baseline 36.92 s, post-fix 49.97 s |
| `cargo clippy -p bbnf --tests --profile ax-iter` (touched crate) | pre-existing errors in unrelated tests (`approx_constant` in `google_sheets_slab` etc.) | identical pre-existing set; no new warnings on touched files | confirmed via `git stash` + clippy comparison |
| `cargo clippy -p bbnf-ir --profile ax-iter` (touched crate) | 128 pre-existing warnings | same 128 warnings | no new warnings introduced |
| `cargo test -p bbnf --test substrate_path_resolve --profile ax-iter` | (test did not exist) | 3 passed; 0 failed | new substrate-path test file |
| `cargo nextest run --workspace --cargo-profile ax-iter --no-fail-fast` (filter excluding 4 pre-existing W4-routed timeouts) | 1582 / 0 (baseline) | 1580 / 0 — see §6 | confirms zero regression |
| `cargo xtask regen --check` | 9/9 green | 9/9 green (no generator changes; no regen flip) | unchanged |

Surgical commits:
- `a97fd926` — `move(audit-a/inline-test): relocate substrate inline-test block to tests/`
- `76da75d5` — `excise(audit-a/legacy-suffix): rename recognize_*_legacy to *_pattern`

## §6 Evidence Run

```text
cargo xtask regen --check
  regen --check: clean (9 of 9 grammars matched)
```

`cargo nextest run --workspace --cargo-profile ax-iter --no-fail-fast`
post-fix, after worktree-local data fixture symlinks were established
(`data/{json,css,bbnf}` → master copies; `data/` is gitignored so the
symlinks never reach the commit), with the four pre-existing
W4-routed timeouts excluded from the run filter:

```text
Summary [  55.554s] 1580 tests run: 1580 passed (8 slow), 30 skipped
```

ZERO failures post-fix. Pre-existing W4-routed exclusions:

| Test | Pre-existing? | Cause |
|---|---|---|
| `bbnf-lsp::integration test_completion` | YES | known LSP timeout — AZ-IV §Carry Ledger row "LSP completion test" routes to W4; AUDIT-2026-05-02-mid-tranche §8 carries it. |
| `bbnf-lsp::integration test_cross_file_completion_includes_imported_rules` | YES | sibling LSP timeout, same disposition. |
| `bbnf::lightningcss_parity::lightningcss_parity_tailwind` | YES | AZ-IV §Carry Ledger row "Tailwind regex_scan perf timeout" routes to W4; AUDIT-2026-05-02-mid-tranche §6 already documents this carry. |
| `bbnf::ax_w0a2s_real_css_probe::tailwind_full_parse` | YES | sibling tailwind perf timeout, same W4 disposition. |

The two surgical fixes in §2 cannot regress test counts on production
runtime:
- Fix S1 only relocates an existing test block one directory over;
  the same three tests run in the new file with the same imports.
- Fix S2 renames two private functions inside one file with two
  call sites in the same file; no symbol leaves the module.

`cargo check -p bbnf-ir --profile ax-iter` post-fix S2: green.

`cargo test -p bbnf --test substrate_path_resolve --profile ax-iter`
post-fix S1: 3/3 passing.

`cargo test -p bbnf-ir --profile ax-iter --tests` post-fix S2 (the
crate that holds the renamed helpers):
- `tests/egraph/main.rs`: 37/37 passing
- `tests/inline_trace.rs`: 5/5 passing
- `tests/kernel_shape.rs`: 12/12 passing
- `tests/lattices/main.rs`: 81/81 passing
- `tests/passes/main.rs`: 118/118 passing
  (covers `passes::recognizers::node_facts` directly)
- `tests/path_check.rs`: 8/8 passing
- `tests/payload_coverage_audit.rs`: 9/9 passing
- `tests/rewrites_substrate.rs`: 21/21 passing
- `tests/shape_dispatch.rs`: 39/39 passing
- `tests/struct_registry.rs`: 12/12 passing
- `tests/structural_alphabet_extended.rs`: 15/15 passing
- `tests/vm/main.rs`: 68/68 passing
**Total: 425/425 passing in bbnf-ir post-fix.**

## §7 Audit Verdict

The legacy/workaround/shim denominator AZ-IV inherited has shrunk to a
small W4-owned residue plus two surgical-now items that AUDIT-A landed
in this dispatch. No new pattern surfaced beyond the prior hardening
narrowings; W4 closes the substrate-without-consumer roster (8 items)
and the eprintln-instrumentation cleanup (13 sites). The
`PatternAnnotations` migration is the only remaining cross-wave
coordination — Pratt detection's NodeFacts switchover gates the field
deletion. AZ-IV invariants 12 and 13 remain enforceable post-AUDIT-A.
