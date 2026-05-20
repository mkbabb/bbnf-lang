# SK-V12 Wave W1a Plan: GrammarProfile Generated Config

Inputs: `SPEC.md` requires Lock 14 generic neutrality and generated ownership of structural alphabets, FIRST/follow tables, escape policy, number policy, flag semantics, sink/view/kind wrappers, and output facts (`restart/skinny/tranches/sk-v12/SPEC.md:261-270`); W1a must introduce `GrammarConfig` or equivalent generated metadata, move JSON policy out of generic code, add a Lock 14 gate consumer, preserve JSON parity/floors, claim no CSS row, and add no directive/BIR/`BackendShape`/public substrate API (`restart/skinny/tranches/sk-v12/SPEC.md:314-349`). The user pin makes CSS L4 the W1 authority and eventual bar `lightningcss_mbps + 1`, while keeping JSON guard floors and the seven Lock 14 leaks active (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:20-34`, `:94-103`, `:148-150`). The plan contract requires one synthesis artifact with owner paths, falsifiability gate, revert protocol, same-wave consumer, and pre-blocked routes, with no source edits in the plan phase (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:51-62`, `:192-196`).

Antecedent research: consolidated W1a research selects codegen/runtime metadata plus gate consumption only, with no CSS row admission (`restart/skinny/tranches/sk-v12/research/w1a/CONSOLIDATED.md:9-23`), and recommends a codegen-private `GrammarProfile` plus generated `config.rs`, JSON policy confined to JSON modules, Lock 14 consumed by `gate-json`, JSON parity/floors preserved, and no IR expansion or public tape API (`restart/skinny/tranches/sk-v12/research/w1a/CONSOLIDATED.md:27-40`). A1 names the seven leaks and minimal edits: structural alphabet, value dispatch, string/escape, number, object/key policy, `OffsetFlags` interpretation, and sink/view/kind binding (`restart/skinny/tranches/sk-v12/research/w1a/A1-codegen-template-leaks.md:23-69`). A2 keeps `Tape`, `TapeBuilder`, `ValueRef`, `OffsetFlags`, and `PayloadArena` storage unchanged, moving policy into generated modules and forbidding public `tape::GrammarConfig` (`restart/skinny/tranches/sk-v12/research/w1a/A2-runtime-grammar-config.md:227-280`). A3 routes the executable consumer through `lock14_baseline::validate` inside `bbnf-bench --bin gate`, without changing `Outcome`, report schema, or `RESULTS.md` for the scan (`restart/skinny/tranches/sk-v12/research/w1a/A3-lock14-gate-consumer.md:39-88`). A4 requires exact generated ownership, stale-file rejection if the roster changes, typed JSON regen coverage, W1a-aware Lock 14 gating, and explicit Section 0.5 guard-floor treatment (`restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md:21-51`, `:57-70`, `:104-132`, `:136-169`). A5 says no IR changes are needed and gives the concrete codegen-private `GrammarProfile` / generated-module boundary (`restart/skinny/tranches/sk-v12/research/w1a/A5-ir-metadata-boundary.md:8-26`, `:195-226`). A6 says `RESULTS.md` currently has only JSON rows, all direct/typed floors pass, no CSS throughput command exists for W1a, and W1a accounting is REDRESS 121 under `G-W1a-GRAMMARCONFIG-LOCK14` (`restart/skinny/tranches/sk-v12/research/w1a/A6-json-guard-redress.md:20-67`, `:117-148`).

Intervention: Replace the JSON-only runtime emission choke point with a codegen-private `GrammarProfile` provider boundary, emit a generated JSON `config.rs` metadata module consumed by generated JSON parser/view/direct code, and wire a W1a-aware Lock 14 generic-crate neutrality scan into `gate-json`.

Owner paths: Redress may touch only these source paths:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs` (new)
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs` (new, moved/renamed from `sink_direct.rs`)
- `skinny/crates/codegen/src/json_typed_direct.rs` (new, moved/renamed from `typed_direct.rs`, only if typed renderer containment is needed)
- `skinny/crates/codegen/src/sink_direct.rs` and `skinny/crates/codegen/src/typed_direct.rs` only to delete or reduce to non-JSON wrappers after the move
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/json_templates/value.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/visitor.rs`
- `skinny/crates/codegen/src/json_templates/config.rs` (new)
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs` only if argument/error plumbing is required for the same `lock14_baseline` consumer
- `skinny/crates/bbnf-bench/src/report.rs` only for an explicit SK-V12 Section 0.5 floor checker, not for outcome/schema churn
- `skinny/xtask/src/main.rs` only for JSON generated roster exactness or floor-check command plumbing
- `skinny/RESULTS.md` only if rewritten by a fresh native JSON guard refresh
- `skinny/REDRESS.md` only for Item 121 PASS/FAIL accounting

Generated outputs: If redress changes codegen as planned, regenerate and own this exact JSON output roster:

- `skinny/crates/runtime/src/grammars/json/config.rs` (new)
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/host.rs`
- `skinny/crates/runtime/src/grammars/json/mod.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/visitor.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` only if `json_typed_direct` output changes

`skinny/crates/ir/src/**`, `skinny/crates/runtime/src/tape/**`, `skinny/crates/runtime/src/lib.rs`, `skinny/crates/passes/src/**`, SIMD crates, grammar inputs, fixtures, CSS generated modules, and non-JSON report fixtures are not owner paths for this plan.

Exact code shape:

1. Add `skinny/crates/codegen/src/grammar_profile.rs` with grammar-neutral metadata types only. It must contain no JSON/CSS/Sheets literals and no parser policy. Shape:

   ```rust
   pub(crate) struct GrammarProfile {
       pub grammar_name: &'static str,
       pub module: ModuleNames,
       pub structural: StructuralProfile,
       pub layout: LayoutProfile,
       pub strings: StringProfile,
       pub numbers: NumberProfile,
       pub flags: FlagProfile,
       pub bindings: BindingNames,
       pub dispatch: &'static [DispatchArm],
       pub literals: &'static [LiteralArm],
   }

   pub(crate) struct RuntimeProvider {
       pub grammar_name: &'static str,
       pub build_profile: fn(&ir::BackendIr) -> Result<GrammarProfile, crate::CodegenError>,
       pub emit_runtime: fn(&GrammarProfile, &crate::lower::LoweredProgram) -> Result<crate::EmittedSource, crate::CodegenError>,
       pub emit_typed: fn(&GrammarProfile, &crate::lower::sink_only::SinkOnlyProgram, &crate::direct_schema::DirectSchemaSet) -> Result<crate::EmittedSource, crate::CodegenError>,
   }

   pub(crate) fn select_provider<'a>(
       backend: &ir::BackendIr,
       providers: &'a [RuntimeProvider],
   ) -> Result<&'a RuntimeProvider, crate::CodegenError>;
   ```

   The selector compares `provider.grammar_name` to `backend.grammar_name`; literal `"json"` lives only in `json_provider`, not in generic files.

2. Replace `json_provider::ensure_runtime_profile(backend)` in `skinny/crates/codegen/src/lib.rs` with:

   ```rust
   let provider = grammar_profile::select_provider(backend, json_provider::providers())?;
   let profile = (provider.build_profile)(backend)?;
   let lowered = lower::lower_to_rust(...);
   (provider.emit_runtime)(&profile, &lowered)
   ```

   Do the same for typed emission through `provider.emit_typed`. Keep `lower::lower_to_rust` and all IR/lowering shape selection unchanged.

3. Move JSON policy and JSON renderer ownership behind `json_provider`. `json_provider.rs` owns `const JSON_PROVIDER: RuntimeProvider`, `fn json_profile(backend: &BackendIr)`, and runtime/typed emission helpers. `json_sink_direct.rs` and `json_typed_direct.rs` are JSON-owned codegen renderers; generic `lib.rs` must not import `JsonSink`, `JsonNodeKind`, JSON literals, JSON number/string helpers, or `serde_json`.

4. Emit `config.rs` from `json_templates/config.rs`. Generated shape:

   ```rust
   // @generated by skinny bbnf-codegen; do not edit by hand.
   pub(crate) struct JsonConfig;

   pub(crate) mod structural {
       pub(crate) const ALPHABET: &[u8] = b"{}[],:\"";
   }

   pub(crate) mod layout {
       pub(crate) fn skip(bytes: &[u8], cursor: usize) -> usize {
           parse_that_regex::skip_ascii_whitespace(bytes, cursor)
       }
   }

   pub(crate) mod flags {
       pub(crate) const STRING_NEEDS_DECODE: u8 = crate::tape::OffsetFlags::HAS_ESC;
   }

   pub(crate) mod strings {
       pub(crate) const QUOTE: u8 = b'"';
       pub(crate) const ESCAPE: Option<u8> = Some(b'\\');
       pub(crate) const REJECT_CONTROL_BELOW: Option<u8> = Some(0x20);
       pub(crate) const FAST_CAP_PARSE: usize = 16;
       pub(crate) const FAST_CAP_DIRECT: usize = 8;
       pub(crate) fn match_span(bytes: &[u8], start: usize) -> Result<parse_that_regex::StringMatch, parse_that_regex::RegexError>;
   }

   pub(crate) mod numbers {
       pub(crate) fn is_start(byte: u8) -> bool;
       pub(crate) fn match_span(bytes: &[u8], cursor: usize, first: u8) -> Option<parse_that_regex::NumberSpan>;
   }
   ```

   JSON may map `STRING_NEEDS_DECODE` to `OffsetFlags::HAS_ESC` inside this generated module. No generic file may interpret that bit as JSON.

5. Update JSON generated templates to import `super::config` and consume metadata:

   - `STRUCTURAL_ALPHABET_JSON` becomes `config::structural::ALPHABET`.
   - `skip_ws` delegates to `config::layout::skip`.
   - string parsing delegates to `config::strings::{QUOTE, ESCAPE, REJECT_CONTROL_BELOW, match_span}`.
   - number parsing delegates to `config::numbers::{is_start, match_span}`.
   - flag patches use `OffsetFlags::NONE.with(config::flags::STRING_NEEDS_DECODE)`.
   - `mod.rs` includes `pub(crate) mod config;`.

   JSON object/pair rules, JSON literal arms, JSON views, and JSON sink callback names may remain JSON-owned in generated JSON output and JSON-owned templates. They must not remain in generic provider/renderer files that CSS would reuse.

6. Add exact generated roster enforcement for the runtime JSON directory. Do not make `EmittedSource::check_dir` globally exact because `check-real-typed` targets `crates/bbnf-bench/src` with many source files. Add either:

   ```rust
   pub fn check_dir_exact_rs(&self, output_dir: impl AsRef<Path>) -> Result<(), CodegenError>
   ```

   or an `xtask check-json` wrapper that fails on any `.rs` file in `crates/runtime/src/grammars/json` not in `EmittedSource.files()`. Add `CodegenError::ExtraFile(String)` if implemented in codegen.

7. Extend `lock14_baseline::validate(root)` with a W1a-aware path:

   ```rust
   validate_entries(ALLOWLIST, root, true)?;
   validate_w1a_owner_delta_or_parent_diff(root)?;
   validate_generic_crate_neutrality(root)?;
   validate_backend_shape_surface(root)?;
   ```

   `validate_generic_crate_neutrality` scans generic roots only: `crates/codegen/src/lib.rs`, `crates/codegen/src/grammar_profile.rs`, `crates/runtime/src/tape`, `crates/runtime/src/lib.rs`, `crates/ir/src`, `crates/passes/src`, and `crates/bbnf-simd/src` if touched. It excludes JSON-owned providers/templates/renderers and generated grammar modules. It fails on grammar parser names, literal grammar-name policy branches, JSON structural alphabet constants, JSON string/number helpers, JSON object-key policy names, JSON `OffsetFlags` meanings, and JSON sink/view/kind names. Add pure helper tests for positive/negative token classes.

8. Add W1a owner-delta validation so `gate-json` can be used before and after the redress commit without weakening Lock 14. Dirty or parent-diff paths are allowed only if every changed path is in this plan's owner/generated list and the commit subject contains `sk-v12-waveW1a` after commit. Any path outside the list is a hard Lock 14 failure.

Falsifiability gate: `G-W1a-GRAMMARCONFIG-LOCK14` passes only when all correctness signals below pass. Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless the command says repo root.

```sh
cargo test -p codegen
cargo test -p runtime
cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
cargo run -p xtask -- lint-loc
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo test -p bbnf-bench skv12_non_json_report -- --nocapture
cargo test -p bbnf-bench --bin gate skv12_non_json_report_arg -- --nocapture
cargo test -p xtask gate_json_passthrough -- --nocapture
```

Lock 14 generic/root scans from repo root must show no hits in generic files except test strings that are deliberately part of `lock14_baseline` negative tests:

```sh
rg -n 'grammar_name == "json"|runtime emission currently supports grammar profile|STRUCTURAL_ALPHABET_JSON|b"\{\}\[\],:\\""|JsonSink|JsonNodeKind|JsonValue|JsonRoot|JsonVisitor|OffsetFlags::HAS_ESC|match_string_at_quote_trusted_utf8|match_number_span_from_first|serde_json|ExpectedColon|ExpectedCommaOr' skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/grammar_profile.rs skinny/crates/runtime/src/tape skinny/crates/runtime/src/lib.rs skinny/crates/ir/src skinny/crates/passes/src
rg -n 'pub (mod|trait|struct|enum).*GrammarConfig|BackendShape|BackendExpr|UnionTape|directive' skinny/crates/runtime/src skinny/crates/ir/src skinny/crates/codegen/src
```

Because this selected plan touches JSON-producing codegen/runtime paths, W1a must use a refreshed JSON guard rather than `not_refreshed:no_behavior_drift`:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard-target CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

The guard floor check must explicitly cover these SPEC Section 0.5 floors after the refresh:

- Direct: `citm_catalog/direct_to_struct` Track 1 >= 18191, Track 2 >= 17431.
- Direct: `apache_builds/direct_to_struct` Track 1 >= 11028, Track 2 >= 9996.
- Direct: `marine_ik/direct_to_struct` Track 1 >= 8759, Track 2 >= 9248.
- Direct: `unicode_basic/direct_to_struct` Track 1 >= 2253, Track 2 >= 2182.
- Typed: `twitter/real_typed_struct` Track 1 >= 17385, Track 2/oracle >= 15593.
- Typed: `citm_catalog/real_typed_struct` Track 1 >= 29928, Track 2/oracle >= 17321.
- Typed: `apache_builds/real_typed_struct` Track 1 >= 8308, Track 2/oracle >= 6754.
- Typed: `github_events/real_typed_struct` Track 1 >= 11633, Track 2/oracle >= 12029.
- Typed: `update_center/real_typed_struct` Track 1 >= 11613, Track 2/oracle >= 10150.
- Typed: `mesh/real_typed_struct` Track 1 >= 9214, Track 2/oracle >= 7739.
- Typed: `marine_ik/real_typed_struct` Track 1 >= 11552, Track 2/oracle >= 9894.

Hard cap: 30 minutes redress. If implementation or measurement reaches the cap without all gates passing, stop, save the rejected patch, revert only the W1a candidate slice, and record REDRESS 121 as BLOCKED/REJECTED.

Same-wave consumer: The generated JSON parser/view/direct/typed modules must import and exercise `super::config` in the same redress commit, and `bbnf-bench --bin gate` must consume the generic-crate scan through `lock14_baseline::validate` in the same commit. A metadata file that is emitted but unused is an orphan and fails W1a.

JSON guard treatment: `not_refreshed:no_behavior_drift` is not valid for this selected plan because codegen/runtime JSON-producing paths move. `skinny/RESULTS.md` may change only through the native `bench-json`/`gate-json` renderer path above. If refreshed JSON rows miss any floor, record measured demotion in REDRESS 121 and do not claim W1a PASS. No CSS/non-JSON row may be added to `RESULTS.md` in W1a.

Forbidden routes:

- No `skinny/crates/ir/src/**` edits, new directive, BIR/`BackendExpr` variant, `BackendShape` variant, or public runtime/tape/substrate API.
- No public `runtime::tape::GrammarConfig`, generic `TapeBuilder<C>`, new `UnionTape`, sidecar vector, retained cursor/list, or generic sink super-trait.
- No grammar-name policy branch in runtime substrate, passes, IR, or generic codegen after provider selection.
- No global broadening of JSON string or number matchers to accept CSS or Sheets.
- No reinterpretation of `OffsetFlags::HAS_ESC` or `HAS_CONTROL` as universal non-JSON flags.
- No `JsonSink`, `JsonNodeKind`, `JsonValue`, JSON views, JSON object/key policy, JSON literals, `serde_json`, or JSON structural alphabet in generic codegen/runtime roots.
- No hand CSS parser, CSS generated parser row, lightningcss comparator claim, `parse_only` admission, schema-only non-JSON PASS, or SOTA claim in W1a.
- No report schema/outcome churn for the Lock 14 scan. It is a gate precondition, not a benchmark row.

Pre-blocked routes: Do not reopen REDRESS 111's report-only/schema non-JSON lane as evidence; do not use REDRESS 112/113's obsolete generated-baseline blocker shape as a reason to fall back to Sheets; do not treat REDRESS 114-119 JSON direct residuals as CSS evidence; do not touch REDRESS 88-90 ASM-gen or REDRESS 96-98 union/event-model routes in W1a; do not treat REDRESS 120 SK-V11 close as SK-V12 close.

Redress/revert protocol:

1. Before implementation, run `git status --short` and confirm unrelated dirty or staged paths are absent or isolated. If unrelated paths appear, do not edit or revert them.
2. If a gate fails, save only the W1a candidate slice:

   ```sh
   git diff --binary HEAD -- \
     skinny/crates/codegen \
     skinny/crates/runtime/src/grammars/json \
     skinny/crates/bbnf-bench/src/lock14_baseline.rs \
     skinny/crates/bbnf-bench/src/bin/gate.rs \
     skinny/crates/bbnf-bench/src/report.rs \
     skinny/xtask/src/main.rs \
     skinny/RESULTS.md \
     skinny/REDRESS.md \
     > /tmp/skv12-waveW1a-rejected.patch
   ```

3. Inspect the rejected patch path list. If it includes user or parallel-agent edits outside the owner/generated list, stop and split the patch instead of reverting.
4. Revert only the candidate W1a files and generated outputs. Do not use `git reset --hard` or broad checkout commands in a shared worktree.
5. Record REDRESS 121 as PASS only if `G-W1a-GRAMMARCONFIG-LOCK14` passes with refreshed JSON guard floors and no CSS row. Record REDRESS 121 as BLOCKED/REJECTED with failed command evidence, guard state, rejected patch path, and routed remainder if any gate fails.
