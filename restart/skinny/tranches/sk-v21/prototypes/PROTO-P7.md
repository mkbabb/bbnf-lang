# PROTO-P7 — TOML through the un-forked emitter, end-to-end

Prototype instrument for SK-V21 §9 (grammar generalization, empirical certification) and the
Lock-14 substance question. Worktree-isolated; nothing here lands on master. HEAD under test:
`05302109d` (committed master, the SPEC's stated base). Measurement discipline: this is a
structural/correctness probe, not a perf probe — no ns figures are quoted. Environment noise for the
record: cold-ish xtask build finished in 7.39s at loadavg `{6.18 7.96 9.96}`; the second regen ran
at loadavg `{16.62 13.59 11.79}`.

## The falsifiable question and the answer

> Does a third serious language (TOML) pass through the existing grammar→emitter pipeline, and what
> is the exhaustive list of grammar-specific breaks it exposes?

**Answer: NO — decisively, and at the architecture level, not the authoring level.** At committed
master there is **no un-forked general emitter**. `emit_from_request` dispatches on a two-variant
enum `RuntimeEmitterKind { CompiledLowering, RequestFacts }`
(`skinny/crates/codegen/src/grammar_provider.rs:40-43`, dispatched at
`runtime_generator.rs:16-26`). `CompiledLowering` splices a **fixed hand-written JSON parser**;
`RequestFacts` splices **fixed CSS template files**. Neither consumes a third grammar's structure to
emit a matching parser. The "general recursive-descent emitter — Sheets via the un-forked generator"
work (main-repo commit `1aba0422e`, plus `RuntimeEmitterKind` deletion at `b5ab2e17c`) exists only as
**uncommitted changes in the main working tree** — it is not on the master the SPEC pins. On master,
the runtime grammar fleet is JSON + CSS + a `sheets_witness` stub (two files:
`crates/runtime/src/grammars/sheets_witness/{mod.rs,event_grammar_witness.rs}`), not a third live
target.

The important nuance for the plan: the **frontend and passes are grammar-general** — `parse_grammar`
and `passes::compile` accept the full TOML grammar without complaint (breaks #2–#4 all fire *after*
compile, inside the renderer). Every break is a **generator-defect concentrated in the emitter
back-half** (the projection gate, the sink renderer's JSON-shaped validation, the regex classifier,
and the verbatim JSON templates). **Zero breaks are grammar-authoring defects.** That is the precise
shape of what TW6 must build and what Lock-14 is currently not testing.

## What was built (resurrectable)

1. `skinny/grammars/toml.bbnf` — a serious TOML v1.0.0 grammar: dotted keys, bare/quoted/literal
   keys, std-tables `[a.b]`, array-of-tables `[[a.b]]`, inline tables, arrays with trailing commas
   and interspersed comments/newlines, all four string forms (basic, multiline-basic, literal,
   multiline-literal), RFC3339 offset/local datetimes + local date + local time, hex/oct/bin/dec
   integers with `_` separators, floats incl. `inf`/`nan`, booleans. Whitespace is threaded
   explicitly JSON-style because TOML newlines are structurally significant (the frontend's `?w`
   modifier skips only ` \t\r`, never `\n` — `crates/grammar/src/lib.rs:1183-1194`).
2. First authored WITH `->` typed projections mirroring the Sheets fleet member
   (`number -> f64`, `string -> input : Span`, `boolean = "true" -> true | "false" -> false`).
3. Then a projections-STRIPPED (recognize-only) variant, to probe how far grammar *structure* flows
   once the projection gate is cleared.
4. `xtask` wiring: a `TOML_TARGET` mirroring `JSON_TARGET` exactly (same 8-file `COMPILED_RUNTIME_FILES`
   roster, `emitter: RuntimeEmitterKind::CompiledLowering`), plus `regen-toml`/`check-toml` commands.
   `RequestFacts` was not used because it ignores grammar structure entirely (see break #5).

xtask diff (`skinny/xtask/src/main.rs`):
```rust
"regen-toml" => regen_toml(&root),
"check-toml" => check_toml(&root),
// ...
const TOML_TARGET: regen::RuntimeTarget = regen::RuntimeTarget {
    grammar_name: "toml", profile: "toml", entry_rule: "toml",
    source_roots: TOML_ROOTS, output_dir: "crates/runtime/src/grammars/toml",
    check_command: "check-toml", source_inputs: TOML_SOURCES,
    metadata_inputs: WORKSPACE_METADATA,
    emitter: codegen::RuntimeEmitterKind::CompiledLowering,
    expected_files: COMPILED_RUNTIME_FILES,
    frontend_requirements: codegen::RuntimeFrontendRequirements::none(),
    output_labels: None,
};
```

## The numbered break list (the deliverable)

Each break: observed error, file:line of the hardcoded assumption, classification
(GRAMMAR-AUTHORING vs GENERATOR-DEFECT), and whether it is fatal-in-principle or a fixable arm.

### BREAK 1 — the typed value plane is rejected outright: every `->` is UNSUPPORTED
- **Observed:** `Error: BBNF-UNSUPPORTED-PROJECTION: skinny/grammars/toml.bbnf@1016` (byte 1016 =
  `boolean = "true" -> true | "false" -> false ;`, the first projection encountered).
- **Root cause:** `RuntimeConstruct::unsupported` maps both `Projection` and `TypedProjection` to
  `BBNF-UNSUPPORTED-PROJECTION` (`crates/grammar/src/lib.rs:219-221`); the gate at
  `crates/codegen/src/grammar_provider.rs:110-119` returns that error for any emitter other than
  `RequestFacts`. The JSON grammar (`skinny/grammars/json.bbnf`) contains **zero** `->`, so the
  CompiledLowering path has never had a projection to support.
- **Classification:** GENERATOR-DEFECT. Fatal-in-principle for the CompiledLowering path — the
  typed value plane the SPEC mandates ("every `->` reaches the emitter", §4/TW3) is structurally
  unreachable for any new grammar through the only committed compiled emitter.

### BREAK 2 — the sink renderer hard-requires JSON's exact three span kinds
- **Observed (after stripping all `->`):** `Error: lowering failed: sink-only renderer missing
  Number RegexProgram in BIR`. This fires *after* `parse_grammar` + `passes::compile` succeed — the
  grammar compiled cleanly; the JSON renderer rejected it.
- **Root cause:** `validate()` in `crates/codegen/src/json_sink_direct/mod.rs:34-45` demands the
  program contain all of `SinkOnlySpanKind::{String, Number, Whitespace}`. TOML's numeric leaves
  never classify as `Number` (break #3), so validation aborts. TOML would also fail the `Whitespace`
  requirement next (break #3b).
- **Classification:** GENERATOR-DEFECT. The renderer encodes JSON's leaf census as a precondition.

### BREAK 3 — regex-kind classification is literal-prefix sniffing tuned to JSON's three patterns
- **Root cause (the substance-of-Lock-14 finding):** `classify()` in
  `crates/bbnf-regex/src/lib.rs:92-105` decides a regex's `RegexKind` by matching JSON's exact
  spellings:
```rust
fn classify(pattern: &str) -> RegexKind {
    if pattern == r"[ \t\n\r]*" { RegexKind::Whitespace }         // JSON's ws, byte-for-byte
    else if pattern.starts_with('"') { RegexKind::QuotedString }
    else if pattern.starts_with("-?(0|[1-9]")                     // JSON's number prefix
        && (pattern.contains(r"\d") || pattern.contains("[0-9]"))
        && pattern.contains("[eE]") { RegexKind::Numeric }
    else { RegexKind::Unknown }
}
```
  TOML's `integer` starts `[+-]?0x…` / `[+-]?(0|[1-9]…` and `float` starts `[+-]?(` — none begin with
  `-?(0|[1-9]`, so both become `RegexKind::Unknown`, never `Numeric` (`passes/src/lib.rs:1596-1597`
  maps `Numeric → SpanKind::Number`). Dates, hex/oct/bin ints, `inf`/`nan` — all `Unknown`.
- **BREAK 3b:** TOML's whitespace `/[ \t]*/` is not byte-identical to `[ \t\n\r]*`, so TOML has NO
  `Whitespace` span either — the next `validate()` requirement would fail even if #3 were fixed.
- **Classification:** GENERATOR-DEFECT. This is the empirical proof behind SPEC §9 / the "…" in the
  charter aim: the "generalized" regex-shape recognizer is three hardcoded JSON sniffers. A grammar
  whose leaves are not spelled byte-identically to JSON's is invisible to it.

### BREAK 4 — the emitted parser body is a fixed JSON parser; grammar rules select nothing
- **Root cause:** even with a structurally-perfect grammar that satisfied #2/#3, `render()`
  (`crates/codegen/src/json_sink_direct/mod.rs`) emits a **verbatim hand-written JSON recursive
  descent**: `use super::sink::JsonSink` (mod.rs:82); `b'{' => parse_object_direct`,
  `b'[' => parse_array_direct` (mod.rs:139-140); `sink.object_bool(true)`, `sink.object_null()`,
  `parse_number_object_direct` (mod.rs:187-200) — keyed on JSON's literal byte alphabet. The only
  grammar-derived inputs are `program.entry_rule`, a `direct_shapes` count, and `dispatch_alt_count`,
  which land in a **header comment** (mod.rs:77-78) and an entry-rule existence check (mod.rs:19-24).
- **BREAK 4b:** `emit_compiled` (`crates/codegen/src/runtime_generator.rs:29-74`) fills 7 of the 8
  output files from `include_str!("json_templates/{generated,parser,value,view,visitor}.rs")` +
  `JSON_HOST_RS` + `JSON_MOD_RS`, and validates the roster is exactly `COMPILED_RUNTIME_FILES`
  (the JSON set). A successful TOML regen would emit a module exporting `JsonValue`,
  `parse_bytes -> JsonValue`, a JSON visitor — i.e., it would parse JSON, not TOML.
- **Classification:** GENERATOR-DEFECT. Fatal-in-principle: the compiled path is a JSON parser
  wearing a grammar-shaped header comment.

### BREAK 5 — the alternate emitter (RequestFacts / CSS) is equally non-general
- **Root cause:** `emit_request_facts` (`runtime_generator.rs:76-103`) emits
  `normalize(CSS_GENERATED_RS)`, `CSS_MOD_RS`, `CSS_PARSER_RS`, `CSS_SINK_RS` verbatim plus a
  request-identity `config.rs`; the profile-id must be one of a fixed `CSS_PROFILE_IDS` list
  (`lib.rs:304-312`). TOML cannot route here without impersonating a CSS companion, and would still
  emit CSS.
- **Classification:** GENERATOR-DEFECT (documented, not exercised).

### BREAK 6 — regex-feature exposure (the one place authoring *would* also matter, deferred)
Because breaks #1–#4 abort before any per-leaf regex is compiled to an engine, the bespoke regex
frontend was never asked to accept TOML's harder patterns: multiline strings `"""[\s\S]*?"""`
(non-greedy `*?`, `[\s\S]`), `\d{4}` bounded repetition, `U[0-9A-Fa-f]{8}`. These are the only
candidates for a future GRAMMAR-AUTHORING classification, and they remain **unmeasured** here — a
named residual for TW6, not a result.

## Grammar-authoring vs generator-defect tally

| # | Break | Classification | Fatal-in-principle? |
|---|-------|----------------|---------------------|
| 1 | `->` projections rejected (`BBNF-UNSUPPORTED-PROJECTION`) | GENERATOR-DEFECT | yes (compiled path) |
| 2 | renderer requires JSON's {String,Number,Whitespace} spans | GENERATOR-DEFECT | yes |
| 3 | regex classifier = 3 JSON literal-prefix sniffers | GENERATOR-DEFECT | yes |
| 3b | TOML `/[ \t]*/` != JSON `[ \t\n\r]*` → no Whitespace span | GENERATOR-DEFECT | yes |
| 4 | emitted parser body is verbatim JSON RD | GENERATOR-DEFECT | yes |
| 4b | 7/8 output files are JSON templates | GENERATOR-DEFECT | yes |
| 5 | RequestFacts path emits verbatim CSS | GENERATOR-DEFECT | yes |
| 6 | TOML regex features (`[\s\S]`, `*?`, `{n}`) | UNMEASURED (potential authoring) | unknown |

**Zero grammar-authoring breaks reached.** The frontend + passes swallowed the whole TOML grammar;
the wall is entirely in the emitter.

## What this prices for TW6 and Lock-14

- **Lock-14 is green-by-vacuity, confirmed with a live falsifier.** The constraint cannot be violated
  because the emitter cannot accept a third grammar's shape in the first place. The widened Lock-14
  the SPEC proposes (§8.3) must key on an *emitted TOML parser that round-trips against the `toml`
  crate*, not on directory paths — otherwise it re-confirms the same vacuum.
- **The tenth-grammar landing (TW6, §9) is not a grammar-authoring task; it is the un-fork.** The
  honest cost is: (a) delete `RuntimeEmitterKind` and the two verbatim template families; (b) make
  `classify()` structural (derive `String`/`Number`/`Whitespace`/date/bool from HIR shape, not from a
  byte-prefix equal to JSON's); (c) make `json_sink_direct::render` walk `SinkOnlyProgram.rules` to
  emit dispatch, instead of splicing a JSON parser; (d) carry `->` projections through
  CompiledLowering (break #1) so the typed value plane exists for non-JSON grammars. The main-repo's
  uncommitted "un-fork" (`1aba0422e`) is the down-payment on (a)/(c) but was never committed and does
  not yet address (b)/(d).
- **The oracle is free.** The `toml` crate (0.8.23) is already in the skinny lockfile and compiled in
  this run; a field-for-field TOML oracle harness is a few dozen lines whenever an emitted parser
  exists to compare against. The blocker is never the oracle — it is that there is nothing to oracle.

## Reproduction
```
cd skinny
export CARGO_TARGET_DIR=/tmp/p7-target
cargo run -p xtask -- regen-toml            # typed grammar  -> BREAK 1
# strip every `-> …` from grammars/toml.bbnf, then:
cargo run -p xtask -- regen-toml            # structural     -> BREAK 2
```
Both grammar variants and the xtask wiring are in the worktree; the typed variant's projections
mirror `grammar/google-sheets/google-sheets.bbnf` line-for-line in style.
