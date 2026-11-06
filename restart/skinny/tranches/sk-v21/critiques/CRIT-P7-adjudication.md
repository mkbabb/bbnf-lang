# CRIT-P7 adjudication — the generalization plank, re-derived from HEAD source

Base of record: HEAD `efc6b875f`. `05302109d` (the SPEC's pinned base) is an ancestor of HEAD and
byte-identical on every file cited below (`git merge-base --is-ancestor 05302109d HEAD` → true). The
un-fork commits `1aba0422e` and `b5ab2e17c` are BOTH ancestors of `05302109d` and of HEAD. Siding
with neither prototype nor critique; both mislocate the true wall.

## Verdict in one line

The PROTOTYPE is disqualified — it tests a phantom pre-un-fork architecture (`RuntimeEmitterKind`)
that does not exist at its own named base. The CRITIQUE is right to reject it and right that dispatch
is un-forked, but the critique ALSO mislocates the TOML wall: it says a third grammar "lowers to
`SinkOnly` then emits JSON-shaped classification," which is false — a third grammar without a `value`
rule routes to a real general recursive-descent emitter (`general_descent`), never to
`json_sink_direct`'s `classify()`/`validate()`. The true walls are two generator defects inside
`general_descent` that neither document names.

## (1) The actual emitter dispatch path at HEAD — structural, with one nominal residual

`emit_from_request` (`crates/codegen/src/runtime_generator.rs:26`) → `lower_for_runtime` → `render`.

- **Layer 1 (structural, un-forked):** `lower_for_runtime` (runtime_generator.rs:40-71) runs the CSS
  shape-harvest `lower::css_scan::derive_program`; `Ok ⇒ LoweredArtifact::Scan` (CSS), `Err ⇒
  LoweredArtifact::SinkOnly`. `render` (runtime_generator.rs:83-86) matches the artifact VARIANT — a
  computed lowering output, no grammar name, no config field. `RuntimeEmitterKind {
  CompiledLowering, RequestFacts }` does not exist at HEAD (only a retrospective comment at
  `crates/bbnf-bench/src/lock14_baseline.rs:1279`). The prototype's paragraph 2, BREAK 4/5, and its
  xtask diff (`emitter: RuntimeEmitterKind::CompiledLowering`; `RuntimeTarget` has no `emitter` field
  — `xtask/src/regen.rs:6-17`) describe code that is not present. **Prototype INVERTED; critique
  CONFIRMED.**

- **Layer 2 (nominal residual):** inside `SinkOnly`, `emit_sink_only` (runtime_generator.rs:98)
  branches `if !sink_only.has_rule("value")` → `emit_general_descent`; else splices the verbatim JSON
  templates (`include_str!("json_templates/…")`). This is the one surviving **grammar-NAMED** gate:
  it keys on a rule literally spelled `value`. The SK-V20 ledger's `classify()` JSON-sniffer
  (`crates/bbnf-regex/src/lib.rs:92-105`) and `validate()` span census
  (`crates/codegen/src/json_sink_direct/mod.rs:126,142-144`) are REAL and current, but reachable
  **only through this `value`-named JSON path** — `classify`/`RegexKind` has zero consumers in
  `general_descent` (verified: its consumers are `json_sink` and the `bbnf-bench` gate only).

- **The general path IS a real grammar-structure emitter.** `general_descent::render`
  (`crates/codegen/src/general_descent/emit.rs:12`) walks the full `SinkOnlyExpr` tower
  (Entry/Seq/Alt/RepeatLoop/OptionalBranch/CallRule/ByteLiteral/RegexProgram/ValueProject) and emits
  a real recursive-descent recognizer. It is LIVE for Google Sheets: `crates/runtime/src/grammars/sheets/`
  is emitted, and the sheets grammar carries **31 `->` projections** including typed scalars
  (`number -> f64`, `string -> input : Span`, `boolean -> true|false`, `error_literal -> 0u8..8u8`),
  all materialized generically by `view.rs::classify_projection` (Float/Bool/Tag/Span, derived from
  projection text). **The prototype's BREAK 1 ("every `->` is UNSUPPORTED; the typed value plane is
  structurally unreachable for any new grammar") is decisively FALSE** — Sheets is the standing
  counter-example, emitted end-to-end.

So: per-grammar specialization is **structural** at Layer 1 and at the leaf/projection level, with a
**single nominal wart** (`has_rule("value")`) remaining, plus the JSON-only census machinery gated
behind it.

## (2) What actually happens to TOML (tables, dotted keys, typed scalars)

Path: not CSS-shaped ⇒ `SinkOnly`; no rule named `value` (TOML ABNF names it `val`) ⇒
`emit_general_descent` ⇒ `general_descent::render`. Frontend + passes + projection carriage all
succeed (as they do for Sheets). Two concrete sites then break, in order:

- **WALL A — whitespace policy is hardcoded whitespace-INSENSITIVE (structural, fatal at HEAD).**
  `skip_ws` (emit.rs:126-135) skips every `is_ascii_whitespace()` byte **including `\n`**, and it is
  injected at each rule entry (emit.rs:272) and between **every** `Seq` element (emit.rs:303). The
  code comment is explicit: "uniform between sequence elements in this whitespace-insensitive
  grammar." TOML is newline-SIGNIFICANT (a key/value pair and a table header terminate at `\n`);
  the frontend's `?w` skips only ` \t\r`, never `\n` (`crates/grammar/src/lib.rs` ws modifier). The
  emitter discards that distinction and swallows newlines, so TOML's table/KV structure cannot be
  recognized. **GENERATOR-DEFECT** — the ws policy must be derived from the grammar's `?w`
  directives, not assumed universal. Neither P7 nor CRIT-P7 names this wall.

- **WALL B — the leaf matcher is a delimiter-scan approximation, not a regex engine (leaf, fatal at
  HEAD).** `match_leaf` (emit.rs:157-221) scans a leaf as the maximal run of a hardcoded text class
  `is_text_byte = [A-Za-z0-9_.$]` (emit.rs:226-228) with doubled-quote (`""`/`''`) escape handling.
  `regex_first.rs:1-10` states plainly: "The runtime has no general regex matcher… handles the leaf
  shapes the Sheets grammar uses… degrades to an empty set on anything it cannot read." TOML breaks
  it three ways: RFC3339 datetimes (`1979-05-27T07:32:00Z` truncates at the first `-`/`:`, outside
  the text class); basic strings with backslash escapes `\"` (consume_quoted only understands `""`
  doubling, so it closes early); multiline `"""…"""`/`'''` and `[\s\S]*?`. **GENERATOR-DEFECT** —
  this is the true substance behind the Lock-14 concern, correctly located at `match_leaf` +
  `regex_first`, NOT at the JSON-only `classify()` that P7/CRIT both cited.

- **WART C — the `value`-name misroute (nominal).** If a TOML author names the value rule `value`,
  `emit_sink_only` misroutes to the verbatim JSON templates and emits a JSON parser. Rename-sensitive
  fork wart; fold JSON into `general_descent` or detect the JSON family structurally.

All three live in the **shared** general emitter and its regex helpers. **Zero per-grammar files are
required** — fixing WALL A/B once generalizes to every grammar. These are generator defects, not fork
walls.

## (3) The honest generalization statement + TW6 feasibility

Honest plank statement for the SPEC: *"At HEAD the emitter dispatch is un-forked and shape-derived
(`SinkOnly`/`Scan`), a general recursive-descent emitter (`general_descent`) already emits a third
family (Google Sheets) with typed `->` projections carried and materialized, and the only JSON-nominal
residues are the `has_rule("value")` gate and the census machinery behind it. A third serious grammar
(TOML) does NOT certify at HEAD — blocked by two generator defects in the general emitter: a hardcoded
whitespace-insensitive `skip_ws` (fatal to TOML's newline significance) and a delimiter-scan
`match_leaf` that cannot match RFC3339 datetimes, escaped/multiline strings, or `[\s\S]*?` (no general
runtime regex matcher). Both are fixable once, generically."*

**TW6 TOML certification IS achievable without per-grammar emitter code** — but not at HEAD. The
required, correctly-scoped work is: (a) derive the ws-skip policy from the grammar's `?w` directives
so `\n` is preserved where the grammar treats it structurally; (b) replace `match_leaf`'s delimiter
approximation with a real per-leaf regex matcher; (c) retire the `has_rule("value")` name-gate. The
prototype's (a-delete-`RuntimeEmitterKind`) is phantom work already done; its (b)/(c)/(d) survive
under new coordinates. Widen Lock-14 to key on an emitted TOML parser round-tripping the `toml` crate
(already in the lockfile), not directory paths.

## Convergence — generalization plank: 58/100

Positive and verified: un-forked structural dispatch, a live general emitter walking grammar
structure, `->` typed projections carried and materialized for a non-JSON grammar. Negative: TW6 TOML
is unbuilt at HEAD behind two concrete generator defects (ws policy, leaf regex) that BOTH P7 and
CRIT-P7 failed to locate — the certifying instrument is compromised and the plank's TOML close-gate
rests on an under-specified residual now first identified here. Real architecture, narrow well-scoped
remaining work, no forks required → 58.
