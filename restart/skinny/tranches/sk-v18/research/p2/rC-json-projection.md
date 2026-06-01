# SK-V18 S-P2 — Class R-C: JSON Projection from `.bbnf` (addendum 1 / wave G1)

Date 2026-05-31. aarch64 / Apple M5 Max only. RESEARCH pass — no cargo, no code.
Every claim path-cited at the live tree. Repo root `/Users/mkbabb/Programming/bbnf-lang`;
codegen lives under `skinny/crates/codegen/src/`.

## 0. The surface, grounded

The JSON `generated.rs` shipped at `skinny/crates/runtime/src/grammars/json/generated.rs`
(1235 LOC) is the byte-for-byte concatenation of THREE sources, assembled in
`runtime_generator.rs:29-37 emit_compiled`:

1. **`include_str!("json_templates/generated.rs")`** — the tape recognizer (the
   `json_templates/` byte oracle, 391 LOC). Hand-written template, `include_str!`-spliced.
2. **`JSON_PARSE_ONLY_GENERATED_RS`** const-`&str` courier (`runtime_generator.rs:195`,
   the parse-only iterative path; the shipped `parse_only_value_iterative` at
   `json/generated.rs:433`). A verbatim raw-string blob — L1 verbatim-blob hazard, one of
   the 8 `_RS` couriers (`a1` §L1).
3. **`json_sink_direct::render(sink_only)`** — the SinkOnly direct-to-struct path
   (`json/generated.rs:746`→end), where the **~70-80% hot leaf lives**:
   `parse_object_value_at_direct` (`json_sink_direct.rs:169`, shipped at
   `json/generated.rs:823`; profile §2 attributes 79.82% to it, 91.52% combined with
   `parse_array_element_at_direct`). **This is the G1 MUST-preserve leaf.**

S-P0 R2's "7× push_str fixed-literal render" IS the `render()` path. The 7 blocks:
`render_header:80`, `render_entry:97`, `render_value_dispatch:125`, `render_container_rules:252`,
`render_string_rule:327`, `render_number_rules:368`, `render_utility_rules:498` — each a
`out.push_str(r#"..."#)` of a fixed JSON parser body. The ONLY grammar-derived bytes in the
whole renderer are: the header comment (`writeln!` at :75 interpolating `program.entry_rule`,
`direct_shapes`, `dispatch_alt_count`) and the number-emitter call prefix (`render_number_emitter:457`
`format!("{prefix}i64(value);")` for `sink.`/`sink.object_`/`sink.array_`). The dispatch match
arms (`b'{'=>`, `b'['=>`, `b'"'=>`, literals), the object/array loops, the string rule — all
hand-authored literals that merely HAPPEN to encode JSON's shape. `validate()` (:18) only
GATES emission (checks entry rule + non-empty shapes/literals/spans); it does not SHAPE it —
exactly the D1 finding (`a1` §L1, `CONSOLIDATED-AUDIT.md:31`). The `@generated` banner asserts
derivation; the body is hand-written ⇒ L1 REJECT as "grammar-driven."

## 1. The metadata that ALREADY exists (the win is reachable)

The lowering pipeline already produces a RICH grammar-derived IR that `render()` discards.
`lower::lower_to_rust` → `sink_only::lower_program` (`lower/sink_only.rs:122`) emits
`SinkOnlyProgram` carrying, per rule, a fully-walked `SinkOnlyExpr` tree
(`sink_only.rs:68-96`: Entry / Seq / Alt{mode,branches} / RepeatLoop{body,min} /
OptionalBranch / ByteLiteral(bytes) / RegexProgram{span_kind,pattern} / CallRule{callee} /
DirectBuild(DirectShape{shape,fields}) / ValueProject / Return) plus `entry_rule`,
`direct_shapes`, `span_kinds`, `literals`, `dispatch_alt_count`, `policy_summary`. The JSON
grammar (`grammar/json/json.bbnf`, 20 LOC) lowers to: a `value` dispatch Alt over
object|array|string|number|bool|null; ByteLiteral `{` `[` `"`; the `true`/`false`/`null`
literals (in `program.literals`); RepeatLoop for array/object members. Everything the 7
push_str blocks hard-code is RECONSTRUCTIBLE from this tree.

Two existing in-tree projections PROVE the AST-walk emitter pattern is the house style:
- **`lower/tape_plan.rs`** — `render_expr` recurses `BackendExpr` emitting per-node ops
  (`Seq`→`seq_begin(n)`, `Alt`→`alt_dispatch_begin(n)`, `ByteLiteral`→`match_literal_hex`,
  `RepeatLoop`→`repeat_begin(min=..)`). A genuine grammar-walk codegen, no fixed body.
- **`json_typed_direct.rs:render`** (1245 LOC) — iterates `schema.roots`/`schema.types`/
  `fields`, generating object-key match arms field-by-field (`:91 for field in fields`,
  `:113` the key-dispatch loop). Data-driven, distinct output per schema.

So G1's task is to make `json_sink_direct.rs` resemble `tape_plan.rs`/`json_typed_direct.rs`:
a `SinkOnlyExpr`-walking emitter, NOT a fixed-literal stringifier.

## 2. The binding diff-control gate (the proof, NOT a line delta)

`SYNTHESIS-AUDIT-OVERFIT §2.1.1` + `CH7`: the same-wave regen MUST diff-match the
`json_templates/` byte-for-byte oracle BEFORE the oracle is deleted. The ±5% line-count is a
SOFT tripwire only (a faithful projection may reorder/dedupe past it). **The binding proof is
byte-equivalence of the regenerated `generated.rs` against the current shipped file** (the
existing `EmittedSource::check_dir`, `lib.rs:74`, already does exact `actual != *source`
comparison; the `emission_is_deterministic`/`direct_parser_is_authored_from_sink_only_lowering`
tests at `lib.rs:481-621` are the harness). Plus the L1 `.bbnf`-mutation falsifier: mutate
`json.bbnf` (e.g. drop `bool`), regen, the emitted dispatch MUST lose the `b't'`/`b'f'` arms —
a fixed body fails this. The hot-leaf preservation check is mechanical: regenerated
`generated.rs` still contains `fn parse_object_value_at_direct` with identical
`#[inline(always)]`/`inline(never)` cfg shape and the same `sink.object_*` call sites
(profile §2 G1 MUST-preserve). NB: the `json_templates/` oracle is `include_str!`-spliced (not
a `_RS` const), so it is L1-clean today as a *template*; G1's obligation is that the SinkOnly
`render()` blocks (the push_str literals) become derived AND that the parse-only `_RS` courier
(source #2) is folded into the same derived path — else G1 trades one blob shape for another.

## 3. Candidate approaches

### Candidate C1 — `SinkOnlyExpr` AST-walk emitter (the `tape_plan.rs` model)
Replace the 7 push_str blocks with a recursive emitter over the `SinkOnlyExpr` tree (mirroring
`tape_plan.rs:render_expr` and `json_typed_direct.rs`'s field loop). Each node emits its body:
Alt{Dispatch}→the `match byte {...}` with one arm per branch keyed off the branch's leading
ByteLiteral / RegexProgram span_kind / literal; RepeatLoop→the container `loop { ... }` with
separator handling; ByteLiteral→`consume_direct(.., b'X', ..)`; the three sink-variant families
(value/object/array) emitted by parameterizing the sink-call prefix (already proven viable by
`render_number_emitter`'s `{prefix}` for i64/u64/f64). The string/number leaf bodies
(`parse_string_direct`, `match_number_span_from_first`, the materialize fast paths) are emitted
as **named grammar-INVOKED primitives** under the §6 (a)-(c) escape: the `.bbnf` `string`/`number`
rules invoke them by name (`string = /.../ -> decode_json_string_to_arena`,
`number = /.../ -> f64`), so the primitive is grammar-derived-by-invocation, and its emission
varies under a rule-shape mutation.
- PRO: fully grammar-derived; distinct output per grammar (L2); single emitter walks any
  SinkOnly grammar (L3-aligned); preserves the exact hot-leaf body shape because the walk emits
  the same `match byte` + `sink.*` call sites the profile rewards (no devirtualization regression).
- CON: highest authoring cost; the hot leaf's exact byte-equivalence to the current handwritten
  body is the gate — the walk must reproduce the `b'-' | b'0'..=b'9'` fast-path split and the
  `match_tiny_plain_string_direct` inline EXACTLY, or the diff-match fails. Risk concentrated in
  reproducing the 3 near-identical dispatch variants (value/object/array) without LCD-collapsing
  them (they differ only by sink prefix — parameterize, as C1 already does).

### Candidate C2 — Parameterized template-fragment library keyed on grammar facts (hybrid)
Keep small hand-authored body fragments (the proven-hot inner kernels:
`parse_string_direct`, the number emitter, `consume_direct`/`take_direct` utilities) as named
primitives, but DRIVE their assembly — which dispatch arms exist, in what order, which
container loops, which literals — from `SinkOnlyProgram` facts (`program.literals`,
`dispatch_alt_count`, the per-rule `direct_shape`, the `SinkOnlyExpr::Alt` branch set). The
dispatch `match`, the literal arms, and the container scaffolds are emitted from facts; only the
leaf scanners are library fragments invoked by grammar-named rules.
- PRO: smaller blast radius than C1 — the highest-risk hot bytes (string/number scan) stay
  byte-identical because they're the same fragment, trivially passing the hot-leaf gate; the
  STRUCTURAL parts (arms, loops, literals) become derived, satisfying the `.bbnf`-mutation
  falsifier. KISS-leaning; matches `render_number_emitter`'s existing `{prefix}` parameterization.
- CON: the "named primitive" escape is the single largest paper-close surface (`a1` §L1 REVISE,
  R-A0-3) — each retained fragment MUST pass (a) grammar-invoked-by-name + (b) emitted-output
  varies under invoking-rule mutation + (c) `verbatim_blob_present==false`, all machine-checked.
  A fragment that is structurally fixed and keyed only off a decorative argument FAILS (b) and is
  a relabeled blob. Requires per-fragment mutate-falsifiers, not just one whole-path test.

### Candidate C3 — Delete the SinkOnly `render()` entirely; project from the typed-direct path
Observe that `json_typed_direct.rs` ALREADY data-drives a struct projection from
`DirectSchemaSet` + `SinkOnlyProgram`. Generalize THAT renderer to also emit the untyped
`JsonSink` SinkOnly path (the schema becomes the universal JSON value-shape), retiring
`json_sink_direct.rs` as a duplicate emitter.
- PRO: collapses two JSON emitters into one (system-cohesion / no-god-module); the typed path is
  already genuinely data-driven, so it inherits L1/L2 cleanliness.
- CON: the typed path's emitted bodies are NOT the profiled hot bodies — it emits
  `parser.parse_string()?`/`parse_type_*` against a `ParserState`, not the fused scan-free
  `parse_object_value_at_direct` monomorphized-sink dispatch the profile rewards (§2). Adopting
  it wholesale risks REGRESSING the 91.5% hot leaf (different call shape, possible
  devirtualization). High performance risk; rejected as primary — its data-driven STRUCTURE is a
  reference, but the SinkOnly hot-body shape must be preserved, which C3 does not guarantee.

## 4. RECOMMENDATION — C1 (AST-walk emitter), borrowing C2's named-primitive discipline for the leaf scanners

C1 is the only candidate that fully discharges addendum 1 (grammar-DERIVED body, not a courier
swap) while structurally preserving the hot leaf. It is the house pattern (`tape_plan.rs`,
`json_typed_direct.rs`) applied to the SinkOnly emitter. The pragmatic execution is C1's walk
for all STRUCTURAL emission (dispatch match, container loops, literal arms, the 3 sink-prefix
variants) WITH the string/number leaf scanners emitted as C2-style named primitives invoked by
the `.bbnf` `string`/`number` rules — each carrying its own (a)-(c) machine falsifier so the
escape is honest, not a relabel. This keeps the proven-hot inner kernels byte-stable (passing
the hot-leaf gate trivially) while making the parser SKELETON derived (passing the
`.bbnf`-mutation falsifier). Reject C3 as primary on hot-leaf-regression risk; keep its
data-driven renderer as the structural reference. Fold the parse-only `_RS` courier (source #2)
into the same walk so G1 retires BOTH blob shapes, not one.

## 5. KEY RISK

The diff-control gate is byte-equivalence, and the hand-written hot bodies contain
micro-optimizations the walk must reproduce EXACTLY: the `b'-' | b'0'..=b'9'` array fast-path
split (`json_sink_direct.rs:306`), the `match_tiny_plain_string_direct` inline string fast path
(:336), the three near-duplicate dispatch variants differing only by sink prefix. If the
AST-walk cannot reproduce these byte-for-byte, EITHER the gate fails (REDRESS) OR the team is
tempted to relabel a retained fixed fragment as a "named primitive" — the R-A0-3 paper-close.
The named-primitive escape MUST be machine-gated (a)-(b)-(c) per fragment; an unfalsifiable
"the hot leaf is a primitive" claim is the precise failure addendum 1 forbids. Secondary risk:
LCD-flattening the value/object/array dispatch triple into one generic during the walk would
erase the monomorphized-sink shape and regress the 91.5% leaf — parameterize by prefix, do not
unify.

## 6. PRUNE / SEQUENCING DEPENDENCY

G1 is the FIRST generalize wave; per §5 it entry-gates on **P-cluster closed** (P1-P5;
specifically **P4 — the Lock-14 green-by-exclusion gate — MUST land BEFORE G2/G3**, and P4
extends `FORBIDDEN_GENERIC_TOKENS` with `CSS_`/`_RS` so the JSON `_RS` couriers G1 retires are
caught at their emit site). **P5 (metalang-leak purge, R15) shares G1's exact surface:** the
`parse_w11_1_number_{direct,object_direct,array_direct}` leak lives **7× inside
`json_sink_direct.rs`** (`:147`/`:187`/`:227` + their emitted definitions in the push_str
bodies) — the file G1's AST-walk emitter re-derives. So P5's rename (`parse_w11_1_number_*` →
`parse_number_*`) and G1's re-emission are NOT independent: either P5 lands first and G1 re-emits
the already-renamed names, or G1's walk emits `parse_number_*` directly and subsumes P5 (the
walk derives the fn names from the `.bbnf` `number` rule, not a hand-copied `w11_1` tag). The
SYNTHESIS-RESEARCH §3 P5 falsifier (`grep -c parse_w11_1_number = 0`) must hold on the
G1-regenerated `json_sink_direct.rs`, not a stale one. G1's success **BLOCKS G2/G3/G4/PROVE**
(`a1` §Dependency-chain):
G2 (CSS lowering) entry-gates on G1; G3 (un-fork emitter) consumes the G1 grammar-walk pattern;
PROVE (Sheets) emits THROUGH the un-forked generator. Profile dependency: G1 is NOT a G5/G6
NEON wave (JSON G5 has NO hot leaf — `json/scan.rs` zero-sampled, §2 — so do not author a JSON
classifier), but G1 MUST NOT regress the §2 SinkOnly hot leaf, which is the load-bearing JSON
>SOTA. The diff-control gate (byte-match `json_templates/` + shipped `generated.rs` before
oracle deletion) is G1's binding exit proof; a ±5% line delta is advisory only.
