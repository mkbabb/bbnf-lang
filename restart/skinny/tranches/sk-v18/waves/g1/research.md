# SK-V18 G1 — RESEARCH + PLAN: `SinkOnlyExpr` AST-walk emitter (R-C C1)

Date: 2026-06-02. Live audit HEAD `83b66db42` (re-grepped at research time). Status: RESEARCH only —
no cargo, no regen, no implementation. Mandate: SPEC §4 (G1: JSON projection). Working tree:
`skinny/`.

This document is the feasibility assessment + transformation plan for G1. The honest bottom line is
in §4 and the executive summary; the supporting current-state map, node mapping, and primitive
analysis are §1–§3; the step plan is §5.

---

## 0 — Reading frame (the crux restated)

G1 must turn `crates/codegen/src/json_sink_direct.rs` from **7 fixed-literal `push_str` blocks** into
a **recursive emitter over the `SinkOnlyExpr` tree** (`lower/sink_only.rs:68-96`), like the in-tree
house patterns `lower/tape_plan.rs::render_expr` and `json_typed_direct.rs::render`. It must ALSO fold
the parse-only `JSON_PARSE_ONLY_GENERATED_RS` const-`&str` courier (`runtime_generator.rs:195`) into
the SAME walk so BOTH blob shapes retire.

The BINDING CONSTRAINT: the regenerated `crates/runtime/src/grammars/json/generated.rs` must be
**byte-equivalent** to (a) the `json_templates/` oracle AND (b) the current shipped `generated.rs`,
BEFORE the oracle is deleted — while being genuinely grammar-DERIVED (structure from the
`SinkOnlyExpr` tree + the `.bbnf` rules, NOT a fixed stringifier). The `.bbnf`-mutation falsifier
proves derivation: dropping the json.bbnf `bool` rule must make the `b't'`/`b'f'` dispatch arms vanish.

**Confirmed path facts (re-grepped):**
- Grammar root: `skinny/grammars/json.bbnf` (NOT `crates/codegen/src/grammar/json/json.bbnf` — that
  path does not exist). 19 lines, no `->` arrows, no `@` directives.
- Generated runtime: `crates/runtime/src/grammars/json/generated.rs` (1199 LOC). The module dir is
  `json/`, not `generated_json/` (the profile doc's `generated_json::generated::…` is the Rust *module
  path* via a re-export, not the on-disk directory).
- Oracle template: `crates/codegen/src/json_templates/generated.rs` (391 LOC).

---

## 1 — CURRENT-STATE MAP: the 7 `render_*` fns and the 3-source concatenation

### 1.1 — `json/generated.rs` is a 3-source byte-concatenation (`runtime_generator.rs:29-37`)

`emit_compiled` builds `generated.rs` as:

```
generated  = include_str!("json_templates/generated.rs")          // (1) 391-LOC tape oracle
           + "\n" + JSON_PARSE_ONLY_GENERATED_RS.trim_start('\n')  // (2) ~353-LOC const-&str courier
           + "\n" + json_sink_direct::render(sink_only)            // (3) ~452-LOC SinkOnly direct path
```

Verified on-disk boundaries in the shipped 1199-LOC file:
- **(1) template** lines 1–391: header `use` block, `attach_structural_index`, the tape-recognizer
  `parse_json`/`parse_value`/`dispatch_value`/`parse_object`/`parse_array`, `match_tiny_plain_string_direct`
  (line 164), number/literal helpers. **`recognizer: SimdScan Exact PreEntry alphabet="{}[],:\""`**
  (line 10) and `dispatch_value` (line 45) live here.
- **(2) courier** lines 393–746: `ParseOnlyState`, `parse_only`, `parse_only_value_iterative` (an
  iterative stack-frame parser), `parse_only_begin_value`, the structural-take helpers. A pure verbatim
  `r#"..."#` blob (`runtime_generator.rs:195-548`).
- **(3) sink_direct** lines 747–1199: the header comment (line 747, grammar-DERIVED), `ParsedString`,
  `parse_direct`, `parse_value_direct`, **`parse_object_value_at_direct` (line 823)** +
  **`parse_array_element_at_direct` (line 863)** — the 91.5% hot leaf — `parse_object_direct`,
  `parse_array_direct`, `parse_string_direct`, the 3 `parse_number_*` + 3 `emit_number_*`, utilities.

G1's mandate retires (2) and rewrites (3)'s renderer. The template (1) is the byte-equivalence ORACLE
that the walk must reproduce, then is deleted (§4 discusses whether the walk can subsume it).

### 1.2 — The 7 `render_*` fns: emitted bytes, fixed-literal vs grammar-interpolated

`render()` (`json_sink_direct.rs:4-16`) calls 7 fns in order. Interpolation census:

| Fn | Lines | Emits | Grammar-DERIVED today? |
|---|---|---|---|
| `render_header` | 68-94 | header comment `// sink-only lowered from BackendIr: entry={} direct_shapes={} dispatch_alt_count={}` + fixed `use super::sink::JsonSink;` + `materialize_*` import + `struct ParsedString` | **PARTIAL**: the comment interpolates `program.entry_rule`, `direct_shapes` (BTreeSet→csv), `dispatch_alt_count` (line 75-79). The `use`/`struct` body (80-93) is a fixed `r#"..."#` literal. |
| `render_entry` | 96-122 | `parse_direct` entry fn (skip-ws, call value, trailing-char check) | **NO** — 100% fixed `r#"..."#` literal. |
| `render_value_dispatch` | 124-249 | `parse_value_direct` + `parse_object_value_at_direct` + `parse_array_element_at_direct` — the 3-variant dispatch triple, each a `match byte { b'{' => … b'"' => … b'-'|b'0'..=b'9' => … b't' => … }` | **NO** — 100% fixed literal. **This carries the 91.5% hot leaf.** |
| `render_container_rules` | 251-324 | `parse_object_direct` (`{`-loop, key/colon/value/comma) + `parse_array_direct` (`[`-loop, element/comma) | **NO** — 100% fixed literal. |
| `render_string_rule` | 326-365 | `parse_string_direct` (tiny-plain fast path + quote-scan fallback → `ParsedString`) | **NO** — 100% fixed literal. |
| `render_number_rules` | 367-419 | 3 `parse_number_*` fns + 3 `emit_number_*` via `render_number_emitter` | **PARTIAL**: the 3 emitters interpolate `{prefix}` ∈ {`sink.`, `sink.object_`, `sink.array_`} (421-459). The `parse_number_*` shells (369-414) are fixed literals. |
| `render_utility_rules` | 461-525 | `consume_literal_direct`, `consume_direct`, `take_direct`, `direct_error`, `string_error` | **NO** — 100% fixed literal. |

**Net grammar-derived bytes today = the header comment interpolation + the number-emitter `{prefix}`
(3×).** Everything else is the L1 "grammar-driven banner over hand-written body" REJECT. This matches
SPEC §4's grounding exactly.

### 1.3 — Hot-leaf emit-site map (the 91.5% MUST-preserve)

From `a1-json-hot-leaves.md` (S-P1 profile, loadavg-caveated but rank-stable):

| Profile leaf | self-% | Emit site (codegen) | Emit site (shipped) | G1 disposition |
|---|---:|---|---|---|
| `parse_object_value_at_direct` | 79.82% | `render_value_dispatch` 167-205 | gen.rs:823 | **structure → walk-derived skeleton** (NOT a primitive) |
| `parse_array_element_at_direct` | 11.70% | `render_value_dispatch` 207-245 | gen.rs:863 | **structure → walk-derived skeleton** |
| `unescape_string` (in `parse_that_regex`) | 3.45% | called by `parse_string_direct` 355-359 (via `match_string_at_quote_trusted_utf8`) | gen.rs:~993 | already an **external named callee** (string leaf primitive) |
| `materialize_u64` (in `parse_that_regex`) | 0.53% | called by `emit_number_*` 447 | gen.rs:~1060 | already an **external named callee** (number leaf primitive) |

**Decisive profile insight:** the 91.5% is the byte-dispatch + inlined object/array recursion — i.e.
the dispatch/loop SKELETON, not an inner scanner. The actual inner scanners (`unescape_string`,
`materialize_u64`, `match_tiny_plain_string_direct`, `match_string_at_quote_trusted_utf8`) are ALREADY
external named functions (they live in `parse_that_regex` or in the template body, not inline in the
sink_direct emit). This is the load-bearing fact for §3 (the leaf primitives are already extracted) and
§4 (the hot leaf is structure, so it must be walk-derived, not blob-preserved).

---

## 2 — `SinkOnlyExpr` ↔ OUTPUT MAPPING

### 2.1 — How json.bbnf lowers to `SinkOnlyExpr`

Chain: `json.bbnf` → `grammar::parse_grammar` → `GrammarIr` → `passes::compile` → `BackendIr`
(`BackendExpr` per rule) → `lower::sink_only::lower_program` → `SinkOnlyProgram { rules:
Vec<SinkOnlyRule{name, expr: SinkOnlyExpr, direct_shape}> }`.

`passes::lower_expr` (`passes/src/lib.rs:1279-1311`) is a **faithful 1:1 structural map** of
`ExprKind` → `BackendExpr`:

```
Seq → Seq, Alt{branches,mode} → Alt{mode}, Repeat{body,min} → RepeatLoop, Optional → OptionalBranch,
Literal{bytes} → ByteLiteral, Regex{pattern} → RegexProgram{span_kind=backend_span_kind(pattern)},
Ref{name} → CallRule{callee=name}
```

Then `materialize_rule` (`passes:1313-1340`) wraps designated rules with semantic markers:

```
Seq[ SpanMark{Start,label}, body, SpanMark{End,label}, TapeEmit{kind}, DirectBuild{shape,fields}, Return ]
```

The designated rules come from `derive_materialization_roles` (`passes:1354-1442`), which detects roles
**structurally from the grammar** (this is the derivation that the `.bbnf`-mutation falsifier exercises):
- `string` = first rule with a String-kind regex (`/"…"/`).
- `number` = first rule with a Number-kind regex (`/-?…/`).
- `bool_value` = rule whose direct literals contain BOTH `"true"` AND `"false"` (`passes:1369-1375`).
- `null_value` = rule whose single direct literal is `"null"`.
- `container` = rule with both `{` and `}` literals; `sequence` = both `[` and `]`.
- `value` = rule referencing ≥4 of {container,sequence,string,number,bool,null}.
- `pair` = rule referencing string ∧ value ∧ a `:` literal.

`lower::sink_only::lower_expr` then maps `BackendExpr` → `SinkOnlyExpr` 1:1 (collapsing `SpanMark`→
`SpanMark`, `TapeEmit`→`TapeEmit`, `ValueProject`→`ValueProject`), accumulating facts (`direct_shapes`,
`span_kinds`, `literals`, `dispatch_alt_count`). The structural alphabet `{}[],:\"` is separately
derived by `recognizers::derive_recognizers` (`passes:332-349`).

**So the `SinkOnlyProgram` for json carries, per rule:** the full `SinkOnlyExpr` tree (Seq/Alt/Repeat/
Optional/ByteLiteral/RegexProgram/CallRule/SpanMark/TapeEmit/DirectBuild/ValueProject/Return), the
materialization shape+fields+TapeKind, the literal set, and `entry_rule=json`. This is confirmed by the
`direct_parser_is_authored_from_sink_only_lowering` test (`lib.rs:594-621`): `program.entry_rule=="json"`,
`has_shape("JsonObject"/"JsonNull")`, `has_literal(b"true"/b"false"/b"null")`.

### 2.2 — Per-node-kind: what Rust a faithful walk would emit

| `SinkOnlyExpr` node | Carries | Walk emits (target Rust) | Derivable from tree alone? |
|---|---|---|---|
| `Entry(inner)` | — | the `parse_direct` entry wrapper (skip-ws, call value, trailing check) | shell shape yes; the EXACT `parse_direct` idiom + cfg attrs = convention (see §4) |
| `Seq(children)` | child order | sequential statements / `?`-chained calls | yes (order from tree) |
| `Alt{mode:Dispatch, branches}` | branch exprs | a `match first_byte { … }` dispatch | **branch bodies yes; the byte→branch KEY must be DERIVED by first-set analysis (NOT in the node)** — see §4-C1 |
| `Alt{mode:Speculative, branches}` | branch exprs | ordered try-each-or-backtrack | yes (none in json's hot path) |
| `RepeatLoop{body,min}` | body, min | `loop { body; if take(',') {continue} else {expect(close); break} }` | loop skeleton yes; the comma/close idiom = convention |
| `OptionalBranch(inner)` | inner | `if peek != close { inner }` | yes |
| `ByteLiteral(bytes)` | bytes | `consume_direct(…, b'{', …)` / literal compare | **yes — the byte is in the node** (this is what makes the bool-drop falsifier fire) |
| `RegexProgram{span_kind,pattern}` | kind, pattern | call the span scanner (`parse_string_direct` / `parse_number_*`) | callee SELECTION yes (by span_kind); the scanner BODY is a named primitive (§3) |
| `CallRule{callee}` | callee name | `parse_{callee}_direct(…)?` | yes (name from node) |
| `SpanMark{label}` | label | (no emit in sink path — span markers are tape-only) | n/a for SinkOnly |
| `TapeEmit` | — | (no emit in sink path) | n/a for SinkOnly |
| `DirectBuild(shape,fields)` | shape, fields | the `sink.<verb>()` call selected by the rule's TapeKind | **verb yes (from TapeKind); the value/object/array PREFIX is NOT in the tree** — see §4-C2 |
| `ValueProject` | — | (projection helper — number materialization branch) | yes |
| `Return` | — | `Ok(())` | yes |

The house pattern `tape_plan.rs::render_expr` does exactly this recursion BUT emits an **abstract op
trace** (e.g. `"alt_dispatch_begin(3)"`, `"event_match_literal_hex(7b)"`), NOT compilable Rust. It is
the *shape* of the walk (the match arm per node kind), not the *output*. `json_typed_direct.rs::render`
is the closer model: it walks a `schema` tree (roots/types/fields) and `push_str`s **real Rust
fragments** derived per node (`render_field_arm`, `render_construct`, `rust_type`). G1's emitter is
structurally `json_typed_direct.rs` applied to `SinkOnlyExpr` instead of `DirectTypeKind`.

### 2.3 — How the walk reproduces the current `generated.rs` structure

A faithful walk over the json `SinkOnlyProgram` reproduces:
- **dispatch match** ← walking the `value` rule's `Alt{Dispatch}` (6 branches: object/array/string/
  number/bool/null), emitting a `match first_byte` whose arms come from each branch's first-set + body.
- **container loops** ← walking `object`/`array` rules: `ByteLiteral(b"{")` → open, the
  `member`/`elements` `RepeatLoop` → the comma loop, `ByteLiteral(b"}")` → close. The `begin_object`/
  `end_object` sink calls come from the container's `DirectBuild`/TapeKind.
- **literal arms** (`b't'`/`b'f'`/`b'n'`) ← the bool/null branches of the `value` Alt, each a
  `ByteLiteral` whose bytes (`true`/`false`/`null`) drive both the dispatch key (first byte) and the
  `consume_literal_direct(config::TRUE_LITERAL)` call. **Dropping the bool rule removes the bool branch
  from the `value` Alt → both arms vanish.** This is the falsifier's mechanism.
- **the 3 sink-prefix variants** (`parse_value_direct` / `parse_object_value_at_direct` /
  `parse_array_element_at_direct`) ← these differ ONLY by sink prefix (`sink.` / `sink.object_` /
  `sink.array_`). They are the SAME `value`-Alt walk emitted 3× with a `prefix` parameter — exactly the
  `render_number_emitter(out, name, prefix)` pattern already in `render_number_rules`. **This is the
  context that the tree does NOT carry (§4-C2); it must be a codegen-side loop over the 3 prefixes.**

---

## 3 — THE 2 LEAF PRIMITIVES (§6 (a)-(d) gate)

SPEC §6 / G1.3 admit exactly TWO named, `.bbnf`-invoked, grammar-derived-data, profile-narrow leaf
primitives. The structural SKELETON (dispatch match, container loops, literal arms, 3 sink-prefix
variants) is walk-derived (C1) and is NOT a primitive.

### 3.1 — Primitive (i): the `string`-rule leaf scanner

SPEC names it `decode_json_string_to_arena`. **Disk truth: that symbol does not exist anywhere in the
tree.** The actual string leaf is the pair:
- `match_tiny_plain_string_direct(bytes, start)` — defined in the **`json_templates/generated.rs`
  oracle body** (line 164), NOT emitted by `json_sink_direct.rs`. The fast `\`-free / quote-bounded
  early-out.
- `match_string_at_quote_trusted_utf8(bytes, start)` + `unescape_string` (Cow borrow) — in
  `parse_that_regex`. Profile leaf #3 (`unescape_string` 3.45%).

These are ALREADY external named callees. `parse_string_direct` (the emitted shell) merely calls them
and builds `ParsedString{raw, needs_unescape}`. **(d) is satisfied trivially**: the primitive *is* the
external scanner; its LOC (the scanner body) ≤ the profiled string-leaf extent; the surrounding
`parse_string_direct` shell is walk-derived (it's the `string` rule's `RegexProgram{String}` →
scanner-call).

### 3.2 — Primitive (ii): the `number`-rule leaf scanner

`parse_number_*` (3 prefix variants) → `match_number_span_from_first(bytes, cursor, first)` (the
`b'-'|b'0'..=b'9'` fast path) → `emit_number_*` → `materialize_i64`/`materialize_u64`/`materialize_f64`
(profile leaf #8 `materialize_u64` 0.53%). All in `parse_that_regex`. Same shape as (i): external named
callees, walk-derived shell.

### 3.3 — The (a)-(d) contract: HONEST status per arm

| Arm | Requirement (SPEC G1.3) | Disk truth / verdict |
|---|---|---|
| **(a) grammar-INVOKED-by-name** | "the `string`/`number` rule carries `-> decode_json_string_to_arena` / `-> parse_number_*`; the emitter reads the callee FROM the rule's `->`" | **NOT SATISFIABLE AS WRITTEN.** `json.bbnf` has ZERO `->` arrows (verified `grep -c '->' = 0`). There is no `->` to read a callee from. **However** the rule IS grammar-invoked by a different, already-implemented mechanism: the `string`/`number` rule is detected by `derive_materialization_roles` via its `RegexProgram{span_kind}`, and the span_kind SELECTS the scanner. So "grammar-invoked-by-name" holds via **span_kind selection** (a regex's kind names the scanner), not via a `->` annotation. The SPEC's `->` phrasing is aspirational; the real invocation seam is the span-kind→callee map. **This is a SPEC-language mismatch, not a feasibility blocker** — but it MUST be documented (§4 finding F2) and the (a) gate re-phrased to "the callee is selected from the rule's RegexProgram span_kind, not a literal", else a literal-checker keyed on `->` will false-fail. |
| **(b) emitted-output-VARIES under rule mutation** | "WIDEN the `number` rule's digit class in `.bbnf` → the `b'0'..=b'9'` literal in the emitted kernel WIDENS" | **PARTIALLY SATISFIABLE, with a caveat.** The dispatch first-byte set `b'-'|b'0'..=b'9'` IS derived from the `number` regex's first-set (so widening the digit class widens the DISPATCH arm — this varies). BUT the inner digit scan (`match_number_span_from_first`) lives in `parse_that_regex` as a FIXED hand-written function — its `b'0'..=b'9'` is NOT regenerated from the `.bbnf` regex. So under a number-class mutation, the *dispatch arm* widens (derived) but the *scanner body* does not (it's a stable named primitive, which is exactly what (c)+(d) permit). The honest reading: (b) is satisfied at the DISPATCH/ARG level (the first-set that keys the scanner-call), which is the SPEC's "emitted ARG byte-sets change" — NOT at the scanner-body level. This must be stated precisely (§4 finding F3): (b) tests the derived arg/first-set, not the primitive interior. |
| **(c) `verbatim_blob_present == false`** | no surviving `r#"..."#` / `_RS` body for the leaf | **SATISFIABLE.** The leaf scanners are `fn` calls, not raw-string blobs. (c) is a property of G1 as a whole (no `r#"..."#` in `json_sink_direct.rs`, `JSON_PARSE_ONLY_GENERATED_RS` deleted). |
| **(d) PROFILE-PROVEN-NARROW-LEAF** | primitive shell LOC ≤ profiled hot-leaf extent | **SATISFIABLE.** Both primitives are pre-extracted external scanners; their LOC is the scanner body (string-scan / digit-scan), categorically smaller than the 91.5% dispatch skeleton. `g1_leaf_primitive_loc <= g1_leaf_primitive_profiled_leaf_extent` holds because the primitives do NOT include the dispatch/loop skeleton (that's walk-derived). |

**Do NOT LCD-unify the value/object/array dispatch triple** — confirmed. The 3 variants differ only by
sink prefix; they must stay 3 monomorphized fns (the `{prefix}`-parameterized emit, NOT a unified fn),
because LCD-unify would re-introduce a runtime branch / devirtualize the sink and regress the 91.5%
(rC §5; `g1_dispatch_triple_not_lcd_collapsed` gate).

---

## 4 — FEASIBILITY VERDICT (the honest crux)

**Question:** can a clean `SinkOnlyExpr` walk reproduce the current `generated.rs` BYTE-EXACT, while
being genuinely grammar-derived?

**Verdict: ACHIEVABLE for the sink_direct path (source 3) and the bool-drop falsifier, but NOT
byte-exact for the parse-only courier (source 2) under a pure structural walk — and the SPEC's (a)-gate
`->`-arrow phrasing is mis-specified against the actual grammar. G1 is achievable WITH three documented
findings (F1–F3), at least one of which (F1) warrants a SPEC clarification.** The walk is the right
architecture; the byte-equivalence is reachable IF the emitter carries a small, named set of
codegen-side CONVENTIONS (the function-name idioms, cfg-attr shapes, the 3-prefix loop, the first-set
keys) that are derived-from-facts-plus-convention, not hand-stringified bodies. Those conventions are
exactly what `json_typed_direct.rs` already does (it hand-writes the `parser.ws()` / `parser.expect`
idiom strings while deriving the structure) — so they are house-pattern-legitimate, NOT the L1 REJECT,
PROVIDED the structure (which arms, which loops, which literals) comes from the tree.

### 4.1 — Structure-by-structure: derivable vs convention vs REJECT-risk

| Output structure | Source | Classification |
|---|---|---|
| dispatch `match byte` arm SET (which of object/array/string/number/bool/null) | the `value` `Alt{Dispatch}` branches | **(a) walk-derived** — falsifier-proven (drop bool ⇒ arms vanish) |
| the dispatch KEY per arm (`b'{'`, `b'"'`, `b'-'|b'0'..=b'9'`, `b't'`, `b'f'`, `b'n'`) | first-set of each branch's leading `ByteLiteral`/`RegexProgram` | **(a) walk-derived VIA a first-set pass** — the byte is in the node (ByteLiteral) or in the regex (span first char); a small `first_byte_set(expr)` helper computes it. Finding **F1**. |
| container `{`/`[` loops + comma handling | `object`/`array` `RepeatLoop` + `ByteLiteral` | **(a) walk-derived** (loop skeleton) + **(b) convention** (the exact `take_direct(b',')`/`consume_direct(b'}')` idiom) |
| `sink.begin_object`/`end_object`/`begin_array`/`end_array` | container `DirectBuild`/TapeKind=Container/Sequence | **(a) walk-derived** (verb from TapeKind) |
| `sink.<string\|bool\|null\|number>` value verbs | each value branch's TapeKind | **(a) walk-derived** (verb from TapeKind) |
| **the value/object/array PREFIX** (`sink.` vs `sink.object_` vs `sink.array_`) | **NOT IN THE TREE** | **(c) convention** — a codegen-side 3-prefix loop. The grammar has ONE `value` rule; the positional context (top-level vs object-member vs array-element) is a codegen specialization, NOT a grammar fact. Finding **F2** (this is the SAME pattern `render_number_emitter`'s `prefix` already uses — legitimate house convention, NOT a blob). |
| fn names (`parse_object_value_at_direct`, `parse_number_direct`, …) | rule name + role + prefix | **(b) convention** — name template `parse_{role}{_context}_direct`. The `parse_w11_1_number` metalang leak this would have re-introduced is ALREADY GONE (§4.3). |
| `#[cfg_attr(feature="parse-attribution", inline(never))]` cfg pairs | fixed per-fn | **(b) convention** — an attribute-emit helper; the hot-leaf gate (`g1_hot_leaf_preserved`) requires these byte-identical, so they're a fixed convention string. |
| `parse_direct` entry idiom, `consume_direct`/`take_direct`/`direct_error` utility fns | fixed | **(b) convention** — runtime-support fns emitted as fixed fragments (like `json_typed_direct.rs`'s `PARSER_RUNTIME` constant at the tail). NOT grammar-varying, so a fixed-fragment emit is correct and honest (these are the parser's runtime support, analogous to `tape_plan`'s helper ops). |
| **the parse-only courier** (`ParseOnlyState`, `parse_only_value_iterative`, the iterative stack-frame machine) | source (2), a 353-LOC `r#"..."#` blob | **(c)-RISK / the honest hard problem** — see §4.2. |

### 4.2 — The hardest byte-equivalence challenge: the parse-only courier (source 2)

The mandate requires folding `JSON_PARSE_ONLY_GENERATED_RS` into the SAME walk. But the parse-only
parser is **architecturally different** from the sink_direct parser: it is an **iterative, explicit-
stack frame machine** (`ParseOnlyFrame::{ObjectExpectKeyOrEnd, ObjectExpectKey, ObjectAfterValue,
ArrayAfterValue}`, a `Vec<ParseOnlyFrame>` driven `while let Some(frame)` loop) that does NO sink emit
(it's recognize-only — the 51/51 `parse_only` recognizer). The sink_direct parser is **recursive
descent with monomorphized sink calls**. These are TWO DIFFERENT LOWERINGS of the same grammar:
- sink_direct = recursive-descent + DirectBuild sink emit.
- parse_only = iterative-stack + no emit (a `ValueProject`-stripped recognizer).

A single `SinkOnlyExpr` walk produces ONE shape. To emit BOTH from the same walk byte-exact, the walk
must be **parameterized by an emit-policy** (sink-emitting recursive vs recognize-only iterative). The
iterative-stack transformation (recursive descent → explicit frame stack) is a non-trivial CPS/
defunctionalization that the current `SinkOnlyExpr` tree does NOT encode — there is no node that says
"materialize this Alt as an explicit frame enum". **This is the genuine §6 finding (F3): the parse-only
courier is not byte-derivable from the `SinkOnlyExpr` tree by a structural walk without either (x)
adding an iterative-lowering policy to the walk, or (y) accepting that the parse-only path is a SECOND
emit-policy of the same walk that produces the iterative shape, which is a substantial codegen
addition, or (z) re-deriving parse_only as the SAME recursive shape as sink_direct minus the sink calls
(which would CHANGE its bytes — breaking byte-equivalence against the shipped courier).**

**Recommendation on the courier:** the honest, KISS path is **(z-relaxed)** — fold the parse-only path
into the walk as the **recognize-only emit policy** (the same recursive-descent walk with sink calls
suppressed), and **relax the courier's byte-equivalence target from "byte-identical to the current
iterative `JSON_PARSE_ONLY_GENERATED_RS`" to "behaviorally-equivalent recognizer, 51/51 held, regenerated
from the walk"**. The current iterative-stack courier is itself hand-written (it is the blob the campaign
exists to retire); demanding the WALK reproduce a hand-written iterative machine byte-exact would force a
"grammar-driven banner over a hand-written iterative body" — the EXACT L1 REJECT. So byte-equivalence
against the courier is the wrong gate; the right gate is: parse_only is walk-DERIVED (recognize policy),
`verbatim_blob_present==false`, 51/51 recognizer rows held, and `regen --check` clean. **This is a SPEC
amendment request (see §4.4).** The sink_direct path (source 3) DOES retain hard byte-equivalence against
the shipped file (that's the 91.5% product and must not move a byte).

### 4.3 — Findings that are NOT blockers (already-resolved or convention)

- **F-resolved (metalang leak / P5):** SPEC §3.5 + §4 claim `parse_w11_1_number` ships 7× at
  `json/generated.rs:801/841/881/…`. **Disk truth at HEAD `83b66db42`: `grep -c parse_w11_1_number
  json/generated.rs == 0`** — the shipped file already uses `parse_number_direct`/`_object_`/`_array_`.
  The metalang leak is ALREADY GONE at the source (`json_sink_direct.rs` emits `parse_number_*`). P5 is
  effectively pre-satisfied for JSON; G1's `g1_metalang_leak_count==0` re-assertion will pass trivially
  on the regenerated file. **The SPEC's line-number citations for the metalang leak are stale.** (The
  P5↔G1 sequencing note still holds in principle, but there is no surviving `parse_w11_1_number` to
  rename.)
- **F2 (the 3-prefix loop):** the value/object/array prefix is a codegen convention, not a tree fact —
  but it is the SAME legitimate pattern `render_number_emitter(prefix)` already uses, and
  `json_typed_direct.rs` hand-writes idiom strings while deriving structure. House-legitimate.
- **The structural alphabet, direct_shapes, dispatch_alt_count, entry_rule** are already grammar-derived
  (verified). No new derivation needed for those.

### 4.4 — Recommendation: ACHIEVABLE with a documented SPEC amendment

G1 is achievable as a `SinkOnlyExpr` AST-walk emitter. The plan needs THREE documented carries into the
CHALLENGE/dispatch packet:

1. **AMENDMENT REQUEST (binding):** relax the parse-only courier (source 2) byte-equivalence target. The
   mandate's "byte-equivalent to the current shipped generated.rs" should be SPLIT:
   - **sink_direct path (source 3, the 91.5% product): HARD byte-equivalence retained** (the regenerated
     `parse_*_direct` fns diff EMPTY vs shipped).
   - **parse-only path (source 2): STRUCTURAL+BEHAVIORAL equivalence** — walk-derived recognize-only
     policy, `verbatim_blob_present==false`, 51/51 `parse_only` recognizer rows held, `regen --check`
     clean. Byte-equivalence against the hand-written iterative courier is NOT required (demanding it
     forces the L1 REJECT). This is the honest §6 finding F3.
   - **template (source 1, the tape oracle): retained as the byte-equivalence ORACLE for the value
     scanners, then deleted** — its `match_tiny_plain_string_direct` etc. are the leaf primitives the
     walk CALLS, not re-derives. If subsuming the template into the walk is out of G1's ≤450-LOC budget,
     keep it as a `parse_that_regex`-style external-fn module (named, diff-audited), NOT a `r#"..."#`
     blob — that satisfies `verbatim_blob_present==false` without re-deriving the scanner internals.
2. **CLARIFICATION (F1):** the (a) gate's "`-> decode_json_string_to_arena`" `->`-arrow mechanism does
   not match json.bbnf (zero arrows). Re-phrase (a) to: "the leaf scanner is SELECTED from the rule's
   `RegexProgram` span_kind (string/number), machine-checked by span_kind→callee mapping" — the existing,
   real invocation seam. The primitive name should be the actual `match_string_at_quote_trusted_utf8` /
   `match_number_span_from_first` (+ `unescape_string`/`materialize_u64`), NOT the non-existent
   `decode_json_string_to_arena`.
3. **PRECISION (F3 (b)):** the (b) falsifier ("widen digit class ⇒ `b'0'..=b'9'` widens") fires at the
   DISPATCH first-set / scanner-call arg level (which IS derived), NOT inside the stable
   `match_number_span_from_first` primitive body (which is a fixed `parse_that_regex` fn — and is allowed
   to be, by (c)+(d)). State this so the falsifier targets the derived arg, not the primitive interior.

If the amendment is rejected and HARD byte-equivalence against the iterative courier is insisted upon,
G1 is **NOT achievable as a pure structural walk** — it would require adding an iterative-lowering
(defunctionalized-stack) emit policy to the `SinkOnlyExpr` walk, which is a substantial codegen addition
likely exceeding the ≤450-LOC budget and the 90-min wall, and risks shipping a banner-over-blob for the
parse_only path. The honest engineering recommendation is the amendment.

---

## 5 — IMPLEMENTATION STEP PLAN (do NOT execute in this research wave)

Edit surface: `crates/codegen/src/json_sink_direct.rs` (rewrite), `runtime_generator.rs` (delete
`JSON_PARSE_ONLY_GENERATED_RS`, fold parse_only into the walk), plus a new walk helper module. Budget
≤450 hand LOC. The net generated delta is ≈0 (sink_direct path must reproduce its bytes).

**Step 0 — pre-flight (no code):** confirm P-cluster green (P4 live, P5 closed). Capture the shipped
`json/generated.rs` as the byte-equivalence baseline. Confirm `direct_parser_is_authored_from_sink_only_lowering`
+ `emission_is_deterministic` are the harness (`lib.rs:482,594`).
Verification: baseline captured; tests located.

**Step 1 — introduce the walk skeleton (no behavior change yet).** Add a `SinkWalk`/`Emitter` struct to
`json_sink_direct.rs` mirroring `json_typed_direct.rs::Renderer`: it holds the `SinkOnlyProgram` and a
`first_byte_set(&SinkOnlyExpr) -> Vec<DispatchByte>` helper (F1). Write a `render_expr(&mut self, expr,
prefix, out)` that matches every `SinkOnlyExpr` variant (the §2.2 table) and `push_str`s the real Rust
fragment — initially emitting the SAME bytes as the current fixed literals (transcribe, then derive).
Verification: emitted `generated.rs` byte-diffs EMPTY vs shipped (the literals haven't moved yet; this
proves the walk plumbing is wired without changing output).

**Step 2 — derive the dispatch arms from the `value` Alt (the falsifier target).** Replace the fixed
`match byte { … }` in `render_value_dispatch` with a walk over the `value` rule's `Alt{Dispatch}`
branches: per branch, compute `first_byte_set` (F1), emit the arm key + the branch body (container →
recurse, string/number → primitive-call shell, bool/null → `consume_literal_direct` + `sink.<verb>`).
Drive the `sink.<verb>` from the branch's TapeKind. Verification: (a) byte-diff EMPTY vs shipped;
(b) THE FALSIFIER — temporarily drop the `bool` rule from `grammars/json.bbnf`, regen, confirm the
`b't'`/`b'f'` arms VANISH from the emitted dispatch, revert. `g1_bbnf_mutation_falsifier_fires==true`.

**Step 3 — the 3-prefix loop (F2).** Replace the 3 hand-written dispatch fns with a `for prefix in
["", "object_", "array_"]` loop emitting `parse_value_direct` / `parse_object_value_at_direct` /
`parse_array_element_at_direct` from the SAME `render_expr(value_alt, prefix)`, threading the prefix into
`sink.{prefix}<verb>`. Verification: byte-diff EMPTY vs shipped (all 3 fns reproduce exactly, including
the `#[cfg_attr…]` pairs); `g1_dispatch_triple_not_lcd_collapsed==true` (3 distinct fns remain).

**Step 4 — container loops + string/number shells from the tree.** Derive `parse_object_direct`/
`parse_array_direct` from the `object`/`array` `RepeatLoop`+`ByteLiteral` nodes; derive
`parse_string_direct`/`parse_number_*` shells from the `string`/`number` `RegexProgram{span_kind}` →
primitive-call (F1 (a) via span_kind selection; the scanners stay external named callees).
Verification: byte-diff EMPTY vs shipped; the 4 leaf scanners (`match_tiny_plain_string_direct`,
`match_string_at_quote_trusted_utf8`, `match_number_span_from_first`, `materialize_u64`) remain external
fn calls (not inlined blobs).

**Step 5 — fold the parse-only courier (the F3 hard step; per §4.4 amendment).** DELETE
`JSON_PARSE_ONLY_GENERATED_RS` (and its `runtime_generator.rs:35` splice). Emit `parse_only` from the
SAME walk under a `EmitPolicy::Recognize` (sink calls suppressed, recursive-descent shape). Verification:
`JSON_PARSE_ONLY_GENERATED_RS` grep == 0; `verbatim_blob_present==false`; 51/51 `parse_only` recognizer
rows held; `regen --check` clean. **NOTE:** under the amendment, the parse_only BYTES change (recursive
vs iterative) — record this as the F3 honest finding, NOT a byte-equivalence failure. If the amendment is
NOT granted, this step is the blocker (§4.4) and G1 returns REVISE with the amendment request.

**Step 6 — the leaf primitives (a)-(d) telemetry.** Emit `g1_leaf_primitive_count==2`,
`g1_leaf_primitive_loc` (scanner body LOC), `g1_leaf_primitive_profiled_leaf_extent` (the 3.45% / 0.53%
extents), and the per-primitive `g1_leaf_primitive_abcd_pass` with (a) re-phrased to span_kind selection
(F1), (b) targeting the derived first-set/arg (F3), (c) blob-free, (d) LOC≤extent. Verification: all
abcd arms green; `g1_leaf_primitive_loc <= g1_leaf_primitive_profiled_leaf_extent`.

**Step 7 — full gate refresh + byte-equivalence proof.** Regen, run the byte-equivalence diff-control
(`EmittedSource::check_dir`) for the sink_direct path against (a) oracle + (b) shipped BEFORE deleting
the oracle. Confirm hot-leaf preservation (`parse_object_value_at_direct`/`parse_array_element_at_direct`
present, same inline cfg, same `sink.object_*`/`sink.array_*` call sites). Run JSON 51/51 strict-vs-
sonic-rs within ±1.0% of SK-V18-open. Emit the full G1.4 telemetry. Verification: every G1 gate
column green; `g1_json_generated_byte_equivalent==true` (sink_direct path); `g1_hot_leaf_preserved==true`;
`g1_json_guard_rows_held==true`. Then delete the `json_templates/` oracle (or relocate its scanners to a
named external module per §4.4).

**Revert slice:** the `json_sink_direct.rs` rewrite + `runtime_generator.rs` courier-deletion + the
regenerated `json/generated.rs`, reverted together; restore the oracle; record a G1 REDRESS naming the
failed gate.

---

## 6 — Risk register (carry into CHALLENGE)

| Risk | Severity | Mitigation |
|---|---|---|
| Parse-only courier not byte-derivable by structural walk (iterative-stack ≠ recursive-descent) — F3 | **HIGH** | The §4.4 amendment (structural+behavioral equivalence for parse_only, hard byte-equiv only for sink_direct). If rejected → REVISE. |
| (a)-gate `->`-arrow phrasing mismatches json.bbnf (zero arrows) — F1 | MED | Re-phrase (a) to span_kind→callee selection; document in dispatch packet. |
| (b) falsifier targeting the wrong layer (scanner interior vs derived arg) — F3 | MED | Target the dispatch first-set / scanner-call arg, not the stable `parse_that_regex` primitive body. |
| LCD-collapsing the 3-prefix dispatch triple → 91.5% regression | HIGH | `g1_dispatch_triple_not_lcd_collapsed` gate; the prefix loop emits 3 monomorphized fns (Step 3). |
| First-set helper mis-derives a dispatch key (e.g. number's `b'-'|b'0'..=b'9'`) → byte-diff fails | MED | Step 2 byte-diff EMPTY gate catches any key drift before the falsifier. |
| SPEC stale line-numbers (metalang leak already gone; `generated_json` dir name) mislead the implementer | LOW | §4.3 records disk truth; the implementer trusts grep over SPEC citations. |
| Subsuming the 391-LOC template oracle blows the ≤450 LOC budget | MED | Per §4.4, keep the scanners as an external named module (not re-derived), not a blob — satisfies `verbatim_blob_present==false` within budget. |
