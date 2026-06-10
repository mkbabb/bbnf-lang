# 22 — The Unified Value API: Census, Design, and the W3/W4 Subsumption

Audit of the value layer as a first-class requirement: one grammar-derived
Value plane offering lazy cursor navigation AND eager materialization over a
SINGLE tape, with a path/query API and direct-to-struct bridge that are derived
from each grammar — not JSON-hardcoded. The finding is that no such unified
layer exists; instead there are **three divergent value substrates, four
disjoint path/query implementations, and a dead direct-to-struct trait pair**,
most of them JSON-shaped. The design that subsumes SK-V19 W3/W4 is at the end.

## 1. The value-substrate census: three impls, two tape encodings

There is no single value plane. There are three, over two incompatible tapes:

1. **Skinny lazy offset-tape cursor** — `skinny/crates/runtime/src/tape/mod.rs`.
   One `Tape<'input>` = `{ source, offsets: Vec<u32>, sparse flag plane
   (flag_cursors/flag_values), payloads }` (`mod.rs:92-99`); cursor =
   `ValueRef<'doc,'input,K>` = `(&Tape, u32)` (`mod.rs:173-178`). Kind decodes
   lazily from the flag plane (`grammars/sheets/view.rs:122-128`). This is the
   clean substrate the SK-V18 R-D research grounds the API on.

2. **Core eager arena `JsonValue` enum** — `crates/core/src/runtime/json/value.rs:3-11`.
   A fully eager, owned 6-variant `JsonValue<'p>` with side-table arenas
   (`JsonArrayId`/`JsonObjectId`, `arena.rs:3-30`). Containers are *materialized
   into `Vec<JsonValue>` / `Vec<JsonPair>` slabs* (`arena.rs:31-40`) — the exact
   opposite of laziness. This is the core's production JSON output.

3. **Core post-order `TapeCursor`** — `crates/core/src/runtime/tape/cursor.rs:24-37`.
   A THIRD encoding: `TapeRec` records in *post-order* with a payload arena,
   children recovered by backward walk (`cursor.rs:1-12`). Different tape from
   skinny's: skinny indexes a `Vec<u32>` offset stream with a sparse flag
   sidecar; core walks typed `TapeRec` records post-order. NOT the same
   substrate despite both being called "tape."

The R-D research (`sk-v18/research/p2/rD-value-api-trait.md:9-17`) claims "ONE
`Tape<'input>`" and "both value APIs are cursors-over-offsets." True *within
skinny*, false across the campaign: core's production JSON path is the eager
arena (#2), and core's tape (#3) is a different record layout. The unified claim
is scoped to skinny and silently does not hold for the totality tree.

## 2. The navigation asymmetry is real — the shared trait is deliberately thin

Even within skinny, two grammars diverge in navigation SHAPE, and this is the
load-bearing constraint:

- **JSON = recursive tree** (`grammars/json/view.rs`): `JsonObject::get(key)`
  (`:106`), `pairs()` (`:90`), `JsonArray::values()` (`:147`), the typed
  `JsonValue` enum + `value_from_ref` byte-dispatch (`value.rs:143`), recursive
  `walk_value` visitor. Containers nest; `next_sibling_cursor` (`view.rs:355`)
  does the depth-walk.
- **Sheets = flat node sweep** (`grammars/sheets/view.rs`): `nodes()` is a linear
  `(0..offsets.len()).map(GoogleSheetsNode)` (`:252-255`); the only verbs are
  `as_f64`/`as_bool`/`as_tag` over a node span (`:167-203`). NO `get(key)`, NO
  keyed nesting, NO recursion.

The `Cursor` trait (`tape/cursor.rs:20-36`) shares ONLY `{tape, cursor, offset,
kind}` with an *associated* `type Kind` so each grammar keeps its own kind enum.
It holds NO navigation verb; the doc comment (`cursor.rs:5-19`) calls this
"non-collapsible by construction." This was the correct R-D decision (Candidate
A, `rD:67-90`): a wider trait can only unify JSON and Sheets by flattening JSON's
keyed tree (the `preserve-rich-ast` REJECT). **But the consequence is that "the
unified Value API" is a 4-method laziness primitive plus a per-grammar
hand-divergent navigation surface — not a value API in the simdjson/serde
sense.** The honest framing must say so; calling it a "unified Value API"
oversells a cursor-dedup.

## 3. The generated view emitter proves navigation is NOT derived

`skinny/crates/codegen/src/general_descent/view.rs` is the ONE generalized view
emitter. From each grammar's `->` projections it derives: the `{Prefix}Kind`
enum, a `KIND_TABLE`, scalar typed accessors (`as_f64`/`as_bool`/`as_tag`,
`view.rs:118-184`), the `TAG_TABLE` (`:190-250`), `{Prefix}Node`/`{Prefix}Document`,
and a flat `nodes()` sweep (`:399-402`). It emits **NO keyed navigation, NO
container nesting, NO recursion, NO typed value-enum**. The rich JSON tree
(`get`/`pairs`/`values`/`JsonValue`) is hand-authored in the SEPARATE
`json_templates` path (`skinny/crates/codegen/src/json_templates/view.rs`),
which carries a FALSE `@generated` header (`json_templates/view.rs:1`) yet is a
hand-written courier.

So there are two emit paths: `general_descent` (derived, flat) and
`json_templates` (hand-authored, rich). SK-V19 W3 exists precisely to retire
`json_templates` (`sk-v19/SPEC.md:84-85`). It has NOT landed: `json_templates`
is still the JSON emit path. **The generalized emitter cannot today produce
JSON's rich navigation — it has no derivation rule for keyed/recursive container
access.** That is the true W3 gap.

## 4. Four disjoint path/query implementations — all JSON-pointer-shaped

A "path" should mean different things per grammar — a JSON pointer, a CSS
selector chain, a Sheets range. It does not. There are four separate path APIs,
all built on the same JSON/jq segment alphabet, none derived from the grammar:

1. **`crates/bbnf-path`** — a `path!` proc-macro (`bbnf-path/src/lib.rs:72-78`)
   validating `Field/Index/Wildcard/VariantName` segments
   (`path_macro.rs:58-61`) against a per-grammar `StructRegistry` sidecar JSON
   (`registry.rs:53-78`). Segments are JSON-pointer-shaped; the sidecar is loaded
   from disk via `std::fs` to dodge a dep cycle (`registry.rs:14-23`) — a
   structural smell.
2. **`crates/core/src/path/`** (2234 LOC) — `TypedPath<G,T>` + `PathExecutor` +
   `PathCursor` lazy-bail-out. Same `Field/Index/Wildcard` alphabet. The
   per-grammar `parse_with` routes "land in W3.2" (`executor.rs:6-9`); only
   `runtime/bbnf/parse_with.rs` and `runtime/google_sheets/parse_with.rs` are
   wired. The module carries **inline `#[cfg(test)]` blocks** (`schema.rs:130`,
   `cursor.rs:313`, `executor.rs:65`) violating no-inline-tests, and every test
   fixture is a `TypedPath<Json,...>` (`executor.rs:71-77`).
3. **`crates/core/src/runtime/json/document.rs`** — a `JsonPathQuery` trait
   (`document.rs:131`) with `get<T: JsonPathQuery>(path)` (`:48`) and impls for
   `&str`/`f64`/`bool`/`JsonValue` (`:156-187`). JSON-NAMED, JSON-only, distinct
   from #2's `TypedPath`.
4. **`crates/bbnf-path-ts`** — a WASM `execute_path` that walks a
   `serde_json::Value` (`bbnf-path-ts/src/lib.rs:77-82, 121-128`), NOT the tape.
   A JSON-document executor disconnected from the grammar-derived value layer.

None expresses a CSS selector or a Sheets range. The path alphabet is
JSON-pointer-hardcoded across all four. A grammar-derived path would let CSS mean
selector-combinator descent and Sheets mean A1-range — the JSON-overfit the
precept forbids, present in four places at once.

## 5. Direct-to-struct: a dead, JSON-hardcoded trait pair

`crates/ser` defines `Serializer`/`Deserializer` (`ser/src/traits.rs`). The
`Deserializer` trait (`traits.rs:113-129`) and its sole impl `SliceDeserializer`
(`slice.rs`) have **ZERO consumers** — nothing in `crates/` or `skinny/` calls
`text_span`/`i64`/`f64`. Worse, `SliceDeserializer::text_span` hardcodes the JSON
delimiter set `{ } [ ] , :` (`slice.rs:38-41`): a "grammar-guided" deserializer
that only knows JSON punctuation. The direct-to-struct requirement (a
grammar-derived serde `Deserialize` or bespoke derive) is **unimplemented**; what
exists is dead JSON-shaped scaffolding. The `Serializer` side is barely better:
`generate/serialize/mod.rs:14-40` emits per-rule functions calling ONLY
`__ser.text(span_text())` — a verbatim round-trip, never using the rich
`i64`/`f64`/`group_open`/`sep` surface (`traits.rs:35-109`). The trait
advertises a typed serializer; codegen wires a span-copier. The whole trait
surface is aspirational dead weight.

## 6. The honesty constraint the unified API must not break

The CSS >SOTA claim rests on `track1_rich` being a LAZY, value-HEAD projection
(`sk-v18/waves/h1/FRAMING.md:6-21`): it classifies the leading significant token
of each declaration and counts selectors by comma, but does NOT walk the full
value subtree lightningcss materializes. Two asymmetries are disclosed —
retention (lazy vs eager) AND depth (head vs full tree, `FRAMING.md:16-21`). This
is the trap for any "match-and-beat-competitors" Value API: simdjson OnDemand,
sonic-rs get-by-path, and yyjson DOM all expose full-depth navigation. If the
unified API forces full-depth eager materialization to reach feature parity, the
>SOTA ratio collapses — the speed advantage is *partly* the shallower typed
product. **The design must keep depth a lazy, opt-in, pay-as-you-go axis, never a
materialization mandate.** Any "we now match simdjson DOM" framing that quietly
re-deepens the typed product is a REJECT.

## 7. The unified design — one grammar-derived value plane, three axes

The honest unification is not one fat trait; it is one *substrate* plus a
*derivation* that emits per-grammar navigation, with eager as a fold over lazy.

**Axis A — one tape (close the substrate fork).** Pick skinny's offset+flag-plane
`Tape` as the single substrate; DELETE core's post-order `TapeCursor` encoding
(#3) and the eager `JsonValue` arena (#2) as the production output. Both are
pre-restart residue the SK-V19 W2 SoA-tape wave should reconcile against. This is
the prerequisite W3 leans on but does not own.

**Axis B — derive navigation, not just scalars (the W3 deficit).** The
`general_descent` view emitter must gain a derivation for *container access*,
keyed on grammar STRUCTURE (not rule names): a rule whose body is a
`(key value)*` repetition under a delimited group derives `get(key)`/`pairs()`
(JSON object, Sheets named-arg list); a rule that is a `value*` repetition
derives `values()` (JSON array, Sheets array-row); a rule that is a pure
alternation of scalars derives only typed accessors (today's flat path). The
rich/flat split becomes a *derived consequence of grammar shape*, not a
hand-authored `json_templates` fork. JSON's `get`/`pairs`/`values` then fall out
of the SAME emitter that produces Sheets' flat sweep — because JSON's grammar IS
keyed-repetition and Sheets' formula grammar is not. This is the un-courier W3
mandates (`SPEC.md:84-85`) made structural.

**Axis C — eager = a fold over lazy; never a second substrate.** Eager
materialization is a generic `fold` over the lazy cursor: `materialize(cursor) ->
OwnedValue` walks the derived navigation once and collects. ONE lazy plane, ONE
fold; the eager `JsonValue` enum becomes the fold's output type (still
grammar-derived), not an independent parser output with its own arena.
Competitors' DOM (yyjson, simdjson DOM, serde owned) map onto this fold;
OnDemand/LazyValue map onto the bare cursor. Both over the same tape — the
first-class requirement, finally met.

**Path/query (one derived alphabet).** Collapse the four path APIs to one. A
"path segment" is derived from the grammar's navigation surface: keyed access
admits `Field`; indexed repetition admits `Index`; alternation admits `Variant`;
CSS's selector grammar admits a combinator segment, Sheets' a range segment. The
segment alphabet is a function of the derived navigation (Axis B), so the path
API is automatically per-grammar without JSON-hardcoding. `bbnf-path`
(proc-macro), `core/src/path`, `JsonPathQuery`, and `bbnf-path-ts` collapse to
this one derivation with thin front-ends (proc-macro / runtime / WASM) over the
SAME segment set.

**Direct-to-struct (one bridge).** Replace the dead `ser::Deserializer` with a
grammar-derived bridge: the lazy cursor already exposes typed leaves
(`as_f64`/`as_bool`/span); a derive walks the cursor and assigns into a
`#[derive(Deserialize)]` target by field-name match against the derived keyed
navigation. This is `serde::Deserialize` *over the cursor*, not over a
JSON-delimiter byte scanner — reusing Axis B's keyed surface, so it works for any
grammar whose shape admits keyed access. The JSON `{ } [ ] , :` byte-set
(`slice.rs:38-41`) is deleted.

## 8. How this subsumes SK-V19 W3 and W4

- **W3** (`SPEC.md:81-92`) is exactly Axis B + Axis C + un-couriering
  `json_templates`. W3's exit (`generator_grammar_branch_count == 0`,
  `verbatim_blob_present == false`, JSON `value_from_ref` byte-equal) is met by
  deriving navigation structurally so the JSON courier deletes and one generator
  emits all grammars. This report adds what W3 under-specifies: the *derivation
  rule* for keyed/indexed/scalar navigation from grammar shape — without it, W3
  "one generator" still cannot reproduce JSON's `get`/`pairs` and regresses to a
  flat sweep (the W3 falsifier, `SPEC.md:91-92`).
- **W4** (`SPEC.md:94-100`) is the compile-time fence: resolve every
  `StructLayout` ONCE at codegen, removing the runtime `REGISTRY:
  LazyLock<StructRegistry>` breach (`generated/json.rs:276`) and the per-leaf
  `compound_kind_for_layout` coupling. Axis B's derived navigation is what W4
  fences: container access emitted as concrete code (not resolved per-leaf
  against a runtime registry) drives `StructRegistry` hot-path self-time to zero
  by construction. The `bbnf-path` sidecar-JSON dance (`registry.rs:14-23`) — a
  proc-macro reading layout from disk — is the same runtime-layout breach in the
  path plane and should be fenced identically: the derived segment alphabet is a
  compile-time const, not a disk read.

The two waves are one move: derive the navigation (W3), then prove the layout
that backs it is resolved at codegen, never at runtime (W4). The Value API is the
artifact both produce; today it is three substrates, four path APIs, and a dead
deserializer, most of them wearing JSON's clothes.
