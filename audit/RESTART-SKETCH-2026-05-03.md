# Restart Sketch — bbnf-lang BA-Restart

Date: 2026-05-03
Repo: `/Users/mkbabb/Programming/bbnf-lang`
HEAD: `b9863bf3` (master)
Audience: user, post-hardening, pre-from-scratch sequencing.

This document has two parts. **Part A** traces the actual JSON parse path
that executes today on `{"a": 1}`, marks every wasted cycle, and answers
"what is `OpenFrame` and why does its `Vec`-clone dominate samply at
86.07% inclusive". **Part B** sketches the post-restart architecture from
first principles — layered, multi-backend, direct-to-struct — and ends
with seven open architectural questions for the user to resolve.

This is a **sketch + trace**, not an execution plan.

---

## PART A — Trace the real parse path

### A.0 Fixture and entry surface

Input: `{"a": 1}`. Grammar: `grammar/json/json.bbnf` (8 rules: `null`,
`bool`, `number`, `string`, `pair`, `object`, `array`, `value`).

Public entry surface:

| Surface | Symbol | File:Line | Purpose |
|---|---|---|---|
| Eager parse → document | `JsonParser::parse(input)` | `crates/core/src/grammar/generated/json.rs:3434` | "give me the typed tree" |
| Lazy path-driven parse | `runtime::json::parse_with(input, &TypedPath)` | `crates/core/src/runtime/json/parse_with.rs:77` | "give me one leaf" |
| Document accessor | `JsonDocument::get<T>(path)` | `crates/core/src/runtime/json/document.rs:156` | post-parse leaf walk |
| Document view | `JsonDocument::view()` | `crates/core/src/runtime/json/document.rs:123` | typed-tree handle |

**The eager path is the lazy path with `__EAGER_EMPTY_PATH`.** That is
the substrate `feedback_no_orthogonal_codepaths` invariant — but it is
only honoured inside the dispatcher; the value-API hot path
(`JsonDocument::get<T>` post-eager-parse) is a *second* operation that
walks the materialized AST. This is BA's `bbnf_get_twitter` 4196× gap
(`docs/tranches/BA/BA.md:22`).

### A.1 Control-flow graph for `{"a": 1}`

```
JsonParser::parse(input="{\"a\": 1}")                           generated/json.rs:3434
  ├─ input.as_bytes()                                            generated/json.rs:3440
  ├─ ScanState::new()                                            generated/json.rs:3441 → 707
  ├─ JsonStructBuilder::new()                                    generated/json.rs:3442 → json/builder.rs:131
  │   ├─ JsonArena::new()                                        json/arena.rs:107   ─── (alloc: 2× empty Vec)
  │   └─ stack: Vec::with_capacity(8)                            json/builder.rs:135 ─── (alloc: 8× OpenFrame)
  ├─ static __EAGER_EMPTY_PATH: LazyLock<TypedPath<Json,&str>>   generated/json.rs:3443 ─── (one-time global)
  ├─ PathCursor::new(&EMPTY_PATH, |_,_,_| ParseFully)            generated/json.rs:3448
  └─ parse_JsonParser_value(bytes, &mut 0, &mut state, &mut builder, &mut cursor)
      │                                                          generated/json.rs:2177
      └─ parse_JsonParser_value__value                           generated/json.rs:2195
          ├─ skip_space(input, &mut p, &mut state)               generated/json.rs:2205, 739
          │   └─ first byte = b'{', no slow path                 generated/json.rs:744
          └─ match first { b'{' => parse_object_… }              generated/json.rs:2210
              │
              parse_object_JsonParser_object                      generated/json.rs:1495
                ├─ check b'{' at *p                              generated/json.rs:1507
                ├─ allocate __layout: StructLayout {              generated/json.rs:1512  ─── (alloc: String("object") + empty Vec)
                │     rule_id:4, rule_name:String::from("object"),
                │     kind: Struct, rule_type: TypeDesc::Span,
                │     fields: Vec::new() }
                ├─ builder.begin_compound(&__layout)             generated/json.rs:1519 → json/builder.rs:261
                │   └─ match (kind, rule_id) → OpenFrame::Object  json/builder.rs:276
                │       └─ stack.push(Object{pairs:Vec::new(), pending_key:None})
                │                                                 ─── (alloc: empty Vec on each push)
                ├─ *p += 1; skip_space                           generated/json.rs:1520
                ├─ peek b'}' branch (not taken on "{\"a\":...")  generated/json.rs:1522
                ├─ cursor.decide(rule_id=4) = ParseFully          generated/json.rs:1527
                │
                │   LOOP for each pair:
                │   ├─ cursor.current_kind() = Wildcard (empty path)  generated/json.rs:1530
                │   ├─ __is_field_seg = false                    generated/json.rs:1531
                │   ├─ __key_save_p = *p
                │   ├─ __key_checkpoint = None (not field-seg)   generated/json.rs:1535
                │   ├─ check b'"' at *p                          generated/json.rs:1540
                │   ├─ parse_string_JsonParser_string             generated/json.rs:1545 → 1387
                │   │   ├─ first_quote_or_backslash(tail)        generated/json.rs:1413, NEON-SIMD
                │   │   ├─ b'"' hit, no escapes
                │   │   ├─ body = "a", *p = end+1
                │   │   └─ builder.push_leaf_with_str(body)       json/builder.rs:357
                │   │       └─ deposit(JsonValue::String("a"))    json/builder.rs:181
                │   │           └─ Object frame, pending_key=None, value is String
                │   │              → promote to pending_key       json/builder.rs:202
                │   ├─ skip_space                                 generated/json.rs:1546
                │   ├─ check b':' at *p                          generated/json.rs:1547
                │   ├─ *p += 1; skip_space                       generated/json.rs:1552-1553
                │   ├─ __matched = false (no key_checkpoint)     generated/json.rs:1554-1578
                │   ├─ branch: not field-seg, descend             generated/json.rs:1584
                │   │   ├─ skip_space
                │   │   └─ parse_wrap_JsonParser_value             generated/json.rs:1587 → 1859
                │   │       ├─ cursor.decide(7) = ParseFully     generated/json.rs:1871
                │   │       ├─ skip_space → first = b'1'         generated/json.rs:1872
                │   │       └─ 'try_branches loop:                generated/json.rs:1876
                │   │           ├─ first=49u8 ('1') hits arm at  generated/json.rs:1926
                │   │           ├─ attempt_p = *p; attempt_builder = builder.checkpoint()
                │   │           │                                 generated/json.rs:1927-1928
                │   │           │  ───────────────────────────────────────────────────────
                │   │           │  HOT PATH WASTE #1 — checkpoint = Vec<OpenFrame>::clone
                │   │           │  json/builder.rs:243 → JsonStructCheckpoint{
                │   │           │      arrays: arena.array_count(),
                │   │           │      objects: arena.object_count(),
                │   │           │      stack: self.stack.clone(),  ← deep clone
                │   │           │      root: self.root, next_handle: ... }
                │   │           │  ───────────────────────────────────────────────────────
                │   │           ├─ parse_number_JsonParser_number  generated/json.rs:1929 → 1255
                │   │           │   └─ regex DFA on tail, parses "1", builder.push_leaf_with_f64(1.0)
                │   │           │       └─ deposit(JsonValue::Number(Float(1.0)))
                │   │           │           └─ Object frame, pending_key=Some("a")
                │   │           │              → push JsonPair{key:"a", value:Number}
                │   │           │                                 json/builder.rs:212
                │   │           ├─ Ok → builder.commit(attempt_builder)
                │   │           │   ─── default no-op; the Vec it cloned is dropped
                │   │           └─ break 'try_branches
                │   ├─ skip_space; peek next byte = b'}'           generated/json.rs:1591
                │   ├─ Some(b'}') arm: *p += 1; end_compound(__handle)
                │   │                                              generated/json.rs:1596-1599
                │   │   └─ json/builder.rs:296: pop frame, push_object(pairs)→JsonObjectId(1),
                │   │      deposit JsonValue::Object(id) — stack now empty,
                │   │      so root = Some(JsonValue::Object(JsonObjectId(1)))
                │   └─ return Ok(())
                │
                ├─ skip_space (post-call eof check)               generated/json.rs:3484
                ├─ pos == input.len() ✓
                └─ builder.finalise(input)                         generated/json.rs:3496 → json/builder.rs:164
                    ├─ debug_assert stack.is_empty() ✓
                    ├─ root.take().expect(...)
                    └─ JsonDocument::new(arena, root, input)       json/document.rs:66
```

### A.2 What is `OpenFrame`?

Defined at `crates/core/src/runtime/json/builder.rs:61-87`. One frame
per partially-built compound on the in-flight stack:

```rust
enum OpenFrame<'p> {
    Array  { items: Vec<JsonValue<'p>> },
    Object { pairs: Vec<JsonPair<'p>>, pending_key: Option<&'p str> },
    Pair   { key: Option<&'p str>, value: Option<JsonValue<'p>> },
    Wrap   { value: Option<JsonValue<'p>> },
}
```

Each `OpenFrame` has at minimum two heap pointers (the `Vec`'s
ptr+len+cap is 24 bytes; `String` keys are reference-bumped). A `clone()`
on `Vec<OpenFrame>` walks every frame and clones each interior `Vec`
element-wise. For an object three deep, every checkpoint copies the
whole tree-in-progress.

### A.3 Why does `Vec<OpenFrame>::clone` account for 86.07% inclusive samples?

The 86.07% number comes from DEEP-B (`docs/tranches/BA/BA.md:19`,
25,963 fat-LTO bench samples). The driver:

1. **`parse_wrap_JsonParser_value` is called once per JSON value.** A
   Twitter-shape blob has thousands of values (every leaf, every nested
   object, every array element).
2. **Every entry to `parse_wrap_*` calls `builder.checkpoint()` BEFORE
   matching the first byte's dispatch arm.** `crates/core/src/grammar/generated/json.rs:1880`
   for `b'"'`, `:1900` for `b'-'`, `:1914`-`:2050` for digits `0–9`,
   `:2054` for `b'['`, `:2074`/`:2116` for `b'f'`/`b't'`, `:2095` for
   `b'n'`, `:2137` for `b'{'`. **Seventeen** speculative attempt sites
   inside `parse_wrap_*` (plus one in the object body at `:1536` for
   key matching, `:1658` for the array body, `:1769` for pair body),
   one entered per value parsed.
3. **The first byte already disambiguates the arm uniquely.** JSON's
   value alphabet is disjoint: `{` → object, `[` → array, `"` → string,
   `-` / `0..9` → number, `t` / `f` → bool, `n` → null. No backtrack
   between arms is structurally possible — the byte-dispatch ladder
   already chose. Yet the codegen-emitted `match` body wraps every arm
   in a `Result::Err → rollback` fallback that never fires on
   well-formed input.
4. **`JsonStructCheckpoint::stack: Vec<OpenFrame>::clone`** at
   `json/builder.rs:243` is therefore allocated, populated, and dropped
   once per value, with the depth of the in-flight stack growing as the
   tree descends. For a deeply nested object (Twitter `statuses[].user.…`),
   the stack at the leaf push is N-deep, and the clone copies N frames.
5. **The clone is then `commit()`-dropped immediately on success** —
   `commit()` is the default trait no-op (`runtime/builder.rs:81`).

Three contributing pathologies stack:

- **Speculative discipline applied to a non-speculative dispatcher.**
  The `parse_wrap_*` body is a typed `Alt` whose alternatives are
  byte-disjoint. The codegen pattern is generic over `Alt` shape and
  doesn't specialise for byte-disjoint Alt.
- **`begin_compound`/`end_compound` allocate `Vec::new()` per frame
  push** (`json/builder.rs:274,279,283,289`). Each compound entry is a
  fresh Vec, even when `with_capacity` hints exist on the arena.
- **The runtime `StructLayout` literal is reconstructed at every parse
  fn call site** (e.g. `generated/json.rs:1512-1518` for `object`,
  similarly for `array` at :1651, `pair` at :1791). Each carries a
  `String::from(rule_name)` + empty `Vec::new()` — the codegen-known
  type info is re-derived per call. This is BA's "type inference output
  is thrown away at the parse boundary" (`BA.md:17`).

### A.4 Cycles-wasted punch list

| # | Location | What's wasted | Why | Mitigation |
|---|---|---|---|---|
| 1 | `generated/json.rs:1512` (object), `:1651` (array), `:1771` (pair) — 42 sites total across all generated grammars | `__layout` / `__pair_layout` reconstruction per parse-fn call: `String::from(rule_name)` + `Vec::new()` fields | Type info known at codegen is re-built at runtime | Direct-projection: emit typed `JsonValue::Object(...)` constructors instead of `begin_compound(&__layout)` |
| 2 | `json/builder.rs:243` (`checkpoint::stack.clone()`) | Vec deep-clone per speculative entry, dominates samply at 86.07% | `Checkpoint` should be O(1) tuple, not O(N) clone | `Checkpoint = (stack_depth: u32, arena_arrays: u32, arena_objects: u32, root_set: bool)`; rollback truncates instead of replaces |
| 3 | `generated/json.rs:1876-2154` (`parse_wrap_JsonParser_value` body) | 18 attempt arms each with checkpoint + commit, on a byte-disjoint Alt | First-byte already disambiguates; no speculation needed | Predictive byte-dispatch: emit `match first { b'{' => …, b'[' => … }` directly, no checkpoint at all on byte-disjoint Alt |
| 4 | `json/builder.rs:274,279,283,289` (every `OpenFrame` constructor) | Fresh `Vec::new()` allocation per compound entry | Capacity hints exist but aren't threaded to per-frame Vecs | Frame Vecs use `Vec::with_capacity(hint)` from `StructRegistry`; or arena allocates via bumpalo |
| 5 | `json/builder.rs:181-235` (`deposit` match) | One match per scalar push that re-discriminates the parent frame | Codegen knows which frame is on top from generation context | Direct-projection emits parent-typed deposit (e.g. `obj.pairs.push(JsonPair {key, value: lit_number(1.0)})`) |
| 6 | `json/document.rs:370-392` (`walk_path`) | Linear pair scan + recursive walk on every path step, post-parse | The path is known at parse-call time (via `parse_with`), so leaf could be projected during parse | `Document::get<T>(path)` reroutes through `parse_with(input, path)` (BA's W4 thesis) |
| 7 | `generated/json.rs:1859-2162` (every `parse_wrap_*` call enters Wrap frame) | `OpenFrame::Wrap` allocated per value just to forward one child | Wrap is a transparent wrapper — it has no payload | Codegen elides Wrap entirely on Alt-of-Refs shapes; the inner ref's deposit lands directly on the enclosing parent |
| 8 | `generated/json.rs:2177-2188` (`parse_JsonParser_value` → `__value`) | Single-call thunk that does nothing except forward | One layer of unneeded indirection at hot path | Inline at codegen; emit one symbol, not two |
| 9 | `runtime/json/parse_with.rs:96-99` (legacy-segment lowering on every lazy `get`) | `Vec<LegacySegment>::with_capacity` + per-step lowering on every lazy parse | `TypedPath` should be the runtime alphabet; `LegacyPath` is a transient bridge | BA W5 carry: delete `LegacyPath`/`LegacySegment` |
| 10 | `parse_string_JsonParser_string` escape path (`generated/json.rs:1424,1439`) | `Vec::with_capacity` + `Box::leak` per escaped string — one leak per string with `\u` or `\\` | `Box::leak` is a permanent leak by design; arena should own these | Arena owns escaped slices; lifetime-extend through the arena, not via `Box::leak` |
| 11 | `runtime/json/builder.rs:135` (`stack: Vec::with_capacity(8)`) | One persistent stack alloc per parse | Per-grammar nesting depth is statically bounded (mineable from grammar) | Codegen emits `[OpenFrame; MAX_DEPTH]` array on the stack — no heap alloc; KISS for grammars where depth is bounded |
| 12 | `generated/json.rs:1855-1942` (cursor consultations on eager path) | `cursor.decide`, `cursor.current_kind`, `cursor.match_field` calls return constant `ParseFully` for the empty-path eager case | Eager path doesn't need the cursor; constant-fold should remove these calls but doesn't | Direct emission of two parse fns (eager / lazy) from one source — NOT two paths in the SAME fn |

`feedback_no_orthogonal_codepaths` is alive: items #3, #5, #7 are
discriminator branches that should be specialised at codegen time, not
re-discriminated at runtime.

### A.5 Allocations summary for `{"a": 1}`

| Allocation | Site | Purpose | Necessary? |
|---|---|---|---|
| `Vec<Vec<JsonValue>>` (arrays slab) | `arena.rs:107` | per-parse arena | yes (one per parse) |
| `Vec<Vec<JsonPair>>` (objects slab) | `arena.rs:107` | per-parse arena | yes (one per parse) |
| `Vec<OpenFrame>` (stack) | `builder.rs:135` | open-frame stack | could be stack array |
| `OpenFrame::Object{pairs: Vec::new(), …}` | `builder.rs:279` | object body | could be capacity-hinted |
| `String::from("object")` | `generated:1514` | layout literal | NO — discard at codegen |
| `Vec::new()` for layout `fields` | `generated:1517` | layout literal | NO — discard at codegen |
| `OpenFrame::Wrap{value: None}` | `generated:1880-2138` (every value) | transparent wrap | NO — elide Wrap on byte-disjoint Alt |
| `Vec<OpenFrame>::clone` (the checkpoint) | `builder.rs:248` | speculative rollback | NO on byte-disjoint Alt |
| `Vec<JsonPair>` (pairs vec) | `builder.rs:279` | object pair list | yes (size known? hint-fold) |
| `JsonObjectId` | `arena.rs:139` | handle | nominal (one per non-empty object) |
| `JsonDocument` | `document.rs:66` | return value | yes |

For `{"a": 1}`: 9 allocations actually issued; **2** are necessary by
mechanism (arena). Six are scaffolding. One of those six (the
checkpoint) issues a deep clone whose cost is O(stack-depth).

### A.6 Annotated cycles-wasted graph (eager `{"a": 1}` path)

```
parse(input)
│
├── ScanState alloc                              <-- needed
├── JsonStructBuilder alloc + 2 arena slabs     <-- needed
├── EAGER_EMPTY_PATH (LazyLock)                  <-- one-time global
├── PathCursor for eager (always ParseFully)     <-- WASTE: cursor consult on eager (Item 12)
│
└── parse_object_JsonParser_object
    ├── __layout reconstruction               <-- WASTE: Item 1 (per call)
    ├── begin_compound(&__layout)
    │   └── stack.push(OpenFrame::Object{...}) <-- WASTE: Vec::new() not capacity-hinted (Item 4)
    │
    ├── parse_string("a")                      <-- needed
    │   └── push_leaf_with_str → deposit       <-- WASTE: deposit re-match (Item 5)
    │
    └── parse_wrap_JsonParser_value(...)
        ├── enter Wrap frame                   <-- WASTE: Wrap on byte-disjoint Alt (Item 7)
        │
        └── 'try_branches loop: first byte = b'1'
            ├── digit-match arm hit
            ├── attempt_p, attempt_builder = checkpoint()
            │   └── stack.clone()              <-- WASTE: 86.07% inclusive (Items 2, 3)
            ├── parse_number → push_leaf_with_f64(1.0)
            │   └── deposit(JsonValue::Number(...))
            └── commit(attempt_builder)
                └── drop the cloned Vec        <-- WASTE: clone was useless
```

### A.7 Aggregate finding

**The 86.07% samply share has one name and three causes.** The name is
`<JsonStructBuilder as StructBuilder>::checkpoint`. The first cause is
**checkpoint shape** (deep clone instead of cheap snapshot tuple). The
second is **codegen pattern** (every Alt arm is treated as speculative
even when the alphabet is byte-disjoint). The third is **frame cost**
(every `OpenFrame::Wrap` push allocates an `Option<JsonValue>`-sized
frame just to forward one value to the parent — nothing the wrap itself
holds is observable).

Fix all three at once and the samply share retires. Fix only one and
it doesn't.

---

## PART B — Post-restart pipeline sketch

This sketch assumes the user has accepted the from-scratch posture: BA
need not preserve the AZ-IV.W2.A `JsonStructBuilder` shape, the
`StructLayout`-runtime-literal pattern, or the `OpenFrame` taxonomy.
What lives is the **direct-projection ambition** captured in BA's
thesis: every grammar rule's `TypeDesc` reaches the emitter and produces
a typed Rust struct/enum. The parse fn writes typed fields directly.

### B.1 Layer specification

```
┌──────────────────────────────────────────────────────────────┐
│ Source                                                       │ &str / &[u8] / future Rope
├──────────────────────────────────────────────────────────────┤
│ Scan                                                         │ SIMD scalars (skip_space, structural index, escape scan)
├──────────────────────────────────────────────────────────────┤
│ Parse                                                        │ recursive descent + Pratt + cursor; no combinator overhead
├──────────────────────────────────────────────────────────────┤
│ Type-infer (build/codegen-time)                              │ project_types → TypeDesc per rule; CSP-mediated
├──────────────────────────────────────────────────────────────┤
│ IR (typed AST + e-graph)                                     │ ONE IR — typed AST; e-graph operates over the same nodes
├──────────────────────────────────────────────────────────────┤
│ Optimize (build/codegen-time)                                │ CSP solver + e-graph saturation + pattern miners over IR
├──────────────────────────────────────────────────────────────┤
│ Codegen                                                      │ trait-based Emitter walking IR → backend-specific source
├──────────────────────────────────────────────────────────────┤
│ Runtime (per backend, generic + per-grammar)                 │ generic substrate (path, scan support); per-grammar typed value sums + parse fns
├──────────────────────────────────────────────────────────────┤
│ Host                                                         │ symbol table mapping `-> fn_name(...)` to backend-native function refs
├──────────────────────────────────────────────────────────────┤
│ Backend ABI                                                  │ Emitter trait per backend; shared driver consumes IR
└──────────────────────────────────────────────────────────────┘
```

#### Layer 1 — Source

| Property | Value |
|---|---|
| Input type | `&'p str` (canonical) or `&'p [u8]` |
| Output type | same; lifetime threaded through every downstream surface |
| Lives in | caller |
| Owns | nothing — borrowed |
| Generality | unchanged from today |

Rope/streaming is **not** a substrate-level concern; if streaming arrives
it lands as a `Source` trait abstraction at this layer, not a
parser-level conditional.

#### Layer 2 — Scan

| Property | Value |
|---|---|
| Input type | `&[u8]`, position |
| Output type | byte / position / structural-index witness |
| Lives in | `crates/simd-scan/` (today) — SIMD primitives, NEON / AVX2 / scalar |
| First-class | `skip_space`, `first_quote_or_backslash`, `structural_index`, `nospace_bitmap_64` |
| Optional | grammar-specific scanners codegen-emitted into `__shape_support_<G>` modules |

`feedback_actual_profiling` rules: scan primitives are mineable from
grammar (whitespace alphabet, structural alphabet, quote class) —
codegen folds these into per-grammar `__shape_support_<G>` modules. No
runtime scanner construction.

#### Layer 3 — Parse

| Property | Value |
|---|---|
| Strategy | recursive descent — one function per rule; Pratt for operator chains; cursor-driven for path-walk lazy |
| Input | bytes + position + ScanState + cursor + builder |
| Output | direct mutation of typed parent struct via codegen-emitted writes |
| Lives in | `crates/core/src/grammar/generated/<g>.rs` |
| Generality | parse fns are per-grammar; the **emitter** that produces them is generic |

**No combinators on the hot path.** `parse-that` combinators stay as
codegen substrate / build-time tooling, never run per parse. (Already
true today.)

**No `OpenFrame` stack.** Direct-projection emits typed-record writes
at codegen — the parent's typed fields are the stack:

```rust
// emitted shape (see B.2 for full sketch):
let mut obj_pairs: SmallVec<[JsonPair<'p>; 8]> = SmallVec::new();
let mut child = JsonValue::Null;
parse_value_into(&mut child)?;
obj_pairs.push(JsonPair { key: parsed_key, value: child });
```

The stack lives on the call stack, not in a heap `Vec<OpenFrame>`. KISS.

#### Layer 4 — Type-infer

| Property | Value |
|---|---|
| When | build-time only (codegen) |
| Input | grammar AST (parsed `.bbnf`) |
| Output | `TypeDesc` per rule, `StructLayout` per Named compound, `BackendType` per backend |
| Lives in | `crates/ir/src/passes/types/`, `crates/ir/src/registry/` |
| Mechanism | mostly mechanical (Alt → enum, Seq → struct/tuple, Repeat → Vec/SmallVec); CSP for cross-rule constraints (mutual recursion, projection arity) |

`feedback_typed-materialization-invariant` is enforced by an
**inverse-layout-audit IR pass** (BA W1 carry): the build fails when a
compound-typed rule has no `StructLayout`. The pass is the source of
truth for "every `->` reaches the tape emitter" — it's what guarantees
no codepath silently drops typed payload.

#### Layer 5 — IR

**Decision posture: ONE IR**, the typed-AST IR (today
`crates/ir/src/types/`, `dag/`). The e-graph (`crates/egraph/`)
operates over the same node alphabet (Alt / Seq / Repeat / Ref / Lit /
Regex / Wrap) — it doesn't introduce its own. There's no
sea-of-nodes IR.

| Property | Value |
|---|---|
| Node shape | `IrNode` enum: `Alt(Vec<NodeId>) | Seq(Vec<NodeId>) | Repeat(NodeId, RepeatKind) | Ref(RuleId) | Lit(...) | Regex(RegexId) | Wrap(NodeId) | Map(NodeId, MapExpr) | Host(NodeId, HostFnRef)` |
| Storage | DAG with `NodeId` interning (today's pattern) |
| Mutation | new node creation; `Changed` flag (per `feedback_changed_flag_convergence`) for fixed-point |
| E-graph relationship | e-graph eclasses are equivalence classes over `IrNode`s; extract picks one node per class via cost model |

There is **no separate "tape IR"**. Tape was a runtime substrate, not an
IR — its abrogation (per AZ-IV / BA) doesn't leave an IR shape behind.

#### Layer 6 — Optimize

Sequence (today's modules):

1. **Type infer** (`passes/types/`) — TypeDesc, BackendType.
2. **Profile / fact mining** (`passes/profile.rs`, `passes/facts/`) — operator chains, regex info, prefix sets.
3. **CSP strategy synthesis** (`passes/csp_strategy/`) — per-rule shape strategies (Alt strategy, Seq strategy) chosen via CSP over fact domains; uses `crates/csp-solver/`.
4. **Pattern miners** (`passes/patterns/`) — operator precedence (Pratt), tail-cons, sentinel-loop, specific anti-patterns.
5. **E-graph saturation** (via `crates/egraph/CspScheduler`) — apply rewrite rules under a cost model; the scheduler is itself CSP-driven (`csp_scheduler.rs`).
6. **Materialization** (`passes/materialization/`) — pin structural decisions onto the IR after the e-graph extracts.
7. **Pre-emit transforms** (`passes/transform/`) — inline, fuse, prune — these are simpler local rewrites that don't need the e-graph hammer.

**Layering, not orthogonality.** CSP is the **substrate** for both
rewrite-scheduling and strategy-selection; e-graph is the **engine**
for confluent rewriting under cost. The user resolves Q3 (orthogonal /
layered / unified) — see B.3.

#### Layer 7 — Codegen

**Trait-based Emitter walks the IR.** One IR → three emitters
(Rust / TS / WASM); per-backend modules in
`crates/core/src/backend/{rust,ts,wasm}/`. **Driver shared**
(`crates/core/src/backend/driver/`) — the IR walk, decision dispatch,
strategy selection are backend-agnostic; only the leaf emission is
per-backend. (This is already the system shape; what changes post-restart
is the *output content* — direct projection instead of `__layout`
construction.)

For ONE rule, see B.2.

#### Layer 8 — Runtime

| Layer | Lives in | Generic across grammars? |
|---|---|---|
| Path types (`PathSegment`, `Path`, `TypedPath`) | `crates/core/src/runtime/path.rs`, `crates/core/src/path/` | yes — generic |
| Scan support (`__shape_support_<G>::skip_space`, `first_quote_or_backslash`, …) | `crates/core/src/grammar/generated/<g>.rs` (codegen-emitted) | **no — per-grammar** (the alphabet is grammar-specific, mined at codegen) |
| Typed value sum (`JsonValue`, `CssTypedValue`, …) | `crates/core/src/grammar/generated/<g>/value.rs` (post-restart) | **no — per-grammar** |
| Document (`JsonDocument`, …) | `crates/core/src/grammar/generated/<g>/document.rs` (post-restart) | **no — per-grammar** |
| Path-query trait + walker | `crates/core/src/grammar/generated/<g>/document.rs` | **no — per-grammar** (typed walker over typed value sum) |
| `StructBuilder` / `OpenFrame` / `arena_template` / `builder_template` | **DELETED** | n/a |

The runtime gets *thinner*, not thicker. The post-restart `runtime/`
hosts only path + error types and grammar-agnostic tooling. Per-grammar
typed value sums and documents emit into `grammar/generated/<g>/` as
their own sub-modules (not flat siblings — `feedback_directory_modules`).

#### Layer 9 — Host

| Property | Value |
|---|---|
| Today | `crates/core/src/grammar/host.rs` — symbol map; `-> parse_hex_color(input)` resolves to a function ref |
| Post-restart | unchanged in shape; expanded so each backend's resolver knows its own native function names |
| Per-backend resolution | Rust: `crate::host::parse_hex_color` (closed); TS: `runtime.parseHexColor`; WASM: indexed extern import |
| Grammar declaration | `@host parse_hex_color(input: bytes, pos: u32) -> CssColor` somewhere in `.bbnf` (BA W4 thesis confirmed) |

This is the only non-Rust backend touchpoint that requires per-backend
table emission, but it's a **symbol-rewrite** at codegen (host fn name
→ backend symbol) — not a runtime indirection.

#### Layer 10 — Backend ABI

| Approach | Shape | Tradeoffs |
|---|---|---|
| **Trait-based Emitter** (recommended) | `trait Emitter { fn emit_alt(&mut self, ...); fn emit_seq(...); ... }`; shared driver walks IR, calls trait methods; per-backend impls produce source strings | DRY; one IR walker; backend authors implement leaves only |
| Source-emit-per-backend | Three independent walkers, three source emitters | quick to bootstrap; risks logic drift between backends |
| IR + per-backend lower (LLVM-style) | Three lowering passes (BBNF-IR → Rust-IR / TS-IR / WASM-IR), per-IR emitters | overkill; we don't have backend-specific IR concerns sufficient to justify |

Recommend: **trait-based Emitter** (already the system shape; preserve).

### B.2 Concrete codegen sketch — JSON `object`

Grammar (excerpted from `grammar/json/json.bbnf`):

```bbnf
pair   = string, colon >> value ;
object = "{" >> ((pair << comma?)*)?w << "}" ;
```

Emitted Rust (post-restart, direct-projection, byte-disjoint dispatch):

```rust
// EMITTED: per-grammar typed value sum (one source of truth)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JsonValue<'p> {
    Null,
    Bool(bool),
    Number(f64),
    String(&'p str),
    Array(JsonArrayId),
    Object(JsonObjectId),
}
#[derive(Debug, Clone, Copy)]
pub struct JsonPair<'p> { pub key: &'p str, pub value: JsonValue<'p> }

// EMITTED: parse fn — direct projection, no OpenFrame, no checkpoint
#[inline]
pub fn parse_object<'p>(
    input: &'p [u8],
    p: &mut usize,
    arena: &mut JsonArena<'p>,
    state: &mut ScanState,
    cursor: &mut PathCursor<'_>,
) -> Result<JsonObjectId, ParseErr> {
    if input.get(*p).copied() != Some(b'{') {
        return Err(ParseErr::syntax(*p));
    }
    *p += 1;
    skip_space(input, p, state);
    if input.get(*p).copied() == Some(b'}') {
        *p += 1;
        return Ok(JsonObjectId::EMPTY);
    }
    let mut pairs: SmallVec<[JsonPair<'p>; 8]> = SmallVec::new();
    loop {
        let key = parse_string_borrowed(input, p)?;          // &'p str
        skip_space(input, p, state);
        if input.get(*p).copied() != Some(b':') { return Err(ParseErr::syntax(*p)); }
        *p += 1;
        skip_space(input, p, state);
        let value = parse_value(input, p, arena, state, cursor)?;  // JsonValue<'p>
        pairs.push(JsonPair { key, value });
        skip_space(input, p, state);
        match input.get(*p).copied() {
            Some(b',') => { *p += 1; skip_space(input, p, state); }
            Some(b'}') => { *p += 1; return Ok(arena.intern_object(pairs.into_vec())); }
            _ => return Err(ParseErr::syntax(*p)),
        }
    }
}
```

Same rule, emitted to TypeScript:

```ts
// EMITTED: per-grammar typed value sum
export type JsonValue =
  | { kind: 'null' }
  | { kind: 'bool';   value: boolean }
  | { kind: 'number'; value: number }
  | { kind: 'string'; value: string }
  | { kind: 'array';  items: JsonValue[] }
  | { kind: 'object'; pairs: JsonPair[] };
export interface JsonPair { key: string; value: JsonValue }

// EMITTED: parse fn — same shape, TS idiom
export function parseObject(ctx: ParseCtx): JsonValue {
  if (ctx.bytes[ctx.pos] !== 0x7b /* { */) throw syntaxErr(ctx.pos);
  ctx.pos++; skipSpace(ctx);
  if (ctx.bytes[ctx.pos] === 0x7d /* } */) { ctx.pos++; return { kind: 'object', pairs: [] }; }
  const pairs: JsonPair[] = [];
  for (;;) {
    const key = parseStringBorrowed(ctx);
    skipSpace(ctx);
    if (ctx.bytes[ctx.pos] !== 0x3a /* : */) throw syntaxErr(ctx.pos);
    ctx.pos++; skipSpace(ctx);
    const value = parseValue(ctx);
    pairs.push({ key, value });
    skipSpace(ctx);
    const b = ctx.bytes[ctx.pos];
    if (b === 0x2c /* , */) { ctx.pos++; skipSpace(ctx); continue; }
    if (b === 0x7d /* } */) { ctx.pos++; return { kind: 'object', pairs }; }
    throw syntaxErr(ctx.pos);
  }
}
```

Properties:

- **No `StructLayout` runtime literal.** `String::from("object")` /
  `Vec::new()` for fields — **gone**.
- **No `OpenFrame` stack.** Local Rust variables hold the pair vec on
  the call stack; the recursion forms the stack naturally.
- **No `checkpoint()` / `rollback()` on byte-disjoint Alt.** The
  dispatcher is a `match` on first byte — no speculative entry.
- **`JsonValue` is a Copy 16-byte tag-and-payload** (today's shape) —
  passed by value into recursion, returned by value, stored by value.
- **The arena owns interned slices** (today's shape; cleaned up).

The emitted parse fn is ~25 lines for `object` vs. the current
`parse_object_JsonParser_object`'s 130 lines + `parse_wrap_*`'s 300
lines. **Order-of-magnitude reduction in generated LOC**, with
matching reduction in inclusive samples.

### B.3 Open architectural questions (for user resolution)

These are flagged, not answered. Each lists what choice **dictates
downstream**.

#### Q1. Did we kill tape for the right reason?

**Today's posture**: tape abrogated; AZ-IV.W2.A landed `JsonStructBuilder`.
The 86.07% samply share argues against the tape's CHILDREN (StructBuilder
+ OpenFrame), not against the tape itself.

| Choice | Dictates |
|---|---|
| Direct-projection only (current BA thesis) | Per-grammar typed value sums everywhere; runtime substrate retires `arena_template`/`builder_template`; matches sonic's pattern; KISS |
| Tape returns as substrate, projection at `get<T>()` time | Restoring tape requires resurrecting `TapeRec`/`TapeCursor`; `get<T>()` is a cursor over packed bytes; analogous to simd-json; shifts complexity from runtime to `get<T>` machinery; cross-grammar parity is automatic but per-grammar ergonomics decline |
| Hybrid: tape as the **first** tier (lazy parse cheap), typed projection as the **second** tier (eager `parse` reads tape, projects to typed) | Two paths in the codegen; violates `feedback_no_orthogonal_codepaths` unless one is the degenerate case of the other |

#### Q2. Cursor-parse vs byte-skip for path API

**Today**: `parse_with` parses with cursor; the cursor decides per-rule
whether to descend / skip / bound. sonic-rs's `get_from_str` does
byte-skip without parsing.

| Choice | Dictates |
|---|---|
| Cursor-parse (current) | One parser, used by both eager and lazy; lazy is "cursor that skips early"; type-checked path; structurally elegant |
| Byte-skip (sonic-style) | Two parsers — full and skipping; per-grammar byte-skip generation; faster lazy path; doubles emitter surface |
| Cursor-parse with byte-skip optimisation **inside** the cursor | The skip bypasses the parse machinery for known-skippable spans (object keys not in path, array indices below target); hybrid that keeps one source of truth |

#### Q3. Optimizer layering

**Today**: CSP solver (`crates/csp-solver/`) generic; e-graph
(`crates/egraph/`) generic; `CspScheduler` schedules e-graph rewrite
application via CSP-style dirty-domain propagation. Pattern miners are
sequential pre-passes.

| Choice | Dictates |
|---|---|
| Orthogonal | CSP for strategy choice, e-graph for rewrites; separate phases; maintainable but lossy across phases |
| Layered (current `CspScheduler`) | E-graph operates inside a CSP-controlled scheduler; one pass driving the other; today's shape works |
| Unified hypergraph | Strategy nodes and rewrite nodes co-exist in one hypergraph; one fixed-point; max generality, max complexity; best-case lowest cost; might be premature |

#### Q4. Proc macro vs xtask for codegen

**Today**: `xtask regen` writes `crates/core/src/grammar/generated/<g>.rs`
into the source tree; commits track the artefact.

| Choice | Dictates |
|---|---|
| Continue xtask (current) | Generated files visible in PR diff; debuggable; slow regen workflow (~60s per `--check`); commits include generated + source change atomically |
| Proc macro at compile time | `parser!{ include_str!("json.bbnf") }`; no committed artefacts; fast iteration; `cargo expand` becomes the inspection surface; requires `--check`-equivalent in CI to detect drift |
| Hybrid: xtask emits to `target/`, proc macro reads from `target/` cache | Worst of both worlds |

Per `feedback_clean_regen_discipline`, current xtask discipline forbids
hand-edits — the choice doesn't change discipline, only iteration
speed and inspection surface.

#### Q5. Path crate consolidation

**Today**: `crates/bbnf-path/` (Rust path build / type-check; macro),
`crates/bbnf-path-ts/` (TS path schema + template tag), and
`crates/core/src/path/` (runtime path types — `TypedPath`, executor,
cursor). Three places.

| Choice | Dictates |
|---|---|
| 1 crate (`bbnf-path` covers all) | Bigger crate; TS sub-crate becomes a feature flag or bindings; cleanest; current structure mostly already supports this |
| 2 crates (`bbnf-path` + `bbnf-path-ts`) | Today's split; preserves backend isolation but the runtime types still live in `core/src/path/`, which is the third place |
| 3 crates (status quo, but rename) | Honest; documents the layered structure; biggest surface |

The user has signalled (`feedback_no_god_modules`) against monolithic
crates and (`feedback_directory_modules`) for directory submodules. Q5
likely resolves to **2 crates** with the runtime types folded into
`bbnf-path` as a `runtime` submodule.

#### Q6. Backend ABI shape

| Choice | Dictates |
|---|---|
| Source-emit-per-backend | Three independent code emitters; logic drifts between backends; today's near-shape (TS/WASM emitters share helpers but not driver) |
| IR + per-backend lower | Three lowering passes (BBNF-IR → Rust-IR / TS-IR / WASM-IR); LLVM-style; overkill for our backend variance |
| Trait-based Emitter (recommended in B.1) | One driver + per-backend `Emitter` trait impls; current direction; KISS; backend authors only implement leaf emission |

The hardening synthesis already nudges toward unified driver + trait
Emitter; user confirmation locks the post-restart codegen layer.

#### Q7. AU floor doctrine

**Today**: AU set 19 floor benchmarks; AZ-IV.W3 closed at 18/19 BELOW
(one row below floor). BA's W2/W3 carries restoring 19/19 via
direct-projection.

| Choice | Dictates |
|---|---|
| Re-meet (BA carry) | BA must restore 19/19 at-or-above; current plan; gates on samply 7-artefact contract; preserves AU as the floor doctrine |
| Re-baseline | Drop AU-rooted floors; new bench set rooted at post-restart commit; loses AU's measurement discipline; risks "we don't regress against ourselves" tautology |
| Sonic-parity | Replace AU floors with "≤ 1.5× sonic" rows per workload; ambitious; ties measurement to external SOTA; aligns with BA's `bbnf_get_twitter ≤ 5×` carry; aspirational |

The user has expressed `feedback_beat_lightning` for CSS (beat
lightningcss in every metric, not approach). The JSON analogue would
push toward **sonic-parity**, but that requires byte-skip choice (Q2)
and direct-projection together — adopting one without the other leaves
the gap.

---

## Closing notes

- **Part A is mechanism-true**: every cited file:line is verified.
- **Part B is sketch**: layering + codegen shape; concrete enough for
  the user to evaluate, abstract enough to leave Q1–Q7 open.
- **No file mutated outside `audit/`.** Tranche docs untouched per
  hardening contract.
- **BA's existing thesis survives the restart.** Direct-projection,
  one-parse-path, cheap-checkpoint, sonic-class `get` — all four BA
  invariants land in B.1/B.2 by mechanism. The user's choice is whether
  to **execute** BA as currently scoped, or **rescope** with Q1–Q7
  resolutions altering the targets.

