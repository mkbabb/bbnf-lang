# LOCK-3 · Pass-2 · Family 3C — GRAMMAR-HOMOIOUSIA (the IR expressed as a grammar)

Re-pinned to the live implementation tree `skinny@061c79fa7` (`git -C skinny
rev-parse HEAD`; the R6 cartography was captured at `7f3be27c2`, R1 recon likewise
— every seam below re-verified against `061c79fa7`). All paths under
`/Users/mkbabb/Programming/bbnf-lang/skinny/` unless prefixed.

The thesis of this family: the IR tower's node algebra is itself a bbnf grammar;
transpositions are grammar-to-grammar maps; the generator eats its own type
declarations (bootstrap alignment with LOCK-7). This report develops that to its
strongest *honest* form, names its one live-adjacent consumer, carries its enabling
gate as a born-RED dependency edge, and states the three prototypes that move it.

---

## 0. What is actually on live — the two-tier node algebra

The IR is two hand-written sum types, both in one file:

- **Frontend `ExprKind`** — `crates/ir/src/lib.rs:234-271`, **10 arms**: `Seq`,
  `Alt{branches,mode}`, `Repeat{body,min,max}`, `Optional`, `Literal{bytes,case}`,
  `Regex{pattern:String}`, `Ref{name,target}`, `Annotation{name,value}`,
  `Project{body,target,ty:Option<String>}`, `Discard{left,right,keep_left}`.
- **Backend `BackendExpr`** — `crates/ir/src/lib.rs:376-413`, **13 arms**: `Entry`,
  `Seq`, `Alt`, `RepeatLoop`, `OptionalBranch`, `ByteLiteral`, `RegexProgram{pattern,
  span_kind}`, `CallRule`, `SpanMark{kind,label}`, `TapeEmit{kind:TapeKind}`,
  `DirectBuild{shape,fields}`, `ValueProject{body,target,ty}`, `Return`.

Both are ~38 lines of hand-authored Rust `enum`. Together with `TapeKind` (9 arms,
`:453-463`) and `BackendShape` (5 arms, `:361-367`) they are the whole spine
vocabulary. `Regex{pattern:String}` (`:250`) is already an **opaque string leaf** —
this is why XL-C's ratification (grammar tier keeps `Regex(StringId)` opaque)
dissolves the ONE-HIR precondition: an `ir.bbnf` grammar treats a regex arm as a
terminal carrying a string, never as a sub-grammar to unify. The regex sublanguage
is *not* a dependency of this family.

The transposition `ExprKind → BackendExpr` is `lower_expr` (`passes/src/lib.rs`,
the arm-by-arm map ending `Project → BackendExpr::ValueProject` at `:1349-1353`),
followed by `materialize_rule` (`:1362-1391`) which splices the emit-order tail
`SpanMark(Start) · body · SpanMark(End) · TapeEmit{kind} · DirectBuild{shape,fields}
· Return`. This splice is the sharpest fact of the family and is treated in §4/R-2.

---

## 1. The gate is exactly one schema construct: sum

The direct-to-struct schema IR `DirectTypeKind` (`crates/codegen/src/direct_schema.rs:
52-58`) has **one variant, `Struct`**. `validate()` (`:162`) and `validate_type_ref`
(`:293`) know only structs; the renderer `json_typed_direct::render`
(`crates/codegen/src/json_typed_direct.rs:10`, 1245 LOC) emits only Rust structs.

An `ExprKind` node is a **sum**: `expr = seq | alt | repeat | optional | literal |
regex | ref | annotation | project | discard`. Its `->` projections declare *ten
variant constructors*. The Struct-only schema cannot represent it. Therefore the
homoiousia formulation is **entirely gated on `DirectTypeKind::Sum`** — and the
directive's warning ("do not treat the product-only, sum-excluding subset as
standing for the formulation") is correct: a grammar that only emits records is not
an IR grammar. The IR *is* a sum of node kinds; without the sum node there is no
family.

That gate is not this family's to invent. It is authored, on the plan of record,
by **GN-3.W1 "The Sum Node"** (`docs/tranches/sk-v24/GN-3.md §4/W1`): add
`DirectTypeKind::Sum{variants,unknown}`, grow HM `unify` over sums, add
`render_sum_arm` beside `render_field_arm` (`json_typed_direct.rs:435`) emitting a
Rust `enum` with one arm per variant. GN-3.W1's *own* consumer is the runtime
`JsonValue` enum (byte-dispatched by `at_cursor`, `grammars/json/value.rs:28-47`).
This family reuses only the **declaration face** of that node — the `enum … {…}`
render — and discards the runtime byte-dispatch half (see §3, the no-ts-ir seam).

---

## 2. The mechanism, made concrete

**Substrate — `ir.bbnf` (net-new grammar source, ~40 lines).** Each IR node kind is
a rule; each alternative carries a `->` projection naming its Rust variant; struct
payloads are rule references. Sketch (faithful to the live arms):

```
expr_kind = seq -> Seq | alt -> Alt | repeat -> Repeat
          | optional -> Optional | literal -> Literal | regex -> Regex
          | ref -> Ref | annotation -> Annotation
          | project -> Project | discard -> Discard ;
seq       = "Seq" -> Vec<ExprId> ;
literal   = bytes:Vec<u8> , case:CaseSensitivity -> Literal ;
regex     = pattern:String -> Regex ;     # opaque leaf — XL-C
...
@pretty expr_kind block ;                  # the declaration-emit directive
```

**Transposition as a grammar-to-grammar map.** `lower_expr`'s pure arms
(`Seq→Seq`, `Alt→Alt`, `Optional→OptionalBranch`, `Literal→ByteLiteral`,
`Regex→RegexProgram`, `Ref→CallRule`, `Project→ValueProject`) are a fixed
variant-to-variant rewrite table — a second grammar `lower.bbnf` whose `->` targets
are `BackendExpr` variants. Seven of the ten arms are *pure* (no external data). The
remaining transposition — `materialize_rule`'s emit-order tail — is fact-dependent
(R-2).

**The generator eats its own declarations.** Feed `ir.bbnf` through the existing
typed entry `emit_typed_from_source` (`crates/codegen/src/lib.rs:130`) — which
already calls `lower::schema_direct::lower_program` + `json_typed_direct::render`
(`lib.rs:238-243`). Once `DirectTypeKind::Sum` exists, the emitter materialises
`ir.bbnf` to the Rust `enum ExprKind {…}` and `enum BackendExpr {…}`. The generated
enums are diffed against the hand-written `ir/src/lib.rs:234-271`/`:376-413`; on
byte-identity the hand enums are *promoted from* the grammar. Codegen then contains,
as data, the definition of the IR it consumes — the homoiousia ("same substance":
the generator's input language and its own IR are one grammar family).

**How this CONSUMES the threaded `:204` node-type (the shared edge).** The
`ir.bbnf` `->` projections *are* node-types. When GN-3.W2 stops discarding
`Project.ty` at `passes/src/lib.rs:204` and threads it into `DirectBuild.target`
(`:1149-1287`, today always `None` — R6 §5d), the IR grammar's `alt -> Alt` arrow
resolves — through that exact thread — into a `DirectSumVariant{ rust_variant:"Alt",
payload }` row. This family therefore **reads one resolved node-type off the thread**
rather than re-deriving it: the arrow target `Alt` becomes the emitted variant name
via the same infer→`type_ref` wire, never via a second classifier. Contrast the live
`derive_materialization_roles` (`passes/src/lib.rs:1403-1490`) which *re-derives*
JSON's taxonomy from byte-literal heuristics — the homoiousia path deletes that
re-derivation in favor of the declared arrow.

---

## 3. The named downstream consumer (the Y.13 answer)

Consumer-less substrate is forbidden. Two consumers, one enabling and one terminal:

**(A) Enabling gate, born-RED, owner GN-3.** `DirectTypeKind::Sum` +
`render_sum_arm` is the schema construct this family stands on. It is *already
owned and scheduled* by GN-3.W1 (LOC +420, born-RED, `GN-3.md` wave manifest §3).
Edge: `3C.grammar-as-IR —needs→ DirectTypeKind::Sum @ direct_schema.rs:52 (GN-3.W1)`.
This family adds **zero** new substrate to obtain the sum node; it consumes GN-3's.
That is the parsimony case (R-1): standalone, generating a 38-line enum from a
generator is net-negative LOC; riding GN-3.W1's already-paid sum node it is nearly
free.

**(B) Terminal consumer, LOCK-7 (GESTALT-B homed).** The actual reader of an
IR-expressed-as-grammar is the **bootstrap / meta-circular oracle** — LOCK-7's 7B
("IR grammar … the generator becomes mostly data") and 7D ("self-hosting as
verification first — grammar-defined grammars serve as differential ORACLES against
the hand parsers; promotion gated on measured parity"). The consumer contract:
`ir.bbnf` emits the `ExprKind`/`BackendExpr` declarations; a differential oracle
diffs generated-vs-hand; the hand enum is retained until byte-identity + the
regen∘regen=regen fixpoint (7A) hold, then promoted. This is on-live-adjacent
because the three preconditions already exist:

- **Self-host precedent lives.** `grammar/bnf/bnf.bbnf` is BNF-in-BBNF with working
  `@pretty grammar block ; @pretty rule group ;` emit directives — a grammar that
  *describes a grammar language* and drives structured emission. `grammar/misc/
  g4.bbnf`, `grammar/misc/regex.bbnf` are further self-descriptions. The apex gap is
  precisely a `bbnf.bbnf`/`ir.bbnf`; nothing structural forbids it.
- **The emitter exists and works.** `json_typed_direct::render` is a live 1245-LOC
  direct-to-struct emitter (PROVE-07 anchor). Its only missing arm is sum, which is
  GN-3.W1.
- **The diff targets exist.** The hand `ExprKind`/`BackendExpr` enums are the exact
  oracle baseline (`ir/src/lib.rs:234-271`,`:376-413`).

**Boundary honored (no-ts-ir / XL-D).** The IR-grammar face emits **type
declarations, never a runtime IR value**. GN-3.W1's Sum node has two faces — a
compile-time `enum` declaration and a runtime first-byte dispatcher (`at_cursor`).
An `ExprKind` is a compile-time data structure, never parsed from input bytes, so
this family consumes **only the declaration face** and drops the dispatcher. That is
exactly XL-D: "the fast meta-grammar face is the build-time emitter." LOCK-3 keeps
the driver-seat question (WHERE the fact contract sits at L1→L2) distinct from
LOCK-2's engine choice; this family touches neither engine nor runtime.

---

## 4. Integration seams (path:line, live)

| Seam | Site | Role in 3C |
|---|---|---|
| Frontend node algebra | `ir/src/lib.rs:234-271` | the sum this family expresses as `ir.bbnf` + the oracle diff target |
| Backend node algebra | `ir/src/lib.rs:376-413` | second sum; `lower.bbnf` map's `->` targets |
| Opaque regex leaf | `ir/src/lib.rs:250` | XL-C: string terminal, no ONE-HIR dependency |
| The transposition map | `passes/src/lib.rs:1349-1353` | pure variant→variant rewrite (7 arms) |
| Emit-order splice | `passes/src/lib.rs:1362-1391` | fact-dependent tail (R-2) |
| Role re-derivation (to delete) | `passes/src/lib.rs:1403-1490` | replaced by declared `->` |
| `->` thread-in point | `passes/src/lib.rs:204` (+plan `:1149-1287`) | the shared node-type this family consumes |
| Schema sum gate (born-RED) | `direct_schema.rs:52-58` | GN-3.W1 provides `DirectTypeKind::Sum` |
| Declaration emitter | `json_typed_direct.rs:10` (+`render_sum_arm`@`:435`) | materialises `ir.bbnf` → `enum` |
| Typed entry | `codegen/src/lib.rs:130` (`emit_typed_from_source`) | the generation driver already wired |
| Self-host precedent | `grammar/bnf/bnf.bbnf`, `@pretty` | proves grammar-describes-grammar + emit |
| Name-gate to retire | `runtime_generator.rs:98` (`has_rule("value")`) | homoiousia removes the JSON-name special case |

---

## 5. Prior art (citations)

- **MLIR ODS / TableGen** — operations are declared as TableGen records; the C++ op
  classes are *generated* from the declaration (`mlir-tblgen`). This is the industrial
  proof of "the IR node algebra is data; the generator emits the node classes," i.e.
  the generator eating its own definitions. The strongest scale anchor. (MLIR
  Operation Definition Specification; Lattner et al., "MLIR: A Compiler Infrastructure
  for the End of Moore's Law," 2021.) MLIR **PDL** (Pattern Descriptor Language) is
  the declarative rewrite face — the analogue of "transpositions are grammar-to-grammar
  maps."
- **Nanopass framework** (Sarkar, Waddell, Dybvig, "A Nanopass Infrastructure for
  Compiler Education," ICFP 2004; Keep & Dybvig 2013) — languages are *defined as
  data* (`define-language`); passes are language-to-language maps with catamorphism
  boilerplate auto-generated. This is the direct prior art for the transposition
  claim: a pass is a partial rewrite over a declared node algebra, the framework fills
  the identity arms. The honest model for `lower_expr` under homoiousia.
- **tree-sitter `node-types.json`** — the typed AST *schema* is emitted from the
  grammar; a grammar-expressed language yields a typed node vocabulary as data. The
  closest analogue to "an IR expressed as a grammar produces the typed node schema."
- **egg/egglog `define_language!`** and this repo's own `crates/egraph` derive-language
  (`crates/egraph/tests/derive_language.rs`) — enum ⇄ language; the project already
  derives a `Language` impl from an existing enum. Prior art *inside the tree* that the
  enum↔grammar correspondence is mechanizable.
- **Metacircular evaluator** (SICP ch. 4; Reynolds, "Definitional Interpreters," 1972)
  — the homoiconic root: the evaluated language and the evaluator's data are one.
- **GHC `deriving` / Template Haskell**, **`syn`+`quote` derive macros** — enum shape
  synthesized from declaration; the mechanical basis of the declaration emitter.

---

## 6. Named risks & counterexamples

- **R-2 (the sharpest — the transposition is NOT pure grammar-to-grammar).**
  `materialize_rule` (`passes/src/lib.rs:1362-1391`) splices `SpanMark/TapeEmit/
  DirectBuild/Return` from `MaterializationPlan.descriptors` — `label`, `shape`,
  `fields:Vec<DirectBuildField>`, `kind:TapeKind` — none of which live in the source
  grammar; they are *derived* by role classification (`derive_materialization_roles`,
  `:1403`; `derive_materialization_plan`, `:1136-1287`). So `ExprKind→BackendExpr` is
  **grammar + facts → grammar**, an *attributed* map, not a pure rewrite. The honest
  claim is therefore weaker than the family's slogan: the node **algebra** is
  grammar-derivable, but the transposition carries synthesized attributes. Two exits,
  both real: (a) fold the facts *into* the grammar as `->` chains (this is exactly
  GN-3.W3 "The Authored Shape" — typed grammars carry roles/labels/cardinality as
  declared arrows, so the plan is derived not heuristic), making the map pure again;
  or (b) model it as an attribute grammar (inherited/synthesized attributes), which is
  Nanopass's actual shape. Either way the family must NOT claim a bare rewrite. A
  prototype must *measure* the pure-arm fraction (§7 PROVE-B).

- **R-1 (consumer thinness / parsimony inversion).** Standalone, replacing a 76-line
  hand enum (`ExprKind`+`BackendExpr`) with `ir.bbnf` (~40 lines) *plus* the emitter
  machinery is net-negative unless the sum node is a sunk cost. Disposition: the family
  is only economical **riding GN-3.W1's already-scheduled sum node** and LOCK-7's
  already-scheduled bootstrap oracle. Absent both owners, Y.13 is not satisfied and the
  family should be BANKED, not landed. Counterexample to over-eagerness: skinny's IR is
  ~23 arms total — MLIR-scale (thousands of ops) is what makes ODS pay; at 23 arms the
  win is *bootstrap coherence and name-gate deletion*, not LOC.

- **R-3 (bootstrap staging skew).** If codegen is generated from `ir.bbnf` and
  `ir.bbnf` is parsed by codegen, a change to `ir.bbnf`'s node set requires the OLD
  generator to emit the NEW enum. Standard bootstrap hazard; handled only by a
  committed stage-0 + the regen∘regen=regen byte-identity fixpoint (LOCK-7 7A). This is
  a real recurring cost, not a one-time bring-up.

- **R-4 (declaration face vs runtime-dispatch face conflation).** GN-3.W1's sum node
  is designed for a *runtime* byte-dispatched `JsonValue`. Naively consuming it would
  drag a first-byte `match` into the IR enum, which is nonsense (IR values are never
  parsed from bytes). The family must consume only `render_sum_arm`'s type-declaration
  output and assert the dispatcher half is absent — this is the no-ts-ir seam and must
  be a gate, not an assumption.

- **R-5 (sum canonicalization / order).** `ir.bbnf`'s alternative order must map to the
  hand enum's variant order for byte-identity, or the oracle diff fails on ordering
  alone. GN-3.W1 already specifies order-insensitive set-equal unification with
  canonical ordering in `sum.rs` (GN-3 R5); this family inherits that requirement and
  must additionally pin *declaration order* = source order for the diff to be strict.

---

## 7. What a prototype must test to move this family

- **PROVE-A — the declaration face round-trips.** Author `ir.bbnf` for the 10
  `ExprKind` arms; run through `emit_typed_from_source` (`codegen/src/lib.rs:130`) with
  a Sum-capable `DirectTypeKind` (borrow GN-3.W1's `render_sum_arm`); assert the
  emitted `enum ExprKind {…}` is variant-set-equal and (post-`rustfmt`) byte-identical
  to `ir/src/lib.rs:234-271`, and parses under `syn::parse_file`. Gate: the generated
  enum *is* the hand enum. This proves the homoiousia declaration face and directly
  exercises GN-3.W1's node — the tightest possible consumer for the sum gate.

- **PROVE-B — quantify the pure-arm fraction (settles R-2).** Express `lower_expr` as a
  declarative `lower.bbnf` rewrite table and count: of the 10 `ExprKind` arms, how many
  map to `BackendExpr` with *no* consultation of `MaterializationPlan`? (Expected: 7
  pure — Seq/Alt/Optional/Literal/Regex/Ref/Project; 3 fact-dependent via
  `materialize_rule`.) Report the fraction and whether GN-3.W3's authored-shape arrows
  eliminate the fact dependency. This is the honest measurement that decides whether the
  family's slogan ("grammar-to-grammar maps") holds or must degrade to "attributed
  map."

- **PROVE-C — the fixpoint (settles R-3).** regen `ir.bbnf`→`ExprKind`, rebuild codegen
  against the generated enum, regen again; assert byte-identical (regen∘regen=regen).
  Proves the generator can eat itself without staging skew — the LOCK-7 7A gate this
  family inherits.

**Disposition of the family in one line:** the IR's node *algebra* is grammar-derivable
and its declaration face round-trips through the existing typed emitter; the family adds
no substrate of its own, standing entirely on two born-RED edges — `DirectTypeKind::Sum`
(GN-3.W1) and the bootstrap oracle (LOCK-7 7B/7D) — and its slogan must be tempered from
"pure grammar-to-grammar maps" to "declared node algebra + attributed transposition"
until PROVE-B shows GN-3.W3's arrows close the fact gap.
