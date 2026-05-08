# Skinny Spec — Compiler Slice

The compiler slice owns the compile-time path from a single grammar source
(`grammars/json.bbnf`) through to emitted Rust under
`runtime/src/grammars/json/`. The skinny exists to validate the V1 SOTA-beat
premise on JSON before tranches A-J commit, so this slice deletes every V1
compiler crate that JSON does not exercise and states the per-skip impact.

This slice composes with three siblings:

- SUBSTRATE (`SUBSTRATE.md`) — `Tape<'input>`, `ValueRef<'i>`, `JsonRoot<'i>`,
  payload arena, SIMD integration contract.
- BENCH (`BENCH.md`) — dual-track measurement (generated parser vs hand-coded
  JSON parallel), reproducibility schema, go/no-go thresholds.
- WORKSPACE (`WORKSPACE.md`) — `Cargo.toml`, member list, per-crate LOC
  budgets for the skinny, build/test commands.

Anything those slices own is referenced here by **contract only**. The compiler
emits Rust that names `JsonRoot<'i>`, `Tape<'input>`, `ValueRef<'i>`, the
structural-alphabet kernel, and the payload arena; it does not specify their
internal layout. SUBSTRATE owns the layout.

The compiler slice is for **one grammar (JSON)** and the **minimal HM** that
JSON requires. The full V1 stack adds DK13 higher-rank, GADT branch-local
equality, finite CSP, e-graph rewrites, recognizer mining, cost-model
extraction, VM replay, language server, and `path-core`. None of those are
reachable from a JSON parse and none are part of this slice.

---

## 1. `json.bbnf` Source Sketch

The skinny ships a host-fn-free JSON grammar as a deliberate deviation from the
V1 JSON row. ARCH §12.2 gives full V1 JSON metadata plus numeric/string host
fns from `host::primitives`; the committed `grammar/json/json.bbnf` decodes
string escapes through `decode_json_string_to_arena(input) -> String`. The
skinny removes that `@host fn` surface and keeps string/number materialisation
lazy in SUBSTRATE-owned accessors so the SOTA hot path measures structure parse,
not scalar extraction.

### 1.1 Skinny grammar text

```bbnf
null   = "null" ;
bool   = "true" | "false" ;

number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;

string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;

ws     = /[ \t\n\r]*/ ;

comma  = "," ws ;
colon  = ":" ws ;

value  = ws (object | array | string | number | bool | null) ws ;

pair   = string ws colon value ;
member = pair (comma pair)* ;
members = member? ;

elements = (value (comma value)*)? ;

array  = "[" ws elements "]" ;
object = "{" ws members "}" ;

json   = ws value ws ;
```

Differences from the committed `grammar/json/json.bbnf` and why:

| Difference | Reason |
|---|---|
| No `-> 0u8`, `-> true`, `-> false`, `-> f64` map tails. | `MapTail` lowers to `Call(kind: Map)` → `ValueProject`. The skinny does not need typed scalar projections to measure SOTA throughput; the typed root keeps raw spans and lazy accessor methods. ShapeFacts mining is replaced by hand-curated shapes. |
| No `decode_json_string_to_arena(input) : String` host call. | Strings emit `RegexProgram` against the validator regex; the tape records the raw string span plus `STRING_NEEDS_UNESCAPE` when needed. `JsonString::as_str()` lazily returns `Cow<'input, str>` through SUBSTRATE, outside the parse-time SOTA measurement. |
| `?w` collapsed to explicit `ws`. | The `?w` whitespace marker is sugar for an `@layout(ws = ...)` policy. The skinny does not lower `LayoutDirective` and does not own a `passes::layout` HM-internal subroutine. ARCH §8.2 reads HM-equality as a layout-lowering subroutine; in the skinny, layout is a no-op pass-through and HM runs as a top-level pass. To keep the grammar legal without `@layout`, whitespace is desugared to an explicit `ws` rule that lowers to a `RegexProgram` discarded before `TapeEmit`. |
| `>>` / `<<` suppression operators replaced with explicit `ws`. | Same reason as `?w`. The suppression operators are layout-policy sugar; the skinny grammar treats whitespace as ordinary rule body. |
| `@pretty object group ;` etc. removed. | Pretty-print directive is a PASS-3 / runtime concern; no BIR variant. Removing keeps the directive surface to zero. |

### 1.2 Regex literals JSON exercises

| Literal | Engine route | Verifier shape |
|---|---|---|
| `/-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/` | `parse-that-regex` lazy DFA. | `RegexProgram { plan: LazyDfa, span: NumberSpan }`. The parser validates the matched span and emits the span; numeric conversion happens in `JsonNumber::as_f64()` / `as_i64()`. |
| `/"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/` | `parse-that-regex` lazy DFA. | `RegexProgram { plan: LazyDfa, span: StringSpan }`. The parser validates UTF-8/escape structure, records `STRING_NEEDS_UNESCAPE`, and does not write decoded bytes to the arena during parse. |
| `/[ \t\n\r]*/` | Trivial DFA, in practice scalar `take_while`. | `RegexProgram { plan: TakeWhileClass, span: WhitespaceSpan }`. The skinny may inline this as a tight loop to avoid the regex VM hop; it is byte-class equivalent. |

The structural-alphabet `SimdScan` is **not** spelled in the grammar source —
it is mined by the recognizer pass (which the skinny skips). The skinny
substitutes a hand-curated structural-alphabet recognizer (see §5).

### 1.3 `@host fn` decision

The skinny is host-fn-free. JSON in V1 declares one host call to decode escape
sequences; the skinny moves that decode into the SUBSTRATE `decode_string`
path and removes the `@host fn` surface entirely. Rationale:

1. The SOTA test measures structure-parse throughput. The cost of routing
   decode through `CallHost` is not assumed away — BENCH §7.8 carries
   **two probes** that bound it separately: (a) a per-call dispatch overhead
   microbench (`host_call_dispatch_overhead`) measuring `CallHost` indirection
   in isolation against direct calls (target: ≤ 50 ns/call), and (b) a
   gross-time eager-decode JSON variant (`host_call_eager_decode`) that
   forces parse-time string decoding through the V1-shaped registry path
   (expected delta on twitter: 5-15% from eager-decode work, not from
   dispatch overhead). The two probes test two different masking modes;
   neither uses a single 2% threshold.
2. Removing host functions removes the entire `host::types`, host overload
   selection CSP path, and `CallHost` lowering. Three crates (`host`,
   `csp-solver`, the `host-overload` improvement bridge) become unreachable.
3. ARCH §12.1 yaml onboarding admits a metadata-only host route. JSON in the
   skinny is a stricter case — no host stanza at all.

The cost: the V1 grammar's `-> f64` and `-> decode(...)` map tails do not
typecheck against the skinny compiler. That is acceptable only because the
SOTA test parses structural shape and validates regex spans; scalar values are
materialised lazily by SUBSTRATE accessors. The bench-counters feature must
show zero payload-arena writes for Track 1 and Track 2.

---

## 2. Grammar IR Subset

The full V1 Grammar IR has 14 variants (ARCH §7.1). JSON exercises 9.

### 2.1 Exercised variants

| Variant | JSON site | Skinny coverage |
|---|---|---|
| `Rule` | every rule (`null`, `bool`, `number`, `string`, `ws`, `comma`, `colon`, `value`, `pair`, `member`, `members`, `elements`, `array`, `object`, `json`). | Required. Carries name, body ID, and (skinny) trivial monomorphic signature. |
| `Seq` | `pair = string ws colon value`; `array = "[" ws elements "]"`; etc. | Required. Empty-Seq normalisation runs in `passes::normalize` (legality rewrite). |
| `Alt` | `value = object \| array \| string \| number \| bool \| null`; `bool = "true" \| "false"`. | Required. JSON is byte-disjoint at every alt site (every alt's first byte is unique: `{`, `[`, `"`, digit/`-`, `t`, `f`, `n`). The skinny lowers every JSON alt as `Alt { mode: Dispatch }`; **`Alt { mode: Speculative }` is unreachable** for JSON. |
| `Repeat` | `(comma pair)*`; `(comma value)*`. | Required. Uses min=0 and no separator metadata — the comma is part of the body. The skinny rejects nullable bodies at `passes::normalize` (legality rewrite: nullable-body Repeat detection). |
| `Optional` | `member?`, `(member (comma member)*)?` (the `members` and `elements` rules). | Required. Empty branch keeps shape (the empty array/object case). |
| `Literal` | `"null"`, `"true"`, `"false"`, `"["`, `"]"`, `"{"`, `"}"`, `","`, `":"`. | Required. Byte literal with `case = Sensitive`. |
| `Regex` | `number`, `string`, `ws`. | Required. Three regex programs total. |
| `Ref` | `value` references `object`, `array`, `string`, `number`, `bool`, `null`; `pair` references `string`, `colon`, `value`; etc. | Required. JSON has no generics, so every `Ref` instantiates the empty type-arg list. |
| `Annotation` | None in the skinny grammar. | Retained as a Grammar IR variant for the BBNF AST round-trip; the skinny grammar carries zero `Annotation` payloads. |

### 2.2 Skipped variants and per-skip impact

| Variant | Skipped because | Impact on SOTA test |
|---|---|---|
| `Predicate` | JSON has no `&` / `!` lookahead. | None. SOTA throughput is unaffected by predicate machinery. |
| `Lookbehind` | JSON has no `\|<` / `\|<!` lookbehind. | None. |
| `Call` (`kind: Map`) | Skinny grammar drops `-> f64`, `-> true`, etc. | Slight: the typed `Json` root exposes raw spans and lazy accessors instead of pre-decoded scalars. Scalar decode runs at access time, not at parse time. SOTA latency is the parse phase, so the move from parse-time to access-time scalar decode is recorded as a favorable skinny deviation. |
| `Call` (`kind: Host`) | Skinny is host-fn-free. | Potentially masking until BENCH's one-host-fn JSON variant proves the `CallHost` registry path stays within 2% median of the direct SUBSTRATE path on all three corpora. |
| `LayoutDirective` | Whitespace is desugared to an explicit `ws` rule. | Slight increase in BIR size (every whitespace site becomes a `CallRule(ws)`). The whitespace rule itself lowers to a tight scalar loop, so the runtime cost is the same as `@layout(ws = ...)`. SOTA neutral. |
| `ErrorDirective` | JSON has no `@error` recovery. | None. SOTA is measured on valid input; recovery is irrelevant. |

### 2.3 Validation pass surface

`passes::validate` in the skinny runs three checks and rejects everything else:

1. **No backend node names** — Grammar IR contains no variant whose name
   matches a Backend IR variant (per ARCH §7.1 invariant
   `ir::validate::grammar_ir_has_no_backend_nodes`). Static enum check.
2. **No nullable Repeat body** — every `Repeat` body must be non-nullable.
   The skinny computes a one-pass nullability fixpoint over Grammar IR. If
   `members`'s body were `pair?` (instead of `pair (comma pair)*` followed by
   the outer `?`), nullability would be flagged. JSON's `(comma pair)*` body
   is non-nullable because `comma` is `","` then `ws`; the leading `,` is
   non-nullable.
3. **All `Ref` targets resolve** — every `Ref { target: RuleId }` resolves to
   a defined `Rule`. The skinny rejects forward references that never bind.
   JSON has one cyclic reference (`value` → `object` → `pair` → `value`); the
   resolver tolerates cycles because cyclic resolution is not the same as
   nullable cycle.

Skipped validation in the skinny:

| V1 check | Skipped because | Impact |
|---|---|---|
| Lookbehind width proof | JSON has no lookbehind. | None. |
| Recovery code registration | JSON has no `@error`. | None. |
| Layout policy scoping | JSON has no `@layout`. | None. |
| Pretty-print directive vocabulary | JSON has no `@pretty`. | None. |
| `directive-canon` lint | Skinny grammar uses zero directives. | None. |

---

## 3. BIR Subset

The full V1 Backend IR has 20 variants (ARCH §7.2). JSON exercises 14.

### 3.1 Exercised variants

| BIR variant | JSON site | Notes |
|---|---|---|
| `Entry` | the `json` root rule. | Single public entry: `pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>`. |
| `Seq` | every `Seq` Grammar IR node lowers to BIR `Seq`. | Straight-line control flow. |
| `Alt { mode: Dispatch }` | every `Alt` Grammar IR node. | All JSON alts are byte-disjoint, so `Dispatch` is the only mode used. The Dispatch discriminator is a 256-entry byte table built at codegen time from the alt's first-byte set. |
| `RepeatLoop` | every `Repeat`. | Min=0, no separator metadata; the body carries the comma. Progress guard required (the body must consume at least one byte). |
| `OptionalBranch` | `members`, `elements` empty cases. | Empty branch keeps shape (the empty `Vec`). |
| `ByteLiteral` | every `Literal`. | Byte compare; the codegen folds the long-prefix literals (`null`, `true`, `false`) into a `u32`/`u64` aligned compare where possible. |
| `RegexProgram` | `number`, `string`, `ws`. | Three programs. The skinny lowers each through `parse-that-regex` lazy DFA (no full DFA, no VM). |
| `SimdScan` | structural-alphabet pre-scan over the whole input. | One `SimdScan { mode: Exact, needle: StructuralAlphabet { '{', '}', '[', ']', ',', ':', '"' }, fallback: scalar }`. The hand-curated recognizer (§5) places this as a single `SimdScan` BIR node before the `Entry` body and feeds the `Alt`/Dispatch table with byte offsets. |
| `CallRule` | every `Ref`. | Regular function call. JSON has 14 rule defs and ~35 `CallRule` sites. |
| `SpanMark` | every captured rule (compiler-generated). | Start + end span pairs for `value`, `string`, `number`, `array`, `object`, `pair`. Used by `JsonRoot` view to expose source slices. |
| `TapeEmit` | every node + token event (compiler-generated). | The tape carries `(NodeKind, span, payload_slot?)`. JSON node kinds: `Object`, `Array`, `Pair`, `String`, `Number`, `Bool(true)`, `Bool(false)`, `Null`, `Member`, `Element`. |
| `DirectBuild` | every typed-view rule (compiler-generated). | Builds the typed `JsonValue<'i>` enum + `JsonObject<'i>` / `JsonArray<'i>` + `JsonString<'i>` view. Co-scheduled with `TapeEmit` per ARCH §7.2 invariant 2. |
| `ValueProject` | the `Json::value(self) -> JsonValue<'i>` projection. | Single projection from `JsonRoot<'i>` to its top-level value; called by user code, not by the parser body. |
| `Return` | end of every rule body. | Compiler-generated. |

### 3.2 Skipped BIR variants and per-skip impact

| Variant | Skipped because | Impact on SOTA test |
|---|---|---|
| `Alt { mode: Speculative }` | JSON has zero non-disjoint alts. | None. The full V1 `Alt` payload still carries a `mode: Dispatch \| Speculative` discriminator; the skinny extractor always picks `Dispatch`. |
| `PrattSpine` | JSON has no operator precedence. | None. |
| `CallHost` | Skinny is host-fn-free. | Not emitted in the main skinny parser. BENCH still emits a one-host-fn measurement variant so the direct-call cut is quantified before RESULTS can claim FAITHFUL. |
| `LayoutScope` | Whitespace desugared to a `ws` rule. | None for throughput. The desugar has the same emitted code shape as a layout policy push/pop because `LayoutScope` lowers to identical scanner state. |
| `ErrorRecover` | JSON has no `@error`. | None. SOTA inputs are valid; recovery is unmeasured. |
| `PathEval` | Skinny does not link `path-core`. | None for SOTA. Path queries are a PASS-3 surface. |
| `DebugMark` | Skinny disables the debug profile. | None. |

### 3.3 BIR construction discipline

The skinny ratifies ARCH §7.2 invariants:

| Invariant | Skinny enforcement |
|---|---|
| Lowerers never inspect Grammar IR. | `codegen::lower::rust` imports `ir::backend_ir::*` only. The skinny does not need an import-deny lint at this size, but the rule holds. |
| Tape and direct-to-struct are one materialization strategy. | `TapeEmit` and `DirectBuild` are scheduled together by `passes::extract` (skinny version: a fixed-shape extraction with no choices). |
| OpenFrame clone stacks are absent. | Skinny uses a single arena; speculative alts are absent for JSON, so checkpoint/rollback is dead code. |
| SIMD is mined, not syntax-directed. | Skinny replaces the miner with a hand-curated recognizer (§5) that nominates the structural-alphabet `SimdScan` site. |
| VM can replay all BIR variants. | **Not enforced in the skinny.** The `vm` crate is stubbed; no replay invariant. |

---

## 4. HM-Only Type Checker

The V1 type system is HM-equality + Pierce-Turner bidirectional + DK13 +
finite CSP + GADT branch-local equality (ARCH §8.2). The skinny ships **only
HM-equality** — Algorithm-W, first-order unification, scheme generalization,
scheme instantiation. Nothing else.

### 4.1 What the skinny HM checker does

| Component | Skinny shape |
|---|---|
| Algorithm-W constraint generation | One pass over Grammar IR rules. Every rule body produces a fresh type variable; `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref` each have one inference rule. |
| First-order unifier | Robinson-style `unify(t1, t2) -> Result<Substitution, TypeError>`. Occurs-check on. ~150 LOC. |
| Scheme generalization | At rule definition: `generalize(t, env) -> Scheme` over free type variables not bound in env. JSON rules are monomorphic so generalization always returns a closed scheme. |
| Scheme instantiation | At every `Ref` site: `instantiate(scheme) -> Type` with fresh type vars per quantifier. JSON schemes have zero quantifiers, so instantiation is identity. |
| `TypeFacts` output | `HashMap<RuleId, Type>` plus `HashMap<NodeId, Type>` for body expressions. Internal to `passes::types`. |
| `TypeObligationLog` | `Vec<TypeObligation>` for diagnostics; carries source span, expected-from, actual-from, solver-stage. Skinny uses this only to format errors; no obligation discharge logic (no coercions in skinny). |

### 4.2 What the skinny HM checker does **not** do, and why JSON does not need it

| Mechanism | Why skipped | Why JSON does not need it |
|---|---|---|
| **DK13 higher-rank algorithmic completeness** | Adds ordered existential contexts, principality tracking, decidability/soundness/completeness proofs, explicit annotation rules for non-principal programs. | JSON has zero higher-rank types. Every JSON rule infers a closed monomorphic type. No `forall` quantifier survives generalization. This cut is JSON-FAITHFUL; CSS L4, Sheets, and BBNF-self remain V1 caveats where generic/host-chain shape can load-bear. |
| **Pierce-Turner bidirectional check/synth** | Requires the synth/check distinction at every node, expected-type propagation through annotations and chain steps. | JSON has zero explicit annotations and zero chain steps. Every node synthesises. The check direction has no callers in JSON. The skinny's HM is pure synth. |
| **Bounded coercion obligations** | Numeric widening, lifetime-owned escalation, generated-record shape narrowing, host-improvement rules. | JSON in the skinny exposes raw spans + arena handles; no scalar widening at parse time. No record narrowing because no `@host fn` returns a narrowed shape. The skinny's `TypeObligationLog` carries only equality failures. |
| **Finite-choice CSP** | Resolves host overload selection, layout representation, materialization mode, recognizer eligibility, recovery strategy, backend erasure, extraction legality. | JSON is host-fn-free (no host overload). Layout is pass-through (no layout choice). Materialization is fixed at tape-direct (no choice). Recognizer is hand-curated (no eligibility CSP). Recovery is absent. Backend is `RustBackend` only. Extraction is single-plan. **Every CSP axis has zero choice for JSON.** |
| **GADT branch-local equality** | Match-arm refinements, OutsideIn(X) implication constraints, `Implication { givens, wanted }` propagation. | JSON has zero match arms. The skinny grammar uses no `Pattern @ where T = U -> Block` form. This cut is JSON-FAITHFUL; BBNF-self and future typed host-chain grammars remain the V1 caveat. |
| **CHR-style improvement** | Closes host-overload ambiguity at the bridge boundary. | No host overload, no ambiguity. |
| **Schema-mining miner** | Telemetry-driven shape inference; corpus-fed candidate proposal; HM/CSP/DK13 solver chain. | Skinny ships **skinny-only hand-curated shapes** for the JSON typed root (§5). The miner is replaced by a small hand-written `ShapeFacts` table that carries a deletion gate at V1 graduation. |
| **Record narrowing** | Finite generated-shape coercion for source/target shapes both known at compile time. | Skinny's `JsonObject<'i>` and `JsonArray<'i>` are open shapes (read-only views); no narrowing is required. |

### 4.3 `TypeFacts` shape

The skinny `TypeFacts` is internal to `passes::layout::types` so the V1
graduation can add DK13/GADT/CSP siblings without moving Algorithm-W:

```rust
// crates/passes/src/layout/types/facts.rs (skinny)
pub(crate) struct TypeFacts {
    /// Inferred type per rule definition.
    pub rule_types: HashMap<RuleId, Type>,
    /// Inferred type per body expression node.
    pub node_types: HashMap<NodeId, Type>,
    /// Free type-variable substitution accumulated during unification.
    pub subst: Substitution,
    /// Diagnostic obligations for any unification failure.
    pub obligations: Vec<TypeObligation>,
}
```

`Type` is a sum:

```rust
pub(crate) enum Type {
    /// Type variable (post-instantiation; free during inference).
    Var(TypeVarId),
    /// Concrete builtin: Bytes, Str, F64, U8, Bool, Span.
    Builtin(BuiltinTy),
    /// Sequence; carries member shape ordering.
    Seq(Vec<Type>),
    /// Alternative (sum); carries member shape set.
    Alt(Vec<Type>),
    /// Repetition: list of body type.
    List(Box<Type>),
    /// Optional: nullable body type.
    Option(Box<Type>),
    /// Reference to a named rule's scheme.
    Rule(RuleId),
}
```

JSON's inferred `Type::Rule(value)` resolves (after one round of substitution)
to:

```text
Alt[
  Rule(object),
  Rule(array),
  Rule(string),
  Rule(number),
  Rule(bool),
  Rule(null),
]
```

Each branch resolves further; the recursion terminates because JSON's
recursive cycle (`value -> object -> pair -> value`) is well-typed under HM
(rule schemes are first-class members of the type lattice, so cyclic
references unify by name, not by structural unfold).

### 4.4 `LayoutFacts` in the skinny

ARCH §8.2 + Lock 2 make `passes::layout` the public boundary; HM is its
internal subroutine. The skinny preserves the boundary but makes
`passes::layout` a **trivial pass-through**:

```rust
// crates/passes/src/layout/mod.rs (skinny)
pub fn run(grammar: &GrammarIr, type_facts: TypeFacts) -> LayoutFacts {
    LayoutFacts {
        rule_types: type_facts.rule_types,
        node_types: type_facts.node_types,
        layout_policies: HashMap::new(), // no @layout in JSON skinny
    }
}
```

`TypeFacts` is consumed by `passes::layout`; `LayoutFacts` is the public
side-table consumed by `passes::extract` (§5). The skinny preserves the
**name and surface** of the boundary so a future tranche can drop in real
layout lowering without renaming the public artefact, but the **content** is
trivial.

JSON does not need any layout policy because:

1. Whitespace is desugared to an explicit `ws` rule.
2. `?w`, `>>`, `<<` operators are absent from the skinny grammar.
3. No `@layout` directive.
4. No layout-derived type narrowing.

### 4.5 HM checker LOC budget

| Module | Skinny LOC budget |
|---|---|
| `passes/src/layout/types/algorithm_w.rs` | ~250 |
| `passes/src/layout/types/unify.rs` | ~150 |
| `passes/src/layout/types/scheme.rs` | ~80 |
| `passes/src/layout/types/facts.rs` | ~60 |
| `passes/src/layout/types/diagnostic.rs` | ~120 |
| **Total `passes/layout/types/` skinny** | **~660** |

Compare to V1 estimate (~3,500 LOC for HM + bidirectional + DK13 + GADT). The
~80% cut is the entire SOTA-validation point: the skinny tests whether SOTA
falls out of the substrate + extraction shape, **independently** of whether
DK13 is in or out.

---

## 5. Pipeline Subset

The full V1 pipeline (ARCH §6) is 13 phases. The skinny runs 8.

### 5.1 Skinny pipeline

```text
source load
  -> BBNF parse
  -> semantic validation
  -> HM inference
  -> minimal shape mining (hand-curated)
  -> BIR construction (single-plan extraction)
  -> Rust lowerer
  -> template emit
  -> regen equality
```

### 5.2 Per-phase shape

| Phase | Skinny implementation | Output |
|---|---|---|
| **source load** | Read `grammars/json.bbnf` from disk; record source hash. | `Source { path, bytes, hash }`. |
| **BBNF parse** | Use the skinny `grammar` crate's partial parser for the §1.1 JSON subset. It parses the six-directive vocabulary enough to reject non-skinny directives with `BBNF-DIRECTIVE-NOT-IN-SKINNY`, but it does not depend on the skipped `parse-that` crate or the full self-host path. | `GrammarIr`. |
| **semantic validation** | The 3 checks at §2.3. | `ValidationReport`; halts compile on failure. |
| **HM inference** | Algorithm-W as in §4. Runs as a top-level pass (not a `passes::layout` subroutine in the skinny — the layout pass is trivial pass-through). | `TypeFacts`. |
| **minimal shape mining (hand-curated)** | A 80-line hand-written `ShapeFacts` table for JSON. Names every typed view: `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`, `JsonPair`. Replaces the V1 schema-mining miner (telemetry-driven; no telemetry in the skinny). | `ShapeFacts`. |
| **BIR construction (single-plan extraction)** | One-pass tree walk: Grammar IR + `LayoutFacts` + `ShapeFacts` → BIR. Single plan — no extraction CSP, no cost frontier, no e-graph. Hand-curated recognizer nominates the one `SimdScan` site (structural alphabet over the input). | `BackendIr`. |
| **Rust lowerer** | `codegen::lower::rust` walks BIR and produces `proc_macro2::TokenStream` for each emitted file. Detail at §6. | `EmittedSource { generated, parser, host (empty), view, value, visitor }`. |
| **template emit** | `codegen::runtime_template` writes the emitted source to `runtime/src/grammars/json/` as committed source (Lock 6). Skinny: write straight to disk via `cargo xtask regen-json`. | Files on disk. |
| **regen equality** | `cargo xtask check-json` re-runs the pipeline and diffs the output against the committed bytes. | Pass / fail diagnostic. |

### 5.3 Skipped phases and per-skip impact

| Phase | Skipped because | Impact on SOTA test |
|---|---|---|
| **recognizer mining** | Replaced by a hand-curated structural-alphabet recognizer for JSON. | JSON-FAITHFUL only after BENCH's alternate-plan stub confirms the hand-curated structural plan beats the scalar recursive-descent fallback and stays within the expected dispatch-table envelope. For grammars beyond JSON the miner becomes load-bearing. |
| **egraph rewrite** | No rewrites in the skinny — pick canonical plan. | Potentially masking until bounded. ARCH §10.1 classifies `cost-driven-rewrites` as ASPIRATIONAL for V1 SOTA; the skinny tests that classification by measuring the canonical plan against a non-egraph alternate-plan stub (structural-index vs scalar, dispatch table vs direct `match`). |
| **CSP extraction** | Trivial single-plan choice. No host overload, no layout choice, no materialisation choice, no recognizer eligibility, no recovery, no backend erasure, no extraction legality. | None for JSON. Every CSP axis has zero choice (§4.2). |
| **cost extraction** | Constant-cost extraction. | JSON has one production plan in the skinny, but BENCH still bounds the missing cost axis with the alternate-plan stub before claiming the cut is FAITHFUL. |
| **VM replay** | No `vm` crate. | None for SOTA. VM is a debug/replay artefact, not a perf path. |

### 5.4 Hand-curated recognizer

The skinny ships a tiny `passes/src/recognizers/skinny_json.rs` (~40 LOC).
This is a skinny-only fixture, not a generic recognizer miner. V1 graduation
deletes this module when `passes::recognizers` can nominate the same site from
grammar shape.

```rust
// crates/passes/src/recognizers/skinny_json.rs (skinny-only)
pub fn nominate(grammar: &GrammarIr) -> Vec<RecognizerNomination> {
    // For JSON, exactly one structural-alphabet SimdScan over the entire
    // input feeds the Alt/Dispatch table at every `value` site.
    vec![RecognizerNomination::SimdStructuralAlphabet {
        alphabet: StructuralAlphabet::new(b"{}[],:\""),
        verifier: VerifierRoute::Scalar,
        site: SimdSite::PreEntry, // emitted before the json rule body
    }]
}
```

`passes::extract` consumes the nomination and emits one `SimdScan { mode:
Exact, ... }` BIR node before the `Entry`. The runtime uses the structural
indices to skip whitespace and dispatch alts in constant time.

The full V1 miner (`passes::recognizers`) is much larger and runs detection
over the entire grammar corpus. The skinny replaces it with a single
hand-written nomination function for JSON. **For grammars beyond JSON the
skinny does not run** — the recognizer is JSON-specific and carries a deletion
gate.

### 5.5 Hand-curated shapes

```rust
// crates/passes/src/shapes/skinny_json.rs (skinny-only)
pub fn shapes_for_json() -> ShapeFacts {
    let mut facts = ShapeFacts::new();
    facts.add_struct("JsonRoot", &[("value", "JsonValue<'i>")]);
    facts.add_enum("JsonValue", &[
        "Object(JsonObject<'i>)",
        "Array(JsonArray<'i>)",
        "String(JsonString<'i>)",
        "Number(JsonNumber<'i>)",
        "Bool(bool)",
        "Null",
    ]);
    facts.add_struct("JsonObject", &[("members", "TapeSlice<'i, JsonPair<'i>>")]);
    facts.add_struct("JsonArray", &[("elements", "TapeSlice<'i, JsonValue<'i>>")]);
    facts.add_struct("JsonPair", &[("key", "JsonString<'i>"), ("value", "JsonValue<'i>")]);
    facts.add_struct("JsonString", &[("span", "Span<'i>"), ("needs_unescape", "bool")]);
    facts.add_struct("JsonNumber", &[("span", "Span<'i>")]);
    facts
}
```

`TapeSlice<'i, T>` and `Span<'i>` are SUBSTRATE-owned contracts. String and
number decoded values are accessor results, not parse-time fields.

---

## 6. `codegen::rust` Path

The Rust lowerer walks `BackendIr` and produces a `proc_macro2::TokenStream`
per emitted file. The lowerer is BIR-only (Lock 5; ARCH §10).

### 6.1 Per-BIR-variant lowering (skinny scope)

| BIR variant | Emitted Rust (sketch) | Notes |
|---|---|---|
| `Entry { symbol, body }` | `pub fn parse_<name><'i>(input: &'i str) -> Result<<Root><'i>, ParseError> { let mut state = ParserState::new(input); <body>; Ok(state.finish()) }` | Public entry; one per grammar. |
| `Seq { children }` | `<child_1>; <child_2>; ...` | Sequential lowered statements. |
| `Alt { mode: Dispatch, branches }` | `match state.peek_byte() { b'{' => <branch_object>, b'[' => <branch_array>, b'"' => <branch_string>, b'-' \| b'0'..=b'9' => <branch_number>, b't' \| b'f' => <branch_bool>, b'n' => <branch_null>, _ => return Err(ParseError::ExpectedValue), }` | Byte-table dispatch. The dispatch table is built at codegen time from the alt's first-byte set. |
| `RepeatLoop { body, min: 0, max: None }` | `loop { let cp = state.checkpoint(); match <body> { Ok(()) => continue, Err(_) => { state.restore(cp); break; } } }` | Progress guard via checkpoint compare. |
| `OptionalBranch { body }` | `let cp = state.checkpoint(); if let Err(_) = <body> { state.restore(cp); }` | Empty branch keeps shape (typed as `Option<T>` in the view). |
| `ByteLiteral { bytes }` | For short literals: `state.expect_bytes(b"<literal>")?;`. For 4-byte literals (`null`, `true`): `state.expect_u32_le(<u32_packed>)?;`. For 5-byte (`false`): `state.expect_bytes_5(b"false")?;`. | Aligned compares for the keyword literals. |
| `RegexProgram { plan: LazyDfa, span_kind }` | `let span = state.match_regex_lazy_dfa(&REGEX_<id>)?;` | The compiled regex is a `static REGEX_<id>: LazyRegex = LazyRegex::new(<pattern>);` initialised once. |
| `SimdScan { mode: Exact, alphabet, verifier }` | `let scan = simd_scan::structural_index_exact(input, &STRUCTURAL_ALPHABET_<id>); state.attach_index(scan);` | Emitted once, before the `Entry` body. The structural index is a `Vec<u32>` of byte offsets matching any of the alphabet bytes, with scalar parity hash available to BENCH. The runtime consumes it through `Tape<'i>::dispatch_offset()`. SUBSTRATE owns the kernel; the lowerer emits the dispatch site only. |
| `CallRule { callee, result_slot }` | `let <slot> = parse_<callee>(state)?;` | Generated rule functions are colocated in `parser.rs`. |
| `SpanMark { kind: Start }` / `End` | `let __start = state.position();` / `let __span = Span::new(__start, state.position());` | Compiler-generated. |
| `TapeEmit { kind, span, payload? }` | `state.tape.emit(NodeKind::<kind>, __span, <payload_slot>);` | Append-only tape write. SUBSTRATE owns `Tape::emit`. |
| `DirectBuild { shape, fields }` | `Json<Shape> { <field_1>: <slot_1>, ... }` | Builds the typed view; cursors point into the tape. Scalar accessors parse or unescape lazily. |
| `ValueProject { from, projection }` | `JsonValue::project(<from>, <projection>)` | Single projection helper for the typed root. |
| `Return { value }` | `Ok(<value>)` | Compiler-generated. |

### 6.2 Emitted file shape

The skinny emits `runtime/src/grammars/json/`:

| File | Skinny content | LOC budget |
|---|---|---|
| `mod.rs` | Re-exports of `Json`, `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonString`, `JsonNumber`. | ~30 |
| `generated.rs` | All BIR-derived parser bodies: `parse_json`, `parse_value`, `parse_object`, `parse_array`, `parse_pair`, `parse_string`, `parse_number`, `parse_bool`, `parse_null`, `parse_ws`. Includes the regex-literal `static`s and the structural-alphabet `static`. | ~600 |
| `parser.rs` | Public entry: `pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>` plus `ParserState`. Calls `generated::parse_json`. | ~120 |
| `host.rs` | **Empty file** (one `// no host fns` comment). JSON is host-fn-free in the skinny. | ~5 |
| `view.rs` | Typed view structs: `JsonRoot<'i>`, `JsonValue<'i>`, `JsonObject<'i>`, `JsonArray<'i>`, `JsonString<'i>`, `JsonNumber<'i>`, `JsonPair<'i>` with span-backed lazy accessor methods. Borrows from `Tape<'i>`. | ~250 |
| `value.rs` | `JsonValue` projection helpers; `Display` impl for the value enum. | ~80 |
| `visitor.rs` | `JsonVisitor` trait + default impls. Skinny ships only the dispatch-by-kind shape; no path crate integration. | ~100 |
| **Total emitted skinny LOC** | | **~1,185** |

V1 baseline is 3,500 → 3,570 (ARCH §12.2). The skinny cuts emitted LOC by
~66% because it omits the `host.rs` host-shim body, the path-schema
sidecar, the visitor's path integration, and the layout-derived view
narrowing.

### 6.3 Emitted parser entry sketch

```rust
// runtime/src/grammars/json/parser.rs (skinny, hand-sketched)
use crate::tape::{Tape, NodeKind, Span};
use super::generated::parse_json;
use super::view::JsonRoot;

pub struct Json;

impl Json {
    /// Parse a `&str` into a `JsonRoot<'i>`.
    ///
    /// The returned root borrows from both the input slice and a tape owned by
    /// the parser state. The cold owned wrapper lives in SUBSTRATE and is not
    /// part of the SOTA hot path.
    pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError> {
        let mut state = ParserState::new(input);
        // Pre-scan: structural-alphabet SIMD index over the whole input.
        // The index is a Vec<u32> of byte offsets matching any of
        // { '{', '}', '[', ']', ',', ':', '"' }.
        let scan = simd_scan::structural_index_exact(
            input.as_bytes(),
            &STRUCTURAL_ALPHABET_JSON,
        );
        state.attach_index(scan);
        // Body.
        parse_json(&mut state)?;
        // Materialise the typed root from the finished tape.
        Ok(state.finish::<JsonRoot<'_>>())
    }
}

pub struct ParserState<'i> {
    input: &'i [u8],
    cursor: usize,
    tape: Tape<'i>,
    scan: Option<StructuralIndex>,
}
```

`Tape<'i>`, `StructuralIndex`, `simd_scan::structural_index_exact`, and
`JsonRoot<'i>` are SUBSTRATE / external contracts. The compiler emits the
calling shape; SUBSTRATE provides the implementations.

### 6.4 Snapshot regen check

`codegen::verify` runs the regen-equality gate at every build:

```sh
cargo xtask check-json
```

Implementation: re-run the pipeline, compare the emitted token streams to the
committed bytes byte-for-byte. Any drift fails the gate.

The skinny also commits a BIR snapshot under
`crates/ir/tests/snapshots/json.bir.snap` so a BIR-shape change is detected
even when emitted Rust differs only in formatting.

---

## 7. What's Stubbed In The Skinny

| V1 crate | Skinny status | Per-skip impact on SOTA measurement |
|---|---|---|
| `cost-model` | **Stubbed.** The skinny treats every BIR construction as constant-cost. No `CostFacts`, no `CostDecision`, no scalar score, no Pareto frontier. | Potentially masking until BENCH's alternate-plan stub bounds the tail. If the canonical plan misses SOTA while the alternate stub succeeds, the cost-model/cost-driven-rewrite axis is a recovery lever and RESULTS must say so. |
| `egraph` | **Stubbed.** No e-class, no rewrite, no saturation, no fixpoint. ARCH §10.1 `legality-rewrites` and `normalization-rewrites` (LOAD-BEARING for V1 correctness) are inlined as pre-extraction passes in `passes::normalize`; `cost-driven-rewrites` (ASPIRATIONAL for V1 SOTA) is dropped. | JSON's canonical plan is identifiable at extraction without rewrite search, but BENCH must bound that assumption with the non-egraph alternate-plan stub before calling the cut FAITHFUL. |
| `csp-solver` | **Stubbed.** No constraint store, no propagation, no improvement, no Implication discharge. | None for JSON. Every CSP axis has zero choice on JSON (§4.2). |
| `vm` | **Stubbed.** No interpreter, no replay, no debug trace. | None for SOTA. VM is a debug/test artefact. The skinny does not have the `vm::replay` golden gate. |
| `bbnf-language-server` | **Stubbed.** No LSP, no editor integration. | None for SOTA. LSP is a developer-experience artefact. |
| `path` / `path-core` | **Stubbed.** No `path!` macro, no path schema, no typed selector. | None for SOTA. Path queries are user-facing; SOTA measures parse-only throughput. The emitted runtime exposes `JsonRoot<'i>` directly without path glue. |

Aggregate cut: the V1 24-crate spec drops to ~10 crates in the skinny
(SUBSTRATE owns ~4, COMPILER owns ~5, BENCH owns ~1). The skinny compiler
slice's crate footprint inside that 10:

| Crate | Skinny LOC budget |
|---|---|
| `bbnf` (CLI + driver) | ~400 |
| `grammar` (BBNF surface IR + partial parser for `json.bbnf`) | ~800 |
| `ir` (Grammar IR + BIR types) | ~500 |
| `passes` (validate + layout/types + layout-passthrough + skinny-only shapes/recognizer fixtures + extract) | ~1,500 |
| `codegen` (lower::rust + runtime_template + verify) | ~1,200 |
| **Compiler skinny crate budget** | **~4,400** |

WORKSPACE sets the binding numbers; this row is the COMPILER slice's input.

---

## 8. The Compile-And-Test Loop

The developer runs four commands end-to-end on the skinny:

| Command | What it does | Expected outcome |
|---|---|---|
| `cargo build -p bbnf` | Build the CLI driver. Compiles `grammar`, `ir`, `passes`, `codegen`, `bbnf`. | Clean build; warnings allowed but no errors. |
| `cargo xtask regen-json` | Run the full pipeline: load `grammars/json.bbnf` → parse → validate → infer → mine shapes → extract BIR → lower to Rust → write `runtime/src/grammars/json/`. | Six files written (`generated.rs`, `parser.rs`, `host.rs`, `view.rs`, `value.rs`, `visitor.rs`). Total emitted LOC ≤ skinny budget (~1,185). |
| `cargo xtask check-json` | Re-run the pipeline and compare bytes to committed runtime files. Fail on any drift. | Exit 0 = unchanged. Exit 1 with diff = drift. |
| `cargo test -p runtime --test json_parity` | BENCH-owned parity test: parse a fixture corpus through the generated parser and through a hand-coded JSON parser; compare outputs. Confirms generated-parser correctness before SOTA bench runs. | All fixture cases pass. |

### 8.1 Bench handoff

After the four commands above pass, the BENCH slice runs:

```sh
cargo bench -p bbnf-bench --bench json_parity -- twitter
cargo bench -p bbnf-bench --bench json_parity -- citm
cargo bench -p bbnf-bench --bench json_parity -- canada
cargo bench -p bbnf-bench --bench simd_scan -- twitter
```

Those commands are owned by BENCH.md; the COMPILER slice's only
responsibility is that the four commands above produce a generated parser
that parses the BENCH fixtures correctly.

### 8.2 Compile-and-test loop end-to-end

```text
edit grammars/json.bbnf
  -> cargo build -p bbnf
  -> cargo xtask regen-json
  -> cargo xtask check-json                            (catches drift if commit forgotten)
  -> cargo test -p runtime --test json_parity         (correctness)
  -> cargo bench -p bbnf-bench --bench json_parity    (BENCH-owned; SOTA gate)
  -> cargo bench -p bbnf-bench --bench simd_scan      (BENCH-owned; SIMD floor)
```

A full clean loop (cold cargo cache) is targeted at ≤ 4s wall time for the
build step (matches PASS-2 §6 row for json: ≤ 4s wall) and ≤ 30s including
the parity test. Bench is separately budgeted by BENCH.

---

## 9. Open Questions And Source-Authority Conflicts

Per the brief's instruction to flag contradictions, two source-authority
points surfaced during exploration:

### 9.1 Layout subroutine ownership in the skinny

ARCH §8.2 + Lock 2 require HM/CSP type checking to run as an internal
subroutine of `passes::layout`; `LayoutFacts` is the public side-table.
ARCH §6 invariant table reads `passes::layout (HM + bidirectional + CSP run as
a subroutine inside layout lowering per Lock 2)`. The skinny keeps Algorithm-W
under `passes::layout::types` but still **inverts the call hierarchy**: the
skinny pipeline calls HM first, then `passes::layout` pass-through wraps the
resulting `TypeFacts` as `LayoutFacts`. This is a deliberate scope cut (JSON has
zero layout policy and zero CSP), and the file placement makes V1 closure
mechanical: real layout lowering later calls the same Algorithm-W module as an
internal subroutine, then adds DK13/GADT/CSP siblings without rewriting it.

### 9.2 Host-fn-free claim for JSON

The brief states "JSON is host-fn-free in the skinny per ARCH §12.1." ARCH
§12.1 is the **YAML onboarding walkthrough**, not a JSON declaration. The
authoritative JSON row at ARCH §12.2 reads `Host route: metadata + numeric/
string host fns from host::primitives`. The committed
`grammar/json/json.bbnf` declares `decode_json_string_to_arena(input)`. The
skinny's host-fn-free decision is a deliberate skinny scope cut (§1.3) but
is **not** sourced from ARCH §12.1 verbatim. The skinny's interpretation:
ARCH §12.1 documents the YAML onboarding pattern as host-fn-optional, and
the skinny adapts that pattern to JSON to delete the `host` and
`csp-solver` crates from the slice. The trade-off is documented at §1.3.

---

## 10. Summary

| Dimension | V1 spec | Skinny |
|---|---|---|
| Grammars supported | 9 + yaml probe | 1 (JSON only) |
| Grammar IR variants reachable | 14 | 9 |
| BIR variants reachable | 20 | 14 |
| Type system mechanisms | 5 (HM + bidirectional + DK13 + CSP + GADT) | 1 (HM) |
| Pipeline phases | 13 | 8 |
| Compiler crates | ~24 (full) | ~5 (compiler slice) |
| Emitted Rust LOC for JSON | 3,500–3,570 | ~1,185 |
| HM checker LOC | ~3,500 | ~660 |
| `host.rs` | host-fn body | empty |
| `path-schema.toml` | emitted | not emitted |
| Recognizer mining | telemetry-driven miner | hand-curated nominator (~40 LOC) |
| Shape mining | telemetry-driven miner | hand-curated table (~80 LOC) |
| Cost model | Pareto frontier + scalar score | constant cost + alternate-plan stub bound |
| E-graph rewrites | legality + normalization + cost-driven | none (legality + normalization inlined) |

The skinny's architectural premise: **if JSON SOTA falls out of the skinny
substrate + extraction shape (single plan, no rewrites, hand-curated
recognizer, host-fn-free) after the host-call and alternate-plan probes pass,
the V1 spec's elaborate machinery — DK13, GADT, CSP, e-graph, cost-model,
miner — is evidence-backed as tail-of-distribution correctness/coverage
machinery rather than load-bearing for JSON throughput. If JSON SOTA misses,
the measurement diagnostic identifies whether the miss is substrate
(SUBSTRATE owns), extraction shape or cost-plan masking (COMPILER owns), or
codegen (COMPILER owns) — and tranches A-J commit only with a calibrated prior
on which V1 axes the SOTA budget actually depends on.**

The compiler slice's job is to produce a generated parser whose performance
ceiling is set by the SUBSTRATE, not by the compiler. Every cut in this
spec is a cut to compiler-side machinery whose absence cannot lower the
ceiling — only correctness coverage. The skinny tests the ceiling.
