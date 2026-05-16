# Restart — Greenfield Re-architecture (2026-05-04)

The bbnf-lang corpus, surveyed across two restart attempts, returns to first principles. The prior `restart/` is archived at `restart-archive-2026-05-04/` (commit history preserved; the audit material survives as a research corpus, not as a plan-set). This README is the new anchor — the synthesis of the user's 35-answer interrogation, the ffuzzy three-primitive insight, the 16 locks (carried forward), and the precepts (`docs/precepts/`, submodule, governs voice and process).

The greenfield mandate is unambiguous: **no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overfitting.** Architectural transpositions for elegance, simplicity, and performance are mandatory. The user-facing API is familiar (sonic-rs, lightning-css, jq idioms); the internals are the apotheosis (rank-1 HM + local bidirectional checking + finite CSP choices + e-graph rewriting + shape mining + evidence-bearing cost decisions + grammar-derived everything). The substrate identity is settled. The path forward is five prompts.

---

## §1 — The Anthem

> *Everything is grammar-derived.*

The grammar source file (`<name>.bbnf`) plus the workspace metadata block (`[workspace.metadata.bbnf.grammars.<name>]`) are the **two onboarding surfaces.** Adding a 10th grammar `yaml.bbnf` requires (a) the source file and (b) the metadata block — nothing else, no Rust crate, no per-grammar match arm in any generic crate. Per-grammar host functions decompose into composition of generic primitives expressed in workspace metadata or in extended-BBNF directives; the optional declaration crate (Lock 14's escape valve) is not used for any of the 9 extant grammars.

What is *not* grammar-derived is small and explicit:

- The 16 locks themselves
- Workspace metadata schema (the TOML grammar that declares grammars)
- Generic primitives (`parse_int_radix`, `parse_hex_pair`, `cow_unescape`, `regex_captures`, `parse_enum<T>`, `slice_borrow`, …)
- The host-fn dispatch mechanism
- The IR types (substrate; only their instances are grammar-specific)
- The e-graph / CSP / cost-model substrates
- The error-type vocabulary (per-grammar errors carry grammar ident as data, not as type)

If anything else lives hand-written per-grammar in any generic crate, it is overfit and a fault.

---

## §2 — Workspace Shape (Settled Positions Q1-Q8)

Balanced split, granular where befitting, terse where befitting. **The `bbnf-` prefix is dropped from internal workspace crates.** It survives on user-facing crates (`bbnf` aggregator, `bbnf-cli`, `bbnf-language-server`) and is absent from generic substrate that may eventually publish independently (`egraph`, `csp-solver`, `parse-that`, `regex` within parse-that, `simd-scan`).

| Crate | Role | Prefix | Publication |
|---|---|---|---|
| `bbnf` | user-facing aggregator (re-exports Parser, Value, Document, Visitor, path!, select!) | yes | crates.io |
| `bbnf-cli` | user-facing CLI tool | yes | crates.io |
| `bbnf-language-server` | LSP (consolidates analysis + lsp) | yes | crates.io |
| `bbnf-bench` | vitest-style bench harness | yes | crates.io |
| `error` | unified error type (`bbnf::Error` re-export) | no | workspace-internal |
| `pipeline` | phase-state pipeline coordinator | no | workspace-internal |
| `source` | input acquisition + line-column infra | no | workspace-internal |
| `grammar` | grammar source loading + metadata reading | no | workspace-internal |
| `ir` | Grammar IR + Backend IR types (no passes) | no | workspace-internal |
| `passes` | every transformation pass (consumes/produces IR) | no | workspace-internal |
| `vm` | bytecode VM (kept per Q7; CSP/egraph rule oracle + debug runtime) | no | workspace-internal |
| `codegen` | per-backend lowerers (Rust V1; WASM + TS through V2 Backend impls) | no | workspace-internal |
| `runtime` | runtime substrate + per-grammar template-emitted subdirs | no | workspace-internal |
| `host` | host-fn dispatch + generic primitive library (`prims` as module) | no | workspace-internal |
| `cost-model` | Cost trait + per-construct/per-rule/per-path costs | no | workspace-internal |
| `path` | Rust `path!` + `select!` proc-macro shells | no | crates.io (with bbnf) |
| `path-core` | shared path-AST + lex + lower + validate + runtime | no | crates.io (with bbnf) |
| `path-ts` | TS path package (deferred to V2 `TsBackend`) | no | deferred |
| `egraph` | e-graph rewrite substrate (publication candidate) | no | crates.io |
| `egraph-derive` | e-graph derive macro | no | crates.io |
| `csp-solver` | CSP propagation substrate (publication candidate) | no | crates.io |
| `parse-that` | combinator library | no | path-dep until stable; eventual publication |
| `simd-scan` | SIMD scanner kernels | no | workspace-internal until stable |
| `test-fixtures` | per-grammar fixture files (`*.bbnf`, `*.json`, etc., no Rust per-grammar) | no | workspace-internal |

**Final count: ~24 workspace members.** Generic substrate carries no `bbnf-` prefix because it may publish independently with a generic name; user-facing crates retain the prefix because the brand is the API surface.

The `regex` engine eventually folds into `parse-that` (Q8) — published as a generic Rust regex library, not as `bbnf-regex`. Until that fold, it lives at `parse-that/regex/` as a sub-crate of the parse-that workspace member.

### Dependency DAG (acyclic; depth 6)

```
bbnf-cli
  └── bbnf
        ├── codegen
        │     ├── passes
        │     │     ├── egraph (+ egraph-derive)
        │     │     ├── csp-solver
        │     │     ├── cost-model
        │     │     └── ir
        │     │           ├── grammar
        │     │           │     └── source
        │     │           └── error
        │     ├── vm  (consumes IR; emits bytecode)
        │     └── ir
        ├── runtime  (consumes generated source from codegen at compile time, not runtime)
        ├── host  (host-fn dispatch + primitive library)
        ├── path  (proc-macro)
        ├── path-core  (shared logic)
        └── parse-that  (regex engine)

bbnf-language-server  → bbnf
bbnf-bench            → bbnf
test-fixtures          → bbnf (dev-dep only)
simd-scan             → consumed by passes and runtime
```

Six-hop deepest path. Acyclic. The optimization sister crates (`egraph`, `csp-solver`, `cost-model`) are siblings under `passes`, composing by output-piping per Lock 4 — not fused into a unified hypergraph.

---

## §3 — Module Layout per Crate (per-X cohesion)

Every crate's `src/` layout honours Lock 13: 4-10 children per directory, no file >500 LOC outside `generated/`, sibling-API uniformity. The standard is set by sonic-rs, lightning-css, simdjson — not by today's `crates/core/src/runtime/` god-directory.

The full per-crate module structure is the output of **PASS-1 (Substrate)** for `error`, `pipeline`, `source`, `grammar`, `ir`, `passes`, `vm`, `host`, `cost-model`, plus `egraph`, `csp-solver`, `parse-that`. **PASS-2 (Codegen)** for `codegen`, `runtime`. **PASS-3 (User Surface)** for `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `test-fixtures`, `simd-scan`. Each PASS produces full `src/` trees with per-module rationale.

---

## §4 — IR Architecture (Settled Positions Q9-Q11)

**Two IRs.** Precedent: rust-analyzer (HIR + MIR + LIR), LLVM (LLVM IR + MIR), GHC (Core + STG + Cmm). Multiple progressive lowerings is the discipline; mixing concerns across one IR is fault.

| IR | Owner crate | Variant count | Role |
|---|---|---:|---|
| **Grammar IR** | `ir` (with passes operating in `passes`) | ~12-15 | parsed `.bbnf` AST + post-pass annotations (typed, cost-annotated, shape-mined); the optimization domain |
| **Backend IR** | `ir` | 20 (19 semantic variants plus `Return`, post-Phase-8.4 fold) | the codegen contract; per-backend lowerers consume; V1 Rust active, V2 TS/WASM ready without BIR retrofit |

The "optimised IR" of the prior plan is **Grammar IR with extra metadata** — not a third type. The type/layout facts, finite CSP legality facts, cost annotations, shape-mining hints, and layout decisions all live as side-tables keyed by Grammar IR node IDs. This is the rust-analyzer / Salsa pattern.

**Grammar-derived value type: hybrid (Q10).** Leaf rules become slice/scalar wrappers (`type Identifier<'i> = &'i str`); compound Seq rules become structs (`struct ColorFunction<'i> { name: &'i str, args: Vec<...> }`); Alt rules become enums (`enum CssValue<'i> { Color(Color<'i>), Length(Length<'i>), ... }`); Repeat becomes Vec; Optional becomes Option. Grammar shape dictates which. The codegen synthesises all four typed-record shapes from grammar IR.

**Deep enum support.** Grammar Alts with mixed-shape branches (some leaf, some compound) generate deeply-typed enums with full pattern-match support. Visitor surface (Q26) covers traversal; pointer macro (Q24) covers compile-time access.

---

## §5 — BBNF Extensions

The greenfield BBNF carries the following extensions. All land in the V1 plan; nothing is deferred. The ffuzzy three-primitive proposal is reduced — only lookbehind survives as a grammar-level surface; rewrite mode is rejected (the Visitor surface covers it); Unicode is solved at the regex layer, not the grammar layer.

### Lookbehind (`|<`)

Bounded-width lookbehind operator. `"s" |< "ch" -> "k"` matches `"ch"` only when preceded by `"s"`; cursor advances by `|"ch"|` only. The walker tracks a k-byte rolling window of recent input (k = max lookbehind width across all rules); at each position checks whether the window ends with the lookbehind pattern. ~350 LOC across `ir` + `passes` + `runtime`.

Use cases beyond ICU: context-sensitive tokenization (type-vs-value disambiguation in C++ / Rust / Scala grammars); Markdown inline parsing (`*` behaves differently after whitespace vs letter); delimiter-sensitive parsing (`'` as apostrophe vs string quote depends on preceding char); regex-with-lookbehind replacement in grammars that today fall back to hand-written state machines.

### Rich regex with first-class modern Unicode

Grammar-level Unicode char-class algebra (`[:L:]`, `A & B`, etc.) is **not** added. The regex layer at `parse-that/regex/` carries the Unicode coverage instead:

- Latest Unicode standard version (currently 16.0, 2024-09)
- Full property support (`\p{L}`, `\p{XID_Start}`, `\p{XID_Continue}`, `\p{Cyrillic}`, `\p{Mark}`, `\p{Number}`, etc.)
- Script-property regex (`\p{sc=Hangul}`, `\p{sc=Han}`, etc.)
- Set algebra inside regex character classes (`[\p{L}--\p{Mark}]`, `[\p{XID_Start}&&\p{ASCII}]`)
- Grapheme cluster awareness
- Normalisation (NFC, NFD, NFKC, NFKD) as composable regex modifiers
- Named character classes via regex literals: `consonants = /\p{L}--[aeiou]/` is the in-grammar surface

The grammar's regex literals are the rich-Unicode entry point. Grammars needing Unicode (CSS L4 identifier; future Python / Rust / OCaml grammars) express through regex; no separate grammar-level surface required.

### `@host fn` directive

In-grammar host-fn definition with closure semantics:

```bbnf
@host fn parse_hex_color(s: regex("#[0-9a-fA-F]{6}")) -> Color {
  Color::Rgb(parse_hex_pair(s[1..3]), parse_hex_pair(s[3..5]), parse_hex_pair(s[5..7]))
}
```

Closure semantics: lexical scoping; type-variable propagation through bidirectional inference; type-checked against generic primitive library at compile time. The `@host fn` body uses chained applications via Rust-style `.method()` syntax. Named identifiers in scope: the rule's captures (`s` above), the workspace's generic primitives (`parse_hex_pair`, `parse_int_radix`, `cow_unescape`, etc.), and any other `@host fn` declared in the same grammar.

Composition is the canonical surface; declaration-crate escape valve (Lock 14) is the rare last resort.

### Multi-function chaining

`-> f1 -> f2 -> f3` chains type projections. Today's BBNF supports terminal-side `-> Type`; the chain extends this with first-class bidirectional inference. The `@host fn` directive's body uses chained applications. HM gives each stage its principal type, local check/synth checks adjacent edges, and finite CSP participates only when the edge triggers bounded implementation choices such as overload or materialisation selection.

```bbnf
hex_byte = /[0-9a-fA-F]{2}/ -> parse_hex_pair -> u8
color = "#" (hex_byte hex_byte hex_byte) -> tuple_to_color -> Color
```

### Generic rules

`Object<V> = "{" pair<V> ("," pair<V>)* "}"; pair<V> = String ":" V`. HM carries type variables; codegen monomorphises the finite validated call-site set on the Rust V1 line; V2 WASM lowering handles equivalent generic structure through type erasure + dispatch. Grammar-level DRY across grammars sharing structural patterns. Land V1.

### `@error(skip | recover | halt)` directive

Per-rule error-recovery hint. Default is auto-inferred from rule shape via shape mining (treesitter-class error recovery via "MISSING" / "ERROR" nodes). The directive overrides when inference disagrees with author intent. Folds into LSP path.

### `@layout(struct | enum | tuple | slice)` hint

Optional override when type inference is ambiguous. Inference is default (per Q21 hybrid); the hint is the explicit-control surface for cases where users want a specific layout. Land V1.

### Auto-detected (no directive)

Pratt operator chains, SIMD scanner opportunities, PHF keyword sets — all emerge from grammar shape via cost-model decision. No `@pratt` / `@simd` / `@phf` directives. Per Lock 10.

---

## §6 — Optimization Apotheosis (Settled Positions Q14-Q19)

The pipeline is **fixed-point co-iteration** with SSA-style discipline. Each phase has explicit input + output IR; each transformation is composable. Hindley-Milner plus local bidirectional checking owns type inference inside layout lowering; the CSP solver owns finite implementation choices and legality checks; the egraph owns rewrite saturation; the cost model selects among legal alternatives with evidence.

### Pass ordering

```
1. Parse           (.bbnf  →  Grammar IR)
2. Validate        (well-formedness; reachability; cycle classification)
3. Type/layout inference  (rank-1 HM core + Pierce-Turner local check/synth +
                           finite CSP choices inside layout lowering; produces LayoutFacts)
                       ┌──── fixed-point co-iteration with (2) ────┐
4. Shape mining    (recogniser miners; identify Pratt operators, SIMD scanners, PHF keywords,
                    error-recovery boundaries, lookbehind window widths)
5. E-graph saturation  (all 7 rewrite categories: algebraic simplification, charclass merging,
                        keyword-set detection, operator-chain detection, repeat-loop hoisting,
                        tail-call elimination, non-progressing-Alt removal)
6. Cost-model extraction  (e-graph  →  optimal-cost AST per cost model)
7. Lower to Backend IR   (Grammar IR + side tables  →  Backend IR)
8. Per-backend lower     (Backend IR  →  Rust source + WASM bytes; TS scope-deferred per Q28
                          but the Backend IR shape supports TS lower without retrofit)
9. Regen-equality verification  (xtask --check; byte-identical re-emission)
```

Type inference (3) and validation (2) co-iterate to fixed-point with SSA-style discipline; left-recursion classification depends on types (typed left-recursion); types depend on validation (well-formed inference domain). Single forward pass of (4)-(9).

### Cost model: hybrid

Local costs per construct feed e-graph extraction (per-construct shape selection). Global costs per rule feed the strategy resolver (which rules emit Pratt vs descent). Per-path costs handle Pratt LUT propagation in left-recursive operator chains. Every selected strategy carries a `CostDecision`: selected alternative, rejected alternatives, dominated alternatives, objective vector, scalarisation profile, target, extraction method, and any solver-backed legality evidence.

### Cost model integration: trait-based

`CostModel` evaluates a typed candidate set and emits `CostDecision`, not only a scalar score. Parser constructs and regex programs implement the same evidence shape with different domain instances; comparison logic lives in `cost-model`. SMT-style solving (Satisfiability Modulo Theories) is an optional composition backend for constrained objective rows, not the default optimizer and not a replacement for e-graph extraction.

### CSP + e-graph bridge (bridged, not fused)

CSP is the finite-choice and legality substrate. E-graphs are the rewrite + extraction substrate. They compose via an explicit bridge at `passes/csp_egraph_bridge.rs`:

- The bridge maintains stable maps among Grammar IR node IDs, CSP variable IDs, e-class IDs, and extracted node IDs.
- Egraph and CSP exchange monotone facts through bridge tables; non-monotone CSP search state stays inside `csp-solver`.
- Rewrite guards can consult solved legality facts, but an e-node representative is never promoted to truth before extraction.
- `BridgeJustification` records why a rewrite or extraction edge was legal, including egraph explanation refs and CSP explanation refs.

The bridge is real architecture, not a fused type. CSP, egraph, miners, and cost remain separate substrates with explicit interface methods. egglog-style Datalog/equality-saturation fusion is a known SOTA alternative; V1 keeps bridge tables because bbnf needs separate diagnostic ownership, monotone exchange boundaries, and independent stabilization gates.

### E-graph rewrites — all 7 categories V1

| Category | Examples | Owner pass |
|---|---|---|
| Algebraic | `(a \| a)` → `a`; `(a, ε)` → `a`; `(a*)?` → `a*`; `(a+)*` → `a*`; ε-elimination | `passes/egraph_rewrites/algebraic.rs` |
| Charclass merging | `[a-z] \| [A-Z]` → `[a-zA-Z]` | `passes/egraph_rewrites/charclass.rs` |
| Keyword set detection | alternation of literals → PHF candidate | `passes/egraph_rewrites/keyword_phf.rs` |
| Operator-chain detection | left-recursive `expr := expr "+" expr \| term` → Pratt | `passes/egraph_rewrites/operator_chain.rs` |
| Repeat-loop hoisting | `(item separator)*` → repeat-with-separator construct | `passes/egraph_rewrites/repeat_loop.rs` |
| Tail-call elimination | `rule := A rule` → loop | `passes/egraph_rewrites/tail_call.rs` |
| Non-progressing-Alt removal | alternatives whose FIRST sets prove empty | `passes/egraph_rewrites/dead_alt.rs` |

The `egraph` crate is generic (egg-inspired or egg-based); bbnf-specific rewrites live as plug-in passes registered via `linkme` / `inventory`. New rewrites land additively without re-architecture.

### Shape mining (Q19)

Trait-based plugin registry:

```rust
trait ShapeMiner {
    fn name(&self) -> &str;
    fn observe(&self, ir: &GrammarIR, ctx: &Context) -> Vec<Hint>;
}
```

Mindful of combinatorial argument increase: hints carry weights; cost model dampens by weight; saturation-bounded.

---

## §7 — Type System

**Rank-1 Hindley-Milner core + Pierce-Turner local check/synth + finite CSP choices.** Hindley-Milner owns principal schemes for ordinary grammar rules, host functions, and V1 generic rules. Pierce-Turner-style bidirectional checking owns local expected-type flow at annotations, host calls, chain edges, and subsumption sites. CSP does not replace unification; it solves bounded choices HM does not model: host overload selection, layout representation, recognizer eligibility, materialisation mode, recovery strategy, backend erasure, and extraction legality.

**GADT and higher-rank surface: V1, fenced.** Dunfield-Krishnaswami algorithmic completeness and OutsideIn(X)-style implication constraints are V1 commitments where explicit annotations make the program principal. Rank-1 remains the default inferred mode; explicit `forall` annotations and branch-local-equality refinements (`Pattern @ where T = U`) land in V1 and route their body work through tranche D close gates. Missing or ill-typed refinement annotations emit `BBNF-LOCAL-EQUALITY-ANNOTATION`.

**Annotation surface: hybrid.** Pure rank-1 inference is default. First-class explicit annotations are welcome where the author wants control (`rule -> u32`, `rule -> Color`, generic-rule type parameters). Multi-function chaining (`-> f1 -> f2 -> f3`) flows types through stages with a bidirectional check at each adjacent edge.

**Generic rules: V1.** `Object<V> = "{" pair<V> ("," pair<V>)* "}"; pair<V> = String ":" V`. HM carries the type variable and codegen monomorphises the finite validated call-site set on the Rust V1 line; V2 WASM lowering handles equivalent generic structure through type erasure + dispatch. CSP participates only when the generic instance interacts with finite choices such as host overloads, layout, materialisation, recognizer eligibility, backend erasure, or extraction legality.

**Subtyping and coercion: directed checking edges.** Numeric coercion (`i32 → i64 → f64`), lifetime coercion (`&'i str → Cow<'i, str> → String`), and typed-record narrowing are explicit solver candidates at check/synth transition sites. They are not global HM rules and not vague CSP relaxation. A failed edge reports the expected type, actual type, registered coercion candidates, and source span.

**Lookbehind types.** The `|<` operator's left operand is a *constraint* on context, not a *capture* of value. The right operand carries the value. Type system tracks this asymmetry: `("s" |< "ch") -> "k"` has the type of `"ch" -> "k"` (the lookbehind context contributes nothing to the value but everything to the constraint).

---

## §8 — Value API & Path DSL (Settled Positions Q24-Q27)

**All four materialisation surfaces** (Q26): `as_<T>()`, `try_into()`, typed-property access, visitor. Generated uniformly per grammar from grammar shape. Cost: per-grammar codegen size; benefit: every use case has its idiomatic surface.

### Path DSL: dual macro (Q24)

| Macro | Style | Use case |
|---|---|---|
| `path!(Json, ["a", "b", 0])` | sonic-rs idiom | compile-time key/index path; typed terminal; random access |
| `select!(Css, "rule > declaration[property=color]")` | XPath/CSS-selector idiom | tree pattern matching; runtime; bitflag-pruned subtree traversal |

Both compile-time (both are proc-macros). Both grammar-derived (read the per-grammar registry that codegen emits). One substrate (`path-core` carries the path AST + lex/lower/validate); two surfaces (`path` ships the proc-macros for both `path!` and `select!`).

### Lazy materialisation: tape + direct-to-struct UNION

The user's deep concern: the failure was **implementation**, not naming. The greenfield's substrate is a proper tape + direct-to-struct union, called **tape**. The 2,000-commit failure was orthogonal codepaths, type ambivalence, and OpenFrame leakage — not the name. Implemented properly, tape is the right substrate.

Tape is:

- **Contiguous parsed-token stream** — the simdjson structural insight; cache-locality-optimal; SIMD-friendly scan
- **Typed-value-borrow target** — direct-to-struct values borrow into the tape; the sonic-rs LazyValue idiom; materialisation walks tape offsets

Tape carries: token discriminant (kind tag), source span (`(lo, hi)` byte offsets into `&'i str`), payload offset (into a parallel payload arena for non-trivial payloads), structural pointer (sib_skip per AV.04 archaeology — tested at AU peak, then lost to substrate-without-consumer; the greenfield re-introduces under proper consumer wiring).

Typed values borrow into tape:

```rust
// Conceptual; refined in PASS-3
struct JsonValue<'i> {
    kind: TokenKind,        // 1 byte
    span: (u32, u32),       // 8 bytes; into &'i str
    tape: &'i Tape<'i>,     // 8 bytes; pointer
    idx: u32,               // 4 bytes; into tape.tokens
}
```

Materialisation: `value.as_str()` indexes `tape.tokens[idx]` for kind/span, returns a borrowed source slice when the token is unescaped, or projects a normalized string from the payload arena when escaping requires it. Kind/span projection is constant-time. Scalar methods such as `as_i64()` are either parsed-scalar payload reads or digit-linear lazy parses, as declared by `TapeShape`. Object/array iteration walks `tape.tokens[idx..]` until the traversal policy's matching close.

Direct-to-struct sinks receive the same source-span authority. String-like
fields lower to source hooks carrying `(raw, needs_decode)`; defaults share
the retained lazy decode policy, while SOTA sinks may consume a fused
decode+sink primitive once measured. Parser-side eager decode and parallel
string payload trees are not part of the substrate.

Slice-borrow integration: `&'i str` source borrow is the primary lifetime; tape borrows from the same `'i`. Bumpalo opt-in via `parse_in(input, &bump)` returns `JsonValue<'arena, 'i>` where `'arena: 'i`. Owned escape via `parse_owned(input)` deep-copies tape + source.

PASS-3 and the BIR fold specify the user-visible tape semantics, then route exact token byte layout, payload arena classes, sibling-skip/end-pointer choice, typed-value borrow fields, and materialisation-cost classes into Tranche B/F implementation gates. Every rule has a `TapeShape` and `ValueShape`: `TapeShape` owns token kind, span class, payload class, and traversal skip policy; `ValueShape` owns generated field/enum projection over the same node id. Any scalar cache must be declared by one of those shapes.

**This time it lands.** The convergent pivot at Tranche F retires OpenFrame across all 9 grammars in a single architectural movement; the tape lives at `runtime/src/tape/`; per-grammar runtime modules at `runtime/src/grammars/<name>/` borrow into tape via the codegen-emitted accessors. No orthogonal codepath; no parallel substrate; no Vec<OpenFrame> ladder. One authoritative identity; one materialisation contract; one Visitor pattern. Direct structs and red-like views may coexist with tape tokens, but every public node traces to one `(TapeId, node id, payload class)`.

### Mutation: read-write visitor only (Q27)

Direct typed-property mutation is unsound under slice-borrow (mutation invalidates borrowed slices); the visitor pattern's `&mut Value` controls lifetime. lightning-css idiom. PASS-3 specifies VisitTypes bitflag + per-record `visit_<Name>` semantics.

---

## §9 — Performance & Backends

**Backend agnostic in design; V1 measures the Rust line, while WASM and TS defer to V2 `WasmBackend: Backend` / `TsBackend: Backend` without BIR retrofit.** The gate: beat the competitor set per dataset:

| Workload | Competitor floor | bbnf target |
|---|---|---|
| JSON twitter parse (M1 Pro) | sonic-rs 436 µs / simd-json 424 µs | ≤ 380 µs |
| JSON canada parse (M1 Pro) | sonic-rs 3.144 ms | ≤ 2.8 ms |
| JSON citm_catalog parse (M1 Pro) | sonic-rs 854 µs / simd-json 831 µs | ≤ 750 µs |
| CSS bootstrap parse (M1 Pro) | lightning-css ~4.16 ms | ≤ 3.0 ms |
| CSS animate parse (M1 Pro) | lightning-css 1.97 ms | ≤ 1.6 ms |
| simdjson On-Demand sustained | 7 GB/s (Intel Skylake) | ≥ 5 GB/s on M-series; ≥ 7 GB/s on x86 |
| BBNF self-host parse | (no SOTA peer) | < 100 ms full self-parse + format roundtrip |

(CSS L4 lightning-css peer numbers per Phase-3 lane S04-7 require platform-specific ratification; PASS-3's gate sub-agent measures local M1 Pro lightning-css parse-only baseline before final gate-setting.)

### SIMD: first-class everywhere (Q29)

ARM NEON (M1/M2/M3), x86 AVX2 + AVX-512, WASM-SIMD (wasm-simd128), portable scalar fallback. Every eligible recognizer receives SIMD consideration; PASS-2 specifies the per-kernel SIMD coverage matrix. Exact SIMD scans must match scalar offsets. Prefilter SIMD scans emit candidates only, and `RegexProgram` or the scalar verifier accepts the candidate before tape emission. Grammar authors may disable an unsafe or unwanted recognizer through metadata, but they do not force SIMD with syntax.

### Incremental parsing

Opt-in feature mode for batch parsers (compile-time `--incremental`); always-on for LSP-class consumers (`bbnf-language-server`). Treesitter-style: stable node identity per parsed token; diff-against-prior-tree algorithm; minimal re-parse window per edit; lossless concrete syntax tree (rowan-inspired).

PASS-3 specifies: the incremental-parse data model (per-token stable identity; tree-edit primitives; diff algorithm); the LSP integration (`Lsp` events emit incremental requests; bbnf-language-server consumes; partial re-parses propagate to type inference + cost-model; the e-graph caches survive across edits where invariants hold).

The VM (kept per Q7) is the debug + replay runtime: incremental edits replay through the VM to produce stepped diagnostics; the LSP exposes "show parse trace" as a feature on top.

---

## §10 — SOTA Synthesis

**All 16 project influences deep-dive.** Per Q31. This table is an influence catalogue, not the research-agent count: the Wave 5 research fold used eight topic agents, each with verified primary-source slots. Each project below contributes specific architectural insight; evidence-bearing claims still follow the source classification in `restart/research/INDEX.md`.

| Project | Idea adopted | Crate that absorbs |
|---|---|---|
| **simdjson** | contiguous tape (token stream + payload arena); two-pass parse (structural scan + materialisation); on-demand API; SIMD escape-handling primitives | `runtime/tape` + `host/prims` + `simd-scan` |
| **sonic-rs** | LazyValue<'a> idiom; compile-time path macro idiom; serde-derive-style typed access; on-demand JSON API | `runtime` + `path` + `path-core` |
| **lightning-css** | Visitor + VisitTypes bitflag; per-record `visit_<Name>`; read-write traversal; CSS selector DSL | `runtime` + `path` |
| **treesitter** | error recovery via MISSING/ERROR nodes; lossless concrete syntax tree; external scanners (escape valve for grammars exceeding BBNF expressiveness); query DSL (folded into `select!`); incremental parsing | `runtime` (CST + recovery) + `bbnf-language-server` (incremental); query DSL into `path` |
| **rust-analyzer** | Salsa-style incremental computation (lazy, memoized, on-demand); ungrammar declarative grammars; rowan-style lossless trees; `chalk_ir` for type-system reference | `passes` (Salsa-style); `ir` (rowan-inspired lossless representation); `passes/types` (chalk-inspired) |
| **chumsky** | typed parser combinators (types-out-the-back guide types-in-the-front); Pratt parsing reference; error recovery patterns | `passes/types` (bidirectional inference reference) |
| **logos** | fast lexer-generator codegen idioms; SIMD-aware lexer specialisation | `simd-scan` + `codegen/rust` |
| **regex-automata** | DFA / NFA / hybrid regex-engine research pressure only; no oracle role survives V1 | deletion archaeology; `parse-that-regex` owns internal VM/lazy-DFA/full-DFA parity |
| **egg** | e-graph substrate; Language derive (subsumes the new-IR-node ffuzzy initially proposed) | `egraph` (sister crate; egg-inspired or egg-based) |
| **z3** | SMT reference for constrained legality and objective checks; finite CSP propagation reference (AC-3, GAC, conflict-driven backtracking) | `csp-solver` + `cost-model` |
| **lalrpop** | LALR codegen idioms; type-driven parser tables (reference; bbnf is not LALR-bound) | `codegen` (reference for table-driven emit) |
| **swc** | hand-written-parser-class iteration speed; NAPI bindings; transformer/codegen separation | `codegen/wasm` (WASM compilation pipeline) |
| **pest** | PEG parser-generator surface (derive-macro UX) | `codegen/rust` (derive-macro UX patterns) |
| **antlr4** | LL(*) parsing reference; multi-target codegen; error reporting | `runtime` (error reporting) |
| **megaparsec / parsec** | Hindley-Milner-typed combinators in Haskell (reference for type system) | `passes/types` |
| **rowan** | lossless concrete syntax tree library (rust-analyzer's CST) | `ir` (CST representation) |

---

## §11 — Locks Carried Forward

The **16 locks** at `restart/locks/LOCKS.md` are settled and govern the greenfield. Particular emphasis under the new gestalt:

| Lock | Greenfield posture |
|---|---|
| 1 — Tape + columnar dead | **Reframed.** Tape is the substrate of the greenfield, properly implemented (per §8). The lock retired the *prior failed implementation* — orthogonal codepaths, OpenFrame parallel substrate, type ambivalence, the Vec<OpenFrame>::clone 86.07% pathology. The greenfield's tape is unioned with direct-to-struct (no parallel substrate); columnar SoA stays buried (AV.04 archaeology). The lock's spirit (no parallel-substrate failure) honours; the lock's letter (don't use the name "tape") is amended — tape is the right name for the right insight when implemented properly. |
| 2 — Layout-lowering canon | Honoured at `passes/layout/`; `TypeDesc`/`StructLayout`/`TypeMap` aliases retire workspace-wide. |
| 3 — Cursor + byte-skip unified | Honoured at `runtime/parse/`; one parse implementation; eager fast-path elides cursor consultation. |
| 4 — Per-domain orthogonal optimisation | Honoured by `passes` composing `egraph` + `csp-solver` + `cost-model` by output-piping; no fused hypergraph. The CSP↔egraph relation (§6) is bridged, not fused; bridge tables carry stable IDs, monotone facts, and justifications rather than e-node representatives as truth. |
| 5 — IR + per-backend lower | Honoured by Backend IR (§4); `codegen/rust/` lowers from Backend IR in V1; WASM/TS defer to V2 Backend impls. |
| 6 — xtask emits committed source | Honoured; regen artefacts greppable on disk; no proc-macro façade for codegen output. |
| 7 — Path crate consolidation | Honoured by `path` + `path-core` on the V1 Rust line, with `path-ts` deferred to V2 (§2); `runtime/path.rs` retires (per BA W3c carry). |
| 8 — Surpass SOTA, not AU | Honoured by §9 gate table; every parse-throughput gate cites competitor + dataset + platform. |
| 9 — Slice-borrow primary; bumpalo + owned escape hatches | Honoured by tape-backed typed-value borrows (§8); `parse(&'i str)` default; `parse_in(&'i str, &Bump)` opt-in; `parse_owned(&str)` escape. |
| 10 — Pratt + SIMD auto-detected | Honoured by shape miners (§6 Q19); no `@pratt` / `@simd` directives. |
| 11 — Path-deps for incubating sister crates | Honoured by `egraph`, `csp-solver`, `parse-that` path-deps; publication when API stabilises (per Q3 + Q8). |
| 12 — ser + gorgeous archive ceremony | **Precondition for execution**; Tranche A.W0 owns it. `crates/ser/` + `crates/gorgeous/` archive at `archive/`; workspace `members` reduced. |
| 13 — No god directories; cohesive encapsulation | Honoured by every crate's `src/` tree (PASS-1/2/3 specify); 4-10 children per dir; no >500 LOC outside `generated/`. |
| 14 — Full grammar generalisation; zero overfitting | Honoured by §1 anthem + §2 workspace shape + §5 BBNF extensions (host-fn in metadata or `@host fn` directive; no `crates/<grammar>/` declaration crates by default). The future-grammar onboarding test for `yaml.bbnf` is the verification gate. |
| 15 — Build-profile + fusion + i-cache discipline | **Lands 2026-05-12 after V9.2 lazy-tape refutation + six-agent comparative-profile cohort.** Every generated runtime crate ships `[profile.release] lto=fat codegen-units=1 panic="abort" debug=true`; force-inline on Grammar IR's mined hot call-graph (`LayoutFacts.hot_call_graph`); target hot-function size ≤ 20 KiB post-LTO (yyjson reference ~18 KiB; current JSON 7,304-byte sub-budget). Diagnostics: `BBNF-FORCE-INLINE-MISSED`, `BBNF-ICACHE-BUDGET-EXCEEDED`. |
| 16 — SIMD/ASM admissibility allowlist + abstract primitive lifts | **Lands 2026-05-12.** Admissible primitives are an explicit allowlist with per-row citation (Lemire, Validark, Mula, Sneller, Travis Downs, dav1d/ffmpeg/VLC lineage). arm64 NEON: `vqtbl4q_u8`, interleaved movemask, `vld1q_u8_x4`, LD4-interleaved classifier, BCAX/EOR3 ternary, svmatch_u8 emulation. x86_64 AVX-512: `_mm512_mask_compressstoreu_epi8`, ternary mask fusion, `vpermi2b`, `_mm512_alignr_epi8`, k-mask arithmetic family, VPCLMULQDQ-512, AVX-IFMA, VNNI, BITALG, GFNI. Handwritten `asm!` admissible only when the equivalent intrinsic is absent from `core::arch::*`. |

---

## §12 — Process & Execution

**Restart-of-restart sequencing: archive first.** Done — prior `restart/` is archived at `restart-archive-2026-05-04/` (commit history preserved); this `restart/` starts clean.

**Prompt-suite shape: main entry + sub-orchestrators + per-target spec.** Five prompts:

| Prompt | Path | Role |
|---|---|---|
| ORCHESTRATOR | `restart/prompts/ORCHESTRATOR.md` | main entry; phase-identification protocol; phase-type fan-out; hardening-cycle naming canon |
| HARDENING-ORCHESTRATOR | `restart/prompts/sub-orchestrators/HARDENING.md` | sub-orchestrator for hardening cycles V1 through V8+; coordinates four parallel hardener agents and the consolidation |
| RESEARCH-FOLD-ORCHESTRATOR | `restart/prompts/sub-orchestrators/RESEARCH-FOLD.md` | sub-orchestrator for research deep-dives + fold cycles |
| AMENDMENT-DISPATCH | `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` | sub-orchestrator for verify-then-patch amendment cycles after AMENDMENT-REQUIRED / SIMPLIFY-AVAILABLE verdicts |
| HARDENING | `restart/prompts/audit-specs/HARDENING-LENS-SET.md` | per-target audit specification; lens contract (lenses A-K post-Phase-8.1); load-bearing input to every HARDENING-ORCHESTRATOR dispatch |

**Cold-start reading order.** Any cold-start agent reads in sequence:

1. `restart/HANDOFF.md` — orientation; current verdict; next move
2. `restart/prompts/ORCHESTRATOR.md` — phase-identification + dispatch protocol
3. `restart/README.md` — gestalt + 16 locks anchor
4. `restart/locks/LOCKS.md` — settled commitments
5. `restart/audit/hardening/HARDENING-CONSOLIDATED-V{N}.md` (most recent) — operating verdict
6. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`

The PASS dispatch prompts that produced the V1 trio (`PASS-1-SUBSTRATE.md`, `PASS-2-CODEGEN.md`, `PASS-3-RUNTIME.md`, `SYNTHESIS.md`) and the original combined hardening orchestrator retired at Phase 8.0; the PASS syntheses live at `restart/audit/pass-{1,2,3}-*/PASS-{1,2,3}.md`, the SYNTHESIS trio lives at `restart/{ARCHITECTURE,MIGRATION,MASTER-PLAN}.md`, and the orchestrator surface restructured into the five prompts above at Phase 8.1.

After hardening returns *ready*, full-spec tranche drafting begins. **Not before.** Tranches are layer-aligned: tranches earning each layer's milestones land in dependency order. The fresh tranche set begins at letter A; the prior BA-BD plan-set survives at `docs/tranches/{BA,BB,BC,BD}/` as inheritance reference, archived to `docs/tranches/archive/legacy-Y-BD/` at Tranche-A.W0 execution time per Pass-C's ratification.

---

## §13 — Voice + Discipline

Per `docs/precepts/instructions/STYLE.md`. Calibrated, trenchant, archaic-permissive ("hereupon", "thereof", "appurtenant", "begotten", "extant"); mild poetic undercurrent; no metalanguage; no commit refs; no "the user said"; no `audit/` cross-references except as ground-truth citations; no soft hedging ("might", "consider", "perhaps"); path:line citations on every concrete claim; tables liberal; per-X tables for every "all-X" claim; no "TBD" / "user adjudicates" / "future without receiver"; no quick solutions; no workarounds; no legacy code uncontested; idiomatic gestalt; architectural transpositions for elegance, simplicity, performance.

---

## §14 — Provenance

This README synthesizes:
- 35-question interrogation (`restart-archive-2026-05-04/INTERROGATION-2026-05-04.md`); user answers ratified
- The ffuzzy three-primitive insight (`docs/ffuzzy.md`)
- The 16 locks (`restart/locks/LOCKS.md`)
- The precepts (`docs/precepts/`, submodule, STYLE + LESSONS-LEARNED + ORCHESTRATION + tranche/{SPEC, START, RESEARCH, CHALLENGE, WAVE_SPEC, AGENT_DISPATCH_TEMPLATE, DOC_UPDATE_WAVE})
- The corpus carried forward at `restart/corpora/` (CENSUS, MODULES, RESTART-SKETCH, SOTA, plus the prior Phase-3 8-lane audit syntheses preserved in archive)

The prior `restart/`'s audit material — pass syntheses, per-agent reports, master plan + Amendment 01, Stage-1 + Stage-2 hardening reports — survives at `restart-archive-2026-05-04/audit/`. The greenfield does not relitigate it; the greenfield uses it as **research signal**: which faults the prior plans surfaced are real (per-grammar declaration crates were overfit; tape/direct-to-struct union failed for naming reasons not architectural reasons; OpenFrame had not actually retired across all 9 grammars; the convergent pivot at Tranche E sharpens to staggered closures).

The legacy plan-set at `docs/tranches/{BA,BB,BC,BD}/` is the **inheritance reference** for tranche full-spec drafting. The waves that survive (per the audits) get re-anchored to the new workspace shape; the waves that dissolve (per the audits) retire by mechanism. Inheritance ledger lives at `restart/inheritance/INDEX.md`.

---

## §15 — Closing Posture

Hereupon the greenfield opens. The substrate is tape identity plus direct-to-struct projections over one slice-borrow contract; the optimization is e-graph rewriting, finite CSP legality, shape mining, and evidence-bearing cost decisions with a bridged-not-fused fact exchange; the type system is rank-1 Hindley-Milner plus Pierce-Turner local check/synth and directed subsumption edges; the API is sonic-rs / lightning-css / treesitter familiar with deeper internals; the BBNF extensions are lookbehind + generics + block-bodied `@host fn` + multi-function chaining + `@error` + `@layout`, with rich Unicode routed through `parse-that/regex`; the workspace is 24 crates with the `bbnf-` prefix dropped from internal substrate; the future-grammar onboarding test is two surfaces. The 16 locks govern. The precepts speak.

The five prompts at `restart/prompts/` dispatch next.
