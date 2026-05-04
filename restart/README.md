# Restart — Greenfield Re-architecture (2026-05-04)

The bbnf-lang corpus, surveyed across two restart attempts, returns to first principles. The prior `restart/` is archived at `restart-archive-2026-05-04/` (commit history preserved; the audit material survives as a research corpus, not as a plan-set). This README is the new anchor — the synthesis of the user's 35-answer interrogation, the ffuzzy three-primitive insight, the 14 locks (carried forward), and the precepts (`docs/precepts/`, submodule, governs voice and process).

The greenfield mandate is unambiguous: **no quick solutions, no workarounds, no legacy code uncontested, no contrivance, no overfitting.** Architectural transpositions for elegance, simplicity, and performance are mandatory. The user-facing API is familiar (sonic-rs, lightning-css, jq idioms); the internals are the apotheosis (CSP + e-graph + shape mining + cost model + bidirectional inference + grammar-derived everything). The substrate identity is settled. The path forward is five prompts.

---

## §1 — The Anthem

> *Everything is grammar-derived.*

The grammar source file (`<name>.bbnf`) plus the workspace metadata block (`[workspace.metadata.bbnf.grammars.<name>]`) are the **two onboarding surfaces.** Adding a 10th grammar `yaml.bbnf` requires (a) the source file and (b) the metadata block — nothing else, no Rust crate, no per-grammar match arm in any generic crate. Per-grammar host functions decompose into composition of generic primitives expressed in workspace metadata or in extended-BBNF directives; the optional declaration crate (Lock 14's escape valve) is not used for any of the 9 extant grammars.

What is *not* grammar-derived is small and explicit:

- The 14 locks themselves
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
| `bbnf` | user-facing aggregator (re-exports Parser, Value, Document, Visitor, pointer!, select!) | yes | crates.io |
| `bbnf-cli` | user-facing CLI tool | yes | crates.io |
| `bbnf-language-server` | LSP (consolidates analysis + lsp) | yes | crates.io |
| `bbnf-bench` | vitest-style bench harness | yes | workspace-internal |
| `error` | unified error type (`bbnf::Error` re-export) | no | workspace-internal |
| `pipeline` | phase-state pipeline coordinator | no | workspace-internal |
| `source` | input acquisition + line-column infra | no | workspace-internal |
| `grammar` | grammar source loading + metadata reading | no | workspace-internal |
| `ir` | Grammar IR + Backend IR types (no passes) | no | workspace-internal |
| `passes` | every transformation pass (consumes/produces IR) | no | workspace-internal |
| `vm` | bytecode VM (kept per Q7; CSP/egraph rule oracle + debug runtime) | no | workspace-internal |
| `codegen` | per-backend lowerers (Rust + WASM; TS deferred per Q28) | no | workspace-internal |
| `runtime` | runtime substrate + per-grammar template-emitted subdirs | no | workspace-internal |
| `host` | host-fn dispatch + generic primitive library (`prims` as module) | no | workspace-internal |
| `cost-model` | Cost trait + per-construct/per-rule/per-path costs | no | workspace-internal |
| `path` | Rust `pointer!` + `select!` proc-macro shells | no | crates.io (with bbnf) |
| `path-core` | shared path-AST + lex + lower + validate + runtime | no | crates.io (with bbnf) |
| `path-ts` | TS proc-macro / cdylib (deferred per Q28) | no | deferred |
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
| **Backend IR** | `ir` | ~22 (per BC.W0 starting point; refines in PASS-1) | the codegen contract; per-backend lowerers consume; uniform across Rust + TS + WASM |

The "optimised IR" of the prior plan is **Grammar IR with extra metadata** — not a third type. The CSP-typed information, the cost annotations, the shape-mining hints, the layout decisions all live as side-tables keyed by Grammar IR node IDs. This is the rust-analyzer / Salsa pattern.

**Grammar-derived value type: hybrid (Q10).** Leaf rules become slice/scalar wrappers (`type Identifier<'i> = &'i str`); compound Seq rules become structs (`struct ColorFunction<'i> { name: &'i str, args: Vec<...> }`); Alt rules become enums (`enum CssValue<'i> { Color(Color<'i>), Length(Length<'i>), ... }`); Repeat becomes Vec; Optional becomes Option. Grammar shape dictates which. The codegen synthesises all four typed-record shapes from grammar IR.

**Deep enum support.** Grammar Alts with mixed-shape branches (some leaf, some compound) generate deeply-typed enums with full pattern-match support. Visitor surface (Q26) covers traversal; pointer macro (Q24) covers compile-time access.

---

## §5 — BBNF Extensions (Settled Positions Q12-Q13, ffuzzy primitives)

The greenfield BBNF folds in **three independently-motivated primitives** (per ffuzzy `docs/ffuzzy.md`, decoupled from any specific consumer) plus **four greenfield additions** the audit surfaced.

### Three primitives (from ffuzzy)

| Primitive | Syntax | Use case beyond ICU |
|---|---|---|
| **Rewrite mode** | `#[parser(path = "...", mode = "rewrite")]` produces `apply(input: &str) -> String` instead of typed AST | sed/awk pipelines, URL slug sanitisation, code migration tools, log scrubbing, source-map-preserving edits, Markdown-to-HTML inline rewriting |
| **Bounded-width lookbehind** | `"s" \|< "ch" -> "k"` — match `"ch"` only if preceded by `"s"`; cursor advances by `\|"ch"\|` only | context-sensitive tokenization, type-vs-value disambiguation in C++/Rust/Scala, Markdown's `*` after whitespace vs letter, delimiter-sensitive parsing, lookbehind-as-regex |
| **Unicode char-class algebra** | `[:L:]`, `[:Cyrillic:]`, `A & B`, `A - B`, `A \| B`, `^A`, named productions like `consonants = [:L:] - [aeiou]` | Unicode identifier parsing (`XID_Start` / `XID_Continue`), script-restricted grammars, mathematical notation grammars |

These are upstream BBNF additions that also cover ICU rule compilation as a downstream consequence. The egraph `Language` derive subsumes the new-IR-node need ffuzzy initially proposed; no new IR variant is required for transducers — they emerge from rewrite-mode + lookbehind + Unicode sets composed.

### Four greenfield additions

| Addition | Syntax sketch | Rationale |
|---|---|---|
| **`@host fn` directive** | `@host fn parse_hex_color(s: regex("#[0-9a-fA-F]{6}")) -> Color { Color::Rgb(parse_hex_pair($1[1..3]), parse_hex_pair($1[3..5]), parse_hex_pair($1[5..7])) }` | host-fn definition in-grammar, declarative; composed of generic primitives; closure semantics inspired by Rust |
| **Generic rules** | `Object<V> = "{" pair<V> ("," pair<V>)* "}"; pair<V> = String ":" V` | DRY; faster grammar authoring; smaller grammar source. CSP solves type-variable propagation; codegen monomorphises per call site |
| **`@error(skip \| recover \| halt)` directive** | per-rule error-recovery hint; defaults to inferred from rule shape | treesitter-class error recovery via "MISSING" / "ERROR" nodes; folds into LSP path |
| **`@layout(struct \| enum \| tuple \| slice)` hint** | optional override when type inference is ambiguous | inference is default (Q21 lean iv); annotations stay where useful (hybrid) |

The shape miner auto-detects Pratt and SIMD opportunities (Q19, Lock 10). No `@pratt` or `@simd` directives — emerges from grammar shape via cost-model decision.

### Multi-function chaining

The ffuzzy gap. Today's BBNF supports `->` for terminal-side type projection; multi-function chaining (`-> f1 -> f2 -> f3`) needs first-class semantics with bidirectional inference. The `@host fn` directive's body uses chained applications via Rust-style `.method()` or piped `|>`. PASS-1 specifies the formal closure semantics, type-variable propagation, and CSP-backed inference algorithm.

---

## §6 — Optimization Apotheosis (Settled Positions Q14-Q19)

The pipeline is **fixed-point co-iteration** with SSA-style discipline. Each phase has explicit input + output IR; each transformation is composable; the egraph is the rewrite substrate; the CSP is the inference substrate; the cost model picks emission shapes.

### Pass ordering

```
1. Parse           (.bbnf  →  Grammar IR)
2. Validate        (well-formedness; reachability; cycle classification)
3. Type inference  (CSP + bidirectional + Pierce-Turner adapted; produces TypedGrammarIR)
                       ┌──── fixed-point co-iteration with (2) ────┐
4. Shape mining    (recogniser miners; identify Pratt operators, SIMD scanners, PHF keywords)
5. E-graph saturation  (algebraic simplification; charclass merging; keyword sets;
                        operator-chain detection; repeat-loop hoisting; tail-call elim;
                        non-progressing-Alt removal)
6. Cost-model extraction  (e-graph  →  optimal-cost AST per cost model)
7. Lower to Backend IR   (TypedGrammarIR  →  ~22-variant Backend IR)
8. Per-backend lower     (Backend IR  →  Rust source / WASM bytes; TS deferred per Q28)
9. Regen-equality verification  (xtask --check; byte-identical re-emission)
```

Type inference (3) and validation (2) co-iterate to fixed-point; left-recursion classification depends on types (typed left-recursion); types depend on validation (well-formed inference domain). Single forward pass of the rest.

### Cost model: hybrid (Q15 lean iv)

Local costs per construct feed e-graph extraction (per-construct shape selection). Global costs per rule feed the strategy resolver (which rules emit Pratt vs descent). Per-path costs handle Pratt LUT propagation in left-recursive operator chains.

### Cost model integration: trait-based (Q16 lean iii)

`Cost` is a trait with `score(&self, ctx: &Context) -> u64` and `branches(&self) -> impl Iterator<Item = (Choice, u64)>`. The parser cost model implements; the regex cost model implements; the comparison logic lives in `cost-model`. Bridging via `Cost` allows the parser to know "this regex scan is X cheap" without knowing regex internals.

### CSP scope (Q18)

CSP is the **central inference substrate**. E-graphs are used only where saturation-style equivalence-class extraction is the natural fit (algebraic rewrites; cost extraction). The two compose by output-piping — CSP infers types and costs; its outputs feed the e-graph's rewrite engine; the e-graph's extracted ASTs feed cost-model selection. A **union system** between CSP variables and e-graph e-classes lives at `passes/csp_egraph_bridge.rs` — when the CSP solves a constraint that names an e-class, the e-class is promoted to a CSP value; when the e-graph extracts an optimal AST that references CSP variables, the CSP's solution is consulted.

### E-graph rewrites (Q17 lean all)

Algebraic, charclass-merging, keyword-set detection, operator-chain detection, repeat-loop hoisting, tail-call elimination, non-progressing-Alt removal — **all** ship V1. Plus more as miners surface them. The `egraph` crate is generic; bbnf-specific rewrites live as passes in `passes/egraph_rewrites/`, plugged in via `linkme` or `inventory`-style registry.

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

## §7 — Type System (Settled Positions Q20-Q23)

**Pierce-Turner local type inference** adapted to grammar shape, with **CSP backing the constraint-collection phase**, with **full Hindley-Milner-style inference** as the canonical algorithm. The PT/HM choice is not exclusive — PT is a *style* of bidirectional check/synth; HM is the underlying inference engine. The greenfield uses Hindley-Milner with bidirectional check/synth at every grammar node, with CSP-backed unification.

The research punt (Q20, Q23): PASS-1's Type System sub-agent does the deep dive against rust-analyzer's `chalk`, GHC's `OutsideIn(X)`, Stephen Dolan's algebraic subtyping, and the latest research (Dunfield-Krishnaswami's bidirectional papers, Pierce-Turner's local inference). PASS-1 commits to a specific algorithm with citations.

**Type annotation surface: hybrid (Q21).** Pure inference is default; first-class explicit annotations welcome where users want them (`rule -> u32` for terminal disambiguation; `rule -> Color` for compound aliases). Generic rules ship V1 (Q22). Subtyping is **full Hindley-Milner with subsumption** (Q23) — the CSP relaxes constraints; coercion is a constraint relaxation; no informal numeric tower bolted on.

---

## §8 — Value API & Path DSL (Settled Positions Q24-Q27)

**All four materialisation surfaces** (Q26): `as_<T>()`, `try_into()`, typed-property access, visitor. Generated uniformly per grammar from grammar shape. Cost: per-grammar codegen size; benefit: every use case has its idiomatic surface.

### Path DSL: dual macro (Q24)

| Macro | Style | Use case |
|---|---|---|
| `pointer!(Json, ["a", "b", 0])` | sonic-rs idiom | compile-time key/index path; typed terminal; random access |
| `select!(Css, "rule > declaration[property=color]")` | XPath/CSS-selector idiom | tree pattern matching; runtime; bitflag-pruned subtree traversal |

Both compile-time (both are proc-macros). Both grammar-derived (read the per-grammar registry that codegen emits). One substrate (`path-core` carries the path AST + lex/lower/validate); two surfaces (`path` ships the proc-macros for both `pointer!` and `select!`).

### Lazy materialisation: tape + direct-to-struct UNION (Q25)

The user's deep concern: it isn't tape's *name* that was the problem — it was 2,000 commits of failed unioning between tape and direct-to-struct. The greenfield's substrate is a **proper union** of:

- **Token stream** — contiguous representation of parsed tokens with offset references; the simdjson structural insight
- **Direct-to-struct** — typed values projected from the token stream on materialisation; the sonic-rs LazyValue idiom

Naming: not "tape" (carries baggage); not "direct-to-struct" (incomplete); call it **`ParseStream`** — a contiguous token stream that direct-to-struct values borrow into. Materialisation walks ParseStream offsets; the contiguous representation is cache-locality-optimal; the typed values borrow slices into the source string + carry ParseStream offsets. PASS-3 specifies the ParseStream layout, the typed-value-borrow shape, and the materialisation cost.

### Mutation: read-write visitor only (Q27)

Direct typed-property mutation is unsound under slice-borrow (mutation invalidates borrowed slices); the visitor pattern's `&mut Value` controls lifetime. lightning-css idiom. PASS-3 specifies VisitTypes bitflag + per-record `visit_<Name>` semantics.

---

## §9 — Performance & Backends (Settled Positions Q28-Q30)

**Backend agnostic in design; Rust + WASM in V1; TS deferred** (Q28). The gate: beat the competitor set per dataset:

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

ARM NEON (M1/M2/M3), x86 AVX2 + AVX-512, WASM-SIMD (wasm-simd128), portable scalar fallback. PASS-2 specifies the per-kernel SIMD coverage matrix.

### Incremental parsing: ii + iii (Q30)

Opt-in feature mode for batch parsers (compile-time `--incremental`); always-on for LSP-class consumers. Treesitter-style: stable node identity + diff-against-prior-tree. PASS-3 specifies the incremental-parse data model + the LSP integration path.

---

## §10 — SOTA Synthesis (Settled Positions Q31-Q33, all-of-the-above)

**Deep-dive for V1 design**:

| Project | Idea adopted | Crate that absorbs |
|---|---|---|
| **simdjson** | contiguous token stream (renamed ParseStream); two-pass parse (structural scan + materialisation); on-demand API | `runtime` (ParseStream) + `host` (escape-handling SIMD primitives) + `simd-scan` (kernels) |
| **sonic-rs** | LazyValue<'a> idiom; pointer! macro; serde-derive-style typed access | `runtime` + `path` + `path-core` |
| **lightning-css** | Visitor + VisitTypes bitflag; per-record `visit_<Name>`; read-write traversal; CSS selector DSL | `runtime` + `path` |
| **treesitter** | error recovery via MISSING/ERROR nodes; lossless concrete syntax tree; external scanners (escape valve for grammars exceeding BBNF expressiveness); query DSL (folded into `select!`); incremental parsing | `runtime` (CST + recovery) + `bbnf-language-server` (incremental); query DSL into `path` |
| **rust-analyzer** | Salsa-style incremental computation (lazy, memoized, on-demand); ungrammar declarative grammars; rowan-style lossless trees; `chalk_ir` for type-system reference | `passes` (Salsa-style); `ir` (rowan-inspired lossless representation); `passes/types` (chalk-inspired) |
| **chumsky** | typed parser combinators (types-out-the-back guide types-in-the-front); Pratt parsing reference; error recovery patterns | `passes/types` (bidirectional inference reference) |
| **logos** | fast lexer-generator codegen idioms; SIMD-aware lexer specialisation | `simd-scan` + `codegen/rust` |
| **regex-automata** | DFA / NFA / hybrid regex engines; the de-facto Rust regex library | `parse-that/regex` (eventual fold) |
| **egg** | e-graph substrate; Language derive (subsumes the new-IR-node ffuzzy initially proposed) | `egraph` (sister crate; egg-inspired or egg-based) |
| **z3** | SMT/CSP propagation reference (AC-3, GAC, conflict-driven backtracking) | `csp-solver` |
| **lalrpop** | LALR codegen idioms; type-driven parser tables (reference; bbnf is not LALR-bound) | `codegen` (reference for table-driven emit) |
| **swc** | hand-written-parser-class iteration speed; NAPI bindings; transformer/codegen separation | `codegen/wasm` (WASM compilation pipeline) |
| **pest** | PEG parser-generator surface (derive-macro UX) | `codegen/rust` (derive-macro UX patterns) |
| **antlr4** | LL(*) parsing reference; multi-target codegen; error reporting | `runtime` (error reporting) |
| **megaparsec / parsec** | Hindley-Milner-typed combinators in Haskell (reference for type system) | `passes/types` |
| **rowan** | lossless concrete syntax tree library (rust-analyzer's CST) | `ir` (CST representation) |

---

## §11 — Locks Carried Forward

The **14 locks** at `restart/locks/14-LOCKS.md` are settled and govern the greenfield. Particular emphasis under the new gestalt:

| Lock | Greenfield posture |
|---|---|
| 1 — Tape + columnar dead | Honoured by the ParseStream union (§8 Q25). Tape's name dies; tape's structural insight folds into ParseStream. Columnar SoA stays buried (per AV.04 archaeology). |
| 2 — Layout-lowering canon | Honoured at `passes/layout/`; `TypeDesc`/`StructLayout`/`TypeMap` aliases retire workspace-wide. |
| 3 — Cursor + byte-skip unified | Honoured at `runtime/parse/`; one parse implementation; eager fast-path elides cursor consultation. |
| 4 — Per-domain orthogonal optimisation | Honoured by `passes` composing `egraph` + `csp-solver` + `cost-model` by output-piping; no fused hypergraph. The CSP↔egraph union (§6) is bridged, not fused. |
| 5 — IR + per-backend lower | Honoured by Backend IR (§4); `codegen/{rust,wasm}/` lowers from Backend IR; TS deferred per Q28. |
| 6 — xtask emits committed source | Honoured; regen artefacts greppable on disk; no proc-macro façade for codegen output. |
| 7 — Path crate consolidation | Honoured by `path` + `path-core` + `path-ts` triplet (§2); `runtime/path.rs` retires (per BA W3c carry). |
| 8 — Surpass SOTA, not AU | Honoured by §9 gate table; every parse-throughput gate cites competitor + dataset + platform. |
| 9 — Slice-borrow primary; bumpalo + owned escape hatches | Honoured by ParseStream's typed-value-borrow shape (§8); `parse(&'i str)` default; `parse_in(&'i str, &Bump)` opt-in; `parse_owned(&str)` escape. |
| 10 — Pratt + SIMD auto-detected | Honoured by shape miners (§6 Q19); no `@pratt` / `@simd` directives. |
| 11 — Path-deps for incubating sister crates | Honoured by `egraph`, `csp-solver`, `parse-that` path-deps; publication when API stabilises (per Q3 + Q8). |
| 12 — ser + gorgeous archive ceremony | **Precondition for execution**; Tranche A.W0 owns it. `crates/ser/` + `crates/gorgeous/` archive at `archive/`; workspace `members` reduced. |
| 13 — No god directories; cohesive encapsulation | Honoured by every crate's `src/` tree (PASS-1/2/3 specify); 4-10 children per dir; no >500 LOC outside `generated/`. |
| 14 — Full grammar generalisation; zero overfitting | Honoured by §1 anthem + §2 workspace shape + §5 BBNF extensions (host-fn in metadata or `@host fn` directive; no `crates/<grammar>/` declaration crates by default). The future-grammar onboarding test for `yaml.bbnf` is the verification gate. |

---

## §12 — Process & Execution (Settled Positions Q34-Q35)

**Restart-of-restart sequencing: archive first (Q34 lean i).** Done — prior `restart/` is archived at `restart-archive-2026-05-04/` (commit history preserved); this `restart/` starts clean.

**Prompt-suite shape: layer-based (Q35 lean layer-based).** Five prompts:

| Prompt | Path | Layer | Owns |
|---|---|---|---|
| PASS-1 | `restart/prompts/PASS-1-SUBSTRATE.md` | bottom | source / grammar / IR / passes / vm / host / cost-model / egraph / csp-solver / type system / BBNF extensions / error vocabulary |
| PASS-2 | `restart/prompts/PASS-2-CODEGEN.md` | middle | codegen / runtime / runtime-template / per-backend lowerers (Rust + WASM) / generated-output regen / SIMD scanner kernels / Pratt + SIMD auto-detection / cost-model integration |
| PASS-3 | `restart/prompts/PASS-3-RUNTIME.md` | top | bbnf aggregator / value API / path + select DSLs / visitor surface / lazy materialisation / ParseStream union / error recovery / incremental parsing / LSP / CLI / fixtures / playground / docs |
| SYNTHESIS | `restart/prompts/SYNTHESIS.md` | meta | consolidates 3 PASS outputs into ARCHITECTURE.md + MIGRATION.md + the master plan |
| HARDENING | `restart/prompts/HARDENING.md` | gate | nine-lane double-back audit (lock-adherence, sequencing, cohesion, SOTA-anchoring, grammar-authoritative, generated-code budget, friction forecast, carry-deferral, greenfield-discipline) with Pro/Con/Explication/Challenge per-item discipline |

**No Stage-2 hardening. No Stage-3 meta-review. One round.** The three prior stages were a contrivance; the user has flagged this. One PASS suite, one synthesis, one hardening, then tranches.

After hardening returns *ready*, full-spec tranche drafting begins. **Not before.** Tranches are layer-aligned: tranches earning each layer's milestones land in dependency order. The fresh tranche set begins at letter A; the prior BA-BD plan-set survives at `docs/tranches/{BA,BB,BC,BD}/` as inheritance reference, archived to `docs/tranches/archive/legacy-Y-BD/` at Tranche-A.W0 execution time per Pass-C's ratification.

---

## §13 — Voice + Discipline

Per `docs/precepts/instructions/STYLE.md`. Calibrated, trenchant, archaic-permissive ("hereupon", "thereof", "appurtenant", "begotten", "extant"); mild poetic undercurrent; no metalanguage; no commit refs; no "the user said"; no `audit/` cross-references except as ground-truth citations; no soft hedging ("might", "consider", "perhaps"); path:line citations on every concrete claim; tables liberal; per-X tables for every "all-X" claim; no "TBD" / "user adjudicates" / "future without receiver"; no quick solutions; no workarounds; no legacy code uncontested; idiomatic gestalt; architectural transpositions for elegance, simplicity, performance.

---

## §14 — Provenance

This README synthesizes:
- 35-question interrogation (`restart-archive-2026-05-04/INTERROGATION-2026-05-04.md`); user answers ratified
- The ffuzzy three-primitive insight (`docs/ffuzzy.md`)
- The 14 locks (`restart/locks/14-LOCKS.md`)
- The precepts (`docs/precepts/`, submodule, STYLE + LESSONS-LEARNED + ORCHESTRATION + tranche/{SPEC, START, RESEARCH, CHALLENGE, WAVE_SPEC, AGENT_DISPATCH_TEMPLATE, DOC_UPDATE_WAVE})
- The corpus carried forward at `restart/corpora/` (CENSUS, MODULES, RESTART-SKETCH, SOTA, plus the prior Phase-3 8-lane audit syntheses preserved in archive)

The prior `restart/`'s audit material — pass syntheses, per-agent reports, master plan + Amendment 01, Stage-1 + Stage-2 hardening reports — survives at `restart-archive-2026-05-04/audit/`. The greenfield does not relitigate it; the greenfield uses it as **research signal**: which faults the prior plans surfaced are real (per-grammar declaration crates were overfit; tape/direct-to-struct union failed for naming reasons not architectural reasons; OpenFrame had not actually retired across all 9 grammars; the convergent pivot at Tranche E sharpens to staggered closures).

The legacy plan-set at `docs/tranches/{BA,BB,BC,BD}/` is the **inheritance reference** for tranche full-spec drafting. The waves that survive (per the audits) get re-anchored to the new workspace shape; the waves that dissolve (per the audits) retire by mechanism. Inheritance ledger lives at `restart/inheritance/INDEX.md`.

---

## §15 — Closing Posture

Hereupon the greenfield opens. The substrate is the typed-enum + slice-borrow + ParseStream union; the optimization is CSP + e-graph + shape-mining + cost-model with a union-system bridge; the type system is Hindley-Milner + bidirectional + Pierce-Turner-styled; the API is sonic-rs / lightning-css / treesitter familiar with deeper internals; the BBNF extensions are rewrite-mode + lookbehind + Unicode sets + generics + `@host fn` + `@error` + `@layout`; the workspace is 24 crates with the `bbnf-` prefix dropped from internal substrate; the future-grammar onboarding test is two surfaces. The 14 locks govern. The precepts speak.

The five prompts at `restart/prompts/` dispatch next.
