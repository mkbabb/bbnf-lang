# LOCK-7 · Family 7C — THE HOST-FUNCTION LATTICE (pass 1, research)

Totality is not a boolean and not a slogan. It is a measured percentage of a grammar's
behavioral surface that lives *inside* the grammar plane, bounded below by a **typed
host-function interface** through which — and only through which — context-dependent and
recursive-with-state semantics escape. This family draws the lawful boundary of the
constitution's clause (i) ("everything is grammar-driven … save primitives and combinators")
by making "save" a *typed, declared, counted* residue rather than an unmeasured escape hatch.

---

## 1. The substrate already exists — dormant, partial, and stringly-typed

The bbnf grammar is self-describing and already carries **two** host mechanisms, both defined
in `grammar/bbnf/bbnf.bbnf`:

- **Out-of-line typed declaration.** `bbnf.bbnf:73`
  `host_directive = "@host" ?w , identifier ?w , ( ":" ?w , type_name ?w ) ? , ( ";" | "." ) ? ;`
  — a named host fn with an *optional return type*. The type comes from `grammar/bbnf/types.bbnf`
  (`type_name`), which admits the primitive widths (`u8..u64`, `i32/i64`, `f32/f64`, `bool`,
  `usize`) **and an arbitrary identifier** (`/[_a-zA-Z][_a-zA-Z0-9]*/`) — i.e. a
  backend-agnostic `Named` type the backend resolves later (the backend-agnostic-types law).
  **Usage across every shipped grammar: zero.** The seam is designed and dormant.
- **Inline host capture.** `bbnf.bbnf:33`, inside the `term` production: `"@{" , rhs ?w , "}"`.
  This one *is* used: `grammar/css/l4/color.bbnf:220` `colorFunction = @{ (colorType) … }`
  ("LargeAggregate routing") and `grammar/css/l4/values.bbnf:69`
  `urlFunction = @{ "url" , "(" >> (string | /[^)"'\s]+/) << ")" }` ("preserves raw URL text
  for source maps"). Yet in the skinny runtime it is **UNSUPPORTED**: the frontend classifies
  it `RuntimeConstructKind::HostCapture` and maps it to the error code
  `BBNF-UNSUPPORTED-HOST-CAPTURE` (`skinny/crates/grammar/src/lib.rs:222`, struct
  `RuntimeHostCapture` at `:170`). So the CSS floor is presently *unmet by the compiler slice*.

And the pathology the lattice abolishes is already in the tree, in the open:
`skinny/crates/passes/src/lib.rs:794` `has_parse_time_host_decode` decides whether a leaf is a
parse-time host decode by a **string heuristic** —
`name.contains("host") && (value.contains("decode") || value.contains("parse"))`
(`passes/src/lib.rs:798`), feeding `ShapeRationale::HostFnParseTime`
(`passes/src/lib.rs:620`, cost lever `HostFnParseTime` at `ir/src/cost.rs:255`). A host boundary
detected by substring sniffing is the untyped, name-gate-adjacent smell 7C exists to replace
with a typed registry lookup. It is a one-file, shape-not-string cure (§5, prototype T2).

---

## 2. The lattice — three strata, one typed seam between the top two

Totality is measured *only* against the boundary between strata 2 and 3. The strata:

1. **GRAMMAR.** Rules, alternations, repetitions, `->` projections, the value-expression DSL
   (`grammar/bbnf/expressions.bbnf`: precedence-climbing `value_or … value_atom`,
   `value_fn_call`, `value_input`). The totality core; carries structure and pure leaf maps.
2. **PRIMITIVES / COMBINATORS.** Pure, referentially-transparent kernels invoked from a
   projection via `value_fn_call` — float decode (Eisel–Lemire, `parse-that-regex/src/number/
   eisel_lemire/mod.rs:147`), string unescape (`parse-that-regex/src/lib.rs:867`), SIMD scan
   (`bbnf-simd`). **This is the constitution's exempt floor.** JSON proves it: `host.rs` is
   *empty* — "JSON is host-fn-free in the skinny compiler slice"
   (`skinny/crates/runtime/src/grammars/json/host.rs:2`). JSON's number and string decodes are
   **primitives, not host fns.** The lattice's first law: *pure decode is a primitive, never a
   host fn.* This alone answers GN-5's TOML-datetime risk (§4, R1).
3. **HOST FUNCTIONS.** The *only* legal home for context-dependent or recursive-with-state
   semantics — behind a declared, typed signature. Two tiers, **declared not inferred**:
   - **PURE-DECODE tier** — `@host name : Span -> T`, no parser state. (Should almost always be
     demoted to stratum 2; the tier exists so a decode that is not yet a kernel has a lawful,
     typed temporary home with a migration obligation.)
   - **CONTEXT tier** — `@host name : (Span, &mut Ctx) -> T`, reads/mutates a *declared, typed*
     parse-time context. The attribute-grammar framing is exact: the context is an **inherited
     attribute** (table-path stack, indent stack, at-rule scope), the return an **synthesized
     attribute**. This is the residue totality counts against.

The typed seam (stratum 2↔3) is one artifact: extend `@host name` to a full signature
`@host name : InType -> OutType` (or `(InType, &mut CtxType) -> OutType`), where the types
flow from `type_name` (already parsed) into LOCK-1's inference as a typed leaf whose result
unifies into the projection algebra, and whose *tier* is read off the presence of a `&mut Ctx`
parameter. The type plane is erased to zero runtime cost (constitution v) for the PURE-DECODE
tier by construction; for the CONTEXT tier, the `Ctx` is a *parse-time-only* channel
(indent/table-path stacks never outlive the parse), so the zero-cost invariant is re-verified,
not assumed (§4, R5).

**The context class drives the cost model — structurally, not by substring.** The reason the
`has_parse_time_host_decode` string-sniff exists at all is that a host boundary has a
*materialization consequence*: a host fn that reads the cursor cannot be deferred; one over
values can be const-folded. 7C makes that consequence a **declared enum on the signature**, and
the cost model reads it:

```rust
// crates/ir/src/host/  (new directory-module; colocation edict, no god module)
pub enum HostContext {            // the lattice's cost axis, low→high materialization
    Pure,        // args are values only → foldable; admits SinkOnly / comptime const-fold
    ParseState,  // reads cursor/offset      → parse-time; forces EagerTape
    TreeFold,    // recurses over a subtree   → EventTape granularity
    CompileIO,   // @import / scope resolution → comptime-ONLY, never on the runtime path
}
pub struct HostSig { pub params: Vec<TypeDesc>, pub ret: TypeDesc,
                     pub context: HostContext, pub purity: Purity }
pub struct HostRegistry { pub sigs: BTreeMap<HostFnId, HostSig>,
                          pub primitive_floor: PrimitiveFloor }   // replaces the "skinny-none" String
```

The cure at `passes/src/lib.rs:794` becomes a registry lookup that generalizes the single
`HostFnParseTime` rationale to the full lattice:

```rust
fn host_shape_constraint(g, id, reg) -> Option<ShapeRationale> {
    match reg.context_of(host_call(g, id)?) {         // structural: HostCall node, not a name
        HostContext::ParseState => Some(ShapeRationale::HostFnParseTime),
        HostContext::TreeFold   => Some(ShapeRationale::HostFnTreeFold),     // new variant
        HostContext::CompileIO  => Some(ShapeRationale::HostFnComptimeOnly), // new variant
        HostContext::Pure       => None, // no constraint — may SinkOnly / const-fold (PROVE-12 seam)
    }
}
```

`Pure` is what *lowers* cost: a pure host fn on a literal argument is a compile-time value, so
the PROVE-12 residualization seam (const-fold to an immediate) generalizes to host calls — a win
the substring sniff could never earn, since it forced eager on *every* `host…decode` name.

**The primitive floor (stratum 2) is a closed, declared, gated list — not an open exemption.**
`PrimitiveFloor` enumerates the constitution's exempt kernels: `bbnf-simd::prim::*`
(`bbnf-simd/src/lib.rs:250-336`, each with a scalar reference + `primitive-checkasm` gate,
`xtask/src/main.rs:52`), Eisel–Lemire (`parse-that-regex/src/number/eisel_lemire/mod.rs:147`),
the tape writer/arena (`runtime/src/tape/mod.rs:92,:36`). Each floor member carries a
`HostContext::Pure` signature and a parity oracle; *adding* a member is a named, gated act. This
closes R7 (floor creep, §4): "primitive" cannot become an escape hatch because the floor is
finite, declared, and checkasm-defended.

---

## 3. Totality as a measured percentage with a named floor per subsystem

For each shipped grammar, a mechanical counter walks the `GrammarIr` and partitions leaves and
alternation branches into {grammar, primitive, host}. **Totality% = (grammar + primitive
behavioral surface) / total**, and equivalently a **host-residue floor** — the enumerated,
justified set of CONTEXT host fns per subsystem. Measured against behavior (leaves × branches
covered), *not* rule count, so a host fn that swallows a subtree counts its whole covered
surface as non-total (defeats the gaming counterexample, §4 R3). The named floors, from the
grammar sources:

| Subsystem | CONTEXT host floor | Evidence |
|---|---|---|
| **json** | **0** | `json/host.rs` empty; number/string are primitives |
| **csv** | **0** | flat, line-oriented; WALL-A newline-significance is grammar-derived |
| **toml** | **table-path nesting** (`[a.b.c]`, `[[array]]`) → `@host push_path : (Span, &mut TablePathStack) -> Unit` | GN-5.W2; nesting is genuinely context-dependent, datetime is *not* (primitive class-run scan) |
| **css l4** | **`@{}` LargeAggregate/raw-span capture** (`colorFunction`, `urlFunction`); at-rule cascade scope candidate | `color.bbnf:220`, `values.bbnf:69`; blocked on productionizing HostCapture |
| **sheets** | operator tower is grammar (39-branch); cross-cell reference resolution the only host candidate | R1 §9 sheets carve-out |
| **bbnf (self-host)** | `@import` module resolution + closure β-reduction (compile-time) | `bbnf.bbnf:4-5,60`, closure at `:52` |

The floors are enforced by a **totality-floor gate** — a sibling of GN-5.W3's `check-name-gates`
(`cargo xtask check-name-gates`, `docs/tranches/sk-v24/GN-5.md` W3) — that fails if a subsystem's
host set exceeds its declared floor *or* if any host fn's name/body is grammar-name-specific
(the overfitting cure, R1 below). This directly discharges GN-5's own open obligation: risk R2
(§7) states "`check-name-gates` must be extended to audit host functions, or the datetime leaf
stays grammar-expressed" — 7C makes that extension the lattice's enforcement organ.

---

## 4. Named risks and counterexamples

- **R1 — the host fn as name-gate in disguise (GN-5 §7 R2).** `@host toml_datetime` that
  hard-codes TOML is overfitting wearing a signature. *Guard:* the totality-floor gate audits
  host-fn identifiers and captured bodies for grammar-name literals (`toml`,`css`,…), the same
  AST walk GN-5.W3 runs over codegen; a floor entry is legal only when justified by
  context-dependence, and datetime — being pure — is forbidden from the host tier (it is a
  primitive class-run scan).
- **R2 — untyped drift.** `passes/src/lib.rs:798` proves detection degrades to
  `name.contains("host")`. *Guard:* the typed registry is the single source of truth; the string
  sniff is deleted, replaced by a tier lookup (prototype T2). If the registry is bypassed the
  build fails closed (`grammar_provider.rs:267` already fails when host capture is required but
  absent).
- **R3 — totality gaming.** Push all structure into one giant host fn to *look* grammar-total.
  *Guard:* measure behavioral surface (leaves × branches), not rules; a swallowing host fn
  scores its whole subtree as non-total.
- **R4 — the CSS floor is presently unmet.** `@{}` HostCapture is `BBNF-UNSUPPORTED`
  (`grammar/src/lib.rs:222`) yet `colorFunction`/`urlFunction` depend on it. *Guard:* the
  family's first production obligation is to lower `RuntimeConstructKind::HostCapture` in the
  skinny runtime, else CSS totality is a paper number.
- **R5 — the CONTEXT tier breaks the zero-cost type plane.** A `&mut Ctx` that leaks into the
  emitted tape violates constitution v. *Guard:* CONTEXT is a *parse-time-only* typed channel
  (stacks discarded at parse end); the zero-cost invariant (`runtime/src/lib.rs:42-44` payload
  no-write assertion, JSON) is re-run per subsystem with a CONTEXT host fn present.
- **R6 — isomorphism drift (LOCK-6).** A host fn body is Rust today, TS later. *Guard:* the
  signature (types + tier) is backend-agnostic (`type_name` → `Named`); only the body is
  per-backend, modeled on modular-FFI's "abstract the binding from its interpretation."
- **R7 — primitive-floor creep.** If "primitive" is an open exemption, everything migrates to
  stratum 2 and totality collapses to the floor. *Guard:* `PrimitiveFloor` is a **closed declared
  list** with a per-member checkasm parity oracle; growth is visible and gated, not silent (§2).
- **R8 — a lying `Pure` signature.** A host fn declared `Pure` that actually reads the cursor
  mis-drives the cost model (SinkOnly where EagerTape was needed). *Guard:* purity is *verified,
  not asserted* — LOCK-1's type plane checks that a `Pure` sig's params are value/`Span` types
  only; a `Pure` fn taking `&mut Ctx`/cursor is a type error. 7C is load-bearing on LOCK-1 here.

---

## 5. What a prototype must test to move this family (falsifiable, ordered)

- **T1 — typed signature.** Extend `@host name` grammar to `@host name : InType -> OutType`
  and `(InType, &mut CtxType) -> OutType`; wire the return `TypeDesc` into LOCK-1 inference as
  a typed leaf. **Pass:** a PURE-DECODE `@host` type-checks and the zero-cost assertion
  (`runtime/src/lib.rs:42-44`) still holds.
- **T2 — kill the string heuristic.** Replace `has_parse_time_host_decode`
  (`passes/src/lib.rs:794`) with a typed-tier registry lookup. **Pass:** `cargo xtask
  check-json` bit-for-bit (JSON floor = 0, must be unaffected).
- **T3 — one real CONTEXT host fn, end to end.** TOML `@host push_path : (Span, &mut
  TablePathStack) -> Unit`; round-trips against the `toml` crate (GN-5.W2 oracle) **and** a
  grammar-name-hardcoded variant is flagged by the floor gate.
- **T4 — productionize `@{}`.** Lower `RuntimeConstructKind::HostCapture` in skinny; CSS
  `colorFunction`/`urlFunction` stop emitting `BBNF-UNSUPPORTED`. **Pass:** CSS regen
  bit-for-bit + measured CSS totality% delta.
- **T5 — the totality counter + floor gate.** Mechanical per-grammar {grammar|primitive|host}
  partition; publish totality% + named floor for all five grammars; a CI gate fails on floor
  regression or grammar-name-specific host. **Pass:** the gate trips on a planted floor breach
  (falsification-proven, GN-5.W3 discipline).
- **T6 — bootstrap fixpoint.** The typed host-directive survives `regen∘regen` byte-identical
  on `bbnf.bbnf` itself (the grammar that *defines* `host_directive` re-parses through its own
  generated parser) — the LOCK-7 staging obligation, `skinny/crates/xtask/src/main.rs:39-52`
  dispatch, `regen.rs:19` mechanics.

---

## 6. Prior art (cited)

- **tree-sitter external scanners** — hand-written C for tokens "that cannot be expressed as
  regular languages" (indentation, heredocs, raw/template strings): the exact
  context-dependent residue, but **untyped and no semantic predicates**. 7C's advance is the
  *typed signature + measured floor*. (tree-sitter.github.io, "External Scanners".)
- **ANTLR semantic predicates & actions** — embed host code to resolve ambiguity, at the cost
  that "the whole grammar is not language-independent anymore" — the backend-agnostic violation
  7C avoids by keeping types `Named` and only the body per-backend. (parser-comparison survey,
  ResearchGate 376883410.)
- **Menhir semantic actions** — OCaml fragments decorating productions; typed, but welded to
  one host language. (Menhir Reference Manual, Inria/Gallium.)
- **Attribute grammars** — synthesized (from children) vs inherited (from parent/siblings)
  attributes: the formal model for the CONTEXT tier — the typed `&mut Ctx` **is** an inherited
  attribute, the return a synthesized one. (Wikipedia, "Attribute grammar"; "Synthesis of
  Semantic Actions in Attribute Grammars", arXiv 2208.06916.)
- **Contextual MetaML / MetaOCaml typed staging** — brackets/escape, type-safe splices: the
  model for a host fn as typed stage-0 code spliced into the stage-1 parser, tying to LOCK-3's
  staged-residual and LOCK-7's bootstrap. (arXiv 2602.03033; 2601.15180.)
- **Modular FFI (types-as-values)** — abstracting a binding's *definition* from its
  *interpretation* (libffi vs codegen): the model for one backend-agnostic host signature with
  Rust-now/TS-later bodies. (ScienceDirect S0167642317300709.)
- **GHC FFI purity axis** — `foreign import ccall f :: T1 -> IO T2` vs a pure return, with
  `unsafePerformIO` as the audited escape: the canonical *typed* host boundary with an explicit
  IO/pure split. 7C's `HostContext`/`Purity` is exactly this axis; the erased-type-plane law
  mirrors GHC's zero-cost erased types. (GHC User's Guide §8, FFI.)
- **MLIR external funcs + op-interfaces** — `func.func` declarations and `CallOpInterface` are
  declared typed contracts across dialects; `unrealized_conversion_cast` is the typed escape.
  The dialect-lattice-with-typed-interfaces is the structural analog of the host lattice.
- **Lean/Coq `opaque` / `axiom`** — a typed hole in an otherwise total system; the trusted
  kernel is the closed primitive floor (stratum 2), with checkasm as the de-Bruijn-style proof
  obligation. The floor is 7C's trusted computing base. (Lean 4 manual, "opaque".)

---

## 7. The family's claim, sharply

The generator does not become "mostly data" (that is a sibling family's wager); it becomes
**grammar plus a typed, counted, floored escape hatch**. Self-hosting terminates because the
un-grammar-expressible residue is finite, named, and typed — the primitives (stratum 2) are the
constitution's exemption made concrete, the host fns (stratum 3) are the context-dependent
remainder made *measurable*. The deliverable is not "100% self-hosted" but a **published
totality percentage per subsystem with an enforced floor** — and the immediate, in-tree win is
deleting one string heuristic (`passes:798`) and lighting up one dormant, already-designed,
already-typed directive (`bbnf.bbnf:73`).
