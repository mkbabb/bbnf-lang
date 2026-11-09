# LOCK-7 pass-2 — Family 7C: THE HOST-FUNCTION LATTICE

*Totality bounded by a TYPED host-fn interface. Context-dependent and recursive semantics
live behind declared signatures; totality is a measured percentage of surface behind
grammars, with a named floor per subsystem.*

---

## §0 The three-tier lattice (the family's shape)

Host is not a monolith; it is a lattice of three tiers, each with a distinct cost and a
distinct dependency on the solver spine:

- **PURE tier** — leaf semantics expressed entirely on the grammar plane: `-> value_expr`
  projections (`Span`, T-constructors, arithmetic over captured spans). Zero host, zero
  runtime cost, grammar-neutral. This is the majority surface and the numerator of totality%.
- **STRUCTURAL tier** — the *build* mechanisms, host-bound but **grammar-neutral and
  comptime-only**: node append (`add_expr`, `ir/src/lib.rs:51`) and cross-rule link
  (`resolve_refs`, `ir/src/lib.rs:84`). These carry no grammar name and cost zero at runtime
  (they run at regen). This is the "host floor" the construction census bottoms out at.
- **CONTEXT tier** — the typed escape hatch: `@host fn : T`, for context-dependent and
  recursive semantics a first-set/shape dispatch cannot express (TOML table nesting, a
  recursive operator tower's precedence climb). Threaded via a declared `&mut Ctx` signature.
  Its existence is gated on a LOCK-1 inherited-attribute verdict and on a proven zero-cost
  invariant for the grammars that do NOT use it.

7C's deliverable is: (a) make the tier boundary **typed and enforced** rather than
substring-sniffed, (b) **count** the percentage of behavior behind grammars with a
falsifiable gate, (c) prove the CONTEXT tier is zero-cost for non-users before it is scheduled.

---

## §1 The concrete substrate — what exists at HEAD

The typed host surface is **already latent in the self-hosted grammar** but rejected by the
skinny hand-parser:

- **`@host name : type` — the typed declaration.** `grammar/bbnf/bbnf.bbnf:76`:
  `host_directive = "@host" ?w , identifier ?w , ( ":" ?w , type_name ?w ) ? , ( ";" | "." ) ?`.
  This is precisely the declared-signature surface the family needs. The skinny parser
  **rejects it**: `crates/grammar/src/lib.rs:1327` emits `BBNF-DIRECTIVE-NOT-IN-SKINNY` for
  every directive save `import|token|pretty|ws` (`:1324`). `@host`/`@recover`/`@debug` are
  born-RED parser gaps.
- **`@{ ... }` — the anonymous host-capture atom.** `bbnf.bbnf:33`: the `term` alternation
  arm `"@{" , rhs ?w , "}"`. The skinny parser scans it as a **runtime FACT** only —
  `parse_runtime_host_capture` (`crates/grammar/src/lib.rs:799`) records a `RuntimeHostCapture`
  into the frontend closure — then maps it **UNSUPPORTED**: `crates/grammar/src/lib.rs:222`,
  `RuntimeConstructKind::HostCapture => "BBNF-UNSUPPORTED-HOST-CAPTURE"`. It **never enters
  `GrammarIr`**: `parse_atom` (`grammar/src/lib.rs:~1503`) has no `@{` arm. This is the
  lower-`@{}` seam the directives name.
- **`|params|` closures — grammar functions.** `bbnf.bbnf:52`:
  `closure = "|" , identifier , ( "," ?w , identifier ) * , "|" ?w , rhs`. No skinny parser
  support; the third bounded gap.
- **The build floor.** `resolve_refs` (`ir/src/lib.rs:84`) is the one cross-rule link: a
  `HashMap<name → RuleId>` pass mutating each `ExprKind::Ref { target }` from `None` to
  `Some(RuleId)` in place (`:96`), TreeFold-shaped, comptime-only. `add_expr` (`:51`) is pure
  append. Together they are the entire STRUCTURAL tier — node values are pure `->`, build
  mechanisms are these two.
- **The heuristic to kill.** `crates/passes/src/lib.rs` `has_parse_time_host_decode` (in the
  `requires_eager_tape` cluster, ~line 794) detects "host decode" by **substring match**:
  `name.contains("host") && (value.contains("decode") || value.contains("parse"))` over an
  `ExprKind::Annotation`. A stringly-typed proxy for the host tier — a name-gate in spirit,
  gating eager-tape selection on a substring rather than a typed node field.

`ExprKind` (`ir/src/lib.rs:234`) has **no host node** today: `Seq | Alt | Repeat | Optional |
Literal | Regex | Ref | Annotation | Project | Discard`. The closest carrier is
`Annotation { name, value }` — exactly why the eager-tape probe is forced into substring
sniffing. Lowering `@{}` introduces the missing typed node.

---

## §2 The split-independence answer (directive Q1)

Two clean strata; the census counter sits astride them.

### LOCK-FREE (no solver-spine dependency; movable now)

1. **Kill `passes/src/lib.rs` `has_parse_time_host_decode`.** Once `@{}` lowers to a typed
   `ExprKind::Host { name, args, body, eager }` node, the "does this need an eager tape"
   question reads a **declared boolean field**, not a name/value substring. Eagerness is a
   *structural* fact — does the host fn read cross-token context? — declared in the `@host`
   signature, independent of any inferred type. `requires_eager_tape` becomes a typed predicate
   `matches!(kind, ExprKind::Host { eager: true, .. })`, DRY with the three sibling structural
   probes in that cluster (`has_recovery_annotation`, `has_dispatch_overlap`, `has_layout_policy`).
2. **Lower `@{}` at `grammar/src/lib.rs:222`.** Promote `HostCapture` from an UNSUPPORTED
   runtime-fact to a first-class `parse_atom` arm emitting `ExprKind::Host`. Bounded parser
   augment; grammar-neutral; touches the parser + the `ExprKind` enum + the `pretty`/lower arms;
   no inference. One of the three named parser gaps (`@{}` atom, `|params|` closures, `?w`→`->`
   attachment in `parse_seq`).
3. **Name `resolve_refs` (`ir/src/lib.rs:84`) as the STRUCTURAL-tier host floor.** It is the
   sole cross-rule link, grammar-neutral, the point the construction census bottoms out at. No
   LOCK-1 verdict needed — it links names, it does not type them.

### LOCK-1-BOUND (needs the solver spine's verdict)

4. **The `@host fn : T` signature flowing into inference.** The declared type must enter
   LOCK-1's HM core as the type of the calling projection — a **checked-against** obligation
   (the host body is opaque; its signature is a principal type the caller is checked against,
   GHC-`foreign import` discipline). Until LOCK-1 fixes whether host signatures are principal
   or bidirectionally checked, this cannot land.
5. **The CONTEXT tier off `&mut Ctx`.** Whether a host fn takes `&mut Ctx` (context threaded
   *down*, e.g. TOML `push_path`) versus a pure `&[u8] -> T` is the **inherited-attribute
   verdict (XL-3)**. An inherited attribute IS a `&mut Ctx` down-thread; a synthesized attribute
   IS the `-> T` up-value. If LOCK-1 admits inherited attributes, the CONTEXT tier gets a typed
   `&mut Ctx` signature; if not, context-dependence stays hand-coded and totality% carries a
   **named CONTEXT floor** per subsystem.

### Prerequisite note

The census/totality% deliverable (§3) **cannot be computed until bbnf parses+builds** — you
cannot count leaves×branches behind grammars until `bbnf.bbnf` routes through the emitter and
materializes a `GrammarIr`. Lock-free item (2) is thus a **hard prerequisite** for the counter;
the counter is not itself LOCK-1-bound, but it is `@{}`-lowering-bound.

---

## §3 The totality% counter and its floor-gate (directive Q2)

### The counter (leaves × branches)

Walk the built `GrammarIr` over the whole grammar corpus:

- **LEAVES = every `-> value_expr` projection target** (`ExprKind::Project`, `ir/src/lib.rs:261`).
  Grammar-expressed if `target`/`ty` resolve to a pure `value_expr` (Span, T-constructor, span
  arithmetic) with **no `@host` call**; host-bound if the target names an `@host` fn.
- **BRANCHES = every `Alt` arm** (`ExprKind::Alt`, `ir/src/lib.rs:236`). Grammar-expressed if
  selection is first-set / shape driven (the extant grammar-neutral dispatch,
  `first_bytes`/`branches_overlap` in `passes/src/lib.rs`); host-bound if it needs a host
  predicate (context-dependent lookahead).
- **totality% = grammar-expressed / (grammar-expressed + host-bound)**, summed over leaves and
  branches, reported **per-subsystem** (per grammar, per crate).

### The per-subsystem floor

Each subsystem **declares its named host floor** — the irreducible host surface below which
totality cannot lawfully fall: *regex NFA construction = 0% grammar (kernel primitive)*; *TOML
table nesting = CONTEXT tier, exactly 1 host fn (`push_path`)*; *sheets operator tower = N
host-bound branches (precedence climb)*. Every host fn **above** the floor is a **defect** —
grammar-expressible surface regressed to hand code (standing law: augment BBNF, never work
around).

### The gate — a `check-name-gates` sibling

GN-5 W3 installs `cargo xtask check-name-gates`: a `syn` AST walk over `crates/codegen/src/`
that fails on grammar-name string literals, paired with `name_gate_falsification.rs` planting
three breaches and asserting the gate trips. **`check-totality` is its structural sibling** —
colocated in `xtask/src/main.rs` alongside the `check-*` arms (`xtask/src/main.rs:40-52`):

```
cargo xtask check-totality      # census GrammarIr Project/Alt; fail below any declared floor
```

Fails if (i) any subsystem's totality% drops below its declared floor, OR (ii) any `@{}`
capture lacks a matching `@host` typed declaration (an **untyped** host is a breach — the point
is a TYPED interface).

### Planted-breach falsification

Mirroring GN-5 W3, an `xtask/tests/totality_falsification.rs` plants three forms into an
in-memory grammar and asserts a violation for each:

- (i) an `@{}` capture with **no matching `@host` declaration** (untyped host — must trip);
- (ii) a `Project` target calling an **undeclared** host fn (must trip);
- (iii) a host fn that **hard-codes a grammar-specific constant** (a name-gate in host disguise
  — the exact GN-5 R2 risk that a host shim smuggles a `datetime`/grammar constant in);

then asserts the **real corpus passes clean**. A gate that cannot fail is a defect (the
constitution's concrete-artifacts clause; the SPEC failure-mode law). Proves it can fail before
it is trusted to pass.

---

## §4 The CONTEXT-tier zero-cost invariant (directive Q3)

The constitution's clause v — *the type plane costs zero at runtime* — must hold for the
CONTEXT tier's `&mut Ctx` thread **before that tier is scheduled**. Two testable predicates:

- **`resolve_refs` round-trip.** `resolve_refs` mutates `Ref.target` `None→Some(RuleId)` at
  build time (`ir/src/lib.rs:96`) and is erased before emission. Testable invariant: after
  `resolve_refs`, a PURE-tier rule (no `@host` call) emits **byte-identical** recognizer output
  whether or not a `Ctx` type exists in the build. Round-trip: `parse → resolve_refs → emit`,
  diff against a `Ctx`-free build; PURE rules must be byte-identical.
- **TOML `push_path` as the CONTEXT witness.** TOML table nesting (`[a.b.c]`) is the canonical
  inherited attribute — the path flows *down*. If the CONTEXT tier lands, `push_path` is a host
  fn `fn(&mut Ctx, key)`. Zero-cost test: a grammar with **no** context-dependent rule (JSON,
  CSS) must emit **zero** `Ctx` parameter — the `&mut Ctx` appears in a signature **only** when
  a rule actually calls a CONTEXT-tier host fn. Test: regen JSON with the CONTEXT tier compiled
  in, assert **byte-identical** to pre-CONTEXT JSON (the `Ctx` thread absent because no JSON rule
  uses it).

The **born-RED schedule gate**: `check-totality` + a `ctx_zero_cost` regen-parity test showing
PURE-tier grammars are `Ctx`-free at the byte level. Until that test is green, the CONTEXT tier
is **BLOCKED** — the direct answer to Q3: the invariant is testable via regen-parity, and the
tier is scheduled only behind it.

---

## §5 Prior art with citations

- **Tree-sitter external scanners** — the canonical typed host-fn interface for
  context-sensitive tokens (Python indentation, heredocs, raw strings, template literals) no CFG
  can express. Five typed C functions (`create/destroy/serialize/deserialize/scan`), state
  persisted through the engine's serialization protocol. Exactly the CONTEXT tier: a bounded,
  typed escape hatch **below the CFG floor** with an explicit state contract. bbnf's
  `@host fn : &mut Ctx -> T` is the isomorphic surface; `Ctx` is the analogue of the scanner's
  persisted state — skinny needs no incremental reparse, so serialize/deserialize collapses to a
  plain `&mut Ctx` thread.
  (tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html)
- **Silver extensible attribute grammars + forwarding** — the totality model: extension
  constructs receive implicit semantics by *forwarding* to a host construct; inherited and
  synthesized attributes are the typed context threading. The `&mut Ctx` down-thread is an
  **inherited attribute**; `-> T` is a **synthesized attribute**. Grounds Q1's LOCK-1
  inherited-attribute verdict (XL-3): the CONTEXT tier is exactly "does this grammar admit
  inherited attributes." (Van Wyk et al., *Silver: an Extensible Attribute Grammar System*.)
- **Semantic actions (ANTLR / yacc / Marpa)** — host expressions embedded in productions,
  invoked at reduce. bbnf's `-> value_expr` **is** a typed semantic action; the difference is
  bbnf **types** it (LOCK-1) and keeps the majority PURE on the grammar plane, reserving host
  code for the declared CONTEXT floor — versus yacc's untyped C blocks scattered everywhere. The
  lattice (typed pure action, majority → typed host action, minority, declared) is the
  discipline yacc lacks.
- **GHC `foreign import` / MLIR op interfaces** — the declared-signature discipline: an external
  operation carries a typed signature the compiler **checks against**, never infers from the
  body. `@host fn : T` follows this; why §2 item (4) is checked-against, not principal-inferred.

---

## §6 Named risks and counterexamples

- **R-A — `@{}` lowering cascades (the shared open empirical question).** `bbnf.bbnf:33`'s
  capture is `"@{" , rhs ?w , "}"` — its body is `rhs`, the **whole grammar**, so the host
  node's body is unbounded and self-referential. If lowering `@{}` to `ExprKind::Host` breaks
  the recognizer round-trip (`regen∘regen` not byte-identical) it is not free. **Guard:** drive
  the `!has_rule("value")` general-descent branch on `bbnf.bbnf` (no `value` rule), **never**
  `json_sink_direct`; watch the `term` alternation (`bbnf.bbnf:29-36`) as the first cascade
  point. Do not seed the verdict from the emitter's mere existence.
- **R-B — untyped `@{}` versus typed `@host` incoherence.** `bbnf.bbnf` carries **both**
  `@{...}` (anonymous atom, `:33`) and `@host fn : type` (named declaration, `:76`). Two
  mechanisms make the lattice incoherent. **Resolution:** `@{}` must lower to a *reference* to
  an `@host`-declared fn (the capture body names the host fn), so there is ONE typed surface; an
  `@{}` with no matching `@host` is the §3 planted-breach (i).
- **R-C — the heuristic-kill regresses eager-tape selection.** `has_parse_time_host_decode`
  gates eager tape; a typed replacement that *infers* eagerness could mis-classify a cross-token
  host fn as lazy and read garbage. **Guard:** eagerness is **declared** in the `@host`
  signature, not inferred — trading a name substring for a wrong inference is the failure mode;
  ties eagerness to the §2 typed-node field, not to LOCK-1.
- **R-D — totality% is gameable.** A subsystem could inflate its % by declaring one giant host
  fn as "a single leaf." **Guard:** the counter weights by leaves×**branches of behavior** — a
  host fn's *internal* dispatch points count as host-bound branches, not one; the floor-gate
  audits host-fn LOC against the generated-size budget (extreme-parsimony law), so a fat host fn
  shows as many host branches and fails its floor.

---

## §7 What a prototype must test (to move this family)

1. **Parser augment.** Add a `@{` arm to `parse_atom` (`grammar/src/lib.rs:~1503`) emitting
   `ExprKind::Host { name, args, body, eager }`; add `|params|` closures to the `rhs` parse;
   attach `?w`→`->` in `parse_seq`. Route `bbnf.bbnf` through `!has_rule("value")`
   general-descent and assert `regen∘regen` **byte-identical** — the shared open empirical
   question, RED-unknown.
2. **Totality counter, non-vacuous.** Walk `GrammarIr` `Project`/`Alt`, classify pure-vs-host,
   emit per-subsystem %. On the extant corpus (json/css/csv/sheets) report the **actual**
   numbers — expect ~100% for json/css (pure `-> Span`/T leaves), a named CONTEXT floor for
   sheets (39-branch operator tower) and TOML (table paths). Triple-identical proves nothing (a
   fixed-const emit path stamps identical bytes for any input); the census must be over *real*
   Project/Alt content.
3. **Floor-gate falsification.** `check-totality` trips on the three planted breaches (untyped
   `@{}`, undeclared host call, grammar-const-in-host), passes clean on HEAD.
4. **Ctx zero-cost.** Compile the CONTEXT tier in, regen JSON, assert byte-identical to
   pre-CONTEXT JSON (PURE grammars `Ctx`-free). **BLOCK** the CONTEXT tier until green.

**Substrate confirmed absent:** skinny xtask has **no** `regen-bbnf`, **no** fixpoint arm, and
**no** `check-name-gates` yet (`xtask/src/main.rs:40-52` lists `regen-json|check-json|
regen-sheets|regen-css|…`); the `~88s→0.5s` fixpoint datum is a **legacy transplant**, not a
skinny measurement — a `grep fixpoint|regen-bbnf` over `skinny/xtask` returns empty. Any
totality gate is thus a **net-new** born-RED sibling, not a modification of an existing gate.
