# LOCK-7 pass-2 · Family 7C — THE HOST-FUNCTION-LATTICE

Totality bounded by a TYPED host-fn interface: context-dependent and recursive
semantics live behind declared signatures, and totality is a *measured percentage*
of decision surface behind grammars with a named per-subsystem floor. This family
develops the lattice as three ordered strata — a lock-free floor that ships without
LOCK-1, a typed-signature stratum bound to LOCK-1's inference verdict, and a
context stratum bound to LOCK-1's inherited-attribute verdict — plus the counter
and the falsifiable floor-gate that make "totality %" an enforced invariant.

---

## §1 The substrate that already exists (census, path:line)

The lattice is not greenfield; four of its five members are already present in
skinny@HEAD and merely un-wired:

- **Build mechanism 1 — `add_expr` (append).** `ir/src/lib.rs:51`
  `add_expr(kind, span) -> ExprId` pushes one node onto `exprs`. Pure, allocation-
  is-a-Vec-push, no context. This is the WHOLE node-construction floor: every
  node value in the self-grammar is a `->` projection whose realized value is a
  Span (see §2), so construction never leaves the append mechanism.
- **Build mechanism 2 — `resolve_refs` (cross-rule link).** `ir/src/lib.rs:84`
  `resolve_refs(&mut self)` is a one-pass TreeFold: build a `name → RuleId` map,
  then walk every `ExprKind::Ref { name, target }` and stamp `*target = Some(id)`.
  This is the archetype of the CONTEXT tier — a `&mut self` fold that threads a
  resolution context. It is a COMPTIME pass (runs at regen, invoked at
  `grammar/src/lib.rs:33` before `validate`), therefore already zero runtime cost
  by construction.
- **Host-capture front-end — `@{}`.** The runtime frontend ALREADY scans and
  balances `@{ … }`: `parse_runtime_host_capture` (`grammar/src/lib.rs:799`,
  `runtime_skip_balanced` on `{`/`}`) yields `RuntimeHostCaptureScan { body, … }`,
  surfaced as `RuntimeHostCapture` (`grammar/src/lib.rs:170`, a body-string +
  span). The construct is classified `RuntimeConstructKind::HostCapture`
  (`:251`, scanned at `:431`) and the codegen provider even gates on its presence
  (`grammar_provider.rs:267 requirements.host_capture`). **But it is a dead end**:
  `RuntimeConstruct::unsupported` maps it to `"BBNF-UNSUPPORTED-HOST-CAPTURE"`
  (`grammar/src/lib.rs:222`), the hand builder's `parse_atom`
  (`grammar/src/lib.rs:1495`) has NO `@` arm (it errors `unexpected token` at
  `:1529`), and there is NO `ExprKind::HostCapture` in the IR (variants stop at
  `Discard`, `ir/src/lib.rs:266`). The scan exists; the lowering does not.
- **Typed host-fn declaration — `@host name : Type`.** The self-grammar already
  carries the declared-signature syntax: `host_directive = "@host" ?w ,
  identifier ?w , ( ":" ?w , type_name ?w ) ?` (`grammar/bbnf/bbnf.bbnf:73`),
  with `type_name` covering the primitive widths plus any identifier
  (`grammar/bbnf/types.bbnf:3-7`). This IS the typed host-fn interface the
  charter names — a name and a return type — parsed but not yet fed to inference.
- **The neutral emitter — `general_descent/`.** `!has_rule("value")` at
  `runtime_generator.rs:98` routes any value-rule-less program (sheets, and
  bbnf.bbnf) to `general_descent::render` (`mod.rs:38`), which emits ONE
  `parse_<rule>` **recognizer** per rule + a lazy tape view. Regex leaves are
  delimiter-scan Span projections; typed leaves are the grammar's declared TODO
  (`mod.rs:16-24`). It is grammar-neutral by construction.

**Consequence.** The self-grammar's host surface is exactly two constructs — the
`@{}` capture (one site, `term = … | "@{" , rhs ?w , "}"`, `bbnf.bbnf:33`) and the
`|params|` closure (grammar functions, `bbnf.bbnf:51 closure`). Everything else is
`-> Span` (identifier, literal, regex, big_comment, comment — five leaves,
`bbnf.bbnf:9-18`) or first-byte dispatch (term 8 arms, directive 7 arms, rhs 2).

---

## §2 Split independence (the three strata)

The charter's split resolves cleanly against the census. **The census/totality-%
deliverable is gated on bbnf.bbnf parsing+building** — the three parser gaps
(§2.1 items 2, and the `?w`→`->` and `|params|` augments) MUST land first, else
there is no GrammarIr to count leaves and branches over.

### §2.1 LOCK-FREE floor (ships without LOCK-1)

1. **Kill the parse-time-host-decode heuristic.** `passes/src/lib.rs:794`
   `has_parse_time_host_decode` tests `ExprKind::Annotation` for
   `name.contains("host") && (value.contains("decode") || value.contains("parse"))`
   — a substring match that forces `ShapeRationale::HostFnParseTime` → EagerTape
   (`:620`, via `requires_eager_tape` `:762`). It is fragile AND dead: no live
   grammar carries an inline `@host`/`host:` *annotation* (verified — JSON is
   host-fn-free `grammars/json/host.rs:2`, sheets/css carry none), and the real
   host construct is `@{}` (a `HostCapture`, never an `Annotation`), which this
   predicate cannot even see. Killing the string-match is lock-free; the
   *correct* replacement (an effect-typed host-fn timing fact) is LOCK-1-bound
   (§2.2) — so the lock-free move is to DELETE the heuristic and let effectful
   captures be caught structurally by the `@{}` lowering (§2.1.2), not by grepping
   annotation text.
2. **Lower `@{}`.** Three coordinated edits, all structural:
   (a) add `ExprKind::HostCapture { body: StringId }` to `ir/src/lib.rs` after
   `Discard`; (b) add a `Some('@')` arm to `parse_atom`
   (`grammar/src/lib.rs:1495`) that consumes `@{`, balances to `}` (reuse
   `parse_runtime_host_capture`'s balance logic already at `:799`), and emits the
   node; (c) teach `general_descent::emit` to lower a `HostCapture` leaf to a call
   into the grammar's `host.rs` module (`grammars/<g>/host.rs`, today the empty
   JSON stub) returning a Span. A *pure-Span* capture stays on the offset-tape
   (zero-cost); a capture that MUTATES parse state is the context tier (§2.3),
   NOT lock-free. Retire the `"BBNF-UNSUPPORTED-HOST-CAPTURE"` code at `:222`.
3. **Name `resolve_refs` as the context-tier archetype.** `ir/src/lib.rs:84` is
   the only cross-rule build mechanism; naming it fixes the tier's shape (a
   `&mut self` TreeFold threading a resolution map) and its cost class (COMPTIME,
   zero runtime residue). Naming is a documentation+classification act — no code,
   no lock dependency.

Plus the two remaining parser augments needed for bbnf to parse (shared facts):
`?w`→`->` attachment (`parse_seq:1361`/`parse_postfix:1449` currently break the
postfix chain so `-> Span` cannot attach to a trailing `?w`, as in
`big_comment = (…) ?w -> Span`, `bbnf.bbnf:17`) and `|params|` closures
(`closure`, `bbnf.bbnf:51`; needs a `|`-disambiguated arm distinct from the Alt
separator, and an `ExprKind::Closure`/`GrammarCall` pair — neither exists in the
IR today). These are structural parser work, lock-free.

### §2.2 LOCK-1-bound signature stratum

Feeding `@host name : Type` (`bbnf.bbnf:73`) into inference so a projection
`factor -> host_fn(input) : T` type-checks — the declared return type must unify
with the projection's expected type. This is LOCK-1's judgment plane (`->`
projections, `1A`/`1C`). Until LOCK-1 fixes how a declared host signature enters
the constraint store, the *correct* replacement for the killed heuristic (an
effect/timing-typed host fn that says "I decode at parse time → EagerTape") has no
home. Mark: **the heuristic-kill is lock-free; the heuristic-replacement is
LOCK-1-bound.** These are two moves, not one.

### §2.3 LOCK-1-bound context stratum (tier off `&mut Ctx`)

The context tier — host fns that thread a mutable context — depends on LOCK-1's
inherited-attribute verdict (the host floor's shape). Two exemplars pin it:
`resolve_refs` (comptime, `&mut self` over exprs) and TOML's `push_path` (a
parse-time `[a.b.c]`/`[[array]]` table-path stack, GN-5.W2's toml.bbnf). The tier
must NOT be scheduled until (a) LOCK-1 rules whether inherited attributes are
first-class, and (b) the zero-cost invariant is proven testable (§4). XL-3 (the
`&mut Ctx` tier-off adjudication) is the gate.

---

## §3 The totality-% counter and its floor-gate

### §3.1 The counter (leaves × branches)

Define the decision surface of a built GrammarIr as **LEAVES + BRANCHES**:

- a **leaf** is a terminal projection site — every `ExprKind::Project { body, … }`
  whose `body` is a `Literal` or `Regex` (`ir/src/lib.rs:261`);
- a **branch** is a dispatch arm — every arm of an `ExprKind::Alt`
  (`ir/src/lib.rs:236`).

A leaf/branch is **grammar-expressed** iff it is realized by a `->` projection or
first-byte dispatch in the emitted `parse_<rule>` recognizer; it is **host-routed**
iff it flows through an `ExprKind::HostCapture` (`@{}`) or a `@host` fn call.
Then, per subsystem g:

```
totality%(g) = grammar_expressed(g) / (grammar_expressed(g) + host_routed(g))
```

counted over g's GrammarIr — which is why the counter is gated on bbnf.bbnf (and
the fleet) building. On bbnf.bbnf the raw shape is ~5 Span leaves + ~19 Alt
branches grammar-expressed, 1 host-routed (`@{}` at `term`), 1 closure —
totality ≈ 24/25 modulo the closure's classification.

**Weighting (anti-gaming, §5-R2).** A `-> Span` leaf that merely defers real
decoding to a downstream host consumer is NOT full totality. Weight a typed
projection `-> T` (T ≠ Span) as 1.0 and a `-> Span` leaf whose value a host fn
later re-decodes as a *partial* (host-routed) leaf. Otherwise totality% is a
vanity number inflatable by coarsening every leaf to Span.

### §3.2 The floor-gate — `check-host-floor` (a GN-5 `check-name-gates` sibling)

Colocate a new xtask arm beside `check-name-gates` (GN-5.W3,
`xtask/src/main.rs`): `check-host-floor` walks each grammar's built GrammarIr,
computes `totality%(g)` via §3.1, and fails if any g falls below its declared
per-subsystem floor (a small committed table: e.g. `bbnf ≥ 0.90`, `json = 1.0`
host-fn-free, `css ≥ …`). The floor is the charter's "named floor per subsystem";
it is data, not a name-gate (it keys on the counter, never on a grammar name).

**Planted-breach falsification (mandatory, precept 19 / Constitution vii).**
Mirror `name_gate_falsification.rs`: `xtask/tests/host_floor_falsification.rs`
plants a grammar where a leaf that COULD be `->`-expressed is instead routed to
`@{}` (dropping its totality below floor) and asserts `check-host-floor` trips;
then asserts the real fleet passes clean. A gate that cannot fail is a defect;
this test is the proof it can. Complementary breach forms: a `-> Span`
coarsening that a host consumer re-decodes (§3.1 weighting must catch it), and a
subsystem whose floor is set unreachably high (the table itself is audited).

---

## §4 CONTEXT-tier zero-cost invariant — testable BEFORE scheduling

Constitution clause v (type plane costs zero at runtime) binds the context tier.
Two independent tests establish it *before* the tier is scheduled (§2.3):

1. **`resolve_refs` comptime round-trip.** resolve_refs runs at regen, never at
   target-document parse time, so it has zero *runtime* residue by construction.
   Prove it non-vacuous: parse a grammar, run `resolve_refs`, and assert the
   resolved GrammarIr equals a hand-pre-linked one (same `target` stamps) —
   equality proves the pass adds no structure, only fills `Ref.target`. Its only
   allocation is the `name → RuleId` HashMap (`ir/src/lib.rs:85`), one pass,
   O(rules), all comptime. This is the COMPTIME context sub-tier.
2. **TOML `push_path` runtime alloc-zero counter.** The PARSE-TIME context
   sub-tier: TOML's `[a.b.c]`/`[[array]]` header threads a table-path context as
   the document parses. Reuse the exact lazy-projection invariant JSON already
   enforces — `runtime/src/lib.rs:42-44` asserts `payloads().len() == 0` and zero
   `PayloadArena` writes/allocs after parse (the bench-gated counters at
   `tape/mod.rs:63`). Assert the same after a TOML parse that exercises nested
   table paths: if `push_path` threads context on the offset-tape with no per-
   header heap allocation, the counter reads zero — the invariant holds. If it
   does not, the context tier is NOT zero-cost and must not be scheduled as such.

The two sub-tiers are DISTINCT (§5-R3): comptime name-resolution (zero by
construction) vs parse-time context-threading (zero only if the alloc counter
proves it). Conflating them is the trap.

---

## §5 Named risks and counterexamples

- **R1 — the `@{}` capture is effectful, silently flipping the shape.** A
  `@{ push_table_path(…) }` that mutates parse state needs eager materialization;
  routing it as a pure-Span leaf would be wrong. Counterexample the lowering must
  guard: the effect must be DECLARED (via the `@host` timing signature, §2.2),
  and an undeclared-effect capture must fail closed, not default to Span. This is
  precisely why the killed heuristic (§2.1.1) cannot simply be *replaced* lock-
  free — the correct replacement reads a declared effect, which is LOCK-1-bound.
- **R2 — totality% gaming via Span coarsening.** Expressing every leaf as
  `-> Span` maximizes the naive count while deferring all semantics to host
  consumers. Counterexample: the §3.1 weighting (typed `-> T` = 1.0, re-decoded
  `-> Span` = partial-host); `check-host-floor` must credit typed leaves, or the
  metric is a vanity number.
- **R3 — resolve_refs mis-tiered.** Naming resolve_refs the "context tier"
  archetype conflates a comptime pass (zero by construction) with TOML's runtime
  push_path (zero only if measured). Counterexample: an `[[array-of-tables]]`
  whose depth is data-dependent cannot be resolved at comptime, proving the two
  are separate sub-tiers (§4).
- **R4 — the closure/`|params|` gap balloons the IR.** Grammar functions need a
  net-new `ExprKind::Closure` + `GrammarCall` pair and beta-reduction; if not
  bounded, the lock-free floor inflates toward a second language. Counterexample:
  bbnf.bbnf uses closures at exactly one rule (`closure`, `:51`) invoked via
  `call_arg`/`term(…)` (`:27`,`:30`); scope the augment to that surface, or
  declare closures a LOCK-1-bound (typed multi-param signature) deferral.
- **R5 — floor-gate circumvented by enum dispatch.** As GN-5.W3-R3 warned for
  name-gates, a host route keyed on a grammar-identity enum rather than a
  string would pass a naive audit. Counterexample: `check-host-floor` counts over
  the *built GrammarIr's* HostCapture/`@host` nodes (structural), not over source
  text, so enum-dispatch cannot hide a host route from the counter.

---

## §6 Prior art (citations)

- **tree-sitter external scanners** (`externals` + `scanner.c`) — the canonical
  host-function-lattice: context-dependent tokens (heredocs, indentation, raw
  strings) the declarative `grammar.js` cannot express are delegated to a hand-
  written C scanner behind a fixed token-symbol interface. Totality% is literally
  "how much lives in grammar.js vs the external scanner." Strongest anchor for
  §3's counter.
- **Attribute grammars** (Knuth, *Semantics of Context-Free Languages*, 1968) —
  the CONTEXT tier IS inherited/synthesized attributes; `resolve_refs` is a
  synthesized name-resolution pass, `push_path` an inherited table-path attribute.
  Reference-attribute-grammar systems (Silver/Copper, UMN; JastAdd) formalize
  the "how complete is the declarative layer" question §3 measures.
- **MLIR OpInterface / dialect ops** — a declared op interface with multiple
  implementations mirrors the typed host-fn signature (§2.2); "in-dialect vs
  external call" is the totality axis.
- **GHC Template Haskell splices `$(…)`** and **Lean `@[extern "c_fn"]`** — typed
  escape hatches into the host language with a declared type at the boundary; the
  `@{}` capture is the TH-splice analogue, `@host name : T` the Lean-extern
  analogue (a trusted, typed FFI declaration bounding the kernel).
- **rustc name resolution** (`rustc_resolve`, a distinct pass over HIR before
  type-check) — the comptime-context precedent for resolve_refs; early- vs late-
  bound resolution maps to §4's comptime/parse-time sub-tier split.
- **ANTLR embedded actions / `@members`, Marpa/OMeta semantic actions** — the
  `{…}` host-escape lineage; their weakness (untyped, unbounded actions) is
  exactly what the TYPED signature + totality floor discipline.

---

## §7 What a prototype must test to move 7C

1. **Land the lock-free floor** (§2.1): `ExprKind::HostCapture` + `parse_atom` `@`
   arm + `general_descent` host-call emit; the `?w`→`->` and `|params|` parser
   augments; delete `has_parse_time_host_decode` (`passes:794`). GATE: bbnf.bbnf
   parses into GrammarIr and builds (add_expr + resolve_refs), round-trips
   parse→pretty→parse identical, and all five live grammars regen BYTE-IDENTICAL
   after the heuristic deletion (proving it was dead).
2. **Route bbnf.bbnf through `general_descent`** (drive `!has_rule("value")`,
   `runtime_generator.rs:98`, never `json_sink_direct`): emit `parse_<rule>`
   recognizers, reparse the `.bbnf` corpus into GrammarIr-equivalent structure
   within the `add_expr`+`resolve_refs` host budget, and prove `regen∘regen`
   byte-identical. This is the shared open empirical question — do NOT seed the
   verdict; report where (if anywhere) it cascades and at which construct.
3. **Compute totality% on the fleet** and install `check-host-floor` with the
   planted-breach falsification test (§3.2). GATE: gate trips on the planted host-
   route, passes clean on the real tree; per-subsystem floors named and met.
4. **Prove CONTEXT-tier zero-cost** (§4): resolve_refs comptime round-trip
   (identical GrammarIr) AND a TOML `push_path` runtime alloc-zero counter
   (`payloads().len() == 0` after nested-table parse) — both RED until measured.
5. **Scope the closure augment** (§5-R4): decide `|params|` as bounded lock-free
   IR (Closure+GrammarCall) or as a LOCK-1-bound typed-signature deferral; a
   prototype that closes bbnf.bbnf's single closure site settles it.
