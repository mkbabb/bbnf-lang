# LOCK-7 pass-2 — research-C — Family 7C: THE HOST-FUNCTION-LATTICE

## Thesis

Grammar totality (Constitution i) is not all-or-nothing self-host; it is a **measured
percentage of surface behind grammars, with a named floor per subsystem** — and the boundary
between grammar-plane and hand-code is a **typed host-function interface**, not a substring
sniff. Family 7C makes that boundary concrete: it (1) replaces the one extant string-matching
host heuristic with a first-class typed host node, (2) lowers `@{}` from a *recognized-and-
rejected* construct into that node, (3) names `resolve_refs` as the archetypal CROSS-RULE host
function and proves its zero-cost invariant by round-trip, and (4) specifies a totality% counter
+ floor-gate as a GN-5 `check-name-gates` sibling. The lattice has three tiers — LEAF-DECODE
(pure, `->`-adjacent), CONTEXT (`&mut Ctx`, e.g. `push_path`), CROSS-RULE (`resolve_refs`/
`validate`, whole-grammar TreeFold) — and totality% is the fraction of leaves and branches whose
semantics live in the grammar plane rather than behind a tier-2/tier-3 signature.

## The substrate as it stands (path:line, skinny@HEAD)

**The only host escape in the IR is untyped.** `ExprKind` (`crates/ir/src/lib.rs:234-271`) has
ten variants; the sole host-ish one is `Annotation { name: String, value: Option<String> }`
(`:257`). It carries *strings*, not a signature, not a type. Codegen never consumes it: `grep
Annotation crates/codegen/src/` is empty — `Annotation` is a parse/passes-level artifact that
the emitters ignore. That is a **consumer-less-substrate** exposure (Constitution viii): the host
boundary exists in the AST but reaches no backend.

**The one heuristic to kill** is `has_parse_time_host_decode`
(`crates/passes/src/lib.rs:794-816`). It gates eager-tape selection (`requires_eager_tape`,
`:762`) by string-sniffing the annotation:

```
name.contains("host") && (value.contains("decode") || value.contains("parse"))
```

A semantic decision keyed on substrings of an untyped payload — a name-gate in spirit. Its three
siblings in the same `requires_eager_tape` disjunction (`has_recovery_annotation` `:769` sniffs
`"recover"`; `has_layout_policy` `:818`; `has_dispatch_overlap` `:822`) show the pattern is
systemic: the passes plane reads host intent by grepping annotation text because the IR gives it
nothing typed to read.

**`@{}` is recognized and rejected, not lowered.** At the runtime-frontend level `@{` is scanned
(`crates/grammar/src/lib.rs:420 rest.starts_with("@{")` → `parse_runtime_host_capture:799`,
producing a `RuntimeHostCapture` with a raw `body: String`). But `RuntimeConstructKind::
HostCapture` maps to error code `"BBNF-UNSUPPORTED-HOST-CAPTURE"` (`grammar/src/lib.rs:~222`, the
`unsupported()` arm) — the skinny compiler *recognizes `@{}` and refuses it*. Crucially the
**hand-builder** `parse_atom` (`crates/grammar/src/lib.rs:1495`) has **no `@` arm at all** — it
dispatches only on `"`, `/`, `(`, and ident-start. So `@{}` cannot even enter `GrammarIr` via the
builder path. This is one of the three bounded parser gaps (with `?w`→`->` attachment and
`|params|` closures) that block routing `bbnf.bbnf` through the general-descent emitter, and it is
the gap 7C owns.

**`resolve_refs` is the archetypal cross-rule host.** `crates/ir/src/lib.rs:84-108`: one pass
over `self.exprs`, resolving every `Ref { name, target }` to `target: Some(rule_id)` via a
name→RuleId map built from `self.rules`. It mutates `&mut self`, it is the *cross-rule link* the
census names, and it has exactly one caller (`crates/grammar/src/lib.rs:33`, inside `parse`).
`validate` (`:110`) / `validate_resolved_refs` (`:300`) is its read-only twin. These two are the
whole "host floor" for construction today: node *values* are pure `->` (`Project`, `:261`); the
only *mechanisms* that are host-bound are `add_expr` (append, `:51`) and `resolve_refs`
(cross-rule TreeFold).

**`?w` is already an `Annotation`.** `parse_seq` (`crates/grammar/src/lib.rs:1370-1379`) emits
`ExprKind::Annotation { name: "ws", value: None }` for a standalone `?w`, and the eager-tape
detector at `:1888` reads it back by `matches!(… name == "ws")`. So the Annotation-as-host-marker
pattern is already load-bearing; 7C's job is to give it a *type*, not to invent it.

## The mechanism made concrete: the typed host-fn lattice

### The node

Introduce a first-class typed host node — either a new `ExprKind::HostCall { sig: HostSig, body:
ExprId, .. }` or (parsimony-preferred, KISS) a *typed refinement of `Annotation`* whose payload
is a `HostSig` instead of `Option<String>`. `HostSig` declares:

- `tier: HostTier` ∈ { `LeafDecode`, `Context`, `CrossRule` };
- `inputs: Vec<TypeDesc>`, `output: TypeDesc` (backend-agnostic `TypeDesc::Named`, per memory
  `backend-agnostic-types`);
- `ctx: Option<TypeDesc>` — present iff the fn takes `&mut Ctx` (the tier-2 discriminant).

The tier is **derived, not declared** wherever possible: a host node whose `ctx` is `None` and
whose body reaches no `Ref` outside its own subtree is `LeafDecode`; one with `ctx: Some(_)` is
`Context`; one operating over `self.rules` (whole-grammar) is `CrossRule`. Tiering becomes a
*fact* the counter can audit, not a promise.

### The three tiers, each with a live exemplar

| tier | discriminant | exemplar in-tree | zero-cost claim |
|------|-------------|------------------|-----------------|
| LEAF-DECODE | pure, `->`-adjacent | a datetime/number leaf decode (GN-5 W2 TOML risk R2) | erased to the `->` projection; no runtime beyond the decode |
| CONTEXT | `&mut Ctx` | TOML `push_path` (table-path stack, GN-5 W2); `?w` ws-context | context stack is parse-scoped scratch; zero residual after parse |
| CROSS-RULE | whole-grammar TreeFold | `resolve_refs` (`ir/src/lib.rs:84`), `validate` (`:110`) | comptime link; `target` fields inlined at codegen — zero runtime |

The lattice is **partial-ordered by capability**: LEAF ⊑ CONTEXT ⊑ CROSS-RULE. Totality prefers
the *lowest* tier that expresses a leaf; a leaf that could be `->` must not silently climb to a
host shim (GN-5 W2 R2's exact hazard: "a host function that hard-codes TOML datetime parsing is a
name-gate in disguise").

## Split independence (the charter's central axis)

### Lock-free — 7C can land these without LOCK-1

1. **Kill the string heuristic.** Replace `has_parse_time_host_decode` (`passes/src/lib.rs:794`)
   with a typed predicate over the host node's `tier`/`ctx`: eager-tape is required iff a
   reachable host node is `Context`-or-higher with a parse-time side-effect flag on its `HostSig`.
   No inference needed — the tier is a structural fact of the node. This also retires the sibling
   `.contains("recover")` sniff (`:772`) once recovery is a typed annotation. **Seam:**
   `requires_eager_tape:762`.

2. **Lower `@{}` into the node.** Add an `@` arm to `parse_atom` (`grammar/src/lib.rs:1495`)
   that, on `@{`, calls the existing balanced-brace scanner (`runtime_skip_balanced`, used by
   `parse_runtime_host_capture:804`) and emits the host node with `tier` inferred from the body.
   Flip `RuntimeConstructKind::HostCapture` off the `"BBNF-UNSUPPORTED-HOST-CAPTURE"` list
   (`grammar/src/lib.rs:~222`) once the builder path accepts it. A *bounded parser augment* — the
   charter's "augment BBNF, never work around" (memory `no-workarounds-self-hosting`) applied
   literally.

3. **Name `resolve_refs` the CROSS-RULE floor.** No code change — a *designation*: `resolve_refs`
   + `validate` ARE the tier-3 host surface, and the census (add_expr append + resolve_refs link)
   is complete. Any future cross-rule semantics (inherited attributes, scope resolution) extends
   *these two*, not a new subsystem (memory `system-cohesion`).

### LOCK-1-bound — these wait on the inference verdict (XL-3)

4. **Typed signature INTO inference.** For the `->`/host boundary to be *checked* (not merely
   declared), `HostSig.inputs/output` must feed LOCK-1's HM solver so a host fn composed with a
   `Project` unifies types across the seam. This depends on LOCK-1's **inherited-attribute
   verdict** (DIRECTIVES shared fact: "the host floor depends on a LOCK-1 inherited-attribute
   verdict") — whether the type plane carries synthesized-only or also inherited attributes
   decides whether CONTEXT-tier `&mut Ctx` fns are typeable at all.

5. **Tier off `&mut Ctx`.** The LeafDecode/Context boundary IS the presence of an inherited
   attribute (the `Ctx`). If LOCK-1 declines inherited attributes, CONTEXT-tier fns remain
   *opaque* to inference (typed at their boundary, untyped within) — a named, bounded escape, not
   a failure.

**Deliverable caveat (charter):** the census/totality% number **needs bbnf to parse+build** —
the counter runs over `GrammarIr`, and `bbnf.bbnf` only reaches `GrammarIr` once the three parser
gaps close (gap 2 above + `?w`→`->` + `|params|`). So the *counter mechanism* (lock-free, item 6)
can ship and be tested on json/css/csv/toml/sheets immediately; the *bbnf-on-bbnf totality
number* is gated on the parser-augment landing. Report as an ordering, not a blocker.

## The totality% counter + floor-gate (GN-5 `check-name-gates` sibling)

### The counter

Define totality over two populations, mirroring GN-5's leaves-and-branches idiom:

- **Leaves** = `Literal` + `Regex` + `Ref` terminals (`ExprKind:246/250/253`).
- **Branches** = `Alt` arms (`:236`) + `Project` targets (`:261`).

`totality% = (grammar-expressed leaves+branches) / (total leaves+branches)`, where "grammar-
expressed" = reachable without traversing a host node of tier ≥ CONTEXT. A LEAF-DECODE host
counts as grammar-expressed (it *is* the `->` value); CONTEXT/CROSS-RULE count as host-behind.
Per-subsystem: compute per grammar (json/css/csv/toml/sheets/bbnf) and per emitter blob. A pure
`GrammarIr` walk — no inference, lock-free.

### The floor-gate (xtask subcommand, `syn`-backed like W3)

`cargo xtask check-totality` walks each generated grammar's `GrammarIr`, computes the ratio, and
**fails if any subsystem drops below its declared floor** (e.g. leaf grammars ≥ 95%, the
operator-tower sheets ≥ 80%). The exact sibling of GN-5 W3's `check-name-gates` (`GN-5.md:126,
§4 W3`) — an AST audit, not a grep (precept 19: a hard gate needs an artefact).

### Planted-breach falsification (the gate must be able to fail)

Mirroring W3's `name_gate_falsification.rs` (`GN-5.md:310`): a test plants a host node where a
`->` would suffice (wraps a plain literal leaf in a synthetic CONTEXT host), asserts
`check-totality` reports the ratio *dropped* and trips the floor, then asserts the real tree
passes clean. A totality gate that cannot fall is a slogan; this proves it measures.

## Zero-cost invariant of the CONTEXT tier (testable NOW, pre-schedule)

Constitution v — the type plane costs zero at runtime — must hold for the CONTEXT tier *before*
it is scheduled. Two concrete tests, both runnable today:

1. **`resolve_refs` round-trip idempotency.** `resolve_refs` is a comptime link; `resolve_refs ∘
   resolve_refs` must be a no-op (every `target` already `Some`, the map rebuild deterministic).
   Test: parse → `resolve_refs` → serialize `GrammarIr` → `resolve_refs` again → assert
   byte-identical serialization AND assert codegen emits identical `.rs` (the `target: RuleId` is
   inlined as a direct call, never a runtime lookup — zero residual). A *non-vacuous* determinism
   check because `resolve_refs` genuinely mutates on first run.

2. **TOML `push_path` residue = 0.** The GN-5 W2 table-path CONTEXT fn pushes/pops a path stack
   during parse. Test: after a full parse the stack is empty (balanced push/pop) and the emitted
   tape carries no `Ctx` field — the stack is parse-scoped scratch, erased at emission. A cold
   per-parse bench (standing law, memory `no-warm-benches`) of a `push_path` grammar vs its
   context-free twin must show **no runtime delta beyond the mutation itself** (ins/B the
   load-proof floor).

If either fails, the CONTEXT tier is *not* zero-cost and must be redressed before LOCK-1 is asked
to type it.

## Prior art (citations)

- **Tree-sitter external scanners** — the strongest analog: context-dependent tokens (Python
  indentation, Rust raw-string delimiters, HTML implicit close tags) live behind a **typed**
  interface: an `enum` of external token names ordered to match the grammar's `externals` array,
  plus `scan`/`serialize`/`deserialize`. The serializable scanner *state* is precisely a
  CONTEXT-tier `&mut Ctx` made persistable at node boundaries so scanner-dependent subtrees
  reparse without replay. 7C's lattice is this generalized to three tiers with a *measured*
  totality boundary.
- **ANTLR semantic predicates & actions** (`{...}?` guards, `{...}` actions) — the direct
  ancestor of `@{}`: host code embedded at a grammar point to resolve context the CFG cannot.
  ANTLR's failure mode (untyped, target-language-coupled actions) is exactly what a *typed*
  `HostSig` + backend-agnostic `TypeDesc` avoids.
- **GHC primops / `foreign import`** and **rustc lang-items/intrinsics** — the "primitive floor"
  precedent: a small, typed, hand-written kernel set the compiler treats as opaque but
  *type-checked at the boundary* — the model for LEAF-DECODE primitives ("what 'primitive'
  lawfully means", LOCK-7 task).
- **Attribute grammars (Knuth) / Menhir-Marpa semantic actions** — frame the synthesized-vs-
  inherited attribute split that LOCK-1's verdict decides; inherited attributes are exactly the
  `&mut Ctx` CONTEXT tier.

Sources: [Tree-sitter External Scanners](https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html),
[Tree-sitter Lexer & Tokenization (DeepWiki)](https://deepwiki.com/tree-sitter/tree-sitter/2.6-lexer-and-tokenization).

## Named risks and counterexamples

- **R1 — the typed node stays consumer-less.** If `HostCall`/typed-`Annotation` lands but no
  emitter reads it, it repeats the current `Annotation` pathology (codegen ignores it,
  Constitution viii). *Guard:* the eager-tape kill (item 1) is the in-wave consumer — the node is
  read by `requires_eager_tape` the moment it exists. No node lands without that reader.
- **R2 — host climb hides overfitting.** A leaf expressible as `->` shoved into a CONTEXT host
  passes a name-gate audit while *lowering* totality% (GN-5 W2 R2). *Counterexample:* the
  planted-breach test plants exactly this and asserts the ratio falls — totality% is the gate the
  name-gate audit cannot be, catching the climb by measurement.
- **R3 — `@{}` body is opaque host source.** If `@{}` carries raw target-language text (like
  ANTLR actions), it is un-typeable and un-mirrorable across rust/ts (breaks Constitution iii,
  LOCK-6). *Counterexample:* restrict `@{}` bodies to a declared `HostSig` reference (a *named*
  host fn), not inline foreign code — the body is a call, its definition lives in a typed host
  table. Inline foreign text is refused (`"BBNF-UNSUPPORTED-HOST-CAPTURE"` *stays* for untyped
  bodies).
- **R4 — totality% is gameable by counting.** Choosing the leaf/branch population to flatter the
  ratio makes the number meaningless. *Guard:* pin the population definition (leaves =
  Literal+Regex+Ref terminals; branches = Alt arms + Project targets) in the gate source and
  falsify with a mutation, exactly as W3 pins name-gate forms.
- **R5 — CONTEXT tier isn't zero-cost.** If `push_path`/`resolve_refs` leave runtime residue,
  Constitution v breaks. *Counterexample:* the two round-trip tests above are born-RED — the tier
  is unschedulable until both are green.

## What a prototype must test to move 7C

1. **The kill round-trips.** Introduce the typed host node, rewrite `requires_eager_tape` to read
   `tier`/`ctx`, regenerate all five grammars, assert `check-json/css/csv/sheets` bit-for-bit
   (behavior-preserving on the existing corpus, since only `?w`/no host nodes exist there).
2. **`@{}` reaches `GrammarIr`.** Add the `parse_atom` `@` arm; parse a grammar containing
   `@{name}`; assert it builds a host node (not `BBNF-UNSUPPORTED`) and that `add_expr` +
   `resolve_refs` accept it — the concrete step toward routing `bbnf.bbnf` through the emitter.
3. **The totality counter runs and the floor-gate falsifies.** Compute totality% on all five
   grammars; plant a spurious CONTEXT host; assert the ratio drops and `check-totality` trips;
   assert the clean tree passes.
4. **CONTEXT zero-cost holds.** `resolve_refs ∘ resolve_refs` byte-identical + identical codegen;
   TOML `push_path` cold-bench delta within noise (ins/B floor).
5. **The LOCK-1 seam is exercised (or its absence named).** Feed one `HostSig.inputs/output` into
   the inference entry point; if LOCK-1's inherited-attribute verdict is unresolved, record the
   CONTEXT tier as boundary-typed-only and name the exact gap — do not seed the verdict.

The unseeded empirical question 7C carries into the joint: **does authoring `-> T` builder
projections over the recognizer emitter reparse the corpus into `GrammarIr`-equivalent structure
within the `add_expr`+`resolve_refs` host budget** — i.e., is the CROSS-RULE floor *sufficient*
for a building (not merely recognizing) self-host, or does a construct force a new tier? That is
the measurement that decides whether the lattice is complete at three tiers.
