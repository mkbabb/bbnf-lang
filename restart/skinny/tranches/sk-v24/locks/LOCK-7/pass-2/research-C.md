# LOCK-7 pass-2 · research-C — family 7C: THE HOST-FUNCTION LATTICE

Grammar totality is not "everything is a `->`". It is a **three-tier lattice**: a pure leaf floor
that is already total, a comptime CONTEXT tier that resolves cross-rule and cross-document
structure at zero runtime cost, and a stateful RECURSIVE tier that lives behind *declared, typed*
host signatures. Totality is then a measured percentage — the fraction of grammar surface (leaves ×
branches) that never crosses into the top tier — with a per-subsystem floor enforced by a
falsifiable gate. This family makes the lattice concrete, splits its independent work from its
LOCK-1-bound work, specifies the counter and its gate, and pins the zero-cost invariant of the
middle tier so it can be scheduled honestly.

---

## 1. The lattice made concrete — three tiers, one classifier

**Tier 0 — PURE LEAF (grammar-expressed, already total).** A production whose value is a `->`
projection over `Literal`/`Regex`/class-run scan. The hand builder already parses `->`
(`grammar/src/lib.rs:1404 parse_postfix` → `parse_projection:1481`, boundary scan
`parse_runtime_projection` at `grammar/src/lib.rs:~838`), lowering to `ExprKind::Project { body,
target, ty }` (`ir/src/lib.rs:234` enum). No host. Zero runtime beyond the recognizer. Every
structural grammar in the fleet (json/css/csv/sheets/toml) is Tier-0-dominant.

**Tier 1 — CONTEXT (comptime, zero runtime cost).** Cross-rule and cross-document *structure*
resolved before any byte is parsed at runtime. The canonical mechanism is **`resolve_refs`**
(`ir/src/lib.rs:84`): it builds a `name → RuleId` map over `self.rules` and mutates every
`ExprKind::Ref { name, target }` to fill `target: Some(rule_id)` (`ir/src/lib.rs:95-105`). It is a
pure build-time TreeFold over `self.exprs`; its *only* output is an `Option<RuleId>` link — an
`ExprId`-space edge, never emitted code. `validate` (`ir/src/lib.rs:110`) is its sibling verifier.
The document-path analogue is TOML's `[table]`/`[[array]]` **`push_path`** (GN-5.W2, `toml.bbnf`):
section nesting is a *path context* resolvable at parse-structure time, not a runtime host state
machine. Tier 1 is where the grammar-homoiousia route (LOCK-3C) and self-host meet: the node-value
census is pure `->`, and the *build* mechanisms — `add_expr` append (`grammar/src/lib.rs:1351…1501`)
and `resolve_refs` cross-rule link — are Tier-1 comptime host, not Tier-2.

**Tier 2 — RECURSIVE / STATEFUL (LOCK-1-bound, typed signature).** Context-*dependent* semantics
that cannot be discharged at structure time: they need a mutable parse context `&mut Ctx`
(position-sensitive tokens, recursive re-entry, host decode). This is exactly the `@{ … }`
host-capture atom. The tier boundary is: **does the declared signature take `&mut Ctx`?** If no, it
demotes to Tier 1 (pure, zero-cost). If yes, it is Tier 2 and its *type* must flow into LOCK-1's
solver. XL-3.

The classifier that assigns a node its tier is one predicate over the IR, not a name test: Tier-0
if the subtree is `Project`/`Literal`/`Regex`/`Seq`/`Alt` of same; Tier-1 if it is a `Ref` or a
declared *pure* host; Tier-2 iff it is a host with a `&mut Ctx` signature. This is the pluggable
decision point (memory: `pluggable-components`) that the totality counter (§3) reads.

---

## 2. Split independence — what moves lock-free vs what waits on LOCK-1

### 2a. Lock-free (no LOCK-1 verdict needed)

**(i) KILL the substring heuristic at `passes/src/lib.rs:794-816`.** `has_parse_time_host_decode`
decides `requires_eager_tape` (consumer live at `passes/src/lib.rs:516`, mapped over every rule)
by sniffing annotation *text*: `name.contains("host") && (value.contains("decode") ||
value.contains("parse"))` (`:798`). This is a name-gate in spirit — a string-substring predicate
standing in for a type fact. Replace it with a **structural** signal: a lowered
`ExprKind::Host { sig }` node whose `sig` carries an explicit `eager_tape: bool` (or a
`ParseTimeDecode` capability) resolved from the host-fn declaration. The eager-tape decision becomes
a property read, not a substring match. Falsification floor: byte-identical regen of
json/css/sheets after the swap (no shipped grammar's eager-tape choice may change) —
`grep -rn '@{' grammar/` shows `@{}` appears only in `grammar/bbnf/bbnf.bbnf`,
`grammar/bbnf/types.bbnf`, `grammar/css/l4/{color,values}.bbnf`, none of which route through
skinny's declared-json emitter today, so the swap is measurably inert on the shipped corpus.

**(ii) LOWER `@{}` — retire the reject at `grammar/src/lib.rs:222`.** Today `@{ … }` is
*recognized then rejected*: the runtime scanner sees `@{` (`grammar/src/lib.rs:420`), calls
`parse_runtime_host_capture` (`grammar/src/lib.rs:~800`) which balanced-skips to `}` and captures
`body: String` (struct `RuntimeHostCapture`, `:170`), then `RuntimeConstructKind::HostCapture` maps
to the error code `BBNF-UNSUPPORTED-HOST-CAPTURE` (`:222`). Critically the **hand builder** has no
`@{` arm at all: `parse_atom` (`grammar/src/lib.rs:1495`) handles `"`, `/`, `(`, ident — never `@`.
Lowering is three edits: add `ExprKind::Host { body: String, sig: HostSig }` to the IR enum
(`ir/src/lib.rs:234`); add an `@` arm to `parse_atom` reusing `runtime_skip_balanced`; bind `body`
to a declared signature at `resolve_refs` time (a `name → HostSig` table alongside the existing
`name → RuleId` map at `ir/src/lib.rs:85`). The scan layer already yields `body` offsets — the
builder just keeps them instead of discarding to an error.

**(iii) NAME `resolve_refs` as the Tier-1 reference mechanism** (`ir/src/lib.rs:84`) — it is
already the cross-rule link; the family's job is only to *label* it Tier-1 and assert its zero-cost
property (§4). No code motion.

### 2b. LOCK-1-bound (waits on the solver spine)

- **Typed signature into inference.** The `HostSig` of a `@{}` capture — its argument types and
  result type — is a *type*, and Tier-2 host results must unify with the enclosing rule's inferred
  projection type. This is a LOCK-1 obligation (HM/CSP over `->` shapes, 1A/1B); the host-fn
  declaration is a leaf in LOCK-1's type lattice, not a bbnf-side concern.
- **Tier selection off `&mut Ctx`.** Whether a host fn is Tier-1 (pure) or Tier-2 (stateful) is the
  **inherited-attribute verdict** the construction census defers to LOCK-1. Attribute grammars
  (Knuth 1968): `resolve_refs`/synthesized attributes are Tier-1; a `&mut Ctx` recursive host is an
  inherited attribute threaded top-down — precisely what a bidirectional/constraint solver must
  schedule. XL-3.

**Deliverable dependency (stated, not hidden):** the census / totality% number (§3) requires
`bbnf.bbnf` to **parse *and* build** through the emitter's `!has_rule("value")` branch
(`codegen/src/runtime_generator.rs:98` → `emit_general_descent`). Until `@{}` is lowered (2a-ii),
bbnf.bbnf cannot build, so the counter runs only on the host-free fleet and reports bbnf's tier mix
as RED-unknown. The counter is *specifiable* now; its bbnf row is gated on 2a-ii landing.

---

## 3. The totality% counter (leaves × branches) and its floor-gate

**Definition.** Over a grammar's `GrammarIr`, count two structural populations:
- **leaves** = terminal-producing nodes: `Literal`, `Regex`, and each `Project` target (the value
  sites).
- **branches** = dispatch points: each `Alt` arm (`ir/src/lib.rs` `ExprKind::Alt.branches`) and each
  `Optional`/`Repeat` decision.

Classify each by its tier (the §1 classifier). Then

```
totality% = 1 − (Σ Tier-2 leaves+branches) / (Σ all leaves+branches)
```

Tier-0 and Tier-1 count as *behind grammar* (comptime, zero runtime host); only Tier-2 (`&mut Ctx`
host) subtracts. This is the crucial anti-gaming property: **hiding leaf semantics in a host fn
LOWERS totality%, it cannot hide behind it** — the exact counter to GN-5.W2 R2 (a host fn that
hard-codes TOML datetime parsing is a name-gate in disguise, `GN-5.md:519-524`). A per-subsystem
**floor** is declared (e.g. `json ≥ 100%`, `css ≥ 100%`, `sheets ≥ 100%`, `toml ≥ 98%` if datetimes
go host, `bbnf ≥ X%` with `@{}` recursion accounted). The floor is a fact, not a slogan.

**The gate — a GN-5 `check-name-gates` sibling.** GN-5.W3 introduces `cargo xtask check-name-gates`
(AST audit, `syn` walk, `GN-5.md:301-316`); skinny's xtask has neither it nor this sibling yet
(`skinny/xtask/src/main.rs:40-52` roster: regen/check-json, sheets, css, real-typed, conformance,
lint-loc — no gate). Add **`cargo xtask check-totality`** colocated in the same `match` arm: for
each fleet grammar, load its `GrammarIr`, run the §1 classifier, compute totality%, and `bail!` if
any grammar falls below its declared floor. The floors live in `[workspace.metadata.bbnf]` beside
`host_registry` (`skinny/Cargo.toml:58`, today `host_registry = "skinny-none"` — an *empty*
registry, so the fleet reads 100% today and the gate is trivially green: **non-vacuity requires
bbnf.bbnf wired**, §5/R-C3).

**Planted-breach falsification (precept 19 — a gate must be proven able to fail).**
`skinny/xtask/tests/totality_falsification.rs`: (a) inject a synthetic `ExprKind::Host { &mut Ctx }`
into a grammar whose floor is 100% and assert `check-totality` trips; (b) lower a grammar's declared
floor above its measured value and assert it trips; (c) assert the real fleet passes clean. A gate
that cannot fail is a defect (Constitution vii).

---

## 4. The CONTEXT-tier zero-cost invariant — testable before it is scheduled

Tier 1 may only be scheduled if its zero-runtime-cost claim (Constitution v) is *proven testable*,
not asserted. Two concrete tests, both runnable on skinny HEAD:

- **`resolve_refs` round-trip.** `parse_grammar(src)` → `resolve_refs` → `pretty()`
  (`ir/src/lib.rs:124`) must equal `parse_grammar(src)` → `pretty()`: `resolve_refs` mutates only
  `target: Option<RuleId>` (`ir/src/lib.rs:103`), a comptime link `pretty` does not render, so the
  surface is provably unchanged. The stronger emit-nothing proof: `grep` the generated `.rs` for any
  construct derived from a `Ref.target` value — it is zero, because the emitter dispatches on rule
  *identity*, not on the resolved link carrying runtime weight.
- **TOML `push_path` round-trip (GN-5.W2 consumer).** A `[a.b]`/`[[a.b]]` document's canonical
  key-path map is identical whether the path is threaded as a Tier-1 comptime structure or a Tier-2
  runtime host state — if the two agree byte-for-byte across the ≥40-doc corpus
  (`crates/test-fixtures/corpus/toml/`), `push_path` is provably Tier-1 and costs zero at runtime.

Only after both pass does the CONTEXT tier earn a schedule slot. This is the honest gate on
`resolve_refs` and `push_path` the family requires before that tier is scheduled.

---

## 5. The shared open empirical question — RED-unknown, and how to drive it

*Do `-> Span`/`-> T` builder projections authored on bbnf's structural rules, over the recognizer
emitter, reparse the corpus into GrammarIr-equivalent structure — within the measured
`add_expr` + `resolve_refs` host budget — while `regen∘regen` holds byte-identical?*

Report **RED-unknown; unseeded.** The route: author `->` projections on `bbnf.bbnf`'s rules, drive
`emit_sink_only`'s `!has_rule("value")` branch (`codegen/src/runtime_generator.rs:98`) — **never**
`json_sink_direct` (`:101`) — regen `bbnf/*`, and diff the reparsed `GrammarIr` against the hand
builder's. Do not seed the verdict from the legacy emitter or from the emitter's mere existence: the
general-descent emitter emits *recognizers*, one `parse_<rule>` per rule (`general_descent/`
~1206 LOC); whether a *builder* augment over it round-trips is genuinely open. The prototype must
localize the first construct where it cascades — candidates: `@{}` capture, `|params|` closures,
`?w`→`->` attachment at `parse_seq` — or certify equivalence.

---

## 6. Prior art (cited)

- **tree-sitter external scanners** — the canonical host-function lattice: grammar expresses the
  bulk; a typed C API (`scan(void *payload, TSLexer *, const bool *valid_symbols)`) handles
  context-dependent tokens (indentation, heredocs, raw strings). `TSLexer *` IS the typed
  `&mut Ctx`; `valid_symbols` is the tier boundary. `@{}` is bbnf's external scanner; the lesson is
  that the interface is *typed and narrow* and the grammar declares which symbols it may produce.
- **Knuth attribute grammars (1968)** — synthesized vs inherited attributes is exactly Tier-1 vs
  Tier-2. `resolve_refs` = synthesized (bottom-up, pure); `&mut Ctx` recursion = inherited
  (top-down, scheduled). The LOCK-1 inherited-attribute verdict rests on 57 years of theory.
- **GHC typed primops / `foreign import`** — the host boundary carries a *declared type*; primops
  are total functions the compiler trusts by signature, not by name. The `HostSig` model.
- **yacc/bison/ANTLR semantic actions (`{ … }`)** — `@{}` is the action block; ANTLR's typed rule
  return values and `@members` are the typed-signature discipline bbnf must adopt (untyped actions
  are the anti-pattern the lattice forbids).
- **rustc lang-items / MLIR external dialect ops** — a fixed, typed set of "known to the compiler"
  operations distinguished structurally (a registry), never by string name — the model for a real
  `host_registry` replacing `"skinny-none"`.

---

## 7. Named risks and counterexamples

- **R-C1 — the substring-heuristic swap changes an eager-tape decision.** If a shipped grammar's
  eager-tape selection secretly depends on `@host decode` annotation text, the structural swap
  (2a-i) flips it. *Guard:* byte-identical json/css/sheets regen is the born-RED gate; `grep`
  confirms `@{}`/`@host` are absent from the shipped-through-skinny corpus, so the swap is
  measurably inert. A divergence means the annotation was load-bearing and its type fact must be
  modeled explicitly first.
- **R-C2 — `@{}` lowering needs `&mut Ctx` → borrow-checker escape.** A Tier-2 host body that
  mutates parse context is the named kernel-escape (7B territory). *Guard:* the `HostSig` declares
  `&mut Ctx` explicitly; a capture that declares purity but touches context fails a signature check.
  The escape is *declared*, never silently inferred.
- **R-C3 — the totality counter is vacuous.** With `host_registry = "skinny-none"`
  (`skinny/Cargo.toml:58`) every fleet grammar reads 100% Tier-0/1 and the gate never exercises the
  `<floor` branch. *Guard:* non-vacuity requires at least one grammar that genuinely needs a Tier-2
  host — bbnf.bbnf's `@{}` recursion or TOML datetime — wired through before the gate is trusted;
  until then the counter is *specified but unproven* and must be labeled so (Constitution vii, no
  vague optimism).
- **R-C4 — consumer-less host lattice (Y.13).** A `host_registry` with declared signatures but no
  runtime caller is forbidden substrate. *Guard:* the first host-fn declaration lands in the same
  wave as its bbnf.bbnf or TOML consumer; the registry grows by need, never speculatively.
- **R-C5 — tier misclassification hides logic.** A leaf routed to a Tier-2 host to "look clean"
  actually *lowers* totality% by construction (§3) — the incentive inverts correctly, but only if
  the classifier counts host leaves as Tier-2, not Tier-1. *Guard:* the §3c falsification plants
  exactly this and asserts the number drops.

---

## 8. What a prototype must test to move this family

1. **P1 (2a-ii):** add `ExprKind::Host` + `parse_atom` `@` arm + `resolve_refs` sig-binding; author
   a one-`@{}` toy grammar; assert it parses, builds (`add_expr` + `resolve_refs` succeed), and
   lowers through `!has_rule("value")`. Measures the add_expr+resolve_refs host budget.
2. **P2 (2a-i):** replace `passes/src/lib.rs:798` substring test with an `ExprKind::Host` structural
   read; assert json/css/sheets regen byte-identical (`cargo xtask check-{json,sheets}` + check-css).
3. **P3 (§3):** implement `cargo xtask check-totality` over the fleet; land the three-form
   falsification test; confirm trip-on-breach + clean-on-HEAD.
4. **P4 (§4):** the `resolve_refs` round-trip + emit-nothing grep, and the TOML `push_path`
   Tier-1/Tier-2 agreement test — the zero-cost invariant proof that unlocks scheduling the CONTEXT
   tier.
5. **P5 (§5):** the RED-unknown driver — `-> Span`/`-> T` on bbnf.bbnf over the recognizer emitter,
   regen∘regen byte-check, GrammarIr-equivalence diff, first-cascade localization.
