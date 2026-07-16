# LOCK-7 / family 7C — THE HOST-FUNCTION LATTICE (pass 2, research-C)

Totality is a *bounded, measured* quantity: the fraction of every subsystem's leaf and control
semantics expressed by grammar (`->` projections, `@{}` span-captures, class-run/regex leaf
scans) versus the fraction that lawfully lives behind a TYPED host interface. The family's job
is to draw that boundary as a declared, checkable **tier lattice**, discriminated not by a
user-written purity string but by the one structural fact the compiler can read off a
signature — *whether the function threads `&mut Ctx`* — measure the fraction behind it per
subsystem on two axes, name a floor, and defend that floor with a CI gate that cannot be gamed
as a grammar-name shim.

## 0. Re-founding on the skinny substrate (XL-A) — the pass-1 correction

Pass-1 modelled the lattice on the LEGACY outer-repo machinery (`crates/core` `@host` decode,
the six-variant `FnDescriptor`, `MapExpr::FnCall`). The measurement substrate is skinny@HEAD,
where that machinery **does not exist**: `runtime/src/grammars/json/host.rs` is a 3-line stub
("JSON is host-fn-free in the skinny compiler slice"), `host_registry = skinny-none`
(`codegen/src/lib.rs:307`), the `ExprKind` enum (`ir/src/lib.rs:234`) has ten variants and **no
host node**, and the AST parser (`grammar/src/lib.rs:1495 parse_atom`) has no `@{` branch — `@`
at rule-item level routes to `parse_directive` (`:1276`) which handles only import/token/pretty/
ws and errors otherwise. The greenfield host plane is *almost blank*. That is the opportunity
and the discipline: the lattice is designed on skinny's `ExprKind`, not retrofitted onto legacy
`core`. Every seam below is a skinny path.

## 1. The mechanism — a three-tier lattice keyed on `&mut Ctx`

The skinny census (shared fact): node **values** are pure `->`; the **build mechanisms** are
host-bound Rust — `add_expr` (append, `ir/src/lib.rs:51`) and `resolve_refs` (cross-rule link
by name→RuleId, a TreeFold over `exprs`, `ir/src/lib.rs:84`). The lattice formalizes exactly
this observed split into three tiers, the boundary read off the signature:

- **PURE tier — `fn(Span)->T` / `fn(&[u8])->T`.** Leaf decodes with no parse/build state: the
  `-> T` projections (`ExprKind::Project`, parsed at `grammar/src/lib.rs:1481`) and the `@{}`
  span-capture (validate-inner-then-project-`Span`). Counted as GRAMMAR coverage. Must NOT
  select the eager tape. Today `@{}` is *not in this tier* — it is flagged
  `BBNF-UNSUPPORTED-HOST-CAPTURE` (`ir/src/lib.rs:222`); lowering it is a lock-free move (§2).
- **CONTEXT tier — `fn(&mut Ctx, …)`.** Build-time mechanisms that mutate the GrammarIr / tape-
  assembly context: `resolve_refs` (cross-rule environment), a hypothetical TOML `push_path`
  (table-nesting stack). The discriminant is exact and mechanical: *the signature takes
  `&mut Ctx`*. These run at BUILD/comptime — O(structure), never O(input bytes) — so they
  legitimately route `ShapeRationale::HostFnParseTime` (`ir/src/cost.rs:255`) yet must satisfy a
  runtime zero-cost invariant (§4). This tier is the parser-generator instance of **inherited
  attributes** (context flowing parent→child); `->` projections are synthesized attributes
  (child→parent).
- **RECURSIVE/HOST tier — opaque signed escape.** Genuine context-dependent recursive semantics
  that neither `->` nor a `&mut Ctx` structural fold expresses. The general-descent rule tower
  (`codegen/src/general_descent/`, one `parse_<rule>` recognizer per rule) is the *in-grammar*
  answer for most recursion; this tier is the last resort, floored and audited (§3).

The tier is INFERRED/checked from the signature, not declared — the pass-2 concreteness over
pass-1's defeatible `@pure`/`@context` string. Reading the tier off `&mut Ctx` is what makes the
escape hatch impossible to mislabel.

## 2. The split — lock-free trio vs LOCK-1-bound

**Lock-free (executable now, no solver dependency):**

1. **Kill the string heuristic at `passes/src/lib.rs:794-816` (`has_parse_time_host_decode`).**
   It fires on `ExprKind::Annotation` where `name.contains("host") && value.contains("decode"|
   "parse")` (`:798`) — a fragile string guess, a latent name-gate. Verified live-dead: the fleet
   grammars build no such Annotation node (the only "host" tokens are COMMENTS —
   `google-sheets.bbnf:60`, `color.bbnf:87 ghostwhite` — never built `Annotation` nodes; skinny
   Annotation nodes arise only from `?w`/directives with `name="ws"`). Consumed at `:620`
   (rationale) and `:764` (`requires_eager_tape`). Deleting it is behaviour-preserving — regen
   bit-for-bit across json/css/sheets — because it selects eager on nothing today. The eager
   trigger returns later as a *tier lookup* (the LOCK-1-bound half).
2. **Lower `@{}` (`ir/src/lib.rs:222`).** Add `ExprKind::SpanCapture(ExprId)` to the enum
   (`:234`), a `parse_atom` branch for `@{` (`grammar/src/lib.rs:1495`) matching the self-grammar
   `term = … | "@{" , rhs , "}"` (`grammar/bbnf/bbnf.bbnf:33`), and a general-descent lowering:
   run the inner recognizer, discard its structure, project the matched byte span. This is the
   PURE-tier `Span` corner — the first host construct that counts as grammar coverage. It moves
   `@{}` from UNSUPPORTED to a first-class node without touching inference. (The runtime facts
   harvest already scans the span via `parse_runtime_host_capture`, `grammar/src/lib.rs:420` —
   reuse its boundary logic.)
3. **Name `resolve_refs` as the CONTEXT-tier floor mechanism.** It already exists
   (`ir/src/lib.rs:84`) as the cross-rule TreeFold; the family's move is to *declare* it a
   CONTEXT-tier host mechanism with a `&mut GrammarIr` signature, so the census counts it as the
   irreducible host floor of every multi-rule grammar (every such grammar needs cross-rule
   linking).

**LOCK-1-bound (needs the solver spine):**

- **Typed signature into inference.** Threading `HostFnSig { params: Vec<TypeDesc>, ret, tier }`
  into the type plane so a call site is arity/param-checked and the tier is *inferred* from the
  presence of a `Ctx` param — the check LOCK-1 owns.
- **Tier off `&mut Ctx`.** Deciding the eager-tape trigger by tier (CONTEXT ⇒ eager, PURE ⇒ not)
  is the typed replacement for the killed heuristic; the tier is a solved type-plane fact.
- **The inherited-attribute verdict (XL-3).** The *floor itself* hinges on LOCK-1: if the solver
  provides in-grammar inherited attributes, `resolve_refs` and `push_path` become grammar-
  expressible (context flows in-grammar) and LEAVE the host floor — raising totality%; if not,
  they stay CONTEXT-tier host mechanisms. **The host floor is a function of the LOCK-1
  inherited-attribute verdict** and cannot be finalized before it.

**Binding dependency for the deliverable.** The census/totality% deliverable *needs bbnf to
parse+build*. `bbnf.bbnf` cannot round-trip through the skinny emitter today because of three
bounded parser gaps: `?w`→`->` attachment in `parse_seq` (`:1361`; the `?w` early-`continue` at
`:1366` breaks projection attachment), the `@{}` atom (gap 2 above), and `|params|` closures
(`bbnf.bbnf:51`, no closure `ExprKind`). Gap 2 is 7C's own lock-free move; the other two are
7A/7B parser-totality work. **7C's measured percentage is downstream of parse+build** — name it
a DAG edge, not a same-wave claim.

## 3. The totality counter (leaves × branches) and the floor-gate

**The counter is two-axis** so a host escape cannot hide in either dimension:

- **LEAVES** = terminal projection/scan sites: each `Project`, `SpanCapture`, `Regex`, `Literal`
  leaf. Score each PURE-grammar (expressed by `->`/`@{}`/class-run) vs host-escaped.
- **BRANCHES** = control/dispatch sites: each `Alt` (`AltMode::Dispatch`), `Repeat`, `Optional`,
  `Discard` — the nodes the general-descent emitter walks. Score each grammar-structural vs
  host-absorbed (a host fn that swallows dispatch is a branch escape).
- **totality% = (grammar leaves + grammar branches) / (total leaves + total branches)**, reported
  per subsystem with the PURE/CONTEXT/HOST tier breakdown. `resolve_refs` is the standing CONTEXT
  floor term. Two axes because a grammar that is 100% pure-leaf could still route all dispatch
  through a host tower — a one-axis leaf count would score it high while it is a branch escape.

**The floor-gate — a GN-5 `check-name-gates` sibling.** GN-5.W3 installs `xtask
check-name-gates` (a `syn` AST audit of `codegen/src/`, `GN-5.md:305-321`) with a paired
falsification test (`name_gate_falsification.rs`, three planted forms). The 7C sibling is
`xtask check-totality-floor`: walk each grammar's *built GrammarIr* (hence the parse+build
dependency), compute the counter, compare against a committed per-subsystem floor table (JSON
100%; CSS's two `@{}` counted as grammar leaves; sheets/csv/toml measured), colocated with the
`check-*` arms at `xtask/src/main.rs:39-52`. **Planted-breach falsification** (mirrors W3's
proof-the-gate-can-fail): inject a synthetic grammar whose leaf routes to a HOST-tier escape
instead of `->` (dropping it below floor) and assert the gate trips; inject a grammar-name-
encoding host fn (`@host parse_toml_datetime`) and assert the extended name-gate AST walk — now
traversing the host declaration surface — flags it; assert the real fleet passes clean. A gate
that cannot fall below floor certifies nothing.

## 4. The CONTEXT-tier zero-cost invariant — testable BEFORE scheduling

Constitution v (the context/type plane costs zero at runtime) applies to the CONTEXT tier: a
`&mut Ctx` host mechanism runs at build/comptime, never per input byte. Q3 requires this be
*testable before the tier is scheduled*. Two concrete probes, both runnable on skinny@HEAD (the
second once TOML lands in GN-5.W2):

- **`resolve_refs` round-trip.** `resolve_refs` mutates only `Ref.target` (`ir/src/lib.rs:96-
  104`); it touches zero input bytes and runs once before emission, its result *baked* into the
  generated recognizer as direct `parse_<rule>` calls. Test: `pretty(resolve_refs(parse(G)))`
  is structurally identical to `pretty(parse(G))` (refs resolved, tree unchanged) AND the emitted
  `generated.rs` contains no residual resolution in any hot loop — proving the mechanism is
  structure-preserving and comptime-only. The zero-cost witness for the tier's floor term,
  available *now*, no LOCK-1 needed.
- **TOML `push_path` cyc/B.** The table-nesting stack is O(number of `[table]` headers), not
  O(bytes). Test: TOML parse cyc/B does not scale with `push_path` calls beyond structural count,
  and the offset-only-tape invariant holds (`payloads().len()==0`, the discipline at
  `runtime/src/lib.rs:42-44`). If `push_path` forced per-byte payload materialization the
  invariant fails and the tier is NOT zero-cost — that failure is the scheduling gate. The tier
  enters the DAG only after both probes pass.

## 5. Integration seams (skinny path:line)

- Kill the heuristic: `passes/src/lib.rs:794-816` (`has_parse_time_host_decode`), consumers
  `:620` (`ShapeRationale::HostFnParseTime`), `:764` (`requires_eager_tape`); rationale enum
  `ir/src/cost.rs:255`.
- Lower `@{}`: `ir/src/lib.rs:222` (the UNSUPPORTED flag), `:234` (`ExprKind` — add
  `SpanCapture`); `grammar/src/lib.rs:1495` (`parse_atom` — add `@{` branch), `:420`
  (`parse_runtime_host_capture` span logic to reuse); self-grammar `grammar/bbnf/bbnf.bbnf:33`.
- CONTEXT floor: `ir/src/lib.rs:84` (`resolve_refs`), `:51` (`add_expr`).
- Parse+build gaps (deliverable dependency): `grammar/src/lib.rs:1361` (`parse_seq` `?w`), `:1495`
  (`parse_atom` `@{`), `bbnf.bbnf:51` (`|params|` closure — no `ExprKind`).
- Counter walk target: built `GrammarIr.exprs`; branch/leaf kinds at `ir/src/lib.rs:234-273`.
- Floor-gate sibling: `xtask/src/main.rs:39-52` (the `check-*` dispatch arms); model on GN-5.W3
  `check-name-gates` (`GN-5.md:305-339`).
- Emitter the census scores: `codegen/src/general_descent/{mod,emit,view,regex_first}.rs`; the
  `!has_rule("value")` shape branch it walks, `runtime_generator.rs:98`.

## 6. Prior art (citations)

- **Attribute grammars (Knuth 1968) — synthesized vs inherited attributes.** The tier lattice's
  spine: `->` projections are synthesized (child→parent); CONTEXT-tier `&mut Ctx` mechanisms are
  inherited (parent→child). The LOCK-1 inherited-attribute verdict decides whether the CONTEXT
  tier is grammar-expressible or host-bound.
  (Knuth, "Semantics of context-free languages", Math. Systems Theory 2, 1968;
  https://link.springer.com/article/10.1007/BF01692511)
- **tree-sitter external scanners** — arbitrary C for non-regular tokens (indentation, heredocs).
  The untyped, unbounded escape. 7C replaces "write whatever C" with a `&mut Ctx`-typed signature
  + a measured floor.
  (https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html)
- **Menhir semantic actions / ANTLR actions+predicates** — typed vs untyped inline host code in
  productions. 7C bounds them behind a named signed interface so the grammar stays analyzable
  (constant-folding, type projection).
  (https://gallium.inria.fr/~fpottier/menhir/manual.html)
- **rustc/GHC effect typing** — classifying a function by the state it touches; `&mut Ctx`-vs-pure
  is the coarse two-point effect lattice driving the tape-shape decision — the granularity the
  killed string heuristic tried and failed to guess.
- **Marpa staged evaluation** — recognition events vs a separate evaluation phase; the
  comptime/runtime split the CONTEXT-tier zero-cost invariant (build-time `&mut Ctx`, erased at
  runtime) encodes, aligning with LOCK-3 staged residual.
  (http://savage.net.au/Perl-modules/html/marpa.faq/faq.html)

## 7. Named risks and counterexamples

- **R1 — host fn as name-gate in disguise (GN-5.R2, binding).** `@host parse_toml_datetime`
  passes a string-literal audit while being an overfit shim. Counter: the floor-gate's name-gate
  walk traverses the *host declaration surface*, and the totality floor demands a high grammar
  fraction — TOML datetime must be proven class-run-expressible (WALL-B idiom) before any shim is
  admitted (self-hosting law: augment BBNF, `no-workarounds-self-hosting`).
- **R2 — the tier discriminant is spoofed.** A host fn hiding mutable context in a `*mut`/global
  rather than a declared `&mut Ctx` param reads as PURE, dodging the eager trigger and corrupting
  the tape shape. Counter: the LOCK-1 signature check rejects any host body reaching parse/build
  state not named in its signature — the tier is verified against the impl, not trusted.
- **R3 — census is vacuous without parse+build.** If bbnf never parses+builds, the counter runs
  only on hand-built fleet IR and the self-host floor is unmeasured. Counter: name the parse+build
  DAG edge explicitly; 7C's self-host floor claim is BLOCKED (not merely pending) until the three
  parser gaps close — do not report a self-host totality% before bbnf round-trips.
- **R4 — killing the heuristic silently changes a shape.** Counter: verified live-dead on the
  fleet (no matching Annotation node); the delete is gated on regen bit-for-bit across
  json/css/sheets — a shape change would surface as a regen diff and block.
- **R5 — CONTEXT tier scheduled before its invariant is proven.** A `push_path` that materializes
  per byte violates zero-cost yet ships. Counter: §4's two probes are a *scheduling gate* — the
  tier enters the DAG only after `resolve_refs` round-trip and TOML `push_path` cyc/B both pass;
  a failing probe carries the tier as BLOCKED with the measured reason.

## 8. Exactly what a prototype must test to move this family

1. **The lock-free trio, regen-clean.** Delete `has_parse_time_host_decode`; add
   `ExprKind::SpanCapture` + the `parse_atom @{` branch + general-descent lowering; declare
   `resolve_refs` the CONTEXT floor term. Success = json/css/sheets regen bit-for-bit AND a
   grammar with a `@{}` leaf builds+emits a Span-projecting recognizer. No LOCK-1 touched.
2. **The two-axis counter + floor-gate.** `xtask check-totality-floor` over the built GrammarIr
   of the fleet; per-subsystem leaves×branches table with tier breakdown; committed floor.
   Success = the planted-breach test trips the gate (below-floor host leaf; grammar-name host fn)
   and the real fleet passes clean. GATED on parse+build for the bbnf self-host row.
3. **CONTEXT-tier zero-cost, both probes.** `resolve_refs` structural round-trip + no hot-loop
   residue; TOML `push_path` cyc/B non-scaling with offset-only tape held. Success = both pass →
   the tier is schedulable; either fails → tier BLOCKED with the reason.
4. **The parse+build dependency, measured not assumed.** Attempt `bbnf.bbnf` through the skinny
   emitter; record exactly which of the three gaps ({`?w`→`->`, `@{}`, `|params|`}) each construct
   cascades on. Success = the DAG edge from 7A/7B parser-totality to 7C's census is evidenced by
   named failing constructs, not presumed.

The family moves when (1) lands lock-free and (2)+(3) establish a *measured* two-axis totality
with a *named floor* and a *cost-honest* CONTEXT tier — the census made real on skinny, its
parse+build dependency and its LOCK-1 inherited-attribute hinge named, not hidden.
