# LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP · FAMILY-SPECS (pass 1, synthesis)

Synthesis of research-{A,B,C,D,priorart} (refreshed 2026-07-16 09:3x), Fable-tier judgment, folding
the pass-1 probe measurements (`PROTO-1*`, `PROTO-2*`) the earlier draft predated. One cogent spec
per family while routes stay incompatible; then the incompatibility map; then the two sharpest
prototype briefs. Elegance earns nothing — a family stalled at a missing primitive equal in
difficulty to the original problem is **BLOCKED-candidate**. All `path:line` re-verified at HEAD.

---

## 0 · SHARED SUBSTRATE REALITY (verified at HEAD — corrections the reports over-credit)

- **The 21,557-LOC self-hosting bbnf parser + its byte-fixpoint gate live in the ARCHIVED outer
  tree** (`crates/core/src/grammar/generated/bbnf.rs`, `crates/bootstrap/src/lib.rs`,
  `crates/core/tests/bbnf_bootstrap_reproducibility.rs` — `cargo xtask regen --grammar bbnf
  --output` twice, assert byte-identical). This is the tree GN-0 archives. The GREENFIELD
  (`skinny/`) inherits a **2142-LOC hand parser** (`skinny/crates/grammar/src/lib.rs:29
  parse_grammar`); `skinny/xtask/src/main.rs:39-48` dispatches json/sheets/css/real-typed only —
  **no `regen-bbnf`, no `regen-regex`, no fixpoint arm.** Reports 7A/7D that call this "~80% built"
  are crediting the archived tree; clause (i) is UNMET in the tree that ships. LOCK-7 is
  **rescue-and-port + a named build wall**, not greenfield invention.
- **RECOGNIZE ≠ BUILD — the spine finding.** `bbnf.bbnf` projects its leaves `-> Span`
  (`identifier`/`string`/`regex`/`comment`); its structural rules carry no `->`. A parser emitted
  from it *recognizes* bbnf and emits a span-tape — it does **not build `GrammarIr`** (arena
  interning, `ExprId` edges, closure β-reduction, `@import` resolution live in hand code,
  unexpressed by the grammar). Corroborated on the mirror: `parse_that::regex::generated` internally
  builds a `RegexParserEnum` but its public `parse_generated(pattern) -> bool` **discards it** — a
  self-host that recognizes, not builds. Every family's central act needs the emitted parser to
  *build* structure equivalent to `GrammarIr`, not merely accept the string.
- **MEASURED (PROTO-1 self-emit probe, request-facts emitter over the bbnf triple):** self-emit
  **succeeds**, **998 LOC** (0.466× the 2142 hand parser, **0.046×** the 21,557 legacy ghost),
  **3× byte-identical** (the fixpoint determinism precondition HOLDS on this path). **The 21k
  blow-up does NOT recur.** This retires the earlier draft's "budget is the make-or-break unknown."
- **MEASURED (the build wall, byte-exact):** the *builder* path (compiled-lowering emitter) dies at
  `BBNF-UNSUPPORTED-PROJECTION: grammar/bbnf/bbnf.bbnf@421` — the `-> Span` on
  `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span ;`. The skinny hand seed *also* rejects the same
  construct (`BBNF-PARSE: unexpected token '-'` @421/@323 — `->` after a regex leaf). Determinism of
  the failure is stable (3× identical). **So recognition + determinism are GREEN; the wall is
  precisely leaf-projection lowering (`-> Span`, `-> i64`), and it fires at the first projected
  leaf.** This is research-D's R1 ("cannot admit its own dialect, fires first") realized as a byte
  offset, and research-B's "recognizing ≠ building" made concrete.
- **`@host` parsed but unlowered:** `RuntimeConstructKind::HostCapture` →
  `"BBNF-UNSUPPORTED-HOST-CAPTURE"` (`skinny/crates/grammar/src/lib.rs:222`); the typed signature
  language exists (`host_directive` bbnf.bbnf:73, `type_name` types.bbnf), the dispatch does not.
  The string heuristic is real: `has_parse_time_host_decode` sniffs `name.contains("host") &&
  (contains("decode")||"parse")` (`passes/src/lib.rs:794-798`).
- **Clause (ii) one-home breach:** two divergent regex grammars — `grammar/misc/regex.bbnf` (27 LOC)
  vs canonical `parse-that/rust/bootstrap/regex.bbnf` (72 LOC, full L4). Collapse is regex-bootstrap
  work owed to LOCK-5.

**The gate all three vertical families share:** does `emit_from_source("bbnf", …)` produce a parser
that *reparses the corpus and builds GrammarIr*? Recognition is proven; determinism is proven; the
open primitive is a general-descent emitter that lowers bbnf's own `-> Span`/`-> T` projection +
`@{}` capture + closure + `@import` surface into **construction**, not just recognition. Until GREEN
past byte 421, the bbnf leg of 7A/7B/7D is gated on this one missing primitive.

---

## 1 · FAMILY 7A — STAGED-BOOTSTRAP

**Center.** Totality is a *closure property* proven by self-application and enforced byte-for-byte
over the *smallest provably-idempotent trust root* — not a percentage (7C), not a promotion oracle
(7D). rustc stage1≡stage2 / META-II / GCC stage2≡stage3.

**Architecture.** Committed **stage-0** `S0` (generated parser bytes); **seed** = `parse_grammar`
stripped to *bbnf-core* (the sub-language `bbnf.bbnf` is written in: `= ; | , () [] {} * + ? ?w`,
`/regex/`, `"literal"`, `@import` — quarantined, LOC-budget ratcheting DOWN); **stage-1** `P1 =
gen(S0.parse(bbnf.bbnf))`; **stage-2** `P2 = gen(P1.parse(bbnf.bbnf))`; gate `blake3(P1) ≡
blake3(P2) ≡ blake3(S0)`. Codegen emit backend NAMED A TRUSTED PRIMITIVE (7A does not
grammar-specify it — that is 7B's Plane 4). Plus a bounded differential soundness guard (parse
`grammar/**` with seed AND generated, assert `GrammarIr` equality) — a *bounded* 7D used only as the
false-fixpoint guard.

**Mechanism / seams.** Clone `JSON_TARGET` (main.rs:177) → `BBNF_TARGET`/`REGEX_TARGET`; extend
`check_target` (regen.rs) from single-application staleness to a two-application `fixpoint` arm;
promote `validate_inputs` blake3 from input-only to emitted-byte hashing; `lint_loc` (main.rs:259)
→ `trusted-seed` budget. Regex resolves as a *dependent* target (regex-as-opaque-span at bootstrap
breaks the nested cycle). Port the outer `bbnf_bootstrap_reproducibility.rs` gate into skinny.

**Gates (born-RED).** (a) ≥2-self-application byte-identity; (b) 3×-run determinism
(reproducible-builds); (c) digest-lock trips on stage-0-edit-without-regen; (d) a planted
non-determinism + stale-digest falsification proving the gate *can* fail; (e) S0 LOC ceiling. Note
(b) is **already MEASURED GREEN** on the request-facts path (3× byte-identical).

**Open questions.** (1) The fixpoint as stated needs `gen(P1.parse(…))` to round-trip `GrammarIr` —
impossible while P1 emits a span-tape; **what is the fixpoint OVER?** Generated `.rs` bytes (proves
only determinism — already GREEN) or GrammarIr equivalence (needs the builder grammar past byte
421, Proto-1)? (2) Can S0 stay minimal (~600 LOC) while *building* GrammarIr, or does "build not
recognize" inflate S0 back toward the hand parser (thesis-collapse)? (3) Skinny 10.2k codegen
determinism unaudited (HashMap iteration) beyond the request-facts happy path.

**Status: VIABLE (rescue-and-port; determinism GREEN, build-fixpoint gated on Proto-1).** Not
blocked — the primitive exists and runs in the outer tree; its *correctness meaning* in the
greenfield is empty until the builder grammar clears byte 421.

---

## 2 · FAMILY 7B — TOTAL-SELF-HOST

**Center.** Maximal coverage: four planes carried as bbnf data over one fixed meta-emitter; "the
generator becomes mostly data." The maximal claim — it lives or dies on two budgets (build-time,
debuggability) and one ratio (Plane 4).

**Architecture — four planes + a surviving hand floor.** **A** bbnf grammar (self-parse, retire the
2142-LOC hand parser); **B** regex grammar (re-home off `bootstrap-regex.sh`'s `cargo expand`+Python
hack); **C** IR grammar (`GrammarIr`/`ExprKind`/`BackendShape` as bbnf, transpositions as `->`
maps); **D** emitter templates (the 10.2k stringly-typed codegen blobs → a template grammar keyed on
IR *shape*, never grammar name). Floor = combinators, aarch64 SIMD, offset-tape runtime, the
meta-emitter. Rides 7A (stage-0) and 7D (plane-by-plane promotion, no cutover).

**Plane dispositions (measurement-updated).**
- **A (bbnf):** ADVANCE. Self-emit budget MEASURED **998 LOC, 0.046× the legacy ghost, 3×
  byte-identical** — the blow-up fear is retired. But this is the *recognizer*; parity to `GrammarIr`
  is gated on the byte-421 build wall (Proto-1). Even a good budget yields only recognition until the
  projection/construction surface lowers.
- **B (regex):** FREE-ADVANCE — already bootstrapped parse-that-side (2293 LOC generated); work is
  collapsing `grammar/misc/regex.bbnf` into the canonical source (clause ii) + a fixpoint-gated
  skinny target. Its self-host also *discards* its AST (recognize-not-build) — the same upgrade.
- **C (IR-as-grammar):** **BLOCKED-candidate.** `GrammarIr` carries interned arenas, `ExprId` graph
  edges, Rust-typed payloads. 7B's own R2 concedes: if the IR needs host fns for its own recursion,
  Plane C *is* 7C's lattice. The missing primitive — "an IR fully expressible as a CFG" — for a
  graph-shaped interned IR is **equal in difficulty to the original self-hosting problem restated at
  the IR level.** Gated behind LOCK-3C; carried as a named boundary, not attempted.
- **D (emitter templates):** **BLOCKED-candidate on the maximal claim.** Rust emission carries
  hygiene, lifetimes, type inference, borrow structure a template grammar cannot express; the
  template risks degenerating into host-fn escapes that ARE the hand code relocated. MLIR ODS/BNFC
  both bound totality exactly here. **Decided by Proto-2's data-vs-escape ratio: ≥50% data → Plane D
  viable; <50% → the maximal claim is refuted and 7C is the honest form.** The request-facts emitter
  (998-LOC deterministic) is the *gradient start*, not proof the maximal template survives.

**Gates.** build-wall cold ceiling vs hand baseline; `lint-loc` per-grammar ceiling (anti-target:
the 21,557 ghost); meta-circular GrammarIr byte-parity on every input including bbnf.bbnf; Plane-D
codegen-LOC-drop with generated output bit-for-bit unchanged (`check-css`).

**Status: MIXED.** Planes A/B VIABLE (A gated on Proto-1's build wall, B free). Planes C and D are
**BLOCKED-candidate** — each rests on a primitive (CFG-expressible interned IR; template-grammar for
borrow-checked Rust) plausibly as hard as the original problem. 7B's *distinctive maximal claim*
therefore hangs on Proto-2; below the ratio it collapses into 7A (Planes A/B) + 7C (bounded floor).

---

## 3 · FAMILY 7C — HOST-FUNCTION LATTICE

**Center.** Totality is a *measured percentage* bounded below by a **typed host-fn interface**, with
a named floor per subsystem defended by CI and an escape hatch that cannot masquerade as a
grammar-name gate. The relief valve for recognize-vs-build.

**Architecture.** Three strata: GRAMMAR (`->`, `@{}`, class-run scans) · PRIMITIVES/COMBINATORS (the
exempt floor: pure decode — Eisel-Lemire, unescape, SIMD — a closed, declared, checkasm-gated
`PrimitiveFloor`) · HOST FUNCTIONS (the only legal home for context-dependent/recursive-with-state
semantics, behind `@host name(InTypes) : OutType @purity`). Unify the *outer* `@host` declaration
(today `HostFnDecl{name, return_type}`, a name→type map) with the already-typed *inner* lattice
`FnDescriptor` (six variants: `EnumWrap`/`BoxWrap`/`NumberConvert`/`HexConvert`/`SpanCapture`/`Expr`)
— `@{}` span-capture is the `pure : Span` corner. The `purity ∈ {pure, context, comptime}` axis is
the cost discriminant, read *structurally* (a lookup) not by substring.

**Mechanism / seams.** Extend `@host` (bbnf.bbnf:73) to a full typed signature flowing into LOCK-1
inference; **delete** the string heuristic (passes/src/lib.rs:794-798), replace with a registry
`context_of(HostCall)` lookup driving the tape shape (`ir/src/cost.rs:255`
`ShapeRationale::HostFnParseTime`); **lower `RuntimeConstructKind::HostCapture`** so `@{}`
(`colorFunction`/`urlFunction`) stops emitting `BBNF-UNSUPPORTED`; `host_map`
`HashMap<String,Option<String>>` → `HashMap<String,HostFnSig>` with backend-agnostic `TypeDesc`; a
`crates/ir/host/` directory-module for `HostSig`/`HostRegistry`.

**Gates (born-RED).** (a) a PURE-DECODE `@host` type-checks and the zero-cost payload-no-write
assertion holds; (b) `check-json` bit-for-bit after killing the heuristic (JSON floor = 0); (c) one
real CONTEXT host fn (TOML `push_path`) round-trips against the `toml` crate; (d) a totality-floor
gate (sibling of `check-name-gates`) fails on floor regression OR a grammar-name-specific host
identifier (`parse_toml_datetime` = the master pathology); (e) purity *verified* by LOCK-1 (a `pure`
sig taking `&mut Ctx` is a type error, not trusted). `xtask census-totality` is the measured
deliverable (per-subsystem grammar-vs-host table + purity breakdown + committed floor).

**Why it advances.** The relief valve: the exact `-> Span`→GrammarIr *construction* steps that
resist `->` expression get a typed, counted, floored home instead of dissolving totality into opaque
host blocks (the yacc/PEG.js anti-pattern). Genuinely orthogonal — governs the horizontal
leaf/context boundary, not the vertical bootstrap ladder; its in-tree win (delete one heuristic,
light one dormant directive) needs no self-emit and yields signal regardless of Proto-1.

**Status: VIABLE (lowest-risk in-tree win).** No missing primitive — all extends existing typed
surface. Load-bearing on LOCK-1 for purity verification (named dependency, not a block). Its bbnf
floor — how many CONTEXT host fns bbnf's own *construction* needs — is exactly Proto-2, and sets
whether 7B Plane A reaches pure-grammar parity (floor = 0) or is 7C-bound (floor > 0).

---

## 4 · FAMILY 7D — META-CIRCULAR ORACLE

**Center.** Self-hosting as *verification first, production later*: emitted bbnf/regex parsers run
*beside* the hand parsers as differential **oracles**; promotion is subsystem-by-subsystem on
measured AST-parity (against a hand-audited golden anchor, never majority vote) + measured perf.
Cures the clause (viii)/Y.13 dormant-substrate violation the instant a parser is emitted (its
consumer is the oracle). Matches the field's strongest precedent — CPython pegen ran old ∥ new for a
full release cycle before promotion.

**Architecture / seams.** A `self_host/` submodule under the GN-5.W1 `bbnf-oracle` directory-module
(`self_host/{bbnf,regex,normal_ast}.rs`); a `check-self-host` xtask arm reusing
`write_targets`/`check_target`; golden fixtures `test-fixtures/corpus/{bbnf,regex}/golden/*.sexp`; a
planted-mutation falsification (mirror `checkasm_parity`'s `INJECT_BUG` + strict/loose switch).
NormalAst = the least-normalizing S-expr preserving rule identity, precedence nesting, modifiers,
`->` expressions, and **spans** (over-canonicalization is the named risk). Regex anchors additionally
on `regex-syntax::Hir` (a third independent implementation). Corpus = `grammar/**/*.bbnf` (zero
fabrication) + harvested `/…/` literals. Metric ladder L0(accept/reject)→L1(node histogram)→
L2(NormalAst equality)→L3(byte-identical emitted parser)→L4(regen∘regen = 7A); promotion names the
rung, never below L2.

**Gates.** admitted% · parity% · cyc/B ratio; promotion = zero unadjudicated divergences two regen
cycles; the artifact is the NormalAst diff, not a grep.

**Open questions (measurement-updated).** (1) **The NormalAst crux collides with recognize≠build:**
the generated side yields a span-tape, so the differential compares at *recognition/span
granularity* only (a weak oracle, missing wrong-precedence/wrong-construction) unless the
builder-grammar exists — the same byte-421 upgrade (Proto-1). (2) R1 (dialect-rejection) is now
EMPIRICALLY the first product: the generated grammar under-specifies at byte 421 (`-> Span`) — the
augment-BBNF task list starts there. (3) R4: the perf gate is vacuous for cold compile-time bbnf
parsing — the honest prize is **LOC deletion** (retire 2142 hand LOC), not speed; teeth exist only
for runtime-hot regex leaves (PROVE-10 floor).

**Status: VIABLE (the lawful entry ramp).** No missing primitive; its first output is a falsifiable
coverage number — already measured to fail at byte 421, which is the oracle working as designed.

---

## 5 · INCOMPATIBILITY MAP

The prior-art "they compose as a DAG (7A→7D→7C→7B)" is largely right of the *mechanisms*. The
genuine incompatibilities are of *centers* — localized to two decision axes a single lock cannot
straddle.

**INC-1 — Promotion trajectory + proof epistemics: 7A (big-bang fixpoint) ⊥ 7D (incremental
oracle).** Same seam (`parse_grammar := P1`), different decision procedures and different proofs. 7A
flips the whole front-end when `bytes(P1) ≡ bytes(P2)` — a *determinism* proof, big-bang; 7D holds
the hand parser as oracle and promotes construct-by-construct on *differential parity* — a
*correctness* proof, incremental. 7D's own §1 refutes 7A's center: "a parser that agrees with itself
proves nothing — the fixpoint certifies the generator is deterministic, not that either parser is
correct." They cannot both be the promotion rule and both be LOCK-7's headline. **Reconciliation:**
the two *gates* compose (require fixpoint ∧ differential — 7A's §4 guard is a bounded 7D; 7D's L4
rung is 7A), but the *trajectory/headline* must be chosen. Evidence tilts 7D-trajectory (CPython
oracle-first) with 7A's fixpoint as a determinism *precondition* (already GREEN), not the trigger.

**INC-2 — Totality reach at the build boundary: 7B (IR/build in grammar) ⊥ 7C (build behind typed
host fns) — THE SHARPEST GENUINE CONFLICT, and it has NO measurement that dissolves it, only one
that *locates the boundary*.** bbnf's `GrammarIr` *construction* (arena interning, `ExprId` edges,
closure β-reduction) is either `->`-expressible (7B: floor = 0, pure grammar) or recursive-with-state
and host-fn-bound (7C: floor > 0). Both cannot be true. This is recognize-vs-build made a boundary
dispute — decidable *per construction step* (Proto-2). 7B Plane C is BLOCKED-candidate precisely
because the interned graph-IR is where this conflict is worst. Whoever wins sets where clause (i)'s
"save primitives" line falls at the IR level.

**INC-3 — The codegen backend's disposition: 7A (trusted primitive) ⊥ 7B Plane D (grammar data) —
HARD, no measurement dissolves it.** 7A NAMES the 10.2k-LOC emit backend a trusted primitive drawn
OUTSIDE grammar-specification (that boundary IS 7A's minimality thesis). 7B Plane D declares the same
mass a template grammar (that inclusion IS 7B's maximality thesis). One artifact cannot be both the
trusted floor and the top plane. Unlike INC-1 this is not "run both gates"; Proto-2's ratio only
tells you *whether Plane D is even feasible* — the disposition (primitive vs plane) is an
architectural fork the next pass must decide.

**INC-4 — Hand-parser fate: 7B (retire on promotion) vs 7D (permanent CI oracle).** *Not hard* —
composable: retire from the *production* path (7B) while keeping it as the *CI-differential*
reference (7D). A named supersession, not an exclusion.

**Cross-cutting bound (retired by measurement):** the earlier draft's "budget blow-up bounds three
families" is DISCHARGED — self-emit measured 998 LOC / 0.046× the ghost / 3× identical. The residual
cross-cutting risk is **debuggability** (an un-steppable generated front-end is a negative result for
7A/7B/7D promotion) — folded into 7D's promotion gate, not a standalone blocker.

**Net.** No family is wholly excluded. The lock resolves to one composed route
(7A-determinism-floor ∧ 7D-oracle-trajectory ∧ 7C-typed-boundary → 7B-totality-per-plane) with
**three** open adjudications the next pass must settle: INC-1 (trajectory/headline — a framing
choice), INC-2 (build boundary — located by Proto-2), INC-3 (codegen disposition — an architectural
fork). 7B Planes C and D stand as BLOCKED-candidate pending LOCK-3C and Proto-2 respectively.

---

## 6 · TWO SHARPEST UNCERTAINTIES → PROTOTYPE BRIEFS

Pivoted forward from the measured pass-1 results. The budget make-or-break is *resolved* (998 LOC,
3× identical); the two live uncertainties that most advance the next pass are: **(1)** whether the
byte-421 *build wall* clears with a bounded projection-lowering augment while preserving the
fixpoint — the gate that decides 7A build-meaning, 7B Plane A parity, and 7D oracle strength jointly;
and **(2)** the construction census / INC-2 adjudication that sets 7C's bbnf floor and decides
whether 7B Plane A reaches pure-grammar parity. Full briefs in the structured return —
`bbnf-worktree` (build-wall close + fixpoint) and `spec-probe` (construction census).
