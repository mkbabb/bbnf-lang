# LOCK-7 pass-1 — ADVERSARIAL CRITIC 1 (WITHIN-LOCK view): internal soundness + prototype sharpness

Critic 1 of 2. Weighting: internal soundness of each family spec and whether the prototypes
actually tested the sharpest uncertainty. Every load-bearing claim re-run on disk at HEAD
(`d2b32f6e3`); where I contradict the record I did so with a compiled, reproducible probe, not a
re-reading. Test artifacts written to run these were removed; nothing left in the tree.

---

## 0. The headline: the lock's single make-or-break gate is measured WRONG

FAMILY-SPECS §0 makes 7A/7B/7D "three postures on ONE bootstrap ladder" sharing "ONE make-or-break
gate … the identical missing primitive." PROTO-1 was the probe of that gate. **PROTO-1 does not
reproduce at HEAD, its stated mechanism is contradicted by the source it ran against, and the §0
governing finding mis-scopes the missing primitive by an order of magnitude.** This is the central
within-lock defect and it contaminates the incompatibility map, 7A's whole feasibility question,
7B Gap A, 7C's T6, and 7D's bbnf edge simultaneously.

### F1 — PROTO-1's numbers do not reproduce; its mechanism is source-false

I recompiled `grammar::parse_grammar` at HEAD and re-ran PROTO-1's exact three cases plus a
seven-case primitive-pinning probe (`ax-iter`, isolated `CARGO_TARGET_DIR`):

| case | PROTO-1 claim | HEAD measured |
|---|---|---|
| `bbnf.bbnf` verbatim | REJECT @ byte **421** (line 9, `identifier … -> Span`) | REJECT @ byte **662** (line 17, `big_comment = ( "/*" … "*/" ) ?w -> Span`) |
| `bbnf.bbnf` imports-stripped | REJECT @ byte **323** (line 9, `->`) | REJECT @ byte **564** (line 15, same `?w ->`) |
| `expressions.bbnf` standalone | REJECT @ byte **115** (`-> i64`) | **PARSES** — fails only `unresolved rule reference type_name` (a cross-file `resolve_refs`, not a parse wall) |
| `types.bbnf` | PARSE OK | PARSE OK |

Primitive-pin (fresh probe): `x = "a" -> Span` **OK**; `x = ( "a" ) -> Span` **OK**;
`x = "a" ?w -> Span` **REJECT** `-`; `x = ( "a" ) ?w -> Span` **REJECT** `-`;
`x = "a" ?w "b"` **OK**; `x = @{ "a" }` **REJECT** `@`; `x = |p| p` mis-parses `p` as a rule ref.

PROTO-1's verdict rests on the assertion *"There is no `->` token anywhere in the RD parser, so
the arrow's `-` falls to the catch-all."* This is flatly false at HEAD: `parse_postfix`
(`grammar/src/lib.rs:1409`) tests `starts_with("->")` and dispatches `parse_projection` (`:1481`)
into `ExprKind::Project`; the handler was added by SK-V18 commit `9eb2aca1f7` on **2026-06-03**,
predating the probe worktree. `?w` is handled in `parse_seq` (`:1366`) and `parse_postfix`
(`:1449`); `<<`/`>>` at `:1414`. The parser is **not** "only the untyped JSON subset"; it parses
`->`, `?w`, groups, and plain projections. PROTO-1's own probe-C claim that `-> i64` rejects is
refuted by expressions.bbnf parsing cleanly.

### F1-consequence — the real missing primitive is THREE narrow fixes, not a rewrite

The base case is genuinely RED (bbnf.bbnf does reject → the directional conclusion "the bbnf leg
needs parser extension" survives). But the wall is precise and small:

1. **`->` projection immediately after a standalone `?w` sequence element** (the *first* wall,
   bbnf.bbnf line 17). Root cause: `parse_seq`'s `?w` branch (`:1366`) pushes a ws annotation and
   `continue`s, so the trailing `->` is routed to `parse_postfix`→`parse_atom` as a *new* element
   rather than attached as a projection over the accumulated sequence. This is a one-branch
   attachment bug, not a missing feature.
2. **`@{ }` host-capture atom** — genuinely unhandled in `parse_atom` (`:1498` has no `@` arm; the
   `@` is only a directive prefix in `parse_rules`). PIN6 confirms a hard parse reject, distinct
   from the runtime `BBNF-UNSUPPORTED-HOST-CAPTURE` classification 7C cites.
3. **`|params|` closures** — mis-parsed (params silently treated as refs), a correctness gap.

§0 frames the primitive as *"a general-descent emitter that lowers bbnf's own closure/`@import`/
`@{}`/recursive-alternation surface"* and PROTO-1 re-scopes it to *"grow the front-end … cannot
parse `->`, `?w`, `@{}`, or `|params|`."* Both are wrong about `->`/`?w`. The honest scope is:
fix one `parse_seq` attachment bug, add one `parse_atom` arm, fix closure param binding — then the
"emitter lowering" question §0 called *unreached-by-construction* becomes askable. Whether this is
even a BLOCKED-candidate (a primitive "equal in difficulty to the original problem," per the
CONSTITUTION parsimony test) is now in serious doubt; it looks like ordinary bounded parser work.

### F2 — §0 fact 4 is factually wrong (PROTO-2 caught it; I verified)

§0 fact 4: *"the public `parse_generated(pattern) -> bool` **discards** [the AST]"* — framing the
regex edge as recognizer-only. Verified false: `parse-that/rust/parse_that/src/regex/mod.rs:34`
is `pub fn parse_generated(pattern) -> Result<Hir, ParseError>`, a public producer via
`host::lower`. `generated.rs:2288`'s bool is a *second* entry, not *the* public path. 7D's
G-7D-3 "recognizer→producer gap" is **already closed**. Two of §0's four "governing facts" (fact
4, and the base-case scope inside the finding) are wrong despite the header's assurance that all
were *"confirmed by inspection, not quotation."* The §0 ground-truth that anchors the whole
incompatibility map is not trustworthy at the resolution the map claims.

### F4 — the evidence is inverted: the load-bearing gate has the broken prototype

Only **7D's regex edge (PROTO-2)** carries a reproducing, well-caveated measured result. The
**shared §0 gate** — on which 7A, 7B Gap A, 7C T6, and 7D's bbnf edge all rest — carries the
prototype that does not reproduce. The prototypes tested the sharpest uncertainty for 7D but
delivered broken evidence for the one gate every ladder posture shares. Per the CONSTITUTION
concrete-artifacts clause, a non-reproducing running probe is worse than SPEC_ONLY: it launders
false numbers as measured fact into §0 and the incompatibility map.

---

## 1. Family 7A — STAGED-BOOTSTRAP — convergence 45%

**Sound.** Rung-1 staleness reuses `regen.rs:37 check_target` verbatim — trivially ships. The
mechanism shape (rustc `S₁==S₂` fixed-point, not `S₂==S₀`) is correctly specified (R1). Seed/
provenance-ledger design is coherent.

**Defects (within-lock).**
- **G-7A-1 rests on the broken PROTO-1.** 7A's own §6 test 1 ("does `emit_from_source(bbnf)`
  parse ≡ hand?") is the go/no-go, and the one probe of it is unreliable (F1). 7A's feasibility is
  *less* blocked than stated (three narrow fixes) but *unproven* — the family cannot claim either
  disposition on current evidence.
- **Rung-1 is a gate that cannot distinguish self-hosting** (the "gates that cannot fail" failure
  mode). §0/7A both concede Rung 1 == json's existing check; 7A's entire distinguishing value is
  Rung 2, which is 100% gated on §0. So the *shippable* part of 7A is vacuous-adjacent and the
  *non-vacuous* part is unproven.
- **Build-budget reference is a cross-tree transplant** (F5): 7A/7B cite the legacy `crates/`
  "~88s→~0.5s B6.W0" datum, but skinny's xtask has **zero** self-host targets (verified: grep
  `regen-bbnf|regen-regex|fixpoint|BbnfBootstrap` in `skinny/xtask/src/main.rs` = 0). A legacy-tree
  number is not a greenfield ceiling.

**Enumerated open gaps.** (a) §0 base case unproven + mis-scoped (F1) — 7A cannot mint a
self-reparsing S₀ until the three parser fixes land and the emitter is then shown to lower.
(b) Q1 seed disposition (frozen-forever vs retire-after-fixpoint) unresolved. (c) Q2 twin-ladder
(one `fixpoint` xtask over bbnf+regex, or two) unresolved — bears on the "minimal surface" metric.
(d) Q3 regex promote-or-oracle collapses into 7D given PROTO-2 (see §4). (e) build-budget ceiling
has no valid skinny datum. (f) seed_LOC headline metric unmeasured.

---

## 2. Family 7B — TOTAL-SELF-HOST — convergence 35%

**Sound.** The data/logic boundary (Gap B: generate IR *definitions* + serde + visitor; hand-write
passes) is the correct ODS/TableGen discipline and needs no runtime parser — a genuine existence
proof. Gap C's honest self-assessment as BLOCKED-candidate-absent-pure-structure-scoping is candid.

**Defects (within-lock).**
- **Gap A shares the mis-scoped §0 wall (F1)** and is the family's load-bearing "promote to
  production" move; unproven either way.
- **Gap B has zero in-tree prototype** — it rests on external prior art (ODS/IRDL/tree-sitter),
  acceptable as SPEC_ONLY but must be *labeled* so; the family text presents it as "feasible" with
  the confidence of a run that never happened. The claimed diff-gate ("generated type surface
  byte-identical to a frozen snapshot; passes/codegen compile unchanged") is untested.
- **Gap C is an unresolved BLOCKED-candidate** — a template DSL emitting *correct specialized*
  Rust needs the type/borrow discipline it replaces (the R3 category error). No prototype scoped
  it; the "if pure-structure holds it degrades to feasible" is a conditional, not a result.
- **INC-1 (7A⊗7B) and INC-2 (7B⊗7D) are real and unadjudicated** — and INC-2 is now *decided
  against 7B* on the one measurable edge (PROTO-2, §4). 7B's own R1 concession ("if diagnostics
  unmet, Gap A demotes to oracle") is 7D; the family collapses toward 7D under measured pressure
  and the spec admits it without resolving it.

**Enumerated open gaps.** (a) Gap A blocked on §0 (F1). (b) Gap B unprototyped — the byte-identical
IR-surface diff-gate is asserted, not run. (c) Gap C BLOCKED-candidate — no pure-structure-scoping
prototype; the kernel-escape-hatch-into-control-flow failure signature untested. (d) Q1 diagnostics
parity (generated parser emits spans as richly as 2142 LOC hand machinery?) unmeasured. (e) Q3
regex-`->`-to-HIR is LOCK-5-dependent and, per PROTO-2, the current `host::lower` rejects all
bounded quantifiers `{n,m}` — the regex projection is materially broken, not merely "recogniser".
(f) build-budget datum is a legacy transplant (F5).

---

## 3. Family 7C — HOST-FUNCTION-LATTICE — convergence 60%

**Sound and best-substantiated in-tree.** Every substrate claim verified on disk: the
`passes/src/lib.rs:798` string heuristic (`name.contains("host") && (value.contains("decode") ||
value.contains("parse"))`) exists verbatim; `bbnf.bbnf:73` typed `host_directive` exists and is
zero-usage-dormant; `bbnf.bbnf:33` `@{}` exists; `grammar/src/lib.rs:222`
`BBNF-UNSUPPORTED-HOST-CAPTURE` exists; `json/host.rs` is empty ("JSON is host-fn-free in the
skinny compiler slice"). T1 (typed signature) and T2 (delete the heuristic, `check-json`
bit-for-bit) are genuinely orthogonal, in-tree, and shippable without any parser work. The
attribute-grammar framing (`&mut Ctx` = inherited attribute) is exact. This is the lock's most
independently-viable family — the assessment is directionally right.

**Defects (within-lock).**
- **"Fully orthogonal / NOT blocked" is overstated.** T6 ("the typed `@host` directive survives
  `regen∘regen` byte-identical on `bbnf.bbnf`") is **unachievable while bbnf.bbnf does not parse**
  (F1) — T6 shares the §0 wall. The bbnf-subsystem totality% floor (bbnf `@import` + closure
  β-reduction) likewise cannot be *counted* until the self-grammar parses. So 7C is orthogonal for
  T1/T2/T3/T5 but **not** for T4/T6, contra INC-4's blanket "orthogonal to all three."
- **T4 understates the work.** PIN6 shows `@{}` is a **hard parse reject** in the `parse_grammar`
  RD path (`@` at `parse_atom`), not merely a runtime `BBNF-UNSUPPORTED` classification. "Lower
  `RuntimeConstructKind::HostCapture`" is necessary but not sufficient; the RD front-end must also
  *parse* `@{}`, tying T4 to F1's item 2.
- **Zero prototype evidence.** Neither PROTO-1 nor PROTO-2 exercised 7C. Its "most
  independently-viable" and "one-file cure" claims are SPEC_ONLY. For a family this cheap to probe
  (delete one heuristic, run `check-json`), the absence of a T2 prototype is a gap, not a virtue.
- **CONTEXT-tier zero-cost (Q2) unverified.** The `&mut Ctx` parse-time-only invariant
  (`runtime/src/lib.rs:42-44` no-write assertion) is asserted-per-subsystem, run for none.

**Enumerated open gaps.** (a) T6 + bbnf-floor counting share the §0 wall — 7C is not
unconditionally orthogonal. (b) T4 requires `@{}` *parsing* (F1-2) in addition to lowering. (c) no
prototype for T1/T2/T5 despite trivial cost — SPEC_ONLY. (d) Q2 CONTEXT zero-cost unverified for
any real host fn (TOML `push_path` is designed, not run). (e) totality% counter (leaves × branches)
is specified but unbuilt; the published per-subsystem numbers are all projected, none measured.

---

## 4. Family 7D — META-CIRCULAR-ORACLE — convergence 62%

**Sound and the only family with a reproducing measured result.** PROTO-2 ran a real paired cold
oracle over 51 patterns: structural parity 0.7647, membership 0.8235, perf GEN/HAND 1.77–1.81×
slower — **neither promotion gate fires → ORACLE-ONLY**, resolving INC-2 toward 7D on the one edge
with committed producer code both sides. The ≡-sensitivity finding (`Empty ≡ Concat([])`
normalization) concretely answers 7D-Q1. The 12-row divergence ledger (9× `{n,m}` reject, `.`
metachar, `(?s)` inline flag) is exactly the "checkable spec of the hand parser" the family
promises — value delivered independent of any cutover, as claimed. The §0-fact-4 correction (F2)
is 7D's own researcher's catch and is verified true. ORACLE-ONLY as a lawful terminal is
well-argued and Y.13-compliant (the oracle is CI-consumed).

**Defects (within-lock).**
- **The bbnf edge shares the mis-scoped §0 wall (F1).** 7D's own §5 test 1 (base-case feasibility)
  is the PROTO-1 that doesn't reproduce; the bbnf ledger row cannot be built on current evidence,
  and the "reaches ORACLE-ONLY the instant the base case lowers" claim inherits F1's uncertainty.
- **Regex parity 0.76 is LOW and the substrate is materially broken**, not "half-self-hosted".
  `host::lower` rejecting *every* bounded quantifier is a large hole; PROTO-2 honestly flags that
  grammar-fuzzing (G-7D-4) would only *lower* parity. This does not undermine the ORACLE-ONLY
  verdict but it means the promotion ledger for regex is far from parity, and the "regex edge
  ships now" framing should read "ships now as a red ledger row."
- **Q3 two-regex-home is LOCK-5-dependent and unresolved** — the oracle's canonical `.bbnf` is
  undefined (three divergent regex grammars exist per prior-art §1.2). Named, not closed.

**Enumerated open gaps.** (a) bbnf edge blocked on §0 (F1) — no bbnf ledger row. (b) regex ledger
row is red (parity 0.76, perf 0.55×); promotion may never fire (Q4, honestly named). (c) ≡ needs
`Empty`-normalization codified before parity% is quotable (from PROTO-2). (d) Q3 canonical regex
`.bbnf` awaits LOCK-5. (e) grammar-driven corpus sampler (test 4) unbuilt — parity is over the
committed 51 only, not saturated.

---

## 5. The incompatibility map — directionally right, but resting on a mis-measured §0

**INC-2 is now decided by measurement**: PROTO-2 resolves 7B-promote vs 7D-oracle **toward 7D** on
the one real edge. The map should elevate this from "convergence note" to "measured resolution."
**INC-1** (7A⊗7B scope contradiction over `ir/src/lib.rs`) is sound. **INC-3/INC-4** are sound in
shape but INC-4's "7C orthogonal to all three" is **false for T4/T6** (F3). The map's foundational
frame — "three postures on ONE ladder sharing the identical missing primitive" — is *directionally*
true (all three bbnf legs hit the same base case) but *false in detail*: the "identical missing
primitive" is three specific, small parser fixes (F1), not a general-descent emitter rewrite, and
once they land the wall may fall cheaply enough that "BLOCKED-candidate" no longer applies. The
lawful-gestalt-shape recommendation (7C ships regardless + one chosen ladder posture, 7D lowest-
risk) survives all findings and is the correct pass-2 spine — but pass 2 must **re-measure the base
case** before repeating §0's numbers.

---

## 6. Verdict summary (critic-1, within-lock)

| family | conv % | disposition | the one gap that dominates |
|---|---|---|---|
| 7A | 45% | ADVANCE-with-caveat | §0 base case unproven + PROTO-1 non-reproducing; Rung-1 vacuous-adjacent |
| 7B | 35% | ADVANCE-with-caveat | Gap C BLOCKED-candidate unresolved; Gap A on §0; Gap B unprototyped; INC-2 decided against it |
| 7C | 60% | ADVANCE | strongest in-tree, but SPEC_ONLY (no probe) and not orthogonal for T4/T6 |
| 7D | 62% | ADVANCE | only reproducing measured result (regex ORACLE-ONLY); bbnf edge on §0 |

**Mandatory pass-2 corrections before any convergence talk.** (1) Re-run the base-case probe at
HEAD and replace PROTO-1's numbers (662/564, not 421/323) and mechanism (three named fixes, not
"no `->` token"). (2) Correct §0 fact 4 (regex producer at `mod.rs:34` exists; recognizer→producer
gap closed). (3) Down-rate the base-case from "primitive equal in difficulty to the original
problem" to "bounded parser work," and re-test whether 7A/7B-A/7D-bbnf are BLOCKED-candidate at
all after the three fixes. (4) Prototype 7C's T2 (trivial) so its "most viable" claim stops being
SPEC_ONLY. (5) Label the ~88s build-budget datum as legacy-tree, not a skinny ceiling.
