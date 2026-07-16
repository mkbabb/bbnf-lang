# LOCK-7 pass-1 — ADVERSARIAL CRITIC 1 (WITHIN-LOCK view): internal soundness + prototype sharpness

Critic 1 of 2. Weighting per charter: internal soundness of each family spec and whether the two
prototypes tested the *sharpest* uncertainty. Every load-bearing claim re-verified on disk at skinny
HEAD `8be944d8f4`; the one contested fact (the hand-parser wall) was settled by a compiled,
reproducible probe (`parse_grammar` over three inputs, `ax-iter`, isolated `CARGO_TARGET_DIR`),
written to `tests/`, run, and removed — the tree is clean. This critique reads the **04:51
FAMILY-SPECS** (RECOGNIZE≠BUILD thesis) against the **05:01 prototypes** (PROTO-1-7B self-emit,
PROTO-2 construction census), which are the newest artifacts and post-date the prior CRIT/REGISTRY
round.

---

## 0. HEADLINE — the make-or-break was CIRCLED by both prototypes and LANDED by neither

FAMILY-SPECS has correctly evolved past the byte-421 parse-wall confusion to a sharper thesis:
**RECOGNIZE ≠ BUILD** (§0.5) — bbnf.bbnf projects its leaves `-> Span` and its structural rules
carry no `->`, so a parser *emitted from it* recognizes bbnf and emits a span-tape; it does not
*build* `GrammarIr`. This is the genuine lock pivot and it is more defensible than "the parser can't
parse `->`." The two prototypes were dispatched to resolve it. **My finding: they advanced the
frontier substantially but the single decisive experiment — author builder `->` projections on
bbnf's structural rules, emit a parser, and show it reconstructs `GrammarIr` — remains UNRUN. Both
prototypes stopped one step short of it, and one re-imports a corrected error.**

### Disk-verified facts (compiled, not quoted)

**FV-1 — there are TWO `->` ingestion paths; the hand parser handles `->`, the runtime-facts scanner
rejects it. PROTO-1-7B's hand-parser cross-check row is source-false.**
At HEAD, `grammar/src/lib.rs` has two distinct arrow handlers:
- **Hand RD parser** `parse_grammar` (`:29`) → `parse_postfix` (`:1409` `starts_with("->")`) →
  `parse_projection` (`:1481`) → `ExprKind::Project`. **My probe: `parse_grammar` on
  `identifier = /…/ -> Span ;` returns `PARSE_OK`.** It parses `->` at byte 421 cleanly and walls at
  **byte 662** — the first `?w` *immediately followed by* `->` in `big_comment` (line 17). This
  reproduces the REGISTRY's fact-3 exactly.
- **Runtime-facts scanner** `scan_runtime_source` (`:355`) → `:492` `rest.starts_with("->")` →
  classifies `BBNF-UNSUPPORTED-PROJECTION` (inline test `w5a_…` at `:2140` pins the offset to the
  first `->`). This is the path `parse_runtime_source_facts`/`emit_runtime_from_request` use.

PROTO-1-7B's **primary** result — the *generator* path walling at byte 421
(`BBNF-UNSUPPORTED-PROJECTION`) — is CORRECT and reproduces (it is the runtime-facts scanner). But
PROTO-1-7B's cross-check table (its lines 102–106) asserts the **t1 hand `parse_grammar`** *also*
walls at 421 because "`parse_atom`/`parse_postfix` have no `->` production" and emits "unexpected
token `-`". **This is false at HEAD and refuted by my execution** (`PARSE_OK` on the identifier
line). The newest artifact in the lock re-imports the byte-421 hand-parser error that CRIT-1/REGISTRY
already corrected — a regression in the record. *Weight:* the generator finding stands; only the
hand-parser cross-check row is wrong; but it re-poisons the "both paths wall at 421" gestalt.

**FV-2 — there is NO grammar-generic descent emitter in skinny. This is sharper than the brief's
make-or-break and it is the real one.** PROTO-1-7B's decisive (and honestly self-flagged) finding:
the only emit that returns `Ok` on bbnf produces a **byte-identical CSS-declaration-values recognizer
template** — `runtime_generator.rs` emits fixed const strings (`JSON_PARSE_ONLY_PARSER_RS:339`,
`CSS_PARSER_RS:401`, `CSS_SINK_RS:505`, …) and stamps only config-level facts; **the grammar's
productions are never read for the recognizer body.** Verified: those consts exist; the RequestFacts
emitter would emit the identical CSS recognizer for *any* input grammar. So the "998-LOC Ok / budget
PASS" is a phantom — a mislabeled CSS copy, correctly rejected by the proto itself. **Consequence
the FAMILY-SPECS understates:** the bbnf leg of 7A/7B/7D is blocked not on "a `->` lowering rule" nor
even "three parser fixes," but on *the non-existence of a general-descent emitter* — a substantial
architectural build. This revives, on measured evidence, the very BLOCKED-candidate framing the prior
REGISTRY down-rated to "bounded parser work." The parse layer is bounded; the **emit** layer is
architecturally absent.

**FV-3 — OPEN-1's exact scoped path was NOT tested.** OPEN-1 (REGISTRY) asks whether
`emit_from_source("bbnf", …)` / `passes::compile` lowers bbnf to a *working* parser. PROTO-1-7B ran
`emit_runtime_from_request` (the two runtime-*template* emitters) — **not** `emit_from_source`
(codegen `:101`) nor `passes::compile(&GrammarIr)` (passes `:32`, the leaf-inlining IR optimizer,
grammar-generic by signature). The negative "no general emitter" is *proven* for the two
runtime-template paths and *inferred* (not proven) for the structural `passes::compile` path. The
sharpest residual uncertainty — is `passes::compile`+structural codegen a general descent emitter or
also template-bound? — is still open. The prototype tested a *nearby* path and generalized.

**FV-4 — PROTO-2's floor-0 verdict is an INFERENCE from the hand parser's SHAPE, not a demonstration
that bbnf `->` expresses construction (elegant-reduction trap).** PROTO-2's probe
(`proto2_construction_census`, 3 tests) genuinely establishes three properties of the **hand**
parser: no structural interner (identical leaves → distinct ExprIds), post-order-synthesized ids,
and cross-rule linking as a separable global pass. From these it *infers* "construction is a
catamorphism ⇒ `->`-expressible ⇒ floor = 0." But the load-bearing step — that bbnf's **actual `->`
projection language** can name a variant constructor (`Seq`/`Alt`/…), collect child results into an
id-list, return an `ExprId` recursively, and thread the append-only arena — is **asserted, not run.**
No builder grammar was emitted; no emitted parser was shown to reconstruct `GrammarIr`. So the RECOGNIZE≠BUILD
gap that §0 raises is "resolved" by PROTO-2 only at the level of *the target shape is catamorphic*,
not *bbnf `->` expresses it*. The "and-then-the-hard-part" (a working builder grammar) is exactly the
unconverged step.

**Net §0:** the two prototypes did real, non-trivial adversarial work — PROTO-1-7B inverted the
expected result (the emitter is absent, not one-rule-short), PROTO-2 dismantled the "interned graph
IR" fear for the construction arena. Both are legitimately RAN_MEASURED; neither is SPEC_ONLY. **But
the one experiment that would resolve the lock — a bbnf builder grammar, emitted, reparsing its own
corpus into equivalent `GrammarIr` — was run by neither.** That single un-run experiment is the lock's
true convergence bottleneck, and it should be pass-2's OPEN-1 with FV-2/FV-3/FV-4 folded in.

---

## 1. Family 7A — STAGED-BOOTSTRAP — convergence 42%

**Sound.** Rung-1 staleness reuses `regen.rs:37 check_target` verbatim; the fixpoint *mechanism*
shape (rustc `S₁≡S₂`, not `S₂≡S₀`) is correct; the blake3 provenance-ledger design coheres.
FAMILY-SPECS honestly concedes the fixpoint "is not achievable at HEAD" (§1 open-Q1).

**Defects (within-lock).**
- **G-7A-1 · the fixpoint is VACUOUS on the only artifact it can currently observe.** §1 asks the
  right question — is the fixpoint over generated `.rs` bytes (proves only determinism) or over
  `GrammarIr` equivalence (needs the builder grammar)? — but does not answer it. On FV-2, the ONLY
  emittable artifact is the CSS phantom; a byte-identical `regen∘regen` over *that* certifies the
  determinism of a template copy, i.e. **a gate that cannot distinguish self-hosting from a fixed
  string.** The distinguishing rung (Rung 2) is blocked on the absent emitter (FV-2), not on "three
  fixes."
- **G-7A-2 · Rung 1 is a gate-that-cannot-fail** (json's staleness check re-pointed) — disclosed by
  the family, but the shippable half is vacuous-adjacent and the non-vacuous half is unbuilt.
- **G-7A-3 · S0 ≤ ~600 LOC is un-anchored and likely thesis-collapsing.** §1 open-Q2 already fears
  "build not recognize inflates S0 toward the hand parser." On FV-4 (construction expressibility
  unproven) the ~600-LOC number has no valid basis; the build-budget ~88s→~0.5s datum is a
  legacy-tree transplant (skinny xtask self-hosts nothing — 0 targets).

**Enumerated open gaps.** (a) no general-descent emitter exists (FV-2) — 7A cannot mint a
self-reparsing S0 at all yet; this is worse than the "3 parser fixes" the record carries. (b) fixpoint
*meaning* (bytes vs GrammarIr) unresolved and empty until a builder grammar exists (FV-4). (c) S0 LOC
ceiling un-anchored; build-budget datum is legacy-transplant. (d) seed disposition (frozen R4-i =
self-*verifying* not self-*hosting*; INC-3 admits 7A-default ⊆ 7D) unchosen. (e) Q2 twin-ladder
(one fixpoint xtask over bbnf+regex or two) unresolved. (f) the GN-0/GN-1 rescue gate (the working
legacy self-host is being archived) is not born-RED in 7A's gate list.

---

## 2. Family 7B — TOTAL-SELF-HOST — convergence 33%

**Sound.** Plane D (templates-as-data) is a real in-tree gradient (`runtime_generator.rs` consts
→ shape-selected). PROTO-2 **de-risks the build-boundary fear** for Plane A construction: the "graph-
shaped interned IR ⇒ as hard as the original problem" premise is empirically false *for the
construction arena* (no interner; append-only; separable link). Plane C's own BLOCKED-candidate
self-assessment is candid.

**Defects (within-lock).**
- **G-7B-1 · Plane A is blocked on FV-2 (no emitter), NOT on parser fixes, AND the floor-0 verdict
  it leans on is inferred (FV-4).** PROTO-2 shows the target shape is catamorphic; it does *not* show
  bbnf `->` expresses it or that any emitter lowers it. "Even a good budget yields only a *recognizer*
  until Brief 2 resolves" (7B Plane A text) is right — but Brief 2 (PROTO-2) resolved only the *shape*
  question, not the *expressibility/emission* question. Plane A remains unbuilt end-to-end.
- **G-7B-2 · Plane B `ir.bbnf` is a consumer-less-substrate risk conditional on an un-named cross-lock
  verdict.** Its gate ("generated type surface byte-identical to a frozen snapshot") means the grammar
  produces *nothing a drift-gate-over-the-enums doesn't already give* — the only consumer is a gate
  that needs no grammar. Double-homed with LOCK-3 3C (GRAMMAR-HOMOIOUSIA, verbatim the same
  deliverable); if LOCK-3 picks 3A/3B/3D, `ir.bbnf` has no IR-spine consumer (clause viii / Y.13). Zero
  in-tree prototype — presented as "feasible" with the confidence of a run that never happened; label
  SPEC_ONLY.
- **G-7B-3 · Plane C is an unresolved BLOCKED-candidate and an elegant-reduction trap.** "Structural
  emission as grammar, kernels stay hand-Rust" reads clean, but the load-bearing "control-flow /
  dispatch / tape-assembly expressible without a borrow checker" is the unconverged hard part
  (R3 category error). No prototype scoped it. Note: PROTO-2's floor-0 finding is about the *parse-
  construction* arena in `grammar/`; it does **not** touch the mid-IR/`BackendExpr` graph in `ir/`
  that Plane C targets — Plane C stays correctly BLOCKED-candidate and PROTO-2 must not be read as
  relieving it.
- **G-7B-4 · INC-2 decided against 7B on the one measurable edge** (PROTO-2 regex: gen 1.77–1.81×
  slower; neither promotion gate fires). 7B's own R1 concession ("if diagnostics unmet, Gap A demotes
  to oracle") is 7D. The family collapses toward 7D under measured pressure — but note this collapse
  is *proven only on the regex edge*; the bbnf edge is unbuilt (FV-2), so the collapse is an
  **unverified gestalt** where it matters most.

**Enumerated open gaps.** (a) Plane A: no emitter (FV-2) + expressibility inferred not demonstrated
(FV-4). (b) Plane B: consumer-less/LOCK-3-conditional + unprototyped diff-gate. (c) Plane C:
BLOCKED-candidate, unscoped, category error. (d) diagnostics parity (generated parser emits spans as
richly as 2142 LOC hand machinery?) unmeasured. (e) regex-`->`-to-HIR is LOCK-5-dependent and
`host::lower` rejects all bounded `{n,m}` (materially broken, not merely "recognizer"). (f) four-plane
build ceiling unmeasured (legacy transplant).

---

## 3. Family 7C — HOST-FUNCTION-LATTICE — convergence 58%

**Sound and best-substantiated in-tree.** Every substrate claim verified: `passes/src/lib.rs:794-798`
string heuristic; `bbnf.bbnf:73` typed `@host` directive (zero-usage dormant); `bbnf.bbnf:33` `@{}`;
`grammar/src/lib.rs:222 BBNF-UNSUPPORTED-HOST-CAPTURE`; empty `json/host.rs`. T2 (delete the heuristic
→ `check-json` bit-for-bit) and T4 (`@{}` lowering) are genuinely in-tree and shippable. **PROTO-2
newly gives 7C a concrete, measured floor:** `resolve_refs` (whole-grammar name link) is a
`TreeFold` host fn — floor = 1 for bbnf's *own* linking, floor = 1 CompileIO for `@import`. This is
the family's first empirically-grounded host-fn instance and it is exactly the "typed, counted,
floored" home 7C promises. The attribute-grammar framing (`&mut Ctx` = inherited attribute) is exact.

**Defects (within-lock).**
- **G-7C-1 · "fully orthogonal / NOT blocked" is overstated (INC-4).** T6 (`@host` survives
  `regen∘regen` on `bbnf.bbnf`) inherits the base-case wall — `bbnf.bbnf` does not fully parse (walls
  662, FV-1) and there is no emitter (FV-2); the bbnf totality% floor cannot be *counted* until a
  builder grammar exists. 7C is orthogonal for T1/T2/T3/T5, **not** for T4/T6.
- **G-7C-2 · T4 understates the work.** `@{}` is a *hard parse reject* in the RD path (`parse_atom`
  `:1495` has no `@` arm), not merely a runtime `BBNF-UNSUPPORTED` classification. T4 must both *parse*
  `@{}` and lower `HostCapture` — the same wall the ladder needs (cross-pollinated fix).
- **G-7C-3 · ZERO prototype exercised 7C.** For a family this cheap to probe (delete one heuristic,
  run `check-json`) the absence of a T2 run makes "most independently viable / one-file cure"
  SPEC_ONLY, not a result. This is the single most cost-effective missing prototype in the lock.
- **G-7C-4 · the typed-lattice half is LOCK-1-coupled.** T1 (typed signature into inference) + T5
  (tier off `&mut Ctx`, zero-cost re-verify) need LOCK-1's solver to admit a typed leaf; the
  independence claim holds only for T2+T4. Split the claim.
- **G-7C-5 · CONTEXT-tier zero-cost (clause v) unverified for the tier that most threatens it** — no
  CONTEXT host fn exists (usage = 0); TOML `push_path` is designed, not run; and the floor-gate has
  never tripped (a gate not yet proven able to fail).

**Enumerated open gaps.** (a) T6 + bbnf-floor share the emitter wall (FV-2). (b) T4 needs `@{}`
*parsing* + lowering. (c) no prototype for T1/T2/T5 despite trivial cost (SPEC_ONLY). (d) T1/T5 are
LOCK-1 consumers. (e) CONTEXT zero-cost unverified; TOML `push_path` unbuilt. (f) totality% counter
(leaves × branches) specified but unbuilt — every published per-subsystem number is projected, none
measured. (g) CSS floor UNMET (`color.bbnf`/`values.bbnf` depend on the un-lowered `@{}`).

---

## 4. Family 7D — META-CIRCULAR-ORACLE — convergence 58%

**Sound and the only family with a reproducing measured result.** PROTO-2 ran a real paired cold
oracle over 51 patterns: structural parity 0.7647 (0.80 under `Empty ≡ Concat([])` normalization),
membership 0.8235, perf gen/hand 1.77–1.81× slower — **neither promotion gate fires → ORACLE-ONLY**,
resolving INC-2 toward 7D on the one edge with committed producer code both sides. The 12-row
divergence ledger is exactly the "checkable spec of the hand parser" the family promises. The §0-fact-4
correction (the regex producer public entry `regex/mod.rs:34` already ships) is 7D's own catch and is
verified true. ORACLE-ONLY as a lawful terminal is well-argued and Y.13-compliant (CI-consumed) —
this is the family that cures the consumer-less-substrate violation the instant a parser is emitted.

**Defects (within-lock).**
- **G-7D-1 · the bbnf edge shares the emitter wall (FV-2) and the NormalAst oracle is a *weak* oracle
  until a builder grammar exists (FV-4).** 7D §4 open-Q1 concedes it: the generated side yields a
  span-tape, so the differential compares only at *recognition/span granularity* — it misses
  wrong-precedence and wrong-construction, the very defects an oracle exists to catch. Without the
  builder grammar the bbnf ledger row is not just red, it is *low-resolution*.
- **G-7D-2 · the regex parity headline is soft and double-homed.** 9 of 12 divergences are ONE
  fixable `host::lower` bug class (bounded `{n,m}` reject) — 0.76 measures LOCK-5's incomplete fold,
  not an oracle verdict. The stable verdict is perf (1.8×). Report "regex ships now *as a red ledger
  row*"; the parity-moving fix lives entirely inside LOCK-5's `host.rs` (cross-lock, not 7D's).
- **G-7D-3 · "7D subsumes 7A on promotion" is vacuous on current evidence** — nothing promotes
  (parity < 1 AND perf > 1 on the one decidable edge), so the fixpoint corollary never activates. 7D
  honestly names ORACLE-ONLY as terminal, so this is disclosed, but the subsumption framing oversells
  a relation that never fires.
- **G-7D-4 · ≡ needs codifying** (`Empty`-normalization + the `Hand = parse+resolve_refs+validate`
  vs `Gen = pure parse` pivot must run shared semantic passes on both or neither) before any parity%
  is quotable; stated, not implemented.

**Enumerated open gaps.** (a) bbnf edge on the emitter wall (FV-2) + weak-oracle-until-builder (FV-4).
(b) regex ledger red; promotion may never fire; the fixable rows are LOCK-5's (XL-2). (c) ≡
uncodified. (d) canonical regex `.bbnf` LOCK-5-blocked (three divergent grammars). (e) grammar-driven
corpus sampler unbuilt — parity over the committed 51 only, not saturated.

---

## 5. Incompatibility map + failure-mode checklist (within-lock)

**Map.** The DAG framing (7A-determinism-floor ∧ 7D-oracle-trajectory ∧ 7C-typed-boundary →
7B-per-plane) is directionally sound. **INC-2** is measured toward 7D on the regex edge — but that
resolution is *proven only where cheap* (regex) and *unverified where it matters* (bbnf, unbuilt per
FV-2): an **unverified gestalt**, to be labeled as such. **INC-1** (7A⊗7B scope) is sound; moot while
7B is banked. **INC-3** (hand-parser fate) composable. **INC-4** ("7C orthogonal to all three") is
**false for T4/T6** (G-7C-1/2).

**Checklist hits.**
- **Elegant-reduction trap:** PROTO-2 "catamorphism ⇒ `->`-expressible" (FV-4, the builder grammar is
  the unconverged hard part); 7B Plane C "structure as grammar, kernels stay hand-Rust" (control-flow
  without a borrow checker is the hard part).
- **Gate-that-cannot-fail:** 7A Rung 1 (staleness re-pointed); a `regen∘regen` fixpoint over the CSS
  phantom (G-7A-1).
- **Vacuous convergence:** 7A fixpoint (empty until a builder grammar exists); 7D "subsumes 7A"
  (never activates).
- **Consumer-less substrate (viii/Y.13):** 7B Plane B `ir.bbnf` (byte-identical-to-snapshot;
  LOCK-3-conditional). 7D cures the *reverse* violation (dormant `.bbnf` gets a consumer).
- **Masked fallback / legacy alias:** 7A-default frozen seed = hand parser stays production forever
  → self-*verifying* dressed as self-*hosting*.
- **Prototype-evidence misuse:** PROTO-1-7B's 998-LOC "Ok" is a CSS phantom — *correctly self-flagged*
  by the proto (good faith); but its t1 hand-parser cross-check row is source-false (FV-1), a re-quoted
  corrected error in the newest artifact. PROTO-2's 0.76 parity over-weights one fold bug (9/12 rows,
  LOCK-5). No SPEC_ONLY anchors a perf claim (both protos RAN_MEASURED) — but 7C's "most viable" and
  7B Plane B's "feasible" ARE SPEC_ONLY and should be labeled.
- **Unverified gestalt:** "families collapse toward 7D" — proven regex, unbuilt bbnf.

**Constitution.** Clause (i) grammar-totality: PROTO-2 fixes bbnf's own honest **floor > 0** — even
the self-host needs 1 TreeFold host fn (`resolve_refs`) + 1 CompileIO (`@import`); combined with FV-2
(no general emitter), *front-end* totality is demonstrated by NO family. The lawful deliverable is to
name front-end grammar-totality as a **counted floor, not 100%** (7C's counter is the instrument) —
no family states this outright. Clause (v): 7C CONTEXT-tier zero-cost unverified. Clause (vii): both
protos are concrete artifacts (good); the one breach is PROTO-1-7B's re-quoted t1 row. Clause (viii):
7B Plane B at risk; 7D compliant. Clause (ix): 7A/7B/7D are net-new-vs-GN tranches (GN omits
bootstrap) — lawful only if declared; 7C re-cuts GN-5 well.

---

## 6. Verdict summary (critic-1, within-lock)

| family | conv % | disposition | the one gap that dominates |
|---|---|---|---|
| 7A | 42% | ADVANCE-narrowed (fixpoint law only) | no general emitter (FV-2); fixpoint vacuous until a builder grammar exists |
| 7B | 33% | BANK (posture measured-dominated) | Plane A blocked on FV-2 + inferred-not-demonstrated (FV-4); Plane B consumer-less; Plane C category error |
| 7C | 58% | ADVANCE (the survivor) | strongest in-tree yet SPEC_ONLY (no T2 probe); not orthogonal for T4/T6; typed half LOCK-1-coupled |
| 7D | 58% | ADVANCE (posture winner) | only reproducing result (regex ORACLE-ONLY); bbnf edge on FV-2 + weak-oracle-until-builder |

**Delta vs the carried REGISTRY (45/35/60/60).** I hold all four slightly lower because the newest
prototypes did not *improve* the vertical families' feasibility — PROTO-1-7B's phantom finding
*worsened* it (the emitter is architecturally absent, not "three fixes"), and PROTO-2's de-risk is an
*inference* (FV-4), not an end-to-end builder demonstration. 7C/7D lose 2 points each for the
un-run cheapest probes (7C-T2) and the un-built builder edge; 7A/7B lose 3/2 for FV-2 hardening the
emit wall.

**Mandatory pass-2 corrections before any convergence talk.**
1. **Reconcile the record:** strike PROTO-1-7B's t1 hand-parser cross-check row (byte-421 "no `->`
   production" is source-false — `parse_grammar` returns `PARSE_OK` on the identifier line, walls at
   662). Carry only the *generator-path* 421 result and the *hand-path* 662 result.
2. **Restate the make-or-break with FV-2/FV-3/FV-4:** OPEN-1 is not "does the general-descent emitter
   lower bbnf" (it presumes an emitter that PROTO-1-7B showed does not exist) but **"author builder
   `->` projections on bbnf's structural rules, emit via `passes::compile`/`emit_from_source` (the
   UNTESTED structural path, FV-3), and show the emitted parser reconstructs `GrammarIr` equivalent to
   the hand parser."** This is the single un-run experiment the whole lock rests on.
3. **Run 7C-T2** (delete `has_parse_time_host_decode` → registry lookup; `check-json` bit-for-bit) so
   7C's "most viable" stops being SPEC_ONLY — the highest ROI probe in the lock.
4. **Name front-end grammar-totality as a counted floor (> 0)**, per PROTO-2's `resolve_refs`/`@import`
   floors and FV-2 — no family currently states the honest ceiling.
5. **Label the ~88s build datum legacy-transplant** and the "collapse toward 7D" gestalt as
   verified-regex / unverified-bbnf.
