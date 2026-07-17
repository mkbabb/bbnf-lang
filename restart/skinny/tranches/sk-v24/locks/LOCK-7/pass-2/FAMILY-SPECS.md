# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 FAMILY SPECS (synthesis, post-fold re-cut #2)

Distilled by the SYNTHESIS agent (Fable-tier), pass 2, from the freshest pass-2 research
(A/B/C/D/priorart, regenerated RED-blind at 22:xx), **all six pass-2 prototypes that touched
disk** — `PROTO-1` (OPEN-1′ builder-augment → refuted at the leaf lexer), `PROTO-3-leaf-evaluator`
(leaf evaluator → **MISSING PRIMITIVE**, cross-repo), `PROTO-2`/`7C-T2` (dead-heuristic delete →
**PASS, near-vacuous**), the regex Tier-S oracle (→ **ORACLE-ONLY**, parity below gate),
**`PROTO-2-UF1`** (structural-only leaf-blind oracle → **RAN**, branch skeleton SOUND 52/53), and
**`UF-2`** (totality counter → **harness authored, NEVER RUN**) — the LOCK-7 REGISTRY standing
verdict, CRIT-1 (within-lock) + CRIT-2 (cross-lock), and CROSS-LOCK (XL-A…XL-H, consumed as binding
fact). One cogent spec per family **while the routes remain incompatible** — architecture,
mechanism, integration seams, gates, exact open questions — then the incompatibility map. Elegance
earns nothing alone: a family stalled at a missing primitive equal in difficulty to the original
problem is **BLOCKED-candidate**; a family stalled at a primitive grep-verified absent AND owned by
another (cross-repo) lock is **BLOCKED (measured)**.

**What this re-cut folds that the prior one (19:33) could not.** The prior FAMILY-SPECS proposed
`UF-1`/`UF-2` as *future* briefs; both then touched disk. **`UF-1` RAN** (20:04) and **refuted the
prime suspect** — the branch skeleton is essentially sound (52/53 leaf-blind isomorphic), so the
INC-5 block is *not* skeleton-poisoning but resolves into three disjoint non-branch seams. **`UF-2`
was authored as a complete harness (19:58) but never executed** — its per-subsystem numbers are
un-banked. Folding these is the same BREACH-1 discipline the registry applied to PROTO-3/7C-T2:
synthesis moves current, not stale.

**Base (XL-H).** Prototypes ran base-pinned (`ce51c36fc`, confirmed ancestor of HEAD). Load-bearing
disk facts re-verified live on the skinny measurement substrate (XL-A): regex-engine grep
`struct (Dfa|Nfa|Program|Instruction)|transition_table|fn match_at` over `crates/` = **EMPTY**;
`general_descent/{emit,mod,regex_first,view}.rs` = **1206 LOC** (452/93/230/431);
`has_parse_time_host_decode` at `passes/src/lib.rs:794`; `bbnf-regex::RegexKind` a 4-bucket
classifier. parse-that anchors at `/Users/mkbabb/Programming/parse-that`.

> **Housekeeping owed to GESTALT.** **BREACH-2** — the regex Tier-S report-of-record was clobbered
> by the 7C-T2 report at `pass-2/PROTO-2.md`; the RAN evidence survives as this file's §4 + `PROTO-2.patch`
> + `pass-1/PROTO-2-7D-regex-oracle.md`. Re-home under `PROTO-2-7D-regex.md` before GESTALT (H-1).
> **UF-1 filename** — the structural-oracle report is `PROTO-2-UF1.md` (named to avoid clobbering
> `PROTO-2.md`); its patch is `PROTO-2-UF1.patch`. **UF-2** — patch `UF-2-totality-counter.patch`
> carries the test but **no run report exists**; owed a RAN report before the counter is quoted.
> **Dead-ID** — CROSS-LOCK keys are **XL-A…XL-H**; numeric cites re-point (regex fork → **XL-C**;
> IR-plane → **XL-B**).

---

## §0 — THE SPINE ALL FOUR FAMILIES SHARE (measured, re-verified, not asserted)

1. **A grammar-NEUTRAL descent emitter EXISTS** — `crates/codegen/src/general_descent/` (**1206
   LOC**). `runtime_generator.rs:98 !has_rule("value")` → `emit_general_descent`. `bbnf.bbnf` has no
   `value` rule → it shape-dispatches here (the branch `regen-sheets` travels). It emits **one
   `parse_<rule>` recognizer per rule** (offset-mark tape + lazy `{Prefix}Document` view), **not
   GrammarIr builders**. The emitter is real, not a stub.

2. **THE HEADLINE — grammar-neutral at the BRANCH level, grammar-SPECIFIC at the LEAF level, and the
   leaf fix is a primitive that DOES NOT EXIST (PROTO-1 + PROTO-3, RAN).** The leaf layer
   (`regex_first::first_bytes` + `emit.rs::{match_leaf, consume_quoted, is_text_byte}`) is a
   Sheets/JSON delimiter-scan approximation. Against bbnf's real leaves it **hard-fails at the first
   regex literal**: `split_top_level_alternation:90` splits at paren-depth 0 only → bbnf's
   `(\.|[^\/])+` collapses to `first={'.'}` → the `match_leaf` head-gate REJECTS every `/…/` literal;
   `consume_quoted` honors doubled-quote not backslash escapes; `is_text_byte` excludes `-` /
   includes `.$`. **PROTO-3 decided the sharp question — bounded arm-swap or missing primitive:
   MISSING PRIMITIVE.** The hypothesized "route `/regex/` leaves through the extant compiled matcher"
   **does not exist**: grammar tier holds an *opaque interned pattern string*
   (`ir::BackendExpr::RegexProgram{pattern:String}` — a misnomer, no program); engine tier
   (`bbnf-regex`) is a **4-bucket classifier** (`{Whitespace,QuotedString,Numeric,Unknown}`) with
   **no consume/match method** — **all 7 bbnf named-rule leaves classify `Unknown` (0/7 tokenize)**;
   `parse-that-regex` is delimiter-fixed JSON scanners. So the bbnf leaf evaluator = **author a
   general regex-execution engine** `fn match_at(pattern, input, start) -> Option<end>` = **LOCK-5's
   cross-repo deliverable (INC-5)**.

3. **THE NEW REFINEMENT — the block is NOT branch-skeleton poisoning; bbnf IS structurally Tier-R at
   the branch level (UF-1, RAN).** `PROTO-2-UF1` lowered the bbnf import-closure GrammarIr through the
   same `!has_rule("value")` path (`sink_only::lower_program`) and compared leaf-blind against the
   oracle. **52/53 rules are branch-isomorphic** (charitable reading; 25/53 strict, the strict/charitable
   delta = exactly the 27 `?w`-bearing rules whose only difference is `Annotation(?w)→Seq([])`, a
   whitespace-leaf representation). The `Seq/Alt/Optional/Repeat/CallRule` nesting+arity survive
   `passes::lower_expr` faithfully. **The prime suspect is REFUTED:** research-B §4's feared "`term`
   8-branch alternation collapses to one node kind" is an `emit.rs` **render-time** concern strictly
   downstream of `SinkOnlyExpr`; at the lowering level `term`'s 8-branch `Alt` is preserved intact.
   The **sole** true divergence is `bool_lit`: `derive_materialization_roles.bool_value` misclassifies
   bbnf's `bool_lit = "true" | "false"` as a JSON boolean value and wraps it in a `BbnfBool`
   `DirectBuild` sequence — **one named materialization-role misfire, not a systemic collapse** (the
   `Alt` survives inside the wrap). **INC-5 therefore resolves into three disjoint, non-branch seams
   in DAG order:** (i) parser front-end (`?w`-then-`->` rejected at byte 2343 in an unpatched tree;
   `@import`-closure resolution) — upstream token-adjacency, not skeleton; (ii) leaf floor (`match_at`,
   §0.2) — runtime, leaf-level, cross-repo; (iii) one `bool_value` materialization misfire — bounded,
   named, in-lock. Carry the omega edge as **"INC-5 = leaf-floor + parser-front-end + 1 materialization
   misfire; branch skeleton SOUND (52/53),"** NOT "bbnf is not even Tier-R."

4. **Construction floor = 2, irreducible ∈ {0,1,2}** (LOCK-1-dependent). Node *values* pure `->`
   (floor 0); *build mechanisms* host-bound — `add_expr` (`ir/src/lib.rs:51`, append — SOFT) +
   `resolve_refs` (`ir/src/lib.rs:84`, cross-rule name→RuleId — HARD, an *inherited attribute*,
   host-bound iff LOCK-1 declines in-grammar inherited attributes).

5. **No self-host exists on skinny (re-greped).** xtask carries `regen/check-{json,sheets,css,
   real-typed}`; **no `regen-bbnf`, no `regen-regex`, no `fixpoint`/`BBNF_TARGET`.** Stage-0 must be
   *authored*, not transplanted. The `88s→0.5s` datum is a **REFUTED legacy transplant** (cargo LTO
   rebuild-avoidance, not regen/parse; archives at GN-0) — quoting it against skinny is a Constitution-ix
   breach.

6. **The regex self-host ships as a PRODUCER, MEASURED ORACLE-ONLY (regex Tier-S, RAN).** parse-that
   `regex/mod.rs:34 parse_generated -> Result<Hir>` (generated parser then `host::lower` — a producer,
   NOT the `generated.rs:2288 -> bool` recognizer twin). Under pinned `ORACLE_REL_V1`: parity **0.7759
   strict (45/58)** / 0.7931 Empty-normed / 0.7843 committed-51 — **all BELOW the 0.80 gate**; GEN
   **1.77× slower**. 12/12 divergences classified (9× `host.rs` bounded-`{n,m}` obligation → LOCK-5;
   3× `regex.bbnf` verdict → oracle bank); 4 planted mutations all trip. Details §4.

7. **7C's totality% counter is a STATIC GrammarIr walk, NOT a self-emit census — and the harness is
   NOW WRITTEN but UN-RUN (UF-2).** research-C §3 defines it verbatim: walk the built `grammar.exprs`
   arena, classify LEAVES×BRANCHES × PURE/CONTEXT/HOST. GrammarIr for **json/sheets/real-typed exists
   today**; **bbnf's own GrammarIr exists too** (PROTO-3: `parse_grammar` over the import-closure = Ok,
   53 rules, resolve_refs clean, behind only the landed Wall-0 `?w`-fix). The counter needs the **build**
   step (available), NOT the **emit** step (blocked). **UF-2's patch carries the complete counter** —
   `census(ir)` tallies leaves (`Literal`/`Regex`/`Project`) × branches (`Alt` arms/`Optional`/`Repeat`),
   `totality% = 1 − ΣTier-2/Σall`, per-subsystem, asserting sensible bounds. **It has never been run and
   its numbers are un-banked.** Its own note (patch line 250) fixes the caveat: the real corpus has NO
   Tier-2 host node → totality% reads **1.0 exactly**; non-vacuity requires a *synthetic* Tier-2
   injection (`HOST_SENTINEL="host"`) to exercise the floor. So the counter is *specified and
   harness-ready* — its execution + a demonstrated non-1.0 floor is the un-discharged step (UF-2 brief).

**The unifying pass-2 fact.** The whole **bbnf leg** (7A recognizer/building seed, 7B Plane-A,
7D-bbnf Tier-R/S) rests on ONE cross-repo gap — **LOCK-5's engine-tier `match_at`** (INC-5) — now
bounded precisely by UF-1 to *leaf + front-end + 1 misfire*, branch skeleton sound. No bbnf-leg
family converges inside LOCK-7 alone. **What DOES converge in-lock:** 7C's lock-free trio + the
static totality counter (harness written, run pending), and 7D's regex ORACLE-ONLY negative. The
bbnf leg's pass-1→pass-2 trajectory is a **reachability regression** (builder-augment → leaf lexer →
general regex engine, cross-repo) even as design clarity rose — **the % tracks reachability.**

---

## §1 — 7A · STAGED-BOOTSTRAP — **BANKED** — 40%

**Architecture.** Classic two-stage self-host on the greenfield tree: a committed stage-0 + a
fixpoint gate certifying `regen∘regen = regen` (the rustc/GHC/pegen `stage2==stage3` law), minimal
auditable bootstrap surface. Net-new on skinny (§0.5). Its one non-redundant asset is the **fixpoint
LAW** (byte-identical cross-generation re-emit, no 2-cycle) + a blake3 provenance ledger — **not** LOC
reduction, which the numbers refute (research-A §1: a *building* seed inflates toward the 2142-LOC
hand parser; in-repo witness = parse-that regex self-host `generated.rs` 2293 + `host.rs` 524 for a
*smaller* language). Prior art is directly supportive (rustc/GCC 3-stage byte-identity;
reproducible-builds; tree-sitter's derived `parser.c` CI diff — the strongest industrial fixpoint
analog, non-vacuous by construction over a *derived* artifact).

**Mechanism / the seed headline (research-A §2 + PROTO-1/3).** The seed axis is
*recognizer-useless-vs-building-adequate*; the probes remove even the recognizer consolation:
`general_descent` **cannot recognize bbnf** until the leaf evaluator lands (LOCK-5's engine, §0.2) —
a recognizer seed produces zero correct leaf spans, nothing to fixpoint over. A *building* seed, if
reached, inflates PAST the hand parser, dissolving the auditability that is 7A's whole selling point.
(The inflation is argued from an in-repo *analogy*, never measured on an actual bbnf building-seed —
an un-run claim both critics flag.)

**Fixpoint SUBJECT (research-A §3 / research-D, decided).** Cross-generation `.rs` bytes as the
shippable gate; a **GrammarIr-equivalence floor** as the non-vacuity witness — and that floor **IS the
same object as 7D's `ORACLE_REL_V1`** (do not double-home). Decide decisively (no hedge): the GrammarIr
floor is primary; the byte-gate is a hygiene sub-gate non-vacuous *only* under a planted input-mutation
that MUST move the bytes (the template-splice `include_str!` path makes a bare byte-identity a
constant-stamp — "triple-identical" certifies the template, not the parser; research-D R5).

**Integration seams.** `BBNF_TARGET` mirroring `SHEETS_TARGET` (`xtask/src/main.rs:200`); re-author the
fixpoint arm against `emit_runtime_from_request`; reuse `regen.rs:37 check_target` for the byte
sub-gate. Rescue only the legacy *corpus* of adversarial bbnf inputs as fixtures; **do NOT** rescue the
legacy bespoke builder front-end (a NAMED overfit refutation — it archives at GN-0; the two generators
are not shape-compatible, research-A §3).

**Gates (born-RED).** (a) `check-bbnf-selfparse` — the import-closure clears with the `parse_postfix_ops`
`?w`-attach fix landed in the PROTO-3/UF-2 worktrees (`grammar/src/lib.rs`, +12 net lines); single-file
bbnf still ERRs at the **`@import`-closure wall** (PROTO-3, a *distinct* wall from Wall-0 and the leaf
floor). (b) leaf-evaluator gate — **BLOCKED cross-repo** on LOCK-5 (§0.2). (c) cross-generation
byte-fixpoint + GrammarIr floor + planted-mutation non-vacuity — **NEVER RUN on any artifact**
(SPEC_ONLY), **but runnable TODAY on the regex oracle artifact** (the regex parser self-emits — UF-3).

**Why BANKED / exact open questions.** After the fixpoint-law graft (→7D, OPEN-2/XP-2) and the
shared-relation convergence (its GrammarIr floor ≡ 7D's `ORACLE_REL_V1`), 7A owns **nothing 7C+7D do
not**. Its concrete pass-2 gain (the `parse_postfix_ops` `?w`-attach, json ERR→Ok) is a *prerequisite of
the 7D/leaf leg*, not a 7A deliverable. **Survival test:** name ONE thing 7A owns post-graft that 7C+7D
do not, or 7A RETIRES next pass. The one fixpoint signal runnable WITHOUT LOCK-5's engine is UF-3 —
decoupled from promotion, needing neither perf≥1.0 nor parity=1.0, only byte-identical re-emit + a
mutation that moves the bytes.

---

## §2 — 7B · TOTAL-SELF-HOST — **BANKED** — 32%

**Architecture.** Constitution (i) at its maximal pole: bbnf + regex + IR + emitter templates *all*
grammar-specified, the generator becoming mostly data, hand code shrinking to kernels/primitives.
Theoretically grounded (third Futamura projection = compiler generator; Jones-optimality =
"generator becomes data" made precise; research-priorart §2.4) with a 1968 answer to the emitter
sub-question (TREE-META unparse rules / OMeta tree-pattern codegen — a *separate* tree-transformation
grammar, NOT `->` projections stuffed into the parse grammar; §2.5). Decomposes into planes with
distinct verdicts. A **research answer, not a route** (its own framing).

**Plane-A — front-end self-host.** **BLOCKED (measured), cross-repo** — same leaf primitive as
7A/7D-bbnf (§0.2). UF-1 refines: the *branch skeleton* is sound (52/53), so Plane-A's block is leaf +
front-end + 1 misfire, not structural.

**Plane-B — the IR plane (`ir.bbnf`→`ExprKind`), joint LOCK-3 ⊗ LOCK-7 (XL-B).** `ExprKind` (10 arms,
`ir/src/lib.rs:234`) is a regular tree grammar → `ir.bbnf` is *writable* as inert data (LOCK-3 3C
GRAMMAR-HOMOIOUSIA). Lawful default **narrow-first**: `ir.bbnf` *generates* the hand enum (the in-tree
`egraph-derive` / memory `derive-language-macro` precedent) with named consumers (`add_expr`,
`resolve_refs`/`validate`, the `lower_expr` fold) — OR `ir.bbnf` as the concrete syntax of a
serialized-IR text with a **round-trip parity oracle** consumer (needs no new type machinery, lawful
today, a `check-ir` sibling of `check_target`). The transposition *logic* is never data (research-B §3;
MLIR ODS/PDL confirms: shape is data, rewrite logic is a rule-table over a primitive engine); only the
IR *shape* can be. **Conditional on LOCK-3 picking 3C; else consumer-less (Y.13).** Gating dependency:
node-value `->` projections presuppose LOCK-1 sum/product projections (GN-3 Walls 1–2).

**Plane-D — emitter templates as grammar data (static line-read on `emit.rs`, 452 LOC).** **DATA ≈
50%** (fixed kernel template + grammar-derivable holes) · **WALK-FOLD ≈ 20%** (a catamorphism over the
`SinkOnlyExpr` sum, `->`-expressible) · **IRREDUCIBLE ESCAPE ≈ 21%** — the speculative-cursor reborrow
`let cursor = &mut _probe` inside `(|| {…})()` committed on `Ok` (Alt/Optional/RepeatLoop, emitted ×3),
plus the progress-guard `if _probe == _before { break; }`. **Named kernel-escape signature:**
transactional commit-or-rollback over a mutable shadow cursor+tape; delete the `&mut _probe` re-alias →
**E0499/E0503** borrow-fail. No production system erases the emitter kernel — it *relocates* into a
combinator/VM runtime (LOCK-4C); the generator becomes data *over that runtime*, the escape does not
vanish (the 3-primitive floor = probe/commit, many-with-progress-guard, mutable-cursor threading, which
must be LOCK-4's existing core, *consumed*, or the escape has moved not shrunk — research-B R6).

**Status: BANKED · maximal pole BLOCKED-candidate; exact open questions.** "Generator becomes mostly
data" holds for ~70% DATA+WALK-FOLD, **false for the ~21% ESCAPE** (a missing primitive equal in
difficulty — it *is* authoring a VM). **Load-bearing caveat both critics pressed:** the 50/20/21 split
is a **static line-count read (research-B §3), NEVER RUN**; shares sum to ~91% (**~9% of `emit.rs`
unclassified — name the residue or the ratio is soft**); the E0499 is *checkable* (delete the re-alias,
observe the compile-fail) but was asserted, not shown as a RAN compile-fail — the twice-un-run
experiment. Scope homes in LOCK-3 (XL-B): if LOCK-3 picks 3C, draft the joint Plane-B family; else 7B
RETIRES.

---

## §3 — 7C · HOST-FUNCTION-LATTICE — **ADVANCED, the SURVIVOR** — 57%

**Architecture.** Totality as a *bounded, measured* quantity: a three-tier lattice keyed on ONE
structural fact read off a signature — **whether it threads `&mut Ctx`** (not a name test, not a
`@pure` string — a pluggable decision point). **Tier 0 LEAF-DECODE** (pure, `->`-adjacent; grammar
coverage) ⊑ **Tier 1 CONTEXT** (`resolve_refs`, TOML `push_path`, `?w` — the parser-generator instance
of Knuth *synthesized attributes*; comptime, zero residual) ⊑ **Tier 2 RECURSIVE/STATEFUL** (`&mut Ctx`
host, an *inherited attribute*, LOCK-1-bound). The anti-gaming property is load-bearing: **hiding a
`->`-able leaf inside a host LOWERS totality%, it cannot hide behind it** (the counter to the
name-gate-in-disguise trap). Tree-sitter proves the escape hatch is the *correct* architecture; bbnf's
typed `@host name : type` is already stricter than tree-sitter's opaque C scanner.

**Independence split (research-C §2, XL-3).** **Lock-free** (no LOCK-1 verdict): (1) delete the string
heuristic `has_parse_time_host_decode` (`passes/src/lib.rs:794`; keys on `name.contains("host") &&
…"decode"|"parse"` — a latent, DEAD name-gate) → a typed predicate over the node's tier; (2) lower
`@{}` — add `ExprKind::Host` (the *real* typed node) + a `parse_atom:1495` `@{` arm reusing
`runtime_skip_balanced`, flip `RuntimeConstructKind::HostCapture` off `"BBNF-UNSUPPORTED-HOST-CAPTURE"`
(`grammar/src/lib.rs:222`); (3) *name* `resolve_refs` the CONTEXT floor (designation, no code); (4)
**run the static totality counter** (§0.7). **LOCK-1-bound:** typed `HostFnSig` into inference (T1);
tier-off-`&mut Ctx` (T5); the floor itself (∈ {0,1,2}).

**Deliverable + gate.** A two-population totality% counter — LEAVES (`Literal`+`Regex`+`Project`) ×
BRANCHES (`Alt` arms + `Optional`/`Repeat`) — per subsystem, PURE/CONTEXT/HOST breakdown; `totality% =
1 − Σ(Tier-2)/Σ(all)`; floor-gate `xtask check-totality` (a GN-5.W3 `check-name-gates` sibling,
`syn`-backed AST audit, floors in `[workspace.metadata.bbnf]`) with three-form planted-breach
falsification. **UF-2 authored the counter as a runnable test** (`grammar/tests/totality_counter_probe.rs`,
259 lines) — builds json/sheets/bbnf-closure GrammarIr, censuses, asserts `total>0 ∧ leaves>0 ∧
branches>0 ∧ totality∈[0,1]` per subsystem. **It has NOT run.** Non-vacuity caveat (patch line 250):
real grammars have NO Tier-2 host → totality% == **1.0 exactly**; the floor is exercised only by the
synthetic `HOST_SENTINEL` injection. **RUNNABLE NOW** over json/sheets/real-typed (built GrammarIr) +
bbnf import-closure; **CSS deferred** until T4 (`@{}` lowering — `color.bbnf:220`/`values.bbnf:69` depend
on it).

**7C-T2 RAN and PASSED (PROTO-2) — but proves a NO-OP, not "ships value."** With real rigor (baseline
control, freshness syntax-error confound-kill, negative control, mechanism probe):
`has_parse_time_host_decode` is a **dead name-gate** — `"ws".contains("host")` is always false (the only
annotation the parser builds is `?w` = `{name:"ws",value:None}`); all four `check-*` byte-identical, AND
the *inverted* predicate `value.is_none()` (fires on every `?w`) is ALSO byte-identical — eager-tape
selection does not flow from this predicate at all. Honest hygiene, but the change keyed on
`value.is_some()` — a **shape-sniff on an always-empty slot**, NOT the typed `ExprKind::Host` node. T2
discharged the string-gate *refutation*, not the *construction*. pass-1 open-gap (b) "zero gate executed"
is genuinely CLOSED; the win is thinner than "one-file cure, ships now" reads.

**Status: ADVANCED, the SURVIVOR — the only lock-internal-closeable family.** Ships value *now*
(lock-free trio + the static counter, leaf-evaluator-independent) and supplies the totality% metric
every ladder reports against (feeds 7D's ledger, XP-1). Held below CRIT-1's 70 because the tier
mechanism is unbuilt, T2 was near-vacuous, and the counter — though harness-ready — is **UN-RUN with a
1.0-on-real-corpus caveat**. **Exact open questions:** run UF-2 + demonstrate a non-1.0 floor via the
synthetic Tier-2 injection (the contested flagship); the real `ExprKind::Host` node + T4 `@{}` un-built;
the CONTEXT-tier zero-cost invariant (`resolve_refs∘resolve_refs` no-op + `pretty()` unchanged + TOML
`push_path` residue=0) un-round-tripped; CSS floor unmet until T4; floor ∈ {0,1,2} LOCK-1-pending.

---

## §4 — 7D · META-CIRCULAR-ORACLE — **ADVANCED, the POSTURE winner** — 51%

**Architecture.** Self-hosting as **verification first**: generated parsers run as differential
ORACLES against the hand parsers; promotion is per-subsystem, gated on a *pinned* equivalence relation
+ golden-anchor parity (never majority vote — three parsers can share one bug) + totality% + cold
per-parse perf — never on the mere existence of a generated parser. **ORACLE-ONLY is a lawful terminal**
(CPython pegen ran old ∥ new for a full cycle; the DDC / translation-validation lineage; the
mrustc-rots-without-a-differential-gate pathology motivates it). Two rows in one ledger — a MEASURABLE
regex row, a bbnf row now *precisely bounded* by UF-1.

**The relation, PINNED AND RUN AS CODE.** `ORACLE_REL_V1 = R1∘R2∘R3`: **R1** unit-collapse to a
fixpoint (`Concat([]) ≡ Empty ≡ Alt([])`, `Concat([x]) ≡ x`, `Alt([x]) ≡ x`, `Group(x) ≡ x`; `norm`
idempotent; `Optional↔Repeat{0,1}` OFF — over-quotient hides divergence); **R2** same-stage comparison
(both post-`host::lower`, shared `ParseOptions` — stops a fold bug masquerading as grammar divergence);
**R3** group/class/quantifier field-for-field. **Four planted mutations trip** (greedy→lazy, drop group,
bind-the-`bool`-twin=vacuous→rejected, alt mis-assoc). The bbnf twin `OracleRel-bbnf-v1` runs over
`validate∘resolve_refs`-normalized GrammarIr (same passes both sides or neither — there is no
`ExprKind::Empty`; the empty match IS `Seq(Vec::new())`, so `Empty ≡ Concat([])` is satisfied at zero
cost by construction), homeable as an e-graph over `ExprKind` (LOCK-2 engine, mirroring `simplify_hir`).

**Regex Tier-S — MEASURED ORACLE-ONLY / BLOCKED.** Parity **0.7759 strict (45/58) [report as
primary]** / 0.7931 Empty-normed / 0.7843 committed-51 — all BELOW the 0.80 gate; perf **1.77× slower**.
12 divergences 100% classified into two disjoint buckets: **9× `host.rs` OBLIGATION** — one bug class,
bounded `{n,m}` (`parse_braced_quantifier` receives the span WITH braces and never strips them;
`pre_recognizer_full=true` proves the parser built the SAME tree) → **route to LOCK-5**; **3× oracle
VERDICT** rooted in `regex.bbnf` (`.` as class-literal not dot-metachar; `(?s)` inline-flag becoming a
literal capturing group) → **bank in the oracle ledger, awaiting LOCK-5's fold onto the ratified split**
(XL-C, SETTLED — grammar tier opaque `Regex(StringId)` + engine tier rich `Hir` + string bridge; NOT an
open fork; the synthesis consumes XL-C as fact). Regex fails promotion on TWO independent counts (3
grammar-locus verdicts + perf) — ORACLE-ONLY even after LOCK-5's fold. **0.80 gate provenance UNCITED**
(a latent gate-tuning risk; robust here as all three metrics clear below it; the Empty-norm moves exactly
one case, 45→46/58, so it launders nothing).

**bbnf Tier-R — the record, now precisely bounded (PROTO-1 + PROTO-3 + UF-1).** Both routes presuppose
`general_descent` *recognizes* bbnf, refuted at the leaf floor. **But UF-1 corrects the pass-1→pass-2
overshoot:** it is *not* "bbnf is not even Tier-R." bbnf **is structurally Tier-R at the branch level**
— 52/53 rules leaf-blind isomorphic through the sink-only lowering; the block is **three disjoint
non-branch seams** (§0.3): leaf floor (`match_at`, cross-repo), parser front-end (`?w`-then-`->`;
`@import`-closure), and one `bool_value` materialization misfire (in-lock, bounded). A **structural-only
(L1) oracle** — accept/reject + branch-skeleton isomorphism, leaves opaque — is therefore *promotable
in-lock TODAY* for the branch skeleton, decoupled from the leaf floor. This is the one bbnf-leg oracle
signal that does not ride LOCK-5's engine. (Producer pin retained: bind `regex/mod.rs:34`, never the
`generated.rs:2288` `bool` twin — M3 proved that vacuous.)

**Promotion ledger (the concrete deliverable + its consumer).** Rows (subsystem × tier); columns
`rel_id · binding · parity_pct` (golden-anchored) `· totality_pct` (**from 7C's counter**, XP-1) `·
perf_ratio` (cold paired, parse-stage isolated from `host::lower`) `· fixpoint` (**7A's subject**, XP-2)
`· verdict`. First COMPLETE row: `regex · ORACLE_REL_V1 · parse_generated(->Hir) · 0.7759 · perf=1.77× ·
fixpoint=EMPTY · BLOCKED (ORACLE-ONLY)`. bbnf row: RED at the leaf/production tiers; **the branch-skeleton
sub-row is GREEN-eligible via the L1 structural oracle (UF-1, 52/53 → 53/53 after the bool_lit fix)**.
The `fixpoint` column is fillable via UF-3 without LOCK-5's engine — the regex row is one column short of
complete.

**Status: ADVANCED, the POSTURE winner.** The strongest MEASURED evidence in the lock — but the regex
row is a NEGATIVE (below gate on all three metrics), and the bbnf leg's positive signal is *structural
only*. The single ladder posture (oracle-first; ORACLE-ONLY lawful terminal). **Exact open questions:**
bbnf production/leaf tiers BLOCKED cross-repo; the 3 `regex.bbnf` verdicts LOCK-5-substrate-bound; 0.80
gate provenance uncited; the grammar-driven corpus sampler unbuilt (parity over committed 51/58 only);
the L1 structural oracle proven at 52/53 but not yet closed to 53/53 (the `bool_value` misfire) nor
wired as a CI gate; the front-end `?w`-attach fix lives in the PROTO-3/UF-2 worktrees but not a shared
base (UF-1 had to leaf-elide 2 sites at byte 2343 — a worktree-reconciliation owed).

---

## §5 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

- **INC-1 · 7A big-bang fixpoint ⊥ 7D incremental oracle — COMPOSED except at the cutover posture.**
  Mechanisms compose (7A's fixpoint LAW grafts onto 7D's oracle artifact, decoupled from promotion =
  OPEN-2/XP-2). What collides is the **hand-parser fate**: 7A-as-distinct retires it at cutover; 7D
  keeps it forever as the golden anchor. 7D's retain-forever wins on the measured edge (regex perf
  1.77× slower ⇒ promotion may never fire) → 7A survives ONLY as the graft. Resolved; drove the 7A bank.

- **INC-2 · 7B build-in-grammar ⊥ 7C build-behind-host — RESOLVED-IN-DIRECTION toward 7C, on a LOCK-1
  dial.** Floor 2, irreducible 1, ∈ {0,1,2} pending LOCK-1's inherited-attribute verdict + a
  value-language arena primitive. The two ends of a LOCK-1-parameterized dial, not a fixed answer.

- **INC-3 · 7A codegen-as-trusted-primitive ⊥ 7B Plane-D codegen-as-grammar-plane — tilts 7A's pole,
  STILL UN-INSTRUMENTED as a running experiment.** The static read (DATA≈50%/ESCAPE≈21%) tilts toward
  the trusted-primitive pole, and `general_descent` (hand-Rust walking the grammar) is disk evidence —
  but no probe CONVERTED a blob and measured the escape by RUNNING. Decided-by-reading, not by-running
  (a low-priority pass-3 variant).

- **INC-4 · hand-parser fate — composable; the kernel-carve-out boundary.** Retire-production vs
  keep-CI-oracle compose (the cutover INC-1 shows never fires). 7C orthogonal for T1/T2/T3/T5, coupled
  at T4/T6. The 7C stratum-2↔3 `&mut Ctx` typed seam IS the surviving ladder's kernel-carve-out boundary
  (XP-3) — 7B's "kernels behind a typed call interface" is precisely this seam.

- **INC-5 · the leaf evaluator is a CROSS-REPO BLOCK, not a within-lock choice (the pass-2 headline) —
  now PRECISELY BOUNDED by UF-1.** PROTO-3 measures the bbnf leaf evaluator = a general regex matcher
  `fn match_at(pattern, input, start) -> Option<end>` = **LOCK-5's engine, homed in parse-that** (XL-C
  one-regex-home). **UF-1 refines the shape of the block, not its existence:** the block is *not*
  branch-skeleton poisoning (52/53 leaf-blind isomorphic) but **three disjoint non-branch seams** —
  (i) leaf floor (`match_at`, cross-repo, the only true LOCK-5 dependency); (ii) parser front-end
  (`?w`-then-`->` / `@import`-closure, in-lock, fix authored); (iii) one `bool_value` materialization
  misfire (in-lock, bounded, one named bug). So 7A / 7B-Plane-A / 7D-bbnf are downstream of a cross-repo
  lock **only at seam (i)**; seams (ii)+(iii) are in-lock and closeable now. The omega DAG edge is
  **"INC-5 = leaf-floor(LOCK-5) + front-end(in-lock) + 1 misfire(in-lock); branch skeleton SOUND,"** a
  hard CAP on the *production/leaf* tiers of every bbnf-leg family but NOT on the *branch-structural*
  oracle. LOCK-9/9C's SCANNER-AS-DFA is a *downstream consumer* of the engine (XL-5), NOT an available
  upstream leaf primitive.

**Convergence (not incompatibility), named.** 7A's fixpoint SUBJECT and 7D's relation are the **same
object** — one `ORACLE_REL_V1`, consumed by both (do not double-home). PROTO-2-7D-regex's `@host
resolve_refs : (&mut GrammarIr) -> Result<(),ValidationError>` is simultaneously 7C's first CONCRETE
CONTEXT instance AND the HARD floor a future builder-augment must house — but that builder-augment is
BLOCKED **upstream** at the leaf floor (seam i), so the HARD-floor host is a **DEFERRED consumer**, not
an active bridge this pass (XP-4).

**Lawful gestalt shape (hardened by pass 2 + UF-1).** **7C ships regardless** (lock-free trio + the
static totality% counter — harness written, one run from a banked number) **+ 7D as the single ladder
posture** (oracle-first; regex row ORACLE-ONLY; bbnf branch-skeleton oracle GREEN-eligible in-lock,
production tiers BLOCKED at seam-i only) **+ 7A's fixpoint law grafted onto 7D** (OPEN-2, runnable today
on the regex artifact) **+ 7B banked** (maximal pole BLOCKED-candidate). The bbnf *production* leg rests
on ONE cross-repo gap: LOCK-5's engine-tier `match_at`. Its *structural* leg does not.

---

## §6 — BLOCKED / BLOCKED-candidate register (elegance earns nothing where the primitive is missing)

- **The bbnf leg — PRODUCTION/LEAF tiers BLOCKED (measured), cross-repo; BRANCH-STRUCTURAL tier NOT
  blocked (UF-1).** The production recognizer is BLOCKED on LOCK-5's `match_at` (seam i,
  INC-5/XL-INC5), equal in difficulty to the original problem — a born-RED DAG edge, not a LOCK-7-internal
  patch. **But the branch-skeleton L1 oracle is closeable in-lock** (52/53 today; 53/53 after the bounded
  `bool_value` fix) — the correction UF-1 forces on the pass-1→pass-2 "not even Tier-R" overshoot.

- **7B Plane-D (maximal "generator becomes mostly data") — BLOCKED-candidate.** Missing primitive =
  grammar-plane expression of the ~21% backtracking-control escape (commit-or-rollback over a shadow
  cursor+tape, E0499/E0503-bound), equal in difficulty (it *is* authoring a combinator/VM runtime). The
  bounded form (DATA+WALK-FOLD lifted, ESCAPE a named committed primitive over LOCK-4's core) is not
  blocked but ⊆ {LOCK-3 3C + the leaf-evaluator + builder-augment}. The ratio is a static-read, un-run.

- **7B Plane-B built-IR** and **7C floor finalization** are **LOCK-1/LOCK-3-gated, not blocked**
  (inherited-attribute verdict; floor ∈ {0,1,2}; LOCK-3 3C adoption).

- **7D regex — BLOCKED (measured), not a candidate.** ORACLE-ONLY on two independent counts (3
  grammar-locus verdicts LOCK-5 cannot discharge + the perf gate). Banked, honest, complete save the
  `fixpoint` column (UF-3).

- **NOT blocked (the corrections) — 7C's static totality% counter + lock-free trio (harness written,
  run pending); the bbnf branch-structural L1 oracle (52/53 RAN).** Both run on the BUILT GrammarIr /
  the sink-only lowering — no leaf evaluator, no self-emit, no cross-repo engine.

---

## §7 — The two sharpest uncertainties (prototype briefs, full text in StructuredOutput)

Chosen for maximal advance of the two ADVANCED survivors, both **runnable TODAY without LOCK-5's
engine**, each with a numeric pass/fail. UF-1 has RAN (folded above); UF-2's harness is written but
**never executed**; UF-3 has never been attempted. The two sharpest *remaining* uncertainties:

1. **UF-2 — EXECUTE the static totality% counter and demonstrate a non-vacuous floor** (bbnf-worktree).
   The SURVIVOR family's contested flagship: the harness exists (`UF-2-totality-counter.patch`) but its
   per-subsystem numbers are un-banked, and its own note says the real corpus reads 1.0 exactly. Run it,
   report json/sheets/bbnf-closure numbers, AND exercise the floor via the synthetic Tier-2 injection —
   converting "runnable in principle" to a RAN number that feeds 7D's ledger `totality_pct`.

2. **UF-3 — the fixpoint gate on the self-emitting regex artifact** (bbnf-worktree / pt-scratch).
   7A's never-run core AND 7D's empty `fixpoint` ledger column, in ONE run, WITHOUT LOCK-5's engine —
   the regex parser self-emits today (`generated.rs`). Assert byte-identical `regen∘regen`; plant an
   input-grammar mutation that MUST move the derived bytes (non-vacuity). Fills both columns; discharges
   7A's SPEC_ONLY reduction.

(Held third: closing the bbnf L1 structural oracle to 53/53 via the bounded `bool_value` fix + wiring
it as a CI gate — smaller, already 52/53-answered by UF-1, scheduled after these two.)
