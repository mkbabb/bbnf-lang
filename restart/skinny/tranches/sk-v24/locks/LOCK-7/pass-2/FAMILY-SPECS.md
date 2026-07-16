# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 FAMILY SPECS (synthesis)

Distilled by the SYNTHESIS agent (Fable-tier), pass 2, from the five pass-2 research reports
(A/B/C/D/priorart), the pass-1 REGISTRY standing verdict, and CROSS-LOCK. One cogent spec per
family **while the routes remain incompatible**; then the incompatibility map. Elegance earns
nothing alone: a family stalled at a missing primitive equal in difficulty to the original problem
is marked **BLOCKED-candidate**.

**Base (XL-H).** Every anchor below re-greped on the live skinny tree at **HEAD `c93fc4314`**
(pass-1 folded `fe160a91a`; the base moved, all cited `path:line` re-verified). parse-that anchors
at `/Users/mkbabb/Programming/parse-that`.

---

## §0 — THE SPINE ALL FOUR FAMILIES SHARE (carried facts, re-verified)

1. **A grammar-NEUTRAL descent emitter EXISTS.** `skinny/crates/codegen/src/general_descent/`
   = 1206 LOC. `runtime_generator.rs:98 if !sink_only.has_rule("value")` → `emit_general_descent`.
   `bbnf.bbnf` has **no `value` rule** (greped: zero `^\s*value\s*=`) → it shape-dispatches here,
   the same branch `regen-sheets` travels. It emits **one `parse_<rule>` recognizer per rule**
   (tape offset-marks), **not GrammarIr builders**.
2. **RECOGNIZE ≠ BUILD is the one live gap (OPEN-1').** The residual bbnf obligation is a *bounded
   builder-augment on an extant machine*, not a re-author. Three bounded parser fixes gate routing
   `bbnf.bbnf` in at all: `parse_atom:1495` has no `@{`/`[`/`{` arm (bbnf.bbnf:33-36); `parse_seq:1361`
   mis-attaches `?w` across `->` (bbnf.bbnf:17); `closure = "|" , identifier … "|" … rhs`
   (bbnf.bbnf:51) has no production. `parse_postfix:1409` already handles `->`.
3. **Construction floor = 2, irreducible ∈ {0,1,2}.** Node *values* are pure `->` (floor 0). Two
   build *mechanisms* are host-bound: `add_expr` (`ir/src/lib.rs:51`, append — SOFT, dischargeable
   by a value-language arena primitive) and `resolve_refs` (`ir/src/lib.rs:84`, cross-rule
   name→RuleId TreeFold — HARD). The HARD floor is **LOCK-1-dependent**: `resolve_refs` is an
   *inherited attribute*, host-bound iff LOCK-1 declines in-grammar inherited attributes.
4. **No self-host exists on skinny.** xtask carries `regen-{json,sheets,css,real-typed}` + `check-*`;
   **no `regen-bbnf`, no `fixpoint`** (greped empty). Stage-0 must be *authored*, not transplanted.
5. **The regex self-host already ships as a PRODUCER.** parse-that `regex/mod.rs:34 parse_generated
   -> Result<Hir>` runs the generated parser then `host::lower` (`:57`). Not a recognizer.

**THE MAKE-OR-BREAK, still RED-unknown.** Does authoring `-> Span`/`-> T` builder projections on
bbnf's structural rules **over the recognizer emitter** reparse the corpus into GrammarIr-equivalent
structure (within the +1 SOFT arena / +1 HARD `resolve_refs` host budget) while `regen∘regen` holds
byte-identical — or does it cascade, and at which construct? **PROTO-1 measured the WRONG path
(`json_sink_direct`); this is untested as such.** Every family's headline rests on it (Brief 1).

---

## §1 — 7A · STAGED-BOOTSTRAP — spec

**Architecture.** Classic two-stage self-host: a committed stage-0 artifact + a fixpoint gate
certifying `regen∘regen = regen`, minimal auditable bootstrap surface. On skinny this is *net-new*:
no stage-0, no fixpoint arm exist (§0.4). The family's distinct contribution is the **fixpoint LAW**
(byte-identical `regen∘regen`, no 2-cycle) + a blake3 provenance ledger — not LOC reduction.

**Mechanism / the honest seed headline (research-A §2, decisive).** The seed axis is *not*
recognizer-cheap-vs-building-dear; it is **recognizer-USELESS-vs-building-adequate**. A recognizer
seed (general_descent output + 133-LOC grammar source ≈ 1340 hand LOC) produces a *tape*, not
GrammarIr — vacuous as a compiler front-end, usable only as a 7D oracle. A *building* seed inflates
**past** the 2142-LOC hand parser it would replace: the in-repo witness is parse-that's regex
self-host — `regex/generated.rs` 2293 + `regex/host.rs` 524 LOC for a *strictly smaller* language.
So: **staged-bootstrap buys auditability + a committed fixpoint, at known LOC inflation, not
shrinkage.** The `88s→0.5s` datum is **REFUTED** as a legacy transplant — it was cargo's LTO
*rebuild* (`REAUDIT…/06-throughput-commit.md:241`), regen itself was ~76 ms; drop it.

**Fixpoint SUBJECT (research-A §3 + priorart §1).** `parser_N` mints `parser_{N+1}`; gate =
**cross-generation `.rs` bytes** (`parser_{N+1} ≡ parser_N`) with a **GrammarIr-equivalence floor**
(`pretty(hand_parse(g)) == pretty(gen_parse(g))`, `ir/src/lib.rs:124`) as the non-vacuity witness.
Same-source-same-emitter idempotence is the WEAK sibling and near-vacuous (a fixed-const path stamps
identical bytes for any input). The law from rustc/GHC/kefir/shecc/CPython-pegen: only the *second*
self-application (stage2==stage3) certifies; stage1 differs by uplift.

**Integration seams.** Rescue `regen.rs:473-474` write-skip (content-equality, grammar-NEUTRAL,
~10 LOC, an independent regen win) into skinny `write_targets`; **re-author** the fixpoint subject
(~200-250 LOC) against `emit_runtime_from_request` — do NOT revive the legacy `--output` flag. Add a
`BBNF_TARGET: RuntimeTarget` const mirroring `SHEETS_TARGET` (~15 LOC).

**Gates it carries (born-RED).** (a) `check-bbnf-selfparse`: `parse_grammar(bbnf.bbnf) == Ok` — RED
until the three §0.2 fixes land; falsification = planted malformed `@{ a ;` errors explicitly, never
silent `Epsilon` (`no-silent-epsilon`). (b) cross-generation byte-fixpoint + IR floor, with a
planted-mutation that MUST trip (a fixpoint that survives an input mutation is a fixed-const path,
REJECTED).

**Status: ADVANCED (narrowed).** Its one non-redundant core (the fixpoint law) composes onto 7D
(OPEN-2); as a *distinct* family (big-bang cutover, hand parser retired) it collides with 7D
(§5 INC-1).

**Exact open questions.** The builder-over-recognizer cascade point (Brief 1); whether a *building*
S0's inflation past the hand parser dissolves the auditability the bootstrap exists for.

---

## §2 — 7B · TOTAL-SELF-HOST — spec

**Architecture.** Constitution (i) at maximal reading: bbnf + regex + IR + emitter templates *all*
grammar-specified, the generator reduced to data + a small kernel. Decomposes into a **FRONT**
(bbnf-in-bbnf, regex-in-bbnf) and a **BACK** (IR + emitter as grammar data — the hard axis).

**Axis A — the IR plane (joint LOCK-3 ⊗ LOCK-7).** `BackendExpr` (`ir/src/lib.rs:376`, 13-variant
sum) is a *regular tree grammar* — exactly the shape `bbnf.bbnf` already expresses for its own
`term`/`factor`/`rhs` tower. So **`ir.bbnf` is writable** (each variant = discriminating literal +
typed children → constructor), which IS LOCK-3 route 3C GRAMMAR-HOMOIOUSIA made concrete. Consumer
(clause viii): the general_descent emitter walk as a *tree transducer* over `ir.bbnf`-parsed IR
(`emit_expr` is its executable form). **Gated:** node *shape* lands now; a grammar-*built* IR
re-enters the host floor (§0.3) and lands only once LOCK-1 prices the inherited-attribute tier.
**Conditional on LOCK-3 picking 3C** — else `ir.bbnf` is consumer-less (Y.13). → LOCK-3 adjudication
(XL-1); draft in two stages (shape unblocked / built LOCK-1-gated), never as one monolith.

**Axis B — structure-scoping, MEASURED (research-B §3, the pass-2 datum).** On the one blob
`emit.rs` (452 LOC), region-by-region: **DATA ≈ 50%** (fixed kernel template, already
grammar-derived, commits verbatim as stage-0) · **WALK-FOLD ≈ 20%** (a catamorphism over the IR sum,
`->`-expressible given a tree-fold combinator) · **IRREDUCIBLE ESCAPE ≈ 21%** (`RepeatLoop`,
`OptionalBranch`, `emit_alt`, `emit_expr_probe` — the semantic core, not incidental). **Named
kernel-escape signature:** *commit-or-rollback over a mutable shadow cursor* (PEG ordered-choice) —
an IIFE closure (`emit.rs:448-451`) that is a **borrow-checker escape hatch**: flatten it and the
outer `cursor:&mut usize` + inner `&mut _probe` collide as **E0499**. It resists grammar expression
on two exact counts — value-algebra mismatch (`->` is a pure/total map; the idiom is stateful
`_matched` + partial `?`-early-return) and the E0499 borrow bite. Prior art is uniform (LLVM
TableGen, MLIR ODS, tree-sitter external scanners): **no production system erases the emitter kernel
into data**; the escape *relocates* into a combinator/VM runtime (LOCK-4C), it does not vanish.

**Status: BANKED · maximal claim BLOCKED-candidate.** "The generator becomes mostly data" is
defensible for the ~70% DATA+WALK-FOLD and **false for the ~21% ESCAPE** — and that escape is a
missing primitive (grammar-plane backtracking control) equal in difficulty to the original problem
(it *is* re-authoring a combinator/VM runtime). The honest, bounded form (DATA+WALK-FOLD lifted;
ESCAPE a named committed primitive) is viable but ⊆ {LOCK-3 3C + the shared builder-augment}. 7B is a
**research answer, not a route.** Its scope questions home in LOCK-3 (XL-1) and the Plane-D
data-vs-escape ratio (now measured: refutes the maximal pole).

---

## §3 — 7C · HOST-FUNCTION-LATTICE — spec

**Architecture.** Totality as a *bounded, measured* quantity: a three-tier lattice keyed on the one
structural fact the compiler reads off a signature — **whether it threads `&mut Ctx`**. **PURE**
`fn(Span)->T` (the `->` projections + `@{}` span-capture; counted as grammar coverage; must not
select the eager tape) · **CONTEXT** `fn(&mut Ctx,…)` (build-time mechanisms mutating GrammarIr/tape
— `resolve_refs`, a hypothetical TOML `push_path`; the parser-generator instance of *inherited
attributes*; runs at comptime, O(structure) not O(bytes)) · **RECURSIVE/HOST** (opaque signed
escape, last resort, floored + audited). The tier is **INFERRED from the signature**, not declared —
the concrete answer to pass-1's defeatible `@pure` string; reading the tier off `&mut Ctx` is what
makes the escape impossible to mislabel.

**Mechanism / the independence split (research-C §2, XL-3).** **Lock-free trio** (no LOCK-1):
(1) delete the string heuristic `has_parse_time_host_decode` (`passes/src/lib.rs:~798`,
`name.contains("host") && …"decode"|"parse"` — a latent name-gate, verified live-dead on the fleet →
regen bit-for-bit); (2) lower `@{}` — add `ExprKind::SpanCapture(ExprId)`, a `parse_atom:1495` `@{`
arm, a general-descent lowering (reuse `parse_runtime_host_capture` span logic, `:420`); (3) *name*
`resolve_refs` the CONTEXT-tier floor mechanism. **LOCK-1-bound:** typed `HostFnSig` into inference;
tier-off-`&mut Ctx` as the typed eager-tape trigger; **the floor itself** (`resolve_refs` HARD iff
LOCK-1 declines inherited attributes — floor ∈ {0,1,2}).

**Deliverable + gate.** A **two-axis totality% counter** — LEAVES (`Project`/`SpanCapture`/`Regex`/
`Literal`) × BRANCHES (`Alt`/`Repeat`/`Optional`/`Discard`) — so a host escape hides in neither
dimension; `totality% = (grammar leaves + grammar branches)/(total)`, per subsystem, PURE/CONTEXT/
HOST breakdown, `resolve_refs` the standing CONTEXT floor term. Floor-gate `xtask
check-totality-floor` — a **GN-5.W3 `check-name-gates` sibling** — with planted-breach falsification
(inject a below-floor host leaf → gate trips; inject `@host parse_toml_datetime` name-gate → the AST
walk flags it; real fleet passes clean).

**The binding correction.** The **census/totality% deliverable NEEDS bbnf to parse+build** → rides
the builder-augment (OPEN-1'). "Needs no self-emit" holds for the *gates* (T1/T2/T3/T5), **not the
deliverable**. Orthogonal for T1/T2/T3/T5; coupled at **T4** (shares the `@{}` wall the ladder needs)
and **T6** (`@host` survives `regen∘regen` → rides the bbnf front-end augment).

**CONTEXT-tier zero-cost invariant (testable NOW, scheduling gate).** `resolve_refs` round-trip
(`pretty(resolve_refs(parse(G))) ≡ pretty(parse(G))` + no residual resolution in any hot loop) +
TOML `push_path` cyc/B non-scaling with `payloads().len()==0` held. Tier enters the DAG only after
both pass.

**Status: ADVANCED, the SURVIVOR.** Ships value *now* (T2/T4, no other lock); orthogonal mechanism
the winner consumes; supplies the totality% metric every ladder reports against. Cheapest signal in
the lock (T2: delete one heuristic, run `check-json` bit-for-bit) is **UN-RUN** — highest-ROI missing
probe.

**Exact open questions.** LOCK-1 inherited-attribute verdict (fixes the floor); the census rides
OPEN-1'; CSS floor unmet until T4 lowers `@{}` (`color.bbnf:220`/`values.bbnf:69` depend on it).

---

## §4 — 7D · META-CIRCULAR-ORACLE — spec

**Architecture.** Self-hosting as **verification first**: the generated bbnf/regex parsers run as
differential ORACLES against the hand parsers; promotion to production is gated on a *pinned*
equivalence relation + a measured perf floor, subsystem by subsystem. Divergence is a generator bug
or a grammar-authoring gap — never a spec ambiguity (both sides realize the *same* grammar).

**Mechanism — two tiers.** **Tier-R** (recognizer-acceptance parity) available NOW (general_descent
recognizers; regex `bool` twin `generated.rs:2288`). **Tier-S** (structure-equivalence) available
NOW for **regex** (`parse_generated` producer → `Hir`), **gated on the builder-augment for bbnf**.

**The equivalence relation, pinned (research-D §2, the family's load-bearing act).**
`ORACLE_REL_V1 = R1 ∘ R2 ∘ R3`, a versioned committed constant every parity% cites:
- **R1 — `Seq([]) ≡ Empty ≡ Concat([])`, `Seq([x]) ≡ x`, `Alt([x]) ≡ x`**, bottom-up to a fixpoint,
  BOTH sides (bbnf collapses inline `grammar/src/lib.rs:1394,1400`; regex has `Hir::Empty`,
  `host.rs:44,54,89`) — else hand's collapsed `x` vs a builder's `Seq([x])` diverge for no bug.
- **R2 — shared-semantic-pass pivot:** compare at the SAME stage — both post-`resolve_refs`+`validate`
  or both pre (`ir/src/lib.rs:84,110`). Makes the oracle test the *parser*, not the *linker*.
- **R3 — projection/`->` preservation** (no-value-discard): `Project` compared field-for-field;
  makes Tier-S strictly stronger than Tier-R.
The extant JSON parity oracle (`bbnf-bench/src/parity.rs:23`) is the working R template (offset
stream + payload-zero invariant + canonical key-sorted string).

**Regex divergences partitioned (XL-C, XL-2).** **Engine-tier `host.rs` OBLIGATIONS** (deterministic
folds — fix, do NOT bank: `lower_escape`, `lower_quantifier`, `apply_flags`) vs **oracle VERDICTS**
(genuine pre-fold parser divergence — the ledger's domain). **Namespace hazard:** bind the
`regex/mod.rs:34` producer (→`Hir`) for Tier-S, NOT the `generated.rs:2288` `bool` twin (→ vacuous
100%). The measured edge: paired cold regex oracle, structural parity **0.76 (0.80 Empty-normed)**,
perf GEN/HAND **1.77–1.81× slower** → **ORACLE-ONLY**; 9/12 divergences are ONE `host.rs` bug class
(bounded `{n,m}`) = **LOCK-5's incomplete fold**, not an oracle verdict.

**Promotion ledger.** Rows (subsystem × tier); columns `rel_id`, `binding` (exact producer symbol —
anti-vacuity pin), `parity_pct` (golden-anchored, NOT majority-vote — DDC/Wheeler), `totality_pct`
(**from 7C's counter**, XP-1), `perf_ratio` (cold paired PMU), `fixpoint` (**7A's subject**, XP-2),
`state`. `PROMOTED` ⟺ Tier-S parity=100 ∧ perf≤1.0 ∧ fixpoint ∧ totality≥floor; `ORACLE-GREEN` =
Tier-S 100% with perf>1.0. Planted-mutation falsification (4 forms) must trip before any green is
trusted.

**Status: ADVANCED, the POSTURE winner.** The lawful entry ramp — an emitted parser's first consumer
is the differential oracle, curing Y.13 the instant a parser exists; ORACLE-ONLY is a lawful
terminal. "gen" side buildable NOW at recognizer/span granularity; L2 Tier-S bbnf gated on OPEN-1'.

**Exact open questions.** L2 oracle strength rides OPEN-1'; the canonical regex `.bbnf` is
LOCK-5-substrate-fork-blocked (XL-2/XL-7); the corrected Empty-normed regex parity + LOCK-5-vs-oracle
partition is the first COMPLETE ledger row (Brief 2).

---

## §5 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

**INC-1 · 7A big-bang fixpoint ⊥ 7D incremental oracle — RESOLVES to composition, EXCEPT at the
cutover posture.** The two *mechanisms* compose: graft 7A's fixpoint LAW onto 7D's oracle artifact,
**decoupled from promotion** (fixpoint over the generated artifact needs neither perf≥1.0 nor
parity=1.0, only byte-identical re-emit; reuse `regen.rs:37`) = OPEN-2/XP-2. What genuinely collides
is the **hand-parser fate**: 7A-as-distinct-family retires the hand parser at cutover; 7D keeps it
forever as the golden anchor (DDC's independent second path). **You cannot both retire and retain the
hand parser.** Adjudication: 7D's retain-forever wins on the measured edge (regex perf 1.8× slower →
promotion may never fire), so 7A survives *only as the fixpoint-law graft*, not as a cutover route.

**INC-2 · 7B build-in-grammar ⊥ 7C build-behind-host — RESOLVED-IN-DIRECTION toward 7C, on a
LOCK-1 dial.** The census (§0.3) is host-bound: floor 2, irreducible 1. But floor ∈ {0,1,2} pending
LOCK-1's inherited-attribute verdict + a value-language arena primitive. If LOCK-1 admits inherited
attributes, `resolve_refs` becomes grammar-expressible and 7B's build-in-grammar **reopens for that
edge**. They are incompatible only under the demand for ONE fixed answer; correctly, they are the two
ends of a **LOCK-1-parameterized dial**. Record: resolved-in-direction (7C), not "open," not "fixed."

**INC-3 · 7A codegen-as-trusted-primitive ⊥ 7B Plane-D codegen-as-grammar-plane — RESOLVED toward
7A's pole (owed a named supersession).** The pass-2 measurement decides it: DATA ≈50% / ESCAPE ≈21%
on `emit.rs`, the escape borrow-checker-bound (E0499). `general_descent` (hand-Rust walking the
grammar) is disk evidence *for* the trusted-primitive pole. The grammar-plane maximal claim is
**refuted for the ~21% escape**; codegen control-flow is a trusted, committed kernel primitive. 7B's
Plane-D maximal pole is a **BLOCKED-candidate** (missing primitive = grammar-plane backtracking).

**INC-4 · hand-parser fate — composable, and the kernel-carve-out boundary.** Retire-production vs
keep-CI-oracle compose (7A retires at a cutover that INC-1 shows never fires; 7D keeps). 7C is
orthogonal for T1/T2/T3/T5, coupled at T4/T6. The **7C stratum-2↔3 typed seam IS the surviving
ladder's kernel-carve-out boundary** (XP-3): 7B's "kernels behind a typed call interface" = 7C's
`&mut Ctx` seam, consumed by 7D as its primitive line.

**Convergence (not incompatibility), named.** The 7A fixpoint SUBJECT and the 7D relation are the
**same object**: 7A's GrammarIr-equivalence floor *is* 7D's `ORACLE_REL_V1` (`ir/src/lib.rs:124`
pretty-normal-form). Both reports independently land "bytes as CI gate + IR-equivalence as semantic
floor." Do not double-home it — one `ORACLE_REL_V1`, consumed by both the 7A fixpoint gate and the
7D oracle. (XP-4: PROTO-2's `@host resolve_refs : (&mut GrammarIr) -> Result<(),ValidationError>` is
simultaneously 7C's first CONCRETE CONTEXT/TreeFold instance AND the HARD floor the OPEN-1'
builder-augment must house — 7C's floor census IS the ladder's builder-augment host budget.)

**Lawful gestalt shape (hardened, unchanged from pass 1 and reinforced here).** **7C ships
regardless** (orthogonal mechanism + the totality% metric every ladder reports against) **+ 7D as
the single ladder posture** (oracle-first, ORACLE-ONLY the lawful terminal) **+ 7A's fixpoint law
grafted onto 7D** (OPEN-2/XP-2, decoupled from promotion) **+ 7B banked** (scope questions homed in
LOCK-3, maximal pole BLOCKED-candidate). The whole ladder rests on ONE disk-located gap: the
**builder-augment on `general_descent`** (OPEN-1').

---

## §6 — BLOCKED-candidate register

- **7B Plane-D (maximal "generator becomes mostly data").** Missing primitive = grammar-plane
  expression of the ~21% backtracking-control escape (commit-or-rollback over a shadow cursor,
  E0499-bound). Equal in difficulty to the original problem — it *is* authoring a combinator/VM
  runtime (LOCK-4C). BLOCKED-candidate; the bounded form (DATA+WALK-FOLD lifted, ESCAPE named
  primitive) is not blocked but is ⊆ {LOCK-3 3C + builder-augment}.
- **7B Axis-A built-IR** and **7C floor finalization** are **LOCK-1-gated, not blocked**: they turn
  on the inherited-attribute verdict (floor ∈ {0,1,2}), a dependency, not a missing primitive.
- **7D bbnf Tier-S** is **OPEN-1'-gated, not blocked**: buildable the instant the builder-augment
  round-trips; if it cascades, bbnf stays Tier-R + the cascade construct is the named negative
  result — honest, not blocked.

---

## §7 — The two sharpest uncertainties (prototype briefs returned via StructuredOutput)

1. **OPEN-1' — the builder-augment cascade** (bbnf-worktree). The make-or-break every family rests
   on; genuinely RED-unknown; decides 7A build-meaning, 7B Plane-A parity, 7C census%, 7D L2 jointly.
2. **7D regex Tier-S under pinned `ORACLE_REL_V1`** (pt-scratch). Runnable NOW (producer ships);
   pins the relation-id all four ledgers cite; partitions divergences into LOCK-5 `host.rs`
   obligations vs oracle verdicts (resolves XL-2 routing); decides regex promotable-vs-ORACLE-ONLY.
