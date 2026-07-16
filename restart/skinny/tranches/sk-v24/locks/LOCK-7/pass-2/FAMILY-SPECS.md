# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 FAMILY SPECS (synthesis, post-fold re-cut)

Distilled by the SYNTHESIS agent (Fable-tier), pass 2, from the five pass-2 research reports
(A/B/C/D/priorart), **all four pass-2 prototypes that RAN** — `PROTO-1` (OPEN-1′ builder-augment
→ refuted at the leaf lexer), `PROTO-3-leaf-evaluator` (leaf evaluator → **MISSING PRIMITIVE**,
cross-repo), `PROTO-2`/7C-T2 (dead-heuristic delete → **PASS, near-vacuous**), and the regex
Tier-S oracle (→ **ORACLE-ONLY**) — the LOCK-7 REGISTRY standing verdict, CRIT-1/CRIT-2, and
CROSS-LOCK. One cogent spec per family **while the routes remain incompatible**; then the
incompatibility map. Elegance earns nothing alone: a family stalled at a missing primitive equal
in difficulty to the original problem is marked **BLOCKED-candidate**.

**Base (XL-H).** Prototypes ran base-pinned (`c93fc4314` / `ce51c36fc`, both confirmed ancestor
of the pin). Every `path:line` re-greped on the live skinny tree (`skinny/crates/…`, grammar
corpus `grammar/…`); parse-that anchors at `/Users/mkbabb/Programming/parse-that`.

> **This re-cut supersedes the 15:12 draft of this file (BREACH-1).** That draft froze thirteen
> minutes before PROTO-3 (15:25) and 7C-T2 (15:26) ran; it narrated the leaf evaluator as an open
> "Brief A" and 7C-T2 as "STILL UN-RUN." Both have now RAN and RESOLVED the lock's central
> questions in the **hard** direction. This file folds them, aligned with the REGISTRY. Two owed
> housekeeping items carried: **BREACH-2** — the regex Tier-S report-of-record was clobbered by the
> 7C-T2 report at `PROTO-2.md`; re-home under `PROTO-2-7D-regex.md` before GESTALT (H-1). Dead-ID:
> the ledger keys are **XL-A…XL-H**; earlier `XL-1/XL-2` citations re-point to **XL-B/XL-C**.

---

## §0 — THE SPINE ALL FOUR FAMILIES SHARE (measured, not asserted)

1. **A grammar-NEUTRAL descent emitter EXISTS** — `skinny/crates/codegen/src/general_descent/`
   (**1206 LOC**: mod 93 / emit 452 / regex_first 230 / view 431). `runtime_generator.rs:98
   !has_rule("value")` → `emit_general_descent`. `bbnf.bbnf` has no `value` rule → it shape-dispatches
   here, the branch `regen-sheets` travels. It emits **one `parse_<rule>` recognizer per rule**
   (offset-mark tape + lazy `{Prefix}Document` view), **not GrammarIr builders** (`grep
   GrammarIr\|add_expr\|ExprKind general_descent/` = EMPTY).

2. **THE HEADLINE — the emitter is grammar-neutral at the BRANCH level, grammar-SPECIFIC at the
   LEAF level, and the leaf fix is a primitive that DOES NOT EXIST (PROTO-1 + PROTO-3, RAN).** Its
   leaf layer (`regex_first::first_bytes` + `emit.rs::{match_leaf, consume_quoted, is_text_byte}`) is
   a Sheets/JSON delimiter-scan approximation. Measured against bbnf's real leaves it **hard-fails at
   the first regex literal**: `split_top_level_alternation:90` splits at paren-depth 0 only → bbnf's
   `(\.|[^\/])+` collapses to `first={'.'}` → the `match_leaf` head-gate REJECTS every `/…/` literal;
   `consume_quoted` honors doubled-quote not backslash escapes; `is_text_byte` excludes `-` /
   includes `.$`. **PROTO-3 then decided the sharp question — bounded arm-swap or missing primitive:
   MISSING PRIMITIVE.** The hypothesized "route `/regex/` leaves through the extant compiled matcher"
   **does not exist** — the grammar tier holds an *opaque interned pattern string*
   (`ir::BackendExpr::RegexProgram{pattern:String}`, a misnomer, no program); the engine tier
   (`bbnf-regex`) is a **4-bucket classifier** (`{Whitespace,QuotedString,Numeric,Unknown}`) with
   **no consume/match method** — all 7 bbnf named-rule leaves classify `Unknown` (0/7 tokenize);
   `parse-that-regex` is delimiter-fixed JSON scanners. **Grep `struct (Dfa|Nfa|Program|Instruction)|
   transition_table|fn match_at` in `skinny/crates` = EMPTY.** So the bbnf leaf evaluator = **author a
   general regex-execution engine** = **LOCK-5's cross-repo deliverable (INC-5)**.

3. **Construction floor = 2, irreducible ∈ {0,1,2}** (LOCK-1-dependent; not reached by pass-2
   probes). Node *values* pure `->` (floor 0); `add_expr` (`ir/src/lib.rs:51`, append — SOFT) +
   `resolve_refs` (`ir/src/lib.rs:84`, cross-rule name→RuleId — HARD, an *inherited attribute*,
   host-bound iff LOCK-1 declines in-grammar inherited attributes).

4. **No self-host exists on skinny (re-greped).** xtask carries `regen/check-{json,sheets,css,
   real-typed}` (`main.rs:40-47`); **no `regen-bbnf`, no `fixpoint`, no `BBNF_TARGET`.** Stage-0 must
   be *authored*, not transplanted. The `88s→0.5s` datum is a **REFUTED legacy transplant** (cargo
   LTO rebuild-avoidance of a monolithic `include!`, not regen/parse; archives at GN-0).

5. **The regex self-host ships as a PRODUCER, MEASURED ORACLE-ONLY (regex Tier-S, RAN).** parse-that
   `regex/mod.rs:34 parse_generated -> Result<Hir>` (generated parser then `host::lower`). Under
   pinned `ORACLE_REL_V1`: parity **0.7759 strict (45/58)** / 0.7931 Empty-normed / 0.7843
   committed-51 — **all BELOW the 0.80 gate**; GEN **1.77× slower**. 12/12 divergences classified
   (9× `host.rs` bounded-`{n,m}` obligation → LOCK-5; 3× `regex.bbnf` verdict → oracle bank); 4
   planted mutations all trip. Details §4.

6. **THE CORRECTION both critics missed (largest of the pass) — 7C's totality% counter is a STATIC
   GrammarIr walk, NOT a self-emit census, and is RUNNABLE TODAY.** research-C §3 defines it verbatim:
   walk the built `grammar.exprs` arena, classify LEAVES×BRANCHES × PURE/CONTEXT/HOST. GrammarIr for
   **json/sheets/real-typed exists today** (the hand parser builds it); **bbnf's own GrammarIr exists
   too** — PROTO-3 measured `parse_grammar` over the bbnf **import-closure** = **Ok, 53 rules,
   resolve_refs clean** (behind only the landed Wall-0 `?w`-fix). The counter needs the **build** step
   (available), NOT the **emit** step (blocked). Only the *self-hosted-coverage* variant needs
   self-emit. This is the pivot that lifts 7C off the critic floor.

**The unifying pass-2 fact.** The whole **bbnf leg** (7A recognizer/building seed, 7B Plane-A,
7D-bbnf Tier-R/S) rests on ONE cross-repo gap — **LOCK-5's engine-tier `match_at`** (INC-5). No
bbnf-leg family converges inside LOCK-7 alone. **What does converge in-lock:** 7C's lock-free trio
+ the static totality counter, and 7D's regex ORACLE-ONLY negative. The bbnf leg's pass-1→pass-2
trajectory is a **reachability regression** (builder-augment → leaf lexer → general regex engine,
cross-repo) even as design clarity rose — **the % tracks reachability, not clarity.**

---

## §1 — 7A · STAGED-BOOTSTRAP — **BANKED** — 40%

**Architecture.** Classic two-stage self-host: a committed stage-0 + a fixpoint gate certifying
`regen∘regen = regen`, minimal auditable bootstrap surface. Net-new on skinny (§0.4). Its one
non-redundant asset is the **fixpoint LAW** (byte-identical `regen∘regen`, no 2-cycle) + a blake3
provenance ledger — not LOC reduction.

**Mechanism / the seed headline (research-A §2 + PROTO-1/3).** The seed axis is
*recognizer-useless-vs-building-adequate*, and the probes remove even the recognizer consolation:
`general_descent` **cannot recognize bbnf** until the leaf evaluator lands, which is LOCK-5's engine
(§0.2). A *building* seed, if reached, inflates **past** the 2142-LOC hand parser (in-repo witness:
parse-that regex self-host = `generated.rs` 2293 + `host.rs` 524 LOC for a *smaller* language),
dissolving the auditability that is 7A's whole selling point.

**Fixpoint SUBJECT (research-A §3).** Cross-generation `.rs` bytes (the rustc/GHC/pegen
`stage2==stage3` law) with a **GrammarIr-equivalence floor** as the non-vacuity witness — and that
floor **IS the same object as 7D's `ORACLE_REL_V1`** (do not double-home). Decide decisively: the
GrammarIr floor, with bytes a supplementary artifact non-vacuous only under a planted input-mutation
that MUST move the bytes.

**Integration seams.** A `BBNF_TARGET` mirroring `SHEETS_TARGET` (`main.rs:200`); re-author the
fixpoint arm against `emit_runtime_from_request`; reuse `regen.rs:37 check_target` for the
byte-gate. **Do NOT** rescue the legacy bespoke emitter cohort (a NAMED overfit refutation).

**Gates (born-RED).** (a) `check-bbnf-selfparse` — RED, cleared by the landed Wall-0 `?w`-fix for
the import-closure. (b) leaf-evaluator gate — **BLOCKED cross-repo** on LOCK-5 (§0.2). (c)
cross-generation byte-fixpoint + IR floor + planted-mutation non-vacuity — **NEVER RUN on any
artifact** (SPEC_ONLY), **but runnable TODAY on the regex oracle artifact** (the regex parser
self-emits); the REGISTRY homes this run as UF-3.

**Why BANKED.** After the fixpoint-law graft (→7D, OPEN-2/XP-2) and the shared-relation convergence
(its GrammarIr floor ≡ 7D's `ORACLE_REL_V1`), 7A owns **nothing 7C+7D do not**. Its one concrete
pass-2 gain (Wall-0 `?w`-attach fix, +12 lines, json ERR→Ok) is a *prerequisite of the 7D/leaf
leg*, not a 7A deliverable. **Open (survival test):** name one thing 7A owns post-graft, or it
RETIRES; the fixpoint gate is fillable via UF-3 without LOCK-5's engine.

---

## §2 — 7B · TOTAL-SELF-HOST — **BANKED** — 32%

**Architecture.** Constitution (i) maximal: bbnf + regex + IR + emitter templates *all*
grammar-specified. Decomposes into planes with distinct verdicts.

**Plane-A — front-end self-host (`ir.bbnf`→`build_grammar_ir(view)`).** **BLOCKED cross-repo** — the
same leaf primitive as 7A/7D-bbnf (§0.2), not "undercut."

**Plane-B — the IR plane (`ir.bbnf`→`ExprKind`), joint LOCK-3 ⊗ LOCK-7 (XL-B).** `ExprKind`
(9 arms, `ir/src/lib.rs:234`) is a regular tree grammar → `ir.bbnf` is *writable* as inert data
(LOCK-3 3C GRAMMAR-HOMOIOUSIA). Lawful default is **narrow-first**: `ir.bbnf` *generates* the hand
enum (the in-tree `egraph-derive` / memory `derive-language-macro` precedent), consumers (1)
`add_expr` (2) `resolve_refs`/`validate` (3) the `lower_expr` fold reading a real Rust type.
**Conditional on LOCK-3 picking 3C; else consumer-less (Y.13).**

**Plane-D — emitter templates as grammar data (measured on `emit.rs`, 452 LOC).** **DATA ≈ 50%**
(fixed kernel template, grammar-derivable) · **WALK-FOLD ≈ 20%** (a catamorphism over the
`SinkOnlyExpr` sum, `->`-expressible) · **IRREDUCIBLE ESCAPE ≈ 21%** — the speculative-cursor
reborrow: `let cursor = &mut _probe` inside an IIFE, committed on `Ok` (15–19 sites; Alt `:400`,
Optional `:344`, RepeatLoop `:311`). **Named kernel-escape:** transactional commit-or-rollback over
a mutable shadow cursor+tape; delete the `&mut _probe` re-alias → **E0499/E0503** borrow-fail. Prior
art is uniform (TableGen, MLIR ODS, tree-sitter): no production system erases the emitter kernel —
it *relocates* into a combinator/VM runtime (LOCK-4C).

**Status: BANKED · maximal pole BLOCKED-candidate.** "Generator becomes mostly data" holds for the
~70% DATA+WALK-FOLD, **false for the ~21% ESCAPE** — a missing primitive (grammar-plane
backtracking) equal in difficulty to the original problem (it *is* authoring a VM). **Caveat both
critics pressed:** the 50/20/21 split is a **static line-count read (research-B §3), never RUN**;
the E0499 is *checkable* (delete the re-alias, observe the compile-fail) but was asserted, not shown
as a RAN compile-fail — the twice-un-run experiment (~9% of `emit.rs` also unclassified). 7B is a
**research answer, not a route**; scope questions home in LOCK-3 (XL-B).

---

## §3 — 7C · HOST-FUNCTION-LATTICE — **ADVANCED, the SURVIVOR** — 57%

**Architecture.** Totality as a *bounded, measured* quantity: a three-tier lattice keyed on one
structural fact read off a signature — **whether it threads `&mut Ctx`**. **LEAF-DECODE** (pure,
`->`-adjacent; counts as grammar coverage) ⊑ **CONTEXT** (`&mut Ctx` — `resolve_refs`, TOML
`push_path`, `?w`; the parser-generator instance of *inherited attributes*; comptime, zero residual)
⊑ **CROSS-RULE** (whole-grammar TreeFold — `resolve_refs`/`validate`, target inlined at codegen). The
tier is **DERIVED from the signature**, not declared — the concrete answer to pass-1's defeatible
`@pure` string. Totality prefers the *lowest* tier that expresses a leaf (a `->`-able leaf shoved
into a host is the GN-5 W2 R2 name-gate-in-disguise the counter must catch by measurement).

**Independence split (research-C §2, XL-3).** **Lock-free** (no LOCK-1): (1) delete the string
heuristic `has_parse_time_host_decode` (`passes/src/lib.rs:794`; keys on `name.contains("host") &&
…"decode"|"parse"` — a latent, DEAD name-gate) → typed predicate over the node's tier; (2) lower
`@{}` — add `ExprKind::Host` (the *real* typed node) + a `parse_atom:1495` `@{` arm, flip
`RuntimeConstructKind::HostCapture` off `"BBNF-UNSUPPORTED-HOST-CAPTURE"` (`grammar/src/lib.rs:222`);
(3) *name* `resolve_refs` the CROSS-RULE floor (designation, no code); (4) **run the static totality
counter** (§0.6). **LOCK-1-bound:** typed `HostFnSig` into inference (T1); tier-off-`&mut Ctx` (T5);
the floor itself (∈ {0,1,2}).

**Deliverable + gate.** A two-population totality% counter — LEAVES (`Literal`+`Regex`+`Ref`
terminals) × BRANCHES (`Alt` arms + `Project` targets) — per subsystem, PURE/CONTEXT/HOST breakdown;
floor-gate `xtask check-totality` (a GN-5 W3 `check-name-gates` sibling, `syn`-backed AST audit) with
planted-breach falsification (plant a spurious CONTEXT host over a plain leaf → assert the ratio
drops and the floor trips; real fleet passes clean). **RUNNABLE NOW over json/sheets/real-typed** on
the built GrammarIr; CSS deferred until T4 (`@{}` lowering); bbnf via its import-closure GrammarIr.

**7C-T2 RAN and PASSED (PROTO-2) — but proves a NO-OP, not "ships value."** With real rigor
(baseline control, freshness syntax-error confound-kill, negative control, mechanism probe):
`has_parse_time_host_decode` is a **dead name-gate** — `"ws".contains("host")` is always false (the
only annotation the parser builds is `?w` = `{name:"ws",value:None}`); all four `check-*`
byte-identical, AND the *inverted* predicate is also byte-identical (eager-tape selection does not
flow from it at all). Honest hygiene (removes a latent name-gate), but the change keyed on
`value.is_some()` — a **shape-sniff on an always-empty slot**, NOT the typed `ExprKind::Host` node.
T2 discharged the string-gate *refutation*, not the *construction*.

**Status: ADVANCED, the SURVIVOR — the only lock-internal-closeable family.** Ships value *now*
(lock-free trio + the static counter, leaf-evaluator-independent); supplies the totality% metric
every ladder reports against (feeds 7D's ledger, XP-1). Held below CRIT-1's 70 because the tier
mechanism is unbuilt and T2 was near-vacuous. **Open:** the static counter is runnable but **UN-RUN**
(the corrected flagship — Brief below); the real `ExprKind::Host` node + T4 `@{}` un-built; the
CONTEXT-tier zero-cost invariant (`resolve_refs∘resolve_refs` no-op + TOML `push_path` residue=0)
un-round-tripped; CSS floor unmet until T4; floor ∈ {0,1,2} LOCK-1-pending.

---

## §4 — 7D · META-CIRCULAR-ORACLE — **ADVANCED, the POSTURE winner** — 51%

**Architecture.** Self-hosting as **verification first**: generated parsers run as differential
ORACLES vs the hand parsers; promotion is gated on a *pinned* equivalence relation + golden-anchor
parity + totality% + cold-per-parse perf — never on the mere existence of a generated parser, never
on majority vote. **ORACLE-ONLY is a lawful terminal** (the field's strongest precedent: CPython
pegen ran old ∥ new for a full cycle).

**The relation, PINNED AND RUN AS CODE (the family's load-bearing act).** `ORACLE_REL_V1 = R1∘R2∘R3`:
**R1** unit-collapse to a fixpoint (`Concat([]) ≡ Empty ≡ Alt([])`, `Concat([x]) ≡ x`, `Alt([x]) ≡
x`; `norm` verified idempotent; `Optional↔Repeat{0,1}` OFF by default — over-quotient hides
divergence); **R2** same-stage comparison (both post-`host::lower`, shared `ParseOptions`); **R3**
group/class/quantifier field-for-field. All **four planted mutations trip** (greedy→lazy, drop group,
bind-the-`bool`-twin=vacuous→rejected, alt mis-assoc) — proven able to fail. The bbnf twin:
`OracleRel-bbnf-v1` over `validate∘resolve_refs`-normalized GrammarIr (same passes both sides or
neither), homeable as an e-graph over `ExprKind` (LOCK-2 engine, mirroring `simplify_hir`).

**Regex Tier-S — MEASURED ORACLE-ONLY / BLOCKED.** Parity **0.7759 strict (45/58)** [report as
primary] / 0.7931 Empty-normed / 0.7843 committed-51 — BELOW the 0.80 gate; perf **1.77× slower**.
12 divergences 100% classified: **9× `host.rs` OBLIGATION** — one bug class, bounded `{n,m}`
(`parse_braced_quantifier` receives the span WITH braces and never strips them → `u32::parse` fails;
`pre_recognizer_full=true` proves the parser built the SAME tree) → **route to LOCK-5**; **3× oracle
VERDICT** rooted in `regex.bbnf` (`.` as class-literal not dot-metachar; `(?s)` inline-flag becomes a
literal capturing group) → **bank in the oracle ledger, awaiting LOCK-5's fold onto the ratified
split** (XL-C, SETTLED — grammar tier opaque `Regex(StringId)` + engine tier rich `Hir` + string
bridge; NOT an open fork). Regex fails promotion on TWO independent counts (3 grammar-locus verdicts
+ perf) — ORACLE-ONLY even after LOCK-5's fold. **0.80 gate provenance UNCITED** (a latent
gate-tuning risk; robust here as all three metrics clear below it).

**bbnf Tier-R AND Tier-S — hard-BLOCKED cross-repo (PROTO-1+PROTO-3).** Both routes presuppose
`general_descent` *recognizes* bbnf, refuted at the leaf floor — bbnf is **not even Tier-R**. Rides
LOCK-5's `match_at` (INC-5). (Namespace pin retained: bind the `regex/mod.rs:34` producer, never the
`generated.rs:2288` `bool` twin — M3 proved that vacuous.)

**Promotion ledger.** Rows (subsystem × tier); columns `rel_id · binding · parity_pct` (golden-
anchored) `· totality_pct` (**from 7C's counter**, XP-1) `· perf_ratio` (cold paired, parse-stage
isolated from `host::lower`) `· fixpoint` (**7A's subject**, XP-2) `· state`. First COMPLETE row:
`ORACLE_REL_V1 · parse_generated(->Hir) · 0.7759 · perf=1.77× · fixpoint=EMPTY · BLOCKED
(ORACLE-ONLY)`. The `fixpoint` column is fillable via UF-3 without LOCK-5's engine.

**Status: ADVANCED, the POSTURE winner.** The strongest MEASURED evidence in the lock — but it is a
NEGATIVE (regex below gate on all three metrics). The single ladder posture. **Open:** bbnf Tier-R/S
BLOCKED cross-repo; the grammar-driven corpus sampler unbuilt (parity over the committed 51/58 only,
generality unmeasured); the structural-only (leaf-blind) bbnf oracle un-tried (Brief below).

---

## §5 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

- **INC-1 · 7A big-bang fixpoint ⊥ 7D incremental oracle — COMPOSED except at the cutover posture.**
  The mechanisms compose (7A's fixpoint LAW grafts onto 7D's oracle artifact, decoupled from
  promotion = OPEN-2/XP-2). What collides is the **hand-parser fate**: 7A-as-distinct retires it at
  cutover; 7D keeps it forever as the golden anchor. 7D's retain-forever wins on the measured edge
  (regex perf 1.77× slower ⇒ promotion may never fire) → 7A survives ONLY as the graft. Resolved;
  drove the 7A bank.

- **INC-2 · 7B build-in-grammar ⊥ 7C build-behind-host — RESOLVED-IN-DIRECTION toward 7C, on a
  LOCK-1 dial.** Floor 2, irreducible 1, ∈ {0,1,2} pending LOCK-1's inherited-attribute verdict + a
  value-language arena primitive. Not a fixed answer — the two ends of a LOCK-1-parameterized dial.

- **INC-3 · 7A codegen-as-trusted-primitive ⊥ 7B Plane-D codegen-as-grammar-plane — tilts 7A's
  pole, STILL UN-INSTRUMENTED as a running experiment.** The static read (DATA≈50%/ESCAPE≈21%) tilts
  toward the trusted-primitive pole, and `general_descent` (hand-Rust walking the grammar) is disk
  evidence for it — but no probe CONVERTED a blob and measured the escape by RUNNING. Fork decided
  by-reading, not by-running (low-priority pass-3 variant).

- **INC-4 · hand-parser fate — composable; the kernel-carve-out boundary.** Retire-production vs
  keep-CI-oracle compose (the cutover INC-1 shows never fires). 7C orthogonal for T1/T2/T3/T5,
  coupled at T4/T6. The 7C stratum-2↔3 `&mut Ctx` typed seam IS the surviving ladder's
  kernel-carve-out boundary (XP-3) — 7B's "kernels behind a typed call interface" is precisely this
  seam.

- **INC-5 · the leaf evaluator is a CROSS-REPO BLOCK, not a within-lock choice (the pass-2
  headline).** PROTO-3 measures the bbnf leaf evaluator = a general regex matcher `fn match_at(
  pattern, input, start) -> Option<end>` = **LOCK-5's engine, homed in parse-that** (XL-C
  one-regex-home). So 7A / 7B-Plane-A / 7D-bbnf are **downstream of a cross-repo lock** — a **hard
  CAP on convergence %** for every bbnf-leg family and a **born-RED DAG edge the omega gestalt must
  carry** (XL-INC5). LOCK-9/9C's SCANNER-AS-DFA is a *downstream consumer* of that engine (XL-5), NOT
  an available upstream leaf primitive.

**Convergence (not incompatibility), named.** 7A's fixpoint SUBJECT and 7D's relation are the **same
object** — one `ORACLE_REL_V1`, consumed by both (do not double-home). PROTO-2-7D-regex's `@host
resolve_refs : (&mut GrammarIr) -> Result<(),ValidationError>` is simultaneously 7C's first CONCRETE
CROSS-RULE instance AND the HARD floor a future builder-augment must house — but that builder-augment
is BLOCKED **upstream** at the leaf floor, so the HARD-floor host is a **DEFERRED consumer** (behind
LOCK-5's engine), not an active bridge this pass (XP-4).

**Lawful gestalt shape (hardened by pass 2).** **7C ships regardless** (lock-free trio + the *static*
totality% counter, runnable now over 3/4 subsystems) **+ 7D as the single ladder posture**
(oracle-first; regex row banked ORACLE-ONLY; bbnf rows BLOCKED cross-repo) **+ 7A's fixpoint law
grafted onto 7D** (OPEN-2, decoupled from promotion, runnable today on the regex artifact) **+ 7B
banked** (maximal pole BLOCKED-candidate). The whole bbnf leg rests on ONE cross-repo gap: LOCK-5's
engine-tier `match_at`.

---

## §6 — BLOCKED-candidate / BLOCKED register

- **The bbnf leg (7A recognizer/building seed · 7B Plane-A · 7D-bbnf Tier-R/S) — BLOCKED (measured),
  cross-repo.** Not a candidate: PROTO-3 grep-verified there is no general regex matcher at any tier
  in `skinny/crates`. The missing primitive is LOCK-5's engine `match_at` (INC-5/XL-INC5), equal in
  difficulty to the original problem. A born-RED DAG edge, not a LOCK-7-internal patch.

- **7B Plane-D (maximal "generator becomes mostly data") — BLOCKED-candidate.** Missing primitive =
  grammar-plane expression of the ~21% backtracking-control escape (commit-or-rollback over a shadow
  cursor+tape, E0499/E0503-bound). Equal in difficulty (it *is* authoring a combinator/VM runtime).
  The bounded form (DATA+WALK-FOLD lifted, ESCAPE a named committed primitive) is not blocked but ⊆
  {LOCK-3 3C + the leaf-evaluator + builder-augment}. Ratio is static-read, un-run.

- **7B Plane-B built-IR** and **7C floor finalization** are **LOCK-1/LOCK-3-gated, not blocked**
  (inherited-attribute verdict; floor ∈ {0,1,2}; LOCK-3 3C adoption).

- **7D regex — BLOCKED (measured), not a candidate.** ORACLE-ONLY on two independent counts (3
  grammar-locus verdicts LOCK-5 cannot discharge + the perf gate). The row is banked, honest,
  complete.

---

## §7 — The two sharpest uncertainties (prototype briefs)

Chosen for breadth across families and maximal advance, both runnable TODAY without LOCK-5's engine:
**UF-2** turns the SURVIVOR family's contested flagship into a RAN number (and feeds 7D's ledger);
**UF-1** precisely bounds the pass-2 headline block (INC-5) — is the cross-repo block *total*, or
does a leaf-blind structural oracle survive? Together they touch all four families. (The REGISTRY's
UF-3 — the fixpoint gate on the regex artifact, filling 7A's never-run core + 7D's empty `fixpoint`
column — is the strong third; held for the pass after these two land.) Returned via StructuredOutput.

1. **UF-2 — the STATIC totality% counter over the existing built GrammarIr** (bbnf-worktree). 7C's
   corrected flagship, never run; converts the metric from "blocked" (both critics' erroneous cap) to
   a RAN per-subsystem number and feeds 7D's ledger `totality_pct`. Walk `grammar.exprs`; classify
   LEAVES×BRANCHES × PURE/CONTEXT/HOST; do NOT drive through the emitter.

2. **UF-1 — the structural-only (leaf-blind) bbnf oracle** (bbnf-worktree). The one bbnf-leg
   experiment runnable in-lock WITHOUT LOCK-5's engine. Does the `general_descent` recognizer's branch
   skeleton match the oracle GrammarIr's when leaves are treated as opaque? Refutes or confirms "bbnf
   not even Tier-R," bounding the INC-5 cross-repo block precisely.
