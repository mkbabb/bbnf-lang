# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 FAMILY SPECS (synthesis, post-fold re-cut)

Distilled by the SYNTHESIS agent (Fable-tier), pass 2, from the five pass-2 research reports
(A/B/C/D/priorart), **all four pass-2 prototypes that RAN** — `PROTO-1` (OPEN-1′ builder-augment
→ refuted at the leaf lexer), `PROTO-3-leaf-evaluator` (leaf evaluator → **MISSING PRIMITIVE**,
cross-repo), `PROTO-2`/`7C-T2` (dead-heuristic delete → **PASS, near-vacuous**), the regex
Tier-S oracle (→ **ORACLE-ONLY**, parity below gate) — the LOCK-7 REGISTRY standing verdict,
CRIT-1 (within-lock) + CRIT-2 (cross-lock), and CROSS-LOCK (XL-A…XL-H, consumed as binding fact).
One cogent spec per family **while the routes remain incompatible** — architecture, mechanism,
integration seams, gates, exact open questions — then the incompatibility map. Elegance earns
nothing alone: a family stalled at a missing primitive equal in difficulty to the original problem
is marked **BLOCKED-candidate**; a family stalled at a primitive that is grep-verified absent AND
owned by another (cross-repo) lock is marked **BLOCKED (measured)**.

**Base (XL-H).** Prototypes ran base-pinned (`c93fc4314` / `ce51c36fc`, both confirmed ancestor of
HEAD). The synthesis re-verified the four load-bearing disk facts live on the skinny measurement
substrate (`/Users/mkbabb/Programming/bbnf-lang/skinny`, XL-A): the regex-engine grep
`struct (Dfa|Nfa|Program|Instruction)|transition_table|fn match_at` over `crates/` = **EMPTY**;
`general_descent/{emit,mod,regex_first,view}.rs` = **1206 LOC** (452/93/230/431);
`has_parse_time_host_decode` at `passes/src/lib.rs:794`; `bbnf-regex::RegexKind` a 4-bucket
classifier at `lib.rs:16`. parse-that anchors at `/Users/mkbabb/Programming/parse-that`.

> **Housekeeping owed to GESTALT (recorded so the fold is not corrupted).** **BREACH-2** — the
> regex Tier-S report-of-record was clobbered by the 7C-T2 report at `pass-2/PROTO-2.md`; the RAN
> evidence (`ORACLE_REL_V1`, parity, 12 divergences, 4 mutations) survives as this file's §4 prose +
> `PROTO-2.patch` + `pass-1/PROTO-2-7D-regex-oracle.md`. Re-home under `PROTO-2-7D-regex.md` before
> GESTALT (H-1). **Dead-ID** — CROSS-LOCK keys are **XL-A…XL-H**; earlier `XL-1/XL-2` numeric cites
> re-point (regex fork `XL-2`→**XL-C**; IR-plane `XL-1`→**XL-B**).

---

## §0 — THE SPINE ALL FOUR FAMILIES SHARE (measured, re-verified, not asserted)

1. **A grammar-NEUTRAL descent emitter EXISTS** — `crates/codegen/src/general_descent/` (**1206
   LOC**). `runtime_generator.rs:98 !has_rule("value")` → `emit_general_descent`. `bbnf.bbnf` has no
   `value` rule → it shape-dispatches here (the branch `regen-sheets` travels). It emits **one
   `parse_<rule>` recognizer per rule** (offset-mark tape + lazy `{Prefix}Document` view), **not
   GrammarIr builders** (`grep GrammarIr|add_expr|ExprKind general_descent/` = EMPTY). This retracts
   the pass-0 "re-author an absent machine" framing: the emitter is real, not a stub.

2. **THE HEADLINE — the emitter is grammar-neutral at the BRANCH level, grammar-SPECIFIC at the LEAF
   level, and the leaf fix is a primitive that DOES NOT EXIST (PROTO-1 + PROTO-3, RAN).** Its leaf
   layer (`regex_first::first_bytes` + `emit.rs::{match_leaf, consume_quoted, is_text_byte}`) is a
   Sheets/JSON delimiter-scan approximation. Measured against bbnf's real leaves it **hard-fails at
   the first regex literal**: `split_top_level_alternation:90` splits at paren-depth 0 only → bbnf's
   `(\.|[^\/])+` collapses to `first={'.'}` → the `match_leaf` head-gate REJECTS every `/…/` literal;
   `consume_quoted` honors doubled-quote not backslash escapes; `is_text_byte` excludes `-` /
   includes `.$`. **PROTO-3 then decided the sharp question — bounded arm-swap or missing primitive:
   MISSING PRIMITIVE.** The hypothesized "route `/regex/` leaves through the extant compiled matcher"
   **does not exist**: the grammar tier holds an *opaque interned pattern string*
   (`ir::BackendExpr::RegexProgram{pattern:String}` — a misnomer, no program); the engine tier
   (`bbnf-regex`) is a **4-bucket classifier** (`{Whitespace,QuotedString,Numeric,Unknown}`) with **no
   consume/match method** — all 7 bbnf named-rule leaves classify `Unknown` (0/7 tokenize);
   `parse-that-regex` is delimiter-fixed JSON scanners. The regex-engine grep in `skinny/crates` is
   **EMPTY** (re-verified). So the bbnf leaf evaluator = **author a general regex-execution engine**
   `fn match_at(pattern, input, start) -> Option<end>` = **LOCK-5's cross-repo deliverable (INC-5)**.

3. **Construction floor = 2, irreducible ∈ {0,1,2}** (LOCK-1-dependent; not reached by pass-2
   probes). Node *values* are pure `->` (floor 0); the *build mechanisms* are host-bound — `add_expr`
   (`ir/src/lib.rs:51`, append — SOFT) + `resolve_refs` (`ir/src/lib.rs:84`, cross-rule name→RuleId —
   HARD, an *inherited attribute*, host-bound iff LOCK-1 declines in-grammar inherited attributes).

4. **No self-host exists on skinny (re-greped).** xtask carries `regen/check-{json,sheets,css,
   real-typed}`; **no `regen-bbnf`, no `regen-regex`, no `fixpoint`/`BBNF_TARGET`.** Stage-0 must be
   *authored*, not transplanted. The `88s→0.5s` datum is a **REFUTED legacy transplant** (cargo LTO
   rebuild-avoidance of a monolithic `include!`, not regen/parse; archives at GN-0). Quoting it
   against skinny is a Constitution-ix supersession breach.

5. **The regex self-host ships as a PRODUCER, MEASURED ORACLE-ONLY (regex Tier-S, RAN).** parse-that
   `regex/mod.rs:34 parse_generated -> Result<Hir>` (generated parser then `host::lower` — a
   producer, NOT the `generated.rs:2288 -> bool` recognizer twin). Under pinned `ORACLE_REL_V1`:
   parity **0.7759 strict (45/58)** / 0.7931 Empty-normed / 0.7843 committed-51 — **all BELOW the 0.80
   gate**; GEN **1.77× slower**. 12/12 divergences classified (9× `host.rs` bounded-`{n,m}` obligation
   → LOCK-5; 3× `regex.bbnf` verdict → oracle bank); 4 planted mutations all trip. Details §4.

6. **THE CORRECTION both critics missed (largest of the pass) — 7C's totality% counter is a STATIC
   GrammarIr walk, NOT a self-emit census, and is RUNNABLE TODAY over 3/4 subsystems.** research-C §3
   defines it verbatim: walk the built `grammar.exprs` arena, classify LEAVES×BRANCHES ×
   PURE/CONTEXT/HOST. GrammarIr for **json/sheets/real-typed exists today** (the hand parser builds
   it — that is what `check-*` and codegen consume); **bbnf's own GrammarIr exists too** — PROTO-3
   measured `parse_grammar` over the bbnf **import-closure** (bbnf+expressions+types) = **Ok, 53 rules,
   resolve_refs clean** (behind only the landed Wall-0 `?w`-fix). The counter needs the **build** step
   (available), NOT the **emit** step (blocked); only the *self-hosted-coverage* variant needs
   self-emit. Both critics inherited a build-vs-emit conflation and capped 7C behind the leaf
   evaluator — erroneously. This is the pivot that lifts 7C off the critic floor.

**The unifying pass-2 fact.** The whole **bbnf leg** (7A recognizer/building seed, 7B Plane-A,
7D-bbnf Tier-R/S) rests on ONE cross-repo gap — **LOCK-5's engine-tier `match_at`** (INC-5). No
bbnf-leg family converges inside LOCK-7 alone. **What DOES converge in-lock:** 7C's lock-free trio +
the static totality counter, and 7D's regex ORACLE-ONLY negative. The bbnf leg's pass-1→pass-2
trajectory is a **reachability regression** (builder-augment → leaf lexer → general regex engine,
cross-repo) even as design clarity rose — **the % tracks reachability, not clarity.**

---

## §1 — 7A · STAGED-BOOTSTRAP — **BANKED** — 40%

**Architecture.** Classic two-stage self-host on the greenfield tree: a committed stage-0 + a
fixpoint gate certifying `regen∘regen = regen` (the rustc/GHC/pegen `stage2==stage3` law), minimal
auditable bootstrap surface. Net-new on skinny (§0.4). Its one non-redundant asset is the **fixpoint
LAW** (byte-identical cross-generation re-emit, no 2-cycle) + a blake3 provenance ledger — **not** LOC
reduction, which the numbers refute.

**Mechanism / the seed headline (research-A §2 + PROTO-1/3).** The seed axis is
*recognizer-useless-vs-building-adequate*, and the probes remove even the recognizer consolation:
`general_descent` **cannot recognize bbnf** until the leaf evaluator lands, which is LOCK-5's engine
(§0.2) — so a recognizer seed produces zero correct leaf spans, and there is nothing to fixpoint over.
A *building* seed, if reached, **inflates PAST the 2142-LOC hand parser** (in-repo witness: the
parse-that regex self-host = `generated.rs` 2293 + `host.rs` 524 LOC for a *smaller* language),
dissolving the auditability that is 7A's whole selling point. (Both critics note this inflation is
argued from an in-repo *analogy*, never measured on an actual bbnf building-seed — an un-run claim.)

**Fixpoint SUBJECT (research-A §3, decided).** Cross-generation `.rs` bytes as the gate, with a
**GrammarIr-equivalence floor** as the non-vacuity witness — and that floor **IS the same object as
7D's `ORACLE_REL_V1`** (do not double-home). Decide decisively (no hedge): the GrammarIr floor is
primary; the byte-gate is a hygiene sub-gate non-vacuous *only* under a planted input-mutation that
MUST move the bytes. The template-splice emit path (fixed `include_str!` for json/css) makes a bare
byte-identity a constant-stamp — "triple-identical" certifies the template, not the parser.

**Integration seams.** A `BBNF_TARGET` mirroring `SHEETS_TARGET`; re-author the fixpoint arm against
`emit_runtime_from_request`; reuse `regen.rs:37 check_target` for the byte sub-gate. Rescue only the
legacy *corpus* of adversarial bbnf inputs as fixtures; **do NOT** rescue the legacy bespoke builder
front-end (a NAMED overfit refutation — it archives at GN-0, its Arena/Document shapes do not exist on
skinny's flat-Vec GrammarIr).

**Gates (born-RED).** (a) `check-bbnf-selfparse` — cleared for the import-closure by the landed
Wall-0 `?w`-fix; single-file bbnf still ERRs at the `@import`-closure wall (PROTO-3, a *distinct*
wall). (b) leaf-evaluator gate — **BLOCKED cross-repo** on LOCK-5 (§0.2). (c) cross-generation
byte-fixpoint + GrammarIr floor + planted-mutation non-vacuity — **NEVER RUN on any artifact**
(SPEC_ONLY, the elegant-reduction step both critics flag), **but runnable TODAY on the regex oracle
artifact** (the regex parser self-emits; the REGISTRY homes this as UF-3).

**Why BANKED / exact open questions.** After the fixpoint-law graft (→7D, OPEN-2/XP-2) and the
shared-relation convergence (its GrammarIr floor ≡ 7D's `ORACLE_REL_V1`), 7A owns **nothing 7C+7D do
not**. Its one concrete pass-2 gain (Wall-0 `?w`-attach fix, +12 net lines, json ERR→Ok) is a
*prerequisite of the 7D/leaf leg*, not a 7A deliverable. **Survival test (the standing open
question):** name ONE thing 7A owns post-graft that 7C+7D do not, or 7A RETIRES next pass. The one
fixpoint signal runnable WITHOUT LOCK-5's engine is UF-3 — decoupled from promotion, needing neither
perf≥1.0 nor parity=1.0, only byte-identical re-emit + a mutation that moves the bytes.

---

## §2 — 7B · TOTAL-SELF-HOST — **BANKED** — 32%

**Architecture.** Constitution (i) at its maximal pole: bbnf + regex + IR + emitter templates *all*
grammar-specified, the generator becoming mostly data, hand code shrinking to kernels/primitives.
Decomposes into planes with distinct verdicts. A **research answer, not a route** (its own framing).

**Plane-A — front-end self-host (`ir.bbnf`→`build_grammar_ir(view)`).** **BLOCKED (measured),
cross-repo** — the same leaf primitive as 7A/7D-bbnf (§0.2), not merely "undercut."

**Plane-B — the IR plane (`ir.bbnf`→`ExprKind`), joint LOCK-3 ⊗ LOCK-7 (XL-B).** `ExprKind` (9
arms, `ir/src/lib.rs:234`) is a regular tree grammar → `ir.bbnf` is *writable* as inert data (LOCK-3
3C GRAMMAR-HOMOIOUSIA). The lawful default is **narrow-first**: `ir.bbnf` *generates* the hand enum
(the in-tree `egraph-derive` / memory `derive-language-macro` precedent), with named consumers (1)
`add_expr`, (2) `resolve_refs`/`validate`, (3) the `lower_expr` fold reading a real Rust type — OR
`ir.bbnf` as the concrete syntax of a serialized-IR text with a **round-trip parity oracle** consumer
(needs no new type machinery, lawful today). The transposition *logic* is never data; only the IR
*shape* can be. **Conditional on LOCK-3 picking 3C; else consumer-less (Y.13).** The gating dependency,
named: node-value `->` projections presuppose LOCK-1 sum/product projections (GN-3 Walls 1–2) —
`ir.bbnf` cannot *construct live nodes* without them.

**Plane-D — emitter templates as grammar data (static line-read on `emit.rs`, 452 LOC).** **DATA ≈
50%** (fixed kernel template, grammar-derivable holes) · **WALK-FOLD ≈ 20%** (a catamorphism over the
`SinkOnlyExpr` sum, `->`-expressible) · **IRREDUCIBLE ESCAPE ≈ 21%** — the speculative-cursor reborrow
`let cursor = &mut _probe` inside an `(|| {…})()` IIFE committed on `Ok` (Alt `:400`, Optional `:344`),
plus the RepeatLoop progress-guard `if _probe == _before { break; }` (`:311`). **Named kernel-escape
signature:** transactional commit-or-rollback over a mutable shadow cursor+tape; delete the `&mut
_probe` re-alias → **E0499/E0503** borrow-fail. Prior art is uniform (TableGen, MLIR ODS, tree-sitter):
no production system erases the emitter kernel — it *relocates* into a combinator/VM runtime (LOCK-4C),
which is the honest floor: the generator becomes data *over that runtime*, the escape does not vanish.

**Status: BANKED · maximal pole BLOCKED-candidate; exact open questions.** "Generator becomes mostly
data" holds for the ~70% DATA+WALK-FOLD, **false for the ~21% ESCAPE** — a missing primitive
(grammar-plane backtracking) equal in difficulty to the original problem (it *is* authoring a VM).
**The load-bearing caveat both critics pressed:** the 50/20/21 split is a **static line-count read
(research-B §3), NEVER RUN**; the shares sum to ~91% (**~9% of `emit.rs` unclassified — name the
residue or the ratio is soft**); the E0499 is *checkable* (delete the re-alias, observe the
compile-fail) but was asserted, not shown as a RAN compile-fail — the twice-un-run experiment. Scope
questions home in LOCK-3 (XL-B): if LOCK-3 picks 3C, draft the joint Plane-B family; else 7B RETIRES.

---

## §3 — 7C · HOST-FUNCTION-LATTICE — **ADVANCED, the SURVIVOR** — 57%

**Architecture.** Totality as a *bounded, measured* quantity: a three-tier lattice keyed on ONE
structural fact read off a signature — **whether it threads `&mut Ctx`** (not a name test, not a
`@pure` string — the pluggable decision point the counter reads). **Tier 0 LEAF-DECODE** (pure,
`->`-adjacent; counts as grammar coverage) ⊑ **Tier 1 CONTEXT** (`resolve_refs`, TOML `push_path`,
`?w` — the parser-generator instance of Knuth *synthesized attributes*; comptime, zero residual) ⊑
**Tier 2 RECURSIVE/STATEFUL** (`&mut Ctx` host, an *inherited attribute*, LOCK-1-bound). The anti-gaming
property is load-bearing: **hiding a `->`-able leaf inside a host LOWERS totality%, it cannot hide
behind it** (the exact counter to the GN-5.W2 R2 name-gate-in-disguise trap). Tree-sitter proves the
escape hatch is the *correct* architecture, present in every production system; bbnf's typed
`@host name : type` is already stricter than tree-sitter's opaque C scanner.

**Independence split (research-C §2, XL-3).** **Lock-free** (no LOCK-1 verdict): (1) delete the
string heuristic `has_parse_time_host_decode` (`passes/src/lib.rs:794`; keys on `name.contains("host")
&& …"decode"|"parse"` — a latent, DEAD name-gate) → a typed predicate over the node's tier; (2) lower
`@{}` — add `ExprKind::Host` (the *real* typed node) + a `parse_atom:1495` `@{` arm reusing
`runtime_skip_balanced`, flip `RuntimeConstructKind::HostCapture` off `"BBNF-UNSUPPORTED-HOST-CAPTURE"`
(`grammar/src/lib.rs:222`); (3) *name* `resolve_refs` the CONTEXT floor (designation, no code); (4)
**run the static totality counter** (§0.6). **LOCK-1-bound:** typed `HostFnSig` into inference (T1);
tier-off-`&mut Ctx` (T5); the floor itself (∈ {0,1,2}).

**Deliverable + gate.** A two-population totality% counter — LEAVES (`Literal`+`Regex`+`Project`
targets) × BRANCHES (`Alt` arms + `Optional`/`Repeat` decisions) — per subsystem, PURE/CONTEXT/HOST
breakdown; `totality% = 1 − Σ(Tier-2)/Σ(all)`; floor-gate `xtask check-totality` (a GN-5.W3
`check-name-gates` sibling, `syn`-backed AST audit, floors in `[workspace.metadata.bbnf]`) with
three-form planted-breach falsification (inject a synthetic `&mut Ctx` host over a plain leaf → ratio
drops + floor trips; raise a floor above measured → trips; real fleet passes clean). **RUNNABLE NOW
over json/sheets/real-typed** on the built GrammarIr; **bbnf** via its import-closure GrammarIr
(Ok, 53 rules); **CSS deferred** until T4 (`@{}` lowering — `color.bbnf:220`/`values.bbnf:69` depend
on it). Non-vacuity note: `host_registry = "skinny-none"` today, so the fleet reads ~100% until a real
Tier-2 host is wired — the counter is *specified and runnable* but its floor is exercised only once a
genuine host lands (bbnf `@{}` recursion or TOML datetime).

**7C-T2 RAN and PASSED (PROTO-2) — but proves a NO-OP, not "ships value."** With real rigor (baseline
control, freshness syntax-error confound-kill, negative control, mechanism probe):
`has_parse_time_host_decode` is a **dead name-gate** — `"ws".contains("host")` is always false (the
only annotation the parser ever builds is `?w` = `{name:"ws",value:None}`, `grammar/src/lib.rs:1373`);
all four `check-*` byte-identical, AND the *inverted* predicate `value.is_none()` (fires on every `?w`)
is ALSO byte-identical — proving eager-tape selection **does not flow from this predicate at all** on
the committed fleet. Honest hygiene (removes a latent name-gate GN-5.W3 forbids), but the change keyed
on `value.is_some()` — a **shape-sniff on an always-empty slot**, NOT the typed `ExprKind::Host` node.
T2 discharged the string-gate *refutation*, not the *construction*. pass-1 open-gap (b) "zero gate
executed" is genuinely CLOSED; the win is thinner than "one-file cure, ships now" reads.

**Status: ADVANCED, the SURVIVOR — the only lock-internal-closeable family.** Ships value *now*
(lock-free trio + the static counter, leaf-evaluator-independent) and supplies the totality% metric
every ladder reports against (feeds 7D's ledger, XP-1). Held below CRIT-1's 70 because the tier
mechanism is unbuilt and T2 was near-vacuous. **Exact open questions:** the static counter is runnable
but **UN-RUN** (the corrected flagship — UF-2 below); the real `ExprKind::Host` node + T4 `@{}` un-built
(T2 was a shape-sniff); the CONTEXT-tier zero-cost invariant (`resolve_refs∘resolve_refs` no-op +
`pretty()` unchanged + TOML `push_path` residue=0) un-round-tripped; CSS floor unmet until T4; floor ∈
{0,1,2} LOCK-1-pending.

---

## §4 — 7D · META-CIRCULAR-ORACLE — **ADVANCED, the POSTURE winner** — 51%

**Architecture.** Self-hosting as **verification first**: generated parsers run as differential
ORACLES against the hand parsers; promotion is per-subsystem, gated on a *pinned* equivalence relation
+ golden-anchor parity (never majority vote — three parsers can share one bug) + totality% + cold
per-parse perf — never on the mere existence of a generated parser. **ORACLE-ONLY is a lawful
terminal** (the field's strongest precedent: CPython pegen ran old ∥ new for a full cycle; the DDC /
translation-validation lineage). Two rows in one ledger — a MEASURABLE regex row, a BLOCKED bbnf row.

**The relation, PINNED AND RUN AS CODE (the family's load-bearing act).** `ORACLE_REL_V1 = R1∘R2∘R3`:
**R1** unit-collapse to a fixpoint (`Concat([]) ≡ Empty ≡ Alt([])`, `Concat([x]) ≡ x`, `Alt([x]) ≡ x`,
`Group(x) ≡ x`; `norm` verified idempotent; `Optional↔Repeat{0,1}` OFF by default — over-quotient hides
divergence); **R2** same-stage comparison (both post-`host::lower`, shared `ParseOptions` — the pivot
that stops a fold bug masquerading as a grammar divergence); **R3** group/class/quantifier
field-for-field. All **four planted mutations trip** (greedy→lazy, drop group,
bind-the-`bool`-twin=vacuous→rejected, alt mis-assoc) — proven able to fail. The bbnf twin
`OracleRel-bbnf-v1` runs over `validate∘resolve_refs`-normalized GrammarIr (same passes both sides or
neither, else every `Ref.target` diverges spuriously; there is no `ExprKind::Empty` — the empty match
IS `Seq(Vec::new())`), homeable as an e-graph over `ExprKind` (LOCK-2 engine, mirroring `simplify_hir`).

**Regex Tier-S — MEASURED ORACLE-ONLY / BLOCKED.** Parity **0.7759 strict (45/58) [report as
primary]** / 0.7931 Empty-normed / 0.7843 committed-51 — all BELOW the 0.80 gate; perf **1.77×
slower**. 12 divergences 100% classified into two disjoint buckets: **9× `host.rs` OBLIGATION** — one
bug class, bounded `{n,m}` (`parse_braced_quantifier` receives the span WITH braces and never strips
them → `u32::parse` fails; `pre_recognizer_full=true` proves the parser built the SAME tree) → **route
to LOCK-5**; **3× oracle VERDICT** rooted in `regex.bbnf` (`.` as class-literal not dot-metachar;
`(?s)` inline-flag becoming a literal capturing group) → **bank in the oracle ledger, awaiting LOCK-5's
fold onto the ratified split** (XL-C, SETTLED — grammar tier opaque `Regex(StringId)` + engine tier
rich `Hir` + string bridge; NOT an open fork; the synthesis consumes XL-C as fact, does not
re-litigate). Regex fails promotion on TWO independent counts (3 grammar-locus verdicts + perf) —
ORACLE-ONLY even after LOCK-5's fold. **0.80 gate provenance UNCITED** (a latent gate-tuning risk;
robust here as all three metrics clear below it; the Empty-norm moves exactly one case, 45→46/58, so
it launders nothing).

**bbnf Tier-R AND Tier-S — hard-BLOCKED (measured), cross-repo (PROTO-1+PROTO-3).** Both routes
presuppose `general_descent` *recognizes* bbnf, refuted at the leaf floor — **bbnf is not even
Tier-R**. Rides LOCK-5's `match_at` (INC-5). Pass-1's "gen side buildable NOW at recognizer
granularity" is refuted. (Producer pin retained: bind `regex/mod.rs:34`, never the `generated.rs:2288`
`bool` twin — M3 proved that vacuous.)

**Promotion ledger (the concrete deliverable + its consumer).** Rows (subsystem × tier); columns
`rel_id · binding · parity_pct` (golden-anchored) `· totality_pct` (**from 7C's counter**, XP-1) `·
perf_ratio` (cold paired, parse-stage isolated from `host::lower`) `· fixpoint` (**7A's subject**,
XP-2) `· verdict` (PROMOTE iff parity=1 ∧ perf≤threshold ∧ fixpoint ∧ totality-gate met). First
COMPLETE row: `regex · ORACLE_REL_V1 · parse_generated(->Hir) · 0.7759 · perf=1.77× · fixpoint=EMPTY ·
BLOCKED (ORACLE-ONLY)`. bbnf row: RED, no producer. The `fixpoint` column is fillable via UF-3 without
LOCK-5's engine — the row is presently one column short of complete (both critics).

**Status: ADVANCED, the POSTURE winner.** The strongest MEASURED evidence in the lock — but it is a
NEGATIVE (regex below gate on all three metrics). The single ladder posture (oracle-first;
ORACLE-ONLY lawful terminal). **Exact open questions:** bbnf Tier-R/S BLOCKED cross-repo; the 3
`regex.bbnf` verdicts LOCK-5-substrate-bound; 0.80 gate provenance uncited; the grammar-driven corpus
sampler unbuilt (parity over the committed 51/58 only — generality unmeasured); the structural-only
(leaf-blind) bbnf oracle un-tried (UF-1 below).

---

## §5 — THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

- **INC-1 · 7A big-bang fixpoint ⊥ 7D incremental oracle — COMPOSED except at the cutover posture.**
  Mechanisms compose (7A's fixpoint LAW grafts onto 7D's oracle artifact, decoupled from promotion =
  OPEN-2/XP-2). What collides is the **hand-parser fate**: 7A-as-distinct retires it at cutover; 7D
  keeps it forever as the golden anchor. 7D's retain-forever wins on the measured edge (regex perf
  1.77× slower ⇒ promotion may never fire) → 7A survives ONLY as the graft. Resolved; drove the 7A bank.

- **INC-2 · 7B build-in-grammar ⊥ 7C build-behind-host — RESOLVED-IN-DIRECTION toward 7C, on a LOCK-1
  dial.** Floor 2, irreducible 1, ∈ {0,1,2} pending LOCK-1's inherited-attribute verdict + a
  value-language arena primitive. Not a fixed answer — the two ends of a LOCK-1-parameterized dial.

- **INC-3 · 7A codegen-as-trusted-primitive ⊥ 7B Plane-D codegen-as-grammar-plane — tilts 7A's pole,
  STILL UN-INSTRUMENTED as a running experiment.** The static read (DATA≈50%/ESCAPE≈21%) tilts toward
  the trusted-primitive pole, and `general_descent` (hand-Rust walking the grammar) is disk evidence
  for it — but no probe CONVERTED a blob and measured the escape by RUNNING. The architectural fork is
  decided-by-reading, not by-running (a low-priority pass-3 variant).

- **INC-4 · hand-parser fate — composable; the kernel-carve-out boundary.** Retire-production vs
  keep-CI-oracle compose (the cutover INC-1 shows never fires). 7C orthogonal for T1/T2/T3/T5, coupled
  at T4/T6. The 7C stratum-2↔3 `&mut Ctx` typed seam IS the surviving ladder's kernel-carve-out
  boundary (XP-3) — 7B's "kernels behind a typed call interface" is precisely this seam.

- **INC-5 · the leaf evaluator is a CROSS-REPO BLOCK, not a within-lock choice (the pass-2 headline).**
  PROTO-3 measures the bbnf leaf evaluator = a general regex matcher `fn match_at(pattern, input,
  start) -> Option<end>` = **LOCK-5's engine, homed in parse-that** (XL-C one-regex-home). So 7A /
  7B-Plane-A / 7D-bbnf are **downstream of a cross-repo lock** — a **hard CAP on convergence %** for
  every bbnf-leg family (not a routing footnote) and a **born-RED DAG edge the omega gestalt must
  carry** (XL-INC5). LOCK-9/9C's SCANNER-AS-DFA is a *downstream consumer* of that engine (XL-5), NOT
  an available upstream leaf primitive (PROTO-3 correction).

**Convergence (not incompatibility), named.** 7A's fixpoint SUBJECT and 7D's relation are the **same
object** — one `ORACLE_REL_V1`, consumed by both (do not double-home). PROTO-2-7D-regex's `@host
resolve_refs : (&mut GrammarIr) -> Result<(),ValidationError>` is simultaneously 7C's first CONCRETE
CONTEXT instance AND the HARD floor a future builder-augment must house — but that builder-augment is
BLOCKED **upstream** at the leaf floor, so the HARD-floor host is a **DEFERRED consumer** (behind
LOCK-5's engine), not an active bridge this pass (XP-4).

**Lawful gestalt shape (hardened by pass 2).** **7C ships regardless** (lock-free trio + the *static*
totality% counter, runnable now over 3/4 subsystems) **+ 7D as the single ladder posture** (oracle-
first; regex row banked ORACLE-ONLY; bbnf rows BLOCKED cross-repo) **+ 7A's fixpoint law grafted onto
7D** (OPEN-2, decoupled from promotion, runnable today on the regex artifact) **+ 7B banked** (maximal
pole BLOCKED-candidate). The whole bbnf leg rests on ONE cross-repo gap: LOCK-5's engine-tier
`match_at`. No bbnf-leg family converges inside LOCK-7 alone.

---

## §6 — BLOCKED / BLOCKED-candidate register (elegance earns nothing where the primitive is missing)

- **The bbnf leg (7A recognizer/building seed · 7B Plane-A · 7D-bbnf Tier-R/S) — BLOCKED (measured),
  cross-repo.** NOT a candidate: PROTO-3 grep-verified there is no general regex matcher at any tier in
  `skinny/crates` (re-verified empty this pass). The missing primitive is LOCK-5's engine `match_at`
  (INC-5/XL-INC5), equal in difficulty to the original problem. A born-RED DAG edge, not a
  LOCK-7-internal patch.

- **7B Plane-D (maximal "generator becomes mostly data") — BLOCKED-candidate.** Missing primitive =
  grammar-plane expression of the ~21% backtracking-control escape (commit-or-rollback over a shadow
  cursor+tape, E0499/E0503-bound). Equal in difficulty (it *is* authoring a combinator/VM runtime). The
  bounded form (DATA+WALK-FOLD lifted, ESCAPE a named committed primitive) is not blocked but ⊆ {LOCK-3
  3C + the leaf-evaluator + builder-augment}. The ratio is a static-read, un-run.

- **7B Plane-B built-IR** and **7C floor finalization** are **LOCK-1/LOCK-3-gated, not blocked**
  (inherited-attribute verdict; floor ∈ {0,1,2}; LOCK-3 3C adoption).

- **7D regex — BLOCKED (measured), not a candidate.** ORACLE-ONLY on two independent counts (3
  grammar-locus verdicts LOCK-5 cannot discharge + the perf gate). The row is banked, honest, complete
  save the `fixpoint` column (UF-3).

- **NOT blocked (the correction) — 7C's static totality% counter + lock-free trio.** Runs on the
  BUILT GrammarIr (present for json/sheets/real-typed; bbnf via import-closure). The build-vs-emit
  conflation that capped it behind the leaf evaluator is corrected (§0.6).

---

## §7 — The two sharpest uncertainties (prototype briefs, full text in StructuredOutput)

Chosen for breadth and maximal advance, both **runnable TODAY without LOCK-5's engine**, each with a
numeric pass/fail. **UF-2** turns the SURVIVOR family's contested flagship — which both critics
erroneously capped as "blocked" — into a RAN per-subsystem number and feeds 7D's ledger `totality_pct`;
**UF-1** precisely bounds the pass-2 headline block (INC-5): is the cross-repo block *total*, or does a
leaf-blind structural oracle over the bbnf import-closure GrammarIr survive and yield a bbnf Tier-R
signal in-lock? Together they touch 7A (via 7D ledger), 7C, and the whole bbnf leg (7B-A/7D-bbnf). The
REGISTRY's **UF-3** — the fixpoint gate on the regex artifact, filling 7A's never-run core + 7D's empty
`fixpoint` column — is the strong held third (it is the single fixpoint signal runnable without the
engine, and 7A's survival test), scheduled for the pass after these two land.

1. **UF-2 — the STATIC totality% counter over the existing built GrammarIr** (bbnf-worktree).
2. **UF-1 — the structural-only (leaf-blind) bbnf oracle** (bbnf-worktree).
