# LOCK-5 — THE REGEX CONTINUUM — pass-3 SYNTHESIS (FAMILY-SPECS, re-deployed)

Synthesis of the pass-3 research corpus (research-{A,B,C,D,priorart}, re-run) against the pass-2
registry, the ratified adjudications (XL-A, XL-C, XL-H), and EVERY prior artifact — including the
four prototypes that RAN this pass. This is the re-deployed pass-3 synthesis: it is written AFTER
the two decisive runs the 15:27 draft declared "unrun/unbuilt," so those outcomes are conveyed here
as **facts**, not obligations. One cogent specification per family while routes remain incompatible;
then the incompatibility map, the cross-lock double-homes LOCK-5 names back, and the two sharpest
runnable briefs. Elegance earns nothing on its own — a family stalled at a missing primitive as hard
as (or harder than) the original problem carries a BLOCKED marker.

**Settled ground (XL-C, RATIFIED — conveyed as FACT, not re-opened).** TWO domain-fit types: the
grammar tier keeps the regex leaf **opaque** (`Regex(StringId)` top-level / `Regex{pattern:String}`
skinny, `ir/src/lib.rs:250`); the engine tier keeps the rich `Hir` grown with `Class`/`Look`
(`parse-that/rust/regex/src/hir/mod.rs:216`); a cheap `string`/`format!` bridge reusing the engine's
`simplify_hir` (`egraph/mod.rs:143`) joins them. 5A-identity is refuted; 5B-functor is retired; the
survivor — two types + cheap bridge — is the lock's Axis-1 verdict, owned by neither family's
charter. Canonical engine home is parse-that (one-regex-home); the vendored `parse-that-regex` inside
skinny is the MEASUREMENT SUBSTRATE (base-pin every number to HEAD, XL-H). **Charter this pass:
HARDEN the survivor with run evidence — not re-argue which type-plane wins.** SPEC-ONLY(grammar-driven)
rides on every finding until LOCK-7/7A.

---

## THE PASS-3 HEADLINE — both of the survivor's decisive run-gates are now CLOSED

Four prototypes ran, base-pinned per XL-H. The two decisive ones landed AFTER the 15:27 draft and
after the two critiques — the pass's central process lesson (stale-gestalt at two document levels).
Conveyed here as settled:

- **5D generator-path lowering — RUN-EARNED PASSED (proto-1-overapprox, HEAD `ce51c36fc`).** Under a
  SOUND over-approximating first(C) drawn from the DFA start-state class set, genuine-give-back holds
  at **3/16 = {word, sheets::string, sheets::identifier}**, machine-asserted; the single disjointness
  flip (`sheet_prefix`) is a `!`-terminated conservative false-positive in both branches → **0 genuine
  sound→unsound flips**. Cold-pair (N=240×256, paired, correctness-asserted): tape **byte-identical
  256/256** on both leaves; direct scan **3.25×** faster than `Dfa::find` on rare `[0-9a-fA-F]{6}`,
  **2.81–3.00×** on dense `[a-z]+`. **VERDICT: the DFA leaves the generator path; direct-scan-default
  RATIFIED, DFA→narrow fallback for the genuine trio.** Gates LOCK-9-9C, LOCK-4-4D.

- **5C automata-as-extraction CENTERPIECE — RUN-EARNED RETIRED (PROTO-2-5C, HEAD `ef10d5b78`,
  parse-that READ-ONLY).** Corpus 26→56 (12 acyclic / 44 cyclic, back-edge DFS); oracle **0/56
  divergences** (five lanes byte-identical, ρ=1). The `DfaSig` congruence rebuild degenerates to
  **O(n²·k)** on exactly the acyclic chain-DFAs it exists to accelerate (`[01]{8}` n=9 → **8
  passes-to-fixpoint**; passes track DFA diameter, not log n); it is the **slowest correct lane** on
  acyclic. `incumbent ≈ null` (<1% both tags): the `dfa.rs:358` full-restart strawman was never slow
  at regex scale — **there was no cost-model foe to beat.** Prior art (determinization-as-e-graph has
  ZERO published art; e-graphs ARE tree automata, the working direction is automata→e-graph, DAG
  extraction NP-hard) predicted the loss; the run confirmed it. Brief-2-of-pass-3 is DISCHARGED.

- **Fork census — Fork-B available FREE (PROTO-2, `ce51c36fc`).** Over 33 Alt-of-regex patterns, on
  **bare** `Alt([Regex,…])` the three grammar-tier regex rules fire (11/19/30 work); on **projected**
  `Alt([Map(Regex,Tag),…])` every rule applies **exactly 0** (`total_applied=0`). Real BBNF
  alternations carry per-branch tags → the `Map` barrier is the common case → the grammar-tier regex
  e-graph is consumer-less under projection-domination (Y.13) → abrogating it (Fork-B) is free; 5B's
  ~215-LOC de-dup is a fork artifact, not a family win.

**The reconciliation this synthesis renders:** the two-types+bridge survivor now has BOTH decisive
run-gates closed — its GENERATOR-PATH lowering is direct-scan-default with the DFA measured off the
path (5D PASSED), and its AUTOMATA-INTERNALS deliverable is 2C-as-table + delete-`:358` + single-pass
Revuz, the e-graph-over-automata centerpiece run-refuted (5C FAILED). The survivor's remaining work is
INTEGRATION, not another type-plane or automata race. **The lock's TITULAR thesis — grammar-driven —
remains at ZERO artifacts after three passes and is the one genuinely un-attempted formulation.**

---

## PRECONDITIONS (lock-wide, gate every family's earned convergence)

**P1 — the two-tree fork, decisively favoring Fork-B (abrogate the grammar-tier regex e-graph).** The
`->` representation differs by tree: skinny lowers `->` to `ExprKind::Project{body,target,ty}`
(`skinny/crates/ir/src/lib.rs:261`) — a typed wrapping edge, no grammar-tier regex e-graph at all;
the top-level tree lowers it to `GrammarENode::Map{inner,fn_id}` (`crates/ir/src/egraph/node.rs:52`).
research-B confirms `Map` is an **outer wrapper node one level above the leaf** (`types/node.rs:79`),
which is exactly what makes the projection-soundness guard **checkable** (Q11 below). PROTO-2's
`total_applied=0` on projected branches settles the consumer-density number; GESTALT still owns the
fork, subject only to counting how often real grammars present a **bare** `Alt`-of-regex (uncounted).
The number leans abrogate.

**P2 — grammar-driven is SPEC-ONLY (re-confirmed; ZERO grammar-driven artifacts after THREE passes) —
the lock's TITULAR-DELIVERABLE FLOOR.** The lock is literally "the grammar-driven regex engine."
`parse-that/rust/parse_that/src/regex/generated.rs` `parse_generated(&str)->bool` still **discards
its AST**; two `regex.bbnf` diverge (`parse-that/rust/bootstrap/regex.bbnf` 72 LOC L4-spec vs
`grammar/misc/regex.bbnf` 27 LOC legacy EBNF, both verified this pass). Every verdict rides
SPEC-ONLY(grammar-driven); **GESTALT-P must read that label as "un-attempted three passes," NOT
"converged pending LOCK-7."** The namesake work is **promote + de-dup + break-the-discard**, gated
LOCK-7/7A — and it is the substance of Brief 2.

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 42% (refactor residual; re-label candidate)**

**Architecture (narrowed to the survivor).** The maximal claim — regex HIR IS a subset of the bbnf
HIR, nothing to map, classify dissolves — is dead (PROTO-2, pass-2). What survives 5A into the
settled winner is engine-tier leaf-growth (`Class`/`Look`, already in `hir/mod.rs`) plus the deletion
of the skinny name-gate heuristic that reconstructs structure the grown leaves already carry.

**Mechanism (source-anchored, research-A).** research-A concedes honestly: the 1148-LOC engine
`classify.rs` **RELOCATES, not shrinks** — `classify_regex_from_hir` (`classify.rs:177`) is already an
HIR analysis pass; re-homing it deletes no line and drags `Hir`+`CharClass`+`Look`+`CharSet128` into
bbnf `ir`. The LOC delta of literal ONE-HIR is **positive** (~2200 relocated, ~15 opaque-string arms
widened, 0 removed). The genuine, *measurable* win research-A isolates is elsewhere and is NOT
ONE-HIR-exclusive: the single coarse datum "which of {ws,string,number,other}" is **triplicated across
four enums joined by three mapping functions** — `RegexKind` (`bbnf-regex:16`) → `SpanKind`
(`ir:440`) → `SinkOnlySpanKind` (`sink_only.rs:103`) → `ArmKind` (`dispatch.rs:88`) — with first-sets
reimplemented **three times** (`bbnf-regex::first_set`, `regex_first_bytes` in passes and in
codegen). A single `classify_from_hir(&Hir) → SpanFacts` computed once at the leaf collapses the four
enums and three first-set copies into projections of one fact. **That collapse is where lines
disappear — and it is available under the two-type split (the bridge calls the engine classifier
once), so it is a survivor win, not a 5A win.**

**Q2 typing + confinement (load-bearing).** `Class → Span` carrying a width-var solved as a bounded
post-solve over LOCK-1's numeric lattice; `Look → Unit`/zero-width (contributes `nullable=true`). The
byte-expansion confinement boundary is PHYSICAL under two-type and DISCIPLINE-ONLY under ONE-HIR:
`to_positive_byte_ranges`/`negate_byte_ranges` are called ONLY from `NfaBuilder::build_class`; the
`Hir` stores classes intensionally (`Bytes{ranges,negated}`). Two-type forbids the 256-blowup leak
structurally (the grammar tier cannot see `Class`); ONE-HIR can only lint it — a named argument
AGAINST literal ONE-HIR.

**Q4 the Look-consumer gap (a LIVE correctness defect, not hypothetical).** `build_look`
(`nfa.rs:254`) matches every `Look` variant to a single zero-width state with `start == accept` — it
emits no assertion; `dfa.rs` has zero anchor logic. Consequence: **`/foo$/` wrongly accepts
`"foobar"`** (the `$` is compiled away). The skinny shadow is worse — `RegexKind` cannot represent an
anchor at all. The failing-then-passing assertion (`Dfa::compile("foo$").find("foobar").is_none()`)
FAILS today and has been buildable-but-unbuilt a THIRD pass. **Growing `Look` into ANY shared type
without a `LookMatcher` consulted at DFA search time violates Y.13** — a named gap blocking any regex
type carrying anchor semantics, independent of the type-plane debate.

**Integration seams / gates.** Engine leaves exist (`hir/mod.rs`). Skinny shrink: delete the ~200-LOC
heuristic block + the 322-LOC shadow crate; re-point `use bbnf_regex::analyze` (`ir/src/lib.rs:1`) to
the engine's `RegexInfo`. Gates, ALL UNBUILT: (G-5A.look) the `/foo$/` test + a `LookMatcher`
consumer; (G-5A.shrink) net ΔLOC AFTER paying the ~787-LOC parser import — **sign unknown** ("delete
522, import 787" may be net GROWTH); (G-5A.leak) `[a-c]≡[a-z]` identical `Span<width>` through LOCK-1's
real HM; (G-5A.fork-collapse) the four-enum→`SpanFacts` before/after LOC.

**BLOCKED assessment: NOT BLOCKED** (no missing primitive) — but the distinguishing mechanism is dead,
the residual is refactor-shaped, and every gate is unbuilt with zero 5A runs across three passes.
Convergence **42%** (CRIT-1 floor: the surfaced parser-import cost is a downgrade to the shrink claim).
**Re-label candidate:** if 5A stays parked a fourth pass, re-label family → "the survivor's
bridge-hardening refactor residual."

---

## FAMILY 5B — LAWFUL-FUNCTOR — **RETIRED — 40%**

**Status.** RETIRED (earned-conditional, pass-2), STRENGTHENED by PROTO-2 (projected-branch rule work
`total_applied=0` → the ~215-LOC de-dup dies FREE under Fork-B, attributable to no family). The
functor as an architecture with independent value is dead; its soundness win (the projection-dropping
union bug) was already a non-bug under the whole-leaf wrapper.

**What research-B hardened (credited, firewalled).** research-B read the census at HEAD: **42 concrete
arms** touch the grammar leaf (26 opaque-grouping, 16 consuming); under 5B the grammar leaf stays
`Regex(StringId)` so **zero grammar-tier arms grow** — a genuine argument the two-type split is the
low-disturbance choice. And Q11 is now CONFIRMED from source: `->` is `Map{inner,fn_id}`, an outer
barrier keyed by `fn_id`, not a leaf annotation. This makes the projection guard **checkable**:
fusion of `Alt([Regex,Regex])` is sound only when branches are bare or carry equal `fn_id`;
`[a-c]->A | [d-f]->B` = `Alt([Map{Regex,A}, Map{Regex,B}])` must refuse byte-set union (would drop A,B).
The `Map`-as-structural-node representation is exactly what vindicates the side-annotation position.
**This is bridge/survivor hardening, NOT a functor win** — crediting it to 5B would launder a retired
family's disposition; research-B firewalls its own reverse-`Hir→String` unparse away from 5B and homes
it in the bridge round-trip oracle. Discipline held.

**Sole re-open trigger (LOCK-1-gated).** The retire rests on `->` staying a whole-leaf wrapper that
never binds a **sub-leaf** capture. If LOCK-1 ratifies a sub-leaf-capture representation (5D's
capture-leaf ambition, `(/[0-9]+/->Digits)` reaching inside the class), the projection-dropping-union
bug reappears AND breaks 5D's `Project` byte-identity oracle — both hang on the same unratified fact.
Routed to LOCK-1 with the PROTO-2 evidence.

**BLOCKED assessment: N/A (RETIRED).** Convergence **40%** — disposition-certainty of a conditional
retire, raised by measured fork-independence, not a shipping number.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **RETIRED (centerpiece) — 55%; internal 2B-as-minimizer BLOCKED**

**Architecture.** Push the shared e-graph one level below the HIR, to the automaton: minimization as
congruence closure, determinization as derivative-saturation, tier-selection as CSP extraction.
Orthogonal to Axis-1 (rides below the survivor's engine `Hir`).

**The centerpiece is RUN-EARNED RETIRED (PROTO-2-5C).** minimization-as-e-graph-congruence: wrong
asymptotics on its own target class (O(n²·k) on acyclic chains), slowest correct lane, and — the
deeper point — **no cost-model foe existed** (`incumbent ≈ null`, <1% at n≤20; every prior
"acyclic-beats-Hopcroft" claim was measured against the `dfa.rs:358` full-restart strawman). Prior art
predicted it (determinization-as-e-graph has ZERO published art; on a 256-byte concrete alphabet
Hopcroft's O(n log n) partition refinement IS congruence closure by another name, so the e-graph frame
RELOCATES work at added cost). Brief-2-of-pass-3 is DISCHARGED — do NOT re-budget.

**Three sub-verdicts.**
- **2A minimization-as-congruence — RETIRED as centerpiece.** What SURVIVES it (structural, no
  e-graph): a single **reverse-topological Revuz pass** on acyclic DFAs — the only lane whose passes
  stay at **1** as n grows (1.42× over bug-fixed Moore, growing with n). Folded at the `dfa.rs:89`
  minimize dispatch behind an `is_acyclic` tag.
- **2B determinization-as-derivative-saturation — BLOCKED (as minimizer), Kozen.** Regular-language
  star-equivalence needs the star-induction inference rule, not a finite ground rewrite → **no finite
  e-graph rule set is complete.** A provably-impossible missing primitive as-hard-as the original —
  **the lock's cleanest BLOCKED.** 2B-as-*determinizer* survives (25/26 ρ=1, low priority); 2A/Hopcroft
  closes the minimizer.
- **2C prefilter/tier-selection — SURVIVES as a LOCAL TABLE, not a CSP.** research-C's zero-witness
  grep (no `lut_budget`/`codegen_budget`/`shared_lut`/`table_budget` in `crates/ir/src/`; the
  `decision_csp.rs` solver is rule-LOCAL, one variable per rule, per-candidate legality predicates, no
  shared resource) → tier is local → the monotone `EngineSet` table wins; a CSP over it is Y.13
  substrate. The **one** condition that flips it: lift the per-tranche generated-size budget into the
  solver as a shared 0/1-knapsack constraint (patterns as items, LUT/table bytes as weights). That
  lifting does not exist in the tree — a post-hoc build gate, not a solver constraint. This is 5C's
  cleanest positive opening, gated on a **budget-binding corpus that does not yet exist**.

**SURVIVOR (re-homed, ADVANCING, needs no e-graph): 2C-as-table + delete-`dfa.rs:358` (mandatory
latent ~O(n³) restart, correctness-neutral, 0 divergences) + single-pass Revuz.** Integration UNBUILT:
minimize-swap (`dfa.rs:89-91`), Valmari/Revuz native partial-DFA `DEAD` handling, tier-table home
(`info/mod.rs:344`).

**Named residual gaps (the survivor's, not the centerpiece's).** (1) The **byte-rarity oracle** — a
NAMED MISSING INPUT for accel-byte quality (regex-automata's static ASCII-rank table, absent in-tree;
without it accel-byte selection is guesswork). (2) A **de-confounded Valmari control** — PROTO-2-5C's
Hopcroft lane was allocation-confounded (`Vec<Vec<Vec<u32>>>` + per-split heap); the FAIL rests on the
allocation-independent passes-count + `incumbent≈null` (sufficient to dispose the centerpiece), but a
clean zero-allocation Valmari (CSR inverse transitions, three parallel partition vectors,
absent-transition partial-DFA rule) was never produced — needed only to SIZE the Revuz win, not to
re-open the centerpiece. (3) `is_acyclic` dispatch net-positivity is marginal (Σ(null−revuz)=7208 ns >
6786 ns dispatch).

**BLOCKED assessment: FAMILY RETIRED (centerpiece); internal 2B-as-minimizer BLOCKED (Kozen).**
Convergence **55%** — convergence-of-disposition, high because the decisive control RAN and a concrete
non-e-graph survivor ships; NOT a shipping number (integration unbuilt). **Route to LOCK-2 as a
BINDING constraint (not a proposal):** automata-minimization is NOT an e-graph island and
tier-selection is NOT a CSP — two of LOCK-2's candidate island/CSP sites are refuted at the regex
layer.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **ADVANCED — 55%; direct-scan-default RATIFIED**

**Architecture.** A `/pat/` lowers DIRECTLY into native grammar IR — a straight-line class-scan
(`Recognizer::ByteClass` membership over the tape, no automaton) when the leaf is FOLLOW-sound; a
`Seq[Repeat, FOLLOW-guard]` or the DFA fallback for genuine give-back. The Glushkov/Berry-Sethi
position automaton done as an IR lowering. Under XL-C, 5D is **the generator-path lowering policy of
the two-types survivor.**

**The thesis is RUN-EARNED (proto-1-overapprox, see headline).** The generator path already realises
5D's thesis in embryo: there is NO regex engine on the codegen path — a `/…/` is `RegexProgram`
(`ir/src/lib.rs:389`) emitted as a first-byte-set `match_leaf` greedy scan (`emit.rs:373`). research-D
sharpens two things the run confirmed: (a) the leading-byte set is computed by **THREE independent
hand parsers** (`bbnf-regex::first_set`, `regex_first::first_bytes`, `json_sink_direct::
regex_first_bytes`) — a god-duplication the survivor collapses to one `first_of`/`class_program_of`
off the engine HIR; (b) `match_leaf` is a greedy maximal run, correct ONLY when
`class(leaf) ∩ FIRST(grammar-FOLLOW(leaf)) = ∅` — the FOLLOW predicate that DEFEATS the unsound
`check_one_pass_eligible` (`one_pass.rs:64`, whose `Repetition` arm recurses without a FOLLOW check).

**The substrate gap (source-confirmed, UNBUILT).** `derive_recognizers` (`passes/src/lib.rs:338`)
emits `Recognizer::SimdScan{ mode: SimdMode::Exact }` ONLY for JSON structural bytes `{}[],:"` plus
quoted-string `"`; `SimdMode` (`ir/src/lib.rs:425`) has the SINGLE variant `Exact` — a general `[a-z]`
falls to `_ => {}`, the **masked wildcard**. The tape-obligation (direct class-scan emits the same tape
a DFA match would) is **EMPIRICALLY DISCHARGED** (256/256 byte-identical), so the tape-risk is retired;
only wiring is unbuilt: add `SimdMode::ByteClass(ByteSet256)` / `Recognizer::ByteClass{set,site}`, gate
`derive_recognizers` on the FOLLOW fixed point, route `BackendExpr::ByteClass` to the tape
(`codegen/src/lower/tape_plan.rs`). This is the piece LOCK-9-9C waits on.

**The likeliest positive outcome, named.** research-D §9: if the FOLLOW-disjoint fraction is ~100% on
the real corpus, `Dfa::find` never fires in codegen — the DFA has **no generator-path consumer** and
survives ONLY as the standalone API's engine (Y.13 then FORBIDS wiring it into codegen). Report as a
positive result, not a patch-around. The genuine-give-back trio (3/16) is the sole DFA-reachable set.

**Named gaps.** (1) The `ByteClass` substrate is DESIGNED, not built (a LOCK-3-governed multi-crate
edit); tape-risk discharged, wiring unbuilt. (2) Corpus breadth: 16 leaves, approximate sheets
FOLLOW-wiring (39-rule subset) — grow with exact wiring. (3) The **over-consumption counterexample**
(research-D §9): a grammar `word "and" word` with `word=/[a-z]+/` — assert the CURRENT emit produces a
WRONG parse (rejects `"a and b"`), proving the FOLLOW-guarded `Seq[Repeat,suffix]` is a CORRECTNESS
FIX, not just a fraction number — UNBUILT. (4) Capture-leaf → `Project` byte-identical to a
hand-authored grammar capture, under LOCK-1's ratified `->` — UNBUILT, LOCK-1-gated (same fact as 5B
re-open). research-D recommends: regex-internal `->` is FORBIDDEN at the byte plane (the class-program
stays projection-free; `->` binds the whole leaf span, matching `ValueProject{body=RegexProgram}`).

**BLOCKED assessment: NOT BLOCKED.** Convergence **55%** — the distinguishing thesis (direct-scan-
default, DFA off the generator path) is RUN-EARNED on a sound source with cold-pair evidence and a
machine-asserted partition; tape-risk empirically retired. What remains is substrate wiring + corpus
breadth + the LOCK-1-gated capture-leaf — integration, not architecture.

---

## INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The Axis-1 mutual exclusion (5A-identity ⊗ 5B-functor) is **CLOSED** — both horns ran and failed
pass-2; the survivor (two types + bridge) is ratified (XL-C). What remains:

- **5A-residual ∧ 5D — COMPATIBLE, one survivor, differ only on generator-path lowering.** Both harden
  the survivor: 5A deletes the name-gate + shadow crate and hardens the bridge (incl. the four-enum
  `SpanFacts` collapse); 5D adds the direct class-scan lowering the bridge feeds. They share the
  `Class`/`Look` engine leaves and the `ByteClass` chooser, and answer the SAME Y.13 anchor question
  (5A's `/foo$/`+`LookMatcher` answers it for both). Not mutually exclusive; jointly the survivor's
  harden+lower policy.

- **5B ⊗ 5D — the dead exclusion, now moot.** With 5B RETIRED and the survivor's bridge string-based,
  5D lowers the leaf into native nodes at desugar and the bridge serves only the fallback DFA path —
  nothing to reconcile.

- **5C ∥ {5A-residual, 5D} — orthogonal, below the HIR.** 5C operates on the automaton, consuming
  whatever the engine `Hir` produces. GRAFT-2 (the byte-class partition `sets/equiv.rs` consumes the
  engine's `Class` byte-sets) feeds Revuz + the 2C table. 5C's only exclusion is INTERNAL:
  2B-as-minimizer (BLOCKED, Kozen) cannot replace 2A/Hopcroft.

- **The SURVIVING meta-tension (a fork, not a family exclusion): Fork-A vs Fork-B.** PROTO-2 made
  Fork-B free in the projection-dominated regime. Fork-A (keep the grammar-tier regex e-graph) is
  required ONLY if GESTALT finds real grammars route **bare** `Alt`-of-regex to the tier — a frequency
  uncounted. GESTALT's to close; the number leans abrogate.

- **GRAFT-3 (coherence requirement, not an exclusion): 5D's direct-scan-vs-DFA FOLLOW-guard ⊗ 5C-2C's
  tier table.** Both decide "class-scan vs DFA" for the same leaves from two homes — 5D on the
  generator path, 2C at the engine tier. They MUST be ONE consistent policy; the pass-4 substrate
  build must wire them as a single decision surface (feeding the PROVE-10 prefilter seam).

- **The LOCK-1 re-open trigger (cross-lock, not intra-lock).** A `->` representation that binds
  SUB-LEAF captures un-retires 5B AND breaks 5D's `Project` byte-identity oracle — both hang on the
  same unratified LOCK-1 fact.

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; LOCK-5 proposals GESTALT ratifies)

1. **Axis-1 verdict ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C — RE-CUT (owed at GESTALT).** All three were
   written against "ONE-HIR"; the verdict is two domain-fit types + cheap bridge; 4D/9C/3C consume the
   two-types landing or converge on a refuted premise. Still nobody's charter.
2. **5C REFUTATION ⊗ LOCK-2 — BINDING INPUT.** automata-min NOT an e-graph island (run-refuted
   O(n²·k), incumbent≈null); tier NOT a CSP (zero shared-resource coupling). Single-home the tier table
   in 5C; escalate to LOCK-2's CSP only on an exhibited shared-LUT/Teddy-budget witness (none exists).
3. **5B retirement ⊗ LOCK-1 (RE-OPEN TRIGGER).** Sub-leaf-capture representation un-retires 5B AND
   breaks 5D's capture oracle — routed with PROTO-2 evidence.
4. **5A/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA) — DE-RISKED, still UNDELIVERED.** LOCK-5 owns the
   `Hir`→DFA→classify substrate + the `ByteClass` chooser; the producer is UNBUILT (masked wildcard),
   but Brief 1 empirically discharged its one correctness obligation (256/256) — 9C's dependency is
   de-risked, only wiring remains.
5. **5A ⊗ LOCK-7 bootstrap.** Two divergent `regex.bbnf` (one discards its AST). "promote + de-dup +
   break-the-discard"; SPEC-ONLY until 7A; zero artifacts three passes — the substance of Brief 2.
6. **Type-plane ⊗ LOCK-1.** `Class ⊢ Span` / `Look ⊢ Unit` + width-var graded only against skinny's
   stub `BuiltinTy`. Route through LOCK-1's real HM + row-poly + widths.
7. **rust/ts ⊗ LOCK-6.** The mirror/skeleton carries BOTH types + automata crossing to TS (PROVE-14
   tape); the serde-derive-ownership question (who adds `CharClass: Serialize`) lands on the parse-that
   READ-ONLY / LOCK-6 boundary — named gap for GESTALT-Ω.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs for pass-4)

Brief 1 delivers the **survivor's generator-path substrate** — the highest-leverage residual (it
unblocks LOCK-9-9C and LOCK-4-4D and turns the RATIFIED policy into in-tree code) AND fires the one
un-built CORRECTNESS claim (the over-consumption counterexample). Brief 2 forces the **lock's TITULAR
deliverable** — the one formulation ZERO passes have touched — producing the first grammar-driven
artifact and letting GESTALT-P dispose of the SPEC-ONLY label as "measured," not "un-attempted." 5A's
`/foo$/`+`LookMatcher` and the 5C byte-rarity oracle are real but lower-leverage; deferred to pass-4
follow-on.

### Brief 1 — 5D: `Recognizer::ByteClass` end-to-end + the over-consumption correctness fix (`bbnf-worktree`)

**Question.** Does the RATIFIED direct-scan-default policy land in-tree with byte-identical tape, and
does the FOLLOW-guard fix a real WRONG parse the current masked-wildcard `match_leaf` produces?

**Method.** Worktree off `bbnf-lang` master (base-pin, XL-H; record HEAD). (a) **Correctness first:**
add a test grammar `rule = word "and" word ; word = /[a-z]+/` and assert the CURRENT emit
(`emit.rs:373` greedy `match_leaf`) produces a WRONG parse of `"a and b"` (the first `word`
over-consumes). (b) Add `SimdMode::ByteClass(ByteSet256)` (`ir/src/lib.rs:425`) +
`Recognizer::ByteClass{set,site}` + `BackendExpr::ByteClass{set,min,max,span_kind}` (beside
`RegexProgram`, `ir/src/lib.rs:389`); gate `derive_recognizers` (`passes/src/lib.rs:338`) on the
FOLLOW fixed point (reuse the closure in
`restart/.../LOCK-5/pass-3/proto-1-overapprox/follow_sound_fraction.rs`), emitting `Seq[Repeat,
FOLLOW-guard]` for the non-disjoint leaf; route `BackendExpr::ByteClass` through
`codegen/src/lower/tape_plan.rs` + `emit.rs`. (c) Re-run.

**Command.** `cargo test -p codegen --test byteclass_lower -- --nocapture` (new test).

**Pass/fail.** current emit WRONG on `"a and b"` (over-consumption proven) AND the ByteClass path
parses it correctly AND emitted tape byte-identical to a hand-authored `word = ([a-z] - "and")+`
grammar on **20/20** inputs ⇒ FOLLOW-guard is a correctness fix + substrate lands; LOCK-9-9C unblocked.
Any tape divergence, OR the current emit already parses `"a and b"` correctly ⇒ the counterexample is
wrong or the greedy scan is already sound — re-home 5D's correctness claim.

### Brief 2 — grammar-driven break-the-discard + de-dup the two `regex.bbnf` (`pt-scratch`)

**Question.** Can the bootstrapped `regex.bbnf` generated parser return an `Hir`/AST (not `bool`) that
threads into the XL-C `simplify_hir` bridge and DFA-matches the hand `hir/parser::parse` — and how
large is the promote+de-dup gap?

**Method.** READ-ONLY rsync of `/Users/mkbabb/Programming/parse-that/rust` into scratchpad (R3 §8
recipe; isolated `CARGO_TARGET_DIR`; original never mutated). (a) In the scratch
`parse_that/src/regex/generated.rs`, change `parse_generated(&str)->bool` to return the `Hir` it
constructs internally (break-the-discard); thread the emitted structure if it builds none. (b)
**Parity oracle:** for each of ≥30 corpus patterns (JSON/Sheets/CSS leaves + the pass-3 tagged set),
assert `Dfa::compile(parse_generated_ast(p))` byte-identical minimal transition table to
`Dfa::compile(hir::parser::parse(p))`. (c) Diff-count `parse-that/rust/bootstrap/regex.bbnf` (72 LOC)
vs `grammar/misc/regex.bbnf` (27 LOC): enumerate rules to promote/delete (the de-dup surface).

**Command.** `cargo test -p parse_that --test grammar_driven_regex -- --nocapture` (in the scratch).

**Pass/fail.** generated-parser AST DFA-equivalent to the hand parser on **≥30/30** patterns AND the
de-dup surface quantified (N rules to promote, M to delete) ⇒ the grammar-driven path has its FIRST
artifact; SPEC-ONLY(grammar-driven) advances from "un-attempted" to "measured-feasible" for GESTALT-P.
Any DFA divergence ⇒ name the divergent construct (the exact LOCK-7A bootstrap-parity gap); the
namesake remains SPEC-ONLY with a concrete blocker, not a vague one.
