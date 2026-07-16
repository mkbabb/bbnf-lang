# LOCK-5 — THE REGEX CONTINUUM — pass-3 SYNTHESIS (FAMILY-SPECS)

Synthesis of pass-3 research-{A,B,C,D,priorart} against the pass-2 registry, the ratified
adjudications (XL-C, XL-A, XL-H), and every prior artifact — including the two prototypes that RAN
this pass (PROTO-1, PROTO-2). One cogent specification per family while the routes remain
incompatible; then the incompatibility map, the cross-lock double-homes LOCK-5 names back, and the
two sharpest runnable briefs. Elegance earns nothing on its own — a family stalled at a missing
primitive as hard as (or harder than) the original problem carries a BLOCKED marker.

**Settled ground (XL-C, RATIFIED — conveyed as fact, NOT re-opened here).** TWO domain-fit types:
the grammar tier keeps the regex leaf **opaque** (`Regex(StringId)` top-level / `Regex{pattern:String}`
skinny); the engine tier keeps the rich `Hir` grown with `Class`/`Look`; a cheap `string`/`format!`
bridge reusing the engine's `simplify_hir` joins them. 5A-identity is refuted; 5B-functor is retired;
the survivor — two types + cheap bridge — is the lock's Axis-1 verdict and is owned by neither
family's original charter. Canonical engine home is parse-that (one-regex-home); the vendored
parse-that-regex inside skinny is the MEASUREMENT SUBSTRATE (base-pin every number to HEAD, XL-H).
**Charter this pass: HARDEN the survivor with run evidence — not re-argue which type-plane wins.**
SPEC-ONLY(grammar-driven) rides on every finding until LOCK-7/7A.

---

## THE PASS-3 HEADLINE — the survivor's LOWERING POLICY got a run; its automata centerpiece is unrun a THIRD pass

Two prototypes ran, both base-pinned to `ce51c36fc` (clean fast-forward, ANCESTOR-OK, not
wrong-artifact-risk):

- **PROTO-1 (5D, G-5D.4) — RAN. The survivor's generator-path lowering policy resolves toward
  DIRECT-SCAN-DEFAULT.** Of 16 regex leaves across `json.bbnf` + `google-sheets.bbnf` + an injected
  counterexample, only **3/16 are genuine give-back** (`word /[a-z]+/`, `sheets::string`,
  `sheets::identifier`); ~12/16 are one-pass-safe (6 bounded-by-construction, 3 conservative
  false-positives, 3 truly sound). The FOLLOW predicate lands the `word "and" word` counterexample
  NON-disjoint (`overlap=[a]`) — **sound exactly where `check_one_pass_eligible` (`one_pass.rs:64`)
  is UNSOUND** (its `Repetition` arm greenlights a straight-line lowering that rejects `"a and b"`).
  **Verdict: the direct class-scan is the DEFAULT generator path gated by a FOLLOW-guard; the DFA
  retreats to a narrow fallback for the genuine-give-back trio.** LOAD-BEARING CAVEAT: the fraction
  ran on `analyze().first`, which **UNDER-approximates** alternations — the "sound" verdicts are not
  yet reliably sound; a production predicate must draw `first(C)` from the DFA start-state class set
  (over-approx). This is 5D's sharpest correctness obligation, still un-validated (Brief 1).

- **PROTO-2 (Fork-gating census, Q10) — RAN. Fork-B is AVAILABLE FOR FREE.** Over 33 Alt-of-regex
  patterns in the only tree where the grammar-tier regex e-graph physically exists (top-level
  `crates/ir/src/egraph/rules/regex.rs`): on **bare** `Alt([Regex,…])` the three regex-specific rules
  fire (`superset-absorb`/`union-merge`/`fuse` = 11/19/30 work); on **projected**
  `Alt([Map(Regex,Tag),…])` every rule applies **exactly 0** — `total_applied=0`, one iteration, no
  unions. `class_regex(child)` finds no bare `Regex` inside a `Map` e-class and returns `None`;
  `FuseAltRegexBranches` sets `all_fusable=false`. **Real BBNF alternations almost always carry
  per-branch tags** (`a->A | b->B`), so the `Map` barrier is the common case → the grammar-tier regex
  e-graph is a **consumer-less substrate under projection-domination** (Y.13) → **Fork-B (abrogate it)
  is free, and 5B's LOC-death is definitively a fork artifact, not a family win.**

**What did NOT run (a third pass in a row):** 5C's reference-Hopcroft control (the automata-as-extraction
centerpiece) and 5A's automata-parity / Look-consumer tests. Under the settled two-types verdict, 5A's
`from_expr≡from_hir` gate is **largely moot** — the string bridge routes to the engine's single existing
`from_hir`, so there is no second NFA builder to keep in parity (a consequence research-B named directly).
5C's centerpiece is the lock's one genuinely-unrun decisive number and is Brief 2.

---

## PRECONDITIONS (lock-wide, gate every family's earned convergence)

**P1 — the two-tree divergence, SHARPENED, with the gating number MEASURED.** The `->` representation
differs by tree (verified this pass): skinny lowers `->` to `ExprKind::Project{body,target,ty}`
(`skinny/crates/ir/src/lib.rs:261`) — a typed wrapping edge, with **no grammar-tier regex e-graph at
all**; the top-level tree lowers it to `GrammarENode::Map{inner,fn_id}` (`crates/ir/src/egraph/node.rs:52`)
— the `Map` e-node PROTO-2 proved makes the regex rules inert. PROTO-2 discharges registry directive #3:
the grammar-tier regex e-graph's projected-branch consumer density is **0**. GESTALT still picks the fork,
but the number now favors **Fork-B (abrogate)** decisively in the projection-dominated regime — subject
only to GESTALT counting how often real grammars present the tier a **bare** `Alt`-of-regex (PROTO-2 did
NOT count that frequency).

**P2 — grammar-driven is SPEC-ONLY (re-confirmed; ZERO grammar-driven artifacts after THREE passes).**
Neither pass-3 prototype exercised a grammar-driven regex path. `parse-that/.../regex/generated.rs:2288`
`parse_generated(&str)->bool` still **discards its AST**; two `regex.bbnf` still diverge
(`bootstrap/regex.bbnf` 72 LOC L4-spec vs `grammar/misc/regex.bbnf` 27 LOC legacy EBNF). The namesake work
is **promote + de-dup + break-the-discard**, gated LOCK-7/7A. **Every verdict line carries the
SPEC-ONLY(grammar-driven) label.**

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 45%** (maximal REFUTED pass-2; residual = the survivor's engine-tier leaf-growth + shadow-crate deletion; NO 5A prototype ran pass-3)

**Architecture (narrowed to the survivor).** The maximal "regex HIR IS a subset of the bbnf IR, nothing
to map, classify dissolves" claim is dead (PROTO-2, pass-2). What 5A contributes to the settled winner: the
`Class`/`Look` leaves the ENGINE-tier rich `Hir` must carry (they already exist in parse-that
`hir/mod.rs:117`), plus the deletion of the **name-gate heuristic** that reconstructs structure the grown
leaves already carry.

**Mechanism (pass-3, source-anchored).** The strongest surviving argument for any single-home instinct is
the name-gate: skinny's `bbnf_regex::classify` (`skinny/crates/bbnf-regex/src/lib.rs:92`) string-matches
`pattern == r"[ \t\n\r]*"`, `starts_with("-?(0|[1-9]")`, `starts_with('"')` — a Constitution-vi violation
reconstructing first-sets/nullability the `Class`/`Repeat` leaves carry by construction. Deleting the
heuristic block (`classify`, `regex_is_nullable`, `first_from_prefix`, `parse_leading_class`, `class_byte`,
~200 LOC) is a genuine skinny-side **SHRINK** (net negative LOC). research-A's honest correction: the
1148-LOC engine `classify.rs` **RELOCATES, not shrinks** — its tier content (`AccelDriven`,
`PrefixThenClass`, `CharClassQuantified`) is real information an IR pass keeps; the only deletion is the
internal re-lex step. A global "1148-LOC collapse" would overclaim.

**Q1 dispatch census (built, 7 arms).** Under the settled verdict the grammar tier stays opaque, so the
grown leaves live engine-side and the skinny arms consume the bridge output, not raw `Class`. The census
(nullability `ir/lib.rs:331`, pretty `:171`, types `passes:194`, scalar_routing `:348`, first_sets `:911`,
materialization `:1341`, trivial-false `:790/814/836/1108`) shows the survivor requires **no grammar-tier
regex dispatch** — the bridge hands each arm scalar facts (`nullable`, first-set, `RegexClass`), never a
byte-set. This is the survivor's Q2 confinement boundary: the 256-bit `ByteSet256` stays inside the engine
leaf; the grammar-tier type carries no byte payload (Constitution v preserved; serialized `GrammarIr` does
not bloat).

**Q2 typing.** `Class → Span` carrying a width-var solved as a bounded post-solve over LOCK-1's numeric
lattice; `Look → Unit`/zero-width (contributes `nullable=true`). Byte-expansion of `[α-ω]*` is CONFINED to
automata (`to_positive_byte_ranges` at NFA build; `CharClass::Unicode` panics there, so it never reaches the
type). Leak test the prototype owes: `[a-c]` and `[a-z]` infer **identical** `Span<width>`.

**Q4 the Look-consumer gap (a real correctness bug in the substrate).** The skinny shadow crate has **no
Look consumer** — `RegexKind` (`bbnf-regex/src/lib.rs:16`) has no anchor variant, so `/foo$/` and `/foo/`
are indistinguishable today (both over-accept). This is the consumer-less-substrate hazard (Constitution
viii) AND a live correctness defect the bridge fixes: re-pointing skinny's `analyze` through the engine's
`RegexInfo` (which parses `$` to `Look::End`) makes `/foo$/` reject `"foobar"`. The failing/passing test is
buildable and is part of the shadow-crate-deletion refactor residual.

**Integration seams.** Engine leaves already exist (`hir/mod.rs:117`). Skinny shrink: delete the ~200-LOC
heuristic block + the 322-LOC shadow crate; re-point `use bbnf_regex::analyze` (`ir/src/lib.rs:1`) to the
engine's `RegexInfo::analyze_from_hir`. Break-the-discard + de-dup are LOCK-7-gated.

**Gates it carries.** (G-5A.2) the `/foo$/` Look-consumer test that FAILS on the shadow path, PASSES through
the bridge — UNBUILT. (G-5A.shrink) skinny ΔLOC (heuristic + shadow deleted) measured, engine classify
relocation measured separately — UNBUILT. (G-5A.leak) `[a-c]≡[a-z]` identical `Span` — UNBUILT.
(G-5A.parser-cost) deleting the heuristic forces skinny to consume a REAL parser (parse-that `hir/parser/`
~787 LOC) — the shrink is contingent on paying that import.

**Exact open questions.** (1) Does the shadow-crate deletion net-shrink after the parser import is paid?
(2) Does re-pointing `analyze` change any `nullability` answer — are all deltas strict correctness
improvements (the anchor-blind fix)? (3) Is `from_expr` needed at all under the settled verdict — research-B
says NO (bridge uses `from_hir`); if so G-5A.3 is retired as moot.

**BLOCKED assessment: NOT BLOCKED** — the residual is a refactor (name-gate deletion + shadow deletion +
bridge re-point), no missing primitive. But the family's *distinguishing* maximal mechanism is dead; what
survives is the survivor's engine-tier hardening, largely already-existing. Convergence held at **45%**:
analysis sharpened (classify-relocates-not-shrinks honestly conceded; parser-import cost surfaced), but no
5A prototype ran a third pass and the residual is refactor-shaped, not architecture-shaped.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **RETIRED — 40%** (retire STRENGTHENED by PROTO-2; the one live residual is a refactor, not the functor)

**Status.** RETIRED (earned-conditional, pass-2). PROTO-2 this pass **strengthened** the retire: the
grammar-tier regex e-graph — the functor's value target — applies **zero work** on projected branches (the
common case), so the 250-LOC de-dup the functor claimed is deleted **for free under Fork-B**, attributable
to no family. The functor as an architecture with independent value is dead; its soundness win (the
projection-dropping-union bug) was already a non-bug under the whole-leaf wrapper (pass-2).

**What research-B surfaced that is NOT the functor (and is credited elsewhere).** research-B correctly
distinguishes THREE homes: the opaque grammar leaf (keep), the canonical engine rich `Hir` (keep), and the
**divergent shadow** — skinny's 322-LOC `bbnf-regex` with its own anchor-blind `classify`. Deleting the
shadow and re-pointing skinny's `analyze` through the engine's `RegexInfo::analyze_from_hir` is a concrete,
runnable, **correctness-improving** refactor (net −322, fixes the anchor-blind nullability bug). **This is
the survivor's bridge-hardening work (homed in 5A's residual / the BANKED refactor residual), NOT a 5B
functor win** — crediting it to 5B would launder a retired family's disposition. research-B's "total reverse
functor" (`Hir->String` structural unparse fixing `canonical_pattern`'s 4-shape partiality) is real
engineering, but it serves the string BRIDGE (the survivor), not a two-way adjunction — the bridge needs
only forward parse + a total unparse for round-trip oracles, which is exactly the survivor's `simplify_hir`
reuse, not the functor's independent value.

**Un-retire gate (unchanged, LOCK-1-gated, now a two-tree question).** Only LOCK-1 abandoning the whole-leaf
wrapper representation of `->` reopens 5B. skinny already uses `Project` (a typed edge, not a `Map`); the
top-level tree uses `Map`. research-A argues `->` never binds sub-leaf captures (projection wraps the WHOLE
regex, so the bridge round-trip is sound because nothing structural crosses the boundary). If LOCK-1
ratifies a representation where a projection binds a *sub-leaf* capture (5D's capture-leaf ambition), the
projection-dropping-union bug reappears and un-retires 5B. Sole re-open trigger; routed to LOCK-1 with the
PROTO-2 evidence.

**BLOCKED assessment: N/A (RETIRED).** Convergence held at **40%** — disposition-certainty of a conditional
retire, raised in confidence by PROTO-2 (fork-independence now measured, not estimated), but not a
convergence-toward-shipping number.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **ADVANCED — 45%** (centerpiece unrun a THIRD pass + prior-art-predicted loss; 2B-as-minimizer BLOCKED; honest survivor = 2C-as-table + delete-`:358`)

**Architecture.** Push the shared e-graph one level below the HIR, to the automaton: minimization as
congruence closure, determinization as derivative-saturation, tier-selection as CSP extraction. Orthogonal
to the Axis-1 verdict (rides below the survivor's engine `Hir`).

**The load-bearing defect (research-C, verified).** `parse-that/.../automata/dfa.rs:291`
`hopcroft_minimize` **is not Hopcroft** — it is Moore partition-refinement with a **full restart on every
split** (`dfa.rs:358` `break; // restart refinement from the beginning`), worst case ~O(n³·k). **Every prior
"acyclic fast path beats Hopcroft" claim was measured against this strawman.** The centerpiece cannot be
adjudicated until a correct O(n log n) control exists — and it has NOT been built after three passes.

**Three sub-verdicts.**
- **2A minimization-as-congruence — EXACT (26/26 byte-identical), perf NO-GO wholesale (3.53× slower,
  pass-2), a 15% acyclic sliver contingent on an unbuilt alloc-light rewrite.** research-C's honest framing:
  the congruence framing is genuinely linear for the **acyclic** half (Revuz single reverse-topological
  pass) and a **loss** for the cyclic half (iterated hashcons O(n²·k), same class as the strawman). The real
  defensible win is **deleting the `:358` restart**, not the acyclic special case per se. The decisive
  question — *does the acyclic path earn its detection cost against a CORRECT control, or does deleting
  `:358` alone erase 2A's reason to exist?* — is UNRUN. Prior art (TRAP A, Remy Wang: e-graphs ARE tree
  automata; the direction that works is automata→e-graph, NOT e-graph→automata; DAG-extraction is NP-hard
  and dominates eqsat time) **predicts the centerpiece loses**.
- **2B determinization-as-derivative-saturation — BLOCKED (as minimizer).** Kozen 1994: regular-language
  star-equivalence needs the star-induction inference rule, not a finite ground rewrite → **no finite
  e-graph rule set is complete** — a provably-impossible missing primitive (the lock's cleanest BLOCKED).
  2B-as-*determinizer* survives (25/26 ρ=1); 2A/Hopcroft closes.
- **2C prefilter/tier-selection — SETTLED toward TABLE, not CSP (research-C's clean pass-3 advance).** A
  grep for any global codegen/LUT-budget coupling (`lut_budget`, `codegen_budget`, `shared_lut`,
  `table_budget`) over `crates/ir/src/` returns **ZERO hits**. Each pattern's tier derives from its own
  start-state row + own literal prefix — `Prefilter::from_dfa` is O(256) local; `derive_feasible_engines`
  (`info/mod.rs:344`) reads only per-pattern fields. **No shared resource → tier selection is LOCAL → the
  monotone `EngineSet` table wins; a CSP over it is a consumer-less substrate (Y.13).** The exact condition
  that flips it to CSP: a bounded per-parser cap on distinct shufti nibble-LUTs (32 B each) or Teddy buckets
  — which **does not exist today**. Named missing input: a **byte-rarity oracle** (regex-automata's static
  ASCII-rank table), absent in-tree, without which accel-byte selection is guesswork.

**Integration seams.** Minimize swap `dfa.rs:89-91`; partial-DFA `DEAD` handling `dfa.rs:330/340/423`
(Valmari's partition-over-transitions replaces the `usize::MAX` block hack); byte-equiv classes done
(`sets/equiv.rs`, `num_classes≤8` → signatures radix-sortable); tier table `info/mod.rs:344`; CSP consumer
`crates/ir/src/passes/{csp_domains,decision_csp}.rs`.

**Gates it carries.** (G-5C.1) reference O(n·k·log n) control — Valmari–Lehtinen (native partial-DFA) OR
Hopcroft+predecessor-lists — + the delete-`:358` null, on a GROWN (≥50, tagged) corpus with `is_acyclic`
dispatch folded across ALL patterns — UNRUN. (G-5C.2) congruence rebuild proven O(n·k·log n), not
degenerate-Moore — UNPROVEN. (G-5C.ρ) quality-debt `minimal(egraph)−minimal(Hopcroft)` — UNMEASURED.
(G-5C.tier) research-C demonstrated table-suffices (zero witness); the CSP downgrade is earned. Corpus still
26 (`COMPILE_PATTERNS`); must grow to ≥50 tagged.

**Exact open questions.** (1) Does the acyclic fast path beat a *correct* control, or does fixing `:358`
alone erase its reason to exist? (2) Does the congruence rebuild hit O(n·k·log n)? (3) Is the 15% slice
net-positive after `is_acyclic` dispatch on the other 85%?

**BLOCKED assessment: FAMILY NOT BLOCKED; internal 2B-as-minimizer BLOCKED (Kozen).** The maximal claim
(e-graph automata beats the classical pipeline at engine scale) is **effectively refuted by prior art +
unrun three passes** — the honest surviving deliverable is **2C-as-table (settled) + the delete-`:358`
correctness/perf fix**, neither of which needs the e-graph-over-automata mechanism. The family is narrowing
hard toward "a tier table + a bug fix"; its distinguishing centerpiece is on the retire path. Convergence
held at **45%** (2C settled toward table is a real advance; the centerpiece staying unrun a third pass is
the ceiling). Brief 2 forces the number that ends it.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **ADVANCING — 45%** (was BANKED; the survivor's generator-path lowering policy; PROTO-1 RAN the gating fraction; the cold-pair + over-approx guard remain)

**Architecture.** A `/pat/` lowers DIRECTLY into native grammar IR — a straight-line class-scan
(`Recognizer::ByteClass` membership over the tape, no automaton) when the leaf is FOLLOW-sound; a
`Seq[Repeat, FOLLOW-guard]` or the DFA fallback when it is genuine give-back. This is the
Glushkov/Berry-Sethi position automaton (FIRST/LAST/FOLLOW over a marked expression) done as an IR lowering.
Under the settled verdict, 5D is **the generator-path lowering policy of the two-types survivor** — not of
the dead identity-merge, and not a second type plane.

**Mechanism, with PROTO-1's run behind it.** PROTO-1 established the partition (3/16 genuine give-back,
~12/16 one-pass-safe) → **direct-scan-default, DFA-narrow-fallback**. The FOLLOW predicate is a genuine
Changed-flag fixed point (nullable→first→follow, per-`ExprId`, `Ref` sites bridging inter-rule) that DEFEATS
the unsound `check_one_pass_eligible` (`one_pass.rs:64`, `Repetition` arm never checks its class against its
FOLLOW). The elegant fix is exactly the desugar: `Seq[Repeat, suffix]` + grammar `AltMode::Dispatch` decides
soundness in the one place the grammar already reasons about FOLLOW (priorart: regex-automata `dfa::onepass`
uses build-and-bail, not a pre-deciding predicate — a hand-written predicate is itself the trap).

**Q8 the substrate gap (source-confirmed, UNBUILT).** There is NO direct class-scan and NO `ByteClass` node:
`derive_recognizers` (`passes/src/lib.rs:338`) emits `Recognizer::SimdScan` ONLY for structural bytes
`{}[],:"` or a `QuotedString`-keyed `Regex`; a general `[a-z]` falls to `_ => {}` — the **masked wildcard**.
No `Recognizer::ByteClass` variant, no `BackendExpr::ByteClass` anywhere (grep: zero). The prototype must add
`Recognizer::ByteClass{set,site}` (`ir/src/lib.rs:416`), gate `derive_recognizers` on the FOLLOW fixed point
(replacing the `QuotedString` mask), and route `BackendExpr::ByteClass` to the tape
(`codegen/src/lower/tape_plan.rs`).

**THE load-bearing caveat (5D's sharpest correctness obligation).** PROTO-1's fraction ran on
`analyze().first`, which **UNDER-approximates** alternations (for `/'…'!|[A-Za-z_]\w*!/` it returns
`Exact{'\''}`, dropping the letter branch) and returns `UNKNOWN` for `sheets::number`. An UNDER-approximate
first-set makes the FOLLOW-guard **unsound** — it could greenlight a direct scan that mishandles a dropped
branch. A sound production predicate must draw `first(C)` from an **OVER-approximation** — the DFA
start-state class set, which PROVE-10's prefilter already computes. Re-running the fraction under the
over-approx source (and confirming the 3/16 genuine-give-back count holds, no leaf flips sound→unsound) is
the un-validated correctness gate.

**Q10 e-graph (measured, PROTO-2).** 5D's direct lowering does NOT route through the grammar-tier regex
e-graph (0/0/0 on projected branches) — it lowers the leaf straight to `ByteClass`/`Seq[Repeat,guard]`. The
census is **evidence FOR 5D**: the direct path needs no grammar-tier e-graph, and keeping one on the
generator path is un-consumed substrate.

**Gates it carries.** (G-5D.4) FOLLOW-sound fraction — RAN (3/16 genuine), but under the UNDER-approx source;
re-run under over-approx. (G-5D.over-approx) the over-approx guard preserves the 3/16 partition — UNBUILT.
(G-5D.3) cold-paired direct class-scan vs prefiltered `Dfa::find` (rare-class AND dense-class leaf, N≥200
cold) — the ratio decides DFA reachability on the generator path — UNBUILT. (G-5D.tape) `ByteClass` reaches
the tape non-inert — UNBUILT. (G-5D.capture) capture-leaf → `Project` byte-identical to a hand-authored
grammar capture, under LOCK-1's ratified `->` — UNBUILT.

**Exact open questions.** (1) Under an over-approx first-set, does the direct-scan-default verdict survive,
or does the genuine-give-back count climb? (2) Does the direct scan tie-or-beat `Dfa::find` on both a
rare-class and a dense-class leaf (→ DFA off the generator path), or lose on dense (→ DFA stays reachable)?
(3) Does `->` bind sub-leaf captures — the LOCK-1 fact that also gates the 5B re-open trigger?

**BLOCKED assessment: NOT BLOCKED** — a named narrowing (DFA stays PROVE-10-floor-reachable as fallback)
plus concrete edits, no missing primitive. Convergence raised 35→**45**: the first decisive number landed
(direct-scan-default verdict directionally established), but the over-approx soundness gate and the entire
cold-pair remain unbuilt. Brief 1 completes both.

---

## INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The Axis-1 mutual exclusion (5A-identity ⊗ 5B-functor) is **CLOSED** — both horns ran and failed pass-2;
the survivor (two types + bridge) is ratified (XL-C). What remains are the residual incompatibilities the
survivor inherits:

- **5A-residual ∧ 5D — COMPATIBLE, one family, differ only on generator-path lowering.** Both harden the
  survivor: 5A deletes the name-gate + shadow crate and hardens the bridge; 5D adds the direct class-scan
  lowering the bridge feeds. They share the `Class`/`Look` engine leaves and the `ByteClass` chooser. Their
  only residual tension is the anchor/`Look` consumer — both answer the same Y.13 question (5A's `/foo$/`
  test answers it for both). Not mutually exclusive; jointly the survivor's harden + lower policy.

- **5B ⊗ 5D — the dead exclusion, now moot.** 5B bridged the opaque leaf with a functor; 5D dissolves the
  leaf into native nodes at desugar. With 5B RETIRED and the survivor's bridge string-based, there is
  nothing to reconcile — 5D lowers the leaf, the bridge serves the fallback DFA path.

- **5C ∥ {5A-residual, 5D} — orthogonal, below the HIR.** 5C operates on the automaton, consuming whatever
  the engine `Hir` produces. GRAFT-1 (2B-determinizer saturates over the engine's derived `Language`) and
  GRAFT-2 (2A's byte-class partition consumes the `Class` byte-sets) hold engine-side. 5C's only exclusion is
  internal: 2B-as-minimizer (BLOCKED, Kozen) cannot replace 2A/Hopcroft.

- **The SURVIVING meta-tension (a fork, not a family exclusion): Fork-A vs Fork-B.** PROTO-2 made Fork-B
  free in the projection-dominated regime. Fork-A (keep the grammar-tier regex e-graph) is required ONLY if
  GESTALT finds real grammars route **bare** `Alt`-of-regex to the tier — a frequency PROTO-2 explicitly did
  NOT count. The one open incompatibility above the families; GESTALT's to close; the number leans abrogate.

- **The LOCK-1 re-open trigger (cross-lock, not intra-lock).** If LOCK-1 ratifies a `->` representation that
  binds SUB-LEAF captures (5D's capture-leaf ambition, `(/[0-9]+/->Digits)` reaching inside the class), then
  (a) 5D's `Project` byte-identity oracle must handle the Map-unwrap and (b) 5B's projection-dropping-union
  bug reappears and un-retires 5B. Both hang on the same unratified LOCK-1 fact.

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; resolutions are LOCK-5 proposals GESTALT RATIFIES)

1. **Axis-1 verdict ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C — RE-CUT (carried forward, still owed).** All three
   were written against "ONE-HIR." The verdict is **two domain-fit types + cheap bridge**. 4D's premise ("one
   HIR, not a second engine") is weakened; 4D consumes the two-types landing or converges on a refuted
   verdict. Re-cut owed at GESTALT; still nobody's charter this pass.
2. **5B retirement ⊗ LOCK-1 (RE-OPEN TRIGGER).** The retire rests on `->` staying a whole-leaf wrapper
   (`Project` skinny / `Map` top-level) that never binds sub-leaf captures. LOCK-1 owns the real `->` typing
   and has not ratified it. Sub-leaf-capture representation un-retires 5B AND breaks 5D's capture oracle —
   routed to LOCK-1 with the PROTO-2 evidence.
3. **5C-2C ⊗ LOCK-2 scan-plan/tier CSP — SINGLE-HOME IN 5C (research-C discharged the witness search).**
   Zero global-budget coupling exists (grep-confirmed) → the tier is a LOCAL table homed in 5C; escalate to
   LOCK-2's CSP home ONLY on an exhibited shared-LUT/Teddy-bucket budget witness. LOCK-2 has not deferred it
   — proposed single-home, GESTALT ratifies.
4. **5A/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA) — SUPERSESSION (proposed).** LOCK-5 owns the `Hir`→DFA→classify
   substrate + the `ByteClass` chooser; 9C consumes it or retires. But the substrate LOCK-5 offers 9C is
   itself UNBUILT (the `ByteClass` recognizer does not fire — masked wildcard). 9C's dependency is real but
   un-delivered; Brief 1 begins delivering it.
5. **5A ⊗ LOCK-7 bootstrap.** Two divergent `regex.bbnf` (one discards its AST). Re-scoped: "promote +
   de-dup + break-the-discard"; SPEC-ONLY(bootstrap) until 7A. Zero artifacts after three passes.
6. **Type-plane ⊗ LOCK-1.** `Class ⊢ Span`/`Look ⊢ Unit` + the width-var graded only against skinny's stub
   `BuiltinTy`. Route through LOCK-1's real HM + row-poly + widths.
7. **rust/ts ⊗ LOCK-6.** The mirror/skeleton carries BOTH types (`Regex(StringId)` grammar-side + rich
   `Hir`/`Class`/`Look`/`ByteClass` engine-side + automata crossing to TS, PROVE-14 tape) — a larger
   obligation than "carry the merged enum." Named gap for GESTALT-Ω; the serde-derive-ownership question
   (who adds `CharClass: Serialize`) lands on the parse-that READ-ONLY / LOCK-6 boundary.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs for pass-4)

Brief 1 completes the **survivor's generator-path lowering policy** (the highest-leverage residual — it
gates LOCK-9-9C and LOCK-4-4D, and it is the family with run-momentum), closing both the over-approx
soundness gate and the cold-pair arbiter PROTO-1 left open. Brief 2 forces the **one genuinely-decisive
number unrun after three passes** — the reference control that either keeps or retires 5C's
automata-as-extraction centerpiece, letting GESTALT dispose of the family cleanly. 5A's remaining tests
(shadow-deletion, `/foo$/`) are refactor-shaped and lower-stakes; deferred.

### Brief 1 — 5D: over-approx FOLLOW-guard + `ByteClass` direct path + cold-pair vs `Dfa::find` (`bbnf-worktree`)

**Question.** Under a SOUND (over-approximating) first-set, does the direct-scan-default verdict survive,
and does the direct class-scan tie-or-beat prefiltered `Dfa::find` — removing the DFA from the generator
path — with byte-identical tape?

**Tree/method.** Worktree off `bbnf-lang` master (base-pin, XL-H). (a) Replace `first(C)` in
`skinny/crates/passes/tests/follow_sound_fraction.rs` with the **DFA start-state class set** (over-approx,
from `Prefilter::from_dfa` / `Dfa` start row) instead of `analyze().first`; re-run
`cargo test -p passes --test follow_sound_fraction -- --nocapture`; assert the genuine-give-back set is
still exactly `{word, sheets::string, sheets::identifier}` and NO leaf flips sound→unsound. (b) Add
`Recognizer::ByteClass{set,site}` at `skinny/crates/ir/src/lib.rs:416`; gate `derive_recognizers`
(`passes/src/lib.rs:338`) on the FOLLOW fixed point (replacing the `QuotedString` mask); route
`BackendExpr::ByteClass` to the tape (`codegen/src/lower/tape_plan.rs`). (c) Cold-pair (N≥200, paired
same-binary, correctness assert at setup) the direct membership scan vs `Dfa::find` on a **rare-class**
(`#[0-9a-fA-F]{6}`) and a **dense-class** (`[a-z]+`) leaf.

**Pass/fail.** Genuine-give-back = **3/16 under over-approx** (no sound→unsound flip) AND emitted tape
byte-identical between direct-scan and `Dfa::find` AND direct-scan ratio **≥ 0.98×** `Dfa::find` on BOTH
leaves ⇒ DFA leaves the generator path; direct-scan-default RATIFIED, DFA→narrow fallback. Any sound→unsound
flip, OR direct-scan **< 0.95×** on the dense leaf ⇒ DFA stays reachable on the generator path; the fallback
is not "narrow" — re-home 5D's lowering policy accordingly.

### Brief 2 — 5C: reference O(n·k·log n) control + delete-`:358` null on a grown tagged corpus (`pt-scratch`)

**Question.** After the incumbent strawman is fixed, does minimization-as-congruence (acyclic Revuz
fast-path) beat a CORRECT control, or does deleting the `:358` restart alone erase the e-graph centerpiece's
reason to exist?

**Tree/method.** Scratch in `parse-that/rust/regex` (base-pin, XL-H). (a) Grow
`benches/regex_dfa_compile.rs COMPILE_PATTERNS` from 26 to **≥50**, each tagged acyclic/cyclic by a
back-edge DFS. (b) Implement three minimizers alongside the incumbent restart-refiner: **Valmari–Lehtinen**
(native partial-DFA, the honest control for this engine's `DEAD` transitions), **Revuz** acyclic single-pass,
the **`DfaSig` congruence** engine on `crates/egraph`. (c) Correctness oracle FIRST: all four emit
**byte-identical** minimal state-counts + transition tables on every pattern (a divergence is a bug, caught
before timing). (d) Time minimize per pattern per tag; measure the congruence rebuild's passes-to-fixpoint
against the O(n·k·log n) claim; fold `is_acyclic` dispatch cost across ALL patterns; run the **delete-`:358`
null** (incumbent minus the restart) as a fourth timing lane. Command:
`cargo bench -p bbnf-regex --bench regex_dfa_compile`.

**Pass/fail.** Revuz-acyclic beats correct Valmari on the acyclic tag by **≥1.3×** AND the congruence
rebuild is measured O(n·k·log n) (not degenerate-Moore) AND the 15% acyclic slice is net-positive after
dispatch on the 85% ⇒ 2A acyclic fast-path SURVIVES against a correct control. Delete-`:358` alone recovers
the acyclic-tag win (congruence/Revuz ties the fixed incumbent) OR the congruence rebuild degenerates to
O(n²·k) ⇒ the e-graph-over-automata centerpiece RETIRES; 5C collapses to 2C-as-table + the `:358` bug fix
(named negative result, the constitution's own disposition).
