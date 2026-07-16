# LOCK-5 — THE REGEX CONTINUUM — pass-3 SYNTHESIS (FAMILY-SPECS)

Synthesis of pass-3 research-{A,B,C,D,priorart} against the pass-2 registry (both within-lock critic
floors), every prior artifact (PROTO-2 5A merge census, PROTO-1 5B un-bank gate), the ratified
cross-lock ground (XL-C, XL-A, XL-H), and the pass-3 research directives. One cogent specification
per family; then the incompatibility map, the cross-lock double-homes named back, and the two
sharpest cross-family uncertainties as runnable briefs.

## 0. THE RATIFIED GROUND RE-CUTS THE FAMILIES (XL-C is a fact, not a horn)

**XL-C (RATIFIED, binding):** two domain-fit types — the grammar tier keeps `Regex(StringId)`
opaque; the engine tier keeps the rich `Hir` grown with `Class`/`Look`; a cheap `string`/`format!`
bridge reusing the engine's `simplify_hir` joins them. The parse-that engine is the one-regex-home;
the vendored skinny `parse-that-regex` is the MEASUREMENT SUBSTRATE (base-pin every number to HEAD,
XL-H). **The Axis-1 type-plane contest is closed.** The pass-3 charter is *harden the ratified
substrate with run evidence*, not re-argue which type-plane wins.

Under that closure the four families are no longer competing type-planes — they are the four
work-streams the ratified substrate decomposes into, and their *remaining* incompatibilities have
migrated off the type axis and onto **the generator path** (5C-engine-on-path ⊥ 5D-no-engine) and
**the tree fork** (Fork-A keep grammar-tier e-graph ⊥ Fork-B abrogate). The re-cut:

| Family | Pass-2 identity | Pass-3 identity under XL-C | Status |
|---|---|---|---|
| 5A | ONE-HIR (identity merge) | **LEAF-GROWTH + CONSUMER RE-HOME** — grammar-side hardening: delete the shadow scanners, consume the engine's one classify/first, cache facts on the node | ADVANCED |
| 5B | LAWFUL-FUNCTOR | **THE BRIDGE** — functor-as-architecture RETIRED (PROTO-1); live residual = the string bridge XL-C *mandates but which is unbuilt* (`Display for Hir` does not exist) | RETIRED-architecture / live-residual |
| 5C | AUTOMATA-AS-EXTRACTION | **ENGINE-TIER OPTIMIZATION** — delete-the-restart (Null-A), Revuz acyclic dispatch, the LOCAL prefilter tier table; 2B-as-minimizer BLOCKED (Kozen) | ADVANCED / one BLOCKED horn |
| 5D | GRAMMAR-FRAGMENT | **GENERATOR-PATH LOWERING POLICY** — direct class-scan into grammar IR on the FOLLOW-sound subset; the DFA engine is the total fallback | BANKED |

**No family is a BLOCKED-candidate wholesale.** The one provably-impossible missing primitive in the
lock is *internal* to 5C: 2B-as-a-minimizer (equality-saturation minimizes the DFA) is BLOCKED by
Kozen 1994 — no finite ground rewrite set is complete for star-equivalence (ρ_max 1.667 measured) —
but 2B-as-determinizer survives (ρ=1, 25/26) and 5C the family survives on classical minimization +
acyclic dispatch + the tier table. Marked at 5C below, not at the family head.

**Two preconditions still gate every earned convergence (unchanged, re-confirmed).**
- **P1 — the tree fork is unpicked.** The grammar-tier regex e-graph (`crates/ir/src/egraph/rules/
  regex.rs`, 434→184 LOC) exists ONLY in top-level `crates/ir`; skinny ships without one. Fork-A
  folds it in; Fork-B deletes it for free. PROTO-1 found the surviving 184 LOC structurally inert on
  `Map`-wrapped (projected) branches → the consumer-density number is unrun and may be ~0. This is
  BRIEF 2. XL-A homes the fork at GESTALT; LOCK-5 owes it the number.
- **P2 — grammar-driven is SPEC-ONLY lock-wide.** Zero grammar-driven regex artifacts after two
  passes. Two `regex.bbnf` diverge; `parse_generated()` returns `bool` and discards its AST. Every
  verdict line below carries the **SPEC-ONLY(grammar-driven)** label until LOCK-7/7A.

---

## FAMILY 5A — LEAF-GROWTH + CONSUMER RE-HOME — **ADVANCED — 48%** (maximal merge REFUTED; the narrowed re-home is the ratified substrate's grammar-side hardening)

**Architecture.** The regex leaf lives *twice* today: the grammar/bbnf tier re-derives regex facts
from raw pattern bytes (skinny `bbnf-regex`, 322 LOC, a four-tag string-comparison classifier;
`codegen/general_descent/regex_first.rs`, 230 LOC, a second leading-token parser; `json_sink_direct/
first_set.rs`, ~40 LOC), while the engine tier already carries the one rich `Hir` with `Class`/`Look`
already grown and already under test. 5A's narrowed, ratified-consistent claim: **delete the shadow,
consume the engine.** The grammar tier stays opaque (`Regex(pattern)` unchanged, XL-C); every bbnf
site that string-scans a pattern instead reads facts off the engine's `classify_regex(&str)` /
`regex_first_chars(&str)` (each parses internally). One parser, one first-set, one classify — the
engine's — consumed by both planes. This is the lawful reconciliation of "ONE-HIR" with the two-type
ratification: the *duplicate* dies, not the type.

**Mechanism.** (1) **Consumer re-home — SHRINK, not relocate.** Delete `regex_first.rs` (230) + the
skinny string-scanner (~200 of 322) + `json/first_set.rs::regex_first_bytes` (~40) ≈ **−470 bbnf-side
LOC**, adding ~0 engine-side LOC (the classify/first machinery is not moved — it is already the
canonical home). (2) **Compute-at-bridge, carry-on-node** to kill repeated parsing: extend
`BackendExpr::RegexProgram{pattern, span_kind}` with a cached `class: RegexClass` (+ optional
`first: CharSet128`, `nullable: bool`) computed ONCE at the `ExprKind::Regex → RegexProgram` lowering;
downstream passes read the cached facts. (3) **Leaf-growth is engine-side and already complete** —
`Class`/`Look` are grown in `hir/mod.rs`; `Look::` is consumed in exactly three files (`automata/
nfa.rs`, `hir/parser/atom.rs`, `info/classify.rs`) — a narrow auditable surface; 5A adds nothing to it.

**Integration seams (path:line, verified).** grammar leaf `ir/src/lib.rs:250`; backend node `:389`;
nullability `ir/src/lib.rs:331`; passes routing `passes/src/lib.rs:225,349`, facts `:944`, match
`:1665`; general first-set `codegen/general_descent/regex_first.rs` (the 230 to delete); json
`json_sink_direct/first_set.rs:77`; skinny scanner `bbnf-regex/src/lib.rs:45,92` (the ~200 to delete).
Engine one-home: `classify.rs:157` (`classify_regex(&str)`), `first.rs:13` (`regex_first_chars(&str)`).

**Gates (born-RED, falsifiable).**
- **G-5A.re-home:** delete the three shadow derivers, route all consumers through
  `bbnf_regex::{classify_regex, regex_first_chars}`; `cargo test -p passes -p codegen -p runtime`
  GREEN; measured LOC delta ≤ −400 (SHRINK, not relocate). **FAIL if** any skinny call has no engine
  cover, or LOC merely relocates.
- **G-5A.classify-agreement:** over the live grammar corpus, `RegexClass` from the engine ≡ the
  current `RegexKind` routing for every regex leaf **before** deletion — the named counterexample is
  skinny's hardcoded `"-?(0|[1-9]"` numeric literal; if a live grammar's number regex differs by a
  byte the two paths disagree. **FAIL if** any leaf's routing flips.
- **G-5A.2 (Look-consumer, can FAIL):** compile `Dfa` for `/foo$/` and `/foo/`; assert
  `find("foobar")` is `None` on the anchored pattern (end-anchor consulted in `from_hir`) and `0..3`
  on the bare one. A stripped-anchor lowering or the byte-positional skinny scanner (which reads `$`
  as `0x24`) FLIPS the first assertion. **FAIL if** it matches.
- **G-5A.3 (automata parity, UNRUN two passes):** a grammar `ExprKind` fragment lowered to `Hir`
  yields a minimized-DFA byte-identical to the engine parsing the twin regex, post-`simplify_hir`
  (Group-elision + `Optional↔Repeat{0,1}` normalized). Compare at the **minimized-DFA** level, not
  raw NFA state-ids (priority-tagged epsilons make raw equality fragile). Throughput freshly re-run,
  NO inherited PROVE-10 band.

**Exact open questions.** (Q2/LOCK-1) `Class`/`Look` type as a finite `Named` tag keyed on the class,
NOT a byte set — byte-expansion (`to_positive_byte_ranges`, panics on `Unicode`) is confined to
`automata/`; route the tag through LOCK-1's real HM, not skinny's stub `BuiltinTy`. (Q11) a named
tier-disagreement LOCK-1 must reconcile: `->` is a `Map` barrier at *codegen* (`view.rs:35`
`classify_projection`) but a *transparent pass-through* at inference (`passes/src/lib.rs:204`
`Project{body,..} => infer_expr(body)` drops `target`/`ty`) — the narrowed leaf type never meets the
declared `ty` in the type plane; LOCK-1 must make `->` a typed edge that unifies body-type against
`ty` while keeping the codegen decode barrier. (Q1) leak *magnitude* soft (pass/fail robust; the
count is not load-bearing). SPEC-ONLY(grammar-driven).

---

## FAMILY 5B — THE BRIDGE — **RETIRED-architecture / LIVE-residual — 40%** (functor retired PROTO-1; the residual is the bridge XL-C mandates and which is UNBUILT)

**Architecture.** The functor-as-independent-architecture is RETIRED by measurement (PROTO-1: copy
dies 250 LOC + 59/59 DFA-equivalence, but the projection-dropping-union soundness bug it was banked to
fix is a **non-bug** under the `Map`-wrapper representation of `->` → the win reduces to a bare
string-bridge merge). What survives, and is now load-bearing, is a fact pass-3 research-B surfaced:
**XL-C ratified a "cheap `string`/`format!` bridge reusing `simplify_hir`" as if it existed — but its
reverse leg does not.** `rg 'impl.*Display for Hir|fn to_pattern'` over `parse-that/rust/regex/src`
returns **zero**. There is no `Hir → String`. The forward leg is a tautology (`Nfa::from_pattern` =
`from_hir(parse(pattern))`, `automata/nfa.rs:63` — do NOT report as an oracle); the reverse leg —
needed whenever a canonicalized `Hir` must be written back into a grammar-tier `Regex(String)` — is
the bridge's unbuilt half. 5B's disposition: **the family is retired; its residual is the bridge
XL-C obliges, and building it is a prerequisite for XL-C to hold, not a new architecture.**

**Mechanism.** Build `Display for Hir` (~80 LOC, colocated `parse-that/rust/regex/src/hir/`): all
eight variants incl. `CharClass` negation-aware rendering (`negated` bit, never materialize the
complement) and `Look → ^/$/`line-anchors. Reconcile with the rival partial reverse map
`RegexClass::canonical_pattern()` (`classify.rs:132`, returns `None` on the data-carrying variants) —
derive `canonical_pattern` *from* `Display` (class → representative `Hir` → display), never two truths.

**Integration seams.** `hir/mod.rs:216` (the `Hir` enum), `:45` (`CharClass` + `negated`), `:172`
(`Look`); `egraph/mod.rs:143` (`simplify_hir`, the canonicalizer the bridge must round-trip through);
`classify.rs:132` (`canonical_pattern`, the rival to reconcile).

**Gates (born-RED).**
- **G-5B.reverse:** `simplify_hir(parse(display(h))) == simplify_hir(h)` GREEN over ≥50 patterns
  (tagged acyclic/cyclic); each failure names an un-normalized non-isomorphism (start with
  `Optional`/`Group`, §4). This certifies the `format!` bridge is *lawful*, not lossy.
- **G-5B.forward:** `L(from_hir(h)) == L(from_hir(simplify_hir(h)))` by NFA match-set equality —
  canonicalization is language-preserving (the bridge is sound). **FAIL** = canonicalization changes
  the recognized language.
- **G-5B.no-two-truths:** `canonical_pattern(class) ≡ display(representative_hir(class))`. FAIL if two
  divergent engine→string paths persist.

**Exact open questions.** The one live re-open trigger for the *functor* (not the bridge) is
LOCK-1-owned: **if LOCK-1's real `->` typing represents projection as anything other than an
inner-class `Map` barrier, the projection-dropping-union bug reappears and un-retires 5B** (G5B-a).
The joint-coproduct variant dies undesigned (acceptable — its core value axis is now a proven
non-bug). Bridge risk: `Display` after `simplify_hir` may reorder `Alternation` branches; if an
`AltMode::Dispatch` consumer depends on branch order, re-embedding changes dispatch — the reverse
oracle compares *languages*, and G fires only for `AltMode::Speculative` leaves or re-runs dispatch
analysis after re-embed. SPEC-ONLY(grammar-driven).

---

## FAMILY 5C — ENGINE-TIER OPTIMIZATION — **ADVANCED — 42%** (2A wholesale NO-GO, acyclic sliver likely a NULL; 2B-minimizer BLOCKED; the shippable residual is the delete-restart fix + the LOCAL tier table)

**Architecture.** Push optimization *below* the HIR onto the automaton, orthogonal to the type plane
(rides below whichever tier matches). Three mechanisms, honestly re-cut by pass-3 research-C:
- **The incumbent minimizer is crippled by ONE global restart, not its algorithmic class.**
  `dfa.rs:291 hopcroft_minimize` is misnamed naive Moore; on any block split it does `break; //
  restart refinement from the beginning` (`:358`, under `if changed{break}` `:362-364`) — O(n²·k·B).
  **The primary fix is delete ~3 lines (Null-A):** restores proper Moore O(n²·k) with alloc-light
  constants intact. The automata are tiny (2–16 states, k≤~10) — the O(n·k·log n) crossover **never
  activates**; the correct worklist Hopcroft (Null-B, Valmari 2012) ran **3.53× SLOWER** on
  constants. So minimization-as-congruence is a wholesale perf NO-GO; the win is deleting the restart.
- **2A acyclic-congruence (Revuz) is likely a NULL once the restart is gone.** The measured 0.80×
  (20%) edge was taken against the *crippled* incumbent; most of it is the restart penalty, not a
  congruence advantage. Against Null-A the acyclic subset converges in ~diameter (≤~4) passes with
  zero alloc → near-tie unless the acyclic fraction (measured 4/26 = 15%) AND diameters are large.
- **2B-as-minimizer BLOCKED (Kozen 1994):** star-equivalence needs the star-induction rule; no finite
  ground rewrite set is complete → ρ>1 is a theorem, not a tuning gap. 2B-as-*determinizer* survives
  (ρ=1, 25/26). **This is the lock's one provably-impossible missing primitive.**
- **2C tier selection is LOCAL — a table, not a CSP.** Entry-set → tier
  (`memchr1<memchr2<memchr3<shufti<Teddy<none`) is a local function of `(cardinality, literal-prefix,
  byte-rarity)`; both candidate global witnesses (runtime L1-LUT coupling, codegen-size-budget
  coupling) refuted concretely. A CSP with no binding constraint violates Y.13. **Home the table in
  5C**; escalate to LOCK-2's scan-plan CSP only on an exhibited global codegen/LUT-budget witness.

**Integration seams.** `automata/dfa.rs:45` `Dfa`, `:14` `DfaState`, `:291` `hopcroft_minimize`
(restart `:358`, `:362-364`), `:218` `subset_construction`; `is_acyclic` **absent** (new O(n·k)
iterative DFS); `egraph/node.rs:44` (`HirENode` derives `Language`); `egraph/rules/mod.rs:64`
`default_hir_rules` (5 rules, the completeness ceiling); PROVE-10 `from_dfa` (un-vendored scratch —
must re-vendor + re-derive the 9.6–29× band in the tree of record). Minimizer becomes a directory
module `automata/minimize/{moore,hopcroft,acyclic,congruence,mod}.rs` — no god module.

**Gates (born-RED).**
- **G-5C.null:** HEAD→Null-A delta on a GROWN corpus (≥50, tagged acyclic/cyclic) proves the restart
  is the pathology. **PASS** = Null-A ≥ incumbent everywhere; the restart is the whole story.
- **G-5C.acyclic:** Revuz-vs-Null-A and Revuz-vs-Null-B as a function of acyclic fraction, `is_acyclic`
  DFS cost folded across ALL patterns. **Report a NULL as a NULL** — an acyclic near-tie vs Null-A is
  the named negative result, not a banked win.
- **G-5C.rebuild:** congruence-rebuild round-count instrumented — ONE round on acyclic (Revuz,
  O(n·k)), O(n) rounds on cyclic (does NOT reach O(n·k·log n) generally). Confirms the general
  e-graph minimizer is a named negative on cyclic patterns.
- **G-5C.tier:** exhibit a grammar whose greedy per-site cost-sorted tiers overflow a stated
  LUT/codegen budget AND whose reassignment strictly wins → CSP earns its keep; else the table
  suffices. Bit-identical regen via fixed tier-ordinal + lexicographic tie-break.

**Exact open questions.** All decisive numbers UNRUN two passes: reference-Hopcroft control, the
delete-`:358` null, a corpus grown past 26 and tagged, the acyclic crossover, ρ quality-debt
`minimal(egraph)−minimal(Hopcroft)`, the 2C global-budget witness. SPEC-ONLY(grammar-driven) does not
bind here (engine-tier), but the tree-of-record fork (P1) does — every number re-runs in the
reconciled tree.

---

## FAMILY 5D — GENERATOR-PATH LOWERING POLICY — **BANKED — 36%** (re-homed onto the two-types survivor; three source-confirmed bugs, resolutions unbuilt; the decisive fraction unrun two passes)

**Architecture.** A grammar's regex literal lowers DIRECTLY into the grammar's own combinator IR
(`ByteClass`/`Repeat`/`Seq`/`Optional`/`Project`), so **no automata engine sits on the generator path
at all** — the full parse-that HIR→NFA→DFA engine survives only as the backing of the STANDALONE
public regex API. This is ALREADY the status quo on the generator path (`emit.rs:365` maps
`RegexProgram` to a `match_leaf` greedy delimiter-scan, no `Dfa::find`), but the current realization
is (a) UNSOUND against grammar FOLLOW and (b) built on a masked-wildcard continuation and a duplicated
first-byte deriver. 5D's work is to make the direct lowering *sound* and *single-sourced*. Prior art
is shipped and load-bearing: re2c compiles regexes AOT to direct-coded DFA (powers PHP's lexer) and
its lookahead-TDFA does capture with minimal overhead (the model for capture-leaf → `Project`); Ragel
names the trap — one-pass direct lowering is correct ONLY on a FOLLOW-sound subset; the residue must
fall to the compiled DFA. flap (PLDI'23) proves fused lexing wins **as staged codegen**, not as a
shared interpreted IR — evidence FOR 5D-lowering, AGAINST a unified runtime engine.

**Mechanism.** (1) **FOLLOW-sound one-pass predicate** — a `C*`/`C+` leaf lowers to a straight-line
greedy byte-run iff `first(C) ∩ FOLLOW(leaf) = ∅`; otherwise the greedy scan steals the next symbol's
bytes (classical maximal-munch hazard). `first(C)` exists (`analyze().byte_classes`); FOLLOW is its
dual and must be BUILT (fixed-point over the grammar, Changed-flag convergence, not a structural
hash). Non-disjoint leaves take `Seq[Repeat(C), suffix]` with the `Repeat` bounded by grammar FOLLOW.
(2) **A real `ByteClass` recognizer** — add `BackendExpr::ByteClass{set: ByteSet256, min, max}`
lowered whenever `analyze()` yields a single-class quantified atom, emitting `while set.contains(byte)
&& count<max { p+=1 }` bounded by `min` — replacing the fixed `[A-Za-z0-9_.$]` `is_text_byte` mask
(`emit.rs:227`) that today makes `[0-9]+` over-consume letters. Reach the tape via the reused
`push_offset` tail (`emit.rs:220`). (3) **Single-source** the two first-byte derivers into one.

**Integration seams.** `ir/src/lib.rs:250` `Regex`, `:389` `RegexProgram`, `:440` `SpanKind`;
`passes/src/lib.rs:1341` lower, `:1660` `backend_span_kind`, `:911` grammar `first_bytes` (FOLLOW's
dual to build); `codegen/general_descent/emit.rs:365` emission, `:158` `match_leaf`, `:227`
`is_text_byte` (the mask to replace), `:220` `push_offset`; the two derivers `regex_first.rs`
(`:81,:18`) + `bbnf-regex/src/lib.rs:45`. `one_pass.rs:64` — the UNSOUND predicate (`Repetition` arm
recurses into the sub, never checks the class against its own FOLLOW; `[a-z]+z` on `"abz"` returns
`true`); `passes/lib.rs:353` — `derive_recognizers` `_ => {}` silently swallows a desugared
`ByteClass`.

**Gates (born-RED).**
- **G-5D.4 (THE number, UNRUN two passes):** the FOLLOW-sound one-pass fraction over the engine
  corpus with real leaves, under BOTH straight-line and `Seq[Repeat,FOLLOW]` lowering, with the
  non-disjoint leaves NAMED. Decides delete-vs-rehome the DFA. This is BRIEF 1.
- **G-5D.3:** cold-paired direct class-scan vs PROVE-10 prefiltered `Dfa::find` on a grown (≥50)
  corpus — honest prior: tie on class-led, DFA wins on literal-prefix-led / skip-heavy. Expose the
  crossover, don't assume it.
- **G-5D.byteclass:** `ByteClass` recognizer actually FIRES (retire the masked wildcard); `[0-9]+` no
  longer consumes trailing letters; `BackendExpr::ByteClass` reaches the tape.
- **G-5D.capture:** `/(...)/ -> name` → `ByteClass + ValueProject` produces a tape BYTE-IDENTICAL to
  hand-authored `name = [class]+ -> name` — rides on Q11/LOCK-1's `->` ratification.

**Exact open questions.** G-5D.4 unmeasured (hand-evaluated ~92% under the *unsound* predicate; sound
fraction lower, unrun). Standalone-API expressivity boundary: routing the public API through
grammar-IR lowering would LOSE backrefs/lookaround — 5D keeps the FULL engine for the standalone API
and routes only the class-restricted grammar-embedded subset; the boundary is the expressivity of
`ExprKind`, and it must be stated. SPEC-ONLY(grammar-driven) on the outer bootstrap ring.

---

## INCOMPATIBILITY MAP (which routes cannot coexist, and precisely why)

**I-1 (the deepest live incompatibility) — 5D-no-engine-on-path ⊥ 5C/5A-engine-on-path.** A regex
leaf's generator-path matcher is EITHER a direct grammar-IR class-scan (5D `ByteClass` recognizer,
zero automaton) OR the compiled DFA + PROVE-10 prefilter (5C's tier ladder, consumed via the engine).
The same leaf cannot use both. They reconcile ONLY via a per-leaf SELECTION SEAM: the FOLLOW-sound
fraction (G-5D.4) partitions leaves into the 5D-eligible set (FOLLOW-disjoint one-pass) and the
DFA-fallback set, and the selection is 5C-2C's tier table extended with "direct class-scan" as tier 0.
Unresolvable as stated; the measured partition (BRIEF 1) is the adjudicator. Ragel's nondeterminism
wall bounds the 5D-eligible fraction; re2c proves the eligible slice ships at speed; flap proves the
fusion must be *staged codegen*, not a runtime unified engine.

**I-2 — 5A node-cached-tag ⊥ 5D structural `ByteClass` leaf.** 5A enriches `RegexProgram{pattern,
span_kind}` with a cached opaque `RegexClass` tag (leaf stays opaque + facts carried). 5D REPLACES
the opaque leaf with a structured `BackendExpr::ByteClass{set,min,max}` (no engine, structural). Two
different backend-node designs for the same leaf. Compatible ONLY if layered (`ByteClass` for the
FOLLOW-sound single-class case, `RegexProgram`+cached-tag for the rest) — but then who owns the
leaf-shape decision is the unsettled seam, and it is the SAME seam as I-1 (leaf-shape follows
path-selection). Incompatible if both claim the leaf as canonical.

**I-3 (a GESTALT fork, not a within-lock resolution) — Fork-A keep ⊥ Fork-B abrogate the grammar-tier
regex e-graph.** The 434→184-LOC grammar-tier e-graph exists only in top-level `crates/ir`. Fork-A
folds it into the tree of record → 5B's bridge and the grammar-tier rules gain a consumer; 5C's
grammar-side saturation has a home. Fork-B deletes it for free (skinny ships without one) → 5B's
bridge and the grammar-tier rules lose their only consumer (Y.13 on their own mechanism), and the
250-LOC de-dup is a fork artifact attributable to no family. Mutually exclusive; swings whether 5B and
the grammar-tier of 5C have consumers at all. Resolution = the fire census (BRIEF 2) feeds GESTALT,
which picks the fork per XL-A. LOCK-5 owes the number, not the decision.

**I-4 (internal to 5C, BLOCKED) — 2B-as-minimizer ⊥ correctness.** Equality-saturation minimizing the
DFA cannot coexist with a correctness requirement: Kozen 1994 proves no finite ground rewrite set is
complete for star-equivalence (ρ_max 1.667 measured, organic miss `\w+(?:[-_]\w+)*`). Classical
minimization (Null-A / Hopcroft) stays; 2B survives only as a determinizer (ρ=1). The one
provably-impossible missing primitive in the lock — a genuine BLOCKED horn, not a tuning gap.

**I-5 (resolved toward the table; not a hard incompatibility) — 5C-2C tier-CSP ⊥ tier-table.** No
binding global constraint exists (both witnesses refuted) → the CSP is a consumer-less substrate
(Y.13). Single-home the table in 5C; the CSP horn is retired absent an exhibited global
codegen/LUT-budget witness. Listed for completeness; the incompatibility collapses toward the table.

**I-6 (a prerequisite gap, not a family conflict) — XL-C's "cheap bridge reusing simplify_hir" ⊥ the
code.** XL-C ratified the bridge as available; `Display for Hir` (the reverse leg) does not exist.
Not a family-vs-family incompatibility — a ratified assumption vs the codebase. The bridge is an ~80
LOC prerequisite (5B's live residual, BRIEF-adjacent) that must be built for XL-C to hold.

---

## CROSS-LOCK DOUBLE-HOMES (LOCK-5 proposals; GESTALT ratifies)

1. **I-1's selection seam ⊗ LOCK-4-4D / LOCK-9-9C / LOCK-3-3C.** All three consumed "ONE-HIR"; the
   ratified landing is two-types + a per-leaf generator-path selection. Re-cut them to consume the
   selection seam (direct class-scan vs DFA), not a single lowering. 4D's "one HIR, not a second
   engine" premise is specifically weakened — the engine survives for the standalone API.
2. **5B re-open ⊗ LOCK-1 (RE-OPEN TRIGGER).** The functor retirement rests on `->` staying a
   `Map`-barrier. LOCK-1 owns the real `->` typing. If it represents projection otherwise, the
   projection-dropping-union bug reappears and un-retires the functor. Route the `Class`/`Look` typing
   (Q2) AND the `->` representation (Q11) through LOCK-1's real HM — and note the inference/codegen
   tier-disagreement 5A surfaced (`view.rs:35` barrier vs `passes/lib.rs:204` pass-through).
3. **5C-2C ⊗ LOCK-2 scan-plan/tier CSP.** Single-home the table in 5C; escalate only on a global
   codegen/LUT-budget witness. LOCK-2 has not deferred it — unadjudicated.
4. **I-1/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA).** Supersession: LOCK-5 owns the HIR→DFA→classify substrate
   + the `ByteClass` chooser; 9C consumes it or retires. But PROTO-2 showed the classify re-home is
   non-trivial (needs regex dispatch) — the substrate LOCK-5 offers 9C is itself unbuilt.
5. **5A/5D ⊗ LOCK-7 bootstrap.** Two divergent `regex.bbnf` (one discards its AST). Re-scope from
   "author regex.bbnf" to "promote + de-dup + break-the-discard"; SPEC-ONLY(bootstrap) until 7A.
   `grammar/misc/regex.bbnf` maps 1:1 onto engine leaves (`char_class→Class`, `anchor→Look`,
   `factor→Repetition`) — the promotion target when 7A opens.
6. **rust/ts ⊗ LOCK-6.** The winner is TWO types → LOCK-6's mirror must carry BOTH (`Regex(StringId)`
   grammar-side + rich `Hir`/`Class`/`Look`/`ByteClass` engine-side + automata crossing to TS,
   PROVE-14 tape). The serde-derive ownership (who adds `CharClass: Serialize`) lands on the
   parse-that READ-ONLY / LOCK-6 boundary.

---

## THE TWO SHARPEST CROSS-FAMILY UNCERTAINTIES (runnable briefs)

The two numbers unrun after two passes that most advance the next pass: **BRIEF 1** adjudicates I-1
(the generator-path architecture); **BRIEF 2** adjudicates I-3 (the tree fork, and with it whether 5B
and 5C's grammar-tier have any consumer). Both cross-family, both runnable, both flagged by the pass-3
registry directives as most-downstream-gating.

### BRIEF 1 — bbnf-worktree — the FOLLOW-sound one-pass fraction (G-5D.4)
**Question.** Of the regex leaves in the live grammar corpus, what fraction are FOLLOW-sound for
straight-line one-pass lowering (`first(C) ∩ FOLLOW(leaf) = ∅`), and which specific leaves REQUIRE
`Seq[Repeat,FOLLOW-guard]`? This partitions I-1: a high fraction means 5D's direct class-scan is the
default generator path and the DFA retreats to fallback; a low fraction (the Ragel trap) means the
DFA stays central. Unmeasured two passes.
**Method.** In a skinny worktree base-pinned to HEAD: (1) build the grammar FOLLOW closure — a
fixed-point over the grammar with a Changed-flag convergence (NOT a structural hash), dual to the
existing `first_bytes` at `passes/src/lib.rs:911`. (2) For each `ExprKind::Regex` leaf at its actual
grammar position, compute `first(C)` (`bbnf-regex analyze().byte_classes` → `ByteSet256`) and test
disjointness against FOLLOW(leaf). (3) Report the disjoint fraction, and NAME every non-disjoint leaf.
**Corpus.** The in-tree JSON + Sheets grammars, PLUS the injected counterexample
`word = [a-z]+ ; sentence = word "and" word` (the greedy run must be shown to steal `"and"`).
**Commands.** `cargo test -p passes follow_sound_fraction -- --nocapture` (new test module).
**Pass/fail.** Emit `disjoint=N/M frac=X%` and a named list of non-disjoint leaves. PASS = the
fraction is reported AND the `word/and` counterexample appears in the non-disjoint list (proving the
predicate is SOUND, unlike `one_pass.rs:64` which returns `true` for `[a-z]+z` on `"abz"`). FAIL =
the counterexample is classified disjoint (predicate still unsound) or FOLLOW does not converge.

### BRIEF 2 — bbnf-worktree — the Fork-gating grammar-tier e-graph fire census (I-3 / Q10)
**Question.** Does the grammar-tier regex e-graph (`crates/ir/src/egraph/rules/regex.rs`, the
surviving 184 LOC after the 250-LOC de-dup) FIRE on any real branch — specifically on `Map`-wrapped
(projected, `x -> f`) branches? PROTO-1 found the rules resolve a branch's regex via
`class_regex(child)` and return `None` on projection-wrapped children → structurally inert. If the
per-rule fire count on projected branches is ~0, Fork-B (abrogate the grammar-tier e-graph for free)
is available and 5B's bridge + 5C's grammar-tier rules lose their consumer (Y.13); if it fires,
Fork-A must keep it. This is the ONE number gating the fork (registry directive #3).
**Method.** In a top-level `crates/ir` worktree base-pinned to HEAD (the only tree where the
grammar-tier regex e-graph physically exists, per P1): run the backend-egraph saturation over the
corpus with the built-in reporter `BBNF_EGRAPH_REPORT=1`, once with BARE regex branches and once with
the same branches under `-> f` projection wrappers; tabulate `per_rule.work` for
`SupersetAbsorbAlt`/`UnionMergeAlt`/`FuseAltRegexBranches`/`DeduplicateAltBranches`.
**Corpus.** ≥30 Alt-of-regex patterns (`(a|b|c)`, `([a-z]|[A-Z])`, overlapping-class unions), each in
a bare form and a `-> tag` projected form.
**Commands.** `BBNF_EGRAPH_REPORT=1 cargo test -p bbnf-ir backend_egraph_fire_census -- --nocapture`.
**Pass/fail.** Emit `per_rule.work` for bare and projected variants. PASS (Fork-B available) = the
four rules fire ≥1 on bare AND ~0 (≤ noise) on projected → the e-graph is inert exactly where
projections dominate, consumer-density is near zero. FAIL/Fork-A = the rules fire materially on
projected branches → the grammar-tier e-graph has a live consumer and must be folded in, not deleted.
