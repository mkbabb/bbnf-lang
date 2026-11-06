# LOCK-5 — THE REGEX CONTINUUM — pass-2 SYNTHESIS (FAMILY-SPECS)

Synthesis of pass-2 research-{A,B,C,D,priorart} against the pass-1 registry (the two within-lock
critic floors) and every prior artifact (PROTO-1 5A tax/nullability, PROTO-2 5C 2A/2B measured).
One cogent specification per family while the routes remain incompatible; then the incompatibility
map, the cross-lock double-homes LOCK-5 must name back, and the two sharpest uncertainties as
runnable briefs. Elegance earns nothing on its own — a family stalled at a missing primitive as
hard (or harder) than the original problem carries a BLOCKED marker.

**What pass-2 was.** A *research + synthesis* pass: no new prototype ran. Every pass-2 finding is
a **source-read, hand-evaluation, or structural estimate** — sharper than pass-1's, and several are
now CONFIRMED against live source — but the numbers the registry demanded *run* (G-5B.2 rule-death,
5D one-pass fraction, 5C reference-Hopcroft control, 5A automata parity on the merged type) remain
**unrun**. They are the pass-3 prototype budget; the two briefs below select the two that gate most.
Convergence % below is the pass-1 critic floor, annotated with the pass-2 conceptual delta and
marked *un-earned until prototype* wherever the delta is read-not-run.

---

## 0. THE LOAD-BEARING TERRAIN (two preconditions gate every family)

### P1 — the two-tree reconciliation, sharpened into a contradiction

| | skinny `crates/ir` (graduating, skv22) | top-level `crates/ir` (mature egraph) |
|---|---|---|
| regex leaf | `ExprKind::Regex{pattern:String}` — opaque (`skinny/crates/ir/src/lib.rs:250`) | `IrNode::Regex(StringId)` — interned (`crates/ir/src/types/node.rs:36`) |
| grammar-tier regex e-graph | **absent** (`backend_egraph.rs` seam only) | `egraph/rules/regex.rs` — **434 LOC, real** (verified: `FuseAltRegexBranches:229`, `parts.join("|"):300`, `regex_escape_literal:318`) |
| e-graph substrate | none inline | `GrammarENode` derives `Language`; `SaturationCache` imported at `passes/regex_info.rs:13` |
| classify | 3-pattern stub (`skinny/crates/bbnf-regex`, 322 LOC) | re-homed into `passes/materialization/classify.rs` |
| shape | **already 5A-shaped** (opaque leaf, nothing for a functor to bridge) | **already 5B-shaped** (opaque grammar leaf + rich HIR + the duplicate copy 5B kills) |

**The pass-2 sharpening.** research-A and research-B **both** independently declare the tree of
record to be the **top-level `crates/ir` + parse-that `bbnf-regex`** pair — because that is the only
tree where 5A's load-bearing enabler (`egraph-derive` auto-deriving the Id-hashed e-node view) and
5B's value target (the 434-LOC copy) *physically exist*. This **contradicts P1's recommendation**
that skinny is the graduating tree of record. The contradiction is now precise and must be handed to
GESTALT as a **named fork, not a footnote**:

- **Fork-A (fold top-level INTO skinny):** the graduating skinny tree gains a grammar-tier regex
  e-graph by folding in the top-level 434-LOC substrate → 5B's value target survives reconciliation,
  5A's `egraph-derive` enabler survives → the Axis-1 contest is real and must be run.
- **Fork-B (abrogate the grammar-tier regex e-graph):** the graduating tree needs no grammar-tier
  regex e-graph (skinny ships without one today) → the 434-LOC copy is *deleted, not bridged* → 5B's
  raison d'être evaporates (RETIRED) and 5A's "one authored type + two derived views" holds only if
  the reconciled tree *reintroduces* an e-graph for the cache (else the two-views claim has no
  cache-view consumer — a Y.13 risk on 5A's own mechanism).

**No Axis-1 verdict is earnable until GESTALT picks the fork.** The two briefs below are deliberately
sited in the **top-level tree** so both Axis-1 horns are measured in the one tree where both
substrates exist — discharging P1's "measure in one tree" demand *under Fork-A's premise*, and
producing the exact number (does the copy die? is the merge clean?) that tells GESTALT which fork is
cheaper.

### P2 — grammar-driven is SPEC-ONLY lock-wide (unchanged, re-confirmed)

Not one artifact this pass exercised a grammar-driven regex path. Two `regex.bbnf` exist and diverge:
`parse-that/rust/bootstrap/regex.bbnf` (whose `parse_generated()` returns `bool` — **discards its
AST**, generated.rs:2288) and `grammar/misc/regex.bbnf` (divergent, carries bugs). skinny hand-scans
regex to an opaque `String` (`grammar/src/lib.rs:1561`). Constitution clause (i) is SPEC-ONLY;
5A/5D's grammar-totality claim is **promote + de-dup + break-the-discard**, not greenfield authoring,
and is gated on LOCK-7/7A. **Every verdict line below carries the SPEC-ONLY(grammar-driven) label.**

---

## FAMILY 5A — ONE-HIR — **ADVANCED — 55% floor → ~62% synthesis-provisional (un-earned until prototype)**

**Architecture.** One node algebra. Regex is the fragment carrying no rule reference and no
projection: `{Seq, Alt, Repeat, Optional, Literal, Class, Look}`. Add exactly two leaves to the
grammar node — `Class(CharClass)` (byte/codepoint set + negation) and `Look(Look)` (zero-width
assertion) — and the regex HIR *is* a subset of the bbnf HIR. Maximal reading of constitution (ii):
not isomorphism-to-a-point but **identity** — "there is nothing to map." The parse-that owned `Hir`
(`Box`/`Vec` tree) is what **dies**; it is the duplicate.

**Mechanism (pass-2, concretised).** The load-bearing enabler is that the e-node form is *derived,
not authored*: `egraph-derive`'s `Language` impl classifies fields by type (`Id`, `Box<[Id]>`,
`[Id;N]`) with no per-field attributes. So ONE *authored* `IrNode`+`{Class,Look}` yields the
Id-children `GrammarENode` for the saturation cache **for free** — the "Trees That Grow" extension
point (Najd & Peyton Jones), egg's `define_language!` discipline. This resolves the pass-1 god-type
tension into **one authored type, two derived views**:
- `IrNode` derives `Serialize,Clone,Debug,PartialEq` — **no `Eq`/`Hash`**; spanned; interned.
- the auto-derived `GrammarENode` derives `Eq`/`Hash` with `Id` children — the span-erased cache key.
- The named counterexample that *pins* the design: the `SaturationCache` **must not key on `IrNode`
  directly** (a spanned `Class` at two offsets is semantically equal but structurally `Eq`-unequal —
  span defeats the hit). The Id-interned view is *load-bearing, not incidental*.

**Class/Look under LOCK-1's real type.** `Class ⊢ Span` — a `[a-z]+` recognizes bytes and projects
to `TypeDesc::Span`, the same node a `Literal` yields; **no scalar-byte type is needed**. The one
two-world concern is `CharClass::Unicode` (`CodepointRange`, a `char` scalar) vs `Bytes` — but
UTF-8 byte-expansion is **confined to automata** (`to_positive_byte_ranges()` *panics* on `Unicode`
at parse-that `hir/mod.rs:82`), never leaking into `TypeDesc` or the shared enum. `CodepointRange`
is POD + `Serialize`-clean, so carrying it in the shared enum is admissible — a judgment, not a leak.

**Look consumer (pass-2 closed the Y.13 gate conceptually).** research-A **named a reader and a
failing test**: the automaton's accept condition for end-anchors. In the merged fragment, rule bodies
`/foo$/` and `/foo/` must diverge on `"foobar"` — `/foo$/` rejects, `/foo/` accepts; drop or ignore
`Look` and both accept → test FAILS. This proves `Look::End/EndLF` is read, and simultaneously shows
`Look::Start` is a **no-op for seam-anchored grammar-embedded regex** → the honest admissible set is
`{End, EndLF}` (± CRLF), *not* the six the standalone engine carries. `Look::Start` earns a leaf only
if a reader is wired (e.g. disabling the PROVE-10 memchr skip on anchored search); else it demotes to
an automaton annotation.

**Integration seams.** Extend `crates/ir/src/types/node.rs:30` (`IrNode`) [tree of record] /
`skinny/crates/ir/src/lib.rs:234` (`ExprKind`, HEAD 10 variants incl. `Project`). `from_hir` →
`from_expr(&IrNode)` sibling at parse-that `automata/nfa.rs:71`. Re-home
`classify_regex_from_hir(&Hir)` (parse-that `classify.rs:177`) as an IR pass over `IrNode::{Class,
Look,Concat,…}`; repoint `passes/regex_info.rs:76`, `types/recognizer_configs.rs:16`,
`fn_descriptor.rs:33`, `grammar.rs:142`. Break-the-discard: `->`-project `bootstrap/regex.bbnf`;
delete `grammar/misc/regex.bbnf` and the skinny 4-variant `RegexKind`.

**Gates it carries.** (G-5A.1) leak census RUN on HEAD 13-variant `IrNode` **and** the LOCK-1/LOCK-3
target enum = **0 REGEX-LEAK** (pass-2 read it 0; unrun). (G-5A.2) `Look`-consumer test FAILS when
`Look` is dropped (the falsifiable `/foo$/` test). (G-5A.3) `from_expr` NFA **byte-identical** to
`from_hir` over the parse-that corpus — explicitly asserting Group-elision (`IrNode` has no `Group`;
parse-that `Hir::Group` is transparent) and `Optional↔Repeat{0,1}` identity — then re-derive
find/scan throughput *from the merged type* (no inherited PROVE-10 band). (G-5A.5) `egraph-derive`
`Language` projection stays **total** on the extended enum AND `SaturationCache` dedups a
spanned-`Class`-inserted-twice to one e-class (the two-views claim). (G-5A.6) classify ΔLOC
before/after (shrink, not relocate).

**Exact open questions.** (Q1) On HEAD + the LOCK-1 target enum, is the leak count actually 0 when
*built* (not read)? (Q2) Does `egraph-derive` stay total when `Class(CharClass)` carries a non-`Id`
payload, and does the cache key on the Id-view without a hand-authored e-node (which would
reintroduce the duplicate 5A claims to kill)? (Q3) Does `from_expr≡from_hir` hold across Group-elision
— or is there a silent NFA divergence? (Q4) Does classify *shrink*, and by how much? (Q5, Fork-B
risk) if the reconciled tree abrogates the grammar-tier e-graph, does the cache-view lose its
consumer (Y.13 on 5A's own mechanism)?

**BLOCKED assessment: NOT BLOCKED.** The god-type is resolved by a *derived* view (a real primitive
that exists in-tree, not a missing one); the bootstrap is LOCK-7-gated SPEC-ONLY, not a
harder-than-original primitive. Leading Axis-1 candidate on the tax axis; the write-once axis (5B's
value) is untested against it.

---

## FAMILY 5D — GRAMMAR-FRAGMENT — **BANKED (into 5A) — 35%** (three in-tree facts CONFIRMED this pass)

**Architecture.** A `/pat/` desugars *once* into native `ExprKind` nodes of the same arena
(`regex_desugar`); nullability, first/follow, typing, lowering, emission all run through machinery
the grammar already owns. The classical NFA→DFA survives as **one** opt-in lowering target the
scan-plan cost model selects for leaves that provably need it. **Folded into 5A** under the "share a
mechanism, share a family" rule (both add a byte-class leaf, dissolve the opaque string, re-home
classify); they differ *only* on lowering policy. BANKED, not RETIRED — 5D retains the decisive
one-pass number that fixes 5A's lowering policy.

**Maximal reading RETIRED** ("no engine on the generator path") — refuted by PROVE-10 (DFA +
start-state memchr, 8.8–29.5×, 44/44) and the scannerless-slowdown literature. Surviving lawful 5D:
a byte-recognizer that STILL compiles to the same DFA+prefilter, `->` projections carried per site.

**Three pass-2 CONFIRMED facts (source-verified, in-tree):**
1. **Q3 — `check_one_pass_eligible` is UNSOUND for its own use.** It checks only alternation
   FIRST-set disjointness; its `Repetition` arm (`parse-that/rust/regex/src/info/one_pass.rs:38`)
   never checks the repetition's class against its FOLLOW. On `[a-z]+z` it returns `true`, but the
   pattern is **not** one-pass (`"abz"`: greedy `[a-z]+` eats `abz`, `z`-literal fails at EOF,
   give-back required). A straight-line lowering would **reject a matching string** — silent
   correctness fault. **Blast radius:** both trees route on this flag (`RegexRecognizer::scanable()`
   returns it directly, `crates/ir/src/recognizer/mod.rs:72`). *Fix (elegant, not a patch):* desugar
   to `Seq[Repeat, suffix]` and let the grammar's **own FOLLOW machinery** (`AltMode::Dispatch`)
   decide — soundness moves out of the regex predicate into the one place the grammar already reasons
   about FOLLOW. Priorart adds: the reference engine (regex-automata `dfa::onepass`) uses
   **build-and-bail**, not a predicate — "there is no separate pass; it tries to build the DFA and
   bails." A hand-written pre-deciding predicate is itself the trap.
2. **Q4 — `derive_recognizers` does NOT fire on `ByteClass`.** `skinny/crates/passes/src/lib.rs:338`
   matches a single-byte `Literal` and a `QuotedString`-keyed `Regex`, and drops the rest to
   `_ => {}`. (a) the `QuotedString` arm keys on `analyze()` — the 3-pattern oracle 5D deletes → dead
   code after `regex_desugar`. (b) the desugared `ByteClass` falls into `_ => {}` **silently
   swallowed** → the SIMD-recognizer chooser 5D's entire fast-path rests on **is not firing**. The
   pass-1 masked-wildcard contradiction is now source-confirmed. *Fix:* structural chooser on the
   desugared *shape*; in the top-level tree a new `ByteClassRecognizer: RecognizerInfo` beside
   `LiteralRecognizer`, `scanable()` = the FOLLOW-corrected flag.
3. **Q5 — structured class never reaches the emitter.** The leaf travels as
   `BackendExpr::RegexProgram{pattern:String}` (`skinny/crates/ir/src/lib.rs:389`) — opaque. The
   typed-materialization invariant obliges building `BackendExpr::ByteClass{set,negated}` → tape and
   asserting zero `ExprKind::Regex` survive (panic-on-residue). **Ragel boundary resolved by the
   thesis:** a whole-leaf `->` wraps the subtree root once (one DFA+prefilter); a *sub-pattern
   capture* `->` desugars its captures into grammar `Seq`/`Project` nodes — the grammar's OWN native
   span capture — so the Laurikari tagged-DFA is **never built**.

**Gates it carries.** (G-5D.4) the **FOLLOW-sound** one-pass fraction over the 26-pattern corpus —
report BOTH the reported (~92% under the unsound predicate) and the sound fraction (THE number
deciding delete-vs-rehome the DFA). (G-5D.3) cold-paired straight-line class-scan vs prefiltered
`Dfa::find`, `count_a==count_b`, N≥200 — a tie is a tie. (G-5D.1) zero `ExprKind::Regex` survive.
(G-5D.chooser) a `ByteClass` grammar emits a shufti/`SimdScan` recognizer, not `Vec::new()` from the
wildcard. (G-5D.tape) `BackendExpr::ByteClass` emits a correct non-inert scan; `RegexProgram` deleted.

**Exact open questions.** (Q1) the sound one-pass fraction — high enough that the DFA is dead weight
on the *generator* path (it is, for the three `json.bbnf` leaves + PROVE-10's four, all FOLLOW-sound)
but never corpus-wide for adversarial input? (Q2) does the class-scan actually beat the prefiltered
DFA, or tie (the honest prior: **tie** on class-led, DFA **wins** on literal-prefix-led)?

**BLOCKED assessment: NOT BLOCKED** — a named narrowing (the DFA stays PROVE-10-floor-reachable) plus
three concrete edits, none a missing primitive.

---

## FAMILY 5B — LAWFUL-FUNCTOR — **BANKED — 45%** (held behind the G-5B.2 un-bank/retire gate; the un-bank case STRENGTHENED this pass)

**Architecture.** Two domain-fit HIRs joined by a total functor pair `F ⊣ G` through the shared
e-graph. `F : HirENode → GrammarENode` (free embedding, total). `G : GrammarENode ⇀ HirENode`
(reflection, total only on the regex-expressible sublanguage `{Literal,Regex,Seq,Alt,Repeat}`,
**undefined** on `Ref`/`Map`/`TokenDispatch`/`Minus`/`Negate`). The adjunction is exactly *regex is a
coreflective subcategory of grammar* (`G∘F=id`, `F∘G⊑id`). The win: **write-the-regex-algebra-once**
and delete the duplicate grammar-tier rules; each side keeps its domain-fit representation.

**The pass-2 reframe — the functor ALREADY EXISTS, degenerate and stringly-typed.** research-B's
strongest new argument, source-verified: the bridge between the two e-node types today **is the
string**. `bbnf_regex::algebra::{pattern_is_superset,try_union_patterns}` **re-parse the pattern to
`Hir` on every call**; `FuseAltRegexBranches` fuses via `parts.join("|")` (`regex.rs:300`),
hand-rolling `regex_escape_literal` (`:318`) — string reimplementations of what `Hir` structure gives
free. This IS `F`/`G` in its worst form: `String` as the interchange type, parse/print as lossy,
per-call, un-cached natural transformations. 5B's thesis is therefore **not "add a bridge" but "make
the bridge already present lawful, structural, and cached."**

**Rule-death count (structural estimate from source — the un-bank number, still un-MEASURED):**

| Rule (`crates/ir/src/egraph/rules/regex.rs`) | Fate | Why |
|---|---|---|
| `DeduplicateAltBranches` | SURVIVES | pure e-class identity, grammar-general |
| `SupersetAbsorbAlt` (~80 LOC) | DIES | duplicate of regex-side `superset.rs` |
| `UnionMergeAlt` (~90 LOC) | DIES | duplicate of regex-side `union.rs` |
| `FuseAltRegexBranches` (~85 LOC) | REPLACED → structural | `parts.join("|")` → build `HirENode::Alternation` + regex rules + extract-back |
| `regex_escape_literal`, `pattern_has_top_level_pipe` (~45 LOC) | DIE | string reimplementations |

**Structural estimate: ~215 of 434 grammar-tier LOC + ~4 string wrappers deleted.** The prototype
must MEASURE this (LOC before/after) — the count is source-read, not run.

**The NEW soundness argument for the functor.** `UnionMergeAlt` merges `[a-c]|[d-f]→[a-f]` by
byte-set. Under LOCK-1 `->` typing, `[a-c]->A | [d-f]->B` would merge to `[a-f]` and **drop the
distinct projections A,B** — a *wrong answer*. The functor makes projection an explicit side-annotation
on the bridge cast, so the merge is checkable (block unless projections equal); **the string bridge
cannot see projections at all.** This is a latent soundness bug the functor fixes and the string
bridge harbours — and it is *shared* with 5A (a merged `IrNode` running the same fuse rule faces the
identical projection-equality obligation), so it does not by itself decide Axis-1; but it is a
concrete, runnable value the functor demonstrably adds.

**The joint coproduct variant (5B's strongest, distinct-from-`translate.rs` form).**
`enum JointENode { Left(HirENode), Right(GrammarENode) }` over ONE union-find arena, with a bridge
rewrite `Right(Regex(sid)) ≡ Left(insert_hir(parse(sid)))` materialized as an
`unrealized_conversion_cast`-style node **folded pre-emission** (MLIR dialect-conversion discipline;
the cast must provably vanish — priorart trap #5). One saturation runs regex-side + grammar rules
together; the regex algebra reaches grammar Alts through the bridge. Worth its doubled match surface
*only* if the leaf-richness census shows non-trivial regex sites; else the two-e-graph + structural
functor-helper variant wins.

**Why still BANKED, not un-banked.** The un-bank gate requires **running** G-5B.2 in the tree that
holds the copy. research-B *read* the source and *estimated* structurally — it did not build the
functor, run the round-trip oracle, or measure the LOC deletion. Squeezed from two sides that pass-2
did not relieve: (1) 5B's shared-optimization payoff reaches only `Concat/Alt/Repeat/Literal/Empty`
(its own risk register concedes `Class`/`Look` cross only as *atomic* nodes) — precisely the fragment
5A unifies for free; (2) the value target lives ONLY in the top-level tree, and Fork-B (P1) would
delete rather than bridge it. The strengthened un-bank case (degenerate-functor-exists + rule-death
table + projection-soundness) makes G-5B.2 **the sharpest runnable Axis-1 value question** — Brief 1.

**Gates it carries / un-bank-retire gate (MANDATORY pass-3).** (G-5B.2) build the structural
`FuseAltRegexBranches` + cross-domain `insert_bbnf`/`extract_bbnf` (modeled on parse-that
`translate.rs:22`) in the top-level tree; round-trip oracle = **DFA-equivalence** (strict off,
canonical on — NOT syntactic `==`), span/projection recovered bit-identical as side-annotation;
projection-soundness test (`[a-c]->A|[d-f]->B` blocked); partiality test (`Alt([Regex,Ref])` → `G`
returns None on `Ref`, no crash, no cross-branch fusion). **Verdict rule:** copy dies by the counted
LOC **and** the reconciled tree keeps a grammar-tier regex e-graph (Fork-A) → **un-bank to ADVANCED**;
copy absent / dies trivially under 5A's merge / Fork-B deletes it → **RETIRED**.

**Exact open questions.** (Q1) does the copy die, by how many LOC, when the algebra is written once
(un-run)? (Q2) does span/projection survive `F∘G` bit-identical as a side-annotation? (Q3) coproduct
vs functor-helper — which does the ~215-LOC deletion justify (leaf-richness census)? (Q4, P1) does the
reconciled tree keep the e-graph at all — a GESTALT fork the prototype informs but cannot decide.

**BLOCKED assessment: NOT BLOCKED.** The cross-domain functor's template exists (`translate.rs` is the
same-domain model); the adjunction is standard (coreflective subcategory). BANKED for
convergence-crowding + value-target-fork-dependence, not a missing primitive.

---

## FAMILY 5C — AUTOMATA-AS-EXTRACTION — **ADVANCED — 60%** (orthogonal; internal 2B-as-minimizer **BLOCKED**)

**Architecture.** Push the shared e-graph one level below the HIR, to the automaton.
**Orthogonal to Axis-1** — rides *below* whichever type-plane family wins, consuming its HIR and
producing its DFA. The centerpiece (research-C, literal not metaphor): **DFA minimization IS
congruence closure IS one e-graph rebuild.** Insert each DFA state as an e-node keyed on
`sig(s)=(is_accept, λclass.rep(δ(s,class)))`; equal signatures hash-cons-merge; the fixpoint
partition is the minimal DFA. Determinization = hash-cons on the NFA-state-*set* signature;
minimization = hash-cons on the transition signature; the acyclic special case = Revuz linear-time
falling out as the terminating branch (reverse-topological, zero fixpoint iterations). Adds a
**second** `Language` (states, not HIR nodes) over the same `EGraph`/`Scheduler`/union-find.

**Three operations, three sub-verdicts (PROTO-2 measured 2A/2B; 2C unprototyped):**

- **2A minimization-as-congruence — EXACT, perf NO-GO wholesale, 15% acyclic sliver contingent.**
  Correctness 26/26 byte-identical minimized state count, cyclic included. Perf: worklist-2A **3.53×
  slower** whole-corpus (measured, N=500) → **NO-GO as wholesale replacement**. Only the
  acyclic-congruence fast path ships (**0.80×, 4/26 = 15% of patterns**), and *even that "needs the
  alloc-light rewrite to matter"* (unbuilt). **The win is against a STRAWMAN** — the incumbent
  `hopcroft_minimize` (parse-that `dfa.rs:291`) is naive Moore + a **global restart** (`:358`
  `break; // restart refinement from the beginning`, verified present), O(n²·k·B), *not* Hopcroft. The
  honest verdict: **2A-general NO-GO; a 15%-coverage acyclic dispatch fast path, contingent on an
  unbuilt rewrite, measured only against the restart strawman.** The crossover vs a *correct*
  O(n·k·log n) Hopcroft — and whether *deleting `:358`* alone erases 2A's reason to exist — is
  UNMEASURED and is the decisive 5C number.
  - **Named asymptotic risk:** naive congruence iteration is O(n) rounds → O(n²·k) = Moore-minus-restart,
    NOT Hopcroft. The rebuild must carry the Hopcroft smaller-half worklist discipline to hit
    O(n·k·log n); the prototype must *prove* it does, not merely beat the crippled incumbent.

- **2B determinization-as-derivative-saturation — BLOCKED (as minimizer), cleanly confirmed.**
  ρ_max = **1.667**, organic corpus miss `\w+(?:[-_]\w+)*` (ρ=1.5), direct witness
  `c(ab)*a|da(ba)*` under-minimizes. `default_hir_rules` is exactly five (`FlattenAltConcat`,
  `DeduplicateAlternation`, `SupersetAbsorbClass`, `UnionMergeClass`, `AbsorbRepetition`) = the
  **ACUI + charclass-lattice + bound-arithmetic** fragment: sound, **incomplete** for regular-language
  equivalence. Kozen 1994 proves star equivalence needs the **star-induction inference rule**, which
  is *not* a finite ground rewrite → **no finite e-graph rule set is complete** — a
  **provably-impossible missing primitive**, cleaner than "as hard as the original." **2B-as-minimizer
  is BLOCKED.** 2B-as-*determinizer* survives (produce a possibly-non-minimal DFA, close with 2A/Hopcroft
  — the automaton is the decision procedure, the e-graph is not); 25/26 corpus patterns ρ=1 corroborate
  engine correctness.

- **2C prefilter/tier-selection — UNPROTOTYPED; the honest boundary is a table, not a CSP.** The
  entry-set→tier map is a **local** function of `(cardinality, literal-prefix presence, byte rarity)`
  — no cross-site coupling, so a **pluggable cost-sorted table suffices** (fixed tier ordinal
  `memchr1<memchr2<memchr3<shufti<teddy<none`, lexicographic tie-break for bit-identical regen). It is
  a **genuine CSP only on a global constraint** — a shared codegen-size / L1-resident-LUT budget: if
  choosing shufti at site A denies budget to site B, the per-site choices couple. Absent such a witness,
  a CSP violates Y.13 (no binding constraint). **Double-homed with LOCK-2's scan-plan CSP** — must be
  single-homed: home the *table* in 5C (a property of the compiled DFA), escalate to the CSP home only
  on an exhibited global-budget witness.

**Gate/corpus fix.** `regex_dfa_compile.rs COMPILE_PATTERNS` holds **26** (verified 29 quoted strings
in file; effective corpus ~26); the "50+" figure belongs to `regex_classify`(57)/`regex_hir_egraph`(62).
**GROW `COMPILE_PATTERNS` to ≥50, stratified & tagged** — an acyclic arm (`a{3,6}`, literal
alternations, anchored fixed strings) and a cyclic arm (`\w+`, `[a-z]*`, self-loops). Report every
minimizer number against the tag, never a blended mean.

**Gates it carries.** (G-5C.1) reference-Hopcroft control (Null-B) + delete-the-restart null (Null-A)
run; the acyclic fast path measured vs *correct* Hopcroft with the `is_acyclic` dispatch cost folded
across ALL patterns. (G-5C.2) the congruence minimizer proven O(n·k·log n), not degenerate. (G-5C.ρ)
ρ across the grown corpus + the quality-debt residual `minimal(egraph)−minimal(Hopcroft)`. (G-5C.tier)
2C: global-budget witness exhibited (→CSP) or table-suffices demonstrated (→table); bit-identical regen.

**Exact open questions.** (1) Does the acyclic fast path beat a *correct* Hopcroft, or does fixing
`:358` erase its entire reason to exist? (2) Does the congruence rebuild hit O(n·k·log n)? (3) Is the
15%-slice net-positive after `is_acyclic` dispatch on the other 85%? (4) Table or CSP for the tier?

**BLOCKED assessment: FAMILY NOT BLOCKED; internal 2B-as-minimizer BLOCKED (Kozen).** 2A/2C ship
independently of 2B; 2B-as-determinizer survives. The maximal claim (e-graph automata beats the
classical pipeline at engine scale) is **effectively refuted** — the honest deliverable is a
15%-applicable contingent acyclic fast path + 2B's named theory bound + 2C's table-vs-CSP adjudication.

---

## INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

**Axis 1 — where regex structure lives (the load-bearing spine, gates LOCK-4-4D / LOCK-9-9C / LOCK-3-3C).**

- **5A ⊗ 5B — mutually exclusive endpoints.** 5A: ONE merged type, functor deleted, parse-that owned
  `Hir` dies, two *derived* views (`Serialize`-owned + Id-hashed cache). 5B: TWO domain-fit types
  kept, joined by a lawful total functor, grammar tier keeps regex opaque. **They cannot both be the
  architecture.** The precise conflict, sharpened by pass-2 into two *read-not-run* horns:
  - *5A's horn (research-A):* the merge is clean because `IrNode`(`Serialize`, no `Eq`/`Hash`) and the
    auto-derived `GrammarENode`(`Eq`/`Hash`, Id children) are the *same authored enum viewed two ways*
    — "there is nothing to map"; the leak census reads 0 on the combinator arms.
  - *5B's horn (research-B):* the grammar tier carries **no rich regex leaf by design** (opaque
    `StringId`); richness lives only on the regex side — the two ARE domain-fit, and merging forces the
    grammar pipeline to walk regex internals it deliberately does not.
  Pass-2 **did not resolve which horn wins** (no prototype). **Convergence condition (unchanged):**
  they converge iff bbnf grows `Class`/`Look` — then the difference is "one type" vs "two types + a
  near-trivial functor," and 5B's shared-optimization payoff reaches *only* the fragment 5A unifies for
  free → 5A simpler if coverage high, functor buys little if low. **But 5B's distinct value axis**
  (the degenerate-string-functor already ships; ~215 LOC of duplicate rules; the projection-dropping
  union soundness bug) is *not* squeezed by that convergence — it is the runnable un-bank question
  (Brief 1). The two briefs run **both horns in the one top-level tree**, collapsing the axis.

- **5B ⊗ 5D — mutually exclusive on the grammar-tier leaf.** 5B bridges the *opaque* grammar regex
  leaf with a functor while keeping both representations; 5D *dissolves* that leaf into native grammar
  nodes at desugar time — after which there is **nothing for 5B's functor to bridge** (`G` has no
  opaque source, `F` no target leaf). If 5D lands, 5B's `G` survives only as a demoted *recovery* path
  (build a DFA for non-one-pass leaves), not as an architecture.

**Axis 2 — same-spine siblings (share a mechanism ⇒ one family).**

- **5A ∧ 5D — COMPATIBLE, one family, differ only on lowering policy.** Both add the byte-class leaf,
  dissolve the opaque string, re-home classify, delete the string re-parsers. They differ *only* on
  what the unified HIR lowers to: 5A keeps `from_expr` NFA construction; 5D routes to a straight-line
  class scan by default with the DFA as an opt-in target chosen by the (FOLLOW-corrected) one-pass
  check. Pass-2 sharpens the tension: 5D's committed one-pass predicate is **UNSOUND** (must fold into
  grammar FOLLOW / adopt build-and-bail), and the DFA is **PROVE-10-floor-mandated** (never deletable).
  So 5D = 5A's generator-path lowering corollary, its narrowing line fixed by the FOLLOW-sound one-pass
  fraction (G-5D.4). Their only genuine residual tension is the `Look`/anchor leaf — both must answer
  the same Y.13 consumer question (5A's `/foo$/` test answers it for both).

**Axis 3 — orthogonal layer (composes with any Axis-1 verdict).**

- **5C ∥ {5A, 5B, 5D} — orthogonal, below the HIR.** 5C operates on the *automaton*, consuming
  whatever HIR the Axis-1 winner produces. Two named grafts: **GRAFT-1** — 2B-as-determinizer reuses
  the HIR-level five rewrite rules as its equivalence theory over the Axis-1 winner's *single derived
  `Language`* (5A's `derive(Language)` on the merged enum, or 5B's `Left` sublanguage); **GRAFT-2** —
  2A's byte-class partition (`sets/equiv.rs`) consumes 5A's `Class` byte-sets directly. 5C's only
  exclusion is *internal*: 2B-as-minimizer (BLOCKED, Kozen) cannot replace 2A — 2B determinizes,
  2A/Hopcroft closes.

**Summary of the mutually-exclusive set.** Exactly one of {5A (+5D as its lowering policy), 5B} can be
the type-plane architecture; 5C rides below the winner; 5D's "no engine" maximal reading is
independently refuted by PROVE-10 and survives only as 5A's generator-path lowering policy. **The
whole Axis-1 verdict is un-earnable until GESTALT picks the P1 fork** (fold-in vs abrogate the
grammar-tier regex e-graph) — the meta-incompatibility that sits above all four families.

---

## CROSS-LOCK DOUBLE-HOMES (constitution ix — LOCK-5 is the hub; named back)

1. **5A/5D ⊗ LOCK-9-9C (SCANNER-AS-DFA).** 9C sources `bootstrap/regex.bbnf` and extends the same
   classify `RegexClass` 5A/5D re-home. **Supersession: LOCK-5 owns the HIR→DFA→classify substrate +
   the `ByteClass`-shape recognizer chooser; 9C consumes it or retires.** Shufti class kernel is the
   shared residual.
2. **5C-2C ⊗ LOCK-2 scan-plan/tier CSP.** Same tier-selection CSP, two homes. **Single-home the
   table in 5C** (a property of the compiled DFA); escalate to the CSP home only on an exhibited
   global codegen/LUT-budget witness.
3. **5A/5D ⊗ LOCK-3 IR tower.** The `Class`/`Look`/`ByteClass` enum edits are LOCK-3-governed; the
   typed-materialization invariant binds the new leaves — **`ByteClass` must reach the tape emitter**
   (`BackendExpr::ByteClass`, replacing `RegexProgram`). LOCK-3-3C is contingent on this ONE-HIR choice.
4. **5A ⊗ LOCK-7 bootstrap.** Two `regex.bbnf` exist (one discards its AST, one divergent). **Re-scope
   5A from "author regex.bbnf" to "promote + de-dup + break-the-discard"; SPEC-ONLY(bootstrap) until 7A.**
5. **5A/5B ⊗ LOCK-4-4D (GRAMMAR-SUGAR).** 4D is contingent on LOCK-5 choosing ONE-HIR; reopening 5B
   weakens 4D. **LOCK-5's Axis-1 choice is load-bearing for 4D, 9C, 3C — three downstream locks blocked
   on a verdict currently asserted, not earned.**
6. **Type-plane ⊗ LOCK-1.** `Class ⊢ Span` was graded against skinny's stub `BuiltinTy`. **Route the
   `Class`/`Look` typing verdict through LOCK-1's real HM + row-poly + widths.**
7. **rust/ts ⊗ LOCK-6.** The rust/ts face of the merged/functored regex HIR (`Class`/`Look`/`ByteClass`
   + automata crossing to TS, PROVE-14 tape) is a **named gap for GESTALT-Ω**: if 5A merges regex into
   `IrNode`, LOCK-6's mirror/skeleton must carry the merged enum.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs for pass-3)

Selected to **collapse Axis-1 — the spine that gates LOCK-4-4D / LOCK-9-9C / LOCK-3-3C — by running
BOTH horns in the ONE top-level tree**, thereby also discharging P1's "measure in one tree" demand
under the Fork-A premise and handing GESTALT the exact number (does the copy die? is the merge clean?)
that decides the fork. Brief 1 runs 5B's write-once value; Brief 2 runs 5A's clean-merge claim. 5C's
remaining question (reference-Hopcroft control) is already precisely specified by research-C's P1
experiment and rides *below* whichever horn wins — it does not gate the downstream locks and is
deferred; 5D's numbers are downstream of the Axis-1 verdict (5D is 5A's lowering policy).

### Brief 1 — 5B un-bank/retire gate: structural fuse + cross-domain functor + rule-death count (`bbnf-worktree`)

**Question.** When the regex algebra is written once as a structural functor, does the 434-LOC
grammar-tier copy die by a counted number, and does the projection-dropping-union soundness bug appear
under the string bridge and vanish under the functor? Decides un-bank vs RETIRE — 5B's whole value axis.

**Tree/method.** Worktree off `bbnf-lang` master, in the **top-level `crates/ir`** (the tree that
holds the copy; `bbnf-regex` is already a dep, Cargo.toml:14). (a) Re-express `FuseAltRegexBranches`
(`crates/ir/src/egraph/rules/regex.rs:229`, currently `parts.join("|"):300`) *structurally*: build
`HirENode::Alternation`, run regex-side `union`/`flatten`/`superset`, extract back; assert the fused
DFA is byte-identical to the current string fusion. (b) Build cross-domain `insert_bbnf`/`extract_bbnf`
modeled on parse-that `regex/src/egraph/translate.rs:22`; round-trip oracle = **DFA-equivalence**
(strict off, canonical on) over the `regex_hir_egraph` corpus. (c) Projection-soundness test:
`[a-c]->A | [d-f]->B` must NOT merge-and-drop under the functor and MUST (today) under the string
bridge. (d) Partiality: `Alt([Regex,Ref])` → `G` returns None on `Ref`, no crash, no cross-branch
fusion. Measure `wc -l regex.rs` before/after deleting the subsumed rules/wrappers.

**Pass/fail.** Copy dies by **≥150 LOC** AND projection-soundness passes-under-functor / fails-under-
string-bridge AND round-trip DFA-equivalent on 100% of corpus ⇒ **5B un-banks to ADVANCED (conditional
on GESTALT Fork-A keeping the e-graph)**. Copy dies <150 LOC, or dies trivially under a bare 5A merge,
or the oracle finds a non-equivalence ⇒ **5B → RETIRED**.

### Brief 2 — 5A merged-type: two-views + total `Language` + leak census RUN on HEAD (`bbnf-worktree`)

**Question.** Does merging `Class`/`Look` into the ONE authored `IrNode` keep the `egraph-derive`
`Language` projection *total* and let `SaturationCache` key on the auto-derived Id-view (the two-views
claim that dissolves the god-type) — and is the per-arm leak census actually **0** when *built* on the
HEAD 13-variant enum, not read? Runs 5A's clean-merge horn.

**Tree/method.** Worktree off `bbnf-lang` master, **top-level `crates/ir`**. (a) Add
`IrNode::Class(bbnf_regex::CharClass)` and `IrNode::Look(bbnf_regex::Look)` at
`crates/ir/src/types/node.rs:30`. Run `cargo build -p bbnf-ir 2>&1 | grep -E "non-exhaustive|not
covered"`; enumerate every broken match across `ir`+`passes`; grade each new arm TRIVIAL-TOTAL
(structural one-liner, e.g. `Class => false`, `Look => true`) vs REGEX-LEAK (needs regex-specific
dispatch). (b) Confirm `GrammarENode` still auto-derives via `egraph-derive` over the extended enum
(`crates/ir/src/egraph/node.rs`) and the `Language` projection stays total. (c) The two-views test:
insert a spanned `Class` at two distinct offsets into an `EGraph`; assert `SaturationCache`
(`passes/regex_info.rs:13`) collapses them to **one e-class** (span-erased Id-view), and assert
`IrNode` still derives `Serialize` (no `Eq`/`Hash` forced onto the spanned type).

**Pass/fail.** REGEX-LEAK count **== 0** on HEAD AND `egraph-derive` `Language` total AND the spanned
duplicate collapses to one e-class AND `Serialize` intact ⇒ **5A god-type dissolved, merge clean, "nothing
to map" earned** (functor unneeded on the tax+cache axes). **≥1** REGEX-LEAK (name the leaking pass), OR
the derive breaks, OR the cache cannot key without forcing `Eq`/`Hash` onto the spanned `IrNode` ⇒ **5A's
merge refuted on that axis, 5B's domain-fit separation justified.**
