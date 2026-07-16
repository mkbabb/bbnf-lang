# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — ADVERSARIAL CRITIC 2, pass 1

Critic 2 of 2. Cross-lock weight, per charter. Every family hardened against the failure-mode
checklist (vacuous convergence, spec-cites-itself circularity, gates-that-cannot-fail, the
elegant-reduction trap, legacy aliases, masked fallbacks, unverified gestalt, consumer-less
substrate) + constitution violations + prototype-evidence misuse.

**This pass supersedes the 04:14 CRIT-2 draft.** That draft critiqued the 03:53 FAMILY-SPECS and
the two *early* probes (t1 + the regex-oracle). Since then the doc was re-synthesised (04:51) and
TWO fresh RAN_MEASURED probes landed (05:01): **PROTO-1-7B-selfemit** (the self-emit budget) and
**PROTO-2** (the GrammarIr construction census). Both ran *after* the 04:51 FAMILY-SPECS was
written — so the doc has had NO chance to reconcile their verdicts. The headline of this critique is
that the two newest probes resolve the lock's two make-or-break briefs with results the current
FAMILY-SPECS still contradicts on its own stated terms. Load-bearing `path:line` re-verified on
disk at HEAD (skinny tree, 2026-07-16). Pass-1 numbers are stringent: four probes, zero clean
re-audits, nothing near the 100% bar (zero gaps + fresh audit + two clean passes).

---

## 0. Disk re-verification of the load-bearing facts (confirmed, not quoted)

| Claim | Source | Verdict on disk |
|---|---|---|
| The two "current generators" are FIXED per-grammar templates, not a general descent emitter | PROTO-1-7B | **CONFIRMED** — `runtime_generator.rs:236-238` emits `normalize(CSS_MOD_RS/CSS_PARSER_RS/CSS_SINK_RS)`; `CSS_PARSER_RS` is a raw-string const at `:401`. The grammar's productions are never read for the recognizer body. |
| The arena is append-only with ZERO dedup → **no structural interner** (PROTO-2 proof C-1) | PROTO-2 | **CONFIRMED** — `ir/src/lib.rs:52` `let id = ExprId(self.exprs.len());` `:53` `self.exprs.push(...)`. No lookup, no hashmap-of-nodes. |
| `resolve_refs` is a separate whole-grammar pass (PROTO-2 proof C-3, the floor-1 TreeFold) | PROTO-2 | **CONFIRMED** — `ir/src/lib.rs:84 pub fn resolve_refs(&mut self) -> Result<(), ValidationError>`; distinct from `add_expr:51`/`add_rule:57`. |
| string heuristic `name.contains("host") && (value.contains("decode")\|"parse")` | 7C | **CONFIRMED** — `passes/src/lib.rs:798`, fed at `:620/:764`; recurses at `:806-812`. |
| `@{}` HostCapture is unlowered → `BBNF-UNSUPPORTED-HOST-CAPTURE` | 7C / §0 | **CONFIRMED** — `grammar/src/lib.rs:222`; scan pushes captures at `:422`, kind at `:251/:431`. |
| bbnf.bbnf leaves project `-> Span`; `->` first appears line 9; `@host` only at its definition | PROTO-1 / §0 | **CONFIRMED** — `bbnf.bbnf:9 identifier = /…/ -> Span ;`, `:73 host_directive = "@host" …`. |
| skinny xtask self-hosts nothing (json only, no `regen-bbnf`/`regen-regex`) | §0 fact 1 | **CONFIRMED** — `xtask/src/main.rs:177 JSON_TARGET`, `:228/:234` both emit via `JSON_TARGET`; no bbnf/regex target. |
| two divergent regex grammars (one-home breach) | §0 / XL-6 | **PARTIAL** — `grammar/misc/regex.bbnf` (1006B) present here; the canonical `parse-that/rust/bootstrap/regex.bbnf` is parse-that-side (not in this checkout). Breach stands, cross-repo. |

Every skinny-side fact the two new probes rest on is verified. The parse-that-side facts (the regex
producer public entry, the fat-LTO caveat) I take from PROTO-2-7D-regex-oracle + the prior CRIT-2's
own disk check; flagged where they cannot be re-verified in this tree.

---

## 1. THE TWO NEW PROBES vs THE DOC — the reconciliation gaps (this pass's core finding)

The 04:51 FAMILY-SPECS named two briefs as make-or-break and left them OPEN. Both have now RUN. The
doc's dispositions are stale against its own gates.

**REC-1 · 7B Plane A's own make-or-break gate RAN NEGATIVE; the doc still says "ADVANCE-pending-
measurement."** FAMILY-SPECS §2 Plane A: *"Brief 1 is make-or-break: emit succeeds ∧ LOC ≤ named
multiple → live; blow-up recurs → SPEC-ONLY."* PROTO-1-7B ran Brief 1. On the FAITHFUL path
(CompiledLowering = the exact JSON_TARGET wiring) **emit did NOT succeed** — `GATE_A: FAIL`,
`BBNF-UNSUPPORTED-PROJECTION @ bbnf.bbnf:421` (the first `->`). The only `Ok` is the RequestFacts
path, and it is a **byte-identical CSS template** (`diff → IDENTICAL_BODY`, 910=910 LOC, 143 `Css*`
refs) that would emit for *any* input grammar. By the doc's OWN conditional, Plane A resolves to
**SPEC-ONLY**, not "ADVANCE-pending." The doc has not reconciled this. Worse than "blow-up recurs":
the general descent emitter the plane presupposes is **architecturally absent** — there is nothing
to blow up. "The generator becomes mostly data" presumes a machine that reads the grammar's rules;
neither shipped emitter does.

**REC-2 · 7B Plane C's BLOCKED-candidate RATIONALE is empirically FALSIFIED by PROTO-2.**
FAMILY-SPECS §2 Plane C: *"the missing primitive — 'an IR fully expressible as a CFG' — is, for a
graph-shaped interned IR, equal in difficulty to the original self-hosting problem."* PROTO-2's
census refutes the premise **for the constructor**: there IS no interner (C-1, verified
`ir:52-53`), IDs are post-order synthesized attributes (C-2), and the sole graph edge `Ref→RuleId`
is deferred out of node-building into `resolve_refs` (C-3). **`N_host_construction == 0`; all 15
sites are `->`-expressible.** The IR is *"a tree in an append-only arena,"* i.e. a catamorphism —
not the graph-shaped interned IR the block feared. So Plane C's stated block is the WRONG block.
The real block is REC-1's absent emitter, not the IR's CFG-expressibility. A mis-diagnosed
BLOCKED-candidate is a convergence hazard: pass 2 would attack the wrong wall.

**REC-3 · FAMILY-SPECS §0 fact 4 still carries the FALSIFIED recognizer-only framing.** §0
(re-synthesised 04:51) still states the self-hosted regex parser's *"public `parse_generated(pattern)
-> bool` **discards it**"* — the recognizer-only claim the prior CRIT-2 AND PROTO-2-7D-regex-oracle
both falsified (`regex/mod.rs:34 pub fn parse_generated -> Result<Hir, ParseError>` already wraps the
producer via `host::lower`). This survived the re-synthesis. It is load-bearing: 7D's §4 open
question 1 ("the generated side yields a span-tape … a weak oracle") inherits a false premise on the
one edge where a full-Hir producer already ships. Parse-that-side; cannot re-verify in this tree, but
two independent artifacts assert it and the doc contradicts both.

**REC-4 · The "INC-2" label is used for two different axes across the probes.** FAMILY-SPECS §5
INC-2 = *"Totality reach at the build boundary: 7B ⊥ 7C."* PROTO-2 (the census) correctly
adjudicates THAT axis (construction floor=0 / linking floor=1 — a clean phase split). But
PROTO-2-7D-regex-oracle opens *"the promotion-vs-oracle-only posture (INC-2: 7B promote vs 7D
oracle-first)"* — which is the **promotion trajectory**, i.e. INC-1/INC-3, NOT the build boundary.
The naming note in PROTO-2.md (its lines 11-13) reconciles the *filenames* but not the *cross-
references*: the regex-oracle probe's "resolves INC-2 toward 7D" and the census's "adjudicates INC-2
(clean split)" adjudicate DIFFERENT conflicts. Both results are real; conflating them would let a
reader think one probe settled both. Pass 2 must renumber: the census settles the build boundary;
the regex-oracle settles the promotion trajectory.

---

## 2. CROSS-LOCK VIEW (critic-2 charge) — contradictions, double-homes, missing edges

**XL-1 · 7B Gap B/Plane C ⊗ LOCK-3 3C — now DEMONSTRABLY one deliverable, reinforced by PROTO-2.**
PROTO-2 didn't just census construction — it mapped every site to `ir/src/lib.rs` (`add_expr:51`,
`add_rule:57`) and concluded the IR builder is grammar-expressible as synthesized attributes. That
*is* LOCK-3 3C GRAMMAR-HOMOIOUSIA verbatim (*"the IR is itself grammar-specified — a bbnf-defined IR
grammar"*). **PROTO-2 performed LOCK-3 3C's feasibility work under a LOCK-7 harness.** So `ir.bbnf`
is homed in BOTH LOCK-3 (3C) and LOCK-7 (7B Gap B), and the feasibility evidence is now a shared
artifact. If LOCK-3 converges on any of 3A/3B/3D instead of 3C, `ir.bbnf` has no consumer in the IR
spine → consumer-less substrate (constitution viii / Y.13). FAMILY-SPECS' "aligns with LOCK-3 3C
without asserting it" is the exact dodge the portfolio's "two routes sharing a mechanism share a
family" rule forbids. Owed in GESTALT-B: collapse into a joint family or a named supersession — and
credit PROTO-2 as cross-lock evidence, not a LOCK-7 local.

**XL-2 · The regex differential oracle is double-homed 7D ⊗ LOCK-5, and the 12-row ledger is
LOCK-5's bug list.** PROTO-2-7D-regex-oracle's artifact (Hand `bbnf_regex` vs Gen `.bbnf`→
`RegexParserEnum`→`host::lower`→`Hir` structural differential) IS LOCK-5B LAWFUL-FUNCTOR material.
Its 12 divergences (9× `{n,m}` reject, `.` metachar, `(?s)` inline-flag) are bugs in **`host::lower`
— LOCK-5's semantic fold, not LOCK-7's.** The 0.76 parity headline measures LOCK-5's incomplete fold
surfaced through a LOCK-7 harness. Whoever fixes parity edits `regex/host.rs`, wholly inside LOCK-5.
Adjudication owed: is the regex oracle a LOCK-7 deliverable (bootstrap verification) or LOCK-5's
(functor round-trip)? Currently claimed by both.

**XL-3 · 7C's typed leaf ⊗ LOCK-1 — "most independent" is half-true; PROTO-2 hands 7C its first real
host fn.** The horizontal in-tree deletions (kill `passes:798`; lower `@{}` HostCapture) are
LOCK-1-free. The typed-lattice half (signature into inference, tier read off `&mut Ctx`, zero-cost
re-verify) is a LOCK-1 CONSUMER. **New, from PROTO-2:** the census produced 7C's first *concrete,
measured* CONTEXT/TreeFold host fn — `@host resolve_refs : (&mut GrammarIr) -> Result<(),
UnresolvedRef>` (HostContext::TreeFold, `ir:84`). That partly closes the prior gap "no CONTEXT host
fn exists (usage=0)" — a real, in-tree target now exists — but its purity/tier verification still
needs LOCK-1. So 7C's "independent" claim splits cleanly: {kill heuristic, lower HostCapture,
name resolve_refs} independent; {type the signature, verify the tier, prove zero-cost} LOCK-1-bound.

**XL-4 · GN-0 archives the feasibility evidence — and PROTO-1-7B makes the rescue HEAVIER than the
prior crit stated.** The prior CRIT-2 flagged that the WORKING legacy self-host + the 21,557-LOC
generated `bbnf.rs` live in the tree GN-0 archives. PROTO-1-7B sharpens the wound: the *ported
skinny* generator is **not** a general emitter at all — it is two grammar-specific string templates
(JSON, CSS). The legacy 21.5k came from the OLD general emitter; **the skinny generator GN-1 carries
forward never had a general-descent path.** So the gate is not "rescue the archived proof" — it is
**re-author (or un-archive) a general descent emitter that skinny never possessed.** That is
strictly larger than the prior crit's framing, and no family names it as a born-RED gate. It is the
single largest under-gated obligation in the lock. PROTO-2 says the target is *expressible*;
PROTO-1-7B says the *machine to realize it is absent*; the gate is to build that machine.

**XL-5 · LOCK-9 9C consumes LOCK-7's regex totality with no reciprocal RED edge.** 9C
SCANNER-AS-DFA dissolves quote/number/ws scanning into *"compiled DFA/prefilter products of the
bootstrapped grammar."* On PROTO-2-7D-regex-oracle evidence the bootstrapped regex path is 1.77–1.81×
slower and 9/51-incomplete — a route measured NOT to fire promotion. If 9C banks on LOCK-7's regex
self-host being production-grade, it inherits a non-firing route. Should be born-RED in the omega DAG;
neither charter names it.

**XL-6 · Constitution (ix) extant-tranche consciousness — partially honored, now with PROTO-2
evidence.** 7C explicitly re-cuts GN-5's totality-witness waves (TOML fifth grammar, host-fn-as-name-
gate audit, `check-name-gates`) — good, and PROTO-2's `resolve_refs`-as-host-fn is a concrete GN-5.W3
extension. But the GN codex omits any bootstrap/fixpoint wave, so 7A/7B/7D are **net-new tranches vs
GN**, lawful only if declared as named additions. FAMILY-SPECS does not declare it. Owed.

---

## 3. PER-FAMILY CONVERGENCE + ENUMERATED OPEN GAPS

### 7A — STAGED-BOOTSTRAP — **42%**

The wall is precisely located (PROTO-1: byte 421) and PROTO-1-7B adds that even past the parse wall
there is no emitter to fixpoint *over*. The fixpoint mechanism half-exists (`check_target`); the
artifact it would iterate is absent. FAMILY-SPECS §1 honestly concedes "the fixpoint as stated is
not achievable at HEAD."

- **G-7A-a** Rung 2 (the only rung that proves self-hosting) is BLOCKED on TWO walls now: the parse
  wall (PROTO-1, byte 421) AND the absent-emitter wall (PROTO-1-7B — even parsed, nothing lowers it).
  Rung 1 is the extant json/css staleness check re-pointed (gate-that-cannot-fail; disclosed).
- **G-7A-b** "The fixpoint over WHAT" (§1 open Q1) is now sharper, not resolved: generated `.rs`
  bytes prove only determinism (PROTO-1-7B: 3× byte-identical holds — of a CSS phantom); GrammarIr
  equivalence needs the builder-emitter PROTO-1-7B proves absent. The determinism precondition is
  MET; the correctness meaning is empty until the emitter exists.
- **G-7A-c** Seed disposition (frozen-forever vs retire-after-fixpoint) UNRESOLVED; frozen-seed =
  hand parser is production forever → 7A-default is self-VERIFYING, not self-hosting (⊆ 7D).
- **G-7A-d** S0 ≤ ~600 LOC while *building* GrammarIr (not recognizing) is unproven and, on PROTO-2's
  census (15 construction sites + resolve_refs), likely inflates toward the hand parser — thesis-
  collapse risk unquantified.
- **G-7A-e** The GN-0/GN-1 re-author-the-emitter gate (XL-4) is absent from 7A's born-RED list.
- **G-7A-f** Twin-ladder (one fixpoint over bbnf+regex, or two) UNRESOLVED; double-counts the
  minimal-surface metric the family exists to minimize.

### 7B — TOTAL-SELF-HOST — **36%**

Better *characterized* than at 04:14 (we now know construction is expressible AND the emitter is
absent), but its central plane resolved to SPEC-ONLY by its own gate, and the doc hasn't caught up.

- **G-7B-a** Plane A: by its OWN make-or-break gate (REC-1), **demote to SPEC-ONLY** — the faithful
  emit FAILED (`bbnf.bbnf:421`), the passing emit is a byte-identical CSS phantom. The general descent
  emitter is architecturally absent. The doc's "ADVANCE-pending-measurement" is stale.
- **G-7B-b** Plane A Gate B (LOC ≤ 4× hand) **PASSED ON A PHANTOM** — 998 LOC certified a budget on
  a byte-identical CSS copy, not bbnf output. A gate that certifies on non-bbnf output is a
  passes-on-phantom / gate-that-cannot-fail hazard. The genuine bbnf self-emit LOC is UNMEASURED and
  unmeasurable until the emitter exists. The 21.5k anti-target remains un-bounded.
- **G-7B-c** Plane C: the BLOCKED-candidate RATIONALE is FALSIFIED (REC-2 / PROTO-2: construction
  floor=0). The plane is not blocked on "CFG-expressible IR"; it is blocked on the SAME absent
  emitter as Plane A. Re-diagnose before pass 2 attacks the wrong wall. Plus XL-1 (LOCK-3 3C
  double-home; conditional on a verdict not named as a gate).
- **G-7B-d** Plane A→D consumer-less/expressibility-vs-buildability: PROTO-2 proves construction is
  *expressible*; it does NOT supply the emitter PROTO-1-7B proves absent. "7B Plane A reaches parity
  in PURE grammar" (PROTO-2 verdict) is a necessary census result, NOT a buildable-emitter result —
  do not lift it as a 7B advance (prototype-evidence-misuse risk).
- **G-7B-e** Diagnostics parity (does a generated front-end emit span-precise errors as richly as
  2142 LOC of hand machinery?) unmeasured; rustc's deliberate hand-parser retention is standing
  evidence against.
- **G-7B-f** Plane D (templates) is the sole live-gradient plane, but its "shape-selected template +
  hole-fill" reads clean while the load-bearing "control flow / dispatch / tape-assembly expressible
  without a borrow checker" is the unconverged hard part (elegant-reduction trap; the god-template
  R5 risk).

### 7C — HOST-FUNCTION-LATTICE — **62%**

Still the most viable family; PROTO-2 handed it a concrete first host fn and a MEASURED bbnf totality
floor (construction=0, linking=1 TreeFold, IO=1, β-reduction absent). Caveats hold: the CSS floor is
UNMET and the typed half is LOCK-1-coupled.

- **G-7C-a** CSS floor UNMET — `@{}` HostCapture = `BBNF-UNSUPPORTED-HOST-CAPTURE` (`:222`, confirmed)
  while `color.bbnf`/`values.bbnf` depend on it. The headline "measured totality% per subsystem" is a
  PAPER number for CSS until HostCapture lowering ships. First *production* obligation, not a metric.
- **G-7C-b** typed-lattice half (signature into inference, tier off `&mut Ctx`, zero-cost re-verify)
  is a LOCK-1 CONSUMER (XL-3). Split the "most independent" claim: {kill `:798`, lower `:222`, name
  `resolve_refs`} independent; the rest LOCK-1-bound.
- **G-7C-c** Zero-cost CONTEXT tier (constitution v) still unverified for the tier that most
  threatens it — but PROTO-2 now names a REAL first CONTEXT fn (`resolve_refs`, TreeFold). It is
  proposed, not built; the invariant remains untested until it (or TOML `push_path`) round-trips.
- **G-7C-d** totality% counter + floor gate unbuilt; the planted-breach falsification is undone —
  a gate that has never tripped is not yet proven able to fail.
- **G-7C-e** T6 (`@host` survives `regen∘regen` on `bbnf.bbnf`) inherits the §0 parse wall (PROTO-1)
  AND the absent-emitter wall (PROTO-1-7B) — transitively blocked like the ladder; 7C is orthogonal
  for its in-tree wins but NOT for T6.

### 7D — META-CIRCULAR-ORACLE — **55%**

Lowest-risk posture, one edge proven RAN_MEASURED (the regex ledger). But that edge is the cheap
one, the bbnf edge is doubly walled, and one of its founding facts is falsified-but-uncorrected.

- **G-7D-a** §0 fact 4 recognizer-only framing FALSIFIED and STILL in the doc (REC-3). The producer
  public entry ships; the gate text and §0 still cite the bool recognizer. Unreconciled from two
  prior artifacts.
- **G-7D-b** The bbnf edge is BLOCKED at BOTH walls (parse: PROTO-1; emitter: PROTO-1-7B). ORACLE-ONLY
  for the edge that matters is gated behind the same re-author-the-emitter obligation as 7A/7B.
- **G-7D-c** On the one decidable edge (regex) NEITHER promotion gate fires (parity 0.76, perf 1.8×);
  so "7D subsumes 7A's fixpoint once bbnf is PROMOTED" is vacuous — nothing promotes. Disclosed
  (ORACLE-ONLY named terminal), but the subsumption framing oversells a relation that never
  activates.
- **G-7D-d** The NormalAst crux is now BETTER-scoped by PROTO-2, not resolved: construction IS
  expressible (PROTO-2), so a *builder* grammar that yields GrammarIr — enabling a tree-level oracle,
  not just span-granularity — is feasible-in-principle; but it needs the emitter PROTO-1-7B proves
  absent. The weak-oracle gap has a path, still gated on the absent machine.
- **G-7D-e** Parity 0.76 is soft: 9/12 divergences are ONE `host::lower` bug class (`{n,m}` reject);
  the number measures LOCK-5's incomplete fold (XL-2), not an oracle verdict. Stable verdict is perf
  (1.8×). Presenting parity as a co-equal gate failure over-weights a transient fold gap
  (prototype-evidence-misuse flag).
- **G-7D-f** Two divergent regex grammars — oracle undefined until LOCK-5 picks canonical `.bbnf`
  (`grammar/misc/regex.bbnf` 1006B present; `parse-that/rust/bootstrap/regex.bbnf` cross-repo).
  LOCK-5-blocked.

---

## 4. FAILURE-MODE CHECKLIST — HITS

- **Vacuous convergence:** 7D-c ("7D subsumes 7A once bbnf PROMOTED") — never activates (nothing
  promotes, PROTO-2-7D-regex-oracle). 7A-a (Rung 1 ships / Rung 2 doubly walled).
- **Gate-that-cannot-fail / passes-on-phantom:** 7B Gate B certified a 998-LOC budget on a
  byte-identical CSS copy (G-7B-b) — the sharpest new instance. 7A Rung 1 is the extant staleness
  check re-pointed.
- **Elegant-reduction trap:** PROTO-2's clean "construction=G / linking=H" phase split reads elegant,
  but the load-bearing "and then build the general descent emitter that lowers 15 sites + resolve_refs
  into working Rust" is exactly what PROTO-1-7B proves ABSENT — the census makes the boundary look
  crossable while the machine to cross it does not exist. 7B Plane D ("structural emission as grammar,
  kernels stay hand-Rust") — the "control flow without a borrow checker" step is unconverged.
- **Masked fallback:** the RequestFacts `Ok` is a LITERAL masked fallback — emit returns Ok by
  templating CSS and ignoring the grammar (`runtime_generator.rs:236-238`). PROTO-1-7B caught it; the
  gate as written would have passed it.
- **Consumer-less substrate:** 7B Gap B `ir.bbnf` — conditional on LOCK-3 picking 3C (XL-1);
  byte-identical-to-snapshot output whose only consumer is a drift-gate that needs no grammar.
- **Unverified gestalt:** §5's "families collapse toward 7D under measured pressure" is now MIXED, not
  uniform — the regex-oracle probe collapses the *promotion/perf* axis toward 7D, but PROTO-2 shows
  the *build-boundary* axis does NOT collapse (construction is expressible; 7B and 7C split cleanly at
  the phase boundary). The gestalt shape is verified on one axis, contradicted on the other.
- **Spec-cites-itself / stale-doc:** REC-1/REC-2/REC-3 — the 04:51 FAMILY-SPECS contradicts its own
  now-run briefs (Plane A gate, Plane C rationale) and still carries the falsified §0 fact 4. The doc
  is the authority the families cite; three of its load-bearing statements are stale against the
  probes it commissioned.
- **Prototype-evidence misuse:** PROTO-2's "reaches parity in PURE grammar" (expressibility census)
  must not be lifted as a 7B buildable advance (G-7B-d). PROTO-2-7D-regex-oracle's 0.76 parity
  over-weights one fixable fold bug (G-7D-e). Positive: all four probes are correctly RAN_MEASURED,
  not SPEC_ONLY; no SPEC_ONLY anchors a perf claim; PROTO-1-7B honestly declined a build-wall number
  rather than time a mislabeled CSS binary; PROTO-2-7D-regex-oracle's perf is cold/paired/load-
  recorded (refutation-law-iv compliant) with the `-O3`-vs-fat-LTO caveat disclosed.
- **Constitution:** clause (i) front-end totality is, jointly on PROTO-1-7B + PROTO-2 + the regex
  oracle, **expressible-in-principle but unbuilt and perf-losing on the one measured production edge**
  — the emitter is absent (PROTO-1-7B), the semantics are grammar-expressible (PROTO-2), and the sole
  measured promotion edge is 1.8× slower (regex). The lawful deliverable names BOTH the re-author-the-
  emitter build gate AND the perf floor as joint born-RED; no family does. clause (viii) Y.13 at risk
  in 7B Gap B. clause (v) unverified for 7C's CONTEXT tier (now with a named-but-unbuilt first fn).
  clause (ix) partially honored (7C re-cuts GN-5; 7A/7B/7D undeclared net-new-vs-GN). clause (ii)
  one-home breach: two regex grammars (XL-6, cross-repo).

---

## 5. SUMMARY

Four RAN_MEASURED probes now bound the lock, and the two newest (05:01) resolve its two make-or-break
briefs with verdicts the 04:51 FAMILY-SPECS has not reconciled. **PROTO-1-7B** proved the current
skinny generator is TWO fixed per-grammar templates (JSON, CSS string constants), not a general
descent emitter: the faithful self-emit FAILS at `bbnf.bbnf:421`, and the only `Ok` is a
byte-identical CSS phantom — so 7B Plane A must demote to SPEC-ONLY by its OWN gate (REC-1), and its
LOC gate passed on a phantom (G-7B-b). **PROTO-2** proved the complementary truth: bbnf's GrammarIr
*construction* is pure-`->`-expressible (floor=0; no interner, post-order synthesized IDs, `resolve_refs`
factored out as the one separable TreeFold at floor=1) — which FALSIFIES 7B Plane C's BLOCKED-candidate
rationale (REC-2) and cleanly splits INC-2 at the phase boundary. Read together: **the target is
expressible (PROTO-2), the machine to realize it is absent (PROTO-1-7B), and building that general
descent emitter is the real, unnamed, born-RED gate (XL-4).** The dominant cross-lock findings pass 2
must gate: XL-1 (PROTO-2 did LOCK-3 3C's feasibility work — `ir.bbnf` is one deliverable homed twice,
conditional on an un-named verdict), XL-2 (the regex oracle's 12-row ledger is LOCK-5's `host::lower`
bug list surfaced through a LOCK-7 harness), XL-4 (skinny never had a general emitter; the gate is
re-author, not rescue — the largest under-gated obligation), XL-5 (LOCK-9 9C banks on a 1.8×-slower
regex route). Family state: **7C at 62%** ships value now (kill `passes:798`, lower `:222` HostCapture,
name `resolve_refs`), CSS floor UNMET, typed half LOCK-1-coupled. **7D at 55%**, lowest-risk, one cheap
edge proven, bbnf edge doubly walled, §0 fact 4 falsified-but-uncorrected. **7A at 42%** ships only its
can't-fail rung. **7B at 36%** — Plane A SPEC-ONLY by its own gate, Plane C's block mis-diagnosed,
Gap B a LOCK-3-conditional consumer-less smell. Deepest unstated truth, now sharpened by two probes:
constitution clause (i) front-end totality is EXPRESSIBLE but UNBUILT and PERF-LOSING; the lawful lock
deliverable is to name the absent-emitter build gate and the perf floor as joint born-RED — and no
family, and not the doc, does.
