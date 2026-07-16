# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — ADVERSARIAL CRITIC 2, pass 1

Critic 2 of 2. Cross-lock weight, per charter. Every family hardened against the failure-mode
checklist (vacuous convergence, spec-cites-itself circularity, gates-that-cannot-fail, the
elegant-reduction trap, legacy aliases, masked fallbacks, unverified gestalt, consumer-less
substrate) + constitution violations + prototype-evidence misuse. Load-bearing claims re-verified
on disk at HEAD (`ef10d5b` parse-that / skinny HEAD). Convergence numbers are stringent — pass 1,
two prototypes, zero clean re-audits, so nothing approaches the 100% bar (zero gaps + fresh audit
+ two clean passes).

---

## 0. Disk re-verification of the load-bearing facts (what I confirmed, not quoted)

| Claim | Source | Verdict on disk |
|---|---|---|
| bbnf front-end rejects `->` at byte 421 (line 9), the FIRST typed projection | PROTO-1 | **CONFIRMED** — `grammar/src/lib.rs` `parse_postfix`:1404, `parse_atom`:1495, catch-all "unexpected token" :1529; `?w` rejected, `BBNF-DIRECTIVE-NOT-IN-SKINNY`:1325/1712/1870; no `->` token in the RD parser |
| skinny xtask has NO `regen-bbnf`/`regen-regex` | §0 fact 1 | **CONFIRMED** — dispatch `main.rs:40-52` = json/sheets/css/real-typed/conformance/checkasm only |
| `@host` usage across shipped grammars = ZERO | 7C | **CONFIRMED** — `@host` appears only at its *definition* `bbnf.bbnf:73` + a comment in `types.bbnf:1`; no call site anywhere |
| self-grammars authored, wired to nothing | §0 fact 2 | **CONFIRMED** — `grammar/bbnf/{bbnf.bbnf 3448B, expressions.bbnf 1378B, types.bbnf 301B}` |
| **§0 fact 4 "public `parse_generated -> bool` DISCARDS the AST"** | FAMILY-SPECS §0 + research-D risk 5 | **FALSIFIED by PROTO-2, confirmed by me** — `regex/mod.rs:34 pub fn parse_generated(&str) -> Result<Hir, ParseError>` runs the generated parser THEN `host::lower` (`host.rs:19 pub fn lower(...) -> Result<Hir>`). The public API is a **producer**. §0 cited the *internal* `generated.rs:2288` bool recognizer and mislabeled it "public." |
| IR node algebra is hand-written (7B Gap B / LOCK-3 3C target) | 7B | **CONFIRMED** — `ir/src/lib.rs:234 ExprKind`, `:376 BackendExpr` |
| string heuristic `name.contains("host")&&…` | 7C | **CONFIRMED** — `passes/src/lib.rs:798`, feeding cost lever at `:620/:764` |

**The one confirmed spec error is load-bearing.** FAMILY-SPECS §0 fact 4 and research-D both
anchored 7D's central obligation ("surface the producer — the recognizer→producer gap defines the
first prototype", G-7D-3) on a fact that is false: the producer public entry already ships. This
is not cosmetic — it is the difference between "7D's first ledger row requires a new producer
surface" and "7D's first ledger row is buildable today," which is exactly what PROTO-2 did. **The
FAMILY-SPECS document still carries the falsified framing in its 7D gate text and §0 fact 4; it
has not been reconciled.** That is an open gap against the doc itself.

---

## 1. CROSS-LOCK VIEW (critic-2 charge) — where LOCK-7 contradicts or double-homes the codex

**XL-1 · 7B Gap B ⊗ LOCK-3 3C — the same `ir.bbnf`, homed twice, conditional on an un-named
verdict.** 7B Gap B authors `ir.bbnf` generating `ExprKind`/`BackendExpr` (`ir/src/lib.rs:234/376`).
LOCK-3 3C GRAMMAR-HOMOIOUSIA is *verbatim* "the IR is itself grammar-specified — a bbnf-defined IR
grammar." These are one deliverable. FAMILY-SPECS 7B dodges with "aligns with LOCK-3 3C without
asserting it" — but that dodge is the hazard: LOCK-3 has FOUR families (3A CONTRACT-TOWER, 3B
SINGLE-IR-MULTIPASS, 3C, 3D STAGED-RESIDUAL), and if the LOCK-3 convergence picks anything but 3C,
7B Gap B's `ir.bbnf` has **no consumer in the IR spine** → consumer-less substrate (Y.13,
constitution viii), the exact pathology research-priorart §4 trap 7 names. Gap B is therefore
**conditional on a cross-lock verdict it does not name as a gate.** Neither LOCK-3's charter nor
LOCK-7's names the double-home; the portfolio's "two routes sharing a mechanism share a family"
rule says these should collapse in the GESTALT-B weave. Owed: a named supersession or a joint
family, not "aligns without asserting."

**XL-2 · The regex differential oracle is double-homed 7D ⊗ LOCK-5.** PROTO-2's entire artifact —
Hand `bbnf_regex` vs Gen (`generated.rs`→`host::lower`→`Hir`) structural-`Hir` differential — IS
LOCK-5B LAWFUL-FUNCTOR ("two HIRs, one total functor each way, tested by round-trip oracles") and
LOCK-5A ("classify becomes an IR analysis pass") material. The regex `.bbnf`→`RegexParserEnum`→
`host::lower`→`Hir` pipeline is precisely what LOCK-5 governs. 7D measures it as a *promotion
ledger row*; LOCK-5 owns it as *the engine's HIR construction*. The 12-row divergence ledger
(9× `{n,m}` reject, `.` metachar, `(?s)`) is a bug list against `host::lower` — which is **LOCK-5's
semantic fold, not LOCK-7's**. So PROTO-2's headline "parity 0.76" is a measurement of LOCK-5's
incomplete fold, surfaced through a LOCK-7 harness. Adjudication owed in GESTALT: is the regex
oracle a LOCK-7 deliverable (bootstrap verification) or a LOCK-5 deliverable (functor round-trip)?
Currently claimed by both; the fix to move parity is entirely inside LOCK-5's `host.rs`.

**XL-3 · 7C's typed leaf ⊗ LOCK-1 — the "most independently viable" verdict is half true.** 7C's
core mechanism ("`@host name : InType -> OutType` flows from `types.bbnf` into LOCK-1 inference as
a typed leaf whose result unifies into the projection algebra") is **LOCK-1-coupled**. The
genuinely in-tree, LOCK-1-free wins are T2 (delete `passes:798`) and T4 (`@{}` HostCapture
lowering). T1/T5 (typed signature into inference, tier read off `&mut Ctx`, zero-cost
re-verification) require LOCK-1's solver to exist and to admit a typed leaf. FAMILY-SPECS'
"NOT blocked / most independently-viable family" verdict conflates the two halves. Accurate
statement: **the horizontal in-tree deletions are independent; the typed-lattice half is a LOCK-1
consumer.** Not a contradiction, but an un-named dependency inside a "needs no other lock" claim.

**XL-4 · GN-0 archives the feasibility evidence; the target lives on the far side. (The single
largest cross-lock gap, under-gated.)** research-A §1.3 + research-A(7A) §1: the WORKING bbnf/regex
self-host + fixpoint gate + the 21557-LOC generated `bbnf.rs` that PROVES the emitter once lowered
bbnf all live in the **legacy `crates/` tree GN-0 ARCHIVES**; SPEC §1's port-organ list OMITS them;
skinny's ported xtask self-hosts nothing. So constitution clause (i) is "on the chopping block, not
the port manifest." **PROTO-1 sharpens this to a contradiction:** the feasibility proof (legacy
`bbnf.rs`, OLD emitter) is being archived, while the NEW skinny front-end can't parse `->` at all
(byte 421). Every family's integration seam says "add `regen-bbnf` to `xtask/main.rs`" — but that
is the *skinny* xtask GN-1 ports, and the organ that made it feasible is in the archived tree. No
family names a **GN-0/GN-1 rescue-or-re-author gate** as born-RED; FAMILY-SPECS §0 fact 1 states
the fact but discharges it nowhere. This is the load-bearing cross-lock supersession the whole lock
depends on and it is currently a footnote, not a gate.

**XL-5 · LOCK-9 9C consumes LOCK-7 with no reciprocal edge.** LOCK-9 9C SCANNER-AS-DFA dissolves
quote/number/ws scanning into "compiled DFA/prefilter products of the bootstrapped grammar." That
consumes LOCK-7's bootstrapped regex grammar totality — but on PROTO-2's evidence the bootstrapped
regex path is 1.8× slower and 9/51-incomplete. If LOCK-9 9C banks on LOCK-7's regex self-host being
production-grade, it inherits a route measured *not* to fire promotion. Not adjudicated in either
charter. A consumer edge that should be born-RED in the omega DAG.

**XL-6 · "Extant-tranche consciousness" (constitution ix) partially honored.** GN-5's totality-
witness waves (TOML fifth grammar, host-fn-as-name-gate audit, `check-name-gates`) ARE re-cut by 7C
(explicitly, and well — 7C's floor gate is the named extension of GN-5.W3). Good. But the GN codex's
*omission* of any bootstrap/fixpoint wave (research-A(7A) §1: "the extant GN codex omits the
bootstrap entirely") means 7A/7B/7D are **net-new tranches against GN**, not re-cuts — which is
lawful only if declared as a named addition. FAMILY-SPECS does not declare it. Owed.

---

## 2. PER-FAMILY CONVERGENCE + ENUMERATED OPEN GAPS

### 7A — STAGED-BOOTSTRAP — **45%**

The wall is now precisely located (PROTO-1: byte 421, `->`) and the fixpoint mechanism is real
(Rung 1 = `check_target`, extant). But the DISTINGUISHING value (Rung 2, the stage ladder) is
blocked, the seed disposition is unchosen, and PROTO-2 evidence says the promotion the family
gestures at will not fire.

- **G-7A-a** Rung 2 (the stage ladder — the *only* rung that proves self-hosting; Rung 1 is
  json/css's existing staleness check re-pointed) is BLOCKED on the §0 wall confirmed RED by
  PROTO-1. **Gate-that-cannot-fail hazard:** the family can bank "Rung 1 ships" as progress while
  its actual self-hosting proof is walled. FAMILY-SPECS names this honestly ("Rung 1 ships, but its
  self-hosting proof shares the ladder's single wall") — so it is *disclosed*, not masked — but the
  convergence % must reflect that the shippable part cannot fail and the distinguishing part is
  blocked.
- **G-7A-b** Q1 seed disposition (frozen-forever R4-i vs retire-after-fixpoint R4-ii) UNRESOLVED.
  Masked-fallback finding: R4-i "frozen seed" = the hand parser stays the production path *forever*;
  the generated parser never becomes production → 7A-default is **self-VERIFYING, not self-hosting**
  (INC-3 admits 7A-default ⊆ 7D). The family's own name over-claims what its default delivers.
- **G-7A-c** Q2 twin-ladder (one `fixpoint` xtask over bbnf+regex, or two) UNRESOLVED — and it
  double-counts the "minimal surface" metric the family is built to minimize.
- **G-7A-d** Q3 regex promote-or-delete RESOLVED by PROTO-2 toward oracle-only (perf 1.8× slower);
  FAMILY-SPECS should record this as closed-toward-7D, not still-open.
- **G-7A-e** seed_LOC headline metric unmeasured (the trade it exists to make is un-numbered).
- **G-7A-f** the GN-0/GN-1 rescue gate (XL-4) is not in 7A's born-RED gate list.

### 7B — TOTAL-SELF-HOST — **35%**

The most speculative family; two of three gaps carry BLOCKED-candidate or consumer-less risk, and
Gap A collapses into 7D under the measured pressure PROTO-2 supplies.

- **G-7B-a** Gap A (promote front-end to production) BLOCKED on the §0 wall (PROTO-1) AND
  measured-not-to-fire: PROTO-2's regex analogue (the only edge with committed code both sides)
  shows the generated path 1.77–1.81× slower — the front-end's generated RD-over-general-IR is the
  same architecture and will lose the same way. INC-2's own concession ("if diagnostics unmet Gap A
  demotes to oracle") = Gap A *is* 7D. **The family collapses toward 7D under contact.**
- **G-7B-b** Gap B (`ir.bbnf`) — **consumer-less-substrate smell FAMILY-SPECS misses.** Its gate is
  "generated type surface byte-identical to a frozen snapshot AND passes/codegen compile unchanged."
  If the output is byte-identical to the committed hand enums, `ir.bbnf` produces *nothing new* — a
  frozen-snapshot diff-gate already prevents drift *without* the grammar. Marginal value of the
  grammar over "just snapshot the enums" is unproven; the only consumer is a gate that can exist
  without the substrate. Plus XL-1 (conditional on LOCK-3 picking 3C).
- **G-7B-c** Gap C (emitter-template grammar) BLOCKED-candidate on its own terms (R3 category error:
  a template DSL emitting correct specialized Rust+NEON needs a type/borrow discipline equal to the
  codegen backend it replaces). Survives only under strict pure-structure scoping with all typed
  logic behind kernel FFI — an unproven scoping. **Elegant-reduction trap:** "structural emission as
  grammar, kernels stay hand-Rust" reads clean but the load-bearing step ("and the control-flow /
  dispatch / tape-assembly is expressible without a borrow checker") is the unconverged hard part.
- **G-7B-d** Q1 diagnostics parity (does the generated parser *emit* span-precise errors as richly
  as 2142 LOC of hand machinery?) unmeasured — and rustc's deliberate hand-parser retention (7B
  research prior art) is standing evidence it will not.
- **G-7B-e** Q3 regex totality is LOCK-5-dependent (the projection to lawful HIR) — XL-2, un-gated.
- **G-7B-f** build-time ceiling (four-plane fixpoint ≤ the 88s reference) unmeasured; risk of a
  bootstrap-of-bootstrap explosion (R2) unquantified.

### 7C — HOST-FUNCTION-LATTICE — **60%**

The most viable family, with the caveats critic-2 must weight: the "independent" claim is half
LOCK-1-coupled (XL-3), and its headline floor is presently UNMET.

- **G-7C-a** Q1 CSS floor UNMET — `@{}` HostCapture is `BBNF-UNSUPPORTED-HOST-CAPTURE`
  (`grammar/src/lib.rs:222`, confirmed) yet `color.bbnf:220`/`values.bbnf:69` depend on it. The
  family's headline "measured totality% per subsystem" is a **paper number for CSS** until
  HostCapture lowering ships (T4). This is the family's first *production* obligation, not a metric.
- **G-7C-b** the typed-lattice half (T1 typed signature into inference, T5 tier off `&mut Ctx`) is
  a LOCK-1 consumer (XL-3); the "needs no generated parser / most independent" verdict holds only
  for T2+T4. Split the claim.
- **G-7C-c** Q2 zero-cost CONTEXT tier (`&mut Ctx` a parse-time-only channel, stacks discarded at
  parse end) is asserted "re-verified per subsystem, not assumed" — but no CONTEXT host fn exists
  yet (usage = 0), so the invariant (constitution v) is **unverified for the tier that most
  threatens it.** T3 (TOML `push_path`) is the first real test and is unbuilt.
- **G-7C-d** totality% counter (leaves×branches partition) + floor gate unbuilt; T5's planted-breach
  falsification undone — a gate that has never tripped is a gate not yet proven able to fail.
- **G-7C-e** T6 (`@host` directive survives `regen∘regen` on `bbnf.bbnf`) inherits the §0 wall —
  `bbnf.bbnf` does not parse (PROTO-1), so this obligation is transitively blocked like the ladder.
  7C is orthogonal to the ladder for its *in-tree* wins but T6 is NOT orthogonal.

### 7D — META-CIRCULAR-ORACLE — **58%**

Lowest-risk posture, one edge proven RAN_MEASURED (PROTO-2 built the first complete ledger row).
But the proven edge is the cheap one, the important edge (bbnf) is walled, and the "7D subsumes 7A"
subsumption is a paper relation on current evidence.

- **G-7D-a** §0 fact 4 / G-7D-3 recognizer→producer framing FALSIFIED (the producer public entry
  `regex/mod.rs:34` already ships). Good for the family (row buildable today) but the **FAMILY-SPECS
  and gate text are unreconciled** — the doc still cites the bool recognizer as "the public entry."
- **G-7D-b** bbnf edge BLOCKED at the parse wall (PROTO-1). ORACLE-ONLY is reachable only *after* a
  T3-scale front-end extension (`->`, `?w`, `@{}`, `|params|`), so 7D's cheapest terminal for the
  edge that matters is gated behind the same wall as 7A/7B. "Weaker bar than promote-to-production"
  is true but the bar is still the wall.
- **G-7D-c** the promotion ledger is measured-empty-forever on the one decidable edge: PROTO-2 shows
  neither gate fires (parity 0.76 < 1.0 AND perf 1.8× > 1.0). So **"7D subsumes 7A's fixpoint once
  bbnf is PROMOTED" is vacuous** — nothing promotes. 7D honestly names ORACLE-ONLY as terminal, so
  this is disclosed, but the subsumption framing oversells a relation that never activates.
- **G-7D-d** Q1 equivalence relation ≡ — PROTO-2 made it concrete (`Empty ≡ Concat([])` must be
  normalized or parity over-counts) but the normalized ≡ is not yet pinned in the spec, and the
  `Hand = parse-then-resolve_refs-then-validate` vs `Gen = pure parse` pivot (must run shared
  semantic passes on both or neither) is stated, not implemented.
- **G-7D-e** the parity headline is soft: **9 of 12 divergences are ONE fixable `host::lower` bug
  class** (`{n,m}` reject), so 0.76 measures LOCK-5's incomplete fold (XL-2), not an oracle verdict.
  The stable verdict is perf (1.8×), not parity. Prototype-evidence-misuse flag: the parity number
  is presented as a co-equal gate failure when it is a transient snapshot of a fold gap.
- **G-7D-f** Q3 two divergent regex grammars — oracle undefined until LOCK-5 picks canonical `.bbnf`
  (confirmed: `grammar/misc/regex.bbnf` 1006B ≠ `rust/bootstrap/regex.bbnf` 1763B). LOCK-5-blocked.

---

## 3. FAILURE-MODE CHECKLIST — HITS

- **Vacuous convergence:** 7D-c ("7D subsumes 7A once bbnf PROMOTED") — subsumption that never
  activates because no edge promotes (PROTO-2). 7A-a (Rung 1 ships / Rung 2 walled).
- **Gate-that-cannot-fail:** 7A Rung 1 is the extant staleness check re-pointed; passing it proves
  codegen-freshness, not self-hosting. Disclosed by FAMILY-SPECS, but weight it in the %.
- **Elegant-reduction trap:** 7B Gap C ("structural emission as grammar, kernels stay hand-Rust") —
  the load-bearing "control flow is expressible without a borrow checker" is the unconverged hard
  part. §5 "ONE ladder posture on the SCOPE×POSTURE plane" — the map resolves nothing; the actual
  choice is explicitly deferred to pass 2 ("how far up SCOPE is feasible, does promotion ever fire").
- **Masked fallback / legacy alias:** 7A-default (frozen seed) keeps the hand parser as production
  forever → self-verifying dressed as self-hosting; INC-3 admits 7A-default ⊆ 7D.
- **Consumer-less substrate:** 7B Gap B `ir.bbnf` — byte-identical-to-snapshot output whose only
  consumer is a drift-gate that needs no grammar; conditional on LOCK-3 3C (XL-1).
- **Unverified gestalt:** §5 "families collapse toward 7D under measured pressure" is proven on the
  *regex* edge only; the bbnf edge is walled and unmeasurable, so the gestalt shape is verified where
  cheap and unverified where it matters.
- **Prototype-evidence misuse:** PROTO-2 parity 0.76 headline over-weights one fixable fold bug (9/12
  rows); the stable verdict is perf. FAMILY-SPECS §0 fact 4 + 7D gate text cite a FALSIFIED
  recognizer-only fact (producer already public) — a spec error that survived into the gate
  definitions. (Both prototypes are correctly RAN_MEASURED, not SPEC_ONLY — no SPEC_ONLY anchors a
  perf claim. PROTO-2's perf is cold/paired/load-recorded, refutation-law-iv compliant, `-O3`-vs-LTO
  caveat honestly disclosed.)
- **Constitution:** clause (i) front-end totality is, on PROTO-1+PROTO-2 evidence, **measured-
  infeasible for any converged family** — 7A-default/7D keep the hand parser (not grammar-driven),
  7B's promotion won't fire (perf), so the best achievable is oracle-verified-hand-parser. No family
  states this forced outcome outright; the honest lock deliverable is "clause (i) front-end totality
  named as the permanent measured exception, with Gap B + 7C the achievable totality." clause (viii)
  Y.13 at risk in 7B Gap B. clause (ix) partially honored (7C re-cuts GN-5; 7A/7B/7D are undeclared
  net-new-vs-GN tranches). clause (v) unverified for 7C's CONTEXT tier (no such host fn exists yet).

---

## 4. SUMMARY

Two RAN_MEASURED prototypes did real adversarial work: PROTO-1 relocated the shared wall EARLIER
than §0 presumed (the front-end can't parse `->` at all — byte 421 — so "does the emitter lower it"
is unreachable-by-construction), and PROTO-2 both corrected a load-bearing §0 spec error (the regex
producer public entry already ships) and resolved INC-2 toward 7D on the one decidable edge (neither
promotion gate fires: parity 0.76, perf 1.8× slower). The lock's honest state: **7C is the family
that ships value now** (in-tree deletions T2/T4, LOCK-1-independent), at 60%, with its CSS floor
presently UNMET and its typed half LOCK-1-coupled. **7D at 58%** is the lowest-risk ladder posture
with one proven edge, but its important (bbnf) edge is walled and its "subsumes 7A" relation never
activates on current perf. **7A at 45%** ships only its can't-fail rung; its distinguishing rung and
its seed disposition are blocked/unchosen. **7B at 35%** is the most speculative — Gap A collapses
into 7D under measured contact, Gap B is a consumer-less-substrate/LOCK-3-conditional smell, Gap C is
a BLOCKED-candidate category error. The dominant cross-lock findings pass 2 must gate: XL-4 (GN-0
archives the feasibility evidence while the target is on the far side — the largest gap, currently a
footnote not a born-RED gate), XL-1 (`ir.bbnf` double-homed with LOCK-3 3C, conditional on an
un-named verdict), XL-2 (the regex oracle is LOCK-5's fold surfaced through a LOCK-7 harness — the
fix to move parity lives entirely in LOCK-5). Deepest unstated truth: on the measured evidence,
constitution clause (i) *front-end* totality is infeasible for every converged family, and the
lawful deliverable is to name it as the permanent exception — no family says so.
