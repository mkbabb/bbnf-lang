# LOCK-7 pass-1 — ADVERSARIAL CRITIC 1 of 2 (WITHIN-LOCK view): internal soundness + prototype sharpness

Critic 1 charter: weight the *within-lock* view — internal soundness of each family spec and whether
the two prototypes tested the *sharpest* uncertainty. This critique reads the **09:51 FAMILY-SPECS**
against the **10:02 PROTO-2** and **10:05 PROTO-1** (the newest artifacts, which POST-DATE the specs
by 11–14 minutes) plus the sibling **PROTO-1-7B-selfemit** and **PROTO-2-7D-regex-oracle**. Every
load-bearing `path:line` was re-compiled/greped on disk at skinny HEAD; where a probe and a prior
critique disagreed, the disagreement was settled against source, not quoted.

---

## 0 · HEADLINE — the synthesis is STALE against its own newest probes, and it re-imports a source-false parse claim

Two structural problems dominate everything below.

**H-1 · The FAMILY-SPECS (09:51) predate PROTO-1 (10:05) and PROTO-2 (10:02) and were NOT reconciled
to them.** §6 lists "the two live uncertainties that most advance the next pass" as prototype briefs
still to run — `bbnf-worktree` (build-wall close + fixpoint) and `spec-probe` (construction census).
**Both have since run and returned verdicts that undercut the specs' optimistic statuses:** PROTO-1
returned **CASCADE** (N = 6 > 5 parse-layer kinds + a required general recursive emitter + new
ExprId-arena construction), and PROTO-2 returned **7C-bound, floor = 2** (irreducible 1). The specs
carry 7A as **VIABLE**, 7B Plane A as **ADVANCE**, and §5 declares the budget bound **DISCHARGED** —
none of which survives the probes they themselves commissioned. The lock's own evidence has moved
past its synthesis; pass-2 must re-cut the statuses on the CASCADE / floor-2 results, not the 09:51
framing.

**H-2 · §0 asserts a DISK-FALSE parse fact and builds the "GREEN recognition" gestalt on it.**
FAMILY-SPECS §0 states, as a HEAD-verified fact: *"The skinny hand seed also rejects the same
construct (`BBNF-PARSE: unexpected token '-'` @421/@323 — `->` after a regex leaf)"* → *"So
recognition + determinism are GREEN; the wall is precisely leaf-projection lowering … it fires at the
first projected leaf."* **This is false at HEAD.** `parse_grammar` (`grammar/src/lib.rs:29`) routes
`->` through `parse_postfix:1409` (`self.source[cursor..].starts_with("->")`) → `parse_projection:1481`
→ `ExprKind::Project`. The hand parser *accepts* `-> Span` after a regex leaf. The only path that
rejects `->` is the **runtime-facts scanner** (`:492 rest.starts_with("->")` → `BBNF-UNSUPPORTED-
PROJECTION`), a different ingestion path. FAMILY-SPECS conflates the two paths into a single "both
walls hit the same construct at byte 421" narrative — a claim PROTO-1's Run A repeats
(`STAGE 1 parse_grammar : ERR BBNF-PARSE "unexpected token -" offset=417`, rationalized as
*"parse_atom accepts only `" / ( ident`"* ) and which the on-disk `parse_postfix`/`parse_projection`
refute. The prior CRIT-1/REGISTRY round already corrected this (hand parser walls at ~662 on the
first `?w ->`, not on the `->` itself); the 09:51 synthesis **re-imported the corrected error**. The
"the wall is *precisely* leaf-projection lowering, fires first" thesis is therefore built on a
misattributed root cause: the *facts scanner* rejects `->`; the *hand parser* does not; the real
builder wall is the emitter (below), not the projection.

**Net.** The lock's genuine finding — RECOGNIZE ≠ BUILD, and the blocker is the absence of a
grammar-generic descent emitter (both live emitters are fixed JSON/CSS templates) — SURVIVES and is
correct. But the specs over-credit the projection as "the" wall and under-credit the emitter, and
they have not folded the CASCADE/floor-2 verdicts. That is the shape of every per-family gap.

---

## 1 · FAMILY 7A — STAGED-BOOTSTRAP — convergence **40%**

**Sound core.** The staging shape (rustc `S1≡S2`, not `S2≡S0`), blake3 provenance-ledger, and the
reuse of `regen.rs check_target` for the staleness rung all cohere. The lock honestly concedes the
build-fixpoint is not achievable at HEAD.

**Defects (within-lock).**
- **G-7A-1 · The one MEASURED-GREEN gate is VACUOUS (gate-that-cannot-fail).** §1 gate (b) —
  *"3×-run determinism … already MEASURED GREEN on the request-facts path (3× byte-identical)"* — and
  §0's *"the fixpoint determinism precondition HOLDS on this path"* both rest on PROTO-1-7B's
  request-facts run. But that run emits a **byte-identical CSS-declaration-values template** for any
  input (`runtime_generator.rs emit_request_facts` stamps only config facts; the body is a fixed const
  string). Emitting a hard-coded constant three times is *trivially* identical; it certifies nothing
  about a bbnf builder fixpoint. The gate as measured **cannot fail** — it distinguishes neither
  self-hosting from a fixed string nor a correct generator from a wrong one. 7A has no non-vacuous
  determinism evidence.
- **G-7A-2 · The non-vacuous fixpoint is blocked on the CASCADE, not "3 fixes."** §1 open-Q1 (*"what
  is the fixpoint OVER?"* — `.rs` bytes = determinism-only vs `GrammarIr` equivalence) is the whole
  ballgame, and PROTO-1 has now answered the sub-question negatively: the `GrammarIr` fixpoint needs
  a general recursive emitter that **does not exist** (both emitters are JSON/CSS-template-bound;
  even with all six parse-layer kinds relaxed, Run B dies in `json_sink_direct`'s tri-span/DirectBuild
  guard). 7A cannot mint a self-reparsing S0 at all yet. The status "VIABLE (rescue-and-port;
  determinism GREEN)" is unsupported: the port target is the ARCHIVED legacy tree, and the greenfield
  emitter is architecturally absent.
- **G-7A-3 · S0 ≤ ~600 LOC is un-anchored and thesis-collapse-prone.** §1 open-Q2 fears "build-not-
  recognize inflates S0 toward the hand parser." PROTO-2's floor-2 (arena + link are host-bound in
  current bbnf) makes that inflation concrete: a *building* S0 must carry the arena/link mechanism the
  recognizer S0 omits. The ~600 figure has no valid basis until the construction surface is fixed.

**Enumerated open gaps.** (a) no general-descent emitter exists → 7A cannot produce a self-reparsing
S0 (PROTO-1 CASCADE). (b) the only measured determinism is the CSS-phantom const (vacuous); real
`GrammarIr`-fixpoint determinism unmeasured. (c) fixpoint *meaning* (bytes vs GrammarIr) unresolved
and empty until a builder grammar exists. (d) S0 LOC ceiling un-anchored; the ~88s→~0.5s build datum
is a legacy-tree transplant (skinny xtask self-hosts *nothing* — verified: 0 matches for
`regen-bbnf|regen-regex|fixpoint` in `xtask/src/main.rs`). (e) the GN-0 rescue gate — a working
legacy self-host is being archived while the greenfield has none — is not born-RED in 7A's gate list.

---

## 2 · FAMILY 7B — TOTAL-SELF-HOST — convergence **30%**

**Sound core.** Plane D (templates-as-data) is a real in-tree gradient. PROTO-2 legitimately
de-risks the "graph-shaped interned IR ⇒ as hard as the original problem" fear *for the construction
arena*: it is empirically a non-interned, post-order, append-only **tree** (PROOF A/A'), hence a
catamorphism. Plane C's own BLOCKED-candidate self-assessment is candid.

**Defects (within-lock).**
- **G-7B-1 · §0/§5 discharge the budget bound on a PHANTOM — prototype-evidence misuse.** §0 reports
  *"self-emit succeeds, 998 LOC (0.046× the ghost), 3× byte-identical … The 21k blow-up does NOT
  recur. This retires 'budget is the make-or-break unknown'"*; §5 declares the cross-cutting bound
  **DISCHARGED**. But the probe that produced the 998 LOC (PROTO-1-7B) labels it in its own verdict:
  *"the 998-LOC 'Ok' is a phantom … a mislabeled CSS copy … it does not bound 7B … genuine bbnf
  self-emit LOC remains UNMEASURED."* The synthesis strips the phantom qualifier and uses the number
  to retire a risk the measuring probe says it **cannot** retire. This is the constitution-vii /
  SPEC_ONLY-anchoring-a-real-claim violation in its purest form: a byte-identical CSS template is
  cited as a self-emit budget. The budget bound is **not** discharged; it is unmeasured.
- **G-7B-2 · Plane A is CASCADE, and the floor-0 it leaned on is now floor-2.** §2 Plane A ("ADVANCE
  … budget MEASURED 998 LOC … the blow-up fear is retired") is falsified twice: by PROTO-1 (CASCADE —
  needs a general emitter + new ExprId-arena construction, N=6 parse kinds) and by PROTO-2 (the build
  *mechanism* is host-bound: `add_expr` Hs + `resolve_refs` Hh). Plane A cannot reach pure-grammar
  parity (floor > 0) and cannot be emitted at all (no descent emitter). "ADVANCE" is unsupported.
- **G-7B-3 · Plane D's decision is pointed at a probe that never measured it.** §2 states *"Plane D
  … Decided by Proto-2's data-vs-escape ratio: ≥50% data → viable."* PROTO-2 measured the
  **construction census** (INC-2), NOT an emitter-template data-vs-escape ratio. **No probe in the
  lock measured Plane D's ratio.** PROTO-1 confirms the emitter is JSON-template substitution
  (items 7–8) but does not quantify data-vs-escape. Plane D is therefore 100% SPEC_ONLY, and its
  "decided by Proto-2" is an unmet promise — a brief/result mismatch. The maximal claim, which §2
  admits "hangs on Proto-2," has no data at all.
- **G-7B-4 · Plane B `ir.bbnf` is a consumer-less-substrate risk (clause viii/Y.13) conditional on an
  un-named cross-lock verdict.** Its gate ("generated type surface byte-identical to a frozen
  snapshot") yields nothing a drift-gate over the enums doesn't already give; the only consumer is a
  gate that needs no grammar. Double-homed verbatim with LOCK-3C (GRAMMAR-HOMOIOUSIA). If LOCK-3 picks
  3A/3B/3D, `ir.bbnf` has no IR-spine consumer. Zero prototype; label SPEC_ONLY.
- **G-7B-5 · The collapse-to-7D is proven only on the regex edge, unverified where it matters.**
  PROTO-2-7D-regex-oracle measured GEN **1.77–1.81× slower** and structural parity **0.76 < 1.0** —
  neither promotion gate fires. That is real, but it is the *regex* edge; the *bbnf* edge (the family's
  headline) is unbuilt (no emitter), so the "7B collapses to 7D" gestalt is an unverified extrapolation
  from regex to bbnf.

**Enumerated open gaps.** (a) Plane A: CASCADE — no descent emitter + arena/link host-bound (floor 2).
(b) budget bound NOT discharged (phantom). (c) Plane D: zero data; "decided by Proto-2" unmet. (d)
Plane B: consumer-less / LOCK-3-conditional / unprototyped. (e) Plane C: BLOCKED-candidate, unscoped
(PROTO-2's floor-0-for-the-arena must NOT be read as relieving the mid-IR `BackendExpr` graph Plane C
targets — different arena). (f) diagnostics parity (does a generated parser emit spans as richly as
the 2142-LOC hand machinery?) unmeasured.

---

## 3 · FAMILY 7C — HOST-FUNCTION-LATTICE — convergence **55%**

**Soundest in-tree substrate, and PROTO-2 gives it its first measured floor instance.** Every claim
verified on disk: the string heuristic `passes/src/lib.rs:794-798` (`name.contains("host") &&
(value.contains("decode") || value.contains("parse"))`); the dormant typed `@host` directive
(`bbnf.bbnf:73`); `@{}` (`bbnf.bbnf:33`) → `BBNF-UNSUPPORTED-HOST-CAPTURE` (`grammar/src/lib.rs:222`);
the six-variant `FnDescriptor`. PROTO-2's `resolve_refs` (whole-grammar name link) is a real,
measured, `TreeFold`-class host-fn instance — exactly the "typed, counted, floored" home 7C promises.
Its in-tree wins (delete the heuristic → `check-json` bit-for-bit; lower `@{}`) need no self-emit and
are genuinely the lock's cheapest signal.

**Defects (within-lock).**
- **G-7C-1 · "the irreducible floor = 1" is SOFTER than PROTO-2 asserts (elegant-reduction / asymmetry
  in the census).** PROTO-2 marks `add_expr` **SOFT** (dischargeable to 0 by a value-language
  arena/list primitive) but `resolve_refs` **HARD** ("no synthesized `->` can express it *in
  principle*; bbnf has no inherited/global attribute"). The asymmetry is under-justified: BOTH are
  "not expressible by *today's* `->`, dischargeable by a language extension." Name resolution is
  expressible by an *inherited-attribute* / two-pass `->` extension exactly as the arena is
  expressible by a list primitive — and augmenting BBNF for a missing feature is the standing law, not
  a workaround. So the truly-irreducible floor may be **0**, and "irreducible = 1" conflates "beyond
  current `->` semantics" with "beyond `->` in principle." This is a LOCK-1 question (does the type/
  attribute plane admit inherited attributes?), which makes 7C's floor a *dependent* number, not a
  fixed one. The census's `count(HOST)=2` is also partly definitional — it follows from the dispatch's
  imposed invariant ("arena mutation MUST classify HOST"), not a neutral measurement.
- **G-7C-2 · ZERO prototype exercised 7C (constitution-vii).** For the cheapest-to-probe family in the
  lock (delete one heuristic, run `check-json`; born-RED gates (a)–(e) all specified), not one gate
  was executed. "Lowest-risk in-tree win" is asserted, not demonstrated. This is the single most
  cost-effective missing prototype in LOCK-7 and the fastest way to move 7C's convergence.
- **G-7C-3 · "genuinely orthogonal / needs no self-emit" is overstated for gates (d)/(e)/T6.** The
  totality-*census* (`xtask census-totality`, the family's measured deliverable) and the
  regen∘regen-survives-on-bbnf gate both need bbnf to parse+build, which needs the emitter (FV-2). 7C
  is orthogonal for the heuristic-deletion and `@{}`-lowering gates, NOT for its census/percentage
  deliverable — which is the deliverable that *defines* the family. So the family's headline metric
  inherits the lock's central blocker.
- **G-7C-4 · Purity verification is LOCK-1-coupled (named, not a block, but load-bearing).** Gate (e)
  ("a `pure` sig taking `&mut Ctx` is a type error, not trusted") is entirely a LOCK-1 inference
  obligation; 7C cannot self-certify it.

**Enumerated open gaps.** (a) the "irreducible floor = 1" is a dependent/soft number pending a LOCK-1
inherited-attribute verdict; could be 0. (b) zero gate executed — SPEC_ONLY despite being cheapest. (c)
the census/percentage deliverable is NOT self-emit-independent (needs the emitter). (d) purity axis
LOCK-1-coupled. (e) PROTO-2 mis-cites `resolve_refs`'s error type (`UnresolvedRef` vs on-disk
`ValidationError`) — cosmetic, but a "re-verified at HEAD" slip.

---

## 4 · FAMILY 7D — META-CIRCULAR ORACLE — convergence **48%**

**Sound core.** The lawful entry ramp — an emitted parser's first consumer is the differential oracle,
curing the Y.13 dormant-substrate violation the instant a parser exists. The metric ladder
(L0→L4), golden-anchor-not-majority-vote, and planted-mutation falsification are well-formed. Its
first output (fail at the projection) is "the oracle working as designed."

**Defects (within-lock).**
- **G-7D-1 · The NormalAst differential is WEAK until the builder grammar exists (recognize≠build
  collides with the oracle).** §4 open-Q1 concedes it: the generated side yields a span-tape, so the
  differential compares at *recognition/span granularity* only — it cannot see wrong-precedence or
  wrong-construction. The oracle's discriminating power (L2 NormalAst equality, the promotion floor)
  is gated on the SAME missing emitter (PROTO-1 CASCADE). Until then 7D is an accept/reject +
  node-histogram oracle (L0/L1), below its own stated promotion floor.
- **G-7D-2 · The "first output = fail at byte 421" claim is confounded by H-2.** §4 open-Q2 asserts
  "the generated grammar under-specifies at byte 421 (`-> Span`) — the augment-BBNF task list starts
  there." But the failure is in the *facts scanner* path, not a differential between two *parsers*; the
  hand parser accepts `-> Span` (disk-verified). A differential oracle that "fails at byte 421" is not
  yet running two parsers against each other — it is one scanner rejecting a construct. The oracle's
  actual first falsifiable number is unrun.
- **G-7D-3 · The perf gate is vacuous for the honest bbnf prize.** §4 open-Q3 concedes it: cold
  compile-time bbnf parsing has no perf teeth; the real prize is LOC deletion (retire 2142 hand LOC),
  and perf teeth exist only for runtime-hot regex leaves — where PROTO-2-7D-regex-oracle already
  measured GEN **1.77–1.81× slower** / parity **0.76**, i.e. the one edge with teeth currently
  FAILS both gates. So 7D's promotion is blocked on regex-engine work (LOCK-5) it does not own.

**Enumerated open gaps.** (a) L2+ oracle strength blocked on the emitter (same CASCADE). (b) the
"first output fails at 421" framing conflates the facts-scanner reject with a parser differential
(H-2). (c) the sole teeth-bearing edge (regex) fails both promotion gates and depends on LOCK-5. (d)
NormalAst over-canonicalization risk (named) unprototyped. (e) hand-parser fate under INC-4 (retire
production vs keep CI oracle) unchosen.

---

## 5 · INCOMPATIBILITY MAP — critique

- **INC-1 (7A big-bang fixpoint ⊥ 7D incremental oracle).** Reconciliation (compose the gates, choose
  the trajectory) is sound. But the tilt-to-7D-trajectory-with-7A-fixpoint-as-precondition rests on
  the precondition being GREEN — and it is only *phantom*-GREEN (G-7A-1). The precondition is unmet,
  so the composition currently has no floor. **Accepted with the correction that the 7A precondition
  is not yet satisfied.**
- **INC-2 (7B build-in-grammar ⊥ 7C build-behind-host).** FAMILY-SPECS lists this as "open, located by
  Proto-2." PROTO-2 has LOCATED it: 7C-bound, floor 2 (irreducible 1). So INC-2 is **decided toward
  7C** at pass-1's evidentiary level — but the "irreducible 1" is soft (G-7C-1), so the decided value
  is "7C-bound, floor ∈ {0,1,2} pending a LOCK-1 inherited-attribute verdict + a value-language arena
  primitive." The specs should record INC-2 as *resolved-in-direction, floor-pending*, not "open."
- **INC-3 (7A codegen-as-trusted-primitive ⊥ 7B Plane D codegen-as-grammar).** Correctly flagged HARD.
  But the specs claim Proto-2's ratio decides Plane D feasibility — and Proto-2 measured no such ratio
  (G-7B-3). So INC-3 is not merely un-adjudicated; it is **un-instrumented**: no probe touched the
  data-vs-escape question. Pass-2's sharpest un-run experiment is here, not in the construction census
  (already run) or the projection wall (already CASCADE).
- **INC-4 (hand-parser fate).** "Not hard / composable" is correct.

---

## 6 · CONSTITUTION & PROTOTYPE-MISUSE LEDGER (within-lock)

| # | clause | finding | severity |
|---|---|---|---|
| L-1 | vii (concrete artifacts / SPEC_ONLY) | §0/§5 cite the 998-LOC **CSS phantom** to "retire"/"discharge" the budget bound the producing probe says it cannot bound (G-7B-1) | HARD |
| L-2 | "gates that cannot fail" | 7A gate (b) MEASURED-GREEN determinism is a hard-coded CSS const emitted thrice (G-7A-1) | HARD |
| L-3 | soundness (re-verified-at-HEAD header) | §0 "hand seed rejects `->` at 421" is disk-false (`parse_postfix:1409`/`parse_projection:1481` accept it); re-imports a corrected error (H-2) | HARD |
| L-4 | vii / brief-result match | Plane D "decided by Proto-2's data-vs-escape ratio" — Proto-2 measured the construction census, not that ratio; Plane D has zero data (G-7B-3) | HARD |
| L-5 | i (grammar totality) | "irreducible floor = 1" conflates "beyond current `->`" with "beyond `->` in principle"; name-resolution is inherited-attribute-expressible → floor possibly 0 (G-7C-1) | MEDIUM |
| L-6 | viii / Y.13 (consumer-less substrate) | Plane B `ir.bbnf` gate consumes only a drift-check; sole consumer needs no grammar; LOCK-3-conditional (G-7B-4) | MEDIUM |
| L-7 | ii (one regex home) | the clause-ii breach cites `parse-that/rust/bootstrap/regex.bbnf` (72 LOC) — **absent from this checkout** (cross-repo); the "72-LOC full L4" figure is unverifiable at HEAD despite the "re-verified" header | LOW |
| L-8 | staleness of synthesis | 09:51 specs never reconciled to 10:02/10:05 probes; statuses (7A VIABLE, 7B ADVANCE, budget DISCHARGED) contradict the CASCADE/floor-2 results (H-1) | HARD |

---

## 7 · Did the prototypes test the SHARPEST uncertainty? — the critic-1 verdict

**Partly, and both stopped one step short of the single decisive experiment.**

- **PROTO-1 (CASCADE)** did real adversarial work (it inverted the expected result — the blocker is
  the JSON-only emitter, not a `->` rule). But it measured the *known* emitter gap (PROTO-1-7B already
  established both emitters are fixed templates) and its N=6 count aggregates walls across an unmodified
  Run A + a hand-relaxed Run B, so "N>5 ⇒ CASCADE" leans on a soft threshold. Its own nuance concedes
  the projection *semantics* are bounded (≈3 leaf kinds, no arena) — that reassurance is SPEC_ONLY (no
  general emitter was built to ride it), and its Run A root-cause (parse_grammar lacks `->`) is
  disk-false (H-2).
- **PROTO-2 (floor-2)** dismantled the "interned graph IR" fear for the construction arena (genuine,
  measured) and gave 7C its first host-fn instance. But its HARD/SOFT asymmetry is under-justified
  (G-7C-1) and its floor is partly a consequence of an imposed classification invariant.
- **The one experiment that resolves the lock was run by NEITHER**: author `->` builder projections on
  bbnf's structural rules over a **general descent emitter**, emit a parser, and show it reparses the
  corpus into `GrammarIr`-equivalent structure while `regen∘regen` holds byte-identical. Every
  family's headline (7A fixpoint-meaning, 7B Plane A parity, 7C census%, 7D L2 oracle) hangs on it.
- **The genuinely un-instrumented uncertainty** is INC-3 / Plane D's data-vs-escape ratio for the
  10.2k emitter mass — no probe touched it, yet it decides the codegen-primitive-vs-plane fork.

**Pass-2 OPEN set (ranked):** (1) build a *minimal general descent emitter* and run the decisive
builder-fixpoint experiment (subsumes 7A/7B-A/7D-L2). (2) run the *free* 7C probe (delete the
heuristic, `check-json` bit-for-bit; lower `@{}`) — cheapest signal, still unrun. (3) instrument
INC-3 — measure Plane D's data-vs-escape ratio (never measured). (4) adjudicate the 7C floor against a
LOCK-1 inherited-attribute verdict (is `resolve_refs` dischargeable → floor 0?). (5) reconcile every
09:51 status to the 10:02/10:05 probes and strike the phantom-anchored discharges.
