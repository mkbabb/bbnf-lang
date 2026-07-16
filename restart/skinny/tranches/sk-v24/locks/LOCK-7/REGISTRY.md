# LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP — CONVERGENCE REGISTRY

Agglomerated by the AGGLOMERATOR (Fable-tier judgment). One row per family, carried forward
across passes. Convergence % is conservative — the critics' floor unless a named, disk-verified
reason overrides it. Every load-bearing `path:line` in this registry was **re-compiled/greped by
the agglomerator on the live skinny tree at HEAD `fe160a91a`** (the same HEAD CRIT-2 cited; PROTO-1
ran a *stale* worktree `wf_63419230-36d-32`, confirmed present in `.claude/worktrees/`). Where a
probe and a critic disagreed on a disk fact, the disagreement was settled against source, not quoted.

> **Supersession note (this rewrite).** The prior REGISTRY draft folded the FIRST prototype
> generation (`PROTO-1-7B-selfemit` → "998 LOC self-emit succeeds"; `PROTO-2-7D-regex-oracle` → 51
> patterns). This pass folds the SECOND, definitive generation — **`PROTO-1.md` (CASCADE)** and
> **`PROTO-2.md` (construction census, floor-2)** — against **CRIT-1** (within-lock) and **CRIT-2**
> (cross-lock), and corrects the single largest error in the lock's record. The prior draft's
> XL-4 ("re-author a machine skinny never had, the largest gap") is **RETRACTED**: a general
> grammar-walking descent emitter EXISTS on disk. See Governing Fact 1.

---

## STANDING VERDICT (current, after pass 1)

| family | name | status | conv % | one-line disposition |
|---|---|---|---|---|
| 7A | STAGED-BOOTSTRAP | ADVANCED (narrowed) | 42% | fixpoint LAW survives as 7D's graft; determinism-GREEN exposed as a phantom; subject-of-fixpoint unchosen |
| 7B | TOTAL-SELF-HOST | BANKED | 33% | Plane A re-characterized (recognizer-via-`general_descent`, builder gated); Plane C splits at the phase boundary; Plane D UN-INSTRUMENTED; Gap B → LOCK-3 |
| 7C | HOST-FUNCTION-LATTICE | ADVANCED | 55% | the survivor — ships value now (T2/T4, no other lock); orthogonal mechanism the winner consumes; zero direct probe is its cheapest gap |
| 7D | META-CIRCULAR-ORACLE | ADVANCED | 53% | POSTURE winner; "gen" side buildable NOW at *recognizer/span* granularity via `general_descent`; tree-oracle gated on the builder-augment |

**Crowding redirect applied:** 7A/7B/7D are three postures on ONE ladder that now converge on a
single, disk-located mechanism gap — **the bounded builder-augment on the extant `general_descent`
recognizer emitter** (recognizer → GrammarIr construction). 7B (the promote-to-production posture)
stays **BANKED**. Two underexplored formulations are opened in place of the crowding: **OPEN-1'**
(the builder-augment on `general_descent`, RE-CUT from the old "does an emitter exist" — it does)
and **INC-3 / Plane-D data-vs-escape ratio** (never instrumented by any probe — the sharpest un-run
experiment in the lock).

---

## THE GOVERNING FACTS (re-verified on disk this pass — supersede FAMILY-SPECS §0)

FAMILY-SPECS §0 (09:51) predates both definitive probes (10:02 / 10:05) and was never reconciled to
them; and both probes, plus §0, re-imported a source-false parse claim. Corrected ground truth:

1. **A GENERAL grammar-walking descent emitter EXISTS — the headline correction.** `crates/codegen/
   src/general_descent/{mod,emit,regex_first,view}.rs` = **1206 LOC** (mod 93 / emit 452 /
   regex_first 230 / view 431). `runtime_generator.rs:98` `if !sink_only.has_rule("value")` →
   `:99 return emit_general_descent(...)` → `general_descent::render` (`:159`). Dispatch is
   **shape-based, never a grammar name** (mod.rs:12 "grammar-NEUTRAL"). It serves a real, non-JSON /
   non-CSS operator-tower grammar **today**: `xtask/src/main.rs:42 "regen-sheets"` compiles
   `grammar/google-sheets/google-sheets.bbnf` through it. **bbnf.bbnf has no `value` rule** (greped)
   → it shape-dispatches to `general_descent`, exactly as Sheets does. FAMILY-SPECS §0's "the
   general emitter is architecturally absent / CompiledLowering emits a JSON parser for every
   grammar" and PROTO-1's CASCADE "dominant blocker … a JSON parser for every grammar" are **FALSE**.
   PROTO-1 drove `emit_from_source` → `json_sink_direct` (the `has_rule("value")`=true branch) and
   never reached the `!has_rule("value")` → `general_descent` branch. *The machine's existence was
   already in the research base* — research-B §1.3 lists `general_descent/emit.rs (452)` — and the
   synthesis flattened it away.

2. **But RECOGNIZE ≠ BUILD survives intact — it is a rider gap, not the absence of the machine.**
   `general_descent` mod.rs:9 states its output verbatim: *"one `parse_<rule>` recognizer per
   rule."* It emits **recognizers**, not GrammarIr **builders**. So the residual bbnf obligation is a
   *bounded builder-augment on top of an extant machine*: (a) projection lowering `-> Span`/`-> T`
   (recognizer → builder); (b) the mod.rs:17 coarse 4-variant regex classifier caveat; (c) closures.
   PROTO-2's floor-2 is the **host budget** for the builder half. This is "rescue-and-port + bounded
   augment" — the framing 7A originally used and PROTO-1/the-prior-XL-4 overrode.

3. **The parse wall was mis-attributed (again). The hand parser DOES handle `->`.** `parse_postfix`
   (`grammar/src/lib.rs:1404`) → `:1409 self.source[..].starts_with("->")` → `parse_projection:1481`
   → `ExprKind::Project` (`:1486`). Disk-verified: `-> Span` after a regex leaf **parses**. The path
   that rejects `->` is the **runtime-facts scanner** (`:492`), a *different ingestion path* than the
   hand builder parser. PROTO-1's Run A ("`unexpected token '-'` offset=417", root-caused to
   "parse_atom accepts only `" / ( ident`") is the **pre-SK-V18 parser** run from a stale worktree.
   The hand-parser wall, where it exists, is byte 662 (the first `?w` immediately followed by `->`,
   an attachment fix in `parse_seq`), `@{}` (no `@` arm in `parse_atom`), and `|params|` closures —
   **three bounded fixes** to get bbnf.bbnf *into* `general_descent`, not a rewrite.

4. **The construction floor is 2, irreducible to 1 (PROTO-2, honest overturn).** Node *values* are
   pure `->` (steps 1-3: `SpanCapture`, `NumberConvert`, `EnumWrap`/`BoxWrap` — floor 0). Two build
   *mechanisms* are host-bound: `add_expr` (`ir/src/lib.rs:51` — `id=ExprId(exprs.len()); push`,
   append-only, **SOFT** / dischargeable by a value-language arena primitive) and `resolve_refs`
   (`ir/src/lib.rs:84` — whole-grammar `HashMap<name,RuleId>` link, **HARD** / TreeFold). β-reduction
   ABSENT; `@{}` DORMANT; `@import` is CompileIO. **`count(HOST)=2`, irreducible floor 1
   (`resolve_refs`).** *Caveat (CRIT-1 G-7C-1, accepted):* the "HARD = irreducible" is a
   LOCK-1-**dependent** number — `resolve_refs` is an *inherited-attribute* whose expressibility
   turns on whether LOCK-1's type/attribute plane admits inherited attributes; the true floor is
   **∈ {0,1,2}**, not fixed. (Cosmetic: PROTO-2 cites the link error as `UnresolvedRef`; disk says
   `ValidationError`.)

5. **The regex producer already ships (FAMILY-SPECS §0 fact-4 FALSIFIED, still uncorrected in the
   doc).** `parse-that/.../regex/mod.rs:34 parse_generated(pattern) -> Result<Hir,ParseError>` is a
   **producer** (runs the generated parser THEN `host::lower`). §0/PROTO-2-7D cited the internal
   `-> bool` recognizer and mislabeled it "public." That is why the regex oracle was runnable.

**THE RE-CUT MAKE-OR-BREAK (the one that survives).** The emitter is NOT absent (Fact 1) and the
parse wall is 3 small fixes (Fact 3). The genuine, still-**UNPROBED** uncertainty is the
**BUILDER-AUGMENT on `general_descent`**: does authoring `-> Span`/`-> T` builder projections on
bbnf's structural rules over the extant 1206-LOC recognizer emitter yield a parser that reparses
the corpus into *GrammarIr-equivalent* structure (with PROTO-2's +1 SOFT / +1 HARD host budget)
while `regen∘regen` holds byte-identical — or does the augment cascade downstream? PROTO-1's CASCADE
was measured on the WRONG path (`json_sink_direct`), so **the builder-augment-on-`general_descent`
is untested as such.** That is OPEN-1' (below): every ladder family rests on it.

---

## THE THREE-WAY CONTRADICTION — ADJUDICATED (CRIT-2 §1, resolved on disk)

The record carried three unreconciled contradiction pairs; all three are now closed against HEAD.

- **CON-1 · the emitter.** FAMILY-SPECS "998-LOC self-emit succeeds" (phantom) vs PROTO-1 CASCADE
  "no emitter" (false-absence). **RESOLVED:** `general_descent` EXISTS and emits *recognizers*
  (Fact 1/2). The 998 LOC is a byte-identical CSS/JSON template (a masked-fallback phantom — the
  producing probe itself said so); PROTO-1's "absent" is a false negative from an un-driven path.
  Both wrong, in opposite directions; the truth is *recognizer-emitter present, builder-augment
  pending*.
- **CON-2 · the construction floor.** 05:01 census (floor 0) vs PROTO-2 (floor 2). **RESOLVED
  toward floor 2** — PROTO-2 honestly applied its dispatch invariant to overturn its own floor-0;
  SOFT half retracts to irreducible 1, and even that is LOCK-1-dependent (Fact 4).
- **CON-3 · the parse wall.** "rejects `->` @421" (§0 / PROTO-1) vs "handles `->` @1409, walls @662"
  (prior REGISTRY / CRIT-1). **RESOLVED:** `parse_postfix:1409` handles `->` (Fact 3); the fresh
  round re-broke a correction the record had already made, via a stale worktree.

---

## PASS 1 — history, per family

### 7A — STAGED-BOOTSTRAP — ADVANCED (narrowed) — 42%

- **Convergence 42%** (critics 40 / 43; lifted 2 off CRIT-1's floor with a named reason — CRIT-1's
  open-gap (a) "no general-descent emitter exists → 7A cannot produce a self-reparsing S0" is
  disk-false (Fact 1); there IS an artifact to fixpoint over. Held near the floor because the
  distinguishing gaps below are real and both critics agree them).
- **What is sound.** Staging shape correct (rustc `S1≡S2`, not `S2≡S0`); blake3 provenance ledger
  coherent; `regen.rs:37 check_target` re-point for the staleness rung is trivially shippable.
- **The corrections that hold it low.**
  - **Determinism-GREEN is a PHANTOM (HARD, both critics).** §0/gate-(b)'s "3× byte-identical …
    determinism precondition HOLDS" rests on the RequestFacts run, which stamps a **fixed const**
    (CSS/JSON template) for *any* input. Emitting a hard-coded constant thrice is trivially
    identical — it certifies nothing about a bbnf builder fixpoint. **7A has no non-vacuous
    determinism evidence.** (Corrects the prior REGISTRY's "determinism GREEN".)
  - **The fixpoint's SUBJECT is unchosen and now sharper.** Over generated `.rs` bytes it proves
    only determinism (vacuous, per above); over GrammarIr equivalence it needs the builder-augment
    (Fact 2/4). 7A cannot mint a self-reparsing *builder* S0 yet — the recognizer S0 is producible
    via `general_descent`, the builder S0 rides OPEN-1'.
  - **Redundancy (crowding).** 7A-default (frozen seed) keeps the hand parser as production forever →
    self-**VERIFYING**, not self-hosting (⊆ 7D + a fixpoint gate). The seed-disposition posture
    (frozen vs retire) is moot under the 7D collapse and stays **banked**. 7A's ONE non-redundant
    contribution is the **fixpoint LAW** (byte-identical `regen∘regen`, no 2-cycle) + provenance
    ledger — grafted onto 7D as OPEN-2.
- **Open gaps.** (a) determinism evidence is vacuous (CSS phantom); real GrammarIr-fixpoint
  determinism unmeasured. (b) fixpoint subject (bytes vs GrammarIr) unchosen, empty until the
  builder-augment (OPEN-1'). (c) seed_LOC headline metric un-anchored; a *building* S0 inflates
  toward the hand parser on PROTO-2's floor-2 (thesis-collapse risk). (d) the ~88s→~0.5s datum is a
  LEGACY-tree transplant; skinny xtask self-hosts nothing (0 matches for `regen-bbnf|fixpoint`). (e)
  the corrected-XL-4 gate (builder-augment **on** `general_descent`, not re-author of an absent
  machine) is absent from 7A's born-RED list. (f) Q2 twin-ladder (one `fixpoint` over bbnf+regex, or
  two) unresolved; regex leg gated on LOCK-5's two-tree fork (XL-2), not free.

### 7B — TOTAL-SELF-HOST — BANKED — 33%

- **Convergence 33%** (critics 30 / 35; lifted 3 off CRIT-1's floor — its "Plane A CASCADE, no
  descent emitter" rests on the disk-false Fact-1 premise — but held low: BANKED, Plane C/D
  unconverged, Plane A basis a phantom). **BANKED stands** per the crowding redirect.
- **Why BANKED (reinforced this pass).**
  - **Plane A — RE-CHARACTERIZED, not merely demoted.** The "998-LOC self-emit succeeds" basis is a
    **phantom** (CSS/JSON template, ignores the grammar; the producing probe said so). But — unlike a
    flat SPEC-ONLY demotion — the correct disposition is: `general_descent` (Fact 1) emits a
    **recognizer** for bbnf-shaped grammars **now**; Plane A's real status is *recognizer-demonstrable,
    builder-parity gated on the projection augment + floor-2*. The genuine bbnf **builder** LOC is
    **unmeasured** (now measurable — route bbnf through `general_descent` and count). Plane A folds
    into the shared ladder builder-augment (OPEN-1'), which is why it does not distinguish 7B.
  - **Plane B (`ir.bbnf` → `ExprKind`/`BackendExpr`) is double-homed with LOCK-3 3C (XL-1).** PROTO-2
    did LOCK-3 3C's feasibility work under a LOCK-7 harness (node-values are `->`-expressible = 3C
    GRAMMAR-HOMOIOUSIA verbatim). Conditional on LOCK-3 picking 3C; if it picks 3A/3B/3D, `ir.bbnf`
    has no IR-spine consumer (Y.13). Its gate (byte-identical to a frozen snapshot) yields nothing a
    drift-gate over the enums doesn't already give. → redirect to LOCK-3 adjudication.
  - **Plane C splits at the phase boundary (re-diagnosed).** Neither cleanly falsified nor cleanly
    blocked: node-**building** is `->`-expressible (PROTO-2 steps 1-3, a non-interned post-order
    TREE), but cross-rule **linking** (`resolve_refs`) is host-bound (Hh, irreducible). BLOCKED-
    candidate for the *linking* edge; open for the *node-value* edge.
  - **Plane D (emitter-template grammar) is UN-INSTRUMENTED — the sharpest un-run experiment.** No
    probe measured its data-vs-escape ratio (PROTO-2 measured the construction census; PROTO-1 the
    wrong emit path). Its "decided by Proto-2" is an unmet promise. INC-3 (codegen-as-trusted-
    primitive ⊥ codegen-as-grammar-plane) is UNDISSOLVED; the load-bearing "control-flow / dispatch /
    tape-assembly expressible without a borrow checker" is the unconverged hard part (god-template
    trap). Note: `general_descent` is *hand-Rust that walks the grammar*, evidence FOR INC-3's 7A
    pole (trusted primitive made flesh), against Plane D's maximal claim.
- **Salvage.** 7B's honest residue is two questions homed elsewhere: the SCOPE axis (GESTALT vs
  LOCK-3) and the Plane-D pure-structure-scoping (a research question, not a route). If LOCK-3 picks
  3C, Plane B may un-bank into a joint LOCK-3⊗LOCK-7 family (owed a named supersession).
- **Open gaps.** (a) Plane A builder LOC unmeasured (measurable now). (b) budget bound NOT
  discharged (the phantom). (c) Plane D zero data / INC-3 un-instrumented. (d) Plane B LOCK-3-
  conditional / consumer-less. (e) Plane C linking host-bound (floor-1). (f) diagnostics parity
  (generated spans as rich as 2142 LOC of hand machinery?) unmeasured — rustc's deliberate
  hand-parser retention is standing evidence against.

### 7C — HOST-FUNCTION-LATTICE — ADVANCED — 55%

- **Convergence 55%** (critics 55 / 60; take the conservative CRIT-1 floor — its floor-softness
  (G-7C-1) and zero-prototype (G-7C-2) points are sound and I cannot cleanly name an error).
  Best-substantiated in-tree family; the survivor.
- **What is sound (all disk-verified).** String heuristic `passes/src/lib.rs:~798`
  (`name.contains("host") && (…"decode"|"parse")`) present; `@host` typed directive `bbnf.bbnf:73`
  dormant (usage=0); `@{}` `bbnf.bbnf:33` → `BBNF-UNSUPPORTED-HOST-CAPTURE` `grammar/src/lib.rs:222`;
  six-variant `FnDescriptor`; `json/host.rs` empty (JSON proves the primitive floor). PROTO-2 hands
  it its **first concrete CONTEXT/TreeFold host fn** — `@host resolve_refs : (&mut GrammarIr) ->
  Result<(),ValidationError>` — the "typed, counted, floored" home the family promises, measured.
  T2 (delete the heuristic → check-json bit-for-bit) and T4 (productionize `@{}`) are genuine
  in-tree wins needing **no generated parser and no other lock**.
- **The corrections that hold it at 55%.**
  - **The floor is a DEPENDENT number, not fixed (G-7C-1).** `resolve_refs` HARD-vs-`add_expr` SOFT
    is under-justified: name resolution is inherited-attribute-expressible exactly as the arena is
    list-primitive-expressible, and augmenting BBNF is the standing law. True floor ∈ {0,1,2} pending
    a LOCK-1 inherited-attribute verdict. `count(HOST)=2` is also partly definitional (follows the
    dispatch's imposed "arena mutation MUST classify HOST" invariant).
  - **NOT unconditionally orthogonal.** Orthogonal for **T1/T2/T3/T5**; coupled at **T4** (shares the
    `@{}` parse+lower wall — the same wall the ladder needs) and **T6** (`@host` survives `regen∘regen`
    on bbnf.bbnf → rides the bbnf front-end augment). The **census / totality% deliverable — the
    metric that DEFINES the family — needs bbnf to parse+build**, so the family's headline inherits
    the builder-augment (OPEN-1'). The "needs no self-emit" claim holds for the *gates*, not the
    *deliverable*.
  - **The typed-lattice half is LOCK-1-coupled (XL-3).** T1 (typed signature into inference) and T5
    (tier off `&mut Ctx`, zero-cost re-verify) require LOCK-1's solver to admit a typed leaf. Split:
    lock-free = {kill `:798`, lower `:222`, name `resolve_refs`}; LOCK-1-bound = the rest.
  - **CSS floor presently UNMET** — `@{}` is `BBNF-UNSUPPORTED` yet `color.bbnf:220`/`values.bbnf:69`
    depend on it; CSS totality% is a paper number until T4. *Nuance (D5):* the CSS scan-*alphabet* is
    now grammar-derived (`derive_alphabet`), so CSS is closer to totality than the prior round
    assumed — but the `@{}` function-value gap is orthogonal and open.
  - **ZERO prototype exercised 7C.** For the cheapest-to-probe family (delete one heuristic, run
    check-json), not one gate ran. "Most viable / one-file cure" is SPEC_ONLY — the highest-ROI
    missing probe in the lock.
- **Open gaps.** (a) floor is dependent/soft (LOCK-1 inherited-attribute verdict). (b) zero gate
  executed. (c) census/percentage deliverable rides the builder-augment. (d) purity axis LOCK-1-
  coupled. (e) CONTEXT-tier zero-cost invariant untested (resolve_refs proposed, not round-tripped;
  TOML `push_path` unbuilt). (f) totality% counter + floor-gate unbuilt; planted-breach
  falsification undone.

### 7D — META-CIRCULAR-ORACLE — ADVANCED — 53%

- **Convergence 53%** (critics 48 / 56; **lifted off CRIT-1's 48 floor with a named reason** — its
  48 inherited the emitter-absent premise (its G-7D-1 "the SAME missing emitter, PROTO-1 CASCADE"),
  which is disk-false: the "gen" side is *producible now* via `general_descent`. Held below CRIT-2's
  informed 56 for the never-fires-promotion + tree-oracle-still-gated gaps both agree on).
- **What is sound.** The lawful entry ramp — an emitted parser's first consumer is the differential
  oracle, curing the Y.13 dormant-substrate violation the instant a parser exists. Metric ladder
  L0→L4, golden-anchor-not-majority-vote, planted-mutation falsification: well-formed. Matches the
  field's strongest precedent (CPython pegen ran old ∥ new for a full cycle). The **bbnf "gen" side
  is buildable NOW at recognizer/span granularity** via `general_descent` (Fact 1) — a real uplift
  the doc under-credits. PROTO-2-7D ran the one measured edge: paired cold regex oracle, structural
  parity **0.76** (0.80 under `Empty ≡ Concat([])`), membership 0.82, perf GEN/HAND **1.77–1.81×
  slower**. Neither promotion gate fires → **ORACLE-ONLY** on that edge.
- **The corrections that hold it at 53%.**
  - **Tree-oracle gated on the builder-augment.** The recognizer oracle (L0/L1) is buildable now;
    L2 NormalAst equality — the promotion floor — needs the builder (OPEN-1'). Until then the bbnf
    oracle is below its own stated promotion floor.
  - **The regex parity headline is soft and RED.** 9/12 divergences are ONE `host::lower` bug class
    (bounded `{n,m}` reject) — parity 0.76 measures **LOCK-5's incomplete fold (XL-2)**, not an
    oracle verdict. The stable verdict is perf (1.8× slower). Report "regex ships now *as a red
    ledger row*"; the parity-moving fix lives entirely in LOCK-5's `host.rs`.
  - **"7D subsumes 7A" is vacuous** — on the one decidable edge (regex) neither promotion gate fires,
    so the fixpoint corollary never activates (resolved by OPEN-2: decouple fixpoint from promotion).
  - **≡ needs `Empty`-normalization codified** and the `Hand = parse+resolve_refs+validate` vs
    `Gen = pure parse` pivot must run shared semantic passes on both or neither (stated, not built).
  - **§0 fact-4 falsified-uncorrected** (Fact 5) across two prior artifacts and the refresh.
  - **Canonical regex `.bbnf` is LOCK-5-blocked deeper than named** — gated on LOCK-5's two-tree
    substrate fork (skinny opaque-regex-leaf vs top-level interned), not merely a pick (XL-2/XL-7).
- **Open gaps.** (a) L2+ oracle strength on OPEN-1'. (b) regex ledger red (parity 0.76/0.80, perf
  0.55×); promotion may never fire (honestly named). (c) ≡ Empty-normalization uncodified. (d)
  canonical regex `.bbnf` on LOCK-5's substrate fork. (e) §0 fact-4 uncorrected. (f) grammar-driven
  corpus sampler unbuilt — parity over the committed 51 only.

---

## THE INCOMPATIBILITY MAP — status after pass 1

- **INC-1 · 7A big-bang fixpoint ⊥ 7D incremental oracle.** SOUND; tilts 7D-trajectory with 7A's
  fixpoint as a determinism precondition — BUT the precondition is only **phantom-GREEN** (7A gate-b
  is a CSS const), so the composition currently has no real floor. Accepted with that correction.
- **INC-2 · 7B build-in-grammar ⊥ 7C build-behind-host.** **RESOLVED-IN-DIRECTION toward 7C** by
  PROTO-2's census (7C-bound, floor 2). Record as *resolved-in-direction, floor ∈ {0,1,2} pending a
  LOCK-1 inherited-attribute verdict + a value-language arena primitive* — not "open," not "fixed 1."
- **INC-3 · 7A codegen-as-trusted-primitive ⊥ 7B Plane D codegen-as-grammar-plane.** **UN-
  INSTRUMENTED** — no probe measured the data-vs-escape ratio. `general_descent` (hand-Rust walking
  the grammar) is disk evidence FOR the 7A pole. The architectural fork pass-2 must decide; the
  sharpest un-run experiment in the lock.
- **INC-4 · hand-parser fate.** Composable (retire production / keep CI oracle). Corrected: 7C is
  NOT orthogonal for T4 (shares `@{}` wall) / T6 (bbnf-floor rides the builder-augment); orthogonal
  for T1/T2/T3/T5. The 7C stratum-2↔3 typed seam is the surviving ladder's kernel-carve-out boundary
  (XP-3).

**Lawful gestalt shape (hardened):** **7C ships regardless** (orthogonal mechanism + the totality%
metric every ladder reports against) **plus 7D as the single ladder posture** (oracle-first,
ORACLE-ONLY lawful terminal, "gen" side buildable now at recognizer granularity), **with 7A's
fixpoint law grafted onto 7D** (OPEN-2), **7B banked** (its scope questions homed in LOCK-3). The
whole ladder rests on ONE disk-located gap: the **builder-augment on `general_descent`** (OPEN-1').

---

## CROSS-LOCK LEDGER (carried, owed to GESTALT)

- **XL-4 · RETRACTED and INVERTED (the single largest correction this pass).** The prior "GN-0
  archives the feasibility evidence / re-author a machine skinny never had, the LARGEST GAP" is
  **false on disk**: `general_descent` (1206 LOC, wired `regen-sheets`) is a live, in-skinny
  feasibility proof for a non-trivial operator-tower grammar. The 21,557-LOC legacy `bbnf.rs` is not
  the only proof. The corrected obligation is **strictly smaller** — route bbnf through
  `general_descent` (no `value` rule, like Sheets), then close (a) builder-projection lowering, (b)
  the coarse-4-variant regex classifier (mod.rs:17), (c) closures; PROTO-2's floor-2 is the host
  budget. A *born-RED augment gate on an extant machine*, not a re-author. Replaces the old XL-4 in
  the ledger. The GN-0 archival concern narrows to: does the corrected augment gate get carried, and
  is the legacy fixpoint-gate a RESCUE or a re-author (still owed a born-RED cost).
- **XL-1 · `ir.bbnf` double-homed 7B-Plane-B ⊗ LOCK-3 3C.** PROTO-2 performed LOCK-3 3C's
  feasibility work under a LOCK-7 harness → credit PROTO-2 as cross-lock (LOCK-3⊗LOCK-7) evidence.
  Conditional on an un-named LOCK-3 verdict. Owed: a joint family or a named supersession.
- **XL-2 · regex oracle double-homed 7D ⊗ LOCK-5.** The 12-row ledger IS LOCK-5's `host.rs` bug list
  (9× bounded `{n,m}`, `.` metachar, `(?s)`); parity 0.76 measures LOCK-5's incomplete fold. Deeper
  than named: gated on LOCK-5's **two-tree substrate fork** (skinny opaque-regex-leaf vs top-level
  interned). Adjudicate: bootstrap-verification (LOCK-7) vs functor-round-trip (LOCK-5)?
- **XL-3 · 7C typed leaf ⊗ LOCK-1.** T1/T5 are LOCK-1 consumers; only {kill `:798`, lower `:222`,
  name `resolve_refs`} are lock-free. Also: the 7C floor itself (Fact 4) is a LOCK-1-dependent
  number.
- **XL-5 · LOCK-9 9C SCANNER-AS-DFA consumes the bootstrapped regex** — measured 1.8× slower /
  9-of-51 incomplete. A consumer edge that should be born-RED in the omega DAG.
- **XL-6 · 7A/7B/7D are net-new-vs-GN tranches** (the GN codex has no bootstrap/fixpoint wave);
  lawful only if declared as named additions. 7C re-cuts GN-5 (well; PROTO-2's `resolve_refs` is a
  concrete GN-5.W3 extension). **New sub-edge:** FAMILY-SPECS §0 lists "sheets" as an existing xtask
  target while asserting "no general emitter" — the Sheets target IS the general-emitter
  self-refutation inside the doc's own fact list. Owed a declaration.
- **XL-7 · one-home breach, cross-repo (clause ii).** Two divergent regex grammars
  (`grammar/misc/regex.bbnf` here; `parse-that/.../bootstrap/regex.bbnf` cross-repo). Downstream of
  LOCK-5's substrate fork (XL-2); not a free re-home.

---

## PASS-2 DIRECTIVES

### What research must answer (per family)

- **7A.** (R-7A-1) Re-derive the seed_LOC headline on the *skinny* tree and state the trade vs a
  total self-host — the family's reason to exist is un-numbered; note a *building* S0 inflates toward
  the hand parser on floor-2. (R-7A-2) Choose the fixpoint SUBJECT: generated `.rs` bytes
  (determinism-only, and note it is GREEN even for the `general_descent` path) vs GrammarIr
  equivalence (rides the builder-augment). (R-7A-3) Name the born-RED **builder-augment-on-
  `general_descent`** gate (corrected XL-4) and cost the legacy fixpoint-gate rescue-vs-re-author.
- **7B (BANKED — research only to test un-banking).** (R-7B-1) Resolve XL-1 with LOCK-3: does LOCK-3
  pick 3C? If yes, draft the joint LOCK-3⊗LOCK-7 Plane-B family; if no, 7B stays banked. (R-7B-2)
  The Plane-D pure-structure-scoping question — can control-flow / dispatch / tape-assembly be
  expressed without a borrow checker, with a *named* kernel-escape failure signature? A research
  answer, not a route, unless it converges.
- **7C.** (R-7C-1) Split the independence claim: {kill `:798`, lower `:222`, name `resolve_refs`}
  lock-free vs T1/T5 LOCK-1 consumers (XL-3); and mark the census/% deliverable as riding the
  builder-augment. (R-7C-2) Specify the totality% counter (leaves × branches) and the floor-gate as
  a GN-5.W3 `check-name-gates` sibling, with the planted-breach falsification. (R-7C-3) Confirm the
  CONTEXT-tier zero-cost invariant is testable via TOML `push_path` before T3 is scheduled.
- **7D.** (R-7D-1) Codify ≡ with `Empty ≡ Concat([])` and the shared-semantic-pass pivot
  (`resolve_refs`/`validate` on both or neither) — pin the equivalence-relation-id before any
  parity% is quoted. (R-7D-2) Consume LOCK-5 for the canonical regex `.bbnf` (XL-2/XL-7); name the
  dependency on LOCK-5's substrate fork. (R-7D-3) Draft the promotion ledger schema with a totality%
  column from 7C's counter (XP-1). Correct §0 fact-4 (producer, not recognizer).

### What to prototype (pass 2), in priority order

1. **OPEN-1' — the BUILDER-AUGMENT on `general_descent` (highest value, the genuine make-or-break,
   RE-CUT).** Land the three bounded parser fixes (Fact 3: `?w`→`->` attachment in `parse_seq:1366`;
   `@{}` atom in `parse_atom:1495`; `|params|` closures) so bbnf.bbnf routes into `general_descent`;
   then author `-> Span`/`-> T` **builder** projections on bbnf's structural rules over the extant
   1206-LOC recognizer emitter, and report: does the emitted parser reparse the corpus into
   GrammarIr-equivalent structure (with PROTO-2's +1 SOFT arena / +1 HARD `resolve_refs` host
   budget), and does `regen∘regen` hold byte-identical — or does the augment cascade downstream, and
   at which construct? **PROTO-1 measured the WRONG path (`json_sink_direct`); this is untested.**
   Binary outcome decides 7A build-meaning, 7B Plane A parity, 7C census%, and 7D L2 oracle strength
   jointly. Do NOT drive `emit_from_source`→`json_sink_direct`; drive the `!has_rule("value")` branch.
2. **INC-3 / Plane-D data-vs-escape ratio (the sharpest UN-INSTRUMENTED experiment).** Convert ONE
   emitter blob (JSON `skeleton.rs` or a CSS template) toward grammar/`quote!`-structured data;
   measure the fraction expressible as data vs escaped to host fns. ≥50% → Plane D viable; <50% →
   the maximal claim is refuted and 7C is the honest form. No probe has touched this; it decides the
   codegen-primitive-vs-plane fork (INC-3). `general_descent` is the reference for the 7A pole.
3. **7C-T2 (trivial, kills the SPEC_ONLY gap; cheapest signal in the lock).** Delete
   `has_parse_time_host_decode` (`passes/src/lib.rs:~798`) → typed-tier registry lookup; run
   `cargo xtask check-json` bit-for-bit. Makes 7C's "one-file cure" a RAN result.
4. **7C-T4 = base-case fix (ii), cross-pollinated.** Parse `@{}` in `parse_atom` + lower
   `RuntimeConstructKind::HostCapture`; CSS `colorFunction`/`urlFunction` stop emitting
   `BBNF-UNSUPPORTED`; CSS regen bit-for-bit + measured totality% delta. Discharges the CSS floor and
   delivers wall #2 for the ladder in one stroke.
5. **7D regex ledger, corrected.** Re-run PROTO-2-7D with ≡ Empty-normalized (parity 0.80/40),
   record it as the first COMPLETE ledger row, and file the 3 substantive divergences (bounded
   `{n,m}`, `.` metachar, `(?s)`) as LOCK-5 `host.rs` obligations (XL-2), not LOCK-7 bugs.

### Cross-pollination (named, both families developed)

- **XP-1 · 7C totality% counter → 7D promotion ledger.** 7C (55%) supplies the metric; add it as a
  column to 7D's (53%) ledger. Justified: 7C's % is the metric every ladder reports against.
- **XP-2 · 7A fixpoint law → 7D oracle (= OPEN-2).** 7A (42%) + 7D (53%); the graft is "fixpoint over
  the oracle artifact," decoupled from promotion (needs neither perf≥1.0 nor parity=1.0 — only that
  the generated artifact re-emits itself byte-identically). Keeps 7A's non-redundant core alive;
  gives 7D a live fixpoint gate its "subsumes 7A" claim currently lacks. Reuse `regen.rs:37`.
- **XP-3 · 7C stratum-2↔3 typed seam = the surviving ladder's kernel-carve-out boundary.** 7B's
  "kernels behind a typed call interface" IS 7C's seam; 7D (the survivor) consumes it as its
  primitive line. Justified by INC-4.
- **XP-4 (NEW) · PROTO-2's `resolve_refs` host fn = the ladder's builder-augment HARD floor.**
  `@host resolve_refs : (&mut GrammarIr) -> Result<(),ValidationError>` is BOTH 7C's first concrete
  CONTEXT/TreeFold instance AND the irreducible host the OPEN-1' builder-augment (7A/7D bbnf leg)
  must house. So 7C's floor census IS the ladder's builder-augment host budget — the two are one
  measurement. Justified: PROTO-2 is explicitly the 7C↔7B-Plane-A bridge probe.

### What stays WITHHELD from researchers (pass 2)

- **The REGISTRY convergence %s and family statuses** (standard protocol — no family allegiance).
- **The 7B-BANK verdict and the crowding redirect.** 7B-owned research is dispatched as open
  scope-axis questions (R-7B-1/2), NOT told the family is banked — else it cannot honestly test the
  un-banking condition (LOCK-3 picking 3C).
- **The pre-committed answer to OPEN-1'.** The builder-augment prototype must run genuinely
  RED-unknown; do NOT seed it with "the legacy 21,557-LOC `bbnf.rs` proves it lowered" (that was the
  OLD emitter) NOR with "`general_descent` exists so it's bounded" (it emits *recognizers*; the
  *builder* rider is the open question). The disk fact to carry is "the emitter EXISTS and emits
  recognizers"; the verdict on the builder-augment stays unseeded.
- **PROTO-1's byte-421 / CASCADE-absent-emitter numbers.** Superseded and non-reproducing (stale
  worktree + wrong emit path). Pass 2 carries only the corrected Facts 1–3: `general_descent` exists
  (1206 LOC), `parse_postfix:1409` handles `->`, byte-662 + `@{}` + closures are the three parse
  fixes. Never re-quote 421 or "no general emitter."

---

## PER-PASS HISTORY

- **Pass 1 (2026-07-16, this file).** Folded the definitive probe generation (`PROTO-1.md` CASCADE,
  `PROTO-2.md` census floor-2) + CRIT-1 (within-lock) + CRIT-2 (cross-lock), with agglomerator disk
  re-verification at HEAD `fe160a91a`. **Headline correction:** `general_descent` (1206 LOC,
  `regen-sheets`) EXISTS → the old XL-4 ("re-author an absent machine") RETRACTED and inverted to a
  bounded builder-augment gate; the make-or-break RE-CUT to OPEN-1'. Three-way contradiction
  (emitter / floor / parse-wall) adjudicated on disk (CON-1/2/3). Standing verdict: 7A ADVANCED
  (narrowed) 42% · 7B BANKED 33% · 7C ADVANCED 55% · 7D ADVANCED 53%. Superseded the prior REGISTRY
  draft that folded the first probe generation (998-LOC self-emit; 51-pattern regex oracle).
