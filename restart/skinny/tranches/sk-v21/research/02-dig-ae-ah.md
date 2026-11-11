# Dig shard 02 — Tranches AE, AF, AG, AH

An excavation of four consecutive tranches from the original bbnf-lang campaign
(the era that ended in total divergence and forced the restart). AE through AH
span the "tape-first substrate completion" and "three-tier emission" arc. They
are, in miniature, the whole failure mode of the pre-restart campaign: a
substrate-break deferred at AE that metastasized across ~20 subsequent tranches,
an optimization edifice (AF) erected on that unclosed substrate, a payoff
(Tier B direct-to-struct emission) deferred three times and never fired on a
single production grammar, and an audit tranche (AH) that ran zero audits.

All four tranches landed on a single calendar day — **2026-04-11**
(`git log --date=short`: every AE/AF/AG commit is dated 2026-04-11). AH produced
zero commits. This velocity is itself the primary process tell: four tranches,
one of them (AF) with seven declared phases, declared complete in one day with
no green gate between them.

---

## 1. What each tranche attempted (charter and waves)

### AE — "Complete the Tape-First Migration"

Source: `docs/tranches/AE/AE.md`, `docs/tranches/AE/tape-shapes.md`.

Context (`AE.md:1-63`): AC.2 had transposed the Rust backend to tape-first
parsing (every rule returns `Option<TapeOffset>`), but the lowering pipeline that
bridges CST tape views back into `GrammarIR` was only *API*-migrated — it called
tape-first accessors (`view.rule_kind()`, `view.child(n)`) while its *semantics*
still assumed the pre-AC.2 flat shape where an optimizer had pre-inlined
transparent wrappers. Under the structural bootstrap path (optimizer gated out),
that gap "collapsed every rule body to `IrNode::Epsilon`. The lowering walked,
the lowering succeeded, the lowering produced empty IR. No panic, no diagnostic"
(`AE.md:19-22`).

Declared deliverables (`AE.md:261-307`, "Architectural commitments"):
1. Shape-agnostic lowering — dispatch by `rule_kind`, never child index; four
   shape primitives in `tape_walk.rs` (`iter_rep_children`, `find_child_by_kind`,
   `peel_transparent`, `iter_pairs_rep`).
2. One directive API — delete the walking `as_*_directive()` accessors and four
   orphan emit-stub modules; keep only `cst_directives::try_as_*_directive`.
3. Identity-aware rule emission — `preserve_identity` rules always emit standalone
   functions regardless of `CallStrategy`; revert the structural-mode strategy clamp.
4. **Clean regen** — `scripts/bootstrap-bbnf.sh` produces a `generated.rs` with no
   hand patches; a permanent round-trip gate (`grammar_roundtrip.rs`) parses every
   production grammar and asserts frozen rule counts.
5. Panic, never Epsilon — unknown `rule_kind` is a programmer error.
6. No legacy code — every debugging workaround removed or formalized.

### AF — "Substrate Closure + Three-Tier Emission"

Source: `docs/tranches/AF/AF.md`, design prototype `docs/tranches/AF/research/prototype.md`.

Eight phases (`AF.md:53-357`):
- **AF.0 / AF.0b** — close AE's substrate-break: replace every hard-coded
  sub-variant reference (`term_0`, `value_atom_0`, `import_directive_0`, …) with
  span-text content dispatch; run the clean regen; ship the round-trip gate;
  delete the hand patches.
- **AF.1** — activate dormant infrastructure (`EClassFacts::is_fixed_shape`,
  dispatch FIRST-set caching, `compute_context_facts` fusion, `TypeDescId` routing).
- **AF.2** — universal cost model: one `CostWeights` struct for every consumer.
- **AF.3** — cross-rule CSP via the Y.5 `UnionFind` substrate (dormant since Y.5);
  per-component solve with `EnginePropagation` / `ParentCompatibility` /
  `TierFollowsMaterialization` constraints.
- **AF.4** — three-tier `EmissionTier` lattice (`Tape` / `Direct` / `LazyView`) +
  `GrammarIR::emission_tier` sidecar.
- **AF.5** — `decode_emission_tier` decision pass, sole writer of the sidecar.
- **AF.6** — the Tier B emitter: leaf rules with typed `->` bodies emit a second
  `__rule_direct(state) -> Option<T>` skipping the tape; the view-layer `DirectSlot`.
  **This is the entire payoff of AF.**
- **AF.7** — default-on + post-AF samply baseline with floor gates.

### AG — "Substrate Closure" (retroactive: "closed the AF circle")

Source: `docs/tranches/AG/AG.md`.

- **AG.0** — build/test parallelization: proc-macro cache-key tier separation
  (`BBNF_SCHEMA_VERSION` owns invalidation), 33 → 4 test binaries, dev-profile
  overrides, bisect script. Target: 2–4 min → under 30 s (`AG.md:6-28`).
- **AG.1** — FastPath regression: classifier/emitter opts divergence.
- **AG.4 / AG.4b** — three structural-mode lowering bugs + a surgical hand-patch
  injected into `generated.rs` for multi-hint `@pretty`.
- **AG.5** — CSP tier-variable activation (per-rule `EmissionTier` in the solver).
- **AG.2 / AG.3** — Tier B emitter + view-layer `DirectSlot`.

### AH — "System Audit"

Source: `docs/tranches/AH/AH.md`. Five planned audits (`AH.md:6-48`):
AH.0 materialization inventory, AH.1 CSP interaction, AH.2 e-graph/cost
consistency, AH.3 recognizer/kernel/pattern, AH.4 end-to-end cost coherence.

---

## 2. What shipped vs. what died on the vine

Cross-checked against `git log --oneline --all` on `docs/tranches/{AE,AF,AG,AH}`
and the touched source paths.

### AE — the substrate-break was deferred at birth

**Shipped** (all 2026-04-11): the tape-shape reference doc (`cb03adf65`), the
shape-agnostic lowering rewrite (`854782847` — explicitly titled **"wip"**,
`c2c9bdf3b`, `49a39b408`, `9ebc601bc`), deletion of the walking directive
accessors and dead stub modules (`db0e9ddcf`, `9e1c84997`), the cycle guard +
span-text directive fallback (`aca9cd4e4`), a relaxed directive layout check
(`2e7cd17d8`), and slab→tape-first test/bench migration (`5af1aed40`,
`8fcf13d43`, `3ab2bae37`, `905661645`).

**Died on the vine** — the clean regen. AE.md's own "The clean regen —
substrate-break deferred" section (`AE.md:197-258`) concedes the regen output is
"structurally incomplete" and that `generated.rs` "remains the HEAD-era
hand-patched file" carrying "two minimal enum-mapping patches (`value_expr` and
`rhs` variants)". The founding commit's own trailer (`git show 854782847`) reads:
"**WIP: AE.1's lowering rewrite still requires Agent A to audit
`lower/value_expr.rs` for the same class of positional-walk bugs.** … bootstrap
regen iteration is gated on AE.2/AE.3 + the orchestrator's AE.4 step."

So AE never closed. The "permanent regression gate" `grammar_roundtrip.rs` was
declared but explicitly conditional: "The test ships once the regen produces a
generated.rs that supports it" (`AE.md:256-258`) — i.e., not in AE.

### AF — substrate + solver + decode shipped; the emitter (the payoff) did not

**Shipped** (all 2026-04-11): AF.0 span-text dispatch (`1db2a2ca7`, `c1c975014`,
`21adcacff`, `6c0e6344b`), the grep gate `no_subvariant_refs.rs` (`2102c78c7`)
and round-trip snapshot gate (`131a146f7`), AF.1 dormant-infra activation
(`0802ef6e8`, `4c63f1432`, `39f4f015e`, `b192ecca9`, `29cfb38ce`), AF.2 universal
`CostWeights` (`cd6ec2a42`, `0221c5fed`, `74d75b9e7`), AF.3 per-component CSP +
three cross-rule constraints (`f76661784`, `0d3485564`, `081964108`, `a6e3fd237`,
`c68a41241`), AF.4 `EmissionTier` lattice + sidecar (`1e568faac`), AF.5
`decode_emission_tier` (`e9a109fbb`).

**Died on the vine:**
- **AF.0b — the clean regen, deferred again.** `AF.md:98-130`: "The clean regen
  itself … is deferred to a follow-up sub-tranche **AF.0b**. A latent bug in the
  backend driver codegen emits a degenerate `__grammar` body (three empty
  `'alt_blk:` branches instead of the expected `'rpt_blk:` loop) … The bug
  reproduces with every Wave 1 change reverted, so it pre-dates the tranche; the
  hand-patched `generated.rs` has been silently masking it." The round-trip gate
  is `#[ignore]`-gated (`AF.md:117-118`).
- **AF.6 — the Tier B emitter, deferred.** `AF.md:323-339`: "Status: deferred to
  AF.6 sub-tranche. … **Until AF.6 lands, `ir.emission_tier` is populated but
  unread by any emitter. Every rule still emits the AE tape-only path.**"
- **AF.7 — the post-AF samply baseline never ran** (no bench commit; gated on AF.6).

AF therefore shipped substrate (lattice, sidecar, cost weights), solver (CSP
components, constraints), and decode pass — but zero consumer. The optimization
was fully wired and optimized nothing.

### AG — one genuine win; the payoff deferred a third time and proven inert

**Shipped:** AG.0 build/test parallelization (`e352ed8d1`) — the single
self-contained, durable win; AG.1 FastPath fix (`41ae539c0`); AG.4 clean-regen
lowering + multi-hint `@pretty` (`f53a68e1f`); AG.4b span-text mapping recovery
(`8db7adf47`); AG.5 CSP tier-variable activation (`1c66b9322`, `91bdd393f`).

**Died on the vine — and this is the decisive admission** (`AG.md:97-103`):
"**AG.2, AG.3 — Deferred. Tier B emitter and view-layer DirectSlot have zero live
consumers: `decode_emission_tier`'s Tier B eligibility check (TransparentElide +
pure-conversion leaf) never fires on any production grammar under the current
constraint set. Infrastructure is live; emitter activation deferred to
Tranche AI.**" The entire AF+AG tier apparatus optimized zero rules on any real
grammar.

AG.4b also re-deferred the clean regen a third time (`AG.md:70-76`): "The clean
regen produces a correct `__grammar` body … but cannot replace the hand-patched
file yet because `host::extract_grammar` reads the pre-inline tape shape.
Deferred to AG.4b." And AG.4 *re-committed the original sin* — it injected a
"surgical `trim_leading_whitespace_mut` injection" **directly into the
hand-patched `generated.rs`** (`AG.md:69-72`): hand-patching generated code, the
exact anti-pattern the whole substrate-break was meant to end.

### AH — zero audits executed

`AH.md:50-63`: "**No commits under AH; all deliverables land under AI.** …
Rather than executing the audits as standalone diagnostics, the findings informed
AI's implementation plan directly." Git confirms: no commits reference AH; the
directory holds only `AH.md`. The five audits were folded into AI's design phase
without ever being run. `docs/tranches/AI/AI.md` exists (210 lines); AI.1 is
"wire emitter" — i.e., the thrice-deferred AF.6.

---

## 3. Root causes — where and how divergence began

### R1. The substrate-break deferral metastasized across ~20 tranches (the master root cause)

The clean-regen deferral chain: **AE.4 → AF.0b → AG.4b → …**. The hand-patched
`crates/core/src/grammar/generated.rs` — introduced with "two minimal
enum-mapping patches" AE swore were "explicitly transitional and removed when the
substrate-break tranche lands" (`AE.md:299-307`) — persisted from **2026-04-11
(AE)** until it was finally structurally retired at **B2 on 2026-04-25**
(`git log`: `09e419a32 feat(b2): retire monolithic generated.rs; xtask emits
crates/core/src/grammar/generated/bbnf.rs`). Fourteen days, roughly twenty
tranches (AE→AF→AG→AH→AI→…→B2), every one built atop a file the first of them
promised to regenerate cleanly. A hand-patched generated parser *is* spec drift:
the grammar source and the compiled parser diverged, and every downstream tranche
read morphology facts off the divergence.

### R2. AF's founding premise was false — an edifice on an unverified substrate

AF's entire architecture rests on one claim, stated three times
(`AF.md:6-11`, `prototype.md:12-16`, `prototype.md:419-423`): "**AE makes
morphology facts trustworthy again.**" But AE never produced a clean regen, so
the "trustworthy" facts were still read off the hand-patched `generated.rs`. AF's
own AF.0b section (`AF.md:108-113`) admits a *pre-existing* codegen bug (the
degenerate `__grammar` body) that "the hand-patched `generated.rs` has been
silently masking." AF built a universal cost model, a per-component CSP, a
three-tier lattice, and a decode pass on a substrate whose central promise was
unmet — and put the only observable payoff (the emitter) in the deferred phase.

### R3. Overfitting the wrong end — machinery before a proven consumer

The Tier B eligibility gate (`AF.md:275-281`: `TransparentElide` + non-closure
`FnDescriptor` over a non-`Ref` inner, or a bare leaf) was so narrow that
`decode_emission_tier`'s check "**never fires on any production grammar**"
(`AG.md:99-102`). The team built the lattice, the CSP variable, the constraints
(`tier::install`, `engine::install`, `parent::install`), and the decode pass
before establishing that a single rule in JSON, CSS L4, or Google Sheets would
qualify. The correct order — prove the consumer benefits on a real grammar, then
build the solver — was inverted.

### R4. Silent-Epsilon lowering — the original correctness sink

The pre-AE lowering's catch-all returned `IrNode::Epsilon` on any unhandled
`rule_kind` (`AE.md:90-99`, `AE.md:335-343`), so a positional read landing on the
wrong node produced empty IR with "no panic, no diagnostic." Positional reads
(`child(1)`, `child(2)`) shifted by one or two whenever a preserved `Optional`
wrapper (`?w`, `"|"?`) sat where the pre-AC.2 optimizer would have elided it. This
is a genuine, decisive negative result (see §4).

### R5. Dual code paths diverging on config (AG.1)

`AG.md:36-45`: `solve_regex_strategy` probed via `emit_regex_direct_call(pattern)`
(hard-coded default `EmitOpts`) while `emit_regex` passed the caller's real opts
to `emit_regex_fast_path`. The opts divergence stayed latent until AF.3's
per-component CSP populated `ir.regex_engine_decisions` and surfaced as a panic at
`generate/regex/emit/mod.rs:38`. A classifier and an emitter that must agree but
read different config is a recurring structural bug class.

---

## 4. Worth resurrecting for the totality plan

### A. The structural-mode tape-shape reference (`docs/tranches/AE/tape-shapes.md`)

Hard-won, correct, load-bearing knowledge that any tape-walking lowering needs:
only `Repeat` / `Optional` / `Rule` push tape records; `Seq`, `Alt`, `Literal`,
`Regex`, `Epsilon`, and `?w` (OptionalWhitespace) push **nothing**
(`tape-shapes.md:9-32`). Rule compound children are therefore flat — a
`pair = key , ":" , value` produces `Rule(pair, [Rule(key), Rule(value)])` with
the literal and the `Seq` invisible (`tape-shapes.md:41-48`). Optional compounds
are *always* pushed (empty on miss), so presence is decided by span-emptiness, not
child count (`tape-shapes.md:78-83`). This table is a genuine artifact worth
carrying forward verbatim.

### B. The "panic, never Epsilon" precept (negative result — do not re-learn)

A closed grammar must panic on an unknown `rule_kind`; a silent Epsilon
fall-through corrupts every downstream rule body invisibly and cost the campaign a
whole tranche to diagnose (`AE.md:90-99`). Any lowering/dispatch catch-all in the
totality plan must be a panic, matching the standing memory precept
`no-silent-epsilon`.

### C. AG.0 build/test parallelization — the one durable win

`AG.md:6-28`: proc-macro cache-key tier separation (a manually-bumped
`BBNF_SCHEMA_VERSION` owns invalidation so unrelated refactors don't bust the
cache), 33 → 4 test binaries (~75% fewer rustc/linker invocations at the same 262
test count), dev-profile `opt-level = 1` for hot crates + `codegen-units = 256`.
This is the tranche that actually moved iteration time (2–4 min → warm 0.19 s lib
build). It aligns with the memory precept `build-infra-first`; resurrect it early.

### D. The shape-primitive discipline — with a caveat

The four primitives (`iter_rep_children`, `find_child_by_kind`, `peel_transparent`,
`iter_pairs_rep`) and the "one place owns the unwrap" rule (`AE.md:76-88`,
`tape-shapes.md:118-145`) are sound. **Caveat**: the *need* for shape-agnostic
wrapper-peeling is itself a symptom — it exists only because structural
(`preserve_identity`) mode preserves wrappers the optimizer would elide, so the
lowering must tolerate both shapes. A totality design should question whether it
wants two tape shapes at all, rather than perfecting the peeler that reconciles
them. (`tape_walk.rs` did ship at `854782847`, was renamed to `view_walk.rs` at
`dcb41e673`/`fb1f33557` in the AZ-II era, and survives today as
`crates/core/src/lower/view_walk.rs`.)

### E. AG.1 dual-path-divergence lesson

Wherever a classifier and an emitter must agree (regex tier selection, materialization
decisions), they must read the *same* config through the *same* predicate — not a
default-opts probe versus a real-opts emit. The AG.1 fix (`AG.md:42-45`) — collapse
the strategy enum, let `emit_regex` walk the tier ladder directly and keep
`solve_regex_strategy` as a *pure classifier running the same predicates* — is the
right shape.

### F. The three-tier design (resurrect the idea, discard the machinery)

Tape / Direct-to-struct / Lazy-view is a defensible axis (`prototype.md:52-89`),
and the direct-to-struct projection for typed `->` leaf bodies is exactly the kind
of specialization a SOTA parser generator wants. But the *implementation order*
was wrong (R3), the eligibility was too narrow to ever fire, and the whole thing
depended on the unclosed substrate. Carry the design intent; rebuild it consumer-first
on a substrate that is actually clean.

---

## 5. Meta-process pathologies

1. **Deferral-as-completion.** Tranches "landed" by relocating the hard part to a
   new sub-tranche letter (AF.0b, AG.4b) or the next tranche (AH → AI). Substrate
   ships; consumer defers; payoff never arrives. The clean regen was deferred at
   least three times (AE.4, AF.0b, AG.4b); the Tier B emitter three times
   (AF.6, AG.2/AG.3, AI.1). No mechanism forced a deferral to ever come due.

2. **Doc/code tense inversion.** AE.md narrates in completed past tense ("AE
   completes the tape-first migration", `AE.md:55`) while its founding commit is
   marked WIP; AF.md asserts "Tranche AE landed the shape-agnostic lowering
   substrate" (`AF.md:5-11`) while the substrate-break was open and the round-trip
   gate `#[ignore]`-d. The prose describes the intended end state, not the shipped
   state — so a later reader (or agent) treats a WIP as a foundation.

3. **Audit theater.** AH planned five diagnostic audits and executed zero,
   folding them into AI's design (`AH.md:50-63`). An audit that produces no
   measurement and no commit is a plan wearing an audit's clothes.

4. **Single-day multi-phase churn.** AE + AF (seven phases) + AG (five phases) + AH
   all dated 2026-04-11, with no cold benchmark and no green gate between them
   (AF.7's baseline never ran; the round-trip gate was born ignored). Tranches
   were declared, not verified.

5. **Gate erosion.** The `grammar_roundtrip.rs` "permanent regression gate"
   (`AE.md:250-258`) shipped `#[ignore]`-gated (`AF.md:117-118`), was still ignored
   at AI.6 (`AI.md`: "unignore … grammar_roundtrip ignores"), and was finally
   *deleted as a fossil* at AW-I.W2.5 (`git log`: `02574ce96 chore(tests): delete
   un-fused IR-shape snapshot fossils`). A gate that is ignored from birth is
   documentation, not a gate.

6. **Recommitting the original sin.** AG.4 hand-patched the generated parser
   (`trim_leading_whitespace_mut` injected into `generated.rs`, `AG.md:69-72`) —
   the precise divergence (hand-edited generated code) the substrate-break existed
   to end. This is the local minimum: patching the artifact was always cheaper in
   the moment than closing the substrate, so the substrate stayed open for two
   weeks. It corroborates the campaign-wide "undocumented / implicit deferral"
   pattern the later meta-audit records (`docs/tranches/meta-audit/03-tranche-drift.md:203-244`).

---

## 6. One-paragraph verdict for the totality plan

AE–AH is a self-similar fractal of the pre-restart divergence: a substrate-break
(clean regen) deferred at AE, an optimization edifice (AF) built on the unclosed
substrate under a false "facts are now trustworthy" premise, a payoff (Tier B)
deferred until it was proven to fire on zero grammars (AG), and an audit tranche
that audited nothing (AH). The generative disease is **deferral-as-completion with
no due date and no green gate** — the hand-patched `generated.rs` survived 14 days
and ~20 tranches because patching the artifact was always locally cheaper than
closing the substrate. The totality plan must invert this: never build a solver
before a consumer is proven to benefit on a real grammar; never let a tranche
"land" with its payoff phase deferred; never ship an `#[ignore]`-gated gate and
call it a gate; and never hand-patch generated code. Resurrect the tape-shape
reference (`tape-shapes.md`), the panic-never-Epsilon precept, the AG.0 build
infrastructure, and the AG.1 same-predicate discipline — and carry the three-tier
*design* while discarding its premature, inert machinery.
