# Archaeological Dig — Tranches AI, AJ, AK, AL

Excavation of the pre-restart campaign's emission-optimization arc. These four
tranches sit at the seam where the original bbnf-lang project peaked
technically (AJ/AK real substrate wins) and then diverged fatally (AL: four
competing plans, no reconciliation, tranche identity collapse). The arc is a
compact case study of every failure mode the restart was founded to escape.

## Corpus inventory

- `docs/tranches/AI/AI.md` (210 lines) — forward plan, "Automatic Optimal Emission."
- `docs/tranches/AJ/AJ.md` (75 lines) — post-hoc close-out with results table.
- `docs/tranches/AK/AK.md` (68 lines) — post-hoc close-out with results table.
- `docs/tranches/AL/research/prototype-1..4.md` (19 / 296 / 366 / 394 lines) —
  four competing, unreconciled plan drafts. **No `AL.md` exists.**
- Cross-check retrospectives (authored in tranche AW, two days-of-work later):
  `docs/tranches/AW/audit/AK-retro.md`, `docs/tranches/AW/audit/AL-retro.md`
  (git objects `772ee93d6`, `71d8e2de0`).

The tranche docs themselves carry a single git touch (`36945f60c`, a bulk
restructure into per-tranche directories). All substantive history lives in
the source-file commits, traced below.

---

## (a) What was attempted

### AI — "Automatic Optimal Emission" (three-tier emission lattice)

AI's thesis (AI.md:16-22): *every rule's emission strategy is the output of a
single cost-minimization problem where the CSP, e-graph, and materialization
classifier cooperate to find the maximally efficient emission form.* Concretely
a three-tier lattice — **Tape / Direct / Lazy** — layered on top of the existing
`MaterializationClass` axis. Six waves:

- AI.1 wire `ir.emission_tier` into the Rust emitter; Direct rules get a
  three-function triad (`__rule_inner`, tape wrapper, `__rule_direct` shim)
  (AI.md:30-63).
- AI.2 widen Tier-B (Direct) eligibility to typed Seq projections, single-branch
  Alt, Map-over-transparent-Ref, bounded Repeat (AI.md:65-92).
- AI.3 emission-aware e-graph cost model — an `emission_tier_bonus` rewarding
  Direct-enabling body shapes (AI.md:94-118).
- AI.4 cross-component tier reconciliation — `reconcile_cross_component_tiers`,
  a monotone widening pass over the call-graph DAG (AI.md:120-144).
- AI.5 view-layer typed accessors (five stub files → real per-kind codegen)
  (AI.md:146-171).
- AI.6 ghost cleanup + samply baseline + floor gates (AI.md:173-192).

Crucially, AI.md **opens by documenting the infrastructure as already dead**:
"the substrate has no consumer: the Rust emitter dispatches solely on
`MaterializationClass`, ignoring `ir.emission_tier` entirely. Zero production
grammar rules classify as Direct" (AI.md:8-13). AI's response to a dead
abstraction was to build *more* of it.

### AJ — Tape hot-path surgery + parse correctness

A post-AI performance audit found the tape-first architecture (introduced in
tranche AC) was *slower than the pre-tape BumpSlab era* due to five structural
hot-path defects (AJ.md:3-5). AJ fixed them:

- AJ.0 zero-alloc `TapeCursor::child(i)` — the old path delegated to
  `children().nth(i)`, allocating a full `Vec` of all K children per accessor
  call (AJ.md:9-20).
- AJ.1 leaf-like Alt promotion in the materialization classifier (AJ.md:22-38).
- AJ.2 zero-copy `Parsed<'p, R>` (was `input.to_owned()` per parse) + trailing-
  whitespace EOF fix (AJ.md:40-63).
- AJ.3 TapeBuilder adaptive capacity `len/8 → len/4` (AJ.md:61-63).

### AK — Materialization-driven Alt emission + flat tape

- AK.0 replace `ChunkedArena<TapeRec>` (`Vec<Vec<TapeRec>>`) with a flat
  `Vec<TapeRec>` — one allocation instead of ~100, +10-14% (AK.md:16-24).
- AK.1/AK.2 per-branch `__branch_idx: u8` variant discriminator threaded through
  Alt bodies; tape record's `variant_idx` now holds the branch index, not the
  rule's global ID (AK.md:25-44).

### AL — attempted four ways, reconciled zero ways

Four prototype agents each drafted a full AL.0..AL.7 plan with **incompatible
primary targets**:

- prototype-1 (19 lines): a live CSP/cost-model coherence audit. Primary target:
  the CSS-L4 CSP blowup and lost structure-sharing.
- prototype-2: "Three-Tier Activation + Global CSP" — delete EmissionTier,
  per-branch `mark_children`, global CSP.
- prototype-3: "Emission Unification…" — same deletion plus allocation surgery,
  e-graph→CSP feedback bridge, `TypeDesc::TaggedUnion` boxing elimination.
- prototype-4: "Maximal -O3 Optimization Pass" — the sonic-rs gap; structural
  SIMD bitmap pre-scan + branchless clmul string scanner.

They disagree on whether AL is a correctness pass, a CSP-cleanup pass, or a
SIMD-frontier pass (AL-retro.md:52-58). No merge agent ran; no `AL.md` landed.

---

## (b) What shipped vs what died on the vine

**AI — shipped in full, then demolished wholesale.** Every wave landed:
`9155adbe4` (AI.1 wire emitter, +228/-59 across 4 emitter files), `bc1f7c3bc`
(AI.2), `78e422639` (AI.3), `26b530578` (AI.4 reconciliation), `15f3bfbde`
(AI.5), `718396a56` (AI.6), plus fixups `e39849130`, `432300869`, and
`e04b52e42` ("value_expr dispatch + OOM mitigation"). Two tranches later, AM.1
(`76085303a`, "delete EmissionTier axis + BumpSlab residue") **deleted the
entire system: 48 files changed, 99 insertions, 2,306 deletions** — including
`emission_tier.rs` (190), `decode_tier.rs` (254), `constraints/tier.rs` (118),
`constraints/parent.rs` (256), and `emission_tier_lattice.rs` tests (534). The
commit's own note: "~2200 LOC net deletion; zero parse-time performance change."
The entire "Automatic Optimal Emission" tranche produced **no measured parse-time
value** and was net-negative LOC once pruned.

**AJ — shipped, real and durable.** `953adbbc0` (AJ.0), `f0afeae04` (AJ.1),
`48be14d7f` (AJ.2+AJ.3), baseline `65e046909`. These are genuine substrate
wins: canada.json went from **FAIL → 1,332 MB/s** (AJ.md:67) and citm +3.8%.

**AK — shipped, real.** `0fc6bebea` (AK.0 flat Vec), `9658bb2e8` (AK.1+AK.2),
`c62ad389c` (AK.3 baseline). Verified cumulative +24-52% since the pre-tape slab
era (AK.md:55-63). The AW retro confirms results match the table verbatim
(AK-retro.md:8-11).

**AL — effectively died on the vine.** Exactly one commit landed under the AL
label: `e6574a98e` "fix(serialize): rewrite codegen for tape-first architecture
(AL.1)" — a `serialize_*` → `NodeView`/tape-cursor migration (+167/-481, 22
tests pass), matching **none of the four prototype AL.1 definitions**
(AL-retro.md:9-16). Everything the four prototypes proposed deferred silently and
re-surfaced across later tranches: EmissionTier deletion → AM.1; per-branch
push_leaf/push_compound → AM.3 (`cffcb6ba`); SIMD structural bitmap → AM.5+;
payload projection → AN Phase 0; structural-dispatch delete → AQ.5 (AL-retro.md:
21-27, AK-retro.md:41-47).

---

## (c) Root causes of divergence — with evidence

**1. Building on knowingly-dead infrastructure (AI).** AI.md:8-13 explicitly
records that `ir.emission_tier` had zero consumers and zero Direct-classified
rules *before* AI began, then AI built six waves of machinery on top. The Direct
tier could never survive: its function signature `fn __<rule>_direct(state) ->
Option<()>` carried **no tape parameter**, so a Direct rule could not call a Tape
child; `reconcile_cross_component_tiers` therefore promoted every Direct caller
to Tape via `tier_join(Direct, Tape) = Tape` (AL prototype-2:41-51). The result:
`ir.emission_tier` written, all values Tape, `emit_direct_tier_rule` dead code
(AL prototype-2:22-35). The `emission_tier_bonus` e-graph reward (AI.3) was
correspondingly **inert** — no rule ever got Direct tier, so the bonus never
fired (AL prototype-2:243-248).

**2. A redundant axis masquerading as a subsystem.** The load-bearing insight,
found only in AL (prototype-2:98-101): *"Delete EmissionTier entirely. The
materialization class (MustTape / TapeSpanOnly / TransparentElide) already
determines the emission shape… they encode the same information."* EmissionTier
was a second axis isomorphic to `MaterializationClass`. AM.1's 2,306-line
deletion confirms it. This is a god-abstraction failure: a whole CSP variable
family, decode pass, two constraint modules, and a reconciliation pass, all
encoding information another axis already held.

**3. A shipped correctness regression under a performance banner (AI→AJ).** AI
shipped with canada.json **failing** (AJ.md:67 shows "Post-Z.7 canada FAIL").
Root causes AJ.2 had to fix: `input.to_owned()` copied the entire 2.2 MB input
on every parse (~0.5 ms pure memcpy), and `state.offset < owned.len()` rejected
any input with a trailing newline — canada ends in `\n` (AJ.md:42-53). AI also
required an "OOM mitigation" fixup (`e04b52e42`). A tranche whose thesis was
"maximally efficient emission" left the flagship benchmark both broken and
memcpy-bound.

**4. Bench framing that hid the real gap.** AK's competitor table cites
simd-json and serde_borrow — "BBNF 2,008 vs simd-json 1,638 on citm: +22.6%",
"+93.8% on canada" (AK.md:66-68). But the true SOTA target, **sonic-rs, was
23-35% ahead**: citm -34%, twitter -34%, data -35%, data_xl -23%, canada -2%
(AL prototype-4:27-33). The campaign measured itself against a beatable baseline
while the frontier target sat a third of a second-per-parse away. The charter's
aim ("bests *every* SOTA parser") was silently narrowed to "bests simd-json."

**5. CSP that was neither global nor cheap.** prototype-1:6-7 measured
`solve_grammar_components` consuming **~11.95 s** and hitting the **1,000,000-
node budget fallback** on a single 147-rule CSS component. The pathology:
`EnginePropagation` enforces pairwise engine *equality* across every regex
variable in a component — `O(n²·|engines|)` implications (prototype-1:6). The
"global" solve was simultaneously too expensive to be honest compile-time and
too budget-capped to be reliably global.

**6. Structure-sharing found then discarded end-to-end.** prototype-1:8 traces
it: `EGraph::add` clones nodes into parents/classes, analysis clones class facts,
`write_back` rebuilds fresh `IrNode` trees, backend prep re-clones solved
sidecars. CSP decisions keyed by `NodeId` get collapsed to `StringId → strongest
engine`, forcing O(n) reverse string scans in the backend (prototype-1:5,16).
Structural identity is destroyed at the IR→backend seam.

**7. Post-hoc close-outs erasing the plan/emergent boundary.** AK.md was authored
*inside* commit `c62ad389` (05:15), **after** both implementation commits (05:04,
05:12) (AK-retro.md:3-6). It "reads cleanly because its author already knew the
numbers" — a commit-message-in-prose, not a forward plan. Future readers cannot
tell planned from emergent work.

**8. Tranche-identity collapse (AL).** No `AL.md` ever coalesced; the label
became "a stray sticker" (AL-retro.md:60). The AN plan (`acaa1898`, 14:06) landed
*before* the sole AL.1 commit (18:35), and AN's Phase-0 fold (`17728fd7`, 18:48)
retroactively renamed AL.1 as "Phase 1.1" of AN — the tranche was preempted and
its label reused for a residual correctness fix while planning had already moved
on (AL-retro.md:29-40).

---

## (d) Worth resurrecting for the totality plan

**Durable substrate invariants (proven, keep as hard rules):**

1. **Never allocate to access one child.** AJ.0's zero-alloc backward-walk
   `TapeCursor::child(i)` / `child_count()` replaced a per-accessor `Vec`
   allocation (AJ.md:9-20). Any node-view API in the totality must preserve this.
2. **Borrow input, never copy.** Zero-copy `Parsed<'p, R>` (AJ.2, AJ.md:42-57).
3. **Contiguous single-allocation tape.** Flat `Vec<TapeRec>` with exact
   `with_capacity`, +10-14% over chunked (AK.md:16-24). A chunked/arena substrate
   is a measured regression — do not reintroduce.
4. **Per-branch leaf/compound dispatch.** Leaf Alt branches must not pay
   `mark_children`/`push_compound` overhead (~9% of citm parse time; prototype-2:
   134-219, prototype-3:97-139). This landed later as AM.3 — verify it survives
   into the restart substrate.

**Load-bearing negative results (must not be re-learned):**

5. **Do not build a Direct/Tape/Lazy tier ladder parallel to
   `MaterializationClass`.** It is redundant, dies to cross-component
   reconciliation, and was deleted at 2,306 LOC net (AM.1 `76085303a`;
   prototype-2:80-113). Emission shape *is* the materialization class. One axis.
6. **If any tier notion returns, the ABI insight is:** "Direct" ≠ "no tape
   parameter"; it means "my own record is a leaf," with the tape as a pass-through
   resource (prototype-2:70-79). But the simpler resolution — delete the axis —
   is the one that won.
7. **Component-wide engine *equality* is the CSP pathology.** Replace with
   shared-engine *pool* variables keyed by canonical regex/HIR reuse groups;
   solve over actual coupling scopes, not whole undirected call-graph components
   (prototype-1:6,15). The `O(n²·|engines|)` `EnginePropagation` + 1M-node budget
   fallback is a documented dead end.
8. **Preserve `NodeId`/canonical-regex identity into backend emission.** The
   `StringId` collapse + reverse-string-scan is a defect; kill it in the totality
   IR design (prototype-1:4-8,16).

**Frontier programs never executed under AL — the real SOTA-beating levers:**

9. **Structural bitmap pre-scan** (prototype-4 AL.3): grammar-mine the structural
   byte set (JSON `{ } [ ] : , "`; CSS `{ } ; : , " '`), SIMD-scan to a flat
   `Vec<u32>` of structural positions (NEON `vceqq_u8` / AVX2 `_mm256_cmpeq_epi8`
   + `pdep`), dispatch on structural positions so `?w` becomes a no-op
   (prototype-4:194-248). Highest-leverage, grammar-agnostic, un-executed.
10. **Branchless escape-parity string scanner** (prototype-4 AL.4): clmul on the
    backslash mask for escape parity, AND with the quote mask, `tzcnt` the closing
    quote; escape-free fast path returns the span with no validation
    (prototype-4:251-286). This is the concrete path to sonic-rs string-workload
    parity — the -34% gap the campaign never closed.
11. **`TypeDesc::TaggedUnion`** — inline discriminated union for small
    heterogeneous Alts, avoiding per-variant heap `Alloc` (153 slab sites in CSS
    L4) (prototype-3:268-318). Note prototype-4:41 argues it is *superseded* under
    tape-first (BoxedEnum runtime cost is zero when values live on the tape) — a
    genuine open disagreement the totality synthesis must resolve, not inherit.
12. **The honest competitor table** (prototype-4:27-33): sonic-rs as the bar, gap
    decomposed by workload (numeric ≈ parity, string-heavy -34%). Resurrect this
    as the benchmark-honesty template; retire simd-json-as-baseline framing.
13. **E-graph → CSP feedback bridge** (prototype-3:223-265): carry per-rule
    extraction costs to the CSP as `SoftLambdaConstraint`s so extraction and
    strategy solve cohere rather than running as two independent, possibly
    conflicting, passes. Un-executed; addresses the incoherence prototype-1 found.

---

## (e) Meta-process pathologies

1. **Plan-before-execute was not a gate.** AL's four prototypes competed with no
   reconciling `AL.md`; the tranche never coalesced and its scope silently
   scattered across AM/AN/AQ (AL-retro.md:60-64, lesson 1). The retro's
   remediation — require `{LETTER}.md` as a hard gate before any `{LETTER}.N`
   commit — is a direct input to the restart's tranche discipline.

2. **No prototype-coalescence step.** Four coherent-but-divergent drafts needed
   an *explicit merge agent* to declare loser scopes as deferred; implicit
   orchestrator judgment let them all evaporate (AL-retro.md:70-73, lesson 3).
   This is precisely the "synthesis agent folds results into one spec" step the
   SK-V21 convergence loop mandates (PASS-ALPHA.md:44) — AL is the negative
   example that justifies it.

3. **Label-integrity on pivots was violated.** Reusing "AL" for a residual
   serialize fix after the plan had shifted to AN corrupted the retrospective
   record (AL-retro.md:65-69, lesson 2) — the exact failure the
   `new-tranche-new-doc` edict later codified.

4. **Post-hoc close-outs (AK) blur planned vs emergent.** AK.md was written after
   its own implementation (AK-retro.md:3-6). Fine for a ≤200-LOC surgical pass,
   but corrosive as a general habit.

5. **A productive rhythm worth codifying:** build-then-prune. AK laid the
   `__branch_idx` mechanism and left obsolete EmissionTier scaffolding standing;
   AM immediately demolished it (AK-retro.md:78-84, lesson 3). The prune pass is
   not "deferred debt" — it is a planning idiom. But note the asymmetry: AJ/AK's
   scaffolding was *small and load-bearing*; AI's was *large and dead-on-arrival*.
   The idiom only pays when the build is validated before the scaffolding grows.

**Synthesis for SK-V21.** The AI→AL arc is the campaign in miniature: a large,
elegant-sounding optimization system (three-tier emission) that was documented as
dead before it was built, shipped a correctness regression, produced zero
measured value, and was deleted at 2,300 LOC — bracketed by two small, honest,
measured substrate tranches (AJ, AK) that actually moved throughput — terminating
in total planning divergence (AL: four plans, zero reconciliation, label
collapse). The restart's founding instincts are vindicated by this shard: one
axis not two; measure against the true SOTA (sonic-rs, not simd-json); reconcile
prototypes into one spec before executing; and the real un-executed frontier work
(structural bitmap + clmul string scanner) is the throughput lever the totality
plan must pick up where AL dropped it.
