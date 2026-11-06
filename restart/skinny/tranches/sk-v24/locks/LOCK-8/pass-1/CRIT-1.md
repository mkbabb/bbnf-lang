# LOCK-8 — THE TS BACKBONE PLANE — pass-1 · CRIT-1 (adversarial critic 1 of 2)

*Within-lock weighting: internal soundness of each family spec + whether the prototypes tested the
sharpest uncertainty each family named. Checklist applied per family: vacuous convergence,
spec-cites-itself circularity, gates-that-cannot-fail, the elegant-reduction trap, legacy aliases,
masked fallbacks, unverified gestalt, consumer-less substrate; constitution i–ix; prototype-evidence
misuse (SPEC_ONLY anchoring a perf claim; prototype validating a weaker artifact than the spec
proposes). Source read: PORTFOLIO.md + CONSTITUTION, FAMILY-SPECS.md, PROTO-1/2/2.patch,
evidence-{probe1,inlining-hetero,deopt}.txt, mapState-fixed-shape-view.patch, research-{A,B,C,D,priorart}.md.*

**Headline (critic-1 verdict).** Two of the four families ran a genuine measurement; one of those two
(8A) measured its sharpest uncertainty squarely and passed it; the other (8B) **measured a strictly
weaker artifact than the family spec proposes and declared PASS on it**, leaving the family's own
self-named "hardest correctness problem" untested and, as written, provably unsound. 8C ran no
prototype and rests entirely on a prior-tranche CONFIRMED datum plus asserted constructibility; 8D
ran no prototype and cannot yet construct its deciding harness because its own open-question (i) is
unresolved. No constitution clause is violated outright. The −14% VM refutation that load-bears three
dispositions is a **real measured prior** (research-priorart.md:121-131, A.W3 `span-dispatch.bench.ts`,
B.W0 P-inv-28), not SPEC_ONLY — that anchor is sound.

---

## FAMILY 8A — THE SHAPE-STABLE CORE — convergence 72%

**What is genuinely converged.** The family's *pivotal* uncertainty — "does constant-capture inlining
actually fire?" — was measured squarely and passed (PROTO-1, probe 2, Node 26). The arity-3 arm
inlines the heterogeneous child leaves; the general loop arm does not (evidence-inlining-hetero.txt is
unambiguous: `stringParser`/`regexParser` inline into `fusedArity3Hetero`, only `ok`/`unsafeSetValue`
into `fusedGeneralHetero`). The `mapState` `Object.create` deopt is real and the fixed-shape
`StateView` collapses the maps (`%HaveSameMap` false→true, evidence-probe1.txt). Zero steady-path
deopts. The disposition ADVANCE is earned and the BLOCKED-SHADOW correctly did **not** become fact.
This is the strongest-evidenced family in the lock.

**Open gaps (enumerated).**

- **8A-G1 (elegant-reduction seam — the actual deliverable is unprototyped).** The family's novel
  move is "make constant-capture a **build-time specializer** (the V8 face of LOCK-4B), not three hand
  arities" (FAMILY-SPECS §8A center B). PROTO-1 measured the **existing hand-written arity-3 arm**; it
  did **not** build or measure any specializer. "Constant-capture inlines [measured] → therefore build
  a specializer [unbuilt]" is precisely the load-bearing-step-reading-and-then-the-hard-part pattern.
  The specializer's feasibility, its emitted-arm shape, and whether generated arms inline identically
  to the hand replica are all SPEC_ONLY.

- **8A-G2 (the specializer's applicability is narrower than the thesis — from PROTO-1's own nuance).**
  PROTO-1 found the unroll buys inlining **only when leaves are SFI-heterogeneous**; homogeneous lists
  inline in *both* arms (PROTO-1 §probe2, carried-forward nuance). SFI identity is a runtime V8
  property. A build-time specializer must therefore decide *when* to unroll from a build-time
  *approximation* of leaf-kind heterogeneity — an unconverged design question the spec's gate (4)
  (which only asserts "unrolled inlines, general does not") does not capture. Gate 4 as written is
  **true only for heterogeneous lists** and would false-green on any homogeneous corpus.

- **8A-G3 (masked fallback — StateView field-completeness unverified, force-cast hides it).** The
  `mapState-fixed-shape-view.patch` narrows the view from the full prototype-chained `ParserState`
  (every field + methods, incl. `getLineAndColumn`) to a 5-field record `{src,value,offset,isError,
  furthest}`, then launders it through `oldView as unknown as ParserState<T>`. The claim "every field
  any consumer reads — R-D" (FAMILY-SPECS §8A "one genuinely new source change") is **asserted, not
  verified by enumerating actual `.mapState` call sites** in value.js/latex/keyframes. If any mapState
  fn reads `.expected`/`.context`/`.text` or calls `oldView.getLineAndColumn()`, the fix silently
  yields `undefined` / a `TypeError`. Every 8A gate is structural (`%HaveSameMap`, deopt, inlining) —
  **none is a behavioral mapState-consumer oracle**. The force-cast is the tell: it is there precisely
  to suppress the type error the narrowing would otherwise raise.

- **8A-G4 (evidence-transfer gap — Maglev vs TurboFan).** PROTO-1 forced a synchronous **TurboFan**
  compile with `--no-maglev`. Node 26's default production tier is **Maglev**, whose inlining policy
  differs. The pivotal inlining win is measured on a tier much value.js code will not reach in
  practice. Whether constant-capture inlines under Maglev is unmeasured; gate (4)
  (`--trace-turbo-inlining`) observes TurboFan only.

- **8A-G5 (named corpus not the corpus run).** Gate (3) requires "zero steady-path deopts **over the
  value.js corpus**." PROTO-1 swept "200k jsonParser + 200k **synthetic** CSS-value triple" — synthetic,
  not the real value.js corpus. The result is credible but the gate's named corpus was not exercised.

- **8A-G6 (bounded, honest — the dispatch residue).** `dispatch` (leaf.ts:134-145) de-megamorphization
  remains the accepted residue / BLOCKED-SHADOW. Correctly bounded to ~+5% no-regression, not a gate.
  Not a defect; listed for completeness as a standing carry.

**Circularity / vacuity / constitution:** clean. Gate (5)'s "~+5% no-regression, never a ≥10% gate"
correctly encodes SPEC §0 (refutation law iv). No god module. StateView allocation replaces the
Object.create allocation → net zero-cost preserved (clause v intact).

---

## FAMILY 8B — TYPE-LEVEL TOTALITY — convergence 58%

**The sharp finding (prototype-evidence misuse: the prototype validated a weaker artifact than the
spec proposes).** FAMILY-SPECS §8B mechanism and research-B.md:55-56 both specify
`all(...Ps): Parser<DropUndefined<Values<Ps>>>` — **with** `DropUndefined`, the type-level mirror of
`all()`'s runtime undefined-compaction. **PROTO-2 shipped `all → Parser<Values<T>>` — without
`DropUndefined`** (PROTO-2.patch.md diff, leaf.ts: `export function all<const T…>(…): Parser<Values<T>>`;
the `DropUndefined` type is defined in `types.ts` but **never wired into `all`**). The proto then
declared "8B PASS." Consequences:

- **8B-G1 (the hardest named uncertainty was not tested — elegant-reduction trap).** The family's own
  Exact-Open-Question (i) — R1, the `opt()`-inside-`all()` parity hole — is labelled "the hardest
  correctness problem" (FAMILY-SPECS §8B) and "8B's hardest correctness problem" (research-B.md:146).
  Its deciding experiment is "a **runtime differential** against value.js's real opt-in-all sites"
  (FAMILY-SPECS §8B(i); research-B.md:153 "a measured audit of which value.js sites actually…"). That
  differential **was not run**. PROTO-2 tested literal-headed sequence tuples and literal-alternation
  unions — the *easy* precision — and left the compaction-soundness question entirely unconverged.

- **8B-G2 (soundness — as written the spec's own signature is unsound; the proto's is a lie of a
  different shape).** research-B.md:147-149 and FAMILY-SPECS §8B(i) both concede: for
  `all(a, opt(b), c)`, `DropUndefined` **mistypes the b-present case** (runtime `[A,B,C]`), the plain
  tuple lies for the b-absent case (`[A,C]`). **Neither `Values<T>` (what the proto shipped) nor
  `DropUndefined<Values<T>>` (what the spec proposes) correctly types opt-in-all.** So the proto did
  not merely under-test the spec — it validated a signature the spec itself does not endorse, and the
  spec's endorsed signature is known-unsound. The three routes (type-level filter+document the one
  lawful widening / redefine `all` breaking / distinct `allOptional`) are unresolved.

- **8B-G3 (the PASS metrics do not transfer to the spec's real signature).** The 1.042× instantiation
  ratio, the byte-identical error set, and the GREEN `.test-d.ts` were all measured on
  `Parser<Values<T>>`. `DropUndefined` is an additional tail-recursive conditional type evaluated at
  every `all()` site (61 in value.js). The instantiation-budget gate — 8B's *signature falsifier* —
  was therefore measured on the cheap signature; the spec's real `DropUndefined` signature's budget is
  **unmeasured**. The falsifier "type-level totality that blows the checker" was not put to the actual
  proposed type.

**Other gaps.**

- **8B-G4 (near-vacuous zero-cost gate + proxy build).** Gate (2) "`dist/` semantic diff EMPTY" is
  near-guaranteed by construction for a type-only patch (pure `type` module + erasing casts) — low
  falsification power, though not strictly unfalsifiable. Separately, PROTO-2 proved emit-identity via
  **esbuild** transpile of one file (`leaf.ts` SHA), not via parse-that's actual `dist/` build (tsc/
  bundler). esbuild's type-stripping is a *proxy* for the real build; the real `dist/` was not built
  both ways. Reasonable, but the gate's named artifact (`dist/`) was not the artifact compared.

- **8B-G5 (result-union vs field-triple deferred, not resolved).** 8B "ADOPTs the result-union" as
  *the* typed outcome; the collision with 8D's field-triple is explicitly kicked to pass-2
  ("Pass-2 must adjudicate whether both live," incompatibility map row 1). A named open adjudication,
  honestly flagged — but an open gap for 8B's own "diagnostics typed by position" claim.

- **8B-G6 (dispatch precision retreat is honest and clean).** PROTO-2 found `dispatch<const K>` forces
  a body edit and buys the consumer nothing → reverted to `Record<string, Parser<T>>`. Sound, KISS,
  no defect. Noted so pass-2 does not re-litigate.

**Constitution:** clause v proven-by-proxy (see G4); new `src/parse/types.ts` is a proper small DRY
home, no god module. No circularity. Disposition ADVANCE is defensible **only if the registry records
that the PASS covers the literal-precision subset, not the spec's `DropUndefined` signature** — as
stated the disposition over-reads the evidence.

---

## FAMILY 8C — THE ISOMORPHIC MIRROR — convergence 55%

**What is converged.** The data-plane anchor — tape SoA decodes byte-exact, stop-early 4.4–4.6× — is
a **prior-tranche CONFIRMED** measurement (SPEC §0 row 14 / PROVE-14), legitimately cited, not
SPEC_ONLY. The ambitious "generate a runtime TS dispatch mirroring SpanParser" arm is correctly
**refuted-on-arrival** by the measured −14% (research-priorart.md:121-131) and BANKED — the
adjudication is sound and the incompatibility-map row 2 is precise (runtime-switch dead; build-time
specializer is a *different* thing, homed at LOCK-4B). Behavioral-mirror-not-mechanism-mirror for
regex (LOCK-5-exempt) and diagnostics (keyframes live surface) is the right call.

**Open gaps (enumerated).**

- **8C-G1 (nothing new was prototyped; every novel mechanism is constructibility-asserted).** No PROTO
  for 8C this pass. Stratum A's single-spec generator emitting **both** the Rust dumper and the TS
  `TapeDoc` decoder, the isomorphism LINTER, and Stratum B's generated node-vocabulary dispatch are
  all SPEC_ONLY. Disposition ADVANCE rests on "fully constructible on existing CONFIRMED substrate" —
  constructibility **asserted, not demonstrated**. PROVE-14 confirms the *decoder works*; it does not
  confirm a *generator can emit both sides regen-stably* (clean-regen, regen∘regen byte-identical is a
  claim with no evidence yet).

- **8C-G2 (a perf-shaped gate with no evidence path run — the SPEC_ONLY-anchors-perf hazard, honestly
  flagged).** Gate (5) requires stop-early **4.4× across the wasm boundary**. The 4.4× is CONFIRMED for
  file round-trip **only**; open-question (ii) concedes "the boundary cost of the export itself is
  **unmeasured**." The gate is born-RED with no measurement yet attempted, and this gate is deferred to
  GN-4.W4 (cross-lock). The spec is honest, but the number 4.4× must not be allowed to migrate onto the
  wasm-crossing claim in any downstream gestalt until measured — pass-2 should tag it explicitly.

- **8C-G3 (scope tension — Stratum A lives bbnf-side, not in the TS backbone).** LOCK-8 is "parse-that/
  typescript," but Stratum A's artifacts are `crates/bbnf-bench/ts-tape/*.mjs` and `tape_dump.rs`
  (FAMILY-SPECS §8C integration seams) — the crossing "lives bbnf-side, where the campaign may write."
  The TS consumer of `TapeDoc` is the **bench harness**, not parse-that proper. Legitimate (parse-that
  is READ-ONLY), but it means 8C's most-provable stratum is only tangentially "the TS backbone plane."
  Pass-2 should state whether 8C's deliverable is a parse-that face or a bbnf-bench face.

- **8C-G4 (Stratum B depends on an unbuilt LOCK-4B primitive).** The generated dispatch *contract*
  holds "the arity-unroll floor **or is refuted**" — but the only lawful non-refuted form is the
  build-time specialized dispatcher, which is LOCK-4B and does not yet exist (same seam as 8A-G1).
  Cross-lock dependency, not a missing-primitive block, but Stratum B cannot be gated GREEN until 4B
  lands. Consumer-substrate check passes (linter→CI, name-map→linter); no consumer-less substrate.

**Constitution:** clause i (grammar totality) has a mild tension — Stratum A's "machine-readable
schema" is not stated to be **bbnf-grammar-defined**; if it is a bespoke schema it sits beside the
grammar-totality edict (arguably exempt as data-layout, but pass-2 should name it). No refutation-law
violation (the −14% anchor is respected). Disposition "ADVANCE conservative / BANK ambitious" is
correctly split.

---

## FAMILY 8D — CONSUMER-CONTRACT-FIRST — convergence 66%

**What is converged.** Strong empirical grounding: the version supersessions are file-cited (value.js
**4.0.0** bundling parse-that with `libraryExternal=[]`, research-D.md:15; latex 0.7.1; keyframes
6.0.0), the `buildDiagnostic` reader is pinned to exact fields (research-D.md:69), and the
below-combinator seams (`new Parser((state)=>…)`, `new ParserState<T>`, `mergeErrorState`) are
located. The semver **inversion** insight (parse-that 1.0→1.1 invisible to value.js public semver, break
risk entirely at value.js *build* time) is a real and non-obvious correction. The R-8D4 "rebuild the
bundle, testing the stale checked-in 1.0.0 dist is a false green" catch is exactly the kind of
gate-that-cannot-fail the checklist hunts for — 8D caught it itself. Pure-construction disposition is
right; no missing primitive.

**Open gaps (enumerated).**

- **8D-G1 (the deciding harness cannot yet be built — open-question (i) is a prerequisite, not a
  detail).** Gate (4), the three-repo bundled-crater with the mandatory rebuild, **depends on** how
  value.js resolves parse-that at build time, which open-question (i) admits is unresolved and "**must
  be pinned before any suite runs**" (R-8D6). So the family's central falsifier is currently
  unconstructable. Not a BLOCK (the resolution is a lookup, not a missing primitive), but the ADVANCE
  "pure construction" framing understates that step (i) gates the whole harness. No crater ran this
  pass.

- **8D-G2 (the "nine symbols / everything else is free to churn" universal is a strong falsifiable
  claim resting on a single grep pass).** research-D.md:37 asserts "the *entire* semver-critical
  surface is nine symbols." The negative half — that **no** consumer touches any other export — is a
  universal over three repos established by reading, not by a mechanical import-census oracle. If one
  missed symbol (`opt`, `many`, `seq`, `map`, `trim`…) is imported by any consumer, the migration
  artifact silently omits it. Pass-2 should require a **mechanical** import extraction (grep/AST over
  all three `dist`+`src`) as the gate, not prose enumeration. (research-B.md:26 lists `skip/next/wrap/
  trim/many` as public combinators — the census must prove each is consumer-unused, not assume it.)

- **8D-G3 (result-union vs field-triple — the same deferred collision as 8B-G5).** 8D's field-triple
  survival and 8B's result-union adoption are reconciled only *additively and provisionally*
  (incompatibility map row 1: "Pass-2 must adjudicate whether both live or v4-migration retires the
  field-triple"). The `buildDiagnostic` oracle (Gate 2) is named as the deciding experiment but **was
  not run** this pass. Open for both families.

- **8D-G4 (latex 0.7→1.1 is real, untested, and the overlay proxy is bounded).** The `^0.7.1→^1.1.0`
  jump is "real and untested"; PROTO-2's latex secondary corpus was a **1.0.0-family overlay**, not a
  native compile (PROTO-2.md caveat: "bounded non-regression check, not a native compile"). The
  three-step attribution matrix (0.7→1.0, then 1.0→1.1) is specified but unrun. Named, not resolved.

**Constitution:** clause viii (consumer-less substrate) is *inverted into the family's thesis* —
exemplary. No god module (the `diagnostics/` split is the subject of Gate 1). No circularity. Clause
ix (extant-tranche consciousness) is honored with named supersessions (3.1.0→4.0.0). Disposition
ADVANCE sound; the caveat is that "pure construction" hides the (i) prerequisite.

---

## CROSS-CUTTING (within-lock)

1. **The two prototypes hit different-strength targets.** PROTO-1 struck 8A's sharpest uncertainty
   dead-center (constant-capture inlining) and passed it — model behavior. PROTO-2 **swerved around**
   8B's sharpest uncertainty (opt-in-all / `DropUndefined` soundness) and passed an easier question,
   then labelled the family PASS. The pass-1 registry must not record 8B at the confidence PROTO-2's
   verdict line implies; the `DropUndefined` differential (research-B.md:182's own gate: "does
   `DropUndefined` typing match runtime output for both b-present and b-absent") is the un-run crux.

2. **Two families share one unbuilt primitive (the build-time specializer).** 8A-G1 and 8C-G4 both
   terminate at LOCK-4B's build-time-generated specialized dispatcher, which does not exist. This is
   correctly a cross-lock *dependency*, not a within-lock block — but it means neither 8A's "specializer"
   deliverable nor 8C's Stratum-B "generated dispatch" can be gated GREEN inside LOCK-8 alone. The
   gestalt, not pass-2 of LOCK-8, closes it.

3. **The −14% anchor is sound and should be reused with confidence.** Three dispositions (8A worst-case
   bound, 8C-ambitious bank, incompatibility-map row 2) lean on "VM/runtime-switch is refuted." That is
   a real measured prior (A.W3 `span-dispatch.bench.ts`, B.W0 P-inv-28), not SPEC_ONLY — no critic
   objection.

4. **No family is BLOCKED-candidate** — concur with the spec. Every central mechanism's primitives
   exist; the gaps above are un-run measurements, an over-read verdict (8B), a masked narrowing (8A-G3),
   and deferred adjudications — not missing primitives equal-in-difficulty to the original problem.

## Pass-2 directives (critic-1)

- **8B:** wire `DropUndefined` into `all` as the spec actually proposes, run the value.js opt-in-all
  runtime differential, re-measure the instantiation budget on the real signature, and resolve the
  three-route soundness question. Re-label the pass-1 PASS as "literal-precision subset only."
- **8A:** prototype (or spec concretely) the build-time specializer including its SFI-heterogeneity
  decision rule; add a behavioral `.mapState`-consumer oracle enumerating fields read across the three
  consumers before shipping the StateView narrowing; re-run the deopt sweep on the real value.js corpus;
  add a Maglev-tier inlining check.
- **8C:** run a minimal single-spec→two-artifact regen-stability probe; explicitly quarantine the 4.4×
  number from the wasm-boundary gate until measured; state the parse-that-face vs bbnf-bench-face scope.
- **8D:** pin value.js's build-time resolution (open-q i) and stand up the crater harness; replace the
  nine-symbol prose census with a mechanical import oracle; run the `buildDiagnostic` deciding experiment.
