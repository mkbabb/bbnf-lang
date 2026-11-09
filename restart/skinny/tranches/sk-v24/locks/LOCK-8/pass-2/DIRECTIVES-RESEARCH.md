# LOCK-8 — THE TS BACKBONE PLANE — PASS-2 RESEARCH DIRECTIVES

*Steering questions only. Prototype on scratch copies (PROVE-11 recipe); parse-that/typescript stays READ-ONLY and lands only via BBNF-PT-G. Every number: cold and deterministic, never warm/cached, never a synthetic stand-in for a named corpus. Disclose the exact corpus (value.js's real 259-file graph, latex-paper native, keyframes) and — where V8-tiered — the tier, for each figure. The V8 JIT and the TS checker are two engines; never conflate a runtime measurement with a checker one.*

## 8A — runtime / shape core
- Mechanically enumerate every field AND prototype method that real `.mapState` consumers read across value.js, latex-paper, keyframes — not by inspection. Does a fixed-shape state view collapse the per-parse hidden-class divergence WITHOUT dropping any of them? Run an actual consumer against the patched `mapState`, not a shape probe alone.
- Can the boundary be typed so no `as unknown as` / any-grade cast is required? Supply the precise shared read-surface type the view and the full state both satisfy.
- Build-time specializer generalizing arity-N fusion to value.js's `all()` sites (overlaps LOCK-4B's fuser — coordinate the TS-execution number, don't duplicate): does it inline under BOTH TurboFan AND Maglev (Node 26's default tier)? What emitted-code-size ceiling holds across the full site count within the generated-size budget?
- Separate the inlining win's real magnitude (structural leg) from the steady-state/temporal leg, both on the real value.js corpus; deopt sweep on the actual corpus, not synthetic triples.

## NEW — static SFI-heterogeneity approximation (8A-homed, shared with 8C)
- What build-time predicate decides which `all()`/`any()` sites are SFI-heterogeneous (need unrolling) vs homogeneous (already inlined by the general loop)? Deliver a rule computable at build time that approximates the runtime SFI-identity property, with its false-positive/negative cost stated.

## 8B — type-level totality
- Does the spec's `Parser<DropUndefined<Values<T>>>` (DropUndefined actually wired into `all`) stay within the instantiation budget at all of value.js's `all()` sites? Re-measure TS2589 count, instantiation count, check-time on THAT signature; disclose which signature every figure was taken on.
- `all(a, opt(b), c)` yields a runtime tuple whose length varies with b's presence. Which resolution stays sound — type-level filter + documented widening, redefine `all`, or a distinct `allOptional`? Decide via a runtime differential against value.js's actual opt-in-`all` sites; first confirm such typed sites exist.
- Prototype the full algebra (`then`, `or`, `chain`, `opt→T|undefined`) and remove the enumerated `Parser<any>` INPUT leaks (PT-E2); measure TS2589/instantiation risk per combinator.
- Compile latex-paper NATIVELY as a budget corpus (not a 1.0.0-family overlay). Prove emit-identity on parse-that's actual `dist/` toolchain built both ways, not a single-file transpile proxy.

## 8C — correspondence
- State whether 8C's deliverable is a parse-that face or a bbnf-bench face. For each of the three structural divergences (SpanParser no-twin, regex Rust-DFA-vs-native-RegExp, diagnostics behavioral-vs-mechanism), name the minimal genuine parse-that-side artifact.
- Determine which of {single-spec→two-artifact generator, behavioral differential oracle, isomorphism linter} is owned by another lock (LOCK-6 6A/6B/6D) versus genuine parse-that residue; scope 8C to only the residue.
- Regen-stability: does a single-spec generator emit both sides such that regen∘regen is byte-identical? Run the minimal probe — this is un-run; the working decoder does not prove a generator emits both sides stably.
- Keep the file-round-trip stop-early number OUT of any wasm-boundary claim until the export/boundary-crossing cost is measured separately. Where does the tape-crossing artifact physically live? Adjudicate the single path with LOCK-6.

## 8D — consumer contract
- Pin FIRST how value.js resolves parse-that at build time (it bundles; `libraryExternal=[]`). The three-repo bundled-crater harness is unconstructable until this is answered.
- Stand up that crater WITH a mandatory `dist/` rebuild step — the checked-in bundle is a false green — and run it once resolution is pinned.
- Replace the nine-symbol prose census with a MECHANICAL import oracle (grep/AST over all three repos' dist+src): does any consumer reach below the combinator layer or import an unlisted symbol?
- DESIGN the PT-E1 diagnostics mechanism: how is the consumer field triple (`furthest`, `expected: string[]`, `getLineAndColumn`) per-parse-scoped with zero hot-path allocation when off? A contract without a scoping mechanism is insufficient.
- Run the real latex-paper `^0.7.1→^1.0→^1.1` three-step attribution matrix and enumerate every break (the overlay already flashed breaks); populate MIGRATION.md.

## PLANE-WIDE
- Home packrat: where does memoization attach in the closure core, what `MemoCell`/memo-key shape stays shape-stable, and what is the type of a memoized parser? Required of all evidence: arming is opt-in; armed/unarmed state disclosed in EVERY perf table; no throughput gate on arming.
- Run the `buildDiagnostic` deciding experiment against a per-parse-scoped returned failed state to adjudicate the result-union vs field-triple diagnostics surface (shared 8B/8D).
