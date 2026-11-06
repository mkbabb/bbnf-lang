# LOCK-6 · THE ISOMORPHISM PLANE (rust/ts split) · Pass-3 Research Directives

**Binding cross-lock frame (facts — build on, do not re-litigate).** Substrate is skinny@HEAD; designs
target the greenfield workspace; legacy top-level `crates/` is archival; every number names its
HEAD/base-commit (XL-A, XL-H). Grammar tier holds Regex opaque (`StringId`), engine tier holds rich
`Hir`, canonical regex home is parse-that (XL-C). The fast TS face is the BUILD-TIME EMITTER; runtime
decoders survive only as isomorphism/correctness oracles (XL-D). Real type consumer is
`TypeAnalysis::from_ir` (`backend/driver/analysis.rs`, XL-F). LOCK-3 owns the middle-IR tower + decision-driver seat (working frame, XL-B).

## 6A — MIRROR-TREE
- Q1. Define the exemption budget's inputs — T (structural mirrors) and I (idiom classes) — by a grep-countable rule *independent of the observed exemption total*. How many idiom classes exist? Does the resulting cap reproduce the live count without being fit to it, go RED on a synthetic extra exemption, and forbid growth at a defended count?
- Q2. For ≥2 representative names on the clean spine, trace each to its origin(s): single grammar-derived source, or authored independently on each side? Give the concrete traces.
- Q3. Who owns the provenance / name-map linter that gates grammar-derived names across LOCK-3/LOCK-6? Propose an owner + rationale against the XL-B tower frame.
- Evidence: drive the TS surface *behaviorally* against canonical Rust over the one live pair — not an intra-Rust comparison.

## 6B — SPEC-GEN-SKELETON
- Q1. On the tape-layout face specifically (byte layout + decoder skeleton + oracle), what is generated-vs-hand-authored LOC across ≥2 grammars? Measured on *that* face, not the schema/type interface.
- Q2. Can one layout spec express BOTH an AoS-stored-discriminator dialect and an SoA-derived-from-source dialect? Give a worked expressibility demonstration, or file the precise reconciliation blocker.
- Q3. For an idiomatic (non-faithful) consumer type, what is the adapter cost (idiomatic type + thin adapter) vs the faithful-union baseline — measured both directions?
- Q4. Is a shared bytecode instruction stream (à la a shared VM layout) a live generation target on the combinator plane that a type-generation refutation does not bind? Argument + routing owed jointly to LOCK-4/LOCK-8.
- Q5. Ownership of `LayoutSpec` + `emit_*` faces and the `MaterializationClass` variable across LOCK-3/LOCK-2 — propose the assignment.
- Evidence: a V8 shape-stability (monomorphism) probe under `node --allow-natives-syntax`, not `tsc` alone.

## 6C — WASM-CORE
- Q1. Over one shared corpus, which browser-deployable structural scan wins for the bbnf batch parser: a wasm-SIMD128 kernel or a TS-native scalar scan? Cold, paired, base-pinned numbers for BOTH options. (wasm-vs-native-NEON is out of scope — do not re-run.)
- Q2. Does a `memory.grow()` mid-batch invalidate outstanding typed-array views in the two-parses-through-one-instance path? Exercise it; quantify copy-out cost against the stop-early margin.
- Q3. Re-confirm the confinement datum (parse-that value.js sync-site count) in-tree where possible, or flag it as an F6/owner-bound assumption (note the open value.js-4.0.0 divorce, XL-G). Price one consolidated cdylib vs two cold instances.

## 6D — ORACLE-ISOMORPHISM
- Q1. Run the digest fold cross-language at least once: a Rust fold of the span / canonical-decimal spec against the TS fold over ≥1 corpus — do they fold bit-identically Rust↔V8? A running instance, not spec text.
- Q2. With deliberate divergences injected (off-by-one escape decode, dropped token class, last-ULP number delta, astral-string mishandle, an error-class flip), what fraction does a static corpus catch vs a coverage-guided fuzzer, and cycles-to-detection? Both numbers.
- Q3. What is the marginal maintenance cost to reach parity for one *added* grammar under this mechanism alone (add a grammar not yet in-tree)?
- Q4. Draw the in/out-of-contract partition (values + accept/reject IN; wording + span OUT; error-class toggle) as a checked-in data file. Carry the DAG constraint: this lock freezes strictly AFTER LOCK-1 (alphabet) and LOCK-7 (tie-breaker).
