# LOCK-3 — IR TRANSPOSITION SPINE · PASS-2 RESEARCH DIRECTIVES

Steering questions only; produce evidence, draw no dispositions. Re-pin every prototype/measurement to live `skinny@b3cb8c16b` (or a named frozen tree) and name its HEAD — stale-base numbers are wrong-artifact (XL-H). skinny@HEAD is the sole living implementation and measurement substrate; designs target greenfield, ports scope organ-level (XL-A).

## Shared: the `->` type value at the infer edge
`passes/src/lib.rs:204` (`ExprKind::Project { body, .. }`) drops `ty` via `..`; `:203` (`Annotation`→`Unit`) is a legitimate untyped directive, NOT a defect — do not target it. The downstream type consumer is `TypeAnalysis::from_ir` (`backend/driver/analysis.rs`, XL-F). Q: what does threading the :204 `ty` from infer to that consumer require, end-to-end, on live? Each formulation below must state how it CONSUMES one threaded node-type here rather than independently re-deriving it. Evidence: the executed thread or the exact consumption contract.

## Contract / verifier formulation
- A per-pass `verify` predicate wired over `ExprKind::Project`@204 on live: does it read RED on HEAD and GREEN once `ty` threads? Evidence: the executed RED→GREEN transition, plus zero emit footprint (regen outputs byte-identical with the predicate on/off).
- Can one `differential(edge, corpus)` runner generalize the existing XOR-checksum + `json_parity` track1/track2 digests so as to (a) reproduce those digests and (b) localize an INJECTED mis-lowering to the specific edge it was injected on? What corpus-coverage metric bounds it, and how are zero-witness constructs reported (unverified vs sound)? Evidence: the runner executed on live corpus.
- Enumerate the labeled transposition edges of the live spine — how many distinct value levels actually exist? Reconcile `materialize_rule`'s SpanMark/TapeEmit/DirectBuild/Return framing and `SinkOnlyProgram`. Evidence: the enumerated edge-DAG on live.

## Single-IR / one-fold formulation
- Can emission be re-expressed as one `emit(&GrammarIr,&Attributes)` fold with all six regen/check outputs (regen/check × json/css/sheets) byte-identical? Evidence: the EXECUTED result on live (mapped ≠ executed).
- Is `materialize_rule`'s emit-order framing a pure derivation recomputable inside that fold byte-identically, or does it carry data absent from `GrammarIr`? Evidence: the data-flow trace.
- Under a single fold, are `has_rule("value")`@`runtime_generator.rs:98` (name-gate) and `derive_materialization_roles`@1403 (byte-literal role classifier) RETIRED, or merely relocated inside the fold? Evidence: retirement, not relocation.
- Does a per-`RuleId` materialization key suffice for every fleet grammar, or does any require per-`ExprId`? Also measure net-LOC delta and the emission-extensibility count (5→2). Evidence: fleet check + measurements.

## Grammar-as-IR (homoiousia) formulation
XL-C (RATIFIED) dissolves the ONE-HIR dependency — the grammar tier keeps `Regex(StringId)` opaque, so this formulation no longer hangs on a ONE-HIR verdict. The sum-node algebra (`ExprKind` 10-arm, `BackendExpr` 13-arm) stays gated on a schema construct beyond today's Struct-only `DirectTypeKind` (`direct_schema.rs:52`). Q: what named downstream wave would consume an IR expressed as a grammar, and what evidence establishes that consumer on or near live? Evidence: the consumer named, or each gate carried as a born-RED dependency edge with its owner. Do not treat the product-only, sum-excluding subset as standing for the formulation.

## Staged-residual formulation
The grammar-derived stop-set already ships on live as `ScanAlphabet::derive_alphabet` → `const … : &[u8]` feeding a SIMD scan (`css_scan_direct`) — declare this dependency, do not re-author it.
- Can a "stage" fact attach to live IR nodes such that that residual island is provably a stage-0 node the live emit path actually exposes? Evidence: `binding_times` run on the REAL CSS/JSON IR, not a synthetic mirror.
- Can "type-plane erasure = stage-0 erasure on the type sub-plane" be shown concretely by threading the :204 `ty` as a stage-0 residual to a Rust *type*? Evidence: the concrete thread on live.
- If both land: what NEW-site cold paired-PMU delta results (JSON first-byte dispatch)? Evidence: measured ins/B — unmeasured plausibility does not count (PROVE-09).

## Boundaries
Driver seat: LOCK-3 owns WHERE the driver sits (the fact contract at the L1→L2 edge); the engine choice is LOCK-2's — keep them distinct. `no-ts-ir` holds: the backend-agnostic middle is tape + schema (PROVE-14); any TS/meta-grammar face emits type declarations, never a runtime IR (XL-D: the fast TS face is the build-time emitter).
