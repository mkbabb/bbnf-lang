# Wave 9 R1 - IR CostFacts Schema Research

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: research-only artifact for `skinny/crates/ir`; no Rust source edits.

## Findings with file:line citations

1. W9 is explicitly scoped as a CostFacts substrate wave, not a runtime tuning
   wave. SPEC 11 names `ir/src/cost.rs` as a new owner file, then requires
   `passes/src/lib.rs`, `codegen/src/lower/mod.rs`, and `xtask/src/main.rs`
   consumers for `cost_facts`, lowerer consumption, and `gate-json
   --with-cost-facts` output (`restart/skinny/tranches/sk-v7/SPEC.md:343`,
   `restart/skinny/tranches/sk-v7/SPEC.md:345`,
   `restart/skinny/tranches/sk-v7/SPEC.md:346`,
   `restart/skinny/tranches/sk-v7/SPEC.md:347`,
   `restart/skinny/tranches/sk-v7/SPEC.md:348`,
   `restart/skinny/tranches/sk-v7/SPEC.md:349`).

2. The current IR crate is already serde-ready. `ir/src/lib.rs` imports
   `Deserialize` and `Serialize` at the root (`skinny/crates/ir/src/lib.rs:1`),
   `RuleId` derives both serde traits (`skinny/crates/ir/src/lib.rs:6`),
   `BackendIr` derives both serde traits (`skinny/crates/ir/src/lib.rs:386`),
   and `BackendShape` derives both serde traits
   (`skinny/crates/ir/src/lib.rs:395`). `skinny/crates/ir/Cargo.toml` already
   depends on workspace serde (`skinny/crates/ir/Cargo.toml:9`,
   `skinny/crates/ir/Cargo.toml:10`), and the workspace serde dependency has
   the `derive` feature enabled (`skinny/Cargo.toml:43`). No Cargo change is
   needed for an IR-only `cost.rs`.

3. `CostFacts.chosen` should reuse the existing `BackendShape` enum rather than
   introducing a second shape taxonomy. The current variants are grammar-neutral:
   `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage`
   (`skinny/crates/ir/src/lib.rs:395`, `skinny/crates/ir/src/lib.rs:396`,
   `skinny/crates/ir/src/lib.rs:397`, `skinny/crates/ir/src/lib.rs:398`,
   `skinny/crates/ir/src/lib.rs:399`, `skinny/crates/ir/src/lib.rs:400`,
   `skinny/crates/ir/src/lib.rs:401`, `skinny/crates/ir/src/lib.rs:402`).

4. The naming precedent after W7/W8 is neutral IR vocabulary, with JSON names
   confined to grammar inputs and generated JSON output. `TapeKind` now uses
   `Container`, `Sequence`, `KeyValuePair`, `StringValue`, `NumberValue`,
   `BoolValue`, `NullValue`, `Member`, and `Element`
   (`skinny/crates/ir/src/lib.rs:485`, `skinny/crates/ir/src/lib.rs:486`,
   `skinny/crates/ir/src/lib.rs:487`, `skinny/crates/ir/src/lib.rs:488`,
   `skinny/crates/ir/src/lib.rs:489`, `skinny/crates/ir/src/lib.rs:490`,
   `skinny/crates/ir/src/lib.rs:491`, `skinny/crates/ir/src/lib.rs:492`,
   `skinny/crates/ir/src/lib.rs:493`, `skinny/crates/ir/src/lib.rs:494`,
   `skinny/crates/ir/src/lib.rs:495`, `skinny/crates/ir/src/lib.rs:496`).
   `DirectBuildDecode` likewise uses `EscapedString` and `NumberScalar`, not
   JSON-prefixed names (`skinny/crates/ir/src/lib.rs:562`,
   `skinny/crates/ir/src/lib.rs:563`, `skinny/crates/ir/src/lib.rs:565`,
   `skinny/crates/ir/src/lib.rs:566`). REDRESS records the W7 grep gate that
   removed public JSON-prefixed parse-that and passes surfaces
   (`skinny/REDRESS.md:2399`, `skinny/REDRESS.md:2400`,
   `skinny/REDRESS.md:2405`, `skinny/REDRESS.md:2420`,
   `skinny/REDRESS.md:2421`, `skinny/REDRESS.md:2422`,
   `skinny/REDRESS.md:2423`) and the W8 IR cleanup that removed
   `StructuralAlphabet::json()` and the JSON whitespace special case
   (`skinny/REDRESS.md:2443`, `skinny/REDRESS.md:2444`,
   `skinny/REDRESS.md:2455`, `skinny/REDRESS.md:2458`).

5. The B2 `Measurement` sketch is not mechanically compilable as written if
   copied into the current crate style. It derives `Eq`
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:104`) while
   containing `Option<f64>` fields for throughput and cycles per byte
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:107`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:108`).
   Rust `f64` does not implement `Eq`, and the problem would cascade through
   `RejectedAlternative` and `CostFacts`, which B2 also sketches with `Eq`
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:53`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:88`).
   Keeping `LayoutFacts` as `PartialEq, Eq` also matters because it currently
   derives both traits (`skinny/crates/passes/src/lib.rs:77`) and would carry
   the new `cost_facts` map.

6. B2 is correct that `Measurement.workload` must remain a free-form
   grammar-neutral tag, not a `JsonCorpus` enum. The design says the workload is
   a string and gives `"generated-retained"`, `"direct"`, and `"track2"` as
   examples (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:105`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:106`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:132`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:133`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:134`). The
   grammar-neutrality checklist further says `EvidenceSource` and
   `RejectionReason` are predicate categories, not workload labels
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:492`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:493`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:494`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:495`).

7. The current producer is exactly the orphan risk W9 must remove. `compile()`
   derives a `shape_plan` and writes only `shape_plan.backend_shape` into
   `layout_facts.backend_shape` (`skinny/crates/passes/src/lib.rs:42`,
   `skinny/crates/passes/src/lib.rs:43`,
   `skinny/crates/passes/src/lib.rs:44`,
   `skinny/crates/passes/src/lib.rs:45`,
   `skinny/crates/passes/src/lib.rs:46`,
   `skinny/crates/passes/src/lib.rs:47`,
   `skinny/crates/passes/src/lib.rs:48`). `LayoutFacts` has
   `backend_shape` but no `cost_facts` field (`skinny/crates/passes/src/lib.rs:77`,
   `skinny/crates/passes/src/lib.rs:78`,
   `skinny/crates/passes/src/lib.rs:79`,
   `skinny/crates/passes/src/lib.rs:80`,
   `skinny/crates/passes/src/lib.rs:81`,
   `skinny/crates/passes/src/lib.rs:82`,
   `skinny/crates/passes/src/lib.rs:83`,
   `skinny/crates/passes/src/lib.rs:84`). The decision tree currently returns
   only `BackendShape` plus diagnostics (`skinny/crates/passes/src/lib.rs:365`,
   `skinny/crates/passes/src/lib.rs:366`,
   `skinny/crates/passes/src/lib.rs:367`,
   `skinny/crates/passes/src/lib.rs:368`,
   `skinny/crates/passes/src/lib.rs:380`,
   `skinny/crates/passes/src/lib.rs:385`,
   `skinny/crates/passes/src/lib.rs:386`,
   `skinny/crates/passes/src/lib.rs:391`,
   `skinny/crates/passes/src/lib.rs:407`,
   `skinny/crates/passes/src/lib.rs:408`,
   `skinny/crates/passes/src/lib.rs:420`,
   `skinny/crates/passes/src/lib.rs:421`).

8. The current codegen consumption path is also shape-only. `LowerCtx` carries
   `backend_shape: &HashMap<RuleId, BackendShape>` and diagnostics
   (`skinny/crates/codegen/src/lower/rust.rs:20`,
   `skinny/crates/codegen/src/lower/rust.rs:21`,
   `skinny/crates/codegen/src/lower/rust.rs:22`,
   `skinny/crates/codegen/src/lower/rust.rs:23`,
   `skinny/crates/codegen/src/lower/rust.rs:24`), `shape_for` falls back to
   `EagerTape` when no entry exists (`skinny/crates/codegen/src/lower/rust.rs:41`,
   `skinny/crates/codegen/src/lower/rust.rs:42`,
   `skinny/crates/codegen/src/lower/rust.rs:43`,
   `skinny/crates/codegen/src/lower/rust.rs:44`,
   `skinny/crates/codegen/src/lower/rust.rs:45`), and per-shape dispatch matches
   directly on `BackendShape` (`skinny/crates/codegen/src/lower/rust.rs:48`,
   `skinny/crates/codegen/src/lower/rust.rs:49`,
   `skinny/crates/codegen/src/lower/rust.rs:50`,
   `skinny/crates/codegen/src/lower/rust.rs:51`,
   `skinny/crates/codegen/src/lower/rust.rs:52`,
   `skinny/crates/codegen/src/lower/rust.rs:53`,
   `skinny/crates/codegen/src/lower/rust.rs:54`). `codegen/src/lib.rs` passes
   only backend-shape maps into lowering (`skinny/crates/codegen/src/lib.rs:98`,
   `skinny/crates/codegen/src/lib.rs:100`,
   `skinny/crates/codegen/src/lib.rs:104`,
   `skinny/crates/codegen/src/lib.rs:106`,
   `skinny/crates/codegen/src/lib.rs:107`,
   `skinny/crates/codegen/src/lib.rs:133`,
   `skinny/crates/codegen/src/lib.rs:135`,
   `skinny/crates/codegen/src/lib.rs:140`,
   `skinny/crates/codegen/src/lib.rs:142`,
   `skinny/crates/codegen/src/lib.rs:143`).

9. REDRESS 72 proves capacity must be evidence-scoped, not global. It admitted
   cap 16 only for generated retained `OffsetTape` parsing
   (`skinny/REDRESS.md:1996`, `skinny/REDRESS.md:1997`,
   `skinny/REDRESS.md:2001`, `skinny/REDRESS.md:2002`,
   `skinny/REDRESS.md:2003`, `skinny/REDRESS.md:2004`) after positive native
   rows (`skinny/REDRESS.md:2032`, `skinny/REDRESS.md:2034`,
   `skinny/REDRESS.md:2035`, `skinny/REDRESS.md:2041`), but rejected a global
   cap-16 policy because Track 2 and generated direct `SinkOnly` regressed
   (`skinny/REDRESS.md:2045`, `skinny/REDRESS.md:2046`,
   `skinny/REDRESS.md:2047`, `skinny/REDRESS.md:2048`,
   `skinny/REDRESS.md:2049`, `skinny/REDRESS.md:2050`,
   `skinny/REDRESS.md:2051`, `skinny/REDRESS.md:2052`,
   `skinny/REDRESS.md:2053`). That is the strongest argument for CostFacts as a
   per-rule, per-shape substrate.

10. W9 exit requires not just type presence but exported evidence. The SPEC exit
    gate requires seven populated JSON-rule CostFacts, `gate-json
    --with-cost-facts` output, and two new diagnostics
    (`restart/skinny/tranches/sk-v7/SPEC.md:358`,
    `restart/skinny/tranches/sk-v7/SPEC.md:359`,
    `restart/skinny/tranches/sk-v7/SPEC.md:360`,
    `restart/skinny/tranches/sk-v7/SPEC.md:361`). B2's falsifiability gate
    further requires at least four rejected alternatives per CostFacts entry,
    REDRESS-backed evidence with `EvidenceSource::RedressBackfill`, serde
    round-trip, and no silent `DefaultOffsetTape`
    (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:477`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:478`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:479`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:480`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:481`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:482`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:483`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:484`,
    `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:485`).

## Recommendations with falsifiability gates

1. Add `skinny/crates/ir/src/cost.rs` as a leaf IR module and re-export the
   public surface from `ir/src/lib.rs`:
   `pub mod cost;` and `pub use cost::{CapacityPolicy, CostFacts,
   EvidenceSource, Measurement, PriorityStep, RejectedAlternative,
   RejectionReason, ShapeRationale};`. Keep the structs in `ir`, not `passes`,
   because B2 requires adjacency to `BackendShape` and shared consumption by
   `passes` and `codegen`
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:42`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:43`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:44`).
   Gate: `cargo check -p ir` passes with no Cargo changes, and downstream code
   can import `ir::CostFacts` without using `ir::cost::CostFacts`.

2. Keep `CostFacts` shape close to B2, but make `Measurement` Eq-safe:
   `rule_id: RuleId`, `chosen: BackendShape`, `rationale: ShapeRationale`,
   `rejected: Vec<RejectedAlternative>`, `priority_fired: PriorityStep`, and
   `capacity_policy: Option<CapacityPolicy>` are the right top-level fields.
   Replace floating measurement fields with scaled integer fields, for example
   `throughput_mbps_x1000: Option<u64>` and
   `cycles_per_byte_x1000: Option<u64>`, or drop `Eq` from every parent type and
   from `LayoutFacts`. The integer path is smaller and preserves the existing
   `Eq` pattern. Gate: the B2 sketch's `Option<f64>` fields are absent from
   `ir/src/cost.rs`, `cargo check -p ir` passes, and `cargo test -p passes`
   still compiles once `LayoutFacts.cost_facts` is added.

3. Extend `Measurement` with a provenance string, not a new enum axis:
   keep `source: EvidenceSource`, add `source_ref: String`, and keep
   `workload: String`. `EvidenceSource` should stay in the B2 category set
   (`BenchProbe`, `RedressBackfill`, `AuthorDeclared`, `StaticAnalysis`) or an
   equivalent neutral set; `source_ref` carries values such as `REDRESS-72` or a
   bench run id. Gate: REDRESS 72 backfill emits at least one
   `EvidenceSource::RedressBackfill` measurement whose `source_ref` is
   `REDRESS-72`, and `rg -n 'Json|json|sonic|yyjson' skinny/crates/ir/src/cost.rs`
   returns no matches.

4. Prefer `RejectedAlternative { evidence: Vec<Measurement> }` with
   `#[serde(default, skip_serializing_if = "Vec::is_empty")]` over
   `Option<Measurement>`. REDRESS 72 contains multiple row-level regressions for
   a single rejected global policy, so a vector avoids either lossy evidence or
   synthetic aggregation. Gate: the REDRESS 72 backfill can record the direct
   `SinkOnly` regressions for `instruments`, `distinct_values`, and
   `y_string_unicode` without inventing a combined metric.

5. Make `CapacityPolicy` sparse so the cost model does not invent caps. Either
   use optional fields (`tiny_string_cap: Option<u8>`,
   `container_initial_capacity: Option<u16>`) or use a typed enum of capacity
   decisions. Avoid the B2 sketch's mandatory two-field struct unless both
   values are always evidence-backed. Gate: no serialized CostFacts entry
   contains a non-null capacity value unless the same entry has a
   `Measurement.source == RedressBackfill` or `BenchProbe` record supporting
   that value.

6. Keep enum names predicate- and shape-oriented. Accept names like
   `FirstSetDisjoint`, `FirstSetOverlap`, `ErrorRecoveryRequired`,
   `DirectBuildNoConsumer`, `EventTapeAltDensity`, `DefaultOffsetTape`,
   `PreconditionUnmet`, `ConsumerMismatch`, and `PreviouslyRegressed`. Reject
   names that encode a grammar, corpus, comparator, or current benchmark suite,
   such as `JsonCorpus`, `TwitterCase`, `SonicRegression`, or `YyjsonGap`.
   Gate: `rg -n 'Json|json|twitter|sonic|yyjson|serde_json' skinny/crates/ir/src/cost.rs`
   returns no matches. This gate should be scoped to `cost.rs`; `serde` imports
   are allowed and should not be confused with `serde_json`.

7. Prevent an orphan substrate by landing the IR type with the same-wave
   producer, carrier, consumer, and exporter. `passes::compile()` should build
   `cost_facts` first, derive `backend_shape` as a projection of
   `cost_facts[rule].chosen`, and assert parity. `codegen` should receive
   `CostFacts`, not only `BackendShape`, so lowerers can consume rationale and
   capacity policy. `xtask gate-json --with-cost-facts` should serialize the
   table. Gate: after W9, `rg -n 'cost_facts|CostFacts' skinny/crates/passes skinny/crates/codegen skinny/xtask`
   finds production uses in all three areas, and `rg -n 'backend_shape: &.*HashMap<RuleId, BackendShape>' skinny/crates/codegen/src/lower`
   no longer identifies the sole lowering input.

8. Encode the priority table as data, not a second hand-written match. B2 calls
   out producer drift as the largest risk and recommends a `&'static
   [PriorityStep]` plus gate function per step
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:508`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:515`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:516`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:517`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:518`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:519`). Gate:
   a test fails if `PriorityStep` variant count and priority table length
   diverge, and every CostFacts row contains one chosen priority plus at least
   four rejected alternatives.

## Risks/pre-blocked routes

1. Do not use CostFacts to reopen pre-blocked route families. HANDOFF 3 blocks
   SK-V5 UTF-8 fusion routes, SK-V6 retained-parse and direct-materialization
   routes, Class A NEON tiny-string parse-G fixes, 12-byte token width churn,
   pair-token fusion, function-pointer dispatch, capacity prescan, generic SWAR
   whitespace, separator elision, raw f64 shortcut, PSI/DTA Rust-codegen
   automata, and EventCursor sidecars
   (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:68`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:71`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:75`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:81`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:84`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:85`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:86`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:87`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:88`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:89`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:90`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:91`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:92`,
   `restart/skinny/tranches/sk-v7/HANDOFF.md:93`).

2. Do not encode global cap-16 as a default. REDRESS 72 explicitly accepts cap
   16 only for generated retained `OffsetTape` and rejects the same policy for
   direct `SinkOnly`, hand retained Track 2, and hand direct Track 2
   (`skinny/REDRESS.md:2045`, `skinny/REDRESS.md:2046`,
   `skinny/REDRESS.md:2047`, `skinny/REDRESS.md:2048`,
   `skinny/REDRESS.md:2049`, `skinny/REDRESS.md:2050`,
   `skinny/REDRESS.md:2051`, `skinny/REDRESS.md:2052`,
   `skinny/REDRESS.md:2053`). CostFacts should record this split, not smooth it
   away.

3. Do not use `EvidenceSource::RedressBackfill` as permission to re-run W5's
   rejected StringBlock16 wrapper route. REDRESS 83 says the wrapper regressed
   every named Track 1 parse row by more than the guard and must not be reopened
   or compensated through blocked parse-that/materialization routes
   (`skinny/REDRESS.md:2320`, `skinny/REDRESS.md:2321`,
   `skinny/REDRESS.md:2322`, `skinny/REDRESS.md:2323`,
   `skinny/REDRESS.md:2332`, `skinny/REDRESS.md:2333`,
   `skinny/REDRESS.md:2334`, `skinny/REDRESS.md:2347`,
   `skinny/REDRESS.md:2348`, `skinny/REDRESS.md:2349`,
   `skinny/REDRESS.md:2350`, `skinny/REDRESS.md:2351`,
   `skinny/REDRESS.md:2352`, `skinny/REDRESS.md:2353`,
   `skinny/REDRESS.md:2354`).

4. Do not let W9 drift back into JSON shell policy. W8 states that generic
   schema/codegen no longer exposes JSON key or hard-coded JSON shape policy
   and that remaining JSON names are confined to grammar inputs and emitted JSON
   parser output (`skinny/REDRESS.md:2436`, `skinny/REDRESS.md:2437`,
   `skinny/REDRESS.md:2438`, `skinny/REDRESS.md:2439`,
   `skinny/REDRESS.md:2440`, `skinny/REDRESS.md:2460`,
   `skinny/REDRESS.md:2461`, `skinny/REDRESS.md:2462`,
   `skinny/REDRESS.md:2463`). A `Json*` CostFacts enum would regress that
   cleanup.

5. The highest implementation risk is producer drift: the priority walker can
   choose a shape while the CostFacts record silently omits a checked rejection.
   B2 names this as the single biggest design risk and recommends a table-driven
   priority walk (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:506`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:508`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:509`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:510`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:511`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:515`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:516`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:517`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:518`).

6. The second implementation risk is freezing the capacity schema too narrowly.
   B2 warns that REDRESS 72 covers tiny string and container initial capacity
   today, but future route evidence may force additional typed policy fields;
   it rejects an untyped map as substrate erosion
   (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:521`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:522`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:523`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:524`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:525`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:526`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:527`,
   `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:528`).

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- `skinny/REDRESS.md`
- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/ir/Cargo.toml`
- `skinny/Cargo.toml`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/passes/src/diagnostics.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/mod.rs`
- `skinny/crates/codegen/src/lower/rust.rs`
