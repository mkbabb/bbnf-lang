# SK-V7 Wave 9 Plan: CostFacts Substrate Projection

Inputs:
- `restart/skinny/tranches/sk-v7/SPEC.md` §11 defines W9 owner paths, tasks,
  and exit gate.
- `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md` defines the
  CostFacts substrate and drift risks.
- `restart/skinny/tranches/sk-v7/research/wave-9-r1-ir-cost-schema.md`
  identifies the Eq-safe IR schema and grammar-neutral naming gate.
- `restart/skinny/tranches/sk-v7/research/wave-9-r2-passes-producer.md`
  identifies `derive_backend_shape_with_diagnostics` as the producer insertion
  point.
- `restart/skinny/tranches/sk-v7/research/wave-9-r3-codegen-lowering.md`
  identifies the behavior-preserving `LowerCtx` thread-through.
- `restart/skinny/tranches/sk-v7/research/wave-9-r4-xtask-gate-redress.md`
  identifies the `gate-json --with-cost-facts` JSON sidecar contract.

Intervention: introduce a grammar-neutral CostFacts side table as the source
of backend-shape evidence while keeping generated parser output byte-identical.

Owner paths:
- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/ir/src/lib.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/passes/src/diagnostics.rs`
- `skinny/crates/codegen/src/lower/mod.rs`
- `skinny/crates/codegen/src/lower/rust.rs`
- `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage,sink_only}.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/main.rs`
- `skinny/REDRESS.md`

Falsifiability gate:
- `cargo test -p ir`
- `cargo test -p passes`
- `cargo test -p codegen`
- `cargo run -p xtask --release -- check-json`
- `cargo run -p xtask --release -- check-real-typed`
- `cargo run -p xtask --release -- check-conformance`
- `cargo test --workspace`
- `cargo run -p xtask --release -- gate-json --with-cost-facts --advisory > /tmp/skv7-costfacts.json`
- `jq -e '.schema == "sk-v7-costfacts-v1" and .grammar == "json" and (.cost_facts | length) >= 7' /tmp/skv7-costfacts.json`
- `jq -e 'all(.cost_facts[]; (.rejected | length) >= 4)' /tmp/skv7-costfacts.json`
- `jq -e '[.cost_facts[].rejected[] | select(.reason == "PreviouslyRegressed" and (.evidence[]?.source == "RedressBackfill"))] | length >= 1' /tmp/skv7-costfacts.json`
- `jq -e '[.diagnostics[]?.code] | index("BBNF-DOMINATED-ALTERNATIVE") != null and index("BBNF-COSTFACTS-MISSING-EVIDENCE") != null' /tmp/skv7-costfacts.json`
- `git diff --exit-code -- skinny/crates/runtime/src/grammars/json/generated.rs`
- `git diff --exit-code -- skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `git diff --exit-code -- skinny/RESULTS.md`
- `rg -n 'Json|json|twitter|sonic|yyjson|serde_json' skinny/crates/ir/src/cost.rs`
  returns no matches.

Hard cap: 360 min.

Revert protocol: if any correctness, JSON sidecar, or byte-identical gate
fails, save the source patch to `/tmp/skv7-wave-9-rejected.patch`, revert the
source edits, append a W9 REDRESS rejection naming the failed gate and next
candidate shape, and commit
`docs(sk-v7-wave9-redress): reject costfacts substrate projection`.

Same-wave consumer: `passes::compile()` must populate `LayoutFacts.cost_facts`,
`codegen::lower_to_rust()` must receive and select through CostFacts, and
`xtask gate-json --with-cost-facts` must serialize the table. A type-only IR
addition without all three consumers is an orphan substrate and fails W9.

Implementation shape:
- Add `ir::cost` with `CostFacts`, `ShapeRationale`,
  `RejectedAlternative`, `RejectionReason`, `Measurement`,
  `EvidenceSource`, `CapacityPolicy`, and `PriorityStep`.
- Use scaled integer measurement fields, not `f64`, so existing `Eq` derives
  on `LayoutFacts` remain viable.
- Refactor the existing backend-shape decision ladder into a single
  CostFacts producer that records the chosen shape, rejected alternatives, and
  priority step, then derives `backend_shape` as a projection.
- Backfill REDRESS 72 as redress evidence: cap 16 accepted only for generated
  retained OffsetTape, and global/direct/Track 2 cap-16 attempts recorded as
  `PreviouslyRegressed` rejected alternatives.
- Add `BBNF-DOMINATED-ALTERNATIVE` and
  `BBNF-COSTFACTS-MISSING-EVIDENCE` diagnostics without making them parser
  selection errors.
- Introduce the `ShapeLowering` trait and select lowerers from CostFacts while
  preserving current lowerer output strings and generated files.
- Keep normal `xtask gate-json` behavior unchanged; the flagged mode prints a
  single parseable JSON CostFacts report and does not refresh `RESULTS.md`.

Pre-blocked routes:
- `restart/skinny/tranches/sk-v7/HANDOFF.md` §3 blocks REDRESS 50-55 UTF-8
  fusion routes, REDRESS 60-72 retained/direct materialization routes,
  REDRESS 28+33 Class A tiny-string wiring, capacity prescan, separator
  elision, function-pointer dispatch, generic SWAR whitespace, raw f64
  shortcut, PSI/DTA Rust codegen, and EventCursor sidecars.
- `skinny/REDRESS.md` item 72 admits only generated-retained cap 16 and
  rejects a global cap-16 policy.
- `skinny/REDRESS.md` items 83 and 84 reject the W5 StringBlock16 wrapper and
  W6 object-pair value-byte route; W9 must record evidence only, not retry
  those hot-leaf interventions.
