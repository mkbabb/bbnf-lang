# SK-V7 W9 R4 - xtask gate CostFacts redress research

## Findings

- W9 explicitly owns `xtask/src/main.rs` for a `gate-json --with-cost-facts` flag, plus CostFacts type creation, `LayoutFacts.cost_facts`, diagnostics, and REDRESS 72 evidence backfill. The exit gate requires populated CostFacts for seven JSON rules and `xtask gate-json --with-cost-facts` output per rule (`restart/skinny/tranches/sk-v7/SPEC.md:343`, `restart/skinny/tranches/sk-v7/SPEC.md:349`, `restart/skinny/tranches/sk-v7/SPEC.md:351`, `restart/skinny/tranches/sk-v7/SPEC.md:358`).
- Current skinny xtask has no local `gate_json` report writer. It exposes `gate-json` in usage, strips only `--advisory` from `bench-json` Criterion args, then forwards all `gate-json` passthrough args to `cargo run -p bbnf-bench --bin gate -- ...` and maps the child exit status (`skinny/xtask/src/main.rs:7`, `skinny/xtask/src/main.rs:208`, `skinny/xtask/src/main.rs:227`, `skinny/xtask/src/main.rs:241`). Therefore the lowest-friction W9 route is passthrough-compatible flag plumbing plus bench-gate/report support, not a second xtask-owned renderer.
- The existing gate authority is Markdown schema v3, not JSON stdout. SPEC says `gate-json` rejects rows missing required columns and the bench harness emits that schema verbatim (`restart/skinny/tranches/sk-v7/SPEC.md:56`, `restart/skinny/tranches/sk-v7/SPEC.md:69`). `skinny/RESULTS.md` is a wide Markdown table with the schema-v3 columns (`skinny/RESULTS.md:1`, `skinny/RESULTS.md:3`), and its notes define Track 1/Track 2/provenance semantics (`skinny/RESULTS.md:216`, `skinny/RESULTS.md:217`, `skinny/RESULTS.md:219`).
- The bench gate also confirms the current report shape: it builds `RESULTS.md`, validates schema v3, writes Markdown, and prints rendered Markdown (`skinny/crates/bbnf-bench/src/bin/gate.rs:23`, `skinny/crates/bbnf-bench/src/bin/gate.rs:221`, `skinny/crates/bbnf-bench/src/bin/gate.rs:229`; `skinny/crates/bbnf-bench/src/report.rs:314`, `skinny/crates/bbnf-bench/src/report.rs:381`). This makes raw JSON on ordinary `gate-json` stdout a compatibility break.
- B2's intended CostFacts surface is a single JSON object keyed by rule id, with nested `chosen`, `rationale`, `priority_fired`, `rejected[]`, `evidence`, and `capacity_policy` fields (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:384`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:390`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:393`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:399`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:427`). Its falsifiability gate requires every CostFacts entry to carry rejected alternatives, REDRESS-backed evidence, serde round-trip, and no silent default shape (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:466`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:477`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:479`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:481`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:483`).
- REDRESS 72 evidence is asymmetric and must be recorded as such. It admits cap 16 only for generated retained `OffsetTape`, with no new directive, BIR variant, side table, or rejected NEON route (`skinny/REDRESS.md:1996`, `skinny/REDRESS.md:2001`). Native retained Track 1 saw positive named-row effects including `distinct_values` +57.5% (`skinny/REDRESS.md:2027`, `skinny/REDRESS.md:2032`, `skinny/REDRESS.md:2041`), while the same native pass rejected global cap 16 because hand Track 2 and generated direct `SinkOnly` regressed (`skinny/REDRESS.md:2045`, `skinny/REDRESS.md:2048`, `skinny/REDRESS.md:2049`, `skinny/REDRESS.md:2051`). The ledger states the miss is now a materialization-plan / Track 2 substrate-shape / cost-model issue, not a global tiny-string policy win (`skinny/REDRESS.md:19`, `skinny/REDRESS.md:21`, `skinny/REDRESS.md:24`).
- Recent SK-V7 entries reinforce that W9 must stay evidence/export focused. W0b shows `cargo run -p xtask --release -- gate-json` is a wrapper over the same schema-v3 gate (`skinny/REDRESS.md:2165`, `skinny/REDRESS.md:2167`, `skinny/REDRESS.md:2169`). W7 and W8 closed neutralization work with no throughput claim and no `RESULTS.md` diff (`skinny/REDRESS.md:2423`, `skinny/REDRESS.md:2425`, `skinny/REDRESS.md:2452`, `skinny/REDRESS.md:2455`). W8 hands W9 the CostFacts substrate before further route-fact decisions (`skinny/REDRESS.md:2460`, `skinny/REDRESS.md:2463`).

## Recommendations with falsifiability gates

- Keep normal `gate-json` behavior Markdown-first. `cargo run -p xtask --release -- gate-json` should still forward to the bench gate, write/print the schema-v3 Markdown report, and preserve existing exit semantics. Falsify with:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- gate-json --advisory
rg -n '^# Skinny JSON Bench Results|^\| Corpus \| Workload \| Outcome ' RESULTS.md
```

- Make `--with-cost-facts` emit one complete JSON document, not NDJSON and not a new `RESULTS.md` table. The payload should follow B2's object shape and include a versioned top level such as `schema: "sk-v7-costfacts-v1"`, `grammar: "json"`, and `cost_facts: { "<rule_id>": CostFacts }`. A single JSON object matches the aggregate gates: rule-count floor, rejected-alternative completeness, REDRESS evidence presence, and serde round-trip. NDJSON would force reassembly before those checks; a Markdown table would flatten nested `rejected[]` and conflict with the existing performance-report table. Falsify with:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- gate-json --with-cost-facts --advisory > /tmp/skv7-costfacts.json
jq -e '.schema == "sk-v7-costfacts-v1" and .grammar == "json" and (.cost_facts | type == "object")' /tmp/skv7-costfacts.json
jq -e '(.cost_facts | length) >= 7' /tmp/skv7-costfacts.json
jq -e 'all(.cost_facts[]; (.rejected | length) >= 4)' /tmp/skv7-costfacts.json
```

- Do not mix Markdown and JSON on stdout for the flagged mode. If the implementation must refresh `RESULTS.md` as part of the gate, keep that write side effect but suppress the Markdown print when `--with-cost-facts` is requested, or add an explicit sidecar path and make the command print only the path. Falsify by proving the flagged stdout is parseable JSON and the unflagged stdout still starts with the Markdown report:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- gate-json --with-cost-facts --advisory > /tmp/skv7-costfacts.json
jq -e '.cost_facts' /tmp/skv7-costfacts.json
cargo run -p xtask --release -- gate-json --advisory > /tmp/skv7-gate.md
head -n 1 /tmp/skv7-gate.md | rg '^# Skinny JSON Bench Results$'
```

- Encode REDRESS 72 as both accepted policy and rejected alternatives. The generated retained `OffsetTape` facts that select cap 16 need an accepted `capacity_policy.tiny_string_cap == 16`; direct `SinkOnly`, hand retained Track 2, and hand direct Track 2 cap-16 attempts need `RejectedAlternative { reason: PreviouslyRegressed, evidence.source: RedressBackfill }`. The evidence tags should be workload-plane tags like `generated-retained`, `direct`, and `track2`, not corpus names embedded into the type system. Falsify with:

```sh
jq -e '[.cost_facts[] | select(.capacity_policy.tiny_string_cap == 16)] | length >= 1' /tmp/skv7-costfacts.json
jq -e '[.cost_facts[].rejected[] | select(.reason == "PreviouslyRegressed" and .evidence.source == "RedressBackfill")] | length >= 1' /tmp/skv7-costfacts.json
jq -e '[.cost_facts[].rejected[]?.evidence?.workload | select(. == "direct" or . == "track2" or . == "generated-retained")] | length >= 2' /tmp/skv7-costfacts.json
```

- Add a focused golden/round-trip test for the `--with-cost-facts` report shape, then run the existing W9 gates. Falsify with:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p ir cost
cargo test -p passes cost_facts redress_evidence
cargo test -p bbnf-bench cost_facts_report
cargo run -p xtask --release -- check-json
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- gate-json --with-cost-facts --advisory > /tmp/skv7-costfacts.json
jq -e '.cost_facts' /tmp/skv7-costfacts.json
```

## Risks/pre-blocked routes

- Do not reopen REDRESS 50-72, REDRESS 28+33, or the earlier blocked route families. HANDOFF pre-blocks retained/direct materialization experiments, twice-rejected Class A NEON tiny-string wiring, capacity prescan, function-pointer dispatch, generic SWAR whitespace, separator elision, raw f64 shortcuts, and EventCursor parallel prepass (`restart/skinny/tranches/sk-v7/HANDOFF.md:66`, `restart/skinny/tranches/sk-v7/HANDOFF.md:75`, `restart/skinny/tranches/sk-v7/HANDOFF.md:81`, `restart/skinny/tranches/sk-v7/HANDOFF.md:84`).
- W9 should not use `--with-cost-facts` as cover for a parser hot-leaf retry. W5 already rejected the generated-retained StringBlock16 wrapper and specifically blocks compensating by widening parse-that full string scanning or materialization routes (`skinny/REDRESS.md:2318`, `skinny/REDRESS.md:2347`, `skinny/REDRESS.md:2350`). W6 rejected object-pair value-byte control compaction and blocks object next-key carry, separator elision, function-pointer dispatch, generic SWAR whitespace, EventCursor sidecars, and W5 string-leaf routes (`skinny/REDRESS.md:2358`, `skinny/REDRESS.md:2386`, `skinny/REDRESS.md:2389`, `skinny/REDRESS.md:2390`).
- Producer drift is B2's biggest substrate risk: if the priority walker and emitted CostFacts record diverge, the audit trail can become incomplete while still selecting a shape. B2 mitigates this by making the priority table a `&'static [PriorityStep]` folded by the walker (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:506`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:508`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:516`).
- The B2 sketch derives `Eq` on structs containing `Option<f64>` measurements; copied literally, that will fail Rust compilation because `f64` is not `Eq` (`restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:88`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:104`, `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md:107`). Use `PartialEq` only or a typed/ordered metric wrapper, and let `cargo test -p ir cost` catch it.
- Triumvirate discipline still applies: W9 research, plan, and redress/admit evidence must stay in separate commits, and no wave closes without a REDRESS entry (`restart/skinny/tranches/sk-v7/HANDOFF.md:162`, `restart/skinny/tranches/sk-v7/HANDOFF.md:164`, `restart/skinny/tranches/sk-v7/HANDOFF.md:174`).

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- `restart/skinny/tranches/sk-v7/research/skv7-B2-costfacts.md`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
