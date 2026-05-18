# SK-V8 W1-a: CostFacts Producer And Evidence Inventory

Date: 2026-05-18.
Scope: CostFacts producer, evidence-bearing fields, diagnostics, and smallest W1 gate-binding path.
Output: `restart/skinny/tranches/sk-v8/research/skv8-W1-a-costfacts-producer.md`.

## §1 — Findings (concrete, file:line cited)

1. W1 is an evidence/gate wave, not a behavior wave. SPEC Section 4 authorizes
   `ir/src/cost.rs`, `passes`, `codegen`, `xtask`, `bbnf-bench`, `RESULTS.md`,
   and `REDRESS.md` only if rejected; it requires CostFacts rule ids, chosen
   shape, rejected alternative ids, evidence source, wave id, REDRESS reference,
   and comparator strictness evidence to be consumed by `gate-json
   --with-cost-facts` while keeping generated JSON output and parser behavior
   unchanged (`restart/skinny/tranches/sk-v8/SPEC.md:374-405`). Its exit gate
   requires missing CostFacts evidence and strict plane/strictness/freshness/path
   mismatches to fail closed, grammar-neutral CostFacts, non-JSON proof, and
   full-table maintain within +/-1.0% of `SK-V8-open`
   (`restart/skinny/tranches/sk-v8/SPEC.md:407-423`).

2. W0 closed with CostFacts deliberately still unresolved. V12 routes
   `none:pre-W1` CostFacts sentinels to W1 and states W3 remains blocked until
   W0/W1 admission plus its own plan/challenge
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:65-73`).
   The handoff repeats that W1 is active and that `gate-json
   --with-cost-facts` must become the same-wave consumer that rejects missing
   evidence after W1 (`restart/skinny/tranches/sk-v8/HANDOFF.md:174-181`).

3. The exact IR CostFacts object already exists. `CostFacts` contains `rule_id`,
   `chosen`, `rationale`, `rejected`, `priority_fired`, and optional
   `capacity_policy` (`skinny/crates/ir/src/cost.rs:4-13`). Rejected
   alternatives contain `shape`, `reason`, and `evidence`; `Measurement` carries
   `workload`, optional throughput/cycles/hot-leaf metrics, `source`, and
   `source_ref`; `EvidenceSource` already supports `BenchProbe`,
   `RedressBackfill`, `AuthorDeclared`, and `StaticAnalysis`
   (`skinny/crates/ir/src/cost.rs:81-117`). `CapacityPolicy` currently carries
   `tiny_string_cap` and `container_initial_capacity`
   (`skinny/crates/ir/src/cost.rs:119-125`).

4. The producer is `passes::compile()`. Compile derives layout, materialization,
   shape facts, recognizers, backend IR, then calls
   `derive_backend_shape_with_diagnostics`; it writes `shape_plan.backend_shape`
   and `shape_plan.cost_facts` into `LayoutFacts`
   (`skinny/crates/passes/src/lib.rs:28-61`). `LayoutFacts` stores
   `backend_shape` and `cost_facts` together
   (`skinny/crates/passes/src/lib.rs:84-92`), and the initial layout starts with
   both maps empty before shape planning fills them
   (`skinny/crates/passes/src/lib.rs:97-109`).

5. Production logic is deterministic and currently grammar-structure driven.
   For each grammar rule, `derive_backend_shape_with_diagnostics` computes
   `chosen`, `rationale`, `priority_fired`, rejected alternatives, capacity
   policy, constructs `CostFacts`, emits diagnostics, then inserts both
   `backend_shape` and `cost_facts`
   (`skinny/crates/passes/src/lib.rs:390-434`). The current priority sequence is:
   eager tape when forced, sink-only when admitted, collapsed-stage only when the
   target author declaration is present, event-tape when preferred, otherwise
   `OffsetTape`; a missing backend rule falls back to eager with an inconsistency
   diagnostic (`skinny/crates/passes/src/lib.rs:449-509`).

6. Rejected alternatives are mechanically complete. The producer iterates every
   `BackendShape` except the chosen one, adds REDRESS-72 evidence when applicable,
   otherwise assigns the structural rejection reason
   (`skinny/crates/passes/src/lib.rs:527-550`). `SinkOnly` is rejected as
   `ConsumerMismatch` unless admitted; `CollapsedStage` is rejected as
   `AuthorWaiverAbsent` unless the author declaration is present; missing backend
   rules reject all shapes as `PreconditionUnmet`
   (`skinny/crates/passes/src/lib.rs:552-567`). The only hard-coded capacity
   policy today is `tiny_string_cap=16` for chosen `OffsetTape` rules containing
   `TapeKind::StringValue` (`skinny/crates/passes/src/lib.rs:569-583`).

7. The only current measurement-backed evidence is the REDRESS-72 backfill for
   rejected `SinkOnly` on string-value rules. It emits two `Measurement` entries,
   workloads `direct` and `track2`, no numeric metric fields, source
   `RedressBackfill`, source ref `REDRESS-72`
   (`skinny/crates/passes/src/lib.rs:585-608`). The W9 redress says this was the
   intended boundary: generated retained `OffsetTape` string rules carry cap 16,
   while direct/Track 2 cap-16 regressions are recorded as
   `PreviouslyRegressed` alternatives sourced from `RedressBackfill`
   (`skinny/REDRESS.md:2468-2486`).

8. The evidence-bearing fields are narrower than the decision metadata. Actual
   source/ref evidence lives in `RejectedAlternative.evidence[*].source` and
   `.source_ref`; optional numeric evidence lives in the adjacent
   throughput/cycles/hot-leaf fields (`skinny/crates/ir/src/cost.rs:81-117`).
   `rationale`, `priority_fired`, `reason`, and `capacity_policy` explain a
   choice but do not themselves carry an evidence source/ref
   (`skinny/crates/ir/src/cost.rs:43-79`, `skinny/crates/ir/src/cost.rs:89-125`).
   This matters because the current missing-evidence predicate treats either any
   rejected-alternative evidence or any capacity policy as enough
   (`skinny/crates/passes/src/lib.rs:610-616`), even though capacity policy has
   no source/ref field.

9. The current missing-evidence diagnostic is non-fatal. A CostFacts entry with
   fewer than four rejected alternatives or no measurement-backed evidence emits
   `BBNF-COSTFACTS-MISSING-EVIDENCE`
   (`skinny/crates/passes/src/lib.rs:420-425`). A previously regressed rejected
   alternative emits `BBNF-DOMINATED-ALTERNATIVE`
   (`skinny/crates/passes/src/lib.rs:426-430`). The diagnostic code strings live
   in `diagnostics.rs` (`skinny/crates/passes/src/diagnostics.rs:35-41`,
   `skinny/crates/passes/src/diagnostics.rs:56-63`). These diagnostics are
   surfaced in tests as expected, not as a gate failure
   (`skinny/crates/passes/src/lib.rs:1517-1561`).

10. Codegen already consumes CostFacts, but only for selecting the existing
    lowerer shape. `emit_from_source` and typed emission pass
    `layout_facts.cost_facts` and diagnostics into `emit_with_layout`
    (`skinny/crates/codegen/src/lib.rs:68-76`,
    `skinny/crates/codegen/src/lib.rs:85-99`). `LowerCtx` carries `backend_shape`,
    `cost_facts`, and diagnostics (`skinny/crates/codegen/src/lower/rust.rs:20-25`);
    lowering uses the CostFacts entry when present, otherwise constructs a
    default projection fallback (`skinny/crates/codegen/src/lower/rust.rs:27-49`).
    Lowerer selection is by `cost.chosen` (`skinny/crates/codegen/src/lower/mod.rs:17-24`).

11. The current `xtask gate-json --with-cost-facts` path is producer-only. Normal
    `gate-json` shells out to the bench gate unless `--with-cost-facts` is
    present (`skinny/xtask/src/main.rs:240-272`). With `--with-cost-facts`,
    xtask accepts only `--with-cost-facts` plus optional `--advisory`, reads
    `grammars/json.bbnf`, calls `codegen::cost_facts_from_source`, serializes
    schema `sk-v7-costfacts-v1`, grammar, cost_facts, and diagnostics, prints
    JSON, and returns `Ok(())` (`skinny/xtask/src/main.rs:274-305`). There is no
    fail-closed check for `BBNF-COSTFACTS-MISSING-EVIDENCE` in that path.

12. The W0 report already has CostFacts and comparator fields, but CostFacts are
    sentinel-only. `SkV8Telemetry` has `costfacts_rule_id`,
    `costfacts_chosen_shape`, `costfacts_rejected_alternative_ids`,
    `redress_entry`, `wave_id`, `run_id`, and comparator evidence fields
    (`skinny/crates/bbnf-bench/src/report.rs:43-68`). Rendering prints a compact
    CostFacts cell as `rule_id:chosen_shape:rejected_ids`
    (`skinny/crates/bbnf-bench/src/report.rs:575-600`). W0 validation currently
    rejects anything except `none:pre-W1` CostFacts plus `redress_entry=none`
    (`skinny/crates/bbnf-bench/src/report.rs:1007-1019`), and the bench gate
    populates those same sentinels (`skinny/crates/bbnf-bench/src/bin/gate.rs:474-488`).

13. Comparator evidence is already much closer to load-bearing than CostFacts.
    The report validator rejects missing, duplicate, empty, invalid, unsupported,
    or incomplete comparator evidence (`skinny/crates/bbnf-bench/src/report.rs:1135-1228`).
    Sidecar comparators must be DOM/strict with matching freshness, and
    `sidecar-same-run` rejects because there is no structured manifest
    (`skinny/crates/bbnf-bench/src/report.rs:1263-1291`). Native strict
    comparators must match the row workload plane, be strict, be same-run-native,
    have no sidecar freshness, carry Mbps, and point to the expected Criterion
    artifact (`skinny/crates/bbnf-bench/src/report.rs:1313-1375`).

14. Command evidence from this research:
    - `git status --short` in `/Users/mkbabb/Programming/bbnf-lang` returned no
      output before editing this artifact.
    - `cargo xtask gate-json --with-cost-facts --advisory | jq '{schema, grammar,
      cost_facts_len: (.cost_facts | length), diagnostic_codes:
      ([.diagnostics[].code] | unique), sample_rule_0: ...}'` exited 0 and
      reported schema `sk-v7-costfacts-v1`, grammar `json`, 15 CostFacts entries,
      diagnostics `BBNF-COSTFACTS-MISSING-EVIDENCE` and
      `BBNF-DOMINATED-ALTERNATIVE`, with sample rule 0 choosing `OffsetTape` and
      four rejected alternatives.
    - `cargo xtask gate-json --with-cost-facts --advisory | jq -r '.cost_facts |
      to_entries[] | ...'` exited 0 and showed every current JSON CostFacts
      entry chooses `OffsetTape` with `DefaultOffsetTape` and
      `P7OffsetTapeDefault`; only rule `3` carries `{"tiny_string_cap":16}` and
      two `REDRESS-72` evidence entries on rejected `SinkOnly`.
    - `cargo test -p passes cost_facts -- --nocapture` exited 0: one filtered
      test ran and passed (`cost_facts_populate_direct_build_rules_with_redress_evidence`).
      Cargo emitted existing workspace patch warnings, but no command failed.

15. Exact current JSON CostFacts inventory from the command above:

    | Rule id | Current rule by grammar order | Chosen | Rationale | Priority | Capacity | Rejected alternatives |
    |---:|---|---|---|---|---|---|
    | 0 | `null` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | `EagerTape:PreconditionUnmet`, `EventTape:PreconditionUnmet`, `SinkOnly:ConsumerMismatch`, `CollapsedStage:AuthorWaiverAbsent` |
    | 1 | `bool` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 2 | `number` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 3 | `string` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | `tiny_string_cap=16` | `EagerTape:PreconditionUnmet`, `EventTape:PreconditionUnmet`, `SinkOnly:PreviouslyRegressed` with two `REDRESS-72` measurements, `CollapsedStage:AuthorWaiverAbsent` |
    | 4 | `ws` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 5 | `comma` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 6 | `colon` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 7 | `value` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 8 | `pair` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 9 | `member` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 10 | `members` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 11 | `elements` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 12 | `array` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 13 | `object` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |
    | 14 | `json` | `OffsetTape` | `DefaultOffsetTape` | `P7OffsetTapeDefault` | none | same as rule 0 |

    The rule-name mapping follows the current JSON grammar order
    (`skinny/grammars/json.bbnf:1-18`); `cost_facts_from_source` serializes the
    CostFacts map by numeric `RuleId` string, not by name
    (`skinny/crates/codegen/src/lib.rs:222-248`).

## §2 — Recommendations (named falsifiability gates)

1. Gate `CF-W1-load-bearing-costfacts`: keep the existing producer, but make
   `gate-json --with-cost-facts` validate before it prints success. The gate
   should fail if any entry has fewer than four rejected alternatives, any
   rejected alternative lacks an evidence source/ref after W1, any rule id/key
   mismatch appears, any `none:pre-W1` sentinel remains in the W1 report path, or
   `BBNF-COSTFACTS-MISSING-EVIDENCE` is still present. Falsifier: remove the two
   `REDRESS-72` measurements from rule 3 or leave rule 0 evidence-empty; `cargo
   xtask gate-json --with-cost-facts` must exit non-zero.

2. Gate `CF-W1-static-evidence-fill`: use the existing `Measurement` and
   `EvidenceSource::StaticAnalysis` carrier to make non-measured structural
   rejections evidence-bearing without changing chosen shapes. For example,
   `PreconditionUnmet`, `ConsumerMismatch`, and `AuthorWaiverAbsent` can cite the
   generic predicate family that rejected the shape, while REDRESS-72 remains
   `RedressBackfill`. Falsifier: `cargo xtask gate-json --with-cost-facts |
   jq -e '([.cost_facts[] | .rejected[] | select((.evidence // []) | length == 0)] | length) == 0'`
   must pass after W1 and fail on current HEAD.

3. Gate `CF-W1-report-manifest-not-row-fiction`: add a gate-consumed CostFacts
   manifest keyed by RuleId rather than pretending each of the 38 workload rows
   owns all 15 materialized grammar-rule facts. The existing row-level
   `CostFacts` cell can point to the entry-rule/manifest, but the full rule
   inventory must live in a manifest or JSON payload consumed by the same
   `gate-json --with-cost-facts` command. Falsifier: delete one materialized rule
   from the manifest or restore `none:pre-W1`; the gate must fail.

4. Gate `CF-W1-strict-refusal`: reuse the existing comparator validator and add
   W1 tests that strict admission fails on plane mismatch, non-strict
   comparator, stale/historical sidecar, unstructured `sidecar-same-run`, or
   non-`measured-row` validation path. Falsifier: mutate a direct row to claim a
   DOM sidecar or view-boundary strict admission; `gate-json --with-cost-facts`
   must exit non-zero. This should not require parser or generated output edits.

5. Gate `CF-W1-zero-generated-drift`: W1 redress should end with `cargo xtask
   check-json`, `cargo xtask check-real-typed`, and a generated-output diff audit
   proving no runtime/generated JSON or parser behavior changed. Falsifier: any
   byte diff under generated JSON runtime output, or any parser/conformance
   regression, rejects the W1 implementation.

6. Gate `CF-W1-non-json-proof`: if generic CostFacts, passes, or codegen code is
   edited, run the SPEC Section 2.1 public API scan, grammar-branch scan,
   primitive/table scan, template/provider-boundary proof, and a CSS L4, Sheets,
   or BBNF-self no-op/lower/cost proof. Falsifier: any JSON/corpus/layout-role
   policy in generic CostFacts code, or no named non-JSON proof, rejects W1
   (`restart/skinny/tranches/sk-v8/SPEC.md:261-286`).

## §3 — Risks (REDRESS entries to pre-block)

1. Producer-only close. Current `gate-json --with-cost-facts` can print
   CostFacts JSON and return success while also reporting
   `BBNF-COSTFACTS-MISSING-EVIDENCE`; that is exactly the producer-only
   telemetry route SPEC pre-blocks (`restart/skinny/tranches/sk-v8/SPEC.md:421-423`).

2. Treating CostFacts as a performance claim. REDRESS item 87 admits CostFacts
   as evidence plumbing and says generated output and `RESULTS.md` stayed
   unchanged (`skinny/REDRESS.md:2481-2486`). The alpha digest states CostFacts
   records choices and rejected alternatives but does not authorize retrying a
   rejected route without fresh evidence
   (`restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md:75-77`).

3. Capacity policy masquerading as sourced evidence. The current
   `has_measurement_evidence` predicate treats `capacity_policy.is_some()` as
   enough (`skinny/crates/passes/src/lib.rs:610-616`), but `CapacityPolicy` has
   no evidence source/ref (`skinny/crates/ir/src/cost.rs:119-125`). W1 should
   either attach sourced evidence to the relevant rejected alternative or wrap
   capacity-policy evidence in the report manifest.

4. Generic JSON policy leak. W1 must not put JSON corpus names, object/array
   role policy, or workload row policy into `ir::cost` or generic passes/codegen.
   REDRESS 87 already recorded that `ir/src/cost.rs` had no JSON, corpus, or
   comparator naming matches and did not reopen pre-blocked routes
   (`skinny/REDRESS.md:2502-2506`).

5. Ambiguous row-to-rule binding. The 38 W0 workload rows are benchmark rows;
   the CostFacts producer emits 15 grammar-rule facts. A row-level shortcut that
   reports only one rule id without a full manifest would leave most materialized
   rule facts producer-only and should fail `CF-W1-report-manifest-not-row-fiction`.

6. Advisory-mode ambiguity. The existing xtask path accepts `--advisory` but has
   no separate advisory behavior (`skinny/xtask/src/main.rs:274-305`). W1 should
   make the non-advisory command load-bearing and keep any advisory mode
   explicitly non-closing.

## §4 — Sources (every external citation)

No external web sources were used.

Local sources read:

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-D-validated-invalidated.md`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md`
- `skinny/grammars/json.bbnf`
- `skinny/crates/ir/src/cost.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/crates/passes/src/diagnostics.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/mod.rs`
- `skinny/crates/codegen/src/lower/rust.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`

Commands run:

- `git status --short`
- `rg --files -g 'SKINNY-TRIUMVIRATE.md' -g 'SPEC.md' -g 'HANDOFF.md' -g '*W0*' -g '*consolidation*' restart skinny`
- `rg -n "CostFacts|cost_facts|cost facts|with-cost-facts|cost-facts|BBNF-COSTFACTS|costfacts" skinny/crates restart/skinny/tranches/sk-v8 -g '!target'`
- `rg -n "cost_facts|CostFacts|none:pre-W1|Strictness|strictness|comparator_|measured_validation|validate_sk_v8|sidecar|with-cost-facts|cost-facts" skinny/crates/bbnf-bench/src skinny/RESULTS.md skinny/REDRESS.md`
- `cargo xtask gate-json --with-cost-facts --advisory | jq '{schema, grammar, cost_facts_len: (.cost_facts | length), diagnostic_codes: ([.diagnostics[].code] | unique), sample_rule_0: (.cost_facts["0"] | {rule_id, chosen, rationale, priority_fired, rejected_len: (.rejected | length), rejected: .rejected})}'`
- `cargo xtask gate-json --with-cost-facts --advisory | jq -r '.cost_facts | to_entries[] | [.key, .value.chosen, .value.rationale, .value.priority_fired, ((.value.capacity_policy // {}) | tostring), ([.value.rejected[] | .shape + ":" + .reason + ":e" + (((.evidence // []) | length) | tostring)] | join(","))] | @tsv'`
- `cargo test -p passes cost_facts -- --nocapture`
