# SK-V13 W9 Research - Same-Substrate Union Material Differential

Date: 2026-05-22.
Wave: W9, SPEC Section 13.
Scope: one fresh union material-differential attempt after W8 policy close.
Disposition: research cohort converged; plan phase may proceed.

## Cohort

- Mendel (`019e4f23-12c7-7e00-bf96-aa4f20363be3`) audited CH1 correctness,
  W9 gate authority, REDRESS 96/97/98/126, and blocked union routes.
- Wegener (`019e4f23-2191-7341-a827-5e6ad8b6eddf`) audited CH2 generality,
  Lock 14, W8 policy surfaces, and JSON-only leak risks.
- Mill (`019e4f23-310b-7222-ab32-ffb0dfea3ff8`) audited CH3 regression
  floors, JSON/CSS guards, and report-chain requirements.
- Goodall (`019e4f23-404b-7551-be1f-45d7a8318173`) audited CH4 cost,
  micro-prove state, and C1/C2/C3 implementation budgets.
- Anscombe (`019e4f23-7a18-75e3-afc5-fff123fea452`) audited CH5 hidden
  coupling and concrete JSON/CSS/tape integration sites.
- Bernoulli (`019e4f23-91ed-7fa0-8ec4-cc1148e238e0`) audited CH6
  anti-paper-close, row movement definitions, and measured-block artifacts.
- Local scout reconciled SPEC Section 13, DISPATCH-PROMPT W9 clauses,
  P3-C/P3-E union gates, scoping union candidates, and current source.

## Binding Gate

W9 is SPEC Section 13: `Same-Substrate Union Material Differential`
(`SPEC.md:715`). Entry requires W0 and W8 admitted or routed, CHALLENGE
acceptance of the material differential against REDRESS 96/97/98, and exactly
one named variant: C1 codegen-private per-rule projection, C2 e-graph selected
shape, or C3 SIMD-first mask-to-tape writer (`SPEC.md:727`-`:733`).

The exit gate is narrow:

1. One union variant admits with strict row movement, or records an
   architectural block with intrinsic evidence.
2. No class column, retained structural index, parser-owned cursor/list, aux
   table, sidecar vector, second scan, or public `UnionTape` survives.
3. Full JSON/CSS guard maintain holds.
4. If W9 touches `skinny/crates/bbnf-simd/` or selects C3, same-wave exit also
   requires `orphan_count_after = 0`, strict checkasm, scalar-reference
   status, delete/demote/revert protocol, and production consumer row
   evidence (`SPEC.md:741`-`:751`).

P3-C names the admissible gate labels:

- `union-c1-per-rule-same-tape`.
- `union-c2-egraph-shape`.
- `union-c3-simd-mask-to-tape`.

## Owner Paths

SPEC owner paths:

- `skinny/crates/runtime/src/tape/`.
- `skinny/crates/runtime/src/grammars/json/`.
- `skinny/crates/runtime/src/grammars/css_l4_*`.
- `skinny/crates/codegen/src/`.
- `skinny/crates/passes/src/`.
- `skinny/crates/bbnf-simd/` only if C3/SIMD-first is selected.
- `skinny/crates/bbnf-bench/`.

W9 report/gate work will necessarily share `bbnf-bench/src/report.rs`,
`bbnf-bench/src/bin/gate.rs`, `bbnf-bench/src/lock14_baseline.rs`, and
`xtask/src/main.rs`, following the W5-W8 companion-report pattern.

## Preblocked Routes

W9 inherits REDRESS 50, 51, 53, 88, 89, 92, 96, 97, 98, and 126.
REDRESS 96/97/98 are `REOPEN-CONDITIONAL`, not category blockers, but exact
route replays stay blocked (`SPEC.md:981`):

- REDRESS 96: class-column plus move-consumed structural-index substrate.
- REDRESS 97: allocation-free streaming cursor substrate.
- REDRESS 98: class-lane-only / retired `G-W3-UNION-SUBSTRATE` thesis.
- REDRESS 126: routed ASM microbench and orphan demotion evidence; not
  production SIMD admission and not permission to keep new orphans.

No W9 plan may retain a class column, parser-local cursor/list, sidecar event
vector, aux density table, `StructuralIndex` sidecar, second source scan,
scalar-delegate body, or public `UnionTape`.

## Candidate Comparison

The cohort converged on C1 as the first W9 route.

| Candidate | Research disposition |
|---|---|
| C1 codegen-private per-rule same-tape projection | Preferred. Smallest legal material differential, estimated roughly 250-340 LOC including report/gate and micro-prove artifacts. Stays in generated grammar modules and existing tape/fact-stream storage. Avoids public API, new `BackendShape`, new BIR/directive, sidecars, and SIMD zero-orphan scope. |
| C2 e-graph equivalence-class shape selection | Legal but larger and compile-cost risky. Must prove structural equivalence and bounded resolver cost, and must preserve W5-W7 fail-closed behavior. Better reserved for a later decision-engine wave unless C1 proves too weak and the plan can show a row-specific need. |
| C3 SIMD-first mask-to-tape writer | Highest risk. Touches `bbnf-simd`, PMULL/CSSC/CTZ route history, checkasm, scalar reference, and same-wave zero-orphan closure. It is a W12-grade route unless W9 explicitly accepts the full SIMD exit predicates. |

## C1 Material Differential

C1 is materially distinct from REDRESS 96/97/98 because it is generated,
grammar-local, and compile-time selected:

- It does not add a retained class column or side vector.
- It does not stream a second cursor or route through two runtime substrate
  types.
- It does not paper-close on a class-lane proof.
- It treats the existing `Tape`/`ValueRef` or CSS fact stream as the only
  retained substrate.
- Tuple/schema meaning remains private generated grammar data, not generic
  JSON policy and not a public substrate API.

The safest plan statement is: union schema is grammar data emitted into
private generated modules; generic runtime storage remains the existing tape
or fact stream; no public substrate type, new `BackendShape`, or JSON policy
enters generic crates.

## Integration Sites

JSON retained/tape path:

- `runtime/src/tape/mod.rs` provides one `Tape` with offsets, sparse flags,
  payloads, and grammar-marked `ValueRef`.
- `runtime/src/tape/assembler.rs` provides admissible `push_offset` and
  `patch_flags` write hooks.
- `runtime/src/grammars/json/parser.rs` exposes `emit_plain_offset`,
  `patch_flags`, and the current no-op `attach_structural_index` call.
- `runtime/src/grammars/json/generated.rs` contains emission sites for string
  quotes, structural bytes, numbers, literals, and container close offsets.

JSON direct path:

- Direct rows do not consume tape flags. Generated direct hooks and `JsonSink`
  calls are possible consumers, but W9 must not change public `JsonSink`
  required methods or create generic JSON sink acceleration.

CSS L4 fact-stream path:

- For CSS, the existing substrate is the generated fact stream, not JSON tape.
- `css_l4_declaration_values_extended/generated.rs` and `sink.rs` are live
  scanner/sink consumers already centralized through W8 config policy.
- Static-captured CSS rows are guard rows only unless the plan names a live
  generated consumer.

## Guard And Report Requirements

W9 must maintain the full JSON and CSS guard tables:

- JSON guard universe: all 51 rows in `restart/skinny/ROLLING-SOTA-DELTA.md`.
  Admitted rows must not silently demote; open direct/typed/parse rows remain
  open unless a strict same-plane result admits them.
- CSS guard universe: all 24 CSS L4 rows currently admitted above
  `lightningcss + 1 Mbps`; all must preserve strict equality and throughput
  status.

Existing reports to chain in final advisory gate-json:

- W5: `--skv13-decision-regex-report`.
- W6: `--skv13-decision-active-cost-report`.
- W7: `--skv13-decision-csp-cascade-report`.
- W8: `--skv13-per-grammar-policy-report`.

W9 must add its own companion report and xtask passthrough. Recommended
report schema:

- Schema: `sk-v13-same-substrate-union-v1`.
- Flag: `--skv13-same-substrate-union-report`.
- Gate print: `G-W9-SAME-SUBSTRATE-UNION <row_move_toward_sota_status> <path>`.
- Measured block id:
  `JSON-CSS-W9-SAME-SUBSTRATE-UNION-CONSUMED-BUT-NO-ROW-MOVEMENT`.

Required fields include variant id, REDRESS 96/97/98 citations, material
differential, forbidden-route absence, substrate cardinality, named consumer
rows, before/after Mbps, strict equality, JSON/CSS guard states, Lock 14 state,
artifact path/hash, affected row ids, REDRESS entry, and SIMD zero-orphan /
checkasm fields only if C3 or `bbnf-simd` is selected.

## Micro-Prove State

No existing isolated same-host microbench proves C1. Current harnesses measure
adjacent surfaces only: SIMD scan positions, JSON production rows, and prior
ASCII set membership. C1 needs a small W9-owned probe:

- Reference: current fixed tape/fact-stream projection.
- Candidate: generated per-rule same-tape projection.
- Metrics: ns/event or ns/byte, offset/fact-stream hash equality, strict row
  result, and before/after row Mbps.
- Consumer: a named JSON retained/direct row or a live CSS generated row.

A positive microbench alone is not admission. It is entry evidence for redress;
the exit gate still requires row movement or architectural block.

## Anti-Paper-Close Rule

W9 paper-closes if it lands "union exists" without a production row consumer,
if it relies on W8 policy consumption, if it uses only checkasm/microbench or
telemetry evidence, or if it defers C3 orphan cleanup to W12. No
`RESULTS.md` or rolling delta update is allowed unless a row actually moves or
admits.

If no row moves, W9 may still close the union obligation by recording the
machine-consumed measured architectural block above, with retained facts,
report validation, full guard maintain, and REDRESS evidence.

## Selected Research Route

Plan phase should select C1 unless it discovers a concrete row-specific reason
that C2 or C3 is necessary and accepts their larger gates. The recommended W9
plan is:

1. Implement only a generated-private C1 same-tape/fact-stream projection.
2. Keep generic tape storage, `ValueRef`, `BackendShape`, BIR, and directives
   unchanged.
3. Add a C1 micro-prove artifact and W9 gate report.
4. Name one JSON or CSS production row consumer before redress.
5. Preserve all JSON/CSS guards and chain W5-W8 report evidence.
6. Admit only on strict row movement; otherwise record the measured
   architectural block with REDRESS-140.

## Minimum Validation Inputs

- Targeted unit/parity tests for the selected C1 projection.
- Targeted JSON or CSS row equality tests for the named consumer.
- `cargo test -p bbnf-bench skv13_same_substrate_union_report -- --nocapture`.
- `cargo test -p bbnf-bench --bin gate skv13_same_substrate_union_report -- --nocapture`.
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_same_substrate_union_report_flag -- --nocapture`.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`.
- Final advisory `gate-json` chaining W5/W6/W7/W8/W9 evidence.
