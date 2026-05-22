# SK-V13 W9 CHALLENGE - Same-Substrate Union Material Differential

Cycle: W9 CHALLENGE. Disposition: ACCEPT WITH CONSTRAINTS.

The six-lens CHALLENGE accepted the C1 material-differential direction but
revised the plan's implementation breadth. Redress may not start from the
unrevised dual JSON/CSS plan. The accepted redress route is CSS-only C1:
`css_l4/declaration_values_extended/direct_to_struct/main` is the production
consumer; JSON is guard-only for W9.

## CH1 Correctness

ACCEPT.

C1 is correctness-admissible because the projection remains private generated
grammar data and uses the existing tape or fact stream as the only retained
substrate. It does not introduce a sidecar or reorder the production
substrate.

Redress constraints:

- Prove exact output equivalence, not just projection equality.
- For CSS, preserve declaration/token order, indexes, depth, byte spans,
  normalized lexemes, and fact-stream framing.
- The retained W9 artifact must include reference and candidate output hashes
  for the production consumer.
- A microbench or projection hash alone is not row evidence.
- Reject if public `JsonSink`, `UnionTape`, retained `StructuralIndex`,
  sidecar/vector/cursor, or microbench-only admission appears.

## CH2 Generality / Lock 14

ACCEPT WITH CONSTRAINTS.

The plan is genericity-sound only if C1 remains generated-private and does not
expand public runtime/codegen shape.

Redress constraints:

- Add `SK_V13_W9_OWNER_PATHS`, extend `current_lock14_owner_paths`, and add
  parent-diff authorization for `sk-v13-waveW9` and
  `sk-v13-wave9-challenge`.
- Add Lock 14 tests for W9 owner acceptance.
- Add generic scan/token tests rejecting blocked union route names:
  `UnionTape`, class column/lane, retained `StructuralIndex`, sidecar vector,
  parser-owned cursor/list, aux table, and second scan.
- Keep JSON policy tokens inside JSON-owned runtime/templates. JSON is
  guard-only in W9 redress.
- Do not touch `skinny/crates/bbnf-simd/`; doing so would promote W9 to C3.
- Do not expand `BackendShape`, BIR, directives, or public substrate API.

## CH3 Regression / REDRESS

ACCEPT WITH CONSTRAINTS.

Redress constraints:

- Full guard refresh is mandatory: all 51 JSON rows and all 24 CSS rows from
  `restart/skinny/ROLLING-SOTA-DELTA.md`.
- No admitted row may silently demote.
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` may change
  only if a row actually moves or its recorded disposition changes.
- Measured-block close is valid only through a machine-consumed W9 report with
  C1 variant id, REDRESS 96/97/98 citations, forbidden-route absence,
  one-substrate proof, named CSS production consumer, before/after Mbps,
  strict equality, JSON/CSS guard state, artifact hash, affected rows, and
  block id
  `JSON-CSS-W9-SAME-SUBSTRATE-UNION-CONSUMED-BUT-NO-ROW-MOVEMENT`.
- If behavior source is edited but no legal row movement survives, revert the
  behavior source unless the retained slice is gate/report/Lock14 evidence
  needed for machine consumption. Save `/tmp/skv13-waveW9-rejected.patch` and
  record measurements, guard state, material differential, and patch path in
  REDRESS.

REDRESS 96/97/98 routes remain blocked except for the named C1 material
differential. REDRESS 126 remains microbench-only history, not SIMD
permission.

## CH4 Cost

REVISE -> ACCEPT WITH CONSTRAINTS.

The unrevised plan was too elastic for the W9 `45 + 15` cap because it left
both JSON retained/tape and CSS fact-stream implementations live. The accepted
route pre-selects one consumer before redress:

- Primary consumer:
  `css_l4/declaration_values_extended/direct_to_struct/main`.
- Primary source scope:
  CSS declaration-values-extended config/template, generated scanner, and fact
  sink only.
- JSON parser/tape is guard-only. Do not touch JSON parser/tape in W9 except
  tests, reporting, or gate evidence.
- `bbnf-simd` is read-only.
- Target source/test LOC after revision: roughly 300-450, mostly W8-style
  report/gate/xtask/Lock14 plumbing plus a small CSS projection helper.
- No row-search loop during redress. Measure the one CSS consumer plus chained
  W5-W8/W9 gate evidence.

Shortest acceptable implementation route: CSS-only C1, generated-private
projection metadata in CSS config/templates, consumed by the existing
fact-stream scanner without changing public output semantics.

## CH5 Hidden Coupling

ACCEPT WITH CONSTRAINTS.

Redress constraints:

- Do not change the public `JsonSink` trait.
- Do not make `attach_structural_index` retain or consume `StructuralIndex`;
  JSON retained/tape is guard-only for W9.
- CSS projection data must remain private generated/config policy or existing
  fact-stream consumption.
- Do not add a sidecar vector or serialize new output fields unless strict CSS
  equality still passes.
- Preserve W8 neutral flag ownership: no `HAS_ESC` / `HAS_CONTROL` revival,
  no generic `GrammarConfig`, no generic JSON policy in generic crates.
- Preserve W5-W7 fail-closed decision-engine behavior.
- Runtime generated files and codegen templates must remain reproducible
  together.

## CH6 Anti-Paper-Close

ACCEPT WITH CONSTRAINTS.

The measured-block path is honest only if same-wave consumer evidence is
concrete.

Redress constraints:

- W9 report must name the live CSS production row and path, then prove strict
  equality plus before/after Mbps for that row.
- Static captured rows, W8 policy consumption, microbench-only output, and
  future W11/W12/W14 consumers are paper-close.
- If behavior source is retained, it must include the live consumer call path.
- If behavior source is reverted and W9 closes as architectural block, the
  report must include rejected patch path/hash or an equivalent retained fact
  artifact proving the attempted consumer was real.
- Reject if redress requires any public substrate API, `UnionTape`, new
  `BackendShape`/BIR/directive, sidecar/class-column/retained-index route,
  generic JSON policy, SIMD touch without same-wave zero-orphan/checkasm/
  scalar-reference closure, guard regression, or postponed consumer.

## Accepted Redress Contract

- Implement only CSS-only C1 for W9.
- Add generated-private projection metadata in
  `css_l4_declaration_values_extended` runtime and template config.
- Consume that metadata from the live generated CSS scanner/fact-stream path.
- Keep JSON parser/tape, `bbnf-simd`, `BackendShape`, BIR, directives, public
  substrate APIs, and public `JsonSink` unchanged.
- Add W9 companion report/gate/xtask support with schema
  `sk-v13-same-substrate-union-v1`, flag
  `--skv13-same-substrate-union-report`, and gate print
  `G-W9-SAME-SUBSTRATE-UNION <status> <path>`.
- Add W9 Lock 14 owner paths, parent-diff authorization, and blocked-route
  token tests.
- Produce retained W9 facts/report under
  `restart/skinny/tranches/sk-v13/research/w9/`.
- Run targeted CSS equality/reproducibility tests, W9 report/gate/xtask tests,
  Lock 14 tests, and final advisory `gate-json` chaining W5/W6/W7/W8/W9.
- Append REDRESS-140. Update RESULTS/rolling delta only if the CSS row moves
  or its disposition changes.
