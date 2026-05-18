# SK-V8 Alpha-E Candidate Shortlist

Date: 2026-05-16

Scope: shortlist only. This file selects SK-V8 candidates from SK-V7 evidence,
with no source edits, no benchmark edits, and no commit.

Inputs read:
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v7/SPEC.md`
- `restart/skinny/tranches/sk-v7/HANDOFF.md`
- all SK-V7 W0 through W10c wave artifacts, with emphasis on W5 through W10c
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Selection boundaries:
- Lock 14 remains binding. Generic parser and pass infrastructure must not gain
  JSON semantic coupling. Per-grammar JSON inputs and generated JSON parser
  output remain allowed only where W8 already preserved that boundary.
- Lock 15 remains binding. Any generated parser growth must carry text-size and
  hot-leaf evidence, not only throughput evidence.
- REDRESS 82, 83, 84, 88, and 89 are rejected routes. They are not reopened
  unless the candidate below states a changed measurement or ownership framing.
- W2 mantissa-widen fallback, W4 quartet escape materializer, W5 StringBlock16
  wrapper, W6 object-pair value-byte carry, and W10/W10b default bitmap body
  fills are not shortlisted as-is.

## 1. Twitter yyjson residual fusion-quality retained parser refactor

Reason to shortlist: SK-V7 still leaves twitter retained parse far behind yyjson
default (`15752` Track 1 Mbps versus `30931` yyjson default Mbps in current
RESULTS). C6 showed yyjson winning with one dominant hot symbol and fused scalar
control, while bbnf remains split across generated value dispatch, scanner, and
cursor helpers. This is the most direct SK-V8 route for the documented yyjson
residual.

Owner paths:
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`

Scalar reference and checkasm status:
- No new SIMD or assembly is part of this candidate.
- Correctness reference is the existing scalar generated JSON parser plus JSON
  parity and gate-json suites.
- If the wave proposes any new byte primitive, it must be split out and pass a
  scalar reference plus checkasm before this candidate can consume it. PMULL,
  CTZ, and bulk bitmap bodies are explicitly out of scope here.

Same-wave consumer plan:
- The first consumer must be retained JSON parse for twitter, not a library-only
  helper and not a sidecar benchmark.
- Implement the fused path in both the runtime source and the codegen template,
  then regenerate or verify generated output in the same wave.
- Keep direct-to-struct and real-typed twitter rows as guards; do not route this
  candidate through direct materialization unless retained parse first shows the
  expected shape.
- Profile before and after the change and record hot-leaf movement in RESULTS or
  the wave artifact.

Falsifiability gate:
- `twitter parse_only`: Track 1 must be >= `20812` Mbps. This closes at least
  one third of the current yyjson gap (`15752` to `30931`) and reaches at least
  99.0 percent of the current sonic strict row (`21020`).
- `twitter parse_only`: Track 2 must be >= `12039` Mbps, no worse than -2.0
  percent from current `12285`.
- `twitter real_typed_struct`: Track 1 must remain >= `15486` Mbps, preserving
  the current sonic strict GO condition.
- Guard rows must have no Track 1 or Track 2 regression worse than -2.0 percent:
  `update_center parse_only`, `apache_builds parse_only`,
  `github_events parse_only`, `unicode_escapes parse_only`, `numbers parse_only`,
  and `citm_catalog parse_only`.
- Lock 15 check: generated hot text for the new dominant driver must either stay
  within +10.0 percent of the pre-wave generated parser text size or document a
  measured hot-leaf consolidation where no more than two leaves exceed 10 percent
  of twitter samples.
- The wave must not claim a yyjson beat unless `twitter parse_only` Track 1 is
  >= `30931` Mbps under the same RESULTS schema and provenance rules.

LOC budget:
- Up to 900 source LOC, including runtime/template parity and focused tests.
- Generated output and RESULTS refreshes do not count toward the source budget,
  but any generated churn must be byte-diff audited.

Risk:
- High. This touches the hot generated parser shape and can regress non-twitter
  retained rows or violate Lock 15 by spreading hot code.
- The most likely failure mode is W5/W6 in another form: a locally plausible
  helper increases call/branch pressure and loses whole-row throughput.

Revert protocol:
- Save the rejected patch as evidence under
  `restart/skinny/tranches/sk-v8/research/`, using the active wave naming
  pattern, before reverting source.
- Revert runtime/template/gate changes together so generated and source paths do
  not diverge.
- Restore pre-wave RESULTS values unless the wave keeps a separate rejected
  comparison table.
- Add a REDRESS entry naming the failed rows and thresholds.

Pre-blocked routes:
- Do not revive the W5 generated-retained `StringBlock16` wrapper.
- Do not revive W6 object-pair value-byte control compaction.
- Do not revive parse-that full-string scan widening, EventCursor, separator,
  function-pointer, generic SWAR, or sidecar-only routes.
- Do not use PMULL, CTZ, or bulk bitmap bodies as the explanation for a fusion
  parser win in this candidate.

## 2. RESULTS schema completion and sidecar freshness gate

Reason to shortlist: W0b admitted schema v3, but REDRESS 78 records remaining
telemetry gaps: `Delta vs SK-V6` is still `n/a`, many sidecar columns are stale
or absent, and hot-leaf fields remain unprofiled. SK-V8 needs better measurement
provenance before more residual work is judged.

Owner paths:
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md`
- optional new manifest path under `skinny/crates/bbnf-bench/` or
  `skinny/profile/`, if the wave needs checked-in sidecar provenance

Scalar reference and checkasm status:
- Not relevant. This candidate must not change parser, scanner, SIMD, or asm
  behavior.

Same-wave consumer plan:
- `gate-json` must consume the schema fields it emits. The schema is not complete
  if it only makes RESULTS look fuller.
- The wave should fail or mark rows explicitly when sidecar values lack corpus
  hash, binary/build provenance, hardware, and freshness metadata.
- The wave should refresh RESULTS only after adding a validation path that can
  reject malformed or stale telemetry.

Falsifiability gate:
- `Delta vs SK-V6` must be numeric for all rows with a matching SK-V6 baseline.
  Rows with no SK-V6 predecessor must use an explicit non-numeric reason such as
  `new-row`, not plain `n/a`.
- Populated competitor sidecar cells for `twitter parse_only`,
  `update_center parse_only`, `apache_builds parse_only`,
  `github_events parse_only`, `citm_catalog parse_only`, and
  `canada direct_to_struct` must have manifest coverage for corpus identity,
  binary identity, hardware, and run date.
- Hot-leaf coverage must be non-`unprofiled` for
  `twitter parse_only`, `update_center parse_only`,
  `apache_builds parse_only`, `github_events parse_only`,
  `unicode_escapes parse_only`, `y_string_unicode parse_only`,
  `citm_catalog parse_only`, and `instruments parse_only`.
- Parser throughput cells must not move by more than +/-1.0 percent in this
  wave. A larger movement means the wave is not telemetry-only and must be split.
- Gate output must reject at least one intentionally malformed sidecar manifest
  in a focused test.

LOC budget:
- Up to 450 source LOC plus RESULTS refresh.

Risk:
- Medium. The risk is schema churn that makes future waves incomparable, or a
  freshness rule that silently excludes too much old evidence.

Revert protocol:
- Revert report/gate/schema changes together.
- Restore the prior RESULTS schema if validation cannot be made reproducible.
- Keep a rejected-wave note with the exact rows that could not be populated.

Pre-blocked routes:
- Do not use telemetry completion to relabel stale sidecars as same-run anchors.
- Do not make parser or codegen changes in this wave.
- Do not treat sidecar-only competitor values as strict admission evidence
  unless the manifest marks them same-run under the gate's rules.

## 3. Remaining Lock 14 template-residue boundary audit and relocation

Reason to shortlist: W7 and W8 removed public parse-that JSON APIs, pass-level
JSON coupling, `StructuralAlphabet::json`, and codegen shell leakage. Remaining
JSON names appear intentionally confined to grammar inputs, templates, tests, and
emitted JSON parser output. SK-V8 should either close that as an audited no-op or
move the remaining template residency behind an explicit per-grammar boundary.

Owner paths:
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/json_templates/value.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/visitor.rs`
- `skinny/crates/bbnf/src/lib.rs`
- `skinny/crates/passes/src/lib.rs`
- `skinny/xtask/src/main.rs`

Scalar reference and checkasm status:
- Not relevant. This is a boundary and generated-output stability candidate.

Same-wave consumer plan:
- If residue is real, move it to a per-grammar template provider or manifest
  consumed by the existing codegen path in the same wave.
- If audit proves no production generic residue remains, land only the audit
  artifact and a grep-backed gate update. Do not churn files to produce motion.
- Generated JSON parser output must remain byte-identical unless the wave is
  explicitly split into a parser behavior wave.

Falsifiability gate:
- Production generic code grep must have zero hits for `json_templates`,
  `include_str!("json_templates`, and emitted `JsonObject` or `JsonArray` names
  outside an allowed per-grammar provider, tests, grammar inputs, or generated
  JSON parser output.
- `skinny/RESULTS.md` must have zero throughput-cell changes.
- Generated JSON runtime output must be byte-identical before and after the
  wave.
- Existing Lock 14 checks from W7/W8 must still pass, including absence of
  `StructuralAlphabet::json` and public parse-that JSON APIs.

LOC budget:
- Up to 500 source LOC or file-move metadata.

Risk:
- Medium. File movement can create broad, low-value churn or accidentally alter
  generated output.

Revert protocol:
- Revert relocation and manifest changes as one slice.
- If only the audit lands and later proves incomplete, revert the gate update and
  keep the evidence table for the corrected wave.

Pre-blocked routes:
- Do not reopen JSON-special semantic passes in `passes`.
- Do not reintroduce `StructuralAlphabet::json`, `skip_json`, `match_json`,
  `unescape_json`, `StrictJson`, or public parse-that JSON APIs.
- Do not hide JSON coupling behind renamed generic helpers.

## 4. Bitmap asm bodies under a changed density-gated measurement framing

Reason to shortlist: W10 proved AArch64 PMULL prefix-XOR correctness and W10b
proved CTZ/bulk correctness, but both failed whole-report throughput when wired
as production defaults. SK-V8 may revisit the bodies only if it stops treating
them as default JSON parser wins and first measures a density-gated envelope.

Owner paths:
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/bbnf-bench/benches/simd_scan.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`

Scalar reference and checkasm status:
- Scalar references and checkasm coverage exist for prefix-XOR, next-set-bit, and
  bulk emit from W10/W10b, and B6 stack-canary Stage 1 was admitted in W10c.
- Any changed asm body, changed predicate, or changed fallback path must rerun
  the scalar reference checks and checkasm before runtime wiring.

Same-wave consumer plan:
- The first consumer is a density-gated scan experiment with explicit fallback to
  the scalar body, not a replacement default for all JSON rows.
- The wave must record stripe density or another named predicate that explains
  why the body is selected for some rows and rejected for W10/W10b falsifiers.
- Production wiring is allowed only if the same wave proves the predicate in
  both the primitive scan benchmark and full JSON RESULTS rows.

Falsifiability gate:
- Exact W10/W10b default rewires are forbidden. A grep or config check must show
  the scalar body remains the default for rows outside the predicate.
- Target row: either `twitter parse_only` Track 1 must be >= `16224` Mbps
  (+3.0 percent from current `15752`) or `apache_builds parse_only` Track 1 must
  be >= `12856` Mbps (+3.0 percent from current `12482`).
- Prior falsifier rows must have no Track 1 or Track 2 regression worse than
  -1.0 percent: `instruments parse_only`, `numbers parse_only`,
  `unicode_escapes parse_only`, `canada parse_only`, `citm_catalog parse_only`,
  `marine_ik parse_only`, and `mesh parse_only`.
- Primitive scan bench rows must show at least one selected-density case with
  >= +5.0 percent improvement and at least one rejected-density case proving the
  scalar fallback path is used.
- If the target parse row does not improve, the asm body may remain as a
  benchmark-only artifact but must not be admitted as production parser code.

LOC budget:
- Up to 500 source LOC, including predicate plumbing, checkasm refresh, and
  focused bench/gate support.

Risk:
- High. W10 and W10b already showed that correct asm can lose whole-report JSON
  throughput. The predicate can also overfit one corpus.

Revert protocol:
- Revert runtime selection, predicate plumbing, and bench/gate changes together.
- Keep scalar reference and checkasm-only hardening only if it is behavior-neutral
  and separately justified.
- Add REDRESS evidence naming both the target row and the falsifier rows.

Pre-blocked routes:
- Do not re-land PMULL prefix-XOR as the unconditional `bitmap_prefix_xor_64`
  hot path.
- Do not re-land CTZ/bulk consumers as unconditional JSON scan consumers.
- Do not use correctness/checkasm alone as admission evidence.
- Do not mask regressions by dropping W10/W10b falsifier rows from the gate.
