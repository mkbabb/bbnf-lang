# SK-V12 SPEC - Pin-Aware S-P3 Wave Plan

Date: 2026-05-20.
Status: S-P3 CONVERGED. This packet supersedes the pre-pin V5 SPEC wherever it
conflicts with `USER-PIN-W1-CSS-L4-SOTA.md`.

Authority:

1. `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
2. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v12/HANDOFF.md`
4. `restart/prompts/ORCHESTRATOR.md`
5. `restart/prompts/skinny/PASS-1-PROFILE.md`
6. `restart/prompts/skinny/PASS-2-RESEARCH.md`
7. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
8. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
9. `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
10. `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
11. The six accepted S-P2 reports under `restart/skinny/tranches/sk-v12/research/p2/`
12. The six 2026-05-20 audits under `restart/skinny/tranches/sk-v12/research/`
13. `skinny/RESULTS.md`
14. `skinny/REDRESS.md`

Dispatch lock:

- S-P3 has converged under the user pin; W0 is the first dispatchable wave.
- W1a/W2/W1b-1/W1b-2a/W1b-2b/W3/W4/W5 dispatch only after their entry
  gates pass.
- Each wave is executed by the per-wave triumvirate: research, plan,
  CHALLENGE when required, and redress in distinct commits.

## Section 0 - Close Condition And Goalset

### Section 0.1 - Global Close Condition

SK-V12 closes by ADMIT or FIXPOINT. No other close shape is valid.

ADMIT requires all of the following:

1. A generated CSS L4 row exists and is measured on Track 1.
2. Track 1 throughput is strictly greater than `lightningcss_mbps + 1` on the
   same corpus, same output plane, same host, and strict equality semantics.
   Equality at `lightningcss_mbps + 1` is a miss.
3. Generated Track 1, independent oracle/Track 2, and lightningcss emit the
   same canonical CSS fact stream, and the equality proof is gate-consumed.
4. The gate consumes generated source/runtime provenance, grammar and input
   checksums, oracle path, lightningcss command/artifact, run id, host,
   feature mask, build flags, sample count, Track 1 Mbps, oracle Mbps,
   lightningcss Mbps, generated LOC and module size, JSON guard state, wave id,
   and REDRESS id.
5. Lock 14 is clean: the generic JSON leaks identified in
   `skv12-value-api-audit.md` are resolved through `GrammarConfig` or an
   equivalent generated metadata surface before CSS L4 emission is legal.
6. Lock 16 is clean for any SIMD/ASM admission: scalar reference,
   checkasm/parity, same-host micro-proof, same-wave consumer, and corpus
   parity where applicable. The `escape_mask_64` xorshift falsifier
   `0xCAFEF00DBAADF00D` is resolved before any new SIMD/ASM admission.
7. The aarch64 orphan set is zero by admission, removal, or inventory demotion
   with evidence: `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
   `bulk_emit_positions_64`, `byte_context`, and `cache_hints`.
8. JSON direct and typed guard floors hold, or a miss is recorded as a measured
   REDRESS demotion. `parse_only` remains diagnostic-only.
9. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SYNTHESIS.md`, `HANDOFF.md`,
   this SPEC, and `DISPATCH-PROMPT.md` agree.

FIXPOINT requires all of the following:

1. CSS L4 has at least one measured redress attempt under this pin. Sheets and
   BBNF-self are not considered before that CSS attempt records BLOCKED or
   REJECTED evidence.
2. ADMIT is measured uncloseable in the closing tranche.
3. A new union-substrate implementation attempt is recorded in REDRESS with
   fresh profile or microbench evidence, strict equality/parity evidence,
   same-wave consumer, CHALLENGE acceptance, and material differential from
   REDRESS 96/97/98.
4. A new ASM-gen implementation attempt is recorded in REDRESS with scalar
   reference, checkasm/parity, same-host microbench, same-wave consumer,
   CHALLENGE acceptance, and material differential from REDRESS 88/89/90 when
   adjacent.
5. The aarch64 orphan set is zero by admission, removal, or explicit inventory
   demotion with evidence. Production orphans make FIXPOINT invalid.
6. REDRESS names every measured miss and the routed remainder for SK-V13 if
   the campaign continues.

The old close formulas `>= 1 Mbps` and `ceil(baseline_mbps * 1.01)` are not
CSS close bars. A measurable CSS baseline below lightningcss is useful
evidence, not admission.

### Section 0.2 - Comparator Classes

| Class | Examples | Admission use |
|---|---|---|
| Same-run strict anchor | lightningcss for CSS L4 fact stream; sonic-rs strict for JSON guard rows where output plane matches | May support admission only when same corpus, output plane, strictness, run id, and equality are gate-consumed. |
| Same-run independent oracle | generated Track 2 or hand oracle that is structurally independent of Track 1 | Required for CSS ADMIT; cannot be the same generated implementation under another name. |
| Same-run flaw probe | permissive/unchecked APIs, lossy modes, stale sidecars | Planning only; never admission. |
| Historical planning signal | simdjson/asmjson/yyjson literature and old SK-V rows | Planning only until refreshed under same-run strict rules. |

### Section 0.3 - Outcome Enum

The SK-V12 packet uses the existing outcome set only:

```text
A
C
G
I
J
K
L
M
N-direct
S
```

No wave may add an outcome variant. CSS rows must use the existing schema
discipline and gate status fields; `parse_only` remains `S / NO-GO`.

### Section 0.4 - Required Telemetry

Every new or refreshed CSS/non-JSON row must carry and gate-consume:

```text
schema_id
row_id
grammar_id
domain
corpus_or_workload
output_plane
strictness
outcome_id
verdict
generated_track1_source_path
generated_runtime_path
grammar_checksum
input_checksum
input_bytes
track1_mbps
track2_or_oracle_mbps
lightningcss_mbps
threshold_mbps
strict_output_equality
track2_or_oracle_source_path
track2_independence_status
lightningcss_command
lightningcss_artifact
measured_validation_path
run_id
host_triple
feature_mask
build_flags
sample_count
sample_cost
benchmark_artifact_path
profile_artifact
generated_loc
generated_module_bytes
grammar_size_guard
lock14_status
lock16_status
same_wave_consumer_class
scalar_reference_status
checkasm_or_parity_status
json_guard_state
gate_status
wave_id
redress_entry
```

Every emitted field must be consumed by `gate-json` or the named CSS/non-JSON
gate in the same wave. Missing lightningcss evidence, missing independent
oracle, stale run id, producer-only telemetry, unsupported outcome, generic
policy leak, parse-only admission, or orphan SIMD primitive rejects the wave.

### Section 0.5 - Opening Goalset

Seed state from SK-V11 close and pin-aware S-P1/S-P2:

| Family | Current state | SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | Diagnostic only. |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | JSON guard and routed ledger. |
| `real_typed_struct` | 7 `A / GO` | JSON typed guard surface. |
| CSS L4 generated parser | no admitted row | Authoritative first target. |
| Sheets / BBNF-self | no admitted row | Fallback only after measured CSS redress attempt. |
| Overall | `N-direct / NoGo` | Seed outcome. |

JSON direct guard floors:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

JSON typed guard floors:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Any wave that changes generic runtime, codegen, generated output, benchmark,
report, gate, parser, scanner, or SIMD paths capable of producing JSON must
rerun JSON guards or prove no JSON-producing path moved and `skinny/RESULTS.md`
is unchanged. Guard misses require REDRESS disposition.

## Section 1 - Non-Negotiables

- CSS L4 is the first target. Sheets and BBNF-self are fallback-only after a
  measured CSS redress attempt.
- CSS admission requires `track1_mbps > lightningcss_mbps + 1`; no baseline
  existence floor or 1% self-improvement floor closes SK-V12.
- No `parse_only` SOTA admission.
- No new directive, BIR variant, `BackendShape` variant, public substrate API,
  parser-owned sidecar, decoded-byte sidecar, hidden host schema, or x86
  implementation target.
- No generic-crate JSON policy. Grammar-specific policy must come from
  generated modules or generated metadata such as `GrammarConfig`.
- No primitive, SIMD/ASM kernel, parse-that helper, generated path, substrate,
  or output-plane contract without scalar reference, parity/checkasm where
  applicable, same-host micro-proof, and same-wave consumer.
- No SIMD/ASM admission before the `escape_mask_64` correctness gate passes.
- No orphan production aarch64 primitive at close.
- No strict admission from permissive, lossy, stale, sidecar-only, historical,
  or output-plane-mismatched comparator evidence.
- Research, plan, CHALLENGE when required, redress, and close remain distinct.
- Every miss becomes REDRESS evidence or an explicit routed residual.
- No wave closes on "wired", "integrated", "future consumer", or any other
  future-phase promise.

## Section 2 - Wave Manifest, Caps, And Reruns

| Wave | Section | Name | Dispatch status | Source/edit LOC budget | Risk | Wall/redress cap |
|---|---|---|---|---:|---|---:|
| W0 | Section 3 | Pin Telemetry And Gate Revalidation | Dispatchable after S-P3 convergence | <=160 docs/gate/test; 0 behavior | medium | <=30 min |
| W1a | Section 4 | GrammarConfig + Lock 14 Legality Gate | Conditional on W0 close | <=360 hand; generated output named separately | high | <=30 min |
| W2 | Section 5 | `escape_mask_64` Correctness Prerequisite | Conditional on W1a close; blocks SIMD admission | <=180 hand/test | high | <=30 min |
| W1b-1 | Section 6 | CSS L4 Generated Track 1 + Independent Oracle Scaffold | Conditional on W1a close; scalar-only unless W2 PASS | <=360 hand; generated output named separately | high | <=30 min |
| W1b-2a | Section 7.1 | CSS L4 Lightningcss Comparator + Criterion Row | Conditional on W1b-1 close | <=220 hand/test; generated output named separately | high | <=30 min |
| W1b-2b | Section 7.2 | CSS L4 Lightningcss SOTA Report + Admission Gate | Conditional on W1b-2a close | <=330 report/gate/test (CHALLENGE V2 budget correction) | high | <=30 min |
| W3 | Section 8 | CSS-Local Same-Tape Union Attempt | Conditional on W1b-2b measured CSS row + CHALLENGE | <=420 hand; generated output named separately | high | <=30 min |
| W4 | Section 9 | ASM-Gen CSS Consumer + AArch64 Orphan Disposition | Conditional on W1b-2b + W2 + CHALLENGE | <=430 hand/test/gate | high | <=30 min |
| W5 | Section 10 | Close And Alpha Feedback | Conditional on W0, W1a, W2, W1b-1, W1b-2a, W1b-2b, W4, and conditional W3 disposition | <=140 docs/report/gate; 0 behavior | medium | <=30 min |

Phase caps for the pinned campaign are tighter than earlier SK-V9/SK-V10
packets: 20 min research, 15 min plan, 30 min redress. CHALLENGE remains
mandatory for first-of-class, primitive, generic-crate, union, ASM-gen, and
high-risk waves. W1a, W2, W1b-1, W1b-2a, W1b-2b, W3, and W4 are
CHALLENGE-mandatory.

At 0.9x cap the agent commits or records the blocking state. At the cap it
halts. A wave may fail honestly; failure records REDRESS evidence and the next
wave starts fresh unless the dependency gate is impossible.

## Section 2.1 - Generality, Lock 14, And Generated Size

Every generic-crate edit must pass the Lock 14 gate:

1. No `JsonParser`, `CssL4Parser`, `GoogleSheetsParser`, `BbnfBootstrap`, or
   grammar-name branch in a generic crate.
2. No JSON structural alphabet, JSON string escape, JSON number policy, JSON
   object key policy, JSON `OffsetFlags` meaning, or `JsonSink` method shape in
   generic code.
3. Per-grammar generated modules own structural alphabets, FIRST/follow tables,
   escape policy, number policy, flag semantics, sink/view/kind wrappers, and
   output facts.
4. CSS L4 must be exercised by benchmark/equality. Prose generality is not
   enough.
5. Generated size is tracked: hand LOC, generated LOC, module byte size, grammar
   source size, and O(N) growth guard. Overflow blocks the wave until the growth
   source is traced.

## Section 3 - W0 Pin Telemetry And Gate Revalidation

Purpose: revalidate the pin-aware profile, W0 gate/report surface, and JSON
seed state without behavior changes.

Owner paths:

- `restart/skinny/tranches/sk-v12/research/`
- `restart/skinny/tranches/sk-v12/research/p1/`
- `skinny/RESULTS.md` only if the gate records unchanged state or measured
  disposition
- `skinny/REDRESS.md`

Entry gate:

- S-P3 has converged under the user pin.
- Worktree slice is clean or unrelated dirty state is isolated.

Tasks:

- Reconcile pin S-P1 authority: `/tmp/skv12-pin-p1`,
  `skv12-p1-pin-replay.tsv`, PMU/xctrace/samply status, and W0 lock.
- Verify W0 telemetry/gate lock at commit `f788eb97` is still the accepted
  surface, or record drift.
- Verify JSON seed rows and guard floors are still rendered.
- Do not edit parser, scanner, SIMD, codegen behavior, generated runtime output,
  or benchmark behavior.

Exit gate `G-W0-PIN-TELEMETRY`:

- PASS: artifacts exist, run ids are current, JSON seed state is reconciled, no
  behavior/source drift.
- REVISE: missing profile or gate artifact.
- FAIL/BLOCKED: W0 lock drift changes result semantics; return to S-P3.

Revert protocol: revert W0 docs/gate/report edits; no behavior patch exists.

## Section 4 - W1a GrammarConfig + Lock 14 Legality Gate

Purpose: make CSS L4 emission legal before any CSS generated parser is emitted.

Owner paths:

- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/`
- `skinny/crates/ir/src/` only for generated metadata types if required
- generated JSON modules only as regen output
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, gate/report scripts as needed

Entry gate:

- W0 PASS.
- Plan names the seven Lock-14 leaks from `skv12-value-api-audit.md`.
- Plan names JSON guard rerun or no-touch proof.

Tasks:

- Introduce `GrammarConfig` or equivalent generated metadata surface for
  structural alphabet, FIRST/follow tables, layout/trivia, escape policy,
  number policy, flag semantics, and sink/view/kind bindings.
- Move JSON policy out of generic code into JSON generated metadata/templates.
- Add Lock 14 scan/gate consumer for generic crates.
- Preserve JSON generated parity and guard floors.

Exit gate `G-W1a-GRAMMARCONFIG-LOCK14`:

- Generic-crate scan passes.
- JSON generated output passes parity and guard floors, or demotion is recorded.
- No CSS parser row is claimed yet.
- No new directive, BIR variant, `BackendShape`, or public substrate API.

Revert protocol: revert generic/template/config changes and generated output;
save rejected patch at `/tmp/skv12-waveW1a-rejected.patch` on FAIL.

## Section 5 - W2 `escape_mask_64` Correctness Prerequisite

Purpose: resolve the known SIMD correctness blocker before any new SIMD/ASM
admission or SIMD-backed CSS path.

Owner paths:

- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs` (caller-level
  adversarial parity and, only if falsified, the minimal carry handoff fix)
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/REDRESS.md`

Entry gate:

- W1a PASS.
- Plan names scalar reference, NEON body, xorshift falsifier, boundary carry
  cases, checkasm command, and corpus parity command.

Tasks:

- Verify the falsifier `0xCAFEF00DBAADF00D` reproduces or is already fixed.
- Fix state handoff/carry semantics if needed.
- Add/refresh checkasm and corpus parity for long backslash runs, carry-in/out,
  tails, alignment, and mixed ASCII/escape windows.

Exit gate `G-W2-ESCAPE-MASK-CORRECTNESS`:

- PASS: scalar reference and NEON body match across falsifier and adversarial
  cases; corpus parity passes.
- FAIL: SIMD remains blocked; W1b-1 must stay scalar-only and W4 cannot admit
  any SIMD/ASM primitive.

Revert protocol: revert SIMD/checkasm edits and save
`/tmp/skv12-waveW2-rejected.patch`.

## Section 6 - W1b-1 CSS L4 Generated Track 1 + Independent Oracle Scaffold

Purpose: create the authoritative CSS L4 generated Track 1 row and independent
oracle/equality scaffold without the lightningcss throughput gate.

Owner paths:

- `grammar/css/l4/{tokens,values,value-unit,properties}.bbnf`
- `skinny/Cargo.toml`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/runtime/src/grammars/json/` only for regenerated guard output
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/crates/bbnf-bench/src/gate.rs` only if report validation needs a
  shared gate helper change beyond `report.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/`
- `skinny/RESULTS.md`, `skinny/REDRESS.md`

Entry gate:

- W1a PASS.
- W2 PASS unless the plan proves the entire wave is scalar-only and does not
  touch `bbnf-simd`, aarch64 modules, or ASM-backed helpers.
- The selected CSS row is exactly
  `css_l4/declaration_values/direct_to_struct/main`.
- Output plane is `css_l4_declaration_value_fact_stream`.
- Generated runtime path is
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`.
- Plan names generated Track 1 source/runtime, CSS fixture corpus, independent
  oracle/Track 2, equality command, benchmark command, gate command, and
  rollback slice.
- Sheets/BBNF-self are not selectable in W1b-1 before a measured CSS redress
  attempt from W1b-2b.
- No new SIMD helper is legal in W1b-1 unless W2 has already passed.

Tasks:

- Generate CSS L4 Track 1 for the selected declaration-values row.
- Build independent oracle/Track 2 for the same canonical CSS fact stream.
- Emit canonical CSS facts for Track 1 and Track 2/oracle.
- Prove the CSS runtime is generated by a CSS-owned provider/profile, not
  hand-only code under a generated path or a JSON provider clone.
- Extend the Lock 14 frozen-root/parent-diff authorization for W1b-1 only,
  covering the Section 6 CSS scaffold owner slice and no substrate, IR,
  grammar-crate, pass-crate, SIMD, directive, or `BackendShape` surface.
- Extend the companion non-JSON report gate so generated size, grammar/input
  checksums, strictness, validation/profile artifacts, Lock 14/16 status,
  scalar-reference status, and parity status are consumed in the same wave.
- Run strict equality and baseline throughput for Track 1 and Track 2/oracle.
- Record generated LOC/module size/O(N) guard and JSON guard state.

Exit gate `G-W1b-1-CSS-L4-ORACLE`:

- PASS: generated Track 1 and independent Track 2/oracle compile, execute,
  produce strict-equal `css_l4_declaration_value_fact_stream` output, and emit
  finite Mbps plus generated-size telemetry.
- BLOCKED/FAIL: CSS cannot be generated/measured inside the W1b-1 scaffold
  surface; record REDRESS and return to plan. This scaffold failure does not
  satisfy the post-CSS-redress fallback condition. Sheets/BBNF fallback remains
  blocked until W1b-2b records measured CSS lightningcss comparator/admission
  redress, unless the user re-pins or S-P3 explicitly revises the topology.

Revert protocol: revert CSS generated/runtime/bench/gate/report changes and
save `/tmp/skv12-waveW1b-1-rejected.patch` when a patch was attempted.

## Section 7 - W1b-2 CSS L4 Lightningcss Comparator + Admission Gate

Purpose: add the same-plane lightningcss comparator, then consume Criterion
evidence into the CSS ADMIT gate for the row scaffolded by W1b-1. Section 7 is
sub-waved because CHALLENGE V2 found the monolithic comparator+gate surface too
large for the 30-minute redress cap.

### Section 7.1 - W1b-2a CSS L4 Lightningcss Comparator + Criterion Row

Purpose: land the dependency, fixture-limited lightningcss-gated source-sidecar
fact emitter, strict equality artifacts, and Criterion benchmark row. This
sub-wave cannot admit CSS SOTA and cannot move RESULTS.

Owner paths:

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/Cargo.lock`
- `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/track1-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/oracle-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/strict-equality.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt`
- `skinny/REDRESS.md`

Entry gate:

- W1b-1 PASS.
- Plan names lightningcss comparator command, version/build hash, equality
  command, benchmark command, artifact paths, dependency lockfile evidence,
  fixture-limit statement, and rollback slice.
- The row remains exactly
  `css_l4/declaration_values/direct_to_struct/main` and output plane remains
  `css_l4_declaration_value_fact_stream`.

Tasks:

- Build the lightningcss-gated same-plane source-sidecar fact extractor for the
  selected frozen fixture. Public lightningcss APIs validate parse success plus
  declaration/property/importance projections; raw token and byte-span facts are
  emitted by the source sidecar and must be fixture-limited in REDRESS.
- Run strict equality across generated Track 1, independent Track 2/oracle, and
  lightningcss.
- Run same-host throughput for generated Track 1, oracle/Track 2, and
  lightningcss with sample count >= 30.

Exit gate `G-W1b-2a-CSS-L4-LIGHTNINGCSS-COMPARATOR`:

- PASS-COMPARATOR: dependency compiles, fixture limits fail closed, strict
  equality artifacts are byte-identical across Track 1, Track 2/oracle, and the
  lightningcss-gated sidecar, and the Criterion group includes
  `lightningcss_same_plane_fact_stream` with sample count >= 30.
- FAIL: dependency, comparator, fixture-limit enforcement, equality, artifact
  writing, or Criterion row execution fails. Save rejected patch at
  `/tmp/skv12-waveW1b-2a-rejected.patch`.

No W1b-2a outcome is CSS ADMIT. RESULTS is not an owner path for W1b-2a.

### Section 7.2 - W1b-2b CSS L4 Lightningcss SOTA Report + Admission Gate

Purpose: consume the already-landed W1b-2a Criterion artifacts and equality
artifacts into the W1b-2-specific SOTA report/gate.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Entry gate:

- W1b-2a PASS-COMPARATOR.
- Plan names the Criterion artifact root, JSON guard root, report path, gate
  flag, no-write rejection matrix, stale-results guidance update, and rollback
  slice.

Tasks:

- Add `sk-v12-css-l4-sota-v1` report validation and
  `--skv12-css-l4-sota-report <path>`.
- Consume Criterion estimates for Track 1, Track 2/oracle, and lightningcss.
- Compute threshold and margin from `lightningcss_mbps + 1`.
- Run JSON guards against an existing accepted JSON Criterion root or a fresh
  populated JSON guard capture, not an empty CSS-only Criterion directory.
- Fail closed if the companion report is combined with write/probe flags.

Exit gate `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`:

- PASS-ADMIT-CANDIDATE: `track1_mbps > lightningcss_mbps + 1`, strict equality,
  oracle independent, telemetry consumed, JSON guards held/demoted.
- PASS-MEASURED-BASELINE: CSS row is strict-equal and measurable but does not
  beat `lightningcss_mbps + 1`; continue to W3/W4 or record FIXPOINT evidence
  later. This outcome records REDRESS evidence and **does not move
  `skinny/RESULTS.md`**.
- BLOCKED/FAIL: comparator, equality, oracle independence, generated-size,
  throughput, or gate consumption fails; record REDRESS. Sheets/BBNF fallback
  requires a subsequent S-P3 or wave plan revision after this measured CSS
  redress attempt.

Revert protocol: revert gate/report/result edits and save
`/tmp/skv12-waveW1b-2b-rejected.patch`.

## Section 8 - W3 CSS-Local Same-Tape Union Attempt

Purpose: satisfy the user-pin union route under a material differential and
test whether a CSS-local same-tape union can move the CSS row.

Owner paths:

- `skinny/crates/runtime/src/tape/`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`, `skinny/REDRESS.md`

Entry gate:

- W1b-2b has a measured CSS row.
- Fresh profile or same-host microbench identifies a CSS hot leaf that a
  same-tape union can consume.
- CHALLENGE accepts the material differential from REDRESS 96/97/98.
- Plan proves no sidecar substrate, no parser-owned cursor/list, no parallel
  `UnionTape`, and no retained decoded-byte/class side vector.

Tasks:

- Implement a single-substrate same-tape union projection local to the CSS row.
- Wire the CSS generated consumer in the same commit.
- Run strict CSS equality, CSS throughput, JSON guards, and profile/microbench.

Exit gate `G-W3-CSS-UNION-ATTEMPT`:

- BEHAVIOR-PASS-CSS-ADMIT if CSS Track 1 beats `lightningcss_mbps + 1` and all
  close guards pass.
- BEHAVIOR-PASS-NONCLOSE if equality/parity and local caller measurement pass
  but the CSS close bar is not met. This is evidence, not ADMIT.
- MEASURED-REJECT if equality passes but throughput misses, microbench rejects
  production wiring, or JSON guard regresses.
- FIXPOINT-CREDIT if the attempt is measured or microbench-rejected before
  production wiring under the accepted wave plan, materially differentiated,
  and REDRESS records the miss.

Revert protocol: revert union/runtime/codegen/generated/gate edits and save
`/tmp/skv12-waveW3-rejected.patch`.

## Section 9 - W4 ASM-Gen CSS Consumer + AArch64 Orphan Disposition

Purpose: attempt a measured ASM-gen route and dispose the orphan production
aarch64 set.

Owner paths:

- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/src/scalar/`
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
- `skinny/crates/parse-that-regex/src/`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- `skinny/crates/codegen/src/css_l4_declaration_values_templates/`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md`
- `skinny/RESULTS.md`, `skinny/REDRESS.md`

Entry gate:

- W1b-2b has a measured CSS row.
- W2 PASS if the candidate touches string/escape/SIMD correctness surfaces.
- CHALLENGE accepts REDRESS adjacency and cost.
- Same-host microbench proves the selected candidate on a CSS or JSON-guard hot
  leaf before production routing.
- Plan selects at most one primary ASM-gen candidate from S-P2 selectable rows:
  `a64_tbl_tbx_byte_class_mask64`, `a64_udot_digit_run_span`,
  `a64_wide_string_special_scan64`, `a64_hex_quartet_decode_x4`, or
  `a64_ascii_set_run_skip`.
- Plan includes a five-row orphan accounting table. Non-selected orphans may
  be `inventory_demoted_with_evidence` only when the plan proves no behavior
  source change is needed. Any orphan requiring production consumption or
  removal outside the selected primitive blocks close or requires a later wave.

Tasks:

- Add/refresh scalar reference and strict checkasm/parity.
- Wire the selected primitive into a same-wave CSS generated consumer or
  JSON-guard consumer.
- Measure strict CSS equality/throughput and JSON guards.
- Dispose the selected primitive's orphan if applicable. Record the status of
  all five production orphans by consumption, removal, or inventory demotion
  with evidence: `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, `cache_hints`.

Exit gate `G-W4-ASM-GEN-CONSUMER`:

- BEHAVIOR-PASS-CSS-ADMIT if CSS Track 1 beats `lightningcss_mbps + 1`, strict
  equality passes, JSON guards hold/demote, Lock 16 passes, and orphan count is
  zero.
- BEHAVIOR-PASS-NONCLOSE if scalar/checkasm/microbench/equality pass but the CSS
  close bar is not met. This is evidence, not ADMIT.
- MEASURED-REJECT if the selected candidate misses or regresses but scalar,
  checkasm, microbench, same-wave consumer, and REDRESS evidence are complete.
- BLOCKED if W2 fails and no non-SIMD ASM-gen candidate can legally dispatch.

Revert protocol: revert ASM/SIMD/runtime/codegen/generated/gate edits and save
`/tmp/skv12-waveW4-rejected.patch`.

## Section 10 - W5 Close And Alpha Feedback

Purpose: close SK-V12 honestly and prepare Pass Alpha if the campaign
continues.

Owner paths:

- `restart/skinny/tranches/sk-v12/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-*.md` if ADMIT/FIXPOINT closes

Entry gate:

- W0, W1a, W2, W1b-1, W1b-2a, W1b-2b, and W4 have admitted, rejected, routed,
  or blocked with evidence.
- W3 has disposition only when closing as FIXPOINT or when no prior CSS row
  satisfies ADMIT. W3 is not required on an already-admitted CSS path.

Tasks:

- Determine ADMIT or FIXPOINT.
- Reconcile close docs and report CSS vs lightningcss final number.
- Record union and ASM-gen dispositions.
- Record final orphan state.
- Route SK-V13 remainder if close not achieved.

Exit gate `G-W5-CLOSE`:

- PASS-ADMIT: Section 0.1 ADMIT holds.
- PASS-FIXPOINT: Section 0.1 FIXPOINT holds.
- ROUTE: ADMIT/FIXPOINT does not hold; Pass Alpha opens SK-V13 with explicit
  blockers. The campaign does not stop unless the user-pin close clause is met
  or a measured fixpoint is recorded.

Revert protocol: docs/report-only revert; no behavior patch exists.

## Section 11 - Pre-Blocked And Reopened Routes

Still blocked:

1. `parse_only` SOTA admission.
2. Sheets or BBNF-self before a measured CSS redress attempt.
3. CSS close on `>= 1 Mbps`, generated-baseline existence, or
   `ceil(baseline_mbps * 1.01)`.
4. Hand-only CSS parser, report-only CSS row, stale witness module, stale
   sidecar comparator, or producer-only telemetry.
5. Generic-crate grammar policy branches or JSON template leftovers masquerading
   as generic code.
6. New directive, BIR variant, `BackendShape` variant, public substrate API,
   parser-owned sidecar, decoded-byte sidecar, hidden host schema,
   benchmark-private Track 1 parser, digest-only proof, and x86 work.
7. Replays of REDRESS 111-120 without material differential and CHALLENGE.
8. Orphan SIMD/ASM admission or checkasm-only performance admission.

Reopened at category level:

1. Union / event-model / class-column / streaming-cursor / retained structural
   family adjacent to REDRESS 96/97/98. Specific historical implementations
   remain rejected. A new attempt requires material differential, CHALLENGE,
   single-substrate semantics, scalar/reference proof, same-wave consumer, and
   measurement.
2. ASM-gen routes adjacent to REDRESS 88/89/90. PMULL, CSSC CTZ, EOR3/BCAX,
   UDOT, TBL/TBX, or another ARMv9.2-A primitive may dispatch only under
   micro-prove-first, scalar reference, checkasm/parity, same-wave consumer,
   JSON guard, and strict CSS/row gate evidence.

## Section 12 - Convergence And Escalation

SK-V12 converges when W0, W1a, W2, W1b-1, W1b-2a, W1b-2b, W4, W5, and W3 when
required for FIXPOINT have dispositions and Section 0.1 ADMIT or FIXPOINT
holds. If neither holds, W5 routes the exact remainder into Pass Alpha for
SK-V13 and the campaign continues.

Escalate immediately if:

- a USER PIN clause needs amendment;
- lightningcss cannot be made a same-plane strict comparator;
- CSS L4 semantic parity requires a public directive/BIR/BackendShape expansion;
- an admitted JSON guard regression is not recoverable in tranche;
- x86 becomes necessary despite being out of scope;
- ADMIT appears unreachable and FIXPOINT evidence is complete enough that the
  user must choose re-pin versus honest close.
