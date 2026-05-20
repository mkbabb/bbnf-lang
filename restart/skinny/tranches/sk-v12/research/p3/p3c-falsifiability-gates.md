# SK-V12 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: PIN-V4.
Date: 2026-05-20.
Scope: per-wave measurable gates for the SK-V12 USER-PIN CSS L4 admission,
Lock 14/Lock 16 prerequisites, union-substrate attempt, ASM-gen attempt, guard
surface, and close/fixpoint forms.
Output: this file.
Pass Alpha goalset: ADMIT when generated CSS L4 Track 1 throughput is strictly
greater than `lightningcss_mbps + 1` on the same corpus, same output plane,
same host, strict equality, with independent oracle/Track 2, gate-consumed
provenance, Lock 14 clean, Lock 16 clean where applicable, JSON guards held or
measured-disposition demoted, and zero production aarch64 orphans. FIXPOINT
when CSS L4 redress proves ADMIT uncloseable and the closing tranche records
both a new measured union-substrate attempt and a new measured ASM-gen attempt.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis

The 2026-05-20 user pin supersedes the pre-pin SK-V12 P3 gate shape. CSS L4 is
the authoritative first generated grammar target; Sheets and BBNF-self are
fallbacks only after a measured CSS L4 redress attempt. The CSS admission
threshold is not `ceil(baseline_mbps * 1.01)`, not a local baseline lift, and
not a JSON parse-only result. The only SK-V12 ADMIT close target is:

`generated_css_l4_track1_mbps > lightningcss_mbps + 1`

on the same corpus, same output plane, same host, strict equality semantics,
and a same-run independent oracle/Track 2.

The live profile/research basis is pin-aware S-P1 and S-P2. S-P1 converged
under PIN-V6/PIN-V7, with JSON telemetry admitted only as nomination evidence
for candidate families, not CSS proof. S-P2 converged under PIN-V3/PIN-V4 and
limits selectable aarch64 rows to C1 `a64_tbl_tbx_byte_class_mask64`, C3
`a64_udot_digit_run_span`, C4 `a64_wide_string_special_scan64`, C5
`a64_hex_quartet_decode_x4`, and C6 `a64_ascii_set_run_skip`. P2-D contributes
no current shortlist-ready tape/union primitive, but USER PIN D3 requires that
the campaign/fixpoint surface include at least one new material-differential
union-substrate attempt if ADMIT is uncloseable. USER PIN D4 likewise unblocks
ASM-gen at the category level and makes one new measured ASM-gen attempt part
of the FIXPOINT close.

The gate set therefore has two layers:

1. Mandatory CSS admission gates: `GrammarConfig`/Lock 14 before CSS emission,
   CSS L4 generated Track 1 plus lightningcss comparator, and strict
   provenance/equality consumption.
2. Campaign-fixpoint gates: `escape_mask_64` Lock 16 correctness before any
   new SIMD admission, then one materially new union route and one materially
   new ASM-gen route, each measured against a CSS L4 hot leaf or, if CSS L4 is
   blocked after redress, a JSON guard hot leaf.

`parse_only` remains diagnostic-only in every form. A gate that attempts to
close on `parse_only`, stale `ceil(baseline_mbps * 1.01)`, a hand-only CSS
parser, a stale witness, a producer-only telemetry field, an orphan SIMD
kernel, or a generic-crate JSON policy leak is unmeasurable and returns REVISE
before redress.

## §2 — Deliverable

P3-B/P3-F may rename waves, but the measurable gates below are binding for the
pin-aware SK-V12 wave plan.

| Wave | Gate | Gate role | Admission / disposition threshold |
|---|---|---|---|
| W0 | `G-W0-PIN-REVALIDATE` | Revalidate W0 telemetry/gate lock and guard surface. | Gate/report schema is consumed; opening JSON surface unchanged or measured; no behavior movement. |
| W1a | `G-W1a-GRAMMARCONFIG-LOCK14` | Resolve the 7 Lock 14 leaks through `GrammarConfig` or equivalent generated metadata before CSS emission. | Generic-crate scan PASS, JSON guard parity PASS, CSS generated metadata compiles without JSON policy. No CSS row admission yet. |
| W2 | `G-W2-ESCAPE-MASK-LOCK16` | Verify and resolve the `escape_mask_64` NEON correctness bug before any new SIMD admission. | Scalar/checkasm falsifier fixed; expanded parity PASS; no new SIMD admission can bypass this gate. |
| W1b-1 | `G-W1b-1-CSS-L4-ORACLE` | Generate the exact CSS L4 Track 1 row and independent oracle/Track 2 equality scaffold. | Row `css_l4/declaration_values/direct_to_struct/main`, output plane `css_l4_declaration_value_fact_stream`, strict Track1-vs-oracle equality PASS, finite Mbps. |
| W1b-2 | `G-W1b-2-CSS-L4-LIGHTNINGCSS` | Add lightningcss comparator, three-way strict equality, and report/gate consumption. | `generated_css_l4_track1_mbps > lightningcss_mbps + 1`; equality at `+1` is FAIL. |
| W3 | `G-W3-CSS-LOCAL-UNION` | New measured union-substrate implementation attempt under USER PIN D3. | CSS ADMIT only if the final CSS row clears the lightningcss bar; otherwise behavior PASS/REJECT can provide FIXPOINT-credit evidence. |
| W4 | `G-W4-ARMV92-ASM-GEN-ORPHANS` | New measured ASM-gen attempt under USER PIN D4 plus zero-orphan aarch64 disposition. | CSS ADMIT only if the final CSS row clears the lightningcss bar; otherwise behavior PASS/REJECT can provide FIXPOINT-credit evidence and orphan disposition. |
| W5 | `G-W5-CLOSE-SK-V12` | Close or roll to next tranche. | ADMIT form or FIXPOINT form; all close docs and `RESULTS.md`/`REDRESS.md` agree. |

### 2.1 Global Measurability Rules

Every gate that emits or consumes row telemetry must consume these fields in
`gate-json`, `gate`, or the same-wave non-JSON companion gate:

| Field family | Required consumed fields |
|---|---|
| Identity | `schema_id`, `row_id`, `grammar_id`, `domain`, `workload`, `output_plane`, `strictness` |
| Generated parser | generated source path, generated runtime path, generated module checksum, grammar checksum, generated LOC, generated module byte size, O(N) grammar-size status |
| Input/provenance | fixture path, fixture checksum, corpus byte count, selected output-plane definition |
| Oracle/comparator | independent oracle or Track 2 path, independence status, lightningcss command, lightningcss artifact, lightningcss Mbps, comparator strictness, comparator output plane |
| Measurement | Track 1 Mbps, Track 2/oracle Mbps, sample count, sample cost, benchmark artifact, run id, host triple, feature mask, build flags, profile artifact |
| Correctness | strict equality result, measured validation path, Lock 14 status, Lock 16 status where applicable, scalar-reference status, checkasm/parity status |
| Wave accounting | same-wave consumer class, JSON guard state, aarch64 orphan state, gate status, wave id, REDRESS id |

Any missing consumed field, stale run id, mixed-run comparator, oracle coupling,
producer-only telemetry, parse-only admission, x86 implementation path, or
permissive/lossy comparator fails closed.

### 2.2 JSON Guard Floors

JSON guards are secondary to CSS L4 ADMIT, but they remain binding unless a
wave records a measured REDRESS demotion. A behavior wave that touches generic
runtime, codegen, generated output, parse-that, SIMD/ASM, benchmark, report, or
gate code must either rerun the guard surface or prove no JSON-producing path
moved and `skinny/RESULTS.md` JSON rows are unchanged.

Direct guard floors:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard floors:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

The inherited JSON shape remains 16 `parse_only S / NO-GO`, one
`parse_only L / NO-GO`, four direct `A / GO`, thirteen direct
`N-direct / NO-GO`, seven typed `A / GO`, and overall `N-direct / NoGo`
unless a same-wave measured disposition changes it. `parse_only` cannot admit
SK-V12.

## §3 — Falsifiability Binding

### 3.1 W0: `G-W0-PIN-REVALIDATE`

Entry:

- Pin-aware S-P1 and S-P2 are converged.
- W0 telemetry/gate lock at commit `f788eb97` is available for revalidation.
- Current `skinny/RESULTS.md` and `skinny/REDRESS.md` are readable.

Exit PASS:

- `gate-json`/report schema consumes the SK-V12 non-JSON companion fields and
  still validates the JSON table.
- No behavior source, parser, scanner, SIMD/ASM, codegen behavior, generated
  runtime output, benchmark body, or row semantics move.
- Outcome enum remains the current admissible set; no new outcome variant is
  introduced by W0.
- JSON guard shape in §2.2 is unchanged, or the wave records a measured
  guard-disposition demotion.
- W0 records profile/gate freshness and rejects stale inherited row ids as
  behavior-wave evidence.

FAIL / REVISE:

- Any W0 source movement that changes behavior.
- Any emitted row/field not consumed by a gate.
- Any attempt to admit a CSS, JSON direct, typed, or parse-only row by
  telemetry accounting alone.

Revert protocol: revert W0 report/gate/result edits as one slice, preserve the
failed gate output in REDRESS, and save `/tmp/skv12-waveW0-rejected.patch` if
source/report code changed.

### 3.2 W1a: `G-W1a-GRAMMARCONFIG-LOCK14`

Entry:

- W0 PASS.
- Owner plan names the generic/runtime/codegen paths that currently carry the
  seven Lock 14 leaks from `skv12-value-api-audit.md`.

Exit PASS:

- `GrammarConfig` or equivalent generated metadata surface exists for
  structural alphabet, FIRST/follow tables, layout/trivia policy, string/escape
  policy, number policy, flag interpretation, sink/view/kind templates, and
  output-plane configuration.
- The seven leaks are removed from generic behavior:
  structural alphabet hardcoding, value dispatch hardcoding, JSON backslash
  string policy, JSON number policy, quoted-key/object-pair assumption,
  JSON-tied `OffsetFlags` semantics, and `JsonSink` method hardcoding.
- Generic-crate scan is negative for grammar-name behavior branches:
  no generic `match grammar`, no `JsonParser`/`CssL4Parser`/`GoogleSheetsParser`
  public API in generic crates, no handwritten CSS/Sheets/BBNF runtime modules
  outside generated output paths, and no generic JSON structural alphabet.
- JSON check/regeneration and guard parity pass, or unchanged JSON output is
  proven by diff plus gate.
- Generated CSS L4 metadata compiles far enough to prove W1b-1 can emit from
  grammar facts without JSON provider policy.
- No CSS L4 row is admitted in W1a. W1a is a legality gate, not the SOTA gate.

FAIL / REVISE:

- CSS codegen still depends on JSON provider policy.
- Generic crates branch on grammar names or encode JSON-only behavior.
- The wave tries to claim Lock 14 by prose, compile-only stubs, or hand-only
  witness modules.

Revert protocol: revert GrammarConfig/codegen/runtime/gate/report edits as one
slice, preserve failed scans/tests in REDRESS, and save
`/tmp/skv12-waveW1a-rejected.patch`.

### 3.3 W1b-1: `G-W1b-1-CSS-L4-ORACLE`

Entry:

- W1a PASS.
- Selected row is CSS L4, not Sheets or BBNF-self. The expected row id is
  `css_l4/declaration_values/direct_to_struct/main`.
- Output plane is `css_l4_declaration_value_fact_stream`.
- Generated runtime path is
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`.
- W2 PASS unless the accepted W1b-1 plan proves the wave is scalar-only and
  touches no `bbnf-simd` or ASM-backed helper.
- Plan names generated Track 1 source/runtime, CSS fixture corpus, independent
  oracle/Track 2, equality command, benchmark command, gate command, and
  rollback slice.

Exit PASS:

- Generated CSS L4 Track 1 compiles and executes from generated output, not a
  hand-only parser or stale witness.
- Independent Track 2/oracle is same-plane, strict, same-host, fresh, and does
  not call generated Track 1 or hidden shared parser code.
- Strict output equality passes between generated Track 1 and independent
  Track 2/oracle on the canonical CSS fact stream.
- Sample count is at least 30 for Track 1 and Track 2/oracle.
- Gate consumes all provenance fields in §2.1.
- No CSS ADMIT is recorded in W1b-1. It is a scaffold/equality wave.

### 3.4 W1b-2: `G-W1b-2-CSS-L4-LIGHTNINGCSS`

Entry:

- W1b-1 PASS.
- Row remains exactly `css_l4/declaration_values/direct_to_struct/main`.
- Output plane remains `css_l4_declaration_value_fact_stream`.
- Plan names lightningcss comparator command, version/build hash, output
  artifact, equality command, benchmark command, gate command, and rollback
  slice.

Exit ADMIT:

- lightningcss comparator is same corpus, same output plane, same host,
  strict semantics, and same run family.
- Strict output equality passes among generated Track 1, independent
  Track 2/oracle, and lightningcss canonical fact stream.
- Sample count is at least 30 for Track 1, Track 2/oracle, and lightningcss.
- Gate consumes all provenance fields in §2.1.
- Throughput satisfies strict inequality:

  `generated_css_l4_track1_mbps > lightningcss_mbps + 1`

  Equality at `lightningcss_mbps + 1` is FAIL. A threshold based on
  `ceil(baseline_mbps * 1.01)` is stale and cannot admit the row.
- JSON guard floors in §2.2 hold, or any demotion is explicitly measured and
  recorded in REDRESS.

Measured REJECT / BLOCKED:

- If strict equality, comparator wiring, oracle independence, or throughput
  misses, W1b-2 records REDRESS with measured evidence and no hidden fallback to
  Sheets/BBNF-self inside the same redress.
- Sheets or BBNF-self may be considered only after the CSS L4 redress attempt
  is recorded.

Revert protocol: revert generated CSS runtime, codegen, fixture, bench, oracle,
report/gate, and `RESULTS.md` edits as one slice on equality miss, comparator
miss, Track 1 threshold miss, Lock 14 regression, guard regression, stale run,
or oracle coupling; save `/tmp/skv12-waveW1b-2-rejected.patch`.

### 3.5 W2: `G-W2-ESCAPE-MASK-LOCK16`

Entry:

- W1a PASS.
- Any planned SIMD/string/escape work depends on this gate first.

Exit PASS:

- The `escape_mask_64` falsifier is present as a regression case: xorshift seed
  `0xCAFEF00DBAADF00D`, iter 0, 128-byte JSON-pool buffer.
- Scalar reference and aarch64 NEON implementation agree on the falsifier, long
  backslash runs, cross-block carry-in/carry-out, all boundary positions,
  random corpus windows, CSS string/identifier escape windows where available,
  and existing JSON guard windows.
- `CHECKASM-REPORT.md` or the same-wave checkasm artifact records PASS.
- No new SIMD primitive is admitted only because this correctness fix lands.
  W2 is a correctness prerequisite; row movement requires W3/W4 or another
  behavior gate with a same-wave consumer.
- JSON guards hold or measured demotion is recorded.

FAIL / REVISE:

- Any remaining scalar/NEON divergence.
- Any SIMD admission before the W2 checkasm gate is green.
- Any attempt to waive the falsifier with throughput evidence.

Revert protocol: revert SIMD/checkasm/report edits as one slice, preserve
failing inputs in REDRESS, and save `/tmp/skv12-waveW2-rejected.patch`.

### 3.6 W3: `G-W3-CSS-LOCAL-UNION`

Entry:

- W2 PASS if W3 touches SIMD/string masks; otherwise W2 may be N/A only for a
  scalar-only union attempt.
- A generated CSS L4 hot leaf exists from W1b-2, or W1b-2 has recorded a measured
  CSS BLOCKED/REJECTED route and CHALLENGE selects a JSON guard hot leaf as the
  fallback consumer for fixpoint evidence.
- Plan cites REDRESS 96/97/98, names the material differential, and passes
  CHALLENGE.

Material differential required:

- Not REDRESS 96 class-column substrate.
- Not REDRESS 97 streaming structural cursor.
- Not REDRESS 98 class-lane proof-only route.
- No public substrate API, `UnionTape`, parser-owned sidecar, retained
  structural-position vector, aux density table, whitespace bitmap, cursor
  list, or parallel projection.
- The only legal shape is same-tape, caller-local, generated-policy metadata or
  transient facts consumed in the same loop and discarded.

Behavior PASS / CSS ADMIT exit:

- Scalar/reference model and equality/parity tests pass.
- Same-host microbench proves the union route's local caller is faster than
  the scalar/reference caller by at least `ceil(reference_mbps * 1.01)`.
- The same-wave consumer is a CSS L4 generated hot leaf if CSS is available, or
  a JSON guard hot leaf only after W1b-2 measured CSS redress is recorded.
- If CSS is the consumer, strict CSS equality still passes and Track 1 remains
  `> lightningcss_mbps + 1` before the wave can be called SK-V12 ADMIT. If the
  CSS close bar is not met, record behavior PASS or measured reject evidence,
  not ADMIT. If JSON is the fallback consumer, the selected JSON row must
  maintain its guard floor or record a measured demotion; JSON evidence cannot
  be SK-V12 ADMIT.
- No substrate cardinality increase is observable in source or telemetry.

FIXPOINT-credit measured reject:

- W3 still counts for FIXPOINT only if source was implemented and measured (or
  microbench-rejected before production wiring under the wave plan), REDRESS
  records the material differential, fresh profile/caller evidence, scalar
  reference, parity/equality result, microbench result, and why the route is
  uncloseable.
- A plan-time statement that "union is bad" is not W3 FIXPOINT evidence.

Revert protocol: revert union/codegen/runtime/bench/report/gate/RESULTS edits
on equality miss, microbench miss, row miss, guard regression, substrate leak,
or REDRESS replay; save `/tmp/skv12-waveW3-rejected.patch`.

### 3.7 W4: `G-W4-ARMV92-ASM-GEN-ORPHANS`

Entry:

- W2 PASS for any new SIMD admission.
- Plan selects one ARMv9.2-A/aarch64 implementation attempt from the S-P2
  survivor/inventory surface and passes CHALLENGE:
  TBL/TBX byte-class mask, UDOT digit-run span, wide string special scan,
  hex-quartet x4 decode, ASCII set run skip, PMULL narrow consumer, CSSC CTZ
  narrow consumer, EOR3/BCAX ternary mask fold, or another named ARMv9.2-A
  primitive with scalar reference and same-wave consumer.
- If adjacent to REDRESS 88/89/90, plan cites the historical implementation
  and names the material differential.

Behavior PASS / CSS ADMIT exit:

- Scalar reference is executable.
- checkasm/parity covers adversarial inputs, tails, alignment, feature masks,
  random corpus windows, and the selected CSS/JSON caller windows.
- Same-host microbench proves the primitive under the actual caller, not a
  standalone orphan: candidate caller throughput at least
  `ceil(reference_mbps * 1.01)`.
- Same-wave consumer is generated CSS L4 if available, or a JSON guard hot leaf
  only after W1b-2 measured CSS redress is recorded.
- Strict equality/parity passes for the output plane.
- CSS consumer keeps Track 1 `> lightningcss_mbps + 1` before the wave can be
  called SK-V12 ADMIT. If the CSS close bar is not met, record behavior PASS or
  measured reject evidence, not ADMIT. JSON fallback consumer keeps guard floors
  or records measured demotion and cannot be SK-V12 ADMIT.
- Feature fallback is present for hosts lacking the selected optional feature.
- No x86 path is added.

Zero-orphan exit:

Each carried orphan from the user pin must be one of `consumed`,
`removed`, or `inventory_demoted_with_evidence` by W4 close:

| Orphan | Required W4 disposition |
|---|---|
| `bitmap_prefix_xor_64` | consumed by a same-wave string/escape caller, removed, or inventory-demoted; PMULL default body replay fails. |
| `bitmap_next_set_bit` | consumed by a local first/next-bit caller, removed, or inventory-demoted; global CTZ bulk replay fails. |
| `bulk_emit_positions_64` | consumed into the canonical tape/fact stream, removed, or inventory-demoted; side-vector emission fails. |
| `byte_context` | consumed by a chunk-spanning string/comment/layout caller, removed, or inventory-demoted. |
| `cache_hints` | consumed by a measured fact-stream/tape/output writer with identical output, removed, or inventory-demoted. |

FIXPOINT-credit measured reject:

- W4 counts for FIXPOINT if the selected ASM-gen implementation attempt is
  measured, rejected with scalar/checkasm/microbench/equality evidence, and
  REDRESS records material differential against relevant historical entries.
- Production orphans at close invalidate FIXPOINT.

Revert protocol: revert ASM/SIMD/parse-that/codegen/runtime/bench/report/gate
edits on checkasm failure, microbench miss, equality miss, row miss, guard
regression, orphan leak, feature fallback miss, or stale run; save
`/tmp/skv12-waveW4-rejected.patch`.

### 3.8 W5: `G-W5-CLOSE-SK-V12`

ADMIT close requires all of:

- W1a PASS.
- W1b-2 ADMIT: generated CSS L4 Track 1 `> lightningcss_mbps + 1`, strict
  equality, independent oracle/Track 2, same-plane same-host lightningcss, and
  gate-consumed provenance.
- W2 PASS before any SIMD admission.
- W3/W4 dispositions recorded if they ran; W4 zero-orphan target satisfied by
  admission, removal, or inventory demotion.
- JSON guards hold or have measured-disposition demotions.
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SYNTHESIS.md`, `SPEC.md`,
  `HANDOFF.md`, and `DISPATCH-PROMPT.md` agree.

FIXPOINT close requires all of:

- W1b-2 has a measured CSS L4 redress attempt proving ADMIT uncloseable in the
  current tranche. Sheets/BBNF-self were not used before that attempt.
- W3 records one new measured union-substrate implementation attempt with
  REDRESS 96/97/98 citation, material differential, fresh profile/caller
  evidence, scalar/reference proof, equality/parity result, and microbench or
  row measurement.
- W4 records one new measured ASM-gen implementation attempt with REDRESS
  88/89/90 citation where adjacent, material differential, scalar reference,
  checkasm/parity, microbench, equality, and same-wave consumer evidence.
- Production aarch64 orphan count is zero by consumption, removal, or explicit
  inventory demotion.
- JSON guards hold or have measured demotions.
- REDRESS names every measured miss and routed remainder, and close docs agree.

Close FAIL:

- CSS close based on `ceil(baseline_mbps * 1.01)`.
- `parse_only` admission.
- Missing lightningcss comparator.
- Missing independent oracle/Track 2.
- Lock 14 prose-only proof.
- SIMD admission with unresolved `escape_mask_64`.
- Any production aarch64 orphan at close.
- Union or ASM-gen category skipped in a FIXPOINT close.
- Future-phase promise instead of measurement.

## §4 — Pre-Blocked Routes

Global pre-blocks that still bind every wave:

- `parse_only` SOTA admission. Parse-only is diagnostic-only.
- CSS L4 admission against `ceil(baseline_mbps * 1.01)`, a bbnf baseline lift,
  a hand-only parser, stale witness module, missing lightningcss comparator, or
  missing strict equality.
- Sheets or BBNF-self before a measured CSS L4 redress attempt.
- Generic-crate JSON policy, grammar-name branches, handwritten per-grammar
  runtime modules outside generated output, or Lock 14 prose-only claims.
- New directive, BIR variant, BackendShape expansion, public substrate API,
  parser-owned sidecar, retained structural vector, or x86 implementation.
- SIMD/ASM admission before `escape_mask_64` correctness is verified and
  resolved.
- Orphan primitive admission: scalar/checkasm success without same-wave
  consumer is proof-only.
- Direct JSON residual row movement as a close target. REDRESS 119/120 remain
  routed guard evidence unless a later wave names fresh material evidence and
  measures both Track 1 and Track 2.

Category-unblocked but historically binding:

| Category | Historical REDRESS | Fresh attempt must prove |
|---|---|---|
| Union / substrate | 96, 97, 98 | Material differential, same-tape/non-sidecar shape, scalar/reference proof, same-wave CSS or guard consumer, and measurement. |
| PMULL / CSSC CTZ / canary-adjacent ASM-gen | 88, 89, 90 | Narrow consumer or new caller, not default global body; scalar/checkasm, microbench, feature fallback, same-wave consumer, and measurement. |
| String / escape proof-only SIMD | 28, 33, 82, 83, 106, 107, 108, 116, 117 | New source delta, strict parity, same-wave CSS/generated consumer, and row measurement. |
| JSON numeric / container / digest residual | 80, 114, 115, 118, 119 | Fresh post-pin profile and material source differential; cannot substitute for CSS L4 close. |
| Non-JSON report/baseline blockers | 111, 112, 113 | Report lane alone is not generated Track 1; W1b-1 must create the CSS row and W1b-2 must create comparator evidence. |

## §5 — Sources

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `/tmp/skv12-pin-p1/pmu/capture_status.tsv`
- `/tmp/skv12-pin-p1/samply/capture_status.tsv`
- `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`
- `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`
