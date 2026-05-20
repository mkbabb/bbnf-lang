# SK-V12 Alpha-C REDRESS Digest

Pass: Pass Alpha V1.
Agent: alpha-C.
Date: 2026-05-20.
Scope: REDRESS digest for SK-V11 -> SK-V12.

## Authority

PASS-ALPHA assigns alpha-C to walk the prior skinny cycle REDRESS entries,
classify admitted, rejected, and partial routes, identify routes that should
pre-block SK-V12, and identify routes that may admit under a materially
different framing (`restart/prompts/pass-contracts/PASS-ALPHA.md:20-29`).
CHALLENGE review then checks REDRESS regression and pre-block coverage
(`restart/prompts/pass-contracts/PASS-ALPHA.md:35-47`).

SK-V11 closed as a measured fixpoint, not as direct `GO` and not as a
grammar-generalization win. The close authority preserves the unchanged
SK-V11-open surface: `parse_only` is 16 `S / NO-GO` plus 1 `L / NO-GO`,
`direct_to_struct` is 4 `A / GO` plus 13 `N-direct / NO-GO`,
`real_typed_struct` is 7 `A / GO`, and overall remains `N-direct / NoGo`
(`restart/skinny/tranches/sk-v11/research/close/close-redress.md:16-28`;
`skinny/REDRESS.md:3531-3541`).

The close finding is load-bearing for SK-V12:

- REDRESS 119 is the per-row direct residual authority.
- REDRESS 112 and 113 block the generated non-JSON baseline/intervention axis.
- REDRESS 96/97/98 and REDRESS 102 remain binding W3/parse-only blocks.
- SK-V12 must solve the generated non-JSON baseline before spending another
  JSON-only micro-wave (`restart/skinny/tranches/sk-v11/HANDOFF.md:147-153`;
  `restart/skinny/tranches/sk-v11/SPEC.md:66-70`).

## SK-V11 REDRESS Classifications

| Entry | Commit | Wave | Classification | Row effect | Evidence and carry-forward |
|---:|---|---|---|---|---|
| 111 | `be45d32b` | W1a non-JSON gate/report schema lane | ADMITTED | None | The companion non-JSON report lane was admitted and gate-consumed via `--w1a-non-json-report`; it did not relax JSON schema-v3/W0 validation, create generated baseline authority, update `skinny/RESULTS.md`, or move a row. Evidence: W1a report and gate tests, pass fixture, negative fixtures for producer-only telemetry, coupled Track 2, shared source, and admission claim, plus `gate-json --with-cost-facts --check-results`; guard diffs for `skinny/RESULTS.md`, codegen, runtime, and SIMD stayed clean (`skinny/REDRESS.md:3282-3309`). |
| 112 | `5dba63aa` | W1b generated non-JSON baseline and oracle lane | REJECTED | None | The selected `css_l4/declaration_values/direct/main` baseline could not admit because skinny codegen still routes direct and typed emission through `json_provider::ensure_runtime_profile`, which accepts only JSON, and no generated CSS L4 runtime exists under `skinny/crates/runtime/src/grammars/`. Generated CSS L4 Track 1 was absent, so the independent oracle path was not admitted. `/tmp/skv11-waveW1b-rejected.patch` is an empty marker; no source, generated runtime, bench body, gate/report schema, or result row moved. Evidence includes codegen/gate tests, W1a pass fixture, `gate-json --with-cost-facts --check-results`, `git diff --exit-code -- skinny/RESULTS.md`, runtime file listing, `ensure_runtime_profile` grep, and absence checks for CSS L4 generated runtime dirs (`skinny/REDRESS.md:3311-3338`). |
| 113 | `c8b8c1b4` | W2 CSS L4 generated direct/typed intervention | BLOCKED | None | W2 could not dispatch because W1b did not close and no `W1b_css_baseline_mbps` exists. W2 may not create the first measurable non-JSON row; without the baseline, `ceil(W1b_css_baseline_mbps * 1.01)` is undefined and `G-W2-CSS-GENERATED-INTERVENTION` is not measurable. No source, generated parser, SIMD kernel, benchmark row, gate schema, or result row moved. Entry record: `restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md` (`skinny/REDRESS.md:3340-3355`). |
| 114 | `85d15ddf` | W3 numeric direct closure slice | REJECTED | None | The scalar `number_span_emit_slot` route passed pre-measure checks but failed the selected `mesh/direct_to_struct` floor. Criterion under `/tmp/skv11-w3-criterion` measured Track 1 3835 Mbps, Track 2 3614 Mbps, sonic-rs direct 4413 Mbps, serde_json direct 3191 Mbps, versus the 8675 Mbps floor. The rejected source patch is `/tmp/skv11-waveW3-rejected.patch`; no `skinny/RESULTS.md` row moved (`skinny/REDRESS.md:3357-3381`). |
| 115 | `1f2df230` | W4 generated dispatch / byte-set control slice | REJECTED | None | The scalar `container_tail_next` route passed malformed-tail, parity, gate/report, compile, regen, and build checks, but probe-first measurement falsified `random/direct_to_struct` before Criterion: Track 1 3518 Mbps across 20000 iterations and Track 2 3498 Mbps across 5000 iterations, both below the 7878 Mbps floor. The rejected source patch is `/tmp/skv11-waveW4-rejected.patch`; no result row moved (`skinny/REDRESS.md:3383-3409`). |
| 116 | `581121a3` | W5 bounded string span and special-byte scan | BLOCKED | None | W5 completed research, two plan passes, and two CHALLENGE passes, but CHALLENGE V2 did not converge. CH1 kept malformed-input parity at REVISE; CH4 kept cost at REVISE because the plan lacked a plausible independent Track 2 cost mechanism to lift `random/direct_to_struct` from 6949 Mbps to the 7878 Mbps floor. No source patch was attempted; `/tmp/skv11-waveW5-rejected.patch` is an empty marker; no source, generated runtime, SIMD kernel, bench body, gate/report schema, or result row moved (`skinny/REDRESS.md:3411-3432`). |
| 117 | `121eb557` | W6 escaped segment and hex decode slice | BLOCKED | None | W6 selected a `unicode_mixed/direct_to_struct` escaped-segment digest-fold plan, but CHALLENGE did not converge. CH3 is the hard block: `JsonDigestSink::*_source` decoded-byte fold reopens REDRESS 54 with REDRESS 55/66/69 adjacency, using the same sink seam, output contract, and allocation-removal claim as the rejected sink-local decoded stats/hash family. CH1, CH4, CH5, and CH6 also required revisions on fixtures, probes, Track 1/Track 2 independence, same-wave consumer evidence, and negative x4 proof. No patch was attempted; no row moved (`skinny/REDRESS.md:3434-3460`). |
| 118 | `ebf16418` | W7 output digest/hash host-sink | BLOCKED | None | All six CHALLENGE lenses accepted the no-source block. CH1 found no legal residual row/source/consumer/oracle candidate; CH2 found no generated non-JSON host-sink baseline; CH3 bound REDRESS 117 and 54; CH4 found the closest residual, `random/direct_to_struct`, could at most clear Track 1 under perfect visible-bucket removal while Track 2 stayed below floor. Evidence: `git diff --exit-code -- skinny/RESULTS.md` and the SK-V11-open advisory gate passed with unchanged `N-direct / NoGo`. No output digest/hash host-sink optimization, non-JSON host-sink baseline, direct-row movement, or reusable scalar oracle admitted (`skinny/REDRESS.md:3462-3493`). |
| 119 | `eca0eb94` | W8 direct residual fixpoint and row reclamation | FIXPOINT | None | W8 selected no source, no W8a split, no gate schema or validator semantic change, and no result row movement. Verification passed with `git diff --exit-code -- skinny/RESULTS.md` and the SK-V11-open advisory gate. Every residual direct row received a measured uncloseable/fixpoint proof tied to W3-W7 attempted or blocked routes. W8 admitted no direct row, no W0-clamped row, no source primitive, and no generated non-JSON intervention (`skinny/REDRESS.md:3495-3527`). |
| 120 | `db2c999b` | W9 close and Alpha feedback | CLOSE | None | W9 closed SK-V11 as a measured fixpoint and Alpha feedback packet. It made no behavior source, generated runtime, benchmark body, gate semantic, or `skinny/RESULTS.md` change. Verification passed with `git diff --exit-code -- skinny/RESULTS.md` and the SK-V11-open advisory gate. REDRESS 119 remains direct-row authority, while REDRESS 112 and 113 keep the generated non-JSON axis blocked. Routed remainder: solve generated non-JSON baseline first; treat the 13 direct residual rows as exhausted absent a material differential beyond REDRESS 114-119 with fresh profile and micro-proof evidence; keep W0-clamped admission pre-blocked by docs-only accounting; preserve strict-vs-strict comparator discipline (`skinny/REDRESS.md:3529-3553`). |

## Direct Residual Fixpoint Evidence

REDRESS 119 is the SK-V12 starting ledger for all residual direct rows. These
rows are not open SK-V11 implementation work; they are fixed unless a future
pass names a fresh material differential beyond REDRESS 114-119.

| Row | Track 1 | Track 2 | sonic direct | floor | SK-V12 carry-forward |
|---|---:|---:|---:|---:|---|
| `twitter/direct_to_struct` | 11613 | 10816 | 15113 | 13740 | W5 string-span route blocked by REDRESS 116; W7 digest route blocked by REDRESS 118; no W8a source candidate remains. |
| `canada/direct_to_struct` | 10316 | 9819 | 11700 | 10637 | W3 numeric route measured-rejected on sibling `mesh`; `canada` has a larger Track 2 floor gap; no W8a numeric candidate remains. |
| `github_events/direct_to_struct` | 11918 | 10596 | 14743 | 13403 | W5 string-span route blocked; W7 digest visible-bucket math cannot close both tracks; no W8a candidate remains. |
| `update_center/direct_to_struct` | 8187 | 7474 | 11064 | 10059 | W5 string-span route blocked; W7 digest route is floor-insufficient; no W8a candidate remains. |
| `mesh/direct_to_struct` | 8561 | 8652 | 9542 | 8675 | W3 `number_span_emit_slot` measured 3835 / 3614 against 8675 and was reverted; row remains uncloseable in SK-V11. |
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | W4 `container_tail_next` probe measured 3518 / 3498 against 7878 and was reverted; W5/W7 blocked; no W8a candidate remains. |
| `gsoc-2018/direct_to_struct` | 2665 | 2578 | 4110 | 3737 | Movemask/string-scan residual; W5 and W7 leave no accepted source authority; no W8a candidate remains. |
| `instruments/direct_to_struct` | 11569 | 10736 | 9865 | 8969 | Numerically above floor but W0-clamped; no W3-W8 measured behavior provenance, so docs-only admission is pre-blocked. |
| `numbers/direct_to_struct` | 4479 | 2366 | 2667 | 2425 | Track 2 misses floor and row is W0-clamped; W3 numeric route rejected; no W8a candidate remains. |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Track 2 misses floor and row is W0-clamped; W6 decoded-source route blocked by REDRESS 117; no W8a candidate remains. |
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | Unicode escape route blocked by W5/W6 and SK-V10 REDRESS 107/108 proof-only limits; no W8a candidate remains. |
| `distinct_values/direct_to_struct` | 1750 | 1625 | 2923 | 2658 | W5 string route blocked; W7 digest bucket insufficient; no W8a candidate remains. |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | Unicode escape/string route blocked by W5/W6 and prior proof-only limits; no W8a candidate remains. |

Citation: `skinny/REDRESS.md:3508-3527`.

## Binding Prior Blocks

REDRESS 96, 97, 98, and 102 remain binding in SK-V12.

- REDRESS 96 rejected the full class-column substrate plus move-consumed
  structural-index vector after correctness/parity checks passed but every W3
  must-improve row and every W10b maintain row failed.
- REDRESS 97 rejected the materially different allocation-free streaming cursor
  plus class lane after correctness/parity checks passed and the same W3/W10b
  floor family still failed.
- REDRESS 98 retired `G-W3-UNION-SUBSTRATE`: the union/event/class-column
  substrate thesis is falsified, not merely deferred.
- REDRESS 102 firewalled parse-only SOTA claims: parse-only rows remain
  diagnostic and no W3 union/event substrate, retained class column,
  structural or streaming cursor, `UnionTape`, parser-owned structural
  projection, or W4-through-W3 cascade-lock may dispatch.

SK-V11 SPEC carried those locks as a close condition: no W3 union/event
substrate, class column, structural-position vector, streaming cursor, class
lane, sidecar producer, parse-plane substrate repair, or cascade-lock through
W3 may dispatch; parse-only rows may serve only as guard, compatibility, or
diagnostic evidence (`restart/skinny/tranches/sk-v11/SPEC.md:39-44`).
SK-V11 Dispatch repeats the same posture: SK-V11 is not a W3 retry, REDRESS
96/97/98 retired the union/event/class-column substrate thesis, and REDRESS
102 firewalled parse-only SOTA claims
(`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:14-18`).

## Hard SK-V12 Pre-Blocks

1. No SK-V12 JSON-only micro-wave should dispatch before the generated
   non-JSON baseline problem is solved. REDRESS 112 rejected the baseline
   attempt because codegen/runtime remain JSON-profiled; REDRESS 113 blocks
   W2-style intervention from creating the first measurable non-JSON row.
2. Do not use W1a's companion report lane as generated non-JSON baseline
   authority. W1a admitted only gate/report consumption with non-admitting
   placeholder evidence and no row movement.
3. Do not spend SK-V12 reopening the 13 SK-V11 direct residual rows unless the
   plan names a material differential beyond REDRESS 114-119 with fresh profile
   and micro-proof evidence. REDRESS 119 is already the measured fixpoint.
4. Do not admit W0-clamped direct rows by docs-only accounting. `instruments`,
   `numbers`, and `unicode_mixed` require behavior or gate-wave measured
   provenance before admission; opening throughput alone is planning evidence
   only (`restart/skinny/tranches/sk-v11/SPEC.md:29-35`).
5. Do not retry `number_span_emit_slot` or `container_tail_next` under a new
   name. The former missed `mesh` at 3835 / 3614 Mbps against 8675; the latter
   missed `random` at 3518 / 3498 Mbps against 7878.
6. Do not reopen the W5 bounded string span route without first closing the
   malformed fixture parity and independent Track 2 cost objections that kept
   CH1 and CH4 at REVISE.
7. Do not reopen the W6 `JsonDigestSink::*_source` decoded-byte fold. CH3 tied
   it to REDRESS 54 with REDRESS 55/66/69 adjacency.
8. Do not claim an output digest/hash host-sink source route from SK-V11 W7
   evidence. W7 found no legal row/source/consumer/oracle candidate and no
   generated non-JSON host-sink baseline; the closest residual could not clear
   both Track 1 and Track 2 floors.
9. Preserve strict-vs-strict comparator discipline. Direct admission still
   needs generated Track 1, independent Track 2/oracle, same-run sonic-rs
   strict direct comparator on the digest plane, output-plane match,
   provenance, and gate consumption. Typed admission cannot come from direct
   digest evidence.
10. Keep REDRESS 96/97/98/102 binding: no W3 union/event/class-column,
    structural-position vector, streaming cursor, class lane, sidecar producer,
    parse-only SOTA movement, parse-plane substrate repair, or W4 cascade-lock.
11. Keep generic JSON policy out of generic crates and runtime outside
    generated per-grammar modules. A non-JSON proof must be generated,
    measured, oracle-backed, and gate-consumed, not asserted in prose.
12. Keep the existing process locks: no parser-owned sidecar/fact slot, new
    directive/BIR variant/public substrate API, x86 implementation work,
    orphan kernel, proof-only producer, or telemetry field without same-wave
    report/gate/fixture consumption (`restart/skinny/tranches/sk-v11/SPEC.md:174-194`;
    `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:168-205`).

## Routes Eligible Only Under Materially Different Framing

| Route family | May admit only if... | Material differential required |
|---|---|---|
| Generated non-JSON baseline | SK-V12 makes the baseline the first-class wave, not an intervention side effect. | Owner authority to replace or bypass the JSON-only `json_provider` path; one generated non-JSON direct or typed Track 1 parser; independent oracle/Track 2 on the same output plane; strict output equality; measured throughput; gate/report consumption; no JSON policy leakage. |
| CSS L4 generated intervention | A generated non-JSON baseline already exists and defines a measurable floor. | W2 cannot create the first measurable row. The intervention must consume the baseline, clear a named threshold, and keep generated metadata grammar-neutral. |
| Direct residual row work | Alpha-E/S-P3 supplies a fresh profile-backed candidate beyond REDRESS 114-119. | New hot-leaf evidence, scalar/oracle proof, same-host microbench, same-wave generated consumer, strict same-run comparator, independent Track 2/oracle, both floors clear, and guards hold. A renamed W3-W7 route is not enough. |
| Numeric direct work | It is not `number_span_emit_slot` replay and does not change generic number policy. | A materially different generated numeric consumer or grammar-neutral fact path, fresh probes showing both Track 1 and Track 2 plausibility, and no fallback/mantissa/f64 policy rewrite. |
| Dispatch/byte-set control work | It is not `container_tail_next` replay and does not introduce object/key/value-byte carry outside a same-loop generated product consumer. | Fresh proof that both Track 1 and independent Track 2 can clear selected floors, with no sidecar, retained cursor, hidden fact table, or generic JSON policy. |
| String span work | CH1 malformed-input parity and CH4 independent Track 2 cost objections are closed before source redress. | Full malformed string/key/value/array fixture rejection across generated Track 1, independent Track 2, `serde_json`, and `sonic-rs`; a plausible Track 2 cost path; same-wave caller evidence. |
| Escaped segment / hex decode work | It avoids the REDRESS 54/55/66/69 decoded-source sink seam and does not rely on existing `unescape_string` as the same-wave source delta. | New source representation, strict scalar/checkasm parity where applicable, sampled consumer evidence, negative x4 proof, Track 1/Track 2 independence, and policy kept per grammar. |
| Output digest/host sink | Fresh post-W6 profiling shows a legal host-sink hot leaf that can clear both tracks. | A materially different host-sink representation below the pre-blocked decoded-string seam, independent oracle, same-run comparator evidence, and same-wave gate/report consumption. |
| Companion non-JSON report lane | It remains accounting/gate infrastructure only. | It can support future baseline admission by consuming evidence, but cannot itself count as generated Track 1, independent oracle, or benchmarked grammar generalization. |

## Alpha-C Handoff

Alpha-E and Alpha-F should treat SK-V11 as a measured-fixpoint close with one
admitted infrastructure win and no row movement. The useful carry-forward is
the W1a non-JSON gate/report lane; the first SK-V12 material problem is the
generated non-JSON baseline. The 13 residual direct rows should be marked
exhausted unless a candidate supplies a new material differential beyond
REDRESS 114-119. REDRESS 96/97/98/102 remain hard negative authority, and
parse-only or W3-adjacent work must fail closed.
