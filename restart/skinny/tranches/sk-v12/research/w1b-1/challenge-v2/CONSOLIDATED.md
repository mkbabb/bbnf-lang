# SK-V12 W1b-1 CHALLENGE V2 - Consolidated Disposition

Date: 2026-05-20.
Phase: W1b-1 CHALLENGE V2.
Scope: adversarial review of Plan V2 after CHALLENGE V1 REVISE.

## Disposition

ACCEPT FOR REDRESS, fail-closed.

Plan V2 repairs the two V1 plan-time blockers:

- SPEC Section 6 and the plan now own
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, so the mandatory
  `gate-json` Lock 14 path can be updated in-wave.
- The budget section now defines a narrowed single-redress scaffold and a hard
  0.9x-cap stop rule instead of an omnibus CSS compiler plus measurement brief.

All six lenses accept redress with hard preconditions. W1b-1 is authorized to
attempt the scalar generated CSS L4 Track 1 + independent `cssparser` oracle
scaffold only. It is not authorized to claim CSS SOTA admission, use
`lightningcss`, touch SIMD/aarch64, alter main `RESULTS.md` columns, or
substitute Sheets/BBNF-self/JSON/root-CSS runtime if the CSS scaffold fails.

## Lens Results

| Lens | Verdict | Load-bearing preconditions |
|---|---|---|
| CH1 correctness / generation / equality | ACCEPT FOR REDRESS | Generated runtime proof, strict fact-stream equality, retained artifacts, no lightningcss overclaim. |
| CH2 generality / Lock 14 | ACCEPT WITH HARD REDRESS PRECONDITIONS | W1b-1 Lock 14 authorization must stay Section-6-only and must not widen substrate/IR/pass/SIMD/directive/BackendShape surfaces. |
| CH3 regression / REDRESS / JSON guard | ACCEPT WITH HARD REDRESS PRECONDITIONS | REDRESS 123 on either outcome; refreshed JSON guard state; `RESULTS.md` unchanged unless a measured demotion is explicitly recorded. |
| CH4 cost / LOC / generated size / deps | ACCEPT-CONDITIONAL | <=360 counted hand source LOC, scalar-only scope, bench-only `cssparser`, generated-size telemetry, 0.9x stop rule. |
| CH5 hidden coupling / oracle independence | ACCEPT FOR REDRESS | No root CSS runtime, generated JSON, `json_provider` reuse for CSS, `lightningcss`, `parse_that_regex`, or `bbnf-simd` coupling in Track 1/oracle. |
| CH6 anti-paper-close | ACCEPT FOR REDRESS, fail-closed | Gate consumes every emitted field; generated proof is real; equality is retained bytes, not digest-only/report-only evidence. |

## Redress Boundary

The redress agent may edit only SPEC Section 6 owner paths. PASS requires:

1. CSS-owned provider/profile emits `mod.rs`, `config.rs`, `parser.rs`, and
   `generated.rs`, and a codegen reproducibility test byte-compares generated
   output against the committed runtime directory.
2. Track 1 and oracle produce byte-identical
   `css_l4_declaration_value_fact_stream` for
   `css_l4/declaration_values/direct_to_struct/main`.
3. The oracle is `cssparser` backed and independent of generated Track 1,
   generated JSON, root CSS runtime, `lightningcss`, `parse_that_regex`, and
   `bbnf-simd`.
4. The companion SK-V12 non-JSON report/gate consumes strictness,
   grammar/input checksums, input bytes, validation/profile artifacts,
   generated LOC/module bytes, grammar-size guard, Lock 14/16 status,
   scalar-reference status, parity status, retained artifact paths, and JSON
   guard state.
5. `lock14_status = pass` comes only from the executable
   `lock14_baseline::validate` path in the same gate run.
6. `json_guard_state` is refreshed for W1b-1 because codegen selection, runtime
   export, report/gate validation, and bench dependencies move.
7. `skinny/RESULTS.md` remains unchanged in W1b-1 unless a JSON guard miss
   requires a measured REDRESS demotion.

If any required condition fails after source edits, save
`/tmp/skv12-waveW1b-1-rejected.patch`, revert only the W1b-1 slice, and record
`BLOCKED/FAIL` in REDRESS 123.

## Artifacts

- `CH1-correctness-generation-equality.md`
- `CH2-generality-lock14.md`
- `CH3-regression-redress-json-guard.md`
- `CH4-cost-loc-size-deps.md`
- `CH5-hidden-coupling-oracle-independence.md`
- `CH6-anti-paper-close.md`
