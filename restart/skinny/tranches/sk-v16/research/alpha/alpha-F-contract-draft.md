# Alpha-F - Contract Draft - SK-V16 V1

Pass: Pass Alpha. Cycle: SK-V15 -> SK-V16.
Date: 2026-05-28.
Scope: `SYNTHESIS.md` + `HANDOFF.md` draft basis.
Output: this file.

## Draft Close Condition

SK-V16 closes only when all of these hold:

1. JSON 51 / 51 remains admitted on the strict measured planes.
2. CSS L4 has a grammar-derived provider from `grammar/css/l4/*.bbnf`; legacy
   string-literal generated proof is not live admission.
3. CSS exposes typed document/value/view/visitor surfaces, and Track 1 typed
   output matches cssparser same-workload typed summary before speed counts.
4. CSS SOTA is measured on Apple M5 Max / aarch64 and beats cssparser on the
   same typed workload; lightningcss remains diagnostic until CSSOM/value parity.
5. Dirty generated CSS and generated real-typed state is either retired, cleanly
   regenerated, or intrinsically blocked with row-level proof. The proof floor
   is an exact dirty-file manifest, `git status --short`, broad command result,
   owner/disposition per file, and a rule that consumed dirty generated files
   block close unless they are external to the wave and covered by intrinsic
   proof.
6. Pattern H advances from line-1 provenance to generator-owned collapse; count
   remains 67 and no header-only close is accepted.
7. Lock 14 / Lock 16 gates report their own exclusions and reject silent
   self-exemption.
8. Decision Engine and the five BackendShape lowerers remain load-bearing and
   grammar-neutral.
9. Any native SIMD work is profile-first, scalar-referenced, checkasm/parity
   verified, same-wave consumed, and aarch64-only.
10. FNV remains bench-only unless a future production contract proves typed
    semantics independently of hash sidecars.
11. PASS-IMPL V3 accepts every axis or records row-level intrinsic-block proof.

## Telemetry Binding

SK-V16 inherits the SK-V15 telemetry schema and adds close-required fields for
CSS typed equality:

| Field | Required meaning |
|---|---|
| `css_track1_typed_passes` | number of corpus files parsed by Track 1 typed CSS |
| `css_cssparser_typed_passes` | number of corpus files parsed by cssparser comparator |
| `css_typed_summary_equal` | boolean equality gate before speed admission |
| `css_provider_source` | grammar source path or explicit non-admission |
| `dirty_generated_state` | clean / retired / routed-intrinsic-block |
| `native_simd_status` | scalar / parity-pass / checkasm-pass / not-applicable |

## Receiver List

S-P3 may split the candidates, but it must preserve this dependency order:

1. Dirty generated state classification before broad generated checks count.
2. Grammar-derived CSS provider before legacy CSS proof deletion.
3. Typed CSS equality before CSS speed admission.
4. Profile-first native SIMD after a typed workload exists.
5. Pattern H generator-owned collapse after regeneration authority is proven.

Alpha does not author `SPEC.md` or `DISPATCH-PROMPT.md`. Those files are S-P3
outputs after S-P0/S-P1/S-P2 convergence. Alpha supplies only the measurable
goalset, telemetry binding, and pre-blocked routes.

S-P3 must bind executable gate consumers for the new report classes:
`--skv16-css-typed-report`, `--skv16-dirty-generated-report`,
`--skv16-pattern-h-roundtrip-report`, and, if native SIMD is in scope,
`--skv16-native-simd-report`.

S-P3 must also quote each package's manual source/test LOC budget,
generated-output status, docs/ledger LOC budget, phase hard cap, split trigger,
and same-commit consumer callsite before redress. Generated output cannot hide
manual scope.

Hidden-coupling escapes are forbidden unless routed through Pass Omega and
G-Omega: retained sidecars, retained sidecar tables, sidecar event vectors,
retained cursor/list, cursor streams, aux density/projection tables,
parser-owned structural projections or streams, parallel source passes, second
tapes, public `UnionTape`, new substrate APIs, sixth `BackendShape`, production
FNV arbiters, production hash-correctness proof, and wrong-plane comparator
admission. The full inherited REDRESS pre-block semantics in `SYNTHESIS.md` and
`alpha-C-redress-digest.md` are binding on S-P0 through S-P3.

## Gate Posture

`PASS-ALPHA.md` and `ORCHESTRATOR.md` describe G-Alpha as mandatory. The active
user pin says only G-Omega is mandatory and every other gate auto-passes. SK-V16
records the conflict plainly and follows the active user pin: do not stop for
G-Alpha. Stop only at G-Omega, unrepaired invariant violation, or completed
SK-V17 close.

Alpha hardening runs CH1-CH7. CH7 overfit-prune cannot be deferred to S-P0 or
folded into CH6.
