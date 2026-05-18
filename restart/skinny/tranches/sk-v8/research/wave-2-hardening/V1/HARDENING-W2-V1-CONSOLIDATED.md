# SK-V8 W2 Hardening V1 Consolidated

Date: 2026-05-18.

Target reviewed: `12aff1e4`
(`feat(sk-v8-wave2-typed): add Apache and CITM typed product rows`).

## Verdict

REVISE.

V1 returned five REVISE lanes and one ACCEPT lane.

| Lane | Verdict | Confidence | Required fold |
|---|---|---:|---|
| CH1 | REVISE | 88 | Results/closure language, Lock 14 accounting, Track 2/oracle wording |
| CH2 | REVISE | 91 | REDRESS/HANDOFF fold, source-only row posture, oracle wording |
| CH3 | REVISE | 92 | Lock 14 parent-diff allowance, schema identity, Apache root fields |
| CH4 | REVISE | 90 | Lock 14 accounting, strict/output-plane wording, Track 2/oracle wording |
| CH5 | ACCEPT | 91 | Accepts W2 if REDRESS keeps Canada route-out and no-RESULTS posture |
| CH6 | REVISE | 92 | Anti-paper-close posture, W2 hardening consolidation, schema identity |

## Consolidated Findings

The W2 source/product slice is acceptable in shape. It adds Apache and CITM
only through the existing real typed schema, generated DirectBuild output,
serde/sonic carriers, checksums, and full-fixture parity tests. It does not
touch parser/runtime/tape/substrate/direct digest/materialization surfaces and
does not add a directive, BIR variant, `BackendShape`, `UnionTape`, sidecar, or
parser-owned cursor.

The challenge found four blocking governance defects before admission language
can close:

1. Lock 14 treats the W2 typed owner paths as frozen parent-diff movement.
   The gate must distinguish wave-authorized typed product movement from
   runtime/parser/substrate/direct movement.
2. The generated schema identity still says `sk-v7-real-typed-v2` after the W2
   root-set expansion.
3. The W2 plan and REDRESS overcount oracle independence. `track2_typed`
   delegates to serde_json, so the distinct strict engines are generated
   Track 1, serde_json as the Track 2/oracle path, and sonic-rs.
4. W2 status text must not claim measured `apache_builds/real_typed_struct` or
   `citm_catalog/real_typed_struct` rows in `skinny/RESULTS.md`. The current
   manifest remains W0's four measured real-typed rows. W2 may admit
   source/product parity and reject benchmark row-table admission for this wave,
   but it cannot claim six measured `real_typed_struct A / GO` rows.

## Required V2 Fold

V2 must:

- Update Lock 14 so only W2-scoped commits whose parent diff is confined to the
  three real typed owner paths are authorized.
- Bump or otherwise resolve the generated real typed schema identity.
- Fold Apache root `mode` and `nodeName` into the W2 host/API schema facts.
- Replace Track 2 wording with serde_json-as-oracle wording.
- Record REDRESS 91 as source/product parity admission plus benchmark
  row-table rejection for this wave, with `skinny/RESULTS.md` unchanged.
- Re-run source parity gates, generated checks, frozen-surface audits, and the
  focused Lock 14 test after the fold.
