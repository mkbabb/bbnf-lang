# SK-V15 W2-F - Authority And Omega Risk

Scope: W2 documentation alignment across SK-V15 SPEC, DISPATCH, HANDOFF,
S-P0 A3, and the V1 implementation-overfit audit.

## Binding Criteria

W2 is `Lock 14 / Lock 16 gate restoration`: risk High, manual budget 120-280
LOC, generated output limited to reports/fixtures, docs 80-180 LOC, entry gate
`W1 admitted or CSS blocked`, and exit gate `Gates report roots/exclusions and
source-present primitive status; self-exemptions fail`
(`restart/skinny/tranches/sk-v15/SPEC.md:176`).

W2 tasks are:

- restore scan coverage for previously omitted codegen/runtime/bench/gate roots
- emit gate-exclusion reports and make gates consume them
- classify source-present SIMD/ASM primitives as wired, scalar-delegated,
  deleted, blocked, or strict-checkasm admitted

The plan must name scan roots, exclusion report schema, fail-closed tests, and
primitive status classification. Because W2 changes gate close semantics, it
is a mandatory seven-lens CHALLENGE candidate unless proven ledger-only; W2 is
not ledger-only.

## Dependency Rows

W2 cites `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`: W2 must expose provider/template
roots through Lock 14 scan coverage, but provider/template deletion waits for
W3/W6 proof. There is no standalone `DEP-W2-*` delete row. If W2 tries to
delete, retire, demote, or neutralize a live claim beyond gate restoration, it
must reject or route before redress.

## Omega Risk

No binding SPEC/DISPATCH/HANDOFF contradiction forces Omega before W2
implementation. G-Omega V9 already authorizes the V1 corpus CRUD patches, and
W2 is an implementation wave under the current locked SK-V15 spec.

Non-forcing doc drift:

- The V1 consolidated audit cites `skinny/xtask/src/lock14_baseline.rs`; the
  current file is `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- The V1 audit's early PRUNE-WAVE-A wording about `CSS_GENERATED_RS` is now
  constrained by SK-V15 SPEC dependency rows: W6 retires CSS generated proof
  only after W5 typed CSS provider proof.

Omega becomes necessary only if W2 needs a LOCKS/SPEC/wave-graph amendment or
hits an invariant violation that cannot be repaired by W2 redress, revert, or
intrinsic-block proof.
