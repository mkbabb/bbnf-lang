# SK-V12 W1a CHALLENGE V2 Consolidated

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: CHALLENGE V2.
Disposition: REVISE.

## Lens Results

| Lens | Artifact | Disposition | Binding finding |
|---|---|---|---|
| CH1 correctness | `CH1-correctness.md` | REVISE | Add `scan.rs` / `sink.rs` ownership or defer their edit, make `json_templates/mod.rs` concrete, and add an executable orphan config/profile-field consumer check. |
| CH2 generality / Lock 14 | `CH2-generality-lock14.md` | REVISE | The generic `passes` root contains a JSON structural alphabet literal while excluded from ownership; scan policy and stub coverage must be made executable. |
| CH3 regression / REDRESS | `CH3-regression-redress.md` | REVISE | Rejected-patch coverage omits `scan.rs` / `sink.rs` if they move. |
| CH4 cost / size | `CH4-cost-size.md` | ACCEPT | V2 removes baseline-failing `lint-loc`, broad report/xtask/bin-gate changes, and has adequate generated-size accounting. |
| CH5 hidden coupling | `CH5-hidden-coupling.md` | REVISE | `scan.rs` / `sink.rs` must be owned source if their generated provenance is changed; retained renderer stubs need deletion or scan coverage. |
| CH6 anti-paper-close | `CH6-anti-paper-close.md` | ACCEPT | V2 preserves the no-CSS/no-SOTA/no-fallback boundary and requires same-wave consumers. |

## Required Plan Revision

Before redress, W1a needs a V3 plan that:

1. Adds `skinny/crates/runtime/src/grammars/json/scan.rs` and
   `skinny/crates/runtime/src/grammars/json/sink.rs` to editable source and
   rejected-patch rosters if their headers/provenance move.
2. Makes `json_templates/mod.rs` a concrete not-owned path unless redress proves
   otherwise and returns to plan.
3. Names an executable orphan config/profile-field consumer check.
4. Adds the affected `skinny/crates/passes/src/lib.rs` generic leak to the
   owner roster with a narrow removal/derivation route, or obtains a later
   CHALLENGE acceptance for a different scan policy.
5. Makes Rust scan and manual sanity command test exclusion agree.
6. Requires deletion or scan coverage for retained `sink_direct.rs` /
   `typed_direct.rs` compatibility stubs.

W1a source redress remains blocked until a revised plan passes CHALLENGE.
