# SK-V12 W1a CH1 Correctness - GrammarConfig + Lock 14

Date: 2026-05-20.
Lens: CH1 Correctness.
Output: `restart/skinny/tranches/sk-v12/research/w1a/challenge/CH1-correctness.md`.

## Verdict

REVISE.

The plan is directionally correct: a codegen-private provider boundary, JSON-local generated config, a `gate-json`-consumed Lock 14 scan, fresh JSON guard refresh, and no CSS row in W1a are the right shape. It is not CH1-acceptable yet because the plan does not make all seven leak closures and JSON floor preservation fully executable. This is a revision, not a rejection: the selected route can work if the required changes below are folded into the plan before redress.

## Findings

### 1. Seven-leak closure is named but not fully executable

SPEC Section 4 requires a `GrammarConfig` or equivalent generated metadata surface for structural alphabet, FIRST/follow tables, layout/trivia, escape policy, number policy, flag semantics, and sink/view/kind bindings, then requires JSON policy to move out of generic code while preserving JSON parity and floors (`restart/skinny/tranches/sk-v12/SPEC.md:332-340`). The user pin is stricter: the seven Lock 14 leaks must be resolved by W1's `GrammarConfig` surface before CSS L4 emission is legal (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:101-103`).

The plan names all seven leaks (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:83-98`; `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:118-134`) and defines a `GrammarProfile` carrying `bindings`, `dispatch`, and `literals` (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:51-62`). But the generated `config.rs` shape and the required template rewrites only explicitly consume structural, layout, string, number, and flag policy (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:92-138`). The plan then allows JSON object/pair rules, literal arms, views, and sink callback names to remain in JSON-owned templates (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:138`).

That containment can be correct, but it is not yet proved. Leaks 2, 5, and 7 are exactly value dispatch, object/key/member policy, and sink/view/kind binding (`restart/skinny/tranches/sk-v12/research/w1a/A1-codegen-template-leaks.md:28-33`). A scan that excludes JSON-owned roots (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:148-150`, `:179-183`) only proves generic roots are clean; it does not prove CSS/non-JSON emission cannot accidentally reuse the excluded JSON-owned roots later.

Required change:

- Add a leak-closure matrix to the plan. For each of the seven leaks, state whether it is moved into generated config and consumed, or quarantined in a JSON-owned root that is unreachable from non-JSON providers.
- Add codegen tests or gate checks proving a non-JSON backend without a provider fails at provider selection and never enters `json_provider`, `json_templates`, `json_sink_direct`, or `json_typed_direct`.
- If `GrammarProfile.dispatch`, `literals`, or `bindings` are the claimed fix for leaks 2, 5, or 7, require a generated consumer for those fields in the same commit. Otherwise remove the unused fields from the correctness claim and classify those leaks as JSON-owned containment with an executable reachability proof.

### 2. JSON guard-floor preservation is still partly prose

The plan correctly requires a refreshed native JSON guard run because the selected patch touches JSON-producing codegen/runtime paths (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:144-148`; `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:246-250`). It also lists the SPEC Section 0.5 direct and typed floors (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:232-244`).

However, A4 found that current executable gate logic does not exactly enforce the SK-V12 Section 0.5 floor table and says W1a must either add that table to gate/report validation or run a separate mechanical floor check after native guard refresh (`restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md:167-170`). P2 says the additive validator is "the only justified report-path change" but phrases it as "If added" (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:194-203`). That leaves a pass path where `gate-json` can be cited while the exact SK-V12 floors are only manually asserted.

Required change:

- Make the SK-V12 direct/typed floor check mandatory and executable. Either add the additive validator in `skinny/crates/bbnf-bench/src/report.rs`/gate plumbing, or name a deterministic checked-in command that parses the refreshed result and fails on any floor miss.
- Require REDRESS 121 to cite the exact floor-check command and result. A manual table in the plan or REDRESS text is not enough for CH1.

### 3. Orphan-config prevention is underspecified

The plan has the right same-wave consumer rule: generated JSON parser/view/direct/typed modules must import and exercise `super::config`, and `gate-json` must consume the generic-crate scan in the same commit (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:184-190`; `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:207-209`). That prevents the obvious orphan case of an emitted `config.rs` with no generated runtime consumer.

It does not yet prevent a partial orphan: `config.rs` may be consumed for structural/string/number/flag policy while `GrammarProfile.dispatch`, `literals`, and `bindings` exist only as inert facade fields. Because those fields correspond to leaks 2, 5, and 7, this would let the plan claim all seven leaks while only five are exercised by generated output.

Required change:

- Add an orphan-config check that enumerates every generated config/profile policy used to satisfy the seven-leak matrix and verifies a same-wave generated consumer for it.
- The check may be unit-level, codegen snapshot-level, or a gate scan, but it must fail if a config/profile field is added and not consumed by generated JSON output or by the `gate-json` Lock 14 path.

## Correctness Notes

- JSON behavior preservation is otherwise well routed: the plan forbids `not_refreshed:no_behavior_drift` for this patch, requires native refresh, byte-exact `RESULTS.md`, JSON parity/conformance, and no CSS/non-JSON result row (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:129-148`, `:173-182`).
- The no-IR/public-substrate boundary is correct for CH1. SPEC allows IR only if required, then forbids new directive/BIR/`BackendShape`/public substrate API at exit (`restart/skinny/tranches/sk-v12/SPEC.md:318-346`), and the plan keeps IR/tape/runtime public surfaces read-only unless a blocker proves otherwise (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:42-53`).
- W1a correctly avoids claiming CSS L4 admission. SPEC says no CSS parser row is claimed yet (`restart/skinny/tranches/sk-v12/SPEC.md:341-345`), and the plan keeps CSS generation and lightningcss comparison in W1b (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:15-18`).

## Required Plan Revision Summary

1. Add the seven-leak closure matrix with executable proof for config consumption or JSON-owned containment.
2. Make SK-V12 JSON direct/typed floor enforcement a mandatory command, not an optional/manual check.
3. Add a same-wave orphan-config/profile-field check covering every policy field used to satisfy the seven leaks.

After these revisions, CH1 would be able to ACCEPT the route if the plan still preserves the existing owner paths, no-CSS-row boundary, fresh JSON guard refresh, and `gate-json` Lock 14 consumer.
