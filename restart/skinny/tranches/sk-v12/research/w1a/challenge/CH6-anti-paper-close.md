# SK-V12 W1a CH6 Challenge - Anti-Paper-Close

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Lens: CH6 anti-paper-close.
Owned artifact: `restart/skinny/tranches/sk-v12/research/w1a/challenge/CH6-anti-paper-close.md`.

## Authorities Read

- `docs/precepts/instructions/tranche/CHALLENGE.md`.
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 4.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/PLAN.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A1-codegen-template-leaks.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A2-runtime-grammar-config.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A3-lock14-gate-consumer.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A5-ir-metadata-boundary.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A6-json-guard-redress.md`.

## CH6 Question

Can the W1a plan close on prose, unused metadata, schema-only non-JSON evidence,
or future CSS promises?

Answer: not if the plan is executed as written. W1a is a legality gate only. It
may make generated CSS L4 emission legal for later W1b work, but it cannot admit
a CSS row, claim lightningcss comparison, claim SOTA movement, open fallback, or
close SK-V12.

## Findings

1. Prose-only close is blocked. The plan requires the W1a generic-crate
   neutrality scan to run through `lock14_baseline::validate` on the existing
   `bbnf-bench --bin gate` / `xtask gate-json` path. Unit tests and prose
   neutrality claims are explicitly insufficient unless `gate-json` consumes the
   scan and preserves `RESULTS.md` exactness.

2. Unused metadata is blocked. A generated JSON `config.rs` or equivalent
   metadata surface must be imported and exercised by generated JSON
   parser/view/direct/typed modules in the same redress commit. An emitted
   metadata file without that same-wave generated consumer is an orphan and
   fails W1a.

3. Schema-only non-JSON evidence is blocked. The Lock 14 scan is a gate
   precondition, not a report row, outcome, or `RESULTS.md` movement. The plan
   forbids report schema/outcome churn for the scan and says no CSS/non-JSON row
   may be added in W1a. Existing SK-V12 non-JSON companion report checks are
   regression checks only; they do not establish CSS L4 admission or SK-V12
   close.

4. JSON guard papering is blocked. Because the selected work moves JSON-producing
   codegen/runtime paths, `not_refreshed:no_behavior_drift` is not valid for
   the selected plan. REDRESS 121 may pass only with refreshed native JSON guard
   evidence, exact SPEC Section 0.5 direct/typed floors, and `RESULTS.md`
   exactness, or it must record measured demotion/rejection.

5. Future CSS promises are blocked. W1a expressly does not emit CSS L4, add a
   CSS benchmark row, compare to lightningcss, open Sheets or BBNF-self fallback,
   or claim SOTA. W1b still must create the generated CSS L4 row, strict oracle,
   same-plane lightningcss comparator, equality evidence, and gate consumption.

6. The remaining CH6 hazard is redress wording, not plan shape. Any REDRESS 121
   PASS text must say legality only: seven Lock 14 leaks resolved, generated
   JSON metadata consumed, JSON parity/guard floors passing, generated size
   recorded, no public substrate/IR expansion, no CSS parser row, no
   lightningcss result, no SOTA claim, no SK-V12 close, and no fallback opened.

## Must-Fix Before Redress

None.

## Redress Reject Conditions

Reject W1a if any of these occur:

- REDRESS 121 claims PASS without a `gate-json` path consuming
  `validate_generic_crate_neutrality`.
- Generated metadata exists but generated JSON parser/view/direct/typed code does
  not use it in the same redress commit.
- A non-JSON companion report, fixture, or schema validation is treated as CSS
  row admission, SOTA evidence, fallback authority, or SK-V12 close evidence.
- `not_refreshed:no_behavior_drift` is recorded after JSON-producing paths move.
- `RESULTS.md` changes without a fresh native JSON guard refresh and follow-up
  exact `gate-json --check-results`.
- A CSS runtime/generated parser path, CSS benchmark row, lightningcss
  comparator result, Sheets fallback, BBNF-self fallback, directive, BIR variant,
  `BackendShape`, public substrate API, or IR edit lands in W1a.
- Future wording such as "wired", "integrated", "ready for W1b", or "legalized"
  is used as a substitute for executable gate evidence.

## Disposition

DISPOSITION: ACCEPT

The W1a plan is redressable under CH6. It cannot close on paper evidence if
redress preserves the plan's own pass predicate: executable Lock 14 gate
consumption, same-wave generated metadata consumption, refreshed JSON guard
floors, `RESULTS.md` exactness, and explicit non-admission of CSS/SOTA/fallback
claims.
