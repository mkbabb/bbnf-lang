# SK-V12 W1a CH2 Challenge - Generality And Lock 14

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Lens: CH2 generality / Lock 14.
Owned artifact: `restart/skinny/tranches/sk-v12/research/w1a/challenge/CH2-generality-lock14.md`.

## Authorities Read

- `docs/precepts/instructions/tranche/CHALLENGE.md`.
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 2.1.
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
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`.

## CH2 Question

Does the W1a plan genuinely satisfy Lock 14 while avoiding grammar-name
branches, public substrate APIs, IR/BIR/`BackendShape` expansion, and JSON policy
leaks in generic code?

## Finding

DISPOSITION: ACCEPT

The W1a plan is acceptable under CH2 as a plan-level legality route. It selects
a codegen-private `GrammarProfile` plus per-grammar generated metadata, consumes
a generic-crate Lock 14 scan through the existing gate path, preserves JSON
guard evidence, and forbids CSS row admission. This is not a closure proof:
redress still fails if the implementation turns the provider selector into a
hardcoded grammar-name policy branch or leaves any of the seven JSON policy
classes in scanned generic roots.

## Evidence Matrix

| Claim | Disposition | Evidence | Plan consequence |
|---|---|---|---|
| Lock 14 is the controlling generality rule for every generic-crate edit. | Accepted. | SPEC Section 2.1 bans parser/type names, grammar-name branches, JSON structural/string/number/object/flag/sink policy in generic code, and assigns those facts to per-grammar generated modules. Section 4 requires generated metadata, JSON policy removal from generic code, a Lock 14 scan/gate consumer, JSON parity/floors, no CSS row, and no directive/BIR/`BackendShape`/public substrate API. | W1a cannot pass by prose, by schema presence, or by existing JSON behavior. The generic scan and same-wave gate consumer are required evidence. |
| The provider selector is a legal data-driven boundary, not a generic grammar-name branch. | Accepted, narrowed. | The plan permits only a selector comparing provider ids to `backend.grammar_name`; grammar-specific literals and policy stay in provider-owned modules. PLAN-P1 says literal `"json"` lives only in `json_provider`, not generic files. PLAN-P2 requires fail-closed profile lookup and says grammar-specific literals/policy stay in per-grammar profile modules. | The generic selector may be variable-to-variable lookup only. A generic `match backend.grammar_name { "json" => ... }`, `if grammar_name == "css_l4"`, or any branch body containing grammar policy is a CH2 failure even if it compiles. |
| Public substrate APIs are avoided. | Accepted. | PLAN marks public `runtime::tape::GrammarConfig`, new directive/BIR/`BackendShape`, and public substrate API as not authorized. A2 requires module-owned `pub(crate)` generated metadata and explicitly forbids exporting `runtime/src/lib.rs` grammar config or public `tape::GrammarConfig`. | No public `GrammarConfig`, generic `TapeBuilder<C>`, `UnionTape`, generic sink super-trait, sidecar vector, retained cursor/list, or public grammar policy constant may land in W1a. |
| IR, BIR, and `BackendShape` expansion are unnecessary and forbidden. | Accepted. | A5 finds `BackendIr`, recognizers, existing five `BackendShape` variants, `BackendExpr` variants, `DirectBuild*`, and `ShapeFacts` sufficient. The selected plan keeps lowering and `BackendShape` selection unchanged and lists `skinny/crates/ir/src/**` as read-only or not owner paths. | Redress must show `BackendShape` remains the five existing variants, `BackendExpr` is unchanged, directive acceptance is unchanged, and no IR edit lands. |
| The seven JSON policy leaks are completely in scope. | Accepted. | A1 and `skv12-value-api-audit.md` name the seven leak classes: JSON structural alphabet, value dispatch, string/escape policy, number policy, object/key/member policy, `OffsetFlags` meaning, and JSON sink/view/kind bindings. PLAN and PLAN-P2 require closing or explicitly failing all seven, with JSON names legal only in generated JSON modules or explicitly JSON-owned provider/template roots excluded from generic reuse. | The plan cannot claim Lock 14 if any scanned generic root still contains `STRUCTURAL_ALPHABET_JSON`, JSON punctuation tables, JSON string/number helpers, JSON key/colon policy, JSON `OffsetFlags` interpretation, `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, `JsonVisitor`, or JSON direct callback shape. |
| The Lock 14 scan is a gate precondition, not a report/schema leak. | Accepted. | A3 routes the scan through `lock14_baseline::validate` inside `bbnf-bench --bin gate`; it explicitly says not to change `Outcome`, schema headers, `SkV12NonJsonReport`, or `RESULTS.md` for the scan. PLAN-P2 repeats that no report schema or outcome change is selected and that the scan is not a row. | A pass requires `gate-json` to execute the scan. Adding outcome ids, report fields, markdown rows, or `RESULTS.md` movement for the scan reopens a policy leak and fails CH2. |
| JSON guard and CSS non-admission are preserved. | Accepted. | USER PIN keeps Lock 14 and the seven leaks active while making CSS L4 the later authoritative target. A6 shows current `RESULTS.md` is JSON-only and no CSS throughput command exists for W1a. PLAN requires refreshed JSON guards after JSON-producing paths move and forbids CSS/non-JSON rows. | REDRESS 121 may record legality only: no CSS parser row, no lightningcss comparator result, no SOTA claim, no Sheets or BBNF-self fallback, and no SK-V12 close. |

## Required Redress Shape

For CH2, W1a remains accepted only if the implementation proves all of the
following:

- `validate_generic_crate_neutrality` scans the declared generic roots and is
  consumed by `lock14_baseline::validate` through `bbnf-bench --bin gate` /
  `xtask gate-json`.
- Generic provider selection contains no grammar literals and no grammar-policy
  branch; per-grammar literals live only in JSON-owned or future grammar-owned
  modules excluded from generic-root scans.
- Generated JSON `config.rs` or equivalent metadata is used by generated JSON
  parser/view/direct/typed code in the same redress commit.
- JSON policy remains only in generated JSON modules or explicitly JSON-owned
  profile/template/renderer roots, never in scanned generic roots.
- `skinny/crates/ir/src/**`, `BackendShape`, BIR/`BackendExpr`, directive
  handling, and public runtime/tape/substrate APIs do not expand.
- JSON parity, exact generated roster, SPEC Section 0.5 guard floors, generated
  size facts, and `RESULTS.md` exactness are recorded under REDRESS 121.

## Reject Conditions

Reject W1a under CH2 if any of these occur:

- A generic root contains `grammar_name == "json"`, `grammar_name == "css_l4"`,
  a grammar-name `match` branch with policy, or any grammar-specific literal or
  policy outside a provider-owned grammar module.
- The selector becomes a hardcoded JSON/CSS/Sheets/BBNF-self branch rather than
  data-driven provider lookup.
- A public `runtime::tape::GrammarConfig`, generic `TapeBuilder<C>`, `UnionTape`,
  sidecar substrate, generic sink super-trait, or other public substrate API is
  added.
- Any IR file, BIR/`BackendExpr` variant, `BackendShape` variant, or directive
  parser behavior changes.
- JSON structural bytes, JSON string/escape/number/object policy,
  `OffsetFlags::HAS_ESC` JSON meaning, or `JsonSink`/view/kind/callback shape
  remains in a scanned generic root.
- The Lock 14 scan is only unit-tested and not consumed by the final gate path.
- The scan changes `Outcome`, report schema, companion report semantics, or
  `RESULTS.md` instead of remaining a gate precondition.
- REDRESS 121 claims CSS admission, lightningcss evidence, SOTA movement,
  fallback authority, or SK-V12 close from W1a.

## Must-Fix Before Redress

None. The plan itself is CH2-acceptable. The implementation must preserve the
narrowing above; otherwise disposition changes to REVISE or REJECT based on the
landed evidence.
