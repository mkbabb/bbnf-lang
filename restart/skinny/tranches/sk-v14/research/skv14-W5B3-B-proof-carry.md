# SK-V14 W5B.3 Research B: Proof Carry

Date: 2026-05-26.
Scope: W5B.3 downstream compatibility.
Output: proof carry list.

## Findings

W5B.3 can be grammar-only if it keeps raw constructs and unsupported behavior
stable. The W5A runtime contract exercises all W5B.3 constructs through the CSS
request path. Sheets/BBNF-self fail-closed behavior also matters because it
currently depends on projection diagnostics.

## Recommendations

- Run W5B.3 exact tests plus W5B.1 import, W5B.2 layout, W5A runtime, JSON, and
  Sheets/BBNF-self carry checks.
- Do not edit codegen behavior, xtask, provider/template, generated runtime, or
  rolling-delta files in W5B.3.

## Risks

Removing projection unsupported diagnostics before W5B.4 would break the
Sheets/BBNF-self fail-closed witness. W5B.3 should add facts while leaving that
consumer-facing diagnostic route intact.
