# SK-V12 W1b-2 CH1 - Correctness

Verdict: REVISE.

The same-plane target is correct, but the V1 plan overclaims what
lightningcss can prove.

Blockers:

- lightningcss does not preserve public source declaration sequence for all
  cases. `DeclarationBlock` stores normal and important declarations in
  separate vectors and iterates normal before important, so the plan cannot
  rely on a simple AST sequence match.
- lightningcss does not expose raw token equality for known properties. Known
  CSS properties are parsed into typed/canonicalized values; token count and
  lexeme equality would come from the source scanner, not lightningcss.
- byte-span verification is partial. `StyleRule::property_location` is line /
  UTF-16-column based and nested declarations do not expose equivalent public
  byte ranges.
- SPEC requires Track 1, Track 2/oracle, and lightningcss evidence for the
  same canonical fact stream. The V1 plan describes a source-scanner fact
  stream gated by lightningcss parse/AST checks, which is admissible only if it
  is named as fixture-limited validation, not lightningcss raw fact emission.

Required revision:

- Reword W1b-2 as a frozen-fixture source-scanner fact stream gated by
  lightningcss parse success plus best-effort AST declaration/property/
  importance checks.
- Do not claim lightningcss verifies token count, byte offsets, or
  byte-identical raw facts.
