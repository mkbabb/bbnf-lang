# SK-V12 W1b-2 CH1 V2 - Correctness

Verdict: ACCEPT.

No correctness blocker remains in PLAN-V2.

Accepted facts:

- PLAN-V2 resolves the lightningcss raw-token overclaim by naming a
  source-sidecar fact emitter gated by lightningcss parse and AST projection.
- lightningcss is no longer claimed to expose raw token facts or byte spans.
- Strict equality remains byte equality of the retained
  `css_l4_declaration_value_fact_stream` artifacts.
- The selected row and output plane are stable.
- Fixture limits are concrete and fail-closed.
