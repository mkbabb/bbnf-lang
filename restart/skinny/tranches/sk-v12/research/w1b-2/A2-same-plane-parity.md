# SK-V12 W1b-2 A2 - Same-Plane Parity

Date: 2026-05-20.
Phase: W1b-2 research.
Scope: equality contract for generated Track 1, cssparser oracle, and
lightningcss comparator.

## Finding

Do not require lightningcss to emit W1b-1 facts from the AST alone. W1b-1 facts
are raw-token shaped: source numeric spelling, source byte offsets, raw
`rgb(... / ...)` token shape, and token kind boundaries. lightningcss exposes a
typed stylesheet/declaration model and printer-derived canonical CSS.

Two admissible equality paths exist:

- Preferred: keep the W1b-1 `css_l4_declaration_value_fact_stream` byte stream
  exactly, but implement lightningcss as a hybrid extractor that AST-verifies
  a source scanner before emitting source-token facts.
- Fallback: define a new normalized stream such as
  `css_l4_declaration_value_fact_stream_lc_norm_v1`, and require all three
  producers to emit that normalized stream.

The plan should select the preferred hybrid path first because SPEC Section 7
names the existing output plane. If the hybrid path cannot pass, record a
measured REDRESS failure or route a plan revision; do not silently weaken the
plane.

## Parity Risks

| Surface | Risk | Required handling |
|---|---:|---|
| Property lowercasing | Low | Compare canonical ASCII-lowercase property ids. |
| Numeric lexemes | High | Hybrid path preserves source spelling; normalized fallback compares value class. |
| Percentages/dimensions | High | Hybrid path preserves source spelling and unit; normalized fallback compares value plus lowercase unit. |
| `rgb(... / alpha)` | High | Hybrid path preserves function/slash/paren tokens; normalized fallback compares semantic color payload. |
| `!important` | Medium | Compare boolean importance and watch declaration ordering. |
| Nested `@media` depth | Low/Medium | Traverse nested rules and require the expected depth. |
| Byte offsets | High | Hybrid path source scanner can preserve W1b-1 offsets; AST-only path cannot. |
| Declaration order | Medium | Current fixture is safe; broader fixtures need source-order reconstruction. |

## Recommended Equality Rules

For W1b-2 redress:

- Track 1, cssparser oracle, and lightningcss comparator must write retained
  fact artifacts.
- `strict_output_equality = pass` means byte equality across all three retained
  fact artifacts.
- The lightningcss artifact must include an AST cross-check status so the
  source scanner is not a disguised raw-token-only comparator.
- If the comparator uses normalized facts instead, the report must name the new
  normalization version and CHALLENGE must accept the output-plane revision
  before redress.
