# SK-V11 W6 R5: Grammar-Neutral Escape Policy

Pass: W6 Phase 1 research.
Date: 2026-05-20.
Scope: grammar-neutrality and non-JSON policy for escaped strings / hex colors
under W6.
Output: this file only.

## Verdict

W6 cannot honestly satisfy the SK-V11 non-JSON close axis from the current
codebase state. It must carry REDRESS 113 forward unless its later plan
explicitly creates, benchmarks, and gate-consumes a generated non-JSON
direct/typed parser plus independent same-plane oracle. That is not present
today.

W6 may still dispatch as a JSON direct-plane escaped-segment / Unicode row
attempt under SPEC Section 10. It may not claim Lock 14 closure from prose,
from `sheets_witness`, from the old non-skinny CSS/Sheets/BBNF runtimes, from
the current JSON `unescape_string` caller, or from `HEX_QUARTET_X4_PROOF` alone.

## Authority Read

SPEC Section 10 authorizes C3 escape segment / hex decode work only after W5
has a disposition and the W6 plan names a new source delta beyond the already
consuming `unescape_string` path. The exit gate allows selected JSON Unicode
direct rows and says a non-JSON escaped-string or hex-color consumer may admit
only if JSON rows record honest measurements and the non-JSON consumer is real.
The same SPEC also keeps the global close axis: at least one non-JSON grammar
must carry an admitted, benchmarked SK-V11 intervention through a generated
direct or typed parser.

W1a created only the companion non-JSON gate/report schema lane. Its accepted
shape rejects missing fields, producer-only telemetry, Track 2 coupling, and
admission claims, but it explicitly did not create generated non-JSON baseline
authority.

W1b rejected the selected CSS L4 direct baseline. The blocker is still live:
`skinny/crates/codegen/src/lib.rs:102-167` calls
`json_provider::ensure_runtime_profile` from both normal and typed emission,
and `skinny/crates/codegen/src/json_provider.rs:4-13` accepts only
`backend.grammar_name == "json"`. The provider then emits JSON-named runtime
files and JSON types (`json_provider.rs:15-74`). Runtime exports only generated
JSON as `grammars::json`, with `sheets_witness` proof-gated for tests/features
(`skinny/crates/runtime/src/lib.rs:1-16`).

W2 is therefore blocked by REDRESS 113: no W1b baseline Mbps exists, no
generated CSS L4 Track 1 exists, and W2 may not create the first measurable
non-JSON row. W6 does not erase that history. A W6 non-JSON claim must either
supersede the W1b/W2 gap with explicit owner authority and a measured generated
row, or carry REDRESS 113 as unresolved.

## C3 Policy Boundary

C3 is grammar-neutral only at the hex digit / hex-run / escaped-segment layer.
The syntax and semantic policy belong to generated per-grammar code or a named
host function.

JSON policy:

- JSON simple escapes and `\uXXXX` surrogate handling are currently hardcoded in
  `parse-that-regex` (`validate_string_escape`, `decode_unicode_escape`, and
  `validate_unicode_escape_run` in `skinny/crates/parse-that-regex/src/lib.rs`).
- `unescape_string` already consumes the aarch64 x4 Unicode escape route through
  `unescape_four_unicode_escapes` when it sees a JSON `\u` escape. Reusing that
  existing production path is pre-blocked as a new admit.
- JSON surrogate policy must stay in generated JSON caller/runtime policy, not
  in a generic crate API advertised as grammar-neutral.

CSS policy:

- CSS strings are not JSON strings. `grammar/css/l4/tokens.bbnf:7-9` allows
  single and double quoted strings with backslash escapes as regex spans.
- CSS hex colors are the best C3 non-JSON proof surface:
  `grammar/css/l4/color.bbnf:187-190` defines `hex = "#" ,
  /[0-9a-fA-F]{3,8}/ -> crate::css_types::parse_hex_color(input) : u32`.
- A future W6-like CSS proof should route a neutral hex-run decoder through the
  generated CSS host-function policy for `#RGB`, `#RGBA`, `#RRGGBB`, and
  `#RRGGBBAA`. It must not import JSON `\uXXXX`, slash escape, or surrogate
  semantics.

Sheets policy:

- The current Sheets grammar is not a C3 hex/unicode escape proof surface.
  `grammar/google-sheets/google-sheets.bbnf:8-12` uses doubled quotes inside
  strings and explicitly defers decode work.
- Sheets can prove C2 string scanning or a future doubled-quote decode policy,
  but not JSON-style C3 without a new generated Sheets string host policy and
  independent oracle.

BBNF-self policy:

- BBNF-self literals and regexes expose escaped spans
  (`grammar/bbnf/bbnf.bbnf:11-15`), so they can prove an escaped-segment
  template if generated parser and oracle support exist.
- BBNF-self does not prove CSS hex color policy or JSON surrogate policy. A
  valid proof would be a generated direct/typed grammar-fact row plus a bounded
  independent scanner that does not call generated Track 1 or `grammar::parse`.

## What A Future Non-JSON Proof Needs

To turn W6 into a non-JSON close-axis admit, a later plan must name all of these
before redress:

1. Generated Track 1 authority: replace or bypass `json_provider` with a
   grammar-neutral generated-runtime path, then generate exactly one selected
   non-JSON parser under `skinny/crates/runtime/src/grammars/` from named input.
   Preferred target remains CSS L4 declaration values / hex color.
2. Independent same-plane oracle: for CSS hex colors, use either a bounded
   independent fact scanner or a `lightningcss`-backed fact stream that does not
   call generated Track 1, generated SinkOnly helpers, generated typed helpers,
   old hand runtimes as authority, or benchmark-private parser code.
3. C3 source delta: add a neutral scalar hex-run / escaped-segment oracle and,
   if SIMD is used, strict checkasm parity over valid, invalid, mixed-case,
   alignment, and length cases. x4 JSON proof stays proof-only unless this new
   caller consumes it in the same wave.
4. Per-grammar policy: CSS length/expansion rules, Sheets doubled-quote rules,
   and BBNF literal/regex rules stay generated or host-policy facts. No generic
   helper may encode JSON object/string/surrogate assumptions.
5. Measurement and gate consumption: render before/after generated Track 1
   Mbps, independent oracle/Track 2 Mbps, strict output equality, run id, host,
   flags, sample count, feature mask, same-wave consumer class, and source
   provenance through the W1a companion report lane or a same-wave gate consumer.
6. JSON honesty: if no JSON W6 row closes, the JSON Unicode rows still need
   honest Track 1/Track 2 measurements and REDRESS entries before any non-JSON
   W6 admit can be used as the close-axis proof.

Without those pieces, a W6 non-JSON claim is unmeasurable under P3-C/P3-D and
replays REDRESS 113.

## W6 Plan Guidance

For the immediate W6 plan, carry REDRESS 113 forward and select one of two
honest routes:

- JSON route: target `unicode_escapes`, `unicode_mixed`, and/or
  `y_string_unicode` direct rows with a new escaped-segment source delta beyond
  existing `unescape_string`, scalar oracle, optional x4 checkasm, Track 1 and
  independent Track 2 measurements, and guard floors.
- Non-JSON route: return REVISE/BLOCKED unless the plan owns a generated
  non-JSON baseline plus independent oracle and names a CSS hex-color or
  BBNF-self escaped-segment consumer with before/after throughput.

Sheets should not be selected for W6 C3 unless the wave explicitly changes the
selected primitive from hex/unicode escape to doubled-quote string decode and
updates the gate accordingly. Under the current SPEC Section 10 C3 surface,
Sheets is fallback evidence for future string policy, not a W6 close-axis
candidate.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0, 2.2, 5, 6, and 10.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-implementation.md`.
- `restart/skinny/tranches/sk-v11/research/w1a/w1a-R4-nonjson-row-shape.md`.
- `restart/skinny/tranches/sk-v11/research/w1b/w1b-R2-fallback-grammars.md`.
- `restart/skinny/tranches/sk-v11/research/w1b/w1b-R3-codegen-boundary.md`.
- `restart/skinny/tranches/sk-v11/research/w1b/redress/w1b-redress-rejection.md`.
- `restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`.
- `skinny/REDRESS.md` REDRESS 111, 112, and 113.
- `skinny/crates/codegen/src/lib.rs:102-167`.
- `skinny/crates/codegen/src/json_provider.rs:4-74`.
- `skinny/crates/runtime/src/lib.rs:1-16`.
- `skinny/crates/runtime/src/grammars/`.
- `skinny/crates/parse-that-regex/src/lib.rs:283-381`,
  `skinny/crates/parse-that-regex/src/lib.rs:384-440`,
  `skinny/crates/parse-that-regex/src/lib.rs:718-790`, and
  `skinny/crates/parse-that-regex/src/lib.rs:919-970`.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33-214`.
- `grammar/css/l4/tokens.bbnf:7-9`.
- `grammar/css/l4/color.bbnf:187-190`.
- `grammar/google-sheets/google-sheets.bbnf:8-12`.
- `grammar/bbnf/bbnf.bbnf:11-15`.

Self-verdict: research-only. No source files edited; no baseline row created.
