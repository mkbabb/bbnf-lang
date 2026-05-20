# SK-V11 W5 Phase 1 Research - R5 Grammar-Neutral / Non-JSON Proof

Status: read-only research artifact.
Scope owner: W5 R5 grammar-neutral and non-JSON proof implications for bounded
string span, especially Sheets versus CSS.
Owned path:
`restart/skinny/tranches/sk-v11/research/w5/w5-R5-grammar-neutral.md`.
Source edits: none.
Date: 2026-05-20.

## Verdict

W5 can claim that the C2 / `pt_bounded_plain_string_end` shape is
grammar-neutral in abstraction, but W5 cannot claim a non-JSON generated-parser
intervention in SK-V11 as the ledger stands. W5 must carry REDRESS 113's W2
non-JSON axis block forward unless a later Alpha/Pass-Omega contract first
creates a generated non-JSON baseline with explicit owner authority.

The distinction is load-bearing:

- Grammar-neutral shape means quote byte, escape or doubled-quote policy,
  control cutoff, cap, and decode-needed status are grammar metadata.
- Non-JSON proof means a generated non-JSON Track 1, independent same-plane
  Track 2/oracle, strict equality, throughput against a baseline, gate
  consumption, and no generic JSON policy leak.

The first is available from P2-F/P2-E. The second is blocked by W1b/W2.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 9 and Section 2.2.
- `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`.
- `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md`.
- W1a/W1b/W2 redress and boundary records:
  `restart/skinny/tranches/sk-v11/research/w1a/w1a-R6-redress-boundaries.md`,
  `restart/skinny/tranches/sk-v11/research/w1a/challenge/CH3-redress-regression.md`,
  `restart/skinny/tranches/sk-v11/research/w1b/w1b-R6-redress-boundaries.md`,
  `restart/skinny/tranches/sk-v11/research/w1b/redress/w1b-redress-rejection.md`,
  `restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md`,
  and `skinny/REDRESS.md` items 111-113.
- Grammar surfaces:
  `grammar/google-sheets/google-sheets.bbnf`,
  `grammar/css/l4/tokens.bbnf`,
  `grammar/css/l4/values.bbnf`,
  `grammar/css/l4/value-unit.bbnf`,
  `grammar/css/l4/color.bbnf`,
  and `grammar/css/l4/stylesheet.bbnf`.

## Binding Facts

SPEC Section 9 names W5 as `G-W5-STRING-SPAN-DIRECT`, with candidates C2
bounded special-byte scan, P2-D D3 `borrowed_string_span`, and P2-E
`pt_bounded_plain_string_end` (`SPEC.md:540-544`). W5 must select one
string/key consumer and at most two rows, add a scalar span oracle, wire one
generated direct/typed string/key consumer, and keep Unicode residual rows
honest (`SPEC.md:559-579`). It also says non-JSON string/literal proof must
pass when generic code changes (`SPEC.md:580-583`).

SPEC Section 2.2 makes that stricter for generic/codegen/runtime-outside-JSON
edits: grammar facts must be generated metadata, a CSS L4 / Sheets / BBNF-self
generated parser proof must run and be consumed in the same wave when generic
behavior changes, and `json_provider` must be replaced, bypassed with proof, or
explicitly left untouched before a non-JSON generality claim can pass
(`SPEC.md:230-245`).

P2-F already grants the useful abstraction shape. C2 is a bounded special-byte
scan whose terminator, escape, control policy, and cap are grammar metadata,
and whose current JSON wrapper is not the generic candidate
(`p2f-grammar-neutral.md:41-44`). P2-F also says C2 is grammar-neutral for CSS
quote/backslash strings and raw URL spans, Sheets doubled-quote strings, and
BBNF literal/regex spans, but only shortlists it with a string-heavy generated
direct/typed consumer and no decoded scratch side substrate
(`p2f-grammar-neutral.md:72-78`).

P2-E gives the parse-that candidate boundary. `pt_bounded_plain_string_end`
returns a borrowed span end offset only; it cannot allocate decoded strings,
retain a string block, add a string side table, or materialize byte output. Its
same-wave product consumer may be JSON string/key fast paths, typed string
skip, or a generated non-JSON string/literal consumer such as CSS dual-quoted
strings, Sheets doubled-quote strings, or BBNF literals (`p2e-parse-that-gaps.md:35-48`).
The generic helper's shape is parameterized by quote byte, escape byte, control
cutoff, and cap; JSON UTF-8 and surrogate policy cannot move into the generic
helper (`p2e-parse-that-gaps.md:62-68`).

REDRESS 111 admitted only the W1a non-JSON gate/report lane. It did not create
generated non-JSON baseline authority or move any parser row (`skinny/REDRESS.md:3284-3310`).
REDRESS 112 rejected W1b because skinny codegen still routes runtime emission
through `json_provider::ensure_runtime_profile`, which accepts only JSON, and
no generated CSS L4 runtime existed (`skinny/REDRESS.md:3313-3338`). REDRESS
113 records W2 as `BLOCKED`: W2 may not create the first measurable non-JSON
row, `W1b_css_baseline_mbps` is absent, and the W2 exit threshold is undefined
(`skinny/REDRESS.md:3340-3355`).

## Sheets Versus CSS For W5 C2

Sheets is the cleaner W5 string-span proof surface if a future contract permits
using the SPEC fallback order. Its string token is a single double-quoted span
with doubled quotes as the escape form (`grammar/google-sheets/google-sheets.bbnf:8-12`).
The formula grammar surrounds operators, argument lists, and arrays with `?w`
but does not introduce CSS-style comment trivia in that file
(`grammar/google-sheets/google-sheets.bbnf:103-161`). For C2, that means the
future generated template can parameterize:

- terminator: `"`;
- special policy: doubled `""` marks decode-needed and advances by generated
  Sheets string policy;
- output: span or decoded value chosen by the generated Sheets caller;
- no JSON backslash, control-byte, UTF-8, or surrogate semantics in the generic
  helper.

CSS remains the preferred non-JSON axis target for SK-V11 as a whole, but it is
a broader W5 string-span surface. CSS L4 tokens have both double- and
single-quoted strings with backslash escapes (`grammar/css/l4/tokens.bbnf:7-9`).
CSS values also include `url()` raw spans that can contain either CSS string
tokens or a raw non-quote, non-space URL body (`grammar/css/l4/values.bbnf:67-69`).
The declaration-value grammar mixes strings with functions, colors,
dimensions, numbers, keywords, identifiers, and catch-all value bytes
(`grammar/css/l4/values.bbnf:84-101`). CSS also has adjacent C3/C4/C5/C6
surfaces: hex colors route through `parse_hex_color`
(`grammar/css/l4/color.bbnf:187-190`), dimensions and percentages consume CSS
number policy (`grammar/css/l4/value-unit.bbnf:8-16`,
`grammar/css/l4/value-unit.bbnf:62-72`), and stylesheet whitespace is
comment-aware generated policy (`grammar/css/l4/stylesheet.bbnf:5-12`).

Therefore:

- Sheets is better for a narrow W5 C2 proof because it isolates a string-span
  primitive around doubled quotes and formula spans.
- CSS is better for the broader SK-V11 non-JSON intervention axis because it
  exercises C1/C2/C3/C4/C5/C6 together, but that breadth makes it easier to
  accidentally prove more or less than W5's bounded string span.

Neither surface currently satisfies the non-JSON proof gate, because neither
has the missing generated Track 1 baseline in skinny.

## W5 Implications

W5 should not state that it closes or admits the non-JSON generated-parser
axis. The strongest honest W5 wording is:

- `C2 / pt_bounded_plain_string_end is grammar-neutral in candidate shape.`
- `W5 carries REDRESS 113's non-JSON generated-intervention block.`
- `Any W5 generic parse-that, bbnf-simd, codegen, or runtime-outside-JSON edit
  must either produce and gate-consume a same-wave generated non-JSON
  string/literal proof under new authority, or return REVISE/BLOCKED before
  claiming generality.`

If W5 proceeds as a JSON direct-plane string/key attempt, it must keep the
non-JSON block explicit. It may use Sheets/CSS grammar facts as parameter
evidence for the scalar shape, but not as generated-parser proof, not as row
admission, and not as a substitute for W1b/W2.

Preblocked proof substitutions:

- `sheets_witness` is not a generated Sheets parser baseline.
- Old hand CSS/Sheets runtimes are cautionary inventory, not SK-V11 generated
  Track 1.
- A scalar helper test, SIMD checkasm, or primitive self-time result is not a
  product-plane proof without the caller and row gate.
- A CSS/Sheets regex citation is not an independent oracle unless it is wired
  as a same-plane Track 2/oracle with strict equality and gate consumption.

## Future Route

Recommended future route name:
`SHEETS-C2-SPAN-GENERATED-BASELINE-THEN-W5`.

1. A later Alpha/Pass-Omega contract creates a generated Sheets formula
   baseline row, for example `sheets/formula/string_literal/direct/main` on a
   `sheets_formula_string_fact_bytes` output plane. It must name generated
   Track 1, independent same-plane Track 2/oracle, strict equality, baseline
   Mbps, run/build/host facts, and gate consumption. It must not reuse
   `sheets_witness` or generated JSON helpers as Track 1 or Track 2.
2. A follow-on W5-shaped intervention wires `pt_bounded_plain_string_end` into
   the generated Sheets string caller with terminator `"` and generated
   doubled-quote policy. It records before/after throughput against the
   baseline, strict equality, scalar fallback, optional SIMD strict parity, and
   no JSON policy in generic crates.
3. If governance insists on CSS first, use the analogous
   `CSS-C2-DECL-STRING-SPAN` route only after a generated
   `css_l4/declaration_values` baseline is restored. Scope that route to CSS
   string and `url()` span facts inside `css_l4_declaration_value_fact_bytes`;
   leave hex color decode, CSS number policy, comment-aware layout, and
   prefix-dispatch improvements to their C3/C4/C5/C6 waves.

Until one of those routes creates the missing generated baseline, W5 must carry
REDRESS 113 as blocked for the non-JSON axis.
