# SK-V11 W1b R2: Fallback Grammar Viability

Status: read-only research artifact.
Scope: W1b Phase 1 research, R2 fallback grammar viability.
Owned path: `restart/skinny/tranches/sk-v11/research/w1b/w1b-R2-fallback-grammars.md`.
Source edits: none.

## Question

Determine whether Sheets or BBNF-self should override CSS L4 as the W1b
selected target, and name blockers.

## Verdict

Do not override CSS L4 for W1b yet.

V2 supersession note: Phase 2 and CHALLENGE select
`css_l4/declaration_values/direct/main` on
`css_l4_declaration_value_fact_bytes`. The original typed-route preference in
this R2 research is superseded for W1b dispatch because current skinny W1b
cannot produce an admissible generated typed CSS Track 1 or add the required
dependency/manifest owners. CSS L4 is still the best W1b seed because it is the
preferred SPEC target, feeds W2's CSS intervention directly, already has an
external comparator surface, and exercises the C1/C2/C3/C4/C5/C6 surfaces that
SK-V11 needs to prove are non-JSON.

Fallback order if CSS fails CHALLENGE:

1. Sheets formulas: viable first fallback, preferably `sheets/formula/typed/main`
   over a curated corpus of formula syntax facts.
2. BBNF-self: viable last fallback for parser/gate plumbing only, preferably
   `bbnf_self/grammar/typed/main`, but it is weaker as a SK-V11 generality proof.

Neither fallback should override CSS on current evidence. Sheets has a smaller
grammar and green self-parity, but lacks a strong independent same-plane oracle
today. BBNF-self is stable and local, but risks self-host coupling and does not
cover the numeric C4 surface unless the imported expression grammar is admitted
as part of the selected workload.

## Authority Read

SPEC Section 5 makes W1b baseline-only: exactly one generated non-JSON direct or
typed parser baseline row, exactly one independent Track 2/oracle, strict output
equality, gate-consumed throughput/provenance, no behavior admission, and no JSON
row movement. The entry gate says CHALLENGE selects one target, preferring CSS L4
declaration values, then Sheets, then BBNF-self.

P3-B and P3-E repeat the topology: W1a creates the gate/report lane, W1b creates
the first generated non-JSON baseline plus oracle, and W2 consumes that baseline.
W2 may not invent the first measurable non-JSON row. P3-D fixes canonical
non-JSON grammar ids as `css_l4`, `sheets`, and `bbnf_self`; W1a research also
requires rejecting `google_sheets` unless SPEC is updated.

P2-F gives the grammar-neutrality map:

- CSS L4 covers C1 byte classes, C2 bounded string/body scans, C3 hex/escape host
  surfaces, C4 numeric dimensions, C5 comment-aware layout policy, C6
  FIRST/prefix/lookahead dispatch, and C7 support masks.
- Sheets covers C1/C2/C4/C5/C6/C7 well, but C3 is rejected for the current
  doubled-quote string grammar.
- BBNF-self covers C1/C2/C5/C6/C7, but not C4 unless the imported expression
  grammar is admitted separately.

## Candidate Comparison

| Target | W1b fit | Oracle fit | W2 fit | Main blockers |
|---|---|---|---|---|
| CSS L4 declaration values | Best. Preferred by SPEC and P3. Existing generated parser/runtime, real CSS fixtures, and external comparator surface exist. | Best. `lightningcss` can provide an independent same-run declaration fact stream if the selected corpus is curated. | Best. W2 is explicitly CSS L4 generated direct/typed intervention. | Must avoid broad stylesheet/canonical-output parity, Tailwind-scale cost, pending color aggregate gaps, permissive `genericDecl`, and old hand-runtime-as-generated proof. |
| Sheets formulas | Viable fallback. Smaller grammar, existing generated parser/runtime, green self-serialize/prettify corpus tests. | Medium. Existing oracle is self-roundtrip, not independent; W1b would need a separate formula fact parser/evaluator or bounded hand oracle that does not reuse Track 1. | Weaker. It proves non-JSON generation but does not seed W2's CSS-specific intervention. | No external comparator in tree; grammar TODOs for string decode, cell-ref aggregate, range tagged enum; C3 rejected; canonical id drift (`sheets` vs `google_sheets`) must stay fixed. |
| BBNF-self grammar | Last fallback. Existing self-host parser/runtime and green broad fixture parity. | Weak/medium. Existing AST oracle is hand-scanned and useful, but self-host coupling risk is high unless oracle stays independent and bounded. | Weakest. It does not seed CSS W2 and lacks C4 numeric proof unless `expressions.bbnf` is admitted into the workload. | Self-host proof can look circular; corpus is grammar source, not an external language corpus; weaker numeric/string diversity; BBNF runtime comments still reference "once cutover.B regen lands" despite generated parse existing. |

## CSS L4 Finding

CSS L4 should remain selected if W1b narrows the output plane to direct
declaration-value fact bytes. The source grammar has the preferred declaration
surface:

- `grammar/css/l4/properties.bbnf` dispatches property groups into typed
  declarations and ends with `declaration = ... | genericDecl`.
- `genericDecl` is intentionally permissive and catches all unlisted properties.
  It is useful for parse reach, but W1b should not use it as the strict fact
  surface.
- `grammar/css/l4/value-unit.bbnf` exposes numeric `number`, dimensions,
  percentages, units, and typed discriminants.
- `grammar/css/l4/values.bbnf` covers function/value dispatch, but broad
  `value` equality is too large for W1b unless the fact schema is curated.
- `crates/core/src/runtime/css_l4/` exposes a struct-direct `CssDocument` with
  declaration/value walking; this is the right Track 1 materialization surface.

Existing tests support CSS as the preferred W1b target but also show why the
row must be bounded:

- `cargo test -p bbnf --test lightningcss_parity --test sheets_self_parity --test bbnf_self_parity -- --nocapture`
  was stopped after the CSS `tailwind` fixture ran for over 60 seconds. Before
  stop, BBNF-self parity passed 56/56; CSS passed `normalize` and `bootstrap`
  admission against `lightningcss`; the RGB color-channel test passed only via a
  soft diagnostic because `bbnf_colors` is still empty pending typed color
  aggregate wiring.
- `cargo test -p bbnf --test sheets_self_parity -- --nocapture` passed 84/84.

Those results argue against a broad CSS corpus such as Tailwind for W1b. They do
not argue against CSS declaration values. The viable CSS row is a small curated
corpus of declarations where both Track 1 and `lightningcss` can emit stable
facts: property name, important flag, value kind, f64 bits or exact scalar
payload, RGBA/hex payload when both sides expose it, and source span only when
the output plane explicitly allows spans.

CSS blockers before redress:

1. Generated authority blocker: the selected Track 1 must be generated non-JSON
   output. Existing hand struct-direct runtime/tests cannot be used alone as
   generated-parser proof.
2. Oracle blocker: the Track 2 oracle must not call `CssL4Parser::parse`,
   generated CSS modules, `runtime::css_l4::parse_with`, generated SinkOnly
   helpers, generated typed helpers, JSON providers, or benchmark-private parser
   code.
3. Corpus blocker: Tailwind/bootstrap full-stylesheet parity is too broad.
   Select a declaration-values corpus with strict fact equality and no shorthand
   expansion, calc simplification, declaration reordering, or recovery behavior.
4. Typed-value blocker: current tests document color-function aggregate wiring as
   pending. Either exclude that value class from W1b or make it a W2
   intervention, not a baseline prerequisite.
5. Fallback declaration blocker: `genericDecl` must not silently dominate the
   selected fact stream. The W1b fact schema should identify whether a
   declaration came through a typed family versus generic fallback and should
   reject a selected typed row that is mostly generic spans.

## Sheets Finding

Sheets should not override CSS now, but it is the best fallback if CSS cannot
produce a bounded independent oracle.

The grammar is compact and well suited to a W1b baseline:

- `grammar/google-sheets/google-sheets.bbnf` has a single `formula` entry and
  clear primary-expression, operator, function-call, LET/LAMBDA, array, cell,
  and range surfaces.
- Generated/runtime support exists under
  `crates/core/src/grammar/generated/google_sheets.rs` and
  `crates/core/src/runtime/google_sheets/`.
- `crates/core/benches/google_sheets/monolithic.rs` already benchmarks simple,
  nested, and stress formulas.
- `sheets_self_parity` passed 84/84, including simple/nested/stress corpus
  checks.

Sheets blockers:

1. Oracle blocker: current strong tests are self-roundtrip/serializer/prettify
   idempotency. That is not an independent same-plane oracle. W1b would need a
   separate formula fact oracle that does not call `GoogleSheetsParser::parse`,
   generated typed helpers, or old runtime witness paths.
2. Typed-shape blocker: grammar comments explicitly defer string decode,
   `CellRef` aggregate decode, and variant-tagged range endpoint payloads.
   A W1b Sheets row must either choose a span-preserving output plane honestly or
   exclude these facts.
3. C3 blocker: P2-F rejects Sheets as an escape/hex proof surface because the
   current grammar uses doubled quotes, not JSON/CSS-style hex escapes.
4. ID blocker: gate/report identity must use `sheets`, not `google_sheets`,
   unless SPEC is updated in the same wave. P3-A's older
   `google_sheets/formula/{direct,typed}` wording must not leak into gate values.
5. W2 blocker: a Sheets W1b baseline would still leave W2's CSS intervention
   without a CSS baseline. SPEC permits fallback selection, but W2 would need
   REVISE or an explicit route to consume a Sheets baseline for a non-CSS W2.

Sheets override condition:

Override CSS only if CHALLENGE finds the CSS declaration-values row cannot meet
strict oracle independence within budget, and a Sheets oracle can be written as
a bounded formula fact stream with generated Track 1, independent Track 2,
same-plane strict equality, and gate consumption inside W1b.

## BBNF-Self Finding

BBNF-self should not override CSS or Sheets for W1b. It is the last fallback if
the goal is to prove non-JSON gate/report plumbing with the smallest local
corpus, not to maximize grammar-generalization strength.

The positive case:

- `grammar/bbnf/` is small: `bbnf.bbnf`, `expressions.bbnf`, and `types.bbnf`.
- Generated/runtime support exists under `crates/core/src/grammar/generated/bbnf.rs`
  and `crates/core/src/runtime/bbnf/`.
- `bbnf_self_parity` passed 56/56 in the focused run, including broad grammar
  fixture AST roundtrip and prettify idempotency.
- Existing `bbnf_ast_parity.rs` contains a hand-written source scanner oracle
  for rule names and directive kinds, which is closer to an independent oracle
  than a pure self-roundtrip.

BBNF-self blockers:

1. Coupling blocker: self-hosting can look circular. The oracle must be a
   separate bounded scanner/fact extractor, not `grammar::parse`,
   `BbnfBootstrap::parse`, generated modules, or `runtime::bbnf::parse_with`.
2. Coverage blocker: P2-F rejects BBNF-self as a C4 numeric proof surface unless
   `expressions.bbnf` is admitted separately. Even then, the main grammar only
   imports value expressions after `->`; it is not as representative as CSS or
   Sheets numeric values.
3. W2 blocker: BBNF-self does not seed CSS W2. Selecting it would force W2 to
   either REVISE from "CSS L4 Generated Direct/Typed Intervention Proof" or
   create a CSS baseline later, which SPEC explicitly blocks.
4. Output-plane blocker: a useful BBNF-self row is likely a typed grammar-fact
   stream (`rule`, `directive`, `term` facts). That is viable, but narrower than
   CSS declaration values or Sheets formulas.
5. Runtime wording blocker: `crates/core/src/runtime/bbnf/mod.rs` still says
   "Once cutover.B regen lands" while generated BBNF parse support is present.
   That comment is not a source blocker, but it is a documentation drift signal
   W1b should not use as proof.

BBNF-self override condition:

Use BBNF-self only if both CSS and Sheets fail CHALLENGE, and only with a
separate fact oracle over grammar source that proves rule/directive/term output
strictly without calling generated Track 1.

## Recommended W1b Selection

Recommended selected target:

```text
grammar_id: css_l4
domain: css_l4_bench
corpus: declaration_values
workload: direct
row_id: css_l4/declaration_values/direct/main
output_plane: css_l4_declaration_value_fact_bytes
oracle: css_l4_decl_value_fact_oracle
```

Recommended fallback targets:

```text
grammar_id: sheets
domain: sheets_bench
corpus: formula
workload: typed
row_id: sheets/formula/typed/main
output_plane: typed direct or SPEC-named formula fact plane
oracle: independent formula fact oracle
```

```text
grammar_id: bbnf_self
domain: bbnf_self_bench
corpus: grammar
workload: typed
row_id: bbnf_self/grammar/typed/main
output_plane: typed direct or SPEC-named grammar fact plane
oracle: independent grammar fact scanner
```

## CHALLENGE Questions

1. Can the CSS row use a curated declaration-values corpus and exclude Tailwind
   scale, broad stylesheet canonicalization, and unresolved typed color aggregate
   classes?
2. Does Track 1 come from generated non-JSON output rather than the old hand
   runtime or JSON provider path?
3. Does the `lightningcss` oracle produce the same output plane without calling
   Track 1, generated helpers, or a shared parser-shaped projection?
4. Does the fact stream expose typed-vs-generic declaration provenance so
   `genericDecl` cannot accidentally pass a typed row?
5. If CSS fails, can Sheets provide an independent formula fact oracle within
   budget, and does the W2 plan explicitly handle the lack of a CSS baseline?
6. If both fail, is BBNF-self's bounded scanner oracle sufficient to prove
   generated non-JSON gate/report mechanics without claiming broad C1-C7
   generality?

## Close Criteria For R2

R2 recommends CSS L4, with Sheets as the first fallback and BBNF-self as the
last fallback. Return REVISE before redress if W1b implementation plans any of
the following:

- selecting Sheets or BBNF-self while CSS has not failed an explicit CHALLENGE
  blocker;
- selecting more than one target;
- using `google_sheets` as the gate grammar id instead of `sheets`;
- using old hand non-JSON runtime proof as generated Track 1;
- using parser self-roundtrip as an independent oracle;
- letting W2 create the first CSS baseline after a non-CSS W1b selection without
  a SPEC/P3 revision.
