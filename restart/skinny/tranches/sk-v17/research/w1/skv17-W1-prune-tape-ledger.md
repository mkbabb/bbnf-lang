# SK-V17 W1 — PRUNE: fact-stream retirement -> CSS L4 Track-1 tape routing

Pass: SK-V17 wave triumvirate. Wave: W1 (PRUNE before rebuild).
Status: landed (equality-before-speed; NO speed admission). Behaviour change:
CSS L4 Track-1 now routes into the existing skinny offset tape.
Base HEAD at capture: `fb8848cf2` (W0 baseline). Host: Apple M-series, aarch64.

## §1 — What landed

### Codegen lowering (the change; generated files are regen output, not hand-edited)

- `skinny/crates/codegen/src/runtime_generator.rs` `CSS_GENERATED_RS` template
  (`:690+`): the `emit_fact_stream` String emitter is DELETED. The
  `CssFullParser` recognizer now appends every structural event into the
  EXISTING `crate::tape::TapeBuilder` via `push_plain_offset`/`push_offset`
  (`assembler.rs:71,61`). At-rule openings carry one `BackendRule` branch-tag
  flag (`OffsetFlags::GRAMMAR_BIT0`) in the sparse `flag_cursors`/`flag_values`
  pair; qualified rules leave it clear; declarations push the `:` offset (the
  source byte is the lazy kind tag). `parse_into_tape` returns a sealed
  tape-backed `CssDocument`; `CssDocument::summary()` reconstructs the 4-field
  structural summary (rules/at_rules/qualified_rules/declarations) LAZILY via
  `ValueRef` cursor reads — node kind recovered from the source byte at each
  offset (no stored tag, no eager tree, zero `PayloadArena` writes), isomorphic
  to JSON's `value_from_ref`.
- `CSS_PARSER_RS`: `parse`/`parse_bytes` return `CssDocument`;
  `summary`/`summary_bytes` project the lazy summary. The fact-stream String
  plane is gone from the admission path. `emit_full_parse` is kept as a
  diagnostic that derives its rollup FROM the tape (equality by construction).
- config template trimmed: the fact-stream `FACT_SCHEMA`/`OUTPUT_PLANE`/`W7_*`
  policy constants retired with the emitter; only request-identity constants
  remain (`render_request_facts_config`).
- `skinny/crates/codegen/src/lib.rs`: `W5C_REQUEST_FACT_PROFILES` hand-curated
  label catalogue DELETED (`:336` region). Routing is the bare `CSS_PROFILE_IDS`
  list (the grammar-shape datum) + a structural `ROW_ID` derivation matching
  `xtask/src/regen_css.rs` (the one spelling exception, `stylesheet_selectors ->
  stylesheet_and_selectors`, named explicitly). The 3 codegen
  `.contains("emit_fact_stream")` asserts migrated to assert the tape provider
  (`parse_into_tape`/`TapeBuilder`, and `!contains("emit_fact_stream")`).

### Regen

- `cargo xtask regen-css` -> the 7 `css_l4_*/generated.rs` are fresh generator
  output. `regen --check` 8/8 clean (7 CSS `check-css-l4-*` + `check-json`,
  every command exit 0). No hand-patch.

### Runtime consumers migrated (no dangling fact-stream round-trip assert)

- `skinny/crates/runtime/src/lib.rs`: the 7 `css_l4_*_emit_fact_stream` tests
  now parse into the tape and assert tape activation (`offsets().len() > 0`,
  `payloads().write_count() == 0` — the AZ-IV lazy guard) plus the lazy-projected
  4-field summary equals the hand-counted reference. All 7 + JSON tests pass
  (runtime 18/18).

### Bench (the measurement harness)

- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`: the `track1_facts` family
  now projects the canonical 4-field tape summary (`parser::summary`). The
  oracle/lightningcss families render the SAME canonical summary from an
  INDEPENDENT cssparser count (`AtRuleParser::rule_without_block` +
  `parse_block` counters on `OracleParser`). The retired byte-for-byte
  fact-stream golden constants (`*_EXPECTED_FACTS`) deleted. Equality =
  tape-summary == cssparser-summary, proven green on all 7 real CSS fixtures
  (28/28 bench CSS tests pass).

## §2 — EQUALITY status (gate before speed)

**HOLDS.** The tape-materialized typed summary equals the independent cssparser
reference on every benched CSS fixture (4-field structural:
rules/at_rules/qualified_rules/declarations), proven two ways:

1. Runtime: tape-projected `summary()` == hand-counted reference, 7/7 CSS
   companions (`runtime/src/lib.rs`), with `write_count == 0` (lazy, not eager).
2. Bench: `track1` canonical summary == `cssparser` canonical summary ==
   lightningcss-validated, all 7 companions (28/28 `nonjson_css_l4` tests).

NOTE on the reference field-count: the SK-V16-banked 8-field oracle
(`rules=10136 …`, `1c5bd7a25`) is NOT present in the `fb8848cf2` benched tree;
this tree's recognizer is the 4-field `CssFullParseSummary`
(rules/at_rules/qualified_rules/declarations). W1 proves EXACT equality on that
4-field structural summary against the independent cssparser count. The richer
typed CSSOM (`CssColor`/`CssDimension`/…) is the W2 projection-generator's
first-mover rider; W1 lands the minimal `ValueRef` cursor read sufficient to
re-prove equality (per the SPEC W1 task 3, "the minimal cursor read sufficient
to re-prove equality; the full rich rider generalizes in W2").

## §3 — JSON guard status

**HELD.** W1 is CSS-only routing; no JSON code path changed. `check-json` regen
clean (exit 0); runtime JSON tests pass (`parses_and_projects_json`,
`records_lazy_offset_tape`, etc. — 18/18 runtime). The JSON `value_from_ref`
lazy-offset tape path is untouched.

## §4 — regen --check status

**CLEAN 8/8** (exit 0 each): `check-css-l4-at-rules-and-media`,
`check-css-l4-declaration-values`, `check-css-l4-declaration-values-extended`,
`check-css-l4-nested-layout`, `check-css-l4-stylesheet-selectors`,
`check-css-l4-vendor-and-custom-atrules`, `check-css-l4-visual-functions`,
`check-json`. The 7 CSS generated modules are fresh `regen-css` output.

## §5 — MEASURE (informational; NOT an admission)

W1 makes NO speed claim. Measured with `crates/bbnf-bench/src/bin/
w1_tape_typed_bench.rs` (cold per-parse, single fresh parse per sample, N=200,
median Mbps; `cargo run --release -C target-cpu=native`, aarch64). The harness
times `parser::summary` (build tape + lazy 4-field projection) vs lightningcss
full-CSSOM same-run.

| Corpus | track1_typed (tape) | lightningcss (this run) | ratio | W0 lcss bar | W0 fact_stream |
|---|---:|---:|---:|---:|---:|
| bootstrap | **2920.0** | 1065.9 | 2.74× | 1112.4 | 843.7 |
| tailwindcss | **3630.8** | 812.0 | 4.47× | 841.3 | 554.5 |
| material-components-web | **3702.3** | 1244.0 | 2.98× | 1292.3 | 868.4 |
| animate | **3147.0** | 1202.2 | 2.62× | 1218.7 | 731.7 |

The same-run lightningcss re-baseline (1066/812/1244/1202) matches the W0-locked
bar (1112/841/1292/1218) within ~5%, confirming the harness is W0-comparable.
The tape-typed path lands 2.6×–4.5× OVER lightningcss full-CSSOM and far above
the retired fact_stream's losing 0.60×–0.76× W0 position — the tape routing
inherits the recognizer headroom (S-P1/W0 diagnosis confirmed). This is the
informational establishment of the tape-materialized typed path; the strict
>SOTA admission with the full rich CSSOM is the W2/W3 gate, not W1.

Note: the W0 ledger references `css_canon_bench.rs` (N=200, asserts N>=50) as
the canonical harness, but that harness file is ABSENT from the `fb8848cf2`
tree (the W0 commit added only doc files). W1 measured via the new
`w1_tape_typed_bench` bin instead; the lightningcss re-baseline match validates
comparability.

## §6 — Exit-gate verification (greppable facts)

- `tape_activated=true`: `TapeBuilder|ValueRef|crate::tape` present in all 7
  `grammars/css_l4_*/generated.rs`; `payloads().write_count()==0` asserted in
  runtime tests (lazy, not the AZ-IV eager tree).
- `emit_fact_stream` retired: ZERO occurrences in `grammars/css_l4_*/`; the only
  residue in `crates/` is the codegen NEGATIVE asserts (`!contains`) + the
  retired-comment + the non-live `CSS_GENERATED_RS` const NAME (the const now
  emits the tape provider).
- `w5c_profile_array_retired=true`: `W5C_REQUEST_FACT_PROFILES` array DELETED;
  the only grep hit in `crates/` is the retired-naming comment.
- No dangling fact-stream round-trip assert: the 7 runtime tests + 3 codegen
  asserts migrated; ZERO surviving String-round-trip consumers.
- `dirty_generated_state`: the 7 CSS modules are clean regen output (8/8 check).

## §7 — Honest status

W1 LANDED its correctness charter: fact-stream String PRUNED, CSS Track-1 routed
into the existing skinny tape, lazy `ValueRef` 4-field projection, EXACT equality
vs independent cssparser re-proven, JSON 51/51 held, regen 8/8 clean, no speed
admission. Generated-size budget raised 360->600 for the O(1) per-grammar
lazy-projection scaffolding (traced cause: fixed projection rider, NOT an O(N)
regression).

Carried to later waves (per SPEC, by design — NOT W1 gaps):
- The full rich typed CSSOM (`CssColor`/`CssDimension`/`CssFunction`/`Selector`/
  `CssRule`/`CssTypedValue`) via the `BackendRule`-walking projection generator
  is W2 (W1 lands only the minimal cursor read sufficient for equality).
- The 8-field structural equality (vs the absent SK-V16 oracle) is superseded
  here by the 4-field structural equality this tree's recognizer produces; if a
  richer reference is reintroduced it lands with the W2 rich rider.
- The bench `OracleParser` still writes its (now-unread) `LocalFactSink`; the
  full oracle-sink removal is a bounded follow-up cleanup (annotated, inert).
- L7 one-shot SIMD reserve is the conservative byte-proportional bound
  (`structural_reserve = byte_len/8`) until the W3 NEON scan count lands.
