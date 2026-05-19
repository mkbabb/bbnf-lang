# SK-V10 P2-F: Grammar-Neutral Abstraction

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: Lock 14 verdicts for Alpha/P1 candidate primitives and product-plane abstractions.
Output: this file.
P1 hot-leaf antecedents: `string_tiny_scan`, `string_full_scan`, `string_escape`, `unicode_escape_hex`, `number_digit_scan`, `number_scan`, `whitespace_skip`, `direct_struct`, `array_walk`, `object_walk`, `simd_movemask`, and `alloc`.
Lock surface: Lock 14, with Lock 1 refusal checks where a candidate could reopen W3 or a parallel substrate.

## V1 Challenge Fold

`p2g-candidate-ledger.md` is the post-CHALLENGE S-P3 eligibility authority.
This file supplies Lock 14 verdicts; it does not by itself make a candidate
row-moving. `Comparator / telemetry refresh` is gate-only evidence schema, not
a behavior or primitive candidate because it has no S-P1 hot-leaf antecedent.
`instruments` typed admission remains a JSON product row only and cannot be
cited as CSS/Sheets/BBNF-self proof.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. Lock 14 is the governing test. Generic crates must carry zero grammar-specific modules, public types, feature flags, or `match grammar` arms; per-grammar deviations belong in grammar source, metadata, or generated per-grammar runtime output only (`restart/locks/LOCKS.md:78`). Alpha-E repeats this as a hard SK-V10 gate: generic-crate, codegen, or runtime-outside-JSON edits require named CSS L4 / Sheets / BBNF-self proof, and JSON-only wins do not prove the generator thesis (`restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:19-21`).

2. The live S-P1 primitive classes are already mostly phrased in grammar-neutral terms: `string_tiny_scan`, `string_full_scan`, `string_escape`, `unicode_escape_hex`, `number_digit_scan`, `number_scan`, `whitespace_skip`, `array_walk`, `object_walk`, `direct_struct`, `simd_movemask`, `alloc`, and `memcpy` (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:29-32`). P2-F's job is therefore not to invent new names; it is to reject any JSON-only binding of those names.

3. The primary JSON frontier is direct output, not parse-only. The current state is 17 direct rows with 3 `A / GO` digest guards and 14 `N-direct / NO-GO`; parse-only is diagnostic and cannot close SK-V10 (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:50-54`, `restart/skinny/tranches/sk-v10/SYNTHESIS.md:80-84`). Direct candidates therefore need an output/control contract before any row movement, not a parser-substrate rewrite (`restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:42-46`, `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:83-86`).

4. W3 remains retired. REDRESS 98 retires `G-W3-UNION-SUBSTRATE`, and SK-V10 pre-blocks W3 under renamed framing, W4-through-W3, and substrate/kernel waves without micro-proof (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:69-72`, `restart/skinny/tranches/sk-v10/SYNTHESIS.md:120-133`; `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:317-326`). Any abstraction that retains a second sidecar, structural projection, or event stream is rejected regardless of local speed.

5. Lock 16 already provides grammar-neutral SIMD vocabulary for byte classification, cross-chunk byte context, branchless accumulation, digit-block decode, parallel-channel classification, bit compaction, and SWAR classify (`restart/locks/LOCKS.md:87-112`). P2-F can admit a candidate only when it maps to one of these shapes or to a per-grammar codegen template consuming one of these shapes.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

| Candidate | Shape | Scalar-ref status | Arch surface | P1 antecedent |
|---|---|---|---|---|
| Tiny string scan | Byte-span literal/terminator matcher over a bounded maximum, parameterized by delimiter byte set and optional literal equality set. | Existing scalar `match_tiny_plain_string_with_cap::<8>` call-site is the oracle; prior 16-byte NEON wiring is REDRESS-blocked until micro-proved at the current caller. | Portable scalar first; possible NEON TBL/equality-set body only behind checkasm and same-wave caller. | `string_tiny_scan` on `twitter`, `github_events`, `instruments`, `update_center`, `apache_builds`, `distinct_values`, `unicode_basic`; P1-E lines 40, 53, 57-58, 63, 67-68. |
| Full string / escape codec | Two-part primitive: string-span special-byte scan (`quote`, escape lead, control class) plus escape-codec unit parameterized by digit count/range, terminator policy, and surrogate policy. | Existing scalar string scanner, `read_hex_unit_scalar`, `unescape_uxxxx_scalar`, and current sink/view behavior are the oracle (`restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:219-225`). | Portable scalar; aarch64 string block and unicode codec only with checkasm; x86 routes are future per Lock 16. | `string_full_scan`, `string_escape`, `unicode_escape_hex`; `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:41-42`, `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:65-66`. |
| Number scan | Numeric-token span detection and digit-block accumulation, with materializer policy split from span recognition. | Existing `match_number_span_from_first` / `scan_digit_run` and vendored exact number materializer are the oracle. Raw `parse::<f64>()` shortcut remains rejected by parity history. | Portable scalar; NEON saturating arithmetic / UDOT and x86 VNNI/IFMA are admissible only as scalar-oracle differentials. | `number_digit_scan` / `number_scan`; P1-E lines 43, 55, 59, 64. |
| Whitespace skip | Byte-set run skipper parameterized by grammar layout policy and whitespace alphabet. | Existing `skip_ascii_whitespace` behavior is the oracle for JSON ASCII WS; non-JSON policies need generated grammar metadata. | Portable SWAR scalar first; SIMD byte-class run skip only after grammar-specific alphabet binding. | `whitespace_skip`; P1-E lines 44, 54, 60, 62-63. |
| Direct output/control-path contract | Output-plane equivalence and row-admission contract: independent oracle status, comparator strictness, validation path, same-run run id, and row disposition. Not a byte kernel. | Scalar reference is the independent Track 2/oracle plus serde_json/sonic typed or direct shape parity, depending on row plane. | No SIMD/ASM. Gate/report/codegen contract only. | `direct_struct`, current 14 direct `N-direct / NO-GO` rows; Alpha-E lines 23-86; P1-F direct table. |
| Root typed generalization | Layout/root model abstraction: generated typed roots can be named struct, array root, or map-entry root without JSON policy in generic code. | serde_json and sonic typed deserialization plus generated Track 1 and independent checksum oracle. | No SIMD/ASM. Codegen/schema abstraction. | Root blockers for `github_events` and `gsoc-2018`; Alpha-E lines 150-199; P1-E lines 57, 61. |
| `instruments` typed admission | Per-grammar generated typed product row for a fixed top-level object. | serde_json + sonic typed + Track 2 checksum oracle. | No SIMD/ASM. Per-grammar generated product proof. | `instruments` string/whitespace hot leaves; Alpha-E lines 88-148; P1-E line 63. |
| Comparator / telemetry refresh | Gate-only evidence schema and same-run freshness manifest. | Existing `gate-json` and report schema are the oracle; no behavior output. | No SIMD/ASM. | P1-F result extraction and Alpha-E lines 277-315. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Lock 14 verdict | CSS L4 | Sheets | BBNF-self | Disposition |
|---|---|---|---|---|---|
| Tiny string scan | CSS L4 / Sheets / BBNF-self generalizable as a bounded byte-span matcher. | Identifiers, short keywords, units, function names, and punctuation-delimited spans can bind the delimiter/literal set from grammar metadata. | Cell references, function names, booleans, errors, and quoted-string short spans can bind the same matcher with Sheets delimiter policy. | Rule names, directive names, tokens, and short literals can bind it. | Admit only as grammar-neutral byte-set/literal-set primitive. Reject JSON field-name-only wiring or reuse of the old 16-byte active dispatch without call-site micro-proof. |
| Full string scan | CSS L4 / Sheets / BBNF-self generalizable as a string-like span scanner, but consumers are per-grammar templates. | CSS strings and URL-ish token spans need quote/escape/control parameters, not JSON quote ownership. | Double-quoted strings with doubled-quote escape policy need a different escape consumer. | Quoted terminals and comments need grammar-specific delimiter policy. | Admit primitive as byte classifier + span scanner. Per-grammar template only for ownership of escape semantics and decoded output. |
| Unicode / escape codec | Per-grammar template only over a grammar-neutral hex/classifier kernel. | CSS L4 has 1-6 hex digits and whitespace/non-hex terminator, no JSON surrogate-pair policy. | Sheets mostly string escaping by doubled quotes; unicode codec applies only if grammar source declares one. | BBNF-self quoted terminals can declare escapes, but policy belongs to grammar source. | Admit the hex-unit classifier/decoder as grammar-neutral. Reject hardcoded `\uXXXX` + surrogate-pair behavior in generic crates as JSON-overfit. |
| Number scan | CSS L4 / Sheets / BBNF-self generalizable as numeric span + digit-block decode. | CSS `<number>`, dimensions, percentages, exponents, signs, and decimals fit with grammar-bound suffix/terminator policy. | Formula numbers fit, but locale/date coercion must stay out of generic numeric scan. | Numeric literals in grammar metadata or semantic actions fit if declared. | Admit as grammar-neutral numeric lexer/materializer split. Reject JSON-number grammar hardcoding in generic public APIs. |
| Whitespace skip | Per-grammar template only over grammar-neutral byte-set run skipping. | CSS whitespace is entangled with comments and token boundaries. | Sheets formula whitespace is context-sensitive around operators/ranges. | BBNF layout/directive whitespace is grammar policy. | Admit byte-set run skipper. Reject a generic "JSON whitespace" helper outside a JSON-generated module. |
| Direct output/control-path contract | Per-grammar template only, not a generic byte primitive. | CSS output equivalence is AST/value/pretty contract against lightning-css-like semantics, not JSON digest. | Sheets output equivalence is formula/value model and canonical document shape. | BBNF-self output equivalence is grammar AST / regenerated source parity. | Admit as generated per-grammar contract template with common telemetry fields. Reject JSON digest semantics as generic gate policy. |
| Root typed generalization | CSS L4 / Sheets / BBNF-self generalizable as layout-root algebra. | CSS roots can be stylesheet, rule list, declaration list, or value root. | Sheets roots can be workbook/sheet/range/formula arrays. | BBNF roots can be grammar, rule list, expression, or token declaration. | Admit as grammar-neutral `RootShape`/layout abstraction. Reject any `JsonRootSchema` branch in generic code. |
| `instruments` typed admission | Per-grammar template only. | No direct CSS analogue to the fixture; the reusable part is the typed row gate. | No direct Sheets analogue to the fixture; the reusable part is checksum/oracle discipline. | No direct BBNF-self analogue. | Admit only as a JSON product-plane row. It cannot be cited as totality proof. |
| Comparator / telemetry refresh | CSS L4 / Sheets / BBNF-self generalizable as evidence schema. | Can record lightning-css comparator id, strictness, freshness, output plane. | Can record formula evaluator/comparator identity and output plane. | Can record bootstrap parser/regenerator comparator identity. | Admit as common telemetry if all emitted fields are consumed by the same gate. Reject sidecar freshness as a producer. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- W3/union substrate: rejected for SK-V10 under REDRESS 98 and pre-blocked in Synthesis. No candidate may retain a structural sidecar, split the tape, or cite W3 as a caller (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:69-72`, `restart/skinny/tranches/sk-v10/SYNTHESIS.md:120-133`; `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:319-320`).
- Tiny-string active NEON dispatch: REDRESS 28 and 33 recorded parity-green but wrong-boundary / regressing active wiring. Tiny string may proceed only as current-call-site micro-proof, scalar oracle first.
- UTF-8/string fusion and retained projection routes: REDRESS 50-55 reject byte-class whitespace cursor, parser-local structural cursor, decoded-string stats sink, and quote-source fused materializer. Full string and escape candidates must stay on existing consumers named by Alpha-E (`match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`, `decode_unicode_escape`, `unescape_string`; `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:229-254`).
- Direct scratch/semantic fact receiver classes: P1-E records the allocator leaf but says eager decoded scratch is not authorized; REDRESS 66-69 remain pre-blocks (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:84-88`).
- Direct contract dishonesty: direct digest rows remain guard-plane evidence until output-plane equivalence, Track 2 status, and comparator anchors are measurable (`restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:42-46`, `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:83-86`). No direct digest row may be relabeled as typed product proof (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:120-130`).
- Canada typed shortcut: root typed generalization and typed row admission cannot move Canada or any typed row by analogy; full-fixture generated/serde/sonic checksum parity is required (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:120-125`; `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:317-326`).
- PMULL/CTZ production rewires: Alpha-E rejects PMULL/CTZ as default production hot paths (`restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:326`). A movemask/CTZ observation on `gsoc-2018` is only a measured hot leaf, not a kernel authorization (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:89-92`).
- Gate-only sidecars: telemetry refresh and sidecar freshness cannot produce parser data, output rows, substrate, or strict admission by themselves (`restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md:290-315`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v10/research/alpha-hardening/V1/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/g-alpha/G-ALPHA-PRESENTATION.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/locks/LOCKS.md`
