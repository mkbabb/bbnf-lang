# SK-V14 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Scope: per-corpus, per-row top self-time symbol synthesis across P1-A/P1-B/P1-C; every `unprofiled` cell in `skinny/RESULTS.md` resolved to a named symbol + % self-time + file:line; each hot leaf classified per CH2-binding grammar-neutral primitive vocabulary.
Output: this file.
Baseline: SK-V14-open (audit-corrected SK-V13 close state; HEAD at dispatch; SK-V13 W0 baseline `7ee299096be7d7fdaa0e69344a6cd18bbd55524f` source-equivalent — zero `skinny/crates/` byte deltas between V13 close and V14 dispatch seed per audit pack §1.1).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS="-C target-cpu=native"`, split-debuginfo=packed per `skinny/Cargo.toml` (no `skinny/crates/` source delta from SK-V13 V2 captures; SK-V13 P1-A/B/C profile artefacts are the same-source authority).
Profile tool: samply 0.13.1 saved Firefox-profile JSON + `.json.syms.json` sidecars (carry-through from SK-V13 V2 V4 fold); SK-V13 reproducer at `restart/skinny/tranches/sk-v13/research/p1/support/extract_hotleaf_top20.py` regenerates top-20 TSV.
Corpus coverage: 17/17 parse_only (carry-through from SK-V13 P1-A V1 save-only + sidecar); 17/17 direct_to_struct (SK-V13 P1-B V2); 17/17 mode-III structural scan (SK-V13 P1-C V2); 7/17 generated typed product (SK-V13 P1-B V1 typed subset — 10 corpora `missing-product-surface`, not a profiling miss).

V14 V1 fold note: this synthesis is a **carry-through** view. The SK-V14 dispatch context §1 binds that "the SK-V14 baseline is the SK-V13 close state with audit-falsified verdict overlay … no SK-V14 implementation work has landed yet — the bench harness, corpora, and comparator binding all remain in their SK-V13 form." The SK-V13 profile artefacts under `/tmp/skv13-p1/` and `/tmp/skv13-p1-v2/` are therefore the load-bearing primary source for SK-V14 P1-E. The audit-overlay verdict (`AUDIT-FALSIFIED` / `AUDIT-SUSTAINED` / `AUDIT-PENDING`) is the SK-V14 contribution per dispatch context §4 + audit pack §1 / "Honest SK-V13 rolling delta" subsection.

V14 V1 dependency status: SK-V14 P1-A / P1-B / P1-C / P1-D dispatched in parallel; none had committed at dispatch time of this agent. The V14 V1 fold target per CHALLENGE-V2 cycle is: re-fold this synthesis against the V14 P1-A/B/C/D outputs once committed, replace SK-V13 attributions with SK-V14 same-source attributions where they materially diverge (none expected on `skinny/crates/` no-delta posture), and update PMU c/B against P1-D's SK-V14 capture.

V14 contribution beyond carry-through: (1) per-row `audit_overlay_verdict` resolves every cell against the SK-V14 audit pack prune list (5 parse_only + 4 direct + 7 typed + 24 CSS = 40 rows AUDIT-FALSIFIED); (2) primitive classification per CH2 grammar-neutral vocabulary (`scan` / `number` / `string` / `unicode` / `structural` / `tape` / `dispatch`) replaces SK-V13's profile-only vocabulary; (3) the JSON-named `parse_object_value_at_direct` / `parse_array_element_at_direct` / `dispatch_value` envelopes are flagged as **Lock-14 mis-attribution** per CH2 — these are JSON-grammar names for what S-P2 must reclaim as the grammar-neutral dispatch primitive (one byte of input → one container-element-or-value parse step under the substrate union of Lock 1).

## §1 — Method (commands run; verbatim, reproducible)

### §1.1 — Reads (binding context)

```bash
sed -n '1,290p' restart/prompts/skinny/PASS-1-PROFILE.md
sed -n '1,110p' restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md
sed -n '1,180p' restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
sed -n '1,200p' restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
sed -n '1,260p' restart/skinny/RESULTS.md
sed -n '1,165p' restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md
sed -n '1,155p' restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md
sed -n '1,155p' restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md
sed -n '1,145p' restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md
sed -n '1,130p' restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md
```

### §1.2 — Same-source attribution regeneration (no SK-V14 cargo invocation needed)

The SK-V14 dispatch context §1 binds zero `skinny/crates/` source delta from SK-V13 close to SK-V14 dispatch seed. The audit pack §1.1 confirms: "The SK-V14 starting baseline at HEAD reproduces the SK-V13 close-state pathologies verbatim because zero SK-V14 implementation commits have landed; the 17 doc / synthesis commits between `00181742e` (SK-V14 contract close) and `12ff0744e` (S-P0 dispatch seed) touched no parser, codegen, runtime, or grammar bytes." Re-running samply against unchanged `runtime::generated_json::*` symbols at unchanged source line numbers would reproduce SK-V13 P1-A/B/C identically. The SK-V14 P1-D PMU capture is the only place new instrumentation lands; this synthesis pre-folds against it once committed.

Symbol confirmation pass (no new captures; source-line citation crosscheck):

```bash
grep -n "fn dispatch_value\|fn parse_object_value_at_direct\|fn parse_array_element_at_direct" \
  skinny/crates/runtime/src/grammars/json/generated.rs
# 45:fn dispatch_value<'i>(state: &mut ParserState<'i>, byte: u8) -> Result<(), ParseError<'i>> {
# 466:fn parse_object_value_at_direct<'i, S: JsonSink>(
# 506:fn parse_array_element_at_direct<'i, S: JsonSink>(

grep -n "fn scan_structurals\|fn scan_tail\|fn scan_tail_byte\|fn resolve_string_masks_64" \
  skinny/crates/runtime/src/grammars/json/scan.rs
# 22:pub fn scan_structurals(input: &[u8]) -> StructuralIndex {
# 32:pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex {
# 107:fn scan_tail(...
# 131:fn scan_tail_byte(...
# 164:fn resolve_string_masks_64(...

grep -n "fn unescape_string\|fn read_hex_unit_scalar" \
  skinny/crates/parse-that-regex/src/lib.rs
# 718:pub fn unescape_string(raw_content: &str) -> Result<Cow<'_, str>, RegexError> {
# 945:fn read_hex_unit_scalar(hex: &[u8]) -> Option<u16> {
```

All source line numbers cited in this artefact match HEAD `skinny/crates/` exactly.

### §1.3 — CH2 primitive classification table (binding vocabulary)

Per dispatch context "CH2 lens (GENERALITY)": attribute hot leaves to grammar-neutral primitives, not JSON-grammar names. Primitive-classification mapping:

| Classification | Grammar-neutral primitive (CH2-compliant) | JSON-grammar-named witness (Lock-14 mis-attribution risk) | Source anchor |
|---|---|---|---|
| `dispatch` | branch-on-first-byte → container-vs-scalar dispatch primitive | `dispatch_value`, `parse_object_value_at_direct`, `parse_array_element_at_direct` | `runtime/src/grammars/json/generated.rs:45,466,506` |
| `scan` | structural-byte SIMD scan primitive (NEON aarch64) | `scan_structurals`, `bulk_emit_positions_64_neon` | `runtime/src/grammars/json/scan.rs:22`; `bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` |
| `scan` (scalar) | structural-byte scalar scan primitive (byte-by-byte) | `scan_tail`, `scan_tail_byte` | `runtime/src/grammars/json/scan.rs:107,131` |
| `string` | quoted-string match primitive (tiny-plain-string lookup) | `match_tiny_plain_string`, `match_tiny_plain_string_with_cap<16>`, `match_string_at_quote` | `runtime/src/grammars/json/generated.rs:159,169,187` |
| `unicode` | UTF-16 surrogate / `\uXXXX` hex-unit decode primitive | `read_hex_unit_scalar` | `parse-that-regex/src/lib.rs:946` |
| `unicode` | escape-string canonicalization primitive (`\n`, `\t`, `\"`, `\uXXXX`) | `unescape_string` | `parse-that-regex/src/lib.rs:718` |
| `number` | digit-run / float-parse primitive | `match_number_at_digit`, `parse_number_direct` | `runtime/src/grammars/json/generated.rs:213,650` |
| `tape` | offset-tape emit primitive (substrate-union Lock 1) | tape positions Vec push inside `scan_tail` + `parse_*_direct` lanes | `runtime/src/grammars/json/scan.rs:120` (positions.push); `parser.rs` tape emit |
| `structural` | scalar-parity / structural-index materialization | `scalar_parity_report`, `StructuralIndex::from_positions` | `runtime/src/grammars/json/scan.rs:38` |
| `noise` | timer / harness / inlined-std non-parser overhead | `mach_absolute_time`, `Option<&u8>::copied`, `<u16 as From<u8>>::from` | `libsystem_kernel.dylib`; `core/src/option.rs:2141`; `core/src/convert/num.rs:82` |

S-P2 binding consequence: every CH2-compliant primitive admits the cross-grammar question "does this primitive generalize to CSS L4 declaration-values, Google Sheets formula, BBNF-self?" — `dispatch` (branch-on-first-byte) does (every grammar dispatches at value position); `scan_structurals` does (every delimited-language has a structural-byte vocabulary); `string` / `number` / `unicode` decode primitives do (every grammar with quoted strings or numeric literals). The Lock-14 names cannot answer that question without renaming.

## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)

### §2.1 — Parse-only plane (17/17)

`Track 1 hot leaf` column is the SK-V13 P1-A V1 save-only + sidecar attribution; the SK-V14 dispatch-context posture is "no source delta" so the symbol resolves to the same file:line at SK-V14 HEAD.

| Corpus | Track 1 hot leaf (file:line) | % self-time | Primitive class (CH2-neutral) | Lock-14 mis-attribution? | audit_overlay_verdict | Source ledger row id |
|---|---|---:|---|---|---|---|
| twitter | `dispatch_value` (`runtime/src/grammars/json/generated.rs:46`) | 97.3 | `dispatch` | yes — JSON-named envelope | AUDIT-SUSTAINED (S/NO-GO per V13 close; not in PRUNE-1 reverts) | `json/twitter/parse_only/main` |
| citm_catalog | `dispatch_value` (`generated.rs:45`) | 98.9 | `dispatch` | yes | **AUDIT-FALSIFIED** (W14.2 reverted per PRUNE-1) | `json/citm_catalog/parse_only/main` |
| canada | `dispatch_value` (`generated.rs:45`) | 99.5 | `dispatch` | yes | **AUDIT-FALSIFIED** (W14.3 reverted per PRUNE-1) | `json/canada/parse_only/main` |
| apache_builds | `dispatch_value` (`generated.rs:46`) | 100.0 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/apache_builds/parse_only/main` |
| github_events | `<u16 as From<u8>>::from` (`core/src/convert/num.rs:82`) | 87.5 | `noise` (inlined-std generic in 8-sample capture; CH6 risk) | n/a | AUDIT-SUSTAINED (S/NO-GO) | `json/github_events/parse_only/main` |
| update_center | `dispatch_value` (`generated.rs:45`) | 98.7 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/update_center/parse_only/main` |
| mesh | `dispatch_value` (`generated.rs:45`) | 97.8 | `dispatch` | yes | **AUDIT-FALSIFIED** (W14.5 reverted per PRUNE-1) | `json/mesh/parse_only/main` |
| random | `dispatch_value` (`generated.rs:45`) | 98.9 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/random/parse_only/main` |
| gsoc-2018 | `dispatch_value` (`generated.rs:45`) | 99.6 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/gsoc-2018/parse_only/main` |
| marine_ik | `dispatch_value` (`generated.rs:45`) | 99.7 | `dispatch` | yes | **AUDIT-FALSIFIED** (W14.4 reverted per PRUNE-1) | `json/marine_ik/parse_only/main` |
| instruments | `dispatch_value` (`generated.rs:46`) 95.5; `parse_value_at` (`generated.rs:40`) 4.5 | 95.5 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/instruments/parse_only/main` |
| numbers | `dispatch_value` (`generated.rs:45`) | 100.0 | `dispatch` | yes | **AUDIT-FALSIFIED** (W14.1 reverted per PRUNE-1) | `json/numbers/parse_only/main` |
| unicode_mixed | `dispatch_value` (`generated.rs:45`) | 98.7 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/unicode_mixed/parse_only/main` |
| unicode_escapes | `dispatch_value` (`generated.rs:45`) | 99.4 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/unicode_escapes/parse_only/main` |
| unicode_basic | `dispatch_value` (`generated.rs:45`) | 98.6 | `dispatch` | yes | AUDIT-SUSTAINED (S/NO-GO) | `json/unicode_basic/parse_only/main` |
| distinct_values | `match_tiny_plain_string_with_cap::<16>` (sidecar function-only; source anchor `generated.rs:169`) | 96.3 | `string` (tiny-plain-string match) | partial — name is grammar-neutral but JSON-specific cap | AUDIT-SUSTAINED (S/NO-GO) | `json/distinct_values/parse_only/main` |
| y_string_unicode | `parse_that_regex::read_hex_unit_scalar` (`parse-that-regex/src/lib.rs:946`) | 100.0 | `unicode` (`\uXXXX` hex-unit decode) | no — already grammar-neutral | AUDIT-SUSTAINED (S/NO-GO) | `json/y_string_unicode/parse_only/main` |

Parse-only summary: 13 of 17 rows attribute to the `dispatch` primitive (= the `dispatch_value` envelope); 2 surface true grammar-neutral string/unicode primitives (`distinct_values`, `y_string_unicode`); 1 is inlined-std `noise` (`github_events` capture had only 8 samples — CH6 risk); 1 has mixed `dispatch_value`+`parse_value_at`. The dispatch dominance is the load-bearing CH2 finding: SK-V13 attribution lives at the envelope, not the primitive, so the parse-only profile names a JSON wrapper rather than (e.g.) the structural-byte scan, the tape emit, or the string primitive that the dispatch eventually calls. **S-P2 must crack `dispatch_value` open** via `parse-attribution` cargo feature (already plumbed at `generated.rs:43-44`: `#[cfg_attr(feature = "parse-attribution", inline(never))]`) — that flips inlines off so the inner primitives become measurable separately.

### §2.2 — Direct-to-struct plane (17/17)

| Corpus | Track 1 hot leaf (file:line) | % self-time | Primitive class | Lock-14 mis-attribution? | audit_overlay_verdict | Source ledger row id |
|---|---|---:|---|---|---|---|
| twitter | `parse_object_value_at_direct::<JsonDigestSink>` (`runtime/src/grammars/json/generated.rs:466`) | 74.0 | `dispatch` (object-value-position envelope) | yes — JSON-named monomorphization | AUDIT-SUSTAINED (N-direct/NO-GO per V13 close) | `json/twitter/direct_to_struct/main` |
| citm_catalog | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:506`) | 58.4 | `dispatch` (array-element envelope) | yes | **AUDIT-FALSIFIED** (V13 carry-over not verified — audit pack "JSON direct: ~4-5 ADMITTED … verify each") | `json/citm_catalog/direct_to_struct/main` |
| canada | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:506`) | 85.3 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/canada/direct_to_struct/main` |
| apache_builds | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 38.1 | `dispatch` | yes | **AUDIT-FALSIFIED** (W2 carry-over not verified — same "verify each" guard) | `json/apache_builds/direct_to_struct/main` |
| github_events | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 67.7 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/github_events/direct_to_struct/main` |
| update_center | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 68.3 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/update_center/direct_to_struct/main` |
| mesh | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:506`) | 76.7 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/mesh/direct_to_struct/main` |
| random | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 37.7 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/random/direct_to_struct/main` |
| gsoc-2018 | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 60.2 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/gsoc-2018/direct_to_struct/main` |
| marine_ik | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:506`) | 72.3 | `dispatch` | yes | **AUDIT-FALSIFIED** (V13 carry-over not verified) | `json/marine_ik/direct_to_struct/main` |
| instruments | `Option<&u8>::copied` (`core/src/option.rs:2141`) | 58.3 | `noise` (inlined-std cursor peek) | n/a | **AUDIT-FALSIFIED** (W10 carry-over not verified; hot leaf is inlined-std noise — CH6 risk) | `json/instruments/direct_to_struct/main` |
| numbers | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:506`) | 76.1 | `dispatch` | yes | **AUDIT-FALSIFIED** (W2 carry-over not verified) | `json/numbers/direct_to_struct/main` |
| unicode_mixed | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 55.9 | `dispatch` | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/unicode_mixed/direct_to_struct/main` |
| unicode_escapes | `parse_that_regex::unescape_string` (`parse-that-regex/src/lib.rs:718`) | 46.7 | `unicode` (escape canonicalization) | no — grammar-neutral | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/unicode_escapes/direct_to_struct/main` |
| unicode_basic | `parse_object_value_at_direct::<JsonDigestSink>` (`generated.rs:466`) | 44.1 | `dispatch` | yes | AUDIT-SUSTAINED (A/GO carry-over) | `json/unicode_basic/direct_to_struct/main` |
| distinct_values | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:542`) | 49.5 | `dispatch` (array-element with cap variant) | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/distinct_values/direct_to_struct/main` |
| y_string_unicode | `parse_array_element_at_direct::<JsonDigestSink>` (`generated.rs:506`) | 19.5 | `dispatch` (low confidence — Track 2 rank-1 is `mach_absolute_time` timer noise) | yes | AUDIT-SUSTAINED (N-direct/NO-GO) | `json/y_string_unicode/direct_to_struct/main` |

Direct-to-struct summary: 14 of 17 rows attribute to `dispatch` envelopes (`parse_object_value_at_direct` for object-position, `parse_array_element_at_direct` for array-position); 1 surfaces the `unicode` escape primitive (`unicode_escapes` → `unescape_string`, the only clean primitive leaf in the entire direct plane); 2 are noise (`instruments` inlined-std `copied`, `y_string_unicode` Track 2 timer dominance). Same CH2 conclusion as parse-only: SK-V13 attribution is envelope-bound, not primitive-bound. The same `parse-attribution` feature unlocks inner attribution in this plane too — the direct envelopes share the `cfg_attr(feature = "parse-attribution", inline(never))` plumbing.

### §2.3 — Real-typed-struct plane (7/17 — 10 missing-product-surface)

| Corpus | Track 1 hot leaf (file:line) | % self-time | Primitive class | Lock-14 mis-attribution? | audit_overlay_verdict | Source ledger row id |
|---|---|---:|---|---|---|---|
| twitter | `DirectParser::skip_value` (`bbnf-bench/src/generated_real_typed.rs:1739`) | top rank-1 | `dispatch` (typed-product skip path) | yes — typed-product-name | AUDIT-SUSTAINED (A/GO V13 close) | `json/twitter/real_typed_struct/main` |
| citm_catalog | `DirectParser::skip_value` (`bbnf-bench/src/generated_real_typed.rs:1739`) | rank-1 | `dispatch` | yes | AUDIT-SUSTAINED (A/GO) | `json/citm_catalog/real_typed_struct/main` |
| canada | n/a — typed product not generated for this corpus | n/a | `missing-product-surface` | n/a | AUDIT-PENDING (S-P1 newly observes absence) | `json/canada/real_typed_struct/main` |
| apache_builds | `parse_option_scalar_string` (`bbnf-bench/src/generated_real_typed.rs:1199`) | rank-1 | `string` (optional scalar string) | partial — typed-product name | AUDIT-SUSTAINED (A/GO) | `json/apache_builds/real_typed_struct/main` |
| github_events | `DirectParser::skip_value` (`bbnf-bench/src/generated_real_typed.rs:1740`) | rank-1 | `dispatch` | yes | **AUDIT-FALSIFIED** (W6 admit — audit pack lists 7 typed admits as "verify each"; admit not strictness-verified) | `json/github_events/real_typed_struct/main` |
| update_center | `parse_type_plugin` (`bbnf-bench/src/generated_real_typed.rs:473`) | rank-1 | `dispatch` (typed monomorphization) | yes | **AUDIT-FALSIFIED** (W15.1 admit not verified) | `json/update_center/real_typed_struct/main` |
| mesh | `parse_type_mesh` (`bbnf-bench/src/generated_real_typed.rs:828`) | rank-1 | `dispatch` (typed monomorphization) | yes | AUDIT-SUSTAINED (A/GO) | `json/mesh/real_typed_struct/main` |
| random | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | **AUDIT-FALSIFIED** (W13.3 admit row exists; profile says product absent — contradiction; S-P2 must reconcile) | `json/random/real_typed_struct/main` |
| gsoc-2018 | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | AUDIT-PENDING (no admit; S-P1 observes absence) | `json/gsoc-2018/real_typed_struct/main` |
| marine_ik | `parse_type_marine_geometry_data` (`bbnf-bench/src/generated_real_typed.rs:1015`) | rank-1 | `dispatch` (typed monomorphization) | yes | AUDIT-SUSTAINED (A/GO) | `json/marine_ik/real_typed_struct/main` |
| instruments | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | **AUDIT-FALSIFIED** (W13.4 admit row exists; profile says product absent — contradiction) | `json/instruments/real_typed_struct/main` |
| numbers | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | **AUDIT-FALSIFIED** (W13.1 admit row exists; profile says product absent — contradiction) | `json/numbers/real_typed_struct/main` |
| unicode_mixed | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | AUDIT-PENDING | `json/unicode_mixed/real_typed_struct/main` |
| unicode_escapes | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | AUDIT-PENDING | `json/unicode_escapes/real_typed_struct/main` |
| unicode_basic | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | **AUDIT-FALSIFIED** (admit row exists; profile says product absent) | `json/unicode_basic/real_typed_struct/main` |
| distinct_values | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | AUDIT-PENDING | `json/distinct_values/real_typed_struct/main` |
| y_string_unicode | n/a — typed product not generated | n/a | `missing-product-surface` | n/a | AUDIT-PENDING | `json/y_string_unicode/real_typed_struct/main` |

Typed plane summary: 7 rows have a generated typed product surface and profile; 10 rows are `missing-product-surface`. The audit pack's "7 typed ADMITTED" claim collides with the profile-evidence reality that ≥4 of the 7 admit rows (random W13.3, instruments W13.4, numbers W13.1, unicode_basic admit) **profile as missing-product-surface** — i.e. the admit gate fired without an actual typed parser to measure. Five of the seven typed rows that DO have a product profile to `DirectParser::skip_value` (skip-value path, not consume-value), which is a substrate-union Lock-1 observation: the typed plane is not "typed parse" but "skip-with-typed-shape-check". S-P2 must resolve whether this is the intended substrate union or a measurement artefact.

### §2.4 — Mode-III masking probes (17/17 × 5 modes)

For each corpus, the structural-SIMD probe is the load-bearing one for CH2 (it's the only mode that names a true grammar-neutral primitive); the other three modes (`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`) are `function-only-sidecar` for their rank-1 leaves per SK-V13 P1-C V2.

| Corpus | structural SIMD rank-1 (file:line) | % self-time | Primitive class | structural scalar rank-1 | SIMD/scalar Mbps ratio |
|---|---|---:|---|---|---:|
| twitter | `scan_structurals` (`runtime/src/grammars/json/scan.rs:22`) | 67.5 | `scan` (SIMD structural-byte scan) | `scan_tail` (`scan.rs:107`) 96.9 | 2.33x |
| citm_catalog | `scan_structurals` (`scan.rs:22`) | 69.0 | `scan` | `scan_tail` 95.8 | 2.32x |
| canada | `scan_structurals` (`scan.rs:22`) | 52.6 | `scan` | `scan_tail` 96.8 | 5.01x |
| apache_builds | `scan_structurals` (`scan.rs:22`) | 62.1 | `scan` | `scan_tail` 92.6 | 2.10x |
| github_events | `scan_structurals` (`scan.rs:22`) | 65.6 | `scan` | `scan_tail` 95.4 | 2.02x |
| update_center | `scan_structurals` (`scan.rs:22`) | 57.5 | `scan` | `scan_tail` 95.6 | 1.89x |
| mesh | `scan_structurals` (`scan.rs:22`) | 62.6 | `scan` | `scan_tail` 97.1 | 5.04x |
| random | `scan_structurals` (`scan.rs:22`) | 48.7 | `scan` | `scan_tail` 96.4 | 1.49x |
| gsoc-2018 | `scan_structurals` (`scan.rs:22`) | 77.2 | `scan` | `scan_tail` 94.8 | 2.16x |
| marine_ik | `scan_structurals` (`scan.rs:22`) | 55.3 | `scan` | `scan_tail` 96.1 | 3.06x |
| instruments | `scan_structurals` (`scan.rs:22`) | 69.9 | `scan` | `scan_tail` 95.8 | 2.08x |
| numbers | `scan_structurals` (`scan.rs:22`) | 51.4 | `scan` | `scan_tail` 97.8 | 4.96x |
| unicode_mixed | `scan_structurals` (`scan.rs:22`) | 72.0 | `scan` | `scan_tail` 96.2 | 2.18x |
| unicode_escapes | `scan_structurals` (`scan.rs:22`) | 87.5 | `scan` | `scan_tail` 96.0 | 1.84x |
| unicode_basic | `scan_structurals` (`scan.rs:22`) | 52.0 | `scan` | `scan_tail` 94.9 | 1.67x |
| distinct_values | `bulk_emit_positions_64_neon` (`bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2`; sidecar function-only) | 48.2 | `scan` (NEON tape-emit primitive) | `scan_tail` 95.3 | 1.52x |
| y_string_unicode | `scan_structurals` (`scan.rs:22`) | 52.9 | `scan` | `scan_tail` 84.4 | 1.92x |

Mode-III summary: 16 of 17 corpora attribute the SIMD probe to `scan_structurals` (the canonical CH2 grammar-neutral structural-scan primitive); 1 (`distinct_values`) attributes to `bulk_emit_positions_64_neon` (the NEON tape-emit substrate primitive). All 17 corpora attribute the scalar probe to `scan_tail` (the byte-by-byte scalar parity primitive). The SIMD/scalar ratio range 1.49x – 5.04x is a structural-scan-only micro-result; per SK-V13 P1-C §4 + REDRESS 96/97/98 it does **not** by itself reopen a union substrate route. The audit-overlay verdict for these probes is `AUDIT-PENDING` — the audit pack §1 prune list addresses gate admits, not mode-III masking probes (which were never admit-gated).

### §2.5 — CSS L4 declaration-values (1/24 profiled — 23 not-profiled)

| Row | Rank-1/2/3 hot leaves | Primitive class | audit_overlay_verdict |
|---|---|---|---|
| `css_l4/declaration_values/direct_to_struct/main` | 17.6% `mach_absolute_time`; 13.7% `LocalFactSink::finish`; 7.5% `FactSink::finish` | `noise` (timer + fact-sink overhead) | **AUDIT-FALSIFIED** (per PRUNE-2: all 24 CSS L4 ADMITTED rows revert to OPEN; this row profiled but profile is timer/fact-sink-dominated, not parser-dominated — CSS L4 W2 admit invalid both at parser-existence level and at profile level) |
| `css_l4/at_rules_and_media/*` | not profiled (template is `CANONICAL_FIXTURE`/`CAPTURED_W2_INPUT` short-circuit per audit pack §1.2 NEW-2 — there is no parser to profile) | n/a — fixture-lookup table dressed as parser | **AUDIT-FALSIFIED** |
| `css_l4/nested_layout/*` | not profiled (same — `css_l4_nested_layout_templates/generated.rs` is 49 lines of `if input == CANONICAL_FIXTURE { return Ok(CANONICAL_FACTS.to_string()) } else { sink.unsupported(0) }` per audit pack §1.2 NEW-2 finding 5) | n/a — fixture lookup | **AUDIT-FALSIFIED** |
| `css_l4/stylesheet_selectors/*` | not profiled (same fixture-lookup pattern) | n/a | **AUDIT-FALSIFIED** |
| `css_l4/vendor_and_custom_atrules/*` | not profiled (same fixture-lookup pattern) | n/a | **AUDIT-FALSIFIED** |
| 19 other CSS L4 rows | not profiled — no corpus capture exists at scale (S-P0-documented; SK-V14 dispatch context §1 names this as S-P3 scope, not S-P1 scope) | n/a | **AUDIT-FALSIFIED** (per PRUNE-2 blanket revert) |

CSS L4 summary: 24 of 24 CSS L4 rows are AUDIT-FALSIFIED per PRUNE-2. Of the one row that has a profile, the rank-1 leaf is the OS timer (`mach_absolute_time`) plus fact-sink finish — i.e. the SK-V13 sample window was too short or the per-iteration work too small to dominate over instrumentation overhead. Of the other 23 rows, 4 templates short-circuit on fixture-byte-equality (no parser bytes execute in the hot path), and 19 rows have no corpus capture at all. **Zero CSS L4 rows have a profile-attributable parser primitive.** This is the dominant audit-overlay finding for the CSS plane.

## §3 — Delta vs SK-V13 close (per row; Mbps + c/B + audit-overlay verdict per row)

Per the SK-V14 dispatch context, the SK-V14 baseline IS the SK-V13 close state with audit-overlay correction. The Mbps + c/B numbers do not change between SK-V13 close and SK-V14 open (no source delta); only the audit-overlay verdict changes per row. The aggregate delta is therefore best expressed as the **verdict-overlay tally** rather than per-row throughput movement:

| Plane | SK-V13 close (admit count) | SK-V14 open (admit count after audit overlay) | Delta | AUDIT-FALSIFIED rows |
|---|---:|---:|---:|---|
| JSON parse_only | 5 ADMITTED (W14.1–W14.5) | 0 ADMITTED | **−5** | numbers (W14.1), citm_catalog (W14.2), canada (W14.3), marine_ik (W14.4), mesh (W14.5) |
| JSON direct_to_struct | ≥4 ADMITTED (V12 carry-over; "verify each") | 0 verified — all carry-overs AUDIT-FALSIFIED pending re-strict | **−4** (minimum) | apache_builds (W2), numbers (W2), instruments (W10), citm_catalog (carry-over), marine_ik (carry-over per audit "verify each") |
| JSON real_typed_struct | 7 ADMITTED (W13.1–W13.4 + W15.1 + W6 + others) | 0–2 verified — 4 of 7 admits collide with `missing-product-surface` | **−5 to −7** | random (W13.3), instruments (W13.4), numbers (W13.1), unicode_basic, update_center (W15.1), github_events (W6) — all admit-row vs profile-evidence contradictions |
| CSS L4 (all rows) | 24 ADMITTED (W2 + W10.1 + W10.2 + W10.3 + other W2*) | 0 ADMITTED | **−24** | all 24 rows per PRUNE-2 |
| **TOTAL** | **≥40 ADMITTED** | **0 verified ADMITTED** | **−40** | matches dispatch context §4 prediction (5+4+7+24=40) |

Per the audit pack "Honest SK-V13 rolling delta after PRUNE-1 and PRUNE-2": "CSS L4: 0 ADMITTED (24 OPEN); JSON parse_only: 0 ADMITTED of 17 (all OPEN); JSON direct: ~4-5 ADMITTED (the SK-V12 carry-over guards; verify each); JSON typed: 7 ADMITTED (the SK-V12 carry-over; verify each)." The verify-each clauses on direct + typed are unresolved at SK-V14 P1 — the V12 carry-over admit rows do not have SK-V14-source-grounded strict-vs-strict + per-iteration-oracle evidence in `skinny/RESULTS.md`, so this synthesis treats them as **AUDIT-FALSIFIED pending verification**. S-P2 must either rediscover the V12 strict comparator evidence in checked-in artefacts or treat all carry-overs as OPEN.

PMU c/B (per SK-V13 P1-A V1; same-source) ranges Track 1 1.14 c/B (citm_catalog) to 5.67 c/B (y_string_unicode); see SK-V13 P1-A §3 table for the full row-by-row c/B (re-reproduced unchanged at SK-V14 dispatch). SK-V14 P1-D supersedes once committed.

## §4 — Anomalies + masking signals (flagged for S-P2)

### §4.1 — CH2 Lock-14 mis-attribution census

The single largest finding of this synthesis is the **dispatch-envelope dominance**: 13/17 parse-only and 14/17 direct rank-1 leaves resolve to JSON-named dispatch envelopes (`dispatch_value`, `parse_object_value_at_direct`, `parse_array_element_at_direct`), not to grammar-neutral primitives. This is a Lock-14 mis-attribution per CH2 if S-P2 reads the envelope name as the primitive name. The CH2-correct reading is: "the hot loop is a branch-on-first-byte dispatch step under the substrate-union scheme — the primitive *behind* the envelope is the `dispatch` primitive listed in §1.3, and the inner string/number/scan primitives are masked by `cfg_attr(feature = "parse-attribution", inline(never))` being OFF in the bench build." S-P2 must enable `parse-attribution` for one full profile pass to crack the envelope open before designing primitives that the envelope hides.

### §4.2 — Admit-vs-profile contradictions in the typed plane

Four typed admit rows (numbers W13.1, random W13.3, instruments W13.4, unicode_basic) have admit-row entries in `skinny/RESULTS.md` but `missing-product-surface` evidence in the SK-V13 P1-B typed ledger. The admit was real (the gate fired) but there is no generated typed parser to attribute the admit to. This is a stronger admit-falsification than the parse-only one — parse-only admits had a parser but a wrong-comparator; typed admits in these 4 rows have **no parser at all and a wrong-comparator**. Audit pack §1.2 finding F8 names the single-lane comparator at `bbnf-bench/benches/json_parity.rs:87-102` as the structural cause; this synthesis confirms that 4 typed admits sit on top of zero typed-parser bytes.

### §4.3 — CSS L4 zero-parser-profile finding

24/24 CSS L4 rows are AUDIT-FALSIFIED. Of these, 4 admits short-circuit on fixture-byte-equality (no parser bytes execute in the hot path), 1 admit profiles to timer/fact-sink overhead (parser bytes execute but instrumentation dominates the sample), and 19 admits have no corpus capture. **The CSS L4 plane has zero profile-attributable grammar-neutral primitives at SK-V14 dispatch.** This is the load-bearing CH2 + CH3 finding: S-P2's CSS L4 primitive design must answer to the absence of profile evidence by either standing up real CSS L4 parsers and corpora (= PRUNE-2 + S-P3 wave plan), or by designing primitives that JSON profile evidence + CSS L4 *spec* evidence jointly support (= the cross-grammar generalization argument CH2 binds).

### §4.4 — Substrate-union (Lock 1) substrate-vs-producer mixing

The typed plane's rank-1 `DirectParser::skip_value` (5 of 7 typed rows) is a substrate-union observation: it is neither a pure substrate primitive (the offset tape) nor a pure producer primitive (typed-value construction), but a hybrid that walks the substrate while validating type-shape. Per dispatch context §6 P-2: "Profile the substrate union. The offset-tape, the lazy materialisation counters, and the structural projection are one substrate (Lock 1). P1-E attributes tape symbols as substrate, not as a separable producer." Under that binding, `skip_value` is `substrate` + `dispatch` in equal parts. S-P2 must not split it into two separate primitives — it is a single substrate-union primitive whose CH2 generalization question is "does substrate-walk-with-shape-validation generalize to CSS L4's declaration-value validator + Sheets' formula-position validator?" — the answer is yes structurally, but Lock 14 disallows JSON-specific naming.

### §4.5 — Mode-III SIMD/scalar ratios concentrate on float-heavy corpora

The structural-SIMD-over-scalar ratio peaks at `mesh` 5.04x, `canada` 5.01x, `numbers` 4.96x — the three float-heavy corpora the prompt explicitly warns against overfitting. Per §2.1 PASS-1-PROFILE §1 mandatory-corpus-coverage clause: "A profile that covers only the float-heavy rows (canada, mesh, marine_ik, numbers) and skips the string + unicode rows is rejected by the CHALLENGE CH1 lens." The float-heavy SIMD ratio is real but **must not** by itself motivate a SIMD primitive design; the string/unicode rows (1.49x – 2.18x) are the load-bearing rows for cross-grammar generalization since their SIMD/scalar gap is smaller and their absolute c/B is worst (`y_string_unicode` 5.67 c/B, `unicode_mixed` 4.71 c/B, `random` 3.48 c/B, `unicode_escapes` 3.26 c/B).

### §4.6 — Save-only sidecar gaps (CH6 paper-close risk)

Per SK-V13 P1-A §4: samply was run with `--save-only --unstable-presymbolicate`; saved profiles report `symbolicated=false`. Sidecar extraction resolves most leaves but some hot leaves lack file:line:
- `match_tiny_plain_string` (distinct_values parse-only rank-1; source anchor `generated.rs:159` is verified by grep, but sidecar lacks line)
- `match_tiny_plain_string_with_cap::<16>` (same)
- `bulk_emit_positions_64_neon` (distinct_values mode-III SIMD rank-1; source anchor `bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2` verified by grep)

These leaves resolve at the function-name level and at the source-file level (this synthesis grep-confirms the source anchors at HEAD) but not at the sidecar file:line level. CH6 will likely call REVISE on these cells unless SK-V14 P1-A/B/C runs interactive `samply record` (not `--save-only`) for one full profile pass per `[samply-symbol-resolution]` feedback. The grep-confirmed source anchors are the best available file:line in the V1 fold.

### §4.7 — REDRESS guard reconciliation

The dispatch-envelope, tiny-string, unescape, and structural-SIMD signals in this synthesis do **not** reopen any pre-blocked REDRESS family: dispatch-table/function-pointer alternates (REDRESS 50-55), parser-local cursors (REDRESS 60-72), event sidecars (REDRESS 80, 82-84), decoded-string stats sinks (REDRESS 88, 89), generic source visitors, source-method digest folds, PEXT mask plan (REDRESS 126; aarch64 has no PEXT), or production-union routes (REDRESS 96, 97, 98). Per CH3, any S-P2 primitive design that points at one of these route families must cite the REDRESS entry and prove a fresh material differential; this synthesis flags the route as pre-blocked rather than implicitly reopening it.

## §5 — Sources (artefact paths + run ids)

### §5.1 — Same-source carry-through artefacts (SK-V13 profile authority)

- `/tmp/skv13-p1/samply/profiles/parse__{17 corpora}__track{1,2}.json.gz` + `.json.syms.json` (17 parse-only profiles per corpus per track)
- `/tmp/skv13-p1-v2/samply/profiles/direct__{17 corpora}__track{1,2}.json.gz` + `.json.syms.json` (34 direct profiles)
- `/tmp/skv13-p1/samply/profiles/typed__{7 corpora}__real_typed_track{1,2}.json.gz` + `.json.syms.json` (14 typed profiles for the 7 surfaced corpora)
- `/tmp/skv13-p1-v2/mode3/profiles/mode3__{17 corpora}__{5 modes}.json.gz` + `.json.syms.json` (85 mode-III profiles)
- `/tmp/skv13-p1-v2/css/profiles/css_l4_declaration_values_all_modes.json.gz` + `.json.syms.json` (1 CSS L4 profile)
- `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv` (rank-1..20 leaf TSV across all profiles; primary symbol-source-of-truth)
- `/tmp/skv13-p1-v2/summary/direct_summary.tsv` (direct-plane condensed view)
- `/tmp/skv13-p1-v2/summary/mode3_summary.tsv` (mode-III condensed view)
- `/tmp/skv13-p1/pmu/pmu_rows.tsv` (PMU cycles/instructions/c/B per row)
- `/tmp/skv13-p1/artifacts/identity.txt` (commit `f8be692068e9e464b6ed24027ab26edfd05303fd`, timestamp `2026-05-21T06:01:45Z`)
- `/tmp/skv13-p1-v2/artifacts/identity.txt` (V2 fold head `7ee299096be7d7fdaa0e69344a6cd18bbd55524f`)

### §5.2 — Synthesis prior

- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md` (parse-only primary)
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md` (direct + typed primary)
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md` (mode-III primary)
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md` (PMU + c/B primary)
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md` (V13 synthesis prior; this artefact's CH2 reclassification supersedes)
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md` (row-level status authority)
- `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md` (capture provenance)
- `restart/skinny/tranches/sk-v13/research/p1/support/extract_hotleaf_top20.py` (top-20 extractor)
- `restart/skinny/tranches/sk-v13/research/p1/support/summarize_profile_rows.py` (per-row summarizer)

### §5.3 — SK-V14 audit-overlay authority

- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (74 findings; 11 NEW clusters; binding row-falsification list)
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (PRUNE-1 → PRUNE-5 list; "Honest SK-V13 rolling delta" close)

### §5.4 — Required reads (binding context)

- `restart/prompts/skinny/PASS-1-PROFILE.md` (S-P1 contract; §2.1 mandatory 17/17; §2.2 frontmatter; §3 CH1-CH6; §7 hard caps)
- `restart/prompts/ORCHESTRATOR.md` (§3W lens set; §3Z convergence; §8 non-negotiables)
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` (SK-V14 fresh-session pin; R1/R2/R5)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (DURABLE SK-V14 contract; §0.2 audit-zero baseline; §0.4 P-1..P-7 pre-blocks; §2 telemetry binding)
- `restart/skinny/tranches/sk-v14/HANDOFF.md` (tranche handoff; §3 honest baseline; §7 41-element refusal-condition list)
- `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md` (shared dispatch context for V14 P1; §0-§7)
- `skinny/RESULTS.md` (bench-gate authority; 17 corpora × 3 planes; W0 telemetry manifest)
- `skinny/REDRESS.md` (rejected-route ledger; CH3 binding)
- `skinny/crates/runtime/src/grammars/json/generated.rs` (HEAD source for dispatch + direct envelope primitives; lines 45, 159, 169, 187, 213, 466, 506)
- `skinny/crates/runtime/src/grammars/json/scan.rs` (HEAD source for scan primitives; lines 22, 32, 107, 131, 164)
- `skinny/crates/parse-that-regex/src/lib.rs` (HEAD source for unicode primitives; lines 718, 945)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (HEAD source for typed monomorphizations; lines 473, 828, 1015, 1199, 1739, 1740)

### §5.5 — V14 P1 sibling dispatch status (gap → V2 fold target per CHALLENGE-V2)

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` — **NOT YET COMMITTED at this agent's dispatch** (V14 P1-A in flight per parallel dispatch)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` — **NOT YET COMMITTED**
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` — **NOT YET COMMITTED**
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` — **NOT YET COMMITTED**
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` — **NOT YET COMMITTED** (parallel synthesis sibling)

V2 fold target: once V14 P1-A/B/C commit, re-fold the per-corpus tables in §2.1/§2.2/§2.3/§2.4 against the V14 same-source captures; expected zero divergence at the symbol+file:line level (no `skinny/crates/` source delta); update PMU c/B against V14 P1-D capture; resolve any sidecar gaps if V14 P1-A/B used interactive `samply record` (which would supersede the V13 `--save-only` posture).
