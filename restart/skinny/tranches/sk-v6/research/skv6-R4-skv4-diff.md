# SK-V6 Wave 1 R4: SK-V4 Diff for Largest Regressions

Baseline check: `skinny/profile/reassay-skv4-2026-05-13/` exists, but it contains only `PROFILE-REPORT.md`; there are no per-target profile JSONs or target leaf tables for `gsoc-2018`, `distinct_values`, or `y_string_unicode`. The SK-V4 side is therefore leaf-unresolved beyond the fused retained parse hub (`runtime::generated_json::generated::parse_value_at`) described by that report. `restart/skinny/tranches/sk-v5/research/skv5-B1-parse-attribution.md` is used only as historical context for the old UTF-8-validator hypothesis; it did not profile these three regression rows.

Build/profile evidence:

- Build: `cd skinny; export CARGO_TARGET_DIR=/tmp/skv6-cargo/R4; cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution`
- Profiles:
  - `/tmp/skv6-R4-profiles/gsoc-2018.profile.json.gz`
  - `/tmp/skv6-R4-profiles/distinct_values.profile.json.gz`
  - `/tmp/skv6-R4-profiles/y_string_unicode.profile.json.gz`
- Fresh profile-loop Mbps under `runtime/parse-attribution`: `gsoc-2018` 20032, `distinct_values` 5289, `y_string_unicode` 5448. The table uses canonical SK-V6 Mbps from `skinny/RESULTS.md`, with profile-loop Mbps in parentheses.

| Corpus | SK-V4 Mbps | SK-V6 Mbps | Delta | Old Hot Leaf | New Hot Leaf | Diagnosis Revision |
|---|---:|---:|---:|---|---|---|
| `gsoc-2018` | 47481 | 21907 (20032 profiled) | -54% | Leaf-unresolved SK-V4 target row; accessible report collapses retained parse under fused `parse_value_at`. | `match_string_at_quote` 63.4%, `match_tiny_plain_string` 19.0%; next parser/event leaves are `consume_container_next` 4.2%, `parse_key_colon` 3.7%, `emit_plain_offset` 2.4%. | Revision from generic fused parse hub to string boundary overhead. The new honest generated runtime spends most self-time proving string delimiters/escapes/control boundaries through `match_json_string_at_quote_trusted_utf8`, not on source hooks, direct SinkOnly codegen emission, or side-table projection. Offset tape/event emission is visible but secondary. |
| `distinct_values` | 16241 | 6097 (5289 profiled) | -62% | Leaf-unresolved SK-V4 target row; accessible report collapses retained parse under fused `parse_value_at`. | `match_tiny_plain_string` 56.3%, `match_string_at_quote` 17.7%, `consume_quote_at_cursor` 6.4%, `emit_plain_offset` 5.5%, `consume_container_next` 4.3%, `parse_key_colon` 4.0%. | Revision to tiny-string boundary overhead plus secondary event emission. This row is not UTF-8-validation bound; the hot path is branchy short-string recognition and quote/offset bookkeeping on the generated retained runtime. |
| `y_string_unicode` | 13109 | 6084 (5448 profiled) | -54% | Leaf-unresolved SK-V4 target row; accessible report collapses retained parse under fused `parse_value_at`. | `match_string_at_quote` 62.2%, `consume_container_next` 7.9%, `match_tiny_plain_string` 7.2%, `patch_flags` 4.5%, `emit_plain_offset` 3.8%, `consume_quote_at_cursor` 3.8%, `parse_string` 3.6%. | Revision to string/unicode boundary overhead with meaningful retained tape flag/offset costs. No `validate_utf8_codepoint` leaf appears because the current generated parse path calls the trusted-UTF-8 string matcher after `&str` admission. |

Leaf-by-leaf delta summary:

- SK-V4 target rows cannot be honestly split below `parse_value_at` from the accessible SK-V4 baseline directory.
- SK-V5 B1's old parse-G diagnosis was `validate_utf8_codepoint` plus `skip_json_string_plain`; that diagnosis does not transfer to these SK-V6 rows because the current generated retained parser uses the trusted-UTF-8 matcher and exposes `match_string_at_quote` / `match_tiny_plain_string` instead.
- The shared regression cluster is string boundary recognition in the honest generated retained runtime. The common overhead is not direct source hooks, not generated SinkOnly emission, and not parse-time side tables. Offset tape/event substrate shows up as `emit_plain_offset`, `consume_quote_at_cursor`, `patch_flags`, and container-next leaves, but it is a second-order cost behind string boundary matching on all three rows.
- Branch/i-cache remains unproven by these profiles; no PMU evidence was collected in R4.

Single candidate intervention to falsify:

Test one collapsed string-boundary recognizer that removes the double tiny/full-string probe shape for retained parse strings while preserving trusted-UTF-8 semantics. Falsifiability gate: on these same three R4 corpora, a before/after `runtime/parse-attribution` samply run must reduce combined `match_string_at_quote + match_tiny_plain_string` self-time below 45% on every row and improve canonical Track 1 Mbps by at least 10% on all three without moving `emit_plain_offset + consume_quote_at_cursor + patch_flags` above 20% self-time.
