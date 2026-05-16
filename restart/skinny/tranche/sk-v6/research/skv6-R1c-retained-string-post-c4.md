# SK-V6 Wave 1c R1c: retained string boundary post-Candidate4

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Role: R1c retained Track 1 attribution after REDRESS 63 (`ContainerNext` / next-byte carry)
Repo edits: none

## Authority read

- `skinny/RESULTS.md` current retained parse authority after Candidate4.
- `skinny/REDRESS.md` item 63: `ContainerNext` admitted; retained parse-G remains 13.
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §8: Candidate 1/2 rejected, Candidate 3 rejected, Candidate 4 admitted.
- `restart/skinny/audit/SK-V6-COHORT/skv6-R1-parse-regressed.md`, `skv6-R2-parse-original-g.md`, `skv6-R4b-string-distribution.md`.

Pre-blocked routes honored: REDRESS 60-63 remain closed. This report does not propose removing `match_tiny_plain_string`, always/delayed 64-byte string scanners, sidecar cursors, retained UTF-8 fusion, direct decoded-string materializers, or quote-source fused materialization.

## Build and capture

Built current HEAD with parse attribution:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv6-cargo/R1c
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution
```

Profiles recorded with:

```bash
/Users/mkbabb/.cargo/bin/samply record --rate 4000 --main-thread-only \
  --unstable-presymbolicate --save-only --no-open \
  -o /tmp/skv6-R1c-profiles/<row>.profile.json.gz \
  /tmp/skv6-cargo/R1c/release/profile-lazy <iters> <fixture-path>
```

Profile artefacts live under `/tmp/skv6-R1c-profiles/`. The profile JSON remains `symbolicated=false`; I mapped leaf RVAs through each `*.profile.json.syms.json` symbol table and confirmed the `match_string_at_quote` PC regions with `llvm-objdump`.

`c/B` below uses the current `skinny/RESULTS.md` retained Track 1 Mbps at 3.5 GHz (`28000 / Mbps`). Samply Mbps is capture-throughput only and is lower due profiler overhead.

## Post-Candidate4 attribution table

| Row | RESULTS Track 1 Mbps | c/B | Samply Mbps | Samples | Dominant hot symbols | Interpretation |
|---|---:|---:|---:|---:|---|---|
| `apache_builds` | 12511 | 2.238 | 5516 | 18830 | `match_tiny_plain_string` 29.2%, `match_string_at_quote` 25.5%, `parse_key_colon` 12.4%, `consume_container_next` 7.6% | Still string-bound; Candidate4 did not expose a new control-flow bottleneck. |
| `github_events` | 13184 | 2.124 | 7281 | 7084 | `match_string_at_quote` 32.4%, `match_tiny_plain_string` 29.8%, `parse_key_colon` 7.8%, `consume_container_next` 7.8% | Mixed short/full string scan remains dominant. |
| `update_center` | 9259 | 3.024 | 4553 | 25008 | `match_tiny_plain_string` 37.2%, `match_string_at_quote` 27.1%, `parse_key_colon` 5.8%, `emit_plain_offset` 5.7% | Dense object string row; no post-C4 non-string close appears. |
| `gsoc-2018` | 21928 | 1.277 | 16560 | 12959 | `match_string_at_quote` 65.1%, `match_tiny_plain_string` 17.4% | Long/trusted string delimiter scan remains the row. |
| `unicode_mixed` | 8107 | 3.454 | 5295 | 25566 | `match_string_at_quote` 78.1%, `match_tiny_plain_string` 5.4% | Trusted string matcher dominates; not a raw UTF-8 validator row. |
| `unicode_escapes` | 9908 | 2.826 | 5352 | 24302 | `match_string_at_quote` 92.3%; next symbols all <1% | Escape/Unicode validation inside trusted matcher is the row. |
| `unicode_basic` | 11092 | 2.524 | 4993 | 26947 | `match_tiny_plain_string` 29.2%, `match_string_at_quote` 25.7%, `consume_quote_at_cursor` 7.3%, `emit_plain_offset` 6.0% | String boundary split unchanged from pre-C4. |
| `distinct_values` | 6144 | 4.557 | 3386 | 57876 | `match_tiny_plain_string` 48.7%, `match_string_at_quote` 19.8%, `consume_quote_at_cursor` 6.6%, `parse_key_colon` 6.2% | Short/mid-string scalar boundary remains dominant. |
| `y_string_unicode` | 6272 | 4.464 | 4320 | 26368 | `match_string_at_quote` 64.3%, `consume_array_next` 7.7%, `match_tiny_plain_string` 6.0%, `patch_flags` 4.1% | Escape-heavy retained string row; Candidate4 added visible array-next cost but not the top blocker. |

## What changed after Candidate4

Candidate4 succeeded at its control-flow target, but it did not change the dominant string diagnosis for these rows.

| Row | Pre-C4 cohort signal | Post-C4 signal | Change |
|---|---|---|---|
| `apache_builds` | R1: combined string boundary 68.2%, `match_tiny_plain_string` 40.4% | combined string boundary 54.7% | Some string share fell, but string remains the top cluster; no new non-string candidate. |
| `github_events` | R1: combined string boundary 67.1%, tiny/quote tie | combined string boundary 62.2% | Same diagnosis. |
| `update_center` | R1: `match_tiny_plain_string` 39.2%, generated string-wrapper row | combined string boundary 64.3% | Still generated string-wrapper dominated. |
| `gsoc-2018` | R1: `match_string_at_quote` 63.0%, combined string boundary 84.8% | quote 65.1%, combined 82.5% | Essentially unchanged. |
| `unicode_mixed` | R2: quote 71.8%, combined string boundary 78.8% | quote 78.1%, combined 83.5% | Still full trusted matcher; no `validate_utf8_codepoint` leaf. |
| `unicode_escapes` | No R1/R2 pre-C4 row; R4b classed long + escape dominated | quote 92.3% | This is now the clearest retained escape-validation target. |
| `unicode_basic` | R2: tiny 28.0%, quote 25.5%, combined 53.5% | tiny 29.2%, quote 25.7%, combined 54.9% | Unchanged. |
| `distinct_values` | R1: combined string boundary 81.3%, tiny 55.9% | combined string boundary 68.5%, tiny 48.7% | String remains dominant despite lower share. |
| `y_string_unicode` | R1: `match_string_at_quote` 62.6% | quote 64.3%, `consume_array_next` 7.7% | Escape string remains dominant; array-next is visible but secondary. |

Post-C4 `consume_array_next` is now visible where expected (`y_string_unicode` 7.7%, `unicode_basic` 4.3%, `apache_builds` 2.0%), while old `consume_container_next + parse_value_at + dispatch_value` no longer looks like the first string-row blocker. That matches REDRESS 63: the control-flow candidate was real, but it is not the string close.

## PC-level detail inside `match_string_at_quote`

Top offsets inside `runtime::generated_json::generated::match_string_at_quote` split into two classes:

- Plain delimiter scan offsets (`+0x68`, `+0x16c`, `+0x188`) dominate rows such as `apache_builds`, `distinct_values`, `unicode_basic`, and `update_center`.
- Escape / `\uXXXX` validation offsets (`+0x220`, `+0x258`, `+0x3f0..+0x6e4`) dominate `unicode_escapes`, `unicode_mixed`, and `y_string_unicode`. In disassembly these are the branch and hex/surrogate-validation blocks after a backslash special byte, not raw UTF-8 validation.

Representative top PC buckets:

| Row | Top `match_string_at_quote` PC buckets | Meaning |
|---|---|---|
| `apache_builds` | `+0x188` 28.2%, `+0x68` 17.3%, `+0x16c` 14.5% | Plain quote/backslash/control scan. |
| `distinct_values` | `+0x188` 25.1%, `+0x16c` 15.0%, `+0x68` 9.5% | Plain mid-string scan. |
| `unicode_basic` | `+0x188` 48.5%, `+0x16c` 10.4%, `+0x68` 10.0% | Plain trusted string scan. |
| `unicode_escapes` | `+0x220` 18.3%, `+0x258` 17.8%, `+0x500` 8.8%, `+0x6e4` 7.0% | Escape dispatch plus hex/surrogate validation. |
| `unicode_mixed` | `+0x220` 23.8%, `+0x3f0` 16.3%, `+0x258` 14.8% | Backslash/special-byte path dominates over tiny-string. |
| `y_string_unicode` | `+0x4bc` 13.6%, `+0x4ec` 11.0%, `+0x48c` 7.5%, `+0x508` 5.3% | Unicode escape-unit validation path. |

No row showed `validate_utf8_codepoint` as a sampled leaf. The retained parser is still on the trusted-UTF8 path. UTF-8 fusion remains non-canonical for this row set.

## One admissible next retained intervention

Candidate: retained Unicode-escape run validator specialization.

Scope:

- `skinny/crates/parse-that-regex/src/lib.rs`: split `validate_json_unicode_escape_run` into an attribution-visible helper and route contiguous `\uXXXX` runs through a batched validator.
- `skinny/crates/bbnf-simd/src/aarch64/`: add a grammar-neutral `hex4x4_validate`/`uxxxx_run_validate` primitive if the scalar helper first proves row impact. The primitive validates four `\uXXXX` units and surrogate-pair legality without materializing decoded chars.
- Same-wave consumer: `match_json_string_at_quote_trusted_utf8` via `validate_json_string_escape`; no orphan primitive, no new BIR variant, no directive, no sidecar substrate.

Why this is admissible:

- It targets the fresh post-C4 PC evidence: `unicode_escapes` is 92.3% inside `match_string_at_quote`, with the top PCs in escape dispatch and hex/surrogate validation, not in raw UTF-8 validation or direct decoded materialization.
- It is not Candidate 1/2/3 repeated. Tiny string remains. No always/delayed wide plain scanner is introduced. Plain rows are gated away unless they actually hit `\u` escape runs.
- It generalizes as a grammar-neutral escaped-code-unit primitive: JSON uses fixed-width `\uXXXX`; CSS, string DSLs, and other grammars can lower their own escape width/table policies to the same byte-class/hex-validation primitive family without adding directives.

Falsifiability gate:

- Focus rows: `unicode_escapes`, `y_string_unicode`, `unicode_mixed`.
- Guard rows: `apache_builds`, `github_events`, `update_center`, `gsoc-2018`, `unicode_basic`, `distinct_values`.
- Accept only if retained Track 1 improves at least `unicode_escapes >= +12%`, `y_string_unicode >= +8%`, and one of `unicode_mixed` or `gsoc-2018 >= +5%`; no guard row regresses by more than 2%.
- Attribution gate: `match_string_at_quote` escape-region PCs (`+0x220..+0x6e4`) drop at least 25% on `unicode_escapes` and `y_string_unicode`; plain-scan PCs remain within noise on guard rows.
- If the scalar helper split shows that hex/surrogate validation is not the real sub-boundary after de-inlining, reject before SIMD/ASM and record the scalar measurement in REDRESS.

This is a narrow escape-cluster intervention, not a global retained parse close. The plain string rows still need synthesis from R2c/R3c/R6c; R1c alone does not justify another universal string scanner.

## Bottom line

Candidate4 cleaned up container/array dispatch, but the requested string-heavy retained rows are still dominated by generated string matching. The post-C4 distinction is sharper: `apache_builds`, `update_center`, `unicode_basic`, and `distinct_values` are plain/tiny delimiter-scan rows; `unicode_escapes` and `y_string_unicode` are Unicode-escape validation rows; `unicode_mixed` straddles the special-byte path. The only R1c-backed intervention worth carrying forward is the narrow retained Unicode-escape run validator, with a scalar split first and a strict same-row falsification gate.
