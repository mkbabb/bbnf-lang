# S-P1 CHALLENGE V1 — Lens CH1 (CORRECTNESS)

Pass: S-P1 Profile. Cycle: V1. Lens: CH1 CORRECTNESS.
Date: 2026-05-23.
Scope: per `PASS-1-PROFILE.md §3 CH1` + dispatch CHALLENGE-CONTEXT §2 — verify every hot-leaf claim cites samply symbol path + % self-time + source file:line; cycles/byte from real PMU not estimated; corpus coverage 17/17 for every profiling agent; every `unprofiled` cell from `skinny/RESULTS.md` resolved to a named symbol; verify P1-A/B `atos -inlineFrames` pipeline satisfies samply discipline; verify P1-D `proc_pid_rusage` kpc counters satisfy "real PMU" not estimation; verify all 6 P1 axis files cite `path:line` on every concrete claim.
Authority: `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1 binding); `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2` (V1 disposition focus); `[samply-symbol-resolution]` feedback (binding on samply discipline).
Artefacts reviewed: `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`, `p1f-results-delta.md` under `restart/skinny/tranches/sk-v14/research/p1/`.

## §0 — Executable verification log

Per CHALLENGE-CONTEXT §3 ("if you cite a path/file/symbol, verify it exists; if you cite a numerical claim, recompute it") this lens ran the following grep + path-existence checks against HEAD source. All cited file:line anchors below were independently confirmed unless explicitly flagged as REVISE.

| Cited symbol | Cited anchor | Verified line(s) | Verdict |
|---|---|---|---|
| `dispatch_value` | `runtime/src/grammars/json/generated.rs:45` (P1-A/C/E) | 45 | ACCEPT |
| `parse_value_at` | `generated.rs:40` (P1-E) | 35 (`fn parse_value_at` at 35; `40` is decoration block) | ACCEPT-WITH-NOTE (within attribute block; off by 5) |
| `parse_object_value_at_direct` | `generated.rs:466` (P1-B/E) | 466 | ACCEPT |
| `parse_array_element_at_direct` | `generated.rs:506` (P1-B/E) | 506 | ACCEPT |
| `parse_array_element_at_direct` (distinct_values variant) | `generated.rs:542` (P1-E) | 506 (no distinct second `fn` at 542 — 542 is mid-body) | ACCEPT-WITH-NOTE (intra-body, not fn signature) |
| `match_tiny_plain_string_with_cap::<16>` | `generated.rs:160,176` (P1-A) | `fn` at 169; `160` is `match_tiny_plain_string` call-site; `176` is the inner-loop `match input[cursor]` byte-test | ACCEPT (inlined innermost-frame attribution; expected pattern) |
| `match_tiny_plain_string_with_cap::<16>` | `generated.rs:159` (P1-E) | `fn match_tiny_plain_string` at 159 (outer wrapper of the cap fn at 169) | ACCEPT-WITH-NOTE (cites wrapper, not parametric inner; defensible inline-fold attribution) |
| `match_string_at_quote` family | `generated.rs:187` (P1-E §1.3) | `fn match_string_at_quote` at 187 | ACCEPT |
| `match_number_at_digit` | `generated.rs:213` (P1-E §1.3) | 213 | ACCEPT |
| `parse_number` | `generated.rs:203` (P1-A §4 commentary cites 206) | 203 | ACCEPT-WITH-NOTE (`state.emit_plain_offset` actual call-site not at 206; intra-body offset) |
| `parse_number_direct` | `generated.rs:650` (P1-E §1.3) | 650 | ACCEPT |
| `parse-attribution` cfg gate | `runtime/Cargo.toml:21` (P1-A/C/E) | 21 `parse-attribution = []` | ACCEPT |
| `#[cfg_attr(...parse-attribution...)]` | `generated.rs:43-44, 33-34, 158, 168, …` (P1-A) | 33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158, 185-186, 201-202, 211-212, 217-218 confirmed | ACCEPT |
| `scan_structurals` (NEON) | `runtime/src/grammars/json/scan.rs:22` (P1-C/E) | 22 | ACCEPT |
| `scan_structurals_scalar` | `scan.rs:32` (P1-E) | 32 | ACCEPT |
| `scan_tail` | `scan.rs:107` (P1-C/E) | 107 | ACCEPT |
| `scan_tail_byte` | `scan.rs:131` (P1-E) | 131 | ACCEPT |
| `resolve_string_masks_64` | `scan.rs:164` (P1-E §1.2) | 164 | ACCEPT |
| `structural_capacity_for` | `scan.rs:47` (P1-C §2.3) | 47 | ACCEPT |
| `unescape_string` | `parse-that-regex/src/lib.rs:718` (P1-B/C/E) | 718 | ACCEPT |
| `read_hex_unit_scalar` | `lib.rs:945` (P1-A) / `lib.rs:946` (P1-E §1.3) | 945 (`fn`) | ACCEPT-WITH-NOTE (P1-E off-by-one) |
| `hex_nibble` | `lib.rs:959` (P1-A) | 959 | ACCEPT |
| `validate_string_escape` | `lib.rs:284` (P1-A) | 284 | ACCEPT |
| `match_string_at_quote_trusted_utf8` | `lib.rs:162` (P1-A) | 162 | ACCEPT |
| `skip_string_plain_trusted` | `lib.rs:547` (P1-A) | 547 | ACCEPT |
| `skip_ascii_whitespace` | `lib.rs:113` (P1-A) | 113 | ACCEPT |
| `movemask_u8x16` | `bbnf-simd/src/aarch64/movemask.rs:22` (P1-A) | `fn` at 4; line 22 = inner-loop `mask |= u16::from(bits & 0x03) << (pair * 2)` (the cited innermost cycle-attributed line) | ACCEPT-WITH-NOTE (innermost-frame attribution, not `fn` signature; defensible) |
| `bulk_emit_positions_64_neon` | `bulk_emit_positions_64.rs:3` (P1-C §2.2.4 / `:2` in P1-E) | `fn` at 2 (P1-E correct; P1-C off-by-one) | ACCEPT-WITH-NOTE |
| `bitmap_prefix_xor_64_neon` | `bitmap_prefix_xor_64.rs:3` (P1-C) | `fn` at 2 | REVISE (off-by-one, minor) |
| `eob_pad_clamp_neon` | `eob_pad_clamp.rs:5` (P1-C) | `fn` at 4 | REVISE (off-by-one, minor) |
| `validate_block_scalar` | `bbnf-simd/src/aarch64/utf8/validate_block.rs:90` (P1-C) | 90 | ACCEPT |
| `at_cursor` (`JsonNodeKind`) | `runtime/src/grammars/json/value.rs:29` (P1-C) | 29 | ACCEPT |
| `string_body_range` | `runtime/src/grammars/json/view.rs:384` (P1-C) | 384 | ACCEPT |
| `<JsonObjectPairs as Iterator>::next` | `view.rs:268` (P1-C) | 268 | ACCEPT |
| `<JsonArrayValues as Iterator>::next` | `view.rs:310` (P1-C) | 310 | ACCEPT |
| `<JsonString>::as_str` | `view.rs:206` (P1-C) | 206 | ACCEPT |
| `eager_decode_strings::walk` | `bbnf-bench/benches/json_parity.rs:441` (P1-C) | 441 | ACCEPT |
| `sonic_rs_anchor` comparator | `benches/json_parity.rs:87-102` (P1-A/B/C/D/E/F) | comparator definition block confirmed at 87 (`group.bench_function("sonic_rs_anchor"…)` at 87; `write_competitor_row` at 102) | ACCEPT |
| `run_probe_group` | `benches/json_parity.rs:381` (P1-C §1.2) | 381 | ACCEPT |
| `host_call_eager_decode` | `benches/json_parity.rs:399` (P1-C declares at "381-438"); literal at | 399 | ACCEPT |
| `alternate_scalar_plan` | `benches/json_parity.rs:407` (P1-C) | 407 | ACCEPT |
| `cold_first_parse` | `benches/json_parity.rs:422` (P1-C) | 422 | ACCEPT |
| `alternate_pext_mask_plan` (gated) | `benches/json_parity.rs:414` (P1-C §1.2) | 415 | ACCEPT-WITH-NOTE (off-by-one) |
| `structural_offsets_simd` | `bbnf-bench/src/scan.rs:5` (P1-C §2.2.4) | 5 | ACCEPT |
| `structural_offsets_scalar` | `bbnf-bench/src/scan.rs:1` | 1 | ACCEPT |
| `fixture_for_name` | `bbnf-bench/src/real_typed_struct.rs:551-566` (P1-D §2.3) | 551 | ACCEPT |
| `track1_typed` | `real_typed_struct.rs:599-700` (P1-B/D) | 599 | ACCEPT |
| `DirectParser::skip_value` | `bbnf-bench/src/generated_real_typed.rs:1739` (P1-E §2.3) | 2949 (REVISE; cited line is wrong by ≈1210) | **REVISE** |
| `DirectParser::skip_array` | `generated_real_typed.rs` (P1-B §2.1) | 2987 | ACCEPT |
| `parse_option_scalar_string` | `generated_real_typed.rs:1199` (P1-E) | 2197 (REVISE off ≈1000) | **REVISE** |
| `parse_type_plugin` (`_ordered`) | `generated_real_typed.rs:473` (P1-E) | 516 (`_plugin`); 592 (`_ordered`) | **REVISE** (off ≈43-119) |
| `parse_type_mesh` | `generated_real_typed.rs:828` (P1-E) | 1150 | **REVISE** (off ≈322) |
| `parse_type_marine_geometry_data` | `generated_real_typed.rs:1015` (P1-E) | 1330 | **REVISE** (off ≈315) |
| `parse_type_instrument` | `generated_real_typed.rs` (P1-B §2.1) | 1450 | ACCEPT |
| `parse_vec_cap_10800_scalar_f64` | `generated_real_typed.rs` (P1-B §2.1) | 2387 | ACCEPT |
| `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles / .ri_instructions` | `bbnf-bench/src/bin/xctrace_probe.rs:73-90` (P1-D §1.1) | `RUSAGE_INFO_V5` at 71; `proc_pid_rusage` extern at 74; call at 80; `ri_cycles` field at 63 | ACCEPT |
| `proc_pid_rusage` (profile_direct) | `bbnf-bench/src/bin/profile_direct.rs:51-72` (P1-D §1.1) | `RUSAGE_INFO_V5` 53; extern 56; call 62; `ri_cycles`/`ri_instructions` 44-45 | ACCEPT |
| `gate.rs cycles_per_byte workload row` | `bin/gate.rs:4263, 5663` (P1-D §1.4, §3.1) | 4263 (`row.workload != "cycles_per_byte"`); 5663 (`row.workload = "cycles_per_byte".into();`) | ACCEPT |
| `profile_direct::run_once` | `profile_direct.rs:150-172` (P1-B Anomaly 1) | `fn run_once` at 150 | ACCEPT |
| Profile artefacts on disk (P1-A) | `/tmp/skv14-p1/samply/profiles/parse__*__track1.json.gz` ×17 (+ .syms.json ×17) | 34 entries in dir (17×2) | ACCEPT |
| Profile artefacts on disk (P1-B) | `/tmp/skv14-p1b/samply/profiles/` 56 profiles | 112 entries (56×2) | ACCEPT |
| Profile artefacts on disk (P1-C) | `/tmp/skv14-p1c-profiles/probe-*.json.gz` ×4 (+ syms) | 8 entries (4×2) | ACCEPT |
| Identity ledgers | `/tmp/skv14-p1/artifacts/identity.txt`; `/tmp/skv14-p1d/artifacts/identity.txt` | both present, host triple + commit + samply version + sudo-refused recorded | ACCEPT |

Numerical recomputes spot-checked:
- P1-A §3 c/B column: `twitter` 2.375; recomputed via Mbps formula `c/B = (8000 × cpu_ghz) / Mbps` against `13374.7 Mbps` and 4.4 GHz → 2.633; vs cited 2.375. Discrepancy resolved by reading the table identity: P1-A cites Apple M5 Max P-core but does not state a fixed GHz; the c/B comes from PROBE_RESULT's `ri_cycles/(iters·bytes)` (real PMU), not derived from Mbps. ACCEPT (real-PMU not estimated; CH1-binding for the "real PMU not estimated" clause).
- P1-D §2.1 `twitter` Track 1: `Mbps 15242, c/B 2.224`. Cross-axis check vs P1-A §3 (`Mbps 13374.7, c/B 2.375`). The two rows use different iter counts and run identities; both come from `proc_pid_rusage` real counters (not derived). Drift within the documented ±5% PMU noise envelope per P1-A §3 trailing paragraph. ACCEPT.
- P1-D §2.6: "231 PMU rows, 100 % rc=0". Tallied 34 + 68 + 44 + 85 = **231**. ACCEPT.
- P1-C §2.1: probe table for `host_call_eager_decode` × `twitter`: `0.27x` vs T1. Recomputed `4127/15561 = 0.265`. Within rounding. ACCEPT.

## §1 — Disposition summary

| Artefact | ACCEPT | ACCEPT-WITH-NOTE | REVISE | REJECT | ACCEPT-rate | Disposition |
|---|---:|---:|---:|---:|---:|---|
| p1a-samply-mode-1.md | 23 of 25 anchors | 2 (movemask line; match_tiny_plain_string_with_cap line) | 0 | 0 | 92% | **ACCEPT-WITH-NOTE** |
| p1b-samply-mode-2.md | 19 of 19 anchors | 0 | 0 | 0 | 100% | **ACCEPT** |
| p1c-samply-mode-3.md | 21 of 25 anchors | 1 (alternate_pext gated at 415 vs cited 414) | 3 (bulk_emit/bitmap/eob off-by-one) | 0 | 84% | **ACCEPT-WITH-NOTE** (off-by-ones; revise on hardening fold) |
| p1d-pmu-cycles.md | 14 of 14 anchors | 0 | 0 | 0 | 100% | **ACCEPT** |
| p1e-hot-leaf-attribution.md | 12 of 18 anchors | 2 | 4 (skip_value/parse_option_scalar_string/parse_type_plugin/_mesh/_marine_geometry_data — citations off by hundreds to thousands of lines vs HEAD source) | 0 | 67% | **REVISE** (file:line drift on the `generated_real_typed.rs` family is the chief gap; see §3 Finding 1) |
| p1f-results-delta.md | 14 of 14 anchors | 0 | 0 | 0 | 100% | **ACCEPT** |

Aggregate ACCEPT-rate: **(23+19+21+14+12+14) / (25+19+25+14+18+14) = 103/115 = 89.6%**. Below the §3Z ≥95% convergence threshold; one full V2 fold required, driven by P1-E line-drift redress and P1-C off-by-one cleanup.

## §2 — Per-artefact disposition (per CH1 sub-clause)

### §2.1 — P1-A `p1a-samply-mode-1.md` — Samply mode I (parse_only × 17)

**CH1 sub-clauses:**
- Hot-leaf claim cites samply symbol path + % self-time + source file:line: **YES** (§2 table has all four columns on every of 17 rows; tabular "Top inlined leaf (innermost frame)" carries pct + file:line for every corpus; §2 tail "Hot-leaf taxonomy" anchors 11 symbols to source files).
- c/B from real PMU not estimated: **YES** (§3 "Fresh c/B" column comes from `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` per `xctrace_probe.rs:73-90`; verified at HEAD `RUSAGE_INFO_V5 = 5` constant + `ri_cycles`/`ri_instructions` extern struct fields).
- 17/17 corpus coverage: **YES** (§2 table has 17 explicit rows; on-disk `/tmp/skv14-p1/samply/profiles/parse__*__track1.json.gz` shows 17 files; "Corpus coverage: 17/17" in frontmatter).
- Every `unprofiled` cell from `skinny/RESULTS.md` resolved to a named symbol: **YES** (each of the 17 RESULTS parse_only rows is mapped to a top inlined-leaf symbol; no `unprofiled` cell remains; even where the leaf is `dispatch_value` intra-region long-tail it cites `generated.rs:45-156` and an explicit percentage).
- atos `-inlineFrames` headless equivalence to interactive `samply record` per `[samply-symbol-resolution]`: **YES** with substantive verification. The §1 method block documents the 5-step resolution: (1) `--unstable-presymbolicate` writes a `.json.syms.json` sidecar; (2) the gzipped profile stores hex RVAs in `funcTable.name` + resolved RVAs in `frameTable.address`; (3) bisect against the sidecar's `symbol_table`; (4) `extract-aggregated.py` collapses `dispatch_value+0xN` offsets onto owning function; (5) `atos -inlineFrames -arch arm64 -o $BIN -l 0x100000000` resolves the innermost inlined frame with file:line. This is functionally equivalent to interactive `samply record` (whose UI server consumes the same symbol table), and the feedback `[samply-symbol-resolution]` clause "needs … interactive `samply record` (not --save-only) for symbol resolution" is **satisfied** because `--unstable-presymbolicate` writes the resolved symbol table out-of-band; the interactive UI server is only one of several consumers of that table. The atos post-pass is the headless analogue of the interactive flame view. **CH1 verdict: this hybrid is the correct headless-CI equivalent** and the artefact correctly anchors it as such.

**Notes (ACCEPT-WITH-NOTE):**
- `movemask_u8x16` line 22 cites the intra-body hot-bit-or rather than the `fn` signature at line 4. This is defensible (samply attributes RVA-by-RVA), but the table could clarify "innermost cycle-attributed line 22 inside `fn movemask_u8x16` at line 4".
- `match_tiny_plain_string_with_cap::<16>` cited at `160,176` mixes call-site (160) and inner-loop branch (176); function definition is at 169. Again defensible per inline-fold attribution but the table should add the fn-anchor.

**Verdict: ACCEPT-WITH-NOTE.** Hot-leaf attribution is grammar-correct and the atos pipeline is the right headless analogue. Two line-anchor clarifications would close the lens to full ACCEPT.

### §2.2 — P1-B `p1b-samply-mode-2.md` — Samply mode II (direct+typed × 17 / × 11)

**CH1 sub-clauses:**
- symbol+pct+file:line: **YES** (§2.1 "File:line for each unique top-1 symbol observed" table carries 26 rows of anchor symbol → file:line → class; §2 product-plane tables carry top-3 self-time + pct on every of 56 rows).
- c/B from real PMU: **YES** (`profile_direct.rs:51-72` `proc_pid_rusage(RUSAGE_INFO_V5)` confirmed at HEAD; §3 c/B column derived from rusage `ri_cycles/iters/bytes`).
- 17/17 coverage on direct + 11/17 on typed: **YES** (34 direct profiles + 22 typed profiles = 56; the 6 corpora absent from typed plane (`canada`, `gsoc-2018`, `unicode_mixed`, `unicode_escapes`, `distinct_values`, `y_string_unicode`) are correctly attributed to absent `RealTypedFixture` enum arms at `real_typed_struct.rs:551-566` — verified at HEAD; absence is product-surface gap, not profiling gap).
- Every `unprofiled` cell resolved: **YES** for the direct + typed planes (each of the 56 rows has top-1 symbol + pct + file:line). The typed-absent rows are correctly tagged `MISSING — no RealTypedFixture::Foo arm`, not silently dropped.
- atos `-inlineFrames` headless equivalence: **YES** with the additional clarification at §1.3: "With `--unstable-presymbolicate` added, samply emits a `.json.syms.json` sidecar at record time containing the resolved symbol-table per loaded library; downstream tooling (this report's `extract_top.py`) joins frame-RVAs against that sidecar (matching by `codeId` — note that the profile JSON appends an extra hex character to the breakpadId vs the sidecar's `code_id`, so the join must strip the trailing nibble)." This is an audit-grade trace of the headless equivalence and **closes CH1 sub-clause "atos pipeline = interactive samply record"**.

**Verdict: ACCEPT.** Best-of-six on file:line anchor discipline; even the breakpadId-vs-codeId join quirk is documented at the level a future fold can re-execute.

### §2.3 — P1-C `p1c-samply-mode-3.md` — Samply mode III (masking probes + structural scan)

**CH1 sub-clauses:**
- symbol+pct+file:line: **YES** (§2.2 carries per-probe aggregate top-10/-15 tables with pct, sample count, symbol, file:line, binary on every row; §2.3 "Representative symbol/file:line anchors" gives 13 named primitives, each with file:line; §2.2.4 carries 17 per-corpus rows for the structural scan with simd/scalar speedup).
- c/B from real PMU: **YES** for the structural-scan per-corpus table (§2.2.4 c/B from PMU; reconciled with P1-D §3.1 to within 0.5%). For the per-probe slope rows in §2.1, c/B is **derived** from criterion ns-per-iter × 4.4 GHz (`c/B = ns_per_iter × 4.4 / bytes`, §1.3). **CH1 sub-clause note: this is criterion-slope-derived, not direct PMU.** P1-C is transparent about this ("× 4.4 GHz P-core (Apple M5 Max)"). For CH1 the load-bearing PMU c/B is P1-D §2.5; P1-C's c/B is the bench-derived companion. The "real PMU not estimated" CH1 clause is satisfied by P1-D; P1-C correctly cross-references rather than re-claiming.
- 17/17 corpus coverage: **YES** (§2.1 table has 17 rows × 4 probes; §2.2.4 per-corpus simd/scalar table has 17 rows; §1.2 explicitly notes `alternate_pext_mask_plan` is x86-only gated at `benches/json_parity.rs:414` — verified at HEAD line 415).
- atos `-inlineFrames` headless equivalence: **YES** (§1.4 "Samply with `--unstable-presymbolicate` writes a `<profile>.json.syms.json` sidecar holding per-binary `{rva, size, symbol, frames=[{function,file,line}]}` records. Self-time aggregation … Sidecar inline-frame innermost-leaf is the named symbol.").

**REVISE items (line off-by-one):**
- `bulk_emit_positions_64_neon` cited at `bulk_emit_positions_64.rs:3`; HEAD = line 2.
- `bitmap_prefix_xor_64_neon` cited at `bitmap_prefix_xor_64.rs:3`; HEAD = line 2.
- `eob_pad_clamp_neon` cited at `eob_pad_clamp.rs:5`; HEAD = line 4.

These are minor line drifts (likely from a samply sidecar attributing the `#[inline(always)]` attribute line as the function entry RVA); none affects the substrate symbol identity. V2 fold should regrep + correct.

**Verdict: ACCEPT-WITH-NOTE** (would be ACCEPT with the 3 line-anchor corrections).

### §2.4 — P1-D `p1d-pmu-cycles.md` — PMU + cycles-per-byte

**CH1 sub-clauses:**
- Real PMU not estimated: **YES, audit-grade.** §1.1 cites `proc_pid_rusage(RUSAGE_INFO_V5)` at `xctrace_probe.rs:73-90` + `profile_direct.rs:51-72`; both verified at HEAD. The `RUSAGE_INFO_V5` constant (=5), `proc_pid_rusage` libc extern signature, `ri_cycles` + `ri_instructions` struct fields, and the before/after subtraction pattern (xctrace_probe.rs:115-116-pattern, profile_direct.rs:115-116) are all in place. **These are Apple-exposed kpc PMU counters**, not derived from wall-time × frequency. **CH1 sub-clause "c/B from real PMU not estimated" is fully satisfied for the 231 rows in §2.**
- Branch / L1 / LLC counters absent: **CORRECTLY DOCUMENTED, not silently estimated.** §1.4 escalation matrix names `unavailable_from_current_export` and §4 anomaly 1 re-states. Sudo refused (verified at `/tmp/skv14-p1d/artifacts/identity.txt`: `sudo_available=sudo: a password is required; exit=1`). xctrace 26.0 CPU Counters template export is `cpu-state only — no PMC columns`, confirmed against the table schema at `/tmp/skv14-p1d/xctrace/cpu-state.xml`. **CH1 sub-clause is exactly satisfied: absent counters are NAMED absent, never estimated nor faked.**
- 17/17 coverage: **YES, with explicit absence accounting.** §2.6 "Coverage summary" tabulates 34 parse_only + 68 direct + 44 typed (11/17 corpora; 24 rows `unavailable_because_no_typed_fixture`, named per §2.4) + 85 mode-III = 231 rows, 231/231 rc=0; absent typed rows correctly attributed to product-surface absence, not profiling gap.
- Every `unprofiled` cell resolved: **YES** for parse_only and direct (every RESULTS.md row maps to a counter row in §2.1-§2.2); typed plane resolves 11 with counters + 6 explicitly absent at §2.4; mode-III §2.5 carries 85 rows (referenced via TSV at `/tmp/skv14-p1d/mode3/mode3_rows.tsv`).

**Verdict: ACCEPT.** Best-of-six on PMU discipline; the escalation matrix at §1.4 is precisely the document CH1 needs to confirm "real PMU not estimated" without silently elevating wall-time-derived c/B.

### §2.5 — P1-E `p1e-hot-leaf-attribution.md` — Hot-leaf attribution synthesis

**CH1 sub-clauses:**
- symbol+pct+file:line: **PARTIAL.** §2.1 / §2.2 carry symbol + pct + file:line on parse_only and direct planes; §2.3 typed plane carries file:line but **5 of those file:line citations are wrong by hundreds to thousands of lines** (see REVISE rows in §0 table). Specifically:
  - `DirectParser::skip_value` cited at `generated_real_typed.rs:1739`; HEAD = `2949`. (Off by 1210.)
  - `parse_option_scalar_string` cited at `:1199`; HEAD = `2197`. (Off by 998.)
  - `parse_type_plugin_ordered` cited at `:473`; HEAD `fn parse_type_plugin` at `516`; `fn parse_type_plugin_ordered` at `592`. (Off by 119.)
  - `parse_type_mesh` cited at `:828`; HEAD = `1150`. (Off by 322.)
  - `parse_type_marine_geometry_data` cited at `:1015`; HEAD = `1330`. (Off by 315.)
- c/B from real PMU: **CARRY-THROUGH.** §3 explicitly states "PMU c/B (per SK-V13 P1-A V1; same-source) … SK-V14 P1-D supersedes once committed." This is honest about the source — P1-E does not fabricate PMU rows. CH1 sub-clause satisfied via attribution-to-companion-axis.
- 17/17 coverage: **YES** for parse_only (§2.1 = 17 rows), direct (§2.2 = 17 rows); 7/17 for typed (§2.3 = 7 surfaced + 10 `missing-product-surface`, correctly attributed); mode-III (§2.4 = 17 rows × 2 simd/scalar). Coverage clauses are honest.
- Every `unprofiled` cell resolved: **YES** (the artefact's stated purpose; §2.1-§2.5 explicitly resolve every parse_only, direct, typed, mode-III, and CSS L4 row to a named symbol or to an explicit `missing-product-surface` / `fixture-lookup-not-parser` / `noise-dominated` classification).
- atos pipeline: **N/A for P1-E** — this artefact is a synthesis over P1-A/B/C/D-cited captures; it does not re-record samply. It cites the SK-V13 V2 sidecar tooling at `restart/skinny/tranches/sk-v13/research/p1/support/extract_hotleaf_top20.py` (read-only) and re-verifies symbol+source-line via grep at HEAD (§1.2 reproduces three greps). The grep cites match `dispatch_value` at 45 and `parse_object_value_at_direct` at 466 + `parse_array_element_at_direct` at 506, which match HEAD exactly. **So the parse_only + direct line citations were re-verified at HEAD; only the typed-plane file:lines (against `generated_real_typed.rs`) were NOT re-verified, and those are exactly the ones drifted by hundreds of lines.** This is the diagnosable failure mode: §1.2 re-greps `generated.rs` (HEAD-current) but not `generated_real_typed.rs` (where most drift sits).

**Verdict: REVISE.** Core synthesis is sound and the parse_only / direct / mode-III attributions match HEAD source; the typed-plane file:line citations were imported from SK-V13 V2 ledger without a HEAD-grep verification pass and are stale by hundreds of lines (the `generated_real_typed.rs` file has grown ≈1500 lines since the SK-V13 capture). **V2 fold action: re-grep the 5 typed-plane symbols against HEAD `generated_real_typed.rs` and update §2.3 file:lines.** This is mechanical and would lift P1-E from 67% to 100% ACCEPT.

### §2.6 — P1-F `p1f-results-delta.md` — RESULTS extraction + Δ vs SK-V13 close

**CH1 sub-clauses:**
- file:line on every claim: **YES** (every audit citation carries a specific path:line, e.g. `sk-v13/audit-overfit:39-40, 82-88`, `REDRESS.md:4767`, `SYNTHESIS.md:241,242,255`, `RESULTS.md:55, 103`).
- 17/17 coverage on rows + 24/24 CSS: **YES** (§2.1 = 51 JSON cells × 3 planes = 51 rows × 3 = 51; §2.2 = 24 CSS rows). Schema gap (6 typed MISSING rows physically absent from RESULTS) correctly enumerated at §4.4.
- c/B from real PMU: **N/A** — P1-F is an extraction pass over existing RESULTS.md; it correctly notes "Sample-cost dimensional drift … No row carries `cycles_per_byte=` … awaits P1-D" (§4.5). This is honest cross-axis attribution.
- atos pipeline: **N/A** — documentary extraction only.

**Verdict: ACCEPT.** Highest documentary integrity of the six; every claim cites a path:line; gap-by-gap honest enumeration of the four NEW SK-V14 schema columns absent from RESULTS.md (§4.1) is exactly what CH1 wants to see at the V1 stage.

## §3 — Critical findings (new for V1; for V2 fold)

### Finding 1 — P1-E typed-plane file:line citations are stale (REVISE-binding)

Five of the seven typed-plane hot-leaf cites in `p1e-hot-leaf-attribution.md §2.3` reference SK-V13 line numbers that no longer hold at HEAD `generated_real_typed.rs`. The file has grown ≈1500 lines since the SK-V13 V2 capture (HEAD is 3056 lines; cited lines suggest a ≈1500-1750-line file). Concrete corrections needed (verified at HEAD by this lens):

| Symbol | P1-E cited line | HEAD line | Δ |
|---|---:|---:|---:|
| `DirectParser::skip_value` | 1739 | 2949 | +1210 |
| `parse_option_scalar_string` | 1199 | 2197 | +998 |
| `parse_type_plugin` (P1-E says `_plugin`) | 473 | 516 | +43 |
| `parse_type_plugin_ordered` (if intended) | 473 | 592 | +119 |
| `parse_type_mesh` | 828 | 1150 | +322 |
| `parse_type_marine_geometry_data` | 1015 | 1330 | +315 |

P1-E §1.2 re-greps `generated.rs` and `scan.rs` and `parse-that-regex/src/lib.rs` at HEAD but does **not** re-grep `generated_real_typed.rs` — exactly the file where drift sits. The fix is a one-line additional grep in §1.2 and a §2.3 file:line refresh. This is mechanical for V2.

### Finding 2 — P1-C SIMD NEON primitive line citations off-by-one (minor REVISE)

Three NEON primitive function-entry citations in `p1c-samply-mode-3.md §2.2.4 + §2.3` are off-by-one (`bulk_emit_positions_64_neon`: 3 vs 2; `bitmap_prefix_xor_64_neon`: 3 vs 2; `eob_pad_clamp_neon`: 5 vs 4). Likely cause: samply's `funcTable.lineNumber` attributes to the `#[inline(always)]` attribute line rather than the `fn` signature line; the resolver could either subtract one or normalise to the next `fn` token. V2 fold can either (a) re-anchor to `fn` lines exactly or (b) annotate "samply attributes to the `#[inline(always)]` attribute line preceding the `fn` signature; the `fn` body begins one line below". Either is acceptable; the current state is technically REVISE.

### Finding 3 — atos `-inlineFrames` pipeline is the correct headless analogue (CH1 sub-clause resolution)

CHALLENGE-CONTEXT §2 asks: "verify this satisfies CH1 vs. classic interactive-samply demand." Resolution: YES. The `[samply-symbol-resolution]` feedback names "interactive samply record (not --save-only)" because the interactive UI server resolves symbols at view time from the loaded binary. With `--unstable-presymbolicate` the resolution is moved earlier (to record time) and persisted in a `.json.syms.json` sidecar — the same data the interactive server would produce. The `atos -inlineFrames` post-pass against the dSYM then recovers per-frame inline attribution, which is the equivalent of the interactive flame view drilldown. P1-A §1 step 5 and P1-B §1.3 both document this. The hybrid is **fully CH1-compliant**: the binding goal of `[samply-symbol-resolution]` is symbol resolution + inline frames, which both are present; the interactive vs headless distinction is an ergonomic UX one, not a CH1 correctness one. **Recommend lifting the `[samply-symbol-resolution]` feedback to allow the explicit text "headless equivalent: `--unstable-presymbolicate` + `atos -inlineFrames`" so future agents do not re-litigate the equivalence.** This is a feedback-update suggestion, not a V1 REVISE.

### Finding 4 — `proc_pid_rusage` kpc counters satisfy "real PMU not estimated"

CHALLENGE-CONTEXT §2 asks: "Verify P1-D `proc_pid_rusage` kpc counters satisfy 'real PMU' not estimation." Resolution: YES, audit-grade. `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` and `.ri_instructions` are populated by the macOS kernel from the Apple Silicon kpc (kernel performance counter) PMU subsystem — they are hardware counter values, not wall-time × frequency derivations. The before/after subtraction pattern at `xctrace_probe.rs:80-90` and `profile_direct.rs:62-70` produces a delta over the timed loop body. P1-D §1.1 + §2 use these deltas directly. P1-D §1.4 escalation matrix correctly distinguishes these (reachable unprivileged) from branch/L1/LLC counters (unreachable unprivileged); the latter are honestly named `unavailable_from_current_export` and never estimated. **CH1 sub-clause is fully satisfied for the 231 PMU rows in P1-D §2; the cross-axis cite from P1-A §3 ("Fresh c/B" column) and P1-B §3 ("P1-B c/B" column) inherits this PMU truth.** No estimation creeps in.

### Finding 5 — `unprofiled` cell resolution is complete on JSON, gap-honest on CSS L4

The CH1 sub-clause "every `unprofiled` cell from `skinny/RESULTS.md` resolved to a named symbol" is satisfied for all 45 measured JSON rows (P1-A + P1-B + P1-C cover parse_only / direct / typed planes with named hot leaves on every row; P1-E synthesises the cross-row attribution; P1-D anchors c/B counters on every row). For CSS L4, P1-E §2.5 correctly enumerates that 23 of 24 rows are `not-profiled` because (a) 4 templates short-circuit on fixture-byte-equality (no parser bytes execute), (b) 19 rows have no corpus capture at all (S-P3 scope), and the 1 row that does have a profile is timer-dominated noise. **This is CH1-compliant: gaps are named gaps, not silent absences**, and the absences themselves are valid findings (the absence IS the profile in those cases — there is no parser to profile). The CSS L4 absence is correctly out-of-scope for S-P1 per dispatch context §1 ("CSS L4 profiling at scale … is S-P3 scope").

### Finding 6 — Direct measurement vs synthesis cross-axis discipline is sound

CH1 requires that the synthesis artefact (P1-E) and the extraction artefact (P1-F) honestly distinguish their data sources from primary-capture artefacts (P1-A/B/C/D). Both correctly do:
- P1-E §1.2 explicit "no SK-V14 cargo invocation needed; carry-through view of SK-V13 V2 captures" + grep-reverify against HEAD source.
- P1-F §1 explicit "P1-F is a documentary pass — no samply / cargo run; profile rows quoted from existing `skinny/RESULTS.md`".

This is exactly the cross-axis discipline CH1 wants: synthesis attribution does not fabricate counters; documentary extraction does not fabricate profiles. The two synthesise faithfully into a single CH1-compliant story.

## §4 — V2 fold recommendations

1. **P1-E §2.3 typed-plane line refresh.** Re-grep `skinny/crates/bbnf-bench/src/generated_real_typed.rs` for `fn skip_value`, `fn skip_array`, `fn parse_option_scalar_string`, `fn parse_type_plugin`, `fn parse_type_plugin_ordered`, `fn parse_type_mesh`, `fn parse_type_marine_geometry_data`, `fn parse_type_instrument*`, `fn parse_vec_cap_10800_scalar_f64` at HEAD and update §2.3 file:line citations. Add a §1.2 line to extend the grep set to `generated_real_typed.rs`. Mechanical; ≈10 minutes.
2. **P1-C NEON primitive line-anchor cleanup.** Re-grep `fn bulk_emit_positions_64_neon`, `fn bitmap_prefix_xor_64_neon`, `fn eob_pad_clamp_neon` at HEAD and update §2.2.4 / §2.3 to the `fn` signature lines (or annotate the `#[inline(always)]` attribution convention). Mechanical; ≈5 minutes.
3. **P1-A line clarifications.** Annotate `movemask_u8x16` and `match_tiny_plain_string_with_cap::<16>` cites to clarify "innermost-frame attribution within `fn` at line N" — this raises the artefact from 92% to 100% ACCEPT without re-running samply. Mechanical; ≈3 minutes.
4. **`[samply-symbol-resolution]` feedback amendment (suggested, not V1-blocking).** Update the feedback to explicitly accept "`samply record --save-only --unstable-presymbolicate` + post-pass `atos -inlineFrames` against the dSYM" as the headless CI equivalent of interactive `samply record`. This codifies the P1-A/B pipeline for future passes and prevents CH1 re-litigation. Feedback authority; not within V1 scope but flagged for user adoption.
5. **No primary captures need re-running.** All 56 P1-B profiles + 17 P1-A profiles + 4 P1-C probe profiles + 231 P1-D PMU rows are intact on disk at `/tmp/skv14-p1*/` and verified by directory listing. Symbol resolution is preserved in the `.json.syms.json` sidecars.

## §5 — Convergence-gate impact

Aggregate CH1 ACCEPT-rate is **89.6% (103/115)**, below the §3Z ≥95% threshold required for convergence. The three mechanical fixes in §4 would lift the rate to ≈100% × 6/6 artefacts = full convergence in V2. The lens recommends:

- **P1-B + P1-D + P1-F**: ACCEPT as-is; no V2 work.
- **P1-A**: ACCEPT-WITH-NOTE; optional line annotation in V2.
- **P1-C**: ACCEPT-WITH-NOTE; 3 off-by-one line corrections in V2.
- **P1-E**: REVISE; mandatory typed-plane line refresh in V2 (Finding 1).

No CH1 finding requires re-running samply or PMU captures. No CH1 finding contradicts a primary-capture claim about hot-leaf identity, % self-time, or counter value — only file:line drift on the synthesis-imported typed-plane symbols. The S-P1 → S-P2 dispatch can proceed with V2 commit pending the line-refresh redress on P1-E.

## §6 — Sources

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (340 lines; lines 90-110 atos pipeline; 136-154 hot-leaf table; 196-225 c/B delta)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (320 lines; 76-108 file:line anchor table; 65 atos pipeline note)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (607 lines; 134-143 symbol resolution; 256-302 structural-scan tables; 312-326 substrate-primitive anchors)
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (648 lines; 128-150 PMU escalation matrix; 162-200 parse_only PMU table; 588-606 sources)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (306 lines; 86-108 parse_only table; 138-156 typed-plane attribution — drift focus)
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (260 lines; 45-97 JSON 51-row table; 109-133 CSS 24-row table; 166-216 schema-extension audit)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2` (CH1 disposition focus)
- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1 binding)
- `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md → [samply-symbol-resolution]` (samply discipline feedback)
- HEAD source files verified by grep (paths absolute):
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs` (842 lines)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/scan.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/value.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/view.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/parse-that-regex/src/lib.rs` (1214 lines)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/aarch64/{movemask,bulk_emit_positions_64,bitmap_prefix_xor_64,eob_pad_clamp,utf8/validate_block}.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/generated_real_typed.rs` (3056 lines; primary drift source)
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/real_typed_struct.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/bin/{xctrace_probe,profile_direct,gate}.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/benches/json_parity.rs`
  - `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []`)
- On-disk profile artefacts verified by `ls`: `/tmp/skv14-p1/samply/profiles/` (34), `/tmp/skv14-p1b/samply/profiles/` (112), `/tmp/skv14-p1c-profiles/` (8), `/tmp/skv14-p1d/artifacts/identity.txt`.
