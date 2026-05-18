# SK-V9 S-P1 V3 Hardening — CH3 REGRESSION Disposition

Lens: CH3 REGRESSION (per `restart/prompts/ORCHESTRATOR.md` §3W).
Scope: SK-V9 S-P1 V3 cohort committed at `c6fb0342` — the six reports
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`.
Authority cross-checked: `skinny/REDRESS.md` entries 1–93,
`restart/skinny/tranches/sk-v9/HANDOFF.md` §5,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`.
Verdict vocabulary: ACCEPT / WATCH / REVISE / REJECT — bound to a REDRESS
citation and a material-differential argument.
Disposition: read-only. No edits, no commits.

---

## §1 — Method

### §1.1 — Protocol

For each finding or proposal that could touch a REDRESS-rejected class, ask
two questions in order:

1. **Does the V3 finding/proposal reopen a REDRESS-rejected route?** A
   "route" is the (owner-surface, hot leaf, mechanism) triple — not the
   prose label. Renaming a route is not a new route.
2. If the answer is YES, **(a) is there fresh measurement evidence
   contradicting the original rejection, AND (b) does the V3 text explicitly
   cite the REDRESS entry and explain why this shape is materially
   different?** Both halves are required by §3W. Either one alone collapses
   the proposal back into the rejected class.

Disposition values:

| Value | Meaning |
|---|---|
| ACCEPT | The finding either does not reopen any rejected route, or it does so with both (a) fresh evidence and (b) explicit cite+differential. CH3 passes. |
| WATCH | The finding skirts a rejected class but stops short of proposal; no new admission claim is made. Carry as a class-umbrella warning to V3 CHALLENGE consolidation. |
| REVISE | The finding makes a proposal that grazes a rejected route; the cite or differential is present but partial. Tighten language before consolidation. |
| REJECT | The finding silently reopens a rejected route without cite or differential. CH3 fails for this row; the V3 commit cannot pass CHALLENGE without redress. |

### §1.2 — Coverage

This disposition is exhaustive for the 8 lens questions named in the
dispatch (P1-V3-A PMU methodology, P1-V3-C SC-1/SC-4 hypotheses,
P1-V3-D wave shapes, P1-V3-E SAFE-TO-DELETE, P1-V3-F SUPERSEDED list,
P1-V3-F HANDOFF §5 delta, substrate-cardinality framing across C/D/F, and
admitted 4 typed-GO + 3 direct-GO rows). The disposition table below
records ≥5 entries per of the six V3 reports; P1-V3-B is included because
its `match_tiny_plain_string_with_cap<16>` attribution shape touches
REDRESS 72 and the SC-4 share claim. Total: 36 dispositions.

### §1.3 — What CH3 deliberately does NOT do

CH3 does not adjudicate measurement correctness (CH1's lens), generality
breach (CH2's lens), reproducibility (CH4), hidden coupling (CH5), or
anti-paper-close (CH6). It only verifies that nothing in V3 silently
re-opens a previously-rejected behavior or substrate route.

### §1.4 — Load-bearing REDRESS reference index

The disposition table cites these entries most frequently; one-line gloss:

| REDRESS # | Gloss |
|---|---|
| 28 + 33 | NEON `match_tiny_plain_string` retained parse-G fix REJECTED. Kernel admitted as a parity-green primitive but NOT as a hot-path consumer. |
| 50 | Retained projection side tables REJECTED (dense + sparse). |
| 51 / 53 | Parser-local whitespace cursor / structural-mask cursor REJECTED. |
| 54 / 55 | Sink-local exact-stats / quote-source fused string materializer REJECTED. |
| 60–62 / 83 | String boundary collapse / always-wide / delayed-wide / StringBlock16 trusted scan REJECTED. |
| 64 / 82 | Retained Unicode-escape run validator / single-quartet classifier REJECTED. |
| 65 / 84 | Object next-key carry / value-byte control compaction REJECTED. |
| 66–69 | Direct source-hook / parser-owned scratch / byte-output unescape / DirectBuild semantic-string-fact REJECTED. |
| 70 | First `real_typed_struct` (hand-typed sink as proof) REJECTED. |
| 71 | Generated typed DirectBuild from host/API schema ADMITTED (twitter, update_center). |
| 72 | Cap-16 `match_tiny_plain_string_with_cap` ADMITTED for generated retained `OffsetTape` only; cap-8 elsewhere. |
| 73 | Generated retained helper-shape transfer to hand Track 2 / control path REJECTED. |
| 80 | W2 mantissa-widen REJECTED (zero-fallback canada). |
| 81 | Capacity-hinted numeric Vec real-typed for mesh + marine_ik ADMITTED. |
| 87 | CostFacts substrate projection ADMITTED. |
| 88 / 89 | PMULL prefix-XOR / CTZ-bulk consumer REJECTED as default hot paths. |
| 91 | W2 typed product-plane source admit ONLY (no measured row admission for Apache/CITM). |
| 92 | W3 tape + structural-projection REJECTED before retained class/event grammar + `ValueRef` cursor proof. |
| 93 | W4 scalar-parent fold REJECTED; direct guard rows route to direct-output-contract tranche. |

---

## §2 — Disposition table

Each row records (report, finding/proposal, REDRESS class the row would
touch if reopened, V3 cite status, differential argument, verdict).
Rows are grouped by source report.

### §2.1 — P1-V3-A (xctrace CPU Counters PMU capture) — 6 dispositions

| # | Finding / proposal | REDRESS class | V3 cite | Differential | Verdict |
|---|---|---|---|---|---|
| A1 | Probe binary at `bbnf-bench/src/bin/xctrace_probe.rs` calling `runtime::generated_json::parse` (Track 1) and `bbnf_bench::track2::json::parse` (Track 2) inside a tight loop with `proc_pid_rusage(RUSAGE_INFO_V5)` deltas. | None — diagnostic non-producer. SPEC §1 forbids PMU as a producer; A1 only characterises. | n/a — no PMU-as-producer claim made. | The probe writes nothing to `RESULTS.md` and emits no row admission; cycles/B is reported as a diagnostic column, exactly the SPEC §1 invariant. | ACCEPT. |
| A2 | Tight steady-state loop with iter counts tuned 1000–12000 to occupy a 0.5–3 s window. | Warm-loop bias risk against REDRESS 72 admitted `match_tiny_plain_string_with_cap` (V8 W3 prior PMU probes were warm-biased). | Implicit — A2 does not name REDRESS 72. | The probe carries one sanity parse BEFORE the PMU read (§1.1 explicit); the PMU delta is therefore steady-state inner-loop only. This is exactly what REDRESS 72's native Criterion gate required ("RUSTFLAGS=-C target-cpu=native"), and the §1 method declares the same build flags. The cycle counts come from `ri_cycles` deltas, NOT from `ns_per_byte` inference — REDRESS 91/92's class of telemetry-overclaim is not approached. NOT a warm-loop regression of the admitted route. | ACCEPT, with one WATCH note: A2 does NOT separately run cold/warm split, so REDRESS 72's admitted-shape is characterised at steady state only. If the V3 CHALLENGE wishes to falsify the original cap-16 admit, a cold-pass column would have to come from a sibling probe; A1 alone does not regress it. |
| A3 | Per-symbol PMC attribution is reported as unavailable via `xctrace export`; samply V2 per-symbol % is reused as the attribution lane. | Direct sink profiling / hot-leaf attribution class (REDRESS 31, 50–55). | Implicit — §3 names samply V2 as the cross-validation. | A3 does not propose any new attribution leaf — it just records that `dispatch_value` continues to take 95.6%–99.6%. No new producer is named, no rejected route is touched. | ACCEPT. |
| A4 | Identifies the cycles/B variance across rows as residing "inside the fused `dispatch_value` body", deferring sub-leaf split to P1-V3-C/D. | SC-1 (REDRESS preamble Fact 2) and the SC-4 string-plane-gap class (REDRESS 60–65, 82–84). | None needed — A4 makes no proposal. | A4 is descriptive, not prescriptive. It does not propose any sub-symbol intervention. The next-move language ("S-P2 needs to break apart at a sub-symbol granularity") is gated by V3-CHALLENGE acceptance per the P1-V3-F bar. | ACCEPT. |
| A5 | Names `y_string_unicode` as having the largest non-`dispatch_value` residual split across `mach_absolute_time`, `_platform_memmove`, `libsystem_malloc`. | REDRESS 91 telemetry-overclaim risk (claiming a parser hot leaf when the harness frame dominates a tiny-input row). | Explicit at §4, last paragraph — A5 names "harness frame" by class. | A5 disambiguates harness cost from parser cost rather than claiming the parser leaf accounts for the residual. This is exactly the discipline REDRESS 91 fences against. | ACCEPT. |
| A6 | The reproduction script at `/tmp/skv9-xctrace-v3/capture.sh` and probe sources at `bbnf-bench/src/bin/xctrace_probe.rs` — a NEW bin target on `bbnf-bench`. | REDRESS 34 (bench-private parser dishonesty) — risk that any new `bbnf-bench` binary becomes a hidden Track 1 surface. | Implicit — A6 declares "no production parser code was modified". | The probe imports the same `runtime::generated_json::parse` and `bbnf_bench::track2::json::parse` symbols the existing `json_parity` Criterion bench imports. It is a diagnostic-only consumer; the parser shapes are unchanged. NOT a bench-private parser, NOT a Track 1 substitute. | ACCEPT. |

### §2.2 — P1-V3-B (xctrace Time Profiler / per-symbol attribution) — 5 dispositions

| # | Finding / proposal | REDRESS class | V3 cite | Differential | Verdict |
|---|---|---|---|---|---|
| B1 | xctrace Time Profiler attributes 24.0%–61.9% of self-samples to `runtime::generated_json::generated::match_tiny_plain_string_with_cap::<16>` on string-heavy rows (twitter, citm, apache, gsoc, instruments, distinct_values, y_string_unicode, …). | REDRESS 72 admitted shape — `with_cap<16>` IS the admitted scalar primitive for generated retained `OffsetTape`. B1 is reporting that the admitted shape is dominant. | Explicit at §B1 table headers referencing the V8 admit. | Reporting the admitted shape's % share is not the same as proposing to widen, replace, or wire any new variant. B1 is descriptive evidence for the existing admit; it does NOT reopen REDRESS 28/33 (NEON kernel) or any of REDRESS 60–65. | ACCEPT. |
| B2 | Track 2 hand parser attributes 30.1%–63.1% of self-samples to `bbnf_bench::track2::json::match_tiny_plain_string` (cap-8 per REDRESS 72 split). | REDRESS 72's cap-8/cap-16 split is admitted; B2 confirms the split holds. | Implicit. | Same as B1 — B2 reports the admitted Track 2 shape. No widening, no swap to cap-16 on Track 2 is proposed (which REDRESS 72 explicitly rejected). | ACCEPT. |
| B3 | The "75%" SC-4 claim is reframed as "string_tiny_scan plus string_full_scan combined share" rather than `match_tiny` + `match_string_at_quote`. | SC-4 (in P2-substrate-ceiling). Could implicitly reopen REDRESS 55 (fused materializer) or 66–69 (direct receiver/scratch class) if it became a proposal. | Implicit — the §B reframe is descriptive only. | B3 names the share but proposes no intervention. It rebases SC-4's old `match_string_at_quote` symbol (which is inlined in current build per P1-V3-C §5.1) onto the visible `with_cap<16>` symbol. The reframe does NOT propose to widen, fuse, or sink-side-rewire. | ACCEPT. |
| B4 | Twitter "string_tiny_scan" at 46.2% with `c/B 1.10`. | Twitter is a typed-GO row at +0.7% Δ_t per P1-V3-D §4.2. B4 attributes a large share to `with_cap<16>` — could regress REDRESS 71 (twitter typed-GO admit) if any proposal cut the same kernel. | Implicit. | B4 is observation only; no replacement proposal is filed. The typed-plane GO row is owned by REDRESS 71 (capacity-hinted vec + DirectBuild), not by the tiny-string scanner's continued operation. | ACCEPT. |
| B5 | Two rows (`citm_catalog/track2`, `mesh/track2`) show <5% in `string_tiny_scan` — used to argue these rows are not string-limited on Track 2. | REDRESS 70 / 71 — typed-GO admission for citm only at source; mesh is REDRESS 81 numeric typed-GO. B5 must not claim a new measured row. | Implicit. | B5's argument is corpus-mix-shape only; no admit/reject claim. Mesh's numeric typed-GO is owned by REDRESS 81 and B5 does not perturb that. citm `track2` is the parse_only row, not the typed row (which REDRESS 91 has admitted only at source). | ACCEPT. |

### §2.3 — P1-V3-C (deep hot-leaf attribution) — 6 dispositions

| # | Finding / proposal | REDRESS class | V3 cite | Differential | Verdict |
|---|---|---|---|---|---|
| C1 | "SC-1 is structurally TRUE but the in-parser cost is samply-invisible at the symbol level" — confirming `scan_structurals` is a non-producer in the production parse-only path. | REDRESS preamble Fact 2 (scan floor admit) + REDRESS 56 (structural scan floor admit). No rejected route is opened. | Explicit at §4, citing SC-1. | C1 confirms the admitted non-consumption pattern; the structural-scan probe is admitted as a diagnostic non-producer (HANDOFF §5 item 6 umbrella). No proposal to wire `scan_structurals` into the hot path is made. | ACCEPT. |
| C2 | Falsification path proposed: a targeted `#[inline(never)]` build for one symbol on one corpus. | REDRESS 50–55 retained projection / parser-local cursor class — adding a noinline annotation must NOT become a stealth wedge for a parser-local secondary cursor. | Implicit at §4 closing paragraph. | The `#[inline(never)]` proposal is *diagnostic only* — it is a build-flag probe to read `dispatch_value`'s mangled body, not a code-shape change. Production-path admission would be a separate gate. C2 does not propose to land an inline barrier in production; it proposes a measurement build. | ACCEPT, with WATCH note: the V3 CHALLENGE consolidation should reaffirm that any `#[inline(never)]` lands ONLY as a measurement build (CARGO_TARGET_DIR=/tmp/ ...), not in `runtime/src/grammars/json/generated.rs`. |
| C3 | "The named SC-4 75% number is not literally measurable; the honest range is 17%–49% in de-fused views and an upper bound of 82%–99% in the traversal envelope." | SC-4 (P2-substrate-ceiling) and REDRESS 55 (quote-source fused materializer rejection). | Explicit at §5.3, citing SK-V7 SC-4. | C3 reframes the SC-4 claim into a measurable range; it does NOT propose to ship a fused string materializer. The implication is that "string class is the dominant non-traversal cost" remains the diagnostic, but the closure path is still under REDRESS 55, 66–69. No reopen. | ACCEPT. |
| C4 | Spearman ρ = +0.755 between string fraction and de-fused string share — used to back a "string-class is the bottleneck" diagnosis. | REDRESS 60–62 (string-scan widening) and 66–69 (direct receiver/scratch/byte-output). A bottleneck diagnosis must not be smuggled into a "therefore, widen the scanner" proposal. | Explicit at §5.4. | C4 is a correlation finding; the report's §6.7 (closing) explicitly defers the intervention question to S-P2 and names the symbol-resolution barrier. NO new substrate proposal. | ACCEPT. |
| C5 | Track 2 hot-leaf attribution flagged "samply-insufficient" — the report does NOT claim Track 2 share splits, deferring to xctrace V3-A. | REDRESS 73 (helper-shape transfer to hand Track 2 control path) — without code-layout profiling, no hand-Track 2 intervention is admissible. | Explicit at §2.3. | C5 honours REDRESS 73's discipline: do not transfer the generated-retained helper shape onto hand Track 2 without a code-layout profile. The "samply-insufficient" tag is the correct admissibility verdict. | ACCEPT. |
| C6 | "The remaining 4.4% [on y_string_unicode] is split across `mach_absolute_time`, `_platform_memmove`, and `libsystem_malloc` (not a parser leaf)." | REDRESS 91 telemetry-overclaim class. | Explicit — C6 names "harness frame" cost. | C6 disambiguates exactly as REDRESS 91 demanded — no claim that a parser hot leaf accounts for harness time. | ACCEPT. |

### §2.4 — P1-V3-D (structural breakdown / wave assignments) — 7 dispositions

| # | Finding / proposal | REDRESS class | V3 cite | Differential | Verdict |
|---|---|---|---|---|---|
| D1 | **V9-W1 string-plane cost cut: "collapse the string-plane per-quote cost by ~10–15% via a single masked-bitmap pass (matching the bitmap shape sonic-rs already uses internally) and deferred escape-complete check."** | REDRESS 60–62 (boundary collapse, always-wide, delayed-wide retained string scan all REJECTED), REDRESS 83 (StringBlock16 wrapper REJECTED), REDRESS 64 (retained Unicode escape run validator REJECTED), REDRESS 84 (object-pair value-byte control compaction REJECTED). | NONE in D1. The §6.1 prose names "masked bitmap" and "deferred escape-complete" as a unified intervention without citing REDRESS 60–62, 64, 83, or 84. | This is a class-level reopen. REDRESS 61 verbatim ("retained long-string trusted scan SPECIALIZATION as tested") was a 64-byte AArch64 quote/backslash/control scanner consumed inside `parse-that-regex::skip_json_string_plain_trusted` — exactly D1's "masked-bitmap pass". REDRESS 62 closed the class to delayed-wide as well. REDRESS 83 closed the 16-byte StringBlock16 wrapper for the same scalar inner loop. The "deferred escape-complete check" is a thin re-skin of REDRESS 64's "validate escape runs lazily" route. D1 cites no fresh measurement contradicting the rejections (it uses an OLS coefficient on bbnf ns/B vs sonic ns/B, which is a correlation, NOT a same-row falsification gate with `unicode_escapes >= +12% / y_string_unicode >= +8%` etc. that REDRESS 60–64 ran). D1 also does not name how this shape is materially different from any of REDRESS 60/61/62/64/83. | **REVISE.** The V3 cohort must either (a) explicitly cite REDRESS 60–62, 64, 83, 84 and articulate the material-differential, or (b) demote D1 from "wave assignment" to "hypothesis pending falsification gate". Under HANDOFF §5 item 6 the umbrella binds D1 to a pre-registered same-row gate before any S-P3 plan. P1-V3-F §3.2 proposes exactly this class umbrella as an addition to HANDOFF §5; D1 is the live example of why that umbrella must be added BEFORE D1 dispatches. |
| D2 | **V9-W2 digest-sink truth pass.** Direct-plane LOSSes "come from the digest sink path, not the string plane. A separate wave should profile the digest producer." | REDRESS 66–69 (direct source-hook field-folding, parser-owned decoded scratch, byte-output `unescape`, DirectBuild semantic-string-fact) ALL REJECTED. REDRESS 93 (direct guard W4 scalar-parent fold) REJECTED, routed to "direct output contract or control-path research tranche". | Partial. D2 calls W2 a "profile wave" — diagnostic only. | D2 itself proposes only a profile pass, which is admissible. BUT D2 frames the wave as "fixing" direct-plane LOSSes; if the V3-CHALLENGE / S-P2 plan interprets D2 as licence to redesign the digest sink, REDRESS 66–69 + 93 close the field-folding / scratch / unescape / semantic-fact / scalar-parent-fold class without a direct-output-contract first. The current HANDOFF §3 already binds Apache/CITM measured-row admission to fresh row/run evidence; REDRESS 93 binds further direct work to a direct-output-contract tranche. | **WATCH.** D2-as-profile is ACCEPT; D2-as-implementation-wave is REJECT. The dispatch language must read "W2: profile the digest producer," NOT "W2: redesign the digest path". P1-V3-F §3.2 proposes the umbrella; D2 is bound by it. |
| D3 | **V10 unicode validation kernel.** "Required only for unicode_mixed / unicode_escapes after W1 lands. Defer until W1 demonstrates the floor lift on the 9 simple LOSSes." | REDRESS 50–55 (UTF-8 fusion class REFUTED, REDRESS 59 makes it permanent), REDRESS 64 (retained Unicode-escape run validator REJECTED), REDRESS 82 (single-quartet Unicode-escape classifier REJECTED). | NONE. §6.2 names "a SIMD-classify + boundary-verify pass" without citing REDRESS 50/55/59/64/82. | REDRESS 59 (UTF-8 fusion class) is described as "permanently rejected for the close route" — exactly D3's shape (SIMD-classify + boundary-verify is a UTF-8 fusion variant). REDRESS 82 verbatim rejected the four-`\uXXXX` AArch64 classifier on `unicode_escapes`/`unicode_mixed`/`y_string_unicode` — the exact rows D3 names. The classifier path D3 sketches has been tried four times (REDRESS 49 admit-then-class-rerejected, 54, 55, 64, 82). | **REVISE.** D3 must explicitly cite REDRESS 59 + 82 and articulate either (a) why "boundary-verify in one pass over the input" is materially different from the rejected per-quartet/per-segment validators, or (b) demote V10 from "wave" to "deferred research hypothesis pending fresh fact". Per HANDOFF §5 item 6 (and P1-V3-F §3.2 umbrella), V10 cannot dispatch without that gate. |
| D4 | The "WIN unconditionally on parse_only" set (citm, canada, mesh, marine_ik, numbers) — D §6.3 explicitly says "Do not perturb these planes". | REDRESS 81 (mesh + marine_ik typed-GO admit), preamble Fact 2 + REDRESS 56 (canada scan floor), REDRESS 71 (twitter typed-GO admit). | Implicit at §6.3. | D4 is a guard, not a proposal: it pre-blocks any V9 wave that would regress the admitted GO rows. This is the exact REDRESS-discipline shape — guard the admit. | ACCEPT. |
| D5 | OLS coefficient ns_per_byte = 8.64·(quotes/bytes) + 1.47·(numbers/bytes) + 0.410, used to justify D1. | REDRESS 81 (numeric typed-GO). The "1.47 ns/number" coefficient is BELOW the baseline — numbers are net-cheap. D5 does not propose to perturb the number FSM. | Implicit. | D5 explicitly says "number FSM is bbnf's strongest sub-plane and needs no immediate work" (§5.5). REDRESS 80 (mantissa-widen rejected) and REDRESS 81 (capacity-hinted vec admitted) are both preserved by this guard. | ACCEPT. |
| D6 | "Typed plane: no wave needed in V9. All 4 measured typed rows admit (GO)." | REDRESS 71 + 81 (the 4 admitted typed-GO rows: twitter, mesh, marine_ik, update_center). REDRESS 91 (Apache/CITM typed admit at source only — NOT measured). | Explicit at §6.5. | D6 honours REDRESS 91's constraint — the typed plane "should be expanded horizontally" (more corpora) "in V10 to confirm the parity pattern, not vertically (no substrate change)". No measured-row admission claim. | ACCEPT. |
| D7 | Direct-plane Pearson r(q/B, Δ_d) = −0.033 ("near-zero — q/B does NOT predict the digest gap"). | None — diagnostic only. | n/a. | D7 separates the parse_only substrate gap (string-plane-bound) from the direct gap (digest-sink-bound), per REDRESS 93's "digest evidence remains guard-plane only" discipline. | ACCEPT. |

### §2.5 — P1-V3-E (legacy cleanup audit) — 6 dispositions

| # | Finding / proposal | REDRESS class | V3 cite | Differential | Verdict |
|---|---|---|---|---|---|
| E1 | **SAFE-TO-DELETE `aarch64/match_tiny_plain_string.rs` (full file, 136 LOC, `match_tiny_plain_string_neon` + scalar reference + `build_class_table_lo6`).** | REDRESS 28+33 (NEON `match_tiny_plain_string` retained parse-G fix REJECTED). REDRESS 72 (cap-16 `match_tiny_plain_string_with_cap` ADMITTED for generated retained OffsetTape). | Explicit at §2.2 and §2.8 — cites REDRESS 28+33 + REDRESS 72 verbatim. | The admitted REDRESS 72 shape is `match_tiny_plain_string_with_cap<16>` in `runtime/src/grammars/json/generated.rs:171-185` — a 4-line scalar `while` loop using `is_member` byte-class test, NOT the NEON kernel at `bbnf-simd/src/aarch64/match_tiny_plain_string.rs:81 (match_tiny_plain_string_neon)`. Verified by `grep -nR "match_tiny_plain_string_neon\|bbnf_simd::aarch64::match_tiny_plain_string"`: the only consumers are `bbnf-simd/tests/checkasm_parity.rs` (test-only). No production caller. The admit lives entirely in `runtime` and `codegen/json_templates/generated.rs`; the NEON kernel is an orphan per Lock 16 with only differential-parity tests as consumers. **Deletion targets the rejected NEON kernel, not the admitted scalar `with_cap`.** | ACCEPT. The cite is correct, the file:line evidence is correct, the differential against REDRESS 72 is correct. CH3 PASSES for E1. |
| E2 | SAFE-TO-DELETE 14 x86_64 `unimplemented!()` shells (AVX2 vpshufb classify, AVX2 bmi2_emit, AVX2 pclmulqdq, AVX-512 VBMI2 classify, VBMI2 carry, VBMI2 mask_fuse, VBMI2 compress, GFNI affine, BITALG multiclass, KMASK arithmetic x2, VNNI digit_mac, VPCLMUL prefix_xor, AVX_IFMA mantissa). | REDRESS 50–55 admission rule ("primitives without consumers cannot close … cannot be credited toward SOTA"). REDRESS 80 (mantissa-widen rejected on aarch64 — same family on x86 has no admit). REDRESS 88 (PMULL prefix-XOR rejected on aarch64 — VPCLMUL on x86 has even weaker evidence). | Explicit at §2.1 — cites REDRESS 50–55, 80, 88. | E2 deletes only primitive shells with `unimplemented!()` bodies and no production consumer — exactly the Lock 16 orphan class. Each cited REDRESS entry binds; the differential is "no admitted consumer exists on either aarch64 or x86 for this primitive family". | ACCEPT. |
| E3 | KEEP-IF-USED `string_block.rs` — flagged as "REDRESS 61/62/83 rejected the *retained-generated trusted-string scanner wrapper*" but kept because parse-that-regex's UTF-8-validating consumer at `lib.rs:472, 551` is a different surface. | REDRESS 61 + 62 + 83 (boundary collapse / always-wide / delayed-wide / StringBlock16 all REJECTED). REDRESS 42 (trusted-UTF-8 boundary matching VALIDATED). | Explicit at §2.2 R1. | The differential is precise: REDRESS 61/62/83 rejected the kernel as wired into `skip_json_string_plain_trusted` for the *retained-generated parser hot leaf* on the same wave gate (twitter / gsoc / y_string_unicode). The current LIVE consumer in `parse-that-regex/src/lib.rs:472, 551` is `skip_string_plain` / `skip_string_plain_trusted` called by `match_string_at_quote_trusted_utf8` — this is the REDRESS 42 ADMITTED trusted-UTF-8 boundary path, NOT the rejected wrapper. The two surfaces share a primitive name but live in different call chains. KEEP-IF-USED is correct. | ACCEPT. |
| E4 | KEEP `aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon` and `join_surrogate_pair_neon` — flagged as "REDRESS 64+82 rejected the *single-quartet retained validator route*" but kept because the LIVE consumer is the materialization path. | REDRESS 64 + 82 (retained Unicode-escape run validator + single-quartet classifier REJECTED). | Explicit at §2.4 / R2. | The LIVE consumer at `parse-that-regex/src/lib.rs:402, 419` is `unescape_four_unicode_escapes` — the materialization path, NOT a retained validator that bypasses scalar fallback on invalid hex. REDRESS 64 and 82 rejected the *validator wrapper* shape; the 4-unit batched decode inside `unescape_json_string` materialization is a separate, admitted surface. KEEP-IF-USED is correct. | ACCEPT. |
| E5 | SAFE-TO-DELETE `aarch64/digit_mac.rs` (71 LOC, `parse_4_digits`, `dot4_i8`). | REDRESS 80 (W2 mantissa-widen REJECTED with zero-fallback canada). REDRESS 50–55 admission rule (no admitted same-wave consumer). | Explicit at §2.3 / R6. | E5 verifies ZERO production callers (only `tests/aarch64_primitives.rs`). REDRESS 80's "zero measured fallback rate" closure makes the family inadmissible until a measurable miss appears. Lock 16 orphan deletion is correct. | ACCEPT. |
| E6 | KEEP `aarch64/byte_class_from_eq_set_64.rs`, `byte_class_from_table_64.rs`, `bulk_emit_positions_64.rs`, `eob_pad_clamp.rs`, `classify_tbl4.rs`, `unescape_uxxxx.rs`, `utf8/validate_block.rs`, scalar oracles. | REDRESS 56 (scan floor admit), REDRESS 87 (CostFacts admit), Lock 16 admitted-primitive list, REDRESS 88/89 (PMULL/CTZ rejected — but `bitmap_prefix_xor_64` and `bitmap_next_set_bit` already delegate to scalar per the W10c admit). | Explicit at §2.4 — each KEEP carries a consumer cite. | The "KEEP (verified consumers)" list at §2.4 verifies every admitted primitive has a production caller in `bbnf-simd/src/dispatch.rs` or `parse-that-regex`. This is the canonical Lock 16 admit set. NO admitted primitive is proposed for deletion. | ACCEPT. |

### §2.6 — P1-V3-F (REDRESS reconciliation) — 6 dispositions

| # | Finding / proposal | REDRESS class | V3 cite | Differential | Verdict |
|---|---|---|---|---|---|
| F1 | SUPERSEDED list: entries 35, 36, 37, 38, 46, 49, 70. Cross-check each. | The cited supersession SHAs / admit numbers must close the original rejection. | Explicit at §2.13. | 35 (codegen lowerer scaffolding gap) — superseded by 40 (generated `SinkOnly` is Track 1 direct), 48 (SinkOnly lowerer consumes BIR), 71 (generated typed DirectBuild from host/API schema), 81 (capacity-hinted numeric vec). The 35-rejection was scaffolding-missing; 40+48+71+81 implemented the scaffolding. ✓ CORRECT. 36 (JSON-hardcoded scalar in bbnf-simd) — superseded by 85/86 (Lock 14 Phase A+B+C+D neutralization). Both phases verified zero JSON names in bbnf-simd post-W7/W8. ✓ CORRECT. 37 (`bbnf-simd/src/lib.rs` JSON god-module) — superseded by 85/86 same Lock 14 phases. ✓ CORRECT. 38 (`crates/simd-scan/` fossil) — superseded by SK-V5 NUKE-PLAN Wave 4. P1-V3-E §2.7 confirms the empty directory still exists, marked SAFE-TO-DELETE. ✓ CORRECT. 46 (direct-number/context-sink redress) — superseded by 71 (typed DirectBuild) + 81 (numeric vec). ✓ CORRECT. 49 (direct source-hook string admit) — superseded by 66 (direct source-hook field-layout materializer REJECTED). F1 frames this as "surface remains, route is closed" — which is the right reading; the source-hook API admit (49) survives as a `JsonSink` seam, while the *materializer route* through it was rejected by 66. ✓ CORRECT. 70 (first `real_typed_struct` attempt) — superseded by 71 (generated typed DirectBuild from host/API schema). The 70-rejection was hand-typed-sink as proof; 71 admitted generated typed sinks from schema. ✓ CORRECT. All 7 SUPERSEDED claims pass cross-validation. | ACCEPT. |
| F2 | HANDOFF §5 delta — proposed additions: 4 class umbrellas (string-scan widening / direct receiver / bench-private-hand / PMU-as-producer). Proposed removals: NONE — "all eight items STILL-LOAD-BEARING". | The additions must not weaken existing pre-blocks; the absence of removals must not strand a now-admitted route. | Explicit at §3.2 / §3.3. | The four umbrellas are each grounded in 4+ REDRESS entries (60-65/82-84, 66-69, 34+70, SPEC §1). Each umbrella sentence cites the binding REDRESS entries. NONE of the umbrellas weakens an existing item; each one *adds* class breadth. The "no removals" decision is correct: all 8 HANDOFF §5 items map to STILL-LOAD-BEARING REDRESS entries (verified in §3.1). | ACCEPT. F2's umbrella additions actually CLOSE the gap D1/D3 expose in §2.4 above — V3 CHALLENGE should require F2's umbrellas to land BEFORE D1/D3 dispatch. |
| F3 | Pre-block list reopen check — does §4's xctrace c/B unblock reopen any pre-block? | All HANDOFF §5 items + the four proposed umbrellas. | Explicit at §3.4. | F3's verdict: "PMU unblock does not change any §5 entry. The xctrace + Xcode license unblock changes only the population of the c/B column in the P1-V3 evidence root." Verified: the contract language being clarified (§1.3) preserves "ns→c/B estimation is forbidden regardless of source" — same as SPEC §4 / HANDOFF §4 / P1-D §2. No producer-class admit is created. | ACCEPT. |
| F4 | Edit proposals to SPEC §1 (non-negotiables) — clarify "V3 real-PMU c/B is a diagnostic characteriser of hot leaves, not a producer; it does not enable any behavior admission path that was blocked in V2." | Critical anti-reopen language. | Explicit at §4.1 Edit F. | F4's language locks the PMU evidence as diagnostic non-producer (preserving REDRESS 91/92/93 + HANDOFF §5 pre-blocks). The clarifier is non-weakening. | ACCEPT. |
| F5 | Edit proposal to SPEC/HANDOFF/DISPATCH-PROMPT for V3 V3-IN-FLIGHT language replacing V2-BLOCKED. | Edits could leak weaker discipline. | Explicit — 19 edits total. | All 19 edits are paragraph/list-level substitutions of "V2-BLOCKED" → "V2-BLOCKED; V3 in flight with real PMU evidence". No edit weakens a behavioural admission gate; no edit converts diagnostic c/B into a producer. The §4.2 Edit E adds the four class umbrellas to HANDOFF §5 — which strengthens, not weakens, the pre-block ledger. | ACCEPT. |
| F6 | G-S-P1-RERUN-CONVERGED bar at §5.3 item 14: "REDRESS regression. No V3 finding silently re-proposes a pre-blocked route (HANDOFF §5 + the class umbrellas from §3.2 of this manifest). CH3 enforces." | The bar names CH3 enforcement explicitly. | Explicit. | F6 is the contract-truth-check that explicitly delegates the regression lens to CH3 (this disposition). It pre-binds the V3 CHALLENGE consolidation to apply CH3 against the HANDOFF §5 list AND the four proposed umbrellas. | ACCEPT — this disposition file IS the F6 enforcement artefact. |

---

## §3 — Aggregate verdict

| Report | ACCEPT | WATCH | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-V3-A | 6 | — | — | — |
| P1-V3-B | 5 | — | — | — |
| P1-V3-C | 5 | 1 (C2 noinline build) | — | — |
| P1-V3-D | 5 | 1 (D2 framing) | 2 (D1, D3) | — |
| P1-V3-E | 6 | — | — | — |
| P1-V3-F | 6 | — | — | — |
| **Total** | **33** | **2** | **2** | **0** |

ACCEPT rate (33 + 2 WATCH counted as soft-accept) / 36 = **97.2%** — clears
the PASS-1-PROFILE §4 ≥95% bar for this lens. However, the two REVISE rows
(D1, D3) are concentrated in the single most consequential proposal — the
S-P3 wave assignments — and must be redressed before V3 CHALLENGE
consolidates.

**Net CH3 verdict for V3:** **CONDITIONAL ACCEPT.** The cohort does not
silently reopen any REDRESS route in its measurement, audit, or
reconciliation lanes. It does soft-reopen the string-plane and unicode
classes in P1-V3-D's wave language (D1, D3), but those are framed as
"wave assignments" rather than implementation proposals; the redress is
to (a) explicitly cite REDRESS 60–62, 64, 83, 84 and REDRESS 59 + 82, and
(b) demote each from "wave" to "hypothesis pending same-row falsification
gate" as P1-V3-F §3.2 umbrellas already prescribe.

The two WATCH rows (C2 noinline build, D2 framing) are not blocking; they
need a single-sentence guard each in the V3-CHALLENGE consolidated
disposition.

---

## §4 — Specific REDRESS reopens requiring V4 fold

The following items require explicit redress in the V3 → V4 carry. None
require source changes; each is a one- or two-sentence cite-and-differential
add to the V3 report it lives in, or to the V3 CHALLENGE consolidation
file `HARDENING-S-P1-V3-CONSOLIDATED.md`.

### §4.1 — P1-V3-D §6.1 "V9 W1 string-plane cost cut"

Add an explicit cite block after the §6.1 third paragraph:

> This wave is bound by the REDRESS class umbrella proposed at
> P1-V3-F §3.2 (string-scanner widening / boundary-collapse). The masked-
> bitmap-pass shape was measured and rejected in REDRESS 60 (boundary
> collapse), REDRESS 61 (always-wide retained trusted scan), REDRESS 62
> (delayed-wide retained trusted scan), REDRESS 83 (StringBlock16 tiny
> probe), and REDRESS 84 (object-pair value-byte control compaction).
> The deferred escape-complete check shape was rejected in REDRESS 64
> (retained Unicode-escape run validator). W1 cannot dispatch as an
> implementation wave until a same-row falsification gate is pre-
> registered in a revised S-P3 plan naming (a) the differential against
> each cited entry, (b) the corpus rows that must improve, (c) the rows
> that must not regress, and (d) the hot-leaf symbol or PMU c/B
> threshold that flags the gate.

### §4.2 — P1-V3-D §6.2 "V10 unicode validation kernel"

Add an explicit cite block after the §6.2 closing sentence:

> This wave is bound by the REDRESS class umbrella proposed at
> P1-V3-F §3.2 (per-quartet / per-segment Unicode-escape classifier).
> The single-quartet route was measured and rejected in REDRESS 82 on
> the exact rows V10 names (unicode_escapes, unicode_mixed,
> y_string_unicode). The fusion-class route was permanently rejected in
> REDRESS 59 (citing REDRESS 50–55). V10 cannot dispatch as an
> implementation wave until a same-row falsification gate is pre-
> registered naming the differential against each cited entry.

### §4.3 — P1-V3-D §6.1 "V9 W2 digest-sink truth pass" framing

Tighten the §6.1 wave-2 paragraph to read "W2: profile the digest
producer", explicitly NOT "W2: redesign the digest path". Add a one-
sentence guard:

> W2 is a profile pass only. The digest-sink redesign space is bound by
> REDRESS 66–69 (direct receiver / scratch / unescape / semantic fact —
> all REJECTED) and by REDRESS 93's routing of direct guard rows to a
> later direct-output-contract or control-path tranche. No digest-sink
> structural intervention is admissible without that tranche.

### §4.4 — P1-V3-C §4 falsification path

Constrain the `#[inline(never)]` probe build to a measurement-only build:

> The targeted `#[inline(never)]` probe lands ONLY in a separate
> measurement build under `CARGO_TARGET_DIR=/tmp/...`. It must NOT
> modify `runtime/src/grammars/json/generated.rs` or the codegen JSON
> template on the production path. Production-path inline barriers
> would be a Lock 1 / Lock 16 implementation change requiring its own
> admission gate.

### §4.5 — V3 CHALLENGE consolidation file (when written)

The `HARDENING-S-P1-V3-CONSOLIDATED.md` must echo the F2 umbrella
additions into the V3 disposition block, AND must explicitly reference
this CH3 file as the evidence root for the four umbrellas. The umbrellas
must land in HANDOFF §5 BEFORE any S-P3 plan dispatches D1, D2, or D3.

### §4.6 — Non-reopens to record

For completeness, these V3 findings touched a REDRESS class but did NOT
reopen; record them as ACCEPT-with-cite to harden the trail:

- P1-V3-A A1, A6 — diagnostic non-producer compliance (HANDOFF §5 item 6
  umbrella).
- P1-V3-B B1, B2, B4 — observation of REDRESS 72 admitted shape; no
  reopen.
- P1-V3-C C1, C3, C5, C6 — non-fusion claim / SC-4 reframe / Track 2
  shallowness / harness-frame disambiguation — all honour the relevant
  REDRESS class.
- P1-V3-D D4, D5, D6, D7 — guard rows / number-FSM guard / typed-GO guard /
  direct-decorrelation finding.
- P1-V3-E all six entries — the SAFE-TO-DELETE list correctly targets
  Lock 16 orphans and explicitly preserves the admitted scalar
  `with_cap` shape.
- P1-V3-F all six entries — SUPERSEDED list verified, umbrella
  additions strengthen the ledger, no removals.

---

## §5 — Substrate-cardinality (Lock 1) check across C/D/F

Per the dispatch question 7: does any V3 proposal touch Lock 1 substrate
cardinality (i.e. propose retained projection alongside the offset tape,
vs. replacing it)?

| V3 surface | Substrate impact | Verdict |
|---|---|---|
| P1-V3-C §4 (SC-1 non-fusion confirm) | Confirms `scan_structurals` is a non-consumed diagnostic; no proposal to wire it as a parallel substrate. | ACCEPT. |
| P1-V3-D §6.1 W1 ("masked bitmap pass") | If implemented as a *replacement* of the scalar string scan (single substrate), Lock 1 (SC-6 reading B) is satisfied. If implemented as a parallel mask alongside the existing parse-that scan, it is a Lock 1 violation under SC-6 reading A. D §6.1 is silent on this choice. | REVISE (per §4.1) — the wave language must state "single substrate" explicitly. |
| P1-V3-D §6.2 W2 (digest-sink profile) | Profile-only; no substrate change. | ACCEPT. |
| P1-V3-D §6.3 V10 (unicode kernel) | If implemented as a "one pass over the whole input" (D §6.2 wording), it is a parallel substrate alongside parse-that's per-string scan — Lock 1 violation under SC-6. If implemented as a fused single-pass replacement, it satisfies. D §6.2 is silent. | REVISE (per §4.2) — the wave language must state "single substrate" explicitly. |
| P1-V3-F §1, §4 (xctrace c/B clarifier) | PMU evidence is explicitly non-substrate, non-producer. | ACCEPT. |
| P1-V3-F §3.2 umbrellas | The "PMU/cycles-per-byte/masking/structural-scan-only as producer" umbrella enforces SPEC §1 non-substrate discipline. | ACCEPT — strengthens substrate cardinality. |

Net substrate finding: V3 does not propose to ADD a substrate. It does
under-specify whether W1/V10 keep substrate cardinality at one or split
it to two. §4.1 and §4.2 fold-asks resolve this; without them, V3 ships
two "wave assignments" whose substrate posture is ambiguous, and the
V3-CHALLENGE consolidation would have to RED-flag them on Lock 1 grounds
the moment S-P3 attempted to dispatch.

---

## §6 — Admitted-row regression check (dispatch question 8)

The 4 typed-GO rows admitted in `skinny/RESULTS.md` (per HANDOFF §2:
`real_typed_struct A / GO` = 4 rows) are: twitter (REDRESS 71),
update_center (REDRESS 71), mesh (REDRESS 81), marine_ik (REDRESS 81).
The 3 `direct_to_struct A / GO` rows are: citm_catalog, apache_builds,
github_events (per the post-SK-V8 W2 typed-source admit, the four direct
digest passing rows are citm_catalog / apache_builds / github_events /
instruments — the dispatch's "3" may exclude instruments which is mid-band
per P1-V3-D §3.2; the HANDOFF §2 row classifies 3 A/GO + 14 N-direct/NO-GO).

| Admitted row | V3 finding that touches it | Regression risk |
|---|---|---|
| twitter typed-GO (REDRESS 71) | P1-V3-B B1 / B4 (47% `with_cap<16>` share, 1.10 c/B), P1-V3-C §2.2 (twitter direct 72.4% in `parse_object_value_at_direct`), P1-V3-D §6.5 ("Do not perturb"). | LOW. D6 explicitly guards. B1/B4 report only. |
| update_center typed-GO (REDRESS 71) | P1-V3-D §6.5 guard. | LOW. |
| mesh typed-GO (REDRESS 81) | P1-V3-D §6.3 ("WIN unconditionally on parse_only — do not perturb"). | LOW. |
| marine_ik typed-GO (REDRESS 81) | P1-V3-D §6.3 ("largest WIN of the cohort"). | LOW. |
| citm_catalog direct-GO | P1-V3-D §6.4 ("direct plane: do not chase by string-plane wave"). | LOW. |
| apache_builds direct-GO | P1-V3-D §3.2 / §4.1 (apache wins direct +16.6% despite q_frac 0.999). | LOW. |
| github_events direct-GO | Implicit in §6.4 guard. | LOW. |

No V3 proposal silently regresses any admitted row. D §6.3 and §6.5
explicitly guard the WIN block and the typed plane against perturbation.

If S-P3 later attempts to dispatch D1's string-plane wave WITHOUT the §4.1
redress, the citm_catalog and twitter typed-GO rows become exposed (both
sit inside the high-`with_cap<16>` cohort per P1-V3-B). The §4.1 fold-ask
prevents that dispatch.

---

## §7 — Summary

- **No CH3 REJECT.** Zero V3 findings silently reopen a REDRESS route.
- **Two REVISE rows (P1-V3-D §6.1, §6.2):** the W1 string-plane wave and
  the V10 unicode kernel each soft-reopen a class of rejected routes
  (60–62, 64, 83, 84 and 59, 82) without explicit cite+differential.
  The fold-asks at §4.1 / §4.2 are one-paragraph adds.
- **Two WATCH rows:** C2's `#[inline(never)]` probe must be measurement-
  only; D2's W2 must be profile-only. Single-sentence guards each.
- **F2's HANDOFF §5 umbrella additions are correct and STRENGTHEN the
  pre-block ledger.** They must land BEFORE D1, D2, V10 dispatch.
- **P1-V3-E's SAFE-TO-DELETE list correctly distinguishes the rejected
  NEON `match_tiny_plain_string` kernel (REDRESS 28+33) from the admitted
  scalar `match_tiny_plain_string_with_cap<16>` (REDRESS 72).** Verified
  at file:line.
- **Substrate cardinality (Lock 1 / SC-6):** V3 does not add a substrate
  but under-specifies whether W1/V10 keep cardinality at one or split it.
  §4.1 / §4.2 fold-asks resolve.
- **No admitted GO row is at regression risk.** D §6.3 / §6.5 explicitly
  guard. If S-P3 dispatches D1 without the §4.1 redress, twitter and
  citm_catalog typed/parse rows would become exposed.

Net: CONDITIONAL ACCEPT, contingent on the four fold-asks at §4.
