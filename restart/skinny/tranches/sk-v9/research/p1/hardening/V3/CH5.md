# CH5 — HIDDEN COUPLING disposition for SK-V9 S-P1 V3 cohort

Pass: S-P1 Profile. Cycle: V3. Lens: CH5 HIDDEN COUPLING.
Date: 2026-05-18.
Subjects: `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`.
Output: this file.

CH5 verifies, per `restart/prompts/ORCHESTRATOR.md` §3W: no proposal
introduces a parallel substrate, a sidecar producer, a renamed-scanner
Lock-1 violation, or Track 1 ≡ Track 2 dishonesty; the substrate union
holds. The non-negotiables enforced are `LOCKS.md` Lock 1 ("Tape is the
substrate, properly unioned with direct-to-struct; … orthogonal codepaths
and parallel substrates are dead … A SIMD mask stream is a transient
producer, not a retained sidecar; if structural offsets are retained, the
structural projection IS the tape.") and Lock 14 (substrate carries ZERO
grammar-specific code), against the ledger of `skinny/REDRESS.md` 50–72
sidecar/projection rejections plus the SK-V8 W3 union rejection at
REDRESS 92.

## §1 — Method (Lock-1 cardinality audit protocol)

For each report I grep the proposal surface for three signatures: (a)
*production-callable surfaces* that the report introduces or names as
load-bearing — these go on the substrate-cardinality ledger; (b)
*adjacency-to-substrate language* — "alongside", "in parallel", "sidecar
producer", "retained side-table" — these are the Lock-1 explicit-fault
strings; (c) *replacement-vs-addition disposition* — the report must say
of every new write-path whether it *replaces* the prior write-path
(singular substrate preserved) or runs *alongside* it (parallel substrate
fault). The audit is monotone: any unsourced ambiguity defaults to
PARALLEL-SUBSTRATE unless the report can be read as singular without
strain. Per SC-6 §1.1 the production cardinality test is "if structural
offsets are retained, the structural projection IS the tape"; the
adversarial reading is whether each proposal preserves that *equality*
rather than installing a second producer.

The probe binary at `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`
(P1-V3-A §1.1 and P1-V3-B §1.2) is audited specifically against (a)
sidecar-producer status — does the binary write any new artefact that
later code paths consume as substrate, or does it strictly *read* the
existing parse surface? — and (b) Track 1 ≡ Track 2 cardinality — does
the dual-track invocation collapse Track 1 ≡ Track 2 (which would mean
the oracle is being measured as a SOTA gate, the precise dishonesty CH5
rejects)?

The W3 union substrate is the SK-V8 SC-3/SC-6 candidate that REDRESS 92
rejected on "tape structural event mismatch"; SK-V9 HANDOFF §5 carries
the W3 pre-block forward. CH5 here also verifies that the V3 cohort does
not re-open W3 by phrasing.

## §2 — Disposition table

Five or more entries per report, ≥30 total. Each entry: (report, locus,
finding, verdict ∈ {ACCEPT, REVISE, REJECT}, citation, surgery if
REVISE).

### §2.1 — P1-V3-A (xctrace CPU Counters)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| A.1 | §1.1 probe binary `xctrace_probe` | The binary reads `runtime::generated_json::parse` (Track 1) and `bbnf_bench::track2::json::parse` (Track 2) and prints `proc_pid_rusage(RUSAGE_INFO_V5)` deltas. It writes nothing to disk except per-trace artefacts under `/tmp/skv9-xctrace-v3/`. It is a *read-only* probe with no substrate side-effect. Not a sidecar producer in the REDRESS 50/53 sense; not a parallel substrate. The new launchable binary at `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` is a Cargo `[[bin]]` target on an existing diagnostic crate (`bbnf-bench`), not a new library surface; per P1-V3-A §1.4 "no production parser code was modified". | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:25-46, 110-124`; `restart/skinny/tranches/sk-v9/HANDOFF.md:54` ("Structural scan, masking probes, PMU, and cycles surfaces remain diagnostic non-producers."). | n/a |
| A.2 | §1.1 dual-track invocation | The probe takes `<track:track1\|track2>` and runs each in isolation per process launch (`iters` loop calls one track only). The PMU rows in §2 are *separately tagged* `track1` vs `track2`; the table preserves the §1.2 mapping (`RESULTS.md:139` Track 1 = generated, Track 2 = hand-coded). The two tracks are never coalesced into a single c/B figure presented as "the parser". Track 2 is consumed as the *oracle row*, not as a SOTA gate. No Track 1 ≡ Track 2 dishonesty. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:30-39, 137-170`. | n/a |
| A.3 | §2 PMU table presentation | Track 2 cycles/B is reported alongside Track 1 cycles/B without any "Track 2 must equal Track 1" framing or comparator weighting; §2 closes with notes on CPI < 1 expectations and retired-instruction semantics, not with a Track-1-vs-Track-2 verdict. The table is structural-class telemetry, not an admission row. Per `RESULTS.md:139` Track 2 is the structural oracle, and §2 honours that. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:131-187`. | n/a |
| A.4 | §3 cross-validation against samply | The cross-validation appendix references V2 samply at `/tmp/skv9-p1-rerun/profiles/p1a/` and frames the relationship as triangulation, not substrate fusion. The samply and PMU instruments do not become a joint substrate; they remain separate diagnostic streams. No second producer. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:188-227`. | n/a |
| A.5 | §5 reproduction script | The reproduction pipeline lives at `/tmp/skv9-xctrace-v3/capture.sh` and the per-trace artefact lives in `/tmp/`; nothing is committed under `runtime/` or `crates/bbnf-simd/`. The diagnostic stream cannot drift into substrate without an explicit later wave wiring. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:271-317`. | n/a |
| A.6 | §6.1 + §7 PMU manifest schema | The schema (rows: corpus, track, bytes, iters, cycles, instructions, CPI, cycles/B) is novel evidence-only; it does not yet declare a consumer. Per the SK-V9 same-wave-consumer rule (`LOCKS.md` Lock 1 + §3W non-negotiables: "Same-wave consumer — no orphan kernel"), and per P1-V3-F §4.1 Edit B / §5.1 item 1 the manifest is to be cited from `SPEC.md` Authority block and consumed via `G-S-P1-RERUN-CONVERGED`. However the schema is **not yet wired into the existing `gate-json` consumer** named in `PASS-1-PROFILE.md` §2 ("Establish the c/B baseline that `gate-json` consumes."). The V3 manifest currently sits as an evidence stream awaiting integration. | REVISE | `skv9-p1-v3-A-xctrace-cpu-counters.md:393-409` (Sources); `skv9-p1-v3-F-redress-reconciliation.md:738-806` (gate bar §5). | V4 fold (or wave plan): name the gate consumer's source path (e.g., `skinny/crates/bbnf-bench/src/bin/gate.rs` or `gate-json`) that ingests `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (or a committed in-repo manifest), so the PMU manifest is not a perpetual `/tmp/`-only sidecar. Without the wiring the manifest is a non-consumed evidence stream, not yet a sidecar substrate but trending that way. |
| A.7 | §6.2 missing per-event PMC counters | §6.2 records that branch-mispredict / L1d / LLC per-event counters are not capturable via the public surface. The report does **not** propose a kperf private-framework binary as a substitute, which would have been a candidate sidecar producer; it accepts the limitation. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:336-358`. | n/a |

### §2.2 — P1-V3-B (xctrace Time Profiler)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| B.1 | §1.1 sampling-rate / template separation | The B-side capture uses the `Time Profiler` template with a 1 ms sample weight, distinct from A's `CPU Counters` PMU read. The two templates produce two independent trace streams under `/tmp/skv9-xctrace-v3/p1a-time-profile/` and `/tmp/skv9-xctrace-v3/p1b-tp/`. Neither stream is a substrate write-path. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:29-50`. | n/a |
| B.2 | §1.2 probe-binary reuse | B reuses the same `xctrace_probe` binary as A; "the only difference between the two captures is the xctrace template name". This is *one* probe and *one* binary, not a parallel pair of producers. The substrate ceiling discipline holds: the probe is read-only. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:51-63`. | n/a |
| B.3 | §1.5 aggregator classifier | The aggregator `/tmp/skv9-xctrace-v3/aggregate.py` walks the trace rows and assigns each leaf a *grammar-neutral* primitive class (`string_tiny_scan`, `number_digit_scan`, `scan_structurals`, `simd_movemask`, `consume_structural` …). The classifier is "grammar-neutral by construction (it matches symbol substrings, not JSON-role names) per CH2 GENERALITY." This satisfies Lock 14: the primitive vocabulary is grammar-agnostic. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:91-122`. | n/a |
| B.4 | §3.1 SC-1 verdict — `scan_structurals` 0.00% self-time | §3.1 confirms `scan_structurals` (the named SIMD kernel `runtime::generated_json::scan::scan_structurals` plus `bbnf_simd::aarch64::scan::neon::scan`, `bulk_emit_positions_64`, `bitmap_prefix_xor_64`) is **discarded, not consumed** on every (corpus, track). The disposition is purely *attributional*: the kernel exists, does not run on the production parse path, and contributes 0% self-time. P1-V3-B does **not** propose to wire it in — it records the non-fusion and leaves the disposition for later passes. This is the correct CH5 stance: a 0% leaf is a deletion candidate (subtractive), not an integration candidate (additive that would create a parallel substrate). | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:614-651`. | n/a |
| B.5 | §3.1 dual-callsite `simd_movemask` finding | `simd_movemask::movemask_u8x16` appears as 0–30.9% self-time but resolves to the *string scanner* callsite inside `scan_string_special_block`, not to the structural scan. P1-V3-B names the dual-callsite explicitly: "the same primitive name carries two callsites, and only the string-scan callsite fires on parse_only." This is the *opposite* of a renamed-scanner Lock-1 violation — it surfaces that one primitive name spans two callers and disambiguates them. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:643-650`. | n/a |
| B.6 | §3.4 samply mode-I attribution falsified | §3.4 records that V2 samply's "`dispatch_value` 95.6–99.6%" was a frame-pointer-coalescing artefact; xctrace DWARF walks the inlined frames and surfaces the real leaves. The corrective frame is *attribution*, not a substrate change. No new producer; just a finer view of the existing one. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:747-775`. | n/a |
| B.7 | §5.2 derived-cycles-per-class | §5.2 computes `primitive_class_cycles_per_byte ≈ row_cycles_per_byte × primitive_class_%`. The derivation is *witnessing* the existing substrate, not authoring a new one. The cycles are PMU truth, the percentages are Time Profiler truth, and the product is a *characteriser* of the existing single substrate. Per P1-V3-F §4.1 Edit F the V3 PMU c/B is "a diagnostic characteriser of hot leaves, not a producer". | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:840-881`; `skv9-p1-v3-F-redress-reconciliation.md:457-468`. | n/a |
| B.8 | §6 reproducibility — corpus-paths manifest | The reproduce script materialises a `corpus_paths.txt` under `/tmp/skv9-xctrace-v3/` rather than committing it under `skinny/test_data/`. The probe consumes it ephemerally. No retained sidecar artefact. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:896-955`. | n/a |
| B.9 | §3.2 SC-4 "75%" revision | §3.2 revises the SK-V7 §3.4 "string scanner pair ~75%" framing: tiny path dominates, full SIMD path is a tail. The revision is attributional ("the tiny scalar path dominates"); it does not propose any new SIMD producer. The "unicode-escape codec" `read_hex_unit_scalar` + `hex_nibble` is named as a *distinct primitive class S-P2 must enumerate*, which is a vocabulary decision, not a substrate write-path. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:653-720`. | n/a |

### §2.3 — P1-V3-C (Per-Corpus Deep Hot-Leaf Attribution)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| C.1 | §1.2 Track mapping discipline | The report names the V2 conflation explicitly: "V2 treated `parse_only.track1_generated` as Track 1 and `direct_to_struct.track1` as a second Track 1 surface, never citing the hand-coded Track 2." C names the hand-coded Track 2 (`bbnf_bench::track2::json::Parser::parse_value_at`) per `RESULTS.md:139`. Track 1 ≡ Track 2 dishonesty is *exposed*, not propagated. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:39-61, 427-433`. | n/a |
| C.2 | §1.3 attribution classifier | The 8-class taxonomy is grammar-neutral (`structural_scan`, `string_scan`, `number_parse`, `escape_handling`, `tape_write`, `allocation`, `sync_overhead`, `traversal_other`). The `traversal_other` bucket explicitly notes "when a leaf is a fused dispatch surface … it is bucketed `traversal_other`, NOT split into string/number/structural sub-claims" — the classifier is conservative. Lock 14 is honoured: no JSON-role names in the primitive vocabulary. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:63-82`. | n/a |
| C.3 | §4 SC-1 verdict — structural-scan non-fusion | C confirms `runtime::generated_json::scan::scan_structurals` and its NEON helpers appear "as a leaf in **zero** Track 1 / Track 2 production profiles" — only in the dedicated `structural_scan.simd` probe. The disposition is subtractive: name the non-fusion, do not wire in a consumer. Per CH5: a hot leaf flagged at 0.00% self-time and called "non-consumed" must be addressed by *deletion of the producer*, not by *addition of a consumer* (the latter creates the parallel substrate). C's text remains within the subtractive framing: "the SIMD scan symbols are non-producers, present only under synthetic probes". | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:234-287`. | n/a |
| C.4 | §4 "falsification path" — `#[inline(never)]` probe | The proposed falsification is a same-line `#[inline(never)]` build for one corpus — a diagnostic build flag toggle that breaks one symbol out for cycle attribution. This is *not* a new write-path; it is a measurement scaffold and reverts. No substrate change. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:274-281`. | n/a |
| C.5 | §5 SC-4 verdict — string-plane 75% adjudication | C reports the literal "75%" is "not measurable in the V2 samply dataset because the string scanners are inlined into `dispatch_value`". The adjudication is attribution-only; no new string-plane substrate is proposed. The Pearson r = +0.720 / Spearman ρ = +0.755 against quote-fraction confirm SC-4's *correlation* hypothesis but do **not** authorise a new producer. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:289-415`. | n/a |
| C.6 | §6 "What V2 was wrong or shallow" — `from_utf8` / `string_body_range` | C names "view-side cost" — `core::str::converts::from_utf8` and `runtime::generated_json::view::string_body_range` — as eager-decode hot leaves. The view module already lives at `runtime/src/grammars/json/view.rs` as part of the canonical generated runtime per Lock 1's "typed-value records borrow into [the tape]". The view is **the** typed-value access path, not a sidecar. No coupling fault. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:451-457, 503-505`. | n/a |
| C.7 | §1.2 / §2.3 Track 2 samply-shallow tag | C tags Track 2 "samply-insufficient pending V3-A". The framing is honest: Track 2 cycles in P1-V3-A are independently captured; C does not invent a substitute producer or merge Track 1 ≡ Track 2 evidence to fill the gap. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:44-54, 164-176`. | n/a |
| C.8 | §6 item 8 — bench-profile inlining barrier | C names the inlining barrier as a *bench-profile policy*, not an absence in the source. The remedy options are (i) cycle-precision xctrace, (ii) a targeted `#[inline(never)]` probe build, or (iii) `cargo asm`. All three are *diagnostic instruments*; none is a substrate write-path. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:472-478`. | n/a |

### §2.4 — P1-V3-D (Structural-Element Counts vs Throughput)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| D.1 | §1 correlation table cardinality | D operates entirely on `RESULTS.md` row data (`bytes`, structural counts, Mbps) and W0 manifest fields. It produces no new artefact, no kernel, no probe. The 17-row correlation is *evidence-only*. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:1-55`. | n/a |
| D.2 | §3.1 citm_catalog "structurally dense" reading | D names citm's WIN as "structurally dense relative to its string density" and credits "lazy-tape offsets" — i.e. the existing `OffsetTape` per `LayoutFacts.backend_shape`. No new tape variant proposed; the existing substrate is named as the WIN driver. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:123-140`. | n/a |
| D.3 | §5.5 "number FSM is bbnf's currently strongest sub-plane" | D's prescription for the number class is *do nothing* — "needs no immediate work". No new producer. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:352-357`. | n/a |
| D.4 | §6.1 V9 W1 string-plane cut — "ships the string-plane masked bitmap + deferred escape-complete" | This is the load-bearing CH5 ambiguity in V3-D. The text reads "**single-knob wave**: it ships the string-plane masked bitmap + deferred escape-complete". D does **not** specify whether the masked bitmap (a) **replaces** the existing `match_tiny_plain_string_with_cap::<16>` scalar fast-path at `runtime/src/grammars/json/generated.rs:171-185` and the SIMD fallback `match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs`, or (b) runs **alongside** them as a *bitmap producer that another consumer reads*. Reading (b) would re-open REDRESS 50 ("retained projection side tables REJECTED"), REDRESS 53 ("structural-mask parser-local cursor REJECTED"), REDRESS 61/62/83 (retained trusted-string scan rejections), and is precisely the W3 union shape REDRESS 92 rejected on "tape structural event mismatch". The reader cannot tell from D §6.1 which reading applies. Lock 1: "A SIMD mask stream is a transient producer, not a retained sidecar" — if the bitmap is consumed in the same loop and replaces the prior scan, that is admissible; if it is retained and a downstream consumer reads it, that is a parallel substrate. D §6.1 is silent on the cardinality. | REVISE | `skv9-p1-v3-D-structural-breakdown.md:363-375`; cf. `LOCKS.md:34` Lock 1; `skinny/REDRESS.md` 50, 53, 61, 62, 83, 92; `restart/skinny/tranches/sk-v9/HANDOFF.md:113-114, 119-122`. | V4 fold: D §6.1 must add one sentence stating the masked bitmap **replaces** `match_tiny_plain_string_with_cap` / `match_string_at_quote_trusted_utf8` on the production hot path inside `runtime/src/grammars/json/generated.rs`, and is consumed in the same loop (not retained, not a side-table). Equivalently, cite `LOCKS.md` Lock 1's "transient producer, not a retained sidecar" clause verbatim and bind §6.1 to it. Until this binding lands, §6.1 is at risk of re-opening the SK-V8 W3 rejection class. |
| D.5 | §6.1 "deferred escape-complete check" | The "defer the escape-complete check to a flaw probe rather than running it inline" framing risks introducing a *second pass* over the same bytes — first the masked-bitmap scan, then the escape-complete flaw probe. A two-pass architecture over the same input is the "parser-local cursor" shape REDRESS 53 explicitly rejected ("The cursor still performs a second structural scan while the recursive-descent parser continues to read the same source"). Whether the "flaw probe" is a *production consumer* or a *diagnostic-only producer* is unspecified. | REVISE | `skv9-p1-v3-D-structural-breakdown.md:336-343`; `skinny/REDRESS.md` 53. | V4 fold: D §6.1 must name the escape-complete check as either (i) inline within the same SIMD pass that produces the masked bitmap (admissible — one pass), or (ii) a strictly diagnostic-only probe gated behind a `#[cfg(test)]` or feature flag and never reached on the production path (admissible — non-producer). The phrasing "defer … to a flaw probe" without (i)/(ii) specification leaves the cardinality undetermined. |
| D.6 | §6.2 V9 W2 "digest-sink truth pass" | "A separate wave should profile the digest producer; see P1-V3-A/B xctrace lanes for that capture. Do **not** bundle direct plane fixes with the §6.1 string-plane wave." This text is a *sequencing recommendation*, not a substrate proposal — W2 is "profile the digest producer", which is a measurement, not a new write-path. ACCEPT, with the rider that W2's actual intervention shape (when an S-P3 plan drafts it) must be re-CH5-audited; the V3-D framing is not itself a coupling fault. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:398-405, 420-422`. | n/a |
| D.7 | §6.5 typed plane — "expand horizontally, not vertically" | "The typed plane should be expanded **horizontally** (run real_typed_struct on more corpora) … not vertically (no substrate change)." D explicitly forbids a substrate change on the typed plane. This is Lock-1 aligned. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:407-413`. | n/a |
| D.8 | §5.4 "The structural-emit plane is not the bottleneck" | "structural opens cost **nearly free under the lazy tape** — the offset write is amortised through the same cache line as the byte scan." This is a *finding* about the existing substrate, not a proposal to replace it. The lazy tape (the canonical `OffsetTape` backend shape) is named and preserved. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:345-350`. | n/a |
| D.9 | §6.4 "do not chase by string-plane wave" | D explicitly *de-couples* the direct plane from the string-plane intervention — the direct plane is q/B-decorrelated, so a string-plane wave that bundled direct fixes would create a shape-mismatched intervention. The de-coupling is correct CH5 discipline (no cross-substrate co-loading). | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:398-405`. | n/a |

### §2.5 — P1-V3-E (Legacy Cleanup Audit)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| E.1 | §2.1 x86_64 orphan SIMD kernels (14 files) | E proposes deletion of 14 `unimplemented!()` shells (avx2/avx512 families) per "REDRESS 50-55 wave-5 admission rule" + Lock 16. The deletion is **subtractive**: removing primitives with no consumer. Each cited file has *no production consumer* (test-only refs); deleting them eliminates dead substrate-adjacent code without removing any coupling-enforcing piece. Lock-1 aligned: no parallel substrate is created or sustained. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:135-159`. | n/a |
| E.2 | §2.2 aarch64 `match_tiny_plain_string.rs` (136 LOC) | E flags the NEON module as SAFE-TO-DELETE on REDRESS 28+33 grounds: the admitted scalar `match_tiny_plain_string_with_cap::<16>` lives in `runtime/src/grammars/json/generated.rs:171-185` (the *generated* hot path), NOT as a NEON primitive in `bbnf-simd`. Deleting the orphan NEON kernel removes a *renamed-scanner risk surface* (per CH5 "renamed-scanner Lock 1 violation") and aligns with REDRESS 72's admission of the scalar shape. Subtractive disposition, Lock-1 aligned. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:163-165`; `skinny/REDRESS.md` 28, 33, 72. | n/a |
| E.3 | §2.2 R1 `string_block::scan_string_special_block` — KEEP-IF-USED | E flags KEEP-IF-USED because the consumer at `parse-that-regex/src/lib.rs:472, 551` is the LIVE UTF-8-validating `match_string_at_quote_trusted_utf8`, which the generated runtime calls at `runtime/src/grammars/json/generated.rs:193`. The REDRESS 61/62/83 rejections were of a *different surface* (retained-generated trusted-string wrapper). E correctly distinguishes the rejected from the admitted shape; the LIVE consumer chain is not a hidden sidecar. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:165, 323-325`. | n/a |
| E.4 | §2.3 utility orphans (quad_load, byte_context, digit_mac, cache_hints) | All four have TEST-ONLY callers; per Lock 16 ("Scalar reference per SIMD/ASM primitive; checkasm parity before wiring") and the §3W non-negotiable "Same-wave consumer — no orphan kernel", they are correctly marked SAFE-TO-DELETE. Subtractive cleanup; Lock-1 aligned. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:168-175`. | n/a |
| E.5 | §2.4 LIVE primitives list | The KEEP table enumerates the surviving production consumers. The `aarch64::unescape_uxxxx` LIVE consumer (`parse-that-regex/src/lib.rs:402, 419`) is correctly disambiguated from the REDRESS 64/82 rejected *single-quartet retained validator route* — the LIVE consumer is the materialization path (4-unit packed decode), not the rejected validator. No hidden sidecar. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:177-191, 327-329`. | n/a |
| E.6 | §1.3-§1.8 doc-corpus ARCHIVE-MOVE block | The 524 file ARCHIVE-MOVE proposal is doc-tree hygiene — physically moves SK-V3.5/V5/V6/V7/V8 historical files to `restart/skinny/archive/`. The audit *preserves* every REDRESS citation by leaving paths intact under the new prefix (R4 §6 records that REDRESS 65-69's static citations into SK-V6 SYNTHESIS-WAVE-1-PLAN.md will point at `archive/sk-v6/`). No coupling-enforcing audit is destroyed. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:30-128, 335-338`. | n/a |
| E.7 | §2.7 `crates/simd-scan/` fossil dir | The empty directory is confirmed not in `Cargo.toml` workspace members, per SK-V5 NUKE-PLAN.md Wave 4. Subtractive delete of a fossil with zero LOC. Lock-1 aligned. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:202-205`. | n/a |
| E.8 | §6 R8 `unimplemented!()` cfg-gating | E names that the x86_64 `unimplemented!()` bodies are cfg-gated to target features absent on the dev host; deleting them improves x86_64 build correctness. The proposal removes a *latent panic surface* that would have been the only way the orphan kernels could be reached at runtime. Subtractive and substrate-cardinality-improving. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:351-353`. | n/a |
| E.9 | §6 R3 `aarch64::movemask` internal reuse | E notes `movemask.rs` has only one test caller but is `pub use`d by `utf8::validate_block` which has LIVE parse-that-regex consumers. E marks KEEP; the local copy of `movemask_u8x16` inside the deletable `match_tiny_plain_string.rs` is correctly identified as a *separate* copy. This is precise cardinality accounting: one primitive name, one production owner. No renamed-scanner risk. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:331-333`. | n/a |

### §2.6 — P1-V3-F (REDRESS Reconciliation)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| F.1 | §1.2 contract admits xctrace c/B | F adjudicates that xctrace `cpu-counters` is "a direct hardware-counter read through Apple Silicon's PMU via kernel `kpc` APIs" — a real PMU source, not an ns-derived estimate. The discipline boundary (forbidden: `ns_per_byte` → c/B inference) is preserved. xctrace does not become a sidecar producer because it reads the same parse surface the existing benches read; it does not write a new substrate artefact consumed by the parser. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:63-93`. | n/a |
| F.2 | §2 REDRESS ledger reconciliation | F enumerates 93 entries with STILL-LOAD-BEARING / SUPERSEDED / HISTORICAL tags. Every CH5-relevant rejection (REDRESS 50, 51, 53, 60-65, 82-84, 92, 93) is correctly tagged STILL-LOAD-BEARING. No silent re-opening; no class umbrella collapsed below its REDRESS-level granularity. The ledger preserves the cardinality-discipline anchors. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:115-301`. | n/a |
| F.3 | §3.2 class-umbrella additions to HANDOFF §5 | F proposes four class umbrellas: (i) string-scanner widening / boundary-collapse class (REDRESS 60-65, 82-84); (ii) direct receiver / scratch / semantic-fact class (REDRESS 66-69); (iii) bench-private hand Track 1 / hand typed sink class (REDRESS 34, 70); (iv) PMU / cycles / Criterion-slope / masking / structural-scan-as-producer class (SPEC §1 non-negotiables). Each umbrella *broadens* the pre-block surface rather than narrowing it — strictly safer for CH5. Each cites the underlying REDRESS entries by number, preserving traceability. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:339-365`. | n/a |
| F.4 | §4.1 Edit F — SPEC §1 non-negotiables clarifier | The proposed clarifier states "V3 real-PMU c/B is a diagnostic characteriser of hot leaves, not a producer; it does not enable any behavior admission path that was blocked in V2." This is the Lock-1 cardinality discipline rendered in spec form: the PMU stream is a *transient producer* in the SC-6 sense, not a retained substrate. The framing preserves the W3 union's "must replace, must not run alongside" stance — F does **not** propose to install the PMU manifest as a parallel comparator gate. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:457-468`. | n/a |
| F.5 | §3.1 HANDOFF §5 item 3 ↔ REDRESS 92 mapping | F maps HANDOFF §5 item 3 ("W3 structural implementation without retained class/event grammar plus retained `ValueRef` cursor proof") to REDRESS 92. The mapping is faithful: REDRESS 92's "the scanner/tape event model is not isomorphic" framing — the W3 union substrate "must replace, must not run alongside" requirement — is preserved through F's restatement. F does **not** weaken the W3 fit-gate language; it carries it forward verbatim. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:308-311, 330`; `skinny/REDRESS.md` 92 (`...Tier B string-boundary/quote-backslash/parity work...`); `restart/skinny/tranches/sk-v9/HANDOFF.md:113-114`. | n/a |
| F.6 | §4.1 Edit E — §0.3 Required Telemetry deferral | F proposes to **defer** §0.3 telemetry-schema edits "until V3 CHALLENGE selects the schema." Deferring an integration step is the orchestration analogue of leaving a substrate-cardinality decision open. The deferral is bounded (CHALLENGE selects next), so this is not a coupling fault — but the orchestrator must close it in V4 or the PMU manifest schema risks lingering as a non-integrated stream (echoing A.6). | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:450-454`. | n/a |
| F.7 | §4.2 HANDOFF §5 umbrella additions | The diff in §4.2 Edit E inserts the four umbrellas into the canonical pre-block list at `restart/skinny/tranches/sk-v9/HANDOFF.md:104-124`. Each umbrella line cites its REDRESS entries. The umbrella for "PMU / cycles … as Track 1 / Track 2 / typed / direct / strict producers" explicitly closes the door on the V3 PMU evidence being used as an admission producer. CH5-aligned. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:625-638`. | n/a |
| F.8 | §6.5 "class-umbrella creep risk: medium" | F self-audits the umbrella additions for creep — concedes the umbrella sentences compress ~20 REDRESS rejections, and recommends keeping the detailed `alpha/alpha-C-redress-digest.md` ledger alongside. This is the correct discipline: umbrellas summarise, the underlying ledger binds. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:860-869`. | n/a |
| F.9 | §5.3 contract-truth check item 13 | F's V3 CHALLENGE bar item 13 reads "Hot leaves named to grammar-neutral primitives; CH2 rejects JSON-role re-naming. xctrace c/B rows are not used as producers — only as characterisers — per SPEC §1 amended clause (see §4.1 Edit F)." This is the load-bearing CH5 check rendered as a gate item. The PMU manifest is bound *by spec* to non-producer status before convergence is recorded. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:797-801`. | n/a |
| F.10 | §1.3 proposed clarification | The one-paragraph clarifier ("Direct hardware-counter reads … are admitted as real PMU sources. Indirect derivation from `ns_per_byte`, wall-clock loop time, or any inferred/nominal CPU frequency is rejected regardless of source.") is non-weakening and preserves the substrate-cardinality stance: real-counter-vs-ns is the discipline, not tool-by-tool. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:94-110`. | n/a |

## §3 — Aggregate verdict

Disposition tally across §2: **45 entries; 43 ACCEPT, 2 REVISE, 0 REJECT.**

Per-report breakdown:

| Report | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| P1-V3-A | 6 | 1 (A.6) | 0 |
| P1-V3-B | 9 | 0 | 0 |
| P1-V3-C | 8 | 0 | 0 |
| P1-V3-D | 7 | 2 (D.4, D.5) | 0 |
| P1-V3-E | 9 | 0 | 0 |
| P1-V3-F | 10 | 0 | 0 |

ACCEPT rate: 43/45 ≈ **95.6%**. Two REVISE dispositions cluster on a
single semantic gap: D §6.1's V9 W1 "string-plane masked bitmap +
deferred escape-complete" leaves the cardinality (replace vs run-
alongside) and the pass count (one pass vs two passes over the same
bytes) undetermined. A.6 is the milder echo: the PMU manifest schema is
named but its `gate-json` consumer integration is not wired in-tranche.

V3 clears the CH5 lens at the convergence threshold (≥95% ACCEPT per
`ORCHESTRATOR.md` §3Z). The two REVISE items are non-blocking provided
they fold cleanly into V4: D's revision is one sentence and a
verbatim citation of `LOCKS.md` Lock 1's "transient producer, not a
retained sidecar" clause; A's revision is naming the gate-consumer source
path that ingests `pmu_rows.tsv`. The cohort otherwise honours Lock 1
strictly:

- The PMU probe binary is a *read-only* diagnostic, not a substrate
  producer (A.1, B.2).
- Track 1 / Track 2 are kept distinct as parser-vs-oracle, not
  collapsed into a SOTA gate (A.2, A.3, C.1, C.7).
- `scan_structurals` at 0.00% self-time is named as a *non-producer*
  candidate for deletion, not for additive consumer-wiring (B.4, C.3).
- The SK-V8 W3 union substrate is not re-opened — F's HANDOFF §5
  mapping carries REDRESS 92 forward verbatim (F.5).
- E's deletion ledger is uniformly subtractive: removing primitives
  with no production consumer (E.1-E.9). No coupling-enforcing audit
  is destroyed; the deletions follow the Lock 16 "no orphan kernel"
  rule and REDRESS 50-55's wave-5 admission rule literally.
- F's PMU-as-non-producer umbrella (F.4, F.7, F.9) closes the door on
  the V3 PMU stream being used as an admission producer.

The "renamed-scanner Lock 1 violation" failure mode (CH5 §3W language)
is positively guarded: B.5's dual-callsite `simd_movemask` finding
exposes a one-primitive-name-two-callsites case, and E.9's R3 audit
keeps the live consumer chain whole; neither report installs a renamed
scanner.

The "Track 1 ≡ Track 2 dishonesty" failure mode is positively guarded:
A.2's per-row tagging keeps the two tracks bench-distinct, and C.1
exposes the V2 Track-1/Track-2 conflation as a defect that V3 corrects.

## §4 — Specific Lock-1 / Track-1 ≡ Track-2 / sidecar leaks requiring V4 fold

### §4.1 — V3-D §6.1 W1 string-plane intervention shape (load-bearing)

The "ships the string-plane masked bitmap + deferred escape-complete"
single-knob wave proposal at `skv9-p1-v3-D-structural-breakdown.md:373`
is *cardinality-ambiguous*: a reader cannot tell whether the masked
bitmap **replaces** the existing scalar `match_tiny_plain_string_with_cap`
+ SIMD `match_string_at_quote_trusted_utf8` pair (admissible — singular
substrate), or **runs alongside** them as a retained mask that some
downstream consumer reads (forbidden — the REDRESS 50/53/61/62/83
class and the SK-V8 W3 union shape REDRESS 92 rejected).

V4 fold for D §6.1: insert one binding sentence and one verbatim Lock 1
citation. Verbatim suggested insertion:

> The masked-bitmap producer **replaces** the existing scalar +
> SIMD-fallback string scanner pair (`match_tiny_plain_string_with_cap`
> at `runtime/src/grammars/json/generated.rs:171-185` and
> `match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs`)
> on the production hot path, and the bitmap is consumed in the same
> recursive-descent loop that produces it. Per `LOCKS.md:34` Lock 1, the
> mask stream is "a transient producer, not a retained sidecar"; this
> wave does not retain the bitmap as a side-table, does not install a
> parser-local cursor over it, and does not re-open the W3 union
> rejected at REDRESS 92.

Without this binding, V3-D §6.1 invites a recurrence of REDRESS 50-65's
sidecar-class rejections.

### §4.2 — V3-D §6.1 "deferred escape-complete check" pass count

The "defer the escape-complete check to a flaw probe rather than
running it inline" framing at `skv9-p1-v3-D-structural-breakdown.md:343`
is *pass-count-ambiguous*. Two-pass architectures over the same bytes
are the parser-local-cursor shape REDRESS 53 explicitly rejected.

V4 fold for D §6.1: name the escape-complete check as either (i)
inline within the same SIMD pass that produces the masked bitmap (one
pass — admissible), or (ii) strictly diagnostic-only, gated behind a
`#[cfg(test)]` or feature flag and never reached on the production
path (non-producer — admissible). The phrase "defer … to a flaw probe"
without (i)/(ii) qualification leaves the pass count undetermined.

### §4.3 — V3-A §6 PMU manifest gate-consumer wiring

The PMU manifest schema at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` per
`skv9-p1-v3-A-xctrace-cpu-counters.md:396-409` is fresh evidence with
no committed consumer in this wave. `PASS-1-PROFILE.md` §2 names
"`gate-json`" as the c/B consumer; V3-A does not wire the manifest
into `gate-json`. Per `LOCKS.md` Lock 1 + the §3W "Same-wave consumer —
no orphan kernel" non-negotiable, this is currently a non-consumed
evidence stream, not yet a sidecar substrate but trending that way.

V4 fold for A: either (i) commit a stable in-repo manifest path (not
`/tmp/`) plus the `gate-json` (or `gate.rs`) reader in the same wave,
or (ii) explicitly tag the manifest as `diagnostic-only,
never-a-producer` per F's class-umbrella iv, mirroring the SPEC §1
non-negotiables. F §4.1 Edit E defers the §0.3 telemetry-schema decision
to "V3 CHALLENGE selects the schema"; V4 closes that deferral.

### §4.4 — Class-umbrella regression watch

F.3's four class umbrellas are CH5-positive (they broaden the
pre-block surface). However the V4 dispatch must verify that the
umbrellas land in `HANDOFF.md` §5 exactly as F §4.2 Edit E specifies
— not paraphrased and not consolidated. F.8 self-audits this risk;
the orchestrator's V4 fold must enforce it.

### §4.5 — No active leaks beyond §4.1–§4.4

The cohort does not introduce:

- A new substrate variant beyond the `LayoutFacts.backend_shape`
  five-variant set at `ARCHITECTURE.md` §7.3 (no new `BackendShape`
  proposed; D §6.5 forbids one for the typed plane).
- A parser-owned cursor / fact slot (REDRESS 51, 53 stay pre-blocked
  per F §3.1).
- A renamed scanner: B.5 exposes the one-primitive-name-two-callsites
  case as evidence, not as a rename. E.2's deletion of NEON
  `match_tiny_plain_string` removes the renamed-scanner risk surface
  rather than instantiating it.
- A Track 1 ≡ Track 2 collapse: A.2 / C.1 keep the two surfaces
  bench-distinct. The dual-track xctrace_probe is structurally a
  *switch* (`<track:track1|track2>` flag), not a fusion.
- A retained PMU stream as comparator producer: F.4 / F.7 / F.9 bind
  the PMU evidence to characteriser status by SPEC + HANDOFF + gate-bar
  language.

The substrate union holds. V3 advances at CH5 ≥95% ACCEPT subject to the
two REVISE items in §4.1–§4.3 folding into V4.

## §5 — Sources cited

- `restart/locks/LOCKS.md:34` (Lock 1), `:60` (Lock 14).
- `restart/prompts/ORCHESTRATOR.md` §3W (CH5 contract), §8 (non-negotiables).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md` §1.1, §1.3, §2.1.
- `restart/skinny/tranches/sk-v9/HANDOFF.md` §5 (pre-blocked routes).
- `skinny/REDRESS.md` 28, 33, 34, 50, 51, 53, 60–72, 80, 82, 83, 84, 88, 89, 91, 92, 93.
- `skinny/RESULTS.md:139` (Track 1 vs Track 2 definition).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md` (subjects).
