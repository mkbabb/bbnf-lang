# AGENT-5 — Bench/test contrivances (SK-V17 close audit, V3)

**Scope:** Is the SK-V17 >SOTA claim (CSS rich-CSSOM 2.0–3.3× > lightningcss; JSON > sonic-rs)
measurement-VALID or CONTRIVED-at-some-granularity? Master HEAD `f6a38445b`.

**Headline verdict: MEASUREMENT-VALID, with ONE disclosed fairness asymmetry (MEDIUM).**
The SK-V16 contrivance family — 24-row broadcast, brace-counter "full_parse", fixture
byte-equality short-circuits, fake `@generated`, single-sample noise — is **absent** from
the SK-V17 measured CSS path. The harness sampling is genuinely cold and N≥50. The 9-field
equality oracle is genuinely independent (cssparser-driven). The numbers reproduce. The one
standing concern is a **materialization-depth asymmetry** in the rich-vs-lightningcss
comparison: track1_rich *counts* a 9-field aggregate lazily; lightningcss *builds an owned
typed CSSOM*. This is disclosed in the design ("lazy, zero payload writes") but is the axis on
which the >SOTA delta is not strictly equal-work. It does **not** rise to PRUNE-REQUIRED
(track1_rich is genuine per-node work, not a brace count), but the next cycle's >SOTA framing
must state the comparison is "lazy-projection Mbps vs eager-materialization Mbps", not implied
equal-work parity.

---

## §1 — Canonical harness sampling honesty — VERDICT: GENUINE COLD, N≥50

The three named harnesses live as `src/bin/`:

| harness | path | claimant |
|---|---|---|
| canonical multi-workload | `css_canon_bench.rs` | per-corpus median Mbps, 4 workloads |
| W2 rich vs lcss | `w2_rich_cssom_bench.rs` | the **>SOTA-claimant** rich/lcss ratio |
| W1 tape-typed (informational) | `w1_tape_typed_bench.rs` | 4-field vs lcss (not a speed admit) |

**Cold-per-parse loop is genuine.** `css_canon_bench.rs:146-159` (`sample`): one untimed touch
outside the loop (page-fault prewarm of the source buffer, explicitly NOT a parser warm — the
parser state is fresh and the output dropped every iteration), then N iterations each timing
exactly one `parse(black_box(input))` with `start = Instant::now()` immediately before and the
result `black_box`'d and dropped. No amortised allocation, no warm cache, no reused parser
state. `w2_rich_cssom_bench.rs:33-38` / `:63-86` and `w1_tape_typed_bench.rs:29-34` are
identical in shape: `cold_sample` times one closure invocation; each builds a fresh tape
(`rich_summary` → `parse_into_tape` → `TapeBuilder::new`) and drops it. This satisfies the
`no-warm-benches` cold-per-parse contract.

**N≥50 is gate-enforced, not asserted-then-ignored.** `css_canon_bench.rs:250`
(`assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")`),
`w2_rich_cssom_bench.rs:45`, `w1_tape_typed_bench.rs:41`. Default N=200. Statistics are real
(median/min/max/stddev computed over the sorted sample vector, `css_canon_bench.rs:160-176`).

**No broadcast.** Each `(corpus, workload)` pair is sampled independently and emitted as its own
`ROW` (`css_canon_bench.rs:261-277`). I ran `./target/release/w2_rich_cssom_bench 80` live: the
four corpora produce four **distinct** lightningcss medians (1086.1 / 827.1 / 1240.4 / 1225.8
Mbps) and four distinct ratios (2.145× / 2.905× / 1.911× / 1.975×). The SK-V16 "single tuple
`2319.041/2362.037/929.281` projected across 24 rows" pattern is **gone** — this is the explicit
thing SK-V17 W0 was built to kill (`HANDOFF.md:54-56,162`), and it is killed.

---

## §2 — Rich-vs-lightningcss fairness — VERDICT: MEDIUM (disclosed asymmetry; real per-node work)

This is the load-bearing axis. The claim: track1_rich (9-field) beats lightningcss full-CSSOM
2.0–3.3×.

**What track1_rich actually computes** (`generated.rs:305-331` `CssDocument::rich_summary`):
for every tape node it (a) recovers the branch tag from `(source byte, at-rule flag)`,
(b) for qualified rules counts selector-list entries by top-level commas over the prelude span
(`generated.rs:149-158`, `count_top_level_commas` → NEON `runtime_simd`), (c) for declarations
classifies the **leading significant token** of the value span into
dimension/number/color/function/keyword (`generated.rs:201-204, 225-254` `CssTypedValue::classify`).
It accumulates **nine `usize` counters**. It allocates nothing per node, builds no owned
structure, parses no full value grammar — it reads the value HEAD only.

**What lightningcss computes** (`css_canon_bench.rs:113-116`, `w2_rich_cssom_bench.rs:81-85`):
`StyleSheet::parse(input, ParserOptions::default())` tokenizes and parses every declaration into
typed owned `Property` variants (full value grammar, allocated selectors, owned `Vec<CssRule>`).

**The asymmetry:** track1_rich is a *lazy aggregate count over a structural recognizer's tape*;
lightningcss is a *full eager owned materialization*. They are not equal-work. The 2–3× delta is
real and reproducible, but it is "lazy 9-field projection Mbps vs eager full-CSSOM Mbps", not a
parity-of-product comparison. This is the SK-V16 brace-counter concern's lineal descendant —
**but materially less severe**, for three reasons:

1. **The recognizer is a genuine CSS structural parser, not a brace counter.**
   `CssFullParser::parse_stylesheet` (`generated.rs:460-599`) distinguishes at-rules vs
   qualified rules vs declarations, handles strings (`consume_string_at`), comments, escapes,
   balanced `()[]{}` (`find_component_delim`/`consume_balanced_at`, `:657-713`), `@charset`
   tokenizer directives, CDO/CDC legacy markers. It is byte-equivalent in *structure* to
   cssparser (proven by §3).
2. **The rich rider pays measurable cost — it is not a no-op.** My live run: `track1_4field`
   (structural only) = 3106.6 Mbps; `track1_rich` (9-field with value classification) = 2329.8
   Mbps on bootstrap — the rich projection is ~25–33% **slower** than the 4-field, proving the
   selector-comma-count + value-head-classify work is genuinely executed, not elided.
3. **The value classification visits every declaration's value head**, re-scanning the balanced
   value span (`scan_value_end`, `generated.rs:817-836`) — real per-declaration work.

**Disclosure status:** The lazy-vs-eager nature IS disclosed in the design prose
(`generated.rs:297-304` "writing nothing to the payload arena", `HANDOFF.md:256` "zero payload
writes", `w2_rich_cssom_bench.rs:1-9` "lazily projects"). What is NOT crisply disclosed is that
the comparison's denominator (lightningcss) does strictly more materialization work, so the
ratio overstates a "build the same CSSOM" parity. **Recommendation: keep the measurement; tighten
the >SOTA narrative to name the materialization-depth difference explicitly.**

---

## §3 — 9-field equality oracle — VERDICT: GENUINE (independent cssparser walk, not tautology)

`rich_cssom_matches_cssparser_on_real_corpora` (`nonjson_css_l4.rs:3544-3552`) →
`assert_rich_strict_equality` (`:451-459`) compares `track1_rich_facts` (the tape projection
rendered to a canonical 9-field string) against `rich_oracle_facts` →
`cssparser_rich_summary_facts` (`:433-443`).

**The oracle is a genuinely independent engine.** `OracleParser` (`:2611-2972`) drives
**cssparser's real tokenizer** (`StyleSheetParser` + `DeclarationParser`/`AtRuleParser`/
`QualifiedRuleParser` impls). It counts the 9 fields from cssparser's `Token::Function`,
`Token::Hash`, `Token::Dimension`, `Token::Number`, etc. (`:2765-2842`) and cssparser's
rule/block structure (`:2891-2962`). At-rules (`:2911-2912, 2919-2920`) and qualified rules
(`:2956-2958`) are counted by cssparser's own grammar dispatch, not by re-reading the tape. This
is a **different tokenization engine producing the same field counts** — the defining property
of a non-tautological oracle.

**Definitional alignment ≠ self-comparison.** The two helpers
`OracleParser::record_leading_typed_value` (`:2688-2710`) and `rich_count_top_level_commas`
share the same *definition* (leading-byte/shape; top-level comma) as the tape's
`CssTypedValue::classify` / `count_top_level_commas`. That is a deliberate shared *specification*
of what "a dimension" or "a selector" means — necessary for any equality oracle — applied to
**independently-derived spans** (cssparser token positions on one side, tape lazy scan on the
other). It is not the tape's code calling itself. The field counts are real: at corpus scale the
test would panic on any mismatch (`:3549-3551`), and it passes on all four real corpora.

**The 4-field structural oracle** (`cssparser_summary_facts`, `:512-522`) and the **lightningcss
sidecar** (`lightningcss_facts`, `:528-544`, which additionally walks the lightningcss CSSOM and
checks a declaration projection) are likewise independent. lightningcss is invoked as the real
`StyleSheet::parse` full-CSSOM (`:530`), the fair bar — not a lighter call.

---

## §4 — Fixture short-circuit / FNV / fake-@generated sweep

| concern | finding | verdict |
|---|---|---|
| **FNV short-circuit in measured path** | `fnv64` appears ONLY in `emit_full_parse` (`generated.rs:393-394, 899`), the diagnostic `track1_full_parse`/fact-stream String workload, as an `input_fnv64` **provenance stamp** of the input bytes. It gates no parse work. The >SOTA-claimant `rich_summary`/`summary` path (`parse_into_tape`) never touches FNV. | CLEAN |
| **FNV closed-enum quarantine (SK-V15)** | `fnv_quarantine.rs` is still bench-only (`CLOSED_ENUM_ROWS` are JSON `direct_to_struct`/`real_typed_struct` rows; `:5-12`), NOT migrated into any measured CSS path. | CLEAN (still quarantined as required) |
| **CANONICAL_FIXTURE / CAPTURED / SHA / byte-equality short-circuit** | The corpus loader (`css_l4_corpus.rs`) carries SHA256 pins (`:28,36,44,52`) and a `_match_manifest` test (`:86-104`) — these validate corpus *identity at load*, not a parse short-circuit. The measured workloads parse the raw bytes every sample; no fixture-keyed early return in the timed region. Fixture-drift tests (`:3555-3559`, `lightningcss_sidecar_fails_closed_on_fixture_drift`) are correctness fail-closed guards, not measurement gates. | CLEAN |
| **fake @generated header** | `generated.rs:1` and `parser.rs:1` claim `@generated by skinny bbnf-codegen`. **Verified honest:** the emitter `crates/codegen/src/runtime_generator.rs` (1611 lines) genuinely produces `CssFullParser`, `rich_summary`, `CssTypedValue::classify`, `count_top_level_commas`, `CssRichSummary` (`:605-608, 644-653, 687, 857, 901-951, 1005-1023`). This is a real generator template emitting the rich CSSOM code — the inverse of SK-V15's fake-header finding. The rich CSSOM code is grammar-NEUTRAL (one template for all CSS grammars), which aids generalization. | CLEAN / POSITIVE |

---

## §5 — target-cpu=native dependence — VERDICT: NOT DEPENDENT

No `target-cpu=native`, no `RUSTFLAGS` CPU tuning anywhere in `.cargo/config.toml` (read in
full) or any `Cargo.toml`. The >SOTA numbers are produced under stock `release` / `ay-final`
(fat LTO, codegen-units=1, opt-level=3 — `Cargo.toml:78-91`, `.cargo/config.toml:74-80`). The
NEON acceleration (W3) is `aarch64` baseline NEON (always-on for arm64), not an opt-in `-C
target-feature` gate. **The >SOTA claim is portable across any arm64 host at stock release; it is
not propped up by undisclosed native tuning.** (Note: the W3 NEON path is arm64-specific; the
x86 portability of the *speedup* is not measured, but the claim is scoped to the arm64 host
disclosed in the `ROW ... host=` field, `css_canon_bench.rs:259`.)

---

## §6 — JSON > sonic-rs — VERDICT: VALID, NOT RE-MEASURED IN SK-V17 (disclosed)

SK-V17 touched only the CSS path; the JSON >sonic-rs claim is inherited from SK-V14
(`RESULTS.md:5-6`) and asserted held ("JSON 51/51 held", `HANDOFF.md:270`). The comparator is
sound where it stands: `sonic_skipper::parse_only` (`:3-11`) uses a real `sonic_rs::Deserializer`
with `IgnoredAny` skip + `end()` (strict — the test `skipper_accepts_complete_json_and_rejects_
trailing_bytes` proves trailing-byte rejection), and `json_parity.rs` benches BOTH a
`sonic_rs_skipper` (skip-validate) and `sonic_rs_direct_to_struct` (full owned materialization)
plane (`:97-122, 273-275`). The strict-product parity oracle (`:21-27`) gates equality before
speed. No SK-V17-specific JSON contrivance was introduced. **VALID; carry forward as-is.**

---

## §7 — Findings summary

| # | Finding | Severity | Verdict | Cite |
|---|---|---|---|---|
| F1 | Cold N≥50 sampling is genuine (fresh parse/sample, output dropped, gate-asserted) | — | CLEAN | `css_canon_bench.rs:146-176,250` |
| F2 | No broadcast — per-corpus distinct medians/ratios reproduce live | — | CLEAN | `w2_rich_cssom_bench.rs:56-94`; live run |
| F3 | rich-vs-lcss is lazy-count vs eager-materialization (NOT equal-work); disclosed as lazy but ratio implies parity | **MEDIUM** | COURSE-CORRECT (narrative), not prune | `generated.rs:305-331`; `css_canon_bench.rs:113-116` |
| F4 | 9-field equality oracle is genuinely independent (cssparser-driven), not tautology | — | CLEAN | `nonjson_css_l4.rs:2611-2972, 451-459` |
| F5 | FNV only a provenance stamp in the diagnostic plane; never gates measured parse | — | CLEAN | `generated.rs:393-394,899` |
| F6 | @generated headers HONEST — real emitter produces the rich CSSOM code | — | CLEAN/POSITIVE | `crates/codegen/src/runtime_generator.rs:605-1023` |
| F7 | No target-cpu=native dependence; stock release/ay-final | — | CLEAN | `.cargo/config.toml` (full); `Cargo.toml:78-91` |
| F8 | JSON >sonic inherited from SK-V14, not re-measured (disclosed); comparator sound | — | CLEAN | `RESULTS.md:5-6`; `sonic_skipper.rs:3-11` |

**No single dispositive contrivance produces the >SOTA delta.** The delta survives: the
recognizer is a real structural parser, the rich rider does real per-node work (measurably
slower than 4-field), the oracle is independent, the numbers reproduce, no native tuning. The
**only** caveat is the materialization-depth asymmetry (F3) — which is a framing issue, not a
fabrication.

---

## §8 — Prune / course-correct recommendations

1. **(F3, MEDIUM — course-correct, do NOT prune)** In the >SOTA narrative
   (`HANDOFF.md:268-270`, RESULTS rows), state the comparison is **"lazy 9-field tape projection
   Mbps vs lightningcss eager full-CSSOM Mbps"**, not implied equal-work parity. The next cycle
   should either (a) add a lightningcss-side **lazy/skip comparator** (the symmetric fair bar:
   lightningcss tokenize-only, the way cssparser_token_scan already exists at
   `css_canon_bench.rs:118-121,282-403`), so the comparison plane is materialization-matched; or
   (b) keep the eager-vs-lazy comparison but rename the column `rich/lcss_full` and footnote the
   asymmetry. Option (a) is the honest >SOTA bar; (b) is the honest disclosure of the current
   one.
2. **(forward)** The `track1_full_parse` workload (`css_canon_bench.rs:103-106`) pays an FNV hash
   + String build it does not need for a speed measurement; it is the slowest workload and not
   the claimant, so harmless — but if it ever becomes a quoted number, strip the FNV/String from
   the timed region first.

## §9 — Inflection-point assessment

**This axis (bench/test honesty) is AT the generalization inflection point — YES, with one
narrative tightening.** The measurement infrastructure is sound, independent, cold, non-broadcast,
non-tautological, and portable. The SK-V16 contrivance family is genuinely eliminated. The
>SOTA delta is real. The next cycle can proceed as a generalization tranche on the
*measurement* axis; the one carry-forward is the F3 fairness-plane symmetry (a same-run lazy
lightningcss comparator), which is an enhancement, not a blocker.

## §10 — Forward-lens note for SK-V18 S-P0

Future >SOTA spec audits must add a **materialization-depth-parity** lens (call it
`NEW-CH5-V3-01`): when claiming "X beats competitor Y at producing Z", verify both X and Y
produce the *same depth of Z* (lazy-count vs eager-owned is a category difference). The
SK-V17 rich-vs-lcss comparison is honest in its sampling but asymmetric in its product; a
broadcast-detection lens alone (`CH5` legacy) would have passed it. The new lens asks: "does the
faster side do strictly less materialization than the comparator, and is that disclosed in the
ratio's name?" Apply it to any future "rich" or "full" >SOTA column.
