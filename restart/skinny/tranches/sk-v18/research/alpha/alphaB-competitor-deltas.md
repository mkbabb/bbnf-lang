---
artefact: αB SK-V17→SK-V18 competitor-delta extraction
pass: PASS-ALPHA cycle V5 (SK-V18 GENERALIZATION cycle)
agent: alphaB
tranche: sk-v18
subject: Per-grammar competitor deltas, honestly framed — the >SOTA bar SK-V18 must PRESERVE from grammar-DERIVED parsers
master_head: f6a38445b (SK-V17 closed)
audit_head: 7dbe44c22 (V3 audit committed)
cycle_head: 318d9c046 (V3 handoff committed; V4 + V5 entry — HEAD unchanged across the alpha loop)
host: aarch64 Apple M5 Max, stock release/ay-final (NO target-cpu=native dependence per V3 AGENT-5 §5), cold per-parse, N≥50 (JSON via criterion slope / N≥30 floor; CSS N≥200 close-median, N=80 live-repro cross-check)
binding_principle: SK-V18 is the GENERALIZATION cycle. The >SOTA delta below is NOT a target to grow — it is the BAR every grammar-DERIVED parser must PRESERVE through the un-fork/lowering rebuild. A grammar-derived parser that loses the delta is NOT done.
canonical_close_correction (V3 C2): CSS >SOTA is lazy-rich-summary vs eager-full-CSSOM — an ASYMMETRIC comparator. Stated as asymmetry below per H1, NOT papered as equal-work.
fold_v4_to_v5: cycle-V4 αB was ACCEPTed by ALL SEVEN CHALLENGE lenses with ZERO REVISE/REJECT on any αB section — re-confirming the V3 disposition (the bar is unchanged by definition). V4 explicit ACCEPTs: CH1 §αB ACCEPT ("correct strictness plane; serde/sonic figures verify; asymmetry disclosed; correctly excludes itself from the C.5 REVISE", CH1:351 + CH1:97-130); CH2 §2 ACCEPT ×2 ("αB tally: ACCEPT ×2, REVISE ×0", CH2:91-107,420); CH3 ACCEPT ("alphaB-competitor-deltas.md — ACCEPT", CH3:232,315); CH5 §2 ACCEPT ×4 (B.1 plane-symmetry / B.2 honest-None / B.3 Sheets-no-competitor / B.4 x86-comparator-OUT-vs-implementation-scope boundary, CH5:69-97,352); CH7 §2 ACCEPT ("alphaB overall: ACCEPT … the precise foreclosure of the corpus-in-timer / fabricated-competitor contrivance", CH7:121-143,318); CH4/CH6 carried no αB-specific cost/impact defect (CH4:10 names αB an out-of-cost-scope competitor axis; CH6 reviewed αB but raised no αB defect). The LONE V4 cohort REVISE was on αE — CH1:355 "αE Candidate Shortlist — REVISE: orphan V3 CH5 §C.5 (x86-scope) NOT folded crate-wide; `:93` P1 exit gate `src/`-scoped over live 3554-LOC `ext/x86/` + nasm `build.rs`". CH1 explicitly traces this through αB's own §6 fold-ledger pointer ("αB's §13 fold-ledger names the REVISE owners as 'αC §1 / αE P1 row / SYNTHESIS §2'", CH1:122-125) and concludes "αB's own disposition is correct; it is ACCEPT" (CH1:124-125). The αE x86-scope REVISE is an IMPLEMENTATION-prune-scope defect on the αE P1 exit gate; αB makes NO "x86 gone" close-claim (its §3.3 asmjson-AVX512-OUT line is the comparator face of the mandate, states the OUT only) — so αB inherits NO orphan from it. V5 FOLD: (1) carry the V4-ACCEPTed bar VERBATIM (it is a PRESERVE bar, unchanged by definition); (2) re-verify every cited number live at HEAD `318d9c046` (done §0/§1/§2 — apache_builds +1.4%, sonic strict-skipper posture, simd-json 0.13.11, the four N=200 CSS ratios, LOCKS.md:349 alphabet — ALL HOLD); (3) record the V4→V5 disposition resolution in §6, re-confirming αB carries no orphan REVISE and the αE x86-scope REVISE is non-αB. No claim weakened; no number changed.
prior_fold_v3_to_v4 (carried): cycle-V3 αB was likewise ACCEPTed by all seven lenses with zero αB REVISE; the V3 cohort REVISEs (αD "18→14 harnesses"; CH5 §C.5/§F.7 x86-scope on αC/αE/SYNTHESIS) did not orphan-touch any αB section. The §2.2 dual-N-plane discipline (V1→V2 cross-cohort note, CH1:71-76 / CH7:97-100) and the V2→V3 GoogleSheets canonicalization (CH2 §8.1 / CH3 §8) remain resolved and carried unchanged.
sources:
  - skinny/RESULTS.md (51 admitted JSON rows; parse_only tuples track1/track2/sonic/serde Mbps; apache_builds canary re-verified live)
  - restart/skinny/tranches/sk-v17/research/w5/skv17-W5-close-ledger.md §3,§7 (CSS N=200 close-median ledger — the headline PRESERVE bar; re-verified live)
  - restart/audit/skinny-impl-overfit/V3/AGENT-1-json-hardcoding.md (JSON >SOTA validity + strictness plane)
  - restart/audit/skinny-impl-overfit/V3/AGENT-5-bench-contrivances.md (CSS lazy-vs-eager asymmetry, live N=80 reproduction)
  - restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md (C1/C2/C3 corrections)
  - skinny/crates/bbnf-bench/src/sonic_skipper.rs:1-7 (strict skipper: IgnoredAny::deserialize + deserializer.end() — re-verified live)
  - skinny/crates/bbnf-bench/Cargo.toml:22-23 (simd-json 0.13.11 serde_impl; sonic-rs 0.5.8 default-features=false features=["sort_keys"] — strict, NOT utf8_lossy — re-verified live)
  - skinny/crates/bbnf-bench/src/gate.rs:41-42,344-345 (simd_json_borrowed_ns/owned_ns DOM comparator)
  - skinny/crates/bbnf-bench/src/report.rs:8,131,219 (SCHEMA_V3 columns; yyjson/asmjson/RapidJSON optional, unmeasured on aarch64)
  - skinny/crates/bbnf-bench/src/bin/{css_canon_bench.rs,w2_rich_cssom_bench.rs} (CSS canonical harness; N=200 close-median + N=80 live 2.145/2.905/1.911/1.975×)
  - restart/locks/LOCKS.md:349 (canonical Lock-14 alphabet: Json | CssL4 | Bbnf | GoogleSheets — third-grammar canonical name; re-verified live)
---

# αB — SK-V17→SK-V18 Competitor Deltas (the >SOTA bar to PRESERVE)

## 0. One-paragraph standing

SK-V17 closed at `f6a38445b` with **both grammars >SOTA on the measured, cold, real-corpus
plane** — JSON beats sonic-rs **strict** on every corpus (`parse_only`, unconditional), CSS
rich-summary beats lightningcss **1.996–3.348×** cold on the real 4-corpus set (N=200 W5-close
median). But the >SOTA was proven on **hand-written, forked parsers**, and the two comparisons
sit on **two different fairness planes**: JSON is a near-symmetric strict-vs-strict comparison
(both produce a recognized result, both reject malformed input); CSS is an **asymmetric
lazy-rich-summary vs eager-full-CSSOM** comparison (bbnf counts 9 aggregate fields lazily;
lightningcss builds an owned typed CSSOM). Both are measurement-VALID (V3 AGENT-5: cold, N≥50,
no broadcast, independent oracle, no native tuning), but the CSS delta is **not equal-work** and
MUST be framed as such (V3 C2 / H1). The SK-V18 job is NOT to grow either delta — the proof is
done. The job is to **preserve every delta below through the generalization** (un-fork generator,
grammar lowering, shared value trait, NEON wiring), proven on a third grammar (GoogleSheets). This
artefact fixes the bar: the precise per-corpus number each grammar-DERIVED parser must reproduce,
and the honest fairness plane each sits on. (All numbers below re-verified live at cycle-V4 HEAD
`318d9c046` against `RESULTS.md` + the W5 close ledger + the bench Cargo/skipper sources.)

**The asymmetry, stated up front (the binding honesty pin):**

| Grammar | Comparator | bbnf product | competitor product | Fairness plane |
|---|---|---|---|---|
| **JSON** | sonic-rs **strict** Skipper | full structural validate + tape (recognizer) | skip-validate (`IgnoredAny`+`.end()`) | **near-symmetric** — both recognize+validate; sonic skipper does NOT build owned tree, bbnf `parse_only` does NOT either (both recognition-plane) |
| **CSS** | lightningcss `StyleSheet::parse` | **lazy** 9-field aggregate count (zero payload writes) | **eager** owned typed CSSOM (full `Vec<CssRule>`) | **ASYMMETRIC** — bbnf is less work; honest framing = "lazy rich-summary beats eager full-CSSOM," NOT "CSSOM beats CSSOM" |

The JSON `parse_only` ↔ sonic `Skipper` pairing is the **cleanest** comparison in the campaign
(both recognition-plane, both strict). The CSS pairing is the one carrying the disclosed MEDIUM
caveat (V3 C2). SK-V18 H1 must either keep the asymmetric comparison RENAMED + footnoted, or
add a symmetric lightningcss-tokenize-only bar — both options recorded in §3.4.

---

## 1. JSON — the strict-vs-strict comparator bar (PRESERVE)

### 1.1 Comparator inventory + strictness plane

| Comparator | Plane | Strictness | Status in harness | Cite |
|---|---|---|---|---|
| **sonic-rs strict Skipper** | recognition (skip-validate) | **STRICT** — `IgnoredAny::deserialize` + `deserializer.end()` rejects trailing bytes; `default-features=false features=["sort_keys"]` (NO `utf8_lossy`) | LIVE, measured every JSON row (`sonic_mbps`) | `sonic_skipper.rs:1-7`; `Cargo.toml:23` |
| **sonic-rs lossy** | owned `Value` materialization | PERMISSIVE — `.utf8_lossy().deserialize::<Value>()` | LIVE, separate `sonic_rs_lossy` lane (flaw probe only, NOT the bar) | criterion `sonic_rs_lossy/metadata.toml` `parse_mode=from_slice_utf8_lossy` |
| **simd-json DOM** (borrowed + owned) | full DOM | strict | LIVE via criterion-ns (`simd_json_borrowed_ns`/`simd_json_owned_ns`) | `gate.rs:41-42,344-345` |
| **serde_json** | owned `Value` | STRICT (reference baseline) | LIVE, measured every row (`serde_mbps`) | RESULTS tuples |
| yyjson default / asmjson SWAR / asmjson AVX-512 / RapidJSON | — | — | **schema columns ONLY, unmeasured on aarch64** (C/C++ FFI not wired; AVX-512 is x86-only and OUT per aarch64 mandate) | `report.rs:8` SCHEMA_V3 header (all `Option<f64>`) |

**Binding clarity for SK-V18 telemetry:** the §4.3 PASS-ALPHA schema has columns for
yyjson/asmjson/RapidJSON, but **none are runnable on this aarch64 host** — they are honest
`None`. asmjson AVX-512 is x86-only and is permanently OUT (aarch64 mandate). The **runnable,
load-bearing JSON comparators are sonic-rs strict Skipper + serde_json (both measured every
row) + simd-json DOM (criterion-ns)**. The strict bar is sonic Skipper.

### 1.2 Per-corpus deltas — Track 1 (recognizer/tape) vs sonic-rs strict Skipper

Source: `skinny/RESULTS.md` `parse_only/main` tuples (`track1_mbps`/`sonic_mbps`, cold,
warmup_iters=0, per-iter strict equality PASS). Δ = (track1 − sonic)/sonic. (apache_builds row
re-verified live at HEAD: track1 13129.331 / sonic 12951.668 / serde 3964.266 → +1.4%.)

| Corpus | Bytes | Track 1 Mbps | sonic strict Mbps | serde Mbps | **Δ vs sonic-strict** | Bar classification |
|---|---:|---:|---:|---:|---:|---|
| twitter | 631,515 | 8349.290 | 4913.095 | 857.188 | **+69.9%** | wide |
| citm_catalog | 1,727,204 | 9079.838 | 8335.772 | 5121.472 | **+8.9%** | moderate |
| canada | 2,251,051 | 16709.901 | 12970.929 | 4581.994 | **+28.8%** | wide |
| apache_builds | 127,275 | 13129.331 | 12951.668 | 3964.266 | **+1.4%** | **THINNEST — the fragile row** |
| github_events | 65,132 | 8148.582 | 5014.433 | 1133.624 | **+62.5%** | wide |
| update_center | 533,178 | 5671.345 | 4707.613 | 665.541 | **+20.5%** | moderate |
| mesh | 723,597 | 11669.302 | 6589.818 | 4254.019 | **+77.1%** | wide |
| random | 510,476 | 3093.724 | 2937.264 | 597.523 | **+5.3%** | thin |
| marine_ik | 2,983,466 | 9505.490 | 5338.935 | 2252.199 | **+78.0%** | widest |
| instruments | 220,346 | 4281.770 | 3457.276 | 805.949 | **+23.9%** | moderate |
| numbers | 150,124 | 14472.308 | 7452.774 | 4761.151 | **+94.2%** | widest |
| unicode_mixed | 1,053,086 | 7379.340 | 7011.268 | 2816.812 | **+5.3%** | thin |
| unicode_escapes | 1,050,797 | 7897.449 | 2984.079 | 4347.844 | **+164.7%** | widest (escape-heavy) |
| unicode_basic | 1,048,586 | 9445.728 | 7059.901 | 1466.887 | **+33.8%** | wide |
| distinct_values | 153,630 | 5155.207 | 3233.781 | 577.906 | **+59.4%** | wide |
| y_string_unicode | 35,601 | 3169.901 | 2417.909 | 1699.763 | **+31.1%** | wide |

**Bar summary (JSON parse_only ↔ sonic strict):** Track 1 > sonic-strict on **16/16 corpora**.
Range **+1.4% (apache_builds, the thinnest) to +164.7% (unicode_escapes)**. Median delta
≈ **+30%**. Every margin is a STRICT-vs-STRICT recognition comparison (V3 AGENT-1 §JSON >SOTA
Validity: per-iter strict equality PASS vs independent sonic_rs/serde; sonic Skipper rejects
trailing bytes). **This is the bar the grammar-DERIVED JSON parser (G1: `json_sink_direct::render`
projecting from `SinkOnlyProgram`) must PRESERVE — every one of the 16 deltas above must hold
within noise after the parser is projected rather than templated.** (Range pin: +1.4%–164.7%,
the full RESULTS-aligned span — NOT the understated +1.4%–78% that CH1 corrected on αA/SYNTHESIS.)

**The fragile row (the canary):** `apache_builds +1.4%` is razor-thin. A grammar-derived
projection that adds even a few % overhead would flip this row sub-SOTA. **SK-V18 G1's
falsifiability gate must name apache_builds as a hard must-hold row.** (`random +5.3%`,
`unicode_mixed +5.3%` are the next-thinnest.)

### 1.3 simd-json DOM reference (literature plane, not strict-skip-symmetric)

V3 AGENT-1 §Margins records `canada parse_only ... (+45.4% vs simdjson DOM)` and
`citm direct_to_struct 33366 vs 21250`. The simd-json comparison is via criterion-ns
(`gate.rs:41-42`), NOT the per-row Mbps tuple, and DOM is a **different output plane**
(simd-json builds a borrowed/owned DOM; bbnf `parse_only` is recognition). It is a
favourable reference but NOT the strict bar — the strict bar is sonic Skipper (§1.2),
which is the recognition-plane match.

### 1.4 Track 2 (typed/structural) caveat — conditional, do NOT carry as the unconditional bar

The `direct_to_struct`/`real_typed_struct` rows (Track 2) show wider margins (e.g. citm
direct_to_struct 33366 vs sonic 21250) but ride a **per-corpus hand-tuned typed schema**
(`xtask/real_typed_schema.rs`, 1014-line per-corpus capacity literals — V3 AGENT-1 F-7).
Those deltas are a FAIR speed comparison (sonic deserializes into the same struct) but are
**conditional on a schema that does not generalize**. **The unconditional, generalizable JSON
>SOTA bar is `parse_only` ↔ sonic-strict (§1.2).** SK-V18 must NOT cite the typed rows as the
preservation bar unless PR-3 (grammar-derived schema, V3 AGENT-1 PR-3) lands first.

---

## 2. CSS — the lazy-rich-summary vs eager-full-CSSOM bar (PRESERVE, ASYMMETRY DISCLOSED)

### 2.1 Comparator inventory + materialization plane

| Comparator | Plane | Materialization depth | Status | Cite |
|---|---|---|---|---|
| **bbnf track1_rich** | 9-field aggregate count over recognizer tape | **LAZY** — zero payload writes, value-HEAD classification only | LIVE, the >SOTA claimant | `generated.rs:305-331`; `w2_rich_cssom_bench.rs` |
| **bbnf track1_4field** | 4-field structural count | LAZY (structural only) | LIVE (informational floor) | `css_canon_bench.rs` |
| **lightningcss** `StyleSheet::parse` | full owned typed L2 CSSOM | **EAGER** — owned `Vec<CssRule>`, typed `Property` variants | LIVE, the comparator (`ParserOptions::default()`) | `css_canon_bench.rs:113-116` |
| **cssparser** token scan | token stream | token-only, NO CSSOM | LIVE (spec admission floor, NOT the >SOTA bar) | `css_canon_bench.rs:118-121` |

**The asymmetry (V3 C2 / AGENT-5 §2, binding):** track1_rich reads the value HEAD only and
accumulates 9 `usize` counters, allocating nothing per node; lightningcss builds the full owned
CSSOM. They are **NOT equal-work**. The delta is real and reproducible, but it is
**"lazy 9-field projection Mbps vs eager full-CSSOM Mbps,"** not parity-of-product. Three facts
keep this MEDIUM (disclosed) not PRUNE-REQUIRED (fabrication): (a) the recognizer is a genuine
CSS structural parser, not a brace counter — it distinguishes at-rules/qualified-rules/decls,
handles strings/comments/escapes/balanced delimiters, byte-structurally equal to cssparser
(§2.3); (b) the rich rider costs measurable work — **~25–33% slower than the 4-field path**
(track1_4field 3106.6 vs track1_rich 2329.8 Mbps on bootstrap, V3 AGENT-5 §2.2), proving the
classification is executed not elided; (c) it visits every declaration's value head.

### 2.2 Per-corpus deltas — track1_rich vs lightningcss (the >SOTA bar) — BOTH N-planes

**FOLD (V2, per CH1:71-76 / CH7:97-100, CARRIED unchanged at V3 and V4):** the CSS bar exists on
two measured sample-size planes, both real, both cited. To prevent the downstream SPEC from
silently mixing them, BOTH are stated here. **The headline PRESERVE bar is the N=200 W5-close
median** (alphaA/SYNTHESIS-aligned, the SK-V17 close ledger); the **N=80 live reproduction (V3
AGENT-5) is the independent cross-check**. Both confirm 4/4 corpora cross lightningcss; the
canary-row identity DIFFERS between planes (see note below), and SK-V18 G2's falsifiability gate
must name the canary FROM THE PLANE IT GATES ON.

**Plane A — N=200 W5-close median (the headline PRESERVE bar):**
Source: `restart/skinny/tranches/sk-v17/research/w5/skv17-W5-close-ledger.md §3,§7` (re-verified
live at HEAD: bootstrap 2.210× / animate 2.355× / tailwind 3.348× / material 1.996×), harness
`w2_rich_cssom_bench`/`css_canon_bench`, cold, real corpus 71KB–495KB, N=200 median, distinct
per-corpus medians, no broadcast, independent 9-field cssparser oracle.

| Corpus | Bytes | rich-typed Track1 Mbps | lightningcss Mbps (eager full-CSSOM) | **rich/lcss** | Bar classification (N=200) |
|---|---:|---:|---:|---:|---|
| animate-4.1.1.min.css | 71,750 | 2937.9 | 1247.7 | **2.355×** | regular |
| bootstrap-5.3.3.min.css | 232,803 | 2473.1 | 1119.1 | **2.210×** | regular |
| tailwindcss-0.2.0.min.css | 179,631 | 2773.4 | 828.5 | **3.348×** | nested/utility — **WIDEST on N=200** |
| material-components-web-14.0.0.min.css | 495,454 | 2618.5 | 1312.0 | **1.996×** | irregular — **THINNEST on N=200 (the canary)** |

**Plane B — N=80 live reproduction (V3 AGENT-5 cross-check):**
Source: V3 AGENT-5 §1 live `w2_rich_cssom_bench 80` at HEAD `f6a38445b`; CONSOLIDATED-AUDIT A5
live re-reproduction (2.15/2.91/1.91/1.98×). Cold, per-parse, N=80, distinct medians, no broadcast.

| Corpus | Bytes | lightningcss Mbps (eager full-CSSOM) | **track1_rich / lcss** | Bar classification (N=80) |
|---|---:|---:|---:|---|
| animate-4.1.1.min.css | 71,750 | 1086.1 | **2.145×** | regular |
| bootstrap-5.3.3.min.css | 232,803 | 827.1 | **2.905×** | regular — widest on N=80 |
| tailwindcss-0.2.0.min.css | 179,631 | 1240.4 | **1.911×** | nested/utility — **THINNEST on N=80** |
| material-components-web-14.0.0.min.css | 495,454 | 1225.8 | **1.975×** | irregular |

**Bar summary (CSS rich-summary ↔ lightningcss):** track1_rich > lightningcss on **4/4 corpora on
BOTH planes**. Headline (N=200): **1.996×–3.348×** (geometric-mean ≈ 2.4×). Cross-check (N=80):
**1.911×–2.905×** (geometric-mean ≈ 2.2×). Both planes match the contract's "1.9–3.3×" framing
and confirm the same conclusion (4/4 cross). **This is the asymmetric bar SK-V18 must PRESERVE**
through G2 (retire `CSS_GENERATED_RS` const-string → grammar lowering) + G3 (un-fork generator)
+ G6 (NEON wiring). V3 G2 finding: the CSS >SOTA does **NOT depend on hand-shaping** — the hot
path is scalar, there is no fragile kernel to preserve — so the LOWERING rebuild is **LOW risk**
to the delta. (Contrast JSON G1, where apache_builds +1.4% is fragile.)

**The canary-plane divergence (the load-bearing FOLD note for the SPEC):** the tightest row is
NOT the same corpus on both planes — on **N=200 the canary is material at 1.996×** (the headline
bar), on **N=80 the canary is tailwind at 1.911×** (the short-rule-density adversary). Both are
real medians at different sample sizes; the lightningcss absolute Mbps differs ~10–35% between
runs (e.g. tailwind lcss 828.5 @N=200 vs 1240.4 @N=80), which is what moves the ratio ordering.
**SK-V18 G2's falsifiability gate must (a) gate on the N=200 close-median plane as the headline
PRESERVE bar — material ≥1.996× and tailwind ≥3.348× — and (b) carry tailwind as the structural
short-rule adversary regardless of plane.** The two sample-size planes must be named explicitly
in the SPEC, never silently substituted. Even at the thinnest observed ratio across both planes
(tailwind 1.911× @N=80 / material 1.996× @N=200), halving the rich-summary throughput keeps the
row >1× lightningcss — so the CSS bar has MORE headroom than JSON's apache_builds +1.4%.

### 2.3 The structural-equality guard (the honesty anchor under the asymmetric delta)

The asymmetric delta is only legitimate because the recognizer produces a **structurally
complete** parse, proven by an independent cssparser-driven oracle (V3 AGENT-5 §3, GENUINE —
different tokenization engine, same field counts). The 9-field equality holds on all 4 corpora.
This is the structural-honesty anchor: the lazy summary is over a REAL CSS structural parse, not
a brace count. **SK-V18 must re-prove this 9-field equality EXACT on the grammar-DERIVED CSS
recognizer before claiming the delta is preserved** — the equality is the bridge from the
hand-written parity oracle to the lowered output.

---

## 3. The honest framing the bar requires (H1 binding)

### 3.1 JSON framing — near-symmetric, clean

JSON `parse_only` ↔ sonic strict Skipper is the campaign's **cleanest** comparison: both are
recognition-plane, both strict (sonic rejects trailing bytes; bbnf per-iter strict equality vs
serde). Neither builds an owned tree in this lane. **No asymmetry caveat — state the +1.4%–164.7%
deltas plainly.** The only conditionality is the typed-struct lane (§1.4, schema-tuned), which is
NOT the bar.

### 3.2 CSS framing — asymmetric, MUST be stated

Per V3 C2/H1, the CSS >SOTA narrative MUST read **"bbnf lazy 9-field rich-summary projection
beats lightningcss eager full-CSSOM 1.996–3.348× cold on the real corpus (N=200 close median;
N=80 live cross-check 1.911–2.905×)"** — NOT "CSSOM beats CSSOM." The materialization-depth
difference is the load-bearing disclosure. SK-V18 RESULTS rows and HANDOFF prose must carry this
exact framing (with the N-plane named) or the H1 honesty gate fails.

### 3.3 What is NOT a runnable comparator on aarch64 (state as honest None)

- yyjson default / RapidJSON default: C FFI not wired — schema `None`.
- asmjson SWAR / asmjson AVX-512: x86-only family; AVX-512 permanently OUT (aarch64 mandate);
  schema `None`.
- The §4.3 schema carries these columns for forward-compat, but SK-V18 emits honest `None` —
  NOT a fabricated number. (CH1/CH5 honesty: a column populated with an un-run engine's number
  would be a contrivance. The §4.2 strict-comparator gate must therefore be read as "those
  comparators where runnable on aarch64," never as a demand for an un-run engine's number.)

**Cross-cohort note (V4):** this asmjson-AVX512-OUT line is the *comparator face* of the
aarch64-only mandate. It is correct and unchanged. It is distinct from the V3 CH5 §C.5/§F.7
REVISE — which targets the IMPLEMENTATION-side "x86 gone" close-claim (the P1 prune must widen to
`bbnf-simd/ext/x86/` + `build.rs`, not just `src/x86_64/`). αB makes NO "x86 gone" close-claim; it
only states that the x86-only comparator (asmjson AVX-512) is OUT of the comparator set. The two
do not conflict and αB carries no orphan from that REVISE (see §6).

### 3.4 H1 disposition options for SK-V18 (carry to αE/αF)

Per V3 AGENT-5 §8, the H1 honesty repair has two admissible forms:
- **Option A (the honest >SOTA bar):** add a lightningcss-side **lazy/tokenize-only comparator**
  (symmetric to the existing cssparser token-scan at `css_canon_bench.rs:118-121`), so the
  comparison plane is materialization-matched. This converts the asymmetric delta into an
  equal-work delta.
- **Option B (honest disclosure of current):** keep the eager-vs-lazy comparison but RENAME the
  column `rich/lcss_full` and FOOTNOTE the materialization asymmetry on every CSS row.

αF should specify which (A is the higher bar; B is the minimum honest framing). Either preserves
H1; silence does not.

---

## 4. The preservation bar (the SK-V18 binding output)

The >SOTA delta is NOT a growth target — SK-V18 is generalization, not new proof. The bar is:
**after the un-fork/lowering/shared-trait rebuild, the grammar-DERIVED parsers must reproduce
these deltas within cold-bench noise.** Per-grammar must-hold gates:

| Grammar | Must-hold bar | Canary row (tightest) | Falsifiability gate for SK-V18 | Risk to delta |
|---|---|---|---|---|
| **JSON** (G1 projection) | Track 1 > sonic-strict on **16/16** parse_only corpora, range +1.4%–164.7%, median ≈ +30% | **apache_builds +1.4%** (then random/unicode_mixed +5.3%) | G1 exit: all 16 deltas hold post-projection; apache_builds stays >0% strictly | MEDIUM — thin canary; projection overhead could flip apache_builds |
| **CSS** (G2 lowering + G3 un-fork + G6 NEON) | track1_rich > lightningcss on **4/4** corpora on the **N=200 close-median plane**, 1.996×–3.348× (N=80 live cross-check 1.911×–2.905×) | **material 1.996× (N=200 headline plane)**; tailwind is the structural short-rule adversary (1.911× @N=80) | G2 exit: 4/4 N=200 ratios hold within noise (material ≥1.996×, tailwind ≥3.348×); 9-field equality EXACT on grammar-derived recognizer; N-plane named explicitly, never mixed | LOW — V3 G2: delta does not depend on hand-shaping; scalar hot path; large headroom |
| **GoogleSheets** (PROVE, 3rd grammar) | NO competitor bar (no SOTA Sheets engine in scope) — the bar is **GENERATION**, not throughput | n/a | PROVE exit: the ONE generator emits a real GoogleSheets parser from `.bbnf` with a non-identical `generated.rs` (diff-census), 25-LOC stub retired | n/a — generation litmus, not a speed comparison |

**The asymmetry the bar carries (restated):** JSON's bar is a STRICT-vs-STRICT recognition
comparison (clean). CSS's bar is a LAZY-vs-EAGER materialization comparison (asymmetric, framed
per §3.2, headline N=200 plane / N=80 cross-check). GoogleSheets has NO competitor — its bar is
that the generator produces it at all (the generalization litmus). SK-V18 must preserve JSON's
clean delta, preserve CSS's asymmetric delta under the disclosed framing on the named N-plane, and
PROVE generality via GoogleSheets generation — not claim a Sheets speed win there is no comparator
for.

**Canonical-naming note (V2→V3 fold, carried V4):** the third grammar is named **`GoogleSheets`**
per the canonical Lock-14 alphabet (`Json | CssL4 | Bbnf | GoogleSheets`, `LOCKS.md:349`,
re-verified live at HEAD), NOT the abbreviated `Sheets`. The V2 cohort REVISE (CH2 §8.1 / CH3 §8)
sharpened the neutrality-grep alphabet to the un-abbreviated canonical form; this αB row is
SPEC-consistent with that fold (the generator emits `GoogleSheets` from
`google_sheets.bbnf`/`sheets.bbnf`, the `SheetsEventGrammar` witness is the inert phantom to
instantiate-or-delete per G4/D2). The bar is unchanged — only the name is canonicalized.

---

## 5. Citation ledger

- JSON per-corpus parse_only tuples (track1/track2/sonic/serde Mbps, cold, warmup_iters=0):
  `skinny/RESULTS.md` `json/<corpus>/parse_only/main` rows (apache_builds canary re-verified live
  at HEAD `318d9c046`: 13129.331 / 12951.668 / 3964.266 → +1.4%).
- sonic-rs strict Skipper (`IgnoredAny::deserialize` + `deserializer.end()`, rejects trailing):
  `sonic_skipper.rs:1-7` (re-verified live); strict feature posture `default-features=false
  features=["sort_keys"]` (no utf8_lossy): `skinny/crates/bbnf-bench/Cargo.toml:23` (re-verified
  live). Lossy lane separate (flaw probe): criterion `sonic_rs_lossy/metadata.toml`
  `parse_mode=from_slice_utf8_lossy`.
- simd-json 0.13.11 DOM comparator (criterion-ns, borrowed+owned): `Cargo.toml:22` (re-verified
  live); `gate.rs:41-42,344-345`. canada +45.4% vs simdjson DOM: V3 AGENT-1 §Margins.
- JSON >SOTA validity (cold, strict per-iter equality, no broadcast, parse_only unconditional /
  typed conditional on bench schema): V3 AGENT-1 §JSON >SOTA Validity, F-7.
- yyjson/asmjson/RapidJSON schema columns (Option, unmeasured on aarch64): `report.rs:8`
  SCHEMA_V3 header; `report.rs:131,219`.
- CSS per-corpus rich/lcss ratios — N=200 close-median (headline PRESERVE bar; bootstrap 2.210× /
  animate 2.355× / tailwind 3.348× / material 1.996×, distinct medians, no broadcast):
  `restart/skinny/tranches/sk-v17/research/w5/skv17-W5-close-ledger.md §3,§7` (re-verified live);
  reconciled in alphaA §2 / SYNTHESIS §0.
- CSS per-corpus rich/lcss ratios — N=80 live cross-check (animate 2.145× / bootstrap 2.905× /
  tailwind 1.911× / material 1.975×): V3 AGENT-5 §1; CONSOLIDATED-AUDIT A5 live re-reproduction
  (2.15/2.91/1.91/1.98×).
- CSS lazy-vs-eager asymmetry (zero payload writes; ~25–33% rich-vs-4field cost: 3106.6 vs
  2329.8 Mbps bootstrap; recognizer genuine structural parser): V3 AGENT-5 §2; CONSOLIDATED C2.
- CSS 9-field independent cssparser oracle (GENUINE, not tautology): V3 AGENT-5 §3.
- CSS >SOTA does NOT depend on hand-shaping (scalar hot path, LOW lowering risk): V3
  CONSOLIDATED G2; AGENT-2/AGENT-5.
- H1 framing repair (Option A symmetric comparator / Option B rename+footnote): V3 AGENT-5 §8.
- aarch64-only, no target-cpu=native dependence (stock release/ay-final): V3 AGENT-5 §5.
- asmjson AVX-512 x86-only OUT, aarch64 mandate: SK-V18 HANDOFF §0; CONSOLIDATED D3. (The
  implementation-side x86-deletion scope widening — `ext/x86/` + `build.rs` — is V3 CH5 §C.5/§F.7,
  an αC/αE/SYNTHESIS REVISE, NOT an αB section; αB makes no "x86 gone" close-claim, see §3.3/§6.)
- Canonical Lock-14 alphabet (`Json | CssL4 | Bbnf | GoogleSheets`; third-grammar canonical name):
  `restart/locks/LOCKS.md:349` (re-verified live); V2 CH2 §8.1 / CH3 §8 grep-alphabet REVISE.

## 6. V4→V5 fold record (CHALLENGE disposition resolution)

### 6.0 V4→V5 (this cycle)

Cycle-V4 αB was ACCEPTed by ALL SEVEN CHALLENGE lenses with ZERO REVISE/REJECT on any αB
section — re-confirming the V3 disposition. Because αB's bar is a PRESERVE bar (unchanged by
definition once the >SOTA is fixed), the V4→V5 fold is verbatim-carry + live re-verification:

| V4 lens | αB disposition | Citation |
|---|---|---|
| **CH1 §αB** | **ACCEPT** | CH1:351 "correct strictness plane; serde/sonic figures verify; asymmetry disclosed; correctly excludes itself from the C.5 REVISE"; CH1:97-130 full review; CH1:124-125 "αB's own disposition is correct; it is ACCEPT" |
| **CH2 §2** | **ACCEPT ×2** | CH2:107 "αB tally: ACCEPT ×2, REVISE ×0"; CH2:420 (2/0/0); three-grammar bar table holds, GoogleSheets canonicalization SPEC-consistent |
| **CH3** | **ACCEPT** | CH3:232 "alphaB-competitor-deltas.md — ACCEPT"; CH3:315 (αB among five sections passing the CH3 lens at V4) |
| **CH4** | no αB defect | CH4:10 "alphaB/alphaD are competitor/ledger axes — only their cost-bearing rows are in scope"; no αB cost defect raised |
| **CH5 §2** | **ACCEPT ×4** | CH5:97 "αB tally: ACCEPT ×4"; B.1 plane-symmetry / B.2 honest-None / B.3 Sheets-no-competitor / B.4 x86-comparator-OUT-vs-implementation-scope boundary (CH5:69-97); CH5:352 |
| **CH6** | no αB defect | CH6:12 reviewed αB; no αB-specific impact defect raised |
| **CH7 §2** | **ACCEPT** | CH7:121-143 "alphaB overall: ACCEPT … §3.3 honest-`None` posture is the precise foreclosure of the corpus-in-timer / fabricated-competitor contrivance"; CH7:318 |

**There was NO orphan REVISE on αB to resolve at V4.** The LONE V4 cohort REVISE was on **αE**
(CH1:355): the αE P1 exit gate (`alphaE:93`) is `src/`-scoped and false-greens over the live
~3554-LOC `bbnf-simd/ext/x86/` vendored ASM + the nasm `build.rs` driver — i.e. the V3 CH5
§C.5/§F.7 x86-deletion-scope REVISE was not folded crate-wide on the αE side. CH1 reached that
αE orphan **by following αB's own §6 fold-ledger pointer** (CH1:122-125: "αB's §13 fold-ledger
names the REVISE owners as 'αC §1 / αE P1 row / SYNTHESIS §2'") and then explicitly confirmed
"αB's own disposition is correct; it is ACCEPT." The αE REVISE is an IMPLEMENTATION-prune-scope
defect on an αE gate; **αB makes no "x86 gone" close-claim** — its §3.3 asmjson-AVX512-OUT line is
the *comparator face* of the aarch64 mandate (states the x86-only comparator is OUT of the
comparator set, makes no implementation close-claim). αB therefore inherits NO orphan from the αE
REVISE; the boundary CH5 §B.4 named at V4 (CH5:91-94) is correct.

**V5 FOLD actions taken:** (1) carried the V4-ACCEPTed bar VERBATIM — no measurement, ratio,
plane, canary, or framing altered (a PRESERVE bar is unchanged by definition); (2) re-verified
every cited number live at HEAD `318d9c046` (HEAD is identical to the V4 cycle_head — the alpha
loop did not advance master): sonic strict skipper (`IgnoredAny::deserialize` + `deserializer.end()`,
`sonic_skipper.rs:1-7`) + strict feature posture (`default-features=false features=["sort_keys"]`,
no `utf8_lossy`, `Cargo.toml:23`); simd-json `=0.13.11` (`Cargo.toml:22`); apache_builds canary
`13129.331 / 12951.668 / 3964.266 → +1.4%` (`RESULTS.md:14,70`); the four N=200 CSS ratios
(bootstrap 2.210× / animate 2.355× / tailwind 3.348× / material 1.996×, W5-close-ledger §3 lines
99-102,212-213); LOCKS.md:349 canonical alphabet (`JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser`)
— **ALL HOLD**; (3) recorded this V4→V5 disposition resolution. No claim weakened; no number changed.

### 6.1 V3→V4 fold record (carried, for continuity)

Cycle-V3 αB was ACCEPTed by ALL SEVEN CHALLENGE lenses with ZERO REVISE/REJECT on any αB
section:
- **CH1 §αB — ACCEPT** ("correct strictness plane; serde/sonic figures verify; asymmetry
  disclosed"; the instruments +23.9% vs αA +23.8% rounding both defensible).
- **CH2 §2 — ACCEPT ×2** ("αB was already CH2-clean"; the three-grammar bar table holds; the
  GoogleSheets canonicalization is SPEC-consistent with the grep alphabet, no number changed).
- **CH3 §αB — ACCEPT** (7/0/0 on the CH3 axis; the CSS bar is framed ASYMMETRIC; a fabricated
  competitor column is REJECTed at the αE close gate, αB carries the honest-None foreclosure).
- **CH5 §B — ACCEPT (all):** B.1 §0/§3 plane-asymmetry (no cross-plane coupling); B.2 §1.4/DM1
  typed-row conditionality (typed rows quarantined, not the preservation bar); B.3 §4 Sheets
  no-competitor-bar (Sheets bar is GENERATION not throughput).
- **CH7 §2 — ACCEPT** ("the §3.3 honest-`None` posture is the precise foreclosure of the
  corpus-in-timer / fabricated-competitor contrivance"; §6 fold record accurate; canonicalization
  sound vs LOCKS.md:349).
- **CH4 / CH6** — carried no αB-specific cost/impact defect.

**There was NO orphan REVISE on αB to resolve.** The V3 REVISEs in the cohort were on OTHER
artefacts and do not touch any αB section:

| V3 REVISE | Owning section | αB touch? | V4 resolution |
|---|---|---|---|
| αD §1 V4 "18 differential harnesses" → disk-true 14 (CH1 §αD, CH4 §6-carry, CH7 §4) | αD validated-invalidated ledger | NONE — αB cites no checkasm-harness count | No αB action. (The SYNTHESIS/HANDOFF already carry the 18→14 correction per CH4/CH7; the αD-side fix is an αD-author action.) |
| CH5 §C.5/§F.7 (NEW V3): P1 x86-deletion scope omits `bbnf-simd/ext/x86/` (~3000 LOC vendored ASM) + `build.rs` (nasm driver); "x86 gone" literally false until P1 widens crate-wide | αC §1 / αE P1 row / SYNTHESIS §2 (implementation-side prune scope) | NONE — αB makes no "x86 gone" close-claim; it only states the x86-only comparator (asmjson AVX-512) is OUT of the comparator set | No αB number/claim change. §3.3/§5 add a cross-cohort note distinguishing the comparator-OUT (αB, correct) from the implementation-prune-scope REVISE (αC/αE/SYNTHESIS) so the SPEC does not conflate them. |

**V4 FOLD actions taken:** (1) carried the V3-ACCEPTed bar VERBATIM (it is a PRESERVE bar,
unchanged by definition — no measurement, ratio, plane, canary, or framing altered); (2)
re-verified every cited number live at HEAD `318d9c046` (§0/§1/§2: sonic strict skipper +
features, simd-json version, apache_builds canary +1.4%, the four N=200 CSS ratios, LOCKS.md:349
alphabet — all hold); (3) added the §3.3/§5/§6 cross-cohort note isolating the αB comparator-OUT
from the V3 CH5 implementation-prune-scope REVISE, so αB stays SPEC-consistent without absorbing a
REVISE that is not its own. The §2.2 dual-N-plane discipline (the lone V1→V2 cross-cohort note,
CH1:71-76 / CH7:97-100) remains resolved and is carried unchanged: N=200 close-median is the
headline PRESERVE bar; N=80 live is the cross-check; the canary-plane divergence is documented; the
SPEC must name the N-plane it gates on, never silently mixing them. The V2→V3 GoogleSheets
canonicalization (CH2 §8.1 / CH3 §8) remains the third-grammar name; re-verified against
`LOCKS.md:349`.
