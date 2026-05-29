# alphaB — CSS Competitor Deltas (SK-V16 → SK-V17, cycle V4)

Pass: Pass Alpha. Cycle SK-V16 → SK-V17. Agent alphaB.
Scope (per PASS-ALPHA §2 α-B, this dispatch): compute per-corpus CSS deltas vs every
**materializing** comparator. Disclose output plane + workload + strictness plane for
each comparator row. Distinguish the fair >SOTA bar (lightningcss full-CSSOM) from the
non-fair token-scan comparator (cssparser), and note where lightningcss itself sits vs
cssparser.

Host discipline: aarch64 Apple M5 Max only. x86/AVX-512 rows are diagnostic, never
admission. Every number below cites a RESULTS row, an audit file path:line, a commit SHA,
or the contract-supplied canonical bench. Uncited claims are CH1/CH6-rejectable.

**Cycle-V2 FOLD note (CH1-R4 + per-corpus REVISE dispositions).** Three V1 defects are
resolved here: (1) the per-corpus endpoint-to-corpus mappings (animate↔~164, tailwind↔~51,
material↔~60) and the per-corpus gap multiples are **INFERRED**, not measured — every
inferred cell is now marked `[INF]` **inline** in the table body, not only in a trailing
footnote, so no downstream artefact lifts an inferred multiple as a measured figure;
(2) the endpoint mapping is tagged **UNMEASURED-PENDING** and §6 forbids any SK-V17 wave
exit-gate from keying on an inferred per-corpus endpoint until the N≥50 harness emits the
real split; (3) a benched-substrate disclosure is added (§0.1) — the benched CSS "Track 1"
today is a **fact-stream `String`** (`track1::parser::parse -> Result<String,String>`,
`nonjson_css_l4.rs:596`), NOT a materialized typed CSSOM; the "typed CSSOM" plane this
artefact compares is the *intended* SK-V17 subject (the de-fact-streamed path α-E C0 must
build), and the gap arithmetic is computed on the contract-canonical numbers which already
correspond to the parse-throughput plane, not the fact-stream serialization cost.

**Cycle-V4 FOLD note (broadcast-count reconciliation, count-correction residue).** This
artefact computes its deltas over the **4-file benched corpus set** (§1), not over the
RESULTS.md CSS broadcast rows, so the broadcast-count reconciliation does not change any
number here. A one-line cross-artefact reconciliation note is nonetheless carried in the
verification ledger so this artefact's count remains in verbatim agreement with the cohort:
the grep-true count of falsified CSS broadcast rows in `skinny/RESULTS.md` is **24**
(`grep -c AUDIT-FALSIFIED skinny/RESULTS.md` = 24, lines 112–135;
`grep -c 'css_l4/.*/direct_to_struct/main'` = **25**, of which 24 are `^| css_l4/`
table rows 112–135 and the 25th, at `:154`, is a prose REDRESS-127 companion reference,
NOT a table row; `grep -nE 'W6.*css|tape.*direct_to_struct'` is EMPTY — there is NO
admitted/distinct W6 typed CSS row in RESULTS.md). **All cohort artefacts state 24 /
lines 112–135 as of V3** (αA §reconciliation-note, αC §4/§7, αD §0/§5, SYNTHESIS §0.2,
HANDOFF Current-State); the V2 "6" undercount is resolved cohort-wide. The earlier V3
phrasing here — that alphaA was the only sibling stating 24 and the others undercounted
as "6" — is itself now stale and is struck (all four already read 24).

---

## 0. The plane taxonomy (read this before any delta)

CSS has THREE comparators in flight. They do NOT occupy the same output plane, so a raw
Mbps ratio between them is meaningless without disclosing what each one *produces*. This
is the CSS analogue of the SK-V6 sonic-rs `utf8_lossy` finding: a faster comparator that
retains less is not a fair >SOTA bar.

| Comparator | Output plane | What it materializes | Workload | Fair bar for bbnf Track 1? |
|---|---|---|---|---|
| **bbnf CSS Track 1** | typed CSSOM (rich AST) — *intended SK-V17 subject; see §0.1* | full typed value tree: Dimension/Color/Function/List/Selector/Rule | `track1::parser::parse` + typed-value visit | (subject under test) |
| **lightningcss `StyleSheet::parse`** | full L2 CSSOM | complete semantic stylesheet: rules, selectors, declarations, typed values | full parse to owned CSSOM | **YES — this is THE >SOTA bar** |
| **cssparser** | token stream only | flat token iteration; NO rule/selector/declaration tree, NO typed values | token-scan | **NO — not a materializing comparator; plane-mismatched** |

The fair >SOTA target for SK-V17 is **lightningcss full-CSSOM (~974 Mbps canonical;
793–833 Mbps in the cited W6 runs)**, because it is the only comparator that builds a
retained, materialized, semantically-rich product on the same plane as bbnf Track 1's
*intended* typed CSSOM. cssparser (~2539 Mbps canonical; 2476–2529 in cited runs) is a
token scanner that retains nothing structural; it is the permissive flaw-probe of the CSS
domain. Beating cssparser is NOT the SK-V17 SOTA condition; beating lightningcss is.

Note the in-tree contradiction this resolves: the SK-V16 SPEC admission gate names
**cssparser** as the Track 1 admit comparator (typed-summary equality vs cssparser, 8-field
structural). That gate is a *correctness/parity* gate (does Track 1 see the same
rules/style/sel/decl counts), NOT a speed >SOTA gate. The SK-V16 W6 reports correctly
separate these:
`restart/audit/skinny-impl-overfit/sk-v16-w6-speed-report.md:55-60` reports cssparser as
the "SPEC admission gate" for *structural equality* while naming lightningcss as "the
user's real bar" for *speed*. SK-V17 must keep this split: cssparser = parity oracle +
plane-disclosed reference; lightningcss = the speed bar to beat.

### 0.1 Benched-substrate disclosure (load-bearing; benched tree = skinny, not core)

Per the alphaE §0 translation correction (`alphaE-candidate-shortlist.md:37-51`,
grep-verified: no `StructLayout`/`OpenFrame`/`CssArena` in `skinny/crates/`), the benched
CSS path is the **skinny tree**, and what it benches today is NOT a materialized typed
CSSOM. The benched "Track 1" entry point is:

```
pub fn track1_facts(input: &str) -> Result<String, String> {
    track1::parser::parse(input).map_err(|error| error.to_string())
}                                          // nonjson_css_l4.rs:596
```

— a **fact-stream `String`** producer (emitted via `emit_fact_stream`,
`css_l4_*/generated.rs:5`; CSS rides `RuntimeEmitterKind::RequestFacts`, JSON rides
`CompiledLowering`). The separate `css_l4_w6_typed_retime` path that reports 3.093 Mbps
(`sk-v16-w6-speed-report.md:58`) is the typed-CSSOM retime, **not** the benched roster row.

Consequence for this artefact: the "typed CSSOM (rich AST)" plane in the §0 taxonomy is
the **intended SK-V17 subject** that α-E candidate C0 must build (de-fact-stream the
benched Track 1 onto a typed view over the skinny `Tape`/`ValueRef` substrate,
`runtime/src/tape/mod.rs`). The gap arithmetic below uses the contract-canonical
parse-throughput numbers (~70 / ~974 / ~2539), which correspond to full-parse throughput,
not the cost of the `String` serialization. The plane the deltas are *computed against*
is honest; the plane the benched row *currently emits* is disclosed here so no reader
mistakes "Track 1 typed CSSOM" for an already-materialized benched product. SK-V17 wave 0
re-baseline must measure the de-fact-streamed typed path on the N≥50 harness.

---

## 1. Canonical baseline (the ground truth this delta is computed against)

Per the SK-V17 dispatch contract, canonical cold bench at **master 1c5bd7a25** (verified
HEAD: `git log --oneline -1` → `1c5bd7a25 feat(sk-v16-W6-tape): add shared flat-tape
runtime substrate`), N=100 cold samples, full CSS L4 corpus:

- **bbnf CSS typed Track 1: ~70 Mbps** full corpus (range **51–164 Mbps per corpus** —
  contract-supplied range; the per-corpus split is NOT separately published, see §2).
- **lightningcss full-CSSOM: ~974 Mbps** (contract canonical; 793–833 in cited W6 runs).
- **cssparser token-scan: ~2539 Mbps** (contract canonical; 2476–2529 in cited W6 runs).

Corpus (`skinny/crates/bbnf-bench/src/css_l4_corpus.rs:23-50`, total **979,638 B**,
4 files — this is the authoritative benched set, NOT the manifest prose; `normalize` is
NOT in the benched set):

| Corpus id | File | Bytes | Structural character |
|---|---|---:|---|
| bootstrap | `bootstrap-5.3.3.min.css` | 232,803 | utility + component framework; moderate rule density |
| tailwindcss | `tailwindcss-0.2.0.min.css` | 179,631 | utility-first; **deeply many short rules** (hardest) |
| material-components-web | `material-components-web-14.0.0.min.css` | 495,454 | component lib; deep nesting, many functions/colors |
| animate | `animate-4.1.1.min.css` | 71,750 | keyframes; **most structurally regular/repetitive** (easiest) |

**Measurement-honesty caveat (load-bearing for SK-V17).** The numbers vary by run and
thermal state across the SK-V16 reports — the same path is reported at 3.093 Mbps (typed
retime, `sk-v16-w6-speed-report.md:58`, single-sample), ~69.668 Mbps (scrutineer cold,
`sk-v16-w6tape-report.md:47`), ~13–15 Mbps (build-machine cold, `:47`). These are NOT
contradictions of architecture; they are **statistical inadequacy of the W6 harness**,
which runs single-sample cold convention (a single `Instant`/elapsed around one full-corpus
parse, `sk-v16-w6-speed-report.md:60`). The SK-V16 W6 conversion report states the
per-corpus single-sheet was **NOT separately benched** ("there is no tape CSS path to
compare against, so the corpus number is the only honest baseline",
`sk-v16-w6tape-report.md:51`). The SK-V17 telemetry MUST use **N≥50 cold samples + median**
(per the §4.3 schema), or every delta computed here carries unquantified run-to-run noise.
This artifact uses the contract-supplied canonical N=100 figures (~70 / ~974 / ~2539) as
the authoritative baseline; the per-run scatter is disclosed but not used for the delta
arithmetic.

---

## 2. Per-corpus delta vs lightningcss (THE fair >SOTA bar)

Plane: lightningcss = full L2 CSSOM (materializing). bbnf Track 1 = typed CSSOM
(materializing, intended SK-V17 subject per §0.1). **Plane-matched → fair comparison.**
Workload: full parse, both cold, both over the identical corpus file. Strictness: both
strict (full semantic parse; neither is a permissive token skip).

The canonical figures are full-corpus aggregates (~70 Track 1, ~974 lightningcss). The
per-corpus split of Track 1 (51–164 Mbps) is the contract-supplied measured *range*; the
per-corpus lightningcss split is NOT separately published in the cited reports — only the
~974 corpus aggregate and the per-run scatter (793–833 Mbps,
`sk-v16-w6-speed-report.md:58`, `sk-v16-w6tape-report.md:47`). Therefore the per-corpus
deltas below pair an **inferred** Track 1 per-corpus endpoint against the lightningcss
**corpus aggregate**; **every inferred cell is marked `[INF]` inline.**

`[INF]` = INFERRED, confirm at N≥50. `[AGG]` = corpus aggregate (only published level).
`[RNG]` = contract-published range endpoint (51–164), but its corpus assignment is `[INF]`.

| Corpus | bbnf Track 1 Mbps | lightningcss Mbps (plane: full CSSOM) | Track 1 / lcss | Δ (Track 1 below lcss) | Gap multiple |
|---|---:|---:|---:|---:|---:|
| animate (easiest, regular) | ~164 `[RNG][INF assign]` | ~974 `[AGG]` | ~16.8% `[INF]` | ~−83.2% `[INF]` | ~5.9× slower `[INF — confirm at N≥50]` |
| bootstrap | ~70 `[INF ≈ agg-mid]` | ~974 `[AGG]` | ~7.2% `[INF]` | ~−92.8% `[INF]` | ~13.9× slower `[INF — confirm at N≥50]` |
| material-components-web | ~60 `[INF est. mid-low]` | ~974 `[AGG]` | ~6.2% `[INF]` | ~−93.8% `[INF]` | ~16.2× slower `[INF — confirm at N≥50]` |
| tailwindcss (hardest, dense) | ~51 `[RNG][INF assign]` | ~974 `[AGG]` | ~5.2% `[INF]` | ~−94.8% `[INF]` | ~19.1× slower `[INF — confirm at N≥50]` |
| **CORPUS AGGREGATE** | **~70** `[AGG cited]` | **~974** `[AGG cited]` | **~7.2%** | **~−92.8%** | **~13.9× slower** `[cited: "reproducibly ~14x"]` |

**Reading.** Only the **aggregate row is cited**: Track 1 is reproducibly **~14× slower
than lightningcss on the corpus aggregate** (contract: "reproducibly ~14x slower";
cross-checked `sk-v16-w6tape-report.md:47` track1/lcss=0.0878 → ~11.4× scrutineer-run, and
0.220 → ~4.5× build-run, run-scatter bracketing the ~14× canonical). Every per-corpus row
is **`[INF]`**: the per-corpus endpoint-to-corpus assignment follows the structural logic
(regular/repetitive corpora amortize speculative dimension-dispatch and allocation cost
better; tailwind's short-rule density maximizes per-rule fixed overhead), consistent with
the architecture synthesis flagging tailwind as the last corpus to cross
(`sk-v16-css-sota-tape-architecture.md:351-355`). But it is **inferential from the 51–164
range + corpus character**, not a per-corpus published number.

**UNMEASURED-PENDING.** The entire per-corpus column (endpoints AND gap multiples) is
UNMEASURED-PENDING. SK-V17 wave 0 re-baseline must emit the actual per-corpus Track 1 AND
per-corpus lightningcss split on the N≥50 harness before any of these cells is treated as
measured. **No SK-V17 wave exit-gate may key on an inferred per-corpus endpoint** (see §6).

---

## 3. Per-corpus delta vs cssparser (NOT a fair materializing comparator)

Plane: cssparser = token stream only (no rule/selector/decl tree, no typed values).
**Plane-MISMATCHED** against bbnf Track 1's typed CSSOM. This row is disclosed as a
reference/parity comparator, NOT a >SOTA bar. Beating cssparser is not the SK-V17 win
condition — it cannot be, because cssparser does strictly less work (it retains nothing).

Same marker convention as §2: per-corpus rows are `[INF]`; only the aggregate is cited.

| Corpus | bbnf Track 1 Mbps | cssparser Mbps (plane: token-scan) | Track 1 / cssparser | Gap multiple | Plane note |
|---|---:|---:|---:|---:|---|
| animate | ~164 `[INF]` | ~2539 `[AGG]` | ~6.5% `[INF]` | ~15.5× `[INF — confirm at N≥50]` | token-scan retains no tree |
| bootstrap | ~70 `[INF]` | ~2539 `[AGG]` | ~2.8% `[INF]` | ~36.3× `[INF — confirm at N≥50]` | token-scan retains no tree |
| material-components-web | ~60 `[INF]` | ~2539 `[AGG]` | ~2.4% `[INF]` | ~42.3× `[INF — confirm at N≥50]` | token-scan retains no tree |
| tailwindcss | ~51 `[INF]` | ~2539 `[AGG]` | ~2.0% `[INF]` | ~49.8× `[INF — confirm at N≥50]` | token-scan retains no tree |
| **CORPUS AGGREGATE** | **~70** `[AGG cited]` | **~2539** `[AGG cited]` | **~2.8%** | **~36.3× slower** | **token-scan retains no tree** |

**Reading.** Track 1 is **~36× slower than cssparser on the corpus aggregate** (aggregate
cited; per-corpus `[INF]`). This gap is LARGER than the lightningcss gap precisely because
cssparser does less work. The honest framing: of cssparser's ~36× margin, a large fraction
is the plane difference (token-scan vs full typed CSSOM), not raw scanner superiority. The
SK-V16 W6 reports confirm cssparser is the structural-equality oracle
(rules=10136/style=9561/sel=9561/decls=20043 all `track1 == cssparser`,
`sk-v16-w6-speed-report.md:102`) — i.e. cssparser is used to *check that Track 1 sees the
same structure*, on a different output plane than it speeds. Using cssparser's Mbps as a
speed bar would be the CSS equivalent of benchmarking against sonic-rs lossy:
plane-dishonest. SK-V17 must report cssparser deltas WITH the plane disclosure, never as
the SOTA condition. The reasoning here (~36×, plane-mismatched, NOT the SOTA bar) is
correct and aggregate-cited; only the per-corpus split is `[INF]`.

---

## 4. Where lightningcss itself sits vs cssparser (the inter-comparator relation)

This is the relation the contract asks to note, and it is the key to understanding why
lightningcss is the fair bar:

- lightningcss ~974 Mbps / cssparser ~2539 Mbps = **lightningcss is ~38% of cssparser's
  throughput (~2.6× slower than cssparser)**. Cross-checked on the cited single run:
  793.326 / 2529.390 = 0.314 (~3.2×, `sk-v16-w6tape-report.md:47`); 833.199 / 2476.472 =
  0.336 (~3.0×, `sk-v16-w6-speed-report.md:58`). The canonical ~2.6× and the run-scatter
  ~3× bracket the same materialization tax.
- This is exactly the expected plane tax: lightningcss builds a full materialized CSSOM;
  cssparser only emits tokens. lightningcss pays ~2.6–3× for materialization that
  cssparser never does. (This is a cited inter-comparator relation, not inferred.)

**Implication for the bbnf target.** The ~2.6–3× lightningcss↔cssparser gap is the
*materialization tax that a SOTA full-CSSOM parser legitimately pays*. bbnf Track 1, once
de-fact-streamed to a materializing typed CSSOM (§0.1), should be measured against the
materializing comparator (lightningcss), not the token scanner (cssparser). If bbnf Track 1
reaches lightningcss (~974 Mbps) it has matched SOTA full-CSSOM; reaching cssparser (~2539)
would mean out-materializing a pure tokenizer, which is a higher and arguably
plane-incoherent bar. **SK-V17's >SOTA condition is therefore: beat the same-run measured
lightningcss full-CSSOM median (~974 Mbps prior-run reference) on as many corpora as
possible, with the rich typed CSSOM preserved.** This is consonant with the architecture
synthesis's own honest framing — the **300–600 Mbps band** is the expected first-cross, and
the doc states the model is *capable* of crossing **754 Mbps** on structurally-regular
corpora, with tailwind (deeply nested, many short rules) the hardest and likely to land
short on the first pass (`sk-v16-css-sota-tape-architecture.md:347-355`, which names
normalize/bootstrap as the regular crossers; **normalize is NOT in the 4-file benched set
(§1)**, so the benched regular-corpus analogue is **animate**). The arch doc's 754 figure
predates the contract-canonical lightningcss ~974; the SK-V17 close bar is the **same-run
re-baselined lightningcss median (Wave 0, N≥50)**, not either literal — ~974 is the
prior-run reference and 754 the arch doc's earlier estimate of the regular-corpus ceiling.

Plane-disclosure summary (the SK-V17 telemetry must carry this verbatim per row):

| Comparator | Mbps (corpus agg) | Plane | Materializing? | Strictness | Role in SK-V17 |
|---|---:|---|---|---|---|
| bbnf CSS Track 1 | ~70 | typed CSSOM (rich AST) — benched today as fact-stream `String`, §0.1 | yes (intended) | strict (full parse) | subject under test |
| lightningcss | ~974 | full L2 CSSOM | yes | strict (full parse) | **fair >SOTA bar** |
| cssparser | ~2539 | token stream | no | strict-token | parity oracle / plane-disclosed reference, NOT a speed bar |

---

## 5. JSON comparator guard (unchanged carry-forward, for completeness)

CSS is the SK-V17 subject, but the JSON comparator plane must not regress. The 51 strict
JSON rows are the guard baseline (`skinny/RESULTS.md` rows 5–24+ all `A`/`GO`/strict,
e.g. `twitter/parse_only` Track 1 8349.290 > sonic-strict 4913.095, +69.9%;
`citm_catalog/real_typed_struct` 20512.601 > sonic-strict 12662.292, +62.0%). Any SK-V17
wave touching the shared tape/projection substrate (skinny `runtime/src/tape/`,
`codegen/src/lower/`) must re-run the touched JSON rows on the same strict plane (sonic-rs
strict, serde_json reference) and preserve Track 1 / Track 2 independence (Lock 1, no
Track1≡Track2 dishonesty). JSON comparators (sonic-rs strict / simdjson DOM / yyjson /
serde_json) are all materializing and plane-matched and remain the JSON-domain bars; they
are not re-derived here as CSS is the scope.

---

## 6. Findings → SK-V17 goalset feed (for α-E / α-F)

1. **The fair >SOTA bar is lightningcss, measured same-run (~974 Mbps prior-run
   reference), corpus aggregate.** Track 1 ~70 Mbps = **~7.2% of lightningcss, ~14× slower**
   (corpus agg, cited). This is the single number the SK-V17 close condition brackets. The
   close threshold is the **same-run re-baselined lightningcss median (Wave 0), N≥50** —
   NOT a fixed 974; ~974 is the prior-run reference.

2. **Per-corpus dispersion is UNMEASURED-PENDING; it MUST NOT gate any wave until the N≥50
   split exists.** The §2/§3 per-corpus endpoints and gap multiples are `[INF]`. The
   structural reasoning (smallest gap likely animate ~6× regular/JSON-isomorphic; widest
   likely tailwind ~19× dense short rules) is a *targeting hypothesis*, not a measured
   fact. **Wave exit-gates may key only on the corpus AGGREGATE crossing the same-run
   lightningcss median, or on a per-corpus endpoint AFTER Wave 0 emits the measured split.**
   No exit-gate may be written against animate↔164 / tailwind↔51 / material↔60 as if
   measured. The §4.4 wave gates should *target* animate/bootstrap crossing first as a
   hypothesis, and treat tailwind as the honest-residual corpus, but the *gate condition*
   binds to measured per-corpus numbers only.

3. **cssparser (~2539) is NOT the speed bar.** It is plane-mismatched (token-scan, retains
   no tree). It stays the structural-equality parity oracle (8-field, all equal at
   `sk-v16-w6-speed-report.md:102`) and a plane-disclosed reference only. SK-V17 telemetry
   must carry the plane column so no future row mistakes the ~36× cssparser gap for a SOTA
   condition.

4. **lightningcss sits at ~38% of cssparser (~2.6–3× slower)** — the legitimate
   materialization tax (cited inter-comparator relation, §4). bbnf Track 1, also
   materializing once de-fact-streamed (§0.1), is correctly compared to lightningcss.
   Reaching lightningcss = matched SOTA full-CSSOM; that is the goalset.

5. **The benched Track 1 is a fact-stream `String` today, and the W6 single-sample harness
   is statistically inadequate.** The benched row emits `track1::parser::parse ->
   Result<String,String>` (`nonjson_css_l4.rs:596`), not a materialized typed CSSOM; the
   3.093 typed number is a separate retime path (§0.1). The W6 harness is single-sample
   (`sk-v16-w6-speed-report.md:60`; 3.093 vs 69.668 vs 13–15 Mbps run scatter across SK-V16
   reports). The SK-V17 RESULTS schema (§4.3) MUST emit N≥50 cold samples + median per
   corpus per comparator, including the per-corpus lightningcss split (currently only the
   corpus aggregate is published) AND the de-fact-streamed typed Track 1, so the per-corpus
   deltas in §2/§3 stop being `[INF]` and become measured.

---

## Verification ledger

- HEAD confirmed `1c5bd7a25` (`git log --oneline -1`).
- Benched corpus set (authoritative): `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:23-50`
  (bootstrap/tailwindcss/material-components-web/animate; 979,638 B; `normalize` ABSENT).
  Bytes cross-checked against `skinny/corpora/css-l4-sk-v14/manifest.md`.
- Benched Track 1 = fact-stream `String`: `track1_facts` →
  `track1::parser::parse(input) -> Result<String,String>`, `nonjson_css_l4.rs:596`.
  Benched tree = skinny (alphaE §0, `alphaE-candidate-shortlist.md:37-51`; no
  StructLayout/OpenFrame/CssArena in skinny/crates, grep-verified).
- Track 1 / lightningcss / cssparser planes + Mbps: SK-V17 dispatch contract canonical
  (N=100, 1c5bd7a25); cross-checked against
  `restart/audit/skinny-impl-overfit/sk-v16-w6-speed-report.md:55-60,102` (3.093 typed /
  833.199 lcss / 2476.472 cssparser single-sample run; 8-field equality all `track1 ==
  cssparser`), `sk-v16-w6tape-report.md:42-51` (~69.668 / ~2529.390 / ~793.326 scrutineer
  cold; 13.416 / 150.715 / 60.96 build run; per-corpus single-sheet NOT separately
  benched).
- Architecture band + per-corpus difficulty ordering:
  `restart/audit/skinny-impl-overfit/sk-v16-css-sota-tape-architecture.md:347-355`.
- INFERRED (marked `[INF]` inline throughout §2/§3, UNMEASURED-PENDING per §6.2): the
  per-corpus endpoint mapping (animate↔~164, tailwind↔~51, material↔~60, bootstrap↔~70)
  is reasoned from the contract's 51–164 range + corpus structural character, NOT a
  per-corpus published Mbps; per-corpus lightningcss split paired against the corpus
  aggregate ~974 (only aggregate published). SK-V17 Wave 0 N≥50 harness must confirm
  before any wave exit-gate keys on these cells.
- **Cross-artefact broadcast-row reconciliation (V4, count-correction residue).** This
  artefact's deltas are computed over the 4-file benched corpus set, not the RESULTS.md CSS
  broadcast rows, so no number here moves; for the record, the grep-true count of falsified
  CSS broadcast rows in `skinny/RESULTS.md` is **24** (`grep -c AUDIT-FALSIFIED
  skinny/RESULTS.md` = 24, lines 112–135). The companion grep
  `grep -c 'css_l4/.*/direct_to_struct/main'` = **25**: 24 are `^| css_l4/` table rows
  (112–135) and the 25th, at `:154`, is a prose REDRESS-127 companion reference, NOT a table
  row; `grep -nE 'W6.*css|tape.*direct_to_struct'` is EMPTY (no admitted/distinct W6 typed
  CSS row). **All cohort artefacts state 24 / lines 112–135 as of V3** (αA, αC §4/§7, αD
  §0/§5, SYNTHESIS §0.2, HANDOFF Current-State); the V2 "6" undercount is resolved
  cohort-wide. The V3 phrasing that alphaA was the sole sibling stating 24 is struck as
  stale.
