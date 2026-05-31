# CH1 — CORRECTNESS (cycle V5) — SK-V18 Pass-Alpha CHALLENGE

Lens: **CH1 Correctness** per `PASS-ALPHA.md §3` ("does every claim cite RESULTS.md
row, REDRESS entry, commit SHA, or measurement file? Are falsifiability gates
measurable? Are competitor deltas computed against the correct strictness plane?") +
ORCHESTRATOR §3W/§3Z. Subject: the Pass-Alpha SK-V18 artefacts
`research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md` (SYNTHESIS + HANDOFF
together constitute the α-F deliverable per PASS-ALPHA §2's output mapping — there is no
separate `research/alpha/αF` file; contract-compliant).

**V5 entry posture.** The V4 wave closed at 90.8% (sub-95%, non-converging) with one CH1
REVISE: the αE x86-scope orphan (the V3 CH5 §C.5 FOLD-1 second-surface widening landed in
αC/SYNTHESIS/HANDOFF but was NOT propagated into αE's P1 row + exit gate, leaving a
`src/`-scoped false-green gate over the live 3554-LOC `ext/x86/` surface). The V5 redress
folded all V4 REVISEs. This V5 CH1 pass **independently re-verifies every load-bearing
path:line / SHA / count / Mbps figure live at HEAD `318d9c046`** and confirms whether the
V4 CH1 REVISE (αE F15) + the cross-lens V4 clusters (CH2 §8.1 projection-tuple = αE F16;
CH3 αA orphan; CH6 §1 deletion-list reach) actually landed in the αA/αE feeders without
re-introducing a correctness defect. Honesty focus per the V3 H1 mandate: >SOTA framing
must disclose the lazy-vs-eager asymmetry; gates must be measurable against the ACTUAL
close condition; competitor deltas must be on the strict-vs-strict plane.

## Disk re-verification — ALL PASS at HEAD `318d9c046`

| Claim | Command | Result | Status |
|---|---|---|---|
| bracket HEAD | `git log --oneline -1` | `318d9c046 docs(sk-v18-handoff)…` | ✓ |
| SK-V17 close SHA | `git log --oneline -1 f6a38445b` | `…W5 close — SK-V17 CLOSED` | ✓ |
| V3 audit SHA | `git log --oneline -1 7dbe44c22` | `audit(skinny-impl-overfit-v3)… SK-V18 = generalization` | ✓ |
| **x86 `ext/x86/` (FOLD-1 surface)** | `find …/ext/x86 -type f \| xargs wc -l` | **3554** (`bbnf.asm` 485 + `x86util.asm` 1036 + `x86inc.asm` 1978 + `LICENSE-VENDOR` 55) | ✓ |
| x86 `src/x86_64/` files | `find …/src/x86_64 -type f \| wc -l` | **24** | ✓ |
| x86 `src/x86_64/` LOC | `find …/src/x86_64 -type f \| xargs wc -l` | **847** (742 `.rs` + 105 `.asm`) | ✓ |
| `build.rs` nasm driver | `wc -l …/build.rs` | **102** | ✓ |
| `Cargo.toml` nasm/build | `grep -n 'nasm-rs\|build =' …/Cargo.toml` | `:8 build="build.rs"`, `:19 nasm-rs="0.3"` | ✓ |
| `lib.rs:247` ext/x86 ref + `:5` mod + `:285` cfg arm | grep | `:5 pub mod x86_64;`, `:247 Contract documented in ext/x86/bbnf.asm`, `:285 cfg(…x86_64…avx512bw)` | ✓ |
| lock14 diagnostic-x86 | grep | `:2463 ("crates/bbnf-simd/src/x86_64","diagnostic-x86")` | ✓ |
| checkasm files | `ls …/tests/checkasm_*.rs \| wc -l` | **14** (12 single + 2) | ✓ |
| CSS replica md5 | `md5 …css_l4_*/generated.rs \| sort -u \| wc -l` | **1** (7 byte-identical) | ✓ |
| css_l4 dirs | `ls -d …css_l4_*/` | **7** | ✓ |
| **F16: css_l4 `fact_schema` distinct** | `grep fact_schema regen_css.rs` | **7 DISTINCT** (`css-l4-at-rules-media-facts-v1` … `css-l4-visual-function-facts-v1`) | ✓ |
| **F16: css_l4 `(source_roots,entry_rule)`** | `grep CSS_L4_ROOTS\|entry_rule` | all 7 share `CSS_L4_ROOTS` + `"stylesheet"` (collapse=1, GREEN) | ✓ |
| **F16: css_l4 `output_dir`/`row_id`** | grep | both vary per row (path + profile id) | ✓ |
| `CSS_GENERATED_RS` const-&str | `grep -n …runtime_generator.rs` | `:701 const CSS_GENERATED_RS: &str = r#"` | ✓ |
| `RuntimeEmitterKind` fork | `grep -n …grammar_provider.rs` | `:40 pub enum RuntimeEmitterKind {` | ✓ |
| `ValueRef<…,G>` two-axis | `grep -n …tape/mod.rs` | `:175 K = AnyKind, G: EventGrammar = AnyGrammar` | ✓ |
| `render(SinkOnlyProgram)` | `grep -n …json_sink_direct.rs` | `:4 pub fn render(program: &SinkOnlyProgram)…` | ✓ |
| `parse_w11_1_number` ×N | `grep -c …json/generated.rs` | **7** | ✓ |
| `GENERIC_SCAN_ROOTS` / exclusion holes | `grep -n …lock14_baseline.rs` | `:2409` strict / `:2420 FORBIDDEN_GENERIC_TOKENS` / `:2442 SKV15_W2_EXTRA_COVERAGE_ROOTS` | ✓ |
| sonic strict skipper | `sed -n 1,8p sonic_skipper.rs` | `IgnoredAny::deserialize` + `deserializer.end()` (strict, no `utf8_lossy`) | ✓ |
| W5 ledger N=200 medians | `sed -n 99,102p skv17-W5-close-ledger.md` | bootstrap 2473.1/1119.1/2.210×; animate 2937.9/1247.7/2.355×; tailwind 2773.4/828.5/3.348×; material 2618.5/1312.0/1.996× | ✓ |
| LOCKS Lock-14 alphabet | `sed -n 349p LOCKS.md` | `JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser` + `Json\|CssL4\|Bbnf\|GoogleSheets` arm census | ✓ |
| RESULTS twitter parse_only | row | t1=8349.290 / sonic-strict=4913.095 / serde=857.188 / Δ=+69.9% | ✓ |
| RESULTS apache_builds parse_only | row | t1=13129.331 / sonic=12951.668 / Δ=+1.4% (thinnest) | ✓ |
| RESULTS unicode_escapes parse_only | row | t1=7897.449 / sonic=2984.079 / Δ=+164.7% (widest) | ✓ |
| RESULTS canada parse_only | row | t1=16709.901 / sonic=12970.929 / Δ=+28.8%; simdjson DOM 11493 / Δ=+45.4% | ✓ |

Every load-bearing citation in the αA–αE cohort + SYNTHESIS + HANDOFF resolves as stated.
**The V4 CH1 REVISE (αE x86-scope orphan) is RESOLVED** — αE now carries F15 (crate-wide
x86) + F16 (projection-tuple), both disk-true. No new correctness defect of REVISE weight
surfaces. One cosmetic prose imprecision is noted (αE F16 narrative lists `output_dir`
among divergence-bearing columns the gate catches, though F16's own gate definition
correctly EXCLUDES it as a path column) — sub-disposition note, does not seed a wrong gate.

---

## §αA — Results Extraction — **ACCEPT** (V4 CH3/CH7 x86-scope orphan RESOLVED)

- The 51-row JSON >sonic-strict table reproduces RESULTS to the decimal. Spot-checked
  twitter (8349.290/4913.095/+69.9%), apache_builds (13129.331/12951.668/+1.4%),
  unicode_escapes (7897.449/2984.079/+164.7%), canada (16709.901/12970.929/+28.8%, simdjson
  DOM 11493/+45.4%) — **all match RESULTS exactly.** The range "+1.4%…+164.7%" is correct,
  apache_builds correctly thinnest, unicode_escapes correctly widest (`:45-46`, `:89`).
- The CSS >SOTA (§"CSS > lightningcss" row, `:89`) is correctly sourced from the W5 close
  ledger (`skv17-W5-close-ledger.md:99-102`), NOT RESULTS.md (which holds the FALSIFIED
  24-broadcast) — the four N=200 medians/ratios verify (1.996×…3.348×).
- **Honesty (H1):** `:6`, `:89` carry the lazy-vs-eager caveat plainly ("rich-typed Track1
  / lightningcss full-CSSOM," "VALID (lazy-vs-eager caveat — H1)"). Correct framing.
- **The V4 CH3/CH7 x86-scope orphan is FOLDED (V5 R-1).** `:13-27` records the fold; the
  §0 census x86 row, `:93` aarch64-only row, and `:204` inventory now name BOTH surfaces
  crate-wide (`src/x86_64/` 24/847 AND `ext/x86/` 3554 AND `build.rs` 102 AND `Cargo.toml`
  nasm dep AND `lib.rs:247` ref), with the close gate moved to crate-wide
  `find …/src/x86_64 …/ext/x86 -type f`=0 AND `grep -riE 'avx|gfni|sve|x86|nasm'
  bbnf-simd/` neutral-only. Every cited LOC re-verified at HEAD (3554 / 847 / 102). The two
  residual `find …/src/x86_64 -type f = 0` mentions (`:24`, `:296`) **explicitly describe
  the OLD `src/`-scoped gate as the DEFECT being corrected** ("would PASS GREEN while ~3656
  LOC of x86 ASM survived") — descriptive, not a live false-green gate.
- αA correctly AUTHORS the checkasm 12+2=14 correction (`:37`, `:45`) and notes the stale
  "18" lived only on alphaD §1.
- Competitor strictness plane correct: Δ vs sonic-**strict** (the `IgnoredAny`+`.end()`
  skipper), simdjson/yyjson honestly `n/a` except canada.

No misattributed claim, no un-cited number, no wrong-plane comparator; the V4 orphan
folded. ACCEPT.

## §αB — Competitor Deltas — **ACCEPT** (V4: ACCEPTed by all seven lenses, re-confirmed)

- The strictness-plane inventory is correct: sonic strict Skipper (`sonic_skipper.rs:1-7`
  re-verified — `IgnoredAny::deserialize` + `deserializer.end()`; `Cargo.toml:22-23`
  simd-json 0.13.11, sonic-rs 0.5.8 `default-features=false features=["sort_keys"]` — NO
  `utf8_lossy`) is the strict bar; sonic lossy quarantined flaw-probe-only;
  yyjson/asmjson/RapidJSON honest `None` on aarch64 (FFI not wired; AVX-512 x86-only, OUT
  per mandate). Correct plane per PASS-ALPHA §4.2 + §9 (the SK-V6 finding).
- The per-corpus Track-1/sonic/Δ table matches RESULTS to the decimal (twitter/canada/
  apache/unicode_escapes + serde figures verify).
- **Honesty (§2, §3.2):** CSS comparison framed ASYMMETRIC up front (`:54` — lazy 9-field
  aggregate vs eager full-CSSOM; "lazy rich-summary beats eager full-CSSOM, NOT CSSOM beats
  CSSOM"). The dual N-plane discipline (N=200 headline / N=80 cross-check) disclosed,
  non-mixable. Correct >SOTA-honest framing.
- **Cross-lens x86 REVISE correctly excluded (§6 fold-ledger, re-verified):** αB makes NO
  "x86 gone" close-claim — its `:73` asmjson-AVX512-OUT line is the *comparator face* of
  the mandate (states the comparator is OUT, makes no implementation close-claim). αB
  inherits no orphan; the αE x86-scope REVISE is correctly non-αB. The §6 fold ledger
  records the V4→V5 resolution; no number changed, no claim weakened.
- The GoogleSheets-no-competitor row is correct (the bar is GENERATION, not throughput);
  the canonical Lock-14 alphabet (`LOCKS.md:349`) cited correctly.

All deltas on the strict-vs-strict plane; asymmetry disclosed; correctly self-excluded.
ACCEPT.

## §αC — REDRESS Digest — **ACCEPT** (FOLD-1 + FOLD-2 retained; gold-standard x86 treatment)

- **FOLD-1 (CH5 V3 §C.5) crate-wide, re-verified:** §P1 (`:147-186`) deletes the ENTIRE
  x86 surface — `src/x86_64/` (−847) + `ext/x86/` (−3554) + `build.rs` (−102) + `Cargo.toml`
  nasm dep + `lib.rs:247` ref. Every cited live fact verifies on disk: `ext/x86/` = 3554
  (`bbnf.asm` 485 + `x86util.asm` 1036 + `x86inc.asm` 1978 + `LICENSE-VENDOR` 55), `build.rs`
  = 102 nasm driver, `Cargo.toml:8,19`. The P1 close gate is crate-wide
  (`grep -riE 'avx|gfni|sve|x86|nasm' bbnf-simd/`). This is the model fold the αA/αE feeders
  now mirror.
- **FOLD-2 (CH2 §8.1) retained:** the relocated-seam enforcement re-attributed to the P3
  structural row-count collapse (PRIMARY) with the arm-census grep NECESSARY-NOT-SUFFICIENT.
- P1–P5 close gates are each a concrete runnable predicate; the §4 verification log re-greps
  every fact live at HEAD.

Gates measurable, citations verified, x86 deletion crate-wide. ACCEPT.

## §αD — Validated/Invalidated Ledger — **ACCEPT** (checkasm 14; phantom-G precise; x86 concorded)

- The V4 row carries the disk-true **14** (12 single-kernel + `checkasm_common.rs` +
  `checkasm_parity.rs`), `:46-54`, with the live HEAD command. αD §1 was the lone surviving
  "18" in the V3 cohort; now corrected and cross-concorded with αA/αC/αE.
- I-rows independently re-verified: `CSS_GENERATED_RS:701`, `RuntimeEmitterKind:40`,
  `ValueRef…:175`, `parse_w11_1_number`=7, CSS replica md5=1. The phantom-`G`-vs-real-`K`
  two-axis precision (`:70-75` — `K` real, `G` test-only `_proof_compiles`, ZERO production
  `G` instantiations) is correct.
- **x86 concordance (V5):** `:30-37` brings αD's I7/S1 owner-surface + close-gate into
  concordance with αC §6 FOLD-1 (BOTH surfaces). The `:34` `find …/src/x86_64 -type f = 0`
  mention is **explicitly the OLD gate described as the false-green being corrected** ("while
  ~3.5K LOC of x86 ASM … survive") — descriptive, not a live gate. "No measurement,
  disposition, or finding of αD is reversed."

V3 REVISE resolved; internally consistent + well-cited; x86 concorded. ACCEPT.

## §αE — Candidate Shortlist — **ACCEPT** (V4 CH1 REVISE RESOLVED: F15 + F16 folded crate-wide, disk-true)

The V4 CH1 sole REVISE (the αE x86-scope orphan) is **RESOLVED**. The two V4 αE REVISEs are
folded as **F15** (x86-scope, `:18`) + **F16** (projection-tuple, `:19`), both
independently re-verified live this pass:

- **F15 (x86 crate-wide) — disk-true:** the P1 owner row (`:94`) now names BOTH surfaces —
  `src/x86_64/` (847) + `ext/x86/` (3554) + `build.rs` (102) + `Cargo.toml:8,19` nasm dep +
  `lib.rs:247` ref + `lib.rs:5` mod + `lib.rs:285-288` cfg arms + `lock14_baseline.rs:2463`
  diagnostic-x86 entry. The P1 exit gate (`:104`) is moved `src/`-scoped → **crate-wide**:
  `grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' bbnf-simd/` →
  neutral-only AND `find …/src/x86_64 …/ext/x86 -type f`=0 AND no `nasm-rs` in `Cargo.toml`.
  LOC corrected −847 → ≈ −4500 (`:94`, `:108`, `:227`). The summary table (`:221`) +
  net-LOC (`:227`) re-rolled to ≈ −10800 / ≈ −12850. **Every figure verifies at HEAD**
  (ext/x86 3554, build.rs 102, src/x86_64 847). The deletion list is reach-matched to the
  crate-wide grep (CH6 V4 §1 cluster-2 fold landed: `lib.rs:5` mod decl + `lib.rs:285-288`
  cfg arms + `Cargo.toml` dep all named — satisfiable-by-construction, no RED-by-construction
  hazard).
- **F16 (projection-tuple) — disk-true and correctly RED-today:** the relocated-seam
  structural check at αE:105/156/236 + cross-cutting notes 4/5 (`:236`, `:237`) projects
  onto the FULL per-grammar config tuple modulo `output_dir`/`expected_files` —
  `count(distinct (fact_schema, row_id, output_plane, emitter, entry_rule, source_roots))
  == 1` per `grammar_name`, NOT the V3 `(source_roots, entry_rule)`-only `sort -u`. **The
  empirical basis verifies exactly:** the 7 css_l4 rows SHARE `CSS_L4_ROOTS` + `"stylesheet"`
  (old projection collapse=1, GREEN) but carry **7 DISTINCT `fact_schema`** values
  (`regen_css.rs:49,67,85,103,121,139,157` — `css-l4-at-rules-media-facts-v1` …
  `css-l4-visual-function-facts-v1`), so a relocated branch riding `fact_schema` sails
  through the narrow projection. The widened gate is correctly RED pre-P3 (7 distinct today),
  GREEN only post-collapse — measurable against the actual close condition.
- The falsifiability triple (PRESERVED->SOTA / GRAMMAR-DERIVATION-PROOF /
  DISTINCT-GRAMMAR-OUTPUT) is unchanged in substance; F15 widens P1 scope, F16 widens one
  projection — both enforcement-reach corrections, not architecture. The CSS −3% floors
  (`:147-152`) pin to the N=200 medians (verified). Checkasm 12+2 correct in four places.
  Honest-None competitor posture (`:236`). Sheets source named
  (`grammar/google-sheets/google-sheets.bbnf`, Pratt-shape litmus, `:194`). Candidate count
  unchanged at 5; no re-opened pre-block.

**Sub-disposition note (cosmetic, NOT a REVISE):** the F16 ledger NARRATIVE at `:19` lists
the divergence-bearing columns as "`fact_schema` / `row_id` / `output_plane` / `output_dir`"
— but `output_dir` is precisely a generated-artefact PATH column that F16's OWN corrected
gate definition (correctly) EXCLUDES ("modulo `output_dir`/`expected_files`"). Disk confirms
`output_dir` varies per row (it must — it is the write path) and is correctly NOT in the
collapse projection at `:105`/`:156`/`:236`. Listing `output_dir` in the prose as a column
"where per-profile divergence demonstrably lives [that the gate must catch]" is internally
inconsistent with the same paragraph's correct exclusion of it. **This does NOT seed a wrong
gate** — the machine-check definition is correct everywhere it is stated, and the
load-bearing falsifier (7 distinct `fact_schema`) is sound. A one-word tightening (drop
`output_dir` from the F16 narrative's divergence list, or relabel it "the path columns
`output_dir`/`expected_files` are the EXCLUDED axis") would close the prose drift. Weighed as
cosmetic per ORCHESTRATOR §3Z (the gate is measurable + correct); ACCEPT.

All x86-scope and projection-tuple V4 REVISEs folded crate-wide + disk-true; the gates are
measurable against their actual close conditions; the lone residual is a cosmetic
narrative imprecision that does not alter the gate. ACCEPT.

## §SYNTHESIS (αF) — **ACCEPT**

The αF contract output. Every Section-0 close-condition gate is measurable + machine-checkable.

- **x86 `x86_tree_deleted` telemetry (`:563`) crate-wide + reach-matched:** redefined "NO
  x86 surface anywhere in `bbnf-simd` — `src/x86_64/` gone AND `ext/x86/` gone AND `build.rs`
  carries no nasm path AND `lib.rs:247` ref re-homed AND `nasm-rs` dep removed from
  `Cargo.toml:19` AND `lib.rs:5 pub mod x86_64;` + `:285-288` cfg arms removed AND in-crate
  doc surfaces scrubbed — deletion list reach-matched to the verify grep
  (`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' bbnf-simd/`),
  NOT `src/`-scoped." Every cited fact verifies on disk. The CH6 V4 §1 deletion-list reach
  fold landed (satisfiable-by-construction).
- **F16 `runtime_target_rows_collapsed` (`:553`) widened:** "all `RuntimeTarget` rows sharing
  one `grammar_name` byte-identical in EVERY field except `output_dir`/`expected_files`;
  `count(distinct config-tuple-minus-output_dir) == 1` over `fact_schema`/`row_id`/
  `output_plane`/`emitter`/`entry_rule`/`source_roots`/`check_command`/
  `frontend_requirements`; a `(source_roots,entry_rule)`-only `sort -u` is INSUFFICIENT — the
  divergence rides `fact_schema`/`output_plane`/`emitter`." Correct + disk-grounded. (Note:
  SYNTHESIS's `:553` list correctly puts `output_dir`/`expected_files` as the EXCLUDED axis —
  the binding contract states the projection more cleanly than the αE F16 narrative.)
- **Honesty (H1):** `materialization_framing ∈ {lazy-rich-vs-eager-cssom, symmetric-comparator}`
  column (`:566`); the CSS >SOTA re-framed lazy-rich-summary vs eager-full-CSSOM (`:327`,
  `:373`) with corpus-in-timer (`:567`) + the honest `css_canon_bench` kept; the
  honest-finding escape (`:331`) is itself GATED (a) `.bbnf` invokes by name, (b)
  grammar-derived data, (c) `verbatim_blob_present == false` — closing the largest paper-close
  surface.
- The JSON >SOTA range is correctly +1.4%–164.7% (`:15`, `:270`, `:338`, `:499`) with
  unicode_escapes widest; **the V1 "+1.4%–78%/marine_ik echo error is explicitly folded** at
  `:15` ("+1.4%–78% → +1.4%–164.7%; the widest row is unicode_escapes, not marine_ik").
- The generalization-axis telemetry (`:550-602`) makes every gate falsifiable:
  `verbatim_blob_present == false`, `generator_grammar_branch_count == 0` (canonical Lock-14
  alphabet over codegen AND xtask), `runtime_target_rows_collapsed == true`,
  `phantom_generic_resolved ∈ {instantiated, deleted}` (the `G` axis, NOT the real `K`),
  `acceleration_at_admission ∈ {admission, scalar-passthrough-labeled, retired}` (NOT
  cfg-test-only), `x86_tree_deleted == true`, `corpus_in_timer == true`; the gate REJECTS any
  row violating these. Competitor strictness plane (§0.6) forbids a fabricated competitor
  column.

Gates measurable + machine-checkable; citations verified; framing honest; the BLOCKING V3
fold + the V4 reach/projection sharpenings landed. ACCEPT.

## §HANDOFF (αF) — **ACCEPT**

- Consistent with SYNTHESIS; the x86 second-surface crate-wide invariant (`:74-75`,
  `:101-110`), the CH6 V4 §1 deletion-list reach fold (`:102-110` names `lib.rs:5` mod +
  `:285-288` cfg arms + `Cargo.toml:19` dep + doc-scrub OR source+manifest grep scope),
  checkasm 14 (`:11`), JSON range +1.4%–164.7% with apache thinnest/unicode_escapes widest
  (`:45-46`), and the H1 lazy-rich-summary reframe (`:153`) all carry correctly.
- Pre-blocked routes carry full semantics; the Lock-14 gate model matches `LOCKS.md:349`.

**Sub-disposition note (cosmetic, NOT a REVISE):** HANDOFF `:74`, `:104` describe the
`ext/x86/` surface as "~3000-LOC vendored ASM" (tilde-prefixed approximation). The precise
ASM figure is 3499 LOC (`bbnf.asm` 485 + `x86util.asm` 1036 + `x86inc.asm` 1978; the 3554
total includes the 55-LOC `LICENSE-VENDOR`). The "~3000" is an explicit, tilde-marked floor
approximation — honest, and the binding P1 close gate is content/grep-based (NOT a LOC
budget), with the precise figures living in αA/αC/αE/SYNTHESIS. Not a false claim; a deft
tightening to "~3.5K-LOC" would align it with the cohort. Weighed cosmetic; ACCEPT.

No un-cited or wrong-plane claim; gates measurable; the BLOCKING fold + V4 reach folds
landed. ACCEPT.

---

## §Cross-artefact correctness note (for the CONSOLIDATOR)

**The cohort is now internally consistent on EVERY load-bearing fact, including the x86
prune scope and the relocated-seam projection tuple** — the two axes that drove the V4
sub-95% wave. The V4 CH1 REVISE (αE x86-scope orphan) is resolved: αE F15 widens P1
crate-wide + moves the exit gate from the false-green `src/`-scoped predicate to the
crate-wide grep + `find …/ext/x86`, with every LOC figure disk-true (3554 / 847 / 102). The
V4 cross-lens clusters (CH2 §8.1 = αE F16 projection-tuple; CH3 αA orphan = αA V5 R-1; CH6
§1 deletion-list reach) all landed in the feeders and the binding contract. The two residual
`find …/src/x86_64 -type f = 0` mentions in the cohort (αA:24, αD:34) are **descriptive of
the OLD corrected gate**, not live false-greens.

The only residuals are two **cosmetic prose imprecisions**, neither of which alters a gate or
mis-states a load-bearing number:

1. αE F16 narrative (`:19`) lists `output_dir` among the divergence-bearing columns the gate
   catches, contradicting F16's own correct exclusion of it as a path column. The gate
   DEFINITION (αE:105/156/236, SYNTHESIS:553) is correct everywhere; the falsifier (7 distinct
   `fact_schema`) is sound.
2. HANDOFF (`:74`, `:104`) approximates `ext/x86/` as "~3000-LOC" (tilde-marked) where the
   precise ASM is 3499 / total 3554; the binding gate is grep/content-based, not LOC.

Both are sub-REVISE per ORCHESTRATOR §3Z (the gates remain measurable against their actual
close conditions). I flag them for a deft V5+ wording tightening but dispose ACCEPT.

**No other CH1 defect.** JSON deltas are on the strict-vs-strict plane (sonic-strict
`IgnoredAny`+`.end()`, verified); CSS deltas disclose the lazy-vs-eager asymmetry (H1)
honestly (`materialization_framing` column machine-checks it); the >SOTA framing is honest
throughout; the honest-finding escape is gated against paper-close; every falsifiability gate
is measurable against its actual close condition; the x86 close gate is crate-wide and
reach-matched (no false-green, no RED-by-construction).

---

## Disposition summary

| Artefact | Disposition | Basis |
|---|---|---|
| αA Results Extraction | **ACCEPT** | every claim cited + disk-verified; H1 lazy-vs-eager framing; checkasm 12+2 authored; V4 CH3/CH7 x86-scope orphan FOLDED crate-wide (V5 R-1); residual `src/`-mentions are descriptive-of-corrected-defect |
| αB Competitor Deltas | **ACCEPT** | correct strict-vs-strict plane (sonic strict skipper verified); serde/sonic figures verify; CSS asymmetry disclosed; correctly self-excludes from the x86 REVISE |
| αC REDRESS Digest | **ACCEPT** | FOLD-1 x86 crate-wide (3554/847/102 verified) + FOLD-2 relocated-seam; gold-standard treatment; measurable PRUNE gates |
| αD Validated/Invalidated | **ACCEPT** | checkasm 14 (12+2); phantom-G two-axis precise; x86 I7/S1 concorded to FOLD-1; residual `src/`-mention is descriptive-of-corrected-defect |
| αE Candidate Shortlist | **ACCEPT** | **V4 CH1 REVISE RESOLVED** — F15 x86 crate-wide + reach-matched + F16 projection-tuple, both disk-true (7 distinct `fact_schema` confirmed); gates measurable; lone residual is cosmetic `output_dir`-in-narrative prose drift (gate definition correct) |
| SYNTHESIS (αF) | **ACCEPT** | `x86_tree_deleted` crate-wide + reach-matched; `runtime_target_rows_collapsed` full-tuple (F16); H1 `materialization_framing`; JSON range +1.4%–164.7% (marine_ik echo folded); honest-finding escape gated; all machine-checkable |
| HANDOFF (αF) | **ACCEPT** | consistent with SYNTHESIS; crate-wide x86 invariant + reach fold; checkasm 14; JSON range correct; lone residual is cosmetic "~3000-LOC" tilde-approx (gate is grep-based) |

The V4 CH1 REVISE is resolved orphan-free; both consecutive ≥95% cycles condition is met on
the CH1 axis (V5 = 7 ACCEPT / 0 REVISE / 0 REJECT = 100%). No architectural re-open, no
stranded >SOTA, no wrong-plane comparator, no un-measurable gate.

TALLY accept=7 revise=0 reject=0
