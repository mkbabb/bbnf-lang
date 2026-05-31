# CH7 — OVERFIT-PRUNE (V5)

Lens: CH7 OVERFIT-PRUNE. Pass: PASS-ALPHA SK-V17→SK-V18 cycle **V5** (the GENERALIZATION cycle /
inflection backtrack). Per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z. Reviewer focus: the SIX NEW
CHALLENGE addenda fire HONESTLY across the alpha artefacts — **verbatim-blob** (const-`&str`
`@generated` = hand-written), **distinct-grammar-output** (N grammars = N non-identical
`generated.rs`), **single-emitter-path** (flag/enum forks), **phantom-generic** (uninstantiated
`<G>`), **timed-plane-symmetry + corpus-in-timer**, **acceleration-wiring** (NEON at admission,
not `#[cfg(test)]`) — plus no-contrivance, x86 DELETED, 7-replica COLLAPSED.

Subject reviewed: `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`. Per PASS-ALPHA
§2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md` (no separate `alphaF-*.md`; confirmed
absent on disk — this is contract-correct, NOT a missing artefact).

**V5 posture (the confirming pass).** This CHALLENGE wave's CH7 history: V1 = 7A/1R (alphaA x86
census 23→24), V2 = 7A/0R, V3 = 7A/1R (alphaD §1 stale checkasm "18"), **V4 = 7A/1R** (alphaA
x86-census scoped `src/x86_64/` only — the BLOCKING second-x86-surface FOLD-1 orphan). The wave
aggregate at V4 was **90.8%** (sub-95%, non-converging); the V4 CONSOLIDATED resolved FIVE distinct
REVISE clusters into V5. **V5 is the confirming wave** that must record the SECOND consecutive ≥95%
cycle per §3Z. **A lens with a four-pass clean-modulo-one-straggler history must NOT rubber-stamp a
"redress landed" claim.** This V5 report (a) re-verifies EVERY addendum surface live at HEAD
`318d9c046` (unchanged since V3/V4); (b) independently confirms the V4 CH7 §1 alphaA REVISE is
folded crate-wide; (c) independently confirms the other four V4 CONSOLIDATED clusters (alphaE F15
orphan, P1 deletion-list reach, the `runtime_target_rows_collapsed` projection tuple F16,
fold-ledger anchor stability) are folded ORPHAN-FREE; and (d) adversarially re-hunts the cohort for a
straggler — the recurrent failure mode this wave keeps surfacing. **V5 finds NONE: the redress is
complete, disk-grounded, and the cohort is concordant.** 7 ACCEPT, 0 REVISE, 0 REJECT.

---

## §0 — Independent verification log (CH7 V5 re-grep, HEAD `318d9c046`)

Every load-bearing overfit/prune claim re-verified before disposition. The addenda are NOT accepted
on the artefacts' word — each is confirmed against the tree this V5 pass. **HEAD is unchanged from
V3/V4 (`318d9c046`); ZERO source divergence.**

| Claim under the addenda | CH7 V5 command | Result | Verdict |
|---|---|---|---|
| **verbatim-blob** (CSS const-`&str`) | `grep -n 'const CSS_GENERATED_RS' codegen/src/runtime_generator.rs` | `runtime_generator.rs:701: const CSS_GENERATED_RS: &str = r#"` | CONFIRMED — path `codegen/src/runtime_generator.rs` |
| **single-emitter-path** (fork) | `grep -rn 'enum RuntimeEmitterKind\|RuntimeEmitterKind::' codegen/src` | `grammar_provider.rs:40 pub enum RuntimeEmitterKind`; `:110 != RequestFacts`; `lib.rs:282 CompiledLowering`/`:291 RequestFacts`; `runtime_generator.rs:17/:25` match-arms | CONFIRMED (fork live at admission) |
| **distinct-grammar-output** (7 replicas) | `md5 -q runtime/.../css_l4_*/generated.rs \| sort \| uniq -c` | `7  b654562ccff46ed62dd48e9ace325830` (7→1 byte-identical) | CONFIRMED |
| **phantom-generic** (`<G>` default; no prod instantiation) | `tape/mod.rs:175`; census non-default `G` | `:175 K = AnyKind, G: EventGrammar = AnyGrammar`; the ONLY non-default `G` uses are `event_grammar_tests.rs:18/:20/:43/:44/:89`, EACH `#[cfg(feature="proof")]`-gated; `CssEventGrammar` does NOT exist (so "instantiate" entails authoring a new grammar-named type — DELETE-default is the abrogate-before-patch posture); the `Json`/`Sheets` witness STRUCTS compile in prod but are NEVER used as a non-default `G` outside the proof-gated test | CONFIRMED — phantom `G` is proof-feature-only as a non-default instantiation; the artefacts' "only `_proof_compiles`" framing is precise |
| **acceleration-wiring** (CSS NEON dead at admission) | `runtime/src/lib.rs` `#[cfg(test)]` boundary; prod reach of `count_top_level_commas` | `lib.rs:51 #[cfg(test)]` region; the three NEON callers `find_css_significant :574`, `find_comment_close :598/:608`, `count_top_level_commas :629/:638` inside it; the **only** prod reach is `css_l4_*/generated.rs:157 → count_top_level_commas (gen:809)` — the COLD rich-summary | CONFIRMED dead-at-admission (2-of-3 CSS NEON consumers; 3rd cold) |
| **corpus-in-timer / timed-plane** (OLD warm bench) | `grep nonjson_css_l4.rs` | `:66 EXPECTED_FIXTURE_BYTES = 187`; `:1989 input.len() != EXPECTED_FIXTURE_BYTES`; `:3091 fn measure_mbps` | CONFIRMED live |
| **x86 census — `src/x86_64/`** | `find …/x86_64 -type f` | **24 files** | CONFIRMED |
| **x86 census — SECOND surface `ext/x86/`** | `find …/ext/x86 -type f \| xargs wc -l` | **3554 LOC** total | CONFIRMED |
| **x86 census — nasm build driver** | `wc -l build.rs`; `grep nasm Cargo.toml` | `build.rs` = **102 LOC**; `Cargo.toml:8 build="build.rs"`, `:19 nasm-rs="0.3"` | CONFIRMED |
| **x86 census — lib.rs module + cfg arms (the V4 cluster-2 reach defect)** | `grep -n 'mod x86_64\|x86_64' src/lib.rs` | `:5 pub mod x86_64;`; `:247` `ext/x86/bbnf.asm` ref; `:285-287` `#[cfg(all(target_arch="x86_64", target_feature="avx512bw"))] … return crate::x86_64::…` dispatch arm | CONFIRMED — the crate-wide verify grep DOES fire on `Cargo.toml:19` + `lib.rs:5` + `lib.rs:285-288`, the surfaces the V4 four-item deletion-list missed |
| **F16 projection disk-truth** (the 7 css_l4 rows) | `grep -n 'fact_schema\|entry_rule\|source_roots\|grammar_name' xtask/src/regen_css.rs` | all 7 rows share `grammar_name:"css_l4"` + `entry_rule:"stylesheet"` + `source_roots:CSS_L4_ROOTS` but carry **7 DISTINCT** `fact_schema` (`css-l4-at-rules-media-facts-v1` … `css-l4-visual-function-facts-v1`) | CONFIRMED — `(source_roots,entry_rule)`-only `sort -u` = 1 (false-GREEN); the F16 full-config-tuple collapse is correctly RED today |
| **Sheets PROVE litmus honesty** | `find google-sheets.bbnf`; skinny-tree presence; `sheets_witness` LOC | `grammar/google-sheets/google-sheets.bbnf` = 7681 B (operator-precedence shaped) EXISTS in totality tree; **0** in skinny tree (correctly "lives in totality tree only"); `sheets_witness/` = **25 LOC** stub | CONFIRMED — PROVE adopts a genuinely-different REAL Pratt grammar, not a fresh "third JSON" |
| **checkasm harness count** | `ls bbnf-simd/tests/checkasm_*.rs \| wc -l` | **14** = 12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs`. NOT 18. | CONFIRMED 14 |
| metalang leak | `grep -c parse_w11_1 json/generated.rs` | `7` | CONFIRMED |

**V5 ground-truth census: HEAD unchanged (`318d9c046`); ZERO source divergence from V4.** Every
addendum surface reproduces. The five V4 CONSOLIDATED REVISE clusters all fold-verified against the
artefacts below.

---

## §1 — V4 → V5 fold verification (the five V4 CONSOLIDATED clusters)

CH7 V5 independently confirms each of the five V4 REVISE clusters is folded into the V5 cohort
ORPHAN-FREE. This is the load-bearing V5 obligation: the §3Z second-consecutive-≥95% requirement
is contingent on the V4 REVISEs being resolved without introducing a new defect.

| # | V4 REVISE cluster | V5 fold site (CH7-verified) | Status |
|---|---|---|---|
| 1 | **x86 FOLD-1 orphan in αA** (CH3, CH7 V4 §1 — the BLOCKING second-x86-surface scoped `src/x86_64/` only) | `alphaA:13-31` V5 FOLD log R-1; `:93` §0 census x86 row (BOTH surfaces); `:204` §3.2 x86 row (BOTH surfaces + nasm + cfg-arms); `:292-296` §5 PRUNE close-condition 4 (crate-wide gate); `:333-343` V5 FOLD ledger R-1 — all now name `src/x86_64/` (24/847) AND `ext/x86/` (3554) AND `build.rs` (102) AND `Cargo.toml` nasm-rs AND `lib.rs:247` AND the crate-wide close-gate, AND retract the inaccurate "ZERO V3 REVISE across all seven lenses" assertion | **FOLDED** ✓ |
| 2 | **P1 deletion-list reach mismatch** (CH6 V4 §1 — the crate-wide grep fires on 3 active surfaces the 4-item list never named) | SYNTHESIS `:109-121` (adds removal targets (e) `nasm-rs` dep `Cargo.toml:19`+`:14-16`, (f) `lib.rs:5 pub mod x86_64;` + `:285-288` cfg-arms, (g) doc-scrub OR `--include='*.rs' --include='Cargo.toml'`); SYNTHESIS `:315` P1 row + `:563` `x86_tree_deleted` telemetry; HANDOFF `:14-17`,`:102-109`,`:218-221`,`:254-255`,`:307` — deletion list is now REACH-MATCHED to the verify grep (satisfiable-by-construction) | **FOLDED** ✓ |
| 3 | **`runtime_target_rows_collapsed` projection tuple** (CH2 V4 §8.1 — bound to `(source_roots,entry_rule)`-only, misses the 5 per-profile columns) | SYNTHESIS `:89-90`,`:133-152`,`:566` (`runtime_target_rows_collapsed` redefined to all non-path columns); HANDOFF `:22-24`,`:274-279` inv.5; alphaE `:19` F16 ledger row + `:71`(F16) / `:90`(F16) / `:95` P3 row / `:105` P3 exit / `:156` B2 gate / `:207` B4 litmus — every F13 site widened to `count(distinct config-tuple-minus-output_dir) == 1` per `grammar_name` over `fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/`source_roots`/`check_command`/`frontend_requirements` | **FOLDED** ✓ (disk-grounded: §0 row confirms the 7-distinct-`fact_schema` empirical) |
| 4 | **αE x86 FOLD-1 orphan** (CH1 V4 §αE, CH3 — same orphan in the αE feeder) | alphaE `:14`,`:19` F15 ledger row; P1 row + P1 exit + LOC budget + candidate-A summary + net-LOC all crate-wide (`:95` P3, `:365`-class P1 in SYNTHESIS mirror) | **FOLDED** ✓ |
| 5 | **fold-ledger self-citation drift** (CH6 V4 §13 — back-refs to prior-cycle line numbers the V4 edits shifted) | self-citations switched to fold-stable section/column anchors ("the G3 close-condition row," the `generator_grammar_branch_count` telemetry column, the §1 checkasm ledger); alphaD `§8.V5` FOLD log present (`:310`) | **FOLDED** ✓ — machine-gate-unaffected; anchors no longer drift |

**All five V4 REVISE clusters fold into V5 with concrete, disk-verified mechanisms. ZERO orphan.
Every fold is a TIGHTENING (x86 deletion goes wider; the relocated-seam projection goes wider; the
deletion list reach-matches the grep) — not a finding reversal, not a loosening.**

---

## §2 — alphaA (results extraction) — the V4 REVISE FOLDED

alphaA was the V4 CH7 §1 REVISE target (the x86-census ORIGIN artefact that scoped x86 to
`src/x86_64/` only). CH7 V5 re-greps every alphaA x86 surface AND confirms the cohort-wide FOLD-1
reached it.

| Section | Addendum coverage | CH7 V5 re-verify | Disposition |
|---|---|---|---|
| §0 headline census x86 row (`:93`) | x86-deleted (BOTH surfaces) | "x86 on TWO surfaces (V5 R-1): (1) `src/x86_64/` 24 files/847 LOC, 14 `unimplemented!`; (2) `ext/x86/` 3554 LOC + nasm `build.rs` 102 + `Cargo.toml` nasm-rs + `lib.rs:247`" — close-gate crate-wide ✓ | **ACCEPT** (was V4 REVISE — folded) |
| §1 JSON >sonic-strict per-corpus Δ (+1.4%…+164.7%) | >SOTA PRESERVE bar | `:88` apache_builds +1.4% canary; +164.7% widest; `sonic_skipper.rs:5-6 IgnoredAny+`.end()` strict plane ✓ | **ACCEPT** |
| §2 CSS 1.996–3.348× + §2.1 lazy-vs-eager | timed-plane-symmetry (H1) pin | `:89` N=200 cold median, real corpus, lazy-vs-eager caveat stated ✓ | **ACCEPT** |
| §3.1 generator-does-not-exist | verbatim-blob / single-emitter-path / distinct-output | `:701`, `:40/:110`, md5 7→1 ✓ — paths correct | **ACCEPT** |
| §3.2 contrivance/wrong-arch table x86 row (`:204`) | timed-plane / x86-census (BOTH surfaces) | x86 row names BOTH trees + `build.rs` + `Cargo.toml` `:8/:19` + `lib.rs:5`/`:247`/`:285-287` + crate-wide gate ✓ | **ACCEPT** (was V4 REVISE — folded) |
| §3.3 phantom + divergent value API | phantom-generic on the `G` axis | `tape/mod.rs:175` two-axis; `K` real, `G` phantom ✓ | **ACCEPT** |
| §3.4 NEON wiring + checkasm "12 single-kernel … not 18" | acceleration-wiring | `ls checkasm_*.rs` = 14; alphaA AUTHORS the false-`18` correction ✓ | **ACCEPT** — remains the model checkasm framing |
| §5 PRUNE close-condition 4 (`:292-296`) | x86-deleted close gate | crate-wide: `find …/src/x86_64 …/ext/x86 -type f`=0 AND `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` neutral-only ✓ | **ACCEPT** (was V4 REVISE — folded) |
| §6/§8 V5 FOLD log R-1 (`:13-31`,`:333-343`) | fold-discipline | retracts "ZERO V3 REVISE across all seven lenses"; records the CH5 V3 §C.5/§F.7 BLOCKING fold; all FOLD-1 facts re-verified at HEAD ✓ | **ACCEPT** — the V5 FOLD log is now accurate |
| §4 substrate / §7 close-seeds | Lock 1 holds | tape singular ✓ | **ACCEPT** |

**alphaA overall: ACCEPT (all sections).** The V4 CH7 §1 REVISE is folded crate-wide on all three
binding surfaces (§0 census `:93`, §3.2 row `:204`, §5 close-condition `:292-296`) and the V5 FOLD
log R-1 (`:13-31`,`:333-343`) correctly retracts the inaccurate "ZERO V3 REVISE" assertion and
re-verifies every FOLD-1 fact at HEAD. alphaA is now CONCORDANT with alphaC §6 FOLD-1, SYNTHESIS
`:315`/`:563`, and HANDOFF inv-3. The §3.4 checkasm framing remains the cohort model. No REVISE.

---

## §3 — alphaB (competitor deltas) — the bar to preserve

CH7's overfit lens on alphaB: (a) CSS lazy-vs-eager asymmetry disclosed up front
(timed-plane-symmetry); (b) no un-run comparator fabricated as a number (corpus-in-timer /
contrivance). alphaB carries its V3/V4-ACCEPTed bar (a PRESERVE bar, unchanged by definition),
re-verified at HEAD.

| Section | Disposition |
|---|---|
| §0 standing + asymmetry pin (JSON near-symmetric strict; CSS asymmetric lazy-vs-eager) | **ACCEPT** — asymmetry up front is timed-plane-symmetry done right |
| §1 JSON strict-vs-strict bar (`sonic_skipper.rs` `IgnoredAny`+`.end()`, no `utf8_lossy`; apache_builds +1.4% canary) | **ACCEPT** — strict plane re-verified |
| §1.3 simdjson DOM = different output plane, NOT the strict bar | **ACCEPT** |
| §1.4 Track 2 typed caveat (conditional on hand-tuned per-corpus schema) | **ACCEPT** |
| §2 CSS lazy-vs-eager (track1_rich vs lightningcss full-CSSOM; keeper `css_canon_bench`; dual N=200/N=80) | **ACCEPT** — the load-bearing H1 disclosure |
| §3.3 NOT-runnable comparators honest `None` (yyjson/asmjson/RapidJSON; asmjson AVX-512 x86-only OUT) | **ACCEPT** — the strongest anti-contrivance posture; the "asmjson AVX-512 OUT" line is the comparator FACE of the aarch64-only mandate and makes NO "x86 gone" close-claim (no scope inheritance) |
| §3.4 H1 options (symmetric comparator OR rename+footnote) | **ACCEPT** |
| §4 preservation bar (per-grammar must-hold + canary; `GoogleSheets` canonical) | **ACCEPT** |
| §6 fold record (the V4 REVISEs are NON-αB sections; no measurement/ratio/plane touched) | **ACCEPT** |

**alphaB overall: ACCEPT.** alphaB's §3.3 honest-`None` posture is the precise foreclosure of the
corpus-in-timer / fabricated-competitor contrivance. The five V4 REVISE clusters touch no αB section
(x86 census, P1 reach, projection tuple, αE/αA orphans, ledger anchors are all non-αB). No REVISE.

---

## §4 — alphaC (REDRESS digest) — the PRUNE waves + pre-blocks (the gold-standard fold)

alphaC was ALREADY crate-wide at V4 (its §6 FOLD-1 was the canonical second-x86-surface fold the
rest of the cohort was reconciled against). CH7 V5 confirms it is unchanged and remains the
authority alphaA was brought into concordance with.

| Section | Addendum | CH7 V5 re-verify | Disposition |
|---|---|---|---|
| §0/§0.A/§0.B framing + state-delta (`emit_fact_stream` gone) | n/a | `grep -c emit_fact_stream` = 0 ✓ | **ACCEPT** |
| §1-P1 delete x86 — crate-wide (`src/x86_64/` AND `ext/x86/` AND nasm `build.rs` AND `lib.rs:247`) | x86-deleted | `find …/ext/x86 -type f` = 3554; `build.rs` 102 ✓ | **ACCEPT** |
| §1-P2 delete OLD bench (`measure_mbps:3091` warm, 187-byte SHA fixtures) | corpus-in-timer | `:66/:1989/:3091` ✓ | **ACCEPT** |
| §1-P3 collapse 7 replicas (md5 single-hash; collapse-default) | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** |
| §1-P4 fix gate holes + `EventGrammar`-type-leak clause | gate-scope | `:2409`/`:2463`/`diagnostic-x86` ✓ | **ACCEPT** |
| §1-P5 purge metalang (`parse_w11_1_number ×7`) | metalang | ×7 ✓ | **ACCEPT** |
| §2.1-§2.6 pre-blocks (AZ-IV / StructRegistry / fact-stream residual fork / 24-broadcast / FNV / x86-AVX-SVE) | pre-blocks; verbatim-blob + single-emitter | `:701` + `:40/:110` ✓ | **ACCEPT** |
| §2.6 checkasm count (`:581` "12 single-kernel + 2 composite = 14, NOT 18") | acceleration-wiring | `ls checkasm_*.rs`=14 ✓ | **ACCEPT** |
| §6 FOLD-1 (CH5 second-x86-surface, crate-wide, with V4 extension — `Cargo.toml` nasm-rs + scalar/checkasm contract refs) | x86-deleted | `ext/x86/` 3554; `build.rs` 102; `Cargo.toml` nasm-rs; `lib.rs:247` ✓ | **ACCEPT** — the canonical fold |

**alphaC overall: ACCEPT (all sections).** alphaC §6 FOLD-1 is the gold-standard treatment of the
second-x86-surface — it folds crate-wide AND extends it (the `Cargo.toml` nasm-rs build-dep + the
scalar/checkasm `ext/x86` contract references). alphaA §2 above is now in concordance with this. No
REVISE.

---

## §5 — alphaD (validated/invalidated ledger) — the V3 + V4 REVISEs FOLDED

alphaD's V3 REVISE (stale checkasm "18") was folded at V4 (§1 V4 now reads "14 … NOT 18"). CH7 V5
confirms the V4 fold persists AND the cluster-5 anchor-stability fold landed (the §8.V5 FOLD log).

| Row | Addendum lens | CH7 V5 re-verify | Disposition |
|---|---|---|---|
| §1 V1-V3 substrate/JSON/CSS validated | PRESERVE bars | tape singular; JSON; CSS N=200 ✓ | **ACCEPT** |
| §1 V4 NEON checkasm discipline ("14 … NOT 18"; `:141`) | acceleration-wiring | `ls checkasm_*.rs` = 14 ✓ | **ACCEPT** — the V3 REVISE stays folded |
| §1 V5-V8 neutral kernel / honest harness / regen / FNV-quarantine | — | dispatch data-predicate; `css_canon_bench`; FNV bench-only ✓ | **ACCEPT** |
| I1 CSS grammar-driven invalidated | verbatim-blob | `:701` ✓ | **ACCEPT** |
| I2 JSON projects from grammar | verbatim-blob | `json_sink_direct` ✓ | **ACCEPT** |
| I3 7 sub-grammars admitted | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** |
| I4 one codegen path | single-emitter-path | `:40` ✓ | **ACCEPT** |
| I5 `ValueRef<G>` parametric | phantom-generic | `:175 G=AnyGrammar`; sole `G` in `event_grammar_tests.rs` (proof-gated) ✓ | **ACCEPT** — the proof-feature-only precision is CORRECT |
| I6 NEON CSS-scan acceleration | acceleration-wiring | `lib.rs:51 #[cfg(test)]`; 2-of-3 dead, count-commas cold ✓ | **ACCEPT** |
| I7 aarch64-only (claim-row disposing to crate-wide P1) | x86-deleted | I7 claim-row cross-refs the crate-wide P1; the binding close-gate is in alphaA (now folded) + SYNTHESIS ✓ | **ACCEPT** — claim-row, not the binding close-gate |
| I8 Lock-14 gate meaningful | gate-scope | `:2409` exclusion ✓ | **ACCEPT** |
| I9 equal-work CSSOM | timed-plane / H1 | track1_4field vs rich ✓ | **ACCEPT** |
| I10 clean shipped symbols | metalang | ×7 ✓ | **ACCEPT** |
| §3 DEMOTED DM1-DM4 (typed conditional; 5 scalar passthroughs; UDOT orphan) | `_neon`-suffix-truth | `digit_mac.rs` udot, 0 runtime callers ✓ | **ACCEPT** |
| §4 STILL-OPEN; §5 pre-blocked; §6 self-verify; §8 V4-FOLD R1 (18→14) + §8.V5 FOLD log (`:310`) | all; cluster-5 anchor-stability | `:46`,`:141`,`:347` checkasm 14; §8.V5 anchors fold-stable ✓ | **ACCEPT** — both the V3 and the V4 ledger folds persist |

**alphaD overall: ACCEPT (all sections).** The V3 REVISE (checkasm 18→14) stays folded; the §8.V5
FOLD log records the V4 dispositions resolved with fold-stable anchors (cluster-5). I7 is a claim-row
disposing to the crate-wide P1 (binding close-gate authored in alphaA + SYNTHESIS). No REVISE.

---

## §6 — alphaE (candidate shortlist) — the two V4 αE REVISEs FOLDED (F15 + F16)

alphaE carried the TWO V4-specific REVISEs (CH1 §αE + CH3 = the x86-scope orphan; CH2 §8.1 = the
projection-tuple sharpening). CH7 V5 confirms both fold as F15 + F16, neither adding/removing a
candidate.

| Cluster | Addendum gate | CH7 V5 re-verify | Disposition |
|---|---|---|---|
| §0 triple (mutate-`.bbnf`→output-changes / DISTINCT-OUTPUT / PRESERVED) | the three load-bearing gates | `:71-72` "a const courier cannot pass" operational falsifier for verbatim-blob ✓ | **ACCEPT** |
| §0 F15 ledger row (x86-scope orphan crate-wide) | x86-deleted | `:14` F15 — P1 deletion scope + close gate crate-wide; no candidate change ✓ | **ACCEPT** (V4 REVISE — folded) |
| §0 F16 ledger row (projection-tuple widening) | distinct-grammar-output structural | `:19` F16 — empirically refutes the `(source_roots,entry_rule)`-only projection (disk: 7 distinct `fact_schema`); MECHANISM unchanged, PROJECTED COLUMN SET widened ✓ | **ACCEPT** (V4 REVISE — folded) |
| A PRUNE (P1-P5; P1 owner-paths crate-wide incl `ext/x86/` + `build.rs` + nasm-rs + cfg-arms) | x86=0; replicas; gate; metalang | `:95` P3 row carries the F16 structural check; P1 reach-matched ✓; checkasm "12 single-kernel + 2 [F4: corrected from 18]" ✓ | **ACCEPT** |
| B1 un-fork + project JSON (G3+G1) | single-emitter-path / verbatim-blob | apache_builds +1.4% canary; `json_sink_direct::render` `:96/:124` template-not-projection ✓ | **ACCEPT** |
| B2 derive CSS (G2; `grep CSS_GENERATED_RS → 0`) | verbatim-blob (centrepiece) | `:156` B2 gate carries F16 collapse; LOW risk ✓ | **ACCEPT** |
| B3 shared trait + kill phantom (G4+H1; DELETE-default + test-excluded grep) | phantom-generic / timed-plane | `CssEventGrammar` absent → INSTANTIATE = burden-of-proof ✓ | **ACCEPT** |
| B4 PROVE Sheets + NEON (3 distinct `generated.rs`; checkasm 14; acceleration-at-admission) | distinct-output / acceleration-wiring | `:207` B4 litmus carries F16; `google-sheets.bbnf` real Pratt, not skinny-tree yet ✓ | **ACCEPT** |
| SUMMARY + cross-cutting (sequencing; F13/F15/F16 folds; net-LOC crate-wide) | all | F15/F16 net-LOC crate-wide; no candidate added/removed (still A, B1-B4) ✓ | **ACCEPT** |

**alphaE overall: ACCEPT.** Both V4 αE REVISEs fold as F15 (x86 scope crate-wide) + F16 (projection
tuple widened to all non-path columns), each disk-grounded; neither adds or removes a candidate. The
shortlist remains additive-by-deletion, exactly 5 (A, B1-B4). No REVISE.

---

## §7 — SYNTHESIS.md + HANDOFF.md (the αF contract) — the goalset (V5-folded)

The master αF output. CH7 V5 confirms (a) the six addenda each carry a §0.1 close-gate + a §0.4
pre-block + a §2 machine-checkable telemetry column the `gate-json` consumer REJECTs on, AND (b) all
THREE binding-contract V4 clusters landed (cluster-2 deletion-list reach, cluster-3 projection
tuple, cluster-1 x86 crate-wide).

### §7.1 — Addendum triple-binding (close gate + pre-block + telemetry)

| Surface | Close gate (§0.1) | Telemetry column (§2) | Pre-block (§0.4) | Disposition |
|---|---|---|---|---|
| G2 verbatim-blob | `grep CSS_GENERATED_RS → 0`; grammar-projected | `verbatim_blob_present == false` (`:550`) + `grammar_derived` (`:548`) | verbatim-blob re-entry | **ACCEPT** |
| G3 single-emitter | `RuntimeEmitterKind` gone + canonical FULL-alphabet arm census + type census + structural row-count | `emitter_fork_present`; `generator_grammar_branch_count==0` (`:552`); `generator_grammar_type_count==0` (`:554`); `runtime_target_rows_collapsed` (`:553`, widened) | fork resurrection / relocated-seam | **ACCEPT** — relocated-seam caught STRUCTURALLY by the WIDENED projection (cluster-3) |
| G4 phantom-generic | `G` instantiated w/ prod grammar OR removed; DELETE default; `_proof_compiles` excluded | `phantom_generic_resolved` + `shared_value_trait_instantiations≥2` + `json_rich_navigation_preserved` (`:558`) | phantom re-entry | **ACCEPT** — `G`-axis-not-`K`-axis + LCD-flatten REJECT (`:323`) precise |
| G6 acceleration-wiring | reached at admission (grep hot path not tests); retire gated on samply non-top-N | `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` NOT `cfg-test-only` (`:565`) | acceleration claim | **ACCEPT** |
| PROVE distinct-grammar-output | Sheets `generated.rs` md5≠JSON≠CSS; `grep const.*_RS Sheets=0`; `sheets_grammar_shape==pratt-operator` | `generated_md5_distinct` + `generator_grammar_count==3` + `sheets_real_grammar` + `sheets_grammar_shape` (`:560-561`) | distinct-output re-entry | **ACCEPT** — `google-sheets.bbnf` is a REAL Pratt grammar, not a "third JSON" |
| H1 timed-plane-symmetry | equal work, real corpus cold, no micro-fixtures; P2 deletes warm bench | `corpus_in_timer == true` + `materialization_framing` | corpus-out-of-timer / more-work | **ACCEPT** |
| P1/P4 x86-deleted (crate-wide, reach-matched) + gate | `find …/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` neutral-only; deletion list = (a)-(g) reach-matched to the grep | `x86_tree_deleted` crate-wide reach-matched (`:563`) + `lock14_gate_scans_codegen` | x86/AVX/SVE/nasm | **ACCEPT** — cluster-2 reach-match folded; satisfiable-by-construction |

### §7.2 — HANDOFF carry-through

HANDOFF carries all six addenda + all V4 folds verbatim into S-P0+: the x86 reach-matched deletion
list (`:14-17`,`:102-109`,`:218-221`,`:254-255`,`:307`), the widened relocated-seam projection
(inv.5, `:22-24`,`:274-279`), the phantom `G`-axis instantiate-or-delete, the acceleration
admission-not-cfg-test gate, the honest-finding escape gated (a)-(c). Inviolable-invariant 3
(crate-wide aarch64-only) and invariant 5 (the multi-surface Lock-14 gate with the WIDENED
structural row-count) are the V5 hardening that makes the P1 gate satisfiable-by-construction.

**SYNTHESIS.md + HANDOFF.md overall: ACCEPT (all sections).** Every one of the six addenda is bound
THREE ways (close gate + pre-block + telemetry the `gate-json` consumer REJECTs on). All three
binding-contract V4 clusters landed: x86 `x86_tree_deleted` is crate-wide AND reach-matched to the
verify grep (cluster-2); `runtime_target_rows_collapsed` projects onto all non-path columns
(cluster-3, disk-grounded by the 7-distinct-`fact_schema` empirical); the honest-finding escape is
gated (a)-(c) (`:331`), foreclosing the largest paper-close surface. No REVISE.

---

## §8 — Cross-artefact addendum coverage matrix (CH7 V5 summary)

| Addendum | Live surface (CH7-verified @ `318d9c046`) | Named in | Close gate | Pre-block | Telemetry |
|---|---|---|---|---|---|
| **verbatim-blob** | `runtime_generator.rs:701` const `&str` | A§3.1, C§2.3, D-I1, E-B2, SYN-G2, HO | `grep CSS_GENERATED_RS → 0` | re-entry | `verbatim_blob_present==false` |
| **distinct-grammar-output** | 7× md5 `b654562c…` | A§3.1, C-P3, D-I3, E-P3/B4, SYN-P3/PROVE, HO | md5-distinct + `const.*_RS` Sheets=0 + WIDENED structural row-count | re-entry | `generated_md5_distinct` + `generator_grammar_count==3` + `sheets_real_grammar` |
| **single-emitter-path** | `grammar_provider.rs:40/:110` + `lib.rs:282/:291` + `runtime_generator.rs:17/:25` | A§3.1, C§2.3, D-I4, E-B1, SYN-G3, HO | `RuntimeEmitterKind → 0` AND FULL-alphabet arm census AND type census AND `runtime_target_rows_collapsed` (widened) | fork / relocated-seam | `emitter_fork_present` + `…branch_count==0` + `…type_count==0` + `runtime_target_rows_collapsed` |
| **phantom-generic** | `tape/mod.rs:175 G=AnyGrammar`; only `G` witness in `event_grammar_tests.rs` (`#[cfg(feature=proof)]`) | A§3.3, D-I5, E-B3, SYN-G4, HO | ≥2 real-prod OR `G` removed; `_proof_compiles` excluded; DELETE default | re-entry | `phantom_generic_resolved` + `shared_value_trait_instantiations≥2` + `json_rich_navigation_preserved` |
| **timed-plane + corpus-in-timer** | `nonjson_css_l4.rs:66/:1989/:3091` warm 187B SHA fixtures | A§3.2, B§3.2, C-P2/§2.4, D-I9, E-A/B2, SYN-H1, HO | P2 delete + H1 frame | corpus-out-of-timer | `materialization_framing` + `corpus_in_timer==true` |
| **acceleration-wiring** | `runtime/src/lib.rs:574/:598/:608/:629/:638` inside `:51 #[cfg(test)]`; `count_top_level_commas → gen:809` cold (2-of-3 dead) | A§3.4, C§2.6, D-I6/DM3, E-B4, SYN-G6, HO | grep ≥1 non-`cfg(test)` caller; retire gated on samply non-top-N | acceleration claim | `acceleration_at_admission` (NOT `cfg-test-only`) |
| **x86-deleted (ALL surfaces, reach-matched)** | `src/x86_64/` (24/847) **AND** `ext/x86/` (3554) **AND** `build.rs` (102) **AND** `Cargo.toml:19` nasm-rs **AND** `lib.rs:5`/`:247`/`:285-288` | A§0/§3.2/§5 (V5 R-1), C§6-FOLD-1, D-I7-disp, E-P1/F15, SYN `:315`/`:563`, HO inv-3 | `find …/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm'` neutral-only; deletion list = grep reach | x86/AVX/SVE/nasm | `x86_tree_deleted` (crate-wide, reach-matched) |

Every addendum has all five columns populated against a CH7-verified surface. The
no-contrivance / x86-deleted / 7-replica-collapsed trio is fully covered — and the x86-deleted row,
the ONE that lagged across four prior passes (V1 alphaA count, V3 alphaD count, V4 alphaA scope), is
now CONCORDANT crate-wide AND reach-matched to the verify grep across the entire cohort.

---

## §9 — Disposition summary

| # | Section | Disposition | Note |
|---|---|---|---|
| 1 | alphaA (all sections; the V4 §3.2/§0/§5 x86 REVISE FOLDED at V5 R-1) | **ACCEPT** | x86 census crate-wide on BOTH surfaces + reach (`build.rs`/`Cargo.toml`/`lib.rs`); close-gate crate-wide; V5 FOLD log retracts the inaccurate "ZERO V3 REVISE" claim; §3.4 remains the model checkasm framing |
| 2 | alphaB (all sections) | **ACCEPT** | honest-`None` posture; the five V4 REVISE clusters touch no αB section; asmjson-AVX512-OUT makes no x86-gone close-claim |
| 3 | alphaC (all sections) | **ACCEPT** | §6 FOLD-1 the canonical crate-wide second-x86-surface fold; alphaA now concordant with it |
| 4 | alphaD (all sections incl §1 V4 + §8.V5) | **ACCEPT** | checkasm 18→14 stays folded; §8.V5 FOLD log records V4 dispositions with fold-stable anchors (cluster-5); I7 claim-row disposes to crate-wide P1 |
| 5 | alphaE (all clusters; the two V4 αE REVISEs FOLDED as F15+F16) | **ACCEPT** | F15 x86 scope crate-wide; F16 projection tuple widened to all non-path columns (disk-grounded by 7-distinct-`fact_schema`); no candidate added/removed |
| 6 | SYNTHESIS.md (all sections) | **ACCEPT** | triple-binding intact; `x86_tree_deleted` crate-wide AND reach-matched (cluster-2); `runtime_target_rows_collapsed` projects onto all non-path columns (cluster-3); honest-finding escape gated (a)-(c) |
| 7 | HANDOFF.md (all sections) | **ACCEPT** | six addenda + all V4 folds carried verbatim (x86 reach-matched inv-3; widened relocated-seam inv-5); pre-block re-entry forbiddances complete |

**CH7 V5 verdict.** The six new addenda fire HONESTLY across the cohort — each named against a
CH7-independently-re-verified live surface at HEAD `318d9c046` (`:701`; `:40/:110` + `:282/:291` +
`:17/:25`; md5 7→1; `:175 G=AnyGrammar` with the sole witness confined to the
`#[cfg(feature=proof)]`-gated `event_grammar_tests.rs`; `runtime/src/lib.rs:51 #[cfg(test)]` with
2-of-3 CSS NEON kernels dead and count-commas cold; warm 187B SHA fixtures), each carrying a
grep-able close gate, a §0.4 pre-block, and a §2 machine-checkable telemetry column. The αF contract
(SYNTHESIS + HANDOFF) carries all six addenda AND folds all FIVE V4 CONSOLIDATED REVISE clusters
correctly: the x86 deletion is crate-wide AND reach-matched to the verify grep (cluster-1+2, the
`Cargo.toml:19` nasm-rs dep + `lib.rs:5`/`:285-288` cfg-arms now on the removal list); the
relocated-seam structural check is widened to all non-path `RuntimeTarget` columns (cluster-3,
empirically grounded — disk-confirmed the 7 css_l4 rows share `(source_roots,entry_rule)` but carry
7 distinct `fact_schema`); the αA and αE x86-scope orphans are folded (cluster-1+4); the fold-ledger
anchors are fold-stable (cluster-5).

**The lens did not rubber-stamp a "redress landed" claim.** CH7 V5 independently disk-verified the
load-bearing F16 empirical (7-distinct-`fact_schema` in `regen_css.rs`), the x86 reach surfaces
(`Cargo.toml:19`, `lib.rs:5`/`:247`/`:285-288`) the cluster-2 fold names, the phantom `G`
proof-feature gating, the acceleration `#[cfg(test)]` boundary, and the Sheets PROVE litmus honesty
(`google-sheets.bbnf` is a real 7681-B Pratt grammar NOT yet in the skinny tree — so PROVE is a
genuine third grammar, not a relabeled stub). The recurrent straggler pattern this wave surfaced for
four passes (V1 alphaA x86 count, V3 alphaD checkasm count, V4 alphaA x86 scope) is, at V5, ABSENT:
alphaA is now concordant with alphaC §6 FOLD-1 / SYNTHESIS `:563` / HANDOFF inv-3; alphaE carries
F15+F16; the binding contract is reach-matched and projection-complete. No REVISE: every V4
disposition folded orphan-free with a concrete, disk-grounded mechanism, every fold a tightening.
No REJECT: nothing in the cohort overclaims a prune, mis-attributes an addendum, or admits a
contrivance as the bar.

7 sections dispositioned: 7 ACCEPT, 0 REVISE, 0 REJECT. **CH7 V5 CONVERGES** — the second
consecutive ≥95% CH7 cycle, per §3Z.

TALLY accept=7 revise=0 reject=0
