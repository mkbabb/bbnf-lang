# CH7 — OVERFIT-PRUNE (V2)

Lens: CH7 OVERFIT-PRUNE. Pass: PASS-ALPHA SK-V17→SK-V18 cycle **V2** (the GENERALIZATION
cycle / inflection backtrack). Per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z. Reviewer focus:
the SIX NEW CHALLENGE addenda fire honestly across the alpha artefacts — **verbatim-blob**
(const-`&str` `@generated` = hand-written), **distinct-grammar-output** (N grammars = N
non-identical `generated.rs`), **single-emitter-path** (flag/enum forks), **phantom-generic**
(uninstantiated `<G>`), **timed-plane-symmetry + corpus-in-timer**, **acceleration-wiring**
(NEON at admission, not `#[cfg(test)]`) — plus no-contrivance, x86 deleted, 7-replica collapsed.

Subject reviewed: `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`. Per
PASS-ALPHA §2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md` (no separate `alphaF-*.md`;
the V1 CONSOLIDATED §1 records this is by-design, not a defect).

**V2 context.** V1 of this CHALLENGE wave returned 74.2% ACCEPT / 0 REJECT across seven lenses
(CH7 itself: 7 ACCEPT, 1 REVISE, 0 REJECT). The 24 REVISEs folded into the α-F contract
(`SYNTHESIS.md`/`HANDOFF.md` are now their V2 revision) per CONSOLIDATED §3; the lone CH7 REVISE
(alphaA §3.2 x86 census 23-vs-24) folded into the research artefact. This V2 report **re-verifies
every addendum surface live at HEAD `318d9c046`** AND confirms the V1 folds landed, then
re-dispositions each section. CH7 does not accept on the artefacts' word — every claim is
re-grepped against the tree.

---

## §0 — Independent verification log (CH7 re-grep, HEAD `318d9c046`)

| Claim under the addenda | CH7 command | Result | Verdict |
|---|---|---|---|
| **verbatim-blob** (CSS const-`&str`) | `grep -n 'const CSS_GENERATED_RS' codegen/src/runtime_generator.rs` | `701:const CSS_GENERATED_RS: &str = r#"` | CONFIRMED |
| **single-emitter-path** (fork) | `grep -rn 'enum RuntimeEmitterKind\|RuntimeEmitterKind::' codegen/src` | `grammar_provider.rs:40 pub enum RuntimeEmitterKind`; `:110 != RuntimeEmitterKind::RequestFacts`; `lib.rs:282 CompiledLowering` / `:291 RequestFacts`; `runtime_generator.rs:17/:25` match-arms | CONFIRMED (fork live at admission) |
| **distinct-grammar-output** (7 replicas) | `md5 -q runtime/src/grammars/css_l4_*/generated.rs \| sort \| uniq -c` | `7  b654562ccff46ed62dd48e9ace325830` (7→1 byte-identical) | CONFIRMED |
| **phantom-generic** (`<G>` default; no real prod instantiation) | `grep -n 'EventGrammar = AnyGrammar' tape/mod.rs`; `grep -rn 'SheetsEventGrammar\|ValueRef<.*EventGrammar' runtime/src` | `tape/mod.rs:175: K = AnyKind, G: EventGrammar = AnyGrammar`; the ONLY non-default `ValueRef<…,G>` (`event_grammar_tests.rs:89 …, JsonEventGrammar>`) is in a `_tests.rs` file; `SheetsEventGrammar`/`JsonEventGrammar` referenced ONLY via `_proof_compiles::<…>` in `event_grammar_tests.rs` | CONFIRMED — **zero** non-test production `G` instantiation |
| **acceleration-wiring** (CSS NEON dead at admission) | enclosing scope of `find_css_significant`/`find_comment_close` callers (`lib.rs:574,598,608`); CSS production caller census | `mod tests` @ `lib.rs:51-52 #[cfg(test)]`; callers at `:574/:598/:608` are inside `#[test] fn neon_*`; **only** `count_top_level_commas` reaches a production `generated.rs:810` (×7), and that path is the COLD rich-summary | CONFIRMED dead-at-admission (2-of-3 CSS NEON consumers) |
| **corpus-in-timer / timed-plane** (OLD warm bench) | (carried from V1; no source churn at HEAD) | `nonjson_css_l4.rs` warm `measure_mbps` + 85–357B SHA fixtures live | CONFIRMED live |
| **x86 census** | `find …/x86_64 -type f \| wc -l`; `-name '*.rs' \| wc -l`; `-name '*.asm'`; `.rs` LOC | **24 files = 23 `.rs` + 1 `.asm`** (`byte_class_from_eq_set_64.asm`), **742 LOC** | CONFIRMED — matches the V1-folded alphaA framing |
| metalang leak | `grep -c parse_w11_1_number runtime/src/grammars/json/generated.rs` | `7` | CONFIRMED |
| EventGrammar impls (prod) | `grep -rn 'impl EventGrammar for' runtime/src` | `SheetsEventGrammar`, `JsonEventGrammar`, `AnyGrammar` — all three impl the trait; **none** is used as a non-default `G` outside tests | CONFIRMED (impl ≠ instantiation) |
| Sheets stub LOC | `find skinny -path '*sheets_witness*' -name '*.rs' \| xargs wc -l` | **25 LOC** (`event_grammar_witness.rs:24` + `mod.rs:1`) | CONFIRMED — the "25-LOC stub" framing is correct (worktree copies inflate a naïve repo-wide count to 400; skinny tree is 25) |
| Sheets `.bbnf` exists | `find . -name google-sheets.bbnf` | `grammar/google-sheets/google-sheets.bbnf` present (totality tree) | CONFIRMED — CH2 V1 claim sound; PROVE adopts it |

**V2 ground-truth correction census: ZERO.** Unlike V1 (which surfaced the 23-vs-24 x86
discrepancy), V2 finds no new ground-truth divergence. The single V1 census defect is folded
(see §1). Two sharpenings strengthen — not weaken — the existing dispositions (§1, §4 below).

---

## §1 — alphaA (results extraction) — the overfit inventory

The V1 CH7 REVISE was the x86 census ("23 `.rs`, 742 LOC" → "24 files = 23 `.rs` + 1 `.asm`").
CH7 re-verifies the fold landed and the surface is unchanged.

**V1 REVISE fold — VERIFIED CLOSED.** `alphaA-results-extraction.md:14-17` now carries the
explicit fold note ("the x86 tree is **24 files = 23 `.rs` + 1 `.asm`** (`byte_class_from_eq_set_64.asm`),
742 LOC, 14 `unimplemented!` … Verified on disk"); `:50`, `:159`, `:241`, `:275-276`, `:295`
all read "24 files (23 `.rs` + 1 `.asm`)". CH7 re-counted the tree on disk: 24 total, 23 `.rs`,
1 `.asm` (`byte_class_from_eq_set_64.asm`), 742 LOC. The P1 close gate (`find …/x86_64 -type f`
= 0) now deletes the `.asm` too. **The REVISE is fully discharged.**

| Section | Addendum coverage | Disposition |
|---|---|---|
| §0 headline 6-axis table | all six axes named; "OVERFIT (P1 delete)" / "verbatim-blob" / "phantom-generic" tags; aarch64-VIOLATED row now "24 files = 23 `.rs` + 1 `.asm`" (`:50`) | **ACCEPT** |
| §1 JSON >sonic-strict per-corpus Δ | the >SOTA PRESERVE bar (not an addendum) | **ACCEPT** |
| §2 CSS 1.996–3.348× + §2.1 lazy-vs-eager | timed-plane-symmetry (H1) pin | **ACCEPT** |
| §3.1 generator-does-not-exist | verbatim-blob (`:701`) / single-emitter-path (`:40/:110`) / distinct-grammar-output (md5 7→1); JSON templates `:151` enumerated (`:195/:550/:572/:594/:598/:612/:665`) | **ACCEPT** — all live-cited |
| §3.2 contrivance/wrong-arch (x86 + OLD bench + metalang + gate holes) | timed-plane / corpus-in-timer / x86-census — **now 24-file framed** | **ACCEPT** (was the V1 REVISE; fold verified) |
| §3.3 phantom + divergent value API | phantom-generic; distinguishes phantom `<G>` from real `ValueRef<Kind>` (the two-axis precision V1/CH5 demanded — present here) | **ACCEPT** |
| §3.4 NEON wiring honesty (`:187-190`) | acceleration-wiring: `find_css_significant`/`find_comment_close` "dead at admission" inside `#[cfg(test)]`; "Only `count_top_level_commas` reaches a generated module, in the **cold** rich-summary"; W3 commit-title overstatement named; 5 scalar passthroughs + UDOT orphan + PMULL/TBX/CSSC enumerated | **ACCEPT** — the 2-of-3 partial-wiring nuance is stated correctly (CH7 verified `:574/:598/:608` test-only AND `generated.rs:810` count-commas prod) |
| §4 substrate / §5–§7 close-seeds | Lock 1 holds; binding inventory; `:263-267` NEON-wiring nuance reprised | **ACCEPT** |

**alphaA overall: ACCEPT (all sections).** The single V1 REVISE is folded and verified; the
NEON-wiring §3.4 is the model anti-overclaim disclosure (it states what is wired — cold
count-commas — and what is NOT — the two `#[cfg(test)]`-only kernels — rather than papering the
W3 "NEON acceleration" commit title as fully live).

---

## §2 — alphaB (competitor deltas) — the bar to preserve

CH7's overfit lens on alphaB: (a) the CSS lazy-vs-eager asymmetry is disclosed, not papered as
equal-work (timed-plane-symmetry); (b) no un-run comparator is fabricated as a number
(corpus-in-timer / contrivance).

| Section | Disposition |
|---|---|
| §0 standing + asymmetry pin (JSON near-symmetric strict; CSS asymmetric lazy-vs-eager) | **ACCEPT** — asymmetry up front is timed-plane-symmetry done right |
| §1 JSON strict-vs-strict bar (sonic strict Skipper, no `utf8_lossy`; apache_builds canary) | **ACCEPT** — correct strict comparator plane |
| §1.3 simdjson DOM = different output plane, NOT the strict bar | **ACCEPT** |
| §1.4 Track 2 typed caveat (conditional on hand-tuned per-corpus schema; NOT the unconditional bar) | **ACCEPT** |
| §2 CSS lazy-vs-eager (track1_rich vs lightningcss full-CSSOM; 25-33% rich rider; keeper `css_canon_bench`) | **ACCEPT** — the load-bearing H1 disclosure |
| §3.3 NOT-runnable comparators honest `None` (yyjson/asmjson/RapidJSON; asmjson AVX-512 x86-only OUT) | **ACCEPT** — the strongest anti-contrivance posture in the cohort; pre-empts the fabricated-competitor-number failure mode the addenda guard |
| §3.4 H1 options (symmetric comparator OR rename+footnote; "silence does not preserve H1") | **ACCEPT** |
| §4 preservation bar table (per-grammar must-hold + canary + risk) | **ACCEPT** |

**alphaB overall: ACCEPT.** CH7 re-confirms the V1 disposition: the §3.3 honest-`None` posture
is the precise foreclosure of the corpus-in-timer / fabricated-competitor contrivance. The V1
non-blocking framing note (αB headlines CSS at the live N=80 "1.9-2.9×" while alphaA/SYNTHESIS
use the N=200 W5-close "1.996-3.348×", with provenance disclosed at `αB:157`) remains a
measurement-N disclosure, not a contradiction — both are the PRESERVE bar, not a growth target.
ACCEPT as-is.

---

## §3 — alphaC (REDRESS digest) — the PRUNE waves + pre-blocks

The most addendum-dense artefact: P1-P5 as PRUNE waves; the six pre-block families re-keyed to
the generator surfaces.

| Section | Addendum | Disposition |
|---|---|---|
| §0 framing + state-delta (`emit_fact_stream` already gone, grep=0) | n/a | **ACCEPT** |
| §1-P1 delete x86 (742 LOC / 24 files / 14 `unimplemented!` / 0 intrinsics) | x86-deleted | **ACCEPT** — "24" correct; close gate `find … = 0` deletes the `.asm` |
| §1-P2 delete OLD bench (`measure_mbps` warm, 85-357B SHA fixtures) | corpus-in-timer / timed-plane | **ACCEPT** — keep `css_canon_bench` |
| §1-P3 collapse 7 replicas (md5 single-hash) | distinct-grammar-output | **ACCEPT** — close gate binds the diff-census |
| §1-P4 fix gate holes (extend `GENERIC_SCAN_ROOTS`; drop `diagnostic-x86`; abrogate-before-patch) | gate-scope | **ACCEPT** |
| §1-P5 purge metalang (`parse_w11_1_number ×7`; gated by G1) | metalang | **ACCEPT** |
| §2.1 AZ-IV eager → re-open on G1/G2/G4 | pre-block | **ACCEPT** |
| §2.2 StructRegistry → re-open on G3/G4 (Lock 2 `Layout` not `StructLayout`) | pre-block | **ACCEPT** |
| §2.3 fact-stream → residual fork (`CSS_GENERATED_RS` const `&str` + `RequestFacts`) | **verbatim-blob + single-emitter-path** | **ACCEPT** — the retirement-clause re-open test is the precise addendum binding |
| §2.4 24-broadcast → PERMANENT; §2.5 FNV/fixture → PERMANENT | n/a | **ACCEPT** |
| §2.6 x86/AVX/SVE → PERMANENT (rebuild + G6 ASM backlog) | acceleration-wiring | **ACCEPT** |
| §3 single distinction + corollary ("checked TWICE — runtime output AND the emitter that produces it") | all | **ACCEPT** — the load-bearing pre-block insight (a refuted carrier can re-land at its SOURCE) |

**alphaC overall: ACCEPT (all sections).** Unchanged from V1; CH7 re-verified the §2.3 residual
fork (`CSS_GENERATED_RS:701` + `RuntimeEmitterKind:40/110`) and the §3 "generator is the new
carrier surface" corollary — both live. No REVISE/REJECT.

---

## §4 — alphaD (validated/invalidated ledger)

alphaD's INVALIDATED table (I1-I10) attaches each addendum lens to a verified surface. CH7
re-greps every tagged surface.

| Row | Addendum lens | CH7 re-verify | Disposition |
|---|---|---|---|
| I1 CSS grammar-driven invalidated | verbatim-blob | `:701` ✓ | **ACCEPT** |
| I2 JSON projects from grammar | verbatim-blob | `json_sink_direct` templates ✓ | **ACCEPT** |
| I3 7 sub-grammars admitted | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** |
| I4 one codegen path | single-emitter-path | `:40/:110` ✓ | **ACCEPT** |
| I5 `ValueRef<G>` parametric | phantom-generic | `:175 G=AnyGrammar`; 0 prod-`G` instantiation (only `_proof_compiles` in `_tests.rs`) ✓ | **ACCEPT** |
| I6 NEON CSS-scan acceleration | acceleration-wiring | `mod tests:52`; 2-of-3 dead, count-commas cold ✓ | **ACCEPT** |
| I7 aarch64-only | x86-deleted | 742 LOC / 24 files ✓ | **ACCEPT** |
| I8 Lock-14 gate meaningful | gate-scope | `:2409` exclusion ✓ | **ACCEPT** |
| I9 equal-work CSSOM | timed-plane / H1 | track1_4field vs rich ✓ | **ACCEPT** |
| I10 clean shipped symbols | metalang | `×7` ✓ | **ACCEPT** |
| §3 DEMOTED DM1-DM4 (typed conditional; substrate-ready-not-proven; 5 scalar passthroughs; UDOT orphan) | `_neon`-suffix-truth | F6/F7 ✓ | **ACCEPT** |
| §4 STILL-OPEN S1-S13; §5 pre-blocked (8 families); §6 self-verification log | all | maps 1:1 to INVALIDATED; self-greps match CH7 | **ACCEPT** |

**alphaD overall: ACCEPT.** Every addendum lens attaches to a CH7-confirmed surface; the §6
self-verification log is the discipline CH7 wants (the artefact greps what it asserts). I5 is
sharpened by CH7's V2 finding (the only non-default `G` instantiation is `JsonEventGrammar` in
`event_grammar_tests.rs:89` — a `_tests.rs` file — confirming I5's "0 production instantiation").

---

## §5 — alphaE (candidate shortlist) — the falsifiability triple

alphaE folds 13 backlog items into 5 clusters (A, B1-B4) under a falsifiability **triple**
(preserved->SOTA / grammar-derivation / distinct-grammar-output).

| Cluster | Addendum gate | Disposition |
|---|---|---|
| §0 triple (PRESERVED / GRAMMAR-DERIVATION mutate-`.bbnf`→output-changes / DISTINCT-OUTPUT) | the three load-bearing gates | **ACCEPT** — "a const courier cannot pass" is the exact operational falsifier for verbatim-blob |
| A PRUNE (P1-P5, ≈−7100 LOC) | x86=0; replicas collapsed; gate meaningful; metalang purged | **ACCEPT** — pure deletion, LOW risk, entry-gate for B |
| B1 un-fork + project JSON (G3+G1; `grep RuntimeEmitterKind → 0`) | single-emitter-path / verbatim-blob | **ACCEPT** — JSON-first ordering justified; apache_builds +1.4% the hard canary |
| B2 derive CSS (G2; `grep CSS_GENERATED_RS → 0`) | verbatim-blob (centrepiece) | **ACCEPT** — LOW risk (scalar hot path) |
| B3 shared trait + kill phantom (G4+H1; instantiate-XOR-delete) | phantom-generic / timed-plane | **ACCEPT** — structurally verifiable; preserve-rich-ast guarded |
| B4 PROVE Sheets + NEON (3 distinct `generated.rs`; acceleration-at-admission) | distinct-grammar-output / acceleration-wiring | **ACCEPT** — the sharpest litmus; same-wave-consumer prevents orphan kernels |
| SUMMARY + cross-cutting 1-6 (sequencing; kept-honest artefacts; net ≈−9150 LOC) | all | **ACCEPT** — "deletes more than it adds" is the correct generalization-cycle shape |

**alphaE overall: ACCEPT.** The falsifiability triple converts every addendum into a grep-able
exit gate. Cross-cutting note 2 (honest-finding escape: a surviving hand-shaping becomes a NAMED
grammar-parameterized primitive, never a silent `_RS` blob) is abrogate-before-patch applied
correctly. No REVISE/REJECT. (CH7 notes for the record: the V1 CONSOLIDATED routed CH4's αE
checkasm "18"→"~12" and CH2's S12-Pratt-owner sharpenings to the research authors; these are
non-addendum precision items outside CH7's lens — CH7 takes no disposition on them.)

---

## §6 — SYNTHESIS.md (the αF contract) — the goalset (V2-folded)

This is the master αF output. CH7 confirms (a) the six addenda each carry a close-gate + a
pre-block + a machine-checkable telemetry column, AND (b) the seven V1 root-cause folds landed.

### §6.1 — V1 fold verification (the seven CONSOLIDATED §3 root causes)

| # | V1 root cause | Fold site | CH7 verdict |
|---|---|---|---|
| 1 | JSON range understated (+1.4%–78% → +1.4%–164.7%) | `SYNTHESIS:15,120-121,188,341` ("widest unicode_escapes; +1.4% apache_builds thinnest"); HANDOFF `:9,29-30` | **FOLDED** ✓ |
| 2 | yyjson/asmjson/RapidJSON honest-`None` | `SYNTHESIS:16,323` ("FFI NOT wired in `Cargo.toml` … honest `None` on aarch64") | **FOLDED** ✓ |
| 3 | Lock-14 canonical + md5 necessary-not-sufficient | `SYNTHESIS:17-18,172` (canonical three-surface model + `match grammar`-arm grep co-gate; `generator_grammar_branch_count == 0`; "md5-distinctness alone is necessary-not-sufficient") | **FOLDED** ✓ |
| 4 | Sheets sourcing (adopt Pratt, not author stub) | `SYNTHESIS:19,176,222,399` (ADOPT `grammar/google-sheets/google-sheets.bbnf`; `sheets_grammar_shape == pratt-operator`; "a third JSON/flat-stream would hollow the litmus") | **FOLDED** ✓ — CH7 verified the `.bbnf` exists |
| 5 | `ValueRef` two-axis (`G` phantom vs `K=Kind` real) | `SYNTHESIS:173,219` (the G4 target is the `G: EventGrammar` axis, NOT the already-real `K=Kind`; DELETE the abrogate-before-patch default; trait separable from `<G>`) | **FOLDED** ✓ — matches CH7 ground truth `tape/mod.rs:175 K=AnyKind, G:EventGrammar=AnyGrammar` |
| 6 | Trait LCD false-green | `SYNTHESIS:173,395,414,427` (`json_rich_navigation_preserved == true` in the gate REJECT set; "≥2 impl-count without rich-nav is an LCD regression") | **FOLDED** ✓ |
| 7 | Deferred revert/cap | `SYNTHESIS:22,221,476` (G6 retire gated on a samply non-top-N MEASUREMENT row; revert dependency graph PRUNE→G1→G2→G3→G4→G5/G6→PROVE→H1; dispatch-hard-cap defaults) | **FOLDED** ✓ |

All seven V1 folds verified present and correctly sited.

### §6.2 — Addendum triple-binding (close gate + pre-block + telemetry)

| Surface | Close gate | Telemetry column | Pre-block | Disposition |
|---|---|---|---|---|
| G2 verbatim-blob | `grep CSS_GENERATED_RS → 0`; `grammar_derived` true | `grammar_derived` (`:388`) | §0.4 verbatim-blob re-entry | **ACCEPT** |
| G3 single-emitter | `RuntimeEmitterKind` gone + canonical `match grammar`-arm grep 0 (`:172`) | `generator_grammar_branch_count == 0` | §0.4 fork resurrection | **ACCEPT** — the md5-necessary-not-sufficient co-gate (V1 fold #3) closes the "neutral output from a branching body" hole |
| G4 phantom-generic | `G` instantiated w/ prod grammar OR removed; "`_proof_compiles` does NOT count" (`:173`) | `phantom_generic_resolved` + `shared_value_trait_instantiations≥2` + `json_rich_navigation_preserved` | §0.4 phantom re-entry | **ACCEPT** — the explicit `_proof_compiles` exclusion (CH7 verified that IS the only `G` witness, in `_tests.rs`) is precise |
| G6 acceleration-wiring | reached at admission (grep hot path not tests); retire branch gated on a samply non-top-N row (`:221`) | `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` NOT `cfg-test-only` (`:400,418-419,429-430`) | §0.4 acceleration claim | **ACCEPT** — the measurement-gated retire (V1 fold #7) forecloses the "mark everything retired with zero wiring" paper-close |
| PROVE distinct-grammar-output | Sheets `generated.rs` md5 ≠ JSON ≠ CSS; `grep const.*_RS Sheets blob = 0`; `sheets_grammar_shape == pratt-operator` (`:176,399`) | `generated_md5_distinct` + `sheets_grammar_shape` | §0.4 distinct-output re-entry | **ACCEPT** |
| H1 timed-plane-symmetry | equal work, real corpus cold, no micro-fixtures; P2 deletes warm bench | `corpus_in_timer == true` (`:429`) | §0.4 corpus-out-of-timer / more-work | **ACCEPT** |
| P1/P4 x86-deleted + gate | `find …/x86_64 -type f = 0` (all 24 files incl `.asm`); P4 gate scans the leak surface | `x86_tree_deleted == true` (`:419`) | §0.4 x86/AVX/SVE | **ACCEPT** |

**SYNTHESIS.md overall: ACCEPT (all sections).** Every one of the six addenda is bound THREE
ways — (1) a §0.1 close gate, (2) a §0.4 pre-block re-entry forbiddance, (3) a §2 telemetry
column the `gate-json` consumer REJECTs on — AND every V1 root-cause fold landed and is correctly
sited. The G3 `generator_grammar_branch_count == 0` co-gate (V1 fold) is the most consequential
V2 hardening for CH7's lens: it closes the distinct-grammar-output false-pass where three
md5-distinct files emerge from a single grammar-branching emitter body. No REVISE/REJECT.

---

## §7 — HANDOFF.md (the αF packet)

CH7 confirms the six addenda + the seven V1 folds are carried verbatim into S-P0+ with re-entry
pre-blocks.

| Section | CH7 verdict |
|---|---|
| Gate Posture / current-state block | JSON range "+1.4%–164.7%" (`:9,29-30`); `ValueRef<G: EventGrammar>` PHANTOM "test-only `_proof_compiles`" (`:54`) — matches CH7 ground truth | **ACCEPT** |
| Addenda block (six verbatim) | verbatim-blob / distinct-grammar-output / single-emitter-path / phantom-generic / timed-plane-symmetry+corpus-in-timer / acceleration-wiring all carried with one-line binding | **ACCEPT** |
| Pre-Blocked Routes | const-`&str` courier re-entry; second uninstantiated `<G>`; byte-identical replicas; timed-plane-asymmetry / corpus-out-of-timer / more-work | **ACCEPT** |
| Lock-14 dual-grep invariant (`:225,228-230`) | "(i) the forbidden-token scan AND (ii) the canonical `match grammar`-arm grep → 0 — they catch different leaks" (V1 fold #3 carried) | **ACCEPT** |
| `G`-axis phantom resolution (`:104`) | "INSTANTIATE-OR-DELETE the `G: EventGrammar` axis (NOT `K=Kind`, already real)" (V1 fold #5) | **ACCEPT** |
| Sheets adoption (`:115-116,120`) | ADOPT existing Pratt `google-sheets.bbnf`; "do NOT author a fresh 'third JSON' stub"; honest-finding candidate if Pratt cannot lower (V1 fold #4) | **ACCEPT** |
| Revert graph + hard-caps (`:295`) | research/plan/redress 20/15/30, "at 0.9N commit, at N halt" (V1 fold #7) | **ACCEPT** |
| Next-Move sequencing (P4 gate-meaningful before G2/G3 rebuild) | the correct dependency order — prevents B1/B2 re-leaking under a blind gate | **ACCEPT** |

**HANDOFF.md overall: ACCEPT.** Six addenda carried verbatim; all seven V1 folds present; each
addendum has a pre-block re-entry forbiddance; the Lock-14 dual-grep (token scan AND arm census)
is the V2 hardening that makes the P4 gate trustworthy as the emitter is rebuilt. No REVISE/REJECT.

---

## §8 — Cross-artefact addendum coverage matrix (CH7 V2 summary)

| Addendum | Live surface (CH7-verified @ `318d9c046`) | Named in | Close gate | Pre-block | Telemetry |
|---|---|---|---|---|---|
| **verbatim-blob** | `runtime_generator.rs:701` const `&str` | A§3.1, C§2.3, D-I1, E-B2, SYN-G2, HO | `grep CSS_GENERATED_RS → 0` | §0.4 verbatim-blob re-entry | `grammar_derived` |
| **distinct-grammar-output** | 7× md5 `b654562c…` | A§3.1, C-P3, D-I3, E-P3/B4, SYN-P3/PROVE, HO | md5-distinct census + `const.*_RS` Sheets blob = 0 | §0.4 distinct-output re-entry | `generated_md5_distinct` + `sheets_grammar_shape` |
| **single-emitter-path** | `grammar_provider.rs:40/:110` + `lib.rs:282/:291` + `runtime_generator.rs:17/:25` | A§3.1, C§2.3, D-I4, E-B1, SYN-G3, HO | `grep RuntimeEmitterKind → 0` AND `match grammar`-arm grep → 0 | §0.4 fork resurrection | `generator_grammar_branch_count == 0` |
| **phantom-generic** | `tape/mod.rs:175 G=AnyGrammar`; only `G` witness in `event_grammar_tests.rs:89/:20/:44` (`_tests.rs`) | A§3.3, D-I5, E-B3, SYN-G4, HO | ≥2 real OR `G` removed; `_proof_compiles` excluded | §0.4 phantom re-entry | `phantom_generic_resolved` + `json_rich_navigation_preserved` |
| **timed-plane + corpus-in-timer** | `nonjson_css_l4.rs` warm SHA fixtures | A§3.2, B§3.2, C-P2/§2.4, D-I9, E-A/B2, SYN-H1/§0.6, HO | P2 delete + H1 frame | §0.4 corpus-out-of-timer / more-work | `corpus_in_timer == true` |
| **acceleration-wiring** | `lib.rs:574/:598/:608` inside `mod tests:52`; `count_top_level_commas` → `generated.rs:810` cold (2-of-3 dead) | A§3.4, C§2.6, D-I6/DM3, E-B4, SYN-G6, HO | grep ≥1 non-`cfg(test)` caller; retire gated on samply non-top-N | §0.4 acceleration claim | `acceleration_at_admission` (NOT `cfg-test-only`) |

Every addendum has all five columns populated against a CH7-verified surface. The **no-contrivance
/ x86-deleted / 7-replica-collapsed** trio is fully covered (P1 close `find … = 0` deletes all 24
files; P2 deletes the warm bench; P3 md5-distinct census). The V2 hardenings —
`generator_grammar_branch_count == 0` (md5 necessary-not-sufficient), `json_rich_navigation_preserved`
(trait LCD false-green), the samply-gated G6 retire, and the `_proof_compiles` exclusion — close
four narrative-satisfiable-but-code-violable holes that V1 left open.

---

## §9 — Disposition summary

| # | Section | Disposition | Note |
|---|---|---|---|
| 1 | alphaA (all sections, incl. §3.2 x86 census) | ACCEPT | V1 REVISE folded + verified (24 files = 23 `.rs` + 1 `.asm`) |
| 2 | alphaB (all sections) | ACCEPT | honest-`None` posture re-confirmed |
| 3 | alphaC (all sections) | ACCEPT | §2.3 residual fork + §3 corollary re-verified live |
| 4 | alphaD (all sections) | ACCEPT | I5 sharpened (only `G` witness in `_tests.rs`) |
| 5 | alphaE (all clusters) | ACCEPT | falsifiability triple intact |
| 6 | SYNTHESIS.md (all sections) | ACCEPT | seven V1 folds verified + triple-binding intact |
| 7 | HANDOFF.md (all sections) | ACCEPT | six addenda + folds carried verbatim |

**CH7 V2 verdict.** The six new addenda fire HONESTLY and exhaustively across the cohort — each
named against a CH7-independently-re-verified live surface at HEAD `318d9c046` (`:701`; `:40/:110`
+ `:282/:291` + `:17/:25`; md5 7→1; `:175 G=AnyGrammar` with the only `G` witness confined to
`event_grammar_tests.rs`; `mod tests:52` with 2-of-3 CSS NEON kernels dead and count-commas cold;
warm SHA fixtures), each carrying a grep-able close gate, a §0.4 pre-block re-entry forbiddance,
and a §2 machine-checkable telemetry column. The single V1 CH7 REVISE (alphaA x86 census 23→24)
is FOLDED and VERIFIED CLOSED. All seven V1 CONSOLIDATED root-cause folds landed in the α-F
contract and are correctly sited, AND four of them (md5 necessary-not-sufficient,
rich-nav-preserved, samply-gated G6 retire, `_proof_compiles` exclusion) directly harden CH7's
lens against narrative-pass/code-violation. No contrivance survives the goalset; x86 deletion
(P1, all 24 files) and 7-replica collapse (P3) are correctly gated. **Zero orphan REVISE; zero
REJECT; the cohort is at full ACCEPT for CH7's lens — convergence-ready per §3Z.**

7 sections dispositioned: 7 ACCEPT, 0 REVISE, 0 REJECT.

TALLY accept=7 revise=0 reject=0
