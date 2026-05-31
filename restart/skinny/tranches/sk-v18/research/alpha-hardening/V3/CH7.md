# CH7 — OVERFIT-PRUNE (V3)

Lens: CH7 OVERFIT-PRUNE. Pass: PASS-ALPHA SK-V17→SK-V18 cycle **V3** (the GENERALIZATION
cycle / inflection backtrack). Per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z. Reviewer focus:
the SIX NEW CHALLENGE addenda fire HONESTLY across the alpha artefacts — **verbatim-blob**
(const-`&str` `@generated` = hand-written), **distinct-grammar-output** (N grammars = N
non-identical `generated.rs`), **single-emitter-path** (flag/enum forks), **phantom-generic**
(uninstantiated `<G>`), **timed-plane-symmetry + corpus-in-timer**, **acceleration-wiring**
(NEON at admission, not `#[cfg(test)]`) — plus no-contrivance, x86 deleted, 7-replica collapsed.

Subject reviewed: `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`. Per
PASS-ALPHA §2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md` (no separate `alphaF-*.md`).

**V3 context + posture.** V1 of this CHALLENGE wave returned CH7 7A/1R/0 (the lone REVISE: alphaA
x86 census 23→24, folded). V2 returned CH7 7A/0R/0. **A two-pass full-ACCEPT history is exactly
where a lens must NOT rubber-stamp.** This V3 report re-verifies EVERY addendum surface live at
HEAD `318d9c046`, confirms the V3 self-folds landed, AND adversarially hunts the cohort for
intra-artefact inconsistencies the prior passes missed. **CH7 V3 surfaces one genuine defect the
V1/V2 passes did not catch in this artefact**: a stale, inflated checkasm-harness count (`18`)
that survives un-folded in alphaD §1 V4 while the rest of the cohort — and the binding αF contract
— carries the disk-true `12+2=14`. This is precisely the P4-class false-gate inflation the cycle
exists to delete; it is a REVISE on alphaD, with a concrete fix below.

---

## §0 — Independent verification log (CH7 re-grep, HEAD `318d9c046`)

Every load-bearing overfit/prune claim re-verified before disposition. The addenda are NOT
accepted on the artefacts' word — each is confirmed against the tree this V3 pass.

| Claim under the addenda | CH7 command | Result | Verdict |
|---|---|---|---|
| **verbatim-blob** (CSS const-`&str`) | `grep -rn 'const CSS_GENERATED_RS' codegen/src/` | `runtime_generator.rs:701:const CSS_GENERATED_RS: &str = r#"`; consumed verbatim via `normalize(CSS_GENERATED_RS)` `:91` | CONFIRMED |
| **single-emitter-path** (fork) | `grep -rn 'enum RuntimeEmitterKind\|RuntimeEmitterKind::' codegen/src` | `grammar_provider.rs:40 pub enum RuntimeEmitterKind`; `:110 != …RequestFacts`; `lib.rs:282 CompiledLowering`/`:291 RequestFacts`; `runtime_generator.rs:17/:25` match-arms | CONFIRMED (fork live at admission) |
| **distinct-grammar-output** (7 replicas) | `md5 -q runtime/.../css_l4_*/generated.rs \| sort \| uniq -c` | `7  b654562ccff46ed62dd48e9ace325830` (7→1 byte-identical) | CONFIRMED |
| **phantom-generic** (`<G>` default; no prod instantiation) | `grep tape/mod.rs:175`; census non-default `ValueRef<…,G>` | `tape/mod.rs:175: K = AnyKind, G: EventGrammar = AnyGrammar`; **the ONLY** non-default `G` uses are `event_grammar_tests.rs:18/:20/:43/:44/:89` — and `event_grammar_tests` is `#[cfg(test)]`-gated (`tape/mod.rs:3`) AND the `JsonEventGrammar`/`SheetsEventGrammar` lines are FURTHER `#[cfg(feature="proof")]`-gated; the `:89 leak` is a STRING LITERAL inside an `fs::write` (a compile-fail fixture, not even in-tree code) | CONFIRMED — **even tighter than V2 stated**: zero prod `G`, and the witnesses are double-gated test + proof scaffolding |
| **acceleration-wiring** (CSS NEON dead at admission) | `#[cfg(test)]` boundary above `lib.rs:574/:598/:608`; prod reach of `count_top_level_commas` | `lib.rs:51 #[cfg(test)]` opens the enclosing `mod tests`; the three NEON callers (`:574/:598/:608`) AND the test wrappers (`:629/:638`) are all inside it; the **only** prod reach is `generated.rs:157 (count) → local count_top_level_commas (gen:810) → runtime_simd::count_top_level_commas` — the COLD rich-summary | CONFIRMED dead-at-admission (2-of-3 CSS NEON consumers; the 3rd is cold) |
| **corpus-in-timer / timed-plane** (OLD warm bench) | `grep nonjson_css_l4.rs` | `:66 EXPECTED_FIXTURE_BYTES = 187`; `:1989 input.len() != EXPECTED_FIXTURE_BYTES`; `:3091 fn measure_mbps` | CONFIRMED live |
| **x86 census** | `find …/x86_64 -type f`; `.rs` LOC; `.asm` LOC; `unimplemented!` | **24 files = 23 `.rs` (742 LOC) + 1 `.asm` (`byte_class_from_eq_set_64.asm`, 105 LOC) = 847 total**, **14** `unimplemented!` | CONFIRMED — matches alphaA/C/E/SYN/HO V3 dual-figure framing |
| metalang leak | `grep -o 'parse_w11_1_number[a-z_]*' json/generated.rs \| sort -u`; `grep -c` | `parse_w11_1_number_{direct,object_direct,array_direct}`; count `7` | CONFIRMED |
| **checkasm harness count** (the V3 defect surface) | `ls bbnf-simd/tests/checkasm_*.rs \| wc -l` | **14** = 12 single-kernel differentials + `checkasm_common.rs` (trampoline) + `checkasm_parity.rs` (aggregate). **NOT 18.** | CONFIRMED 14 — alphaD §1 V4 still says 18 (REVISE, §4) |
| fact-stream retired | `grep -c emit_fact_stream …/css_l4_declaration_values/generated.rs`; `codegen/src/lib.rs:298` | `0`; `:298` is the `W5C_REQUEST_FACT_PROFILES` RETIRED comment | CONFIRMED |
| canonical Lock-14 alphabet | `LOCKS.md:349` | Lock-14 verification cmd uses `GoogleSheets`/`GoogleSheetsParser` un-abbreviated + `match grammar { … GoogleSheets => }` | CONFIRMED — alphaB/E/SYN/HO `GoogleSheets` canonicalization sound |
| DocumentView sole impl | `grep -rn 'impl.*DocumentView.*for' runtime/src` | SOLE: `json/view.rs:68 impl DocumentView for JsonDocument` (CSS none) | CONFIRMED (αD S9 / SYN G4) |
| digit_mac UDOT orphan | `grep digit_mac.rs` | `:27 parse_4_digits_dotprod`, `:40 udot …` real asm; runtime callers = 0 | CONFIRMED (αD DM4 / αE B4) |
| sonic strict plane | `sonic_skipper.rs:3-7`; `Cargo.toml:23` | `IgnoredAny::deserialize` + `deserializer.end()`; `sonic-rs default-features=false features=["sort_keys"]` (NO `utf8_lossy`) | CONFIRMED (αB §1) |

**V3 ground-truth census: HEAD unchanged from V2 (`318d9c046`); ZERO source divergence.** Two
sharpenings strengthen the cohort (the phantom-`G` is double-gated test+proof scaffolding; the
prod NEON reach is exactly one cold path). **One stale-number defect surfaces** (checkasm `18` in
alphaD §1 V4) — the only REVISE in this report.

---

## §1 — alphaA (results extraction) — the overfit inventory

alphaA V3 carried ZERO V2 REVISE/REJECT and folds the three V2 CH1 non-blocking notes
(x86 LOC dual-figure 742 `.rs`/105 `.asm`/847 total; working-tree md5-collapse caveat rewrite;
"no V1 CONSOLIDATED" non-applicability). CH7 re-verifies the surfaces.

| Section | Addendum coverage | CH7 re-verify | Disposition |
|---|---|---|---|
| §0 headline 6-axis table | all six axes; x86 row now "24 files = 23 `.rs` 742 LOC + 1 `.asm` 105 LOC; 847 total" (`:55`) | `find -type f` = 24; 742/105/847 ✓ | **ACCEPT** |
| §1 JSON >sonic-strict per-corpus Δ (+1.4%…+164.7%) | >SOTA PRESERVE bar (not an addendum) | range cited to RESULTS; apache_builds +1.4% canary; unicode_escapes +164.7% widest ✓ | **ACCEPT** |
| §2 CSS 1.996–3.348× + §2.1 lazy-vs-eager | timed-plane-symmetry (H1) pin | N=200 W5-close medians ✓ | **ACCEPT** |
| §3.1 generator-does-not-exist | verbatim-blob (`:701`) / single-emitter-path (`:40/:110`) / distinct-grammar-output (md5 7→1); JSON templates enumerated | `:701`, `:40/:110`, md5 7→1 ✓ | **ACCEPT** — all live-cited |
| §3.2 contrivance/wrong-arch (x86 + OLD bench + metalang + gate holes) | timed-plane / corpus-in-timer / x86-census — **24-file dual-LOC framed** | `:66/:3091` warm; `parse_w11_1` ×7; gate roots `:2409/:2463` ✓ | **ACCEPT** (the V1 REVISE; fold verified) |
| §3.3 phantom + divergent value API | phantom-generic on the `G` axis; distinguishes phantom `<G>` from real `K=Kind` | `tape/mod.rs:175` two-axis; `K` real in `json/view.rs` ✓ | **ACCEPT** |
| §3.4 NEON wiring honesty (`:194-210`) | acceleration-wiring; **checkasm = 12 single-kernel + 2 = 14, NOT 18** (`:198-209`) | `ls checkasm_*.rs` = 14 ✓; alphaA states "18 … un-satisfiable … exact P4-class false-gate this cycle is fixing" | **ACCEPT** — alphaA is the CORRECT model: it names the false-`18` anti-pattern explicitly |
| §4 substrate / §5–§7 close-seeds | Lock 1 holds; binding inventory | tape singular ✓ | **ACCEPT** |

**alphaA overall: ACCEPT (all sections).** alphaA §3.4 is the artefact that AUTHORS the
correct checkasm framing ("12 single-kernel differentials + 2 harness/aggregate = 14; a gate
asserting 18 would be un-satisfiable on a clean tree — the exact P4-class false-gate this cycle is
fixing"). It is the standard against which the alphaD §4 defect is measured. The NEON-wiring
disclosure (cold count-commas wired, two `#[cfg(test)]`-only kernels dead, W3 commit-title
overstatement named) is the model anti-overclaim posture.

---

## §2 — alphaB (competitor deltas) — the bar to preserve

CH7's overfit lens on alphaB: (a) the CSS lazy-vs-eager asymmetry is disclosed up front
(timed-plane-symmetry); (b) no un-run comparator is fabricated as a number (corpus-in-timer /
contrivance). alphaB V3 carried ZERO V2 REVISE; it canonicalizes the third-grammar name to
`GoogleSheets` (V2 CH2 §8.1 cross-artefact alphabet fold) and changes no measurement.

| Section | Disposition |
|---|---|
| §0 standing + asymmetry pin (JSON near-symmetric strict; CSS asymmetric lazy-vs-eager) | **ACCEPT** — asymmetry stated up front is timed-plane-symmetry done right |
| §1 JSON strict-vs-strict bar (sonic strict Skipper `IgnoredAny`+`.end()`, no `utf8_lossy`; apache_builds +1.4% canary) | **ACCEPT** — strict comparator plane correct (`sonic_skipper.rs:3-7`, `Cargo.toml:23` re-verified) |
| §1.3 simdjson DOM = different output plane, NOT the strict bar | **ACCEPT** |
| §1.4 Track 2 typed caveat (conditional on hand-tuned per-corpus schema; NOT the unconditional bar) | **ACCEPT** |
| §2 CSS lazy-vs-eager (track1_rich vs lightningcss full-CSSOM; ~25-33% rich rider; keeper `css_canon_bench`; dual N=200/N=80 plane discipline) | **ACCEPT** — the load-bearing H1 disclosure |
| §3.3 NOT-runnable comparators honest `None` (yyjson/asmjson/RapidJSON; asmjson AVX-512 x86-only OUT) | **ACCEPT** — the strongest anti-contrivance posture in the cohort; pre-empts the fabricated-competitor failure mode |
| §3.4 H1 options (symmetric comparator OR rename+footnote; "silence does not preserve H1") | **ACCEPT** |
| §4 preservation bar (per-grammar must-hold + canary + risk; `GoogleSheets` canonical) | **ACCEPT** |
| §6 V2→V3 fold record | **ACCEPT** — accurately records the αB-as-ACCEPTed-by-all-seven + the only-name-canonicalized fold |

**alphaB overall: ACCEPT.** The §3.3 honest-`None` posture is the precise foreclosure of the
corpus-in-timer / fabricated-competitor contrivance the addenda guard. The `GoogleSheets`
canonicalization is sound (LOCKS.md:349 verified: the canonical alphabet is un-abbreviated, and
`Sheets\w*` would MISS a `GoogleSheets =>` arm). No measurement, ratio, or plane was altered.

---

## §3 — alphaC (REDRESS digest) — the PRUNE waves + pre-blocks

The most addendum-dense artefact: P1-P5 as PRUNE waves; the six pre-block families re-keyed to the
generator surfaces. alphaC V3 carried ZERO V2 REVISE and folds three V2 non-blocking notes
(x86 LOC dual-figure; the stale "no V1 CONSOLIDATED" drop; Note-2 αA-scoped).

| Section | Addendum | CH7 re-verify | Disposition |
|---|---|---|---|
| §0/§0.A/§0.B framing + state-delta (`emit_fact_stream` gone grep=0; `W5C…` retirement comment) | n/a | `grep -c emit_fact_stream` = 0; `lib.rs:298` retired comment ✓ | **ACCEPT** |
| §1-P1 delete x86 (847 LOC / 24 files / 14 `unimplemented!` / 0 intrinsics) | x86-deleted | 24/742/105/847/14 ✓ | **ACCEPT** — close gate `find … -type f = 0` deletes the `.asm` |
| §1-P2 delete OLD bench (`measure_mbps:3091` warm, 187-byte SHA fixtures) | corpus-in-timer / timed-plane | `:66/:1989/:3091` ✓ | **ACCEPT** — keep `css_canon_bench` |
| §1-P3 collapse 7 replicas (md5 single-hash; collapse-default, differentiate-only-if-distinct-`.bbnf`) | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** — binds distinct-output to PROVENANCE not cosmetics |
| §1-P4 fix gate holes + **the witness/`EventGrammar`-type-leak clause** (V2 CH5 C.4 fold) | gate-scope | `lock14_baseline.rs:2409/:2463` ✓; the "checked twice" emitter-token clause is the precise binding | **ACCEPT** — adds `EventGrammar`/`*EventGrammar` to the emitter forbidden-tokens (a re-emitted grammar-named type the arm census misses) |
| §1-P5 purge metalang (`parse_w11_1_number ×7`; gated by G1) | metalang | ×7 ✓ | **ACCEPT** |
| §2.1 AZ-IV eager → re-open on G1/G2/G4; §2.2 StructRegistry → split (Lock 2 `Layout`); §2.3 fact-stream → residual fork (`CSS_GENERATED_RS` + `RequestFacts`) | pre-blocks; **verbatim-blob + single-emitter-path** | `:701` + `:40/:110` ✓ | **ACCEPT** — the retirement-clause re-open test is the precise addendum binding |
| §2.4 24-broadcast / §2.5 FNV-fixture / §2.6 x86-AVX-SVE → PERMANENT | n/a / acceleration-wiring | per-block ✓ | **ACCEPT** |
| §3 single distinction + "checked TWICE" corollary (runtime output AND the emitter that produces it) | all | — | **ACCEPT** — the load-bearing pre-block insight (a refuted carrier can re-land at its SOURCE) |

**alphaC overall: ACCEPT (all sections).** CH7 re-verified the §2.3 residual fork (`:701` +
`:40/:110`), the §0.B state-delta (`emit_fact_stream`=0, `W5C…` retired comment), and the §3
"generator is the new carrier surface" corollary — all live. The §1-P4 witness/`EventGrammar`
clause is the sharpest V2-fold carry: it closes the seam where an un-forked generator that
INSTANTIATES `<G>` could emit a grammar-named `EventGrammar` type literal the generic-crate-scoped
gate cannot see. No REVISE/REJECT.

---

## §4 — alphaD (validated/invalidated ledger) — **the one REVISE**

alphaD's INVALIDATED table (I1-I10) attaches each addendum lens to a verified surface, and its
§8 V3-FOLD log folds the four V2 sharpenings (phantom-`G` test-only; fact-stream RETIRED;
`CssEventGrammar` absent; entry-HEAD re-anchor). CH7 re-greps every tagged surface AND audits the
VALIDATED (§1) carry-forward rows the FOLD log does not touch.

| Row | Addendum lens | CH7 re-verify | Disposition |
|---|---|---|---|
| §1 V1-V3 substrate/JSON/CSS validated | (PRESERVE bars) | tape singular; JSON 51/51; CSS N=200 ✓ | **ACCEPT** |
| **§1 V4 NEON checkasm discipline** | (V4 carry-forward) | **"18 differential harnesses … `tests/checkasm_*.rs` (18)" — FALSE.** Disk: `ls checkasm_*.rs` = **14** (12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs`) | **REVISE** — see fix below |
| §1 V5-V8 neutral kernel / honest harness / regen / FNV-quarantine | — | dispatch data-predicate; `css_canon_bench:250 assert(n>=50)`; FNV bench-only ✓ | **ACCEPT** |
| I1 CSS grammar-driven invalidated | verbatim-blob | `:701` ✓ | **ACCEPT** |
| I2 JSON projects from grammar | verbatim-blob | `json_sink_direct` ✓ | **ACCEPT** |
| I3 7 sub-grammars admitted | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** |
| I4 one codegen path | single-emitter-path | `:40` ✓ | **ACCEPT** |
| I5 `ValueRef<G>` parametric | phantom-generic | `:175 G=AnyGrammar`; sole `G` in `event_grammar_tests.rs:18/:20/:89` (test+proof gated) ✓ | **ACCEPT** — the F1 test-only precision is CORRECT and CH7-verified |
| I6 NEON CSS-scan acceleration | acceleration-wiring | `lib.rs:51 #[cfg(test)]`; 2-of-3 dead, count-commas cold ✓ | **ACCEPT** |
| I7 aarch64-only | x86-deleted | 742 `.rs` LOC / 24 files; "742 is `.rs`-only" framing matches CH1 Note-1 ✓ | **ACCEPT** |
| I8 Lock-14 gate meaningful | gate-scope | `:2409` exclusion ✓ | **ACCEPT** |
| I9 equal-work CSSOM | timed-plane / H1 | track1_4field vs rich ✓ | **ACCEPT** |
| I10 clean shipped symbols | metalang | `×7`; symbols `_direct/_object_direct/_array_direct` ✓ | **ACCEPT** |
| §3 DEMOTED DM1-DM4 (typed conditional; substrate-ready; 5 scalar passthroughs; UDOT orphan) | `_neon`-suffix-truth | `digit_mac.rs:27/:40` udot, 0 runtime callers ✓ | **ACCEPT** |
| §4 STILL-OPEN S1-S13; §5 pre-blocked; §6 self-verification log; §8 V3-FOLD log | all | maps 1:1; `CssEventGrammar` absent ✓; `DocumentView` sole impl `json/view.rs:68` ✓ | **ACCEPT** (but see §4-defect propagation note: S11 owner row references "current N=12 differentials" correctly — the defect is isolated to §1 V4) |

**§4 REVISE — concrete fix.** alphaD §1 V4 (`alphaD-validated-invalidated.md:85`) states the
checkasm discipline as "**18 differential harnesses**" with evidence "`tests/checkasm_*.rs`
(**18**)". CH7 disk-verified the tree is **14 `checkasm_*.rs` files = 12 single-kernel
differentials + `checkasm_common.rs` (signal trampoline / stack-canary) + `checkasm_parity.rs`
(aggregate)**. The `18` is the SAME stale overcount that CH4 raised as a V2 REVISE against
`SYNTHESIS.md:348` — and which the αF contract DID fold (SYNTHESIS `:43`,`:377`; HANDOFF `:15`
all now read "12 single-kernel + 2 = 14"), AND which alphaA §3.4 (`:198-209`), alphaC §2.6
(`:428-430`), and alphaE F4 (`:34/:79/:179/:213`) all carry corrected. **alphaD §1 V4 is the lone
un-folded straggler.** This is not cosmetic: alphaD §1 explicitly calls V4 "the gold standard …
SK-V18 G6 lands new kernels through THIS discipline" — i.e. it is a load-bearing reference for the
G6 same-wave-consumer rule. A downstream S-P3 gate keyed to alphaD's V4 row would assert "18
present," which is **un-satisfiable on a clean tree** — the EXACT P4-class false-gate inflation
alphaA §3.4 and alphaE F4 name as the anti-pattern this cycle exists to delete. **Fix:** amend
`:85` to "**12 single-kernel differential harnesses + `checkasm_common.rs` + `checkasm_parity.rs`
= 14 `checkasm_*.rs` total** (NOT 18 — the prior overcount would seed a P4-class un-satisfiable
gate)" and change the evidence cell `tests/checkasm_*.rs (18)` → `tests/checkasm_*.rs (14: 12+2)`,
so it agrees with alphaA/C/E and the αF contract, and any G6 checkasm gate asserts against the
satisfiable 12+k. This is a precision REVISE, not a finding reversal — the checkasm discipline
IS the gold standard; only the count is stale.

**alphaD overall: REVISE (§1 V4 only); all other sections ACCEPT.** The §8 FOLD log's four
sharpenings (phantom-`G` test-only, fact-stream RETIRED, `CssEventGrammar` absent, HEAD
re-anchor) are each CH7-verified and tighten the INVALIDATED claims correctly. The defect is
confined to the one VALIDATED carry-forward row the FOLD log did not propagate the V2 CH4 fold
into.

---

## §5 — alphaE (candidate shortlist) — the falsifiability triple

alphaE folds 13 backlog items into 5 clusters (A, B1-B4) under a falsifiability **triple**
(PRESERVED->SOTA / GRAMMAR-DERIVATION / DISTINCT-GRAMMAR-OUTPUT). alphaE V3 has the most complete
FOLD ledger in the cohort (V1→V2 F1-F8, V2→V3 F9-F12), each tagged at the exact gate line.

| Cluster | Addendum gate | CH7 re-verify | Disposition |
|---|---|---|---|
| §0 triple (mutate-`.bbnf`→output-changes / DISTINCT-OUTPUT / PRESERVED) | the three load-bearing gates | "a const courier cannot pass" is the exact operational falsifier for verbatim-blob | **ACCEPT** |
| A PRUNE (P1-P5, ≈−7200 LOC; P1 −847 incl `.asm` [F11]) | x86=0; replicas collapsed; gate meaningful; metalang purged | **checkasm "untouched … 12 single-kernel + 2 [F4: count corrected from 18 to 12+2]"** (`:79`) ✓ | **ACCEPT** — alphaE carries the CORRECTED count |
| B1 un-fork + project JSON (G3+G1; `grep RuntimeEmitterKind → 0` + canonical alphabet `:105`) | single-emitter-path / verbatim-blob | apache_builds +1.4% the hard canary ✓ | **ACCEPT** |
| B2 derive CSS (G2; `grep CSS_GENERATED_RS → 0`; N=200 per-row floors) | verbatim-blob (centrepiece) | LOW risk (scalar hot path) ✓ | **ACCEPT** |
| B3 shared trait + kill phantom (G4+H1; F6 DELETE-default + test-excluded grep; F7 rich-ast; F9 trait-grep test-exclusion) | phantom-generic / timed-plane | `CssEventGrammar` absent → INSTANTIATE = burden-of-proof ✓ | **ACCEPT** — instantiate-XOR-delete structurally verifiable |
| B4 PROVE Sheets + NEON (3 distinct `generated.rs`; **checkasm "12 single-kernel + 2 = 14 … prior '18' was an overcount … exact P4-class false gate this cycle fixes" [F4]** `:179`; acceleration-at-admission; F12 `dispatch.rs` owner-path) | distinct-grammar-output / acceleration-wiring | `ls checkasm_*.rs`=14 ✓; `dispatch.rs` is `src/dispatch.rs` only ✓ | **ACCEPT** — the sharpest litmus; carries the CORRECTED count |
| SUMMARY + cross-cutting 1-8 (sequencing; kept-honest artefacts incl "12 checkasm single-kernel differentials" `:213`; net ≈−9250 LOC) | all | — | **ACCEPT** — "deletes more than it adds" is the correct generalization-cycle shape |

**alphaE overall: ACCEPT.** alphaE is the artefact that does the checkasm count CORRECTLY in four
places (F4 at `:34`, `:79`, `:179`, `:213`), each explicitly naming "18" as "an overcount that
would make any '18 present' gate un-satisfiable on a clean tree — the exact P4-class false gate
this cycle fixes." This is the dispositive intra-cohort evidence that alphaD §1 V4's surviving
"18" is a genuine un-folded defect (§4), not a defensible alternate framing. The falsifiability
triple converts every addendum into a grep-able exit gate; cross-cutting note 2 (honest-finding
escape: a surviving hand-shaping becomes a NAMED grammar-parameterized primitive invoked from the
`.bbnf`, never a silent `_RS` blob) is abrogate-before-patch applied correctly. No REVISE/REJECT.
(For the record: the V2→V3 F10 canonical-alphabet + F12 `dispatch.rs` folds are non-addendum
precision items CH7 confirms landed but takes no CH7-lens disposition on.)

---

## §6 — SYNTHESIS.md (the αF contract) — the goalset (V3-folded)

This is the master αF output. CH7 confirms (a) the six addenda each carry a §0.1 close-gate + a
§0.4 pre-block + a §2 machine-checkable telemetry column, AND (b) the three V2 CONSOLIDATED
REVISEs landed.

### §6.1 — V2 fold verification (the three CONSOLIDATED REVISEs)

| # | V2 REVISE | Fold site | CH7 verdict |
|---|---|---|---|
| 1 | CH2 §8 neutrality-grep alphabet + scan-root widening | `SYNTHESIS:29-41` + `:201`,`:423-424` (`generator_grammar_branch_count` FULL alphabet `Json\|CssL4\|(GoogleSheets\|Sheets)\|Bbnf` over codegen AND xtask; NEW `generator_grammar_type_count` type census) | **FOLDED** ✓ — canonical `GoogleSheets` un-abbreviated (CH7 verified `Sheets\w*` would miss `GoogleSheets =>`; LOCKS.md:349 confirms) |
| 2 | **CH4 §6 stale checkasm "18"** | `SYNTHESIS:42-46` ("'18 differential harnesses' is corrected to the disk-true 12 single-kernel + 2 = 14 … an un-propagated αA fold that, left in the binding contract, would seed a P4-class un-satisfiable downstream gate") + `:377` | **FOLDED** ✓ — the contract is correct; **the SAME fold is MISSING in alphaD §1 V4** (§4 REVISE) |
| 3 | CH5 E.1 shared-trait grep test-exclusion | `SYNTHESIS:47-53` + `:426` (`shared_value_trait_instantiations` "≥2 real production … test-only does NOT count … `grep -v 'tests.rs\|#[cfg(test)]'` on the trait-impl axis") | **FOLDED** ✓ |

All three V2 REVISEs verified present and correctly sited. The SYNTHESIS even names REVISE #2 as
"an un-propagated αA fold that, left in the binding contract, would seed a P4-class un-satisfiable
downstream gate" — exactly the defect class CH7 V3 finds STILL un-propagated in alphaD §1 V4.

### §6.2 — Addendum triple-binding (close gate + pre-block + telemetry)

| Surface | Close gate (§0.1) | Telemetry column (§2) | Pre-block (§0.4) | Disposition |
|---|---|---|---|---|
| G2 verbatim-blob | `grep CSS_GENERATED_RS → 0`; grammar-projected | `verbatim_blob_present == false` (`:421`) | verbatim-blob re-entry (`:285`) | **ACCEPT** |
| G3 single-emitter | `RuntimeEmitterKind` gone + canonical arm census (`:201`) AND type census | `emitter_fork_present`; `generator_grammar_branch_count==0`; `generator_grammar_type_count==0` (`:422-424`) | fork resurrection / relocated-overfit-seam (`:296`) | **ACCEPT** — md5-necessary-not-sufficient + relocated-branch-in-xtask-metadata both closed |
| G4 phantom-generic | `G` instantiated w/ prod grammar OR removed; DELETE default; `_proof_compiles` excluded (`:202`,`:425`) | `phantom_generic_resolved` + `shared_value_trait_instantiations≥2` + `json_rich_navigation_preserved` (`:425-427`) | phantom re-entry (`:288`) | **ACCEPT** — the explicit `_proof_compiles` exclusion (CH7 verified that IS the only `G` witness, double-gated test+proof) is precise |
| G6 acceleration-wiring | reached at admission (grep hot path not tests); retire branch gated on a samply non-top-N row (`:250`) | `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` NOT `cfg-test-only` (`:432`) | acceleration claim (`:204`) | **ACCEPT** — measurement-gated retire forecloses the "mark everything retired with zero wiring" paper-close |
| PROVE distinct-grammar-output | Sheets `generated.rs` md5≠JSON≠CSS; `grep const.*_RS Sheets=0`; `sheets_grammar_shape==pratt-operator` (`:205`,`:431`) | `generated_md5_distinct` + `generator_grammar_count==3` + `sheets_grammar_shape` (`:428-431`) | distinct-output re-entry (`:291`) | **ACCEPT** |
| H1 timed-plane-symmetry | equal work, real corpus cold, no micro-fixtures; P2 deletes warm bench (`:206`) | `materialization_framing` + `corpus_in_timer == true` (`:436-437`) | corpus-out-of-timer / more-work (`:297`) | **ACCEPT** |
| P1/P4 x86-deleted + gate | `find …/x86_64 -type f = 0` (all 24); P4 scans leak surface (`:194`,`:197`) | `x86_tree_deleted` + `lock14_gate_scans_codegen` (`:433-434`) | x86/AVX/SVE (`:282`) | **ACCEPT** |

**SYNTHESIS.md overall: ACCEPT (all sections).** Every one of the six addenda is bound THREE
ways — a §0.1 close gate, a §0.4 pre-block re-entry forbiddance, and a §2 telemetry column the
`gate-json --skv18-generalization-report` consumer REJECTs on (`:457-466` enumerates the REJECT
set). All three V2 REVISEs landed and are correctly sited. The G3 dual census
(`generator_grammar_branch_count==0` arm census + `generator_grammar_type_count==0` type census,
both over codegen AND xtask metadata) is the most consequential V3 hardening for CH7's lens: it
closes the distinct-grammar-output false-pass where three md5-distinct files emerge from a single
grammar-branching emitter body OR a branch relocated into a neutral-identifier `RuntimeTarget`
data-table. No REVISE/REJECT.

---

## §7 — HANDOFF.md (the αF packet)

CH7 confirms the six addenda + the three V2 folds are carried verbatim into S-P0+ with re-entry
pre-blocks.

| Section | CH7 verdict |
|---|---|
| Status/fold block (`:7-18`) — names all three V2 REVISEs incl "the carry-forward '18 differential harnesses' corrected to the disk-true 12 single-kernel + 2 = 14" (`:15`) | **ACCEPT** — HANDOFF explicitly carries the `18→14` correction the alphaD §1 V4 row is missing |
| Current-state block (`:35-75`) — JSON +1.4%–164.7%; `ValueRef<G>` PHANTOM "test-only `_proof_compiles`" (`:61`) | **ACCEPT** — matches CH7 ground truth |
| What-SK-V18-Opens (P1-P5, G1-G6, PROVE, H1) | **ACCEPT** |
| Gate-Posture addenda block (`:166-177`) — six addenda verbatim with one-line bindings | **ACCEPT** |
| Pre-Blocked Routes (`:179-223`) — verbatim-blob/phantom/distinct-output re-entry; corpus-out-of-timer/more-work; no second substrate | **ACCEPT** |
| Inviolable-invariant 5 (`:231-251`) — the THREE-surface Lock-14 gate (token scan + canonical FULL-alphabet arm census over codegen AND xtask + grammar-named-type census; the `EventGrammar` emitter-token clause) | **ACCEPT** — the V3 hardening that makes the P4 gate trustworthy as the emitter is rebuilt |
| Next-Move + revert-graph + hard-caps (`:256-316`) — PRUNE→G1→G2→G3→G4→G5/G6→PROVE→H1; dispatch-hard-cap 20/15/30 | **ACCEPT** — the correct dependency order + halt ceiling |

**HANDOFF.md overall: ACCEPT.** Six addenda carried verbatim; all three V2 REVISEs present
(including the `18→14` checkasm correction at `:15`); each addendum has a pre-block re-entry
forbiddance; the Lock-14 three-surface gate (token scan AND arm census AND type census, over
codegen AND xtask metadata) is the V3 hardening that makes the P4 gate trustworthy. No REVISE/REJECT.

---

## §8 — Cross-artefact addendum coverage matrix (CH7 V3 summary)

| Addendum | Live surface (CH7-verified @ `318d9c046`) | Named in | Close gate | Pre-block | Telemetry |
|---|---|---|---|---|---|
| **verbatim-blob** | `runtime_generator.rs:701` const `&str`, consumed `:91` | A§3.1, C§2.3, D-I1, E-B2, SYN-G2, HO | `grep CSS_GENERATED_RS → 0` | §0.4 verbatim-blob re-entry | `verbatim_blob_present==false` |
| **distinct-grammar-output** | 7× md5 `b654562c…` | A§3.1, C-P3, D-I3, E-P3/B4, SYN-P3/PROVE, HO | md5-distinct census + `const.*_RS` Sheets=0 | §0.4 distinct-output re-entry | `generated_md5_distinct` + `generator_grammar_count==3` + `sheets_grammar_shape` |
| **single-emitter-path** | `grammar_provider.rs:40/:110` + `lib.rs:282/:291` + `runtime_generator.rs:17/:25` | A§3.1, C§2.3, D-I4, E-B1, SYN-G3, HO | `RuntimeEmitterKind → 0` AND arm census AND type census (codegen + xtask) | §0.4 fork resurrection / relocated-seam | `emitter_fork_present` + `generator_grammar_branch_count==0` + `generator_grammar_type_count==0` |
| **phantom-generic** | `tape/mod.rs:175 G=AnyGrammar`; only `G` witness in `event_grammar_tests.rs` (`#[cfg(test)]`+`#[cfg(feature=proof)]`) | A§3.3, D-I5, E-B3, SYN-G4, HO | ≥2 real OR `G` removed; `_proof_compiles` excluded; DELETE default | §0.4 phantom re-entry | `phantom_generic_resolved` + `shared_value_trait_instantiations≥2` + `json_rich_navigation_preserved` |
| **timed-plane + corpus-in-timer** | `nonjson_css_l4.rs:66/:1989/:3091` warm 187B SHA fixtures | A§3.2, B§3.2, C-P2/§2.4, D-I9, E-A/B2, SYN-H1/§0.6, HO | P2 delete + H1 frame | §0.4 corpus-out-of-timer / more-work | `materialization_framing` + `corpus_in_timer==true` |
| **acceleration-wiring** | `lib.rs:574/:598/:608` inside `mod tests` (`:51 #[cfg(test)]`); `count_top_level_commas` → `generated.rs:810` cold (2-of-3 dead) | A§3.4, C§2.6, D-I6/DM3, E-B4, SYN-G6, HO | grep ≥1 non-`cfg(test)` caller; retire gated on samply non-top-N | §0.4 acceleration claim | `acceleration_at_admission` (NOT `cfg-test-only`) |

Every addendum has all five columns populated against a CH7-verified surface. The **no-contrivance
/ x86-deleted / 7-replica-collapsed** trio is fully covered (P1 close `find … = 0` deletes all 24
files incl `.asm`; P2 deletes the warm bench; P3 md5-distinct census). The V3 hardenings —
`generator_grammar_type_count==0` (re-emitted grammar-named type the arm census misses), the
codegen+xtask dual scan root (relocated branch in a neutral-identifier data-table), the trait-impl
test-exclusion, and the double-gated phantom-`G` witness — close holes V2 left implicit.

**The one defect the matrix does NOT paper over:** the checkasm-harness count is `14` (12+2), not
`18`. Five artefacts + both αF documents carry it correctly; alphaD §1 V4 alone carries the stale
`18` — the §4 REVISE.

---

## §9 — Disposition summary

| # | Section | Disposition | Note / Fix |
|---|---|---|---|
| 1 | alphaA (all sections) | ACCEPT | V1 REVISE folded + verified; §3.4 is the model checkasm-count framing (names false-`18` anti-pattern) |
| 2 | alphaB (all sections) | ACCEPT | honest-`None` posture re-confirmed; `GoogleSheets` canonicalization sound |
| 3 | alphaC (all sections) | ACCEPT | §2.3 residual fork + §3 corollary + §1-P4 `EventGrammar` clause re-verified live |
| 4 | **alphaD §1 V4** | **REVISE** | `:85` checkasm "18 differential harnesses" / `tests/checkasm_*.rs (18)` → "12 single-kernel + 2 = 14 (NOT 18; an 18-present gate is un-satisfiable on a clean tree — the P4-class false-gate this cycle fixes)", matching alphaA §3.4 / alphaC §2.6 / alphaE F4 / SYNTHESIS / HANDOFF |
| 5 | alphaD (all other sections: §1 V1-V3/V5-V8, §2 I1-I10, §3, §4, §5, §6, §8) | ACCEPT | I5 phantom-`G` test-only precision CH7-verified (double-gated witness); §8 FOLD log sharpenings all landed |
| 6 | alphaE (all clusters) | ACCEPT | falsifiability triple intact; carries the CORRECTED checkasm count in 4 places (F4) — the dispositive evidence the alphaD `18` is a defect |
| 7 | SYNTHESIS.md (all sections) | ACCEPT | three V2 REVISEs verified + triple-binding intact; G3 dual census is the load-bearing V3 hardening |
| 8 | HANDOFF.md (all sections) | ACCEPT | six addenda + three V2 folds carried verbatim (incl `18→14` at `:15`); three-surface Lock-14 gate |

**CH7 V3 verdict.** The six new addenda fire HONESTLY across the cohort — each named against a
CH7-independently-re-verified live surface at HEAD `318d9c046` (`:701`; `:40/:110` + `:282/:291` +
`:17/:25`; md5 7→1; `:175 G=AnyGrammar` with the sole witness confined to the double-gated
`event_grammar_tests.rs`; `lib.rs:51 #[cfg(test)]` with 2-of-3 CSS NEON kernels dead and
count-commas cold; warm 187B SHA fixtures), each carrying a grep-able close gate, a §0.4 pre-block,
and a §2 machine-checkable telemetry column. The no-contrivance / x86-deleted / 7-replica-collapsed
trio is fully covered. The αF contract (SYNTHESIS + HANDOFF) folds all three V2 CONSOLIDATED
REVISEs correctly, INCLUDING the CH4 checkasm `18→14` correction.

**The lens did not rubber-stamp a two-pass clean history.** CH7 V3 found the one place the V2 CH4
`18→14` fold was NOT propagated — **alphaD §1 V4** still asserts "18 differential harnesses /
`tests/checkasm_*.rs (18)`," while alphaA §3.4, alphaC §2.6, alphaE F4 (×4), SYNTHESIS, and HANDOFF
all carry the disk-true `12+2=14` AND explicitly name "18" as the un-satisfiable P4-class
false-gate the cycle exists to delete. Because alphaD §1 V4 is a load-bearing reference for the G6
checkasm same-wave-consumer rule, the stale count could seed an un-satisfiable downstream gate —
exactly the overfit-prune failure mode this lens guards. That is the one **REVISE** (concrete fix
in §4 + §9). No REJECT: nothing in the cohort overclaims a prune, mis-attributes an addendum, or
admits a contrivance as the bar.

8 sections dispositioned: 7 ACCEPT, 1 REVISE, 0 REJECT.

TALLY accept=7 revise=1 reject=0
