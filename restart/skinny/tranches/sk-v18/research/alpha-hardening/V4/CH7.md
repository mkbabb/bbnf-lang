# CH7 — OVERFIT-PRUNE (V4)

Lens: CH7 OVERFIT-PRUNE. Pass: PASS-ALPHA SK-V17→SK-V18 cycle **V4** (the GENERALIZATION
cycle / inflection backtrack). Per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z. Reviewer focus: the
SIX NEW CHALLENGE addenda fire HONESTLY across the alpha artefacts — **verbatim-blob**
(const-`&str` `@generated` = hand-written), **distinct-grammar-output** (N grammars = N
non-identical `generated.rs`), **single-emitter-path** (flag/enum forks), **phantom-generic**
(uninstantiated `<G>`), **timed-plane-symmetry + corpus-in-timer**, **acceleration-wiring**
(NEON at admission, not `#[cfg(test)]`) — plus no-contrivance, x86 DELETED, 7-replica COLLAPSED.

Subject reviewed: `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`. Per
PASS-ALPHA §2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md` (no separate `alphaF-*.md`).

**V4 context + posture.** This CHALLENGE wave's history: V1 CH7 = 7A/1R (the lone REVISE: alphaA
x86 census 23→24, folded). V2 CH7 = 7A/0R. V3 CH7 = 7A/1R (the lone REVISE: alphaD §1 V4 stale
checkasm "18", folded). **A lens with a three-pass clean-modulo-one-straggler history must NOT
rubber-stamp.** This V4 report re-verifies EVERY addendum surface live at HEAD `318d9c046`
(unchanged since V3), confirms the V3 self-folds landed, AND adversarially hunts the cohort for the
ONE artefact that lags a cohort-wide fold — the recurrent failure pattern this wave keeps surfacing
(V1: alphaA x86 count; V3: alphaD checkasm count). **CH7 V4 surfaces exactly that pattern again on
a NEW, more consequential axis**: the **CH5 V3 §C.5/§F.7 BLOCKING second-x86-surface fold**
(`ext/x86/` ~3.5K-LOC vendored ASM + nasm `build.rs` + `lib.rs:247`) was propagated into alphaC,
alphaD, alphaE, SYNTHESIS, and HANDOFF — but **alphaA, the x86-census ORIGIN artefact, was left
scoping x86 to `src/x86_64/` only**, with a `src/`-scoped close-gate. That is a REVISE on alphaA,
concrete fix in §1.

---

## §0 — Independent verification log (CH7 re-grep, HEAD `318d9c046`)

Every load-bearing overfit/prune claim re-verified before disposition. The addenda are NOT accepted
on the artefacts' word — each is confirmed against the tree this V4 pass.

| Claim under the addenda | CH7 V4 command | Result | Verdict |
|---|---|---|---|
| **verbatim-blob** (CSS const-`&str`) | `grep -n 'const CSS_GENERATED_RS' codegen/src/runtime_generator.rs` | `runtime_generator.rs:701: const CSS_GENERATED_RS: &str = r#"`; consumed verbatim `:91` | CONFIRMED — path is `codegen/src/runtime_generator.rs` (NOT `codegen/src/codegen/…`; the V3 report's own verify-log had a double-`codegen/` typo, but the ARTEFACTS cite the correct path) |
| **single-emitter-path** (fork) | `grep -n 'enum RuntimeEmitterKind\|RuntimeEmitterKind::' codegen/src` | `grammar_provider.rs:40 pub enum RuntimeEmitterKind`; `:110 != …RequestFacts`; `lib.rs:282 CompiledLowering`/`:291 RequestFacts`; `runtime_generator.rs:17/:25` match-arms | CONFIRMED (fork live at admission) |
| **distinct-grammar-output** (7 replicas) | `md5 -q runtime/.../css_l4_*/generated.rs \| sort \| uniq -c` | `7  b654562ccff46ed62dd48e9ace325830` (7→1 byte-identical) | CONFIRMED |
| **phantom-generic** (`<G>` default; no prod instantiation) | `tape/mod.rs:175`; census non-default `G` | `:175 K = AnyKind, G: EventGrammar = AnyGrammar`; the ONLY non-default `G` uses are `event_grammar_tests.rs:18/:20/:89` — module included `#[cfg(test)]` at `tape/mod.rs:3`, AND the file's contents are per-item `#[cfg(feature="proof")]`-gated (double-gated). The `JsonEventGrammar`/`SheetsEventGrammar` witness STRUCTS (`grammars/json/event_grammar_witness.rs:4`, `grammars/sheets_witness/event_grammar_witness.rs:4`) compile in production (`lib.rs:34/:38` `pub mod`, ungated) but are NEVER used as a non-default `G` outside the double-gated test | CONFIRMED — phantom `G` is test-only as a non-default instantiation; the artefacts' "only `_proof_compiles`" framing is precise |
| **acceleration-wiring** (CSS NEON dead at admission) | `runtime/src/lib.rs` `#[cfg(test)]` boundary; prod reach of `count_top_level_commas` | `lib.rs:51 #[cfg(test)] mod tests`; parity-guard header `:498-504` AND the three NEON callers (`find_css_significant :574`, `find_comment_close :598/:608`) inside it; the **only** prod reach is `css_l4_*/generated.rs:157 → count_top_level_commas (gen:809) → runtime_simd::count_top_level_commas (:810)` — the COLD rich-summary | CONFIRMED dead-at-admission (2-of-3 CSS NEON consumers; 3rd cold) |
| **corpus-in-timer / timed-plane** (OLD warm bench) | `grep nonjson_css_l4.rs` | `:66 EXPECTED_FIXTURE_BYTES = 187`; `:1989 input.len() != EXPECTED_FIXTURE_BYTES`; `:3091 fn measure_mbps` | CONFIRMED live |
| **x86 census — `src/x86_64/`** | `find …/x86_64 -type f`; `.rs`/`.asm` LOC; `unimplemented!` | **24 files = 23 `.rs` (742 LOC) + 1 `.asm` (`byte_class_from_eq_set_64.asm`, 105 LOC) = 847 total**, **14** `unimplemented!` | CONFIRMED |
| **x86 census — SECOND surface `ext/x86/` (the V4 defect axis)** | `find …/ext/x86 -type f \| xargs wc -l`; `wc -l build.rs`; `grep -n 'nasm\|x86' build.rs`; `sed -n 247 src/lib.rs` | `ext/x86/{bbnf.asm,x86util.asm,x86inc.asm}` = **3499 LOC** `.asm` (3554 incl `LICENSE-VENDOR`); `build.rs` = **102 LOC** nasm-rs x86-assembler driver (`:1` "assembles vendored + authored x86_64 .asm sources"); `src/lib.rs:247` "Contract documented in ext/x86/bbnf.asm"; `Cargo.toml` `build="build.rs"` + `nasm-rs="0.3"` build-dep | CONFIRMED — a SECOND x86 surface exists; the OLD `src/x86_64/`-only gate FALSE-PASSES over it |
| **checkasm harness count** | `ls bbnf-simd/tests/checkasm_*.rs \| wc -l` | **14** = 12 single-kernel differentials + `checkasm_common.rs` (trampoline) + `checkasm_parity.rs` (aggregate). NOT 18. | CONFIRMED 14 — the V3 alphaD "18" defect is FOLDED (alphaD §1 V4 now reads "14 … NOT 18") |
| metalang leak | `grep -c parse_w11_1 json/generated.rs`; symbols | `7`; `parse_w11_1_number_{direct,object_direct,array_direct}` | CONFIRMED |
| Lock-14 gate exclusion holes | `grep -n 'GENERIC_SCAN_ROOTS\|diagnostic-x86' bbnf-bench/src/lock14_baseline.rs` | `:2409 GENERIC_SCAN_ROOTS` lists `codegen/src/lib.rs`/`runtime/src/lib.rs` but `runtime_generator.rs`/`json_sink_direct.rs`/`json_templates` route into the weaker `:2435-39` allowlist; x86 tagged `("crates/bbnf-simd/src/x86_64","diagnostic-x86")` `:2463`; `accepts_current_allowlist :2729` PASSES | CONFIRMED — green-gate-over-leaks accurate |
| DocumentView sole impl | `grep -rn 'impl.*DocumentView.*for' runtime/src` | SOLE: `json/view.rs:68 impl DocumentView for JsonDocument` (CSS none) | CONFIRMED |
| sonic strict plane | `sonic_skipper.rs`; `Cargo.toml` | `IgnoredAny::deserialize` + `.end()`; `default-features=false` (NO `utf8_lossy`) | CONFIRMED |

**V4 ground-truth census: HEAD unchanged (`318d9c046`); ZERO source divergence from V3.** The V3
folds landed in the artefacts (checkasm 18→14 in alphaD; the BLOCKING second-x86-surface crate-wide
in alphaC/D/E/SYN/HO). **One stale-scope defect survives** — alphaA's x86 census + close-gate did
NOT absorb the second-x86-surface fold the rest of the cohort took. That is the only REVISE.

---

## §1 — alphaA (results extraction) — **the one REVISE**

alphaA is the **x86-census ORIGIN artefact** — the inventory other artefacts cite. Its V4 FOLD log
(`:13-31`) asserts alphaA "carried ZERO V3 REVISE/REJECT across all seven CHALLENGE lenses" and
re-verifies "x86 = 24 files." CH7 re-greps every alphaA surface AND audits whether the cohort-wide
V3 folds reached it.

| Section | Addendum coverage | CH7 V4 re-verify | Disposition |
|---|---|---|---|
| §0 headline 6-axis table (`:67` x86 row) | x86-deleted | "24 files (23 `.rs` 742 + 1 `.asm` 105; 847 total), 14 `unimplemented!`" ✓ for `src/x86_64/`; **omits `ext/x86/`** | **REVISE** (scope) |
| §1 JSON >sonic-strict per-corpus Δ (+1.4%…+164.7%) | >SOTA PRESERVE bar | apache_builds +1.4% canary; unicode_escapes +164.7% widest ✓ | **ACCEPT** |
| §2 CSS 1.996–3.348× + §2.1 lazy-vs-eager | timed-plane-symmetry (H1) pin | N=200 W5-close medians ✓ | **ACCEPT** |
| §3.1 generator-does-not-exist (`:169` `:170`) | verbatim-blob (`:701`) / single-emitter-path (`:40/:110`) / distinct-output (md5 7→1) | `:701`, `:40/:110`, md5 7→1 ✓ — paths correct (`codegen/src/runtime_generator.rs`) | **ACCEPT** |
| §3.2 contrivance/wrong-arch table (`:178` x86 row; `:179` OLD bench; `:180` metalang; `:181` gate holes) | timed-plane / corpus-in-timer / x86-census | `:66/:3091` warm ✓; `parse_w11_1` ×7 ✓; gate roots `:2409`/`diagnostic-x86 :2463` ✓; **x86 row scopes `src/x86_64/` ONLY** | **REVISE** (the x86 row) |
| §3.3 phantom + divergent value API | phantom-generic on the `G` axis | `tape/mod.rs:175` two-axis; `K` real in `json/view.rs` ✓ | **ACCEPT** |
| §3.4 NEON wiring honesty + checkasm `= 12 single-kernel … not 18` (`:217`,`:275`) | acceleration-wiring | `ls checkasm_*.rs` = 14; alphaA names the false-`18` anti-pattern correctly ✓ | **ACCEPT** — alphaA remains the MODEL checkasm-count framing |
| §6 close-summary (`:35`,`:266-267` PRUNE close) | x86-deleted close gate | "P1 x86 tree gone … `find …/x86_64 -type f` = 0" — **`src/`-scoped close-gate** | **REVISE** (close-gate scope) |
| §4 substrate / §5/§7 close-seeds | Lock 1 holds | tape singular ✓ | **ACCEPT** |

**§1 REVISE — concrete fix.** Three alphaA surfaces scope the x86 PRUNE to `src/x86_64/` ONLY and
carry a `src/`-scoped close-gate, omitting the SECOND x86 surface the V3 CH5 §C.5/§F.7 BLOCKING
REVISE named and the rest of the cohort folded:

1. **`alphaA-results-extraction.md:178`** (§3.2 x86 row) — `skinny/crates/bbnf-simd/src/x86_64/ — 24 files … P1 DELETE — close gate `find …/x86_64 -type f` = 0 deletes all 24 files`.
2. **`alphaA-results-extraction.md:67`** (§0 census table x86 row) — `24 files … OVERFIT (P1 delete; gate = file-count = 0)`.
3. **`alphaA-results-extraction.md:35`, `:266-267`** (§6 close-summary) — `P1 x86 tree gone (all 24 files …; find …/x86_64 -type f = 0)`.
4. **`alphaA-results-extraction.md:13-31`** (V4 FOLD log) — asserts "ZERO V3 REVISE/REJECT across all seven CHALLENGE lenses" and re-verifies "x86 = 24 files"; the V4 FOLD log does NOT record the CH5 V3 §C.5/§F.7 BLOCKING REVISE.

CH7 V4 disk-verified (HEAD `318d9c046`) the surviving second surface:
`skinny/crates/bbnf-simd/ext/x86/` = `bbnf.asm`/`x86util.asm`/`x86inc.asm` = **3499 LOC** vendored
x264/FFmpeg `cglobal`/AVX-512 ZMM-macro ASM (3554 incl `LICENSE-VENDOR`); `bbnf-simd/build.rs` =
**102-LOC nasm-rs x86-assembler driver** (`:1` "assembles vendored + authored x86_64 .asm
sources"); `src/lib.rs:247` "Contract documented in ext/x86/bbnf.asm"; `Cargo.toml`
`build="build.rs"` + `nasm-rs="0.3"` build-dep. **alphaA is the LONE cohort artefact that scopes
x86 to `src/x86_64/` only** — alphaC §6 FOLD-1, alphaD V4-fold R-row, alphaE P1, SYNTHESIS
(`:58-75`,`:162-169`,`:246`,`:491`), and HANDOFF (`:12-17`,`:71-72`,`:98-102`,`:208-210`,`:242-244`,
`:288-289`,`:315-316`) ALL carry the crate-wide scope. This is not cosmetic: alphaA is the
results-extraction INVENTORY a downstream S-P3 wave reads to enumerate the P1 deletion set and the
P1 close-gate. A P1 keyed to alphaA's `find …/src/x86_64 -type f = 0` would **PASS GREEN while
~3.5K LOC of x86 ASM + an x86-assembler build driver survive** — the EXACT "x86 gone is literally
false" false-pass the CH5 V3 REVISE flagged BLOCKING, and the EXACT overfit-prune failure mode
this lens guards (a green gate scoped to miss the leak surface). It is the same straggler pattern
this wave found at V1 (alphaA x86 count) and V3 (alphaD checkasm count): one artefact lagging a
cohort fold.

**Fix:** amend the three alphaA x86 surfaces (`:178`, `:67`, `:35/:266-267`) to name BOTH surfaces
— `src/x86_64/` (24 files / 847 LOC) **AND** `ext/x86/` (3499 LOC vendored ASM) **AND**
`build.rs` (102-LOC nasm-rs driver, delete-or-neutralize) **AND** the `src/lib.rs:247` reference —
and widen the close-gate to crate-wide, matching SYNTHESIS `:491`: `find
…/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` returns only aarch64-neutral comments. And add an R-row to the alphaA V4
FOLD log recording the CH5 V3 §C.5/§F.7 BLOCKING second-x86-surface fold (so the FOLD log no longer
asserts the inaccurate "ZERO V3 REVISE/REJECT across all seven lenses" — CH5 V3 carried a BLOCKING
REVISE that touches alphaA's x86 census). This is a precision/scope REVISE, not a finding reversal:
the x86 tree IS overfit and IS P1's target; only alphaA's enumeration of WHAT x86 surface exists,
and its close-gate, are stale.

**alphaA overall: REVISE (§3.2 x86 row + §0 census x86 row + §6 close-gate + V4 FOLD log);
all other sections ACCEPT.** alphaA's §3.4 remains the model checkasm framing (it AUTHORS the
"12 single-kernel … not 18" correction). The defect is confined to the x86-surface enumeration the
V3 CH5 BLOCKING fold did not reach.

---

## §2 — alphaB (competitor deltas) — the bar to preserve

CH7's overfit lens on alphaB: (a) CSS lazy-vs-eager asymmetry disclosed up front
(timed-plane-symmetry); (b) no un-run comparator fabricated as a number (corpus-in-timer /
contrivance). alphaB V4 carries its V3-ACCEPTed bar verbatim (a PRESERVE bar, unchanged by
definition), re-verifies every number at HEAD, and records the cross-cohort x86-scope awareness in
its §6/§0 fold block.

| Section | Disposition |
|---|---|
| §0 standing + asymmetry pin (JSON near-symmetric strict; CSS asymmetric lazy-vs-eager) | **ACCEPT** — asymmetry stated up front is timed-plane-symmetry done right |
| §1 JSON strict-vs-strict bar (sonic Skipper `IgnoredAny`+`.end()`, no `utf8_lossy`; apache_builds +1.4% canary) | **ACCEPT** — strict comparator plane re-verified (`sonic_skipper.rs`, `Cargo.toml`) |
| §1.3 simdjson DOM = different output plane, NOT the strict bar | **ACCEPT** |
| §1.4 Track 2 typed caveat (conditional on hand-tuned per-corpus schema) | **ACCEPT** |
| §2 CSS lazy-vs-eager (track1_rich vs lightningcss full-CSSOM; keeper `css_canon_bench`; dual N=200/N=80) | **ACCEPT** — the load-bearing H1 disclosure |
| §3.3 NOT-runnable comparators honest `None` (yyjson/asmjson/RapidJSON; asmjson AVX-512 x86-only OUT) | **ACCEPT** — the strongest anti-contrivance posture in the cohort; pre-empts the fabricated-competitor failure mode. The "asmjson AVX-512 OUT" line is the comparator FACE of the aarch64-only mandate and makes NO "x86 gone" close-claim (so it does NOT inherit the alphaA scope defect) |
| §3.4 H1 options (symmetric comparator OR rename+footnote) | **ACCEPT** |
| §4 preservation bar (per-grammar must-hold + canary; `GoogleSheets` canonical) | **ACCEPT** |
| §6 V3→V4 fold record (records the αD §1 V4 18→14 + the CH5 second-x86-surface awareness, neither orphan-touching an αB section) | **ACCEPT** — `alphaB:13`,`:368` correctly note these are NON-αB REVISEs |

**alphaB overall: ACCEPT.** alphaB's §3.3 honest-`None` posture is the precise foreclosure of the
corpus-in-timer / fabricated-competitor contrivance. Its §6 fold record correctly identifies that
the two V3 REVISEs (αD checkasm 18→14; CH5 second-x86-surface) are NOT αB sections, and changes no
measurement, ratio, or plane. No REVISE/REJECT.

---

## §3 — alphaC (REDRESS digest) — the PRUNE waves + pre-blocks

The most addendum-dense artefact: P1-P5 as PRUNE waves; the six pre-block families re-keyed to the
generator surfaces. alphaC V4's §6 FOLD-1 is the CANONICAL fold of the CH5 V3 BLOCKING
second-x86-surface REVISE — and it EXTENDS it (finds `Cargo.toml` nasm-rs dep + scalar/checkasm
contract refs the V3 CH5 sweep did not enumerate).

| Section | Addendum | CH7 V4 re-verify | Disposition |
|---|---|---|---|
| §0/§0.A/§0.B framing + state-delta (`emit_fact_stream` gone; `W5C…` retirement comment) | n/a | `grep -c emit_fact_stream` = 0 ✓ | **ACCEPT** |
| §1-P1 delete x86 — **crate-wide** (`src/x86_64/` 847 LOC / 24 files AND `ext/x86/` 3499–3554 LOC AND nasm `build.rs` AND `lib.rs:247`) | x86-deleted | `find …/ext/x86 -type f \| xargs wc -l` = 3554 (3499 `.asm`); `build.rs` 102 LOC ✓ | **ACCEPT** — the close gate is crate-wide, NOT `src/`-scoped |
| §1-P2 delete OLD bench (`measure_mbps:3091` warm, 187-byte SHA fixtures) | corpus-in-timer / timed-plane | `:66/:1989/:3091` ✓ | **ACCEPT** |
| §1-P3 collapse 7 replicas (md5 single-hash; collapse-default, differentiate-only-if-distinct-`.bbnf`) | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** |
| §1-P4 fix gate holes + `EventGrammar`-type-leak clause | gate-scope | `:2409`/`:2463`/`diagnostic-x86` ✓ | **ACCEPT** |
| §1-P5 purge metalang (`parse_w11_1_number ×7`) | metalang | ×7 ✓ | **ACCEPT** |
| §2.1-§2.6 pre-blocks (AZ-IV / StructRegistry / fact-stream residual fork / 24-broadcast / FNV / x86-AVX-SVE) | pre-blocks; verbatim-blob + single-emitter-path | `:701` + `:40/:110` ✓ | **ACCEPT** |
| §3 "checked TWICE" corollary (runtime output AND the emitter) | all | — | **ACCEPT** |
| §6 FOLD-1 (CH5 second-x86-surface, crate-wide, with V4 extension) | x86-deleted | `ext/x86/` 3554 LOC; `build.rs` 102; `Cargo.toml` nasm-rs dep; `lib.rs:247` ✓ | **ACCEPT** — the canonical fold of the BLOCKING REVISE; alphaA must match THIS |

**alphaC overall: ACCEPT (all sections).** alphaC §6 FOLD-1 is the gold-standard treatment of the
CH5 V3 BLOCKING second-x86-surface REVISE — it not only folds it crate-wide but EXTENDS it (the
`Cargo.toml` `nasm-rs` build-dep and the scalar/checkasm `ext/x86` contract references). This is
abrogate-before-patch applied to the prune scope itself. alphaA §1 must be brought into concordance
with alphaC §6 FOLD-1 (the §1 REVISE). No REVISE/REJECT on alphaC.

---

## §4 — alphaD (validated/invalidated ledger) — the V3 REVISE FOLDED

alphaD §1 V4 (the V3 REVISE target) now reads **"14 checkasm files = 12 single-kernel differentials
+ `checkasm_common.rs` (trampoline) + `checkasm_parity.rs` (aggregate) (NOT 18 — an '18-present'
gate is un-satisfiable on a clean tree …)"** (`:105`), with the §8 V4-fold R1 row (`:231`,`:276`)
recording the 18→14 correction and naming the cohort concordance. CH7 V4 disk-confirms `ls
checkasm_*.rs | wc -l` = 14. **The V3 REVISE is FOLDED.**

| Row | Addendum lens | CH7 V4 re-verify | Disposition |
|---|---|---|---|
| §1 V1-V3 substrate/JSON/CSS validated | PRESERVE bars | tape singular; JSON; CSS N=200 ✓ | **ACCEPT** |
| §1 V4 NEON checkasm discipline (now "14 … NOT 18") | acceleration-wiring | `ls checkasm_*.rs` = 14 ✓ — **the V3 REVISE is folded** | **ACCEPT** |
| §1 V5-V8 neutral kernel / honest harness / regen / FNV-quarantine | — | dispatch data-predicate; `css_canon_bench:250`; FNV bench-only ✓ | **ACCEPT** |
| I1 CSS grammar-driven invalidated | verbatim-blob | `:701` ✓ | **ACCEPT** |
| I2 JSON projects from grammar | verbatim-blob | `json_sink_direct` ✓ | **ACCEPT** |
| I3 7 sub-grammars admitted | distinct-grammar-output | md5 7→1 ✓ | **ACCEPT** |
| I4 one codegen path | single-emitter-path | `:40` ✓ | **ACCEPT** |
| I5 `ValueRef<G>` parametric | phantom-generic | `:175 G=AnyGrammar`; sole `G` in `event_grammar_tests.rs:18/:20/:89` (test+proof gated) ✓ | **ACCEPT** — the test-only precision is CORRECT |
| I6 NEON CSS-scan acceleration | acceleration-wiring | `lib.rs:51 #[cfg(test)]`; 2-of-3 dead, count-commas cold ✓ | **ACCEPT** |
| I7 aarch64-only — **does it scope `ext/x86/`?** | x86-deleted | I7 (`:126`) scopes "742 LOC `.rs`-only … 24 files" — `src/x86_64/`. **BUT** alphaD's §8 V4-fold and SYNTHESIS-reference carry the crate-wide scope, AND alphaD I7 disposition routes to "P1 — DELETE the entire `src/x86_64/` tree …" + cross-refs CH5 V3. alphaD is the LEDGER (invalidated-claim row), not the inventory; its disposition cross-refs the crate-wide P1. The inventory-of-record is alphaA (the REVISE target). alphaD I7's `src/x86_64/` framing is a claim-row, not a close-gate definition | **ACCEPT** (claim-row; not the binding close-gate; CH7 takes the inventory REVISE on alphaA where the close-gate is authored) |
| I8 Lock-14 gate meaningful | gate-scope | `:2409` exclusion ✓ | **ACCEPT** |
| I9 equal-work CSSOM | timed-plane / H1 | track1_4field vs rich ✓ | **ACCEPT** |
| I10 clean shipped symbols | metalang | ×7 ✓ | **ACCEPT** |
| §3 DEMOTED DM1-DM4 (typed conditional; substrate-ready; 5 scalar passthroughs; UDOT orphan) | `_neon`-suffix-truth | `digit_mac.rs` udot, 0 runtime callers ✓ | **ACCEPT** |
| §4 STILL-OPEN S1-S13; §5 pre-blocked; §6 self-verify; §8 V4-FOLD R1 (18→14) | all | maps 1:1; `CssEventGrammar` absent; DocumentView sole impl ✓ | **ACCEPT** — §8 R1 folds the V3 checkasm REVISE |

**alphaD overall: ACCEPT (all sections).** The V3 lone REVISE (§1 V4 stale "18") is folded — §1 V4
now reads the disk-true 14 and §8 R1 records the correction with cohort-concordance citations.
alphaD I7 is a claim-ROW that disposes to the crate-wide P1 (cross-refs CH5 V3), not a close-gate
definition; the binding x86 close-gate is authored in alphaA (the §1 REVISE) and SYNTHESIS (correct,
crate-wide). No REVISE/REJECT on alphaD.

---

## §5 — alphaE (candidate shortlist) — the falsifiability triple

alphaE folds 13 backlog items into 5 clusters under a falsifiability triple (PRESERVED->SOTA /
GRAMMAR-DERIVATION / DISTINCT-GRAMMAR-OUTPUT). alphaE V4 carries the CORRECTED checkasm count in four
places (F4) AND the CH5 second-x86-surface fold in P1.

| Cluster | Addendum gate | CH7 V4 re-verify | Disposition |
|---|---|---|---|
| §0 triple (mutate-`.bbnf`→output-changes / DISTINCT-OUTPUT / PRESERVED) | the three load-bearing gates | "a const courier cannot pass" is the operational falsifier for verbatim-blob | **ACCEPT** |
| A PRUNE (P1-P5; **P1 owner-paths name `ext/x86/` + `build.rs` crate-wide?**) | x86=0; replicas; gate; metalang | P1 carries crate-wide x86 scope (the CH5 fold) ✓; checkasm "12 single-kernel + 2 [F4: corrected from 18]" (`:79`,`:90`) ✓ | **ACCEPT** — carries the CORRECTED count AND the crate-wide P1 |
| B1 un-fork + project JSON (G3+G1; `grep RuntimeEmitterKind → 0` + canonical alphabet) | single-emitter-path / verbatim-blob | apache_builds +1.4% canary ✓ | **ACCEPT** |
| B2 derive CSS (G2; `grep CSS_GENERATED_RS → 0`; N=200 per-row floors) | verbatim-blob (centrepiece) | LOW risk (scalar hot path) ✓ | **ACCEPT** |
| B3 shared trait + kill phantom (G4+H1; DELETE-default + test-excluded grep; rich-ast; trait-grep test-exclusion) | phantom-generic / timed-plane | `CssEventGrammar` absent → INSTANTIATE = burden-of-proof ✓ | **ACCEPT** |
| B4 PROVE Sheets + NEON (3 distinct `generated.rs`; checkasm "12 single-kernel + 2 = 14 … prior '18' an overcount" `:179`,`:190`; acceleration-at-admission) | distinct-output / acceleration-wiring | `ls checkasm_*.rs`=14 ✓ | **ACCEPT** — carries the CORRECTED count |
| SUMMARY + cross-cutting 1-9 (sequencing; F13/F14 folds; net ≈−9250 LOC) | all | F14 = no-op confirm αE does NOT inherit the αD "18"; F13 = relocated-seam re-attributed to structural row-count check ✓ | **ACCEPT** |

**alphaE overall: ACCEPT.** alphaE does the checkasm count CORRECTLY in four places (F4) and carries
the crate-wide P1 x86 scope (the CH5 fold). Its cross-cutting note 9 records the V3→V4 convergence
accurately, including that F14 (the αD-only "18") is a no-op for αE. No candidate was added/removed
(still A, B1-B4). No REVISE/REJECT.

---

## §6 — SYNTHESIS.md (the αF contract) — the goalset (V4-folded)

This is the master αF output. CH7 V4 confirms (a) the six addenda each carry a §0.1 close-gate + a
§0.4 pre-block + a §2 machine-checkable telemetry column, AND (b) the three V3 CONSOLIDATED REVISEs
landed.

### §6.1 — V3 fold verification (the three V3 REVISEs)

| # | V3 REVISE | Fold site | CH7 V4 verdict |
|---|---|---|---|
| 1 | **CH5 §C.5/§F.7 second-x86-surface (BLOCKING)** | `SYNTHESIS:58-75` (the "SECOND x86 surface … most consequential V3 fold" block), `:162-169` (census), `:246` (P1 close-gate crate-wide), `:491` (`x86_tree_deleted` telemetry redefined crate-wide) | **FOLDED** ✓ — `x86_tree_deleted` is now "NO x86 surface anywhere in `bbnf-simd` — `src/x86_64/` AND `ext/x86/` AND `build.rs` AND `lib.rs:247` re-homed; verified crate-wide `grep -riE 'avx\|gfni\|sve\|x86\|nasm'`, NOT `src/`-scoped." **This is the fold alphaA §1 is MISSING.** |
| 2 | CH2 §8.1 arm-census reach scoped honestly + relocated-seam structural check | `SYNTHESIS:18`,`:30`,`:78`,`:480` (`generator_grammar_branch_count` self-disclosing-token only; relocated-seam caught STRUCTURALLY by `runtime_target_rows_collapsed`, not the regex) | **FOLDED** ✓ — a regex is syntactically incapable of firing on a token-free `RuntimeTarget` data-table; the structural row-count check is the correct machine-check |
| 3 | CH1 §αD / CH7 §4 checkasm "18"→14 | `SYNTHESIS:43` ("'18 differential harnesses' corrected to the disk-true 12 single-kernel + 2 = 14") | **FOLDED** ✓ |

All three V3 REVISEs verified present and correctly sited in the αF contract. The contract is the
SOURCE OF TRUTH the stale alphaA §1 must be reconciled against.

### §6.2 — Addendum triple-binding (close gate + pre-block + telemetry)

| Surface | Close gate (§0.1) | Telemetry column (§2) | Pre-block (§0.4) | Disposition |
|---|---|---|---|---|
| G2 verbatim-blob | `grep CSS_GENERATED_RS → 0`; grammar-projected | `verbatim_blob_present == false` (`:478`) | verbatim-blob re-entry | **ACCEPT** |
| G3 single-emitter | `RuntimeEmitterKind` gone + canonical arm census + type census + structural row-count | `emitter_fork_present`; `generator_grammar_branch_count==0` (`:480`); `generator_grammar_type_count==0` (`:482`); `runtime_target_rows_collapsed` | fork resurrection / relocated-seam | **ACCEPT** — the relocated-seam is now caught STRUCTURALLY, not by a token-blind regex |
| G4 phantom-generic | `G` instantiated w/ prod grammar OR removed; DELETE default; `_proof_compiles` excluded | `phantom_generic_resolved` (`:483`) + `shared_value_trait_instantiations≥2` real-prod (`:484`) + `json_rich_navigation_preserved` | phantom re-entry | **ACCEPT** — the `G`-axis-not-`K`-axis scoping + the trait-impl test-exclusion are precise |
| G6 acceleration-wiring | reached at admission (grep hot path not tests); retire branch gated on a samply non-top-N row | `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` NOT `cfg-test-only` (`:490`) | acceleration claim | **ACCEPT** |
| PROVE distinct-grammar-output | Sheets `generated.rs` md5≠JSON≠CSS; `grep const.*_RS Sheets=0`; `sheets_grammar_shape==pratt-operator` | `generated_md5_distinct` (`:487`) + `generator_grammar_count==3` + `sheets_real_grammar` | distinct-output re-entry | **ACCEPT** |
| H1 timed-plane-symmetry | equal work, real corpus cold, no micro-fixtures; P2 deletes warm bench | `corpus_in_timer == true` (`:495`) + `materialization_framing` | corpus-out-of-timer / more-work | **ACCEPT** |
| P1/P4 x86-deleted (crate-wide) + gate | `find …/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` neutral-only; P4 scans leak surface | `x86_tree_deleted` crate-wide (`:491`) + `lock14_gate_scans_codegen` | x86/AVX/SVE | **ACCEPT** — crate-wide; the honest second-surface gate |

**SYNTHESIS.md overall: ACCEPT (all sections).** Every one of the six addenda is bound THREE ways
(§0.1 close gate + §0.4 pre-block + §2 telemetry the `gate-json` consumer REJECTs on). All three V3
REVISEs landed: the BLOCKING second-x86-surface is crate-wide in both the §0.1 close-gate (`:246`)
and the `x86_tree_deleted` telemetry (`:491`); the relocated-seam is machine-checked STRUCTURALLY;
the checkasm count is 14. The §2.x `honest-finding escape gated (a)-(c)` (`:262`) forecloses the
largest paper-close surface. No REVISE/REJECT.

---

## §7 — HANDOFF.md (the αF packet)

CH7 V4 confirms the six addenda + the three V3 folds are carried verbatim into S-P0+ with re-entry
pre-blocks.

| Section | CH7 V4 verdict |
|---|---|
| Status/fold block (`:7-25`) — names all three V3 REVISEs: CH5 §C.5 BLOCKING crate-wide x86; CH2 §8.1 arm-census reach + structural seam check; CH1/CH7 checkasm 18→14 | **ACCEPT** — HANDOFF explicitly carries the second-x86-surface crate-wide fold (`:12-17`) that alphaA §1 is missing |
| Current-state block — JSON +1.4%–164.7%; `ValueRef<G>` PHANTOM test-only; the SECOND x86 surface named (`:71-72`) | **ACCEPT** — matches CH7 ground truth |
| What-SK-V18-Opens (P1-P5 crate-wide x86, G1-G6, PROVE, H1) | **ACCEPT** — P1 (`:98-102`,`:208-210`,`:288-289`) deletes `src/x86_64/` AND `ext/x86/` AND nasm `build.rs` AND re-homes `lib.rs:247` |
| Gate-Posture addenda block — six addenda verbatim with one-line bindings | **ACCEPT** |
| Pre-Blocked Routes — verbatim-blob/phantom/distinct-output re-entry; corpus-out-of-timer; no second substrate; x86/AVX/SVE/nasm crate-wide (`:208-210`,`:242-244`) | **ACCEPT** |
| Inviolable-invariant 3 (`:242-244`) — aarch64-only: zero x86/AVX/SVE/nasm CRATE-WIDE; `src/x86_64/` AND `ext/x86/` AND `build.rs` all gone; verified crate-wide `grep -riE`, NOT `src/`-scoped | **ACCEPT** — the V4 hardening that makes the P1 gate honest |
| Inviolable-invariant 5 — three-surface Lock-14 gate (token scan + canonical FULL-alphabet arm census over codegen AND xtask + grammar-named-type census + `EventGrammar` emitter-token clause + structural row-count) | **ACCEPT** |
| Next-Move + revert-graph + hard-caps — PRUNE→G1→G2→G3→G4→G5/G6→PROVE→H1; dispatch-hard-cap 20/15/30 | **ACCEPT** |

**HANDOFF.md overall: ACCEPT.** Six addenda carried verbatim; all three V3 REVISEs present
(including the BLOCKING crate-wide x86 at `:12-17`,`:98-102`,`:242-244`,`:315-316`); each addendum
has a pre-block re-entry forbiddance; the Lock-14 three-surface gate is the V4 hardening. The
crate-wide x86 invariant (invariant 3) is the precise fold alphaA §1 lacks. No REVISE/REJECT.

---

## §8 — Cross-artefact addendum coverage matrix (CH7 V4 summary)

| Addendum | Live surface (CH7-verified @ `318d9c046`) | Named in | Close gate | Pre-block | Telemetry |
|---|---|---|---|---|---|
| **verbatim-blob** | `runtime_generator.rs:701` const `&str`, consumed `:91` | A§3.1, C§2.3, D-I1, E-B2, SYN-G2, HO | `grep CSS_GENERATED_RS → 0` | verbatim-blob re-entry | `verbatim_blob_present==false` |
| **distinct-grammar-output** | 7× md5 `b654562c…` | A§3.1, C-P3, D-I3, E-P3/B4, SYN-P3/PROVE, HO | md5-distinct census + `const.*_RS` Sheets=0 + structural row-count | distinct-output re-entry | `generated_md5_distinct` + `generator_grammar_count==3` + `sheets_real_grammar` |
| **single-emitter-path** | `grammar_provider.rs:40/:110` + `lib.rs:282/:291` + `runtime_generator.rs:17/:25` | A§3.1, C§2.3, D-I4, E-B1, SYN-G3, HO | `RuntimeEmitterKind → 0` AND arm census AND type census AND `runtime_target_rows_collapsed` | fork resurrection / relocated-seam | `emitter_fork_present` + `generator_grammar_branch_count==0` + `generator_grammar_type_count==0` |
| **phantom-generic** | `tape/mod.rs:175 G=AnyGrammar`; only `G` witness in `event_grammar_tests.rs` (`#[cfg(test)]`+`#[cfg(feature=proof)]`) | A§3.3, D-I5, E-B3, SYN-G4, HO | ≥2 real-prod OR `G` removed; `_proof_compiles` excluded; DELETE default | phantom re-entry | `phantom_generic_resolved` + `shared_value_trait_instantiations≥2` + `json_rich_navigation_preserved` |
| **timed-plane + corpus-in-timer** | `nonjson_css_l4.rs:66/:1989/:3091` warm 187B SHA fixtures | A§3.2, B§3.2, C-P2/§2.4, D-I9, E-A/B2, SYN-H1, HO | P2 delete + H1 frame | corpus-out-of-timer / more-work | `materialization_framing` + `corpus_in_timer==true` |
| **acceleration-wiring** | `runtime/src/lib.rs:574/:598/:608` inside `mod tests` (`:51 #[cfg(test)]`); `count_top_level_commas → generated.rs:810` cold (2-of-3 dead) | A§3.4, C§2.6, D-I6/DM3, E-B4, SYN-G6, HO | grep ≥1 non-`cfg(test)` caller; retire gated on samply non-top-N | acceleration claim | `acceleration_at_admission` (NOT `cfg-test-only`) |
| **x86-deleted (BOTH surfaces)** | `src/x86_64/` (24 files/847 LOC) **AND** `ext/x86/` (3499–3554 LOC) **AND** nasm `build.rs` (102 LOC) **AND** `src/lib.rs:247` | C§6-FOLD-1, D-I7-disp, E-P1, **SYN `:491`/HO inv-3** — **NOT alphaA (the §1 REVISE)** | `find …/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE 'avx\|gfni\|sve\|x86\|nasm'` neutral-only | x86/AVX/SVE/nasm | `x86_tree_deleted` (crate-wide) |

Every addendum has all five columns populated against a CH7-verified surface. The **no-contrivance /
x86-deleted / 7-replica-collapsed** trio is covered — but the x86-deleted row is the ONE where a
single cohort artefact (**alphaA**) carries the STALE `src/`-scoped enumeration the CH5 V3 BLOCKING
fold corrected everywhere else. That is the §1 REVISE.

---

## §9 — Disposition summary

| # | Section | Disposition | Note / Fix |
|---|---|---|---|
| 1 | **alphaA** (§3.2 x86 row `:178` + §0 census x86 row `:67` + §6 close-gate `:35`/`:266-267` + V4 FOLD log `:13-31`) | **REVISE** | Name BOTH x86 surfaces — `src/x86_64/` (24 files/847 LOC) AND `ext/x86/` (3499–3554 LOC vendored ASM) AND nasm `build.rs` (102 LOC) AND `src/lib.rs:247` — and widen the close-gate to crate-wide (`find …/src/x86_64 …/ext/x86 -type f = 0` AND `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` neutral-only), matching SYNTHESIS `:491` / HANDOFF inv-3 / alphaC §6 FOLD-1; add an R-row to the V4 FOLD log recording the CH5 V3 §C.5/§F.7 BLOCKING fold (the "ZERO V3 REVISE across all seven lenses" claim is inaccurate — CH5 V3 carried a BLOCKING REVISE touching alphaA's x86 census) |
| 2 | alphaA (all other sections: §1, §2/2.1, §3.1, §3.3, §3.4, §4, §5, §7) | ACCEPT | §3.4 remains the model checkasm-count framing; verbatim-blob/single-emitter/phantom/acceleration all live-cited correct |
| 3 | alphaB (all sections) | ACCEPT | honest-`None` posture re-confirmed; §6 correctly identifies the two V3 REVISEs as NON-αB; asmjson-AVX512-OUT makes no x86-gone close-claim |
| 4 | alphaC (all sections) | ACCEPT | §6 FOLD-1 is the canonical crate-wide second-x86-surface fold (with V4 extension — `Cargo.toml` nasm-rs dep + scalar/checkasm contract refs); alphaA must match THIS |
| 5 | alphaD (all sections incl §1 V4) | ACCEPT | the V3 REVISE (stale "18") is FOLDED — §1 V4 reads "14 … NOT 18"; §8 R1 records it; I7 is a claim-row disposing to crate-wide P1, not a close-gate def |
| 6 | alphaE (all clusters) | ACCEPT | carries the CORRECTED checkasm count (F4 ×4) AND the crate-wide P1 x86 scope; F13/F14 folds verified |
| 7 | SYNTHESIS.md (all sections) | ACCEPT | three V3 REVISEs verified + triple-binding intact; `x86_tree_deleted` crate-wide (`:491`); relocated-seam machine-checked STRUCTURALLY; honest-finding escape gated (a)-(c) |
| 8 | HANDOFF.md (all sections) | ACCEPT | six addenda + three V3 folds carried verbatim (incl crate-wide x86 at `:12-17`/inv-3); three-surface Lock-14 gate |

**CH7 V4 verdict.** The six new addenda fire HONESTLY across the cohort — each named against a
CH7-independently-re-verified live surface at HEAD `318d9c046` (`:701`; `:40/:110` + `:282/:291` +
`:17/:25`; md5 7→1; `:175 G=AnyGrammar` with the sole witness confined to the double-gated
`event_grammar_tests.rs`; `runtime/src/lib.rs:51 #[cfg(test)]` with 2-of-3 CSS NEON kernels dead and
count-commas cold; warm 187B SHA fixtures), each carrying a grep-able close gate, a §0.4 pre-block,
and a §2 machine-checkable telemetry column. The αF contract (SYNTHESIS + HANDOFF) folds all three
V3 CONSOLIDATED REVISEs correctly — INCLUDING the BLOCKING CH5 §C.5/§F.7 second-x86-surface fold
(crate-wide `x86_tree_deleted`) and the CH1/CH7 checkasm `18→14`. The no-contrivance /
7-replica-collapsed pillars are fully covered.

**The lens did not rubber-stamp a three-pass clean-modulo-one-straggler history.** CH7 V4 found —
for the THIRD pass running, on a NEW axis — the one place a cohort-wide fold did NOT propagate:
**alphaA, the x86-census ORIGIN artefact, still scopes x86 to `src/x86_64/` only** (census `:67`/
`:178`, close-gate `:35`/`:266-267`, and a V4 FOLD log asserting "ZERO V3 REVISE across all seven
lenses"), while alphaC §6 FOLD-1, alphaD I7-disposition, alphaE P1, SYNTHESIS `:491`, and HANDOFF
inv-3 all carry the crate-wide scope the BLOCKING CH5 V3 REVISE mandated. Because alphaA is the
results-extraction INVENTORY a downstream S-P3 wave reads to enumerate the P1 deletion set and
close-gate, the stale `src/`-scoped gate would PASS GREEN while ~3.5K LOC of x86 ASM + an
x86-assembler build driver survive — the exact "x86 gone is literally false" false-pass the CH5 V3
REVISE flagged BLOCKING, and the exact overfit-prune failure mode this lens guards. That is the one
**REVISE** (concrete fix in §1 + §9). No REJECT: nothing in the cohort overclaims a prune,
mis-attributes an addendum, or admits a contrivance as the bar.

8 sections dispositioned: 7 ACCEPT, 1 REVISE, 0 REJECT.

TALLY accept=7 revise=1 reject=0
