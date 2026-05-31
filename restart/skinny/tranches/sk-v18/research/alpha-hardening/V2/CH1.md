# CH1 — CORRECTNESS (V2) — Pass Alpha SK-V18 alpha-hardening

Lens: CH1 Correctness per PASS-ALPHA §3 + ORCHESTRATOR §3W. Reviewer: **V2**.
Date: 2026-05-31. Subject: `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
+ `SYNTHESIS.md` + `HANDOFF.md` (the αF deliverable lives in `SYNTHESIS.md` + `HANDOFF.md`
per PASS-ALPHA §2/§6 — the absent `alphaF-*.md` is the prescribed structure, NOT a defect;
confirmed unchanged from V1).

Focus (per dispatch): every claim cites a V3 finding / RESULTS row / SHA at path:line;
the >SOTA framing is honest (lazy-vs-eager disclosed); gates are measurable.

This is the **V2 confirming pass**. The V2 artefacts are explicit folds of the V1 CHALLENGE
dispositions. CH1's job at V2 is twofold: (1) verify the three V1 CH1 REVISEs are folded
WITHOUT introducing a new error, and (2) independently re-verify the ground-truth claims at
HEAD (since the V2 fold touched the load-bearing numbers).

## Method — independent re-verification at HEAD (`318d9c046`)

Every CH1-checkable claim was re-grepped on disk this pass. Results:

| Claim | Verify command / source | Result |
|---|---|---|
| x86 tree = 24 files (23 `.rs` + 1 `.asm`) | `find …/bbnf-simd/src/x86_64 -type f` | **24** (23 `.rs` + `byte_class_from_eq_set_64.asm`) ✓ |
| x86 LOC | `.rs`-only `wc -l` = **742**; all-files = **847** (asm = 105) | both numbers real ✓ — see §Note-1 |
| x86 `unimplemented!` | `grep -rc unimplemented!` | **14** ✓ |
| `CSS_GENERATED_RS` | `grep -n "const CSS_GENERATED_RS" runtime_generator.rs` | `:701` ✓; raw-string runs `701→1611` (~910 LOC), `runtime_generator.rs` total 1611 LOC ✓ |
| `RuntimeEmitterKind` | `grep -n "enum RuntimeEmitterKind" grammar_provider.rs` | `:40` ✓ |
| `ValueRef` four-slot decl | `tape/mod.rs:175` | `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` ✓ (two defaulted axes) |
| `parse_w11_1_number` ×7 | `grep -c …json/generated.rs` | **7** ✓ |
| checkasm file count | `ls checkasm_*.rs \| wc -l` | **14** (12 single-kernel + `checkasm_common.rs` + `checkasm_parity.rs`) ✓ |
| `GENERIC_SCAN_ROOTS` | `lock14_baseline.rs` | `:2409` ✓ |
| `render(program: &SinkOnlyProgram)` | `json_sink_direct.rs:4` | ✓ |
| witnesses | `JsonEventGrammar`/`SheetsEventGrammar` | both present (`json/event_grammar_witness.rs:4`, `sheets_witness/event_grammar_witness.rs:4`) ✓ |
| sonic strict skipper | `sonic_skipper.rs:5-6` | `IgnoredAny::deserialize` + `.end()` ✓ (strict) |
| google-sheets.bbnf | `wc -l grammar/google-sheets/google-sheets.bbnf` | **185 LOC**, EXISTS in totality tree ✓ |
| W5 close-ledger N=200 ratios | `skv17-W5-close-ledger.md:99-102` | Track1 2473.1/2937.9/2773.4/2618.5; lcss 1119.1/1247.7/828.5/1312.0; **2.210×/2.355×/3.348×/1.996×** ✓ |
| JSON delta thinnest | RESULTS `apache_builds/parse_only` | 13129.331 vs sonic 12951.668 = **+1.4%** ✓ |
| JSON delta widest | RESULTS `unicode_escapes/parse_only` | 7897.449 vs sonic 2984.079 = **+164.7%** ✓ |
| JSON 51/51 | `grep -c …\| A \| GO \| strict \| measured-row` | **17 parse_only + 17 direct_to_struct + 17 real_typed_struct = 51** ✓ (17 corpora incl. `gsoc-2018`) |
| CSS not in RESULTS | RESULTS `css_l4/*` rows | all `not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED`; ZERO admitted typed CSS rows ✓ |
| 7 CSS replicas md5 at `f6a38445b` | `git show f6a38445b:…css_l4_*/generated.rs \| md5 \| sort -u` | **1** (byte-identical) ✓ |

The citation discipline is overwhelmingly strong across all V2 artefacts — every load-bearing
claim carries a path:line, a V3 finding id (D1–D4, C1–C3, A1–A6, F-n), a RESULTS row, or a
SHA, and every independently checkable one resolves at HEAD. The three V1 CH1 REVISEs are all
folded correctly (see §Fold-verification). **Two non-blocking accuracy notes survive; both are
on `αA`/`αC` self-narration, not on a load-bearing gate or number — they are flagged but do not
rise to a REVISE.**

---

## Fold-verification — the three V1 CH1 REVISEs

The V1 CH1 (`V1/CH1.md`) issued three REVISEs. Each is verified folded at the exact path:line:

1. **JSON range "+1.4%–78%" → "+1.4%–164.7%"** (V1 CH1 fix #1, was SYNTHESIS:107/:174/:322 +
   HANDOFF:26-27). FOLDED: `SYNTHESIS.md:14-15` records the fold; `:120-122` ("Track 1 > sonic
   +1.4%–164.7% … +78% = marine_ik; +164.7% = unicode_escapes widest"), `:188`, `:341-342`,
   `:308` all read +1.4%–164.7%. `HANDOFF.md:30-31` reads "+1.4%–164.7% … +164.7% =
   unicode_escapes widest." Independently re-verified against RESULTS: +1.4% (apache_builds) and
   +164.7% (unicode_escapes) are the exact thinnest/widest rows. **RESOLVED, accurate.**

2. **§0.6 yyjson/asmjson/RapidJSON marked honest `None` on aarch64** (V1 CH1 fix #2). FOLDED:
   `SYNTHESIS.md:324-326` ("yyjson / asmjson / RapidJSON are schema columns only — honest `None`
   on aarch64 … the gate must NOT be read to require an un-run engine's number; a fabricated
   competitor column is … REJECTed"). Mirrors αB §1.1/§3.3. Independently confirmed: `Cargo.toml`
   wires only simd-json + sonic-rs (αB §sources verified). **RESOLVED, accurate.**

3. **αE CSS PRESERVED→SOTA gate pinned to N=200 `css_canon_bench` medians** (V1 CH1 fix #3, was
   αE:91/:140). FOLDED: `alphaE-candidate-shortlist.md` F1 (`:16`) + B2 gate#1 table (`:108-117`)
   now pins each per-row floor to the N=200 Track1 Mbps with an explicit −3% floor (bootstrap
   ≥2398.9 / animate ≥2850.0 / tailwind ≥2690.2 / material ≥2540.0) and demotes the N=80
   reproduction to "cross-check only, NOT the gate reference"; B4 gate#4 (`:173`) carries the same
   floors. Independently re-verified: the N=200 Track1 medians (2473.1/2937.9/2773.4/2618.5) match
   the W5 close ledger `:99-102` exactly, and the −3% floors are arithmetically correct
   (2473.1×0.97 = 2398.9 ✓; 2937.9×0.97 = 2850.0 ✓; 2773.4×0.97 = 2690.2 ✓; 2618.5×0.97 = 2540.0 ✓).
   The gate is now a per-row machine-checkable Mbps threshold, not a ratio range. **RESOLVED,
   measurable.**

All three V1 CH1 REVISEs are folded with the correct numbers, the correct path:line targets, and
no over-correction. The V2 fold is clean on the CH1 axis. **Zero orphan REVISE from V1 CH1.**

---

## Disposition by section (V2)

### αA — Results Extraction — **ACCEPT**

Every JSON row (§1, 51/51) carries Track1/sonic/Δ tuples sourced to `RESULTS.md` `parse_only`
rows; independently re-verified — the per-corpus table reproduces RESULTS, the +1.4%…+164.7%
span is the true thinnest/widest pair. §2 correctly states the CSS headline does NOT live in
RESULTS.md (verified: all 24 `css_l4/*` rows are `AUDIT-FALSIFIED` broadcast diagnostics, one
tuple `2319.041/2362.037/929.281`) and routes the real numbers to the W5 close ledger §3 +
`css_canon_bench` (verified: ledger `:99-102` carries the four ratios). The §3 overfit-surface
path:lines re-verify (CSS_GENERATED_RS:701, RuntimeEmitterKind:40, ValueRef four-slot:175, x86
24 files, parse_w11_1×7). The §2.1 lazy-vs-eager caveat (H1) is explicit. The V2-FOLD ledger
(§0, §6) correctly carries the four V1 cross-lens REVISEs (CH7 x86 24-file, CH5 two-axis
`K`/`G`, CH4 checkasm-12, CH2 Sheets-source) and each is independently true on disk.

**Non-blocking accuracy note (Note-2, does NOT block):** αA §6 caveat asserts "a raw `diff` of
the 7 CSS `generated.rs` in the *working tree* now reports DIFFERS … at the audited close SHA
they are byte-IDENTICAL." My on-disk check this pass finds the **working-tree** md5 also
collapses to a single hash (`… | sort -u | wc -l` = 1) — i.e. the working tree is NOW identical
again (the regen noise αA described has apparently been re-regenerated to parity). This makes the
caveat stale-but-conservative: it warned of a false-refutation that no longer exists, which is
harmless. The load-bearing claim (7 identical at `f6a38445b`) is independently TRUE
(`git show f6a38445b:… | md5 | sort -u | wc -l` = 1). No fix required; the caveat is over-cautious,
not wrong-direction. ACCEPT.

### αB — Competitor Deltas — **ACCEPT**

The strict-vs-strict (JSON) vs lazy-vs-eager (CSS) fairness-plane table (§0) is the single most
honest framing in the cohort and is unchanged from the V1-ACCEPTed version. §1.1 marks
yyjson/asmjson/RapidJSON as schema-columns-only / honest `None` on aarch64 (independently
verified). §3.3 forbids fabricating an un-run engine's number. §1.4 correctly demotes the typed
rows as schema-conditional, NOT the unconditional bar. The §6 V1→V2 fold record honestly states
αB had ZERO REVISE/REJECT at V1 and resolves the lone cross-cohort note by presenting BOTH
N-planes side-by-side (§2.2 Plane A N=200 headline / Plane B N=80 cross-check) with the
canary-plane divergence documented (material @N=200 vs tailwind @N=80) and the JSON range pinned
at the full +1.4%–164.7%. The N=200 headline ratios and the N=80 cross-check ratios both
independently reconcile (N=200 to the W5 ledger; N=80 to V3 AGENT-5). The plane discipline is now
non-mixable downstream — exactly the V1 CH1 cross-cohort warning, resolved. ACCEPT.

### αC — REDRESS Digest — **ACCEPT**

Every PRUNE wave (P1–P5) carries live-at-HEAD evidence (file census, md5, grep counts), all
re-verified this pass (x86 24 files, 7 CSS dirs single-md5, parse_w11_1×7, CSS_GENERATED_RS:701,
RuntimeEmitterKind:40). The §2.1–§2.6 pre-block families each carry the measured-refutation
SHA/factor (AZ-IV 118× `cb14970f`; StructRegistry 983×/10583×) AND an SK-V18-specific re-open
test keyed to the new surfaces (generator, shared trait, phantom `G`). The §0.A V2 fold correctly
resolves the two V1 REVISEs on αC (CH2 §3.5 P3 collapse-default; CH5 C.4 P4 witness/`EventGrammar`
seam) and the CH3 sequencing note (P1/x86-tag same-commit). The §0.B state-delta
(`emit_fact_stream` gone, `W5C_REQUEST_FACT_PROFILES` a retirement comment) is consistent with the
αD verified grep=0. The Lock load-bearing list (§3) cites `LOCKS.md`.

**Non-blocking accuracy note (Note-3, does NOT block):** αC §0.A and §4 assert "**No V1
CONSOLIDATED was produced** (the V1 wave committed CH1..CH7 but no CONSOLIDATED)"; αD §8 echoes
this. On disk a `V1/CONSOLIDATED.md` (7703 bytes, aggregate tally CH1 4/3/0, CH2 24/8/0, …) DOES
exist — written `13:52`, concurrent with / just after the V2 alpha artefacts (alphaC at 13:51).
The most charitable reading: at the moment the αC author began the V2 fold, the CONSOLIDATED had
not yet been written, so the statement was true-at-authorship and is now stale. **This is not a
correctness defect on any gate or number** — the αC fold independently resolved the *actual* V1 CH
dispositions (verified folded), which is what matters; whether a CONSOLIDATED roll-up existed is
narration, not substance. Flagged for the V2 CONSOLIDATOR's awareness (so it does not repeat
"no V1 CONSOLIDATED"), but it does not warrant a REVISE — the load-bearing content is correct
and complete. ACCEPT.

### αD — Validated/Invalidated Ledger — **ACCEPT**

V1–V8 (validated) and I1–I10 (invalidated) each cite SHA/RESULTS/path:line; the §6 verification
log re-greps every checkable claim at HEAD and reports command + result — all reproduce this pass
(x86 742 `.rs` LOC, replica IDENTICAL, phantom `G` two-axis, `K` real at `json/view.rs:86…256`,
`DocumentView` impl `json/view.rs:68`, CSS_GENERATED_RS:701, metalang ×7, google-sheets.bbnf 185
LOC). The §8 V2 FOLD log correctly resolves the three V1 REVISEs on αD (CH5 §D.2 two-axis `K`/`G`;
CH5 §D.3 `DocumentView` impl-citation re-pin; CH2 §4.3 Sheets owner-surface) — each independently
true. The DM rows distinguish conditional typed-struct wins from the unconditional `parse_only`
proof. §5 pre-block assertion is defensible. The FNV-quarantine claim (V8) is independently
consistent: the only `runtime/` `fnv64` is the CSS diagnostic provenance stamp
(`push_hex64(&mut out, fnv64(...))` at `css_l4_*/generated.rs:394/899`), exactly as αD V8 cites
(`:393-394,899-900`); it gates no parse work. ACCEPT.

### αE — Candidate Shortlist — **ACCEPT** *(was REVISE at V1; the V1 CH1 REVISE is folded)*

The V1 CH1 REVISE on αE (CSS PRESERVED→SOTA gate ambiguous between N=200 and N=80, range-not-
per-row) is FOLDED as F1 (`:16`) + B2 gate#1 (`:108-117`) + B4 gate#4 (`:173`) + cross-cutting
note 7 (`:202`): every CSS preservation floor is now pinned to the N=200 `css_canon_bench`
close-ledger per-row Mbps with an explicit −3% floor, the N=80 set demoted to cross-check, and
the SPEC instructed never to mix the planes. Independently re-verified: the N=200 baselines match
the ledger and the −3% floors are arithmetically correct (§Fold-verification #3). The falsifiability
triple (PRESERVED→SOTA / GRAMMAR-DERIVATION-PROOF / DISTINCT-GRAMMAR-OUTPUT) remains operational
(mutate-the-`.bbnf` test, md5-distinct + grammar-neutral-body co-gate). Every candidate carries
owner paths, scalar-ref/checkasm status (corrected to 12 single-kernel differentials per F4 —
independently verified `ls checkasm_*.rs`=14 = 12+2), same-wave consumer, LOC budget, risk,
pre-blocks. The apache_builds +1.4% tripwire (B1 gate#1) is the correct thinnest-row canary
(verified against RESULTS). The remaining gates are measurable (grep counts, md5-distinct,
−3% per-row thresholds, `accepts_current_allowlist` red-on-reintroduction). The other five V1
REVISEs on αE (F2–F8: Sheets source, md5-necessary-not-sufficient, checkasm-12, G6-body-bound,
phantom DELETE-default + test-excluded grep, rich-ast structural gate, x86 24-file) are folded
and each is independently consistent with ground truth. The G6 NEON-body ceiling (F5: PMULL
`bitmap_prefix_xor_64` as the ONE committed body, others retire-or-consumer-gated) makes the LOC
budget bounded rather than open-ended — a measurable cost gate. ACCEPT.

### SYNTHESIS.md — **ACCEPT** *(was REVISE at V1; both V1 CH1 REVISEs folded)*

The two V1 CH1 REVISEs are folded (§Fold-verification #1, #2): the JSON range is +1.4%–164.7% at
every occurrence (`:15`, `:120-122`, `:188`, `:308`, `:341-342`), and §0.6 (`:324-326`) marks
yyjson/asmjson/RapidJSON honest-`None`-on-aarch64 with an explicit REJECT of a fabricated
competitor column. The close-condition table (§0.1) is per-gate verifiable-by-grep; the telemetry
binding (Section 2) adds machine-checkable generalization columns (`verbatim_blob_present==false`,
`emitter_fork_present==false`, `generator_grammar_branch_count==0`,
`phantom_generic_resolved∈{instantiated,deleted}`, `generated_md5_distinct`,
`sheets_grammar_shape==pratt-operator`, `acceleration_at_admission` NOT `cfg-test-only`,
`corpus_in_timer==true`) with an executable `gate-json --skv18-generalization-report` consumer
(§Section 2 tail) that explicitly REJECTS the dishonest states. The lazy-vs-eager honesty (H1) is
carried in §0.1 (G2/H1 gate), §0.6, and the `materialization_framing` column. The G4 axis
discipline (target the `G:EventGrammar` axis, NOT the already-real `K=Kind`; DELETE is the
abrogate-before-patch default) is correct and matches the verified four-slot `ValueRef` decl. The
N=200/N=80 CSS-plane discipline is carried (§0.2 uses the N=200 ratios as the bar; the αE −3% gate
references N=200 Mbps). No measurable-claim error survives. ACCEPT.

### HANDOFF.md — **ACCEPT** *(was REVISE at V1; the V1 CH1 REVISE folded)*

The V1 CH1 REVISE (the "+1.4%–78%" error at `HANDOFF.md:26-27`) is folded: `:30-31` now reads
"Track 1 +1.4%–164.7% … `+1.4%` = apache_builds thinnest, `+164.7%` = unicode_escapes widest, per
alphaA §1 / `skinny/RESULTS.md`" — independently correct against RESULTS. The CSS ratios it carries
(`:33` bootstrap 2.210× / animate 2.355× / tailwind 3.348× / material 1.996×) are the N=200 ledger
set, verified. The 16 backlog items each tag their V3 finding id; the six CHALLENGE addenda
(`:159-170`), the R10 close criterion (`:276-285`), the pre-blocks (`:172-216`), and the revert
dependency graph + hard-cap defaults (`:287-297`) are all measurable and accurately mirror the
SYNTHESIS. The canonical Lock-14 `match grammar`-arm co-gate (`:224-234`, invariant 5) is
correctly stated as catching a different leak class than the token scan. ACCEPT.

---

## §Notes (non-blocking, flagged for the CONSOLIDATOR)

- **Note-1 (x86 LOC framing):** αA/αC/αD/αE cite x86 as "742 LOC." Verified: 742 is the `.rs`-only
  count; all-24-files (incl. the 105-LOC `.asm`) is 847. Every artefact that cites 742 ALSO names
  the 24-file count and the `.asm` separately (αA §3.2, αC §1-P1, αE F8), so the LOC figure is
  unambiguous in context (it is the recoverable Rust LOC; the `.asm` is deleted by file-count, not
  LOC-budget). NOT a defect — the prune close gate is `find …/x86_64 -type f` = 0, which deletes
  all 24 regardless of which LOC figure is headlined.
- **Note-2 (αA §6 working-tree caveat stale):** see αA disposition. Over-cautious, not
  wrong-direction. No fix.
- **Note-3 (αC/αD "no V1 CONSOLIDATED"):** see αC disposition. A V1 CONSOLIDATED exists on disk
  (written concurrent with the V2 fold); the statement is stale-at-read, true-at-authorship. The
  CONSOLIDATOR should not repeat "no V1 CONSOLIDATED was produced." No correctness impact on any
  gate/number.

None of the three notes is a measurable-claim error, a fabricated number, an uncited claim, or an
unmeasurable gate. All are self-narration / framing items on artefacts whose load-bearing content
independently verifies. Per the CH1 standard (a REVISE requires a claim that contradicts its cited
source, an unmeasurable gate, or a fabrication), none rises to REVISE.

---

## Summary

The V2 cohort folds all three V1 CH1 REVISEs correctly, with the correct numbers, at the correct
path:lines, and without over-correction. Independent re-verification at HEAD reproduces every
CH1-checkable claim: the JSON +1.4%–164.7% span (apache_builds / unicode_escapes), the 51/51
breakdown (17×3), the N=200 CSS ledger ratios + −3% floors, the x86 24-file/742-`.rs`-LOC census,
the phantom `G`-axis four-slot `ValueRef`, the byte-identical CSS replicas at `f6a38445b`, the
metalang ×7 leak, the sonic strict skipper, and the google-sheets.bbnf source. The strict-comparator
gate honors the §4.2 "if runnable" qualifier (yyjson/asmjson/RapidJSON honest `None`). The
lazy-vs-eager (H1) honesty is disclosed throughout. Every gate is machine-checkable per row.

- **ACCEPT:** αA, αB, αC, αD, αE, SYNTHESIS, HANDOFF (7 sections).
- **REVISE:** none (the three V1 CH1 REVISEs are folded; the three surviving items are non-blocking
  self-narration notes, not measurable-claim errors).
- **REJECT:** none.

The CH1 axis has converged: V2 is ≥95% ACCEPT on Correctness with zero orphan REVISE.

TALLY accept=7 revise=0 reject=0
