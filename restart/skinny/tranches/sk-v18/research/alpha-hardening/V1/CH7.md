# CH7 — OVERFIT-PRUNE (V1)

Lens: CH7 OVERFIT-PRUNE. Pass: PASS-ALPHA SK-V17→SK-V18 cycle V1 (the GENERALIZATION
cycle / inflection backtrack). Per PASS-ALPHA §3 + ORCHESTRATOR §3W. Reviewer focus:
the SIX NEW CHALLENGE addenda fire honestly across the alpha artefacts — **verbatim-blob**
(const-`&str` `@generated` = hand-written), **distinct-grammar-output** (N grammars = N
non-identical `generated.rs`), **single-emitter-path** (flag/enum forks), **phantom-generic**
(uninstantiated `<G>`), **timed-plane-symmetry + corpus-in-timer**, **acceleration-wiring**
(NEON at admission, not `#[cfg(test)]`) — plus no-contrivance, x86 deleted, 7-replica collapsed.

Subject reviewed: `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md`. (There is
no `alphaF.md` artefact; per PASS-ALPHA §2 the α-F output IS `SYNTHESIS.md` + `HANDOFF.md`,
which are reviewed in their stead — see §0 note.)

Ground truth re-verified live at HEAD `318d9c046` (SK-V17 close `f6a38445b`); each disposition
below cites path:line/SHA and carries a concrete fix where REVISE/REJECT.

---

## §0 — Independent verification log (CH7 re-grep, HEAD `318d9c046`)

Every load-bearing overfit/prune claim re-verified before disposition. The addenda are NOT
accepted on the artefacts' word — they are confirmed against the tree.

| Claim under the addenda | CH7 command | Result | Verdict |
|---|---|---|---|
| **verbatim-blob** (CSS const-`&str`) | `grep -n 'const CSS_GENERATED_RS' runtime_generator.rs` | `701:const CSS_GENERATED_RS: &str = r#"` | CONFIRMED |
| **single-emitter-path** (fork) | `grep -n 'enum RuntimeEmitterKind\|RuntimeEmitterKind::' grammar_provider.rs` | `40:pub enum RuntimeEmitterKind`; `110:… != RuntimeEmitterKind::RequestFacts` | CONFIRMED |
| **distinct-grammar-output** (7 replicas) | `git show f6a38445b:…css_l4_*/generated.rs \| md5 \| sort -u` | single hash `b654562ccff46ed62dd48e9ace325830` (7→1) | CONFIRMED at close SHA |
| **phantom-generic** (`<G>` default, no real instantiation) | `grep -n 'G: EventGrammar = AnyGrammar' tape/mod.rs`; `grep -rn '(Json\|Css\|Sheets)EventGrammar>' runtime/src \| grep -v test` | `175: … G: EventGrammar = AnyGrammar`; **zero** non-test production instantiations | CONFIRMED |
| **acceleration-wiring** (NEON dead at admission) | nearest `#[cfg(test)]` above the `find_css_significant`/`find_comment_close` callers (`lib.rs:574,598,608`) | `51:#[cfg(test)] / 52:mod tests {` — no intervening `mod`; callers are inside `mod tests` | CONFIRMED dead-at-admission |
| **corpus-in-timer / timed-plane** (OLD warm bench) | `grep -n 'measure_mbps\|lightningcss_facts\|EXPECTED_FIXTURE_BYTES' nonjson_css_l4.rs` | `66:…=187`; `528:lightningcss_facts`; `3091:measure_mbps`; `1989: input.len()!=187` | CONFIRMED live |
| **x86 census** | `find …/x86_64 -type f \| wc -l`; `…-name '*.rs' \| wc -l`; `wc -l`; `grep -rc unimplemented!` | **24 files total (23 `.rs` + 1 `.asm`)**, **742 LOC**, **14** `unimplemented!` | CONFIRMED (census-count caveat below) |
| metalang leak | `grep -c parse_w11_1_number json/generated.rs` | `7` | CONFIRMED |
| gate exclusion | `grep -n 'GENERIC_SCAN_ROOTS\|diagnostic-x86' lock14_baseline.rs` | `2409` roots; `2463 diagnostic-x86`; `4956 assert(contains)` | CONFIRMED |
| Sheets stub | `find sheets_witness -name '*.rs' \| xargs wc -l` | **25 LOC** | CONFIRMED |
| 16-lock count | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` | `16` | CONFIRMED |

**One census discrepancy surfaced (the only material CH7 correction to ground truth):** the x86
tree is **24 files total = 23 `.rs` + 1 `.asm`** (`byte_class_from_eq_set_64.asm`). The artefacts
disagree on the count: alphaA §3.2 says "23 `.rs` files, 742 LOC"; alphaC §1-P1, alphaD I7, αE-A,
SYNTHESIS, HANDOFF say "24 files." **Both are defensible** (24 total files / 23 `.rs`), but the
inconsistency is a CH1-adjacent precision defect that CH7 flags here because the P1 close gate
must delete the `.asm` too, not just the `.rs` tree. This is folded into the alphaA disposition
below as a REVISE (the only one in this report).

---

## §1 — alphaA (results extraction) — the overfit inventory

alphaA is the results-and-overfit inventory. CH7 dispositions each overfit-surface section
against whether the addenda are correctly named AND the path:line is live.

| Section | Content | Addendum coverage | Disposition |
|---|---|---|---|
| §0 headline table | 6-axis ground truth (JSON valid / CSS valid / substrate / generator-overfit / phantom / x86) | names all six axes; tags "OVERFIT (P1 delete)", "verbatim-blob", "phantom-generic" | **ACCEPT** — every axis maps to a verified surface |
| §1 JSON 51/51 | per-corpus Δ vs sonic-strict table | not an addendum surface (it is the >SOTA bar) | **ACCEPT** |
| §2 CSS 1.996–3.348× + §2.1 lazy-vs-eager caveat | the H1 honesty pin | timed-plane-symmetry (H1) named correctly | **ACCEPT** |
| **§3.1 generator-does-not-exist** | CSS const-`&str` `:701`; JSON templates `:195`…; FORKED `:40/:110`; 7 replicas | verbatim-blob / single-emitter-path / distinct-grammar-output ALL named with live path:line | **ACCEPT** — verified `:701`, `:40`, `:110`, md5 |
| **§3.2 contrivance/wrong-arch** | x86 tree `:5/:285-287`; OLD bench `:528/:3091`; metalang `×7`; gate holes `:2409/:2463` | timed-plane-symmetry / corpus-in-timer named; x86 census | **REVISE** — see fix below |
| §3.3 phantom + divergent value API | phantom `<G>`; divergent JSON-tree-vs-CSS-stream | phantom-generic named; distinguishes phantom `<G>` from real `ValueRef<Kind>` (a precise, non-overclaimed distinction) | **ACCEPT** |
| §3.4 NEON wiring honesty | callers dead at admission (`lib.rs:574,598,608` in `#[cfg(test)]`); 5 scalar passthroughs; UDOT orphan | acceleration-wiring named correctly; verified `mod tests` @ `:51` | **ACCEPT** |
| §4 substrate validated | Lock 1 holds; the pre-blocks | n/a | **ACCEPT** |
| §5–§7 close-condition seeds + synthesis | binding inventory for αF | n/a | **ACCEPT** |

**§3.2 REVISE — concrete fix.** alphaA §3.2 states the x86 tree as "23 `.rs` files, **742 LOC**"
(`alphaA-results-extraction.md:134`). CH7 verified the tree is **24 files = 23 `.rs` + 1 `.asm`
(`byte_class_from_eq_set_64.asm`), 742 LOC, 14 `unimplemented!`**. The `.rs`-only framing UNDER-counts
the prune target: the P1 close gate (`find …/x86_64 -type f` = 0) requires deleting the `.asm` as
well, and the artefact's own §5 P1 close-condition ("742 LOC, 0 files remain") is correct only if
"files" means all 24. **Fix:** amend `:134` to "24 files (23 `.rs` + 1 `.asm`), 742 LOC, 14
`unimplemented!`" so it agrees with alphaC/D/SYNTHESIS/HANDOFF and the P1 gate deletes the `.asm`.
This is a precision REVISE, not a finding reversal — the prune target and its disposition are sound.

---

## §2 — alphaB (competitor deltas) — the bar to preserve

alphaB fixes the >SOTA bar SK-V18 must PRESERVE and the fairness plane each sits on. CH7's
overfit lens checks: (a) the CSS asymmetry is disclosed (not papered as equal-work — the
timed-plane-symmetry addendum), and (b) no contrived comparator is admitted as the bar.

| Section | Content | Disposition |
|---|---|---|
| §0 standing + asymmetry pin table | JSON near-symmetric strict; CSS asymmetric lazy-vs-eager | **ACCEPT** — the asymmetry stated up front is the timed-plane-symmetry honesty done right |
| §1 JSON strict-vs-strict bar (per-corpus) | sonic strict Skipper (no utf8_lossy), apache_builds +1.4% canary | **ACCEPT** — strict comparator plane correct (sonic_skipper.rs:5-6, Cargo.toml:23) |
| §1.3 simdjson DOM | flagged as different output plane, NOT the strict bar | **ACCEPT** — honest plane discipline |
| §1.4 Track 2 typed caveat | conditional on hand-tuned per-corpus schema; NOT the unconditional bar | **ACCEPT** — correctly demotes the contrived schema row (matches DM1) |
| §2 CSS lazy-vs-eager bar | track1_rich vs lightningcss full-CSSOM, asymmetry disclosed; 25-33% rich rider cost | **ACCEPT** — the asymmetry is the load-bearing disclosure; the kept harness is `css_canon_bench` |
| §3.3 NOT-runnable comparators | yyjson/asmjson/RapidJSON honest `None`; asmjson AVX-512 x86-only OUT | **ACCEPT** — directly forecloses the contrivance of populating a column with an un-run engine's number |
| §3.4 H1 options A/B | symmetric comparator OR rename+footnote | **ACCEPT** — both preserve H1; "silence does not" |
| §4 preservation bar table | per-grammar must-hold + canary rows + risk | **ACCEPT** |

**alphaB overall: ACCEPT.** The §3.3 honest-`None` posture is the strongest anti-contrivance
stance in the cohort — it pre-empts the exact failure mode (fabricated competitor number) the
timed-plane/corpus-in-timer addenda guard. CH7 notes one non-blocking framing nuance for the
record (not a REVISE): αB §0 headlines CSS at "1.9-2.9×" (live N=80) while alphaA/SYNTHESIS
headline "1.996-3.348×" (N=200 W5 close). Both are cited to their source and both are the
PRESERVE bar (not a growth target), so this is a measurement-N difference, not a contradiction
— αB itself discloses the N=80-vs-N=200 provenance (`alphaB:157`). ACCEPT as-is.

---

## §3 — alphaC (REDRESS digest) — the PRUNE waves + pre-blocks

alphaC is the most addendum-dense artefact: it frames P1-P5 as PRUNE waves and re-keys the six
pre-block families to the new generator surfaces. CH7 dispositions per the addenda.

| Section | Content | Addendum | Disposition |
|---|---|---|---|
| §0 framing + state-delta | PRUNE-then-GENERALIZE; `emit_fact_stream` already gone (grep=0) | n/a | **ACCEPT** — the state-delta (do-not-re-fight-retired-surfaces) is exactly right |
| **§1-P1 delete x86** | 742 LOC / 24 files / 14 `unimplemented!` / 0 intrinsics; `:5/:285-287/:2463` | x86-deleted | **ACCEPT** — file count "24" is correct here (unlike alphaA); close gate is `find … = 0` |
| **§1-P2 delete OLD bench** | `measure_mbps:3091` warm, 85-357B SHA fixtures, more-work competitor | corpus-in-timer / timed-plane-symmetry | **ACCEPT** — verified `:66/:528/:1989/:3091`; "keep css_canon_bench" is the H1 keeper |
| **§1-P3 collapse 7 replicas** | md5 single-hash; collapse-to-1-or-N-distinct | distinct-grammar-output | **ACCEPT** — close gate binds the diff-census addendum |
| §1-P4 fix gate holes | extend `GENERIC_SCAN_ROOTS`; remove `diagnostic-x86`; abrogate-before-patch (P4 is the only patch) | n/a (gate-scope) | **ACCEPT** |
| §1-P5 purge metalang | `parse_w11_1_number ×7`; gated by G1 grammar-derived names | n/a | **ACCEPT** |
| §2.1 AZ-IV eager → re-open test on G1/G2/G4 | per-leaf eager payload re-land via the generator | n/a (pre-block) | **ACCEPT** — re-keys to the new generator surface correctly |
| §2.2 StructRegistry → re-open on G3/G4 | per-leaf registry deref via un-forked emitter; Lock 2 `Layout` not `StructLayout` | n/a | **ACCEPT** |
| **§2.3 fact-stream → residual fork** | `CSS_GENERATED_RS` still hand-written; `RequestFacts` still the CSS fork | **verbatim-blob + single-emitter-path** | **ACCEPT** — the retirement-clause re-open test ("closes with `CSS_GENERATED_RS` still const `&str`") is the precise addendum binding |
| §2.4 24-broadcast → PERMANENT | one tuple → N rows; Sheets corpus danger | n/a | **ACCEPT** |
| §2.5 FNV/fixture → PERMANENT | generator must not emit per-corpus capacity/fixture names; P5 the live instance | n/a | **ACCEPT** |
| §2.6 x86/AVX/SVE → PERMANENT | binds the rebuild + G6 ASM backlog to not re-add x86; acceleration-wiring named | acceleration-wiring | **ACCEPT** |
| §3 single distinction + corollary | "checked TWICE — runtime output AND the emitter that produces it" | all | **ACCEPT** — the "generator is the new carrier surface" corollary is the sharpest pre-block insight in the cohort |

**alphaC overall: ACCEPT (all sections).** This artefact correctly weaponizes the new addenda as
**re-open tests keyed to the generator**, not merely as static greps — the §3 corollary (every
re-open test runs against `runtime/.../generated.rs` AND `codegen/.../runtime_generator.rs`) is
the load-bearing CH7 insight: a refuted carrier can re-land at its SOURCE. No REVISE/REJECT.

---

## §4 — alphaD (validated/invalidated ledger)

alphaD's INVALIDATED table (I1-I10) is where each addendum gets its "new CHALLENGE lens" tag.
CH7 checks each tag is correctly attached to a verified surface.

| Row | Claim invalidated | Addendum lens tagged | Verified | Disposition |
|---|---|---|---|---|
| I1 | CSS grammar-driven | (verbatim-blob, via §3) | `:701` ✓ | **ACCEPT** |
| I2 | JSON projects from grammar | (verbatim-blob) | `json_sink_direct` ✓ | **ACCEPT** |
| I3 | 7 sub-grammars admitted | **distinct-grammar-output** | md5 ✓ | **ACCEPT** |
| I4 | one codegen path | **single-emitter-path** | `:40` ✓ | **ACCEPT** |
| I5 | `ValueRef<G>` parametric | **phantom-generic** | `:175 = AnyGrammar`, 0 prod instantiation ✓ | **ACCEPT** |
| I6 | NEON acceleration of CSS scan | **acceleration-wiring** | `mod tests:51` ✓ | **ACCEPT** |
| I7 | aarch64-only satisfied | (x86-deleted) | 742 LOC ✓ | **ACCEPT** |
| I8 | Lock-14 gate meaningful | gate-scope-honesty | `:2409` exclusion ✓ | **ACCEPT** |
| I9 | equal-work CSSOM | (timed-plane-symmetry/H1) | track1_4field vs rich ✓ | **ACCEPT** |
| I10 | clean shipped symbols | (metalang) | `×7` ✓ | **ACCEPT** |
| §3 DEMOTED DM1-DM4 | typed rows conditional; substrate-ready-not-proven; 5 scalar passthroughs; UDOT orphan | `_neon`-suffix-truth | F6/F7 ✓ | **ACCEPT** |
| §4 STILL-OPEN S1-S13 | the 13 candidates with parity oracles | all | maps 1:1 to INVALIDATED | **ACCEPT** |
| §5 pre-blocked | 8 families carried verbatim | n/a | **ACCEPT** |
| §6 verification log | direct re-grep at `7dbe44c22` | — | **ACCEPT** — alphaD ran its own verification (matches CH7's) |

**alphaD overall: ACCEPT.** Every addendum lens is attached to an empirically confirmed surface;
the §6 self-verification log is the discipline CH7 wants (the artefact does not assert what it did
not grep). DM3's `_neon`-suffix-truth and DM4's orphan-UDOT correctly feed G6's same-wave-consumer
rule — no orphan kernel survives without an admission-path caller.

---

## §5 — alphaE (candidate shortlist) — the falsifiability triple

alphaE folds the 13 backlog items into 5 clusters (A, B1-B4) under a falsifiability **triple**
(preserved->SOTA / grammar-derivation-proof / distinct-grammar-output). CH7 checks each cluster's
gate operationalizes the addenda as machine-checkable falsifiers.

| Cluster | Items | Addendum gate | Disposition |
|---|---|---|---|
| §0 triple | PRESERVED / GRAMMAR-DERIVATION (mutate-`.bbnf`→output-changes) / DISTINCT-OUTPUT | the three load-bearing gates | **ACCEPT** — "mutate the `.bbnf` → regenerated output changes; a const courier cannot pass" is the exact operational falsifier the verbatim-blob lens needs |
| **A** PRUNE | P1-P5, ≈−7100 LOC | x86=0; replicas collapsed; gate meaningful; metalang purged | **ACCEPT** — pure deletion, LOW risk, entry-gate for all B |
| **B1** un-fork + project JSON | G3+G1; single-emitter-path gate (`grep RuntimeEmitterKind → 0`) | single-emitter-path / verbatim-blob | **ACCEPT** — JSON-first ordering justified (spine already grammar-shaped); apache_builds +1.4% named as the hard must-hold canary |
| **B2** derive CSS | G2; `grep CSS_GENERATED_RS → 0` | verbatim-blob (centrepiece) | **ACCEPT** — "the verbatim-blob lens was authored to catch this"; LOW risk (scalar hot path) |
| **B3** shared trait + kill phantom | G4+H1; phantom resolved (`grep ValueRef<…EventGrammar> ≥1 OR G:EventGrammar → 0`) | phantom-generic / timed-plane (H1) | **ACCEPT** — the instantiate-XOR-delete falsifier is structurally verifiable; preserve-rich-ast guarded |
| **B4** PROVE Sheets + NEON | PROVE+G5+G6; 3 distinct `generated.rs`; acceleration-at-admission (`grep ≥1 non-test caller`) | distinct-grammar-output / acceleration-wiring | **ACCEPT** — the sharpest litmus; same-wave-consumer rule prevents orphan kernels; honest-finding escape (HANDOFF §6) for a failed Sheets litmus |
| SUMMARY + cross-cutting 1-6 | sequencing binding; kept-honest artefacts; no re-open | all | **ACCEPT** — net ≈−9150 LOC; "deletes more than it adds" is the correct generalization-cycle shape |

**alphaE overall: ACCEPT.** The falsifiability triple converts every addendum into a grep-able
exit gate (e.g. `grep -c 'RuntimeEmitterKind' → 0`, `md5` distinctness, `grep ≥1 non-cfg(test)
caller`). Cross-cutting note 2 (the honest-finding escape: a hand-shaping that survives becomes a
NAMED grammar-parameterized primitive, never a silent `_RS` blob) is the abrogate-before-patch
discipline applied correctly — it forecloses the paper-close failure mode. No REVISE/REJECT.

---

## §6 — SYNTHESIS.md (the αF contract) — the goalset

This is the master αF output (PASS-ALPHA §2 α-F = `SYNTHESIS.md` + `HANDOFF.md`). CH7 checks
the §0.1 close-condition table binds each addendum to a verifiable gate, and §0.4 pre-blocks the
re-entry of each.

| Surface | Addendum binding | Disposition |
|---|---|---|
| §0.1 G2 close | "verbatim-blob CHALLENGE passes: no `@generated` CSS file is a verbatim `&str` literal in codegen"; verify `grep -c CSS_GENERATED_RS → 0` | **ACCEPT** |
| §0.1 G3 close | "single-emitter-path CHALLENGE passes; `RuntimeEmitterKind` gone" | **ACCEPT** |
| §0.1 G4 close | "phantom-generic CHALLENGE passes; `<G>` instantiated with real grammar OR deleted; ≥2 real production instantiations; test-only `_proof_compiles` does NOT count" | **ACCEPT** — the explicit exclusion of `_proof_compiles` is precise |
| §0.1 G6 close | "acceleration-wiring CHALLENGE passes; reached at admission not `#[cfg(test)]`; no `_neon` label on scalar body" | **ACCEPT** |
| §0.1 PROVE close | "Sheets `generated.rs` md5-distinct from JSON+CSS (distinct-grammar-output)" | **ACCEPT** |
| §0.1 H1 close | "timed-plane-symmetry + corpus-in-the-timer CHALLENGE passes; equal work on real corpus cold; no micro-fixtures" | **ACCEPT** |
| §0.1 P1/P4 close | x86-deleted; gate scans the leak surface | **ACCEPT** |
| §0.4 pre-blocks | explicit "verbatim-blob re-entry", "phantom-generic re-entry", "distinct-grammar-output re-entry", "timed-plane-asymmetry / corpus-out-of-the-timer / more-work competitor" all listed as forbidden | **ACCEPT** — each addendum has both a close-gate AND a pre-block (forward + backward) |
| §2 telemetry | new columns `verbatim_blob_present==false`, `emitter_fork_present==false`, `phantom_generic_resolved∈{instantiated,deleted}`, `generated_md5_distinct`, `acceleration_at_admission∈{admission,…}` (NOT `cfg-test-only`), `corpus_in_timer==true` | **ACCEPT** — every addendum is a machine-checkable column the `gate-json` consumer REJECTS on |
| §0.5 litmus + §0.6 comparator gate | binary-structural close on grammar-DERIVED parsers; strict comparator preserved | **ACCEPT** |

**SYNTHESIS.md overall: ACCEPT.** This is the strongest part of the cohort for CH7's lens: every
one of the six addenda is bound THREE ways — (1) a §0.1 close-condition gate, (2) a §0.4 pre-block
re-entry forbiddance, (3) a §2 telemetry column the executable `gate-json` consumer rejects on.
That triple-binding (close + pre-block + machine-checkable telemetry) is precisely what prevents an
addendum from being satisfied in narrative but violated in code. No REVISE/REJECT.

CH7 notes (non-blocking, for the record): SYNTHESIS §0.1 G6 references the x86 file count
indirectly via "(all 24 files)" at the P1 row — consistent with alphaC/D (24), inconsistent with
alphaA (23 `.rs`). The alphaA REVISE in §1 above reconciles this; SYNTHESIS itself is correct and
needs no change.

---

## §7 — HANDOFF.md (the αF packet)

CH7 checks the HANDOFF carries the six addenda verbatim into S-P0+ and pre-blocks their re-entry.

| Section | Addendum binding | Disposition |
|---|---|---|
| Gate Posture block | lists all six addenda verbatim (verbatim-blob / distinct-grammar-output / single-emitter-path / phantom-generic / timed-plane-symmetry+corpus-in-the-timer / acceleration-wiring) with one-line definitions binding S-P0 + every pass CHALLENGE | **ACCEPT** |
| Pre-Blocked Routes | "verbatim-blob re-entry (a new const-`&str` courier); phantom-generic re-entry (a second uninstantiated `<G>`); distinct-grammar-output violation (byte-identical replicas)" + timed-plane-asymmetry/corpus-out-of-timer/more-work | **ACCEPT** |
| Inviolable invariants 1-7 | aarch64-only (x86 deleted, P1); grammar-neutral gate must scan; >SOTA preserved from grammar-DERIVED parsers | **ACCEPT** |
| Next Move | CHALLENGE → G-Alpha → S-P0(audit w/ six addenda) → S-P1/P2/P3; PRUNE→GENERALIZE→PROVE→HONESTY sequencing; `--skv18-generalization-report` gate consumer | **ACCEPT** |

**HANDOFF.md overall: ACCEPT.** The six addenda are carried verbatim into the downstream passes
(S-P0 binds them as audit lenses), and each has a pre-block re-entry forbiddance. The Next-Move
sequencing (P4 gate-meaningful MUST land before the G2/G3 emitter rebuild, so the gate is
trustworthy as the emitter is rebuilt) is the correct dependency order — it prevents B1/B2 from
re-leaking under a blind gate. No REVISE/REJECT.

---

## §8 — Cross-artefact addendum coverage matrix (CH7 summary)

The six addenda, confirmed fired across the cohort with a live surface AND a close gate AND a
pre-block:

| Addendum | Live surface (verified) | Named in | Close gate | Pre-block |
|---|---|---|---|---|
| **verbatim-blob** | `runtime_generator.rs:701` const `&str` | A§3.1, C§2.3, D-I1, E-B2, SYN-G2, HO | G2: `grep CSS_GENERATED_RS → 0` | §0.4 verbatim-blob re-entry |
| **distinct-grammar-output** | 7× md5 `b654562c…` | A§3.1, C-P3, D-I3, E-P3/B4, SYN-P3/PROVE, HO | P3+PROVE: md5-distinct census | §0.4 distinct-output re-entry |
| **single-emitter-path** | `grammar_provider.rs:40/:110` | A§3.1, C§2.3, D-I4, E-B1, SYN-G3, HO | G3: `grep RuntimeEmitterKind → 0` | §0.4 (fork resurrection) |
| **phantom-generic** | `tape/mod.rs:175 =AnyGrammar`, 0 prod-instantiation | A§3.3, D-I5, E-B3, SYN-G4, HO | G4: ≥2 real OR `G:EventGrammar→0` | §0.4 phantom re-entry |
| **timed-plane-symmetry + corpus-in-timer** | `nonjson_css_l4.rs:66/:1989/:3091` warm SHA fixtures | A§3.2, B§3.2, C-P2/§2.4, D-I9, E-A/B2, SYN-H1/§0.6, HO | P2 delete + H1 frame | §0.4 corpus-out-of-timer/more-work |
| **acceleration-wiring** | `lib.rs:574,598,608` inside `mod tests:51` | A§3.4, C§2.6, D-I6/DM3, E-B4, SYN-G6, HO | G6: `grep ≥1 non-cfg(test) caller` | §0.4 (acceleration claim) |

Every addendum has all four columns populated against a CH7-verified surface. No addendum is
asserted in narrative without a live surface, a close gate, and a pre-block. **The "no
contrivance / x86 deleted / 7-replica collapsed" trio is likewise fully covered** (P1 close `find
… = 0`; P2 delete the warm bench; P3 md5-distinct census).

---

## §9 — Disposition summary

| # | Section | Disposition | Fix (if REVISE/REJECT) |
|---|---|---|---|
| 1 | alphaA §0,§1,§2,§3.1,§3.3,§3.4,§4-7 | ACCEPT | — |
| 2 | **alphaA §3.2** | **REVISE** | `:134` x86 census "23 `.rs` files, 742 LOC" → "24 files (23 `.rs` + 1 `.asm` `byte_class_from_eq_set_64.asm`), 742 LOC, 14 `unimplemented!`" so it agrees with alphaC/D/SYN/HO and P1 deletes the `.asm` |
| 3 | alphaB (all sections) | ACCEPT | — |
| 4 | alphaC (all sections) | ACCEPT | — |
| 5 | alphaD (all sections) | ACCEPT | — |
| 6 | alphaE (all clusters) | ACCEPT | — |
| 7 | SYNTHESIS.md (all sections) | ACCEPT | — |
| 8 | HANDOFF.md (all sections) | ACCEPT | — |

**CH7 verdict:** the six new addenda fire HONESTLY and exhaustively across the cohort — each is
named against a CH7-independently-verified live surface (`:701`, `:40/:110`, md5-7→1, `:175`+0
prod-instantiation, `mod tests:51`, `:66/:3091`), each carries a grep-able close gate, and each
carries a §0.4 pre-block re-entry forbiddance plus a §2 machine-checkable telemetry column. No
contrivance survives the goalset; x86 deletion (P1) and 7-replica collapse (P3) are correctly
gated. The SINGLE defect is a census-precision inconsistency in alphaA §3.2 (23-vs-24 file count
for the x86 tree) — REVISE, with the concrete fix above; it does not reverse any finding, and the
P1 prune target is otherwise sound. No REJECTs: nothing in the cohort overclaims a prune,
mis-attributes an addendum, or admits a contrivance as the bar.

8 sections dispositioned: 7 ACCEPT, 1 REVISE, 0 REJECT.

TALLY accept=7 revise=1 reject=0
