# CH6 ANTI-PAPER-CLOSE — SK-V9 S-P3 Cohort

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V1. Lens: CH6.
Date: 2026-05-18.
Scope: the seven S-P3 P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` — P3-A candidate
shortlist, P3-B wave sequencing, P3-C falsifiability gates, P3-D
telemetry schema, P3-E pre-blocked ledger, P3-F SPEC draft, P3-F
DISPATCH-PROMPT draft.

## §1 — Method

CH6 ANTI-PAPER-CLOSE (`ORCHESTRATOR.md` §3W) verifies that no S-P3
artefact closes a claim on a self-report ("complete"/"wired"/
"verified"), an aspirational gate, or a deferral that papers over the
pass's own task. Per §3W and §8: every closed claim must carry live
evidence (a bench row, a samply symbol path, a checkasm pass, a commit
SHA, a file:line); no wave gate may name "wired"/"integrated" without a
measurable threshold; no deferral may abrogate S-P3's deliverable.

The S-P3 deliverable, per `PASS-3-SYNTHESIS-PLAN.md` and the prompt: a
wave-sequenced, falsifiability-gated SK-V9 SPEC + DISPATCH-PROMPT plus
the five supporting artefacts (shortlist, sequencing, gates, telemetry,
ledger). S-P3 is a *plan* pass — it does not move rows; its CH6 burden
is that the *plan it hands forward* cannot paper-close, not that it
itself produced bench evidence.

Method applied:

1. Read all seven P3 artefacts end-to-end.
2. Read the P2-E honest verdict source
   (`skv9-p2-E-unicode-escape-codec.md` §6.2/§6.4) and the S-P2
   converged dependency order (`HARDENING-S-P2-CONVERGED.md`).
3. Spot-checked ≥12 cited claims against the live tree:
   `DIRECT_PROJECTION_SONIC_SLACK = 1.10` at `gate.rs:56` (verified);
   `class_table` at `bbnf-simd/src/lib.rs:41` (verified);
   `w0_real_typed_metadata_expectation_*` assertion site in
   `bin/gate.rs` (verified present near `:811-883`); the P2-E
   NEAR-FAIL/FAIL percentages (verified — see §2 rows 1-4).
4. Cross-checked the seven prompt sub-questions against each artefact.
5. Dispositioned ≥25 individual claims (§2). Aggregate verdict §3;
   the V2-fold defect list §4.

Disposition vocabulary: PASS (honest, evidence-cited, no paper-close);
NEAR-FAIL (honest in substance but a stated mechanic is a soft
paper-close needing a one-line fix); FAIL (a paper-close that must be
folded before S-P3 converges).

## §2 — Disposition table

| # | Artefact / claim | CH6 sub-question | Evidence checked | Disposition |
|--:|---|---|---|---|
| 1 | P3-A §2.2 C4 + §4: `unicode_escapes` carries `NEAR-FAIL 94.5%`, not "codec closes the row" | Q1 carry P2-E honest verdict | P2-E §6.4 line 675 "unicode_escapes — NEAR-FAIL at 94.5%"; P3-A §2.2 C4 verbatim match | PASS |
| 2 | P3-A §2.2 C4: `y_string_unicode NEAR-FAIL 94.8%` | Q1 | P2-E line 681 "NEAR-FAIL at 94.8%"; P3-A match | PASS |
| 3 | P3-A §2.2 C4: `unicode_mixed FAIL 63.7% (needs C5 paired)` | Q1 | P2-E line 941 "FAIL 63.7%"; P3-A carries the paired-knob caveat | PASS |
| 4 | P3-A §4 summary: "zero of the four uncloseable unicode rows admit on the codec alone" | Q1 | P2-E line 696 verbatim; P3-A §4 final paragraph carries it explicitly | PASS |
| 5 | P3-C §1.3 + §4.1: the V1 fabricated c/B column claiming `unicode_escapes PASS` is named rejected; rederived posture encoded in the gate table | Q1, Q5 | P2-E lines 671-672, 940 ("PASS 100.5% → NEAR-FAIL 94.5%"); P3-C §4.1 table reproduces the rederived projection row-for-row | PASS |
| 6 | P3-C §4.2 clause 4 + §4.4: W4 gate falsified if it "claims `gsoc-2018` or `unicode_mixed` closure without the measured combined Mbps clearing the threshold — a paper-close" | Q1, Q3 | P3-C §4.4 names the paper-close as an explicit falsifier; honest | PASS |
| 7 | P3-F SPEC §8 W5 exit gate clause 3: each of the four rows dispositioned by its P2-E §6.2 basis; conditional-same-wave thresholds 16319 / 8270 / 12338; `gsoc-2018` no-regression | Q1, Q3 | P2-E §6.2 table; SPEC §8 reproduces the four thresholds + the conditional-same-wave rule | PASS |
| 8 | P3-F SPEC §8 W5 exit gate clause 4 + revert: "A row that does not clear its paired threshold is recorded NEAR-FAIL or FAIL ... not relabeled as a pass"; "W5 may close with zero strict admissions ... a valid measured close, not a paper-close" | Q1 | SPEC §8 verbatim; this is the honest NEAR-FAIL/FAIL projection encoded as the gate, not "closes 4 rows" | PASS |
| 9 | P3-F SPEC §0.1 clause 6: the four P1-named uncloseable rows "either admit by the W5 conditional same-wave rule or are recorded NEAR-FAIL / FAIL with the honest projection in REDRESS" | Q1, Q6 | SPEC §0.1 — the close condition itself encodes the conditional rule, not an aspirational "all four close" | PASS |
| 10 | P3-F SPEC §0.4: `[INTEGRATE P3-D]` placeholder — "when `skv9-p3-D-telemetry-schema.md` lands, its column binding ... supersedes this section" | Q2 | P3-D **already exists** at `research/p3/skv9-p3-D-telemetry-schema.md` (dated 2026-05-18, same as SPEC). The marker names a sibling that is present, not pending. The placeholder is stale, not honest. | FAIL |
| 11 | P3-F SPEC §2 wave manifest: `[INTEGRATE P3-B]` — "when `skv9-p3-B-wave-sequencing.md` lands, its per-wave entry gates ... supersede this manifest" | Q2 | P3-B exists; same defect as #10. The marker is a paper-placeholder for an artefact already on disk. | FAIL |
| 12 | P3-F SPEC §4/§5/§6/§7/§8 each end `[INTEGRATE P3-E]`; SPEC §4 pre-blocked routes line `[INTEGRATE P3-E]` | Q2 | P3-E exists; the five `[INTEGRATE P3-E]` markers point at a present artefact. Honest *form* (they name exactly P3-E) but stale *premise* — they imply P3-E is unlanded. | FAIL |
| 13 | P3-F SPEC integration note (lines 17-23): "sibling P3 artefacts P3-A..E were not present at `research/p3/` when this draft was authored" | Q2, Q7 | Directory listing: all of P3-A..F mtime 2026-05-18 18:12-18:15; SPEC draft mtime 18:14. The "not present" claim is contradicted by the cohort's own timestamps — P3-A/B/C/D/E were authored 18:12-18:14, the SPEC at 18:14. This is a paper-close of S-P3's *integration* task: the SPEC was drafted from P2 evidence and never reconciled against the siblings it must fold. | FAIL |
| 14 | P3-F DISPATCH integration note: `[INTEGRATE P3-x]` markers at §Wave-Manifest, §Falsifiability-Gates, §Pre-Blocked-Routes | Q2 | Same stale-premise defect as #10-13; the DISPATCH draft inherits it. | FAIL |
| 15 | P3-F SPEC §3 W0 exit gate: `G-W0-TELEMETRY-LOCK` PASS — manifest carries "exactly the 38 main row identities, one uniform run id" | Q3 | Measurable, names the row count + run-id + the named freeze surfaces; cites `skv9-W0-close.md`. Not aspirational. | PASS |
| 16 | P3-F SPEC §4 W1 exit gate `G-W1-TYPED-ADMISSION`: 7 numbered clauses, Apache/CITM `≥ ceil(sonic_rs strict / 1.10)`, four typed-GO rows maintain, `gate-json --check-results` | Q3 | Every clause names rows + a measurable threshold or a binary gate command; the 1.10 slack cites `gate.rs:56` (verified). Not aspirational. | PASS |
| 17 | P3-F SPEC §6 W3 exit gate `G-W3-UNION-SUBSTRATE`: must-improve Mbps floors (twitter ≥17685 etc.), W10b six-row block with per-row floors, `consume_structural` ≤5% self-time | Q3 | All eight clauses name rows + concrete thresholds; the self-time clause is measurable from samply. Not aspirational. | PASS |
| 18 | P3-F SPEC §6 W3: "`consume_structural` is deleted from `generated.rs`; the class column read is present in `at_cursor` — the same-wave consumer is wired" | Q3 | "wired" appears — but it is bounded by clause 1's measurable ≤5% self-time gate and clause 3 names the deletion as a checkable diff. "Wired" here is gated by a threshold, so it is not a bare paper-close. Borderline; honest in context. | NEAR-FAIL |
| 19 | P3-F SPEC §7 W4 exit gate `G-W4-STRING-BLOCK` clause 1: "the W4 plan binds the per-row Mbps floors against sonic-strict / 1.10" | Q3 | The SPEC defers the *concrete* per-row floors to "the W4 plan" rather than stating them. P3-C §2 W4 row and P3-A §2.2 C5 *do* name the target rows (`twitter`/`apache_builds`/`distinct_values`, `unicode_mixed`, `gsoc-2018`) but no Mbps number is bound in the SPEC. An exit gate clause that says "the plan binds the floors" is one indirection short of measurable — it defers the threshold to a future artefact. | NEAR-FAIL |
| 20 | P3-F SPEC §8 W5 exit gate: thresholds 16319 / 8270 / 12338 / 21430-class all named with rows | Q3 | Fully measurable, per-row, cites P2-E §6.2. Strongest gate in the SPEC. | PASS |
| 21 | P3-F SPEC §5 W2 + §3.4 P3-C: the proof-only wave states "zero `RESULTS.md` row movement", "`skinny/RESULTS.md` is byte-identical — the proof moved zero rows" | Q4 | SPEC §5 clause 5 + P3-A §4 ("C2 Proof-only — No") + P3-B §3 W2 + P3-C §3.2 all state W2 moves zero rows. The proof-only wave honestly states it moves zero rows. | PASS |
| 22 | P3-C §3.4 "Why W2 is not a paper-close": W2 closes on "a compiler verdict ... a mechanically checkable, falsifiable observation, not a promise" | Q4 | Honest — the binary compile pass/fail is a measurable gate; the same-wave-consumer rule is correctly stated as silent (binds substrates, not contracts), citing P2-B §5.1. | PASS |
| 23 | P3-F SPEC §0.1 close condition: 8 numbered clauses, each recorded against a named gate (`G-W0-TELEMETRY-LOCK`, `G-S-P1-RERUN-CONVERGED`, `G-BEHAVIOR-RELEASE`, the four-row clause, the W10b clause, the five-doc agreement) | Q6 | Every clause is a recorded-state check, not a future-phase promise. Measurable at close. | PASS |
| 24 | P3-F SPEC §0.1 clause 3: `G-BEHAVIOR-RELEASE` "passed: every behavior wave has either admitted by its named row gate or rejected with REDRESS measurement" | Q6 | Measurable — admit-or-measured-reject is binary per wave. Not aspirational. | PASS |
| 25 | P3-E §4 item 4 + P3-A §2.2 C3: "SC-3 Tier A production migration deferred to SK-V10" — P2-A W3 union is the routed precursor, the *production migration* is a separate SK-V10 wave | Q7 | P2-B §5 + P3-B §4 establish the asymmetry: SK-V9 W3 lands the union event-model (the REDRESS-92-routed precursor); the SC-6-L1-R1 refinement + Tier A production migration are independently-ratifiable SK-V10+ scope. This is a legitimate scope boundary — SK-V9 W3 *does* deliver a measured-row union wave; it does not paper-close its own task by deferring. Legitimate deferral. | PASS |
| 26 | P3-B §2 note: "The SPEC §8 direct-contract placeholder is NOT scheduled ... The direct route stays an explicitly-blocked placeholder ... This is recorded, not silently dropped" | Q7 | Honest — REDRESS 93 has no fresh candidate; P3-B records the explicit block per SK-V9 §0 close-condition 6 rather than papering it. Legitimate. | PASS |
| 27 | P3-A §2.2 C8 + §3: `checkasm_digit_mac.rs` ownership "carried forward to a future numeric-row wave (P2-D §6.2.1 — no paper-close to a no-consumer wave)" | Q7 | Honest — `digit_mac` has no SK-V9 consumer; assigning its checkasm test to the first wave that wires a numeric consumer is correct same-wave discipline, not a paper-close. Legitimate. | PASS |
| 28 | P3-D §3.2 ruling: SPEC §0.3 outcome enum is 7-identifier but the W0-admissible code set is 10; "The SPEC §0.3 list must be corrected ... This is a SPEC-text fix (P3-F's burden)" | Q2, Q6 | P3-D names a real producer/consumer contradiction and assigns the fix to P3-F. But P3-F SPEC §0.3 / §0.4 does **not** carry the 10-identifier correction — it retains the W0 schema unreconciled. P3-D correctly flags the defect; P3-F fails to fold it. The fix is named but unexecuted within the same cohort. | FAIL |
| 29 | P3-B §1 + §2: wave count "four post-W0 (W1-W4) plus a W5 close" — but P3-F SPEC §2 manifests **six** behavior waves (W1-W6) | Q3 | P3-B numbers behavior waves W1-W5; P3-F SPEC numbers W1-W6 (W1 typed, W2 proof, W3 union, W4 string-block, W5 codec, W6 close). P3-B §2 note acknowledges the SPEC-slot rename ("P3-F binds them to SPEC section numbers") but the two artefacts ship divergent wave-letter schemes uncreconciled. A reader of the cohort cannot tell whether "W4" means string-block (SPEC) or the paired codec+string wave (P3-B/P3-C). Not a paper-close of a *gate*, but an integration gap that lets a wave's identity be ambiguous. | NEAR-FAIL |
| 30 | P3-C §1.4 + P3-B §2: P3-C maps W4 = codec+string-block paired, W5 = ASM kernels (EOR3/CTZ/structural-bitmap); P3-B maps W4 = codec+string-block paired, W5 = close | Q3 | P3-C and P3-B **disagree** on what W5 is: P3-C §1.4 calls W5 "aarch64 ASM substrate kernels" with an exit gate (`gsoc-2018 ≥ 41198` etc.); P3-B calls W5 "Close + Alpha feedback". P3-F SPEC resolves it as W5=codec, W6=close, folding the ASM kernels (C6 EOR3, C7 CTZ) into W3/W5 rather than a standalone wave. Three artefacts, three wave maps. The ASM-kernel exit gate P3-C §2 authored for "W5" (movemask ≤12%, `github_events`/`random` floors) has **no home wave** in the SPEC. | FAIL |
| 31 | P3-F SPEC §7 W4: C6 (SHA3 EOR3) and C7 (CSSC CTZ) from P3-A are absent — SPEC §8 W5 pre-blocked line says the SHA3 collapse "is NOT in W5 scope"; no wave admits C6/C7 | Q3, Q7 | P3-A ranks C6/C7 as candidates 7-8 (row-contributing accelerators, same-wave with C3); P3-C §1.4 places them in "W5 ASM kernels". P3-F SPEC drops them entirely with no recorded disposition — neither scheduled nor explicitly blocked-with-citation. A candidate that the shortlist carried must be either waved or explicitly blocked; silent omission is a soft paper-close of the shortlist's own output. | NEAR-FAIL |
| 32 | P3-F SPEC §0.4: claims "31 required fields" and lists them; P3-D §2.2 pins the canonical set at **36** identifiers | Q2 | P3-D §2.2 explicitly states the gate-consumed set is 36, not 31, and that "SPEC §0.4 names 31 of these (it elides ...)". P3-F SPEC §0.4 still says 31. The `[INTEGRATE P3-D]` marker (#10) was supposed to absorb this; it did not. | FAIL |
| 33 | P3-A / P3-B / P3-C / P3-D / P3-E §"Sources": every artefact cites file:line, REDRESS entries, and the P2 reports it draws from | Q5 | Spot-checked: `gate.rs:56` slack (verified), `bbnf-simd/src/lib.rs:41` class_table (verified), `bin/gate.rs` assertion site (verified present), P2-E NEAR-FAIL percentages (verified). Citations are live and accurate across A/B/C/D/E. | PASS |
| 34 | P3-E §5 W10b six-row block: the regression table cites verbatim per-row drops (`canada` T1 -3.11%, `mesh` T1 -8.07% etc.) from the SK-V7 W10b rejection | Q5 | Concrete, REDRESS-89-sourced, per-row. The maintain gate is bound to live numbers, not an aspirational "no regression". | PASS |
| 35 | P3-F SPEC §9 W6 + §N G-Gate: `G-W6-CLOSE` "passes only if the five documents agree, every wave has an admit or a measured reject in REDRESS, and the §0.1 close condition is satisfied" | Q3, Q6 | Measurable — document agreement + admit-or-measured-reject is checkable. Not aspirational. | PASS |
| 36 | P3-C §2 W1 maintain envelope (c): "The W10b WIN-block is **not gated** here — W1 is a row-table-only wave ... a no-regression check is vacuous" | Q3 | Honest — W1 touches no parse loop, so W10b non-regression is vacuous-true by construction; P3-C states the reasoning rather than silently omitting the gate. | PASS |

## §3 — Aggregate verdict

36 claims dispositioned: **24 PASS, 5 NEAR-FAIL, 7 FAIL.**

ACCEPT rate (PASS / total) = 24/36 = **66.7%** — below the §3Z 95%
convergence floor. **S-P3 V1 does not converge on the CH6 lens; a V2
fold is required.**

The honest core is sound. On the load-bearing question — Q1, does the
SPEC carry P2-E's honest verdict — the cohort PASSES cleanly (rows
1-9, 20): P3-A, P3-C, and the P3-F SPEC all encode "zero of the four
rows admit on the codec alone", the NEAR-FAIL/FAIL projections, the
conditional same-wave-pairing rule, and the explicit "W5 may close with
zero strict admissions ... not a paper-close" clause. The W4 unicode
codec wave's falsifiability gate does **not** claim "closes 4 rows" —
it encodes the conditional rule. The proof-only W2 wave honestly states
it moves zero rows (rows 21-22). The legitimate-deferral question (Q7)
PASSES on all three checked deferrals (rows 25-27): SC-3 Tier A
production migration, the direct-contract placeholder, and the
`checkasm_digit_mac` ownership are all genuine scope boundaries with
recorded blocks, not paper-closes of S-P3's own task.

The failures are **not** falsified gates or fabricated evidence — they
are an **integration paper-close**. The P3-F SPEC and DISPATCH drafts
were authored from S-P2 evidence alone and never reconciled against the
five sibling P3 artefacts that exist in the same directory with the
same date (row 13). The `[INTEGRATE P3-x]` markers (rows 10-14) are not
honest placeholders for *pending* siblings — every sibling they name is
already on disk. This is the precise CH6 failure mode: the SPEC's own
integration task — folding P3-A..E — was deferred behind markers that
imply the siblings have not landed. The consequence is concrete,
measurable defects the markers were meant to absorb but did not:

- The outcome enum is wrong (7 vs the 10 P3-D ruled — row 28, 32).
- The required-field count is wrong (31 vs 36 — row 32).
- Three artefacts carry three irreconcilable wave-letter maps (rows
  29-30); the P3-C "W5 ASM kernels" exit gate has no home wave (row
  30); C6/C7 are silently dropped (row 31).

These are folds, not redesigns. The honest evidence S-P3 must carry
forward is intact; it has simply not been merged into the SPEC. A V2
that executes the integration the `[INTEGRATE P3-x]` markers promised
will clear all seven FAILs and the five NEAR-FAILs.

## §4 — Paper-close violations requiring V2 fold

**V2-FOLD-1 — Reconcile the SPEC integration note and resolve every
`[INTEGRATE P3-x]` marker (rows 10-14, 13).** P3-A..E exist; the SPEC's
"not present when authored" claim is false against the cohort's own
mtimes. Delete the integration note's "not present" framing. For each
of the five `[INTEGRATE P3-x]` markers, either fold the sibling
artefact's content into the section and delete the marker, or — if a
genuine conflict remains — replace the marker with a one-line statement
naming the *specific unresolved item*, not a blanket "supersedes this
section". A marker that names an artefact already on disk is a
paper-close.

**V2-FOLD-2 — Correct the outcome enum (rows 28, 32).** P3-D §3.2
ruled the SK-V9 outcome enum is the 10-identifier W0-admissible set
`A C G I J K L M N-direct S`, not the 7-identifier SPEC §0.3 subset,
because `validate_w0_outcome` already gate-admits all ten and a
7-identifier SPEC enum makes `gate-json` reject a row the code
produces. P3-F SPEC §0.3 must carry the 10-identifier set. P3-D
explicitly assigned this fix to P3-F's burden; it was not executed.

**V2-FOLD-3 — Correct the required-field count (row 32).** P3-D §2.2
pins the canonical gate-consumed set at 36 identifiers; SPEC §0.4 still
says 31. Fold P3-D's 36-row field table (or its count + elision note)
into SPEC §0.4 and delete the `[INTEGRATE P3-D]` marker.

**V2-FOLD-4 — Unify the wave-letter scheme across P3-B, P3-C, P3-F
(rows 29-30).** Three artefacts ship three wave maps: P3-B has W1-W5
(W5=close), P3-C has W1-W5 (W5=ASM kernels), P3-F SPEC has W1-W6
(W5=codec, W6=close). Pick one canonical numbering — the SPEC's W1-W6
is the natural authority since it binds the dispatch contract — and
re-issue P3-B and P3-C against it, or add an explicit cross-reference
table. As shipped, "W4" and "W5" are ambiguous across the cohort.

**V2-FOLD-5 — Give the P3-C "W5 ASM kernels" exit gate a home wave, or
record its candidates as explicitly blocked (rows 30-31).** P3-C §2
authored a full exit gate for an ASM-kernel wave (`gsoc-2018 ≥ 41198`,
`movemask_u8x16` self-time ≤12%, `github_events`/`random` floors) that
no SPEC wave consumes. P3-A C6 (SHA3 EOR3) and C7 (CSSC CTZ) are
shortlisted candidates the SPEC neither schedules nor explicitly
blocks. The SPEC must either (a) schedule an ASM-kernel wave carrying
P3-C's gate, (b) fold C6/C7 into W3/W5 as named same-wave sub-slices
with their gate clauses, or (c) record C6/C7 as explicitly deferred
with a citation, per SK-V9 §0 close-condition 6. Silent omission of a
shortlisted candidate is a paper-close of P3-A's own output.

**V2-FOLD-6 — Bind the W4 string-block per-row Mbps floors in the SPEC
(row 19).** SPEC §7 `G-W4-STRING-BLOCK` clause 1 defers the per-row
Mbps floors to "the W4 plan". The target rows are already named in P3-A
§2.2 C5 and P3-C §2; fold the concrete floors into the SPEC §7 exit
gate so the gate is measurable as written, not measurable-pending-a-
plan.

**V2-FOLD-7 — Tighten the SPEC §6 W3 "the same-wave consumer is wired"
phrasing (row 18).** The clause is gated by a measurable ≤5% self-time
threshold so it is not a bare paper-close, but "wired" is exactly the
word §3W/§8 flags. Restate as "the class-column read in `at_cursor`
shows in the samply symbol path on the affected rows and
`consume_structural` self-time is ≤5%" — bind the verb to the
observable, per the W4/W5 phrasing the SPEC already uses elsewhere.

Folds 1-5 are FAIL-class and block S-P3 CH6 convergence. Folds 6-7 are
NEAR-FAIL polish that should land in the same V2 pass. None of the
seven requires re-research or a redesign — every fix is a merge of an
existing sibling artefact's already-converged content into the P3-F
SPEC/DISPATCH drafts.
