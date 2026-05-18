# CH6 ANTI-PAPER-CLOSE — SK-V9 S-P3 Cohort

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2. Lens: CH6.
Date: 2026-05-18.
Scope: the seven S-P3 P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` — verify the V2 integration
fold cleared the V1 CH6 "integration paper-close" (66.7%, 7 FAIL +
5 NEAR-FAIL).

## §0 — Method

CH6 ANTI-PAPER-CLOSE (`ORCHESTRATOR.md` §3W) verifies that no S-P3
artefact closes a claim on a self-report, an aspirational gate, or a
deferral that papers over the pass's own task. V1 dispositioned 36
claims at 24 PASS / 5 NEAR-FAIL / 7 FAIL; the V1 root cause was an
*integration paper-close* — the P3-F SPEC + DISPATCH drafts were
authored from S-P2 evidence alone, never reconciled against the
P3-A..E siblings on disk, and shipped five `[INTEGRATE P3-x]` markers
that named already-landed artefacts. V1 §4 raised seven V2-FOLD items.

V2 verification applied:

1. Read both V2-fold P3-F drafts (SPEC + DISPATCH) end-to-end; both
   carry `Cycle: V2` and a `§0 footer — V2 fold` line.
2. Read P3-A..E and cross-checked the V1-flagged defects (outcome
   enum, field count, wave scheme, C6/C7 disposition).
3. Spot-checked ≥12 cited claims against the live tree (§2).
4. Verified the seven prompt sub-questions Q1-Q7.
5. Dispositioned ≥20 V2 claims (§2). Aggregate verdict §3; any new
   paper-close §4.

Disposition vocabulary unchanged from V1: PASS / NEAR-FAIL / FAIL.

## §1 — V1-FAIL resolution (the seven V2-FOLD items)

| V2-FOLD | V1 defect | V2 state | Verdict |
|--:|---|---|---|
| 1 | Five `[INTEGRATE P3-x]` markers naming already-landed siblings; SPEC "not present when authored" framing | `grep -rn INTEGRATE` over all seven artefacts returns **zero** `[INTEGRATE P3-x]` markers — only the two V2-fold footer lines ("all [INTEGRATE] markers resolved"). The false "not present" integration note is deleted; the SPEC §0-§9 fold the P3-A..E content directly (wave manifest §2 cites P3-B; gates §4-§8 cite P3-C; schema §0.y cites P3-D; pre-blocked routes cite P3-E verbatim). | RESOLVED |
| 2 | Outcome enum 7-identifier (`A C G K L N-direct S`); P3-D ruled 10 | SPEC §0.x now carries the **10-identifier W0-admissible set** `A C G I J K L M N-direct S`, with the per-identifier gloss and the explicit "a narrower enum would make `gate-json` reject a row the code produces" rationale. Verified live: `validate_w0_outcome` (`report.rs:981-984`) `matches!` exactly `"A"\|"C"\|"G"\|"I"\|"J"\|"K"\|"L"\|"M"\|"N-direct"\|"S"`. | RESOLVED |
| 3 | Required-field count 31; P3-D pinned 36 | SPEC §0.y now carries the **36-identifier set**, lists all 36 names, cites P3-D §2.2, and binds the per-wave same-wave-consumption obligation. SPEC §1 non-negotiable restated as "the schema is the 36-identifier set". | RESOLVED |
| 4 | Three irreconcilable wave maps (P3-B W1-W5, P3-C W1-W5/ASM, P3-F W1-W6) | SPEC §2 now manifests **W1-W5 with W4 sub-waved into W4a-d** ("five behaviour brackets W1-W5"). P3-B (mtime 18:23) re-touched: §"wave count" reads "four post-W0 (W1-W4) plus a W5 close", W4 explicitly sub-waved. P3-A re-touched to W4a-d. The W1-W6 scheme is gone. | RESOLVED |
| 5 | P3-C "W5 ASM kernels" exit gate had no home wave; C6/C7 silently dropped from SPEC | SPEC §7.3 (W4c SHA3 EOR3) and §7.4 (W4d CSSC CTZ) now schedule C6/C7 as named sub-waves, each with a full exit gate (`G-W4c-EOR3`, `G-W4d-CTZ`), owner paths, and W10b binding. P3-A shortlist table rows 7-8 carry C6/C7 with risk/LOC/dependency. The structural-bitmap kernel is folded into W3 §6. | RESOLVED at SPEC level — see §4 D-1 for the P3-C residue |
| 6 | W4 string-block per-row floors deferred to "the W4 plan" | SPEC §7.1 `G-W4a-STRING-BLOCK` clause 1 still reads "the W4a plan binds the per-row Mbps floors against `ceil(sonic_strict / 1.10)`" but now **names the target rows and live baselines**: `unicode_mixed` today 6803 (`RESULTS.md:33`), `gsoc-2018` today 22184 (`:24`), and the standard-parity rule. The DISPATCH §"Falsifiability Gates" binds the floor formula. The threshold rule is measurable; the absolute number is one rule-application away. Improved from V1 but not fully numeric. | RESOLVED-as-NEAR (see §2 row 17) |
| 7 | SPEC §6 W3 "the same-wave consumer is wired" bare phrasing | SPEC §6 clause 3 now reads "the same-wave consumer is wired same-commit (P2-A §4.4 #1, #2). CH5 falsifier: `rg 'consume_structural' ... returns zero outside the deletion-commit diff." The verb is bound to a `rg` observable + the clause-1 ≤5% self-time gate. | RESOLVED |

Five of seven V2-FOLDs are cleanly RESOLVED; FOLD-6 lands as a
residual NEAR-FAIL (one indirection short of a literal number, but the
rows and the formula are both named — measurable as written); FOLD-5
is RESOLVED in the SPEC but leaves a stale-sibling residue in P3-C
(§4 D-1).

## §2 — V2 dispositions

| # | Artefact / claim | Sub-Q | Evidence checked | Disposition |
|--:|---|---|---|---|
| 1 | No `[INTEGRATE P3-x]` marker survives the V2 P3-F drafts | Q1 | `grep -rn INTEGRATE` over all seven artefacts: only the two V2-fold footer notes. Zero stale markers. | PASS |
| 2 | SPEC integration note's false "not present when authored" framing deleted | Q7 | `grep "not present\|when authored"` returns only the §0.3 row-table sentence ("Apache/CITM measured typed rows are not present until W1"). The V1 paper-close framing is gone. | PASS |
| 3 | SPEC §0.x outcome enum = 10-identifier W0-admissible set | Q2, Q5 | `report.rs:981-984` `validate_w0_outcome` `matches!` admits exactly the ten. SPEC §0.x reproduces them with the gate rationale. | PASS |
| 4 | SPEC §0.y schema = 36-identifier set | Q2 | SPEC §0.y lists 36 names, cites P3-D §2.2; SPEC §1 non-negotiable matches. | PASS |
| 5 | SPEC §0.1 close condition — 8 numbered clauses, each a recorded-state gate | Q5 | Every clause is a recorded check (`G-W0-TELEMETRY-LOCK`, `G-S-P1-RERUN-CONVERGED`, `G-BEHAVIOR-RELEASE`, the four-row clause 6, the W10b clause 7, the five-doc agreement clause 8). Measurable at close. | PASS |
| 6 | SPEC §0.1 clause 6 — the four uncloseable rows "either admit by the W4b conditional same-wave-pairing rule or are recorded NEAR-FAIL / FAIL ... W4 may close with zero strict unicode-row admissions; that is a measured outcome, not a paper-close" | Q1, Q2 | Verbatim P2-E §6.4 honest verdict encoded as the close condition itself, not an aspirational "all four close". | PASS |
| 7 | SPEC §7.2 W4b — "PAIRED with W4a — strictly adjacent, never separable ... zero of the four uncloseable rows admit on the codec alone" | Q2 | The codec gate does NOT claim "closes 4 rows". The pairing rule + the P2-E §6.4 verdict are stated as the binding constraint. | PASS |
| 8 | SPEC §7.2 `G-W4b-CODEC` — per-row conditional: `unicode_escapes` ≥16319 projected NEAR-FAIL 94.5%; `y_string_unicode` ≥8270 NEAR-FAIL 94.8%; `unicode_mixed` ≥12338 FAIL 63.7% on codec alone; `gsoc-2018` ≥21430 no-regression | Q2 | The four NEAR-FAIL/FAIL projections are encoded as the gate; `unicode_mixed` "admits iff the *combined* W4a + W4b measured Mbps clears". Honest projection, not paper-close. | PASS |
| 9 | SPEC §7.2 "the honest posture" — "W4b may close with zero strict unicode-row admissions ... not a paper-close" | Q2 | Verbatim P2-E §6.4 carry. Reverted-wholesale-only-on-checkasm-or-W10b clause present. | PASS |
| 10 | SPEC §5 W2 proof-only wave — "zero `RESULTS.md` row movement"; §5 clause 5 "`skinny/RESULTS.md` is byte-identical — the proof moved zero rows" | Q3 | W2 objective + clause 5 + clause 6 (`rg event_grammar ... returns zero`) all state the proof moves zero rows. Honest. | PASS |
| 11 | SPEC §3 `G-W0-TELEMETRY-LOCK` — "exactly the 38 main row identities, one uniform run id"; "populated all 36 schema fields" | Q4 | Measurable, names row count + run-id + the 36-field population + the freeze surfaces. Not aspirational. | PASS |
| 12 | SPEC §4 `G-W1-TYPED-ADMISSION` — 7 clauses, Apache/CITM `≥ ceil(sonic_typed / 1.10)`, four typed-GO maintain, `gate-json --check-results` | Q4 | Every clause names rows + a measurable threshold or a binary gate command; the ±9.1% slack cites `RESULTS.md:7,18,21,28`. | PASS |
| 13 | SPEC §6 `G-W3-UNION-SUBSTRATE` — must-improve floors (`twitter` ≥17685 etc.), W10b six-row block with per-row floors, `consume_structural` ≤5% self-time | Q4 | All eight clauses name rows + concrete thresholds; the `canada` floor is 15866 (`today × 0.98`, live sonic 12723) — the V1-flagged stale 15871 is gone, corrected with a citation. | PASS |
| 14 | SPEC §6 clause 3 — "the same-wave consumer is wired same-commit; CH5 falsifier: `rg 'consume_structural' ... returns zero outside the deletion-commit diff" | Q4, Q7 | The V1 NEAR-FAIL bare "wired" is now bound to an `rg` observable + clause-1 ≤5% self-time. The verb is gated. | PASS |
| 15 | SPEC §7.3 `G-W4c-EOR3` + §7.4 `G-W4d-CTZ` — C6/C7 each carry a home wave with a full exit gate | Q4, Q7 | The V1 "no home wave" FAIL is resolved: §7.3 binds C6 to W4c (EOR3 ladder, `checkasm_bitmap_prefix_xor_64.rs` three-way differential, W10b binding); §7.4 binds C7 to W4d (CSSC CTZ, `cargo asm` proof, W10b hard precondition). | PASS |
| 16 | SPEC §2 wave manifest — five behaviour brackets W1-W5, W4 sub-waved W4a-d; the W1-W6 scheme deleted | Q4 | §2 reads "five behaviour brackets (W1-W5), with W4 sub-waved into four sub-waves (W4a-d) ... inside the ≤12 skinny-bracket ceiling". P3-B re-touched to match. | PASS |
| 17 | SPEC §7.1 `G-W4a-STRING-BLOCK` clause 1 — "the W4a plan binds the per-row Mbps floors against `ceil(sonic_strict / 1.10)`" | Q4 | Improved from V1: the target rows (`unicode_mixed`, `gsoc-2018`) and live baselines are now named, and the floor *formula* is stated. But the absolute Mbps number is still one rule-application away — "the plan binds the floors" remains one indirection. Measurable-as-written via the formula; not literal. | NEAR-FAIL |
| 18 | SPEC §8 W5 + `G-W5-CLOSE` — "the five documents agree, every W1-W4 wave and sub-wave has an admit or a measured reject in REDRESS, and the §0.1 close condition is satisfied" | Q4, Q5 | Document-agreement + admit-or-measured-reject is checkable. W5 "carries zero source LOC and no CHALLENGE" — honestly a reconciliation wave. | PASS |
| 19 | SPEC §N `G-ALPHA-SK-V9` — 7 clauses, each a recorded gate or close-condition check | Q5 | Every clause is a recorded-state check; clause 5 restates the §0.1 clause-6 "zero strict admissions ... if every uncloseable row records NEAR-FAIL / FAIL honestly". Measurable. | PASS |
| 20 | SPEC §2.2 cascade-lock disambiguated — three distinct "same-wave" relations named | Q3, Q7 | §2.2 names cascade-lock / same-wave-consumer / codec-scanner-pairing as three distinct relations; the P2-D §0 "may not be split" is read as "a kernel must not land without the union substrate existing", satisfied by W3 preceding W4a-d. No paper-close. | PASS |
| 21 | SPEC §6 — SC-3 Tier A production migration deferred to SK-V10 | Q6 | P3-B §"Lock impact" row W3: "If a future SC-3 Tier A *production migration* wants the SC-6-L1-R1 refinement it is a separate SK-V10+ wave"; SK-V9 W3 lands the routed precursor union event-model. Legitimate scope boundary — W3 delivers a measured union wave, does not paper-close. | PASS |
| 22 | SPEC §8 direct-contract placeholder — recorded explicitly-blocked, not silently dropped | Q6 | SPEC §1 + the pre-blocked routes carry REDRESS 93 "scalar-parent / parent-digest fold" and REDRESS 66-69 "no SK-V9 wave enters the direct plane" as explicit blocks with citations. Legitimate. | PASS |
| 23 | W2 W4 W3 carry no new "wired/verified/complete" claim without evidence | Q7 | SPEC §1 non-negotiable: "No wave closes on a future-phase promise. 'Wired' or 'integrated' without a bench-row threshold is a paper-close." Every gate clause that says "wired" is bound to a same-commit `rg`/`samply` observable (W3 #3, W4a #3). | PASS |
| 24 | Citation layer — `gate.rs:56`, `bbnf-simd/src/lib.rs:41`, `report.rs:977`, live `canada` sonic | Q5 | Spot-checked: `DIRECT_PROJECTION_SONIC_SLACK = 1.10` at `gate.rs:56` (verified); `class_table` at `lib.rs:41` (verified); `validate_w0_outcome` at `report.rs:977` (verified, admits the ten); `canada` sonic-strict parse_only 12723 at `RESULTS.md:10` (verified). | PASS |
| 25 | DISPATCH draft V2 — wave manifest W1-W5 (W4a-d), 10-outcome enum, 36-field schema, live floors | Q2, Q4 | DISPATCH §"Wave Manifest" matches the SPEC §2 W1-W5/W4a-d scheme; §"Non-Negotiables" carries the 10-enum + 36-schema; §"Falsifiability Gates" carries the corrected `canada ≥ 15866` floor and the V1 stale-15871 correction note. | PASS |
| 26 | P3-D §3.2 ruling vs the V2 SPEC | Q2 | P3-D still reads "SPEC §0.3 names the SK-V9 outcome enum as `A C G K L N-direct S` — 7" and "SPEC §0.4 ... 31 field names". P3-D was NOT re-authored against the V2 SPEC (mtime 18:13, pre-dates the V2 SPEC's 18:28). The *ruling* is correct and the V2 SPEC executed it — but P3-D's prose now describes a SPEC that no longer exists. Cohort-internal staleness, not a SPEC paper-close. | NEAR-FAIL |
| 27 | P3-C §"W5" section vs the V2 SPEC | Q4 | P3-C §133 still carries a standalone "W5 — aarch64 ASM substrate kernels (EOR3 ladder, CSSC CTZ, structural-bitmap)" section with its own exit gate; the V2 SPEC folded these into W3/W4c/W4d and made W5 the close wave. P3-C was re-touched (mtime 18:24) for the W4a-d codec section but its §133 W5 ASM section was not reconciled. The SPEC is the authority and is correct; P3-C carries a divergent wave map for W5. | NEAR-FAIL |

## §3 — Aggregate verdict

27 claims dispositioned: **24 PASS, 3 NEAR-FAIL, 0 FAIL.**

ACCEPT rate (PASS / total) = 24/27 = **88.9%.** All seven V1 FAIL-class
defects are RESOLVED — every one at the level that matters for CH6,
the **P3-F SPEC + DISPATCH drafts**, which are the artefacts that hand
the plan forward and bind the dispatch contract:

- The five `[INTEGRATE P3-x]` markers are gone; the false "not present
  when authored" integration note is deleted (rows 1-2).
- The outcome enum is the live 10-identifier set, verified against
  `validate_w0_outcome` (row 3).
- The required-field schema is the 36-identifier set (row 4).
- The wave scheme is one canonical W1-W5/W4a-d map; the W1-W6 scheme
  is gone (row 16).
- C6/C7 each have a home wave (W4c/W4d) with a full exit gate; the
  orphaned "W5 ASM kernels" gate is gone from the SPEC (row 15).

On the load-bearing Q1/Q2 question — does the V2 SPEC carry P2-E's
honest verdict — the cohort PASSES cleanly (rows 6-9): §0.1 clause 6,
§7.2 W4b, the per-row NEAR-FAIL/FAIL projections, the codec-scanner
pairing rule, and the verbatim "W4 may close with zero strict
unicode-row admissions ... not a paper-close" clause are all encoded.
The W4b gate does **not** claim "closes 4 rows". The proof-only W2
wave honestly states it moves zero rows (row 10). Every wave's exit
gate names measurable rows + thresholds (rows 11-19). The SK-V9 close
condition (SPEC §0.1) is a set of recorded-state checks (row 5). The
dependency deferrals — SC-3 Tier A → SK-V10, the direct-contract
placeholder — are legitimate recorded scope (rows 21-22). No new
"wired/verified/complete" claim ships without an observable (row 23).

The three NEAR-FAILs are polish, not paper-close: one threshold-by-
formula (row 17) and two cohort-internal staleness residues (rows 26,
27) — see §4. None blocks the plan the SPEC hands forward.

**S-P3 V2 converges on the CH6 lens at 88.9%** — above the V1 66.7%
but below the §3Z 95% floor on a strict reading. The shortfall is
entirely the §4 D-1 cohort-staleness residue, not a SPEC paper-close;
a one-line V3 touch-up to P3-C/P3-D clears it.

## §4 — Any new paper-close

The V2 integration introduced **no new paper-close.** The SPEC and
DISPATCH are internally coherent and evidence-bound. Two cohort-
internal staleness residues remain — neither is a SPEC paper-close,
both are V3-touch-up class:

**D-1 — P3-C §133 "W5 ASM kernels" and P3-D §3.2 still describe the
V1 SPEC (rows 26, 27).** The V2 fold re-authored only P3-F (SPEC +
DISPATCH bumped to `Cycle: V2`); P3-A/B/C received the F-AUX surgical
W4a-d touch-ups (mtimes 18:23-18:24) but **P3-D and P3-E were not
re-touched** (mtimes 18:13-18:14, pre-dating the V2 SPEC). Consequence:

- P3-D §3.2 ruling text still reads "SPEC §0.3 names ... 7" and
  "SPEC §0.4 ... 31". P3-D's *ruling* is correct (10 / 36) and the V2
  SPEC *executed* it — but P3-D's prose now points at a SPEC that no
  longer exists.
- P3-C still carries a standalone §"W5 — aarch64 ASM substrate
  kernels" section with its own exit gate, even though the V2 SPEC
  folded those kernels into W4c/W4d and made W5 the close wave.

This is the *inverse* of the V1 defect — V1 was a SPEC that ignored
its siblings; V2 is siblings that lag the re-authored SPEC. It does
NOT paper-close S-P3's deliverable: the deliverable is the SPEC +
DISPATCH, and those are the canonical, correct, internally-coherent
authority. But a cohort that ships a SPEC saying "the enum is 10" next
to a P3-D saying "SPEC names 7" cannot be read as converged without a
reader-confusing contradiction. The V1 CONSOLIDATED F-AUX item
explicitly scoped P3-B/C/A touch-ups; it under-scoped — P3-C's W5
section and P3-D's ruling-prose SPEC-references also needed the fold.

**V3 touch-up (not a re-research, ~10 min):** (a) update P3-D §3.2 to
read "the V2 SPEC §0.x carries the 10-identifier set / §0.y carries
the 36-identifier set" — past-tense, ruling-executed; (b) replace
P3-C §"W5" with a one-line cross-reference: "the P2-D §5 ASM kernels
are scheduled as SPEC §7.3 W4c (EOR3) and §7.4 W4d (CTZ); the
structural-bitmap producer folds into §6 W3; SPEC W5 is the close
wave"; (c) bump P3-A..E to `Cycle: V2` so the cohort version is
uniform.

**D-2 — FOLD-6 residual (row 17).** SPEC §7.1 `G-W4a-STRING-BLOCK`
clause 1 still defers the literal per-row Mbps number to "the W4a
plan binds the floors", though it now names the target rows and the
`ceil(sonic_strict / 1.10)` formula. This is measurable-as-written
(the formula + the live `RESULTS.md` baselines fully determine the
number) and the DISPATCH §"Falsifiability Gates" binds the same
formula — so it is a NEAR-FAIL, not a FAIL. A V3 one-liner stating
the two computed floors (`unicode_mixed` and `gsoc-2018` against
their live sonic-strict rows) would make the gate literal. Not
blocking.

No FAIL-class paper-close survives V2. The honest evidence S-P3 must
carry forward — P2-E's "zero of four rows admit on the codec alone",
the conditional same-wave-pairing rule, the proof-only W2 zero-row
statement, the measured-row exit gates, the legitimate SK-V10
deferrals — is intact and correctly folded into the SPEC + DISPATCH.
V2 cleared the V1 integration paper-close; D-1/D-2 are cohort-version
hygiene, resolvable in a sub-10-minute V3 touch-up.
