# CH6 ANTI-PAPER-CLOSE — SK-V9 S-P3 Cohort, V3

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3. Lens: CH6.
Date: 2026-05-18.
Scope: the seven S-P3 P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` — verify the V3
comprehensive integration fold cleared the V2 cohort-staleness residue
(88.9%, 3 NEAR-FAIL: row 17 FOLD-6 threshold-by-formula, rows 26-27
P3-D/P3-C V1-SPEC staleness).

## §0 — Method

CH6 ANTI-PAPER-CLOSE (`ORCHESTRATOR.md` §3W) verifies that no S-P3
artefact closes a claim on a self-report, an aspirational gate, or a
deferral that papers over the pass's own task. V2 dispositioned 27
claims at 24 PASS / 3 NEAR-FAIL / 0 FAIL = 88.9%; the V2 shortfall was
*not* a SPEC paper-close — it was cohort-version staleness, P3-C §133
"W5 ASM kernels" and P3-D §3.2 still describing the superseded V1 SPEC,
plus the W4a threshold-by-formula. The V3 fold was tasked to re-author
P3-C/D/E to the unified manifest, sub-divide W4b, run a W3 cap check,
and apply arithmetic corrections.

V3 verification applied:

1. Read all seven V3 artefacts end-to-end; confirmed each is stamped
   `Cycle: V3` (line 3) and carries a `§0 V3 fold footer`.
2. Cross-checked the V2-flagged staleness (P3-C wave map, P3-D
   ruling-prose) against the re-authored V3 text.
3. Spot-checked ≥12 cited claims against the live tree (§2).
4. Verified the eight CH6 prompt sub-questions.
5. Dispositioned ≥20 V3 claims (§2). Aggregate §3; any new
   paper-close §4.

Disposition vocabulary unchanged: PASS / NEAR-FAIL / FAIL.

## §1 — V2-residue resolution

| V2 residue | V2 defect | V3 state | Verdict |
|---|---|---|---|
| D-1a (V2 row 26) — P3-D §3.2 still read "SPEC §0.3 names … 7 / §0.4 … 31"; described a superseded V1 SPEC | Cohort-internal staleness | P3-D is re-authored `Cycle: V3`. §3.1-§3.2 now read past-tense: "The V3 SPEC §0.x carries the SK-V9 outcome enum as exactly that **10-identifier W0-admissible set** `A C G I J K L M N-direct S`"; the binding ruling block says "**enacted by the V3 SPEC §0.x**"; the §0 footer item 1 names the change. §2.1/§2.2 say "**36-identifier set**", the false "31 distinct" is gone (footer item 2). | RESOLVED |
| D-1b (V2 row 27) — P3-C §133 still carried a standalone "W5 — aarch64 ASM kernels" section divergent from the V2 SPEC | Cohort-internal staleness | P3-C is re-authored `Cycle: V3`. §1.4's candidate→wave map is the unified W1-W5 / W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d table; §1.4 prose states "The standalone 'aarch64 ASM kernels' wave the V1 manifest carried as W5 **is dissolved**: the EOR3 ladder is W4c, the CSSC CTZ is W4d, and the structural-bitmap chain folds into W3 … W5 is the docs-only close wave." A new §2a per-sub-wave gate table replaces the old §2 "W4 PAIRED" / "W5 ASM" rows. §0 footer item 3 names it. | RESOLVED |
| D-2 (V2 row 17) — W4a `G-W4a-STRING-BLOCK` clause 1 deferred the literal Mbps to "the W4a plan binds the floors" | Threshold-by-formula NEAR-FAIL | SPEC §7.1 clause 1 now names live baselines — `unicode_mixed` "today 6803, `RESULTS.md:33`" and `gsoc-2018` "today 22184, `:24`" — and the formula `ceil(sonic_strict / 1.10)`, but **still reads "the W4a plan binds the per-row Mbps floors"**: the absolute Mbps number is one rule-application away. Unchanged from V2. P3-C §2a W4a row carries the same phrasing. | NEAR-FAIL persists (§2 row 17) |

Two of the three V2 residues — the cohort-staleness pair D-1a/D-1b that
*caused the V2 88.9%* — are cleanly RESOLVED by the comprehensive
re-author. D-2 (the W4a threshold-by-formula) persists as one NEAR-FAIL:
the V3-CONSOLIDATED plan scoped arithmetic corrections N1-N5 and W4b
sub-division but did not name the W4a literal-floor computation, so the
formula-not-number gate survives. It is measurable-as-written (the
formula + the live `RESULTS.md:33`/`:24` baselines fully determine the
two floors) — a NEAR-FAIL, not a FAIL.

## §2 — V3 dispositions

| # | Artefact / claim | Sub-Q | Evidence checked | Disposition |
|--:|---|---|---|---|
| 1 | No `[INTEGRATE P3-x]` marker survives the V3 cohort | Q1 | `grep -rn INTEGRATE` over all seven artefacts: two hits only, both inside the P3-F `§0 footer — V2 fold` lines reading "all [INTEGRATE] markers resolved" — a historical resolution note, not a live marker. Zero live `[INTEGRATE P3-x]` markers. | PASS |
| 2 | All seven artefacts stamped `Cycle: V3` | Q6 | `grep "Cycle: V" *.md`: all seven read "Pass: S-P3 Synthesis-Plan. Cycle: V3." on line 3. The V2-CONSOLIDATED "bump all P3-A..E to Cycle: V3" instruction is executed. | PASS |
| 3 | All seven carry an honest `§0 V3 fold footer` naming what changed | Q6 | Every artefact carries a `§0 V3 fold footer` (P3-A line 798, P3-B 289, P3-C 476, P3-D 405, P3-E 675, F-spec 23, F-dispatch 19). Each footer enumerates concrete changes — P3-C names five, P3-D four, F-spec three. P3-A/P3-B retain the prior `§0 — V2 fold` line above the V3 footer (history preserved). Honest, specific, non-aspirational. | PASS |
| 4 | P3-D §3.2 ruling-prose is past-tense — the SPEC carries the enum | Q2 | P3-D §3.2: "The SK-V9 outcome enum **is** the 10-identifier W0-admissible set … The V3 SPEC §0.x **carries** the 10-outcome enum verbatim … The V1 SPEC §0.3 7-identifier subset **was** a SPEC-text defect … The V3 SPEC **corrected** it; no code change was ever required." All past-tense / present-state — no "should". The SPEC §0.x is the live carrier (verified row 5). | PASS |
| 5 | SPEC §0.x outcome enum = 10-identifier W0-admissible set | Q2 | SPEC §0.x reproduces `A C G I J K L M N-direct S` with a per-identifier gloss and the rationale "A narrower enum would make `gate-json` reject a row the code itself produces." Verified live: `validate_w0_outcome` (`report.rs:981-984`) `matches!` exactly `"A"\|"C"\|"G"\|"I"\|"J"\|"K"\|"L"\|"M"\|"N-direct"\|"S"`. The SPEC enum matches the code. | PASS |
| 6 | SPEC §0.y schema = 36-identifier set; P3-D §2.2 the canonical table | Q2 | SPEC §0.y lists 36 names in a code block, cites P3-D §2.2. P3-D §2.2 is the 36-row field table. P3-D §1 N5 correction: `SkV8ComparatorEvidence` is 7 fields, "the 36-row count holds against the 7-field struct" (`value_mbps`/`source_artifact` fold into the comparator-string column). Verified live: `SkV8ComparatorEvidence` (`report.rs:33-40`) has exactly 7 fields — `comparator_id`, `comparator_plane`, `comparator_strictness`, `comparator_freshness`, `sidecar_freshness`, `value_mbps`, `source_artifact`. The N5 correction is accurate. | PASS |
| 7 | SPEC §0.1 clause 6 — the four uncloseable rows "either admit by the W4b conditional same-wave-pairing rule or are recorded NEAR-FAIL / FAIL … W4 may close with zero strict unicode-row admissions; that is a measured outcome, not a paper-close" | Q1, Q3 | Verbatim P2-E §6.4 honest verdict encoded as the close condition itself, not an aspirational "all four close". Survives the W4b three-way split intact. | PASS |
| 8 | W4b codec gate carries P2-E's honest verdict — zero of four rows admit on the codec alone | Q4 | SPEC §7.2 "Pairing." paragraph: "P2-E §6.4 is the binding honest verdict: **zero of the four uncloseable rows admit on the codec alone.** The codec alone moves no row to GO; a standalone codec wave would paper-close." The codec gate does NOT claim "closes 4 rows". | PASS |
| 9 | SPEC §7.2.2 `G-W4b-2-CODEC` — per-row NEAR-FAIL/FAIL projections | Q4 | The four projections are encoded as the gate clauses: `unicode_escapes` ≥16319 "Projected 15423 — **NEAR-FAIL 94.5%** on the codec alone"; `y_string_unicode` ≥8270 "Projected 7837 — **NEAR-FAIL 94.8%**"; `unicode_mixed` ≥12338 "the codec alone projects 7864 — **FAIL 63.7%** … admits iff the *combined* W4a + W4b-2 measured Mbps clears"; `gsoc-2018` ≥21963 no-regression. Honest projection, conditional pairing, not paper-close. | PASS |
| 10 | SPEC §7.2.2 — the honest posture verbatim | Q4 | "**The honest posture (P2-E §6.4, carried verbatim).** W4b-2 may close with **zero strict unicode-row admissions** … the row stays `S / NO-GO`, the measured codec contribution is recorded in REDRESS … That is an honest measured outcome, **not a paper-close**." Reverted-wholesale-only-on-checkasm-or-W10b clause present. | PASS |
| 11 | SPEC §5 W2 proof-only — "`skinny/RESULTS.md` is byte-identical — the proof moved zero rows" | Q5 | SPEC §5 objective: "Proof-only depth: **zero `RESULTS.md` row movement**, zero generated output, zero production consumer." Exit-gate clause 5: "`skinny/RESULTS.md` is byte-identical — the proof moved zero rows." P3-C §3.2 + P3-D §2.3 W2 row both state zero row movement. Honest. | PASS |
| 12 | W3 cap decision — ≤110-min CHALLENGE-gated extension is honest, not a paper-close of the over-cap problem | Q3 | SPEC §2.2 "W3 redress cap": "W3 at ~265 hand + ~120 regen + the P2-D §5 SIMD chain … is ~465-635 hand-equivalent + ~120 regen … It plausibly overruns the 75-min redress sub-cap … W3 is **not** sub-waved: the union substrate and the SIMD producer form one cascade — splitting them orphans the class column from its only producer … the SPEC §1 same-wave-consumer non-negotiable forbids that. W3 instead carries an honest **CHALLENGE-gated redress-extension note** … the W3 CHALLENGE may grant a single redress extension to ≤110 min, recorded in the CHALLENGE disposition, with the orchestrator surfacing the extension decision to the user." The over-cap is named, the no-sub-divide reason is given (orphan-kernel violation), the extension is CHALLENGE-gated and user-surfaced — not papered over. | PASS |
| 13 | W3 MEDIUM→HIGH risk escalation recorded | Q3, Q7 | SPEC §2.2: "W3's risk is **HIGH** — P2-A C3 §2.2 warned the folded P2-D §5 chain raises the wave's aggregate risk from MEDIUM to HIGH; that escalation is recorded here, in §2, and in §6." SPEC §2 manifest W3 row: Risk "HIGH (CHALLENGE-gated redress extension)". SPEC §6 W3 intro: "risk **HIGH** … P2-A C3 §2.2 records that folding the P2-D §5 structural-bitmap chain in whole raises the wave's aggregate risk from MEDIUM to HIGH." P3-C §2 W3 gate gains a "Risk + redress cap" row recording the same. Recorded in four places consistently. | PASS |
| 14 | SPEC §7.2 W4b — "PAIRED with W4a … strictly adjacent, never separable" survives the three-way split | Q4 | SPEC §7.2 "Pairing." + §7.2.2: "**W4b-2 is PAIRED with W4a — strictly adjacent, never separable** (P2-E §6.4): neither the codec nor the string-block widening closes the four uncloseable rows alone, so W4b-2 dispatches only with W4a landed." W4b-1 and W4b-3 carry no row gate. The pairing migrated to the row-moving sub-wave precisely. | PASS |
| 15 | SPEC §2.2 — the W4b three-way split is honestly LOC-driven, not gold-plating | Q3 | "**W4b is itself three sub-waves.** The `escape_codec_hex_unit` codec (C4) is ~1,045 net LOC across the eleven P2-E §7.4 slices … One 75-min redress cannot land it; sub-waving the *bracket* W4a-d did not sub-divide the *codec*." The split is bound to the redress-cap arithmetic, cut along named P2-E §7.4 slice seams. Honest scope decision answering the V2-CONSOLIDATED CH4 ~1,045-LOC defect. | PASS |
| 16 | SPEC §7.2.1 W4b-1 — the parity-foundation sub-wave honestly carries no row gate | Q4, Q5 | §7.2.1: "W4b-1 ships no NEON body and moves no row; it is the scalar-reference + checkasm precondition the SK-V5 orphan-kernel discipline mandates before any kernel wires." Exit gate `G-W4b-1-CODEC-HARNESS` is compile + parity, not Mbps; "W4b-1 moves no row; it has no W10b maintain obligation beyond compiling clean." No false row claim. | PASS |
| 17 | SPEC §7.1 `G-W4a-STRING-BLOCK` clause 1 — "the W4a plan binds the per-row Mbps floors against `ceil(sonic_strict / 1.10)`" | Q4 | The clause now names the target rows and the live baselines (`unicode_mixed` today 6803 `RESULTS.md:33`, `gsoc-2018` today 22184 `:24`) and the floor formula, but still defers the absolute Mbps to "the W4a plan binds the per-row Mbps floors". Measurable-as-written (formula + live baselines fully determine the number); the literal floor is one rule-application away. Unchanged from V2; P3-C §2a W4a row carries the same phrasing. | NEAR-FAIL |
| 18 | Arithmetic corrections N1-N5 landed honestly with citations | Q7, Q8 | SPEC §0 footer item 3 + §6 clause 1-2: `update_center` floor `14369 → 14370` (`ceil(15806/1.10)`); `gsoc-2018` no-regression base `21646 → 22184` live (`RESULTS.md:24`), floor `21430 → 21963`; W10b floored uniformly — `citm_catalog` `28631 → 28630`, `numbers` `17597 → 17596`. P3-D §1 N5: `SkV8ComparatorEvidence` `6 → 7` (verified live, row 6). P3-C §0 footer + P3-A §0 footer carry the same arithmetic. The corrections are stated with the computation shown and a `RESULTS.md` line cite — not asserted. | PASS |
| 19 | SPEC §6 W3 clause 3 — "the same-wave consumer is wired same-commit … CH5 falsifier: `rg 'consume_structural' … returns zero outside the deletion-commit diff" | Q7 | The "wired" verb is bound to an `rg` observable + the clause-1 ≤5% self-time gate. No bare "wired". | PASS |
| 20 | SPEC §8 W5 — "carries zero source LOC and no CHALLENGE … reconciliation wave"; `G-W5-CLOSE` measurable | Q3, Q5 | §8: "W5 is a reconciliation wave only … It carries zero source LOC and no CHALLENGE … records, per uncloseable row, the honest W4b disposition (admit / NEAR-FAIL / FAIL with the measured contribution)." `G-W5-CLOSE`: "the five documents agree, every W1-W4 wave and sub-wave has an admit or a measured reject in REDRESS, and the §0.1 close condition is satisfied." Honestly a docs reconciliation; checkable. | PASS |
| 21 | SPEC §N `G-ALPHA-SK-V9` — every sub-wave gate enumerated; clause 5 carries the honest close-condition | Q5, Q8 | §N clause 4 enumerates all ten gates `G-W1..G-W5` including `G-W4b-1-CODEC-HARNESS`/`G-W4b-2-CODEC`/`G-W4b-3-CODEC-BINDINGS`; clause 5 "the §0.1 close condition holds in full — including clause 6: W4 may close with zero strict unicode-row admissions if every uncloseable row records NEAR-FAIL / FAIL honestly." Each clause is a recorded-state check. | PASS |
| 22 | P3-C §2a — new per-sub-wave gate table replaces the V2 §2 "W4 PAIRED" / "W5 ASM" rows | Q4 | P3-C §2a covers W4a, W4b-1, W4b-2, W4b-3, W4c, W4d, each with the four mandatory gate parts. §0 footer item 3: "A new §2a per-sub-wave gate table … replacing the old §2 'W4 — PAIRED' and 'W5 — ASM kernels' gate rows; the un-sourced `github_events`/`random` W5 exit rows are removed." The V2 row-27 stale W5 section is gone; the removal of un-sourced rows is itself an anti-paper-close fix. | PASS |
| 23 | P3-E §1 — lettered→numeric mapping; the §0 footer states the pre-block content is unchanged | Q3, Q6 | P3-E §1 carries the W-AC→W1 / W-RG→W2 / W-UE→W3 / W-UC→W4a+W4b-1/2/3 / W-AS→W4c+W4d mapping table; §2/§3 headers carry numeric ids with lettered shorthand parenthesised. §0 footer: "The per-wave pre-block content is unchanged — the material differentials and hard pre-blocks bind identically; only the wave labelling is reconciled." Honest scoping of a label-only re-bind — no false "re-researched" claim. | PASS |
| 24 | P3-D §2.3 per-wave population table re-bound to actual V3 waves | Q6 | §2.3 table rows are W0 / Interlock / W1 / W2 / W3 / W4a / W4b-1 / W4b-2 / W4b-3 / W4c / W4d / W5; §2.3 preamble: "the V3 SPEC §2 behaviour waves … not the superseded SPEC-placeholder slot numbering." §0 footer item 4 names the change. W4b-1/W4b-3 rows honestly state "Emits **no** RESULTS row". | PASS |
| 25 | P3-A §3 dependency graph re-bound to the W4 sub-wave structure | Q6 | P3-A §0 V3 footer: the DEPTH-2 graph block re-bound — "C4 codec → W4b-1/W4b-2/W4b-3, C5 → W4a, C6 → W4c, C7 → W4d"; the cascade-lock stated in its disambiguated reading "'W3 precedes the W4 sub-waves', not 'one monolithic wave'"; "the stale 'one cascade-locked behaviour wave' … prose is corrected." The V2 P3-A leg is fully reconciled. | PASS |
| 26 | DISPATCH draft — wave manifest + G-Gate enumerate the W4b three-way split | Q4, Q6 | DISPATCH §"Wave Manifest" has W4b-1/W4b-2/W4b-3 rows with SPEC §7.2.1/§7.2.2/§7.2.3 bindings; §"Falsifiability Gates" enumerates `G-W4b-1-CODEC-HARNESS`/`G-W4b-2-CODEC`/`G-W4b-3-CODEC-BINDINGS`; §0 V3 footer: "The G-Gate enumerates `G-W4b-1-CODEC-HARNESS` / `G-W4b-2-CODEC` / `G-W4b-3-CODEC-BINDINGS` in place of `G-W4b-CODEC`." Consistent with the SPEC. | PASS |
| 27 | DISPATCH §"Falsifiability Gates" — codec/W4b row carries the honest verdict | Q4 | "**The codec admits zero rows alone — only W4b-2 moves rows.** P2-E §6.4 is binding … **W4 may close with zero strict unicode-row admissions — that is an honest measured outcome, not a paper-close.**" The V2-flagged honest verdict survives in the dispatch contract. | PASS |
| 28 | DISPATCH §"Convergence" — bracket count honest | Q3, Q8 | "The bracket is W0 + W1-W3 + the six W4 sub-waves + W5 = 11 brackets, inside the ≤12 skinny-bracket ceiling." Arithmetic checks: W0,W1,W2,W3 (4) + W4a,W4b-1,W4b-2,W4b-3,W4c,W4d (6) + W5 (1) = 11. Honest count, no slot-stuffing. | PASS |
| 29 | No new "wired/verified/complete" without evidence | Q7 | SPEC §1 non-negotiable: "No wave closes on a future-phase promise. 'Wired' or 'integrated' without a bench-row threshold is a paper-close." Every "wired" in a gate clause is bound to a same-commit `rg`/`samply`/`cargo asm` observable (W3 §6#3 `rg`, W4a §7.1#3 `samply`, W4c §7.3#2 `cargo asm`, W4d §7.4#1 `cargo asm`). W4b-1's "the harness IS the same-wave consumer" is a compile-checked claim. No bare evidence-free verb. | PASS |

## §3 — Aggregate verdict

29 claims dispositioned: **28 PASS, 1 NEAR-FAIL, 0 FAIL.**

ACCEPT rate (PASS / total) = 28/29 = **96.6%** — above the §3Z 95%
floor.

Both V2 cohort-staleness residues — the D-1a/D-1b pair that *was* the
V2 88.9% shortfall — are cleanly RESOLVED by the comprehensive
re-author (§1, rows 4, 22, 24, 25):

- P3-D §3.2 ruling-prose is honestly past-tense — "the V3 SPEC §0.x
  carries the 10-outcome enum", "the V3 SPEC corrected it", no "should"
  (row 4); verified against `validate_w0_outcome` (row 5).
- P3-C's standalone "W5 ASM kernels" section is dissolved; §1.4 carries
  the unified manifest and a new §2a per-sub-wave gate table; the
  un-sourced `github_events`/`random` W5 exit rows are removed (row 22).
- P3-E is re-bound to the numeric manifest with an honest "pre-block
  content unchanged" footer (row 23); P3-A's dependency graph and P3-D's
  per-wave table are re-bound (rows 24-25).
- All seven artefacts are stamped `Cycle: V3` with honest, specific §0
  fold footers (rows 2-3).

On the eight CH6 prompt sub-questions:

1. **No `[INTEGRATE]` markers survive** (row 1) — the only two hits are
   inside historical V2-fold resolution notes.
2. **P3-D ruling-prose is honestly past-tense** (row 4) — the SPEC
   carries the enum/schema, not "should".
3. **The W3 ≤110-min cap decision is honest** (rows 12-13) — the over-cap
   is named, the no-sub-divide reason is the orphan-kernel non-negotiable,
   the extension is CHALLENGE-gated and user-surfaced; the MEDIUM→HIGH
   escalation is recorded in four places (§2.2, §2 manifest, §6, P3-C §2).
4. **The W4b codec gate carries P2-E's honest verdict** (rows 8-10, 14,
   27) — zero of four rows admit on the codec alone, conditional W4a
   pairing, NEAR-FAIL/FAIL projections encoded as the gate, "not a
   paper-close" verbatim.
5. **W2 proof-only honestly states zero row movement** (row 11) — SPEC
   §5, P3-C §3.2, P3-D §2.3 all carry it; W4b-1/W4b-3 likewise honestly
   "emit no RESULTS row".
6. **All seven artefacts stamped V3 with honest §0 fold footers**
   (rows 2-3, 22-26).
7. **No new "wired/verified/complete" without evidence** (rows 19, 29) —
   every "wired" gate clause is bound to an `rg`/`samply`/`cargo asm`
   observable.
8. **Spot-check** (≥12 done): rows 1, 5, 6, 13, 18, 26, 28 plus the
   live-tree checks below. `validate_w0_outcome` admits exactly the ten
   (`report.rs:981-984`, verified); `SkV8ComparatorEvidence` has 7
   fields (`report.rs:33-40`, verified — the N5 `6→7` correction is
   accurate); `DIRECT_PROJECTION_SONIC_SLACK = 1.10` (`gate.rs:56`,
   verified); `class_table` at `bbnf-simd/src/lib.rs:41` (verified, a
   generated `[u8;256]` builder); all seven artefacts `Cycle: V3`
   (verified); the bracket count 4+6+1 = 11 ≤ 12 (verified, row 28); the
   W3 escalation recorded in §2.2/§2/§6/P3-C (verified, row 13).

The single NEAR-FAIL (row 17) is the V2 D-2 carryover — SPEC §7.1
`G-W4a-STRING-BLOCK` clause 1 names the target rows, the live baselines
(`RESULTS.md:33`/`:24`), and the `ceil(sonic_strict / 1.10)` formula,
but still defers the absolute Mbps to "the W4a plan binds the per-row
Mbps floors". It is measurable-as-written and not a SPEC paper-close —
the formula plus the cited live baselines fully determine the floors —
but it is one indirection short of a literal number. Not blocking; not
a FAIL.

**S-P3 V3 converges on the CH6 lens at 96.6%** — above the V1 66.7%,
the V2 88.9%, and the §3Z 95% floor. The V2 shortfall was entirely
cohort-version staleness; the V3 comprehensive single-agent fold cleared
it. No FAIL-class paper-close survives.

## §4 — Any new paper-close

The V3 integration introduced **no new paper-close.** The SPEC,
DISPATCH, and the five siblings are internally coherent, the wave map
is uniform across all seven files, and every gate is evidence-bound.

**D-2 residual (row 17) — the W4a threshold-by-formula.** SPEC §7.1
`G-W4a-STRING-BLOCK` clause 1 still defers the literal per-row Mbps
number to "the W4a plan binds the per-row Mbps floors", though it now
names the two target rows, their live `RESULTS.md` baselines, and the
`ceil(sonic_strict / 1.10)` formula. This is the *inverse* of a
paper-close — it under-states a determinable number rather than
over-claiming an undetermined one — and it is measurable-as-written
(formula + live baselines fully determine the floors). The
V3-CONSOLIDATED plan scoped arithmetic corrections N1-N5 (which landed,
row 18) and the W4b sub-division but did not name the W4a literal-floor
computation, so the gate kept the formula form. A V4 one-liner stating
the two computed floors (`unicode_mixed` and `gsoc-2018` against their
live sonic-strict rows) would make the gate literal. Not blocking,
NEAR-FAIL not FAIL.

Verification that the largest V3 structural change — the W4b three-way
sub-division — introduced no paper-close:

1. **W4b-1/W4b-3 honestly carry no row gate** (rows 16, 22, 24). The
   SPEC, P3-C §2a, and P3-D §2.3 all state W4b-1 (parity foundation) and
   W4b-3 (grammar-neutrality breadth) "move no row" and "emit no RESULTS
   row" — they are not dressed up with aspirational thresholds. The
   row-moving claim is confined to W4b-2.
2. **The split is honestly LOC-driven** (row 15). SPEC §2.2 binds it to
   the ~1,045-net-LOC codec exceeding the 75-min redress, cut along
   named P2-E §7.4 slice seams — answering the V2-CONSOLIDATED CH4 real
   substantive defect, not gold-plating.
3. **The W4a+W4b-2 pairing survives the split** (row 14) — the
   row-moving codec sub-wave W4b-2 is "PAIRED with W4a — strictly
   adjacent, never separable"; the conditional `unicode_mixed`-on-combined-
   Mbps rule is intact.
4. **The W3 cap decision is honest, not a paper-close of the over-cap**
   (rows 12-13). W3 is *not* sub-waved — the SPEC names the reason (the
   class column and its sole SIMD producer are one cascade; splitting
   them orphans the column, violating the §1 same-wave-consumer
   non-negotiable) and instead records an honest CHALLENGE-gated ≤110-min
   extension with a user-surfaced extension decision and a recorded
   MEDIUM→HIGH escalation. The over-cap problem is named and routed, not
   absorbed silently.

No FAIL-class paper-close survives V3. The honest evidence S-P3 must
carry forward — P2-E's "zero of four rows admit on the codec alone", the
conditional W4a+W4b-2 same-wave-pairing rule, the proof-only W2 zero-row
statement, the measured-row exit gates, the W3 MEDIUM→HIGH escalation,
the legitimate SK-V10 deferrals — is intact and correctly folded into
the SPEC + DISPATCH + the five re-authored siblings. V3 cleared the V2
cohort-staleness residue; the lone D-2 NEAR-FAIL is a determinable-by-
formula threshold, resolvable in a sub-five-minute V4 touch-up.
