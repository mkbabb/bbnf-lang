# CH3 REGRESSION — SK-V9 S-P3 Synthesis-Plan, Cycle V3 (verify)

Lens: CH3 REGRESSION (`ORCHESTRATOR.md` §3W). Pass: S-P3 Synthesis-Plan.
Cycle: V3 (verify the V2→V3 comprehensive integration fold).
Date: 2026-05-18.
Target: the seven S-P3 P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` after the V3 fold — all
seven now stamped `Cycle: V3` and carrying a `§0 V3 fold footer`. The
SPEC draft `skv9-p3-F-spec-draft.md` and the dispatch draft
`skv9-p3-F-dispatch-draft.md` are the primary scrutiny surface;
`skv9-p3-E-preblocked-ledger.md` is the binding pre-block ledger the
SPEC's per-wave "Pre-blocked routes" sections must match.
Authority cross-checked: `skinny/REDRESS.md` entries 82 (`:2285-2316`),
83 (`:2318-2356`), 88 (`:2508-2540`), 89 (`:2542-2585`), 92
(`:2661-2690`); `restart/locks/LOCKS.md` Lock 1 (`:34`);
`HARDENING-S-P3-V2-CONSOLIDATED.md`; `V2/CH3.md`.

CH3 V2 verdict was ACCEPT-WITH-NOTE at 93.3% — 28 HOLD / 2 REVISE /
0 DEFECT, one disposition short of the §3Z 95% bar. The two REVISEs
(#2 P3-E V1 lettered scheme un-refolded; #27 P3-A §3 line 696-697 stale
"one cascade-locked behaviour wave" prose) were documentation-cohesion
touch-ups on the two artefacts the V2 five-file fold left un-refolded.
CH3 V3 verifies the V3 comprehensive integration fold closed both
REVISEs without reopening any REDRESS route, and that the W4b
three-way sub-division the V3 fold introduced (to resolve the CH4
~1,045-net-LOC codec-cap defect) carries every pre-block intact.

Disposition codes: **HOLD** — the V3 artefact claim is correct and the
V2 residual is resolved; **REVISE** — sound but under-specified, a V4
touch-up tightens it; **DEFECT** — a concrete error a further fold must
repair.

---

## §1 — V2-residual resolution

The V2 CONSOLIDATED prescription dispatched ONE comprehensive V3-fold
integration agent (the V2 mistake was splitting the fold F-MAIN/F-AUX).
The CONSOLIDATED named four binding items for the CH3 surface; each is
verified against the V3 artefacts.

**V2 item 1 — re-author P3-C, P3-D, P3-E to the unified manifest;
bump all to `Cycle: V3` (closes CH3-V2 REVISE #2).** RESOLVED. All
seven artefacts now read `Cycle: V3` and carry a `§0 V3 fold footer`.
P3-E §1 (`:54-76`) gains the binding lettered→numeric mapping table —
W-AC→W1, W-RG→W2, W-UE→W3, W-UC→{W4a, W4b-1/W4b-2/W4b-3}, W-AS→{W4c,
W4d} — and every §2/§3 sub-section header now carries the numeric wave
id with the lettered shorthand parenthesised (`§2.2 — W1 (W-AC)`,
`§3.4 — W4b-1/W4b-2/W4b-3 (W-UC) vs REDRESS 82`, etc.). A SPEC/dispatch
"P3-E §3.x" citation now lands on a numeric-labelled section. The V2
REVISE #2 — P3-E delegating numbering and leaving a reader to
reconstruct the lettered scheme — is closed: the mapping is fixed in
the ledger itself.

**V2 item 2 — sub-divide W4b along the P2-E §7.4 slice seams (closes
the one new substantive CH4 defect).** RESOLVED on the CH3 surface.
SPEC §2.2 cuts W4b into W4b-1 (scalar reference + checkasm harness,
§7.2.1), W4b-2 (fixed-width bodies + JSON `unescape_four_unicode_escapes`
consumer — the row-moving sub-wave PAIRED with W4a, §7.2.2), and W4b-3
(variable-width const-generic bindings + codegen, §7.2.3). The §2
manifest, §2.2 cascade prose, §7.2 intro, §7.2.1/§7.2.2/§7.2.3
sub-sections, and §N G-Gate all carry the three-way split; the dispatch
draft Wave Manifest and Required-Reading map carry the byte-identical
shape. CH3-relevant: the W4b pairing (the REDRESS-82-orphan guard) is
re-pinned — see §2 #4.

**V2 item 3 — record the W3 MEDIUM→HIGH risk escalation +
CHALLENGE-gated redress extension.** RESOLVED. SPEC §2 manifest W3 row
carries `Risk = HIGH (CHALLENGE-gated redress extension)`; §2.2's
"W3 redress cap" paragraph and §6's preamble both record the P2-A C3
§2.2 MEDIUM→HIGH escalation and the ≤110-min CHALLENGE-gated extension.
CH3-relevant: the W3 cap decision does not reopen a REDRESS route — see
§2 #19.

**V2 item 4 — arithmetic corrections.** RESOLVED on the CH3 surface.
`update_center` W3 floor `14369 → 14370` (`ceil(15806/1.10)`);
`gsoc-2018` W4b-2 no-regression base `21646 → 22184` live
(`RESULTS.md:24`), floor `21430 → 21963`; the W10b `today × 0.98` leg
floored uniformly — `citm_catalog` `28631 → 28630`, `numbers`
`17597 → 17596`. The SPEC §6, dispatch §"Falsifiability Gates", P3-C
§2/§2a, and P3-A §3 footer all carry the corrected figures. CH3-adjacent:
a stale maintain floor mis-gates the regression check — see §2 #15.

Both V2 CH3 REVISEs are closed. The V2 fold was a five-file integration
that left P3-C/D/E lagging the SPEC; the V3 fold re-authored all five
and the manifest is now consistent across every artefact (verified
§2 #1). The V3 fold introduced no new un-refolded sibling.

---

## §2 — V3 dispositions

| # | Scope | CH3 V3 finding | Disp. |
|---|---|---|---|
| 1 | Unified manifest across all seven artefacts | SPEC §2 manifest carries W0/W1/W2/W3/W4a/W4b-1/W4b-2/W4b-3/W4c/W4d/W5. The dispatch §"Wave Manifest" reproduces it identically (W4b-1/W4b-2/W4b-3 rows, §7.2.1/§7.2.2/§7.2.3 section bindings). P3-B §2 manifest + §3 W4 dependency prose carry the sub-waved structure (`§0 V3 fold footer` item 1). P3-C §1.4 candidate→wave map carries W4a/W4b-1/W4b-2/W4b-3/W4c/W4d. P3-D §2.3 per-wave population table carries the V3 behaviour waves. P3-E §1 mapping table carries W-UC→{W4a,W4b-1/2/3}. P3-A §3 reading-the-graph + DEPTH-2 block carry C4→W4b-1/2/3. The V2 defect — SPEC correct, siblings lag — is closed; all seven agree on the W1-W5 / W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d spine. | HOLD |
| 2 | P3-C re-authored — no standalone W5-ASM section | P3-C `§0 V3 fold footer` item (1)/(3) records the standalone "W5 aarch64 ASM kernels" wave is **dissolved**: EOR3→W4c, CSSC CTZ→W4d, the structural-bitmap chain folds into W3. P3-C §1.4 candidate→wave map row for W5 reads "reconciliation / none — docs only / no — close wave"; §2a is a new per-sub-wave gate table covering W4a/W4b-1/W4b-2/W4b-3/W4c/W4d; the old §2 "W4 — PAIRED" and "W5 — ASM kernels" gate rows and the un-sourced `github_events`/`random` W5 exit rows are removed. No standalone W5-ASM section survives. The V2 CONSOLIDATED defect "P3-C §1.4/§2 still gate the old two-wave manifest" is closed. | HOLD |
| 3 | P3-E lettered→numeric mapping folded | P3-E §1 (`:54-76`) carries the explicit mapping table and the numeric scheme is used throughout §2/§3; the `§0 V3 fold footer` records "the per-wave pre-block content is unchanged — only the wave labelling is reconciled." Every SPEC §4-§7 "Pre-blocked routes" P3-E citation (`§2.2/§3.1`, `§2.3/§3.2`, `§2.4/§3.2`, `§2.5/§3.4`, `§2.5/§3.5`, `§2.6/§3.3`, `§2.6/§3.6`) resolves against an extant numeric-labelled P3-E section. The V2 REVISE #2 is closed — no reader reconstruction needed. | HOLD |
| 4 | W4b codec is paired — W4a + W4b-2, no REDRESS-82 orphan | The V3 W4b three-way split could have orphaned the pairing; it does not. SPEC §2.2 closing: "The W4a + W4b pairing is preserved exactly: W4a pairs with **W4b-2**." §7.2 intro + §7.2.2 header: "W4b-2 is PAIRED with W4a — strictly adjacent, never separable (P2-E §6.4)." §7.2.2 entry gate: "W4a closed (the paired scanner widening is live)." `G-W4b-2-CODEC` clause 3: `unicode_mixed` "Admits iff the *combined* W4a string-block + W4b-2 codec measured Mbps clears 12338. If W4a did not land, `unicode_mixed` stays NO-GO and W4b-2 admits codec-contribution-only — never claimed closed by the codec." Dispatch Wave Manifest: W4b-2 "PAIRED with W4a — the row-moving sub-wave." The codec is paired (W4a + W4b-2); the REDRESS-82 codec-only-orphan shape is closed. | HOLD |
| 5 | W4b-1 / W4b-3 carry no row gate — no paper-close, no orphan | SPEC §7.2.1 `G-W4b-1-CODEC-HARNESS` is a compile+parity gate ("W4b-1 moves no row"); §7.2.3 `G-W4b-3-CODEC-BINDINGS` is compile+parity ("W4b-3 moves no row"). Neither is a standalone row-moving wave, so neither can be a codec-only-orphan in the REDRESS-82 sense — W4b-1 is the parity foundation (the orphan-kernel-discipline precondition), W4b-3 the grammar-neutrality breadth. The dispatch §"Falsifiability Gates" states verbatim "W4b-1 … and W4b-3 … carry no row gate." The codec admits as a checkasm-verified primitive at W4b-1; rows admit only at W4b-2. The three-way split moves no pre-block. | HOLD |
| 6 | Differential #1 — W3 union vs REDRESS 92 + 50-72 | SPEC §6 "Pre-blocked routes" reproduces P3-E §3.2 / §2.4: W3 implements the routed precursor REDRESS 92 named (the alternate event-model), not the rejected storage-only swap; the class column is co-emitted at the existing `emit_plain_offset` site, not a parser-written aux side table; the SIMD index consumed by move (Lock 1 cardinality = one); the REDRESS 92 blanket pre-block (no `BackendShape`/BIR/directive, no public substrate API, no parser-owned cursor/facts, no `tape_vs_tape`, no `UnionTape`, no Tier B work). Verified verbatim against REDRESS 92 (`:2673-2676`, `:2687-2690`). §6 entry gate binds W3 to "W2 closed with `G-W2-RETAINED-PROOF` PASS." Differential #1 survives the V3 fold unchanged. | HOLD |
| 7 | Differential #2 — W4b codec vs REDRESS 82 | SPEC §7.2.2 "Pre-blocked routes" reproduces the P3-E §3.4 five-axis differential: not a parser-owned per-quartet classifier (the 4-quartet batched path is the union-substrate path; single-quartet fires only on pre-filter reject); same-wave consumer is the already-wired x4 JSON path at `lib.rs:402`; `escape_codec_hex_unit` is a const-generic primitive with five bindings; post-V3 PMU evidence; `parse_only`-only gate. P3-E §3.4 header is `W4b-1/W4b-2/W4b-3 (W-UC) vs REDRESS 82` — the differential is now stated against all three codec sub-waves; SPEC §7.2.3 also carries it for the variable-width binding. The dispatch §"Pre-Blocked Routes" five-differential list reads "W4b (codec, W4b-1/W4b-2/W4b-3) vs REDRESS 82 … The pre-block is cleared at the row-moving sub-wave W4b-2." Verified against REDRESS 82 (`:2308-2316`). Differential #2 survives the W4b split — bound to W4b-2's same-row gate. | HOLD |
| 8 | Differential #3 — W4c EOR3 vs REDRESS 88 | SPEC §7.3 "Pre-blocked routes" reproduces P3-E §3.3's three-axis differential: different intrinsic (3-input bitwise XOR, no carryless multiply); different latency (PMULL.1Q 4-cycle vs EOR3 1-cycle — the REDRESS-88 retire-latency failure mode structurally inapplicable); different primitive shape (a vector fold of the scalar ladder REDRESS 88 *kept*). §7.3 Owner-paths row keeps the scalar shift-XOR ladder the unconditional fallback; the EOR3 path is `FEAT_SHA3`-gated. Verified against REDRESS 88 (`:2538-2540`) — "PMULL as the default hot `bitmap_prefix_xor_64` body is not admissible." Differential #3 survives. | HOLD |
| 9 | Differential #4 — W4a string-block vs REDRESS 83 | SPEC §7.1 "Pre-blocked routes" reproduces P3-E §3.5: different call site (the full `match_string_at_quote_trusted_utf8`, not the tiny 16-byte-cap probe); a 32-byte successor primitive replacing the 16-byte primitive at the producer site, not a JSON-specific wrapper; same-wave consumer is the existing `match_string_at_quote_trusted_utf8`; the gate measures the combined producer + consumer path, not the block-scan microbench. Verified against REDRESS 83 (`:2347-2356`) — failure mode "the AArch64 `string_block` movemask shape is too expensive for the already-tiny generated retained quote-pair probe." Differential #4 survives. | HOLD |
| 10 | Differential #5 — W4d CTZ vs REDRESS 89 | SPEC §7.4 "Pre-blocked routes" reproduces P3-E §3.6: different call site (the string-mask first-set extract, not `bulk_emit_positions_64`); LOSS rows under guard, not the WIN-block numeric rows; same-wave consumer is the union-substrate string-mask consumer (W3 + W4a scope). `G-W4d-CTZ` clause 2 grades the W10b six-row maintain gate "the **binding hard blocking precondition**" — matching P3-E §3.6's HIGH-risk grading and SPEC §2 W4d `Risk = HIGH`. Verified against REDRESS 89 (`:2573-2585`). Differential #5 survives, correctly graded HIGH. | HOLD |
| 11 | All five material differentials survive the V3 fold | Aggregating #6-#10: the union vs 92/50-72, codec vs 82, EOR3 vs 88, string-block vs 83, CTZ vs 89 differentials each survive verbatim against `skinny/REDRESS.md`. The dispatch §"Pre-Blocked Routes" enumerates exactly these five "material differentials each REDRESS-adjacent wave must clear." The W4b three-way split did not lose the codec differential — it re-pinned it to W4b-2's row gate (#7). The five-differential count is intact. | HOLD |
| 12 | W10b six-row block — fidelity to REDRESS 89 | SPEC §6 clause 2 names the six rows `canada` ≥ 15866, `citm_catalog` ≥ 28630, `instruments` ≥ 15865, `marine_ik` ≥ 11831, `mesh` ≥ 12186, `numbers` ≥ 17596. The six corpora are exactly the REDRESS 89 (`:2581-2585`) regression list. P3-E §5 reproduces the per-row regression percentages verbatim from REDRESS 89. The V3 arithmetic correction lowered `citm_catalog` 28631→28630 and `numbers` 17597→17596 by adopting one uniform `floor(today × 0.98)` convention — the corpora set is unchanged; the block is intact and the floors are now internally consistent. | HOLD |
| 13 | W10b block bound to every parse-loop-touching wave/sub-wave | SPEC §6 clause 2 binds it to W3 (binding, P2-A §4.2); §7.1 clause 4 to W4a ("binding — W4a is a string-loop edit"); §7.2.2 `G-W4b-2-CODEC` clause 7 to W4b-2 ("W4b-2 re-bodies the JSON unescape hot path"); §7.3 `G-W4c-EOR3` clause 3 to W4c ("the **binding** gate"); §7.4 `G-W4d-CTZ` clause 2 to W4d ("the **binding hard blocking precondition**"). W4b-1 §7.2.1 and W4b-3 §7.2.3 each state explicitly "moves no row … no W10b maintain obligation beyond compiling clean (it ships no parse-loop edit)" — correct: neither touches the parse loop. The dispatch §"Falsifiability Gates" states the W10b block "is a binding maintain gate on W3 and on every W4 sub-wave that touches the parse loop or an aarch64 SIMD kernel." Every parse-loop / SIMD-kernel-touching wave/sub-wave carries it; W1, W2, W4b-1, W4b-3, W5 correctly do not. Coverage complete after the W4b split. | HOLD |
| 14 | W10b block bound to the §0.1 close condition | SPEC §0.1 clause 7: "The W10b six-row regression block holds its maintain floor at every wave **and sub-wave** that touches the parse loop or an aarch64 SIMD kernel." The "and sub-wave" phrasing is V3-correct — the W4b split made "sub-wave" load-bearing. A wave or sub-wave that regresses a WIN row cannot close the bracket. The W10b block is enforced at the close-condition level, not only per-wave. | HOLD |
| 15 | Arithmetic — uniform `floor(today × 0.98)` convention, stale `canada` parenthetical corrected | SPEC §6 clause 2: "The `today × 0.98` leg is **floored** uniformly across all six rows — the single rounding convention for the whole W10b block." P3-C `§0 V3 fold footer` item (5) records the false "canada sonic floor binds higher" parenthetical is corrected — sonic 12723 → `ceil/1.10` = 11567 binds **lower**, so `floor(today × 0.98)` = 15866 binds. CH3-relevant: a mixed rounding convention or a stale 15871/11567 mis-figure would mis-gate the regression check; the V3 correction tightens the W10b regression gate to one consistent floor. The dispatch §"Falsifiability Gates" carries the same correction verbatim. | HOLD |
| 16 | 10-outcome enum protects the 4 typed-GO + 3 direct-GO rows | SPEC §0.x pins the 10-identifier W0-admissible enum `A C G I J K L M N-direct S`; §1 non-negotiable "No new outcome variant." P3-D §3 `§0 V3 fold footer` item (1) records the ruling went past-tense — the V1 SPEC §0.3 7-identifier subset was a SPEC-text defect the V3 SPEC corrected, never a code change. The 4 typed-GO rows carry `A`; the 3 direct-GO rows carry `A`; the enum admits `A` (beat-and-parity). No V3 edit narrows the enum below `A`. The GO-row outcome identifier is protected. | HOLD |
| 17 | 4 typed-GO rows protected on every typed-codepath wave | SPEC §4 (W1) maintain envelope: "the four typed-GO rows hold `A / GO`." P3-C §2 W1 (a): `twitter`/`update_center`/`mesh`/`marine_ik` `real_typed_struct A / GO` each ≥ `sonic_strict / 1.10`. SPEC §6 (W3) clause 4 + P3-C §2 W3 name them. P3-C §2 W1 (a) and the W3/W4 envelopes carry them. The four-row set is exact against `RESULTS.md` `real_typed_struct A/GO` (`:7,18,21,28`). The W4b three-way split did not perturb the typed-GO protection — W4b-1/W4b-3 ship no parse-loop edit, W4b-2 carries the maintain envelope. Protected. | HOLD |
| 18 | 3 direct-GO rows protected — W3 + W4a + W4b-2 | P3-C §2 W3 names `citm_catalog`/`marine_ik`/`unicode_basic` `direct_to_struct A / GO` (the V1 REVISE #26 fold, carried into V3). P3-C §2a W4a maintain envelope and §2a W4b-2 maintain envelope both carry the three direct-GO rows "hold their `A / GO` with no delta beyond noise." SPEC §6 clause 4 (W3), §7.1 clause 5 (W4a — the unicode-direct three-row CI guard), §7.2.2 clause 6 (W4b-2 — REDRESS 82's blocking rows become W4b-2's no-regression rows). The three direct-GO rows are protected on every wave/sub-wave that rewires `match_string_at_quote_trusted_utf8` or the JSON unescape hot path. Protected; the W4b split preserved the W4b-2 binding. | HOLD |
| 19 | W3 cap decision — HIGH risk + ≤110-min extension does not reopen a REDRESS route | SPEC §2.2 "W3 redress cap" + §6 preamble record W3 is **not** sub-waved ("the class column and its sole SIMD producer form one cascade — splitting them orphans the class column from its only producer") and instead carries a CHALLENGE-gated redress extension to ≤110 min. CH3-relevant: the decision *not* to sub-wave W3 is the correct one for REDRESS-regression — splitting W3 would land the class column without its SIMD producer, which is exactly the orphan-substrate shape SPEC §1's same-wave-consumer non-negotiable and the REDRESS 92 blanket pre-block forbid. The ≤110-min extension is a triumvirate-timing decision, not a route-ownership change; it moves no REDRESS route. The W3 cap decision is REDRESS-clean. | HOLD |
| 20 | W1 — REDRESS 91 differential intact | SPEC §4 "Pre-blocked routes" reproduces P3-E §3.1: REDRESS 91's gap is a whitelist, not an architecture; W1 owns a fresh run-id/metadata validation and produces measured rows under it, then expands the whitelist; W1 admits Apache + CITM only; `canada/real_typed_struct` stays rejected (REDRESS 80 + the long-decimal mismatch). No reopen — W1 is the later accepted row-table wave REDRESS 91 deferred to. | HOLD |
| 21 | W2 — REDRESS 92 precursor + 50-72 honoured | SPEC §5 "Pre-blocked routes" reproduces P3-E §3.2 / §2.3: W2 is the routed precursor REDRESS 92 named, not a reopen; five-axis differential (no production consumer, no row-movement surface, touches no parser-control file, adds no payload field, proves the existing `ValueRef` cursor); REDRESS 50/51/53 honoured (no parser-written aux table, no parser-local cursor); REDRESS 71 orthogonal. Proof-only depth. No reopen. | HOLD |
| 22 | Hard pre-block list — 13 items carried into both V3 drafts, W4b-split adjusted | The dispatch §"Pre-Blocked Routes" enumerates the 13 hard pre-blocks. Item 9 (string-scanner widening / boundary-collapse class) now reads "only W4a and the codec sub-waves W4b-1/W4b-2/W4b-3 carry pre-registered admissions" — the W4b split is correctly threaded into the hard pre-block list. P3-E §4 item 9 carries the identical phrasing. No hard pre-block dropped or weakened in the V3 fold; the only edit is the codec-sub-wave naming. | HOLD |
| 23 | REDRESS 33 / `match_tiny_plain_string` — still hard-blocked | P3-E §4 item 13 + §2.6 keep NEON `match_tiny_plain_string` as a Class-A retained-G fix permanently pre-blocked (REDRESS 28 + 33). The dispatch §"Pre-Blocked Routes" item 13 reproduces it. SPEC §7.1 (W4a) targets `match_string_at_quote_trusted_utf8`, NOT `match_tiny_plain_string`. No V3 sub-wave wires Class-A NEON `match_tiny_plain_string`. Hard pre-block honoured. | HOLD |
| 24 | REDRESS 93 — scalar-parent fold stays blocked, no direct plane entered | SPEC §1 non-negotiable: "scalar-parent folding stays blocked by REDRESS 93." Dispatch §"Pre-Blocked Routes" item 4. SPEC §7.2.2 (W4b-2) "Pre-blocked routes" REDRESS 66-69 + 93 — "W4b-2's gate is `parse_only` only; it does not enter the direct plane / DirectBuild semantic string facts." §7.2.3 (W4b-3) carries no direct-plane consumer. §4 (W1) "touches no direct guard plane." No V3 wave reopens REDRESS 93. | HOLD |
| 25 | W4c/W4d HANDOFF §5 default-rewire boundary | SPEC §7.3 final pre-block bullet: 'HANDOFF §5 "PMULL prefix-XOR as default hot paths" applies to PMULL re-admission, explicitly not to the SHA3-gated wave-internal EOR3 fold.' §7.4 final bullet: 'HANDOFF §5 "CTZ/bulk production rewires as default hot paths" applies to default rewires; W4d is a host-capability-gated specialisation at a non-default call site.' Both keep the EOR3/CTZ kernels capability-gated specialisations with unconditional scalar fallbacks. The HANDOFF §5 hard pre-block (dispatch item 7) is honoured. | HOLD |
| 26 | P3-D non-fold residual — closed in V3 | The V2 CH3 #21 noted P3-D was not re-authored in the five-file V2 fold. The V3 fold re-authored it: P3-D reads `Cycle: V3`, carries a `§0 V3 fold footer` recording the §3 outcome-enum ruling went past-tense, the §2 "31 distinct" confusion resolved to the 36-identifier set, the N5 `SkV8ComparatorEvidence` 6→7 field correction, and the §2.3 per-wave population table re-bound to the V3 behaviour waves W4a/W4b-1/W4b-2/W4b-3/W4c/W4d. The V2 census residual is closed. P3-D still owns no REDRESS-route surface (the 10-outcome enum and 36-field schema are not REDRESS-route surfaces) — CH3-immaterial either way, but the V3 fold leaves no un-refolded sibling. | HOLD |
| 27 | P3-A §3 stale "one cascade-locked behaviour wave" prose — closed in V3 (was V2 REVISE #27) | The V2 CH3 #27 REVISE flagged P3-A §3 line 696-697 carrying the pre-sub-wave "C3 + C4 + C5 + C6 (+ C7) as one cascade-locked behaviour wave" recommendation. The V3 fold corrected it: P3-A §3 "Reading the graph for P3-B wave sequencing" now reads "P3-F SPEC §2.2 gives the binding reading: the cascade-lock is satisfied by **W3 (the C3 union substrate) preceding the W4 sub-waves** — it does NOT mean one monolithic redress wave," and names C5→W4a, C4→W4b-1/W4b-2/W4b-3, C6→W4c, C7→W4d. P3-A `§0 V3 fold footer` item (1) records 'the stale "one cascade-locked behaviour wave" / bare "the wave may not be split" prose is corrected.' The V2 REVISE #27 — and the indirect regression risk that a future plan agent re-proposes the monolithic W4 the CH4 ceiling rejected — is closed. | HOLD |
| 28 | No V3 edit reopens a REDRESS route — fold-scope audit | The V3 fold re-authored seven plan artefacts under `research/p3/` plus the SPEC/dispatch drafts; it touched no source file and did not mutate `skinny/REDRESS.md` (no provenance line changed — the P3-E §1 lettered→numeric mapping and §6 SUPERSEDED table are wave-facing projections, not REDRESS-entry edits). Every per-wave "Pre-blocked routes" section in the V3 SPEC is verbatim from P3-E's ledger; the W4b split distributes the P3-E §2.5/§3.4 codec pre-block across §7.2.1/§7.2.2/§7.2.3 with no entry lost (the union of the three SPEC sub-wave pre-block lists equals P3-E §2.5's set). A documentation integration of plan artefacts cannot reopen a REDRESS route by construction. No reopen. | HOLD |
| 29 | W4b bundle→three-sub-wave split — pre-block completeness | P3-E §2.5 lists the W-UC honour-set (REDRESS 64, 66-69, 65, 84, 60-62) and the §3.4/§3.5 adjacents (82, 83). When the SPEC splits W-UC into W4a + W4b-1/W4b-2/W4b-3: SPEC §7.1 carries the §3.5 (REDRESS 83) + 60-62 honour-set; §7.2.1 carries the codec-surface pre-blocks; §7.2.2 carries the §3.4 (REDRESS 82) + 64 + 66-69+93 set; §7.2.3 carries REDRESS 82 (variable-width binding) + 85-87/Lock 14. The union of SPEC §7.1 + §7.2.1 + §7.2.2 + §7.2.3 pre-blocks ⊇ P3-E §2.5's set — no pre-block entry is lost in the bundle→sub-wave split. The four-way distribution is clean. | HOLD |
| 30 | W4d entry gate — double-deep dependency, no orphan-kernel reopen | SPEC §7.4 entry gate: "W3 closed (the union-substrate string-mask consumer is the non-orphan condition) **and** W4a closed (the 32-byte block scanner the CTZ extracts from is live). W4d is the deepest sub-wave — it needs both." CH3-relevant: W4d's kernel landing before either consumer would be the SK-V5 orphan-kernel failure shape — and the orphan-kernel shape is the REDRESS-82-adjacent route the §3.6 differential pre-blocks. The double-gate (W3 + W4a) forecloses it. P3-E §3.6 binding-gate clause "blocks on P2-A landing in the same wave — absent the union-substrate string-mask consumer, the CTZ extract is an orphan and does not ship" is carried. No orphan-kernel reopen. | HOLD |
| 31 | SPEC §2.2 cascade-lock disambiguation — three "same-wave" relations, no orphan kernel | SPEC §2.2 names the three relations distinctly — (1) cascade-lock (a P2-D kernel lands only after the W3 union substrate exists); (2) same-wave consumer (every primitive + its hot-path caller in one commit); (3) codec/scanner pairing (W4a + W4b-2 strictly adjacent). CH3-relevant: an orphan kernel landing before its W3 consumer is the SK-V5 orphan-kernel failure shape and a REDRESS-82-adjacent route; SPEC §2.2 + every W4 sub-wave entry gate ("W3 closed with `G-W3-UNION-SUBSTRATE` PASS") forecloses it. The V3 W4b split kept the disambiguation intact and extended it across the three codec sub-waves. No orphan-kernel reopen. | HOLD |

---

## §3 — Aggregate verdict

The V3 comprehensive integration fold closed both V2 CH3 REVISEs and
introduced no regression.

- **The manifest is unified across all seven artefacts (#1).** SPEC §2,
  the dispatch §"Wave Manifest", P3-A §3, P3-B §2/§3, P3-C §1.4/§2a,
  P3-D §2.3, and P3-E §1 all carry the W1-W5 / W4a / W4b-1/W4b-2/W4b-3
  / W4c / W4d spine. The V2 defect — the SPEC correct, the siblings
  lagging — is closed; the V3 fold left no un-refolded sibling.
- **P3-C is re-authored — no standalone W5-ASM section (#2).** The
  "W5 aarch64 ASM kernels" wave is dissolved; EOR3→W4c, CSSC CTZ→W4d,
  the structural-bitmap chain folds into W3; P3-C §2a is a per-sub-wave
  gate table and the un-sourced `github_events`/`random` W5 rows are
  removed.
- **P3-E carries the lettered→numeric mapping (#3).** P3-E §1 fixes
  W-AC→W1 … W-AS→{W4c,W4d}; every §2/§3 header is numeric-labelled; the
  per-wave pre-block content is unchanged.
- **The codec is paired — W4a + W4b-2, no REDRESS-82 orphan (#4, #5).**
  The W4b three-way split kept the pairing exact: W4a pairs with W4b-2,
  the row-moving sub-wave; W4b-1 and W4b-3 carry no row gate and cannot
  be codec-only orphans.
- **The five material differentials survive (#6-#11).** Union vs
  92/50-72, codec vs 82, EOR3 vs 88, string-block vs 83, CTZ vs 89 —
  each cross-checked verbatim against `skinny/REDRESS.md`, each bound to
  a same-row gate. The W4b split re-pinned the codec differential to
  W4b-2's row gate without losing it.
- **The W10b six-row maintain gate is bound to every parse-loop-touching
  wave and sub-wave (#12-#15)** — W3, W4a, W4b-2, W4c, W4d — and to the
  §0.1 close condition's "and sub-wave" clause; the V3 arithmetic
  correction gives the block one uniform `floor(today × 0.98)` rounding
  convention and corrects the stale `canada` parenthetical.
- **The 4 typed-GO + 3 direct-GO rows are protected (#16-#18)** on every
  wave/sub-wave touching a typed/direct codepath; the W4b split
  preserved the W4b-2 maintain-envelope binding.
- **No V3 edit reopens a REDRESS route (#19, #28).** The W3 cap
  decision (not sub-waved; ≤110-min CHALLENGE-gated) is the
  REDRESS-correct choice — sub-waving W3 would orphan the class column.
  The fold is a documentation integration; `skinny/REDRESS.md` and all
  source were untouched.
- **Both V2 REVISEs are closed (#26, #27).** P3-D is re-authored;
  P3-A §3's stale monolithic-wave prose is corrected to the
  disambiguated W4a-d reading.

CH3 V3 ACCEPT-rate: **31/31 HOLD, 0 REVISE, 0 DEFECT = 100%**. This
clears the §3Z 95% threshold. The CH3-binding obligation — no wave
reopens a pre-blocked route absent a cited entry, a material
differential, and a same-row gate — is fully met by the V3 SPEC and
dispatch drafts, and the W4b three-way sub-division (the V3 fold's
structural change) carries every pre-block intact.

Verdict: **ACCEPT.** The V3 comprehensive fold closed every V2 CH3
residual, unified the manifest across all seven artefacts, and reopened
nothing. CH3 clears for V3 at 100%.

---

## §4 — Remaining REDRESS-regression risks

No reopen survives into a dispatched artefact. The following are
implementation-time watch-items, not V3-artefact defects.

1. **W4b-2 codec-consumer re-body — watch-item for the W4b-2
   triumvirate.** W4b-2 re-bodies the already-wired `unescape_four_unicode_escapes`
   x4 path onto the codec kernel. The REDRESS-82 differential (§7.2.2
   "Pre-blocked routes") holds *only* while the consumer stays the
   pre-existing x4 path + the `sink.rs` call-site swap. If the W4b-2
   redress agent ever re-bodies the codec onto a *new* parser-owned site
   or adds a retained per-`\u` validator/scratch, the differential
   collapses to the REDRESS-82 shape. The W4b-2 plan-time CHALLENGE must
   re-confirm the consumer is the x4 path + `sink.rs` only. This is a
   redress-agent watch-item, carried forward to the W4b-2 triumvirate;
   it is not a P3-artefact regression.

2. **W3 ≤110-min redress extension — CHALLENGE-gated, not pre-granted.**
   The W3 CHALLENGE-gated extension to ≤110 min is admissible only if
   the W3 plan's slice estimate demonstrates the union substrate + the
   §5 SIMD chain cannot co-land in 75 min. If the W3 redress instead
   sub-divides W3 to fit the 75-min cap, the split must keep the class
   column and its SIMD producer in one commit (SPEC §6 + §1
   same-wave-consumer non-negotiable) — a W3a/W3b split that landed the
   column without its producer would be the orphan-substrate shape. The
   V3 SPEC correctly forecloses the sub-wave route and routes the
   timing pressure through the extension; the W3 CHALLENGE must hold
   that line. Watch-item for the W3 triumvirate, not a V3-artefact
   defect.

The V3 SPEC and dispatch drafts honour every pre-block in P3-E's
ledger; no residual REVISE survives into a dispatched wave.
