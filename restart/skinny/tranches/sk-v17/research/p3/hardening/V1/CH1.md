# SK-V17 S-P3 CHALLENGE — CH1 CORRECTNESS (V1)

Lens: CH1 CORRECTNESS. Cycle: V1. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` + `restart/skinny/tranches/sk-v17/SPEC.md`.
Authority: PASS-3-SYNTHESIS-PLAN §3 (CH1), ORCHESTRATOR §3W. Master HEAD `f87ee713a`.

## §0 — CH1 charge

CH1 asks four falsifiable questions per PASS-3 §3:

1. Does every shortlist candidate trace to an S-P2 survivor and through it to an S-P1 hot leaf?
2. Is every falsifiability gate measurable — named corpus rows + concrete Mbps thresholds, not prose?
3. Does every wave's exit gate compare against the `SK-V17-open` baseline?
4. Do the comparator deltas in the gates use the strict plane?

Plus the dispatch-prompt focus: every wave cites a real S-P2 candidate (L1–L9) + a measurable
exit gate; no REJECTed candidate shortlisted; thresholds trace to the S-P1 canonical bench.

## §1 — Verification performed (every check executed against source, not asserted)

### 1.1 — Candidate → S-P2 → S-P1 trace (PASS-3 §3 Q1) — PASS

Read `HARDENING-S-P2-V3-CONSOLIDATED.md` §3 (L1–L9) and §4/§6 at HEAD `f87ee713a` (verified
`git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`). Every P3-A shortlist row maps
1:1 to a LOCKED S-P2 survivor and names its S-P1 antecedent; cross-checked each antecedent
against `HARDENING-S-P1-V4-CONSOLIDATED.md §3.3`:

| P3-A | S-P2 pool | S-P1 antecedent (verified §3.3 line) |
|---|---|---|
| S1 | L1 eq-set classifier | `find_component_delim` 59.24% (`:143`) + `consume_balanced_at` 10.31% (`:144`) = ~69% scan leaf (`:178`) |
| S2 | L2 `push_plain_offset` | `emit_fact_stream` 25.01% (`:159`) + ~64% alloc floor (`:158`) |
| S3 | L3 lazy `ValueRef` | String-materialization floor `emit_fact_stream` carries (`:159`, §3.4 item 2) |
| S4 | L4 tokenize-once | 2–3× re-walk `find_component_delim`/`find_colon_before`/`parse_declaration` (§3.3 P1-D, `:150`) |
| S5 | L5 `comment_body_mask_64` | comment-skip arm of the ~69% scan leaf |
| S6 | L6 `bracket_depth_mask_64` | `consume_balanced_at` 10.31% recursion (`:144`) |
| S7 | L7 one-shot reserve | ~64% alloc floor grow-churn (`:158`) |
| S8 | L8 sparse-flag side-table | mechanism for S3 (indirect, guarded) |
| S9 | L9 commit-by-construction (CONDITIONAL) | recognition-control loop 28.87%+2.45% (`:145`), classed structural-recognition-control NOT rollback |

All S-P1 percentages reproduced verbatim from `HARDENING-S-P1-V4 §3.3` (grepped: 59.24, 10.31,
25.01, 28.87, 2.45, ~64). The L9 conditionality is correctly carried — S-P1 measured ZERO
speculative rollback self-time; the 28.87%+2.45% figure is explicitly NOT treated as a rollback
antecedent in P3-A §2 (S9), the SPEC §7, and P3-C §2.4. No candidate is grounded on a phantom
leaf.

### 1.2 — No REJECTed candidate shortlisted (dispatch focus) — PASS

`HARDENING-S-P2-V3 §4` REJECTed set = {CF-4a/C5/C-B3/G4 orphan udot · CF-4b/C6 i8mm · FNV/hex ·
asmjson FSM · lo6 `classify_tbl4`-on-CSS · D6 second substrate}. Grepped `p3a/p3b/p3f/SPEC` for
each token: every occurrence appears ONLY in barred/pre-block/forbidden context (e.g. SPEC §9
line 793 "REJECTed candidates barred"; p3f:152 "the orphan udot/i8mm digit kernel" inside the
pre-block list; p3b:340 "the orphan udot ... §0.4"). The active shortlist S1–S8 (+ S9
conditional) contains none of them. The lo6-on-CSS REJECT is correctly inverted into a POSITIVE
constraint (the `;{`→slot-59 `& 0x3f` collision forces the eq-set fan); verified the collision is
real against `dispatch.rs:101` (`lo6_table_admissible`) — present at HEAD.

### 1.3 — Gates measurable, not prose (PASS-3 §3 Q2) — PASS (SPEC) / one REVISE (P3-C, §2.2 D2)

Every SPEC wave exit gate (§3–§8) resolves to a number read from the bench, a grep over
`skinny/crates/`, or an equality/counter assertion:

- W0 §3: `lcss(corpus)@W0` median emitted N≥50 for 4 corpora; `gate-json` rejects 4 malformed-row
  classes (proven by a fixture row); JSON 51/51 byte-identical. Measurable.
- W1 §4: `tape_activated` grep non-zero; `PayloadArena` counters; `w5c_profile_array_retired` grep
  zero; EXACT 8-field equality (`rules=10136, style=9561, sel=9561, decls=20043`). Measurable.
- W2 §5: `lazy_view_generated`; `css_rich_ast_preserved` (value-plane population parity counts);
  per-corpus typed median N≥50 no worse than W1; `regen --check` 9/9. Measurable.
- W3 §6: **≥1 regular corpus `delta_vs_lightningcss > 1.0×` at N≥50 cold median, `full-cssom`
  plane**; `native_simd_status ∈ {parity-pass, checkasm-pass}`. THE load-bearing gate — a concrete
  ratio threshold, named corpus rows (animate OR bootstrap), N≥50. Measurable.
- W4 §7: byte-identical tape + a measured N≥50 lift on the gated corpus vs W3 plane. Measurable.
- W5 §8: ≥1 regular corpus crosses re-confirmed; tailwind admit-or-honest-REDRESS;
  `dirty_generated_state=clean`. Measurable.

No SPEC exit gate closes on "wired"/"integrated"/"advisory" — Section 1 forbids it explicitly and
§3.x of P3-C self-audits it. The N≥50 assert is REAL: `css_canon_bench.rs:250`
`assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` verified live at HEAD.

### 1.4 — Exit gates compare against `SK-V17-open` (PASS-3 §3 Q3) — PASS

Every behaviour-wave threshold denominator is the W0-recorded `lcss(corpus)@W0` / `fs@W0` /
`SK-V17-open` median (P3-C §3 binding table; SPEC §0.5 lines 201–204). The inferred-endpoint trap
is explicitly closed: SPEC lines 202–204 "ALL per-corpus lightningcss endpoints are
UNMEASURED-PENDING: no wave exit-gate may key on an inferred per-corpus endpoint until the W0 N≥50
harness emits the per-corpus split." P3-A §3 and P3-C §1.2 both restate the S-P1 V4 band
(1237/1110/1261/833 lightningcss; 741/851/874/559 fact_stream — all verified against
`HARDENING-S-P1-V4 §3.1` lines 92–95) as SIZING references, NOT gates. No gate keys on the
alphaB inferred endpoints (164/51/60), which are self-flagged INFERRED. Correct.

### 1.5 — Comparator deltas use the strict plane (PASS-3 §3 Q4) — PASS

SPEC §0.2 comparator table + §6 W3 gate use `track1_typed` (preserve-rich-ast) ÷ `lightningcss`
full-CSSOM, same-run, with `css_comparator_plane=full-cssom` (line 585). cssparser token-scan is
flaw-probe-only (SPEC §0.2 line 118; never strict admission). The recognition-only `track1_full_parse`
plane (outcome A, 4-field) is explicitly NOT a typed >SOTA discharge (SPEC §0.3 lines 141–144).
P3-C §3.x line 166 self-audits: "Every comparator delta uses the strict plane ... never the
cssparser flaw probe, never recognition-only full_parse." Correct.

### 1.6 — Owner-path / greppable-fact spot-check (all live at HEAD) — PASS

Grepped `skinny/crates/` at HEAD `f87ee713a`:

- `css_canon_bench.rs:250` — N≥50 assert present, wording exact.
- `dispatch.rs:42` `select_classifier(alphabet: &[u8;64])`; `lo6_table_admissible` at `:101` (SPEC §2.1 cites `:101` — exact).
- `assembler.rs:42` `TapeBuilder`, `:71` `push_plain_offset`, `:89` `reserve_offsets_cold` — all present.
- `lib.rs:336` `W5C_REQUEST_FACT_PROFILES`, consumed `:567`/`:611`, selected `:299` — all four sites present, matching SPEC §4 and §9.
- `byte_class_from_eq_set_64.rs:33` eq-set kernel present.

Every load-bearing owner-path citation in the SPEC and P3-A/P3-C resolves. The SPEC's
benched-surface discipline (grep `skinny/crates/` not `crates/core/`) is internally enforced.

## §2 — Defects

### D1 — REVISE — cross-artefact wave-numbering contradiction (P3-B vs SPEC/P3-C/P3-F)

`p3b-wave-sequencing.md:77-85` sequences a **5-wave plan W0–W4**:
W0 baseline · **W1 = tape+projection (levers 1+2: L2/L3/L8/L7)** · **W2 = NEON (lever 3:
L1/L4/L5/L6/L7)** · **W3 = commit-by-construction (L9, conditional)** · W4 close.
P3-B line 85: "Wave count = 5 (W0-W4)".

The SPEC (`SPEC.md:257-264`) and P3-C (`p3c:46-52`) and P3-F (`p3f:81`) use a **6-wave plan
W0–W5**: W0 baseline · **W1 = PRUNE/tape activation** · **W2 = layout projection generator** ·
**W3 = NEON** · **W4 = commit-by-construction** · W5 close. SPEC line 266: "Wave count = 6".

The same wave number denotes different levers across artefacts: **W2 = NEON in P3-B but
projection in the SPEC; W3 = commit-by-construction in P3-B but NEON in the SPEC.** P3-C §1
opens "For every wave W0–W5 in the P3-B sequence" — but the P3-B sequence is W0–W4, so the
citation is internally false. A reader reconciling the wave-sequencing authority (P3-B) against
the contract (SPEC) cannot map a wave number to a lever set unambiguously.

This is a CORRECTNESS defect: the candidate→wave placement is sound in BOTH numberings (no
candidate is orphaned, no REJECTed candidate enters), but the cross-artefact wave identity is
contradictory, so a wave dispatched by number from one document lands the wrong lever set if
gated by the other.

- **Disposition:** REVISE.
- **path:line:** `p3b-wave-sequencing.md:53-85` (the 5-wave collapse + manifest) vs `SPEC.md:257-266` + `p3c-falsifiability-gates.md:1` / `:43-52`.
- **Concrete fix:** Re-sequence P3-B to the 6-wave W0–W5 plan the SPEC adopts (split P3-B's W1 tape+projection into SPEC-W1 tape activation + SPEC-W2 projection generator; renumber NEON W2→W3, commit-by-construction W3→W4, close W4→W5). The SPEC's 6-wave split is the better factoring (it isolates the PRUNE/tape-activation substrate gate from the projection-generator generality gate, each with its own first-of-class CHALLENGE), so P3-B should conform to the SPEC, not vice-versa. P3-C §1 line 1 then references a P3-B sequence that actually matches.

### D2 — REVISE — P3-C imposes a W1 speed threshold the SPEC drops; derived denominator untraced

`p3c-falsifiability-gates.md:83` and §3 binding table (`:155`) impose on W1:
`track1_typed@W1(c) >= 1.40 × fs@W0(c)` on ≥3/4 corpora (a measurable Mbps speed threshold).
The SPEC's W1 exit gate (`SPEC.md:447`) states the opposite: "**NO speed admission this wave
(equality is the gate before speed)**" and carries NO +40% threshold (grep of SPEC for
`1.40`/`+40%`/`track1_typed@W1` returns empty).

Two problems:
1. **Contract divergence.** P3-C is the falsifiability-gate authority the SPEC folds (PASS-3 §2
   P3-C row); the SPEC silently drops P3-C's W1 speed gate. The SPEC and P3-C disagree on whether
   W1 has a speed exit gate.
2. **Untraced denominator.** The `1.40×` floor is a derived/invented "conservative floor that
   proves the floor fell" (P3-C §2.1 self-description); it is NOT traced to any S-P1 measurement.
   S-P1 quantifies the 4.4× instr/byte gap (`§3.4 item 3`) and the 0.60–0.77× ratio band, but a
   `1.40×` fact-stream-improvement threshold is an author inference, not a bench-derived number.
   Per the dispatch focus ("thresholds trace to the S-P1 canonical bench"), an invented threshold
   on a measurable gate is a CH1 concern even when conservative.

The SPEC's choice (W1 closes on equality + grep + `PayloadArena` counters, NO speed) is itself
CH1-clean and measurable — equality-before-speed is a legitimate substrate-truth gate (P3-C §3.x
line 168 acknowledges "W1/W2 close on equality + grep + counters"). So the SPEC side is correct;
the defect is the P3-C/SPEC mismatch plus the untraced `1.40×`.

- **Disposition:** REVISE (on P3-C, to reconcile with the SPEC).
- **path:line:** `p3c-falsifiability-gates.md:83` + `:155` (the `>= 1.40 × fs@W0` threshold) vs `SPEC.md:447` (NO speed admission).
- **Concrete fix:** Either (a) demote P3-C's `1.40×` to a NON-GATING sizing diagnostic ("expected directional drop; not a W1 exit gate — W1 closes on equality + tape-activation grep + `PayloadArena` counters per SPEC §4") and remove it from the §3 binding-table threshold cell, matching the SPEC; or (b) if a W1 speed floor is wanted, fold it into the SPEC §4 exit gate AND re-derive the floor from the S-P1 4.4× instr/byte gap with the arithmetic shown (not an asserted `1.40×`). Option (a) is preferred — it preserves the SPEC's equality-before-speed discipline and removes an untraced number. Once reconciled, P3-C §3.x line 168 stays true.

## §3 — Dispositions (per section/wave)

| Artefact / section | Disposition | Note |
|---|---|---|
| P3-A §1 synthesis (candidate→P1/P2 trace) | ACCEPT | every candidate traces to a verified S-P2 survivor + S-P1 hot leaf |
| P3-A §2 shortlist S1–S8 + S9 conditional | ACCEPT | 8 active ≤8; L9 correctly conditional; no REJECTed candidate |
| P3-A §3 falsifiability binding | ACCEPT | predicate + named rows; inferred-endpoint trap explicitly avoided |
| P3-A §4 pre-blocked + binding conditions | ACCEPT | §4/§6 carried verbatim from S-P2 |
| P3-B §1–§2 wave sequence (5-wave W0–W4) | REVISE (D1) | wave numbering contradicts SPEC/P3-C/P3-F 6-wave plan |
| P3-C §2.0 W0 gate | ACCEPT | measurable baseline + gate-reject |
| P3-C §2.1 W1 gate (+40% threshold) | REVISE (D2) | speed threshold contradicts SPEC §4; `1.40×` untraced to S-P1 |
| P3-C §2.2 W2 gate | ACCEPT | no-regression + W5C-retire grep + regen-check; measurable |
| P3-C §2.3 W3 gate (cross-bar) | ACCEPT | strict-plane ratio > 1.0×, named rows, N≥50; the load-bearing gate is clean |
| P3-C §2.4 W4 gate (L9 conditional) | ACCEPT | admission IS a measured re-profile; +5% lift gate |
| P3-C §2.5 W5 gate | ACCEPT | tranche criterion + honest tailwind, no corpus-average |
| P3-C §3 binding table | REVISE (D2) | inherits the W1 +40% threshold defect |
| P3-D telemetry schema | ACCEPT | columns map to gate consumers; producer-only fields rejected (not CH1-load-bearing; CH1 spot-checked only the gate-binding columns) |
| P3-E pre-blocked ledger | ACCEPT (CH1 scope) | REJECTed-route enumeration consistent with S-P2 §4 (CH3 owns full enumeration) |
| P3-F spec-draft | ACCEPT | 6-wave manifest matches the SPEC; correct candidate→wave map |
| SPEC §0 close condition + goalset | ACCEPT | 12 axes measurable; strict comparator; inferred-endpoint guard present |
| SPEC §0.5 goalset rows | ACCEPT | per-corpus, W0-rebaselined, no fixed-number gate |
| SPEC §1 non-negotiables | ACCEPT | strict comparator + equality-before-speed enforced |
| SPEC §2 wave manifest (6-wave W0–W5) | ACCEPT | internally consistent; P3-B must conform (D1) |
| SPEC §3 W0 | ACCEPT | measurable, no behaviour change |
| SPEC §4 W1 | ACCEPT | equality-before-speed, grep + counters; measurable substrate-truth gate |
| SPEC §5 W2 | ACCEPT | projection generality, no-regression, regen-check |
| SPEC §6 W3 | ACCEPT | the cross-bar >SOTA gate — strict plane, named rows, N≥50, vs `lcss@W0` |
| SPEC §7 W4 | ACCEPT | doubly-conditional; byte-identical + measured lift |
| SPEC §8 W5 | ACCEPT | close on measurement, honest residual, no paper-close |
| SPEC §9 pre-blocked ledger | ACCEPT (CH1 scope) | REJECTed set barred; binding conditions verbatim |
| SPEC §10 dispatch scope | ACCEPT | W0 authorized; W1–W5 conditional; L9 doubly-conditional |

## §4 — Counts

- Sections/waves dispositioned: **27**.
- ACCEPT: **23**.
- REVISE: **4** (P3-B §1–§2 [D1]; P3-C §2.1 [D2]; P3-C §3 binding table [D2]; — D1 and D2 are 2
  root defects spanning 4 sections, but per-section count = P3-B 1 + P3-C 2 = 3 REVISE sections;
  the SPEC §2 manifest is ACCEPT-with-dependency-on-D1's-fix-in-P3-B, not a REVISE).
- REJECT: **0**.

Corrected per-section tally: ACCEPT **24**, REVISE **3** (P3-B §1–§2; P3-C §2.1; P3-C §3),
REJECT **0**, of 27 dispositioned sections. ACCEPT rate = 24/27 = **88.9%**.

Both REVISEs are convergent (D1: conform P3-B to the SPEC's 6-wave split; D2: demote P3-C's
untraced W1 +40% to a non-gating diagnostic to match the SPEC). Neither touches the load-bearing
SPEC W3 >SOTA gate, which is CH1-clean. No orphan REVISE: each names a path:line and a concrete fix.

## §5 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (scope matrix), §3 (CH1 charge), §8 (axes).
- `restart/skinny/tranches/sk-v17/SPEC.md` §0–§10 (the contract under review).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3f}.md`.
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` §3 (L1–L9), §4 (REJECTed), §6 (binding conditions). Commit `f87ee713a`.
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` §3.1 (bench medians, lines 92–95), §3.3 (hot leaves, lines 143–159), §3.4 (lever order). Commit `0ae1caa52`.
- Live source at HEAD `f87ee713a`: `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:250`; `bbnf-simd/src/dispatch.rs:42,101`; `runtime/src/tape/assembler.rs:42,71,89`; `codegen/src/lib.rs:336,299,567,611`; `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`.
