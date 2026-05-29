# SK-V17 S-P3 CHALLENGE — CH1 CORRECTNESS (V2)

Lens: CH1 CORRECTNESS. Cycle: V2. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` + `restart/skinny/tranches/sk-v17/SPEC.md`.
Authority: PASS-3-SYNTHESIS-PLAN §3 (CH1), ORCHESTRATOR §3W. Master HEAD `f87ee713a` (verified `git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §0 — CH1 charge

CH1 asks four falsifiable questions per PASS-3 §3, plus the dispatch focus:

1. Does every shortlist candidate trace to an S-P2 survivor (L1–L9) and through it to an S-P1 hot leaf?
2. Is every falsifiability gate measurable — named corpus rows + concrete Mbps thresholds, not prose?
3. Does every wave's exit gate compare against the `SK-V17-open` baseline?
4. Do the comparator deltas in the gates use the strict plane?

Dispatch focus: every wave cites a real S-P2 candidate (L1–L9) + a measurable exit gate (named corpus rows + Mbps threshold vs lightningcss N≥50); no REJECTed candidate shortlisted; thresholds trace to the S-P1 canonical bench.

## §1 — V1 fold verification (the two V1 REVISEs are resolved)

V1 CH1 raised two convergent REVISEs. Both are folded in V2 — verified against source, not asserted:

### V1 D1 (wave-numbering contradiction P3-B vs SPEC/P3-C/P3-F) — RESOLVED

V1 found P3-B sequenced a **5-wave** map (W2=NEON, W3=L9) while the SPEC/P3-C/P3-F used a **6-wave** map (W2=projection, W3=NEON, W4=L9). V2 `p3b-wave-sequencing.md` is re-authored to the SPEC six-wave manifest verbatim:

- `p3b:10-31` carries an explicit "§0 — Cycle V2 fold note" documenting the re-sequence (D1 / R-CH2-2 / CH4-2/3 / CH5-1 / R-CH7-1 attribution).
- `p3b:132` now reads "Wave count = 6 (W0-W5)" with the manifest table `p3b:123-130` mapping **W1=tape (L2/L7/L3-min) · W2=projection (L3-full/L8/L4) · W3=NEON (L1/L5/L6) · W4=L9 conditional · W5=close** — identical to `SPEC.md:262-269` and `p3c:43-52`.
- `p3c:5` opens "For every wave W0–W5 in the canonical SPEC 6-wave manifest" — the V1 false "W0–W5 in the P3-B sequence" citation against a W0–W4 sequence is gone; P3-C and P3-B now agree on six waves.
- `p3f:46,80-83,112-113` carry the 6-wave W0–W5 manifest. Cross-artefact wave identity is now unambiguous: a wave number denotes the same lever set in every document.

### V1 D2 (P3-C W1 `1.40×` speed threshold untraced + contradicts SPEC) — RESOLVED

V1 found P3-C imposed `track1_typed@W1(c) >= 1.40 × fs@W0(c)` while `SPEC.md:447` states "NO speed admission this wave" and the `1.40×` traced to no S-P1 measurement. V2 P3-C demotes it cleanly, option (a) as V1 recommended:

- `p3c:83` (W1 Mbps row) now reads "**NO speed admission this wave** (`SPEC.md:447`)" and "No +40% (or any %) denominator is author-invented as a gate — it has no S-P1/S-P2 trace and is demoted to diagnostic per CHALLENGE V1 D2/REVISE-2." The `track1_typed@W1 ÷ fs@W0` ratio is now explicitly "DIAGNOSTIC (non-gating) sizing signal only … CANNOT fail the wave."
- `p3c:155` (§3 binding table W1 row) now reads "(no speed lift — substrate truth) … `track1_typed@W1(c) ÷ fs@W0(c)` RECORDED as diagnostic sizing telemetry only (non-gating, no threshold)."
- `p3c:168` (§3.x self-audit) restates the demotion citing "CHALLENGE V1 D2/REVISE-2, no S-P1/S-P2 trace for the 1.40× denominator."
- `SPEC.md:475` ("NO speed admission this wave") and P3-C now agree. The untraced number is removed from every gating cell. Grep of `p3c` for `1.40` returns only the demotion-narrative reference, not a threshold cell.

Both V1 root defects are folded with no orphan residue.

## §2 — Independent V2 verification (every check executed against source)

### 2.1 — Candidate → S-P2 → S-P1 trace (Q1) — PASS

Re-read `p3a-candidate-shortlist.md` (now `Cycle: V2`, the R1 cosmetic-fold obligation carried at `p3a:43-46`). Every P3-A row maps 1:1 to a LOCKED S-P2 survivor (L1–L9) and names its S-P1 antecedent; each antecedent re-verified against `HARDENING-S-P1-V4 §3.3` at HEAD:

| P3-A | L | S-P1 antecedent (verified §3.3) |
|---|---|---|
| S1 | L1 eq-set classifier | `find_component_delim` 59.24% (`:143`) + `consume_balanced_at` 10.31% (`:144`) = ~69% scan leaf |
| S2 | L2 `push_plain_offset` | `emit_fact_stream` 25.01% (`:159`) + ~64% alloc floor (`:158`) |
| S3 | L3 lazy `ValueRef` | String-materialization floor `emit_fact_stream` (`:159`) |
| S4 | L4 tokenize-once | 2–3× re-walk `find_component_delim`/`find_colon_before`/`parse_declaration` (`:150`) |
| S5 | L5 `comment_body_mask_64` | comment-skip arm of the ~69% scan leaf |
| S6 | L6 `bracket_depth_mask_64` | `consume_balanced_at` 10.31% recursion (`:144`) |
| S7 | L7 one-shot reserve | ~64% alloc floor grow-churn (`:158`) |
| S8 | L8 sparse-flag side-table | mechanism for S3 (indirect, guarded) |
| S9 | L9 commit-by-construction (CONDITIONAL) | recognition-control loop 28.87%+2.45% (`:145`), classed structural-recognition-control NOT rollback |

All S-P1 percentages reproduced verbatim (grepped `HARDENING-S-P1-V4`: 59.24 `:143`, 10.31 `:144`, 25.01 `:159`, 28.87+2.45 `:145`, 4.4× `:187`). The L9 conditionality is correctly carried — S-P1 measured ZERO speculative rollback self-time; the 28.87%+2.45% figure is explicitly NOT a rollback antecedent in P3-A §2 (S9), SPEC §7, P3-B Constraint B, and P3-C §2.4. No candidate is grounded on a phantom leaf.

### 2.2 — No REJECTed candidate shortlisted (dispatch focus) — PASS

`HARDENING-S-P2-V3 §4` REJECTed set = {CF-4a/C5/C-B3/G4 orphan udot · CF-4b/C6 i8mm · FNV/hex · asmjson FSM (x86) · lo6 `classify_tbl4`-on-CSS · D6 second substrate} (verified `:252-286,:343`). Grepped `p3a/p3b/p3f/SPEC` for each token, filtering out barred/forbidden context:

- `udot`: appears ONLY at `SPEC.md:848` (the §9 barred list).
- `i8mm`: appears at `SPEC.md:848` (barred list) and `SPEC.md:102` (the §0.1 NEON-discipline line "aarch64-only (NEON + optional dotprod/i8mm; NO x86, NO SVE)" — naming the architecture's optional feature within the aarch64-only constraint, NOT an active candidate). Neither is an active shortlist entry.

The active shortlist S1–S8 (+ S9 conditional) contains none of the REJECTed family. The lo6-on-CSS REJECT is correctly inverted into a POSITIVE constraint (the `;{`→slot-59 `& 0x3f` collision forces the eq-set fan) — verified the collision mechanism against `dispatch.rs:101` (`lo6_table_admissible`), present at HEAD. SPEC §2.1 (`:316`) and §9 (`:849`) both carry it.

### 2.3 — Gates measurable, not prose (Q2) — PASS

Every SPEC wave exit gate (§3–§8) resolves to a number read from the bench, a grep over `skinny/crates/`, or an equality/counter assertion:

- **W0 §3 (`:368-375`):** `SK-V17-open` captured; per-corpus lightningcss full-CSSOM median emitted N≥50 cold for 4 corpora; `gate-json` rejects 4 malformed-row classes (proven by a fixture row); JSON 51/51 within ±1.0%; NO behaviour change. Measurable.
- **W1 §4 (`:461-476`):** `tape_activated` grep non-zero; `PayloadArena` write/alloc counters; `w5c_profile_array_retired` grep zero; no dangling `emit_fact_stream` round-trip assertion (grep zero); EXACT 8-field equality (`rules=10136, style=9561, sel=9561, decls=20043`); JSON 51/51 ±1.0%; "NO speed admission this wave." Measurable substrate-truth gate.
- **W2 §5 (`:547-566`):** `lazy_view_generated`; the R-CH2-1 JSON-rider-byte-equal diff (the load-bearing CH2 anti-overfit check — diff of regenerated JSON `value_from_ref` = empty); `css_rich_ast_preserved` (value-plane population parity); per-corpus typed median N≥50 no worse than W1; regen-check; JSON 51/51. Measurable.
- **W3 §6 (`:632-643`):** **≥1 regular corpus (animate OR bootstrap) `delta_vs_lightningcss > 1.0×` at N≥50 cold median, `full-cssom` plane**; `native_simd_status ∈ {parity-pass, checkasm-pass}`; `simd_non_json_exercise=css_l4`. THE load-bearing cross-bar gate — a concrete ratio threshold, named corpus rows, N≥50, vs `lcss@W0`. Measurable.
- **W4 §7 (`:703-711`):** byte-identical tape + a measured `≥ +5%` N≥50 lift vs the W3 plane on the gated corpus; a lift below +5% disposes L9 as not-warranted (recorded, not a failure). Measurable; the L9 admission gate is itself a measured re-profile.
- **W5 §8 (`:750-761`):** `dirty_generated_state=clean` (`regen --check` 9/9 exit 0); ≥1 regular corpus crosses re-confirmed; tailwind admit-or-honest-REDRESS, no corpus-average; Lock-14 audit; RESULTS≡REDRESS≡HANDOFF agree. Measurable.

No SPEC exit gate closes on "wired"/"integrated"/"advisory" — Section 1 (`:249-251`) forbids it and P3-C §3.x (`:165`) self-audits it. The N≥50 assert is REAL: `css_canon_bench.rs:250` `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` verified live at HEAD.

### 2.4 — Exit gates compare against `SK-V17-open` (Q3) — PASS

Every behaviour-wave threshold denominator is the W0-recorded `lcss(corpus)@W0` / `fs@W0` / `SK-V17-open` median (P3-C §2/§3 binding table `:60,:150`; SPEC §0.5 lines 206–209). The inferred-endpoint trap is explicitly closed: SPEC `:207-209` "ALL per-corpus lightningcss endpoints are UNMEASURED-PENDING: no wave exit-gate may key on an inferred per-corpus endpoint until the W0 N≥50 harness emits the per-corpus split." P3-A §3 (`:177-184`) and P3-C §1.2 (`:28-37`) both restate the S-P1 V4 band (1237/1110/1261/833 lightningcss; 741/851/874/559 fact_stream — all verified against `HARDENING-S-P1-V4 §3.1` lines 92–95) as SIZING references, NOT gates. No gate keys on the alphaB inferred endpoints (164/51/60), which are self-flagged INFERRED. Correct.

### 2.5 — Comparator deltas use the strict plane (Q4) — PASS

SPEC §0.2 comparator table (`:120-125`) + §6 W3 gate (`:634-635`) use `track1_typed` (preserve-rich-ast) ÷ `lightningcss` full-CSSOM, same-run, with `css_comparator_plane=full-cssom`. cssparser token-scan is flaw-probe-only (SPEC §0.2 line 123; never strict admission). The recognition-only `track1_full_parse` plane (outcome A, 4-field) is explicitly NOT a typed >SOTA discharge (SPEC §0.3 lines 145–149). P3-C §3.x line 166 self-audits: "Every comparator delta uses the strict plane … never the cssparser flaw probe, never recognition-only full_parse." Correct.

### 2.6 — Owner-path / greppable-fact spot-check (all live at HEAD) — PASS

Grepped `skinny/crates/` at HEAD `f87ee713a`:

- `css_canon_bench.rs:250` — N≥50 assert present, wording exact.
- `dispatch.rs:42` `pub fn select_classifier(alphabet: &'static [u8; 64])`; `lo6_table_admissible` at `:101` (SPEC §2.1 / §9 cite `:101` — exact).
- `assembler.rs:42` `pub struct TapeBuilder<'input>`, `:71` `pub fn push_plain_offset`, `:89` `fn reserve_offsets_cold` — all present.
- `codegen/src/lib.rs:299/336/567/611` `W5C_REQUEST_FACT_PROFILES` — all four sites present (the W1 deletion targets, matching SPEC §4 `:403-404`).
- `byte_class_from_eq_set_64.rs:33` `pub fn byte_class_from_eq_set_64_neon` — present.
- 8-field equality counts (`rules=10136/style=9561/sel=9561/decls=20043`) verified against `SYNTHESIS.md:112,:137` (banked `1c5bd7a25`).

Every load-bearing owner-path citation in the SPEC and P3-A/P3-C resolves. The benched-surface discipline (grep `skinny/crates/` not `crates/core/`) is internally enforced (SPEC §0.1 `:38-40`, §9 `:809-810`).

## §3 — Dispositions (per section/wave)

| Artefact / section | Disposition | Note |
|---|---|---|
| P3-A §1 synthesis (candidate→P1/P2 trace) | ACCEPT | every candidate traces to a verified S-P2 survivor + S-P1 hot leaf |
| P3-A §2 shortlist S1–S8 + S9 conditional | ACCEPT | 8 active ≤8; L9 correctly conditional; no REJECTed candidate |
| P3-A §3 falsifiability binding | ACCEPT | predicate + named rows; inferred-endpoint trap explicitly avoided |
| P3-A §4 pre-blocked + binding conditions | ACCEPT | §4/§6 carried verbatim; R1 fold obligation recorded |
| P3-B §0 fold note + §1–§2 wave sequence (6-wave W0–W5) | ACCEPT | **D1 RESOLVED** — re-sequenced to the SPEC six-wave manifest verbatim; wave count=6; cross-artefact identity consistent |
| P3-C §2.0 W0 gate | ACCEPT | measurable baseline + gate-reject + ±15% drift floor |
| P3-C §2.1 W1 gate | ACCEPT | **D2 RESOLVED** — `1.40×` demoted to non-gating diagnostic; "NO speed admission" matches SPEC §4; no untraced number gates |
| P3-C §2.2 W2 gate | ACCEPT | R-CH2-1 JSON-byte-equal + regen-check + W5C-retire; measurable |
| P3-C §2.3 W3 gate (cross-bar) | ACCEPT | strict-plane ratio > 1.0×, named rows, N≥50; the load-bearing gate is clean |
| P3-C §2.4 W4 gate (L9 conditional) | ACCEPT | admission IS a measured post-W1 re-profile; +5% lift gate |
| P3-C §2.5 W5 gate | ACCEPT | tranche criterion + honest tailwind, no corpus-average |
| P3-C §3 binding table | ACCEPT | **D2 RESOLVED** — W1 row carries diagnostic-only, no threshold cell |
| P3-D telemetry schema | ACCEPT (CH1 scope) | columns map to gate consumers; gate-binding columns spot-checked (full schema = P3-D/CH4 scope) |
| P3-E pre-blocked ledger | ACCEPT (CH1 scope) | REJECTed-route enumeration consistent with S-P2 §4 (full enumeration = CH3 scope) |
| P3-F spec-draft | ACCEPT | 6-wave manifest matches the SPEC; correct candidate→wave map; V1 fold documented |
| SPEC §0 close condition + goalset | ACCEPT | 12 axes measurable; strict comparator; inferred-endpoint guard present |
| SPEC §0.5 goalset rows | ACCEPT | per-corpus, W0-rebaselined, no fixed-number gate |
| SPEC §1 non-negotiables | ACCEPT | strict comparator + equality-before-speed + no-deferrals enforced |
| SPEC §2 wave manifest (6-wave W0–W5) | ACCEPT | internally consistent; P3-B now conforms |
| SPEC §3 W0 | ACCEPT | measurable, no behaviour change |
| SPEC §4 W1 | ACCEPT | equality-before-speed, grep + counters; consumer-migration clause present |
| SPEC §5 W2 | ACCEPT | projection generality, R-CH2-1 JSON-byte-equal, regen-check |
| SPEC §6 W3 | ACCEPT | the cross-bar >SOTA gate — strict plane, named rows, N≥50, vs `lcss@W0` |
| SPEC §7 W4 | ACCEPT | doubly-conditional; byte-identical + measured +5% lift |
| SPEC §8 W5 | ACCEPT | close on measurement, honest residual, no paper-close |
| SPEC §9 pre-blocked ledger | ACCEPT (CH1 scope) | REJECTed set barred; binding conditions verbatim |
| SPEC §10 dispatch scope | ACCEPT | W0 authorized; W1–W5 conditional; L9 doubly-conditional |

## §4 — Counts

- Sections/waves dispositioned: **26**.
- ACCEPT: **26**.
- REVISE: **0**.
- REJECT: **0**.
- ACCEPT rate = 26/26 = **100%**.

Both V1 REVISEs (D1 wave-numbering, D2 untraced W1 `1.40×`) are folded and verified resolved against source. No new CH1 defect surfaced in V2. Every shortlist candidate traces to an S-P2 survivor and an S-P1 hot leaf; every wave's exit gate is measurable (named corpus rows + Mbps ratio / equality / grep, N≥50); every behaviour-wave threshold denominators on `SK-V17-open` (`lcss@W0`/`fs@W0`); every comparator delta uses the strict full-CSSOM plane; no REJECTed candidate is shortlisted active; all load-bearing owner-path facts resolve at HEAD `f87ee713a`. The load-bearing SPEC W3 >SOTA gate is CH1-clean.

## §5 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (scope matrix), §3 (CH1 charge), §8 (axes).
- `restart/skinny/tranches/sk-v17/SPEC.md` §0–§10 (the contract under review).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3f}.md` (V2 cohort).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V1/CH1.md` (the V1 defects D1/D2 whose fold this cycle verifies).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` §3 (L1–L9), §4 (REJECTed `:252-286,:343`), §6 (binding conditions). Commit `f87ee713a`.
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` §3.1 (bench medians, lines 92–95), §3.3 (hot leaves, lines 143–159), §3.4 (lever order, line 187). Commit `0ae1caa52`.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions, 8-field equality `:112,:137`). Commit `6496fecae`.
- Live source at HEAD `f87ee713a`: `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:250`; `bbnf-simd/src/dispatch.rs:42,101`; `runtime/src/tape/assembler.rs:42,71,89`; `codegen/src/lib.rs:299,336,567,611`; `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`.
