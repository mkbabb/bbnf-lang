# SK-V17 S-P3 CHALLENGE — CH1 CORRECTNESS (V3)

Lens: CH1 CORRECTNESS. Cycle: V3. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` + `restart/skinny/tranches/sk-v17/SPEC.md`.
Authority: PASS-3-SYNTHESIS-PLAN §3 (CH1), ORCHESTRATOR §3W/§3Z. Master HEAD `f87ee713a` (verified `git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §0 — CH1 charge

PASS-3 §3 CH1 asks four falsifiable questions, plus the dispatch focus:

1. Does every shortlist candidate trace to an S-P2 survivor (L1–L9) and through it to an S-P1 hot leaf?
2. Is every falsifiability gate measurable — named corpus rows + concrete Mbps thresholds, not prose?
3. Does every wave's exit gate compare against the `SK-V17-open` baseline?
4. Do the comparator deltas in the gates use the strict plane?

Dispatch focus: every wave cites a real S-P2 candidate (L1–L9) + a measurable exit gate (named corpus rows + Mbps threshold vs lightningcss N≥50); no REJECTed candidate shortlisted; thresholds trace to the S-P1 canonical bench.

## §1 — V3 posture: a citation re-key cycle over a V2-100%-clean substrate

V2 CH1 returned 26/26 ACCEPT with both V1 REVISEs (D1 wave-numbering, D2 untraced W1 `1.40×`) verified folded against source. V3 is, by P3-B's own §0 fold note (`p3b:10-24`), a **SPEC-line-citation re-key cycle**: the content topology is unchanged; the SPEC grew when the W1/W2 consumer enumeration + the R-CH2-1 byte-equal check folded in, so every `SPEC.md:<line>` anchor across the cohort shifted. P3-B §0 declares it re-keyed those anchors and routes the residual reconciliation to P3-A + P3-C. CH1 therefore re-runs the four falsifiable questions from scratch against source AND audits whether the V3 re-key actually landed in P3-A/P3-C.

## §2 — Independent V3 verification (every check executed against source at HEAD)

### 2.1 — Candidate → S-P2 → S-P1 trace (Q1) — PASS

Every P3-A row maps 1:1 to a LOCKED S-P2 survivor and names its S-P1 antecedent; each antecedent re-verified against `HARDENING-S-P1-V4 §3.3` and the S-P2 §3 pool (`HARDENING-S-P2-V3-CONSOLIDATED.md:99-251`):

| P3-A | L | S-P1 antecedent (verified §3.3) |
|---|---|---|
| S1 | L1 eq-set classifier | `find_component_delim` 59.24% + `consume_balanced_at` 10.31% = ~69% scan leaf |
| S2 | L2 `push_plain_offset` | `emit_fact_stream` 25.01% + ~64% alloc floor |
| S3 | L3 lazy `ValueRef` | String-materialization floor `emit_fact_stream` carries |
| S4 | L4 tokenize-once | 2–3× re-walk `find_component_delim`/`find_colon_before`/`parse_declaration` |
| S5 | L5 `comment_body_mask_64` | comment-skip arm of the ~69% scan leaf |
| S6 | L6 `bracket_depth_mask_64` | `consume_balanced_at` 10.31% recursion |
| S7 | L7 one-shot reserve | ~64% alloc floor grow-churn |
| S8 | L8 sparse-flag side-table | mechanism for S3 (indirect, guarded) |
| S9 | L9 commit-by-construction (CONDITIONAL) | recognition-control 28.87%+2.45%, classed structural-recognition-control NOT rollback |

The L9 conditionality is correctly carried in every artefact: S-P1 measured ZERO speculative-rollback self-time, so the 28.87%+2.45% figure is explicitly NOT a rollback antecedent (P3-A §2 S9, P3-C §2.4, P3-B Constraint B, SPEC §7 `:670-680`). No candidate is grounded on a phantom leaf.

### 2.2 — No REJECTed candidate shortlisted (dispatch focus) — PASS

`HARDENING-S-P2-V3 §4` REJECTed set = {CF-4a/C5/C-B3/G4 orphan udot · CF-4b/C6 i8mm · FNV/hex · asmjson FSM (x86) · lo6 `classify_tbl4`-on-CSS · D6 second substrate} (verified `:252-286,343`). The active shortlist S1–S8 (+ S9 conditional) contains none of them. udot/i8mm appear only in the SPEC §9 barred list. The lo6-on-CSS REJECT is correctly inverted into a positive constraint (the `;{`→slot-59 `& 0x3f` collision forces the eq-set fan — verified `dispatch.rs:101` `lo6_table_admissible` present at HEAD). The eligible-set / barred-set partition is enforced verbatim in P3-A §4 and SPEC §9.

### 2.3 — Gates measurable, not prose (Q2) — PASS

Every SPEC wave exit gate (§3–§8) resolves to a number from the bench, a grep over `skinny/crates/`, or an equality/counter assertion. Verified live anchors at HEAD:

- `css_canon_bench.rs:250` `assert!(n >= 50, "N must be >= 50 (SK-V17 telemetry-honesty gate)")` — the N≥50 gate is real.
- `:260/:266` emit `ROW corpus=… median_mbps=…` — the schema row gates key on.
- `css_l4_corpus.rs:22-54` — the four sha256-pinned corpora (bootstrap/tailwindcss/material/animate).

- **W0 §3 (`:368-375`):** `SK-V17-open` captured; per-corpus lightningcss full-CSSOM median N≥50; `gate-json` rejects 4 malformed-row classes; JSON 51/51 ±1.0%. Measurable.
- **W1 §4 (`:461-476`):** `tape_activated` grep non-zero; `PayloadArena` write/alloc counters; `w5c_profile_array_retired` grep zero; EXACT 8-field equality (`rules=10136/style=9561/sel=9561/decls=20043`); NO speed admission. Measurable substrate-truth gate.
- **W2 §5 (`:547-571`):** `lazy_view_generated`; R-CH2-1 JSON-rider-byte-equal diff; `css_rich_ast_preserved` (value-plane population parity); per-corpus typed median ≥ −2.0% vs W1; `regen --check`. Measurable.
- **W3 §6 (`:637-648`):** **≥1 regular corpus (animate OR bootstrap) `delta_vs_lightningcss > 1.0×` at N≥50 cold median, `full-cssom` plane** — the load-bearing cross-bar gate: concrete ratio, named rows, N≥50, vs `lcss@W0`. Measurable.
- **W4 §7 (`:708-716`):** byte-identical tape + measured `≥ +5%` N≥50 lift vs the W3 plane on the gated corpus; a lift below +5% disposes L9 not-warranted. The admission gate is itself a measured post-W1 re-profile. Measurable.
- **W5 §8 (`:750+`):** `regen --check` 9/9 exit 0; ≥1 regular corpus crosses re-confirmed; tailwind admit-or-honest-REDRESS, no corpus-average. Measurable.

No SPEC exit gate closes on "wired"/"integrated"; SPEC §1 (`:250-251`) forbids it; P3-C §3.x (`:165-168`) self-audits it.

### 2.4 — Exit gates compare against `SK-V17-open` (Q3) — PASS

Every behaviour-wave threshold denominator is the W0-recorded `lcss(corpus)@W0` / `fs@W0` / `SK-V17-open` median (P3-C §2/§3 binding table; SPEC §0.5). The inferred-endpoint trap is closed: P3-A §3 (`:177-184`), P3-C §1.2 (`:28-37`), and P3-B §3 (`:450-453`) all restate the S-P1 V4 band (1237/1110/1261/833 lightningcss) as a SIZING reference only, and explicitly bar any gate keyed on the alphaB inferred endpoints (164/51/60), which are self-flagged INFERRED. Correct.

### 2.5 — Comparator deltas use the strict plane (Q4) — PASS

SPEC §6 W3 gate (`:639-641`) and the P3-C §2/§3 tables use `track1_typed` (preserve-rich-ast) ÷ `lightningcss` full-CSSOM, same-run, `css_comparator_plane=full-cssom`. cssparser token-scan is flaw-probe-only (never strict admission); recognition-only `track1_full_parse` (outcome A, 4-field) is explicitly NOT a typed >SOTA discharge. P3-C §3.x (`:166`) self-audits this. Correct.

### 2.6 — Owner-path / greppable-fact spot-check (all live at HEAD) — PASS

Grepped `skinny/crates/` at HEAD `f87ee713a`: `css_canon_bench.rs:250` (N≥50 assert, wording exact); `dispatch.rs:42` `select_classifier(alphabet: &'static [u8; 64])` + `:101` `lo6_table_admissible`; `assembler.rs:42` `TapeBuilder`, `:71` `push_plain_offset`; `codegen/src/lib.rs:299/336/567/611` `W5C_REQUEST_FACT_PROFILES` (all four W1-deletion targets present, matching SPEC §4 `:403-404`); `byte_class_from_eq_set_64.rs:33`. Every load-bearing owner-path citation resolves.

## §3 — D1(V3): SPEC-line citation drift in P3-A and P3-C — REVISE

P3-B's V3 fold note (`p3b:18-24`) declares it re-keyed the cohort's stale `SPEC.md:` line citations after the SPEC grew (W1 PRUNE `:382→:390`, W2 projection `:466→:494`, W1 NO-speed-admission `:447→:475`, post-W1 L9 gate `:616,637→:666-672,690,842`) and routes the reconciliation to P3-A + P3-C. P3-A's W2/L4 cite WAS re-keyed (`SPEC.md:494-499`, verified correct: SPEC `:494` is the W2 section head, L4 named at `:498`). But three citation families in P3-A and P3-C still carry the **pre-grow (stale) anchors** — the fix landed in P3-B only:

1. **P3-C `:83`, `:88`, `:155`, `:168`** cite `SPEC.md:447` for "NO speed admission this wave." Verified: the unique "NO speed admission this wave" line is **`SPEC.md:475`** (`grep -n` returns exactly one hit at `:475`); SPEC `:447` is an unrelated clause inside the W1 tasks ("L7 is gated behind the tape; if W3 has not landed the index, L7 sizes from a conservative byte-proportional bound"). A reader resolving the P3-C W1 no-speed gate against `:447` lands on the L7 sizing clause, not the no-speed-admission rule.

2. **P3-A `:156`, `:160`, `:218`** cite `SPEC.md:616,637` for the W4 **post-W1 L9 admission gate**. Verified: SPEC `:613-618` is the **W3 entry gate** ("W0/W1/W2 admitted; … W3 plan presents the RE-PROFILE …"), NOT the W4 L9 gate. The actual W4 L9 admission gate is SPEC **§7 `:670-680`** (entry) / `:695-697` / exit `:708-716`. P3-A points the L9 admission gate at the wrong wave's lines.

3. **P3-A `:131`, `:135`** cite `SPEC.md:388,391` for "L7 lands in W1." Verified: SPEC `:388` is the W0-downstream line ("W0 rejection blocks W1–W5"); SPEC §4 (W1) begins at `:390`; the L7-W1-default-body task is at `:446-448`. The `:388/:391` anchors do not point at any L7 content.

**Severity.** REVISE, not REJECT. The candidate→wave **placement** and the gate **predicates** are correct and consistent everywhere (L4→W2, L7→W1, L9→W4 post-W1, W1 no-speed-admission) — I verified each against the SPEC content, not the anchors. Only the line numbers drifted, and only in P3-A/P3-C (P3-B's own anchors and the P3-A W2 cite are correct). But CH1 is the correctness lens: a falsifiability gate whose SPEC-line citation resolves to an unrelated clause is not fully traceable — the dispatch focus requires each gate to "trace to the S-P1 canonical bench" and the SPEC, and a wrong anchor breaks that trace for the executing triumvirate. This is precisely the citation-re-key obligation P3-B §0 declared discharged but left orphaned in P3-A/P3-C.

**Concrete fix.**
- `p3c-falsifiability-gates.md:83,88,155,168` — replace `SPEC.md:447` with `SPEC.md:475` (the unique "NO speed admission this wave" line) for the W1 no-speed-admission citation. (The L7-gated-behind-tape sense of `:447`, if intended anywhere, is a separate citation; the no-speed-admission rule is `:475`.)
- `p3a-candidate-shortlist.md:156,160,218` — replace `SPEC.md:616,637` with `SPEC.md:670-672,695` (the W4 §7 L9 post-W1 admission gate); `:616-617` is the W3 entry-gate re-profile clause and must not be cited as the W4 L9 gate.
- `p3a-candidate-shortlist.md:131,135` — replace `SPEC.md:388,428–430,391` with `SPEC.md:390,396,446-448` (W1 §4 head + the L2/L7 candidate line + the L7 W1-default-body task).

This is the V3 twin of the V2 D1 (the orphan-REVISE class where one artefact's re-key did not propagate to the cohort); folding it makes every gate's SPEC anchor resolve to its own clause.

## §4 — Dispositions (per section/wave)

| Artefact / section | Disposition | Note |
|---|---|---|
| P3-A §1 synthesis (candidate→P1/P2 trace) | ACCEPT | every candidate traces to a verified S-P2 survivor + S-P1 hot leaf |
| P3-A §2 shortlist S1–S8 + S9 conditional | ACCEPT | 8 active ≤8; L9 correctly conditional; no REJECTed candidate |
| P3-A §2 S4/S7/S9 SPEC line citations | REVISE | D1(V3) drift: `:388,391` (S7) and `:616,637` (S9) point at wrong-wave clauses; re-key to `:390,396,446-448` and `:670-672,695` |
| P3-A §3 falsifiability binding | ACCEPT | predicate + named rows; inferred-endpoint trap explicitly avoided |
| P3-A §4 pre-blocked + binding conditions | ACCEPT | §4/§6 carried verbatim; R1 fold obligation recorded |
| P3-B §0 fold note + §1–§2 wave sequence (6-wave W0–W5) | ACCEPT | re-keyed correctly within P3-B; placement content unchanged and correct |
| P3-C §2.0 W0 gate | ACCEPT | measurable baseline + gate-reject + ±15% drift floor |
| P3-C §2.1 W1 gate | REVISE | content correct (NO speed admission, diagnostic-only); SPEC anchor `:447` is stale — re-key to `:475` (4 sites) |
| P3-C §2.2 W2 gate | ACCEPT | R-CH2-1 JSON-byte-equal + regen-check + W5C-retire; measurable |
| P3-C §2.3 W3 gate (cross-bar) | ACCEPT | strict-plane ratio > 1.0×, named rows, N≥50; the load-bearing gate is clean |
| P3-C §2.4 W4 gate (L9 conditional) | ACCEPT | admission IS a measured post-W1 re-profile; +5% lift gate |
| P3-C §2.5 W5 gate | ACCEPT | tranche criterion + honest tailwind, no corpus-average |
| P3-C §3 binding table | ACCEPT | W1 row carries diagnostic-only, no threshold cell |
| P3-D telemetry schema (CH1 scope) | ACCEPT | gate-binding columns map to gate consumers |
| P3-E pre-blocked ledger (CH1 scope) | ACCEPT | REJECTed-route enumeration consistent with S-P2 §4 |
| P3-F spec-draft | ACCEPT | 6-wave manifest matches the SPEC; correct candidate→wave map |
| SPEC §0 close condition + goalset | ACCEPT | axes measurable; strict comparator; inferred-endpoint guard present |
| SPEC §1 non-negotiables | ACCEPT | strict comparator + equality-before-speed + no-deferrals enforced |
| SPEC §2 wave manifest (6-wave W0–W5) | ACCEPT | internally consistent; wave count 6 ≤ 12; shortlist 8 ≤ 8 |
| SPEC §3 W0 | ACCEPT | measurable, no behaviour change |
| SPEC §4 W1 | ACCEPT | equality-before-speed, grep + counters; consumer-migration clause present |
| SPEC §5 W2 | ACCEPT | projection generality, R-CH2-1 JSON-byte-equal, regen-check |
| SPEC §6 W3 | ACCEPT | the cross-bar >SOTA gate — strict plane, named rows, N≥50, vs `lcss@W0` |
| SPEC §7 W4 | ACCEPT | doubly-conditional; byte-identical + measured +5% lift; post-W1 antecedent |
| SPEC §8 W5 | ACCEPT | close on measurement, honest residual, no paper-close |
| SPEC §9 pre-blocked ledger (CH1 scope) | ACCEPT | REJECTed set barred; binding conditions verbatim |
| SPEC §10 dispatch scope | ACCEPT | W0 authorized; W1–W5 conditional; L9 doubly-conditional |

## §5 — Counts

- Sections/waves dispositioned: **26**.
- ACCEPT: **24**.
- REVISE: **2** (P3-A §2 S7/S9 SPEC-line citations; P3-C §2.1 W1 `:447`→`:475`) — both the same D1(V3) citation-drift defect, two-artefact orphan.
- REJECT: **0**.
- ACCEPT rate = 24/26 = **92.3%**.

The two REVISEs are one root defect (SPEC-line citation drift left orphaned in P3-A/P3-C after P3-B's V3 re-key) with a deterministic three-line fix. No substantive correctness failure: every shortlist candidate traces to an S-P2 survivor + an S-P1 hot leaf; no REJECTed candidate is shortlisted; every wave's exit gate is measurable (named corpus rows + Mbps ratio / equality / grep, N≥50); every behaviour-wave threshold denominators on `SK-V17-open`; every comparator delta uses the strict full-CSSOM plane; the load-bearing W3 >SOTA gate is CH1-clean; all owner-path facts resolve at HEAD `f87ee713a`. Folding the D1(V3) re-key restores full anchor traceability and should return CH1 to 100%.

## §6 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (scope matrix), §3 (CH1 charge), §8 (axes).
- `restart/skinny/tranches/sk-v17/SPEC.md` §0–§10 — verified anchors: `:250-251` (no-paper-close), `:264-272` (manifest), `:390` (W1 head), `:446-448` (L7 W1 task), `:475` (unique NO-speed-admission), `:494-499` (W2/L4), `:613-618` (W3 entry), `:637-648` (W3 >SOTA gate), `:668-716` (W4 L9 gate).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3f}.md` (V3 cohort).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V2/CH1.md` (V2 100% baseline; V1 D1/D2 folds verified there).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` §3 (L1–L9 `:99-251`), §4 (REJECTed `:252-286,343`), §6 (binding conditions `:315-330`). Commit `f87ee713a`.
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` §3.1/§3.3/§3.4 (bench medians, hot leaves, lever order). Commit `0ae1caa52`.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1/§0.4/§0.5 (close conditions, 8-field equality `:112,137`). Commit `6496fecae`.
- Live source at HEAD `f87ee713a`: `bbnf-bench/src/bin/css_canon_bench.rs:250,260,266`; `bbnf-bench/src/css_l4_corpus.rs:22-54`; `bbnf-simd/src/dispatch.rs:42,101`; `runtime/src/tape/assembler.rs:42,71`; `codegen/src/lib.rs:299,336,567,611`; `bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`.
