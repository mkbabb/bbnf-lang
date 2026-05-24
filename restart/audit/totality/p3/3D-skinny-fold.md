---
agent: 3D
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-23T22:30:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: "n/a — fold doc"
proposed_deltas_count: 14
prior_cycle_normalisation:
  note: "T-P3 cohort cycle counter normalised from V4 (V3-baseline + per-artefact increment carried from prior dispatches) to V2 (V1 dispatch + this V2 fold) for cohort coherence with 3A/3B/3F. Other artefacts: 3A/3B/3F at V1; 3C/3D/3E previously at V4 — now realigning under the T-P3 V1→V2 fresh-cycle convention per F-V2 V2 fold dispatch + CH1 §5 open question."
delta_summary:
  carried_from_prior_cycle:
    - FOLD-3D-001
    - FOLD-3D-002
    - FOLD-3D-003
    - FOLD-3D-004
    - FOLD-3D-005
    - FOLD-3D-006
    - FOLD-3D-007
    - FOLD-3D-008
    - FOLD-3D-009
    - FOLD-3D-010
    - FOLD-3D-011
    - FOLD-3D-012
    - FOLD-3D-013
    - FOLD-3D-014
  removed: []
  answered: []
  newly_added: []
prior_cycle_dispositions_folded:
  accepted:
    - G-T-P3-V3-CH1
    - G-T-P3-V3-CH2
    - G-T-P3-V3-CH3
    - G-T-P3-V3-CH4
    - G-T-P3-V3-CH5
    - G-T-P3-V3-CH6
    - G-S-P3-V3-COHORT-LOCK
    - G-T-P1-V5-LOCK
    - G-T-P2-V3-LOCK
  rejected: []
  revised:
    - F-V2-CH6-3D
---

## Executive Summary

3D distils the SK-V{1..14} skinny journey into a monotonic fold for the
totality V1 spec amendment packet (`restart/prompts/totality/PASS-3-SYNTHESIS.md:53`
+ `:213`). Per §8.4 the direction is strict: skinny informs totality;
totality never dictates back to a live skinny iteration. The V2 cycle
(cohort-normalised from prior V4 per-artefact counter to V1+1 = V2,
aligning with 3A/3B/3F) carries FOLD-3D-001..014 byte-identical forward
and folds the F-V2-CH6-3D REVISE (SK-V12 W1a substrate-prerequisite vs
SK-V13 W1b CSS L4 row-admit reconciliation cross-cite between §1 row 8
and §2 row 3). The V1-cycle V4 (per-artefact counter) absorbed the
S-P3 §3Z COHORT LOCK at `867b0cd0b`
(`restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:61`-`75`)
which crystallises the SK-V14 SPEC 12-wave plan W0..W11
(`restart/skinny/tranches/sk-v14/SPEC.md:237`-`248`), the 8-candidate
shortlist C1..C8
(`restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:57`-`316`),
the Stage-A authoring queued for W10
(`HARDENING-S-P3-V3-CONSOLIDATED.md:412`-`424`), the
F-V2-P1ABC-RERECORD Stage-0 W10 UNCONDITIONAL binding
(`SPEC.md:982` + `:990` + `:1000`; `HARDENING-S-P3-V3-CONSOLIDATED.md:426`-`436`),
and the CH4 6-class cost-neutrality taxonomy
(`HARDENING-S-P3-V3-CONSOLIDATED.md:234`-`255`). The V1 fold packet
(FOLD-3D-001..010) carries forward unchanged; four NEW folds
(FOLD-3D-011..014) absorb the SK-V14 SPEC 12-wave plan and the CH4
6-class cost-neutrality discipline. 14 folds total; 0 removed; 0
re-opened; 7 prior-cycle CHALLENGE dispositions folded ACCEPT.

## V2 Delta Summary

| disposition | count | notes |
|---|---:|---|
| Carried from prior T-P3 cycle | 14 | FOLD-3D-001..014 all land byte-identical from V1 cycle (per-artefact V4 counter, now cohort-normalised to V2); per §8.4 the fold direction is monotonic and prior wins are not re-litigated by totality. |
| Removed | 0 | No prior fold is removed; CH3 REGRESSION refused any rejected-route reopen (`PASS-3-SYNTHESIS.md:113`-`116`). |
| Answered | 0 | Open questions route to 3C / 3E / 3F per `PASS-3-SYNTHESIS.md:86`-`90`; 3D is the evidence-fold, not the disposition surface. |
| Revised | 1 | F-V2-CH6-3D REVISE folded: §1 row 8 (SK-V12 W1a substrate prerequisite) ↔ §2 row 3 (SK-V13 W1b CSS L4 row-admit AUDIT-FALSIFIED) reconciling cross-cite added so a downstream reader cannot cite §1 row 8 as forward authority while §2 row 3 marks it disproved. Both rows are correct under different framings: §1 row 8 carries the `escape_mask_64` prerequisite primitive substrate-target (`admitted_fact_output`); §2 row 3 disposes of the 7-provider hand-written CSS L4 admission attempt. Mirrors 3B Wave Classification Ledger CSS L4 row at `3B-master-plan-reconciliation.md:99` reseat-dependency wording. |
| Newly added | 0 | V2 fold is a coherence/reconciliation pass; no new fold proposals. |

## §1 — Skinny wins → V1-spec-authoritative (audit pack v4 LOAD-BEARING confirmation)

The audit-survival census per S-P0 `SYNTHESIS-AUDIT-OVERFIT.md`
(74-finding ledger) and S-P1 `p1e-hot-leaf-attribution.md` yields the
following durable wins. Each row is V1-spec-authoritative; the lock
disposition column points the consuming 3C/3E lock hunk.

| skinny win | audit-pack v4 status | evidence | V1-spec routing | 3C/3E target |
|---|---|---|---|---|
| W5 `bbnf-regex` extraction crate (LOAD-BEARING per audit pack v4 §1). | LOAD-BEARING; consumed from `ir::nullability`, `passes::recognizers::regex_first_bytes`, `passes::extract::span_kind`. | `skinny/REDRESS.md:4081`-`4083`; `1D-skinny-lessons.md:124`; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:179`. | V1-AUTH: bbnf-regex is grammar-neutral substrate; consume HIR facts in decision-engine resolver, not opaque pattern strings. | 3C-L10-decision-engine-cost (LAC-2F-03); 3E-D02 resolver pipeline. |
| W6 e-graph active cost extraction (LOAD-BEARING extraction-only per audit pack v4 §1). | LOAD-BEARING; extraction reaches `CostFacts.chosen` consumed by `codegen::lower::rust::lower_to_rust`; emitted templates do NOT yet render the chosen candidate. | `skinny/REDRESS.md:4106`-`4115`; `1D-skinny-lessons.md:126`; `SYNTHESIS.md:180`. | V1-AUTH: e-graph cost active in decision-engine; runtime consumer gated to C-4 (PRUNE-5) per SK-V14 W7. | 3C-L10-decision-engine-cost (T2A-LAC-05 + LAC-2D-01/02); 3E-D02. |
| W7 CSP solver cascade (LOAD-BEARING per audit pack v4 §1+§2). | LOAD-BEARING; 5-constraint fail-closed wiring at `crates/codegen/src/lower/rust.rs:37`-`89`; `DecisionCspFacts` carried in `CostFacts`. | `skinny/REDRESS.md:4149`-`4158`; `SYNTHESIS-AUDIT-OVERFIT.md:33`-`38`; `1D-skinny-lessons.md:127`. | V1-AUTH: CSP cascade is the decision-engine spine; hardcoded P1-P8 fails closed for JSON/CSS/Sheets/BBNF-self post-W7. | 3C-L10-decision-engine-cost; 3E-D02; SPEC §1:229. |
| OffsetFlags + Tape generic substrate + GrammarConfig private generated config. | PROVED historically through SK-V12 W1a; preserved in SK-V14 baseline. | `skinny/REDRESS.md:3555`-`3601`; `SYNTHESIS.md:178`-`187`; `1D-skinny-lessons.md:117`. | V1-AUTH: single-substrate union under Lock 1 (substrate-target ∈ {local_temp_only, existing_tape, direct_sink, admitted_fact_output}). | 3C-L01-substrate-ceiling-history (LAC-1E-01 + T2A-LAC-01 + LAC-2D-05); SPEC §1:223. |
| Generated `parse_direct` + `parse_real_typed_*` JSON parsers (real codegen, not include_str). | PROVED per audit pack v4 §3.1 + §4.1. | `1D-skinny-lessons.md:132`; `SYNTHESIS.md:184`-`185`. | V1-AUTH: codegen output is the only legal per-grammar runtime; hand-written providers Lock 14 violation. | 3C-L14-generated-output-and-per-wave-gate (LAC-1E-08 + LAC-1E-11). |
| `bbnf-simd` 52-file grammar-neutral primitive surface. | PROVED per audit pack v4 A3 §4; SK-V5 PC-008 verify-before-rederive obligation open. | `1D-skinny-lessons.md:131`; `SYNTHESIS.md:182`; `1F-past-corpora.md:74` + `:158`. | V1-AUTH: bbnf-simd is grammar-neutral primitive library; G-SIMD-GRAMMAR-POLICY gates non-JSON consumers. | 3C-L16-manifest-checkasm-orphans (LAC-2B-03 G-SIMD-GRAMMAR-POLICY); SPEC §1:228. |
| Cold/lazy JSON payload arena (0/0 payload counters preserved through SK-V14). | PROVED JSON-empirical; non-JSON generalisation pending. | `1D-skinny-lessons.md:118`; `skinny/REDRESS.md:134`. | V1-AUTH: cold-materialisation rule generalises to all grammars via generated grammar-owned flags. | 3C-L01-substrate-ceiling-history; 3E-D05 (fact streams = output planes). |
| SK-V12 W1a `declaration_values_extended` admitted row (one strict same-plane fact-stream row) — historical-row-evidence carrying forward as the `escape_mask_64` prerequisite primitive. **Cross-cite: SEE §2 row 3 below** — the SK-V13 W1b CSS L4 admission attempt (7-provider hand-written + fake `@generated` header + fixture-lookup short-circuits) is AUDIT-FALSIFIED at SK-V14 audit-zero; the surviving carry-forward at THIS row is the W1a `escape_mask_64` checkasm-backed primitive substrate (see §1 row 9 below), NOT the SK-V13 W1b CSS L4 admission. | ADMITTED-EVIDENCE per `RESULTS.md:94` as **historical-row-evidence-at-SK-V13 + AUDIT-FALSIFIED-at-SK-V14-audit-zero + reseat dependency on SK-V14 SPEC W8 R6** (mirrors 3B Wave Classification Ledger CSS L4 row at `3B-master-plan-reconciliation.md:99` reseat-dependency wording for the other 54 pending waves); the carry-forward is the **prerequisite primitive** (`escape_mask_64` + substrate-target = `admitted_fact_output`), NOT a forward authority for CSS L4 row admission. | `skinny/RESULTS.md:94`; `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:50`; `1D-skinny-lessons.md:122`; SK-V14 audit-zero reversal at `restart/skinny/tranches/sk-v14/SYNTHESIS.md:191`-`198`; reseat anchor `restart/skinny/tranches/sk-v14/SPEC.md` §11 W8 R6. | V1-AUTH: admitted evidence as **fact-stream prerequisite primitive substrate-target** (`admitted_fact_output`); NOT a sixth BackendShape; NOT full CSS parity (24 rows AUDIT-FALSIFIED at SK-V14 audit-zero — see §2 row 3); reader MUST NOT cite this row as forward authority for CSS L4 admission post-SK-V14 audit reversal. | 3E-D05 (output plane); 3C-L08-row-plane-bench-feed; 3B Wave Classification Ledger CSS L4 row at `:99` (reseat dependency on SK-V14 SPEC W8 R6). |
| Generated `bbnf-simd::escape_mask_64` checkasm-backed primitive (correctness prerequisite). | PROVED prerequisite-only; not production admission until consumed. | `skinny/REDRESS.md:3603`-`3632`; `1D-skinny-lessons.md:131`. | V1-AUTH: Lock 16 prerequisite class; zero-orphan disposition required. | 3C-L16-manifest-checkasm-orphans (LAC-1E-10 + LAC-2B-04). |

## §2 — Skinny rejections → locks-strengthening evidence (cross-ref 3C)

The audit-pack v4 cross-axis ledger yields the following skinny
rejections. Each becomes locks-strengthening evidence per §8.4 (not a
JSON-narrowing amendment per §8.3); the consuming 3C hunk is named.
Per CH3 REGRESSION
(`PASS-3-SYNTHESIS.md:113`-`116`) none of these are reopen
candidates; per `[abrogate-before-patch]` the two-or-more-reopen rule
forces DELETE rather than patch.

| skinny rejection | rejection class | evidence | locks-strengthening routing | 3C target |
|---|---|---|---|---|
| SK-V9 W3 retired union-substrate hypothesis (REDRESS 96 retained class-column / 97 streaming structural cursor / 98 class-lane-only). | PERMANENT pre-block (audit pack v4 + S-P0 dispatch §0.3 §3Z LOCK ratification). | `skinny/REDRESS.md:2910`-`2940`; `SPEC.md:829` (REDRESS 96-98 PERMANENT-PRE-BLOCK top-level promotion); `SPEC.md:1109` (§15 global watch-list); `HARDENING-S-P3-V3-CONSOLIDATED.md:333`-`346`. | Lock 1 v+1 union material-differential rule: no union replay without (a) changed data movement, (b) changed consumer shape, (c) measured row outcome. | 3C-L01-substrate-ceiling-history (LAC-2B-05 + LAC-2B-07 + LAC-2D-05 + LAC-2E-04). |
| SK-V11 W1b non-JSON codegen baseline rejected (grammar-name leak `backend.grammar_name == "json"`). | DISPROVED-AND-RECTIFIED-BUT-INCOMPLETE; A3 census shows 30 Lock-14 violations across 8 per-grammar provider modules. | `skinny/REDRESS.md:3311`-`3338`; `SYNTHESIS-AUDIT-OVERFIT.md:23` + `:141`-`:145`; `SYNTHESIS.md:198`; `1D-skinny-lessons.md:123`. | Lock 14 v+1 forward invariant: ZERO new `.rs` files in `skinny/crates/{codegen,runtime,passes,bbnf,grammar}/src/` per new grammar; per-wave name+shape leak gate. | 3C-L14-generated-output-and-per-wave-gate (LAC-1E-08 + LAC-2C-01/02/03/05). |
| SK-V12 W1b CSS admission AUDIT-FALSIFIED (7/7 CSS L4 providers hand-written + fake `@generated` header + 4 fixture-lookup short-circuits). **Cross-cite: SEE §1 row 8 above** — this DISPROVED row is the SK-V13 W1b CSS L4 admission attempt under SK-V14 audit-zero; it does NOT retract the §1 row 8 SK-V12 W1a `escape_mask_64` prerequisite primitive carry-forward, which survives as fact-stream substrate evidence under different framing (admitted-as-substrate-prerequisite vs admitted-as-row-admit-for-CSS-parity). Both rows are correct: §1 row 8 carries the substrate-target prerequisite; §2 row 3 here disposes of the row-admit attempt. | DISPROVED (audit pack v4 §1 Claim 1) — **scope: the SK-V13 W1b CSS L4 row-admit attempt; NOT the SK-V12 W1a `escape_mask_64` substrate prerequisite preserved in §1 row 8 as fact-stream output-plane substrate-target evidence**. | `SYNTHESIS.md:191`-`192`; `SYNTHESIS-AUDIT-OVERFIT.md:122`-`134`; `1D-skinny-lessons.md:122`; cross-cite §1 row 8 above (substrate prerequisite preserved); 3B Wave Classification Ledger row `3B-master-plan-reconciliation.md:99` (B.W4 `refuted-at-HEAD` reseat dependency on SK-V14 SPEC W8 R6). | Lock 14 v+1 + R4 obligation: regen-css xtask + grammar-derived providers + production CSS L4 corpora (~960 KB); §1 row 8 substrate prerequisite (`escape_mask_64`) remains LOAD-BEARING for the future re-admission, but the row-admit itself MUST come from generated providers, not the hand-written 7-provider trap. | 3C-L14 (LAC-1E-11 + T2A-LAC-04); SPEC §5 W2 + §6 W3; 3B `:99` (reseat on SK-V14 SPEC W8 R6). |
| SK-V13 W14.1-5 parse-only admits AUDIT-FALSIFIED as gate-relabel-only (parser unchanged). | DISPROVED baseline (strict REDRESS PASS-ADMIT cardinality: 5 parse_only + 4 direct + 7 typed + 24 CSS = 40 admit rows). | `skinny/REDRESS.md:4765`-`4917`; `SYNTHESIS.md:54`-`84`; `1D-skinny-lessons.md:119` + `:140`. | Lock 8 row-plane SOTA: parse_only requires distinct Skipper-class comparator + per-iter equality oracle; no gate-relabel admit. | 3C-L08-row-plane-bench-feed (LAC-1E-04 + T2A-LAC-02 + LAC-2F-04); SPEC §10 W7 + §13 W10. |
| SK-V13 W11.1/W11.3 direct admits AUDIT-FALSIFIED (comparator misbinding: `sonic_rs::from_slice::<Value>` eager DOM, not strict per-corpus struct deser). | DISPROVED as currently bound; structural prospect retained pending R1+R2. | `SYNTHESIS-AUDIT-OVERFIT.md:90`-`97`; `1D-skinny-lessons.md:120` + `:146`. | Lock 8 row-plane SOTA + comparator-plane provenance rule per BENCH Section 8. | 3C-L08-row-plane-bench-feed; SPEC §4 W1. |
| SK-V13 W13.1/W13.3/W13.4/W15.1 typed admits AUDIT-FALSIFIED (4 of 7 profile as `missing-product-surface`: admit row exists, no generated typed parser). | DISPROVED (admit-vs-profile contradiction). | `p1e-hot-leaf-attribution.md:162`-`169`; `1D-skinny-lessons.md:121` + `:153`. | Lock 8 row-plane SOTA: per-corpus typed struct deser comparator before any typed row admit. | 3C-L08-row-plane-bench-feed; SPEC §12 W9. |
| SK-V13 W13.5..W13.9 MEASURED-REJECT / CORRECTNESS-REJECT (NOT PASS-ADMIT, NOT part of audit-falsified admit tally). | MEASURED-REJECT (distinct from broader audit overlay reading; MUST NOT be treated as reopen candidates). | `skinny/REDRESS.md:4621`/`:4645`/`:4674`/`:4704`/`:4734`; `1D-skinny-lessons.md:140` (V2 fold CH3-005 split #1). | Lock 1 + Lock 16: rejected-route ledger; reopen requires fresh material differential per Lock 1 v+1 triad. | 3C-L01 + 3C-L16; SPEC §15 watch-list. |
| Pattern H 67 hand-written per-grammar runtime files (V13 64 + 3 css_pretty co-derivation). | DISPROVED (Lock 14 violation: 9 grammar directories under `crates/core/src/runtime/`; PRUNE-4 9 sub-waves not 8). | `SYNTHESIS-AUDIT-OVERFIT.md:200`-`231`; `1D-skinny-lessons.md:130` + `:144`. | Lock 14 v+1 + C-1 PRUNE-3+PRUNE-4: replace `RuntimeProvider` enum with trait-based dispatch; collapse 8 providers to ONE generic generator template; PRUNE-4 9 sub-waves. | 3C-L14; SPEC §8 W5 + §9 W6 (9 sub-waves). |
| `LegacyPath` rename shims in 4 `parse_with.rs` files (`json`/`css_l4`/`bbnf`/`google_sheets`). | DISPROVED ([no-workarounds] violation: A6 NEW-HIGH-1 backwards-compat shim). | `SYNTHESIS-AUDIT-OVERFIT.md:157`-`167`; `1D-skinny-lessons.md:156`. | Lock 14 v+1 explicit ban on `LegacyPath` style backwards-compat shims; typed-path collapse. | 3C-L14 (LAC-1E-08 + LAC-2C-01); SPEC §9 W6 sub-waves. |
| `builder_template.rs`/`arena_template.rs` doc-comments enshrining Pattern-H hot-grammar opt-out as design-of-record. | DISPROVED (A6 NEW-HIGH-2: substrate doc enshrines Lock 14 violation). | `SYNTHESIS-AUDIT-OVERFIT.md:168`-`176`; `1D-skinny-lessons.md:158`. | Lock 14 v+1 doc-rewrite-or-delete: substrate doc carries deletion plan if hot-grammar bodies remain hand-written. | 3C-L14; SPEC §8 W5. |

## §3 — Non-JSON generalisation gaps (cross-ref 3E; totality spec must absorb)

T-P2 2C
(`restart/audit/totality/p2/2C-grammar-neutrality.md`) catalogues 15
CSS L4 sub-grammars + Sheets falsifiers + BBNF-self falsifiers as
ADMITTED-VIA-C4-W10 NOT-VALIDATED. Per §8.3 + 3E-D07 a fleet-wide
grammar-neutrality claim requires positive CSS row + Sheets/BBNF-self
negative-control witness. The following gaps the totality spec must
absorb:

| non-JSON gap | T-P2 finding | totality absorption | 3E target |
|---|---|---|---|
| CSS L4 24 rows AUDIT-FALSIFIED at SK-V14 audit-zero; 14/15 `.bbnf` files at `grammar/css/l4/` orphaned (no `regen-css` xtask). | 2C V4 CSS L4 census; `SYNTHESIS-AUDIT-OVERFIT.md:136`-`145`; `1D-skinny-lessons.md:133`. | SK-V14 SPEC §5 W2 lands `regen-css` as first instance of `regen-{grammar}` family; SPEC §6 W3 lands ~960 KB production corpora; SPEC §11 W8 re-admits CSS L4 against work-equivalent lightningcss/cssparser. | 3E-D05 + 3E-D07. |
| Pass-layer JSON byte alphabet leak at `skinny/crates/passes/src/lib.rs:331` + role mining at `:1300`-`1391` — non-JSON grammars whose tokens diverge from `{`,`}`,`[`,`]`,`:`,`"`,`,` silently denied recognizer derivation. | `1B-codegen-evidence.md:69`-`73`; `1D-skinny-lessons.md:124`. | Sheets / BBNF-self role-mining fixture must round-trip clean WITHOUT code change in passes; both layers (codegen-name + pass-shape) must close before non-JSON generation reads proved. | 3E-D08 grammar-shape leak census. |
| CSS L4 layout asymmetry: skinny 7-cluster (`css_l4_at_rules_and_media`/`declaration_values`/...) vs main monolithic `css_l4/` + `css_pretty/`. Neither canonical for V1. | `1C-runtime-evidence.md:167`; `1D-skinny-lessons.md:134`. | R4 `regen-css` xtask is canonical-layout determinant; if main is canonical, skinny 7-cluster IS over-fragmentation Lock 14 was authored to prevent. | 3E-D01 BackendShape matrix CSS rows; 3F MIGRATION. |
| Sheets formulas/references/operators are JSON-role falsifiers: object/member/value mining cannot model formulas, references, directives, or Pratt/operator chains. | `2C-grammar-neutrality.md:72`-`79`; `1D-skinny-lessons.md:124`. | Future-grammar onboarding test 3E-D06 step 6: Sheets/BBNF-self must fail closed under JSON-shaped mining; pass only after generated facts replace it. | 3E-D06 + 3E-D07. |
| BBNF-self directives + Pratt/operator chains require precedence/associativity facts; Lock 10 forbids `@pratt` and requires auto-detection. | `restart/locks/LOCKS.md:70`; `1D-skinny-lessons.md:124`. | Decision-engine resolver consumes generated Grammar IR facts; no `@pratt` directive. | 3E-D02 resolver pipeline + 3E-D06. |
| 15 CSS `.bbnf` files at `grammar/css/l4/` UNWIRED at SK-V14 dispatch; only root `Cargo.toml:22` cites `stylesheet.bbnf` orthogonally. | `SYNTHESIS-AUDIT-OVERFIT.md:136`-`145`; `1D-skinny-lessons.md:148`. | SPEC §5 W2 `cargo xtask regen-css` as parametrised grammar-neutral generator; round-trip check. | 3E-D06 onboarding test step 2; SPEC §5 W2. |
| Comparator misbinding: single `sonic_rs_anchor` lane at `bbnf-bench/benches/json_parity.rs:87`-`102` serves as strict comparator for all three JSON planes simultaneously. | `1D-skinny-lessons.md:146`. | SPEC §4 W1 R1+R2: 3 plane-correct strict comparators (parse_only → Skipper-class; direct → strict struct deser; typed → per-corpus typed deser) + per-iter equality oracle inside timing region. | 3E-D04 primitive vocabulary transfer + 3C-L08. |

## §4 — Proposed Delta Table (V1 carry-forward + V4 new)

### §4.1 — Carry-forward (FOLD-3D-001..010 byte-identical from V3)

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1 surface section | rationale and expected 3C/3E impact |
|---|---|---|---|
| FOLD-3D-001: Single-substrate/output-plane taxonomy — JSON offset tape, direct SinkOnly projection, CSS fact-stream rows as one substrate family with fenced output planes. | 1A-SUB-014 / 1A-DIV-006; `1A-substrate-evidence.md:32` + `:46` + `:58`; `skinny/REDRESS.md:110` + `:126`. | Lock 1; substrate catalog; direct row spec; CSS row spec. | 3C Lock 1 MODIFY: admit fact-stream rows as output-plane of same substrate discipline while preserving ban on parallel substrates. 3E re-uses taxonomy for non-JSON import boundaries. |
| FOLD-3D-002: Cold/lazy materialization in totality substrate contract; generated grammar-owned flags + sinks. | `skinny/REDRESS.md:134`; `RESULTS.md:99`; `1C-runtime-evidence.md:79` + `:83`; T-P2 generated-config-legal-public-trait-illegal contract. | Lock 1; Lock 14; runtime consumption; generated sink schema. | 3C cold/lazy materialization as lock-strengthening rule (not JSON-only). 3E verifies generated flag/sink surfaces are grammar-owned. |
| FOLD-3D-003: Row-plane SOTA accounting (parse / direct / typed / non-JSON rows). | `RESULTS.md:5`-`7`; `REDRESS.md:2980` + `:3040` + `:3106`; `1E-locks-evidence.md:103`. | Lock 8; BENCH; skinny-results import contract. | 3C MODIFY Lock 8: each row carries corpus + plane + comparator + generated-artifact + row-predicate + routed-remainder. 3E applies same ledger to CSS + future grammars. |
| FOLD-3D-004: SK-V12 CSS L4 row durable evidence; rejected as full CSS parity or campaign closure. | `RESULTS.md:94`; `CAMPAIGN-CLOSE-SK-V12-V12.md:12`; SK-V13 `SYNTHESIS.md:38`; CSS parity gap. | Lock 8; Lock 14; CSS parity matrix; non-JSON benchmark gate. | 3C records CSS row as positive Lock 8/14 evidence, not closure shortcut. 3E requires full feature-family coverage before non-JSON totality claim. |
| FOLD-3D-005: Generalize Lock 14 via generated provider registries + name-vs-shape distinction. | `REDRESS.md:3557` (GrammarConfig partial); `1E-locks-evidence.md:112`; `2C-grammar-neutrality.md:82`. | Lock 14; Lock 10; directive lowering; generated provider registry; onboarding test. | 3C MODIFY Lock 14 with generated-provider exception + reject public GrammarConfig/JsonSink-style generics. 3E requires Sheets/BBNF-self negative controls. |
| FOLD-3D-006: SIMD/ASM via Lock 16 manifest with scalar parity + checkasm + row movement + same-wave consumer + zero-orphan. | `REDRESS.md:3603` + `:3766` + `:3869`; SK-V13 scoping SIMD/ASM/union; `2B-primitive-vocabulary.md:126` + `:186`; `2E-host-arch-esoterica.md:115`. | Lock 16; SIMD primitive ledger; host-arch gate; BENCH row admission. | 3C MODIFY Lock 16: support-only SIMD never totality evidence. 3E rejects primitive-only imports without scalar fallbacks + corpus parity + feature gating + same-wave consumer. |
| FOLD-3D-007: Rejected-route ledger preventing replay without material differentials. | `REDRESS.md:209` (pair fusion) + `:216` (dispatch tables) + `:226` (skipless 12-byte); `:2795` + `:2850` + `:2910` (union 96/97/98 retired); `2E-host-arch-esoterica.md:184`. | Lock 1; Lock 4; Lock 10; Lock 14; Lock 16; rejected-route appendix. | 3C names historical preblocks non-admissive unless candidate changes data movement / consumer shape / measured row outcome. 3E uses ledger as replay filter. |
| FOLD-3D-008: SK-V13 decision-engine route as P1-P8 cascade replacement; no new directive / BIR / BackendShape / substrate. | SK-V13 `SYNTHESIS.md:59`; scoping decision-engine `:13`; `2D-cost-model.md:55` + `:139`. | Lock 4; Lock 10; Lock 14; cost model; recognizer import boundary. | 3C Lock 4/10/14 MODIFY with explicit no-new-surface clause. 3E verifies optimizer consumes existing 5 BackendShape values + generated providers only. |
| FOLD-3D-009: Full-SOTA / anti-demotion handoff gate for G-Omega before W0. | SK-V13 `SYNTHESIS.md:30` + `:95` + `:112`; S-P1/S-P2 converged as research, not admissions. | Lock 8; BENCH; HANDOFF; implementation packet gate. | 3C ensures no lock allows S-P1/S-P2 profile facts or one-row CSS to close campaign. 3F handoff gate names remaining JSON/CSS rows + G-Omega prerequisite. |
| FOLD-3D-010: Monotonic skinny boundary — skinny evidence informs totality; totality does not reopen or rewrite live skinny artefacts. | `PASS-3-SYNTHESIS.md:21` + `:213` + `:228`; HANDOFF forbids governance edits from this packet. | T-P3 packet boundary; 3C lock-disposition checklist; Pass Omega intake. | 3C treats this file as evidence, not authority. 3E/3F cites skinny only as historical evidence unless a future skinny tranche independently admits/rejects new rows. |

### §4.2 — V4 new (FOLD-3D-011..014)

| proposed delta | source S-P3 V3 LOCK / SK-V14 SPEC anchor | affected V1 surface section | rationale and expected 3C/3E impact |
|---|---|---|---|
| FOLD-3D-011: Adopt the SK-V14 SPEC 12-wave plan W0..W11 as the V1.1 wave manifest (W0 Baseline Profile + Telemetry Lock; W1 Comparator Rebind + Per-Iter Equality + PRUNE-1; W2 regen-css xtask; W3 Production CSS Corpora; W4 PRUNE-2 delete 7 CSS templates + revert 24 CSS admits; W5 PRUNE-3 Lock-14 refactor; W6 PRUNE-4 9 sub-waves per-grammar runtime collapse; W7 PRUNE-5 wire W8 policy + W9 union; W8 CSS L4 Re-Admit; W9 JSON Direct + Typed Re-Admit; W10 JSON parse_only Distinct Path + Re-Admit; W11 Close + Alpha Feedback). | `SPEC.md:237`-`248` (V2-LOCKED through V3); `HARDENING-S-P3-V3-CONSOLIDATED.md:259`-`273` (wave-numbering 12/12 parity); `1D-skinny-lessons.md:144` (PRUNE-4 9 sub-waves not 8). | `MASTER-PLAN.md` §H wave table; SK-V14 SPEC §2 binding ordering. | 3B MASTER-PLAN wave reconciliation: 12-wave manifest replaces any prior wave count drift. 3F MIGRATION carries PRUNE-2 → PRUNE-3 → PRUNE-4 → PRUNE-5 sequencing (R4 → PRUNE-2 + C-1 → C-4 per `1D-skinny-lessons.md:149`-`150`). |
| FOLD-3D-012: Adopt the 8-candidate shortlist C1..C8 with Stage-A authoring queued for W10 + F-V2-P1ABC-RERECORD Stage-0 W10 UNCONDITIONAL binding. C1 `long_string_body_simd_scan` (NF-CH6-4 canonical-name binding) / C2 `structural_index_singular_substrate_consumer` (substrate-union typed-skip) / C3 `digit_block_simd_accumulate` (UDOT) / C4 `unicode_escape_neon_nibble_decode` (`\uXXXX` x4/x8 batch) / C5 `parse_attribution_envelope_cracker` (F-V2-P1ABC-RERECORD Stage-0) / C6 force-inline LTO envelope discipline / C7 ASCII whitespace SIMD skip / C8 SinkOnly activation. | `p3a-candidate-shortlist.md:57`-`316`; `HARDENING-S-P3-V3-CONSOLIDATED.md:412`-`436`; `SPEC.md:221` (Stage-0 binding) + `:982` + `:990` + `:1000` (W10 5-step inheritance chain UNCONDITIONAL); `1D-skinny-lessons.md:145` (Lock-14 mis-attribution envelope crack). | Lock 1; Lock 14; Lock 16; SPEC §10 W7 + §13 W10 Stage-0 binding; F-V2-P1ABC-RERECORD operational atoms (cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands). | 3C-L01 (LAC-2B-05 substrate-target triad on every candidate); 3C-L14 (per-wave name+shape leak gate); 3C-L16 (per-candidate CF-3 3-gate admission: scalar-ref + checkasm-parity + same-wave-consumer NAMED). NF-CH6-4 canonical-name binding: ONE primitive name + ONE scalar-ref function — three orthogonal SIMD bodies REJECT per Lock 14 v+1 (`SPEC.md:222`). |
| FOLD-3D-013: Adopt the CH4 6-class cost-neutrality taxonomy as canonical admission-cost-neutrality test for cosmetic-fold cycles in research-layer artefacts. Classes: (1) cite-rebind / (2) cite-cosmetic / (3) REJECT-label-refinement / (4) anti-paper-close-paragraph-insertion / (5) anchor-refresh / (6) mirror-refresh (NEW at V3). | `HARDENING-S-P3-V3-CONSOLIDATED.md:234`-`255` (V3 CH4 §1 extends T-P1 5-class taxonomy with 6th class); `HARDENING-S-P3-V3-CONSOLIDATED.md:348`-`355` (taxonomy carries forward to T-P3 §3C + wave-triumvirate dispatch); LAC-1E-12 procedural addendum (`SPEC.md:226`); NEW-CH2-V3-02 orphan-cell propagation guard. | T-P3 §3C disposition discipline; CHALLENGE V{N} fold-packet authoring; wave-triumvirate dispatch admission gate. | 3C cost-neutrality discipline: every ACCEPT/MODIFY disposition that touches a research-layer artefact classifies its fold into one of 6 classes; un-classified folds default to admission cost. 3F MIGRATION: institutionalise pre/post-grep on every cite-bearing micro-fold (NEW-CH2-V3-02 binding). |
| FOLD-3D-014: Inherit the SK-V14 audit-zero reversal as the V1.1 baseline state. 40-admit AUDIT-FALSIFIED (5 parse_only W14.1-5 + 4 direct + 7 typed + 24 CSS L4 W5 PASS-ADMIT lineage); audit-overlay column (`audit_overlay_verdict` ∈ {AUDIT-FALSIFIED, AUDIT-SUSTAINED, AUDIT-PENDING}); indefatigable close clause (R10: SK-V14 brackets SK-V15 automatically if any goal unmet). | `SPEC.md:54`-`59` audit-zero baseline; `SPEC.md:75`-`84` ROLLING-SOTA-DELTA reconciliation; `SPEC.md:230` audit-overlay gate-enforced; `SPEC.md:44`-`50` + `:396`-`:407` indefatigable close; `1D-skinny-lessons.md:140`-`141` (CH3-005 strict-vs-broader split). | RESULTS schema (audit_overlay_verdict column); BENCH telemetry binding; HANDOFF close clause; ARCHITECTURE.md / MASTER-PLAN.md / LOCKS.md (audit-overlay + close-clause potentially missing). | 3B MASTER-PLAN: confirm wave program respects R6/R7/R8 cannot dispatch until honest baseline restated in RESULTS.md + ROLLING-SOTA-DELTA.md (`1D-skinny-lessons.md:182`). 3A ARCHITECTURE: absorb audit-overlay column. 3C: no lock weakening for audit-falsified admit rows; fresh material differential evidence required per row to re-admit. |

## §5 — Expected 3C Lock Impact Index (V4 updated)

| 3C target | expected disposition pressure | contributing folds |
|---|---|---|
| Lock 1 substrate | MODIFY: output-plane taxonomy + cold/lazy materialization + rejected-route replay filter + substrate-target triad on every candidate + REDRESS 96/97/98 PERMANENT pre-block (`restart/locks/LOCKS.md:52`). | FOLD-3D-001, FOLD-3D-002, FOLD-3D-007, FOLD-3D-012, FOLD-3D-014 |
| Lock 4 output-piping | MODIFY: replace hard-coded decision cascade with active cost / CSP / e-graph gates; no fused generic substrate or post-hoc output piping (`restart/locks/LOCKS.md:58`). | FOLD-3D-008 |
| Lock 8 SOTA | MODIFY: row-plane SOTA ledger + CSS partial-admit rule + full-SOTA handoff gate + comparator-plane provenance + per-iter equality oracle + audit-overlay column (`restart/locks/LOCKS.md:66`). | FOLD-3D-003, FOLD-3D-004, FOLD-3D-009, FOLD-3D-014 |
| Lock 10 directives | MODIFY: keep BackendShape side-table closed; decision-engine consumes generated providers only; no `@pratt` directive (`restart/locks/LOCKS.md:70`). | FOLD-3D-005, FOLD-3D-008 |
| Lock 14 grammar generalization | MODIFY: legal generated per-grammar modules from one template; illegal generic runtime grammar arms / names / public JSON-shaped sinks / JSON-specific flags; per-wave name+shape leak gate; forward invariant on new-grammar onboarding (`restart/locks/LOCKS.md:78`). | FOLD-3D-002, FOLD-3D-004, FOLD-3D-005, FOLD-3D-008, FOLD-3D-011, FOLD-3D-012 |
| Lock 16 SIMD | MODIFY: scalar/checkasm/corpus parity + feature gates + same-wave production consumer + zero-orphan disposition + CF-3 3-gate admission per shortlist candidate (`restart/locks/LOCKS.md:87`). | FOLD-3D-006, FOLD-3D-007, FOLD-3D-012 |
| 5-shape BackendShape coherence | VERIFY in 3C/3E: decision engine + non-JSON import consume existing 5 shapes; no hidden shape variants; CSS fact streams = output planes, not 6th shape (`1B-codegen-evidence.md:36`; `PASS-3-SYNTHESIS.md:211`). | FOLD-3D-005, FOLD-3D-008 |
| CH4 cost-neutrality taxonomy (6 classes) | DISCIPLINE: every 3C ACCEPT/MODIFY disposition that touches a research-layer artefact classifies its fold into one of 6 classes (cite-rebind / cite-cosmetic / REJECT-label-refinement / anti-paper-close-paragraph-insertion / anchor-refresh / mirror-refresh). | FOLD-3D-013 |
| Wave manifest (W0..W11) | ADOPT into 3B MASTER-PLAN: 12-wave plan + per-wave LOC budget + 90-min impl/redress cap + rerun ceiling + Section 2.1 Generality+Lock 14 exit gate. | FOLD-3D-011 |
| F-V2-P1ABC-RERECORD Stage-0 W10 UNCONDITIONAL | BIND into 3C-L14 + SPEC §13 W10: cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands. | FOLD-3D-012 |

## §6 — Consequences (V4 updated)

| delta group | positive effect | cost / risk / wave | propagation |
|---|---|---|---|
| FOLD-3D-001 + 002 | Skinny substrate wins → grammar-neutral substrate taxonomy without reopening live skinny rows. | Doc delta small; impl follow-through medium (runtime sinks / flags / output planes cross substrate + runtime ownership). | 3C Lock 1/14 wording; 3E non-JSON import checks; future generated-sink audits. |
| FOLD-3D-003 + 004 | Blocks parse-only and one-row CSS from masquerading as campaign closure while preserving admitted CSS row. | High verification cost: SK-V13 reopens 51 JSON rows + 23 remaining CSS feature families. | 3C Lock 8; BENCH schema; 3F handoff gate; future skinny result ledgers. |
| FOLD-3D-005 | Lock 14 actionable: generated per-grammar specialisation legal only when generic crates stay grammar-shape neutral. | High risk if totality blurs grammar names with grammar shapes (T-P1 current drift + T-P2 negative controls). | 3C Lock 14; 3E generated-provider registry; Sheets/BBNF-self onboarding tests. |
| FOLD-3D-006 + 007 | SIMD/ASM + union history → admission gates instead of repeated experiments. | Medium-to-high impl risk: primitives need scalar parity + host feature gates + production consumers + material differentials. | 3C Lock 16; rejected-route appendix; SIMD manifest; future union packet checklist. |
| FOLD-3D-008 | Decision-engine work gets bounded totality receiving surface; prevents optimizer from inventing new directives/shapes. | High design risk: current engine hard-coded; missing e-graph/CSP pieces. | 3C Lock 4/10/14; 3E BackendShape coherence; impl SPEC/DISPATCH post-G-Omega. |
| FOLD-3D-009 + 010 | S-P1/S-P2 stay as evidence; G-Omega is pre-W0 gate; monotonic skinny→totality direction. | Low doc risk; high process risk if future packets treat research profiles as admissions. | 3C no-silent-drop checklist; 3F handoff; Pass Omega intake. |
| FOLD-3D-011 (V4 NEW) | 12-wave plan W0..W11 is V1.1-authoritative; 3B MASTER-PLAN aligns wave count with SK-V14 SPEC binding. | Doc delta medium; PRUNE-4 9 sub-waves (not 8) affects wave count + 90-min cap accounting. | 3B MASTER-PLAN §H; 3F MIGRATION sequencing (R4→PRUNE-2 + C-1→C-4). |
| FOLD-3D-012 (V4 NEW) | 8-candidate shortlist + Stage-A W10 binding consolidates 39 active S-P2 candidate-slots into one ≤8-candidate workplan; F-V2-P1ABC-RERECORD Stage-0 UNCONDITIONAL closes the orphan-kernel hole (REDRESS C5 could ship in zero waves). | High impl risk: each candidate carries CF-3 3-gate admission; NF-CH6-4 canonical-name binding (ONE primitive name) is REJECT trigger for orthogonal SIMD bodies. | 3C-L01/L14/L16; SPEC §10/§13; F-V2-P1ABC-RERECORD operational atoms. |
| FOLD-3D-013 (V4 NEW) | CH4 6-class cost-neutrality taxonomy institutionalises strengthening-not-neutral micro-fold discipline; LAC-1E-12 + NEW-CH2-V3-02 become wave-triumvirate dispatch discipline. | Low doc risk; high process discipline: every cite-bearing micro-fold carries pre/post grep on downstream mirrors. | 3C disposition discipline; 3F MIGRATION institutionalisation; CHALLENGE V{N} fold-packet authoring. |
| FOLD-3D-014 (V4 NEW) | SK-V14 audit-zero baseline + audit-overlay column + indefatigable close clause make V1.1 honest about the 40-admit AUDIT-FALSIFIED reversal; R10 binds SK-V15 if any goal unmet. | High process risk if V1 over-claims CSS L4 admission after SK-V14 audit reverses SK-V12 W1b; audit-overlay column required in RESULTS schema. | 3A ARCHITECTURE.md; 3B MASTER-PLAN.md wave program; 3C no-lock-weakening for audit-falsified rows; RESULTS schema. |

## §7 — V4 Cost And Routing Ledger

This ledger makes the V4 folds budgeted and receiver-bound. It routes
skinny evidence into totality amendments without authorising any live
skinny source / RESULTS / REDRESS edit (per §8.4 monotonic boundary).

| fold | LOC budget | propagation surfaces | risk class | wave alignment | same-wave consumer / receiver | hard cap or abrogate gate |
|---|---:|---:|---|---|---|---|
| FOLD-3D-001 | 80-180 docs | 4 | Medium-high | Lock 1 / substrate taxonomy | Receiver: 3C Lock 1 + 3E non-JSON taxonomy. | Block if fact streams retained sidecars or hidden substrates. |
| FOLD-3D-002 | 80-200 docs | 4 | High | Lock 1/14 generated sink wave | Receiver: generated flag/sink schema + Lock 14 scan. | Abrogate generic public sink/flag APIs encoding JSON policy. |
| FOLD-3D-003 | 100-220 docs/report | 5 | High | Lock 8 / BENCH row-plane ledger | Receiver: row-plane SOTA ledger covering parse / direct / typed / CSS. | Block if parse-only tolerance or row demotion loophole remains. |
| FOLD-3D-004 | 60-140 docs | 4 | Medium | CSS parity gate | Receiver: Lock 8/14 + S-P3 CSS feature manifest. | Block if SK-V12 CSS row treated as full CSS parity closure. |
| FOLD-3D-005 | 120-260 docs | 5 | High | Lock 14 provider-registry wave | Receiver: generated manifest + leak scan + negative controls. | Abrogate if generic crates require hand-coded grammar roles. |
| FOLD-3D-006 | 120-260 docs | 5 | High | Lock 16 SIMD/ASM manifest | Receiver: primitive manifest + source-present state machine. | Block support-only SIMD; each primitive wires / deletes / delegates / blocks. |
| FOLD-3D-007 | 80-180 docs | 4 | High | Rejected-route/material-differential ledger | Receiver: Lock 1/16 preblock text + S-P3 wave gates. | Block replay of pair fusion / dispatch table / skipless token / old union routes without material differential. |
| FOLD-3D-008 | 100-240 docs | 5 | High | Decision-engine fold | Receiver: Lock 4/10/14 + S-P3 G2 wave set. | Abrogate if optimizer work invents new directives / BIR variants / BackendShape / retained substrate. |
| FOLD-3D-009 | 60-140 docs | 4 | High process | G-Omega before W0 | Receiver: 3F handoff + BENCH gate. | Block any W0/source/generated/gate/RESULTS/REDRESS edit before G-Omega + S-P3 convergence. |
| FOLD-3D-010 | 40-100 docs | 3 | Medium process | T-P3/Omega monotonic boundary | Receiver: Pass Omega intake + G3 packet. | Block if totality edits rewrite live skinny artefacts or treat research as admission. |
| FOLD-3D-011 (V4 NEW) | 120-280 docs | 5 | Medium-high | MASTER-PLAN §H + SK-V14 SPEC §2 12-wave manifest | Receiver: 3B MASTER-PLAN wave reconciliation; 3F MIGRATION sequencing (R4→PRUNE-2 + C-1→C-4); PRUNE-4 = 9 sub-waves. | Block any wave-count revision that drops to 8 sub-waves or omits W2 regen-css. |
| FOLD-3D-012 (V4 NEW) | 240-520 docs (+ candidate manifest in 3C-L16) | 6 | High | SPEC §10 W7 + §13 W10 Stage-0 binding | Receiver: 3C-L01 substrate-target triad on every shortlist candidate; 3C-L14 per-wave name+shape leak gate; 3C-L16 CF-3 3-gate admission per candidate; F-V2-P1ABC-RERECORD operational atoms. | Block any wave admitting a dispatch-envelope-internal primitive without shipping F-V2-P1ABC-RERECORD as Stage 0 of the same wave; REJECT orthogonal SIMD bodies for one canonical primitive (NF-CH6-4). |
| FOLD-3D-013 (V4 NEW) | 80-180 docs | 4 | Low | 3C disposition discipline + CHALLENGE V{N} authoring | Receiver: every 3C ACCEPT/MODIFY classifies fold into 6 classes; LAC-1E-12 + NEW-CH2-V3-02 institutionalised. | Block any cite-bearing micro-fold without pre/post-grep evidence on downstream mirrors. |
| FOLD-3D-014 (V4 NEW) | 140-300 docs (+ RESULTS column add) | 5 | High process | 3A ARCHITECTURE absorption + RESULTS schema + indefatigable close clause | Receiver: audit-overlay column in RESULTS; close clause in ARCHITECTURE/MASTER-PLAN; no lock weakening for audit-falsified admit rows. | Block any V1 spec wording that over-claims CSS L4 admission post-SK-V14 audit reversal; block any R6/R7/R8 dispatch before honest baseline restated in RESULTS + ROLLING-SOTA-DELTA. |

## §8 — V4 Gated Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 | Does 3C V4 disposition every 1E + 2X lock candidate touched by these folds (Lock 1, 4, 8, 10, 14, 16) plus the V4 NEW folds (FOLD-3D-011..014) with no silent drops? | 3C / G3 packet. | 3D only routes folds; 3C owns candidate disposition. | 3C V4 ledger must list every candidate group + every V4 NEW fold; no ACCEPT/MODIFY as implementation admission. |
| CH2 | Should the generated-provider Lock 14 amendment require Sheets + BBNF-self negative controls in the first V1.1 lock text, or may that remain a 3E onboarding gate (3E-D06)? | 3C / 3E / G-Omega. | T-P2 requires negative control but not exact cardinality. | G-Omega pins witness cardinality or records explicit receiver gate per `2C-grammar-neutrality.md:168`. |
| CH3 | What exact evidence distinguishes a fresh union material differential from a replay of REDRESS 96/97/98 (PERMANENT pre-block)? | 3C Lock 1/16 + S-P3 union wave. | Prior union routes are historical failures; user pin unblocks category only with material differential. | SPEC wave must name (a) changed data movement, (b) changed consumer shape, (c) measured row outcome before redress (Lock 1 v+1 triad). |
| CH4 | Should CSS source-sidecar / lightningcss comparator code be named in Lock 1/14 text as comparator-only, so it cannot be mistaken for runtime substrate? CSP T-P1 CH5-004 names CSS source-sidecar at `nonjson_css_l4.rs:648`+. | 3C / 3E / BENCH. | CSS sidecar valid comparator evidence but can become hidden coupling if unclassified. | Lock/BENCH text must state comparator-only provenance + no runtime dependency. |
| CH5 | Where should the G-Omega before W0 gate live: HANDOFF only, BENCH only, or both? | 3F + Pass Omega CRUD-4. | T-P3 cannot edit either surface directly. | CRUD-4 must name W0 refusal conditions + S-P3 / G-Omega prerequisites. |
| CH6 | Should row-plane SOTA ledgers include profile-method freshness as a required column (alongside the V4 NEW audit-overlay column)? | 3C Lock 8 + 3F handoff/BENCH routing. | S-P1 profile facts are not gate admissions. | Row ledger schema must separate profile freshness from row admission evidence; audit-overlay (`audit_overlay_verdict`) is gate-enforced per row (SPEC §1:230). |

## §9 — Monotonic boundary declaration (§8.4 verbatim binding)

Per `restart/prompts/totality/PASS-3-SYNTHESIS.md:213` (§8.4): "The
skinny→totality fold is monotonic (3D). Skinny wins become
V1-authoritative; skinny rejections become locks-strengthening
evidence; the totality spec never dictates back to a live skinny
iteration."

This 3D V2 artefact:

1. Treats S-P3 §3Z COHORT LOCK at `867b0cd0b` as evidence input to the
   totality fold; does NOT propose any edit to live S-P3 V3-LOCKED
   artefacts (SPEC.md / DISPATCH-PROMPT.md / p3a..p3f / hardening
   consolidator).
2. Carries forward FOLD-3D-001..014 byte-identical from V1 cycle
   (per-artefact V4 counter, cohort-normalised to V2); no skinny-fold
   reversal.
3. Folds the F-V2-CH6-3D REVISE: §1 row 8 (SK-V12 W1a substrate
   prerequisite `escape_mask_64` ADMITTED-EVIDENCE) and §2 row 3
   (SK-V13 W1b CSS L4 row-admit attempt DISPROVED) now carry an
   explicit reconciling cross-cite per CH6 anti-paper-close
   discipline; both rows are correct under distinct framings
   (substrate-prerequisite-preserved vs row-admit-AUDIT-FALSIFIED)
   and mirror 3B's substrate-pillar-vs-row-admit distinction at
   `3B-master-plan-reconciliation.md:99` + `:194`.
4. Per `[no-deferrals]` + LAC-1E-12 executable verification mandate,
   every cite in this fold is path:line + re-executable at HEAD
   `867b0cd0b` (S-P3) / `34a28f5c1` (T-P2) / `0a9c0fe65d` (T-P1) per
   `T-P3-DISPATCH-CONTEXT.md §1`.
5. Per CH3 REGRESSION
   (`PASS-3-SYNTHESIS.md:113`-`116`): zero proposed delta re-opens a
   route in `skinny/REDRESS.md`; zero proposed delta proposes
   reviving a refuted wave; zero proposed delta promotes a rejected
   route; zero proposed delta weakens a lock REDRESS evidence
   strengthened.
6. Cycle-counter cohort-normalisation: frontmatter cycle field
   migrated from V4 (per-artefact V3-baseline + per-cycle increment)
   to V2 (V1 dispatch + this V2 fold) for cohort coherence with
   3A/3B/3F. Other artefacts: 3A/3B/3F at V1; 3C/3D/3E previously at
   V4 — realigning under T-P3 V1→V2 fresh-cycle convention per CH1
   §5 open question.

The skinny→totality fold direction holds. Totality V1.1 amendment
packet flows forward to 3C disposition + 3B MASTER-PLAN reconciliation
+ 3E grammar-generalisation cross-ref + 3F MIGRATION/HANDOFF; the
live SK-V14 skinny iteration is undisturbed and wave-triumvirate
dispatch unblocks per `HARDENING-S-P3-V3-CONSOLIDATED.md §6.1` after
T-P2 LOCK → T-P3 §3C disposition → G-Omega user gate.
