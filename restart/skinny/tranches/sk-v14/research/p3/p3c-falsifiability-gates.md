# SK-V14 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-23.
Scope: per-wave falsifiability gates for the SK-V14 wave program — named corpus rows, Mbps thresholds vs `SK-V14-open`, full-table maintain budgets, exit gates, revert protocols, R1 strict-vs-strict comparator plane binding, R2 per-iter equality binding.
Output: this file.
Pass Alpha goalset: SYNTHESIS.md §0.1 R10 close-condition (51 JSON cells × 3 planes + 24 CSS L4 features ADMIT > strict-vs-strict OR architectural-block proof) + §0.3 R1–R10 acceptance + §0.4 P-1..P-7 pattern pre-blocks.
Candidate pool: S-P2 §3Z LOCKED post-V3 cohort (`research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`) consumed via SYNTHESIS §3 C-1..C-5 candidate slate. P3-B not yet authored at V1; wave sequence inferred from SYNTHESIS §3 + audit-overfit §2.1-§2.3 sequencing constraints and committed in §2.1 of this artefact.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

### §1.1 — Wave sequence inherited from SYNTHESIS §3 + audit-overfit §2

SYNTHESIS.md §3 (sk-v14/SYNTHESIS.md:263-285) enumerates five S-P2 candidates C-1..C-5 mapping to R1–R10. `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §2.1-§2.3` (lines 240-292) binds three architectural sequencing constraints:

1. **§2.1**: R4 (regen-css xtask, in C-3) MUST land BEFORE PRUNE-2 (in C-5) — without R4 there is no generator to replace the 7 deleted templates.
2. **§2.2**: C-1 (PRUNE-3 + PRUNE-4) MUST land BEFORE C-4 (PRUNE-5) — wiring W8/W9 before the Lock-14 dispatcher exists doubles the refactor surface.
3. **§2.3**: PRUNE-4 carries 9 sub-waves (`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:282-290` — css_pretty addition since SK-V13 baseline), not 8.

The PRUNE-1 (JSON revert) leg of C-5 has no sequencing dependence on R4, so it can land before PRUNE-2.

### §1.2 — Wave manifest (12 waves; ≤12 cap per S-P3 dispatch §2; SPEC §2 binding ordering)

| Wave | Candidate | Scope | R-targets met | Sub-waves |
|---|---|---|---|---|
| W0 | infrastructure | baseline + telemetry lock (`SK-V14-open` snapshot; audit_overlay_verdict + comparator_plane + per_iter_equality + track2_entry_point columns wired into `xtask gate-json`) | telemetry binding (SYNTHESIS §2) | — |
| W1 | C-2 fused C-5 PRUNE-1 | comparator rebind (3 plane-correct strict comparators) + per-iter equality oracle in the timing region; revert 5 JSON `parse_only` (W14.1–.5) + 6 JSON `direct` + 11 JSON `typed` audit-falsified rows | R1, R2, R3.PRUNE-1 | — |
| W2 | C-3 part-A | `cargo xtask regen-css` pipeline (R4 — first instance of `regen-{grammar}` family) | R4 | — |
| W3 | C-3 part-B | `skinny/corpora/css-l4-sk-v14/` (Bootstrap + Tailwind + Material + Animate; ~960 KB) | R5 | — |
| W4 | C-5 PRUNE-2 | delete 7 CSS L4 hand-written template files + `include_str!`'d `generated.rs`; revert 24 CSS L4 admitted rows | R3.PRUNE-2 | — |
| W5 | C-1 PRUNE-3 | replace `RuntimeProvider` enum with trait-based dispatch; collapse 8 per-grammar provider modules in `skinny/crates/codegen/` into ONE grammar-agnostic generator template | R3.PRUNE-3 | — |
| W6 | C-1 PRUNE-4 | refactor 67 hand-written per-grammar files in `crates/core/src/runtime/{grammar}/` into emitted output, 9 sub-waves (one per grammar: json / css_l4 / css_l4_full / css_pretty / sheets / bbnf-self / runtime-helpers-shim / typed-path-collapse / regen-binding) | R3.PRUNE-4 | 9 |
| W7 | C-4 PRUNE-5 | wire W8 per-grammar policy + W9 same-substrate union from SCAFFOLD-ONLY to LOAD-BEARING; CSP picks shapes; runtime honors selections | R3.PRUNE-5, R9 | — |
| W8 | R6 | re-admit each CSS L4 feature via grammar-derived pipeline vs lightningcss/cssparser work-equivalent comparator | R6 | 24 (per feature) |
| W9 | R7-direct fused R7-typed | re-admit each JSON `direct_to_struct` row vs sonic-rs strict per-corpus struct deser + each JSON `real_typed_struct` row vs per-corpus typed struct deser | R7 (direct + typed) | 17 + 17 (per corpus) |
| W10 | R8 | stand up distinct `parse_only` code path in `generated_json` (no full-tape build); wire to sonic-rs Skipper-class comparator; admit; Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding) | R8 | 17 (per corpus) |
| W11 | close ceremony | reconcile RESULTS/REDRESS/HANDOFF/SPEC; Alpha feedback; no implementation LOC | R10 close posture | — |

R10 (indefatigable close) is a bracket-level condition (SYNTHESIS §0.1 + §6 close posture), discharged via the W11 close ceremony; SK-V14 closes when W0–W10 dispositions exit + every row family meets the §0.1 bar OR carries architectural-block proof.

### §1.3 — Comparator strict-plane bindings (R1 — load-bearing for every gate admitting a row)

Per SYNTHESIS.md §0.3 (R1 row, line 93) + ORCHESTRATOR-PROMPT §SK-V14 GOALSET R1 (lines 98-103) + SYNTHESIS §2 telemetry binding (line 241 `comparator_plane` column):

| Plane | Strict comparator (R1 binding) | Binding source |
|---|---|---|
| JSON `parse_only` | `sonic_rs::Skipper` (structural-skip-only API; no value materialization) | SYNTHESIS §0.3 R1 + ORCHESTRATOR-PROMPT R1 |
| JSON `direct_to_struct` | `sonic_rs` strict struct deserialization per corpus (per-corpus typed `from_slice::<CorpusStruct>`, NOT `from_slice::<Value>`) | SYNTHESIS §0.3 R1 + §0.4 P-2 |
| JSON `real_typed_struct` | per-corpus typed struct deserialization (`serde_json::from_slice::<CorpusStruct>` strict + sonic-rs strict per-corpus typed variant) | SYNTHESIS §0.3 R1 + §0.4 P-2 |
| CSS L4 (all 24 features) | lightningcss full-parse strict + cssparser full-parse (no fact-stream vs full-AST asymmetry) | SYNTHESIS §0.3 R6 + §2 telemetry row `lightningcss Mbps` |

R1 gate rule, verbatim from SYNTHESIS §2 column `comparator_plane`: **"`xtask gate-json` rejects any row whose comparator does work asymmetric to Track 1"**. Per pattern-pre-block P-2 (SYNTHESIS §0.4): the single `sonic_rs::from_slice::<Value>` eager-DOM mislabelling is forbidden across all three JSON planes.

### §1.4 — Per-iter equality binding (R2 — load-bearing for every gate admitting a row)

Per SYNTHESIS.md §0.3 R2 (line 94) + ORCHESTRATOR-PROMPT R2 (lines 105-108) + SYNTHESIS §2 telemetry binding (line 242):

> `per_iter_equality` — **NEW (R2)** — boolean column emitted per iteration; PASS only if equality verified inside the timing region.

R2 gate rule: **"`xtask gate-json` rejects rows whose equality column is empty"** (SYNTHESIS §0.3 R2 verbatim; SYNTHESIS §2 telemetry row enforcement). Startup-only checksum parity fails the addendum's strict admit rule.

### §1.5 — `SK-V14-open` baseline definition (per W0 exit)

`SK-V14-open` = the W0-captured snapshot of `skinny/RESULTS.md` populated with the V14 telemetry schema (SYNTHESIS §2 — 24 columns + `comparator_plane` + `per_iter_equality` + `audit_overlay_verdict` + `track2_entry_point`). All Mbps thresholds in §2 below quote deltas vs `SK-V14-open` per ORCHESTRATOR.md §8 baseline-anchored measurement rule.

Initial `SK-V14-open` cell-state (per SYNTHESIS §1.3 honest baseline, lines 213-218):
- JSON parse_only: 0 / 17 ADMITTED
- JSON direct: 0 / 17 ADMITTED
- JSON typed: 0 / 17 ADMITTED
- CSS L4: 0 / 24 ADMITTED

Throughput floors for the W1 re-baseline are computed post-W1 from rebound comparators (C-2 leg of the fused W1); pre-W1 Mbps numbers in current `RESULTS.md` (the SK-V13 fake-admit baseline) MAY NOT be used as comparator references after W1 closes — they are AUDIT-FALSIFIED per SYNTHESIS §1.2.

## §2 — Deliverable (per-wave falsifiability gate set)

Every gate below carries: (a) named corpus rows verbatim from `skinny/RESULTS.md` enumeration, (b) Mbps threshold delta vs `SK-V14-open`, (c) full-table maintain budget, (d) measurable exit gate, (e) revert protocol, (f) R1 comparator plane binding, (g) R2 per-iter equality binding. UNMEASURABLE gate = REJECT (see §3 — zero rejects in V1).

### §2.0 — W0: Baseline + Telemetry Lock

**Same-wave consumer**: `xtask gate-json` consumes every emitted telemetry field; rejects malformed/missing evidence in the same W0 slice.

**Named corpus rows it must LIFT**: NONE. W0 is the telemetry-substrate wave per `[build-infra-first]`. No behavior wave runs first.

**Mbps threshold (delta vs SK-V14-open)**: NONE explicit (W0 *creates* `SK-V14-open`). Full-table cells stay within ±1.0 % of the captured seed (frozen-surface invariant).

**Full-table maintain budget**: every current main row in `skinny/RESULTS.md` (all 51 JSON cells per SYNTHESIS §0.1 + all 24 CSS L4 features) carries:
- `comparator_plane` populated (R1 binding identifier)
- `per_iter_equality` boolean populated per iter (R2 binding)
- `audit_overlay_verdict` populated (AUDIT-FALSIFIED / AUDIT-SUSTAINED / AUDIT-PENDING)
- `track2_entry_point` populated (symbol path)

**Exit gate** (measurable from bench):
1. `xtask gate-json` returns non-zero for any row missing required column (per SYNTHESIS §2 line 230: "rejects any row missing required columns").
2. `xtask gate-json` rejects any row whose `comparator_plane` claims `sonic_rs::from_slice::<Value>` for any plane (P-2 pre-block).
3. `xtask gate-json` rejects any row whose `per_iter_equality` column is empty (R2 verbatim).
4. `xtask gate-json` rejects any row whose Track 1 and Track 2 entry-point symbol paths share a common ancestor in `runtime::tape::` beyond public `Tape` / `OffsetFlags` types (CH5 anti-coupling per SYNTHESIS §2 line 240).
5. Throughput cells stay within ±1.0 % of the captured `SK-V14-open` seed (no behavior drift).
6. No parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or generated parser output change lands.

**Revert protocol**: revert W0 telemetry slice as one commit set; restore prior `skinny/RESULTS.md`; record W0 REDRESS rejection naming the missing column or failed gate.

**Comparator strict-vs-strict plane (R1)**: W0 binds the COLUMN, not the comparator behavior; populating `comparator_plane=sonic_rs::Skipper` for parse_only / `comparator_plane=<corpus>::from_slice strict struct deser` for direct / `comparator_plane=<corpus>::from_slice typed strict` for typed is the R1 binding. CSS rows bind `comparator_plane=lightningcss full-parse`.

**Per-iter equality column (R2)**: `xtask gate-json` rejects any W0 row whose `per_iter_equality` column is unpopulated for the SK-V14-open snapshot.

### §2.1 — W1: C-2 fused C-5 PRUNE-1 (comparator rebind + per-iter equality + JSON audit-falsified revert)

**Same-wave consumer**: bench harness consumes the rebound comparators on every named JSON row; `xtask gate-json` enforces the schema (per SYNTHESIS §3 C-2 row line 272). REDRESS scribe consumes the revert (22 new row-keyed REDRESS entries per SYNTHESIS §3 C-5 row); `ROLLING-SOTA-DELTA.md` rebase consumes the revert.

**Named corpus rows it must LIFT**: NONE strictly admit at W1 close — W1 stands up infrastructure (C-2 part) AND restores admit-baseline (PRUNE-1 part). ALL 51 JSON cells (17 corpora × 3 planes per SYNTHESIS §0.1) MUST acquire populated rebound-comparator columns + per-iter equality columns at W1 close. Per-row Mbps comparison vs `SK-V14-open` is re-baselined.

**Named corpus rows it must MAINTAIN at AUDIT-FALSIFIED**: 22 audit-falsified JSON rows (verbatim from `skinny/RESULTS.md` enumeration):
- 5 parse_only W14.1–.5 admits: `json/numbers/parse_only/main`, `json/citm_catalog/parse_only/main`, `json/canada/parse_only/main`, `json/marine_ik/parse_only/main`, `json/mesh/parse_only/main` (per SYNTHESIS §1.2 + RESULTS.md rows 36, 8, 11, 30, 22)
- 6 direct admits (broader ledger per SYNTHESIS §0.2 — 4 dispatch + 2 extension: marine_ik + instruments): `json/twitter/direct_to_struct/main` (HOLDING but comparator-misbound), `json/citm_catalog/direct_to_struct/main`, `json/apache_builds/direct_to_struct/main`, `json/marine_ik/direct_to_struct/main`, `json/instruments/direct_to_struct/main`, `json/numbers/direct_to_struct/main`
- 11 typed admits (broader ledger per SYNTHESIS §0.2): `json/twitter/real_typed_struct/main`, `json/citm_catalog/real_typed_struct/main`, `json/apache_builds/real_typed_struct/main`, `json/github_events/real_typed_struct/main`, `json/update_center/real_typed_struct/main`, `json/mesh/real_typed_struct/main`, `json/marine_ik/real_typed_struct/main`, `json/random/real_typed_struct/main`, `json/instruments/real_typed_struct/main`, `json/numbers/real_typed_struct/main`, `json/unicode_basic/real_typed_struct/main`

**Mbps threshold (delta vs SK-V14-open)**:
- per-plane comparator rebind cost: ≤ 5 % overhead vs `SK-V14-open` on the Track 1 Mbps column for any of the 51 JSON cells (rebound comparator runs in the same harness; per-iter equality adds 1 ns/iter call; both stay below the 5 % CH4 cost threshold).
- per-iter equality cost: ≤ 2 % overhead vs `SK-V14-open` on every JSON row's Track 1 Mbps (per-iter equality is a `==` digest comparison, not a re-parse).
- net Track 1 Mbps regression budget for any of the 51 JSON cells: ≤ 5 % vs `SK-V14-open` (R2 + R1 combined).
- 22 revert rows: each flips to `audit_overlay_verdict=AUDIT-FALSIFIED`, Outcome demoted, Verdict NO-GO (no Mbps threshold — revert leg of the wave).

**Full-table maintain budget**:
- 51 JSON cells: per-row Track 1 Mbps ≥ 0.95 × `SK-V14-open` Track 1 Mbps; Track 2 Mbps ≥ 0.95 × `SK-V14-open` Track 2 Mbps (C-2 rebind cost).
- 24 CSS L4 cells: no perf budget interaction (W1 touches JSON harness only); ±1.0 % vs `SK-V14-open` (default frozen-surface).
- Non-target rows (rows outside the 22-row revert set): ±1.0 % of W0 `SK-V14-open` Track 1 / Track 2 cells.

**Exit gate** (measurable from bench):
1. For every JSON corpus × plane (51 cells = 17 corpora × 3 planes per SYNTHESIS §0.2): `comparator_plane` column reads one of `sonic_rs::Skipper` (parse_only) | `<corpus>::strict_struct_deser` (direct) | `<corpus>::typed_strict_struct_deser` (typed). `xtask gate-json` rejects any cell still binding `sonic_rs::from_slice::<Value>` per P-2 pre-block.
2. For every JSON corpus × plane: `per_iter_equality` column non-empty AND PASS on every iter (R2 verbatim). `xtask gate-json` rejects empty per-iter equality cells.
3. The harness inner-loop call site (cite the file:line in W1 plan) executes equality INSIDE the timed region; the W1 plan quotes the harness line per ORCHESTRATOR-PROMPT R2.
4. Per-row Track 1 / Track 2 Mbps within 0.95 × `SK-V14-open` for all 51 JSON cells.
5. 24 CSS L4 cells within ±1.0 % vs `SK-V14-open`.
6. `ROLLING-SOTA-DELTA.md` shows `JSON parse_only: 0/17`, `JSON direct: 0/17`, `JSON typed: 0/17` per SYNTHESIS §1.3.
7. `skinny/REDRESS.md` carries 22 new row-keyed entries citing `audit-overfit/validation/v2-json-validation.md §1`–§4 + `v6-comparator-integrity.md §1, §3` per SYNTHESIS §3 C-5 row.
8. Every reverted row's `audit_overlay_verdict` column = `AUDIT-FALSIFIED` and cites the validation-pack §reference (per SYNTHESIS §2 column rule).
9. `xtask gate-json` accepts the 22-row revert manifest end-to-end.

**Revert protocol**: revert C-2 comparator rebind + per-iter equality + PRUNE-1 revert slice as one commit; restore prior `sonic_rs::from_slice::<Value>` only with explicit `audit_overlay_verdict=AUDIT-FALSIFIED` carry-over; restore `ROLLING-SOTA-DELTA.md` to pre-W1 state; preserve PRUNE-1 research artefacts under `restart/skinny/tranches/sk-v14/research/`; record REDRESS naming the failed comparator rebind, harness regression, or non-target row regression.

**Comparator strict-vs-strict plane (R1)**: W1 IS the R1 binding wave. Each of the three JSON planes acquires a plane-correct strict comparator per the table in §1.3 above. `xtask gate-json` rejects any row admitting under a comparator that does work asymmetric to Track 1 (per SYNTHESIS §2 `comparator_plane` column rule). PRUNE-1 reverts AUDIT-FALSIFIED admits that were granted under the WRONG comparator (`sonic_rs::from_slice::<Value>`) — fused with the strict rebind in the same wave.

**Per-iter equality column (R2)**: W1 IS the R2 binding wave. Bench harness emits `per_iter_equality` column per iteration; `xtask gate-json` rejects rows whose equality column is empty (SYNTHESIS §0.3 R2 verbatim). The 22 reverted rows continue to fail per-iter equality on the OLD admit data (they had startup-only checksum, not per-iter — fail mechanism unchanged from W0).

### §2.2 — W2: C-3 part-A — R4 (regen-css xtask)

**Same-wave consumer**: runtime regenerated from the 15 `.bbnf` files in the same wave per SYNTHESIS §3 C-3 row (line 273); first instance of the `regen-{grammar}` family per SPEC §2 line 239. MUST land BEFORE PRUNE-2 per audit-overfit §2.1.

**Named corpus rows it must LIFT**: NONE strictly admit at W2 close — W2 stands up infrastructure (xtask pipeline only; corpora materialization lands in W3).

**Mbps threshold (delta vs SK-V14-open)**: NONE for W2 (infrastructure wave). Throughput re-measurement happens in W8 R6.

**Full-table maintain budget**:
- 24 CSS L4 cells: ±1.0 % vs `SK-V14-open` (cells are still on the OLD admit path under audit-falsified verdict; the xtask is built but not yet wired to admit; PRUNE-2 / W8 deletes/admits).
- 51 JSON cells: ±1.0 % vs `SK-V14-open` (W2 touches CSS path only).

**Exit gate** (measurable from bench):
1. `cargo xtask regen-css` exists and is invokable from `skinny/xtask/src/main.rs` (cite the registered command path in W2 plan).
2. Round-trip clean per SYNTHESIS §3 C-3 row verbatim: `rm -rf skinny/crates/runtime/src/grammars/css_l4_* crates/core/src/runtime/css_l4/ && cargo xtask regen-css && git diff --quiet -- skinny/crates/runtime/src/grammars/css_l4_* crates/core/src/runtime/css_l4/` returns exit 0.
3. Bypass-header detector empty: `git grep -l '@generated by skinny bbnf-codegen' -- skinny/crates/runtime crates/core/src/runtime` traces every match to a registered xtask emission (P-1 pre-block — fake `@generated` header on hand-written templates).

**Revert protocol**: revert W2 xtask slice as one commit; restore prior `include_str!()` template paths only with explicit `audit_overlay_verdict=AUDIT-FALSIFIED` carry-over; record REDRESS naming the failed xtask round-trip.

**Comparator strict-vs-strict plane (R1)**: CSS rows bind `comparator_plane=lightningcss full-parse` + `cssparser_oracle full-parse` (SYNTHESIS §2 telemetry rows 250, 251). No fact-stream vs full-AST asymmetry (SYNTHESIS §0.3 R6 verbatim).

**Per-iter equality column (R2)**: CSS rows acquire `per_iter_equality` column per iter at W2 wiring; `xtask gate-json` rejects CSS rows whose equality column is empty (same rule as JSON per SYNTHESIS §0.3 R2).

### §2.3 — W3: C-3 part-B — R5 (production CSS corpora ~960 KB)

**Same-wave consumer**: bench rows wired to the new corpora per SYNTHESIS §3 C-3 row (line 273); W2 R4 xtask is the upstream substrate. MUST land BEFORE PRUNE-2 per audit-overfit §2.1.

**Named corpus rows it must LIFT**: NONE strictly admit at W3 close — W3 stands up infrastructure. CSS L4 corpora at `skinny/corpora/css-l4-sk-v14/` must materialize at ≥ 800 KB total (per SYNTHESIS §3 C-3 row + ORCHESTRATOR-PROMPT R5 line 134 — Bootstrap + Tailwind + Material + Animate ≈ 960 KB).

**Mbps threshold (delta vs SK-V14-open)**: NONE for W3 (infrastructure wave). Throughput re-measurement happens in W8 R6.

**Full-table maintain budget**:
- 24 CSS L4 cells: ±1.0 % vs `SK-V14-open` (cells are still on the OLD admit path under audit-falsified verdict; corpora are staged but not yet wired to admit; PRUNE-2 / W8 deletes/admits).
- 51 JSON cells: ±1.0 % vs `SK-V14-open` (W3 touches CSS path only).

**Exit gate** (measurable from bench):
1. `du -sh skinny/corpora/css-l4-sk-v14` reports ≥ 800 KB (R5 floor).
2. `skinny/corpora/css-l4-sk-v14/` contains Bootstrap, Tailwind, Material, Animate fixtures (each named in the W3 plan; per ORCHESTRATOR-PROMPT R5).
3. CH7 round-trip rule trigger armed: any CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× inherits the W10.3 nested_layout 124× round-trip-rule (per SYNTHESIS §0.4 P-1).

**Revert protocol**: revert W3 corpora staging slice as one commit; record REDRESS naming the failed corpora-floor miss or provenance gap.

**Comparator strict-vs-strict plane (R1)**: CSS rows preserve W2 binding `comparator_plane=lightningcss full-parse` + `cssparser_oracle full-parse`; W3 stages production corpora that the W8 R6 admit will read against this comparator.

**Per-iter equality column (R2)**: CSS rows preserve W2 R2 column wiring; per-iter equality continues per row.

### §2.4 — W4: C-5 PRUNE-2 (CSS L4 audit-falsified template delete + row revert)

**Same-wave consumer**: REDRESS scribe consumes the 24 + 7 revert (24 row-keyed + 7 template-keyed); `ROLLING-SOTA-DELTA.md` rebase consumes the revert.

**Named corpus rows it must MAINTAIN at AUDIT-FALSIFIED**: all 24 CSS L4 features (verbatim from `skinny/RESULTS.md`):
`css_l4/declaration_values/direct_to_struct/main`, `css_l4/stylesheet_and_selectors/direct_to_struct/main`, `css_l4/stylesheet_root/direct_to_struct/main`, `css_l4/selectors/direct_to_struct/main`, `css_l4/pseudo_classes/direct_to_struct/main`, `css_l4/pseudo_elements/direct_to_struct/main`, `css_l4/attribute_selectors/direct_to_struct/main`, `css_l4/declaration_values_extended/direct_to_struct/main`, `css_l4/declarations/direct_to_struct/main`, `css_l4/css_variables/direct_to_struct/main`, `css_l4/calc_expressions/direct_to_struct/main`, `css_l4/var_url_functions/direct_to_struct/main`, `css_l4/color_functions/direct_to_struct/main`, `css_l4/visual_functions/direct_to_struct/main`, `css_l4/gradients/direct_to_struct/main`, `css_l4/transforms/direct_to_struct/main`, `css_l4/filters/direct_to_struct/main`, `css_l4/easing_functions/direct_to_struct/main`, `css_l4/at_rules_and_media/direct_to_struct/main`, `css_l4/at_rules_keyframes/direct_to_struct/main`, `css_l4/media_queries/direct_to_struct/main`, `css_l4/vendor_and_custom_atrules/direct_to_struct/main`, `css_l4/vendor_prefixes/direct_to_struct/main`, `css_l4/custom_at_rules/direct_to_struct/main`, `css_l4/nested_layout/direct_to_struct/main`, `css_l4/nested_rules/direct_to_struct/main`, `css_l4/logical_properties/direct_to_struct/main`, `css_l4/grid/direct_to_struct/main`, `css_l4/flexbox/direct_to_struct/main`, `css_l4/typed_property_groups/direct_to_struct/main`.

**Mbps threshold (delta vs SK-V14-open)**: NONE for W4 (revert wave); each of the 24 rows above flips to `audit_overlay_verdict=AUDIT-FALSIFIED`, Outcome demoted, Verdict NO-GO. CSS L4 cell count goes from current admit posture → 0/24 ADMITTED (SYNTHESIS §1.3 honest baseline restored).

**Full-table maintain budget**: zero perf regression on the 51 JSON cells (W4 touches CSS only); ±1.0 % of `SK-V14-open` Track 1 / Track 2 cells.

**Exit gate** (measurable from bench):
1. The 7 hand-written CSS L4 template files NOT present in working tree (per SYNTHESIS §3 C-5 row + ORCHESTRATOR-PROMPT R3 PRUNE-2 line 113): `find skinny/crates/codegen/src/css_l4_templates/ -name '*.template' -o -name '*.hand'` returns empty.
2. The `include_str!`'d `generated.rs` files referencing the 7 templates DELETED.
3. `git grep -l '@generated' crates/core/src/runtime/css_l4/` returns only files produced by `cargo xtask regen-css` (R4 from W2 — sequencing dep enforced).
4. `ROLLING-SOTA-DELTA.md` shows `CSS L4: 0/24` per SYNTHESIS §1.3.
5. `skinny/REDRESS.md` carries 24 + 7 = 31 new row/template-keyed entries citing `audit-overfit/validation/v1-css-l4-validation.md §1–§6` per SYNTHESIS §3 C-5 row.
6. Every reverted row's `audit_overlay_verdict` = `AUDIT-FALSIFIED` and cites validation pack §reference.

**Revert protocol**: revert W4 PRUNE-2 delete + revert slice as one commit; restore prior 7 templates + `include_str!` paths ONLY under explicit `audit_overlay_verdict=AUDIT-FALSIFIED` (the templates were AUDIT-FALSIFIED — restoration is for diagnostic only, never admit); record REDRESS naming the failed PRUNE-2.

**Comparator strict-vs-strict plane (R1)**: CSS rows bind `comparator_plane=lightningcss full-parse` (no change from W2/W3 wiring); revert preserves the W2/W3 R1 column binding.

**Per-iter equality column (R2)**: revert preserves W2/W3 R2 column wiring; reverted rows continue to fail per-iter equality on the OLD admit data (they never had it; the admit was gate-relabel only per P-4).

### §2.5 — W5: C-1 PRUNE-3 (trait dispatch + grammar-agnostic codegen template)

**Same-wave consumer**: regen-derived runtime for every grammar emitted in the same wave (per SYNTHESIS §3 C-1 row line 271 verbatim "same-wave consumer"); per-sub-wave gate runs before commit.

**Named corpus rows it must LIFT**: NONE — W5 is an architectural refactor wave (Lock-14 dispatch collapse).

**Mbps threshold (delta vs SK-V14-open)**: ZERO regression budget on any of the 51 JSON + 24 CSS L4 cells. Lock-14 refactor is a *structural* change; runtime behavior is invariant. Per-row Track 1 / Track 2 Mbps within ±1.0 % of `SK-V14-open` (frozen-surface invariant).

**Full-table maintain budget**: all 75 rows (51 JSON + 24 CSS L4 — even AUDIT-FALSIFIED rows continue to emit numbers as baseline guard) within ±1.0 % vs `SK-V14-open`.

**Exit gate** (measurable from bench):
1. `RuntimeProvider` enum DELETED from `skinny/crates/` (per ORCHESTRATOR-PROMPT R3 PRUNE-3 lines 115-120); replaced with trait-based dispatch.
2. 8 per-grammar provider modules under `skinny/crates/codegen/` COLLAPSED to ONE grammar-agnostic generator template (per SYNTHESIS §3 C-1 row): `find skinny/crates/codegen/src -name '*provider*.rs' -o -name '*per_grammar*.rs'` returns ≤ 1 result (the generic template).
3. SYNTHESIS §3 C-1 forward invariant (line 271 verbatim): `find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO post-redress.
4. Per-row Track 1 / Track 2 Mbps within ±1.0 % of `SK-V14-open` for all 75 rows.
5. Non-JSON proof per SYNTHESIS §4 line 311 + sk-v13 G-SIMD-GRAMMAR-POLICY: CSS L4 + Sheets + BBNF-self compile / lower / cost without JSON structural roles.
6. Lock 14 baseline gate accepts the refactor (the gate rejects any commit violating the forward invariant per SYNTHESIS §3 C-1).

**Revert protocol**: revert C-1 PRUNE-3 slice as one commit (codegen + crate boundary edits); restore `RuntimeProvider` enum and 8 per-grammar modules; record REDRESS naming the failed refactor (e.g., Lock-14 invariant violated, grammar-name branch leaked into generic crate).

**Comparator strict-vs-strict plane (R1)**: refactor preserves W1 R1 column bindings (JSON planes) + W2/W3 R1 bindings (CSS planes); no comparator behavior change.

**Per-iter equality column (R2)**: refactor preserves W1 R2 column bindings (JSON) + W2/W3 R2 bindings (CSS); per-iter equality continues to run for every row.

### §2.6 — W6: C-1 PRUNE-4 (9 sub-waves: per-grammar runtime → emitted output)

**Same-wave consumer**: each sub-wave's emitted output is consumed by the existing runtime tests + bench rows on that grammar (per SYNTHESIS §3 C-1 row + `[no-deferrals]`).

**Sub-wave manifest** (9 sub-waves per audit-overfit §2.3 line 282-290 + §3.3 line 372):
- W6.1: json runtime → emitted
- W6.2: css_l4 runtime → emitted
- W6.3: css_l4_full runtime → emitted
- W6.4: css_pretty runtime → emitted (the +1 vs SK-V13 baseline per audit-overfit §2.3)
- W6.5: sheets runtime → emitted
- W6.6: bbnf-self runtime → emitted
- W6.7: runtime-helpers-shim collapse → emitted
- W6.8: typed-path-collapse (LegacyPath shim fold per audit-overfit §3.1 A6 NEW-HIGH-1)
- W6.9: regen-binding (xtask plumbing for new grammars per SYNTHESIS §3 C-1 forward invariant)

**Named corpus rows it must LIFT**: NONE — W6 is the structural runtime refactor wave.

**Mbps threshold (delta vs SK-V14-open) per sub-wave**: ZERO regression budget. Per-row Track 1 / Track 2 Mbps within ±1.0 % of `SK-V14-open` on every row belonging to the grammar being refactored AND every row on other grammars.

**Full-table maintain budget per sub-wave**: all 75 rows within ±1.0 % vs `SK-V14-open`.

**Exit gate per sub-wave** (measurable from bench):
1. `find crates/core/src/runtime/{grammar} -name '*.rs' -not -name 'generated*.rs'` returns ZERO hand-written files for the refactored grammar.
2. SYNTHESIS §3 C-1 forward invariant: `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs at W6 complete close.
3. `cargo xtask regen-{grammar}` round-trips clean for the refactored grammar (R4 from W2 generalizes per SYNTHESIS §3 C-3 row "first instance of the `regen-{grammar}` family").
4. All 75 rows within ±1.0 % of `SK-V14-open`.
5. Per-grammar emitted output consumed by existing runtime tests (per `[no-deferrals]`); test suite green.
6. Lock 14 baseline gate accepts each sub-wave commit (rejects any sub-wave violating the forward invariant).

**Exit gate for W6 as a whole** (measurable):
- W6.1–W6.9 all individually green.
- `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO directories (the forward invariant SYNTHESIS §3 C-1 row).
- Lock 14 baseline at SK-V14-W6-close shows 0 critical / 0 high (vs 11 + 7 pre-PRUNE per SYNTHESIS §1.2 / ORCHESTRATOR-PROMPT line 66).

**Revert protocol per sub-wave**: revert the sub-wave commit only (W6 is intentionally split into 9 distinct revert-able slices); record REDRESS naming the failed grammar refactor; remaining sub-waves can still proceed once the failed one is reverted + re-planned.

**Revert protocol for W6 as a whole**: if Lock 14 critical/high count fails to reach 0 at W6 close, revert ALL 9 sub-waves as one slice; restore the 67 hand-written per-grammar files; record W6 REDRESS rejection.

**Comparator strict-vs-strict plane (R1)**: refactor preserves W1 R1 column bindings (JSON) + W2/W3 R1 bindings (CSS) unchanged.

**Per-iter equality column (R2)**: refactor preserves W1 R2 column bindings (JSON) + W2/W3 R2 bindings (CSS) unchanged; per-iter equality continues per row.

### §2.7 — W7: C-4 PRUNE-5 (W8 + W9 SCAFFOLD → LOAD-BEARING)

**Same-wave consumer**: CSP-selected shape produces measurable runtime divergence on at least one named pre-wave row in the same wave (per SYNTHESIS §3 C-4 row line 274 verbatim).

**Named corpus rows it must LIFT — pre-wave hot-leaf attribution change**: `json/numbers/direct_to_struct/main` (per SYNTHESIS §3 C-4 row verbatim).

**Mbps threshold (delta vs SK-V14-open)**:
- `json/numbers/direct_to_struct/main`: Track 1 Mbps ≥ 1.05 × `SK-V14-open` Track 1 Mbps for that row (5 % uplift floor — the wave admits only if the W11.1 number-specialised symbol delivers measurable divergence; per SYNTHESIS §3 C-4 row "hot leaf attribution changes in `RESULTS.md`").
- Per SYNTHESIS §4 (line 320-322): the hardcoded P1–P8 cascade fails closed; silent fallback to the old cascade is NOT admission evidence.

**Full-table maintain budget**:
- 51 JSON cells: per-row Track 1 / Track 2 Mbps ≥ 0.98 × `SK-V14-open` (2 % regression budget for the decision-engine dispatch overhead per SYNTHESIS §4 cap).
- 24 CSS L4 cells: per-row Track 1 / Track 2 Mbps ≥ 0.98 × `SK-V14-open` (decision-engine touches both grammars per §4 "after the decision-engine resolver lands, the hardcoded P1–P8 cascade fails closed for JSON / CSS / Sheets / BBNF-self rows").

**Exit gate** (measurable from bench):
1. `json/numbers/direct_to_struct/main` `Hot leaf` column in `RESULTS.md` changes from pre-W7 `parse_value_at` to post-W7 W11.1 number-specialised symbol name explicitly (the symbol name is declared in the W7 plan; cite `crates/core/src/runtime/json/...` symbol path).
2. samply trace pre-W7 vs post-W7 shows the leaf substitution (CSP-selected shape consumed at runtime).
3. Per-shape Lock-1 triad declared in REDRESS per SYNTHESIS §4 line 317-319: `substrate_target`, `retention_lifetime`, `policy_owner` triple populated for every SIMD consumer wired by C-4; `xtask gate-json` rejects any row whose REDRESS lacks the triple.
4. C-4 shape consumer exercised across at least TWO grammar families before admit cites runtime divergence as load-bearing (per SYNTHESIS §4 line 335 verbatim "one-grammar runtime divergence is wave evidence, not admit evidence").
5. C-4 dispatch in `skinny/crates/codegen/src/lib.rs` dispatches on the CSP-emitted `BackendShape` enum ALONE; NO `match grammar { Json => ..., CssL4 => ... }` arm in the dispatch path (per SYNTHESIS §4 line 338-341 verbatim).
6. Lock 14 + non-JSON proof pass (SYNTHESIS §4 line 311-319 — G-SIMD-GRAMMAR-POLICY: consuming-grammar quote/escape/control policy or no-string policy, scalar parity, checkasm/differential coverage, same-wave measured row consumption, no public substrate API, no retained sidecar classifier state).
7. `json/numbers/direct_to_struct/main` Track 1 Mbps ≥ 1.05 × `SK-V14-open`.
8. No row regresses below 0.98 × `SK-V14-open` Track 1 / Track 2.

**Revert protocol**: revert W7 decision-engine wire-up slice as one commit; restore SCAFFOLD-ONLY status for W8 + W9; record REDRESS naming the failed dispatch (e.g., hot-leaf attribution did not change, grammar-name branch leaked, Lock-1 triad incomplete, non-JSON consumer absent).

**Comparator strict-vs-strict plane (R1)**: preserves W1 R1 column bindings (JSON) + W2/W3 R1 bindings (CSS) unchanged.

**Per-iter equality column (R2)**: preserves W1 R2 column bindings (JSON) + W2/W3 R2 bindings (CSS) unchanged.

### §2.8 — W8: R6 CSS L4 re-admit (24 features)

**Same-wave consumer**: each CSS L4 feature's grammar-derived parser (from W2 R4 regen + W6.2 + W6.3 + W6.4) feeds the W8 admit gate per feature.

**Named corpus rows it must LIFT**: all 24 CSS L4 features (verbatim from §2.4 above). Each feature admits when `Track 1 Mbps > lightningcss Mbps + 1 Mbps` AND `cssparser_oracle equality PASS per iter` (R6 verbatim from SYNTHESIS §0.3 row 98).

**Mbps threshold (delta vs SK-V14-open)** — per CSS L4 feature row:
- Track 1 Mbps ≥ `comparator_plane lightningcss Mbps + 1 Mbps` (the §0.1 R10 close-condition `Track 1 > comparator strict + 1` verbatim).
- Track 1 Mbps measured on `skinny/corpora/css-l4-sk-v14/` (≥ 800 KB; per §2.3 W3 exit gate). Tiny-fixture cells DO NOT admit (P-3 pre-block enforced).
- Round-trip rule trigger: any CSS feature whose Track 1 Mbps exceeds the same-plane lightningcss comparator by ≥ 50× triggers user re-pin (per SYNTHESIS §0.4 P-1 line 116-120). The CH7 round-trip rule armed at W3 fires here.

**Full-table maintain budget**:
- 51 JSON cells: ±1.0 % vs `SK-V14-open` (W8 touches CSS only).
- 23 non-target CSS L4 cells per sub-attempt: each feature admit must not regress the other 23 by more than 2 % vs `SK-V14-open`.

**Exit gate** (measurable from bench, per feature):
1. `comparator_plane=lightningcss full-parse` (R1 binding from §1.3 above).
2. `per_iter_equality` PASS on every iter (R2 verbatim from SYNTHESIS §0.3).
3. Track 1 Mbps > lightningcss Mbps + 1 Mbps on `skinny/corpora/css-l4-sk-v14/` measurement.
4. Feature-coverage match: every CSS variant lightningcss accepts, the row accepts; every variant lightningcss rejects, the row rejects (per SYNTHESIS §0.1 verbatim).
5. `audit_overlay_verdict=AUDIT-SUSTAINED` (the row has graduated from W4 AUDIT-FALSIFIED to W8 honest admit).
6. Round-trip rule NOT triggered: Track 1 Mbps < 50 × lightningcss Mbps (any feature crossing this triggers escalate-immediately per ORCHESTRATOR-PROMPT line 190).
7. Same-wave consumer present: emitted grammar-derived parser from W2/W6 is the Track 1 path; Track 2 is independent oracle (cssparser); `xtask gate-json` rejects any row whose `track2_entry_point` shares ancestor with `track1_entry_point` beyond public `Tape` / `OffsetFlags`.

**Exit gate for W8 as a whole**:
- All 24 features either ADMIT (Track 1 > lightningcss + 1; per-iter equality PASS) OR carry architectural-block proof per SYNTHESIS §0.1 row family. Implementation-limited misses are REOPEN (NOT close-eligible).

**Revert protocol per feature**: revert the per-feature admit commit; row reverts to W4 `audit_overlay_verdict=AUDIT-FALSIFIED` posture; record REDRESS naming the failed feature (threshold miss, equality fail, round-trip-rule trigger, coverage mismatch).

**Revert protocol for W8 as a whole**: any feature that REOPENs blocks SK-V14 close per §0.1 R10 indefatigable clause; SK-V15 brackets opens per `[execute-planned-architecture]`.

**Comparator strict-vs-strict plane (R1)**: `comparator_plane=lightningcss full-parse` per row; `xtask gate-json` rejects any CSS row binding fact-stream comparator (per SYNTHESIS §0.3 R6 verbatim: "no fact-stream vs full-AST asymmetry").

**Per-iter equality column (R2)**: equality verified inside timing region per iter; cssparser_oracle returns AST-equivalence boolean per iter; `xtask gate-json` rejects empty per-iter equality cells.

### §2.9 — W9: R7-direct fused R7-typed JSON re-admit (17 direct + 17 typed corpora)

**Same-wave consumer**: bench harness consumes the W1 rebound `sonic-rs strict struct deser` comparator on every direct row AND the W1 rebound `per-corpus typed struct deser` comparator on every typed row.

**Named corpus rows it must LIFT (direct plane)**: all 17 JSON `direct_to_struct` rows (verbatim from `skinny/RESULTS.md`): `json/twitter/direct_to_struct/main`, `json/citm_catalog/direct_to_struct/main`, `json/canada/direct_to_struct/main`, `json/apache_builds/direct_to_struct/main`, `json/github_events/direct_to_struct/main`, `json/update_center/direct_to_struct/main`, `json/mesh/direct_to_struct/main`, `json/random/direct_to_struct/main`, `json/gsoc-2018/direct_to_struct/main`, `json/marine_ik/direct_to_struct/main`, `json/instruments/direct_to_struct/main`, `json/numbers/direct_to_struct/main`, `json/unicode_mixed/direct_to_struct/main`, `json/unicode_escapes/direct_to_struct/main`, `json/unicode_basic/direct_to_struct/main`, `json/distinct_values/direct_to_struct/main`, `json/y_string_unicode/direct_to_struct/main`.

**Named corpus rows it must LIFT (typed plane)**: all 17 JSON `real_typed_struct` rows (verbatim from `skinny/RESULTS.md`): `json/twitter/real_typed_struct/main`, `json/citm_catalog/real_typed_struct/main`, `json/canada/real_typed_struct/main`, `json/apache_builds/real_typed_struct/main`, `json/github_events/real_typed_struct/main`, `json/update_center/real_typed_struct/main`, `json/mesh/real_typed_struct/main`, `json/random/real_typed_struct/main`, `json/gsoc-2018/real_typed_struct/main`, `json/marine_ik/real_typed_struct/main`, `json/instruments/real_typed_struct/main`, `json/numbers/real_typed_struct/main`, `json/unicode_mixed/real_typed_struct/main`, `json/unicode_escapes/real_typed_struct/main`, `json/unicode_basic/real_typed_struct/main`, `json/distinct_values/real_typed_struct/main`, `json/y_string_unicode/real_typed_struct/main`.

(Note: 6 of the typed rows — `json/canada/real_typed_struct/main`, `json/gsoc-2018/real_typed_struct/main`, `json/unicode_mixed/real_typed_struct/main`, `json/unicode_escapes/real_typed_struct/main`, `json/distinct_values/real_typed_struct/main`, `json/y_string_unicode/real_typed_struct/main` — are NOT in current `RESULTS.md` typed plane and represent fresh admit attempts per SYNTHESIS §0.1 "all 17 reopen".)

**Mbps threshold (delta vs SK-V14-open)** — per direct row:
- Track 1 Mbps ≥ `sonic-rs strict struct deser Mbps + 1 Mbps` (the §0.1 R10 close-condition `Track 1 > comparator strict + 1` verbatim).
- Track 1 Mbps measured on the existing JSON corpora (Bytemark, twitter, citm_catalog, etc.); no corpus-size floor change (JSON corpora are already >1 KB).
- Per-corpus typed struct (matching the corpus shape) is the comparator; eager DOM comparator FORBIDDEN per P-2 pre-block.

**Mbps threshold (delta vs SK-V14-open)** — per typed row:
- Track 1 Mbps ≥ `per-corpus typed struct deser Mbps + 1 Mbps` (the §0.1 R10 close-condition verbatim).
- Track 1 Mbps measured on the named JSON corpora.

**Full-table maintain budget**:
- 17 parse_only cells (W10 not yet admitted): ±1.0 % vs `SK-V14-open`.
- 24 CSS L4 cells: ±1.0 % vs `SK-V14-open` (W9 is JSON-only).
- 16 non-target direct cells per per-corpus admit attempt: each ≥ 0.98 × `SK-V14-open` (2 % regression budget per row sub-attempt).
- 16 non-target typed cells per per-corpus admit attempt: each ≥ 0.98 × `SK-V14-open`.

**Exit gate** (measurable from bench, per direct corpus):
1. `comparator_plane=<corpus>::strict_struct_deser` (R1 binding from §1.3 above; NOT `sonic_rs::from_slice::<Value>`).
2. `per_iter_equality` PASS on every iter (R2 verbatim).
3. Track 1 Mbps > sonic-rs strict struct deser Mbps + 1 Mbps on the named corpus.
4. `audit_overlay_verdict=AUDIT-SUSTAINED` for the row (or `AUDIT-FALSIFIED` reopened to fresh ADMIT).
5. Track 2 oracle (independent serde struct deser) returns equality per iter.
6. Per SYNTHESIS §3 C-2 row + §2.1 W1 exit gate: equality column non-empty AND PASS per iter.
7. Same-wave consumer present: generated `direct_to_struct` parser (from W2-derived `regen-{grammar}` family OR existing W6.1 emitted json runtime) is Track 1 path; Track 2 is independent oracle.

**Exit gate** (measurable from bench, per typed corpus):
1. `comparator_plane=<corpus>::typed_strict_struct_deser` (R1 binding from §1.3 above).
2. `per_iter_equality` PASS on every iter (R2 verbatim).
3. Track 1 Mbps > per-corpus typed struct deser Mbps + 1 Mbps on the named corpus.
4. Track 2 oracle independent typed deser (serde-based) returns equality per iter.
5. `track2_entry_point` does NOT share ancestor with Track 1 entry point beyond public `Tape` / `OffsetFlags` (SYNTHESIS §2 column rule).

**Exit gate for W9 as a whole**:
- All 17 direct rows either ADMIT (Track 1 > sonic-rs strict struct deser + 1) OR carry architectural-block proof per SYNTHESIS §0.1.
- All 17 typed rows either ADMIT OR carry architectural-block proof per SYNTHESIS §0.1.

**Revert protocol per corpus**: revert the per-corpus admit (per plane); row stays at W1 `audit_overlay_verdict=AUDIT-FALSIFIED` posture; REDRESS records the threshold miss.

**Comparator strict-vs-strict plane (R1)**: direct plane binds `comparator_plane=<corpus>::strict_struct_deser` (per-corpus typed deser); typed plane binds `comparator_plane=<corpus>::typed_strict_struct_deser`; `xtask gate-json` rejects any direct or typed row binding `sonic_rs::from_slice::<Value>` or any other eager-DOM comparator per P-2 pre-block.

**Per-iter equality column (R2)**: direct plane: per-iter equality oracle reads back the struct and digest-compares per iter; typed plane: typed struct equality comparison per iter inside timing region; rejects empty cells across both planes.

### §2.10 — W10: R8 JSON `parse_only` distinct path + admit (17 corpora)

**Same-wave consumer**: the distinct `parse_only` code path in `generated_json` (per ORCHESTRATOR-PROMPT R8 lines 147-149: "no full-tape build"); wired to sonic-rs Skipper-class comparator.

**Named corpus rows it must LIFT**: all 17 JSON `parse_only` rows (verbatim from `skinny/RESULTS.md`): `json/twitter/parse_only/main`, `json/citm_catalog/parse_only/main`, `json/canada/parse_only/main`, `json/apache_builds/parse_only/main`, `json/github_events/parse_only/main`, `json/update_center/parse_only/main`, `json/mesh/parse_only/main`, `json/random/parse_only/main`, `json/gsoc-2018/parse_only/main`, `json/marine_ik/parse_only/main`, `json/instruments/parse_only/main`, `json/numbers/parse_only/main`, `json/unicode_mixed/parse_only/main`, `json/unicode_escapes/parse_only/main`, `json/unicode_basic/parse_only/main`, `json/distinct_values/parse_only/main`, `json/y_string_unicode/parse_only/main`.

**Mbps threshold (delta vs SK-V14-open)** — per parse_only row:
- Track 1 Mbps ≥ `sonic_rs::Skipper Mbps + 1 Mbps` (the §0.1 R10 close-condition verbatim).
- Track 1 Mbps measured on the existing JSON corpora.
- Sonic-rs Skipper is structural-skip-only (no value materialization); Track 1 must perform structural-skip equivalent work (NO full-tape build per ORCHESTRATOR-PROMPT R8 line 148).

**Full-table maintain budget**:
- 17 direct cells (W9 outcome dependent): each ≥ 0.98 × post-W9 floor.
- 17 typed cells (W9 outcome dependent): each ≥ 0.98 × post-W9 floor.
- 24 CSS L4 cells: ±1.0 % vs `SK-V14-open`.
- 16 non-target parse_only cells per per-corpus admit attempt: each ≥ 0.98 × `SK-V14-open`.

**Exit gate** (measurable from bench, per corpus):
1. `comparator_plane=sonic_rs::Skipper` (R1 binding from §1.3 above; structural-skip-only).
2. `per_iter_equality` PASS on every iter (R2 verbatim — per-iter parse-equality verified inside timing region; e.g., structural cursor offset matches between Track 1 and Track 2 per iter).
3. Track 1 Mbps > sonic_rs::Skipper Mbps + 1 Mbps.
4. Distinct `parse_only` code path EXISTS in `generated_json` (cite path:line in W10 plan; e.g., `crates/core/src/runtime/json/parse_only.rs`) — NOT a flag-gated branch of the full-tape parser.
5. NO full-tape allocation in the parse_only path (per ORCHESTRATOR-PROMPT R8 line 148): `cargo asm` shows no `Tape::push` calls in the parse_only emission.
6. `audit_overlay_verdict=AUDIT-SUSTAINED` (graduated from W1 AUDIT-FALSIFIED).
7. Track 2 oracle independent sonic_rs::Skipper invocation returns equality per iter.
8. Stage-0 F-V2-P1ABC-RERECORD shipped UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding — W10 is the bound wave per the 5-step inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands.

**Exit gate for W10 as a whole**:
- All 17 parse_only rows either ADMIT OR carry architectural-block proof per SYNTHESIS §0.1.

**Revert protocol per corpus**: revert per-corpus admit; row stays at W1 AUDIT-FALSIFIED posture (the W14.1–.5 fake-admit revert holds); REDRESS records threshold miss.

**Revert protocol for W10 as a whole**: any row that REOPENs blocks SK-V14 close per §0.1 indefatigable clause; SK-V15 opens.

**Comparator strict-vs-strict plane (R1)**: `comparator_plane=sonic_rs::Skipper`; `xtask gate-json` rejects any parse_only row binding `sonic_rs::from_slice::<Value>` (eager DOM) or any non-Skipper comparator per P-2.

**Per-iter equality column (R2)**: per-iter structural-equality oracle: structural cursor offsets compared per iter inside timing region; rejects empty cells.

### §2.11 — W11: Close ceremony + Alpha feedback

**Same-wave consumer**: docs / RESULTS / REDRESS / HANDOFF / SPEC reconciliation only (per SPEC §2 line 248 + §14); no source LOC, no implementation, no behavior delta.

**Named corpus rows it must LIFT**: NONE — W11 is ceremonial. Every row family disposition (51 JSON cells + 24 CSS L4 features) is read out of W1–W10 outcomes; SK-V14 closes when all dispositions meet the §0.1 R10 bar OR carry architectural-block proof.

**Mbps threshold (delta vs SK-V14-open)**: NONE. W11 does no benching; throughput numbers are inherited from W10 close-state.

**Full-table maintain budget**: all 75 rows hold their W10-close cell-state (no perf measurement; this is reconciliation only).

**Exit gate** (measurable from documentation):
1. `skinny/RESULTS.md` cell-state reconciled with `skinny/ROLLING-SOTA-DELTA.md` for every row in the 75-row matrix; `xtask gate-json` accepts the post-W10 manifest.
2. `skinny/REDRESS.md` carries final-state entries for every REOPEN row + every architectural-block proof.
3. `restart/skinny/tranches/sk-v14/HANDOFF.md` reconciled: REDRESS 119/120 HISTORY-only carry preserved; every wave-level disposition (W0..W10) cited.
4. SK-V14 close-honesty checklist passes (per SPEC §14): every row family meets the §0.1 R10 bar OR carries architectural-block proof; ZERO gate-relabel admits (P-4 pre-block).
5. Alpha feedback (per `ORCHESTRATOR-PROMPT §SK LOOP` close step): R10 indefatigable clause discharged.
6. If any row family REOPENs without architectural-block proof, SK-V15 bracket opens per `[execute-planned-architecture]`.

**Revert protocol**: W11 is documentation-only; "revert" means restore prior `skinny/RESULTS.md` / `HANDOFF.md` / `REDRESS.md` if the close ceremony writes inconsistent state. Record REDRESS naming the inconsistency.

**Comparator strict-vs-strict plane (R1)**: W11 preserves all R1 column bindings inherited from W1–W10; no comparator behavior change.

**Per-iter equality column (R2)**: W11 preserves all R2 column bindings inherited from W1–W10; no per-iter equality behavior change.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds — summary)

Every gate in §2 binds:
- (a) named corpus rows verbatim from `skinny/RESULTS.md` enumeration
- (b) Mbps threshold delta vs `SK-V14-open` (NOT vs sonic-rs strict directly; vs `SK-V14-open` per ORCHESTRATOR.md §8)
- (c) full-table maintain budget (every row in the 75-row matrix)
- (d) measurable exit gate (`xtask gate-json` rule + bench cell + audit overlay verdict)
- (e) revert protocol (commit-sliced; never hand-edited per `[clean-regen-discipline]`)
- (f) R1 strict-vs-strict comparator plane binding (per plane: parse_only sonic_rs::Skipper / direct sonic-rs strict struct deser per corpus / typed per-corpus typed struct deser / CSS lightningcss full-parse)
- (g) R2 per-iter equality binding (`xtask gate-json` rejects empty per-iter equality cells per SYNTHESIS §0.3 R2 verbatim)

UNMEASURABLE gates rejected at this V1 pass: **ZERO**. Every wave's gate is measurable from the bench (gate-json column rule + Mbps threshold OR structural commit-evidence + zero-regression budget). Specifically:
- W0: column-population gate (gate-json rejection rule, measurable).
- W1: per-row comparator/equality column + cost-overhead gate + row-revert gate (Mbps + commit-evidence + ROLLING-SOTA-DELTA cell-state, measurable).
- W2: round-trip xtask exit code (measurable).
- W3: `du` corpora floor + provenance (measurable).
- W4: file-existence + commit-evidence + ROLLING-SOTA-DELTA cell-state (measurable).
- W5: forward-invariant grep + Mbps regression budget (measurable).
- W6 + 9 sub-waves: forward-invariant grep + per-sub-wave Mbps regression budget (measurable).
- W7: hot-leaf attribution change + samply trace + Lock-1 triad presence + Mbps uplift floor (measurable; the hot-leaf attribution change is samply-observable per SYNTHESIS §3 C-4 row verbatim).
- W8 + 24 features: Mbps threshold vs lightningcss + per-iter equality + round-trip-rule trigger (measurable).
- W9 + 17 direct + 17 typed corpora: Mbps threshold vs per-corpus sonic-rs strict struct deser (direct) + per-corpus typed struct deser (typed) + per-iter equality (measurable).
- W10 + 17 corpora: Mbps threshold vs sonic_rs::Skipper + per-iter equality + no-full-tape `cargo asm` evidence (measurable).
- W11: documentation reconciliation gate (RESULTS/REDRESS/HANDOFF/SPEC consistency check; close-honesty checklist, measurable from doc state).

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

Per SYNTHESIS §0.4 P-1..P-7 + §5 carry-forward list + S-P3 dispatch context §1 architectural sequencing constraints + the dispatch instruction "REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 119/120, 126":

**Pattern-level pre-blocks (P-1..P-7 every wave consumes; from SYNTHESIS §0.4 lines 109-148):**
- **P-1** — Fake `@generated` header on hand-written templates. W2 + W3 + W4 + W5 + W6 + W8 + every CSS-touching wave must NOT re-emit hand-patched templates carrying `@generated`. The CH7 round-trip rule fires at W3 + W8 on any feature whose claimed Mbps exceeds same-plane SOTA by ≥ 50×.
- **P-2** — `sonic_rs::from_slice::<Value>` mislabelled as strict comparator. W1 + W9 + W10 must NOT re-bind eager-DOM comparator under any plane label.
- **P-3** — Tiny-fixture Criterion-overhead Mbps inflation. W8 must NOT admit on <1 KB CSS fixtures (R5 corpora floor = 800 KB enforced at W3; cells measured on <1 KB fixtures cannot admit).
- **P-4** — Gate-relabel as admit. W8 + W9 + W10 admit requires a parser/codegen source delta cited per row + measurement evidence per REDRESS — NOT a gate.rs / report.rs / lock14_baseline.rs touch only.
- **P-5** — Scaffold-research counted as load-bearing. W7 + every wave citing W8/W9 must NOT cite scaffold-only as evidence; W7 PRUNE-5 wires both end-to-end + measurable runtime divergence on named pre-wave row.
- **P-6** — Per-grammar provider modules in generic codegen. W5 + W6 must collapse 8 provider modules to ONE generic generator template (per SYNTHESIS §3 C-1 forward invariant).
- **P-7** — Track 1 ≡ Track 2 dishonesty. EVERY admit wave (W8 + W9 + W10) must keep Track 1 (generated) structurally distinct from Track 2 (independent oracle); `xtask gate-json` rejects rows where `track2_entry_point` shares ancestor with Track 1 entry point beyond public `Tape` / `OffsetFlags` per SYNTHESIS §2 column rule.

**REDRESS pre-block list (per S-P3 dispatch instruction `[no-orphan-redress]` ledger):**

| Wave | REDRESS items NOT to re-open | Source |
|---|---|---|
| W0 | none (telemetry wave) | — |
| W1 | REDRESS 119, 120 (HISTORY only per HANDOFF.md:131); REDRESS 88, 89 (PMULL prefix-XOR default hot body, CTZ/bulk production consumer as performance evidence — comparator rebind is comparator, not perf claim); fused C-5 revert leg CREATES new REDRESS entries 22 row-keyed | sk-v8/SPEC.md:794-797; sk-v14/HANDOFF.md:131 |
| W2 | REDRESS 28+33 (Class A NEON/TBL tiny-string wiring as parse close — wave is xtask, not parse close); REDRESS 96-98 (carried-forward CSS-template fake `@generated` route per S-P3 dispatch) | sk-v8/SPEC.md:779-780 |
| W3 | REDRESS 28+33 (Class A NEON/TBL tiny-string wiring as parse close — wave is corpora staging, not parse close); REDRESS 96-98 (carried-forward CSS-template fake `@generated` route per S-P3 dispatch) | sk-v8/SPEC.md:779-780 |
| W4 | REDRESS 50-55 (no-allocation visitor, parse-time aux side tables, EventCursor, parser-local structural-mask cursor, decoded stats sink, quote-source fused string materializer); REDRESS 60-72 (retained string-boundary collapse, always-wide / delayed-wide scanning, Unicode validator/classifier retries, object/key carry, global/direct/Track 2 cap-16, generated-retained StringBlock16 tiny probe, direct source-hook/materialization families, parser-owned scratch, byte-output unescape, semantic string facts, hand typed sinks as proof, stale mantissa widening, raw f64 shortcut); REDRESS 80 (REDRESS-80 source-hook variant); REDRESS 82-84 (single-quartet Unicode classifier, StringBlock16 tiny probe, object-pair value-byte control compaction); REDRESS 88, 89 (PMULL/CTZ/B6 canary as performance evidence — CSS revert is not perf) | sk-v8/SPEC.md:785-797 |
| W5 | REDRESS 36-38 (Lock 14 residue, old JSON helpers, generic JSON branches, `StructuralAlphabet::json`); REDRESS 85-86 (Lock 14 baseline allowlist drift); REDRESS 50-55 (visitor / aux side tables / EventCursor); REDRESS 60-72 (string-boundary / direct source-hook / parser-owned scratch); REDRESS 126 (carried-forward dispatch route per S-P3 dispatch) | sk-v8/SPEC.md:781-790; sk-v14 dispatch §1 |
| W6 | same as W5 (PRUNE-3 + PRUNE-4 share the Lock-14 pre-block surface); additionally REDRESS 49 (no-allocation visitor) | sk-v8/SPEC.md:785 |
| W7 | REDRESS 49-55 (W7 is decision-engine wire-up, not parser-owned cursor / visitor / aux tables); REDRESS 88, 89 (PMULL/CTZ as performance evidence — decision-engine measurement is hot-leaf attribution, not SIMD performance claim); REDRESS 119, 120 (HISTORY-only per HANDOFF) | sk-v8/SPEC.md:785, 796-797; sk-v14/HANDOFF.md:131 |
| W8 | REDRESS 28+33 (Class A NEON/TBL tiny-string wiring as CSS close); REDRESS 96-98 (CSS-template fake @generated); REDRESS 119, 120 (HISTORY only) | sk-v8/SPEC.md:779; sk-v14 dispatch §1 |
| W9 | REDRESS 50-55 (visitor / aux tables); REDRESS 60-72 (string-boundary / direct source-hook); REDRESS 80, 82-84; REDRESS 88, 89 (perf evidence vs admit gate); REDRESS 119, 120 (HISTORY); REDRESS 126 (W9 fuses direct + typed planes; both share pre-block surface) | sk-v8/SPEC.md:785-797; sk-v14 dispatch §1 |
| W10 | REDRESS 50-55, 60-72, 80, 82-84, 88, 89, 119, 120, 126; ADDITIONALLY REDRESS 49 (no-allocation visitor as parse_only path — must NOT re-open; the W10 distinct parse_only path is a CODEGEN delta, not a visitor) | sk-v8/SPEC.md:783-797 |
| W11 | none (close ceremony — documentation reconciliation only; no source/parser/codegen edits) | — |

**Wave-program-level pre-blocks (every wave inherits per SYNTHESIS §5 unblocked/blocked list + sk-v8/SPEC.md §10):**
- No new BBNF directive, BIR variant, `BackendShape` variant, public substrate API, `UnionTape`, parser-owned cursor/facts, sidecar substrate, parallel substrate (SYNTHESIS §4 line 309-311; sk-v8/SPEC.md §10 line 763-766).
- No strict admission from sidecar / permissive / lossy / stale comparator evidence (SYNTHESIS §5 lossy sonic-rs/RapidJSON forbidden).
- No `tape_vs_tape`, `parse_only`-as-telemetry, or telemetry rows as W3 production consumer (sk-v8 §10).
- No orphan primitives, checkasm-only admission, harness-only hardening as performance proof (sk-v8 §10).
- No Track 1 / Track 2 coupling or benchmark-private parsers (SYNTHESIS §0.4 P-7).
- No grammar-name branches in generic crates, parser-owned sidecars, hidden Track 1 ≡ Track 2 coupling, stale comparator sidecars (SYNTHESIS §5).

## §5 — Sources (every upstream artefact cited)

- `restart/skinny/tranches/sk-v14/research/p3/S-P3-DISPATCH-CONTEXT.md` (full; §0–§5).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (§0.1 R10 close, §0.3 R1–R10, §0.4 P-1..P-7, §1.2 audit-falsified list, §1.3 honest baseline, §2 telemetry binding, §3 C-1..C-5 candidates, §4 S-P3 constraints, §5 pre-blocked routes).
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` (R1–R10 verbatim lines 96-158; THE SK LOOP lines 161-172; discipline lines 175-185; escalate lines 188-194).
- `restart/skinny/tranches/sk-v14/HANDOFF.md` (REDRESS 119/120 HISTORY-only carry per :131).
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (§2.1 R4-before-PRUNE-2 sequencing line 240-260, §2.2 C-1-before-C-4 line 261-281, §2.3 PRUNE-4 9 sub-waves line 282-292, §3.1 candidate-to-finding coverage line 339-358, §3.3 sub-wave count summary line 372+).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (§3Z LOCK at line 36+; cohort confirmation that the C-1..C-5 candidate pool is LOCKED with zero orphan REVISE).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (§2.1 frontmatter; §3 CH1 measurability rule line 110-114; §8.4 same-row falsifiability gate; §8.5 same-wave-consumer rule).
- `restart/skinny/tranches/sk-v8/SPEC.md` (§0.1 close-condition shape; §0.4 24-column telemetry schema; §10 pre-blocked routes line 756-801; SK-V14 telemetry-binding precedent).
- `skinny/RESULTS.md` (lines 1-124 corpus row enumeration: 17 JSON corpora × 3 planes (51 cells); 24 CSS L4 features; row identifiers verbatim per §2 above).
- `skinny/REDRESS.md` (line 1-92 structure + headings; ~5041 lines total; per-wave REDRESS pre-block surface per §4 above).
- `restart/locks/LOCKS.md` (Lock 1 substrate-target triad per SYNTHESIS §4 line 317-319; Lock 14 baseline gate per SYNTHESIS §3 C-1 forward invariant; Lock 16 NOT-PRESENT-at-HEAD admission per S-P3 dispatch §1).
- `restart/prompts/ORCHESTRATOR.md` (§8 baseline-anchored measurement; §3Z convergence; §3W CH7 binding).
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` (§CH7 round-trip rule; P-1 fake @generated header trigger).
