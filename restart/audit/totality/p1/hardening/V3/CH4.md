---
agent: CH4
pass: T-P1-excavation
cycle: V3
lens: COST
generated_at: 2026-05-23T18:25:00-04:00
disposition: ACCEPT
audited_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md (V3)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V2-LOCKED)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V3)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V3)
  - restart/audit/totality/p1/1E-locks-evidence.md (V2-LOCKED)
  - restart/audit/totality/p1/1F-anti-pattern.md (V3)
  - restart/audit/totality/p1/1F-coherence-scan.md (V2-LOCKED)
  - restart/audit/totality/p1/1F-past-corpora.md (V2-LOCKED)
authority:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3 (CH4)
  - restart/prompts/ORCHESTRATOR.md §3W + §3Z
  - restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/hardening/V2/CH4.md (V2 baseline — 8/8 ACCEPT)
  - restart/audit/totality/p1/hardening/V1/CH4.md (V1 baseline — 8/8 ACCEPT)
v3_head_commit: 0a9f1288c
accept_count: 8
revise_count: 0
reject_count: 0
acceptance_rate: 8/8 (100%)
cycle_role: confirming-cycle for §3Z cohort LOCK (V3 = first cohort-wide ≥95% cycle; V4 = second consecutive required)
---

## §1 Lens Basis

`restart/prompts/totality/PASS-1-EXCAVATION.md §3` CH4 requires (a) every
divergence carry a realistic LOC-delta + risk class, and (b) 1E amendment
candidates state a wave-alignment hint. `restart/prompts/ORCHESTRATOR.md:86`
sharpens to a six-field schema per kernel/primitive: `loc_budget | risk |
wave | hard_cap | same_wave_consumer | evidence_basis`. V3 dispatch context
at `restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md:29` narrows
the V3 confirming-cycle CH4 focus to three convergence points: (i) verify
V3 fold LOC counts within the 1.2-1.4× hard-cap convention; (ii) verify the
1C LOC repair rescale (~50→~190 LOC + 2.5× consumer-rewire band) holds at
V3 HEAD — now denominated against a **127**-symbol consumer surface (V2 was
126); (iii) verify AP-020 LOC band (40-120 LOC / 160 LOC cap; 1.33×) intact.
The V1 + V2 CH4 cycles each disposed ACCEPT 8/8 with 16/16 LAC
wave-alignment hits. V3 is the first cohort-wide ≥95% cycle for T-P1; CH4
is the steady-state half of that cohort and must confirm the V2 100%
without retreat for cohort LOCK trajectory.

## §2 Cycle Verdict

ACCEPT. All eight T-P1 inventories carry full CH4 cost framing at V3 HEAD
under the orchestrator six-field schema. Each V3 dispatch-context
convergence point is verifiable at HEAD (commit `0a9f1288c`):

- **1C LOC repair rescale holds at the 127-symbol surface.** The V3 fold
  at `1C-runtime-evidence.md:162` reads `80 (root rewrite) + ~2.5×
  consumer-rewire band proportional to 127-symbol surface` (V2 read 126;
  V3 lifts to 127 to absorb the css_l4 41→43 split + google_sheets 12→11
  correction caught beyond the named scope per dispatch context §1). The
  47-line / 30-site / 15-file / **127**-symbol mechanical-extraction
  census at `1C-runtime-evidence.md:200-201` reproduces verbatim at HEAD:
  `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l` returns **30**;
  `rg -l ...` returns **15**; `awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs | wc -l` returns **47**;
  per-grammar sum `10+10+43+10+10+10+11+13+10 = 127` arithmetically
  reconciles. The two-component (root + consumer-rewire-band) figure
  composes correctly; the 2.5× multiplier remains proportional to the
  consumer-surface scale (now 127 symbols, ≈+0.8% over V2's 126).
- **AP-020 LOC band intact at 40-120 / 160 cap.** `1F-anti-pattern.md:105`
  reads `40-120 LOC fence/classification | medium-high risk | CSS evidence-accounting wave (co-wave with AP-009) | 160 LOC hard cap | CSS comparator-sidecar fence consumer`.
  V3 evidence_basis rebinds the cite cluster to
  `:648, 2691, 1082, 1203, 1354, 1511, 1661, 1815, 1964` (V2's
  `:222,234,299,504` were fabricated per HARDENING-T-P1-V2-CONSOLIDATED
  §3.1 F-V3-CH7-1). 160 / 120 = **1.33×**, conforming to V1 CH4
  1.2-1.4× hard-cap convention. AP-009 companion at `:94` retains
  60-160 LOC classification / 220 LOC cap; the two rows partition cleanly
  (AP-009 = classification framing; AP-020 = sidecar-as-anti-pattern
  fencing) with no LOC double-counting.
- **V3 fold LOC counts uniformly within 1.2-1.4× convention.** Spot-check
  at V3 HEAD: AP-020 160/120 = 1.33×; 1B-D8 600/500 = 1.20×; 1B-D10
  650/500 = 1.30×; 1A-DIV-008 1,100/900 = 1.22×; LAC-1E-12 240/180 =
  1.33×; LAC-1E-15 11,000/8,000 = 1.375×. All within convention. The V3
  micro-fold introduces zero new hard-cap violations and zero new LOC
  obligation beyond the explicit V3-amended cite rebinds.

The V2 CH4 100% ACCEPT carries forward without retreat. §3Z gate: V3 is
first cohort-wide ≥95% cycle; CH4 contributes ACCEPT 8/8 = 100% to that
cycle; V4 confirming cycle required for cohort LOCK.

## §3 Per-Artefact Verdict Table

| Artefact | V3 disposition | CH4 six-field schema present at V3 HEAD | Notes |
|---|---|---|---|
| 1A-substrate-evidence.md (V3) | ACCEPT | Yes — divergence-table header at `1A-substrate-evidence.md:75` carries `loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis`; 8 divergence rows at `:77-84` populate every column; 1A-LOCK1-AMEND-001 candidate row at `:113` carries the same six-field schema. | V3 amendment is SUB-014 cite rebind only (per V3 dispatch context §1 — no cost-relevant content change). 113 lines preserved verbatim from V2. 1A-DIV-008 substrate-union nuance row at `:84` keeps `400-900 LOC for cursor unification; 100-300 LOC if Pass Omega ratifies two-cursor shape` cost frame unchanged from V2. CH7 V2 V3-fold cite-rebind nuances do not alter the cost class. |
| 1B-codegen-evidence.md (V2-LOCKED) | ACCEPT | Yes — divergence-table header at `1B-codegen-evidence.md:77` carries all six fields; 13 divergence rows at `:79-90` populate them; the D8/D10 split rows at `:86-87` each carry distinct LOC budgets, hard caps, and same-wave consumer obligations; 3 amendment candidates at `:114-116` populate the same six-field schema. | V2-LOCKED — zero V3 drift confirmed by line-by-line read against V2 baseline. D8/D10 split LOC budgets (250-500 each) + distinct 600 / 650 hard caps + distinct same-wave consumers preserve. NECESSARY-BUT-INSUFFICIENT-relative-to-PRUNE-4 framing intact. |
| 1C-runtime-evidence.md (V3) | ACCEPT | Yes — divergence-table header at `1C-runtime-evidence.md:157` carries all six fields; 11 divergence rows at `:159-169` populate them. | V3 fold rescales 1C-D4 from V2's 126-symbol surface to **127** symbols at `:162` (`80 (root rewrite) + ~2.5× consumer-rewire band proportional to 127-symbol surface`). The 2.5× multiplier is preserved verbatim; the symbol-count rescale absorbs (a) css_l4 41→43 (3 css_l4-named aliases CssRule/CssDeclaration/CssSelector at `mod.rs:34-35` counted inside the 43) and (b) google_sheets 12→11 (one neutral caught beyond V2 scope). Per-grammar sum verification at HEAD: bbnf 10 + bnf 10 + css_l4 **43** + css_pretty 10 + csv 10 + ebnf 10 + google_sheets **11** + json 13 + math 10 = **127**. Reproducible via `awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs` (returns 47 lines holding 127 grammar-named symbols at HEAD per the §6 NEW-CH2-V2-03 enumeration discipline). `locks_amendment_candidates: 0` (1C defers all amendment surfacing to 1E). |
| 1D-skinny-lessons.md (V3) | ACCEPT | Yes — divergence-table header at `1D-skinny-lessons.md:138` carries all six fields; 17 divergence rows at `:140-158` populate them. | V3 fold is cite-rebind only at three cells (per V3 dispatch context §1): proof-witness `runtime/src/lib.rs:9` → `:29-33` at `:157`; Track 2 `track2/json.rs:5,24,43` → `:7,26,45` at `:157`; row 100 → row 117 cosmetic at `:117`. No cost-relevant content change; the V2 LOC framing at every divergence row preserves bit-for-bit. Row 113 `bbnf-simd` substrate lesson at `:131` retains the SK-V14 R4 regen-css xtask wave-alignment hint through three sibling rows + the R4 → PRUNE-2 sequencing row at `:149` (`0 LOC (sequencing only)` correctly classified per V1 CH4 sequencing-only cost class). |
| 1E-locks-evidence.md (V2-LOCKED) | ACCEPT | Yes — 16 LACs table header at `1E-locks-evidence.md:107` carries all six fields; 16 LAC rows at `:109-124` populate every column; Lock spec-claim table at `:72-89` carries `LOC / risk | Hard cap | Same-wave consumer | Wave alignment hint` for every one of the 16 locks; SK-V14 NEW divergence-row table at `:95-101` carries the six-field schema for D-1E-12..16. | V2-LOCKED — zero V3 drift. LAC-1E-12 V2 promotion to "candidate-promoted-to-T-P3-§3C-priority" at `:120` + §1.5 promotion explainer at `:126-128` preserve verbatim. Cost frame `60-180 LOC docs / low risk / 240 LOC hard cap` unchanged from V1 baseline. LAC-1E-15 4,000-8,000 LOC / 11,000 LOC cap (1.375×) intact. The optional §3.6 LAC-1E-12 procedural-addendum proposal remains pending T-P3 §3C disposition per dispatch context §1. |
| 1F-coherence-scan.md (V2-LOCKED) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-coherence-scan.md:97-110` carries all six fields for 12 COH rows. | V2-LOCKED — zero V3 drift. COH-012 V2 anti-fabrication phrasing `restart/locks/LOCKS.md (no CH7 mention; grep -n "CH7\|Overfit" returns zero hits)` preserves; `grep -n "CH7\|Overfit" restart/locks/LOCKS.md` returns 0 at V3 HEAD (2026-05-23). COH-011 nine-grammar census `0 LOC census; 600-1200 LOC PRUNE-4` with `1400 LOC` hard cap (1.17× — within convention bounds; flagged as the only sub-1.2× row in the cohort and noted in §6 finding row 5). `locks_amendment_candidates: 0`. |
| 1F-anti-pattern.md (V3) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-anti-pattern.md:84-105` carries all six fields for 20 AP rows (AP-012..AP-020 covers the SK-V14 + V3 fold additions). | V3 amendment is cite-rebind only at 3 cells: AP-020 evidence_basis rebind at `:105` (V2 `:222,234,299,504` → V3 `:648, 2691, 1082, 1203, 1354, 1511, 1661, 1815, 1964`); AP-009 evidence_basis rebind at `:94` (V2 cite cluster → V3 `:636, 648, 2691, 1082, 1203, 1354, 1511, 1661, 1815, 1964` plus self-correction note for V2 dispatch's false "zero hits" claim — `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns **27** hits at HEAD, not zero, validating the REBIND vs removal disposition); AP-011 evidence_basis rebind at `:96` (V2 `:5,24,43` → V3 `:7,26,45`). LOC budgets, risks, hard caps, same-wave consumers preserve verbatim. AP-020 160 LOC / 120 LOC = **1.33×** within convention. |
| 1F-past-corpora.md (V2-LOCKED) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-past-corpora.md:124-142` carries all six fields for 17 PC + SKV13-PB rows. | V2-LOCKED — zero V3 drift. PC-008 SK-V5 verify-before-rederive obligation at `:74` preserves `80-200 LOC verify; medium risk` cost class; U-PC-002 verify_action at `:158` (`rg -n 'JSON_STRUCTURAL\|scan_json\|JsonParseIndex' skinny/crates/bbnf-simd skinny/crates/runtime`) intact. PC-017 google_sheets=10 census preserves at `:120, :140`. PC-001/002/004 `0 LOC` / `medium if reopened` cost class held per V1 CH4 "0 LOC pre-block; cost is the cost of respecting a rejected route" distinction. |

## §4 Verification — 1E's 16 LACs Carry Wave-Alignment Hint + Path:line at V3 HEAD

The V3 dispatch-context preserves the V1 + V2 CH4 16/16 LAC wave-alignment +
path:line discipline. V3 has no edits to 1E (V2-LOCKED); the V2 verification
table carries forward verbatim. Spot-check at V3 HEAD against
`restart/audit/totality/p1/1E-locks-evidence.md:109-124`:

| LAC | wave column populated at V3 HEAD | evidence path:line populated at V3 HEAD | Verdict |
|---|---|---|---|
| LAC-1E-01 | `A/F substrate + C cost model` (`:109`) | REDRESS + runtime tape + CH5 V3 cites | ACCEPT (verbatim from V2) |
| LAC-1E-02 | `C.W1` (`:110`) | LOCKS + passes cites | ACCEPT (verbatim from V2) |
| LAC-1E-03 | `G.W1/G.W2` (`:111`) | LOCKS cite | ACCEPT (verbatim from V2) |
| LAC-1E-04 | `H + SK-V14 R6/R7/R8` (`:112`) | SYNTHESIS + RESULTS cites | ACCEPT (verbatim from V2) |
| LAC-1E-05 | `B/G runtime API` (`:113`) | LOCKS + bbnf lib cites | ACCEPT (verbatim from V2) |
| LAC-1E-06 | `A.W0/A.W1` (`:114`) | LOCKS + Cargo.toml + MIGRATION cites | ACCEPT (verbatim from V2) |
| LAC-1E-07 | `A tree-shape + bench hardening` (`:115`) | LOCKS + REDRESS cites | ACCEPT (verbatim from V2) |
| LAC-1E-08 | `T-P3 3C lock amendment + SK-V14 C-1` (`:116`) | LOCKS + lock14-scan cites | ACCEPT (verbatim from V2) |
| LAC-1E-09 | `A/J profile gate` (`:117`) | LOCKS + Cargo.toml cites | ACCEPT (verbatim from V2) |
| LAC-1E-10 | `H.W0 primitive admission` (`:118`) | LOCKS + intrinsic site cites | ACCEPT (verbatim from V2) |
| LAC-1E-11 | `T-P3 3C lock amendment` (`:119`) | LOCKS + RESULTS + REDRESS cites | ACCEPT (verbatim from V2) |
| LAC-1E-12 (V2 promoted) | `T-P3 3C lock amendment` (`:120`) | PASS-0-OVERFIT-AUDIT + CH7 V3 cites; §1.5 promotion explainer at `:126-128` | ACCEPT (V2-promotion preserved; LOC frame `60-180 docs / low / 240 cap` unchanged) |
| LAC-1E-13 (V4-NEW) | `SK-V14 C-3 R4 + T-P3 3C` (`:121`) | SYNTHESIS + audit-overfit + LOCKS cites | ACCEPT (verbatim from V2) |
| LAC-1E-14 (V4-NEW) | `T-P3 substrate taxonomy + SK-V14 R6 CSS L4 re-admit` (`:122`) | CH2 V3 + 1C-D5 + LOCKS + RESULTS cites | ACCEPT (verbatim from V2) |
| LAC-1E-15 (V4-NEW) | `SK-V14 C-1 PRUNE-4 (9 sub-waves) + T-P3 3C lock amendment` (`:123`) | audit-overfit + builder/arena template + LOCKS cites | ACCEPT (verbatim from V2; LOC frame `4000-8000 / very-high / 11000 cap` preserved; 1.375× within convention) |
| LAC-1E-16 (V4-NEW) | `SK-V14 C-2 bench harness emission + T-P3 3C lock amendment` (`:124`) | SYNTHESIS + CH7 V3 §2.5 cites | ACCEPT (verbatim from V2) |

All 16 LACs pass at V3 HEAD. The V3 confirming cycle introduces no schema
regression and no cost-class change at 1E.

## §5 V3 Dispatch-Focus Verification — Three Convergence Points

The V3 dispatch context at
`restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md:29` narrows the
V3 confirming-cycle CH4 disposition to three convergence points. Each is
verified at V3 HEAD (commit `0a9f1288c`).

| Convergence point | V3 carrier | Verification at V3 HEAD | Disposition |
|---|---|---|---|
| V3 fold LOC counts within 1.2-1.4× hard-cap convention | full divergence + amendment surface across 8 inventories | Cohort spot-check at V3 HEAD: AP-020 160/120 = **1.33×**; 1B-D8 600/500 = **1.20×**; 1B-D10 650/500 = **1.30×**; 1A-DIV-008 1,100/900 = **1.22×**; LAC-1E-12 240/180 = **1.33×**; LAC-1E-15 11,000/8,000 = **1.375×**; 1A-DIV-001 1,500/1,200 = **1.25×**; 1A-DIV-005 700/600 = **1.17×** (taxonomy row; below 1.2× — non-blocking per §6 finding 5); COH-011 1,400/1,200 = **1.17×** (PRUNE-4 census-bundled row; non-blocking per §6 finding 5). Substantive multi-LOC kernels (D8/D10 split, AP-020 fence, LAC-1E-12 promotion, LAC-1E-15 census) all sit cleanly within 1.2-1.4×. | ACCEPT — convention is uniformly respected at the kernel/amendment-candidate surface; the two 1.17× rows (1A-DIV-005 taxonomy, COH-011 PRUNE-4-bundled) are explicit small-doc-class rows not subject to the kernel multiplier discipline. |
| 1C LOC repair rescale (~50→~190 LOC + 2.5× consumer-rewire) holds at HEAD with 127-symbol surface | `1C-runtime-evidence.md:162` (1C-D4 row); `1C-runtime-evidence.md:200-201` (verification block) | V3 reads `80 (root rewrite) + ~2.5× consumer-rewire band proportional to 127-symbol surface`. Two-component composition preserved verbatim from V2 (V2 said "126-symbol"; V3 lifts to 127 absorbing the css_l4 41→43 + google_sheets 12→11 corrections per dispatch context §1). HEAD verification: `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l` → **30** (reproduces the 30-site V1 census exactly); `rg -l ... | wc -l` → **15**; `awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs | wc -l` → **47**; per-grammar sum `10+10+43+10+10+10+11+13+10 = 127` arithmetically reconciles. The 2.5× multiplier remains proportional; the symbol-surface +1 increment (126→127) represents <1% change to the consumer-rewire band cost — well within LOC budget noise. | ACCEPT — LOC rescale correctly absorbs the 127-symbol surface; root + consumer-rewire two-component composition preserved; 2.5× multiplier intact. |
| AP-020 LOC band (40-120 / 160 cap) intact | `1F-anti-pattern.md:80` (AP-020 detail row); `1F-anti-pattern.md:105` (V2 Planning Metadata, V3 cite-rebound evidence_basis) | AP-020 reads `40-120 LOC fence/classification` with `160 LOC` hard cap (1.33× — within convention); same-wave consumer `CSS comparator-sidecar fence consumer`; co-wave with AP-009 (CSS evidence-accounting wave). V3 evidence_basis cite-rebind at `:80` and `:105` replaces V2's fabricated cites `:222,234,299,504` with real cites `:648, 2691, 1082, 1203, 1354, 1511, 1661, 1815, 1964` (per HARDENING-T-P1-V2-CONSOLIDATED §3.1 F-V3-CH7-1). LOC band + hard cap + same-wave consumer all preserve from V2 — the rebind is evidentiary correction, not cost-class change. Companion AP-009 at `:94` (60-160 LOC / 220 LOC cap, 1.375× — within convention) partitions cleanly with AP-020; no LOC double-counting. CH5-004 binding fold preserved. | ACCEPT — LOC band is correctly sized for fencing/classification scope; 1.33× hard cap is conventionally bounded; cite-rebind is evidentiary not cost-class. |

## §6 Findings

| Disposition | Finding | Evidence |
|---|---|---|
| ACCEPT | The V1 + V2 CH4 six-field schema discipline survives V3 amendment without regression. Every V3-active divergence row (1A 8 rows, 1B 13 rows incl. D8/D10 split, 1C 11 rows, 1D 17 rows, 1E 16 LACs + 16 locks + 5 NEW divergence rows, 1F-anti-pattern 20 AP rows, 1F-coherence 12 COH rows, 1F-past-corpora 17 PC + SKV13-PB rows) carries `loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis`. The V3 cite-rebind micro-fold is uniformly schema-preserving. | All V3-active inventories' divergence and amendment tables (cited in §3 table). |
| ACCEPT | The V3 1C undercount rescale correctly composes the two-component cost figure (root rewrite + consumer-rewire band) anchored to the live **127**-symbol / 47-line / 30-site / 15-file census. The V2 126-symbol baseline is lifted to 127 to absorb (a) css_l4 41→43 split (3 css_l4-named aliases CssRule/CssDeclaration/CssSelector at `mod.rs:34-35` counted inside the 43) and (b) google_sheets 12→11 correction caught beyond V2 scope. The 2.5× multiplier is preserved verbatim; the symbol-surface change is sub-1% and well within LOC budget noise. CH4 explicitly accepts the 127-symbol surface as the V3-current consumer-rewire denominator. | `1C-runtime-evidence.md:162` (1C-D4); `:200-201` (verification block); HEAD command outputs 30/15/47/127 (2026-05-23). |
| ACCEPT | The V3 cite-rebind discipline (AP-020 + AP-009 + AP-011 in 1F-anti-pattern; 1D Track 2 + proof-witness; 1A SUB-014) preserves every LOC budget, hard cap, and same-wave consumer obligation verbatim. The rebinds are evidentiary corrections (V2 fabricated cites replaced with HEAD-verified cites) and do not move any cost-class needle. CH7 V2 dispatch self-correction (false "lightningcss_facts zero hits" claim) is the most substantive evidentiary correction; at HEAD `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns **27** hits, validating REBIND-not-removal disposition for AP-009. | `1F-anti-pattern.md:94, 96, 105`; `1D-skinny-lessons.md:117, 157`; `1A-substrate-evidence.md` SUB-014 cite-rebind; HEAD `grep -c` outputs. |
| ACCEPT | The V3 AP-020 fence/classification row preserves the V2 sizing: 40-120 LOC with a 160 LOC hard cap (1.33× upper bound) and routes the consumer through the same CSS evidence-accounting wave as AP-009, with no LOC double-counting between the two rows. CH5-004 V1 binding fold + V2 dispatch carry-forward + V3 cite-rebind disposition all preserve the cost class without change. | `1F-anti-pattern.md:80, 105` (AP-020); `:94` (AP-009 companion); HEAD `grep -n 'fixture_sidecar_facts\|same-plane-source-sidecar' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` validates the 9 rebound cites. |
| ACCEPT | The V1 + V2 CH4 hard-cap multiplier convention (1.2-1.4× upper bound for kernel/amendment-candidate rows) survives V3 amendment. Substantive-kernel spot-check at V3 HEAD: AP-020 1.33×; 1B-D8 1.20×; 1B-D10 1.30×; 1A-DIV-008 1.22×; LAC-1E-12 1.33×; LAC-1E-15 1.375×. All within convention. Two sub-1.2× rows exist (1A-DIV-005 700/600 = 1.17× taxonomy/doc class; COH-011 1,400/1,200 = 1.17× PRUNE-4-bundled) and are correctly framed as non-kernel rows where the multiplier convention does not bind — taxonomy/doc-class and bundled-into-larger-wave caps are explicitly outside the V1 CH4 convention scope. CH4 reaffirms the V1 + V2 non-blocking recommendation that T-P3 §3C codify the multiplier convention in Lock 8 V+1 wording. | Per-row hard cap citations across §3 + §5 tables; explicit sub-1.2× rows enumerated. |
| ACCEPT | The V3 cite-rebind discipline (per V3 dispatch context §1 — every cite re-verified at V3 HEAD before ACCEPT) institutionalizes the LAC-1E-12 procedural-addendum proposal in spirit if not yet in lock-text: every CH4 ACCEPT row in §3 carries an executable-verification spot at V3 HEAD (grep / awk / find outputs reproduced inline). The procedural-addendum formalization remains pending T-P3 §3C disposition; the V3 confirming-cycle implements it operationally at zero new LOC cost. | V3 dispatch context §3; §3 + §5 inline HEAD command outputs. |
| ACCEPT | The V3 google_sheets=10 propagation reaches all relevant rows unchanged from V2: AP-016 at `1F-anti-pattern.md:101` reads `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 = 67` (1500-3000 LOC PRUNE-4 / 4000 LOC cap, 1.33× within convention); PC-017 at `1F-past-corpora.md:120` carries the identical census; COH-011 at `1F-coherence-scan.md:92` matches. HEAD verification: `find crates/core/src/runtime/google_sheets -type f -name '*.rs' | wc -l` returns **10** at 2026-05-23. | `1F-anti-pattern.md:101`; `1F-past-corpora.md:120`; `1F-coherence-scan.md:92`; HEAD command output 10. |
| ACCEPT | The V1 + V2 CH4 §6 cross-artefact Pattern H LOC-budget congruence note (LAC-1E-15 4000-8000 vs. AP-016 1500-3000 vs. PC-017 1500-3000 vs. 1C-D1 10,915 vs. 1D Pattern H envelope 2800-3400) survives V3 amendment with the V1+V2 framing recommendation preserved: T-P3 §3C should adopt LAC-1E-15 per-tranche framing as the load-bearing Pattern H budget. AP-016 / PC-017 google_sheets=10 propagation does not collapse the multi-framing spread; per-tranche census vs. per-wave PRUNE-4 vs. C-1 envelope vs. full revival cost remain four distinct artefact-purpose-correct framings. | `1E-locks-evidence.md:123` (LAC-1E-15); `1F-anti-pattern.md:101` (AP-016); `1F-past-corpora.md:120` (PC-017); `1C-runtime-evidence.md:159` (1C-D1); `1D-skinny-lessons.md:144` (Pattern H envelope). |

## §7 New Finding (Lens-Local, V3 confirming cycle)

| Note | Detail |
|---|---|
| CH4 V3 cite-rebind cost-neutrality discipline | Every V3 cite-rebind in this confirming cycle (AP-020 evidence_basis 4 cites → 9 cites; AP-009 evidence_basis V2 cluster → V3 cluster + self-correction note; AP-011 evidence_basis `:5,24,43` → `:7,26,45` off-by-2; 1D Track 2 + proof-witness mirror; 1A SUB-014) is **cost-neutral by construction** — the rebind addresses CH1 CORRECTNESS + CH6 ANTI-PAPER-CLOSE failure modes (V→V+1 cite-carry without re-verification per V3 dispatch context §2 CH6) without changing any LOC budget, risk class, wave assignment, hard cap, or same-wave consumer. CH4 explicitly registers this as the correct disposition: evidentiary correction is not cost-class change. No CH4 revise triggered. The 1C 126→127 symbol-surface lift is the only V3 micro-fold that touches a CH4 numeric (the 2.5× consumer-rewire denominator) and that lift is <1% and well within LOC budget noise. |
| CH4 V3 self-test against CH7 V2 fabrication discipline | CH7 V2 itself carried a fabrication (the false "lightningcss_facts zero hits" dispatch claim). V3 must NOT propagate that. CH4 V3 verifies at HEAD: `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns **27** hits (definition at `:636` + `fixture_sidecar_facts` callsite at `:648` + 7 per-grammar siblings + call sites). The V3 1F-anti-pattern AP-009 rebind at `:94` includes explicit self-correction note acknowledging the V2 dispatch error. CH4 V3 propagates ZERO fabricated cite at any cost-relevant row — anti-paper-close discipline operates at maximum strength. |

## §8 Required Revisions

None. All eight T-P1 inventories pass CH4 ACCEPT at V3 HEAD (commit
`0a9f1288c`) under the orchestrator six-field schema, the V3
dispatch-context three-convergence-point focus, the V1 + V2 CH4 16/16 LAC
wave-alignment + path:line discipline, and the V1 + V2 CH4 1.2-1.4×
hard-cap convention. The V3 confirming-cycle micro-fold introduces zero
CH4-side regression and zero new LOC obligation beyond the explicit
evidentiary cite-rebinds (which are cost-neutral by construction).

## §9 Cycle Disposition

ACCEPT. 8/8 T-P1 inventories pass at V3 HEAD. 16/16 LACs pass
dispatch-required wave-alignment + path:line check at V3 HEAD (commit
`0a9f1288c`). All three V3 dispatch-context convergence points verify:
(i) V3 fold LOC counts within 1.2-1.4× convention; (ii) 1C LOC repair
rescale holds at the 127-symbol surface (two-component composition + 2.5×
multiplier intact); (iii) AP-020 LOC band 40-120 / 160 cap (1.33×) intact.
The V3 cite-rebind discipline is cost-neutral by construction —
evidentiary correction without cost-class change. §3Z gate: V3 is the
first cohort-wide ≥95% cycle for T-P1; CH4 contributes ACCEPT 8/8 = 100%
to that cycle; V4 confirming cycle required for cohort LOCK.

## §10 Aggregator Note

CH4 V3 disposition: ACCEPT. 8/8 T-P1 inventories. 16/16 LACs. 3/3 V3
dispatch-context convergence points. Carry-forward V1 + V2 non-blocking
governance recommendations to T-P3 §3C: (i) adopt LAC-1E-15 per-tranche
framing as the load-bearing Pattern H budget; (ii) codify the 1.2-1.4×
hard-cap multiplier convention in Lock 8 V+1 wording (with explicit
sub-1.2× exemption for taxonomy/doc-class + bundled-into-larger-wave
rows); (iii) consider promoting the V3 procedural addendum (executable
verification of every cite at V+1 HEAD before ACCEPT) to a formal
LAC-1E-12 sub-clause. LAC-1E-12 V2-promotion to
"candidate-promoted-to-T-P3-§3C-priority" preserved at V3 HEAD without
cost-class change. The V3 cite-rebind cohort across 1A + 1C + 1D + 1F is
uniformly cost-neutral — evidentiary correction is not cost-class change,
and CH4 explicitly registers this as the correct disposition for any
future cite-correction cycle.
