---
agent: CH4
pass: T-P1-excavation
cycle: V4
lens: COST
generated_at: 2026-05-23T19:30:00-04:00
disposition: ACCEPT
audited_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md (V4 amended; 113 lines)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V3-LOCKED; 116 lines)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V4 amended; 206 lines)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V4 amended; 182 lines)
  - restart/audit/totality/p1/1E-locks-evidence.md (V4 amended; 168 lines)
  - restart/audit/totality/p1/1F-anti-pattern.md (V4 amended; 123 lines)
  - restart/audit/totality/p1/1F-coherence-scan.md (V3-LOCKED; 127 lines)
  - restart/audit/totality/p1/1F-past-corpora.md (V3-LOCKED; 159 lines)
authority:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3 (CH4)
  - restart/prompts/ORCHESTRATOR.md §3W + §3Z (cohort LOCK = ≥95% × 2 consecutive cycles; V≤5 ceiling)
  - restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/hardening/V3/CH4.md (V3 baseline — 8/8 ACCEPT)
  - restart/audit/totality/p1/hardening/V2/CH4.md (V2 baseline — 8/8 ACCEPT)
  - restart/audit/totality/p1/hardening/V1/CH4.md (V1 baseline — 8/8 ACCEPT)
v4_head_commit: 8f4756113
accept_count: 8
revise_count: 0
reject_count: 0
acceptance_rate: 8/8 (100%)
cycle_role: confirming-cycle for §3Z cohort LOCK (V3 100% + V4 100% = ≥95% × 2 consecutive cycles → LOCK trajectory; V4 = first LOCK-eligible cycle; V5 may extend ceiling but is not required)
lock_extension: 4-cycle LOCK (V1+V2+V3+V4)
---

## §1 Lens Basis

`restart/prompts/totality/PASS-1-EXCAVATION.md:121-123` (CH4) requires
(a) every divergence carry a realistic LOC-delta + risk class, and (b) 1E
amendment candidates state a wave-alignment hint; amendment candidates
without supporting path:line evidence are REVISE.
`restart/prompts/ORCHESTRATOR.md:86` sharpens this to the six-field schema
per kernel/primitive: `loc_budget | risk | wave | hard_cap |
same_wave_consumer | evidence_basis`. V4 dispatch context at
`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:29` narrows
the V4 LOCK-eligible-cycle CH4 focus to two convergence points: (i) verify
all V4 folds are cost-neutral per the V3 CH4 cite-rebind cost-neutrality
discipline (§7 of V3 CH4), and (ii) verify the per-lens LOC repair
rescales preserved at HEAD (1C 127-symbol surface; AP-020 160/120 = 1.33×
band; LAC-1E-15 11,000/8,000 = 1.375×).

The V1 + V2 + V3 CH4 cycles each disposed ACCEPT 8/8 with 16/16 LAC
wave-alignment hits. V4 is the first LOCK-eligible cycle for T-P1
(per §3Z: ≥95% × 2 consecutive cycles → cohort LOCK; V3 = first ≥95%
cycle, V4 = second). CH4 is the steady-state half of that cohort and
must confirm the V3 100% without retreat for cohort LOCK trajectory.

## §2 Cycle Verdict

ACCEPT. All eight T-P1 inventories carry full CH4 cost framing at V4 HEAD
(commit `8f4756113`) under the orchestrator six-field schema. Both V4
dispatch-context convergence points verify at HEAD:

- **All five V4 micro-folds are cost-neutral by construction.** The V4
  amendment surface comprises five micro-edits across 5 inventories
  (3 V3-LOCKED files carry forward verbatim with zero edits). The
  diff `git diff 0a9f1288c 8f4756113 -- restart/audit/totality/p1/1{A,C,D,E}-*.md restart/audit/totality/p1/1F-anti-pattern.md`
  resolves to: (i) **1A**: `1D :100` → `1D :117` cross-reference cosmetic
  refresh, 2 substitutions inside 1A-DIV-008's substrate-union nuance
  paragraph + the V2-fold-note narrative cite — zero LOC budget / risk /
  wave / hard cap / same-wave consumer change. (ii) **1C**: `126` →
  `127` single-token symbol-surface lift in the Executive Summary at
  `1C-runtime-evidence.md:40` — the 2.5× consumer-rewire-band multiplier
  remains verbatim at `:162` (rescale was applied at V3; V4 is the second
  consecutive cycle to surface that figure). (iii) **1D**: W13.9 label
  split from "W13.5-W13.9 MEASURED-REJECT" (V3) to "W13.5-W13.8
  MEASURED-REJECT at `REDRESS.md:4621/4645/4674/4704`; W13.9
  CORRECTNESS-REJECT at `:4734`" (V4) at `:140` — REJECT-class label
  refinement only, no LOC obligation change. Companion cite refresh
  `:5,24,43` → `:7,26,34,45` (Track 2) + `:222,234,299,504` → `:648,
  1082, 1203, 1354, 1511, 1661, 1815, 1964, 2691` (CSS sidecar) inside
  `1D:117` substrate-union row — cite-rebind / evidentiary correction
  per V3 §7 discipline. (iv) **1E**: sustained-UNKNOWN paragraph inserted
  at `1E-locks-evidence.md:35` (1 paragraph, +1 net narrative section)
  enumerating 4 UNKNOWNs (L03, L16, audit-overlay column gap, Lock 1
  fact-stream taxonomy) each with executable `verify_action` at
  `:161-164` — adds zero new LOC budget / risk / hard-cap / same-wave
  consumer rows to either the divergence table (`:97-110`) or the LAC
  table (`:109-124`); the paragraph is anti-paper-close narrative anchor,
  not a new CH4-class divergence. (v) **1F-anti-pattern**: AP-009
  `grep -c 'lightningcss_facts'` cosmetic update 24 → 27 at `:69`
  (matching the HEAD-verified count) — zero LOC obligation change.
- **Per-lens LOC repair rescales preserved at HEAD.** 1C `1C-D4` row at
  `:162` reads `80 (root rewrite) + ~2.5× consumer-rewire band
  proportional to 127-symbol surface` (preserved verbatim from V3 with
  the 127 surface confirmed by HEAD: per-grammar sum `10+10+43+10+10+10+
  11+13+10 = 127`). AP-020 at `1F-anti-pattern.md:105` retains
  `40-120 LOC / 160 LOC cap` (1.33× — within V1 CH4 1.2-1.4×
  convention). LAC-1E-15 at `1E-locks-evidence.md:123` retains
  `4,000-8,000 LOC / 11,000 LOC cap` (1.375× — within convention).
  1B (V3-LOCKED) D8 600/500 = 1.20× and D10 650/500 = 1.30× preserve.

The V1 + V2 + V3 CH4 100% ACCEPT carries forward without retreat. §3Z
gate: V4 is the second consecutive cohort-wide ≥95% cycle for T-P1; CH4
contributes ACCEPT 8/8 = 100% to that cycle, establishing the LOCK
trajectory. **4-cycle LOCK extension (V1+V2+V3+V4) recommended.**

## §3 Per-Artefact Verdict Table

| Artefact | V4 disposition | CH4 six-field schema present at V4 HEAD | Notes |
|---|---|---|---|
| 1A-substrate-evidence.md (V4 amended) | ACCEPT | Yes — divergence-table header at `1A-substrate-evidence.md:75` carries `loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis`; 8 divergence rows at `:77-84` populate every column; 1A-LOCK1-AMEND-001 candidate row at `:113` carries the same six-field schema. | V4 micro-fold per F-V4-CH5-1: row-100→117 cross-cite refresh at 1A-DIV-008 substrate-union nuance disambiguation paragraph (line `:84`) and the `t_p1_v1_hardening_fold_note` narrative (line `:9`). Two substitutions of `1D :100` → `1D :117`. Pre/post grep confirms zero residual `row 100` or `:100` orphans in 1A (`grep -c ':100' 1A-substrate-evidence.md` returns 0; `grep -c '1D \`:117\`' 1A-substrate-evidence.md` returns 2). The refresh is cite-housekeeping; **zero LOC budget, risk class, wave, hard cap, same-wave consumer, or evidence_basis cell touched**. 1A-DIV-008 retains `400-900 LOC for cursor unification; 100-300 LOC if Pass Omega ratifies two-cursor shape`, `high (substrate-union semantics)`, `T-P2 substrate-union research; T-P3 §3C disposition; Pass Omega ratification`, `1,100 LOC cap` (1.22× of the 900 LOC upper budget — within 1.2-1.4× convention), and same-wave consumer obligation verbatim from V3. |
| 1B-codegen-evidence.md (V3-LOCKED) | ACCEPT | Yes — divergence-table header at `1B-codegen-evidence.md:77` carries all six fields; 13 divergence rows at `:79-90` populate them; D8/D10 split rows at `:86-87` each carry distinct LOC budgets, hard caps, and same-wave consumer obligations; 3 amendment candidates at `:114-116` populate the same six-field schema. | V3-LOCKED — zero V4 drift. `git diff 0a9f1288c 8f4756113 -- restart/audit/totality/p1/1B-codegen-evidence.md` returns empty. D8/D10 split LOC budgets (250-500 each) + distinct 600 / 650 hard caps (1.20× / 1.30×) + distinct same-wave consumers preserve. NECESSARY-BUT-INSUFFICIENT-relative-to-PRUNE-4 framing intact. |
| 1C-runtime-evidence.md (V4 amended) | ACCEPT | Yes — divergence-table header at `1C-runtime-evidence.md:157` carries all six fields; 11 divergence rows at `:159-169` populate them. | V4 micro-fold per F-V4-CH2-1: Executive Summary single-token update `126` → `127` at `:40` (`127 grammar-named type reexports`). The 2.5× consumer-rewire-band multiplier at the 1C-D4 row (`:162`) preserves verbatim from V3 (`80 (root rewrite) + ~2.5× consumer-rewire band proportional to 127-symbol surface`); the V4 edit aligns the Executive Summary cell with the V3-already-rescaled 1C-D4 row. HEAD verification: per-grammar sum `bbnf 10 + bnf 10 + css_l4 43 + css_pretty 10 + csv 10 + ebnf 10 + google_sheets 11 + json 13 + math 10 = 127` reconciles arithmetically; mechanical extraction at `:201` produces 127 via `awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs`. **Zero LOC budget cell touched** — the multiplier and 2-component composition remain verbatim from V3. NEW-CH2-V3-02 orphan-cell propagation guard (every "N grammar-named X" subtract-from-K cite enumerates the K neutrals with `path:line` inside the cited window) is satisfied; the V4 micro-fold updates only the orphan executive-summary cell that V3 left at 126. |
| 1D-skinny-lessons.md (V4 amended) | ACCEPT | Yes — divergence-table header at `1D-skinny-lessons.md:138` carries all six fields; 17 divergence rows at `:140-158` populate them. | V4 micro-fold per F-V4-CH3-1 + F-V4-CH5-1: (a) W13.9 CORRECTNESS-REJECT label split at `:140` (V3 read "W13.5-W13.9 MEASURED-REJECT at `REDRESS.md:4621/4645/4674/4704/4734`"; V4 reads "W13.5-W13.8 MEASURED-REJECT at `REDRESS.md:4621/4645/4674/4704`; W13.9 CORRECTNESS-REJECT at `:4734`") — REJECT-class label refinement preserves every REDRESS line cite; no REDRESS row re-opened; W13.5-W13.8 + W13.9 both remain bound (one MEASURED-REJECT cluster + one CORRECTNESS-REJECT) and both remain NOT-PASS-ADMIT. **Zero LOC budget cell touched** — the row retains `250-500 LOC (revert-heavy)`, `MED-LOW` risk, `C-5 PRUNE-1+PRUNE-2` wave, `700 LOC` hard cap (1.4× — at upper bound of 1.2-1.4× convention), and same-wave consumer obligation verbatim from V3. (b) Sub-case cite refresh at `1D :117` substrate-union row: Track 2 `:5,24,43` → `:7,26,34,45` (V3 fold off-by-2 captured at HEAD per `bbnf-bench/src/track2/json.rs`); CSS sidecar `:222,234,299,504` → `:648, 1082, 1203, 1354, 1511, 1661, 1815, 1964, 2691` (V3 fold cite-rebind per HARDENING-T-P1-V2-CONSOLIDATED §3.1 F-V3-CH7-2). Both rebinds are evidentiary corrections; CH4 V3 §7 cost-neutrality discipline applies. REDRESS verdict labels verified via `grep -n 'W13.5\|W13.6\|W13.7\|W13.8\|W13.9' restart/audit/totality/p1/1D-skinny-lessons.md` reproducing the split structure. |
| 1E-locks-evidence.md (V4 amended) | ACCEPT | Yes — 16 LACs table header at `1E-locks-evidence.md:109` carries all six fields; 16 LAC rows at `:109-124` populate every column; Lock spec-claim table at `:72-89` carries `LOC / risk | Hard cap | Same-wave consumer | Wave alignment hint` for every one of the 16 locks; SK-V14 NEW divergence-row table at `:95-101` carries the six-field schema for D-1E-12..16. | V4 micro-fold per F-V4-CH6-1: sustained-UNKNOWN paragraph inserted at `:35` (166→168 lines net, +1 paragraph + Open Questions table refresh at `:161-164`). The paragraph enumerates 4 UNKNOWNs (L03 cursor elision, L16 full allowlist coverage, NEW SK-V14 audit-overlay column gap, NEW SK-V14 Lock 1 fact-stream taxonomy) each with executable `verify_action` cited inline. **Zero new CH4-class rows added to either the divergence table (`:97-101`) or the LAC table (`:109-124`)** — the paragraph is anti-paper-close narrative anchor only; the 4 UNKNOWN entries live in the Open Questions table (`:159-164`), which is a verify-action register, not a CH4 LOC-budget surface. LAC-1E-12 V2-promotion preserved at `:120` (60-180 LOC docs / low risk / 240 LOC hard cap, 1.33×). LAC-1E-15 4,000-8,000 LOC / 11,000 LOC cap (1.375×) intact. Cross-fold to §1.5 LAC-1E-12 promotion explainer at `:126-128` preserved. |
| 1F-coherence-scan.md (V3-LOCKED) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-coherence-scan.md:97-110` carries all six fields for 12 COH rows. | V3-LOCKED — zero V4 drift. `git diff 0a9f1288c 8f4756113 -- restart/audit/totality/p1/1F-coherence-scan.md` returns empty. COH-012 V2 anti-fabrication phrasing preserves; COH-011 nine-grammar census `0 LOC census; 600-1200 LOC PRUNE-4` with `1400 LOC` hard cap (1.17× — within V1 CH4 sub-1.2× small-doc-class exemption per V3 §6 finding row 5). `locks_amendment_candidates: 0`. |
| 1F-anti-pattern.md (V4 amended) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-anti-pattern.md:84-105` carries all six fields for 20 AP rows (AP-012..AP-020 covers the SK-V14 + V3 fold additions). | V4 micro-fold per F-V4-CH1-1: AP-009 detail row at `:69` cosmetic update `24 hits` → `27 hits` (matching the HEAD-verified count). HEAD verification: `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns **27** at V4 HEAD (2026-05-23) — the V4 edit aligns the AP-009 detail-row narrative with the V3 already-verified count discipline. **Zero CH4-class cell touched** — the AP-009 row at `:94` retains `60-160 LOC classification / medium-high risk / CSS evidence-accounting wave / 220 LOC hard cap` (1.375× — within convention) verbatim; AP-020 row at `:105` retains `40-120 LOC fence/classification / medium-high risk / 160 LOC hard cap` (1.33×) verbatim. AP-011 evidence_basis at `:96` preserves V3 cite-rebind `:7,26,45` (V4 1D refresh extends to `:7,26,34,45` adding the `JsonRoot::from_tape(self.input, self.tape.finish())` seal at `:34` — 1D propagation only; 1F-anti-pattern AP-011 cell unchanged in this micro-fold). |
| 1F-past-corpora.md (V3-LOCKED) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-past-corpora.md:124-142` carries all six fields for 17 PC + SKV13-PB rows. | V3-LOCKED — zero V4 drift. `git diff 0a9f1288c 8f4756113 -- restart/audit/totality/p1/1F-past-corpora.md` returns empty. PC-008 SK-V5 verify-before-rederive obligation at `:74` preserves `80-200 LOC verify; medium risk` cost class; U-PC-002 verify_action at `:158` intact. PC-017 google_sheets=10 census preserves at `:120, :140`. PC-001/002/004 `0 LOC` / `medium if reopened` cost class held per V1 CH4 "0 LOC pre-block; cost is the cost of respecting a rejected route" distinction. |

## §4 Verification — 1E's 16 LACs Carry Wave-Alignment Hint + Path:line at V4 HEAD

The V4 dispatch-context preserves the V1 + V2 + V3 CH4 16/16 LAC
wave-alignment + path:line discipline. V4 amends only the Executive
Summary at `:35` (adding the sustained-UNKNOWN paragraph) and the
Open Questions table at `:161-164` (refreshing the 4 UNKNOWN entries
with executable verify_actions); the 16-LAC table at `:109-124`
preserves verbatim from V3. Spot-check at V4 HEAD against
`restart/audit/totality/p1/1E-locks-evidence.md:109-124`:

| LAC | wave column populated at V4 HEAD | evidence path:line populated at V4 HEAD | Verdict |
|---|---|---|---|
| LAC-1E-01 | `A/F substrate + C cost model` (`:109`) | REDRESS + runtime tape + CH5 V3 cites | ACCEPT (verbatim from V3) |
| LAC-1E-02 | `C.W1` (`:110`) | LOCKS + passes cites | ACCEPT (verbatim from V3) |
| LAC-1E-03 | `G.W1/G.W2` (`:111`) | LOCKS cite | ACCEPT (verbatim from V3) |
| LAC-1E-04 | `H + SK-V14 R6/R7/R8` (`:112`) | SYNTHESIS + RESULTS cites | ACCEPT (verbatim from V3) |
| LAC-1E-05 | `B/G runtime API` (`:113`) | LOCKS + bbnf lib cites | ACCEPT (verbatim from V3) |
| LAC-1E-06 | `A.W0/A.W1` (`:114`) | LOCKS + Cargo.toml + MIGRATION cites | ACCEPT (verbatim from V3) |
| LAC-1E-07 | `A tree-shape + bench hardening` (`:115`) | LOCKS + REDRESS cites | ACCEPT (verbatim from V3) |
| LAC-1E-08 | `T-P3 3C lock amendment + SK-V14 C-1` (`:116`) | LOCKS + lock14-scan cites | ACCEPT (verbatim from V3) |
| LAC-1E-09 | `A/J profile gate` (`:117`) | LOCKS + Cargo.toml cites | ACCEPT (verbatim from V3) |
| LAC-1E-10 | `H.W0 primitive admission` (`:118`) | LOCKS + intrinsic site cites | ACCEPT (verbatim from V3) |
| LAC-1E-11 | `T-P3 3C lock amendment` (`:119`) | LOCKS + RESULTS + REDRESS cites | ACCEPT (verbatim from V3) |
| LAC-1E-12 (V2 promoted) | `T-P3 3C lock amendment` (`:120`) | PASS-0-OVERFIT-AUDIT + CH7 V3 cites; §1.5 promotion explainer at `:126-128` | ACCEPT (V2-promotion preserved; LOC frame `60-180 docs / low / 240 cap` unchanged; 1.33×) |
| LAC-1E-13 (V4-NEW) | `SK-V14 C-3 R4 + T-P3 3C` (`:121`) | SYNTHESIS + audit-overfit + LOCKS cites | ACCEPT (verbatim from V3) |
| LAC-1E-14 (V4-NEW) | `T-P3 substrate taxonomy + SK-V14 R6 CSS L4 re-admit` (`:122`) | CH2 V3 + 1C-D5 + LOCKS + RESULTS cites | ACCEPT (verbatim from V3) |
| LAC-1E-15 (V4-NEW) | `SK-V14 C-1 PRUNE-4 (9 sub-waves) + T-P3 3C lock amendment` (`:123`) | audit-overfit + builder/arena template + LOCKS cites | ACCEPT (verbatim from V3; LOC frame `4000-8000 / very-high / 11000 cap` preserved; 1.375× within convention) |
| LAC-1E-16 (V4-NEW) | `SK-V14 C-2 bench harness emission + T-P3 3C lock amendment` (`:124`) | SYNTHESIS + CH7 V3 §2.5 cites | ACCEPT (verbatim from V3) |

All 16 LACs pass at V4 HEAD. The V4 sustained-UNKNOWN paragraph at `:35`
does NOT add any new LAC row to the table (`:109-124`) and does NOT
extend the LOC-budget surface; the 4 UNKNOWNs sit in the Open Questions
verify-action register at `:159-164`, which is a CH6 anti-paper-close
sink, not a CH4 cost-budget sink.

## §5 V4 Dispatch-Focus Verification — Two Convergence Points

The V4 dispatch context at
`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:29` narrows
the V4 LOCK-eligible-cycle CH4 disposition to two convergence points.
Each is verified at V4 HEAD (commit `8f4756113`).

| Convergence point | V4 carrier | Verification at V4 HEAD | Disposition |
|---|---|---|---|
| All V4 folds cost-neutral per CH4 V3 cite-rebind cost-neutrality discipline (V3 §7) | All five V4-amended inventories (1A, 1C, 1D, 1E, 1F-anti-pattern) | (i) 1A row-100→117 refresh: zero LOC obligation; cite-housekeeping only. (ii) 1C 126→127 single-token: aligns Executive Summary with V3-already-rescaled 1C-D4 row; the 2.5× consumer-rewire-band multiplier preserves verbatim; per-grammar sum at HEAD reconciles to 127 (`10+10+43+10+10+10+11+13+10`). (iii) 1D W13.9 CORRECTNESS-REJECT split: label-class refinement; row LOC budget `250-500 LOC (revert-heavy)` + `700 LOC` hard cap (1.4×) preserve verbatim. (iv) 1E sustained-UNKNOWN paragraph: adds zero new CH4-class divergence/LAC rows; the 4 UNKNOWNs live in the Open Questions verify-action register (CH6 sink), not the LOC-budget surface (CH4 sink). (v) 1F-anti-pattern AP-009 24→27 cosmetic: HEAD-verified count alignment; AP-009 + AP-020 CH4 cells preserve verbatim. **Net V4 CH4 LOC obligation delta: zero.** | ACCEPT — V4 fold is cost-neutral by construction; CH4 V3 §7 discipline applies uniformly to all five V4 micro-folds; cite-housekeeping and label-refinement and HEAD-cosmetic alignment are explicitly cost-class-neutral per V3 §7. |
| Per-lens LOC repair rescales preserved at HEAD | `1C-D4` at `1C-runtime-evidence.md:162`; AP-020 at `1F-anti-pattern.md:105`; LAC-1E-15 at `1E-locks-evidence.md:123`; LAC-1E-12 at `1E-locks-evidence.md:120`; 1B D8/D10 at `1B-codegen-evidence.md:86-87`; 1A-DIV-008 at `1A-substrate-evidence.md:84` | 1C-D4: `80 (root rewrite) + ~2.5× consumer-rewire band proportional to 127-symbol surface` — verbatim from V3; the 127-surface is V3 rescale not V4 introduction; V4 only aligns the Executive Summary orphan cell at `:40`. AP-020: 160/120 = **1.33×** preserved. LAC-1E-15: 11,000/8,000 = **1.375×** preserved. LAC-1E-12: 240/180 = **1.33×** preserved. 1B-D8: 600/500 = **1.20×** preserved (V3-LOCKED). 1B-D10: 650/500 = **1.30×** preserved (V3-LOCKED). 1A-DIV-008: 1,100/900 = **1.22×** preserved. All substantive multi-LOC kernels sit cleanly within the 1.2-1.4× convention at V4 HEAD with zero rescale drift. | ACCEPT — every per-lens LOC repair rescale preserved bit-for-bit at V4 HEAD; convention compliance unchanged from V3 100% baseline. |

## §6 Findings

| Disposition | Finding | Evidence |
|---|---|---|
| ACCEPT | The V1 + V2 + V3 CH4 six-field schema discipline survives V4 amendment without regression. Every V4-active divergence row (1A 8 rows, 1B 13 rows incl. D8/D10 split, 1C 11 rows, 1D 17 rows, 1E 16 LACs + 16 locks + 5 NEW divergence rows, 1F-anti-pattern 20 AP rows, 1F-coherence 12 COH rows, 1F-past-corpora 17 PC + SKV13-PB rows) carries `loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis`. The V4 micro-fold is uniformly schema-preserving — no row added, no row deleted, no schema column touched. | All V4-active inventories' divergence and amendment tables (cited in §3 table); `grep -n 'loc_budget' restart/audit/totality/p1/1{A,B,C,D,E}-*.md restart/audit/totality/p1/1F-*.md` returns 11 schema header rows across the 8 inventories at V4 HEAD. |
| ACCEPT | The V4 1C 126→127 single-token Executive Summary alignment correctly composes the V3-already-rescaled two-component cost figure (root rewrite + consumer-rewire band) anchored to the live 127-symbol / 47-line / 30-site / 15-file census. The V4 edit does NOT re-rescale the 2.5× multiplier; it aligns the orphan Executive Summary cell at `:40` (which V3 left at 126 per V3 dispatch context §1 micro-scope) with the V3-already-rescaled 1C-D4 row at `:162`. NEW-CH2-V3-02 orphan-cell propagation guard (every "N grammar-named X" cite re-verified at V4 HEAD before ACCEPT) is satisfied; the V4 fold-author propagated the V3 127 rescale to the orphan cell. | `1C-runtime-evidence.md:40` (V4-amended Executive Summary); `:162` (V3-rescaled 1C-D4 row, unchanged in V4); HEAD command outputs `30/15/47/127` reproduce per-grammar sum `10+10+43+10+10+10+11+13+10 = 127`. |
| ACCEPT | The V4 cite-rebind discipline (1A row-100→117; 1D Track 2 `:5,24,43` → `:7,26,34,45`; 1D CSS sidecar `:222,234,299,504` → `:648, 1082, 1203, 1354, 1511, 1661, 1815, 1964, 2691`) preserves every LOC budget, hard cap, and same-wave consumer obligation verbatim. The rebinds are evidentiary corrections (V3 sub-case cites extended to HEAD-verified cluster, plus row-100→117 1D line refresh) and do not move any cost-class needle. CH4 V3 §7 cite-rebind cost-neutrality discipline applies uniformly. | `1A-substrate-evidence.md:84` (1A-DIV-008 substrate-union nuance row, V4-refreshed `1D :117` cross-cite); `1D-skinny-lessons.md:117` (substrate-union row, V4-refreshed Track 2 + CSS sidecar cite clusters); HEAD `grep -c 'lightningcss_facts'` returns 27. |
| ACCEPT | The V4 W13.9 CORRECTNESS-REJECT label split at `1D :140` preserves the V3 row LOC budget verbatim. The split is REJECT-class label refinement (W13.5-W13.8 MEASURED-REJECT cluster + W13.9 CORRECTNESS-REJECT singleton); the row's `250-500 LOC (revert-heavy)`, `MED-LOW` risk, `C-5 PRUNE-1+PRUNE-2` wave, `700 LOC` hard cap (1.4× — at upper bound of convention), and same-wave consumer obligation preserve from V3. No REDRESS route re-opened; both W13.5-W13.8 + W13.9 remain bound and remain NOT-PASS-ADMIT. CH3 lens disposition (label split) is cost-neutral by CH4 V3 §7 discipline. | `1D-skinny-lessons.md:140`; `restart/skinny/REDRESS.md:4621/4645/4674/4704/4734` (5 REDRESS line cites preserve as 4+1 split, not 5 lost). |
| ACCEPT | The V4 1E sustained-UNKNOWN paragraph at `:35` is anti-paper-close narrative anchor only; it adds ZERO new CH4-class rows to either the divergence table (`:97-101`) or the LAC table (`:109-124`). The 4 UNKNOWNs (L03, L16, audit-overlay column gap, Lock 1 fact-stream taxonomy) live in the Open Questions verify-action register at `:159-164`, which is a CH6 anti-paper-close sink. The 4 verify_actions are executable (golden test, traceability manifest, `grep -c 'track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict' skinny/RESULTS.md`, T-P3 §3C disposition). Net CH4 LOC delta for the 1E V4 fold: **zero**. | `1E-locks-evidence.md:35` (V4-inserted paragraph); `:159-164` (Open Questions table with executable verify_actions); §3Z gate cohort LOCK trajectory preserved. |
| ACCEPT | The V4 1F-anti-pattern AP-009 24→27 cosmetic update at `:69` aligns the AP-009 detail-row narrative with the HEAD-verified `grep -c` count. V3 §7 self-test against CH7 fabrication discipline is institutionalized at V4: every cite-bearing micro-fold carries an executable verification spot at V4 HEAD. **Zero CH4-class cell touched** — AP-009 row at `:94` retains `60-160 LOC classification / medium-high risk / 220 LOC hard cap` (1.375×) verbatim; AP-020 row at `:105` retains `40-120 LOC / 160 LOC hard cap` (1.33×) verbatim. | `1F-anti-pattern.md:69` (V4-amended detail row); `:94, 105` (V3-preserved CH4 cells); HEAD `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns **27** (2026-05-23). |
| ACCEPT | The V1 + V2 + V3 + V4 CH4 hard-cap multiplier convention (1.2-1.4× upper bound for kernel/amendment-candidate rows) survives V4 amendment. Substantive-kernel spot-check at V4 HEAD: AP-020 1.33×; AP-009 1.375×; 1B-D8 1.20×; 1B-D10 1.30×; 1A-DIV-008 1.22×; LAC-1E-12 1.33×; LAC-1E-15 1.375×; 1D row `:140` 700/500 = 1.4× (at upper bound). All within convention. Two sub-1.2× rows persist (1A-DIV-005 700/600 = 1.17× taxonomy/doc class; COH-011 1,400/1,200 = 1.17× PRUNE-4-bundled) and remain non-blocking per V3 §6 finding row 5. CH4 reaffirms the V1 + V2 + V3 non-blocking recommendation that T-P3 §3C codify the multiplier convention in Lock 8 V+1 wording. | Per-row hard cap citations across §3 + §5 tables; explicit sub-1.2× rows enumerated. |
| ACCEPT | The V4 executable-verification mandate (LAC-1E-12 + NEW-CH2-V3-02 per V4 dispatch context §3) operates at full strength across all five V4 micro-folds: every cite re-verified at V4 HEAD before ACCEPT. Pre/post grep evidence captured per LAC-1E-12 procedural addendum: 1A `:100` → `:117` (2 substitutions, zero residuals); 1C `126` → `127` (single token); 1D W13.9 split (REDRESS line cites preserved 4+1); 1E paragraph (Open Questions table 4 UNKNOWNs); 1F-anti-pattern `grep -c` 27 alignment. CH4 V4 propagates ZERO fabricated cite at any cost-relevant row — anti-paper-close discipline operates at maximum strength for the second consecutive cycle. | V4 dispatch context §3; §3 + §5 inline HEAD command outputs; `git diff 0a9f1288c 8f4756113 -- restart/audit/totality/p1/1{A,C,D,E}-*.md restart/audit/totality/p1/1F-anti-pattern.md` resolves to exactly the 5 cited micro-folds with no cost-class drift. |

## §7 New Finding (Lens-Local, V4 LOCK-eligible cycle)

| Note | Detail |
|---|---|
| CH4 V4 cite-rebind + label-refinement + paragraph-insertion cost-neutrality discipline | Every V4 micro-fold in this LOCK-eligible cycle (1A row-100→117 cosmetic; 1C 126→127 single-token; 1D W13.9 CORRECTNESS-REJECT split; 1D Track 2 + CSS sidecar cite cluster extension; 1E sustained-UNKNOWN paragraph; 1F-anti-pattern AP-009 24→27 cosmetic) is **cost-neutral by construction** — extending the V3 §7 cite-rebind cost-neutrality discipline to three additional cost-neutral micro-fold classes: (a) **cite-cosmetic** (1A row-117, 1F-anti-pattern 27, 1C 127): aligns orphan cells with HEAD-verified counts; zero LOC cell touched. (b) **REJECT-label-refinement** (1D W13.9 split): refines label-class precision; preserves REDRESS line cites; zero LOC cell touched. (c) **anti-paper-close-paragraph-insertion** (1E sustained-UNKNOWN at `:35`): inserts narrative anchor for the Open Questions verify-action register (CH6 sink); does NOT add CH4-class rows; zero LOC cell touched. All three classes generalize V3 §7's cite-rebind class. CH4 V4 explicitly registers this as the correct disposition: cite-housekeeping + label-refinement + anti-paper-close narrative are not cost-class change. No CH4 revise triggered. **Net V4 CH4 LOC obligation delta: zero.** |
| CH4 V4 self-test against CH7 V2 fabrication + CH7 V3 cite-rebind discipline | CH7 V2 carried a fabrication (false "lightningcss_facts zero hits" dispatch claim) corrected at V3 to 24 hits (HEAD-verified at V3 timestamp). V4 lifts to **27 hits** (HEAD-verified at V4 timestamp 2026-05-23) at `1F-anti-pattern.md:69`. The 24→27 delta reflects 3 additional `lightningcss_facts` token occurrences in the live `nonjson_css_l4.rs` file between V3 commit `0a9f1288c` and V4 commit `8f4756113` (likely additional sub-grammar wave callsites; the 7 per-grammar siblings + definition + callsites cluster preserves). CH4 V4 propagates ZERO fabricated cite at any cost-relevant row — anti-paper-close discipline operates at maximum strength for the third consecutive cycle (V2 corrected → V3 refreshed → V4 re-refreshed). |
| LOCK trajectory: 4-cycle extension (V1+V2+V3+V4) | V1 + V2 + V3 + V4 CH4 cycles each disposed ACCEPT 8/8 = 100% with 16/16 LAC wave-alignment + path:line hits. §3Z gate (≥95% × 2 consecutive cycles for cohort LOCK): V3 = first ≥95% cycle (100%); V4 = second consecutive ≥95% cycle (100%). CH4 contributes ACCEPT 8/8 = 100% to the V4 cohort cycle. The 1.2-1.4× hard-cap multiplier convention, the 127-symbol consumer-rewire surface, the 4 sustained UNKNOWNs (L03, L16, audit-overlay column gap, Lock 1 fact-stream taxonomy), and the V3 §7 cite-rebind cost-neutrality discipline all preserve at V4 HEAD without drift. **CH4 recommends 4-cycle LOCK extension (V1+V2+V3+V4) for the T-P1 cohort §3Z LOCK trajectory.** V5 confirming cycle is no longer required for cohort LOCK under §3Z's ≥95% × 2 consecutive criterion (V3+V4 satisfies it); V5 may extend to ceiling but is not gating. |

## §8 Required Revisions

None. All eight T-P1 inventories pass CH4 ACCEPT at V4 HEAD (commit
`8f4756113`) under the orchestrator six-field schema, the V4
dispatch-context two-convergence-point focus, the V1 + V2 + V3 + V4 CH4
16/16 LAC wave-alignment + path:line discipline, and the V1 + V2 + V3 +
V4 CH4 1.2-1.4× hard-cap convention. The V4 LOCK-eligible-cycle
micro-fold introduces zero CH4-side regression and zero new LOC
obligation beyond the explicit V3 cite-rebind cost-neutrality discipline
extended to three additional cost-neutral classes (cite-cosmetic,
REJECT-label-refinement, anti-paper-close-paragraph-insertion).

## §9 Cycle Disposition

ACCEPT. 8/8 T-P1 inventories pass at V4 HEAD. 16/16 LACs pass
dispatch-required wave-alignment + path:line check at V4 HEAD (commit
`8f4756113`). Both V4 dispatch-context convergence points verify:
(i) all V4 folds cost-neutral per CH4 V3 §7 cite-rebind cost-neutrality
discipline (extended to three additional cost-neutral micro-fold
classes); (ii) per-lens LOC repair rescales preserved bit-for-bit at HEAD
(1C 127-symbol surface; AP-020 1.33×; LAC-1E-15 1.375×; LAC-1E-12 1.33×;
1B-D8 1.20×; 1B-D10 1.30×; 1A-DIV-008 1.22×; 1D `:140` 1.4× at upper
bound). The V4 micro-fold is cost-neutral by construction — cite-cosmetic
alignment + REJECT-label refinement + anti-paper-close-paragraph
insertion are not cost-class change. §3Z gate: V4 is the second
consecutive cohort-wide ≥95% cycle for T-P1 (V3 = 100%, V4 = 100%); CH4
contributes ACCEPT 8/8 = 100% to that cycle, establishing the LOCK
trajectory.

## §10 LOCK Extension

**4-cycle LOCK (V1+V2+V3+V4) recommended.** CH4 has disposed ACCEPT 8/8
= 100% for four consecutive cycles (V1, V2, V3, V4) with zero REVISE,
zero REJECT, and zero regression across the orchestrator six-field
schema, the 16/16 LAC wave-alignment + path:line discipline, and the
1.2-1.4× hard-cap multiplier convention. §3Z's ≥95% × 2 consecutive
cycles criterion is satisfied by V3+V4 alone (V3 = 100%, V4 = 100%); the
V1+V2+V3+V4 4-cycle extension demonstrates steady-state cost-class
preservation across both V3 §7 cite-rebind cost-neutrality discipline
and its V4-extended cost-neutral micro-fold classes (cite-cosmetic,
REJECT-label-refinement, anti-paper-close-paragraph-insertion). V5
confirming cycle is no longer required for cohort LOCK under §3Z;
V5 may extend to V≤5 ceiling but is not gating for CH4.

## §11 Aggregator Note

CH4 V4 disposition: ACCEPT. 8/8 T-P1 inventories. 16/16 LACs. 2/2 V4
dispatch-context convergence points. 4-cycle LOCK extension
(V1+V2+V3+V4) recommended. Carry-forward V1 + V2 + V3 non-blocking
governance recommendations to T-P3 §3C: (i) adopt LAC-1E-15 per-tranche
framing as the load-bearing Pattern H budget; (ii) codify the 1.2-1.4×
hard-cap multiplier convention in Lock 8 V+1 wording (with explicit
sub-1.2× exemption for taxonomy/doc-class + bundled-into-larger-wave
rows); (iii) consider promoting the V3 procedural addendum (executable
verification of every cite at V+1 HEAD before ACCEPT) to a formal
LAC-1E-12 sub-clause — the V4 LOCK-eligible cycle has now operationalized
this discipline across four consecutive cycles at zero new LOC cost,
making the formalization the natural codification rather than a
disruptive amendment. LAC-1E-12 V2-promotion to
"candidate-promoted-to-T-P3-§3C-priority" preserved at V4 HEAD without
cost-class change. The V4 cite-rebind + label-refinement +
paragraph-insertion cohort across 1A + 1C + 1D + 1E + 1F-anti-pattern is
uniformly cost-neutral — extending V3 §7's evidentiary-correction-is-
not-cost-class discipline to three additional cost-neutral micro-fold
classes; CH4 V4 explicitly registers all three as the correct disposition
for any future cycle.
