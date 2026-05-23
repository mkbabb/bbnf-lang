---
agent: CH4
pass: T-P1-excavation
cycle: V2
lens: COST
generated_at: 2026-05-23T23:30:00-04:00
disposition: ACCEPT
audited_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md (V2)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V2)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V2)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V2)
  - restart/audit/totality/p1/1E-locks-evidence.md (V2)
  - restart/audit/totality/p1/1F-anti-pattern.md (V2)
  - restart/audit/totality/p1/1F-coherence-scan.md (V2)
  - restart/audit/totality/p1/1F-past-corpora.md (V2)
authority:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3 (CH4)
  - restart/prompts/ORCHESTRATOR.md §3W + §3Z
  - restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/hardening/V1/CH4.md (100% ACCEPT — V1 baseline)
v2_head_commit: 87816a2cd
accept_count: 8
revise_count: 0
reject_count: 0
acceptance_rate: 8/8 (100%)
---

## §1 Lens Basis

`restart/prompts/totality/PASS-1-EXCAVATION.md §3` CH4 requires (a) every
divergence carry a realistic LOC-delta + risk class, and (b) 1E amendment
candidates state a wave-alignment hint. `restart/prompts/ORCHESTRATOR.md:86`
sharpens to a six-field schema per kernel/primitive: `loc_budget | risk |
wave | hard_cap | same_wave_consumer | evidence_basis`. V2 dispatch context
at `restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md:27` narrows
the V2-cycle CH4 focus to five convergence points: (i) 1C LOC repair
rescale (~50→~190 LOC + 2.5× consumer-rewire band proportional to the
126-symbol surface); (ii) 1B D8/D10 split LOC-delta correctness; (iii) AP-020
LOC band correctness (40-120 fence + 160 consumer); (iv) 1D row 113
cross-cite preserving the SK-V14 R4 regen-css xtask wave-alignment hint;
(v) LAC-1E-12 promotion introduces no new LOC obligation. The V1 CH4 cycle
disposed ACCEPT 8/8 with 16/16 LAC wave-alignment hits and surfaced two
non-blocking governance recommendations (LAC-1E-15 per-tranche framing
convergence; 1.2-1.4× hard-cap multiplier codification); the V2 micro-fold
(commit `87816a2cd`) must preserve those V1 disciplines without regression.

## §2 Cycle Verdict

ACCEPT. All eight V2-amended T-P1 inventories carry full CH4 cost framing
under the orchestrator six-field schema. Each V2 dispatch-context convergence
point is verifiable at HEAD:

- 1C undercount rescale lands at `1C-runtime-evidence.md:161` with
  `80 (root rewrite) + ~2.5× consumer-rewire band proportional to 126-symbol
  surface` — a properly two-component figure that replaces V1's
  single-component "~50 LOC" floor. The 126-symbol / 47-line / 30-site /
  15-file census is captured verbatim in the verification block at
  `1C-runtime-evidence.md:199-200`, reproducible at HEAD via
  `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/`
  (returns 30 hits across 15 files at 2026-05-23).
- 1B D8/D10 row split at `1B-codegen-evidence.md:86-87` carries two distinct
  LOC budgets (250-500 each) with distinct hard caps (600 / 650 LOC) and
  distinct same-wave consumer obligations (Sheets/BBNF-self recognizer
  fixture vs. role-mining fixture). Each row carries the
  NECESSARY-BUT-INSUFFICIENT-relative-to-PRUNE-4 framing the V2
  dispatch-context demands.
- AP-020 LOC band at `1F-anti-pattern.md:105` reads `40-120 LOC
  fence/classification` with hard cap `160 LOC` and same-wave consumer
  `CSS comparator-sidecar fence consumer`, co-wave-routed with AP-009 (CSS
  evidence-accounting wave). 160 / 120 = 1.33×, conforming to the V1 CH4
  1.2-1.4× hard-cap convention.
- 1D row 113 (`bbnf-simd` grammar-neutral substrate lesson) at
  `1D-skinny-lessons.md:131` cross-cites PC-008 + U-PC-002 and routes the
  verify-before-rederive obligation against the SK-V14 axis A3 v3 §4
  verdict; the R4 regen-css xtask wave-alignment hint is preserved at three
  sibling rows (`:122` / `:133` / `:134`) and at the R4 → PRUNE-2 sequencing
  row at `:148-149` (`0 LOC (sequencing only)` correctly classified per V1
  CH4 sequencing-only cost class).
- LAC-1E-12 promotion to "candidate-promoted-to-T-P3-§3C-priority" at
  `1E-locks-evidence.md:120` and the §1.5 governance-signal explainer at
  `:126-128` preserve the V1 cost framing verbatim (`60-180 LOC docs / low
  risk / 240 LOC hard cap`); promotion adds zero new LOC obligation and
  remains non-blocking for V2 mechanical convergence, satisfying the V2
  dispatch-context "T-P1 PROPOSES; T-P3 disposes; Pass Omega merges" rule.

The V1 CH4 100% ACCEPT carries forward without retreat. §3Z gate
first-cycle ≥95% reached on V1 (100%) and reaffirmed on V2 (100%); §3Z
standalone-closed at V2. Predicted V2 → LOCK trajectory; no V3 CH4 dispatch
required.

## §3 Per-Artefact Verdict Table

| Artefact | Disposition | CH4 six-field schema present | Path:line verified at HEAD | Notes |
|---|---|---|---|---|
| 1A-substrate-evidence.md (V2) | ACCEPT | Yes — divergence-table header at `1A-substrate-evidence.md:75` carries `loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis`; 8 divergence rows at `:77-84` populate every column; 1A-LOCK1-AMEND-001 candidate row at `:112` carries the same six-field schema. | Yes — every row carries spec path:line + impl path:line in `evidence_basis`. | V2 amendment to 1A-DIV-008 substrate-union nuance row at `:84` preserves the V1 `400-900 LOC / cap 1,100 LOC` cost frame and adds the substrate-union ratification disposition language (`100-300 LOC if Pass Omega ratifies two-cursor shape`). The 1D `:100` cross-fold pointer is uniformly preserved; T-P3 §3C PENDING flag carried per `1A-UNK-005`. |
| 1B-codegen-evidence.md (V2) | ACCEPT | Yes — divergence-table header at `1B-codegen-evidence.md:77` carries all six fields; 13 divergence rows at `:79-90` populate them; the D8/D10 split rows at `:86-87` each carry distinct LOC budgets, hard caps, and same-wave consumer obligations; 3 amendment candidates at `:114-116` populate the same six-field schema. | Yes — every row carries spec/impl path:line + reproducible `verify_action` column. | V2 D8/D10 split is the load-bearing V2 micro-fold for CH4: row D8 (recognizer-byte alphabet plane) and row D10 (literal-predicate role-mining plane) now stand as distinct upstream Sheets/BBNF-self generalization blockers, each fenced as NECESSARY-BUT-INSUFFICIENT relative to PRUNE-4. The 250-500 LOC budgets are realistic for pass-layer recognizer/role-mining rewrite scope; the 600 / 650 LOC hard caps respect the V1 CH4 1.2-1.4× multiplier convention. |
| 1C-runtime-evidence.md (V2) | ACCEPT | Yes — divergence-table header at `1C-runtime-evidence.md:156` carries all six fields; 11 divergence rows at `:158-168` populate them. | Yes — every row carries spec path:line + live workspace path:line; verification commands captured at `:194-201`. | The V2 dispatch-focus 1C undercount rescale is the load-bearing fold: 1C-D4 at `:161` reads `80 (root rewrite) + ~2.5× consumer-rewire band proportional to 126-symbol surface` (replaces V1's "~50 LOC" with a properly two-component figure); 1C-D5 at `:162` reads `480 LOC` for the parse_with shim leak across 4 files × 4 sites. The verification block at `:199-200` captures the 30-site / 15-file / 126-symbol / 47-line census verbatim, reproducible at HEAD (HEAD command output 30 at 2026-05-23). `locks_amendment_candidates: 0` (1C defers all amendment surfacing to 1E). |
| 1D-skinny-lessons.md (V2) | ACCEPT | Yes — divergence-table header at `1D-skinny-lessons.md:138` carries all six fields; 17 divergence rows at `:140-158` populate them. | Yes — every row carries SK-V14 SYNTHESIS or audit-overfit path:line + live REDRESS / p1e-hot-leaf-attribution / SYNTHESIS-AUDIT-OVERFIT cites. | Row 113 (the `bbnf-simd` grammar-neutral substrate lesson at `1D:131`) is the V2 dispatch-focus convergence point: V2 amendment cross-cites `1F-past-corpora.md:74` PC-008 + `1F-past-corpora.md:158` U-PC-002 and routes the verify-before-rederive obligation as a non-LOC-bearing audit obligation. The SK-V14 R4 regen-css xtask wave-alignment hint flows through three sibling rows (`:122` / `:133` / `:134`) and a sequencing-only row at `:148-149` (`0 LOC (sequencing only)` correctly classified per V1 CH4 sequencing-only cost class). 1D row 106 1B D8/D10 split mirror lands at `1D:124` with V2 fold (CH2 required revision 2 + V1-CONSOLIDATED §1.4) explicitly naming the two-layer (codegen-name + grammar-shape) generalization blocker. 1D row 100 substrate-union nuance at `1D:117` carries the T-P3 §3C PENDING flag with no new LOC obligation. |
| 1E-locks-evidence.md (V2) | ACCEPT | Yes — 16 LACs table header at `1E-locks-evidence.md:107` carries all six fields; 16 LAC rows at `:109-124` populate every column; Lock spec-claim table at `:72-89` carries `LOC / risk | Hard cap | Same-wave consumer | Wave alignment hint` for every one of the 16 locks; SK-V14 NEW divergence-row table at `:95-101` carries the six-field schema for D-1E-12..16. | Yes — every LAC carries supporting path:line evidence (see §4 line-by-line verification). | V2 amendment promotes LAC-1E-12 to "candidate-promoted-to-T-P3-§3C-priority" at `:120`; the promotion explainer at `:126-128` adds COH-012 meta-CH7 collision cross-cite + LAC-1E-12 anti-fabrication phrasing template (`LOCKS.md (no CH7 mention)` at `:97,120,145`). **No new LOC obligation is introduced** — LAC-1E-12 retains the V1 `60-180 LOC docs / low risk / 240 LOC hard cap` frame verbatim. T-P1 PROPOSES; T-P3 disposes whether Lock 17/18 numbering or in-preface CH7-binding clause is the carrier (both routes cost-bounded by the same envelope). |
| 1F-coherence-scan.md (V2) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-coherence-scan.md:97-110` carries all six fields for 12 COH rows. | Yes — every row carries spec path:line + SK-V14 SYNTHESIS path:line in `evidence_basis`. | V2 amendment to COH-012 at `:93` + `:110` corrects the V1 fabricated `LOCKS.md:46 declares "Lock 14 + CH7 Overfit-Prune lens binding"` cite to the anti-fabrication phrasing `restart/locks/LOCKS.md (no CH7 mention; grep -n "CH7\|Overfit" returns zero hits)` — verifiable at HEAD (grep returns 0 at 2026-05-23). The 30-60 LOC docs cost class preserves. COH-011 nine-grammar census at `:92` + `:109` reads `0 LOC census; 600-1200 LOC PRUNE-4` with `1400 LOC` hard cap — correctly bundled into PRUNE-4 (9 sub-waves; google_sheets=10 verified at HEAD). `locks_amendment_candidates: 0` (deferred to 1E). |
| 1F-anti-pattern.md (V2) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-anti-pattern.md:84-105` carries all six fields for 20 AP rows (AP-012..AP-020 covers the SK-V14 + V2 fold additions). | Yes — every row carries live impl path:line in `evidence_basis`; AP-016 at `:101` corrects google_sheets census to 10 files (reproducible at HEAD via `find crates/core/src/runtime/google_sheets -type f \| wc -l` returns 10). | The V2 dispatch-focus AP-020 row at `:105` reads `40-120 LOC fence/classification | medium-high risk | CSS evidence-accounting wave (co-wave with AP-009) | 160 LOC hard cap | CSS comparator-sidecar fence consumer | live path:line evidence at nonjson_css_l4.rs:222,234,299,504`. The 160 LOC hard cap is exactly 1.33× the 120 LOC upper bound, conforming to the V1 CH4 1.2-1.4× hard-cap convention. Companion AP-009 at `:94` carries 60-160 LOC classification / 220 LOC cap — the two rows partition cleanly (AP-009 = classification framing; AP-020 = sidecar-as-anti-pattern fencing) with no LOC double-counting. AP-002 retains `UNKNOWN mixed-concern status` and routes to a post-inventory hard cap of 300 LOC — correctly framed per CH4 + CH6 boundary. |
| 1F-past-corpora.md (V2) | ACCEPT | Yes — `V2 Planning Metadata (authoritative CH4 carrier)` table at `1F-past-corpora.md:124-142` carries all six fields for 17 PC + SKV13-PB rows. | Yes — every row carries SK-V14 alpha-C / SYNTHESIS / REDRESS path:line in `evidence_basis`; PC-017 at `:120` + `:140` corrects google_sheets census to 10 files (matches AP-016 / COH-011). | PC-008 V2 fold at `:74` carries the SK-V5 verify-before-rederive obligation as `80-200 LOC verify; medium risk`; the U-PC-002 verify_action at `:158` carries the captured-artefact requirement (`rg -n 'JSON_STRUCTURAL\|scan_json\|JsonParseIndex' skinny/crates/bbnf-simd skinny/crates/runtime`). PC-001/002/004 verdicts now mirror PC-003 parity (`accepted historical pre-block; current absence UNKNOWN`) with `0 LOC` / `medium if reopened` cost class — correctly framed per the V1 CH4 "0 LOC pre-block; cost is the cost of respecting a rejected route" distinction. |

## §4 Verification — 1E's 16 LACs Carry Wave-Alignment Hint + Path:line at V2 HEAD

The V2 dispatch-context requires explicit confirmation that V2 amendments do
not break the V1 16/16 LAC wave-alignment + path:line discipline. Verified
row-by-row against `restart/audit/totality/p1/1E-locks-evidence.md:109-124`.

| LAC | wave column populated (V2) | evidence path:line populated (V2) | Verdict |
|---|---|---|---|
| LAC-1E-01 | `A/F substrate + C cost model` (`:109`) | `skinny/REDRESS.md:246`, `:274`; `skinny/crates/runtime/src/tape/mod.rs:94`; `hardening/V3/CH5.md:34-39` (`:109`) | ACCEPT (verbatim from V1) |
| LAC-1E-02 | `C.W1` (`:110`) | `restart/locks/LOCKS.md:92-100`; `skinny/crates/passes/src/lib.rs:84` (`:110`) | ACCEPT (verbatim from V1) |
| LAC-1E-03 | `G.W1/G.W2` (`:111`) | `restart/locks/LOCKS.md:102-109` (`:111`) | ACCEPT (verbatim from V1) |
| LAC-1E-04 | `H + SK-V14 R6/R7/R8` (`:112`) | `sk-v14/SYNTHESIS.md:54-60`; `:191-198`; `skinny/RESULTS.md:94`; `skinny/RESULTS.md:5-35` (`:112`) | ACCEPT (verbatim from V1) |
| LAC-1E-05 | `B/G runtime API` (`:113`) | `restart/locks/LOCKS.md:155-162`; `skinny/crates/bbnf/src/lib.rs:75-83` (`:113`) | ACCEPT (verbatim from V1) |
| LAC-1E-06 | `A.W0/A.W1` (`:114`) | `restart/locks/LOCKS.md:190-205`; `Cargo.toml:2`; `restart/MIGRATION.md:70, 604` (`:114`) | ACCEPT (verbatim from V1) |
| LAC-1E-07 | `A tree-shape + bench hardening` (`:115`) | `restart/locks/LOCKS.md:207-218`; `skinny/REDRESS.md:299` (`:115`) | ACCEPT (verbatim from V1) |
| LAC-1E-08 | `T-P3 3C lock amendment + SK-V14 C-1` (`:116`) | `restart/locks/LOCKS.md:220-263`; `sk-v14-audit-overfit-lock14-scan.md:88-95, 7-9` (`:116`) | ACCEPT (verbatim from V1) |
| LAC-1E-09 | `A/J profile gate` (`:117`) | `restart/locks/LOCKS.md:265-280`; `skinny/Cargo.toml:74`; `Cargo.toml:80` (`:117`) | ACCEPT (verbatim from V1) |
| LAC-1E-10 | `H.W0 primitive admission` (`:118`) | `restart/locks/LOCKS.md:309-318`; `digit_mac.rs:39`; `cache_hints.rs:6`; `sk-v14/SYNTHESIS.md:104-148` (`:118`) | ACCEPT (verbatim from V1) |
| LAC-1E-11 | `T-P3 3C lock amendment` (`:119`) | `restart/locks/LOCKS.md:1-13`, `:220-263`; `skinny/RESULTS.md:94`; `skinny/REDRESS.md:3824` (`:119`) | ACCEPT (verbatim from V1) |
| LAC-1E-12 (V2 promoted) | `T-P3 3C lock amendment` (`:120`) | `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`; `hardening/V3/CH7.md:54-62` (`:120`); §1.5 promotion explainer at `:126-128` adds COH-012 meta-CH7 collision cross-cite | ACCEPT (LOC frame `60-180 docs / low / 240 cap` preserved verbatim; promotion sharpens wave-alignment to T-P3 §3C priority without introducing new LOC obligation, satisfying V2 dispatch-context "T-P1 PROPOSES; T-P3 disposes" rule) |
| LAC-1E-13 (V4-NEW) | `SK-V14 C-3 R4 + T-P3 3C` (`:121`) | `sk-v14/SYNTHESIS.md:96`, `:110-120`; `sk-v14-audit-overfit-pre-restart-pattern.md:153, 184`; `restart/locks/LOCKS.md:115`, `:222-238` (`:121`) | ACCEPT (verbatim from V1) |
| LAC-1E-14 (V4-NEW) | `T-P3 substrate taxonomy + SK-V14 R6 CSS L4 re-admit` (`:122`) | `hardening/V3/CH2.md:87`; `1C-runtime-evidence.md:102`; `LOCKS.md:66-71`; `skinny/RESULTS.md:94`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:4-6` (`:122`) | ACCEPT (verbatim from V1) |
| LAC-1E-15 (V4-NEW) | `SK-V14 C-1 PRUNE-4 (9 sub-waves) + T-P3 3C lock amendment` (`:123`) | `sk-v14-audit-overfit-pre-restart-pattern.md:10-12, 41-56, 153-157`; `crates/core/src/runtime/builder_template.rs:13-31`; `arena_template.rs:1-31`; `LOCKS.md:220-263` (`:123`) | ACCEPT (verbatim from V1; LOC frame `4000-8000 / very-high / 11000 cap` preserved) |
| LAC-1E-16 (V4-NEW) | `SK-V14 C-2 bench harness emission + T-P3 3C lock amendment` (`:124`) | `sk-v14/SYNTHESIS.md:240-255`; `:230`; CH7 V3 §2.5 (`:124`) | ACCEPT (verbatim from V1) |

All 16 LACs pass. The V2 micro-fold introduces no schema regression. LAC-1E-12
V2 promotion is the only delta; it sharpens wave-alignment without changing
cost class.

## §5 V2 Dispatch-Focus Verification — Five Convergence Points

The V2 dispatch context at `restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md:23-30`
narrows the V2 CH4 disposition to five convergence points. Each is verified
at V2 HEAD (commit `87816a2cd`).

| Convergence point | V2 carrier | Verification | Disposition |
|---|---|---|---|
| 1C LOC repair rescales (~50→~190 LOC + 2.5× consumer-rewire band) | `1C-runtime-evidence.md:161` (1C-D4 row); `1C-runtime-evidence.md:199-200` (verification block) | `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` returns 30 at HEAD; the `mod.rs:25-71` 47-line / 126-symbol mechanical extraction is reproduced verbatim; V2 framing `80 (root rewrite) + ~2.5× consumer-rewire band proportional to 126-symbol surface` correctly composes (a) root rewrite cost and (b) consumer-rewire band cost, replacing V1's single-component "~50 LOC" undercount. | ACCEPT — LOC rescale correctly captures both root + consumer obligation; 2.5× multiplier is proportional to the 126-symbol consumer surface scale. |
| 1B D8/D10 split LOC delta correctness | `1B-codegen-evidence.md:86` (D8); `1B-codegen-evidence.md:87` (D10); each with distinct 250-500 LOC budget + 600 / 650 LOC hard cap | D8 captures recognizer-plane JSON-byte whitelist at `skinny/crates/passes/src/lib.rs:331` (inside `derive_recognizers` at `:325`); D10 captures role-mining JSON-literal predicates at `:1300-1391` (`derive_materialization_roles` at `:1302`); both rows carry NECESSARY-BUT-INSUFFICIENT-relative-to-PRUNE-4 framing in the `evidence_basis` column. Verify commands distinguish the two planes (recognizer-byte alphabet vs. literal-predicate role inference). | ACCEPT — split LOC budgets are realistic for pass-layer rewrite scope; hard caps respect 1.2-1.4× upper-bound convention; same-wave consumer obligations distinct per plane. |
| AP-020 LOC band (40-120 fence + 160 consumer) | `1F-anti-pattern.md:80` (AP-020 detail row); `1F-anti-pattern.md:105` (V2 Planning Metadata) | AP-020 reads `40-120 LOC fence/classification` with `160 LOC` hard cap (1.33× upper bound; conforms to V1 CH4 multiplier convention); same-wave consumer `CSS comparator-sidecar fence consumer`; co-wave with AP-009 (CSS evidence-accounting wave). Companion AP-009 at `:94` carries 60-160 LOC / 220 LOC cap — the two rows partition cleanly (AP-009 = classification framing; AP-020 = sidecar-as-anti-pattern fencing) with no LOC double-counting. CH5-004 binding fold per V2 dispatch context is correctly carried. | ACCEPT — band is correctly sized for fencing/classification scope; hard cap is conventionally bounded; co-wave routing is correct. |
| 1D row 113 cross-cite preserves SK-V14 R4 regen-css xtask wave-alignment hint | `1D-skinny-lessons.md:131` (`bbnf-simd` row); `:122` / `:133` / `:134` (sibling R4 rows); `:148-149` (sequencing-only row) | Row 113 cross-cites `1F-past-corpora.md:74` PC-008 + `1F-past-corpora.md:158` U-PC-002 + V2 fold (CH2 required revision 3); the SK-V14 R4 regen-css xtask wave-alignment hint flows through three sibling rows; the R4 → PRUNE-2 sequencing row at `:149` correctly carries `0 LOC (sequencing only)` / `HIGH risk` / `n/a hard cap` — the V1 CH4 sequencing-only cost class is preserved and respected. | ACCEPT — R4 wave-alignment is uniformly preserved across all relevant 1D rows; sequencing-only cost class correctly framed; verify-before-rederive obligation is non-LOC-bearing audit work, not implementation work. |
| LAC-1E-12 promotion introduces no new LOC obligation | `1E-locks-evidence.md:120` (LAC-1E-12 row); `:126-128` (§1.5 promotion explainer) | LAC-1E-12 V2 row preserves V1 cost framing `60-180 LOC docs / low risk / 240 LOC hard cap` verbatim; promotion explainer at `:126-128` adds wave-alignment sharpening ("candidate-promoted-to-T-P3-§3C-priority") + COH-012 meta-CH7 collision cross-cite, with explicit "non-blocking for V2 mechanical convergence" gate. T-P3 §3C disposes whether Lock 17/18 numbering or in-preface CH7-binding clause is the carrier — both routes cost-bounded by the same 60-180 LOC docs envelope. | ACCEPT — promotion is wave-alignment sharpening, not cost-class change; "T-P1 PROPOSES; T-P3 disposes; Pass Omega merges" rule satisfied. |

## §6 Findings

| Disposition | Finding | Evidence |
|---|---|---|
| ACCEPT | The V1 CH4 six-field schema discipline survives V2 amendment without regression. Every V2-amended divergence row (1A 8 rows, 1B 13 rows incl. D8/D10 split, 1C 11 rows, 1D 17 rows, 1E 16 LACs + 16 locks + 5 NEW divergence rows, 1F-anti-pattern 20 AP rows, 1F-coherence 12 COH rows, 1F-past-corpora 17 PC + SKV13-PB rows) carries `loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis`. The V2 micro-fold is uniformly schema-preserving. | All V2 inventories' divergence and amendment tables (cited in §3 table). |
| ACCEPT | The V2 1C undercount rescale correctly composes a two-component cost figure (root rewrite + consumer-rewire band) anchored to the live 126-symbol / 47-line / 30-site / 15-file census. The V1 "~50 LOC" single-component undercount is closed; the V2 ~190 LOC + 2.5× consumer-rewire band framing is reproducible at HEAD via captured `rg`/mechanical-extraction commands. CH4 explicitly accepts the 2.5× multiplier as proportional to the 126-symbol consumer surface scale. | `1C-runtime-evidence.md:161` (1C-D4); `:199-200` (verification block); HEAD command `rg -n 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/core/src/runtime/ \| wc -l` returns 30 (2026-05-23). |
| ACCEPT | The V2 1B D8/D10 row split carries distinct LOC budgets, hard caps, and same-wave consumer obligations across two genuinely distinct upstream Sheets/BBNF-self generalization blockers (recognizer-byte alphabet plane at `passes/src/lib.rs:331` vs. literal-predicate role-mining plane at `:1300-1391`). The NECESSARY-BUT-INSUFFICIENT-relative-to-PRUNE-4 framing is correctly carried in both rows. CH2 hardening fold provenance is uniformly cited. | `1B-codegen-evidence.md:86-87` (D8 + D10 rows); `1B:69-73` (Sheets / BBNF-Self Implications section). |
| ACCEPT | The V2 AP-020 fence/classification row sizes the CSS comparator-sidecar fencing obligation at 40-120 LOC with a 160 LOC hard cap (1.33× upper bound) and routes the consumer through the same CSS evidence-accounting wave as AP-009, with no LOC double-counting between the two rows. CH5-004 V1 binding fold is correctly carried into the V2 anti-pattern surface. | `1F-anti-pattern.md:80` (AP-020 detail); `:105` (V2 Planning Metadata); AP-009 companion at `:94`. |
| ACCEPT | The V2 1D row 113 amendment preserves the SK-V14 R4 regen-css xtask wave-alignment hint as a cross-cutting discipline across four sibling rows (`:122`, `:131`, `:133`, `:134`) and one sequencing-only row (`:149`), with the verify-before-rederive obligation routed through PC-008 + U-PC-002 as a non-LOC-bearing audit obligation. The sequencing-only cost class (`0 LOC (sequencing only)` / `HIGH risk` / `n/a hard cap`) is correctly framed per the V1 CH4 distinction. | `1D-skinny-lessons.md:122, 131, 133, 134, 148-149`; `1F-past-corpora.md:74, 158`. |
| ACCEPT | The V2 LAC-1E-12 promotion to "candidate-promoted-to-T-P3-§3C-priority" introduces zero new LOC obligation — the V1 `60-180 LOC docs / low risk / 240 LOC hard cap` frame is preserved verbatim. Promotion sharpens wave-alignment (T-P3 §3C priority) and reinforces binding-surface authority via the COH-012 meta-CH7 collision cross-cite, without changing the cost class. The V2 dispatch-context rule "T-P1 PROPOSES; T-P3 disposes; Pass Omega merges" is satisfied. | `1E-locks-evidence.md:120` (LAC-1E-12 row); `:126-128` (§1.5 promotion explainer); V1 baseline `restart/audit/totality/p1/hardening/V1/CH4.md:65` (LAC-1E-12 V1 row). |
| ACCEPT | The V2 google_sheets=10 propagation reaches all relevant rows: AP-016 at `1F-anti-pattern.md:101` reads `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 = 67` (1500-3000 LOC PRUNE-4 / 4000 LOC cap); PC-017 at `1F-past-corpora.md:120` carries the identical census; COH-011 at `1F-coherence-scan.md:92` matches. The 67-file Pattern H census is now uniformly anchored at HEAD via reproducible `find crates/core/src/runtime/google_sheets -type f \| wc -l` (returns 10 at 2026-05-23). | `1F-anti-pattern.md:101`; `1F-past-corpora.md:120`; `1F-coherence-scan.md:92`; HEAD command output 10. |
| ACCEPT | The V1 CH4 §6 cross-artefact Pattern H LOC-budget congruence note (LAC-1E-15 4000-8000 vs. AP-016 1500-3000 vs. PC-017 1500-3000 vs. 1C-D1 10,915 vs. 1D Pattern H envelope 2800-3400) survives V2 amendment with the V1 framing recommendation preserved: T-P3 §3C should adopt LAC-1E-15 per-tranche framing as the load-bearing Pattern H budget. AP-016 / PC-017 google_sheets=10 propagation does not collapse the multi-framing spread; per-tranche census vs. per-wave PRUNE-4 vs. C-1 envelope vs. full revival cost remain three distinct artefact-purpose-correct framings. | `1E-locks-evidence.md:123` (LAC-1E-15); `1F-anti-pattern.md:101` (AP-016); `1F-past-corpora.md:120` (PC-017); `1C-runtime-evidence.md:158` (1C-D1); `1D-skinny-lessons.md:144` (Pattern H envelope). |
| ACCEPT | The V1 CH4 §6 hard-cap multiplier convention (1.2-1.4× upper bound) survives V2 amendment. V2-amended rows representative spot-check: AP-020 160 / 120 = 1.33×; 1B-D8 600 / 500 = 1.20×; 1B-D10 650 / 500 = 1.30×; 1A-DIV-008 1,100 / 900 = 1.22×; LAC-1E-12 240 / 180 = 1.33×; LAC-1E-15 11,000 / 8,000 = 1.375×. All within convention. CH4 reaffirms the V1 non-blocking recommendation that T-P3 §3C codify the multiplier convention in Lock 8 V+1 wording. | Per-row hard cap citations in §3 + §5 tables. |
| ACCEPT | The V2 1F schema-indirection defect flagged by the prior V2 CH4 cycle (REVISE: 1F divergence tables join to V2 Planning Metadata by ID rather than carrying schema inline) is resolved at the V2 micro-fold inventory level — each 1F file now carries the explicit "V2 Planning Metadata (authoritative CH4 carrier)" header convention (`1F-anti-pattern.md:82`, `1F-coherence-scan.md:95`, `1F-past-corpora.md:122`) plus an explicit note that the structural index table is non-authoritative (`1F-past-corpora.md:102`). The V1 CH4 8/8 ACCEPT discipline is restored at V2. | `1F-anti-pattern.md:82`; `1F-coherence-scan.md:95`; `1F-past-corpora.md:102, 122`. |

## §7 New Finding (Lens-Local, V2 cycle)

| Note | Detail |
|---|---|
| CH4 V2 sequencing-only cost class is uniformly respected | The V2 micro-fold introduces several rows that are sequencing-only (`0 LOC (sequencing only)`), notably 1D row at `:149` (R4 → PRUNE-2), 1D row at `:150` (C-1 → C-4), 1D row at `:151` (PRUNE-4 = 9 sub-waves), and the 1D Track 2 substrate-helper caveat at `:157` (`0 LOC (taxonomy clarification)`). Each correctly carries `HIGH risk` (or `low risk` for taxonomy-only rows) with `n/a` or `bundled` hard cap. CH4 explicitly accepts this cost class — the cost is the cost of respecting a sequencing constraint or taxonomy clarification, not implementation cost. No revise is triggered. The V1 CH4 distinction (per `restart/audit/totality/p1/hardening/V1/CH4.md:79` Findings row 3) is preserved verbatim. |
| CH4 V2 V1-hardening cross-cite discipline | V2-amended rows that depend on V1 hardening artefacts (e.g., 1A-DIV-008 cites SK-V14 S-P1 CH5 V3 binding; 1D row 100 cites V1-CONSOLIDATED §1.2; 1D row 113 cites V2 fold per V1-CONSOLIDATED §1.4; 1D row 157 cites `restart/audit/totality/p1/hardening/V1/CH5.md` CH5-005) uniformly carry the V1 hardening cite as fold-provenance rather than substantive evidence — the CH1 V2 finding CH1-V2-005 discipline (primary evidence preferred over hardening-file evidence for substantive rows) is respected in the V2 micro-fold cost framing. No CH4-side regression. |

## §8 Required Revisions

None. All eight V2-amended artefacts pass CH4 ACCEPT under the orchestrator
six-field schema, the V2 dispatch-context five-convergence-point focus, the
V1 CH4 16/16 LAC wave-alignment + path:line discipline, and the V1 CH4
1.2-1.4× hard-cap convention. The V2 micro-fold introduces zero CH4-side
regression and zero new LOC obligation beyond the explicit V2-amended
divergence rows.

## §9 Cycle Disposition

ACCEPT. 8/8 V2-amended artefacts pass. 16/16 LACs pass dispatch-required
wave-alignment + path:line check at V2 HEAD (commit `87816a2cd`). All five
V2 dispatch-context convergence points verify. Hard-cap and
same-wave-consumer convention is uniformly populated; google_sheets=10
propagation is uniform; 1C ~50→~190 LOC repair is correctly composed; D8/D10
split LOC delta is correct; AP-020 LOC band conforms to multiplier
convention; 1D row 113 preserves R4 wave-alignment hint; LAC-1E-12 promotion
adds zero new LOC obligation. §3Z gate: ≥95% cycle reached on V1 (100%) and
reaffirmed on V2 (100%); §3Z standalone-closed at V2. Predicted V2 → LOCK
trajectory; no V3 CH4 dispatch required.

## §10 Aggregator Note

CH4 V2 disposition: ACCEPT. 8/8 V2-amended artefacts. 16/16 LACs. 5/5 V2
dispatch-context convergence points. Carry-forward V1 non-blocking
governance recommendations to T-P3 §3C: (i) adopt LAC-1E-15 per-tranche
framing as the load-bearing Pattern H budget; (ii) codify the 1.2-1.4×
hard-cap multiplier convention in Lock 8 V+1 wording. LAC-1E-12 promotion
to "candidate-promoted-to-T-P3-§3C-priority" is a wave-alignment sharpening
without cost-class change; T-P3 §3C disposes whether Lock 17/18 numbering or
in-preface CH7-binding clause is the carrier (both routes cost-bounded by
the same 60-180 LOC docs envelope).
