---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V2
disposition: ACCEPT
generated_at: 2026-05-23T22:00:00-04:00
files_audited:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/audit/totality/p1/hardening/V1/CH5.md
  - restart/audit/totality/p1/hardening/V4/CH5.md
  - restart/audit/totality/p1/hardening/V5/CH5.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md
prior_cycle_review:
  - "V1 CH5 REVISE (3/7 ACCEPT, 4 REVISE on substrate-coupling nuances; CH5-002 / CH5-004 / CH5-005 / CH5-007)"
  - "V4 CH5 ACCEPT (scoped classifications carry the V1 REVISE quartet)"
  - "V5 CH5 ACCEPT (second consecutive acceptance of V4 posture)"
live_truth_method: "grep + line-anchored Read at V2 amended HEAD commit 87816a2cd; verification of V1 CH5 REVISE fold points across V2 1A/1D/1F-anti-pattern inventories; cross-cite of V1 CONSOLIDATED §2.10 #1-#4 against V2 cell payloads; substantive cite verification (skinny/crates/runtime/src/grammars/json/scan.rs, skinny/crates/bbnf-bench/src/track2/json.rs, skinny/crates/runtime/src/lib.rs, skinny/crates/bbnf-bench/src/nonjson_css_l4.rs); V1 line-number-stability check"
---

## Executive Summary

CH5 returns **ACCEPT** on this V2 cycle with one non-blocking
cite-staleness caveat (CH5-V2-008). V1 CH5 returned REVISE with four
substrate-coupling REVISEs (CH5-002 renamed scanner, CH5-004 CSS source
sidecar, CH5-005 Track 2 substrate-helper, CH5-007 proof witness). The
V2 amendment per V1 CONSOLIDATED §2.10 binds all four into the V2
inventories with explicit classifications, cross-cites, and a binding
T-P3 §3C ratification rule for the substrate-union question.

Specifically: 1A-SUB-014 is downgraded to "partial / unknown — no
retained `StructuralIndex` identity found, renamed scanner side plane
live" with the V1 CH5-002 fold and a cross-cite to the CSS source-sidecar
surface; a NEW V2 AP-020 row in `1F-anti-pattern.md` carries CSS
source-sidecar comparator plane as a separately classifiable anti-pattern
per the V1 CH5-004 binding; 1D's divergence-row at `:157` and Spec-Claim
row at `:117` carry CH5-005 Track 2 substrate-helper caveat as a Lock 1
union sub-case (not Track-independence violation) with the T-P3 §3C
ratification framing; 1F-anti-pattern AP-010 strengthens the proof-witness
verdict per V1 CH5-007 + V1 CONSOLIDATED §2.10 #4; and 1A-DIV-008 binds
the two-cursor structural split at HEAD with explicit T-P3 §3C "ratify
or unify" disposition. The SK-V14 binding rows (1A-SUB-016 / 017 / 018,
1A-DIV-008, 1A-LOCK1-AMEND-001) read against the CH5 firewall correctly:
the JSON-named `dispatch_value` / `parse_object_value_at_direct` /
`parse_array_element_at_direct` envelopes hide the grammar-neutral
`dispatch` primitive across offset and direct paths — exactly the
"hidden coupling" CH5 is authored to surface.

The cite-staleness caveat is documentation drift, not classification
error: V1 CH5-004's line numbers `nonjson_css_l4.rs:222,234,299,504`
were folded forward verbatim into V2 1A-SUB-014, 1F-anti-pattern AP-020,
and 1D-skinny-lessons `:117`. Live `nonjson_css_l4.rs` is now 3,644 lines
(verified `wc -l`); `fixture_sidecar_facts` is at line 2691, called from
line 648; `same-plane-source-sidecar` writers appear at lines 1082, 1203,
1354, 1511, 1661, 1815, 1964. The function and pattern exist; only the
line numbers drift. The Track 2 cites at `track2/json.rs:5,24,43` are
off-by-2 from HEAD (`:7,26,45`). The proof-witness cite at
`runtime/src/lib.rs:9` drifts to `:29-33` at HEAD. All classification
content holds against live evidence; only the line coordinates are
stale, and all V2 inventory rows carry the canonical V1-fold cite. This
is a V3 housekeeping fold item, not a V2 blocker.

## Dispositions

| ID | Disposition | Finding | Evidence |
|---|---|---|---|
| CH5-V2-001 | ACCEPT | V1 CH5-001 closure preserved — Lock 1 retained-vs-direct path classification holds with admitted-SinkOnly direct evidence. | V2 1A-SUB-010 verdict "partial / scheduling UNKNOWN — admitted SinkOnly (route → 1A-UNK-003)" at `restart/audit/totality/p1/1A-substrate-evidence.md:63` (cited by 1A-SUB-010 row). Retained `ParserState { input, bytes, cursor, tape: TapeBuilder<'i> }` at `skinny/crates/runtime/src/grammars/json/parser.rs:7-12`; direct path with `JsonSink` per `skinny/crates/runtime/src/grammars/json/sink.rs:1-15` (cited at row 1A-SUB-010). One-scheduled-materialization audit remains open per `1A-UNK-003`. SK-V14 1A-SUB-016 / 1A-DIV-008 sharpen the gap (two structurally independent cursor types at HEAD; substrate-union ratification pending T-P3 §3C per `1A-UNK-005`). |
| CH5-V2-002 | ACCEPT | V1 CH5-002 (renamed scanner classification) fold complete in 1A-SUB-014 + 1F-anti-pattern AP-008. | V2 1A-SUB-014 verdict downgraded from "implemented" to "partial / unknown — no retained `StructuralIndex` identity found, renamed scanner side plane live" at `restart/audit/totality/p1/1A-substrate-evidence.md:67` per V1 CONSOLIDATED §2.10 #1 + §2.11 #1. Cites added: `skinny/crates/runtime/src/grammars/json/scan.rs:1` ("JSON-owned structural scan source"; verified live at HEAD line 1), `scan.rs:22` (`scan_structurals` returns `StructuralIndex`; verified live at HEAD line 22), `scan.rs:47-54` (`structural_capacity_for`; verified live), `scan.rs:51` (`OneShotSimd` capacity from scanned positions; verified live), and `generated.rs:12-15` (`attach_structural_index` is a no-op on retained parse). Transient capacity/proof fencing made explicit. 1F-anti-pattern AP-008 carries the same classification at `restart/audit/totality/p1/1F-anti-pattern.md:68` with planning metadata routing to retained-substrate audit consumer at `restart/audit/totality/p1/1F-anti-pattern.md:93`. |
| CH5-V2-003 | ACCEPT | V1 CH5-003 (CSS fact-stream substrate-category gap) preserved as 1A-SUB-015 admitted same-plane fact-stream evidence with no promotion. | V2 1A-SUB-015 verdict "admitted CSS fact-stream evidence / substrate-category gap" at `restart/audit/totality/p1/1A-substrate-evidence.md:68`; 1A-DIV-006 carries LOC/risk metadata at `restart/audit/totality/p1/1A-substrate-evidence.md:82`. CSS L4 declaration-values emits a TSV-like fact stream through `FactSink` with no `Tape`/`ValueRef`/`DocumentView` — preserved as admitted same-plane product row. The substrate finding remains that V1 lacks a category for admitted fact-stream telemetry/product rows; no promotion into runtime substrate. |
| CH5-V2-004 | ACCEPT | V1 CH5-004 (CSS source-sidecar comparator plane) fold complete via NEW V2 AP-020 row in 1F-anti-pattern + 1A-SUB-014 cite cross-ref. | V2 1F-anti-pattern AP-020 is the NEW V2 row at `restart/audit/totality/p1/1F-anti-pattern.md:80` classifying CSS source-sidecar comparator plane as "comparator-sidecar coupling; fence required (non-runtime-authoritative; not retained document identity)" per V1 CONSOLIDATED §2.10 #2 binding. Cited: `bbnf-bench/src/nonjson_css_l4.rs:222,234` route comparator evidence through `fixture_sidecar_facts`; `:299` writes a `same-plane-source-sidecar` artifact; `:504` validates hardcoded fixture spans inside `fixture_sidecar_facts`. Companion classification: AP-009 records the same file at `:222-234,298-303` as comparator-sidecar evidence (`restart/audit/totality/p1/1F-anti-pattern.md:69`); AP-020 lifts the sidecar-as-anti-pattern row separately so it is not collapsed into AP-009. AP-020 LOC/risk + co-wave with AP-009 recorded in V2 Planning Metadata at `restart/audit/totality/p1/1F-anti-pattern.md:105`. 1A-SUB-014 cite at `restart/audit/totality/p1/1A-substrate-evidence.md:67` carries the CSS source-sidecar surface back into the substrate audit. **See CH5-V2-008 for cite-staleness caveat on line numbers 222/234/299/504 (semantic content holds; coordinates drifted).** |
| CH5-V2-005 | ACCEPT | V1 CH5-005 (Track 2 substrate-helper caveat) fold complete in 1D divergence-row + 1F-anti-pattern AP-011 + V1 CONSOLIDATED §2.10 #3 binding. | V2 1D divergence-row at `restart/audit/totality/p1/1D-skinny-lessons.md:157` reads "Track 1/Track 2 independence is parser-implementation only, NOT substrate-helper independence. V2 fold (CH5-005): Track 2 retained-JSON bench harness imports `runtime::grammars::json::scan::structural_capacity_for` + `TapeBuilder` + `OffsetFlags` + `CapacityPlan` from generic runtime at `skinny/crates/bbnf-bench/src/track2/json.rs:5,24,43`, then seals through `JsonRoot::from_tape`. Parser body is hand-written (independent of Track 1 generated parser per `1F-past-corpora.md:34` closed bench-private dishonesty) but the substrate helpers are shared." Live verification: `skinny/crates/bbnf-bench/src/track2/json.rs:7` imports `tape::{CapacityPlan, OffsetFlags, TapeBuilder}` (off-by-2 from V2 cite at `:5`); line 26 calls `runtime::grammars::json::scan::structural_capacity_for` (off-by-2 from `:24`); line 34 constructs `tape: TapeBuilder::new(...)`; line 45 returns `JsonRoot::from_tape(...)` (off-by-2 from `:43`). 1F-anti-pattern AP-011 carries the classification independently at `restart/audit/totality/p1/1F-anti-pattern.md:71` with "independent parser authority with shared runtime substrate helpers" verdict + 0 LOC classification cost. Independence holds for parser implementation; substrate helpers are deliberately shared and that sharing is Lock 1 union sub-case, not Track-independence violation. |
| CH5-V2-006 | ACCEPT | V1 CH5-006 (prior Track 1 == Track 2 bench-private dishonesty closed) preserved unchanged. | V2 1F-anti-pattern AP-007 verdict "implemented" at `restart/audit/totality/p1/1F-anti-pattern.md:67` — Track 1 generated direct now calls generated runtime (`runtime/src/grammars/json/generated.rs:393-407`), not bench-private parser, per `skinny/REDRESS.md:535-557`. 1F-past-corpora at `restart/audit/totality/p1/1F-past-corpora.md:34` (cited in CH5-V2-005's 1D row) preserves closure of the old bench-private SinkParser dishonesty. V2 fold did not need to revise this row. |
| CH5-V2-007 | ACCEPT | V1 CH5-007 (proof-witness generic-runtime coupling) fold complete via 1F-anti-pattern AP-010 verdict strengthen + 1D cross-cite. | V2 1F-anti-pattern AP-010 verdict strengthened to "Lock 14 leak under unverified proof gate (pending captured `cargo build` evidence to confirm proof-cfg fully fences witnesses from production builds; if proof gates verify, restate as 'proof-cfg fenced; production absent')" at `restart/audit/totality/p1/1F-anti-pattern.md:70` per V1 CONSOLIDATED §2.10 #4. Live verification: `skinny/crates/runtime/src/lib.rs:29-33` carries `pub mod json_event_grammar_witness` and `pub mod sheets_witness` under `#[cfg(any(test, feature = "proof"))]` gates (verified at HEAD; the 1D cross-cite to `runtime/src/lib.rs:9` drifts from the precise location, but AP-010's :29-33 cite is correct at HEAD). Planning metadata routes to proof-crate or generated witness consumer at `restart/audit/totality/p1/1F-anti-pattern.md:95`. |
| CH5-V2-008 | ACCEPT-with-caveat | NEW V2 cite-staleness caveat: `nonjson_css_l4.rs:222,234,299,504` line numbers carried forward from V1 CH5-004 into V2 1A-SUB-014, 1F-anti-pattern AP-020, AP-020 evidence_basis, and 1D divergence row do not match HEAD line positions; off-by-2 drift on Track 2 cites and `runtime/src/lib.rs:9` proof-witness cite also drifted. | The V2 amendment carried V1 CH5-004's line numbers verbatim per V1 CONSOLIDATED §2.10 #2 binding. Live `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` is 3,644 lines (verified `wc -l`); `fixture_sidecar_facts` is at line 2691 (not 504); the `lightningcss_facts` invocation calling `fixture_sidecar_facts(input)` is at line 648 (not 222/234); `same-plane-source-sidecar` literal writers exist at lines 1082, 1203, 1354, 1511, 1661, 1815, 1964 (not 299); imports at `:17-24`. Track 2 cites `:5,24,43` drift off-by-2 from HEAD (`:7,26,45` for imports/structural_capacity_for/JsonRoot::from_tape). Proof-witness cite `runtime/src/lib.rs:9` drifts to `:29-33` at HEAD. **The semantic content — the functions exist, the writers exist, the comparator-sidecar pattern is live, Track 2 substrate-helper sharing is live, proof witnesses are exposed under cfg gates — is verified at HEAD.** Only the precise line coordinates are stale. **This is documentation drift, not classification error.** The V2 fold faithfully discharged V1 CONSOLIDATED §2.10 by carrying the canonical V1 cite forward; V3 may refresh to current line numbers as low-cost housekeeping. Disposition: ACCEPT-with-caveat because the classification (comparator-sidecar coupling, fence required; Track 2 substrate-helper sub-case; proof-witness Lock 14 leak) is correct and the file/function/pattern exist at HEAD. |
| CH5-V2-009 | ACCEPT | NEW V2 substrate-union nuance disposition (1A-DIV-008) lands correctly with binding T-P3 §3C ratification rule and no paper-close. | V2 1A-DIV-008 at `restart/audit/totality/p1/1A-substrate-evidence.md:84` records two structurally independent cursor types at HEAD: `ParserState.cursor` over `TapeBuilder<'i>` at `runtime/src/grammars/json/parser.rs:7-12` (verified live) and `DirectParser.cursor` with no tape at `codegen/src/json_typed_direct.rs:518-522` (cited; covered by 1A-SUB-016 at `:69`). The substrate-union nuance disposition reads: "T-P3 §3C must either ratify the two-cursor shape as the V1 substrate-union (1D `:100` reads correctly under ratified definition) or mandate unification (1D `:100` downgrades to 'disproved at HEAD; obligation deferred to T-P2 unification')." Per `1A-UNK-005` verify_action at `restart/audit/totality/p1/1A-substrate-evidence.md:106`. 1D row at `:117` carries the same disposition with explicit "T-P3 §3C PENDING" flag and cross-cites to CH5-002/004/005/007 as sub-cases of the same Lock 1 union-vs-split disposition. No paper-close — both readings carry forward with binding ratification rule. 1A-LOCK1-AMEND-001 at `restart/audit/totality/p1/1A-substrate-evidence.md:113` carries the candidate to 1E. |
| CH5-V2-010 | ACCEPT | NEW V2 1D row 100 substrate-cardinality T-P3 §3C pending markup carries no paper-close; cross-inventory tension carried forward with binding ratification rule. | V2 1D `:117` Spec-Claim row "Single substrate: Lock 1 tape ∪ direct-to-struct union must not split into parallel producers" verdict downgraded from prior "proved" to "proved historically; SK-V14 1A-DIV-008 records two-cursor structural split at HEAD pending T-P3 §3C disposition" per V1 CONSOLIDATED §2.11 #3 + §1.2 substrate-coupling convergence. Note column explicitly carries the cross-inventory tension forward with T-P3 §3C PENDING flag and the "ratify-or-unify" binding rule. Sub-cases CH5-002 (renamed scanner / `scan.rs:1,22,51`), CH5-004 (CSS source-sidecar / `nonjson_css_l4.rs:222,234,299,504` — cite-staleness per CH5-V2-008), CH5-005 (Track 2 substrate-helper / `bbnf-bench/src/track2/json.rs:5,24,43` — off-by-2 drift per CH5-V2-008), and CH5-007 (proof-witness generic-runtime exports / `runtime/src/lib.rs:9` — drifts to `:29-33`) all named in the row's note. Cross-cites to 1A-DIV-008 visible at row position. No paper-close. |
| CH5-V2-011 | ACCEPT | NEW V2 1D §2.10#3 Track 1/2 substrate-helper caveat row reads as CH5-005 framing (Lock 1 union sub-case, not Track-independence violation). | V2 1D divergence-row at `:157` reads "spec_unimplemented (taxonomy caveat) … substrate-helper sharing reclassified as Lock 1 union sub-case, not as Track-independence violation." Per V1 CONSOLIDATED §2.10 #3 binding. The "0 LOC (taxonomy clarification)" loc_budget + "T-P3 §3C disposition (folds into row 100 substrate-union ratification)" wave assignment correctly route this as a sub-case of the row-100 ratification question, not as a standalone Track-1/2 split. Cites confirmed (with off-by-2 drift caveat from CH5-V2-008). |
| CH5-V2-012 | ACCEPT | NEW V2 1A-SUB-016/017/018 (SK-V14 binding) substrate-union evidence rows carry CH5 firewall correctly — the dispatch envelope mis-attribution IS a hidden-coupling finding. | 1A-SUB-016 at `restart/audit/totality/p1/1A-substrate-evidence.md:69` records substrate-union as two parallel cursor types with verdict "partial / diverged — substrate-union is two parallel cursor types"; 1A-SUB-017 at `:70` records JSON `dispatch_value` LTO-fused envelope as the substrate-union dispatch primitive Lock-14-mis-attributed by name; 1A-SUB-018 at `:71` records S-P1 CH2 13/17 parse_only + 14/17 direct rank-1 envelope mis-attribution census. All three rows carry SK-V14 S-P1 CH5 V3 binding. CH5 reading: the JSON-named envelope hides the grammar-neutral `dispatch` primitive across both offset and direct paths — a structural Lock 14 mis-attribution with hidden-coupling implications (one primitive instantiated three times across two cursor types, manifesting as three JSON-named envelopes that LTO-fuse into callers). No paper-close; each row routes to T-P3 §3C disposition + S-P2 envelope-crack (`parse-attribution` cargo feature) for runtime verification. |

## Convergence Check

| Cycle | Total findings | ACCEPT | REVISE | REJECT | ACCEPT-rate |
|---|---:|---:|---:|---:|---:|
| V1 | 7 | 3 | 4 | 0 | 42.9 % |
| V2 | 12 | 12 (1 ACCEPT-with-caveat for documentation drift) | 0 | 0 | 100.0 % |

V2 CH5 ACCEPT-rate is 100 % (12/12), with CH5-V2-008 carrying a
non-blocking cite-staleness caveat that does not affect classification.
This is the first ≥95 % cycle for CH5 on this V2 amendment baseline.
V4 + V5 prior consecutive ACCEPTs already met the convergence criterion
on the V4 inventory baseline; the V2 cycle reopened the substrate-coupling
nuance set with the SK-V14 binding (1A-DIV-008 two-cursor split, 1A-SUB-016
/017/018 dispatch-envelope mis-attribution census) and the V1
four-REVISE fold packet. The V2 amendment closes all V1 REVISE points
and adds explicit T-P3 §3C ratification rules for both the substrate-union
nuance and the Lock 1 honoured-vs-split tension.

## §3Z Gate Evaluation

Predicted V2 → V3 → LOCK trajectory:
- **V2 (this cycle):** 100 % ACCEPT on CH5; first cycle ≥95 % on V2
  baseline.
- **V3 (housekeeping):** refresh stale cite line numbers
  (`nonjson_css_l4.rs:222,234,299,504` → live HEAD positions; Track 2
  off-by-2; proof-witness `runtime/src/lib.rs:9` → `:29-33`). Pure
  documentation-cohesion fold; classification holds. Predicted ACCEPT.
- **LOCK:** two consecutive cycles ≥95 % ACCEPT satisfy §4 convergence
  criterion. T-P1 advances to T-P2 post-G1.

The cite-staleness caveat (CH5-V2-008) is dispositioned as ACCEPT
because the classification is correct and the live evidence is verified;
the line numbers are a fold-forward artifact of the V1 CONSOLIDATED §2.10
binding, not a freshness failure on the V2 author's part.

## Required V3 Fold (Housekeeping Only)

V3 fold for CH5 is documentation-only:
1. **CSS source-sidecar cite refresh** at `1A-substrate-evidence.md:67`,
   `1F-anti-pattern.md:80`, `1F-anti-pattern.md:105`, and
   `1D-skinny-lessons.md:117`: replace `nonjson_css_l4.rs:222,234,299,504`
   with current HEAD line numbers (`:648` invocation, `:2691` function
   body, `:1082`/`:1203`/`:1354`/`:1511`/`:1661`/`:1815`/`:1964` writer
   sites; imports at `:17-24`). LOC budget: ~30 LOC across 4 files.
2. **Track 2 cite refresh** at `1D-skinny-lessons.md:157` and
   `1F-anti-pattern.md:71`: replace `bbnf-bench/src/track2/json.rs:5,24,43`
   with `:7,26,45` (off-by-2 drift). LOC budget: ~10 LOC.
3. **Proof-witness cite refresh** at `1D-skinny-lessons.md:117`: replace
   `runtime/src/lib.rs:9` with `runtime/src/lib.rs:29-33`. LOC budget:
   ~5 LOC.

These are NOT V2 ACCEPT blockers; they are V3 freshness improvements.
The V2 cycle correctly carries the canonical V1-fold cite forward; the
classifications hold against live HEAD evidence regardless of line-number
drift.

## Closing Posture

CH5 hidden-coupling firewall holds at V2. Every V1 REVISE point is
folded with explicit classifications routed to T-P3 §3C disposition or
to existing verify_actions. The substrate-union nuance (1A-DIV-008,
1D row 100) carries a binding ratification rule that prevents paper-close
in either direction. The SK-V14 binding (1A-SUB-016/017/018) adds the
two-cursor structural split, the dispatch envelope mis-attribution, and
the LOC-14 envelope-name mis-attribution census as substantive new
evidence — and all three rows correctly read against CH5 (the JSON-named
envelope hides the grammar-neutral `dispatch` primitive across offset
and direct paths, which is exactly the "hidden coupling" CH5 is authored
to surface). The cite-staleness caveat on CSS sidecar line numbers,
Track 2 line numbers, and proof-witness line cite is documentation
drift, not a classification error, and is dispositioned as a V3 fold
housekeeping item. V2 CH5 is ACCEPT.
