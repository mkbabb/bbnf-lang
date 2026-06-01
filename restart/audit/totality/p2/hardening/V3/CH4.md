# CH4 COST — SK-V18 T-P2 CHALLENGE (cycle V3)

Lens name: CH4 COST. Every grounded primitive must carry an admission cost —
scalar reference + checkasm parity per Lock 16, a same-wave consumer named,
realistic LOC/risk, no orphan-kernel research. Confabulated/unverifiable
citations and refuted-route groundings are inadmissible.

Disposition: `REVISE`.

Target packet (regenerated 2026-06-01 for SK-V18, commit unchanged from V2):
`2A-sota-landscape.md` (cycle SK-V18-T-P2), `2B-primitive-vocabulary.md`
(V3-SKV18), `2C-grammar-neutrality.md` (V3), `2D-cost-model.md` (V3-SKV18),
`2E-host-arch-esoterica.md` (V6-SKV18), `2F-parse-that-gaps.md` (V3).

The cost-field schema is `T-P2-V2-FOLD-ADDENDUM.md:27-64` (transfer_reason /
admission_gate / verification_action / close_status / loc_estimate / risk_class /
wave_owner / hard_cap_fit; plus SIMD-row scalar_reference / parity_or_checkasm /
hardware_gate / same_wave_consumer / row_movement_target). The live wave-band
authority is SPEC §8 `restart/skinny/tranches/sk-v18/SPEC.md:436-444`
(G1/G2/G3/G4/G5-G6 each `≤450 hand source/test/gate LOC`, `≤90 min wave wall`,
`30/45 min/redress`).

## V2 fold verification (the three CH4-V2 obligations)

All three V2 CH4 REVISE folds LANDED and are correct:

- **CH4-V2-01 (2B, 2C in-cell retired `W#` owners)** — FOLDED. 2B's V2 grounding
  table now sits under `## Technique Grounding Table — SK-V15 HISTORICAL
  (non-SK-V18-cost)` (`2B:54`) with an explicit in-section disclaimer (`2B:60-64`:
  "Do NOT copy a `W#` owner from a row here as a live SK-V18 cost"), the A3a
  manifest likewise (`2B:151-156`), and the SK-V18 Cost Manifest (`2B:401-417`)
  re-keys every SK-V18-scope row to the LIVE §8 ledger. 2C carries the same
  HISTORICAL headers on both its grounding table (`2C:56-61`) and its LAC table
  (`2C:148-153`), plus a dedicated `## V3 Wave-Owner Re-key` paragraph
  (`2C:319-334`). No live SK-V18 cost cell carries a retired `W#` owner.
- **CH4-V2-02 (2D CollapsedStage inert-slot vs conditional split)** — FOLDED.
  LAC-2D-V3-04 (`2D:98`) now carries `Manifest (cost SPLIT
  committed-vs-conditional): (a) COMMITTED inert-slot cost = ≈0 LOC,
  wave_owner=none … (b) CONDITIONAL rebuild cost = ≤450 hand LOC … wave_owner=G5/G6
  GATED on a profiled CollapsedStage hot leaf … NOT a committed G5/G6 build`. The
  ≤450 is no longer budgeted against the inert slot.
- **CH4-V2-03 (2A sonic-rs conjectural-consumer over-budget)** — FOLDED.
  T2A-V1-SOTA-JSON-003 (`2A:56`) is re-keyed to `wave_owner=SK-V19-receiver (no
  LIVE SK-V18 consumer)`, `close_status=blocked`, `loc_estimate=150-350 per leaf,
  CONDITIONAL-on-a-profiled-CSS-string/number-leaf-hot (not a committed SK-V18
  budget)`, `hard_cap_fit=n/a in SK-V18`. The live SK-V18 budget is no longer
  charged a leaf with no SK-V18 consumer.

## Citation spot-verification (load-bearing, this lens)

Every load-bearing cost grounding was confirmed against a primary source or the
on-disk tree.

External (WebFetch/WebSearch):

- **iburg** (2D R-A cost-model spine): Fraser/Hanson/Proebsting, "Engineering a
  Simple, Efficient Code-Generator Generator", ACM LOPLAS 1(3) 1992, pp.213-226,
  DOI 10.1145/151640.151642 — VERIFIED exact (ACM DL + drh/iburg + Arizona pure).
  Real. Dynamic-programming compile-time tree-matcher = the dispatch-on-selected-
  pattern claim 2D grounds R-A on. Honest.
- **Lemire-2026 ARM match** (2E `MatchSetSve2` REFUTE + 2B/2A eq-fan framing):
  `lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/`
  — VERIFIED LIVE. Exact title/date confirmed; "SVE2 match … fastest" (SVE 16.0
  vs NEON 15.5 GB/s, 25% fewer instrs) matches the dossier framing and correctly
  REFUTES NEON-svmatch on the SVE2-absent host. CAVEAT (see CH4-V3-03): the
  post's BODY uses the Langdale/Lemire TABLE-driven (shuffle/TBL) classifier as
  the conventional NEON route; the `vceqq_u8` eq-fan appears in a COMMENTER
  suggestion, not the author's text. 2E `:71` discloses this accurately ("…the
  `vceqq_u8` eq-fan (comments)"); the 2E/2B exec-summary framing is looser.

Local (on-disk, this lens):

- **`find_css_significant` salvage kernel** (drives the `≤150 LOC` retarget
  estimates in 2B/2E/2F): `runtime_simd.rs:169-216` — VERIFIED exact: two-fan
  `set_a[8]=fixed[..8]` + `set_b=fixed[8]⧺delimiters`, `byte_class_from_eq_set_64(.,set_a)
  | byte_class_from_eq_set_64(.,set_b)` OR-reduce at `:199`, `trailing_zeros`,
  scalar tail. Dead-caller is `#[cfg(test)]`-only at `lib.rs:574`. The salvage is
  real → the low LOC estimates are credible.
- **`count_top_level_commas` live CSS consumer** (the same-wave-consumer cell for
  the eq-set/bracket kernels): `runtime_simd.rs:29,44,47,56` — VERIFIED consuming
  `byte_class_from_eq_set_64` + `bracket_depth_mask_64`. The "CSS-only by live
  consumer, structurally neutral by caller-data" disclosure is accurate.
- **SHRN movemask divergence** (2E `Movemask64Shrn`, the `≈10 LOC` LOW-risk
  kernel-internal swap): `movemask.rs:5` uses canonical `vshrn_n_u16::<4>` (2
  instr); `byte_class_from_eq_set_64.rs:79-87` carries a DUPLICATE local
  `movemask_u8x16` on the slow shift-add `vaddv_u8` path. VERIFIED — the
  divergence is real, the swap is genuinely a single-site kernel-internal change.
- **REDRESS production precedent + cautionary priors** (the cost/risk grounding
  in 2B/2E/2F): REDRESS 144 `:4418-4438` `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT,
  Track 1 `444.208` vs `434.1316` Mbps, `+109.87%`, `byte_class_from_eq_set_64`
  wired into CSS `Scanner::scan_block` — VERIFIED exact. REDRESS 126 `:3766-3805`
  `ROUTE-PRODUCTION-SPLIT` (microbench `4.538843×` is NOT a row move) — VERIFIED.
  REDRESS 96/97/98 `:2926-2935` `G-W3-UNION-SUBSTRATE` retired (M5 Max scalar
  cheaper than a streamed SIMD cursor) — VERIFIED. The ledger framing is honest.
- **2D DERIVED-not-new engine** (the `≈0 LOC` LAC-2D-V3-03 regression-guard cost
  and the G3 `≤450 LOC` un-fork cost): `backend_egraph.rs:191/193`
  `NormalizeDirectSinkCost` struct+impl live, instantiated `:75`,
  `BackoffScheduler` `:73`, `DecisionCostModel` `:259`, `Extractor::new` `:84` —
  VERIFIED the engine EXISTS, so "DERIVED-not-new / ≈0 LOC guard" is honest.
  `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork live at
  `grammar_provider.rs:40-42` — the R-A DELETE target is real, so the G3 ≤450 LOC
  is a genuine un-fork cost. `runtime_target_rows_collapsed` grep == 0 — confirms
  the PLANNED-not-live disclosure (no asserted-live overclaim).
- **Host probe** (drives the svmatch REFUTE row's `n/a` cost): `Apple M5 Max`,
  `FEAT_SVE2` ABSENT (unknown oid), `FEAT_SME2=1`, `FEAT_PMULL=1` — VERIFIED.

No confabulated or unverifiable citation found. No refuted-route grounding. No
SIMD row admits from citation alone; every Section-B/candidate row in 2E carries
`same_wave_consumer=NONE` + `abrogate=DO NOT author`.

## Critical Findings

| id | severity | dossier | finding | required action | close |
|---|---|---|---|---|---|
| CH4-V3-01 | MED | 2C | The V3 Technique Grounding Table (`2C:210-221`, 12 SK-V18-2C rows) carries `transfer_reason/admission_gate/verification_action/close_status/wave_owner` inline but OMITS `loc_estimate` AND `risk_class` from every grounded/partial/refuted row — the two fold-addendum fields (`T-P2-V2-FOLD-ADDENDUM.md:39-40`) mandated for any `grounded`/`partial`/`architecture-pressure` row. 2B (`2B:401-417`), 2D (`2D:95-98`), 2E (`2E:132-144`), and 2F (`2F:92-100`) each grew a DEDICATED per-row cost manifest with `loc_estimate`/`risk_class`/`rollback`/`abrogate`; 2C never grew one for its V3 grounding rows. The `## V3 Wave-Owner Re-key` paragraph (`2C:319-334`) supplies wave owners but no LOC/risk. This is the same cost-field-completeness gap CH4-V1-03/04 raised against 2D/2E/2F — which they folded — surviving uncorrected in 2C. A future consumer copying a 2C grounding row (e.g. the CSS-typed-provider G2 row, the un-fork G3 row) has a wave owner but no LOC/risk budget. | REVISE: add a 2C SK-V18 cost manifest (the `2B:409`/`2D:95`/`2E:132` shape) covering the SK-V18-scope V3 rows with `loc_estimate` + `risk_class` keyed to the live §8 band (G1/G2/G3/PROVE: `≤450 LOC`; the 3 SK-V19-receiver rows carry their tranche_scope) and `rollback`/`abrogate` for the routes that build code (typed CSS provider, un-fork, witness-emission). The 9-IDENT-LEAK / CSS-TYPES-HOST-SHIM / fleet-onboarding rows are SK-V19-receivers (cost = SK-V19); the firewall/named-primitive rows are gate-only. | blocks-completeness-only |
| CH4-V3-02 | LOW | 2C, 2D | The `css_provider_source == generated` SECOND SEAM in SK-V18-2C-RELOCATED-SEAM-FIREWALL (`2C:216`) introduces a NEW admission gate (the G2 emitter must read ZERO symbols from the hand-owned generic `crates/core/src/runtime/css_l4/` surface) but carries no `loc_estimate`/`risk_class`/`rollback`/`abrogate`. 2D's firewall cost row (LAC-2D-V3-02, `2D:96`: `+1 PartialEq derive + grep gate ≈ +10 gate LOC`) costs ONLY the `RuntimeTarget`-field firewall — it does NOT cost this CSS-typed-side-channel second seam (the `css_provider_source` grep gate is a SEPARATE check). So a second admission gate is introduced uncosted in either dossier. | REVISE: either fold the `css_provider_source==generated` grep gate's LOC/risk into the 2C cost manifest (CH4-V3-01) as a G2-owned gate-only row (`≈ +5 gate LOC`, LOW, rollback=remove the additive grep, abrogate=escalate to a structural css-provider-origin assertion) OR extend 2D LAC-2D-V3-02 to cost BOTH seams. As written the second seam is a free admission gate. | non-blocking |
| CH4-V3-03 | LOW | 2A, 2E, 2B | The Lemire-2026 "deployable NEON route IS the `vceqq_u8` eq-fan" attribution is load-bearing for the G6 eq-fan grounding but the post's BODY uses the Langdale/Lemire TABLE-driven (TBL/shuffle) classifier as the conventional NEON route; the `vceqq_u8` eq-fan is a COMMENTER suggestion. 2E `:71` discloses this precisely ("…the `vceqq_u8` eq-fan (comments)"); the 2E Executive Summary (`2E:46-48`) and Assertion 3 (`2E:178-183`), and 2B's exec framing, present the eq-fan as the post's endorsed route without the "(comments)" qualifier. The G6 eq-fan is INDEPENDENTLY grounded on simdjson/Langdale-Lemire vectorized classification + the live on-disk two-fan kernel, so the cost is not at risk — only the citation precision is loose in the summary prose. | REVISE: carry the `(comments)` qualifier from 2E `:71` into the 2E Executive Summary / Assertion 3 and the 2B exec framing, so the eq-fan-as-deployable-route is attributed to the comment thread, not the post's benchmark. The primary endorsement the post's body makes is the TBL classifier; the eq-fan's real grounding is the simdjson lineage + the on-disk kernel. | non-blocking |

## Per-row cost-lens enumeration (ACCEPT / REVISE / REJECT)

ACCEPT = the row carries a complete, realistic admission cost with a named
same-wave consumer (or an explicit no-consumer/abrogate disclosure) and no
orphan-kernel research. REVISE names the dossier + the exact correction.

### 2A — SOTA landscape (primitive cost routed to 2B/2C/2E/2F; that routing is correct)
- ACCEPT — T2A-V1-SOTA-JSON-001/002/004/005 (`primitive_cost=none in 2A`,
  comparator-plane rows; no kernel cost owed here).
- ACCEPT — T2A-V1-SOTA-JSON-003 sonic-rs leaf (CH4-V2-03 FOLDED:
  `wave_owner=SK-V19-receiver`, `close_status=blocked`, conditional LOC,
  `hard_cap_fit=n/a in SK-V18` — the live SK-V18 budget is no longer charged).
- ACCEPT — T2A-V1-SOTA-CSS-001/002/003 (`primitive_cost=provider cost lives in
  2C/2F`; correct hand-off, no double-count).
- ACCEPT — T2A-V18-DAV1D-001 (G5/G6 retarget; full Lock-16 cell + `≤450`/`≤90min`
  band; scalar ref `scan_scalar`/`find_component_delim` + checkasm + P3-collapsed
  same-wave consumer named).
- ACCEPT — T2A-V18-DAV1D-002 (`g6_speedup_claim_emitted==false` machine gate;
  cost=none, pure discipline — the anti-paper-close gate the lens wants).
- ACCEPT — T2A-V18-CSS-LAZY-001 / JSON-SONIC-001 / SONIC-LAZY-002 / ASMJSON-001
  (`primitive_cost=none in 2A`, projection cost owned by G1/G2; ASMJSON authors
  NO JSON kernel — the no-orphan-kernel discipline applied positively).
- REVISE (CH4-V3-03) — the Lemire-2026 eq-fan-as-deployable-route attribution is
  loosely stated in the 2A/2B exec framing (the post's body endorses the TBL
  classifier; the eq-fan is a commenter route).
- ACCEPT (refutations, no cost owed) — all 2A REFUTE rows.

### 2B — Primitive vocabulary
- ACCEPT — V2 HISTORICAL grounding table + A3a manifest (CH4-V2-01 FOLDED:
  retired `W#` owners now under explicit `SK-V15 HISTORICAL (non-SK-V18-cost)`
  headers with in-cell "do NOT copy" disclaimers).
- ACCEPT — every SK-V18 §6 grounding row (`2B:267-273`): scalar reference +
  checkasm + hardware gate + named live consumer; the eq-set kernel (the
  strongest) has a live CSS `count_top_level_commas` consumer (verified on disk).
- ACCEPT — SK-V18 Cost Manifest (`2B:409-417`): live `G5/G6`/`G2`/`none-DELETE`
  owners, realistic `≈0`/`≤150`/`≤200` LOC, rollback + abrogate columns;
  FSM/frame-stack row `DELETE-only, 0 LOC` (no orphan rebuild budgeted, A5
  reconcile blocks the retained-frame-stack reintroduction).
- ACCEPT (refutations) — LD4/PMULL/CSSC/SVE2/FSM citation-only rows REFUTED;
  citation density admits nothing. The eq-set 8-byte-cap refutation is on-disk
  grounded (two-fan exists).

### 2C — Grammar neutrality
- ACCEPT — V2 HISTORICAL grounding + LAC tables (CH4-V2-01 FOLDED: HISTORICAL
  headers + in-cell disclaimers `2C:56-61`,`:148-153`).
- REVISE (CH4-V3-01) — V3 grounding rows `2C:210-221`: NO `loc_estimate`/
  `risk_class` cells; 2C never grew the dedicated cost manifest 2B/2D/2E/2F all
  grew. The Wave-Owner Re-key gives owners but no LOC/risk.
- REVISE (CH4-V3-02) — the `css_provider_source==generated` second seam
  (`2C:216`) is a new admission gate carried with no LOC/risk and not costed in
  2D's firewall row either.
- ACCEPT — the 3 SK-V19-receiver rows (9-IDENT-LEAK, CSS-TYPES-HOST-SHIM,
  fleet-onboarding) carry `tranche_scope` inline so a SK-V18 budget is not
  charged a SK-V19 close (the empirical 13-site self-gate falsification is
  verified-at-HEAD; only its R16 close is SK-V19).

### 2D — Cost model
- ACCEPT — R-A row `2D:53` + LAC-2D-V3-01 `2D:95`: full manifest (`wave_owner=G3,
  ≤450 LOC, HIGH-risk, rollback=PATH-local revert, abrogate=DELETE-rebuild`).
  DERIVED-not-new verified live (`NormalizeDirectSinkCost`, the fork DELETE target
  both on disk).
- ACCEPT — firewall LAC-2D-V3-02 `2D:96` (`G3, +10 gate LOC, MED, additive`) —
  but see CH4-V3-02: it costs only the `RuntimeTarget`-field seam, not the
  CSS-side-channel second seam 2C adds.
- ACCEPT — LAC-2D-V3-03 `2D:97` (regression-guard `≈0 LOC, LOW`; correctly
  re-keyed since the engine landed — verified live).
- ACCEPT — LAC-2D-V3-04 `2D:98` CollapsedStage (CH4-V2-02 FOLDED: committed
  inert-slot `≈0`/no-consumer vs conditional rebuild `≤450`/G5/G6-gated split;
  REDRESS 96/97/98 prior cited and verified).
- ACCEPT — no-candidates axes `2D:100-104` (five-shape canon, no sixth shape, no
  new cost axis, no new stage — no speculative cost admitted; five-shape verified
  at `lower/mod.rs:18-24`).

### 2E — Host-arch esoterica
- ACCEPT — Section A `SignificantSetSkipTwoFan` / `Movemask64Shrn` (cost manifest
  `2E:134-135`): live `G5/G6` owners, `≤150`/`≈10` LOC, rollback + abrogate,
  salvage kernel + SHRN divergence both verified on disk. The QUARANTINE split
  (`2E:94-101`) WIRED-IN vs HOST-PRESENT-NO-CONSUMER is the no-orphan discipline.
- ACCEPT — Section B rows `2E:136-143`: every row `same_wave_consumer=NONE`,
  `wave_owner=none`, LOC only for the `if reopened` build, `abrogate=DO NOT
  author`. TBL/LD4/PMULL/CSSC/DotProd/I8MM/SHA3 held host-present-no-consumer.
- ACCEPT — Section C `2E:144` (PMU process; x86 DELETED; svmatch REFUTED on the
  SVE2-absent host — verified by Lemire-2026 + local probe).
- REVISE (CH4-V3-03) — the eq-fan-as-deployable-route attribution to the post is
  loose in the exec summary / Assertion 3 (precise at `2E:71`).
- ACCEPT — LACs `2E:228-230` (G5/G6/G2-entry owners; close tests are
  emitted-asm/grep/structural-co-gate probes).

### 2F — parse-that gaps
- ACCEPT — per-gap cost manifest `2F:92-100`: every gap carries `wave_owner`
  (G1/G2/G5-G6/SK-deferred), realistic LOC, rollback, abrogate; float no-fallback
  `0 LOC / n/a` (REFUTED, no admission); provenance-reconcile `≈0 (decision)`.
- ACCEPT — PTG-2F-09/10/11/12/13 `2F:81-85`: each names the existing
  checkasm-gated kernel + scalar reference + a live-or-deferred consumer; the G6
  WIRE is a RETARGET of an already-admissible kernel (`byte_class_from_eq_set_64`,
  live CSS consumer), not a new-kernel admission. The R-F Cand-B bitmap is
  LEDGER-FENCED against REDRESS 96/97/98 inline.
- ACCEPT — LAC-2F-V3-01/02/03 (single-substrate mask-unification co-gate +
  RETARGET-not-AUTHOR + re-scope; close tests are greps + a structural
  `bbnf_simd_single_mask_convention` co-gate).

## Cost-lens assessment

On the CH4 axis the SK-V18 packet is strong: every SK-V18-scope grounded
primitive carries the full Lock-16 admission cell (scalar reference + checkasm
parity + hardware gate + same-wave consumer), realistic LOC keyed to the live §8
wave band, and rollback/abrogate columns. The three V2 CH4 folds all landed and
are correct. The two architectural disciplines the lens most wants are present:

1. **No orphan-kernel research.** 2E quarantines WIRED-IN (Section A) from
   HOST-PRESENT-NO-CONSUMER (Section B) so a citation count cannot aggregate into
   a primitive admission; every Section-B/candidate row is
   `same_wave_consumer=NONE` with `abrogate=DO NOT author`. 2B's FSM/frame-stack
   route is `DELETE-only, 0 LOC` (A5 reconcile blocks the retained-frame-stack
   reintroduction). 2A authors NO JSON classifier because the path is scan-free.
   The anti-orphan discipline is applied uniformly.
2. **Named same-wave consumer per admission.** The G6 retarget rides the verified
   already-live kernel (`byte_class_from_eq_set_64` ← `count_top_level_commas` ←
   generated CSS), so the admission burden is "retarget an admissible kernel," not
   "admit a new loop"; the `find_css_significant` salvage that makes the `≤150 LOC`
   estimate credible is verified on disk, and the REDRESS 144 production precedent
   + the 96/97/98 cautionary prior are both verified exactly.

The REVISE is narrow and completeness-scoped, not a re-grounding: 2C's V3
grounding rows never grew the per-row `loc_estimate`/`risk_class` cost manifest
that 2B/2D/2E/2F all carry (CH4-V3-01 — the same gap CH4-V1-03/04 raised and the
other dossiers folded, surviving in 2C); 2C's CSS-side-channel second seam is an
uncosted new admission gate (CH4-V3-02); and the Lemire-2026 eq-fan attribution is
loosely stated in the exec prose (CH4-V3-03, the cost itself is independently
grounded). No citation is confabulated; no refuted-route is admitted; no cost is
broadcast; no SIMD row admits from citation alone.

## Fold Requirements (REVISE)

1. **CH4-V3-01 (2C):** add a 2C SK-V18 cost manifest (the `2B:409`/`2D:95`/
   `2E:132` shape) covering the SK-V18-scope V3 grounding rows with `loc_estimate`
   + `risk_class` keyed to the live §8 band, plus `rollback`/`abrogate` for the
   code-building routes (typed CSS provider G2, un-fork G3, witness-emission P4).
   The 9-IDENT-LEAK / CSS-TYPES-HOST-SHIM / fleet-onboarding rows are
   SK-V19-receivers (cost = SK-V19, carried by their existing `tranche_scope`);
   the firewall / named-primitive / 5-shape rows are gate-only (`≈0`/small LOC).
2. **CH4-V3-02 (2C / 2D):** cost the `css_provider_source==generated` second-seam
   grep gate — either as a G2-owned gate-only row in the new 2C manifest (`≈ +5
   gate LOC`, LOW, additive, rollback=remove the grep, abrogate=escalate to a
   structural css-provider-origin assertion) or by extending 2D LAC-2D-V3-02 to
   cover BOTH the `RuntimeTarget`-field and CSS-typed-side-channel seams.
3. **CH4-V3-03 (2A / 2B / 2E):** carry the `(comments)` qualifier from 2E `:71`
   into the 2E Executive Summary / Assertion 3 and the 2A/2B exec framing, so the
   eq-fan-as-deployable-NEON-route is attributed to the Lemire-2026 comment thread
   (the post's body endorses the TBL classifier); the eq-fan's binding grounding
   is the simdjson/Langdale-Lemire lineage + the on-disk two-fan kernel.

## Convergence Impact

CH4 returns `REVISE`, so it **blocks T-P2 V3 convergence** until the three folds
land. All findings are cost-field-completeness / citation-precision hygiene; none
is a confabulated citation, a refuted-route admission, an orphan kernel, or a
broadcast cost. The three V2 CH4 obligations all folded correctly. A V4 fold of
these three (a 2C cost manifest, the second-seam cost, the Lemire-2026 qualifier)
is expected to clear CH4 to ACCEPT.

TALLY accept=24 revise=3 reject=0
