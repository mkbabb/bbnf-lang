# CH4 COST — SK-V18 T-P2 CHALLENGE (cycle V2)

Lens name: CH4 COST. Every grounded primitive must carry an admission cost —
scalar reference + checkasm parity per Lock 16, a same-wave consumer named,
realistic LOC/risk, no orphan-kernel research.

Disposition: `REVISE`.

Target packet (regenerated 2026-06-01 for SK-V18):
`2A-sota-landscape.md` (cycle SK-V18-T-P2), `2B-primitive-vocabulary.md`
(V3-SKV18), `2C-grammar-neutrality.md` (V3), `2D-cost-model.md` (V3-SKV18),
`2E-host-arch-esoterica.md` (V6-SKV18), `2F-parse-that-gaps.md` (V3).

This is a fresh CH4 review of the SK-V18 dossiers — NOT a carry of the prior
SK-V15 CH4 ACCEPT (whose line anchors no longer resolve against the
regenerated packet). The cost-field schema is `T-P2-V2-FOLD-ADDENDUM.md:27-64`
(transfer_reason / admission_gate / verification_action / close_status /
loc_estimate / risk_class / wave_owner / hard_cap_fit; plus SIMD-row
scalar_reference / parity_or_checkasm / hardware_gate / same_wave_consumer /
row_movement_target). The live wave-band authority is SPEC §8
`restart/skinny/tranches/sk-v18/SPEC.md:433-444` (G1/G2/G3/G4/G5-G6 each
`≤450 hand source/test/gate LOC`, `≤90 min wave wall`, `30/45 min/redress`).

## Citation spot-verification (load-bearing, this lens)

Every load-bearing cost grounding was confirmed against a primary source:

- **iburg** (2D R-A cost-model spine): Fraser/Hanson/Proebsting, "Engineering a
  Simple, Efficient Code-Generator Generator", ACM LOPLAS 1(3) 1992, pp.213-226,
  DOI 10.1145/151640.151642 — VERIFIED (ACM DL + drh/iburg). Real.
- **egg** (2D cost engine): Willsey et al., PACMPL 5(POPL) 2021, Article 23,
  DOI 10.1145/3434304 — VERIFIED exact. Real.
- **Mison** (2D SinkOnly direct projection): Li et al., PVLDB 10(10) 2017,
  pp.1118-1129, DOI 10.14778/3115404.3115416 — VERIFIED exact. Real.
- **Pratt** (2C Sheets precedence-tower negative-control cost): "Top Down
  Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 — VERIFIED. Real.
- **Kutenin SHRN movemask** (2E `Movemask64Shrn` cost): "Bit twiddling with Arm
  Neon: beating SSE movemasks…", Arm Community blog — VERIFIED, `vshrn_n_u16`
  shift-right-4 narrowing confirmed. Real.
- **Lemire-2026 ARM match** (2B/2E eq-fan-deployable + svmatch re-refutation):
  `lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/`
  — VERIFIED live; "SVE2 match may be the fastest" matches the dossier framing.
  Real (and correctly used to REFUTE NEON-svmatch on the SVE2-absent host).

Load-bearing LOCAL cost claims spot-verified against the tree:

- **REDRESS 144 production precedent** (cited in 2B/2E/2F as the retarget
  cost/risk grounding): `skinny/REDRESS.md:4420-4436` — Track 1 `444.208` Mbps
  vs prior `434.1316…`, Criterion `+109.87%`, `G-W12-SIMD-ASM-PRODUCTION`
  `PASS-ADMIT`. VERIFIED exact.
- **REDRESS 96/97/98** negative prior (`G-W3-UNION-SUBSTRATE` retired) at
  `REDRESS.md:2797,2852,2910,2934`; **REDRESS 126** `ROUTE-PRODUCTION-SPLIT`
  (microbench PASS is NOT a row move) at `:3768-3769` — VERIFIED. The
  cautionary-prior framing is honest.
- **`find_css_significant` salvage kernel** (the "already exists" claim driving
  the `≤150 LOC` retarget estimates in 2B/2E/2F):
  `skinny/crates/runtime/src/runtime_simd.rs:169-216` — VERIFIED: two-fan
  `set_a[8]` + `set_b` split, `byte_class_from_eq_set_64(.,set_a) |
  byte_class_from_eq_set_64(.,set_b)`, scalar tail; only `#[cfg(test)]` caller
  at `lib.rs:574`. The salvage is real → the low LOC estimates are credible.
- **`find_ascii_set_member64` no-live-caller / `count_top_level_commas`
  CSS-live-consumer** (the same-wave-consumer cells): VERIFIED —
  `find_ascii_set_member64` has only bench/report/test references;
  `count_top_level_commas` IS consumed by generated CSS
  (`css_l4_nested_layout/generated.rs:157`, `css_l4_stylesheet_selectors/…`).
  The "CSS-only by live consumer" disclosure is accurate, not paper-close.

No confabulated or unverifiable citation found. No refuted-route grounding.

## Critical Findings

| id | severity | dossier | finding | required action | close |
|---|---|---|---|---|---|
| CH4-V2-01 | MED | 2B, 2C | Retired SK-V15 `W#` wave-owner cells survive INSIDE per-row cost manifests. 2B V2 grounding table (`2B:62-79`) and the A3a route manifest (`2B:149-158`) write `wave_owner=W2`/`W2/W7`/`W8/W9` in-cell; 2C V2 grounding+LAC rows (`2C:60-152`) write `wave_owner=W1/W4/W5/W6/W7/W8`. The fold-addendum field `wave_owner` is supposed to name the OWNING wave; a future consumer copying a cost row cites a retired SK-V15 wave. The re-key to the live §8 ledger exists only as a CLOSING paragraph (`2B:382-398` "the V2 `W#` keys … stand only as the SK-V15 historical record"; 2C `§V3 Wave-Owner Re-key:300-315`), not in-cell. | REVISE: mark each retired-`W#` cost row inline (e.g. `wave_owner=W2 [SK-V15-historical; SK-V18→P-cluster]`) OR move the V2 manifest tables under an explicit `## SK-V15 HISTORICAL (non-SK-V18-cost)` header so no live cost cell carries a retired wave id. The SK-V18-scope rows (2B §6 table + Cost Manifest `2B:390-395`; 2C V3 grounding `2C:200-211`) already carry live `G#` owners and are clean. | blocks-clarity-only |
| CH4-V2-02 | LOW | 2D | LAC-2D-V3-04 (`2D:98`) gives CollapsedStage admission `wave_owner=G5/G6` with `loc_estimate ≤450` while its risk row is `MED-HIGH` and the body is `diagnostic-only / author-declared` with NO same-wave consumer named today (the slot is inert). That is the correct close_status, but the `≤450 LOC` + `G5/G6` owner reads as a budgeted build for a row whose `final disposition = diagnostic-only-pending-G5/G6-evidence`. The cost is conditional, not committed. | REVISE: split the CollapsedStage cost into (a) the inert-slot cost (≈0, no consumer) and (b) the conditional rebuild cost (`≤450`, gated on a profiled CollapsedStage hot leaf + scalar oracle + checkasm + same-wave consumer). As written a reader could budget `≤450` G5/G6 LOC against an inert slot. | non-blocking |
| CH4-V2-03 | LOW | 2A | T2A-V1-SOTA-JSON-003 (sonic-rs targeted leaf, `2A:56`) carries `loc_estimate=150-350 per leaf`, `wave_owner=G5/G6`, but its OWN admission_gate states the JSON direct path is scan-free so "a sonic-rs leaf has no SK-V18 JSON consumer; its only viable same-wave consumer is the G5/G6 CSS scan or a SK-V19 receiver." This is honestly disclosed (abrogate→SK-V19, rollback=leave unwired, `close_status=source-present-unwired`) — so it is NOT an orphan-kernel admission. The only weakness is that a `150-350 LOC` G5/G6 cost is asserted for a leaf whose consumer is admittedly conjectural. | REVISE: re-key the cost to `wave_owner=SK-V19-receiver (no LIVE SK-V18 consumer)` with the G5/G6 figure marked conditional-on-a-CSS-string/number-leaf-profiling-hot, so the live SK-V18 budget is not charged a leaf with no SK-V18 consumer. | non-blocking |

## Per-row cost-lens enumeration (ACCEPT / REVISE / REJECT)

ACCEPT means the row carries a complete, realistic admission cost with a named
same-wave consumer (or an explicit no-consumer/abrogate disclosure) and no
orphan-kernel research. REVISE names the dossier + the exact correction.

### 2A — SOTA landscape (cost mostly routed to 2B/2C/2E/2F; that routing is correct)
- ACCEPT — T2A-V1-SOTA-JSON-001/002/004/005 (`primitive_cost=none in 2A`,
  routed to a 2B/2E leaf; comparator-plane rows, no kernel cost owed here).
- REVISE (CH4-V2-03) — T2A-V1-SOTA-JSON-003 sonic-rs leaf: conjectural
  same-wave consumer charged a live G5/G6 LOC budget.
- ACCEPT — T2A-V1-SOTA-CSS-001/002/003 (`primitive_cost=provider cost lives in
  2C/2F`; correct hand-off, no double-count).
- ACCEPT — T2A-V18-DAV1D-001 (G6 retarget, full Lock-16 cell + `≤450`/`≤90min`
  G5/G6 band; scalar ref + checkasm + P3-collapsed same-wave consumer named).
- ACCEPT — T2A-V18-DAV1D-002 (`g6_speedup_claim_emitted==false` gate; cost=none,
  pure discipline — the anti-paper-close gate the lens wants).
- ACCEPT — T2A-V18-CSS-LAZY-001 / JSON-SONIC-001 / SONIC-LAZY-002 / ASMJSON-001
  (`primitive_cost=none in 2A`, projection cost owned by G1/G2; ASMJSON honestly
  authors NO JSON kernel — the no-orphan-kernel discipline applied positively).
- ACCEPT (refutations, no cost owed) — all 2A REFUTE rows.

### 2B — Primitive vocabulary
- REVISE (CH4-V2-01) — V2 grounding table `2B:62-79` + A3a manifest
  `2B:149-158`: retired `W#` wave-owner cells inside live cost manifests.
- ACCEPT — every SK-V18 §6 grounding row `2B:257-263`: each carries scalar
  reference + checkasm + hardware gate + a named live consumer; the eq-set
  kernel (the strongest) has a live CSS `count_top_level_commas` consumer
  (verified). No citation-only admission.
- ACCEPT — SK-V18 Cost Manifest `2B:390-395`: live `G5/G6`/`G2` owners,
  realistic `≈0`/`≤150`/`≤200` LOC, rollback + abrogate columns, FSM/frame-stack
  row `DELETE-default, 0 LOC` (no orphan rebuild budgeted).
- ACCEPT (refutations) — LD4/PMULL/CSSC/SVE2 citation-only rows REFUTED with
  `loc_estimate` for the BLOCKED rebuild stated but `hard_cap_fit=no` —
  citation density admits nothing, exactly the lens position.

### 2C — Grammar neutrality
- REVISE (CH4-V2-01) — V2 grounding+LAC rows `2C:60-152`: retired
  `W1/W4/W5/W6/W7/W8` wave-owner cells inside live cost manifests.
- ACCEPT — V3 grounding rows `2C:200-211`: live `G1/G2/G3/PROVE/P4/SK-V19`
  owners; the 3 SK-V19-receiver rows (9-IDENT-LEAK, CSS-TYPES-HOST-SHIM,
  fleet-onboarding) carry `tranche_scope` inline so a SK-V18 budget is not
  charged a SK-V19 close.
- ACCEPT — V3 Wave-Owner Re-key `2C:300-315` (explicit SK-V15→SK-V18 wave map;
  the right discipline — but it must reach the in-cell W# values per CH4-V2-01).
- ACCEPT — V3 LACs `2C:326-…` (G2/SK-V19 owners, close tests are greps, not
  prose).

### 2D — Cost model
- ACCEPT — R-A row `2D:53` + LAC-2D-V3-01 `2D:95`: full manifest
  (`wave_owner=G3, ≤450 LOC, HIGH-risk, rollback=PATH-local revert,
  abrogate=DELETE-rebuild if relocated seam re-appears`). DERIVED-not-new is
  correct and verified (the engine exists: `NormalizeDirectSinkCost` live).
- ACCEPT — firewall LAC-2D-V3-02 `2D:96` (`G3, +10 gate LOC, MED, additive`).
- ACCEPT — LAC-2D-V3-03 `2D:97` (regression-guard, `≈0 LOC, LOW`; correctly
  re-keyed from activation-requirement to invariant since the engine landed).
- REVISE (CH4-V2-02) — LAC-2D-V3-04 `2D:98` CollapsedStage: `≤450`/`G5/G6`
  budget for an inert slot with no same-wave consumer; split committed vs
  conditional cost.
- ACCEPT (no-candidates axes `2D:100-104`; the five-shape canon holds, no sixth
  shape, no new cost axis, no new stage — no speculative cost admitted).

### 2E — Host-arch esoterica
- ACCEPT — Section A `SignificantSetSkipTwoFan` / `Movemask64Shrn`
  (`2E:106-107`, cost manifest `2E:134-135`): live `G5/G6` owners, `≤150`/`≈10`
  LOC, rollback + abrogate, salvage-kernel verified on disk. The two-section
  QUARANTINE (`2E:94-101`) — WIRED-IN vs HOST-PRESENT-NO-CONSUMER — is exactly
  the no-orphan-kernel discipline: it stops a consumer aggregating citation
  count across wired and no-consumer rows.
- ACCEPT — Section B rows `2E:112-118` + manifest `2E:136-143`: every row is
  `same_wave_consumer = NONE`, `wave_owner=none`, `loc_estimate` stated only for
  the conditional `if reopened` build, `abrogate threshold = DO NOT author`.
  TBL/LD4/PMULL/CSSC/DotProd/I8MM/SHA3 are all correctly held as host-present,
  no-consumer — no orphan kernel admitted.
- ACCEPT — Section C `2E:122-125` (PMU process; x86 DELETED; svmatch REFUTED on
  the SVE2-absent host — verified by the Lemire-2026 citation + local probe).
- ACCEPT — LACs `2E:228-230` (G5/G6/P1 owners; close tests are emitted-asm/grep
  probes).

### 2F — parse-that gaps
- ACCEPT — per-gap cost manifest `2F:92-100`: every gap carries `wave_owner`
  (G1/G2/G5-G6/SK-deferred), realistic LOC, rollback, abrogate; the float
  no-fallback gap is `0 LOC / n/a` (REFUTED, no admission); the
  provenance-reconcile gap is `≈0 (decision, not code)`.
- ACCEPT — PTG-2F-09/10/11/12/13 `2F:81-85`: each names the existing
  checkasm-gated kernel + scalar reference + a live-or-deferred consumer; the
  G6 WIRE is correctly a RETARGET of an already-admissible kernel
  (`byte_class_from_eq_set_64`, live CSS consumer), not a new-kernel admission.
- ACCEPT — LAC-2F-V3-01/02/03 (single-substrate + RETARGET-not-AUTHOR +
  re-scope, close tests are greps).

## Cost-lens assessment

The SK-V18 packet is, on the CH4 axis, the strongest of the cycle: every
SK-V18-scope grounded primitive carries the full Lock-16 admission cell
(scalar reference + checkasm parity + hardware gate + same-wave consumer),
realistic LOC keyed to the LIVE §8 wave band, and rollback/abrogate columns.
The two architectural disciplines the lens most wants are present and correct:

1. **No orphan-kernel research.** 2E quarantines WIRED-IN (Section A) from
   HOST-PRESENT-NO-CONSUMER (Section B) so a citation count cannot be aggregated
   into a primitive admission; every Section-B / candidate row is
   `same_wave_consumer = NONE` with `abrogate = DO NOT author`. 2B's
   FSM/frame-stack rebuild is `DELETE-default, 0 LOC`. 2A authors NO JSON
   classifier because the path is scan-free. This is the anti-orphan discipline
   applied uniformly.
2. **Named same-wave consumer per admission.** The G6 retarget rides a verified
   already-live kernel (`byte_class_from_eq_set_64` ← `count_top_level_commas` ←
   generated CSS), so the admission burden is "retarget an admissible kernel,"
   not "admit a new hand-tuned loop" — the cheapest honest Lock-16 path, and the
   `find_css_significant` salvage that makes the `≤150 LOC` estimate credible is
   verified on disk.

The REVISE is narrow and clarity-scoped, not structural: retired SK-V15 `W#`
wave owners survive INSIDE per-row cost cells of the carried-forward V2 sections
of 2B and 2C (CH4-V2-01), so a future consumer can cite a retired wave from a
live cost manifest; plus two over-budgeted conditional rows (CollapsedStage
inert slot, 2D; sonic-rs no-SK-V18-consumer leaf, 2A). No citation is
confabulated; no refuted-route is admitted; no cost is broadcast; no SIMD row
admits from citation alone.

## Fold Requirements (REVISE)

1. **CH4-V2-01 (2B, 2C):** retired `W#` wave-owner values must not appear in a
   live SK-V18 cost cell. Either annotate each in-cell `wave_owner=W#` as
   `[SK-V15-historical]` or move the V2 manifest tables under an explicit
   `SK-V15 HISTORICAL — non-SK-V18-cost` header. The closing re-key paragraph is
   necessary but does not reach the cells.
2. **CH4-V2-02 (2D):** split LAC-2D-V3-04 CollapsedStage cost into committed
   inert-slot cost (≈0, no consumer) vs conditional rebuild cost (≤450, gated on
   a profiled CollapsedStage hot leaf + scalar oracle + checkasm + same-wave
   consumer). Do not budget ≤450 G5/G6 LOC against an inert slot.
3. **CH4-V2-03 (2A):** re-key T2A-V1-SOTA-JSON-003 to
   `wave_owner=SK-V19-receiver (no LIVE SK-V18 consumer)`, the 150-350 LOC
   marked conditional-on-a-CSS-string/number-leaf-profiling-hot, so the live
   SK-V18 budget is not charged a leaf with no SK-V18 consumer.

## Convergence Impact

CH4 returns `REVISE`, so it **blocks T-P2 V2 convergence** until the three folds
land. All findings are clarity/budget-hygiene; none is a confabulated citation,
a refuted-route admission, an orphan kernel, or a broadcast cost. A V3 fold of
these three is expected to clear CH4 to ACCEPT.

TALLY accept=24 revise=3 reject=0
