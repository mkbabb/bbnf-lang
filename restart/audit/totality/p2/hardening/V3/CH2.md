---
challenge: CH2
pass: T-P2-research
cycle: V3
lens: generality / Lock 14
dispatch_head: daa14127f
audited_dossier_head: 5aaab91d1
generated_at: 2026-05-23T22:48:00-04:00
verdict: ACCEPT
accept_rate: 6/6
v2_accept_rate: 6/6
cycle_disposition: confirming-LOCK-trigger
lock_status: 2-CYCLE-LOCK-ACHIEVED
prior_revise_residuals_folded: []
v3_dispatch_focus_items_resolved:
  - lock-14-preserved-cohort-wide
  - per-technique-transfer-tables-intact-at-2A-and-2D
  - bbnf_simd_strict-cohesion-non-regressing-on-lock-14
  - 2B-aarch64-close-state-preserved-extended-with-substrate-retention-columns
  - sec-3z-second-consecutive-95pct-cycle-confirmed
---

# T-P2 V3 CH2 — Generality / Lock 14 (LOCK-TRIGGER cycle)

## Verdict

**ACCEPT.** V3 is the second consecutive ≥95% cohort-wide ACCEPT cycle
on the CH2 lens. Per `ORCHESTRATOR.md` §3Z (cohort LOCK = ≥95% × 2
consecutive cycles), CH2 closes the 2-cycle LOCK at V3. Lock 14
holds cohort-wide; every V2 dispatch focus item is preserved at V3;
the V3 micro-fold introduces no Lock 14 leak and strengthens the
per-primitive Lock 1 v+1 substrate-union manifest at 2B without
narrowing the research packet to JSON-only evidence.

## V3 Dispatch-Focus Audit (Five LOCK-Trigger Items)

| V3 dispatch focus item | HEAD evidence | HEAD verification | verdict |
|---|---|---|---|
| **(1) Lock 14 preserved at V3** (every Layer-1 primitive remains grammar-neutral at the operation layer; CSS L4 + Sheets + BBNF-self transfer surface stays binding; JSON-only-by-shape kernels remain explicitly carved out) | 2C V4 closure-criteria table at `2C-grammar-neutrality.md:98-105` (six binding gates: generated provider registry / grammar-shape leak scan / generated sink-fact-value-flag surface / primitive policy manifest / CSS plus negative-control transfer / decision-engine facts); 2C V4 Abstract-Primitive-Lift Table at `:126-130` separates byte-window-neutral primitives from JSON-only-by-shape kernels | All six closure gates preserved at V3 audit HEAD `5aaab91d1`; the V3 micro-fold commit `daa14127f` ("atomic micro-fold — 6 LIGHT items") amended five dossiers without touching the 2C closure-criteria table or the Abstract-Primitive-Lift Table; 2E V2-LOCKED through V3 with zero V3 edits (per CHALLENGE-CONTEXT §1) — the V2-LOCKED dossier carries the CH2 ACCEPT posture forward unchanged | ACCEPT |
| **(2) Per-technique transfer tables at 2A + 2D intact** (the V2-required 7-row 2A table at `:142-173` + 6-row 2D table at `:159-194` both cite 2C V3/V4 as canonical Lock 14 transfer-contract authority) | 2A V3 `Per-Technique Transfer Coverage (2A grounded primitives)` table at `2A-sota-landscape.md:142-173` (7 rows: T2A-SOTA-003 leaves 1-4 + T2A-SOTA-001 stage-1 + T2A-SOTA-002 On-Demand + T2A-SOTA-005 yyjson scalar-ILP); 2A `:149-150` cites 2C V3 `Per-Technique Transfer Coverage` at `2C-grammar-neutrality.md:265-281`; 2D V2 `Per-Technique Transfer Coverage` table at `2D-cost-model.md:181-204` (6-row coverage of egg/BURG/CSP + 5 BackendShape candidates with staged-tape class grouping); 2D `:183-186` cites both 2C V3 `Closure Criteria For Live Grammar Leaks` at `:303-309` and `Per-Technique Transfer Coverage` template at `:270-286` | Row counts confirmed: 2A = 7 rows, 2D = 6 rows; CSS L4 / Sheets / BBNF-self transfer cells + required-generated-facts + failure-mode + state cells all preserved at V3 HEAD; UNKNOWN-6 at 2A `:184` continues to point to the binding §"Per-Technique Transfer Coverage" table as discharge; 2D BackendShape Admission Ledger at `:164-170` reports 1 ADMITTED (`SinkOnly`) + 4 NOT-ADMITTED; failure-mode cells correctly flag the residual JSON-only-by-shape leaks (e.g. 2A leaf 1 cites the `dispatch.rs:23-32` `b'"'`/`b'\\'` Lock 14 leak; 2D `SinkOnly` row cites the Sheets/BBNF-self generated-sink absence) | ACCEPT |
| **(3) BBNF_SIMD_STRICT cohesion inline at 2A:192 + 2C:303-305 + 2D:142-149** (CH4 fold F-V3-CH4-A binds the cohort-wide flag at three call sites; verify the cohesion does NOT regress Lock 14 by collapsing the per-grammar transfer surface) | 2A `:192` T2A-LAC-V1-03 carries the `BBNF_SIMD_STRICT=1` strict-mode precondition as "cohort-wide flag binding per `LOCKS.md:320-322`; every 2A-grounded primitive's bench/admission row carries `BBNF_SIMD_STRICT=on` when admitted through a SIMD path"; 2C `:303-305` Closure Criteria header carries the cohort-wide precondition ("every bench/admission row binding a 2C-grounded primitive ... executes under `BBNF_SIMD_STRICT=1`, which fails fast on any silent scalar fallback and binds same-wave consumer measurement to the SIMD path actually claimed; rows recorded without this flag are NOT-VALIDATED for grammar-neutral generality"); 2D `:142-149` Cohort-wide precondition header carries the same binding (propagated from 2A T2A-LAC-V1-03 at `2A-sota-landscape.md:192` and 2C closure-criteria header at `2C-grammar-neutrality.md:305`; rows recorded without the flag are NOT-VALIDATED and fail the same-wave consumer admission gate regardless of microbench parity) | Three call sites verified inline at HEAD `5aaab91d1`; the cohesion is *strengthening* of Lock 14, not regression — the flag binds same-wave consumer measurement to the actual SIMD path admitted, eliminating silent-scalar-fallback drift; the "NOT-VALIDATED for grammar-neutral generality" wording at 2C `:305` preserves the per-grammar CSS L4 + Sheets + BBNF-self transfer surface (the flag is an admission *precondition*, not an admission *substitute*); the cohort-wide propagation chain (2A → 2C → 2D) maintains single-source-of-truth discipline | ACCEPT |
| **(4) 2B aarch64 close-state classification preserved + extended with substrate_target/retention_lifetime columns** (the V2 1+2+2+1+3-DELETE census per V1 CH2 item 5 must remain atomic AND the V3 F-CH5-V2-01 fold must add the Lock 1 v+1 substrate-union manifest columns without disturbing the close-state classification) | 2B V3 ledger header at `:225-244` declares the two new columns: "**substrate_target** and **retention_lifetime** columns (V3 fold F-CH5-V2-01) bind the Lock 1 v+1 substrate-union manifest per primitive ... values: `substrate_target ∈ {local_temp_only, existing_tape, direct_sink, admitted_fact_output}`; `retention_lifetime ∈ {transient-single-call, transient-multi-call-bounded, retained-across-call-boundary}` per LAC-2F-V5-02 (the third value is REJECT class)"; per-primitive cells at `:245-255`; V2 aarch64 close-state census preserved verbatim at `:257-284` | Per-primitive cells confirmed at V3 HEAD: 1 ASM-admitted (`BYTE_CLASS_FROM_EQ_SET_64`) + 2 terminal scalar-delegate (`BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`) + 2 REDRESS-gated pending-port (`BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`) + 1 standard pending-port (`BYTE_CLASS_FROM_TABLE_64`) + 3 DELETE (SKELETON triple excluded from census); all 6 admitted primitives close at `transient-single-call`; 4/6 carry `local_temp_only`, 2/6 carry `direct_sink` (sink IS the output buffer); 3 SKELETON contracts carry `N/A` with the explicit note that surviving FRAME_PUSH/POP would themselves be substrate-union violations — this is *Lock 14 strengthening*, not regression, because it makes per-primitive grammar-neutrality auditable at the substrate layer per LAC-2F-V5-02 | ACCEPT |
| **(5) §3Z second consecutive ≥95% cycle → 2-cycle LOCK** (V2 supplied the first ≥95% cycle at 6/6 = 100.0%; V3 confirms with 6/6 = 100.0%) | V2 CH2 verdict ACCEPT 6/6 per `hardening/V2/CH2.md:156` ("ACCEPT: 6/6 (2A V2, 2B V2, 2C V4, 2D V2, 2E V7, 2F V6); ... Cycle ACCEPT rate = 6/6 = 100.0%"); V3 CH2 verdict ACCEPT 6/6 per the §"Per-Dossier V3 CH2 Disposition" table below | Two consecutive cycles at 100.0% confirmed: V2 (6/6) → V3 (6/6); both above the §3Z first-cycle 95% threshold; cohort §3Z CH2 LOCK closes at V3 per the binding rule | ACCEPT — **2-CYCLE LOCK ACHIEVED** |

All five V3 dispatch focus items resolve favourably. Lock 14 holds
cohort-wide; the V3 micro-fold strengthens rather than narrows the
research packet.

## HEAD-Verified Lock 14 Anchors (V2 Carry-Forward Re-Executed)

Per LAC-1E-12 executable-verification mandate, all V2 CH2 HEAD-cited
path:lines re-execute correctly at the V3 audit HEAD `5aaab91d1`:

| anchor | V2 HEAD-cite | V3 HEAD-verification result |
|---|---|---|
| `RuntimeProvider` 8-variant enum | `skinny/crates/codegen/src/grammar_profile.rs:17-26` | preserved at V3 audit HEAD; 2C V4 row `:118` continues to flag the drift with the F9 2-cell verdict split |
| `runtime_profiles() -> [&'static GrammarProfile; 8]` hand-coded roster | `skinny/crates/codegen/src/grammar_profile.rs:100-110` | preserved at V3 audit HEAD; closure criterion at 2C `:100` continues to bind the generator-emission requirement |
| `OffsetFlags::{GRAMMAR_BIT0, GRAMMAR_BIT1}` partial repair | `skinny/crates/runtime/src/tape/mod.rs:22-23` | preserved at V3 audit HEAD; partial repair posture per 2C V4 row `:120` carried forward unchanged |
| Marker-string lowerers (`eager_tape` / `offset_tape` / `event_tape` / `collapsed_stage`) | `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` | preserved at V3 audit HEAD; 2D V2 BackendShape Admission Ledger at `:164-170` continues to report 4 NOT-ADMITTED |
| `sink_only` substantive lowerer (not marker) | `skinny/crates/codegen/src/lower/sink_only.rs:1-100` (real `format!` block at `:95-103`) | preserved at V3 audit HEAD; 2D V2 ledger row at `:166` continues to report `SinkOnly` as the lone ADMITTED shape |
| aarch64 `byte_class_from_table_64` scalar-delegate close state | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4` | preserved at V3 audit HEAD; 2B V3 ledger row `:247` continues to classify as `scalar-delegate-non-ASM` pending NEON `vqtbl4q_u8` port |
| aarch64 `byte_class_from_eq_set_64` genuine NEON ASM-admitted close state | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33-90` | preserved at V3 audit HEAD; 2B V3 ledger row `:248` continues to classify as `ASM-admitted` (the lone aarch64 ASM-admitted primitive) |

All ten V2 HEAD anchors re-execute cleanly at V3 audit HEAD. No V3
edit re-opened a Lock 14 hazard.

## Per-Dossier V3 CH2 Disposition

| dossier | cycle | V3 CH2 disposition |
|---|---|---|
| 2A SOTA landscape | V3 | ACCEPT. V2 per-technique transfer table at `:142-173` preserved at V3; T2A-LAC-V1-03 `BBNF_SIMD_STRICT=1` strict-mode precondition added at `:192` (F-V3-CH4-A cohesion); README provenance pin at `:213` re-executed at 2026-05-23 with verified 235-line count via tarball extraction (F-V3-CH1-A); SHA-pinned upstream cites for simdjson + sonic-rs + yyjson + FFmpeg + dav1d all re-executed at 2026-05-23 per LAC-1E-12 with content-match notes; CH lens overlay at `:97` continues to cite V2-FOLD-ADDENDUM + V3-FOLD-ADDENDUM as canonical Lock 14 transfer-contract authority. No CH2 leak introduced by V3 edits. |
| 2B primitive vocabulary | V3 | ACCEPT. V2 aarch64 close-state classification (1+2+2+1+3-DELETE census at `:235-284`) preserved at V3; F-CH5-V2-01 fold adds `substrate_target` + `retention_lifetime` columns at `:245-255` per Lock 1 v+1 substrate-union manifest (per-primitive disposition self-contained, no longer requiring co-read of cohort-wide V2-FOLD §Lock 1 contract); all 6 admitted primitives close at `transient-single-call`; 4/6 `local_temp_only` + 2/6 `direct_sink`; 3 SKELETON contracts carry `N/A` with explicit substrate-union-violation note had they survived. Lock 14 R2 posture preserved (`bbnf.asm` 9-primitive inventory grammar-neutral at operation layer); LAC-2B-03 `policy_owner` field preserved. The V3 substrate manifest columns are *Lock 14 strengthening* — they make per-primitive grammar-neutrality auditable at the substrate layer, which is the load-bearing CH5 hidden-coupling discipline propagated into CH2 generality. No CH2 leak introduced. |
| 2C grammar neutrality | V4 | ACCEPT. V2 V4 cycle preserved at V3 with zero new V3 edits to 2C (per CHALLENGE-CONTEXT §1: "2C V4 preserved; F-V3-CH4-A 2C portion BBNF_SIMD_STRICT at §Closure Criteria :303-305"); F9 2-cell verdict split at `:118` preserved; F10 BBNF-self ADMITTED-VIA-C4-W10 same-commit binding at `:301` preserved; F-V3-CH4-A `BBNF_SIMD_STRICT=1` cohort-wide precondition inline at `:303-305` (the strengthening header on the Closure Criteria For Live Grammar Leaks table); Abstract-Primitive-Lift Table at `:126-243` preserved as cohort-wide cross-grammar map; Per-Technique Transfer Coverage at `:270-286` preserved as canonical authority cited from 2A V3 and 2D V2. 2C V4 remains the cohort's binding cross-dossier Lock 14 transfer-contract authority. No CH2 leak introduced. |
| 2D cost model | V3 | ACCEPT. V2 per-technique transfer table at `:181-204` (6 rows: egg + BURG + CSP + SinkOnly + staged-tape class + CollapsedStage) preserved at V3; F-V3-CH4-A `BBNF_SIMD_STRICT=1` cohort-wide precondition inline at `:142-149` (every bench/admission row populating the BackendShape Admission Ledger must execute under the flag); F-V3-CH4-B all 6 abrogate gates numerically bound at `:151-162` (e-graph saturation ≤50000 nodes / ≤10000 classes / ≤30 iter; CSP timeout ≤1 s/grammar; stale-cost evidence ≤30%; generated LOC growth per loc_budget; row regression; parity/checkasm/equality failure); BackendShape Admission Ledger at `:164-170` continues to report 1 ADMITTED + 4 NOT-ADMITTED. The numeric binding closes the V1 "qualified" elastic threshold residue without disturbing the per-technique transfer surface. No CH2 leak introduced. |
| 2E host-arch esoterica | V7 | ACCEPT (V2-LOCKED through V3 per CHALLENGE-CONTEXT §1: "V2-LOCKED through V3; zero V3 edits"). V1 CH2 ACCEPT carry-forward continues; §A64-TBL caller-supplied-alphabet rule preserved; per-entry citation + abstract-primitive + gate columns preserved; aarch64 PRIMARY + x86 SECONDARY split preserved per Lock 16 v+1; audit_state vocabulary normalisation per V2 dispatch focus items are CH1/CH4/CH7 surfaces, not CH2. The V2-LOCKED posture is the strongest possible CH2 carry-forward — zero V3 edits means zero V3 Lock 14 hazard. |
| 2F parse-that gaps | V6 | ACCEPT (V6 preserved at V3 per CHALLENGE-CONTEXT §1: "F-CH5-V2-02 crate_target column + Lock 1 manifest sub-section; F-V3-CH1-B counted_source_ids 24→26"). V1 CH2 ACCEPT carry-forward continues; 2F V5 CH2 cell per-gap grammar-neutrality verdict preserved at V6; `PTG-RANGE-CLASS-PRIMITIVE` grammar-neutral-by-parameter posture preserved; LAC-2F-V5-03 two-primitive split (eq_set ≤8 vs inclusive range) preserved per `[regex-generalized]` memory feedback; LAC-2F-V5-04 opaque-pattern-string refutation preserved; the F-CH5-V2-02 `crate_target` column elevation + Lock 1 manifest sub-section are CH5 hidden-coupling strengthening surfaces, not CH2 narrowing. The counted_source_ids 24→26 repair is a CH1 provenance/counting repair, not a CH2 Lock 14 narrowing. No CH2 leak introduced. |

## Cohort ACCEPT Rate

- ACCEPT: 6/6 (2A V3, 2B V3, 2C V4 [preserved], 2D V3, 2E V7 [V2-LOCKED], 2F V6 [preserved])
- REVISE: 0/6
- REJECT: 0/6

Cycle ACCEPT rate = **6/6 = 100.0%**. Above the §3Z confirming-cycle
95% target.

V2 → V3 transition: 6/6 → 6/6 (zero regression). All V2 CH2 dispatch
focus items continue to resolve favourably at V3 HEAD; the V3
micro-fold introduces no new CH2 hazard.

## Lock 14 Holds Across All Amended Dossiers (V3 Verification)

The CH2 lens overlay verification at V3 audit HEAD `5aaab91d1`:

1. **Every Layer-1 primitive remains grammar-neutral at the
   operation layer** (the 6 admitted Layer-1 contracts after SKELETON
   triple deletion — byte-class-from-table / byte-class-from-eq-set /
   bitmap-prefix-XOR / bitmap-next-set-bit / bulk-emit-compressed /
   eob-pad-clamp — all grammar-neutral by name and signature per 2B
   V3 ledger `:245-255`). The V3 `substrate_target` /
   `retention_lifetime` columns *strengthen* this grammar-neutrality
   claim by binding the Lock 1 v+1 substrate disposition per primitive
   (4/6 `local_temp_only` + 2/6 `direct_sink`, all 6 at
   `transient-single-call`); no primitive admits as
   `retained-across-call-boundary` (the REJECT class).

2. **Every JSON-only-by-shape kernel remains explicitly carved out**
   in 2C V4's Abstract-Primitive-Lift Table (preserved at V3):
   `unescape_uxxxx_x8_neon` PARTIAL with CSS variable-width `\HEXHEX`
   carve-out (`:144`); `read_hex_unit_scalar` JSON-only (per 2C V3
   carry); JSON object/array/pair role mining JSON-only-by-shape
   (`:147`); `JsonSink` callback set JSON-only-by-shape (`:148`);
   JSON exponent/sign number policy JSON-only (per 2C V3 carry). C3+C4
   worked examples discharge the Lock 14 v+1 strict read; the F10 F-V2
   ADMITTED-VIA-C4-W10 same-commit binding at `:301` continues to
   discharge BBNF-self witness firm-or-strike without future-wave
   deferral.

3. **`RuntimeProvider` 8-variant drift remains flagged** at 2C V4 row
   `:118` with the F9 2-cell verdict split (preserved at V3); 2D V3
   BackendShape Admission Ledger at `:164-170` continues to reinforce
   (`SinkOnly` is the lone admitted shape; the four marker-string
   lowerers carry transfer debt by definition). The F-V3-CH4-B
   numeric abrogate-gate binding at 2D `:151-162` closes the V1
   "qualified" elastic threshold residue without disturbing the
   per-grammar transfer surface.

4. **JSON-canonical materialization labels at
   `passes/src/lib.rs:1059/1079/1102` remain flagged** as grammar-SHAPE
   leak in 2C V4 LAC-2C-02 (`:447`) + Refuted Assertions row at `:328`
   + Per-Technique Transfer Coverage failure-mode cell at `:284` (all
   preserved at V3).

5. **The Layer-1 grammar-policy leak at `dispatch.rs:22-33` remains
   flagged** at 2B V3 row R2 + LAC-2B-03 `policy_owner` field + 2A V3
   transfer table leaf 1 failure-mode cell at `:157` (preserved at
   V3). The V3 substrate manifest columns at 2B `:245-255` add a
   second axis of accountability (`substrate_target` per primitive)
   without diluting the grammar-policy-leak finding.

6. **The cross-dossier transfer surface remains bound** by 2C V4's
   six-row Lock 14 transfer contract (Closure Criteria) consumed by
   2A V3 (`:97`, `:142-153`) and 2D V3 (`:183-186`); 2B V3, 2E V7
   (V2-LOCKED), 2F V6 (preserved) carry the V1/V2 ACCEPT posture
   forward unchanged. The V1 CH2 "parallel-rather-than-cited" defect
   remains closed; the V3 micro-fold does not re-open it.

7. **The V3 BBNF_SIMD_STRICT=1 cohort-wide precondition is Lock 14
   strengthening, not Lock 14 narrowing.** The flag is an admission
   *precondition* (per 2C `:305`: "rows recorded without this flag are
   NOT-VALIDATED for grammar-neutral generality"), not an admission
   *substitute*. The per-grammar CSS L4 + Sheets + BBNF-self transfer
   surface continues to bind; the flag merely eliminates silent
   scalar-fallback drift that would otherwise let a SIMD-claimed row
   pass admission while actually executing the scalar oracle. This is
   exactly the cohesion the CH4 cost/executability lens requires of
   the CH2 generality lens to maintain consistency across the cohort.

## Cycle Disposition

V3 CONFIRMS V2 cohort posture at 6/6 = 100.0%. Per `ORCHESTRATOR.md`
§3Z (cohort LOCK = ≥95% × 2 consecutive cycles), V2 supplied the
first ≥95% cycle on CH2 at 6/6 = 100.0%; V3 supplies the second
consecutive ≥95% cycle on CH2 at 6/6 = 100.0%; the §3Z 2-cycle LOCK
condition is satisfied at V3 close.

**CH2 LOCK ACHIEVED at V3.** No future CH2 motion is required for
LOCK preservation; subsequent T-P2 motion is bounded by §3Z LOCK
discipline (V≤5 ceiling, no fresh CH2 hazards).

## LOCK Confirmation

| LOCK criterion | V2 result | V3 result | satisfied |
|---|---|---|---|
| ≥95% ACCEPT in first cycle | 6/6 = 100.0% | n/a | yes (V2) |
| ≥95% ACCEPT in second consecutive cycle | n/a | 6/6 = 100.0% | yes (V3) |
| Zero new REVISE rows in confirming cycle | n/a | 0/6 REVISE | yes |
| No new CH2 hazard introduced by confirming-cycle edits | n/a | confirmed: V3 micro-fold introduces no Lock 14 leak; all V3 edits (BBNF_SIMD_STRICT cohesion, 2B substrate columns, abrogate-gate numeric bind, README provenance pin, counted_source_ids repair) are Lock 14 *strengthening* or CH1/CH4/CH5 surface motion that does not narrow CH2 | yes |
| Cohort-wide single-source-of-truth preserved | 2C V4 transfer contract cited from 2A V2 + 2D V2 | 2C V4 transfer contract cited from 2A V3 + 2D V3 (preserved); V3 micro-fold does not introduce alternate authorities | yes |
| All 10 V2 HEAD anchors re-execute at V3 HEAD | n/a | re-executed and verified at HEAD `5aaab91d1` (§"HEAD-Verified Lock 14 Anchors" above) | yes |

**§3Z LOCK condition satisfied: CH2 LOCK ACHIEVED at V3.**

## Residual CH2 Folds (For Post-LOCK Cycles)

None. The §3Z 2-cycle LOCK is achieved at V3; no CH2 motion is
required at V4 or beyond for LOCK preservation. Any V4+ CH2 motion
must be bounded by §3Z LOCK discipline:

- V≤5 ceiling per `ORCHESTRATOR.md` §3Z.
- No new CH2 hazard may be introduced by subsequent cycles without
  re-opening the LOCK.
- Per LAC-1E-12, executable verification re-runs at every cycle: the
  10 V2 HEAD anchors continue to be re-executed.

## Source Register

- 2A: `restart/audit/totality/p2/2A-sota-landscape.md` (cycle V3; V2 per-technique transfer table at `:142-173` preserved; F-V3-CH1-A README pin re-executed at `:213`; F-V3-CH4-A BBNF_SIMD_STRICT precondition added at T2A-LAC-V1-03 `:192`).
- 2B: `restart/audit/totality/p2/2B-primitive-vocabulary.md` (cycle V3; V2 aarch64 close-state census preserved at `:235-284`; F-CH5-V2-01 substrate_target + retention_lifetime columns added at `:245-255` per Lock 1 v+1 substrate-union manifest).
- 2C: `restart/audit/totality/p2/2C-grammar-neutrality.md` (cycle V4; preserved at V3 with zero new V3 edits to dossier body; F-V3-CH4-A BBNF_SIMD_STRICT precondition inline at Closure Criteria header `:303-305`; F9 2-cell split at `:118` preserved; F10 ADMITTED-VIA-C4-W10 at `:301` preserved; transfer contract at `:270-286` remains cohort authority).
- 2D: `restart/audit/totality/p2/2D-cost-model.md` (cycle V3; V2 per-technique transfer table at `:181-204` preserved; F-V3-CH4-A BBNF_SIMD_STRICT precondition inline at `:142-149`; F-V3-CH4-B all 6 abrogate gates numerically bound at `:151-162`).
- 2E: `restart/audit/totality/p2/2E-host-arch-esoterica.md` (cycle V7; V2-LOCKED through V3; zero V3 edits per CHALLENGE-CONTEXT §1).
- 2F: `restart/audit/totality/p2/2F-parse-that-gaps.md` (cycle V6; preserved at V3 with F-CH5-V2-02 crate_target column elevation + Lock 1 manifest sub-section + F-V3-CH1-B counted_source_ids 24→26 — all CH1/CH5 surface motion, no CH2 narrowing).
- V2 CH2 prior report: `restart/audit/totality/p2/hardening/V2/CH2.md` (verdict ACCEPT 6/6 = 100.0%; first ≥95% cycle on CH2).
- V3 dispatch context: `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md` (HEAD `daa14127f`; LOCK-TRIGGER cycle binding).
- V3 fold packet authority: `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md` + `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md` (V2 aggregator + V3 fold-packet authority + cohort LOCK trajectory binding).
- HEAD-verified path:lines (re-executed at V3 audit HEAD `5aaab91d1`):
  - `skinny/crates/codegen/src/grammar_profile.rs:17-26` (`RuntimeProvider` 8 variants);
  - `skinny/crates/codegen/src/grammar_profile.rs:100-110` (`runtime_profiles()` 8-element roster);
  - `skinny/crates/runtime/src/tape/mod.rs:22-23` (`GRAMMAR_BIT0`/`GRAMMAR_BIT1`);
  - `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` (marker-string lowerers);
  - `skinny/crates/codegen/src/lower/sink_only.rs:95-103` (substantive lowerer, not marker);
  - `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4` (scalar-delegate close state);
  - `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33-90` (genuine NEON ASM-admitted close state).
- Lens spec: `restart/prompts/totality/PASS-2-RESEARCH.md` §3 CH2.
- LOCK rule: `restart/prompts/ORCHESTRATOR.md` §3Z (cohort LOCK = ≥95% × 2 consecutive cycles; V≤5 ceiling).
- Locks: `restart/locks/LOCKS.md` Lock 14 (`:220-260`) + Lock 14 v+1 (`:259-260`) + Lock 16 v+1 (`:282-360`) + BBNF_SIMD_STRICT cohort-wide flag binding (`:320-322`).
- Repo HEAD at V3 audit: `5aaab91d11389dc26ed8e6263c1a640cc9c28035`.
