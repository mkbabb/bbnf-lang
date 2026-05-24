---
challenge: CH2
pass: T-P2-research
cycle: V2
lens: generality / Lock 14
dispatch_head: b5628414f
audited_dossier_head: 4f17880d0
generated_at: 2026-05-23T22:35:00-04:00
verdict: ACCEPT
accept_rate: 6/6
v1_prior_accept_rate: 4/6
v1_revise_residuals_folded:
  - 2A-V1-CH2-fold-item-1 (cite 2C V3/V4 + per-technique transfer table)
  - 2D-V1-CH2-fold-item-2 (cite 2C V3/V4 + per-technique transfer table)
  - 2B-V1-CH2-aarch64-close-state-atomic-classification
---

# T-P2 V2 CH2 — Generality / Lock 14

## Verdict

**ACCEPT.** Lock 14 holds cohort-wide at V2. Every V1 REVISE residual
is folded; the five V2-dispatch-context CH2 focus items resolve
favourably at HEAD `4f17880d0`:

1. **2A V2 per-technique transfer table** lands at
   `2A-sota-landscape.md:142-173` as a binding seven-row table mapping
   the four sonic-rs SIMD leaves (long string, float fraction, field
   lookup, whitespace) plus simdjson stage-1 (T2A-SOTA-001), On-Demand
   forward-only (T2A-SOTA-002), and yyjson scalar ILP (T2A-SOTA-005)
   onto CSS L4 / Sheets / BBNF-self. The table cites 2C V3 authority
   at `:149-150` ("Authority: 2C V3 `Per-Technique Transfer Coverage`
   (`restart/audit/totality/p2/2C-grammar-neutrality.md:265-281`)")
   and the CH lens overlay at `:97` cites V2-FOLD-ADDENDUM +
   V3-FOLD-ADDENDUM as canonical Lock 14 transfer-contract authority.
   Each row carries CSS L4 / Sheets / BBNF-self transfer cells +
   `required generated facts` + `failure mode if absent` + `state`
   (NOT-VALIDATED / DEFER / grounded-as-primitive), matching the 2C V3
   template surface at `2C-grammar-neutrality.md:276-286`.

2. **2D V2 per-technique transfer table** lands at
   `2D-cost-model.md:159-194` as a binding six-row table covering the
   three decision-engine techniques (egg `T2D-EGRAPH-EXTRACTION`, BURG
   `T2D-BURG-FINITE-ALTERNATIVES`, CSP `T2D-CSP-FEASIBILITY-LAYER`)
   plus the five `BackendShape` candidates (`SinkOnly`, the simdjson
   staged-tape class grouping `EagerTape`/`OffsetTape`/`EventTape`,
   `CollapsedStage`) across CSS L4 / Sheets / BBNF-self. The table
   cites 2C V3 authority at `:161-164` ("cite 2C V3 V2-FOLD / V3-FOLD
   canonical Lock 14 transfer contract (the 'Closure Criteria For Live
   Grammar Leaks' table at `2C-grammar-neutrality.md:303-309` plus the
   'Per-Technique Transfer Coverage' template at
   `2C-grammar-neutrality.md:270-286`)"). The table reports the
   load-bearing operational truth: `SinkOnly` ADMITTED on CSS L4
   declaration-values only; all other rows are NOT-VALIDATED with
   explicit failure-mode cells (`CollapsedStage` flagged
   ARCHITECTURE-PRESSURE-ONLY on every grammar per Lock 16 v+1 x86-only
   constraint).

3. **2B aarch64 close-state classification** is atomic at
   `2B-primitive-vocabulary.md:223-262` per the V1 CH2 item 5 fold.
   The classification splits the 6 admitted Layer-1 primitives into 4
   non-uniform close states (vs the V1 imprecise "uniformly
   `scalar-delegate-non-ASM`" reading): **1 ASM-admitted**
   (`BYTE_CLASS_FROM_EQ_SET_64`, genuine NEON intrinsic at
   `src/aarch64/byte_class_from_eq_set_64.rs:33-90`); **2 terminal
   scalar-delegate by design** (`BULK_EMIT_COMPRESSED` no NEON 1-op
   for VBMI2 mask-compress-store at 64-bit lane;
   `EOB_PAD_CLAMP` scalar optimal for byte-copy + zero-pad on
   aarch64); **2 pending-REDRESS-gated**
   (`BITMAP_PREFIX_XOR_64` PMULL hot-body per REDRESS-88 S-P2 V3
   C-P2C-2; `BITMAP_NEXT_SET_BIT` CSSC CTZ bulk per REDRESS-89 S-P2
   V3 C-P2C-2); **1 standard pending-port** (`BYTE_CLASS_FROM_TABLE_64`
   awaits NEON `vqtbl4q_u8` 4-table lookup port per Lemire 2019
   lineage). The 3 SKELETON contracts (`FSM_DISPATCH_THREADED`,
   `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`) are DELETE per CH4-F1
   and correctly excluded from the aarch64 close-state census.

4. **2C V4 F9 verdict 2-cell split** lands at
   `2C-grammar-neutrality.md:118` for `2C-RUNTIME-PROVIDER-REGISTRY`.
   The verdict cell carries: "**refuted (Lock-14-citation-grounded)**
   (Lock 14 + HEAD evidence grounds the refutation: the enum-as-roster
   is a generic-crate name leak) **+ pending-generator-emission**
   (bbnf-discharge requires generated provider manifest emitter
   replacing the 8-variant hand-coded enum)". The "Generator-stub
   gap:" prose binds the bbnf-discharge cell to a namable closure path:
   "the bbnf-discharge cell closes when `cargo xtask` regenerates
   `RuntimeProvider` from `Cargo.toml` workspace metadata (or
   equivalent manifest source) and a Sheets/BBNF-self witness provider
   lands without editing `grammar_profile.rs`." The 2-cell template is
   referenced as "inherited from `2C-BBNF-SELF-FALSIFIER`" — a single
   template family, no orthogonal CH6 F9 surface introduced.
   Frontmatter V2-fold note at `2C-grammar-neutrality.md:22`
   enumerates the four rows amended under F9
   (`2C-CSS-TOKEN-ALPHABET`, `2C-CSS-SELECTOR-SCOPE`,
   `2C-CSS-CALC-VAR`, `2C-RUNTIME-PROVIDER-REGISTRY`).

5. **2C V4 F10 BBNF-self FIRMED ADMITTED-VIA-C4-W10** lands at
   `2C-grammar-neutrality.md:301`. The BBNF-self witness row state
   reads: "**ADMITTED-VIA-C4-W10** — bound to SK-V14 SPEC §13 W10 wave
   (`restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md:982`
   ships W10 Stage-0 UNCONDITIONALLY; P3-C `:36` W10 wave-manifest
   cell + `:423` W10 exit-gate item 8 bind C4 admission); same-commit
   binding: the W10 commit that admits C4 SIMD body necessarily
   exercises the BBNF-self literal-escape consumer in the same commit
   because the consumer routes through
   `parse_that_regex::unescape_string` which is the C4 SIMD body's
   caller, so admission and exercise are atomic. No future-wave-landing
   dependency; verdict is firm at admission of C4-W10." This is the
   firm-or-strike disposition the V1 CH6 wave required — there is no
   "future-wave-landing" soft-deferral; the same-commit atomicity is
   the firm verdict.

## V2 Dispatch-Focus Audit (Five Items)

| dispatch focus item | dossier evidence at HEAD | HEAD verification | verdict |
|---|---|---|---|
| 2A V2 per-technique transfer table (7 techniques × CSS L4 / Sheets / BBNF-self) citing 2C V3/V4 | `2A-sota-landscape.md:142-173` 7-row table (T2A-SOTA-003 leaves 1-4 + T2A-SOTA-009 lazy field skip combined with leaf 3, T2A-SOTA-001 simdjson stage-1, T2A-SOTA-002 On-Demand, T2A-SOTA-005 yyjson scalar-ILP); authority cite to 2C V3 at `:149-150`; CH lens overlay cite to V2-FOLD / V3-FOLD at `:97`; UNKNOWN-6 at `:184` points to the binding table | Row count = 7 confirmed; each row carries CSS L4 + Sheets + BBNF-self + required generated facts + failure mode + state cells; failure-mode cells explicitly flag the JSON-only-by-shape leaks (e.g. leaf 1 cites "hardcoded JSON `b'"'`/`b'\\'` in shared `dispatch.rs:23-32` (Lock 14 leak per CH2 V1 row R2)"); state cells carry NOT-VALIDATED / DEFER / grounded-as-primitive verdicts per the 2C V3 strict read | ACCEPT |
| 2D V2 per-technique transfer table (7 techniques) + 2C V3 cross-citation | `2D-cost-model.md:159-194` 6-row table (egg, BURG, CSP, SinkOnly, staged-tape class, CollapsedStage); authority cite to 2C V3 at `:161-164` (both "Closure Criteria For Live Grammar Leaks" at `:303-309` and "Per-Technique Transfer Coverage" at `:270-286` cited) | Per-technique counting: egg + BURG + CSP (3 decision-engine techniques) + SinkOnly + EagerTape + OffsetTape + EventTape + CollapsedStage (5 shape candidates, with EagerTape/OffsetTape/EventTape grouped into a single staged-tape row per simdjson VLDB 2019 class lineage) = 8 distinct techniques covered, satisfying the ≥7 dispatch requirement. The BackendShape Admission Ledger at `:142-148` confirms per-shape disposition (1 ADMITTED + 4 NOT ADMITTED). | ACCEPT |
| 2B aarch64 close-state classification atomic (1 ASM + 2 terminal-scalar + 2 pending-REDRESS + 1 pending-standard) | `2B-primitive-vocabulary.md:235-262` aarch64 close-state census | 1 ASM-admitted (`BYTE_CLASS_FROM_EQ_SET_64` via `src/aarch64/byte_class_from_eq_set_64.rs:33-90`) + 2 terminal scalar-delegate-by-design (`BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`) + 2 pending-REDRESS-gated (`BITMAP_PREFIX_XOR_64` per REDRESS-88, `BITMAP_NEXT_SET_BIT` per REDRESS-89) + 1 standard pending-port (`BYTE_CLASS_FROM_TABLE_64` awaits NEON `vqtbl4q_u8`) + 3 DELETE (SKELETON triple excluded from census) = exact 1+2+2+1+(3 excluded) split the V2 dispatch focus item requires. The V1 "uniformly `scalar-delegate-non-ASM`" imprecision is resolved per the per-primitive disposition cells. | ACCEPT |
| 2C V4 F9 verdict 2-cell split | `2C-grammar-neutrality.md:118` `2C-RUNTIME-PROVIDER-REGISTRY` row | Verdict cell carries "refuted (Lock-14-citation-grounded) + pending-generator-emission" 2-cell format; "Generator-stub gap:" prose names the closure mechanism ("`cargo xtask` regenerates `RuntimeProvider` from `Cargo.toml` workspace metadata and a Sheets/BBNF-self witness provider lands without editing `grammar_profile.rs`"); "Two-cell template inherited from `2C-BBNF-SELF-FALSIFIER`" enforces single template family. Frontmatter `:22` enumerates the four amended rows. Closure Criteria row at `:307` reinforces ("V3-V4 drift expanded the enum from 2 to 8 variants without converting to generation"). | ACCEPT |
| 2C V4 F10 BBNF-self FIRMED ADMITTED-VIA-C4-W10 | `2C-grammar-neutrality.md:301` BBNF-self witness state | State cell reads "ADMITTED-VIA-C4-W10 — bound to SK-V14 SPEC §13 W10 wave (`p3f-spec-draft.md:982` ships W10 Stage-0 UNCONDITIONALLY; P3-C `:36` W10 wave-manifest cell + `:423` W10 exit-gate item 8); same-commit binding: ... admission and exercise are atomic. No future-wave-landing dependency; verdict is firm at admission of C4-W10." The same-commit atomicity (consumer routes through `parse_that_regex::unescape_string`, which IS the C4 SIMD body's caller) discharges F10 without future-wave deferral. | ACCEPT |

All five V2 dispatch focus items resolve favourably at HEAD. Lock 14
holds cohort-wide.

## HEAD-Verified Lock 14 Anchors (V1 Carry-Forward Re-Executed)

Per LAC-1E-12 executable-verification mandate, all V1 CH2 HEAD-cited
path:lines re-execute correctly at the V2 audit HEAD `4f17880d0`:

| anchor | V1 HEAD-cite | V2 HEAD-verification result |
|---|---|---|
| `RuntimeProvider` 8-variant enum | `skinny/crates/codegen/src/grammar_profile.rs:17-26` | confirmed: 8 variants (`Json`, `CssL4DeclarationValues`, `CssL4DeclarationValuesExtended`, `CssL4StylesheetSelectors`, `CssL4VisualFunctions`, `CssL4AtRulesAndMedia`, `CssL4VendorAndCustomAtRules`, `CssL4NestedLayout`) |
| `runtime_profiles() -> [&'static GrammarProfile; 8]` hand-coded roster | `skinny/crates/codegen/src/grammar_profile.rs:100-110` | confirmed: 8-element array literal at exact lines |
| `OffsetFlags::{GRAMMAR_BIT0, GRAMMAR_BIT1}` partial repair | `skinny/crates/runtime/src/tape/mod.rs:22-23` | confirmed: `pub const GRAMMAR_BIT0: u8 = 0x01; pub const GRAMMAR_BIT1: u8 = 0x02;` |
| Marker-string lowerer `eager_tape` | `skinny/crates/codegen/src/lower/eager_tape.rs:15-17` | confirmed: `format!("rule {} -> eager_tape", rule.name)` |
| Marker-string lowerer `offset_tape` | `skinny/crates/codegen/src/lower/offset_tape.rs:15-16` | confirmed: `format!("rule {} -> offset_tape", rule.name)` |
| Marker-string lowerer `event_tape` | `skinny/crates/codegen/src/lower/event_tape.rs:15-16` | confirmed: `format!("rule {} -> event_tape", rule.name)` |
| Marker-string lowerer `collapsed_stage` | `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-16` | confirmed: `format!("rule {} -> collapsed_stage", rule.name)` |
| `sink_only` substantive lowerer (not marker) | `skinny/crates/codegen/src/lower/sink_only.rs:1-100` | confirmed: real `format!` block with code emission at `:95-103`, not a marker string |
| aarch64 `byte_class_from_table_64` scalar-delegate | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4` | confirmed: NEON wrapper at `:2-4` forwards verbatim to `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(src, table)` |

## Per-Dossier V2 CH2 Disposition

| dossier | cycle | V2 CH2 disposition |
|---|---|---|
| 2A SOTA landscape | V2 | ACCEPT. V1 CH2 fold item 1 discharged: (a) Per-Technique Transfer Coverage table at `:142-173` with 7 rows × CSS L4/Sheets/BBNF-self cells; (b) 2C V3 authority cite at `:149-150` (`Per-Technique Transfer Coverage` at `2C-grammar-neutrality.md:265-281`); (c) CH lens overlay V2 fold at `:97` cites V2-FOLD-ADDENDUM + V3-FOLD-ADDENDUM. The four-leaf + stage-1 + On-Demand + scalar-ILP technique set is mapped per-grammar with NOT-VALIDATED / DEFER / grounded-as-primitive verdicts; transfer requirements explicit; failure modes explicit. UNKNOWN-6 at `:184` now points to the binding table as discharge ("binding per-technique transfer table now lives at §'Per-Technique Transfer Coverage' above"). No CH2 leak remains. |
| 2B primitive vocabulary | V2 | ACCEPT. V1 CH2 fold item 5 (aarch64 close-state atomic classification) discharged: §A5 audit at `:223-262` carries explicit per-primitive aarch64 close-state cells; the 1+2+2+1+(3 excluded) census at `:235-262` resolves the V1 "uniformly scalar-delegate-non-ASM" imprecision. Lock 14 R2 posture preserved (`bbnf.asm` 9-primitive inventory remains grammar-neutral at operation layer per `:88`); LAC-2B-03 `policy_owner` field preserved; SKELETON triple correctly DELETE-dispositioned and excluded from aarch64 census; A6 adoption-cost ledger at `:291-301` uses the 8-state V2 normalised admission-state vocabulary cleanly. No CH2 leak remains. |
| 2C grammar neutrality | V4 | ACCEPT. F9 2-cell split and F10 BBNF-self FIRMED ADMITTED-VIA-C4-W10 both lock the V3 CH6 anti-paper-close residuals: (F9) verdict cell at `:118` carries `refuted + pending-generator-emission` with namable bbnf-discharge closure path; "Two-cell template inherited from `2C-BBNF-SELF-FALSIFIER`" enforces single-template-family discipline (no orthogonal CH6 surface). (F10) BBNF-self witness state at `:301` reads `ADMITTED-VIA-C4-W10` with same-commit binding rationale and `parse_that_regex::unescape_string` consumer-call path; "No future-wave-landing dependency; verdict is firm at admission of C4-W10." The 2C V4 transfer contract remains the cohort's binding cross-dossier authority (`Per-Technique Transfer Coverage` at `:270-286`; `Closure Criteria For Live Grammar Leaks` at `:303-309`); 2A V2 and 2D V2 both cite it correctly. No CH2 leak remains. |
| 2D cost model | V2 | ACCEPT. V1 CH2 fold item 2 discharged: (a) Per-Technique Transfer Coverage table at `:159-194` with 6 rows covering egg, BURG, CSP, SinkOnly, staged-tape class (EagerTape/OffsetTape/EventTape), CollapsedStage × CSS L4/Sheets/BBNF-self; (b) 2C V3 V2-FOLD/V3-FOLD authority cited at `:161-164` plus "Closure Criteria For Live Grammar Leaks" cite + "Per-Technique Transfer Coverage" template cite. The transfer table makes 2C V3's Lock 14 v+1 strict read concrete for the three decision-engine techniques (grammar-neutral by construction over `BackendExpr` plans; failure modes are grammar-name leaks in node vocabulary, cost-axis schema, or feasibility predicates) and the four non-`SinkOnly` shapes (transfer debt by definition; marker-string lowerers do not emit per-grammar bodies). The `BackendShape` Admission Ledger at `:142-148` reports 1 admitted (`SinkOnly`) + 4 not-admitted; UNKNOWN-2D-01 at `:227` points to the leak-scan verification harness (`2C-grammar-neutrality.md:354`). No CH2 leak remains. |
| 2E host-arch esoterica | V7 | ACCEPT (carry-forward; no V2 CH2 motion required per V1 ACCEPT). §A64-TBL caller-supplied-alphabet rule preserved (per V1 CH2 audit at 2C V3 ledger `:286`); per-entry citation + abstract-primitive + gate columns preserved; aarch64 PRIMARY + x86 SECONDARY split preserved per Lock 16 v+1 at `LOCKS.md:346-349`; URL-refresh + esoterica labels + audit_state vocabulary normalisation per V2 dispatch focus items are CH1/CH4/CH7 surfaces, not CH2. |
| 2F parse-that gaps | V6 | ACCEPT (carry-forward; no V2 CH2 motion required per V1 ACCEPT). 2F V5 CH2 cell per-gap grammar-neutrality verdict preserved at V6; `PTG-RANGE-CLASS-PRIMITIVE` grammar-neutral-by-parameter posture preserved; LAC-2F-V5-03 two-primitive split (eq_set ≤8 vs inclusive range) preserved per `[regex-generalized]` memory feedback; LAC-2F-V5-04 opaque-pattern-string refutation preserved; LAC-2F-V5-01 / -02 CH3 pre-flight + T-P3 §3C elevation are CH3 surfaces, not CH2. |

## Cohort ACCEPT Rate

- ACCEPT: 6/6 (2A V2, 2B V2, 2C V4, 2D V2, 2E V7, 2F V6)
- REVISE: 0/6
- REJECT: 0/6

Cycle ACCEPT rate = **6/6 = 100.0%**. Above the §3Z first-cycle ≥95%
target; first ≥95% cycle on CH2 achieved at V2.

V1 → V2 transition: 4/6 → 6/6 (+33.3 percentage points). The two V1
REVISE rows (2A V1 + 2D V1) both close via the dispatch-required V2
fold (per-technique transfer table + 2C V3/V4 cross-citation); the 2B
V1 ACCEPT-with-residual closes via the aarch64 atomic classification
census in §A5.

## Lock 14 Holds Across All Amended Dossiers

The CH2 lens overlay verification at HEAD `4f17880d0`:

1. **Every Layer-1 primitive remains grammar-neutral at the
   operation layer** (`bbnf.asm:30-44` declares 9 abstract-operation
   contracts; the SKELETON triple is DELETE per CH4-F1 — the remaining
   6 are byte-class / mask / carry / next-set-bit / bulk-emit /
   pad-clamp, all grammar-neutral by name and by signature).

2. **Every JSON-only-by-shape kernel remains explicitly carved out**
   in 2C V4's Abstract-Primitive-Lift Table:
   `unescape_uxxxx_x8_neon` PARTIAL with CSS variable-width `\HEXHEX`
   carve-out (`:144`); `read_hex_unit_scalar` JSON-only (per 2C V3
   carry); JSON object/array/pair role mining JSON-only-by-shape
   (`:147`); `JsonSink` callback set JSON-only-by-shape (`:148`); JSON
   exponent/sign number policy JSON-only (per 2C V3 carry). C3+C4
   worked examples at `:223-243` discharge the Lock 14 v+1 strict
   read.

3. **`RuntimeProvider` 8-variant drift remains flagged** at 2C V4
   row `:118` with the F9 2-cell verdict split + Closure Criteria row
   `:307` ("adding enum variants or grammar-name branches in
   `codegen` (V3-V4 drift expanded the enum from 2 to 8 variants
   without converting to generation)"); 2D V2 BackendShape Admission
   Ledger reinforces (`SinkOnly` is the lone admitted shape; the four
   marker-string lowerers carry transfer debt by definition).

4. **JSON-canonical materialization labels at
   `passes/src/lib.rs:1059/1079/1102` remain flagged** as grammar-SHAPE
   leak in 2C V4 LAC-2C-02 (`:447`) + Refuted Assertions row at `:328`
   + Per-Technique Transfer Coverage failure-mode cell at `:284`.

5. **The Layer-1 grammar-policy leak at `dispatch.rs:22-33` remains
   flagged** at 2B V2 row R2 + LAC-2B-03 `policy_owner` field
   amendment + 2A V2 transfer table leaf 1 failure-mode cell at `:157`
   ("hardcoded JSON `b'"'`/`b'\\'` in shared `dispatch.rs:23-32`
   (Lock 14 leak per CH2 V1 row R2)").

6. **The cross-dossier transfer surface is now bound** by 2C V4's
   six-row Lock 14 transfer contract (Closure Criteria) consumed by
   2A V2 (`:97`, `:142-153`) and 2D V2 (`:80-87`, `:161-164`); 2B V2,
   2E V7, 2F V6 carry the V1 ACCEPT posture forward unchanged. The
   V1 CH2 "parallel-rather-than-cited" defect is closed.

## Cycle Disposition

V2 LOCKS cohort on CH2 lens at 6/6 = 100.0%. Per `ORCHESTRATOR.md`
§3Z (cohort LOCK = ≥95% × 2 consecutive cycles), V2 supplies the
first ≥95% cycle on CH2. V3 must confirm the same posture to
complete the §3Z LOCK. No V3 CH2 motion is currently required: the
five V2 dispatch focus items all ACCEPT; no V2 edit re-opened a
Lock 14 hazard; Lock 14 holds.

Predicted V2 → V3 = confirming-cycle ACCEPT (no CH2 fold required);
cohort §3Z CH2 LOCK at V3 close.

## Residual CH2 Folds (For V3)

None. The five V2 dispatch focus items all resolve favourably at HEAD;
no Lock 14 leak is re-opened by V2 edits; the 2C V4 transfer contract
is the cohort's binding cross-dossier authority and is now cited from
both 2A V2 and 2D V2.

The V3 cycle confirms V2 posture by re-executing the five HEAD
verifications above; if all five remain stable, the cohort §3Z CH2
LOCK closes at V3.

## Source Register

- 2A: `restart/audit/totality/p2/2A-sota-landscape.md` (cycle V2; CH2 V1 fold item 1 discharged at `:142-173`).
- 2B: `restart/audit/totality/p2/2B-primitive-vocabulary.md` (cycle V2; CH2 V1 fold item 5 discharged at `:223-262`).
- 2C: `restart/audit/totality/p2/2C-grammar-neutrality.md` (cycle V4; F9 2-cell split at `:118`; F10 FIRMED ADMITTED-VIA-C4-W10 at `:301`).
- 2D: `restart/audit/totality/p2/2D-cost-model.md` (cycle V2; CH2 V1 fold item 2 discharged at `:159-194`).
- 2E: `restart/audit/totality/p2/2E-host-arch-esoterica.md` (cycle V7; V1 CH2 ACCEPT carry-forward).
- 2F: `restart/audit/totality/p2/2F-parse-that-gaps.md` (cycle V6; V1 CH2 ACCEPT carry-forward).
- V1 CH2 prior report: `restart/audit/totality/p2/hardening/V1/CH2.md` (verdict REVISE 4/6 = 66.7%; two REVISE rows 2A V1 + 2D V1 closed at V2 per dispatch focus items 1 and 2; one ACCEPT-with-residual row 2B V1 closed at V2 per dispatch focus item 3).
- V2 dispatch context: `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md` (HEAD `b5628414f`).
- V2 fold packet authority: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`.
- HEAD-verified path:lines (re-executed at V2 audit HEAD `4f17880d0`):
  - `skinny/crates/codegen/src/grammar_profile.rs:17-26` (`RuntimeProvider` 8 variants);
  - `skinny/crates/codegen/src/grammar_profile.rs:100-110` (`runtime_profiles()` 8-element roster);
  - `skinny/crates/runtime/src/tape/mod.rs:22-23` (`GRAMMAR_BIT0`/`GRAMMAR_BIT1`);
  - `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` (marker-string lowerers);
  - `skinny/crates/codegen/src/lower/sink_only.rs:95-103` (substantive lowerer, not marker);
  - `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4` (scalar-delegate close state);
  - `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33-90` (genuine NEON ASM-admitted close state).
- Lens spec: `restart/prompts/totality/PASS-2-RESEARCH.md` §3 CH2.
- Locks: `restart/locks/LOCKS.md` Lock 14 (`:220-260`) + Lock 14 v+1 (`:259-260`) + Lock 16 v+1 (`:282-360`) + Lock 16 close-state vocabulary (`:335-342`, `:346-349`).
- Repo HEAD at V2 audit: `4f17880d085973f6c130093fde6a376ed40d2274`.
