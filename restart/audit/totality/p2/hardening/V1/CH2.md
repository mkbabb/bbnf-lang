---
challenge: CH2
pass: T-P2-research
cycle: V1
lens: generality / Lock 14
dispatch_head: 8d5e4e8f6
audited_dossier_head: f25c3af2e
generated_at: 2026-05-23T22:30:00-04:00
verdict: REVISE
accept_rate: 4/6
---

# T-P2 V1 CH2 — Generality / Lock 14

## Verdict

**REVISE.** Lock 14 holds in posture across the cohort; the five dispatch
focus items resolve favourably at HEAD `f25c3af2e`:

1. Every grounded primitive is named grammar-neutrally at the operation
   layer (Layer-1 byte-class / mask / carry / prefix-XOR / next-set-bit /
   MAC / TBL classifier).
2. JSON-only-by-shape kernels are explicitly carved out in 2C's
   Abstract-Primitive-Lift Table (`unescape_uxxxx_x8_neon` PARTIAL with CSS
   `\HEXHEX` shape-orthogonal carve-out; `read_hex_unit_scalar` JSON-only;
   role mining / `JsonSink` / JSON number policy refuted as fleet-wide).
3. The `RuntimeProvider` V3→V4 enum drift (2→8 variants) is correctly
   flagged at 2C row "2C-RUNTIME-PROVIDER-REGISTRY" and "Architectural
   Assertions Refuted" with the explicit "without conversion to generation
   is itself a Lock 14 fault" pin.
4. JSON-canonical materialization labels at
   `passes/src/lib.rs:1059/1079/1102` are flagged as grammar-SHAPE leak in
   LAC-2C-02 with executable repair guidance.
5. The Layer-1 grammar-policy leak at `dispatch.rs:22-33` is flagged at 2B
   row R2 with `G-SIMD-GRAMMAR-POLICY` pre-block citation and LAC-2B-03
   `policy_owner` field amendment.

The cycle is REVISE, not ACCEPT, because two of the six dossiers (2A V1,
2D V1) have not yet folded the prior CH2 revise — they remain at V1 cycle
while 2C/2E/2F have advanced (V3/V6/V5) and now carry the canonical
transfer contract that 2A/2D should consume by reference. The cross-
dossier transfer surface is still split: 2C's six-row Lock 14 transfer
contract (Executive Summary §V3 Fold Authority) is the natural single
binding gate, but 2A V1 / 2D V1 do not yet cite it.

## Dispatch-Focus Audit (Five Items)

| dispatch focus item | dossier evidence at HEAD | HEAD verification | verdict |
|---|---|---|---|
| Lock 14 holds: every primitive grounded grammar-neutrally | 2B Layer-1 9-primitive inventory all named by abstract operation (`bbnf.asm:30-44` declares `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`); 2E §A64-TBL row "remains grammar-neutral while alphabet is caller-provided and collision-checked"; 2F PTG-RANGE-CLASS-PRIMITIVE "grammar-neutral by parameter". | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:30-44` confirmed; `skinny/crates/runtime/src/tape/mod.rs:18-36` shows `OffsetFlags::{GRAMMAR_BIT0, GRAMMAR_BIT1}` (HEAD partial repair from `HAS_ESC`/`HAS_CONTROL`); 2C row "2C-FLAG-SEMANTICS" notes the partial close. | ACCEPT |
| 2C JSON-only-by-shape refutations correctly carved out | 2C Abstract-Primitive-Lift Table rows: `unescape_uxxxx_x8_neon` (PARTIAL — CSS variable-width carved out per `p3a:106`); `read_hex_unit_scalar` (JSON-only-by-shape, fixed-4-nibble policy); JSON object/array/pair role mining (JSON-only-by-shape, `passes/src/lib.rs:1053-1110` cited); `JsonSink` callback set (JSON-only-by-shape, `runtime/src/grammars/json/sink.rs:4-16` cited); JSON exponent/sign number policy (JSON-only-by-shape, RFC 8259 specific). C3+C4 worked-examples section §C3/C4 documents the two strict-read patterns (ADMIT via cross-grammar non-JSON consumer; ADMIT via shape-identical + shape-orthogonal carve-out). | `skinny/crates/runtime/src/grammars/json/sink.rs` — name and method-set checked; JSON-canonical callback names confirmed. | ACCEPT |
| 2C `RuntimeProvider` V3-V4 enum drift (2→8 variants) flagged | 2C row "2C-RUNTIME-PROVIDER-REGISTRY" refuted state: "The registry must be generated from workspace metadata or grammar manifests. Adding CSS, Sheets, or BBNF-self by editing this enum is a Lock 14 failure. Note the V3-V4 drift expanded the enum from the 2C V2-fold's 2-variant cite to the 8-variant HEAD without converting to generation." Closure Criteria row also notes: "adding enum variants or grammar-name branches in `codegen` (V3-V4 drift expanded the enum from 2 to 8 variants without converting to generation)." | `skinny/crates/codegen/src/grammar_profile.rs:17-26` confirmed 8 variants (`Json`, `CssL4DeclarationValues`, `CssL4DeclarationValuesExtended`, `CssL4StylesheetSelectors`, `CssL4VisualFunctions`, `CssL4AtRulesAndMedia`, `CssL4VendorAndCustomAtRules`, `CssL4NestedLayout`); `runtime_profiles() -> [&'static GrammarProfile; 8]` at `:100-:110` confirmed as hand-coded roster. | ACCEPT |
| 2C JSON-canonical labels at `passes/src/lib.rs:1059/1079/1102` flagged as grammar-SHAPE leak (LAC-2C-02) | LAC-2C-02 "grammar-shape leak census" proposes: "Lock 14 verification must scan not only literal grammar names but grammar-shaped role policy: JSON punctuation alphabets, object/array/pair/string/number/bool/null roles, JSON-canonical materialization-descriptor labels, and hardcoded sink callback names. Consumer: CSS/Sheets/BBNF-self fixtures that derive labels from generated facts; modify `MaterializationDescriptor.label` to source from grammar rule names." Open Research Question §2C records the executable repair: "Replace `label: \"object\"` with `label: rule_name.to_owned()` or grammar-metadata source field; prove JSON parity (test fixtures at `:1684`-`:1760` use `\"object\"`/`\"array\"`/`\"pair\"` rule names already)." | `skinny/crates/passes/src/lib.rs:1059` confirmed `label: "object".to_string()`; `:1079` confirmed `label: "array".to_string()`; `:1102` confirmed `label: "pair".to_string()`. `TapeKind::{Container, Sequence, KeyValuePair}` enum is grammar-neutral; only the descriptor label string carries the JSON-canonical leak. | ACCEPT |
| 2B Layer 1 grammar-policy LEAK at `dispatch.rs:22-33` flagged | 2B row R2 "Layer 1 may encode grammar-specific constants": "However, `classify_tbl4`'s dispatch hardcodes JSON quote / backslash / control values at `skinny/crates/bbnf-simd/src/dispatch.rs:22-33`. This is acceptable for the current JSON caller but not for shared CSS or arbitrary-grammar use." LAC-2B-03 binds the repair: "Require `policy_owner` field on every Layer 1 consumer call site: `generated_grammar` (codegen emits the LUT/constants), `caller_data` (consumer supplies at runtime), or `none` (truly grammar-neutral, e.g. `BITMAP_PREFIX_XOR_64`). Reject shared call sites with hardcoded JSON constants." | `skinny/crates/bbnf-simd/src/dispatch.rs:23-32` confirmed hardcodes `b'"', b'\\', 0x20` as `classify_block_from_table` arguments. The Tbl4 backend is grammar-neutral; the dispatch is the JSON-policy injection point. | ACCEPT |

All five dispatch focus items resolve favourably at HEAD. The dossier
cohort correctly identifies the Lock 14 hazards and pins each to
executable repair guidance.

## Per-Dossier CH2 Disposition (Six)

| dossier | cycle | CH2 finding | disposition |
|---|---|---|---|
| 2A SOTA landscape | V1 | 2A correctly narrows SOTA transfer to generated grammar data or policy traits (`2A-sota-landscape.md:70`, `:133`) and refutes the grammar-name JSON whitelist at `passes/src/lib.rs:331` + role-miner at `:1300-1391` (T2A-LAC-V1-04). The CH2 lens overlay at `:70` correctly cites "CSS L4/Sheets/BBNF-self transfer evidence is required for generality claims; one CSS declaration-values row is non-JSON evidence, not full closure." | REVISE. Fold by citing 2C V3 V2-FOLD / V3-FOLD authority for the canonical transfer contract; 2A V1 does not yet do so. Per-technique transfer table for the four sonic-rs SIMD leaves (long string, float fraction, field lookup, whitespace) across CSS L4 / Sheets / BBNF-self is identified as Open Research Question (`:124`) but not yet filled in. Promote the §UNKNOWN row to a binding table cross-referenced with 2C's "Per-Technique Transfer Coverage". |
| 2B primitive vocabulary | V1 | 2B keeps Layer 1 at the byte / mask / carry / MAC level (R2 refutation, LAC-2B-03 `policy_owner`). Three of nine Layer-1 contracts (`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`) are explicitly flagged as skeleton-contract-only and non-admissible (A5 audit table). | ACCEPT (with one residual fold). The CH2 posture is correct. Residual fold: §A5 audit at HEAD lists 6/9 Layer-1 primitives `scalar-delegate-non-ASM` on aarch64 — record explicitly that aarch64 grammar-neutrality is `scalar-delegate-non-ASM` close state for those six until NEON bodies land per LAC-2B-05; the contract neutrality is preserved but the admission state is partial. |
| 2C grammar neutrality | V3 | 2C V3 fold-authority section establishes the canonical Lock 14 transfer contract as a six-row gate (`generated provider registry`, `grammar-shape leak scan`, `generated sink/fact/value/flag surface`, `primitive policy manifest`, `CSS plus negative-control transfer`, `decision-engine facts`). The Abstract-Primitive-Lift Table separates grammar-neutral primitives from JSON-only-by-shape kernels with `non-JSON consumer evidence` cell per row. C3+C4 worked examples discharge Lock 14 v+1 strict read. | ACCEPT. 2C V3 is the strongest CH2 dossier in the cohort and is the binding cross-dossier reference. The Future-Grammar Onboarding Test §Steps 1-7 with §What this test surfaces is the executable mechanical gate the V1 CH2 disposition required. The Closure Criteria For Live Grammar Leaks table is the V1 CH2 "no open questions" disposition. |
| 2D cost model | V1 | 2D V1 correctly requires backend-shape rewrites and CSP / cost guards to consume generated grammar metadata, not grammar-name branches or JSON-role mining (LAC-2D-03). The five-shape `BackendShape` defended as the V1 candidate set (`T2D-FIVE-SHAPE-FINITE-SET`). e-graph extraction + CSP feasibility + active cost extraction is the four-stage replacement for the hardcoded P1-P8 cascade. | REVISE. 2D V1 does not cite 2C V3's Closure Criteria; the LAC-2D-03 entry "CSS plus Sheets or BBNF-self proof fixtures required before fleet-wide admission" overlaps with 2C V3's "CSS plus negative-control transfer" row but is not cross-referenced. The 2D `T2D-EGRAPH-EXTRACTION` + `T2D-CSP-SCOPE` route per §3 is precisely what 2C V3's "decision-engine facts" closure criterion names; fold by mutual citation. Per-grammar transfer table per technique (`SinkOnly`, `Mison`, `CollapsedStage`) for CSS L4 + Sheets + BBNF-self is still absent — promote 2D Open Research Question UNKNOWN-2D-01 to a binding table. |
| 2E host-arch esoterica | V6 | 2E V6 §A64-TBL row makes the caller-supplied alphabet rule explicit: "remains grammar-neutral while alphabet is caller-provided and collision-checked (Lock 14 holds). Production-row gate: same-wave consumer + named generated grammar policy." Per-entry citation + abstract-primitive + gate columns preserved. NEON `svmatch_u8` refutation correctly carries the Lock 14 v+1 caller-provided alphabet rule. | ACCEPT. 2E correctly folds the caller-provided alphabet rule into the abstract-primitive declaration list. The 13 aarch64 PRIMARY + 9 x86 SECONDARY split preserves the M5 Max primary / x86 architecture-pressure posture per Lock 16 v+1 at `LOCKS.md:346-349`. |
| 2F parse-that gaps | V5 | 2F V5 CH2 cell at `:76` makes the grammar-neutrality verdict explicit per gap: "Each gap row carries an explicit grammar-neutrality verdict per S-P2 V3 P2-E §3 (8/8 gaps grammar-neutral); the regex/HIR gaps are grammar-neutral by construction (operate on patterns); the SIMD/string/float gaps carry parametric byte sets / ranges / escape bytes." `PTG-RANGE-CLASS-PRIMITIVE` is the load-bearing grammar-neutral generalization vehicle. LAC-2F-V5-03 pins the two-primitive split (eq_set ≤8 vs inclusive range) per `[regex-generalized]` memory feedback. | ACCEPT. 2F V5 correctly fences the extraction/import boundary: `bbnf-regex` carries HIR + nullability + first-set + byte-class facts as grammar-neutral; opaque pattern strings in `SinkOnlyExpr::RegexProgram` are refuted (LAC-2F-V5-04). The grammar-named helper rule (live only in generated grammar modules or grammar-local facades) is honoured. |

## Cohort ACCEPT Rate

- ACCEPT: 4/6 (2B, 2C, 2E, 2F)
- REVISE: 2/6 (2A V1, 2D V1)
- REJECT: 0/6

Cycle ACCEPT rate = **4/6 = 66.7%**. Below the §3Z first-cycle ≥95% target.
Per `PASS-2-RESEARCH.md §3` "Cycle V1 expects ≥30% REVISE", a 33.3% REVISE
rate is exactly the §3 V1 expectation surface — non-paper-close.

## Residual CH2 Folds (For V2)

1. **2A V1 → V2 fold: cite 2C V3 V2-FOLD / V3-FOLD authority.** The
   2C V3 transfer contract is the single binding gate for cross-dossier
   Lock 14 closure; 2A V1's LAC-V1-04 reproduces the same posture without
   reference. Per `[system-cohesion]` memory feedback, fold by mutual
   citation; do not create an orthogonal CH2 transfer surface in 2A V2.

2. **2D V1 → V2 fold: cite 2C V3 V2-FOLD / V3-FOLD authority.** Same
   posture: 2D V1's LAC-2D-03 overlaps 2C V3's "CSS plus negative-control
   transfer" row; fold by mutual citation. Specifically: 2D's
   `T2D-EGRAPH-EXTRACTION` + `T2D-CSP-SCOPE` is what 2C V3's
   "decision-engine facts" closure criterion names.

3. **2A V1 → V2 fold: fill in per-technique transfer table.** The
   four sonic-rs SIMD leaves (long string, float fraction, field lookup,
   whitespace) admit at the primitive layer per 2A defended assertion 3
   ("Targeted SIMD inside the parse envelope is grammar-neutral"). The
   per-grammar CSS L4 + Sheets + BBNF-self transfer verdict is currently
   posed as Open Research Question (`:124`) — promote to binding table
   matching 2C's "Per-Technique Transfer Coverage" template.

4. **2D V1 → V2 fold: fill in per-technique transfer table.** Per-grammar
   transfer for `SinkOnly` (Mison), `CollapsedStage` (asmjson / Sneller),
   `EagerTape`/`OffsetTape`/`EventTape` (simdjson tape class) across
   CSS L4 / Sheets / BBNF-self. The 2D V1 Open Research Question
   UNKNOWN-2D-01 — "Does `BackendExpr` have a stable grammar-neutral node
   vocabulary suitable for `egraph::Language`, without embedding JSON/CSS
   names?" — is the natural locus. Use 2C V3's table template.

5. **2B V1 → V2 fold (minor): explicitly classify the 6 aarch64
   `scalar-delegate-non-ASM` close states.** §A5 audit table at HEAD lists
   `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`
   (and three more) as `scalar-delegate-non-ASM` on aarch64. Record
   explicitly that abstract-primitive neutrality is preserved (the
   primitive name is the contract) but the M5 Max admission close state
   is `scalar-delegate-non-ASM` per Lock 16 v+1 vocabulary. This is not a
   CH2 leak — it is a CH2 partial-disclosure that V2 should make atomic.

## Disposition

No dossier is REJECTed on CH2. V1 contains real grammar-neutrality
evidence at HEAD and names the important leaks (`RuntimeProvider` 2→8
drift, `passes/src/lib.rs:1059/:1079/:1102` JSON-canonical labels,
`dispatch.rs:22-33` JSON-hardcoded Tbl4 constants, `JsonSink` callback
names, `OffsetFlags` HAS_ESC/HAS_CONTROL → GRAMMAR_BIT0/GRAMMAR_BIT1
partial repair). The cohort fails CH2 first-cycle convergence (4/6 vs
≥95% target) because 2A V1 and 2D V1 have not yet folded into 2C V3's
canonical transfer contract — they remain parallel rather than cited.

Required V2 fold: 2A V2 and 2D V2 cite 2C V3 V2-FOLD-ADDENDUM +
V3-FOLD-ADDENDUM as binding cross-dossier authority, and each fills in a
per-technique transfer table per the 2C "Per-Technique Transfer Coverage"
template. The five dispatch focus items remain ACCEPT at HEAD — they need
no V2 motion.

## Source Register

- 2A: `restart/audit/totality/p2/2A-sota-landscape.md` (cycle V1).
- 2B: `restart/audit/totality/p2/2B-primitive-vocabulary.md` (cycle V1).
- 2C: `restart/audit/totality/p2/2C-grammar-neutrality.md` (cycle V3, with V2-FOLD-ADDENDUM + V3-FOLD-ADDENDUM cross-references).
- 2D: `restart/audit/totality/p2/2D-cost-model.md` (cycle V1).
- 2E: `restart/audit/totality/p2/2E-host-arch-esoterica.md` (cycle V6).
- 2F: `restart/audit/totality/p2/2F-parse-that-gaps.md` (cycle V5).
- HEAD-verified path:lines: `skinny/crates/codegen/src/grammar_profile.rs:17-26,100-110`; `skinny/crates/passes/src/lib.rs:1053-1110`; `skinny/crates/bbnf-simd/src/dispatch.rs:18-40`; `skinny/crates/runtime/src/tape/mod.rs:18-36`; `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:30-44`; `skinny/crates/runtime/src/grammars/json/sink.rs:4-16` (per 2C citation).
- Dispatch: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md` (HEAD `8d5e4e8f6`).
- Lens spec: `restart/prompts/totality/PASS-2-RESEARCH.md` §3 CH2 (`:106-110`).
- Locks: `restart/locks/LOCKS.md` Lock 14 (`:220-260`) + Lock 14 v+1 (`:259-260`) + Lock 16 v+1 (`:282-360`).
- Repo HEAD at audit: `f25c3af2e`.
