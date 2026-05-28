# T-P2 V2 Fold Addendum — SK-V15

Pass: T-P2 Research.
Cycle: V2.
Date: 2026-05-28.
Authority: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`
at commit `8209c7399`.

The prior `T-P2-V2-FOLD-ADDENDUM.md`, `T-P2-V3-FOLD-ADDENDUM.md`, and
`T-P2-V4-FOLD-ADDENDUM.md` were SK-V14 lineage. Git history preserves them;
the live SK-V15 fold authority is this file only.

## V1 Verdict Consumed

T-P2 V1 hardening returned `REVISE`.

| lens | disposition | V2 obligation |
|---|---|---|
| CH1 correctness | REVISE | Repair the broken simdjson tape URL in 2D and record the source check. |
| CH2 generality | ACCEPT | Preserve grammar-neutrality, no new directive, no new BIR, no sixth BackendShape. |
| CH3 regression | REVISE | Fence retained sidecar/cursor/list/class-column routes as REDRESS-refuted in 2A. |
| CH4 cost | REVISE | Add uniform cost/admission manifest fields to all grounded primitive and rebuild routes. |
| CH5 hidden coupling | ACCEPT | Preserve substrate, sidecar, broadcast, grammar-switch, and BackendShape fences. |
| CH6 anti-paper-close | REVISE | Add inline transfer reason, admission gate, verification action, and close status to grounded rows. |
| CH7 overfit-prune | ACCEPT | Preserve broadcast, gate-exclusion, CSS fake-parity, x86-close, and delete-before-provider guards. |

## Uniform V2 Row Shape

Every row whose state includes `grounded`, `partial`, or
`architecture-pressure` must be able to stand alone. Use these fields either
as explicit table columns or as a compact per-row suffix:

```text
transfer_reason
admission_gate
verification_action
close_status
loc_estimate
risk_class
wave_owner
hard_cap_fit
```

Allowed `close_status` values:

```text
admissible-after-gate
diagnostic-only
partial-blocked
source-present-unwired
scalar-delegated
blocked
refuted
```

For SIMD/ASM/primitive rows, the admission gate must also name:

```text
scalar_reference
parity_or_checkasm
hardware_gate
same_wave_consumer
row_movement_target
```

Rows may be concise. The fold requirement is not long prose; it is that a
future consumer cannot cite a source row without also carrying the gate that
prevents paper-close.

## Dossier-Specific Fold Requirements

### 2A — SOTA Parsing Landscape

1. Update `cycle: V2`.
2. Replace the 2A LAC-04 wording so retained cursor/list/class-column/sidecar
   routes are REDRESS-refuted for SK-V15. Allowed shape: transient same-loop
   masks consumed into the existing substrate, or generated single-substrate
   consumption. Any retained sidecar-like route requires a new Alpha/P1/SPEC
   contract.
3. Add inline row gates to SOTA rows: comparator plane, row-local
   equality/timing, host close route, close status, LOC/risk/wave owner where
   a transferred parser primitive is proposed.

### 2B — Primitive Vocabulary

1. Update `cycle: V2`.
2. Preserve the scalar/checkasm/hardware/consumer/row-movement manifest.
3. Add LOC estimate, risk class, wave owner, and hard-cap fit to each primitive
   or macro-family route.
4. Preserve the x86 diagnostic-only and source-inventory-not-admission fences.

### 2C — Grammar Neutrality

1. Update `cycle: V2`.
2. Add uniform row-shape fields to generated provider manifest, CSS typed value
   API, Pattern H provenance, full-surface Lock 14 scan, Sheets, BBNF-self,
   and future grammar onboarding rows.
3. Cost generated provider, Pattern H provenance, CSS typed provider, and Lock
   14 scan routes separately; no bulk "generated provider" rewrite may stand
   without owner, LOC/risk, and hard-cap fit.

### 2D — Cost Model

1. Update `cycle: V2`.
2. Replace or remove `https://simdjson.github.io/simdjson/md_doc_tape.html`;
   record the URL check or replacement source.
3. Split Decision Engine and BackendShape lowerer work into W7/W8/W9 units.
   Each unit states LOC estimate, risk, wave owner, hard-cap fit, admission
   gate, verification action, and close status.
4. Preserve the five-shape BackendShape canon; do not propose a sixth shape.

### 2E — Host-Arch Esoterica

1. Update `cycle: V2`.
2. Preserve Apple M5 Max / aarch64 as the only close route.
3. Add LOC estimate, risk class, wave owner, and hard-cap fit to each host
   primitive row.
4. Preserve refutations for NEON `svmatch_u8`, AVX-512 close evidence, and
   PMULL/CSSC promotion from ISA/checkasm alone.

### 2F — parse-that Primitive Gaps

1. Update `cycle: V2`.
2. Add per-gap owner, scalar oracle, checkasm/parity command, hardware gate,
   same-wave consumer, row-local movement target, LOC/risk, wave owner,
   hard-cap fit, verification action, and close status.
3. Keep CSS value parsing assigned to a generated CSS typed provider, not JSON
   string/number semantics.
4. Preserve runtime-regex import as blocked absent a named generated-runtime
   consumer plus CH3/CH5 review.

## Non-Regression Guards

V2 must not weaken these V1 ACCEPT surfaces:

- no CSS 24-row broadcast admission;
- no `CssFullParseSummary`, fact-stream string, or brace-counter CSS Value/API
  parity;
- no self-excluding Lock 14 / Lock 16 gate;
- no retained sidecar or public substrate expansion;
- no new directive, new BIR variant, or sixth `BackendShape`;
- no x86 / AVX-512 M5 Max close evidence;
- no deletion or retirement before replacement provider / same-wave consumer
  proof.

## V2 Close Expectation

After the six dossiers fold these requirements, commit the V2 packet and
dispatch fresh CH1-CH7 under `restart/audit/totality/p2/hardening/V2/`.
