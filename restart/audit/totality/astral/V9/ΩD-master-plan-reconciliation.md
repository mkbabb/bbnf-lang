# Omega-D Master-Plan Reconciliation - Pass Omega V9 SK-V15 Authority

Date: 2026-05-28.
Scope: `restart/MASTER-PLAN.md` Section 13 / active tranche reconciliation
against the locked SK-V15 Pass Alpha, S-P0, S-P1, S-P2, S-P3, T-P1, T-P2, and
T-P3 packets.
Disposition: ACCEPT-WITH-PATCH.

## Verdict

`restart/MASTER-PLAN.md` is stale as an active dispatch surface. It still routes
the H-tranche receiver through SK-V14 W5B / Pass Omega V8 authority, while the
current locked implementation contract is SK-V15 W0-W11:

```text
W0 baseline/telemetry
  -> W1 CSS admission honesty
  -> W2 Lock 14 / Lock 16 gate restoration
  -> W3 codegen leak abrogation
  -> W4 Pattern H generated discipline
  -> W5 CSS typed Value provider
  -> W6 CSS same-workload retime and old-proof retirement
  -> W7 Decision Engine spine
  -> W8 EagerTape/OffsetTape lowerers
  -> W9 EventTape/SinkOnly/CollapsedStage all-five gate
  -> W10 FNV quarantine
  -> W11 close and PASS-IMPL V2 handoff
```

The patch should not edit SK-V15 `SPEC.md`. The SPEC is already the locked
source of truth. The required update is a MASTER-PLAN authority repair: mark the
SK-V14 / MP-NW blocks as historical/pre-block evidence, add an active SK-V15
receiver block, and update implementation order so real SK-V15 implementation
waves start next after Pass Omega V9 / G-Omega CRUD. No additional Alpha/Omega
planning loop is required by this reconciliation.

## Authority Chain

| Layer | Closure state consumed | Reconciliation consequence |
|---|---|---|
| PASS-IMPL V1 | JSON honest; CSS L4 contrived; Pattern H not collapsed; Lock 14 holes; Decision Engine scaffold; no CSS Value API. | CSS admits, fake generated CSS, brace counters, and scaffold close routes are rejected as active MASTER proof. |
| Pass Alpha | V1 hardening folded Alpha-scope revisions; S-P0 must consume PASS-IMPL and S-P3 must expand exact waves. | Alpha is not a blocker; its constraints are already in SK-V15. |
| S-P0 | FAIL / PRUNE-REQUIRED; receiver buckets A-G preserved. | MASTER must route PRUNE before REBUILD and keep CSS old proof diagnostic. |
| S-P1 | LOCKED after two ACCEPT cycles. | Empirical profiling floor is available; it does not authorize stale CSS or primitive admits. |
| S-P2 | LOCKED at V3, 7/7 ACCEPT; aarch64/native host and primitive survivor boundaries locked. | SIMD/primitive rows become Apple M5 Max / aarch64-only gates with scalar/parity/same-wave consumer requirements. |
| S-P3 | LOCKED at V4, 7/7 ACCEPT; W0-W11 dispatch contract locked. | MASTER should import W0-W11 as the active receiver block. |
| T-P1 | Clean-final / G1-auto-pinned, not normal two-cycle 3Z; open work preserved. | Governance note must be recorded; do not rewrite history as normal T-P1 lock. |
| T-P2 | Normal 3Z lock at V3. | Research conclusions are stable inputs for MASTER. |
| T-P3 | Final-convergence V5 lock; Pass Omega V9 is next. | This Omega-D output proposes CRUD input only; no live surface is edited here. |

## Reconciliation Findings

| MASTER surface | Current state | Required V9 disposition |
|---|---|---|
| Section 13.3 SK-V14 W0-W11 | Active-looking receiver block with V8 W5B-FRONTENDR authority. | Keep as historical/pre-block evidence only; no current dispatch may route through it. |
| Section 13.4 MP-NW-01..14 | Older T-P3 V4 / SK-V13-SK-V14 receiver map. | Keep as historical/pre-block evidence except refusal rows; superseded for active dispatch by SK-V15. |
| Section 13 H.W6 / MP.NW2..4 / J.W1 CSS language | Old CSS feature expansion and lightningcss framing can be read as live admit route. | Route CSS through W1 diagnostic demotion, W5 typed Value provider, and W6 same-workload cssparser retime. Lightningcss counts only after comparable CSSOM/value output exists. |
| H.W2 / H.W2.5 / Lock 16 allowlist | Source-present primitive inventory and deep SIMD research coexist with older admission wording. | W2 reports all primitives; no SIMD/ASM body admits without Apple M5 Max / aarch64 evidence, scalar oracle, strict parity/checkasm, same-wave consumer, and row movement. x86/AVX-512 is diagnostic only. |
| Pattern H census | MASTER preserves 67-file count, but active close still sits in old PRUNE lineage. | W4 owns true line-1 generated provenance for all 67 root runtime files plus regeneration/check proof. Header-only or fake generated status is rejected. |
| Decision Engine | MP.NW8 / C.W4-C.W5 language still reads as replacement work, but implementation audit says scaffold. | W7 activates nonzero e-graph/CSP spine; W8/W9 implement lowerers and all-five gate. No scaffold, label-string, sidecar, sixth-shape, or advisory-only close. |
| FNV | Bench-only flag appears in audit and T-P packets, but MASTER needs active quarantine. | W10 owns FNV quarantine, production scan, and adversarial strict-product fixtures; no production FNV arbiter or correctness proof. |
| Section 25 implementation order | Still says Pass Omega/G-Omega before SK-V13 W0. | Replace with Pass Omega V9 / G-Omega before SK-V15 W0; after CRUD, execute real W0-W11 implementation waves next. |

## Active SK-V15 Receiver Block

Pass Omega V9 should add a new Section 13.5 after the existing Section 13.4.
The new block is a MASTER receiver block, not a second SPEC. It binds the
existing locked SK-V15 SPEC graph and carries these non-negotiables:

- PRUNE precedes REBUILD: W1-W4 must route/demote/restore gates before W5-W10
  can claim rebuild evidence.
- No implementation wave may close by deferral to SK-V16, stale SK-V14 proof,
  documentation-only evidence, or another Omega/Alpha churn cycle.
- All admission evidence is Apple M5 Max / aarch64. Deep SIMD is allowed only
  as host-native work with scalar oracle, strict parity/checkasm, same-wave
  consumer, and measured row movement.
- CSS must expose typed value/document/view/visitor surfaces before old CSS
  proof is retired; W6 retimes on the same typed workload against `cssparser`.
- Pattern H generated provenance is real generator provenance over exactly 67
  root runtime files, not fake headers or relocated hand-written code.
- Decision Engine activation means at least one e-graph rewrite, a
  non-tautological CSP, grammar-neutral facts, and real lowerers for exactly
  the five existing `BackendShape` variants.
- FNV closed-enum products remain bench-only; production FNV selection,
  arbitration, or correctness proof is blocked.

## Proposed Patch Summary

The exact proposed text is filed in
`restart/audit/totality/astral/V9/master-plan-diff.md`.

Apply only after Pass Omega V9 hardening and G-Omega authorization:

1. Add a short supersession note at the top of MASTER Section 13.3.
2. Add a short supersession note at the top of MASTER Section 13.4.
3. Insert new MASTER Section 13.5, `SK-V15 PRUNE-then-REBUILD Receiver Block`.
4. Update MASTER Section 25 item 2 and its blocking paragraph from SK-V13 to
   SK-V15 W0-W11.
5. Leave `restart/skinny/tranches/sk-v15/SPEC.md` unchanged; it is the locked
   source of truth consumed by the new MASTER block.

## Refused Routes

- No CSS 24-row broadcast admission.
- No `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, or
  brace-counter CSS proof as live admission.
- No delete-before-provider, header-only Pattern H close, or fake generated
  provenance.
- No gate exclusions hidden from Lock 14 / Lock 16 reports.
- No source-present SIMD/ASM primitive admission without consumer and row
  movement.
- No x86/AVX-512 close route for SK-V15.
- No PMULL/CSSC/checkasm-only hot-body promotion.
- No public `UnionTape`, second tape, retained stream, sixth `BackendShape`,
  sidecar EventTape, or grammar-named Decision facts.
- No production FNV arbiter, runtime selector, or hash correctness proof.
- No W12, challenge-time implementation overflow, SK-V16 deferral as close
  evidence, or renewed planning loop before W0.

## Close State

Omega-D V9 is ready for CHALLENGE review. The required live edits are limited to
MASTER-PLAN CRUD after G-Omega; SK-V15 SPEC is already locked and should remain
the active canonical wave graph.
