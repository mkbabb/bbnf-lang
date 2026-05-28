# Omega-F Migration And Handoff - Pass Omega V9 SK-V15 Totality Closure

Date: 2026-05-28.
Scope: proposal-only migration, handoff, and next-cycle dispatch directive after
SK-V15 totality T-P1/T-P2/T-P3 closure.
Disposition: ACCEPT-WITH-PATCH, G-Omega gated.

## Verdict

The top-level restart surfaces are stale. `restart/HANDOFF.md` and
`restart/MIGRATION.md` still present SK-V14 W5B-FRONTENDR / Pass Omega V8 as
current implementation authority, while SK-V15 has a locked W0-W11
implementation contract and a converged totality packet. Omega-F V9 should
route CRUD-4 to replace the current top-level handoff/migration authority with
SK-V15 implementation authority.

This artifact does not edit live V1 surfaces. It proposes the HANDOFF and
MIGRATION updates that CRUD-4 should apply after Pass Omega V9 CHALLENGE
convergence and mandatory G-Omega V9 authorization.

The next executable sequence is:

1. Pass Omega V9 converges and presents G-Omega V9.
2. After G-Omega V9 closes, apply the authorized V1 corpus CRUD patches;
   SK-V15 SPEC/DISPATCH stay read-only for V9.
3. Stop Omega/Alpha churn for this SK-V15 implementation authority.
4. Dispatch actual SK-V15 implementation waves W0 through W11 in order through
   the SKINNY triumvirate, with G-Omega as the only mandatory user gate and all
   other gates auto-passing under the active pin.

No contrivances, no workarounds, no deferrals, no doc-only closes, and no
implementation-limited misses may close SK-V15. Admission evidence is Apple M5
Max / aarch64 only. x86 and AVX/AVX-512 are diagnostic only. Deep SIMD work is
allowed only with scalar oracle, parity/checkasm or equivalent, same-wave
consumer, and row movement.

## Inputs Consumed

Primary current-state inputs:

- `restart/HANDOFF.md`
- `restart/MIGRATION.md`
- `restart/prompts/pass-contracts/PASS-OMEGA.md`
- `restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md`
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
- `restart/skinny/tranches/sk-v15/{SYNTHESIS,HANDOFF,SPEC,DISPATCH-PROMPT}.md`
- `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`
- `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md`
- `restart/audit/totality/p3/3F-migration-handoff.md`
- `restart/audit/totality/astral/V8/{G-OMEGA-PACKET,CRUD-LOG,G-OMEGA-SIGNOFF,ΩF-migration-handoff}.md`

Governance facts to preserve:

| Surface | Current fact |
|---|---|
| T-P1 | CLEAN-FINAL / G1-AUTO-PINNED, not normal two-clean-cycle 3Z. V5 closed every known evidence defect at the pass ceiling. |
| T-P2 | Normal 3Z LOCK / G2-AUTO-PASSED from two consecutive clean cycles, V2 and V3. |
| T-P3 | Final convergence at V5: V3 clean substantive cycle plus V5 all-ACCEPT confirmation after a V4 citation-only repair. G3 auto-passes; G-Omega V9 is next mandatory gate. |
| SK-V15 skinny | S-P3 V4 locked `SPEC.md` and `DISPATCH-PROMPT.md` as the W0-W11 implementation contract. |
| PASS-IMPL V1 | JSON is honest; CSS L4 is contrived; Pattern H is not collapsed; Lock 14/16 gates have holes; Decision Engine is scaffold. |

## Migration Impact

Omega-F V9 is a document-authority migration. It does not directly authorize
source edits, generated output movement, `RESULTS.md`, `REDRESS.md`, gate
implementation changes, or runtime deletion. Those belong to SK-V15 W0-W11
after G-Omega V9 and authorized V1 corpus CRUD application.

### Proposed MIGRATION Receiver

CRUD-4 should insert a current SK-V15 receiver before the historical Pass
Omega V2..V8 receivers in `restart/MIGRATION.md`. Existing V2..V8 sections
remain historical SK-V14 correction records, not current dispatch authority.

| Receiver | Migration rule | Blocker | Gate |
|---|---|---|---|
| Current SK-V15 authority | SK-V15 W0-W11 supersedes stale SK-V14 W5B.0 dispatch text as the current implementation route. | Any text still naming SK-V14 W5B.0 as next dispatch. | G-Omega V9 plus CRUD-4. |
| W0 baseline/telemetry | Capture `SK-V15-open`; JSON 51/51 guard remains strict; CSS broadcast evidence is diagnostic only. | Missing gate-consumed SK-V15 telemetry. | W0 exit gate. |
| W1 CSS honesty | Demote or collapse the 24-row CSS broadcast; no live CSS admit from the W8R shared tuple. | Reused broadcast measurement as admit evidence. | `DEP-W1-CSS-BROADCAST`. |
| W2 Lock 14 / Lock 16 restoration | Gates scan previously excluded roots, report their exclusions, and classify source-present primitives. | Self-exempting scan or source-present primitive with no status. | W2 exit gate. |
| W3 codegen leaks | Remove one coherent generic leak family with same-wave generator/check consumer and non-JSON receiver proof. | Grammar-family branch, profile roster, or JSON/CSS recognizer left in generic path. | `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`. |
| W4 Pattern H provenance | Keep Pattern H at 67 root runtime files while proving true generated provenance at line 1 with regen/check evidence. | Header-only generated claim or destructive delete without provider proof. | `DEP-W4-PATTERN-H-PROVENANCE`. |
| W5 CSS typed provider | Build typed CSS value/document/view/visitor capability comparable to JSON; old CSS proof remains diagnostic. | Fact-stream-only or brace-counter path treated as live proof. | W5 exit gate. |
| W6 CSS retime and old-proof retirement | Retime against same-workload typed `cssparser`; retire `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter proof from live admission. | CSS floor based on W8R, lightningcss wrong-plane proof, or missing typed comparator. | `DEP-W6-CSS-GENERATED-RS` and `DEP-W6-CSS-SUMMARY-FACT-STREAM`. |
| W7 Decision Engine spine | E-graph rewrite count is nonzero, CSP is non-tautological, and grammar-named facts are removed. | Zero-rule e-graph, marker CSP, or advisory-only cost facts. | `DEP-W7-DECISION-SPINE`. |
| W8/W9 BackendShape lowerers | EagerTape, OffsetTape, EventTape, SinkOnly, and CollapsedStage lowerers are real or gate-rejected; exactly five shapes remain. | Label-string lowerer, pass-through scaffold, sixth shape, sidecar EventTape. | `DEP-W8-LOWERERS-A` and `DEP-W9-LOWERERS-B`. |
| W10 FNV quarantine | FNV closed-enum products stay bench-only and cannot migrate into production correctness proof. | Production FNV arbiter, production hash correctness proof, or runtime leakage. | `DEP-W10-FNV-QUARANTINE`. |
| W11 close | PASS-IMPL V2 consumes every dependency row and accepts every axis or records row-level intrinsic-block proof. | Any orphan dependency row, doc-only close, or implementation-limited miss. | `DEP-W11-CLOSE-NO-ORPHANS`. |

Migration gate clause to add:

> No delete, retirement, provider/template removal, old CSS proof retirement,
> runtime-shim deletion, primitive promotion, SIMD/ASM admission, or lowerer
> close may proceed unless the SK-V15 `SPEC.md` dependency row proves the
> rebuild provider lands no later than the delete/retire wave, or the row is
> explicitly diagnostic-demotion-only.

### Proposed HANDOFF Replacement

CRUD-4 should replace the top-level "Current Totality Override" and the
"Pass Omega V8 next-cycle dispatch directive" with SK-V15 current authority.
The replacement should make these points load-bearing:

- SK-V14 row-ledger close is historical; PASS-IMPL V1 reclassified CSS L4 and
  generic infrastructure as open implementation truth.
- SK-V15 is the current tranche.
- S-P3 V4 locked `restart/skinny/tranches/sk-v15/SPEC.md` and
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`.
- T-P1/T-P2/T-P3 governance must be stated honestly as above.
- G-Omega remains mandatory. G1, G2, G3, G-Alpha, challenge confirmation gates,
  and wave-internal gates auto-pass unless an unrepaired invariant violation
  or REDRESS route requires G-Omega.
- Current dispatch authority is W0, not SK-V14 W5B.0.
- The implementation sequence is W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 ->
  W7 -> W8 -> W9 -> W10 -> W11. No W12 escape hatch.

Proposed top-level blocker matrix:

| Blocker | Receiver | Dispatch consequence |
|---|---|---|
| CSS 24-row broadcast | W1 | CSS rows are diagnostic until W1 demotes/collapses shared evidence. |
| `CSS_GENERATED_RS`, brace-counter summary, fact-stream-only CSS parse | W5/W6 | Old proof cannot be live after W6 and cannot retire before typed provider proof. |
| Lock 14 / Lock 16 scan exclusions | W2 | Gate exclusions must be reported and consumed; self-exemptions fail. |
| Codegen grammar-family branches | W3 | Generic codegen and passes must stop branching on JSON/CSS families. |
| Pattern H 0/67 generated provenance | W4 | Header-only generated status is rejected; 67-file count stays fixed unless G-Omega amends. |
| Decision Engine scaffold | W7 | No load-bearing claim until rewrite/CSP/fact changes affect selection. |
| BackendShape label-string lowerers | W8/W9 | Exactly five shape lowerers must become real or gate-rejected. |
| FNV closed-enum bench products | W10 | Bench-only quarantine; no production FNV arbiter or hash correctness proof. |
| Dependency-row orphan | W11 | SK-V15 cannot close while any dependency row lacks proof, revert, REDRESS, or intrinsic-block evidence. |

## Correctness Gate Fold

Authorized V1 corpus CRUD patches after G-Omega V9 should preserve these exact
guardrails:

- Apply only the V9 authorized surface patches before W0.
- Do not edit source, generated output, `RESULTS.md`, `REDRESS.md`, or gates
  during Omega-F itself.
- Do not dispatch W0 until HANDOFF/MIGRATION stop routing current work through
  SK-V14 W5B.0/Omega V8.
- Do not run another Alpha or Omega loop as routine churn after G-Omega V9.
  Additional Omega is justified only by a concrete REDRESS/invariant issue that
  needs a V1 surface, LOCKS, or wave-graph amendment.
- Do not use SK-V16 routing as SK-V15 close evidence. SK-V16 receives proven
  remainder only after W11/PASS-IMPL V2.
- Preserve 16 locks and the exact five-shape `BackendShape` canon unless
  G-Omega explicitly authorizes a locks amendment. FactStream remains a Lock 1
  substrate-manifest category, not a sixth shape.
- Preserve Pattern H census discipline: current baseline is 67 root runtime
  files; SK-V15 W4 proves true generated provenance rather than shrinking the
  count by undocumented deletion.

## G-Omega V9 And CRUD Implications

G-Omega V9 is mandatory before any V9 patches make the top-level V1 surfaces
authoritative. Until G-Omega V9 closes, this Omega-F file is proposal-only.

Proposed CRUD receiver map after G-Omega V9:

| CRUD | Surface | V9 operation |
|---|---|---|
| CRUD-1 ARCHITECTURE | `restart/ARCHITECTURE.md` | Apply only authorized implementation-status alignment if Omega-A/Omega-D require it. Preserve no new substrate, public API, directive, BIR variant, or `BackendShape` variant unless separately authorized. |
| CRUD-2 MASTER-PLAN | `restart/MASTER-PLAN.md` | Mark SK-V15 W0-W11 as current implementation route; ensure W0 is first dispatch after G-Omega and no W12 overflow exists. SK-V15 SPEC/DISPATCH are read-only for V9. |
| CRUD-3 LOCKS | `restart/locks/LOCKS.md` | G-Omega-gated. Preserve 16 locks and five-shape canon unless Omega-C V9 locks diff authorizes a concrete amendment. |
| CRUD-4 HANDOFF + MIGRATION | `restart/HANDOFF.md`, `restart/MIGRATION.md` | Replace stale SK-V14 W5B/Omega V8 current authority with SK-V15 W0-W11 implementation authority and the blocker/receiver/gate tables above. |
| CRUD-5 SKINNY CORPUS | `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER,BENCH,SUBSTRATE}.md` | Align only where these surfaces still route current work through SK-V14 or omit SK-V15 overfit-prune constraints. |
| CRUD-6 AUDIT + CLEANUP | `restart/audit/totality/astral/V9/{CRUD-LOG,G-OMEGA-SIGNOFF}.md` | Record V9 authorization, patch scope, and verification that Omega-F itself did not move live source/generated/results/redress surfaces. |

## Handoff Directive

After Pass Omega V9 CHALLENGE convergence:

1. Present G-Omega V9 with the cycle summary, consolidated verdict, locks diff,
   master-plan edit operations, and CRUD operation list.
2. If G-Omega V9 is held, stop at the gate.
3. If G-Omega V9 authorizes, apply the authorized V1 corpus CRUD patches
   exactly as authorized; do not edit SK-V15 SPEC/DISPATCH.
4. Verify top-level HANDOFF/MIGRATION now route current work to SK-V15 W0-W11,
   not SK-V14 W5B.0 or Omega V8.
5. Stop Omega/Alpha churn for this SK-V15 implementation authority.
6. Dispatch SK-V15 W0 through `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`.
7. Continue W1 through W11 in strict SPEC order, using the research -> plan ->
   redress triumvirate for every wave.
8. Prune aggressively: demote false admits, delete or quarantine contrivances
   only after provider proof, and route misses to REDRESS/revert/intrinsic-block
   evidence immediately.
9. Maintain Apple M5 Max / aarch64-only admission evidence. Treat x86,
   AVX/AVX-512, stale sidecars, wrong-output-plane comparators, and warm benches
   as diagnostics only.
10. Do not close SK-V15 until W11 plus PASS-IMPL V2 accepts every axis or records
    row-level intrinsic-block proof at HEAD.

## Refusal Conditions

CRUD-4 should fail closed if any of these remains true after the proposed patch:

- `restart/HANDOFF.md` still says the next implementation move is SK-V14
  W5B.0 LOCK14-GATE.
- `restart/MIGRATION.md` has no current SK-V15 receiver before the historical
  SK-V14 V2..V8 receivers.
- G-Omega V9 is bypassed or treated as optional.
- G-Alpha, G1, G2, G3, or wave confirmation gates are treated as mandatory user
  stops under the active pin.
- Old CSS broadcast, brace-counter, fact-stream-only, or `CSS_GENERATED_RS`
  proof can still close CSS admission.
- A delete/retire wave can run before rebuild-provider proof.
- Production FNV, retained sidecars, second tapes, public substrate APIs, a
  sixth `BackendShape`, or Track 1 == Track 2 sidecar equality can re-enter.
- W11 can close with an orphan dependency row, documentation-only evidence, or
  a planned SK-V16 handoff substituting for SK-V15 proof.

## Open Questions

None blocking Omega-F V9. The historical Pass Omega V5 W5R name collision is
resolved by naming the current cycle "Pass Omega V9" and leaving historical
V5 W5R sections explicitly historical.
