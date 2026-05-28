# Pass Omega V9 Master-Plan / SK-V15 SPEC Diff

Disposition: proposed. Do not apply until Pass Omega V9 hardening converges and
G-Omega authorizes CRUD.

## Summary

`restart/MASTER-PLAN.md` should stop presenting SK-V14 W5B / Pass Omega V8 as
the active implementation receiver. The active receiver is the locked SK-V15
PRUNE-then-REBUILD W0-W11 graph in `restart/skinny/tranches/sk-v15/SPEC.md`.

`restart/skinny/tranches/sk-v15/SPEC.md` needs no text change from Omega-D V9.
It is the canonical source of truth. The proposed diff below updates MASTER to
point at that graph and records that real SK-V15 implementation waves start next
after Pass Omega V9 / G-Omega CRUD.

## MASTER-PLAN Exact Proposed Diff

```diff
diff --git a/restart/MASTER-PLAN.md b/restart/MASTER-PLAN.md
--- a/restart/MASTER-PLAN.md
+++ b/restart/MASTER-PLAN.md
@@
-### §13.3 SK-V14 W0..W11 Receiver Block (per MP-3B-V1-D02 + MP-NW-SK14-W0..W11-INHERIT)
+### §13.3 SK-V14 W0..W11 Receiver Block (historical/pre-block; superseded for active dispatch by §13.5)
+
+Pass Omega V9 supersession note: this section remains provenance and
+pre-block evidence for SK-V14/Omega V8, including the W5B-FRONTENDR ordering
+lessons. It is not the active implementation dispatch graph after SK-V15 S-P3
+V4 and T-P3 V5. Active dispatch authority moves to §13.5, which imports the
+locked SK-V15 W0-W11 PRUNE-then-REBUILD contract. No W5B, W5C, W5D, W6, W7,
+W8, W9, or W10 row in this historical block may be used to bypass SK-V15 W0,
+W1 CSS admission honesty, W2 Lock 14 / Lock 16 gate restoration, W5 CSS typed
+Value provider, W6 same-workload retime, W7-W9 Decision/lowerer activation, or
+W10 FNV quarantine.

 The SK-V14 SPEC §3-§14 12-wave W0..W11 plan is admitted as a MASTER-plan
 receiver block sibling to §13.2 (Pass Omega V1.1 Receiver Waves). SK-V14
@@
-### §13.4 New Waves From T-P3 V4 LOCK (14 NEW; per MP-NW-01..14)
+### §13.4 New Waves From T-P3 V4 LOCK (historical/pre-block; superseded for active dispatch by §13.5)
+
+Pass Omega V9 supersession note: the MP-NW-01..14 rows remain historical
+receiver/pre-block evidence. They no longer define the active skinny dispatch
+manifest. SK-V15 consumes their surviving constraints through §13.5 W0-W11 and
+the SK-V15 dependency rows: anti-broadcast telemetry, full-surface Lock 14 /
+Lock 16 gate restoration, CSS typed value rebuild, Pattern H generated
+provenance, Decision Engine activation, exact five-BackendShape lowerers,
+aarch64-only SIMD/primitive admission, and FNV quarantine.

 The 14 NEW waves added under T-P3 V4 LOCK consist of (a) the 12 V3-carried
 MP.NW0..MP.NW12 receiver waves preserved in §13.2 above, plus (b) two new
@@
 The MP-NW-SK14-W0..W11-INHERIT wave (admits the SK-V14 SPEC verbatim as a
 sibling receiver block) is materialized in §13.3 rather than duplicated here.
 The MP-NW-SK14-F-V2-P1ABC-RERECORD-STAGE-0 row also creates a
 PERMANENT-PRE-BLOCK refusal at §24 carry/friction ledger (per MP-3B-V1-D10);
 any future parse_only distinct path reorder inherits it.
+
+### §13.5 SK-V15 PRUNE-then-REBUILD Receiver Block (active after Pass Omega V9 / G-Omega)
+
+The locked SK-V15 implementation contract is
+`restart/skinny/tranches/sk-v15/SPEC.md` plus
+`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`. This MASTER section
+imports that graph as the active receiver block. It does not amend the SPEC and
+does not dispatch implementation by itself. After Pass Omega V9 convergence and
+G-Omega CRUD authorization, SK-V15 W0 is the next implementation wave.
+
+SK-V15 supersedes the SK-V14 W5B/Omega V8 active route because PASS-IMPL V1
+refuted CSS L4 admission as a generalization claim: JSON remains honest, while
+CSS L4 is contrived; Pattern H is not collapsed; Lock 14 / Lock 16 gates have
+holes; Decision Engine is scaffold; and the FNV closed-enum product must stay
+bench-only. The repair order is PRUNE before REBUILD. No stale CSS row,
+documentation-only proof, overfit comparator, x86/AVX-512 diagnostic, source
+inventory, or SK-V16 deferral can close SK-V15.
+
+Global SK-V15 gates:
+
+- Admission, SIMD, primitive, SOTA, and close evidence are Apple M5 Max /
+  aarch64 only. x86 and AVX-512 remain diagnostic.
+- Deep SIMD for Apple M5 Max is admissible only with scalar oracle, strict
+  parity/checkasm, same-wave consumer, and measured row movement. Source-present
+  primitives without consumers are manifest entries, not admits.
+- CSS must expose typed value, document, view, and visitor surfaces comparable
+  to JSON's Value API before old CSS proof can retire. W6 retimes on the same
+  typed workload against `cssparser`; `lightningcss` counts only after Track 1
+  emits comparable CSSOM/value output.
+- Pattern H remains exactly 67 root runtime files and every file must carry
+  true line-1 generated provenance backed by regeneration/check proof.
+- Decision Engine activation requires at least one e-graph rewrite, a
+  non-tautological CSP, grammar-neutral facts, and real lowerers for exactly
+  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`.
+- W11L/W11N/W11O FNV closed-enum products stay bench-only. Production FNV
+  selectors, arbiters, or correctness proofs are blocked.
+- W0-W11 consume the full wave ceiling. There is no W12, challenge-time
+  implementation overflow, or renewed Alpha/Omega planning loop before W0.
+
+| SK-V15 receiver | Status | Entry gate | Exit gate | MASTER allocation |
+|---|---|---|---|---|
+| MP.SK15.W0 Baseline and telemetry lock | active pending | S-P3 and G-Omega closed | Gate consumes SK-V15 telemetry; CSS broadcast evidence is diagnostic; no provider deletion. | H/J/BENCH current-state ledger. |
+| MP.SK15.W1 CSS admission honesty | active pending | W0 admitted | 24 CSS broadcast admits are demoted or collapsed; no W8R live admit remains. | H.W6 and J.W1 CSS truth repair. |
+| MP.SK15.W2 Lock 14 / Lock 16 gate restoration | active pending | W1 admitted or CSS blocked | Gates report roots, exclusions, and source-present primitive status; self-exemptions fail. | A.W4, H.W4.LOCK14, H.W2/H.W2.5, Lock 16. |
+| MP.SK15.W3 Codegen leak abrogation | active pending | W2 admitted | One coherent generic leak family is removed with same-wave generator consumer. | F.W3/F.W5, MP.NW6. |
+| MP.SK15.W4 Pattern H generated discipline | active pending | W2 and W3 admitted/routed | 67 root runtime files have true provenance and non-writing regeneration/check proof. | A/F Pattern H census and Lock 14 forward invariant. |
+| MP.SK15.W5 CSS typed Value provider | active pending | W1-W4 admitted/routed | Typed CSS value/document/view/visitor provider exists; old proof remains diagnostic. | G.W2/G.W3, H.W6, J.W1. |
+| MP.SK15.W6 CSS same-workload retime and old-proof retirement | active pending | W5 admitted | Fresh typed `cssparser` comparison sets any CSS floor; old CSS proof paths retire. | H.W6, J.W1, BENCH. |
+| MP.SK15.W7 Decision Engine spine | active pending | W6 admitted/routed | E-graph rewrite and non-tautological CSP are gate-consumed. | C.W4/C.W5, H.W4/H.W7, MP.NW8. |
+| MP.SK15.W8 BackendShape harness plus EagerTape/OffsetTape | active pending | W7 admitted | Harness rejects label scaffolds; EagerTape/OffsetTape emit runtime-relevant output. | E/F/H lowerer boundary. |
+| MP.SK15.W9 EventTape/SinkOnly/CollapsedStage plus all-five gate | active pending | W8 admitted | Remaining lowerers are real and all-five gate proves exactly five BackendShape variants. | H.W4/H.W7 and BackendShape canon. |
+| MP.SK15.W10 FNV quarantine | active pending | W9 admitted/routed | FNV stays bench-only; production FNV scan and adversarial fixtures are consumed. | J.W1/J.W5 and bench-only guard. |
+| MP.SK15.W11 Close and PASS-IMPL V2 handoff | active pending | W1-W10 resolved | PASS-IMPL V2 accepts each axis or records row-level intrinsic-block proof at HEAD. | J.W5 and Master Close. |
+
+Dependency rows are inherited from SK-V15 SPEC §2.1: `DEP-W1-CSS-BROADCAST`,
+`DEP-W6-CSS-GENERATED-RS`, `DEP-W6-CSS-SUMMARY-FACT-STREAM`,
+`DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, `DEP-W4-PATTERN-H-PROVENANCE`,
+`DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM`, `DEP-W7-DECISION-SPINE`,
+`DEP-W8-LOWERERS-A`, `DEP-W9-LOWERERS-B`, `DEP-W10-FNV-QUARANTINE`, and
+`DEP-W11-CLOSE-NO-ORPHANS`. Missing dependency proof blocks the consuming
+exit gate; it does not route to SK-V16 as close evidence.

 ## 14. Tranche I - Recovery, Incremental, LSP
@@
 ## 25. Implementation Order

 The implementation order is:

 1. Commit Phase 2 synthesis outputs.
-2. Complete Pass Omega convergence and G-Omega before applying any V1.1
-   MASTER/LOCKS/HANDOFF/MIGRATION/SKINNY corpus amendments or dispatching
-   SK-V13 W0.
+2. Complete Pass Omega V9 convergence and G-Omega before applying any V1.1
+   MASTER/LOCKS/HANDOFF/MIGRATION/SKINNY corpus amendments or dispatching
+   SK-V15 W0. After authorized CRUD, execute the real SK-V15 W0-W11
+   implementation wave program next; do not insert another Alpha/Omega
+   planning loop unless a new hardening finding explicitly blocks W0.
 3. Start tranche A from a clean worktree.
 4. Archive `ser` and `gorgeous`.
 5. Create the 24-crate skeleton and metadata validator.
@@
-Those documents are inputs. SK-V13 source/generated/gate/result waves remain
-blocked until Pass Omega convergence, CRUD authorization, and G-Omega.
+Those documents are inputs. SK-V15 W0-W11 source/generated/gate/result waves
+remain blocked until Pass Omega V9 convergence, CRUD authorization, and
+G-Omega. Once authorized, W0 is first; W11 prepares SK-V16 input only after
+PASS-IMPL V2 accepts every axis or records row-level intrinsic-block proof.
```

## SK-V15 SPEC Proposed Diff

No text change is proposed for `restart/skinny/tranches/sk-v15/SPEC.md`.

Reason: the SPEC is already locked at S-P3 V4 and carries the W0-W11 graph,
global close condition, dependency rows, non-JSON receiver matrix, Lock 14 /
Lock 16 gates, CSS typed provider and retime split, Decision/lowerer split,
FNV quarantine, and W11 PASS-IMPL V2 close. Omega-D V9 only updates MASTER to
recognize that locked SPEC as the active authority.

```diff
diff --git a/restart/skinny/tranches/sk-v15/SPEC.md b/restart/skinny/tranches/sk-v15/SPEC.md
--- a/restart/skinny/tranches/sk-v15/SPEC.md
+++ b/restart/skinny/tranches/sk-v15/SPEC.md
@@
+# no Omega-D V9 changes
```

## CRUD Notes

- Apply the MASTER patch only after G-Omega V9 authorization.
- Do not edit SK-V15 SPEC during CRUD-2 for this reconciliation.
- Do not stage or commit as part of Omega-D worker output.
- Preserve unrelated dirty implementation files in the worktree.
