# Pass Omega V1 Proposed Master-Plan Diff

Status: proposed patch-style document only. Do not apply during Ω-D. `restart/MASTER-PLAN.md` remains untouched until Pass Omega convergence, CHALLENGE acceptance, CRUD authorization, and G-Omega.

```diff
diff --git a/restart/MASTER-PLAN.md b/restart/MASTER-PLAN.md
--- a/restart/MASTER-PLAN.md
+++ b/restart/MASTER-PLAN.md
@@ restart/MASTER-PLAN.md:189-202 @@
 | Tranche | Title | Stub waves | Primary close gate |
 |---|---:|---:|---|
 ...
-| H | Pratt, SIMD | 5 | Auto-detected Pratt/SIMD pass early SOTA gates on the Rust line. WASM defers post-V1 via `WasmBackend: Backend` per `restart/ARCHITECTURE.md` §7.5. |
+| H | Pratt, SIMD, typed-event codegen, full-SOTA receivers | 10 current rows + V1.1 receiver waves | Auto-detected Pratt/SIMD pass, typed-event/direct row-plane accounting, CSS parity, JSON 51-row strict matrix, decision-engine fold, union material-differential/block, zero-orphan primitive discipline, and no-demotion gates on the Rust line. WASM defers post-V1 via `WasmBackend: Backend` per `restart/ARCHITECTURE.md` §7.5. |
 ...
 The counts are planning stubs. Full wave docs are not part of Phase 2.
+
+Pass Omega V1.1 reconciliation note: the A-J table remains a planning census, but §13 already has ten concrete H rows (`H.W0`, `H.W1`, `H.W2`, `H.W2.5`, `H.W3`, `H.W4`, `H.W4.LOCK14`, `H.W5`, `H.W6`, `H.W7`). The current concrete MASTER census is 59 rows: A5 + B5 + C6 + D6 + E5 + F6 + G5 + H10 + I5 + J6. Scoped skinny landings are not V1/root/campaign close unless the row text says so. Evidence: `restart/MASTER-PLAN.md:524-535`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:34-41`.
```

```diff
@@ restart/MASTER-PLAN.md:489-535 @@
 ## 13. Tranche H - Pratt, SIMD, typed-event codegen

+Pass Omega V1.1 proposed status ledger:
+
+| Wave | V1.1 proposed status | Receiver |
+|---|---|---|
+| H.W0 | LANDED-SCOPED: preflight, Plan D capacity, and escape-mask correctness prerequisite landed; no throughput row admitted by this alone. | Lock 15/16 evidence and S-P3 primitive guards. |
+| H.W1 | PARTIAL: Rust-state substrate/backend-shape derivation landed (`603308b3`), throughput recovery pending. | H/J benchmark rows and S-P3 row movement. |
+| H.W2 | PARTIAL: consumed primitive subset admitted; new primitives require scalar parity, checkasm, same-wave consumer, and measured row movement. | Lock 16 manifest and primitive ledger. |
+| H.W2.5 | PARTIAL: primitive vocabulary exists; contract-only macros stay non-admitting until consumed or deleted/demoted/blocked. | Primitive state machine and zero-orphan gate. |
+| H.W3 | SPLIT: number materialization landed; UTF-8/string fusion is refuted as a close route. | Regex/HIR facts and string/Unicode row gates. |
+| H.W4 | PARTIAL: SinkOnly/direct/typed row-plane accounting remains open under SK-V13. | JSON 51-row strict sonic matrix. |
+| H.W4.LOCK14 | PARTIAL: GrammarConfig legality is evidence, not full grammar-neutral repair. | Generated provider/config/sink repair plus Sheets/BBNF-self negative controls. |
+| H.W5 | LANDED-SCOPED: consumed arm64/generic set landed; x86 successor optional/background; no-orphan rule mandatory. | Lock 16 and SK-V13 G4. |
+| H.W6 | REPLACE CURRENT WORDING: SK-V6 strict-matrix-before-CSS text is superseded by the SK-V13 full-SOTA receiver map. | SK-V13 G1-G7 and J.W1. |
+| H.W7 | PENDING: Pratt recognizer facts and BIR `PrattSpine` still depend on C/E fact and BIR closure. | C.W3/C.W4/E.W0/E.W1/H. |
+
 | Wave | Scope | Consumer gate |
 |---|---|---|
 ...
-| H.W6 | **SK-V6 strict matrix target before CSS gates.** Full 17-corpus × 7-workload matrix with same-plane sidecars, zero parse-G or falsified rejections, and zero N-direct or falsified rejections. | SK-V6 Wave 4 close condition fires before CSS SOTA work resumes. |
+| H.W6 | **SK-V13 full-SOTA receiver map.** G1 full CSS L4 parity, G2 decision-engine fold, G3 union variant or architectural block, G4 zero aarch64 production orphans, G5 all 51 JSON rows above strict sonic-rs or architecturally blocked, G6 Totality V1.1/G-Omega before W0, and G7 no-demotion. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md` G1-G7 governs S-P3 and J.W1; every miss needs row admission, measured rejection, or architectural-block proof. |
```

```diff
@@ restart/MASTER-PLAN.md:535 @@
 | H.W7 | Pratt recognizer facts and BIR `PrattSpine`. | Expression grammar uses auto-detected Pratt. |
+
+### 13.2 Pass Omega V1.1 Receiver Waves (proposed)
+
+| New wave | Allocation | Receiver |
+|---|---|---|
+| MP.NW0 | G-Omega and Totality V1.1 ratification before any SK-V13 W0/source/RESULTS/REDRESS wave. | G-Omega packet; CRUD-2/CRUD-4; S-P3 pre-W0 refusal gate. |
+| MP.NW1 | Current-state authority and row-plane telemetry fold for SK-V12 CSS admission and JSON parse/direct/typed planes. | BENCH, HANDOFF, MASTER current-state ledger. |
+| MP.NW2 | CSS stylesheet root and selector framework under strict lightningcss parity. | SK-V13 G1 CSS feature rows. |
+| MP.NW3 | CSS declaration-values expansion: declarations, `var()`, `calc()`, colors, custom-property/value facts. | SK-V13 G1 feature rows. |
+| MP.NW4 | CSS visual/rule expansion: gradients, transforms, filters, easing, at-rules, nesting. | SK-V13 G1 feature matrix. |
+| MP.NW5 | JSON 51-row strict sonic matrix: 17 corpora x parse_only/direct_to_struct/real_typed_struct. | SK-V13 G5 and J.W1. |
+| MP.NW6 | Lock 14 generated provider/config/sink/fact/flag/schema repair with CSS plus Sheets/BBNF-self negative controls. | Lock 14, generated registry, non-JSON witnesses. |
+| MP.NW7 | Regex/HIR fact extraction import boundary through `parse-that-regex` or equivalent facts. | D/H regex fact consumer and generated parser/resolver row. |
+| MP.NW8 | Decision-engine replacement: bbnf-regex extraction, egraph language, guarded rewrites, active cost, CSP feasibility, P1-P8 retirement/fail-closed compatibility. | SK-V13 G2, C.W4/C.W5, backend-shape rows. |
+| MP.NW9 | AArch64 ASCII run-skip production split and zero-orphan disposition. | SK-V13 G4, CSS scan-block consumer or measured rejection. |
+| MP.NW10 | Fresh union-substrate variant or architectural block with material differential beyond REDRESS 96/97/98. | SK-V13 G3, substrate/runtime/codegen/bench wave. |
+| MP.NW11 | Sheets and BBNF-self negative-control/generalization witnesses for generated role facts and future grammar onboarding. | Lock 14 and future grammar onboarding gates. |
+| MP.NW12 | Rolling SOTA delta and no-demotion close gate for every JSON row/plane and every CSS feature. | BENCH rolling table, HANDOFF close gate, MASTER J.W1/J.W5. |
+
+Cost/risk summary: MP.NW0 250-700 doc LOC high process; MP.NW1 180-420 doc/report LOC high gate; MP.NW2 350-500 high parity; MP.NW3 600-840 medium-high; MP.NW4 700-950 medium; MP.NW5 350-900 high; MP.NW6 700-2000 cap 2600 high; MP.NW7 300-700 high; MP.NW8 900-2200 cap 2800 high; MP.NW9 120-220 narrow high if generalized; MP.NW10 700-1600 cap 2000 high; MP.NW11 250-800 medium-high; MP.NW12 150-350 high anti-paper-close. These costs are proposed allocations for CRUD-2/G-Omega review, not implementation authorization.
```

```diff
@@ restart/MASTER-PLAN.md:647-669 @@
 ## 15. Tranche J - Parity, Docs, Publication Close
 ...
-| J.W1 | Final SOTA gate and benchmark report. | JSON/CSS/SIMD targets met; misses require amendment before close. |
+| J.W1 | Final SOTA gate and benchmark report. | JSON/CSS/SIMD targets met under SK-V13 G1-G7: full CSS parity, all 51 JSON rows above strict sonic-rs or architecturally blocked, decision-engine fold, union variant/block, zero aarch64 production orphans, Totality V1.1/G-Omega pre-W0, and no demotion. Misses require architectural-block proof or amendment before close. |
```

```diff
@@ restart/MASTER-PLAN.md:875-906 @@
 ## 24. Carry And Friction Ledger
 ...
+| Omega V1.1 MASTER reconciliation | CRUD-2 / G-Omega | §H scoped landings, refuted routes, and SK-V13 full-SOTA receivers are not visible in current MASTER. | Apply the Ω-D accepted diff only after Pass Omega convergence and G-Omega; preserve landed-scoped/partial/refuted/pending labels. | omega |
+| Rolling SOTA delta | H/J/BENCH/HANDOFF | Close can paper over row demotion or one-row CSS admission. | `restart/skinny/ROLLING-SOTA-DELTA.md` or equivalent BENCH-owned table carries every JSON row/plane and CSS feature; regressions fail G7 unless architectural-block/user re-pin is recorded. | omega + skinny |
+| G-Omega before SK-V13 W0 | HANDOFF / S-P3 | Implementation waves can start before Totality V1.1 ratifies skinny lessons. | HANDOFF and S-P3 SPEC refuse Wave 0, source edit waves, and RESULTS/REDRESS-writing waves until G-Omega closes. | omega + skinny |
```

```diff
@@ restart/MASTER-PLAN.md:926-945 @@
 ## 25. Implementation Order

 The implementation order is:

 1. Commit Phase 2 synthesis outputs.
+2. Complete Pass Omega convergence and G-Omega before applying any V1.1 MASTER/LOCKS/HANDOFF/MIGRATION/SKINNY corpus amendments or dispatching SK-V13 W0.
 2. Start tranche A from a clean worktree.
 ...
-No implementation tranche starts by editing PASS outputs, prompt contracts,
-locks, corpora, or inheritance docs. Those documents are inputs.
+No implementation tranche starts by editing PASS outputs, prompt contracts,
+locks, corpora, inheritance docs, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.
+Those documents are inputs. SK-V13 source/generated/gate/result waves remain
+blocked until Pass Omega convergence, CRUD authorization, and G-Omega.
```

Footer: no master-plan reconciliation merges before Omega convergence and G-Omega.
