# SK-V14 → SK-V16 Indefatigable Execution Handoff

You inherit a bbnf-lang restart campaign mid-flight. Three full SK cycles remain: SK-V14 (wave execution in progress), SK-V15, SK-V16. Your job is to drive all three to close, end-to-end, under one binding discipline: **never relinquish control except at G-Omega**.

This prompt is deliberately lean. It points at the existing prompt suite and current state instead of restating them. Read the cited files; do not assume their contents from this prompt.

---

## §0 — Binding pin (carry verbatim through all cycles)

- **Do not relinquish control except at G-Omega user gate.** G-Omega is the only mandatory relinquish. Every other gate auto-passes per the SK-V14 ORCHESTRATOR-PROMPT user-pin override.
- **No quick solutions, no workarounds.** Idiomatic, gestalt approaches. Per `[no-workarounds]` and `[no-workarounds-arch]` in user memory at `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/`.
- **Apple M5 Max only.** aarch64 target; x86 OUT.
- **Indefatigable.** Successive tranches roll automatically per `restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0.1 R10 until full ADMIT or per-row intrinsic-block proof covers everything. Through SK-V16 explicitly under this handoff.
- **No warm benches.** Cold per-parse only per `[no-warm-benches]`.
- **Deep parallelization.** Spawn sub-agents aggressively per `[high-parallelization]` + `[agent-orchestration]`; never let sub-agents race on shared files (commit before parallelizing; distinct file sets only).

All other discipline (cap budgets, executable verification, refutation honesty, no metalanguage docs, etc.) lives in user memory at `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` — read it.

---

## §1 — Entry state (the moment you take over)

- **HEAD commit:** `8e7378025` (SK-V14 W11W close — full admission anchor).
- **Working tree:** clean modulo pre-existing dirty generated/root-runtime files (preserve untouched).
- **SK-V14 status:** CLOSED with full admission — JSON 51/51 (17 parse_only + 17 direct_to_struct + 17 real_typed_struct) + CSS L4 24/24. Invariants pass: 16 locks · 5 BackendShape canon · Pattern H = 67.
- **SK-V14 LOCK convergence:** 5 of 5 cohorts §3Z LOCKED.

  | cohort | LOCK commit |
  |---|---|
  | S-P2 | `4c70b6f193` |
  | T-P1 | `0a9c0fe65d` |
  | S-P3 | `626cb06cc1` |
  | T-P2 | `34a28f5c15` |
  | T-P3 | `69eea1c5c` |

- **G-Omega gates closed:** V2 (T-P3 LOCK amendments, 2026-05-24, `46aa2db57`) + V3 (W2R wave-graph correction, `83a7548cd`) + V4 (W4R wave-graph correction, `62d2e4119`).
- **V1 spec surfaces at v+2:** ARCHITECTURE.md, MASTER-PLAN.md, LOCKS.md, HANDOFF.md, MIGRATION.md, skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,HARDENING,WORKSPACE}.md. Read `restart/HANDOFF.md` first.
- **SK-V14 implementation audit (2026-05-26):** `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md` — JSON is honest >SOTA; CSS L4 is contrived (24-broadcast measurement + 646-LOC hand-written tokeniser embedded as string literal + brace-counter "full_parse" vs lightningcss CSSOM); Pattern H NOT collapsed (67 hand-written files, 0/67 carry `@generated`); Lock 14 grep gate has explicit allowlist holes; Decision Engine is SCAFFOLD (e-graph rules = 0; `block_id` self-confesses). **The inflection point is NOT reached.** See §3.
- **Next immediate move:** dispatch SK-V15 cycle starting with `PASS-IMPL-OVERFIT-AUDIT` (V1 already authored — consume its CONSOLIDATED), then SK-V15 Pass Alpha bracketing per §3.

---

## §2 — The SK loop (per-cycle process model)

Each SK-V{N} cycle executes the same seven-phase loop in sequence. Do not skip phases; do not reorder.

```
┌──────────────────────────────── one SK cycle ────────────────────────────────┐
│                                                                              │
│  (0) PASS-IMPL-OVERFIT-AUDIT  (NEW — cycle handoff)                          │
│      Audits the OUTGOING SK-V{N-1} implementation.                          │
│      Contract: restart/prompts/skinny/PASS-IMPL-OVERFIT-AUDIT.md            │
│      Outputs: restart/audit/skinny-impl-overfit/V{V}/{AGENT-1..6 + CONSOLIDATED} │
│      6 parallel agents; 30 min cap each; one-shot (no iteration).           │
│      Feeds (1) Pass Alpha + (2) S-P0 with corrective-receiver constraints.  │
│                                                                              │
│  (1) Pass Alpha                                                              │
│      Brackets SK-V{N} from SK-V{N-1} close packet + PASS-IMPL findings.     │
│      Contract: restart/prompts/pass-contracts/PASS-ALPHA.md                 │
│      Outputs: restart/skinny/tranches/sk-v{N}/SYNTHESIS.md + HANDOFF.md     │
│                                                                              │
│  (2) Skinny passes (4-stage; S-P0 consumes PASS-IMPL findings)              │
│      S-P0 audit-overfit:  restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md    │
│      S-P1 profile:        restart/prompts/skinny/PASS-1-PROFILE.md          │
│      S-P2 research:       restart/prompts/skinny/PASS-2-RESEARCH.md         │
│      S-P3 synthesis-plan: restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md   │
│      Each pass: dispatch → V1 → CHALLENGE V1 → ... → §3Z COHORT LOCK.       │
│                                                                              │
│  (3) Totality passes (3-stage; runs in parallel with skinny where deps OK)  │
│      T-P1 excavation: restart/prompts/totality/PASS-1-EXCAVATION.md         │
│      T-P2 research:   restart/prompts/totality/PASS-2-RESEARCH.md           │
│      T-P3 synthesis:  restart/prompts/totality/PASS-3-SYNTHESIS-PLAN.md     │
│      Same §3Z convergence per pass.                                         │
│                                                                              │
│  (4) Pass Omega (G-Omega gated)                                             │
│      Contract: restart/prompts/pass-contracts/PASS-OMEGA.md                 │
│      Substantive Ω-A..F + 6-lens CHALLENGE + 6-agent CRUD.                  │
│      G-Omega is mandatory; surface AskUserQuestion at the gate.             │
│                                                                              │
│  (5) Wave implementation                                                    │
│      W0..W{max} per the SK-V{N} SPEC.                                       │
│      Wave-triumvirate per wave: research → plan → redress.                  │
│      Contract: restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md         │
│      In-wave REDRESS may trigger ad-hoc Pass Omega (V3 W2R/V4 W4R precedent).│
│                                                                              │
│  (6) Close + loop                                                           │
│      SK-V{N} close per its SYNTHESIS §0.1 R10 close-condition.              │
│      Return to (0) for SK-V{N+1}. Loop.                                     │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘
```

**PASS-IMPL is the gate that keeps the campaign honest** about whether hand-crafted parsers have crossed from "acceptable >SOTA proof" into "contrivance that won't generalize". Per the user's binding latitude: hand-craft is admissible during the proof phase; contrivances (broadcast measurements, fixture short-circuits, fake-generated headers, brace-counter "full_parse" comparators) are not. PASS-IMPL surfaces the latter without flagging the former.

Per-phase convergence and discipline are defined in the cited prompt files. **Do not invent variants.** Where a phase has a 7-lens CHALLENGE (the current generation; CH7 OVERFIT-PRUNE binding from S-P0), apply seven lenses; do not regress to six.

**§3Z convergence (per-phase):** ≥95% ACCEPT × 2 consecutive cycles + zero orphan REVISEs + V≤5 ceiling. Source: `restart/prompts/ORCHESTRATOR.md` §3W + §3Z.

**Hard caps (per dispatch type):** research ~20 min, plan ~15 min, redress ~30 min. Per `[dispatch-hard-cap]`. Commit at 0.9N, halt at N.

---

## §3 — SK-V15 entry constraints (binding — derived from PASS-IMPL V1)

SK-V14 closed full-admit but the implementation audit (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`) flagged dispositive CSS contrivances + Pattern H non-collapse + Lock 14 gate holes + Decision Engine scaffold. SK-V15 is a **PRUNE-then-REBUILD cycle** that retires those contrivances and brings CSS to JSON's honesty level. Only after SK-V15 closes can SK-V16 attempt grammar-driven generalization.

SK-V15 Pass Alpha MUST bracket the cycle with explicit receivers for each of the following waves. SK-V15 SPEC §13.3 wave-graph emerges from these.

**PRUNE-WAVE-A (CSS contrivance retirement):**
- Collapse the 24-row CSS L4 broadcast (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:206-228 measure_full_parse_profiles` + `W8_SELECTED_CSS_ROWS=24` line 17) to ONE honest row, or partition the corpus and time each feature independently.
- Retire the 646-LOC `CSS_GENERATED_RS` string literal at `skinny/crates/codegen/src/runtime_generator.rs:713-1359`; replace with grammar-derived emission from `grammar/css/l4/*.bbnf` (this is the W2/W5 promise unfulfilled).
- Replace the brace-counter `CssFullParseSummary` (`{rules, at_rules, qualified_rules, declarations}` only) at `generated.rs:53-59` with a real CSS value type (CSSOM-equivalent rule/declaration/at-rule typed nodes).
- Restate CSS measurement against cssparser (the actual same-workload comparator); lightningcss requires a fuller CSSOM build to qualify as same-workload.
- Delete the topology-pinning tests at `skinny/xtask/src/regen_css.rs:148, :164`.

**PRUNE-WAVE-B (Lock 14 gate restoration):**
- Remove the `GENERIC_SCAN_ROOTS` exclusion list at `skinny/xtask/src/lock14_baseline.rs:2370-2379` (`runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates/`).
- Re-run the Lock 14 baseline against the full surface; expect a wave of newly-visible leaks; PRUNE each.

**PRUNE-WAVE-C (codegen leak abrogation):**
- Collapse 9 `xtask::Cmd::Regen<Grammar>` variants + 9 hardcoded match arms (`skinny/xtask/src/main.rs:62-89`) to the single generic `Cmd::Regen { grammar: Option<String>, ... }` already present.
- Collapse the 4-variant `RuntimeStyle` enum (`skinny/xtask/src/regen_simple_runtime.rs:32-37`) to one template.
- Collapse the 2-variant `RuntimeGenerationMode` (a renamed grammar-family branch: `PassCompiled=JSON`, `FrontendFacts=CSS`) at `runtime_generator.rs`.
- Retire the 7-arm CSS L4 profile-id match at `runtime_generator.rs:114-153`.
- Retire `validate_non_json_frontend_materiality` (grammar partition encoded in function name) at `grammar_provider.rs:210`.
- Retire pass-layer JSON-byte literal recognizers at `skinny/crates/passes/src/lib.rs:338, 345, 1111, 1131, 1154, 1370, 1373, 1381`.

**PRUNE-WAVE-D (Pattern H discipline):**
- Every Pattern H runtime file MUST carry `// @generated by skinny bbnf-codegen; do not edit by hand.` at line 1 (parity with the skinny twin runtime, where 43/48 already carry it).
- Collapse the 4/9 bespoke grammars (CSS L4 with its 14-variant `OpenFrame` enum at `crates/core/src/runtime/css_l4/builder.rs:14-80`, JSON sink/scan, plus two others) into the single template that 5/9 grammars (math, csv, bnf, ebnf, css_pretty) already share.
- Census stays 67 (no file count change) but `grep -l "@generated by skinny bbnf-codegen" crates/core/src/runtime/**/*.rs | wc -l` should return 67 at close.

**REBUILD-WAVE-E (CSS value API):**
- Author the CSS Value type (mirror `JsonValue` shape).
- Author the CSS view/visitor (mirror JSON shape).
- Re-time CSS >SOTA against the typed CSSOM workload; report honest deltas vs cssparser AND lightningcss.

**REBUILD-WAVE-F (Decision Engine activation):**
- Populate the e-graph with at least one real rewrite rule at `backend_egraph.rs:66` (currently zero).
- Make `DecisionCspFacts` drive emission selection (not just scaffold). Remove `block_id = "JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT"` self-confession at `decision_csp.rs:166`.
- Remove `static_css_provider_status` + `json_sink_only_status` grammar-named fields at `ir/src/cost.rs:242-243`; replace with grammar-neutral `provider_status` indexed by grammar-id.
- Implement the 4 stub BackendShape lower-impls (`eager_tape.rs`, `offset_tape.rs`, `event_tape.rs`, `collapsed_stage.rs`) — currently 17-LOC stubs returning `format!("rule {} -> shape_name", ...)`; each ≥50 LOC of actual lowering, matching the 270-LOC `sink_only.rs` precedent.

**REBUILD-WAVE-G (bench-only contrivance quarantine):**
- The W11L/W11N/W11O FNV-64 closed-enum scheme at `skinny/xtask/src/real_typed_schema.rs:942-957` + `generated_real_typed.rs` stays bench-only. Add explicit guard: no FNV-keyed arbiter may migrate into `crates/core/src/runtime/`.
- Strengthen the strict-product comparator to detect closed-enum-aware sidecars in the differential (the current strict-product can't catch hash collisions because sonic-rs and serde sidecars deserialize into the same closed enum).

**Three NEW CHALLENGE-lens procedural addenda** for SK-V15 T-P3 (per `CONSOLIDATED-AUDIT.md` §discipline-forward-lens):

- `NEW-CH3-V5-01 wave-graph cycle detection.` Every spec amendment that mandates deletion of artefact X in wave N MUST verify rebuild capability for X is delivered no later than wave N (V3 W2R + V4 W4R precedent).
- `NEW-CH5-V5-02 broadcast-admission detection.` When N rows admit on a measurement, verify the TSV produces N distinct rows of measurement data, not 1 row broadcast N times (per AUDIT-5 F-1 dispositive).
- `NEW-CH7-V5-03 gate exclusion detection.` Every Lock 14 / Lock 16 grep gate must scan its own exclusion list and surface anything in the exclusion list as a finding (per AUDIT-4 F-8: `lock14_baseline.rs:2370-2379` was excluding the four worst leaks).

---

## §4 — SK-V15 and SK-V16 trajectory

SK-V15 cycle index conventions:
- Pass Omega cycle for SK-V15 = V5 (V2 closed T-P3 amendments; V3 closed W2R; V4 closed W4R; V5 is SK-V15 LOCK amendments).
- PASS-IMPL cycle for SK-V15 close = V2.

SK-V15 success criteria (gates for the SK-V16 trajectory):
- All PRUNE-WAVE-{A,B,C,D} close at admission. Failures route through the in-wave REDRESS + Pass Omega Vn precedent.
- REBUILD-WAVE-E delivers a typed CSS Value API isomorphic to `JsonValue`.
- REBUILD-WAVE-F brings Decision Engine LOAD-BEARING (e-graph rule count ≥1; CSP non-tautological; 5 BackendShape impls all ≥50 LOC).
- PASS-IMPL V2 (cycle close audit) returns ACCEPT on every axis OR documents proven-blocked status with intrinsic-block proof per row.

**If SK-V15 closes cleanly** → SK-V16 is the **grammar-driven generalization tranche**. Inflection point reached. The W6 Pattern H collapse becomes structurally meaningful (not just renaming hand-written files under a regenerator). The 646-LOC CSS string literal is gone; CSS is grammar-derived. CSS Value API matches JSON. Decision Engine drives emission. SK-V16 admits NEW grammars (math operational, csv operational, sheets operational, BBNF-self operational) without any per-grammar hand-tuning.

**If SK-V15 surfaces new contrivances** → PASS-IMPL V2 routes them as SK-V16 PRUNE-WAVE inputs. Indefatigable loop continues. SK-V16 may also be a PRUNE cycle if needed.

**Do not invent SK-V15/SK-V16 content beyond the constraints in §3.** Each cycle's content emerges from Pass Alpha re-entry against the prior cycle's close packet + PASS-IMPL findings. The role of this handoff prompt is to lock in the loop discipline + carry forward the binding SK-V14 audit findings.

**Three SK-V14 close LACs forward into SK-V15** (per `3F-migration-handoff.md` "Pass Alpha re-entry" section): LAC-1E-14 FactStream resolution · LAC-1E-15 Pattern H residual · LAC-1E-16 audit-overlay column population proof.

---

## §5 — G-Omega gate (per cycle; user-facing)

Each SK cycle has one G-Omega user gate (after Pass Omega CHALLENGE convergence; before CRUD merges LOCKS). The contract is `restart/prompts/pass-contracts/PASS-OMEGA.md` §6.

At each G-Omega gate:
- Surface the cohort §3Z LOCK declaration.
- Surface the proposed locks-diff (`3C-locks-v+1-diff.md` or equivalent for that cycle).
- Surface the proposed master-plan-diff.
- Surface the CRUD packet list.
- Ask the user via `AskUserQuestion`. Three options: Authorise / Hold for review / V5 extra confirming wave.

**Until the user authorises**, do not merge LOCKS, do not run CRUD-3. Other CRUDs can proceed only if the prompt explicitly authorises pre-G-Omega-diff staging (Pass Omega V1 + V2 precedent shows the orchestrator typically waits for the gate before any CRUD).

After Authorise: dispatch all 6 CRUDs (CRUD-3 LOCKS first, then CRUD-1/2/4/5 in parallel, then CRUD-6). Per-CRUD detail in `restart/audit/totality/astral/V2/CRUD-LOG.md` (V2 precedent; mirror the structure).

---

## §6 — Artefact filing convention (do not improvise paths)

- Skinny per-tranche: `restart/skinny/tranches/sk-v{N}/{SYNTHESIS,HANDOFF,SPEC,DISPATCH-PROMPT,ORCHESTRATOR-PROMPT}.md` + `audit-overfit/` + `research/p{0,1,2,3}/`.
- Totality per-cycle: `restart/audit/totality/p{1,2,3}/{1,2,3}{A-F}-*.md` + `hardening/V{1..5}/CH{1-7}.md` + `hardening/HARDENING-T-P{1,2,3}-V{N}-CONSOLIDATED.md`.
- Pass Omega per-cycle: `restart/audit/totality/astral/V{V}/` per `PASS-OMEGA.md` §7. Increment V per cycle (V1 closed 2026-05-22; V2 closed 2026-05-24; V3 onward).
- V1 spec surfaces: `restart/{ARCHITECTURE,MASTER-PLAN,HANDOFF,MIGRATION}.md` + `restart/locks/LOCKS.md` + `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,HARDENING,WORKSPACE}.md`.
- Skinny corpus tranche history archived under `restart/skinny/tranches/sk-v{N-K}/` per `[new-tranche-new-doc]`; never overwrite.

Per `[clean-regen-discipline]`: generated files are output of fresh regen, never hand-patched. Per `[no-backward-compat]`: full migrate, no compat shims.

---

## §7 — Inviolable invariants (verify every cycle close)

Each of these is a binding invariant of the V1 spec at every cycle close. If a cycle proposes to amend any of these, route through Pass Omega Ω-C → G-Omega; do not amend silently.

1. **16-lock count.** Verify: `grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md` returns 16.
2. **5-shape BackendShape canon at Lock 10.** `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. No 6th variant. LAC-1E-14 FactStream is the **5th SUBSTRATE-manifest category at Lock 1** — orthogonal to BackendShape.
3. **Pattern H = 67.** Verify: `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` returns 67. The `-maxdepth 2` form is the LAC-1E-12 trap (returns 63).
4. **LAC-2F-V5-02 substrate-union elevation.** Lock 1 v+1; REDRESS 96/97/98 generalised to ALL transient classifier-state primitives.
5. **LAC-1E-12 executable verification mandate.** Every cite must re-execute at HEAD; commit anchors must be re-anchored when LOCKS-amendment commits land.
6. **NEW-CH2-V3-02 orphan-cell propagation guard.** Pre/post grep evidence per fold-author edit.
7. **No deferrals.** Per `[no-deferrals]`: integrate everything in the current pass; never punt to "future tranche".

---

## §8 — Termination

You stop only at:
- A G-Omega user gate (the user chooses Hold or V5; await response).
- SK-V16 close per its R10 condition. At that point the campaign explicitly terminates under this handoff. Hand state back to the user with a close packet (mirror `restart/audit/totality/astral/V2/G-OMEGA-SIGNOFF.md` structure) summarising the three-cycle trajectory.
- An invariant violation that cannot be repaired via the in-pass discipline. Per `[abrogate-before-patch]`: for intrinsic-failure subsystems, ask "can we delete?" before "can we patch?" — if neither, surface to user.

Do not stop for:
- "Need user direction" (you have direction; it is the binding pin and the SK loop).
- "Plan is large" (the plan is the §2 loop; size does not justify halt).
- "Cycle is long" (it is; per `[no-deferrals]` you carry it).

---

## §9 — Read before dispatch (one-time bootstrap)

In this order:
1. `restart/HANDOFF.md` — canonical current-state document.
2. `restart/prompts/ORCHESTRATOR.md` — §3W/§3Z convergence + non-negotiables.
3. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` — R1-R10 + P-1..P-7 + R10 close-condition.
4. `restart/skinny/tranches/sk-v14/SPEC.md` — W0..W11 wave program.
5. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` — R-target acceptance criteria + dispatch order.
6. `restart/audit/totality/p3/3F-migration-handoff.md` (full file) — dispatch checklist + per-wave receivers + open questions.
7. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` — per-wave triumvirate contract.
8. `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` — feedback discipline index.

After §9 read, dispatch SK-V14 W0 wave-triumvirate.

---

End of handoff. Pick up at SK-V14 W0; carry through SK-V16 close.
