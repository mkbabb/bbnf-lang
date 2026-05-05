# RESEARCH-FOLD ORCHESTRATOR — Wave 5+ (Greenfield Restart)

You are the research-fold orchestrator. Wave 4 returned READY at `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md` (commit `f0b186ea`). Your role is to dispatch (a) one final metahardening V5 cycle that knows the V1→V4 history and audits for what those cycles structurally missed, then (b) a research-folding cycle that grounds the SOTA-asserted architecture in the source literature, then (c) a final hardening V6 against the folded artefacts.

The pipeline is single-round per phase; phases sequence with autonomous amendment loops where intermediate verdicts demand surgery. The terminal verdict (Phase 4 V6 consolidation) gates per-tranche full-spec drafting.

## §1 — Required reading (mandatory)

1. `restart/README.md` — gestalt anchor; settled positions; 14 locks; BBNF V1 extensions; tape + direct-to-struct union; SOTA synthesis.
2. `restart/locks/14-LOCKS.md` — settled architectural commitments.
3. `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md` — V4 cohort verdict + closure ledger (the carry-baseline).
4. `restart/audit/hardening/HARDENING-CONSOLIDATED.md` (V1) + `HARDENING-CONSOLIDATED-V2.md` + `HARDENING-CONSOLIDATED-V3.md` — V1→V4 history (audit drift evidence).
5. `restart/prompts/HARDENING-ORCHESTRATOR.md` — the per-target hardening contract (you reuse it unchanged for V5 + V6).
6. `restart/prompts/HARDENING.md` — the per-target audit specification.
7. `restart/prompts/AMENDMENT-DISPATCH.md` — verify-then-patch discipline (you reuse it for any intermediate amendment cycle).
8. `docs/precepts/instructions/STYLE.md` + `LESSONS-LEARNED.md` + `CONSUMING.md`.
9. `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` — folded surfaces.
10. `restart/audit/pass-{1-substrate,2-codegen,3-runtime}/PASS-{1,2,3}.md` — folded surfaces.

## §2 — Phase 0 — V5 metahardening (4 parallel; carry-aware)

The V1→V4 cycles audited per-target punch lists. V5 audits what those cycles structurally missed because of their punch-list focus. Five lenses V5 applies that V1-V4 did not centrally apply:

- **Inter-document narrative coherence**: does the trio (ARCHITECTURE + MIGRATION + MASTER-PLAN) read as one document or three independent voices? Where does narrative bind across files versus drift?
- **Vocabulary drift**: post-Wave-4.1 amendments may have introduced subtle terminology shifts. Does `LayoutFacts` mean the same thing in PASS-1 §3, ARCHITECTURE §7.3, MASTER-PLAN C.W1? Same for `passes::layout`, `BackendIR`, `Tape`, `LayoutSink`, `pointer!`/`select!`, `fixture-manifest`, `bench cohort`?
- **Worked-example scarcity**: V1-V4 audited gates and tables but rarely demanded one complete walkthrough. Does the corpus carry: (1) a complete yaml onboarding worked example, grammar→generated→benchmark; (2) a complete query worked example through `pointer!` + `select!`; (3) a complete incremental-parse walkthrough?
- **Coverage gaps**: lanes V1-V4 did not push (publication readiness, ergonomics under unfamiliar use, fault-tolerant incremental parsing, WASM-host primitive surface, debug-runtime hooks). Are these settled in document text or only in lock summaries?
- **Architectural axiom cumulative consistency**: do the 14 locks hold under their cumulative constraints? E.g., does Lock 1 (tape + direct union) survive Lock 6 (e-graph rewrites that may transform tape projections)? Does Lock 4 (HM + bidirectional + CSP) survive Lock 10 (generic rules + chains)?

Output paths: `restart/audit/hardening/HARDENING-{PASS-1,PASS-2,PASS-3,MASTER-PLAN}-V5.md`. Each agent applies the five lenses + the standard 9-lane audit (compressed verification mode since V4 closed) + the 16-command tightened gate-rerun. Verdict: READY / AMENDMENT-REQUIRED / RE-DRAFT.

Each V5 agent reads the V1-V4 history reports for its target before forming the audit; V1-V4 evidence is the carry-baseline, not the verdict source. The lenses look for what those cycles missed.

Cap: 75 min per parallel agent. Wall ~75 min.

After all four V5 commits land, you (the orchestrator) consolidate at `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md` (~400-800 lines; §1 target identifications, §2 cohort verdict, §3 cross-target conflicts, §4 punch list, §5 final verdict, §6 voice locks, §7 closing posture).

If V5 returns AMENDMENT-REQUIRED with ≤10 narrow items: dispatch a single parallel narrow-amendment cycle (3 agents on non-overlapping surfaces) per AMENDMENT-DISPATCH §3 Wave 4.1 pattern, then re-verify with one V5.1 cycle. If still AMENDMENT-REQUIRED, escalate to RE-DRAFT class.

If V5 returns READY: proceed to Phase 1.

## §3 — Phase 1 — Research deep-dives (8 parallel)

Eight research topics, each grounding one or two architectural locks. Each agent reads the existing restart corpus + the SOTA literature for its topic (papers, canonical codebases, blog posts, benchmark reports), and produces a research artefact at `restart/research/<topic>.md` (~500-1000 lines).

The research artefact carries:
- §1 — Settled position in the restart (cite path:line).
- §2 — SOTA literature deep-dive (papers + codebases + benchmarks); each citation = primary source.
- §3 — Convergence points (where restart matches SOTA).
- §4 — Divergence points (where restart departs and why).
- §5 — Refinements to fold (specific text changes for the existing trio + PASS surfaces).
- §6 — Adversarial findings (where SOTA contradicts a settled lock or proves a settled claim too weak/too strong).
- §7 — Surgery proposals (concrete edits Phase 2 would land).

The eight topics:

| # | Topic | Anchors | Owner locks | Key sources |
|---|---|---|---|---|
| 1 | Type system foundations: HM + algorithm W + Damas-Milner | Lock 4 | Restart README §7 + ARCH §8 | Damas-Milner 1982; Pierce *Types & Programming Languages* ch. 22; algorithm-W canonical impls in OCaml/SML |
| 2 | Bidirectional + Pierce-Turner + Dunfield-Krishnaswami | Lock 4 | Restart README §7 + ARCH §8 | Pierce-Turner 1998; Dunfield-Krishnaswami 2013 *Complete and Easy Bidirectional Typechecking*; Dunfield 2019 |
| 3 | CSP-backed unification + GADTs + parametric polymorphism + generic rules | Lock 4 + Lock 10 | Restart README §7 + §5 | Pottier-Rémy *Essence of ML Type Inference*; OutsideIn(X) GHC; HM(X) variants; System F applications |
| 4 | E-graphs + equality saturation + bridge-vs-union | Lock 6 | Restart README §6 + ARCH §10 | Tate 2009; Willsey 2021 *egg*; Chow extraction; egglog |
| 5 | Cost models + Pareto extraction + SMT-backed | Lock 6 + Lock 7 | Restart README §6 + ARCH §10 | egg analysis trait; SMT-LIB cost composition; multi-objective optimisation |
| 6 | Tape encoding + direct-to-struct union | Lock 1 | Restart README §8 + ARCH §11 | sonic-rs Tape; simdjson 2018/2020 papers; yyjson; rapidjson; mtreelib |
| 7 | Green/red trees + incremental parsing + fault tolerance | Lock 1 + carry-incr | Restart README §8 + carry ledger | rowan; treesitter incremental; rust-analyzer salsa; tree-sitter parsing under errors |
| 8 | SIMD scanning + DFA construction + bespoke regex HIR | Lock 1 + bbnf-regex | Restart README §6 + ARCH §10 | simdjson SIMD; vectorscan; logos; regex-automata; Cox 2007 *Regular Expression Matching: the Virtual Machine Approach* |

Cap: 90 min per agent. Wall ~90 min (8 parallel).

Each agent commits its research artefact: `docs(restart/research): <topic> — research deep-dive`.

## §4 — Phase 2 — Synthesis fold (4 parallel)

Four agents read the eight research artefacts and fold findings into the existing surfaces.

Routing matrix:

| Agent | Surface | Folded findings |
|---|---|---|
| PASS-1 fold | `restart/audit/pass-1-substrate/PASS-1.md` | Topics 1 + 2 + 3 (type system); topic 4 + 5 (e-graph evidence touching Grammar IR); topic 7 (incremental — Grammar-IR-side fault tolerance) |
| PASS-2 fold | `restart/audit/pass-2-codegen/PASS-2.md` | Topics 6 + 8 (tape + SIMD lower-time obligations); topic 4 + 5 (cost-model trait + e-graph drive codegen); topic 3 (generic-rule lowering) |
| PASS-3 fold | `restart/audit/pass-3-runtime/PASS-3.md` | Topics 6 + 7 (tape + green/red runtime); topic 8 (regex-side runtime); topic 1 + 2 (HM-backed value typing visible to user) |
| SYNTHESIS fold | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | All eight topics' Phase-2-routed text; ARCHITECTURE §8 (type system) + §10 (optimization) + §11 (runtime); MASTER-PLAN tranche-level evidence rows; MIGRATION corpus citations |

Discipline: verify-then-patch per AMENDMENT-DISPATCH §1. Each fold is verified against the research artefact's §6 (adversarial findings) and §7 (surgery proposals). The fold agent rejects any fold that contradicts a settled lock without escalation; such contradictions surface in the §6 adversarial findings of the research artefact and trigger a Phase 2 escalation amendment cycle (see §5).

Each fold agent commits two: classification + amendment per the Wave-2 pattern.

Cap: 75 min per agent. Wall ~75 min (4 parallel).

## §5 — Phase 2 escalation (conditional)

If any research artefact's §6 (adversarial findings) carries a finding that contradicts a settled lock — e.g., "Pierce-Turner alone insufficient for the rule-quantifier surface; Dunfield-Krishnaswami required" — the fold agent surfaces it as a Phase-2-escalation item rather than folding it.

The orchestrator (you) consolidates Phase-2-escalation items at `restart/research/escalation-summary.md` (~100-300 lines) listing per-finding: the contradicted lock, the SOTA evidence, the proposed amendment, the receiving phase. If escalation count is zero: no action; proceed to Phase 3. If 1-5: dispatch a single narrow-amendment cycle to address before Phase 3. If >5 or any item argues for a structural lock change: return RE-DRAFT and halt.

## §6 — Phase 3 — Hardening V6 (4 parallel; HARDENING-ORCHESTRATOR.md Phase 3 reused)

Reuse `restart/prompts/HARDENING-ORCHESTRATOR.md` Phase 3 unchanged. Output paths use V6 suffix:
- `restart/audit/hardening/HARDENING-PASS-1-V6.md`
- `restart/audit/hardening/HARDENING-PASS-2-V6.md`
- `restart/audit/hardening/HARDENING-PASS-3-V6.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN-V6.md`

Each V6 agent applies the standard 9-lane audit (P/C/E/C per row) + the 16-command tightened gate-rerun. Each V6 agent additionally reads the eight research artefacts as evidence corpus; the audit verifies that folded research is coherent with the surface that absorbed it.

Cap: 70 min per agent. Wall ~70 min (4 parallel).

## §7 — Phase 4 — V6 consolidation

You (the orchestrator) consolidate at `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` (~600-1200 lines; §1-§7 per HARDENING-ORCHESTRATOR.md Phase 6).

Final verdict:
- READY → per-tranche full-spec drafting unblocks.
- AMENDMENT-REQUIRED with ≤10 narrow items → narrow-amendment cycle + V6.1 re-verify.
- RE-DRAFT or AMENDMENT-REQUIRED with structural items → escalate to user.

## §8 — Cross-tranche scope boundary

You touch ONLY:
- The Agent dispatch invocations (Phases 0, 1, 2, 3).
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md` (Phase 0 consolidation).
- `restart/research/escalation-summary.md` (Phase 2 escalation, conditional).
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` (Phase 4 consolidation).

You do NOT modify:
- `restart/README.md`, `restart/locks/`, `restart/prompts/`, `restart/inheritance/`, `restart/corpora/`.
- The dispatched agents' V5/research/fold/V6 outputs (they own theirs).
- `crates/`, `docs/`, `restart-archive-2026-05-04/`.

## §9 — Hard caps + total wall

| Phase | Wall (parallel) | Sequencing |
|---|---|---|
| Phase 0 V5 + consolidation | ~100 min | 4 parallel + 1 consolidation |
| Phase 0.5 amendment cycle (conditional) | ~60 min | 3 parallel + 1 verification |
| Phase 1 research | ~90 min | 8 parallel |
| Phase 2 fold | ~75 min | 4 parallel |
| Phase 2 escalation amendment (conditional) | ~60 min | 1-3 parallel |
| Phase 3 V6 hardening | ~75 min | 4 parallel |
| Phase 4 V6 consolidation | ~30 min | 1 |
| **Total** | **~7-9 hours** | |

If amendment cycles trigger at Phase 0 or Phase 2, add ~60-90 min each. Worst-case wall ~10-12 hours.

## §10 — Methodology

You orchestrate; you do not author per-phase content (except Phase 0/4 consolidations and Phase 2 escalation summary). Per-phase substantive work is the dispatched agents' role.

- Phase 0 + Phase 1 + Phase 2 + Phase 3 dispatches: parallel — multiple Agent tool invocations in a single message; each `run_in_background: true`; each carries the per-target / per-topic dispatch prompt the orchestrator composes.
- Phase 0/2/4 consolidations: direct.
- Cross-target conflicts and SOTA-vs-lock contradictions surface at consolidation; the dispatched agents catch what they can, the orchestrator catches what falls between.

Each per-agent dispatch prompt carries:
- The agent's role + scope.
- Reference to the operational contract (`HARDENING-ORCHESTRATOR.md` for V5/V6; this prompt for research/fold).
- Reference to `restart/locks/14-LOCKS.md`.
- The verify-then-patch discipline (for fold agents).
- The output path + commit message format.
- The cross-tranche scope boundary.
- The hard cap.
- For research agents: the topic anchor + key sources + adversarial-finding obligation.

## §11 — Closing posture

The greenfield restart's research-fold cycle grounds asserted SOTA citations in primary literature, surfaces any architectural reconsideration the SOTA evidence demands, and verifies the folded corpus through one final hardening cycle. The terminal V6 verdict gates per-tranche full-spec drafting.

Hereupon Phase 0 (V5 metahardening) dispatches.
