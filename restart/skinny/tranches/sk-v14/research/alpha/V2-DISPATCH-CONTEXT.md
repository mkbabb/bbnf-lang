# Pass Alpha V2 Dispatch Context — SK-V13 → SK-V14 Bracket

Authored by the SK-V14 orchestrator after CHALLENGE V1 aggregator
returned `596b897bf` with verdict **PENDING-V2** at 86.86 % aggregate
ACCEPT. V2 folds the 7 REJECTs + 29 REVISEs into the four α-artefacts
that need redispatch. α-B, α-D, DISPATCH-CONTEXT V1 stand.

This file is the shared V2 dispatch context. Each V2 agent reads §0 — §3
+ its own V2 fold packet (cited in §4 below; the full per-agent packet
lives in `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/HARDENING-ALPHA-V1-CONSOLIDATED.md §2.X`).

## §0 — Authority (unchanged from V1)

All V1 authority binds. Read order:

1. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` — SK-V14 fresh-session pin.
2. `restart/prompts/pass-contracts/PASS-ALPHA.md` — contract.
3. `restart/prompts/ORCHESTRATOR.md` — meta-binding.
4. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` — campaign-wide bar.
5. `restart/locks/LOCKS.md` — 16 locks; **Lock 1 substrate-ceiling fold at lines 73-82 is load-bearing for E-3 and F-14**.
6. `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` + `validation/v{1..6}-*.md`.
7. `restart/skinny/tranches/sk-v14/research/alpha/DISPATCH-CONTEXT.md` — original V1 dispatch (your scope is unchanged; only the fold-list overlay is new).

## §1 — V1 verdict bind

V1 ACCEPT-rate 86.86 % (238/274). 7 REJECTs (3 CH4, 1 CH5, 2 CH6, 1 CH7-BINDING). 29 REVISEs. **CH7's binding REJECT on C-3's round-trip gate** independently forecloses V1 convergence per `PASS-0-OVERFIT-AUDIT.md §CH7` final paragraph; it must land in V2.

V2 forecast (per V1 CONSOLIDATED §3.1): 274/274 = 100 % if every §2 fold lands verbatim; realistic 95–98 % under 0–3 new-finding-per-lens assumption.

V3 confirming pass follows V2 per `ORCHESTRATOR.md §3Z` two-consecutive-cycle rule.

## §2 — V2 cycle discipline (binding on every V2 α-agent)

- **HARD CAP** per agent: α-F 45 min; α-E 45 min; α-A 15 min; α-C 15 min. At 0.9·N commit-equivalent (write file) what you have; at N halt.
- **WRITE-ONLY.** Do NOT `git add`. Do NOT `git commit`. Overwrite your prior V1 artefact in place; the orchestrator commits all four V2 α-artefacts atomically once all four return, then dispatches CHALLENGE V2.
- **OVERWRITE IN PLACE** per `ORCHESTRATOR.md §3Z step 1`: your V2 output replaces your V1 file at the same path; git history preserves the V1 commit naturally.
- **SEQUENCING:** α-A + α-C + α-E may run in parallel (different files, no overlap). α-F must SERIALISE after α-E completes — F-3, F-4, F-17 inherit α-E's updated values (risk class, LOC budget, table-row text).
- Cite `path:line` on every concrete claim. Voice per `STYLE.md`: archaic-permissive; no metalanguage.
- Docs/synthesis only — no source touch, no cargo, no benchmarks.
- Each fold in your packet has a numeric tag (e.g. F-3, E-1, A-1, C-1) cross-referenced to the V1 lens finding that authored it. Land every fold or escalate the gap explicitly in your report.

## §3 — Report-back format

Return: (a) confirmation that your file is written and untracked, (b) per-fold checklist (which folds landed; which deferred and why); (c) total minutes spent; (d) any escalation flag (new finding surfaced; binding-doc contradiction; fold inapplicable).

## §4 — Per-agent V2 fold packets

| Agent | Fold packet location | Folds | Hard cap | Sequencing |
|---|---|---|---:|---|
| **α-A** | CONSOLIDATED `§2.3` | 3 (A-1, A-2, A-3) | 15 min | parallel with α-C, α-E |
| **α-C** | CONSOLIDATED `§2.4` | 1 (C-1) | 15 min | parallel with α-A, α-E |
| **α-E** | CONSOLIDATED `§2.2` | 14 (E-1..E-14; **E-1 BINDING per CH7 REJECT; E-3 BINDING per CH5 REJECT**) | 45 min | parallel with α-A, α-C |
| **α-F** | CONSOLIDATED `§2.1` | 17 (F-1..F-17; **F-1 BINDING per CH6 REJ-2; F-2 BINDING per CH6 REJ-1**) | 45 min | **SERIALISE AFTER α-E** |

All folds and their full text live at `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/HARDENING-ALPHA-V1-CONSOLIDATED.md §2.X`. Each agent reads its §2.X section verbatim before editing.

## §5 — Post-α-V2 orchestrator steps

- Orchestrator commits all four V2 α-artefacts atomically once all four have returned. Commit subject `docs(sk-v14-alpha): V2 redispatch — V1 fold dispositions landed`. Body enumerates per-agent fold counts + cites the V1 CONSOLIDATED REJECT/REVISE references.
- CHALLENGE V2 dispatches the same seven lenses (CH1–CH7) under the unchanged `CHALLENGE-CONTEXT.md`; the V2 lens dispositions go to `research/alpha-hardening/V2/CH{1..7}.md` under the same write-only protocol.
- CHALLENGE V2 aggregator authors `HARDENING-ALPHA-V2-CONSOLIDATED.md` and commits all eight V2 hardening files atomically.
- If V2 converges (≥ 95 % per `ORCHESTRATOR.md §3Z`), CHALLENGE V3 fires as the confirming pass (same lenses; same artefacts; expected near-100 % since artefacts unchanged from V2). V3 aggregator commits.
- After V3 converges, the SK-V14 contract is locked. Per the SK-V14 ORCHESTRATOR-PROMPT "Do not relinquish control except at G-Omega user gate", the orchestrator does NOT pause for G-Alpha; it proceeds directly to S-P0 (Task #2).
