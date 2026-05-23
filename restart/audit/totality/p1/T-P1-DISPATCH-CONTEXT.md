# T-P1 Dispatch Context — SK-V14 Totality Excavation Pass

Authored by the SK-V14 orchestrator concurrent with S-P2 dispatch. Pass Omega T-P1/T-P2/T-P3 runs alongside the skinny S-P1/S-P2/S-P3 chain per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP. T-P1 dispatches first per `restart/prompts/totality/PASS-1-EXCAVATION.md`.

Each T-P1 agent reads §0 — §3 + own per-agent section (§1A..1F per PASS-1-EXCAVATION.md §2 scope matrix).

## §0 — Authority

1. `restart/prompts/totality/PASS-1-EXCAVATION.md` — your contract; §2 scope matrix; §2 frontmatter; §3 CH1-CH6 lens overlay; §7 hard caps; §8 bbnf-lang specifics.
2. `restart/prompts/ORCHESTRATOR.md` §3 totality track; §3W; §3Z.
3. `restart/ARCHITECTURE.md` + `restart/MASTER-PLAN.md` — V1 greater spec surfaces (1A/1B/1C inventory against these).
4. `restart/locks/LOCKS.md` — 16 locks (1E load-bearing).
5. `restart/HANDOFF.md` — totality handoff (405 lines).
6. `skinny/REDRESS.md` (5041 lines; grep + offset) + `skinny/RESULTS.md` (185 lines) — empirical floor for 1D.

## §1 — SK-V14 audit-corrected baseline (binding ground truth)

SK-V14 Pass Alpha + S-P0 + S-P1 ran ahead of this T-P1; the durable findings are bound below. T-P1 catalogues evidence against the V1 spec; the SK-V14 work IS the most recent skinny-track lessons that 1D digests.

**Bound facts:**
- SK-V14 audit-zero baseline: JSON parse_only 0/17, JSON direct 0/17, JSON typed 0/17, CSS L4 0/24 per SK-V14 SYNTHESIS §0.2.
- S-P0 prune list: 74 findings (31 CRIT + 20 HIGH + 12 MED + 11 LOW) per `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`.
- **30 Lock 14 violations** in skinny generic crates (8 per-grammar provider modules in codegen/; 11 CRITICAL + 7 HIGH + 5 MED + 7 LOW per A3).
- **Pattern H = 67 hand-written per-grammar runtime files** in `crates/core/src/runtime/{json, css_l4, css_pretty, google_sheets, bbnf, csv, ebnf, bnf, math}/` (V13 baseline was 64; +3 from css_pretty addition).
- **Three architectural sequencing constraints** for S-P3 wave plan: R4 (regen-css xtask) must land BEFORE PRUNE-2; C-1 (PRUNE-3+PRUNE-4) must land BEFORE C-4 (PRUNE-5 W8/W9); PRUNE-4 sub-wave count is 9 not 8 (css_pretty).
- **9-grammar census** at workspace (re-verified 5 consecutive cycles in S-P0): bbnf/json/css_l4/css_pretty/google_sheets/ebnf/bnf/csv/math.
- **S-P1 hot-leaf census:** dispatch_value LTO-fused envelope (14 functions gated under `parse-attribution`); DirectParser::skip_value dominates typed 72.5-76.1%; CH2 13/17+14/17 envelope mis-attribution.
- W5 bbnf-regex / W6 e-graph / W7 CSP solver are LOAD-BEARING (per audit pack v4); W8 per-grammar policy + W9 same-substrate union are SCAFFOLD-ONLY (PRUNE-5 wires).

**SK-V14 work is the most recent skinny tranche** — 1D's "proved/disproved/pending table" incorporates SK-V14's audit-corrected findings as the binding empirical evidence the V1 spec must reflect.

## §2 — Discipline (binding)

- HARD CAP 45 min per agent per PASS-1-EXCAVATION.md §7.
- WRITE-ONLY. Orchestrator commits all 6 (+optional 1F-extras) T-P1 outputs atomically.
- T-P1 is **read-only against** `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md` (governance surfaces; only Pass Omega CRUD amends them, post-G-Omega).
- Cite spec-claim path:line ↔ impl path:line per row; no recalled LOC; no recalled symbol path.
- 1E proposes LOCKS-amendment **candidates only** (disposition is T-P3 §3C; merge is Pass Omega).
- Per `[doc-alongside-code]` `[no-deferrals]`: every divergence catalogued has a verdict (implemented / unimplemented / impl-exceeds-spec / unknown); UNKNOWN carries a verify_action.

## §3 — Output structure

Each agent writes ONE inventory at `restart/audit/totality/p1/{1A..1F}-{topic}.md` per PASS-1-EXCAVATION.md §5. YAML frontmatter per §2 + body sections (Executive Summary ≤200 words; Spec-Claim ↔ Implementation Table; Divergences Catalogued; Gaps; Open Questions). 1E additionally emits LOCKS-AMENDMENTS-CANDIDATE table.

## §4 — Per-agent scope

Per PASS-1-EXCAVATION.md §2 (read your row verbatim):
- **1A:** Substrate-layer evidence — Lock 1 (tape ∪ direct-to-struct union, 20-variant BIR alphabet, `&'i Tape<'i>`) against `runtime/src/tape/`, `ir/src/`, `runtime/src/grammars/`. Output `1A-substrate-evidence.md`.
- **1B:** Codegen-layer evidence — `BackendShape` 5-shape canon (ARCHITECTURE.md §7.3), `derive_backend_shape`, Lock 10 cost model, Lock 5 lowerer hierarchy against `codegen/src/lower/`, `codegen/src/lib.rs`, `passes/src/`. Output `1B-codegen-evidence.md`.
- **1C:** Runtime-layer evidence — generated parsers + `runtime/src/grammars/<g>/`. **Per-grammar runtime module census; hand-written-vs-generated audit (Lock 14).** SK-V14 binding: 67 hand-written files = Pattern H baseline; cite each per grammar. Output `1C-runtime-evidence.md`.
- **1D:** Skinny-track lessons digest — SK-V1..SK-V14 iterations empirically proved/disproved that V1 spec must absorb. SK-V14 audit-corrected baseline + S-P0 74-finding prune list + S-P1 hot-leaf census = the most recent empirical evidence. Output `1D-skinny-lessons.md`.
- **1E:** Locks evidence + amendment candidates — audit 16 locks against current code + skinny REDRESS. Honoured/drifted/over-stated/silent-must-add. **Candidates only**; T-P3 disposes. Output `1E-locks-evidence.md`.
- **1F:** Cross-corpus coherence + anti-pattern + past-corpora scan — multi-output permitted. Pattern H 67-file recurrence; co-derivation note (css_pretty addition); audit-overlay column adoption status. Output `1F-coherence-scan.md` (+ `1F-anti-pattern.md`, `1F-past-corpora.md` if scope warrants).

## §5 — Post-T-P1

After 6 (+optional) T-P1 outputs commit + CHALLENGE V1 (CH1-CH7 + aggregator) per PASS-1-EXCAVATION.md §3, §3Z convergence gates T-P2 dispatch per `restart/prompts/totality/PASS-2-RESEARCH.md`. G1 user-pin is OPTIONAL per ORCHESTRATOR.md §6 — per the SK-V14 ORCHESTRATOR-PROMPT pin "do not relinquish except at G-Omega", G1 auto-passes and T-P2 fires.
