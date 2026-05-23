# S-P2 CHALLENGE V2 Dispatch Context — SK-V14 Research Pass

Authored by SK-V14 orchestrator after S-P2 V2 atomic micro-fold commit (4 amended axis files: P2-B SHA pinning, P2-C 3-candidate demotion, P2-D 1-candidate demotion, P2-F 6-sub-fold packet). P2-A and P2-E locked at V1 (7/7 lens ACCEPT). Same seven lenses; write-only; aggregator commits 8 hardening files atomically.

## §0 — Authority
1. `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 (CH1-CH6 specialised to S-P2)
2. `restart/prompts/ORCHESTRATOR.md` §3W + §3Z
3. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` §CH7
4. `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md` — V1 aggregator + fold-packet authority
5. `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH{1..7}.md` — V1 lens reports (carry-forward)
6. `restart/locks/LOCKS.md` (Lock 1 + Lock 14 + Lock 16 load-bearing)

## §1 — Artefacts under review (6 P2 axis files at V2 cycle)
- `p2a-sota-teardown.md` (V1-LOCKED; 367 lines; no V2 edits — confirm zero drift)
- `p2b-dav1d-process.md` (V2 amended; 217 lines; FFmpeg `08571418...` + dav1d `1718ff9a...` SHAs pinned at §5.1 lines 183-185; minor V1-consolidator §5.4→§5.3 register correction surfaced)
- `p2c-arch-esoterica.md` (V2 amended; 143→164 lines; 8→5 active candidates; C-P2C-1/6/7 demoted to `§2.X — Non-candidate inventory (zero P1-antecedent at SK-V14)` with full technical content verbatim + disposition stamp)
- `p2d-substrate-tape.md` (V2 amended; 257→254 lines; 3→2 active candidates; C-P2D-3 demoted to `§1.6(d)` substrate-side observation with identifier stub gap-note in §2)
- `p2e-parse-that-gaps.md` (V1-LOCKED; 342 lines; no V2 edits — confirm zero drift)
- `p2f-grammar-neutral.md` (V2 amended; 334→360 lines; 6 sub-folds: Fold-2 C8 DEMOTED + Fold-6 SKIPPED per `[no-deferrals]` default; Fold-3 C6/C7/C10/C12/C13 disposition stamps with C12 reframed CH4-ACCEPT per CF-1; Fold-4 C10 scalar-ref `crates/bbnf-simd/src/scalar/byte_context_64.rs`; Fold-5 C13 scalar-ref `crates/bbnf-simd/src/scalar/bcax_64.rs`; NF-CH6-3 C2 upgrade with P2-E Gap 6 three-way composition; NF-CH6-4 cross-axis tracking §2.Y for long-string-body SIMD scan consolidation)

## §2 — V2 disposition focus per `PASS-2-RESEARCH.md §3`
- **CH1 CORRECTNESS:** verify V2 folds discharge V1 REVISEs cleanly; confirm SHA inheritance correctness (P2-B); verify demoted-candidate technical content preservation (P2-C C-P2C-1/6/7, P2-D C-P2D-3, P2-F C8); confirm scalar-ref path:line claims at HEAD (P2-F C10/C13/C2).
- **CH2 GENERALITY:** Lock 14 holds across all amended axes; demotions do not introduce JSON-only divergence; cross-axis tracking note (P2-F §2.Y) consolidates correctly without losing grammar-neutrality.
- **CH3 REGRESSION:** no V2 edit re-opens REDRESS routes; demoted candidates remain pre-blocked.
- **CH4 COST:** C12 reframing per CF-1 verified (scalar-ref exists at `scan.rs:32`); C10/C13 Stage-A scalar-refs validated; F-V2-P1ABC-RERECORD CH2/CH4 dual-gating documented in V2 hardening context.
- **CH5 HIDDEN COUPLING:** substrate-union YES still holds (P2-D §1.6(d) demotion does not break union claim); no parallel substrate introduced by demotions.
- **CH6 ANTI-PAPER-CLOSE:** P2-F NF-CH6-4 cross-axis tracking note is exemplary anti-paper-close pattern (3 axes naming same primitive — S-P3 binding to one canonical name); demoted-candidate disposition stamps prevent silent re-introduction.
- **CH7 OVERFIT-PRUNE:** demotions do not introduce overfit; gap-note identifier preservation maintains cross-tranche stability per SK-V14 audit-overlay column discipline.

**Special V2 attention — V1-LOCKED axis drift audit:** P2-A + P2-E are V1-LOCKED. CH lenses must confirm zero V2 drift on these two files via diff against V1 commit `b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b`. Any unexpected drift = REVISE.

**Special V2 attention — V1 consolidator §5.4→§5.3 register correction:** V1 CONSOLIDATED §3 fold packet for P2-B named source as P2-A §5.4; actual SHAs reside in P2-A §5.3. V2 hardening context (this doc) carries the correction. V2 aggregator must propagate to consolidated doc.

## §3 — Discipline
- HARD CAP 30 min/lens. WRITE-ONLY (no git add/commit). Aggregator commits 8 atomically.
- Cite path:line; executable verification mandate (`grep -n`, `wc -l`, `find` for any LOC claim).
- §3Z gate evaluation per lens: first ≥95% cycle on V2; predicted V2 → V3 → LOCK trajectory.

## §4 — Output: `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH{N}.md` per V1 §4 structure. Aggregator at `HARDENING-S-P2-V2-CONSOLIDATED.md`.
