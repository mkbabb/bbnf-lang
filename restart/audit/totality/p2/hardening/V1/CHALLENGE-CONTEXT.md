# T-P2 CHALLENGE V1 Dispatch Context — SK-V14 Totality Research Pass

Authored by SK-V14 orchestrator after T-P2 V1 atomic seed commit (7 files: 6 T-P2 dossiers + DISPATCH-CONTEXT). Six lenses (CH1-CH6 per PASS-2-RESEARCH.md §3) + CH7 (binding from S-P0 carry-forward for SK-V14 consistency). Same write-only protocol; aggregator commits 8 hardening files atomically.

## §0 — Authority
1. `restart/prompts/totality/PASS-2-RESEARCH.md` §3 (CH1-CH6 specialised to T-P2)
2. `restart/prompts/ORCHESTRATOR.md` §3W + §3Z
3. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` §CH7 (binding from S-P0 carry-forward)
4. `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`
5. `restart/locks/LOCKS.md` (Lock 1 + Lock 14 + Lock 16 load-bearing)

## §1 — Artefacts under review (6 T-P2 dossiers at V1 cycle)
- `2A-sota-landscape.md` (168 lines; 14 sources; 9 grounded + 5 refuted; 5 LACs)
- `2B-primitive-vocabulary.md` (287 lines; 18 sources; Layer 0 = 138 vendored macros + Layer 1 = 9 contracts (6 admitted, 3 SKELETON); 5 LACs)
- `2C-grammar-neutrality.md` (457 lines; 9 sources; primitives classified grammar-neutral vs JSON-only-by-shape; 7-step onboarding test; LAC-2C-02 grammar-SHAPE leak escalation)
- `2D-cost-model.md` (~200 lines; 12 sources; 7 grounded + 5 refuted; 5 LACs; 5-shape canon defends finite-set but 4/5 lowerers absent)
- `2E-host-arch-esoterica.md` (419 lines; 28 sources; 13 aarch64 PRIMARY + 9 x86 SECONDARY; per-entry citation+primitive+gate columns; BCAX NEW)
- `2F-parse-that-gaps.md` (583 lines; 24 sources; 9 primitive gaps; upstream-vs-vendor decisions; 3 load-bearing refutations; 4 LACs)

**Cohort total:** ~105 primary citations; ~27+ LACs; ~30+ refutations across dossiers.

## §2 — V1 disposition focus per `PASS-2-RESEARCH.md §3`
- **CH1 CORRECTNESS:** every paper cited exists and carries claimed finding; every library-source citation resolves to claimed path:line at HEAD; benchmark numbers trace to corpus + platform; refuted-technique rows match literature's actual position. Verify SHA-pinned source citations (FFmpeg `08571418`, dav1d `1718ff9a`, simdjson `168ef580`) at HEAD via curl raw-content (LAC-1E-12 mandate).
- **CH2 GENERALITY:** Lock 14 holds; every primitive grounded grammar-neutrally; 2C generalisation story shows transfer to CSS L4 / Sheets / BBNF-self. A technique grounded JSON-only-by-shape (e.g. 2C refuted set) explicitly carved out per Lock 14 v+1.
- **CH3 REGRESSION:** no dossier grounds a route already refuted in `skinny/REDRESS.md`; REDRESS 88/89/96/97/98 pre-blocks honored per 2E + 2F.
- **CH4 COST:** every grounded primitive carries admission cost (scalar reference + checkasm parity per Lock 16); same-wave consumer named; LOC/risk realistic; no orphan-kernel research.
- **CH5 HIDDEN COUPLING:** no grounded design implies parallel substrate / sidecar producer / Lock 1 violation. 2D CollapsedStage research keeps mask stream transient. 2B Layer 0/Layer 1 clean two-layer dependency.
- **CH6 ANTI-PAPER-CLOSE:** no dossier claims technique "validated"/"proven" on citation density alone; reference-stuffing flagged; every grounded technique states bbnf-specific transfer reason. No deferral to "later research pass".
- **CH7 OVERFIT-PRUNE:** SK-V14 audit-overlay column discipline preserved; no fake-pattern recurrence in dossier text; refutations are honest (e.g. asmjson non-admissible / yyjson scalar-wins are first-class outputs not paper-close cover).

## §3 — Discipline
- HARD CAP 30 min/lens.
- WRITE-ONLY (no git add/commit). Aggregator commits 8 atomically.
- Cite path:line; **executable verification mandate** (LAC-1E-12 procedural addendum institutionalized) — every cite must be re-executed at HEAD.
- §3Z: target first ≥95% ACCEPT-cycle on V1.

## §4 — Output: `restart/audit/totality/p2/hardening/V1/CH{N}.md` per V1 §4 structure. Aggregator at `HARDENING-T-P2-V1-CONSOLIDATED.md`.
