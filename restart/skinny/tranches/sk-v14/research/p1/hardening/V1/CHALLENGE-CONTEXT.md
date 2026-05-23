# S-P1 CHALLENGE V1 Dispatch Context — SK-V14 Profile Pass

Authored by the SK-V14 orchestrator after S-P1 V1 atomic commit (six P1 axis files; 2481 lines). Same seven lenses as Pass Alpha + S-P0; same write-only protocol; aggregator commits 8 hardening files atomically.

## §0 — Authority

Binding (read end-to-end):

1. `restart/prompts/skinny/PASS-1-PROFILE.md` §3 (CH1-CH6 specialised to S-P1).
2. `restart/prompts/ORCHESTRATOR.md` §3W (universal CH1-CH6 definitions); §3Z (convergence).
3. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` §CH7 (Overfit-Prune lens definition; binding from S-P0).
4. `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md` — the spec the P1 axis agents executed against.
5. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `HANDOFF.md` — SK-V14 contract; §2 telemetry binding incl. new columns (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`); §3 C-1..C-5 candidate slate.
6. `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — S-P0 prune list (74 findings; 3 architectural sequencing constraints).

## §1 — Artefacts under review

Six P1 axis files (committed atomically):
- `p1a-samply-mode-1.md` (340 lines; parse_only × 17 corpora; atos pipeline; LTO-fused dispatch_value envelope)
- `p1b-samply-mode-2.md` (320 lines; direct × 17 + typed × 11; 56 profiles; DirectParser::skip_value typed-plane finding)
- `p1c-samply-mode-3.md` (607 lines; mode-III × 17 × 4 probes; 8 ANOMs incl. alternate_scalar_plan misnaming)
- `p1d-pmu-cycles.md` (648 lines; 231 PMU rows; cycles+inst REACHABLE; PMC counters UNREACHABLE)
- `p1e-hot-leaf-attribution.md` (306 lines; CH2 Lock-14 mis-attribution census; –40 admit AUDIT-FALSIFIED)
- `p1f-results-delta.md` (260 lines; 75 rows; 8 schema escalations; 45/45 stale criterion-slope hot-leaf)

## §2 — V1 disposition focus

Per-lens lens-overlay per PASS-1-PROFILE §3:

- **CH1 CORRECTNESS** — Every hot-leaf cites samply symbol path + % self-time + source file:line? c/B from real PMU? 17/17 corpus coverage? Every `unprofiled` cell resolved? **Note:** P1-A and P1-B used `atos -inlineFrames` pipeline to recover inlined frames (headless equivalent of interactive samply record per `[samply-symbol-resolution]`); verify this satisfies CH1 vs. classic interactive-samply demand.
- **CH2 GENERALITY** — Hot leaves named to grammar-neutral primitives, not JSON-named code paths? P1-E surfaced 13/17 + 14/17 envelope mis-attribution (dispatch_value, parse_object_value_at_direct, etc.) — verify this is correctly flagged for S-P2 `parse-attribution` feature enablement.
- **CH3 REGRESSION** — Any §4 anomaly re-proposes a REDRESS route? Specifically check P1-C ANOM-1/2/3 vs REDRESS-126 zero-orphan guard already documented; check no §4 anomaly silently re-opens REDRESS 50-55, 60-72, 80, 82-84, 88, 89.
- **CH4 COST** — Reproducible profile? Every §1 method block carries verbatim commands? Run id + host triple + build flags present?
- **CH5 HIDDEN COUPLING** — Hot-leaf attribution implies parallel substrate / sidecar / retained cursor? Track 1 ≡ generated runtime vs Track 2 structurally independent — P1-B's `DirectParser::skip_value` finding (typed plane is structural-skip not typed-decode) flags substrate-walk-with-shape-validation primitive — verify this is correctly classified.
- **CH6 ANTI-PAPER-CLOSE** — Flame profile file exists on disk? Symbol resolvable? Specifically check: P1-D PMC counters `unavailable_from_current_export` documented as Lock 14 finding not paper-close; P1-C ANOM-4 dispatch_value folded symbol (parse-attribution off) — paper-close risk if not addressed in V2.
- **CH7 OVERFIT-PRUNE** — P1-D Δ vs SK-V13 = -1.0% c/B confirms zero-source-byte SK-V14 baseline; the audit-overlay column per row tracks the S-P0 prune list; verify no S-P1 finding re-introduces a fake @generated / scaffold-as-load-bearing / gate-relabel pattern.

## §3 — Discipline (write-only)

- **HARD CAP 30 min** per lens.
- **DO NOT** `git add` / `git commit`. Write to `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH{N}.md`. Aggregator commits all 8 atomically.
- Cite `path:line` on every claim. Use per-axis tables for "all 17 corpora" / "all 6 P1 outputs" claims.
- **Executable-verification mandate** (institutionalized): if you cite a path/file/symbol, verify it exists; if you cite a numerical claim, recompute it.

## §4 — Output structure (per lens)

Write to `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH{N}.md`. Same §4 structure as Pass Alpha + S-P0 CHALLENGE (disposition summary; per-artefact table; critical findings; V2 fold recommendations).

## §5 — Aggregator (separate dispatch)

V1 aggregator reads all 7 CH files; authors `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`; commits all 8 atomically with `docs(sk-v14-p1-hardening-V1): challenge V1 + consolidated`.

§3Z convergence (≥95% × 2 cycles, zero orphan REVISEs) gates **S-P2 dispatch** per PASS-1-PROFILE §6 + the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP. No user gate intervenes.
