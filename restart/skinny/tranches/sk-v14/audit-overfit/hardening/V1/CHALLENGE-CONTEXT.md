# S-P0 CHALLENGE V1 Dispatch Context — SK-V14 Overfit Audit Pass

Authored by the SK-V14 orchestrator after S-P0 synthesis aggregator
landed atomic commit `d4cbc8204` (7 files: 6 per-axis + SYNTHESIS-
AUDIT-OVERFIT.md). 74 findings (31 CRIT + 20 HIGH + 12 MED + 11 LOW);
5/6 axes FAIL + 1 PARTIAL PASS; S-P0 V1 verdict FAIL — PRUNE LIST
CONFIRMED.

CHALLENGE V1 dispatches per `PASS-0-OVERFIT-AUDIT.md §Procedure step 2`
+ `ORCHESTRATOR.md §3W + §3Z`. Same seven lenses as Pass Alpha; same
write-only protocol; aggregator commits all 8 files atomically.

## §0 — Authority

Binding (read end-to-end):

1. `restart/prompts/ORCHESTRATOR.md` §3W (CH1-CH6 universal lens definitions); §3Z (convergence).
2. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` §CH7 lines 62-87 (new lens definition); §Procedure step 2-3.
3. `restart/skinny/tranches/sk-v14/audit-overfit/S-P0-DISPATCH-CONTEXT.md` — the spec the 6 axis agents executed against.
4. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `HANDOFF.md` — the durable SK-V14 contract; §3 C-1..C-5 covers all 74 S-P0 findings.

## §1 — Artefacts under review

Seven S-P0 files (committed atomically at `d4cbc8204`):

- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (488 lines)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-css-measurement.md` (A1; 194 lines; 8 findings; FAIL)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-admit-mechanism.md` (A2; 9 findings; FAIL)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md` (A3; 30 findings, exact v3 reproduction; FAIL)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-generator-truth.md` (A4; 16 findings, 3 NEW; FAIL)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-decision-engine.md` (A5; 4 findings, 1 NEW MED; PARTIAL PASS)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md` (A6; 7 findings, 4 NEW; FAIL)

## §2 — Bound facts (do not re-litigate)

- 74 findings total across 6 axes; 63 CONFIRM V13 byte-for-byte; 11 NEW.
- All 74 findings covered by SK-V14 SYNTHESIS §3 C-1..C-5 slate (zero orphans).
- Three architectural sequencing constraints surfaced for S-P3:
  - R4 (regen-css xtask) MUST land BEFORE PRUNE-2 (else 7 CSS ADMITTED rows unrecoverable).
  - C-1 (PRUNE-3 + PRUNE-4) MUST land BEFORE C-4 (PRUNE-5 W8/W9) (else W8 re-deepens Lock-14).
  - PRUNE-4 sub-wave count is 9 not 8 (css_pretty added between V13 and SK-V14).

## §3 — Lens disposition focus for S-P0 V1

The S-P0 audit is largely CONFIRMATIVE (63/74 reproduce v13). The CHALLENGE wave reviews: (1) executable-verification quality across all 7 files (per the institutionalized V3→V4 lesson); (2) cross-axis coherence (e.g. the 8 vs 9 sub-wave count consistency); (3) prune-list completeness vs the 74-finding total; (4) sequencing constraint correctness; (5) any new audit pattern that would extend CH7's 5 criteria.

Per-lens focus:

- **CH1 CORRECTNESS** — every `path:line` citation resolves; every executable-verification command actually quoted (no documentary-only claims).
- **CH2 GENERALITY** — A3's 30 violations cover Lock 14 surface; A6's Pattern H scope is grammar-neutral; no JSON-specific scope creep.
- **CH3 REGRESSION** — 63 confirms reproduce v13 verbatim; 11 NEW findings don't silently re-open prior REDRESS routes.
- **CH4 COST** — synthesis prune list maps to SK-V14 SYNTHESIS C-1..C-5 with LOC budgets intact; sequencing constraints have realistic wave-cost implications.
- **CH5 HIDDEN COUPLING** — A4's discovery that JSON `generated.rs` is ALSO fake `@generated` extends fake-codegen scope; verify this doesn't introduce a Track 1 ≡ Track 2 collapse vector.
- **CH6 ANTI-PAPER-CLOSE** — A5 PARTIAL PASS verdict; ensure no scaffold-only finding gets paper-closed.
- **CH7 OVERFIT-PRUNE** — the meta-lens applied to the audit itself; verify no S-P0 finding becomes a fake @generated / scaffold-as-load-bearing pattern in its own write-up.

## §4 — Discipline (write-only protocol unchanged from Pass Alpha)

- **HARD CAP 30 min** per lens.
- **DO NOT** `git add` / `git commit`. Write to `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CH{N}.md`. Aggregator commits all 8 files (7 lens + CONSOLIDATED) atomically with `docs(sk-v14-audit-overfit-hardening-V1): challenge V1 + consolidated`.
- **EXECUTABLE-VERIFICATION MANDATE** — if you cite a shell command, count, or grep pattern in your disposition, run it yourself and quote the output.
- Cite `path:line` on every concrete claim.
- Voice per `STYLE.md`: calibrated direct prose; archaic-permissive register; no metalanguage.
- Docs/synthesis only — no source touch.
- Report back: (a) file written + untracked, (b) ACCEPT-rate + per-§ disposition counts, (c) any escalation.

## §5 — Output structure (per lens)

Write to `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/CH{N}.md`. Same V1 §4 structure as Pass Alpha CHALLENGE (disposition summary; per-artefact disposition table; critical findings; V2 fold recommendations).

## §6 — Aggregator (separate dispatch after CH1-CH7 complete)

Same as Pass Alpha aggregator pattern. Reads 7 CH files; authors `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V1/HARDENING-S-P0-V1-CONSOLIDATED.md`; commits all 8 files atomically.

§3Z convergence (≥95% ACCEPT × 2 consecutive cycles, no orphan REVISEs) gates **G-S-P0-CONVERGED**. Per `PASS-0-OVERFIT-AUDIT.md §Procedure step 4`, the prune list then feeds Pass Alpha's §0 goalset — but SK-V14's Pass Alpha already closed at §3Z LOCK on `00181742e`; the SK-V14 SYNTHESIS already incorporates this prune list. So G-S-P0-CONVERGED here gates **S-P1 dispatch** (Task #3) per the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.
