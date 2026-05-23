# CHALLENGE V1 Dispatch Context — Pass Alpha SK-V13 → SK-V14 Bracket

Authored by the SK-V14 orchestrator for the seven CHALLENGE lens agents
(CH1–CH7) + aggregator. Each lens agent reads §0 — §3 + its own lens
section (§CH-X). The aggregator reads §0 — §3 + every CH-X output.

## §0 — Authority

Binding (read end-to-end before any output, in this order):

1. `restart/prompts/ORCHESTRATOR.md` — meta-binding. §3W defines CH1–CH6 lens set; §3Z convergence; §8 non-negotiables.
2. `restart/prompts/pass-contracts/PASS-ALPHA.md` — Pass Alpha contract under review. §3 CHALLENGE pass; §4 goalset template.
3. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` — CH7 Overfit-Prune lens definition (§CH7 below for the dispatch-local restatement).
4. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` — the SK-V14 fresh-session pin (R1–R10 goalset; PRUNE-first sequencing).
5. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` — campaign-wide bar.
6. `restart/locks/LOCKS.md` — 16 locks; Lock 14 + Lock 1 are the recurrence vectors.

Audit pack (the honest baseline — every claim in Pass Alpha output must reconcile):

- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v{1..6}-*.md`

## §1 — Artefacts under review (Pass Alpha V1 cycle output)

Read all eight end-to-end (none are large; 213–660 lines each):

- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` — the master synthesis (350 lines; §0 goalset, §0.4 P-1..P-7 pre-blocks, §2 telemetry, §3 candidate shortlist C-1..C-5, §4 S-P3 constraints).
- `restart/skinny/tranches/sk-v14/HANDOFF.md` — tranche handoff (213 lines; bracket verdict, honest baseline, §6 next-move, §7 refusal conditions, §8 V1 disposition).
- `restart/skinny/tranches/sk-v14/research/alpha/alpha-A-results-extraction.md` (362 lines).
- `restart/skinny/tranches/sk-v14/research/alpha/alpha-B-competitor-deltas.md` (328 lines).
- `restart/skinny/tranches/sk-v14/research/alpha/alpha-C-redress-digest.md` (428 lines).
- `restart/skinny/tranches/sk-v14/research/alpha/alpha-D-validated-invalidated.md` (545 lines).
- `restart/skinny/tranches/sk-v14/research/alpha/alpha-E-candidate-shortlist.md` (660 lines).
- `restart/skinny/tranches/sk-v14/research/alpha/DISPATCH-CONTEXT.md` (206 lines; the spec the α-agents executed against).

The α-agents committed under a parallel-staging race — commit subjects (`docs(sk-v14-alpha): {tag}`) do not always match commit contents, but file contents in HEAD are byte-identical to authored versions per six independent agent reports. Treat the files in tree as the artefacts under review; the commit metadata is informational.

## §2 — Bound facts the lenses must respect (do not re-litigate)

The audit pack output is bound. The lenses do NOT re-audit SK-V13 — they audit Pass Alpha's *use* of the audit findings. Treat as ground truth:

- 0 / 43 SK-V13 admitted rows survive strict-vs-strict (per audit synthesis).
- Eight architectural pillars hold and carry forward (W5, W6, W7, bbnf-simd, OffsetFlags, Tape, `generated_json::parse_direct`, `generated_real_typed::parse_*`, 15 unwired CSS `.bbnf` grammars).
- Four falsification classes: 25 CSS admits (hand-written templates); 5 parse_only admits (gate-relabel); 4 direct + 7 typed admits (comparator misbinding); W8 + W9 (scaffold-only).
- 30 Lock 14 violations (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW; codex undercount by 43%).

## §3 — Lens-agent discipline

- **HARD CAP: 30 min** per lens agent. At 27 min commit-equivalent (write file) what you have; at 30 halt.
- **DO NOT git add. DO NOT git commit.** Write your CH{N}.md file to disk. The aggregator (separate dispatch) commits all 8 files atomically with one comprehensive message. This avoids the staging race that contaminated V1's α-phase commits.
- Cite `path:line` on every concrete claim. Quote excerpts from the artefacts under review (e.g., `SYNTHESIS.md:80-82`) when issuing REJECT or REVISE.
- Per-section disposition table: every § of every artefact gets ACCEPT / REVISE / REJECT with a one-line reason.
- This pass is docs/synthesis only — no source touch, no cargo, no benchmarks.
- Voice per `STYLE.md`: archaic-permissive register; no metalanguage; standalone prose.
- Report back: the file path you wrote, plus a one-paragraph disposition summary (overall ACCEPT-rate, count of REJECTs, count of REVISEs, any escalation flag).

## §4 — Output structure (per lens)

Write to `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CH{N}.md`. Suggested structure:

```
# CH{N} {Lens Name} — Pass Alpha V1 Disposition

## §0 — Disposition summary
- ACCEPT-rate: X%
- REJECT count: N (with one-line summaries)
- REVISE count: M (with one-line summaries)
- Critical findings: Y

## §1 — Per-artefact disposition table
| Artefact | § | Disposition | Reason |
...

## §2 — Critical findings
{REJECT and ACCEPT-conditional details with file:line citations}

## §3 — Recommended folds for V2 (if any)
{V2 dispatch instructions for the Pass Alpha α-agents to address findings}
```

The aggregator consolidates §0 + §3 across all seven lenses into the V1 verdict.

---

## §CH-1 — CORRECTNESS

Per `ORCHESTRATOR.md §3W` row CH1: "Every claim cites file:line, commit SHA, RESULTS row, or REDRESS entry that resolves. Falsifiability gates are measurable. Comparator deltas match the strictness plane."

Disposition focus for Pass Alpha SK-V14:
- Every citation in SYNTHESIS + HANDOFF + α-A..α-E must resolve (path exists; line range matches; quoted text matches).
- Falsifiability gates in candidate shortlist C-1..C-5 must be measurable (concrete commands, concrete thresholds).
- The audit-overlay verdicts per row in α-A must cite the correct validation §reference.
- The R1–R10 R-target acceptance criteria in SYNTHESIS §0.3 must be empirically verifiable.

REJECT triggers: unresolved citation; unmeasurable gate; audit-overlay verdict mis-cited; honest baseline disagreement with the audit pack.

---

## §CH-2 — GENERALITY (Lock 14)

Per `ORCHESTRATOR.md §3W` row CH2: "Lock 14 holds: no grammar-name leak; every proposed intervention is grammar-neutral and works for CSS L4 / Sheets / BBNF-self, not only JSON."

Disposition focus for Pass Alpha SK-V14:
- C-1 (PRUNE-3 + PRUNE-4 Lock-14 refactor cluster) must collapse the 8 per-grammar provider modules into a grammar-AGNOSTIC template. Verify the dispatch context's §α-E falsifiability gate (`grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO) is the right test, not a JSON-only check.
- C-3 (R4 regen-css pipeline) must structure the xtask to be grammar-neutral — pattern should generalise to a future `regen-{grammar}` family, not be CSS-specific in shape.
- C-4 (W8 + W9 wiring) must wire CSP solver's shape selection grammar-neutrally — no `if grammar == "json"` branches.
- Every candidate's same-wave consumer must work for the multi-grammar surface.
- The SYNTHESIS §4 S-P3 constraints must forbid grammar-name leaks in any new code.

REJECT triggers: any candidate or constraint that bakes a grammar name; any test that only verifies JSON-side; any same-wave consumer that's grammar-specific.

---

## §CH-3 — REGRESSION (REDRESS)

Per `ORCHESTRATOR.md §3W` row CH3: "No proposal re-opens a route in `skinny/REDRESS.md`; the pre-block list is correctly identified; no admitted row is silently regressed."

Disposition focus for Pass Alpha SK-V14:
- Verify α-C's REDRESS classification by sampling 5–10 entries (use `grep -n 'REDRESS' skinny/REDRESS.md | head -50` to navigate; do not full-read at 5041 lines).
- The §0.4 pattern pre-blocks (P-1..P-7) must cover every recurrence vector the audit pack identified.
- Candidate C-5 (PRUNE-1 + PRUNE-2 revert) must REDRESS-entry every reverted row — verify the scope: 5 parse_only + 24 CSS = 29 REDRESS entries minimum.
- Verify REDRESS 119/120 LIFTED disposition is correctly flagged.
- Check that no candidate re-opens a route the audit explicitly closed (e.g., re-using `sonic_rs::from_slice::<Value>` as comparator).
- Verify the HANDOFF §7 refusal conditions cover the recurrence-vector list.

REJECT triggers: missing REDRESS entry for a reverted row; missing pre-block; unflagged REDRESS-119/120 reopening; candidate that silently reverses a prior REJECT decision.

---

## §CH-4 — COST

Per `ORCHESTRATOR.md §3W` row CH4: "LOC budget, risk class, wave alignment, and hard cap are stated and realistic; same-wave consumer present per kernel/primitive."

Disposition focus for Pass Alpha SK-V14:
- α-E candidate shortlist must state LOC budget per candidate — verify reasonableness (C-1 is the biggest at 4.95k–8.3k per α-E report; C-2 medium; C-3/C-4/C-5 smaller).
- Risk classification (LOW/MED/HIGH) must match scope:
  - C-1 HIGH (architectural, multi-wave) ✓
  - C-2 MED (harness-local) ✓
  - C-3 MED (xtask + corpora) ✓
  - C-4 MED (wires existing scaffold) ✓
  - C-5 LOW (revert + REDRESS scribe) ✓
- Same-wave consumer rule per `[execute-planned-architecture]` and ORCHESTRATOR §8: every primitive lands WITH its hot-path caller in the same commit. Verify each candidate's same-wave consumer is named.
- Hard cap discipline: 20 min research / 15 min plan / 30 min redress per memory `[dispatch-hard-cap]`; 45 min redress for decision-engine wiring waves per the addendum amendment.

REJECT triggers: missing LOC budget; risk-class mismatch (e.g., C-1 marked LOW); missing same-wave consumer; hard caps absent or unrealistic.

---

## §CH-5 — HIDDEN COUPLING

Per `ORCHESTRATOR.md §3W` row CH5: "No parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or Track 1 ≡ Track 2 dishonesty; substrate union holds."

Disposition focus for Pass Alpha SK-V14:
- P-7 (Track 1 ≡ Track 2 dishonesty) is in the pre-block list. Verify the SYNTHESIS §2 telemetry schema preserves Track 1 / Track 2 plane separation; verify §4 S-P3 constraints forbid plane collapse.
- C-2 (comparator rebind) must not introduce a parallel substrate (e.g., a sidecar `parse_only` scanner). Verify R8's "distinct parse_only path" stays inside `generated_json` per ORCHESTRATOR-PROMPT.md R8, not a separate crate.
- C-1 (Lock-14 refactor) must replace per-grammar modules with a single generator — not a generator + 8 hidden adapter shims. Verify the architectural intent.
- The `G-SIMD-GRAMMAR-POLICY` clause in SYNTHESIS §4 (lines 276–281) must close the SIMD substrate-union hole.
- Lock 1 audit: no renamed pre-restart scanners; no parallel SIMD producer.

REJECT triggers: parallel substrate proposed; Track 1 ≡ Track 2 plane collapse allowed; SIMD wired without G-SIMD-GRAMMAR-POLICY enforcement; renamed-scanner Lock 1 violation re-introduced.

---

## §CH-6 — ANTI-PAPER-CLOSE

Per `ORCHESTRATOR.md §3W` row CH6: "No agent self-report of 'complete'/'wired'/'verified' stands without orchestrator-cited live evidence (bench row, samply symbol path, checkasm pass). No deferral to a future phase."

Disposition focus for Pass Alpha SK-V14:
- α-F's claim "no contradictions detected between my α-F synthesis and the peer α-A / α-C / α-D / α-E outputs" must be independently verifiable — sample-check by reading the actual α files vs SYNTHESIS extraction.
- SYNTHESIS §0.5 explicitly defers the §4.4 wave-by-wave gates to S-P3. This is a CONTRACTED deferral per PASS-ALPHA §4 — verify it is contracted, not paper-closed.
- HANDOFF §6 next-move chain (`CHALLENGE-V1 → G-Alpha → S-P0`) must not skip any binding gate.
- Verify no candidate's acceptance criterion is "documented" / "designed" / "planned" — every gate must be MEASURED.
- Triumvirate discipline: research / plan / redress in distinct commits per memory `[triumvirate-discipline]`. Verify SYNTHESIS §4 S-P3 constraints encode this.

REJECT triggers: paper-close in any candidate ("planned"/"designed"/"future-wave"); deferral outside the contracted §4.4 boundary; un-substantiated convergence claim; missing triumvirate constraint.

---

## §CH-7 — OVERFIT-PRUNE (new lens, per S-P0 binding)

Per `PASS-0-OVERFIT-AUDIT.md §CH7` (lines 62–87): the new lens checks that:

1. Every new code added is grammar-derived (template + grammar metadata + emission command) — never hand-written under a `// @generated` header.
2. Lock 14 generic-crate compliance preserved.
3. Every admit lands via a real parser/codegen/SIMD source change, measured against a strict-vs-strict comparator on the same plane, with a per-iteration equality oracle.
4. Every "generated" output passes a round-trip test (delete + regen ⇒ byte-equivalent).
5. No SCAFFOLD-ONLY landing counts as admit.

Disposition focus for Pass Alpha SK-V14:
- P-1 through P-7 pre-blocks in SYNTHESIS §0.4 must enumerate all five CH7 criteria as recurrence vectors. Verify mapping:
  - P-1 fake `@generated` ↔ CH7-1 (grammar-derived only).
  - P-2 mislabelled comparator ↔ CH7-3 (strict-vs-strict + per-iter equality).
  - P-3 tiny-fixture inflation ↔ CH7-3 partly (admit measurement honesty).
  - P-4 gate-relabel ↔ CH7-3 (real source change).
  - P-5 scaffold-as-load-bearing ↔ CH7-5 (no scaffold admit).
  - P-6 per-grammar provider modules ↔ CH7-2 (Lock 14).
  - P-7 Track-1≡Track-2 ↔ CH7 fall-through to CH5 (hidden coupling).
- C-3 (R4 regen-css pipeline) must include a round-trip test per CH7-4. Verify the falsifiability gate (round-trip xtask check) is explicit.
- C-4 (W8+W9 wiring) must require measured runtime consumption per CH7-5. Verify the falsifiability gate (named row shows runtime divergence).
- The SYNTHESIS §4 S-P3 constraints must inherit CH7-binding for downstream waves.

REJECT triggers: any P-X uncovered by CH7-N criteria; missing round-trip test for any "generated" output; missing measured-runtime-consumption gate for any scaffold-wiring candidate; CH7 binding not inherited by S-P3 constraints.

CH7 is the most consequential lens for SK-V14 — its REJECT triggers immediate plan revise OR redress revert per PASS-0-OVERFIT-AUDIT.md §CH7 final paragraph.

---

## §Aggregator — CONSOLIDATED.md (separate dispatch after CH1–CH7 complete)

After all seven lens files exist at `research/alpha-hardening/V1/CH{1..7}.md`:

- Read all seven CH files end-to-end.
- Author `research/alpha-hardening/V1/HARDENING-ALPHA-V1-CONSOLIDATED.md` per `ORCHESTRATOR.md §3Z step 4`:
  - The six (now seven) dispositions.
  - Cycle verdict: ACCEPT-rate (% across all per-§ dispositions); REJECT list (verbatim from each lens); REVISE list (verbatim).
  - Convergence test: ≥95% ACCEPT per `§3Z`. If converged, advance to V2 confirmation cycle (the §3Z rule requires two consecutive ≥95% cycles).
  - If not converged: list the fold dispositions for V2 dispatch (which α-agents to redispatch with what corrections).
- Commit all eight files atomically: `git add restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/ && git commit -m 'docs(sk-v14-alpha-hardening-V1): challenge V1 + consolidated'`. The aggregator owns the only commit for this wave to avoid the race the α-phase suffered.
