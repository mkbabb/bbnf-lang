# Pass Alpha Dispatch Context — SK-V13 → SK-V14 Bracket, V1 Cycle

Authored by the SK-V14 fresh-session orchestrator for the six α-agents.
This file is the shared context. Each α-agent reads §0 — §3 + its own
per-agent scope section (§α-A through §α-F).

## §0 — Authority

Binding (read end-to-end before any output, in this order):

1. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` — the SK-V14 fresh-session pin (current orchestrator authority; supersedes prior tranche framings where conflicting).
2. `restart/prompts/pass-contracts/PASS-ALPHA.md` — your contract. §2 scope matrix names your role; §3 CHALLENGE lens set; §4 goalset template (the load-bearing artefact); §6 output structure.
3. `restart/prompts/ORCHESTRATOR.md` — meta-binding. §3 pass table; §3W universal CHALLENGE lenses CH1–CH6; §3Z convergence; §6 sign-off gates; §8 non-negotiables; §9 hard caps.
4. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` — campaign-wide bar (full SOTA + indefatigable).
5. `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md` — CSS L4 SOTA pin.
6. `restart/locks/LOCKS.md` — 16 settled architectural commitments. Lock 14 is the recurrence vector; CH7 Overfit-Prune lens is bound here.

## §1 — Audit pack (honest baseline)

Six S-P0 validation agents committed the audit (latest is `b24232776 cross-tranche stability + pattern emergence`; the prior five are visible in `git log --oneline -10`). The audit verdict is bound below. The honest baseline supersedes any nominal SK-V13 admit. Where SK-V13 RESULTS.md or ROLLING-SOTA-DELTA.md claim an admit that the audit falsifies, the audit wins; cite the validation file + § that falsifies or sustains.

Read end-to-end:

- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — cross-axis audit verdict + binding PRUNE list.
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v3-lock14-deep-scan.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v4-decision-engine-trace.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v5-cross-tranche-stability.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md`

Honest-baseline bind (do not re-litigate; cite when needed):

**SURVIVES** — architectural pillars carrying forward into SK-V14:
- W5 bbnf-regex extraction (LOAD-BEARING).
- W6 e-graph Language + cost (LOAD-BEARING, extraction-only).
- W7 CSP solver, 5 constraints, fail-closed (LOAD-BEARING; `passes/lib.rs:476-478`).
- `bbnf-simd` 52 files (grammar-neutral).
- OffsetFlags + Tape (grammar-neutral).
- `generated_json::parse_direct` (real codegen from grammar).
- `generated_real_typed::parse_*` (real codegen from grammar).
- 15 CSS `.bbnf` grammars at `/grammar/css/l4/` (present, unwired; R4 makes them load-bearing).

**DOES NOT SURVIVE** — falsified or downgraded:
- 25 CSS L4 admitted rows incl. SK-V12 W1b 2.54× headline — hand-written templates with fake `@generated` header; no `regen-css` xtask.
- 5 JSON `parse_only` admits W14.1–W14.5 — gate-relabel only; source diffs touch `gate.rs`/`report.rs`/`lock14_baseline.rs` and not the parser; comparator misnamed.
- 4 JSON direct admits — REAL parsers, comparator misbinding (`sonic_rs::from_slice::<Value>` eager DOM instead of strict struct deser per corpus).
- 7 JSON typed admits — REAL parsers, same comparator misbinding.
- W8 per-grammar policy — COSMETIC, no runtime consumption.
- W9 same-substrate union — COSMETIC, hardcoded constants.
- 30 Lock 14 violations — 11 CRITICAL + 7 HIGH + 5 MED + 7 LOW; 8 hand-written per-grammar provider modules in `codegen/` are the recurrence vector. Codex undercounted by 43 %.

Honest rolling delta after audit:

- JSON `parse_only`: 0 / 17
- JSON `direct`:     0 / 17
- JSON `typed`:      0 / 17
- CSS L4:            0 / 24

Campaign at zero on numbers; non-zero on architecture.

## §2 — SK-V13 antecedents (the four canonical surfaces + alpha template)

- `restart/skinny/tranches/sk-v13/SYNTHESIS.md` — prior tranche synthesis (271 lines).
- `restart/skinny/tranches/sk-v13/SPEC.md` — prior wave plan (1004 lines; grep + offset, do not full-read).
- `restart/skinny/tranches/sk-v13/HANDOFF.md` — prior tranche handoff (171 lines).
- `restart/skinny/tranches/sk-v13/research/alpha/alpha-{A..F}-*.md` — prior bracket's six α-artefacts (template + continuity).
- `skinny/RESULTS.md` (185 lines) — current results ledger.
- `skinny/REDRESS.md` (5041 lines) — current REDRESS ledger; grep + offset, do not full-read.
- `restart/HANDOFF.md` (405 lines) — totality handoff.
- `restart/skinny/ROLLING-SOTA-DELTA.md` (99 lines) — current rolling delta (will need honest re-baseline post-audit).

## §3 — Discipline (binding on every α-agent)

- **HARD CAP: 45 min.** At 40 min commit what you have; at 45 min halt. Per `restart/prompts/ORCHESTRATOR.md §9` + `PASS-ALPHA.md §2`.
- Cite `path:line` on every concrete claim. Use per-X tables for "all rows" / "all lenses" / "all targets" claims.
- Voice per `restart/README.md` + `docs/precepts/instructions/STYLE.md`: calibrated direct prose; archaic-permissive register; **no metalanguage** ("after the prior attempt", "this time", "lessons from earlier", "the codex orchestrator" are forbidden — write as a standalone synthesis, not commentary on session history).
- Read-size preflight: `wc -l` before Read on any file > 2K lines. REDRESS.md is 5041; SPEC.md is 1004; HANDOFF totality is 405. Use grep + offset.
- This pass is **docs/synthesis only**. Do NOT touch source; do NOT run cargo / benchmarks. Touch exactly the file(s) named in your scope.
- After writing, commit with the subject pattern `docs(sk-v14-alpha): <scope-tag>` and a 5–15 line body. The pre-commit hook fast-exits for docs-only commits.
- Report back: (a) commit SHA, (b) one-paragraph summary, (c) any escalation flag (new audit pattern, contradiction in binding docs, etc.).

---

## §α-A — Results extraction (audit-overlay)

Per `PASS-ALPHA.md §2 α-A`: extract every row of `skinny/RESULTS.md` + `restart/skinny/ROLLING-SOTA-DELTA.md`: per-corpus per-workload (parse_only, direct_to_struct, real_typed_struct + every workload present) Mbps + c/B + strictness plane + output plane + hot leaf attribution. Compute delta vs SK-V12 per row.

**SK-V14 overlay.** For every row currently marked ADMITTED, cross-validate against the audit pack. Each admitted row gets two columns:
- (a) nominal SK-V13 state (what RESULTS / ROLLING-SOTA-DELTA claim).
- (b) audit-corrected state with the specific validation-pack §reference that falsifies or sustains.

Mark falsified rows AUDIT-FALSIFIED; mark unaudited rows AUDIT-PENDING. The honest-baseline §1 bind gives you the audit-zero deltas; restate them per-row.

**Output:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-A-results-extraction.md`.
**Commit tag:** `results-extraction`.

---

## §α-B — Competitor deltas (rebound-comparator overlay)

Per `PASS-ALPHA.md §2 α-B`: for every comparator (sonic-rs strict / sonic-rs lossy / simdjson DOM / simdjson On Demand / yyjson default / yyjson minify / asmjson SWAR / asmjson AVX-512 / RapidJSON default / serde_json + lightningcss + cssparser for CSS L4 rows), compute per-corpus delta vs bbnf Track 1. Disclose strictness + output plane per comparator row.

**SK-V14 overlay.** Per R1 of `ORCHESTRATOR-PROMPT.md`, the SK-V13 `sonic_rs::from_slice::<Value>` was misbinding — eager DOM, not strict-vs-strict per plane. Correct comparators per plane:
- `parse_only` → `sonic_rs::Skipper` (structural skip).
- `direct` → sonic-rs strict struct deserialization per corpus.
- `typed` → per-corpus typed struct deserialization.

Document per row × plane × comparator: nominal SK-V13 delta under misbound comparator, what's measurable today (the misbound number), what's NOT measurable until R1 lands (the honest strict-vs-strict number), which SK-V13 deltas are now SUSPECT vs HONEST. Mark uncomputable rows COMPARATOR-PENDING-R1.

For CSS L4 rows, the analogous overlay: per `v1-css-l4-validation`, Mbps was inflated by tiny embedded fixtures + Criterion overhead. Mark CSS deltas CORPUS-PENDING-R5 + PIPELINE-PENDING-R4.

**Output:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-B-competitor-deltas.md`.
**Commit tag:** `competitor-deltas`.

---

## §α-C — REDRESS digest (pattern-level pre-blocks)

Per `PASS-ALPHA.md §2 α-C`: walk every REDRESS entry from SK-V13's cycle (`skinny/REDRESS.md`, 5041 lines — grep + offset). Classify each: admitted (commit SHA) / rejected (measurement evidence) / partial. Identify routes that should pre-block SK-V14. Identify routes that may admit under different framing.

**SK-V14 overlay.** The audit pack discloses *pattern-level* pre-blocks individual REDRESS entries do not yet capture. Add a `§pre-block-patterns` section enumerating at minimum:

- **P-1** Fake `@generated` header on hand-written templates (v1-css-l4-validation; pattern recurrence vector).
- **P-2** `sonic_rs::from_slice::<Value>` mislabelled as strict comparator (v6-comparator-integrity).
- **P-3** Tiny-fixture Criterion-overhead Mbps inflation, < 400 bytes (v1-css-l4-validation §6).
- **P-4** Gate-relabel as admit (v2-json-validation §1–4; W14.1–5).
- **P-5** Scaffold-research counted as load-bearing (v4-decision-engine-trace; W8 / W9 SCAFFOLD-ONLY presented as wired).
- **P-6** Per-grammar provider modules in generic codegen (v3-lock14-deep-scan; 8 modules in `codegen/` as Lock-14 recurrence).
- **P-7** Track-1 ≡ Track-2 dishonesty (cross-referenced from prior Lock-1 violations).

Per memory `[abrogate-before-patch]`: where a row's REDRESS history shows two-or-more reopen attempts against the same fake-pattern, propose DELETE rather than PATCH for SK-V14.

REDRESS 119 / 120 disposition: per USER-PIN-ADDENDUM both are LIFTED (HISTORY only). State this explicitly. The 13-row direct fixpoint + the SK-V11 close are wave-eligible under the addendum; each row must reopen with fresh material differential under the rebound strict-vs-strict gate, OR record an architectural-level intrinsic-block proof.

**Output:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-C-redress-digest.md`.
**Commit tag:** `redress-digest`.

---

## §α-D — Validated / invalidated / demoted / still-open ledger (audit-corrected)

Per `PASS-ALPHA.md §2 α-D`: update the validated / invalidated / demoted / still-open ledger from prior SK iterations. Cite commit SHAs + RESULTS rows + audit-pack §refs. Identify the load-bearing wins from SK-V13 that carry forward; identify the still-open items that become SK-V14 candidates.

**SK-V14 overlay.** The honest baseline §1 gives the immediate ledger. Restate per-item with citations:

- **VALIDATED (carries forward):** W5 / W6 / W7 / bbnf-simd / OffsetFlags + Tape / `generated_json::parse_direct` / `generated_real_typed::parse_*` / 15 CSS `.bbnf` grammars.
- **INVALIDATED (claimed admit; audit-falsified):** 25 CSS rows; 5 parse_only admits (W14.1–5); 4 direct admits; 7 typed admits. Enumerate the 4 direct + 7 typed corpora from ROLLING-SOTA-DELTA's ADMITTED rows.
- **DEMOTED (claimed wired; audit-evidence COSMETIC):** W8 per-grammar policy; W9 same-substrate union.
- **STILL-OPEN (SK-V14 candidate; below addendum bar):** all 51 JSON rows × 3 planes; all 24 CSS L4 features. The surviving 4 direct + 7 typed reopen under rebound comparators per R7.

State the audit-zero delta explicitly: JSON parse_only 0/17, JSON direct 0/17, JSON typed 0/17, CSS L4 0/24.

**Output:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-D-validated-invalidated.md`.
**Commit tag:** `validated-invalidated`.

---

## §α-E — Candidate shortlist (PRUNE-first)

Per `PASS-ALPHA.md §2 α-E`: synthesise SK-V13 cohort A/B/C reports under `restart/skinny/tranches/sk-v13/research/p1-p3/`. Produce a shortlist of ≤ 5 candidate interventions for SK-V14. Each candidate: file path, scalar reference status, checkasm test status, same-wave consumer plan, falsifiability gate (named rows + Mbps thresholds), LOC budget, risk class.

**SK-V14 overlay.** SK-V14 is structurally prune-then-rebuild per R3+R4+R5+R6+R7+R8 of `ORCHESTRATOR-PROMPT.md`. The five candidate slots map naturally to the R-targets. Recommended set:

- **C-1** — R3 PRUNE-3 + PRUNE-4 (Lock-14 refactor cluster). Replace `RuntimeProvider` enum with trait-based dispatch in `skinny/crates/`. Collapse 8 per-grammar provider modules in `codegen/` into ONE grammar-agnostic generator template consuming grammar source + workspace metadata. Same wave (sub-divided by grammar): refactor 64 hand-written per-grammar files in `crates/core/src/runtime/{grammar}/` into emitted output. Falsifiability: `find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar'` returns ZERO post-redress; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs.
- **C-2** — R1 + R2 (comparator rebind + per-iteration equality oracle). Three plane-correct strict comparators; per-iter equality inside the timing region. Falsifiability: bench harness emits an equality-pass column per iter; xtask gate-json rejects any row whose equality column is empty.
- **C-3** — R4 + R5 (regen-css pipeline + production corpora). `cargo xtask regen-css` consuming the 15 `.bbnf` files at `/grammar/css/l4/`; `skinny/corpora/css-l4-sk-v14/` with Bootstrap + Tailwind + Material + Animate (~960 KB). Falsifiability: round-trip xtask check returns clean; corpora dir > 800 KB.
- **C-4** — R3 PRUNE-5 (W8 + W9 scaffold → load-bearing). CSP-chosen shape produces measurable runtime divergence on a named pre-wave row.
- **C-5** — R3 PRUNE-1 + PRUNE-2 (clean revert of fake admits). PRUNE-1: revert W14.1–W14.5 in RESULTS + ROLLING-SOTA-DELTA, REDRESS cites v2 §1–4. PRUNE-2: delete 7 CSS hand-written template files + their `include_str!`'d generated.rs; revert 24 CSS L4 admitted rows; REDRESS per row cites v1 §1–6. Falsifiability: post-redress `git grep -l '@generated' crates/core/src/runtime` excludes any file produced by hand; ROLLING-SOTA-DELTA shows CSS L4 0/24 + parse_only 0/17.

R6 / R7 / R8 (re-admit waves) are downstream CONSUMERS of C-1 through C-5; they belong in the SK-V14 wave program after these candidates land, but are not standalone Pass Alpha candidates.

If a different five-candidate set fits the addendum better, propose it explicitly with justification.

**Output:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-E-candidate-shortlist.md`.
**Commit tag:** `candidate-shortlist`.

---

## §α-F — SK-V14 SYNTHESIS + HANDOFF contract draft

Per `PASS-ALPHA.md §2 α-F` + §6 output structure. Compose TWO files:

1. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` — master synthesis (carries §0 goalset / close-condition per `PASS-ALPHA.md §4`).
2. `restart/skinny/tranches/sk-v14/HANDOFF.md` — tranche handoff (ready-for-S-P0 verdict + next-move line).

You may use peer α-A through α-E outputs IF they are committed at dispatch time (check `git log --oneline -20 -- restart/skinny/tranches/sk-v14/research/alpha/`). Otherwise synthesise directly from raw sources — CHALLENGE V1 will catch divergence; V2 reconciles.

**SK-V14 overlay.**
- §0 close-condition IS R10 verbatim: every JSON cell + every CSS feature ADMITs > strict-vs-strict OR carries an architectural-level intrinsic-block proof; campaign indefatigable; close = full ADMIT or per-row architectural intrinsic-block proofs cover everything.
- §0 goalset row enumeration: 51 JSON rows × 3 planes / 17 corpora + 24 CSS L4 features (per `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`). All currently zero-admit per the audit.
- §0 pre-blocked routes: the pattern-level pre-blocks from α-C (or your own enumeration of P-1 through P-7 if α-C unavailable).
- Telemetry schema per `PASS-ALPHA.md §4.3` stands; add columns for per-iteration equality oracle confirmation + audit-overlay verdict per row (AUDIT-FALSIFIED / AUDIT-SUSTAINED / AUDIT-PENDING).
- §4.4 wave-by-wave gates are deferred to S-P3 in SK-V14/SPEC.md per the PASS-ALPHA explicit boundary. State the deferral in §0 with a forward pointer.

HANDOFF.md carries:
- Bracket verdict (SK-V13 closed per the audit; SK-V14 opens prune-first).
- Honest baseline summary (8 SURVIVES + 4 DOES-NOT-SURVIVE).
- Pre-S-P0 readiness: working tree clean; SK-V14 dirs seeded; ORCHESTRATOR-PROMPT.md committed at 496a81417; DISPATCH-CONTEXT.md committed alongside.
- Next move: dispatch S-P0 Overfit Audit Pass (6 agents A1-A6 per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`).
- Pass Alpha bracket V1 dispositions pending CHALLENGE convergence.

The SYNTHESIS must read as a standalone tranche-opening document. Voice: archaic-permissive, no metalanguage. "Pass Alpha brackets SK-V14 against an audit-corrected baseline" is correct; "after the prior orchestrator's credit cap" is forbidden.

**Output:** `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `restart/skinny/tranches/sk-v14/HANDOFF.md` (one commit, both files).
**Commit tag:** `synthesis + handoff contract draft`.
