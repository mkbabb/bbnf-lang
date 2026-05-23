# Pass S-P0 Dispatch Context — SK-V14 Overfit Audit (Skinny Track)

Authored by the SK-V14 orchestrator after Pass Alpha CHALLENGE V5 closed §3Z
LOCK at commit `00181742e`. SK-V14 contract (`SYNTHESIS.md` + `HANDOFF.md`)
is DURABLE; G-Alpha auto-signs per the SK-V14 ORCHESTRATOR-PROMPT pin; S-P0
is the binding first phase of the SK-V14 tranche per `PASS-0-OVERFIT-AUDIT.md
§Standing SK process loop`.

This file is the shared dispatch context. Each S-P0 axis agent reads §0 — §3
+ its own per-axis section (§A1 — §A6).

## §0 — Authority

Binding (read end-to-end before any output):

1. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` — your pass contract. §Scope = six axes (A1-A6); §CH7 lens definition (lines 62-87); §Procedure (lines 43-60); §Hard caps (25 min per axis, 30 min synthesis, ~60 min total wall).
2. `restart/prompts/ORCHESTRATOR.md` — meta-binding. §3W CH1–CH6 universal lens set; §3Z convergence (≥95% ACCEPT × 2 consecutive cycles).
3. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` — the SK-V14 fresh-session pin (R1-R10 goalset; PRUNE-first; "do not relinquish except at G-Omega").
4. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` — the DURABLE SK-V14 contract (post §3Z LOCK at `00181742e`); §0 close-condition; §0.4 P-1..P-7 pre-blocks; §3 C-1..C-5 candidate shortlist; §4 S-P3 constraints.
5. `restart/skinny/tranches/sk-v14/HANDOFF.md` — durable tranche handoff; §3 honest baseline; §7 41-element refusal-condition list; §8 V1→V5 V≤5 disposition.
6. `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` — campaign-wide bar.
7. `restart/locks/LOCKS.md` — 16 locks; **Lock 14 + Lock 1 are the recurrence vectors** A3/A6 specifically audit.

## §1 — Prior tranche audit pack (binding ground truth)

The SK-V13 audit pack served double duty: SK-V13's own post-tranche audit AND the prior-tranche audit Pass Alpha SK-V13→SK-V14 consumed. Your S-P0 builds on this — verify the findings still hold at SK-V14 starting state (= SK-V13 close state, unchanged since no SK-V14 implementation yet); look for new issues; confirm cleanliness of the SK-V14 contract relative to the audit pack.

Read end-to-end:

- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — cross-axis verdict.
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v2-json-validation.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v3-lock14-deep-scan.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v4-decision-engine-trace.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v5-cross-tranche-stability.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v6-comparator-integrity.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/sk-v13-audit-overfit-{css-measurement, decision-engine, generator-truth, json-parse-only, lock14-scan}.md` (5 per-axis files).

## §2 — SK-V14 starting baseline (audit-zero honest delta)

Per HANDOFF §3 + SYNTHESIS §0.2 + ROLLING-SOTA-DELTA (pending PRUNE-1+PRUNE-2 re-baseline):

```
JSON parse_only: 0 / 17  (5 fake-admit W14.1-5 to revert; 12 OPEN)
JSON direct:     0 / 17  (6 comparator-misbound to revert under R1; 11 OPEN)
JSON typed:      0 / 17  (11 comparator-misbound to revert under R1; 6 OPEN)
CSS L4:          0 / 24  (24 hand-written-template fake-admits to delete; PRUNE-2 + R4 regen)
```

Architectural pillars HOLD: W5 bbnf-regex (LOAD-BEARING), W6 e-graph (LOAD-BEARING extraction-only), W7 CSP solver fail-closed (LOAD-BEARING), bbnf-simd (grammar-neutral), OffsetFlags + Tape (grammar-neutral), `generated_json::parse_direct` + `generated_real_typed::parse_*` (real codegen), 15 CSS .bbnf grammars at `/grammar/css/l4/` (present, unwired — R4 makes them load-bearing).

Pattern recurrence vectors STILL PRESENT in source (PRUNE waves not yet dispatched):

- **30 Lock 14 violations** (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW) — to be addressed by PRUNE-3 + PRUNE-4.
- **64 hand-written per-grammar runtime files** in `crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}/` — Pattern H — to be addressed by PRUNE-4.
- **8 per-grammar provider modules** in `skinny/crates/codegen/` — Lock-14 recurrence vector — to be addressed by PRUNE-3.
- **7 CSS hand-written template files** with fake `@generated` header — to be addressed by PRUNE-2.
- **W8 per-grammar policy + W9 same-substrate union** SCAFFOLD-ONLY — to be wired by PRUNE-5.

## §3 — Discipline (binding on every S-P0 axis agent)

- **HARD CAP: 25 min** per axis (per `PASS-0-OVERFIT-AUDIT.md §Hard caps`). At 22 min finish-equivalent; at 25 halt.
- **WRITE-ONLY.** Do NOT `git add`. Do NOT `git commit`. Orchestrator (or aggregator) commits all six A1-A6 outputs atomically per the institutionalized pattern from Pass Alpha V2-V5. This avoids the V1 α-phase staging-race recurrence.
- Cite `path:line` on every concrete claim. Use per-X tables for "all targets" / "all rows" / "all violations" claims.
- Voice per `STYLE.md`: calibrated direct prose; archaic-permissive register; no metalanguage.
- Read-size preflight: `wc -l` before Read on any file > 2K lines.
- **Executable verification mandate** (institutionalized from Pass Alpha V3→V4): if you cite a shell command, build invocation, grep pattern, or file count — actually run it and quote the output. Documentary citation alone is paper-close.
- This pass is docs/synthesis only — no source touch, no implementation, no cargo builds (cargo metadata queries are OK).
- Report back: (a) file written + untracked, (b) per-finding count + severity (CRITICAL / HIGH / MED / LOW per `PASS-0-OVERFIT-AUDIT.md §Scope` pass criteria), (c) any escalation flag.

## §4 — Output structure

Each axis agent writes ONE file to `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-{axis-tag}.md`. Suggested structure:

```
# SK-V14 S-P0 Axis A{N} — {Axis Name}

## §0 — Disposition summary
Pass criterion (from PASS-0-OVERFIT-AUDIT.md §Scope row):
- Findings: CRITICAL=N1, HIGH=N2, MED=N3, LOW=N4
- Pass / Fail
- Confirms / extends SK-V13 audit pack: yes / no with delta
- New findings (not in SK-V13 audit pack): N5

## §1 — Methodology
{Executable verification commands run; files audited; grep patterns used}

## §2 — Per-finding ledger
| Severity | Finding | Citation | Status (NEW / CONFIRMS V13) |
| --- | --- | --- | --- |

## §3 — Pass criterion verdict
{Quote PASS-0 §Scope criterion verbatim; declare PASS or FAIL}

## §4 — Recommended prune actions
{Cross-reference to PRUNE-1..5 + R4..R8 + C-1..C-5 candidates in SK-V14 SYNTHESIS §3}
```

The synthesis agent (separate dispatch after all six axis agents return) consolidates §0 + §2 + §4 across all six axes per `PASS-0-OVERFIT-AUDIT.md §Procedure` step 1.

---

## §A1 — Measurement Integrity

Per `PASS-0-OVERFIT-AUDIT.md §Scope` row A1: corpora sizes, identical-number clusters, throughput plausibility vs SOTA, comparator same-run discipline.

**Pass criterion (verbatim from §Scope):** Every ADMITTED row in the prior tranche's ROLLING-SOTA-DELTA has (a) representative corpus ≥ 1 KB, (b) per-row distinct measurement, (c) plausible Mbps relative to byte-throughput ceiling.

SK-V14 overlay: the SK-V13 ROLLING-SOTA-DELTA carries 24 CSS ADMITTED + 5 parse_only ADMITTED + 4 direct ADMITTED + 7 typed ADMITTED — all audit-falsified per v1+v2+v6. Confirm: (i) the CSS corpora are still tiny embedded fixtures (85-357 bytes per v1 §6); (ii) the JSON parse_only admits still ship the eager-DOM `sonic_rs::from_slice::<Value>` comparator binding; (iii) no NEW admit slipped in between the audit and the SK-V14 close. Check `git log --oneline restart/skinny/ROLLING-SOTA-DELTA.md` to verify no commits since the audit pack landed.

**Output:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-css-measurement.md`.

---

## §A2 — Admit-Mechanism Integrity

Per `PASS-0-OVERFIT-AUDIT.md §Scope` row A2: for every admitted row, the source diff that achieved the admission, the comparator binding, the per-iteration equality oracle.

**Pass criterion:** No admit lands by gate-relabel; every admit cites a parser/codegen/SIMD source change; comparator is strict-vs-strict on the same plane.

SK-V14 overlay: per HANDOFF §3 + §7 refusal-list, 47 audit-falsified admit rows (25 CSS + 5 parse_only + 6 direct + 11 typed) are pending revert via C-5 (PRUNE-1 + PRUNE-2). Confirm: (i) the W14.1-5 source diffs still touch only `gate.rs` / `report.rs` / `lock14_baseline.rs` (no parser/codegen change); (ii) the comparator binding at `benches/json_parity.rs:87-102` still calls `sonic_rs::from_slice::<Value>` for parse_only and direct planes; (iii) no per-iteration equality oracle exists (R2 still required). Spot-sample the W14.1-5 commits.

**Output:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-admit-mechanism.md`.

---

## §A3 — Lock 14 Generic-Crate Scan

Per `PASS-0-OVERFIT-AUDIT.md §Scope` row A3: grammar-name leaks (string literals, byte literals, function/struct names, enum match arms) in nominally-generic crates.

**Pass criterion:** Zero CRITICAL or HIGH violations in skinny generic crates.

SK-V14 overlay: v3 found 30 violations (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW). Verify still present (will be remediated by C-1 = PRUNE-3 + PRUNE-4). Run `git grep -n 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar\|"json"\|"css_l4"\|css_pretty' skinny/crates/` and quote the count + sample 5 hits. Run `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` and quote the directory list (expect 8 per-grammar dirs).

**Output:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md`.

---

## §A4 — Generator-vs-Hand-Curated

Per `PASS-0-OVERFIT-AUDIT.md §Scope` row A4: for every generated module: round-trip test (delete + regen produces byte-equivalent). For every claimed grammar-derived parser: locate the grammar source + emission command.

**Pass criterion:** 100% round-trip pass; every "generated.rs" comes from a real `cargo xtask regen` command + a real `.bbnf` grammar source.

SK-V14 overlay: v1 + v4 found 7 CSS L4 providers use `include_str!()` of hand-written templates with fake `@generated` header; no `regen-css` xtask exists. Verify: (i) run `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/runtime crates/core/src/runtime` and quote the count; (ii) for each, locate the supposed grammar source (`.bbnf`) + the xtask command that emits it; (iii) for the 15 `.bbnf` grammars at `/grammar/css/l4/`, verify they exist but are NOT yet consumed by any xtask (R4 + PRUNE-2 will close this).

**Output:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-generator-truth.md`.

---

## §A5 — Decision-Engine Fold Integrity

Per `PASS-0-OVERFIT-AUDIT.md §Scope` row A5: the CSP+egraph+cost resolver wired into compile; per-grammar policy + union substrate wired to actual runtime.

**Pass criterion:** The resolver drives emission; no scaffold-only wave admits a row.

SK-V14 overlay: v4 confirmed W5 (bbnf-regex) + W6 (e-graph) + W7 (CSP) LOAD-BEARING; W8 (per-grammar policy) + W9 (same-substrate union) SCAFFOLD-ONLY. Verify: (i) `skinny/crates/passes/lib.rs:476-478` still wires the CSP solver fail-closed (run the cited line range); (ii) W8 + W9 still emit research/docs only without runtime consumption (PRUNE-5 + C-4 close this); (iii) no scaffold-only finding gets cited as load-bearing in the SK-V14 contract (cross-check SYNTHESIS §3 + §4).

**Output:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-decision-engine.md`.

---

## §A6 — Pre-Restart Pattern Recurrence

Per `PASS-0-OVERFIT-AUDIT.md §Scope` row A6: hardcoded grammars, hand-coded "generated" code, hand-written per-grammar runtime modules, renamed pre-restart code, backend-specific code in shared crates, combinator/monolithic mixes, backwards-compat shims.

**Pass criterion:** Zero CRITICAL Pattern H violations; every other pattern reads CLEAN.

SK-V14 overlay: Pattern H is the 64 hand-written per-grammar runtime files at `crates/core/src/runtime/{json, css_l4, …}/` (v3 + the audit pack body). Verify: (i) run `find crates/core/src/runtime -name '*.rs' -path '*/json/*' -o -name '*.rs' -path '*/css_l4/*' | wc -l`; (ii) confirm the 8-grammar directory list still matches the pre-PRUNE-4 state; (iii) sample 3 files for hand-written vs grammar-derived markers; (iv) scan for other patterns (combinator monolithic mixes, backwards-compat shims, renamed pre-restart scanners) — most likely CLEAN per the audit pack but verify per the V3 lens-depth lesson (run greps, don't just trust prior findings).

**Output:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md`.

---

## §Synthesis (separate dispatch after A1-A6 complete)

After all six axis agents commit-equivalent (write file, untracked), the orchestrator dispatches a synthesis agent that reads all six per-axis files + authors `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` consolidating findings into:

1. Cross-axis verdict (PASS / FAIL per axis; aggregate).
2. Prune list (cross-reference to SK-V14 SYNTHESIS §3 C-1..C-5).
3. Delta vs SK-V13 audit pack (new findings; closed findings).
4. CH1-CH7 challenge readiness (every claim file:line-cited + executable-verified).

The synthesis agent commits all 7 files (6 axis + synthesis) atomically with `docs(sk-v14-audit-overfit): synthesize SK-V14 S-P0 + 6-axis verdict`.

Then CHALLENGE V1 over the S-P0 outputs dispatches per `ORCHESTRATOR.md §3W` + `PASS-0-OVERFIT-AUDIT.md §Procedure` step 2-3 (CH1-CH6 + CH7); convergence per §3Z fires G-S-P0-CONVERGED; then S-P1 dispatches.
