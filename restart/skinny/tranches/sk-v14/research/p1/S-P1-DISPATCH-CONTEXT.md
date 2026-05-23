# S-P1 Dispatch Context — SK-V14 Profile Pass

Authored by the SK-V14 orchestrator after S-P0 closed G-S-P0-CONVERGED at `ff653fbe6`. SK-V14 contract durable; G-Alpha auto-signed per the SK-V14 ORCHESTRATOR-PROMPT pin. S-P1 dispatches next per `restart/prompts/skinny/PASS-1-PROFILE.md` + the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.

This file is the shared dispatch context. Each S-P1 agent reads §0 — §3 + its own per-agent section (§P1-A through §P1-F per PASS-1-PROFILE.md §2 scope matrix).

## §0 — Authority

Binding (read end-to-end):

1. `restart/prompts/skinny/PASS-1-PROFILE.md` — your contract; §2 scope matrix; §2.1 mandatory 17/17 corpus coverage; §2.2 frontmatter; §3 CH1–CH6 lens overlay; §7 hard caps (45 min/agent); §8 bbnf-lang specifics.
2. `restart/prompts/ORCHESTRATOR.md` — meta-binding §3W lens set; §3Z convergence; §8 non-negotiables (cold per-parse, sequential bench, single-cargo-per-target).
3. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` — SK-V14 fresh-session pin (R1 comparator rebind; R2 per-iter equality oracle; R5 production corpora).
4. `restart/skinny/tranches/sk-v14/SYNTHESIS.md` — DURABLE SK-V14 contract; §0.2 goalset (audit-zero baseline: JSON parse_only 0/17, JSON direct 0/17, JSON typed 0/17, CSS L4 0/24); §0.4 P-1..P-7 pre-blocks; §2 telemetry binding.
5. `restart/skinny/tranches/sk-v14/HANDOFF.md` — tranche handoff; §3 honest baseline; §7 41-element refusal-condition list.
6. `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — S-P0 prune list (74 findings; 3 architectural sequencing constraints).

## §1 — SK-V14 starting baseline (what S-P1 profiles)

The SK-V14 baseline is the SK-V13 close state with audit-falsified verdict overlay. No SK-V14 implementation work has landed yet — the bench harness, corpora, and comparator binding all remain in their SK-V13 form.

**What runs:**
- `cargo bench` against `skinny/RESULTS.md` 17 JSON corpora (twitter, citm_catalog, canada, apache_builds, github_events, update_center, mesh, random, gsoc-2018, marine_ik, instruments, numbers, unicode_mixed, unicode_escapes, unicode_basic, distinct_values, y_string_unicode).
- `samply record` against the same binaries with `debug=true`.
- The misbound `sonic_rs::from_slice::<Value>` comparator at `benches/json_parity.rs:87-102` (R1 has not landed; document the misbinding as a finding for S-P2 design, do not "fix" in S-P1).

**What's audit-falsified per the S-P0 prune list:** the ADMIT classifications on 5 JSON parse_only + 4 JSON direct + 7 JSON typed + 24 CSS L4 rows. The PROFILES themselves are still real — hot leaves are genuine symbol paths, c/B is genuine PMU output, deltas vs SK-V13 are computable. **S-P1 profiles the bench-harness reality and overlays the audit-corrected verdict per row** (AUDIT-FALSIFIED / AUDIT-SUSTAINED / AUDIT-PENDING).

**What's NOT in scope for S-P1:**
- R1 comparator rebind implementation (= C-2 wave; happens post G-Omega).
- R5 production corpora capture (= C-3 wave; happens post G-Omega).
- CSS L4 profiling at scale (the 7 hand-written templates + tiny embedded fixtures are S-P0-documented; S-P1 can profile what's there + document the absence of real corpora as a finding for S-P3).

S-P1's measurement IS the empirical floor S-P2's primitive design grounds against; S-P2's primitive design IS the input S-P3's wave plan consumes.

## §2 — Discipline (binding on every S-P1 agent)

- **HARD CAP: 45 min per agent** per PASS-1-PROFILE §7. At 40 min commit what you have; at 45 halt.
- **WRITE-ONLY for docs.** Do NOT `git add` / `git commit` your output artefact. Orchestrator/aggregator commits all 6 P1 outputs atomically after returns. (Cargo invocations + samply runs ARE allowed; they're profiling, not source mutation.)
- **No source mutation.** S-P1 is read-only against `skinny/` source. Output goes only under `restart/skinny/tranches/sk-v14/research/p1/`. Profile binaries land in `/tmp/skv14-p1/` (cite paths in artefact; do not commit binaries).
- **Cold per-parse only** per `[no-warm-benches]`.
- **Sequential bench, single invocation** per `[bench-sequential-regression]` + `[bench-single-run]` + `[test-output-to-file]` (redirect to file, grep over file; never re-invoke cargo per corpus filter).
- **Single cargo per CARGO_TARGET_DIR** per `[single-cargo-per-target]`. Recommended: each agent sets `CARGO_TARGET_DIR=/tmp/skv14-p1a-target` (use your agent letter) to parallelize without lock contention; alternatively serialize cargo invocations across agents.
- **samply discipline** per `[samply-symbol-resolution]`: `debug=true` profile + interactive `samply record` (NOT `--save-only` — that drops symbol resolution).
- **17/17 mandatory corpus coverage** per PASS-1-PROFILE §2.1. Float-heavy overfit (only canada/mesh/marine_ik/numbers) is REJECT per CH1. The string + unicode corpora carry the worst sonic-strict deltas and are load-bearing.
- **§2.2 frontmatter mandatory.** Per-corpus per-symbol table is the load-bearing artefact; prose without table fails CH1.
- **aarch64 only** — host triple is `aarch64-apple-darwin` per the user pin.

## §3 — Output structure

Each agent writes ONE file at the assigned path per PASS-1-PROFILE §2 / §5:

```
restart/skinny/tranches/sk-v14/research/p1/
├── p1a-samply-mode-1.md        ← P1-A (samply mode I: cold parse_only × 17 corpora)
├── p1b-samply-mode-2.md        ← P1-B (samply mode II: direct_to_struct + real_typed_struct × 17)
├── p1c-samply-mode-3.md        ← P1-C (samply mode III: masking probes + structural scan × 17)
├── p1d-pmu-cycles.md           ← P1-D (PMU + cycles-per-byte × 17 × workload)
├── p1e-hot-leaf-attribution.md ← P1-E (synthesis across A/B/C; resolve every unprofiled cell)
├── p1f-results-delta.md        ← P1-F (RESULTS extraction + Δ vs SK-V13 close)
```

Structure per agent (per PASS-1-PROFILE §2.2 frontmatter):

```
# SK-V14 P1-{X}: {Topic}
Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-23.
Baseline: SK-V14-open (audit-corrected SK-V13 close state; commit at HEAD).
Host triple: aarch64-apple-darwin.
Build flags: release + debug=true + (feature mask if relevant).
Profile tool: samply <version> (or PMU source for P1-D).
Corpus coverage: 17/17 (or explicit subset + reason).

## §1 — Method (verbatim, reproducible commands)
## §2 — Findings (per-corpus per-symbol table; file:line citations)
## §3 — Delta vs SK-V13 close (per row; Mbps + c/B + audit-overlay verdict per row)
## §4 — Anomalies + masking signals (flagged for S-P2)
## §5 — Sources (artefact paths + run ids)
```

## §4 — SK-V14 audit-overlay per row

For every cell in your per-corpus tables, add a column `audit_overlay_verdict ∈ {AUDIT-FALSIFIED, AUDIT-SUSTAINED, AUDIT-PENDING}` per SK-V14 SYNTHESIS §2 telemetry binding. Map from the SK-V13 audit pack:
- AUDIT-FALSIFIED: any row marked admit in ROLLING-SOTA-DELTA whose admit is reverted by S-P0 prune list (5 parse_only + 4 direct + 7 typed + 24 CSS = 40 rows; precise per-row mapping in `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §1`).
- AUDIT-SUSTAINED: rows the audit explicitly endorses (W5/W6/W7 decision-engine; bbnf-simd substrate; generated_json::parse_direct; generated_real_typed::parse_*).
- AUDIT-PENDING: rows where the audit didn't reach (often OPEN rows S-P1 will newly measure).

## §5 — Report-back format

Return: (a) confirmation file written + untracked, (b) per-finding count + corpus coverage achieved (17/17 or subset + reason), (c) any escalation (samply unavailable; bench harness fails to build; specific corpus crashes; etc.). Note any executable verification you ran (cargo build success; samply record exit code; PMU access status on macOS).

## §6 — Per-agent scope (§P1-A through §P1-F)

Each agent's scope is defined in `restart/prompts/skinny/PASS-1-PROFILE.md §2` row P1-{X}. Read your row verbatim before writing. Brief restatement:

- **P1-A:** samply mode I = cold per-parse `parse_only` workload, release+debug. All 17 corpora. Per-corpus table: flame profile path, top-20 self-time symbols, run id, host triple, build flags.
- **P1-B:** samply mode II = cold per-parse `direct_to_struct` + `real_typed_struct` workloads. Same 17 corpora. Product-plane counterpart of P1-A.
- **P1-C:** samply mode III = masking-probe workloads (`host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`) + structural-scan-only path. Same 17 corpora. Instrumentation-divergence counterpart.
- **P1-D:** PMU counters (cycles, instructions, branch-misses, L1/LLC misses) → cycles/byte for every corpus × workload. Re-run the masking-probe table. Establish c/B baseline for `gate-json`.
- **P1-E:** Per-corpus per-row top self-time symbol synthesis across P1-A/B/C. Resolve every `unprofiled` cell in `skinny/RESULTS.md` to a named symbol + % self-time + file:line. Classify hot leaf: scan / number / string / unicode / structural / tape / dispatch.
- **P1-F:** Extract every row of `skinny/RESULTS.md`. Compute Δ vs SK-V13 close per row. Classify per schema-v3 enum. Flag stale/absent telemetry fields against `restart/skinny/tranches/sk-v8/SPEC.md §0.4`.

P1-E + P1-F depend on P1-A/B/C output. If parallel-dispatched, P1-E + P1-F consume the committed artefacts in the CHALLENGE-fold cycle (V2). For V1 they read what's available + flag the gap.

## §7 — Post-S-P1

After all 6 P1 outputs + aggregator commit, CHALLENGE V1 dispatches per PASS-1-PROFILE §3 (CH1-CH6 + CH7 from S-P0 binding). §3Z convergence (≥95% ACCEPT × 2 cycles) gates S-P2 dispatch per PASS-1-PROFILE §6.

Per the SK-V14 ORCHESTRATOR-PROMPT "do not relinquish except at G-Omega", S-P2 dispatches directly after S-P1 converges; no user gate intervenes.
