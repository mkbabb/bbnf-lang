# SK-V12 W1b-2b CH1 Correctness Challenge V3

Date: 2026-05-20.
Lens: CH1 correctness.
Plan under review: `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.
Verdict: ACCEPT.

## Scope Read

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` §4.
- `restart/skinny/tranches/sk-v12/SPEC.md` §0.4 and §7.2.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/challenge-v2/CONSOLIDATED.md`.

## Blocking Findings

None.

## V2 Correctness Blocker Review

1. Split CSS/JSON roots: fixed. PLAN-V3 uses two separate redress commands:
   CSS SOTA validation reads the normal CSS Criterion root without
   `--check-results`, then the JSON guard runs separately with
   `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion`. That removes the V2
   ambiguity where one `criterion_root()` had to be both CSS throughput
   authority and JSON stale-results authority.

2. Required telemetry fields: fixed. PLAN-V3 restores the SPEC §0.4 fields
   CH1 V2 identified as missing, including `track2_or_oracle_source_path`,
   `lightningcss_command`, `measured_validation_path`, and
   `profile_artifact`, and binds them to bounded validators. The plan also
   preserves the rest of the row identity, generated/source proof, comparator
   proof, and gate-context fields needed for a gate-consumed CSS row.

3. Strict equality: fixed. PLAN-V3 requires `strict_output_equality == pass`,
   `three_way_equality == pass:track1=cssparser=lightningcss`, explicit
   lightningcss sequence status, and independent cssparser oracle status.
   It also requires retained fact and equality artifacts to be read and
   content-verified, not only named.

4. `lightningcss_mbps + 1` comparator: fixed. PLAN-V3 derives
   `threshold_mbps = lightningcss_mbps + 1`, derives
   `admission_margin_mbps = track1_mbps - threshold_mbps`, and admits only
   when `track1_mbps > lightningcss_mbps + 1`. Equality at threshold routes to
   `PASS-MEASURED-BASELINE`, which matches the pin and SPEC §7.2.

5. Criterion authority: fixed. PLAN-V3 names the live `new/` files for all
   three lanes and requires `benchmark.json` throughput bytes equal 187,
   finite positive `mean.point_estimate`, and `sample.json.iters.len() >= 30`.
   It forbids fallback to `base/`, `change/`, report-only Mbps, or hand-entered
   values.

6. Artifact consumption: fixed. PLAN-V3 requires the gate to read
   `track1-facts.txt`, `oracle-facts.txt`, `lightningcss-facts.txt`,
   `strict-equality.txt`, and `lightningcss-strict-equality.txt`; verify pass
   status, row id, plane, input FNV, byte count, stream FNV, accepted retained
   run id, and `fact_stream_sha256`; and compare the three fact streams
   byte-for-byte. The comparator isolation source audit is also correctly
   routed as a redress-time fail-closed check.

7. REDRESS-125 routing: fixed. PLAN-V3 uses only
   `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA` and `REDRESS-125`, and routes
   `skinny/RESULTS.md` reconciliation to W5 close. That satisfies the V2
   requirement to remove the unbounded RESULTS movement branch from W1b-2b.

## Conditions Carried Into Redress

- The SPEC budget correction must remain limited to W1b-2b's report/gate/test
  estimate and must not expand owner paths beyond PLAN-V3.
- `skinny/RESULTS.md` must stay byte-identical in W1b-2b, including on
  `PASS-ADMIT-CANDIDATE`; W5 owns close reconciliation.
- The CSS gate must validate before any optional mixed-root JSON continuation;
  redress evidence should use the two-command protocol, not the mixed-root
  fallback.
- A missing or stale retained W1b/W1b-2a artifact is a W1b-2b
  `BLOCKED/FAIL`, not a reason to weaken artifact checks.

CH1 accepts PLAN-V3 for redress.
