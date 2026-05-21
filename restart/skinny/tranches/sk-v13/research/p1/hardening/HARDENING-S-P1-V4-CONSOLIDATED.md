# SK-V13 S-P1 V4 Hardening Consolidated

Pass: S-P1 Profile. Cycle: V4.
Date: 2026-05-21.
Scope: consolidated CH1-CH6 challenge verdict for the SK-V13 S-P1 V4 profile cohort.
Output: this file.

## Verdict

`G-S-P1-V4-CONVERGED`: PASS for this cycle.

V4 is the narrow CH4 reproducibility fold requested by V3. It checks in durable
mode-III and CSS profiler source snapshots, preserves repo-relative rebuild
commands, verifies rebuilt binary hashes, and adds a checked-in direct/mode-III
summary reproducer. The six-lens challenge accepts the packet. Because the pass
framework requires two consecutive accepted hardening cycles, S-P1 is not yet
closed from V4 alone; V5 is required as the confirmation cycle.

| Lens | Disposition | Load-bearing reason |
|---|---|---|
| CH1 correctness | ACCEPT | Direct 17/17 and mode-III 85/85 coverage are explicit; unresolved, missing, and CSS nonparser rows stay non-admissions. |
| CH2 generality / Lock 14 | ACCEPT | JSON envelopes, typed-only rows, JSON-confirmed unicode/scanner candidates, and CSS timer/fact-sink evidence remain quarantined from grammar-neutral proof. |
| CH3 regression / REDRESS | ACCEPT | REDRESS 119/120, 96/97/98, pre-pin route families, and REDRESS-126 guardrails remain inline and require material differentials for future waves. |
| CH4 cost / reproducibility | ACCEPT | The former temp-only mode-III/CSS harnesses now have checked-in source, repo-relative manifests, rebuild commands, verified binary hashes, and reproducible TSV summary scripts. |
| CH5 hidden coupling | ACCEPT | Sidecar coverage, temp artefact paths, retained V1 limitations, direct/CSS/mode capture boundaries, and generated summaries are disclosed and bounded. |
| CH6 anti-paper-close | ACCEPT | The packet states all rows are profile signals, not gate admissions; residual CSS, typed, parse/typed rebuild, and comparator gaps remain routed rather than closed. |

Acceptance rate: 6/6 = 100%.
Consecutive accepted cycles: 1 (V4).

## V4 Improvements Banked

- `support/harnesses/mode3/` preserves the complete mode-III profiler source
  snapshot with repo-relative crate paths.
- `support/harnesses/css_profiler/` preserves the complete CSS declaration
  profiler source snapshot with repo-relative crate paths.
- `support/summarize_profile_rows.py` reproduces
  `/tmp/skv13-p1-v2/summary/direct_summary.tsv` and
  `/tmp/skv13-p1-v2/summary/mode3_summary.tsv`.
- `support/profile-provenance-v3.md` and
  `support/mode3-harness-provenance.md` record source hashes, rebuild commands,
  rebuilt binary hashes, and summary TSV hashes.
- P1-A through P1-F cite the V4 support files where they rely on regenerated
  direct or mode-III summaries.

## Carry-Forward Limitations

- Retained V1 parse/typed captures are accepted as auditable-only historical
  artefacts with binary hashes because the exact original cargo build command
  was not preserved.
- Branch/L1/LLC counters remain unavailable from the current xctrace export.
- CSS declaration-values profiling is timer/fact-sink dominated and does not
  identify a parser hot leaf.
- Ten typed rows still lack generated typed surfaces.
- No S-P1 row is a gate admission; every candidate remains routed to S-P2/S-P3
  and later wave measurement.

## Cycle Disposition

S-P1 V4 passes challenge but does not yet close S-P1. Dispatch S-P1 V5 as the
confirmation challenge cycle against the same V4 profile packet. If V5 accepts
at least 95% with no new fold required, S-P1 converges and S-P2 Research becomes
authorized.
