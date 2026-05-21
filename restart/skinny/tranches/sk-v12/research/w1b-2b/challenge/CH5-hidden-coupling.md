# SK-V12 W1b-2b CHALLENGE CH5 - Hidden Coupling

Date: 2026-05-20.
Scope: Section 7.2, W1b-2b PLAN, PLAN-AUDIT, A1, A2, and A3.
Lens: hidden coupling in the CSS L4 lightningcss SOTA report/gate.

## Verdict

REVISE.

The Section 7.2 plan is close enough to preserve, but CH5 cannot accept it as
written because several authority boundaries remain expressed as report text
instead of executable gate checks. The companion report may name the right
schema and fields, yet hidden coupling can still enter through self-provided
Mbps, loose run-id/artifact binding, CSS-only JSON guard roots, direct
cssparser leakage into the lightningcss comparator path, and companion flag
collision handling.

This is a plan revision, not a source rejection. The revised plan should keep
the dedicated `sk-v12-css-l4-sota-v1` surface and the W1b-2b gate, but make the
following checks binding before implementation starts.

## Required Revisions

1. Do not trust report-provided Mbps.

   A1 lists `track1_mbps`, `track2_or_oracle_mbps`, `lightningcss_mbps`,
   `threshold_mbps`, `admission_margin_mbps`, and `sample_count` as report
   fields. A3 correctly says these must be derived from Criterion `new/`
   artifacts. The plan must make the gate recompute all lane Mbps and sample
   counts from:

   - `track1_generated_css_l4_decl_values/new/{benchmark.json,estimates.json,sample.json}`
   - `track2_cssparser_oracle/new/{benchmark.json,estimates.json,sample.json}`
   - `lightningcss_same_plane_fact_stream/new/{benchmark.json,estimates.json,sample.json}`

   The serialized report values may be consistency checks only. Any fallback to
   report JSON, `base/`, `change/`, hand-entered Mbps, or non-finite arithmetic
   is a CH5 failure because the report producer would become its own admission
   oracle.

2. Bind run id to every consumed artifact, not just to report identity.

   The top-level `run_id` shape check is insufficient. The gate must require
   Track 1, cssparser, lightningcss, equality facts, measured validation,
   benchmark artifacts, and profile artifacts to bind to the same W1b-2b
   `run_id` and to the single row
   `css_l4/declaration_values/direct_to_struct/main`.

   A stale W1b-1 or W1b-2a artifact path with fresh report text must fail
   closed. This includes paths that point at the right workload but do not bind
   to the W1b-2b run identity, the 187-byte fixture checksum, and the declared
   fact-stream output plane.

3. Reject CSS-only JSON guard roots.

   Section 7.2 and A2 say JSON guards must use an accepted JSON Criterion root
   or a fresh populated JSON guard capture. The plan must make this executable:
   `json_guard_state=not_refreshed:no_behavior_drift` is acceptable only with a
   no-behavior-drift proof and byte-identical `skinny/RESULTS.md`; otherwise
   the existing JSON gate must consume a populated JSON root with JSON fixture
   and SIMD scan rows.

   A CSS-only Criterion root under `nonjson_css_l4` cannot satisfy JSON guard
   proof, even if the CSS SOTA report itself validates.

4. Prove lightningcss comparator independence from direct cssparser use.

   Transitive `cssparser` inside lightningcss is not a defect. Direct
   `cssparser` API calls in the lightningcss same-plane comparator path are a
   defect because they collapse the SOTA comparator into the cssparser oracle.
   The revised plan should require an import/content audit over the
   lightningcss comparator lane that permits `lightningcss` and rejects direct
   comparator-path `cssparser` use.

   The cssparser oracle may remain the independent Track 2 correctness anchor,
   but it must not share Track 1 generated code and must not be used to produce
   the lightningcss measurement.

5. Make companion flag collisions shared and fail-closed.

   A2 describes the right no-write matrix, but CH5 needs the W1b-2b flag wired
   into the same companion-report count as the existing W1a/W1b report flags.
   `--skv12-css-l4-sota-report` must reject duplicate companion reports, mixed
   companion reports, write/update flags, volatile probes, missing paths,
   flag-as-path, and unrelated extra arguments through the shared companion
   parser.

   If `--check-results` or `--with-cost-facts` is present, the gate must
   validate the CSS report first and then continue into the existing JSON path.
   If neither is present, it should return after the CSS report PASS line
   without touching `skinny/RESULTS.md`.

## Blocking Drift To Fix

- A1 still says `redress_entry == REDRESS-124`; PLAN and PLAN-AUDIT correctly
  require `REDRESS-125` for W1b-2b. Use `REDRESS-125` only.
- The report path still points outside `research/w1b-2b/` in Section 7.2 and
  PLAN. If that legacy `research/w1b/` path is intentional, mark it as
  intentional and bind it to W1b-2b run identity; otherwise move the planned
  report artifact path into the W1b-2b area.
- Lock14 must be process evidence, not report text. `lock14_status` may claim
  pass only after `lock14_baseline::validate` runs in the same gate process.
- Lock16 must be explicit. For this scalar CSS row,
  `n/a:no_simd_or_asm_claim` is acceptable only when no SIMD/ASM admission is
  claimed and scalar/parity coverage is recorded.

## Acceptance Bar

CH5 can accept the plan after revision if the implementation instructions and
tests require:

- Criterion-derived Mbps, threshold, margin, and sample counts;
- same-run artifact binding across Track 1, cssparser, lightningcss, equality,
  benchmark, profile, and measured validation artifacts;
- executable rejection of CSS-only JSON guard roots;
- direct-cssparser rejection in the lightningcss comparator path;
- shared companion flag collision handling;
- no `skinny/RESULTS.md` movement for `PASS-MEASURED-BASELINE`;
- `REDRESS-125` as the only W1b-2b redress slot.
