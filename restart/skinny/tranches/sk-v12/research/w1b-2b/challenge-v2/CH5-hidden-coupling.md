# SK-V12 W1b-2b CHALLENGE V2 CH5 - Hidden Coupling

Date: 2026-05-20.
Scope: Section 7.2, W1b-2b PLAN-V2, and `gate.rs` companion/Criterion root behavior.
Lens: CH5 hidden coupling.

## Verdict

REVISE.

PLAN-V2 fixes the first CH5 rejection's largest fault: CSS Mbps, threshold,
margin, and sample count are no longer report authority. The gate is required
to recompute them from the three Criterion `new/` lanes, and the new companion
flag is routed through the shared parser shape that already rejects write/probe
flags and mixed companion reports.

Two hidden-coupling gaps remain blocking before redress.

## Blocking Findings

1. Retained equality/fact artifacts are still path-string evidence.

   PLAN-V2 requires the report to name the retained W1b artifacts, but the gate
   checks are bounded to strings. That leaves `strict_output_equality`,
   `three_way_equality`, and `lightningcss_sequence_status` as report text
   unless redress separately proves freshness. Current artifacts show why this
   matters: `lightningcss-strict-equality.txt` and `strict-equality.txt` still
   carry `run_id=sk-v12-w1b-1:fixture-fnv64-27240148e5780a54`, while W1b-2b's
   planned report run id begins `sk-v12-w1b-2b:criterion-fnv64-`. A stale
   equality artifact with the right path would pass PLAN-V2's proposed gate.

   Required fix: make source-artifact freshness executable. Either regenerate
   the equality artifacts for W1b-2b, or keep them explicitly retained and have
   the W1b-2b gate read the files, require `status=pass`, exact row id, fixture
   SHA/input byte binding, and fact-stream byte equality/hashes across
   Track 1, cssparser, and lightningcss. The report should carry the consumed
   fact artifact hash, and the gate should compare it to the file content. A
   path-only check is not enough.

2. Direct-cssparser isolation for the lightningcss measurement is not an
   executable gate.

   The current source appears structurally acceptable: the Criterion lane calls
   `lightningcss_facts`, and that function validates via `StyleSheet::parse`,
   a lightningcss AST projection, and fixture-sidecar spans; direct cssparser is
   used by `oracle_facts`, not by the measured lightningcss lane. PLAN-V2,
   however, does not require an executable audit preserving that boundary.
   Future redress could accidentally let cssparser feed the lightningcss fact
   stream while still passing report-text equality fields.

   Required fix: add a bounded redress/gate check that rejects direct
   `cssparser` use in the `lightningcss_facts` path, or a focused source audit
   recorded as redress evidence. The rule can be narrow: `lightningcss_facts`
   may call lightningcss parse/projection and fixture-sidecar span emission,
   but must not call `oracle_facts`, `ParserInput`, `Parser`, or cssparser
   parser APIs.

## Accepted PLAN-V2 Surfaces

- Mixed companion reports: ACCEPT. The plan requires
  `--skv12-css-l4-sota-report` to join the shared companion-report parser and
  reject duplicate/mixed companion reports, write/update flags, volatile probes,
  missing paths, flag-as-path, and unrelated args.
- CSS-only Criterion roots: ACCEPT. With `--check-results` or
  `--with-cost-facts`, PLAN-V2 validates CSS first and then continues into the
  existing JSON path; a CSS-only root lacks the required JSON groups/SIMD rows
  and therefore fails closed.
- JSON guard co-flags: ACCEPT. Allowed co-flags are no-write only, and the
  CSS-only no-guard mode returns after the CSS gate.
- Stale report-only Mbps: ACCEPT. The plan makes Criterion `new/` artifacts
  the throughput authority and forbids `base/`, `change/`, and hand-entered
  Mbps fallback.
- Fixture path coupling: ACCEPT with the artifact freshness revision above.
  The fixture checksum `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`,
  187-byte input, exact row id, and intentional legacy `research/w1b/` artifact
  directory are adequate once the files themselves are consumed rather than
  merely named.

## Concrete Revision

Revise PLAN-V2 with one short "Source Artifact Freshness And Comparator
Isolation" paragraph:

- W1b-2b gate reads the retained W1b fact/equality files named by the report.
- It verifies the three fact files are byte-identical or share the report's
  `fact_stream_sha256`.
- It verifies equality artifacts have `status=pass`, the exact CSS row id, and
  an explicitly accepted retained-artifact run id, or else W1b-2b regenerates
  them with the W1b-2b run id.
- It records a focused source audit proving `lightningcss_facts` does not call
  the cssparser oracle path.

After that revision, CH5 should accept redress.
