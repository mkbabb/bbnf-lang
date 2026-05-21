# SK-V12 W1b-2b CHALLENGE V3 CH5 - Hidden Coupling

Date: 2026-05-20.
Scope: Section 7.2, W1b-2b PLAN-V3, CH5 V2 revision blockers, and the current CSS comparator / gate companion surfaces.
Lens: CH5 hidden coupling.

## Verdict

ACCEPT.

PLAN-V3 removes the CH5 V2 blockers without expanding the wave into a
RESULTS-renderer or mixed-root benchmark wave. The report/gate is now a bounded
companion-gate disposition: it consumes the retained W1b/W1b-2a CSS evidence,
verifies live Criterion `new/` measurements from the CSS root, and runs JSON
guards as a separate command against an explicitly JSON-populated root.

## Blocking Findings

None.

## Accepted Surfaces

1. Retained artifact file consumption: ACCEPT.

   CH5 V2 rejected path-string equality evidence. PLAN-V3 now requires the gate
   to read `track1-facts.txt`, `oracle-facts.txt`, `lightningcss-facts.txt`,
   `strict-equality.txt`, and `lightningcss-strict-equality.txt`, not merely
   trust report strings. It also requires the exact row id, plane,
   `input_fnv64=27240148e5780a54`, `input_bytes=187`, and
   `stream_fnv64=285dd62f19dea4a8`. The current retained fact files are
   byte-identical under SHA-256
   `caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`, so the
   planned check has concrete evidence to consume.

2. Fact stream hash/equality: ACCEPT.

   PLAN-V3 requires the three fact streams to be byte-identical and to match the
   report's `fact_stream_sha256`; equality artifacts must have `status=pass`,
   exact row id, and the retained W1b run id. That closes the hidden producer
   coupling where a stale fact path could have passed by name alone.

3. Run-id handling: ACCEPT.

   PLAN-V3 explicitly accepts
   `sk-v12-w1b-1:fixture-fnv64-27240148e5780a54` only for retained equality/fact
   artifacts while keeping W1b-2b's report disposition on `REDRESS-125` and
   `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`. This is the right separation: W1b-2b
   consumes landed comparator artifacts and live Criterion measurements; it does
   not pretend the retained fact streams were regenerated under the W1b-2b run
   id.

4. Lightningcss path isolation from cssparser: ACCEPT.

   Current source supports the planned isolation rule. `oracle_facts` owns
   `ParserInput`, `Parser`, and `StyleSheetParser`; `lightningcss_facts` owns
   `StyleSheet::parse`, lightningcss AST projection, and fixture-sidecar span
   emission. The Criterion lightningcss lane calls `lightningcss_facts`
   directly, not the cssparser oracle. PLAN-V3 adds a focused redress audit that
   rejects `lightningcss_facts` calling `oracle_facts`, `ParserInput`, `Parser`,
   or cssparser parser APIs; that is sufficient for CH5 because the boundary is
   narrow and executable by source inspection.

5. Mixed companion report parser behavior: ACCEPT.

   The existing companion parser already centralizes no-write/no-probe rejection
   and mixed-report rejection for the W1a/SK-V12 non-JSON reports. PLAN-V3
   requires the new `--skv12-css-l4-sota-report` flag to join that same parser
   and reject duplicate/mixed companion reports, missing paths, flag-as-path,
   write/update flags, volatile probes, and unrelated args. Allowing only
   no-write JSON co-flags is acceptable because V3 makes the redress path use
   CSS-only mode and treats mixed-root execution as non-authoritative.

6. CSS/JSON root separation: ACCEPT.

   V3's two-command protocol is the material fix: CSS SOTA consumes only
   `target/criterion/nonjson_css_l4/{track1_generated_css_l4_decl_values,
   track2_cssparser_oracle,lightningcss_same_plane_fact_stream}/new/`, while the
   JSON guard/stale check runs separately with
   `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion`. This prevents a CSS-only
   Criterion root from masquerading as JSON guard evidence and prevents JSON
   stale-results behavior from forcing W1b-2b to mutate `skinny/RESULTS.md`.

## Concrete Redress Checks

- Add focused tests for the new companion flag's single-path extraction,
  mixed-report rejection, write/probe rejection, flag-as-path rejection, and
  allowed no-write co-flags.
- Make the CSS SOTA gate compute Mbps only from each lane's
  `new/benchmark.json`, `new/estimates.json`, and `new/sample.json`; reject
  `base/`, `change/`, missing throughput bytes, non-finite means, or sample
  counts below 30.
- Consume the retained fact/equality artifact files during the gate run and
  compare their content to report fields and hashes.
- Record the `lightningcss_facts` isolation audit in REDRESS-125. If the audit
  finds cssparser parser APIs in the lightningcss path, route to BLOCKED/FAIL.
- Keep `skinny/RESULTS.md` byte-identical in W1b-2b; W5 owns close
  reconciliation.

## Non-Blocking Implementation Note

The schema should either carry explicit paths for both equality artifacts or
define their derivation from the retained W1b artifact directory in the report
validator. PLAN-V3's requirement that the gate read the retained files is enough
for acceptance; the redress implementation should avoid leaving equality file
selection implicit in ad hoc code.
