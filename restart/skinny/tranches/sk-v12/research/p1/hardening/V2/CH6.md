# SK-V12 S-P1 Hardening V2 CH6: Anti-Paper-Close

Disposition: ACCEPT.
Date: 2026-05-20.
Lens: CH6 anti-paper-close.
Scope: SK-V12 S-P1 packet after `d1e6938a`, V1 CH6, V1 fold revisions,
`skv12-p1-capture-manifest.md`, P1-A through P1-F, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, and `/tmp/skv12-p1` evidence.
Output: this file.

## Findings

### CH6-1 - V1 self-time gap is folded with citable exports

Disposition: ACCEPT.

V1 CH6 required either exported/symbolicated self-time summaries or an explicit
downgrade of hot-leaf tables to source-map evidence only
(`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH6.md:75`,
`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH6.md:80`,
`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/CH6.md:171`). The V1
fold chose the export path: all 34 parse Time Profiler bundles were exported,
all 48 product rows were recaptured under
`/tmp/skv12-p1/direct-xctrace/time-profiler-v2`, and both summary/details TSVs
were generated
(`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md:10`,
`restart/skinny/tranches/sk-v12/research/p1/hardening/V1/FOLD-REVISIONS.md:17`).

The manifest records the replay commands and status: parse exports are written
under `/tmp/skv12-p1/parse-xctrace/exports`, product v2 exports under
`/tmp/skv12-p1/direct-xctrace/exports-v2`, original all-row export status is
82/82 PASS, and product v2 export status is 48/48 PASS
(`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:70`,
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:116`).
Live evidence matches the manifest: `/tmp/skv12-p1/time_profile_export_status.tsv:1`
through `:14` shows parse PASS rows with XML outputs, and
`/tmp/skv12-p1/product_time_profile_v2_status.tsv:1` through `:14` shows product
record/export PASS rows with trace and XML paths. Inventory check at review time:
34 parse XML exports, 48 product-v2 XML exports, 48 product-v2 trace bundles,
82 samply `.json.gz` files, and 82 `.json.syms.json` files.

P1-A, P1-B, and P1-E now bind their row tables to the derived self-time TSVs
instead of pretending that unsymbolicated samply JSON carries the percentages
(`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:97`,
`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:103`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:114`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:128`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:72`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:80`).
The TSVs provide 82 summary rows plus header and 410 detail rows plus header:
five ranked leaves for each parse/direct/typed row. Example exact rows include
`/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:2` and
`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:2`.

Bound carried forward: 13 summary top leaves and 31 detail leaves resolve to a
real source path with line `0` rather than a precise source line, for example
`/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:3`,
`/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv:8`, and
`/tmp/skv12-p1/time_profile_hot_leaf_details.tsv:7`. That does not paper-close
because the packet does not hide it behind samply self-report: the exact
self-time percentage, symbol, artifact path, and primitive family are present,
while P1-E supplies grammar-neutral source loci for the family map
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:198`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:216`).
S-P2/S-P3 may treat `:0` rows as family-bounded evidence, not exact source-line
proof.

### CH6-2 - Mode III absence is explicit and nonproducer-bound

Disposition: ACCEPT.

P1-C does not claim fresh Mode III call stacks. Its frontmatter states 17/17 W0
masking and structural facts, 17/17 fresh parse PMU baseline rows, and 0/17
fresh Mode III samply call-stack probe rows
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:13`,
`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:19`). The
method block states there is no `/tmp/skv12-p1/samply/probes`, no
`json_probes_*` capture under `/tmp/skv12-p1`, and no structural-scan capture
there (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`,
`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:57`). The
absence table then repeats 0/17 for eager decode, alternate scalar,
cold-first-parse, and fresh structural scan
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:145`,
`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:157`).

The manifest makes the boundary enforceable: `/tmp/skv12-p1` contains parse,
direct, and typed lanes, but no fresh Mode III call stacks or structural-scan
xctrace lane; Mode III values are W0 Criterion diagnostic nonproducer evidence
only, and no S-P2/S-P3 wave may use Mode III symbols as fresh SK-V12 hot-leaf
authority without a later explicit capture
(`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:160`,
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:168`).
Live `/tmp` inventory agrees: `/tmp/skv12-p1/samply` has only `parse`, `direct`,
and `typed` subdirectories, while the W0 Criterion root carries 68 probe
`estimates.json` files and 34 structural-scan `estimates.json` files. The packet
therefore exposes the Mode III hole rather than burying it in a profile-complete
claim.

### CH6-3 - Profile-only evidence does not move rows

Disposition: ACCEPT.

The packet consistently preserves `skinny/RESULTS.md` as the row-admission
authority. P1-D says its PMU values do not move `skinny/RESULTS.md`, admit a
direct or typed row, or change the SK-V12 opening `N-direct / NoGo` surface
(`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:81`,
`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:89`). P1-E says
PMU throughput/cycles are diagnostic planning facts, not row-admission facts
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:25`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:30`);
its parse rows are diagnostic only, direct rows retain four guards plus thirteen
pre-blocked residuals, and typed rows remain guards
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:221`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:253`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:281`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:287`).
P1-F records zero delta from SK-V11 close and keeps the overall surface as
`N-direct / NoGo`
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:70`,
`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:83`,
`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:241`,
`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:244`).

The authorities agree. `skinny/RESULTS.md` still records overall
`N-direct / NoGo` and identifies Track 1/Track 2 separation
(`skinny/RESULTS.md:143`, `skinny/RESULTS.md:145`). REDRESS 119 closes W8 as a
measured direct fixpoint with no behavior source intervention, no gate/report
semantic change, and no RESULTS row movement
(`skinny/REDRESS.md:3497`, `skinny/REDRESS.md:3505`). REDRESS 120 closes SK-V11
as a measured fixpoint, not overall direct `GO` and not grammar-generalization
admission, with no behavior source, generated runtime, benchmark body, gate
semantic, or RESULTS change
(`skinny/REDRESS.md:3531`, `skinny/REDRESS.md:3538`). No row admission is being
derived from profile-only data.

### CH6-4 - The packet does not substitute future promises for measured evidence

Disposition: ACCEPT.

The generated non-JSON baseline remains absent and is treated as the first
material blocker, not as something already satisfied by JSON profile data.
P1-B states there is no non-JSON product row in its capture set and that JSON
product profiling does not substitute for the required generated non-JSON
baseline
(`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:252`,
`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:260`). P1-E
names the current codegen/runtime gap and says a JSON-only micro-wave before the
non-JSON priority succeeds or blocks would contradict the opening contract
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:301`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:322`).
P1-F likewise says the REDRESS 111 non-JSON report lane is not a generated
baseline, not an admission row, and not row movement, while the REDRESS 112
blocker remains present
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:183`,
`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:193`).

The opening authorities are concrete enough to prevent a promise-close.
`SYNTHESIS.md` requires exactly one generated non-JSON direct or typed parser
baseline before any JSON-only micro-wave, with generated Track 1, independent
Track 2/oracle, strict equality, finite throughput, provenance, and gate/report
consumption
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:49`). `HANDOFF.md` repeats the
same priority and pre-gate, including generated emission/runtime path, runtime
module build, fixture corpus, same-plane oracle, compile/equality smoke, and
REDRESS 111 consumption
(`restart/skinny/tranches/sk-v12/HANDOFF.md:51`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:68`). The packet is therefore
measured-evidence-only for S-P1 and blocker-aware for S-P2/S-P3.

### CH6-5 - S-P2/S-P3 handoff constraints are enforceable

Disposition: ACCEPT.

The handoff is not just advisory prose. `HANDOFF.md` refuses source work before
S-P3, JSON-only direct work before the generated non-JSON baseline/intervention
priority is satisfied or explicitly blocked, W3/parse-only/JSON residual
reopens without fresh material evidence, producer-only telemetry, guard
weakening without measured disposition, and JSON policy leaks
(`restart/skinny/tranches/sk-v12/HANDOFF.md:108`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:125`). P1-C carries the same order:
generated non-JSON baseline first, same-row grammar-generalized intervention
second, and JSON residuals only after REDRESS 119/120 reopen burden is met
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:224`,
`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:227`). P1-E
maps profile temptations to REDRESS pre-blocks and states the required treatment
for structural, parse-only, string/unicode, numeric, digest, JSON residual, and
generated non-JSON report-lane temptations
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:324`,
`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:337`).

Carry-forward constraint: S-P2/S-P3 may consume the V2 profile as empirical
flooring only under these bounds:

1. Exact percentages from `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` and
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` are valid for parse/direct/
   typed profile rows.
2. Any top leaf whose source is line `0` is bounded to the named primitive family
   and source file, not an exact file:line proof.
3. Mode III values are throughput-only W0 diagnostics unless a later capture
   supplies fresh call stacks.
4. No PMU, self-time, masking, structural-scan, parse-only, JSON guard, JSON
   residual, or REDRESS 111 report-lane fact may satisfy the generated non-JSON
   baseline requirement.

## Verdict

ACCEPT. The V2 packet no longer paper-closes the V1 CH6 gap: exact self-time
percentages are exported and citable for all 82 parse/direct/typed rows, the
remaining line-0 source cases are bounded instead of hidden, Mode III absence is
explicit, profile-only evidence admits no rows, no future non-JSON promise is
substituted for measured evidence, and the S-P2/S-P3 handoff constraints are
enforceable.
