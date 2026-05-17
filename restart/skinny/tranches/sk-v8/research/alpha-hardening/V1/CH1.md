# CH1 Correctness Challenge - SK-V8 Alpha V1

Date: 2026-05-17.
Lens: CH1 Correctness.
Scope: alpha A-F under
`restart/skinny/tranches/sk-v8/research/alpha/`, plus
`skinny/RESULTS.md`, `skinny/REDRESS.md`, and
`restart/prompts/pass-contracts/PASS-ALPHA.md`.

Overall disposition: REVISE.

No alpha artifact is rejected wholesale. The current packet is directionally
honest about the SK-V7 close, strict sonic repair, schema-v3 reporting, W10
bitmap rejection, and W10c B6-only admission. It cannot close G-Alpha yet
because several final-contract claims remain uncited, some candidate diagnoses
depend on stale or sidecar profile evidence while current RESULTS hot leaves
are explicitly unprofiled, and the final SPEC shape still leaves exact row
targets and schema semantics open.

## Evidence Baseline

- Current measured authority is `skinny/RESULTS.md`: overall
  `N-direct / NoGo`; 17 `parse_only` rows are `K / NO-GO`; 11
  `direct_to_struct` rows are `N-direct / NO-GO`; 6 `direct_to_struct` rows
  are `A / GO`; 4 `real_typed_struct` rows are `A / GO`.
- Current strictness disclosure in every main row is `Strictness=deferred`,
  `parse_utf8=view-boundary`, `escape_complete=yes`; current hot-leaf cells
  say `unprofiled in W0b; no kernel prescription from this row`.
- Current comparator provenance is the RESULTS note: sonic-rs strict/lossy and
  serde_json are same-run; C++ simdjson, yyjson, RapidJSON, and asmjson values
  are documented sidecar profile values when populated and are not same-run
  strict anchors.
- SK-V7 REDRESS items 77-90 are the binding route ledger for the current
  tranche: 77 strict sonic repair, 78 schema-v3 telemetry, 79 TapeKind rename,
  80 mantissa route rejected, 81 typed Vec expansion admitted, 82 Unicode
  quartet route rejected, 83 StringBlock16 route rejected, 84 object-pair
  value-byte route rejected, 85-86 Lock 14 neutralization admitted, 87
  CostFacts admitted, 88 PMULL default body rejected, 89 CTZ/bulk consumer
  rejected, 90 B6 canary Stage 1 admitted only.

## Artifact Dispositions

| Artifact | Disposition | CH1 finding | Required fix |
|---|---|---|---|
| alpha-A results extraction | ACCEPT | The row counts, blocker rows, GO rows, schema caveats, W0-W10c admit/reject ledger, and missing telemetry caveats agree with RESULTS rows and REDRESS items 77-90. | Keep as evidence source. When folding into final docs, cite exact RESULTS rows for every numeric claim and REDRESS item for every wave claim. |
| alpha-B competitor deltas | REVISE | The delta math matches RESULTS values, but sidecar strictness-plane statements for simdjson/yyjson/RapidJSON/asmjson need explicit source citations, not just registry prose. Direct and typed workload rows must not imply C++ sidecar coverage where RESULTS has `n/a`. | Add measurement-file or sidecar-report citations for each C++ comparator plane. State that workload comparisons are strict only against same-run sonic-rs strict and serde_json unless new same-run telemetry is produced. |
| alpha-C redress digest | REVISE | Items 77-90 and the main pre-block table are well grounded. The "Additional standing blocks" bullets include several uncited historical blocks, so the final packet could inherit policy claims without row, REDRESS, commit, or measurement-file support. | Add REDRESS item numbers or measurement-file citations to every historical block, or delete/move unsupported policy-only bullets out of the correctness-critical pre-block list. |
| alpha-D validated/invalidated ledger | REVISE | The admitted/rejected ledger is mostly correct. One label is ambiguous: I1 says `instruments` and `unicode_basic` stayed `G / NO-GO`, which is W0-era wording from REDRESS 77, while current RESULTS schema-v3 rows are `K / NO-GO`. Forecast-count claims also need commit or measurement citations. | Change I1 to "W0-era G/NO-GO per REDRESS 77; current RESULTS row is K/NO-GO" or drop the outcome letter. Cite the SK-V7 forecast source commit for every "predicted X rows" statement. |
| alpha-E candidate shortlist | REVISE | The candidates have measurable gates, but candidate 1 uses a current-hot-path diagnosis that conflicts with current RESULTS hot leaves being unprofiled. Candidate 2 includes `canada direct_to_struct` in a sidecar-manifest gate even though direct rows have no populated C++ sidecar cells in RESULTS. | Make parse-fusion work conditional on W0 profile evidence, or phrase the current diagnosis as a hypothesis from sidecar/history only. Remove `canada direct_to_struct` from sidecar-manifest requirements or restate it as same-run sonic/serde provenance. |
| alpha-F contract draft | REVISE | The draft correctly blocks dispatch from alpha-F alone and captures W10 honesty. It still contains open items for exact row thresholds, selected W1-W3 rows, exact schema field names, and exact owner paths. PASS-ALPHA requires a precise, measurable goalset before G-Alpha. | Either make only W0 dispatchable and require post-W0 Alpha hardening for W1-W3, or fill the final SPEC with exact row targets, gates, schema fields, owner paths, and citations before G-Alpha. |

## Claim-Level Rejections

These are not whole-artifact rejections. They are claims or gate clauses that
must not be copied into the final SK-V8 contract as written.

| Claim | Disposition | Evidence | Concrete fix |
|---|---|---|---|
| Alpha-E candidate 1: current bbnf twitter parse "remains split across generated value dispatch, scanner, and cursor helpers." | REJECT as a current evidence claim. | Current RESULTS row `twitter/parse_only` has `Hot leaf=unprofiled in W0b; no kernel prescription from this row`; REDRESS items 83 and 84 reject adjacent string/control helper routes. | Rewrite as a hypothesis pending W0 profile evidence, or cite a concrete profile measurement file with symbol shares. |
| Alpha-E candidate 2: sidecar manifest coverage for `canada direct_to_struct` populated sidecar cells. | REJECT as a sidecar gate. | Current RESULTS row `canada/direct_to_struct` has C++ sidecar columns all `n/a`; only sonic-rs strict and serde_json are populated. | Remove the row from sidecar freshness scope, or require new C++ direct-sidecar telemetry in the wave. |
| Any final-contract use of `G / NO-GO` as the current outcome for `instruments/parse_only` or `unicode_basic/parse_only`. | REJECT unless explicitly marked historical W0 wording. | REDRESS item 77 uses W0-era `G / NO-GO`; current RESULTS rows are `K / NO-GO`. | Say "W0-era G/NO-GO per REDRESS 77; current schema-v3 K/NO-GO" or omit the letter. |

## Blocking Corrections

### CH1-R1: Final Contract Needs Citation-Complete Claims

Disposition: REVISE.

Problem: Alpha-F is meant to become the final SK-V8 contract, but many draft
claims are phrased as synthesis facts without local evidence hooks. Examples:
"first repair observability," "prefer product-plane expansion," "digest stressor
is a guard," and "parse remains unknown until profile binding is repaired." The
claims are supportable, but only if they cite current RESULTS rows and REDRESS
items.

Concrete fix:

- For every numeric Mbps or percent claim, cite `skinny/RESULTS.md` by
  `corpus/workload` row.
- For every admitted or rejected route claim, cite the REDRESS item and, when
  used as route authority, the admit/reject commit SHA.
- For every measurement outside current RESULTS, cite the archived measurement
  file path, for example REDRESS item 88's W10 hard-row regressions, item 89's
  W10b RESULTS comparison, or item 90's negative canary logs.

Acceptance test: a grep over the final SYNTHESIS/SPEC/HANDOFF should find no
uncited numeric row target, no uncited route block, and no "profile showed"
claim without either a RESULTS row, REDRESS item, commit SHA, or measurement
file.

### CH1-R2: Outcome Enum Mismatch Must Be Resolved

Disposition: REVISE.

Problem: Alpha-A correctly notes that current RESULTS outcomes include `K` and
`N-direct`, while the PASS-ALPHA template names `A / C / G / L`. Alpha-F says
SK-V8 should preserve schema-v3 but does not decide whether SK-V8 extends the
enum or remaps current rows.

Evidence: current RESULTS rows include `twitter/parse_only` outcome `K` and
`twitter/direct_to_struct` outcome `N-direct`; REDRESS item 78 admitted this
schema-v3 reporting surface.

Concrete fix: final SPEC must choose one:

- Amend the SK-V8 schema to allow `K` and `N-direct`, citing REDRESS item 78
  and current RESULTS rows; or
- Define a lossless mapping from `K` and `N-direct` to PASS-ALPHA's enum before
  gate-json claims schema compatibility.

Acceptance test: `gate-json` schema validation rejects any outcome outside the
declared SK-V8 enum and the final docs use the same enum vocabulary.

### CH1-R3: Competitor Plane Rules Need Hard Wording

Disposition: REVISE.

Problem: Alpha-B computes deltas correctly from RESULTS, but final-contract
language must prevent sidecar or permissive rows from becoming strict
admission evidence.

Evidence: RESULTS note says sonic-rs strict/lossy and serde_json are same-run,
while C++ simdjson, yyjson, RapidJSON, and asmjson columns are sidecar profile
values when populated. RESULTS parse rows disclose lossy/permissive competitors
as flaw probes. REDRESS item 75 rejects lossy sonic rows as strict S anchors;
REDRESS item 77 repairs sonic-rs strict; REDRESS item 78 records same-run
strict/lossy provenance.

Concrete fix:

- Final SPEC must define three comparator classes: same-run strict anchor,
  same-run flaw probe, and sidecar planning signal.
- Direct and real-typed workload claims must compare only to sonic-rs strict
  and serde_json unless new same-run C++ workload telemetry is added.
- yyjson and simdjson parse deltas may guide planning, but final admission
  cannot say "strict beat" or "SOTA beat" from those sidecar cells unless the
  wave refreshes them under the declared same-run/freshness rules.

Acceptance test: every competitor-delta table row names strictness plane,
output plane, freshness status, and admission eligibility.

### CH1-R4: Alpha-D I1 Must Not Mix W0 G Labels With Current K Labels

Disposition: REVISE.

Problem: Alpha-D I1 says `instruments` and `unicode_basic` stayed `G / NO-GO`.
That is supported only as W0-era REDRESS wording. Current RESULTS after W0b
schema-v3 classifies both parse rows as `K / NO-GO`.

Evidence: REDRESS item 77 says the W0 row-flip forecast missed and the rows
stayed `G / NO-GO`; current RESULTS rows `instruments/parse_only` and
`unicode_basic/parse_only` both show outcome `K`, verdict `NO-GO`.

Concrete fix: rewrite I1 as:

`REDRESS 77 records the W0-era rows as still G/NO-GO; in the current
schema-v3 RESULTS table, instruments/parse_only and unicode_basic/parse_only
are K/NO-GO. In both vocabularies, no parse row reclassified to GO.`

Acceptance test: final docs never use `G` as the current parse outcome unless
they explicitly identify it as historical W0 wording.

### CH1-R5: Candidate 1 Must Be Profile-Conditional

Disposition: REVISE.

Problem: Alpha-E candidate 1 says yyjson had one dominant hot symbol and bbnf
"remains split" across generated value dispatch, scanner, and cursor helpers.
The yyjson sidecar gap is measured, but current bbnf hot-leaf attribution is
not. RESULTS hot-leaf cells say unprofiled in W0b, and Alpha-A warns that no
kernel prescription is derivable from current RESULTS hot leaves.

Evidence: RESULTS row `twitter/parse_only` is Track 1 15752 Mbps vs sonic
strict 21020, simdjson DOM 24522, and yyjson 30931, but its `Hot leaf` cell is
`unprofiled in W0b; no kernel prescription from this row`. REDRESS items 83
and 84 reject nearby string/control helper routes despite correctness.

Concrete fix:

- Keep the `twitter/parse_only` yyjson residual as a planning target.
- Change the implementation diagnosis to "hypothesis pending W0 profile" unless
  candidate 1 cites a concrete measurement file for the bbnf split.
- Make the parse-fusion wave entry gate require W0 profile artifact, top symbol,
  sample share, and c/B before any code path is named.

Acceptance test: W2 cannot dispatch from the final SPEC unless W0 produced a
profile artifact path and the CHALLENGE/consolidated gate accepted that the
candidate is not a renamed REDRESS 83 or 84 route.

### CH1-R6: Candidate 1 Typed Guard Wording Is Too Loose

Disposition: REVISE.

Problem: Alpha-E candidate 1 says `twitter real_typed_struct` Track 1 must
remain >= 15486 Mbps, "preserving the current sonic strict GO condition." That
threshold is measurable, but the wording is wrong. Current GO is based on the
typed row's gate/slack signal, not necessarily an at-or-above-sonic threshold.

Evidence: RESULTS row `twitter/real_typed_struct` is Track 1 18513 Mbps, Track
2 16193 Mbps, sonic-rs strict 15486 Mbps, verdict `GO`; RESULTS signal says
the generated typed output is within sonic-rs `1.10 ns slack`.

Concrete fix: choose one wording:

- If the threshold stays `>= 15486`, say it is a stricter guard preserving
  at-or-above current sonic strict, not merely current GO; or
- If the intent is only to preserve current GO slack, set the threshold to
  `>= 14078 Mbps` because 15486 / 1.10 = 14078.18.

Acceptance test: every Mbps guard states whether it preserves current GO slack,
current absolute row value, or at-or-above comparator speed.

### CH1-R7: Candidate 2 Sidecar Manifest Gate Includes A Non-Sidecar Row

Disposition: REVISE.

Problem: Alpha-E candidate 2 requires manifest coverage for populated sidecar
cells on `canada direct_to_struct`. Current RESULTS has no simdjson, yyjson,
RapidJSON, or asmjson cells populated for direct rows.

Evidence: RESULTS row `canada/direct_to_struct` has sonic-rs strict 12421 Mbps
and serde_json 7469 Mbps, but simdjson DOM, simdjson On Demand, yyjson,
asmjson SWAR, asmjson AVX-512, and RapidJSON are all `n/a`.

Concrete fix: remove `canada direct_to_struct` from the sidecar-manifest row
list, or restate it as a same-run sonic/serde provenance requirement. If the
goal is to add C++ direct sidecars, say that explicitly and make the gate
require newly populated direct workload sidecar cells.

Acceptance test: sidecar freshness validation only targets rows with populated
sidecar cells, or rows that the wave explicitly populates.

### CH1-R8: Historical Pre-Blocks Need Citations Or Demotion

Disposition: REVISE.

Problem: Alpha-C's "Additional standing blocks" list is useful but not fully
citation-complete. Some bullets have obvious REDRESS backing, while others are
policy shorthand without a cited measurement.

Evidence examples:

- Pair-token fusion is supported by REDRESS item 16.
- Function-pointer dispatch table is supported by REDRESS item 17.
- Token-width churn is supported by REDRESS item 18.
- Raw f64 shortcut is supported by REDRESS item 31 and current W2 rejection in
  REDRESS item 80.
- EventCursor and side-substrate blocks are supported by REDRESS items 50, 51,
  and 53.
- Direct string/materialization blocks are supported by REDRESS items 54, 55,
  and 66-69.

Concrete fix: add the missing item references inline. If a block has no
RESULTS row, REDRESS item, commit SHA, or measurement-file citation, demote it
to an architectural caution rather than a correctness pre-block.

Acceptance test: every pre-blocked route in final HANDOFF and DISPATCH-PROMPT
has an evidence parenthetical.

### CH1-R9: Alpha-F Open Items Block G-Alpha

Disposition: REVISE.

Problem: Alpha-F section 10 leaves exact row thresholds, W1 typed rows, W2 parse
rows, W3 direct rows, schema field names, and owner paths as open items. That
is acceptable for a draft, but not for G-Alpha.

Evidence: PASS-ALPHA requires a detailed goalset with per-row close
conditions, strict comparator gate, telemetry binding, and wave-by-wave
falsifiability gates. Alpha-F itself says it cannot close G-Alpha and lists
the open items.

Concrete fix: final SPEC must choose one of these contract shapes:

- W0-only dispatch contract: G-Alpha permits only W0 Baseline Profile And
  Telemetry Lock. W1-W3 remain explicitly undispatchable until a post-W0
  Alpha-hardening revision fills exact rows and thresholds.
- Full SK-V8 dispatch contract: fill W1-W3 rows, thresholds, owner paths,
  schema field names, and gate commands before G-Alpha.

Acceptance test: DISPATCH-PROMPT cannot contain an executable W1, W2, or W3
instruction that depends on "Alpha E to confirm" or "selected after W0" unless
the contract explicitly requires a new CHALLENGE revision after W0.

### CH1-R10: Observability Gates Need Command And Artifact Binding

Disposition: REVISE.

Problem: Alpha-F correctly requires hot leaf, profile artifact, c/B or
equivalent, run id, and delta versus SK-V7-open, but the draft does not bind
exact commands, artifact path formats, or validation tests.

Evidence: current RESULTS hot-leaf cells are placeholders; REDRESS item 78
admits schema-v3 but explicitly leaves `Delta vs SK-V6` as `n/a`; REDRESS item
87 admits CostFacts substrate with `gate-json --with-cost-facts --advisory`
JSON output, but it did not add performance telemetry.

Concrete fix: final W0 must define:

- The profiler command or accepted profiler family.
- The profile artifact path pattern under `restart/skinny/tranches/sk-v8/` or
  `skinny/profile/`.
- The c/B formula or approved equivalent.
- The SK-V7-open baseline capture command and run id.
- A focused gate test that rejects one missing hot leaf, one missing profile
  artifact, and one stale sidecar manifest.

Acceptance test: W0 can be audited from checked-in artifacts without relying on
untracked local profiler output.

## Non-Blocking Acceptances

- Alpha-A correctly refuses to derive global SK-V7-vs-SK-V6 numeric deltas from
  current artifacts; RESULTS renders all current `Delta vs SK-V6` cells as
  `n/a (no machine-readable SK-V6 baseline in W0b)`.
- Alpha-B's numeric delta formula is correct for populated RESULTS values. For
  example, `twitter/parse_only` vs yyjson is `15752 / 30931 - 1 = -49.1%`,
  and `citm_catalog/parse_only` vs yyjson is `31784 / 20956 - 1 = +51.7%`.
- Alpha-C correctly carries PMULL and CTZ/bulk as rejected production routes:
  REDRESS item 88 rejects PMULL default prefix-XOR after JSON parse regressions,
  REDRESS item 89 rejects CTZ/bulk after more than 2% maintained-row drops, and
  REDRESS item 90 admits only B6 canary Stage 1 with zero production or RESULTS
  diff.
- Alpha-D correctly demotes real typed wins to typed-output/product-plane
  evidence. Current RESULTS rows for `twitter`, `update_center`, `mesh`, and
  `marine_ik` real typed workloads are `A / GO`, while every row still
  discloses `deferred / view-boundary`.
- Alpha-F correctly states that no SK-V8 implementation wave should dispatch
  from alpha-F alone.

## Final CH1 Gate

CH1 result: REVISE.

Required before G-Alpha:

1. Resolve CH1-R1 through CH1-R10 or explicitly route each unresolved item to
   a new alpha-hardening revision.
2. Ensure every final-contract claim has a RESULTS row, REDRESS item, commit
   SHA, or measurement-file citation.
3. Ensure every wave gate names measurable rows, thresholds, guard rows,
   commands, owner paths, and revert protocol.
4. Ensure competitor deltas are labeled as same-run strict anchor, same-run
   flaw probe, or sidecar planning signal.
5. Ensure no parse or direct implementation route dispatches from unprofiled
   current hot-leaf placeholders.
