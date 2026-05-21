# SK-V13 S-P1 V1 CH2: Generality

Disposition: REVISE.

CH2 question: do the S-P1 V1 artifacts attribute hot leaves to grammar-neutral
primitives (scanner, classifier, tape, dispatch, string, number, unicode)
rather than JSON-named paths that cannot safely generalize to CSS L4, Sheets,
or BBNF-self?

Answer: not yet. The V1 profile is useful measurement, and several artifacts
correctly warn about their own limits, but the fold cannot accept it as a
generality substrate because the dominant leaves remain JSON generated function
envelopes, generated typed-schema paths, or unprofiled CSS/direct cells.

## Findings

### CH2-V1-001 - `dispatch_value` is an envelope, not primitive attribution

Severity: REVISE.

Evidence:

- `p1a-samply-mode-1.md:75`-`91` reports most Track 1 parse rows as
  `runtime::generated_json::generated::dispatch_value` or the shortened
  `dispatch_value`, with self-time up to 100%.
- `p1a-samply-mode-1.md:93` explicitly states that the generated Track 1
  profile usually collapses into `dispatch_value` and identifies only the hot
  generated function envelope, not always the inner primitive.
- `p1e-hot-leaf-attribution.md:61`-`79` repeats the same attribution as the
  per-corpus hot-leaf synthesis and classifies every parse-only Track 1 row as
  `dispatch`.
- `p1e-hot-leaf-attribution.md:136`-`139` correctly warns that parse-only is
  not scan/number/string attribution at this granularity and that S-P2 must not
  infer unicode/number/string primitives from parse-only PMU.

CH2 assessment: this is a Lock 14 mis-attribution risk if S-P2 consumes the row
as the hot primitive. `dispatch_value` is acceptable as an unresolved envelope,
but not as the primitive hot leaf. V1 must revise the attribution vocabulary so
each row distinguishes `envelope=generated_json::dispatch_value` from the
underlying grammar-neutral candidate (`classifier`, `tape dispatch`,
`string/escape`, `number`, `structural scan`) or marks that primitive as
unresolved.

Required fold action:

- Add a primitive-attribution column or ledger keyed by corpus and workload:
  `envelope_symbol`, `primitive_symbol`, `primitive_class`, and
  `generality_status`.
- For every `dispatch_value` row, set `generality_status=unresolved-envelope`
  unless a deeper sample, inlining-aware symbolization, or source-level counter
  names a grammar-neutral primitive.
- Block S-P2 from treating `dispatch_value` self-time as evidence for a
  unicode, number, structural, or string primitive.

### CH2-V1-002 - Typed hot leaves are JSON/generated-schema paths

Severity: REVISE.

Evidence:

- `p1b-samply-mode-2.md:129`-`132` flags the CH2/Lock 14 gap: typed hot leaves
  live in `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, not a
  grammar-neutral runtime surface, and must not be generalized to CSS without a
  non-JSON consumer.
- `p1e-hot-leaf-attribution.md:63`-`72` attributes typed rows to
  `<bbnf_bench::generated_real_typed::DirectParser>::skip_value`,
  `parse_option_scalar_string`, `parse_type_plugin`, `parse_type_mesh`, and
  `parse_type_marine_geometry_data` in `generated_real_typed.rs`.
- `p1b-samply-mode-2.md:77`-`86` lists the same typed hot leaf statuses, but
  only as generated typed rows.

CH2 assessment: the typed results may be real product-plane wins, but their
symbol paths are schema/product generated code. They do not establish that the
same primitive exists for CSS L4, Sheets, or BBNF-self. The current wording is
careful in places, but the fold needs a stronger "JSON typed only" quarantine.

Required fold action:

- Mark all `generated_real_typed.rs` leaves with
  `generality_status=json-typed-only`.
- Split schema-specific parse functions (`parse_type_plugin`,
  `parse_type_mesh`, `parse_type_marine_geometry_data`) from grammar-neutral
  leaf classes. Their primitive class may be `structural`, but their
  generality status remains unresolved until a non-JSON generated consumer
  produces the same primitive leaf.
- Require a CSS/Sheets/BBNF-self typed or direct consumer before S-P2 cites
  these leaves as cross-grammar design evidence.

### CH2-V1-003 - CSS has throughput evidence but no profile leaf

Severity: REVISE.

Evidence:

- `p1e-hot-leaf-attribution.md:81`-`85` reports the CSS L4 declaration-values
  row as `unprofiled`, with no samply/xctrace hot-leaf artifact under
  `/tmp/skv13-p1/samply/profiles`.
- `p1e-hot-leaf-attribution.md:128`-`130` flags the CSS profile risk: CSS has
  throughput/equality measurement but no hot-leaf profile artifact.
- `p1f-results-delta.md:110`-`114` records a fresh CSS measurement and a large
  method-mismatched delta, but no CSS hot-leaf attribution.
- `p1f-results-delta.md:154`-`158` warns that the CSS measurement is
  stale/heterogeneous telemetry against the SK-V12 W1b Criterion close.

CH2 assessment: CSS is the immediate generality check, and V1 has no CSS
profile leaf. A JSON primitive cannot be claimed grammar-neutral while the CSS
row is unprofiled.

Required fold action:

- Capture CSS L4 declaration-values hot-leaf profiles with the same symbol and
  file:line discipline as JSON.
- Add CSS rows to the primitive-attribution ledger. If CSS remains unprofiled,
  mark JSON-derived primitives as `json-only evidence`, not grammar-neutral.
- Reconcile the CSS `/tmp` measurement method with the W1b Criterion close
  before using CSS throughput as a generality signal.

### CH2-V1-004 - Direct product-plane hot leaves remain unprofiled

Severity: REVISE.

Evidence:

- `p1b-samply-mode-2.md:11` states direct PMU is 17/17 but direct samply has
  0/17 valid hot-leaf profiles because the workload panicked before timed
  parsing.
- `p1b-samply-mode-2.md:62`-`72` rejects direct-to-struct self-time symbols
  from the V1 samply profiles and identifies the fixture/path panic.
- `p1e-hot-leaf-attribution.md:61`-`79` marks every direct_to_struct Track 1
  hot leaf as `unprofiled: direct samply panic`, even where PMU c/B is present.
- `p1e-hot-leaf-attribution.md:122`-`127` repeats that all JSON direct samply
  logs contain a panic and direct rows remain unprofiled at symbol level.

CH2 assessment: direct PMU can rank rows, but it cannot support
grammar-neutral primitive attribution without a leaf. The worst unicode/string
direct rows are especially important for generality, and V1 cannot say whether
their cost is scanner, escape decode, tape, dispatch, or generated schema code.

Required fold action:

- Fix the direct profiling harness fixture/path issue and rerun direct samply
  17/17.
- Attribute direct rows by primitive before S-P2 uses direct c/B anomalies as
  primitive design input.
- Preserve PMU-only direct rows as `ranked-unattributed`, not accepted hot
  leaves.

### CH2-V1-005 - Mode III and masking probes are not generality evidence yet

Severity: REVISE.

Evidence:

- `p1c-samply-mode-3.md:10`-`11` records no dedicated mode III samply capture
  and 0/17 coverage for P1-C mode III.
- `p1c-samply-mode-3.md:53` states no P1-C hot-leaf claim is made because mode
  III profiles are absent.
- `p1c-samply-mode-3.md:93`-`103` lists required fold-cycle captures for
  `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, and
  structural-scan-only paths.
- `p1d-pmu-cycles.md:171` states masking-probe rows named in
  `skinny/RESULTS.md` are not independent PMU rows in `/tmp/skv13-p1`.

CH2 assessment: masking probes are relevant to separating host-call,
structural, and dispatch effects, but V1 has no mode III primitive leaf. They
must remain unresolved and cannot be used to infer cross-grammar primitives.

Required fold action:

- Capture 17/17 mode III profiles and PMU rows or explicitly mark each probe
  unsupported with a reason.
- Keep masking-probe claims outside the primitive ledger until they have
  profile artifacts and file:line-backed symbols.

### CH2-V1-006 - RESULTS extraction uses JSON row paths, not primitive identity

Severity: REVISE.

Evidence:

- `p1f-results-delta.md:54`-`108` keys the row ledger by JSON-named row paths
  such as `json/twitter/parse_only/main`, `json/.../direct_to_struct/main`,
  and `json/.../real_typed_struct/main`.
- `p1f-results-delta.md:159`-`161` records that hot-leaf fields in
  `skinny/RESULTS.md` remain stale placeholders and must be replaced by
  resolved samply/xctrace symbols.

CH2 assessment: row extraction is not wrong, but it is not primitive
attribution. The fold must avoid treating row-path classifications (`A`, `G`,
`N-direct`) as proof that the cost belongs to a grammar-neutral primitive.

Required fold action:

- Add a crosswalk from each JSON row path to its resolved primitive attribution
  state.
- Where the primitive is unresolved, keep row-path classification separate from
  primitive design input.

## Fold Gate

V1 can advance only as REVISE for CH2. The V2 fold must produce a
generality-safe primitive ledger with these minimum states:

| State | Meaning |
|---|---|
| `resolved-neutral` | The hot leaf is a grammar-neutral primitive with symbol, %, and file:line. |
| `unresolved-envelope` | The hot leaf is an envelope such as `dispatch_value`; inner primitive unknown. |
| `json-typed-only` | The leaf is generated JSON typed/schema code, useful only as JSON product-plane evidence. |
| `ranked-unattributed` | PMU ranks the row, but no valid samply/xctrace hot leaf exists. |
| `unprofiled` | No valid profile artifact exists. |

Required V2 fold actions:

1. Rerun or deepen symbolization for parse rows currently collapsed into
   `dispatch_value`.
2. Fix and rerun direct samply 17/17.
3. Capture CSS L4 hot-leaf profiles or quarantine CSS as unprofiled.
4. Capture mode III masking probes 17/17 or mark them unsupported with explicit
   reasons.
5. Quarantine all `generated_real_typed.rs` typed leaves as `json-typed-only`
   until a non-JSON consumer confirms the same primitive.
6. Keep row-path outcomes from P1-F separate from primitive attribution.

No REJECT is issued because the artifacts repeatedly disclose the limits above
instead of falsely claiming full generality. No ACCEPT is possible because the
current hot-leaf surface is still dominated by JSON envelopes and unprofiled
non-JSON/direct lanes.
