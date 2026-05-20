# SK-V12 S-P3 PIN-V1 CH5 Hidden Coupling

Lens: CH5 hidden coupling, shared-file races, substrate/API coupling,
comparator/oracle coupling, generated-vs-hand boundary, and dispatch ownership.
Reviewed packet: committed S-P3 PIN-V1 packet at `fa312b55`.

Verdict: REVISE
Confidence: 94%

## Findings

### CH5-1: W1b can still route a new SIMD helper before W2 fixes `escape_mask_64`

`SPEC.md` blocks SIMD/ASM admission before the `escape_mask_64` correctness gate
passes (`SPEC.md:227`) and W2 is the gate that fixes/verifies that surface
(`SPEC.md:393-424`). But the sequencing puts W1b before W2
(`p3b-wave-sequencing.md:84-89`), while C1 says CSS checkasm/parity is "N/A
unless C1 calls a SIMD helper" without making W2 a prerequisite for that branch
(`p3a-candidate-shortlist.md:79-85`). W1b's tasks then broadly allow generating
and benchmarking the CSS row (`SPEC.md:372-379`) without an explicit
"scalar-only until W2 PASS" owner constraint.

That is a hidden coupling between the CSS baseline wave and the SIMD correctness
blocker: a W1b implementer could legally read the current packet as permitting a
new generated CSS SIMD path before the known falsifier is resolved, as long as
they call it baseline infrastructure rather than SIMD admission.

Required fold: make W1b explicitly scalar-only/no new `bbnf-simd` or ASM-backed
helper unless W2 has already passed. If W1b needs SIMD to create the CSS row,
resequence W2 before W1b or split W1b into scalar baseline and post-W2 SIMD
variant.

### CH5-2: W3 sidecar/substrate rejection is miswired to W4 in P3-B

The W3 gate text correctly describes the union/substrate surface
(`p3b-wave-sequencing.md:157-164`), but the sidecar rejection sentence says any
retained vector, cursor list, sidecar, public substrate API, or `UnionTape`
"falsifies W4" (`p3b-wave-sequencing.md:166-168`). This should falsify W3 before
W3 redress. `SPEC.md:439-446` and `p3c-falsifiability-gates.md:290-300` carry
the stricter W3-owned rule, so this is a packet inconsistency rather than an
architectural permission.

Required fold: change the P3-B reference from W4 to W3 and preserve the same
no-sidecar/no-public-substrate/no-`UnionTape` wording in the W3 row.

### CH5-3: CSS generated-runtime and bench owner paths are not canonical across the packet

The CSS owner path is named three ways: P3-A uses
`skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
(`p3a-candidate-shortlist.md:68-78`), P3-B uses
`skinny/crates/runtime/src/grammars/css_l4/*` (`p3b-wave-sequencing.md:76`), and
the SPEC uses `skinny/crates/runtime/src/grammars/css/` plus broad
`skinny/benches/` ownership (`SPEC.md:354-360`). The repo currently has
`skinny/crates/bbnf-bench`, not `skinny/benches`, so the SPEC path also points
redress at a non-canonical bench surface.

This creates a shared-file/dispatch ownership risk: W1b, W3, and W4 all touch
generated CSS runtime plus report/gate paths, and inconsistent path names make it
too easy for a redress agent to create a sibling generated module or edit a broad
bench surface outside the intended slice.

Required fold: choose one generated CSS runtime path in SPEC/P3-A/P3-B/P3-F and
use it everywhere. Replace `skinny/benches/` with the exact
`skinny/crates/bbnf-bench/...` bench, report, gate, and fixture paths named by
the wave.

### CH5-4: P3-D leaves a public substrate API exception that the pin does not grant

P3-D's union/ASM attempt telemetry table says `public_api_delta` is "No new
public substrate API unless SPEC explicitly authorizes under pin"
(`p3d-telemetry-schema.md:226-237`). The rest of the packet is stricter:
`SPEC.md:219-220` and `p3e-preblocked-ledger.md:60` block new public substrate
APIs, and P3-E states D3/D4 do not permit new public substrate API or enum
surface.

Required fold: remove the "unless SPEC explicitly authorizes under pin" escape
hatch from P3-D. D3/D4 reopen the route categories only; they do not reopen the
public substrate/API surface.

## Checks Passed

- No direct permission found for a new BBNF directive, BIR variant, or
  `BackendShape` variant; the packet blocks those routes in SPEC/P3-C/P3-E.
- Comparator/oracle independence is mostly well bound: same corpus/output plane,
  strict equality, lightningcss Mbps/artifact, and Track 2 independence are
  required and gate-consumed.
- Producer-only telemetry is broadly fail-closed in SPEC/P3-C/P3-D/P3-E.
- Orphan SIMD close is broadly fail-closed; W4 must dispose the five carried
  aarch64 orphans before ADMIT/FIXPOINT close.

## Required Folds

1. Add an explicit W1b scalar-only / no-new-SIMD guard unless W2 PASS is already
   an entry condition.
2. Fix P3-B's W3 sidecar rejection typo from W4 to W3.
3. Canonicalize CSS generated runtime and bench/report/gate owner paths across
   SPEC, DISPATCH, P3-A, P3-B, and P3-F.
4. Remove P3-D's public-substrate-API exception language.
