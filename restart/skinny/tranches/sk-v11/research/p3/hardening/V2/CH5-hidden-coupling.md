# SK-V11 S-P3 V2 CH5 - Hidden Coupling / Lock 1

Verdict: ACCEPT

Acceptance percentage: 95%

Scope audited: committed S-P3 V2 packet at `5c84bb47
docs(sk-v11-p3): fold V1 challenge into V2 synthesis packet`, S-P2
convergence, S-P3 V1 hardening, and REDRESS-backed hidden-coupling surfaces
after the W1a/W1b split.

## Coupling Audit

### 1. W1a/W1b split does not create a new sidecar lane

V2 gives W1a only non-JSON gate/report schema authority. It may teach the gate
to consume grammar id, output plane, comparator/oracle, Track 2/oracle
independence, run id, host, feature mask, same-wave consumer class, and
producer-only telemetry rejection, but it cannot move parser rows or claim a
generated non-JSON baseline. W1b may create exactly one generated non-JSON
baseline plus independent oracle, but it cannot admit an intervention or move
JSON rows. That split is a CH5 improvement over V1 because the baseline cannot
be smuggled in as a schema artifact and the intervention cannot mint its own
baseline in W2.

Accepted control: the SPEC and P3-B/P3-E all state the same boundary: W1a is
harness-only, W1b is baseline-only, and W2 must consume the W1b baseline. The
pre-blocked routes explicitly reject generated baseline authority in W1a,
behavior admission in W1b, and first-baseline creation in W2.

### 2. Sidecars and alternate substrates remain closed

The V2 close condition and non-negotiables still bind REDRESS 96/97/98 and
REDRESS 102: no union/event substrate, class column, structural-position
vector, streaming cursor, class lane, sidecar producer, parse-plane substrate
repair, or W4 cascade-lock through W3 may dispatch. SPEC §13 also blocks aux
columns, whitespace cursors, structural cursors, event cursors, retained
position vectors, hidden bitmap tables, parser-owned sidecars, public substrate
APIs, and second retained substrates.

Accepted control: P3-E maps C1/C5/C7/C9 and D1 back to REDRESS 50/51/53,
92, 96/97/98, and 102. The permitted shapes are transient masks, first
offsets, generated same-loop consumers, or accounting fields consumed by the
gate. No V2 wave grants authority to write a retained sidecar, projection, or
parallel substrate.

### 3. JSON-provider dependency is a named gate, not a hidden proof

The live `json_provider` path remains a real Lock 14 risk, but V2 exposes it
instead of relying on it. P3A-C6 says current codegen still routes through
`json_provider` and therefore cannot admit CSS/Sheets/BBNF-self generality
until the W1a/W1b/W2 lane proves a generated non-JSON parser. SPEC §2.2
requires generic/codegen/runtime-outside-JSON edits to prove that JSON policy
is either replaced, bypassed with a grammar-neutral template proof, or left
untouched before a generality claim passes.

Accepted control: W1b must prove the live `json_provider` path does not leak
JSON policy into the selected generated parser, and W2 repeats that no JSON
policy may appear in generic crates or runtime outside generated per-grammar
code. This is sufficient for CH5 because JSON-provider dependence is no longer
an implicit template for non-JSON admission.

### 4. Track 1 / Track 2 coupling is fail-closed

V2 preserves the S-P2 CH5 requirement that every row-moving packet name
generated Track 1, independent Track 2 or oracle, same-output proof, and a
no-shared-parser assertion. SPEC §2.3 forbids Track 2/oracle calls into
generated Track 1, generated SinkOnly helpers, generated typed helpers, or
hidden shared parser code. P3-D makes coupled Track 2 evidence a gate rejection
and requires non-JSON comparator/oracle identity, source artifact, output
plane, and independence proof to be consumed in the same wave.

Accepted control: W1b cannot create a baseline unless its oracle or Track 2
does not call generated Track 1; W2 cannot admit unless generated Track 1 and
independent Track 2/oracle both exist for the selected workload. The split
therefore reduces, rather than increases, the old Track 1 == Track 2 dishonesty
risk.

### 5. Output sink masking cannot close parser work

C8 remains output oracle or per-product host sink only, and C9 remains
Lock-1/output-plane accounting only. W7 is sequenced after W3-W6 and requires
fresh post-parser profile evidence that `output_digest_hash` is still limiting.
SPEC §13 blocks output digest as parser primitive, hidden semantic string
facts, hash side tables, and direct digest evidence as typed proof.

Accepted control: V2 does not let W1b or W2 use digest/hash as a parser
substrate. If W1b uses a digest-like non-JSON output plane, the oracle still
has to be independent, same-plane, strict, and gate-consumed; it cannot become
a semantic sidecar or shared parser shortcut.

### 6. Already-wired SIMD proof rebranding is still blocked

V2 keeps `HEX_QUARTET_X4_PROOF` as proof/support unless a later wave supplies a
new source delta, scalar oracle, strict checkasm/parity, caller microbench,
same-wave consumer, and row gate. P3-E maps this to REDRESS 107 and 108: W8
proved the existing `unescape_string`/x4 caller, but W9 rejected production
admission because the exact caller already consumed the primitive before the
wave. SPEC §13 names already-wired `unescape_string` reuse as a hard pre-block.

Accepted control: a narrower name, wrapper, feature re-gate, or remeasurement
of the current caller is not a material differential. W6 can use the x4 proof
only if it lands a real source delta and clears the product row gate.

### 7. Generated-code and shared-runtime ownership leaks are contained

The W1b and W2 owner paths are broad enough to be dangerous: codegen, runtime
grammars, bench gates, selected grammars, and regenerated parser output all
sit in the same wave family. V2 adds the needed Lock 14 containment: generated
output may be committed only as regenerated output from named inputs; generic,
codegen, or runtime-outside-JSON edits need same-wave non-JSON proof; proof
failure reverts generic/codegen/runtime edits as one slice.

Accepted control: W1b selects exactly one non-JSON target and names the
independent oracle path before baseline work. W2 selects exactly one generated
non-JSON intervention and names the scalar oracle, baseline Mbps, threshold,
and Lock 14 proof before redress. This is enough to prevent hand-patched
generated output or shared-runtime JSON policy from becoming hidden dispatch
authority.

## Residual Watch Items

- W1b's owner surface includes `skinny/crates/runtime/src/grammars/`, so the
  wave plan must name the selected grammar and generated inputs narrowly. V2
  requires this, but CH5 should reject any W1b plan that treats the whole
  grammar runtime as an ownership pool.
- P3-D allows a companion non-JSON report instead of direct `RESULTS.md`
  extension. That is acceptable only if the companion gate is run in the same
  wave and reconciled into SPEC, HANDOFF, and REDRESS before close.
- W7 remains output-sink-sensitive. Any attempt to use digest/hash state as
  parser vocabulary, semantic string facts, or hidden Track 2 evidence reopens
  CH5 immediately.

## Verdict

ACCEPT. The S-P3 V2 packet resolves the V1 W1 baseline/intervention coupling
without introducing a new hidden substrate. Sidecars, alternate substrates,
JSON-provider generality, Track 1/Track 2 independence, output-sink masking,
already-wired SIMD proof reuse, and generated/shared-runtime ownership are all
represented as explicit gates with revert or REDRESS outcomes.

## Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/p3/hardening/V1/CH5-hidden-coupling.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p2/hardening/V2/CH5.md`
- `skinny/REDRESS.md`
