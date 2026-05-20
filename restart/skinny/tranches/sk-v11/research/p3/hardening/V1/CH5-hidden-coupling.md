# SK-V11 S-P3 V1 CH5 - Hidden Coupling / Lock 1

Verdict: ACCEPT

Acceptance percentage: 94%

Scope audited: committed S-P3 V1 packet
`383e430f docs(sk-v11-p3): archive synthesis-plan V1 packet`, S-P2
convergence, and REDRESS-backed hidden-coupling surfaces across sidecars,
alternate substrates, JSON-provider dependencies, Track 1/Track 2 independence,
output-sink masking, already-wired SIMD proof rebranding, and generated-code /
shared-runtime ownership.

## Coupling Audit

### 1. Sidecars and alternate substrates are hard-blocked

The packet carries the W3/sidecar family as a closed route, not as an
implementation dependency. `SPEC.md` close condition and non-negotiables block
W3 union/event/class-column/streaming-cursor/class-lane/substrate repair,
parser-owned sidecars, second retained substrates, and public substrate APIs.
`P3-E` repeats the concrete REDRESS mapping: REDRESS 50/51/53 block aux
columns, event cursors, whitespace/structural cursors, sidecars, and second
scanners; REDRESS 96/97/98/102 close the W3 union/class-column/streaming-cursor
family and W4-through-W3 cascade lock.

Accepted control: W4 and C1/C5/C6 language consistently requires transient
masks consumed in the same loop, not retained position vectors, class lanes, or
sidecar facts. W8 also blocks paper fixpoint and W0-clamped admission without
behavior provenance. This is enough for Lock 1.

### 2. JSON-provider dependency is exposed as a gate, not hidden as generality

S-P2 convergence states that non-JSON generality must be measured through a
generated direct/typed parser and that JSON-only telemetry is insufficient.
S-P3 makes that executable: W1 exists specifically because the live
`json_provider` path is a Lock 14 blocker, and W2 cannot claim the non-JSON
axis without a generated CSS L4, Sheets, or BBNF-self direct/typed parser row,
independent oracle, strict output equality, before/after Mbps, and same-wave
gate consumption.

Accepted control: `SPEC.md` and `DISPATCH-PROMPT.md` both reject
JSON-provider emission as generic proof, prose-only Lock 14, and JSON policy in
generic crates or runtime outside generated per-grammar modules.

### 3. Track 1 / Track 2 independence is not just a status string

The strongest SK-V10 CH5 concern was a diluted Track 2 dependency proof. SK-V11
fixes that by promoting the forbidden dependency list into the SPEC and dispatch
load-bearing facts. Track 2/oracle may not call generated Track 1, generated
SinkOnly helpers, generated typed helpers, hidden shared parser code, or
benchmark-private parser code. `P3-C` requires the Track 2/oracle path as an
unmeasurable-gate prerequisite, and `P3-D` requires gate rejection for coupled
Track 2 evidence.

Accepted control: row movement requires generated Track 1 and an independent
Track 2/oracle on the same output plane under one same-run strict comparator.
Direct digest evidence cannot admit typed rows, and non-JSON rows need their own
oracle proof.

### 4. Output sink work cannot mask parser losses

C8 is consistently scoped as benchmark oracle or per-product host sink only.
`P3-B` deliberately sequences W7 after parser primitive waves so digest/hash
cannot masquerade as parser semantics. W7 requires fresh post-intervention
profile evidence that `output_digest_hash` remains limiting, strict
Track 1/Track 2-or-oracle parity, selected row floors, and guard preservation.
`P3-D` rejects direct digest as typed product proof and producer-only output
fields.

Accepted control: C8 cannot enter generic parser crates as parser vocabulary,
semantic string facts, hash side tables, or hidden Track 2 evidence. If W7
misses row floors or lacks hot-leaf proof, the route reverts and records
REDRESS instead of closing a parser row by output accounting.

### 5. Already-wired SIMD proof cannot be rebranded as production

The packet carries REDRESS 106-108 precisely. C3/W6 allows
`HEX_QUARTET_X4_PROOF` only as support unless a new source delta, scalar x4
oracle, strict checkasm, caller microbench, same-wave consumer, and row gate all
land together. The already-consuming `unescape_string` path is named as a
pre-blocked production admit in `P3-A`, `P3-B`, `P3-E`, `SPEC.md`, and
`DISPATCH-PROMPT.md`.

Accepted control: reusing the existing `unescape_string` caller, adding a
cosmetic wrapper, or re-gating the current x4 code cannot admit W6. A narrower
name is explicitly not a material differential.

### 6. Generated-code and shared-runtime ownership is controlled

The packet lists generated JSON and typed artifacts in candidate owner paths,
which is inherently a drift risk, but SK-V11 adds the missing lock: generated
output may be committed only as regenerated output from named generator/schema
input. `SPEC.md` also says every generic/codegen/runtime-outside-JSON edit
requires same-wave CSS L4, Sheets, or BBNF-self proof, and the Lock 14 gate
reverts generic/codegen/runtime edits as one slice on proof failure.

Accepted control: this closes the SK-V10 generated-file ownership leak. A wave
may own generated artifacts only through generator/schema ownership and
regeneration evidence; hand-patched generated output is outside the dispatch
contract.

## Residual Watch Items

- W1 must make the non-JSON gate concrete before W2. `P3-A` correctly notes
  that the non-JSON Mbps floor is currently unbound until W1/W2 materialize a
  baseline. This is not a V1 hidden-coupling defect because `SPEC.md` blocks
  behavior row admission until the generated non-JSON Track 1 and independent
  oracle evidence exist.
- W8 allows optional narrow source work only with CHALLENGE acceptance and exact
  remaining owner paths. That remains acceptable because W8 defaults to
  docs/gate fixpoint and cannot introduce a new source route without a measured
  candidate and pre-redress CHALLENGE.

## Verdict

ACCEPT. The S-P3 V1 packet is hard enough for CH5 hidden-coupling / Lock 1:
sidecars and alternate substrates are closed, JSON-provider generality is a
measured gate, Track 1/Track 2 independence is explicit, output sinks cannot
hide parser losses, already-wired SIMD proof reuse is blocked, and generated
artifact ownership is tied to regeneration rather than hand edits.

## Sources

- `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md`
- `skinny/REDRESS.md`
