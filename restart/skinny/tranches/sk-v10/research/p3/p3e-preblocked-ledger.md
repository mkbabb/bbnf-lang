# SK-V10 P3-E: Pre-Blocked Route Ledger

Pass: S-P3 Synthesis-Plan. Cycle: P3-E.
Date: 2026-05-19.
Scope: pre-blocked routes, per-wave refusal conditions, and material
differentials required before any later SK-V10 implementation wave can proceed.
Output: this file.

## Authority

- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `skinny/REDRESS.md`

## Ledger Rule

This ledger is negative authority for S-P3. A later `SPEC.md` or
`DISPATCH-PROMPT.md` may tighten these blocks, but it may not loosen them
without a fresh CHALLENGE disposition.

The central distinction is:

- PASS-3 tape/direct runtime union is the user-surface identity contract:
  generated typed roots, direct structs, `ValueRef`, visitors, path/select, and
  tooling project through one tape/direct document identity.
- SK-V10 W3 union/event substrate is a skinny implementation route retired by
  REDRESS 98 after REDRESS 96 and 97 falsified it with measurement.

Do not collapse those two meanings of "union." PASS-3 keeps tape/direct
identity; SK-V10 refuses W3/event/class-column/structural-index substrate work.

Disposition vocabulary:

- `pre-blocked`: do not dispatch in SK-V10.
- `gate-only`: evidence or reporting can move, behavior rows cannot.
- `proof-only`: correctness or micro-proof can close, `RESULTS.md` cannot move.
- `row-gated`: behavior may move only under the named row gate and same-wave
  consumer.
- `material differential`: the new fact that makes a later route different from
  a REDRESS-rejected route. A rename, narrower prose, or a different helper name
  is not a material differential.

## Global Pre-Blocks

| Route | S-P3 disposition | Material differential required before any future reconsideration |
|---|---|---|
| W3/union/event substrate, retained class column, `UnionTape`, structural index, streaming structural cursor, parser-owned structural projection | `pre-blocked` | None inside SK-V10. REDRESS 98 retires the thesis. A future cycle would need new Alpha/S-P3 authority, same-host micro-proof, live output-plane target, and proof that it is not another REDRESS 96/97 integration shape. |
| W4 cascade-lock through W3 | `pre-blocked` | Re-scope W4 work to existing string/unescape/tape call sites. W3 cannot be the consumer or entry gate. |
| Parse-only SOTA close | `pre-blocked` | Parse-only rows are diagnostic `S / NO-GO` guard evidence only. Row movement must be direct or typed product movement under current comparator/oracle gates. |
| Sidecar or parallel substrate producer | `pre-blocked` | Sidecars may be PASS-3 metadata (`*.path-schema.toml`) or SK-V10 comparator freshness evidence only. They may not produce parser data, retained cursor state, row output, or a second source pass. PASS-3 also keeps columnar SoA and parallel substrates discarded. |
| Generic JSON policy leaks | `pre-blocked` | Generic crates, codegen, or runtime outside JSON require a grammar-neutral design and named CSS L4, Sheets, or BBNF-self evidence. JSON quote, slash, `\u`, surrogate, number, whitespace, output, and row semantics belong in generated per-grammar templates. |
| Direct-vs-typed relabeling | `pre-blocked` | A direct digest improvement moves only `direct_to_struct`. A typed row moves only with generated typed output, serde/sonic typed comparators, full-fixture checksum parity, same-run run id, and its own REDRESS entry. |
| Canada typed shortcut | `pre-blocked` | Full-fixture generated DirectBuild versus serde/sonic parity must prove the decimal-coordinate row. Numeric-only, length-only, digest-only, or analogy from Apache/CITM is insufficient. |
| PMULL/VPCLMUL prefix-XOR as default, CSSC/CTZ bulk emission as default | `pre-blocked` | REDRESS 88 and 89 block default hot-path rewires. A future route must be narrow, caller-proven, non-default, same-host, and tied to a named production consumer plus maintain floors. |
| Eager scratch or decoded direct materialization replay | `pre-blocked` | Allocation reuse, parser-owned scratch, byte-output unescape, and semantic fact hashing were falsified. A future route must change the output representation or prove a standalone grammar-neutral primitive plus same-wave consumer that beats the current baseline before integration. |
| Capacity pre-scan as product evidence | `pre-blocked` | `BBNF_CAPACITY_PLAN=exact|oneshot-simd` and `scan_structurals` capacity pre-scans are diagnostic/env-only. Row movement uses the default one-pass production plan unless a later accepted CH5 change admits a second source pass. |

## Per-Wave Pre-Blocks

V2 CHALLENGE fold: the final dispatch numbering is the top-level SPEC W0-W10
manifest. This ledger's global pre-blocks are binding as written; the scoped
sections below are aligned to the final SPEC wave numbers and candidate
families. Older compressed draft aliases are not dispatch identifiers.

### W0 - Clamps And Dispatch Hygiene

Allowed scope: contract clamps, row-disposition invariants, and gate refusal
language. No behavior row moves.

Pre-blocks:

- W0 clamps preserve current row dispositions until a same-wave behavior gate
  moves them. Apache/numbers direct rows that already clear isolated floors
  still do not move without the direct output/control contract.
- W0/W0b telemetry fixes from REDRESS 77 and 78 are reporting authority, not a
  parser/runtime throughput win. Strict/lossy provenance and schema-v3 fields
  cannot reclassify rows by themselves.
- PASS-3 path/select sidecars remain metadata. A sidecar freshness manifest is
  gate-only and cannot become a parser producer, retained side table, or strict
  admission shortcut.
- Source work remains refused until S-P3 names owner paths, gates, and revert
  protocol.

Material differential:

- A W0 change can close only if every emitted telemetry field is consumed by
  `gate-json` in the same wave and all current row dispositions are preserved.
  Any missing consumer reverts the field and records REDRESS.

### W1 / W2 / W10 - Direct Output/Control-Path Contract And Direct Movement

Allowed scope: `C1-direct-output-contract`. W1 is contract-only, W2 is
zero-behavior direct row-table reclamation, and W10 is the direct residual
behavior tranche after CHALLENGE. All direct movement requires output
equivalence, independent Track 2/oracle status, sonic direct strict comparator,
same-run run id, and `gate-json` consumer.

Pre-blocks:

- Direct digest rows cannot be relabeled as typed product proof.
- The REDRESS 93 scalar-parent fold and REDRESS 73 helper-shape transfer cannot
  return as the direct close without a new direct-contract material
  differential.
- REDRESS 50-55 block parse-time aux side tables, byte-class whitespace
  `EventCursor`, parser-local structural-mask cursors, sink-local exact decoded
  stats, and quote-source streaming hash replays.
- REDRESS 66-69 block direct source-hook receiver folding, parser-owned decoded
  scratch, byte-output `unescape_json_string`, and semantic string fact hashing
  under the current direct digest workload.
- Generic `JsonSink`/direct-event language must name the exact consumer. Direct
  `JsonSink` callbacks, typed `DirectParser` field writers, retained
  `TapeBuilder`, and hand Track 2 are not interchangeable.

Material differential:

- The wave must define the direct output/control contract first. A behavior
  follow-on needs fresh direct profile evidence, a concrete generated direct
  caller, same-run Track 1 and independent Track 2/oracle rows that both meet
  `ceil(sonic_direct / 1.10)`, and a revert path that leaves all direct rows
  unchanged on any missing comparator/run-id/provenance field.

### W3 - W3 And Parse-Only Firewall

Allowed scope: governance and gate refusal only. No behavior row moves.

Pre-blocks:

- W3/union/event substrate, retained class column, `UnionTape`, structural
  cursor, parser-owned structural projection, class-lane-only fallback, and W4
  cascade-lock remain retired by REDRESS 96-98.
- Parse-only rows remain diagnostic `S / NO-GO` and cannot satisfy SK-V10 SOTA.
- PASS-3 tape/direct runtime identity is not a skinny W3 implementation route.

Material differential:

- None inside SK-V10. A future reconsideration requires new Alpha/S-P3
  authority, same-host micro-proof, live output-plane target, and proof that
  the route is not another REDRESS 96/97 shape.

### W4 - `instruments` Typed Product Admission

Allowed scope: `C2-instruments-typed-admission`, one new
`real_typed_struct` row if full typed product proof exists.

Pre-blocks:

- Apache/CITM typed admission cannot be generalized by analogy.
- Canada typed shortcut remains blocked by full-fixture decimal-coordinate
  parity failure history.
- Direct digest parity cannot move typed rows.
- Source-only typed fixture/schema work cannot force a measured row-table
  admission when same-run Criterion metadata is absent or stale.

Material differential:

- `instruments` must supply generated typed output, independent Track 2/oracle,
  serde_json typed, sonic typed, checksum parity over the full fixture, and
  same-run Criterion rows. Existing typed GO rows (`twitter`, `citm_catalog`,
  `apache_builds`, `update_center`, `mesh`, `marine_ik`) must maintain their
  typed gates.

### W5 / W6 - Root-Type Typed Generalization And Row Admission

Allowed scope: `C3-root-typed-generalization`, proof-only root model work for
`github_events` top-level arrays and `gsoc-2018` map roots unless the same wave
also supplies full typed comparator rows.

Pre-blocks:

- Do not reopen W3/union. Root-type work is typed schema/codegen proof and
  optional typed row admission, not the retired W3 union/event substrate.
- JSON-specific root policy may not leak into generic codegen or runtime.
- No `RESULTS.md` row moves in a root-only proof wave.
- Handwritten per-corpus shortcuts are blocked; root arrays and map-entry roots
  must be represented by the typed schema model.

Material differential:

- A valid root proof must show `DirectRootSchema` or its successor can represent
  `Vec<T>` roots and map-entry roots without JSON-only policy in generic code,
  and generated roots must preserve full-fixture generated/serde/sonic checksum
  parity. Row movement needs a paired typed row gate with same-run comparator
  evidence.

### W7 / W8 / W9 - Existing-Substrate Unicode/String Kernel Proof And Production

Allowed scope: `C4`, `C5`, `C6`, and `C7` only as proof-first or row-gated
work against current call sites such as
`match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`,
`decode_unicode_escape`, or `unescape_string`. W3 cannot be a caller.

Pre-blocks:

- REDRESS 28/33 block active 16-byte tiny-string NEON wiring as the retained
  parse fix. A parity-green kernel is not row evidence.
- REDRESS 60, 61, and 62 block deleting the retained tiny-string probe,
  always-wide retained trusted string scan, and delayed-wide retained trusted
  scan on the measured baseline.
- REDRESS 64 blocks retained Unicode-escape run validation as shipped.
- REDRESS 66-69 block the current direct string allocation, scratch, byte-write,
  and semantic-fact family.
- REDRESS 72 admits only generated retained cap 16. It explicitly rejects a
  global cap-16 policy: generated direct remains cap 8, hand Track 2 remains
  cap 8, typed parse is cap 32, and typed skip is cap 96.
- REDRESS 82 blocks per-quartet Unicode escape classifier replay.
- REDRESS 83 blocks the generated-retained StringBlock16 tiny wrapper.
- REDRESS 84 blocks object-pair value-byte control compaction and any attempt
  to compensate by reopening object next-key carry, separator elision,
  function-pointer dispatch, generic SWAR whitespace, EventCursor sidecars, or
  W5 string leaf routes.
- PMULL/CTZ defaults from REDRESS 88/89 remain blocked.
- Parse-only improvements cannot be SOTA admissions.

Material differential:

- S-P3 must split each primitive family. Every kernel wave needs scalar oracle,
  checkasm or parity harness, host feature gate, representative slices,
  per-call-site microbench, failure threshold, and exact same-wave production
  consumer before production wiring.
- A string primitive must name the cap and plane: retained cap 16 excluded
  unless explicitly targeted, generated direct cap 8, typed parse cap 32, typed
  skip cap 96, and hand Track 2 cap 8. Evidence for one cap or plane does not
  authorize another.
- Any aarch64 SIMD/string/unescape production wiring inherits the W10b maintain
  floors for `canada`, `citm_catalog`, `instruments`, `marine_ik`, `mesh`, and
  `numbers`.

### W0 / Close - Comparator, Telemetry, And Close Refresh

Allowed scope: `C12-telemetry-refresh`, SK-V10-open report identity, optional
same-run sidecar freshness manifest, and `gate-json` schema consumption.

Pre-blocks:

- Sidecar freshness cannot move behavior rows.
- A report schema change cannot admit a parser/runtime row without a same-wave
  behavior gate.
- W0b's `Delta vs SK-V6 = n/a` limitation must not be converted into inferred
  performance movement.
- Missing comparator strictness, sidecar freshness, run id, feature mask,
  validation path, or output-plane provenance fails closed.

Material differential:

- A valid telemetry wave must prove every new field is emitted and rejected by
  `gate-json` when missing in the same commit. The wave closes gate-only and
  preserves all current row dispositions unless another same-wave behavior gate
  explicitly changes them.

## REDRESS Family Register

| REDRESS | Binding effect for SK-V10 S-P3 |
|---|---|
| 28/33 | Tiny-string aarch64 primitives may remain inventory or future caller-specific proof work, but active retained 16-byte NEON wiring is not the parse-G fix. |
| 50-55 | Parse-time aux side tables, event cursors, parser-local structural-mask cursors, decoded stats sinks, and quote-source streaming hash replays are blocked. The material differential must be single-substrate or output-contract work, not a sidecar/second scanner. |
| 60-72 | The retained/direct string and materialization family is mostly rejected, with narrow admissions only for `ContainerNext`/next-byte carry (63), generated typed DirectBuild from host/API schema (71), and generated retained cap 16 (72). Do not globalize the admitted shapes. |
| 80 | Mantissa-widen/table-only numeric work is blocked because the measured fallback pool was zero. Numeric work needs a fresh hot leaf and generated direct/typed consumer. |
| 82-84 | Per-quartet Unicode classifier, StringBlock16 tiny wrapper, and object-pair value-byte control compaction are blocked on the current baselines. |
| 88 | PMULL default prefix-XOR body is blocked after parse regressions. |
| 89 | CSSC/CTZ bulk consumer as default is blocked after maintain-row regressions. |
| 96-98 | W3 class-column and streaming-cursor implementations falsified every W3 and W10b row, and REDRESS 98 retires the W3 union-substrate thesis. |

## Material Differential Checklist

Every S-P3 wave that touches a route adjacent to this ledger must write a
material-differential paragraph before implementation. The paragraph must name:

1. the specific REDRESS rows it is adjacent to;
2. the old rejected mechanism in one sentence;
3. the new mechanism in one sentence;
4. the exact consumer plane and call site;
5. the scalar oracle or independent oracle;
6. the same-host benchmark or Criterion rows;
7. the failure threshold and revert protocol;
8. why direct, typed, retained, and Track 2 evidence are not being interchanged;
9. whether any generic crate, codegen, or runtime-outside-JSON edit needs
   CSS L4, Sheets, or BBNF-self proof;
10. the REDRESS entry that will be written if the wave fails.

If any item is missing, the route is not S-P3 eligible.

## Close Posture

P3-E closes as a refusal ledger, not as source authorization. The live SK-V10
frontier remains direct output/control-path contract first, typed product
generalization second, and existing-substrate unicode/string kernels only after
micro-prove-first. W3/union, parse-only SOTA, sidecar/parallel substrate,
PMULL/CTZ defaults, Canada typed shortcut, direct-vs-typed relabeling, generic
JSON policy leaks, and REDRESS replay without material differential remain
pre-blocked.
