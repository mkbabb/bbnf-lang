# Alpha-E Candidate Shortlist For SK-V10

Date: 2026-05-19.

Role: PASS-ALPHA alpha-E shortlist. This is a contract shortlist only; it does
not dispatch implementation. Downstream S-P3 owns the detailed wave plan.

## Global Gates

Parse-only is retired as a SOTA close target. It remains diagnostic
substrate-guard evidence while all parse rows are `S / NO-GO`.

Micro-prove-first is mandatory for every substrate or kernel candidate. A
profile finding is not enough for S-P3 wave scoping. The candidate must first
show a same-host isolated micro-benchmark proving the primitive or call-site
change on representative slices, with scalar reference, intended consumer, host
flags, feature gates, and rejection threshold named.

Lock 14/generalization remains binding. Generic-crate, codegen, or
runtime-outside-json edits require grammar-neutral design plus named CSS L4 /
Sheets / BBNF-self proof. JSON-only wins do not prove the generator thesis.

## Candidate 1: Direct Output/Control-Path Contract

Goal: make `direct_to_struct` the first SK-V10 JSON frontier. Fourteen of
seventeen direct rows are still `N-direct / NO-GO`, while three digest rows are
already `A / GO`.

Owner paths:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `restart/skinny/tranches/sk-v10/research/p1/direct-profile/`
- `restart/skinny/tranches/sk-v10/research/p2/direct-contract/`
- `restart/skinny/tranches/sk-v10/research/p3/direct-wave-plan/`

Adding source owner paths outside this list requires S-P3 CHALLENGE approval.

Falsifiability gate:
No direct row moves until S-P1 profiles direct rows specifically and S-P2/S-P3
define output-plane equivalence, independent Track 2 status, and comparator
anchor semantics. Digest-only rows remain guard evidence until that contract
exists.

Risk:
HIGH. REDRESS 93 blocks scalar-parent folding; REDRESS 73 blocks helper-shape
transfer across generated/hand boundaries. It is still the largest winnable
pool in JSON.

Wave contract seed:

- LOC budget: 180-320 documentation/gate LOC for the contract-only wave; any
  source optimization wave must be separately scoped by S-P3.
- Hard cap: 90 min research, 90 min plan, 90 min redress.
- Same-wave consumer: `gate-json` and `RESULTS.md` row disposition logic. A
  source behavior follow-on must name a concrete direct parser caller in S-P3.
- Revert/disposition: if output-plane equivalence, Track 2 status, or comparator
  anchor semantics cannot be made measurable, write a REDRESS reject and leave
  all 14 `N-direct / NO-GO` rows unchanged.

Initial direct target matrix:

| Corpus | Current Track 1 | Current Track 2 | sonic direct | Floor `ceil(sonic / 1.10)` | Alpha disposition |
|---|---:|---:|---:|---:|---|
| `twitter` | 11931 | 11064 | 15224 | 13840 | target row |
| `canada` | 10466 | 10326 | 12074 | 10977 | target row |
| `apache_builds` | 11157 | 10145 | 11021 | 10020 | no movement without fresh contract; W0 clamp remains |
| `github_events` | 11983 | 11091 | 15800 | 14364 | target row |
| `update_center` | 8356 | 7561 | 11176 | 10160 | target row |
| `mesh` | 8431 | 8769 | 9807 | 8916 | target row |
| `random` | 7685 | 6927 | 8507 | 7734 | target row |
| `gsoc-2018` | 14676 | 14126 | 23078 | 20980 | target row |
| `instruments` | 11708 | 10803 | 12194 | 11086 | target row |
| `numbers` | 12182 | 11803 | 12966 | 11788 | no movement without fresh contract; W0 clamp remains |
| `unicode_mixed` | 4609 | 4562 | 10245 | 9314 | target row |
| `unicode_escapes` | 5131 | 5025 | 13779 | 12527 | target row |
| `distinct_values` | 6052 | 5241 | 11024 | 10022 | target row |
| `y_string_unicode` | 4887 | 3669 | 8829 | 8027 | target row |

The numeric floor alone is insufficient for admission. A row moves only if the
same wave also supplies the direct output/control-path contract and fresh
Criterion rows proving Track 1 and the independent Track 2/oracle meet the
floor under the same run id.

## Candidate 2: `instruments` Typed Product Admission

Goal: add one measured `real_typed_struct` row for `instruments` first. It is
the most plausible typed generalization because it is a fixed top-level object
that the current typed DirectBuild schema model can express with structs,
arrays, options/nulls, and checksums.

Owner paths:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md` and `skinny/REDRESS.md` only after gate disposition

Scalar reference:
serde_json and sonic-rs typed deserialization plus generated Track 1 typed
output. Track 2/oracle remains structurally independent and checksum-based.

Checkasm:
Not applicable.

Falsifiability gate:

- Generated Track 1, serde_json typed, sonic-rs typed, and Track 2/oracle
  checksums equal over the full fixture.
- Same-run `track1_real_typed_struct`, `track2_real_typed_struct`,
  `sonic_rs_real_typed_struct`, and `serde_json_real_typed_struct` Criterion
  rows exist with coherent run id and sample metadata.
- The row is `A / GO`: Track 1 time no worse than 1.10x sonic-rs typed time.
- Existing six typed GO rows maintain their current `A / GO` disposition.
- Any missing schema, parity, same-run metadata, or comparator evidence rejects
  without editing `RESULTS.md`.

Risk:
MEDIUM. The W1 row-table path is proven; the remaining risk is schema/parity and
whether `instruments` actually clears the typed sonic gate.

Wave contract seed:

- LOC budget: 160-260 source/generated LOC plus 40-80 gate/report LOC, assuming
  the existing named-struct root model fits.
- Hard cap: 90 min research, 90 min plan, 90 min redress.
- Same-wave consumer: `track1_real_typed_struct`, `track2_real_typed_struct`,
  `sonic_rs_real_typed_struct`, `serde_json_real_typed_struct`, `gate-json`, and
  `RESULTS.md`.
- Revert/disposition: if any checksum, comparator row, run-id, or typed floor is
  missing, revert the row/schema patch, save the rejected patch, and record a
  REDRESS reject with `instruments` still absent from `real_typed_struct`.

Typed target seed:

| Corpus | Current typed row | Comparator anchor | Floor |
|---|---|---|---|
| `instruments` | absent | same-wave sonic-rs typed strict row | `ceil(sonic_typed / 1.10)` for generated Track 1 and the independent Track 2/oracle |

Existing typed maintain rows stay admitted only if they remain `A / GO` under
their same-run typed comparator gate: `twitter`, `citm_catalog`,
`apache_builds`, `update_center`, `mesh`, and `marine_ik`.

## Candidate 3: Root-Type Typed Generalization

Goal: unblock `github_events` and `gsoc-2018` typed rows by extending the typed
schema root model before attempting product admission.

Current blocker:
`github_events` is a top-level array and `gsoc-2018` is a top-level object map
keyed by numeric strings. The current `DirectRootSchema` points to named struct
types and the typed renderer is structured around `DirectTypeKind::Struct`.

Owner paths:

- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`

Falsifiability gate:

- `DirectRootSchema` or successor root model can represent `Vec<T>` and
  map-entry roots without JSON-specific policy in generic code.
- Generated roots preserve full-fixture generated/serde/sonic checksum parity.
- No `RESULTS.md` row moves in the root-model wave unless the same wave also
  supplies measured typed comparator rows.

Risk:
MEDIUM-HIGH. This is a schema/codegen generalization, not a mechanical row-table
addition.

Wave contract seed:

- LOC budget: 220-420 source/generated LOC plus 60-120 tests/gate LOC.
- Hard cap: 90 min research, 90 min plan, 90 min redress.
- Same-wave consumer: generated typed code for fixture roots plus
  `json_parity` checksum tests. A root-model-only wave has no `RESULTS.md`
  movement.
- Revert/disposition: if root arrays or map-entry roots require JSON policy in
  generic code, revert and REDRESS-reject the root model. If throughput rows are
  attempted in the same wave but comparator evidence is missing, keep the root
  proof only if S-P3 pre-authorized a proof-only close; otherwise reject the
  whole row-moving slice.

Root target matrix:

| Corpus | Current typed row | Root blocker | Row movement rule |
|---|---|---|---|
| `github_events` | absent | top-level array | no row movement in a root-only wave; a paired typed wave must use `ceil(sonic_typed / 1.10)` after same-wave comparator generation |
| `gsoc-2018` | absent | top-level object map keyed by numeric strings | no row movement in a root-only wave; a paired typed wave must use `ceil(sonic_typed / 1.10)` after same-wave comparator generation |

## Candidate 4: Existing-Substrate Unicode/String Kernel Pair

Goal: salvage only the W4 work that can be wired to existing hot paths without
W3: string block widening at `match_string_at_quote_trusted_utf8` and unicode
escape codec work at current unescape/string consumers.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`

Scalar reference:
Existing scalar string scanner, `read_hex_unit_scalar`,
`unescape_uxxxx_scalar`, and current string/unescape sink behavior.

Checkasm:
Required before production wiring. Missing standalone harnesses must be added
for the retained primitive surface; no orphan kernel ships.

Falsifiability gate:

- Micro-prove-first passes before S-P3 wave scoping.
- Same-wave production caller is a current caller such as
  `match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`,
  `decode_unicode_escape`, or `unescape_string`; W3 is not a caller.
- W10b maintain block holds on `canada`, `citm_catalog`, `instruments`,
  `marine_ik`, `mesh`, and `numbers`.
- Unicode rows may record NEAR-FAIL/FAIL honestly; no parse-only row becomes a
  SOTA admission while it remains `S / NO-GO`.

Risk:
HIGH until micro-proof and checkasm gaps close. Correctness/checkasm can be
bounded; row admission remains uncertain.

Wave contract seed:

- LOC budget: 350-650 LOC split by S-P3 if the primitive/checkasm and production
  caller cannot fit one redress.
- Hard cap: 90 min research, 90 min plan, 90 min redress per sub-wave. No
  kernel sub-wave dispatches before micro-prove-first passes.
- Same-wave consumer: current production callers only:
  `match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`,
  `decode_unicode_escape`, or `unescape_string`.
- Revert/disposition: any scalar-reference mismatch, checkasm parity failure,
  missing production caller, or W10b maintain miss reverts the production patch
  and records REDRESS. A unicode target row that remains below its floor is
  recorded honestly as `NO-GO`; that alone is not a checkasm rejection.

Kernel target matrix:

| Corpus | Current direct Track 1 | sonic direct | Floor `ceil(sonic / 1.10)` | Candidate effect |
|---|---:|---:|---:|---|
| `unicode_escapes` | 5131 | 13779 | 12527 | unicode escape codec |
| `unicode_mixed` | 4609 | 10245 | 9314 | unicode escape codec + string path |
| `y_string_unicode` | 4887 | 8829 | 8027 | unicode escape codec + string path |
| `gsoc-2018` | 14676 | 23078 | 20980 | unicode/string-heavy direct row |
| `instruments` | 11708 | 12194 | 11086 | string path guard |

Inherited W10b maintain floors for any parse-loop or aarch64 SIMD kernel:

| Corpus | Floor Mbps |
|---|---:|
| `canada` | 15866 |
| `citm_catalog` | 28630 |
| `instruments` | 15865 |
| `marine_ik` | 11831 |
| `mesh` | 12186 |
| `numbers` | 17596 |

## Candidate 5: Comparator And Telemetry Refresh

Goal: create a clean SK-V10-open report identity and optional same-run sidecar
freshness manifest without allowing evidence ingestion to move behavior rows.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`

Falsifiability gate:
The refresh preserves current row dispositions unless a same-wave behavior gate
admits or rejects a row. Any new telemetry field must be consumed by
`gate-json` in the same wave. Sidecar freshness cannot produce parser data,
row output, substrate, or strict admission by itself.

Risk:
LOW-MEDIUM. Gate-only, but schema drift can corrupt later wave evidence.

Wave contract seed:

- LOC budget: 120-240 gate/report LOC.
- Hard cap: 90 min research, 90 min plan, 90 min redress.
- Same-wave consumer: `gate-json` must reject missing fields in the same commit
  that emits them.
- Revert/disposition: if any new telemetry field is emitted without same-wave
  consumption, revert the field and record REDRESS; current row dispositions
  stay unchanged.

Target matrix:

| Scope | Required outcome |
|---|---|
| SK-V10-open report identity | all 40 existing rows preserve current dispositions until a behavior wave admits/rejects them |
| Sidecar freshness manifest | consumed as comparator evidence only; cannot move behavior rows |
| Telemetry schema field | emitted and consumed by `gate-json` in the same wave |

## Rejected As SK-V10 Defaults

- W3 union/event substrate.
- W4 cascade-lock through W3.
- Canada typed shortcut without full-fixture proof.
- Parse-only SOTA close condition while parse rows remain `S / NO-GO`.
- Substrate/kernel intervention without micro-prove-first evidence.
- JSON-policy leakage in generic code or runtime outside JSON without a
  non-JSON proof.
- PMULL/CTZ production rewires as default hot paths.
