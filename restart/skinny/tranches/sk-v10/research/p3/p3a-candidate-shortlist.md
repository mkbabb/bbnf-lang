# SK-V10 P3-A: Candidate Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-19.
Output: this file.
Scope: shortlist the SK-V10 S-P3 candidate pool before `SPEC.md` and
`DISPATCH-PROMPT.md`.

## Authority

`restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md` is the
candidate-pool authority. This shortlist contains only P2G canonical ids with
S-P3 eligibility. Earlier P2-A through P2-F aliases are evidence only.

Read inputs consumed:

- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/prompts/totality/PASS-3-SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Global Gates

No source implementation is authorized by P3-A. S-P3 still must write
`SPEC.md` and `DISPATCH-PROMPT.md` with owner paths, waves, revert protocol,
and row gates before any source wave starts.

Global pre-blocks:

- Parse-only rows remain `S / NO-GO` and cannot close SK-V10.
- W3 union/event/class-column/streaming-cursor routes remain retired by
  REDRESS 96, 97, and 98, including renamed aliases.
- Direct digest rows cannot be relabeled as typed product proof.
- Canada typed remains blocked without full-fixture generated/serde/sonic
  checksum parity.
- Any kernel/SIMD/ASM production wiring needs scalar oracle, checkasm,
  same-host microbench, feature gate, representative corpus slices, failure
  threshold, and exact same-wave consumer before integration.
- Generic crates, codegen, and runtime outside JSON must stay grammar-neutral
  under Lock 14; JSON policy belongs in generated per-grammar templates.
- PMU/cycles/structural-scan probes and sidecar freshness are evidence only,
  never row producers.
- X86-only ISA routes are inventory-only for SK-V10 on the current
  `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` host.

## Shared Floors

Direct row movement uses the P2G direct floor table: both generated Track 1 and
the independent Track 2/oracle must meet `ceil(sonic_direct / 1.10)` under one
run id and strict comparator plane.

| Corpus | Direct floor Mbps |
|---|---:|
| `twitter` | 13840 |
| `canada` | 10977 |
| `apache_builds` | 10020 |
| `github_events` | 14364 |
| `update_center` | 10160 |
| `mesh` | 8916 |
| `random` | 7734 |
| `gsoc-2018` | 20980 |
| `instruments` | 11086 |
| `numbers` | 11788 |
| `unicode_mixed` | 9314 |
| `unicode_escapes` | 12527 |
| `distinct_values` | 10022 |
| `y_string_unicode` | 8027 |

Any aarch64 SIMD/string/unescape production wiring also carries the W10b
maintain block unless S-P3 tightens it:

| Corpus | Maintain floor Mbps |
|---|---:|
| `canada` | 15866 |
| `citm_catalog` | 28630 |
| `instruments` | 15865 |
| `marine_ik` | 11831 |
| `mesh` | 12186 |
| `numbers` | 17596 |

Existing typed rows must maintain their current typed `A / GO` gate. Floors
below are computed from the current sonic typed Mbps under the existing
`ceil(sonic_typed / 1.10)` rule.

| Corpus | Existing typed floor Mbps |
|---|---:|
| `twitter` | 14424 |
| `citm_catalog` | 20053 |
| `apache_builds` | 7373 |
| `update_center` | 11365 |
| `mesh` | 8428 |
| `marine_ik` | 7369 |

## Shortlist

Shortlist count: 8.

### 1. `C1-direct-output-contract`

Status: `row-gated` or contract-only, depending on SPEC wave.

Owner paths:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v10/research/p3/direct-contract/`

Scalar-ref status: present for the current digest plane as independent hand
Track 2 plus serde/sonic shape parity. A promoted direct row must keep that
oracle or name a replacement before row movement.

Checkasm status: not applicable unless the SPEC pairs the contract with a
kernel primitive; then the paired primitive owns checkasm.

Same-wave consumer: `gate-json`, `RESULTS.md` row disposition logic, generated
`JsonSink` callbacks, and independent hand Track 2 digest. Hand Track 2 remains
oracle evidence, not proof that Track 1 took the same path.

Falsifiability gate:

- Contract-only close: output-plane equivalence, comparator strictness,
  validation path, Track 2 independence, same-run run id, and direct row
  disposition rules are executable in `gate-json`.
- Row-moving close: any named direct corpus row must meet its shared direct
  floor for both Track 1 and Track 2/oracle under the same run id. Initial
  target rows are `twitter` 13840, `canada` 10977, `github_events` 14364,
  `update_center` 10160, `mesh` 8916, `random` 7734, `gsoc-2018` 20980,
  `instruments` 11086, `unicode_mixed` 9314, `unicode_escapes` 12527,
  `distinct_values` 10022, and `y_string_unicode` 8027 Mbps.
- `apache_builds` 10020 and `numbers` 11788 Mbps stay W0-clamped unless the
  SPEC explicitly explains why fresh direct guard passes become behavior
  evidence.

Pre-blocks: direct digest cannot admit typed rows; parse-only evidence cannot
admit direct rows; W3 cannot be a consumer; no gate weakening for missing
run-id, comparator, strictness, or provenance evidence.

### 2. `C2-instruments-typed-admission`

Status: `row-gated` JSON product-plane row.

Owner paths:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md` and `skinny/REDRESS.md` only after the row gate closes

Scalar-ref status: present as the typed admission pattern from W1:
generated Track 1 typed output, independent Track 2 checksum oracle,
serde_json typed, and sonic-rs typed. The `instruments` schema/parity instance
does not exist yet and must be created in the same wave.

Checkasm status: not applicable.

Same-wave consumer: `track1_real_typed_struct`,
`track2_real_typed_struct`, `sonic_rs_real_typed_struct`,
`serde_json_real_typed_struct`, `json_parity`, `gate-json`, and
`RESULTS.md`.

Falsifiability gate:

- `instruments` admits only if generated Track 1, independent Track 2/oracle,
  serde_json typed, and sonic-rs typed checksums match over the full fixture.
- The Mbps floor is `ceil(same-wave sonic_typed / 1.10)` for `instruments`;
  the row is blocked until that numeric floor is rendered by the same run.
- Existing typed rows must maintain `twitter` 14424, `citm_catalog` 20053,
  `apache_builds` 7373, `update_center` 11365, `mesh` 8428, and `marine_ik`
  7369 Mbps.

Pre-blocks: no admission by analogy from Apache/CITM; no direct digest evidence
as typed proof; no missing schema, checksum, comparator, run-id, or sample
metadata.

### 3. `C3-root-typed-generalization`

Status: `proof-only` unless paired with a typed row gate.

Owner paths:

- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `restart/skinny/tranches/sk-v10/research/p3/root-typed-proof/`

Scalar-ref status: proof wave needs generated/serde/sonic/checksum parity for
representative root shapes. Mbps scalar reference exists only if the same wave
also creates typed comparator rows.

Checkasm status: not applicable.

Same-wave consumer: generated typed code for array-root and map-entry-root
fixtures plus `json_parity` checksum tests. A root-model-only wave has no
`RESULTS.md` movement.

Falsifiability gate:

- Proof-only close: root model can represent `github_events` top-level array
  and `gsoc-2018` numeric-string map root without JSON policy in generic code.
- Row-moving close: `github_events` and/or `gsoc-2018` need same-wave sonic
  typed comparator rows and floors `ceil(same-wave sonic_typed / 1.10)`, plus
  generated/serde/sonic/checksum parity over the full fixtures.

Pre-blocks: no JSON-only `JsonRootSchema` branch in generic code; no
`RESULTS.md` movement from a root-only proof; no Canada typed shortcut.

### 4. `C4-tiny-string-proof`

Status: `proof-only` first; row-gated only after proof.

Owner paths:

- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`

Scalar-ref status: call-site scalar loops exist, but the generic
caller-owned delimiter/control oracle must be extracted or named before SIMD.
Per-plane caps are distinct: generated direct cap 8, typed parse cap 32,
typed skip cap 96. Retained cap 16 is excluded unless explicitly targeted.

Checkasm status: required before production wiring. The exact standalone
tiny-string first-special surface is missing; existing checkasm skeletons are
process evidence only.

Same-wave consumer: exactly one current generated direct or typed string caller
per proof. Allowed examples are `match_tiny_plain_string_with_cap::<8>`,
`DirectParser::tiny_plain_string_end`, or
`DirectParser::skip_plain_string_end`.

Falsifiability gate:

- Proof close: scalar oracle, checkasm cell, host feature gate, representative
  corpus slices, and caller microbench pass for one cap/plane.
- Row-moving direct close, if separately authorized: target row must meet its
  direct floor, typically `twitter` 13840, `github_events` 14364,
  `update_center` 10160, `instruments` 11086, or `distinct_values` 10022 Mbps.
- Typed production wiring must also maintain typed floors for `twitter` 14424,
  `apache_builds` 7373, and `update_center` 11365 Mbps if those callers are
  touched.

Pre-blocks: REDRESS 28/33 and 83 block old retained/StringBlock16 active
wrappers; no parser sidecar, W3 consumer, or JSON-hardcoded generic primitive.

### 5. `C5-full-string-proof`

Status: `proof-only` first; row-gated only after proof.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs`

Scalar-ref status: current scalar string scanner and sink/view behavior exist.
The SPEC must bind exact quote, escape, control, non-ASCII, UTF-8, and error
offset semantics as caller policy.

Checkasm status: required before production wiring. Exact multi-mask
full-string/escape-control checkasm is not yet admitted for this surface.

Same-wave consumer: one current direct or typed string caller, such as
`match_string_at_quote_trusted_utf8`; no parse-only row consumer.

Falsifiability gate:

- Proof close: scalar/checkasm/caller microbench over representative
  `unicode_mixed`, `unicode_escapes`, and `unicode_basic` slices.
- Row-moving close: direct rows must meet `unicode_mixed` 9314 and/or
  `unicode_escapes` 12527 Mbps. `unicode_basic` is already direct `A / GO`
  and is a maintain guard, not an admission target.
- Any aarch64 production wiring must preserve W10b maintain floors:
  `canada` 15866, `citm_catalog` 28630, `instruments` 15865, `marine_ik`
  11831, `mesh` 12186, and `numbers` 17596 Mbps.

Pre-blocks: no eager decoded scratch, no decoded stats sidecar, no retained
projection route, no asmjson-style no-control-scan strictness shortcut.

### 6. `C6-hex-escape-proof`

Status: `proof-only` first; row-gated only after proof.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`

Scalar-ref status: scalar `read_hex_unit_scalar`, `unescape_uxxxx_scalar`,
surrogate validation, and current unescape behavior exist. A generic kernel is
hex decode/classify only; slash/introducer, `\u`, surrogate, CSS width, and
Sheets quote-doubling policy stay in grammar templates.

Checkasm status: required before production wiring. Historical single-quartet
coverage is not enough for the x4/run production surface; dedicated invalid
hex, BMP, surrogate, and alignment cases are required.

Same-wave consumer: `validate_unicode_escape_run`, `decode_unicode_escape`, or
`unescape_string` in the same proof wave. Standalone parse-only quartet
admission is blocked.

Falsifiability gate:

- Proof close: scalar/checkasm/caller microbench over
  `unicode_escapes`, `unicode_mixed`, and `y_string_unicode` slices.
- Row-moving close: direct rows must meet `unicode_escapes` 12527,
  `unicode_mixed` 9314, and/or `y_string_unicode` 8027 Mbps.
- W10b maintain floors apply for any aarch64 production wiring.

Pre-blocks: REDRESS 82 blocks the old single-quartet production route; REDRESS
66-69 block decoded scratch/source-hook families; no JSON policy in generic
SIMD or parse-that public APIs.

### 7. `C7-string-segment-fold`

Status: `proof-only` and per-plane gated.

Owner paths:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `restart/skinny/tranches/sk-v10/research/p3/string-segment-fold/`

Scalar-ref status: current `unescape_string` output and direct/typed
per-plane consumers are the byte-for-byte oracle. A generic consumer trait is
not accepted unless it is free of JSON key/value, digest, and sink-local
decoded-stats semantics.

Checkasm status: not applicable for a scalar segment fold. If the proof calls
the C5/C6 aarch64 kernels, their checkasm gates are inherited.

Same-wave consumer: exactly one generated output plane: direct digest fold or
typed owned field writer. Direct evidence cannot move typed rows and typed
evidence cannot move direct rows.

Falsifiability gate:

- Proof close: segment fold matches current scalar unescape/materialization
  semantics byte-for-byte for the chosen plane and reduces caller work in a
  same-host microbench.
- Row-moving direct close, if separately authorized: `y_string_unicode` must
  meet 8027 Mbps; unicode sibling rows are `unicode_mixed` 9314 and
  `unicode_escapes` 12527 Mbps.
- Typed row movement is blocked unless a typed product comparator row and
  `ceil(same-wave sonic_typed / 1.10)` floor are present in the same wave.

Pre-blocks: P1-C eager decode is slower on every row; REDRESS 66-69 block
parser-owned decoded scratch, source hooks, semantic facts, and sink-local
decoded stats as proof.

### 8. `C8-digit-number-proof`

Status: `proof-only` first; row-gated only after proof.

Owner paths:

- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/parse-that-regex/src/number/integer.rs`
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs`
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`

Scalar-ref status: current `match_number_span_from_first` and
`scan_digit_run` are the JSON oracle. The proof must split grammar-neutral
digit masks/accumulators from generated grammar-owned number policy.

Checkasm status: required before production wiring for digit-MAC or SIMD
classification. No dedicated product-plane digit-run checkasm is admitted yet.

Same-wave consumer: one generated direct numeric caller or typed numeric
materializer. Numeric parse-only rows are not admission consumers.

Falsifiability gate:

- Proof close: scalar oracle, overflow/error-offset parity, checkasm, host
  feature gate, representative slices, and caller microbench pass for one
  numeric caller.
- Row-moving direct close: target rows must meet `canada` 10977, `mesh` 8916,
  and/or `numbers` 11788 Mbps. `marine_ik` is already direct `A / GO` and is a
  maintain guard.
- Typed production wiring must preserve `mesh` 8428 and `marine_ik` 7369 Mbps
  typed floors, and Canada typed remains blocked without full-fixture parity.
- Any aarch64 production wiring also carries W10b maintain floors.

Pre-blocks: REDRESS 80 blocks mantissa-widen shortcuts; isolated structural
scan speedups on numeric rows are masking signals only; JSON number grammar
must not move into generic APIs.

## W0 Notes

W0 should be gate-only unless S-P3 deliberately chooses a proof-only candidate
with no `RESULTS.md` movement. The safest W0 is P2G `C12-telemetry-refresh`,
handled as close infrastructure rather than a behavior candidate.

W0 owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md` only if `gate-json` regenerates an SK-V10-open report

W0 scalar-ref status: existing `gate-json` report parser and current
`skinny/RESULTS.md` are the oracle.

W0 checkasm status: not applicable.

W0 same-wave consumer: every emitted telemetry or freshness field must be
consumed by `gate-json` in the same wave.

W0 falsifiability gate:

- Preserve all current row dispositions: 17 parse-only `S / NO-GO`, 3 direct
  `A / GO`, 14 direct `N-direct / NO-GO`, and 6 typed `A / GO`.
- Preserve current direct and typed floors listed above unless a same-wave
  behavior gate explicitly admits or rejects a row.
- Sidecar freshness, run-id, comparator strictness, validation path, and
  provenance fields cannot move behavior rows by implication.

W0 pre-blocks: no strict-admission rewrite, no direct-to-typed relabel, no
sidecar producer, no source behavior edit, and no report schema field emitted
without same-wave gate consumption.

## Close Notes

P3-A closes when:

- The shortlist remains at eight or fewer candidates.
- Every shortlisted item maps to a P2G canonical id.
- No `inventory-only` or `rejected` alias is shortlisted.
- Every candidate names owner paths, scalar-reference status, checkasm status,
  same-wave consumer, falsifiable corpus rows/floors, and pre-blocks.
- W0 is explicitly gate-only unless the future SPEC chooses a proof-only
  no-row-movement slice.

Not shortlisted under P2G authority:

| P2G id | Reason |
|---|---|
| `C9-whitespace-class-skip` | maintain-only unless a future SPEC names an exact caller, row gate, and maintain floors |
| `C10-byte-class-movemask` | inventory-only unless paired to a current direct/typed caller; W3/retained structural consumption rejected |
| `C11-tape-economy-contract` | proof/invariant only; capacity pre-scans are diagnostic/env-only for SK-V10 row movement |
| `C13-x86-secondary-isa` | inventory-only on the current Apple aarch64 host |
| `C14-redress-blocked-structural` | rejected by REDRESS 96-98 and P2G |
| `C15-rejected-product-shortcuts` | rejected as generic SIMD/ASM route; may return only as generated per-grammar work with fresh gates |

Next S-P3 work: write `SPEC.md` and `DISPATCH-PROMPT.md`. They must select
waves from this shortlist, assign exact owner paths and revert protocol, bind
the relevant floor tables into executable gates, and refuse implementation
requests that bypass P2G, W0, or the global pre-blocks above.
