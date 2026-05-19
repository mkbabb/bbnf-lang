# SK-V10 P2-G: Candidate And Micro-Proof Ledger

Pass: S-P2 Research. Cycle: V1 fold.
Date: 2026-05-19.
Scope: canonical S-P2 post-CHALLENGE candidate ledger for S-P3 eligibility.
Output: this file.
Authority: folds S-P2 V1 CH1, CH2, CH4, CH5, and CH6 REVISE dispositions.

## Section 1 - Ledger Rule

S-P3 may shortlist only candidates named in this ledger. Earlier P2-A through
P2-F aliases are evidence, not dispatch authority. An alias missing here is
`inventory-only` for SK-V10 until a later accepted hardening fold adds row
gates, scalar/reference status, and same-wave consumers.

Disposition vocabulary:

- `row-gated`: may move a row only when the SPEC names corpus floors, same-run
  comparator/oracle evidence, and a same-wave consumer.
- `proof-only`: may close on correctness/micro-proof with no `RESULTS.md`
  movement.
- `gate-only`: may update evidence/reporting but cannot move behavior rows.
- `inventory-only`: architecture or future-host evidence; not S-P3 eligible for
  SK-V10 source waves on the current Apple aarch64 host.
- `rejected`: pre-blocked route; do not shortlist.

All kernel or SIMD rows require micro-prove-first before production wiring:
scalar oracle, checkasm target, host feature gate, representative corpus slices,
caller microbench, failure threshold, and same-wave consumer. The current host
is `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`
(`research/p1/p1e-hot-leaf-attribution.md:10`).

## Section 2 - Canonical Candidates

| Canonical id | Alias family | Status | P1 antecedent | S-P3 eligibility |
|---|---|---|---|---|
| `C1-direct-output-contract` | Direct output/control-path contract; Direct SAX-style sink; direct product-plane contract | `row-gated` or contract-only, depending on SPEC wave | `direct_struct`, `array_walk`, `object_walk`, `string_tiny_scan`, `number_digit_scan` on direct rows | Eligible only if SPEC binds direct Track 1, independent Track 2/oracle, sonic direct strict comparator, same-run run id, and `gate-json` consumer. Digest movement never admits typed rows. |
| `C2-instruments-typed-admission` | `instruments` typed product row | `row-gated` | `string_tiny_scan`, `whitespace_skip`, `object_walk` on `instruments` | Eligible as a JSON product-plane row only: generated typed output, Track 2 checksum oracle, serde_json typed, sonic typed, and same-run Criterion rows must all exist. |
| `C3-root-typed-generalization` | Root-type typed generalization; root/layout algebra | `proof-only` unless paired with typed row gate | `github_events` top-level array and `gsoc-2018` map-root blockers from Alpha | Eligible as proof-only for root model/codegen/tests. No `RESULTS.md` movement unless the same wave also supplies typed comparator rows and full-fixture parity. |
| `C4-tiny-string-proof` | Strict tiny-string terminator; `tiny_plain_string_first_special_64`; `STRING_FIRST_SPECIAL_16_INLINE`; `bounded_plain_string_end`; Tiny string scan | `proof-only` first; row-gated only after proof | `string_tiny_scan` (`p1e:40`, rows `p1e:53`, `:57`, `:58`, `:63`, `:68`; typed `p1b:101`, `:103`, `:104`) | Eligible for one call-site proof at a time. Generic API returns offsets/classes under caller-owned delimiter/control policy. Per-plane caps are distinct: generated direct cap 8; typed parse cap 32; typed skip cap 96; retained cap 16 excluded unless explicitly targeted. |
| `C5-full-string-proof` | Strict full-string escape/control scanner; `string_full_scan_escape_control_64`; `plain_string_special_span` | `proof-only` first; row-gated only after proof | `string_full_scan` on `unicode_mixed`, `unicode_escapes`, Track 2 `unicode_basic` (`p1e:41`, `p1b:91-93`) | Eligible only with caller-supplied class policy and a current direct/typed string caller. Parse-only rows cannot be SOTA admissions. |
| `C6-hex-escape-proof` | Unicode escape quartet decode/classify; `unicode_escape_hex4_decode`; `escape_run_decode_x4`; hex escape unit/run | `proof-only` first; row-gated only after proof | `string_escape` / `unicode_escape_hex` on `unicode_escapes`, `unicode_mixed`, `y_string_unicode` (`p1e:42`, `p1b:91-95`) | Generic kernel is hex decode/classify only. Grammar templates own slash/introducer, `\\u`, surrogate policy, CSS 1-6 digit termination, and Sheets quote-doubling. |
| `C7-string-segment-fold` | `string_segments_fold`; decoded segment iterator | `proof-only` and per-plane gated | `alloc` on `y_string_unicode`, plus unicode escape rows (`p1e:47`, `:69`) | Eligible only as generated per-grammar output-plane work unless the consumer trait is free of JSON key/value, digest, and sink-local decoded stats. Direct digest movement cannot admit typed rows. |
| `C8-digit-number-proof` | Digit-run scanner; JSON-number span scanner; `number_digit_run_classify_64`; `digit_run_span_64`; `number_span_parts` | `proof-only` first; row-gated only after proof | `number_digit_scan` / `number_scan` on `canada`, `mesh`, `numbers`, `marine_ik`, typed `mesh`, typed `marine_ik` (`p1e:43`, `p1e:55`, `:59`, `:64`, `p1b:105-106`) | Eligible only after split into grammar-neutral digit masks/accumulators and generated grammar-owned number policy. Canada typed remains blocked without full-fixture proof. |
| `C9-whitespace-class-skip` | `whitespace_skip_mask_64`; `ascii_class_skip`; byte-class skip | `maintain-only` unless row-gated | `whitespace_skip` on `citm_catalog`, `random`, `mesh`, `marine_ik`, typed `citm_catalog` (`p1e:44`, `p1b:80`, `:85`, `:86`, `:88`, `:102`) | Eligible only as caller-owned class-table skip with maintain floors and exact consumer. Generic `skip_ascii_whitespace` is not the Lock 14 abstraction. |
| `C10-byte-class-movemask` | transient byte-class/table classify; movemask; equality-set classify | `inventory-only` unless paired to direct/typed caller | `simd_movemask` on `gsoc-2018` and secondary rows (`p1e:46`, `:61`, `p1b:87`) | Not row-moving by itself. W3/retained structural cursor consumption is rejected. |
| `C11-tape-economy-contract` | tape capacity/flag economy; capacity plan | `proof-only` / invariant only | materialization ratios, not a hot leaf | Not a primitive. `BBNF_CAPACITY_PLAN=exact|oneshot-simd` and structural capacity pre-scans are diagnostic/env-only for SK-V10 row movement; default one-pass production plan is the admission baseline. |
| `C12-telemetry-refresh` | comparator/telemetry refresh; sidecar freshness manifest | `gate-only` | no P1 hot leaf | Eligible only if every emitted field is consumed by `gate-json` same-wave. It cannot move behavior rows. |
| `C13-x86-secondary-isa` | AVX2/AVX-512/VBMI2/BITALG/GFNI/VPCLMUL/VNNI/IFMA routes | `inventory-only` | no current-host evidence | Not S-P3 eligible for SK-V10 source waves on Apple aarch64. Requires a future same-host x86 profile, scalar oracles, checkasm, row floors, and consumers. |
| `C14-redress-blocked-structural` | structural cursor from movemask; retained class column; W3/union; mask-next/bulk emit as default; PMULL/CTZ defaults | `rejected` | invalidated by REDRESS | Do not shortlist. Transient byte masks inside a current string/number caller remain possible only through another ledger row. |
| `C15-rejected-product-shortcuts` | allocation materializer as SIMD primitive; array/object dispatch hint as bbnf-simd primitive; Canada typed shortcut | `rejected` | blocked or not a SIMD primitive | May return only as generated per-grammar output/control work with fresh material differential and row gate; not as generic SIMD/ASM. |

## Section 3 - Row Floors And Consumers

Direct row movement uses the Alpha-E direct matrix. A direct row moves only if
both generated Track 1 and independent Track 2/oracle meet
`ceil(sonic_direct / 1.10)` under the same run id and strict comparator plane.
Initial floors:

| Corpus | Floor Mbps |
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

Existing typed maintain rows remain admitted only if they keep their typed
`A / GO` gate: `twitter`, `citm_catalog`, `apache_builds`, `update_center`,
`mesh`, and `marine_ik`. New typed rows require same-wave generated typed,
Track 2/oracle, serde_json typed, sonic typed, checksum parity, and Criterion
rows.

For any aarch64 SIMD/string/unescape production wiring, the W10b maintain block
from Alpha-E is binding unless S-P3 tightens it:

| Corpus | Floor Mbps |
|---|---:|
| `canada` | 15866 |
| `citm_catalog` | 28630 |
| `instruments` | 15865 |
| `marine_ik` | 11831 |
| `mesh` | 12186 |
| `numbers` | 17596 |

Consumer planes are not interchangeable:

| Plane | Consumer authority |
|---|---|
| retained parse | `TapeBuilder` offset/flag writes and retained `ValueRef` views |
| generated direct digest | generated `JsonSink` callbacks plus independent hand Track 2 digest |
| real typed product | generated `DirectParser` field writers plus typed checksum/oracle |
| hand Track 2 | independent oracle only; never proof that Track 1 took the same path |

## Section 4 - Micro-Proof Cost Bands

| Candidate ids | Allowed SK-V10 slice | Cost band | Close rule |
|---|---|---:|---|
| `C1`, `C2`, `C3`, `C12` | contract/gate/proof waves | 80-420 LOC depending on codegen/gate scope | Close on named tests, comparator rows, or proof-only artefact; no behavior movement unless row gate is present. |
| `C4`, `C5`, `C6`, `C8`, `C9` | one primitive family, one call-site proof | 90-260 scalar/checkasm/microbench LOC before production | Close proof-only on scalar/checkasm/microbench. Production caller wiring is a later row-gated slice unless S-P3 proves it fits. |
| `C7` | one output-plane segment/fold proof | 120-260 LOC | Close only for the named direct or typed plane; no cross-plane movement. |
| `C10`, `C13` | inventory / maintain research | n/a | No SK-V10 behavior close. |
| `C14`, `C15` | rejected | n/a | No dispatch. |

S-P3 must split bundled kernel work. A single <=90-minute wave may not combine
multiple primitive families, scalar oracle, checkasm, microbench, and multiple
production consumers unless CHALLENGE explicitly accepts the combined cost.

## Section 5 - Source Hygiene

The P2-C ISA tables are inventory unless a wave plan supplies
instruction-specific manual or intrinsics anchors. Broad Arm/Intel landing pages
do not authorize implementation. The P2-B DAV1D/FFmpeg process references are
process background unless pinned source line anchors are carried into the S-P3
plan. For SK-V10, only current-host aarch64 evidence can become behavior
authority.

## Section 6 - Sources

- `restart/skinny/tranches/sk-v10/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH1-correctness.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH2-generality-lock14.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH4-cost-microproof.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH5-hidden-coupling-lock1.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH6-anti-paper-close.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
