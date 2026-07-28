# Benchmark constitution

Performance is admitted only after semantic equivalence. A faster implementation
of less work is a different product, not a win.

## Required manifest

Every benchmark row records:

```text
subject commit and dirty digest
competitor commit/version
host model, OS, CPU features, power mode, load
compiler/runtime versions and complete flags
input object hashes
semantic plane
validation scope
UTF-8 policy
string escape/unescape policy
number target and exactness policy
duplicate-key and member-order policy
depth/resource/error/recovery policy
retained output and checksum
input mutation, padding, and copying
allocator/parser/buffer reuse
raw samples and counter availability
```

An absent field makes the row observational, never certifying.

## Semantic planes

| Plane | Required work | Honest comparison class |
|---|---|---|
| P0 classify | candidate/class masks and carry checksum | BBNF scalar vs BBNF NEON |
| P1 check | consume all input and decide validity | full validator with no retained tree |
| P2 token/event | normalized full-consumption event checksum | cssparser component/token plane; BBNF reference |
| P3 lossless | validated borrowed/tape value preserving declared raw data | simdjson/simd-json equivalent lossless plane |
| P4 decoded DOM | decoded strings/numbers and retained unknown structure | sonic-rs/yyjson/simdjson/serde_json equivalent DOM |
| P5 selective | identical path result under identical validation promise | simdjson On-Demand/sonic lookup with checked and unchecked split |
| P6 typed | same schema, fields consumed and checksumed | sonic/serde/simd-json direct decode |
| P7 transform | identical transforms and serialization | Lightning CSS only at equivalent typed coverage |

Lightning CSS is not a tokenizer baseline. cssparser is not a typed-property
stylesheet baseline. Lazy iteration is not full validation unless every promised
value is forced.

Every named competitor is only a candidate until an adapter proves identical
consumption, validation, error, output, and checksum promises for the row.

## Corpus axes

The minimum controlled matrix includes:

- 64 B, 1 KiB, 64 KiB, and 1 MiB;
- compact and whitespace-heavy;
- punctuation-dense and long-string;
- ASCII, multibyte UTF-8, no escapes, and escape-heavy;
- integer, float/exponent, and exact-number stress;
- shallow-wide and deep-narrow;
- expected/reverse/missing/unknown typed fields;
- duplicate names under the declared policy;
- early/middle/late invalid input;
- all SIMD tails, alignments, and guarded page boundaries;
- CSS comments, both quote forms, escapes, identifiers, functions, blocks,
  dimensions, and malformed components.

`twitter`, `citm_catalog`, and `canada` may remain continuity fixtures. They never
constitute the claim by themselves.

## Measurement

Report ns/document, GiB/s, cycles/byte, instructions/byte, branch misses,
cache misses where available, allocations, peak retained bytes, output size,
binary/generated size, and compile time.

- Cold and hot cache are separate.
- Fresh allocation and reuse are separate.
- Checked and unchecked are separate.
- Cross-language overhead is amortized inside native batches.
- Every declared output is checksumed.
- Raw samples, not just medians, are retained.
- Paired interleaving is required for promotion comparisons.

Before the first promotion run, an A/A calibration alternates identical binaries
through the complete harness. The sealed decision plan declares the paired
estimator, warmup, minimum pair count, stopping rule, bootstrap/resampling method,
and comparison family. Promotion uses a simultaneous 95% interval after the
declared multiplicity correction. The practical improvement floor is the larger
of 10% and three times the calibrated A/A noise floor; the 3% regression budget
is likewise invalid if it lies inside calibrated noise.

Optimization promotion requires:

1. semantic and error equivalence on the declared plane;
2. 95% confidence that the target kernel improves;
3. at least 10% end-to-end improvement on two representative corpus families;
4. no reproducible regression above 3% on a non-pathological family;
5. justified code/binary/compile-time cost.

A microkernel win without end-to-end movement is banked, not shipped.

Correctness/API transposition is not subject to the 10% speed threshold. Its
decision budget is declared before measurement and must include maximum allowed
throughput, allocation, peak-memory, binary-size, and compile-time regressions.
An absent budget blocks transposition; it does not inherit an arbitrary global
number.

## Current baseline disposition

The 2026-07-28 local runs are **NON-CERTIFYING**:

- clean checkout lacked the benchmark inputs;
- local ignored files supplied the data;
- benchmark builds pulled broad unconditional dependencies;
- BBNF and sonic-rs retained different value semantics;
- runs were sequential rather than paired;
- no raw-sample artifact or environment manifest was emitted.

They are still useful direction: BBNF was slower than sonic-rs on all four tested
files, and therefore has no present basis for a SOTA claim.

## Reference disciplines

- simdjson: separate structural, DOM, and On-Demand work.
- yyjson: distinguish in-situ from ordinary DOM and expose layout cost.
- sonic-rs: specialize measured leaves and direct decode without a mandatory tape.
- regex-automata: choose terminal engines by capabilities and cost.
- dav1d: scalar/reference kernels, per-ISA substitution, differential checkasm.
- cssparser/Lightning CSS: compare only equivalent CSS planes.

Pins and upstream links used by the audit:

- simdjson v4.6.4: <https://github.com/simdjson/simdjson/tree/1bcf71bd85059ab6574ea1159de9298dcc1212c5>
- yyjson 0.12.0: <https://github.com/ibireme/yyjson/tree/8b4a38dc994a110abaec8a400615567bd996105f>
- sonic-rs 0.5.8: <https://github.com/cloudwego/sonic-rs/tree/e339451e37a8f2d3f9b93f155985d31fcbcb115a>
- Lightning CSS 1.33.0: <https://github.com/parcel-bundler/lightningcss/tree/c6a0c3cebf3395635e61075d2c81a96a710d4910>
- cssparser 0.35.0: <https://github.com/servo/rust-cssparser/tree/3dd4f63639925438d9932dafc0d81395847f9be4>
- dav1d 1.5.4: <https://code.videolan.org/videolan/dav1d/-/tree/54706fc6bc0cdecab7e9593974a4039cc038fca7>
- regex-automata 0.4.16: <https://github.com/rust-lang/regex/tree/regex-automata-0.4.16>
- re2c 4.1: <https://github.com/skvadrik/re2c/tree/4.1>
- tree-sitter 0.26.11: <https://github.com/tree-sitter/tree-sitter/tree/v0.26.11>
- simd-json 0.17.3: <https://github.com/simd-lite/simd-json/tree/0.17.3>
