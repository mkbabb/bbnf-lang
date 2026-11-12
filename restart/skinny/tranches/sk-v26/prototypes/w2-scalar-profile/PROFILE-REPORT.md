# W2 scalar classifier profile report

Disposition: **REJECT** this always-on eight-byte scalar classifier. This is a
prototype disposition, not a rejection of grammar-derived classification as a
research direction and not a production/SOTA claim.

## What was measured

The prototype compiles generic terminal data into:

- a byte-indexed lexical choice table;
- exact terminals with word-boundary policy;
- class terminals with separate first and continuation tables;
- quoted terminals with opener, closer, escape, control-byte, and safe-region
  tables;
- generic sequence, choice, repetition, option, recursive-rule, and
  full-consumption parser nodes.

The oracle advances by one byte or UTF-8 scalar. The candidate consumes eligible
ASCII regions in eight-byte scalar blocks, falling back for tails, the first
rejecting block, UTF-8, and escapes. Both arms retain the same ordered token
kind, exact source span, source-byte checksum contribution, and EOF event.
Nothing dispatches on JSON, CSS, field names, or a delimiter-only loop.

Correctness passed 10,532 checks: the 16-cell full fixture matrix, 512
alignment/tail combinations, deep/invalid witnesses, malformed UTF-8, and
10,000 deterministically generated valid/invalid documents. Every retained
event stream, typed outcome, span, checksum, and raw-probe endpoint matched.
All timed checksums are equal and nonzero.

## Profile result

The simultaneous intervals below are paired-bootstrap 95% intervals after the
predeclared Bonferroni correction. Positive values favor the classifier.

| Family | Bytes | Estimate | Simultaneous interval | Oracle → classifier GiB/s | Eligible bytes in full blocks |
|---|---:|---:|---:|---:|---:|
| compact | 65,536 | -1.84% | [-3.73%, 1.06%] | 0.111 → 0.109 | 0.02% |
| compact | 1,048,576 | -1.53% | [-2.96%, 0.79%] | 0.111 → 0.110 | 0.00% |
| whitespace/punctuation | 65,536 | -23.83% | [-29.19%, -15.40%] | 0.166 → 0.132 | 14.06% |
| whitespace/punctuation | 1,048,576 | -17.22% | [-24.56%, -11.64%] | 0.160 → 0.135 | 14.04% |
| long scalar | 65,536 | +31.49% | [28.27%, 36.52%] | 0.516 → 0.778 | 99.98% |
| long scalar | 1,048,576 | +37.98% | [37.58%, 38.57%] | 0.493 → 0.797 | 100.00% |
| UTF-8/escape | 65,536 | -5.14% | [-6.71%, 0.43%] | 0.479 → 0.456 | 0.00% |
| UTF-8/escape | 1,048,576 | -1.41% | [-7.80%, 4.30%] | 0.455 → 0.452 | 0.00% |

Raw classification confirms that the primitive is real but conditional:

| Raw region | Large-input simultaneous result |
|---|---:|
| ASCII name class | +41.40% to +42.51% at 64 KiB; +11.58% to +31.48% at 1 MiB |
| ASCII number class | +40.59% to +44.37% |
| quote-safe ASCII | +68.60% to +75.71% |
| quote/UTF-8/escape fallback | -28.59% to -1.39% |

The predeclared `REJECT` rule fires because whitespace/punctuation rows have
simultaneous upper bounds beyond their calibrated regression guards; compact
1 KiB also violates its guard. A long-scalar microkernel and one end-to-end
family cannot cancel regressions elsewhere.

Reserved parsing performs zero allocations in both arms. Fresh parsing uses the
same caller-owned token/event growth shape; at the largest token-dense fixture,
the classifier requested at most 50,331,456 bytes across two initial allocations
and 34 growth reallocations, then retained 25,165,824 bytes of buffer capacity.
These are retained P2 output buffers, not an input copy or a peak-RSS claim.

## Measurement identity

- host: Mac17,7, Apple M5 Max, macOS 26.4.1 (25E253), AC power;
- toolchain: rustc 1.96.0-nightly (02c7f9bec, LLVM 22.1.2);
- flags: `-C target-cpu=native -C opt-level=3 -C codegen-units=1`, release
  profile with thin LTO, abort panic, and stripped symbols;
- subject commit: `cd49c8819190abac9f71743da8d05bf17ce48ab1`;
- stable pre/post dirty digest:
  `a4f45feed53ee127d95e2f87000b980d2e075e2bd4942c6ca9d2fc8ff7a02a7c`;
- sealed-plan SHA-256:
  `bb132f2d671c9027b091f55dd1b2fb5affeced2045d754f4e466dac89c46cf2a`;
- harness-source manifest SHA-256:
  `a1719c5e9f337779ec4a7a1295d2757c3f5e7bbe1e9b00994752618fb8328092`;
- clean release build: 2,523,859,375 ns;
- executable: 524,736 bytes, SHA-256
  `4c2aa5f275fad665021f4b1b48cb96175e1577286bf5962982ec5c15f8b910c3`.

`profile.raw.json` is the machine receipt. It contains every input digest, all
40 raw paired samples per row, all A/A samples, intervals, ns/document, GiB/s,
allocation/capacity probes, eligibility/fallback counts, checksums, environment,
policies, footprint, mechanical reasons, and limitations.

## Routing

Do not transpose this always-on implementation into the W2 production scanner.
The evidence supports a later, separately pre-registered prototype that makes
classifier selection part of the grammar-derived plan and proves a cheap
region-length/ASCII eligibility guard before paying block-classification
overhead. Quote-safe long regions are the strongest retained hypothesis;
UTF-8/escape-heavy and short trivia/atom regions must stay on the byte/scalar
oracle unless a new design changes their measured cost.

This run is hot-cache only. CPU cycles, instructions, branch/cache counters,
cold cache, guarded pages, production emitter/code-size effects, and external
competitors were unavailable or outside this isolated W2 prototype. The host
load was high and is retained in the JSON; paired interleaving and A/A
calibration limit but do not erase that constraint.
