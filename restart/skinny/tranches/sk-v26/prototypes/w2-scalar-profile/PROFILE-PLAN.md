# W2 grammar-derived scalar classifier decision plan

Status: **SEALED BEFORE MEASUREMENT**.

This plan governs only the isolated scalar prototype in this directory. It does
not authorize production integration, SIMD/ASM work, or a SOTA claim. The
prototype compares two scanners inside the same consuming, table-driven grammar
parser:

- `oracle`: one byte (or one UTF-8 scalar) at a time;
- `classifier`: eight-byte scalar classification blocks derived from each
  terminal's compiled start/continuation/safe-byte tables, with explicit scalar
  fallback for tails, escapes, UTF-8, and rejected blocks.

Exact terminals, lexical candidate ordering, and parser productions are data in
the prototype grammar. Neither scanner contains JSON-, CSS-, field-, or
delimiter-name dispatch. The same generic grammar, fixtures, parser,
full-consumption rule, event stream, checksum, allocator, and output retention
apply to both arms.

## Semantic and eligibility gates

Measurement is inadmissible unless all gates pass:

1. byte-for-byte equality of retained canonical token events, spans, source
   bytes, full-consumption outcome, and checksum for every valid and invalid
   fixture;
2. equality for every offset 0 through 15 and every tail length 0 through 31 of
   the raw lexical probes;
3. equality for 10,000 deterministic generated documents, including malformed
   early/middle/late cases;
4. valid UTF-8 is required; malformed UTF-8 is a typed rejection in both arms;
5. no classifier block may cross a quote, escape, control byte, lexical-class
   boundary, non-ASCII scalar, or end of input;
6. eligibility, eight-byte block bytes, and each fallback cause are counted.

An exact terminal has no lexical-region eligibility. A class terminal is
eligible when it has a compiled continuation table. A quoted terminal is
eligible when opener, closer, escape, and safe-byte policy compile without
ambiguity. UTF-8 scalars, escapes, tails shorter than eight bytes, and the first
rejecting block are intentional fallbacks, not silently reclassified work.

## Fixtures and declared policies

The full-parser matrix crosses 64 B, 1 KiB, 64 KiB, and 1 MiB with four
grammar-neutral families: compact bindings/calls, whitespace and punctuation,
long scalar regions, and UTF-8/escape-heavy scalars. Separate correctness
witnesses cover deep nesting, shallow/wide input, every block tail/alignment,
and early/middle/late invalid input.

- semantic plane: P2 token/event;
- consumption: the complete input, including trailing trivia;
- output: retained token kind, exact source span, source-byte digest contribution,
  end-of-input event, and final checksum;
- UTF-8: whole input must be valid UTF-8; non-ASCII scalars are preserved raw;
- strings: raw source bytes and escapes are retained, not decoded;
- numbers: lexical decimal validation only, raw exact spelling retained;
- duplicate/order policy: not applicable to this statement grammar; token order
  is retained exactly;
- recovery: none; first typed lexical or syntax error rejects;
- input mutation/padding/copying: input is immutable; fixtures are generated once
  before timing and exact-size padding is grammar trivia;
- reuse: timed rows reuse the event and token buffers; separate allocation probes
  report both fresh and reserved/reused operation.

## Measurement and estimator

The release command uses:

```text
RUSTFLAGS="-C target-cpu=native -C opt-level=3 -C codegen-units=1"
cargo run --release --locked
```

There are 8 untimed warmup pairs and at least 40 measured pairs per row. Arm
order alternates every pair. `black_box` retains input and checksum work.

Before candidate comparison, full-parser A/A calibration runs two labels of the
identical oracle path through the complete harness. The paired estimator is the
median of `ln(control_ns / candidate_ns)`, reported as percent improvement.
Intervals use 50,000 deterministic paired bootstrap resamples. Family-wise
simultaneous 95% intervals use Bonferroni correction over every full-parser
candidate row. Raw-classifier rows are observational and receive a separate
Bonferroni family.

For each full-parser row the calibrated noise floor is the greatest absolute
endpoint of its simultaneous A/A interval. Its practical improvement floor is
`max(10%, 3 * noise_floor)`. The regression guard is `max(3%, noise_floor)`.

Counters unavailable without privileged tooling on this host (cycles,
instructions, branch misses, and cache misses) must be recorded as unavailable,
not estimated. Wall-time rows report raw nanoseconds, ns/document, and GiB/s.
Allocation probes count allocation/reallocation calls and requested bytes
through the process global allocator; they do not claim peak RSS.

## Predeclared disposition

- **REJECT** if any semantic/eligibility gate fails, or if a non-pathological
  full-parser row has a simultaneous upper confidence bound worse than its
  regression guard.
- **ACCEPT** for later production consideration only if the simultaneous lower
  bound clears the practical floor for at least two representative fixture
  families (at 64 KiB or larger), no full-parser row violates the regression
  guard, reused parsing adds zero allocations beyond caller-owned retained
  output capacity, and code/generated-size costs are recorded.
- **BANK** when correctness passes and the raw classifier has a significant
  useful region but the full parser does not meet `ACCEPT`, or when full-parser
  evidence is inside calibrated noise. A microkernel result alone can never
  produce `ACCEPT`.

The program computes this disposition mechanically from the recorded samples.
Changing sample counts, estimators, thresholds, fixtures, or comparison families
after the first run requires a new plan version and makes earlier runs
exploratory.
