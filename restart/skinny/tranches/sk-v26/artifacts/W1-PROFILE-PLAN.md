# W1 correctness-transposition decision plan

Status: **SEALED BEFORE THE W1 ACCEPTANCE RUN; NON-PROMOTING**.

This plan governs the W1 Rust-kernel acceptance measurements. Earlier package
size observations and developer test timings were exploratory and cannot satisfy
this plan. W1 is a correctness/API transposition, not an optimization promotion.
It therefore does not make a SOTA, cross-language throughput, or end-to-end BBNF
claim.

## Subjects and honest comparison boundary

- Candidate: the final committed `parse_that` and `bbnf-regex` packages on
  `codex/sk-v26-parse-that`; the acceptance receipt must record their commit,
  tree, and clean-package archive hashes.
- Production-v1 control: parse-that W0 payload `627c9b46a773a278ed8c60728c9d335be6747905`.
  It is admissible for package/dependency/topology comparison only. Its mutable
  parser, domain parsers, padding, recovery, and retained values do not perform
  the W1 kernel's work and cannot supply an equivalent throughput row.
- Invocation-v4 control: protected-main prototype
  `docs/tranches/T/prototypes/invocation-kernel-v4`, source ledger
  `6bc0e3fdc6296e3c0872208229104299ecc44e38a92a777df80f0a37005ca1dc`.
  It is admissible for state-law and package-topology comparison only. It is a
  TypeScript graph/memo/left-recursion carrier and has no equivalent Rust
  literal/regex/event throughput plane.

The absence of an equivalent predecessor makes all W1 raw throughput samples
observational. It does not license a synthetic speedup claim. The first
certifying end-to-end comparison belongs to W2/W4 after a genuine emitted BBNF
event vertical exists.

## Correctness/API budget

The candidate is rejected unless every row passes:

| Quantity | Maximum admitted regression / required bound |
|---|---|
| semantics | zero known divergence from the selected W1 laws except an explicitly named, tested correction of a reference defect |
| foreign-source safety | zero accepted foreign cursor, span, failure, or fault outcomes, including independently allocated empty inputs |
| rollback | zero cursor or sink leakage after mismatch or fault |
| termination | every non-progressing repeat returns a typed fault; no hang |
| regex compile boundary | every accepted byte pattern returns a checked table or a typed error; no panic; compiler and runtime accept the same class-count domain |
| allocation | zero allocations for successful/mismatching/faulting recognition with `NoSink`; zero allocations for erased event construction |
| peak retained memory | no input-sized allocation in the runtime recognition kernel; retained sink growth is caller-owned output and must be reported separately |
| runtime normal dependencies | exactly zero |
| packaged runtime size | no larger than the W0 production-v1 package; target at least 50% smaller |
| clean compile wall time | observational only, because W0 and W1 dependency graphs are not equivalent; any result above the W0 control must block and be explained |
| throughput | observational, non-promoting; no W1 cutover claim may cite it as an improvement |

The `bbnf-regex` construction package must retain one normal default dependency
or fewer, apply resource limits before NFA expansion, and execute
compiler-produced plans through the real `parse_that::RegexTerminal` in a
cross-crate law.

## Acceptance measurements

Run from a clean candidate tree with a fresh task-specific target directory:

1. replay all kernel, allocation, regex, cross-crate, doctest, rustdoc, and
   strict-lint gates;
2. produce clean `.crate` archives, verify them in a fresh empty `CARGO_HOME`,
   and record compressed/uncompressed size, file allowlist, dependency metadata,
   and SHA-256;
3. record source files, lines, and bytes for W0 production-v1 and W1;
4. record at least twenty raw wall-time samples for clean package compilation
   and the fixed recognition probes, without interpreting them as a regression
   comparison;
5. record host, OS, compiler, flags, input bytes/hashes, output checksum,
   allocation mode, and unavailable hardware counters.

No A/A calibration or confidence interval is required because no performance
promotion decision is permitted in W1. If a later wave attempts promotion, it
must seal a new paired decision plan before measuring.
