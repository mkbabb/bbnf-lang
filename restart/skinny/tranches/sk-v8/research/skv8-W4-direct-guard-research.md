# SK-V8 W4 Direct Guard Research

Date: 2026-05-18.

Status: W4 research record. This is not a source patch.

Authority:

- `restart/skinny/tranches/sk-v8/SPEC.md` Section 7.
- `restart/skinny/tranches/sk-v8/HANDOFF.md` W4 active wave record.
- `skinny/RESULTS.md` `SK-V8-open` direct rows.
- `skinny/REDRESS.md` items 54, 55, 66-69, 72, 80, and 84.

## Entry State

W4 may select one to three `N-direct` rows, must name strict direct thresholds
from `SK-V8-open`, must preserve generated Track 1 versus independent hand
Track 2 separation, and must route residual direct rows without treating the
synthetic digest as typed product proof.

The current direct surface has three measured GO rows:

| Row | Track 1 Mbps | Track 2 Mbps |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 21151 | 19434 |
| `marine_ik/direct_to_struct` | 9357 | 9488 |
| `unicode_basic/direct_to_struct` | 9363 | 8420 |

The remaining 14 direct rows are `N-direct / NO-GO`. The W4 strict floor is
the existing direct guard: Track 1 and Track 2 must each be within 1.10x
`sonic-rs` strict time, which is equivalent to each lane being at least
`sonic_mbps / 1.10`.

## Candidate Partition

Rows where generated Track 1 already clears the same-run sonic strict direct
floor and the miss is Track 2-only are the only bounded W4 candidates. Rows
where Track 1 misses require generated SinkOnly or direct output-contract work,
which falls into the already rejected string/materializer families or a later
typed-product plane.

| Row | Track 1 | Track 2 | sonic-rs | Floor | Baseline failure |
|---|---:|---:|---:|---:|---|
| `apache_builds/direct_to_struct` | 8306 | 7796 | 8852 | 8048 | Track 2 needs +3.2% |
| `numbers/direct_to_struct` | 9773 | 6966 | 7953 | 7230 | Track 2 needs +3.8% |
| `random/direct_to_struct` | 7751 | 6952 | 8141 | 7401 | Track 2 needs +6.5% |

Other Track-1-clearing rows (`twitter`, `github_events`) need much larger
Track 2 moves and are not selected for W4. Rows such as `mesh`, `instruments`,
`unicode_mixed`, `unicode_escapes`, `distinct_values`, and
`y_string_unicode` fail generated Track 1 as well and remain residual direct
rows unless a later wave changes the direct output contract.

## Route Screen

Pre-blocked source routes remain closed:

- REDRESS 54 and 55 reject sink-local decoded stats and quote-source streaming
  hash for direct strings.
- REDRESS 66 rejects direct source-hook receiver folding.
- REDRESS 67 rejects parser-owned decoded scratch.
- REDRESS 68 rejects byte-output unescape materialization inside the current
  `Cow<str>` API.
- REDRESS 69 rejects semantic string fact hashing for the current digest
  workload and routes the larger question to the direct output contract.
- REDRESS 72 explicitly keeps cap-16 out of generated direct SinkOnly and hand
  direct Track 2.
- REDRESS 80 rejects stale Canada/numeric mantissa widening with no measured
  fallback pool.
- REDRESS 84 rejects object-pair value-byte control compaction and Track 2
  coupling to generated helper shape.

The remaining admissible W4 route is narrower: keep the direct digest output
contract unchanged, keep generated Track 1 unchanged, keep hand Track 2
independent, but stop constructing temporary scalar child digests in the hand
Track 2 parser when the parent container can fold the scalar directly. This is
the same semantic digest math already used by `JsonDigestSink` for generated
Track 1 object/array scalar callbacks; the hand parser still owns its cursor,
string matching, number parsing, object/array recursion, and errors.

## Owner Paths

Candidate source owner:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`

No runtime, codegen, parser, scanner, BIR, directive, substrate, or generic
crate owner is nominated for W4.

## Verification Already Run

Baseline direct correctness and JSON/conformance checks pass from the Skinny
workspace:

```text
cargo test -p bbnf-bench direct_struct -- --nocapture
cargo xtask check-json
cargo xtask check-conformance
```

The initial root-level `cargo test -p bbnf-bench direct_struct` command failed
only because the Cargo workspace is `skinny/`, not the repository root.

## Research Finding

W4 has one bounded implementation candidate: independent Track 2 scalar-parent
folding for `apache_builds`, `numbers`, and `random` direct rows. If this fails
the row floors or guard floors, W4 must reject and route direct guard triage
without reopening the pre-blocked direct string/materializer/mantissa routes.
