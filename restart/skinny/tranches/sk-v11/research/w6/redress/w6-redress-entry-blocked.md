# SK-V11 W6 Redress Entry Block

Date: 2026-05-20.

Scope: W6 escaped segment and hex decode slice.

Disposition: BLOCKED before implementation dispatch.

## Evidence

W6 completed Phase 1 research, Phase 2 plan, and mandatory six-lens CHALLENGE.
The selected plan was the JSON direct-plane escaped-segment digest fold:
override `JsonDigestSink::*_source` methods in
`skinny/crates/bbnf-bench/src/direct_struct.rs` and fold decoded escaped bytes
into the existing direct digest fields without allocating a decoded
`String`/`Cow<str>`.

CHALLENGE did not accept the plan:

- CH2 accepted the generality / Lock 14 frame because the plan carried
  REDRESS 113 forward and did not claim the non-JSON close axis.
- CH1 required a tighter line-cited correctness contract, full escaped fixture
  equality against generated Track 1, independent Track 2, `serde_json`, and
  `sonic-rs`, and an explicit Track 2 independence decision.
- CH4 accepted the plausibility of a `unicode_mixed` Track 2 cost mechanism,
  but required repeated probes, same-run guard/comparator binding,
  typed-guard Criterion coverage, and a tighter owner budget.
- CH5 required fail-closed coverage for all four `JsonDigestSink::*_source`
  paths, explicit gate/report consumption, and tighter Track 1 / Track 2
  independence.
- CH6 required attributable before/after movement, saved `samply` evidence that
  the new digest-fold consumer is hot, no deferred Track 2 independence clause,
  and an explicit negative x4 proof clause.
- CH3 was load-bearing: it found that the selected `JsonDigestSink::*_source`
  decoded-byte fold reopens REDRESS 54, with REDRESS 55/66/69 adjacency. The
  same sink seam, same direct digest length/fingerprint output contract, and
  same allocation-removal claim were already measured and rejected.

Because CH3 names an existing falsified implementation family, W6 cannot revise
the selected plan into source redress without choosing a different product
consumer representation outside the current SPEC Section 10 authority. Running
the patch would knowingly replay a pre-blocked REDRESS family.

## Disposition

No behavior source, generated runtime, SIMD kernel, benchmark body,
`skinny/RESULTS.md`, gate schema, or report schema moved. The rejected-patch
marker is `/tmp/skv11-waveW6-rejected.patch`; it is empty because no source
patch was attempted.

W6 admits no escaped-segment primitive, x4 production consumer, source-method
digest fold, non-JSON proof, or rejected-but-reusable scalar oracle. W7 may
dispatch only through SPEC Section 11's output digest / host-sink route, and it
must carry this W6 block plus REDRESS 54/55/66/69, 64, 82, 107, 108, 113, and
116/117 as pre-blocked or unresolved background.

## Evidence Commands

- `RUSTFLAGS="-C target-cpu=native" cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench direct_digest -- --nocapture`
- `git diff --exit-code -- skinny/RESULTS.md`
- `CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml -p bbnf-bench --bin gate -- --advisory`
