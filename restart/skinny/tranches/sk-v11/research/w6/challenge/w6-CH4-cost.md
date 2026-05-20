# SK-V11 W6 CH4 - Cost Lens

Pass: W6 CHALLENGE.
Lens: CH4 Cost.
Date: 2026-05-20.
Disposition: REVISE.

## Verdict

The selected `unicode_mixed/direct_to_struct` row is the right W6 cost target,
and the proposed no-allocation escaped-string digest fold has a plausible cost
mechanism. The row's Track 2 floor miss is only 161 Mbps, or 6.6%, and W6 R4
attributes Track 2 time to `full_string` 26.4%, `unescape` 18.4%, and
`validate_escape` 13.8%. A direct decoded-byte fold that avoids allocating an
owned decoded `String` and avoids a second hash/length pass needs to remove only
a modest share of that string/escape surface to clear 2588 Mbps.

That is not enough to dispatch redress. The plan's probe and Criterion packet
are under-specified for CH4. They do not bind the exact repeated Track 1/Track 2
probe form from W6 R4, do not require pre-Criterion guard probes to hold, and do
not include a Criterion command for the typed guard rows even though the exit
gate says guard floors hold. A W6 source patch should not start until those
measurement details are fixed.

## Cost Assessment

The handwritten budget is credible only for the narrow bench-owned version:
`direct_struct.rs` source-method overrides, a local Track 2 escaped-digest path,
focused escaped-fixture parity, gate/report consumption, `RESULTS.md` on PASS,
and `REDRESS.md`. That shape can plausibly stay inside the Section 10 cap of
<=360 handwritten source/test/gate LOC and the <=90 min wave cap.

The budget is not credible if redress adds a generic `parse-that-regex`
segment-stream API, x4 scalar oracle, strict x4 checkasm expansion, codegen
rewrites, or generated runtime changes in the same wave. Those are valid future
routes only as a revised or split plan, because they would turn this from an
output-consumer cost slice into the larger C3 primitive package that W6 research
already marked as high risk.

The independent Track 2 mechanism is plausible but must be locked down before
implementation. Track 2 must either duplicate the local escaped-digest fold or
use a CHALLENGE-accepted output-plane scalar helper that has no generated-parser
or generated-sink dependency. A shared helper is acceptable for cost only if the
plan also keeps separate caller paths and proves generated Track 1 is not being
used as the Track 2 oracle.

## Required Plan Changes

1. Replace the loose probe commands with the exact W6 R4 probe shape: build
   native `profile_direct`, run before/after probes for `unicode_mixed`,
   `unicode_escapes`, `y_string_unicode`, and the direct guard rows with both
   `track1` and `track2`, and run typed guard probes with both
   `real_typed_track1` and `real_typed_track2`.

2. Bind the pre-Criterion threshold explicitly: at least repeated post-patch
   `unicode_mixed` Track 2 probes must be >=2620 Mbps, Track 1 must remain
   above 2588 Mbps, and direct plus typed guard probes must remain above their
   Section 0.5 floors before Criterion is authorized.

3. Add the missing typed-guard Criterion packet:

```sh
CRITERION_HOME=/tmp/skv11-w6-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench \
  --bench json_parity -- \
  'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
```

4. State that W6 admits only from the same Criterion home/run family containing
   the selected row, scout rows, direct guards, typed guards, and strict
   comparators. Stale `RESULTS.md` guard values do not satisfy CH4.

5. Keep the redress owner slice narrow. If implementation requires
   `parse-that-regex`, `bbnf-simd`, `codegen`, or generated-runtime changes,
   return to plan instead of consuming the current W6 budget.

## CH4 Disposition

REVISE. The cost mechanism is plausible for `unicode_mixed` Track 2, but the
plan must tighten probe, guard, Criterion, and owner-budget requirements before
source redress.
