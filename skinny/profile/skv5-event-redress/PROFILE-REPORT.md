# SK-V5 Event-Cursor Redress Profile Report

Date: 2026-05-14

This profile set records the baseline immediately after rejecting the
byte-class whitespace `EventCursor` experiment. The experiment was reverted;
these captures are from the clean retained/generated runtime.

## Captures

| Capture | Command | Loop Mbps | Profile |
|---|---|---:|---|
| Retained parse, twitter | `samply record ... profile-lazy 12000 twitter` | 11396 | `lazy-twitter.profile.json.gz` |
| Retained parse, random | `samply record ... profile-lazy 20000 test_data/random.json` | 7339 | `lazy-random.profile.json.gz` |
| Generated direct, unicode_mixed Track 1 | `samply record ... profile_direct 15000 unicode_mixed track1` | 3885 | `direct-unicode-mixed-track1.profile.json.gz` |

All captures used:

```bash
CARGO_TARGET_DIR=/tmp/skv5-profile-target
RUSTFLAGS="-C target-cpu=native"
samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open
```

## Retained Parse Finding

Both retained profiles still collapse symbol-level attribution into
`runtime::generated_json::generated::parse_value_at`.

Top PC-level leaves:

| Capture | Dominant leaf family | Samples |
|---|---|---:|
| twitter | `parse_value_at+0xfec`, `+0xff8`, `+0x1004`, `+0x1704`, `+0x1120`, `+0x100c` | 21,256 |
| random | `parse_value_at+0x2e0`, `+0x2d4`, `+0xff8`, `+0xfec`, `+0x2ec`, `+0x2f4` | 44,471 |

Interpretation: H.W1 remains a parse-hub decomposition problem. A wrapper that
renames whitespace skipping as event consumption does not remove the collapsed
hub. The next admissible implementation must consume the scanner's live JSON
structural emit mask and keep only O(1) pending state.

## Direct Finding

The generated direct `unicode_mixed` profile has two distinct hot families:

| Family | Representative leaves |
|---|---|
| Generated object-value dispatch | `parse_object_value_at_direct::<JsonDigestSink>+0x1614`, `+0x44c`, `+0x17dc`, `+0x164c`, `+0x17f8` |
| String decode/materialization | `parse_that_regex::unescape_json_string+0x54c`, `+0x294`, `+0x1fc`, `+0x5b8`; `JsonDigestSink::object_string` leaves |

Interpretation: direct `N-direct` is not solved by retained cursor work alone.
It still needs a fused decoded-string sink primitive or equivalent generated
source-hook specialization that avoids the rejected generic decoded visitor.

## Redress Link

`skinny/REDRESS.md` items 51-52 carry the binding decision:

- byte-class whitespace cursor: rejected and reverted;
- structural-mask EventCursor: still admissible;
- fresh baseline profiles: retained parse hub plus direct decoded-string leaves.
