# SK-V6 profiling cohort C1: retained parse profiles

Date: 2026-05-15. Workspace: `/Users/mkbabb/Programming/bbnf-lang`.

Repo discipline: read-only. No repository files were edited. No new samply
captures were created because this task constrained writes to this report only.
Evidence below reuses existing repo and `/tmp` artifacts.

## Inputs read

- `restart/skinny/tranches/sk-v6/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v6/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v6/research/skv6-B3-profile-retained-three-way.md`
- Existing profile reports/artifacts under `skinny/profile/*` and `/tmp/skv6-B3-*`

## Artifact inventory

- Freshest retained parse-attribution binary found:
  `/tmp/skv6-B3-profile-target/release/profile-lazy`
- Build command recorded by B3:
  `CARGO_TARGET_DIR=/tmp/skv6-B3-profile-target cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution`
- Fresh B3 retained profiles found:
  `/tmp/skv6-B3-profiles/twitter.profile.json.gz`,
  `/tmp/skv6-B3-profiles/unicode_escapes.profile.json.gz`,
  plus matching `.syms.json`; B3 also has `gsoc-2018`, outside this C1 row set.
- Older expanded retained profiles found:
  `skinny/profile/skinny-expanded/{twitter,citm_catalog,canada,github_events,update-center,random,numbers,unicode_escapes}.profile.json.gz`
  plus matching `.syms.json` and `.stderr`.
- Older PC/PMU retained profiles found:
  `skinny/profile/wave2-pmu/{github_events,update-center,random,unicode_escapes,y_string_unicode}.profile.json.gz`
  and `skinny/profile/wave2-asm/{github_events,update-center,random,unicode_escapes,y_string_unicode}.profile.json.gz`.
- Candidate-4 attribution profiles found:
  `/tmp/skv6-wave2-candidate4-profiles/{citm,canada}-{base,cand}.profile.json.gz`.
- No BBNF retained samply artifact was found for current `distinct_values`.
  `distinct_values` evidence is therefore current `RESULTS.md` throughput plus
  REDRESS cap-16/native gate data.

## Current retained parse gate

c/B uses the existing SK-V6 convention `c/B = 28000 / Mbps`, assuming a
3.5 GHz Apple performance core.

| corpus | Track 1 Mbps | Track 1 c/B | Track 2 Mbps | Track 2 c/B | sonic/S Mbps | S c/B | read |
|---|---:|---:|---:|---:|---:|---:|---|
| twitter | 15597 | 1.80 | 12128 | 2.31 | 21184 | 1.32 | retained G; Track 1 is 73.6% of sonic |
| citm_catalog | 32459 | 0.86 | 20792 | 1.35 | 24910 | 1.12 | Track 1 beats sonic; Track 2 keeps row G |
| github_events | 15268 | 1.83 | 13034 | 2.15 | 22182 | 1.26 | retained G |
| update_center | 11912 | 2.35 | 9226 | 3.03 | 19983 | 1.40 | retained G |
| random | 10071 | 2.78 | 7800 | 3.59 | 15370 | 1.82 | retained G |
| unicode_escapes | 12905 | 2.17 | 12931 | 2.17 | 16048 | 1.74 | retained G |
| distinct_values | 9783 | 2.86 | 6100 | 4.59 | 16259 | 1.72 | retained G |
| y_string_unicode | 6290 | 4.45 | 6034 | 4.64 | 13673 | 2.05 | retained G |
| numbers | 20085 | 1.39 | 18671 | 1.50 | 13567 | 2.06 | guard A |
| canada | 18775 | 1.49 | 17133 | 1.63 | 12658 | 2.21 | guard A |

## Dominant profile evidence

| corpus | best retained profile path | profile Mbps/cB | dominant hot symbols or regions |
|---|---|---:|---|
| twitter | `/tmp/skv6-B3-profiles/twitter.profile.json.gz` | 11182 / 2.50 | `match_tiny_plain_string` 42.26%, `match_string_at_quote` 18.13%, `consume_container_next` 9.82%, `parse_key_colon` 5.48% |
| citm_catalog | `skinny/profile/skinny-expanded/citm_catalog.profile.json.gz` plus `/tmp/skv6-wave2-candidate4-profiles/citm-*.profile.json.gz` | older expanded 8947 / 3.13 | older: `parse_value` 53.67%, `simd_scan::scan_json_parse_index` 31.60%, `consume_structural` 10.11%; candidate-4 redress says container re-entry dropped 24.97% -> 14.51% self |
| github_events | `skinny/profile/wave2-pmu/github_events.profile.json.gz`; older expanded `skinny/profile/skinny-expanded/github_events.profile.json.gz` | PMU 18757 / 1.49; expanded 7378 / 3.80 | PMU/ASM: `parse_value_at` 85.5% inclusive; `match_tiny_plain_string` key 32.8%, value 17.3%; older expanded also shows `parse_string` + `match_json_string` around 16.2% |
| update_center | `skinny/profile/wave2-pmu/update-center.profile.json.gz`; older expanded `skinny/profile/skinny-expanded/update-center.profile.json.gz` | PMU 13555 / 2.07; expanded 5289 / 5.29 | PMU/ASM: short-string SWAR/tiny-string scalar loop dominates; older expanded: `simd_scan` 44.39%, `parse_value` 21.77%, `consume_structural` 16.35%, `parse_string` 13.22% |
| random | `skinny/profile/wave2-pmu/random.profile.json.gz`; older expanded `skinny/profile/skinny-expanded/random.profile.json.gz` | PMU 11242 / 2.49; expanded 6674 / 4.20 | PMU/ASM: `match_tiny_plain_string` key 31.0%, value 20.5%; high mixed dispatch entropy but string-tail dominates |
| unicode_escapes | `/tmp/skv6-B3-profiles/unicode_escapes.profile.json.gz` | 11068 / 2.53 | B3: `match_string_at_quote` 90.44% self; older PMU/ASM isolates escape/hex regions, including `\uXXXX` scalar decode |
| distinct_values | no retained BBNF samply artifact found | n/a | current gate plus REDRESS 72: generated retained cap-16 improved Track 1 by +57.5%; missing current samply is the evidence gap |
| y_string_unicode | `skinny/profile/wave2-pmu/y_string_unicode.profile.json.gz`; `skinny/profile/wave2-asm/y_string_unicode.profile.json.gz` | PMU 10510 / 2.66 | PMU/ASM: escape-decode scalar mode; `\uXXXX` hex decode 13.9% source-band, parse-pair/key escape recovery 35.1%; tiny-string cost also visible on short strings |
| numbers | `skinny/profile/skinny-expanded/numbers.profile.json.gz` | 8603 / 3.25 | guard: `parse_value` 71.25%, `simd_scan` 28.03%; current gate is A, so string intervention must preserve this row |
| canada | `skinny/profile/skinny-expanded/canada.profile.json.gz` plus `/tmp/skv6-wave2-candidate4-profiles/canada-*.profile.json.gz` | older expanded 4640 / 6.03 | guard: `parse_value` 58.91%, `simd_scan` 27.87%, `consume_structural` 13.04%; candidate-4 reduced container re-entry 27.37% -> 6.48% self |

## Interpretation

The current C1 row set splits into three retained parse modes:

1. Short/plain-string rows: `twitter`, `github_events`, `update_center`,
   `random`, and likely `distinct_values`. REDRESS 72 already admitted the
   generated retained cap-16 scalar probe because native rows moved materially:
   twitter +27.5%, citm +49.2%, github_events +16.9%, update_center +27.4%,
   random +21.8%, distinct_values +57.5%. The remaining failure is not "make
   cap-16 global": Track 2 and generated direct regress when cap-16 is applied
   globally.
2. Escape/string-tail rows: `unicode_escapes` and `y_string_unicode`. Fresh B3
   says `unicode_escapes` is almost entirely inside `match_string_at_quote`;
   older PMU/ASM shows `y_string_unicode` and `unicode_escapes` light up the
   scalar `\uXXXX` decode/escape path, but the REDRESS 64 four-unit retained
   validator failed because it helped `unicode_escapes` and regressed
   `y_string_unicode`.
3. Guard rows: `numbers` and `canada` are already retained A rows in the
   current gate. They should be used as regression sentinels, not optimization
   targets.

Scanner work is not the C1 retained parse close. The older expanded report
shows visible `simd_scan` cost, but B3 and SK-V6 synthesis supersede that for
current attribution: the active levers are generated string boundary,
escape-tail matching, and Track 2 shape/cost parity.

## Falsifiable retained intervention candidate

Candidate: replace the per-`\uXXXX` scalar nibble classification inside the
existing retained trusted string/escape path with a per-unit table/TBL
classifier, not the rejected four-unit contiguous-run validator. The candidate
must remain inside `match_string_at_quote` / parse-that string validation
boundaries, add no retained sidecar, no second source pass, no BIR variant, and
no grammar directive.

Why this is not REDRESS 64 replay: REDRESS 64 batched four contiguous Unicode
escape units and failed on `y_string_unicode`, whose strings are short and
boundary-heavy. This candidate targets every individual `\uXXXX` unit in the
existing slow path, so it should help both dense long escapes
(`unicode_escapes`) and short-run Unicode (`y_string_unicode`) if the diagnosis
is correct.

Falsification gate:

- `unicode_escapes` retained Track 1 improves by >= 15%.
- `y_string_unicode` retained Track 1 improves by >= 8%.
- `twitter`, `github_events`, `random`, `distinct_values`, `numbers`, and
  `canada` regress by no more than 2% under native `profile-lazy` smoke.
- Native Criterion retained rows then confirm no guard row regresses by more
  than 5%.
- A parse-attribution build must expose either a new symbol boundary or a
  reduced `match_string_at_quote` attributed c/B; if symbol attribution remains
  in the wrapper, row Mbps/cB is the deciding signal.

Next evidence needed before code: current post-cap-16 `samply` captures for
`distinct_values`, `y_string_unicode`, `github_events`, `update_center`, and
`random` using the B3 `runtime/parse-attribution` build shape. The current
write-only constraint prevented creating those artifacts in this pass.
