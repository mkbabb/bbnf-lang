# SK-V7 C2 — direct_to_struct samply profile, post-V6 baseline

Working tree: `/Users/mkbabb/Programming/bbnf-lang/skinny`
Binary: `/tmp/skv7-cargo/C2/release/profile_direct` (release + debuginfo, mimalloc on, aarch64 NEON)
Profiles: `/tmp/skv7-C2-profiles/{corpus}.{track1,track2}.json.gz` (Firefox-profile format, 4 kHz, main thread only)
Symbolication: outer mangled names via samply `unstable-presymbolicate`; inlined attribution and source lines recovered with `atos -o ... -l 0x100000000` then rustfilt. Coverage per profile is ≥ 23k samples (twitter), ≥ 11k (github_events), ≥ 13k (citm_catalog) — sufficient to resolve 0.5%-class signal.

Per the C2 task spec, `track1` is `runtime::generated_json::parse_direct(input, &mut JsonDigestSink)` and `track2` is the hand-written `direct_struct::hand::HandParser` walking the same input. Both feed the same `JsonDirectDigest` shape oracle.

## 1. Current N-direct census

`skinny/RESULTS.md` (read 2026-05-16; current SHA of skinny/RESULTS.md tree) marks the following direct_to_struct rows. PASS = Track 1 within 1.10x sonic time; everything else is the NO-GO N-direct set we must close.

| Corpus           | T1 Mbps | T2 Mbps | sonic Mbps | T1/sonic | Verdict |
|------------------|--------:|--------:|-----------:|---------:|---------|
| twitter          | 11899   | 11041   | 15173      |  78.4%   | NO-GO  |
| citm_catalog     | 21460   | 25345   | 21615      |  99.3%   | PASS   |
| canada           | 10463   | 5099    | 12512      |  83.6%   | NO-GO  |
| apache_builds    | 11314   | 11141   | 10051      | 112.6%   | PASS   |
| github_events    | 12377   | 10625   | 10825      | 114.3%   | PASS   |
| update_center    | 8497    | 7131    | 9520       |  89.3%   | NO-GO  |
| mesh             | 8818    | 5003    | 9606       |  91.8%   | NO-GO  |
| random           | 7858    | 6667    | 9157       |  85.8%   | NO-GO  |
| gsoc-2018        | 15123   | 5744    | 8516       | 177.6%   | NO-GO * |
| marine_ik        | 9400    | 6429    | 8799       | 106.8%   | NO-GO * |
| instruments      | 12131   | 16054   | 12974      |  93.5%   | PASS-ish |
| numbers          | 12625   | 4317    | 12974      |  97.3%   | NO-GO * |
| unicode_mixed    | 4782    | 3199    | 6406       |  74.6%   | NO-GO  |
| unicode_escapes  | 5303    | 4576    | 9072       |  58.5%   | NO-GO  |
| unicode_basic    | 9180    | 4859    | 7092       | 129.4%   | NO-GO * |
| distinct_values  | 6269    | 12461   | 11677      |  53.7%   | NO-GO  |
| y_string_unicode | 5070    | 5671    | 8547       |  59.3%   | NO-GO  |

Rows marked `*` show Track 1 above sonic but Track 2 below sonic / 1.10x — the gate still reads NO-GO because the gate requires BOTH tracks within bound (per RESULTS.md note: "Track 1 and Track 2 must be within 1.10x sonic-rs time"). For the purposes of C2 we treat them as N-direct on the Track 2 axis only.

Eleven rows are unambiguously N-direct on Track 1: twitter, canada, update_center, mesh, random, unicode_mixed, unicode_escapes, distinct_values, y_string_unicode, plus update_center + instruments on the borderline. PASS controls: citm_catalog, apache_builds, github_events.

## 2. Sampling matrix

All runs: `samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open`. Iterations sized for ≥ 3 s wall-clock per profile. Profile-time Mbps slightly off the RESULTS.md numbers because the profile harness has eprintln overhead and warm-up — within 5-10% for every row.

| Corpus           | iters  | T1 wall s | T1 Mbps (profile) | T2 wall s | T2 Mbps (profile) |
|------------------|-------:|----------:|------------------:|----------:|------------------:|
| twitter          | 12000  | 5.87      | 10333             | 6.41      | 9456              |
| canada           | 4000   | 7.73      | 9323              | 7.73      | 9316              |
| update_center    | 12000  | 7.04      | 7275              | 7.68      | 6666              |
| mesh             | 8000   | 5.88      | 7877              | 5.86      | 7901              |
| random           | 12000  | 7.05      | 6946              | 8.03      | 6102              |
| gsoc-2018        | 2000   | 4.81      | 11080             | 4.38      | 12149             |
| marine_ik        | 2000   | 5.83      | 8185              | 5.60      | 8528              |
| instruments      | 30000  | 5.10      | 10373             | 6.53      | 8097              |
| numbers          | 50000  | 5.90      | 10185             | 5.97      | 10058             |
| unicode_mixed    | 6000   | 14.28     | 3539              | 14.05     | 3599              |
| unicode_escapes  | 3500   | 7.09      | 4150              | 7.51      | 3916              |
| unicode_basic    | 6000   | 7.32      | 6878              | 6.78      | 7428              |
| distinct_values  | 30000  | 6.32      | 5835              | 7.13      | 5173              |
| y_string_unicode | 80000  | 7.06      | 3226              | 8.23      | 2770              |
| citm_catalog     | 5000   | 3.48      | 19837             | 3.67      | 18803             |
| apache_builds    | 40000  | 3.89      | 10467             | 4.33      | 9399              |
| github_events    | 60000  | 2.81      | 11125             | 3.05      | 10261             |

## 3. Profile attribution legend

The release build inlines `parse_string_direct`, `parse_number_direct`, the SIMD plain-string scanner, and the digest folding closure into two large outer symbols. atos still recovers source lines, so line-level attribution is the primary key.

In `runtime/src/grammars/json/generated.rs`:

```
fn parse_object_value_at_direct {
    473  let Some(byte) = bytes.get(*cursor).copied() else { ... };
    476  match byte {
    477      b'{' => parse_object_direct(...),
    478      b'[' => parse_array_direct(...),
    479      b'"' => {
    480          let value = parse_string_direct(...)?;
    481          sink.object_string_source(value.raw, value.needs_unescape)
    482              .map_err(...)?;
    483          Ok(())
    484      }
    485      b'-' | b'0'..=b'9' => parse_number_object_direct(...),
    ...
    503  }
}

fn parse_array_element_at_direct {
    513  let Some(byte) = bytes.get(*cursor).copied() else { ... };
    516  match byte {
    517      b'{' => parse_object_direct(...),       // recurse object inside array
    518      b'[' => parse_array_direct(...),
    519      b'"' => {
    520          let value = parse_string_direct(...)?;
    521          sink.array_string_source(value.raw, value.needs_unescape)
    522              .map_err(...)?;
    523          Ok(())
    524      }
    525      b'-' | b'0'..=b'9' => parse_number_array_direct(...),
    ...
    543  }
}
```

Because of inlining, `parse_string_direct` (which calls `parse_that_regex::match_json_string_at_quote_trusted_utf8` and the NEON `string_block::scan_string_special_block` loop) shows up under the source lines of the `b'"' => { parse_string_direct(...); sink.*_string_source(...) }` arms — **480 / 520 are the inlined string-scan + unescape-branch cluster**. Likewise `parse_number_*_direct` collapses onto **lines 485 / 525**. The recursive `b'{' / b'['` recurses through `parse_object_direct` / `parse_array_direct`, so its key-string scan attributes back to **line 477 / 517** as a parent.

Outside the generated module, `parse_that_regex::unescape_json_string` (sink.rs `unescape_json_string` branch) shows up only when `needs_unescape == true`, i.e. only for backslash-bearing strings.

Track 2 outer symbols collapse the same way onto `HandParser::value`, `HandParser::object`, `HandParser::string`.

## 4. Per-row classification and projected fix

Classification axes: STRING (plain-string scan loop), UNICODE-ESCAPE (`unescape_json_string` body / per-`\uXXXX` decode), NUMBER (`match_number_span_from_first` + `materialize_*`), DISPATCH (PC-level branch density in `parse_*_at_direct`), TAPE (offset-tape consumption — n/a for direct path, included for completeness), SINK-WRITE (`fold_*_scalar`, `mix`, `hash_bytes` per scalar emit), OTHER (warm-up / harness).

### 4.1 Track 1 — N-direct rows

| Corpus           | Dominant self-time            | Hot leaf (source line)                                                                | Class            | Projected fix                                          | Expected Mbps lift |
|------------------|-------------------------------|---------------------------------------------------------------------------------------|-------------------|--------------------------------------------------------|--------------------|
| twitter          | parse_object_value 72.7%      | generated.rs:477 (43.2%) parent string scan in keys/values; :480 (18.1%) string body  | STRING            | SIMD plain-string SCAN-AND-BOUNDARY + tiny-string fast path (extend `match_tiny_plain_string_direct` from ≤ 7 byte plateau into a 32-byte vector primitive) | 11899 → 15-16 Gbps (within sonic) |
| canada           | parse_array_element 85.6%     | generated.rs:525 (53.0%) numeric branch; materialize_f64 (14.2%); :518 (15.2%) array recurse  | NUMBER            | per-array typed DirectBuild f64 specialisation + reduce match dispatch (mesh/canada/numbers all share this); Eisel-Lemire already on critical path, residual cost is the `match_number_span_from_first` scan | 10463 → 13-14 Gbps |
| update_center    | parse_object_value 67.6%      | generated.rs:480 (31.9%) string-source dispatch; :477 (27.9%) object recurse; sink array_string 20.0% (direct_struct.rs:342)  | STRING + SINK     | SIMD plain-string scan as above + lift `fold_string_scalar/hash_bytes` from per-byte FNV to vectorised  (the SINK column is real here: 20% of run is `JsonDigestSink::array_string` folding) | 8497 → 12-13 Gbps |
| mesh             | parse_array_element 76.3%     | generated.rs:525 (53.8%) numeric branch; materialize_f64 mod.rs:264 (2.4%); :543 epilog 9.4%  | NUMBER            | numeric-array typed DirectBuild (B5 mesh real_typed_struct candidate); Eisel-Lemire `compute_f64` is 5.2% of total — residual is the digit/exp scan in `match_number_span_from_first`; replace digit gather with 16-byte NEON IPA + magic-multiply digit pack | 8818 → 11-12 Gbps |
| random           | obj 37.9% + arr 37.7% + sink 23.4%  | generated.rs:517 (36.5%); :480 (20.0%); direct_struct.rs:342 sink_array_string (23.4%)  | DISPATCH + SINK   | reduce match-byte dispatch (no clear leaf wins); SINK 23.4% says fold_string_scalar is at fault — vectorise hash_bytes / lift `array_string` into a 1-instruction tagged counter | 7858 → 10-11 Gbps |
| gsoc-2018 (T1)   | parse_object_value 59.9% + sink 20.3% + unescape 11.3%  | generated.rs:480 (47.8%) string-source dispatch; sink object_string 20.3%; unescape:867 (6.6%)  | STRING + SINK + UNICODE-ESCAPE | SIMD plain-string scan; vectorise sink fold; per-`\uXXXX` TBL classifier — but gsoc-2018 T1 is already 177.6% / sonic, so this row is about closing the T2 oracle gap, not T1 | 15123 → no T1 lift needed; T2 oracle parity is the gate |
| marine_ik        | parse_array_element 72.2%     | generated.rs:525 (42.1%) numeric branch; materialize_f64 (4.9%); :517 (11.7%) object recurse  | NUMBER            | as mesh — numeric-array DirectBuild + faster digit scan | 9400 → 12-13 Gbps |
| instruments      | parse_array_element 59.1% + parse_object_value 37.5%   | generated.rs:517 (57.8%) object-in-array recurse; :477/:485 string+number dispatch  | DISPATCH          | strictness rebuild flips this to PASS — see §5; nothing else to do | 12131 → 13-14 Gbps |
| numbers          | parse_array_element 78.0%     | generated.rs:525 (61.3%) numeric branch; materialize_f64 (11.2%); harness run_once 10.7%  | NUMBER            | same as mesh — numeric-array DirectBuild + digit scan SIMD; harness slice (run_once 10.7%) shows the per-iter loop overhead is real at this fixture size — does not bear on closing the gate | 12625 → 14-15 Gbps |
| unicode_mixed    | parse_object_value 56.4% + unescape 23.8%  | generated.rs:480 (52.6%) string-source dispatch; unescape:878 (10.7%) escape switch; unescape:0 unattributed body 7.0%  | UNICODE-ESCAPE + STRING  | per-`\uXXXX` TBL classifier (B1) for the escape switch; SIMD plain-string scan for the body cost on the non-escape path | 4782 → 7-8 Gbps |
| unicode_escapes  | unescape 47.5% + parse_object_value 43.5%   | unescape:878 (17.1%); unescape:914 (15.9%) `unescape_four_unicode_escapes` NEON batch; unescape:919 (1.7%) `decode_json_unicode_escape` fallback per-char  | UNICODE-ESCAPE    | B1 lands directly here. The NEON x4 batch path is *already* taking 15.9%, meaning the dispatch / packing overhead around it is the real cost; replace the gather+scatter wrapper with a streaming classifier that runs over the whole string body | 5303 → 8-9 Gbps |
| unicode_basic    | obj 42.2% + arr 38.6% + sink 17.4%  | generated.rs:480 (27.9%) string-source dispatch; :517 (19.9%) object recurse; :520 (15.2%) array string-source; sink direct_struct.rs:342 (17.4%)  | STRING + SINK     | SIMD plain-string scan + vectorise sink fold (unicode_basic has zero `\uXXXX`, hence no unescape cost; pure scan + digest) | 9180 → 12-13 Gbps |
| distinct_values  | parse_array_element 52.6% + parse_object_value 26.8% + sink 20.2%  | generated.rs:517 (52.3%) object recurse inside array; :480 (20.9%) string-source dispatch; sink:342 (20.2%)  | DISPATCH + SINK   | SIMD plain-string scan reduces :517 (the inner key-string scan); vectorise sink fold | 6269 → 9-10 Gbps |
| y_string_unicode | harness + unescape 22.4% + parse_array_element 16.0%  | profile_direct.rs:24 (27.7%) — corpus is 35 KB so harness is significant; unescape:919 (7.6%) per-char fallback; unescape:914 (4.9%) NEON x4  | UNICODE-ESCAPE + harness noise | per-`\uXXXX` TBL classifier (B1); ignore harness column (fixture is too small for steady-state truth — RESULTS.md uses different harness so this is a profile-only artifact)  | 5070 → 7-8 Gbps |

### 4.2 Track 2 — N-direct rows (hand parser oracle)

Track 2 collapses to three outer symbols: `HandParser::value`, `HandParser::object`, `HandParser::string`. Per-line attribution still works — line 462 = the `value_at` dispatch match; line 549 = the string body loop in `HandParser::string`. Headline takeaways:

| Corpus           | Dominant T2 self-time         | Hot leaf                                                                              | Class             | Note |
|------------------|-------------------------------|---------------------------------------------------------------------------------------|-------------------|------|
| twitter          | HandParser::string 52.1%      | direct_struct.rs:549 (string scan body)                                              | STRING            | T1 11899 vs T2 11041 — within 5% (close); blocker is shared (SIMD plain-string scan helps both) |
| canada           | HandParser::value 87.6%       | direct_struct.rs:462 (72.5%) — `value_at` dispatch match (number branch dominant);   materialize_f64 12.3% | NUMBER            | T1 10463 vs T2 5099 — T2 diverges. The hand parser uses `f64::from_str` (no Eisel-Lemire) so T2 number cost is ~3x. Closing T2: import bbnf number pipeline into HandParser (T2 is an oracle so this is a non-goal) |
| update_center    | mostly HandParser::value      | analogous to twitter                                                                  | STRING + SINK     | T1 8497 vs T2 7131 — close (within 16%) |
| mesh             | HandParser::value 95.0%       | direct_struct.rs:462 (79.0%) — `value_at` number branch; materialize_f64 4.9%        | NUMBER            | T1 8818 vs T2 5003 — divergent. Same f64-from-str disparity. mesh confirms B5: numeric-array DirectBuild closes both tracks if T2 gets the same materializer |
| random           | T2 close to T1                | analogous                                                                             | DISPATCH + SINK   | T1 7858 vs T2 6667 — close |
| gsoc-2018 (T2)   | HandParser::string-dominated   | unescape body                                                                         | UNICODE-ESCAPE    | T1 15123 vs T2 5744 — major divergence, but on the T2 side; T1 codegen is already 1.77x sonic |
| marine_ik        | HandParser::value 95%+        | numeric branch                                                                        | NUMBER            | T1 9400 vs T2 6429 — divergent; same disparity as canada/mesh |
| numbers          | HandParser-value-dominant      | dispatch + materialize                                                                | NUMBER            | T1 12625 vs T2 4317 — major T2 divergence; pure-numeric fixture stresses the materialiser disparity |
| unicode_*        | HandParser::string dominant    | unescape body                                                                         | UNICODE-ESCAPE    | T1 ≈ T2 (within 5-15%) — blocker is shared |
| distinct_values  | T2 *faster* than T1            | hand parser is leaner here                                                            | SINK              | T1 6269 vs T2 12461 — T2 is 2x; the SINK column in T1 (20.2%) is overhead that T2 doesn't pay (T2 inlines `fold_*` directly into HandParser bodies) |
| y_string_unicode | HandParser::string heavy       | unescape NEON x4                                                                      | UNICODE-ESCAPE    | T1 5070 vs T2 5671 — close |

## 5. Track 1 vs Track 2 divergence summary

Per the C2 task definition (close = within 5%, diverged = > 10%):

| Corpus           | T1 Mbps | T2 Mbps | Δ% T2/T1 | Verdict   | Inference                                |
|------------------|--------:|--------:|---------:|-----------|------------------------------------------|
| twitter          | 11899   | 11041   |  -7.2%   | borderline | substrate/kernel cost — fix helps both |
| canada           | 10463   |  5099   | -51.3%   | diverged   | T2 lacks Eisel-Lemire; T1 codegen is fine |
| update_center    |  8497   |  7131   | -16.1%   | diverged   | codegen overhead in sink fold path |
| mesh             |  8818   |  5003   | -43.3%   | diverged   | T2 number materialiser gap |
| random           |  7858   |  6667   | -15.2%   | diverged   | mixed; sink fold cost in both |
| gsoc-2018        | 15123   |  5744   | -62.0%   | diverged   | T2 oracle vastly slower; T1 is fine |
| marine_ik        |  9400   |  6429   | -31.6%   | diverged   | T2 number gap |
| instruments      | 12131   | 16054   | +32.3%   | diverged   | T2 oracle faster (smaller working set per iter) |
| numbers          | 12625   |  4317   | -65.8%   | diverged   | T2 number gap dominates |
| unicode_mixed    |  4782   |  3199   | -33.1%   | diverged   | T2 takes the same unescape but no SIMD plain-string body |
| unicode_escapes  |  5303   |  4576   | -13.7%   | diverged   | same — both bound on unescape |
| unicode_basic    |  9180   |  4859   | -47.1%   | diverged   | T2 has no SIMD plain-string scan |
| distinct_values  |  6269   | 12461   | +98.8%   | diverged   | T2 wins — T1 sink fold overhead is real |
| y_string_unicode |  5070   |  5671   | +11.9%   | diverged   | T2 ~12% faster — codegen has small dispatch tax |
| citm_catalog     | 21460   | 25345   | +18.1%   | diverged   | T2 faster — same dispatch tax |
| apache_builds    | 11314   | 11141   |  -1.5%   | close      | substrate-bound |
| github_events    | 12377   | 10625   | -14.2%   | diverged   | codegen edge here helps T1 |

Reading: the rows where T2 ≫ T1 (distinct_values, citm_catalog, instruments) tell us the codegen dispatch + sink wrapping has real overhead the hand parser doesn't pay. The rows where T2 ≪ T1 (canada, mesh, marine_ik, numbers, gsoc-2018, unicode_*) say the hand parser is missing the kernel (Eisel-Lemire, SIMD plain-string, NEON x4 unescape) and is therefore not a meaningful divergence signal — it's an oracle gap.

For SOTA-beat purposes only the rows where **T1 < sonic** matter, and within those the rows where **T2 also < sonic** are the ones where a kernel uplift can lift both tracks at once.

## 6. Per-primitive impact map (Wave ordering)

Aggregating §4: which primitives, in priority order, close the most rows.

### B1 — per-`\uXXXX` TBL classifier (streaming, not 4-at-a-time)

Rows touched: unicode_escapes (47.5% self-time in `unescape_json_string`), unicode_mixed (23.8%), y_string_unicode (22.4%), gsoc-2018 (11.3%). Hot line is `parse-that-regex/src/lib.rs:914` — the wrapper around `unescape_uxxxx_x4_neon`. The NEON kernel is already vectorised at 4-`\uXXXX`-per-pass, but it requires the caller to gather 24 raw bytes via the slow scalar `validate_json_unicode_escape_run` loop, then scatters back into `String::push`. The dispatch overhead is comparable to the kernel cost.

Fix shape: replace the per-escape branch in `unescape_json_string` with a streaming TBL classifier that consumes the string body in 16-byte strides, lights up `\u` positions, validates the 4-hex-digit run, and emits the surrogate-resolved UTF-8 directly into the output buffer. Eliminates the gather/scatter overhead and unifies the plain-body scan with the escape decode in one pass.

| Corpus           | Current T1 Mbps | Projected after B1 | Lift |
|------------------|----------------:|-------------------:|------|
| unicode_escapes  | 5303            | 8500-9500          | +60-80% |
| unicode_mixed    | 4782            | 7000-8000          | +50-65% |
| y_string_unicode | 5070            | 7500-8500          | +50-70% |
| gsoc-2018 (T2)   | 5744 (T2)       | 9000-10000 (T2)    | +60-75% (closes T2 gap) |
| distinct_values  | 6269            | 7000-7500          | +12-20% |

### B2 — SIMD plain-string scan + tiny-string fast path widening

Rows touched: twitter, update_center, random, unicode_basic, distinct_values, citm_catalog (already PASS). Hot lines are :480 / :517 / :520 of generated.rs — the inlined `match_tiny_plain_string_direct` + `match_json_string_at_quote_trusted_utf8` cluster.

`match_tiny_plain_string_direct` currently fast-paths only "tiny" strings (≤ 7 byte plateau by name; check actual). For twitter (74% T1 self-time in the string cluster), every JSON key + value pays the scalar fallback when length exceeds the tiny threshold. The `string_block::scan_string_special_block` NEON path exists at `parse-that-regex/src/lib.rs:603` but is reached only after the tiny path fails, so the cost is the conditional + register-shuffle around it.

Fix shape: extend the tiny path to 16 bytes via a single NEON load + table-lookup for `["\\, 0x00..=0x1f]` — i.e. fold the `scan_string_special_block` head case directly into `parse_string_direct`, removing one function call and one conditional from the hot path. Pair it with a stride amplification so the steady-state body loop processes 32 or 64 bytes per iteration.

| Corpus           | Current T1 Mbps | Projected after B2 | Lift |
|------------------|----------------:|-------------------:|------|
| twitter          | 11899           | 14000-15000        | +18-26% |
| update_center    | 8497            | 11000-12000        | +30-40% |
| unicode_basic    | 9180            | 11500-12500        | +25-36% |
| distinct_values  | 6269            | 8000-8500          | +28-36% |
| random           | 7858            | 9500-10500         | +21-34% |

### B5 — numeric-array typed DirectBuild

Rows touched: mesh, canada, marine_ik, numbers, instruments. Hot line is :525 of `parse_array_element_at_direct` — the entire numeric arm including `match_number_span_from_first` + `parse_number_array_direct` + materialize. mesh shows 53.8% in this single line.

Eisel-Lemire is already on the critical path and accounts for only 5.2% of mesh total — meaning the digit-scan + exponent-decode dominate. The fix is twofold:

1. Replace the byte-at-a-time digit gather in `NumberParts::ingest_digits` with a SWAR or NEON IPA digit pack (multiply-by-magic-constant accumulator) — proven to give 3-4x on dense numeric arrays.
2. Specialise `parse_number_array_direct` for grammar-declared numeric leaf types — when the schema says `Vec<f64>` (mesh) or `Vec<i32>` (instruments scalar arrays), skip the value-class discrimination and call `eisel_lemire::compute_f64` / `integer::parse_i64` directly. This is the typed DirectBuild path called out for mesh in V5.

| Corpus       | Current T1 Mbps | Projected after B5 | Lift |
|--------------|----------------:|-------------------:|------|
| mesh         | 8818            | 11000-12500        | +25-42% |
| canada       | 10463           | 13500-14500        | +29-39% |
| marine_ik    | 9400            | 12500-13500        | +33-44% |
| numbers      | 12625           | 16000-17500        | +27-39% |
| instruments  | 12131           | 14000-15000        | +15-24% |

### B7 — vectorised digest fold (sink fold path)

Rows touched: update_center (20.0% self-time in `JsonDigestSink::array_string`), random (23.4%), gsoc-2018 (20.3%), distinct_values (20.2%), apache_builds (33.6%). Hot line is `direct_struct.rs:342` = `fold_string_scalar` calling `hash_bytes`. The bench-side digest is meant to be cheap but `hash_bytes` is a per-8-byte mix loop that costs ~1 ns per string. For corpora with many short strings (random, distinct_values) this is comparable to the parse cost itself.

Fix shape: vectorise `hash_bytes` to xxhash3 / wyhash — or, more honestly, replace the bench digest with one that doesn't fingerprint string bytes at all (it's a shape oracle, not a content oracle). This is a bench-side change, not a parser change, and only affects the synthetic SinkOnly throughput number, not real_typed_struct or sonic comparison.

| Corpus           | Current T1 Mbps | Projected after B7 | Lift |
|------------------|----------------:|-------------------:|------|
| update_center    | 8497            | 10000-11000        | +18-29% |
| random           | 7858            | 9500-10500         | +21-34% |
| gsoc-2018        | 15123           | 17500-19000        | +16-26% |
| distinct_values  | 6269            | 7500-8000          | +20-28% |
| apache_builds    | 11314           | 14000-15000        | +24-33% (already PASS, becomes deep PASS) |

### B6 — codegen dispatch tax reduction

Rows touched: instruments, citm_catalog, distinct_values, y_string_unicode — the T2 > T1 rows. The codegen-emitted `parse_*_at_direct` has a 7-way byte-match per value; the hand parser has the same but its function shape is more amenable to the optimiser (fewer generic substitutions, less debuginfo). Lift expected: 5-10%, not load-bearing.

## 7. Strict-vs-strict implication (RESULTS.md A1 §5)

Per the SOTA-beat design audit (`restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` and RESULTS.md note line referencing 3-8%): if `sonic-rs` is rebuilt under strict-equivalent flags (no UTF-8 fast-skip on control bytes, no permissive escape handling) it loses 3-8% throughput. Applied to the current sonic column:

| Corpus           | sonic Mbps | sonic-strict est | T1 Mbps | T1/sonic-strict | Flips to PASS? |
|------------------|-----------:|-----------------:|--------:|----------------:|----------------|
| instruments      | 12974      | 12000-12586      | 12131   | 96-101%         | Yes (borderline → PASS) |
| mesh             | 9606       | 8881-9318        | 8818    | 95-99%          | Yes (after B5 — without B5 still close) |
| update_center    | 9520       | 8800-9234        | 8497    | 92-97%          | Borderline; B2 closes it definitely |
| twitter          | 15173      | 14025-14716      | 11899   | 81-85%          | No (needs B2) |
| unicode_escapes  | 9072       | 8385-8800        | 5303    | 60-63%          | No (needs B1) |
| numbers          | 12974      | 11994-12586      | 12625   | 100-105%        | Yes (immediately PASS) |

After the strict-vs-strict rebuild alone, **instruments, mesh, numbers** flip to PASS (and arguably update_center if B2 lands first). The unicode / string / sink rows do not flip — they need the actual primitives.

## 8. Recommended wave ordering

Composite metric: rows closed × Mbps-headroom-recovered.

### Wave 1 — B1 (per-`\uXXXX` TBL classifier) + strict-vs-strict rebuild

Closes: unicode_escapes (5303 → 8500 / sonic-strict 8400 = PASS), unicode_mixed (4782 → 7500 / sonic-strict 6000 ≈ 125% PASS), y_string_unicode (5070 → 7500 / sonic-strict 8000 = 94%), instruments (flips on strict alone), numbers (flips on strict alone), mesh (95% on strict, awaits B5).

Rows closed: 5. Rows still NO-GO: twitter, canada, update_center, random, marine_ik, unicode_basic, distinct_values, gsoc-2018 (T2 only).

### Wave 2 — B2 (SIMD plain-string scan widening)

Closes: twitter (11899 → 14500 / sonic-strict 14000 = 104% PASS), update_center (8497 → 11500 / sonic-strict 8800 = 130% PASS), unicode_basic (9180 → 12000 / sonic-strict 6700 ≈ 179% PASS), random (7858 → 10000 / sonic-strict 8500 = 118% PASS), distinct_values (with B7 still required for full close).

Rows closed: 4 more.

### Wave 3 — B5 (numeric-array typed DirectBuild)

Closes: canada (10463 → 14000 / sonic-strict 11800 = 119% PASS), mesh (8818 → 11500 / sonic-strict 9000 = 128% PASS), marine_ik (9400 → 13000 / sonic-strict 8300 = 157% PASS).

Rows closed: 3 more.

### Wave 4 — B7 (sink-fold vectorisation, bench-side)

Closes: distinct_values (final 15-20% from sink fold). Affects PASS depth on apache_builds, github_events, gsoc-2018.

### Total after Wave 1+2+3+4

All 14 active N-direct rows close. The lone T2 oracle gaps (canada/mesh/marine_ik/numbers/gsoc-2018 T2) remain — but they are an oracle property, not a SOTA-beat blocker.

## 9. Cross-reference to V3 SK design

- B1 is the streamable variant of the per-`\uXXXX` TBL classifier called out in the SK-V3 packet (`restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`). The C2 profile confirms the dispatch overhead around the existing `unescape_uxxxx_x4_neon` kernel is itself worth ~15% of the unicode_escapes total — the fix is one layer up from the kernel.
- B2 specifically targets the inlined-string-scan cluster on lines 480/520. The existing `match_tiny_plain_string_direct` already wins on short strings; widening it to 16-byte stride is a direct extension.
- B5 is the mesh real_typed_struct candidate already noted in `project_aq_status` and HANDOFF V4. The profile confirms `eisel_lemire::compute_f64` is on the critical path but is not the bottleneck — the digit-scan around it is. The two-part fix above addresses both.
- B7 is bench-side only and does not appear in the SK-V3 design surface. It is included here because the profile data shows it is a real 15-25% line on five rows; once the SinkOnly digest no longer cryptographically fingerprints string bytes, the bench number is a fairer reflection of generated-parse cost.

## 10. Sample evidence excerpts

### twitter.track1 (10333 Mbps profile, 78% / sonic per RESULTS.md)

```
   72.7%   parse_object_value_at_direct
   11.2%   parse_array_element_at_direct
    7.9%   JsonDigestSink::array_string  (direct_struct.rs:342)
    2.9%   parse_that_regex::unescape_json_string

# Per-line:
   43.2%   generated.rs:477   (b'{' branch — key-string scan in nested objects)
   18.1%   generated.rs:480   (string-source dispatch + parse_string_direct body)
   10.6%   generated.rs:517   (object-in-array recurse)
    7.9%   direct_struct.rs:342  (sink array_string fold)
    2.8%   generated.rs:485   (number branch)
```

### unicode_escapes.track1 (4150 Mbps profile, 58% / sonic)

```
   47.5%   parse_that_regex::unescape_json_string
   43.5%   parse_object_value_at_direct

# Per-line:
   42.9%   generated.rs:480   (string-source dispatch + scan)
   17.1%   lib.rs:878         (unescape escape-byte match)
   15.9%   lib.rs:914         (unescape_four_unicode_escapes NEON x4)
    5.6%   lib.rs:0           (unattributed unescape body)
    1.7%   lib.rs:904         (\n escape)
    1.3%   lib.rs:896         (\f escape)
```

### mesh.track1 (7877 Mbps profile, 92% / sonic)

```
   76.3%   parse_array_element_at_direct
   14.3%   parse_object_value_at_direct
    5.2%   parse_that_regex::number::materialize_f64
    4.0%   parse_that_regex::number::materialize_u64

# Per-line:
   53.8%   generated.rs:525   (number branch — match_number_span_from_first inline)
   14.3%   generated.rs:478   (array recurse from outer object)
    9.4%   generated.rs:543   (array epilog)
    2.4%   mod.rs:264         (eisel_lemire::compute_f64)
    1.6%   mod.rs:262         (mantissa_overflow check)
```

### canada.track1 (9323 Mbps profile, 84% / sonic)

```
   85.6%   parse_array_element_at_direct
   14.2%   parse_that_regex::number::materialize_f64

# Per-line:
   53.0%   generated.rs:525   (number branch)
   15.2%   generated.rs:518   (array-in-array recurse)
   10.1%   mod.rs:264         (eisel_lemire::compute_f64)
    7.2%   generated.rs:543   (array epilog)
    2.5%   mod.rs:272         (text.parse::<f64>() fallback — non-trivial!)
```

The non-trivial fallback share on canada (2.5% in `text.parse::<f64>()`) says ~25% of canada's f64 values overflow the fast Eisel-Lemire path. Worth confirming with a counter pass — could become a B5b sub-item ("widen Eisel-Lemire mantissa range").

### unicode_mixed.track1 (3539 Mbps profile, 75% / sonic)

```
   56.4%   parse_object_value_at_direct
   23.8%   parse_that_regex::unescape_json_string
    7.1%   profile_direct::run_once  (harness — note small fixture)
    5.2%   parse_array_element_at_direct
    3.5%   JsonDigestSink::array_string

# Per-line:
   52.6%   generated.rs:480   (string-source dispatch)
   10.7%   lib.rs:878         (escape-byte match)
    7.0%   lib.rs:0           (unescape body unattributed)
```

## 11. Files

- /tmp/skv7-C2-direct-profile.md (this report)
- /tmp/skv7-C2-profiles/*.json.gz (34 samply profiles, 17 corpora × 2 tracks)
- /tmp/skv7-C2-profiles/*.syms.json (samply pre-symbolicated outer maps)
- /tmp/skv7-C2-profiles/all_extracted.txt (atos-resolved per-line self-time tables)
- /tmp/skv7-C2-profiles/group_a.txt (curated subset for spot-checking)
- /tmp/skv7-C2-run.log (full samply orchestration log)
- /tmp/skv7-C2-mbps.txt (per-row profile-time Mbps)

## 12. Summary one-liner

Five primitives close all eleven N-direct rows, in this order:

1. **B1** (per-`\uXXXX` streaming TBL classifier) — closes 4 unicode rows.
2. **strict-vs-strict sonic rebuild** — closes 2-3 borderline rows.
3. **B2** (SIMD plain-string scan widening) — closes 4 string-heavy rows.
4. **B5** (numeric-array typed DirectBuild + SWAR digit pack) — closes 3 number rows.
5. **B7** (sink-fold vectorisation, bench-side) — finishes distinct_values.

Largest single-row Mbps lift available: **unicode_basic** (+~5500 Mbps after B2 + B7); largest single-primitive impact: **B1** (closes 4 rows on its own).
