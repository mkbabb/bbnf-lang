# SK-V13 P1-F: RESULTS Extraction + Delta

Pass: S-P1 Profile. Cycle: V13 / S-P1 V2 fold.
Date: 2026-05-21.
Scope: Extract current `skinny/RESULTS.md`, reconcile V2 profile rows, and record delta/progress signals against SK-V12 close.
Output: this file.
Baseline: SK-V13-open (`7ee299096be7d7fdaa0e69344a6cd18bbd55524f`; no behavior-source delta from SK-V13 S-P1 V1).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS="-C target-cpu=native"` for V2 profile captures.
Profile tool: `skinny/RESULTS.md`, `/tmp/skv13-p1/pmu/pmu_rows.tsv`, `/tmp/skv13-p1-v2/samply/*`, `/tmp/skv13-p1-v2/mode3/*`, CSS profile log.
Corpus coverage: JSON 51/51 conceptual rows; measured parse 17/17, direct 17/17, typed 7/17, mode III 17/17 x 5; CSS declaration-values 1/1.

V3 fold note: row status, primitive status, missing typed rows, CSS
method-mismatch, and unavailable counters are canonicalized in
`support/evidence-ledger-v3.md`. Every classification below remains
`profile_signal_not_gate_admission`.

## §1 - Method

Inputs:

```bash
sed -n '1,260p' skinny/RESULTS.md
sed -n '1,260p' skinny/REDRESS.md
sed -n '1,280p' restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md
sed -n '1,260p' restart/skinny/tranches/sk-v13/HANDOFF.md
sed -n '1,240p' restart/skinny/tranches/sk-v13/SYNTHESIS.md
sed -n '1,240p' restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md
cat /tmp/skv13-p1-v2/artifacts/identity.txt
sed -n '1,20p' /tmp/skv13-p1-v2/samply/direct_capture_status.tsv
sed -n '1,20p' /tmp/skv13-p1-v2/mode3/mode3_rows.tsv
cat /tmp/skv13-p1-v2/css/logs/css_l4_declaration_values_all_modes.log
```

Classification in this P1-F artefact is `profile_signal_not_gate_admission`.
Only later gate-json/REDRESS waves can admit or demote rows. P1-F records
which rows are measured, missing, stale, or method-mismatched.

## §2 - Findings

Checked `skinny/RESULTS.md` still represents the SK-V12 close surface:
17 `parse_only`, 17 `direct_to_struct`, 7 `real_typed_struct`, and the admitted
CSS L4 declaration-values row. The SK-V13 addendum expands the target to all
51 JSON rows plus full CSS L4 lightningcss parity.

Fresh V2 profile-state ledger:

| Surface | Required by addendum | Fresh profile coverage | Current extraction state |
|---|---:|---:|---|
| JSON parse_only | 17 rows | 17/17 V1 parse profiles + PMU | measured, still uses stale sonic parse comparator from `RESULTS.md` |
| JSON direct_to_struct | 17 rows | 17/17 V2 direct profiles + logs | measured, symbol-attributed, non-panic |
| JSON real_typed_struct | 17 rows | 7/17 V1 typed profiles + PMU | 10 rows missing product surface |
| JSON mode III probes | 17 x 5 rows | 85/85 V2 profiles + counters | measured masking/structural evidence |
| CSS declaration-values | 1 admitted row | V2 equality + throughput + samply profile | measured, but absolute Mbps differs from SK-V12 Criterion close |
| CSS remaining parity matrix | 23 rows | not yet measured by S-P1 | open SK-V13 S-P2/S-P3 scope |

Direct row progress signal from V2:

| Row | V2 Track 1 Mbps | V2 Track 2 Mbps | V2 Track 1 c/B | Rank-1 hot leaf | Profile signal |
|---|---:|---:|---:|---|---|
| twitter | 11821 | 10842 | 2.969 | `parse_object_value_at_direct` | measured direct |
| citm_catalog | 21969 | 20806 | 1.605 | `parse_array_element_at_direct` | measured direct |
| canada | 10547 | 10148 | 3.262 | `parse_array_element_at_direct` | measured direct |
| apache_builds | 11071 | 10129 | 3.081 | `parse_object_value_at_direct` | measured direct |
| github_events | 11886 | 11062 | 2.839 | `parse_object_value_at_direct` | measured direct |
| update_center | 8206 | 7334 | 4.140 | `parse_object_value_at_direct` | measured direct |
| mesh | 8787 | 8063 | 3.865 | `parse_array_element_at_direct` | measured direct |
| random | 7661 | 6840 | 4.425 | `parse_object_value_at_direct` | measured direct |
| gsoc-2018 | 14523 | 13955 | 2.337 | `parse_object_value_at_direct` | measured direct |
| marine_ik | 9241 | 9225 | 3.673 | `parse_array_element_at_direct` | measured direct |
| instruments | 11738 | 10895 | 2.882 | `Option<&u8>::copied` | measured direct |
| numbers | 12216 | 11950 | 2.777 | `parse_array_element_at_direct` | measured direct |
| unicode_mixed | 4423 | 4284 | 7.667 | `parse_object_value_at_direct` | measured direct |
| unicode_escapes | 4772 | 4260 | 7.074 | `unescape_string` | measured direct primitive |
| unicode_basic | 8858 | 8043 | 3.817 | `parse_object_value_at_direct` | measured direct |
| distinct_values | 6097 | 5459 | 5.559 | `parse_array_element_at_direct` | measured direct |
| y_string_unicode | 3101 | 2976 | 10.942 | `parse_array_element_at_direct` | measured direct; timer noise in Track 2 |

CSS declaration-values V2 signal:

| Metric | V2 value | SK-V12 close value | Extraction note |
|---|---:|---:|---|
| Track 1 | 48.319656 Mbps | 429.34 Mbps | different harness/iteration method; not an automatic demotion |
| cssparser | 25.042278 Mbps | 217.43 Mbps | same order-of-magnitude drop as Track 1 |
| lightningcss | 19.076229 Mbps | 168.93 Mbps | same order-of-magnitude drop as Track 1 |
| strict equality | pass | pass | semantic equality preserved |
| top profile leaf | `mach_absolute_time` 17.6% | n/a | timer/fact-sink dominated profile |

## §3 - Delta vs SK-V12

SK-V12 close publishes precise CSS declaration-values numbers and JSON
disposition counts, but not a per-JSON-row profile ledger. Therefore:

- JSON profile deltas vs SK-V12 are `n/a`; current row state is derived from
  `skinny/RESULTS.md` plus V2 profile evidence.
- Direct V2 resolves the S-P1 V1 direct-profile defect, so the intra-pass delta
  is `panic-path -> measured`.
- CSS V2 absolute throughput is not comparable to SK-V12 close because the V2
  harness measures a different local profile loop. Equality remains pass and
  all three comparators moved down together.

Current addendum-facing row inventory:

| Category | Count | State |
|---|---:|---|
| JSON rows required | 51 | 41 checked in `RESULTS.md`; 10 typed rows missing |
| JSON rows with any current profile | 41 | parse 17, direct 17, typed 7 |
| JSON rows with direct V2 hot leaf | 17 | all non-panic |
| JSON rows with mode-III masking evidence | 17 | all five captured probes |
| CSS rows admitted from SK-V12 | 1 | declaration-values equality retained |
| CSS rows still unmeasured for parity expansion | 23 | S-P2/S-P3 scope, not S-P1 admission |

## §4 - Anomalies + Masking Signals

- `simdjson`, `yyjson`, `asmjson`, and RapidJSON sidecars remain absent or
  `n/a` on the checked result surface. This is a comparator freshness gap, not
  a proof of superiority.
- Parse-only is repinned by the addendum, but S-P1 still lacks same-run sonic
  parse PMU. Parse rows need a later gate-consumed strict comparator rerun.
- Ten typed rows are absent from the generated typed surface; they cannot be
  counted as profiled or admitted.
- CSS V2 throughput is method-mismatched against SK-V12 close; the correct use
  is hot-leaf/equality signal, not demotion.
- Every profile classification in this document is explicitly
  `profile_signal_not_gate_admission`.
- REDRESS 119/120 are history under the user pin, but V2 direct profile signals
  are not direct-row reopens or admissions. A future reopen must cite the prior
  fixpoint, name a material differential, and use same-harness strict
  comparator evidence.
- Pre-pin rejected route families and REDRESS-126 zero-orphan accounting remain
  in force; V2 profile signals do not create implementation authority.

## §5 - Sources

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `/tmp/skv13-p1/artifacts/identity.txt`
- `/tmp/skv13-p1/pmu/pmu_rows.tsv`
- `/tmp/skv13-p1-v2/artifacts/identity.txt`
- `/tmp/skv13-p1-v2/samply/direct_capture_status.tsv`
- `/tmp/skv13-p1-v2/samply/logs/direct__*.log`
- `/tmp/skv13-p1-v2/mode3/mode3_rows.tsv`
- `/tmp/skv13-p1-v2/css/logs/css_l4_declaration_values_all_modes.log`
- `/tmp/skv13-p1-v2/summary/direct_summary.tsv`
- `/tmp/skv13-p1-v2/summary/mode3_summary.tsv`
- `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md`
