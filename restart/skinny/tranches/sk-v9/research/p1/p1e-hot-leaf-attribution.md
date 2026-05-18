# SK-V9 P1-E: Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V9.
Date: 2026-05-18.
Scope: hot-leaf attribution across `parse_only`, `direct_to_struct`,
`real_typed_struct`, and masking probes.
Output: this file.
Baseline: Alpha-closed opening authority at HEAD `b258a406ff7f`; no fresh
`SK-V9-open` W0 telemetry lock exists yet.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` from the
current W0 manifest authority (`skinny/RESULTS.md:46`).
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`
from the current W0 manifest authority (`skinny/RESULTS.md:46`).
Profile tool: no fresh P1-A/P1-B/P1-C samply artifact is available; current
rows expose Criterion slope artifacts only.
Corpus coverage: 17/17 corpora in current `skinny/RESULTS.md`; 38 main rows
and the configured masking-probe surface are covered as authority/gap rows.

## Section 1 - Method

This artifact is a read-only synthesis over the current opening authority. I
read `skinny/RESULTS.md`, `skinny/REDRESS.md`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md`,
`restart/skinny/tranches/sk-v9/HANDOFF.md`,
`restart/prompts/skinny/PASS-1-PROFILE.md`,
`restart/audit/pass-1-substrate/PASS-1.md`, and the source files cited below.

Commands used:

```sh
nl -ba skinny/RESULTS.md
nl -ba skinny/REDRESS.md
nl -ba restart/skinny/tranches/sk-v9/SYNTHESIS.md
nl -ba restart/skinny/tranches/sk-v9/HANDOFF.md
nl -ba restart/prompts/skinny/PASS-1-PROFILE.md
nl -ba restart/audit/pass-1-substrate/PASS-1.md
rg -n "criterion-slope-profile|Masking|host_call|alternate_scalar|cold_first|parse_value_at|match_number_span|match_string|TapeBuilder|parse_direct|JsonSink" skinny/crates
```

No profiling command was run for this file. That is intentional: the SK-V9
handoff says SK-V9 remains pre-dispatch until G-Alpha and no implementation wave
exists (`restart/skinny/tranches/sk-v9/HANDOFF.md:107`), while the task
requires using current HEAD as Alpha-closed opening authority and treating fresh
SK-V9-open profiles as absent until W0 telemetry-lock. The S-P1 prompt requires
P1-E to consume P1-A/P1-B/P1-C output if available, but permits the parallel
case to defer consumption to a challenge-fold cycle
(`restart/prompts/skinny/PASS-1-PROFILE.md:59`).

## Section 2 - Authority And Gap Rules

The opening benchmark authority is still the W0-rendered `skinny/RESULTS.md`
with 38 `SK-V8-open` manifest rows and overall `N-direct / NoGo`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:25`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:26`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:27`). The current family state is
16 `S / NO-GO` plus 1 `L / NO-GO` for `parse_only`, 3 `A / GO` plus 14
`N-direct / NO-GO` for `direct_to_struct`, and 4 measured `A / GO`
`real_typed_struct` rows (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:33`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:34`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:35`).

Every current main row is `Strictness=deferred`; native Rust comparators are
same-run in W0, while C++ sidecars are historical or absent until a later
same-run sidecar manifest exists (`restart/skinny/tranches/sk-v9/HANDOFF.md:35`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:36`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:37`). SK-V9 telemetry will require
hot leaf and profile artifact fields, but that schema is a future gate binding,
not proof that samply percentages exist now
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:278`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:280`).

The current gate formats hot leaves as Criterion slope artifact bindings:
`criterion-slope-profile:json_<corpus>/<bench>/new/estimates.json;hot-leaf=criterion-slope-profile;row=...`
(`skinny/crates/bbnf-bench/src/bin/gate.rs:616`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:620`). The report validator rejects
placeholder spellings but accepts this artifact binding as the W0 hot-leaf cell
(`skinny/crates/bbnf-bench/src/report.rs:981`,
`skinny/crates/bbnf-bench/src/report.rs:986`,
`skinny/crates/bbnf-bench/src/report.rs:989`). That is not a resolved samply
symbol and contains no self-time percentage. Therefore every `% self-time` cell
below is an explicit gap, not an estimate.

Classification rule: a hot leaf is classified as `scan`, `number`, `string`,
`unicode`, `structural`, `tape`, or `dispatch` only when a resolved symbol or a
single-purpose probe supports that class. Main workload rows currently have no
resolved symbol, so their hot-leaf class is `GAP:not-classified`. Source-backed
eligible classes are recorded separately so S-P2 can see the possible surfaces
without this file claiming a hot leaf.

## Section 3 - Source Surfaces Used For Class Eligibility

Retained `parse_only` rows run `runtime::generated_json::parse`, which builds a
`ParserState`, attaches the generated structural index placeholder, parses, and
finishes a tape-backed `JsonRoot` (`skinny/crates/runtime/src/grammars/json/parser.rs:47`,
`skinny/crates/runtime/src/grammars/json/parser.rs:49`,
`skinny/crates/runtime/src/grammars/json/parser.rs:50`,
`skinny/crates/runtime/src/grammars/json/parser.rs:51`). Source-supported
eligible classes are:

| Class | Source support |
|---|---|
| `dispatch` | `parse_value_at` loads the current byte and routes through `dispatch_value`; `dispatch_value` matches object, array, string, number, literal, and error cases (`skinny/crates/runtime/src/grammars/json/generated.rs:37`, `skinny/crates/runtime/src/grammars/json/generated.rs:42`, `skinny/crates/runtime/src/grammars/json/generated.rs:47`, `skinny/crates/runtime/src/grammars/json/generated.rs:58`). |
| `string` | `parse_string` uses a tiny plain-string fast path and falls back to `match_string_at_quote_trusted_utf8` (`skinny/crates/runtime/src/grammars/json/generated.rs:142`, `skinny/crates/runtime/src/grammars/json/generated.rs:147`, `skinny/crates/runtime/src/grammars/json/generated.rs:151`). |
| `unicode` | string validation routes `\u` escapes through `validate_unicode_escape_run`, with surrogate checks (`skinny/crates/parse-that-regex/src/lib.rs:283`, `skinny/crates/parse-that-regex/src/lib.rs:288`, `skinny/crates/parse-that-regex/src/lib.rs:346`, `skinny/crates/parse-that-regex/src/lib.rs:353`). |
| `number` | `parse_number` calls `match_number_span_from_first`; that scanner handles integer, fraction, exponent, and SWAR digit runs (`skinny/crates/runtime/src/grammars/json/generated.rs:205`, `skinny/crates/runtime/src/grammars/json/generated.rs:216`, `skinny/crates/parse-that-regex/src/number/mod.rs:38`, `skinny/crates/parse-that-regex/src/number/mod.rs:105`). |
| `structural` | object/array delimiters and closes are consumed by structural helpers (`skinny/crates/runtime/src/grammars/json/generated.rs:292`, `skinny/crates/runtime/src/grammars/json/generated.rs:303`, `skinny/crates/runtime/src/grammars/json/generated.rs:333`, `skinny/crates/runtime/src/grammars/json/generated.rs:373`). |
| `tape` | emitted offsets route through `ParserState::emit_plain_offset` into `TapeBuilder::push_plain_offset`, and flags route through `patch_flags` (`skinny/crates/runtime/src/grammars/json/parser.rs:33`, `skinny/crates/runtime/src/grammars/json/parser.rs:41`, `skinny/crates/runtime/src/tape/assembler.rs:61`, `skinny/crates/runtime/src/tape/assembler.rs:94`). |

`direct_to_struct` rows run the generated sink-only parser for Track 1 and an
independent hand parser for Track 2. Track 1 direct dispatches to object, array,
string, number, and literal sink paths (`skinny/crates/runtime/src/grammars/json/generated.rs:409`,
`skinny/crates/runtime/src/grammars/json/generated.rs:427`,
`skinny/crates/runtime/src/grammars/json/generated.rs:437`,
`skinny/crates/runtime/src/grammars/json/generated.rs:464`). Direct strings use
`parse_string_direct` plus `JsonSink::*_source` unescape hooks
(`skinny/crates/runtime/src/grammars/json/generated.rs:610`,
`skinny/crates/runtime/src/grammars/json/generated.rs:624`,
`skinny/crates/runtime/src/grammars/json/sink.rs:16`,
`skinny/crates/runtime/src/grammars/json/sink.rs:28`). Direct numbers materialize
integer or f64 sink calls after `match_number_span_from_first`
(`skinny/crates/runtime/src/grammars/json/generated.rs:645`,
`skinny/crates/runtime/src/grammars/json/generated.rs:652`,
`skinny/crates/runtime/src/grammars/json/generated.rs:690`,
`skinny/crates/runtime/src/grammars/json/generated.rs:712`). The hand Track 2
parser has separate source paths and must not be conflated with Track 1
(`skinny/crates/bbnf-bench/src/direct_struct.rs:408`,
`skinny/crates/bbnf-bench/src/direct_struct.rs:440`,
`skinny/crates/bbnf-bench/src/direct_struct.rs:459`).

`real_typed_struct` rows run generated typed DirectBuild parsers for Track 1 and
serde/sonic typed lanes for oracle and checksum parity. The measured fixtures
are selected by `fixture_for_name`; only `twitter`, `update_center`, `mesh`, and
`marine_ik` have current measured rows in `RESULTS.md`, while Apache/CITM are
source/product parity rows that require fresh row-table admission
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:182`,
`skinny/crates/bbnf-bench/src/real_typed_struct.rs:190`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:216`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:217`). Generated typed parsers enter
through parser-specific functions and then through typed object parsers
(`skinny/crates/bbnf-bench/src/generated_real_typed.rs:31`,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs:42`,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs:75`,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs:97`).

Masking probes are first-class but the current rendered `skinny/RESULTS.md`
contains no `## Masking Probes` section. The report can render such rows
(`skinny/crates/bbnf-bench/src/report.rs:612`), and the gate knows the probe set
(`skinny/crates/bbnf-bench/src/bin/gate.rs:1507`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:1515`). REDRESS records the intended
probe artifact surface for host-call dispatch, eager string decode, alternate
scalar plan, optional PEXT plan, and cold first parse (`skinny/REDRESS.md:163`,
`skinny/REDRESS.md:165`, `skinny/REDRESS.md:166`, `skinny/REDRESS.md:167`).

## Section 4 - Findings: `parse_only`

All `parse_only` rows below have W0 Criterion artifact bindings but no resolved
samply symbol and no self-time percentage. The hot-leaf class is therefore
`GAP:not-classified` for every row. Eligible source classes are the parser
surface classes in Section 3, not an attribution result.

| Corpus | W0 state | Track 1 / Track 2 / sonic Mbps | W0 hot-leaf artifact | `% self-time` | Hot-leaf class | Row evidence |
|---|---|---:|---|---|---|---|
| `twitter` | `S / NO-GO` | 9581 / 9741 / 18176 | `criterion-slope-profile:json_twitter/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:5` |
| `citm_catalog` | `S / NO-GO` | 28644 / 19214 / 21717 | `criterion-slope-profile:json_citm_catalog/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:8` |
| `canada` | `L / NO-GO` | 15497 / 12171 / 8729 | `criterion-slope-profile:json_canada/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:10` |
| `apache_builds` | `S / NO-GO` | 12694 / 11715 / 16904 | `criterion-slope-profile:json_apache_builds/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:12` |
| `github_events` | `S / NO-GO` | 10689 / 10073 / 16408 | `criterion-slope-profile:json_github_events/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:14` |
| `update_center` | `S / NO-GO` | 11926 / 9312 / 18769 | `criterion-slope-profile:json_update_center/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:16` |
| `mesh` | `S / NO-GO` | 9367 / 10000 / 8143 | `criterion-slope-profile:json_mesh/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:19` |
| `random` | `S / NO-GO` | 10011 / 8018 / 15639 | `criterion-slope-profile:json_random/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:22` |
| `gsoc-2018` | `S / NO-GO` | 23209 / 21857 / 49101 | `criterion-slope-profile:json_gsoc-2018/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:24` |
| `marine_ik` | `S / NO-GO` | 13100 / 12164 / 9921 | `criterion-slope-profile:json_marine_ik/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:26` |
| `instruments` | `S / NO-GO` | 13320 / 11351 / 17976 | `criterion-slope-profile:json_instruments/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:29` |
| `numbers` | `S / NO-GO` | 12818 / 13537 / 9854 | `criterion-slope-profile:json_numbers/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:31` |
| `unicode_mixed` | `S / NO-GO` | 6390 / 4970 / 9943 | `criterion-slope-profile:json_unicode_mixed/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:33` |
| `unicode_escapes` | `S / NO-GO` | 12731 / 8521 / 13851 | `criterion-slope-profile:json_unicode_escapes/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:35` |
| `unicode_basic` | `S / NO-GO` | 11189 / 10040 / 15797 | `criterion-slope-profile:json_unicode_basic/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:37` |
| `distinct_values` | `S / NO-GO` | 10279 / 6457 / 18282 | `criterion-slope-profile:json_distinct_values/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:39` |
| `y_string_unicode` | `S / NO-GO` | 5577 / 5480 / 12009 | `criterion-slope-profile:json_y_string_unicode/track1_generated/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:41` |

## Section 5 - Findings: `direct_to_struct`

All direct rows are digest-plane guard rows until a direct output contract or
control-path tranche exists (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:180`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:181`). No row has a resolved
self-time symbol, so no main-row hot-leaf class is assigned.

| Corpus | W0 state | Track 1 / Track 2 / sonic Mbps | W0 hot-leaf artifact | `% self-time` | Hot-leaf class | Row evidence |
|---|---|---:|---|---|---|---|
| `twitter` | `N-direct / NO-GO` | 11859 / 9881 / 12890 | `criterion-slope-profile:json_twitter/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:6` |
| `citm_catalog` | `A / GO` | 21151 / 19434 / 18241 | `criterion-slope-profile:json_citm_catalog/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:9` |
| `canada` | `N-direct / NO-GO` | 6586 / 9769 / 12430 | `criterion-slope-profile:json_canada/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:11` |
| `apache_builds` | `N-direct / NO-GO` | 8306 / 7796 / 8852 | `criterion-slope-profile:json_apache_builds/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:13` |
| `github_events` | `N-direct / NO-GO` | 9088 / 7337 / 9818 | `criterion-slope-profile:json_github_events/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:15` |
| `update_center` | `N-direct / NO-GO` | 7863 / 7514 / 10525 | `criterion-slope-profile:json_update_center/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:17` |
| `mesh` | `N-direct / NO-GO` | 8640 / 9049 / 9967 | `criterion-slope-profile:json_mesh/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:20` |
| `random` | `N-direct / NO-GO` | 7751 / 6952 / 8141 | `criterion-slope-profile:json_random/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:23` |
| `gsoc-2018` | `N-direct / NO-GO` | 15042 / 14380 / 23356 | `criterion-slope-profile:json_gsoc-2018/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:25` |
| `marine_ik` | `A / GO` | 9357 / 9488 / 8559 | `criterion-slope-profile:json_marine_ik/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:27` |
| `instruments` | `N-direct / NO-GO` | 8494 / 8766 / 9872 | `criterion-slope-profile:json_instruments/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:30` |
| `numbers` | `N-direct / NO-GO` | 9773 / 6966 / 7953 | `criterion-slope-profile:json_numbers/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:32` |
| `unicode_mixed` | `N-direct / NO-GO` | 3596 / 3694 / 10077 | `criterion-slope-profile:json_unicode_mixed/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:34` |
| `unicode_escapes` | `N-direct / NO-GO` | 4020 / 4016 / 13999 | `criterion-slope-profile:json_unicode_escapes/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:36` |
| `unicode_basic` | `A / GO` | 9363 / 8420 / 8971 | `criterion-slope-profile:json_unicode_basic/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:38` |
| `distinct_values` | `N-direct / NO-GO` | 4438 / 4151 / 8950 | `criterion-slope-profile:json_distinct_values/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:40` |
| `y_string_unicode` | `N-direct / NO-GO` | 4828 / 3563 / 9065 | `criterion-slope-profile:json_y_string_unicode/track1_direct_to_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:42` |

## Section 6 - Findings: `real_typed_struct`

The current measured typed plane has four `A / GO` rows. Apache/CITM typed
source/product parity exists, but SK-V8 W2 explicitly did not update
`skinny/RESULTS.md` with those measured rows (`skinny/REDRESS.md:2622`,
`skinny/REDRESS.md:2648`, `skinny/REDRESS.md:2651`). `canada/real_typed_struct`
is pre-blocked by checksum mismatch (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:218`).

| Corpus | W0 state | Track 1 / Track 2 / sonic Mbps | W0 hot-leaf artifact | `% self-time` | Hot-leaf class | Row evidence |
|---|---|---:|---|---|---|---|
| `twitter` | `A / GO` | 15333 / 14516 / 13646 | `criterion-slope-profile:json_twitter/track1_real_typed_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:7` |
| `update_center` | `A / GO` | 11958 / 10367 / 11952 | `criterion-slope-profile:json_update_center/track1_real_typed_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:18` |
| `mesh` | `A / GO` | 9623 / 7674 / 9305 | `criterion-slope-profile:json_mesh/track1_real_typed_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:21` |
| `marine_ik` | `A / GO` | 11783 / 8321 / 6951 | `criterion-slope-profile:json_marine_ik/track1_real_typed_struct/new/estimates.json` | GAP: no SK-V9-open samply profile | GAP:not-classified | `skinny/RESULTS.md:28` |

| Corpus | Status | `% self-time` | Hot-leaf class | Evidence |
|---|---|---|---|---|
| `apache_builds` | Source/product parity admitted by REDRESS 91; absent as measured `RESULTS.md` row | GAP: no measured row and no samply profile | GAP:not-classified | `restart/skinny/tranches/sk-v9/SYNTHESIS.md:216`; `skinny/REDRESS.md:2622` |
| `citm_catalog` | Source/product parity admitted by REDRESS 91; absent as measured `RESULTS.md` row | GAP: no measured row and no samply profile | GAP:not-classified | `restart/skinny/tranches/sk-v9/SYNTHESIS.md:217`; `skinny/REDRESS.md:2623` |
| `canada` | Rejected/routed until full-fixture checksum proof exists | GAP: no measured row and no samply profile | GAP:not-classified | `restart/skinny/tranches/sk-v9/SYNTHESIS.md:218`; `skinny/REDRESS.md:2637` |

## Section 7 - Findings: Masking Probes

Masking probe rows are configured and source-visible, but current
`skinny/RESULTS.md` does not render probe rows. The missing measurements are
therefore explicit gaps. Probe classes below are assigned only when the probe is
single-purpose in source.

| Probe | Source-defined operation | Current measurement | `% self-time` | Supported class | Evidence |
|---|---|---|---|---|---|
| `host_call_dispatch_overhead` | Function-pointer registry call over `&str` | GAP: no rendered row in current `RESULTS.md` | GAP: no SK-V9-open samply profile | `dispatch` | `skinny/crates/bbnf-bench/benches/json_parity.rs:394`; `skinny/crates/bbnf-bench/benches/json_parity.rs:396`; `skinny/crates/bbnf-bench/src/bin/gate.rs:1554` |
| `host_call_eager_decode` | Parse, then walk every key/string and call `as_str()` | GAP: no rendered row in current `RESULTS.md` | GAP: no SK-V9-open samply profile | `string`; `unicode` only when escapes reach unescape/decode paths | `skinny/crates/bbnf-bench/benches/json_parity.rs:399`; `skinny/crates/bbnf-bench/benches/json_parity.rs:440`; `skinny/crates/runtime/src/grammars/json/view.rs:205`; `skinny/crates/parse-that-regex/src/lib.rs:302` |
| `alternate_scalar_plan` | External `serde_json::Value` parse | GAP: no rendered row in current `RESULTS.md` | GAP: no SK-V9-open samply profile | GAP:not-classified; external implementation is not a local hot leaf | `skinny/crates/bbnf-bench/benches/json_parity.rs:407`; `skinny/crates/bbnf-bench/benches/json_parity.rs:409` |
| `alternate_dispatch_table_plan` | Disabled duplicate-probe row; gate marks invalid | INVALID by gate if emitted | GAP: no valid profile | GAP:not-classified | `skinny/crates/bbnf-bench/src/bin/gate.rs:1516`; `skinny/crates/bbnf-bench/src/bin/gate.rs:1523`; `skinny/REDRESS.md:216` |
| `alternate_pext_mask_plan` | x86-only structural-offset scalar scan probe | GAP: absent on this aarch64 host and no rendered row | GAP: no SK-V9-open samply profile | `scan` for the x86-only source probe; not runnable here | `skinny/crates/bbnf-bench/benches/json_parity.rs:414`; `skinny/crates/bbnf-bench/benches/json_parity.rs:417`; `skinny/crates/bbnf-bench/src/bin/gate.rs:1513` |
| `cold_first_parse` | Clone bytes, convert to UTF-8, then run generated parse | GAP: no rendered row in current `RESULTS.md` | GAP: no SK-V9-open samply profile | GAP:not-classified; parse composition has no resolved leaf | `skinny/crates/bbnf-bench/benches/json_parity.rs:422`; `skinny/crates/bbnf-bench/benches/json_parity.rs:427`; `skinny/crates/bbnf-bench/src/bin/gate.rs:1577` |

## Section 8 - Anomalies And S-P2 Inputs

1. Current `RESULTS.md` hot-leaf cells are not `unprofiled`, but they are still
   Criterion estimate bindings rather than named samply symbols. This satisfies
   the W0 validator but not the S-P1 CH1 expectation that each hot-leaf claim
   name a symbol path, self-time percentage, and file:line
   (`restart/prompts/skinny/PASS-1-PROFILE.md:123`,
   `restart/prompts/skinny/PASS-1-PROFILE.md:124`).
2. No `parse_only`, `direct_to_struct`, or `real_typed_struct` row can be
   classified as `scan`, `number`, `string`, `unicode`, `structural`, `tape`, or
   `dispatch` as a hot leaf yet. Source support only names eligible surfaces.
3. Masking probe classifications are partial: `host_call_dispatch_overhead` is
   source-supported as `dispatch`, `host_call_eager_decode` as `string` with a
   `unicode` escape subpath, and the x86-only `alternate_pext_mask_plan` as
   `scan`. All other probe rows remain unclassified gaps.
4. Direct digest rows remain guard-plane evidence only; they are not product
   proof before a direct output contract or control-path tranche
   (`restart/skinny/tranches/sk-v9/HANDOFF.md:47`,
   `restart/skinny/tranches/sk-v9/HANDOFF.md:49`).
5. The tape/direct substrate must stay unified: PASS-1 says tape/direct/value
   variants carry tape kind, span, payload slot, and direct fields, with direct
   values borrowing from tape identity (`restart/audit/pass-1-substrate/PASS-1.md:54`);
   S-P1 specifically says P1-E must attribute tape symbols as substrate rather
   than as a separable producer (`restart/prompts/skinny/PASS-1-PROFILE.md:263`,
   `restart/prompts/skinny/PASS-1-PROFILE.md:265`).

## Section 9 - Sources

Primary authority:

- `skinny/RESULTS.md:3` - main table schema with `Hot leaf`.
- `skinny/RESULTS.md:5` through `skinny/RESULTS.md:42` - current main rows.
- `skinny/RESULTS.md:138` through `skinny/RESULTS.md:141` - overall outcome,
  Track 1/Track 2 identity, and W0 telemetry freshness.
- `skinny/REDRESS.md:163` through `skinny/REDRESS.md:170` - masking probe
  report artifact surface.
- `skinny/REDRESS.md:2622` through `skinny/REDRESS.md:2652` - SK-V8 W2 typed
  product/source admission without row-table admission.
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md:21` through
  `restart/skinny/tranches/sk-v9/SYNTHESIS.md:52` - opening state and
  Alpha-E gate-only prerequisites.
- `restart/skinny/tranches/sk-v9/HANDOFF.md:22` through
  `restart/skinny/tranches/sk-v9/HANDOFF.md:37` - current state.
- `restart/prompts/skinny/PASS-1-PROFILE.md:50` through
  `restart/prompts/skinny/PASS-1-PROFILE.md:57` - P1 agent scope matrix.
- `restart/prompts/skinny/PASS-1-PROFILE.md:123` through
  `restart/prompts/skinny/PASS-1-PROFILE.md:160` - S-P1 challenge expectations
  for symbol, self-time, file:line, and gap discipline.

Source authority:

- `skinny/crates/runtime/src/grammars/json/parser.rs:47` through
  `skinny/crates/runtime/src/grammars/json/parser.rs:51` - generated parse
  entry and tape finish.
- `skinny/crates/runtime/src/grammars/json/generated.rs:35` through
  `skinny/crates/runtime/src/grammars/json/generated.rs:58` - retained dispatch.
- `skinny/crates/runtime/src/grammars/json/generated.rs:142` through
  `skinny/crates/runtime/src/grammars/json/generated.rs:217` - string and number
  retained parse helpers.
- `skinny/crates/runtime/src/grammars/json/generated.rs:409` through
  `skinny/crates/runtime/src/grammars/json/generated.rs:464` - direct sink entry
  and dispatch.
- `skinny/crates/runtime/src/grammars/json/sink.rs:16` through
  `skinny/crates/runtime/src/grammars/json/sink.rs:92` - direct sink string
  source and unescape hooks.
- `skinny/crates/runtime/src/tape/assembler.rs:61` through
  `skinny/crates/runtime/src/tape/assembler.rs:113` - tape offset and flag
  writes.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:43` through
  `skinny/crates/bbnf-bench/benches/json_parity.rs:48` - Track 1 retained bench.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:181` through
  `skinny/crates/bbnf-bench/benches/json_parity.rs:223` - direct Track 1/2
  bench rows.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:261` through
  `skinny/crates/bbnf-bench/benches/json_parity.rs:350` - real typed bench rows.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:381` through
  `skinny/crates/bbnf-bench/benches/json_parity.rs:455` - masking probe bench
  group and eager decode walker.
