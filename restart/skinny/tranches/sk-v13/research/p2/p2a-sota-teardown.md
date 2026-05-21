# SK-V13 P2-A: SOTA Comparator Teardown

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-21.
Scope: SOTA comparator teardown for asmjson, sonic-rs, simdjson, and yyjson keyed to SK-V13 S-P1 hot leaves and strict-vs-strict discipline.
Output: this file.
P1 hot-leaf antecedents: `dispatch_value`, `parse_object_value_at_direct`, `parse_array_element_at_direct`, `match_tiny_plain_string_with_cap::<16>`, `parse_that_regex::unescape_string`, `read_hex_unit_scalar`, `scan_tail`, `scan_structurals`, `bulk_emit_positions_64_neon`, `fold_string_scalar`/`hash_bytes`, `materialize_u64`/`materialize_f64`, CSS `LocalFactSink::finish`/timer overhead.
Lock surface: Lock 1 + Lock 14 + Lock 16.

## §1 — Findings

S-P2 is authorized because S-P1 converged 6/6 in V4 and V5, with no S-P1
profile fact promoted to admission (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:10`,
`:19`-`:24`, `:60`-`:68`). The P2-A scope is specifically comparator
architecture and candidate primitive research, not source changes or wave
selection (`restart/prompts/skinny/PASS-2-RESEARCH.md:36`-`:48`,
`:62`-`:85`). The full-SOTA addendum controls the comparator bar: all 51 JSON
rows must beat sonic-rs strict on the same plane by at least 1 Mbps, `parse_only`
is admission-eligible again, and all remaining CSS L4 parity rows must beat
lightningcss on strict same-plane equality (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22`-`:46`,
`:78`-`:94`).

The S-P1 hot leaves are mostly envelopes, not ready-made primitives. Direct
rows rank `parse_object_value_at_direct` or `parse_array_element_at_direct` as
rank-1 on 15 of 17 direct rows, with `unicode_escapes` naming
`parse_that_regex::unescape_string` and `instruments` resolving only to generic
inline/noise (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:32`-`:50`).
Parse-only is 15/17 `dispatch_value`, plus `match_tiny_plain_string_with_cap`
and `read_hex_unit_scalar` on the two exceptions
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:52`-`:62`).
Mode III shows the scanner micro-route is real but non-admitting:
`scan_structurals`/`bulk_emit_positions_64_neon` beat scalar structural scan
1.49x-5.04x across 17 corpora, but the ledger marks it as a scanner
micro-result only and explicitly does not reopen REDRESS 96/97/98
(`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:69`-`:96`;
`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:78`-`:92`).
CSS declaration-values is currently
timer/fact-sink dominated, with parser hot leaf unresolved
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100`-`:104`;
`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:79`-`:88`).

The current result surface carries useful comparator metadata, but only
sonic-rs strict is a JSON admission comparator today. `RESULTS.md` records
same-run native sonic strict rows; C++/DOM sidecars are frequently historical,
absent, or `n/a`, and the file itself warns that native Rust comparators are
same-run while C++ sidecars are historical or absent (`skinny/RESULTS.md:3`,
`:91`-`:94`, `:145`-`:149`). Therefore simdjson, yyjson, and asmjson are
architecture pressure unless S-P3 later wires strict same-plane sidecars.
Likewise, asmjson is not a strict anchor here: the authoritative source found is
the crate-published docs.rs source, whose own conformance note is permissive;
no maintained official repository source was found that should override that
gap [A1] [A3].

| Comparator | Structural classification | Number/string fast paths | Output plane | Strictness discipline | What it does that bbnf does not, keyed to S-P1 |
|---|---|---|---|---|---|
| asmjson | Published crate docs describe 64-byte AVX-512BW assembly or portable SWAR classification [A1] [A2]. This is x86-only for the assembly path, so it is outside SK-V13 host scope except as shape evidence. | Wide byte-state masks skip whitespace/string bodies, but the conformance note says controls can be treated permissively and unescaped controls inside strings are not scanned [A3]. | DOM/tape and SAX writer APIs, including unsafe AVX-512BW direct entries [A2] [A4]. | Architecture-pressure only. Not a strict JSON comparator for SK-V13, and x86 is out of scope. | A 64-byte transient classifier plus direct writer/tape sink. bbnf has `byte_class_from_eq_set_64` and structural-scan evidence, but no same-wave generated consumer that turns the classifier into JSON/CSS row movement (`skinny/crates/bbnf-simd/src/lib.rs:259`-`:271`; `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:87`-`:92`). |
| sonic-rs | Explicitly does not use simdjson's two-stage algorithm; uses SIMD in targeted spots: long strings, float fraction parsing, field lookup, and whitespace [S1]. | Serde direct-to-struct, lazy value/object iterators, `RawNumber`, and number precision matching Rust std by default [S1] [S2]. | Rust serde direct plane, mutable untyped value, lazy array/object iterator, raw/lazy value surfaces [S2]. | This is the binding JSON SOTA comparator only through strict same-plane rows. Lossy, unchecked, permissive, or flaw-probe modes are not admission anchors (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:89`-`:94`). | Targeted SIMD in the exact families S-P1 names: string spans, float/numeric spans, field/object lookup, whitespace. bbnf has direct/typed surfaces, but ten typed rows still lack a generated typed product surface and all 13 N-direct rows are reopened under the addendum (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:102`-`:111`; `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:58`-`:74`). |
| simdjson | Official docs describe stage-1 structural indexing and stage-2 tape/On-Demand consumption; stage 1 finds structural indexes and validates UTF-8 before stage 2 builds tape [J1] [J2]. | Quote/escape/in-string masks and deferred On-Demand value extraction; On-Demand parses/skips values as used rather than materializing all values up front [J2] [J3]. | DOM tape and On-Demand iterator/front-end. | Strict JSON architecture source, but not a SK-V13 row comparator unless same-run same-plane sidecars are produced. | Structural index consumption without scalar rediscovery. bbnf cannot retain a parallel structural index under Lock 1; any legal transfer must consume masks in the same loop into the existing tape/direct/CSS sink (`restart/locks/LOCKS.md:52`; `skinny/REDRESS.md:2910`-`:2940`). |
| yyjson | Portable ANSI C with no explicit SIMD, relying on scalar inlining, branch prediction, and compact data layout [Y1] [Y2]. | Specialized scalar string/number readers, strict default flags, and opt-in JSON5/permissive features [Y1] [Y3]. | Immutable/mutable document/value model; object/array iteration over a C value tree [Y4]. | Strict default is useful as pressure, but current bbnf rows do not carry same-run yyjson strict sidecars on most planes (`skinny/RESULTS.md:3`, `:149`). | The lesson is scalar discipline: keep hot envelopes small, inlined, and branch-predictable. This maps to P1's `dispatch_value`/direct envelopes more than to a new retained substrate (`skinny/crates/runtime/src/grammars/json/generated.rs:45`-`:56`, `:466`-`:542`). |

The comparator consensus is narrower than "copy a SOTA parser." Transferable
shapes are transient byte classification, bounded string special-byte scans,
quote/escape state, digit-run accumulation, generated FIRST/follow dispatch,
and row-owned output sinks. Every candidate below is therefore framed as a
scalar-reference-first primitive with checkasm/parity expectations and a
same-wave consumer; support-only landings are disallowed by the addendum
(`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:96`-`:102`) and by
Lock 16 (`restart/locks/LOCKS.md:87`-`:112`).

## §2 — Candidate primitives

| Candidate | Shape | Scalar-ref status | Arch + checkasm expectation | P1 antecedent | Same-wave consumer / reject boundary |
|---|---|---|---|---|---|
| C1 `class_mask64_transient` | Given a grammar-supplied byte-set table and up-to-64-byte window, return transient masks for delimiter, trivia, quote, escape, FIRST-set, and stop classes. No retained lane. | Partially present through scalar `byte_class_from_eq_set_64` and JSON `scan_tail`; needs a generated-table scalar oracle covering duplicate classes and high-bit bytes (`skinny/crates/bbnf-simd/src/lib.rs:259`-`:271`; `skinny/crates/runtime/src/grammars/json/scan.rs:107`-`:160`). | aarch64 NEON TBL/TBX or equality-tree body only after scalar oracle. Checkasm: all alignments, tails 0-63, empty/full masks, duplicate sets, low-6 collisions, high-bit bytes, CSS/JSON/Sheets/BBNF byte sets. | `scan_structurals`, `scan_tail`, `dispatch_value`, `parse_object_value_at_direct`, CSS fact-sink/timer profile. | Must wire into a generated CSS delimiter/layout scanner, generated JSON dispatch guard, or Sheets/BBNF token scanner in the same wave. Reject if it creates a retained class sidecar or lands as benchmark-only inventory. |
| C2 `bounded_special_string_end` | Bounded scan returning first terminator, escape, control, non-ASCII, or grammar-sentinel byte plus stop kind. Quote/escape/control policy is supplied by grammar config. | Partial scalar exists in JSON generated tiny string scan and parse-that string code; needs lifted scalar reference with caller-owned policy (`skinny/crates/runtime/src/grammars/json/generated.rs:169`-`:183`; `skinny/crates/parse-that-regex/src/lib.rs:718`-`:775`). | aarch64 wide string-block/vext body allowed only with direct scalar parity. Checkasm: every stop byte position, cap boundaries, no-stop tails, all alignments, long ASCII, high-bit bytes, grammar-specific terminators. | `match_tiny_plain_string_with_cap::<16>`, `unescape_string`, direct string sinks, CSS string/URL/selector rows once S-P3 scopes them. | Same-wave consumer must be JSON string row or CSS string/URL/identifier scanner that moves a SOTA/parity row. Reject a replay of REDRESS 28/33 or REDRESS 83 tiny-probe shapes without grammar-neutral policy and row movement. |
| C3 `escape_segment_hex_decode` | Decode escaped segments and fixed-width hex units, returning decoded scalar(s), validity, surrogate state, and raw/borrowed segment boundaries. | Scalar exists as `unescape_string` and private `read_hex_unit_scalar`; a public scalar oracle for x4/xN hex decode is missing (`skinny/crates/parse-that-regex/src/lib.rs:718`, `:945`-`:965`). | aarch64 TBL/TBX or NEON nibble classify; checkasm over valid/invalid hex, surrogate pairs, mixed raw+escaped windows, tails, alignment, and xorshift/adversarial escape masks. `escape_mask_64` dedicated proof is prerequisite where mask fusion is used (`skinny/crates/bbnf-simd/src/lib.rs:175`-`:205`; `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:398`-`:463`). | `unicode_escapes` direct `unescape_string`, `y_string_unicode` parse-only `read_hex_unit_scalar`, unicode direct residuals. | Same-wave consumer must move JSON unicode parse/direct rows or CSS escaped-ident/string parity. Reject proof-only unicode codec replay from REDRESS 82/107/108 class without admitted row movement. |
| C4 `digit_run_accumulate` | Scan digit run and optionally accumulate bounded prefixes into integer lanes, returning end, digit count, overflow/truncation, decimal/exponent boundary, and raw span. Grammar number policy stays above primitive. | Scalar span/materialization exists through `match_number_span_from_first` and direct materializers; UDOT helper is support-only today (`skinny/crates/bbnf-bench/src/direct_struct.rs:10`, `:98`-`:102`, `:579`; `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:148`-`:151`). | aarch64 UDOT/DotProd body if micro-proven. Checkasm: 0/1/4/8/16/19/20-digit runs, overflow, stop bytes, signs, decimal/exponent boundary, invalid stops, CSS dimension/percentage samples. | `parse_array_element_at_direct` on `canada`, `numbers`, `mesh`, `marine_ik`; lower-rank `materialize_u64`/`materialize_f64` from P1 top-20. | Same-wave consumer must be JSON numeric direct/typed/parse row or CSS number/dimension/calc row. Reject if it merely wires `digit_mac` without gate-consumed row movement. |
| C5 `generated_first_follow_probe` | Codegen emits one- or two-byte lookahead tables from FIRST/follow facts and returns a branch code to current generated parser; no runtime grammar names. | Needs scalar generated interpreter and table dump oracle. JSON hardcoded match arms are current evidence, not the final generic shape (`skinny/crates/runtime/src/grammars/json/generated.rs:45`-`:56`, `:466`-`:542`). | No checkasm unless backed by C1. If SIMD-backed, inherit C1 parity. Must include grammar-neutral table-generation proof under Lock 14. | `dispatch_value`, direct envelopes, CSS parser hot leaf unresolved but parity matrix open. | Same-wave consumer must simplify a generated CSS feature row or JSON envelope row. Reject generic-crate JSON/CSS match arms or a dispatch-table/function-pointer replay that CH3 would classify as prior blocked route (`restart/prompts/skinny/PASS-2-RESEARCH.md:109`-`:117`). |
| C6 `same_loop_structural_mask_consume` | Consume structural masks in the same loop into existing offset tape, direct sink, or CSS fact sink. Masks are transient; if retained, they are the tape projection, not a sidecar. | Scalar reference is `scan_tail` plus existing generated consumer; must prove equivalence at the event/fact stream boundary, not just mask equality (`skinny/crates/runtime/src/grammars/json/scan.rs:107`-`:160`). | aarch64 PMULL/CSSC-CTZ/EOR3 only with material differential vs REDRESS 96/97/98 and 88/89. Checkasm: mask parity, bit extraction order, string carry, tail windows, corpus parity, same-wave row profile. | Mode-III structural SIMD 1.49x-5.04x; direct/parse envelopes that rediscover structural bytes. | Highest CH3 risk. Same-wave consumer must move a JSON or CSS row. Reject retained class-column, streaming cursor, or class-lane-only variants already falsified by REDRESS 96/97/98. |
| C7 `ascii_set_member_find64_css` | Production wrapper around `byte_class_from_eq_set_64` for finding first member of a small generated delimiter/layout set in CSS scanner loops. | Scalar `find_scalar` and candidate wrapper already exist in checkasm artifact; W4 microbench measured 4.72x but production wiring was historically routed to a split (`skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs:20`-`:41`, `:137`-`:191`; `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:31`-`:73`). | aarch64 NEON equality-set body already dispatches through `byte_class_from_eq_set_64`; add production-path checkasm/equality with CSS corpus and every delimiter set selected. | CSS declaration-values timer/fact-sink profile plus SK-V12 W4 production split; selected CSS parity rows may expose layout/delimiter loops. | Same-wave consumer must be generated CSS scan block and strict lightningcss equality. Reject another microbench-only route; REDRESS-126 already says production/gate split is routed separately, not admitted. |
| C8 `output_digest_fold_u64x2_sink` | Output-plane hash fold over raw and decoded segments for JSON direct digest rows; bypass parser only if generated resolver selects a legal SinkOnly shape. | Scalar exists in `JsonDirectDigest::fold_string_scalar`, `hash_bytes`, and `mix` (`skinny/crates/bbnf-bench/src/direct_struct.rs:123`-`:127`, `:717`-`:737`). | aarch64 u64x2/PMULL/SHA3 mixing only after scalar parity. Checkasm: chunk boundaries, raw vs decoded segments, endian stability, strict Track 2 equality, no hash-only semantic shortcut. | Lower-rank `fold_string_scalar`/digest leaves on direct residuals; direct envelopes on twitter/github/update/distinct/gsoc. | Same-wave consumer is JSON direct digest only. Reject if proposed as parser speed or generic grammar primitive; REDRESS 118/119 history must be cited and material differential named. |

Support-only items not listed as row-moving primitives: raw movemask packers,
prefetch/cache hints, orphan bitmap scalar delegates, comparator harness
adapters, and standalone egraph/CSP scaffolding. They become admissible only
when attached to a candidate above with scalar reference, checkasm/parity, and a
same-wave row consumer.

## §3 — Grammar-neutrality

| Candidate | Grammar-neutrality verdict |
|---|---|
| C1 `class_mask64_transient` | Generalizes if classes are generated tables. JSON structurals, CSS delimiters/trivia/operator starts, Sheets formula delimiters, and BBNF token starts are data, not generic-crate branches. |
| C2 `bounded_special_string_end` | Generalizes if the stop set and escape/control policy are caller-owned. JSON strings, CSS strings/URLs/idents, Sheets doubled quotes, and BBNF literals differ above the stop-scan layer. |
| C3 `escape_segment_hex_decode` | Generalizes as byte/nibble decode plus state reporting. JSON `\uXXXX`, CSS escaped idents/strings, BBNF escapes, and Sheets quote rules require grammar policy tables from GrammarConfig. |
| C4 `digit_run_accumulate` | Generalizes at digit-run and bounded-accumulator level. JSON numbers, CSS dimensions/percentages/calc numbers, Sheets numerics, and BBNF numeric literals differ in grammar policy after span. |
| C5 `generated_first_follow_probe` | Generalizes only if generated from FIRST/follow facts and emitted into per-grammar generated modules. A JSON object-tail probe cannot live in generic runtime or codegen. |
| C6 `same_loop_structural_mask_consume` | Generalizes only as transient mask consumption into the one legal substrate/output sink. It must not become a grammar-named generic substrate or retained sidecar. |
| C7 `ascii_set_member_find64_css` | Generalizes as small-set byte search. CSS is the first obvious consumer, but the primitive must accept generated sets and be valid for Sheets/BBNF token delimiters as well. |
| C8 `output_digest_fold_u64x2_sink` | JSON-output-plane only unless a grammar explicitly chooses a digest fact stream as its row output. It is not a parser primitive and should not be shared as generic parse policy. |

Lock 14 is the hard line: generic crates cannot learn the words JSON, CSS,
Sheets, or BBNF in control flow (`restart/locks/LOCKS.md:78`). Candidate
grammar differences must enter as generated tables, GrammarConfig policy, or
per-grammar generated modules. Lock 1 is the substrate line: simdjson/asmjson
style retained indexes are legal only if they are the single tape projection,
not a sidecar (`restart/locks/LOCKS.md:52`). Lock 16 is the SIMD line: every
SIMD/ASM route needs scalar parity, checkasm/unit parity, corpus parity, and
allowlist provenance before it can be selected (`restart/locks/LOCKS.md:87`-`:112`).

## §4 — Risks

- REDRESS 96/97/98 remain live historical falsifiers. C6 or any union candidate
  must cite them and name a material differential; retained class-column,
  allocation-free streaming cursor, and class-lane-only paper-close variants are
  not fresh (`skinny/REDRESS.md:2850`-`:2906`, `:2910`-`:2940`).
- REDRESS 119 is history only under the addendum, but it still defines the
  prior direct-row evidence. Every direct reopen must cite it, name the new
  material differential, and use strict same-plane comparator evidence
  (`skinny/REDRESS.md:3506`-`:3544`;
  `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:58`-`:74`).
- REDRESS 126 closes the prior zero-orphan accounting by demotion, not by
  production admission. New SIMD routes cannot land as support inventory; they
  must wire a production consumer or delete/demote with evidence (`skinny/REDRESS.md:3864`-`:3872`).
- PMULL prefix-XOR and CSSC CTZ remain REDRESS 88/89-adjacent. USER PIN D4
  unblocks the category, not stale implementations; C6 must prove a same-loop
  consumer and row movement before any PMULL/CSSC body is production code
  (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:156`-`:170`).
- REDRESS 28/33 and 82-84 block naive NEON tiny-string, single-quartet unicode,
  StringBlock16 tiny-probe, and object-pair replay. C2/C3 must be grammar-policy
  driven and row-moving, not renamed old kernels
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:109`-`:117`).
- C++/DOM comparators cannot be used as JSON admission where `RESULTS.md` says
  the sidecar is absent, historical, or different plane. S-P3 must either wire a
  same-run strict same-plane comparator or keep simdjson/yyjson/asmjson as
  architecture pressure only (`skinny/RESULTS.md:3`, `:91`-`:94`, `:149`).
- CSS P1 declaration-values profile is not a parser hot-leaf proof. C7 may be
  routed because it has a prior microbench and plausible CSS scanner consumer,
  but S-P3 still needs the selected CSS feature row to expose a measured parser
  hot loop and strict lightningcss equality
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100`-`:104`).
- C8 is output-plane scoped. A digest fold may move JSON direct rows, but it
  cannot be represented as a grammar-neutral parser optimization or used to
  sidestep strict semantic equality.

## §5 — Sources

Local authority and profile sources:

- `restart/prompts/skinny/PASS-2-RESEARCH.md:36`-`:48`, `:62`-`:85`,
  `:95`-`:120` for P2-A scope, output schema, and candidate requirements.
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22`-`:46`,
  `:58`-`:94`, `:96`-`:102`, and `:151`-`:170` for full CSS/JSON SOTA,
  strict-vs-strict, row movement, and close rules.
- `restart/skinny/tranches/sk-v13/HANDOFF.md:28`-`:31`, `:42`-`:52`,
  `:128`-`:140`, and `:142`-`:168` for SK-V13 obligations and refusal
  conditions.
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:10`,
  `:19`-`:24`, `:53`-`:61`, and `:65`-`:68` for S-P1 convergence and limits.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:23`,
  `:32`-`:62`, `:78`-`:104`, and `:115`-`:128` for hot leaves, profile status,
  mode-III boundaries, CSS profile state, and REDRESS guards.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:69`-`:96`
  for structural scanner scalar/SIMD ratios and source anchors.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:41`-`:55`,
  `:57`-`:88`, `:102`-`:131` for result inventory, direct/CSS profile signals,
  and comparator gaps.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:10`-`:23`,
  `:31`-`:73`, `:77`-`:137`, and `:142`-`:170` for orphan state, W4
  production split, union candidates, and ARMv9.2 surface.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:398`-`:463`
  for `escape_mask_64` falsifier status and remaining proof gaps.
- `skinny/RESULTS.md:3`, `:91`-`:94`, `:145`-`:149` for comparator columns,
  same-run strict sidecar status, CSS row, and warning about historical/absent
  sidecars.
- `skinny/REDRESS.md:2850`-`:2906`, `:2910`-`:2940`, `:3506`-`:3544`,
  and `:3864`-`:3872` for REDRESS 96/97/98, 119, and 126 risk surfaces.
- `restart/locks/LOCKS.md:52`, `:78`, and `:87`-`:112` for Locks 1, 14, and 16.

Local code anchors:

- `skinny/crates/runtime/src/grammars/json/generated.rs:12`-`:15` for the
  current no-op `attach_structural_index`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:45`-`:56`,
  `:88`-`:115`, `:169`-`:183`, and `:466`-`:542` for JSON dispatch, key-colon,
  tiny string, and direct envelopes.
- `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`:35`,
  `:47`-`:104`, and `:107`-`:160` for structural scan, capacity counting, and
  scalar tail scanning.
- `skinny/crates/parse-that-regex/src/lib.rs:718`-`:775` and `:945`-`:965`
  for `unescape_string`, `read_hex_unit_scalar`, and hex nibble decoding.
- `skinny/crates/bbnf-simd/src/lib.rs:175`-`:205`, `:209`-`:222`, and
  `:259`-`:271` for `escape_mask_64`, `compact_mask`, and byte-set classify.
- `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs:20`-`:41`
  and `:137`-`:191` for scalar/candidate delimiter find and microbench artifact.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:98`-`:127`,
  `:579`, and `:717`-`:737` for direct numeric materialization and digest fold.

Primary comparator sources:

- [A1] asmjson crate-published README, 64-byte AVX-512/SWAR classifier:
  https://docs.rs/crate/asmjson/0.2.5/source/README.md#L295-L300
- [A2] asmjson runtime selection / unsafe AVX-512 entry points:
  https://docs.rs/crate/asmjson/0.2.5/source/README.md#L321-L329
- [A3] asmjson conformance caveats:
  https://docs.rs/crate/asmjson/0.2.5/source/README.md#L478-L489
- [A4] asmjson output/API surface:
  https://docs.rs/crate/asmjson/0.2.5/source/README.md#L457-L470
- [S1] sonic-rs README, SIMD usage and no simdjson two-stage algorithm:
  https://github.com/cloudwego/sonic-rs/blob/main/README.md#L60-L66
- [S2] sonic-rs README, features / serde / lazy value / raw number:
  https://github.com/cloudwego/sonic-rs/blob/main/README.md#L84-L90
- [J1] simdjson parse-many docs, stage 1 structural indexes and stage 2 tape:
  https://simdjson.org/api/1.0.0/md_doc_parse_many.html
- [J2] simdjson basics, On-Demand parser, strict UTF-8 JSON input, padding, and
  iterator model:
  https://raw.githubusercontent.com/simdjson/simdjson/master/doc/basics.md
- [J3] simdjson On-Demand design:
  https://raw.githubusercontent.com/simdjson/simdjson/master/doc/ondemand_design.md
- [Y1] yyjson README, ANSI C / no explicit SIMD / strict RFC 8259 / UTF-8:
  https://github.com/ibireme/yyjson/blob/master/README.md#L10-L15
- [Y2] yyjson README, performance preference for high ILP, branch prediction,
  and optimizer:
  https://github.com/ibireme/yyjson/blob/master/README.md#L113-L118
- [Y3] yyjson read flags:
  https://github.com/ibireme/yyjson/blob/master/src/yyjson.h#L736-L834
- [Y4] yyjson README, document/value sample API:
  https://github.com/ibireme/yyjson/blob/master/README.md#L121-L160
