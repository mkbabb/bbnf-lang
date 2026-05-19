# SK-V10 S-P2 V1 CH5: Hidden Coupling / Lock 1

Disposition: REVISE.
Date: 2026-05-19.
Scope: S-P2 P2-A..P2-F hidden-coupling review, with primary emphasis on
P2-D/P2-E, Lock 1 substrate cardinality, sidecar/event-stream/tape-split
risks, consumer coupling, and direct-vs-typed plane honesty.
Output: this file.

## Verdict

REVISE, not REJECT. The S-P2 packet is directionally sound: it keeps W3 retired,
keeps parse-only out of SOTA admission, rejects retained structural cursors, and
does not explicitly propose a new retained sidecar. The remaining defects are
contract ambiguities that can let S-P3 couple planes later unless folded now.

The required fixes are:

1. Fence capacity pre-scans as diagnostic/env-only or reject them as product
   evidence under CH5.
2. Replace "same sink event stream" language with a per-plane consumer matrix.
3. Split string/number primitive consumers by exact direct, typed, and retained
   call site, including current cap differences.
4. State that direct digest row movement cannot authorize typed row movement,
   and that typed admission still requires its own generated/serde/sonic parity.

## Lock 1 Predicate

Lock 1 says the tape is the substrate, direct-only `SinkOnly` retains no
queryable document identity, SIMD mask streams may be transient producers, and
retained structural offsets are the tape rather than a sidecar
(`restart/locks/LOCKS.md:52`). PASS-2 CH5 rejects a candidate that proposes a
parallel substrate, sidecar producer, renamed scanner, second source scan,
retained cursor, aux density table, or parser-owned structural projection
(`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`:131`).

## Findings

### 1. The retained runtime substrate is currently one tape. ACCEPT.

The code matches the single-retained-substrate claim. `Tape` owns one source
pointer, one offset vector, sparse flag vectors, payload arena, and `TapeId`
(`skinny/crates/runtime/src/tape/mod.rs:94`-`:100`). `ValueRef` stores `&Tape`
plus a cursor (`skinny/crates/runtime/src/tape/mod.rs:175`-`:181`), and
`TapeBuilder` writes offsets plus sparse flags before sealing one `Tape`
(`skinny/crates/runtime/src/tape/assembler.rs:42`-`:48`,
`:61`-`:67`, `:115`-`:123`).

Generated retained JSON parsing still emits offsets through `ParserState` and
`TapeBuilder` (`skinny/crates/runtime/src/grammars/json/parser.rs:35`-`:42`;
`skinny/crates/runtime/src/grammars/json/generated.rs:292`-`:305`). Views then
derive JSON node kind from `source[offset]`, not from a retained class sidecar
(`skinny/crates/runtime/src/grammars/json/value.rs:29`-`:45`). This is Lock
1-honest today.

### 2. Capacity pre-scans are the open Lock 1 edge. REVISE.

P2-D's C1 says capacity/flag work "may not add a retained class column, sidecar
bitmap, structural index, or second source pass" (`p2d-substrate-tape.md:30`).
That is the right rule, but the current implementation still has env-selected
capacity plans that perform a source pass before parsing: `ParserState::new`
calls `structural_capacity_for` (`skinny/crates/runtime/src/grammars/json/parser.rs:16`-`:23`),
and `structural_capacity_for` can run `exact_structural_count(source)` or
`scan_structurals(source)` for `Exact` and `OneShotSimd`
(`skinny/crates/runtime/src/grammars/json/scan.rs:47`-`:52`).

Those paths are not retained sidecars, but CH5 also rejects "a second source
scan." S-P3 must not use `BBNF_CAPACITY_PLAN=B/C` or the structural scan
capacity path as product evidence unless it is explicitly classified as
diagnostic telemetry. The fold is simple: state that `GrowOnly` is the
admission/default production plan for row movement, and any capacity pre-scan is
non-admission evidence until a later accepted plan changes the CH5 rule.

### 3. "Same sink event stream" is too broad. REVISE.

P2-D C3 describes the admissible container-walk target as "same source cursor,
same sink event stream, no retained sidecar" (`p2d-substrate-tape.md:42`-`:45`).
The "no retained sidecar" part is correct. The "same sink event stream" part is
not currently a single concrete stream across planes:

- Retained parse emits tape offsets, and retained views infer token kind from
  source bytes (`skinny/crates/runtime/src/grammars/json/generated.rs:292`-`:305`;
  `skinny/crates/runtime/src/grammars/json/value.rs:29`-`:45`).
- Generated direct `SinkOnly` calls `JsonSink` callbacks from
  `parse_direct`, `parse_object_direct`, and `parse_array_direct`
  (`skinny/crates/runtime/src/grammars/json/generated.rs:409`-`:443`,
  `:548`-`:575`, `:582`-`:604`).
- Real typed direct uses a separate generated `DirectParser` that constructs
  typed structs directly, with no `JsonSink` trait in the path
  (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:31`-`:95`,
  `:1183`-`:1205`).

S-P3 must replace the abstract phrase with a consumer matrix that names the
exact consumer per plane. A direct digest consumer, a typed direct field writer,
and retained `TapeBuilder` are not interchangeable evidence for one another.

### 4. The string primitive candidates hide a known cap and plane split. REVISE.

P2-E correctly observes that generated direct, retained, and typed direct string
loops duplicate a grammar-neutral operation (`p2e-parse-that-gaps.md:16`). The
same section then proposes shared direct/typed consumers for
`bounded_plain_string_end` (`p2e-parse-that-gaps.md:28`-`:35`). The hidden
coupling risk is that these are not currently one call site:

- Retained generated parse uses `match_tiny_plain_string_with_cap::<16>`, while
  generated direct uses `::<8>` (`skinny/crates/runtime/src/grammars/json/generated.rs:159`-`:168`).
- Typed direct has `tiny_plain_string_end` at cap 32 and `skip_plain_string_end`
  at cap 96 (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1344`-`:1370`;
  template source `skinny/crates/codegen/src/typed_direct.rs:634`-`:660`).
- REDRESS already records that applying cap 16 globally regressed direct guard
  rows, and that the admitted split is retained cap 16 while generated direct,
  hand retained Track 2, and hand direct Track 2 use cap 8
  (`skinny/REDRESS.md:2045`-`:2053`).

The fold must require a per-call-site scalar oracle and microbench:
`parse_direct` cap 8, typed `parse_string` cap 32, typed skip cap 96, retained
parse excluded unless explicitly named, and Track 2 hand paths kept separate.
No retained or typed result may authorize a direct row, and no direct result may
authorize typed row movement.

### 5. Direct-vs-typed plane honesty is mostly present, but one bridge needs a hard fence. REVISE.

The packet repeatedly says direct digest is not typed proof
(`p2d-substrate-tape.md:35`-`:40`; `p2f-grammar-neutral.md:51`-`:55`), and
SK-V10 synthesis pre-blocks direct digest relabeling as typed product proof
(`restart/skinny/tranches/sk-v10/SYNTHESIS.md:120`-`:130`). Keep that.

The ambiguity is in output-plane bridge language such as P2-E's
`string_segments_fold`, where the same row says the consumer could be "a direct
product consumer such as digest fold or typed owned field writer"
(`p2e-parse-that-gaps.md:33`), and P2-A's direct SAX-style sink contract groups
"typed digest/typed-product events" under one sink-style shape
(`p2a-sota-teardown.md:35`). The code surfaces are not equivalent. Direct digest
parity requires exact Track 1 == Track 2, but only shape equality against serde
and sonic (`skinny/crates/bbnf-bench/src/direct_struct.rs:420`-`:425`).
Typed direct admission constructs real generated product structs
(`skinny/crates/bbnf-bench/src/generated_real_typed.rs:31`-`:95`).

Required fold: a `direct_to_struct` digest improvement moves only direct rows.
A `real_typed_struct` row moves only with a generated typed output, serde/sonic
typed comparator, full-fixture parity/checksum, same-run run id, and its own
REDRESS entry.

### 6. Sidecar freshness and telemetry are safely gate-only. ACCEPT WITH PRESERVATION.

The handoff says same-run sidecar freshness is "Gate-only evidence ingestion; no
behavior movement by itself" (`restart/skinny/tranches/sk-v10/HANDOFF.md:55`-`:60`).
P2-F repeats that comparator/telemetry refresh has no behavior output
(`p2f-grammar-neutral.md:30`-`:33`) and rejects sidecar freshness as a producer
(`p2f-grammar-neutral.md:47`, `:58`). This is CH5-correct.

Preserve the wording in S-P3: sidecar freshness can improve provenance, not
produce parser data, a substrate, a row movement, or strict admission by itself.

### 7. Structural cursor routes remain correctly blocked. ACCEPT WITH PRESERVATION.

P2-E names `structural_cursor_from_movemask` as a non-candidate and says future
classification may be transient only inside an existing string/number caller
(`p2e-parse-that-gaps.md:38`). P2-D and P2-B also reject sidecar scans, union
substrate, parser-owned class tables, decoded scratch, and parse-only SOTA
claims (`p2d-substrate-tape.md:65`-`:69`; `p2b-dav1d-process.md:157`-`:160`).
This closes the obvious W3 reopening path.

The required preservation is exact: no retained structural offsets/classes for
parse closure, no parser-owned structural cursor, no sidecar event vector, and
no W3-as-consumer framing.

## Required Fixes

1. In P2-D C1/S-P3 carry-forward, add: `BBNF_CAPACITY_PLAN=exact|oneshot-simd`
   and any `scan_structurals` capacity pre-scan are diagnostic/env-only unless a
   later accepted CH5 change explicitly admits a second source pass. Row
   movement uses the default one-pass production plan.

2. Replace P2-D C3's "same sink event stream" with a per-plane matrix:
   retained `TapeBuilder` offsets; generated direct `JsonSink` callbacks;
   real typed generated `DirectParser` field writers; Track 2 direct hand
   parser. Each candidate must name exactly one or more of these consumers.

3. Add a string primitive consumer table to P2-E/S-P3: retained cap 16 excluded
   unless explicitly targeted; generated direct cap 8; typed direct parse cap
   32; typed direct skip cap 96; hand Track 2 cap 8. Each cap gets its own scalar
   oracle, microbench, corpus rows, and failure threshold.

4. Add a direct-vs-typed row-movement rule: digest-plane parity cannot move
   typed rows; typed product parity cannot move direct digest rows; shared
   parse-that or SIMD primitives must be re-proved at each plane's caller.

5. Preserve sidecar and structural-cursor refusals in S-P3: telemetry and
   sidecar freshness are gate-only, structural masks are transient, and any
   retained structural projection outside the tape is CH5 REJECT.

## Final Disposition

REVISE. The cohort does not currently introduce a retained sidecar, but S-P3
would be able to smuggle coupling through capacity pre-scans, a vague "same sink
event stream" phrase, and shared direct/typed primitive labels. Fold the five
fixes above before S-P2 can be accepted under the Lock 1 lens.
