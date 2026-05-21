# SK-V13 P2-D: Substrate + Tape Design

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-21.
Scope: substrate/tape interrogation under Lock 1: lazy materialization,
tape ratios, structural projection union, union variants distinct from
REDRESS 96/97/98, and same-substrate tape-shape changes that could move
S-P1 hot leaves without a parallel substrate.
Output: this file.
P1 hot-leaf antecedents: `dispatch_value`; `parse_object_value_at_direct`;
`parse_array_element_at_direct`; `match_tiny_plain_string_with_cap`;
`read_hex_unit_scalar`; `unescape_string`; `scan_tail`; `scan_structurals`;
`bulk_emit_positions_64_neon`; CSS `LocalFactSink::finish` /
`FactSink::finish` timer-dominated profile.
Lock surface: Lock 1 and Lock 14.

## §1 - Findings

### F1. The current retained substrate is a lazy offset tape, not a payload tape.

The live tape surface stores `source`, `offsets: Vec<u32>`,
`flag_cursors`, `flag_values`, `payloads`, and `id`
(`skinny/crates/runtime/src/tape/mod.rs:94-101`). `ValueRef` is a
cursor into that single `Tape` and carries an event-grammar phantom,
not another storage surface (`skinny/crates/runtime/src/tape/mod.rs:175-181`).
`TapeBuilder` writes offsets directly into spare capacity before sealing
(`skinny/crates/runtime/src/tape/assembler.rs:61-85`) and stores sparse
flags separately (`skinny/crates/runtime/src/tape/assembler.rs:93-113`).

The SK-V12 close table reports `0 payload bytes` for every JSON corpus
and `0/0` payload arena writes/allocations for both tracks wherever
reported (`skinny/RESULTS.md:99-144`). Therefore P2-D does not surface
a payload-arena optimization candidate. The measurable substrate levers
are offset count, sparse flag bytes, allocated capacity, and how parser
events are projected into the offset stream.

### F2. Lazy materialization ratios show capacity/tape-shape headroom, but not a standalone SOTA route.

The retained tape ratios are uneven:

| Corpus | Offsets | Logical + flag ratio | Allocated tape ratio | Substrate signal |
|---|---:|---:|---:|---|
| `y_string_unicode` | 2,202 | 0.50x input | 0.75x input | highest allocation ratio, mostly sparse flags and quotes |
| `mesh` | 80,250 | 0.44x input | 0.72x input | numeric/array-heavy offset stream |
| `marine_ik` | 359,563 | 0.48x input | 0.70x input | largest offset count; number-dense |
| `update_center` | 35,281 | 0.27x input | 0.49x input | string-heavy with flag bytes |
| `canada` | 223,236 | 0.40x input | 0.47x input | number/array-heavy |
| `unicode_escapes` | 11,274 | 0.05x input + 9,385 flag bytes | 0.07x input | low offset volume, escape flags dominate |
| `gsoc-2018` | 41,714 | 0.05x input + 8,545 flag bytes | 0.08x input | sparse offset volume |

Sources: `skinny/RESULTS.md:100-144`.

This points to a narrow capacity-policy candidate for over-allocated
rows, especially `y_string_unicode`, `mesh`, and `marine_ik`. It does
not justify a second structural list or eager payload materialization:
the rows with the largest parse/direct SOTA gaps are dominated by
generated dispatch envelopes, unicode/string decode, or output-plane
work in S-P1, not by an exposed payload writer
(`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:54-74`;
`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:104-117`).

### F3. Structural SIMD is a strong scanner micro-signal, but REDRESS 96/97 falsified retaining it as a side structure.

The JSON structural scanner owns `scan_structurals`, which returns a
`StructuralIndex` of positions (`skinny/crates/runtime/src/grammars/json/scan.rs:22-30`;
`skinny/crates/bbnf-simd/src/lib.rs:71-97`). The generated parser still
calls `attach_structural_index`, but that function only asserts the
configured alphabet and drops the state (`skinny/crates/runtime/src/grammars/json/generated.rs:10-15`).
Default parsing constructs capacity through `ParserState::new` and then
uses scalar `consume_structural` to emit offsets
(`skinny/crates/runtime/src/grammars/json/parser.rs:16-24`;
`skinny/crates/runtime/src/grammars/json/generated.rs:290-304`).

S-P1 mode III shows structural SIMD beating scalar scan on all 17
corpora, with ratios from 1.49x to 5.04x and the strongest rows at
`mesh`, `canada`, and `numbers`
(`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:63-81`;
`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:112-118`). That is a scanner candidate only. It does
not reopen a retained SIMD-position vector, a streaming cursor, a class
column, or a parser-owned structural cursor. REDRESS 96 and 97 were
correctness-green, parity-green, and then failed every W3 must-improve
row and every W10b maintain row (`skinny/REDRESS.md:2795-2848`;
`skinny/REDRESS.md:2850-2906`). REDRESS 98 records the empirical
finding: consuming the SIMD index added memory traffic and cursor
indirection that the scalar path did not pay (`skinny/REDRESS.md:2910-2940`).

### F4. Direct-to-struct is `SinkOnly`; its legal union with tape is conceptual, not a retained second document.

Lock 1 explicitly permits direct-only `SinkOnly` with no queryable
document identity and forbids parallel substrates (`restart/locks/LOCKS.md:52`).
The generated direct path calls `parse_direct`, uses a `JsonSink`, and
does not construct a retained `Tape` (`skinny/crates/runtime/src/grammars/json/generated.rs:407-462`;
`skinny/crates/runtime/src/grammars/json/sink.rs:4-120`). That is legal
under Lock 1 only because it is sink-only, not a second retained
document. The retained parse path still seals a `JsonRoot` over one
`Tape` (`skinny/crates/runtime/src/grammars/json/parser.rs:27-51`;
`skinny/crates/runtime/src/grammars/json/view.rs:12-35`).

S-P1 direct hot leaves are generated direct envelopes on most rows
(`parse_object_value_at_direct` and `parse_array_element_at_direct`),
while `unicode_escapes` exposes `parse_that_regex::unescape_string`
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:32-50`).
Therefore a direct-row substrate candidate must be a generated
sink-event shape change or a same-tape/sink-only projection change, not
a retained tape walk or a new direct struct tree.

### F5. Current grammar-neutral substrate surface is incomplete.

The as-shipped config surface is `pub(crate)` metadata, not a public
`GrammarConfig` trait (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:11-36`).
It resolved the structural alphabet leak, but dispatch, string policy,
number policy, key-pair policy, `OffsetFlags`, and `JsonSink` remain
partial or unresolved (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:63-117`).
The live source confirms the two substrate-relevant leaks: `OffsetFlags`
names JSON bits (`skinny/crates/runtime/src/tape/mod.rs:16-36`) and
`JsonSink` is JSON-specific (`skinny/crates/runtime/src/grammars/json/sink.rs:4-120`).

Any P2-D candidate that puts more semantics into the tape must first
make the flag/event interpretation grammar-private or generated. A
generic-crate `match grammar { Json => ... }`, public `UnionTape`, new
BIR variant, or new `BackendShape` violates Lock 14 and the current
SK-V13 handoff refusal conditions (`restart/locks/LOCKS.md:78`;
`restart/skinny/tranches/sk-v13/HANDOFF.md:69-84`).

### F6. A legal "structural projection union" is a same-tape projection.

Lock 1 says that if structural offsets are retained, that projection is
the tape, not a sidecar (`restart/locks/LOCKS.md:52`). The SK-V13 value
scoping agrees: union selection can be legal only if it is codegen-private,
keeps `Tape`, `ValueRef`, and `TapeBuilder` stable, and has no `UnionTape`,
sidecar vectors, parser-owned cursor/list, new directive, new BIR variant,
or new `BackendShape` (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:242-301`).

This is the load-bearing P2-D conclusion: the admissible substrate
space is not "retain the scanner output." It is "change how generated
code emits or consumes the single tape/sink event projection, with a
same-wave consumer and a row-moving gate."

## §2 - Candidate primitives

P2-D does not select waves. It surfaces candidate primitives and
substrate shapes for S-P3 to shortlist only if CHALLENGE accepts the
scalar-reference, parity/checkasm, same-wave-consumer, and Lock 1
constraints below.

### D1. Lazy tape capacity policy tuning

**Shape.** Replace the fixed `GrowOnly` production default or its
growth heuristic with a grammar-neutral capacity policy selected from
observed structural density and generated grammar metadata. The
smallest first test is not a second scan; it is a `TapeBuilder`
capacity/growth tweak over the same offset stream.

**P1 antecedent.** High allocated/logical tape ratios on
`y_string_unicode`, `mesh`, `marine_ik`, `update_center`, `canada`,
and `numbers` (`skinny/RESULTS.md:105-144`); mode-III structural
capacity signals
(`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:63-81`).

**Scalar reference.** Existing `CapacityPlan::GrowOnly` and
`TapeBuilder::push_plain_offset` are the scalar/control reference
(`skinny/crates/runtime/src/tape/assembler.rs:13-40`;
`skinny/crates/runtime/src/tape/assembler.rs:61-91`).

**Checkasm / parity.** No ISA checkasm unless the policy consumes a SIMD
count. Required parity is tape materialization equality:
offset count, sparse flags, `ValueRef` traversal, and canonical output
must match the current retained parse. If a SIMD count is introduced,
its count oracle must be a checkasm-style scalar differential before
being used for capacity.

**Same-wave consumer.** `ParserState::new` in retained JSON parse
(`skinny/crates/runtime/src/grammars/json/parser.rs:16-24`) and an
equivalent generated CSS parser constructor if CSS gets retained rows.
The gate must measure parse rows where over-allocation is visible
(`y_string_unicode`, `mesh`, `marine_ik`, `canada`) and guard all
previous JSON admits.

**Eligibility.** Medium-low. It is legal and cheap, but likely moves
memory footprint more than throughput. It must micro-prove row movement
before S-P3 treats it as a behavior wave.

### D2. GrammarConfig per-rule same-tape event projection (Union C1 narrowed)

**Shape.** Generate a per-rule substrate policy inside per-grammar
config/codegen-private data, then emit one of a small number of
same-tape projections for that rule: offset-only, offset-plus-fact
flag, or sink-only collapsed event. It must not introduce a public
`UnionTape`; all retained output still seals one `Tape<'input>`.

**Material differential vs REDRESS 96/97/98.** REDRESS 96 added a
class column; REDRESS 97 streamed a cursor/class tuple; REDRESS 98 left
the producer absent. D2 is codegen-time monomorphic routing with no
retained class side vector and no runtime cursor list
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:318-347`;
`skinny/REDRESS.md:2795-2940`).

**P1 antecedent.** JSON direct generated envelope dominance on
`twitter`, `github_events`, `update_center`, `canada`, `mesh`,
`numbers`, and `gsoc-2018`
(`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:66-83`);
CSS needs full stylesheet/selector rows where rule context matters
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:153-179`).

**Scalar reference.** Current fixed offset tape plus generated
`ValueRef` traversal for retained parse and current `JsonSink` for
sink-only direct (`skinny/crates/runtime/src/grammars/json/view.rs:42-60`;
`skinny/crates/runtime/src/grammars/json/generated.rs:407-462`).

**Checkasm / parity.** Non-ISA path: differential parity over offset
stream, token stream, `ValueRef` traversal, direct digest, and CSS fact
stream. If any SIMD producer feeds the projection, Lock 16 checkasm
applies to the producer first (`restart/locks/LOCKS.md:87-112`).

**Same-wave consumer.** Best first consumer is a generated CSS
stylesheet/selector fact stream because CSS requires rule context and
has a lightningcss strict oracle. JSON direct may be a guard/secondary
consumer only if the row gate measures actual direct throughput
movement, not just tape shape existence.

**Eligibility.** High for S-P3 consideration if the wave also owns the
CSS or JSON consumer. It is the cleanest category-level D3 union route
that is materially distinct from REDRESS 96/97/98.

### D3. SIMD mask-to-tape writer (Union C3, high-risk)

**Shape.** Use a grammar-supplied structural alphabet to create a
64-byte mask, then write selected positions directly into the active
`TapeBuilder` offset stream in source order. The producer may use
PMULL/CSSC/CTZ/bulk-position emission only if it never exposes a
retained `StructuralIndex` vector to the parser and never creates a
class sidecar.

**Material differential vs REDRESS 96/97/98.** Prior W3 attempts
materialized or streamed structural projection separately from the
tape-consuming scalar loop. D3 makes the mask-to-offset write itself
the tape producer: if the projection is retained, it is the tape.
This also differs from REDRESS 88/89 because PMULL/CSSC would be
admitted only behind a same-wave tape consumer, not as a default
support body (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:123-136`;
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:156-170`).

**P1 antecedent.** Structural SIMD/scalar ratios are positive on all
17 rows; strongest ratios are `mesh` 5.04x, `canada` 5.01x, and
`numbers` 4.96x
(`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:63-81`;
`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:112-118`).

**Scalar reference.** Existing `scan_tail` / `scan_tail_byte` and
current `consume_structural` emission
(`skinny/crates/runtime/src/grammars/json/scan.rs:107-161`;
`skinny/crates/runtime/src/grammars/json/generated.rs:290-304`).

**Checkasm / parity.** Required. Scalar-vs-SIMD mask parity for
quote/backslash carry, all alignments, tails, dense/sparse masks,
invalid/error cases, and the direct writer's output order. Existing
bulk/prefix/next-bit tests show the test pattern but the current
aarch64 bodies are scalar delegates
(`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`;
`skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`;
`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`).

**Same-wave consumer.** A retained parse row or CSS row must consume
the emitted tape in the same wave. A scanner microbench is insufficient.
Gate candidates: parse-only rows now re-pinned under the full-SOTA
addendum, or a CSS selector/stylesheet row if the structural alphabet
and skip/comment policy are grammar-configured.

**Eligibility.** High reward but high risk. It should not be shortlisted
unless S-P3 can give it a micro-prove-first gate and a same-wave row
consumer. It must explicitly carry REDRESS 96/97/98 and REDRESS 88/89
material differentials.

### D4. SinkOnly event adapter for direct rows

**Shape.** Generate a grammar-specific direct sink trait and a compact
event adapter that shares semantic event definitions with retained tape
without building a retained document. For JSON this replaces the
handwritten `JsonSink`-owned shape; for CSS it becomes the required
fact-stream sink. This is a direct/tape union because the same generated
event vocabulary feeds retained tape or sink-only output depending on
the row plane.

**P1 antecedent.** Direct hot leaves are generated sink envelopes on
most rows
(`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:66-83`)
and ten typed product rows are missing generated surfaces
(`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:48-55`).
The current
`JsonSink` hardcoding is an unresolved Lock 14 leak
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:70-71`).

**Scalar reference.** Current `JsonSink` direct digest and independent
Track 2 direct parser (`skinny/crates/runtime/src/grammars/json/sink.rs:4-120`;
`skinny/crates/bbnf-bench/src/direct_struct.rs:15-29`).

**Checkasm / parity.** No ISA checkasm unless a SIMD digest/string
primitive is included. Required parity: strict same-plane direct digest
equality across generated Track 1, independent Track 2, serde_json, and
sonic-rs strict for JSON; CSS fact-stream equality against lightningcss
and an independent oracle for CSS.

**Same-wave consumer.** A direct row that currently misses SOTA or a new
CSS parity row. D4 is not admissible as "new sink infrastructure" alone;
it must move at least one direct/CSS row or record an architectural
block on the touched row family.

**Eligibility.** High for JSON direct and CSS parity planning, but it
belongs partly to P2-F/Lock 14 and P2-E primitive gaps. P2-D's boundary
is the event vocabulary/tape union, not the string/number primitive.

### D5. Grammar-neutral sparse flag policy

**Shape.** Replace JSON-named `OffsetFlags::HAS_ESC` /
`OffsetFlags::HAS_CONTROL` semantics with generated per-grammar flag
interpretation while keeping the physical sparse flag storage stable.
The generic tape sees raw bits; generated grammar views decide whether
bit 0 means JSON escape, CSS escaped identifier, Sheets doubled quote,
or another grammar-local fact.

**P1 antecedent.** Unicode/string rows expose flag-heavy and decode-heavy
work: `unicode_escapes` direct is rank-1 `unescape_string`, while
`y_string_unicode`, `unicode_escapes`, `unicode_mixed`, and
`gsoc-2018` carry substantial sparse flag bytes in `skinny/RESULTS.md`
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:47`;
`skinny/RESULTS.md:123-144`).
CSS expansion also requires non-JSON escape policy
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:196-214`).

**Scalar reference.** Current `OffsetFlags` storage plus
`JsonString::as_str` flag interpretation
(`skinny/crates/runtime/src/tape/mod.rs:144-158`;
`skinny/crates/runtime/src/grammars/json/view.rs:200-217`).

**Checkasm / parity.** Non-ISA parity: raw offset stream and flag bytes
must match expected grammar-local facts; JSON canonical string output
must be unchanged. If a SIMD escape classifier feeds flags, its
classifier gets Lock 16 checkasm.

**Same-wave consumer.** Must be paired with a CSS row that exercises
escaped identifiers/strings or a JSON unicode row that improves
decode/flag consumption. As a support-only generic cleanup it fails the
addendum's per-wave row-movement rule.

**Eligibility.** Required legality groundwork for CSS/full grammar
parity; only row-moving when paired with D4 or a string/unicode
primitive.

### Non-candidates P2-D excludes

- A retained `StructuralIndex` vector, class column, streaming cursor,
  aux density table, whitespace bitmap, or `UnionTape`: rejected by Lock
  1 and REDRESS 96/97/98 unless a new material differential turns the
  projection into the tape itself.
- Payload arena widening or eager decoded payload storage: `skinny/RESULTS.md`
  reports zero payload bytes on the current JSON materialization rows.
- Track 2 parser rewrites as substrate proof: Track 2 is an independent
  oracle; it cannot justify Track 1 substrate claims without Track 1
  row movement.

## §3 - Grammar-neutrality

| Candidate | Grammar-neutral verdict | CSS / Sheets / BBNF-self route | Lock 14 notes |
|---|---|---|---|
| D1 lazy capacity policy | Generalisable if driven by grammar metadata and observed density, not JSON names. | CSS retained rows can use the same policy once they retain a tape; Sheets array formulas likely stress capacity similarly. | No generic `Json` branches; capacity knobs live in generated config or cost facts. |
| D2 per-rule same-tape event projection | Generalisable if the event/fact vocabulary is generated from grammar metadata. | CSS selectors/rules and Sheets cells can emit different fact IDs over the same physical tape. | No new directive/BIR/`BackendShape`; no public substrate API; codegen-private policy only. |
| D3 SIMD mask-to-tape writer | Generalisable at the byte-set/mask level; grammar-specific at the alphabet/quote policy layer. | CSS may consume delimiter/comment/selector alphabets; Sheets may consume delimiter/schema bytes. | SIMD classifier must accept grammar-supplied alphabets; no hardcoded JSON punctuation in generic crate. |
| D4 SinkOnly event adapter | Generalisable if sink traits are generated per grammar from the same event vocabulary. | CSS fact stream is the first non-JSON consumer; Sheets direct cells can be a later consumer. | Current `JsonSink` is a leak; generated `GrammarSink` per grammar must not become a public generic crate arm. |
| D5 sparse flag policy | Generalisable if bits are physical only and semantics are generated per grammar. | CSS escaped identifiers and Sheets doubled quotes can share sparse storage with different interpretations. | Must remove or wrap JSON-named `HAS_ESC` / `HAS_CONTROL` from generic semantics. |

Grammar-neutrality is measured, not asserted. A candidate that only
moves JSON without a CSS/Sheets/BBNF-self proof must be labeled
JSON-only and routed to P2-F/S-P3 for either a paired non-JSON consumer
or rejection. The S-P1 ledger's quarantine still applies: JSON parse
envelopes, JSON direct envelopes, JSON typed leaves, and CSS timer
samples are profile signals, not gate admissions
(`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:8-23`).

## §4 - Risks

1. **REDRESS 96/97/98 union history.** Any union route must cite the
   exact prior failures and name a material differential. REDRESS 96
   failed with a class column plus move-consumed structural vector;
   REDRESS 97 failed with an allocation-free streaming cursor; REDRESS
   98 retired the SK-V9 union gate and explains the wide-core cost model
   (`skinny/REDRESS.md:2795-2940`). Repeating "consume the scanner
   output" is a REJECT.

2. **REDRESS 92 scanner/tape non-isomorphism.** The scanner keeps
   structural punctuation plus real quotes; the retained tape is a
   parser event stream of container opens/closes, opening quotes,
   numbers, and literal starts (`skinny/REDRESS.md:2661-2690`). D3 must
   prove semantic isomorphism before writing scanner positions into a
   tape.

3. **REDRESS 119/120 direct fixpoint history.** The addendum lifts the
   fixpoint as close authority, but not as evidence that profile signals
   are admissions. Every direct reopen must cite the prior row and name
   the new material differential (`skinny/REDRESS.md:3497-3553`;
   `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:52-75`).

4. **REDRESS 88/89/126 SIMD/orphan history.** PMULL/CSSC categories are
   unblocked, but the historical implementations remain measured
   failures or demoted inventory. The five aarch64 orphan files exist
   but are scalar delegates or support-only; no new primitive may ship
   without a scalar reference, checkasm/parity, same-wave consumer, and
   zero-orphan disposition
   (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:14-23`;
   `skinny/REDRESS.md:3798-3820`; `skinny/REDRESS.md:3860-3872`).

5. **Lock 14 flag/sink leaks.** D2/D4/D5 cannot be implemented through a
   generic public `GrammarConfig` or generic-crate grammar match. Current
   `OffsetFlags` and `JsonSink` are still JSON-presupposing
   (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:63-117`).

6. **No branch/cache overfit.** S-P1 could not extract branch/L1/LLC
   counters from current xctrace export; those fields are unavailable,
   not zero
   (`restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:43-55`;
   `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:106-113`).
   Tape-shape candidates may use cycles/byte and row Mbps, but not
   invented cache-miss explanations.

7. **CSS profile is not a parser-hot-leaf proof.** The current CSS
   declaration-values profile is timer/fact-sink dominated
   (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100-104`;
   `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:77-87`).
   CSS row candidates need fresh narrower profiling or strict equality
   plus throughput gates; P2-D cannot infer a CSS substrate bottleneck
   from the existing CSS sample.

## §5 - Sources

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
- `restart/locks/LOCKS.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs`
