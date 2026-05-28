# SK-V15 P2-D: Substrate + Tape Design

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-28.
Scope: offset-tape substrate, lazy materialisation counters, logical-vs-allocated tape ratios, and structural-projection union.
Output: this file.
P1 hot-leaf antecedents: `allocation/tape materialization`, `generic direct-product parser cursor`, `generic string/whitespace scanner`, `unicode/string validation`, `grammar-neutral structural scanner wrapper`, `NEON byte-compare scanner primitive`, and product-plane c/B misses on `mesh` and `unicode_escapes`.
Lock surface: Lock 1 and Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. **Tape and structural projection are one substrate.** Lock 1 states that retained structural offsets are the tape, not a sidecar, and rejects parser-owned cursors, retained class streams, public `UnionTape`, or a second tape unless G-Omega amends the lock (`restart/locks/LOCKS.md:75`-`127`). The live skinny retained document matches that: `Tape` owns source, one `Vec<u32>` offset stream, sparse flag cursor/value vectors, payload arena, and id (`skinny/crates/runtime/src/tape/mod.rs:94`-`101`); `ValueRef` is only `&Tape + cursor` (`skinny/crates/runtime/src/tape/mod.rs:175`-`222`). Therefore P2-D concludes the structural projection and tape are a single substrate. S-P3 must not shortlist a parallel retained structural index, cursor list, class lane, whitespace bitmap, aux density table, or second document projection.

2. **The live offset tape is lazy and source-derived.** `TapeBuilder` writes offsets directly into spare capacity and stores flags sparsely (`skinny/crates/runtime/src/tape/assembler.rs:61`-`113`). JSON kind lookup is derived from `source[offsets[cursor]]`, not from a stored node-kind column (`skinny/crates/runtime/src/grammars/json/value.rs:28`-`47`). The view layer lazily derives string spans, number spans, subtree spans, and token streams from the same tape (`skinny/crates/runtime/src/grammars/json/view.rs:42`-`49`, `:333`-`:451`). This is the substrate to tune; there is no payload-token stream to resurrect.

3. **Lazy materialisation counters argue for capacity/flag economy, not a new substrate.** The materialisation report records offset count, logical offset bytes, sparse flag bytes, allocated tape bytes, and zero payload bytes from the sealed `JsonRoot` tape (`skinny/crates/bbnf-bench/src/materialization.rs:20`-`100`). The historical current bench fact table shows:

| Corpus | Logical bytes | Allocated bytes | Allocated/logical | Signal |
|---|---:|---:|---:|---|
| `twitter` | 119,852 | 133,632 | 1.11x | sparse flags visible, modest capacity waste |
| `citm_catalog` | 340,145 | 524,312 | 1.54x | capacity-policy pressure |
| `canada` | 892,944 | 1,048,576 | 1.17x | dense numeric/array offsets, no flags |

Source rows: `skinny/REDRESS.md:84`-`91`. These numbers interrogate logical-vs-allocated tape economy, but the same REDRESS file says remaining parse misses are runtime/materialisation and event/tape-consumption gaps, not scanner-floor failure (`skinny/REDRESS.md:63`-`67`). A candidate may reduce over-allocation only if offset/flag parity and row throughput move together.

4. **Structural SIMD remains a producer signal, not a retained output plane.** The JSON scanner can return `StructuralIndex` positions (`skinny/crates/runtime/src/grammars/json/scan.rs:22`-`35`), but generated retained parsing currently drops `attach_structural_index` on entry (`skinny/crates/runtime/src/grammars/json/generated.rs:10`-`15`) and emits offsets from the parse loop. P1-C classifies structural-scan profiles as masking evidence and warns not to mistake hash/checksum or wrapper leaves for parser wins (`restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md:37`-`45`). Any SIMD structural candidate must write the active tape or feed a same-wave sink/fact consumer; retaining the scanner output is a Lock 1 reject.

5. **P1 antecedents are real but mixed.** P1-E resolves 119 profiled rows and marks allocation/tape, cursor/skip/ws, unicode/string, and structural-scan wrappers as the S-P2 antecedent surface (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31`-`33`). It also says parse-only vector-frame operations are harness materialisation, not JSON-specific primitives (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:42`-`42`). P1-D adds the current c/B miss boundary: direct strict misses on `mesh` and `unicode_escapes`, and real typed misses on `unicode_escapes` (`restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:70`-`73`). P2-D can therefore nominate same-substrate tape/projection candidates, but it must not turn harness materialisation into parser proof.

6. **Lock 14 makes the generic tape opaque.** Lock 14 forbids grammar-named policy in generic crates and requires grammar-specific meanings to come from generated grammar facts/config (`restart/locks/LOCKS.md:349`-`390`). The current JSON config uses generic `GRAMMAR_BIT0` for string decode flags and exposes the Lock-1 triad as generated constants (`skinny/crates/runtime/src/grammars/json/config.rs:4`-`30`). A tape/projection candidate is grammar-neutral only if generic runtime stores offsets/opaque flags/facts while generated grammar code interprets them.

7. **REDRESS 96/97/98 are pre-blocks for retained union-substrate repeats.** The prior union thesis was measured twice: a full class-column substrate plus move-consumed `scan_structurals` vector, and an allocation-free streaming cursor. Both were correctness/parity green and then missed every must-improve/maintain row (`skinny/REDRESS.md:2910`-`2922`). The surviving empirical finding is that scalar delimiter consumption was cheaper than materialising or streaming a SIMD structural cursor through retained parsing on this host (`skinny/REDRESS.md:2928`-`2939`). SK-V15 may only consider materially different same-substrate consumers.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

| Candidate | Shape | Scalar ref | Checkasm/parity | P1 antecedent | Grammar-neutrality | REDRESS risks / verdict |
|---|---|---|---|---|---|---|
| `offset_tape_capacity_policy_v2` | Tune `TapeBuilder` initial capacity/growth and sealing telemetry over the same offset stream using input length, generated metadata, emitted-count feedback, or same-loop accounting only; no second source scan, pre-scan capacity oracle, retained capacity sidecar, or parallel source pass. | Current `CapacityPlan::GrowOnly`, `TapeBuilder::new`, `push_plain_offset`, and `reserve_offsets_cold` (`skinny/crates/runtime/src/tape/assembler.rs:13`-`91`). | No checkasm unless a SIMD body is introduced inside the same loop. Required parity: equal offsets, equal sparse flags, equal `ValueRef` traversal, zero payload writes/allocations, and refreshed materialisation ratios. | `allocation/tape materialization` rows in P1-E plus logical/allocated ratios. | General if keyed to input length, emitted offset count, generated grammar metadata, and retained/sink mode; overfit if keyed to corpus or JSON byte roles in generic code. | Does not reopen 96/97/98 only while capacity-only and same-substrate. A separate pre-scan capacity oracle is CH5 REJECT unless G-Omega amends Lock 1. |
| `sparse_flag_same_tape_access` | Keep per-cursor flags inside `Tape`, but allow generated accessors or representation tuning for lazy string/value facts; no decoded payload sidecar. | Current ordered `flag_cursors`/`flag_values`, `Tape::flags_at`, and JSON `string_needs_decode` (`skinny/crates/runtime/src/tape/mod.rs:130`-`164`; `skinny/crates/runtime/src/grammars/json/config.rs:12`-`20`). | Non-ISA parity over flag cursor/value streams, lazy decode semantics, canonical output, and zero eager payload materialisation. If bit-packing uses SIMD, scalar-vs-SIMD flag parity is mandatory. | `unicode/string validation`, `allocation/tape materialization`, `unicode_escapes` direct/typed c/B miss. | Generic tape sees opaque bits only; generated JSON/CSS/Sheets/BBNF-self accessors own bit meaning. | Must not repeat REDRESS 54/55 string materialiser or REDRESS 60-72 eager materialisation families. S-P3-eligible only with a same-wave lazy consumer. |
| `same_tape_fact_projection` | Generated per-rule fact projection attached to the existing tape or emitted as admitted fact output; retained form is same tape, sink form is `direct_sink`/`admitted_fact_output`. No public `UnionTape`. | Current source-derived `JsonNodeKind::at_cursor`, `token_stream`, retained view traversal, and generated W7 policy triad (`skinny/crates/runtime/src/grammars/json/value.rs:28`-`47`; `skinny/crates/runtime/src/grammars/json/config.rs:22`-`30`). | Parity over token/fact stream, direct product, CSS full-parse/fact oracle where applicable, and JSON guards. No checkasm unless the fact producer uses SIMD. | `generic direct-product parser cursor`, product-plane c/B misses on `mesh`/`unicode_escapes`, and SK-V14 W7 same-substrate policy consumption (`skinny/REDRESS.md:5297`-`5311`). | General as `substrate_target`, `retention_lifetime`, and `policy_owner` facts consumed by generated grammar code. Not general if generic crates branch on JSON/CSS roles. | Materially distinct from 96/97/98 only when the same-wave consumer is generated grammar fact/output work, not retained structural cursor replacement. Candidate, high scrutiny. |
| `mask_to_tape_writer_local` | A SIMD/scalar structural mask producer writes selected positions directly into the active `TapeBuilder` or sink event stream within one generated function; no retained `StructuralIndex` crosses the call boundary. | Scalar `scan_tail`/`scan_tail_byte` and current parse-loop offset emission (`skinny/crates/runtime/src/grammars/json/scan.rs:107`-`161`; `skinny/crates/runtime/src/grammars/json/generated.rs:58`-`155`). | Required if SIMD: scalar-vs-SIMD checkasm/parity for quotes, escapes, tails, dense/sparse masks, source order, and tape offsets. Same-wave consumer must measure row movement. | `grammar-neutral structural scanner wrapper`, `NEON byte-compare scanner primitive`, mode-III structural scan evidence. | General only as alphabet/config-driven byte classification with grammar-provided structural alphabets; JSON punctuation cannot live in generic SIMD policy. | Near REDRESS 96/97/98. Legal only because retained output is the tape itself; reject if it exposes positions/cursors/classes as retained state. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

- `offset_tape_capacity_policy_v2` is grammar-neutral when capacity is derived from generic tape/fact counts and generated metadata. It is not grammar-neutral if the policy embeds JSON/CSS corpus names or punctuation roles outside generated grammar code.
- `sparse_flag_same_tape_access` is grammar-neutral only when generic `Tape` stores opaque bits and generated grammar modules provide typed accessors. JSON escape flags, CSS escape/identifier flags, Sheets formula flags, and BBNF-self token flags must be generated interpretations over the same storage.
- `same_tape_fact_projection` is grammar-neutral as a generated projection contract: `existing_tape`, `direct_sink`, or `admitted_fact_output` with `retention_lifetime` and `policy_owner` declared. It fails Lock 14 if generic code matches grammar names, node roles, or CSS/JSON byte alphabets.
- `mask_to_tape_writer_local` is grammar-neutral only as a byte-set/classifier primitive parameterised by generated grammar alphabets and consumed inside the same generated function. It cannot retain quote/escape/structural state across call boundaries under Lock 1 v+1 (`restart/locks/LOCKS.md:137`-`158`).

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

1. **Substrate split.** Any retained projection independently traversable from `Tape` reopens REDRESS 96/97/98 and fails CH5.
2. **Ratio over-read.** Logical-vs-allocated tape ratios are capacity evidence, not SOTA proof. Smaller allocation without row movement and equality is paper-close.
3. **Harness materialisation confusion.** P1-A/P1-E show vector frame and checksum materialisation in profile output. Those rows can motivate accounting and capacity probes, not parser primitives by themselves.
4. **CSS overfit carry.** SK-V15 demotes CSS as contrived and requires Lock 14/Lock 16 gate repair before grammar-general claims (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:34`-`50`). P2-D candidates cannot use CSS broadcast rows as proof.
5. **Checkasm bypass.** Any SIMD mask/count/write primitive needs scalar reference, parity/checkasm, and same-wave consumer. A local mask producer without these is an orphan kernel.
6. **Opaque fact leakage.** `same_tape_fact_projection` is safe only while generic runtime stores opaque facts and generated grammar code owns interpretation.
7. **Explicit non-candidate class.** Retained structural-position vectors, streaming cursors, class columns, whitespace bitmaps, density/projection tables, decoded-byte sidecars, and public `UnionTape` shapes are not candidates in this packet.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- `restart/prompts/skinny/PASS-2-RESEARCH.md:36`-`58`, `:62`-`:85`, `:119`-`:131`, `:237`-`:240`.
- `restart/skinny/tranches/sk-v15/HANDOFF.md:6`-`18`, `:30`-`:41`.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md:34`-`50`, `:119`-`:127`.
- `restart/locks/LOCKS.md:75`-`158`, `:349`-`:400`.
- `restart/skinny/tranches/sk-v15/research/p1/p1a-samply-mode-1.md:37`-`43`.
- `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:37`-`45`.
- `restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md:37`-`45`.
- `restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:70`-`78`.
- `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:11`-`33`, `:38`-`:42`.
- `restart/skinny/tranches/sk-v15/research/p1/p1f-results-delta.md:21`-`30`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv`.
- `skinny/RESULTS.md:1`-`35`, `:137`-`:155`.
- `skinny/REDRESS.md:10`-`41`, `:63`-`:67`, `:84`-`:91`, `:126`-`:161`, `:246`-`:289`, `:2910`-`:2950`, `:5297`-`:5311`.
- `skinny/crates/runtime/src/tape/mod.rs:94`-`173`, `:175`-`:222`.
- `skinny/crates/runtime/src/tape/assembler.rs:13`-`123`.
- `skinny/crates/runtime/src/grammars/json/parser.rs:7`-`52`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:10`-`155`.
- `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`53`, `:107`-`:161`, `:207`-`:260`.
- `skinny/crates/runtime/src/grammars/json/value.rs:28`-`47`, `:143`-`:172`.
- `skinny/crates/runtime/src/grammars/json/view.rs:42`-`49`, `:333`-`:451`.
- `skinny/crates/runtime/src/grammars/json/config.rs:4`-`30`.
- `skinny/crates/bbnf-bench/src/materialization.rs:20`-`100`.
- `skinny/crates/bbnf-bench/src/parity.rs:23`-`80`.
