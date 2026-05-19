# SK-V9 Wave W3 Research V3: Class-Lane-Only Triage

Inputs: `skinny/REDRESS.md` Items 96 and 97;
`/tmp/skv9-waveW3-rejected.patch`;
`/tmp/skv9-waveW3-v2-rejected.patch`;
`restart/skinny/tranches/sk-v9/SPEC.md` Section 6;
`restart/skinny/tranches/sk-v9/HANDOFF.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-research-v2.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-plan-v2.md`;
`restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v3.md`.

Purpose: identify whether a third W3 implementation route remains inside the
current SK-V9 Section 6 contract after two measured scanner/cursor redress
rejects.

## Research Result

Three read-only research lenses converged:

1. The binding numeric W3 gate remains the four must-improve `parse_only`
   Track 1 rows plus the W10b six-row maintain block. `gate-json` reads
   Criterion slopes from `$CRITERION_HOME/json_<corpus>/<bench>/new/estimates.json`
   and converts with `bytes * 8000 / ns`, but it does not currently enforce
   the W3 floors or consume real profiler self-time for `consume_structural`
   and `JsonNodeKind::at_cursor`. W3 redress must therefore check those floors
   explicitly from Criterion JSON and carry profiler evidence separately.
2. The only mechanically distinct next source shape is an emit-site
   class-lane-only substrate: keep the current parser walk and offset stream,
   write a packed retained class at each existing JSON emit site, and switch
   `JsonNodeKind::at_cursor` to `tape.class_at(cursor)`.
3. That class-lane-only shape is not a valid W3 admission under the current
   SPEC. It leaves the parser's scalar structural discovery in place, carries
   no same-wave SIMD structural producer, does not delete the current
   `consume_structural` control path, and is expected to be neutral or
   negative for `track1_generated` parse-only because the benchmark only parses
   and black-boxes the root; it does not exercise retained view traversal.

## Binding Gate Surfaces

The current W3 numeric floors are:

| Row | Floor |
|---|---:|
| `twitter/parse_only` | 17685 |
| `apache_builds/parse_only` | 14124 |
| `update_center/parse_only` | 14370 |
| `distinct_values/parse_only` | 15731 |
| `canada/parse_only` | 15866 |
| `citm_catalog/parse_only` | 28630 |
| `instruments/parse_only` | 15865 |
| `marine_ik/parse_only` | 11831 |
| `mesh/parse_only` | 12186 |
| `numbers/parse_only` | 17596 |

The sufficient targeted numeric falsifier for any future W3 source candidate is:

```text
RUSTFLAGS="-C target-cpu=native" \
CRITERION_HOME=/tmp/skv9-w3-vNEXT-target \
cargo bench -p bbnf-bench --bench json_parity -- \
'json/(twitter|apache_builds|update_center|distinct_values|canada|citm_catalog|instruments|marine_ik|mesh|numbers)/track1_generated$'
```

If the numeric gate survives, the no-leak guard can be targeted with:

```text
RUSTFLAGS="-C target-cpu=native" \
CRITERION_HOME=/tmp/skv9-w3-vNEXT-guards \
cargo bench -p bbnf-bench --bench json_parity -- \
'json/(twitter|apache_builds|update_center|distinct_values|canada|citm_catalog|instruments|marine_ik|mesh|numbers)/(track2_handcoded|track1_direct_to_struct|track2_direct_to_struct)$'
```

Partial targeted captures are redress-candidate evidence only. A final
accepted wave still needs the SPEC's full measurement and profiler evidence.

## Feasible But Non-Admitting Shape

The emit-site class-lane-only route would touch:

- `skinny/crates/runtime/src/tape/{mod,assembler}.rs` for a mandatory
  co-indexed class lane, `class_at`, `class_bytes`, and
  `push_offset_with_class`.
- `skinny/crates/runtime/src/grammars/json/{parser,generated,value}.rs` and
  `skinny/crates/codegen/src/json_templates/{parser,generated,value}.rs` for
  event-class writes and `JsonNodeKind::at_cursor` class reads.
- `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs` and
  `skinny/crates/runtime/src/tape/event_grammar_tests.rs` because JSON's
  retained event-class domain is nine classes, not the current seven-byte
  structural alphabet.
- `skinny/crates/bbnf-bench/src/{parity,track2/json}.rs` and possibly
  `materialization.rs` so the independent Track 2 oracle emits and compares
  the same class stream.

Its proof obligation is straightforward: for every retained cursor `i`,
`offsets[i]` is byte-identical to the current parser-event stream and
`classes[i]` is the parser-known event class for that same cursor. The proof
keeps `ValueRef` layout-neutral: `ValueRef` remains `&Tape + cursor + phantom
grammar`, and the runtime class is read from `Tape`.

## Why It Cannot Close Current W3

The current SPEC Section 6 admits W3 only if the union substrate removes the
structural rediscovery hot leaf and uses the same-wave structural producer. An
emit-site class lane does neither:

- It is parser-produced, not scan-produced.
- It preserves the old scalar delimiter and whitespace walk.
- It does not consume the aarch64 structural-bitmap producer.
- It cannot plausibly move `track1_generated` parse-only rows upward because
  retained view/source-byte rediscovery happens after the parse-only benchmark
  has already completed.
- It would require Track 2 and Lock 14 authorization changes, which are
  acceptable for a planned wave but not evidence that the current W3 gate can
  be met.

Therefore the class-lane-only route is a candidate for a future SPEC amendment
or preparatory proof slice, not a valid W3 redress under the current
`G-W3-UNION-SUBSTRATE` contract.

## Recommendation

Do not dispatch another W3 source redress against the current Section 6 gate
unless the plan names a materially new parse-only producer that is neither the
REDRESS 96 full-position-vector shape nor the REDRESS 97 streaming-cursor
shape. If no such producer is available, CHALLENGE should reject the V3 plan
before source edits and escalate W3 as a SPEC/blocker decision rather than
recording another predictable measured source reject.
