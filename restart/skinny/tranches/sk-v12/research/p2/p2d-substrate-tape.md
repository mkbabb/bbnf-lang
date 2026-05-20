# SK-V12 P2-D: Substrate + Tape Design
Pass: S-P2 Research. Cycle: V12.
Date: 2026-05-20.
Scope: Interrogate the offset-tape substrate, lazy-materialisation counters, logical-vs-allocated tape ratios, structural-projection union, and where a tape-shape change would move an accepted P1 hot leaf.
Output: this file.
P1 hot-leaf antecedents: `bounded_plain_string_scan`, `string_escape_decode`, `unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, `output_digest_hash`, `typed_direct_projection`, and `serde_json_oracle_read_parse`.
Lock surface: Lock 1 and Lock 14. Tape remains the single retained substrate; structural projection is tape when structural offsets are retained; generic crates stay grammar-neutral.

## §1 — Findings

1. The current retained substrate is already the Lock 1 union, not a sidecar pair.
   `Tape` owns the source, one `Vec<u32>` offset stream, sparse flag cursor/value
   vectors, and the payload arena. `ValueRef` is `&Tape + cursor`, and `JsonRoot`
   owns the `Tape` through the generated document view. `ParserState` owns one
   `TapeBuilder`, emits offsets through that builder, and seals it into `JsonRoot`.
   This matches the REDRESS statement that Track 1 and Track 2 consume the same
   one-buffer tape builder.

2. Structural projection and tape are one substrate in the live JSON runtime.
   `attach_structural_index` is a no-op apart from its debug alphabet assertion.
   `consume_structural` validates source bytes and emits source offsets directly
   into the tape. `JsonNodeKind::at_cursor` derives kind by reading
   `source[offsets[cursor]]`; there is no retained class column, structural vector,
   parser-owned structural cursor, whitespace bitmap, or `UnionTape` in the live
   path. Therefore the structural-projection union is already concluded: retained
   structural offsets are the tape.

3. Lazy materialisation is functioning for payloads. `skinny/RESULTS.md` reports
   zero payload bytes for every corpus in the lazy tape materialisation rows, and
   REDRESS records runtime assertions for zero payload bytes, zero writes, and
   zero allocations. String escapes are marked through `HAS_ESC`; decoded strings
   materialise only when the view asks for `JsonString::as_str()`.

4. Logical tape bytes remain bounded, but allocation headroom is visible. The
   reported logical+flag/input ratios range from 0.05x to 0.50x. Allocated/input
   ratios range from 0.07x to 0.75x. The largest allocated/input rows are
   `y_string_unicode` 0.75x, `mesh` 0.72x, `marine_ik` 0.70x, `random` 0.51x,
   and `unicode_basic` 0.50x. The largest allocation headroom ratio is
   `update_center` at roughly 1.85x allocated/logical; `github_events`,
   `numbers`, and `mesh` are also above 1.60x. This nominates capacity policy
   and sparse-flag layout as measurement questions, not a new substrate.

5. A tape-shape change only helps S-P2 if it moves a named P1 hot family. The
   live movement map is:

   | Tape-shape pressure | Code that moves first | P1 hot leaf moved? | Disposition |
   | --- | --- | --- | --- |
   | Offset capacity / reserve policy | `TapeBuilder::new`, `push_plain_offset`, `reserve_offsets_cold`, `offset_capacity_bytes` | Not yet. P1 did not name builder reserve/allocation as hot. | Guarded candidate only for high allocated/input rows. |
   | Sparse flag lookup / representation | `Tape::flags_at`, `JsonString::as_str`, `patch_flags` | Not for current parse/direct hot leaves. It can touch `string_escape_decode` only in retained lazy-view profiles. | Guarded candidate; requires fresh view evidence. |
   | Stored node-kind or class bits | `JsonNodeKind::at_cursor`, `next_sibling_cursor`, token iteration | Retained-view code first. If pushed into parse, it tries to move `container_dispatch` from source-byte rediscovery into scanner/cursor state. | Pre-blocked by W3 falsification unless materially new evidence exists. |
   | Sibling skip or subtree skip projection | `next_sibling_cursor`, `span_for_value`, array/object iterators | Retained-view traversal only; current P1 hot leaves are parse/direct/typed guards, not retained view walks. | Diagnostic only; no shortlist without a fresh hot leaf. |

6. Parallel substrate proposals are rejected, not deferred. A structural side
   vector, event side vector, class lane, `UnionTape`, parser-local structural
   cursor, whitespace bitmap, aux projection column, OpenFrame ladder, or second
   retained document representation violates Lock 1. The SK-V9 W3 class-column
   and streaming-cursor attempts were correctness/parity green and still missed
   every must-improve row; REDRESS retired `G-W3-UNION-SUBSTRATE`, not merely
   paused it. P2-D therefore must not resurrect sidecar/union-substrate routes.

7. Direct-only `SinkOnly` remains compatible with Lock 1 because it retains no
   queryable document identity. It may share event contracts and generated
   lowering facts, but it cannot become a parallel retained substrate next to
   the tape. Any direct-only change still has to respect the SK-V12 priority
   that generated non-JSON baselines land before JSON residual retry work.

## §2 — Candidate primitives

Candidate count: 4 total. Three are guarded research candidates inside the
existing tape substrate; one is explicitly rejected/flagged because it is a
parallel-substrate resurrection.

| Candidate | Shape | Scalar-reference status | Arch surface | P1-grounding | Disposition |
| --- | --- | --- | --- | --- | --- |
| `offset_tape_capacity_policy` | Tune grammar-neutral initial capacity and growth for the existing offset vector, using the published logical-vs-allocated counters as the research input. No extra retained vector. | Existing `TapeBuilder::new` plus `reserve_offsets_cold` is the scalar reference. The proof target is equal offset stream, equal flags, equal payload counters. | None. This is allocator/capacity policy, not SIMD. | High offset-count rows and high allocated/input rows: `mesh`, `marine_ik`, `y_string_unicode`, `unicode_basic`, `random`, plus `container_dispatch`/`number_digit_span` writers. | Guarded candidate. It may only advance if fresh S-P3 evidence names builder capacity/reserve cost or proves allocation reduction without regressing row gates. |
| `sparse_flag_lookup_policy` | Keep sparse flags inside `Tape`, but research an in-tape encoding or lookup policy that avoids harmful binary-search/view overhead on escape-heavy rows. No sidecar flag column. | Current ordered `flag_cursors`/`flag_values` and `Tape::flags_at` are the scalar reference. Equal `HAS_ESC` semantics and zero payload writes remain mandatory. | None initially; optional table/bit packing is scalar unless separately proven. | `string_escape_decode` and `unicode_escape_hex_decode` on `unicode_mixed`, `unicode_escapes`, and `y_string_unicode`; `RESULTS.md` shows large sparse flag bytes on those rows. | Guarded candidate. It does not close current parse/direct rows unless retained lazy-view profiles expose `flags_at` or lazy decode as hot. |
| `retained_cursor_skip_projection` | If retained traversal becomes hot, encode subtree/sibling skip facts as generated tape facts or same-tape metadata, never as a second structural index. | Current `JsonNodeKind::at_cursor`, `span_for_value`, and `next_sibling_cursor` source-derived walk remain the oracle. | None unless a later generated grammar proves a grammar-neutral cursor plan. | Related to `container_dispatch`, but current P1 evidence is parse/direct dispatch, not retained view traversal. | Diagnostic/flagged candidate. It is concrete but not selectable from SK-V12 P1 alone. It needs fresh retained-view hot-leaf evidence and must stay within one tape. |
| `structural_class_lane_union` | Add a class column, structural-position vector, streaming structural cursor, `UnionTape`, parser-owned projection, or event sidecar to avoid source-byte rediscovery. | No admissible scalar reference under Lock 1. Prior W3 versions were parity green and measured negative. | SIMD structural producer plus retained parser consumer. | Tempted by `container_dispatch` and structural rediscovery at `consume_structural`/delimiter paths. | Rejected. REDRESS 96, 97, and 98 falsified and retired this route; Lock 1 forbids it as a parallel substrate. |

No wave selection is made here. The two implementable-looking items are
capacity policy and sparse-flag lookup, but both are only research candidates
until a legal S-P3 packet supplies fresh measurement authority. The retained
cursor-skip item is a future retained-view diagnostic, not a JSON direct close
route. The structural class-lane/union route is closed.

## §3 — Grammar-neutrality

1. `offset_tape_capacity_policy` is grammar-neutral only if the policy consumes
   generic facts: structural count estimates, offset count, input length, and
   observed capacity waste. It must not special-case JSON corpus names, JSON
   punctuation, or JSON grammar modules in generic crates.

2. `sparse_flag_lookup_policy` is grammar-neutral only if flag meanings are
   declared by generated per-grammar metadata and carried through the generic
   tape API as opaque bits or generated accessors. A generic crate must not
   learn that JSON has `HAS_ESC` or that CSS/Sheets/BBNF-self have different
   escape semantics.

3. `retained_cursor_skip_projection` can generalise only as generated
   per-grammar retained-view metadata over the same tape. CSS L4 declaration
   values, Sheets formulas, and BBNF-self AST views may have different nesting
   and value projection rules; those differences belong in generated runtime
   modules, not in grammar-name matches in generic crates.

4. Direct-only `SinkOnly` must remain a non-retained output path. Sharing shape
   selection, event facts, or direct-build lowering is fine; retaining a second
   queryable representation beside `Tape` is not grammar-neutral and violates
   Lock 1.

5. Any revived structural class lane would need a new Alpha/S-P3 contract and a
   materially different measurement thesis. Under the current SK-V12 evidence,
   it is not a candidate: it is a rejected parallel substrate.

## §4 — Risks

1. Capacity tuning could be a paper close if it only reduces allocated bytes.
   P2-D can nominate it, but S-P3 must show row-relevant behavior movement or a
   fresh hot leaf in builder reserve/allocation. Logical tape ratio alone is not
   a throughput proof.

2. Sparse-flag work can accidentally become eager materialisation work. The
   guard is strict: zero payload bytes/writes/allocations must remain true, and
   decoded strings must stay lazy at view access.

3. Stored kind bits, sibling skips, and class projections are close to rejected
   W3 terrain. Any plan that adds a retained class column, event vector,
   structural-position vector, cursor sidecar, or parser-owned projection is a
   Lock 1 violation even if it is phrased as a tape improvement.

4. JSON-only attraction is high because SK-V12 P1 hot leaves come from JSON
   captures. Lock 14 and the SK-V12 handoff require generated non-JSON baseline
   work first; P2-D candidates must not add grammar-specific code to generic
   crates or hand-written per-grammar runtime files.

5. Parse-only and direct residual rows are pre-blocked. `container_dispatch`,
   string/unicode, numeric, SIMD movemask, and digest hot-family labels are
   planning evidence only. They do not reopen retired W3, rejected numeric-slot,
   direct-digest, or parse-only close routes.

## §5 — Sources

- `restart/prompts/skinny/PASS-2-RESEARCH.md:51` and `:237`: P2-D scope and
  Lock 1 substrate-union constraint.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`: SK-V12 S-P2 priority, refusal
  conditions, and generated non-JSON baseline boundary.
- `restart/locks/LOCKS.md:52`: Lock 1, tape as substrate and no parallel
  substrates.
- `restart/locks/LOCKS.md:78`: Lock 14, full grammar generalisation and no
  grammar-specific code in generic crates.
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`: parse-only
  diagnostic hot-family surface.
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`: direct and
  typed product-plane accepted hot-family surface.
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:220`: Mode
  III absence boundary and largest allocated/input ratio summary.
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`: PMU cycle
  context for parse/direct/typed rows.
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:136`-
  `:138`, `:207`-`:214`, `:326`-`:334`, and `:345`-`:356`: canonical hot
  families, source anchors, and pre-block table.
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`: unchanged
  SK-V11 close state and generated non-JSON baseline priority.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`:
  accepted S-P1 convergence packet.
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md` and
  `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`: capture
  manifest and replay ledger.
- `skinny/RESULTS.md:97`-`:143`: lazy tape materialisation counters and
  `N-direct / NoGo` outcome.
- `skinny/REDRESS.md:110`-`:116`, `:195`-`:207`, `:246`-`:256`, and `:274`-`:279`:
  one-buffer tape builder, lazy-offset tape, sparse flags, and direct spare
  capacity.
- `skinny/REDRESS.md:742`-`:813`: rejected event cursor and parser-local cursor
  routes.
- `skinny/REDRESS.md:2795`-`:2950`: SK-V9 W3 class-column, streaming-cursor, and
  union-substrate retirement.
- `skinny/REDRESS.md:3040`-`:3058`: SK-V10 W3 parse-firewall audit.
- `skinny/REDRESS.md:3311`-`:3381` and `:3495`-`:3553`: SK-V11 non-JSON baseline
  blocker, numeric route rejection, direct residual fixpoint, and SK-V12 routing.
- `restart/skinny/tranches/sk-v9/research/skv9-W3-research-v3.md` and
  `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v4.md`: W3
  falsification context for class-lane-only and structural union routes.
- `restart/skinny/tranches/sk-v11/research/w3/w3-plan-number-span-emit-slot.md`:
  later W3 numeric falsification context.
- `skinny/crates/runtime/src/tape/mod.rs:94`-`:164`: live `Tape` shape and
  offset/flag byte reporting.
- `skinny/crates/runtime/src/tape/assembler.rs:42`-`:123`: live `TapeBuilder`
  shape, direct spare-capacity writes, sparse flags, and finish path.
- `skinny/crates/runtime/src/grammars/json/parser.rs:7`-`:52`: generated parser
  state, capacity plan, one tape builder, and parse-to-root flow.
- `skinny/crates/runtime/src/grammars/json/generated.rs:10`-`:17`,
  `:37`-`:58`, `:240`-`:305`, and `:310`-`:338`: no-op structural index,
  source-byte dispatch, whitespace/delimiter handling, and offset emission.
- `skinny/crates/runtime/src/grammars/json/value.rs:28`-`:47`: kind derivation
  from `source[offsets[cursor]]`.
- `skinny/crates/runtime/src/grammars/json/view.rs:11`-`:48` and `:332`-`:413`:
  retained document view, token stream, span calculation, and sibling walk.
- `skinny/crates/ir/src/lib.rs:402`-`:407`,
  `skinny/crates/passes/src/lib.rs:456`-`:495`, and
  `skinny/crates/codegen/src/lower/mod.rs:19`-`:23`: grammar-neutral backend
  shape surface for `OffsetTape`, `EventTape`, `SinkOnly`, and
  `CollapsedStage`.
