# SK-V12 P2-D: Substrate + Tape Design
Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-20.
Scope: Interrogate the offset-tape substrate, logical-vs-allocated tape ratios, lazy materialisation, and same-tape CSS-local union eligibility under the SK-V12 user pin.
Output: this file.
P1 hot-leaf antecedents: `bounded_plain_string_scan`, `container_dispatch`, `unicode_escape_hex_decode`, `number_digit_span`, `simd_movemask`, `string_escape_decode`, `output_digest_hash`, `ascii_whitespace_skip`, `typed_direct_projection`, and `serde_json_oracle_read_parse`.
Lock surface: Lock 1 and Lock 14. Tape remains the single retained substrate; any structural projection retained after S-P3 must be part of the same tape/materialisation surface, and generic crates must not learn CSS/JSON/Sheets/BBNF-self policy.

## §1 — Findings

1. The live retained substrate is a single offset tape, not two retained document
   planes. `Tape` owns `source`, one `Vec<u32>` offset stream, sparse flag
   cursor/value vectors, a payload arena, and a `TapeId`
   (`skinny/crates/runtime/src/tape/mod.rs:94`-`:100`). `ValueRef` is still
   `&Tape + cursor` with node-kind and event-grammar phantom axes
   (`skinny/crates/runtime/src/tape/mod.rs:175`-`:222`). `ParserState` owns one
   `TapeBuilder` and seals it into `JsonRoot`
   (`skinny/crates/runtime/src/grammars/json/parser.rs:7`-`:31`). This satisfies
   the Lock 1 cardinality rule: one retained tape surface.

2. Structural projection is not currently retained beside the tape. The JSON
   scanner can emit a `StructuralIndex`, but that value is only positions plus a
   backend tag (`skinny/crates/bbnf-simd/src/lib.rs:71`-`:97`); the generated
   parser's `attach_structural_index` is a no-op (`skinny/crates/runtime/src/grammars/json/generated.rs:10`-`:17`).
   Retained JSON parsing finds delimiters from source bytes and emits tape
   offsets (`skinny/crates/runtime/src/grammars/json/generated.rs:240`-`:305`).
   Retained kind lookup also derives from `source[offsets[cursor]]`
   (`skinny/crates/runtime/src/grammars/json/value.rs:28`-`:47`). Therefore, if
   SK-V12 later retains CSS structural facts, those facts must be the tape or
   generated same-tape metadata, not a parallel side vector.

3. The user pin changes the REDRESS posture but not the Lock 1 shape rule.
   USER-PIN D3 rescinds category-level pre-blocks on union/event/class/cursor
   routes and allows new attempts that cite REDRESS 96/97/98, name the material
   differential, provide scalar reference plus parity/checkasm plus same-wave
   consumer, and pass CHALLENGE
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`-`:56`).
   It does not authorize a second retained substrate. PASS-2 and the orchestrator
   still reject sidecars and substrate splits under CH5
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:237`-`:240`,
   `restart/prompts/ORCHESTRATOR.md:83`-`:88`).

4. REDRESS 96/97/98 are historical implementation falsifiers, not category
   blockers. REDRESS 96 measured a co-indexed event-class byte column plus
   move-consumed `scan_structurals` vector and failed every W3/W10b row
   (`skinny/REDRESS.md:2797`-`:2843`). REDRESS 97 removed the full vector and
   used an allocation-free streaming cursor, then failed the same gate surface
   (`skinny/REDRESS.md:2852`-`:2901`). REDRESS 98 retired that SK-V9 gate because
   those two faithful implementations proved that materializing or streaming a
   structural cursor through retained parsing cost more than the source-byte
   rediscovery it replaced (`skinny/REDRESS.md:2910`-`:2933`). A SK-V12 attempt
   must therefore be materially different in consumer, row, and fact ownership.

5. No same-tape CSS-local union candidate is shortlist-grounded yet. Pin-aware
   S-P1 accepted JSON hot families only; it explicitly states that JSON telemetry
   may nominate primitive families but does not prove CSS L4/Sheets/BBNF-self
   behavior (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60`-`:75`).
   P1-E records 82 JSON hot-leaf summary rows and 410 detail rows, but CSS L4 is
   absent because there is no generated skinny CSS L4 parser or lightningcss
   comparator row (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:21`-`:50`).
   P1-F likewise finds zero admitted generated CSS L4 rows and no lightningcss
   close-bar evidence in `RESULTS.md`
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80`-`:85`,
   `:177`-`:184`). Under CH1/CH2, S-P3 should not shortlist a CSS-local union
   primitive until W1a/W1b create CSS generated Track 1, a same-plane fact stream,
   a lightningcss comparator, equality evidence, and CSS hot-leaf attribution.

6. The CSS goalset does define the only plausible material differential:
   a same-tape CSS fact stream consumed by CSS equality/visitor/comparator paths,
   not JSON parse-only structural rediscovery. The SK-V12 close path requires a
   generated CSS L4 parser faster than `lightningcss_mbps + 1` on the same
   corpus/output plane with strict equality
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:39`-`:60`). The handoff seeds a
   possible W3 as "CSS-local same-tape union attempt," but only after W1a/W1b
   establish CSS legality and baseline evidence
   (`restart/skinny/tranches/sk-v12/HANDOFF.md:111`-`:140`). P2-D's verdict is
   therefore conditional: keep the shape as a post-baseline research aperture,
   not as a P3-A shortlist candidate in this S-P2 cycle.

7. Lazy materialisation and tape ratios do not by themselves justify union work.
   `RESULTS.md` reports zero payload bytes on every lazy tape row and logical
   tape ratios from 0.05x to 0.50x input, with allocated tape ratios from 0.07x
   to 0.75x input (`skinny/RESULTS.md:97`-`:142`). The large allocated/input
   rows (`y_string_unicode` 0.75x, `mesh` 0.72x, `marine_ik` 0.70x) nominate
   capacity-policy measurement, not a class lane. The materialization stats code
   computes these from `offset_bytes + flag_bytes`, allocated capacity, and
   payload bytes on the same `JsonRoot` tape
   (`skinny/crates/bbnf-bench/src/materialization.rs:20`-`:99`).

8. Current codegen still blocks CSS emission before any substrate redress can be
   meaningful. Runtime emission calls `json_provider::ensure_runtime_profile`
   and that guard accepts only `backend.grammar_name == "json"`
   (`skinny/crates/codegen/src/lib.rs:102`-`:136`,
   `skinny/crates/codegen/src/json_provider.rs:4`-`:12`). The generated template
   also hardcodes JSON structural alphabet and JSON value dispatch
   (`skinny/crates/codegen/src/json_templates/generated.rs:10`-`:17`,
   `:47`-`:58`). A CSS-local same-tape primitive before `GrammarConfig`/CSS
   generated runtime work would be a paper-close and a Lock 14 risk.

## §2 — Candidate primitives

No P2-D primitive is shortlist-ready for S-P3 in this cycle. The table names the
concrete shapes S-P3 may disposition after CSS baseline evidence exists, plus
why each is excluded or diagnostic today.

| Candidate | Shape | Scalar-ref status | P1 antecedent | Lock 1 cardinality effect | Same-wave consumer | Micro-proof need | P2-D verdict |
|---|---|---|---|---|---|---|---|
| `css_fact_stream_same_tape_kind` | Opaque generated per-grammar fact/kind bits stored in the existing tape or generated same-tape metadata for CSS L4 facts; no structural-position vector, no cursor sidecar, no `UnionTape`, no public substrate API. | Missing until W1a/W1b produce a scalar generated CSS fact stream and independent same-plane oracle. Existing JSON `JsonNodeKind::at_cursor` is only a JSON oracle. | Nominally adjacent to `container_dispatch`, `simd_movemask`, and `ascii_whitespace_skip`, but those are JSON-only pin hot leaves; no CSS hot leaf yet. | Must stay one retained tape. A packed fact lane inside `Tape` is cardinality +0 only if it is opaque and generated-owned; any extra retained vector is cardinality +1 and fails Lock 1. | CSS L4 equality/visitor/fact-stream comparator in the same wave as the tape change, with lightningcss same-plane comparison. | Cite REDRESS 96/97/98, prove material differential is CSS fact-stream consumption rather than JSON parse delimiter replacement, prove strict equality, JSON guard preservation, and row throughput against `lightningcss_mbps + 1`. | Do not shortlist yet; conditional post-W1b aperture. |
| `offset_tape_capacity_policy` | Adjust existing `TapeBuilder` initial capacity/growth or cost-derived capacity policy; no new retained facts. | Existing grow-only `TapeBuilder::new`/`reserve_offsets_cold` is the scalar reference (`skinny/crates/runtime/src/tape/assembler.rs:50`-`:91`). | No accepted Track 1 P1 hot leaf names builder reserve/allocation; RESULTS ratio evidence is telemetry, not self-time. | Cardinality +0; same offsets/flags/payloads. | Same-wave generated CSS parser or JSON guard run must show row movement and equal tape/parity. | Equal offset stream, equal sparse flags, zero payload writes, lower allocated/input ratio, and non-regressed parse/direct/typed guards. | Diagnostic only; not a union primitive. |
| `sparse_flag_same_tape_policy` | Keep escape/control flags inside the tape, possibly changing representation or generated accessors; no side flag substrate. | Current ordered `flag_cursors`/`flag_values` plus `Tape::flags_at` are scalar reference (`skinny/crates/runtime/src/tape/mod.rs:130`-`:164`). | Adjacent to `bounded_plain_string_scan`, `unicode_escape_hex_decode`, and `string_escape_decode`; current Track 1 evidence does not name `flags_at` as hot. | Cardinality +0 if flags remain in `Tape`; any decoded-byte sidecar or grammar-owned flag vector fails Lock 1. | Same-wave retained string/CSS value consumer that actually calls lazy decode or flag lookup. | Equal lazy semantics, zero eager payload materialisation, strict CSS/string equality, and measurable consumer hot-leaf movement. | Diagnostic only; not shortlist-ready. |
| `retained_view_skip_same_tape_fact` | Generated same-tape subtree/sibling skip facts for retained traversal, encoded as grammar-derived opaque facts. | Current scalar oracle is source-derived `JsonNodeKind::at_cursor`, token stream, and sibling/span walk. | Weakly adjacent to `typed_direct_projection` and `container_dispatch`, but P1 did not profile CSS retained traversal. | Cardinality +0 only as generated facts attached to the same tape; a side projection column fails Lock 1. | Same-wave retained CSS fact-stream view or equality walker; parse-only black-box root is not a consumer. | Equal token/fact stream, no generic grammar-name branch, retained traversal hot-leaf attribution, and JSON guard preservation. | Diagnostic only until CSS retained consumer is hot. |
| `parallel_structural_cursor_or_class_lane` | Any retained structural-position vector, streaming cursor, whitespace bitmap, aux density/projection column, decoded-byte sidecar, parser-owned structural projection, or `UnionTape`. | Historical scalar/parity references exist only as REDRESS 96/97 rejected implementations. | Tempted by `container_dispatch`/`simd_movemask`; no CSS P1 antecedent and historical JSON antecedent regressed. | Cardinality +1 retained substrate or hidden producer; violates Lock 1 unless redefined as same-tape opaque facts with a new consumer. | None legal in this shape. | Would need a new user/orchestrator contract; under current PASS-2 this is CH5 reject. | Not a candidate. |

## §3 — Grammar-neutrality

1. A CSS-local union primitive can pass Lock 14 only if the generic operation is
   grammar-neutral: "store/retrieve an opaque per-cursor fact on the same tape"
   or "derive capacity from input/offset counts." CSS token roles, JSON node
   roles, Sheets formula facts, and BBNF-self AST roles must be generated from
   grammar metadata and runtime templates, not matched in generic crates.

2. `css_fact_stream_same_tape_kind` is generalisable only as a per-grammar
   template surface over a generic same-tape fact API. It is not generalisable
   if `bbnf-runtime`, `bbnf-codegen`, or `bbnf-simd` learns that a class means a
   CSS declaration, JSON string, Sheets cell, or BBNF rule. Lock 14 forbids
   generic grammar-name branches (`restart/locks/LOCKS.md:78`).

3. `offset_tape_capacity_policy` is grammar-neutral when keyed to generic facts
   such as input length, structural/fact count, retained-output mode, and
   measured capacity waste. It becomes overfit if it keys off corpus names or
   CSS/JSON-specific byte alphabets outside generated metadata.

4. `sparse_flag_same_tape_policy` is grammar-neutral only if flag meanings are
   opaque to generic tape. JSON `HAS_ESC`, CSS escape/identifier flags, Sheets
   formula-token flags, and BBNF-self token flags must be generated accessors
   over the same underlying storage.

5. Direct-only `SinkOnly` remains compatible with the substrate union because it
   retains no queryable document identity. It must not be turned into a second
   retained representation next to `Tape`.

## §4 — Risks

1. **Paper-shortlist risk.** A same-tape CSS union shape before CSS Track 1
   exists would satisfy neither CH1 nor CH6. S-P3 must first land CSS generated
   runtime, same-plane lightningcss comparator, equality, and CSS hot-leaf
   capture.

2. **Historical REDRESS replay.** Class-column, streaming-cursor, and
   class-lane-only shapes must cite REDRESS 96/97/98 and name a material
   differential. "Now CSS" is not enough; the consumer must be CSS fact-stream
   equality/admission work, not JSON parse delimiter replacement under a new
   label.

3. **Lock 1 cardinality drift.** A retained side vector can be hidden behind
   harmless names (`density`, `projection`, `cursor`, `class lane`). If it can
   be traversed independently of the tape, it is a second substrate and CH5
   should reject it.

4. **Lock 14 leakage.** Current codegen still has JSON-only runtime emission.
   Adding CSS-specific arms to generic crates to rush the union attempt would
   break the prerequisite `GrammarConfig`/generated-metadata work.

5. **Ratio over-interpretation.** Logical-vs-allocated tape ratios justify a
   capacity-policy question, not a throughput claim. S-P3 should require row
   movement and parity, not just smaller allocated capacity.

6. **JSON guard movement.** Any tape/runtime/codegen change can affect JSON.
   The SK-V12 guard rule requires refreshed JSON guard evidence or measured
   REDRESS demotion when generic runtime, codegen, generated output, benchmark,
   report, or gate paths move (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144`-`:148`).

## §5 — Sources

- `restart/prompts/skinny/PASS-2-RESEARCH.md:36`-`:58`, `:62`-`:85`,
  `:95`-`:107`, `:220`-`:240` — S-P2 read-only scope, P2-D scope, schema, CH1/CH2,
  scalar-reference discipline, and Lock 1 substrate-union rule.
- `restart/prompts/ORCHESTRATOR.md:74`-`:122` — CH1-CH6 and convergence rules.
- `restart/locks/LOCKS.md:52`, `:70`, `:78` — Lock 1, materialization plan, and
  Lock 14 grammar-neutrality.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18`-`:56`,
  `:80`-`:88` — CSS L4 authority, `lightningcss_mbps + 1`, union category
  unblock, and parse-time priority.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md:39`-`:82`, `:152`-`:180` —
  CSS admission requirements, fixpoint union-attempt requirement, and pin-aware
  candidate space.
- `restart/skinny/tranches/sk-v12/HANDOFF.md:88`-`:140`, `:143`-`:171` —
  required S-P2/S-P3 re-derivation, seeded CSS-local same-tape W3, telemetry,
  and refusal conditions.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:50`-`:84`
  — accepted S-P1 hot families, CSS absence, PMU aggregates, and advancement
  conditions.
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:131`-`:178`,
  `:207`-`:221` — capture/replay coverage and derived hot-leaf tables.
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:75`-`:85`,
  `:177`-`:210` — parse hot-leaf families and CSS L4 profile absence.
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:23`-`:45`,
  `:131`-`:181` — product-plane hot families and direct/typed PMU implications.
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`-`:82`,
  `:113`-`:140` — Mode III/CSS absence and generated CSS blockers.
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:69`-`:96`,
  `:222`-`:236` — PMU aggregate surface and CSS PMU absence.
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:21`-`:50`,
  `:220`-`:260` — hot-leaf authority and pin/REDRESS boundaries.
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:73`-`:85`,
  `:145`-`:194` — current RESULTS surface, non-JSON REDRESS state, and absence
  of CSS/lightningcss row evidence.
- `skinny/RESULTS.md:51`-`:91`, `:97`-`:146` — output plane, lazy tape
  materialisation counters, zero payload bytes, Track 1/Track 2 tape boundary.
- `skinny/REDRESS.md:2795`-`:2938`, `:3040`-`:3058` — REDRESS 96/97/98 and SK-V10
  parse-only firewall.
- `restart/skinny/tranches/sk-v9/research/skv9-W3-research.md:1`-`:74`,
  `restart/skinny/tranches/sk-v9/research/skv9-W3-research-v2.md:1`-`:75`,
  and `restart/skinny/tranches/sk-v9/research/skv9-W3-challenge-v4.md:1`-`:83`
  — SK-V9 W3 material-differential and challenge context.
- `skinny/crates/runtime/src/tape/mod.rs:94`-`:173`,
  `skinny/crates/runtime/src/tape/assembler.rs:42`-`:123`,
  `skinny/crates/runtime/src/grammars/json/parser.rs:7`-`:52`,
  `skinny/crates/runtime/src/grammars/json/generated.rs:10`-`:17`, `:240`-`:305`,
  `skinny/crates/runtime/src/grammars/json/value.rs:28`-`:47`,
  `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`:53`,
  and `skinny/crates/bbnf-simd/src/lib.rs:71`-`:97` — live substrate,
  parser, scanner, and structural-index code.
- `skinny/crates/bbnf-bench/src/materialization.rs:20`-`:99`,
  `skinny/crates/bbnf-bench/src/parity.rs:23`-`:80`,
  `skinny/crates/codegen/src/lib.rs:102`-`:136`,
  `skinny/crates/codegen/src/json_provider.rs:4`-`:12`,
  and `skinny/crates/codegen/src/json_templates/generated.rs:10`-`:17`, `:47`-`:58`
  — materialisation stats, parity oracle, JSON-only codegen emission, and
  template structural policy.
