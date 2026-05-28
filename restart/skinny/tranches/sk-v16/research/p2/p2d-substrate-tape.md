# SK-V16 P2-D: Substrate And Tape Design

Pass: S-P2 Research. Cycle: V16.
Date: 2026-05-28.
Scope: offset tape, structural projection union, and tape/view hot leaves under Lock 1.
Output: this file.
P1 hot-leaf antecedents: tape/view, structural scan, generated product, string body range.
Lock surface: Lock 1.

## Section 1 - Findings

Lock 1 is load-bearing: if structural offsets are retained, that structural
projection is the tape. A transient SIMD mask stream is allowed only inside a
single chunk call; retained class streams, parser-owned cursor/list state,
sidecars, public `UnionTape`, and second tapes are rejected.

Current substrate shape:

- `skinny/crates/runtime/src/tape/mod.rs:94`-`101` stores source, offsets,
  flag cursors/values, payloads, and id in one retained tape object.
- `Tape::offset_at` is a direct offset vector lookup at
  `skinny/crates/runtime/src/tape/mod.rs:138`-`142`.
- `ValueRef` stores a tape reference plus cursor at
  `skinny/crates/runtime/src/tape/mod.rs:175`-`222`.
- JSON views perform cursor walks and string-body range calculation at
  `skinny/crates/runtime/src/grammars/json/view.rs:355`-`430`.
- JSON structural scan is already a same-substrate producer in
  `skinny/crates/runtime/src/grammars/json/scan.rs:22`-`35`; the aarch64 scan
  emits positions that the tape builder consumes, not a public sidecar.

P1 Mode III shows tape/view cost in eager decode and cold-first parse, but
does not justify a retained auxiliary column. REDRESS 50-55 already measured
and rejected retained aux side tables, byte-class whitespace cursors,
parser-local structural-mask cursors, and decoded-string stats sinks.

Current evidence does not contain a committed lazy-materialisation ratio table.
Do not invent one. The executable surface exists in
`skinny/crates/bbnf-bench/src/materialization.rs:4`-`90`, which reports
`input_bytes`, `offset_count`, `offset_bytes`, `flag_bytes`,
`offset_capacity_bytes`, `payload_bytes`, and per-input-byte ratios. S-P3 may
require a ratio-reporting wave, but P2-D only records the gap.

## Section 2 - Candidate Primitives

| Candidate | Shape | Scalar-ref status | Checkasm/parity expectation | P1 antecedent |
|---|---|---|---|---|
| `tape_cursor_step` | given tape and cursor, return kind plus next cursor/offset under generated grammar rules | scalar design only; lives in runtime/codegen, not `bbnf-simd` | unit/golden tests over generated JSON/CSS/Sheets views | `JsonNodeKind::at_cursor`, `next_sibling_cursor` |
| `string_body_range_fast` | compute string body byte range from open quote cursor without repeated kind lookups | scalar design only unless later string mask consumer proves need | generated view parity plus invalid cursor tests | `string_body_range`, `Tape::offset_at` |
| `structural_emit_to_tape` | consume transient masks directly into the tape builder without materialized public index | existing scan emits positions; direct builder path is design candidate | scalar parity against current scan/tape assembly | structural_scan_only and scan tail |
| `flag_lookup_shape` | replace binary-search flags with grammar-owned sparse map or inline flags only if measured | scalar substrate candidate | unit parity and Mode III retime; no SIMD | `flags_at`, view traversal |
| `materialization_ratio_report` | emit logical-vs-allocated tape ratio facts from existing stats helpers | scalar/evidence only | gate consumed report, not checkasm | S-P3 close telemetry |

## Section 3 - Grammar-Neutrality

All four candidates are grammar-neutral if generated grammars provide:

- kind maps;
- delimiter/close rules;
- string and scalar token policies;
- view accessor code.

They fail Lock 14 if they hard-code JSON node names or emit per-grammar runtime
branches in generic crates.

## Section 4 - Risks

- Retained side tables and retained cursors are pre-blocked. The candidate
  must reduce work inside the tape/view surface, not add a second product.
- Parser-owned structural projection is rejected. Any structural projection
  that persists must be the tape or part of tape construction.
- Tape optimization cannot create Track 1 == Track 2 dishonesty. Product
  comparators remain same-plane and independent.
- Mode III hot leaves include harness and FNV effects. S-P2 excludes those as
  optimization authority.

## Section 5 - Sources

- `restart/locks/LOCKS.md:75`-`152`
- `restart/skinny/tranches/sk-v16/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1e-hot-leaf-attribution.md`
- `skinny/crates/runtime/src/tape/mod.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/bbnf-bench/src/materialization.rs`
- `skinny/REDRESS.md` REDRESS 50-55
