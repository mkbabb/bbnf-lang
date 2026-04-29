# AZ-II.cutover.O3a-P1 - Projection Totality and Generated View Residue
**Opens after**: AZ-II.cutover.O3a baseline capture and six-agent audit synthesis
**Agents**: up to 10 parallel
**Hard gate**: `projection_totality_runtime_call_count` is either closed inside O3 or routed to a named O3b before generated-view purge lands.
**Status**: complete_with_misses

2026-04-29 Round 2 triad complete: research, plan, and redress/probe
artifacts exist under `docs/tranches/AZ-II/audit/O3a-P1-*.md`. P1
closes inside O3. No `O3b.md` is required unless an O3 implementation
agent later proves an out-of-bounds owner with source evidence.

## Scope

1. Root-cause the `bbnf::projection_totality projection_totality_runtime_call_count`
   failure against generated tape-view and `ValueRoot` residue.
2. Prove whether O3's generated-view purge is sufficient or whether a
   separate O3b projection-totality wave is required.
3. Ensure document-owned projection APIs replace any consumed generated
   node-view surface.
4. Gate O3 close on a zero-residue scan over StructDirect generated
   output.

## File Bounds

| File | Access |
|---|---|
| `docs/tranches/AZ-II/audit/O3a-P1-research.md` | create |
| `docs/tranches/AZ-II/audit/O3a-P1-plan.md` | create |
| `docs/tranches/AZ-II/waves/cutover/O3.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O3b.md` | create only if required |
| `crates/core/src/backend/rust/view/**` | future O3 redress |
| `crates/core/src/backend/rust/emitter/grammar.rs` | future O3 redress |
| `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs` | future O3 redress |
| `crates/core/tests/projection_totality.rs` | future O3 redress |

**Do NOT touch**: `Parsed<R>`, `TapeDirect`, `crates/tape/**`, or
benchmark surfaces. O4/O5/O6 own those.

## Triumvirate Dispatch

| Lane | Agents | Deliverable |
|---|---:|---|
| Research | 2 | Generated residue inventory and test root cause |
| Plan + wave creation | 1 | `O3a-P1-plan.md` and O3 amendment or `O3b.md` |
| Redress | up to 3 | O3 commits or halt with O3b handoff |
| Orchestrator | 1 | Regen-window integration and zero-residue scan |

## Hard Gate

1. `docs/tranches/AZ-II/audit/O3a-P1-research.md` maps the failing
   projection-totality test to generated-view, runtime projection, or
   materializer ownership.
2. `docs/tranches/AZ-II/audit/O3a-P1-plan.md` states P1 closes in O3;
   no O3b child spec is currently justified.
3. `cargo nextest run -p bbnf --test projection_totality --cargo-profile ax-iter -- --nocapture` passes before O3 closes.
4. O3's scan artifact records zero generated StructDirect `TapeCursor`,
   node-view, and `ValueRoot` production hits.

## Dependencies

- **Depends on**: AZ-II.cutover.O3a
- **Blocks**: AZ-II.cutover.O3 close
