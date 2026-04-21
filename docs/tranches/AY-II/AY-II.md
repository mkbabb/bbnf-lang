# Tranche AY-II — Architectural Consolidation + Near-Parity Close (Pass II)

AY-II executes the gestalt re-ordered remainder of AY's parity
thesis. Pass I (`../AY-I/AY-I.md`, `../AY-I/FINAL.md`) landed the
write-time substrate experiment, the direct-to-struct admission
broadening, and the honest diagnostic record documenting why the
experiment did not compose. The 4-agent audit triumvirate at
`audit/AUDIT-{A,B,C,D}-*.md` converged on three architectural
transpositions; AY-II lands all three atomically in W0 and closes
on the near-parity gates in W1.

Two waves. Three parallel sub-agents in W0 on disjoint file bounds.
No recorded misses this pass — either the gates close or the pass
re-plans under the §Diagnostic-loop relinquish protocol.

## Architectural thesis

1. **ONE compound emission API.** `push_compound` retires. Every
   shape emitter (including retry-IIFE sites: `wrap`, `keyword`,
   `inline`, `alt_dispatch`, `flat`, `array` Shape-2, `pratt`
   reducer) emits via `open_compound` + `close_compound`.
2. **ONE stamping path.** `TapeBuilder::note_push` and
   `TapeRec::SIB_SKIP_STAMPED_BIT` retire. Finaliser post-pass is
   the sole `sib_skip` stamp source (pre-W5 / AU-era discipline,
   strictly faster per AUDIT-C + AUDIT-D).
3. **Rollback is a first-class primitive.** `Columns::rollback_to(open_offset)`
   replaces every `columns_mut().truncate(save)` site. Single
   source of liveness; emitter retry contracts are architecturally
   coherent.
4. **`Parsed::to_value()` routes through the visitor lane.** The
   visitor-lane codegen already posts a 0.99× sonic geomean across
   5 fixtures (AUDIT-D §2); default `to_value()` adopts that
   emission discipline and retires the tape-reconstruction path.
5. **`navigate_tape` retires.** Zero production consumers per
   AUDIT-B; `__path_walk` simplifies to classic cursor descent.
6. **No grammar-name dispatch.** Already invariant-holds per
   AUDIT-C §6 (zero matches for `JsonParser|CssL4Parser|BbnfParser|
   GoogleSheetsParser` in emitter/runtime/tape). AY-II preserves.

## Invariants

1. `push_compound` absent from `TapeBuilder`'s public API + every
   `quote!` block in the emitter.
2. `note_push` + `SIB_SKIP_STAMPED_BIT` absent from `tape.rs` +
   `builder.rs` + `finaliser.rs`.
3. `Columns::rollback_to(open_offset)` is the only retry-path
   truncation surface; `columns_mut().truncate()` on raw lengths
   disappears from the emitter.
4. `Parsed::to_value()` is defined against the visitor-lane
   emission; the tape-reconstruction path is retired.
5. `navigate_tape` absent from `runtime/path.rs`; `__path_walk`
   reads through `TapeCursor` directly.
6. Close-matrix fat-LTO benches run clean across all five bench
   binaries (`json_monolithic`, `css_l4`, `google_sheets_monolithic`,
   `bbnf_monolithic`, `compile_pipeline`) — no panics, no skipped
   fixtures.
7. Near-parity gates on the default eager JSON path (the 5-fixture
   geomean ≤ 1.20 × sonic; twitter ≤ 1.15 × sonic; canada / citm ≤
   1.20 × sonic).
8. Workspace green at every wave boundary.
9. Bootstrap regen cycle-1 = cycle-2 byte-identical at close.
10. No stubs, fallbacks, feature flags, shadow surfaces, or
    `#[allow(...)]` masks added to hide pass-II work. Every
    previously-MISS'd pass-I gate either closes or is retired with
    rationale.
11. Pass II lands no "recorded misses" — a wave that cannot close
    triggers §Diagnostic-loop relinquish, not status drift.

## Operational posture

1. W0 dispatches 3 parallel sub-agents on disjoint file bounds per
   AUDIT-C §1–4 decomposition. W0 close runs the full fat-LTO
   bench matrix to surface any lingering CSS / Sheets panic before
   W1.
2. W1 captures the close evidence — full fat-LTO matrix, samply on
   eager JSON twitter, close-stamp `cargo asm` where the emitter's
   close point is canonically identifiable, apples-to-apples value
   bench vs sonic-rs.
3. Every wave close runs `make ay-bench-close WAVE=II` + the full
   `make iter-test-{leaf,grammar,ws}` tier. Bench failure or panic
   in any grammar is a wave-close blocker, not a recorded miss.
4. Bootstrap regen runs at W0 close and at W1 close; both cycles
   pass idempotency (cycle-1 = cycle-2 byte-identical).
5. `cargo expand` output is primary evidence per the
   audit-expand-begotten-code edict
   (`docs/instructions/README.md`).

## Wave summary

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Architectural consolidation: substrate rollup + emitter unification + navigate_tape retirement + visitor-lane to_value | tranche open | planned |
| **W1** | [waves/W1.md](waves/W1.md) | Close matrix + near-parity gates + FINAL + successor handoff | W0 | planned |

## Defensible floor

AY-II's defensible floor is not "architectural infrastructure
partially landed." The minimum closeable outcome is:

1. W0 lands the full architectural consolidation (three
   transpositions per thesis §1–5) and the CSS / Sheets fat-LTO
   panic is fixed transitively.
2. W1 hits every near-parity gate in invariant #7 on fat-LTO.

Anything less opens pass III, not a recorded miss.

## AY-II → BA / BB / BC handoff contract

AY-II does not close until:

1. Every AY-I routed gate (per `../AY-I/FINAL.md` §Hard gates
   status table) either closes or is retired with rationale.
2. The default JSON parse routes through the visitor-lane
   `to_value()`; the tape-reconstruction path is deleted.
3. `cargo expand -p bbnf --bench json_monolithic` contains zero
   `push_compound` calls and zero `note_push` references.
4. Fat-LTO `cargo bench` runs clean across all five bench
   binaries.
5. Near-parity gates in invariant #7 hold on `post-AY-II-eager.json`.
6. `docs/tranches/AY-II/FINAL.md` authored against the close
   artefacts.
7. `docs/tranches/BA/BA.md`, `BB/BB.md`, `BC/BC.md` updated to
   reference AY-II (not AY) as their predecessor close and
   acknowledge the visitor-lane default.

## Indefatigability

When AY-II closes correctly, bbnf has one parser, one substrate,
one compound emission API, one stamping path, one rollback
primitive, and a `Parsed::to_value()` that rides the already-
gate-beating visitor-lane codegen. BA opens on a substrate that
actually holds the invariants AY's original plan declared.
