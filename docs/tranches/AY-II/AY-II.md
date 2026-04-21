# Tranche AY-II — Fused Substrate + Full Grammar-Derived Semantic Parity (Pass II)

AY-II executes the architectural close of AY. Pass I
(`../AY-I/AY-I.md`, `../AY-I/FINAL.md`) proved the emitted visitor-
lane shape reaches 0.99× sonic on eager JSON, mapped the
`note_push` regression root cause, and broadened direct-to-struct
admission to 71 grammar-derived projections. The 4-agent audit
triumvirate at `audit/AUDIT-{A,B,C,D}-*.md` diagnosed the
remainder. AY-II lands it on one path, grammar-derived, close-gated
against real semantic peers — not internal ratios.

Two waves. Five parallel sub-agents in W0 on disjoint file bounds.
No recorded misses. Every AY-II close gate is a semantic or
runtime fact verifiable against an external reference (sonic-rs,
simd-json, lightningcss, cssparser) or a grammar-derived
totality check (projection counts, consumer counts,
materializer counts in agreement).

## Architectural thesis

1. **ONE parse pass.** The default parser writes the canonical
   tape substrate AND constructs the `<Grammar>Value` semantic
   surface in a single pass (fused pipeline). `Parsed::to_value()`
   is a thin projector over the already-constructed value — it
   does **not** reparse, does **not** drive a second visitor pass,
   does **not** walk the tape to reconstruct children per compound.
2. **ONE substrate.** The tape remains the canonical structural
   substrate; the value surface is populated in lockstep through a
   parallel `ValueBuilder<R>` that mirrors the compound open/close
   frame stack.
3. **ONE compound emission API.** `push_compound` retires. Every
   shape emitter uses a unified open/close API against a
   rollback-aware `Columns::rollback_to(open_offset)` primitive.
4. **ONE stamping path.** `note_push` and `SIB_SKIP_STAMPED_BIT`
   retire. `finaliser::finalise` is the sole `sib_skip` stamp
   source (pre-W5 / AU-era discipline).
5. **ALL semantic information is grammar-derived.** No hand-coded
   grammar-name dispatch. No hardcoded type tables. No
   hand-bound Rust struct shapes for grammar-declared typed surfaces.
   Every `<Grammar>Value` variant, every projection struct, every
   materializer path is emitted from CSP-inferred type facts +
   egraph-derived projections + payload-layout analysis over the
   IR. A grammar declaring `color = ... -> Color ;` produces a
   fully-emitted `<Grammar>Color` struct + its materializer
   without a single line of hand-written binding.
6. **Structural scan is a universal substrate service.** The
   `Columns` + `Cursor` API surface carries the scan primitive
   unconditionally. Grammar-derived activation — the emitter
   chooses per-grammar-per-rule whether to wire the scan path based
   on CSP alphabet-density + digraph-signature facts. No
   mandatory whole-input prepass. No hardcoded grammar
   specialization.
7. **Consumer totality.** Every emitted substrate surface
   (`PROJECTION_DIRECT_TO_STRUCT` entry, `__grammar_projection_*`
   marker, `materialize_projection_*` helper, `<Grammar>Value`
   variant, structural-scan policy slot) has a production consumer.
   An admitted surface without a runnable helper and a call site
   fails the wave.
8. **Peer-first close gates.** Close criteria are keyed to
   external references, not internal ratios: sonic-rs + simd-json
   for JSON eager + parse matrix; lightningcss + cssparser for CSS
   typed semantic parity + canonical output; Sheets + BBNF own
   their own declared gates. JSON-only close is a miscalibration.
9. **BBNF, Sheets, CSS L4 are first-class peers to JSON.** Each
   grammar is benched, profiled with samply, expanded via
   `cargo expand`, and gate-verified at every wave boundary.
   No grammar is a "tail fixture" checked only at tranche close.

## Invariants

1. `Parsed::to_value()` contains no parse invocation, no
   `parse_with_visitor` call, no tape-reconstruction fallback.
   Evidence: `grep 'parse_with_visitor\|<.*Parser>::parse\b'
   crates/core/src/runtime/parsed.rs` returns zero matches.
2. `push_compound` absent from `TapeBuilder`'s public API and from
   every `quote!` block in the emitter. Evidence: `cargo expand`
   output shows zero `.push_compound(` calls.
3. `note_push` + `SIB_SKIP_STAMPED_BIT` absent from `tape.rs`,
   `builder.rs`, `finaliser.rs`. `nm` on bench binary confirms
   `note_push` symbol absent.
4. `Columns::rollback_to(open_offset)` is the only retry-path
   truncation surface. `columns_mut().truncate()` on raw
   lengths disappears from the emitter.
5. The fused pipeline is real: `<Grammar>::parse` constructs both
   tape and value in one walk; `Parsed::into_value()` / `to_value()`
   returns the already-constructed value without a second
   materialization pass.
6. `navigate_tape` absent from `runtime/path.rs`; the capability
   migrates to cursor API + emitted navigation primitives, not a
   dead free function.
7. Projection totality: `PROJECTION_DIRECT_TO_STRUCT.len() ==
   count of materialize_projection_* fns == count of production
   consumers` per grammar and in aggregate. Evidence: an assertion
   test `crates/core/tests/projection_totality.rs`.
8. Grammar-derived only: zero matches for
   `JsonParser|CssL4Parser|BbnfParser|GoogleSheetsParser` as
   dispatch predicates in emitter / runtime / tape source. No
   hand-written Rust type aliased to a grammar-declared name. All
   typed surfaces (including CSS `Color`, `Declaration`, `Value`,
   `Selector` families) emitted from CSP + egraph facts.
9. Typed CSS semantic parity vs lightningcss is a hard close gate
   (not an internal named-type list). Test surfaces:
   `crates/core/tests/lightningcss_parity.rs`,
   `crates/core/tests/css_l4_canonical_parity.rs`,
   `crates/core/tests/typed_accessor_surface.rs`.
10. `make ay-bench-close WAVE=close` runs clean on the full 5-bench
    fat-LTO matrix at every wave boundary (W0 close AND W1 close).
    No grammar is a skipped fixture.
11. Competitor benches published at W1 close:
    `crates/core/benches/json/competitors.rs` vs sonic-rs +
    simd-json; `crates/core/benches/css/competitors.rs` vs
    lightningcss + cssparser.
12. Workspace green at every commit. Bootstrap regen cycle-1 =
    cycle-2 byte-identical at both wave closes.
13. No recorded misses. A wave that cannot close triggers
    §Diagnostic-loop relinquish, not status drift.

## Operational posture

1. W0 dispatches 5 parallel sub-agents on disjoint file bounds
   per the decomposition in `waves/W0.md`. Each sub-agent owns a
   sub-gate verified by `cargo expand` inspection, a spot bench,
   and (where runtime-observable) samply attribution.
2. Fat-LTO bench matrix runs at W0 close AND W1 close — both are
   full 5-bench matrices (json_monolithic, css_l4,
   google_sheets_monolithic, bbnf_monolithic, compile_pipeline).
3. Samply coverage at every wave boundary spans all four primary
   grammars, not JSON alone: eager JSON twitter, CSS tailwind,
   Sheets parse_stress, BBNF bbnf_self. Each capture under
   `.profiles/samply/AY-II-<wave-label>/<grammar>/`.
4. `cargo expand` output is primary evidence per the
   audit-expand-begotten-code edict
   (`docs/instructions/README.md`). Source-only gates
   are not load-bearing.
5. Bootstrap regen runs at W0 close and W1 close; both cycles
   pass idempotency.

## Wave summary

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Fused substrate + emitter unification + runtime consolidation + projection totality + structural-scan integration (5 parallel sub-agents) | tranche open | in_progress |
| **W1** | [waves/W1.md](waves/W1.md) | JSON — semantic parity + peer-referenced perf (sonic + simd-json); grammar-derived typed admission totality; samply-proven hot path | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | CSS L4 — lightningcss total typed-semantic parity (Rule, Declaration, Value, Selector, MediaRule, Keyframes families); canonical-output byte parity; zero hardcoded bindings | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | Google Sheets — grammar-derived typed formula/cell/range families; self-parity + formula_expr parity; samply-proven hot paths | W2 | planned |
| **W4** | [waves/W4.md](waves/W4.md) | BBNF — self-hosting identity + grammar-meta typed surface; double-regen byte-identical; typed declared-type annotation coverage | W3 | planned |
| **W5** | [waves/W5.md](waves/W5.md) | Cross-grammar close matrix + competitor benches aggregated + FINAL + successor handoff | W4 | planned |

## Defensible floor

AY-II's defensible floor is not "architectural infrastructure
partially landed." The minimum closeable outcome is:

1. W0 lands every thesis invariant §1–9 end-to-end. The full
   fat-LTO bench matrix runs clean at W0 close.
2. W1 hits every peer-referenced close gate:
   - `bbnf_value_twitter / sonic_value_twitter ≤ 1.15`
   - `bbnf_value_canada / sonic_value_canada ≤ 1.20`
   - `bbnf_value_citm / sonic_value_citm ≤ 1.20`
   - 5-fixture value-lane geomean `≤ 1.20 × sonic`
   - bbnf at or below sonic + simd-json on the JSON parse
     competitor bench
   - lightningcss typed-parity tests green
     (`lightningcss_parity.rs` + `css_l4_canonical_parity.rs` +
     `typed_accessor_surface.rs`)
   - bbnf at or below lightningcss + cssparser on the CSS
     competitor bench for declared fixtures
   - BBNF + Sheets fat-LTO benches clean; their declared
     self-parity suites green

Anything less opens pass III, not a recorded miss.

## AY-II → BA / BB / BC handoff contract

AY-II does not close until:

1. Every AY-I routed gate in `../AY-I/FINAL.md` §Hard gates
   status table closes OR is retired with grammar-derived
   rationale recorded in AY-II/FINAL.md.
2. The default parse path constructs both tape and value in one
   walk; `Parsed::to_value()` does not reparse.
3. `cargo expand -p bbnf --bench json_monolithic` + `--bench css_l4`
   + `--bench google_sheets_monolithic` + `--bench bbnf_monolithic`
   each contain zero `push_compound`, zero `note_push`, zero
   `navigate_tape`, zero `parse_with_visitor` calls from the
   `to_value` path.
4. Fat-LTO `cargo bench` runs clean across all five bench
   binaries.
5. Projection totality invariant §7 verified at W0 close AND W1
   close.
6. Typed CSS semantic parity suite green with no `#[ignore]`
   added in AY-II.
7. `docs/tranches/AY-II/FINAL.md` authored against the close
   artefacts.
8. `docs/tranches/BA/BA.md`, `BB/BB.md`, `BC/BC.md` updated to
   reference AY-II (not AY) as their predecessor close; BA's
   beyond-parity thesis carries forward on the fused-substrate
   truth AY-II lands.

## Indefatigability

When AY-II closes correctly, bbnf has one parser, one substrate,
one semantic-construction path, one compound emission API, one
stamping path, one rollback primitive, one structural-scan
capability (grammar-activated), and one typed-semantic surface
that is grammar-derived for every grammar in the corpus. The
close ledger is peer-referenced, not internally ratio'd. BA
opens on a fused-pipeline substrate that holds the invariants
AY's original plan declared and goes beyond them at the peer
surface.
