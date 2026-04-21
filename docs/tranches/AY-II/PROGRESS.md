# AY-II — Progress Log

Dated execution log for tranche AY-II (pass II of AY; see
`../AY-I/FINAL.md` for pass-I close and `audit/AUDIT-{A,B,C,D}-*.md`
for the triumvirate that informs this pass).

- `Status`: planned
- `Current wave`: not started
- `Next wave`: W0

---

## Scaffold landing

AY-II opens at the commit that lands the split + this scaffold.
The four audit artefacts at `audit/AUDIT-{A,B,C,D}-*.md` were
cherry-picked from their worktrees during the pass-I → pass-II
transition and placed under this pass's `audit/` directory per
the multi-pass-tranche edict
(`docs/instructions/tranche/SPEC.md` §Multi-pass tranche split).

The plan (`AY-II.md`), wave specs (`waves/W0.md` + `waves/W1.md`),
and this PROGRESS were authored without an execution dispatch.
Any sub-agent dispatched into AY-II waves operates on the scaffold
as-is; mid-wave plan edits follow the SPEC §Scope-reveal protocol.

W7's preempted worktree from AY-I — and the four audit worktrees —
are discarded as part of this scaffold commit. The W7 draft fix
(`prev < new_idx` guard in `TapeBuilder::note_push`) is explicitly
abandoned; the architectural consolidation in W0 supersedes.

## Scaffold revision — gestalt tightening

A senior-perf-engineer pass against the initial AY-II scaffold
surfaced three architectural errors and three scope gaps. The
scaffold is rewritten accordingly:

- **Fused pipeline, not a second parse.** The initial scaffold
  routed `Parsed::to_value()` through `parse_with_visitor` — a
  second parse of the source. Corrected: `AY-II.W0.c` introduces
  `ValueBuilder<R>` parallel to `TapeBuilder`, constructing the
  `<Grammar>Value` in lockstep with the tape during the single
  parse pass. `to_value()` is a thin projector over the already-
  constructed value.
- **Peer-referenced close gates, not internal ratios.** The
  initial scaffold keyed close on `bbnf_value_* / sonic_value_*`
  only. Corrected: `AY-II.W1` adds `crates/core/benches/json/competitors.rs`
  + `crates/core/benches/css/competitors.rs` publications vs
  sonic-rs + simd-json + lightningcss + cssparser.
- **CSS typed-semantic parity as hard close gate.** Corrected:
  `AY-II.W0.d` extends grammar-derived typed projection to cover
  lightningcss's typed surfaces (rule, declaration, value,
  selector families); `AY-II.W1` gates on
  `lightningcss_parity.rs` + `css_l4_canonical_parity.rs` +
  `typed_accessor_surface.rs` all green.
- **Structural scan promoted, not retired.** Corrected:
  `AY-II.W0.e` migrates `StructuralIndex` + `scan_structural`
  into cursor API + emitted navigation primitives with
  grammar-derived activation policy (CSP-inferred
  alphabet-density + digraph-signature drives per-grammar-per-rule
  emission). `navigate_tape` as a dead free function dies; the
  capability lives.
- **Projection totality as hard invariant.** Corrected:
  `crates/core/tests/projection_totality.rs` asserts
  `PROJECTION_DIRECT_TO_STRUCT.len() == materializer count ==
  consumer count` per grammar. W0 close + W1 close both verify.
- **BBNF + Sheets + CSS L4 first-class peers to JSON.**
  Corrected: every wave boundary runs the full 5-bench fat-LTO
  matrix; samply per primary grammar (not JSON alone).

The main rewrite rule: AY-II closes on ONE path, no second parse
hidden in `to_value()`, no consumerless substrate surfaces, no
JSON-only parity close. All semantic information grammar-derived
via CSP + egraph — no hardcoded bindings for any grammar.
