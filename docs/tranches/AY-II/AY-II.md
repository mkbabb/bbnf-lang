# Tranche AY-II — Fused Substrate + Full Grammar-Derived Semantic Parity (Pass II)

AY-II executes the architectural close of AY. Pass I
(`../AY-I/AY-I.md`, `../AY-I/FINAL.md`) proved the emitted visitor-
lane shape reaches 0.99× sonic on eager JSON, mapped the
`note_push` regression root cause, and broadened direct-to-struct
admission to 71 grammar-derived projections. The 4-agent audit
triumvirate at `audit/AUDIT-{A,B,C,D}-*.md` diagnosed the
remainder. AY-II lands it on one path, grammar-derived, close-gated
against real semantic peers — not internal ratios.

AY-II now executes as W0 (partial) → W0' → W1 → W2 → W3 → W4 → W5.
W0' source landings are in; B1 (prelude annex), B3 (parser-baseline
restoration), and B2 (build-time codegen transposition) close in
sequence as predecessors over the proof surface; AY-II resumes
immediately afterward on the refreshed substrate where `cargo xtask
regen` is the canonical regen entrypoint, the proc-macro IR-pipeline
contract is retired, and the pre-B2 80-min cold rustc-side wall has
ceased to exist. Every AY-II close gate remains a semantic or runtime
fact verifiable against an external reference (sonic-rs, simd-json,
lightningcss, cssparser) or a grammar-derived totality check
(projection counts, consumer counts, materializer counts in agreement).

## Architectural thesis

1. **ONE parse pass.** The default parser writes the canonical
   tape substrate AND constructs the `<Grammar>Value` semantic
   surface in a single pass (fused pipeline). `Parsed::to_value()`
   is a thin projector over the already-constructed value — it
   does **not** reparse, does **not** drive a second visitor pass,
   does **not** walk the tape to reconstruct children per compound.
2. **ONE substrate.** The tape remains the canonical structural
   substrate; the value surface is populated in lockstep through the
   fused builder/value-column path, not a second parser or a post-parse
   tape walk.
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
    fat-LTO matrix at every wave boundary (W0' close AND W1 close).
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

1. W0 is retained as the partial landing record in `waves/W0.md`;
   it is not an active dispatch surface. Active execution resumes at
   W0' close on the B1-refreshed proof surface, then proceeds
   sequentially through W1-W5.
2. Fat-LTO bench matrix runs at W0' close AND W1 close — both are
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
5. Bootstrap regen runs at W0' close and W4 close; both cycles
   pass idempotency.

## Wave summary

W0' and W1-W5 all gate on the predecessor close sequence
B1 → B3 → B2. See `PATH-FORWARD.md` for program order:
B1 close → B3 close → B2 close → AY-II.W0' compressed-honest close →
AY-II.W1-W5 sequential.

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Fused substrate + emitter unification + runtime consolidation + projection totality + structural-scan integration (5 parallel sub-agents) | tranche open | superseded — partial landing retained; W0' owns close |
| **W0'** | [waves/W0p.md](waves/W0p.md) | FusedBuilder collapse + projection-consumer wiring + scan-policy splice + legacy-cruft deletion (3 parallel sub-agents + d-lineage dev-infra follow-ons) | W0 partial landing | complete — close ceremony folded into B4.W1 (2026-04-25); unified `builder.rollback_to(...)` atomic-tape+value path lands the contract the W0'.a substrate shipped without; transitional aliases retire entirely; 327-failure runtime-parser regression resolves; `cargo xtask regen --check` clean. See `docs/tranches/B4/audit/W1-close.md` |
| **W1** | [waves/W1.md](waves/W1.md) | JSON — semantic parity + peer-referenced perf (sonic + simd-json); grammar-derived typed admission totality; samply-proven hot path | W0' close | planned |
| **W2** | [waves/W2.md](waves/W2.md) | CSS L4 — lightningcss total typed-semantic parity (Rule, Declaration, Value, Selector, MediaRule, Keyframes families); canonical-output byte parity; zero hardcoded bindings | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | Google Sheets — grammar-derived typed formula/cell/range families; self-parity + formula_expr parity; samply-proven hot paths | W2 | planned |
| **W4** | [waves/W4.md](waves/W4.md) | BBNF — self-hosting identity + grammar-meta typed surface; double-regen byte-identical; typed declared-type annotation coverage | W3 | planned |
| **W5** | [waves/W5.md](waves/W5.md) | Cross-grammar close matrix + competitor benches aggregated + FINAL + successor handoff | W4 | planned |

## Plan-audit findings (pre-W0' dispatch)

Pre-W0'-dispatch read of the remaining wave specs + BA / BB / BC
openers surfaced latent legacy / deferral markers whose resolution
belongs in this tranche, not downstream. The findings:

### Legacy references requiring update under W0'

1. **`parse_with_visitor` opt-in bench entry** — W1/W4/W5 plans
   permit it as a bench-only alternative entry point with the
   constraint that it does not appear in `Parsed::to_value()`'s
   call graph. Under W0' FusedBuilder, the fused parse IS the
   visitor lane; the separate `parse_with_visitor_<Grammar>`
   emission is redundant scaffolding. **Action**: W0'.a retires
   the entry point + its `emit_parse_with_visitor` emitter path at
   `crates/core/src/backend/rust/emitter/grammar.rs:1163-1334`.
   Update W1 §6 + W4.e + W5 §15 accordingly at the same commit.
2. **`<Grammar>Value::Unknown` fallback** — W1 §scope reference,
   W3 §scope reference. Under W0'.b projection-consumer wiring
   with totality holding per-grammar, the Unknown fallback has no
   un-admitted rules to serve. **Action**: W0'.b retires per
   grammar; where a grammar carries genuinely un-admitted rules
   (known subset), the retention is justified in a per-grammar
   exception ledger inside the W0' return report.
3. **W4.d conditional `grammar_roundtrip.rs`** — W4's scope
   authored the harness as "iff the fused-pipeline value-lane
   diverges semantically from combinator-side prettify at W0
   close". W0' resolves the precondition. **Action**: the
   decision is made at W4 open based on observed FusedBuilder
   behaviour; W4.d text updates at W0' close to remove the
   conditional.
4. **W4.e samply expectations** — currently reference
   `ValueBuilder::push` or "W0.c's landed name". **Action**:
   W4.e updates at W0' close to reference the concrete fused
   symbol name (`FusedBuilder::push_leaf_*` / `begin_compound`).
   **Status**: landed preemptively at `waves/W4.md:20,136,161`
   during the AY-II/B1 redress pass.

### Unnamed deferrals in AY-II plans

5. **W3 `TODO AU.6.7` aggregate / variant-tagged forms** — W3's
   grammar has `cell_ref`, `range_end`, and similar rules whose
   declared canonical target is an aggregate tuple or tagged
   union that the current `type_annotation` grammar cannot
   express. The deferral ledger in W3.a is specified but the
   destination tranche is not explicitly named in the wave text.
   **Action**: W3 plan update names BA as the destination (BA
   owns `type_annotation` grammar extensions per BA.md §Scope).

### AUDIT-D 15-item debt ledger mirror

`audit/AY-II-AUDIT-D-predecessor-successor.md` enumerates 15
cross-tranche debt items (11 AY-II internal, 4 external).
Source is authoritative; this is the enumeration mirror for
status-scanning purposes. Transcribe on next editorial pass;
currently the count is tracked but individual items are not
surfaced outside the audit doc. See U2 / DC1 in
`docs/tranches/meta-audit/03-tranche-drift.md`.

### Scoped deferrals correctly routed (no action needed)

6. **W2's `scale_interop_tailwind` calc-evaluator gap** →
   destination BA (calc-evaluator workstream). W2 plan explicit
   + BA plan inherits. Clean.
7. **W2 OUT-OF-SCOPE rows** (`CounterStyleRule`, `ScopeRule`,
   etc.) — zero matches in declared fixtures; admission via
   `genericAtRule` fallback; typed parity in BA scope. Clean.

### d-lineage scope-creep rationalisation

W0'.d4-d7 landed edits to `.cargo/config.toml` + proc-macro
dev-deps (`PROGRESS.md:287-310`). W0 §File bounds declared
`.cargo/config.toml` and `scripts/*` out-of-bounds
(`waves/W0.md:75-81`). The pivot was documented in
`audit/W0p-infra-root-cause.md` +
`audit/W0p-infra-fix-plan.md` +
`audit/W0-iter-surface-verification.md`, and the tranche
response was to promote B1 from scaffold to authoritative
prelude annex. Per SPEC §new-tranche-new-doc, a mid-tranche
scope pivot opens a new letter — B1 is that letter. W0'
does not re-audit d4-d7 at close; B1.W0 owns the re-audit
(`docs/tranches/B1/waves/W0.md`).

### Downstream-tranche audit

8. **BA inv 7** ("BA does not inherit unfinished AY parity debt")
   currently FAILS under W0 landed state (`to_value()` panics).
   **Action**: W0' close satisfies this invariant; BA open is
   unblocked at W0' close.
9. **BA/W3 file-bound** flagged for re-home in BA scrub
   (originally cited a path that does not exist; BA's own scrub
   redirects to the post-B2 sub-crate substrate).
10. **BB** — gates on BA close; no AY-II-specific dependency
    beyond BA's opening. Clean.

### `f372e7ef` history disposition

Transient hand-patch at commit `f372e7ef` persists only at that
commit; master HEAD's `generated.rs` has proper regen. Per AUDIT-C
Q4 + AUDIT-D Q7, history retained (rebase would break predecessor
audit SHA citations). Forward dispatches adopt cherry-pick-compose
discipline so future waves do not re-introduce transient
hand-patches.

## Defensible floor

AY-II's defensible floor is not "architectural infrastructure
partially landed." The minimum closeable outcome is:

1. W0' closes every thesis invariant §1–9 end-to-end. The full
   fat-LTO bench matrix runs clean at W0' close.
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
5. Projection totality invariant §7 verified at W0' close AND W1
   close.
6. Typed CSS semantic parity suite green with no `#[ignore]`
   added in AY-II.
7. `docs/tranches/AY-II/FINAL.md` authored against the close
   artefacts.
8. `docs/tranches/BA/BA.md`, `BB/BB.md` updated to
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
