# Tranche AY-III — Gestalt Continuation on the Post-B5 Substrate (Pass III)

**STATUS: DEFERRED — 2026-04-27.** Per the fifth /plan synthesis cycle,
AY-III's tape-substrate verification is on a deprecating substrate
(direct-to-struct in AZ-I supersedes the tape lane for the three
primary data grammars; AZ-II deletes the tape crate entirely).
Durable AY-III gates absorb forward as grammar-general infrastructure:
AY-III.W0 + W1 admission-totality + competitor-keyed close gates +
fused-pipeline wire contracts → AZ-I.W4 close ceremony; AY-III.W2
BBNF self-host identity gate → AZ-II.W2 post-cutover regen-check.
This document remains as historical record.

AY-III closes the AY arc on the substrate B5 delivered. Pass I
(`../AY-I/AY-I.md`, `../AY-I/FINAL.md`) proved the visitor-lane
shape reaches 0.99× sonic on eager JSON and broadened
direct-to-struct admission to 71 grammar-derived projections.
Pass II (`../AY-II-I/AY-II-I.md`) refined the wave structure
under the welded `FusedBuilder` substrate; its W0' close ceremony
folded into B4.W1, and the predecessor B-series (B1 → B3 → B4 →
B2 → B5 → B6) retired six post-B4 architectural smells along
with the dev-loop drag that would have inflated AY-II-I's
remaining waves. AY-III opens against the post-B5 / post-B6
surface where the substrate is one type — `Tape<R>` over
`Columns` — with a two-method parser-substrate boundary, the
proc-macro IR-pipeline contract is gone, the cold xtask wall has
collapsed 192×, and `cargo xtask regen --check` is the canonical
self-host identity gate.

## Architectural thesis

AY-III is verification work where W0 lands; architectural work
where W1 closes. One substrate (Tape<R> over Columns;
FusedBuilder retired). One wave atomicity rule (substrate +
consumer + wire-contract test in same commit). Peer-first close
gates (sonic-rs, simd-json, lightningcss, cssparser; no internal
ratios). Performance trajectory: twitter `bbnf_value` 548 →
≥ 2236 MB/s (the AY-II-I.W1 floor); 4.1× recovery requires
architectural levers (direct-to-struct, specialised inner loops,
IR-derived dispatch activation), not parity polish. The
defensible floor is twitter ≥ 1500 MB/s (≤ 1.50× sonic) with the
remaining lever routing to AZ-I.W2 by named destination; below
floor the AY arc closes on escape per FINAL's deferred ledger.

## Invariants

1. **Substrate-with-consumer atomic per wave.** Every `pub
   const`, dispatch table, projection, or materialiser
   introduced in wave N has a runtime consumer plus a
   wire-contract test landing in the same commit. Verifier:
   `<wave>_admission_totality.rs` per grammar (admission count
   == materialiser count == consumer count).
2. **Predicate-symmetry on gates.** Gate predicates are frozen
   at the introducing wave; widening requires re-plan. PROGRESS
   records every predicate at wave open AND wave close — the
   diff must be empty.
3. **Peer-first close gates only; no internal ratios.**
   `bbnf_value_*` keyed externally (`sonic_value_*`,
   `lightningcss_typed_*`); BBNF self-host identity keyed
   against `cargo xtask regen --check` byte-equal.
4. **Wire-contract test per wave artefact.** N artefacts → N
   tests, each named `<wave>_<artefact>_wire_contract`,
   asserting runtime invocation via instrumented call counter,
   samply attribution, `nm` symbol presence, OR the called-fn
   count via `cargo expand` post-process.
5. **Workspace nextest 1477+/1477+ green at every wave close.**
   Floor is the post-B6 baseline.
6. **Bench non-regression vs B5 baseline.** `compile_bbnf`
   median ≤ 2.806 ms × 1.05 at every wave close; full
   close-matrix at tranche close.
7. **No `#[allow(...)]` outside macros.** AY-II-I.W0.e shipped
   `STRUCTURAL_SCAN_POLICY` with `#[allow(dead_code)]`; AY-III
   prohibits the pattern in source.
8. **No new features.** Grammar `.bbnf` edits are out of scope;
   CSS L4 grammar extensions land only when an admission already
   shapes the rule.
9. **No silent deferrals.** Items not landed go to FINAL's
   deferred ledger with named destination tranche plus
   rationale.
10. **`extern crate self as bbnf` does not return.** Any wave
    whose surgery would require restoration is a
    substrate-pivot re-plan signal, not a workaround.

## Operational posture

1. Three waves — W0 (JSON closure), W1 (CSS + Sheets + AZ-I
   baseline), W2 (BBNF + close ceremony) — execute sequentially
   on a single substrate. No prelude annex; B6's dev-loop annex
   already closed the iteration drag.
2. Fat-LTO 5-bench matrix runs at W0 close, W1 close, AND W2
   close; the W2 capture is the tranche-close artefact published
   to `docs/benchmarks/post-AY-III.json`.
3. Samply coverage spans the four primary grammars at every wave
   boundary. Captures ride the prepared-binary surface
   (`make ay-prepare-profile-wave`) and land under
   `.profiles/samply/AY-III-<wave>/<grammar>/`.
4. `cargo expand` output is primary evidence per the
   audit-expand-begotten-code edict; source-only gates are not
   load-bearing.
5. `cargo xtask regen --check` runs at every wave close and at
   tranche close. The B6.W0 content-equality skip preserves
   mtime so cargo reuses cached `bbnf` rmeta — close ceremony
   walls run in seconds, not minutes.

## Wave summary

| Wave | Spec | Headline | Agents | Hard gate (selected) | Status |
|---|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | JSON closure on the post-B5 substrate. Admission totality + competitor lane + samply (consolidated to 2 gates from 5 fixtures) + parity-test tightening + fused-pipeline wire contract. | 4 parallel | `bbnf_value_twitter ≤ 1.15× sonic`; canada / citm ≤ 1.20×; geomean ≤ 1.20×; **floor at AY-III close: ≤ 1.50** (twitter ≥ 1500 MB/s); `json_admission_totality.rs` green; `json_fused_pipeline_parsecount.rs` green (1 parse per `to_value`); `nm`: retired surfaces absent, post-B5 surfaces present. | planned |
| **W1** | [waves/W1.md](waves/W1.md) | CSS L4 typed parity + Sheets self-parity + AZ-I.W0 baseline absorption. Consolidates AY-II-I.W2 + W3 + the AZ-I.W0 baseline-bench MERGE. | 5 parallel | lightningcss + cssparser + Sheets parity tests green at fat-LTO; CSS `materialize_projection_*` admission totality wire-contract; `make ay-bench-close WAVE=W1-close` clean across 5 binaries; AZ-I.W0 baseline at `docs/benchmarks/post-AZ-I-W0-baseline.json`. | planned |
| **W2** | [waves/W2.md](waves/W2.md) | BBNF self-host identity + close ceremony + competitor matrix. Folds AY-II-I.W4 + W5. | 3 parallel + 1 serial closer | `cargo xtask regen --check` exit 0; `bbnf_parity` + `bbnf_ast_parity` + `bbnf_self_parity` + `grammar_roundtrip` green; `bbnf_admission_totality.rs` green (1:1:1); full fat-LTO 5-bench matrix at ≤ 5 % B5 regression; competitor benches published; `AY-III/FINAL.md` lands. | planned |

## Critical files

| File | Owning wave | Purpose |
|---|---|---|
| `crates/core/tests/json_admission_totality.rs` | W0 | JSON 1:1:1 admission ↔ materialiser ↔ consumer wire contract |
| `crates/core/tests/json_fused_pipeline_parsecount.rs` | W0 | Asserts parse-root invocation count equals `to_value()` count |
| `crates/core/benches/json/competitors.rs` | W0 | bbnf vs sonic-rs, simd-json, serde_json, jiter |
| `crates/core/benches/json/value.rs` | W0 | Eager JSON value-lane bench |
| `crates/core/tests/css_admission_totality.rs` | W1 | CSS L4 1:1:1 wire contract |
| `crates/core/tests/lightningcss_parity.rs` | W1 | CSS typed AST parity vs lightningcss |
| `crates/core/tests/css_l4_canonical_parity.rs` | W1 | Byte-identical canonical CSS |
| `crates/core/tests/typed_accessor_surface.rs` | W1 | Typed-accessor totality |
| `crates/core/tests/sheets_parity.rs` | W1 | Sheets self-parity |
| `crates/core/tests/sheets_expr_parity.rs` | W1 | Sheets expression-tree parity |
| `crates/core/benches/css/competitors.rs` | W1 | bbnf vs lightningcss, cssparser |
| `crates/core/tests/bbnf_admission_totality.rs` | W2 | BBNF 1:1:1 wire contract |
| `crates/core/tests/bbnf_parity.rs` | W2 | BBNF Span-leaf parity |
| `crates/core/tests/bbnf_ast_parity.rs` | W2 | BBNF AST projection parity |
| `crates/core/tests/bbnf_self_parity.rs` | W2 | Self-hosting identity at runtime |
| `crates/core/tests/grammar_roundtrip.rs` | W2 | Cross-grammar roundtrip + prettify-idempotency |
| `xtask/src/regen.rs` | W2 | `cargo xtask regen --check` self-host identity gate |
| `docs/benchmarks/post-AY-III.json` | W2 | Tranche-close 5-bench matrix |
| `docs/benchmarks/post-AZ-I-W0-baseline.json` | W1 | AZ-I.W0 baseline-bench MERGE artefact |
| `docs/tranches/AY-III/FINAL.md` | W2 | Tranche-close ceremony |

## Hard gates summary

### W0 — JSON closure

1. `bbnf_value_twitter / sonic_value_twitter ≤ 1.15` at fat-LTO.
2. `bbnf_value_canada / sonic_value_canada ≤ 1.20`.
3. `bbnf_value_citm / sonic_value_citm ≤ 1.20`.
4. 5-fixture value-lane geomean `≤ 1.20 × sonic`.
5. bbnf parse at or below sonic-rs on each competitor fixture;
   within 2× simd-json (parse-only).
6. `cargo test --test sonic_rs_parity --release` green; zero
   new `#[ignore]`.
7. `cargo test --test json_canonical_parity --release` green
   (data_xl passes).
8. `cargo test --test value_api_apples_to_apples --release`
   green including `beat_sonic_twitter_eager` un-ignored at
   ≤ 1.50 sanity floor.
9. `cargo test --test json_admission_totality --profile
   ax-iter` green; `cargo expand`-derived materialiser count
   equals admission count equals consumer count.
10. `cargo test --test json_fused_pipeline_parsecount
    --release` green on twitter / canada / citm — parse-root
    invocation count equals `to_value()` invocation count.
11. `nm` on bench binary: `note_push`, `value_frame_at`,
    `parse_with_visitor_JsonParser`, `ValueBuilder::*`,
    `push_compound`, `navigate_tape` absent;
    `Tape::push_leaf_*`, `Tape::begin_compound`,
    `Tape::end_compound`, `Tape::position`,
    `Tape::rollback_to` present.
12. Twitter samply top-5 names emitted `parse_*_JsonParser_*`
    + at least one `Tape<R>` symbol; retired-surface symbols
    absent from any `to_value` lineage.

### W1 — CSS L4 typed parity + Sheets + AZ-I baseline

1. `cargo test --test lightningcss_parity --release` green
   with `color_channel_parity_all_families`, `selector_parity`,
   `media_query_parity`, `property_parity`. Zero `#[ignore]`.
2. `cargo test --test css_l4_canonical_parity --release` green
   on the post-W1 corpus (`canonical_parity_normalize` plus
   per-feature fixtures).
3. `cargo test --test css_l4_parity --release` green;
   `cargo test --test css_l4_named_color_parity --release`
   green on the 148 named colors.
4. `cargo test --test typed_accessor_surface --profile ax-iter`
   green; CSS L4 floors match post-W1 exact counts.
5. `cargo test --test css_admission_totality --profile ax-iter`
   green: `materialize_projection_*_CssL4Parser` count equals
   `PROJECTION_DIRECT_TO_STRUCT.len()` equals consumer count.
6. `cargo test --test sheets_parity --profile ax-iter` green;
   `sheets_expr_parity` green with ≥ 9 new field-for-field
   `SheetsGValue` assertions; `sheets_self_parity` green on the
   full corpus; zero new `#[ignore]`.
7. `cargo test --test sheets_parse_nested_no_panic --profile
   bench` green — no panic on any line of
   `data/sheets/nested.txt` under fat-LTO.
8. bbnf CSS parse at or below cssparser on
   `normalize` / `bootstrap` / `tailwind`; within 2×
   lightningcss on the same fixture set.
9. Samply per CSS fixture (`tailwind`, `bootstrap`,
   `normalize`) shows fused-pipeline hot-path ownership
   (`Tape<R>` symbols + emitted `parse_*_CssL4Parser_*` in
   top-5); zero `materialize_projection_*` self-time on the
   parse path; zero retired-surface symbols.
10. Samply per Sheets fixture (`simple`, `nested`, `stress`)
    shows fused-pipeline hot-path ownership; same retirement
    audit holds.
11. `make ay-bench-close WAVE=W1-close` clean across 5 bench
    binaries; no panic on any grammar.
12. `docs/benchmarks/post-AZ-I-W0-baseline.json` exists with
    classifier-unification audit data + IR audit data + the 5
    fixture-baseline numbers AZ-I.W0 claims as its baseline.
13. `nm` on bench binaries: retired surfaces absent on every
    grammar; post-B5 surfaces present.

### W2 — BBNF self-host + close ceremony

1. `cargo xtask regen --check` exits 0 (the post-B2 native
   self-host identity gate). Artefact:
   `docs/benchmarks/post-AY-III-W2-regen-diff.txt` (empty
   content = positive).
2. `cargo test --test bbnf_parity --release` green;
   `cargo test --test bbnf_ast_parity --release` green with
   field-layout parity over every BBNF admission;
   `cargo test --test bbnf_self_parity --release` green on the
   full multi-grammar corpus; zero new `#[ignore]`.
3. `cargo test --test grammar_roundtrip --release` green —
   every `@pretty`-bearing grammar re-emits byte-identically
   across the round trip.
4. `cargo test --test bbnf_admission_totality --profile
   ax-iter` green: BBNF axis count agrees 1:1:1 at the post-W2
   totality value (≥ 10 admissions floor).
5. `make ay-bench-close WAVE=W2-close` clean across 5 bench
   binaries; `compile_bbnf` median ≤ 2.806 ms × 1.05.
6. Samply on `bbnf_self` shows fused-pipeline attribution; zero
   retired-surface attribution in the `to_value` lineage.
7. `cargo test --workspace --no-fail-fast` returns 0 failures;
   workspace nextest 1477+/1477+.
8. Competitor benches published:
   `docs/benchmarks/post-AY-III-competitors-json.txt` (bbnf vs
   sonic-rs + simd-json) and
   `docs/benchmarks/post-AY-III-competitors-css.txt` (bbnf vs
   lightningcss + cssparser).
9. `docs/tranches/AY-III/FINAL.md` exists with invariant
   verification table, hard-gate status table, commit ledger,
   debt reconciliation, and successor handoff.
10. `docs/tranches/AZ-I/AZ-I.md`, `AZ-II/AZ-II.md`, `BA/BA.md`,
    `BB/BB.md` reference AY-III close commit (not AY-II-I); the
    fused-pipeline substrate truth carries forward in their
    opening preconditions.

## Audit consolidation — DROPS, MERGES, PROMOTE, REFINES

The AY-II-I audit triumvirate (audits A/B/C/D under
`../AY-II-I/audit/`) inventoried 18 cross-pass debt items; the
post-B5 / post-B6 substrate retires several at source. Carry
disposition:

### DROPS (9)

1. **AY-II-I.W0' close ceremony** — folded into B4.W1
   (2026-04-25). Substrate folded; nothing for AY-III to redo.
2. **W1.§1 emit-time `parse_with_visitor` retirement** — B5.W1
   retired the weld; the fused parse IS the visitor lane.
3. **W2 `__named_type_shim_color` retirement** — B5.W0
   cluster-A peel collapsed the divergent peel routines into
   `view/peel.rs`; the shim retires at source.
4. **W4.a §3 AX.W0a one-shot escape recipe doc** — B2 retired
   the proc-macro IR-pipeline that made the recipe relevant;
   `cargo xtask regen` is now native.
5. **AZ-I.W0 derive-cache + Watt sub-agents** —
   T3-superseded post-B2 (no proc-macro to relocate the cache
   for, no proc-macro to wrap with Watt).
6. **W2 OUT-OF-SCOPE rows enumeration** —
   `CounterStyleRule` / `ScopeRule` etc. defer to FINAL's
   deferred ledger with a named BA destination, not to the
   wave's scope text.
7. **W4.d conditional `grammar_roundtrip.rs`** — pre-resolve at
   W2.b open. The AY-II-I-era conditional is moot under the
   post-B5 fused-substrate truth: prettify rides the same value
   surface; the harness is unconditional.
8. **W5 `cargo asm` on the unified compound close symbol** —
   samply attribution + `nm` symbol audit cover the same proof
   surface; `cargo asm` adds no incremental evidence.
9. **W5 cycles-per-byte capture** — superseded by per-fixture
   MB/s plus peer-ratio publication in the close-matrix
   competitor benches.

### MERGES (7)

1. **AY-II-I.W2 + W3 → AY-III.W1.** CSS L4 typed parity and
   Sheets self-parity share the same substrate-truth proof
   surface; one wave with five parallel agents replaces two
   waves with eight aggregate dispatches.
2. **AY-II-I.W4 + W5 → AY-III.W2.** BBNF self-host identity
   plus close ceremony fold into one wave; the close ceremony's
   bench / samply / FINAL composition is the wave's serial
   closer.
3. **AZ-I.W0 baseline-bench → AY-III.W1 final batch.** AZ-I.W0
   produces baseline-bench artefacts that AY-III's W1 close can
   capture in the same fat-LTO matrix run; the merge eliminates
   redundant bench cycles.
4. **AY-II-I.W1.b + W1.c competitor + samply tooling →
   AY-III.W0.b + W0.c.** The prebuilt-binary samply tooling
   B0/B1 delivered makes the W1.c-era separate sub-agent
   redundant; W0.c folds into one agent across the five JSON
   fixtures.
5. **AY-II-I.W2 lightningcss audit + named-types-carve →
   AY-III.W1.a + W1.b.** The audit CSV plus the
   `view/named_types.rs` carve compose into one parallel pair;
   the audit drives the carve, not a separate wave.
6. **AY-II-I.W4.b BBNF totality + W4.c `@pretty` byte-identity
   → AY-III.W2.** Both gate the BBNF self-host identity; one
   close ceremony covers both.
7. **AY-II-I.W4.e BBNF samply + W5 BBNF samply → AY-III.W2.c.**
   Single samply capture per primary grammar at tranche close
   replaces the per-wave-then-tranche-close duplication.

### PROMOTE (1)

1. **AY-II-I.W1.d wire-contract test** → AY-III invariant 4
   universal. Per-wave per-artefact wire-contract tests are
   mandatory across every grammar's close; the AY-II-I-era
   single-test-per-grammar form generalises.

### REFINES (6)

1. **W0 floor named at 1500 MB/s.** AY-II-I.W1 stated
   "≤ 1.50 sanity floor" inline; AY-III names it as a
   defensible-floor numeric (twitter ≥ 1500 MB/s) with explicit
   escape clause to AZ-I.W2.
2. **W2 byte-identity gate is `cargo xtask regen --check`.**
   Post-B2 native; supersedes AY-II-I.W4's bash-bootstrap
   cycle-1 ≡ cycle-2 contract.
3. **Invariant 7 prohibits `#[allow(dead_code)]`.** AY-II-I.W0.e
   shipped `STRUCTURAL_SCAN_POLICY` with the attribute as a
   stop-gap; AY-III prohibits the pattern.
4. **Invariant 8 prohibits new features.** Per the user
   directive; CSS L4 grammar extensions land only when an
   admission already shapes the rule.
5. **Bench-non-regression refers to B5 baseline (not B4).**
   `compile_bbnf` median ≤ 2.806 ms × 1.05 at every wave close
   per B5/FINAL.md table.
6. **Competitor bench against simd-json clarifies "within 2×
   simd-json" as parse-only.** The value-lane gate is
   sonic-only (sonic-rs is the value-construction peer;
   simd-json's SIMD lead is parse-only ceiling).

## Cross-tranche debt

### Inherited (AY-II-I → AY-III)

- Twitter recovery 548 → ≥ 1500 MB/s floor, ≥ 2236 MB/s
  declared. Architectural levers route to AZ-I.W2 below floor
  per FINAL's deferred ledger.
- CSS L4 has 3 known deep driver gaps at AY-II-I read time
  (`project_css_typed_codegen` lineage). Floor: CSS struct-only
  with named semantic gaps; full lightningcss parity is
  BA-scope.
- `<Grammar>Value::Unknown` per-grammar exception ledger:
  retire where totality holds; record per-grammar retention
  rationale in the W2 close audit.
- `STRUCTURAL_SCAN_POLICY` consumer-wiring decision (deferred
  from AY-II-I.W0' / W0.e). AY-III.W0.c reads the policy at
  emit time per AY-II-I AUDIT-C §3.2, OR retires the surface
  entirely. Decision lands at W0 close; policy-retention is
  load-bearing if and only if samply shows a measurable
  hot-path admission, otherwise the policy retires per
  invariant 7.

### Forwarded (AY-III → successor)

- Direct-to-struct activation for JSON / Sheets / CSS L4 →
  AZ-I.W1 + W2 + W3.
- `crates/tape/` deletion → AZ-II.W3.
- Lazy typed pointer-path queries → BA.
- E-graph rule inference + VM oracle → BB.

## Defensible floor

**Minimum acceptable AY-III close:**

1. Twitter `bbnf_value_twitter` ≥ 1500 MB/s
   (≤ 1.50× sonic_value_twitter).
2. 5-fixture value-lane geomean ≤ 1.50× sonic.
3. CSS / Sheets / BBNF parity tests green; zero new
   `#[ignore]` on any parity test.
4. No recorded misses on per-grammar admission totality
   (1:1:1 holds per grammar).
5. `cargo xtask regen --check` exit 0; workspace nextest
   1477+/1477+ green; `compile_bbnf` median ≤ 2.806 ms × 1.05.
6. FINAL.md lands; competitor benches published.

**Below floor (escape clause).** AY arc closes on escape per
FINAL's deferred ledger; the architectural lever
(direct-to-struct, specialised inner loops, IR-derived
dispatch activation) routes to AZ-I.W2 with named destination.
The deferred ledger names the unmet floor item, the
diagnostic artefact (samply summary, bench delta, expand
slice), and the destination tranche.

**Above floor + below declared
(1500 ≤ twitter < 1900 MB/s).** AY-III closes on floor; AY-IV
opens iff a measurable architectural lever surfaces and is not
AZ-I-scope. The discriminator is mechanical: a lever whose
prescribed work fits in a single AY-IV wave without silent
deferral opens AY-IV; otherwise AZ-I.W2 inherits.

## Risks (named)

1. **Twitter 4.1× under floor requires architectural levers.**
   The AY-II-I baseline measured `bbnf_value_twitter`
   effectively at the panic ceiling (`to_value()` panicked on
   empty slab); the post-B5 substrate restores the value lane,
   but the gap from current parse-only ~688 MB/s (AY-I.W1
   ceiling) to the AU-baseline 1967 MB/s is the headline
   recovery. Direct-to-struct, specialised inner loops via
   IR-derived dispatch activation, per-grammar
   `STRUCTURAL_SCAN_POLICY` activation are the named levers.
   The AY-III floor at 1500 MB/s is the declared sanity
   threshold; AZ-I.W2 owns full AU-baseline recovery.
2. **Substrate-with-consumer atomicity at high agent count.**
   W1 dispatches 5 agents; Era V's chronic anti-pattern (AW-V's
   substrate-without-consumer) recurred at AW-V despite the
   post-AU prescription. AY-III invariant 1 binds at every wave
   close, not at tranche close.
3. **CSS L4 vs lightningcss has 3 known deep driver gaps.**
   Floor: CSS struct-only with named semantic gaps; full
   lightningcss parity is BA-scope. The gap enumeration lands
   in `docs/tranches/AY-III/audit/` if surfaced, with named
   destination tranche.
4. **BBNF self-host at W2.** `cargo xtask regen --check`
   non-zero triggers diagnostic-loop relinquish per SPEC
   §Diagnostic-loop relinquish; the orchestrator dispatches a
   research + plan + redress triumvirate landing under
   `audit/`. The post-B6.W0 content-equality skip preserves
   regen-cycle stability at seconds-cost wall, so a relinquish
   converges in one ceremony cycle if it fires.

## Indefatigability

When AY-III closes correctly, bbnf has one parser, one
substrate (`Tape<R>` over `Columns`), one semantic-construction
path (the fused parse), one compound emission API
(`begin_compound` / `end_compound` / `end_compound_post_order`),
one stamping path (`finaliser::finalise`), one rollback
primitive (`Tape::rollback_to`), one position accessor
(`Tape::position`), one structural-scan capability
(grammar-activated where samply-justified, otherwise retired),
one typed-semantic surface that is grammar-derived for every
grammar in the corpus, and one self-host identity gate
(`cargo xtask regen --check`). The close ledger is
peer-referenced — sonic-rs / simd-json for JSON, lightningcss /
cssparser for CSS, BBNF self-host for the meta-grammar — not
internally ratio'd. AZ-I opens on a fused-pipeline substrate
that holds the invariants AY's original plan declared and
extends to direct-to-struct activation across the data
grammars.
