# Tranche AY — Canonical Packed Substrate and Near-Parity Closure

AY is the performance-closure tranche. It is no longer the old
"BEAT-sonic by 20-40%" portfolio, and it is no longer a mixed
projection/tooling/compile-time bucket. AY has one job:

**land one parser, one canonical packed runtime substrate, and one
grammar-derived hot path that brings JSON to near sonic-rs parity while
preserving first-class view/debug/incremental viability on that same
substrate.**

AY operates from the current post-W4 state:

- W0-W4 already landed substantial recovery work.
- JSON is on the non-legacy RD/shape-emitted path.
- The dominant remaining loss is not "missing more mechanisms"; it is
  that the canonical runtime contract is still too tape-first,
  compatibility-first, and reconstruction-heavy.

AY therefore absorbs the above critique directly:

- no orthogonal parse paths,
- no hand-written grammar-specific semantic parser logic,
- no revival of DTA or PSI as dominant runtime substrates,
- no compile-time/toolchain work unless it is necessary to iterate on
  AY's parity-critical hot path,
- no replay/recovery/incremental scope in AY beyond the provenance
  minimum required to avoid painting AZ into a corner.

AY succeeds if the default JSON parser path is architecturally
`sonic-rs`-class, informed by `simdjson`-class structural leverage,
and enriched only by those bbnf-native ideas that survive as profitable,
fully general mechanisms.

## Architectural thesis

Five propositions govern AY.

1. **One path, one substrate.** There is one parser entry path and one
   canonical runtime substrate. `view()`, `to_value()`, `get()`, debug
   UX, and future incremental/replay consumers all read the same output.
   No tape-path/document-path duality is permitted.

2. **The target is a generalized hybrid, not a copy.** AY should absorb:
   - sonic-rs-class direct hot-path construction,
   - simdjson-class structural skip/count/index ideas where they improve
     the same runtime path,
   - bbnf-native direct-to-struct, Pratt flattening, structural mining,
     and globally informed optimizer decisions,
   while remaining grammar-derived and not grammar-handwritten.

3. **BBNF is the semantic source.** The `.bbnf` files remain the sole
   source of semantic and structural information within reason. The host
   type/projection system may enrich that information; it may not
   replace it with hand-written JSON-only, CSS-only, or Sheets-only
   parser logic.

4. **Parity-critical work stays in AY.** Anything required to reach
   near parity on the default eager JSON path stays in AY:
   canonical packed layout, direct write, direct-to-struct projection,
   navigation metadata, Pratt/operator lowering where hot, and the
   minimum optimizer integration needed to choose the right emitted
   forms. Anything not required for that target moves to AZ, BA, or BB.

5. **Runtime truth closes the tranche.** AY does not close on mechanism
   count, source grep, or optimistic extrapolation. It closes on the
   default parser and default eager consumer reaching documented
   near-parity gates.

## Invariants

1. **No second runtime path.** No “tape path versus document path”.
   The canonical packed substrate is the runtime output.

2. **No grammar-name dispatch.** Every optimization is admitted by IR,
   recognizer facts, type inference, projection, or grammar-declared
   structure, never by grammar name.

3. **No legacy DTA carry-forward.** No interpreter-shaped dominant hot
   path, no revived `DtaTable` runtime, no PSI stream on the hot path.
   Only salvageable ideas may survive as emitter/runtime mechanisms.

4. **Direct-to-struct is general or it does not ship.** Any direct
   projection must be derived from grammar/type/projection facts.

5. **Pratt flattening is general or it does not ship.** Operator-shape
   lowering is a grammar-derived mechanism, not a Sheets exception.

6. **Structural side information stays on the same path.** Any index,
   skip table, quote map, or other structural side data must accelerate
   the canonical path, not create a parallel one.

7. **Value/view/debug consumers share the substrate.** `view()`,
   `to_value()`, `get()`, debug/readback, and future AZ features must
   all target the canonical packed substrate.

8. **No substrate-without-consumer landings.** Every new field, side
   table, enum variant, or payload lane must ship with a production
   consumer and runtime evidence.

9. **Near parity is the AY floor.** AY must close at near parity if at
   all possible with a grammar-derived single-path design. “Architectural
   parity later” is not an acceptable AY close state.

10. **No orthogonal concern creep.** Compile-time/toolchain work,
    replay/recovery/incremental tooling, and beyond-parity scale-out do
    not enter AY unless they are directly load-bearing for the near-
    parity hot path.

## Current-state assay

AY opened as a broad BEAT-sonic tranche. That was mis-scoped.

What is true now:

- W1 restored the flat AoS write path and removed the largest post-AU
  write-side regression.
- W2 landed useful Named-preservation and wrap-elision work, but not at
  the originally claimed magnitude.
- W3 landed a value API and a visitor path, but the default eager path
  still reconstructs after parse rather than writing the canonical hot
  representation directly.
- W4 landed string/regex/number work, but not the closing architectural
  shift.

What remains is coherent:

- replace the current generic tape contract with the canonical packed
  substrate,
- make the default JSON parser write it directly,
- make `view()`, `to_value()`, and `get()` consume it directly,
- wire the minimum globally informed optimizer decisions required to
  emit the right forms,
- close on near-parity truth.

That is a reasonable scope gestalt for AY. Anything broader mixes
concerns; anything smaller risks closing below the “fully implemented
sonic-rs-class work” bar.

## Wave schedule

AY's remaining execution is four waves.

| Wave | Scope | Agents | Hard gate |
|------|-------|--------|-----------|
| **W5** | Canonical packed substrate contract + JSON direct-write emission | 3 parallel | JSON default parse writes the canonical substrate directly; hot JSON path no longer depends on generic post-parse reconstruction; `finalise` absent from hot JSON samples |
| **W6** | Consumer unification + general direct-to-struct + Pratt/operator lowering | 3 parallel | `view()`, `to_value()`, and `get()` read the same substrate; direct-to-struct fires for admitted named projections; string lazy-path is real; object lookup no longer pays generic child iteration on hot JSON access |
| **W7** | Minimal globally informed optimizer integration | 3 parallel | emission decisions for packed layout, direct-to-struct, structural side information, and hot-path specialization are driven by shared facts/objective; dead duplicated lattices and dead surfaces retired or consumed |
| **W8** | Near-parity close + FINAL + AZ/BA/BB handoff | 2 serial | eager JSON near-parity gates met; AY FINAL authored on truth; AZ/BA/BB handoff updated |

## Hard gates summary

AY closes only if all of the following are true:

1. **One canonical substrate**: default JSON parse writes the canonical
   packed substrate directly; no separate dominant tape-first runtime
   remains.
2. **Consumers unified**: `view()`, `to_value()`, and `get()` all read
   that same substrate.
3. **Direct-to-struct generalized**: grammar-derived named/projection
   layouts reach emit and runtime consumption without grammar-specific
   hand wiring.
4. **Pratt/operator lowering generalized**: operator-shape lowering is
   emitted from grammar facts and reduces hot-path structure cost where
   applicable.
5. **Dead surfaces retired**: any mined or emitted surface without a
   runtime consumer is either consumed or deleted.
6. **Near parity**: eager JSON on the default parser path reaches:
   - `bbnf_value_twitter / sonic_value_twitter <= 1.15`
   - `bbnf_value_canada / sonic_value_canada <= 1.20`
   - `bbnf_value_citm / sonic_value_citm <= 1.20`
   - geomean over the 5 JSON eager fixtures `<= 1.20`
7. **No regressions of principle**: CSS/Sheets/BBNF preserve their
   current functional/parity guarantees; no new grammar DSL directives
   are introduced.

Stretch target, not close gate:

- at least one of the 5 JSON eager fixtures reaches `<= 1.00`.

## Phase detail

### AY.W5 — Canonical packed substrate and direct write

This wave defines and lands the canonical runtime substrate.

It must replace the current hot contract's genericity:

- overloaded pointer/payload lanes,
- post-pass structural closure where write-time closure is possible,
- reconstruction-heavy value materialization,
- compatibility-first cursor burdens.

The canonical packed node must provide:

- explicit tag,
- explicit byte span,
- explicit subtree skip/count/length semantics,
- direct scalar payload storage in final hot form,
- borrowed-or-arena string storage,
- object-as-key/value run layout,
- stable structural metadata sufficient for future debug/incremental
  work without creating a second runtime.

Files:

- `crates/tape/src/tape.rs`
- `crates/tape/src/columns.rs`
- `crates/tape/src/builder.rs`
- `crates/tape/src/cursor.rs`
- `crates/core/src/backend/rust/emitter/shapes/*`
- `crates/core/src/runtime/*`

Hard gate:

- `cargo expand` on the JSON bench shows default `JsonParser::parse()`
  writing the canonical packed substrate directly.
- Hot JSON profiles no longer show the generic finalize/reconstruction
  burden as dominant self-time.
- `value_api_apples_to_apples` is updated to target the new substrate.

### AY.W6 — Consumer unification, direct-to-struct, and operator lowering

This wave makes the substrate real by moving all first-class consumers
onto it.

Scope:

- `view()` reads node headers and navigation metadata directly,
- `to_value()` becomes wrapping/projecting instead of reconstructing,
- `get()` uses object/key navigation metadata instead of generic child
  iteration,
- the currently weak string lazy path becomes real or is removed as a
  first-class promise,
- direct-to-struct uses grammar/type/projection facts to store and
  project admitted structures directly,
- Pratt/operator lowering becomes a general emitted strategy driven by
  grammar facts.

Files:

- `crates/core/src/runtime/parsed.rs`
- `crates/core/src/runtime/path.rs`
- `crates/core/src/backend/rust/view/*`
- `crates/core/src/backend/rust/emitter/grammar.rs`
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
- `crates/core/src/backend/rust/emitter/shapes/pratt.rs`
- `crates/core/tests/value_api_apples_to_apples.rs`
- `crates/core/tests/named_type_preservation.rs`

Hard gate:

- JSON `PathQuery<&str>` is real and emitted.
- Hot JSON object lookup no longer depends on generic child walks.
- `PROJECTION_DIRECT_TO_STRUCT` is populated by grammar-derived
  admission, not manual routing.
- `to_value()` and `view()` both read the same substrate without an
  intermediate rebuild step.

### AY.W7 — Minimal globally informed optimizer integration

AY does not own the entire future optimizer agenda, but it does own the
minimum integration required to choose the right packed forms.

Scope:

- persist reusable fact summaries instead of recomputing parallel
  lattices where possible,
- unify the relevant objective/cost surfaces across grammar extraction,
  regex extraction, strategy/materialization decisions, and backend
  inline analysis,
- make direct-to-struct, Pratt lowering, structural side information,
  and packed-layout emission decisions consume the same fact substrate,
- either retire shape-dict/dead side surfaces or wire them into the
  canonical path,
- decide `pay_f64` and similar specializations only by measured
  consumer benefit.

Files:

- `crates/core/src/pipeline/compile.rs`
- `crates/ir/src/egraph/*`
- `crates/ir/src/passes/materialization/*`
- `crates/ir/src/passes/csp_strategy/*`
- `crates/ir/src/passes/recognizers/*`
- `crates/core/src/backend/rust/analysis/inline.rs`

Hard gate:

- dead duplicate fact derivation is measurably reduced or removed,
- any retained substrate has a production consumer,
- emitted packed-layout decisions and projection/lowering decisions are
  visibly driven by shared facts rather than isolated heuristics,
- bench/profiles show the optimizer integration translates into default
  eager JSON gains.

### AY.W8 — Near-parity close

This wave is close and documentation, not new substrate invention.

Scope:

- full 19-entry parse matrix + 10-entry eager JSON value matrix,
- near-parity verification on the default eager path,
- FINAL.md authored against truth,
- AZ rebased to the AY substrate,
- BA and BB reaffirmed as the immediate successors.

Artifacts:

- `docs/benchmarks/post-AY.json`
- `docs/benchmarks/post-AY-eager.json`
- `docs/tranches/AY/FINAL.md`

Hard gate:

- near-parity gates met as above,
- AY closes without silent “architectural parity later” language.

## Research integration guidance

AY may and should absorb lessons from the prior research corpus, but
only under the generality invariant.

### Keep, recast as general mechanisms

- sonic-rs-style direct per-shape hot-path construction
- simdjson-style structural skip/count/index semantics where they
  accelerate the same runtime path
- direct-to-struct from type inference and projection
- Pratt flattening / operator-shape lowering
- structural mining and recognizer-driven specialization
- only those DTA/PSI-era ideas that survive as profitable, single-path
  runtime mechanisms

### Reject

- any interpreter-shaped dominant hot path
- any second runtime substrate justified by “future tooling”
- any grammar-name specialization that bypasses BBNF/IR/type facts
- any substrate landing without a production consumer and runtime proof

## Cross-tranche debt routing

AY does not own:

- replay/recovery/incremental/debug tooling,
- beyond-parity scale-out and cross-grammar exceedance,
- compile-time/build/bench/test tooling discipline.

Those route explicitly:

- **AZ** — replay, recovery, incremental, parse-debug UX, and the
  non-parity substrate refinements needed for those features.
- **BA** — beyond-parity throughput expansion and cross-grammar scale-
  out after AY closes near parity.
- **BB** — compile-time, build, bench, profiling, and generated-code
  discipline.

## Closing statement

AY is the tranche where bbnf stops treating the generic tape contract
as the canonical runtime.

If AY closes correctly, the system will have:

- one grammar-derived parser,
- one canonical packed substrate,
- one default eager JSON path in the sonic-rs class,
- one shared consumer surface for value/view/debug/incremental futures,
- and a truthful substrate handoff to AZ, BA, and BB.
