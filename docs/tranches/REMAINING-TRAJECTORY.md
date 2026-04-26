# Remaining Trajectory — Architecture, Optimisation, and Competitor Plan

Status: planning canon. This file explains the remaining path
`B1 -> B3 -> B4 -> B2 -> AY-II -> AZ-I -> AZ-II -> BA -> BB` after the
2026-04-25 B2 close. It does not authorize execution.

## 1. Path Change From The Redress

The runway picks up three predecessor tranches between B1 and AY-II:

```text
B1 -> B3 -> B4 -> B2 -> AY-II.W0' close + W1-W5 -> AZ-I -> AZ-II -> BA
                                                          \
                                                           -> BB (parallel with AZ-II where disjoint)
```

The redress + post-B1 predecessor sequence changes the floor + the
post-B2 wall expectations, not the destination order beyond AY-II:

1. **B3 closed (parser-baseline restoration).** Five forward fixes
   (γ–η) restored `BbnfBootstrap::parse` to microseconds against
   grammars that previously hung indefinitely; no AY-II.W0' source
   landings reverted. See `docs/tranches/B3/FINAL.md`.
2. **B4.W0 closed (codegen `syn::parse2` emit-correctness).** A
   single-source emitter fix landed against the SIMD bitmap kernel's
   token sequence; the bbnf self-host regen reaches `prettyplease`
   end-to-end without rejection.
3. **B2 closed (build-time codegen transposition).** The `bbnf_derive`
   proc-macro IR-pipeline contract retires; `cargo xtask regen` is the
   canonical regen entrypoint; per-grammar source emerges on disk
   under `crates/core/src/grammar/generated/<ident>.rs`; consumer
   crates `pub use ::bbnf::grammar::generated::<ident>::*` in place of
   `#[derive(Parser)]`; `crates/derive/` deletes outright; the pre-B2
   80-min cold rustc-side IR-pipeline wall ceases to exist; CI +
   pre-commit gate on `cargo xtask regen --check`. See
   `docs/tranches/B2/FINAL.md`.
4. **AZ-I is stricter.** CSS L4 may not retain a tape-backed aggregate
   path. AZ-I's floor is now "JSON, CSS L4, and Sheets are struct-only;
   CSS may have named semantic parity gaps." Tape remains for BBNF only.
5. **AZ-I.W0 amended post-B2.** The derive-cache relocation + Watt
   proc-macro precompilation items DROP (T3-superseded; no proc-macro
   to relocate the cache for, no proc-macro to wrap with Watt). The
   classifier unification + IR audit items KEEP. Sub-agent count drops
   from 3 to 2; hard-gate items from 5 to 4.
6. **AZ-II remains mandatory.** Full `crates/tape/` deletion is still
   binding repo policy. Post-B2 the byte-equal reproducibility cycles
   cost seconds rather than the hours the pre-B2 bootstrap wall would
   have imposed; reversal narrows.
7. **B1 linker posture is normalized.** macOS lld is opt-in through
   `brew install lld`; default is Apple ld64 until a developer opts in.
8. **Preflight was executed and folded into canon.** The 2026-04-24
   four-agent pass showed that several wave plans assumed command
   surfaces or substrates that are not live yet (`cargo-nextest`,
   `StructRegistry`, BB rule storage, typed paths). The path stays
   ambitious, but every remaining implementation wave now opens on an
   executable preflight packet in its owning tranche spec.

## 2. Novel Architecture Thesis

The individual ingredients are not novel by themselves: sonic-rs has
excellent direct struct and pointer ergonomics; simdjson proved the
tape and OnDemand shape; lightningcss proved hand-written typed CSS
parsing; egg and Ruler proved e-graph and enumerative rule discovery;
parse-that carries recursive descent and a bespoke regex HIR.

The novel bbnf architecture is the **union mechanism**:

- one grammar surface;
- one IR substrate;
- one parse path;
- grammar-derived semantic shape through `->` annotations;
- `project_types` + `StructRegistry` deriving native structs;
- CSP cost model selecting parse/emit/scan strategies globally;
- e-graph rewrites over IR and regex HIR, not over ad hoc backend text;
- VM oracle only for proof residue the e-graph cannot discharge;
- backend emitters projecting the same IR facts into Rust / TS / WASM /
  Python surfaces.

The bet is not "out-SIMD sonic-rs by hand". The bet is: **derive the
specialized inner loop that hand-written libraries write by hand, and
apply it to every grammar without a second semantic implementation**.
When a SOTA peer wins a fixture, bbnf treats the delta as a measured
compiler-transposition target: extract the principle, encode it in
grammar/IR/CSP/e-graph form, and re-measure. "They are hand-rolled, so
we stop at imitation" is not an accepted explanation.

## 3. Full-Landing Probabilities

"Full landing" means the tranche closes on declared gates, not merely
on defensible floor. These probabilities are copied from
`docs/RISK-PERF-MATRIX.md` after the 2026-04-24 AZ-I floor redress.

| Tranche | Full landing probability | Defensible-floor probability | Interpretation |
|---|---:|---:|---|
| B1 | closed | closed | Closed 2026-04-24 (`B1/FINAL.md`). |
| B3 | closed | closed | Closed 2026-04-25 (`B3/FINAL.md`); parser-baseline restored. |
| B4 | closed | closed | Closed 2026-04-25 (`B4/FINAL.md`); W0 codegen `syn::parse2` emit-correctness fix + W1 unified `builder.rollback_to(...)` atomic-tape+value path + transitional alias retirement; AY-II.W0' close ceremony folded into B4.W1 close. |
| B2 | closed | closed | Closed 2026-04-25 (`B2/FINAL.md`); proc-macro IR-pipeline contract retired; `cargo xtask regen` canonical. |
| AY-II.W0' | closed | closed | Closed 2026-04-25 at B4.W1 close; the unified atomic-rollback path lands the contract the W0'.a substrate shipped without; transitional aliases retire entirely; 327-failure runtime-parser regression resolves. See `docs/tranches/B4/audit/W1-close.md`. |
| AY-II (W1-W5) | 0.30 | 0.65 | One-path semantic closure on tape substrate; dense integration tranche. Post-B4: W0' close ceremony complete; W1 cursor-shape projection (10 remaining test failures) is the immediate next dispatch. |
| AZ-I | 0.080 | 0.36 | Data-grammar performance + CSS typed richness tranche. Post-B2 lift: W0 derive-cache + Watt items drop (T3-superseded); two sub-agents instead of three; classifier unification + IR audit retain load-bearing scope. |
| AZ-II | 0.20 | 0.50 | BBNF bootstrap byte-equality and tape deletion. Post-B2 lift: byte-equal reproducibility cycles cost seconds rather than hours; reversal narrows. |
| BA | 0.27 | 0.55 | Path query surface over a settled struct tree. |
| BB | 0.10 | 0.32 | E-graph rule inference and automated ranking; useful floor likely. |

The declared-gate joint probability is small because the runway stacks
many ambitious gates, not because the target is too high. The response
is technical discipline: wave-local proof, measured re-plan rails, and
refusal to close on softened architecture.

The post-preflight probabilities below are conditional on the
wave-opening packets in the owning tranche specs landing before the
implementation wave they guard. Until those packets pass, the baseline
table above remains the live forecast.

| Tranche | Post-preflight full landing | Post-preflight floor | Lift mechanism |
|---|---:|---:|---|
| B1 | closed | closed | Closed; lift was: host/tool packet caught pin, nextest, alias, Make, cache, and bench-harness drift before implementation. |
| AY-II | 0.36 | 0.72 | Fresh narrow expand matrix proves no second parse, projection totality, and CSS same-path materialization. Post-B2: compressed-honest W0' close ceremony in ~15 min on a substrate where the 80-min bootstrap wall doesn't exist; wave-cap budget recovered for W1-W5 substrate work. |
| AZ-I | 0.13-0.15 | 0.42-0.46 | Struct-only JSON/Sheets/CSS vertical slices prove no tape-backed projection before broad rollout. Post-B2: W0 amendment drops the derive-cache + Watt sub-agents; load-bearing classifier + IR audit work retains; bisect cycles run in seconds rather than hours. |
| AZ-II | 0.28-0.32 | 0.62-0.68 | Bootstrap-cutover executable preflight and live-symbol tape census prevent false byte-equality closes. Post-B2: byte-equal reproducibility cycles cost seconds; reversal cycles tractable; rapid-iteration on drift-source enumeration. |
| BA | 0.34-0.38 | ~0.65 | BA.W-1 proves no tape, full registry, and typed path/ascent substrate before path APIs open. |
| BB | 0.18-0.25 | 0.45-0.55 | Rule storage/discovery/oracle/fire-extract-writeback-emission chain must exist before enumeration close. |

## 4. Historical Optimisation Ledger

| Tranche / era | Optimisation or architectural change | JSON effect | CSS effect | Status |
|---|---|---|---|---|
| Tranche F | Delim-scan pre-solved per grammar | Constant delimiter classes in emitted scanners | Same for CSS token boundaries | Landed and durable. |
| Tranche H/J/K | Hand-authored e-graph rewrites + scheduler | Regex / IR simplification | Regex / selector simplification substrate | Landed; BB generalizes. |
| Tranche W | Bespoke regex HIR and kernel families | No `regex` crate in emitted hot path | CSS regex token rules become first-class HIR | Landed; BB can rewrite it. |
| AU | Typed payload writes, arena, AU baseline | JSON twitter 1967 MB/s baseline | CSS normalize/bootstrap/tailwind 735/454/496 | Baseline anchor. |
| AV/AW | DTA/PSI/shape-emitter attempts | JSON shape-emitter demo reached sonic-class once, then lost | CSS/Sheets/BBNF remained far below AU | Retired as Era V rut. |
| AX.W0b | DTA interpreter deletion | Removes interpreter tax and dead gates | Same | ~78K LOC removed. |
| AY-I.W1 | Column revert | Twitter recovered to 688 MB/s | CSS recovered from Era V trough but still below AU | Landed. |
| AY-II.W0' | `FusedBuilder` collapse | `parse` + `to_value` become one fused path | Same-path typed CSS projection | Source landings remain on master through B3; the runtime parser regression originally attributed to W0' was traced to a latent contract violation between the tape finaliser's reverse-walk and the Pratt shape's pre-order emission, resolved at B3.W0 via five forward architectural fixes (γ-η) without any W0' revert. |
| B3 | Parser-baseline restoration (γ retire `derive_frame_depth`; δ atomic depth rollback in `Columns`; ε cycle-safe cursor walk; ζ widened `end_compound_post_order` bump scope; η Pratt operand seeding + lowering cousin-leak guard) | json regen end-to-end (compile_paths_request 1.48 ms, generate_all 3.02 ms, prettyplease 11.13 ms); `compile_pipeline::compile_bbnf` median 2.831 ms | Same-path improvement on `compile_css_l4` (26.72 ms median) | Closed 2026-04-25. Bbnf self-host xtask regen surfaces a separate downstream `syn::parse2` codegen-emission defect; out of B3 scope, opens B4. |
| B4.W0 | SIMD bitmap kernel `syn::parse2` emit-correctness fix at the source emitter | bbnf self-host regen reaches `prettyplease` end-to-end without rejection | Same; data grammars unaffected (defect was bbnf-self-host-specific) | Closed 2026-04-25. Single-source emitter correction; B4.W1 owns FusedOutput<R> / FusedBuilder consumer-fixture polish (327 debug-mode failures + 3 timeouts). |
| B2 | Build-time codegen transposition: `bbnf_derive` proc-macro IR-pipeline contract retired; `cargo xtask regen` canonical regen entrypoint; per-grammar source on disk under `crates/core/src/grammar/generated/<ident>.rs`; `crates/derive/` deleted; `BBNF_SCHEMA_VERSION` retired; `cargo xtask regen --check` CI + pre-commit gate | No runtime change; cycle-1 cold regen wall drops from > 80 min to ~12:43 (full sweep, 9 grammars; per-grammar bbnf lib rebuild dominates) or ~5 min (single-grammar cold xtask compile); IR pipeline itself ~73 ms per grammar | No runtime change; same-path CSS L4 emission preserved | Closed 2026-04-25. AY-II.W0' close ceremony tractable in compressed-honest ~15 min on the post-B2 substrate; AZ-I.W0 derive-cache + Watt items dropped (T3-superseded). |

## 5. Remaining Optimisation Ledger

| Tranche | Optimisation / architecture | JSON | CSS L4 | Primary code shape |
|---|---|---|---|---|
| B1 | Pinned toolchain, divan, nextest, script abrogation | No runtime change; cold numbers become trustworthy | No runtime change | `rust-toolchain.toml`, divan benches, nextest CI. |
| B2 | Build-time codegen transposition; proc-macro IR-pipeline retired; `cargo xtask regen` canonical | No runtime change; cold-regen wall ~12:43 (full sweep) vs pre-B2 > 80 min | No runtime change | `xtask/src/regen.rs`, `crates/core/src/grammar/generated/<ident>.rs`, `pub use ::bbnf::grammar::generated::<ident>::*`. |
| AY-II.W0' | Fused parse/value lane | `Parsed::to_value()` projects from value frames; no reparse | Same projection lane for typed CSS | `FusedBuilder<R>`, `FusedOutput<R>`, `project_value_<Grammar>`. |
| AY-II.W1-W5 | Projection totality + same-path structural scan | JSON semantic parity, no consumerless substrate | lightningcss-keyed typed parity on tape substrate | materializer helpers + emitted consumers. |
| AZ-I.W0 | Classifier unification + audit pass (post-B2: derive-cache + Watt items dropped, T3-superseded) | Payload classifier collision resolved before activation | Same for `Length`, `Color`, selector/value payloads | `payload_coverage.rs`, classifier design. |
| AZ-I.W1 | `StructRegistry` closure | Every JSON `Named` rule gets `StructLayout` | Every CSS L4 `Named` rule gets `StructLayout` | `crates/ir/src/registry/struct.rs`. |
| AZ-I.W2 | Scalar direct-to-struct | Numbers, strings, bools, arrays/objects write direct | Out-of-scope except no regression | JSON builder writes fields, not tape records. |
| AZ-I.W3 | CSS aggregate direct-to-struct | Holds JSON parity | `Length`, `Color`, selectors, declarations write direct | CSS typed enums + struct builder. |
| AZ-II.W1-W2 | BBNF self-hosting struct cutover | Holds three-data-grammar struct path | Holds CSS struct path | `bbnf-derive` emits struct-writing parsers. |
| AZ-II.W3 | Physical tape deletion | No live tape symbols/imports | No live tape symbols/imports | `crates/tape/` deleted. |
| BA | Lazy typed path queries | Compile-time checked `path!`, zero allocation | Typed selector/declaration paths | `Path<Grammar, Target>`, parent/ascent strategy. |
| BB | E-graph rule inference + VM residue oracle | Auto-derived JSON rewrites, object-key seek, scalar folds | Auto-derived selector/value rewrites | `crates/ir/src/rewrites/`, `grammar/<name>/rewrites/`. |

## 6. Code Shape Over Time

These are estimated slices of begotten code. They are not promised exact
APIs; they show how the architecture should change.

### JSON at AY-II Close

```rust
pub fn parse_json(src: &str) -> Result<Parsed<JsonParser>, Error> {
    let mut state = JsonState::new(src);
    let mut out = FusedBuilder::<JsonParser>::new(src);
    __json_value(&mut state, &mut out)?;
    Ok(Parsed::from_fused(src, out.finish_fused()))
}

impl<'p> Parsed<'p, JsonParser> {
    pub fn to_value(&'p self) -> JsonValue<'p> {
        project_value_JsonParser(self.value_frames(), self.root())
    }
}
```

Shape: still tape-backed, but value materialisation is fused into the
same parse output. No second parse, no visitor reconstruction.

### JSON at AZ-I.W2

```rust
pub struct JsonDocument<'p> {
    pub root: JsonValue<'p>,
    pub arena: JsonArena<'p>,
}

pub enum JsonValue<'p> {
    Null,
    Bool(bool),
    Number(JsonNumber),
    String(&'p str),
    Array(JsonArrayId),
    Object(JsonObjectId),
}

pub fn parse_json(src: &str) -> Result<JsonDocument<'_>, Error> {
    let mut b = JsonStructBuilder::new(src);
    __json_value_struct(&mut JsonState::new(src), &mut b)?;
    Ok(b.finish())
}
```

Shape: no JSON tape records. Object and array children are struct/arena
indices. String and number payloads land in the final value slot the first
time the scanner sees them.

### JSON at BA

```rust
const TEXT: Path<JsonParser, &'static str> =
    path!(JsonParser, "statuses", 0, "text");

let doc = JsonParser::parse(input)?;
let text: &str = doc.get(TEXT)?;
```

Shape: sonic-rs `pointer!` ergonomics, but grammar-typed. Invalid paths
fail at compile time or macro expansion, not at runtime.

### JSON at BB

```ron
Rule(
  name: "json_object_key_seek",
  lhs: Seq([ObjectOpen, Repeat(KeyValue), ObjectClose]),
  rhs: SeekKey(ObjectLayout, RequestedKey),
  proof: EGraphThenVmResidue,
  class: AutoAccept,
)
```

Shape: key seek and scalar fold rewrites are inferred from grammar/IR
facts, ranked, proven, and then emitted as parse-path improvements.

### CSS at AY-II Close

```rust
pub enum CssL4Value<'p> {
    Length(CssLength),
    Color(CssColor),
    Selector(CssSelector<'p>),
    Unknown(Span), // only where projection totality has not closed
}

pub fn project_value_CssL4Parser(
    frames: ValueFrames<'_>,
    root: NodeId,
) -> CssL4Value<'_> {
    materialize_projection_stylesheet_CssL4Parser(frames, root)
}
```

Shape: typed CSS comes from grammar-derived projection helpers. The close
gate is lightningcss semantic parity, not an internal type-count list.

### CSS at AZ-I.W3

```rust
pub struct CssDeclaration<'p> {
    pub property: CssProperty<'p>,
    pub value: CssTypedValue<'p>,
    pub important: bool,
}

pub enum CssTypedValue<'p> {
    Length(Length),
    Color(Color),
    Selector(Selector<'p>),
    Function(FunctionValue<'p>),
}

fn parse_declaration_struct(
    state: &mut CssState<'_>,
    out: &mut CssStructBuilder<'_>,
) -> Result<(), Error> {
    let slot = out.begin_declaration();
    __property_name(state, out.field(slot, Field::Property))?;
    __typed_value(state, out.field(slot, Field::Value))?;
    out.end_declaration(slot);
    Ok(())
}
```

Shape: every CSS aggregate is a typed struct or enum. A semantic gap may
remain only as a named parity TODO row; no CSS aggregate reads tape.

### CSS at BA/BB

```rust
const FIRST_COLOR: Path<CssL4Parser, Color> =
    path!(CssL4Parser, "rules", 0, "declarations", 0, "value.color");

let color: Color = sheet.get(FIRST_COLOR)?;
```

```ron
Rule(
  name: "selector_descendant_flatten",
  lhs: Selector(Descendant(Descendant(A, B), C)),
  rhs: Selector(Descendant(A, Descendant(B, C))),
  proof: EGraph,
  class: Review,
)
```

Shape: path accessors navigate typed CSS structure, and inferred rewrite
rules simplify selector/value IR before codegen.

## 7. Throughput Trajectory and Competitor Delta

All future numbers are targets or estimates until B1 re-measures under
divan. Competitor deltas must be refreshed under the same harness; the
known sonic figure below comes from AY-I Audit D's value-path comparison.

### JSON Value / Full Parse

| Juncture | bbnf twitter MB/s | sonic-rs value MB/s | Delta | Notes |
|---|---:|---:|---:|---|
| AU baseline | 1967 | not same harness | N/A | Tape-only parse baseline. |
| AY-I value path | 538 | 2151 | 0.25x | Honest current semantic value gap. |
| AY-I parse path | 688 | not same harness | N/A | Column-revert plateau. |
| AY-II target | 1200-1500 | refresh in B1 | estimated 0.55-0.70x | One-path value lane closes correctness first. |
| AZ-I.W2 floor | 1967 | 2151 old | 0.91x | AU recovery on struct-only JSON. |
| AZ-I.W2 target | 2200 | 2151 old | 1.02x | First credible sonic-class full value target. |
| BA lazy path | >= 2400 on 3-field twitter | sonic pointer refresh | target +20% | Laziness should beat full materialisation. |
| BB post-rewrites | parity or +5-15% over AZ-I | refresh | target beat | Object-key seek and scalar folds should be visible. |

### CSS L4

| Juncture | normalize | bootstrap | tailwind | Competitor delta posture |
|---|---:|---:|---:|---|
| AU baseline | 735 | 454 | 496 | lightningcss reference range cited as 450-900 MB/s. |
| AY-I current cited | ~300 | ~200 | ~210 | Recovery target remains AU plus lightningcss-class typed parity. |
| AY-II target | 500-600 | 350-400 | 380-440 | Typed parity on tape substrate, not final speed. |
| AZ-I.W3 floor | 735 | 600 | 496 | Struct-only CSS L4, no tape bridge. |
| AZ-I.W3 target | 850 | 700 | 600 | Within or above cited lightningcss range on key fixtures. |
| BA path queries | parity full parse | parity full parse | parity full parse | Lazy declaration/selector access should beat full parse for queries. |
| BB post-rewrites | +5-10% over AZ-I | +5-10% | +5-10% | Selector/value rewrites and classifier simplification. |

## 8. Can This Beat sonic-rs, simdjson, and lightningcss?

Yes, under the hard version of the plan:

1. **Full JSON parse/value** can beat sonic-rs on at least some fixtures if
   AZ-I direct-to-struct removes tape projection and BB recovers key seek /
   scalar-fold rewrites. The old AY-I value path is not the target path;
   it is the measured delta that AZ-I/BB are designed to erase.
2. **Lazy JSON queries** should beat sonic-rs and simdjson-style OnDemand
   for grammar-known paths, because BA has compile-time path typing plus a
   struct tree that can skip irrelevant branches without runtime path
   decoding.
3. **CSS typed semantics** can beat lightningcss by reaching
   lightningcss-equivalent typed richness without hand-coded CSS adapters,
   then applying grammar-derived struct emission and rule/cost
   improvements. That is AZ-I.W3's reason to exist. A string-preserving
   parser that skips typed semantics would be faster and wrong for this
   project.
4. **Cross-grammar leverage** is where bbnf should exceed hand-written
   peers architecturally: a rule inferred for regex HIR, scanner choice,
   payload placement, or path traversal can apply to JSON, CSS, Sheets, and
   BBNF without writing four bespoke parsers.

The non-negotiable stance: if a SOTA peer wins, profile the win, extract the
principle, represent it in grammar/IR/CSP/e-graph form, and re-measure. Treat
hand-written SIMD wins as missing compiler transpositions until proven
otherwise.
