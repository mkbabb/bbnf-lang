# AZ Planning — Prior-Tranche Archaeology

Archaeology-only audit of direct-to-struct projection, Value API, and
external-comparator parity lineage bearing on the next tranche (letter
AZ). Read-only; every claim cites a commit hash.

## 1. Chronic-debt ledger

Items re-proposed across ≥3 predecessors. Citations are the adding,
retiring, and resurrecting commits.

| # | Item | Chain | Disposition |
|---|------|-------|-------------|
| 1 | Named-aggregate struct projection | AS.2.3 scaffold (`c7511936`) → AU.4.2 delete (`ab8588a3`) → AW.0.5 `NamedTypeResolver` (`492ab8e5` + `70886a0b`) → AW-III.W6.4 universal `BINDINGS` (`d1fef50a` + `63bf36bb`) → AW-IV.W3.5a emit-wire (`56d66234`) → AX.W1r.1 dead-code delete (`5d5096eb` / `fcb64d5f`) | **Drifting.** 5 tranches of substrate, 0 runtime consumers; `audit/W1r1-diag.md` — zero `TypeDesc::Named(_)` in `ir.types` on any grammar. |
| 2 | `StructRegistry` central table | AS.2.3 add (`c7511936` / `8c52218d`) → AU.4.2 delete (`ab8588a3`) | **Retired.** Delete cites "two tranches on the books, zero population, zero effective consumption — per-backend tables replace". |
| 3 | sonic-rs / lightningcss parity harnesses | Plan-only AP-AT (`lever-efficacy.md:73`) → AW-III route (`FINAL-III.md:528`) → AW-IV.W5.2 land (`86424b39`, `73828e16`, `95b819f0`) → AW-V.W6 verify (5/5 + 4/4) → AX.W1r.2/3/3a canonical-form (`a6429d3e`, `42f20192`, `293be673`) | **Landed + delivered**, caveat: byte-parity retreated to scale+interop on lightningcss bootstrap/tailwind per `audit/W1r3a-diag.md`. |
| 4 | Pratt precedence-tower flattening | AU.6.3 not-shipped → AV.3 table-only → AW-III.W6.5 LUT + consumer (`2f667a82`, `9cadda76`, `b931eafc`) → AW-IV.W3.4 walker inline → AX.W0a.2.k/.l per-rule LUT (`f585ce37` reverted → `64d6ab2f`) | **Landed + delivered.** |
| 5 | Value API (`bbnf::<grammar>::Value`) | AW-V W2.1 JSON prototype (`4fdef7c3`, `0dcf9743`) → AX.W1.A json::Value (`680c7bd8`, `66c7a827`, `7a060b37`, `fafba89d`) → AX.W1.B css::StyleSheet + 22 TypeOnly (`7fbb7fd0`/`405917bf`, `82bda9aa`/`faa7682d`) → AX.W1r.0 revert (`3429aaba`, −6128 LOC) | **Retired.** Revert cites invariants 4/11/18. Grammar-derived `NodeView` + canonical-serialization parity replaces; invariant 21 added. |
| 6 | Lever-4 `push_compound_fused_v32` consumer | AW-V.W1.3 substrate (`1cf69a69`) → AW-V.W6 "no consumer engages" (`FINAL-V.md:204`) → carried to AX | **Drifting.** Substrate-only 2 tranches. |
| 7 | `dispatch_one` interpreter removal | AW-III.W4.d inline to 0% self-time (`9a87dc61…9ce5f28e`) → AX.W0b delete (`0adabb23`) | **Landed + delivered.** |

Three resolution patterns: (a) **formal retirement with named replacement** (items 2, 5: delete commit names the successor architecture); (b) **substrate-first-consumer-never** (items 1, 6: hard gate closes on "code exists", runtime never fires — `audit/SYNTHESIS.md` §"Recurring anti-patterns" #1); (c) **partial-byte-parity retreat** (item 3: narrows to canonical-form equality or scale+interop when comparator prints lossily).

## 2. Direct-to-struct archaeology

Six attempts on `-> input : <Name>`:

- **AS.2.3** — `GrammarIR::struct_registry: HashMap<StringId, Vec<TypeDesc>>` (`c7511936` / `8c52218d`). Phase-4.5 placeholder; never populated.
- **AT.6.1** — Named-struct codegen carry; grep gate fired while `branch_pushes_children` (in `driver/alt.rs`, absent from AT critical-files) misclassified Ref leaves. Every typed capture a dead store until AU.1.1 (`83357e4`) per `AT-retro.md:22–25`.
- **AU.4.2 delete** (`ab8588a3`): "two tranches on the books, zero population, zero effective consumption — per-backend type tables replace". −83/+5 LOC.
- **AW.0.5 per-backend re-open**: `492ab8e5` (IR `NamedTypeResolver` trait + `LARGE_PAYLOAD_MAX=64`); `70886a0b` (RustNamedTypes + Color view + `.as_color()` shim hardcoding `matches!(name, "Color"|"ColorMix")`).
- **AW-III.W6.4 universalisation** (`d1fef50a` + `63bf36bb`): static `BINDINGS` + 4 parity test files; FINAL-III concedes "consumer wiring at `emit_view_impl` remains on the AW-IV side" (`FINAL-III.md:408–410`).
- **AW-IV.W3.5a emit-wire** (`56d66234`): threads resolver into `emit_type_definitions_impl`; emits `PROJECTION_DIRECT_TO_STRUCT` const. Introspection surface, not runtime activation.
- **AX.W1r.1 delete** (`5d5096eb` / `fcb64d5f`): static table + free fn deleted; IR-walker `RustNamedTypes::from_ir` replaces. Diag `audit/W1r1-diag.md`: zero `Named(sid)` in `ir.types` on any of 6 grammars. VM path preserves `Named(String)` for JSON; Rust path collapses.

**Collapse location.** W1r.1 §Finding: Rust `prepare_grammar → analyze_grammar → project_types` rewrites `Named(Color)` → `Tuple([Span, U8, BoxedEnum×3, Option(BoxedEnum)])`; `colorFunction` / `colorMix` eliminated from `ir.rules`; `colorFn` survives as structural tuple. VM runs fewer rewrites, preserving `Named(12)` for JSON `string` (activates `universal_named_shape` at `crates/ir/src/passes/payload/layout.rs:436`). **No commit introduces the collapse**; it is emergent from the union of type-inference + projection passes, never a deliberate design.

## 3. Value API archaeology

Three arcs.

- **AW-V.W2.1 json-prototype** (`4fdef7c3` + `0dcf9743`): hand-tuned parser writing sonic-layout `Value`/`Document` directly. Beats sonic-rs 0.89–0.94× on all 5 entries (`FINAL-V.md:31–39`); `nm` verified zero `dispatch_one`/`DtaState`/`FrameStack`. Emitter visitor-path matched prototype ±2% at W3 close (`c1e86ab3`) then regressed W6. Crate renamed `json-prototype` at AX.W0b.C (`b464a99c`/`1327491e`/`6ad76124`).

- **AX.W1.A/B hand-coded duplicates (reverted)**: `680c7bd8` + `66c7a827` + `7a060b37` + `fafba89d` (json::Value + Number); `7fbb7fd0`/`405917bf` + `82bda9aa`/`faa7682d` (css::StyleSheet, 22 TypeOnly). Reverted `3429aaba` citing invariants 4/11/18. −3500 LOC CSS, −450 LOC JSON, sonic-rs → dev-dep.

- **AX.W1r.0–7 grammar-derived NodeView**: W1r.1 IR-derived resolver (dead code, §2); W1r.2 JSON canonical-form vs sonic-rs (`a6429d3e`); W1r.3/3a CSS canonical-form vs lightningcss, scale+interop fallback (`42f20192`, `d11874db`, `b930cf2c`, `293be673`); W1r.4/4a `@pretty sep(X)` codegen (`f6a264e2`, `28fd46fc`, `53d99e4a`); W1r.5 BBNF self-parity 56/0 (`53318493`); W1r.6 typed-accessor audit 14/0 (`81627d7c`); W1r.7 twitter lazy-field via NodeView (`ab7c218d`; AoS 4.14× SoA ax-iter, 1.67× release).

**Worked.** Canonical-form byte equality (both parser sides, no bridge types); typed-accessor audit 295 rules × 7 classes; NodeView bench over `TapeCursor`. **Didn't.** Per-grammar enum duplication (unbounded divergence surface); byte-parity on prettified output against lossy comparator printers.

## 4. Performance-comparator archaeology

Trajectory (MB/s, `audit/last10-slowdown-census.md`): post-AU high-water twitter 1967 / bootstrap 454 / sheets_stress 121 / bbnf_self 394 → AW-I/II universal −91% (parse failures masked as bootstrap "win") → AW-III dispatch_one → 0% self-time but geomean 0.08× (`FINAL-III.md:57`) → AW-IV.W5.2 lands sonic-rs (`86424b39`) + lightningcss (`73828e16`) harnesses CI-gated (`95b819f0`), W4.4 parallel fork tailwind +131% (9→37) → AW-V.W2.1 prototype beats sonic 0.89–0.94× but W6 `has_w4_classified` breaks JSON visitor bench → AX.W0a 77/77 parity at `6b03dd53`; 18/18 bench baseline at `1241e7ac`.

**Lever classification** per `audit/SYNTHESIS.md` §"Recurring anti-patterns":

- **Gate-off (#5)**: AP.1 `structural_mode=false` (AQ.5 delete).
- **Substrate-without-activation (#1)**: AQ.6 typed-payload; AS.2.3 StructRegistry; AV.0.5 Color admission; AW-III.W6 PHF/ClassifyByte/SHAPE_DICT/CTNS/Bounded-Regex; AW-IV.W3 ShapeRef/Pratt-LUT/CTNS; AW-V W4/W5 per-shape emitters for CSS/Sheets/BBNF.
- **Hard-gate-via-grep (#6)**: AT Phase-1 `branch_pushes_children`.
- **Bench-omission (#9)**: AV V0–V5 2.5–4.5× regression invisible until V10 first bench.
- **Predicate-widening-breaks-working-bench** (new; carried as AX invariant 9): AW-V.W4-fix-rest `has_w4_classified` widen.
- **Invariant-violation**: AX.W1.A/B hand-coded Value duplicates (4/11/18).

Materially-moved levers (≥10%) across the arc: AP.3 SIMD WS bitmap + string scanner; AP.4 CSS key dispatch `__declaration`; AS.1 CSS `scan_ident` sub-flag; AU.1.1 `branch_pushes_children` (`83357e4`); AW-IV.W4.4 parallel fork; AW-V.W2.1 hand-prototype (`4fdef7c3`). Of the six, only W2.1 is direct-to-struct / Value-API work.

## 5. Chronic samply hotspots

Per `AU/profiling-2.md`, `audit/last10-slowdown-census.md`: `JsonParser::__value` 79–83% JSON self-time (AU→AW-V; AW-V.W3 visitor matched prototype, regressed W6); `CssL4Parser::__compoundSelector` 33–43% (AU.2.7 structural bitmap missed its perf gate; classifier still open); `CssL4Parser::__declaration` 11–15% (AP.4 key dispatch; PHF + SIMD pending); BBNF `__mapped_factor` 15–40% (nested `push_compound(Repeat)` even when `->` absent); `compute_f64` Eisel-Lemire dominates canada (AW-V.W3 16-digit NEON inlined); `__dta_walker_inline::run` 33–37% across grammars AW-III→V (AX.W0b deleted); `__regex_scan_<grammar>` 37% residual (parse-that bespoke HIR; persists); CSS whitespace + block-comment 11–13% (partial via structural bitmap).

## 6. Guardrails for the next tranche

1. **No hand-coded `bbnf::<grammar>::Value` duplicates.** W1.A/B (`680c7bd8`, `7fbb7fd0`) reverted at `3429aaba` after ~6128 LOC. Any Value API re-surfaces grammar-derived (invariant 21); parity proves via canonical-serialization byte equality, not `From<third-party>` / `PartialEq<third-party>` bridges.

2. **No `TypeOnly` placeholder variants.** W1.B's 22 stubs violated invariant 18. Every emitted variant ships field-complete on day one of its wave.

3. **Do not gate off working benches to admit new classifications.** AW-V.W4-fix-rest widened Flat/Wrap, tripping `has_w4_classified` at `emitter/grammar.rs:718`, breaking the prototype-matching `json_monolithic_value` bench (`FINAL-V.md:162`). Classification widenings carry a per-grammar wire-contract test on the gate's output (invariant 14) and re-run the full bench matrix at the widening commit (invariant 16).

4. **Substrate-without-consumer is not a close.** The Named-projection chain (AS.2.3 → AU.4.2 → AW.0.5 → AW-III.W6.4 → AW-IV.W3.5a) never fired at runtime; W1r.1 confirms `resolve_named_type` returns `None` on every grammar. If AZ introduces a Named-preservation pass, pair it with a runtime consumer test asserting the call fires (samply / `nm` / wire-contract end-to-end).

5. **`grep` + `cargo expand` are supplementary, not load-bearing.** AT.1 passed its grep gate while every typed capture was a dead store (`AT-retro.md:22–25`); AW-IV.W3.5a emitted `PROJECTION_DIRECT_TO_STRUCT` with zero runtime fires. Gates cite samply self-time, `nm` symbol presence/absence, or end-to-end wire-contract tests.

6. **Attack the Named collapse at the upstream pass.** W1r.1 diag names it: Rust backend's `prepare_grammar → analyze_grammar → project_types`. The collapse has no introducing commit — it is emergent from type-inference + projection unioning. AZ must make the preservation deliberate (e.g. a dedicated pass that keeps `Named(sid)` through `project_types`), then ship a consumer that reads it.

7. **Byte-parity vs canonical-form vs scale+interop is a plan-time decision.** W1r.3 → W1r.3a retreat (`audit/W1r3a-diag.md`) burned a sub-wave because lightningcss `calc()` arithmetic + shorthand reordering defeats symmetric byte-parity. If AZ scopes cssparser / simdjson / simd-json, declare per comparator × corpus which parity tier applies — small corpora often admit byte, large usually do not.
