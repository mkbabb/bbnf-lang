# AY Planning — Synthesis of Six Fresh Audits

Unified findings from A1–A6 on master HEAD `9074a685`, all agents
working off the Apr 20 01:43 fresh prepare. This document ties the
six reports into a single priority matrix + opens AY.

## 1. Performance landscape (fresh, walker retired)

### 1.1 Parse throughput vs comparators

| Grammar | Comparator | bbnf/comp ratio | Verdict |
|---------|------------|-----------------|---------|
| JSON data | sonic-rs | 5.78× slower | **GAP** |
| JSON twitter | sonic-rs | 7.93× slower | **GAP** |
| JSON citm | sonic-rs | 7.81× slower | **GAP** |
| JSON canada | sonic-rs | 8.24× slower | **GAP** (worst) |
| JSON data_xl | sonic-rs | 5.51× slower | **GAP** |
| CSS L4 normalize | lightningcss | 1.07× slower | close |
| CSS L4 bootstrap | lightningcss | **0.81× faster** | **WIN** |
| CSS L4 tailwind | lightningcss | **0.60× faster** | **WIN** |
| CSS L4 all | cssparser | 2.43-3.10× slower | expected (cssparser is token-only) |
| Sheets/BBNF | — | +2-5% vs W0a.close | stable, no regression |

**Headline**: JSON has a **5.5-8.2× gap to sonic-rs**; CSS beats
lightningcss at scale; Sheets/BBNF are stable. The JSON gap is the
primary performance target.

### 1.2 Universal hot-path union (across 4 grammar families)

| Symbol | JSON | CSS L4 | Sheets | BBNF | Notes |
|--------|-----:|-------:|-------:|-----:|-------|
| `tape::columns::Columns::push_structural` | 36-43% | 23-27% | 28-32% | 28-34% | **universal** |
| `tape::finaliser::finalise` | 23-27% | 14-16% | 12-18% | 14-23% | **universal** |
| `__regex_scan_<Parser>` | ~0% | 26% | 11.8% (stress) | 4-8% | token-heavy grammars |
| `<Parser>::parse` dispatcher shell | 3-5% | 7-12% | 7-8% | 6-12% | universal |
| Per-rule emitted parse_* fns | 18-23% | ~30% | 15-25% | 20-30% | universal |
| `TapeBuilder::push_leaf_with` | 3-4% | 1-3% | 1-3% | 1-2% | mild universal |
| `mi_malloc_aligned` | 1-2% | 1-2% | 1-2% | 1-2% | allocator backbone |

**Key insight**: `push_structural + finalise = 50-70% of self-time on
every grammar and every fixture.** These two symbols dominate the
runtime floor. **No scanner, no dispatch, no allocator is the primary
loss — the tape substrate's write + post-pass IS the loss.**

### 1.3 The JSON gap mechanics

Per A1 §5, JSON is uniquely punished because:
- Tape depth is uniform (object → members → array → elements → scalars), so the push_structural call site fires at every brace, bracket, and comma.
- sonic-rs amortises structural bookkeeping into SIMD vector loads — the tape-write equivalent happens in ~vpcmpeqb intervals.
- bbnf's cross-crate `Columns::push_structural` call boundary is not inlined, so every scalar emit pays a function-call prologue.

Per A2, CSS L4 is NOT punished because:
- Tape depth varies (some rules use Flat shape, skipping structural emits).
- bbnf's scanner is specialized per-grammar; lightningcss's is generic.
- lightningcss materializes CssRule variants (allocation per rule), amortising bbnf's tape cost.

Per A3, Sheets/BBNF are intermediate.

## 2. Architectural intervention priorities

Five levers, ranked by universal impact + invariant-compatibility:

### Priority 1 — Tape-write inline (L-tape-inline)

`tape::columns::Columns::push_structural` is declared `#[inline]` at
`crates/tape/src/columns.rs:321` but emits a real cross-crate call
boundary in every bench binary (per A1 §3, A2 §3, A3 §3). Changing to
`#[inline(always)]` OR monomorphising per-grammar into emit fn bodies
reclaims **~50-70% of dominant self-time** (tape-write + its callers
amortise into per-rule parse fns).

Invariant compatibility: substrate-only change; no grammar DSL
addition, no new emitted surface. **Pure win.**

Files: `crates/tape/src/columns.rs`, possibly
`crates/core/src/backend/rust/emitter/shapes/*.rs` (call sites).

Expected parse delta: 20-40% across all grammars. Specifically
shrinks JSON twitter gap from 7.93× → ~4-5× vs sonic-rs.

### Priority 2 — Finalise fusion (L-finalise-fuse)

`tape::finaliser::finalise` runs a post-parse linear sweep populating
`sib_skip` / `child_off` / `span_hi` backlinks (per A1 §3 — citm
shows this clearly; other fixtures inline the fold loop). This is
recomputable during the emit pass if compound-open/compound-close
back-patches the parent record's `sib_skip` at close time. Removes
the second memory sweep entirely.

Invariant compatibility: substrate change; tape semantics
preserved (finalise output bits must match). **Pure win** if
back-patch is correct.

Files: `crates/tape/src/finaliser.rs` (delete or shrink to empty),
`crates/tape/src/columns.rs` (add back-patch helpers),
`crates/core/src/backend/rust/emitter/shapes/*.rs` (emit close-time
back-patch calls).

Expected parse delta: 10-20% across all grammars. A3's L2 proposes
`[Option<u32>; 32]` stack buffers to replace the `Vec` allocations
that live inside finalise's current impl.

### Priority 3 — Named-type preservation + direct-to-struct (L-named-preserve)

Per A6 + W1r.1 diag: `TypeDesc::Named(sid)` collapses before Rust
emit on every current grammar. The fix lives in 3 IR passes
(candidates per A6: `metadata.rs` alias/transparent stamping,
`egraph/` cost-guided extraction, `span.rs::unwrap_map_node`). Once
Named survives, `emit_direct_to_struct_projection` activates
universally, `.as_color()` fires, and the 40-byte aggregate payload
path engages for all grammar-declared aggregate types.

Per invariant 21: grammar-derived, no hand-coded duplicate.
Per A5 §6: staged with `<Grammar>Value` emission (L-eager lane).

Files: `crates/ir/src/passes/metadata.rs`, `crates/ir/src/egraph/*`,
`crates/ir/src/passes/span.rs`, **new** `crates/core/tests/named_type_preservation.rs`.

Expected outcome: **Direct-to-struct activates for every
`-> input : <Name>` annotation across all grammars.** Not a
direct throughput improvement (aggregate-vs-children may be
throughput-neutral per A6 §8); the deliverable is **correctness of
invariant 20/21** and downstream unlock for A5's `<Grammar>Value`
materialization.

### Priority 4 — Value API eager lane (L-value-eager)

Per A5 §6 + A1 §5: bbnf has no materialized-tree mode, so the
json_monolithic_value bench compares bbnf parse+walk_cursor (11.15×
slower than sonic) against sonic-rs `from_str::<Value>` (eager). To
reach apples-to-apples:

1. Emit `<Grammar>Value` enum from IR TypeDesc (one variant per
   non-transparent rule; grammar-derived per invariant 21).
2. Emit `parsed.to_value::<T>() -> T` method on `Parsed<R>` (walks
   tape, materializes owned tree).
3. Add eager-bench lane: `bbnf_value_twitter` vs `sonic_value_twitter`.

Expected: opens a fair comparator lane; reveals whether post-L1+L2
the residual JSON gap is dispatch, regex, or materialization cost.

Files: **new** `crates/core/src/backend/rust/view/value.rs` (emitter),
`crates/core/src/generate/*` (wiring), `crates/core/benches/json/value.rs`
(lane additions).

### Priority 5 — Regex-scan specialisation (L-regex-specialise)

Per A2 §7: `__regex_scan_CssL4Parser` at 26% self-time on CSS L4
tailwind + 12% on Sheets stress. Specialisation proposals:
- Byte-class dispatch as a primary pre-filter before full regex.
- Property-name PHF dispatch from grammar's known alphabet.
- DFA table hoisting (also A4 L-B's compile-time win).

Invariant compatibility: scanner internals; bbnf-regex crate scope.

Expected parse delta: 8-15% on CSS L4 + Sheets specifically.

Files: `crates/core/src/generate/regex/*`, potentially `crates/bbnf-regex/*`.

### Priority 6 — Compile-time levers (L-compile-*)

Per A4 §"delta vs doc 06": CSS L4 compile **dropped 69% wall + 27% RSS**
since doc 06 because W1r.3a `@pretty` refactors removed a super-linear
rustc codepath. The compile-time urgency **halved**. A4's 5 levers
still apply but with reduced priority:

- **L-A**: `@import`-split CSS L4 emission (20-30% rustc on css_l4).
- **L-B**: Hoist DFA/keyword/byte-class tables to `pub(crate) const` (compile + cache size win).
- **L-C**: De-generic-ify `parse_that` hot API (`parse_that` is 107s of 67s workspace; dominant dep).
- **L-D**: Shared keyword-dispatch PHF across CSS L4 rules.
- **L-E**: `ax-iter` profile tuning.

Invariant compatibility: all local + substrate. No grammar DSL.

Expected compile delta: additional 20-30% off CSS L4 rustc time;
10-20s off workspace cold build.

## 3. Carry-forward debt

From `00-session-recap.md §3`, unchanged:

### 3.1 Five stale W0a/W0b-era tests (invariant 14)

- `bbnf_profile_wire_contract.rs` (8 compile errors)
- `grammar_profile_wire_contract.rs` (15 compile errors)
- `json_parity_shape_emit.rs` (2 compile errors)
- `gate_predicate_wire_contract.rs` (2 compile errors)
- `aw_v_w5_2_per_ref_routing.rs` (2 compile errors)

All reference retired predicates + carved GrammarProfile fields. Per
invariant 14 retire with their predicates. **AY opens with a
`retire-stale-tests` wave** clearing all five in one commit.

### 3.2 ebnf_prettify recognizer bug

`EbnfParser::parse("digit = \"0\" ;")` fails at offset 0. Pre-existing
— unrelated to W1r landings (bbnf_self_parity 56/0 parses same source
successfully via `BbnfEmit::parse`). Divergence is in the
ebnf.bbnf-derived `EbnfParser::parse`. Investigation in AY.

### 3.3 AX close artefacts

- `post-AX-W1-close.json` bench matrix: NOT CAPTURED (invariant 10).
- AX `FINAL.md`: NOT WRITTEN.

AY decides: AX closes via a dedicated AX.FINAL wave, OR AY absorbs AX closure
(treats W0a+W0b+W0c+W1 as AX's complete scope, skips W2-W15, opens AY
directly). **Given W2-W15 are largely unaligned with fresh-audit
priorities, AY should declare AX closed via AX.FINAL wave that captures
the bench matrix + writes FINAL.md referencing AY as continuation.**

## 4. Tranche letter decision

- **AY** is reserved for replay/recovery/incremental per existing
  `docs/tranches/AY/AY.md` (461 lines, fully drafted tooling tranche).
- **AY** is the next available letter for a performance + projection tranche.
- AY follows AX per SPEC ("Successor to AX close").
- AY opens AFTER AY per AX→AY handoff contract ("AX closes first;
  AY's Y0 substrate depends on stable shape emitter").

**Decision: AY is allocated for direct-to-struct + tape hot-path + Value API + compile-time.**

## 5. AY wave schedule (proposed)

Six numbered waves + one close wave:

| Wave | Scope | Agents | Opens after |
|------|-------|--------|-------------|
| **AY.W0** | Retire 5 stale W0a/W0b tests + diagnose ebnf_prettify + AX.FINAL (bench matrix + AX close doc) | 1 serial (cleanup + bench) + 1 parallel (ebnf_prettify diagnosis) | tranche open |
| **AY.W1** | L-tape-inline + L-finalise-fuse (tape substrate hot-path) | 2 parallel (inline + finalise-fuse) | W0 |
| **AY.W2** | L-named-preserve (Named-type preservation + direct-to-struct activation + wire-contract) | 1 serial (preservation fix) → 1 serial (consumer verify) | W1 |
| **AY.W3** | L-value-eager (grammar-emitted `<Grammar>Value` + `to_value` + eager bench lane) | 2 parallel (emitter + bench) | W2 |
| **AY.W4** | L-regex-specialise (byte-class pre-filter + PHF + DFA hoist) | 2 parallel (CSS + Sheets) | W1 (regex is independent of Named) |
| **AY.W5** | L-compile-A/B/D (CSS L4 @import split + DFA hoist + shared PHF) | 3 parallel | W3 |
| **AY.W6** | L-compile-C + L-compile-E (parse_that de-generic + ax-iter config) | 2 parallel | W5 |
| **AY.W7** | FINAL — bench matrix + FINAL.md + AY handoff | 1 serial | W6 |

Hard-gate design per wave uses the WAVE_SPEC.md canonical form:
concrete measurements, artefact paths, no narrative gates, no
substrate-without-consumer.

## 6. Invariants carried forward

AX invariants 1-21 survive unchanged. AY adds:

- **Invariant 22** (tape substrate inline): `push_structural` +
  `finalise` paths are always inlined at emit call sites; no
  cross-crate call-boundary overhead on hot-path record emission. Wave close
  verifies via `nm` absence of the symbol as a linker-level export +
  samply self-time shifting from the tape crate into per-rule parse
  fns.

- **Invariant 23** (Named preservation end-to-end): every grammar-
  declared `-> input : <Name>` annotation (where `<Name>` is
  non-scalar) reaches the tape emitter as `TypeDesc::Named(sid)`.
  Rust emitter's `emit_direct_to_struct_projection` admits the rule
  and emits a runtime shim. Enforced by per-grammar wire-contract
  test at pipeline close.

- **Invariant 24** (Value API apples-to-apples): bench comparisons
  vs external comparators (sonic-rs / lightningcss) are work-matched
  — lazy-to-lazy via `NodeView::get`, eager-to-eager via
  `Parsed::to_value`, text-to-text via `serialize_compact`. Mixed-
  work comparisons forbidden in reported headline numbers.

No grammar DSL additions (invariant 4 preserved).

## 7. Commit checkpoint

This synthesis doc commits as `docs(next-tranche): synthesis of A1–A6
findings (AY.planning)`. Phase 2 continues with AY parent doc +
per-wave specs.
