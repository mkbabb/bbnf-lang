# BA.W5c — BBNF Direct-To-Struct Migration

**Thesis** (the BBNF `OpenFrame` substrate retires; per-rule generated `parse_<rule>(...)` functions return typed `BbnfValue<'p>` directly; Pratt operator-chain emission for grammar's own operators lands; the self-host parse-and-format roundtrip passes; the BBNF aggregator disposition per `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md` is closed; Lock 1 honour continues from W5a/W5b). **Closer-gate** (post-W5c `rg -n 'enum OpenFrame' crates/core/src/runtime/bbnf/` returns 0; self-host roundtrip at `tests/bbnf_self_host_roundtrip.rs` passes; `cargo nextest run -p bbnf -p bbnf-bootstrap -E 'test(bbnf) + test(self_host)' --profile ax-iter` 100% pass; BBNF aggregator namespaced).

## §1 — Deliverable

Hereupon BBNF's `OpenFrame` taxonomy at `crates/core/src/runtime/bbnf/builder.rs:1-243` (per `audit/MODULES-2026-05-03.md:978`) retires. BBNF's substrate is the smallest of the four major-grammar builders (243 LOC vs JSON's 382, CSS L4's 1,014, Sheets' 357) but its consumer is the most stress-test-heavy: BBNF self-hosts the language. The bootstrap binary at `crates/bbnf-bootstrap/src/bin/dump_ir.rs` per `audit/MODULES-2026-05-03.md:221` consumes the BBNF parser to lift grammar source into IR; if W5c regresses the parser, every downstream grammar regen breaks.

The BBNF grammar at `grammar/bbnf/bbnf.bbnf` exercises every direct-projection construct from `docs/tranches/BA/audit/W5-generated-parser-shape.md` §1.1-§1.10:

- **Alt (byte-disjoint)** — top-level rule alternation; emits direct `match first {...}`.
- **Alt (speculative)** — `mapped_factor` overlap; O(1) tuple Checkpoint.
- **Seq** — every typed construction.
- **Repeat** — rule-list, alt-list, sequence-list with `SmallVec<[_; 8]>` per `audit/RESTART-SKETCH-2026-05-03.md:215`.
- **Optional** — every `?`-suffixed construct.
- **CharClass** — identifier class, whitespace.
- **Keyword** — grammar reserved words (`@import`, `@host`, etc.).
- **Scanner** — bespoke regex DFA for grammar literals.
- **Pratt operator chain** — BBNF's grammar carries operator chains (`|`, `,`, `&`, `=`); per W5-generated-parser-shape §1.x (Pratt) the operator-token lookup table emits with recursive descent for operands. Per `feedback_grammar_closures` and `project_grammar_closures` BBNF's operators carry first-class closures and beta-reduction at compile time.
- **MapExpr** — typed-enum constructors per rule.
- **HostCall** — BBNF has host fns for grammar-introspection (e.g. `@debug` markers per `project_debug_infra`).

The W5c emit shape for BBNF's `rule` rule (representative):

```rust
fn parse_rule<'p>(
    input: &'p [u8], p: &mut usize, arena: &mut BbnfArena<'p>,
    state: &mut ScanState, cursor: &mut PathCursor<'_>,
) -> Result<BbnfRule<'p>, ParseErr> {
    let name = parse_identifier(input, p)?;          // &'p str
    skip_space(input, p, state);
    expect_byte(input, p, b'=')?;
    skip_space(input, p, state);
    let body = parse_alt(input, p, arena, state, cursor)?;  // BbnfAlt<'p>
    skip_space(input, p, state);
    expect_byte(input, p, b';')?;
    Ok(BbnfRule { name, body })
}
```

Each BBNF construct compiles to direct `BbnfValue<'p>` constructor; the typed-enum is `Rule(BbnfRule<'p>) | Alt(BbnfAlt<'p>) | Seq(BbnfSeq<'p>) | Repeat(BbnfRepeat<'p>) | Optional(BbnfOptional<'p>) | Identifier(&'p str) | Literal(&'p str) | ...` per `feedback_preserve_rich_ast`.

Per surgery #19 the BBNF aggregator `pub use bbnf::*` at `crates/core/src/grammar/generated/mod.rs:14-23` (per `audit/CENSUS-2026-05-03.md:199`) deletes within W5c (or earlier, at W2.M4 per surgery #19 timing). W5c verifies: BBNF accesses through the namespaced path `crate::grammar::generated::bbnf::*` (parallel to all other grammars per `feedback_no_workarounds` "no asymmetry"). The `bbnf-bootstrap` crate at `crates/bbnf-bootstrap/src/lib.rs` (28 LOC per `audit/MODULES-2026-05-03.md:220`) updates its re-export to the namespaced path.

The performance trajectory: BBNF self-host parse of the BBNF grammar's own source is regression-bound (no SOTA target); BA-G1c records the post-W5c measurement against pre-W5c baseline. Per `audit/SOTA-2026-05-03.md` BBNF has no external SOTA competitor (no other grammar engine self-hosts a grammar language at this scale); the BB tranche's BB-G2 records BBNF self-host regression-bound.

The Era V failure mode is closed for BBNF at W5c; the same-wave consumer is the self-host roundtrip + `bbnf-bootstrap` regen.

## §2 — Milestones

> **M0 — BbnfValue typed-enum lands; BBNF aggregator namespaced**
>
> *Surface*: `crates/core/src/runtime/bbnf/value.rs` (per `audit/MODULES-2026-05-03.md:976`); `crates/core/src/grammar/generated/mod.rs:14-23` (the aggregator per `audit/CENSUS-2026-05-03.md:199`); `crates/bbnf-bootstrap/src/lib.rs` (28 LOC per MODULES:220).
> *Action*: Per surgery #19 + `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md`, delete `pub use bbnf::*` aggregator; rewrite `bbnf-bootstrap`'s re-export to `pub use bbnf::grammar::generated::bbnf::BbnfBootstrap`; update every consumer of the aggregator path to the namespaced path. Land typed-enum `BbnfValue<'p>` per `feedback_preserve_rich_ast`.
> *Gate*: aggregator deleted; namespaced access only; `BbnfValue<'p>` enum lands.
> *Exit-criteria*: `rg -n '^pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'BbnfValue::' crates/core/src/runtime/bbnf/value.rs | wc -l | tr -d '\n'` returns ≥ 8 (8+ variants for typed AST).

> **M1 — Per-rule `parse_<rule>` direct-projection emit for BBNF**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct/{header,body,fields,finalize}.rs`; the BBNF grammar at `grammar/bbnf/bbnf.bbnf`; the regen target `crates/core/src/grammar/generated/bbnf.rs` (21,503 LOC pre-W5c per MODULES:619).
> *Action*: Implement direct-projection emit. Per W5-generated-parser-shape §1.1-§1.10: byte-disjoint Alt → direct `match`; speculative Alt → O(1) tuple Checkpoint; Pratt operator chain → operator-token lookup table + recursive descent for operands; HostCall → `crate::grammar::host::bbnf::*` namespace.
> *Gate*: post-regen `crates/core/src/grammar/generated/bbnf.rs` contains `pub fn parse_rule<'p>(...) -> Result<BbnfRule<'p>, ParseErr>` etc.; no `OpenFrame` references.
> *Exit-criteria*: `cargo xtask regen --grammar bbnf && rg -n 'OpenFrame' crates/core/src/grammar/generated/bbnf.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'pub fn parse_(rule|alt|seq|repeat|optional)' crates/core/src/grammar/generated/bbnf.rs | wc -l | tr -d '\n'` returns ≥ 5.

> **M2 — Pratt operator-chain emission for BBNF operators**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/pratt/` (post-W2.M2 split if present; otherwise added at W5c.M2); the BBNF operator-table at codegen.
> *Action*: Implement Pratt operator-chain emission per W5-generated-parser-shape §1.x. The operator-token lookup table compiles at codegen; the recursive descent for operands matches BBNF's operator precedence: `|` < `,` < `&` < `=`. Per `feedback_grammar_closures` BBNF's operators carry first-class closures; the codegen beta-reduces at compile time.
> *Gate*: post-regen `bbnf.rs` contains a Pratt-style operator-token dispatch; the `cargo expand`-equivalent inspection shows the operator lookup table.
> *Exit-criteria*: `rg -n 'OPERATOR_PRECEDENCE\|fn parse_pratt' crates/core/src/grammar/generated/bbnf.rs | wc -l | tr -d '\n'` returns ≥ 1.

> **M3 — `OpenFrame` deletes from BBNF path**
>
> *Surface*: `crates/core/src/runtime/bbnf/builder.rs` (243 LOC pre-W5c per MODULES:978).
> *Action*: Delete `enum OpenFrame`; reduce `BbnfStructBuilder` to arena-management surface; per the BA.md §Generated-LOC budget table the post-W5c `bbnf.rs` ≤ 19,000 LOC (down from 21,503; ~12% shrinkage from speculative-checkpoint retiral and `__layout` literal retiral). The `runtime/bbnf/arena.rs:220` "legacy emission paths or non-BBNF" arm per `audit/CENSUS-2026-05-03.md:201` deletes (the legacy disjunct is dead post-direct-projection); `runtime/bbnf/view.rs:28-33` "tape-direct `child(i)` accessor" + "discriminator (replaces the tape-era `variant_idx`)" deletes per `audit/CENSUS-2026-05-03.md:82`.
> *Gate*: `enum OpenFrame` is gone from `runtime/bbnf/`.
> *Exit-criteria*: `rg -n 'enum OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/bbnf/ 2>&1 | wc -l | tr -d '\n'` returns `0`.

> **M4 — Self-host roundtrip passes**
>
> *Surface*: `crates/core/tests/bbnf_self_host_roundtrip.rs` (or equivalent per the test fixture); `crates/bbnf-bootstrap/src/bin/dump_ir.rs`.
> *Action*: Run `cargo nextest run -p bbnf -p bbnf-bootstrap -E 'test(bbnf) + test(self_host)' --profile ax-iter`; verify 100% pass. The roundtrip: parse BBNF source → IR → emit Rust → re-parse Rust output → verify byte-equal AST.
> *Gate*: 100% pass on BBNF self-host tests.
> *Exit-criteria*: `cargo nextest run -p bbnf -p bbnf-bootstrap -E 'test(bbnf) + test(self_host)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M5 — BA-G1c: BBNF self-host parse-and-format roundtrip regression-bound**
>
> *Surface*: bench harness for BBNF self-host; fixture `grammar/bbnf/bbnf.bbnf` (the grammar parses itself).
> *Action*: Run `cargo bench --bench bench_bbnf -- self_host` post-regen; record measurement. BA-G1c is regression-bound (no SOTA target — no external competitor self-hosts a grammar language); post-W5c measurement should be ≤ pre-W5c baseline × 1.05.
> *Gate*: post-W5c bench output recorded; no regression vs pre-W5c.
> *Exit-criteria*: `bbnf_self_host.mean ≤ pre-W5c × 1.05`.

> **M6 — BBNF generated-LOC budget verification**
>
> *Surface*: pre-W5c `crates/core/src/grammar/generated/bbnf.rs` (21,503 LOC); post-W5c same file.
> *Action*: Per the BA.md §Generated-LOC budget table the post-W5c target is ≤ 19,000 LOC. Verify `wc -l crates/core/src/grammar/generated/bbnf.rs` returns ≤ 19,000.
> *Gate*: `bbnf.rs` post-W5c LOC ≤ 19,000.
> *Exit-criteria*: `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/bbnf.rs | awk '{print ($1 <= 19000)}' | tr -d '\n'` returns `1`.

> **M7 — W5c artefact emission**
>
> *Surface*: `docs/tranches/BA/audit/W5c-bbnf-direct-to-struct.md` (new artefact).
> *Action*: Emit a per-grammar disposition artefact recording (a) BBNF pre-/post-W5c generated-LOC, (b) sample post-regen `parse_rule` body, (c) Pratt operator-chain emission shape, (d) self-host roundtrip pass-rate, (e) aggregator-disposition closure (`pub use bbnf::*` deleted; namespaced access only).
> *Gate*: artefact exists; records the five evidence items.
> *Exit-criteria*: `test -f /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5c-bbnf-direct-to-struct.md && wc -l /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5c-bbnf-direct-to-struct.md | awk '{print ($1 > 80)}' | tr -d '\n'` returns `1`.

## §3 — Closer gate

```
# BBNF OpenFrame deleted
rg -n 'enum OpenFrame' crates/core/src/runtime/bbnf/     ; expect: 0 matches

# bbnf.rs LOC reduction
wc -l crates/core/src/grammar/generated/bbnf.rs          ; expect: ≤ 19,000

# BBNF aggregator deleted
rg -n '^pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs
                                                         ; expect: 0 matches

# Self-host roundtrip 100% pass
cargo nextest run -p bbnf -p bbnf-bootstrap -E 'test(bbnf) + test(self_host)' --profile ax-iter
                                                         ; expect: 100% pass

# BA-G1c BBNF self-host regression-bound
cargo bench --bench bench_bbnf -- self_host              ; expect: ≤ pre-W5c × 1.05

# Pratt operator-chain emit landed
rg -n 'OPERATOR_PRECEDENCE\|fn parse_pratt' crates/core/src/grammar/generated/bbnf.rs
                                                         ; expect: ≥ 1
```

## §4 — Invariants

§I1. **Lock 1 partial honour** (BBNF-side). Per the canonical W5a invariant, W5c lands the discipline for BBNF. Sheets / cohort follow at W5d / W5e. Lock 1's BA close honour completes at W5e.

§I2. **Lock 3** (one parse impl). The W4-introduced `parse_with(input, &path)` is the single parse implementation; W5c's direct-projection rewrites the body but preserves the unified surface.

§I3. **Lock 9** (slice-borrow primary). BBNF's `BbnfValue<'p>` is slice-borrowed; per `audit/SOTA-2026-05-03.md:122-123` `&'i str` over input.

§I4. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The dual-strategy resolver (DirectToStruct for JSON+CSS L4+BBNF post-W5c; OpenFrame for Sheets+cohort transient) collapses progressively at W5d / W5e.

§I5. **No combinators monolithic** (per `feedback_no_combinators_monolithic`). BBNF's emit extends alt-classifier (M1) + adds Pratt operator-chain emission (M2) — no combinator/SpanParser usage.

§I6. **Preserve rich AST** (per `feedback_preserve_rich_ast`). The `BbnfValue<'p>` typed-enum preserves shape: 8+ variants for grammar AST. The typed-enum reaches every grammar rule per `feedback_typed_materialization_invariant`.

§I7. **Self-host stress** (per `audit/MODULES-2026-05-03.md:227-228` BBNF self-host's bootstrap pipeline). The post-W5c parser must lift the BBNF grammar source into IR with byte-equal output to pre-W5c; if the migration regresses, every downstream grammar regen breaks. M4 is the gate.

§I8. **No asymmetry** (per `feedback_no_workarounds` and CENSUS:199). Per surgery #19 the BBNF aggregator deletes; namespaced access for BBNF parallels every other grammar.

## §5 — Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Pratt operator-chain emission misclassifies BBNF's operator precedence | Medium | M2 + M4 self-host roundtrip | The precedence table is mined from grammar metadata; M2's verification reads post-regen `bbnf.rs` and inspects the OPERATOR_PRECEDENCE constant |
| Self-host roundtrip regresses because the post-W5c parser produces a different IR shape vs pre-W5c | High | M4 `cargo nextest run -p bbnf-bootstrap -E 'test(self_host)'` | Per `feedback_typed_materialization_invariant` every `->` reaches the emitter; if any rule's typed-equivalence breaks, root-cause via the IR-diff test (`bbnf-bootstrap dump_ir.rs` per MODULES:221) |
| BBNF aggregator deletion at M0 cascades into consumer breakage across `crates/analysis/`, `crates/bbnf-bootstrap/`, `crates/core/src/grammar/generated/` | Medium | M0 + M4 (consumer compilation) | Per `audit/CENSUS-2026-05-03.md:133-138` analysis/`features/formatting.rs` consumes `bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView}` — namespaced; BBNF-bootstrap's lib.rs updates to namespaced re-export at M0 |
| `bbnf.rs` LOC reduction (≤ 19,000) is missed because BBNF's grammar is more rule-heavy than JSON | Low | M6 LOC verification | The W0 comment scrub provides ~3% baseline reduction; speculative-checkpoint and `__layout` literal retiral provides additional ~9%; total ~12% shrinkage matches the budget |
| Post-W5c self-host bench regresses vs pre-W5c | Low | M5 bench output | Per `feedback_no_warm_benches` cold per-parse only; if regressed, profile-driven via samply |

## §6 — Cross-references

- **Closes BA-G1c** (per BA.md §Hard gates internal-progress row): post-W5c BBNF self-host roundtrip regression-bound.
- **Continues Lock 1 honour** (per BA.md §13-Lock honoured): BBNF OpenFrame retires; Sheets / cohort follow at W5d/e.
- **Honours Lock 9** (per BA.md): BBNF parse returns slice-borrowed `BbnfValue<'p>`.
- **Closes surgery #19**: BBNF aggregator `pub use bbnf::*` deleted; namespaced access only.
- **Preceding wave**: BA.W5b (CSS L4 direct-to-struct migration).
- **Following wave**: BA.W5d (Sheets direct-to-struct migration).
- **Producing**: Pratt operator-chain emission pattern; W5d/e do not need Pratt (Sheets and cohort grammars lack operator chains); the pattern carries forward to BB tranche grammars that do.
- **Consuming**: BA.W2.M4 BBNF aggregator disposition (per `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md`); BA.W0.M1 BBNF host-fn relocation (per surgery #15 extension).

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo xtask regen --grammar bbnf` | ≤ 22 s | exit 0 | Per-grammar regen for BBNF direct-projection + Pratt |
| `cargo nextest run -p bbnf -p bbnf-bootstrap -E 'test(bbnf) + test(self_host)' --profile ax-iter` | ≤ 18 s | 100% | Self-host roundtrip gate (M4) |
| `cargo bench --bench bench_bbnf -- self_host` | ≤ 60 s | regression-bound | BA-G1c internal-progress |
| `wc -l crates/core/src/grammar/generated/bbnf.rs` | < 1 s | ≤ 19,000 | Post-W5c generated-LOC budget gate |
| `rg -n '^pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs` | < 1 s | 0 | Aggregator-deletion gate |

## §8 — Verification artefacts

- `docs/tranches/BA/audit/W5c-bbnf-direct-to-struct.md` — per-grammar disposition artefact (M7); records BbnfValue typed-enum mapping + Pratt operator-chain emission + aggregator-disposition closure.
- `docs/tranches/BA/audit/W6-bbnf-aggregator-disposition.md` (consumed) — surgery #19 closure record.
- `docs/tranches/BA/audit/W5-generated-parser-shape.md` — per-construct emission shapes; W5c deviations (Pratt operator-chain emit per §1.x).

## §9 — Audit lane forecast

The W5c audit lane forecast: post-W5c, the following lanes progress:

- Lane 04 (sota anchoring) — BBNF self-host has no SOTA competitor; BA-G1c is regression-bound only.
- Lane 06 (generated code budget) — `bbnf.rs` ≤ 19,000 progresses the BA-tranche aggregate.
- Lane 05 (substrate audit) — BBNF OpenFrame retires; substrate_audit zero-caller count drops.

Lanes still open: Sheets / cohort retire OpenFrame at W5d/e; Lock 4 (e-graph) and Lock 10 (Pratt + SIMD auto-detected) close at BB.W3.

## §10 — Phase-4 surgery ledger

| Surgery # | Description | Landed at | Verification |
|---|---|---|---|
| 1 | Delete BBNF `OpenFrame` preservation | W5c (no longer deferred to BB.W1b) | M3 closer-gate |
| 2 | Lock 1 substrate identity flipped from option (b) to option (a) per user override; W5 split into W5a..W5e per-grammar | W5c | per-sub-wave closer-gates |
| 19 | BBNF aggregator `pub use bbnf::*` deletion | W5c.M0 (was W2.M4 surgery; closure at W5c) | M0 closer-gate |
| 11 | Per-grammar engineering gate (BA-G1c) marked regression-bound (no SOTA — no external competitor self-hosts a grammar language) | BA.md §Hard gates rewrite | BA.md §Hard gates row BA-G1c |
