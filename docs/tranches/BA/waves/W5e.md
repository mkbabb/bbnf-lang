# BA.W5e — Five-Grammar Cohort Direct-To-Struct Migration

**Thesis** (the five-grammar cohort — BNF, CSV, EBNF, CSS Pretty, Math — each retire their `OpenFrame` substrate via hand-written direct-to-struct emission per grammar; the templating consolidation is BB.W2's concern per `feedback_no_deferrals` (BA owns five hand-written modules; BB.W2 absorbs into one parameterised template); each cohort grammar parses its fixture without regression; Lock 1 honour completes at BA close — `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 across all nine grammars). **Closer-gate** (post-W5e `rg -n 'enum OpenFrame' crates/core/src/runtime/` returns 0 across BNF/CSV/EBNF/CSS Pretty/Math; cohort fixture parses for each grammar; `cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)' --profile ax-iter` 100% pass; the dual-strategy resolver collapses — `EmitStrategy::OpenFrame` variant deletes from `crates/ir/src/registry/strategy.rs`).

## §1 — Deliverable

Hereupon the five-grammar cohort retires its `OpenFrame` substrate. Each cohort grammar — BNF, CSV, EBNF, CSS Pretty, Math — has a 54-55 LOC builder shim onto `SimpleStructBuilder` per `audit/MODULES-2026-05-03.md:993` and a 134 LOC arena via `arena_template.rs` per MODULES:921. The W5e migration is mechanical per grammar — each cohort grammar gets a hand-written direct-to-struct emission module; templating consolidation is BB.W2's concern per `feedback_no_deferrals` (BA owns the five hand-written modules; BB.W2 absorbs into one parameterised template per gap D `docs/tranches/BB/audit/W2-cohort-template-spec.md`).

Per `docs/tranches/BA/audit/W5-generated-parser-shape.md` §1.1-§1.10 the per-construct emission shapes are JSON-side canonical; cohort-grammar deviations are minimal because each cohort grammar exercises a strict subset of constructs:

| Cohort grammar | Constructs exercised | Pre-W5e gen LOC | Post-W5e target |
|---|---|---:|---:|
| BNF | Alt (byte-disjoint), Seq, Repeat, Optional, CharClass, Keyword | 3,290 | ≤ 3,000 |
| CSV | Alt (byte-disjoint), Seq, Repeat, CharClass, Scanner | 1,693 | ≤ 1,500 |
| EBNF | Alt (byte-disjoint, speculative), Seq, Repeat, Optional, CharClass, Keyword | 7,646 | ≤ 7,000 |
| CSS Pretty | Alt (byte-disjoint, speculative), Seq, Repeat, Optional, CharClass, Keyword, MapExpr | 9,021 | ≤ 8,500 |
| Math | Alt (byte-disjoint), Seq, Repeat, CharClass, Keyword, Pratt operator chain | 871 | ≤ 800 |

Each cohort grammar's typed-enum (`BnfValue<'p>`, `CsvValue<'p>`, `EbnfValue<'p>`, `CssPrettyValue<'p>`, `MathValue<'p>`) preserves its rich AST per `feedback_preserve_rich_ast`. The hand-written direct-to-struct module for each grammar is mechanical: replace `OpenFrame` push/pop with typed-enum constructor; replace `__layout` runtime literal with codegen-known type info; replace speculative `Vec<OpenFrame>::clone` with O(1) tuple Checkpoint (or no checkpoint on byte-disjoint cases).

The W5e emit shape for Math's `expr` rule (representative; Pratt operator chain):

```rust
fn parse_expr<'p>(
    input: &'p [u8], p: &mut usize, arena: &mut MathArena<'p>,
    state: &mut ScanState, cursor: &mut PathCursor<'_>,
) -> Result<MathValue<'p>, ParseErr> {
    parse_pratt(input, p, arena, state, cursor, /* min_prec */ 0)
}

fn parse_pratt<'p>(
    input: &'p [u8], p: &mut usize, arena: &mut MathArena<'p>,
    state: &mut ScanState, cursor: &mut PathCursor<'_>,
    min_prec: u32,
) -> Result<MathValue<'p>, ParseErr> {
    let mut lhs = parse_atom(input, p, arena, state, cursor)?;
    loop {
        skip_space(input, p, state);
        let op = match input.get(*p).copied() {
            Some(b) if let Some(op) = MATH_OPERATOR_TABLE.get(&b) => op,
            _ => break,
        };
        if op.precedence < min_prec { break; }
        *p += 1;
        skip_space(input, p, state);
        let rhs = parse_pratt(input, p, arena, state, cursor, op.precedence + 1)?;
        lhs = MathValue::Binary(arena.intern_binary(op.kind, lhs, rhs));
    }
    Ok(lhs)
}
```

Each of the five cohort modules is hand-written at W5e; BB.W2 consolidates them via the cohort template per gap D. Per `feedback_no_deferrals` BA owns the migration; BB.W2 owns the consolidation. Per `feedback_no_workarounds` no shim survives BA close (the `SimpleStructBuilder` template per MODULES:923 is consumed-and-deleted within W5e — each cohort grammar's `runtime/<g>/builder.rs` shrinks from 54-55 LOC builder shim to a minimal arena-management surface, with the `SimpleStructBuilder` template itself retiring).

The dual-strategy resolver collapses at W5e close: `crates/ir/src/registry/strategy.rs` post-W5e has only `EmitStrategy::DirectToStruct` (the `OpenFrame` variant deletes); per `feedback_no_orthogonal_codepaths` the singular strategy is the post-BA invariant.

The performance trajectory: each cohort grammar has no SOTA competitor; BA-G1e is regression-bound (each cohort grammar parses its fixture; no regression vs pre-W5e). The cohort waves are mechanical — the typed-enum + direct-projection emit are the deliverables; perf claims are deferred to BB tranche if external competitors surface.

The Era V failure mode is closed for the cohort at W5e; the same-wave consumer is the cohort fixture run + `cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)'` at 100% pass.

## §2 — Milestones

> **M0 — Per-cohort-grammar typed-enum lands**
>
> *Surface*: `crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math}/value.rs` (per `audit/MODULES-2026-05-03.md:985-987`).
> *Action*: For each cohort grammar, land typed-enum `<G>Value<'p>` per `feedback_preserve_rich_ast`. Each grammar's typed-enum preserves its rich AST; per-grammar variant tables:
>
> | Grammar | Variant count | Sample variants |
> |---|---:|---|
> | BNF | 6+ | Rule, Alt, Seq, Identifier, Literal, Reference |
> | CSV | 4+ | Row, Field, Number, String |
> | EBNF | 8+ | Rule, Alt, Seq, Repeat, Optional, Group, Identifier, Literal |
> | CSS Pretty | 7+ | Stylesheet, Rule, Selector, Declaration, Value, AtRule, Comment |
> | Math | 6+ | Number, Identifier, Binary, Unary, Call, Group |
>
> *Gate*: each cohort grammar's typed-enum lands; variant counts ≥ table.
> *Exit-criteria*: `for g in bnf csv ebnf css_pretty math; do rg -n "${g^}Value::" /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/$g/value.rs | wc -l; done | awk '$1>=4 {n++} END {print (n==5)}' | tr -d '\n'` returns `1`.

> **M1 — Per-rule `parse_<rule>` direct-projection emit for each cohort grammar**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct/{header,body,fields,finalize}.rs`; the cohort grammars at `grammar/{bnf,csv,ebnf,css_pretty,math}/<g>.bbnf`; the regen targets `crates/core/src/grammar/generated/{bnf,csv,ebnf,css_pretty,math}.rs`.
> *Action*: Implement direct-projection emit for every cohort grammar's rule. Per W5-generated-parser-shape §1.1-§1.10: byte-disjoint Alt → direct `match`; speculative Alt (EBNF, CSS Pretty) → O(1) tuple Checkpoint; Pratt operator chain (Math) → operator-token lookup table.
> *Gate*: post-regen each cohort `<g>.rs` contains direct `parse_<rule>` functions; no `OpenFrame` references.
> *Exit-criteria*: `for g in bnf csv ebnf css_pretty math; do cargo xtask regen --grammar $g; rg -n 'OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/$g.rs 2>&1 | wc -l; done | awk '$1==0 {n++} END {print (n==5)}' | tr -d '\n'` returns `1`.

> **M2 — `OpenFrame` deletes from each cohort path; SimpleStructBuilder template retires**
>
> *Surface*: `crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math}/builder.rs` (54-55 LOC each per MODULES:993); `crates/core/src/runtime/builder_template.rs` (286 LOC; `SimpleStructBuilder` per MODULES:923).
> *Action*: Delete `enum OpenFrame` from each cohort grammar's runtime; reduce each `<G>StructBuilder` to a minimal arena-management surface; retire the `SimpleStructBuilder` + `SimpleValue` template (the cohort grammars no longer consume it; the template is dead code post-W5e). Per the BA.md §Generated-LOC budget table the post-W5e cohort generated LOCs are: `bnf.rs ≤ 3,000`, `csv.rs ≤ 1,500`, `ebnf.rs ≤ 7,000`, `css_pretty.rs ≤ 8,500`, `math.rs ≤ 800`.
> *Gate*: cohort `enum OpenFrame` is gone; `SimpleStructBuilder` template retires.
> *Exit-criteria*: `rg -n 'enum OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/{bnf,csv,ebnf,css_pretty,math}/ 2>&1 | wc -l | tr -d '\n'` returns `0`; `test ! -f /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/builder_template.rs` returns truthy OR the file is reduced to ≤ 50 LOC of arena-template-only surface.

> **M3 — Cohort fixture parsing passes**
>
> *Surface*: `crates/core/tests/cohort_*.rs` (or per-grammar `tests/parse_with_<g>.rs` files).
> *Action*: Run `cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)' --profile ax-iter`; verify 100% pass. Each cohort grammar parses its fixture; byte-equality roundtrip per cohort grammar.
> *Gate*: 100% pass on cohort tests.
> *Exit-criteria*: `cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M4 — Strategy resolver collapses — `EmitStrategy::OpenFrame` variant deletes**
>
> *Surface*: `crates/ir/src/registry/strategy.rs:130-185` (per `audit/CENSUS-2026-05-03.md:115`); the metadata-driven resolver from BA.W1.M1.
> *Action*: Per `feedback_no_orthogonal_codepaths` the dual-strategy resolver was transient through W5a..W5d; W5e collapses to one strategy. Delete `EmitStrategy::OpenFrame` variant; the resolver returns only `EmitStrategy::DirectToStruct` for every grammar. Per surgery #1 the OpenFrame substrate dies in BA — strategy.rs reflects.
> *Gate*: `EmitStrategy::OpenFrame` variant absent; resolver returns DirectToStruct only.
> *Exit-criteria*: `rg -n 'EmitStrategy::OpenFrame' crates/ir/src/registry/strategy.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'EmitStrategy::DirectToStruct' crates/ir/src/registry/strategy.rs | wc -l | tr -d '\n'` returns ≥ 1.

> **M5 — BA-G1e: Each cohort grammar parses fixture without regression**
>
> *Surface*: bench harness for cohort grammars; per-grammar fixture.
> *Action*: Run `cargo bench --bench bench_cohort -- <grammar>` post-regen for each cohort grammar (or equivalent harness); record measurement. BA-G1e is regression-bound (no SOTA target — no external competitor at this scale); post-W5e measurement ≤ pre-W5e × 1.05 per cohort grammar.
> *Gate*: post-W5e bench output recorded for each cohort grammar; no regression vs pre-W5e.
> *Exit-criteria*: each cohort grammar's bench `mean ≤ pre-W5e × 1.05`.

> **M6 — Cohort generated-LOC budget verification**
>
> *Surface*: pre-W5e `crates/core/src/grammar/generated/{bnf,csv,ebnf,css_pretty,math}.rs`; post-W5e same files.
> *Action*: Per the BA.md §Generated-LOC budget table the post-W5e per-grammar targets are: `bnf.rs ≤ 3,000`, `csv.rs ≤ 1,500`, `ebnf.rs ≤ 7,000`, `css_pretty.rs ≤ 8,500`, `math.rs ≤ 800`.
> *Gate*: each cohort grammar's `<g>.rs` post-W5e LOC ≤ target.
> *Exit-criteria*: `for g in bnf csv ebnf css_pretty math; do wc -l /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/$g.rs; done | awk '($2~/bnf\.rs/&&$1<=3000)||($2~/csv\.rs/&&$1<=1500)||($2~/ebnf\.rs/&&$1<=7000)||($2~/css_pretty\.rs/&&$1<=8500)||($2~/math\.rs/&&$1<=800) {n++} END {print (n==5)}' | tr -d '\n'` returns `1`.

> **M7 — Lock 1 honoured: full all-grammar OpenFrame retiral verified**
>
> *Surface*: `crates/core/src/runtime/` (all nine grammars).
> *Action*: Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` option (a), Lock 1 honoured at BA close requires `rg -n 'enum OpenFrame' crates/core/src/runtime/` returning 0 across JSON, CSS L4, BBNF, Sheets, BNF, CSV, EBNF, CSS Pretty, Math. M7 verifies.
> *Gate*: zero `enum OpenFrame` declarations across all nine grammar runtime modules.
> *Exit-criteria*: `rg -n 'enum OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/ 2>&1 | wc -l | tr -d '\n'` returns `0`.

> **M8 — W5e artefact emission**
>
> *Surface*: `docs/tranches/BA/audit/W5e-cohort-direct-to-struct.md` (new artefact).
> *Action*: Emit a per-cohort disposition artefact recording (a) per-cohort-grammar pre-/post-W5e generated-LOC, (b) sample per-cohort `parse_<rule>` body, (c) per-cohort fixture parse pass-rate, (d) per-cohort typed-enum mapping table, (e) cohort template hand-off to BB.W2 (the five hand-written modules' parameter-table extracts; BB.W2 consolidates).
> *Gate*: artefact exists; records the five evidence items.
> *Exit-criteria*: `test -f /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5e-cohort-direct-to-struct.md && wc -l /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5e-cohort-direct-to-struct.md | awk '{print ($1 > 100)}' | tr -d '\n'` returns `1`.

## §3 — Closer gate

```
# All-grammar OpenFrame deleted (Lock 1 honoured at BA close)
rg -n 'enum OpenFrame' crates/core/src/runtime/        ; expect: 0 matches

# Per-cohort generated-LOC budget verified
for g in bnf csv ebnf css_pretty math; do
  wc -l crates/core/src/grammar/generated/$g.rs
done                                                   ; expect: per-grammar ≤ target

# Strategy resolver collapsed
rg -n 'EmitStrategy::OpenFrame' crates/ir/src/registry/strategy.rs
                                                       ; expect: 0 matches

# Cohort nextest 100% pass
cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)' --profile ax-iter
                                                       ; expect: 100% pass

# BA-G1e cohort regression-bound
for g in bnf csv ebnf css_pretty math; do
  cargo bench --bench bench_cohort -- $g
done                                                   ; expect: each ≤ pre-W5e × 1.05
```

## §4 — Invariants

§I1. **Lock 1 honoured at BA close** (final). Per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` option (a), Lock 1 closes at W5e — every grammar's `OpenFrame` retires; the substrate is dead. The BA close lock-honoured table cell L1 reads "honoured at W5a..W5e".

§I2. **Lock 3** (one parse impl). The W4-introduced `parse_with(input, &path)` is the single parse implementation; W5e's direct-projection rewrites the body but preserves the unified surface.

§I3. **Lock 9** (slice-borrow primary). Each cohort grammar's typed-enum is slice-borrowed; `&'p str` over input for every grammar at BA close.

§I4. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The strategy resolver collapses at M4 — `EmitStrategy::DirectToStruct` is the singular strategy; the dual-strategy transient retires.

§I5. **No combinators monolithic** (per `feedback_no_combinators_monolithic`). Cohort emit extends alt-classifier (M1) — no combinator/SpanParser usage.

§I6. **Preserve rich AST** (per `feedback_preserve_rich_ast`). Each cohort grammar's typed-enum preserves shape: 4-7 variants per grammar.

§I7. **BB.W2 cohort template hand-off** (per `feedback_no_deferrals`). BA owns the five hand-written modules; BB.W2 consolidates into one parameterised template per gap D. The hand-off is named at M8's artefact (per-grammar parameter-table extracts).

§I8. **No asymmetry** (per `feedback_no_workarounds`). All nine grammars use the namespaced access path post-W5e (BBNF aggregator deletion at W5c.M0); the cohort grammars already use namespaced access pre-W5e per `audit/CENSUS-2026-05-03.md:199`.

## §5 — Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Hand-written direct-to-struct emission for five grammars introduces per-grammar inconsistencies | Medium | M3 cohort fixture parse | Per `feedback_no_workarounds` the five modules are mechanical replicas of W5a/b/c/d patterns; M8's per-grammar parameter-table extract surfaces inconsistencies; BB.W2 consolidation enforces uniformity |
| `SimpleStructBuilder` template retiral at M2 cascades into the post-W2 builder modules | Medium | `cargo check --workspace` post-M2 | The template was consumed only by the cohort grammars; W5e's hand-written direct-to-struct emission supplants it; the template's `arena_template.rs` may survive as arena-only utility |
| Cohort generated-LOC budgets missed (e.g. `ebnf.rs > 7,000`) | Low | M6 LOC verification | The 8-12% per-grammar shrinkage matches the W5a..W5d pattern; if missed, profile-driven |
| Pratt emission for Math at M1 misclassifies operator precedence | Medium | M3 Math fixture parse | M1's verification reads post-regen `math.rs` and inspects the `MATH_OPERATOR_TABLE` constant; precedence table is mined from grammar metadata |
| Strategy resolver collapse at M4 leaves dead OpenFrame references in metadata | Low | M4 + cargo check | M4's verification: `rg -n 'EmitStrategy::OpenFrame' crates/ir/src/registry/strategy.rs` returns 0; metadata cleanup enforced |

## §6 — Cross-references

- **Closes BA-G1e** (per BA.md §Hard gates internal-progress row): each cohort grammar parses fixture without regression.
- **Closes Lock 1** (per BA.md §13-Lock honoured): the all-grammar OpenFrame retiral completes at BA close.
- **Honours Lock 9** (per BA.md): every grammar's parse returns slice-borrowed `<G>Value<'p>`.
- **Hands off to BB.W2**: per `feedback_no_deferrals` BA owns the five hand-written cohort modules; BB.W2 consolidates into one parameterised template per gap D.
- **Preceding wave**: BA.W5d (Sheets direct-to-struct migration).
- **Following wave**: BA.W6 (BA close; Lock cross-reference table verifies Lock 1 honoured).
- **Producing**: per-cohort hand-written direct-to-struct modules; W5e is the precursor to BB.W2's cohort template consolidation.
- **Consuming**: BA.W5a/b/c/d direct-to-struct emission patterns (canonical exemplars).

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo xtask regen --grammar bnf -p csv -p ebnf -p css_pretty -p math` (parallel where supported) | ≤ 25 s | exit 0 | Per-grammar regen for cohort direct-projection (parallel-capable) |
| `cargo nextest run -p bbnf -E 'test(bnf) + test(csv) + test(ebnf) + test(css_pretty) + test(math)' --profile ax-iter` | ≤ 30 s | 100% | Cohort fixture parse gate (M3) |
| `cargo bench --bench bench_cohort` | ≤ 90 s | regression-bound per grammar | BA-G1e internal-progress |
| `for g in bnf csv ebnf css_pretty math; do wc -l crates/core/src/grammar/generated/$g.rs; done` | < 1 s | per-grammar ≤ target | Post-W5e generated-LOC budget gate |
| `rg -n 'enum OpenFrame' crates/core/src/runtime/` | < 1 s | 0 | Lock 1 honoured at BA close |
| `rg -n 'EmitStrategy::OpenFrame' crates/ir/src/registry/strategy.rs` | < 1 s | 0 | Strategy resolver collapse |

## §8 — Verification artefacts

- `docs/tranches/BA/audit/W5e-cohort-direct-to-struct.md` — per-cohort disposition artefact (M8); records per-grammar typed-enum mapping + cohort template hand-off parameter table for BB.W2.
- `docs/tranches/BA/audit/W5-substrate-identity-decision.md` (consumed) — Lock 1 option (a) closure record at W5e.
- `docs/tranches/BA/audit/W5-generated-parser-shape.md` — per-construct emission shapes; W5e is the canonical exemplar for the simpler cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math).

## §9 — Audit lane forecast

The W5e audit lane forecast: post-W5e, the following lanes close:

- Lane 04 (sota anchoring) — cohort grammars have no SOTA competitor; BA-G1e is regression-bound only.
- Lane 06 (generated code budget) — every cohort grammar's `<g>.rs` ≤ target; BA-tranche aggregate ≤ ~150,000 (down from 168,750 pre-BA per BA.md §Generated-LOC budget table; cumulative ~11% shrinkage).
- Lane 05 (substrate audit) — all-grammar OpenFrame retires; substrate_audit zero-caller count is at minimum (or whitelisted with rationale at W6.M5).

Lock 1 closes at BA close per BA.W6.M1 lock cross-reference table verification. Lock 4 + Lock 10 + Lock 11 close at BB tranche.

## §10 — Phase-4 surgery ledger

| Surgery # | Description | Landed at | Verification |
|---|---|---|---|
| 1 | Delete cohort `OpenFrame` preservation | W5e (no longer deferred to BB.W2) | M2 closer-gate |
| 2 | Lock 1 substrate identity flipped from option (b) to option (a) per user override; W5 split into W5a..W5e per-grammar | W5e (closure) | M7 closer-gate |
| 11 | Per-grammar engineering gate (BA-G1e) marked regression-bound (no SOTA — no external competitor at cohort scale) | BA.md §Hard gates rewrite | BA.md §Hard gates row BA-G1e |
| 24 | BA→BB carry retires per-grammar OpenFrame migration scope; renumbered C1' carry is cohort hand-written → BB.W2 template-consolidation only | BA.md §Carry-tags TO BB rewrite | §Carry-tags TO BB row BA→BB.C1' |
