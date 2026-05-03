# BB.W2b — Cursor Unification Across All 9 Grammars

**Thesis** Hereupon each of the 9 grammars' eager `parse(input)` rewrites as `parse_with(input, &__EAGER_EMPTY_PATH)` per Lock 3 + carry BA→BB.C3; the eager fast path elides cursor consultation entirely; samply traces verify zero cursor calls on each eager path. **Closer-gate** `tests/parse_with_<g>.rs` passes for all 9 grammars; `rg -n 'cursor.decide\|cursor.current_kind\|cursor.match_field' crates/core/src/grammar/generated/` returns zero on eager paths.

## §1 Deliverable

W2b extends the cursor + byte-skip unification (carry BA→BB.C3 from BA.W4) to all 9 grammars in the same wave per C03-13 of `audit/HARDENING-PLAN-2026-05-03-03-cohesion.md:27` (closing the duplicate-owner concern between BA.W4 and BB.W2). The unification was demonstrated on JSON in BA.W4; W2b generalises across the cohort + specialised.

Each grammar's eager `parse(input)` emits as:

```rust
pub fn parse<'i>(input: &'i str) -> Result<<G>Value<'i>, ParseErr> {
    static __EAGER_EMPTY_PATH: LazyLock<PathSegments> = LazyLock::new(PathSegments::empty);
    parse_with(input, &__EAGER_EMPTY_PATH)
}
```

The eager fast path elides cursor consultation: the `__EAGER_EMPTY_PATH` LazyLock holds a path with zero segments; the cursor's `decide`, `current_kind`, `match_field` methods are NOT called during eager parse; samply traces verify zero hits on these methods for the eager codepath.

The 9 grammars: JSON (already unified at BA.W4); CSS L4, BBNF, Sheets (specialised, post-W1{a,b,c}); BNF, CSV, EBNF, CSS Pretty, Math (cohort, post-W2a).

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W2b verification | Verify W2a closer-gate passed; cohort templated emission stable | `cargo nextest run -p bbnf --profile ax-iter` 100% pass | W2a baseline holds. |
| M1 | Per-grammar parse rewrite | Each of the 9 grammars' top-level `parse(input)` rewrites as `parse_with(input, &__EAGER_EMPTY_PATH)`; the rewrite emits at xtask regen time via the codegen layer | `rg -n 'fn parse<' crates/core/src/grammar/generated/' \| wc -l` returns 9; each fn body is `parse_with(input, &__EAGER_EMPTY_PATH)` | Eager parse → cursor parse_with bridge lands for all 9 grammars. |
| M2 | Cursor-call elision verification | Run samply trace on each grammar's eager `parse` invocation against representative inputs; verify zero hits on `cursor.decide`, `cursor.current_kind`, `cursor.match_field` | samply flame-graph for each grammar shows zero cursor symbol hits on eager path | Cursor consultation structurally precluded on eager path. |
| M3 | Per-grammar parse_with test | Each grammar has `tests/parse_with_<g>.rs` exercising both eager and path-driven parse; both 100% pass | `cargo nextest run -p bbnf --test parse_with_bnf --test parse_with_csv --test parse_with_ebnf --test parse_with_css_pretty --test parse_with_math --test parse_with_json --test parse_with_bbnf --test parse_with_css_l4 --test parse_with_google_sheets --profile ax-iter` 100% pass | Per-grammar test surface gates the unification. |
| M4 | Lock 3 verification artefact | Land `docs/tranches/BB/audit/W2b-cursor-unification.md` recording per-grammar samply traces + the rewrite shape | `test -f docs/tranches/BB/audit/W2b-cursor-unification.md` | Lock 3 unification artefact lands. |

## §3 Closer gate

```sh
rg -n 'fn parse<' crates/core/src/grammar/generated/ | wc -l                                      # 9
cargo nextest run -p bbnf --profile ax-iter                                                       # 100% pass
rg -n 'cursor\.(decide|current_kind|match_field)' crates/core/src/grammar/generated/ \
   | grep -v 'parse_with' | wc -l                                                                  # 0 (no cursor calls outside parse_with)
test -f docs/tranches/BB/audit/W2b-cursor-unification.md                                          # artefact lands
```

## §4 Invariants

§I1. **Lock 3** — cursor + byte-skip unified across all 9 grammars; `__EAGER_EMPTY_PATH` LazyLock at BA.W4 is the unification point.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Eager path interaction with grammar-specific behaviour regresses tests | Low | Per-grammar `tests/parse_with_<g>.rs` covers; each grammar's regression detection is mechanical. |
| samply trace shows residual cursor calls due to non-monomorphised `parse_with` body | Medium | The codegen emits the eager path with a monomorphised empty-path core (per BA.W4 deliverable carrying forward); the cursor-elision is structural, not policy-driven. |

## §6 Cross-references

- **Carry-tags consumed**: BA→BB.C3 (cursor unification from BA.W4).
- **Preceding wave**: BB.W2a.
- **Following wave**: BB.W2c.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo nextest run -p bbnf --profile ax-iter` | ≤ 90 s |
| `samply record -- cargo bench -p bbnf -- json_twitter` | ≤ 30 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W2b-cursor-unification.md` | `docs/tranches/BB/audit/` | Per-grammar samply traces + rewrite shape evidence |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L3 honoured across 9 grammars |
| Lane 2 | Same-wave consumer is the per-grammar test gate |
| Lane 5 | The unification mechanism is grammar-agnostic; the rewrite is uniform |
