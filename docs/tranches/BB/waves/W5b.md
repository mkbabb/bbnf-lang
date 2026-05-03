# BB.W5b — Visitor + VisitTypes

**Thesis** Hereupon per-grammar `Visitor<'i, T>` + `VisitTypes` bitflag emits at xtask regen time per the spec at `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md`; CSS L4 exposes `visit_color`, `visit_length`, `visit_url`, `visit_property`; JSON exposes `visit_string`, `visit_number`, `visit_object`, `visit_array`. The receiver in BC is BC.W4 not BC.W5 per surgery 30 + D08-6 of `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:15`. **Closer-gate** BB-G9 met; `cargo doc -p bbnf 2>&1 \| grep -c 'pub trait .*Visitor'` returns 9; `cargo bench -p bbnf -- visitor_bootstrap_css` ≤ 5 ms (≤ 1.4× of parse, lightningcss ratio).

## §1 Deliverable

W5b is the second of three W5 sub-waves. The visitor surface lands here; the cookbooks + diagnostic gates land at W5c.

Per surgery 30 of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:54` + D08-6, the Visitor receiving wave in BC is BC.W4 not BC.W5. The carry BB→BC.C3's receiver is corrected from the prior draft.

The visitor pattern per `audit/SOTA-2026-05-03.md:104-118`:

```rust
pub trait <G>Visitor<'i, T> {
    type Error;
    fn visit_types(&self) -> <G>VisitTypes;
    fn visit_<record1>(&mut self, _: &mut <Record1Type><'i>) -> Result<(), Self::Error> { Ok(()) }
    fn visit_<record2>(&mut self, _: &mut <Record2Type><'i>) -> Result<(), Self::Error> { Ok(()) }
    // ... per-grammar
}

bitflags! {
    pub struct <G>VisitTypes: u32 {
        const RECORD1 = 1 << 0;
        const RECORD2 = 1 << 1;
        // ... per-grammar; cardinality = record-count per surgery 21
    }
}
```

The per-record method emission emerges from the `<G>Value` enum cardinality. Per surgery 21 ("visitor delta bounded by record count"), the generated-LOC delta scales with record count.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W5b verification | Verify W5a `pointer!` + `LazyValue` lands | `cargo nextest run -p path --test pointer_macro --profile ax-iter` 100% pass | W5a baseline holds. |
| M1 | Visitor codegen | Each grammar's `<G>Visitor` trait + `<G>VisitTypes` bitflag emits at xtask regen time per `crates/core/src/codegen/visitor.rs` | `cargo doc -p bbnf 2>&1 \| grep -c 'pub trait .*Visitor'` returns 9 | Visitor codegen lands across 9 grammars. |
| M2 | CSS L4 visitor methods | CSS L4 exposes `visit_color`, `visit_length`, `visit_url`, `visit_property` per `audit/SOTA-2026-05-03.md:107-115` reference | `cargo doc -p bbnf` shows the four methods on `CssVisitor<'i, T>` | CSS L4 visitor methods exhaustive against SOTA reference. |
| M3 | JSON visitor methods | JSON exposes `visit_string`, `visit_number`, `visit_object`, `visit_array`; each method receives `&mut <RecordType>` | `cargo doc -p bbnf` shows the four methods on `JsonVisitor<'i, T>` | JSON visitor methods exhaustive. |
| M4 | Pruning verification | Tests verify CSS L4's `visit_color` is invoked exactly N times for the test fixture; `VisitTypes::COLORS` bitflag prunes non-color subtrees | `cargo nextest run -p bbnf --test visitor_pruning` 100% pass | `VisitTypes` bitflag works as traversal pruner. |
| M5 | Bench verification | CSS L4 visitor traversal of bootstrap.css ≤ 5 ms (≤ 1.4× of parse, lightning-css ratio) | `cargo bench -p bbnf -- visitor_bootstrap_css` ≤ 5 ms | Lightning-css ratio honoured. |
| M6 | Method count verification | Per-grammar method count = record count per surgery 21 | `cargo doc -p bbnf 2>&1 \| grep -A 100 'pub trait JsonVisitor'` shows method count = JSON's `<G>Value` cardinality (6) | Surgery 21 visitor delta bound honoured. |
| M7 | Coverage artefact | Land `docs/tranches/BB/audit/W5b-visitor-coverage.md` recording per-grammar Visitor + per-record method inventory | `test -f docs/tranches/BB/audit/W5b-visitor-coverage.md` | Coverage artefact lands. |

## §3 Closer gate

```sh
cargo doc -p bbnf 2>&1 | grep -c 'pub trait .*Visitor'                              # 9
cargo doc -p bbnf 2>&1 | grep -c 'fn visit_'                                        # ≥ 36 (4 methods × 9 grammars minimum)
cargo nextest run -p bbnf --test visitor_traversal --test visitor_pruning           # 100% pass
cargo bench -p bbnf -- visitor_bootstrap_css --profile ax-iter                      # ≤ 5 ms
test -f docs/tranches/BB/audit/W5b-visitor-coverage.md                              # artefact lands
```

## §4 Invariants

§I1. **Lock 5** — IR + per-backend lower precursor; the visitor is the per-backend traversal API.
§I2. **G05-8** of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:31` — typed pointer terminal alignment between path-API and visitor-API; the same typed-record taxonomy.
§I3. **Lock 8** — surpass lightningcss; visitor traversal ≤ 1.4× of parse (matching lightning-css ratio).
§I4. **Surgery 30 routing** — visitor receiver in BC is BC.W4 not BC.W5; the carry BB→BC.C3 reflects this.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Visitor traversal adds dispatch overhead on small documents | Medium | Bitflag check is a single AND; per-node cost constant; threshold ≤ 1.4× of parse cost. |
| Per-record method count explodes (record count grows beyond expected) | Low | Method count = record count by mechanism; per surgery 21 generated-LOC bound; if record count grows unexpectedly, the layout-lowering pass surface emits the additional records explicitly. |

## §6 Cross-references

- **BB-G gates closing**: BB-G9.
- **Carry-tags consumed**: BA→BB.C1 (direct-to-struct codegen scaffolding); BA→BB.C2 (Layout canon).
- **Carry-tags produced**: BB→BC.C3 (Visitor + VisitTypes; receiver BC.W4 per surgery 30).
- **Preceding wave**: BB.W5a.
- **Following wave**: BB.W5c.

## §7 Iter-time check

| Cargo Command | Expected Duration |
|---|---|
| `cargo nextest run -p bbnf --test visitor_traversal --test visitor_pruning --profile ax-iter` | ≤ 22 s |
| `cargo bench -p bbnf -- visitor_bootstrap_css --profile ax-iter` | ≤ 8 s |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W5b-visitor-coverage.md` | `docs/tranches/BB/audit/` | Per-grammar Visitor + per-record method inventory |
| `W5b-visittypes-bitflag.md` | same | Per-grammar `VisitTypes` bitflag definition; cardinality + assignment policy |

## §9 Audit lane forecast

| Lane | Response |
|---|---|
| Lane 1 | L5, L8 honoured |
| Lane 4 | Visitor traversal ≤ 1.4× of parse (lightning-css ratio) |
| Lane 6 | Method count = record count per surgery 21 |
| Lane 8 | Surgery 30 routing closes; BB→BC.C3 receiver is BC.W4 |
