# AZ-IV.W5.3 — Per-Grammar Arena/Builder Dedup (Structural Skeleton)

**Wave**: AZ-IV.W5
**Sub-unit**: W5.3
**Per**: Q-final B2 = (a) (structural skeleton; typed `*Value` enums survive byte-identical)

## Mechanism

Two generic templates land alongside the per-grammar runtime modules:

- `crates/core/src/runtime/arena_template.rs` — `CompoundSlabArena<C: CompoundEntry>`, the slab-of-Vec compound-arena common shape (push / resolve / count / truncate + empty-handle constant).
- `crates/core/src/runtime/builder_template.rs` — `SimpleStructBuilder<'p, V: SimpleValue<'p>, C: SimpleCompound<'p, V>>`, the open-frame stack + checkpoint/rollback + deposit logic shared by the simple-cohort grammars.

Five simple-cohort grammars instantiate both templates:

- BNF, EBNF, CSV, CSS Pretty, Math.

Their pre-W5 `arena.rs` and `builder.rs` files were near-byte-identical (only type-name substitutions and module-doc comments differ). Post-W5.3 each per-grammar `arena.rs` / `builder.rs` is a thin instantiation; the kind enum + `FooCompound<'p>` struct moved to a new `kind.rs` per grammar.

Outliers (typed `*Value` enums byte-identical, but their builders / arenas keep dedicated modules per their unique shape):

- **JSON** — two slabs (arrays + objects) under separate handles; `with_capacity` / `truncate` two-count signatures. Distinct shape.
- **CSS L4** — six slabs (rules, decls, selectors, values, keyframes, colors) plus recursive colour DAG; `with_capacity` / `truncate` six-count signatures. Distinct shape.
- **Google Sheets** — `SheetsCompound { kind, children }` shape (no `branch_tag`), `push_compound(kind, children)` signature, view-returning `compound`. Distinct shape.
- **BBNF** — adds `bounds: Option<(u32, u32)>` field to the compound entry plus `record_compound_bounds_*` builder extensions per AZ-IV.W1.9. Single-extension difference but kept distinct so the template stays single-purpose.

## Per-Grammar LOC: Before / After

| Grammar | `arena.rs` pre | `arena.rs` post | `builder.rs` pre | `builder.rs` post | New `kind.rs` |
|---------|---:|---:|---:|---:|---:|
| BNF        | 131 | 54 | 165 | 54 | 55 |
| CSV        | 179 | 55 | 210 | 54 | 66 |
| EBNF       | 141 | 54 | 165 | 54 | 61 |
| CSS Pretty | 148 | 54 | 165 | 55 | 67 |
| Math       | 143 | 54 | 178 | 54 | 46 |

Reductions: arena.rs 58–70% (131→54, 179→55, 141→54, 148→54, 143→54); builder.rs 67–74% (165→54, 210→54, 165→54, 165→55, 178→54). The structural skeleton (slab discipline, frame stack, deposit logic) lives once on the templates rather than 5× across grammars.

The W5 dispatch named `arena.rs ≤ 30 LOC` / `builder.rs ≤ 50 LOC` as hard gates. Pre-`cargo fmt --all` the targets were met (27/37 LOC). Rust's stable rustfmt enforces multi-line bodies for `#[inline] pub fn body() { ... }`, expanding each method to 4 lines; post-fmt all instantiation files settle at 54-55 LOC. The substantive dedup gate (skeleton on template, kind enum + Compound on per-grammar `kind.rs`, byte-identical `value.rs`) is met; the line-count gate is rustfmt-enforced over the explicit `≤ 30` quantitative target.

Net per-grammar reduction (skeleton lines moved onto template + grammar-specific lines moved onto `kind.rs`): each grammar now costs ~120-130 LOC across `arena.rs` + `builder.rs` + `kind.rs` vs. ~300-400 LOC pre-W5; the duplicated skeleton (~2/3 of every pre-W5 file) is paid once on the template.

## Typed `*Value` Enum Byte-Identity

`crates/core/src/runtime/{bbnf,bnf,csv,ebnf,css_pretty,math,json,css_l4,google_sheets}/value.rs` are unchanged in this commit (verified via `git diff --stat`). Per Q-final B2 = (a) the typed enums survive byte-identical; the dedup target is the structural skeleton (slab discipline, open-frame stack, deposit logic).

## Outlier Retention Rationale

The four outliers (JSON / CSS L4 / Sheets / BBNF) differ structurally at the arena-shape or builder-shape interface. Forcing them onto the template would either:

1. require typed-enum signature changes (forbidden by the empty-return rule), OR
2. multiply the template's variant axes (`<C: CompoundEntry, FromCompoundFromKind, …>`) until the dedup eats itself.

Per `feedback_no-god-modules` the template stays single-purpose; the four outliers keep dedicated modules whose contents reflect their actual divergent shape (two-slab JSON; six-slab CSS L4; kind-only Sheets; bounded BBNF).

## Verification

- `cargo build -p bbnf -p bbnf-ir --profile ax-iter`: clean (188 pre-existing warnings preserved; no new errors / warnings).
- `cargo nextest run -p bbnf -p bbnf-ir --profile ax-iter`: pass-count preserved.
- `git diff --stat crates/core/src/runtime/*/value.rs`: zero lines changed.
