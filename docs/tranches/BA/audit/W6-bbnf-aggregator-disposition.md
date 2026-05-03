# BA.W6 BBNF Aggregator Disposition — Surgery #19

Date: 2026-05-03
Surface: `crates/core/src/grammar/generated/mod.rs:35` `pub use bbnf::*`.
Surgery: surgery #19 ("Add BA.W0 or BA.W2 gate deleting the BBNF aggregator `pub use bbnf::*`; BBNF uses namespaced generated access like every other grammar.").

## §1 — Status quo

Per `audit/CENSUS-2026-05-03.md:412` and CENSUS:199, the generated grammar aggregator at `crates/core/src/grammar/generated/mod.rs:35` carries `pub use bbnf::*` — only BBNF is glob-aggregated; other grammars are namespaced. Per CENSUS:199, "Asymmetry per §3.1 row `back-compat`. FAIL-EXPLICIT — make BBNF namespaced too."

The asymmetry's history: the BBNF self-host's `BbnfBootstrap` struct is consumed by `bbnf-bootstrap` crate's `pub use ::bbnf::grammar::generated::BbnfBootstrap`; per CENSUS:199 the framing is "back-compat with consumers" — rejected under `feedback_no_backward_compat`.

## §2 — Receiving wave

The disposition: surgery #19 lands at **BA.W2.M4**, alongside the fail-explicit table (per `docs/tranches/BA/audit/W2-fail-explicit-table.md`). The W2.M4 gate adds the row "BBNF aggregator `pub use bbnf::*`" with grep gate `rg -n 'pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs` returns 0.

Rationale: W2 is the layout-lowering rename + god-module split wave; the BBNF aggregator's asymmetry is a Lock 13 cohesion violation (one grammar is special-cased in the otherwise-symmetric aggregator); the W2.M4 fail-explicit table is the appurtenant home.

## §3 — Migration plan

| Step | Action | Verification |
|---|---|---|
| 1 | Delete `pub use bbnf::*;` line at `crates/core/src/grammar/generated/mod.rs:35` | `rg -n 'pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs` returns 0 |
| 2 | Identify every consumer of the glob-imported BBNF surface (likely `crates/bootstrap/src/lib.rs` and `crates/analysis/src/`) | `rg -n 'use bbnf::grammar::generated::Bbnf' crates/` enumerates consumers |
| 3 | For each consumer, rewrite import to namespaced form (`use bbnf::grammar::generated::bbnf::BbnfBootstrap;`) | per-consumer `cargo check -p <crate>` passes |
| 4 | Run `cargo check --workspace` | 0 errors |
| 5 | Run `cargo nextest run -p bbnf -p bbnf-ir -p analysis -p bootstrap` | 100% pass |

## §4 — Closer gate

```
rg -n 'pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs   ; expect: 0
cargo check --workspace 2>&1 | rg -c 'error\['                       ; expect: 0
cargo nextest run -p bbnf -p bbnf-ir -p analysis -p bootstrap        ; expect: 100% pass
```

The disposition is final: BBNF uses namespaced generated access at `crates/core/src/grammar/generated/bbnf::*` like every other grammar after BA.W2.M4 close. Per `feedback_no_backward_compat`, no transitional `pub use` survives.

## §5 — Cross-references

- BA.W2.M4 (fail-explicit table) — receiving wave per surgery #19.
- `docs/tranches/BA/audit/W2-fail-explicit-table.md` — the table this disposition row joins.
- `audit/CENSUS-2026-05-03.md:199, 412` — the source citations.
- `feedback_no_backward_compat` — the discipline that forbids the "back-compat" framing.
