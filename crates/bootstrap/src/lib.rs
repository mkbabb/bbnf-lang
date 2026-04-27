//! Bootstrap crate for the self-hosted BBNF grammar parser.
//!
//! Pre-B2: this crate carried `the proc-macro derive (retired B2)` and was the source
//! `cargo expand -p bbnf-bootstrap` post-processed into
//! `crates/core/src/grammar/generated.rs` via `scripts/bootstrap-bbnf.sh`.
//!
//! Post-B2.W0.c: the IR pipeline runs in `cargo xtask regen`, which
//! emits `crates/core/src/grammar/generated/bbnf.rs` directly. The
//! `bbnf` crate `include!`s that file and re-exports `BbnfBootstrap`
//! at `bbnf::grammar::generated::BbnfBootstrap`. This crate becomes a
//! thin re-export so existing consumers of `bbnf_bootstrap::BbnfBootstrap`
//! continue to resolve through the canonical type.
//!
//! Per `docs/tranches/B2/audit/W0-per-grammar-boundary.md` §3.3, this
//! unifies the previously-divergent `bbnf::grammar::generated::BbnfBootstrap`
//! (proc-macro relocated by `bootstrap-bbnf.sh`) and
//! `bbnf_bootstrap::BbnfBootstrap` (proc-macro raw expansion) into one.
//!
//! Post-B6.W0: `cargo xtask regen` skips the on-disk write when the
//! IR pipeline's emitted bytes match the existing file's bytes. This
//! preserves `crates/core/src/grammar/generated/bbnf.rs` mtime across
//! no-op regens and lets cargo reuse the cached `bbnf` rmeta —
//! cold-wall regen drops from ~88 s to ~0.5 s when the regen produces
//! identical output, which is the steady-state condition. The
//! `BbnfBootstrap::parse` route remains the single self-host parse
//! entry; W0 made the build wrapping it cheap, not the parse itself.

pub use ::bbnf::grammar::generated::BbnfBootstrap;
