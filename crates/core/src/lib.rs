// B5.W1 — self-alias retired alongside the welded substrate
// boundary. Generated code uses `crate::runtime::*` paths uniformly
// inside the `bbnf` crate; downstream consumers (`bbnf-bootstrap`,
// integration tests) reach the same types via `bbnf::runtime::*`
// where `bbnf` is the cargo dependency, not the self-alias.

pub mod types;
pub use types::*;

/// Re-export parse_that types used by the grammar-generated enum.
pub use parse_that::Span;

// Host shims for grammar-side `-> crate::<module>::<fn>(...)` map
// annotations. Pre-B2 these lived in test-common modules; post-B2.W1
// the xtask-emitted source resides under `bbnf` lib so `crate::`
// resolves here. Production callers (binaries, benches, library
// consumers) reach the symbol via `bbnf::css_types::parse_hex_color`;
// some test crates still inline a `mod css_types { ... }` shadow for
// hermetic-compilation reasons (tracked by REAUDIT-2026-04-30 lane 3
// §5.1 as a pending mechanical-fix item).
pub mod css_types;

pub mod grammar;

pub mod generate;
pub use generate::*;

pub mod backend;

pub mod graph;
pub use graph::*;

pub mod imports;

pub mod lower;
pub mod pipeline;
pub mod runtime;
