//! Regex-to-inline-code emission.
//!
//! Three tiers of regex code generation:
//! 1. **HIR walker** (`try_emit_regex_inline`): Walks regex-syntax HIR to emit
//!    direct byte operations. Best for simple patterns (classes, loops, concat).
//! 2. **DFA compiler** (`try_emit_dfa_inline`): Compiles pattern to a minimized
//!    DFA and emits inline state machine or transition table. Handles everything
//!    the HIR walker can't (complex alternation, Unicode properties, lazy quantifiers).
//! 3. **LazyLock fallback** (`emit_regex_lazy_static`): Last resort, runtime regex
//!    compilation. Should never be reached — the DFA tier covers all supported features.

pub mod audit;
mod dfa_emit;
mod fallback;
mod hir_walk;

pub use dfa_emit::try_emit_dfa_inline;
pub use fallback::emit_regex_lazy_static;
pub use hir_walk::try_emit_regex_inline;
