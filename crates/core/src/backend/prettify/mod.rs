//! Backend-agnostic prettify planning and analysis.
//!
//! This module contains types, planning logic, and IR analysis used by all
//! backend emitters for prettify codegen. No target-specific code (TokenStream,
//! syntax, etc.) belongs here.

pub mod analysis;
pub mod plan;
pub mod sep_rewrite;
pub mod types;

pub use analysis::{emits_only_on_success, may_open_groups};
pub use plan::build_rule_plans;
pub use sep_rewrite::{SilentPosition, split_inner_for_sep};
pub use types::*;
