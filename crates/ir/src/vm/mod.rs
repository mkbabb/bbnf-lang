//! Bytecode VM: compiler, interpreter, and opcode definitions.
//!
//! This module groups the three VM components that were previously top-level
//! files in `bbnf-ir`. All public types are re-exported from the crate root
//! for backward compatibility.

pub mod bytecode;
pub mod compiler;
pub mod debug;
pub mod interpreter;
