pub(crate) mod config;
pub mod generated;
pub mod parser;
pub mod sink;

pub use parser::{parse, parse_bytes};
pub use sink::CssFactError;
