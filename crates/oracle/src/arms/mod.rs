//! The arms G1 runs (LW3.COMP-3a). The spine and the reference interpreter
//! join at LW3.COMP-1c and LW3.COMP-3b.

pub mod mutant;
pub mod root;
pub mod ts_package;

use crate::arm::Arm;

/// An arm by its `--arms` name.
pub fn by_name(name: &str) -> Option<Box<dyn Arm>> {
    match name {
        "root" => Some(Box::new(root::Root)),
        "ts-package" => Some(Box::new(ts_package::TsPackage::default())),
        #[cfg(feature = "mutants")]
        "mutant-const" => Some(Box::new(mutant::Constant)),
        _ => None,
    }
}
