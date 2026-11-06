use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::{Type, VerticalError};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ClosedRow(pub BTreeMap<String, Type>);

impl ClosedRow {
    pub fn unify(&self, other: &Self) -> Result<Self, VerticalError> {
        if self == other {
            return Ok(self.clone());
        }
        let conflict = self
            .0
            .iter()
            .find(|(name, ty)| other.0.get(*name).is_some_and(|other_ty| other_ty != *ty))
            .map(|(name, _)| name.as_str())
            .unwrap_or("row labels");
        Err(VerticalError::Type(format!(
            "incompatible closed row at {conflict}"
        )))
    }
}
