use crate::direct_schema::DirectSchemaSet;

use super::sink_only::SinkOnlyProgram;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedDirectProgram {
    pub schema: DirectSchemaSet,
    pub direct_shape_roster: Vec<String>,
}

pub fn lower_program(
    sink_only: &SinkOnlyProgram,
    schema: &DirectSchemaSet,
) -> Result<TypedDirectProgram, String> {
    schema.validate()?;
    if sink_only.direct_shapes.is_empty() {
        return Err("typed DirectBuild requires sink-only DirectBuild shapes".to_string());
    }
    if sink_only.literals.is_empty() {
        return Err("typed DirectBuild requires literal recognizers".to_string());
    }
    let direct_shape_roster = sink_only.direct_shapes.iter().cloned().collect();
    Ok(TypedDirectProgram {
        schema: schema.clone(),
        direct_shape_roster,
    })
}
