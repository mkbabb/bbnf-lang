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
    for required in [
        "JsonObject",
        "JsonArray",
        "JsonPair",
        "JsonString",
        "JsonNumber",
        "JsonBool",
        "JsonNull",
    ] {
        if !sink_only.has_shape(required) {
            return Err(format!(
                "typed DirectBuild requires sink-only shape `{required}`"
            ));
        }
    }
    if !sink_only.has_literal(b"true")
        || !sink_only.has_literal(b"false")
        || !sink_only.has_literal(b"null")
    {
        return Err("typed DirectBuild requires JSON literal recognizers".to_string());
    }
    let direct_shape_roster = sink_only.direct_shapes.iter().cloned().collect();
    Ok(TypedDirectProgram {
        schema: schema.clone(),
        direct_shape_roster,
    })
}
