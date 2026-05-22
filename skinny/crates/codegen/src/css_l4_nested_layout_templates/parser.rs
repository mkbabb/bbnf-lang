use super::generated;
use super::sink::CssFactError;

pub fn parse(input: &str) -> Result<String, CssFactError> {
    generated::emit_fact_stream(input)
}

pub fn parse_bytes(input: &[u8]) -> Result<String, CssFactError> {
    let input = std::str::from_utf8(input).map_err(|error| CssFactError {
        offset: error.valid_up_to(),
        message: "invalid UTF-8",
    })?;
    parse(input)
}
