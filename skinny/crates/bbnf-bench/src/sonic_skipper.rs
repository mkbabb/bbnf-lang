use serde::Deserialize;

pub fn parse_only(bytes: &[u8]) -> sonic_rs::Result<()> {
    let mut deserializer = sonic_rs::Deserializer::from_slice(bytes);
    serde::de::IgnoredAny::deserialize(&mut deserializer)?;
    deserializer.end()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn skipper_accepts_complete_json_and_rejects_trailing_bytes() {
        parse_only(br#"{"a":[1,true,null]}"#).unwrap();
        assert!(parse_only(br#"{"a":1} trailing"#).is_err());
    }
}
