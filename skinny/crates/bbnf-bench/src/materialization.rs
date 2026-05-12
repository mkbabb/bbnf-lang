use runtime::{
    grammars::json::{JsonNodeKind, JsonRoot},
    tape::{TapeToken, TokenFlags},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaterializationStats {
    pub input_bytes: usize,
    pub token_count: usize,
    pub tape_bytes: usize,
    pub tape_capacity_bytes: usize,
    pub payload_bytes: usize,
    pub root_tokens: usize,
    pub pair_tokens: usize,
    pub container_open_tokens: usize,
    pub structural_close_tokens: usize,
    pub scalar_tokens: usize,
    pub sibling_skip_tokens: usize,
}

impl MaterializationStats {
    pub fn from_root(root: &JsonRoot<'_>) -> Self {
        let mut stats = Self {
            input_bytes: root.source().len(),
            token_count: root.tokens().len(),
            tape_bytes: root.tokens().len() * std::mem::size_of::<TapeToken>(),
            tape_capacity_bytes: root.tape().token_capacity() * std::mem::size_of::<TapeToken>(),
            payload_bytes: root.tape().payloads().len(),
            root_tokens: 0,
            pair_tokens: 0,
            container_open_tokens: 0,
            structural_close_tokens: 0,
            scalar_tokens: 0,
            sibling_skip_tokens: 0,
        };

        for token in root.tokens() {
            if token.flags.contains(TokenFlags::IS_STRUCTURAL_CLOSE) {
                stats.structural_close_tokens += 1;
            }
            if token.flags.payload_class() == TokenFlags::SIBLING_SKIP {
                stats.sibling_skip_tokens += 1;
            }

            match JsonNodeKind::from_id(token.kind).expect("JSON parser emits JSON node kinds") {
                JsonNodeKind::Root => stats.root_tokens += 1,
                JsonNodeKind::ObjectOpen | JsonNodeKind::ArrayOpen => {
                    stats.container_open_tokens += 1;
                }
                JsonNodeKind::ObjectClose | JsonNodeKind::ArrayClose => {}
                JsonNodeKind::Pair => stats.pair_tokens += 1,
                JsonNodeKind::String
                | JsonNodeKind::Number
                | JsonNodeKind::True
                | JsonNodeKind::False
                | JsonNodeKind::Null => stats.scalar_tokens += 1,
            }
        }

        stats
    }

    pub fn tape_bytes_per_input_byte(&self) -> f64 {
        if self.input_bytes == 0 {
            0.0
        } else {
            self.tape_bytes as f64 / self.input_bytes as f64
        }
    }

    pub fn tape_capacity_bytes_per_input_byte(&self) -> f64 {
        if self.input_bytes == 0 {
            0.0
        } else {
            self.tape_capacity_bytes as f64 / self.input_bytes as f64
        }
    }

    pub fn summary(&self, corpus: &str) -> String {
        format!(
            "{corpus} tape materialization: {} tokens, {} logical tape bytes ({:.2}x input), {} allocated tape bytes ({:.2}x input), {} payload bytes; pairs {}, opens {}, closes {}, scalars {}, sibling-skips {}.",
            self.token_count,
            self.tape_bytes,
            self.tape_bytes_per_input_byte(),
            self.tape_capacity_bytes,
            self.tape_capacity_bytes_per_input_byte(),
            self.payload_bytes,
            self.pair_tokens,
            self.container_open_tokens,
            self.structural_close_tokens,
            self.scalar_tokens,
            self.sibling_skip_tokens
        )
    }
}

pub fn track_stats(input: &str) -> Option<(MaterializationStats, MaterializationStats)> {
    let track1 = runtime::generated_json::parse(input).ok()?;
    let track2 = crate::track2::json::parse(input).ok()?;
    Some((
        MaterializationStats::from_root(&track1),
        MaterializationStats::from_root(&track2),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_json_tape_materialization_shape() {
        let root = runtime::generated_json::parse(r#"{"a":[1,true,null]}"#).unwrap();
        let stats = MaterializationStats::from_root(&root);
        assert_eq!(stats.root_tokens, 1);
        assert_eq!(stats.pair_tokens, 1);
        assert_eq!(stats.container_open_tokens, 2);
        assert_eq!(stats.structural_close_tokens, 0);
        assert_eq!(stats.scalar_tokens, 4);
        assert_eq!(stats.payload_bytes, 0);
        assert_eq!(stats.tape_bytes, stats.token_count * 16);
        assert!(stats.tape_capacity_bytes >= stats.tape_bytes);
    }
}
