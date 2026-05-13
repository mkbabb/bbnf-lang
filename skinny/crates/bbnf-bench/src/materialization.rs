use runtime::grammars::json::{JsonNodeKind, JsonRoot};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaterializationStats {
    pub input_bytes: usize,
    pub offset_count: usize,
    pub offset_bytes: usize,
    pub flag_bytes: usize,
    pub offset_capacity_bytes: usize,
    pub payload_bytes: usize,
    pub object_open_offsets: usize,
    pub array_open_offsets: usize,
    pub close_offsets: usize,
    pub string_quote_offsets: usize,
    pub number_offsets: usize,
    pub literal_offsets: usize,
    pub separator_offsets: usize,
}

impl MaterializationStats {
    pub fn from_root(root: &JsonRoot<'_>) -> Self {
        let mut stats = Self {
            input_bytes: root.source().len(),
            offset_count: root.tape().offsets().len(),
            offset_bytes: root.tape().offset_bytes(),
            flag_bytes: root.tape().flag_bytes(),
            offset_capacity_bytes: root.tape().offset_capacity_bytes(),
            payload_bytes: root.tape().payloads().len(),
            object_open_offsets: 0,
            array_open_offsets: 0,
            close_offsets: 0,
            string_quote_offsets: 0,
            number_offsets: 0,
            literal_offsets: 0,
            separator_offsets: 0,
        };

        for cursor in 0..root.tape().offsets().len() as u32 {
            match JsonNodeKind::at_cursor(root.tape(), cursor) {
                JsonNodeKind::ObjectOpen => stats.object_open_offsets += 1,
                JsonNodeKind::ArrayOpen => stats.array_open_offsets += 1,
                JsonNodeKind::ObjectClose | JsonNodeKind::ArrayClose => stats.close_offsets += 1,
                JsonNodeKind::String => stats.string_quote_offsets += 1,
                JsonNodeKind::Number => stats.number_offsets += 1,
                JsonNodeKind::True | JsonNodeKind::False | JsonNodeKind::Null => {
                    stats.literal_offsets += 1;
                }
                JsonNodeKind::ElementSeparator | JsonNodeKind::MemberSeparator => {
                    stats.separator_offsets += 1;
                }
                JsonNodeKind::Root | JsonNodeKind::Pair => {}
            }
        }

        stats
    }

    pub fn offset_bytes_per_input_byte(&self) -> f64 {
        if self.input_bytes == 0 {
            0.0
        } else {
            self.offset_bytes as f64 / self.input_bytes as f64
        }
    }

    pub fn offset_capacity_bytes_per_input_byte(&self) -> f64 {
        if self.input_bytes == 0 {
            0.0
        } else {
            self.offset_capacity_bytes as f64 / self.input_bytes as f64
        }
    }

    pub fn logical_tape_bytes_per_input_byte(&self) -> f64 {
        if self.input_bytes == 0 {
            0.0
        } else {
            (self.offset_bytes + self.flag_bytes) as f64 / self.input_bytes as f64
        }
    }

    pub fn summary(&self, corpus: &str) -> String {
        format!(
            "{corpus} lazy tape materialization: {} offsets, {} logical offset bytes + {} sparse flag bytes ({:.2}x input), {} allocated tape bytes ({:.2}x input), {} payload bytes; object opens {}, array opens {}, closes {}, string quotes {}, numbers {}, literals {}, separators {}.",
            self.offset_count,
            self.offset_bytes,
            self.flag_bytes,
            self.logical_tape_bytes_per_input_byte(),
            self.offset_capacity_bytes,
            self.offset_capacity_bytes_per_input_byte(),
            self.payload_bytes,
            self.object_open_offsets,
            self.array_open_offsets,
            self.close_offsets,
            self.string_quote_offsets,
            self.number_offsets,
            self.literal_offsets,
            self.separator_offsets,
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
    fn counts_json_lazy_tape_materialization_shape() {
        let root = runtime::generated_json::parse(r#"{"a":[1,true,null]}"#).unwrap();
        let stats = MaterializationStats::from_root(&root);
        assert_eq!(stats.object_open_offsets, 1);
        assert_eq!(stats.array_open_offsets, 1);
        assert_eq!(stats.close_offsets, 2);
        assert_eq!(stats.number_offsets, 1);
        assert_eq!(stats.literal_offsets, 2);
        assert_eq!(stats.payload_bytes, 0);
        assert_eq!(stats.offset_bytes, stats.offset_count * 4);
        assert_eq!(stats.flag_bytes, 0);
        assert!(stats.offset_capacity_bytes >= stats.offset_bytes + stats.flag_bytes);
    }
}
