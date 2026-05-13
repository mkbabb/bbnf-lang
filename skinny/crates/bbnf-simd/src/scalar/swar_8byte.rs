use crate::classifier::ClassifyResult;

pub fn classify_chunk(bytes: &[u8; 64], alphabet: &[u8; 64]) -> ClassifyResult {
    let mut result = ClassifyResult::default();
    for (index, byte) in bytes.iter().copied().enumerate() {
        let bit = 1u64 << index;
        if crate::alphabet_contains(alphabet, byte) {
            result.structural_mask |= bit;
        }
        match byte {
            b'"' => result.quote_mask |= bit,
            b'\\' => result.backslash_mask |= bit,
            0x00..=0x1f => result.control_mask |= bit,
            _ => {}
        }
    }
    result
}
