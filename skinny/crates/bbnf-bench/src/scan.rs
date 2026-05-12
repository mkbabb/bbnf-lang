pub fn structural_offsets_scalar(bytes: &[u8]) -> Vec<u32> {
    simd_scan::scan_scalar(bytes, &simd_scan::JSON_STRUCTURAL).into_positions()
}

pub fn structural_offsets_simd(bytes: &[u8]) -> Vec<u32> {
    simd_scan::scan_json_structurals(bytes).into_positions()
}

pub fn parity_hash(bytes: &[u8]) -> String {
    let report = simd_scan::scalar_parity_report(bytes, &simd_scan::JSON_STRUCTURAL);
    blake3::hash(&report.hash).to_hex().to_string()
}

pub fn hash_offsets(offsets: &[u32]) -> String {
    let mut bytes = Vec::with_capacity(offsets.len() * std::mem::size_of::<u32>());
    for offset in offsets {
        bytes.extend_from_slice(&offset.to_le_bytes());
    }
    blake3::hash(&bytes).to_hex().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scalar_scan_reports_json_structurals() {
        let offsets = structural_offsets_scalar(br#"{"a":"[,]"}"#);
        assert_eq!(offsets, vec![0, 1, 3, 4, 5, 9, 10]);
    }

    #[test]
    fn simd_hash_matches_scalar_hash() {
        let bytes = br#"{"a":[1,2,3]}"#;
        let scalar = structural_offsets_scalar(bytes);
        let simd = structural_offsets_simd(bytes);
        assert_eq!(hash_offsets(&scalar), hash_offsets(&simd));
    }
}
