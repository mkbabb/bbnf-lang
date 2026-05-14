pub fn structural_offsets_scalar(bytes: &[u8]) -> Vec<u32> {
    runtime::grammars::json::scan::scan_structurals_scalar(bytes).into_positions()
}

pub fn structural_offsets_simd(bytes: &[u8]) -> Vec<u32> {
    runtime::grammars::json::scan::scan_structurals(bytes).into_positions()
}

pub fn parity_hash(bytes: &[u8]) -> String {
    let report = runtime::grammars::json::scan::scalar_parity_report(bytes);
    blake3::hash(&report.hash).to_hex().to_string()
}

pub fn hash_offsets(offsets: &[u32]) -> String {
    let bytes: Vec<_> = offsets
        .iter()
        .flat_map(|offset| offset.to_le_bytes())
        .collect();
    blake3::hash(&bytes).to_hex().to_string()
}
