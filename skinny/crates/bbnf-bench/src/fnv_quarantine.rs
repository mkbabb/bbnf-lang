pub const SKV15_FNV_QUARANTINE_SCHEMA: &str = "sk-v15-fnv-quarantine-v1";
pub const SKV15_FNV_QUARANTINE_WAVE_ID: &str = "SK-V15-W10";
pub const SKV15_FNV_QUARANTINE_DEP_ROW: &str = "DEP-W10-FNV-QUARANTINE";

pub const CLOSED_ENUM_ROWS: &[&str] = &[
    "json/y_string_unicode/direct_to_struct/main",
    "json/y_string_unicode/real_typed_struct/main",
    "json/unicode_mixed/direct_to_struct/main",
    "json/unicode_mixed/real_typed_struct/main",
    "json/gsoc-2018/direct_to_struct/main",
    "json/gsoc-2018/real_typed_struct/main",
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SidecarDomain {
    IndependentTypedSemantic,
    SharedClosedEnum,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StrictProductWitness<'a> {
    pub row_id: &'a str,
    pub track1_typed_checksum: u64,
    pub track2_typed_checksum: u64,
    pub serde_typed_checksum: u64,
    pub sonic_typed_checksum: u64,
    pub track1_fnv64: u64,
    pub track2_fnv64: u64,
    pub serde_fnv64: u64,
    pub sonic_fnv64: u64,
    pub sidecar_domain: SidecarDomain,
}

pub fn validate_strict_product_witness(witness: &StrictProductWitness<'_>) -> Result<(), String> {
    if !CLOSED_ENUM_ROWS.contains(&witness.row_id) {
        return Err(format!(
            "{} is not a W11L/W11N/W11O closed-enum row",
            witness.row_id
        ));
    }
    if witness.sidecar_domain == SidecarDomain::SharedClosedEnum {
        return Err(format!(
            "{} uses shared closed-enum sidecar coupling",
            witness.row_id
        ));
    }
    let typed = [
        witness.track1_typed_checksum,
        witness.track2_typed_checksum,
        witness.serde_typed_checksum,
        witness.sonic_typed_checksum,
    ];
    if typed.iter().any(|checksum| *checksum != typed[0]) {
        return Err(format!(
            "{} typed semantic mismatch despite FNV metadata track1={:016x}",
            witness.row_id, witness.track1_fnv64
        ));
    }
    let fnv = [
        witness.track1_fnv64,
        witness.track2_fnv64,
        witness.serde_fnv64,
        witness.sonic_fnv64,
    ];
    if fnv.iter().any(|checksum| *checksum != fnv[0]) {
        return Err(format!("{} FNV metadata mismatch", witness.row_id));
    }
    Ok(())
}

pub fn validate_quarantine_witnesses(witnesses: &[StrictProductWitness<'_>]) -> Result<(), String> {
    if witnesses.len() != CLOSED_ENUM_ROWS.len() {
        return Err(format!(
            "expected {} W10 quarantine witnesses, saw {}",
            CLOSED_ENUM_ROWS.len(),
            witnesses.len()
        ));
    }
    for row in CLOSED_ENUM_ROWS {
        if !witnesses.iter().any(|witness| witness.row_id == *row) {
            return Err(format!("missing W10 quarantine witness for {row}"));
        }
    }
    for witness in witnesses {
        validate_strict_product_witness(witness)?;
    }
    Ok(())
}

pub fn bench_only_quarantine_witnesses() -> Vec<StrictProductWitness<'static>> {
    CLOSED_ENUM_ROWS
        .iter()
        .enumerate()
        .map(|(index, row_id)| {
            let checksum = 0x5154_0000_0000_0000u64 | index as u64;
            let fnv64 = 0xF17E_0000_0000_0000u64 | index as u64;
            StrictProductWitness {
                row_id: *row_id,
                track1_typed_checksum: checksum,
                track2_typed_checksum: checksum,
                serde_typed_checksum: checksum,
                sonic_typed_checksum: checksum,
                track1_fnv64: fnv64,
                track2_fnv64: fnv64,
                serde_fnv64: fnv64,
                sonic_fnv64: fnv64,
                sidecar_domain: SidecarDomain::IndependentTypedSemantic,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fnv_quarantine_rejects_matching_hash_with_mismatched_typed_semantics() {
        let witness = StrictProductWitness {
            row_id: "json/y_string_unicode/real_typed_struct/main",
            track1_typed_checksum: 10,
            track2_typed_checksum: 11,
            serde_typed_checksum: 10,
            sonic_typed_checksum: 10,
            track1_fnv64: 0xfeed,
            track2_fnv64: 0xfeed,
            serde_fnv64: 0xfeed,
            sonic_fnv64: 0xfeed,
            sidecar_domain: SidecarDomain::IndependentTypedSemantic,
        };

        let error = validate_strict_product_witness(&witness).unwrap_err();
        assert!(error.contains("typed semantic mismatch"));
    }

    #[test]
    fn fnv_quarantine_rejects_shared_closed_enum_sidecar() {
        let witness = StrictProductWitness {
            row_id: "json/y_string_unicode/real_typed_struct/main",
            track1_typed_checksum: 10,
            track2_typed_checksum: 10,
            serde_typed_checksum: 10,
            sonic_typed_checksum: 10,
            track1_fnv64: 0xfeed,
            track2_fnv64: 0xfeed,
            serde_fnv64: 0xfeed,
            sonic_fnv64: 0xfeed,
            sidecar_domain: SidecarDomain::SharedClosedEnum,
        };

        let error = validate_strict_product_witness(&witness).unwrap_err();
        assert!(error.contains("shared closed-enum"));
    }

    #[test]
    fn fnv_quarantine_report_accepts_bench_only_metadata() {
        let witnesses = bench_only_quarantine_witnesses();

        validate_quarantine_witnesses(&witnesses).unwrap();
        assert_eq!(witnesses.len(), CLOSED_ENUM_ROWS.len());
        assert!(witnesses
            .iter()
            .all(|witness| { witness.sidecar_domain == SidecarDomain::IndependentTypedSemantic }));
    }
}
