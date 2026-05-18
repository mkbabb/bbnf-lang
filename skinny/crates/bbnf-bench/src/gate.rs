use crate::metadata::{RowMetadata, TrackTag};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Outcome {
    ABeatAndParity,
    BBeatSubstrateParityCodegen,
    CSubstrateParityCodegenAcceptable,
    DSubstrateParityCodegenGap,
    ESubstrateParityCodegenFailure,
    FPositive,
    FNoise,
    GSubstrateFailure,
    IParityOracleFail,
    JSchemaFail,
    KSimdParityHashFail,
    LSimdThroughputFail,
    MMemoryResidencyFail,
    NDirectProjectionFailure,
    SSubstrateGuardNonAdmission,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    Go,
    GoWithFocus,
    Conditional,
    Invalid,
    NoGo,
}

#[derive(Debug, Clone)]
pub struct ThresholdInput {
    pub schema_ok: bool,
    pub parity_ok: bool,
    pub simd_parity_ok: bool,
    pub simd_canada_gbps: Option<f64>,
    pub simd_floor_gbps: f64,
    pub track1_ns: f64,
    pub track2_ns: f64,
    pub sonic_rs_anchor_ns: Option<f64>,
    pub simd_json_borrowed_ns: Option<f64>,
    pub simd_json_owned_ns: Option<f64>,
    pub readme_target_ns: f64,
    pub fastest_competitor_peak_rss: Option<u64>,
    pub bbnf_peak_rss: Option<u64>,
}

#[derive(Debug, Clone)]
pub struct DirectProjectionInput {
    pub correctness_ok: bool,
    pub track1_ns: Option<f64>,
    pub track2_ns: Option<f64>,
    pub sonic_rs_ns: Option<f64>,
}

pub const DIRECT_PROJECTION_SONIC_SLACK: f64 = 1.10;

#[derive(Debug, Clone, Copy)]
pub struct StrictAdmissionEvidence<'a> {
    pub outcome_id: &'a str,
    pub row_strictness: &'a str,
    pub parse_utf8: &'a str,
    pub escape_complete: &'a str,
    pub row_output_plane: &'a str,
    pub comparator_plane: &'a str,
    pub comparator_strictness: &'a str,
    pub comparator_freshness: &'a str,
    pub sidecar_freshness: &'a str,
    pub measured_validation_path: &'a str,
}

impl Outcome {
    pub fn verdict(self) -> Verdict {
        match self {
            Outcome::ABeatAndParity
            | Outcome::BBeatSubstrateParityCodegen
            | Outcome::CSubstrateParityCodegenAcceptable => Verdict::Go,
            Outcome::DSubstrateParityCodegenGap => Verdict::GoWithFocus,
            Outcome::ESubstrateParityCodegenFailure | Outcome::FPositive | Outcome::FNoise => {
                Verdict::Conditional
            }
            Outcome::JSchemaFail => Verdict::Invalid,
            Outcome::GSubstrateFailure
            | Outcome::IParityOracleFail
            | Outcome::KSimdParityHashFail
            | Outcome::LSimdThroughputFail
            | Outcome::MMemoryResidencyFail
            | Outcome::NDirectProjectionFailure
            | Outcome::SSubstrateGuardNonAdmission => Verdict::NoGo,
        }
    }

    pub fn id(self) -> &'static str {
        match self {
            Outcome::ABeatAndParity => "A",
            Outcome::BBeatSubstrateParityCodegen => "B",
            Outcome::CSubstrateParityCodegenAcceptable => "C",
            Outcome::DSubstrateParityCodegenGap => "D",
            Outcome::ESubstrateParityCodegenFailure => "E",
            Outcome::FPositive => "F-positive",
            Outcome::FNoise => "F-noise",
            Outcome::GSubstrateFailure => "G",
            Outcome::IParityOracleFail => "I",
            Outcome::JSchemaFail => "J",
            Outcome::KSimdParityHashFail => "K",
            Outcome::LSimdThroughputFail => "L",
            Outcome::MMemoryResidencyFail => "M",
            Outcome::NDirectProjectionFailure => "N-direct",
            Outcome::SSubstrateGuardNonAdmission => "S",
        }
    }
}

pub fn parse_outcome_id(value: &str) -> Option<Outcome> {
    Some(match value {
        "A" => Outcome::ABeatAndParity,
        "B" => Outcome::BBeatSubstrateParityCodegen,
        "C" => Outcome::CSubstrateParityCodegenAcceptable,
        "D" => Outcome::DSubstrateParityCodegenGap,
        "E" => Outcome::ESubstrateParityCodegenFailure,
        "F-positive" => Outcome::FPositive,
        "F-noise" => Outcome::FNoise,
        "G" => Outcome::GSubstrateFailure,
        "I" => Outcome::IParityOracleFail,
        "J" => Outcome::JSchemaFail,
        "K" => Outcome::KSimdParityHashFail,
        "L" => Outcome::LSimdThroughputFail,
        "M" => Outcome::MMemoryResidencyFail,
        "N-direct" => Outcome::NDirectProjectionFailure,
        "S" => Outcome::SSubstrateGuardNonAdmission,
        _ => return None,
    })
}

pub fn validate_strict_admission(evidence: &StrictAdmissionEvidence<'_>) -> Result<(), String> {
    let Some(outcome) = parse_outcome_id(evidence.outcome_id) else {
        return Err(format!("unsupported outcome {}", evidence.outcome_id));
    };
    if matches!(outcome, Outcome::KSimdParityHashFail) || evidence.outcome_id == "S" {
        return Err(format!(
            "{} is not strict-admission eligible",
            evidence.outcome_id
        ));
    }
    if evidence.row_strictness != "strict" {
        return Err("row strictness is not strict".to_string());
    }
    if evidence.comparator_strictness != "strict" {
        return Err("comparator strictness is not strict".to_string());
    }
    if evidence.parse_utf8 != "measured-row" {
        return Err("UTF-8 validation is not measured-row".to_string());
    }
    if evidence.escape_complete != "yes" {
        return Err("escape validation is incomplete".to_string());
    }
    if normalize_plane(evidence.row_output_plane) != normalize_plane(evidence.comparator_plane) {
        return Err("row/comparator output plane mismatch".to_string());
    }
    if evidence.measured_validation_path != "measured-row" {
        return Err("validation path is not measured-row".to_string());
    }
    if evidence.comparator_freshness.starts_with("stale:")
        || evidence.comparator_freshness.starts_with("historical:")
        || evidence.comparator_freshness.starts_with("absent:")
        || evidence.sidecar_freshness.starts_with("stale:")
        || evidence.sidecar_freshness.starts_with("historical:")
        || evidence.sidecar_freshness.starts_with("absent:")
    {
        return Err("comparator freshness is not same-run strict evidence".to_string());
    }
    if evidence.comparator_freshness != "same-run-native" || evidence.sidecar_freshness != "n/a" {
        return Err("comparator freshness is unsupported for strict admission".to_string());
    }
    Ok(())
}

pub fn validate_schema(rows: &[RowMetadata]) -> bool {
    !rows.is_empty()
        && rows.iter().all(RowMetadata::required_fields_present)
        && rows.iter().all(|row| match row.track {
            TrackTag::Track1Generated | TrackTag::Track2Handcoded => {
                row.arena_writes == Some(0) && row.payload_allocations == Some(0)
            }
            TrackTag::Competitor => {
                row.competitor_crate
                    .as_deref()
                    .is_some_and(|value| !value.is_empty())
                    && row
                        .competitor_version
                        .as_deref()
                        .is_some_and(|value| !value.is_empty())
            }
            TrackTag::SimdScan => row.has_scalar_parity_hash(),
            _ => true,
        })
}

fn normalize_plane(value: &str) -> String {
    value
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_ascii_lowercase()
}

pub fn classify(input: &ThresholdInput) -> Outcome {
    if !input.schema_ok {
        return Outcome::JSchemaFail;
    }
    if !input.parity_ok {
        return Outcome::IParityOracleFail;
    }
    if !input.simd_parity_ok {
        return Outcome::KSimdParityHashFail;
    }
    if input
        .simd_canada_gbps
        .is_some_and(|gbps| gbps < input.simd_floor_gbps)
    {
        return Outcome::LSimdThroughputFail;
    }
    if memory_floor_failed(input.fastest_competitor_peak_rss, input.bbnf_peak_rss) {
        return Outcome::MMemoryResidencyFail;
    }

    let Some(s) = fastest_anchor(input) else {
        return Outcome::JSchemaFail;
    };
    let beat_bound = (s * 0.95).min(input.readme_target_ns);
    let track2 = input.track2_ns;
    let track1 = input.track1_ns;

    if track2 > s * 1.10 {
        return Outcome::GSubstrateFailure;
    }
    if track2 > s * 1.05 {
        if track1 <= track2 * 1.05 {
            return Outcome::FPositive;
        }
        return Outcome::FNoise;
    }
    if track1 > track2 * 1.50 {
        return Outcome::ESubstrateParityCodegenFailure;
    }
    if track1 > track2 * 1.15 {
        return Outcome::DSubstrateParityCodegenGap;
    }
    if track2 <= beat_bound && track1 <= track2 * 1.10 {
        return Outcome::ABeatAndParity;
    }
    if track2 <= beat_bound {
        return Outcome::BBeatSubstrateParityCodegen;
    }
    Outcome::CSubstrateParityCodegenAcceptable
}

pub fn classify_direct_projection(input: &DirectProjectionInput) -> Option<Outcome> {
    if !input.correctness_ok {
        return Some(Outcome::IParityOracleFail);
    }
    let (Some(track1), Some(track2), Some(sonic)) =
        (input.track1_ns, input.track2_ns, input.sonic_rs_ns)
    else {
        return None;
    };
    if track1 > sonic * DIRECT_PROJECTION_SONIC_SLACK
        || track2 > sonic * DIRECT_PROJECTION_SONIC_SLACK
    {
        return Some(Outcome::NDirectProjectionFailure);
    }
    None
}

pub fn worst_outcome(outcomes: impl IntoIterator<Item = Outcome>) -> Option<Outcome> {
    outcomes
        .into_iter()
        .max_by_key(|outcome| severity(*outcome))
}

fn fastest_anchor(input: &ThresholdInput) -> Option<f64> {
    [
        input.sonic_rs_anchor_ns,
        input.simd_json_borrowed_ns,
        input.simd_json_owned_ns,
    ]
    .into_iter()
    .flatten()
    .filter(|value| value.is_finite() && *value > 0.0)
    .min_by(|a, b| a.total_cmp(b))
}

fn memory_floor_failed(
    fastest_competitor_peak_rss: Option<u64>,
    bbnf_peak_rss: Option<u64>,
) -> bool {
    match (fastest_competitor_peak_rss, bbnf_peak_rss) {
        (Some(competitor), Some(bbnf)) if competitor > 0 => bbnf > competitor.saturating_mul(3),
        _ => false,
    }
}

fn severity(outcome: Outcome) -> u8 {
    match outcome {
        Outcome::ABeatAndParity => 0,
        Outcome::BBeatSubstrateParityCodegen => 1,
        Outcome::CSubstrateParityCodegenAcceptable => 2,
        Outcome::DSubstrateParityCodegenGap => 3,
        Outcome::ESubstrateParityCodegenFailure => 4,
        Outcome::FPositive => 5,
        Outcome::FNoise => 6,
        Outcome::JSchemaFail => 7,
        Outcome::IParityOracleFail => 8,
        Outcome::KSimdParityHashFail => 9,
        Outcome::LSimdThroughputFail => 10,
        Outcome::MMemoryResidencyFail => 11,
        Outcome::GSubstrateFailure => 12,
        Outcome::SSubstrateGuardNonAdmission => 13,
        Outcome::NDirectProjectionFailure => 14,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base() -> ThresholdInput {
        ThresholdInput {
            schema_ok: true,
            parity_ok: true,
            simd_parity_ok: true,
            simd_canada_gbps: Some(6.0),
            simd_floor_gbps: 5.0,
            track1_ns: 390.0,
            track2_ns: 360.0,
            sonic_rs_anchor_ns: Some(436.0),
            simd_json_borrowed_ns: Some(424.0),
            simd_json_owned_ns: Some(450.0),
            readme_target_ns: 380.0,
            fastest_competitor_peak_rss: None,
            bbnf_peak_rss: None,
        }
    }

    #[test]
    fn classifies_beat_and_parity_before_broader_parity() {
        assert_eq!(classify(&base()), Outcome::ABeatAndParity);
    }

    #[test]
    fn correctness_gates_precede_throughput() {
        let mut input = base();
        input.parity_ok = false;
        input.track2_ns = 10_000.0;
        assert_eq!(classify(&input), Outcome::IParityOracleFail);
    }

    #[test]
    fn classifies_codegen_gap_when_substrate_is_parity() {
        let mut input = base();
        input.track2_ns = 430.0;
        input.track1_ns = 600.0;
        assert_eq!(classify(&input), Outcome::DSubstrateParityCodegenGap);
    }

    #[test]
    fn memory_floor_is_blocking() {
        let mut input = base();
        input.fastest_competitor_peak_rss = Some(100);
        input.bbnf_peak_rss = Some(301);
        assert_eq!(classify(&input), Outcome::MMemoryResidencyFail);
    }

    #[test]
    fn simd_json_can_be_fastest_anchor() {
        let mut input = base();
        input.sonic_rs_anchor_ns = Some(500.0);
        input.simd_json_borrowed_ns = Some(300.0);
        input.simd_json_owned_ns = Some(420.0);
        input.readme_target_ns = f64::INFINITY;
        input.track2_ns = 284.0;
        input.track1_ns = 312.0;
        assert_eq!(classify(&input), Outcome::ABeatAndParity);
    }

    #[test]
    fn schema_rejects_simd_scan_without_hash() {
        let host = crate::metadata::HostFacts {
            cpu_model: "cpu".into(),
            cpu_arch: "arch".into(),
            os_kernel: "os".into(),
            rustflags: String::new(),
            target_cpu: "default".into(),
            bbnf_commit: "commit".into(),
        };
        let mut row = RowMetadata::from_bench(
            &host,
            crate::metadata::BenchFacts::simd_scan(
                "other",
                "a".repeat(64),
                12,
                "hash".into(),
                5.0,
                100,
            ),
        );
        row.scalar_parity_hash_twitter = None;
        assert!(!validate_schema(&[row]));
    }

    #[test]
    fn direct_projection_failure_is_blocking() {
        let input = DirectProjectionInput {
            correctness_ok: true,
            track1_ns: Some(500.0),
            track2_ns: Some(450.0),
            sonic_rs_ns: Some(300.0),
        };
        assert_eq!(
            classify_direct_projection(&input),
            Some(Outcome::NDirectProjectionFailure)
        );
    }

    #[test]
    fn direct_projection_passes_within_sonic_slack() {
        let input = DirectProjectionInput {
            correctness_ok: true,
            track1_ns: Some(329.0),
            track2_ns: Some(330.0),
            sonic_rs_ns: Some(300.0),
        };
        assert_eq!(classify_direct_projection(&input), None);
    }

    fn strict_evidence() -> StrictAdmissionEvidence<'static> {
        StrictAdmissionEvidence {
            outcome_id: "A",
            row_strictness: "strict",
            parse_utf8: "measured-row",
            escape_complete: "yes",
            row_output_plane: "digest",
            comparator_plane: "digest",
            comparator_strictness: "strict",
            comparator_freshness: "same-run-native",
            sidecar_freshness: "n/a",
            measured_validation_path: "measured-row",
        }
    }

    #[test]
    fn rejects_unsupported_outcome_id() {
        assert!(parse_outcome_id("Q").is_none());
        let mut evidence = strict_evidence();
        evidence.outcome_id = "Q";
        assert!(validate_strict_admission(&evidence).is_err());
    }

    #[test]
    fn rejects_k_or_reserved_s_as_strict_admission() {
        let mut evidence = strict_evidence();
        evidence.outcome_id = "K";
        assert!(validate_strict_admission(&evidence).is_err());
        evidence.outcome_id = "S";
        assert!(validate_strict_admission(&evidence).is_err());
    }

    #[test]
    fn rejects_deferred_view_boundary_strict_claim() {
        let mut evidence = strict_evidence();
        evidence.row_strictness = "deferred";
        assert!(validate_strict_admission(&evidence).is_err());
        evidence = strict_evidence();
        evidence.measured_validation_path = "view-boundary";
        assert!(validate_strict_admission(&evidence).is_err());
        evidence = strict_evidence();
        evidence.parse_utf8 = "view-boundary";
        assert!(validate_strict_admission(&evidence).is_err());
        evidence = strict_evidence();
        evidence.escape_complete = "no";
        assert!(validate_strict_admission(&evidence).is_err());
    }

    #[test]
    fn rejects_strict_plane_mismatch_and_stale_sidecar() {
        let mut evidence = strict_evidence();
        evidence.comparator_plane = "DOM";
        assert!(validate_strict_admission(&evidence).is_err());
        evidence = strict_evidence();
        evidence.comparator_freshness = "historical:sk-v7-sidecar";
        evidence.sidecar_freshness = "historical:sk-v7-sidecar";
        assert!(validate_strict_admission(&evidence).is_err());
    }

    #[test]
    fn rejects_sidecar_same_run_without_structured_manifest() {
        let mut evidence = strict_evidence();
        evidence.comparator_freshness = "sidecar-same-run";
        evidence.sidecar_freshness = "sidecar-same-run";
        assert!(validate_strict_admission(&evidence).is_err());
    }
}
