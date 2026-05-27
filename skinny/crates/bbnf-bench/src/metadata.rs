use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;

pub const SCHEMA_VERSION: &str = "3";

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum TrackTag {
    Track1Generated,
    Track2Handcoded,
    Competitor,
    SimdScan,
    Probe,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RowMetadata {
    pub schema_version: String,
    pub cpu_model: String,
    pub cpu_arch: String,
    pub os_kernel: String,
    pub rustflags: String,
    pub target_cpu: String,
    pub profile: String,
    pub input_sha256: String,
    pub input_bytes: u64,
    pub competitor_crate: Option<String>,
    pub competitor_version: Option<String>,
    pub bbnf_commit: String,
    pub warmup_samples: u32,
    pub warmup_time_s: f64,
    pub sample_size: u32,
    pub measurement_time_s: f64,
    pub confidence_interval: f64,
    pub outlier_rejection: String,
    pub statistical_method: String,
    pub track: TrackTag,
    pub workload: String,
    pub strictness: String,
    pub parse_utf8: String,
    pub escape_complete: String,
    pub flaw_probe: String,
    pub output_plane: String,
    pub feature_mask: String,
    pub api_symbol: String,
    pub sidecar_freshness: String,
    pub primitive_status: String,
    pub hot_leaf: String,
    pub materialisation: String,
    pub parse_mode: String,
    pub source_ownership: String,
    pub allocator: String,
    pub plan_variant: String,
    pub host_call_mode: String,
    pub arena_writes: Option<u64>,
    pub payload_allocations: Option<u64>,
    pub scalar_parity_hash_twitter: Option<String>,
    pub scalar_parity_hash_citm: Option<String>,
    pub scalar_parity_hash_canada: Option<String>,
    pub peak_rss_bytes: Option<u64>,
    pub cold_cache_mode: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostFacts {
    pub cpu_model: String,
    pub cpu_arch: String,
    pub os_kernel: String,
    pub rustflags: String,
    pub target_cpu: String,
    pub bbnf_commit: String,
}

#[derive(Debug, Clone)]
pub struct BenchFacts {
    pub input_sha256: String,
    pub input_bytes: u64,
    pub competitor_crate: Option<String>,
    pub competitor_version: Option<String>,
    pub track: TrackTag,
    pub workload: String,
    pub strictness: String,
    pub parse_utf8: String,
    pub escape_complete: String,
    pub flaw_probe: String,
    pub output_plane: String,
    pub feature_mask: String,
    pub api_symbol: String,
    pub sidecar_freshness: String,
    pub primitive_status: String,
    pub hot_leaf: String,
    pub materialisation: String,
    pub parse_mode: String,
    pub source_ownership: String,
    pub plan_variant: String,
    pub host_call_mode: String,
    pub arena_writes: Option<u64>,
    pub payload_allocations: Option<u64>,
    pub scalar_parity_hash_twitter: Option<String>,
    pub scalar_parity_hash_citm: Option<String>,
    pub scalar_parity_hash_canada: Option<String>,
    pub peak_rss_bytes: Option<u64>,
    pub measurement_time_s: f64,
    pub sample_size: u32,
}

impl HostFacts {
    pub fn probe() -> Self {
        let rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
        let target_cpu = parse_target_cpu(&rustflags).unwrap_or_else(|| "default".to_string());
        Self {
            cpu_model: probe_cpu_model(),
            cpu_arch: std::env::consts::ARCH.to_string(),
            os_kernel: command_output("uname", &["-a"])
                .unwrap_or_else(|| std::env::consts::OS.into()),
            rustflags,
            target_cpu,
            bbnf_commit: command_output("git", &["rev-parse", "HEAD"])
                .unwrap_or_else(|| "unknown".to_string()),
        }
    }
}

impl BenchFacts {
    pub fn bbnf_json(
        input_sha256: String,
        input_bytes: u64,
        track: TrackTag,
        arena_writes: u64,
        payload_allocations: u64,
        measurement_time_s: f64,
        sample_size: u32,
    ) -> Self {
        Self {
            input_sha256,
            input_bytes,
            competitor_crate: None,
            competitor_version: None,
            track,
            workload: "parse_only".to_string(),
            strictness: "deferred".to_string(),
            parse_utf8: "view-boundary".to_string(),
            escape_complete: "yes".to_string(),
            flaw_probe: "invalid UTF-8 rejected outside hot scan".to_string(),
            output_plane: "parse_only".to_string(),
            feature_mask: "n/a".to_string(),
            api_symbol: match track {
                TrackTag::Track1Generated => "runtime::generated_json::parse_only",
                TrackTag::Track2Handcoded => "bbnf_bench::track2::json::parse",
                _ => "bbnf",
            }
            .to_string(),
            sidecar_freshness: "same-run".to_string(),
            primitive_status: "distinct parse_only runtime path".to_string(),
            hot_leaf: "unprofiled in W0b".to_string(),
            materialisation: "parse_only_validator".to_string(),
            parse_mode: "parse_str_prevalidate".to_string(),
            source_ownership: "borrowed".to_string(),
            plan_variant: runtime::tape::CapacityPlan::from_env().label().to_string(),
            host_call_mode: "none".to_string(),
            arena_writes: Some(arena_writes),
            payload_allocations: Some(payload_allocations),
            scalar_parity_hash_twitter: None,
            scalar_parity_hash_citm: None,
            scalar_parity_hash_canada: None,
            peak_rss_bytes: current_peak_rss_bytes(),
            measurement_time_s,
            sample_size,
        }
    }

    pub fn bbnf_json_workload(
        input_sha256: String,
        input_bytes: u64,
        track: TrackTag,
        materialisation: &str,
        arena_writes: u64,
        payload_allocations: u64,
        measurement_time_s: f64,
        sample_size: u32,
    ) -> Self {
        let mut facts = Self::bbnf_json(
            input_sha256,
            input_bytes,
            track,
            arena_writes,
            payload_allocations,
            measurement_time_s,
            sample_size,
        );
        facts.materialisation = materialisation.to_string();
        facts.workload = workload_for_materialisation(materialisation).to_string();
        facts.output_plane = match materialisation {
            "parse_only_validator" => "parse_only",
            "direct_strict_product" => "direct strict product",
            "direct_to_struct" => "digest",
            "real_typed_struct" => "typed direct",
            _ => "borrowed view over offset tape",
        }
        .to_string();
        facts
    }

    pub fn competitor(
        input_sha256: String,
        input_bytes: u64,
        crate_name: &str,
        crate_version: &str,
        materialisation: &str,
        measurement_time_s: f64,
        sample_size: u32,
    ) -> Self {
        Self {
            input_sha256,
            input_bytes,
            competitor_crate: Some(crate_name.to_string()),
            competitor_version: Some(crate_version.to_string()),
            track: TrackTag::Competitor,
            workload: workload_for_materialisation(materialisation).to_string(),
            strictness: strictness_for_competitor(crate_name, materialisation).to_string(),
            parse_utf8: parse_utf8_for_competitor(crate_name, materialisation).to_string(),
            escape_complete: escape_complete_for_competitor(crate_name, materialisation)
                .to_string(),
            flaw_probe: flaw_probe_for_competitor(crate_name, materialisation).to_string(),
            output_plane: output_plane_for_competitor(materialisation).to_string(),
            feature_mask: feature_mask_for_competitor(crate_name, materialisation).to_string(),
            api_symbol: api_symbol_for_competitor(crate_name, materialisation).to_string(),
            sidecar_freshness: "same-run".to_string(),
            primitive_status: "comparator".to_string(),
            hot_leaf: "unprofiled in W0b".to_string(),
            materialisation: materialisation.to_string(),
            parse_mode: parse_mode_for_competitor(crate_name, materialisation).to_string(),
            source_ownership: "owned".to_string(),
            plan_variant: "canonical".to_string(),
            host_call_mode: "none".to_string(),
            arena_writes: None,
            payload_allocations: None,
            scalar_parity_hash_twitter: None,
            scalar_parity_hash_citm: None,
            scalar_parity_hash_canada: None,
            peak_rss_bytes: current_peak_rss_bytes(),
            measurement_time_s,
            sample_size,
        }
    }

    pub fn simd_scan(
        fixture_name: &str,
        input_sha256: String,
        input_bytes: u64,
        scalar_parity_hash: String,
        measurement_time_s: f64,
        sample_size: u32,
    ) -> Self {
        let (twitter, citm, canada) = match fixture_name {
            "twitter" => (Some(scalar_parity_hash), None, None),
            "citm_catalog" => (None, Some(scalar_parity_hash), None),
            "canada" => (None, None, Some(scalar_parity_hash)),
            _ => (Some(scalar_parity_hash), None, None),
        };
        Self {
            input_sha256,
            input_bytes,
            competitor_crate: None,
            competitor_version: None,
            track: TrackTag::SimdScan,
            workload: "cycles_per_byte".to_string(),
            strictness: "strict".to_string(),
            parse_utf8: "none".to_string(),
            escape_complete: "n/a".to_string(),
            flaw_probe: "structural scan parity probe".to_string(),
            output_plane: "offset bitmap".to_string(),
            feature_mask: format!("{:?}", bbnf_simd::active_backend()),
            api_symbol: "bbnf_bench::scan::structural_offsets_simd".to_string(),
            sidecar_freshness: "same-run".to_string(),
            primitive_status: "checkasm-backed primitive".to_string(),
            hot_leaf: "unprofiled in W0b".to_string(),
            materialisation: "structural_offsets".to_string(),
            parse_mode: "simd_scan".to_string(),
            source_ownership: "borrowed".to_string(),
            plan_variant: format!("{:?}", bbnf_simd::active_backend()),
            host_call_mode: "none".to_string(),
            arena_writes: None,
            payload_allocations: None,
            scalar_parity_hash_twitter: twitter,
            scalar_parity_hash_citm: citm,
            scalar_parity_hash_canada: canada,
            peak_rss_bytes: current_peak_rss_bytes(),
            measurement_time_s,
            sample_size,
        }
    }
}

impl RowMetadata {
    pub fn from_bench(host: &HostFacts, facts: BenchFacts) -> Self {
        Self {
            schema_version: SCHEMA_VERSION.to_string(),
            cpu_model: host.cpu_model.clone(),
            cpu_arch: host.cpu_arch.clone(),
            os_kernel: host.os_kernel.clone(),
            rustflags: host.rustflags.clone(),
            target_cpu: host.target_cpu.clone(),
            profile: std::env::var("PROFILE").unwrap_or_else(|_| "bench".to_string()),
            input_sha256: facts.input_sha256,
            input_bytes: facts.input_bytes,
            competitor_crate: facts.competitor_crate,
            competitor_version: facts.competitor_version,
            bbnf_commit: host.bbnf_commit.clone(),
            warmup_samples: 3,
            warmup_time_s: 3.0,
            sample_size: facts.sample_size,
            measurement_time_s: facts.measurement_time_s,
            confidence_interval: 0.95,
            outlier_rejection: "iqr".to_string(),
            statistical_method: "bootstrap".to_string(),
            track: facts.track,
            workload: facts.workload,
            strictness: facts.strictness,
            parse_utf8: facts.parse_utf8,
            escape_complete: facts.escape_complete,
            flaw_probe: facts.flaw_probe,
            output_plane: facts.output_plane,
            feature_mask: facts.feature_mask,
            api_symbol: facts.api_symbol,
            sidecar_freshness: facts.sidecar_freshness,
            primitive_status: facts.primitive_status,
            hot_leaf: facts.hot_leaf,
            materialisation: facts.materialisation,
            parse_mode: facts.parse_mode,
            source_ownership: facts.source_ownership,
            allocator: "mimalloc".to_string(),
            plan_variant: facts.plan_variant,
            host_call_mode: facts.host_call_mode,
            arena_writes: facts.arena_writes,
            payload_allocations: facts.payload_allocations,
            scalar_parity_hash_twitter: facts.scalar_parity_hash_twitter,
            scalar_parity_hash_citm: facts.scalar_parity_hash_citm,
            scalar_parity_hash_canada: facts.scalar_parity_hash_canada,
            peak_rss_bytes: facts.peak_rss_bytes,
            cold_cache_mode: "warm".to_string(),
        }
    }

    pub fn write_toml(&self, path: &Path) -> io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let text = toml::to_string_pretty(self)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        fs::write(path, text)
    }

    pub fn required_fields_present(&self) -> bool {
        self.schema_version == SCHEMA_VERSION
            && !self.cpu_model.is_empty()
            && !self.cpu_arch.is_empty()
            && !self.os_kernel.is_empty()
            && !self.target_cpu.is_empty()
            && !self.profile.is_empty()
            && !self.input_sha256.is_empty()
            && self.input_bytes > 0
            && !self.bbnf_commit.is_empty()
            && self.warmup_samples > 0
            && self.warmup_time_s > 0.0
            && self.sample_size > 0
            && self.measurement_time_s > 0.0
            && self.confidence_interval > 0.0
            && !self.outlier_rejection.is_empty()
            && !self.statistical_method.is_empty()
            && !self.workload.is_empty()
            && !self.strictness.is_empty()
            && !self.parse_utf8.is_empty()
            && !self.escape_complete.is_empty()
            && !self.flaw_probe.is_empty()
            && !self.output_plane.is_empty()
            && !self.feature_mask.is_empty()
            && !self.api_symbol.is_empty()
            && !self.sidecar_freshness.is_empty()
            && !self.primitive_status.is_empty()
            && !self.hot_leaf.is_empty()
            && !self.materialisation.is_empty()
            && !self.parse_mode.is_empty()
            && !self.source_ownership.is_empty()
            && !self.allocator.is_empty()
            && !self.plan_variant.is_empty()
            && !self.host_call_mode.is_empty()
            && !self.cold_cache_mode.is_empty()
            && (self.track != TrackTag::Competitor
                || (self
                    .competitor_crate
                    .as_deref()
                    .is_some_and(|value| !value.is_empty())
                    && self
                        .competitor_version
                        .as_deref()
                        .is_some_and(|value| !value.is_empty())))
            && (self.track != TrackTag::SimdScan || self.has_scalar_parity_hash())
    }

    pub fn has_scalar_parity_hash(&self) -> bool {
        [
            &self.scalar_parity_hash_twitter,
            &self.scalar_parity_hash_citm,
            &self.scalar_parity_hash_canada,
        ]
        .into_iter()
        .flatten()
        .any(|hash| !hash.is_empty())
    }
}

fn workload_for_materialisation(materialisation: &str) -> &str {
    match materialisation {
        "direct_to_struct" | "direct_strict_product" => "direct_to_struct",
        "real_typed_struct" => "real_typed_struct",
        "skip_checked" => "parse_only",
        _ => "parse_only",
    }
}

fn strictness_for_competitor(crate_name: &str, materialisation: &str) -> &'static str {
    if crate_name == "sonic-rs" && materialisation.contains("lossy") {
        "permissive"
    } else {
        match crate_name {
            "sonic-rs" | "simd-json" | "serde_json" => "strict",
            _ => "unknown",
        }
    }
}

fn parse_utf8_for_competitor(crate_name: &str, materialisation: &str) -> &'static str {
    if crate_name == "sonic-rs" && materialisation.contains("lossy") {
        "none"
    } else {
        match crate_name {
            "sonic-rs" | "simd-json" | "serde_json" => "scan-boundary",
            _ => "none",
        }
    }
}

fn escape_complete_for_competitor(crate_name: &str, materialisation: &str) -> &'static str {
    if crate_name == "sonic-rs" && materialisation.contains("lossy") {
        "no"
    } else {
        "yes"
    }
}

fn flaw_probe_for_competitor(crate_name: &str, materialisation: &str) -> &'static str {
    if crate_name == "sonic-rs" && materialisation.contains("lossy") {
        "lossy UTF-8 substitution; not S-anchor eligible"
    } else {
        "none"
    }
}

fn output_plane_for_competitor(materialisation: &str) -> &'static str {
    match materialisation {
        "direct_strict_product" => "direct strict product",
        "direct_to_struct" | "direct_to_struct_lossy" => "digest",
        "real_typed_struct" | "real_typed_struct_lossy" => "typed direct",
        "skip_checked" => "parse_only/sonic_rs::Skipper",
        "borrowed" | "owned" | "eager_typed" | "eager_typed_lossy" => "DOM",
        _ => "DOM",
    }
}

fn feature_mask_for_competitor(crate_name: &str, _materialisation: &str) -> &'static str {
    match crate_name {
        "sonic-rs" => "sort_keys",
        "simd-json" => "serde_impl",
        "serde_json" => "preserve_order",
        _ => "unknown",
    }
}

fn api_symbol_for_competitor(crate_name: &str, materialisation: &str) -> &'static str {
    match (crate_name, materialisation) {
        ("sonic-rs", "skip_checked") => "bbnf_bench::sonic_skipper::parse_only",
        ("sonic-rs", "eager_typed_lossy") => {
            "sonic_rs::Deserializer::from_slice(...).utf8_lossy().deserialize::<Value>()"
        }
        ("sonic-rs", _) => "sonic_rs::from_slice::<T>",
        ("simd-json", "borrowed") => "simd_json::to_borrowed_value",
        ("simd-json", "owned") => "simd_json::to_owned_value",
        ("serde_json", _) => "serde_json::from_slice::<T>",
        _ => "unknown",
    }
}

fn parse_mode_for_competitor(crate_name: &str, materialisation: &str) -> &'static str {
    if crate_name == "sonic-rs" && materialisation == "skip_checked" {
        "skip_checked"
    } else if crate_name == "sonic-rs" && materialisation.contains("lossy") {
        "from_slice_utf8_lossy"
    } else {
        "from_slice"
    }
}

pub fn current_peak_rss_bytes() -> Option<u64> {
    let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
    let rc = unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) };
    if rc != 0 {
        return None;
    }
    let maxrss = unsafe { usage.assume_init().ru_maxrss };
    if maxrss <= 0 {
        return None;
    }
    let bytes = if cfg!(target_os = "macos") {
        maxrss as u64
    } else {
        maxrss as u64 * 1024
    };
    Some(bytes)
}

fn parse_target_cpu(rustflags: &str) -> Option<String> {
    let mut words = rustflags.split_whitespace();
    while let Some(word) = words.next() {
        if word == "-C" {
            if let Some(value) = words
                .next()
                .and_then(|next| next.strip_prefix("target-cpu="))
            {
                return Some(value.to_string());
            }
        }
        if let Some(value) = word.strip_prefix("-Ctarget-cpu=") {
            return Some(value.to_string());
        }
    }
    None
}

fn probe_cpu_model() -> String {
    if cfg!(target_os = "macos") {
        command_output("sysctl", &["-n", "machdep.cpu.brand_string"])
            .unwrap_or_else(|| "unknown".to_string())
    } else {
        command_output("lscpu", &[])
            .and_then(|text| {
                text.lines()
                    .find_map(|line| line.strip_prefix("Model name:"))
                    .map(str::trim)
                    .map(str::to_string)
            })
            .unwrap_or_else(|| "unknown".to_string())
    }
}

fn command_output(program: &str, args: &[&str]) -> Option<String> {
    let output = Command::new(program).args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8(output.stdout).ok()?;
    Some(text.trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_target_cpu_from_rustflags() {
        assert_eq!(
            parse_target_cpu("-C target-cpu=native -C opt-level=3").as_deref(),
            Some("native")
        );
        assert_eq!(
            parse_target_cpu("-Ctarget-cpu=apple-m1").as_deref(),
            Some("apple-m1")
        );
    }

    #[test]
    fn row_metadata_has_required_fields() {
        let host = HostFacts {
            cpu_model: "cpu".into(),
            cpu_arch: "arch".into(),
            os_kernel: "os".into(),
            rustflags: String::new(),
            target_cpu: "default".into(),
            bbnf_commit: "commit".into(),
        };
        let row = RowMetadata::from_bench(
            &host,
            BenchFacts::bbnf_json(
                "a".repeat(64),
                12,
                TrackTag::Track2Handcoded,
                0,
                0,
                5.0,
                100,
            ),
        );
        assert!(row.required_fields_present());
    }
}
