use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;

pub const SCHEMA_VERSION: &str = "2";

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
            materialisation: "typed_root_over_tape".to_string(),
            parse_mode: "parse_str_prevalidate".to_string(),
            source_ownership: "borrowed".to_string(),
            plan_variant: "canonical".to_string(),
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
            materialisation: materialisation.to_string(),
            parse_mode: "from_slice".to_string(),
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
