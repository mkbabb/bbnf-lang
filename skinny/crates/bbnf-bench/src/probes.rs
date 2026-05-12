#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProbeKind {
    HostCallDispatchOverhead,
    HostCallEagerDecode,
    AlternateScalarPlan,
    AlternateDispatchTablePlan,
    AlternatePextMaskPlan,
    ColdFirstParse,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProbeResult {
    pub kind: ProbeKind,
    pub status: ProbeStatus,
    pub threshold: Option<ProbeThreshold>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProbeStatus {
    Pending,
    Passed,
    Failed,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ProbeThreshold {
    pub max_ratio: f64,
}

pub fn configured_masking_probes() -> Vec<ProbeResult> {
    [
        ProbeKind::HostCallDispatchOverhead,
        ProbeKind::HostCallEagerDecode,
        ProbeKind::AlternateScalarPlan,
        ProbeKind::AlternateDispatchTablePlan,
        ProbeKind::AlternatePextMaskPlan,
        ProbeKind::ColdFirstParse,
    ]
    .into_iter()
    .map(|kind| ProbeResult {
        kind,
        status: ProbeStatus::Pending,
        threshold: default_threshold(kind),
    })
    .collect()
}

fn default_threshold(kind: ProbeKind) -> Option<ProbeThreshold> {
    match kind {
        ProbeKind::HostCallDispatchOverhead => Some(ProbeThreshold { max_ratio: 1.02 }),
        ProbeKind::HostCallEagerDecode => Some(ProbeThreshold { max_ratio: 1.10 }),
        ProbeKind::AlternateScalarPlan
        | ProbeKind::AlternateDispatchTablePlan
        | ProbeKind::AlternatePextMaskPlan => Some(ProbeThreshold { max_ratio: 0.98 }),
        ProbeKind::ColdFirstParse => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn configured_probe_rows_cover_masking_matrix() {
        let probes = configured_masking_probes();
        assert_eq!(probes.len(), 6);
        assert!(probes
            .iter()
            .all(|probe| probe.status == ProbeStatus::Pending));
    }
}
