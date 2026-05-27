use std::collections::BTreeSet;

pub const W9_WAVE_ID: &str = "SK-V14-W9";
pub const W9_SELECTED_DIRECT_ROWS: usize = 17;
pub const W9_SELECTED_TYPED_ROWS: usize = 17;

const SELECTED_CORPORA: &[&str] = &[
    "twitter",
    "citm_catalog",
    "canada",
    "apache_builds",
    "github_events",
    "update_center",
    "mesh",
    "random",
    "gsoc-2018",
    "marine_ik",
    "instruments",
    "numbers",
    "unicode_mixed",
    "unicode_escapes",
    "unicode_basic",
    "distinct_values",
    "y_string_unicode",
];

const MISSING_TYPED_SURFACES: &[&str] = &[
    "canada",
    "gsoc-2018",
    "unicode_mixed",
    "unicode_escapes",
    "distinct_values",
    "y_string_unicode",
];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum W9SurfaceDisposition {
    MeasurementRequired,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JsonW9SurfaceReport {
    pub wave_id: &'static str,
    pub selected_direct_rows: usize,
    pub selected_typed_rows: usize,
    pub fixtures_loaded: usize,
    pub direct_parity_passes: usize,
    pub direct_required_plane_blocks: usize,
    pub typed_surface_rows: usize,
    pub typed_missing_product_surface_rows: usize,
    pub typed_parity_passes: usize,
    pub typed_measurement_candidate_rows: usize,
    pub missing_typed_corpora: Vec<String>,
    pub disposition: W9SurfaceDisposition,
}

pub fn selected_corpora() -> &'static [&'static str] {
    SELECTED_CORPORA
}

pub fn missing_typed_surfaces() -> &'static [&'static str] {
    MISSING_TYPED_SURFACES
}

pub fn run_surface_attempt() -> Result<JsonW9SurfaceReport, String> {
    let fixtures = test_fixtures::load_available_bench_fixtures()
        .map_err(|error| format!("failed to load JSON W9 fixture suite: {error}"))?;
    let fixture_names = fixtures
        .iter()
        .map(|fixture| fixture.name.as_str())
        .collect::<BTreeSet<_>>();
    for expected in SELECTED_CORPORA {
        if !fixture_names.contains(expected) {
            return Err(format!("missing W9 selected fixture `{expected}`"));
        }
    }

    let mut direct_parity_passes = 0usize;
    let mut typed_surface_rows = 0usize;
    let mut typed_parity_passes = 0usize;
    let mut missing_typed_corpora = Vec::new();

    for fixture in fixtures
        .iter()
        .filter(|fixture| SELECTED_CORPORA.contains(&fixture.name.as_str()))
    {
        let input = std::str::from_utf8(&fixture.bytes)
            .map_err(|error| format!("{} is not UTF-8: {error}", fixture.name))?;
        crate::direct_struct::assert_direct_struct_parity(input, &fixture.bytes)
            .map_err(|error| format!("{} direct parity failed: {error}", fixture.name))?;
        direct_parity_passes += 1;

        if let Some(real_typed) = crate::real_typed_struct::fixture_for_name(&fixture.name) {
            typed_surface_rows += 1;
            crate::real_typed_struct::assert_real_typed_parity(input, &fixture.bytes, real_typed);
            typed_parity_passes += 1;
        } else {
            missing_typed_corpora.push(fixture.name.clone());
        }
    }

    missing_typed_corpora.sort();
    let expected_missing = MISSING_TYPED_SURFACES
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let actual_missing = missing_typed_corpora
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    if actual_missing != expected_missing {
        return Err(format!(
            "W9 missing typed surface roster drifted: expected {:?}, actual {:?}",
            MISSING_TYPED_SURFACES, missing_typed_corpora
        ));
    }

    Ok(JsonW9SurfaceReport {
        wave_id: W9_WAVE_ID,
        selected_direct_rows: W9_SELECTED_DIRECT_ROWS,
        selected_typed_rows: W9_SELECTED_TYPED_ROWS,
        fixtures_loaded: fixture_names.len(),
        direct_parity_passes,
        direct_required_plane_blocks: W9_SELECTED_DIRECT_ROWS,
        typed_surface_rows,
        typed_missing_product_surface_rows: missing_typed_corpora.len(),
        typed_parity_passes,
        typed_measurement_candidate_rows: typed_parity_passes,
        missing_typed_corpora,
        disposition: W9SurfaceDisposition::MeasurementRequired,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn w9_surface_probe_classifies_direct_and_typed_rows() {
        let report = run_surface_attempt().expect("W9 surface probe");

        assert_eq!(report.wave_id, W9_WAVE_ID);
        assert_eq!(report.selected_direct_rows, 17);
        assert_eq!(report.selected_typed_rows, 17);
        assert_eq!(report.direct_parity_passes, 17);
        assert_eq!(report.direct_required_plane_blocks, 17);
        assert_eq!(report.typed_surface_rows, 11);
        assert_eq!(report.typed_missing_product_surface_rows, 6);
        assert_eq!(report.typed_parity_passes, 11);
        assert_eq!(report.typed_measurement_candidate_rows, 11);
        assert_eq!(
            report.disposition,
            W9SurfaceDisposition::MeasurementRequired
        );
    }

    #[test]
    fn w9_missing_typed_surface_roster_is_exact() {
        assert_eq!(
            missing_typed_surfaces(),
            &[
                "canada",
                "gsoc-2018",
                "unicode_mixed",
                "unicode_escapes",
                "distinct_values",
                "y_string_unicode",
            ]
        );
    }
}
