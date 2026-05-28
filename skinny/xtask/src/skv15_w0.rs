use anyhow::{bail, Context, Result};
use std::collections::{BTreeMap, BTreeSet};

pub const MANIFEST_HEADING: &str = "## SK-V15 W0 Telemetry Manifest";
pub const MANIFEST_HEADER: &str = "| Row id | Grammar | Domain | Wave | Run id | Track 1 entry | Track 2 entry | Comparator plane | Per-iter equality | Audit overlay | Audit reference | Sidecar freshness | Substrate target | Retention lifetime | Policy owner | Validation | Profile artifact | Sample cost | Sample count | Build flags | Host triple | Feature mask | CostFacts | Redress | SK-V14-open delta | Substrate | Structural projection | Cardinality | Consumer | Track 2 | Diagnostic nonproducer | Comparator evidence | measurement_row_id | measurement_origin | value_plane | css_comparator_workload | generator_source | lock14_scan_scope | lock16_status | checkasm_or_parity_status | gate_exclusion_report | broadcast_group_id |";
pub const MANIFEST_ALIGN: &str = "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---:|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|";

pub const JSON_CORPORA: &[&str] = &[
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

pub const JSON_WORKLOADS: &[&str] = &["parse_only", "direct_to_struct", "real_typed_struct"];

pub const CSS_FEATURES: &[&str] = &[
    "declaration_values",
    "declarations",
    "stylesheet_root",
    "selectors",
    "at_rules_keyframes",
    "nested_rules",
    "css_variables",
    "calc_expressions",
    "var_url_functions",
    "color_functions",
    "gradients",
    "transforms",
    "filters",
    "easing_functions",
    "media_queries",
    "vendor_prefixes",
    "custom_at_rules",
    "pseudo_classes",
    "pseudo_elements",
    "attribute_selectors",
    "logical_properties",
    "grid",
    "flexbox",
    "typed_property_groups",
];

const EXPECTED_HOST: &str = "aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max";
const CSS_BROADCAST_GROUP: &str = "SK-V14-W8R-css-l4-full-parse";
const CSS_MEASUREMENT_ROW: &str = "SK-V14-W8R-css-full-parse-profile-cold-8";

#[derive(Debug, Clone)]
struct ManifestRow {
    row_id: String,
    grammar: String,
    domain: String,
    wave: String,
    run_id: String,
    track1_entry: String,
    track2_entry: String,
    comparator_plane: String,
    per_iter_equality: String,
    audit_overlay: String,
    audit_reference: String,
    sidecar_freshness: String,
    substrate_target: String,
    retention_lifetime: String,
    policy_owner: String,
    validation: String,
    profile_artifact: String,
    sample_cost: String,
    sample_count: u64,
    build_flags: String,
    host_triple: String,
    feature_mask: String,
    costfacts: String,
    redress: String,
    skv14_open_delta: String,
    substrate: String,
    structural_projection: String,
    cardinality: String,
    consumer: String,
    track2: String,
    diagnostic_nonproducer: String,
    comparator_evidence: String,
    measurement_row_id: String,
    measurement_origin: String,
    value_plane: String,
    css_comparator_workload: String,
    generator_source: String,
    lock14_scan_scope: String,
    lock16_status: String,
    checkasm_or_parity_status: String,
    gate_exclusion_report: String,
    broadcast_group_id: String,
}

#[derive(Debug, Clone)]
struct VisibleJsonRow {
    corpus: String,
    workload: String,
    outcome: String,
    verdict: String,
    strictness: String,
    parse_utf8: String,
    escape_complete: String,
}

pub fn validate_results(results_text: &str) -> Result<()> {
    let rows = parse_manifest_rows(results_text)?;
    let visible_json = parse_visible_json_rows(results_text)?;
    validate_row_universe(&rows, &visible_json)?;
    validate_manifest_rows(&rows)?;
    validate_visible_json(&visible_json)?;
    validate_broadcast_groups(&rows)?;
    Ok(())
}

fn parse_manifest_rows(results_text: &str) -> Result<Vec<ManifestRow>> {
    if results_text.contains("## SK-V14 W0 Telemetry Manifest") {
        bail!("RESULTS.md still carries SK-V14 W0 telemetry heading after SK-V15 W0 redress");
    }
    if results_text.matches(MANIFEST_HEADING).count() != 1 {
        bail!("RESULTS.md must carry exactly one {MANIFEST_HEADING}");
    }

    let mut in_manifest = false;
    let mut rows = Vec::new();
    for line in results_text.lines() {
        if line.trim() == MANIFEST_HEADING {
            in_manifest = true;
            continue;
        }
        if in_manifest && line.starts_with("## ") {
            break;
        }
        if !in_manifest {
            continue;
        }
        let cells = markdown_cells(line);
        if cells.is_empty()
            || cells[0] == "Row id"
            || cells[0] == "---"
            || !(cells[0].starts_with("json/") || cells[0].starts_with("css_l4/"))
        {
            continue;
        }
        rows.push(ManifestRow::from_cells(&cells)?);
    }
    if rows.is_empty() {
        bail!("{MANIFEST_HEADING} contains no telemetry rows");
    }
    Ok(rows)
}

impl ManifestRow {
    fn from_cells(cells: &[String]) -> Result<Self> {
        if cells.len() != 42 {
            bail!(
                "SK-V15 W0 manifest row {} expected 42 cells, saw {}",
                cells
                    .first()
                    .map(String::as_str)
                    .unwrap_or("<missing-row-id>"),
                cells.len()
            );
        }
        for (index, value) in cells.iter().enumerate() {
            if value.trim().is_empty() {
                bail!("{} has empty SK-V15 W0 cell {index}", cells[0]);
            }
        }
        Ok(Self {
            row_id: cells[0].clone(),
            grammar: cells[1].clone(),
            domain: cells[2].clone(),
            wave: cells[3].clone(),
            run_id: cells[4].clone(),
            track1_entry: cells[5].clone(),
            track2_entry: cells[6].clone(),
            comparator_plane: cells[7].clone(),
            per_iter_equality: cells[8].clone(),
            audit_overlay: cells[9].clone(),
            audit_reference: cells[10].clone(),
            sidecar_freshness: cells[11].clone(),
            substrate_target: cells[12].clone(),
            retention_lifetime: cells[13].clone(),
            policy_owner: cells[14].clone(),
            validation: cells[15].clone(),
            profile_artifact: cells[16].clone(),
            sample_cost: cells[17].clone(),
            sample_count: cells[18]
                .parse()
                .with_context(|| format!("{} has invalid sample count {}", cells[0], cells[18]))?,
            build_flags: cells[19].clone(),
            host_triple: cells[20].clone(),
            feature_mask: cells[21].clone(),
            costfacts: cells[22].clone(),
            redress: cells[23].clone(),
            skv14_open_delta: cells[24].clone(),
            substrate: cells[25].clone(),
            structural_projection: cells[26].clone(),
            cardinality: cells[27].clone(),
            consumer: cells[28].clone(),
            track2: cells[29].clone(),
            diagnostic_nonproducer: cells[30].clone(),
            comparator_evidence: cells[31].clone(),
            measurement_row_id: cells[32].clone(),
            measurement_origin: cells[33].clone(),
            value_plane: cells[34].clone(),
            css_comparator_workload: cells[35].clone(),
            generator_source: cells[36].clone(),
            lock14_scan_scope: cells[37].clone(),
            lock16_status: cells[38].clone(),
            checkasm_or_parity_status: cells[39].clone(),
            gate_exclusion_report: cells[40].clone(),
            broadcast_group_id: cells[41].clone(),
        })
    }
}

fn parse_visible_json_rows(results_text: &str) -> Result<Vec<VisibleJsonRow>> {
    let mut rows = Vec::new();
    for line in results_text.lines() {
        let cells = markdown_cells(line);
        if cells.len() != 26 || !JSON_CORPORA.contains(&cells[0].as_str()) {
            continue;
        }
        if !JSON_WORKLOADS.contains(&cells[1].as_str()) {
            bail!(
                "visible RESULTS row {} has unknown JSON workload {}",
                cells[0],
                cells[1]
            );
        }
        rows.push(VisibleJsonRow {
            corpus: cells[0].clone(),
            workload: cells[1].clone(),
            outcome: cells[2].clone(),
            verdict: cells[3].clone(),
            strictness: cells[4].clone(),
            parse_utf8: cells[5].clone(),
            escape_complete: cells[6].clone(),
        });
    }
    Ok(rows)
}

fn validate_row_universe(rows: &[ManifestRow], visible_json: &[VisibleJsonRow]) -> Result<()> {
    let expected_json = JSON_CORPORA.len() * JSON_WORKLOADS.len();
    let expected_css = CSS_FEATURES.len();
    let json_count = rows
        .iter()
        .filter(|row| row.row_id.starts_with("json/"))
        .count();
    let css_count = rows
        .iter()
        .filter(|row| row.row_id.starts_with("css_l4/"))
        .count();
    if rows.len() != expected_json + expected_css {
        bail!(
            "SK-V15 W0 manifest expected {} rows, saw {}",
            expected_json + expected_css,
            rows.len()
        );
    }
    if json_count != expected_json {
        bail!("SK-V15 W0 manifest expected {expected_json} JSON rows, saw {json_count}");
    }
    if css_count != expected_css {
        bail!("SK-V15 W0 manifest expected {expected_css} CSS rows, saw {css_count}");
    }
    if visible_json.len() != expected_json {
        bail!(
            "visible RESULTS table expected {expected_json} JSON rows, saw {}",
            visible_json.len()
        );
    }

    let mut seen = BTreeSet::new();
    for row in rows {
        if !seen.insert(row.row_id.clone()) {
            bail!("duplicate SK-V15 W0 manifest row {}", row.row_id);
        }
    }
    for corpus in JSON_CORPORA {
        for workload in JSON_WORKLOADS {
            let row_id = format!("json/{corpus}/{workload}/main");
            if !seen.contains(&row_id) {
                bail!("SK-V15 W0 manifest missing {row_id}");
            }
        }
    }
    for feature in CSS_FEATURES {
        let row_id = format!("css_l4/{feature}/direct_to_struct/main");
        if !seen.contains(&row_id) {
            bail!("SK-V15 W0 manifest missing {row_id}");
        }
    }
    Ok(())
}

fn validate_manifest_rows(rows: &[ManifestRow]) -> Result<()> {
    for row in rows {
        validate_base_carrier_fields(row)?;
        validate_platform(row)?;
        validate_gate_exclusion(row)?;
        if row.row_id.starts_with("json/") {
            validate_json_row(row)?;
        } else if row.row_id.starts_with("css_l4/") {
            validate_css_row(row)?;
        } else {
            bail!("SK-V15 W0 row {} has unsupported family", row.row_id);
        }
    }
    Ok(())
}

fn validate_base_carrier_fields(row: &ManifestRow) -> Result<()> {
    for (field, value) in [
        ("row_id", row.row_id.as_str()),
        ("grammar", row.grammar.as_str()),
        ("domain", row.domain.as_str()),
        ("wave", row.wave.as_str()),
        ("run_id", row.run_id.as_str()),
        ("track1_entry", row.track1_entry.as_str()),
        ("track2_entry", row.track2_entry.as_str()),
        ("comparator_plane", row.comparator_plane.as_str()),
        ("per_iter_equality", row.per_iter_equality.as_str()),
        ("audit_overlay", row.audit_overlay.as_str()),
        ("audit_reference", row.audit_reference.as_str()),
        ("sidecar_freshness", row.sidecar_freshness.as_str()),
        ("substrate_target", row.substrate_target.as_str()),
        ("retention_lifetime", row.retention_lifetime.as_str()),
        ("policy_owner", row.policy_owner.as_str()),
        ("validation", row.validation.as_str()),
        ("profile_artifact", row.profile_artifact.as_str()),
        ("sample_cost", row.sample_cost.as_str()),
        ("build_flags", row.build_flags.as_str()),
        ("host_triple", row.host_triple.as_str()),
        ("feature_mask", row.feature_mask.as_str()),
        ("costfacts", row.costfacts.as_str()),
        ("redress", row.redress.as_str()),
        ("skv14_open_delta", row.skv14_open_delta.as_str()),
        ("substrate", row.substrate.as_str()),
        ("structural_projection", row.structural_projection.as_str()),
        ("cardinality", row.cardinality.as_str()),
        ("consumer", row.consumer.as_str()),
        ("track2", row.track2.as_str()),
        (
            "diagnostic_nonproducer",
            row.diagnostic_nonproducer.as_str(),
        ),
        ("comparator_evidence", row.comparator_evidence.as_str()),
    ] {
        if value.trim().is_empty() {
            bail!("{} has empty SK-V15 W0 base field {field}", row.row_id);
        }
    }
    Ok(())
}

fn validate_platform(row: &ManifestRow) -> Result<()> {
    if row.host_triple != EXPECTED_HOST {
        bail!(
            "{} host {} is not native Apple M5 Max/aarch64",
            row.row_id,
            row.host_triple
        );
    }
    if !row.build_flags.contains("target-cpu=native")
        || !row.feature_mask.contains("arch=aarch64")
        || !row.feature_mask.contains("target_cpu=native")
    {
        bail!(
            "{} lacks native Apple M5/aarch64 build evidence",
            row.row_id
        );
    }
    Ok(())
}

fn validate_gate_exclusion(row: &ManifestRow) -> Result<()> {
    for (field, value) in [
        ("lock14_scan_scope", row.lock14_scan_scope.as_str()),
        ("lock16_status", row.lock16_status.as_str()),
        ("gate_exclusion_report", row.gate_exclusion_report.as_str()),
    ] {
        if value.contains("self-exempting") {
            bail!("{} has self-exempting {field}: {value}", row.row_id);
        }
    }
    Ok(())
}

fn validate_json_row(row: &ManifestRow) -> Result<()> {
    let (corpus, workload) = parse_json_row_id(&row.row_id)?;
    if !JSON_CORPORA.contains(&corpus) || !JSON_WORKLOADS.contains(&workload) {
        bail!("{} is outside the SK-V15 W0 JSON universe", row.row_id);
    }
    if row.grammar != "json" || row.domain != "json_bench" {
        bail!(
            "{} has non-JSON grammar/domain {}/{}",
            row.row_id,
            row.grammar,
            row.domain
        );
    }
    if row.audit_overlay != "AUDIT-SUSTAINED" || !row.per_iter_equality.starts_with("PASS:") {
        bail!(
            "{} JSON row is not sustained with per-iteration equality",
            row.row_id
        );
    }
    if row.measurement_row_id != row.row_id {
        bail!(
            "{} measurement_row_id {} is not row-keyed",
            row.row_id,
            row.measurement_row_id
        );
    }
    for required in [
        format!("row={}", row.row_id),
        format!("run={}", row.run_id),
        format!("sample_count={}", row.sample_count),
        "sample_cost=".to_string(),
    ] {
        if !row.measurement_origin.contains(&required) {
            bail!("{} measurement_origin missing {required}", row.row_id);
        }
    }
    if row.value_plane != json_value_plane(workload) {
        bail!(
            "{} value_plane {} does not match {workload}",
            row.row_id,
            row.value_plane
        );
    }
    if row.css_comparator_workload != "n/a:not-css" {
        bail!("{} JSON row carries CSS comparator workload", row.row_id);
    }
    if row.generator_source != "grammar=skinny/grammars/json.bbnf;generator=skinny-json-runtime" {
        bail!(
            "{} JSON row lacks grammar-backed generator source",
            row.row_id
        );
    }
    if !row
        .lock14_scan_scope
        .contains("included=json-runtime+bench+gate")
        || !row.lock14_scan_scope.contains("excluded=none")
    {
        bail!("{} JSON row lacks full Lock 14 scan scope", row.row_id);
    }
    if row.lock16_status != "not-applicable:no-simd-or-asm" {
        bail!(
            "{} JSON row carries unexpected Lock 16 status {}",
            row.row_id,
            row.lock16_status
        );
    }
    if row.checkasm_or_parity_status != "pass:json_same_run_parity" {
        bail!("{} JSON row lacks same-run parity status", row.row_id);
    }
    if row.gate_exclusion_report != "none:full-surface-scan" {
        bail!(
            "{} JSON row has gate exclusion {}",
            row.row_id,
            row.gate_exclusion_report
        );
    }
    if row.broadcast_group_id != "none:independent" {
        bail!("{} JSON row is grouped as a broadcast", row.row_id);
    }
    Ok(())
}

fn validate_css_row(row: &ManifestRow) -> Result<()> {
    let feature = parse_css_row_id(&row.row_id)?;
    if !CSS_FEATURES.contains(&feature) {
        bail!("{} is outside the SK-V15 W0 CSS universe", row.row_id);
    }
    if row.grammar != "css_l4" || row.domain != "css_l4_bench" {
        bail!(
            "{} has non-CSS grammar/domain {}/{}",
            row.row_id,
            row.grammar,
            row.domain
        );
    }
    if row.audit_overlay == "AUDIT-SUSTAINED"
        || row.per_iter_equality.starts_with("PASS:")
        || row.skv14_open_delta.starts_with("admitted:")
    {
        bail!(
            "{} still presents W8R CSS broadcast evidence as live admission",
            row.row_id
        );
    }
    if row.audit_overlay != "AUDIT-FALSIFIED" {
        bail!("{} CSS row must be AUDIT-FALSIFIED in W0", row.row_id);
    }
    if !row
        .per_iter_equality
        .starts_with("not_admitted:SK-V15-W0-broadcast-diagnostic")
    {
        bail!(
            "{} CSS row lacks W0 broadcast diagnostic marker",
            row.row_id
        );
    }
    if row.redress != "pending:SK-V15-W1-CSS-BROADCAST" {
        bail!(
            "{} CSS row is not routed to W1 broadcast redress",
            row.row_id
        );
    }
    if row.measurement_row_id != CSS_MEASUREMENT_ROW {
        bail!(
            "{} CSS measurement_row_id {} is not the W8R broadcast id",
            row.row_id,
            row.measurement_row_id
        );
    }
    if !row.measurement_origin.starts_with("diagnostic-broadcast:")
        || !row.measurement_origin.contains("run=")
        || !row.measurement_origin.contains("profile=")
    {
        bail!("{} CSS row lacks diagnostic broadcast origin", row.row_id);
    }
    if row.value_plane != "full_parse_summary" {
        bail!(
            "{} CSS row value_plane {} is not full_parse_summary",
            row.row_id,
            row.value_plane
        );
    }
    if !row
        .css_comparator_workload
        .starts_with("mismatch:track1_full_parse_summary_vs_lightningcss_cssom")
    {
        bail!(
            "{} CSS row does not disclose comparator workload mismatch",
            row.row_id
        );
    }
    if !row.generator_source.contains("CSS_GENERATED_RS") {
        bail!(
            "{} CSS row does not disclose CSS_GENERATED_RS source",
            row.row_id
        );
    }
    if !row
        .lock14_scan_scope
        .starts_with("diagnostic:pre-W2-incomplete")
    {
        bail!("{} CSS row hides incomplete Lock 14 scope", row.row_id);
    }
    if row.lock16_status != "not-applicable:no-simd-or-asm" {
        bail!(
            "{} CSS row carries unexpected Lock 16 status {}",
            row.row_id,
            row.lock16_status
        );
    }
    if row.checkasm_or_parity_status != "pass:cssparser_full_parse_diagnostic" {
        bail!(
            "{} CSS row lacks diagnostic cssparser parity status",
            row.row_id
        );
    }
    if !row
        .gate_exclusion_report
        .contains("disposition=non-admission")
    {
        bail!(
            "{} CSS row gate exclusion report is not non-admission",
            row.row_id
        );
    }
    if row.broadcast_group_id != CSS_BROADCAST_GROUP {
        bail!(
            "{} CSS row has unexpected broadcast group {}",
            row.row_id,
            row.broadcast_group_id
        );
    }
    Ok(())
}

fn validate_visible_json(rows: &[VisibleJsonRow]) -> Result<()> {
    let mut seen = BTreeSet::new();
    for row in rows {
        if !seen.insert((row.corpus.clone(), row.workload.clone())) {
            bail!("duplicate visible JSON row {}/{}", row.corpus, row.workload);
        }
        if row.outcome != "A"
            || row.verdict != "GO"
            || row.strictness != "strict"
            || row.parse_utf8 != "measured-row"
            || row.escape_complete != "yes"
        {
            bail!(
                "visible JSON row {}/{} is not strict admitted measured-row evidence",
                row.corpus,
                row.workload
            );
        }
    }
    for corpus in JSON_CORPORA {
        for workload in JSON_WORKLOADS {
            if !seen.contains(&((*corpus).to_string(), (*workload).to_string())) {
                bail!("visible RESULTS table missing JSON row {corpus}/{workload}");
            }
        }
    }
    Ok(())
}

fn validate_broadcast_groups(rows: &[ManifestRow]) -> Result<()> {
    let mut groups: BTreeMap<(&str, &str), Vec<&ManifestRow>> = BTreeMap::new();
    for row in rows {
        groups
            .entry((&row.measurement_row_id, &row.broadcast_group_id))
            .or_default()
            .push(row);
    }
    for ((measurement_row_id, broadcast_group_id), grouped) in groups {
        if broadcast_group_id == "none:independent" {
            if grouped.len() != 1 {
                bail!(
                    "independent measurement {measurement_row_id} is broadcast across {} rows",
                    grouped.len()
                );
            }
            continue;
        }
        if broadcast_group_id != CSS_BROADCAST_GROUP || measurement_row_id != CSS_MEASUREMENT_ROW {
            bail!("unexpected broadcast group {broadcast_group_id} for {measurement_row_id}");
        }
        if grouped.len() != CSS_FEATURES.len() {
            bail!(
                "CSS diagnostic broadcast group expected {} rows, saw {}",
                CSS_FEATURES.len(),
                grouped.len()
            );
        }
        if grouped.iter().any(|row| {
            row.audit_overlay != "AUDIT-FALSIFIED"
                || !row.per_iter_equality.starts_with("not_admitted:")
        }) {
            bail!("CSS broadcast group contains live-admission evidence");
        }
    }
    Ok(())
}

fn parse_json_row_id(row_id: &str) -> Result<(&str, &str)> {
    let tail = row_id
        .strip_prefix("json/")
        .and_then(|tail| tail.strip_suffix("/main"))
        .with_context(|| format!("{row_id} is not a JSON main row"))?;
    let (corpus, workload) = tail
        .rsplit_once('/')
        .with_context(|| format!("{row_id} lacks JSON workload"))?;
    Ok((corpus, workload))
}

fn parse_css_row_id(row_id: &str) -> Result<&str> {
    row_id
        .strip_prefix("css_l4/")
        .and_then(|tail| tail.strip_suffix("/direct_to_struct/main"))
        .with_context(|| format!("{row_id} is not a CSS L4 direct_to_struct row"))
}

fn json_value_plane(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "json_parse_only",
        "direct_to_struct" => "json_direct_strict_product",
        "real_typed_struct" => "json_typed_direct",
        _ => "unknown",
    }
}

fn markdown_cells(line: &str) -> Vec<String> {
    if !line.trim_start().starts_with('|') {
        return Vec::new();
    }
    let inner = line.trim().trim_matches('|');
    let mut cells = Vec::new();
    let mut current = String::new();
    let mut escaped = false;
    for ch in inner.chars() {
        if escaped {
            if ch == '|' {
                current.push('|');
            } else {
                current.push('\\');
                current.push(ch);
            }
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if ch == '|' {
            cells.push(current.trim().to_string());
            current.clear();
        } else {
            current.push(ch);
        }
    }
    if escaped {
        current.push('\\');
    }
    cells.push(current.trim().to_string());
    cells
}
