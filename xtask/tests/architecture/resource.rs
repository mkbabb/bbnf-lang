use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use serde::Deserialize;
use xtask::architecture::evidence::binding::SourceBinding;
use xtask::architecture::evidence::write::{EvidencePayload, GenerationStatus, publish};
use xtask::architecture::inventory::Inventory;
use xtask::architecture::manifest::ConstellationManifest;
use xtask::architecture::project::project;
use xtask::architecture::schema::{GraphArtifact, PipelineStage, RequiredFailure, Sha256Digest};

const MAX_WALL: Duration = Duration::from_secs(45);
const MAX_RSS_BYTES: u64 = 384 * 1024 * 1024;

#[derive(Deserialize)]
struct ResourceFixture {
    schema: String,
    publication_hash: String,
    binding_file_hash: String,
    source_binding_hash: String,
    exporter_hash: String,
    host_hash: String,
    expected_files: Vec<String>,
}

#[test]
#[ignore = "full-shape external resource gate; set generation, fixture, and fixture SHA-256"]
fn full_shape_publication_stays_inside_resource_envelope() {
    let retained = required_path("BBNF_ARCHITECTURE_RESOURCE_GENERATION");
    let fixture_path = required_path("BBNF_ARCHITECTURE_RESOURCE_FIXTURE");
    let expected_fixture = std::env::var("BBNF_ARCHITECTURE_RESOURCE_FIXTURE_SHA256")
        .expect("BBNF_ARCHITECTURE_RESOURCE_FIXTURE_SHA256");
    let fixture_bytes = std::fs::read(&fixture_path).expect("resource fixture");
    assert_eq!(Sha256Digest::of(&fixture_bytes).as_str(), expected_fixture);
    let fixture: ResourceFixture = serde_json::from_slice(&fixture_bytes).expect("fixture JSON");
    assert_eq!(fixture.schema, "bbnf.architecture.resource-gate.v3.1");
    verify_provenance(&retained, &fixture);

    let started = Instant::now();
    let output = Command::new("/usr/bin/time")
        .arg("-l")
        .arg(std::env::current_exe().expect("architecture test executable"))
        .args([
            "--ignored",
            "--exact",
            "resource::full_shape_resource_replay_child",
            "--nocapture",
        ])
        .env("BBNF_ARCHITECTURE_RESOURCE_GENERATION", &retained)
        .output()
        .expect("external time resource child");
    let wall = started.elapsed();
    let stderr = String::from_utf8(output.stderr).expect("time output UTF-8");
    let rss = maximum_resident_set(&stderr).expect("maximum resident set from /usr/bin/time -l");
    eprintln!(
        "{}",
        serde_json::json!({
            "schema": "bbnf.architecture.resource-measurement.v3.1",
            "fixture_hash": expected_fixture,
            "publication_hash": fixture.publication_hash,
            "source_binding_hash": fixture.source_binding_hash,
            "exporter_hash": fixture.exporter_hash,
            "host_hash": fixture.host_hash,
            "wall_micros": wall.as_micros(),
            "max_rss_bytes": rss,
            "wall_limit_micros": MAX_WALL.as_micros(),
            "rss_limit_bytes": MAX_RSS_BYTES,
        })
    );
    assert!(
        output.status.success(),
        "resource child failed: stdout={} stderr={stderr}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert!(wall <= MAX_WALL, "resource replay took {wall:?}");
    assert!(rss <= MAX_RSS_BYTES, "resource replay used {rss} bytes RSS");
}

#[test]
#[ignore = "isolated child for full_shape_publication_stays_inside_resource_envelope"]
fn full_shape_resource_replay_child() {
    let retained = required_path("BBNF_ARCHITECTURE_RESOURCE_GENERATION");
    replay(&retained);
}

fn replay(retained: &Path) {
    let started = Instant::now();
    let read = |name: &str| std::fs::read(retained.join(name)).expect("generation member");
    let binding: SourceBinding = serde_json::from_slice(&read("binding.json")).expect("binding");
    let manifest: ConstellationManifest =
        serde_json::from_slice(&read("manifest.json")).expect("manifest");
    let inventory: Inventory = serde_json::from_slice(&read("inventory.json")).expect("inventory");
    let mut graph: GraphArtifact = serde_json::from_slice(&read("graph.json")).expect("graph");
    let failures: Vec<RequiredFailure> =
        serde_json::from_slice(&read("obligations.json")).expect("obligations");
    let projections = project(&graph).expect("full-shape projections");
    let mut projection_bytes = serde_json::to_vec(&projections).expect("projection bytes");
    projection_bytes.push(b'\n');
    assert_eq!(projection_bytes, read("projections.json"));
    xtask::architecture::evidence::validate::validate_all(
        &manifest,
        &inventory,
        &binding,
        &mut graph,
        &projections,
    )
    .expect("full-shape validation");

    let output = tempfile::tempdir().expect("disposable publication root");
    let generation = publish(
        output.path(),
        EvidencePayload {
            binding: &binding,
            manifest: Some(&manifest),
            inventory: Some(&inventory),
            graph: Some(&graph),
            graph_validated: true,
            projections: Some(&projections),
            failures: Some(&failures),
            diagnostics: Vec::new(),
            stage: PipelineStage::Complete,
            status: GenerationStatus::Red,
        },
    )
    .expect("full-shape publication");
    let published = output
        .path()
        .join("generations")
        .join(generation.run_id.as_str());
    for entry in std::fs::read_dir(retained).expect("retained members") {
        let entry = entry.expect("retained member");
        assert_eq!(
            std::fs::read(entry.path()).expect("retained bytes"),
            std::fs::read(published.join(entry.file_name())).expect("published bytes"),
            "member {:?}",
            entry.file_name()
        );
    }
    let (allocation_bytes, peak_allocation_bytes) = super::take_heap_usage();
    eprintln!(
        "{}",
        serde_json::json!({
            "stage": "complete-resource-replay",
            "wall_micros": started.elapsed().as_micros(),
            "allocation_bytes": allocation_bytes,
            "peak_allocation_bytes": peak_allocation_bytes,
            "nodes": graph.nodes().len(),
        })
    );
}

fn verify_provenance(retained: &Path, fixture: &ResourceFixture) {
    let publication = std::fs::read(retained.join("publication.json")).expect("publication");
    assert_eq!(
        Sha256Digest::of(&publication).as_str(),
        fixture.publication_hash
    );
    let binding_bytes = std::fs::read(retained.join("binding.json")).expect("binding bytes");
    assert_eq!(
        Sha256Digest::of(&binding_bytes).as_str(),
        fixture.binding_file_hash
    );
    let binding: serde_json::Value = serde_json::from_slice(&binding_bytes).expect("binding JSON");
    assert_eq!(binding["binding_hash"], fixture.source_binding_hash);
    assert_eq!(binding["build"]["exporter_hash"], fixture.exporter_hash);
    assert_eq!(binding["build"]["host_hash"], fixture.host_hash);

    let publication: serde_json::Value =
        serde_json::from_slice(&publication).expect("publication JSON");
    let expected = fixture
        .expected_files
        .iter()
        .cloned()
        .collect::<BTreeSet<_>>();
    let declared = publication["expected_files"]
        .as_array()
        .expect("expected files")
        .iter()
        .map(|name| name.as_str().expect("file name").to_string())
        .collect::<BTreeSet<_>>();
    let actual = std::fs::read_dir(retained)
        .expect("generation directory")
        .map(|entry| {
            entry
                .expect("generation member")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(declared, expected);
    assert_eq!(actual, expected);
    for (name, member) in publication["artifacts"].as_object().expect("artifact map") {
        if member["state"] == "published" {
            let bytes = std::fs::read(retained.join(name)).expect("published member");
            assert_eq!(member["size"], bytes.len() as u64);
            assert_eq!(member["hash"], Sha256Digest::of(&bytes).as_str());
        }
    }
}

fn required_path(name: &str) -> PathBuf {
    std::env::var_os(name)
        .map(PathBuf::from)
        .unwrap_or_else(|| panic!("{name} is required"))
}

fn maximum_resident_set(stderr: &str) -> Option<u64> {
    stderr.lines().find_map(|line| {
        line.contains("maximum resident set size")
            .then(|| line.split_whitespace().next()?.parse().ok())
            .flatten()
    })
}
