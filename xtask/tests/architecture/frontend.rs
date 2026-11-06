use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use tempfile::tempdir;
use xtask::architecture::collect;
use xtask::architecture::evidence::binding::{BindingCapture, SourceBinding};
use xtask::architecture::inventory::{Inventory, WorktreeRecord, resolve_nominations};
use xtask::architecture::manifest::{ConstellationManifest, ResolvedRepository};
use xtask::architecture::schema::{
    CanonicalText, GraphArtifact, ObligationCode, RepositoryId, Sha256Digest, SourceAnchor,
    WorktreeId,
};
use xtask::architecture::test_support::{RunEpoch, ToolSelection};

#[test]
fn f12_semantic_symlink_policy_rejects_rust_and_frontend_escape_dangle_and_in_repo_links() {
    for (path, target) in [
        ("escape.rs", "../outside.rs"),
        ("dangling.ts", "missing.ts"),
        ("inside.css", "target.css"),
    ] {
        let roots = tempdir().expect("roots");
        let mut manifest = super::manifest();
        let repositories = materialize(&manifest, roots.path());
        std::fs::write(repositories[0].root.join("target.css"), "body {}\n").expect("target");
        std::fs::write(roots.path().join("outside.rs"), "pub fn outside() {}\n").expect("outside");
        std::os::unix::fs::symlink(target, repositories[0].root.join(path)).expect("symlink");
        let error = Inventory::collect_snapshot(&manifest, &repositories)
            .expect_err("semantic symlink must fail");
        assert!(
            error.to_string().contains("SEMANTIC_SYMLINK_REJECTED"),
            "{path}: {error:#}"
        );
        manifest.canonicalize();
    }
}

pub(super) fn p3_configured_frontend_captured_epoch_scenario() {
    let fixture = frontend_fixture();
    let mut baseline = GraphArtifact::new();
    collect::frontend::collect(
        &fixture.manifest,
        &fixture.inventory,
        &fixture.captured,
        &fixture.binding,
        &mut baseline,
    )
    .expect("baseline captured frontend graph");
    baseline.canonicalize().expect("canonical baseline");
    let repository = &fixture.repositories[0].id;
    let source_facts = baseline
        .obligations()
        .into_iter()
        .filter_map(|obligation| match &obligation.source {
            SourceAnchor::File {
                repository: source_repository,
                path,
                start_line,
                start_column,
                end_line,
                end_column,
            } if source_repository == repository
                && matches!(path.as_str(), "src/App.vue" | "src/base.css") =>
            {
                Some((
                    path.as_str(),
                    obligation.code,
                    *start_line,
                    *start_column,
                    *end_line,
                    *end_column,
                    obligation.reason.as_str(),
                ))
            }
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        source_facts,
        [
            (
                "src/App.vue",
                ObligationCode::VueScriptSourceOmitted,
                1,
                1,
                1,
                1,
                "Vue script src resolution remains Unknown",
            ),
            (
                "src/App.vue",
                ObligationCode::VueBlockSpanMisattributed,
                1,
                1,
                1,
                1,
                "Vue block offsets require file-correct span promotion",
            ),
            (
                "src/App.vue",
                ObligationCode::CssImportClassificationUnsound,
                2,
                1,
                2,
                1,
                "Vue style import url and layer semantics remain Unknown",
            ),
            (
                "src/base.css",
                ObligationCode::CssImportClassificationUnsound,
                1,
                1,
                1,
                1,
                "parsed CSS import url and layer semantics remain Unknown",
            ),
        ]
        .into_iter()
        .collect(),
        "captured frontend graph omitted fixture-derived source facts"
    );
    let baseline_json = serde_json::to_vec(&baseline).expect("canonical baseline JSON");

    let live_vue = fixture.repositories[0].root.join("src/App.vue");
    let restore = FileRestore::replace(&live_vue, b"<template>transient</template>\n");
    let mut recaptured = GraphArtifact::new();
    let collected = collect::frontend::collect(
        &fixture.manifest,
        &fixture.inventory,
        &fixture.captured,
        &fixture.binding,
        &mut recaptured,
    );
    restore.restore();
    collected.expect("collector consumes captured bytes");
    recaptured.canonicalize().expect("canonical recapture");
    assert_eq!(recaptured, baseline, "live mutation changed captured truth");
    assert_eq!(
        serde_json::to_vec(&recaptured).expect("canonical recapture JSON"),
        baseline_json,
        "canonical captured graph bytes changed"
    );

    let selected = super::tool_selection();
    let paths = ["ar", "cargo", "rustc", "node", "git", "sccache", "cc", "ld"]
        .into_iter()
        .map(|tool| {
            (
                tool.to_owned(),
                selected
                    .path(tool)
                    .expect("selected tool path")
                    .to_path_buf(),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let epoch =
        RunEpoch::for_test(Some("frontend-node-spawn"), None).expect("captured frontend epoch");
    let scope = epoch.scope().expect("captured frontend epoch scope");
    let explicit = ToolSelection::capture_with_paths_for_test(&paths).expect("explicit fanout");
    for (tool, path) in &paths {
        assert_eq!(explicit.path(tool).expect("explicit tool path"), path);
    }
    super::assert_epoch(&epoch, 0, true, false);
    let binding = fixture_binding(&fixture, explicit);
    let mut graph = GraphArtifact::new();
    let error = collect::frontend::collect(
        &fixture.manifest,
        &fixture.inventory,
        &fixture.captured,
        &binding,
        &mut graph,
    )
    .expect_err("frontend event after full tool fanout");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `frontend-node-spawn`"
    );
    super::assert_epoch(&epoch, 1, false, false);
}

#[test]
fn f13_selected_node_bytes_and_capabilities_are_bound_to_helper_facts() {
    let fixture = frontend_fixture();
    let capability = fixture
        .binding
        .tools
        .iter()
        .find(|tool| tool.tool.as_str() == "node")
        .expect("node")
        .capability
        .as_str()
        .to_string();
    with_wrapped_binding(
        &fixture,
        &capability,
        r#"{"schema":"bbnf.architecture.frontend-boundary.v3.1","edges":[],"unknowns":[]}"#,
        |wrapped, _trace| {
            assert_ne!(fixture.binding.binding_hash, wrapped.binding_hash);
            assert_ne!(
                fixture
                    .binding
                    .tools
                    .iter()
                    .find(|tool| tool.tool.as_str() == "node")
                    .expect("node")
                    .executable_hash,
                wrapped
                    .tools
                    .iter()
                    .find(|tool| tool.tool.as_str() == "node")
                    .expect("node")
                    .executable_hash
            );
            let mut graph = GraphArtifact::new();
            collect::frontend::collect(
                &fixture.manifest,
                &fixture.inventory,
                &fixture.captured,
                wrapped,
                &mut graph,
            )
            .expect("bound substitute response");
            assert!(graph.obligations().is_empty());
        },
    );
}

#[test]
fn f13_helper_schema_edges_and_anchors_are_rejected_before_graph_facts() {
    let fixture = frontend_fixture();
    let capability = fixture
        .binding
        .tools
        .iter()
        .find(|tool| tool.tool.as_str() == "node")
        .expect("node")
        .capability
        .as_str()
        .to_string();
    for (json, code) in [
        (
            r#"{"schema":"wrong","edges":[],"unknowns":[]}"#,
            "FRONTEND_HELPER_SCHEMA_MISMATCH",
        ),
        (
            r#"{"schema":"bbnf.architecture.frontend-boundary.v3.1","edges":[{}],"unknowns":[]}"#,
            "FRONTEND_HELPER_EDGE_UNVALIDATED",
        ),
        (
            r#"{"schema":"bbnf.architecture.frontend-boundary.v3.1","edges":[],"unknowns":[{"code":"GLOB_SEMANTICS_UNSOUND","path":"absent.ts","line":1,"column":1,"reason":"missing anchor"}]}"#,
            "FRONTEND_HELPER_ANCHOR_MISMATCH",
        ),
    ] {
        with_wrapped_binding(&fixture, &capability, json, |binding, _trace| {
            let mut graph = GraphArtifact::new();
            let error = collect::frontend::collect(
                &fixture.manifest,
                &fixture.inventory,
                &fixture.captured,
                binding,
                &mut graph,
            )
            .expect_err(code);
            assert!(error.to_string().contains(code), "{error:#}");
            assert!(graph.nodes().is_empty());
            assert!(graph.edges().is_empty());
            assert!(graph.obligations().is_empty());
        });
    }
}

#[test]
fn f18_frontend_events_are_operation_local_and_survive_tool_capture() {
    let fixture = frontend_fixture();
    let selection = super::tool_selection();
    let cargo = tempdir().expect("Cargo wrapper");
    let cargo_path = cargo.path().join("cargo");
    let cargo_trace = cargo.path().join("trace");
    std::fs::write(&cargo_trace, b"").expect("initialize Cargo trace");
    for (failpoint, body, invocations) in [
        (
            "tool-cargo-spawn",
            "printf '%s\\n' 'cargo 0.0.0-test' 'release: architecture-test'\n",
            0,
        ),
        ("tool-cargo-read", "exit 23\n", 1),
        ("tool-cargo-decode", "printf '\\377'\n", 1),
    ] {
        let bytes =
            format!("#!/bin/sh\nprintf '%s\\n' '{failpoint}' >> \"${{0%/*}}/trace\"\n{body}");
        write_executable(&cargo_path, &bytes);
        let bound_hash = Sha256Digest::of(bytes.as_bytes());
        assert_eq!(
            Sha256Digest::of(&std::fs::read(&cargo_path).expect("Cargo wrapper bytes")),
            bound_hash
        );
        let before = trace_lines(&cargo_trace).len();
        let epoch = RunEpoch::for_test(Some(failpoint), None).expect("Cargo tool epoch");
        let scope = epoch.scope().expect("Cargo tool epoch scope");
        let result = selection.clone().replace_tool_for_counterexample(
            "cargo",
            &cargo_path,
            &["--version", "--verbose"],
        );
        drop(scope);
        let error = result.expect_err(failpoint);
        assert_eq!(
            error.to_string(),
            format!("INJECTED_FAILURE at real operation `{failpoint}`")
        );
        super::assert_epoch(&epoch, 1, false, false);
        let trace = trace_lines(&cargo_trace);
        assert_eq!(
            trace.len() - before,
            invocations,
            "wrong {failpoint} invocation count"
        );
        assert!(
            trace[before..].iter().all(|line| line == failpoint),
            "wrong {failpoint} trace suffix: {trace:?}"
        );
        assert_eq!(
            Sha256Digest::of(&std::fs::read(&cargo_path).expect("stable Cargo wrapper bytes")),
            bound_hash
        );
    }
    assert_eq!(
        trace_lines(&cargo_trace),
        vec!["tool-cargo-read".to_owned(), "tool-cargo-decode".to_owned(),]
    );

    with_wrapped_binding(
        &fixture,
        "node architecture-test",
        "{invalid-json",
        |binding, trace| {
            assert_eq!(
                trace_lines(trace),
                vec!["version".to_owned(), "version".to_owned()],
                "binding capture must execute both exact Node probes"
            );
            for (failpoint, expected_trace) in [
                ("frontend-boundary", Vec::<String>::new()),
                (
                    "frontend-node-spawn",
                    vec!["version".to_owned(), "version".to_owned()],
                ),
                (
                    "helper-decode",
                    vec![
                        "version".to_owned(),
                        "version".to_owned(),
                        "helper".to_owned(),
                    ],
                ),
            ] {
                let before = trace_lines(trace).len();
                let epoch = RunEpoch::for_test(Some(failpoint), None).expect("frontend epoch");
                let scope = epoch.scope().expect("frontend epoch scope");
                let mut graph = GraphArtifact::new();
                let result = collect::frontend::collect(
                    &fixture.manifest,
                    &fixture.inventory,
                    &fixture.captured,
                    binding,
                    &mut graph,
                );
                drop(scope);
                let error = result.expect_err(failpoint);
                assert_eq!(
                    error.to_string(),
                    format!("INJECTED_FAILURE at real operation `{failpoint}`")
                );
                super::assert_epoch(&epoch, 1, false, false);
                let actual = trace_lines(trace);
                assert_eq!(actual[before..], expected_trace, "wrong {failpoint} phase");
            }

            let cargo_bytes = "#!/bin/sh\nprintf '%s\\n' survival-cargo >> \"${0%/*}/trace\"\nprintf '%s\\n' 'cargo 0.0.0-test' 'release: architecture-test'\n";
            write_executable(&cargo_path, cargo_bytes);
            let cargo_hash = Sha256Digest::of(cargo_bytes.as_bytes());
            let cargo_before = trace_lines(&cargo_trace).len();
            let node_before = trace_lines(trace).len();
            let epoch = RunEpoch::for_test(Some("frontend-node-spawn"), None)
                .expect("surviving frontend epoch");
            let scope = epoch.scope().expect("surviving frontend epoch scope");
            selection
                .clone()
                .replace_tool_for_counterexample("cargo", &cargo_path, &["--version", "--verbose"])
                .expect("tool capture preserves frontend event");
            super::assert_epoch(&epoch, 0, true, false);
            assert_eq!(
                trace_lines(&cargo_trace)[cargo_before..],
                ["survival-cargo".to_owned(), "survival-cargo".to_owned()],
                "unrelated Cargo binding did not execute its two exact probes"
            );
            assert_eq!(
                Sha256Digest::of(&std::fs::read(&cargo_path).expect("stable Cargo wrapper bytes")),
                cargo_hash
            );
            let mut graph = GraphArtifact::new();
            let result = collect::frontend::collect(
                &fixture.manifest,
                &fixture.inventory,
                &fixture.captured,
                binding,
                &mut graph,
            );
            drop(scope);
            let error = result.expect_err("frontend node event");
            assert_eq!(
                error.to_string(),
                "INJECTED_FAILURE at real operation `frontend-node-spawn`"
            );
            super::assert_epoch(&epoch, 1, false, false);
            assert_eq!(
                trace_lines(trace)[node_before..],
                ["version".to_owned(), "version".to_owned()],
                "frontend event fired outside the exact post-verification boundary"
            );
            assert_eq!(
                trace_lines(trace),
                vec![
                    "version".to_owned(),
                    "version".to_owned(),
                    "version".to_owned(),
                    "version".to_owned(),
                    "version".to_owned(),
                    "version".to_owned(),
                    "helper".to_owned(),
                    "version".to_owned(),
                    "version".to_owned(),
                ],
                "Node trace omitted or collapsed an invocation"
            );
        },
    );
}

struct FrontendFixture {
    _roots: tempfile::TempDir,
    manifest: ConstellationManifest,
    repositories: Vec<ResolvedRepository>,
    inventory: Inventory,
    captured: xtask::architecture::inventory::CapturedInputs,
    binding: SourceBinding,
}

fn frontend_fixture() -> FrontendFixture {
    let roots = tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = materialize(&manifest, roots.path());
    std::fs::create_dir_all(repositories[0].root.join("src")).expect("src");
    std::fs::write(
        repositories[0].root.join("src/App.vue"),
        "<script src=\"./main.ts\"></script>\n<style>@import \"./base.css\";</style>\n",
    )
    .expect("Vue");
    std::fs::write(
        repositories[0].root.join("src/base.css"),
        "@import \"./theme.css\";\n",
    )
    .expect("CSS");
    let snapshot =
        Inventory::collect_snapshot(&manifest, &repositories).expect("captured inventory");
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace")
        .to_path_buf();
    let worktrees = vec![worktree("bbnf"), worktree("parse-that")];
    let nominations = resolve_nominations(&manifest, &worktrees).expect("nominations");
    let binding = SourceBinding::capture(BindingCapture {
        workspace_root: &workspace,
        manifest_hash: Sha256Digest::of(
            &std::fs::read(workspace.join("xtask/architecture/constellation-v3.toml"))
                .expect("manifest bytes"),
        ),
        selection: super::tool_selection(),
        manifest: &manifest,
        repositories: &repositories,
        inventory: &snapshot.inventory,
        collector_inputs: snapshot.inputs.bindings(),
        worktrees: &worktrees,
        worktree_hashing: Default::default(),
        nominations: &nominations,
    })
    .expect("binding");
    FrontendFixture {
        _roots: roots,
        manifest,
        repositories,
        inventory: snapshot.inventory,
        captured: snapshot.inputs,
        binding,
    }
}

fn fixture_binding(fixture: &FrontendFixture, selection: ToolSelection) -> SourceBinding {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace")
        .to_path_buf();
    let worktrees = vec![worktree("bbnf"), worktree("parse-that")];
    let nominations = resolve_nominations(&fixture.manifest, &worktrees).expect("nominations");
    SourceBinding::capture(BindingCapture {
        workspace_root: &workspace,
        manifest_hash: Sha256Digest::of(
            &std::fs::read(workspace.join("xtask/architecture/constellation-v3.toml"))
                .expect("manifest bytes"),
        ),
        selection,
        manifest: &fixture.manifest,
        repositories: &fixture.repositories,
        inventory: &fixture.inventory,
        collector_inputs: fixture.captured.bindings(),
        worktrees: &worktrees,
        worktree_hashing: Default::default(),
        nominations: &nominations,
    })
    .expect("fixture binding")
}

pub(super) fn materialize(
    manifest: &ConstellationManifest,
    root: &Path,
) -> Vec<ResolvedRepository> {
    let repositories = manifest
        .repositories
        .iter()
        .map(|repository| {
            let path = root.join(repository.id.as_str());
            std::fs::create_dir_all(&path).expect("repository");
            ResolvedRepository {
                id: repository.id.clone(),
                root: path,
            }
        })
        .collect::<Vec<_>>();
    let roots = repositories
        .iter()
        .map(|repository| (repository.id.clone(), repository.root.clone()))
        .collect::<BTreeMap<_, _>>();
    for face in &manifest.package_faces {
        std::fs::create_dir_all(roots[&face.repository].join(face.root.as_str()))
            .expect("face root");
        write_role(&roots[&face.repository], face.manifest.as_str());
    }
    for workspace in &manifest.workspaces {
        write_role(&roots[&workspace.repository], workspace.manifest.as_str());
    }
    for context in &manifest.contexts {
        write_role(&roots[&context.repository], context.manifest.as_str());
        write_role(&roots[&context.repository], context.lock.as_str());
        for path in &context.config_inputs {
            write_role(&roots[&context.repository], path.as_str());
        }
    }
    for document in &manifest.documentation {
        write_role(&roots[&document.repository], document.path.as_str());
    }
    for repository in &repositories {
        std::process::Command::new("git")
            .arg("-C")
            .arg(&repository.root)
            .arg("init")
            .output()
            .expect("git init");
    }
    repositories
}

fn write_role(root: &Path, relative: &str) {
    let path = root.join(relative);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("role parent");
    }
    std::fs::write(path, "{}\n").expect("role");
}

fn worktree(repository: &str) -> WorktreeRecord {
    WorktreeRecord {
        identity: WorktreeId::new(format!("{repository}-primary")).expect("identity"),
        repository: RepositoryId::new(repository).expect("repository"),
        administrative_key: CanonicalText::new("primary").expect("key"),
        head: CanonicalText::new("0123456789012345678901234567890123456789").expect("head"),
        branch: None,
        detached: false,
        bare: false,
        dirty: false,
        content_hash: Sha256Digest::of(b"worktree"),
        administrative_config_hash: Sha256Digest::of(b"admin-config"),
        ignored_content_policy: CanonicalText::new(
            "ignored-derived-bytes-excluded; collector-readable-bytes-source-bound",
        )
        .expect("policy"),
    }
}

struct FileRestore {
    path: PathBuf,
    bytes: Option<Vec<u8>>,
}

impl FileRestore {
    fn replace(path: &Path, replacement: &[u8]) -> Self {
        let bytes = std::fs::read(path).expect("captured live input");
        std::fs::write(path, replacement).expect("transient live mutation");
        Self {
            path: path.to_path_buf(),
            bytes: Some(bytes),
        }
    }

    fn restore(mut self) {
        let bytes = self.bytes.take().expect("pending live restoration");
        std::fs::write(&self.path, bytes).expect("restore live input");
    }
}

impl Drop for FileRestore {
    fn drop(&mut self) {
        if let Some(bytes) = self.bytes.take() {
            let _ = std::fs::write(&self.path, bytes);
        }
    }
}

fn with_wrapped_binding<T>(
    fixture: &FrontendFixture,
    capability: &str,
    response: &str,
    use_binding: impl FnOnce(&SourceBinding, &Path) -> T,
) -> T {
    let wrapper = tempdir().expect("wrapper");
    let node = wrapper.path().join("node");
    let trace = wrapper.path().join("trace");
    write_executable(
        &node,
        &format!(
            "#!/bin/sh\nif [ \"$1\" = \"--version\" ]; then\n  printf '%s\\n' version >> \"${{0%/*}}/trace\"\n  printf '%s\\n' '{}'\nelse\n  printf '%s\\n' helper >> \"${{0%/*}}/trace\"\n  printf '%s\\n' '{}'\nfi\n",
            capability.replace('\'', ""),
            response.replace('\'', "")
        ),
    );
    let selection = super::tool_selection();
    let selection = selection
        .replace_tool_for_counterexample("node", &node, &["--version"])
        .expect("wrapped Node selection");
    let binding = fixture_binding(fixture, selection);
    use_binding(&binding, &trace)
}

fn write_executable(path: &Path, bytes: &str) {
    use std::os::unix::fs::PermissionsExt as _;

    std::fs::write(path, bytes).expect("test executable");
    let mut permissions = std::fs::metadata(path).expect("metadata").permissions();
    permissions.set_mode(0o755);
    std::fs::set_permissions(path, permissions).expect("permissions");
}

fn trace_lines(path: &Path) -> Vec<String> {
    std::fs::read_to_string(path)
        .expect("wrapper trace")
        .lines()
        .map(str::to_string)
        .collect()
}
