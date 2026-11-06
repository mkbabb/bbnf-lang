use xtask::architecture::manifest::ContextKind;
use xtask::architecture::schema::GraphArtifact;
use xtask::architecture::test_support::RunEpoch;

#[test]
fn four_cargo_contexts_have_distinct_workspace_and_lock_identity() {
    let manifest = super::manifest();
    let cargo = manifest
        .contexts
        .iter()
        .filter(|context| context.kind == ContextKind::Cargo)
        .collect::<Vec<_>>();
    assert_eq!(cargo.len(), 4);
    let identities = cargo
        .iter()
        .map(|context| (&context.id, &context.workspace, &context.lock))
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(identities.len(), 4);
}

#[test]
fn f18_cargo_boundary_failpoint_is_operation_local() {
    let manifest = super::manifest();
    let roots = tempfile::tempdir().expect("roots");
    let repositories = super::frontend::materialize(&manifest, roots.path());
    let mut graph = GraphArtifact::new();
    let epoch = RunEpoch::for_test(Some("cargo-boundary"), None).expect("Cargo epoch");
    let scope = epoch.scope().expect("Cargo epoch scope");
    let error = xtask::architecture::collect::cargo::collect(&manifest, &repositories, &mut graph)
        .expect_err("cargo boundary");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `cargo-boundary`"
    );
    super::assert_epoch(&epoch, 1, false, false);
}
