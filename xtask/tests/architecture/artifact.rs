use xtask::architecture::collect;
use xtask::architecture::schema::{GraphArtifact, ObligationCode};
use xtask::architecture::test_support::RunEpoch;

#[test]
fn unsupported_artifact_planes_are_named_required_unknowns() {
    let mut graph = GraphArtifact::new();
    collect::artifact::collect(&mut graph).expect("artifact boundaries");
    let codes = graph
        .obligations()
        .into_iter()
        .map(|item| item.code)
        .collect::<std::collections::BTreeSet<_>>();
    for code in [
        ObligationCode::GeneratedLineageAbsent,
        ObligationCode::WasmPackageLineageIncomplete,
        ObligationCode::CallContextBoundaryUnattributed,
        ObligationCode::RelocatedConstellationBoundary,
        ObligationCode::PermutationTotalityBoundary,
    ] {
        assert!(codes.contains(&code));
    }
}

#[test]
fn f18_artifact_boundary_failpoint_is_operation_local() {
    let mut graph = GraphArtifact::new();
    let epoch = RunEpoch::for_test(Some("artifact-boundary"), None).expect("artifact epoch");
    let scope = epoch.scope().expect("artifact epoch scope");
    let error = collect::artifact::collect(&mut graph).expect_err("artifact boundary");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `artifact-boundary`"
    );
    super::assert_epoch(&epoch, 1, false, false);
}
