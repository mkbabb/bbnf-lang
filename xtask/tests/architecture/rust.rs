use xtask::architecture::collect;
use xtask::architecture::schema::{GraphArtifact, ObligationCode};
use xtask::architecture::test_support::RunEpoch;

#[test]
fn rust_counterexample_families_are_required_unknown_not_exact_edges() {
    let manifest = super::manifest();
    let mut graph = GraphArtifact::new();
    collect::rust::collect(&manifest, &mut graph).expect("Rust boundary collection");
    assert!(graph.edges().is_empty());
    let codes = graph
        .obligations()
        .into_iter()
        .map(|item| item.code)
        .collect::<std::collections::BTreeSet<_>>();
    for code in [
        ObligationCode::RustInlineModuleAttributionUnsound,
        ObligationCode::RustSelfPathFalseCycle,
        ObligationCode::RustPathPrefixGuessRejected,
        ObligationCode::RustExternRenameUnresolved,
        ObligationCode::RustCfgDiscoveryUnevaluated,
        ObligationCode::RustTestAttributionUnsound,
        ObligationCode::SourceSpanIncomplete,
        ObligationCode::RustExternalPackageUnlinked,
    ] {
        assert!(codes.contains(&code), "missing {code:?}");
    }
}

#[test]
fn f18_rust_boundary_failpoint_is_operation_local() {
    let mut graph = GraphArtifact::new();
    let epoch = RunEpoch::for_test(Some("rust-boundary"), None).expect("Rust epoch");
    let scope = epoch.scope().expect("Rust epoch scope");
    let error = collect::rust::collect(&super::manifest(), &mut graph).expect_err("Rust boundary");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `rust-boundary`"
    );
    super::assert_epoch(&epoch, 1, false, false);
}
