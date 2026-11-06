use super::*;

#[test]
#[ignore = "retained real-integration campaign; run explicitly"]
fn p3_retained_real_integration_campaign() {
    super::write::injected_early_failure_publishes_one_complete_generation();
    crate::project::p3_one_thousand_ordering_scenario();
    crate::frontend::p3_configured_frontend_captured_epoch_scenario();
}

#[test]
#[ignore = "residual F18 operation matrix; run explicitly as a campaign gate"]
fn f18_residual_operation_matrix_reaches_all_twenty_two_actual_call_sites() {
    const TOOL_OPERATIONS: [&str; 20] = [
        "tool-ar-spawn",
        "tool-ar-read",
        "tool-ar-decode",
        "tool-cc-spawn",
        "tool-cc-read",
        "tool-cc-decode",
        "tool-git-spawn",
        "tool-git-read",
        "tool-git-decode",
        "tool-ld-spawn",
        "tool-ld-read",
        "tool-ld-decode",
        "tool-node-read",
        "tool-node-decode",
        "tool-rustc-spawn",
        "tool-rustc-read",
        "tool-rustc-decode",
        "tool-sccache-spawn",
        "tool-sccache-read",
        "tool-sccache-decode",
    ];

    let mut terminal = 0;
    for operation in TOOL_OPERATIONS {
        let epoch = RunEpoch::for_test(Some(operation), None).expect("tool operation epoch");
        let scope = epoch.scope().expect("tool operation epoch scope");
        let error = ToolSelection::capture().expect_err(operation);
        drop(scope);
        assert_eq!(
            error.to_string(),
            format!("INJECTED_FAILURE at real operation `{operation}`"),
            "{error:#}"
        );
        crate::assert_epoch(&epoch, 1, false, false);
        terminal += 1;
    }

    let fixture = materialized_runtime_fixture();
    for (operation, stage) in [
        ("repository-resolution", PipelineStage::Manifest),
        ("final-recensus", PipelineStage::Recensus),
    ] {
        let epoch = RunEpoch::for_test(Some(operation), None).expect("runtime operation epoch");
        let output = fixture.roots.path().join(format!("matrix-{operation}"));
        run_at_with_epoch(&fixture.bbnf, &output, &epoch).expect_err(operation);
        crate::assert_epoch(&epoch, 1, false, false);

        let generation = output.join("generations").join(current_run_id(&output));
        let diagnostics: Vec<Diagnostic> = serde_json::from_slice(
            &fs::read(generation.join("diagnostics.json")).expect("matrix diagnostics"),
        )
        .expect("matrix diagnostics JSON");
        assert_eq!(diagnostics.len(), 1, "{operation}");
        assert_eq!(diagnostics[0].stage, stage, "{operation}");
        assert_eq!(
            diagnostics[0].message.as_str(),
            format!("INJECTED_FAILURE at real operation `{operation}`"),
            "{operation}"
        );
        assert!(diagnostics[0].chain.is_empty(), "{operation}");
        terminal += 1;
    }
    assert_eq!(terminal, 22);
}
