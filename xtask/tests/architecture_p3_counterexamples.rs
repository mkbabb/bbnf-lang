use std::collections::BTreeSet;
use std::path::PathBuf;

use serde::Deserialize;
use xtask::architecture::schema::Sha256Digest;

#[derive(Deserialize)]
struct History {
    schema: String,
    born_red_authority: Authority,
    repair_authorities: Vec<Authority>,
    historical_subject: HistoricalSubject,
    bank: Vec<BankRow>,
}

#[derive(Deserialize)]
struct Authority {
    path: String,
    sha256: String,
}

#[derive(Deserialize)]
struct BankRow {
    id: String,
    p31_test: String,
    normalized_input: String,
    expected_failure: String,
    actual_historical_result: String,
    mutation_inversion: String,
    source_digest: Option<String>,
    artifact_digest: String,
}

#[derive(Deserialize)]
struct HistoricalSubject {
    status: String,
    reason: String,
    source_digest: Option<String>,
    source_file_count: usize,
    source_line_count: usize,
    source_ledger_algorithm: String,
    recovery_base_commit: String,
    recovery_session: String,
    recovery_session_digest: String,
    historical_source_receipt: Authority,
    source_bank: SourceBank,
    claimed_historical_binary_digest: String,
    binary_digest: Option<String>,
}

#[derive(Deserialize)]
struct SourceBank {
    command: String,
    result: String,
    normalization: String,
    normalized_digest_run_1: String,
    normalized_digest_run_2: String,
    raw_log_digest_run_1: String,
    raw_log_digest_run_2: String,
    full_suite_result: String,
    full_suite_log_digest: String,
    format_result: String,
    cargo_lock_digest: String,
}

#[test]
fn exact_f01_f20_bank_is_bound_to_born_red_and_repair_authorities() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace")
        .to_path_buf();
    let fixture =
        std::fs::read(workspace.join("xtask/tests/fixtures/architecture/p31-history.json"))
            .expect("history fixture");
    let history: History = serde_json::from_slice(&fixture).expect("history schema");
    assert_eq!(history.schema, "bbnf.architecture.p31-history.v3");
    for authority in std::iter::once(&history.born_red_authority).chain(&history.repair_authorities)
    {
        let bytes = std::fs::read(workspace.join(&authority.path)).expect("bound authority");
        assert_eq!(Sha256Digest::of(&bytes).as_str(), authority.sha256);
    }
    let ids = history
        .bank
        .iter()
        .map(|row| row.id.as_str())
        .collect::<Vec<_>>();
    assert_eq!(
        ids,
        (1..=20)
            .map(|index| format!("F{index:02}"))
            .collect::<Vec<_>>()
    );
    assert_eq!(
        history.historical_subject.status,
        "source_and_bank_proven_binary_identity_unproven"
    );
    assert!(history.historical_subject.reason.contains("binary"));
    let historical_source = history
        .historical_subject
        .source_digest
        .as_deref()
        .expect("recovered exact source digest");
    assert_eq!(
        historical_source,
        "eb7c8829a240025585418a566af062b3a8b699faab1c7d0881afa86a490f334b"
    );
    assert_eq!(history.historical_subject.source_file_count, 56);
    assert_eq!(history.historical_subject.source_line_count, 5_548);
    assert!(
        history
            .historical_subject
            .source_ledger_algorithm
            .contains("sorted")
    );
    assert_eq!(
        history.historical_subject.recovery_base_commit,
        "af15f63e0d2d3d719938c13b906a50acbb92ea3b"
    );
    assert!(
        history
            .historical_subject
            .recovery_session
            .starts_with("codex-session:")
    );
    Sha256Digest::new(history.historical_subject.recovery_session_digest.clone())
        .expect("recovery session digest");
    let receipt = &history.historical_subject.historical_source_receipt;
    let receipt_bytes = std::fs::read(workspace.join(&receipt.path)).expect("P3 source receipt");
    assert_eq!(Sha256Digest::of(&receipt_bytes).as_str(), receipt.sha256);
    let bank = &history.historical_subject.source_bank;
    assert!(bank.command.contains("architecture_p3_counterexamples"));
    assert_eq!(bank.result, "10 passed; 0 failed; run twice");
    assert!(bank.normalization.contains("<elapsed>"));
    assert_eq!(
        bank.normalized_digest_run_1,
        "265553a9ecbb78d4747979d91e1ee1bbcf39a8017554bb0f8ac57e6814ec7e18"
    );
    assert_eq!(bank.normalized_digest_run_1, bank.normalized_digest_run_2);
    for digest in [&bank.raw_log_digest_run_1, &bank.raw_log_digest_run_2] {
        Sha256Digest::new(digest.clone()).expect("historical raw log digest");
    }
    assert_eq!(bank.full_suite_result, "26 passed; 0 failed");
    Sha256Digest::new(bank.full_suite_log_digest.clone()).expect("historical full-suite log");
    assert_eq!(bank.format_result, "GREEN");
    assert_eq!(
        bank.cargo_lock_digest,
        "a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908"
    );
    assert_eq!(
        history.historical_subject.claimed_historical_binary_digest,
        "a83502242adbc8336d4e21229a17c3d931a587fa67a542547904ff284e078cb8"
    );
    assert!(history.historical_subject.binary_digest.is_none());
    let test_sources = [
        "tests/architecture/evidence.rs",
        "tests/architecture/frontend.rs",
        "tests/architecture/inventory.rs",
        "tests/architecture/manifest.rs",
        "tests/architecture/project.rs",
        "tests/architecture/schema.rs",
        "tests/architecture/worktree.rs",
    ]
    .into_iter()
    .map(|path| std::fs::read_to_string(workspace.join("xtask").join(path)).expect("test source"))
    .collect::<Vec<_>>();
    let mut named = BTreeSet::new();
    for row in &history.bank {
        assert!(named.insert(row.p31_test.as_str()) || row.id == "F04");
        assert!(
            test_sources
                .iter()
                .any(|source| source.contains(&format!("fn {}", row.p31_test))),
            "{} is not routed through its owning P3.1 test",
            row.id
        );
        assert_eq!(row.normalized_input, row.p31_test);
        assert!(!row.expected_failure.is_empty());
        assert!(row.actual_historical_result.starts_with("UNPROVEN:"));
        assert!(row.actual_historical_result.contains("binary"));
        assert!(row.mutation_inversion.contains(&row.p31_test));
        assert_eq!(row.source_digest.as_deref(), Some(historical_source));
        assert_eq!(
            row.artifact_digest, history.born_red_authority.sha256,
            "{} must bind the only retained historical artifact",
            row.id
        );
    }
}
