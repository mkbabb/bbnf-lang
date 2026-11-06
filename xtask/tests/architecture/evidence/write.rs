use super::*;

pub(super) fn injected_early_failure_publishes_one_complete_generation() {
    let output = tempdir().expect("output");
    let status = Command::new(env!("CARGO_BIN_EXE_xtask"))
        .args(["architecture", "--output"])
        .arg(output.path())
        .env("BBNF_ARCHITECTURE_FAIL_STAGE", "manifest")
        .status()
        .expect("run xtask");
    assert!(!status.success());
    let generations = output.path().join("generations");
    let entries = fs::read_dir(&generations)
        .expect("generations")
        .map(|entry| entry.expect("generation entry"))
        .collect::<Vec<_>>();
    assert_eq!(entries.len(), 1, "expected exactly one generation");
    assert!(entries[0].file_type().expect("generation type").is_dir());
    let run_id = entries[0]
        .file_name()
        .into_string()
        .expect("portable run ID");
    let generation = generations.join(&run_id);

    let publication_bytes = fs::read(generation.join("publication.json")).expect("publication");
    let current: CurrentPointer =
        serde_json::from_slice(&fs::read(output.path().join("CURRENT")).expect("CURRENT"))
            .expect("typed CURRENT");
    assert_eq!(current.schema.as_str(), "bbnf.architecture.current.v3.1");
    assert_eq!(current.run_id.as_str(), run_id);
    assert_eq!(current.publication_size, publication_bytes.len() as u64);
    assert_eq!(
        current.publication_hash,
        Sha256Digest::of(&publication_bytes)
    );
    assert_eq!(run_id, format!("p31-{}", current.publication_hash));

    let publication: PublicationReceipt =
        serde_json::from_slice(&publication_bytes).expect("publication receipt");
    let summary: GenerationSummary = serde_json::from_slice(
        &fs::read(generation.join("generation.json")).expect("generation summary"),
    )
    .expect("generation summary JSON");
    let binding_bytes = fs::read(generation.join("binding.json")).expect("binding");
    let binding: SourceBinding = serde_json::from_slice(&binding_bytes).expect("typed binding");
    let expected_diagnostics = vec![Diagnostic {
        stage: PipelineStage::Manifest,
        message: CanonicalText::new("INJECTED_FAILURE at typed stage Manifest")
            .expect("diagnostic"),
        chain: Vec::new(),
    }];
    assert_eq!(
        publication.schema.as_str(),
        "bbnf.architecture.generation.v3.1"
    );
    assert_eq!(publication.status, GenerationStatus::Failure);
    assert_eq!(publication.stage, PipelineStage::Manifest);
    assert_eq!(publication.source_binding, binding.binding_hash);
    assert_eq!(
        publication.movement_authorization.status,
        MovementAuthorizationStatus::Denied
    );
    assert_eq!(publication.diagnostics, expected_diagnostics);
    assert_eq!(summary.schema, publication.schema);
    assert_eq!(summary.status, publication.status);
    assert_eq!(summary.stage, publication.stage);
    assert_eq!(summary.source_binding, publication.source_binding);
    assert_eq!(
        summary.movement_authorization,
        publication.movement_authorization
    );
    assert_eq!(summary.diagnostics, expected_diagnostics);
    let diagnostics: Vec<Diagnostic> = serde_json::from_slice(
        &fs::read(generation.join("diagnostics.json")).expect("diagnostics"),
    )
    .expect("typed diagnostics");
    assert_eq!(diagnostics, expected_diagnostics);
    let obligations: Vec<xtask::architecture::schema::RequiredFailure> = serde_json::from_slice(
        &fs::read(generation.join("obligations.json")).expect("obligations"),
    )
    .expect("typed obligations");
    assert!(obligations.is_empty());

    let available = [
        "binding.json",
        "diagnostics.json",
        "generation.json",
        "obligations.json",
        "publication.json",
    ]
    .into_iter()
    .collect::<BTreeSet<_>>();
    assert_eq!(
        publication
            .expected_files
            .iter()
            .map(CanonicalText::as_str)
            .collect::<BTreeSet<_>>(),
        available
    );
    assert_eq!(
        publication
            .artifacts
            .keys()
            .map(CanonicalText::as_str)
            .collect::<BTreeSet<_>>(),
        ARTIFACT_ROLES.into_iter().collect()
    );
    for (name, artifact) in &publication.artifacts {
        let name = name.as_str();
        let path = generation.join(name);
        match artifact {
            ArtifactPublication::Published { size, hash } => {
                assert!(available.contains(name));
                let bytes = fs::read(&path).expect("published partial member");
                assert_eq!(*size, bytes.len() as u64, "{name} size");
                assert_eq!(*hash, Sha256Digest::of(&bytes), "{name} hash");
            }
            ArtifactPublication::Unavailable { stage, reason } => {
                assert!(!available.contains(name));
                assert_eq!(*stage, PipelineStage::Manifest);
                assert_eq!(reason.as_str(), "stage did not produce this typed artifact");
                assert!(!path.exists(), "unavailable {name} was published");
            }
            ArtifactPublication::PublicationRoot => {
                assert_eq!(name, "publication.json");
                assert!(path.is_file());
            }
        }
    }
    assert_eq!(
        fs::read_dir(&generation)
            .expect("generation members")
            .map(|entry| {
                entry
                    .expect("generation member")
                    .file_name()
                    .into_string()
                    .expect("portable member")
            })
            .collect::<BTreeSet<_>>(),
        available.into_iter().map(str::to_owned).collect()
    );
    assert_eq!(
        fs::read_dir(output.path())
            .expect("publication root")
            .map(|entry| {
                entry
                    .expect("publication root member")
                    .file_name()
                    .into_string()
                    .expect("portable root member")
            })
            .collect::<BTreeSet<_>>(),
        [".publication.lock", "CURRENT", "generations"]
            .into_iter()
            .map(str::to_owned)
            .collect()
    );
    assert!(
        fs::read_dir(&generations)
            .expect("generations")
            .all(|entry| !entry
                .expect("generation entry")
                .file_name()
                .to_string_lossy()
                .starts_with(".stage-"))
    );
    assert!(!output.path().join("quarantine").exists());
}

#[test]
fn f18_one_shot_complete_write_failure_publishes_a_clean_failure_generation() {
    let fixture = materialized_runtime_fixture();
    let output = fixture.roots.path().join("output");
    let epoch = RunEpoch::for_test(Some("file-create"), None).expect("file-create epoch");
    let error = run_at_with_epoch(&fixture.bbnf, &output, &epoch).expect_err("safe RED result");
    crate::assert_epoch(&epoch, 1, false, false);

    let generations = output.join("generations");
    let generation_entries = fs::read_dir(&generations)
        .expect("generations")
        .map(|entry| entry.expect("generation entry"))
        .collect::<Vec<_>>();
    assert_eq!(
        generation_entries.len(),
        1,
        "expected exactly one generation"
    );
    assert!(
        generation_entries[0]
            .file_type()
            .expect("file type")
            .is_dir()
    );
    let run_id = generation_entries[0]
        .file_name()
        .into_string()
        .expect("portable run ID");
    assert!(run_id.starts_with("p31-"));
    let generation = generations.join(&run_id);
    assert_eq!(
        error.to_string(),
        format!(
            "architecture P3.1 failed at Write; stage-safe failure generation {run_id} published"
        )
    );

    let publication_bytes = fs::read(generation.join("publication.json")).expect("publication");
    let current: CurrentPointer =
        serde_json::from_slice(&fs::read(output.join("CURRENT")).expect("CURRENT"))
            .expect("typed CURRENT");
    assert_eq!(current.schema.as_str(), "bbnf.architecture.current.v3.1");
    assert_eq!(current.run_id.as_str(), run_id);
    assert_eq!(current.publication_size, publication_bytes.len() as u64);
    assert_eq!(
        current.publication_hash,
        Sha256Digest::of(&publication_bytes)
    );
    assert_eq!(run_id, format!("p31-{}", current.publication_hash));

    let publication: PublicationReceipt =
        serde_json::from_slice(&publication_bytes).expect("publication receipt");
    assert_eq!(
        publication.schema.as_str(),
        "bbnf.architecture.generation.v3.1"
    );
    assert_eq!(publication.status, GenerationStatus::Failure);
    assert_eq!(publication.stage, PipelineStage::Write);
    assert_eq!(
        publication.movement_authorization.status,
        MovementAuthorizationStatus::Denied
    );
    let expected_artifacts = ARTIFACT_ROLES
        .into_iter()
        .map(str::to_owned)
        .collect::<BTreeSet<_>>();
    assert_eq!(
        publication
            .artifacts
            .keys()
            .map(|name| name.as_str().to_owned())
            .collect::<BTreeSet<_>>(),
        expected_artifacts
    );
    let expected_files = publication
        .expected_files
        .iter()
        .map(|name| name.as_str().to_owned())
        .collect::<BTreeSet<_>>();
    assert_eq!(expected_files.len(), publication.expected_files.len());
    let exact_files = expected_artifacts
        .iter()
        .filter(|name| name.as_str() != "candidate-graph.json")
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_eq!(expected_files, exact_files);
    let actual_files = fs::read_dir(&generation)
        .expect("generation members")
        .map(|entry| {
            entry
                .expect("generation member")
                .file_name()
                .into_string()
                .expect("portable member")
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(actual_files, expected_files);
    for (name, artifact) in &publication.artifacts {
        let path = generation.join(name.as_str());
        match artifact {
            ArtifactPublication::Published { size, hash } => {
                let bytes = fs::read(&path).expect("published member");
                assert_eq!(*size, bytes.len() as u64, "{} size", name.as_str());
                assert_eq!(*hash, Sha256Digest::of(&bytes), "{} hash", name.as_str());
            }
            ArtifactPublication::Unavailable { .. } => {
                assert!(
                    !path.exists(),
                    "unavailable {} was published",
                    name.as_str()
                );
            }
            ArtifactPublication::PublicationRoot => {
                assert_eq!(name.as_str(), "publication.json");
                assert!(path.is_file());
            }
        }
    }

    let expected_diagnostics = vec![Diagnostic {
        stage: PipelineStage::Write,
        message: CanonicalText::new("INJECTED_FAILURE at real operation `file-create`")
            .expect("diagnostic"),
        chain: Vec::new(),
    }];
    let diagnostics: Vec<Diagnostic> = serde_json::from_slice(
        &fs::read(generation.join("diagnostics.json")).expect("diagnostics"),
    )
    .expect("diagnostics JSON");
    let summary: GenerationSummary = serde_json::from_slice(
        &fs::read(generation.join("generation.json")).expect("generation summary"),
    )
    .expect("generation summary JSON");
    assert_eq!(diagnostics, expected_diagnostics);
    assert_eq!(publication.diagnostics, expected_diagnostics);
    assert_eq!(summary.diagnostics, expected_diagnostics);
    assert_eq!(summary.schema, publication.schema);
    assert_eq!(summary.status, publication.status);
    assert_eq!(summary.stage, publication.stage);
    assert_eq!(summary.source_binding, publication.source_binding);
    assert_eq!(
        summary.movement_authorization,
        publication.movement_authorization
    );

    assert_eq!(
        fs::read_dir(&output)
            .expect("publication root")
            .map(|entry| {
                entry
                    .expect("publication root member")
                    .file_name()
                    .into_string()
                    .expect("portable root member")
            })
            .collect::<BTreeSet<_>>(),
        [".publication.lock", "CURRENT", "generations"]
            .into_iter()
            .map(str::to_owned)
            .collect()
    );
    assert!(
        fs::read_dir(&generations)
            .expect("generations")
            .all(|entry| !entry
                .expect("generation entry")
                .file_name()
                .to_string_lossy()
                .starts_with(".stage-"))
    );
    assert!(!output.join("quarantine").exists());
}

#[test]
fn f18_full_pipeline_cycle_and_cluster_failures_publish_exact_stage_attribution() {
    let fixture = materialized_runtime_fixture();
    for (requested, stage, context) in [
        ("cycle", PipelineStage::Cycle, "PIPELINE_STAGE_CYCLE"),
        ("cluster", PipelineStage::Cluster, "PIPELINE_STAGE_CLUSTER"),
    ] {
        let output = fixture
            .roots
            .path()
            .join(format!("full-pipeline-{requested}"));
        let epoch = RunEpoch::for_test(None, Some(requested)).expect("full-pipeline epoch");
        let error = run_at_with_epoch(&fixture.bbnf, &output, &epoch)
            .expect_err("typed stage failure must retain a safe publication");
        crate::assert_epoch(&epoch, 1, false, false);

        let run_id = current_run_id(&output);
        assert_eq!(
            error.to_string(),
            format!(
                "architecture P3.1 failed at {stage:?}; stage-safe failure generation {run_id} published"
            )
        );
        let generation = output.join("generations").join(&run_id);
        let publication: PublicationReceipt = serde_json::from_slice(
            &fs::read(generation.join("publication.json")).expect("publication"),
        )
        .expect("publication JSON");
        let summary: GenerationSummary = serde_json::from_slice(
            &fs::read(generation.join("generation.json")).expect("generation summary"),
        )
        .expect("generation summary JSON");
        let diagnostics: Vec<Diagnostic> = serde_json::from_slice(
            &fs::read(generation.join("diagnostics.json")).expect("diagnostics"),
        )
        .expect("diagnostics JSON");
        let expected = vec![Diagnostic {
            stage,
            message: CanonicalText::new(context).expect("pipeline context"),
            chain: vec![
                CanonicalText::new(format!("INJECTED_FAILURE at typed stage {stage:?}"))
                    .expect("typed injected cause"),
            ],
        }];

        assert_eq!(publication.status, GenerationStatus::Failure);
        assert_eq!(publication.stage, stage);
        assert_eq!(summary.status, GenerationStatus::Failure);
        assert_eq!(summary.stage, stage);
        assert_eq!(diagnostics, expected);
        assert_eq!(publication.diagnostics, diagnostics);
        assert_eq!(summary.diagnostics, diagnostics);
    }
}

#[test]
fn f03_and_f04_corrupt_existing_generations_are_verified_quarantined_and_rebuilt() {
    let output = tempdir().expect("output");
    let manifest = output.path().join("manifest.toml");
    fs::write(&manifest, "fixture = 1\n").expect("manifest");
    let binding = SourceBinding::minimal(&manifest).expect("binding");
    let first = publish_minimal(output.path(), &binding);
    let generation = output
        .path()
        .join("generations")
        .join(first.run_id.as_str());

    fs::remove_file(generation.join("diagnostics.json")).expect("remove artifact");
    let replay = publish_minimal(output.path(), &binding);
    assert_eq!(first.run_id, replay.run_id);
    assert!(generation.join("diagnostics.json").is_file());

    fs::write(generation.join("binding.json"), b"corrupt\n").expect("corrupt");
    fs::write(generation.join("extra.json"), b"{}\n").expect("extra");
    let replay = publish_minimal(output.path(), &binding);
    assert_eq!(first.run_id, replay.run_id);
    assert!(!generation.join("extra.json").exists());
    assert_eq!(current_run_id(output.path()), first.run_id.as_str());
    assert!(
        fs::read_dir(output.path().join("quarantine"))
            .expect("quarantine")
            .count()
            >= 2
    );
}

#[test]
fn f18_real_publication_failpoints_preserve_the_previous_current_generation() {
    for failpoint in [
        "file-create",
        "file-write",
        "file-fsync",
        "staging-directory-fsync",
        "publication-lock",
        "existing-generation-verify",
        "generation-rename",
        "generations-directory-fsync",
        "current-create",
        "current-write",
        "current-fsync",
        "current-replace",
        "root-directory-fsync",
    ] {
        let output = tempdir().expect("output");
        let first_manifest = output.path().join("first.toml");
        let next_manifest = output.path().join("next.toml");
        fs::write(&first_manifest, "fixture = 1\n").expect("first");
        fs::write(&next_manifest, "fixture = 2\n").expect("next");
        let first_binding = SourceBinding::minimal(&first_manifest).expect("first binding");
        let next_binding = SourceBinding::minimal(&next_manifest).expect("next binding");
        let first = publish_minimal(output.path(), &first_binding);
        let old_current = fs::read(output.path().join("CURRENT")).expect("CURRENT");
        let binding = if failpoint == "existing-generation-verify" {
            &first_binding
        } else {
            &next_binding
        };
        let epoch = RunEpoch::for_test(Some(failpoint), None).expect("publication epoch");
        let scope = epoch.scope().expect("publication epoch scope");
        let error = try_publish_minimal(output.path(), binding).expect_err(failpoint);
        drop(scope);
        assert_eq!(
            error.to_string(),
            format!("INJECTED_FAILURE at real operation `{failpoint}`"),
            "{error:#}"
        );
        crate::assert_epoch(&epoch, 1, false, false);
        assert_eq!(
            fs::read(output.path().join("CURRENT")).expect("CURRENT after failure"),
            old_current,
            "{failpoint} changed CURRENT"
        );
        assert_eq!(current_run_id(output.path()), first.run_id.as_str());
        assert!(
            fs::read_dir(output.path())
                .expect("output")
                .all(|entry| !entry
                    .expect("entry")
                    .file_name()
                    .to_string_lossy()
                    .starts_with(".CURRENT-")),
            "{failpoint} left a CURRENT temporary"
        );
    }
}

#[test]
fn f18_same_run_durability_failures_never_restore_known_corrupt_current_bytes() {
    for failpoint in ["generations-directory-fsync", "root-directory-fsync"] {
        let output = tempdir().expect("output");
        let manifest = output.path().join("manifest.toml");
        fs::write(&manifest, "fixture = 1\n").expect("manifest");
        let binding = SourceBinding::minimal(&manifest).expect("binding");
        let first = publish_minimal(output.path(), &binding);
        let directory = output
            .path()
            .join("generations")
            .join(first.run_id.as_str());
        fs::write(directory.join("binding.json"), b"old-corrupt-target\n").expect("corrupt");
        let epoch = RunEpoch::for_test(Some(failpoint), None).expect("durability epoch");
        let scope = epoch.scope().expect("durability epoch scope");
        let error = try_publish_minimal(output.path(), &binding).expect_err(failpoint);
        drop(scope);
        assert_eq!(
            error.to_string(),
            format!("INJECTED_FAILURE at real operation `{failpoint}`")
        );
        crate::assert_epoch(&epoch, 1, false, false);
        assert!(
            !output.path().join("CURRENT").exists(),
            "{failpoint} restored a pointer to known-corrupt bytes"
        );
        assert!(
            fs::read_dir(output.path().join("quarantine"))
                .expect("quarantine")
                .next()
                .is_some(),
            "{failpoint} discarded the untrusted quarantine"
        );
    }
}

#[test]
fn f18_global_validation_failpoint_is_operation_local() {
    let manifest_path = crate::manifest_path();
    let manifest = crate::manifest();
    let binding = SourceBinding::minimal(&manifest_path).expect("binding");
    let inventory = Inventory {
        schema: "bbnf.architecture.inventory.v3.1".into(),
        records: Vec::new(),
    };
    let mut graph = synthetic_graph();
    let projections = project(&graph).expect("projections");
    let epoch =
        RunEpoch::for_test(Some("global-validation"), None).expect("global validation epoch");
    let scope = epoch.scope().expect("global validation epoch scope");
    let error = validate_all(&manifest, &inventory, &binding, &mut graph, &projections)
        .expect_err("global validation");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `global-validation`"
    );
    crate::assert_epoch(&epoch, 1, false, false);
}

#[test]
fn f20_complete_payload_publishes_hash_bound_json_dot_report_basis_quotient_and_shards() {
    let output = tempdir().expect("output");
    let manifest = output.path().join("manifest.toml");
    fs::write(&manifest, "fixture = 1\n").expect("manifest");
    let binding = SourceBinding::minimal(&manifest).expect("binding");
    let graph = synthetic_graph();
    let projections = project(&graph).expect("projections");
    let failures = graph.required_failures();
    let generation = publish(
        output.path(),
        EvidencePayload {
            binding: &binding,
            manifest: None,
            inventory: None,
            graph: Some(&graph),
            graph_validated: true,
            projections: Some(&projections),
            failures: Some(&failures),
            diagnostics: Vec::new(),
            stage: PipelineStage::Complete,
            status: GenerationStatus::Red,
        },
    )
    .expect("publish");
    assert_eq!(
        generation.movement_authorization.status,
        MovementAuthorizationStatus::Denied
    );
    let directory = output
        .path()
        .join("generations")
        .join(generation.run_id.as_str());
    for name in [
        "graph.json",
        "projections.json",
        "graph.dot",
        "report.txt",
        "condensation.json",
        "basis.json",
        "quotient.json",
        "shards.json",
        "semantic-ledger.json",
        "binding.json",
        "obligations.json",
        "generation.json",
        "publication.json",
    ] {
        assert!(directory.join(name).is_file(), "missing {name}");
    }
    for (name, publication) in &generation.artifacts {
        if let ArtifactPublication::Published { size, hash } = publication {
            let bytes = fs::read(directory.join(name.as_str())).expect("artifact");
            assert_eq!(*size, bytes.len() as u64);
            assert_eq!(*hash, Sha256Digest::of(&bytes));
        }
    }
    assert!(generation.expected_files.iter().all(|name| {
        matches!(
            generation.artifacts.get(name),
            Some(ArtifactPublication::Published { .. } | ArtifactPublication::PublicationRoot)
        )
    }));
    assert!(matches!(
        generation
            .artifacts
            .get(&CanonicalText::new("generation.json").expect("name")),
        Some(ArtifactPublication::Published { .. })
    ));
}
