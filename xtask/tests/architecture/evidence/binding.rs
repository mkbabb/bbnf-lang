use super::*;

#[cfg(target_os = "macos")]
#[path = "binding/apple.rs"]
mod apple;

fn expected_runtime_fact(name: &str, value: impl Into<std::ffi::OsString>) -> String {
    let runtime = BTreeMap::from([
        ("BBNF_ARCHITECTURE_FAILPOINT".to_owned(), None),
        ("BBNF_ARCHITECTURE_FAIL_STAGE".to_owned(), None),
        ("BBNF_ARCHITECTURE_WORKTREE_THREADS".to_owned(), None),
        ("PATH".to_owned(), None),
    ]);
    let mut runtime = runtime;
    runtime.insert(name.to_owned(), Some(value.into()));
    let prefix = format!("runtime-input:{name}:sha256:");
    xtask::architecture::test_support::exporter_execution_contract_for_test(&runtime)
        .expect("direct runtime contract")
        .into_iter()
        .find(|fact| fact.as_str().starts_with(&prefix))
        .expect("bound runtime fact")
        .as_str()
        .to_owned()
}

#[cfg(target_os = "macos")]
fn independently_selected_xcode_paths() -> [std::path::PathBuf; 3] {
    use std::os::unix::ffi::OsStringExt as _;

    let run = |program, arguments: &[&str], developer: Option<&std::path::Path>| {
        let mut command = Command::new("/usr/bin/arch");
        command.env_clear();
        command.envs([("LC_ALL", "C"), ("TZ", "UTC")]);
        command.args(["-arm64", program]).args(arguments);
        if let Some(developer) = developer {
            command.env("DEVELOPER_DIR", developer);
        }
        let output = command.output().expect("independent Apple ARM observation");
        assert!(output.status.success());
        let mut record = output.stdout;
        assert_eq!(record.pop(), Some(b'\n'));
        assert!(!record.is_empty() && !record.contains(&b'\n') && !record.contains(&0));
        fs::canonicalize(std::path::PathBuf::from(std::ffi::OsString::from_vec(
            record,
        )))
        .expect("canonical independent Apple path")
    };
    let developer = run("/usr/bin/xcode-select", &["-p"], None);
    ["ar", "clang", "ld"].map(|tool| run("/usr/bin/xcrun", &["--find", tool], Some(&developer)))
}

#[test]
fn bound_os_inputs_preserve_exact_non_utf8_identity() {
    use std::collections::BTreeMap;
    use std::os::unix::ffi::OsStringExt;

    use xtask::architecture::test_support::{
        bound_path_hash_for_test, exporter_execution_contract_for_test,
    };

    let values = [
        std::ffi::OsString::from_vec(vec![b'r', b'a', b'w', 0xff]),
        std::ffi::OsString::from_vec(vec![b'r', b'a', b'w', 0xfe]),
    ];
    assert_eq!(values[0].to_string_lossy(), values[1].to_string_lossy());
    let contract = |path: &std::ffi::OsString| {
        exporter_execution_contract_for_test(&BTreeMap::from([
            ("BBNF_ARCHITECTURE_FAILPOINT".to_owned(), None),
            ("BBNF_ARCHITECTURE_FAIL_STAGE".to_owned(), None),
            ("BBNF_ARCHITECTURE_WORKTREE_THREADS".to_owned(), None),
            ("PATH".to_owned(), Some(path.clone())),
        ]))
        .expect("direct runtime contract")
    };
    assert_ne!(
        contract(&values[0]),
        contract(&values[1]),
        "PATH runtime binding must preserve exact OS bytes"
    );

    let paths = values.map(std::path::PathBuf::from);
    assert_ne!(
        bound_path_hash_for_test(&paths[0]),
        bound_path_hash_for_test(&paths[1]),
        "invocation/effective path binding must preserve exact OS bytes"
    );
}

#[test]
fn source_and_build_input_path_owners_reject_lossy_collisions() {
    use std::os::unix::ffi::OsStringExt;

    let root = tempfile::tempdir().expect("repository root");
    for owner in [
        xtask::architecture::evidence::binding::source_member_repo_path_for_test,
        xtask::architecture::evidence::binding::build_input_repo_path_for_test,
    ] {
        assert_eq!(
            owner(root.path(), &root.path().join("éclat.rs"))
                .expect("Unicode path")
                .as_str(),
            "éclat.rs"
        );
        let invalid = [
            std::ffi::OsString::from_vec(vec![b'n', 0xff]),
            std::ffi::OsString::from_vec(vec![b'n', 0xfe]),
        ];
        assert_eq!(invalid[0].to_string_lossy(), invalid[1].to_string_lossy());
        for invalid in invalid {
            assert!(owner(root.path(), &root.path().join(invalid)).is_err());
        }
    }
}

#[test]
fn f13_missing_typed_context_input_makes_binding_incomplete_and_wrong_plane_manifest_fails() {
    let xtask_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = xtask_root.parent().expect("workspace");
    let manifest_path = workspace.join("xtask/architecture/constellation-v3.toml");
    let manifest = crate::manifest();
    let repositories = manifest
        .resolve_repositories(workspace)
        .expect("repositories");
    let missing = &manifest.contexts[0].lock;
    let mut records = Vec::new();
    for context in &manifest.contexts {
        for path in std::iter::once(&context.manifest)
            .chain(std::iter::once(&context.lock))
            .chain(&context.config_inputs)
        {
            if path == missing && context.repository == manifest.contexts[0].repository {
                continue;
            }
            records.push(InventoryRecord {
                identity: PhysicalIdentity {
                    repository: context.repository.clone(),
                    path: path.clone(),
                    kind: PhysicalKind::File,
                },
                content_hash: Some(Sha256Digest::of(path.as_str().as_bytes())),
                semantic_faces: Vec::new(),
                exclusion: None,
            });
        }
    }
    records.sort();
    records.dedup_by(|left, right| left.identity == right.identity);
    let inventory = Inventory {
        schema: "bbnf.architecture.inventory.v3.1".into(),
        records,
    };
    let worktrees = vec![
        dummy_worktree("bbnf"),
        dummy_worktree("parse-that"),
        dummy_worktree("pprint"),
    ];
    let nominations = resolve_nominations(&manifest, &worktrees).expect("nominations");
    let binding = SourceBinding::capture(BindingCapture {
        workspace_root: workspace,
        manifest_hash: Sha256Digest::of(&std::fs::read(&manifest_path).expect("manifest bytes")),
        selection: crate::tool_selection(),
        manifest: &manifest,
        repositories: &repositories,
        inventory: &inventory,
        collector_inputs: Vec::new(),
        worktrees: &worktrees,
        worktree_hashing: Default::default(),
        nominations: &nominations,
    })
    .expect("binding");
    let cargo = binding
        .tools
        .iter()
        .find(|tool| tool.tool.as_str() == "cargo")
        .expect("Cargo binding");
    let rustc = binding
        .tools
        .iter()
        .find(|tool| tool.tool.as_str() == "rustc")
        .expect("rustc binding");
    assert!(cargo.capability.as_str().starts_with("cargo "));
    assert!(rustc.capability.as_str().starts_with("rustc "));
    assert_ne!(
        cargo.executable_hash, rustc.executable_hash,
        "Cargo and rustc must identify their distinct effective payloads"
    );
    assert_eq!(
        cargo.toolchain_selection, rustc.toolchain_selection,
        "Cargo and rustc must bind the same active rustup selection"
    );
    let exporters = binding
        .tools
        .iter()
        .filter(|tool| tool.tool.as_str() == "xtask-exporter")
        .collect::<Vec<_>>();
    assert_eq!(exporters.len(), 1, "executing exporter binding is unique");
    let exporter = exporters[0];
    let build = binding.build.as_ref().expect("exporter build receipt");
    assert_eq!(build.exporter_hash, exporter.executable_hash);
    assert_eq!(build.source_ledger_hash, binding.source.ledger_hash);
    for path in [
        ".cargo/config.toml",
        ".config/nextest.toml",
        ".github/workflows/ci.yml",
        ".rustfmt.toml",
        "Cargo.lock",
        "Makefile",
        "restart/skinny/tranches/sk-v25/CAMPAIGN-STATE.md",
        "restart/skinny/tranches/sk-v25/HANDOFF.md",
        "restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md",
        "restart/skinny/tranches/sk-v25/RUN-LEDGER.md",
        "restart/skinny/tranches/sk-v25/graph/P3-ADJUDICATION.md",
        "restart/skinny/tranches/sk-v25/graph/P3-EVIDENCE.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-GESTALT-2-ADJUDICATION.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-ASSAY-PROTOCOL.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-S2-RETIREMENT.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-S2-LEDGER.tsv",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-SUCCESSOR-AUTHORITY.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-RESOURCE-SUCCESSOR-ADJUDICATION.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-RESOURCE-SUCCESSOR-LEDGER.tsv",
        "scripts/test-tier.sh",
        "xtask/tests/architecture/resource_successor.rs",
        "xtask/tests/architecture_feature_gate.rs",
    ] {
        assert!(
            binding
                .source
                .members
                .iter()
                .any(|member| member.path.as_str() == path),
            "missing architecture source member {path}"
        );
    }
    assert!(!binding.complete);
    assert!(
        binding
            .unavailable
            .iter()
            .any(|item| { item.as_str() == format!("context:{}:lock", manifest.contexts[0].id) })
    );

    let mut wrong_plane = manifest;
    let frontend = wrong_plane
        .contexts
        .iter_mut()
        .find(|context| context.kind == xtask::architecture::manifest::ContextKind::Frontend)
        .expect("frontend");
    frontend.manifest = RepoPath::new("Cargo.toml").expect("path");
    let error = wrong_plane.validate().expect_err("wrong plane manifest");
    assert!(error.to_string().contains("CONTEXT_MANIFEST_ROLE_MISMATCH"));
}

#[test]
fn f14_final_recensus_detects_any_binding_drift() {
    let output = tempdir().expect("output");
    let manifest = output.path().join("manifest.toml");
    fs::write(&manifest, "fixture = 1\n").expect("manifest");
    let binding = SourceBinding::minimal(&manifest).expect("binding");
    let mut changed = binding.clone();
    changed.manifest_hash = Sha256Digest::of(b"changed input");
    let error = binding.verify_recensus(&changed).expect_err("drift");
    assert!(error.to_string().contains("SOURCE_SNAPSHOT_DRIFT"));
}

#[test]
#[ignore = "full final-recensus semantic oracle; run explicitly as a campaign gate"]
fn f14_final_recensus_semantic_conformance_matches_serial_oracle() {
    for case in ["normal", "failpoint", "fail-stage"] {
        let output = Command::new(std::env::current_exe().expect("test executable"))
            .args([
                "--ignored",
                "--exact",
                "evidence::binding::f14_final_recensus_serial_oracle_child",
                "--show-output",
            ])
            .env("BBNF_TEST_RECENSUS_CASE", case)
            .env_remove("BBNF_ARCHITECTURE_FAILPOINT")
            .env_remove("BBNF_ARCHITECTURE_FAIL_STAGE")
            .output()
            .expect("serial-oracle child");
        let transcript = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(output.status.success(), "{case} child failed: {transcript}");
        assert_one_named_child_passed(
            &transcript,
            "evidence::binding::f14_final_recensus_serial_oracle_child",
            &format!("{case} child"),
        );
    }
}

#[test]
#[ignore = "isolated child for final-recensus scheduling environment"]
fn f14_final_recensus_serial_oracle_child() {
    let case = std::env::var("BBNF_TEST_RECENSUS_CASE").expect("recensus case");
    let (epoch, diagnostic_message, matched) = match case.as_str() {
        "normal" => (RunEpoch::disabled(), None, 0),
        "failpoint" => (
            RunEpoch::for_test(Some("generation-assembly"), None)
                .expect("generation operation epoch"),
            Some("INJECTED_FAILURE at real operation `generation-assembly`"),
            1,
        ),
        "fail-stage" => (
            RunEpoch::for_test(None, Some("generation")).expect("generation stage epoch"),
            Some("INJECTED_FAILURE at typed stage Generation"),
            1,
        ),
        other => panic!("unknown recensus case {other}"),
    };
    let fixture = materialized_runtime_fixture();
    let oracle_scope = epoch.scope().expect("serial oracle epoch scope");
    let oracle = serial_runtime_binding(&fixture.bbnf);
    drop(oracle_scope);
    crate::assert_epoch(&epoch, 0, case != "normal", false);
    let output = fixture.roots.path().join("oracle-output");
    run_at_with_epoch(&fixture.bbnf, &output, &epoch).expect_err("architecture remains RED");
    crate::assert_epoch(&epoch, matched, false, false);

    let generation = output.join("generations").join(current_run_id(&output));
    let summary: serde_json::Value = serde_json::from_slice(
        &fs::read(generation.join("generation.json")).expect("generation summary"),
    )
    .expect("generation JSON");
    let diagnostics: Vec<Diagnostic> = serde_json::from_slice(
        &fs::read(generation.join("diagnostics.json")).expect("diagnostics"),
    )
    .expect("diagnostics JSON");
    let summary_diagnostics: Vec<Diagnostic> =
        serde_json::from_value(summary["diagnostics"].clone()).expect("summary diagnostics");
    assert_eq!(summary_diagnostics, diagnostics);
    if let Some(message) = diagnostic_message {
        assert_eq!(summary["status"], "failure");
        assert_eq!(summary["stage"], "generation");
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].stage, PipelineStage::Generation);
        assert!(diagnostics[0].chain.is_empty());
        assert_eq!(
            diagnostics[0].message.as_str(),
            message,
            "wrong injected diagnostic"
        );
    } else {
        assert_eq!(summary["status"], "red");
        assert_eq!(summary["stage"], "complete");
        assert!(diagnostics.is_empty());
    }
    let binding: serde_json::Value = serde_json::from_slice(
        &fs::read(generation.join("binding.json")).expect("published binding"),
    )
    .expect("binding JSON");
    assert_eq!(binding, serde_json::to_value(oracle).expect("oracle JSON"));
}

#[test]
fn f18_final_recensus_injection_wins_after_both_workers_overlap_and_join() {
    const INVENTORY: u8 = 0b01;
    const WORKTREE: u8 = 0b10;

    let epoch = RunEpoch::for_test(Some("git-command"), None).expect("recensus epoch");
    let gate = ExactBitGate::new(INVENTORY | WORKTREE);
    let inventory_natural_formed = AtomicBool::new(false);
    let inventory_completed = AtomicBool::new(false);
    let worktree_completed = AtomicBool::new(false);
    let inventory = || {
        gate.arrive(INVENTORY)?;
        let error = anyhow::Error::new(NaturalToolFailure("inventory"));
        inventory_natural_formed.store(true, Ordering::Release);
        inventory_completed.store(true, Ordering::Release);
        Err(error)
    };
    let worktree = || {
        gate.arrive(WORKTREE)?;
        while !inventory_completed.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        let error = failpoint("git-command").expect_err("worktree epoch injection");
        worktree_completed.store(true, Ordering::Release);
        Err(error)
    };
    let error = exercise_final_recensus_workers(&epoch, inventory, worktree)
        .expect_err("typed injection must dominate natural inventory failure");
    assert!(error.downcast_ref::<NaturalToolFailure>().is_none());
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `git-command`"
    );
    assert_eq!(gate.arrived(), INVENTORY | WORKTREE);
    assert!(inventory_natural_formed.load(Ordering::Acquire));
    assert!(inventory_completed.load(Ordering::Acquire));
    assert!(worktree_completed.load(Ordering::Acquire));
    crate::assert_epoch(&epoch, 1, false, false);
}

#[test]
fn f18_final_recensus_worktree_panic_resumes_after_each_inventory_outcome_completes() {
    const INVENTORY: u8 = 0b01;
    const WORKTREE: u8 = 0b10;

    for injected in [false, true] {
        let epoch = if injected {
            RunEpoch::for_test(Some("inventory-read"), None).expect("inventory injection epoch")
        } else {
            RunEpoch::disabled()
        };
        let gate = ExactBitGate::new(INVENTORY | WORKTREE);
        let inventory_completed = AtomicBool::new(false);
        let panic_observed_completion = AtomicBool::new(false);
        let inventory = || -> anyhow::Result<()> {
            gate.arrive(INVENTORY)?;
            let outcome = if injected {
                failpoint("inventory-read")
            } else {
                Err(anyhow::Error::new(NaturalToolFailure("inventory")))
            };
            inventory_completed.store(true, Ordering::Release);
            outcome
        };
        let worktree = || -> anyhow::Result<()> {
            gate.arrive(WORKTREE)?;
            while !inventory_completed.load(Ordering::Acquire) {
                std::thread::yield_now();
            }
            panic_observed_completion.store(true, Ordering::Release);
            std::panic::panic_any(ToolWorkerPanic);
        };
        let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            exercise_final_recensus_workers(&epoch, inventory, worktree)
        }))
        .expect_err("worktree panic must be resumed");
        assert!(panic.downcast_ref::<ToolWorkerPanic>().is_some());
        assert_eq!(gate.arrived(), INVENTORY | WORKTREE);
        assert!(inventory_completed.load(Ordering::Acquire));
        assert!(panic_observed_completion.load(Ordering::Acquire));
        crate::assert_epoch(&epoch, u8::from(injected), false, false);
    }
}

#[test]
fn f14_final_recensus_reexecutes_and_rehashes_a_changed_selected_tool() {
    use std::os::unix::fs::PermissionsExt as _;

    let tools = tempdir().expect("tools");
    let git = tools.path().join("git");
    fs::write(&git, "#!/bin/sh\nprintf '%s\\n' 'git fixture 1'\n").expect("tool");
    let mut permissions = fs::metadata(&git).expect("metadata").permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&git, permissions).expect("executable");
    let selection = crate::tool_selection();
    let first = capture_complete_binding(
        selection
            .clone()
            .replace_tool_for_counterexample("git", &git, &["--version"])
            .expect("first selection"),
    );
    fs::write(
        tools.path().join("git"),
        "#!/bin/sh\nprintf '%s\\n' 'git fixture 2'\n",
    )
    .expect("mutated git");
    let second = capture_complete_binding(
        selection
            .replace_tool_for_counterexample("git", &git, &["--version"])
            .expect("second selection"),
    );
    let error = first.verify_recensus(&second).expect_err("live tool drift");
    assert!(error.to_string().contains("SOURCE_SNAPSHOT_DRIFT"));
    let before = first
        .tools
        .iter()
        .find(|tool| tool.tool.as_str() == "git")
        .expect("first Git");
    let after = second
        .tools
        .iter()
        .find(|tool| tool.tool.as_str() == "git")
        .expect("second Git");
    assert_ne!(before.capability_hash, after.capability_hash);
    assert_ne!(before.executable_hash, after.executable_hash);

    let mut omitted = first.clone();
    omitted.source.members.pop().expect("source member");
    assert!(
        first
            .verify_recensus(&omitted)
            .expect_err("omitted source member")
            .to_string()
            .contains("SOURCE_SNAPSHOT_DRIFT")
    );
    let mut extra = first.clone();
    extra.source.members.push(extra.source.members[0].clone());
    assert!(
        first
            .verify_recensus(&extra)
            .expect_err("extra source member")
            .to_string()
            .contains("SOURCE_SNAPSHOT_DRIFT")
    );
    let mut omitted_retirement = first.clone();
    omitted_retirement.source.members.retain(|member| {
        member.path.as_str() != "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-S2-RETIREMENT.md"
    });
    assert!(
        first
            .verify_recensus(&omitted_retirement)
            .expect_err("omitted S2 retirement receipt")
            .to_string()
            .contains("SOURCE_SNAPSHOT_DRIFT")
    );
}

#[test]
fn f14_build_receipt_censuses_compiled_local_inputs_and_build_tools() {
    let selection = crate::tool_selection();
    #[cfg(target_os = "macos")]
    let xcode_paths = independently_selected_xcode_paths();
    let expected_build_tool_ledger = selection
        .build_tool_ledger_hash()
        .expect("selected build-tool ledger");
    let binding = capture_complete_binding(selection);
    let build = binding.build.as_ref().expect("build receipt");
    assert_eq!(build.build_tool_ledger_hash, expected_build_tool_ledger);
    let serialized = serde_json::to_value(&binding).expect("serialized binding");
    let serialized_build = serialized["build"].as_object().expect("serialized build");
    assert_eq!(
        serialized_build["schema"],
        "bbnf.architecture.exporter-build.v3.2"
    );
    assert!(serialized_build.contains_key("exporter_environment_hash"));
    assert!(!serialized_build.contains_key("exporter_execution_contract_hash"));
    assert!(serialized_build.contains_key("build_feature_graph_hash"));
    let mut legacy = serialized.clone();
    legacy["build"]
        .as_object_mut()
        .expect("legacy build")
        .remove("build_feature_graph_hash");
    assert!(serde_json::from_value::<SourceBinding>(legacy).is_err());
    for (label, mutate) in [
        ("repository", "/repositories/0"),
        ("context", "/contexts/0"),
        ("helper", "/helpers/0"),
    ] {
        let mut hostile = serialized.clone();
        hostile
            .pointer_mut(mutate)
            .unwrap_or_else(|| panic!("missing {label} fixture"))
            .as_object_mut()
            .expect("nested binding object")
            .insert("hostile_unknown".to_owned(), serde_json::json!(true));
        assert!(
            serde_json::from_value::<SourceBinding>(hostile).is_err(),
            "unknown nested {label} field was accepted"
        );
    }
    assert!(
        serde_json::from_value::<ConfigBinding>(serde_json::json!({
            "repository": "bbnf",
            "path": "Cargo.toml",
            "hash": digest("config"),
            "hostile_unknown": true
        }))
        .is_err(),
        "unknown config-binding field was accepted"
    );
    for field in ["source", "build"] {
        let mut omitted = serialized.clone();
        omitted
            .as_object_mut()
            .expect("binding object")
            .remove(field);
        assert!(
            serde_json::from_value::<SourceBinding>(omitted).is_err(),
            "attesting binding defaulted missing {field}"
        );
    }
    for tool in serialized["tools"].as_array().expect("serialized tools") {
        let tool = tool.as_object().expect("serialized tool");
        assert!(tool.contains_key("environment"));
        assert!(!tool.contains_key("execution_contract"));
    }
    if build.build_profile.as_str() == "integration-test" {
        assert_eq!(
            build
                .build_argv
                .iter()
                .map(|argument| argument.as_str())
                .collect::<Vec<_>>(),
            [
                "cargo",
                "test",
                "--no-run",
                "--locked",
                "-p",
                "xtask",
                "--features",
                "architecture-test-seams",
                "--test",
                "architecture",
            ]
        );
    }
    for (repository, path) in [
        ("bbnf", "Cargo.toml"),
        ("bbnf", "rust-toolchain.toml"),
        ("bbnf", ".cargo/config.toml"),
        ("bbnf", "xtask/src/regen.rs"),
        ("bbnf", "xtask/src/regen_css.rs"),
        ("bbnf", "crates/ir/build.rs"),
        (
            "bbnf",
            "crates/csp-solver/data/sudoku_puzzles/4/easy/template-0.json",
        ),
        ("parse-that", "rust/parse_that/src/lib.rs"),
        ("parse-that", "rust/regex/src/lib.rs"),
        ("pprint", "rust/src/lib.rs"),
        ("pprint", "rust/derive/lib.rs"),
    ] {
        assert!(
            build.local_inputs.members.iter().any(|member| {
                member.repository.as_str() == repository && member.path.as_str() == path
            }),
            "missing local build input {repository}:{path}"
        );
    }
    assert!(
        binding
            .source
            .members
            .iter()
            .any(|member| member.path.as_str() == "Cargo.toml"),
        "the root manifest is part of the production source closure as well as the separate local build-input closure"
    );
    for tool in ["sccache", "cc", "ld"] {
        assert!(
            binding
                .tools
                .iter()
                .any(|binding| binding.tool.as_str() == tool),
            "missing build tool {tool}"
        );
    }
    #[cfg(target_os = "macos")]
    {
        use std::os::unix::ffi::OsStrExt as _;
        let bound_path_hash = |path: &std::path::Path| {
            let mut bytes = b"bbnf.exact-os-identity.v1\0".to_vec();
            for field in [b"bound-tool-path".as_slice(), path.as_os_str().as_bytes()] {
                bytes.extend_from_slice(&(field.len() as u64).to_le_bytes());
                bytes.extend_from_slice(field);
            }
            Sha256Digest::of(&bytes)
        };
        let launcher = std::path::Path::new("/usr/bin/arch");
        let launcher_hash = Sha256Digest::of(&fs::read(launcher).expect("Apple ARM launcher"));
        for ((tool, args), selected) in [
            ("ar", &["--version"][..]),
            ("cc", &["--version"][..]),
            ("ld", &["-v"][..]),
        ]
        .into_iter()
        .zip(&xcode_paths)
        {
            let selected_hash =
                Sha256Digest::of(&fs::read(selected).expect("selected Xcode payload"));
            let captured = binding
                .tools
                .iter()
                .find(|binding| binding.tool.as_str() == tool)
                .expect("captured Xcode binding");
            assert_eq!(captured.invocation_name.as_str(), "arch");
            assert_eq!(captured.invocation_path_hash, bound_path_hash(launcher));
            assert_eq!(captured.proxy_hash, launcher_hash);
            assert_eq!(captured.effective_path_hash, bound_path_hash(selected));
            assert_eq!(captured.executable_hash, selected_hash);
            let selection_label = captured.toolchain_selection.as_str();
            assert_eq!(selection_label, "xcode-developer-directory-selection");
            assert_eq!(
                captured
                    .argv
                    .iter()
                    .map(CanonicalText::as_str)
                    .collect::<Vec<_>>(),
                std::iter::once(tool)
                    .chain(args.iter().copied())
                    .collect::<Vec<_>>()
            );
        }
    }

    let mut omitted_path_dependency = binding.clone();
    let omitted_build = omitted_path_dependency
        .build
        .as_mut()
        .expect("build receipt");
    omitted_build.local_inputs.members.retain(|member| {
        !(member.repository.as_str() == "parse-that"
            && member.path.as_str() == "rust/parse_that/src/lib.rs")
    });
    omitted_build.local_inputs.ledger_hash = Sha256Digest::of(b"post-build path mutation");
    assert!(
        omitted_build
            .verify_embedded_census()
            .expect_err("mutate after build without rebuild")
            .to_string()
            .contains("EXPORTER_BUILD_INPUT_DRIFT")
    );
    assert!(
        binding
            .verify_recensus(&omitted_path_dependency)
            .expect_err("omitted path dependency")
            .to_string()
            .contains("SOURCE_SNAPSHOT_DRIFT")
    );

    let mut changed_wrapper = binding.clone();
    changed_wrapper
        .tools
        .iter_mut()
        .find(|tool| tool.tool.as_str() == "sccache")
        .expect("wrapper")
        .executable_hash = Sha256Digest::of(b"changed wrapper");
    assert!(
        binding
            .verify_recensus(&changed_wrapper)
            .expect_err("changed wrapper")
            .to_string()
            .contains("SOURCE_SNAPSHOT_DRIFT")
    );
    let changed_build = changed_wrapper.build.as_mut().expect("build receipt");
    changed_build.build_tool_ledger_hash = Sha256Digest::of(b"post-build wrapper mutation");
    assert!(
        changed_build
            .verify_embedded_census()
            .expect_err("wrapper changed after build without rebuild")
            .to_string()
            .contains("EXPORTER_BUILD_INPUT_DRIFT")
    );
}

#[test]
fn f14_exact_build_tool_hash_rejects_mismatched_xcode_proxy() {
    let selection = crate::tool_selection();
    let mut paths = ["ar", "cargo", "cc", "ld", "rustc", "sccache"]
        .into_iter()
        .map(|tool| {
            let path = if matches!(tool, "cargo" | "rustc") {
                let output = Command::new("rustup")
                    .args(["which", tool])
                    .output()
                    .expect("rustup-selected tool");
                assert!(output.status.success());
                std::path::PathBuf::from(
                    std::str::from_utf8(&output.stdout)
                        .expect("UTF-8 tool path")
                        .trim(),
                )
            } else {
                selection
                    .path(tool)
                    .expect("selected build-tool path")
                    .to_path_buf()
            };
            (tool.to_owned(), path)
        })
        .collect::<BTreeMap<_, _>>();
    let exact = capture_build_tool_binding_hash(&paths).expect("exact binding-format hash");
    let stable = capture_build_tool_binding_hash(&paths).expect("stable exact hash");
    assert_eq!(stable, exact);
    let directory = tempdir().expect("copied tool directory");
    let selected_cc = paths["cc"].clone();
    let selected_cargo = paths["cargo"].clone();
    let copied_cc = directory.path().join("cc");
    let copied_cargo = directory.path().join("cargo");
    fs::copy(&selected_cc, &copied_cc).expect("copy selected clang");
    fs::copy(&selected_cargo, &copied_cargo).expect("copy selected Cargo");
    let aliased_cargo = directory.path().join("cargo-alias");
    std::os::unix::fs::symlink(&selected_cargo, &aliased_cargo).expect("symlink selected Cargo");
    let mut dotted_cc = selected_cc
        .parent()
        .expect("selected clang parent")
        .join(".");
    dotted_cc.push(selected_cc.file_name().expect("selected clang name"));
    let mut dotted_cargo = selected_cargo
        .parent()
        .expect("selected Cargo parent")
        .join(".");
    dotted_cargo.push(selected_cargo.file_name().expect("selected Cargo name"));
    for (dotted, selected) in [(&dotted_cc, &selected_cc), (&dotted_cargo, &selected_cargo)] {
        assert!(dotted.is_absolute());
        assert_ne!(dotted.as_os_str(), selected.as_os_str());
    }
    for (tool, candidate, message) in [
        ("cc", copied_cc, "Xcode-selected payload"),
        ("cc", dotted_cc, "Xcode-selected payload"),
        ("cargo", copied_cargo, "rustup-selected toolchain payload"),
        ("cargo", aliased_cargo, "rustup-selected toolchain payload"),
        ("cargo", dotted_cargo, "rustup-selected toolchain payload"),
    ] {
        paths.insert("cc".to_owned(), selected_cc.clone());
        paths.insert("cargo".to_owned(), selected_cargo.clone());
        paths.insert(tool.to_owned(), candidate);
        let error = capture_build_tool_binding_hash(&paths).expect_err("mismatched build tool");
        assert!(error.to_string().contains(message), "{error:#}");
    }
}

#[test]
fn f14_live_binding_planes_reject_coherently_resealed_lies() {
    let workspace_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root");
    let cargo_config = fs::read_to_string(workspace_root.join(".cargo/config.toml"))
        .expect("checked-in Cargo config");
    assert!(cargo_config.contains("target = \"aarch64-apple-darwin\""));
    let doctor =
        fs::read_to_string(workspace_root.join("scripts/doctor.sh")).expect("doctor source");
    assert!(doctor.contains("[ ! -x /usr/bin/arch ]"));
    assert!(doctor.contains("[ ! -x /usr/bin/xcrun ]"));
    assert!(doctor.contains("/usr/bin/arch -arm64 /usr/bin/xcrun --find \"$tool\""));
    assert!(!doctor.contains("|| true"));
    for alias in ["CC=", "AR=", "LD="] {
        assert!(!doctor.contains(alias), "doctor contains {alias} alias");
    }

    let selection = crate::tool_selection();
    let exporter_path = selection
        .path("xtask-exporter")
        .expect("selected exporter")
        .to_path_buf();
    let (binding, workspace, repository_roots) = capture_grounded_binding(selection);
    binding
        .validate_live_binding_planes_for_test(&workspace, &repository_roots, &exporter_path)
        .expect("baseline live binding planes");

    for plane in [
        "manifest",
        "repositories",
        "contexts",
        "collector-inputs",
        "helpers",
        "worktrees",
        "worktree-hashing",
        "nominations",
    ] {
        let mut hostile = binding.clone();
        match plane {
            "manifest" => hostile.manifest_hash = digest("forged manifest"),
            "repositories" => hostile.repositories.clear(),
            "contexts" => hostile.contexts.clear(),
            "collector-inputs" => hostile.collector_inputs.push(ConfigBinding {
                repository: xtask::architecture::schema::RepositoryId::new("bbnf")
                    .expect("repository"),
                path: RepoPath::new("Cargo.toml").expect("path"),
                hash: digest("forged collector input"),
            }),
            "helpers" => hostile.helpers.clear(),
            "worktrees" => hostile.worktrees.clear(),
            "worktree-hashing" => hostile.worktree_hashing.jobs += 1,
            "nominations" => hostile.nominations.clear(),
            _ => unreachable!(),
        }
        hostile.reseal_for_test().expect("coherently reseal lie");
        let error = hostile
            .validate_binding_planes_against_grounded_truth_for_test(&binding)
            .expect_err("live plane lie");
        assert!(
            error.to_string().contains("binding planes differ"),
            "{plane}: {error:#}"
        );
    }
}

#[test]
fn f14_manifest_cannot_omit_pprint_from_the_live_repository_closure() {
    let mut manifest = crate::manifest();
    manifest
        .repositories
        .retain(|repository| repository.id.as_str() != "pprint");
    let error = manifest
        .validate()
        .expect_err("manifest omitted the build-input pprint repository");
    assert!(
        error
            .to_string()
            .contains("exactly the three physical repositories"),
        "{error:#}"
    );
}

#[test]
fn f18_required_tool_workers_overlap_at_one_exact_bit_gate() {
    let epoch = RunEpoch::disabled();
    let gate = ExactBitGate::new(ALL_TOOL_WORKERS);
    let worker = |tool| {
        gate.arrive(tool_worker_bit(tool))?;
        Ok(())
    };
    let tools = exercise_required_tool_workers(&epoch, &worker).expect("eight-worker fanout");
    assert_eq!(
        tools,
        ["ar", "cargo", "rustc", "node", "git", "sccache", "cc", "ld"]
    );
    assert_eq!(gate.arrived(), ALL_TOOL_WORKERS);
    crate::assert_epoch(&epoch, 0, false, false);
}

#[test]
fn f18_typed_tool_injection_wins_over_natural_errors_on_both_sides() {
    let epoch = RunEpoch::for_test(Some("tool-node-spawn"), None).expect("tool epoch");
    let selection = crate::tool_selection();
    let scope = epoch.scope().expect("exporter request scope");
    let exporter_before = selection
        .recapture_exporter()
        .expect("exporter before match");
    drop(scope);
    let expected_request = expected_runtime_fact("BBNF_ARCHITECTURE_FAILPOINT", "tool-node-spawn");
    assert_eq!(
        exporter_before
            .execution_contract
            .iter()
            .filter(|fact| {
                fact.as_str()
                    .starts_with("runtime-input:BBNF_ARCHITECTURE_FAILPOINT:")
            })
            .map(CanonicalText::as_str)
            .collect::<Vec<_>>(),
        vec![expected_request.as_str()]
    );
    let gate = ExactBitGate::new(ALL_TOOL_WORKERS);
    let natural = AtomicU8::new(0);
    let worker = |tool| {
        gate.arrive(tool_worker_bit(tool))?;
        match tool {
            "cargo" | "git" => {
                natural.fetch_or(tool_worker_bit(tool), Ordering::SeqCst);
                Err(anyhow::Error::new(NaturalToolFailure(tool)))
            }
            "node" => failpoint("tool-node-spawn"),
            _ => Ok(()),
        }
    };
    let error = exercise_required_tool_workers(&epoch, &worker).expect_err("typed injection");
    assert_eq!(
        natural.load(Ordering::SeqCst),
        tool_worker_bit("cargo") | tool_worker_bit("git")
    );
    assert!(error.downcast_ref::<NaturalToolFailure>().is_none());
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `tool-node-spawn`"
    );
    crate::assert_epoch(&epoch, 1, false, false);
    let scope = epoch.scope().expect("matched exporter request scope");
    let exporter_after = selection
        .recapture_exporter()
        .expect("exporter after match");
    drop(scope);
    assert_eq!(exporter_after, exporter_before);
}

#[test]
fn f18_natural_tool_errors_use_required_order_not_completion_order() {
    let epoch = RunEpoch::disabled();
    let gate = ExactBitGate::new(ALL_TOOL_WORKERS);
    let rustc_completed = AtomicBool::new(false);
    let completed = AtomicU8::new(0);
    let worker = |tool| {
        gate.arrive(tool_worker_bit(tool))?;
        let result = match tool {
            "rustc" => {
                let error = anyhow::Error::new(NaturalToolFailure(tool));
                completed.fetch_or(tool_worker_bit(tool), Ordering::Release);
                rustc_completed.store(true, Ordering::Release);
                return Err(error);
            }
            "cargo" => {
                while !rustc_completed.load(Ordering::Acquire) {
                    std::thread::yield_now();
                }
                assert_ne!(
                    completed.load(Ordering::Acquire) & tool_worker_bit("rustc"),
                    0
                );
                let error = anyhow::Error::new(NaturalToolFailure(tool));
                completed.fetch_or(tool_worker_bit(tool), Ordering::Release);
                return Err(error);
            }
            _ => Ok(()),
        };
        completed.fetch_or(tool_worker_bit(tool), Ordering::SeqCst);
        result
    };
    let error = exercise_required_tool_workers(&epoch, &worker).expect_err("natural errors");
    let first = error
        .downcast_ref::<NaturalToolFailure>()
        .expect("typed natural failure");
    assert_eq!(first.0, "cargo");
    assert!(rustc_completed.load(Ordering::Acquire));
    assert_eq!(completed.load(Ordering::SeqCst), ALL_TOOL_WORKERS);
    crate::assert_epoch(&epoch, 0, false, false);
}

#[test]
fn f18_tool_worker_panic_wins_after_every_other_worker_finishes() {
    let epoch = RunEpoch::for_test(Some("tool-node-spawn"), None).expect("panic epoch");
    let gate = ExactBitGate::new(ALL_TOOL_WORKERS);
    let panic_started = AtomicBool::new(false);
    let completed = AtomicU8::new(0);
    let worker = |tool| {
        gate.arrive(tool_worker_bit(tool))?;
        if tool == "git" {
            panic_started.store(true, Ordering::Release);
            std::panic::panic_any(ToolWorkerPanic);
        }
        while !panic_started.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        let result = match tool {
            "cargo" => Err(anyhow::Error::new(NaturalToolFailure(tool))),
            "node" => failpoint("tool-node-spawn"),
            _ => Ok(()),
        };
        completed.fetch_or(tool_worker_bit(tool), Ordering::SeqCst);
        result
    };
    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        exercise_required_tool_workers(&epoch, &worker)
    }))
    .expect_err("worker panic must be fatal");
    assert!(panic.downcast_ref::<ToolWorkerPanic>().is_some());
    assert!(panic_started.load(Ordering::Acquire));
    assert_eq!(
        completed.load(Ordering::SeqCst),
        ALL_TOOL_WORKERS & !tool_worker_bit("git")
    );
    crate::assert_epoch(&epoch, 1, false, false);
}

#[test]
#[ignore = "pre-build helper emits the complete bound build-tool ledger"]
fn f14_emit_build_tool_ledger_for_release_receipt() {
    let selection = ToolSelection::capture().expect("tool selection");
    println!(
        "BUILD_TOOL_LEDGER_HASH={}",
        selection
            .build_tool_ledger_hash()
            .expect("build tool ledger")
    );
}

#[test]
fn f14_exporter_child_environment_contract_ignores_hostile_ambient_locale_and_timezone() {
    const CHILD_CASE: &str = "BBNF_TEST_EXPORTER_ENVIRONMENT_CASE";
    const TEST_NAME: &str = "evidence::binding::f14_exporter_child_environment_contract_ignores_hostile_ambient_locale_and_timezone";
    const RECEIPT_PREFIX: &str = "EXPORTER_EXECUTION_CONTRACT_RECEIPT:";

    if let Some(case) = std::env::var_os(CHILD_CASE) {
        match case.to_str() {
            Some("clean") => {
                for name in ["LANG", "LC_ALL", "LC_CTYPE", "TZ", "RUSTUP_TOOLCHAIN"] {
                    assert!(
                        std::env::var_os(name).is_none(),
                        "clean parent leaked {name}"
                    );
                }
            }
            Some("hostile") => {
                for (name, expected) in [
                    ("LANG", "tr_TR.UTF-8"),
                    ("LC_ALL", "C.UTF-8"),
                    ("LC_CTYPE", "ja_JP.UTF-8"),
                    ("TZ", "Pacific/Kiritimati"),
                    ("RUSTUP_TOOLCHAIN", "hostile-parent-toolchain"),
                ] {
                    assert_eq!(std::env::var(name).expect(name), expected);
                }
            }
            other => panic!("unknown exporter execution-contract case {other:?}"),
        }

        let child = canonical_child_command(std::path::Path::new("/usr/bin/env"))
            .output()
            .expect("canonical /usr/bin/env child");
        assert!(child.status.success(), "canonical environment child failed");
        assert!(
            child.stderr.is_empty(),
            "canonical environment child wrote stderr"
        );
        let mut child_environment = std::str::from_utf8(&child.stdout)
            .expect("canonical environment UTF-8")
            .lines()
            .map(CanonicalText::new)
            .collect::<anyhow::Result<Vec<_>>>()
            .expect("canonical environment rows");
        child_environment.sort();
        assert_eq!(
            child_environment
                .iter()
                .map(CanonicalText::as_str)
                .collect::<Vec<_>>(),
            ["LC_ALL=C", "TZ=UTC"]
        );
        assert!(child_environment.iter().all(|row| {
            !row.as_str().starts_with("LANG=")
                && !row.as_str().starts_with("LC_CTYPE=")
                && !row.as_str().starts_with("RUSTUP_TOOLCHAIN=")
        }));

        let exporter = crate::capture_current_exporter().expect("child exporter");
        let execution_contract = exporter.execution_contract;
        let child_policy = execution_contract
            .iter()
            .filter(|fact| !fact.as_str().contains(":sha256:"))
            .map(CanonicalText::as_str)
            .collect::<BTreeSet<_>>();
        assert_eq!(
            child_policy,
            [
                "child-policy-scope=all-production-architecture-children",
                "child-policy:env-clear=true",
                "child-policy:set:LC_ALL=C",
                "child-policy:set:TZ=UTC",
            ]
            .into_iter()
            .collect()
        );
        let hashed_names = execution_contract
            .iter()
            .filter_map(|fact| fact.as_str().split_once(":sha256:").map(|row| row.0))
            .collect::<BTreeSet<_>>();
        assert_eq!(
            hashed_names,
            [
                "runtime-input:BBNF_ARCHITECTURE_FAILPOINT",
                "runtime-input:BBNF_ARCHITECTURE_FAIL_STAGE",
                "runtime-input:BBNF_ARCHITECTURE_WORKTREE_THREADS",
                "runtime-input:PATH",
            ]
            .into_iter()
            .collect()
        );
        assert!(
            execution_contract
                .iter()
                .all(|row| !row.as_str().contains("RUSTUP_TOOLCHAIN"))
        );
        let execution_contract_hash = Sha256Digest::of(
            &serde_json::to_vec(&execution_contract).expect("execution-contract JSON"),
        );
        let child_environment_hash = Sha256Digest::of(
            &serde_json::to_vec(&child_environment).expect("child-environment JSON"),
        );
        let receipt = ExporterExecutionContractReceipt {
            execution_contract,
            execution_contract_hash,
            child_environment,
            child_environment_hash,
        };
        println!(
            "{RECEIPT_PREFIX}{}",
            serde_json::to_string(&receipt).expect("execution-contract receipt JSON")
        );
        return;
    }

    let path = std::env::var_os("PATH").expect("test PATH");
    let run_child = |case: &str, hostile: bool| {
        let mut command = Command::new(std::env::current_exe().expect("test executable"));
        command
            .args(["--exact", TEST_NAME, "--show-output", "--test-threads=1"])
            .env_clear()
            .env("PATH", &path)
            .env(CHILD_CASE, case);
        if hostile {
            command
                .env("LANG", "tr_TR.UTF-8")
                .env("LC_ALL", "C.UTF-8")
                .env("LC_CTYPE", "ja_JP.UTF-8")
                .env("TZ", "Pacific/Kiritimati")
                .env("RUSTUP_TOOLCHAIN", "hostile-parent-toolchain");
        }
        let output = command.output().expect("exporter environment child");
        let transcript = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            output.status.success(),
            "{case} exporter environment child failed: {transcript}"
        );
        assert_one_named_child_passed(&transcript, TEST_NAME, case);
        let rows = transcript
            .lines()
            .filter_map(|line| line.strip_prefix(RECEIPT_PREFIX))
            .collect::<Vec<_>>();
        assert_eq!(rows.len(), 1, "{case} receipt was not unique: {transcript}");
        serde_json::from_str::<ExporterExecutionContractReceipt>(rows[0])
            .expect("typed exporter execution-contract receipt")
    };

    let clean = run_child("clean", false);
    let hostile = run_child("hostile", true);
    assert_eq!(clean, hostile);
}

#[test]
fn f14_worktree_thread_selector_is_bound_by_exact_name() {
    const CHILD: &str = "BBNF_TEST_WORKTREE_SELECTOR_CHILD";
    let selector = "7";
    let expected = expected_runtime_fact("BBNF_ARCHITECTURE_WORKTREE_THREADS", selector);
    if std::env::var_os(CHILD).is_some() {
        assert_eq!(
            std::env::var("BBNF_ARCHITECTURE_WORKTREE_THREADS").expect("child selector"),
            selector
        );
        let exporter = crate::capture_current_exporter().expect("child exporter");
        let facts = exporter
            .execution_contract
            .iter()
            .filter(|fact| {
                fact.as_str()
                    .starts_with("runtime-input:BBNF_ARCHITECTURE_WORKTREE_THREADS:")
            })
            .map(CanonicalText::as_str)
            .collect::<Vec<_>>();
        assert_eq!(
            facts,
            vec![expected.as_str()],
            "child did not bind the exact selector fact"
        );
        assert!(
            exporter
                .execution_contract
                .iter()
                .all(|fact| { !fact.as_str().contains("BBNF_ARCHITECTURE_WORKERS:") })
        );
        println!("WORKTREE_SELECTOR_CHILD:{selector}:{expected}");
        return;
    }

    let child = Command::new(std::env::current_exe().expect("test executable"))
        .args([
            "--exact",
            "evidence::binding::f14_worktree_thread_selector_is_bound_by_exact_name",
            "--show-output",
            "--test-threads=1",
        ])
        .env(CHILD, "1")
        .env("BBNF_ARCHITECTURE_WORKTREE_THREADS", selector)
        .env_remove("BBNF_ARCHITECTURE_FAILPOINT")
        .env_remove("BBNF_ARCHITECTURE_FAIL_STAGE")
        .output()
        .expect("selector child");
    let transcript = format!(
        "{}{}",
        String::from_utf8_lossy(&child.stdout),
        String::from_utf8_lossy(&child.stderr)
    );
    assert!(
        child.status.success(),
        "selector child failed: {transcript}"
    );
    let selector_fact = format!("WORKTREE_SELECTOR_CHILD:{selector}:{expected}");
    assert_eq!(
        transcript
            .lines()
            .filter(|line| *line == selector_fact.as_str())
            .count(),
        1,
        "selector child fact was not unique: {transcript}"
    );
    assert_one_named_child_passed(
        &transcript,
        "evidence::binding::f14_worktree_thread_selector_is_bound_by_exact_name",
        "selector child",
    );
}
