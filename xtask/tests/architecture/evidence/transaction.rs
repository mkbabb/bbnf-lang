use super::*;

#[test]
fn publication_lock_path_is_persistent_after_the_owning_handle_drops() {
    let output = tempdir().expect("output");
    let manifest = output.path().join("manifest.toml");
    std::fs::write(&manifest, "fixture = 1\n").expect("manifest");
    let binding = SourceBinding::minimal(&manifest).expect("binding");
    publish(
        output.path(),
        EvidencePayload {
            binding: &binding,
            manifest: None,
            inventory: None,
            graph: None,
            graph_validated: false,
            projections: None,
            failures: Some(&[]),
            diagnostics: Vec::new(),
            stage: PipelineStage::Complete,
            status: GenerationStatus::Red,
        },
    )
    .expect("publication");
    assert!(
        output.path().join(".publication.lock").is_file(),
        "the persistent path is not ownership and must not be unlinked on drop"
    );
}

#[test]
#[cfg(unix)]
fn publication_lock_serializes_live_a_b_c_and_releases_on_a_crash() {
    let root = tempdir().expect("lock composition");
    let output = root.path().join("output");
    std::fs::create_dir_all(&output).expect("output");
    let manifest = root.path().join("manifest.toml");
    std::fs::write(&manifest, "fixture = 1\n").expect("manifest");
    assert!(
        Command::new("/usr/bin/mkfifo")
            .arg(output.join("CURRENT"))
            .status()
            .expect("mkfifo")
            .success()
    );

    let mut a = publication_child(&output, &manifest);
    wait_for_lock_owner(&output, a.id());
    let mut b = publication_child(&output, &manifest);
    std::thread::sleep(Duration::from_millis(100));
    assert!(
        b.try_wait().expect("B status").is_none(),
        "B bypassed live A"
    );
    wait_for_lock_owner(&output, a.id());

    a.kill().expect("crash A");
    let _ = a.wait().expect("A crash status");
    wait_for_lock_owner(&output, b.id());
    let mut c = publication_child(&output, &manifest);
    std::thread::sleep(Duration::from_millis(100));
    assert!(
        c.try_wait().expect("C status").is_none(),
        "C bypassed live B"
    );
    wait_for_lock_owner(&output, b.id());

    std::fs::write(output.join("CURRENT"), b"invalid-current\n").expect("release FIFO reader");
    assert!(
        b.wait().expect("B status").success(),
        "B did not replace crashed A"
    );
    assert!(c.wait().expect("C status").success(), "C did not follow B");
    assert!(output.join(".publication.lock").is_file());
}

#[test]
#[ignore = "isolated publisher for publication_lock_serializes_live_a_b_c_and_releases_on_a_crash"]
fn publication_process_child() {
    let output = std::env::var_os("BBNF_ARCHITECTURE_LOCK_OUTPUT")
        .map(std::path::PathBuf::from)
        .expect("output");
    let manifest = std::env::var_os("BBNF_ARCHITECTURE_LOCK_MANIFEST")
        .map(std::path::PathBuf::from)
        .expect("manifest");
    let binding = SourceBinding::minimal(&manifest).expect("binding");
    publish(
        &output,
        EvidencePayload {
            binding: &binding,
            manifest: None,
            inventory: None,
            graph: None,
            graph_validated: false,
            projections: None,
            failures: Some(&[]),
            diagnostics: Vec::new(),
            stage: PipelineStage::Complete,
            status: GenerationStatus::Red,
        },
    )
    .expect("publication");
}

#[cfg(unix)]
fn publication_child(output: &std::path::Path, manifest: &std::path::Path) -> std::process::Child {
    Command::new(std::env::current_exe().expect("test executable"))
        .args([
            "--ignored",
            "--exact",
            "evidence::transaction::publication_process_child",
        ])
        .env("BBNF_ARCHITECTURE_LOCK_OUTPUT", output)
        .env("BBNF_ARCHITECTURE_LOCK_MANIFEST", manifest)
        .spawn()
        .expect("publication child")
}

#[cfg(unix)]
fn wait_for_lock_owner(output: &std::path::Path, pid: u32) {
    let expected = pid.to_string();
    let started = Instant::now();
    loop {
        if std::fs::read_to_string(output.join(".publication.lock"))
            .is_ok_and(|owner| owner.trim() == expected)
        {
            return;
        }
        assert!(
            started.elapsed() < Duration::from_secs(5),
            "publisher {pid} did not acquire the kernel lock"
        );
        std::thread::sleep(Duration::from_millis(10));
    }
}
