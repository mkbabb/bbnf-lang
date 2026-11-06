use xtask::architecture::inventory::{Inventory, InventoryRecord, reconcile_tracked_exclusions};
use xtask::architecture::manifest::ExclusionDecl;
use xtask::architecture::schema::{
    CanonicalText, ExclusionId, PhysicalIdentity, PhysicalKind, RepoPath, RepositoryId,
    Sha256Digest,
};
use xtask::architecture::test_support::RunEpoch;

#[test]
fn f18_inventory_walk_and_read_failpoints_are_operation_local() {
    let roots = tempfile::tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = super::frontend::materialize(&manifest, roots.path());
    for failpoint in ["inventory-walk", "inventory-read"] {
        let epoch = RunEpoch::for_test(Some(failpoint), None).expect("inventory epoch");
        let scope = epoch.scope().expect("inventory epoch scope");
        let error = Inventory::collect_snapshot(&manifest, &repositories).expect_err(failpoint);
        drop(scope);
        assert_eq!(
            error.to_string(),
            format!("INJECTED_FAILURE at real operation `{failpoint}`")
        );
        super::assert_epoch(&epoch, 1, false, false);
    }
}

#[test]
fn hash_only_inventory_replays_exact_collector_bindings_without_retaining_bytes() {
    let roots = tempfile::tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = super::frontend::materialize(&manifest, roots.path());
    std::fs::write(
        repositories[0].root.join("playground/resource.css"),
        "@import './base.css';\n",
    )
    .expect("frontend input");
    let snapshot = Inventory::collect_snapshot(&manifest, &repositories).expect("byte snapshot");
    let hash_only = Inventory::collect(&manifest, &repositories).expect("hash-only inventory");
    assert_eq!(hash_only, snapshot.inventory);
    assert_eq!(
        hash_only
            .collector_bindings(&manifest)
            .expect("derived collector bindings"),
        snapshot.inputs.bindings()
    );
}

#[test]
fn inventory_path_ingress_preserves_unicode_and_rejects_non_utf8() {
    use std::os::unix::ffi::OsStringExt;

    let invalid = [
        std::ffi::OsString::from_vec(vec![b'n', b'o', b'n', 0xff]),
        std::ffi::OsString::from_vec(vec![b'n', b'o', b'n', 0xfe]),
    ];
    assert_eq!(invalid[0].to_string_lossy(), invalid[1].to_string_lossy());
    let roots = tempfile::tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = super::frontend::materialize(&manifest, roots.path());
    let bbnf = &repositories[0].root;
    std::fs::write(bbnf.join("éclat.rs"), "unicode").expect("Unicode fixture");
    let snapshot =
        Inventory::collect_snapshot(&manifest, &repositories).expect("Unicode inventory");
    assert!(
        snapshot
            .inventory
            .find(
                &RepositoryId::new("bbnf").expect("repository"),
                &RepoPath::new("éclat.rs").expect("Unicode path"),
            )
            .is_some()
    );
    for invalid in invalid {
        let error = xtask::architecture::inventory::filesystem::relative_path_for_test(
            bbnf,
            &bbnf.join(invalid),
        )
        .expect_err("non-UTF8 repository path")
        .to_string();
        assert!(error.contains("UTF-8"), "{error}");
    }
}

#[test]
fn symlink_content_hash_preserves_exact_non_utf8_target_identity() {
    use std::os::unix::ffi::OsStringExt;

    let roots = tempfile::tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = super::frontend::materialize(&manifest, roots.path());
    let bbnf = &repositories[0].root;
    let targets = [
        std::ffi::OsString::from_vec(vec![b't', b'a', b'r', b'g', b'e', b't', 0xff]),
        std::ffi::OsString::from_vec(vec![b't', b'a', b'r', b'g', b'e', b't', 0xfe]),
    ];
    assert_eq!(targets[0].to_string_lossy(), targets[1].to_string_lossy());
    for (name, target) in ["raw-link-a", "raw-link-b"].into_iter().zip(&targets) {
        std::os::unix::fs::symlink(target, bbnf.join(name)).expect("raw target symlink");
    }

    let snapshot = Inventory::collect_snapshot(&manifest, &repositories).expect("inventory");
    let repository = RepositoryId::new("bbnf").expect("repository");
    let content_hash = |name| {
        snapshot
            .inventory
            .find(
                &repository,
                &RepoPath::new(name).expect("fixture repository path"),
            )
            .expect("inventoried raw target symlink")
            .content_hash
            .clone()
            .expect("symlink target hash")
    };
    assert_ne!(
        content_hash("raw-link-a"),
        content_hash("raw-link-b"),
        "exact non-UTF-8 symlink targets must not collide through lossy spelling"
    );
}

#[test]
fn duplicate_physical_triples_fail_before_face_validation() {
    let record = InventoryRecord {
        identity: PhysicalIdentity {
            repository: RepositoryId::new("bbnf").expect("repo"),
            path: RepoPath::new("Cargo.toml").expect("path"),
            kind: PhysicalKind::File,
        },
        content_hash: Some(Sha256Digest::of(b"manifest")),
        semantic_faces: Vec::new(),
        exclusion: None,
    };
    let inventory = Inventory {
        schema: "bbnf.architecture.inventory.v3.1".into(),
        records: vec![record.clone(), record],
    };
    let error = inventory
        .validate(&super::manifest())
        .expect_err("duplicate physical path");
    assert!(error.to_string().contains("INVENTORY_PATH_AMBIGUOUS"));
}

#[test]
fn f08_tracked_authored_input_cannot_be_erased_by_an_exclusion() {
    use std::process::Command;
    use tempfile::tempdir;
    use xtask::architecture::manifest::ResolvedRepository;

    let fixture = tempdir().expect("fixture");
    Command::new("git")
        .arg("-C")
        .arg(fixture.path())
        .arg("init")
        .output()
        .expect("git init");
    std::fs::create_dir(fixture.path().join(".profiles")).expect("profiles");
    std::fs::write(fixture.path().join(".profiles/authored.md"), "authored").expect("authored");
    let status = Command::new("git")
        .arg("-C")
        .arg(fixture.path())
        .args(["add", ".profiles/authored.md"])
        .status()
        .expect("git add");
    assert!(status.success());
    let mut manifest = super::manifest();
    manifest.exclusions.push(ExclusionDecl {
        id: ExclusionId::new("tracked-profiles").expect("id"),
        repository: RepositoryId::new("bbnf").expect("repository"),
        pattern: CanonicalText::new(".profiles/**").expect("pattern"),
        reason: CanonicalText::new("fixture must fail").expect("reason"),
    });
    manifest.canonicalize();
    manifest.validate().expect("exclusion grammar");
    let repositories = [ResolvedRepository {
        id: RepositoryId::new("bbnf").expect("repository"),
        root: fixture.path().to_path_buf(),
    }];
    let error =
        reconcile_tracked_exclusions(&manifest, &repositories).expect_err("tracked exclusion");
    assert!(error.to_string().contains("TRACKED_INPUT_EXCLUDED"));
}

#[test]
fn manifest_keeps_skinny_and_wasm_in_the_bbnf_repository() {
    let manifest = super::manifest();
    for face in manifest
        .package_faces
        .iter()
        .filter(|face| face.id.as_str().contains("skinny") || face.id.as_str().contains("wasm"))
    {
        assert_eq!(face.repository.as_str(), "bbnf");
    }
}

#[test]
fn f08_checked_in_policy_records_but_never_traverses_tracked_exclusion_boundary_symlink() {
    use std::process::Command;
    use tempfile::tempdir;

    let roots = tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = super::frontend::materialize(&manifest, roots.path());
    let bbnf = &repositories[0].root;
    std::fs::create_dir_all(bbnf.join("archive")).expect("archive");
    std::os::unix::fs::symlink(
        roots.path().join("external-node-modules"),
        bbnf.join("archive/node_modules"),
    )
    .expect("symlink");
    let status = Command::new("git")
        .arg("-C")
        .arg(bbnf)
        .args(["add", "archive/node_modules"])
        .status()
        .expect("git add");
    assert!(status.success());
    let snapshot =
        Inventory::collect_snapshot(&manifest, &repositories).expect("tracked boundary symlink");
    let record = snapshot
        .inventory
        .find(
            &RepositoryId::new("bbnf").expect("repository"),
            &RepoPath::new("archive/node_modules").expect("path"),
        )
        .expect("inventoried symlink");
    assert_eq!(record.identity.kind, PhysicalKind::Symlink);
    assert!(record.content_hash.is_some());
    assert!(record.exclusion.is_some());
    assert!(record.semantic_faces.is_empty());
}

#[test]
fn f11_ignored_excluded_bytes_are_mechanically_unreadable_to_collectors() {
    use tempfile::tempdir;

    let roots = tempdir().expect("roots");
    let mut manifest = super::manifest();
    manifest.exclusions.push(ExclusionDecl {
        id: ExclusionId::new("ignored-hidden").expect("id"),
        repository: RepositoryId::new("bbnf").expect("repository"),
        pattern: CanonicalText::new("hidden/**").expect("pattern"),
        reason: CanonicalText::new("ignored derived fixture").expect("reason"),
    });
    manifest.canonicalize();
    manifest.validate().expect("manifest");
    let repositories = super::frontend::materialize(&manifest, roots.path());
    std::fs::create_dir_all(repositories[0].root.join("hidden")).expect("hidden");
    std::fs::write(
        repositories[0].root.join("hidden/site.css"),
        "@import \"secret\";\n",
    )
    .expect("ignored input");
    let snapshot = Inventory::collect_snapshot(&manifest, &repositories).expect("snapshot");
    assert!(
        snapshot
            .inputs
            .find(
                &RepositoryId::new("bbnf").expect("repository"),
                &RepoPath::new("hidden/site.css").expect("path"),
            )
            .is_none(),
        "collector captured a byte hidden by the typed exclusion"
    );
    snapshot
        .inputs
        .validate(&snapshot.inventory)
        .expect("collector-read reconciliation");
}
