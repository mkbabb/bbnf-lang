use std::collections::BTreeMap;
use std::path::PathBuf;

use xtask::architecture::inventory::{Inventory, InventoryRecord};
use xtask::architecture::manifest::RepositoryDiscovery;
use xtask::architecture::schema::{PhysicalIdentity, PhysicalKind, RepoPath, Sha256Digest};
use xtask::architecture::test_support::RunEpoch;

#[test]
fn f18_manifest_read_failpoint_is_operation_local() {
    let epoch = RunEpoch::for_test(Some("manifest-read"), None).expect("manifest epoch");
    let scope = epoch.scope().expect("manifest epoch scope");
    let error = xtask::architecture::manifest::ConstellationManifest::load(&super::manifest_path())
        .expect_err("manifest read");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `manifest-read`"
    );
    super::assert_epoch(&epoch, 1, false, false);
}

#[test]
fn f05_canonical_repository_root_alias_fails_before_inventory() {
    let mut manifest = super::manifest();
    let parse = manifest
        .repositories
        .iter_mut()
        .find(|repository| repository.id.as_str() == "parse-that")
        .expect("parse-that");
    parse.discovery = RepositoryDiscovery::WorkspaceRoot;
    manifest.validate().expect("relational manifest");
    let xtask_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = xtask_root.parent().expect("workspace");
    let error = manifest
        .resolve_repositories(workspace)
        .expect_err("aliased roots");
    assert!(error.to_string().contains("REPOSITORY_ROOT_ALIAS"));
}

#[test]
fn f06_relational_cross_owner_and_reciprocal_mutations_fail_exactly() {
    let current = super::manifest();

    let mut cross_workspace = current.clone();
    cross_workspace.products[0].workspaces[0] = current
        .workspaces
        .iter()
        .find(|workspace| workspace.repository.as_str() == "parse-that")
        .expect("parse workspace")
        .id
        .clone();
    assert_code(
        cross_workspace.validate(),
        "PRODUCT_WORKSPACE_REPOSITORY_MISMATCH",
    );

    let mut omitted_face = current.clone();
    omitted_face.products[0].package_faces.pop();
    assert_code(omitted_face.validate(), "FACE_RECIPROCITY_MISMATCH");

    let mut cross_context = current.clone();
    cross_context.contexts[0].repository = current.repositories[1].id.clone();
    assert_code(cross_context.validate(), "CONTEXT_REPOSITORY_MISMATCH");

    let mut cross_nomination = current;
    cross_nomination.worktree_nominations[0].repository =
        cross_nomination.repositories[1].id.clone();
    assert_code(
        cross_nomination.validate(),
        "NOMINATION_REPOSITORY_MISMATCH",
    );

    let current = super::manifest();
    let mut cross_workspace_face = current.clone();
    let generated = cross_workspace_face
        .package_faces
        .iter_mut()
        .find(|face| face.id.as_str() == "bbnf-wasm-browser")
        .expect("generated face");
    generated.workspace =
        xtask::architecture::schema::WorkspaceId::new("bbnf-root").expect("workspace");
    assert_code(
        cross_workspace_face.validate(),
        "WORKSPACE_FACE_RECIPROCITY_MISMATCH",
    );

    let mut absent_nomination = current.clone();
    absent_nomination.worktree_nominations.pop();
    assert_code(
        absent_nomination.validate(),
        "NOMINATION_CARDINALITY_MISMATCH",
    );

    let mut duplicate_nomination = current;
    let mut duplicate = duplicate_nomination.worktree_nominations[0].clone();
    duplicate.id =
        xtask::architecture::schema::WorktreeId::new("bbnf-primary-duplicate").expect("id");
    duplicate_nomination.worktree_nominations.push(duplicate);
    assert_code(
        duplicate_nomination.validate(),
        "NOMINATION_CARDINALITY_MISMATCH",
    );

    let current = super::manifest();
    let mut cross_product_face = current.clone();
    cross_product_face.package_faces[0].product = current.products[1].id.clone();
    assert_code(cross_product_face.validate(), "FACE_RECIPROCITY_MISMATCH");

    let mut cross_face_repository = current.clone();
    cross_face_repository.package_faces[0].repository = current.repositories[1].id.clone();
    assert_code(
        cross_face_repository.validate(),
        "FACE_RECIPROCITY_MISMATCH",
    );

    let mut context_without_face = current.clone();
    context_without_face.contexts[0].package_faces.clear();
    assert_code(
        context_without_face.validate(),
        "CONTEXT_FACE_RECIPROCITY_MISMATCH",
    );

    let mut context_wrong_kind = current.clone();
    let frontend_face = current
        .package_faces
        .iter()
        .find(|face| {
            face.repository == context_wrong_kind.contexts[0].repository
                && face.kind == xtask::architecture::manifest::PackageFaceKind::Frontend
        })
        .expect("frontend face");
    context_wrong_kind.contexts[0].workspace = frontend_face.workspace.clone();
    context_wrong_kind.contexts[0].package_faces = vec![frontend_face.id.clone()];
    context_wrong_kind.contexts[0].manifest = frontend_face.manifest.clone();
    assert_code(context_wrong_kind.validate(), "CONTEXT_FACE_KIND_MISMATCH");

    let mut context_wrong_manifest = current.clone();
    context_wrong_manifest.contexts[0].manifest = RepoPath::new("not-a-face.toml").expect("path");
    assert_code(
        context_wrong_manifest.validate(),
        "CONTEXT_MANIFEST_ROLE_MISMATCH",
    );

    let mut invalid_supersession = current;
    let superseded = invalid_supersession
        .documentation
        .iter()
        .position(|document| document.superseded_by.is_some())
        .expect("superseded evidence");
    let authority_id = invalid_supersession.documentation[superseded]
        .superseded_by
        .clone()
        .expect("authority");
    let authority = invalid_supersession
        .documentation
        .iter_mut()
        .find(|document| document.id == authority_id)
        .expect("authority row");
    authority.repository = invalid_supersession.repositories[1].id.clone();
    assert_code(
        invalid_supersession.validate(),
        "DOCUMENTATION_SUPERSESSION_INVALID",
    );
}

#[test]
fn f07_required_context_ids_cannot_exchange_cargo_and_frontend_kinds() {
    let mut manifest = super::manifest();
    let cargo = manifest
        .contexts
        .iter()
        .position(|context| context.id.as_str() == "cargo-bbnf-root")
        .expect("Cargo context");
    let frontend = manifest
        .contexts
        .iter()
        .position(|context| context.id.as_str() == "frontend-bbnf-root")
        .expect("frontend context");
    let cargo_id = manifest.contexts[cargo].id.clone();
    manifest.contexts[cargo].id = manifest.contexts[frontend].id.clone();
    manifest.contexts[frontend].id = cargo_id;
    let error = manifest.validate().expect_err("typed context ID swap");
    assert!(error.to_string().contains("CONTEXT_ID_KIND_MISMATCH"));
}

#[test]
fn f07_current_p3_and_p31_governing_supersession_documents_are_bound() {
    let manifest = super::manifest();
    let rows = manifest
        .documentation
        .iter()
        .map(|document| document.id.as_str())
        .collect::<std::collections::BTreeSet<_>>();
    for required in [
        "p3-verdict-authority",
        "p3-evidence",
        "p3-review-a-evidence",
        "p3-review-b-evidence",
        "p31-gestalt1-verdict-authority",
        "p31-gestalt1-critic-a-evidence",
        "p31-gestalt1-critic-b-evidence",
        "p31-gestalt1-critic-c-evidence",
        "p31-gestalt2-verdict-authority",
        "p31-repair-author-evidence",
        "p31-gestalt2-critic-a-evidence",
        "p31-gestalt2-critic-b-evidence",
    ] {
        assert!(
            rows.contains(required),
            "missing governing document {required}"
        );
    }
    assert!(manifest.documentation.iter().all(|document| {
        document.role != xtask::architecture::manifest::DocumentationRole::SupersededEvidence
            || document.superseded_by.is_some()
    }));
}

#[test]
fn f07_every_declared_physical_role_requires_the_exact_kind() {
    let manifest = super::manifest();
    let inventory = physical_role_inventory(&manifest);
    inventory.validate(&manifest).expect("role inventory");

    let mut wrong_lock = inventory.clone();
    let lock = wrong_lock
        .records
        .iter_mut()
        .find(|record| record.identity.path.as_str() == "Cargo.lock")
        .expect("lock");
    lock.identity.kind = PhysicalKind::Directory;
    lock.content_hash = None;
    assert_code(wrong_lock.validate(&manifest), "CONTEXT_LOCK_KIND_MISMATCH");

    let mut missing_document = inventory.clone();
    let document = &manifest.documentation[0];
    missing_document.records.retain(|record| {
        record.identity.repository != document.repository || record.identity.path != document.path
    });
    assert_code(
        missing_document.validate(&manifest),
        "DOCUMENT_PATH_KIND_MISMATCH",
    );

    let mut symlink_root = inventory;
    let root = &manifest.package_faces[0].root;
    let repository = &manifest.package_faces[0].repository;
    let record = symlink_root
        .records
        .iter_mut()
        .find(|record| &record.identity.repository == repository && &record.identity.path == root)
        .expect("root");
    record.identity.kind = PhysicalKind::Symlink;
    record.content_hash = Some(Sha256Digest::of(b"target"));
    assert_code(
        symlink_root.validate(&manifest),
        "PACKAGE_FACE_ROOT_KIND_MISMATCH",
    );

    for (role, code) in [
        ("face-manifest", "PACKAGE_FACE_MANIFEST_KIND_MISMATCH"),
        ("workspace-manifest", "WORKSPACE_MANIFEST_KIND_MISMATCH"),
        ("context-manifest", "CONTEXT_MANIFEST_KIND_MISMATCH"),
        ("context-config", "CONTEXT_CONFIG_KIND_MISMATCH"),
    ] {
        let mut probe_manifest = manifest.clone();
        let probe = RepoPath::new(format!("__physical-role-probe/{role}")).expect("probe path");
        let repository = match role {
            "face-manifest" => {
                probe_manifest.package_faces[0].manifest = probe.clone();
                probe_manifest.package_faces[0].repository.clone()
            }
            "workspace-manifest" => {
                probe_manifest.workspaces[0].manifest = probe.clone();
                probe_manifest.workspaces[0].repository.clone()
            }
            "context-manifest" => {
                probe_manifest.contexts[0].manifest = probe.clone();
                probe_manifest.contexts[0].repository.clone()
            }
            "context-config" => {
                probe_manifest.contexts[0].config_inputs = vec![probe.clone()];
                probe_manifest.contexts[0].repository.clone()
            }
            _ => unreachable!(),
        };
        let mut probe_inventory = physical_role_inventory(&probe_manifest);
        let record = probe_inventory
            .records
            .iter_mut()
            .find(|record| {
                record.identity.repository == repository && record.identity.path == probe
            })
            .expect("probe role");
        record.identity.kind = PhysicalKind::Directory;
        record.content_hash = None;
        assert_code(probe_inventory.validate(&probe_manifest), code);
    }
}

#[test]
fn f08_unsupported_or_overbroad_exclusion_grammar_is_rejected() {
    for pattern in [".profiles", "../outside/**", "**/**", "**/nested/path/**"] {
        let mut manifest = super::manifest();
        manifest.exclusions[0].pattern =
            xtask::architecture::schema::CanonicalText::new(pattern).expect("pattern text");
        let error = manifest.validate().expect_err("unsupported exclusion");
        assert!(
            error.to_string().contains("EXCLUSION_PATTERN_UNSUPPORTED"),
            "{pattern}: {error:#}"
        );
    }
}

fn physical_role_inventory(
    manifest: &xtask::architecture::manifest::ConstellationManifest,
) -> Inventory {
    let mut roles = BTreeMap::new();
    for face in &manifest.package_faces {
        roles.insert(
            (face.repository.clone(), face.root.clone()),
            PhysicalKind::Directory,
        );
        roles.insert(
            (face.repository.clone(), face.manifest.clone()),
            PhysicalKind::File,
        );
    }
    for workspace in &manifest.workspaces {
        roles.insert(
            (workspace.repository.clone(), workspace.manifest.clone()),
            PhysicalKind::File,
        );
    }
    for context in &manifest.contexts {
        for path in std::iter::once(&context.manifest)
            .chain(std::iter::once(&context.lock))
            .chain(&context.config_inputs)
        {
            roles.insert(
                (context.repository.clone(), path.clone()),
                PhysicalKind::File,
            );
        }
    }
    for document in &manifest.documentation {
        roles.insert(
            (document.repository.clone(), document.path.clone()),
            PhysicalKind::File,
        );
    }
    Inventory {
        schema: "bbnf.architecture.inventory.v3.1".into(),
        records: roles
            .into_iter()
            .map(|((repository, path), kind)| InventoryRecord {
                identity: PhysicalIdentity {
                    repository,
                    path: RepoPath::new(path.as_str()).expect("path"),
                    kind: kind.clone(),
                },
                content_hash: (kind != PhysicalKind::Directory).then(|| Sha256Digest::of(b"role")),
                semantic_faces: Vec::new(),
                exclusion: None,
            })
            .collect(),
    }
}

fn assert_code(result: anyhow::Result<()>, code: &str) {
    let error = result.expect_err(code);
    assert!(error.to_string().contains(code), "{error:#}");
}
