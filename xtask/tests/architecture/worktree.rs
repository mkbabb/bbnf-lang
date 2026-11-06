use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;
use xtask::architecture::inventory::{
    WorktreeRecord, collect_worktree_census_with_git, collect_worktrees,
    reconcile_tracked_exclusions, resolve_nominations,
};
use xtask::architecture::manifest::ResolvedRepository;
use xtask::architecture::schema::{CanonicalText, RepositoryId, Sha256Digest, WorktreeId};
use xtask::architecture::test_support::{RunEpoch, requested_worker_policy_for_test};

#[test]
fn f09_nomination_resolves_exactly_one_measured_same_repository_worktree() {
    let manifest = super::manifest();
    let records = vec![
        record("bbnf", "primary", "bbnf-primary-measured"),
        record("parse-that", "primary", "parse-primary-measured"),
    ];
    let nominations = resolve_nominations(&manifest, &records).expect("nominations");
    assert_eq!(nominations.len(), 2);
    assert!(nominations.iter().all(|nomination| {
        records.iter().any(|record| {
            record.identity == nomination.worktree && record.repository == nomination.repository
        })
    }));
    assert!(nominations.iter().all(|nomination| {
        nomination.worktree != nomination.nomination
            && records
                .iter()
                .any(|record| record.identity == nomination.worktree)
    }));

    let error = resolve_nominations(&manifest, &records[1..]).expect_err("absent primary");
    assert!(error.to_string().contains("PRODUCT_WORKTREE_UNNOMINATED"));

    let mut ambiguous = records;
    ambiguous.push(record("bbnf", "primary", "bbnf-primary-second"));
    let error = resolve_nominations(&manifest, &ambiguous).expect_err("ambiguous primary");
    assert!(error.to_string().contains("PRODUCT_WORKTREE_UNNOMINATED"));

    let cross_repository = vec![
        record("parse-that", "primary", "bbnf-primary-in-wrong-repository"),
        record("parse-that", "linked", "parse-linked"),
    ];
    let error = resolve_nominations(&manifest, &cross_repository).expect_err("cross repository");
    assert!(error.to_string().contains("PRODUCT_WORKTREE_UNNOMINATED"));
}

#[test]
fn f10_administrative_ids_survive_branch_spelling_detachment_and_dirty_state() {
    let fixture = git_fixture();
    let branch_slash = fixture.path().join("branch-slash");
    let branch_dash = fixture.path().join("branch-dash");
    let detached_a = fixture.path().join("detached-a");
    let detached_b = fixture.path().join("detached-b");
    git(
        &fixture.path().join("repo"),
        &["worktree", "add", "-b", "feature/a"],
        Some(&branch_slash),
    );
    git(
        &fixture.path().join("repo"),
        &["worktree", "add", "-b", "feature-a"],
        Some(&branch_dash),
    );
    git(
        &fixture.path().join("repo"),
        &["worktree", "add", "--detach"],
        Some(&detached_a),
    );
    git(
        &fixture.path().join("repo"),
        &["worktree", "add", "--detach"],
        Some(&detached_b),
    );

    let repository = resolved(&fixture.path().join("repo"));
    let before = collect_worktrees(std::slice::from_ref(&repository)).expect("worktrees");
    assert_eq!(
        before
            .iter()
            .map(|record| &record.identity)
            .collect::<std::collections::BTreeSet<_>>()
            .len(),
        before.len()
    );
    assert!(
        before.iter().any(|record| {
            record.branch.as_ref().map(CanonicalText::as_str) == Some("feature/a")
        })
    );
    assert!(
        before.iter().any(|record| {
            record.branch.as_ref().map(CanonicalText::as_str) == Some("feature-a")
        })
    );
    let detached_before = before
        .iter()
        .filter(|record| record.detached)
        .map(|record| (record.administrative_key.clone(), record.identity.clone()))
        .collect::<std::collections::BTreeMap<_, _>>();
    fs::write(detached_a.join("dirty.txt"), "dirty").expect("dirty");
    let after = collect_worktrees(&[repository]).expect("worktrees after mutation");
    for record in after.iter().filter(|record| record.detached) {
        assert_eq!(
            detached_before.get(&record.administrative_key),
            Some(&record.identity)
        );
    }
}

#[test]
fn f11_ignored_policy_is_explicit_and_nonignored_untracked_bytes_are_hashed() {
    let fixture = git_fixture();
    let repository = resolved(&fixture.path().join("repo"));
    let initial = primary(collect_worktrees(std::slice::from_ref(&repository)).expect("initial"));
    fs::write(fixture.path().join("repo/ignored.bin"), "ignored").expect("ignored");
    let ignored = primary(collect_worktrees(std::slice::from_ref(&repository)).expect("ignored"));
    assert_eq!(initial.content_hash, ignored.content_hash);
    assert!(!ignored.dirty);
    assert!(
        ignored
            .ignored_content_policy
            .as_str()
            .contains("source-bound")
    );

    fs::write(fixture.path().join("repo/ordinary.txt"), "ordinary").expect("ordinary");
    let ordinary = primary(collect_worktrees(std::slice::from_ref(&repository)).expect("ordinary"));
    assert_ne!(ignored.content_hash, ordinary.content_hash);
    assert!(ordinary.dirty);
    fs::write(fixture.path().join("repo/tracked.txt"), "tracked mutation").expect("tracked");
    let tracked = primary(collect_worktrees(&[repository]).expect("tracked mutation"));
    assert_ne!(ordinary.content_hash, tracked.content_hash);
}

#[test]
fn f18_git_truth_failpoint_is_operation_local() {
    let fixture = git_fixture();
    let repository = resolved(&fixture.path().join("repo"));
    let epoch = RunEpoch::for_test(Some("git-command"), None).expect("Git epoch");
    let scope = epoch.scope().expect("Git epoch scope");
    let error = collect_worktrees(&[repository]).expect_err("Git truth command");
    drop(scope);
    assert_eq!(
        error.to_string(),
        "INJECTED_FAILURE at real operation `git-command`",
        "{error:#}"
    );
    super::assert_epoch(&epoch, 1, false, false);
}

#[test]
fn f10_canonical_common_dir_identity_ignores_external_alias_spelling() {
    let fixture = git_fixture();
    let root = fixture.path().join("repo");
    let linked = fixture.path().join("linked");
    git(&root, &["worktree", "add", "--detach"], Some(&linked));
    let repository = resolved(&root);
    let before = collect_worktrees(std::slice::from_ref(&repository)).expect("before");
    let before = before
        .iter()
        .find(|record| record.administrative_key.as_str().starts_with("worktrees/"))
        .expect("linked record")
        .identity
        .clone();

    let original = fs::read_to_string(linked.join(".git")).expect("linked .git");
    let real = std::path::PathBuf::from(
        original
            .trim()
            .strip_prefix("gitdir: ")
            .expect("gitdir row"),
    );
    let external = fixture.path().join("external/worktrees");
    fs::create_dir_all(&external).expect("external worktrees");
    let alias = external.join("alias");
    std::os::unix::fs::symlink(&real, &alias).expect("administrative alias");
    fs::write(
        linked.join(".git"),
        format!("gitdir: {}\n", alias.display()),
    )
    .expect("redirect");

    let after = collect_worktrees(&[repository]).expect("after");
    let after = after
        .iter()
        .find(|record| record.administrative_key.as_str().starts_with("worktrees/"))
        .expect("linked record")
        .identity
        .clone();
    assert_eq!(before, after);
}

#[test]
fn f18_one_global_hash_queue_has_a_hard_worker_and_memory_cap() {
    let fixture = git_fixture();
    let root = fixture.path().join("repo");
    for index in 0..64 {
        fs::write(
            root.join(format!("untracked-{index:03}.bin")),
            [index as u8; 64],
        )
        .expect("untracked file");
    }
    let census =
        collect_worktree_census_with_git(&[resolved(&root)], Path::new("git")).expect("census");
    let available = std::thread::available_parallelism()
        .map(|count| count.get())
        .unwrap_or(1);
    let requested = requested_worker_policy_for_test(9999, 64, available);
    assert_eq!(requested, 18.min(available).min(64));
    assert!(census.hashing.workers <= requested);
    assert_eq!(census.hashing.buffer_bytes_per_worker, 512 * 1024);
    assert_eq!(census.hashing.jobs, 64);
}

#[test]
fn f19_git_census_ignores_ambient_redirection_and_unbound_exclude_sources() {
    let legitimate = git_fixture();
    let malicious = git_fixture();
    let malicious_root = malicious.path().join("repo");
    fs::create_dir_all(malicious_root.join("node_modules")).expect("poison directory");
    fs::write(malicious_root.join("node_modules/poison"), "poison").expect("poison file");
    git(&malicious_root, &["add", "."], None);
    git(&malicious_root, &["commit", "-m", "poison"], None);

    let legitimate_root = legitimate.path().join("repo");
    fs::write(legitimate_root.join(".git/info/exclude"), "ordinary.txt\n").expect("info exclude");
    let global_ignore = legitimate.path().join("global-ignore");
    fs::write(&global_ignore, "ordinary.txt\n").expect("global ignore");
    let global_config = legitimate.path().join("global-git-config");
    fs::write(
        &global_config,
        format!("[core]\n\texcludesFile = {}\n", global_ignore.display()),
    )
    .expect("global Git config");

    let output = Command::new(std::env::current_exe().expect("test executable"))
        .args([
            "--ignored",
            "--exact",
            "worktree::f19_git_redirection_child",
            "--nocapture",
        ])
        .env("BBNF_TEST_LEGITIMATE_GIT_ROOT", &legitimate_root)
        .env("GIT_DIR", malicious_root.join(".git"))
        .env("GIT_WORK_TREE", &malicious_root)
        .env("GIT_INDEX_FILE", malicious_root.join(".git/index"))
        .env("GIT_CONFIG_GLOBAL", &global_config)
        .env("GIT_CONFIG_COUNT", "1")
        .env("GIT_CONFIG_KEY_0", "core.excludesFile")
        .env("GIT_CONFIG_VALUE_0", &global_ignore)
        .output()
        .expect("Git redirection child");
    assert!(
        output.status.success(),
        "Git redirection child failed: stdout={} stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
#[ignore = "isolated child for ambient Git redirection counterexample"]
fn f19_git_redirection_child() {
    let root = std::path::PathBuf::from(
        std::env::var_os("BBNF_TEST_LEGITIMATE_GIT_ROOT").expect("legitimate root"),
    );
    let repository = resolved(&root);
    let initial = primary(
        collect_worktrees(std::slice::from_ref(&repository)).expect("initial sanitized census"),
    );
    fs::write(root.join("ordinary.txt"), "ordinary").expect("ordinary untracked input");
    let changed = primary(
        collect_worktrees(std::slice::from_ref(&repository)).expect("changed sanitized census"),
    );
    assert!(changed.dirty);
    assert_ne!(initial.content_hash, changed.content_hash);
    assert!(
        changed
            .ignored_content_policy
            .as_str()
            .contains("info-global-system-excludes-disabled")
    );

    let manifest = super::manifest();
    let bbnf_repository = ResolvedRepository {
        id: RepositoryId::new("bbnf").expect("repository"),
        root,
    };
    reconcile_tracked_exclusions(&manifest, &[bbnf_repository])
        .expect("tracked exclusion census ignores ambient Git redirection");
}

#[test]
fn f19_local_core_worktree_redirection_is_overridden_and_config_bound() {
    let fixture = git_fixture();
    let root = fixture.path().join("repo");
    let repository = resolved(&root);
    let before = primary(
        collect_worktrees(std::slice::from_ref(&repository)).expect("initial local config census"),
    );
    let redirected = fixture.path().join("redirected");
    fs::create_dir(&redirected).expect("redirected root");
    fs::write(redirected.join("poison.txt"), "poison").expect("redirected poison");
    git(
        &root,
        &[
            "config",
            "core.worktree",
            redirected.to_str().expect("path"),
        ],
        None,
    );
    let after = primary(collect_worktrees(&[repository]).expect("bound local config census"));
    assert_eq!(before.content_hash, after.content_hash);
    assert_eq!(before.dirty, after.dirty);
    assert_ne!(
        before.administrative_config_hash,
        after.administrative_config_hash
    );
}

fn record(repository: &str, administrative_key: &str, identity: &str) -> WorktreeRecord {
    WorktreeRecord {
        identity: WorktreeId::new(identity).expect("identity"),
        repository: RepositoryId::new(repository).expect("repository"),
        administrative_key: CanonicalText::new(administrative_key).expect("key"),
        head: CanonicalText::new("0123456789012345678901234567890123456789").expect("head"),
        branch: None,
        detached: false,
        bare: false,
        dirty: false,
        content_hash: Sha256Digest::of(b"worktree"),
        administrative_config_hash: Sha256Digest::of(b"admin-config"),
        ignored_content_policy: CanonicalText::new(
            "ignored-derived-bytes-excluded; collector-readable-bytes-source-bound",
        )
        .expect("policy"),
    }
}

fn git_fixture() -> tempfile::TempDir {
    let fixture = tempdir().expect("fixture");
    let root = fixture.path().join("repo");
    fs::create_dir(&root).expect("repo");
    git(&root, &["init"], None);
    git(
        &root,
        &["config", "user.email", "fixture@example.invalid"],
        None,
    );
    git(&root, &["config", "user.name", "Fixture"], None);
    fs::write(root.join(".gitignore"), "ignored.bin\n").expect("ignore");
    fs::write(root.join("tracked.txt"), "tracked").expect("tracked");
    git(&root, &["add", "."], None);
    git(&root, &["commit", "-m", "fixture"], None);
    fixture
}

fn resolved(root: &Path) -> ResolvedRepository {
    ResolvedRepository {
        id: RepositoryId::new("fixture").expect("repository"),
        root: root.to_path_buf(),
    }
}

fn primary(records: Vec<WorktreeRecord>) -> WorktreeRecord {
    records
        .into_iter()
        .find(|record| record.administrative_key.as_str() == "primary")
        .expect("primary")
}

fn git(root: &Path, args: &[&str], final_path: Option<&Path>) {
    let mut command = Command::new("git");
    command.arg("-C").arg(root).args(args);
    if let Some(path) = final_path {
        command.arg(path);
    }
    let output = command.output().expect("git");
    assert!(
        output.status.success(),
        "git {:?}: {}",
        args,
        String::from_utf8_lossy(&output.stderr)
    );
}
