use std::fs;

use bbnf::imports::{load_module_graph, ImportError};

fn setup_test_dir() -> tempfile::TempDir {
    tempfile::tempdir().expect("Failed to create temp dir")
}

#[test]
fn test_glob_import() {
    let dir = setup_test_dir();
    let base_path = dir.path().join("base.bbnf");
    let main_path = dir.path().join("main.bbnf");

    fs::write(&base_path, "number = /[0-9]+/;\nstring = /[a-z]+/;").unwrap();
    fs::write(
        &main_path,
        r#"@import "base.bbnf";
value = number | string;"#,
    )
    .unwrap();

    let registry = load_module_graph(&main_path).unwrap();
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);

    let imported = registry.imported_rule_names(&main_path.canonicalize().unwrap());
    assert!(imported.contains("number"));
    assert!(imported.contains("string"));
}

#[test]
fn test_selective_import() {
    let dir = setup_test_dir();
    let base_path = dir.path().join("base.bbnf");
    let main_path = dir.path().join("main.bbnf");

    fs::write(
        &base_path,
        "number = /[0-9]+/;\nstring = /[a-z]+/;\nident = /[a-zA-Z_]+/;",
    )
    .unwrap();
    fs::write(
        &main_path,
        r#"@import { number, string } from "base.bbnf";
value = number | string;"#,
    )
    .unwrap();

    let registry = load_module_graph(&main_path).unwrap();
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);

    let imported = registry.imported_rule_names(&main_path.canonicalize().unwrap());
    assert!(imported.contains("number"));
    assert!(imported.contains("string"));
    assert!(!imported.contains("ident"), "ident should not be imported");
}

#[test]
fn test_diamond_dependency() {
    let dir = setup_test_dir();
    let common = dir.path().join("common.bbnf");
    let a = dir.path().join("a.bbnf");
    let b = dir.path().join("b.bbnf");
    let main = dir.path().join("main.bbnf");

    fs::write(&common, r"ws = /\s+/;").unwrap();
    fs::write(
        &a,
        r#"@import "common.bbnf";
a = ws;"#,
    )
    .unwrap();
    fs::write(
        &b,
        r#"@import "common.bbnf";
b = ws;"#,
    )
    .unwrap();
    fs::write(
        &main,
        r#"@import "a.bbnf";
@import "b.bbnf";
entry = a | b;"#,
    )
    .unwrap();

    let registry = load_module_graph(&main).unwrap();
    // common.bbnf should be loaded only once (dedup).
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
}

#[test]
fn test_circular_import_allowed() {
    let dir = setup_test_dir();
    let a = dir.path().join("a.bbnf");
    let b = dir.path().join("b.bbnf");

    fs::write(
        &a,
        r#"@import "b.bbnf";
a = /x/;"#,
    )
    .unwrap();
    fs::write(
        &b,
        r#"@import "a.bbnf";
b = /y/;"#,
    )
    .unwrap();

    let registry = load_module_graph(&a).unwrap();
    // Cyclic imports are now allowed (Python-style partial-init).
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
    // Both modules should be loaded.
    let a_canon = a.canonicalize().unwrap();
    let b_canon = b.canonicalize().unwrap();
    assert!(
        registry.get_module(&a_canon).is_some(),
        "Module a should be loaded"
    );
    assert!(
        registry.get_module(&b_canon).is_some(),
        "Module b should be loaded"
    );
    // A can see B's rules.
    let imported = registry.imported_rule_names(&a_canon);
    assert!(imported.contains("b"), "A should see B's rule 'b'");
}

#[test]
fn test_missing_file() {
    let dir = setup_test_dir();
    let main = dir.path().join("main.bbnf");
    fs::write(
        &main,
        r#"@import "nonexistent.bbnf";
value = /x/;"#,
    )
    .unwrap();

    let registry = load_module_graph(&main).unwrap();
    let file_errors: Vec<_> = registry
        .errors
        .iter()
        .filter(|e| matches!(e, ImportError::FileNotFound { .. }))
        .collect();
    assert!(!file_errors.is_empty(), "Expected file not found error");
}

#[test]
fn test_missing_rule_in_selective_import() {
    let dir = setup_test_dir();
    let base = dir.path().join("base.bbnf");
    let main = dir.path().join("main.bbnf");

    fs::write(&base, "number = /[0-9]+/;").unwrap();
    fs::write(
        &main,
        r#"@import { number, nonexistent } from "base.bbnf";
value = number;"#,
    )
    .unwrap();

    let registry = load_module_graph(&main).unwrap();
    let missing_errors: Vec<_> = registry
        .errors
        .iter()
        .filter(|e| matches!(e, ImportError::MissingRule { .. }))
        .collect();
    assert!(
        !missing_errors.is_empty(),
        "Expected missing rule error"
    );
}

#[test]
fn test_non_transitive() {
    let dir = setup_test_dir();
    let c = dir.path().join("c.bbnf");
    let b = dir.path().join("b.bbnf");
    let a = dir.path().join("a.bbnf");

    fs::write(&c, "c_rule = /c/;").unwrap();
    fs::write(
        &b,
        r#"@import "c.bbnf";
b_rule = c_rule;"#,
    )
    .unwrap();
    fs::write(
        &a,
        r#"@import "b.bbnf";
a_rule = b_rule;"#,
    )
    .unwrap();

    let registry = load_module_graph(&a).unwrap();
    let imported = registry.imported_rule_names(&a.canonicalize().unwrap());
    // A imports B, B imports C. A should see b_rule but NOT c_rule.
    assert!(imported.contains("b_rule"), "Should import b_rule");
    assert!(
        !imported.contains("c_rule"),
        "Should NOT see c_rule (non-transitive)"
    );
}

#[test]
fn test_three_way_cycle() {
    let dir = setup_test_dir();
    let a = dir.path().join("a.bbnf");
    let b = dir.path().join("b.bbnf");
    let c = dir.path().join("c.bbnf");

    fs::write(
        &a,
        r#"@import "b.bbnf";
a = /x/;"#,
    )
    .unwrap();
    fs::write(
        &b,
        r#"@import "c.bbnf";
b = /y/;"#,
    )
    .unwrap();
    fs::write(
        &c,
        r#"@import "a.bbnf";
c = /z/;"#,
    )
    .unwrap();

    let registry = load_module_graph(&a).unwrap();
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
    assert!(registry.get_module(&a.canonicalize().unwrap()).is_some());
    assert!(registry.get_module(&b.canonicalize().unwrap()).is_some());
    assert!(registry.get_module(&c.canonicalize().unwrap()).is_some());
}

#[test]
fn test_self_import() {
    let dir = setup_test_dir();
    let a = dir.path().join("a.bbnf");
    fs::write(
        &a,
        r#"@import "a.bbnf";
a = /x/;"#,
    )
    .unwrap();

    let registry = load_module_graph(&a).unwrap();
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);
    assert!(registry.get_module(&a.canonicalize().unwrap()).is_some());
}

#[test]
fn test_selective_transitive_unfurling() {
    let dir = setup_test_dir();
    let base = dir.path().join("base.bbnf");
    let main = dir.path().join("main.bbnf");

    // percentage depends on number and percentageUnit
    fs::write(
        &base,
        r#"number = /[0-9]+/;
percentageUnit = "%";
percentage = number, percentageUnit;"#,
    )
    .unwrap();
    fs::write(
        &main,
        r#"@import { percentage } from "base.bbnf";
value = percentage;"#,
    )
    .unwrap();

    let registry = load_module_graph(&main).unwrap();
    assert!(registry.errors.is_empty(), "Errors: {:?}", registry.errors);

    let imported = registry.imported_rule_names(&main.canonicalize().unwrap());
    assert!(
        imported.contains("percentage"),
        "Should import percentage"
    );
    assert!(
        imported.contains("number"),
        "Should unfurl transitive dep: number"
    );
    assert!(
        imported.contains("percentageUnit"),
        "Should unfurl transitive dep: percentageUnit"
    );
}
