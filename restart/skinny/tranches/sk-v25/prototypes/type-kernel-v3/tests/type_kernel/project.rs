use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

use bbnf_type_kernel_v3::type_kernel::OperationContext;
use bbnf_type_kernel_v3::type_kernel::Origin;
use bbnf_type_kernel_v3::type_kernel::project::{
    IdentifierAllocator, IdentifierStyle, TargetLanguage, ToolchainIdentity, generate,
};
use bbnf_type_kernel_v3::type_kernel::solve::{Constraint, SolvedType, solve};
use bbnf_type_kernel_v3::type_kernel::store::{Primitive, RowSpec, StoreBuilder};

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

fn hostile_record() -> SolvedType {
    let mut builder = StoreBuilder::new();
    let i64_ty = builder.primitive(Primitive::I64).unwrap();
    let singleton = builder.tuple([i64_ty]).unwrap();
    let row = RowSpec::closed([
        ("type", i64_ty),
        ("bad-label", i64_ty),
        ("foo_bar", i64_ty),
        ("foo__bar", i64_ty),
        ("9lives", i64_ty),
        ("λ", i64_ty),
        ("singleton", singleton),
    ])
    .unwrap();
    let root = builder.record(row).unwrap();
    let store = builder.finish([root]).unwrap();
    let mut operations = OperationContext::unlimited();
    solve(
        &store,
        &[Constraint::equal(
            root,
            root,
            Origin::new("project", "closed hostile record"),
        )],
        [root],
        &mut operations,
    )
    .unwrap()
    .type_of(root)
    .unwrap()
    .clone()
}

#[test]
fn target_allocators_handle_keywords_unicode_empty_leading_digits_and_collisions() {
    for target in [TargetLanguage::Rust, TargetLanguage::TypeScript] {
        let mut types = IdentifierAllocator::new(target, IdentifierStyle::Type);
        let values = [
            types.allocate(""),
            types.allocate("9lives"),
            types.allocate("foo_bar"),
            types.allocate("foo__bar"),
            types.allocate("λ"),
            types.allocate(if target == TargetLanguage::Rust {
                "Self"
            } else {
                "class"
            }),
        ];
        assert!(values.iter().all(|value| !value.is_empty()));
        assert!(values[1].starts_with("Bbnf"));
        assert_ne!(values[2], values[3]);
        assert!(values[4].contains("03bb"));
        if target == TargetLanguage::Rust {
            assert_ne!(values[5], "Self");
        }
    }

    let mut rust_fields = IdentifierAllocator::new(TargetLanguage::Rust, IdentifierStyle::Field);
    assert_eq!(rust_fields.allocate("type"), "bbnf_type");
    assert_eq!(rust_fields.allocate("bad-label"), "bad_label");
    assert_eq!(rust_fields.allocate("foo_bar"), "foo_bar");
    assert_eq!(rust_fields.allocate("foo__bar"), "foo_bar_2");
    let mut ts_fields =
        IdentifierAllocator::new(TargetLanguage::TypeScript, IdentifierStyle::Field);
    assert_eq!(ts_fields.allocate("class"), "bbnf_class");
}

#[test]
fn hostile_rust_and_typescript_values_compile_with_real_toolchains() {
    let ty = hostile_record();
    let rust_version = command_version("rustc", &["--version"]);
    let typescript_version = command_version("npx", &["--yes", "tsc", "--version"]);
    let mut rust_ops = OperationContext::unlimited();
    let rust = generate(
        TargetLanguage::Rust,
        "type",
        &ty,
        ToolchainIdentity::new("rustc", rust_version),
        "rustc --edition=2024 -D warnings --crate-type=lib hostile.rs",
        &mut rust_ops,
    )
    .unwrap();
    let rust_consumer = format!(
        "{}\npub fn hostile_value() -> {} {{\n    {} {{\n        bbnf_9lives: 9,\n        bad_label: 1,\n        foo_bar: 2,\n        foo_bar_2: 3,\n        singleton: (7,),\n        bbnf_type: 4,\n        u03bb: 5,\n    }}\n}}\n",
        rust.source(),
        rust.exported_name(),
        rust.exported_name()
    );

    let mut ts_ops = OperationContext::unlimited();
    let typescript = generate(
        TargetLanguage::TypeScript,
        "class",
        &ty,
        ToolchainIdentity::new("tsc", typescript_version),
        "npx --yes tsc --strict --noEmit --lib es2022,dom hostile.ts",
        &mut ts_ops,
    )
    .unwrap();
    let ts_consumer = format!(
        "{}\nexport const hostileValue: {} = {{\n  \"9lives\": 9,\n  \"bad-label\": 1,\n  \"foo__bar\": 3,\n  \"foo_bar\": 2,\n  \"singleton\": [7],\n  \"type\": 4,\n  \"λ\": 5,\n}};\n",
        typescript.source(),
        typescript.exported_name()
    );

    let directory = temp_directory();
    fs::create_dir_all(&directory).unwrap();
    let rust_path = directory.join("hostile.rs");
    let ts_path = directory.join("hostile.ts");
    fs::write(&rust_path, rust_consumer).unwrap();
    fs::write(&ts_path, ts_consumer).unwrap();

    let rust_output = Command::new("rustc")
        .args(["--edition=2024", "-D", "warnings", "--crate-type=lib"])
        .arg(&rust_path)
        .arg("-o")
        .arg(directory.join("libhostile.rlib"))
        .output()
        .unwrap();
    assert!(
        rust_output.status.success(),
        "rustc failed:\n{}",
        String::from_utf8_lossy(&rust_output.stderr)
    );

    let ts_output = Command::new("npx")
        .args([
            "--yes",
            "tsc",
            "--strict",
            "--noEmit",
            "--lib",
            "es2022,dom",
        ])
        .arg(&ts_path)
        .output()
        .unwrap();
    assert!(
        ts_output.status.success(),
        "tsc failed:\n{}",
        String::from_utf8_lossy(&ts_output.stderr)
    );
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn clean_generations_are_byte_identical_and_provenance_complete() {
    let ty = hostile_record();
    let mut first_ops = OperationContext::unlimited();
    let first = generate(
        TargetLanguage::Rust,
        "payload",
        &ty,
        ToolchainIdentity::new("rustc", "test"),
        "rustc payload.rs",
        &mut first_ops,
    )
    .unwrap();
    let mut second_ops = OperationContext::unlimited();
    let second = generate(
        TargetLanguage::Rust,
        "payload",
        &ty,
        ToolchainIdentity::new("rustc", "test"),
        "rustc payload.rs",
        &mut second_ops,
    )
    .unwrap();
    assert_eq!(first.source(), second.source());
    assert_eq!(first.provenance(), second.provenance());
    assert!(!first.provenance().source_digest().is_empty());
    assert!(!first.provenance().semantic_manifest_digest().is_empty());
    assert!(!first.provenance().generator_version().is_empty());
    assert!(!first.provenance().target_toolchain().name().is_empty());
    assert!(!first.provenance().target_toolchain().version().is_empty());
    assert!(!first.provenance().reproduction_command().is_empty());
}

fn temp_directory() -> PathBuf {
    std::env::temp_dir().join(format!(
        "bbnf-type-kernel-v3-{}-{}",
        std::process::id(),
        NEXT_TEMP.fetch_add(1, Ordering::Relaxed)
    ))
}

fn command_version(program: &str, arguments: &[&str]) -> String {
    let output = Command::new(program).args(arguments).output().unwrap();
    assert!(output.status.success(), "{program} version probe failed");
    String::from_utf8(output.stdout).unwrap().trim().to_owned()
}
