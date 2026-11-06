use std::process::Command;

use bbnf_type_kernel_v4::*;

use super::super::{operations, temp_dir, toolchain};

#[test]
fn independent_rust_and_typescript_emitters_compile_real_hostile_values() {
    let solution = projection_solution();
    let mut operations = operations();
    let plan = lower(
        &solution,
        TargetProfile {
            semantic: SemanticProfile::ExactValues,
            layout: LayoutProfile::SemanticOnly,
        },
        &mut operations,
    )
    .expect("lower");
    let rust = project(&plan, TargetLanguage::Rust, toolchain(), &mut operations)
        .expect("Rust projection");
    let typescript = project(
        &plan,
        TargetLanguage::TypeScript,
        toolchain(),
        &mut operations,
    )
    .expect("TypeScript projection");
    assert!(rust.source().contains("Shared, Shared"));
    assert!(rust.source().contains("AlphaBeta, AlphaBeta2"));
    assert!(
        typescript
            .source()
            .contains("((arg0: number) => boolean) | null")
    );
    compile_rust(rust.source());
    compile_typescript(typescript.source());
    assert!(
        rust.manifest()
            .contains(solution.semantic_digest().as_str())
    );
    assert!(
        typescript
            .manifest()
            .contains(solution.semantic_digest().as_str())
    );
}

#[test]
fn two_independent_generations_are_byte_identical_and_long_names_are_charged() {
    let first_solution = projection_solution();
    let _unrelated = super::super::primitive_solution(Primitive::U64, "unrelated");
    let second_solution = projection_solution();
    assert_eq!(
        first_solution.semantic_digest(),
        second_solution.semantic_digest()
    );
    let first = generate(&first_solution);
    let second = generate(&second_solution);
    assert_eq!(first, second);

    let mut setup = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder.primitive(Primitive::I64, &mut setup).expect("type");
    let huge = "x".repeat(1_000_000);
    let error = builder
        .close(
            [(ExportKey::new(huge), ty)],
            &mut OperationContext::new(256),
        )
        .expect_err("large export must exhaust before publication");
    assert!(matches!(error.kind(), TypeErrorKind::Exhausted { .. }));
}

#[test]
fn projection_rejects_open_and_recursive_semantics_before_compilers() {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let meta = builder.meta(1, &mut operations).expect("meta");
    let (problem, _) = builder
        .close([(ExportKey::new("Open"), meta)], &mut operations)
        .expect("close");
    let solution = solve(problem, &mut operations).expect("solve");
    let plan = lower(
        &solution,
        TargetProfile {
            semantic: SemanticProfile::ExactValues,
            layout: LayoutProfile::SemanticOnly,
        },
        &mut operations,
    )
    .expect("lower");
    let error =
        project(&plan, TargetLanguage::Rust, toolchain(), &mut operations).expect_err("open type");
    assert_eq!(error.kind(), &TypeErrorKind::OpenTypeVariable);
}

fn projection_solution() -> TypeSolution {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let shared = builder.named("Shared", &mut operations).expect("shared");
    let collision_a = builder
        .named("Alpha-Beta", &mut operations)
        .expect("collision a");
    let collision_b = builder
        .named("Alpha Beta", &mut operations)
        .expect("collision b");
    let number = builder
        .primitive(Primitive::F64, &mut operations)
        .expect("number");
    let boolean = builder
        .primitive(Primitive::Bool, &mut operations)
        .expect("boolean");
    let function = builder
        .function([number], boolean, &mut operations)
        .expect("function");
    let maybe_function = builder
        .option(function, &mut operations)
        .expect("option fn");
    let inner_row = builder
        .row(
            RowKind::Record,
            [
                ("type".to_owned(), number),
                ("slash/name".to_owned(), boolean),
            ],
            None,
            Origin::boundary("inner"),
            &mut operations,
        )
        .expect("inner row");
    let inner = builder
        .record(inner_row, &mut operations)
        .expect("inner record");
    let outer_row = builder
        .row(
            RowKind::Record,
            [("inner".to_owned(), inner)],
            None,
            Origin::boundary("outer"),
            &mut operations,
        )
        .expect("outer row");
    let outer = builder
        .record(outer_row, &mut operations)
        .expect("outer record");
    let unit = builder
        .primitive(Primitive::Unit, &mut operations)
        .expect("unit");
    let variant_row = builder
        .row(
            RowKind::Variant,
            [("some-value".to_owned(), outer), ("none".to_owned(), unit)],
            None,
            Origin::boundary("variant"),
            &mut operations,
        )
        .expect("variant row");
    let variant = builder
        .variant(variant_row, &mut operations)
        .expect("variant");
    let empty_row = builder
        .row(
            RowKind::Variant,
            [],
            None,
            Origin::boundary("empty"),
            &mut operations,
        )
        .expect("empty row");
    let empty = builder
        .variant(empty_row, &mut operations)
        .expect("empty variant");
    let payload = builder
        .tuple(
            [
                shared,
                shared,
                collision_a,
                collision_b,
                maybe_function,
                outer,
                variant,
            ],
            &mut operations,
        )
        .expect("payload");
    let (problem, _) = builder
        .close(
            [
                (ExportKey::new("Empty"), empty),
                (ExportKey::new("Payload"), payload),
            ],
            &mut operations,
        )
        .expect("close");
    solve(problem, &mut operations).expect("solve")
}

fn generate(solution: &TypeSolution) -> (String, String, String, String) {
    let mut operations = operations();
    let plan = lower(
        solution,
        TargetProfile {
            semantic: SemanticProfile::ExactValues,
            layout: LayoutProfile::SemanticOnly,
        },
        &mut operations,
    )
    .expect("lower");
    let rust = project(&plan, TargetLanguage::Rust, toolchain(), &mut operations).expect("Rust");
    let typescript = project(
        &plan,
        TargetLanguage::TypeScript,
        toolchain(),
        &mut operations,
    )
    .expect("TypeScript");
    (
        rust.source().to_owned(),
        rust.manifest().to_owned(),
        typescript.source().to_owned(),
        typescript.manifest().to_owned(),
    )
}

fn compile_rust(generated: &str) {
    let directory = temp_dir("rust-crater");
    let source = directory.join("artifact.rs");
    let consumer = format!(
        "{generated}\n#[derive(Clone, Debug, PartialEq)] pub struct Shared;\n#[derive(Clone, Debug, PartialEq)] pub struct AlphaBeta;\n#[derive(Clone, Debug, PartialEq)] pub struct AlphaBeta2;\npub fn value() -> Payload {{ let nested = KernelRow3 {{ inner: KernelRow2 {{ slash_name: true, type_: 1.0 }} }}; let variant_nested = KernelRow5 {{ inner: KernelRow4 {{ slash_name: false, type_: 2.0 }} }}; (Shared, Shared, AlphaBeta, AlphaBeta2, Some(|x| x > 0.0), nested, KernelRow6::SomeValue(variant_nested)) }}\n"
    );
    std::fs::write(&source, consumer).expect("write Rust crater");
    let output = Command::new("rustc")
        .args(["--edition=2024", "-D", "warnings", "--crate-type=lib"])
        .arg(&source)
        .arg("-o")
        .arg(directory.join("libartifact.rlib"))
        .output()
        .expect("run rustc");
    assert!(
        output.status.success(),
        "rustc failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    std::fs::remove_dir_all(directory).expect("remove Rust crater");
}

fn compile_typescript(generated: &str) {
    let directory = temp_dir("ts-crater");
    let source = directory.join("artifact.ts");
    let consumer = format!(
        "type Shared = {{ readonly shared: true }};\ntype Alpha_Beta = {{ readonly a: true }};\ntype Alpha_Beta_2 = {{ readonly b: true }};\n{generated}\nconst nested = {{ inner: {{ 'slash/name': true, type: 1 }} }} as const;\nconst payload: Payload = [{{shared:true}}, {{shared:true}}, {{a:true}}, {{b:true}}, null, nested, {{tag:'some-value', value:nested}}];\nvoid payload;\n"
    );
    std::fs::write(&source, consumer).expect("write TypeScript crater");
    let output = Command::new("npx")
        .args([
            "--yes",
            "tsc",
            "--strict",
            "--noEmit",
            "--lib",
            "es2022,dom",
        ])
        .arg(&source)
        .output()
        .expect("run tsc");
    assert!(
        output.status.success(),
        "tsc failed: stdout={} stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    std::fs::remove_dir_all(directory).expect("remove TypeScript crater");
}
