use std::process::Command;

use bbnf_type_kernel_v4::*;

use super::super::{operations, temp_dir};

#[test]
fn draft_handles_do_not_cross_the_closed_capability_boundary() {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let binder = builder.fresh_rec_var(&mut operations).expect("binder");
    let escaped = builder.rec_var(binder, &mut operations).expect("rec var");
    let closed = builder
        .primitive(Primitive::I64, &mut operations)
        .expect("closed root");
    let error = builder
        .close(
            [
                (ExportKey::new("closed"), closed),
                (ExportKey::new("escaped"), escaped),
            ],
            &mut operations,
        )
        .expect_err("escaping recursive identity must fail closure");
    assert!(matches!(
        error.kind(),
        TypeErrorKind::EscapingRecursion { .. }
    ));
    assert!(!error.proof().nodes().is_empty());

    let crater = temp_dir("draft-compile-fail");
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    std::fs::create_dir_all(crater.join("src")).expect("create source directory");
    std::fs::write(
        crater.join("Cargo.toml"),
        format!(
            "[package]\nname='draft-crater'\nversion='0.0.0'\nedition='2024'\n[dependencies]\nbbnf-type-kernel-v4={{path={manifest_dir:?}}}\n[workspace]\n"
        ),
    )
    .expect("write manifest");
    std::fs::write(
        crater.join("src/main.rs"),
        "use bbnf_type_kernel_v4::*; fn main() { let mut o=OperationContext::new(100); let mut b=ProblemBuilder::new(); let d=b.primitive(Primitive::I64,&mut o).unwrap(); let _ = solve(d,&mut o); }",
    )
    .expect("write source");
    let output = Command::new("cargo")
        .args(["check", "--quiet"])
        .env("CARGO_TARGET_DIR", crater.join("target"))
        .current_dir(&crater)
        .output()
        .expect("run compile-fail crater");
    assert!(!output.status.success());
    let diagnostics = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostics.contains("expected `ClosedProblem`, found `DraftTypeId`"));
    std::fs::remove_dir_all(crater).expect("remove crater");
}

#[test]
fn recursion_is_alpha_local_for_every_root_and_batch_scheme() {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let first = recursive_option(&mut builder, &mut operations);
    let second = recursive_option(&mut builder, &mut operations);
    let (problem, _) = builder
        .close(
            [
                (ExportKey::new("first"), first),
                (ExportKey::new("second"), second),
            ],
            &mut operations,
        )
        .expect("close recursive roots");
    let solution = solve(problem, &mut operations).expect("solve recursive roots");
    let first_root = solution
        .export_root(&ExportKey::new("first"))
        .expect("first root");
    let second_root = solution
        .export_root(&ExportKey::new("second"))
        .expect("second root");
    assert_eq!(
        solution.root_bytes(first_root),
        solution.root_bytes(second_root)
    );
    let scheme = generalize_roots(
        &solution,
        &[first_root, second_root],
        0,
        BindingForm::SyntacticValue,
        &mut operations,
    )
    .expect("generalize batch");
    assert_eq!(scheme.root_count(), 2);
    let mut target = ProblemBuilder::new();
    let instantiated = instantiate(
        &scheme,
        &mut target,
        &mut RigidContext::new(),
        1,
        Origin::boundary("test.instantiate"),
        &mut operations,
    )
    .expect("instantiate both recursive roots");
    let (problem, _) = target
        .close(
            instantiated
                .roots()
                .iter()
                .enumerate()
                .map(|(index, root)| (ExportKey::new(format!("root-{index}")), *root)),
            &mut operations,
        )
        .expect("close instantiated roots");
    let round_trip = solve(problem, &mut operations).expect("solve instantiated roots");
    let left = round_trip
        .export_root(&ExportKey::new("root-0"))
        .expect("left");
    let right = round_trip
        .export_root(&ExportKey::new("root-1"))
        .expect("right");
    assert_eq!(round_trip.root_bytes(left), round_trip.root_bytes(right));
}

#[test]
fn nested_outer_use_prebuilt_composition_and_vacuity_are_explicit() {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let outer = builder.fresh_rec_var(&mut operations).expect("outer");
    let inner = builder.fresh_rec_var(&mut operations).expect("inner");
    let outer_use = builder.rec_var(outer, &mut operations).expect("outer use");
    let inner_use = builder.rec_var(inner, &mut operations).expect("inner use");
    let pair = builder
        .tuple([outer_use, inner_use], &mut operations)
        .expect("pair");
    let inner_mu = builder.mu(inner, pair, &mut operations).expect("inner mu");
    let outer_mu = builder
        .mu(outer, inner_mu, &mut operations)
        .expect("outer mu");
    let (problem, _) = builder
        .close([(ExportKey::new("nested"), outer_mu)], &mut operations)
        .expect("close nested recursive type");
    solve(problem, &mut operations).expect("solve nested recursive type");

    let mut builder = ProblemBuilder::new();
    let binder = builder.fresh_rec_var(&mut operations).expect("binder");
    let body = builder
        .primitive(Primitive::Unit, &mut operations)
        .expect("body");
    let error = builder
        .mu(binder, body, &mut operations)
        .expect_err("vacuous recursion must reject");
    assert_eq!(error.kind(), &TypeErrorKind::VacuousRecursion);
}

fn recursive_option(
    builder: &mut ProblemBuilder,
    operations: &mut OperationContext,
) -> DraftTypeId {
    let binder = builder.fresh_rec_var(operations).expect("binder");
    let rec = builder.rec_var(binder, operations).expect("rec");
    let body = builder.option(rec, operations).expect("option");
    builder.mu(binder, body, operations).expect("mu")
}
