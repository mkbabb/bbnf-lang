use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::dag::NodeId;
use bbnf_ir::interpreter::{Interpreter, Value, ValueSlice};
use bbnf_ir::passes::{
    AltMode, ComponentReport, ConstraintCounts, DecisionCounts, MaterializationClass,
    OptimizationFailureLocus, OptimizationUnavailable, OptimizationUnavailableReason,
    RecognizerDecision, SolveCompletionStatus,
};
use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, RuleMeta, TypeDesc};

use crate::lower::DirectiveSet;
use crate::pipeline::{
    CompileError, CompileOutput, CompileRequest, PipelineOptions, compile_ast, compile_ast_request,
    compile_grammar, compile_grammar_request,
};
use crate::types::AST;

const DIRECT_GRAMMAR: &str = r#"
branch = ("x" -> 0u8), "z" | ("y" -> 1u8), "z" ;
start = "!", branch ;
"#;

fn assert_typed_arm(ir: &bbnf_ir::GrammarIR, branch: &IrNode, literal: &str, value: i64) {
    let IrNode::Seq(items) = branch else {
        panic!("branch arm must remain a two-item Seq: {branch:#?}");
    };
    let [map, suffix] = items.as_slice() else {
        panic!("branch arm must contain exactly Map then suffix: {items:#?}");
    };
    let IrNode::Map { inner, fn_id } = map else {
        panic!("branch arm first item must be Map: {map:#?}");
    };
    let IrNode::Literal(inner_id) = inner.as_ref() else {
        panic!("Map must wrap its arm literal directly: {inner:#?}");
    };
    assert_eq!(ir.get_string(*inner_id), literal);
    let IrNode::Literal(suffix_id) = suffix else {
        panic!("branch arm suffix must remain a literal: {suffix:#?}");
    };
    assert_eq!(ir.get_string(*suffix_id), "z");
    assert_eq!(
        ir.fns.get(*fn_id as usize),
        Some(&FnDescriptor::Expr {
            expr: MapExpr::IntLit(value),
            return_type: Some(TypeDesc::U8),
        }),
        "FnId {fn_id} must point at the exact typed constant expression",
    );
    assert_eq!(
        ir.node_type(map),
        Some(&TypeDesc::U8),
        "the type projection must attach U8 to the Map node itself",
    );
}

fn run(ir: &bbnf_ir::GrammarIR, input: &str) -> bbnf_ir::interpreter::ParseResult {
    let program = compile_bytecode(ir);
    Interpreter::new(&program, input).run()
}

#[test]
fn canonical_compile_entrypoints_share_the_structured_error_type() {
    let _: fn(&str, &PipelineOptions) -> Result<GrammarIR, CompileError> = compile_grammar;
    let _: fn(&str, &CompileRequest) -> Result<CompileOutput, CompileError> =
        compile_grammar_request;
    let _: for<'a> fn(
        AST<'a>,
        &'a DirectiveSet<'a>,
        &PipelineOptions,
    ) -> Result<GrammarIR, CompileError> = compile_ast;
    let _: for<'a> fn(
        AST<'a>,
        &'a DirectiveSet<'a>,
        &CompileRequest,
    ) -> Result<CompileOutput, CompileError> = compile_ast_request;
}

#[test]
fn structural_alt_width_rejects_the_first_unsupported_count() {
    let branch = || bbnf_ir::AltBranch {
        node: IrNode::Epsilon,
        first_set: None,
    };
    let max = bbnf_ir::AltBranchId::MAX_COUNT;
    let mut branches = Vec::with_capacity(max + 1);
    branches.resize_with(max, branch);
    let at_limit = IrNode::Alt(branches, None);
    crate::pipeline::validate::validate_node_alt_width(&at_limit, "bounded", max)
        .expect("the exact structural branch count must be admitted");

    let IrNode::Alt(mut branches, _) = at_limit else {
        unreachable!()
    };
    branches.push(branch());
    let over_limit = IrNode::Alt(branches, None);
    let error = crate::pipeline::validate::validate_node_alt_width(&over_limit, "bounded", max)
        .expect_err("the first unsupported count must fail during compilation");
    let CompileError::TooManyAlternatives {
        rule,
        count,
        max: reported_max,
    } = error
    else {
        panic!("the exact typed width error must survive")
    };
    assert_eq!(rule, "bounded");
    assert_eq!(count, bbnf_ir::AltBranchId::MAX_COUNT + 1);
    assert_eq!(reported_max, bbnf_ir::AltBranchId::MAX_COUNT);
}

#[test]
fn recover_alt_width_obeys_the_same_exact_boundary_as_rule_bodies() {
    let branch = || bbnf_ir::AltBranch {
        node: IrNode::Epsilon,
        first_set: None,
    };
    let max = bbnf_ir::AltBranchId::MAX_COUNT;
    let mut branches = Vec::with_capacity(max + 1);
    branches.resize_with(max, branch);

    let mut ir = GrammarIR::default();
    ir.strings.push("bounded".into());
    ir.rules.push(IrRule {
        id: 0,
        name: 0,
        body: IrNode::Epsilon,
        meta: RuleMeta::default(),
        source_span: None,
    });
    ir.rules[0].meta.directives.recover = Some(IrNode::Alt(branches, None));
    crate::pipeline::validate::validate_ir_alt_width(&ir)
        .expect("the exact recovery branch count must be admitted");

    let Some(IrNode::Alt(branches, _)) = &mut ir.rules[0].meta.directives.recover else {
        unreachable!()
    };
    branches.push(branch());
    let error = crate::pipeline::validate::validate_ir_alt_width(&ir)
        .expect_err("the first unsupported recovery branch count must fail");
    let CompileError::TooManyAlternatives {
        rule,
        count,
        max: reported_max,
    } = error
    else {
        panic!("the exact typed width error must survive")
    };
    assert_eq!(rule, "bounded");
    assert_eq!(count, max + 1);
    assert_eq!(reported_max, max);
}

#[test]
fn direct_pipeline_preserves_both_typed_arms_and_current_vm_contract() {
    let ir = compile_grammar(DIRECT_GRAMMAR, &PipelineOptions::default())
        .expect("direct grammar must compile without ambient optimizer fallback");
    let branch = ir.find_rule("branch").expect("typed branch must survive");
    let IrNode::Alt(arms, _) = &branch.body else {
        panic!(
            "direct pipeline must retain the branch Alt: {:#?}",
            branch.body
        );
    };
    let [x_arm, y_arm] = arms.as_slice() else {
        panic!("branch must retain exactly two arms: {arms:#?}");
    };
    assert_typed_arm(&ir, &x_arm.node, "x", 0);
    assert_typed_arm(&ir, &y_arm.node, "y", 1);

    let x_fn = match &x_arm.node {
        IrNode::Seq(items) => match &items[0] {
            IrNode::Map { fn_id, .. } => *fn_id,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    let y_fn = match &y_arm.node {
        IrNode::Seq(items) => match &items[0] {
            IrNode::Map { fn_id, .. } => *fn_id,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    assert_ne!(x_fn, y_fn, "the two typed arms must retain distinct FnIds");

    for input in ["!xz", "!yz"] {
        let accepted = run(&ir, input);
        assert!(accepted.success, "{input} must be accepted");
        assert_eq!(accepted.offset, 3);
        assert!(accepted.diagnostics.is_empty());
        assert_eq!(
            accepted.value,
            Some(Value::Tagged {
                tag: ir.get_rule(ir.entry).name,
                span: (0, 3),
                children: ValueSlice::from_vec(vec![
                    Value::Span(0, 1),
                    Value::Tagged {
                        tag: branch.name,
                        span: (1, 3),
                        children: ValueSlice::from_vec(vec![Value::Span(1, 2), Value::Span(2, 3)]),
                    },
                ]),
            }),
        );
    }

    let rejected = run(&ir, "!x");
    assert!(!rejected.success);
    assert_eq!(rejected.offset, 1);
    assert_eq!(
        rejected.value,
        Some(Value::Tagged {
            tag: ir.get_rule(ir.entry).name,
            span: (0, 1),
            children: ValueSlice::from_vec(vec![Value::Span(0, 1)]),
        }),
    );
    assert_eq!(rejected.diagnostics.len(), 1);
    assert_eq!(rejected.diagnostics[0].offset, 2);
    assert_eq!(rejected.diagnostics[0].rule_name.as_deref(), Some("branch"));
    assert!(rejected.diagnostics[0].expected.is_empty());
}

#[test]
fn optimizer_error_preserves_exact_public_variant_and_sentinel_sidecars() {
    let decision_node = NodeId(17);
    let materialization_node = NodeId(23);
    let mut ir = GrammarIR::default();
    ir.recognizer_decisions.insert(
        decision_node,
        RecognizerDecision {
            alt_mode: Some(AltMode::Checkpoint),
            ..RecognizerDecision::default()
        },
    );
    ir.materialization
        .insert(materialization_node, MaterializationClass::TapeSpanOnly);
    let decisions_before = ir.recognizer_decisions.clone();
    let materialization_before = ir.materialization.clone();

    let unavailable = OptimizationUnavailable {
        reason: OptimizationUnavailableReason::BudgetExceeded,
        locus: OptimizationFailureLocus::Search,
        reports: vec![ComponentReport {
            root: 3,
            members: vec![1, 3],
            variables: DecisionCounts::default(),
            sites: DecisionCounts::default(),
            domain_values: DecisionCounts::default(),
            constraints: ConstraintCounts::default(),
            semantic_sites: 0,
            covered_sites: 0,
            status: SolveCompletionStatus::BudgetExceeded,
            nodes_explored: 2,
            node_budget: Some(2),
            cancelled: false,
            budget_exceeded: true,
            objective: None,
            failure: Some(OptimizationFailureLocus::Search),
        }],
    };
    let expected = unavailable.clone();

    let error = super::install_grammar_solution(&mut ir, Err(unavailable))
        .expect_err("optimizer failure must stop both sidecar writes");
    let CompileError::OptimizationUnavailable(actual) = error else {
        panic!("optimizer failure must retain the public structured variant");
    };
    assert_eq!(actual, expected);
    assert_eq!(ir.recognizer_decisions, decisions_before);
    assert_eq!(ir.materialization, materialization_before);
}
