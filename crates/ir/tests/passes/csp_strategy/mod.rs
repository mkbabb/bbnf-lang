use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use csp_solver::constraint::{ImplicationConstraint, LambdaConstraint};
use csp_solver::domain::{CostDomain, Domain};
use csp_solver::ordering::Ordering;
use csp_solver::variable::Variable;
use csp_solver::{CancelToken, Csp, OptimizationMode, Pruning, SolveConfig};

use super::site::{Site, SiteRegistry, decode_complete_solution, decode_exact_factorized_optimum};
use super::solve::{
    GrammarTransaction, commit_component, enter_component, finish_grammar_transaction,
    solve_exact_constrained,
};
use super::*;
use crate::dag::NodeId;
use crate::passes::materialization::classify_materialization;
use crate::{AltBranch, IrRule, RuleMeta};

fn alt(value: AltMode) -> StrategyValue {
    StrategyValue::Alt(value)
}

fn wrap(value: WrapMode) -> StrategyValue {
    StrategyValue::Wrap(value)
}

fn domain(entries: Vec<(StrategyValue, f64)>) -> StrategyDomain {
    StrategyDomain::new(entries)
}

fn phase_ir(bodies: Vec<IrNode>) -> GrammarIR {
    let mut strings = vec!["rule".to_owned()];
    strings.extend((0..bodies.len()).map(|index| format!("value-{index}")));
    let rules = bodies
        .into_iter()
        .enumerate()
        .map(|(index, body)| IrRule {
            id: index as u32,
            name: 0,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        })
        .collect();
    let mut ir = GrammarIR {
        rules,
        strings,
        ..GrammarIR::default()
    };
    crate::dag::ensure_dag(&mut ir);
    classify_materialization(&mut ir);
    ir
}

fn controlled_transaction<'a>(
    ir: &'a GrammarIR,
    token: &CancelToken,
    component_capacity: usize,
) -> GrammarTransaction<'a> {
    GrammarTransaction::new(
        ir,
        &GrammarSolveControl {
            node_budget: Some(1_000_000),
            cancel: Some(token.clone()),
        },
        component_capacity,
    )
    .expect("phase fixture has a complete grammar DAG")
}

fn optimize_implication(
    ordering: Ordering,
    reverse_values: bool,
    constrained: bool,
) -> (Vec<StrategyValue>, f64) {
    let mut x = vec![
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 2.0),
    ];
    let mut y = vec![
        (wrap(WrapMode::Generic), 10.0),
        (wrap(WrapMode::SepBy), 0.0),
    ];
    if reverse_values {
        x.reverse();
        y.reverse();
    }

    let mut csp = Csp::new();
    let x_var = csp.add_variable(domain(x));
    let y_var = csp.add_variable(domain(y));
    if constrained {
        csp.add_constraint(ImplicationConstraint::new(
            x_var,
            y_var,
            alt(AltMode::Checkpoint),
            vec![wrap(WrapMode::Generic)],
        ));
    } else {
        // Keep this on the constrained search path while removing the hard
        // implication whose semantic effect the witness measures.
        csp.add_constraint(LambdaConstraint::new(
            vec![x_var, y_var],
            |_| true,
            "removed-implication-mutant",
        ));
    }
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering,
        optimization_mode: OptimizationMode::MinimizeCost,
        node_budget: None,
        ..SolveConfig::default()
    };
    let solution = solve_exact_constrained(&mut csp, &config).expect("exact optimum");
    let score = solution
        .iter()
        .enumerate()
        .map(|(index, value)| csp.variables[index].domain.cost(value))
        .sum();
    (solution, score)
}

fn exhaustive_implication(constrained: bool) -> (Vec<StrategyValue>, f64) {
    let xs = [
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 2.0),
    ];
    let ys = [
        (wrap(WrapMode::Generic), 10.0),
        (wrap(WrapMode::SepBy), 0.0),
    ];
    let mut best: Option<(Vec<StrategyValue>, f64)> = None;
    for (x, x_cost) in &xs {
        for (y, y_cost) in &ys {
            let implication_holds = *x != alt(AltMode::Checkpoint) || *y == wrap(WrapMode::Generic);
            if constrained && !implication_holds {
                continue;
            }
            let score = *x_cost + *y_cost;
            if match &best {
                Some((_, best_cost)) => score < *best_cost,
                None => true,
            } {
                best = Some((vec![x.clone(), y.clone()], score));
            }
        }
    }
    best.expect("finite exhaustive product has an optimum")
}

#[test]
fn prune_restore_preserves_canonical_cost_and_order() {
    let original = domain(vec![
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 2.0),
        (alt(AltMode::KeyDispatch), 17.0),
    ]);
    let values = original.values();
    let costs: Vec<_> = values
        .iter()
        .map(|value| original.cost(value).to_bits())
        .collect();
    for keep in &values {
        let mut variable = Variable::new(original.clone());
        variable.restrict_to(keep, 7);
        variable.restore(7);

        assert_eq!(variable.domain.values(), values);
        assert_eq!(
            variable
                .domain
                .values()
                .iter()
                .map(|value| variable.domain.cost(value).to_bits())
                .collect::<Vec<_>>(),
            costs,
        );
    }
}

#[test]
fn implication_matches_exhaustive_immutable_optimum_across_orderings() {
    let expected = exhaustive_implication(true);
    for ordering in [Ordering::Chronological, Ordering::FailFirst, Ordering::Mrv] {
        for reverse_values in [false, true] {
            for _ in 0..2 {
                let actual = optimize_implication(ordering, reverse_values, true);
                assert_eq!(actual.0, expected.0);
                assert_eq!(actual.1.to_bits(), expected.1.to_bits());
            }
        }
    }

    let mutant = optimize_implication(Ordering::FailFirst, false, false);
    let unconstrained = exhaustive_implication(false);
    assert_eq!(mutant.0, unconstrained.0);
    assert_eq!(mutant.1.to_bits(), unconstrained.1.to_bits());
    assert_ne!(
        mutant.0, expected.0,
        "discarding the hard constraint must invert"
    );
}

#[test]
fn site_registry_joins_legality_and_rejects_cost_disagreement() {
    let site = Site::Alt(NodeId(7));
    let mut registry = SiteRegistry::default();
    registry
        .register(
            site,
            domain(vec![
                (alt(AltMode::Checkpoint), 1.0),
                (alt(AltMode::ByteDispatch), 2.0),
            ]),
        )
        .unwrap();
    registry
        .register(site, domain(vec![(alt(AltMode::Checkpoint), 1.0)]))
        .unwrap();
    assert_eq!(registry.entries.len(), 1);
    assert_eq!(
        registry.entries[0].1.values(),
        vec![alt(AltMode::Checkpoint)]
    );

    let failure = registry
        .register(site, domain(vec![(alt(AltMode::Checkpoint), 9.0)]))
        .unwrap_err();
    assert_eq!(
        failure.reason,
        OptimizationUnavailableReason::InvalidAssignment
    );
    assert_eq!(
        failure.locus,
        OptimizationFailureLocus::Site {
            node: NodeId(7),
            family: DecisionFamily::Alt,
        },
    );
}

#[test]
fn decoder_rejects_duplicate_semantic_writes() {
    let mut csp = Csp::new();
    let left = csp.add_variable(domain(vec![(alt(AltMode::Checkpoint), 0.0)]));
    let right = csp.add_variable(domain(vec![(alt(AltMode::Checkpoint), 0.0)]));
    let failure = decode_complete_solution(
        &[alt(AltMode::Checkpoint), alt(AltMode::Checkpoint)],
        &csp,
        &[(left, Site::Alt(NodeId(3))), (right, Site::Alt(NodeId(3)))],
    )
    .unwrap_err();
    assert_eq!(failure.covered, 1);
    assert_eq!(
        failure.locus,
        OptimizationFailureLocus::Site {
            node: NodeId(3),
            family: DecisionFamily::Alt,
        },
    );
}

#[test]
fn pre_cancelled_factorized_grammar_rejects_without_output() {
    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Literal(1),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings: vec!["entry".into(), "x".into()],
        ..GrammarIR::default()
    };
    crate::dag::ensure_dag(&mut ir);
    crate::passes::materialization::classify_materialization(&mut ir);

    let token = CancelToken::new();
    token.cancel();
    let control = GrammarSolveControl {
        node_budget: Some(1_000_000),
        cancel: Some(token),
    };
    let error = solve_grammar_components(&ir, &control)
        .expect_err("pre-cancelled factorized grammar must not expose output");
    assert_eq!(error.reason, OptimizationUnavailableReason::Cancelled);
    assert_eq!(error.locus, OptimizationFailureLocus::Search);
    assert_eq!(error.reports.len(), 1);
    let boundary = &error.reports[0];
    assert_eq!(boundary.root, 0);
    assert_eq!(boundary.members, vec![0]);
    assert_eq!(boundary.status, SolveCompletionStatus::Cancelled);
    assert!(boundary.cancelled);
    assert_eq!(boundary.objective, None);
    assert_eq!(boundary.failure, Some(OptimizationFailureLocus::Search));
}

#[test]
fn factorized_acceptance_observes_boundary_cancellation() {
    let ir = phase_ir(vec![IrNode::Literal(1)]);
    let token = CancelToken::new();
    let mut transaction = controlled_transaction(&ir, &token, 1);
    let component = enter_component(&mut transaction, 0, &[0]).expect("factorized local proof");
    assert_eq!(
        component.report.status,
        SolveCompletionStatus::FactorizedExact
    );
    assert!(component.report.objective.is_some());

    token.cancel();
    let error = commit_component(&mut transaction, component)
        .expect_err("cancelled factorized proof must not commit");
    assert_eq!(error.reason, OptimizationUnavailableReason::Cancelled);
    assert_eq!(error.locus, OptimizationFailureLocus::Search);
    assert_eq!(error.reports.len(), 1);
    let report = &error.reports[0];
    assert_eq!(report.root, 0);
    assert_eq!(report.members, vec![0]);
    assert_eq!(report.status, SolveCompletionStatus::Cancelled);
    assert!(report.cancelled);
    assert_eq!(report.objective, None);
    assert_eq!(report.failure, Some(OptimizationFailureLocus::Search));
}

#[test]
fn constrained_acceptance_observes_boundary_cancellation() {
    let ir = phase_ir(vec![IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Regex(1),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            },
        ],
        None,
    )]);
    let token = CancelToken::new();
    let mut transaction = controlled_transaction(&ir, &token, 1);
    let component = enter_component(&mut transaction, 0, &[0]).expect("constrained local proof");
    assert_eq!(component.report.status, SolveCompletionStatus::Optimal);
    assert!(component.report.constraints.token_dispatch > 0);
    assert!(component.report.objective.is_some());

    token.cancel();
    let error = commit_component(&mut transaction, component)
        .expect_err("cancelled constrained proof must not commit");
    assert_eq!(error.reason, OptimizationUnavailableReason::Cancelled);
    assert_eq!(error.reports.len(), 1);
    let report = &error.reports[0];
    assert_eq!(report.status, SolveCompletionStatus::Cancelled);
    assert!(report.cancelled);
    assert_eq!(report.objective, None);
    assert_eq!(report.failure, Some(OptimizationFailureLocus::Search));
}

#[test]
fn completed_component_then_cancel_reports_completed_plus_boundary() {
    let ir = phase_ir(vec![IrNode::Literal(1), IrNode::Literal(2)]);
    let token = CancelToken::new();
    let mut transaction = controlled_transaction(&ir, &token, 2);
    let first = enter_component(&mut transaction, 0, &[0]).expect("first local proof");
    commit_component(&mut transaction, first).expect("first proof commits internally");

    token.cancel();
    let error = match enter_component(&mut transaction, 1, &[1]) {
        Ok(_) => panic!("next component boundary must observe cancellation"),
        Err(error) => error,
    };
    assert_eq!(error.reason, OptimizationUnavailableReason::Cancelled);
    assert_eq!(error.reports.len(), 2);
    assert_eq!(error.reports[0].root, 0);
    assert_eq!(error.reports[0].members, vec![0]);
    assert_eq!(
        error.reports[0].status,
        SolveCompletionStatus::FactorizedExact,
    );
    assert!(error.reports[0].objective.is_some());
    assert_eq!(error.reports[1].root, 1);
    assert_eq!(error.reports[1].members, vec![1]);
    assert_eq!(error.reports[1].status, SolveCompletionStatus::Cancelled);
    assert_eq!(error.reports[1].objective, None);
    assert_eq!(
        error.reports[1].failure,
        Some(OptimizationFailureLocus::Search),
    );
}

#[test]
fn final_return_observes_boundary_cancellation() {
    let ir = phase_ir(vec![IrNode::Literal(1)]);
    let token = CancelToken::new();
    let mut transaction = controlled_transaction(&ir, &token, 1);
    let component = enter_component(&mut transaction, 0, &[0]).expect("local proof");
    commit_component(&mut transaction, component).expect("internal commit");

    token.cancel();
    let error = finish_grammar_transaction(transaction)
        .expect_err("final cancellation must withhold the grammar output");
    assert_eq!(error.reason, OptimizationUnavailableReason::Cancelled);
    assert_eq!(error.reports.len(), 1);
    let report = &error.reports[0];
    assert_eq!(report.status, SolveCompletionStatus::Cancelled);
    assert!(report.cancelled);
    assert_eq!(report.objective, None);
    assert_eq!(report.failure, Some(OptimizationFailureLocus::Search));
}

#[test]
fn cancellation_before_search_discards_every_solution() {
    let token = CancelToken::new();
    token.cancel();
    let mut csp = Csp::new();
    let left = csp.add_variable(domain(vec![
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 1.0),
    ]));
    let right = csp.add_variable(domain(vec![
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 1.0),
    ]));
    csp.add_not_equal(left, right);
    let failure = solve_exact_constrained(
        &mut csp,
        &SolveConfig {
            pruning: Pruning::None,
            optimization_mode: OptimizationMode::MinimizeCost,
            cancel: Some(token),
            ..SolveConfig::default()
        },
    )
    .unwrap_err();
    assert_eq!(failure.reason, OptimizationUnavailableReason::Cancelled);
    assert!(csp.stats().cancelled);
}

#[test]
fn cancellation_on_final_singleton_leaf_discards_incumbent() {
    let token = CancelToken::new();
    let mut csp = Csp::new();
    let only = csp.add_variable(domain(vec![(alt(AltMode::Checkpoint), 0.0)]));
    let hook_token = token.clone();
    csp.add_constraint(LambdaConstraint::new(
        vec![only],
        move |assignment| {
            if assignment[only as usize].is_some() {
                hook_token.cancel();
            }
            true
        },
        "cancel-on-final-singleton-leaf",
    ));

    let result = solve_exact_constrained(
        &mut csp,
        &SolveConfig {
            pruning: Pruning::None,
            optimization_mode: OptimizationMode::MinimizeCost,
            node_budget: None,
            cancel: Some(token.clone()),
            ..SolveConfig::default()
        },
    );
    assert!(token.is_cancelled());
    let failure = result.expect_err("cancelled final-leaf incumbent must be discarded");
    assert_eq!(failure.reason, OptimizationUnavailableReason::Cancelled);
}

#[test]
fn cancellation_after_first_incumbent_discards_best_so_far() {
    let token = CancelToken::new();
    let checks = Arc::new(AtomicUsize::new(0));
    let mut csp = Csp::new();
    let left = csp.add_variable(domain(vec![
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 1.0),
    ]));
    let right = csp.add_variable(domain(vec![
        (wrap(WrapMode::Generic), 0.0),
        (wrap(WrapMode::SepBy), 1.0),
    ]));
    let hook_token = token.clone();
    let hook_checks = Arc::clone(&checks);
    csp.add_constraint(LambdaConstraint::new(
        vec![left, right],
        move |assignment| {
            if assignment[left as usize].is_some()
                && assignment[right as usize].is_some()
                && hook_checks.fetch_add(1, AtomicOrdering::SeqCst) == 0
            {
                hook_token.cancel();
            }
            true
        },
        "cancel-after-first-complete-assignment",
    ));
    let failure = solve_exact_constrained(
        &mut csp,
        &SolveConfig {
            pruning: Pruning::None,
            ordering: Ordering::Chronological,
            optimization_mode: OptimizationMode::MinimizeCost,
            node_budget: None,
            cancel: Some(token),
            ..SolveConfig::default()
        },
    )
    .unwrap_err();
    assert_eq!(failure.reason, OptimizationUnavailableReason::Cancelled);
    assert!(checks.load(AtomicOrdering::SeqCst) >= 1);
    assert!(csp.stats().cancelled);
}

#[test]
fn budget_after_an_incumbent_discards_best_so_far() {
    let values = vec![
        (alt(AltMode::Checkpoint), 0.0),
        (alt(AltMode::ByteDispatch), 1.0),
    ];
    let mut budgeted = Csp::new();
    let left = budgeted.add_variable(domain(values.clone()));
    let right = budgeted.add_variable(domain(values));
    let complete_assignments = Arc::new(AtomicUsize::new(0));
    let complete_assignments_hook = Arc::clone(&complete_assignments);
    budgeted.add_constraint(LambdaConstraint::new(
        vec![left, right],
        move |assignment| {
            if assignment[left as usize].is_some() && assignment[right as usize].is_some() {
                complete_assignments_hook.fetch_add(1, AtomicOrdering::SeqCst);
            }
            true
        },
        "observe-budget-incumbent",
    ));
    let budget = solve_exact_constrained(
        &mut budgeted,
        &SolveConfig {
            node_budget: Some(2),
            pruning: Pruning::None,
            optimization_mode: OptimizationMode::MinimizeCost,
            ..SolveConfig::default()
        },
    )
    .unwrap_err();
    assert_eq!(budget.reason, OptimizationUnavailableReason::BudgetExceeded);
    assert!(complete_assignments.load(AtomicOrdering::SeqCst) >= 1);
    assert!(budgeted.stats().budget_exceeded);
}

#[test]
fn unsatisfiable_problem_has_no_per_variable_fallback() {
    let mut unsat = Csp::new();
    let only = alt(AltMode::Checkpoint);
    let left = unsat.add_variable(domain(vec![(only.clone(), 0.0)]));
    let right = unsat.add_variable(domain(vec![(only, 0.0)]));
    unsat.add_not_equal(left, right);
    let unsat = solve_exact_constrained(&mut unsat, &SolveConfig::default()).unwrap_err();
    assert_eq!(unsat.reason, OptimizationUnavailableReason::Unsatisfiable);
}

#[test]
fn decoder_rejects_incomplete_and_oversized_assignments() {
    let mut csp = Csp::new();
    let var = csp.add_variable(domain(vec![(alt(AltMode::Checkpoint), 0.0)]));
    let sites = [(var, Site::Alt(NodeId(0)))];
    let incomplete = decode_complete_solution(&[], &csp, &sites);
    let oversized = decode_complete_solution(
        &[alt(AltMode::Checkpoint), alt(AltMode::Checkpoint)],
        &csp,
        &sites,
    );
    let loci = (
        incomplete.err().map(|failure| failure.locus),
        oversized.err().map(|failure| failure.locus),
    );

    assert_eq!(
        loci,
        (
            Some(OptimizationFailureLocus::Search),
            Some(OptimizationFailureLocus::Search),
        ),
    );
}

#[test]
fn decoder_rejects_foreign_var_and_missing_value() {
    let mut csp = Csp::new();
    let var = csp.add_variable(domain(vec![(alt(AltMode::Checkpoint), 0.0)]));
    let foreign = decode_complete_solution(
        &[alt(AltMode::Checkpoint)],
        &csp,
        &[(var + 1, Site::Alt(NodeId(0)))],
    );
    let missing = decode_complete_solution(&[alt(AltMode::Checkpoint)], &csp, &[]);
    let loci = (
        foreign.err().map(|failure| failure.locus),
        missing.err().map(|failure| failure.locus),
    );

    assert_eq!(
        loci,
        (
            Some(OptimizationFailureLocus::Variable(var + 1)),
            Some(OptimizationFailureLocus::Search),
        ),
    );
}

#[test]
fn decoder_rejects_a_value_from_the_wrong_decision_family() {
    let mut csp = Csp::new();
    let var = csp.add_variable(domain(vec![(alt(AltMode::Checkpoint), 0.0)]));
    let failure = decode_complete_solution(
        &[wrap(WrapMode::Generic)],
        &csp,
        &[(var, Site::Alt(NodeId(0)))],
    )
    .unwrap_err();
    assert_eq!(failure.locus, OptimizationFailureLocus::Variable(var));
}

#[test]
fn factorized_decoder_rejects_non_finite_and_overflowing_objectives() {
    let mut non_finite = Csp::new();
    let var = non_finite.add_variable(domain(vec![(alt(AltMode::Checkpoint), f64::NAN)]));
    let non_finite_failure =
        decode_exact_factorized_optimum(&non_finite, &[(var, Site::Alt(NodeId(0)))]).unwrap_err();

    let mut overflowing = Csp::new();
    let left = overflowing.add_variable(domain(vec![(alt(AltMode::Checkpoint), f64::MAX)]));
    let right = overflowing.add_variable(domain(vec![(wrap(WrapMode::Generic), f64::MAX)]));
    let overflow_failure = decode_exact_factorized_optimum(
        &overflowing,
        &[(left, Site::Alt(NodeId(0))), (right, Site::Wrap(NodeId(1)))],
    )
    .unwrap_err();

    assert_eq!(
        non_finite_failure.locus,
        OptimizationFailureLocus::Variable(var),
    );
    assert_eq!(
        overflow_failure.locus,
        OptimizationFailureLocus::Variable(right),
    );
}

#[test]
fn factorized_ties_keep_canonical_order_and_report_exact_objective() {
    let mut csp = Csp::new();
    let var = csp.add_variable(domain(vec![
        (alt(AltMode::ByteDispatch), 1.0),
        (alt(AltMode::Checkpoint), 1.0),
    ]));
    let decoded = decode_exact_factorized_optimum(&csp, &[(var, Site::Alt(NodeId(0)))])
        .expect("finite tied domain has an exact optimum");
    assert_eq!(
        decoded.assignments.decisions[&NodeId(0)].alt_mode,
        Some(AltMode::ByteDispatch),
    );
    assert_eq!(decoded.objective.to_bits(), 1.0f64.to_bits());
    assert_eq!(decoded.covered, 1);
}
