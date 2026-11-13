//! The stage verifier's mutants (sk-v27 LW3.COMP-6). Each test compiles one
//! lawful fixture, breaks one law in one stage carrier, and requires the
//! verifier to report exactly that stage and law. The fixture holds every
//! node kind, two Pratt tables, records with fields and discards, a transform,
//! trivia and a recursive component.

use super::super::{CompileOptions, GrammarInstanceInput, StageResult, compile_instance};
use super::{Graph, Law, Op, Spine, Stage, TypeRow};
use crate::compiler::EventKind;

const SOURCE: &[u8] = br#"
document = expression ?w, tail*, product_expression;
tail = %{ ~",", item: entry };
entry = keep_right | keep_left | difference_live | tagged | list;
left = %{ value: "l" };
kept = %{ value: "k" };
right = %{ value: "r" };
keep_right = left >> kept;
keep_left = kept << right;
difference_live = kept - right;
tagged = %{ ~"<", tag: @raw(/[A-Z]+/), ~">" };
list = %{ ~"[", children: entry*, ~"]" };
expression = atom;
atom = number | group;
group = "(" ?w, expression, ")" ?w;
number = /(?:0|[1-9][0-9]*)/ ?w;
prefix_minus = "-" ?w;
postfix_bang = "!" ?w;
sum = "+" ?w;
product_expression = product_atom;
product_atom = /[a-z]+/;
product = "*";
optional = ("x", "z")?;
many = "y"+;
empty = epsilon;
"#;

const MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "compiler-verify-mutants.bbnf",
  "entry": "document",
  "trivia_regex": "[ \\t]*",
  "pratt_tables": [
    {
      "site_rule": "expression",
      "atom_rule": "atom",
      "operators": [
        { "matcher_rule": "prefix_minus", "fixity": "prefix", "left_binding_power": null, "right_binding_power": 70 },
        { "matcher_rule": "postfix_bang", "fixity": "postfix", "left_binding_power": 90, "right_binding_power": null },
        { "matcher_rule": "sum", "fixity": "infix", "left_binding_power": 40, "right_binding_power": 41 }
      ]
    },
    {
      "site_rule": "product_expression",
      "atom_rule": "product_atom",
      "operators": [
        { "matcher_rule": "product", "fixity": "infix", "left_binding_power": 60, "right_binding_power": 61 }
      ]
    }
  ]
}"#;

fn read(source: &[u8]) -> Spine {
    let compiled = match compile_instance(
        GrammarInstanceInput::new(MANIFEST, source),
        CompileOptions::default(),
    ) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => panic!("the verifier fixture compiles: {diagnostics:?}"),
    };
    Spine::read(
        compiled.program(),
        compiled.dependency(),
        compiled.types(),
        compiled.facts(),
        compiled.lowering(),
        compiled.events(),
    )
}

fn fixture() -> Spine {
    read(SOURCE)
}

/// The first node whose op satisfies `pick`.
fn node(graph: &Graph, pick: impl Fn(&Op) -> bool) -> usize {
    graph
        .nodes
        .iter()
        .position(|(_, op)| pick(op))
        .expect("the fixture holds the node kind")
}

/// The first node that satisfies `pick` and is no rule's root.
fn inner_node(graph: &Graph, pick: impl Fn(&Op) -> bool) -> usize {
    graph
        .nodes
        .iter()
        .enumerate()
        .position(|(index, (_, op))| pick(op) && graph.rules.iter().all(|rule| rule.root != index))
        .expect("the fixture holds the inner node kind")
}

fn rule(graph: &Graph, name: &str) -> usize {
    graph
        .rules
        .iter()
        .position(|rule| rule.name == name)
        .expect("the fixture defines the rule")
}

fn site(spine: &Spine, kind: EventKind) -> usize {
    spine
        .events
        .sites
        .iter()
        .position(|site| site.kind == kind)
        .expect("the fixture's event plan holds the site kind")
}

/// Re-derive the component index and a dependency-first order (where one
/// exists) after a mutant edits the components.
fn rebuild_components(spine: &mut Spine, mut components: Vec<Vec<usize>>) {
    let read = &mut spine.dependency;
    components.sort();
    read.component_by_rule = vec![0; read.edges.len()];
    for (index, members) in components.iter().enumerate() {
        for &rule in members {
            read.component_by_rule[rule] = index;
        }
    }
    let mut order = Vec::new();
    let mut placed = vec![false; components.len()];
    while order.len() < components.len() {
        let ready = (0..components.len()).find(|&component| {
            !placed[component]
                && components[component].iter().all(|&rule| {
                    read.edges[rule].iter().all(|&target| {
                        let dependency = read.component_by_rule[target];
                        dependency == component || placed[dependency]
                    })
                })
        });
        // A cycle across components has no dependency-first order; place the
        // lowest unplaced component so the mutant reaches the order law.
        let next = ready.unwrap_or_else(|| (0..components.len()).find(|&c| !placed[c]).unwrap_or(0));
        placed[next] = true;
        order.push(next);
    }
    read.order = order;
    read.components = components;
}

#[test]
fn the_fixture_is_lawful() {
    assert_eq!(fixture().check(), Ok(()));
}

#[test]
fn the_fixture_reaches_every_kind() {
    let spine = fixture();
    let graph = &spine.program.graph;
    let kinds = [
        |op: &Op| matches!(op, Op::Epsilon),
        |op: &Op| matches!(op, Op::Literal(_)),
        |op: &Op| matches!(op, Op::Regex(_)),
        |op: &Op| matches!(op, Op::RuleRef(_)),
        |op: &Op| matches!(op, Op::Sequence(_)),
        |op: &Op| matches!(op, Op::Choice(_)),
        |op: &Op| matches!(op, Op::Repeat { .. }),
        |op: &Op| matches!(op, Op::Transform { .. }),
        |op: &Op| matches!(op, Op::Projection { .. }),
        |op: &Op| matches!(op, Op::Difference { .. }),
        |op: &Op| matches!(op, Op::Trim { .. }),
        |op: &Op| matches!(op, Op::Pratt { .. }),
        |op: &Op| matches!(op, Op::Record { .. }),
    ];
    for kind in kinds {
        node(graph, kind);
    }
    assert_eq!(graph.tables.len(), 2);
    assert!(spine.dependency.components.iter().any(|members| members.len() > 1));
}

macro_rules! mutant {
    ($name:ident, $stage:ident, $law:ident, |$spine:ident| $body:block) => {
        #[test]
        fn $name() {
            let mut $spine = fixture();
            $body
            assert_eq!(
                $spine.check().map_err(|violation| (violation.stage, violation.law)),
                Err((Stage::$stage, Law::$law)),
            );
        }
    };
}

// Program.

mutant!(program_rule_reference_dangles, Program, RuleInRange, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::RuleRef(_)));
    graph.nodes[at].1 = Op::RuleRef(graph.rules.len());
});

mutant!(program_node_identity_swapped, Program, RowIdentity, |spine| {
    spine.program.graph.nodes.swap(0, 1);
});

mutant!(program_sequence_child_dangles, Program, NodeInRange, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Sequence(_)));
    let dangling = graph.nodes.len();
    if let Op::Sequence(children) = &mut graph.nodes[at].1 {
        children[0] = dangling;
    }
});

mutant!(program_rule_root_dangles, Program, NodeInRange, |spine| {
    let graph = &mut spine.program.graph;
    graph.rules[0].root = graph.nodes.len();
});

mutant!(program_entry_dangles, Program, RuleInRange, |spine| {
    let graph = &mut spine.program.graph;
    graph.entry = graph.rules.len();
});

mutant!(program_field_owner_dangles, Program, RuleInRange, |spine| {
    let graph = &mut spine.program.graph;
    graph.fields[0].owner = graph.rules.len();
});

mutant!(program_record_slot_field_dangles, Program, FieldInRange, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Record { slots, .. } if slots.iter().any(|slot| slot.field.is_some())));
    let dangling = graph.fields.len();
    if let Op::Record { slots, .. } = &mut graph.nodes[at].1 {
        let slot = slots.iter_mut().find(|slot| slot.field.is_some()).expect("a field slot");
        slot.field = Some(dangling);
    }
});

mutant!(program_record_slot_names_another_field, Program, RecordSlotIsItsField, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Record { slots, .. } if slots.iter().any(|slot| slot.field.is_some())));
    let Op::Record { owner, .. } = graph.nodes[at].1 else { unreachable!() };
    let foreign = graph.fields.iter().position(|field| field.owner != owner).expect("a field of another record");
    if let Op::Record { slots, .. } = &mut graph.nodes[at].1 {
        let slot = slots.iter_mut().find(|slot| slot.field.is_some()).expect("a field slot");
        slot.field = Some(foreign);
    }
});

mutant!(program_pratt_table_dangles, Program, PrattInRange, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Pratt { .. }));
    let dangling = graph.tables.len();
    if let Op::Pratt { table, .. } = &mut graph.nodes[at].1 {
        *table = dangling;
    }
});

mutant!(program_operator_claims_another_table, Program, PrattOperatorInItsTable, |spine| {
    spine.program.graph.operators[0].table = 1;
});

mutant!(program_prefix_operator_binds_left, Program, PrattFixityShape, |spine| {
    let graph = &mut spine.program.graph;
    let prefix = graph
        .operators
        .iter()
        .position(|operator| operator.left.is_none())
        .expect("a prefix operator");
    graph.operators[prefix].left = Some(1);
});

mutant!(program_rule_bodies_share_a_node, Program, RuleBodiesAreTrees, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Sequence(children) if children.len() > 1));
    if let Op::Sequence(children) = &mut graph.nodes[at].1 {
        children[1] = children[0];
    }
});

mutant!(program_rule_body_has_a_cycle, Program, RuleBodiesAreTrees, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Repeat { .. }));
    if let Op::Repeat { child, .. } = &mut graph.nodes[at].1 {
        *child = at;
    }
});

mutant!(program_node_is_orphaned, Program, RuleBodiesAreTrees, |spine| {
    let graph = &mut spine.program.graph;
    let orphan = graph.nodes.len();
    graph.nodes.push((orphan, Op::Epsilon));
    spine.program.owners.push(0);
});

mutant!(program_node_owner_misnamed, Program, NodeOwners, |spine| {
    let rules = spine.program.graph.rules.len();
    let owner = &mut spine.program.owners[0];
    *owner = (*owner + 1) % rules;
});

mutant!(program_rule_name_repeats, Program, RuleNamesUnique, |spine| {
    let graph = &mut spine.program.graph;
    graph.rules[1].name = graph.rules[0].name.clone();
});

mutant!(program_choice_is_empty, Program, AggregateHasChildren, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Choice(_)));
    graph.nodes[at].1 = Op::Choice(Vec::new());
});

mutant!(program_repeat_bounds_inverted, Program, RepeatBounds, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Repeat { .. }));
    if let Op::Repeat { minimum, maximum, .. } = &mut graph.nodes[at].1 {
        *minimum = 3;
        *maximum = Some(1);
    }
});

mutant!(program_transform_ordinal_skips, Program, TransformOrdinals, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Transform { .. }));
    if let Op::Transform { transform, .. } = &mut graph.nodes[at].1 {
        *transform += 1;
    }
});

mutant!(program_record_moves_to_another_rule, Program, RecordBelongsToItsRule, |spine| {
    let graph = &mut spine.program.graph;
    let at = node(graph, |op| matches!(op, Op::Record { .. }));
    let Op::Record { owner, .. } = graph.nodes[at].1 else { unreachable!() };
    let other = (owner + 1) % graph.rules.len();
    if let Op::Record { owner: record, .. } = &mut graph.nodes[at].1 {
        *record = other;
    }
    for field in &mut graph.fields {
        if field.owner == owner {
            field.owner = other;
        }
    }
});

// DependencySolution.

mutant!(dependency_edge_dropped, Dependency, EdgesAreReferences, |spine| {
    let edges = &mut spine.dependency.edges;
    let rule = edges.iter().position(|row| !row.is_empty()).expect("a rule with a dependency");
    edges[rule].pop();
});

mutant!(dependency_edges_unsorted, Dependency, EdgesSortedUnique, |spine| {
    let edges = &mut spine.dependency.edges;
    let rule = edges.iter().position(|row| row.len() > 1).expect("a rule with two dependencies");
    edges[rule].reverse();
});

mutant!(dependency_edge_row_missing, Dependency, EdgeRowPerRule, |spine| {
    spine.dependency.edges.pop();
});

mutant!(dependency_rule_in_two_components, Dependency, ComponentsPartitionRules, |spine| {
    let components = &mut spine.dependency.components;
    let rule = components[0][0];
    let other = components.iter().position(|members| !members.contains(&rule)).expect("another component");
    components[other].push(rule);
    components[other].sort_unstable();
});

mutant!(dependency_component_index_wrong, Dependency, ComponentIndex, |spine| {
    let read = &mut spine.dependency;
    read.component_by_rule[0] = (read.component_by_rule[0] + 1) % read.components.len();
});

mutant!(dependency_unrelated_rules_merged, Dependency, ComponentsStronglyConnected, |spine| {
    let graph = &spine.program.graph;
    let (optional, many) = (rule(graph, "optional"), rule(graph, "many"));
    let mut components = spine
        .dependency
        .components
        .iter()
        .filter(|members| !members.contains(&optional) && !members.contains(&many))
        .cloned()
        .collect::<Vec<_>>();
    components.push(vec![optional.min(many), optional.max(many)]);
    rebuild_components(&mut spine, components);
});

mutant!(dependency_recursive_component_split, Dependency, DependencyFirstOrder, |spine| {
    let mut components = Vec::new();
    for members in &spine.dependency.components {
        components.extend(members.iter().map(|&rule| vec![rule]));
    }
    rebuild_components(&mut spine, components);
});

mutant!(dependency_order_reversed, Dependency, DependencyFirstOrder, |spine| {
    spine.dependency.order.reverse();
});

mutant!(dependency_order_drops_a_component, Dependency, DependencyFirstOrder, |spine| {
    spine.dependency.order.pop();
});

mutant!(dependency_from_another_revision, Dependency, OneRevision, |spine| {
    let mut other = SOURCE.to_vec();
    other.push(b'\n');
    spine.dependency.revision = read(&other).program.graph.revision;
});

// TypeSolution.

mutant!(types_node_type_dangles, Types, TypeInRange, |spine| {
    let types = &mut spine.types;
    types.nodes[0].1 = types.arena.len();
});

mutant!(types_named_type_is_another_binder, Types, NamedBinder, |spine| {
    let types = &mut spine.types;
    types.rules[0].named = types.rules[1].named;
});

mutant!(types_root_conclusion_wrong, Types, RootConclusion, |spine| {
    let types = &mut spine.types;
    assert_ne!(types.rules[0].root, types.rules[0].named);
    types.rules[0].root = types.rules[0].named;
});

mutant!(types_arena_child_dangles, Types, TypeInRange, |spine| {
    let arena = &mut spine.types.arena;
    let dangling = arena.len();
    let at = arena
        .iter()
        .position(|row| matches!(row, TypeRow::Option(_) | TypeRow::List(_)))
        .expect("an option or list type");
    arena[at] = TypeRow::List(dangling);
});

mutant!(types_arena_repeats_a_witness, Types, ArenaCanonical, |spine| {
    let arena = &mut spine.types.arena;
    arena.push(arena[0].clone());
});

mutant!(types_derivation_row_missing, Types, RowPerProgramRow, |spine| {
    spine.types.nodes.pop();
});

mutant!(types_named_rule_dangles, Types, RuleInRange, |spine| {
    let rules = spine.program.graph.rules.len();
    let arena = &mut spine.types.arena;
    let at = arena.iter().position(|row| matches!(row, TypeRow::Named(_))).expect("a named type");
    arena[at] = TypeRow::Named(rules);
});

mutant!(types_record_holds_a_foreign_field, Types, FieldInRange, |spine| {
    let fields = spine.program.graph.fields.clone();
    let arena = &mut spine.types.arena;
    let at = arena
        .iter()
        .position(|row| matches!(row, TypeRow::Record { fields, .. } if !fields.is_empty()))
        .expect("a record type with a field");
    if let TypeRow::Record { owner, fields: slots } = &mut arena[at] {
        let foreign = fields.iter().position(|field| field.owner != *owner).expect("a foreign field");
        slots[0].0 = foreign;
    }
});

// AnalysisFacts.

mutant!(facts_nullable_row_consumes, Analysis, NullableHasZeroMinimum, |spine| {
    let facts = &mut spine.facts;
    let at = facts
        .nodes
        .iter()
        .position(|fact| !fact.nullable && fact.minimum.is_some_and(|minimum| minimum > 0))
        .expect("a consuming node");
    facts.nodes[at].nullable = true;
});

mutant!(facts_rule_fact_is_not_its_root, Analysis, RuleFactIsRootFact, |spine| {
    let at = rule(&spine.program.graph, "number");
    let fact = &mut spine.facts.rules[at];
    fact.minimum = fact.minimum.map(|minimum| minimum + 1);
});

mutant!(facts_literal_minimum_wrong, Analysis, FactTransfer, |spine| {
    let at = inner_node(&spine.program.graph, |op| matches!(op, Op::Literal(_)));
    let fact = &mut spine.facts.nodes[at];
    fact.minimum = fact.minimum.map(|minimum| minimum + 1);
});

mutant!(facts_epsilon_not_nullable, Analysis, FactTransfer, |spine| {
    let graph = &spine.program.graph;
    let empty = rule(graph, "empty");
    let at = graph.rules[empty].root;
    spine.facts.nodes[at].nullable = false;
    spine.facts.rules[empty].nullable = false;
});

mutant!(facts_sequence_nullable_wrong, Analysis, FactTransfer, |spine| {
    let at = inner_node(&spine.program.graph, |op| matches!(op, Op::Sequence(_)));
    let fact = &mut spine.facts.nodes[at];
    assert!(!fact.nullable);
    fact.nullable = true;
    fact.minimum = Some(0);
});

mutant!(facts_row_identity_swapped, Analysis, RowIdentity, |spine| {
    spine.facts.nodes.swap(0, 1);
});

// LoweringPlan.

mutant!(plan_rule_reference_dangles, Lowering, RuleInRange, |spine| {
    let graph = &mut spine.plan.graph;
    let at = node(graph, |op| matches!(op, Op::RuleRef(_)));
    graph.nodes[at].1 = Op::RuleRef(graph.rules.len());
});

mutant!(plan_regex_table_dangles, Lowering, RegexInRange, |spine| {
    let dangling = spine.plan.regexes.len();
    let graph = &mut spine.plan.graph;
    let at = node(graph, |op| matches!(op, Op::Regex(_)));
    graph.nodes[at].1 = Op::Regex(Some(dangling));
});

mutant!(plan_regex_transition_dangles, Lowering, RegexTableWellFormed, |spine| {
    let regex = &mut spine.plan.regexes[0];
    regex.transitions[0] = regex.state_count;
});

mutant!(plan_trim_without_trivia, Lowering, TrimNeedsTrivia, |spine| {
    spine.plan.trivia = None;
});

mutant!(plan_renames_a_rule, Lowering, PlanKeepsRules, |spine| {
    spine.plan.graph.rules[0].name.push_str("_renamed");
});

mutant!(plan_node_type_dangles, Lowering, TypeInRange, |spine| {
    let plan = &mut spine.plan;
    plan.node_types[0] = plan.arena.len();
});

mutant!(plan_named_type_is_another_binder, Lowering, NamedBinder, |spine| {
    let plan = &mut spine.plan;
    plan.rule_types[0] = plan.rule_types[1];
});

mutant!(plan_rule_body_has_a_cycle, Lowering, RuleBodiesAreTrees, |spine| {
    let graph = &mut spine.plan.graph;
    let at = node(graph, |op| matches!(op, Op::Sequence(_)));
    if let Op::Sequence(children) = &mut graph.nodes[at].1 {
        children[0] = at;
    }
});

// ScalarEventPlan.

mutant!(events_schema_drifts, Events, EventSchema, |spine| {
    spine.events.schema.push_str(".old");
});

mutant!(events_literal_site_names_a_choice, Events, SiteShape, |spine| {
    let choice = node(&spine.plan.graph, |op| matches!(op, Op::Choice(_)));
    let at = site(&spine, EventKind::Literal);
    spine.events.sites[at].node = Some(choice);
});

mutant!(events_site_node_dangles, Events, SiteShape, |spine| {
    let dangling = spine.plan.graph.nodes.len();
    let at = site(&spine, EventKind::Regex);
    spine.events.sites[at].node = Some(dangling);
});

mutant!(events_field_site_names_another_rule, Events, FieldSiteIsItsField, |spine| {
    let rules = spine.plan.graph.rules.len();
    let at = site(&spine, EventKind::EnterField);
    let rule = spine.events.sites[at].rule.as_mut().expect("a field site names its rule");
    *rule = (*rule + 1) % rules;
});

mutant!(events_pratt_reduce_names_a_foreign_operator, Events, PrattReduceIsItsTable, |spine| {
    let at = site(&spine, EventKind::PrattReduce);
    let Some(Op::Pratt { table, .. }) = spine.events.sites[at].node.map(|node| spine.plan.graph.nodes[node].1.clone()) else {
        unreachable!("a Pratt reduce site names a Pratt node")
    };
    let table = spine.plan.graph.tables[table];
    let foreign = (0..spine.plan.graph.operators.len())
        .find(|operator| !(table.start..table.start + table.len).contains(operator))
        .expect("an operator of the other table");
    spine.events.sites[at].operator = Some(foreign);
});

mutant!(events_rule_entered_twice, Events, RuleEnteredAndLeftOnce, |spine| {
    let sites = &mut spine.events.sites;
    let enter = sites[site_index(sites, EventKind::EnterRule)];
    sites.insert(0, enter);
});

mutant!(events_input_ends_before_the_last_site, Events, InputEndsOnceLast, |spine| {
    let sites = &mut spine.events.sites;
    let last = sites.len() - 1;
    sites.swap(last - 1, last);
});

mutant!(events_transform_site_names_another_node, Events, TransformSites, |spine| {
    let transform = &mut spine.events.transforms[0];
    transform.1 = (transform.1 + 1) % spine.plan.graph.nodes.len();
});

mutant!(events_from_another_revision, Events, OneRevision, |spine| {
    let mut other = SOURCE.to_vec();
    other.push(b'\n');
    spine.events.revision = read(&other).program.graph.revision;
});

fn site_index(sites: &[super::SiteRow], kind: EventKind) -> usize {
    sites.iter().position(|site| site.kind == kind).expect("the site kind")
}
