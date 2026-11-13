//! The laws of the stage verifier, one function per stage carrier.

use std::collections::BTreeSet;

use super::super::event::{EVENT_SCHEMA, EventKind};
use super::super::pratt::PrattFixity;
use super::super::regex_table::DEAD_STATE;
use super::{FactRow, Graph, Law, Op, RegexRow, Spine, Stage, TypeRow, Violation};

fn ensure(holds: bool, stage: Stage, law: Law, row: usize) -> Result<(), Violation> {
    if holds {
        Ok(())
    } else {
        Err(Violation { stage, law, row })
    }
}

impl Spine {
    /// Program: canonical rows, references in range, rule bodies are trees,
    /// and `node_owners` names each node's rule.
    pub(super) fn check_program(&self) -> Result<(), Violation> {
        let read = &self.program;
        let owners = check_graph(&read.graph, Stage::Program, None)?;
        ensure(
            read.owners.len() == owners.len(),
            Stage::Program,
            Law::NodeOwners,
            read.owners.len(),
        )?;
        for (node, (owner, tree_owner)) in read.owners.iter().zip(&owners).enumerate() {
            ensure(owner == tree_owner, Stage::Program, Law::NodeOwners, node)?;
        }
        Ok(())
    }

    /// DependencySolution: the graph's edges are exactly each rule body's
    /// references, the components are its strongly connected components, and
    /// the order puts every component after the components it depends on.
    pub(super) fn check_dependency(&self) -> Result<(), Violation> {
        let stage = Stage::Dependency;
        let read = &self.dependency;
        let program = &self.program.graph;
        let rules = program.rules.len();
        ensure(
            read.revision == program.revision && read.graph_revision == program.revision,
            stage,
            Law::OneRevision,
            0,
        )?;
        ensure(
            read.edges.len() == rules,
            stage,
            Law::EdgeRowPerRule,
            read.edges.len(),
        )?;

        let references = rule_references(program, &self.program.owners);
        for (rule, edges) in read.edges.iter().enumerate() {
            ensure(
                edges.windows(2).all(|pair| pair[0] < pair[1])
                    && edges.iter().all(|target| *target < rules),
                stage,
                Law::EdgesSortedUnique,
                rule,
            )?;
            ensure(
                edges.iter().copied().eq(references[rule].iter().copied()),
                stage,
                Law::EdgesAreReferences,
                rule,
            )?;
        }

        let mut component_of = vec![None; rules];
        for (index, members) in read.components.iter().enumerate() {
            ensure(
                !members.is_empty() && members.windows(2).all(|pair| pair[0] < pair[1]),
                stage,
                Law::ComponentsPartitionRules,
                index,
            )?;
            for &rule in members {
                ensure(
                    rule < rules && component_of[rule].is_none(),
                    stage,
                    Law::ComponentsPartitionRules,
                    index,
                )?;
                component_of[rule] = Some(index);
            }
        }
        ensure(
            component_of.iter().all(Option::is_some)
                && read.components.windows(2).all(|pair| pair[0] < pair[1]),
            stage,
            Law::ComponentsPartitionRules,
            read.components.len(),
        )?;
        ensure(
            read.component_by_rule.len() == rules,
            stage,
            Law::ComponentIndex,
            read.component_by_rule.len(),
        )?;
        for (rule, component) in read.component_by_rule.iter().enumerate() {
            ensure(
                component_of[rule] == Some(*component),
                stage,
                Law::ComponentIndex,
                rule,
            )?;
        }

        // Within a component every member reaches the first and is reached
        // from it, along edges that stay inside the component.
        for (index, members) in read.components.iter().enumerate() {
            let inside = members.iter().copied().collect::<BTreeSet<_>>();
            let forward = reach(members[0], &inside, |rule| read.edges[rule].clone());
            let backward = reach(members[0], &inside, |rule| {
                members
                    .iter()
                    .copied()
                    .filter(|&source| read.edges[source].binary_search(&rule).is_ok())
                    .collect()
            });
            ensure(
                forward.len() == members.len() && backward.len() == members.len(),
                stage,
                Law::ComponentsStronglyConnected,
                index,
            )?;
        }

        let mut position = vec![None; read.components.len()];
        for (at, &component) in read.order.iter().enumerate() {
            ensure(
                component < position.len() && position[component].is_none(),
                stage,
                Law::DependencyFirstOrder,
                at,
            )?;
            position[component] = Some(at);
        }
        ensure(
            position.iter().all(Option::is_some),
            stage,
            Law::DependencyFirstOrder,
            read.order.len(),
        )?;
        for (rule, edges) in read.edges.iter().enumerate() {
            let source = read.component_by_rule[rule];
            for &target in edges {
                let dependency = read.component_by_rule[target];
                ensure(
                    dependency == source || position[dependency] < position[source],
                    stage,
                    Law::DependencyFirstOrder,
                    rule,
                )?;
            }
        }
        Ok(())
    }

    /// TypeSolution: one derivation per program node and rule, in program
    /// order; every type reference in its arena; each rule's named type is
    /// its binder and its root type is its root node's type.
    pub(super) fn check_types(&self) -> Result<(), Violation> {
        let stage = Stage::Types;
        let read = &self.types;
        let program = &self.program.graph;
        ensure(
            read.revision == program.revision,
            stage,
            Law::OneRevision,
            0,
        )?;
        ensure(
            read.nodes.len() == program.nodes.len() && read.rules.len() == program.rules.len(),
            stage,
            Law::RowPerProgramRow,
            read.nodes.len(),
        )?;
        for (index, (node, _)) in read.nodes.iter().enumerate() {
            ensure(*node == index, stage, Law::RowIdentity, index)?;
        }
        for (index, rule) in read.rules.iter().enumerate() {
            ensure(rule.rule == index, stage, Law::RowIdentity, index)?;
        }
        check_arena(&read.arena, program, stage)?;
        for (index, (_, type_id)) in read.nodes.iter().enumerate() {
            ensure(*type_id < read.arena.len(), stage, Law::TypeInRange, index)?;
        }
        for (index, rule) in read.rules.iter().enumerate() {
            ensure(
                rule.named < read.arena.len() && rule.root < read.arena.len(),
                stage,
                Law::TypeInRange,
                index,
            )?;
            ensure(
                read.arena[rule.named] == TypeRow::Named(index),
                stage,
                Law::NamedBinder,
                index,
            )?;
            ensure(
                read.nodes.get(program.rules[index].root).map(|row| row.1) == Some(rule.root),
                stage,
                Law::RootConclusion,
                index,
            )?;
        }
        Ok(())
    }

    /// AnalysisFacts: one fact per program node and rule, in program order;
    /// a nullable row consumes nothing at least; a rule's fact is its root's;
    /// and each node's fact follows from its children's where the transfer is
    /// exact.
    pub(super) fn check_facts(&self) -> Result<(), Violation> {
        let stage = Stage::Analysis;
        let read = &self.facts;
        let program = &self.program.graph;
        ensure(
            read.revision == program.revision,
            stage,
            Law::OneRevision,
            0,
        )?;
        ensure(
            read.nodes.len() == program.nodes.len() && read.rules.len() == program.rules.len(),
            stage,
            Law::RowPerProgramRow,
            read.nodes.len(),
        )?;
        for (index, row) in read.nodes.iter().chain(&read.rules).enumerate() {
            let expected = if index < read.nodes.len() {
                index
            } else {
                index - read.nodes.len()
            };
            ensure(row.id == expected, stage, Law::RowIdentity, index)?;
            ensure(
                !row.nullable || row.minimum == Some(0),
                stage,
                Law::NullableHasZeroMinimum,
                index,
            )?;
        }
        for (index, rule) in read.rules.iter().enumerate() {
            let root = read.nodes[program.rules[index].root];
            ensure(
                (rule.nullable, rule.minimum) == (root.nullable, root.minimum),
                stage,
                Law::RuleFactIsRootFact,
                index,
            )?;
        }
        for (index, (_, op)) in program.nodes.iter().enumerate() {
            let fact = read.nodes[index];
            let child = |node: usize| read.nodes[node];
            let holds = match op {
                Op::Epsilon => fact.nullable && fact.minimum == Some(0),
                Op::Literal(length) => {
                    fact.nullable == (*length == 0) && fact.minimum == Some(*length as u64)
                }
                Op::RuleRef(rule) => {
                    let target = read.rules[*rule];
                    (fact.nullable, fact.minimum) == (target.nullable, target.minimum)
                }
                Op::Sequence(children) => {
                    fact.nullable == children.iter().all(|&node| child(node).nullable)
                        && fact.minimum == sequence_minimum(children.iter().map(|&n| child(n)))
                }
                Op::Choice(children) => {
                    fact.nullable == children.iter().any(|&node| child(node).nullable)
                        && fact.minimum
                            == children
                                .iter()
                                .filter_map(|&node| child(node).minimum)
                                .min()
                }
                Op::Repeat { minimum: 0, .. } => fact.nullable,
                Op::Regex(_)
                | Op::Repeat { .. }
                | Op::Transform { .. }
                | Op::Projection { .. }
                | Op::Difference { .. }
                | Op::Trim { .. }
                | Op::Pratt { .. }
                | Op::Record { .. } => true,
            };
            ensure(holds, stage, Law::FactTransfer, index)?;
        }
        Ok(())
    }

    /// LoweringPlan: the plan's rule bodies obey the program's graph laws,
    /// keep the program's rules and entry, type every node, rule and field
    /// inside the plan's own arena, and carry well-formed regex tables.
    pub(super) fn check_lowering(&self) -> Result<(), Violation> {
        let stage = Stage::Lowering;
        let read = &self.plan;
        let program = &self.program.graph;
        let plan = &read.graph;
        ensure(
            plan.revision == program.revision,
            stage,
            Law::OneRevision,
            0,
        )?;
        check_graph(plan, stage, Some(read.regexes.len()))?;
        ensure(
            plan.rules.len() == program.rules.len() && plan.entry == program.entry,
            stage,
            Law::PlanKeepsRules,
            plan.rules.len(),
        )?;
        for (index, (rule, kept)) in plan.rules.iter().zip(&program.rules).enumerate() {
            ensure(rule.name == kept.name, stage, Law::PlanKeepsRules, index)?;
        }
        check_arena(&read.arena, program, stage)?;
        ensure(
            read.node_types.len() == plan.nodes.len()
                && read.rule_types.len() == plan.rules.len()
                && read.field_types.len() == plan.fields.len(),
            stage,
            Law::TypeInRange,
            read.node_types.len(),
        )?;
        for (index, type_id) in read
            .node_types
            .iter()
            .chain(&read.rule_types)
            .chain(&read.field_types)
            .enumerate()
        {
            ensure(*type_id < read.arena.len(), stage, Law::TypeInRange, index)?;
        }
        for (index, type_id) in read.rule_types.iter().enumerate() {
            ensure(
                read.arena[*type_id] == TypeRow::Named(index),
                stage,
                Law::NamedBinder,
                index,
            )?;
        }
        for (index, regex) in read.regexes.iter().chain(&read.trivia).enumerate() {
            ensure(
                regex_well_formed(regex),
                stage,
                Law::RegexTableWellFormed,
                index,
            )?;
        }
        for (index, (_, op)) in plan.nodes.iter().enumerate() {
            ensure(
                !matches!(op, Op::Trim { .. }) || read.trivia.is_some(),
                stage,
                Law::TrimNeedsTrivia,
                index,
            )?;
        }
        Ok(())
    }

    /// ScalarEventPlan: the schema of record; every site's references land in
    /// the plan; each site's shape fits its kind and the plan node it names;
    /// every rule is entered and left once; input ends once, last; and the
    /// transform sites are exactly the plan's transforms.
    pub(super) fn check_events(&self) -> Result<(), Violation> {
        let stage = Stage::Events;
        let read = &self.events;
        let plan = &self.plan.graph;
        ensure(read.revision == plan.revision, stage, Law::OneRevision, 0)?;
        ensure(read.schema == EVENT_SCHEMA, stage, Law::EventSchema, 0)?;
        let mut entered = vec![0_usize; plan.rules.len()];
        let mut left = vec![0_usize; plan.rules.len()];
        for (index, site) in read.sites.iter().enumerate() {
            ensure(
                site.node.is_none_or(|node| node < plan.nodes.len())
                    && site.rule.is_none_or(|rule| rule < plan.rules.len())
                    && site.field.is_none_or(|field| field < plan.fields.len())
                    && site
                        .operator
                        .is_none_or(|operator| operator < plan.operators.len()),
                stage,
                Law::SiteShape,
                index,
            )?;
            let op = site.node.map(|node| &plan.nodes[node].1);
            let slots = (
                site.node.is_some(),
                site.rule.is_some(),
                site.field.is_some(),
                site.operator.is_some(),
            );
            let shaped = match site.kind {
                EventKind::EnterRule | EventKind::LeaveRule => slots == (false, true, false, false),
                EventKind::EnterField | EventKind::LeaveField => slots == (true, true, true, false),
                EventKind::Literal => {
                    slots == (true, false, false, false) && matches!(op, Some(Op::Literal(_)))
                }
                EventKind::Regex => {
                    slots == (true, false, false, false) && matches!(op, Some(Op::Regex(_)))
                }
                EventKind::ChoiceBranch => {
                    slots == (true, false, false, false) && matches!(op, Some(Op::Choice(_)))
                }
                EventKind::RepeatCount => {
                    slots == (true, false, false, false) && matches!(op, Some(Op::Repeat { .. }))
                }
                EventKind::Transform => {
                    slots == (true, false, false, false) && matches!(op, Some(Op::Transform { .. }))
                }
                EventKind::Trivia => {
                    slots == (true, false, false, false) && matches!(op, Some(Op::Trim { .. }))
                }
                EventKind::PrattReduce => {
                    slots == (true, false, false, true) && matches!(op, Some(Op::Pratt { .. }))
                }
                EventKind::EndInput => slots == (false, false, false, false),
            };
            ensure(shaped, stage, Law::SiteShape, index)?;
            match site.kind {
                EventKind::EnterRule => entered[site.rule.unwrap_or_default()] += 1,
                EventKind::LeaveRule => left[site.rule.unwrap_or_default()] += 1,
                EventKind::EnterField | EventKind::LeaveField => {
                    let field = plan.fields[site.field.unwrap_or_default()];
                    ensure(
                        Some(field.owner) == site.rule && Some(field.child) == site.node,
                        stage,
                        Law::FieldSiteIsItsField,
                        index,
                    )?;
                }
                EventKind::PrattReduce => {
                    let Some(Op::Pratt { table, .. }) = op else {
                        unreachable!("the site's shape was read above")
                    };
                    let table = plan.tables[*table];
                    let operator = site.operator.unwrap_or_default();
                    ensure(
                        (table.start..table.start + table.len).contains(&operator),
                        stage,
                        Law::PrattReduceIsItsTable,
                        index,
                    )?;
                }
                EventKind::EndInput => ensure(
                    index + 1 == read.sites.len(),
                    stage,
                    Law::InputEndsOnceLast,
                    index,
                )?,
                EventKind::Literal
                | EventKind::Regex
                | EventKind::ChoiceBranch
                | EventKind::RepeatCount
                | EventKind::Transform
                | EventKind::Trivia => {}
            }
        }
        ensure(
            read.sites
                .last()
                .is_some_and(|site| site.kind == EventKind::EndInput),
            stage,
            Law::InputEndsOnceLast,
            read.sites.len(),
        )?;
        for (rule, (entries, leaves)) in entered.iter().zip(&left).enumerate() {
            ensure(
                *entries == 1 && *leaves == 1,
                stage,
                Law::RuleEnteredAndLeftOnce,
                rule,
            )?;
        }
        let transforms = plan
            .nodes
            .iter()
            .filter_map(|(node, op)| match op {
                Op::Transform {
                    transform, name, ..
                } => Some((*transform, *node, name.clone())),
                _ => None,
            })
            .collect::<Vec<_>>();
        ensure(
            read.transforms.len() == transforms.len(),
            stage,
            Law::TransformSites,
            read.transforms.len(),
        )?;
        for (index, (site, expected)) in read.transforms.iter().zip(&transforms).enumerate() {
            ensure(
                site.0 == index && site == expected,
                stage,
                Law::TransformSites,
                index,
            )?;
        }
        Ok(())
    }
}

/// The laws the program and the plan share. Returns the rule whose body holds
/// each node.
fn check_graph(
    graph: &Graph,
    stage: Stage,
    regexes: Option<usize>,
) -> Result<Vec<usize>, Violation> {
    let nodes = graph.nodes.len();
    let rules = graph.rules.len();
    let fields = graph.fields.len();
    let tables = graph.tables.len();
    let operators = graph.operators.len();

    for (index, (id, _)) in graph.nodes.iter().enumerate() {
        ensure(*id == index, stage, Law::RowIdentity, index)?;
    }
    for (index, rule) in graph.rules.iter().enumerate() {
        ensure(rule.id == index, stage, Law::RowIdentity, index)?;
        ensure(rule.root < nodes, stage, Law::NodeInRange, index)?;
    }
    for (index, field) in graph.fields.iter().enumerate() {
        ensure(field.id == index, stage, Law::RowIdentity, index)?;
        ensure(field.owner < rules, stage, Law::RuleInRange, index)?;
        ensure(field.child < nodes, stage, Law::NodeInRange, index)?;
    }
    ensure(graph.entry < rules, stage, Law::RuleInRange, rules)?;
    for (index, table) in graph.tables.iter().enumerate() {
        ensure(table.id == index, stage, Law::RowIdentity, index)?;
        ensure(
            table.site < rules && table.atom < rules,
            stage,
            Law::RuleInRange,
            index,
        )?;
        ensure(
            table
                .start
                .checked_add(table.len)
                .is_some_and(|end| end <= operators),
            stage,
            Law::PrattInRange,
            index,
        )?;
    }
    for (index, operator) in graph.operators.iter().enumerate() {
        ensure(operator.id == index, stage, Law::RowIdentity, index)?;
        ensure(operator.matcher < rules, stage, Law::RuleInRange, index)?;
        ensure(operator.table < tables, stage, Law::PrattInRange, index)?;
        let table = graph.tables[operator.table];
        ensure(
            (table.start..table.start + table.len).contains(&index),
            stage,
            Law::PrattOperatorInItsTable,
            index,
        )?;
        ensure(
            match operator.fixity {
                PrattFixity::Prefix => operator.left.is_none() && operator.right.is_some(),
                PrattFixity::Postfix => operator.left.is_some() && operator.right.is_none(),
                PrattFixity::Infix => operator.left.is_some() && operator.right.is_some(),
            },
            stage,
            Law::PrattFixityShape,
            index,
        )?;
    }

    let mut names = BTreeSet::new();
    for (index, rule) in graph.rules.iter().enumerate() {
        ensure(
            !rule.name.is_empty() && names.insert(rule.name.as_str()),
            stage,
            Law::RuleNamesUnique,
            index,
        )?;
    }

    let mut transforms = 0_usize;
    for (index, (_, op)) in graph.nodes.iter().enumerate() {
        ensure(
            op.children().iter().all(|child| *child < nodes),
            stage,
            Law::NodeInRange,
            index,
        )?;
        match op {
            Op::RuleRef(rule) => ensure(*rule < rules, stage, Law::RuleInRange, index)?,
            Op::Record { owner, slots } => {
                ensure(*owner < rules, stage, Law::RuleInRange, index)?;
                for slot in slots {
                    if let Some(field) = slot.field {
                        ensure(field < fields, stage, Law::FieldInRange, index)?;
                        let declared = graph.fields[field];
                        ensure(
                            declared.owner == *owner && declared.child == slot.child,
                            stage,
                            Law::RecordSlotIsItsField,
                            index,
                        )?;
                    }
                }
            }
            Op::Pratt { table, .. } => ensure(*table < tables, stage, Law::PrattInRange, index)?,
            Op::Regex(table) => ensure(
                match (regexes, table) {
                    (None, None) => true,
                    (Some(count), Some(table)) => *table < count,
                    (None, Some(_)) | (Some(_), None) => false,
                },
                stage,
                Law::RegexInRange,
                index,
            )?,
            Op::Sequence(children) | Op::Choice(children) => ensure(
                !children.is_empty(),
                stage,
                Law::AggregateHasChildren,
                index,
            )?,
            Op::Repeat {
                minimum, maximum, ..
            } => ensure(
                maximum.is_none_or(|maximum| maximum >= *minimum && maximum > 0),
                stage,
                Law::RepeatBounds,
                index,
            )?,
            Op::Transform { transform, .. } => {
                ensure(
                    *transform == transforms,
                    stage,
                    Law::TransformOrdinals,
                    index,
                )?;
                transforms += 1;
            }
            Op::Epsilon
            | Op::Literal(_)
            | Op::Projection { .. }
            | Op::Difference { .. }
            | Op::Trim { .. } => {}
        }
    }

    // Each rule body is a tree hanging from its root: no node is reached
    // twice (no sharing, no cycle) and every node is reached from some root.
    let mut owner = vec![None; nodes];
    for (rule, row) in graph.rules.iter().enumerate() {
        let mut pending = vec![row.root];
        while let Some(node) = pending.pop() {
            ensure(owner[node].is_none(), stage, Law::RuleBodiesAreTrees, node)?;
            owner[node] = Some(rule);
            pending.extend(graph.nodes[node].1.children());
        }
    }
    let owner = owner
        .iter()
        .enumerate()
        .map(|(node, rule)| {
            rule.ok_or(Violation {
                stage,
                law: Law::RuleBodiesAreTrees,
                row: node,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    for (index, (_, op)) in graph.nodes.iter().enumerate() {
        if let Op::Record { owner: record, .. } = op {
            ensure(
                owner[index] == *record,
                stage,
                Law::RecordBelongsToItsRule,
                index,
            )?;
        }
    }
    Ok(owner)
}

/// The rules each rule body references: its rule references and, for a Pratt
/// node, its table's operator matchers.
fn rule_references(graph: &Graph, owners: &[usize]) -> Vec<BTreeSet<usize>> {
    let mut references = vec![BTreeSet::new(); graph.rules.len()];
    for ((_, op), &owner) in graph.nodes.iter().zip(owners) {
        match op {
            Op::RuleRef(rule) => {
                references[owner].insert(*rule);
            }
            Op::Pratt { table, .. } => {
                let table = graph.tables[*table];
                for operator in &graph.operators[table.start..table.start + table.len] {
                    references[owner].insert(operator.matcher);
                }
            }
            _ => {}
        }
    }
    references
}

/// A type arena is closed (every child in range), names only program rules
/// and fields, keeps each record field with its owner, and holds no duplicate.
fn check_arena(arena: &[TypeRow], program: &Graph, stage: Stage) -> Result<(), Violation> {
    let rules = program.rules.len();
    for (index, row) in arena.iter().enumerate() {
        match row {
            TypeRow::Unit | TypeRow::Span => {}
            TypeRow::Named(rule) => ensure(*rule < rules, stage, Law::RuleInRange, index)?,
            TypeRow::Product(children) | TypeRow::Sum(children) => ensure(
                children.iter().all(|child| *child < arena.len()),
                stage,
                Law::TypeInRange,
                index,
            )?,
            TypeRow::Option(child) | TypeRow::List(child) => {
                ensure(*child < arena.len(), stage, Law::TypeInRange, index)?;
            }
            TypeRow::Record { owner, fields } => {
                ensure(*owner < rules, stage, Law::RuleInRange, index)?;
                for &(field, value) in fields {
                    ensure(
                        program
                            .fields
                            .get(field)
                            .is_some_and(|row| row.owner == *owner),
                        stage,
                        Law::FieldInRange,
                        index,
                    )?;
                    ensure(value < arena.len(), stage, Law::TypeInRange, index)?;
                }
            }
        }
    }
    let unique = arena.iter().collect::<BTreeSet<_>>();
    ensure(
        unique.len() == arena.len(),
        stage,
        Law::ArenaCanonical,
        arena.len(),
    )
}

/// The least bytes a sequence consumes: the sum of its children's, or none
/// when a child has an empty language or the sum overflows.
fn sequence_minimum(children: impl Iterator<Item = FactRow>) -> Option<u64> {
    children
        .map(|child| child.minimum)
        .try_fold(0_u64, |sum, minimum| sum.checked_add(minimum?))
}

/// A regex table is a total DFA over its byte classes.
fn regex_well_formed(regex: &RegexRow) -> bool {
    let states = regex.state_count as usize;
    let classes = usize::from(regex.class_count);
    regex.byte_classes.len() == 256
        && regex
            .byte_classes
            .iter()
            .all(|class| usize::from(*class) < classes)
        && states.checked_mul(classes) == Some(regex.transitions.len())
        && regex
            .transitions
            .iter()
            .all(|target| *target == DEAD_STATE || (*target as usize) < states)
        && regex.accepting.len() == states
        && (regex.start_state as usize) < states
}

/// The members of `inside` reachable from `start` by `next`, `start` included.
fn reach(
    start: usize,
    inside: &BTreeSet<usize>,
    next: impl Fn(usize) -> Vec<usize>,
) -> BTreeSet<usize> {
    let mut reached = BTreeSet::from([start]);
    let mut pending = vec![start];
    while let Some(rule) = pending.pop() {
        for target in next(rule) {
            if inside.contains(&target) && reached.insert(target) {
                pending.push(target);
            }
        }
    }
    reached
}
