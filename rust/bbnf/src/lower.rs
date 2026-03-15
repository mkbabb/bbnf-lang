//! Lowering pass: Expression AST → GrammarIR.
//!
//! This module converts the parsed BBNF `Expression` AST (with all analysis results)
//! into the canonical `GrammarIR` consumed by all backends.

use std::collections::{HashMap, HashSet};

use bbnf_ir::{
    AltBranch, CharSet128, DispatchHint, FnDescriptor, FnId, GrammarIR, IrNode, IrRule,
    MemoStrategy, PrettyHints, RuleId, RuleMeta, StringId,
};

use crate::analysis::{
    CharSet, DispatchTable, FirstSets, SccResult,
};
use crate::generate::prettify::hints;
use crate::types::{Expression, Token, AST};

/// String interning table used during lowering.
struct StringInterner {
    strings: Vec<String>,
    map: HashMap<String, StringId>,
}

impl StringInterner {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn intern(&mut self, s: &str) -> StringId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = self.strings.len() as StringId;
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }
}

/// Host function table used during lowering.
struct FnTable {
    fns: Vec<FnDescriptor>,
}

impl FnTable {
    fn new() -> Self {
        Self { fns: Vec::new() }
    }

    fn push(&mut self, desc: FnDescriptor) -> FnId {
        let id = self.fns.len() as FnId;
        self.fns.push(desc);
        id
    }
}

/// Context for the lowering pass.
struct LowerCtx<'a> {
    strings: StringInterner,
    fns: FnTable,

    /// Map from nonterminal name → RuleId.
    name_to_rule_id: HashMap<&'a str, RuleId>,

    /// Analysis results.
    first_sets: &'a FirstSets<'a>,
    scc_result: &'a SccResult<'a>,

    /// Metadata maps.
    aliases: &'a HashMap<&'a Expression<'a>, &'a Expression<'a>>,
    transparent_rules: &'a HashSet<String>,
    span_eligible_rules: &'a HashSet<String>,
    cyclic_rules: &'a HashSet<Expression<'a>>,

    /// Directives.
    recovers: Option<&'a HashMap<String, Expression<'a>>>,
    pretties: Option<&'a HashMap<String, Vec<String>>>,
    no_collapse_rules: Option<&'a HashSet<String>>,

    /// The current LHS expression being lowered (for branch_firsts lookup).
    current_lhs: Option<&'a Expression<'a>>,

    /// When true, unknown nonterminals emit Epsilon instead of Literal fallback,
    /// and unsupported expressions emit Epsilon. Used for recovery sync expressions.
    recovery_mode: bool,
}

/// Convert a `CharSet` ([u32; 4]) to a `CharSet128` ([u64; 2]).
fn charset_to_128(cs: &CharSet) -> CharSet128 {
    CharSet128::from_u32x4(&cs.bits)
}

/// Lower a full BBNF grammar (AST + analysis results) to the canonical `GrammarIR`.
///
/// # Arguments
///
/// * `ast` — Topologically sorted AST.
/// * `first_sets` — Pre-computed FIRST sets.
/// * `scc_result` — SCC analysis results.
/// * `aliases` — Alias detection results.
/// * `transparent_rules` — Transparent alternation rule names.
/// * `span_eligible_rules` — Span-eligible rule names.
/// * `recovers` — `@recover` directive map (rule_name → sync expression).
/// * `pretties` — `@pretty` directive map (rule_name → hint strings).
/// * `no_collapse_rules` — `@no_collapse` rule name set.
/// * `dispatch_tables` — Pre-built dispatch tables for alternation rules.
#[allow(clippy::too_many_arguments)]
pub fn lower_to_ir<'a>(
    ast: &'a AST<'a>,
    first_sets: &'a FirstSets<'a>,
    scc_result: &'a SccResult<'a>,
    aliases: &'a HashMap<&'a Expression<'a>, &'a Expression<'a>>,
    transparent_rules: &'a HashSet<String>,
    span_eligible_rules: &'a HashSet<String>,
    recovers: Option<&'a HashMap<String, Expression<'a>>>,
    pretties: Option<&'a HashMap<String, Vec<String>>>,
    no_collapse_rules: Option<&'a HashSet<String>>,
    dispatch_tables: &'a HashMap<String, DispatchTable>,
) -> GrammarIR {
    let mut ctx = LowerCtx {
        strings: StringInterner::new(),
        fns: FnTable::new(),
        name_to_rule_id: HashMap::new(),
        first_sets,
        scc_result,
        aliases,
        transparent_rules,
        span_eligible_rules,
        cyclic_rules: &scc_result.cyclic_rules,
        recovers,
        pretties,
        no_collapse_rules,
        current_lhs: None,
        recovery_mode: false,
    };

    // Phase 1: Assign RuleIds to all nonterminal names.
    let mut rule_names: Vec<&str> = Vec::new();
    for (lhs, _) in ast.iter() {
        if let Expression::Nonterminal(Token { value, .. }) = lhs {
            let id = rule_names.len() as RuleId;
            ctx.name_to_rule_id.insert(value.as_ref(), id);
            rule_names.push(value.as_ref());
        }
    }

    // Phase 2: Lower rule bodies (interns all body strings).
    let mut rule_bodies: Vec<(RuleId, StringId, IrNode, &str, &Expression)> =
        Vec::with_capacity(rule_names.len());

    for (lhs, rhs) in ast.iter() {
        let name = match lhs {
            Expression::Nonterminal(Token { value, .. }) => value.as_ref(),
            _ => continue,
        };

        let rule_id = ctx.name_to_rule_id[name];
        let name_id = ctx.strings.intern(name);

        // Unwrap Rule wrapper to get the body expression.
        let body_expr = unwrap_rule(rhs);

        // Set current LHS for branch_firsts lookup during alternation lowering.
        ctx.current_lhs = Some(lhs);

        // Lower the body expression.
        let body = lower_expression(body_expr, &mut ctx);

        rule_bodies.push((rule_id, name_id, body, name, lhs));
    }

    // Phase 3: Build metadata (recovery expressions can now intern strings too).
    let mut rules = Vec::with_capacity(rule_bodies.len());
    for (rule_id, name_id, body, name, lhs) in rule_bodies {
        let meta = build_rule_meta(lhs, name, &mut ctx, dispatch_tables);
        rules.push(IrRule {
            id: rule_id,
            name: name_id,
            body,
            meta,
        });
    }

    // Default entry: last rule in the (topologically sorted) IR.
    // Callers should override this with the actual entry rule from the
    // original source order (e.g., the last rule in the grammar file).
    let entry = rules.last().map(|r| r.id).unwrap_or(0);

    GrammarIR {
        rules,
        entry,
        strings: ctx.strings.strings,
        fns: ctx.fns.fns,
        types: Vec::new(), // Type info populated by a later pass or backend.
        follow_sets: HashMap::new(), // Populated by compute_follow_sets pass.
    }
}

/// Unwrap a `Rule(inner, mapping)` to get the inner expression.
fn unwrap_rule<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::Rule(inner, _) => inner,
        other => other,
    }
}

/// Lower a mapping function expression to a `FnId`.
///
/// B.3: Parses the closure's `-> ReturnType` annotation (if present) and stores
/// it as a `TypeDesc::Named` in the `FnDescriptor::Custom` variant. This allows
/// IR type inference to use the actual return type instead of the closure source text.
fn lower_mapping_fn<'a>(expr: &Expression<'a>, ctx: &mut LowerCtx<'a>) -> FnId {
    match expr {
        Expression::MappingFn(Token { value, .. }) => {
            let string_id = ctx.strings.intern(value.as_ref());
            // Try to parse the closure and extract its return type annotation.
            let return_type = parse_closure_return_type(value.as_ref(), ctx);
            ctx.fns.push(FnDescriptor::Custom {
                source: string_id,
                return_type,
            })
        }
        _ => {
            let text = format!("{:?}", expr);
            let string_id = ctx.strings.intern(&text);
            ctx.fns.push(FnDescriptor::Custom {
                source: string_id,
                return_type: None,
            })
        }
    }
}

/// Parse a Rust closure source string to extract the return type annotation.
/// Returns `Some(TypeDesc::Named(sid))` if the closure has `-> ReturnType`, None otherwise.
fn parse_closure_return_type(source: &str, ctx: &mut LowerCtx<'_>) -> Option<bbnf_ir::TypeDesc> {
    let closure = syn::parse_str::<syn::ExprClosure>(source).ok()?;
    if let syn::ReturnType::Type(_, ty) = &closure.output {
        // Intern the return type as a string for TypeDesc::Named.
        let ty_str = quote::ToTokens::to_token_stream(ty).to_string();
        let sid = ctx.strings.intern(&ty_str);
        Some(bbnf_ir::TypeDesc::Named(sid))
    } else {
        None
    }
}

/// Lower a single `Expression` to an `IrNode`.
fn lower_expression<'a>(expr: &'a Expression<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match expr {
        Expression::Literal(Token { value, .. }) => {
            let id = ctx.strings.intern(value.as_ref());
            IrNode::Literal(id)
        }

        Expression::Regex(Token { value, .. }) => {
            let id = ctx.strings.intern(value.as_ref());
            IrNode::Regex(id)
        }

        Expression::Epsilon(_) => IrNode::Epsilon,

        Expression::Nonterminal(Token { value, .. }) => {
            let name: &str = value.as_ref();
            match ctx.name_to_rule_id.get(name) {
                Some(&rule_id) => IrNode::Ref(rule_id),
                None if ctx.recovery_mode => IrNode::Epsilon,
                None => {
                    // Unknown nonterminal — emit as literal for robustness.
                    // Backends should validate and report this as an error.
                    let id = ctx.strings.intern(name);
                    IrNode::Literal(id)
                }
            }
        }

        Expression::Group(inner) => {
            // Group is purely syntactic — lower the inner expression directly.
            lower_expression(&inner.value, ctx)
        }

        Expression::Optional(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::Repeat {
                inner: Box::new(inner_node),
                lo: 0,
                hi: 1,
            }
        }

        Expression::Many(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::Repeat {
                inner: Box::new(inner_node),
                lo: 0,
                hi: u32::MAX,
            }
        }

        Expression::Many1(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::Repeat {
                inner: Box::new(inner_node),
                lo: 1,
                hi: u32::MAX,
            }
        }

        Expression::OptionalWhitespace(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::OptionalWhitespace(Box::new(inner_node))
        }

        Expression::Skip(left, right) => {
            let left_node = lower_expression(&left.value, ctx);
            let right_node = lower_expression(&right.value, ctx);
            IrNode::Skip(Box::new(left_node), Box::new(right_node))
        }

        Expression::Next(left, right) => {
            let left_node = lower_expression(&left.value, ctx);
            let right_node = lower_expression(&right.value, ctx);
            IrNode::Next(Box::new(left_node), Box::new(right_node))
        }

        Expression::Minus(left, right) => {
            let left_node = lower_expression(&left.value, ctx);
            let right_node = lower_expression(&right.value, ctx);
            IrNode::Minus(Box::new(left_node), Box::new(right_node))
        }

        Expression::Concatenation(token) => {
            let children: Vec<IrNode> = token
                .value
                .iter()
                .map(|child| lower_expression(child, ctx))
                .collect();
            if children.len() == 1 {
                children.into_iter().next().unwrap()
            } else {
                IrNode::Seq(children)
            }
        }

        Expression::Alternation(token) => {
            let branches: Vec<AltBranch> = token
                .value
                .iter()
                .enumerate()
                .map(|(i, child)| {
                    let node = lower_expression(child, ctx);

                    // Attach per-branch FIRST set if available.
                    let first_set = ctx
                        .current_lhs
                        .and_then(|lhs| ctx.first_sets.branch_firsts.get(lhs))
                        .and_then(|branch_firsts| {
                            if i < branch_firsts.len() {
                                let (ref cs, nullable) = branch_firsts[i];
                                if !nullable && !cs.is_empty() {
                                    Some(charset_to_128(cs))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        });

                    AltBranch {
                        node,
                        first_set,
                    }
                })
                .collect();

            if branches.len() == 1 {
                branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(branches, None)
            }
        }

        Expression::MappedExpression((inner, mapping_fn)) => {
            let inner_node = lower_expression(&inner.value, ctx);
            let fn_id = lower_mapping_fn(&mapping_fn.value, ctx);
            IrNode::Map {
                inner: Box::new(inner_node),
                fn_id,
            }
        }

        Expression::DebugExpression((inner, _label)) => {
            // Debug expressions are transparent in the IR — they're a development tool.
            lower_expression(&inner.value, ctx)
        }

        Expression::MappingFn(_) => {
            // A standalone mapping function reference shouldn't appear as a body,
            // but handle gracefully by treating it as a custom function with Epsilon input.
            let fn_id = lower_mapping_fn(expr, ctx);
            IrNode::Map {
                inner: Box::new(IrNode::Epsilon),
                fn_id,
            }
        }

        Expression::Rule(inner, mapping) => {
            let inner_node = lower_expression(inner, ctx);

            if let Some(mapping_expr) = mapping {
                let fn_id = lower_mapping_fn(mapping_expr.as_ref(), ctx);
                IrNode::Map {
                    inner: Box::new(inner_node),
                    fn_id,
                }
            } else {
                inner_node
            }
        }

        Expression::ProductionRule(_lhs, rhs) => {
            // Production rules shouldn't appear in the body, but handle gracefully.
            // Lower the RHS as the body.
            lower_expression(rhs, ctx)
        }
    }
}

/// Build rule metadata from analysis results.
fn build_rule_meta<'a>(
    lhs: &'a Expression<'a>,
    name: &str,
    ctx: &mut LowerCtx<'a>,
    dispatch_tables: &HashMap<String, DispatchTable>,
) -> RuleMeta {
    // FIRST set.
    let first_set = ctx
        .first_sets
        .first
        .get(lhs)
        .map(charset_to_128)
        .unwrap_or_default();

    // Nullability.
    let nullable = ctx.first_sets.nullable.contains(lhs);

    // SCC info.
    let scc_id = ctx.scc_result.scc_index.get(lhs).map(|&id| id as u32);
    let is_cyclic = ctx.cyclic_rules.contains(lhs);

    // Memoization strategy.
    let memo = if is_cyclic {
        MemoStrategy::Full
    } else {
        MemoStrategy::None
    };

    // Dispatch hint.
    let dispatch = dispatch_tables.get(name).map(|dt| DispatchHint::ByteTable {
        table: dt.table.to_vec(),
    });

    // Span eligibility.
    let span_eligible = ctx.span_eligible_rules.contains(name);

    // Alias detection.
    let is_alias = ctx.aliases.get(lhs).and_then(|target| {
        if let Expression::Nonterminal(Token { value, .. }) = target {
            ctx.name_to_rule_id.get(value.as_ref()).copied()
        } else {
            None
        }
    });

    // Transparent alternation.
    let is_transparent = ctx.transparent_rules.contains(name);

    // Pretty hints.
    let pretty = ctx
        .pretties
        .and_then(|p| p.get(name))
        .map(|hint_strs| lower_pretty_hints(hint_strs));

    // Recovery expression (lowered with recovery_mode = true).
    let recover = ctx.recovers.and_then(|r| r.get(name)).map(|sync_expr| {
        ctx.recovery_mode = true;
        let node = lower_expression(sync_expr, ctx);
        ctx.recovery_mode = false;
        node
    });

    // No-collapse.
    let no_collapse = ctx
        .no_collapse_rules
        .is_some_and(|set| set.contains(name));

    RuleMeta {
        first_set,
        nullable,
        scc_id,
        is_cyclic,
        memo,
        dispatch,
        span_eligible,
        is_alias,
        is_transparent,
        pretty,
        recover,
        no_collapse,
        has_sp_method: false, // Computed by compute_sp_method_rules pass.
        sub_variants: Vec::new(),
    }
}

/// Lower `@pretty` hint strings into structured `PrettyHints`.
fn lower_pretty_hints(hint_strs: &[String]) -> PrettyHints {
    let mut ph = PrettyHints::default();

    for hint in hint_strs {
        let h = hint.as_str();
        match h {
            "group" => ph.group = true,
            "indent" => ph.indent = true,
            "dedent" => ph.dedent = true,
            "block" => ph.block = true,
            "blankline" => ph.blankline = true,
            "nobreak" => ph.nobreak = true,
            "softbreak" => ph.softbreak = true,
            "hardbreak" => ph.hardbreak = true,
            "compact" => ph.compact = true,
            "fast" => ph.fast = true,
            "off" => ph.off = true,
            _ => {
                // Check for parameterized hints.
                if let Some(sep_str) = hints::extract_sep_string(h) {
                    ph.sep = Some(sep_str.to_string());
                } else if let Some(split_delim) = hints::extract_split_delim(h) {
                    ph.split = Some(split_delim.to_string());
                }
                // Unknown hints are silently ignored (validation happens earlier).
            }
        }
    }

    ph
}
