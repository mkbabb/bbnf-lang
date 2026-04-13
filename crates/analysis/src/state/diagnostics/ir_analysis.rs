use std::collections::{HashMap, HashSet};

use bbnf::grammar::generated::BbnfBootstrapNodeView;
use bbnf::lower::DirectiveSet;
use bbnf::pipeline::{PipelineOptions, compile_ast};

use super::super::parsing::CachedParseResult;
use super::super::types::IrRuleMeta;

/// Results from the IR pipeline: per-rule metadata + FIRST set analysis.
#[derive(Default)]
pub(super) struct IrAnalysis {
    pub meta: HashMap<String, IrRuleMeta>,
    pub first_set_labels: HashMap<String, String>,
    pub nullable_rules: HashSet<String>,
    pub first_set_conflicts: Vec<(String, Vec<String>)>,
}

/// Run the IR pipeline on a cached parse result and extract per-rule metadata.
///
/// On failure (e.g., the grammar is incomplete or uses features not yet supported
/// by the IR lowering), returns defaults -- callers degrade gracefully.
pub(super) fn try_compile_ir(cached: &CachedParseResult<'_>) -> IrAnalysis {
    let ast = cached.ast.clone();

    // Reconstruct directive maps from the analysis-layer types.
    // `DirectiveSet::recovers` is typed over `BbnfBootstrapNodeView`
    // (the tape-first RHS reference); we hand the IR compile an
    // empty map because the analysis layer has already surfaced
    // directive diagnostics at this point and the IR pipeline only
    // uses the map to re-thread recover sync expressions through
    // codegen.
    let recover_map: HashMap<String, BbnfBootstrapNodeView<'_>> = HashMap::new();

    let pretty_map: HashMap<String, Vec<String>> = cached
        .pretties
        .iter()
        .map(|p| (p.rule_name.clone(), p.hints.clone()))
        .collect();

    let token_set: HashSet<String> = cached
        .tokens
        .iter()
        .map(|tok| tok.rule_name.clone())
        .collect();
    let token_ref = if token_set.is_empty() {
        None
    } else {
        Some(&token_set)
    };

    let mut debug_set: HashSet<String> = HashSet::new();
    let mut debug_all = false;
    for dbg in &cached.debugs {
        if dbg.rule_name == "*" {
            debug_all = true;
        } else {
            debug_set.insert(dbg.rule_name.clone());
        }
    }
    let debug_ref = if debug_set.is_empty() {
        None
    } else {
        Some(&debug_set)
    };

    let ws_pattern = cached.ws_pattern.as_ref().map(|ws| ws.pattern.as_str());

    let recovers_ref = if recover_map.is_empty() {
        None
    } else {
        Some(&recover_map)
    };
    let pretties_ref = if pretty_map.is_empty() {
        None
    } else {
        Some(&pretty_map)
    };

    let directives = DirectiveSet {
        recovers: recovers_ref,
        pretties: pretties_ref,
        ws_pattern,
        token_rules: token_ref,
        debug_rules: debug_ref,
        debug_all,
        host_fns: None,
    };

    // Structural mode: disable destructive optimization passes (inline,
    // fuse, prune) so all user-authored rules survive in the IR. This
    // is required for correct FIRST set conflict, alias, and
    // unreachable-rule diagnostics — the analysis layer needs every
    // rule the user wrote, not the optimizer's minimal form.
    let options = PipelineOptions {
        structural: true,
        ..PipelineOptions::default()
    };

    let ir = match compile_ast(ast, &directives, &options) {
        Ok(ir) => ir,
        Err(_) => return IrAnalysis::default(),
    };

    // Build a lookup from RuleId -> TypeDesc.
    let type_map: HashMap<u32, &bbnf_ir::TypeDesc> =
        ir.types.iter().map(|(id, td)| (*id, td)).collect();

    let mut meta = HashMap::new();
    let mut first_set_labels = HashMap::new();
    let mut nullable_rules = HashSet::new();

    for rule in &ir.rules {
        let name = ir.get_string(rule.name).to_string();

        // FIRST set label from IR metadata.
        if !rule.meta.first_set.is_empty() {
            first_set_labels.insert(
                name.clone(),
                format_charset_iter(rule.meta.first_set.iter()),
            );
        }

        // Nullable from IR metadata.
        if rule.meta.nullable {
            nullable_rules.insert(name.clone());
        }

        let follow_set_label = ir
            .follow_sets
            .get(&rule.id)
            .map(|cs| format_charset_iter(cs.iter()));

        let projected_type = type_map.get(&rule.id).map(|td| format_type_desc(td, &ir));

        meta.insert(
            name,
            IrRuleMeta {
                follow_set_label,
                has_dispatch: rule.meta.dispatch.is_some(),
                memo_strategy: format!("{:?}", rule.meta.memo),
                span_eligible: rule.meta.span_eligible,
                has_sp_method: rule.meta.has_sp_method,
                projected_type,
                is_transparent: rule.meta.is_transparent,
            },
        );
    }

    // FIRST set conflict detection from IR Alt branches.
    let mut first_set_conflicts = Vec::new();
    for rule in &ir.rules {
        let name = ir.get_string(rule.name).to_string();
        if let bbnf_ir::IrNode::Alt(branches, _) = &rule.body {
            let mut conflicts = Vec::new();
            for i in 0..branches.len() {
                for j in (i + 1)..branches.len() {
                    if let (Some(fs_i), Some(fs_j)) =
                        (&branches[i].first_set, &branches[j].first_set)
                    {
                        if !fs_i.is_disjoint(fs_j) {
                            let overlap = fs_i.intersection(fs_j);
                            conflicts.push(format!(
                                "branches {} and {} overlap on {}",
                                i,
                                j,
                                format_charset_iter(overlap.iter())
                            ));
                        }
                    }
                }
            }
            if !conflicts.is_empty() {
                first_set_conflicts.push((name, conflicts));
            }
        }
    }

    IrAnalysis {
        meta,
        first_set_labels,
        nullable_rules,
        first_set_conflicts,
    }
}

/// Format a set of byte values for display (e.g., `{'a', 'b', 0x0a}`).
fn format_charset_iter(iter: impl IntoIterator<Item = u8>) -> String {
    let chars: Vec<u8> = iter.into_iter().collect();
    if chars.is_empty() {
        return "\u{2205}".into(); // empty set
    }
    let formatted: Vec<String> = chars
        .iter()
        .map(|&b| {
            if b.is_ascii_graphic() {
                format!("'{}'", b as char)
            } else {
                format!("0x{:02x}", b)
            }
        })
        .collect();
    format!("{{{}}}", formatted.join(", "))
}

/// Format a `TypeDesc` as a human-readable string.
fn format_type_desc(td: &bbnf_ir::TypeDesc, ir: &bbnf_ir::GrammarIR) -> String {
    match td {
        bbnf_ir::TypeDesc::Span => "Span".into(),
        bbnf_ir::TypeDesc::F64 => "f64".into(),
        bbnf_ir::TypeDesc::Bool => "bool".into(),
        bbnf_ir::TypeDesc::U8 => "u8".into(),
        bbnf_ir::TypeDesc::U32 => "u32".into(),
        bbnf_ir::TypeDesc::Option(inner) => format!("Option<{}>", format_type_desc(inner, ir)),
        bbnf_ir::TypeDesc::Vec(inner) => format!("Vec<{}>", format_type_desc(inner, ir)),
        bbnf_ir::TypeDesc::Tuple(items) => {
            let parts: Vec<_> = items.iter().map(|t| format_type_desc(t, ir)).collect();
            format!("({})", parts.join(", "))
        }
        bbnf_ir::TypeDesc::BoxedEnum => "Box<Enum>".into(),
        bbnf_ir::TypeDesc::Enum => "Enum".into(),
        bbnf_ir::TypeDesc::Named(id) => ir.get_string(*id).to_string(),
    }
}
