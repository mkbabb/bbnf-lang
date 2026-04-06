use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use indexmap::IndexMap;

use crate::imports::load_module_graph;
use crate::pipeline::CompileError;
use crate::lower::DirectiveSet;
use crate::types::{AST, Expression, ParsedGrammar};

#[derive(Default)]
pub(crate) struct DirectiveMaps<'a> {
    recover_map: HashMap<String, Expression<'a>>,
    pretty_map: HashMap<String, Vec<String>>,
    ws_pattern: Option<String>,
    token_set: HashSet<String>,
    debug_set: HashSet<String>,
    debug_all: bool,
    host_map: HashMap<String, Option<String>>,
}

impl<'a> DirectiveMaps<'a> {
    pub(crate) fn from_parsed(parsed: ParsedGrammar<'a>) -> (AST<'a>, Self) {
        let ParsedGrammar {
            imports: _,
            recovers,
            pretties,
            rules,
            ws_pattern,
            debug_rules,
            token_rules,
            host_fns,
        } = parsed;

        let mut maps = Self {
            ws_pattern: ws_pattern.map(|p| p.into_owned()),
            ..Self::default()
        };

        for rec in recovers {
            maps.recover_map
                .insert(rec.rule_name.into_owned(), rec.sync_expr);
        }
        for pretty in pretties {
            maps.pretty_map.insert(
                pretty.rule_name.into_owned(),
                pretty.hints.into_iter().map(|h| h.into_owned()).collect(),
            );
        }
        for name in token_rules {
            maps.token_set.insert(name.into_owned());
        }
        for decl in host_fns {
            maps.host_map.insert(
                decl.name.into_owned(),
                decl.return_type.map(|t| t.into_owned()),
            );
        }
        for name in debug_rules {
            if name.as_ref() == "*" {
                maps.debug_all = true;
            } else {
                maps.debug_set.insert(name.into_owned());
            }
        }

        (rules, maps)
    }

    pub(crate) fn as_directive_set(&self) -> DirectiveSet<'_> {
        DirectiveSet {
            recovers: (!self.recover_map.is_empty()).then_some(&self.recover_map),
            pretties: (!self.pretty_map.is_empty()).then_some(&self.pretty_map),
            ws_pattern: self.ws_pattern.as_deref(),
            token_rules: (!self.token_set.is_empty()).then_some(&self.token_set),
            debug_rules: (!self.debug_set.is_empty()).then_some(&self.debug_set),
            debug_all: self.debug_all,
            host_fns: (!self.host_map.is_empty()).then_some(&self.host_map),
        }
    }
}

pub(crate) struct MergedStaticGrammar {
    pub(crate) ast: AST<'static>,
    pub(crate) directives: DirectiveMaps<'static>,
}

pub(crate) fn load_merged_paths(paths: &[PathBuf]) -> Result<MergedStaticGrammar, CompileError> {
    if paths.is_empty() {
        return Err(CompileError::Import(
            "no grammar paths were provided to the derive pipeline".to_string(),
        ));
    }

    let mut ast = IndexMap::new();
    let mut directives = DirectiveMaps::default();

    for path in paths {
        let registry =
            load_module_graph(path).map_err(|err| CompileError::Import(err.to_string()))?;
        if !registry.errors.is_empty() {
            let msg = registry
                .errors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            return Err(CompileError::Import(msg));
        }

        let entry = path.canonicalize().unwrap_or_else(|_| path.clone());
        let mut module_paths: Vec<PathBuf> = registry.paths().cloned().collect();
        module_paths.sort();

        for module_path in module_paths.iter().filter(|candidate| **candidate != entry) {
            let module = registry.get_module(module_path).ok_or_else(|| {
                CompileError::Import(format!("missing module for `{}`", module_path.display()))
            })?;
            merge_module(module, &mut ast, &mut directives);
        }

        if let Some(module) = registry.get_module(&entry) {
            merge_module(module, &mut ast, &mut directives);
        } else {
            return Err(CompileError::Import(format!(
                "missing entry module for `{}`",
                entry.display()
            )));
        }
    }

    Ok(MergedStaticGrammar { ast, directives })
}

fn merge_module(
    module: &crate::imports::ModuleData,
    ast: &mut AST<'static>,
    directives: &mut DirectiveMaps<'static>,
) {
    for rec in &module.grammar.recovers {
        directives
            .recover_map
            .insert(rec.rule_name.to_string(), rec.sync_expr.clone());
    }

    for pretty in &module.grammar.pretties {
        directives.pretty_map.insert(
            pretty.rule_name.to_string(),
            pretty.hints.iter().map(|hint| hint.to_string()).collect(),
        );
    }

    if let Some(pattern) = &module.grammar.ws_pattern {
        directives.ws_pattern = Some(pattern.to_string());
    }

    for name in &module.grammar.token_rules {
        directives.token_set.insert(name.to_string());
    }

    for decl in &module.grammar.host_fns {
        directives.host_map.insert(
            decl.name.to_string(),
            decl.return_type.as_ref().map(|t| t.to_string()),
        );
    }

    for name in &module.grammar.debug_rules {
        if name.as_ref() == "*" {
            directives.debug_all = true;
        } else {
            directives.debug_set.insert(name.to_string());
        }
    }

    for (name, expr) in &module.grammar.rules {
        ast.insert(name.clone(), expr.clone());
    }
}
