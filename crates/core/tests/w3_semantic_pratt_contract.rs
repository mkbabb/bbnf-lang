use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process::Command;

use bbnf::compiler::{
    NodeKind, PlanOp, PlanRecordSlot, PrattOperatorId, PrattTableId, ProjectionKeep, RuleId,
};
use bbnf::{
    CompileOptions, CompiledModule, DiagnosticCode, GrammarInstanceInput, StageResult,
    compile_instance,
};
use syn::visit::{self, Visit};

const SEMANTIC_SOURCE: &[u8] = br#"
document = keep_right | keep_left | difference_live | trimmed
         | difference_nullable | difference_empty | trimmed_again
         | nullable_trimmed | ordinary_regex;
left = %{ value: "l" };
kept = %{ value: "k" };
right = %{ value: "r" };
keep_right = left >> kept;
keep_left = kept << right;
mixed_keep_right = epsilon >> kept;
mixed_keep_left = kept << epsilon;
mixed_nullable_kept_right = left >> epsilon;
mixed_nullable_kept_left = epsilon << right;
nullable_keep_right = epsilon >> epsilon;
nullable_keep_left = epsilon << epsilon;
difference_live = kept - right;
nullable_include = epsilon;
nonnullable_exclude = "x";
difference_nullable = nullable_include - nonnullable_exclude;
nullable_exclude = epsilon;
difference_empty = kept - nullable_exclude;
trimmed = kept ?w;
trimmed_again = right ?w;
nullable_trimmed = epsilon ?w;
ordinary_regex = /z/;
"#;

const SEMANTIC_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/semantic-laws.bbnf",
  "entry": "document",
  "trivia_regex": "[ \\t]*",
  "pratt_tables": []
}"#;

const PRATT_SOURCE: &[u8] = br#"
document = expression ?w;
expression = atom;
atom = number | group;
group = "(" ?w, expression, ")" ?w;
number = /(?:0|[1-9][0-9]*)/ ?w;
prefix_minus = "-" ?w;
postfix_bang = "!" ?w;
power = "^" ?w;
product = "*" ?w;
sum = "+" ?w;
probe_then_fail = "<", "!" ?w;
long_shift = "<<" ?w;
short_shift = "<" ?w;
prefix_one_baseline = prefix_minus, atom;
prefix_two_baseline = prefix_minus, prefix_minus, atom;
infix_one_baseline = atom, sum, atom;
infix_two_baseline = atom, sum, atom, sum, atom;
"#;

const PRATT_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/pratt-laws.bbnf",
  "entry": "document",
  "trivia_regex": "[ \\t]*",
  "pratt_tables": [
    {
      "site_rule": "expression",
      "atom_rule": "atom",
      "operators": [
        {
          "matcher_rule": "prefix_minus",
          "fixity": "prefix",
          "left_binding_power": null,
          "right_binding_power": 70
        },
        {
          "matcher_rule": "postfix_bang",
          "fixity": "postfix",
          "left_binding_power": 90,
          "right_binding_power": null
        },
        {
          "matcher_rule": "power",
          "fixity": "infix",
          "left_binding_power": 80,
          "right_binding_power": 80
        },
        {
          "matcher_rule": "product",
          "fixity": "infix",
          "left_binding_power": 60,
          "right_binding_power": 61
        },
        {
          "matcher_rule": "sum",
          "fixity": "infix",
          "left_binding_power": 40,
          "right_binding_power": 41
        },
        {
          "matcher_rule": "probe_then_fail",
          "fixity": "infix",
          "left_binding_power": 30,
          "right_binding_power": 31
        },
        {
          "matcher_rule": "long_shift",
          "fixity": "infix",
          "left_binding_power": 20,
          "right_binding_power": 21
        },
        {
          "matcher_rule": "short_shift",
          "fixity": "infix",
          "left_binding_power": 20,
          "right_binding_power": 21
        }
      ]
    }
  ]
}"#;

const MULTI_PRATT_SOURCE: &[u8] = br#"
document = sum_expression, ";", product_expression;
sum_expression = sum_atom;
sum_atom = /(?:0|[1-9][0-9]*)/;
sum_operator = "+";
product_expression = product_atom;
product_atom = /(?:0|[1-9][0-9]*)/;
product_operator = "*";
"#;

const MULTI_PRATT_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/two-site-pratt-laws.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": [
    {
      "site_rule": "product_expression",
      "atom_rule": "product_atom",
      "operators": [{
        "matcher_rule": "product_operator",
        "fixity": "infix",
        "left_binding_power": 20,
        "right_binding_power": 21
      }]
    },
    {
      "site_rule": "sum_expression",
      "atom_rule": "sum_atom",
      "operators": [{
        "matcher_rule": "sum_operator",
        "fixity": "infix",
        "left_binding_power": 10,
        "right_binding_power": 11
      }]
    }
  ]
}"#;

const UNUSED_PRATT_SOURCE: &[u8] = br#"
document = "d";
unused_expression = unused_atom;
unused_atom = "1";
unused_sum = "+";
"#;

const UNUSED_PRATT_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/unused-site-pratt-laws.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": [{
    "site_rule": "unused_expression",
    "atom_rule": "unused_atom",
    "operators": [{
      "matcher_rule": "unused_sum",
      "fixity": "infix",
      "left_binding_power": 10,
      "right_binding_power": 11
    }]
  }]
}"#;

const AFFIX_OVERLAP_SOURCE: &[u8] = br#"
document = expression;
expression = atom;
atom = "1";
prefix_long = "-", "!";
prefix_short = "-";
postfix_long = "!", "?";
postfix_short = "!";
"#;

const AFFIX_OVERLAP_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/affix-overlap-laws.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": [{
    "site_rule": "expression",
    "atom_rule": "atom",
    "operators": [
      {
        "matcher_rule": "prefix_long",
        "fixity": "prefix",
        "left_binding_power": null,
        "right_binding_power": 60
      },
      {
        "matcher_rule": "prefix_short",
        "fixity": "prefix",
        "left_binding_power": null,
        "right_binding_power": 60
      },
      {
        "matcher_rule": "postfix_long",
        "fixity": "postfix",
        "left_binding_power": 70,
        "right_binding_power": null
      },
      {
        "matcher_rule": "postfix_short",
        "fixity": "postfix",
        "left_binding_power": 70,
        "right_binding_power": null
      }
    ]
  }]
}"#;

const PROJECTION_STREAM_SOURCE: &[u8] = br#"
project_right = transformed_regex >> record_literal;
baseline_right = transformed_regex, record_literal;
project_left = transformed_regex << record_literal;
baseline_left = transformed_regex, record_literal;
transformed_regex = @capture(/[a-z]+/);
record_literal = %{ value: "!" };
"#;

fn compiler_event_kind_variants() -> Vec<&'static str> {
    let source = include_str!("../src/compiler/event.rs");
    let body = source
        .split_once("pub enum EventKind {")
        .expect("compiler EventKind declaration")
        .1
        .split_once("\n}")
        .expect("compiler EventKind closing brace")
        .0;
    body.lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("//") && !line.starts_with("#["))
        .map(|line| line.strip_suffix(',').expect("plain EventKind variant"))
        .collect()
}

fn compiler_rust_sources(root: &Path, files: &mut Vec<std::path::PathBuf>) {
    for entry in
        fs::read_dir(root).unwrap_or_else(|error| panic!("read {}: {error}", root.display()))
    {
        let entry = entry.expect("compiler source directory entry");
        let path = entry.path();
        if path.is_dir() {
            compiler_rust_sources(&path, files);
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            files.push(path);
        }
    }
}

fn macro_tokens_invoke_include(tokens: &str) -> bool {
    tokens.match_indices("include").any(|(start, word)| {
        let before = tokens[..start].chars().next_back();
        let after_word = start + word.len();
        let after = tokens[after_word..].chars().next();
        let identifier_boundary = |character: Option<char>| {
            character
                .is_none_or(|character| !(character.is_ascii_alphanumeric() || character == '_'))
        };
        let suffix = &tokens[after_word..];
        identifier_boundary(before)
            && identifier_boundary(after)
            && suffix.chars().next().is_some_and(char::is_whitespace)
            && suffix.trim_start().starts_with('!')
    })
}

fn include_escape_count(file: &syn::File) -> (usize, usize) {
    struct Includes {
        direct: usize,
        nested_tokens: usize,
    }
    impl<'ast> Visit<'ast> for Includes {
        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            if macro_
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "include")
            {
                self.direct += 1;
            } else if macro_tokens_invoke_include(&macro_.tokens.to_string()) {
                self.nested_tokens += 1;
            }
            visit::visit_macro(self, macro_);
        }
    }
    let mut includes = Includes {
        direct: 0,
        nested_tokens: 0,
    };
    includes.visit_file(file);
    (includes.direct, includes.nested_tokens)
}

fn assert_no_include_escape(file: &syn::File, path: &Path) {
    assert_eq!(
        include_escape_count(file),
        (0, 0),
        "active compiler source {} cannot escape its exact module graph through include! at any item, statement, expression, const, block, or macro-token position",
        path.display()
    );
}

fn active_compiler_source_closure(lib: &Path) -> Vec<std::path::PathBuf> {
    fn visit_items(
        items: &[syn::Item],
        module_base: &Path,
        files: &mut BTreeSet<std::path::PathBuf>,
    ) {
        for item in items {
            let syn::Item::Mod(module) = item else {
                continue;
            };
            assert!(
                module
                    .attrs
                    .iter()
                    .all(|attribute| !attribute.path().is_ident("path")),
                "active compiler modules cannot redirect source with #[path]"
            );
            let name = module.ident.to_string();
            if let Some((_, inline)) = &module.content {
                visit_items(inline, &module_base.join(name), files);
                continue;
            }
            let flat = module_base.join(format!("{name}.rs"));
            let nested = module_base.join(&name).join("mod.rs");
            let candidates = [flat, nested]
                .into_iter()
                .filter(|path| path.is_file())
                .collect::<Vec<_>>();
            assert_eq!(
                candidates.len(),
                1,
                "active module `{name}` resolves to exactly one Rust source"
            );
            visit_file(&candidates[0], files);
        }
    }

    fn visit_file(path: &Path, files: &mut BTreeSet<std::path::PathBuf>) {
        let path = path.to_path_buf();
        if !files.insert(path.clone()) {
            return;
        }
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read active module {}: {error}", path.display()));
        let file = syn::parse_file(&source)
            .unwrap_or_else(|error| panic!("parse active module {}: {error}", path.display()));
        assert_no_include_escape(&file, &path);
        let parent = path.parent().expect("active module has a parent");
        let file_name = path.file_name().and_then(|name| name.to_str());
        let module_base = if matches!(file_name, Some("lib.rs" | "mod.rs")) {
            parent.to_path_buf()
        } else {
            parent.join(
                path.file_stem()
                    .and_then(|name| name.to_str())
                    .expect("active module has a UTF-8 stem"),
            )
        };
        visit_items(&file.items, &module_base, files);
    }

    let mut files = BTreeSet::new();
    visit_file(lib, &mut files);
    files.into_iter().collect()
}

fn forbidden_analysis_carriers(source: &str) -> BTreeSet<String> {
    #[derive(Default)]
    struct Surface {
        findings: BTreeSet<String>,
    }
    impl Surface {
        fn inspect_declaration(&mut self, identity: &syn::Ident) {
            let original = identity.to_string();
            let compact = original
                .bytes()
                .filter(u8::is_ascii_alphanumeric)
                .map(|byte| byte.to_ascii_lowercase())
                .collect::<Vec<_>>();
            let compact = std::str::from_utf8(&compact).expect("ASCII compact identifier");
            if compact == "first"
                || compact.contains("firstset")
                || (compact.contains("difference")
                    && (compact.contains("exact") || compact.contains("language")))
            {
                self.findings.insert(original);
            }
        }

        fn inspect_macro_tokens(&mut self, tokens: &str) {
            let bytes = tokens.as_bytes();
            let mut offset = 0;
            while offset < bytes.len() {
                if bytes[offset] == b'"' {
                    offset += 1;
                    while offset < bytes.len() {
                        if bytes[offset] == b'\\' {
                            offset = (offset + 2).min(bytes.len());
                        } else if bytes[offset] == b'"' {
                            offset += 1;
                            break;
                        } else {
                            offset += 1;
                        }
                    }
                    continue;
                }
                if !(bytes[offset].is_ascii_alphanumeric() || bytes[offset] == b'_') {
                    offset += 1;
                    continue;
                }
                let start = offset;
                offset += 1;
                while offset < bytes.len()
                    && (bytes[offset].is_ascii_alphanumeric() || bytes[offset] == b'_')
                {
                    offset += 1;
                }
                let token = &tokens[start..offset];
                let compact = token
                    .bytes()
                    .filter(u8::is_ascii_alphanumeric)
                    .map(|byte| byte.to_ascii_lowercase())
                    .collect::<Vec<_>>();
                let compact = std::str::from_utf8(&compact).expect("ASCII compact macro token");
                let preceding = tokens[..start]
                    .bytes()
                    .rev()
                    .find(|byte| !byte.is_ascii_whitespace());
                let ordinary_first_method = compact == "first" && preceding == Some(b'.');
                if !ordinary_first_method
                    && (compact == "first"
                        || compact.contains("firstset")
                        || (compact.contains("difference")
                            && (compact.contains("exact") || compact.contains("language"))))
                {
                    self.findings.insert(token.to_owned());
                }
            }
        }
    }
    impl<'ast> Visit<'ast> for Surface {
        fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
            fn inspect_use_tree(surface: &mut Surface, tree: &syn::UseTree) {
                match tree {
                    syn::UseTree::Path(path) => {
                        surface.inspect_declaration(&path.ident);
                        inspect_use_tree(surface, &path.tree);
                    }
                    syn::UseTree::Name(name) => surface.inspect_declaration(&name.ident),
                    syn::UseTree::Rename(rename) => {
                        surface.inspect_declaration(&rename.ident);
                        surface.inspect_declaration(&rename.rename);
                    }
                    syn::UseTree::Group(group) => {
                        for item in &group.items {
                            inspect_use_tree(surface, item);
                        }
                    }
                    syn::UseTree::Glob(_) => {}
                }
            }
            inspect_use_tree(self, &item.tree);
            visit::visit_item_use(self, item);
        }

        fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
            for segment in &path.path.segments {
                self.inspect_declaration(&segment.ident);
            }
            visit::visit_type_path(self, path);
        }

        fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_struct(self, item);
        }

        fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_enum(self, item);
        }

        fn visit_item_union(&mut self, item: &'ast syn::ItemUnion) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_union(self, item);
        }

        fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_type(self, item);
        }

        fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_trait(self, item);
        }

        fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_const(self, item);
        }

        fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_static(self, item);
        }

        fn visit_field(&mut self, field: &'ast syn::Field) {
            if let Some(identity) = &field.ident {
                self.inspect_declaration(identity);
            }
            visit::visit_field(self, field);
        }

        fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
            self.inspect_declaration(&item.sig.ident);
            visit::visit_item_fn(self, item);
        }

        fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
            self.inspect_declaration(&item.sig.ident);
            visit::visit_impl_item_fn(self, item);
        }

        fn visit_trait_item_fn(&mut self, item: &'ast syn::TraitItemFn) {
            self.inspect_declaration(&item.sig.ident);
            visit::visit_trait_item_fn(self, item);
        }

        fn visit_impl_item_const(&mut self, item: &'ast syn::ImplItemConst) {
            self.inspect_declaration(&item.ident);
            visit::visit_impl_item_const(self, item);
        }

        fn visit_trait_item_const(&mut self, item: &'ast syn::TraitItemConst) {
            self.inspect_declaration(&item.ident);
            visit::visit_trait_item_const(self, item);
        }

        fn visit_impl_item_type(&mut self, item: &'ast syn::ImplItemType) {
            self.inspect_declaration(&item.ident);
            visit::visit_impl_item_type(self, item);
        }

        fn visit_trait_item_type(&mut self, item: &'ast syn::TraitItemType) {
            self.inspect_declaration(&item.ident);
            visit::visit_trait_item_type(self, item);
        }

        fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
            self.inspect_declaration(&item.ident);
            visit::visit_item_mod(self, item);
        }

        fn visit_variant(&mut self, variant: &'ast syn::Variant) {
            self.inspect_declaration(&variant.ident);
            visit::visit_variant(self, variant);
        }

        fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
            for segment in &macro_.path.segments {
                self.inspect_declaration(&segment.ident);
            }
            self.inspect_macro_tokens(&macro_.tokens.to_string());
            visit::visit_macro(self, macro_);
        }

        fn visit_pat_ident(&mut self, binding: &'ast syn::PatIdent) {
            self.inspect_declaration(&binding.ident);
            visit::visit_pat_ident(self, binding);
        }
    }

    let file = syn::parse_file(source).expect("compiler source is syntactic Rust");
    let mut surface = Surface::default();
    surface.visit_file(&file);
    surface.findings
}

fn compile(
    manifest_bytes: &[u8],
    source_bytes: &[u8],
    options: CompileOptions,
) -> StageResult<CompiledModule> {
    compile_instance(
        GrammarInstanceInput::new(manifest_bytes, source_bytes),
        options,
    )
}

fn accepted(manifest_bytes: &[u8], source_bytes: &[u8]) -> CompiledModule {
    match compile(manifest_bytes, source_bytes, CompileOptions::default()) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(
                diagnostics.is_empty(),
                "accepted W3 fixture produced diagnostics"
            );
            value
        }
        StageResult::Rejected { diagnostics } => panic!(
            "W3 fixture was rejected:\n{}",
            diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        ),
    }
}

fn manifest_for_entry(logical_path: &str, entry: &str, trivia_regex: Option<&str>) -> Vec<u8> {
    serde_json::to_vec(&serde_json::json!({
        "schema": "bbnf.grammar-instance.v1",
        "logical_path": logical_path,
        "entry": entry,
        "trivia_regex": trivia_regex,
        "pratt_tables": []
    }))
    .expect("fixture manifest is JSON-serializable")
}

fn pratt_manifest_for_entry(logical_path: &str, entry: &str) -> Vec<u8> {
    let mut manifest: serde_json::Value =
        serde_json::from_slice(PRATT_MANIFEST).expect("Pratt fixture manifest is JSON");
    manifest["logical_path"] = logical_path.into();
    manifest["entry"] = entry.into();
    serde_json::to_vec(&manifest).expect("entry-mutated Pratt manifest is JSON-serializable")
}

fn one_operator_pratt_manifest(logical_path: &str) -> Vec<u8> {
    serde_json::to_vec(&serde_json::json!({
        "schema": "bbnf.grammar-instance.v1",
        "logical_path": logical_path,
        "entry": "document",
        "trivia_regex": null,
        "pratt_tables": [{
            "site_rule": "expression",
            "atom_rule": "atom",
            "operators": [{
                "matcher_rule": "matcher",
                "fixity": "infix",
                "left_binding_power": 10,
                "right_binding_power": 11
            }]
        }]
    }))
    .expect("single-table fixture manifest is JSON-serializable")
}

fn pratt_manifest_without_failed_probe() -> Vec<u8> {
    let mut manifest: serde_json::Value =
        serde_json::from_slice(PRATT_MANIFEST).expect("Pratt fixture manifest is JSON");
    let operators = manifest["pratt_tables"][0]["operators"]
        .as_array_mut()
        .expect("Pratt fixture operators are an array");
    let removed = operators.remove(5);
    assert_eq!(removed["matcher_rule"], "probe_then_fail");
    manifest["logical_path"] = "contracts/w3/pratt-without-failed-probe.bbnf".into();
    serde_json::to_vec(&manifest).expect("mutated fixture manifest is JSON-serializable")
}

fn mutated_pratt_manifest(
    logical_path: &str,
    mutate: impl FnOnce(&mut Vec<serde_json::Value>),
) -> Vec<u8> {
    let mut manifest: serde_json::Value =
        serde_json::from_slice(PRATT_MANIFEST).expect("Pratt fixture manifest is JSON");
    manifest["logical_path"] = logical_path.into();
    mutate(
        manifest["pratt_tables"][0]["operators"]
            .as_array_mut()
            .expect("Pratt fixture operators are an array"),
    );
    serde_json::to_vec(&manifest).expect("mutated Pratt manifest is JSON-serializable")
}

fn mutated_affix_manifest(
    logical_path: &str,
    mutate: impl FnOnce(&mut Vec<serde_json::Value>),
) -> Vec<u8> {
    let mut manifest: serde_json::Value =
        serde_json::from_slice(AFFIX_OVERLAP_MANIFEST).expect("affix manifest is JSON");
    manifest["logical_path"] = logical_path.into();
    mutate(
        manifest["pratt_tables"][0]["operators"]
            .as_array_mut()
            .expect("affix operators are an array"),
    );
    serde_json::to_vec(&manifest).expect("mutated affix manifest is JSON-serializable")
}

fn assert_rejected_with(manifest_bytes: &[u8], source_bytes: &[u8], expected: DiagnosticCode) {
    let StageResult::Rejected { diagnostics } =
        compile(manifest_bytes, source_bytes, CompileOptions::default())
    else {
        panic!("invalid W3 fixture was accepted");
    };
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code() == expected),
        "expected {expected:?}, got:\n{}",
        diagnostics
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n")
    );
}

fn rule(module: &CompiledModule, name: &str) -> RuleId {
    module
        .program()
        .rules()
        .iter()
        .find(|rule| rule.name() == name)
        .unwrap_or_else(|| panic!("missing fixture rule `{name}`"))
        .id()
}

fn dependency_names(module: &CompiledModule, owner: RuleId) -> BTreeSet<String> {
    module
        .dependency()
        .graph()
        .dependencies(owner)
        .expect("rule belongs to dependency carrier")
        .map(|dependency| {
            module
                .program()
                .rule(dependency)
                .expect("dependency belongs to program")
                .name()
                .to_owned()
        })
        .collect()
}

fn rule_root(module: &CompiledModule, name: &str) -> bbnf::compiler::NodeId {
    module
        .program()
        .rule(rule(module, name))
        .expect("rule belongs to program")
        .root()
}

fn plan_rule_root(module: &CompiledModule, name: &str) -> bbnf::compiler::PlanNodeId {
    module
        .lowering()
        .rules()
        .get(rule(module, name).index())
        .expect("rule belongs to lowering")
        .root()
}

fn generated_descriptor_surface(
    source: &str,
    descriptor: &str,
) -> (BTreeSet<String>, BTreeSet<String>) {
    let file = syn::parse_file(source).expect("generated Rust parses as a syntax tree");
    let mut fields = None;
    let mut public_items = BTreeSet::new();

    for item in &file.items {
        if let syn::Item::Struct(item) = item {
            if item.ident == descriptor {
                assert!(
                    fields.is_none(),
                    "generated descriptor `{descriptor}` is declared once"
                );
                assert!(
                    matches!(item.vis, syn::Visibility::Public(_)),
                    "generated descriptor `{descriptor}` is public"
                );
                let syn::Fields::Named(named) = &item.fields else {
                    panic!("generated descriptor `{descriptor}` must use named fields");
                };
                let names = named
                    .named
                    .iter()
                    .map(|field| {
                        assert!(
                            matches!(field.vis, syn::Visibility::Inherited),
                            "generated descriptor `{descriptor}` fields remain private"
                        );
                        field
                            .ident
                            .as_ref()
                            .expect("named descriptor field")
                            .to_string()
                    })
                    .collect::<BTreeSet<_>>();
                assert_eq!(
                    names.len(),
                    named.named.len(),
                    "generated descriptor `{descriptor}` fields are unique"
                );
                fields = Some(names);
            }
        }

        let syn::Item::Impl(item) = item else {
            continue;
        };
        if item.trait_.is_some() {
            continue;
        }
        let syn::Type::Path(self_type) = item.self_ty.as_ref() else {
            continue;
        };
        if !self_type
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == descriptor)
        {
            continue;
        }
        for impl_item in &item.items {
            match impl_item {
                syn::ImplItem::Fn(method) if matches!(method.vis, syn::Visibility::Public(_)) => {
                    assert!(
                        method.sig.generics.params.is_empty()
                            && method.sig.inputs.len() == 1
                            && matches!(
                                method.sig.inputs.first(),
                                Some(syn::FnArg::Receiver(receiver))
                                    if receiver.reference.is_some()
                                        && receiver.mutability.is_none()
                            ),
                        "public descriptor item `{descriptor}::{}` must be a zero-argument getter",
                        method.sig.ident
                    );
                    let [syn::Stmt::Expr(syn::Expr::Field(field), None)] =
                        method.block.stmts.as_slice()
                    else {
                        panic!(
                            "public descriptor getter `{descriptor}::{}` directly returns its same-named field",
                            method.sig.ident
                        );
                    };
                    assert!(
                        matches!(
                            field.base.as_ref(),
                            syn::Expr::Path(path) if path.path.is_ident("self")
                        ) && matches!(
                            &field.member,
                            syn::Member::Named(member) if member == &method.sig.ident
                        ),
                        "public descriptor getter `{descriptor}::{}` directly returns `self.{}`",
                        method.sig.ident,
                        method.sig.ident
                    );
                    assert!(
                        public_items.insert(method.sig.ident.to_string()),
                        "duplicate public descriptor getter `{descriptor}::{}`",
                        method.sig.ident
                    );
                }
                syn::ImplItem::Const(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                    public_items.insert(format!("const {}", item.ident));
                }
                syn::ImplItem::Type(item) if matches!(item.vis, syn::Visibility::Public(_)) => {
                    public_items.insert(format!("type {}", item.ident));
                }
                _ => {}
            }
        }
    }

    (
        fields.unwrap_or_else(|| panic!("missing generated descriptor `{descriptor}`")),
        public_items,
    )
}

fn path_segments(path: &syn::Path) -> Vec<String> {
    path.segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect()
}

fn integer_literal(expression: &syn::Expr) -> Option<usize> {
    let syn::Expr::Lit(literal) = expression else {
        return None;
    };
    let syn::Lit::Int(integer) = &literal.lit else {
        return None;
    };
    integer.base10_parse().ok()
}

fn some_integer(expression: &syn::Expr) -> Option<usize> {
    let syn::Expr::Call(call) = expression else {
        return None;
    };
    let syn::Expr::Path(function) = call.func.as_ref() else {
        return None;
    };
    (function.path.segments.last()?.ident == "Some" && call.args.len() == 1)
        .then(|| integer_literal(&call.args[0]))
        .flatten()
}

fn expression_definitely_exits(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Return(_) => true,
        syn::Expr::Block(block) => {
            block
                .block
                .stmts
                .last()
                .is_some_and(|statement| match statement {
                    syn::Stmt::Expr(expression, _) => expression_definitely_exits(expression),
                    _ => false,
                })
        }
        _ => false,
    }
}

fn canonical_control_name(name: &str) -> bool {
    name.starts_with("rule_")
        || name.starts_with("node_")
        || name.starts_with("pratt_")
        || matches!(
            name,
            "charge_work"
                | "with_depth"
                | "emit_event"
                | "parse_with_sink"
                | "check"
                | "check_with_limits"
                | "parse_events"
                | "parse_events_with_limits"
        )
}

fn parser_primitive_name(name: &str) -> bool {
    matches!(
        name,
        "sequence" | "choice" | "difference" | "repeat" | "literal" | "eof" | "regex"
    )
}

fn generated_method_name(name: &str) -> bool {
    matches!(
        name,
        "and_then"
            | "atom_rule"
            | "bounded"
            | "child_node"
            | "contains"
            | "copied"
            | "cursor"
            | "emit_with"
            | "end"
            | "enumerate"
            | "events"
            | "expect"
            | "field"
            | "fixity"
            | "get"
            | "id"
            | "input_bytes"
            | "into_parts"
            | "into_stream"
            | "is_empty"
            | "is_none"
            | "is_some"
            | "iter"
            | "kind"
            | "left_binding_power"
            | "len"
            | "logical_events"
            | "map"
            | "matcher_rule"
            | "min"
            | "name"
            | "nesting"
            | "node"
            | "offset"
            | "operator"
            | "operator_len"
            | "operator_start"
            | "owner_rule"
            | "parse_module"
            | "push"
            | "revision"
            | "right_binding_power"
            | "rule"
            | "semantic_type"
            | "set"
            | "skip"
            | "sink"
            | "site_rule"
            | "source"
            | "source_slot"
            | "start"
            | "table"
            | "take"
            | "transform"
            | "truncate"
            | "unwrap_or"
            | "value"
            | "with_input_bytes"
            | "with_logical_events"
            | "with_nesting"
            | "with_work"
            | "work"
    )
}

fn direct_path(expression: &syn::Expr) -> Option<&syn::Path> {
    match expression {
        syn::Expr::Path(path) => Some(&path.path),
        _ => None,
    }
}

fn direct_call<'a>(expression: &'a syn::Expr, expected: &str) -> Option<&'a syn::ExprCall> {
    let syn::Expr::Call(call) = expression else {
        return None;
    };
    let syn::Expr::Path(function) = call.func.as_ref() else {
        return None;
    };
    (function.qself.is_none()
        && function.path.segments.len() == 1
        && function.path.segments[0].ident == expected)
        .then_some(call)
}

fn closure_expression(expression: &syn::Expr) -> Option<&syn::ExprClosure> {
    match expression {
        syn::Expr::Closure(closure) => Some(closure),
        _ => None,
    }
}

fn closure_block(closure: &syn::ExprClosure) -> Option<&syn::Block> {
    match closure.body.as_ref() {
        syn::Expr::Block(block) => Some(&block.block),
        _ => None,
    }
}

fn top_level_functions<'a>(file: &'a syn::File) -> BTreeMap<String, Vec<&'a syn::ItemFn>> {
    let mut functions = BTreeMap::<String, Vec<&syn::ItemFn>>::new();
    for item in &file.items {
        if let syn::Item::Fn(function) = item {
            functions
                .entry(function.sig.ident.to_string())
                .or_default()
                .push(function);
        }
    }
    functions
}

fn unique_function<'a>(
    functions: &'a BTreeMap<String, Vec<&'a syn::ItemFn>>,
    name: &str,
) -> &'a syn::ItemFn {
    let matches = functions
        .get(name)
        .unwrap_or_else(|| panic!("generated v6 missing canonical `{name}`"));
    assert_eq!(
        matches.len(),
        1,
        "generated v6 must define exactly one canonical `{name}`"
    );
    matches[0]
}

fn function_tail(function: &syn::ItemFn) -> &syn::Expr {
    let Some(syn::Stmt::Expr(expression, None)) = function.block.stmts.last() else {
        panic!("{} returns one tail expression", function.sig.ident);
    };
    expression
}

#[derive(Default)]
struct DirectCalls {
    calls: Vec<Vec<String>>,
    paths: BTreeSet<String>,
}

impl<'ast> Visit<'ast> for DirectCalls {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let Some(path) = direct_path(&call.func) {
            self.calls.push(path_segments(path));
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        if let Some(identity) = path.path.segments.last() {
            self.paths.insert(identity.ident.to_string());
        }
        visit::visit_expr_path(self, path);
    }
}

struct CanonicalSurface {
    violations: BTreeSet<&'static str>,
    closure_slot: bool,
    callee_slot: bool,
    top_level_functions: BTreeSet<usize>,
}

impl CanonicalSurface {
    fn for_file(file: &syn::File) -> Self {
        Self {
            violations: BTreeSet::new(),
            closure_slot: false,
            callee_slot: false,
            top_level_functions: file
                .items
                .iter()
                .filter_map(|item| match item {
                    syn::Item::Fn(function) => Some(function as *const syn::ItemFn as usize),
                    _ => None,
                })
                .collect(),
        }
    }

    fn visit_callback_argument(&mut self, expression: &syn::Expr, allowed: bool) {
        let inherited = self.closure_slot;
        self.closure_slot = allowed;
        self.visit_expr(expression);
        self.closure_slot = inherited;
    }
}

impl<'ast> Visit<'ast> for CanonicalSurface {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if !self
            .top_level_functions
            .contains(&(item as *const syn::ItemFn as usize))
        {
            self.violations.insert("non-top-level-function");
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        self.violations.insert("module-control-surface");
        visit::visit_item_mod(self, item);
    }

    fn visit_item_macro(&mut self, item: &'ast syn::ItemMacro) {
        self.violations.insert("macro-definition");
        visit::visit_item_macro(self, item);
    }

    fn visit_macro(&mut self, macro_: &'ast syn::Macro) {
        let path = path_segments(&macro_.path);
        let tokens = macro_.tokens.to_string();
        let canonical = match path.as_slice() {
            [name] if name == "assert" => tokens == "parse_that :: KERNEL_ABI == 3",
            [name] if name == "panic" => tokens == "\"compiler emitted invalid regex table\"",
            _ => false,
        };
        if !canonical {
            self.violations.insert("noncanonical-macro-invocation");
        }
        visit::visit_macro(self, macro_);
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        if item.generics.type_params().any(|parameter| {
            parameter.bounds.iter().any(|bound| {
                matches!(
                    bound,
                    syn::TypeParamBound::Trait(bound)
                        if bound.path.segments.last().is_some_and(|segment| {
                            matches!(
                                segment.ident.to_string().as_str(),
                                "Fn" | "FnMut" | "FnOnce"
                            )
                        })
                )
            })
        }) {
            self.violations.insert("callable-field");
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        self.violations.insert("type-alias");
        visit::visit_item_type(self, item);
    }

    fn visit_item_extern_crate(&mut self, item: &'ast syn::ItemExternCrate) {
        if item.rename.is_some() {
            self.violations.insert("renamed-extern-crate");
        }
        visit::visit_item_extern_crate(self, item);
    }

    fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
        self.violations.insert("trait-dispatch");
        visit::visit_item_trait(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        for member in &item.items {
            if let syn::ImplItem::Fn(method) = member {
                let mut calls = DirectCalls::default();
                calls.visit_block(&method.block);
                if calls.calls.iter().any(|path| {
                    path.last().is_some_and(|name| {
                        canonical_control_name(name) || parser_primitive_name(name)
                    })
                }) {
                    self.violations.insert("parser-control-method");
                }
            }
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_type_bare_fn(&mut self, item: &'ast syn::TypeBareFn) {
        self.violations.insert("function-pointer");
        visit::visit_type_bare_fn(self, item);
    }

    fn visit_type_trait_object(&mut self, item: &'ast syn::TypeTraitObject) {
        if item.bounds.iter().any(|bound| {
            matches!(
                bound,
                syn::TypeParamBound::Trait(bound)
                    if bound.path.segments.last().is_some_and(|segment| {
                        matches!(segment.ident.to_string().as_str(), "Fn" | "FnMut" | "FnOnce")
                    })
            )
        }) {
            self.violations.insert("callable-field");
        }
        visit::visit_type_trait_object(self, item);
    }

    fn visit_item_use(&mut self, item: &'ast syn::ItemUse) {
        fn renamed_control(tree: &syn::UseTree) -> bool {
            match tree {
                syn::UseTree::Rename(rename) => {
                    let original = rename.ident.to_string();
                    canonical_control_name(&original)
                        || matches!(original.as_str(), "sequence" | "difference" | "regex")
                }
                syn::UseTree::Path(path) => renamed_control(&path.tree),
                syn::UseTree::Group(group) => group.items.iter().any(renamed_control),
                _ => false,
            }
        }
        fn contains_rename(tree: &syn::UseTree) -> bool {
            match tree {
                syn::UseTree::Rename(_) => true,
                syn::UseTree::Path(path) => contains_rename(&path.tree),
                syn::UseTree::Group(group) => group.items.iter().any(contains_rename),
                _ => false,
            }
        }
        if renamed_control(&item.tree) {
            self.violations.insert("imported-control-alias");
        }
        if contains_rename(&item.tree) {
            self.violations.insert("renamed-import");
        }
        visit::visit_item_use(self, item);
    }

    fn visit_local(&mut self, local: &'ast syn::Local) {
        if let Some(initializer) = &local.init {
            match initializer.expr.as_ref() {
                syn::Expr::Closure(_) => {
                    self.violations.insert("stored-closure");
                }
                syn::Expr::Path(path)
                    if path.path.segments.last().is_some_and(|segment| {
                        canonical_control_name(&segment.ident.to_string())
                    }) =>
                {
                    self.violations.insert("local-control-alias");
                }
                _ => {}
            }
        }
        visit::visit_local(self, local);
    }

    fn visit_expr_return(&mut self, expression: &'ast syn::ExprReturn) {
        if expression
            .expr
            .as_deref()
            .is_some_and(|expression| matches!(expression, syn::Expr::Closure(_)))
        {
            self.violations.insert("returned-closure");
        }
        visit::visit_expr_return(self, expression);
    }

    fn visit_expr_closure(&mut self, closure: &'ast syn::ExprClosure) {
        if !self.closure_slot {
            self.violations.insert("noncanonical-closure");
        }
        let inherited = self.closure_slot;
        self.closure_slot = false;
        self.visit_expr(&closure.body);
        self.closure_slot = inherited;
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        let direct = direct_path(&call.func);
        let terminal = direct
            .and_then(|path| path.segments.last())
            .map(|segment| segment.ident.to_string());
        if let syn::Expr::Path(function) = call.func.as_ref()
            && (function.qself.is_some() || function.path.segments.len() != 1)
        {
            let segments = path_segments(&function.path);
            let canonical_constructor = function.qself.is_none()
                && matches!(
                segments.as_slice(),
                [owner, method]
                    if matches!(
                        (owner.as_str(), method.as_str()),
                        ("Cell", "new")
                            | ("EventBuffer", "new")
                            | ("Input", "new")
                            | ("Limits", "new")
                            | ("LimitsConfig", "default")
                            | ("NoSink", "new")
                            | ("RegexPlan", "new")
                            | ("RegexTerminal", "new")
                            | ("Run", "with_sink")
                            | ("Step", "Fault" | "Matched" | "Mismatch")
                            | ("Vec", "new")
                            | ("u32", "try_from")
                    )
                );
            if !canonical_constructor {
                self.violations.insert("qualified-function-call");
            }
        }
        let inherited_callee = self.callee_slot;
        self.callee_slot = true;
        self.visit_expr(&call.func);
        self.callee_slot = inherited_callee;
        for (index, argument) in call.args.iter().enumerate() {
            let allowed = match terminal.as_deref() {
                Some("with_depth") => index == 2,
                Some("sequence") => index == 1,
                Some("difference") => matches!(index, 1 | 2),
                Some("choice") => matches!(index, 1 | 2),
                Some("repeat") | Some("emit_event") => index + 1 == call.args.len(),
                _ => false,
            };
            self.visit_callback_argument(argument, allowed);
        }
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        let method = call.method.to_string();
        if canonical_control_name(&method) || !generated_method_name(&method) {
            self.violations.insert("noncanonical-method-call");
        }
        self.visit_expr(&call.receiver);
        for argument in &call.args {
            self.visit_callback_argument(argument, call.method == "parse_module");
        }
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        if !self.callee_slot
            && path.path.segments.last().is_some_and(|segment| {
                let name = segment.ident.to_string();
                canonical_control_name(&name) || parser_primitive_name(&name)
            })
        {
            self.violations.insert("callable-value");
        }
        visit::visit_expr_path(self, path);
    }
}

fn impl_self_type(item: &syn::ItemImpl) -> Option<String> {
    let syn::Type::Path(path) = item.self_ty.as_ref() else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn assert_closed_impl_surface(file: &syn::File) {
    fn names<const N: usize>(values: [&str; N]) -> BTreeSet<String> {
        values.into_iter().map(str::to_owned).collect()
    }

    let inherent = BTreeMap::from([
        (
            "EventBuffer",
            names(["events", "into_stream", "new", "revision", "source"]),
        ),
        (
            "EventStream",
            names(["events", "into_parts", "revision", "source"]),
        ),
        (
            "FieldDescriptor",
            names([
                "child_node",
                "id",
                "name",
                "owner_rule",
                "semantic_type",
                "source_slot",
            ]),
        ),
        ("Limits", names(["new"])),
        (
            "LimitsConfig",
            names([
                "bounded",
                "input_bytes",
                "logical_events",
                "nesting",
                "with_input_bytes",
                "with_logical_events",
                "with_nesting",
                "with_work",
                "work",
            ]),
        ),
        (
            "PrattOperatorDescriptor",
            names([
                "fixity",
                "id",
                "left_binding_power",
                "matcher_rule",
                "right_binding_power",
                "table",
            ]),
        ),
        (
            "PrattTableDescriptor",
            names([
                "atom_rule",
                "id",
                "operator_len",
                "operator_start",
                "site_rule",
            ]),
        ),
        (
            "ScalarEvent",
            names([
                "end",
                "field",
                "kind",
                "node",
                "operator",
                "rule",
                "start",
                "transform",
                "value",
            ]),
        ),
        ("TransformDescriptor", names(["id", "name"])),
    ]);
    let trait_impls = BTreeMap::from([
        (
            ("EventBuffer", "EventSink"),
            names(["len", "push", "truncate"]),
        ),
        (("LimitsConfig", "Default"), names(["default"])),
    ]);
    let mut seen_inherent = BTreeSet::new();
    let mut seen_traits = BTreeSet::new();

    for item in &file.items {
        let syn::Item::Impl(item) = item else {
            continue;
        };
        let owner =
            impl_self_type(item).unwrap_or_else(|| panic!("generated impl uses a named self type"));
        let methods = item
            .items
            .iter()
            .filter_map(|member| match member {
                syn::ImplItem::Fn(method) => Some(method.sig.ident.to_string()),
                _ => None,
            })
            .collect::<BTreeSet<_>>();

        if let Some((_, trait_path, _)) = &item.trait_ {
            let trait_name = trait_path
                .segments
                .last()
                .expect("generated trait impl has a trait identity")
                .ident
                .to_string();
            let key = (owner.as_str(), trait_name.as_str());
            let expected = trait_impls.get(&key).unwrap_or_else(|| {
                panic!("forbidden generated trait impl `{trait_name} for {owner}`")
            });
            assert_eq!(
                &methods, expected,
                "generated trait impl `{trait_name} for {owner}` has its exact method closure"
            );
            let associated_types = item
                .items
                .iter()
                .filter_map(|member| match member {
                    syn::ImplItem::Type(item) => Some(item),
                    _ => None,
                })
                .collect::<Vec<_>>();
            if key == ("EventBuffer", "EventSink") {
                let [event] = associated_types.as_slice() else {
                    panic!("EventBuffer's EventSink impl owns exactly one associated Event type");
                };
                assert!(
                    event.ident == "Event"
                        && matches!(
                            &event.ty,
                            syn::Type::Path(path)
                                if path.qself.is_none()
                                    && path.path.is_ident("ScalarEvent")
                        ),
                    "EventBuffer's EventSink::Event authority is exactly ScalarEvent"
                );
                assert_eq!(
                    item.items.len(),
                    methods.len() + 1,
                    "EventBuffer's EventSink impl contains only its exact methods and Event type"
                );
            } else {
                assert!(
                    associated_types.is_empty() && methods.len() == item.items.len(),
                    "generated trait impl `{trait_name} for {owner}` contains methods only"
                );
            }
            assert!(
                seen_traits.insert((owner, trait_name)),
                "generated trait impl is declared once"
            );
        } else {
            assert_eq!(
                methods.len(),
                item.items.len(),
                "generated inherent impl `{owner}` contains methods only"
            );
            let expected = inherent
                .get(owner.as_str())
                .unwrap_or_else(|| panic!("forbidden generated inherent impl for `{owner}`"));
            assert_eq!(
                &methods, expected,
                "generated inherent impl `{owner}` has its exact method closure"
            );
            assert!(
                seen_inherent.insert(owner),
                "generated inherent impl is declared once"
            );
        }
    }
    assert_eq!(
        seen_inherent,
        inherent
            .keys()
            .map(|owner| (*owner).to_owned())
            .collect::<BTreeSet<_>>(),
        "generated v6 declares every exact inherent implementation once"
    );
    assert_eq!(
        seen_traits,
        trait_impls
            .keys()
            .map(|(owner, trait_)| ((*owner).to_owned(), (*trait_).to_owned()))
            .collect::<BTreeSet<_>>(),
        "generated v6 declares every exact trait implementation once"
    );
}

fn assert_limits_construction(file: &syn::File) {
    fn method<'a>(
        file: &'a syn::File,
        owner: &str,
        trait_: Option<&str>,
        name: &str,
    ) -> &'a syn::ImplItemFn {
        let matches = file
            .items
            .iter()
            .filter_map(|item| {
                let syn::Item::Impl(item) = item else {
                    return None;
                };
                if impl_self_type(item).as_deref() != Some(owner) {
                    return None;
                }
                let actual_trait = item.trait_.as_ref().and_then(|(_, path, _)| {
                    path.segments
                        .last()
                        .map(|segment| segment.ident.to_string())
                });
                if actual_trait.as_deref() != trait_ {
                    return None;
                }
                item.items.iter().find_map(|member| match member {
                    syn::ImplItem::Fn(method) if method.sig.ident == name => Some(method),
                    _ => None,
                })
            })
            .collect::<Vec<_>>();
        let [method] = matches.as_slice() else {
            panic!("one exact `{owner}::{name}` implementation");
        };
        method
    }

    fn direct_struct<'a>(method: &'a syn::ImplItemFn, owner: &str) -> &'a syn::ExprStruct {
        let [syn::Stmt::Expr(syn::Expr::Struct(value), None)] = method.block.stmts.as_slice()
        else {
            panic!("`{owner}` directly returns one closed struct initializer");
        };
        assert!(
            value.path.is_ident("Self") && value.rest.is_none(),
            "`{owner}` constructs only `Self`"
        );
        value
    }

    fn fields<'a>(value: &'a syn::ExprStruct, owner: &str) -> BTreeMap<String, &'a syn::Expr> {
        let fields = value
            .fields
            .iter()
            .map(|field| {
                let syn::Member::Named(name) = &field.member else {
                    panic!("`{owner}` uses named fields");
                };
                (name.to_string(), &field.expr)
            })
            .collect::<BTreeMap<_, _>>();
        assert_eq!(
            fields.len(),
            value.fields.len(),
            "`{owner}` initializes every field once"
        );
        fields
    }

    fn exact_cell_zero(expression: &syn::Expr) -> bool {
        let syn::Expr::Call(call) = expression else {
            return false;
        };
        direct_path(&call.func)
            .is_some_and(|path| path_segments(path).as_slice() == ["Cell", "new"])
            && call.args.len() == 1
            && exact_zero(&call.args[0])
    }

    let default = method(file, "LimitsConfig", Some("Default"), "default");
    assert!(
        default.sig.inputs.is_empty(),
        "LimitsConfig::default has no inputs"
    );
    let default_fields = fields(
        direct_struct(default, "LimitsConfig::default"),
        "LimitsConfig::default",
    );
    assert_eq!(
        default_fields
            .keys()
            .map(String::as_str)
            .collect::<BTreeSet<_>>(),
        BTreeSet::from(["input_bytes", "logical_events", "nesting", "work"])
    );
    for (field, authority) in [
        ("input_bytes", "MAX_INPUT_BYTES"),
        ("logical_events", "MAX_LOGICAL_EVENTS"),
        ("nesting", "MAX_NESTING"),
        ("work", "MAX_WORK"),
    ] {
        assert_eq!(
            path_terminal(default_fields[field]).as_deref(),
            Some(authority),
            "LimitsConfig::default `{field}` retains its exact configured authority"
        );
    }

    let new = method(file, "Limits", None, "new");
    let arguments = new
        .sig
        .inputs
        .iter()
        .map(|argument| match argument {
            syn::FnArg::Typed(argument) => {
                pat_ident(&argument.pat).expect("Limits::new uses a plain config binding")
            }
            syn::FnArg::Receiver(_) => panic!("Limits::new is an associated constructor"),
        })
        .collect::<Vec<_>>();
    assert_eq!(arguments, ["config"], "Limits::new(config)");
    let new_fields = fields(direct_struct(new, "Limits::new"), "Limits::new");
    assert_eq!(
        new_fields
            .keys()
            .map(String::as_str)
            .collect::<BTreeSet<_>>(),
        BTreeSet::from(["config", "depth", "work"])
    );
    assert!(
        exact_path(new_fields["config"], "config"),
        "Limits::new retains its exact bounded configuration"
    );
    assert!(
        exact_cell_zero(new_fields["work"]) && exact_cell_zero(new_fields["depth"]),
        "Limits::new initializes the sole Work and Nesting counters to zero"
    );
}

fn assert_canonical_surface(file: &syn::File) {
    let mut surface = CanonicalSurface::for_file(file);
    surface.visit_file(file);
    assert!(
        surface.violations.is_empty(),
        "generated v6 contains forbidden control indirection: {:?}",
        surface.violations
    );
    assert_closed_impl_surface(file);
    assert_limits_construction(file);
}

fn flatten_use_tree(
    prefix: &mut Vec<String>,
    tree: &syn::UseTree,
    paths: &mut Vec<(Vec<String>, bool)>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            flatten_use_tree(prefix, &path.tree, paths);
            prefix.pop();
        }
        syn::UseTree::Name(name) => {
            let mut path = prefix.clone();
            path.push(name.ident.to_string());
            paths.push((path, false));
        }
        syn::UseTree::Rename(rename) => {
            let mut path = prefix.clone();
            path.push(rename.ident.to_string());
            paths.push((path, true));
        }
        syn::UseTree::Glob(_) => paths.push((prefix.clone(), true)),
        syn::UseTree::Group(group) => {
            for child in &group.items {
                flatten_use_tree(prefix, child, paths);
            }
        }
    }
}

fn assert_parser_primitive_provenance(
    file: &syn::File,
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
) {
    assert!(
        functions.keys().all(|name| !parser_primitive_name(name)),
        "generated v6 cannot shadow a parse-that primitive with a local function"
    );

    let mut imports = Vec::new();
    for item in &file.items {
        if let syn::Item::Use(item) = item {
            flatten_use_tree(&mut Vec::new(), &item.tree, &mut imports);
        }
    }
    let mut calls = DirectCalls::default();
    calls.visit_file(file);
    let called = calls
        .calls
        .iter()
        .filter_map(|path| {
            path.last()
                .filter(|name| parser_primitive_name(name))
                .cloned()
        })
        .collect::<BTreeSet<_>>();
    for primitive in called {
        let owners = imports
            .iter()
            .filter(|(path, renamed)| {
                !renamed && path.as_slice() == ["parse_that", primitive.as_str()]
            })
            .count();
        assert_eq!(
            owners, 1,
            "canonical primitive `{primitive}` has exactly one direct parse-that import"
        );
        assert!(
            imports.iter().all(|(path, renamed)| {
                path.last().map_or(true, |name| {
                    name != &primitive
                        || (!renamed && path.as_slice() == ["parse_that", primitive.as_str()])
                })
            }),
            "canonical primitive `{primitive}` has no foreign or aliased import"
        );
    }
}

fn canonical_surface_violations(source: &str) -> BTreeSet<&'static str> {
    let file = syn::parse_file(source).expect("surface falsifier is syntactic Rust");
    let mut surface = CanonicalSurface::for_file(&file);
    surface.visit_file(&file);
    surface.violations
}

fn pattern_variant(pattern: &syn::Pat) -> Option<String> {
    match pattern {
        syn::Pat::TupleStruct(pattern) => pattern
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string()),
        syn::Pat::Path(pattern) => pattern
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string()),
        _ => None,
    }
}

fn charged_tail(function: &syn::ItemFn) -> &syn::Expr {
    let arguments = function_argument_names(function);
    assert!(
        arguments.len() >= 2,
        "{} has current run and limits arguments",
        function.sig.ident
    );
    let run = &arguments[0];
    let limits = &arguments[1];
    let Some(syn::Stmt::Expr(syn::Expr::Match(guard), _)) = function.block.stmts.first() else {
        panic!(
            "{} must begin with the canonical Work guard",
            function.sig.ident
        );
    };
    let Some(charge) = direct_call(&guard.expr, "charge_work") else {
        panic!(
            "{} Work guard must call charge_work directly",
            function.sig.ident
        );
    };
    assert_exact_call_arguments(
        charge,
        &[run.as_str(), limits.as_str()],
        &format!("{} Work guard", function.sig.ident),
    );
    let variants = guard
        .arms
        .iter()
        .map(|arm| {
            assert!(arm.guard.is_none(), "charge_work arms cannot be guarded");
            pattern_variant(&arm.pat).expect("plain Step arm")
        })
        .collect::<Vec<_>>();
    assert_eq!(
        variants,
        ["Matched", "Mismatch", "Fault"],
        "charge_work uses the exact Step partition"
    );
    assert!(
        !expression_definitely_exits(&guard.arms[0].body)
            && guard.arms[1..]
                .iter()
                .all(|arm| expression_definitely_exits(&arm.body)),
        "only charge_work Matched may continue"
    );
    function_tail(function)
}

fn guarded_transaction_block(function: &syn::ItemFn) -> &syn::Block {
    let arguments = function_argument_names(function);
    let [run, limits, ..] = arguments.as_slice() else {
        panic!(
            "{} has current run and limits arguments",
            function.sig.ident
        );
    };
    let tail = charged_tail(function);
    let Some(depth) = direct_call(tail, "with_depth") else {
        panic!("{} must tail-call with_depth", function.sig.ident);
    };
    assert_eq!(depth.args.len(), 3, "with_depth(run, limits, parser)");
    assert!(
        exact_path(&depth.args[0], run) && exact_path(&depth.args[1], limits),
        "{} enters depth with its exact current run and limits",
        function.sig.ident
    );
    let depth_parser = closure_expression(&depth.args[2]).expect("immediate depth closure");
    let depth_run = closure_input_name(depth_parser, "with_depth");
    let sequence_expression = match depth_parser.body.as_ref() {
        syn::Expr::Call(_) => depth_parser.body.as_ref(),
        syn::Expr::Block(block) => match block.block.stmts.as_slice() {
            [syn::Stmt::Expr(expression, None)] => expression,
            _ => panic!("depth closure contains only the transaction"),
        },
        _ => panic!("depth closure directly returns the transaction"),
    };
    let sequence =
        direct_call(sequence_expression, "sequence").expect("direct parse_that::sequence");
    assert_eq!(sequence.args.len(), 2, "sequence(run, parser)");
    assert!(
        exact_path(&sequence.args[0], &depth_run),
        "{} transacts the exact with_depth callback run",
        function.sig.ident
    );
    let parser = closure_expression(&sequence.args[1]).expect("immediate transaction closure");
    let transaction_run = closure_input_name(parser, "sequence");
    assert_eq!(
        transaction_run, depth_run,
        "{} retains one current run identity through depth and transaction callbacks",
        function.sig.ident
    );
    closure_block(parser).expect("transaction closure uses a block")
}

#[derive(Default)]
struct ResourceShape {
    work: usize,
    nesting: usize,
    gets: usize,
    sets: usize,
    parser_calls: usize,
    matched: usize,
}

impl<'ast> Visit<'ast> for ResourceShape {
    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        match path
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
        {
            Some(name) if name == "Work" => self.work += 1,
            Some(name) if name == "Nesting" => self.nesting += 1,
            Some(name) if name == "Matched" => self.matched += 1,
            _ => {}
        }
        visit::visit_expr_path(self, path);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        match call.method.to_string().as_str() {
            "get" => self.gets += 1,
            "set" => self.sets += 1,
            _ => {}
        }
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if direct_path(&call.func).is_some_and(|path| path.is_ident("parser")) {
            self.parser_calls += 1;
        }
        visit::visit_expr_call(self, call);
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
struct RuntimeResourceAuthority {
    work_gets: usize,
    work_sets: usize,
    depth_gets: usize,
    depth_sets: usize,
    work_kinds: usize,
    nesting_kinds: usize,
    counter_aliases: usize,
    foreign_gets: usize,
    foreign_sets: usize,
}

impl<'ast> Visit<'ast> for RuntimeResourceAuthority {
    fn visit_local(&mut self, local: &'ast syn::Local) {
        fn counter_field(expression: &syn::Expr) -> Option<&str> {
            match expression {
                syn::Expr::Reference(reference) => counter_field(&reference.expr),
                syn::Expr::Paren(paren) => counter_field(&paren.expr),
                syn::Expr::Group(group) => counter_field(&group.expr),
                _ => field_path(expression)
                    .and_then(|path| path.last().cloned())
                    .and_then(|field| {
                        matches!(field.as_str(), "work" | "depth")
                            .then(|| if field == "work" { "work" } else { "depth" })
                    }),
            }
        }
        if local
            .init
            .as_ref()
            .and_then(|initializer| counter_field(&initializer.expr))
            .is_some()
        {
            self.counter_aliases += pattern_binding_names(&local.pat).len().max(1);
        }
        visit::visit_local(self, local);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        let receiver = field_path(&call.receiver);
        match (
            receiver.as_deref(),
            call.method.to_string().as_str(),
            call.args.len(),
        ) {
            (Some([_, field]), "get", 0) if field == "work" => self.work_gets += 1,
            (Some([_, field]), "set", 1) if field == "work" => self.work_sets += 1,
            (Some([_, field]), "get", 0) if field == "depth" => self.depth_gets += 1,
            (Some([_, field]), "set", 1) if field == "depth" => self.depth_sets += 1,
            (_, "get", 0) => self.foreign_gets += 1,
            (_, "set", 1) => self.foreign_sets += 1,
            _ => {}
        }
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        match path.path.segments.last().map(|segment| &segment.ident) {
            Some(identity) if identity == "Work" => self.work_kinds += 1,
            Some(identity) if identity == "Nesting" => self.nesting_kinds += 1,
            _ => {}
        }
        visit::visit_expr_path(self, path);
    }
}

fn runtime_resource_authority(function: &syn::ItemFn) -> RuntimeResourceAuthority {
    let mut authority = RuntimeResourceAuthority::default();
    authority.visit_block(&function.block);
    authority
}

fn pat_ident(pattern: &syn::Pat) -> Option<String> {
    match pattern {
        syn::Pat::Ident(binding) if binding.subpat.is_none() => Some(binding.ident.to_string()),
        _ => None,
    }
}

fn pattern_binding_names(pattern: &syn::Pat) -> Vec<String> {
    struct Bindings(Vec<String>);
    impl<'ast> Visit<'ast> for Bindings {
        fn visit_pat_ident(&mut self, pattern: &'ast syn::PatIdent) {
            self.0.push(pattern.ident.to_string());
            visit::visit_pat_ident(self, pattern);
        }
    }
    let mut bindings = Bindings(Vec::new());
    bindings.visit_pat(pattern);
    bindings.0
}

fn assignment_binding_names(expression: &syn::Expr) -> BTreeSet<String> {
    struct Bindings(BTreeSet<String>);
    impl<'ast> Visit<'ast> for Bindings {
        fn visit_expr_path(&mut self, expression: &'ast syn::ExprPath) {
            if expression.path.segments.len() == 1 {
                self.0.insert(expression.path.segments[0].ident.to_string());
            }
            visit::visit_expr_path(self, expression);
        }
    }
    let mut bindings = Bindings(BTreeSet::new());
    bindings.visit_expr(expression);
    bindings.0
}

fn assignment_operator(operator: &syn::BinOp) -> bool {
    matches!(
        operator,
        syn::BinOp::AddAssign(_)
            | syn::BinOp::SubAssign(_)
            | syn::BinOp::MulAssign(_)
            | syn::BinOp::DivAssign(_)
            | syn::BinOp::RemAssign(_)
            | syn::BinOp::BitXorAssign(_)
            | syn::BinOp::BitAndAssign(_)
            | syn::BinOp::BitOrAssign(_)
            | syn::BinOp::ShlAssign(_)
            | syn::BinOp::ShrAssign(_)
    )
}

fn function_argument_names(function: &syn::ItemFn) -> Vec<String> {
    function
        .sig
        .inputs
        .iter()
        .map(|argument| match argument {
            syn::FnArg::Typed(argument) => {
                pat_ident(&argument.pat).expect("canonical helper uses plain argument bindings")
            }
            syn::FnArg::Receiver(_) => panic!("canonical helper is a free function"),
        })
        .collect()
}

fn expression_path_name(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    (path.path.segments.len() == 1).then(|| path.path.segments[0].ident.to_string())
}

fn field_path(expression: &syn::Expr) -> Option<Vec<String>> {
    match expression {
        syn::Expr::Path(_) => Some(vec![expression_path_name(expression)?]),
        syn::Expr::Field(field) => {
            let mut path = field_path(&field.base)?;
            let syn::Member::Named(member) = &field.member else {
                return None;
            };
            path.push(member.to_string());
            Some(path)
        }
        syn::Expr::Paren(paren) => field_path(&paren.expr),
        syn::Expr::Group(group) => field_path(&group.expr),
        _ => None,
    }
}

fn local_binding(statement: &syn::Stmt) -> Option<(String, &syn::Expr)> {
    let syn::Stmt::Local(local) = statement else {
        return None;
    };
    let initializer = local.init.as_ref()?;
    Some((pat_ident(&local.pat)?, initializer.expr.as_ref()))
}

fn statement_expression(statement: &syn::Stmt) -> Option<&syn::Expr> {
    match statement {
        syn::Stmt::Expr(expression, _) => Some(expression),
        _ => None,
    }
}

fn exact_method_call<'a>(
    expression: &'a syn::Expr,
    receiver: &[&str],
    method: &str,
) -> Option<&'a syn::ExprMethodCall> {
    let syn::Expr::MethodCall(call) = expression else {
        return None;
    };
    (call.method == method
        && field_path(&call.receiver).as_deref()
            == Some(
                &receiver
                    .iter()
                    .map(|value| (*value).to_owned())
                    .collect::<Vec<_>>(),
            ))
    .then_some(call)
}

fn exact_path(expression: &syn::Expr, expected: &str) -> bool {
    expression_path_name(expression).as_deref() == Some(expected)
}

fn assert_exact_call_arguments(call: &syn::ExprCall, expected: &[&str], owner: &str) {
    assert_eq!(
        call.args.len(),
        expected.len(),
        "{owner} receives its exact argument closure"
    );
    for (argument, expected) in call.args.iter().zip(expected) {
        assert!(
            exact_path(argument, expected),
            "{owner} receives the current `{expected}` binding"
        );
    }
}

fn closure_input_name(closure: &syn::ExprClosure, owner: &str) -> String {
    let inputs = closure.inputs.iter().collect::<Vec<_>>();
    let [input] = inputs.as_slice() else {
        panic!("{owner} callback binds exactly one current run");
    };
    pat_ident(input).unwrap_or_else(|| panic!("{owner} callback uses one plain run binding"))
}

fn exact_reference_to(expression: &syn::Expr, expected: &str) -> bool {
    matches!(
        expression,
        syn::Expr::Reference(reference) if exact_path(&reference.expr, expected)
    )
}

fn exact_path_or_reference(expression: &syn::Expr, expected: &str) -> bool {
    exact_path(expression, expected) || exact_reference_to(expression, expected)
}

fn exact_span_method(expression: &syn::Expr, binding: &str, method: &str) -> bool {
    matches!(
        expression,
        syn::Expr::MethodCall(call)
            if call.method == method
                && call.args.is_empty()
                && exact_path(&call.receiver, binding)
    )
}

fn exact_zero(expression: &syn::Expr) -> bool {
    integer_literal(expression) == Some(0)
}

fn exact_unit_matched(expression: &syn::Expr) -> bool {
    let syn::Expr::Call(call) = expression else {
        return false;
    };
    direct_path(&call.func)
        .and_then(|path| path.segments.last())
        .is_some_and(|segment| segment.ident == "Matched")
        && call.args.len() == 1
        && matches!(&call.args[0], syn::Expr::Tuple(tuple) if tuple.elems.is_empty())
}

fn exact_resource_fault_call(
    expression: &syn::Expr,
    run: &str,
    kind: &str,
    limit_path: &[&str],
) -> bool {
    let Some(call) = direct_call(expression, "resource_fault") else {
        return false;
    };
    call.args.len() == 3
        && exact_path(&call.args[0], run)
        && path_terminal(&call.args[1]).as_deref() == Some(kind)
        && field_path(&call.args[2]).as_deref()
            == Some(
                &limit_path
                    .iter()
                    .map(|value| (*value).to_owned())
                    .collect::<Vec<_>>(),
            )
}

fn assert_resource_fault_helper(functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    let function = unique_function(functions, "resource_fault");
    let arguments = function_argument_names(function);
    assert_eq!(arguments.len(), 3, "resource_fault(run, kind, limit)");
    let [run, kind, limit] = arguments.as_slice() else {
        unreachable!();
    };
    let [statement] = function.block.stmts.as_slice() else {
        panic!("resource_fault is one returned expression");
    };
    let expression = statement_expression(statement).expect("resource_fault tail expression");
    let syn::Expr::Call(fault) = expression else {
        panic!("resource_fault returns Step::Fault directly");
    };
    assert_eq!(path_terminal(&fault.func).as_deref(), Some("Fault"));
    assert_eq!(fault.args.len(), 1, "resource_fault returns one fault");
    let syn::Expr::Struct(resource) = &fault.args[0] else {
        panic!("resource_fault constructs one ResourceLimit");
    };
    assert_eq!(
        resource
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .as_deref(),
        Some("ResourceLimit")
    );
    assert!(resource.rest.is_none());
    let fields = resource
        .fields
        .iter()
        .map(|field| {
            let syn::Member::Named(name) = &field.member else {
                panic!("ResourceLimit fields are named");
            };
            (name.to_string(), &field.expr)
        })
        .collect::<BTreeMap<_, _>>();
    assert_eq!(
        fields.keys().map(String::as_str).collect::<BTreeSet<_>>(),
        BTreeSet::from(["at", "kind", "limit"])
    );
    assert!(exact_path(fields["kind"], kind));
    assert!(exact_path(fields["limit"], limit));
    let syn::Expr::MethodCall(cursor) = fields["at"] else {
        panic!("resource fault position is the current run cursor");
    };
    assert!(
        cursor.method == "cursor" && cursor.args.is_empty() && exact_path(&cursor.receiver, run),
        "resource fault position is the current run cursor"
    );
}

fn assert_charge_work_helper(function: &syn::ItemFn) {
    let arguments = function_argument_names(function);
    assert_eq!(arguments.len(), 2, "charge_work(run, limits)");
    let [run, limits] = arguments.as_slice() else {
        unreachable!();
    };
    let [binding, guard, increment, matched] = function.block.stmts.as_slice() else {
        panic!("charge_work has the exact four-statement value path");
    };
    let (work, initial) = local_binding(binding).expect("charge_work reads its counter once");
    let counter = [limits.as_str(), "work"];
    let get = exact_method_call(initial, &counter, "get").expect("one Work counter read");
    assert!(get.args.is_empty());

    let syn::Expr::If(guard) = statement_expression(guard).expect("charge_work limit guard") else {
        panic!("charge_work second statement is its limit guard");
    };
    let syn::Expr::Binary(condition) = guard.cond.as_ref() else {
        panic!("charge_work compares current work with its limit");
    };
    assert!(
        matches!(condition.op, syn::BinOp::Ge(_))
            && exact_path(&condition.left, &work)
            && field_path(&condition.right)
                == Some(vec![limits.clone(), "config".to_owned(), "work".to_owned()]),
        "charge_work compares the same counter with the configured Work limit"
    );
    assert!(guard.else_branch.is_none());
    let [syn::Stmt::Expr(syn::Expr::Return(returned), _)] = guard.then_branch.stmts.as_slice()
    else {
        panic!("charge_work exhausted branch returns immediately");
    };
    assert!(
        returned.expr.as_deref().is_some_and(|expression| {
            exact_resource_fault_call(
                expression,
                run,
                "Work",
                &[limits.as_str(), "config", "work"],
            )
        }),
        "charge_work returns the exact Work fault"
    );

    let increment =
        statement_expression(increment).expect("charge_work increments its one counter");
    let set = exact_method_call(increment, &counter, "set").expect("one Work counter increment");
    assert_eq!(set.args.len(), 1, "Work set receives one value");
    let syn::Expr::Binary(value) = &set.args[0] else {
        panic!("Work increments by one");
    };
    assert!(
        matches!(value.op, syn::BinOp::Add(_))
            && exact_path(&value.left, &work)
            && integer_literal(&value.right) == Some(1),
        "Work increments from the exact value read"
    );
    assert!(
        statement_expression(matched).is_some_and(exact_unit_matched),
        "charge_work returns Step::Matched(()) after the increment"
    );
}

fn assert_with_depth_helper(function: &syn::ItemFn) {
    let arguments = function_argument_names(function);
    assert_eq!(arguments.len(), 3, "with_depth(run, limits, parser)");
    let [run, limits, parser] = arguments.as_slice() else {
        unreachable!();
    };
    let [binding, guard, increment, invocation, restore, outcome] = function.block.stmts.as_slice()
    else {
        panic!("with_depth has the exact six-statement value path");
    };
    let (depth, initial) = local_binding(binding).expect("with_depth reads its counter once");
    let counter = [limits.as_str(), "depth"];
    let get = exact_method_call(initial, &counter, "get").expect("one Nesting counter read");
    assert!(get.args.is_empty());

    let syn::Expr::If(guard) = statement_expression(guard).expect("with_depth limit guard") else {
        panic!("with_depth second statement is its limit guard");
    };
    let syn::Expr::Binary(condition) = guard.cond.as_ref() else {
        panic!("with_depth compares current depth with its limit");
    };
    assert!(
        matches!(condition.op, syn::BinOp::Ge(_))
            && exact_path(&condition.left, &depth)
            && field_path(&condition.right)
                == Some(vec![
                    limits.clone(),
                    "config".to_owned(),
                    "nesting".to_owned(),
                ]),
        "with_depth compares the same counter with the configured Nesting limit"
    );
    assert!(guard.else_branch.is_none());
    let [syn::Stmt::Expr(syn::Expr::Return(returned), _)] = guard.then_branch.stmts.as_slice()
    else {
        panic!("with_depth exhausted branch returns immediately");
    };
    assert!(
        returned.expr.as_deref().is_some_and(|expression| {
            exact_resource_fault_call(
                expression,
                run,
                "Nesting",
                &[limits.as_str(), "config", "nesting"],
            )
        }),
        "with_depth returns the exact Nesting fault"
    );

    let increment = statement_expression(increment).expect("with_depth enters its frame");
    let set = exact_method_call(increment, &counter, "set").expect("one Nesting increment");
    assert_eq!(set.args.len(), 1, "Nesting set receives one value");
    let syn::Expr::Binary(value) = &set.args[0] else {
        panic!("Nesting increments by one");
    };
    assert!(
        matches!(value.op, syn::BinOp::Add(_))
            && exact_path(&value.left, &depth)
            && integer_literal(&value.right) == Some(1),
        "Nesting increments from the exact value read"
    );

    let (result, invocation) =
        local_binding(invocation).expect("with_depth retains the one parser outcome");
    let syn::Expr::Call(invocation) = invocation else {
        panic!("with_depth invokes its immediate parser directly");
    };
    assert!(
        exact_path(&invocation.func, parser)
            && invocation.args.len() == 1
            && exact_path(&invocation.args[0], run),
        "with_depth invokes parser(run) exactly once"
    );

    let restore = statement_expression(restore).expect("with_depth restores its prior depth");
    let restore = exact_method_call(restore, &counter, "set").expect("one Nesting restoration");
    assert_eq!(restore.args.len(), 1, "Nesting restore receives one value");
    assert!(
        exact_path(&restore.args[0], &depth),
        "with_depth restores the exact prior depth on every returned Step"
    );
    assert!(
        statement_expression(outcome).is_some_and(|value| exact_path(value, &result)),
        "with_depth returns the retained parser outcome after restoration"
    );
}

fn assert_emit_event_helper(function: &syn::ItemFn) {
    let arguments = function_argument_names(function);
    assert_eq!(arguments.len(), 3, "emit_event(run, limits, event)");
    let [run, limits, event] = arguments.as_slice() else {
        unreachable!();
    };
    let [binding, guard, emission, matched] = function.block.stmts.as_slice() else {
        panic!("emit_event has the exact four-statement value path");
    };
    let (logical_events, length) =
        local_binding(binding).expect("emit_event reads its logical-event count once");
    let syn::Expr::MethodCall(length) = length else {
        panic!("emit_event reads the sink length");
    };
    assert!(length.method == "len" && length.args.is_empty());
    let syn::Expr::MethodCall(sink) = length.receiver.as_ref() else {
        panic!("emit_event reads run.sink().len()");
    };
    assert!(
        sink.method == "sink" && sink.args.is_empty() && exact_path(&sink.receiver, run),
        "emit_event reads the current run's sink"
    );

    let syn::Expr::If(guard) = statement_expression(guard).expect("emit_event limit guard") else {
        panic!("emit_event second statement is its limit guard");
    };
    let syn::Expr::Binary(condition) = guard.cond.as_ref() else {
        panic!("emit_event compares logical events with its limit");
    };
    assert!(
        matches!(condition.op, syn::BinOp::Ge(_))
            && exact_path(&condition.left, &logical_events)
            && field_path(&condition.right)
                == Some(vec![
                    limits.clone(),
                    "config".to_owned(),
                    "logical_events".to_owned(),
                ]),
        "emit_event compares the same logical-event count with the configured Events limit"
    );
    assert!(guard.else_branch.is_none());
    let [syn::Stmt::Expr(syn::Expr::Return(returned), _)] = guard.then_branch.stmts.as_slice()
    else {
        panic!("emit_event exhausted branch returns immediately");
    };
    assert!(
        returned.expr.as_deref().is_some_and(|expression| {
            exact_resource_fault_call(
                expression,
                run,
                "Events",
                &[limits.as_str(), "config", "logical_events"],
            )
        }),
        "emit_event returns the exact current-cursor Events fault"
    );

    let emission = statement_expression(emission).expect("emit_event publishes once");
    let emission =
        exact_method_call(emission, &[run.as_str()], "emit_with").expect("run.emit_with(event)");
    assert_eq!(emission.args.len(), 1);
    assert!(
        exact_path(&emission.args[0], event),
        "emit_event publishes only its immediate event constructor"
    );
    assert!(
        statement_expression(matched).is_some_and(exact_unit_matched),
        "emit_event returns Step::Matched(()) after publishing"
    );
}

fn assert_canonical_resource_helpers(functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    assert_resource_fault_helper(functions);
    let charge = unique_function(functions, "charge_work");
    let mut charge_shape = ResourceShape::default();
    charge_shape.visit_block(&charge.block);
    assert_eq!(
        (
            charge_shape.work,
            charge_shape.nesting,
            charge_shape.gets,
            charge_shape.sets,
            charge_shape.parser_calls,
        ),
        (1, 0, 1, 1, 0),
        "canonical charge_work authority"
    );
    assert_eq!(charge_shape.matched, 1, "charge_work success outcome");
    assert_charge_work_helper(charge);

    let depth = unique_function(functions, "with_depth");
    let mut depth_shape = ResourceShape::default();
    depth_shape.visit_block(&depth.block);
    assert_eq!(
        (
            depth_shape.work,
            depth_shape.nesting,
            depth_shape.gets,
            depth_shape.sets,
            depth_shape.parser_calls,
        ),
        (0, 1, 1, 2, 1),
        "canonical with_depth authority"
    );
    assert_with_depth_helper(depth);

    let emit = unique_function(functions, "emit_event");
    assert_emit_event_helper(emit);
}

fn direct_internal_calls(block: &syn::Block) -> Vec<String> {
    let mut calls = DirectCalls::default();
    calls.visit_block(block);
    calls
        .calls
        .into_iter()
        .filter_map(|path| {
            (path.len() == 1 && canonical_control_name(&path[0])).then(|| path[0].clone())
        })
        .collect()
}

fn call_count(block: &syn::Block, name: &str) -> usize {
    let mut calls = DirectCalls::default();
    calls.visit_block(block);
    calls
        .calls
        .iter()
        .filter(|path| path.as_slice() == [name])
        .count()
}

fn parser_primitive_counts(block: &syn::Block) -> BTreeMap<String, usize> {
    let mut calls = DirectCalls::default();
    calls.visit_block(block);
    let mut counts = BTreeMap::new();
    for name in calls.calls.into_iter().filter_map(|path| {
        path.last()
            .filter(|name| parser_primitive_name(name))
            .cloned()
    }) {
        *counts.entry(name).or_insert(0) += 1;
    }
    counts
}

fn assert_parser_primitive_counts(
    function: &syn::ItemFn,
    expected: impl IntoIterator<Item = (&'static str, usize)>,
) {
    assert_eq!(
        parser_primitive_counts(&function.block),
        expected
            .into_iter()
            .map(|(name, count)| (name.to_owned(), count))
            .collect::<BTreeMap<_, _>>(),
        "{} has only its canonical parse-that primitives",
        function.sig.ident
    );
}

fn option_integer(expression: &syn::Expr) -> Option<Option<usize>> {
    if let Some(value) = some_integer(expression) {
        return Some(Some(value));
    }
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    path.path.is_ident("None").then_some(None)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EventOperator {
    None,
    Literal(usize),
    DescriptorId,
}

impl EventOperator {
    const fn is_some(self) -> bool {
        !matches!(self, Self::None)
    }
}

fn event_operator(expression: &syn::Expr) -> Option<EventOperator> {
    if matches!(expression, syn::Expr::Path(path) if path.path.is_ident("None")) {
        return Some(EventOperator::None);
    }
    let syn::Expr::Call(some) = expression else {
        return None;
    };
    if !direct_path(&some.func).is_some_and(|path| path.is_ident("Some")) || some.args.len() != 1 {
        return None;
    }
    if let Some(value) = integer_literal(&some.args[0]) {
        return Some(EventOperator::Literal(value));
    }
    matches!(
        &some.args[0],
        syn::Expr::MethodCall(id) if id.method == "id" && id.args.is_empty()
    )
    .then_some(EventOperator::DescriptorId)
}

fn event_identity(call: &syn::ExprCall) -> Option<(String, [Option<usize>; 4], EventOperator)> {
    if call.args.len() != 6 {
        return None;
    }
    let variant = call.args.first()?;
    let syn::Expr::Path(variant) = variant else {
        return None;
    };
    let variant = variant.path.segments.last()?.ident.to_string();
    let syn::Expr::Call(identity) = call.args.get(1)? else {
        return None;
    };
    let identity_path = direct_path(&identity.func)?;
    if !identity_path.is_ident("identity") || identity.args.len() != 4 {
        return None;
    }
    Some((
        variant,
        [
            option_integer(&identity.args[0])?,
            option_integer(&identity.args[1])?,
            option_integer(&identity.args[2])?,
            option_integer(&identity.args[3])?,
        ],
        event_operator(&call.args[2])?,
    ))
}

#[derive(Default)]
struct EventIdentities {
    rows: Vec<(String, [Option<usize>; 4], EventOperator)>,
}

impl<'ast> Visit<'ast> for EventIdentities {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if direct_path(&call.func).is_some_and(|path| path.is_ident("event"))
            && let Some(row) = event_identity(call)
        {
            self.rows.push(row);
        }
        visit::visit_expr_call(self, call);
    }
}

fn event_calls_named<'a>(block: &'a syn::Block, variant: &'a str) -> Vec<&'a syn::ExprCall> {
    struct Events<'a> {
        variant: &'a str,
        calls: Vec<&'a syn::ExprCall>,
    }
    impl<'a, 'ast: 'a> Visit<'ast> for Events<'a> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if direct_path(&call.func).is_some_and(|path| path.is_ident("event"))
                && event_identity(call).is_some_and(|(variant, _, _)| variant == self.variant)
            {
                self.calls.push(call);
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut events = Events {
        variant,
        calls: Vec::new(),
    };
    events.visit_block(block);
    events.calls
}

fn assert_identity_helper(functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    let function = unique_function(functions, "identity");
    let arguments = function_argument_names(function);
    assert_eq!(
        arguments,
        ["rule", "node", "field", "transform"],
        "identity accepts the exact scalar-event identity fields in schema order"
    );
    let [statement] = function.block.stmts.as_slice() else {
        panic!("identity has one returned EventIdentity constructor");
    };
    let syn::Expr::Struct(identity) =
        statement_expression(statement).expect("identity tail expression")
    else {
        panic!("identity directly constructs EventIdentity");
    };
    assert!(
        identity
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "EventIdentity")
            && identity.rest.is_none(),
        "identity constructs one closed EventIdentity"
    );
    let fields = identity
        .fields
        .iter()
        .map(|field| {
            let syn::Member::Named(name) = &field.member else {
                panic!("EventIdentity fields are named");
            };
            (name.to_string(), &field.expr)
        })
        .collect::<BTreeMap<_, _>>();
    assert_eq!(
        fields.keys().map(String::as_str).collect::<BTreeSet<_>>(),
        BTreeSet::from(["field", "node", "rule", "transform"]),
        "identity initializes the exact EventIdentity field closure"
    );
    for argument in arguments {
        assert!(
            exact_path(fields[argument.as_str()], &argument),
            "identity `{argument}` comes from its exact same-named argument"
        );
    }
}

fn assert_event_helper(functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    let function = unique_function(functions, "event");
    let arguments = function_argument_names(function);
    assert_eq!(
        arguments.len(),
        6,
        "event(kind, identity, operator, start, end, value)"
    );
    let [kind, identity, operator, start, end, value] = arguments.as_slice() else {
        unreachable!();
    };
    let [statement] = function.block.stmts.as_slice() else {
        panic!("event has one returned ScalarEvent constructor");
    };
    let expression = statement_expression(statement).expect("event tail expression");
    let syn::Expr::Struct(event) = expression else {
        panic!("event directly constructs ScalarEvent");
    };
    assert_eq!(
        event
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .as_deref(),
        Some("ScalarEvent")
    );
    assert!(event.rest.is_none());
    let fields = event
        .fields
        .iter()
        .map(|field| {
            let syn::Member::Named(name) = &field.member else {
                panic!("ScalarEvent fields are named");
            };
            (name.to_string(), &field.expr)
        })
        .collect::<BTreeMap<_, _>>();
    assert_eq!(
        fields.keys().map(String::as_str).collect::<BTreeSet<_>>(),
        BTreeSet::from([
            "end",
            "field",
            "kind",
            "node",
            "operator",
            "rule",
            "start",
            "transform",
            "value",
        ]),
        "event initializes the exact scalar-events-v5 field closure"
    );
    assert!(exact_path(fields["kind"], kind));
    assert!(exact_path(fields["operator"], operator));
    assert!(exact_path(fields["value"], value));
    for member in ["rule", "node", "field", "transform"] {
        assert_eq!(
            field_path(fields[member]),
            Some(vec![identity.clone(), member.to_owned()]),
            "event `{member}` comes from its exact EventIdentity field"
        );
    }

    fn exact_u32_conversion(expression: &syn::Expr, source: &str) -> bool {
        let syn::Expr::MethodCall(expect) = expression else {
            return false;
        };
        if expect.method != "expect" || expect.args.len() != 1 {
            return false;
        }
        let syn::Expr::Call(conversion) = expect.receiver.as_ref() else {
            return false;
        };
        direct_path(&conversion.func)
            .is_some_and(|path| path_segments(path).as_slice() == ["u32", "try_from"])
            && conversion.args.len() == 1
            && exact_path(&conversion.args[0], source)
    }
    assert!(
        exact_u32_conversion(fields["start"], start) && exact_u32_conversion(fields["end"], end),
        "event converts its exact source offsets after the public input bound"
    );
}

fn assert_event_emission_authority(functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    #[derive(Default)]
    struct Emissions {
        in_immediate_callback: bool,
        emit_calls: usize,
        event_calls: usize,
        invalid: BTreeSet<&'static str>,
    }
    impl<'ast> Visit<'ast> for Emissions {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if direct_path(&call.func).is_some_and(|path| path.is_ident("emit_event")) {
                self.emit_calls += 1;
                if call.args.len() != 3 {
                    self.invalid.insert("emit-event-arity");
                    visit::visit_expr_call(self, call);
                    return;
                }
                if !exact_path(&call.args[0], "run")
                    || !exact_path_or_reference(&call.args[1], "limits")
                {
                    self.invalid.insert("stale-event-context");
                }
                let Some(callback) = closure_expression(&call.args[2]) else {
                    self.invalid.insert("non-immediate-event-callback");
                    visit::visit_expr_call(self, call);
                    return;
                };
                if closure_direct_call(callback, "event").args.len() != 6 {
                    self.invalid.insert("event-arity");
                }
                self.visit_expr(&call.func);
                self.visit_expr(&call.args[0]);
                self.visit_expr(&call.args[1]);
                let inherited = self.in_immediate_callback;
                self.in_immediate_callback = true;
                self.visit_expr(&call.args[2]);
                self.in_immediate_callback = inherited;
                return;
            }
            if direct_path(&call.func).is_some_and(|path| path.is_ident("event")) {
                self.event_calls += 1;
                if !self.in_immediate_callback || event_identity(call).is_none() {
                    self.invalid.insert("unbound-event-constructor");
                }
            }
            visit::visit_expr_call(self, call);
        }
    }

    struct ScalarConstructors(usize);
    impl<'ast> Visit<'ast> for ScalarConstructors {
        fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
            self.0 += usize::from(
                expression
                    .path
                    .segments
                    .last()
                    .is_some_and(|segment| segment.ident == "ScalarEvent"),
            );
            visit::visit_expr_struct(self, expression);
        }
    }

    assert_identity_helper(functions);
    assert_event_helper(functions);
    for (name, definitions) in functions {
        let function = definitions[0];
        #[derive(Default)]
        struct ContextRebindings {
            allow_root_limits: bool,
            root_limits: usize,
            violations: BTreeSet<&'static str>,
        }
        impl ContextRebindings {
            fn inspect_binding_names(
                &mut self,
                names: impl IntoIterator<Item = String>,
                allow_current_run: bool,
                allow_root_limits: bool,
            ) {
                for name in names {
                    match name.as_str() {
                        "run" if !allow_current_run => {
                            self.violations.insert("run-pattern-rebinding");
                        }
                        "limits" if !allow_root_limits => {
                            self.violations.insert("limits-pattern-rebinding");
                        }
                        _ => {}
                    }
                }
            }

            fn visit_callback(&mut self, closure: &syn::ExprClosure, current_run: bool) {
                if current_run {
                    let inputs = closure.inputs.iter().collect::<Vec<_>>();
                    let [input] = inputs.as_slice() else {
                        self.violations.insert("current-run-callback-arity");
                        self.visit_expr(&closure.body);
                        return;
                    };
                    let canonical = matches!(
                        input,
                        syn::Pat::Ident(binding)
                            if binding.ident == "run"
                                && binding.by_ref.is_none()
                                && binding.mutability.is_none()
                                && binding.subpat.is_none()
                    );
                    if !canonical {
                        self.violations.insert("current-run-callback-pattern");
                        self.inspect_binding_names(pattern_binding_names(input), false, false);
                    }
                } else {
                    if !closure.inputs.is_empty() {
                        self.violations.insert("zero-input-callback-pattern");
                    }
                    for input in &closure.inputs {
                        self.inspect_binding_names(pattern_binding_names(input), false, false);
                    }
                }
                self.visit_expr(&closure.body);
            }

            fn inspect_assignment(&mut self, left: &syn::Expr) {
                let names = assignment_binding_names(left);
                if names.contains("run") {
                    self.violations.insert("run-assignment");
                }
                if names.contains("limits") {
                    self.violations.insert("limits-assignment");
                }
            }
        }
        impl<'ast> Visit<'ast> for ContextRebindings {
            fn visit_local(&mut self, local: &'ast syn::Local) {
                let names = pattern_binding_names(&local.pat);
                let canonical_root_limits = names.as_slice() == ["limits"]
                    && pat_ident(&local.pat).as_deref() == Some("limits")
                    && self.allow_root_limits
                    && local.init.as_ref().is_some_and(|initializer| {
                        let syn::Expr::Call(call) = initializer.expr.as_ref() else {
                            return false;
                        };
                        direct_path(&call.func)
                            .is_some_and(|path| path_segments(path).as_slice() == ["Limits", "new"])
                            && call.args.len() == 1
                            && exact_path(&call.args[0], "config")
                    });
                if canonical_root_limits {
                    self.root_limits += 1;
                    if self.root_limits != 1 {
                        self.violations.insert("duplicate-root-limits");
                    }
                }
                self.inspect_binding_names(names, false, canonical_root_limits);
                if let Some(initializer) = &local.init {
                    self.visit_expr(&initializer.expr);
                    if let Some((_, diverge)) = &initializer.diverge {
                        self.visit_expr(diverge);
                    }
                }
            }

            fn visit_pat_ident(&mut self, pattern: &'ast syn::PatIdent) {
                self.inspect_binding_names([pattern.ident.to_string()], false, false);
                visit::visit_pat_ident(self, pattern);
            }

            fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
                let terminal = direct_path(&call.func)
                    .and_then(|path| path.segments.last())
                    .map(|segment| segment.ident.to_string());
                self.visit_expr(&call.func);
                for (index, argument) in call.args.iter().enumerate() {
                    let callback = match terminal.as_deref() {
                        Some("with_depth") => (index == 2).then_some(true),
                        Some("sequence") => (index == 1).then_some(true),
                        Some("difference") | Some("choice") => {
                            matches!(index, 1 | 2).then_some(true)
                        }
                        Some("repeat") => (index + 1 == call.args.len()).then_some(true),
                        Some("emit_event") => (index + 1 == call.args.len()).then_some(false),
                        _ => None,
                    };
                    if let Some(current_run) = callback
                        && let Some(closure) = closure_expression(argument)
                    {
                        self.visit_callback(closure, current_run);
                    } else {
                        self.visit_expr(argument);
                    }
                }
            }

            fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
                self.visit_expr(&call.receiver);
                for argument in &call.args {
                    if call.method == "parse_module"
                        && let Some(closure) = closure_expression(argument)
                    {
                        self.visit_callback(closure, true);
                    } else {
                        self.visit_expr(argument);
                    }
                }
            }

            fn visit_expr_closure(&mut self, closure: &'ast syn::ExprClosure) {
                for input in &closure.inputs {
                    self.inspect_binding_names(pattern_binding_names(input), false, false);
                }
                self.visit_expr(&closure.body);
            }

            fn visit_expr_assign(&mut self, assignment: &'ast syn::ExprAssign) {
                self.inspect_assignment(&assignment.left);
                visit::visit_expr_assign(self, assignment);
            }

            fn visit_expr_binary(&mut self, expression: &'ast syn::ExprBinary) {
                if assignment_operator(&expression.op) {
                    self.inspect_assignment(&expression.left);
                }
                visit::visit_expr_binary(self, expression);
            }
        }
        let mut rebindings = ContextRebindings {
            allow_root_limits: name == "parse_with_sink",
            root_limits: 0,
            violations: BTreeSet::new(),
        };
        rebindings.visit_block(&function.block);
        if rebindings.allow_root_limits && rebindings.root_limits != 1 {
            rebindings.violations.insert("missing-root-limits");
        }
        assert!(
            rebindings.violations.is_empty(),
            "`{name}` cannot shadow or assign its current run/limits context: {:?}",
            rebindings.violations
        );
        let mut emissions = Emissions::default();
        emissions.visit_block(&function.block);
        assert!(
            emissions.invalid.is_empty(),
            "`{name}` violates the immediate emit_event/event authority: {:?}",
            emissions.invalid
        );
        assert_eq!(
            emissions.emit_calls, emissions.event_calls,
            "`{name}` has a bijection between emit_event calls and six-argument event callbacks"
        );
        let emit_with = method_calls_named(&function.block, "emit_with").len();
        assert_eq!(
            emit_with,
            usize::from(name == "emit_event"),
            "run.emit_with is owned only by the exact emit_event helper"
        );
        assert!(
            name == "emit_event" || method_calls_named(&function.block, "push").is_empty(),
            "`{name}` cannot bypass emit_event through a direct sink push"
        );
        let mut constructors = ScalarConstructors(0);
        constructors.visit_block(&function.block);
        assert_eq!(
            constructors.0,
            usize::from(name == "event"),
            "ScalarEvent is constructed only by the exact event helper"
        );
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum DescriptorIndex {
    Literal(usize),
    Binding(String),
}

fn descriptor_index(expression: &syn::Expr) -> Option<DescriptorIndex> {
    match expression {
        syn::Expr::Lit(_) => integer_literal(expression).map(DescriptorIndex::Literal),
        syn::Expr::Path(_) => expression_path_name(expression).map(DescriptorIndex::Binding),
        syn::Expr::MethodCall(call)
            if matches!(call.method.to_string().as_str(), "expect" | "unwrap_or")
                && call.args.len() == 1 =>
        {
            descriptor_index(&call.receiver)
        }
        syn::Expr::Paren(paren) => descriptor_index(&paren.expr),
        syn::Expr::Group(group) => descriptor_index(&group.expr),
        _ => None,
    }
}

fn indexed_pratt_operator(expression: &syn::Expr) -> Option<DescriptorIndex> {
    match expression {
        syn::Expr::Index(index)
            if matches!(
                index.expr.as_ref(),
                syn::Expr::Path(path) if path.path.is_ident("PRATT_OPERATORS")
            ) =>
        {
            descriptor_index(&index.index)
        }
        syn::Expr::Reference(reference) => indexed_pratt_operator(&reference.expr),
        syn::Expr::Paren(paren) => indexed_pratt_operator(&paren.expr),
        syn::Expr::Group(group) => indexed_pratt_operator(&group.expr),
        _ => None,
    }
}

fn pratt_descriptor_bindings(block: &syn::Block) -> BTreeMap<String, DescriptorIndex> {
    fn operator_index(expression: &syn::Expr) -> Option<DescriptorIndex> {
        match expression {
            syn::Expr::Index(_)
            | syn::Expr::Reference(_)
            | syn::Expr::Paren(_)
            | syn::Expr::Group(_) => indexed_pratt_operator(expression),
            _ => None,
        }
    }

    struct Bindings(BTreeMap<String, DescriptorIndex>);
    impl<'ast> Visit<'ast> for Bindings {
        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(index) = local
                .init
                .as_ref()
                .and_then(|initializer| operator_index(&initializer.expr))
                && let Some(binding) = pat_ident(&local.pat)
            {
                assert!(
                    self.0.insert(binding.clone(), index).is_none(),
                    "descriptor binding `{binding}` has one lexical authority"
                );
            }
            visit::visit_local(self, local);
        }
    }
    let mut bindings = Bindings(BTreeMap::new());
    bindings.visit_block(block);
    bindings.0
}

fn assert_pratt_event_operator_authority(block: &syn::Block) -> DescriptorIndex {
    let events = event_calls_named(block, "PrattReduce");
    assert_eq!(events.len(), 1, "one canonical PrattReduce constructor");
    let event = events[0];
    let syn::Expr::Call(some) = &event.args[2] else {
        panic!("PrattReduce operator is Some(descriptor.id())");
    };
    assert!(
        direct_path(&some.func).is_some_and(|path| path.is_ident("Some")) && some.args.len() == 1,
        "PrattReduce operator is Some(descriptor.id())"
    );
    let syn::Expr::MethodCall(id) = &some.args[0] else {
        panic!("PrattReduce operator comes from a descriptor getter");
    };
    assert!(id.method == "id" && id.args.is_empty());
    let bindings = pratt_descriptor_bindings(block);
    indexed_pratt_operator(&id.receiver)
        .or_else(|| {
            expression_path_name(&id.receiver).and_then(|binding| bindings.get(&binding).cloned())
        })
        .expect("PrattReduce operator derives from the exact exported PRATT_OPERATORS row")
}

fn assert_work_guard_and_tail_only(function: &syn::ItemFn) -> &syn::Expr {
    assert_eq!(
        function.block.stmts.len(),
        2,
        "{} contains only its Work guard and canonical returned dataflow",
        function.sig.ident
    );
    charged_tail(function)
}

fn event_rows(function: &syn::ItemFn) -> Vec<(String, [Option<usize>; 4], EventOperator)> {
    let mut events = EventIdentities::default();
    events.visit_block(&function.block);
    events.rows
}

fn event_rows_expression(
    expression: &syn::Expr,
) -> Vec<(String, [Option<usize>; 4], EventOperator)> {
    let mut events = EventIdentities::default();
    events.visit_expr(expression);
    events.rows
}

fn assert_event_rows(
    function: &syn::ItemFn,
    expected: impl IntoIterator<Item = (&'static str, [Option<usize>; 4], EventOperator)>,
) {
    assert_eq!(
        event_rows(function),
        expected
            .into_iter()
            .map(|(variant, identity, operator)| (variant.to_owned(), identity, operator))
            .collect::<Vec<_>>(),
        "{} has its exact event constructor closure",
        function.sig.ident
    );
}

#[derive(Debug, Eq, PartialEq)]
enum NodeExecution {
    Child(String),
    Event(String, [Option<usize>; 4], EventOperator),
}

fn node_execution_trace(block: &syn::Block) -> Vec<NodeExecution> {
    struct Trace(Vec<NodeExecution>);
    impl<'ast> Visit<'ast> for Trace {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let Some(path) = direct_path(&call.func)
                && path.segments.len() == 1
            {
                let name = path.segments[0].ident.to_string();
                if name.starts_with("node_") {
                    self.0.push(NodeExecution::Child(name));
                } else if name == "event"
                    && let Some((variant, identity, operator)) = event_identity(call)
                {
                    self.0
                        .push(NodeExecution::Event(variant, identity, operator));
                }
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut trace = Trace(Vec::new());
    trace.visit_block(block);
    trace.0
}

fn child_match_calls(block: &syn::Block) -> Vec<String> {
    struct ChildMatches(Vec<String>);
    impl<'ast> Visit<'ast> for ChildMatches {
        fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
            if let syn::Expr::Call(call) = expression.expr.as_ref()
                && let Some(path) = direct_path(&call.func)
                && path.segments.len() == 1
            {
                let name = path.segments[0].ident.to_string();
                if name.starts_with("node_") {
                    assert_exact_call_arguments(call, &["run", "limits"], &name);
                    assert_eq!(
                        expression
                            .arms
                            .iter()
                            .map(|arm| {
                                assert!(arm.guard.is_none(), "child Step arms are unguarded");
                                pattern_variant(&arm.pat).expect("plain child Step arm")
                            })
                            .collect::<Vec<_>>(),
                        ["Matched", "Mismatch", "Fault"],
                        "{name} uses the exact Step partition"
                    );
                    assert!(
                        !expression_definitely_exits(&expression.arms[0].body)
                            && expression.arms[1..]
                                .iter()
                                .all(|arm| expression_definitely_exits(&arm.body)),
                        "{name} continues only after Matched"
                    );
                    self.0.push(name);
                }
            }
            visit::visit_expr_match(self, expression);
        }
    }
    let mut calls = ChildMatches(Vec::new());
    calls.visit_block(block);
    calls.0
}

fn sequence_transaction(function: &syn::ItemFn) -> &syn::Block {
    let tail = assert_work_guard_and_tail_only(function);
    let sequence = direct_call(tail, "sequence")
        .unwrap_or_else(|| panic!("{} returns its sequence transaction", function.sig.ident));
    assert_eq!(sequence.args.len(), 2, "sequence(run, parser)");
    assert!(
        exact_path(&sequence.args[0], "run"),
        "{} transacts its exact current run",
        function.sig.ident
    );
    let parser = closure_expression(&sequence.args[1]).expect("immediate sequence parser");
    assert_eq!(
        closure_input_name(parser, "sequence"),
        "run",
        "{} retains the current run in its sequence callback",
        function.sig.ident
    );
    closure_block(parser).expect("sequence parser owns a block")
}

fn assert_plain_sequence_tail(
    function: &syn::ItemFn,
    children: impl IntoIterator<Item = bbnf::compiler::PlanNodeId>,
) {
    let expected = children
        .into_iter()
        .map(|child| format!("node_{}", child.index()))
        .collect::<Vec<_>>();
    let block = sequence_transaction(function);
    assert_eq!(
        child_match_calls(block),
        expected,
        "{} invokes every recorded child once and in order",
        function.sig.ident
    );
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("node_"))
            .collect::<Vec<_>>(),
        expected,
        "{} contains no alternate or dead child call",
        function.sig.ident
    );
    assert_eq!(
        block.stmts.len(),
        expected.len() + 1,
        "{} sequence transaction contains only child steps and its success outcome",
        function.sig.ident
    );
    assert!(
        block
            .stmts
            .last()
            .and_then(statement_expression)
            .is_some_and(exact_unit_matched),
        "{} sequence transaction returns Step::Matched(())",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, [("sequence", 1)]);
    assert_event_rows(function, []);
}

#[derive(Clone, Copy)]
enum TerminalExpectation<'a> {
    Literal(&'a [u8]),
    Regex(usize),
}

fn assert_terminal_tail(function: &syn::ItemFn, terminal: TerminalExpectation<'_>, node: usize) {
    let (primitive, event_kind, expected_statements) = match terminal {
        TerminalExpectation::Literal(_) => ("literal", "Literal", 2),
        TerminalExpectation::Regex(_) => ("regex", "Regex", 3),
    };
    assert_eq!(
        function.block.stmts.len(),
        expected_statements,
        "{} has only its Work guard, optional regex terminal, and returned terminal match",
        function.sig.ident
    );
    let syn::Expr::Match(recognition) = charged_tail(function) else {
        panic!("{} returns its terminal Step match", function.sig.ident);
    };
    let call = direct_call(&recognition.expr, primitive)
        .unwrap_or_else(|| panic!("{} calls `{primitive}` directly", function.sig.ident));
    assert!(
        exact_path(&call.args[0], "run"),
        "{primitive} receives the current run"
    );
    match terminal {
        TerminalExpectation::Literal(expected) => {
            let syn::Expr::Reference(reference) = &call.args[1] else {
                panic!("literal receives its exact emitted byte slice");
            };
            let syn::Expr::Array(array) = reference.expr.as_ref() else {
                panic!("literal byte slice is an inline array");
            };
            assert_eq!(
                array
                    .elems
                    .iter()
                    .map(|byte| {
                        u8::try_from(integer_literal(byte).expect("literal byte"))
                            .expect("literal byte range")
                    })
                    .collect::<Vec<_>>(),
                expected,
                "{} embeds the exact lowering literal bytes",
                function.sig.ident
            );
        }
        TerminalExpectation::Regex(regex) => {
            assert!(
                matches!(&call.args[1], syn::Expr::Reference(reference)
                    if exact_path(&reference.expr, "terminal")),
                "regex receives its exact emitted terminal"
            );
            let (binding, initializer) = local_binding(&function.block.stmts[1])
                .expect("Regex node owns one terminal binding");
            assert_eq!(binding, "terminal");
            let syn::Expr::Call(constructor) = initializer else {
                panic!("Regex terminal uses RegexTerminal::new");
            };
            assert!(
                direct_path(&constructor.func).is_some_and(|path| {
                    path_segments(path).as_slice() == ["RegexTerminal", "new"]
                }) && constructor.args.len() == 1
                    && exact_path(&constructor.args[0], &format!("REGEX_{regex}_PLAN")),
                "{} binds its exact lowering regex plan",
                function.sig.ident
            );
        }
    }
    assert_eq!(
        recognition
            .arms
            .iter()
            .map(|arm| {
                assert!(arm.guard.is_none(), "terminal Step arms are unguarded");
                pattern_variant(&arm.pat).expect("plain terminal Step arm")
            })
            .collect::<Vec<_>>(),
        ["Matched", "Mismatch", "Fault"],
        "{} returns the exact terminal Step partition",
        function.sig.ident
    );
    assert_eq!(call.args.len(), 2, "{primitive}(run, terminal)");
    let expected_event = vec![(
        event_kind.to_owned(),
        [None, Some(node), None, None],
        EventOperator::None,
    )];
    assert_eq!(
        event_rows_expression(&recognition.arms[0].body),
        expected_event,
        "{} emits only from terminal Matched",
        function.sig.ident
    );
    let span = matched_payload_binding(&recognition.arms[0].pat, event_kind);
    let events = event_calls_named_expression(&recognition.arms[0].body, event_kind);
    let [event] = events.as_slice() else {
        panic!("{event_kind} matched arm contains one exact event");
    };
    assert!(
        exact_span_method(&event.args[3], &span, "start")
            && exact_span_method(&event.args[4], &span, "end")
            && exact_zero(&event.args[5]),
        "{event_kind} event derives start/end from its exact recognized span and reserves value 0"
    );
    assert!(
        recognition.arms[1..]
            .iter()
            .all(|arm| event_rows_expression(&arm.body).is_empty()),
        "{} mismatch/fault arms cannot emit",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, [(primitive, 1)]);
    assert_event_rows(
        function,
        [(
            event_kind,
            [None, Some(node), None, None],
            EventOperator::None,
        )],
    );
}

fn assert_epsilon_tail(function: &syn::ItemFn) {
    let tail = assert_work_guard_and_tail_only(function);
    assert!(
        exact_unit_matched(tail),
        "{} returns only Step::Matched(())",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, []);
    assert_event_rows(function, []);
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("rule_") || call.starts_with("node_"))
            .count(),
        0,
        "{} has no parser child",
        function.sig.ident
    );
}

fn assert_rule_ref_tail(function: &syn::ItemFn, rule: RuleId) {
    let expected = format!("rule_{}", rule.index());
    let tail = assert_work_guard_and_tail_only(function);
    let call = direct_call(tail, &expected)
        .unwrap_or_else(|| panic!("{} returns its exact rule reference", function.sig.ident));
    assert_eq!(call.args.len(), 2, "{expected}(run, limits)");
    assert!(exact_path(&call.args[0], "run") && exact_path(&call.args[1], "limits"));
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("rule_") || call.starts_with("node_"))
            .collect::<Vec<_>>(),
        [expected],
        "{} has only its recorded rule reference",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, []);
    assert_event_rows(function, []);
}

fn exact_cursor_offset(expression: &syn::Expr) -> bool {
    let syn::Expr::MethodCall(offset) = expression else {
        return false;
    };
    if offset.method != "offset" || !offset.args.is_empty() {
        return false;
    }
    matches!(
        offset.receiver.as_ref(),
        syn::Expr::MethodCall(cursor)
            if cursor.method == "cursor"
                && cursor.args.is_empty()
                && exact_path(&cursor.receiver, "run")
    )
}

fn assert_choice_step(
    expression: &syn::Expr,
    node: usize,
    children: &[bbnf::compiler::PlanNodeId],
    branch: usize,
) {
    assert!(
        !children.is_empty(),
        "canonical Choice has at least one child"
    );
    if children.len() > 1 {
        let choice = direct_call(expression, "choice").expect("balanced Choice returns `choice`");
        assert_eq!(choice.args.len(), 3, "choice(run, left, right)");
        assert!(exact_path(&choice.args[0], "run"));
        let midpoint = children.len() / 2;
        for (argument, (partition, offset)) in choice.args.iter().skip(1).zip([
            (&children[..midpoint], branch),
            (&children[midpoint..], branch + midpoint),
        ]) {
            let closure = closure_expression(argument).expect("immediate Choice branch closure");
            assert_eq!(closure_input_name(closure, "choice"), "run");
            assert_choice_step(&closure.body, node, partition, offset);
        }
        return;
    }

    let syn::Expr::Block(leaf) = expression else {
        panic!("Choice leaf is a direct returned block");
    };
    assert_eq!(
        leaf.block.stmts.len(),
        2,
        "Choice leaf has start and child match"
    );
    let (start, initializer) =
        local_binding(&leaf.block.stmts[0]).expect("Choice leaf records its start");
    assert_eq!(start, "start");
    assert!(exact_cursor_offset(initializer));
    let syn::Stmt::Expr(syn::Expr::Match(recognition), _) = &leaf.block.stmts[1] else {
        panic!("Choice leaf returns its child Step match");
    };
    let child = format!("node_{}", children[0].index());
    let call = direct_call(&recognition.expr, &child).expect("Choice leaf calls recorded child");
    assert_eq!(call.args.len(), 2, "{child}(run, limits)");
    assert!(exact_path(&call.args[0], "run") && exact_path(&call.args[1], "limits"));
    assert_eq!(
        recognition
            .arms
            .iter()
            .map(|arm| {
                assert!(arm.guard.is_none(), "Choice Step arms are unguarded");
                pattern_variant(&arm.pat).expect("plain Choice Step arm")
            })
            .collect::<Vec<_>>(),
        ["Matched", "Mismatch", "Fault"],
        "Choice leaf uses the exact Step partition"
    );
    let matched_events = event_calls_named_expression(&recognition.arms[0].body, "ChoiceBranch");
    assert_eq!(
        matched_events.len(),
        1,
        "Choice success emits its branch once"
    );
    assert_eq!(
        event_identity(matched_events[0]),
        Some((
            "ChoiceBranch".to_owned(),
            [None, Some(node), None, None],
            EventOperator::None,
        ))
    );
    assert_eq!(
        integer_literal(&matched_events[0].args[5]),
        Some(branch),
        "Choice event preserves its exact branch ordinal"
    );
    let ends = cursor_bindings_expression(&recognition.arms[0].body);
    let end_bindings = ends.iter().collect::<Vec<_>>();
    let [end] = end_bindings.as_slice() else {
        panic!("Choice success records one exact end cursor");
    };
    assert_event_span_value_paths(matched_events[0], &start, end, branch, "ChoiceBranch");
    let syn::Expr::Block(matched_body) = recognition.arms[0].body.as_ref() else {
        panic!("Choice matched arm owns its end/event block");
    };
    assert!(
        cursor_binding_dominates_event(&matched_body.block, matched_events[0], end),
        "ChoiceBranch end cursor is captured after its child commits"
    );
    assert!(
        recognition.arms[1..]
            .iter()
            .all(|arm| event_rows_expression(&arm.body).is_empty()),
        "Choice mismatch/fault cannot emit"
    );
}

fn event_calls_named_expression<'a>(
    expression: &'a syn::Expr,
    variant: &'a str,
) -> Vec<&'a syn::ExprCall> {
    struct Calls<'a> {
        variant: &'a str,
        rows: Vec<&'a syn::ExprCall>,
    }
    impl<'a, 'ast: 'a> Visit<'ast> for Calls<'a> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if direct_path(&call.func).is_some_and(|path| path.is_ident("event"))
                && event_identity(call).is_some_and(|(variant, _, _)| variant == self.variant)
            {
                self.rows.push(call);
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut calls = Calls {
        variant,
        rows: Vec::new(),
    };
    calls.visit_expr(expression);
    calls.rows
}

fn matched_payload_binding(pattern: &syn::Pat, owner: &str) -> String {
    let syn::Pat::TupleStruct(pattern) = pattern else {
        panic!("{owner} matched arm binds its exact payload");
    };
    assert!(
        pattern
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "Matched")
            && pattern.elems.len() == 1,
        "{owner} matched arm binds exactly one payload"
    );
    pat_ident(&pattern.elems[0])
        .unwrap_or_else(|| panic!("{owner} matched payload is one plain binding"))
}

fn cursor_bindings_expression(expression: &syn::Expr) -> BTreeSet<String> {
    struct Bindings(BTreeSet<String>);
    impl<'ast> Visit<'ast> for Bindings {
        fn visit_local(&mut self, local: &'ast syn::Local) {
            if let Some(initializer) = &local.init
                && exact_cursor_offset(&initializer.expr)
                && let Some(binding) = pat_ident(&local.pat)
            {
                self.0.insert(binding);
            }
            visit::visit_local(self, local);
        }
    }
    let mut bindings = Bindings(BTreeSet::new());
    bindings.visit_expr(expression);
    bindings.0
}

fn event_call_in_statement<'a>(statement: &'a syn::Stmt, variant: &'a str) -> &'a syn::ExprCall {
    let expression = statement_expression(statement)
        .unwrap_or_else(|| panic!("{variant} owner statement is executable"));
    let events = event_calls_named_expression(expression, variant);
    let [event] = events.as_slice() else {
        panic!("{variant} owner statement contains exactly one event");
    };
    event
}

fn assert_event_span_value_paths(
    call: &syn::ExprCall,
    start: &str,
    end: &str,
    value: usize,
    owner: &str,
) {
    assert!(
        exact_path(&call.args[3], start) && exact_path(&call.args[4], end),
        "{owner} event derives its exact start/end bindings"
    );
    assert_eq!(
        integer_literal(&call.args[5]),
        Some(value),
        "{owner} event carries its exact scalar value"
    );
}

fn exact_repeat_count_value(expression: &syn::Expr, count: &str) -> bool {
    let syn::Expr::MethodCall(fallback) = expression else {
        return false;
    };
    if fallback.method != "unwrap_or" || fallback.args.len() != 1 {
        return false;
    }
    let syn::Expr::Call(conversion) = fallback.receiver.as_ref() else {
        return false;
    };
    direct_path(&conversion.func)
        .is_some_and(|path| path_segments(path).as_slice() == ["u32", "try_from"])
        && conversion.args.len() == 1
        && exact_path(&conversion.args[0], count)
        && field_path(&fallback.args[0]).as_deref() == Some(&["u32".to_owned(), "MAX".to_owned()])
}

fn cursor_binding_dominates_event(
    block: &syn::Block,
    event: &syn::ExprCall,
    binding: &str,
) -> bool {
    struct Dominance<'a> {
        event: &'a syn::ExprCall,
        binding: &'a str,
        visible: Vec<bool>,
        dominated: bool,
    }
    impl<'a, 'ast: 'a> Visit<'ast> for Dominance<'a> {
        fn visit_block(&mut self, block: &'ast syn::Block) {
            let inherited = self.visible.last().copied().unwrap_or(false);
            self.visible.push(inherited);
            for statement in &block.stmts {
                match statement {
                    syn::Stmt::Local(local) => {
                        if let Some(initializer) = &local.init {
                            self.visit_expr(&initializer.expr);
                        }
                        if pat_ident(&local.pat).as_deref() == Some(self.binding)
                            && local
                                .init
                                .as_ref()
                                .is_some_and(|initializer| exact_cursor_offset(&initializer.expr))
                        {
                            *self.visible.last_mut().expect("active lexical block") = true;
                        }
                    }
                    syn::Stmt::Item(item) => self.visit_item(item),
                    syn::Stmt::Expr(expression, _) => self.visit_expr(expression),
                    syn::Stmt::Macro(statement) => self.visit_stmt_macro(statement),
                }
            }
            self.visible.pop();
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if std::ptr::eq(call, self.event) {
                self.dominated = self.visible.last().copied().unwrap_or(false);
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut dominance = Dominance {
        event,
        binding,
        visible: Vec::new(),
        dominated: false,
    };
    dominance.visit_block(block);
    dominance.dominated
}

fn assert_choice_tail(
    function: &syn::ItemFn,
    node: usize,
    children: impl IntoIterator<Item = bbnf::compiler::PlanNodeId>,
) {
    let children = children.into_iter().collect::<Vec<_>>();
    let tail = assert_work_guard_and_tail_only(function);
    assert_choice_step(tail, node, &children, 0);
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("node_"))
            .collect::<Vec<_>>(),
        children
            .iter()
            .map(|child| format!("node_{}", child.index()))
            .collect::<Vec<_>>(),
        "{} invokes its recorded Choice children once and in order",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, [("choice", children.len().saturating_sub(1))]);
    assert_event_rows(
        function,
        children.iter().map(|_| {
            (
                "ChoiceBranch",
                [None, Some(node), None, None],
                EventOperator::None,
            )
        }),
    );
}

fn assert_repeat_tail(
    function: &syn::ItemFn,
    node: usize,
    child: bbnf::compiler::PlanNodeId,
    minimum: u32,
    maximum: Option<u32>,
) {
    assert_eq!(
        function.block.stmts.len(),
        3,
        "{} has only Work, start, and returned Repeat dataflow",
        function.sig.ident
    );
    let (start, initializer) =
        local_binding(&function.block.stmts[1]).expect("Repeat records its start cursor");
    assert_eq!(start, "start");
    assert!(exact_cursor_offset(initializer));
    let syn::Expr::Match(recognition) = charged_tail(function) else {
        panic!("{} returns its Repeat Step match", function.sig.ident);
    };
    let repeat = direct_call(&recognition.expr, "repeat").expect("direct Repeat primitive");
    assert_eq!(repeat.args.len(), 4, "repeat(run, minimum, maximum, child)");
    assert!(exact_path(&repeat.args[0], "run"));
    assert_eq!(integer_literal(&repeat.args[1]), Some(minimum as usize));
    assert_eq!(
        option_integer(&repeat.args[2]),
        Some(maximum.map(|value| value as usize))
    );
    let expected = format!("node_{}", child.index());
    let parser = closure_expression(&repeat.args[3]).expect("immediate Repeat child closure");
    assert_eq!(closure_input_name(parser, "repeat"), "run");
    let child_call = closure_direct_call(parser, &expected);
    assert_eq!(child_call.args.len(), 2, "{expected}(run, limits)");
    assert!(exact_path(&child_call.args[0], "run") && exact_path(&child_call.args[1], "limits"));
    assert_eq!(
        recognition
            .arms
            .iter()
            .map(|arm| {
                assert!(arm.guard.is_none(), "Repeat Step arms are unguarded");
                pattern_variant(&arm.pat).expect("plain Repeat Step arm")
            })
            .collect::<Vec<_>>(),
        ["Matched", "Mismatch", "Fault"],
        "Repeat returns the exact Step partition"
    );
    assert_eq!(
        event_rows_expression(&recognition.arms[0].body),
        [(
            "RepeatCount".to_owned(),
            [None, Some(node), None, None],
            EventOperator::None,
        )],
        "Repeat emits only after a successful count"
    );
    let count = matched_payload_binding(&recognition.arms[0].pat, "RepeatCount");
    let events = event_calls_named_expression(&recognition.arms[0].body, "RepeatCount");
    let [event] = events.as_slice() else {
        panic!("Repeat matched arm contains one exact RepeatCount event");
    };
    let ends = cursor_bindings_expression(&recognition.arms[0].body);
    let end_bindings = ends.iter().collect::<Vec<_>>();
    let [end] = end_bindings.as_slice() else {
        panic!("Repeat success records one exact end cursor");
    };
    assert!(
        exact_path(&event.args[3], &start)
            && exact_path(&event.args[4], end)
            && exact_repeat_count_value(&event.args[5], &count),
        "RepeatCount derives its exact transaction span and saturating matched count"
    );
    let syn::Expr::Block(matched_body) = recognition.arms[0].body.as_ref() else {
        panic!("Repeat matched arm owns its end/event block");
    };
    assert!(
        cursor_binding_dominates_event(&matched_body.block, event, end),
        "RepeatCount end cursor is captured after repeat commits"
    );
    assert!(
        recognition.arms[1..]
            .iter()
            .all(|arm| event_rows_expression(&arm.body).is_empty()),
        "Repeat mismatch/fault cannot emit"
    );
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("node_"))
            .collect::<Vec<_>>(),
        [expected]
    );
    assert_parser_primitive_counts(function, [("repeat", 1)]);
    assert_event_rows(
        function,
        [(
            "RepeatCount",
            [None, Some(node), None, None],
            EventOperator::None,
        )],
    );
}

fn assert_record_tail(
    function: &syn::ItemFn,
    owner: RuleId,
    slots: impl IntoIterator<Item = PlanRecordSlot>,
) {
    let slots = slots.into_iter().collect::<Vec<_>>();
    let block = sequence_transaction(function);
    let expected_children = slots
        .iter()
        .map(|slot| format!("node_{}", slot.child().index()))
        .collect::<Vec<_>>();
    assert_eq!(
        child_match_calls(block),
        expected_children,
        "{} recognizes every Record slot once and in order",
        function.sig.ident
    );
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("node_"))
            .collect::<Vec<_>>(),
        expected_children,
        "{} contains no alternate or dead Record child",
        function.sig.ident
    );

    let mut expected_events = Vec::new();
    let mut expected_trace = Vec::new();
    let mut expected_statements = 1;
    let mut statement = 0;
    for slot in &slots {
        let child = slot.child().index();
        match slot {
            PlanRecordSlot::Field { field, .. } => {
                let identity = [Some(owner.index()), Some(child), Some(field.index()), None];
                expected_events.extend([
                    ("EnterField", identity, EventOperator::None),
                    ("LeaveField", identity, EventOperator::None),
                ]);
                expected_trace.extend([
                    NodeExecution::Event("EnterField".to_owned(), identity, EventOperator::None),
                    NodeExecution::Child(format!("node_{child}")),
                    NodeExecution::Event("LeaveField".to_owned(), identity, EventOperator::None),
                ]);
                expected_statements += 5;
                let (start, start_initializer) = local_binding(&block.stmts[statement])
                    .expect("Field records its start immediately before EnterField");
                assert!(exact_cursor_offset(start_initializer));
                let enter = event_call_in_statement(&block.stmts[statement + 1], "EnterField");
                assert_eq!(
                    event_identity(enter),
                    Some(("EnterField".to_owned(), identity, EventOperator::None))
                );
                assert_event_span_value_paths(enter, &start, &start, 0, "EnterField");
                let child_expression = statement_expression(&block.stmts[statement + 2])
                    .expect("Field child statement is executable");
                let child_name = format!("node_{child}");
                let child_calls = direct_calls_named_expression(child_expression, &child_name);
                let [child_call] = child_calls.as_slice() else {
                    panic!("Field invokes its exact child between enter and leave");
                };
                assert_exact_call_arguments(child_call, &["run", "limits"], &child_name);
                let (end, end_initializer) = local_binding(&block.stmts[statement + 3])
                    .expect("Field records its end immediately after its child");
                assert!(exact_cursor_offset(end_initializer));
                let leave = event_call_in_statement(&block.stmts[statement + 4], "LeaveField");
                assert_eq!(
                    event_identity(leave),
                    Some(("LeaveField".to_owned(), identity, EventOperator::None))
                );
                assert_event_span_value_paths(leave, &start, &end, 0, "LeaveField");
                statement += 5;
            }
            PlanRecordSlot::Discard { .. } => {
                expected_trace.push(NodeExecution::Child(format!("node_{child}")));
                expected_statements += 1;
                let expression = statement_expression(&block.stmts[statement])
                    .expect("Discard slot owns its child statement");
                let child_name = format!("node_{child}");
                let child_calls = direct_calls_named_expression(expression, &child_name);
                let [call] = child_calls.as_slice() else {
                    panic!("Discard slot invokes exactly its recorded child");
                };
                assert_exact_call_arguments(call, &["run", "limits"], &child_name);
                statement += 1;
            }
            unexpected => panic!("unverified generated-v6 Record slot: {unexpected:?}"),
        }
    }
    assert_eq!(
        block.stmts.len(),
        expected_statements,
        "{} contains only its exact Record slot transactions and success outcome",
        function.sig.ident
    );
    assert!(
        block
            .stmts
            .last()
            .and_then(statement_expression)
            .is_some_and(exact_unit_matched),
        "{} Record transaction ends in Step::Matched(())",
        function.sig.ident
    );
    assert_eq!(
        node_execution_trace(block),
        expected_trace,
        "{} preserves exact field-enter/child/field-leave dataflow",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, [("sequence", 1)]);
    assert_event_rows(function, expected_events);
}

fn assert_transform_tail(
    function: &syn::ItemFn,
    node: usize,
    child: bbnf::compiler::PlanNodeId,
    transform: bbnf::compiler::TransformId,
) {
    let block = sequence_transaction(function);
    assert_eq!(
        block.stmts.len(),
        4,
        "{} has only start, child, end, and Transform event",
        function.sig.ident
    );
    let (start, start_initializer) =
        local_binding(&block.stmts[0]).expect("Transform records start");
    let (end, end_initializer) = local_binding(&block.stmts[2]).expect("Transform records end");
    assert_eq!((start.as_str(), end.as_str()), ("start", "end"));
    assert!(exact_cursor_offset(start_initializer) && exact_cursor_offset(end_initializer));
    let child = format!("node_{}", child.index());
    assert_eq!(child_match_calls(block), [child.clone()]);
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("node_"))
            .collect::<Vec<_>>(),
        [child.clone()],
        "{} contains only its recorded Transform child",
        function.sig.ident
    );
    let identity = [None, Some(node), None, Some(transform.index())];
    let transform_event = event_call_in_statement(&block.stmts[3], "Transform");
    assert_eq!(
        event_identity(transform_event),
        Some(("Transform".to_owned(), identity, EventOperator::None))
    );
    assert_event_span_value_paths(transform_event, &start, &end, 0, "Transform");
    assert_eq!(
        node_execution_trace(block),
        [
            NodeExecution::Child(child),
            NodeExecution::Event("Transform".to_owned(), identity, EventOperator::None),
        ],
        "{} transforms only after its child succeeds",
        function.sig.ident
    );
    assert_parser_primitive_counts(function, [("sequence", 1)]);
    assert_event_rows(function, [("Transform", identity, EventOperator::None)]);
}

fn assert_rule_functions(module: &CompiledModule, functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    let expected = module
        .lowering()
        .rules()
        .iter()
        .enumerate()
        .map(|(index, _)| format!("rule_{index}"))
        .collect::<BTreeSet<_>>();
    let actual = functions
        .keys()
        .filter(|name| name.starts_with("rule_"))
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_eq!(actual, expected, "one canonical function per lowered rule");

    for (ordinal, rule) in module.lowering().rules().iter().enumerate() {
        let name = format!("rule_{ordinal}");
        let function = unique_function(functions, &name);
        let _ = assert_work_guard_and_tail_only(function);
        let transaction = guarded_transaction_block(function);
        assert_eq!(
            transaction.stmts.len(),
            5,
            "{name} contains only start, enter, root, end, and leave"
        );
        let (start, start_initializer) =
            local_binding(&transaction.stmts[0]).expect("Rule records its entry cursor");
        assert!(exact_cursor_offset(start_initializer));
        let enter = event_call_in_statement(&transaction.stmts[1], "EnterRule");
        assert_event_span_value_paths(enter, &start, &start, 0, "EnterRule");
        let (end, end_initializer) =
            local_binding(&transaction.stmts[3]).expect("Rule records its committed end cursor");
        assert!(exact_cursor_offset(end_initializer));
        let leave = event_call_in_statement(&transaction.stmts[4], "LeaveRule");
        assert_event_span_value_paths(leave, &start, &end, 0, "LeaveRule");
        assert_parser_primitive_counts(function, [("sequence", 1)]);
        let expected_node = format!("node_{}", rule.root().index());
        assert_eq!(
            direct_internal_calls(transaction)
                .into_iter()
                .filter(|call| call.starts_with("node_"))
                .collect::<Vec<_>>(),
            [expected_node],
            "{name} invokes only its recorded root node"
        );
        assert_event_rows(
            function,
            [
                (
                    "EnterRule",
                    [Some(ordinal), None, None, None],
                    EventOperator::None,
                ),
                (
                    "LeaveRule",
                    [Some(ordinal), None, None, None],
                    EventOperator::None,
                ),
            ],
        );
        assert_eq!(
            child_match_calls(transaction),
            [format!("node_{}", rule.root().index())],
            "{name} recognizes its exact root through the canonical Step partition"
        );
        assert_eq!(
            node_execution_trace(transaction),
            [
                NodeExecution::Event(
                    "EnterRule".to_owned(),
                    [Some(ordinal), None, None, None],
                    EventOperator::None,
                ),
                NodeExecution::Child(format!("node_{}", rule.root().index())),
                NodeExecution::Event(
                    "LeaveRule".to_owned(),
                    [Some(ordinal), None, None, None],
                    EventOperator::None,
                ),
            ],
            "{name} preserves enter/root/leave execution order"
        );
    }
}

fn closure_direct_call<'a>(closure: &'a syn::ExprClosure, expected: &str) -> &'a syn::ExprCall {
    let expression = match closure.body.as_ref() {
        syn::Expr::Call(_) => closure.body.as_ref(),
        syn::Expr::Block(block) => match block.block.stmts.as_slice() {
            [syn::Stmt::Expr(expression, None)] => expression,
            _ => panic!("canonical callback contains only its direct dataflow call"),
        },
        _ => panic!("canonical callback directly returns its child call"),
    };
    direct_call(expression, expected)
        .unwrap_or_else(|| panic!("canonical callback must invoke `{expected}` directly"))
}

fn assert_difference_tail(
    function: &syn::ItemFn,
    include: bbnf::compiler::PlanNodeId,
    exclude: bbnf::compiler::PlanNodeId,
) {
    let tail = assert_work_guard_and_tail_only(function);
    let difference =
        direct_call(tail, "difference").expect("Difference node returns the owned primitive");
    assert_eq!(
        difference.args.len(),
        3,
        "difference(run, include, exclude)"
    );
    assert!(
        exact_path(&difference.args[0], "run"),
        "Difference receives its exact current run"
    );
    for (argument, child) in difference.args.iter().skip(1).zip([include, exclude]) {
        let closure = closure_expression(argument).expect("immediate Difference child closure");
        assert_eq!(closure_input_name(closure, "difference"), "run");
        let expected = format!("node_{}", child.index());
        let call = closure_direct_call(closure, &expected);
        assert_exact_call_arguments(call, &["run", "limits"], &expected);
    }
    let calls = direct_internal_calls(&function.block);
    assert_eq!(
        calls
            .iter()
            .filter(|name| name.starts_with("node_"))
            .count(),
        2,
        "Difference has only its exact include/exclude children"
    );
    assert_eq!(
        call_count(&function.block, "difference"),
        1,
        "Difference has one live primitive"
    );
    assert_parser_primitive_counts(function, [("difference", 1)]);
    assert_event_rows(function, []);
}

#[derive(Debug, Eq, PartialEq)]
enum TrimStep {
    Regex,
    Trivia(usize),
    Child,
}

struct TrimTrace<'a> {
    node: usize,
    child: &'a str,
    active_span: Option<String>,
    nonempty_span: Option<String>,
    steps: Vec<TrimStep>,
    regex_matches: usize,
}

impl TrimTrace<'_> {
    fn expression_uses_trivia_plan(expression: &syn::Expr) -> bool {
        #[derive(Default)]
        struct PlanUse(bool);
        impl<'ast> Visit<'ast> for PlanUse {
            fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
                if path.path.is_ident("TRIVIA_PLAN") {
                    self.0 = true;
                }
                visit::visit_expr_path(self, path);
            }
        }
        let mut use_ = PlanUse::default();
        use_.visit_expr(expression);
        use_.0
    }

    fn matched_binding(pattern: &syn::Pat) -> Option<String> {
        let syn::Pat::TupleStruct(pattern) = pattern else {
            return None;
        };
        if !pattern
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "Matched")
            || pattern.elems.len() != 1
        {
            return None;
        }
        let syn::Pat::Ident(binding) = &pattern.elems[0] else {
            return None;
        };
        Some(binding.ident.to_string())
    }

    fn span_method(expression: &syn::Expr, binding: &str, method: &str) -> bool {
        matches!(
            expression,
            syn::Expr::MethodCall(call)
                if call.method == method
                    && call.args.is_empty()
                    && matches!(call.receiver.as_ref(), syn::Expr::Path(path)
                        if path.path.is_ident(binding))
        )
    }

    fn nonempty_condition(expression: &syn::Expr, binding: &str) -> bool {
        let syn::Expr::Binary(binary) = expression else {
            return false;
        };
        matches!(binary.op, syn::BinOp::Ne(_) | syn::BinOp::Lt(_))
            && Self::span_method(&binary.left, binding, "start")
            && Self::span_method(&binary.right, binding, "end")
    }

    fn validate_trivia_event(&mut self, call: &syn::ExprCall) {
        let binding = self
            .active_span
            .as_deref()
            .expect("Trivia occurs only in a regex Matched arm");
        assert_eq!(
            self.nonempty_span.as_deref(),
            Some(binding),
            "Trivia emission is dominated by its same-span nonempty check"
        );
        let (variant, identity, operator) =
            event_identity(call).expect("canonical Trivia event identity");
        assert_eq!(variant, "Trivia");
        assert_eq!(identity, [None, Some(self.node), None, None]);
        assert_eq!(
            operator,
            EventOperator::None,
            "Trivia has no operator identity"
        );
        assert!(
            Self::span_method(&call.args[3], binding, "start")
                && Self::span_method(&call.args[4], binding, "end"),
            "Trivia span derives from the exact matched span"
        );
        let side = integer_literal(&call.args[5]).expect("Trivia side literal");
        assert!(matches!(side, 0 | 1), "Trivia side is leading or trailing");
        self.steps.push(TrimStep::Trivia(side));
    }
}

impl<'ast> Visit<'ast> for TrimTrace<'_> {
    fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
        if let Some(regex_call) = direct_call(&expression.expr, "regex") {
            assert_eq!(regex_call.args.len(), 2, "regex(run, trivia_terminal)");
            assert!(
                exact_path(&regex_call.args[0], "run"),
                "Trim regex receives its exact transactional run"
            );
            assert!(
                Self::expression_uses_trivia_plan(&expression.expr),
                "Trim regex directly uses TRIVIA_PLAN"
            );
            assert_eq!(
                expression
                    .arms
                    .iter()
                    .map(|arm| {
                        assert!(arm.guard.is_none(), "Trim Step arms are unguarded");
                        pattern_variant(&arm.pat).expect("plain Trim Step arm")
                    })
                    .collect::<Vec<_>>(),
                ["Matched", "Mismatch", "Fault"],
                "Trim regex uses the exact Step partition"
            );
            self.steps.push(TrimStep::Regex);
            self.regex_matches += 1;
            for arm in &expression.arms {
                let inherited = self.active_span.clone();
                self.active_span = Self::matched_binding(&arm.pat);
                if self.active_span.is_none() {
                    assert!(
                        expression_definitely_exits(&arm.body),
                        "Trim regex mismatch/fault exits its transaction"
                    );
                }
                self.visit_expr(&arm.body);
                self.active_span = inherited;
            }
            return;
        }
        visit::visit_expr_match(self, expression);
    }

    fn visit_expr_if(&mut self, expression: &'ast syn::ExprIf) {
        if let Some(binding) = &self.active_span
            && Self::nonempty_condition(&expression.cond, binding)
        {
            let inherited = self.nonempty_span.clone();
            self.nonempty_span = Some(binding.clone());
            self.visit_block(&expression.then_branch);
            self.nonempty_span = inherited;
            if let Some((_, else_branch)) = &expression.else_branch {
                self.visit_expr(else_branch);
            }
            return;
        }
        visit::visit_expr_if(self, expression);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if direct_path(&call.func).is_some_and(|path| path.is_ident(self.child)) {
            assert_exact_call_arguments(call, &["run", "limits"], self.child);
            self.steps.push(TrimStep::Child);
        }
        if direct_path(&call.func).is_some_and(|path| path.is_ident("event"))
            && event_identity(call).is_some_and(|(variant, _, _)| variant == "Trivia")
        {
            self.validate_trivia_event(call);
        }
        visit::visit_expr_call(self, call);
    }
}

fn trim_transaction_block(function: &syn::ItemFn) -> &syn::Block {
    let tail = assert_work_guard_and_tail_only(function);
    let sequence = direct_call(tail, "sequence")
        .expect("complete leading/child/trailing Trim path is one transaction");
    assert_eq!(sequence.args.len(), 2);
    assert!(
        exact_path(&sequence.args[0], "run"),
        "Trim transaction receives its exact current run"
    );
    let closure = closure_expression(&sequence.args[1]).expect("immediate Trim transaction");
    assert_eq!(closure_input_name(closure, "Trim sequence"), "run");
    closure_block(closure).expect("Trim transaction block")
}

fn assert_trim_tail(function: &syn::ItemFn, node: usize, child: bbnf::compiler::PlanNodeId) {
    let body = trim_transaction_block(function);
    let child = format!("node_{}", child.index());
    let mut trace = TrimTrace {
        node,
        child: &child,
        active_span: None,
        nonempty_span: None,
        steps: Vec::new(),
        regex_matches: 0,
    };
    trace.visit_block(body);
    assert_eq!(
        trace.steps,
        [
            TrimStep::Regex,
            TrimStep::Trivia(0),
            TrimStep::Child,
            TrimStep::Regex,
            TrimStep::Trivia(1),
        ],
        "Trim has exact leading/child/trailing dataflow"
    );
    assert_eq!(trace.regex_matches, 2);
    assert_eq!(call_count(&function.block, &child), 1);
    assert_parser_primitive_counts(function, [("regex", 2), ("sequence", 1)]);
    assert_event_rows(
        function,
        [
            (
                "Trivia",
                [None, Some(node), None, None],
                EventOperator::None,
            ),
            (
                "Trivia",
                [None, Some(node), None, None],
                EventOperator::None,
            ),
        ],
    );
}

fn assert_pratt_node_tail(function: &syn::ItemFn, table: PrattTableId) {
    let expected = format!("pratt_{}", table.index());
    let tail = assert_work_guard_and_tail_only(function);
    let call = direct_call(tail, &expected).expect("Pratt node is the sole table ingress adapter");
    assert_eq!(call.args.len(), 3, "{expected}(run, limits, 0)");
    assert!(exact_path(&call.args[0], "run") && exact_path(&call.args[1], "limits"));
    assert_eq!(integer_literal(&call.args[2]), Some(0));
    assert_eq!(
        direct_internal_calls(&function.block)
            .into_iter()
            .filter(|call| call.starts_with("pratt_"))
            .collect::<Vec<_>>(),
        [expected]
    );
    assert_parser_primitive_counts(function, []);
    assert_event_rows(function, []);
}

fn assert_node_functions(module: &CompiledModule, functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    let expected = module
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .map(|(index, _)| format!("node_{index}"))
        .collect::<BTreeSet<_>>();
    let actual = functions
        .keys()
        .filter(|name| name.starts_with("node_"))
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_eq!(actual, expected, "one canonical function per plan node");

    for (ordinal, node) in module.lowering().nodes().iter().enumerate() {
        let function = unique_function(functions, &format!("node_{ordinal}"));
        assert!(
            !matches!(charged_tail(function), syn::Expr::MethodCall(_)),
            "node_{ordinal} cannot delegate its PlanOp to an inherent or trait method"
        );
        match node.op() {
            PlanOp::Epsilon => assert_epsilon_tail(function),
            PlanOp::Literal(bytes) => {
                assert_terminal_tail(function, TerminalExpectation::Literal(bytes), ordinal)
            }
            PlanOp::Regex(regex) => {
                assert_terminal_tail(function, TerminalExpectation::Regex(regex), ordinal)
            }
            PlanOp::RuleRef(rule) => assert_rule_ref_tail(function, rule),
            PlanOp::Sequence(children) => assert_plain_sequence_tail(function, children),
            PlanOp::Choice(children) => assert_choice_tail(function, ordinal, children),
            PlanOp::Repeat {
                child,
                minimum,
                maximum,
            } => assert_repeat_tail(function, ordinal, child, minimum, maximum),
            PlanOp::Record { owner, slots } => assert_record_tail(function, owner, slots),
            PlanOp::Transform {
                child, transform, ..
            } => assert_transform_tail(function, ordinal, child, transform),
            PlanOp::Projection {
                left,
                right,
                keep: _,
            } => assert_plain_sequence_tail(function, [left, right]),
            PlanOp::Difference { include, exclude } => {
                assert_difference_tail(function, include, exclude);
            }
            PlanOp::Trim { child } => assert_trim_tail(function, ordinal, child),
            PlanOp::Pratt { atom: _, table } => assert_pratt_node_tail(function, table),
            unexpected => panic!("unverified generated-v6 PlanOp: {unexpected:?}"),
        }
    }
}

fn top_level_array<'a>(file: &'a syn::File, name: &str) -> &'a syn::ExprArray {
    let matches = file
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Const(item) if item.ident == name => Some(item.expr.as_ref()),
            syn::Item::Static(item) if item.ident == name => Some(item.expr.as_ref()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(matches.len(), 1, "one canonical `{name}` definition");
    let syn::Expr::Array(array) = matches[0] else {
        panic!("`{name}` is a direct array initializer");
    };
    array
}

fn struct_fields(expression: &syn::Expr) -> BTreeMap<String, &syn::Expr> {
    let syn::Expr::Struct(row) = expression else {
        panic!("descriptor row is a direct struct initializer");
    };
    assert!(
        row.rest.is_none(),
        "descriptor rows cannot use update syntax"
    );
    row.fields
        .iter()
        .map(|field| {
            let syn::Member::Named(name) = &field.member else {
                panic!("descriptor fields are named");
            };
            (name.to_string(), &field.expr)
        })
        .collect()
}

fn path_terminal(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn assert_pratt_descriptors(file: &syn::File, module: &CompiledModule) {
    let tables = top_level_array(file, "PRATT_TABLES");
    let operators = top_level_array(file, "PRATT_OPERATORS");
    assert_eq!(tables.elems.len(), module.lowering().pratt_tables().len());
    assert_eq!(
        operators.elems.len(),
        module.lowering().pratt_operators().len()
    );

    for (index, (actual, expected)) in tables
        .elems
        .iter()
        .zip(module.lowering().pratt_tables())
        .enumerate()
    {
        let fields = struct_fields(actual);
        assert_eq!(
            fields.keys().map(String::as_str).collect::<BTreeSet<_>>(),
            BTreeSet::from([
                "atom_rule",
                "id",
                "operator_len",
                "operator_start",
                "site_rule",
            ])
        );
        let operator_ids = expected.operators().collect::<Vec<_>>();
        assert_eq!(integer_literal(fields["id"]), Some(index));
        assert_eq!(
            integer_literal(fields["site_rule"]),
            Some(expected.site_rule().index())
        );
        assert_eq!(
            integer_literal(fields["atom_rule"]),
            Some(expected.atom_rule().index())
        );
        assert_eq!(
            integer_literal(fields["operator_start"]),
            operator_ids.first().map_or(Some(0), |id| Some(id.index()))
        );
        assert_eq!(
            integer_literal(fields["operator_len"]),
            Some(operator_ids.len())
        );
    }

    for (index, (actual, expected)) in operators
        .elems
        .iter()
        .zip(module.lowering().pratt_operators())
        .enumerate()
    {
        let fields = struct_fields(actual);
        assert_eq!(
            fields.keys().map(String::as_str).collect::<BTreeSet<_>>(),
            BTreeSet::from([
                "fixity",
                "id",
                "left_binding_power",
                "matcher_rule",
                "right_binding_power",
                "table",
            ])
        );
        assert_eq!(integer_literal(fields["id"]), Some(index));
        assert_eq!(
            integer_literal(fields["table"]),
            Some(expected.table().index())
        );
        assert_eq!(
            integer_literal(fields["matcher_rule"]),
            Some(expected.matcher_rule().index())
        );
        assert_eq!(
            option_integer(fields["left_binding_power"]),
            Some(expected.left_binding_power().map(usize::from))
        );
        assert_eq!(
            option_integer(fields["right_binding_power"]),
            Some(expected.right_binding_power().map(usize::from))
        );
        assert_eq!(
            path_terminal(fields["fixity"]).map(|value| value.to_ascii_lowercase()),
            Some(format!("{:?}", expected.fixity()).to_ascii_lowercase())
        );
    }
}

fn assert_exact_call_order(actual: &[String], expected: &[String], at: &str) {
    assert_eq!(actual, expected, "{at} preserves exact manifest row order");
}

fn direct_calls_named<'a>(block: &'a syn::Block, name: &'a str) -> Vec<&'a syn::ExprCall> {
    struct NamedCalls<'a> {
        name: &'a str,
        calls: Vec<&'a syn::ExprCall>,
    }
    impl<'a, 'ast: 'a> Visit<'ast> for NamedCalls<'a> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if direct_path(&call.func).is_some_and(|path| path.is_ident(self.name)) {
                self.calls.push(call);
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut calls = NamedCalls {
        name,
        calls: Vec::new(),
    };
    calls.visit_block(block);
    calls.calls
}

fn direct_calls_named_expression<'ast>(
    expression: &'ast syn::Expr,
    name: &str,
) -> Vec<&'ast syn::ExprCall> {
    struct NamedCalls<'name, 'ast> {
        name: &'name str,
        calls: Vec<&'ast syn::ExprCall>,
    }
    impl<'name, 'ast> Visit<'ast> for NamedCalls<'name, 'ast> {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if direct_path(&call.func).is_some_and(|path| path.is_ident(self.name)) {
                self.calls.push(call);
            }
            visit::visit_expr_call(self, call);
        }
    }
    let mut calls = NamedCalls {
        name,
        calls: Vec::new(),
    };
    calls.visit_expr(expression);
    calls.calls
}

fn method_calls_named<'a>(block: &'a syn::Block, name: &'a str) -> Vec<&'a syn::ExprMethodCall> {
    struct NamedMethods<'a> {
        name: &'a str,
        calls: Vec<&'a syn::ExprMethodCall>,
    }
    impl<'a, 'ast: 'a> Visit<'ast> for NamedMethods<'a> {
        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            if call.method == self.name {
                self.calls.push(call);
            }
            visit::visit_expr_method_call(self, call);
        }
    }
    let mut calls = NamedMethods {
        name,
        calls: Vec::new(),
    };
    calls.visit_block(block);
    calls.calls
}

fn exact_descriptor_getter(
    expression: &syn::Expr,
    getter: &str,
    bindings: &BTreeMap<String, DescriptorIndex>,
) -> Option<DescriptorIndex> {
    match expression {
        syn::Expr::MethodCall(call) if call.method == getter && call.args.is_empty() => {
            indexed_pratt_operator(&call.receiver).or_else(|| {
                expression_path_name(&call.receiver)
                    .and_then(|binding| bindings.get(&binding).cloned())
            })
        }
        syn::Expr::MethodCall(call)
            if call.method == "expect"
                && call.args.len() == 1
                && matches!(&call.args[0], syn::Expr::Lit(literal)
                    if matches!(literal.lit, syn::Lit::Str(_))) =>
        {
            exact_descriptor_getter(&call.receiver, getter, bindings)
        }
        syn::Expr::Paren(paren) => exact_descriptor_getter(&paren.expr, getter, bindings),
        syn::Expr::Group(group) => exact_descriptor_getter(&group.expr, getter, bindings),
        _ => None,
    }
}

fn descriptor_getter_uses(
    expression: &syn::Expr,
    getter: &str,
    bindings: &BTreeMap<String, DescriptorIndex>,
) -> Vec<DescriptorIndex> {
    struct GetterUses<'a> {
        getter: &'a str,
        bindings: &'a BTreeMap<String, DescriptorIndex>,
        rows: Vec<DescriptorIndex>,
    }
    impl<'a, 'ast> Visit<'ast> for GetterUses<'a> {
        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            if call.method == self.getter
                && call.args.is_empty()
                && let Some(row) = indexed_pratt_operator(&call.receiver).or_else(|| {
                    expression_path_name(&call.receiver)
                        .and_then(|binding| self.bindings.get(&binding).cloned())
                })
            {
                self.rows.push(row);
            }
            visit::visit_expr_method_call(self, call);
        }
    }
    let mut uses = GetterUses {
        getter,
        bindings,
        rows: Vec::new(),
    };
    uses.visit_expr(expression);
    uses.rows
}

fn pratt_table_indices(block: &syn::Block) -> Vec<usize> {
    #[derive(Default)]
    struct TableIndices(Vec<usize>);
    impl<'ast> Visit<'ast> for TableIndices {
        fn visit_expr_index(&mut self, index: &'ast syn::ExprIndex) {
            if matches!(
                index.expr.as_ref(),
                syn::Expr::Path(path) if path.path.is_ident("PRATT_TABLES")
            ) {
                self.0.push(
                    integer_literal(&index.index)
                        .expect("canonical PRATT_TABLES access uses its literal table ordinal"),
                );
            }
            visit::visit_expr_index(self, index);
        }
    }
    let mut indices = TableIndices::default();
    indices.visit_block(block);
    indices.0
}

fn assignment_descriptor_value(
    expression: &syn::Expr,
    bindings: &BTreeMap<String, DescriptorIndex>,
) -> Option<DescriptorIndex> {
    if let Some(value) = integer_literal(expression) {
        return Some(DescriptorIndex::Literal(value));
    }
    if let syn::Expr::Call(call) = expression
        && direct_path(&call.func).is_some_and(|path| path.is_ident("Some"))
        && call.args.len() == 1
    {
        return assignment_descriptor_value(&call.args[0], bindings);
    }
    if let syn::Expr::MethodCall(call) = expression
        && call.method == "id"
        && call.args.is_empty()
    {
        return indexed_pratt_operator(&call.receiver).or_else(|| {
            expression_path_name(&call.receiver).and_then(|binding| bindings.get(&binding).cloned())
        });
    }
    None
}

fn descriptor_assignments(
    expression: &syn::Expr,
    bindings: &BTreeMap<String, DescriptorIndex>,
) -> BTreeMap<String, Vec<DescriptorIndex>> {
    struct Assignments<'a> {
        bindings: &'a BTreeMap<String, DescriptorIndex>,
        rows: BTreeMap<String, Vec<DescriptorIndex>>,
    }
    impl<'a, 'ast> Visit<'ast> for Assignments<'a> {
        fn visit_expr_assign(&mut self, assignment: &'ast syn::ExprAssign) {
            if let Some(target) = expression_path_name(&assignment.left)
                && let Some(row) = assignment_descriptor_value(&assignment.right, self.bindings)
            {
                self.rows.entry(target).or_default().push(row);
            }
            visit::visit_expr_assign(self, assignment);
        }
    }
    let mut assignments = Assignments {
        bindings,
        rows: BTreeMap::new(),
    };
    assignments.visit_expr(expression);
    assignments.rows
}

#[derive(Debug)]
struct PrattProbe {
    matcher: String,
    in_loop: bool,
    eligibility: Vec<DescriptorIndex>,
    matched_statement_count: usize,
    exact_mismatch_fallthrough: bool,
    matched_assignments: BTreeMap<String, Vec<DescriptorIndex>>,
}

fn pratt_probes(
    block: &syn::Block,
    matcher_names: &BTreeSet<String>,
    bindings: &BTreeMap<String, DescriptorIndex>,
    min_bp: &str,
) -> Vec<PrattProbe> {
    struct Probes<'a> {
        matcher_names: &'a BTreeSet<String>,
        bindings: &'a BTreeMap<String, DescriptorIndex>,
        min_bp: &'a str,
        loop_depth: usize,
        eligibility: Vec<Vec<DescriptorIndex>>,
        rows: Vec<PrattProbe>,
    }
    impl Probes<'_> {
        fn condition_eligibility(&self, condition: &syn::Expr) -> Vec<DescriptorIndex> {
            let syn::Expr::Binary(binary) = condition else {
                return Vec::new();
            };
            let (getter, floor) = match binary.op {
                syn::BinOp::Ge(_) => (binary.left.as_ref(), binary.right.as_ref()),
                syn::BinOp::Le(_) => (binary.right.as_ref(), binary.left.as_ref()),
                _ => return Vec::new(),
            };
            if !exact_path(floor, self.min_bp) {
                return Vec::new();
            }
            exact_descriptor_getter(getter, "left_binding_power", self.bindings)
                .into_iter()
                .collect()
        }
    }
    impl<'ast> Visit<'ast> for Probes<'_> {
        fn visit_expr_if(&mut self, expression: &'ast syn::ExprIf) {
            let eligible = self.condition_eligibility(&expression.cond);
            self.eligibility.push(eligible);
            self.visit_block(&expression.then_branch);
            self.eligibility.pop();
            if let Some((_, alternate)) = &expression.else_branch {
                self.visit_expr(alternate);
            }
        }

        fn visit_expr_loop(&mut self, expression: &'ast syn::ExprLoop) {
            self.loop_depth += 1;
            visit::visit_expr_loop(self, expression);
            self.loop_depth -= 1;
        }

        fn visit_expr_while(&mut self, expression: &'ast syn::ExprWhile) {
            self.loop_depth += 1;
            visit::visit_expr_while(self, expression);
            self.loop_depth -= 1;
        }

        fn visit_expr_for_loop(&mut self, expression: &'ast syn::ExprForLoop) {
            self.loop_depth += 1;
            visit::visit_expr_for_loop(self, expression);
            self.loop_depth -= 1;
        }

        fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
            if let syn::Expr::Call(call) = expression.expr.as_ref()
                && let Some(path) = direct_path(&call.func)
                && path.segments.len() == 1
            {
                let matcher = path.segments[0].ident.to_string();
                if self.matcher_names.contains(&matcher) {
                    assert_exact_call_arguments(call, &["run", "limits"], &matcher);
                    assert_eq!(
                        expression
                            .arms
                            .iter()
                            .map(|arm| {
                                assert!(arm.guard.is_none(), "Pratt probe arms are unguarded");
                                pattern_variant(&arm.pat).expect("plain Pratt Step arm")
                            })
                            .collect::<Vec<_>>(),
                        ["Matched", "Mismatch", "Fault"],
                        "Pratt probe uses the exact Step partition"
                    );
                    assert!(
                        !expression_definitely_exits(&expression.arms[0].body),
                        "a successful Pratt probe remains on its selected path"
                    );
                    assert!(
                        !expression_definitely_exits(&expression.arms[1].body),
                        "a Pratt probe mismatch falls through to the next manifest row"
                    );
                    assert!(
                        expression_definitely_exits(&expression.arms[2].body),
                        "a Pratt probe fault exits the table transaction"
                    );
                    self.rows.push(PrattProbe {
                        matcher,
                        in_loop: self.loop_depth != 0,
                        eligibility: self.eligibility.iter().flatten().cloned().collect(),
                        matched_statement_count: match expression.arms[0].body.as_ref() {
                            syn::Expr::Block(block) => block.block.stmts.len(),
                            _ => 0,
                        },
                        exact_mismatch_fallthrough: exact_empty_block(&expression.arms[1].body),
                        matched_assignments: descriptor_assignments(
                            &expression.arms[0].body,
                            self.bindings,
                        ),
                    });
                }
            }
            visit::visit_expr_match(self, expression);
        }
    }
    let mut probes = Probes {
        matcher_names,
        bindings,
        min_bp,
        loop_depth: 0,
        eligibility: Vec::new(),
        rows: Vec::new(),
    };
    probes.visit_block(block);
    probes.rows
}

fn assert_probe_selects(
    probe: &PrattProbe,
    selected: &DescriptorIndex,
    operator: usize,
    prefix: bool,
) {
    let DescriptorIndex::Binding(selected) = selected else {
        panic!("canonical Pratt selection retains a descriptor-row ordinal binding");
    };
    assert_eq!(
        probe.matched_statement_count, 1,
        "{} successful probe contains only its exact descriptor selection",
        probe.matcher
    );
    assert!(
        probe.exact_mismatch_fallthrough,
        "{} mismatch is the exact empty fallthrough to the next manifest row",
        probe.matcher
    );
    assert_eq!(
        probe
            .matched_assignments
            .get(selected)
            .cloned()
            .unwrap_or_default(),
        [DescriptorIndex::Literal(operator)],
        "{} successful probe selects exact PRATT_OPERATORS[{operator}]",
        probe.matcher
    );
    assert_eq!(
        probe.in_loop,
        !prefix,
        "{} executes in the canonical {} phase",
        probe.matcher,
        if prefix { "prefix" } else { "postfix/infix" }
    );
    assert_eq!(
        probe.eligibility,
        if prefix {
            Vec::new()
        } else {
            vec![DescriptorIndex::Literal(operator)]
        },
        "{} has exactly its row-owned left-binding-power/min_bp eligibility guard",
        probe.matcher
    );
}

fn expression_abandons_block(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Return(_) | syn::Expr::Break(_) | syn::Expr::Continue(_) => true,
        syn::Expr::Block(block) => block
            .block
            .stmts
            .last()
            .and_then(statement_expression)
            .is_some_and(expression_abandons_block),
        syn::Expr::Paren(paren) => expression_abandons_block(&paren.expr),
        syn::Expr::Group(group) => expression_abandons_block(&group.expr),
        _ => false,
    }
}

fn exact_empty_block(expression: &syn::Expr) -> bool {
    matches!(
        expression,
        syn::Expr::Block(block)
            if block.attrs.is_empty()
                && block.label.is_none()
                && block.block.stmts.is_empty()
    )
}

fn exact_bare_break(expression: &syn::Expr) -> bool {
    matches!(
        expression,
        syn::Expr::Break(break_) if break_.label.is_none() && break_.expr.is_none()
    ) || matches!(
        expression,
        syn::Expr::Block(block)
            if matches!(
                block.block.stmts.as_slice(),
                [syn::Stmt::Expr(syn::Expr::Break(break_), _)]
                    if break_.label.is_none() && break_.expr.is_none()
            )
    )
}

fn exact_forward_step_return(expression: &syn::Expr) -> bool {
    if let syn::Expr::Block(block) = expression {
        let [syn::Stmt::Expr(expression, _)] = block.block.stmts.as_slice() else {
            return false;
        };
        return exact_forward_step_return(expression);
    }
    let syn::Expr::Return(returned) = expression else {
        return false;
    };
    let Some(syn::Expr::Call(step)) = returned.expr.as_deref() else {
        return false;
    };
    let Some(variant) = path_terminal(&step.func) else {
        return false;
    };
    matches!(variant.as_str(), "Mismatch" | "Fault")
        && step.args.len() == 1
        && expression_path_name(&step.args[0]).is_some()
}

fn exact_unit_matched_pattern(pattern: &syn::Pat) -> bool {
    matches!(
        pattern,
        syn::Pat::TupleStruct(pattern)
            if path_segments(&pattern.path).as_slice() == ["Step", "Matched"]
                && pattern.elems.len() == 1
                && matches!(&pattern.elems[0], syn::Pat::Tuple(unit) if unit.elems.is_empty())
    )
}

fn exact_step_payload_binding(pattern: &syn::Pat, expected: &str) -> Option<String> {
    let syn::Pat::TupleStruct(pattern) = pattern else {
        return None;
    };
    if path_segments(&pattern.path).as_slice() != ["Step", expected] {
        return None;
    }
    if pattern.elems.len() != 1 {
        return None;
    }
    let syn::Pat::Ident(binding) = &pattern.elems[0] else {
        return None;
    };
    (binding.by_ref.is_none() && binding.mutability.is_none() && binding.subpat.is_none())
        .then(|| binding.ident.to_string())
}

fn exact_same_step_return(expression: &syn::Expr, variant: &str, payload: &str) -> bool {
    if let syn::Expr::Block(block) = expression {
        if !block.attrs.is_empty() || block.label.is_some() {
            return false;
        }
        let [syn::Stmt::Expr(expression, _)] = block.block.stmts.as_slice() else {
            return false;
        };
        return exact_same_step_return(expression, variant, payload);
    }
    let syn::Expr::Return(returned) = expression else {
        return false;
    };
    if !returned.attrs.is_empty() {
        return false;
    }
    let Some(syn::Expr::Call(step)) = returned.expr.as_deref() else {
        return false;
    };
    if !step.attrs.is_empty() {
        return false;
    }
    let Some(path) = direct_path(&step.func) else {
        return false;
    };
    path_segments(path).as_slice() == ["Step", variant]
        && step.args.len() == 1
        && exact_path(&step.args[0], payload)
}

fn assert_exact_recursive_pratt_step_match<'a>(
    expression: &'a syn::ExprMatch,
    owner: &str,
) -> &'a syn::ExprCall {
    let call = direct_call(&expression.expr, owner)
        .unwrap_or_else(|| panic!("{owner} recursion is a direct Step match"));
    assert_eq!(
        expression.arms.len(),
        3,
        "{owner} recursion has exactly Matched, Mismatch, and Fault arms"
    );
    let [matched, mismatch, fault] = expression.arms.as_slice() else {
        unreachable!();
    };
    assert!(
        expression
            .arms
            .iter()
            .all(|arm| arm.attrs.is_empty() && arm.guard.is_none()),
        "{owner} recursive Step arms are unadorned and unguarded"
    );
    assert!(
        exact_unit_matched_pattern(&matched.pat) && exact_empty_block(&matched.body),
        "{owner} recursive Matched(()) arm is the exact empty continuation"
    );
    let mismatch_payload = exact_step_payload_binding(&mismatch.pat, "Mismatch")
        .unwrap_or_else(|| panic!("{owner} recursive mismatch binds its exact failure"));
    assert!(
        exact_same_step_return(&mismatch.body, "Mismatch", &mismatch_payload),
        "{owner} recursive mismatch returns the same failure"
    );
    let fault_payload = exact_step_payload_binding(&fault.pat, "Fault")
        .unwrap_or_else(|| panic!("{owner} recursive fault binds its exact fault"));
    assert!(
        exact_same_step_return(&fault.body, "Fault", &fault_payload),
        "{owner} recursive fault returns the same fault"
    );
    call
}

fn recursive_pratt_step_matches<'a>(block: &'a syn::Block, owner: &str) -> Vec<&'a syn::ExprMatch> {
    struct RecursiveMatches<'name, 'ast> {
        owner: &'name str,
        matches: Vec<&'ast syn::ExprMatch>,
    }
    impl<'name, 'ast> Visit<'ast> for RecursiveMatches<'name, 'ast> {
        fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
            if direct_call(&expression.expr, self.owner).is_some() {
                self.matches.push(expression);
            }
            visit::visit_expr_match(self, expression);
        }
    }
    let mut matches = RecursiveMatches {
        owner,
        matches: Vec::new(),
    };
    matches.visit_block(block);
    matches.matches
}

fn indexed_pratt_table(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Index(index)
            if matches!(
                index.expr.as_ref(),
                syn::Expr::Path(path) if path.path.is_ident("PRATT_TABLES")
            ) =>
        {
            integer_literal(&index.index).is_some()
        }
        syn::Expr::Reference(reference) => indexed_pratt_table(&reference.expr),
        syn::Expr::Paren(paren) => indexed_pratt_table(&paren.expr),
        syn::Expr::Group(group) => indexed_pratt_table(&group.expr),
        _ => false,
    }
}

fn assert_live_pratt_authority(
    block: &syn::Block,
    owner: &str,
    descriptor_bindings: &BTreeMap<String, DescriptorIndex>,
    min_bp: &str,
    selections: &BTreeSet<String>,
) {
    struct LivePratt<'a> {
        owner: &'a str,
        descriptor_bindings: &'a BTreeMap<String, DescriptorIndex>,
        min_bp: &'a str,
        selections: &'a BTreeSet<String>,
        loops: usize,
        loop_depth: usize,
        allow_break: usize,
        selection_initializers: BTreeMap<String, usize>,
        violations: BTreeSet<&'static str>,
    }
    impl LivePratt<'_> {
        fn selected_path(&self, expression: &syn::Expr) -> bool {
            expression_path_name(expression).is_some_and(|name| self.selections.contains(&name))
        }

        fn selected_some_pattern(pattern: &syn::Pat) -> bool {
            matches!(
                pattern,
                syn::Pat::TupleStruct(pattern)
                    if pattern.path.segments.last().is_some_and(|segment| segment.ident == "Some")
                        && pattern.elems.len() == 1
                        && pat_ident(&pattern.elems[0]).is_some()
            )
        }

        fn selection_condition(&self, expression: &syn::Expr) -> bool {
            match expression {
                syn::Expr::MethodCall(call)
                    if matches!(call.method.to_string().as_str(), "is_none" | "is_some")
                        && call.args.is_empty() =>
                {
                    self.selected_path(&call.receiver)
                }
                syn::Expr::Let(let_) => {
                    Self::selected_some_pattern(&let_.pat) && self.selected_path(&let_.expr)
                }
                _ => false,
            }
        }

        fn eligibility_condition(&self, expression: &syn::Expr) -> bool {
            let syn::Expr::Binary(binary) = expression else {
                return false;
            };
            let (getter, floor) = match binary.op {
                syn::BinOp::Ge(_) => (binary.left.as_ref(), binary.right.as_ref()),
                syn::BinOp::Le(_) => (binary.right.as_ref(), binary.left.as_ref()),
                _ => return false,
            };
            exact_path(floor, self.min_bp)
                && exact_descriptor_getter(getter, "left_binding_power", self.descriptor_bindings)
                    .is_some()
        }

        fn fixity_condition(&self, expression: &syn::Expr) -> bool {
            let syn::Expr::Binary(binary) = expression else {
                return false;
            };
            if !matches!(binary.op, syn::BinOp::Eq(_) | syn::BinOp::Ne(_)) {
                return false;
            }
            for (getter, variant) in [
                (binary.left.as_ref(), binary.right.as_ref()),
                (binary.right.as_ref(), binary.left.as_ref()),
            ] {
                if exact_descriptor_getter(getter, "fixity", self.descriptor_bindings).is_some()
                    && path_terminal(variant).is_some_and(|variant| {
                        matches!(variant.as_str(), "Prefix" | "Postfix" | "Infix")
                    })
                {
                    return true;
                }
            }
            false
        }

        fn direct_step_match(&self, expression: &syn::ExprMatch) -> bool {
            let syn::Expr::Call(call) = expression.expr.as_ref() else {
                return false;
            };
            let Some(path) = direct_path(&call.func) else {
                return false;
            };
            let Some(name) = path
                .segments
                .len()
                .eq(&1)
                .then(|| path.segments[0].ident.to_string())
            else {
                return false;
            };
            if !(name.starts_with("rule_") || name == self.owner || name == "emit_event") {
                return false;
            }
            expression
                .arms
                .iter()
                .map(|arm| {
                    arm.guard
                        .is_none()
                        .then(|| pattern_variant(&arm.pat))
                        .flatten()
                })
                .collect::<Option<Vec<_>>>()
                .is_some_and(|variants| variants == ["Matched", "Mismatch", "Fault"])
        }

        fn selection_match(&self, expression: &syn::ExprMatch) -> bool {
            self.selected_path(&expression.expr)
                && expression.arms.len() == 2
                && expression.arms.iter().all(|arm| arm.guard.is_none())
                && expression
                    .arms
                    .iter()
                    .map(|arm| pattern_variant(&arm.pat))
                    .collect::<Option<BTreeSet<_>>>()
                    .is_some_and(|variants| {
                        variants == BTreeSet::from(["None".to_owned(), "Some".to_owned()])
                    })
        }

        fn fixity_match(&self, expression: &syn::ExprMatch) -> bool {
            exact_descriptor_getter(&expression.expr, "fixity", self.descriptor_bindings).is_some()
                && expression.arms.len() == 3
                && expression.arms.iter().all(|arm| arm.guard.is_none())
                && expression
                    .arms
                    .iter()
                    .map(|arm| pattern_variant(&arm.pat))
                    .collect::<Option<BTreeSet<_>>>()
                    .is_some_and(|variants| {
                        variants
                            == BTreeSet::from([
                                "Prefix".to_owned(),
                                "Postfix".to_owned(),
                                "Infix".to_owned(),
                            ])
                    })
        }

        fn canonical_local(&self, local: &syn::Local) -> bool {
            let Some(initializer) = &local.init else {
                return false;
            };
            let selected_bindings = pattern_binding_names(&local.pat)
                .into_iter()
                .filter(|binding| self.selections.contains(binding))
                .collect::<Vec<_>>();
            if selected_bindings.len() == 1
                && initializer.diverge.is_none()
                && matches!(
                    initializer.expr.as_ref(),
                    syn::Expr::Path(path) if path.path.is_ident("None")
                )
            {
                return true;
            }
            if let Some((_, diverge)) = &initializer.diverge {
                return Self::selected_some_pattern(&local.pat)
                    && self.selected_path(&initializer.expr)
                    && exact_bare_break(diverge);
            }
            pat_ident(&local.pat).is_some()
                && (exact_cursor_offset(&initializer.expr)
                    || indexed_pratt_operator(&initializer.expr).is_some()
                    || indexed_pratt_table(&initializer.expr))
        }

        fn canonical_assignment(&self, assignment: &syn::ExprAssign) -> bool {
            expression_path_name(&assignment.left)
                .is_some_and(|target| self.selections.contains(&target))
                && assignment_descriptor_value(&assignment.right, self.descriptor_bindings)
                    .is_some()
        }

        fn canonical_statement(&self, statement: &syn::Stmt) -> bool {
            match statement {
                syn::Stmt::Local(local) => self.canonical_local(local),
                syn::Stmt::Item(_) | syn::Stmt::Macro(_) => false,
                syn::Stmt::Expr(expression, _) => match expression {
                    syn::Expr::If(expression) => {
                        !expression.then_branch.stmts.is_empty()
                            && expression
                                .else_branch
                                .as_ref()
                                .is_none_or(|(_, alternate)| {
                                    !matches!(
                                        alternate.as_ref(),
                                        syn::Expr::Block(block) if block.block.stmts.is_empty()
                                    )
                                })
                            && (self.selection_condition(&expression.cond)
                                || self.eligibility_condition(&expression.cond)
                                || self.fixity_condition(&expression.cond))
                    }
                    syn::Expr::Match(expression) => {
                        self.direct_step_match(expression)
                            || self.selection_match(expression)
                            || self.fixity_match(expression)
                    }
                    syn::Expr::Loop(_) => true,
                    syn::Expr::Assign(assignment) => self.canonical_assignment(assignment),
                    syn::Expr::Break(_) => self.allow_break != 0 && exact_bare_break(expression),
                    syn::Expr::Continue(_) => false,
                    syn::Expr::Return(_) => exact_forward_step_return(expression),
                    _ => exact_unit_matched(expression),
                },
            }
        }
    }
    impl<'ast> Visit<'ast> for LivePratt<'_> {
        fn visit_expr_if(&mut self, expression: &'ast syn::ExprIf) {
            if !(self.selection_condition(&expression.cond)
                || self.eligibility_condition(&expression.cond)
                || self.fixity_condition(&expression.cond))
            {
                self.violations.insert("unrecognized-pratt-if");
            }
            if expression.then_branch.stmts.is_empty()
                || expression
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, alternate)| {
                        matches!(
                            alternate.as_ref(),
                            syn::Expr::Block(block) if block.block.stmts.is_empty()
                        )
                    })
            {
                self.violations.insert("empty-pratt-control-branch");
            }
            visit::visit_expr_if(self, expression);
        }

        fn visit_expr_match(&mut self, expression: &'ast syn::ExprMatch) {
            let direct_step = self.direct_step_match(expression);
            let selection = self.selection_match(expression);
            let fixity = self.fixity_match(expression);
            if !(direct_step || selection || fixity) {
                self.violations.insert("unrecognized-pratt-match");
            }
            if direct_step {
                for arm in &expression.arms {
                    let valid = match pattern_variant(&arm.pat).as_deref() {
                        Some("Matched") => matches!(arm.body.as_ref(), syn::Expr::Block(_)),
                        Some("Mismatch") => {
                            exact_empty_block(&arm.body) || exact_forward_step_return(&arm.body)
                        }
                        Some("Fault") => exact_forward_step_return(&arm.body),
                        _ => false,
                    };
                    if !valid {
                        self.violations.insert("noncanonical-pratt-step-arm");
                    }
                }
            } else if selection || fixity {
                for arm in &expression.arms {
                    if !matches!(arm.body.as_ref(), syn::Expr::Block(_))
                        && !(self.loop_depth != 0
                            && pattern_variant(&arm.pat).as_deref() == Some("None")
                            && exact_bare_break(&arm.body))
                    {
                        self.violations.insert("noncanonical-pratt-control-arm");
                    }
                }
            }
            self.visit_expr(&expression.expr);
            for arm in &expression.arms {
                if let Some((_, guard)) = &arm.guard {
                    self.visit_expr(guard);
                }
                let canonical_loop_exit = self.loop_depth != 0
                    && self.selection_match(expression)
                    && pattern_variant(&arm.pat).as_deref() == Some("None")
                    && exact_bare_break(&arm.body);
                if canonical_loop_exit {
                    self.allow_break += 1;
                    self.visit_expr(&arm.body);
                    self.allow_break -= 1;
                } else {
                    self.visit_expr(&arm.body);
                }
            }
        }

        fn visit_expr_loop(&mut self, expression: &'ast syn::ExprLoop) {
            self.loops += 1;
            self.loop_depth += 1;
            self.visit_block(&expression.body);
            self.loop_depth -= 1;
        }

        fn visit_expr_while(&mut self, expression: &'ast syn::ExprWhile) {
            self.violations.insert("noncanonical-pratt-while");
            visit::visit_expr_while(self, expression);
        }

        fn visit_expr_for_loop(&mut self, expression: &'ast syn::ExprForLoop) {
            self.violations.insert("noncanonical-pratt-for");
            visit::visit_expr_for_loop(self, expression);
        }

        fn visit_expr_break(&mut self, expression: &'ast syn::ExprBreak) {
            if self.allow_break == 0
                || self.loop_depth == 0
                || expression.label.is_some()
                || expression.expr.is_some()
            {
                self.violations.insert("noncanonical-pratt-break");
            }
            visit::visit_expr_break(self, expression);
        }

        fn visit_expr_continue(&mut self, expression: &'ast syn::ExprContinue) {
            self.violations.insert("noncanonical-pratt-continue");
            visit::visit_expr_continue(self, expression);
        }

        fn visit_local(&mut self, local: &'ast syn::Local) {
            let selected_bindings = pattern_binding_names(&local.pat)
                .into_iter()
                .filter(|binding| self.selections.contains(binding))
                .collect::<Vec<_>>();
            if !selected_bindings.is_empty() {
                let canonical = selected_bindings.len() == 1
                    && local.init.as_ref().is_some_and(|initializer| {
                        initializer.diverge.is_none()
                            && matches!(
                                initializer.expr.as_ref(),
                                syn::Expr::Path(path) if path.path.is_ident("None")
                            )
                    });
                if canonical {
                    *self
                        .selection_initializers
                        .entry(selected_bindings[0].clone())
                        .or_insert(0) += 1;
                } else {
                    self.violations
                        .insert("noncanonical-pratt-selection-initializer");
                }
            }
            if let Some(initializer) = &local.init
                && let Some((_, diverge)) = &initializer.diverge
            {
                let canonical = Self::selected_some_pattern(&local.pat)
                    && self.selected_path(&initializer.expr)
                    && matches!(
                        diverge.as_ref(),
                        syn::Expr::Block(block)
                            if matches!(
                                block.block.stmts.as_slice(),
                                [syn::Stmt::Expr(syn::Expr::Break(break_), _)]
                                    if break_.expr.is_none()
                            )
                    );
                if !canonical {
                    self.violations.insert("unrecognized-pratt-let-else");
                }
                self.visit_expr(&initializer.expr);
                if canonical {
                    self.allow_break += 1;
                    self.visit_expr(diverge);
                    self.allow_break -= 1;
                } else {
                    self.visit_expr(diverge);
                }
                return;
            }
            visit::visit_local(self, local);
        }

        fn visit_block(&mut self, block: &'ast syn::Block) {
            let mut abandoned = false;
            for statement in &block.stmts {
                if abandoned {
                    self.violations.insert("authority-after-control-transfer");
                }
                if !self.canonical_statement(statement) {
                    self.violations.insert("noncanonical-pratt-statement");
                }
                self.visit_stmt(statement);
                abandoned |= statement_expression(statement).is_some_and(expression_abandons_block);
            }
        }
    }

    let mut live = LivePratt {
        owner,
        descriptor_bindings,
        min_bp,
        selections,
        loops: 0,
        loop_depth: 0,
        allow_break: 0,
        selection_initializers: BTreeMap::new(),
        violations: BTreeSet::new(),
    };
    live.visit_block(block);
    assert_eq!(
        live.loops, 1,
        "{owner} has exactly one live canonical postfix/infix loop"
    );
    for selection in selections {
        assert_eq!(
            live.selection_initializers
                .get(selection)
                .copied()
                .unwrap_or(0),
            1,
            "{owner} initializes selected descriptor ordinal `{selection}` to None exactly once on its live lexical path"
        );
    }
    assert!(
        live.violations.is_empty(),
        "{owner} contains only recognized live Pratt control structure: {:?}",
        live.violations
    );
}

fn assert_pratt_functions(
    file: &syn::File,
    module: &CompiledModule,
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
) {
    let expected = module
        .lowering()
        .pratt_tables()
        .iter()
        .enumerate()
        .map(|(index, _)| format!("pratt_{index}"))
        .collect::<BTreeSet<_>>();
    let actual = functions
        .keys()
        .filter(|name| name.starts_with("pratt_"))
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_eq!(actual, expected, "one canonical callable per Pratt table");
    assert_pratt_descriptors(file, module);

    for (table_index, table) in module.lowering().pratt_tables().iter().enumerate() {
        let name = format!("pratt_{table_index}");
        let function = unique_function(functions, &name);
        let _ = assert_work_guard_and_tail_only(function);
        let arguments = function_argument_names(function);
        assert_eq!(arguments.len(), 3, "{name}(run, limits, min_bp)");
        let min_bp = &arguments[2];
        let transaction = guarded_transaction_block(function);
        let (start, start_initializer) = transaction
            .stmts
            .first()
            .and_then(local_binding)
            .unwrap_or_else(|| panic!("{name} transaction begins by capturing its reduced start"));
        assert!(
            exact_cursor_offset(start_initializer),
            "{name} reduced start is its entry cursor"
        );
        assert!(
            transaction
                .stmts
                .last()
                .and_then(statement_expression)
                .is_some_and(exact_unit_matched),
            "{name} returned transaction has one live successful tail"
        );
        assert_parser_primitive_counts(function, [("sequence", 1)]);
        let mut calls = DirectCalls::default();
        calls.visit_block(transaction);
        let descriptor_bindings = pratt_descriptor_bindings(transaction);
        let recursive_matches = recursive_pratt_step_matches(transaction, &name);
        assert_eq!(
            recursive_matches.len(),
            2,
            "{name} has exactly prefix and RHS recursive Step matches"
        );
        let recursive_authorities = recursive_matches
            .iter()
            .map(|recursive_match| {
                let recursive =
                    assert_exact_recursive_pratt_step_match(recursive_match, &name);
                assert_eq!(
                    recursive.args.len(),
                    3,
                    "{name} recursive call uses run, limits, and a descriptor-derived binding power"
                );
                assert!(
                    exact_path(&recursive.args[0], "run")
                        && exact_path(&recursive.args[1], "limits"),
                    "{name} recursion retains its exact current run and limits"
                );
                exact_descriptor_getter(
                    &recursive.args[2],
                    "right_binding_power",
                    &descriptor_bindings,
                )
                .unwrap_or_else(|| {
                    panic!(
                        "{name} recursive argument is the selected row's exact right_binding_power().expect(...)"
                    )
                })
            })
            .collect::<Vec<_>>();
        let [prefix_selection, loop_selection] = recursive_authorities.as_slice() else {
            unreachable!();
        };
        let selection_assignments = descriptor_assignments(
            &syn::Expr::Block(syn::ExprBlock {
                attrs: Vec::new(),
                label: None,
                block: transaction.clone(),
            }),
            &descriptor_bindings,
        );
        let mut selections = selection_assignments
            .keys()
            .cloned()
            .collect::<BTreeSet<_>>();
        for selection in [prefix_selection, loop_selection] {
            if let DescriptorIndex::Binding(selection) = selection {
                selections.insert(selection.clone());
            }
        }
        assert_live_pratt_authority(
            transaction,
            &name,
            &descriptor_bindings,
            min_bp,
            &selections,
        );
        let atom = format!("rule_{}", table.atom_rule().index());
        assert_eq!(call_count(transaction, &atom), 1, "{name} exact atom");
        let atom_calls = direct_calls_named(transaction, &atom);
        let [atom_call] = atom_calls.as_slice() else {
            panic!("{name} has one exact atom call");
        };
        assert_exact_call_arguments(atom_call, &["run", "limits"], &atom);
        let mut prefix_matchers = Vec::new();
        let mut loop_matchers = Vec::new();
        let mut expected_rows = Vec::new();
        let mut expected_control_counts =
            BTreeMap::from([(name.clone(), 2usize), (atom.clone(), 1)]);
        for operator in table.operators() {
            let operator = module
                .lowering()
                .pratt_operator(operator)
                .expect("operator belongs to table");
            let matcher = format!("rule_{}", operator.matcher_rule().index());
            assert_eq!(
                call_count(transaction, &matcher),
                1,
                "{name} exact matcher dispatch"
            );
            let prefix = format!("{:?}", operator.fixity()).eq_ignore_ascii_case("prefix");
            if prefix {
                prefix_matchers.push(matcher.clone());
            } else {
                loop_matchers.push(matcher.clone());
            }
            *expected_control_counts.entry(matcher.clone()).or_insert(0) += 1;
            expected_rows.push((operator.id().index(), matcher, prefix));
        }
        assert_eq!(
            selection_assignments.values().map(Vec::len).sum::<usize>(),
            expected_rows.len(),
            "{name} selects exactly one descriptor ordinal for every manifest operator row and no extra or dead row"
        );
        let actual_control_counts = direct_internal_calls(transaction)
            .into_iter()
            .filter(|call| {
                call.starts_with("rule_") || call.starts_with("node_") || call.starts_with("pratt_")
            })
            .fold(BTreeMap::new(), |mut counts, call| {
                *counts.entry(call).or_insert(0usize) += 1;
                counts
            });
        assert_eq!(
            actual_control_counts, expected_control_counts,
            "{name} has no unrecorded, alternate, or dead parser-control call"
        );
        prefix_matchers.extend(loop_matchers);
        let matcher_names = prefix_matchers.iter().cloned().collect::<BTreeSet<_>>();
        let actual_matchers = calls
            .calls
            .iter()
            .filter_map(|path| {
                (path.len() == 1 && matcher_names.contains(&path[0])).then(|| path[0].clone())
            })
            .collect::<Vec<_>>();
        assert_exact_call_order(
            &actual_matchers,
            &prefix_matchers,
            &format!("{name} prefix-then-loop matcher probes"),
        );
        let probes = pratt_probes(transaction, &matcher_names, &descriptor_bindings, min_bp);
        assert_eq!(
            probes
                .iter()
                .map(|probe| probe.matcher.clone())
                .collect::<Vec<_>>(),
            prefix_matchers,
            "{name} probe control follows the exact manifest row order"
        );
        for (operator, matcher, prefix) in expected_rows {
            let probe = probes
                .iter()
                .find(|probe| probe.matcher == matcher)
                .expect("every descriptor row owns one exact matcher probe");
            assert_probe_selects(
                probe,
                if prefix {
                    prefix_selection
                } else {
                    loop_selection
                },
                operator,
                prefix,
            );
        }
        assert!(
            calls.paths.contains("PRATT_TABLES") && calls.paths.contains("PRATT_OPERATORS"),
            "{name} executes the exported descriptor authorities"
        );
        assert_eq!(
            pratt_table_indices(transaction),
            [table_index],
            "{name} executes only its exact PRATT_TABLES[{table_index}] descriptor"
        );
        let mut events = EventIdentities::default();
        events.visit_block(transaction);
        let event_authority = assert_pratt_event_operator_authority(transaction);
        let reductions = event_calls_named(transaction, "PrattReduce");
        let [reduction] = reductions.as_slice() else {
            panic!("{name} owns one PrattReduce event");
        };
        let end = expression_path_name(&reduction.args[4])
            .unwrap_or_else(|| panic!("{name} PrattReduce end is a cursor binding"));
        assert!(
            exact_path(&reduction.args[3], &start)
                && end != start
                && exact_zero(&reduction.args[5]),
            "{name} PrattReduce spans from the exact expression entry through commit and reserves value 0"
        );
        assert!(
            cursor_binding_dominates_event(transaction, reduction, &end),
            "{name} PrattReduce end cursor is captured after the selected operator commits"
        );
        assert_eq!(
            event_authority,
            loop_selection.clone(),
            "{name} PrattReduce derives from the same selected loop descriptor as RHS recursion"
        );
        assert_eq!(
            descriptor_getter_uses(
                &syn::Expr::Block(syn::ExprBlock {
                    attrs: Vec::new(),
                    label: None,
                    block: transaction.clone(),
                }),
                "fixity",
                &descriptor_bindings,
            ),
            [loop_selection.clone()],
            "{name} chooses postfix versus infix from the exact selected loop descriptor"
        );
        let pratt_nodes = module
            .lowering()
            .nodes()
            .iter()
            .enumerate()
            .filter_map(|(node, plan)| match plan.op() {
                PlanOp::Pratt { atom, table } if table.index() == table_index => Some((node, atom)),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(pratt_nodes.len(), 1, "one PlanOp site for `{name}`");
        assert_eq!(
            pratt_nodes[0].1,
            table.atom_rule(),
            "{name} PlanOp retains the table's exact atom authority"
        );
        assert_eq!(
            events.rows,
            [(
                "PrattReduce".to_owned(),
                [None, Some(pratt_nodes[0].0), None, None],
                EventOperator::DescriptorId,
            )],
            "{name} owns only one exact PrattReduce constructor"
        );
    }
}

fn referenced_constant(expression: &syn::Expr) -> Option<String> {
    match expression {
        syn::Expr::Path(_) => path_terminal(expression),
        syn::Expr::Reference(reference) => referenced_constant(&reference.expr),
        syn::Expr::Group(group) => referenced_constant(&group.expr),
        syn::Expr::Paren(paren) => referenced_constant(&paren.expr),
        _ => None,
    }
}

fn top_level_expression<'a>(file: &'a syn::File, name: &str) -> Option<&'a syn::Expr> {
    let matches = file
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Const(item) if item.ident == name => Some(item.expr.as_ref()),
            syn::Item::Static(item) if item.ident == name => Some(item.expr.as_ref()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(matches.len() <= 1, "at most one canonical `{name}`");
    matches.first().copied()
}

fn regex_plan_constructor(expression: &syn::Expr) -> Option<&syn::ExprCall> {
    match expression {
        syn::Expr::Call(call) => {
            let path = direct_path(&call.func)?;
            let segments = path_segments(path);
            (segments.as_slice() == ["RegexPlan", "new"]).then_some(call)
        }
        syn::Expr::Match(expression) => regex_plan_constructor(&expression.expr),
        syn::Expr::Group(group) => regex_plan_constructor(&group.expr),
        syn::Expr::Paren(paren) => regex_plan_constructor(&paren.expr),
        _ => None,
    }
}

fn regex_plan_declarations(file: &syn::File) -> BTreeSet<String> {
    file.items
        .iter()
        .filter_map(|item| {
            let (name, type_) = match item {
                syn::Item::Const(item) => (item.ident.to_string(), item.ty.as_ref()),
                syn::Item::Static(item) => (item.ident.to_string(), item.ty.as_ref()),
                _ => return None,
            };
            let syn::Type::Path(type_) = type_ else {
                return None;
            };
            type_
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "RegexPlan")
                .then_some(name)
        })
        .collect()
}

fn regex_plan_constructor_count(file: &syn::File) -> usize {
    #[derive(Default)]
    struct Constructors(usize);
    impl<'ast> Visit<'ast> for Constructors {
        fn visit_expr_call(&mut self, expression: &'ast syn::ExprCall) {
            if direct_path(&expression.func)
                .is_some_and(|path| path_segments(path).as_slice() == ["RegexPlan", "new"])
            {
                self.0 += 1;
            }
            visit::visit_expr_call(self, expression);
        }
    }
    let mut constructors = Constructors::default();
    constructors.visit_file(file);
    constructors.0
}

fn assert_regex_plan_definition_closure(file: &syn::File, regex_count: usize, has_trivia: bool) {
    let mut expected = (0..regex_count)
        .map(|index| format!("REGEX_{index}_PLAN"))
        .collect::<BTreeSet<_>>();
    if has_trivia {
        expected.insert("TRIVIA_PLAN".to_owned());
    }
    assert_eq!(
        regex_plan_declarations(file),
        expected,
        "generated v6 has only the lowering-owned ordinary and singleton trivia RegexPlans"
    );
    assert_eq!(
        regex_plan_constructor_count(file),
        regex_count + usize::from(has_trivia),
        "every generated RegexPlan is constructed exactly once at its canonical top-level owner"
    );
}

fn array_integers(file: &syn::File, name: &str) -> Vec<usize> {
    top_level_array(file, name)
        .elems
        .iter()
        .map(|value| integer_literal(value).expect("integer DFA table cell"))
        .collect()
}

fn array_bools(file: &syn::File, name: &str) -> Vec<bool> {
    top_level_array(file, name)
        .elems
        .iter()
        .map(|value| match value {
            syn::Expr::Lit(literal) => match &literal.lit {
                syn::Lit::Bool(value) => value.value,
                _ => panic!("boolean DFA table cell"),
            },
            _ => panic!("boolean DFA table cell"),
        })
        .collect()
}

fn mutate_first_array_integer(source: &str, name: &str) -> String {
    let declaration = format!("const {name}:");
    let start = source
        .find(&declaration)
        .unwrap_or_else(|| panic!("mutation fixture missing `{name}`"));
    let array = source[start..]
        .find("= [")
        .map(|offset| start + offset + 3)
        .expect("array initializer");
    let number_start = source[array..]
        .find(|character: char| character.is_ascii_digit())
        .map(|offset| array + offset)
        .expect("first array integer");
    let number_end = source[number_start..]
        .find(|character: char| !character.is_ascii_digit())
        .map(|offset| number_start + offset)
        .expect("array integer terminator");
    let value: usize = source[number_start..number_end]
        .parse()
        .expect("array integer");
    let mut mutated = source.to_owned();
    mutated.replace_range(number_start..number_end, &(value + 1).to_string());
    mutated
}

fn assert_trivia_plan(file: &syn::File, module: &CompiledModule, mixed_regex: bool) {
    let trivia = module.lowering().trivia();
    assert_regex_plan_definition_closure(file, module.lowering().regexes().len(), trivia.is_some());
    let plan = top_level_expression(file, "TRIVIA_PLAN");
    match trivia {
        None => {
            assert!(plan.is_none(), "null trivia emits no TRIVIA_PLAN");
            let mut calls = DirectCalls::default();
            calls.visit_file(file);
            assert!(
                !calls.paths.contains("Trivia"),
                "null trivia emits no Trivia constructor"
            );
        }
        Some(trivia) => {
            let plan = plan.expect("one canonical TRIVIA_PLAN");
            let constructor =
                regex_plan_constructor(plan).expect("TRIVIA_PLAN is one RegexPlan::new");
            assert_eq!(constructor.args.len(), 6, "complete RegexPlan fields");
            let byte_classes =
                referenced_constant(&constructor.args[0]).expect("trivia byte-class table");
            let transitions =
                referenced_constant(&constructor.args[4]).expect("trivia transition table");
            let accepting =
                referenced_constant(&constructor.args[5]).expect("trivia accepting table");
            assert_eq!(
                array_integers(file, &byte_classes),
                trivia
                    .byte_classes()
                    .iter()
                    .map(|value| usize::from(*value))
                    .collect::<Vec<_>>()
            );
            assert_eq!(
                integer_literal(&constructor.args[1]),
                Some(trivia.state_count())
            );
            assert_eq!(
                integer_literal(&constructor.args[2]),
                Some(trivia.class_count())
            );
            assert_eq!(
                integer_literal(&constructor.args[3]),
                Some(trivia.start_state())
            );
            assert_eq!(
                array_integers(file, &transitions),
                trivia
                    .transitions()
                    .iter()
                    .map(|value| *value as usize)
                    .collect::<Vec<_>>()
            );
            assert_eq!(array_bools(file, &accepting), trivia.accepting());
            if mixed_regex {
                assert!(
                    file.items.iter().any(|item| match item {
                        syn::Item::Const(item) => {
                            let name = item.ident.to_string();
                            name.starts_with("REGEX_") && name.ends_with("_PLAN")
                        }
                        syn::Item::Static(item) => {
                            let name = item.ident.to_string();
                            name.starts_with("REGEX_") && name.ends_with("_PLAN")
                        }
                        _ => false,
                    }),
                    "mixed fixture keeps ordinary REGEX_N_PLAN distinct from TRIVIA_PLAN"
                );
            }
        }
    }
}

fn canonical_graph(
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
) -> BTreeMap<String, BTreeSet<String>> {
    let identities = functions.keys().cloned().collect::<BTreeSet<_>>();
    functions
        .iter()
        .map(|(name, functions)| {
            let mut calls = DirectCalls::default();
            calls.visit_block(&functions[0].block);
            let targets = calls
                .calls
                .into_iter()
                .filter_map(|path| {
                    if path.len() != 1 {
                        return None;
                    }
                    identities.contains(&path[0]).then(|| path[0].clone())
                })
                .collect();
            (name.clone(), targets)
        })
        .collect()
}

fn reachable(graph: &BTreeMap<String, BTreeSet<String>>, root: &str) -> BTreeSet<String> {
    let mut reached = BTreeSet::new();
    let mut pending = VecDeque::from([root.to_owned()]);
    while let Some(name) = pending.pop_front() {
        if !reached.insert(name.clone()) {
            continue;
        }
        if let Some(callees) = graph.get(&name) {
            pending.extend(callees.iter().cloned());
        }
    }
    reached
}

fn assert_no_noncanonical_authority(functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    for (name, definitions) in functions {
        if canonical_control_name(name) {
            continue;
        }
        assert!(
            matches!(name.as_str(), "event" | "identity" | "resource_fault"),
            "generated v6 contains noncanonical free helper `{name}`"
        );
        for definition in definitions {
            let mut calls = DirectCalls::default();
            calls.visit_block(&definition.block);
            assert!(
                calls.calls.iter().all(|path| {
                    path.last().map_or(true, |called| {
                        !canonical_control_name(called) && !parser_primitive_name(called)
                    })
                }),
                "noncanonical free function `{name}` owns parser control"
            );
            let mut resources = ResourceShape::default();
            resources.visit_block(&definition.block);
            assert_eq!(
                (
                    resources.work,
                    resources.nesting,
                    resources.gets,
                    resources.sets,
                ),
                (0, 0, 0, 0),
                "noncanonical free function `{name}` owns resource control"
            );
        }
    }
}

fn assert_single_entry_rule(boundary: &syn::ItemFn, entry: &str) {
    assert_eq!(
        direct_internal_calls(&boundary.block)
            .into_iter()
            .filter(|name| name.starts_with("rule_"))
            .collect::<Vec<_>>(),
        [entry.to_owned()],
        "public boundary has exactly one parser root"
    );
}

fn assert_public_roots(module: &CompiledModule, functions: &BTreeMap<String, Vec<&syn::ItemFn>>) {
    for (root, limited) in [
        ("check", "check_with_limits"),
        ("parse_events", "parse_events_with_limits"),
    ] {
        let root_function = unique_function(functions, root);
        assert_eq!(
            direct_internal_calls(&root_function.block),
            [limited.to_owned()],
            "{root} has one value path"
        );
        assert!(
            direct_call(function_tail(root_function), limited).is_some(),
            "{root} returns its limited root directly"
        );
        let root_call = direct_call(function_tail(root_function), limited)
            .expect("public convenience root returns its limited root");
        assert!(
            root_call
                .args
                .first()
                .is_some_and(|argument| exact_path(argument, "input")),
            "{root} forwards its exact current input"
        );
        let limited_function = unique_function(functions, limited);
        assert_eq!(
            direct_internal_calls(&limited_function.block),
            ["parse_with_sink".to_owned()],
            "{limited} has one value path"
        );
        assert!(
            direct_call(function_tail(limited_function), "parse_with_sink").is_some(),
            "{limited} returns parse_with_sink directly"
        );
    }
    let boundary = unique_function(functions, "parse_with_sink");
    let boundary_arguments = function_argument_names(boundary);
    let [input, sink, _config] = boundary_arguments.as_slice() else {
        panic!("parse_with_sink(input, sink, config)");
    };
    let syn::Expr::MethodCall(tail) = function_tail(boundary) else {
        panic!("parse_with_sink returns Run::parse_module directly");
    };
    assert_eq!(tail.method, "parse_module");
    let parse_module = method_calls_named(&boundary.block, "parse_module");
    assert_eq!(
        parse_module.len(),
        1,
        "public boundary invokes Run::parse_module once"
    );
    let parse_module = parse_module[0];
    let syn::Expr::Call(receiver) = parse_module.receiver.as_ref() else {
        panic!("parse_module receiver is Run::with_sink(input, sink)");
    };
    assert!(
        direct_path(&receiver.func)
            .is_some_and(|path| path_segments(path).as_slice() == ["Run", "with_sink"])
            && receiver.args.len() == 2,
        "public boundary owns the exact Run::with_sink(...).parse_module(...) crossing"
    );
    assert!(
        exact_path(&receiver.args[0], input) && exact_path(&receiver.args[1], sink),
        "Run::with_sink receives the exact public input and sink"
    );
    assert_eq!(
        parse_module.args.len(),
        1,
        "parse_module has one immediate parser"
    );
    let parser =
        closure_expression(&parse_module.args[0]).expect("parse_module parser is immediate");
    assert_eq!(
        closure_input_name(parser, "parse_module"),
        "run",
        "parse_module callback binds the current run"
    );
    let parser = closure_block(parser).expect("parse_module parser owns a block");
    let Some(syn::Stmt::Expr(sequence_tail, None)) = parser.stmts.last() else {
        panic!("parse_module callback returns its full-consumption transaction");
    };
    let sequence =
        direct_call(sequence_tail, "sequence").expect("parse_module callback tail is sequence");
    assert_eq!(sequence.args.len(), 2);
    assert!(
        exact_path(&sequence.args[0], "run"),
        "public full-consumption transaction receives the current run"
    );
    let full_parser =
        closure_expression(&sequence.args[1]).expect("full-consumption parser is immediate");
    assert_eq!(
        closure_input_name(full_parser, "public sequence"),
        "run",
        "public transaction callback binds the current run"
    );
    let full_parser = closure_block(full_parser).expect("full-consumption parser owns a block");
    assert_eq!(call_count(&boundary.block, "sequence"), 1);
    assert_eq!(call_count(&boundary.block, "eof"), 1);
    assert_parser_primitive_counts(boundary, [("eof", 1), ("sequence", 1)]);
    let entry = format!("rule_{}", module.lowering().entry().index());
    assert_eq!(call_count(&boundary.block, &entry), 1);
    assert_eq!(call_count(parser, "sequence"), 1);
    assert_eq!(call_count(parser, "eof"), 1);
    assert_eq!(call_count(parser, &entry), 1);
    assert_eq!(call_count(full_parser, "eof"), 1);
    assert_eq!(call_count(full_parser, &entry), 1);
    let entry_calls = direct_calls_named(full_parser, &entry);
    let [entry_call] = entry_calls.as_slice() else {
        panic!("public transaction has one exact entry call");
    };
    assert!(
        entry_call.args.len() == 2
            && exact_path(&entry_call.args[0], "run")
            && exact_reference_to(&entry_call.args[1], "limits"),
        "public entry receives the current run and exact bounded limits"
    );
    let eof_calls = direct_calls_named(full_parser, "eof");
    let [eof_call] = eof_calls.as_slice() else {
        panic!("public transaction has one eof call");
    };
    assert_exact_call_arguments(eof_call, &["run"], "public eof");
    assert_single_entry_rule(boundary, &entry);
    assert_eq!(
        event_rows(boundary),
        [(
            "EndInput".to_owned(),
            [None, None, None, None],
            EventOperator::None,
        )],
        "public boundary owns only the exact EndInput event"
    );
    let end_inputs = event_calls_named(full_parser, "EndInput");
    let [end_input] = end_inputs.as_slice() else {
        panic!("public boundary owns one EndInput event");
    };
    let at = expression_path_name(&end_input.args[3])
        .unwrap_or_else(|| panic!("EndInput start is the terminal cursor binding"));
    assert_event_span_value_paths(end_input, &at, &at, 0, "EndInput");
    assert!(
        cursor_binding_dominates_event(full_parser, end_input, &at),
        "EndInput captures its exact cursor only after eof commits"
    );
}

fn assert_pratt_control_edges(
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
    graph: &BTreeMap<String, BTreeSet<String>>,
    public: &BTreeSet<String>,
    owner: &str,
    site_node: &str,
    site_rule: &str,
    selected: bool,
) {
    assert!(
        reachable(graph, site_node).contains(owner),
        "{owner} is reachable from its exact site adapter"
    );
    let callers = functions
        .iter()
        .filter_map(|(name, definitions)| {
            let mut calls = DirectCalls::default();
            calls.visit_block(&definitions[0].block);
            calls
                .calls
                .iter()
                .any(|path| path.len() == 1 && path[0] == owner)
                .then(|| name.clone())
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        callers,
        BTreeSet::from([site_node.to_owned(), owner.to_owned()]),
        "{owner} has only its exact adapter and recursion as callers"
    );
    if selected {
        assert!(
            public.contains(site_rule) && public.contains(owner),
            "dependency-reachable Pratt site and owner occur on the public-root path"
        );
    }
}

fn assert_global_runtime_resource_authority(file: &syn::File) {
    let mut global_resources = RuntimeResourceAuthority::default();
    global_resources.visit_file(file);
    assert_eq!(
        global_resources,
        RuntimeResourceAuthority {
            work_gets: 1,
            work_sets: 1,
            depth_gets: 1,
            depth_sets: 2,
            work_kinds: 1,
            nesting_kinds: 1,
            ..RuntimeResourceAuthority::default()
        },
        "the entire generated item/impl/nested-body closure has only the exact charge_work and with_depth Work/Nesting authority"
    );
}

#[derive(Debug, Default, Eq, PartialEq)]
struct GlobalGeneratedAuthority {
    limits_calls: usize,
    limits_references: usize,
    noncanonical_limits_calls: usize,
    limits_structs: usize,
    vec_calls: usize,
    vec_references: usize,
    noncanonical_vec_calls: usize,
    event_calls: usize,
    event_references: usize,
    identity_calls: usize,
    identity_references: usize,
    scalar_event_constructors: usize,
}

fn qself_type_terminal(qself: &syn::QSelf) -> Option<String> {
    let syn::Type::Path(path) = qself.ty.as_ref() else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn limits_constructor_path(path: &syn::ExprPath) -> bool {
    (path.qself.is_none() && path_segments(&path.path).as_slice() == ["Limits", "new"])
        || (path.qself.as_ref().and_then(qself_type_terminal).as_deref() == Some("Limits")
            && path
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "new"))
}

fn exact_limits_constructor_path(path: &syn::ExprPath) -> bool {
    path.qself.is_none() && path_segments(&path.path).as_slice() == ["Limits", "new"]
}

fn vec_constructor_path(path: &syn::ExprPath) -> bool {
    let segments = path_segments(&path.path);
    (path.qself.is_none()
        && segments.len() >= 2
        && segments[segments.len() - 2..] == ["Vec", "new"])
        || (path.qself.as_ref().and_then(qself_type_terminal).as_deref() == Some("Vec")
            && path
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "new"))
}

fn exact_vec_constructor_path(path: &syn::ExprPath) -> bool {
    path.qself.is_none()
        && path.path.segments.len() == 2
        && path
            .path
            .segments
            .iter()
            .zip(["Vec", "new"])
            .all(|(segment, expected)| {
                segment.ident == expected && matches!(&segment.arguments, syn::PathArguments::None)
            })
}

fn exact_free_path(path: &syn::ExprPath, expected: &str) -> bool {
    path.qself.is_none() && path.path.is_ident(expected)
}

fn generated_named_path(path: &syn::ExprPath, expected: &str) -> bool {
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == expected)
}

impl<'ast> Visit<'ast> for GlobalGeneratedAuthority {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref() {
            if exact_limits_constructor_path(path) {
                self.limits_calls += 1;
            } else if limits_constructor_path(path) {
                self.noncanonical_limits_calls += 1;
            }
            if exact_vec_constructor_path(path) {
                self.vec_calls += 1;
            } else if vec_constructor_path(path) {
                self.noncanonical_vec_calls += 1;
            }
            self.event_calls += usize::from(exact_free_path(path, "event"));
            self.identity_calls += usize::from(exact_free_path(path, "identity"));
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
        self.limits_references += usize::from(limits_constructor_path(path));
        self.vec_references += usize::from(vec_constructor_path(path));
        self.event_references += usize::from(generated_named_path(path, "event"));
        self.identity_references += usize::from(generated_named_path(path, "identity"));
        visit::visit_expr_path(self, path);
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        self.limits_structs += usize::from(
            expression
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "Limits"),
        );
        self.scalar_event_constructors += usize::from(
            expression
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "ScalarEvent"),
        );
        visit::visit_expr_struct(self, expression);
    }
}

fn top_level_generated_authority(
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
) -> GlobalGeneratedAuthority {
    let mut authority = GlobalGeneratedAuthority::default();
    for definitions in functions.values() {
        for function in definitions {
            authority.visit_block(&function.block);
        }
    }
    authority
}

fn assert_event_buffer_new_authority(file: &syn::File) {
    let matches = file
        .items
        .iter()
        .filter_map(|item| {
            let syn::Item::Impl(item) = item else {
                return None;
            };
            if item.trait_.is_some() || impl_self_type(item).as_deref() != Some("EventBuffer") {
                return None;
            }
            item.items.iter().find_map(|member| match member {
                syn::ImplItem::Fn(method) if method.sig.ident == "new" => Some(method),
                _ => None,
            })
        })
        .collect::<Vec<_>>();
    let [new] = matches.as_slice() else {
        panic!("one exact inherent EventBuffer::new implementation");
    };
    let inputs = new.sig.inputs.iter().collect::<Vec<_>>();
    let [syn::FnArg::Typed(source)] = inputs.as_slice() else {
        panic!("EventBuffer::new(source)");
    };
    let source =
        pat_ident(&source.pat).unwrap_or_else(|| panic!("EventBuffer::new binds one plain source"));
    assert_eq!(source, "source", "EventBuffer::new(source)");
    assert!(
        matches!(
            &new.sig.output,
            syn::ReturnType::Type(_, output)
                if matches!(
                    output.as_ref(),
                    syn::Type::Path(path)
                        if path.qself.is_none() && path.path.is_ident("Self")
                )
        ),
        "EventBuffer::new returns Self directly"
    );
    let [syn::Stmt::Expr(syn::Expr::Struct(value), None)] = new.block.stmts.as_slice() else {
        panic!("EventBuffer::new directly returns one closed struct initializer");
    };
    assert!(
        value.path.is_ident("Self") && value.rest.is_none(),
        "EventBuffer::new constructs only Self"
    );
    let fields = value
        .fields
        .iter()
        .map(|field| {
            let syn::Member::Named(name) = &field.member else {
                panic!("EventBuffer::new uses named fields");
            };
            (name.to_string(), &field.expr)
        })
        .collect::<BTreeMap<_, _>>();
    assert_eq!(
        fields.keys().map(String::as_str).collect::<BTreeSet<_>>(),
        BTreeSet::from(["events", "revision", "source"]),
        "EventBuffer::new initializes its exact field closure"
    );
    assert_eq!(
        fields.len(),
        value.fields.len(),
        "EventBuffer::new initializes every field once"
    );
    assert!(
        exact_path(fields["revision"], "REVISION") && exact_path(fields["source"], &source),
        "EventBuffer::new retains the canonical revision and exact source"
    );
    let syn::Expr::Call(events) = fields["events"] else {
        panic!("EventBuffer::new initializes events with Vec::new()");
    };
    let syn::Expr::Path(constructor) = events.func.as_ref() else {
        panic!("EventBuffer::new calls Vec::new directly");
    };
    assert!(
        exact_vec_constructor_path(constructor) && events.args.is_empty(),
        "EventBuffer::new owns the sole exact zero-argument Vec::new call"
    );
}

fn assert_event_buffer_push_authority(file: &syn::File) {
    let matches = file
        .items
        .iter()
        .filter_map(|item| {
            let syn::Item::Impl(item) = item else {
                return None;
            };
            if impl_self_type(item).as_deref() != Some("EventBuffer")
                || item
                    .trait_
                    .as_ref()
                    .and_then(|(_, path, _)| path.segments.last())
                    .is_none_or(|segment| segment.ident != "EventSink")
            {
                return None;
            }
            item.items.iter().find_map(|member| match member {
                syn::ImplItem::Fn(method) if method.sig.ident == "push" => Some(method),
                _ => None,
            })
        })
        .collect::<Vec<_>>();
    let [push] = matches.as_slice() else {
        panic!("one exact EventSink::push implementation for EventBuffer");
    };
    let inputs = push.sig.inputs.iter().collect::<Vec<_>>();
    let [syn::FnArg::Receiver(receiver), syn::FnArg::Typed(event)] = inputs.as_slice() else {
        panic!("EventBuffer::push(&mut self, event)");
    };
    assert!(
        receiver.reference.is_some() && receiver.mutability.is_some(),
        "EventBuffer::push owns one mutable receiver"
    );
    let event =
        pat_ident(&event.pat).unwrap_or_else(|| panic!("EventBuffer::push binds one plain event"));
    let [statement] = push.block.stmts.as_slice() else {
        panic!("EventBuffer::push has one exact publication statement");
    };
    let expression =
        statement_expression(statement).expect("EventBuffer::push publication expression");
    let publication = exact_method_call(expression, &["self", "events"], "push")
        .expect("EventBuffer::push publishes into its exact event vector");
    assert!(
        publication.args.len() == 1 && exact_path(&publication.args[0], &event),
        "EventBuffer::push publishes only its exact event argument"
    );
}

fn assert_global_generated_authority(
    file: &syn::File,
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
) {
    let mut global = GlobalGeneratedAuthority::default();
    global.visit_file(file);
    let top_level = top_level_generated_authority(functions);

    assert_eq!(
        (
            global.limits_calls,
            global.limits_references,
            global.noncanonical_limits_calls,
            global.limits_structs,
        ),
        (1, 1, 0, 0),
        "the complete free/impl/nested generated source owns exactly one direct Limits::new call and no alias, qualified, or direct-struct constructor"
    );
    assert_eq!(
        top_level.limits_calls, global.limits_calls,
        "the sole Limits::new authority remains in the top-level parse boundary"
    );
    assert_eq!(
        (
            global.vec_calls,
            global.vec_references,
            global.noncanonical_vec_calls,
        ),
        (1, 1, 0),
        "the complete generated source owns exactly one direct Vec::new call/reference and no alias, qualified, or qself constructor"
    );
    assert_event_buffer_new_authority(file);

    assert_event_buffer_push_authority(file);
    assert_eq!(
        (
            global.event_calls,
            global.identity_calls,
            global.scalar_event_constructors,
        ),
        (
            top_level.event_calls,
            top_level.identity_calls,
            top_level.scalar_event_constructors,
        ),
        "impl, trait, const, static, and nested bodies own no scalar-event construction or event/identity call authority"
    );
    assert_eq!(
        global.event_calls, global.identity_calls,
        "every scalar event has exactly one immediate EventIdentity authority"
    );
    assert_eq!(
        global.event_references,
        global.event_calls + 2,
        "the only non-callee `event` value paths are emit_event's callback and EventBuffer::push's exact argument"
    );
    assert_eq!(
        global.identity_references,
        global.identity_calls + 4,
        "the only non-callee `identity` value paths are the event helper's four exact identity fields"
    );
    assert_eq!(
        global.scalar_event_constructors, 1,
        "ScalarEvent is constructed exactly once, inside the canonical `event` helper"
    );
}

fn assert_global_control_ownership(
    file: &syn::File,
    module: &CompiledModule,
    functions: &BTreeMap<String, Vec<&syn::ItemFn>>,
) {
    assert_no_noncanonical_authority(functions);
    for (name, definitions) in functions {
        for function in definitions {
            let expected_resources = match name.as_str() {
                "charge_work" => RuntimeResourceAuthority {
                    work_gets: 1,
                    work_sets: 1,
                    work_kinds: 1,
                    ..RuntimeResourceAuthority::default()
                },
                "with_depth" => RuntimeResourceAuthority {
                    depth_gets: 1,
                    depth_sets: 2,
                    nesting_kinds: 1,
                    ..RuntimeResourceAuthority::default()
                },
                _ => RuntimeResourceAuthority::default(),
            };
            assert_eq!(
                runtime_resource_authority(function),
                expected_resources,
                "`{name}` has only its exact Work/Nesting counter and fault-kind authority"
            );
            assert_eq!(
                method_calls_named(&function.block, "parse_module").len(),
                usize::from(name == "parse_with_sink"),
                "Run::parse_module is confined to the exact parse_with_sink boundary"
            );
            let mut calls = DirectCalls::default();
            calls.visit_block(&function.block);
            assert_eq!(
                calls
                    .calls
                    .iter()
                    .filter(|path| path.as_slice() == ["Run", "with_sink"])
                    .count(),
                usize::from(name == "parse_with_sink"),
                "Run::with_sink is confined to the exact parse_with_sink boundary"
            );
        }
    }
    assert_global_runtime_resource_authority(file);

    let difference_nodes = module
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .filter_map(|(node, plan)| {
            matches!(plan.op(), PlanOp::Difference { .. }).then_some(format!("node_{node}"))
        })
        .collect::<BTreeSet<_>>();
    let trim_nodes = module
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .filter_map(|(node, plan)| {
            matches!(plan.op(), PlanOp::Trim { .. }).then_some(format!("node_{node}"))
        })
        .collect::<BTreeSet<_>>();
    struct TriviaPlanUses(usize);
    impl<'ast> Visit<'ast> for TriviaPlanUses {
        fn visit_expr_path(&mut self, path: &'ast syn::ExprPath) {
            self.0 += usize::from(path.path.is_ident("TRIVIA_PLAN"));
            visit::visit_expr_path(self, path);
        }
    }
    for (name, functions) in functions {
        let count = call_count(&functions[0].block, "difference");
        assert_eq!(
            count,
            usize::from(difference_nodes.contains(name)),
            "Difference primitive ownership at `{name}`"
        );

        let mut events = EventIdentities::default();
        events.visit_block(&functions[0].block);
        assert_eq!(
            events.rows.len(),
            call_count(&functions[0].block, "event"),
            "every generated event constructor at `{name}` has the exact six-argument identity/operator/span/value surface"
        );
        assert!(
            events
                .rows
                .iter()
                .all(|(variant, _, operator)| { (variant == "PrattReduce") == operator.is_some() }),
            "operator is Some exactly for PrattReduce at `{name}`"
        );
        assert_eq!(
            events
                .rows
                .iter()
                .filter(|(variant, _, _)| variant == "Trivia")
                .count(),
            if trim_nodes.contains(name) { 2 } else { 0 },
            "Trivia event ownership at `{name}`"
        );
        let mut trivia_plan_uses = TriviaPlanUses(0);
        trivia_plan_uses.visit_block(&functions[0].block);
        assert_eq!(
            trivia_plan_uses.0,
            if trim_nodes.contains(name) { 2 } else { 0 },
            "TRIVIA_PLAN ownership at `{name}`"
        );
    }

    let pratt_names = module
        .lowering()
        .pratt_tables()
        .iter()
        .enumerate()
        .map(|(table, _)| format!("pratt_{table}"))
        .collect::<BTreeSet<_>>();
    for (name, functions) in functions {
        let mut calls = DirectCalls::default();
        calls.visit_block(&functions[0].block);
        if calls.paths.contains("PrattReduce") {
            assert!(
                pratt_names.contains(name),
                "PrattReduce exists only in its canonical table owner"
            );
        }
    }
    let actual_control = functions
        .keys()
        .filter(|name| canonical_control_name(name))
        .cloned()
        .collect::<BTreeSet<_>>();
    let mut expected_control = BTreeSet::from([
        "charge_work".to_owned(),
        "with_depth".to_owned(),
        "emit_event".to_owned(),
        "parse_with_sink".to_owned(),
        "check".to_owned(),
        "check_with_limits".to_owned(),
        "parse_events".to_owned(),
        "parse_events_with_limits".to_owned(),
    ]);
    expected_control.extend(
        module
            .lowering()
            .rules()
            .iter()
            .enumerate()
            .map(|(index, _)| format!("rule_{index}")),
    );
    expected_control.extend(
        module
            .lowering()
            .nodes()
            .iter()
            .enumerate()
            .map(|(index, _)| format!("node_{index}")),
    );
    expected_control.extend(pratt_names);
    assert_eq!(
        actual_control, expected_control,
        "canonical control identity closure"
    );

    let graph = canonical_graph(functions);
    let public = reachable(&graph, "check")
        .union(&reachable(&graph, "parse_events"))
        .cloned()
        .collect::<BTreeSet<_>>();
    let mut selected_rules = BTreeSet::new();
    let mut pending_rules = VecDeque::from([module.lowering().entry()]);
    while let Some(rule) = pending_rules.pop_front() {
        if !selected_rules.insert(rule.index()) {
            continue;
        }
        pending_rules.extend(
            module
                .dependency()
                .graph()
                .dependencies(rule)
                .expect("lowered rule belongs to dependency solution"),
        );
    }
    for (table, descriptor) in module.lowering().pratt_tables().iter().enumerate() {
        let owner = format!("pratt_{table}");
        let site_nodes = module
            .lowering()
            .nodes()
            .iter()
            .enumerate()
            .filter_map(|(node, plan)| match plan.op() {
                PlanOp::Pratt { atom: _, table } if table.index() == descriptor.id().index() => {
                    Some(format!("node_{node}"))
                }
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        assert_eq!(
            site_nodes.len(),
            1,
            "Pratt table has one exact site adapter"
        );
        let site_node = site_nodes
            .into_iter()
            .next()
            .expect("one exact Pratt site adapter");
        let site_rule = format!("rule_{}", descriptor.site_rule().index());
        assert_pratt_control_edges(
            functions,
            &graph,
            &public,
            &owner,
            &site_node,
            &site_rule,
            selected_rules.contains(&descriptor.site_rule().index()),
        );
    }
}

fn assert_generated_manifest_counts(module: &CompiledModule) {
    let raw = module.generated().manifest();
    let manifest: serde_json::Value =
        serde_json::from_str(raw).expect("generated-v6 manifest JSON");
    let object = manifest.as_object().expect("generated-v6 manifest object");
    assert_eq!(
        object.keys().map(String::as_str).collect::<BTreeSet<_>>(),
        BTreeSet::from([
            "event_schema",
            "fields",
            "nodes",
            "pratt_operators",
            "pratt_tables",
            "regexes",
            "revision",
            "rules",
            "schema",
            "source_blake3",
            "source_bytes",
            "transforms",
            "trivia_dfas",
        ]),
        "generated-v6 manifest key closure"
    );
    for (name, count) in [
        ("nodes", module.lowering().nodes().len()),
        ("rules", module.lowering().rules().len()),
        ("regexes", module.lowering().regexes().len()),
        ("transforms", module.events().transforms().len()),
        ("fields", module.lowering().fields().len()),
        (
            "trivia_dfas",
            usize::from(module.lowering().trivia().is_some()),
        ),
        ("pratt_tables", module.lowering().pratt_tables().len()),
        ("pratt_operators", module.lowering().pratt_operators().len()),
    ] {
        assert_eq!(
            object[name].as_u64(),
            Some(count as u64),
            "generated-v6 manifest `{name}` carrier count"
        );
    }
    assert_eq!(
        object["schema"].as_str(),
        Some("bbnf.generated-rust.v6"),
        "generated-v6 manifest schema"
    );
    assert_eq!(
        object["event_schema"].as_str(),
        Some("bbnf.scalar-events.v5"),
        "generated-v6 manifest event schema"
    );
    assert_eq!(
        object["event_schema"].as_str(),
        Some(module.events().schema()),
        "generated-v6 manifest event carrier identity"
    );
    let revision = module.source_map().revision().hex();
    assert_eq!(
        object["revision"].as_str(),
        Some(revision.as_str()),
        "generated-v6 manifest revision"
    );
    let generated = module.generated().source();
    let source_blake3 = blake3::hash(generated.as_bytes()).to_hex().to_string();
    assert_eq!(
        object["source_blake3"].as_str(),
        Some(source_blake3.as_str()),
        "generated-v6 manifest source digest"
    );
    assert_eq!(
        object["source_bytes"].as_u64(),
        Some(generated.len() as u64),
        "generated-v6 manifest source byte count"
    );
    assert_eq!(
        serde_json::to_string(&manifest).expect("canonical generated manifest serialization"),
        raw,
        "generated-v6 manifest bytes are canonical JSON"
    );
}

fn assert_generated_v6_source(module: &CompiledModule, source: &str, mixed_regex: bool) {
    let file = syn::parse_file(source).expect("generated v6 is syntactic Rust");
    assert_canonical_surface(&file);
    let functions = top_level_functions(&file);
    assert_parser_primitive_provenance(&file, &functions);
    assert_global_generated_authority(&file, &functions);
    assert_canonical_resource_helpers(&functions);
    assert_event_emission_authority(&functions);
    assert_rule_functions(module, &functions);
    assert_node_functions(module, &functions);
    assert_pratt_functions(&file, module, &functions);
    assert_trivia_plan(&file, module, mixed_regex);
    assert_public_roots(module, &functions);
    assert_global_control_ownership(&file, module, &functions);
    assert_generated_manifest_counts(module);
}

fn assert_generated_v6_canonical(module: &CompiledModule, mixed_regex: bool) {
    assert_generated_v6_source(module, module.generated().source(), mixed_regex);
}

fn generated_function_range(source: &str, name: &str) -> std::ops::Range<usize> {
    let declaration = format!("fn {name}");
    let start = source
        .find(&declaration)
        .unwrap_or_else(|| panic!("generated mutation basis missing `{name}`"));
    assert!(
        source[start + declaration.len()..]
            .find(&declaration)
            .is_none(),
        "generated mutation basis has one `{name}` declaration"
    );
    let remainder = &source[start + declaration.len()..];
    let private = remainder.find("\nfn ");
    let public = remainder.find("\npub fn ");
    let end = [private, public]
        .into_iter()
        .flatten()
        .min()
        .map_or(source.len(), |offset| start + declaration.len() + offset);
    start..end
}

fn replace_once_in_generated_function(
    source: &str,
    function: &str,
    needle: &str,
    replacement: &str,
) -> String {
    let range = generated_function_range(source, function);
    let body = &source[range.clone()];
    assert_eq!(
        body.matches(needle).count(),
        1,
        "`{function}` mutation basis contains one `{needle}`"
    );
    let mutated = body.replacen(needle, replacement, 1);
    let mut product = source.to_owned();
    product.replace_range(range, &mutated);
    product
}

fn replace_first_in_generated_function(
    source: &str,
    function: &str,
    needle: &str,
    replacement: &str,
) -> String {
    let range = generated_function_range(source, function);
    let body = &source[range.clone()];
    assert!(
        body.contains(needle),
        "`{function}` mutation basis contains `{needle}`"
    );
    let mutated = body.replacen(needle, replacement, 1);
    let mut product = source.to_owned();
    product.replace_range(range, &mutated);
    product
}

fn replace_last_in_generated_function(
    source: &str,
    function: &str,
    needle: &str,
    replacement: &str,
) -> String {
    let range = generated_function_range(source, function);
    let body = &source[range.clone()];
    let offset = body
        .rfind(needle)
        .unwrap_or_else(|| panic!("`{function}` mutation basis contains `{needle}`"));
    let mut mutated = body.to_owned();
    mutated.replace_range(offset..offset + needle.len(), replacement);
    let mut product = source.to_owned();
    product.replace_range(range, &mutated);
    product
}

fn swap_once_in_generated_function(
    source: &str,
    function: &str,
    first: &str,
    second: &str,
) -> String {
    let range = generated_function_range(source, function);
    let body = &source[range.clone()];
    assert_eq!(
        body.matches(first).count(),
        1,
        "`{function}` first swap row"
    );
    assert_eq!(
        body.matches(second).count(),
        1,
        "`{function}` second swap row"
    );
    const PLACEHOLDER: &str = "__BBNF_W3_PRATT_SWAP_PLACEHOLDER__";
    assert!(!source.contains(PLACEHOLDER));
    let mutated = body
        .replacen(first, PLACEHOLDER, 1)
        .replacen(second, first, 1)
        .replacen(PLACEHOLDER, second, 1);
    let mut product = source.to_owned();
    product.replace_range(range, &mutated);
    product
}

fn mutate_pratt_event_descriptor(source: &str, function: &str) -> String {
    let range = generated_function_range(source, function);
    let body = &source[range.clone()];
    let event = body
        .find("EventKind::PrattReduce")
        .expect("PrattReduce mutation basis");
    let some = body[event..]
        .find("Some(")
        .map(|offset| event + offset)
        .expect("PrattReduce event must carry an operator");
    let receiver_start = some + "Some(".len();
    let id = body[receiver_start..]
        .find(".id())")
        .map(|offset| receiver_start + offset)
        .expect("PrattReduce descriptor id mutation basis");
    let mut mutated = body.to_owned();
    mutated.replace_range(receiver_start..id, "PRATT_OPERATORS[0]");
    let mut product = source.to_owned();
    product.replace_range(range, &mutated);
    product
}

fn mutate_event_argument(
    source: &str,
    function: &str,
    variant: &str,
    occurrence: usize,
    argument: usize,
    replacement: &str,
) -> String {
    let range = generated_function_range(source, function);
    let body = &source[range.clone()];
    let marker = format!("event(EventKind::{variant},");
    let event = body
        .match_indices(&marker)
        .nth(occurrence)
        .map(|(offset, _)| offset)
        .unwrap_or_else(|| {
            panic!("`{function}` mutation basis contains {variant} occurrence {occurrence}")
        });
    let open = event + "event".len();
    assert_eq!(&body[open..open + 1], "(");
    let mut depth = 1usize;
    let mut start = open + 1;
    let scan_start = start;
    let mut arguments = Vec::new();
    for (offset, byte) in body.as_bytes()[scan_start..].iter().enumerate() {
        let index = scan_start + offset;
        match *byte {
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth == 0 {
                    arguments.push(start..index);
                    break;
                }
            }
            b',' if depth == 1 => {
                arguments.push(start..index);
                start = index + 1;
            }
            _ => {}
        }
    }
    assert_eq!(
        arguments.len(),
        6,
        "{variant} mutation basis preserves the six event arguments"
    );
    let selected = arguments
        .get(argument)
        .unwrap_or_else(|| panic!("event argument {argument} exists"));
    let leading = body[selected.clone()]
        .len()
        .saturating_sub(body[selected.clone()].trim_start().len());
    let trailing = body[selected.clone()]
        .len()
        .saturating_sub(body[selected.clone()].trim_end().len());
    let selected = selected.start + leading..selected.end - trailing;
    let mut mutated = body.to_owned();
    mutated.replace_range(selected, replacement);
    let mut product = source.to_owned();
    product.replace_range(range, &mutated);
    product
}

fn assert_generated_v6_mutation_rejected(
    module: &CompiledModule,
    mutated: &str,
    mixed_regex: bool,
    label: &str,
) {
    syn::parse_file(mutated)
        .unwrap_or_else(|error| panic!("{label} mutation must remain syntactic Rust: {error}"));
    assert!(
        std::panic::catch_unwind(|| {
            assert_generated_v6_source(module, mutated, mixed_regex);
        })
        .is_err(),
        "canonical generated-source verifier accepted {label}"
    );
}

fn canonical_resource_fixture() -> &'static str {
    "fn resource_fault(run: R, kind: K, limit: usize) -> Step {
         Step::Fault(Fault::ResourceLimit { at: run.cursor(), kind, limit })
     }
     fn charge_work(run: R, limits: L) -> Step {
         let work = limits.work.get();
         if work >= limits.config.work {
             return resource_fault(run, ResourceKind::Work, limits.config.work);
         }
         limits.work.set(work + 1);
         Step::Matched(())
     }
     fn with_depth(run: R, limits: L, parser: P) -> Step {
         let depth = limits.depth.get();
         if depth >= limits.config.nesting {
             return resource_fault(run, ResourceKind::Nesting, limits.config.nesting);
         }
         limits.depth.set(depth + 1);
         let outcome = parser(run);
         limits.depth.set(depth);
         outcome
     }"
}

fn canonical_limits_fixture() -> &'static str {
    "struct LimitsConfig {
         input_bytes: usize,
         nesting: usize,
         logical_events: usize,
         work: usize,
     }
     impl Default for LimitsConfig {
         fn default() -> Self {
             Self {
                 input_bytes: MAX_INPUT_BYTES,
                 nesting: MAX_NESTING,
                 logical_events: MAX_LOGICAL_EVENTS,
                 work: MAX_WORK,
             }
         }
     }
     struct Limits {
         config: LimitsConfig,
         depth: Cell<usize>,
         work: Cell<usize>,
     }
     impl Limits {
         fn new(config: LimitsConfig) -> Self {
             Self {
                 config,
                 depth: Cell::new(0),
                 work: Cell::new(0),
             }
         }
     }"
}

fn canonical_global_authority_fixture() -> &'static str {
    "fn identity() {}
     fn event(identity: EventIdentity) {
         let _ = ScalarEvent {
             rule: identity.rule,
             node: identity.node,
             field: identity.field,
             transform: identity.transform,
         };
     }
     fn emit_event(event: ScalarEvent) {
         let _ = event;
     }
     fn parse_with_sink() {
         let _limits = Limits::new(config);
         event(identity());
     }
     struct EventBuffer;
     impl EventBuffer {
         fn new(source: Input) -> Self {
             Self {
                 revision: REVISION,
                 source,
                 events: Vec::new(),
             }
         }
         fn events(&self) {}
     }
     impl EventSink for EventBuffer {
         type Event = ScalarEvent;
         fn push(&mut self, event: Self::Event) {
             self.events.push(event);
         }
     }"
}

#[test]
fn generated_v6_canonical_verifier_rejects_control_indirection_and_decoys() {
    for (tag, source) in [
        (
            "parser-control-method",
            "struct X; impl X { fn parse(&self) { node_0(); } }",
        ),
        (
            "parser-control-method",
            "struct X; impl X { fn parse(&self) { literal(run, b\"x\"); } }",
        ),
        (
            "noncanonical-method-call",
            "fn node_0() { owner.hidden_parser(run); }",
        ),
        ("trait-dispatch", "trait Parser { fn parse(&self); }"),
        ("function-pointer", "fn f(parser: fn()) { parser(); }"),
        ("callable-field", "struct Stored<F: FnMut()> { parser: F }"),
        (
            "local-control-alias",
            "fn f() { let parser = node_0; parser(); }",
        ),
        (
            "callable-value",
            "fn f() { let parser = Some(node_0); parser.unwrap()(); }",
        ),
        ("stored-closure", "fn f() { let parser = || node_0(); }"),
        (
            "returned-closure",
            "fn f() -> impl Fn() { return || node_0(); }",
        ),
        (
            "imported-control-alias",
            "use parse_that::difference as manual_difference;",
        ),
        (
            "type-alias",
            "type HiddenLimits = Limits;
             fn hidden(config: LimitsConfig, depth: Cell<usize>, work: Cell<usize>) {
                 let _ = HiddenLimits { config, depth, work };
             }",
        ),
        (
            "type-alias",
            "type HiddenScalarEvent = ScalarEvent;
             fn hidden(
                 kind: EventKind,
                 rule: Option<u32>,
                 node: Option<u32>,
                 field: Option<u32>,
                 transform: Option<u32>,
                 operator: Option<u32>,
             ) {
                 let _ = HiddenScalarEvent {
                     kind,
                     rule,
                     node,
                     field,
                     transform,
                     operator,
                     start: 0,
                     end: 0,
                     value: 0,
                 };
             }",
        ),
        (
            "renamed-import",
            "use self::Limits as HiddenLimits;
             fn hidden(config: LimitsConfig, depth: Cell<usize>, work: Cell<usize>) {
                 let _ = HiddenLimits { config, depth, work };
             }",
        ),
        (
            "renamed-import",
            "use self::ScalarEvent as HiddenScalarEvent;
             fn hidden(
                 kind: EventKind,
                 rule: Option<u32>,
                 node: Option<u32>,
                 field: Option<u32>,
                 transform: Option<u32>,
                 operator: Option<u32>,
             ) {
                 let _ = HiddenScalarEvent {
                     kind,
                     rule,
                     node,
                     field,
                     transform,
                     operator,
                     start: 0,
                     end: 0,
                     value: 0,
                 };
             }",
        ),
        (
            "renamed-extern-crate",
            "extern crate self as hidden;
             fn hidden(
                 kind: EventKind,
                 rule: Option<u32>,
                 node: Option<u32>,
                 field: Option<u32>,
                 transform: Option<u32>,
                 operator: Option<u32>,
             ) {
                 let _ = hidden::ScalarEvent {
                     kind,
                     rule,
                     node,
                     field,
                     transform,
                     operator,
                     start: 0,
                     end: 0,
                     value: 0,
                 };
             }",
        ),
        (
            "module-control-surface",
            "mod hidden { pub fn parse() {} } fn node_0() { hidden::parse(); }",
        ),
        (
            "non-top-level-function",
            "fn node_0() { fn hidden() {} hidden(); }",
        ),
        (
            "qualified-function-call",
            "fn node_0() { hidden::parse(run); }",
        ),
        (
            "macro-definition",
            "macro_rules! hidden { () => { node_0() } }",
        ),
        (
            "noncanonical-macro-invocation",
            "fn node_0() { hidden!(); }",
        ),
    ] {
        assert!(
            canonical_surface_violations(source).contains(tag),
            "canonical surface verifier missed `{tag}`"
        );
    }
    let associated_event_type = canonical_surface_violations(
        "struct EventBuffer;
         impl EventSink for EventBuffer {
             type Event = ScalarEvent;
         }",
    );
    assert!(
        !associated_event_type.contains("type-alias"),
        "the exact EventSink::Event associated type remains distinct from a forbidden Rust type alias"
    );
    for source in [
        "fn node_0() { let _ = Step::Matched(()); let _ = Input::new(b\"\"); }",
        "extern crate core;",
        "const _: () = assert!(parse_that::KERNEL_ABI == 3);",
        "const PLAN: P = match RegexPlan::new(&[], 0, 0, 0, &[], &[]) {
             Ok(plan) => plan,
             Err(_) => panic!(\"compiler emitted invalid regex table\"),
         };",
    ] {
        assert!(
            canonical_surface_violations(source).is_empty(),
            "canonical surface verifier rejected an admitted constructor/macro control"
        );
    }

    let resources =
        syn::parse_file(canonical_resource_fixture()).expect("canonical resource fixture syntax");
    let resource_functions = top_level_functions(&resources);
    assert_canonical_resource_helpers(&resource_functions);
    let limits =
        syn::parse_file(canonical_limits_fixture()).expect("canonical limits fixture syntax");
    assert_limits_construction(&limits);
    for (label, mutated) in [
        (
            "nonzero Work initialization",
            canonical_limits_fixture().replacen("work: Cell::new(0)", "work: Cell::new(1)", 1),
        ),
        (
            "foreign default Work authority",
            canonical_limits_fixture().replacen("work: MAX_WORK", "work: MAX_NESTING", 1),
        ),
    ] {
        let file = syn::parse_file(&mutated).expect("limits mutation syntax");
        assert!(
            std::panic::catch_unwind(|| assert_limits_construction(&file)).is_err(),
            "canonical verifier accepted {label}"
        );
    }
    for (label, mutated) in [
        (
            "stale resource-fault cursor",
            canonical_resource_fixture().replace("at: run.cursor()", "at: stale"),
        ),
        (
            "non-restoring nesting frame",
            canonical_resource_fixture().replacen(
                "limits.depth.set(depth);",
                "limits.depth.set(depth + 1);",
                1,
            ),
        ),
        (
            "non-incrementing work counter",
            canonical_resource_fixture().replacen(
                "limits.work.set(work + 1);",
                "limits.work.set(work);",
                1,
            ),
        ),
    ] {
        let file = syn::parse_file(&mutated).expect("resource mutation syntax");
        let functions = top_level_functions(&file);
        assert!(
            std::panic::catch_unwind(|| assert_canonical_resource_helpers(&functions)).is_err(),
            "canonical verifier accepted {label}"
        );
    }
    for (label, impl_body) in [
        (
            "indirect Work read in an allowed impl",
            "let counter = &limits.work; let _ = counter.get();",
        ),
        (
            "dereferenced Nesting write in an allowed impl",
            "let counter = &limits.depth; (*counter).set(0);",
        ),
        (
            "pattern-aliased Work read in an allowed impl",
            "match &limits.work { counter => { let _ = counter.get(); } }",
        ),
    ] {
        let source = format!(
            "{}\nstruct EventBuffer;\nimpl EventBuffer {{ fn events(&self) {{ {impl_body} }} }}",
            canonical_resource_fixture()
        );
        let file = syn::parse_file(&source).expect("allowed-impl resource mutation syntax");
        assert!(
            std::panic::catch_unwind(|| assert_global_runtime_resource_authority(&file)).is_err(),
            "global resource verifier accepted {label}"
        );
    }

    let global_authority = syn::parse_file(canonical_global_authority_fixture())
        .expect("canonical global authority fixture syntax");
    let global_functions = top_level_functions(&global_authority);
    assert!(
        canonical_surface_violations(canonical_global_authority_fixture()).is_empty(),
        "the canonical EventBuffer::new fixture admits only its exact Vec::new constructor"
    );
    assert_global_generated_authority(&global_authority, &global_functions);
    let generic_vec =
        canonical_global_authority_fixture().replacen("Vec::new()", "Vec::<ScalarEvent>::new()", 1);
    let generic_vec =
        syn::parse_file(&generic_vec).expect("generic Vec constructor mutation syntax");
    let generic_vec_functions = top_level_functions(&generic_vec);
    assert!(
        std::panic::catch_unwind(|| {
            assert_global_generated_authority(&generic_vec, &generic_vec_functions)
        })
        .is_err(),
        "global authority verifier accepted path arguments on Vec::<ScalarEvent>::new"
    );
    for (label, mutation) in [
        (
            "off-site Vec constructor in an admitted impl body",
            "let _foreign: Vec<u8> = Vec::new();",
        ),
        (
            "aliased Vec constructor in an admitted impl body",
            "let constructor = Vec::new; let _foreign: Vec<u8> = constructor();",
        ),
        (
            "extra Limits constructor in an admitted impl body",
            "let _fresh = Limits::new(config);",
        ),
        (
            "aliased Limits constructor in an admitted impl body",
            "let constructor = Limits::new; let _fresh = constructor(config);",
        ),
        (
            "qualified Limits constructor in an admitted impl body",
            "let _fresh = <Limits>::new(config);",
        ),
        (
            "direct Limits struct construction in an admitted impl body",
            "let _fresh = Limits { config, depth, work };",
        ),
        (
            "event/identity authority in an admitted impl body",
            "event(identity());",
        ),
        (
            "event callable alias in an admitted impl body",
            "let constructor = event; constructor(identity());",
        ),
        (
            "identity callable alias in an admitted impl body",
            "let identify = identity; event(identify());",
        ),
        (
            "qualified event authority in an admitted impl body",
            "let _ = <External as Trait>::event();",
        ),
        (
            "direct ScalarEvent construction in an admitted impl body",
            "let _ = ScalarEvent {};",
        ),
    ] {
        let source = canonical_global_authority_fixture().replace(
            "fn events(&self) {}",
            &format!("fn events(&self) {{ {mutation} }}"),
        );
        let file = syn::parse_file(&source).expect("global authority mutation syntax");
        let functions = top_level_functions(&file);
        assert!(
            std::panic::catch_unwind(|| { assert_global_generated_authority(&file, &functions) })
                .is_err(),
            "global authority verifier accepted {label}"
        );
    }
    let relocated_vec = format!(
        "{}\nfn foreign_events() -> Vec<ScalarEvent> {{ Vec::new() }}",
        canonical_global_authority_fixture()
            .replace("events: Vec::new(),", "events: foreign_events(),")
    );
    let relocated_vec =
        syn::parse_file(&relocated_vec).expect("relocated Vec constructor mutation syntax");
    let relocated_functions = top_level_functions(&relocated_vec);
    assert!(
        std::panic::catch_unwind(|| {
            assert_global_generated_authority(&relocated_vec, &relocated_functions)
        })
        .is_err(),
        "global authority verifier accepted a sole Vec::new relocated outside EventBuffer::new"
    );
    for (label, replacement) in [
        (
            "EventBuffer::push publishes a foreign event",
            "self.events.push(stale);",
        ),
        (
            "EventBuffer::push duplicates its event",
            "self.events.push(event); self.events.push(event);",
        ),
    ] {
        let source =
            canonical_global_authority_fixture().replace("self.events.push(event);", replacement);
        let file = syn::parse_file(&source).expect("EventBuffer::push mutation syntax");
        let functions = top_level_functions(&file);
        assert!(
            std::panic::catch_unwind(|| { assert_global_generated_authority(&file, &functions) })
                .is_err(),
            "global authority verifier accepted {label}"
        );
    }
    for (label, mutation) in [
        (
            "event callable alias in a top-level free boundary",
            "let event_alias = event;",
        ),
        (
            "identity callable alias in a top-level free boundary",
            "let identity_alias = identity;",
        ),
    ] {
        let source = canonical_global_authority_fixture().replace(
            "let _limits = Limits::new(config);",
            &format!("let _limits = Limits::new(config); {mutation}"),
        );
        let file = syn::parse_file(&source).expect("top-level authority alias mutation syntax");
        let functions = top_level_functions(&file);
        assert!(
            std::panic::catch_unwind(|| { assert_global_generated_authority(&file, &functions) })
                .is_err(),
            "global authority verifier accepted {label}"
        );
    }

    for (label, source) in [
        (
            "foreign primitive owner",
            "use evil::literal; fn node_0() { literal(run, b\"x\"); }",
        ),
        (
            "local primitive shadow",
            "fn literal() {} fn node_0() { literal(); }",
        ),
    ] {
        let file = syn::parse_file(source).expect("primitive provenance mutation syntax");
        let functions = top_level_functions(&file);
        assert!(
            std::panic::catch_unwind(|| { assert_parser_primitive_provenance(&file, &functions) })
                .is_err(),
            "canonical verifier accepted {label}"
        );
    }

    let helper_collision = syn::parse_file("fn charge_work() {} fn charge_work(_: ()) {}")
        .expect("helper collision syntax");
    let helper_functions = top_level_functions(&helper_collision);
    assert!(
        std::panic::catch_unwind(|| unique_function(&helper_functions, "charge_work")).is_err(),
        "canonical verifier accepted a colliding resource helper"
    );

    for (tag, source) in [
        ("parser control", "fn decoy() { if false { node_0(); } }"),
        (
            "resource control",
            "fn extra() { let _ = ResourceKind::Work; limits.work.get(); }",
        ),
    ] {
        let decoy = syn::parse_file(source).expect("decoy helper syntax");
        let functions = top_level_functions(&decoy);
        assert!(
            std::panic::catch_unwind(|| assert_no_noncanonical_authority(&functions)).is_err(),
            "canonical verifier accepted {tag} in a decoy free function"
        );
    }

    let second_root = syn::parse_file("fn parse_with_sink() { rule_0(); rule_1(); }")
        .expect("second public root syntax");
    let second_root_functions = top_level_functions(&second_root);
    let second_root = unique_function(&second_root_functions, "parse_with_sink");
    assert!(
        std::panic::catch_unwind(|| assert_single_entry_rule(second_root, "rule_0")).is_err(),
        "canonical verifier accepted a second public parser root"
    );

    let unguarded =
        syn::parse_file("fn pratt_0() { pratt_0(); }").expect("unguarded recursion syntax");
    let unguarded_functions = top_level_functions(&unguarded);
    let unguarded = unique_function(&unguarded_functions, "pratt_0");
    assert!(
        std::panic::catch_unwind(|| guarded_transaction_block(unguarded)).is_err(),
        "canonical verifier accepted unguarded Pratt recursion"
    );
    let dead_pratt = syn::parse_file(
        "fn pratt_0() {
             if false {
                 let descriptor = &PRATT_OPERATORS[0];
                 match rule_0(run, limits) { _ => pratt_0(run, limits, 0) };
                 event(EventKind::PrattReduce, identity(None, Some(0), None, None),
                       Some(descriptor.id()), start, end, 0);
             }
         }",
    )
    .expect("dead Pratt authority mutation syntax");
    let dead_pratt_functions = top_level_functions(&dead_pratt);
    let dead_pratt = unique_function(&dead_pratt_functions, "pratt_0");
    assert!(
        std::panic::catch_unwind(|| {
            assert_live_pratt_authority(
                &dead_pratt.block,
                "pratt_0",
                &BTreeMap::new(),
                "min_bp",
                &BTreeSet::new(),
            )
        })
        .is_err(),
        "canonical verifier accepted descriptor/probe/recursion/event authority in a dead Pratt branch"
    );

    let canonical_exit = syn::parse_file(
        "fn pratt_0() {
             let mut selected = None;
             loop {
                 if selected.is_none() {
                     selected = Some(0);
                 }
                 let Some(operator) = selected else { break; };
                 Step::Matched(())
             }
         }",
    )
    .expect("canonical Pratt loop-exit fixture syntax");
    let canonical_exit_functions = top_level_functions(&canonical_exit);
    let canonical_exit = unique_function(&canonical_exit_functions, "pratt_0");
    assert_live_pratt_authority(
        &canonical_exit.block,
        "pratt_0",
        &BTreeMap::new(),
        "min_bp",
        &BTreeSet::from(["selected".to_owned()]),
    );
    for (label, mutation) in [
        (
            "inert Pratt expression",
            "let Some(operator) = selected else { break; };\n                 1 + 1;",
        ),
        (
            "empty recognized Pratt condition",
            "let Some(operator) = selected else { break; };\n                 if selected.is_none() {}",
        ),
    ] {
        let source = format!(
            "fn pratt_0() {{
                 let mut selected = None;
                 loop {{
                     if selected.is_none() {{
                         selected = Some(0);
                     }}
                     {mutation}
                     Step::Matched(())
                 }}
             }}"
        );
        let file = syn::parse_file(&source).expect("Pratt statement mutation syntax");
        let functions = top_level_functions(&file);
        assert!(
            std::panic::catch_unwind(|| {
                assert_live_pratt_authority(
                    &unique_function(&functions, "pratt_0").block,
                    "pratt_0",
                    &BTreeMap::new(),
                    "min_bp",
                    &BTreeSet::from(["selected".to_owned()]),
                )
            })
            .is_err(),
            "canonical verifier accepted {label}"
        );
    }
    for (label, mismatch) in [
        ("Pratt mismatch break", "break"),
        ("Pratt mismatch continue", "continue"),
        ("Pratt mismatch return", "return Step::Mismatch(failure)"),
    ] {
        let source = format!(
            "fn pratt_0() {{
                 loop {{
                     match rule_0(run, limits) {{
                         Step::Matched(()) => {{ selected = Some(0); }},
                         Step::Mismatch(failure) => {{ {mismatch}; }},
                         Step::Fault(fault) => return Step::Fault(fault),
                     }}
                 }}
             }}"
        );
        let file = syn::parse_file(&source).expect("Pratt mismatch mutation syntax");
        let functions = top_level_functions(&file);
        let function = unique_function(&functions, "pratt_0");
        let probes = pratt_probes(
            &function.block,
            &BTreeSet::from(["rule_0".to_owned()]),
            &BTreeMap::new(),
            "min_bp",
        );
        assert_eq!(probes.len(), 1);
        assert!(
            !probes[0].exact_mismatch_fallthrough,
            "canonical verifier accepted {label}"
        );
    }
    let canonical_fallthrough = syn::parse_file(
        "fn pratt_0() {
             loop {
                 match rule_0(run, limits) {
                     Step::Matched(()) => { selected = Some(0); },
                     Step::Mismatch(failure) => {},
                     Step::Fault(fault) => return Step::Fault(fault),
                 }
             }
         }",
    )
    .expect("canonical mismatch-fallthrough fixture syntax");
    let canonical_fallthrough_functions = top_level_functions(&canonical_fallthrough);
    let canonical_fallthrough = unique_function(&canonical_fallthrough_functions, "pratt_0");
    let probes = pratt_probes(
        &canonical_fallthrough.block,
        &BTreeSet::from(["rule_0".to_owned()]),
        &BTreeMap::new(),
        "min_bp",
    );
    assert_eq!(probes.len(), 1);
    assert!(probes[0].exact_mismatch_fallthrough);

    let canonical_recursive_pratt = "fn pratt_0() {
         match pratt_0(run, limits, prefix_power) {
             Step::Matched(()) => {},
             Step::Mismatch(failure) => return Step::Mismatch(failure),
             Step::Fault(fault) => return Step::Fault(fault),
         }
         match pratt_0(run, limits, rhs_power) {
             Step::Matched(()) => {},
             Step::Mismatch(failure) => return Step::Mismatch(failure),
             Step::Fault(fault) => return Step::Fault(fault),
         }
     }";
    let recursive_file =
        syn::parse_file(canonical_recursive_pratt).expect("canonical recursive Pratt syntax");
    let recursive_functions = top_level_functions(&recursive_file);
    let recursive_function = unique_function(&recursive_functions, "pratt_0");
    let recursive_matches = recursive_pratt_step_matches(&recursive_function.block, "pratt_0");
    assert_eq!(recursive_matches.len(), 2);
    for recursive_match in recursive_matches {
        assert_exact_recursive_pratt_step_match(recursive_match, "pratt_0");
    }
    for (label, needle, replacement) in [
        (
            "recursive Pratt Matched arm with hidden work",
            "Step::Matched(()) => {}",
            "Step::Matched(()) => { let _ = (); }",
        ),
        (
            "recursive Pratt mismatch fallthrough",
            "Step::Mismatch(failure) => return Step::Mismatch(failure)",
            "Step::Mismatch(failure) => {}",
        ),
        (
            "recursive Pratt mismatch with foreign payload",
            "Step::Mismatch(failure) => return Step::Mismatch(failure)",
            "Step::Mismatch(failure) => return Step::Mismatch(stale)",
        ),
        (
            "recursive Pratt mismatch with wrong Step variant",
            "Step::Mismatch(failure) => return Step::Mismatch(failure)",
            "Step::Mismatch(failure) => return Step::Fault(failure)",
        ),
        (
            "recursive Pratt fault fallthrough",
            "Step::Fault(fault) => return Step::Fault(fault)",
            "Step::Fault(fault) => {}",
        ),
        (
            "recursive Pratt fault with foreign payload",
            "Step::Fault(fault) => return Step::Fault(fault)",
            "Step::Fault(fault) => return Step::Fault(stale)",
        ),
        (
            "guarded recursive Pratt Step arm",
            "Step::Matched(()) => {}",
            "Step::Matched(()) if enabled => {}",
        ),
    ] {
        let mutated = canonical_recursive_pratt.replacen(needle, replacement, 1);
        let file = syn::parse_file(&mutated).expect("recursive Pratt mutation syntax");
        let functions = top_level_functions(&file);
        let function = unique_function(&functions, "pratt_0");
        assert!(
            std::panic::catch_unwind(|| {
                let matches = recursive_pratt_step_matches(&function.block, "pratt_0");
                assert_eq!(matches.len(), 2);
                for recursive_match in matches {
                    assert_exact_recursive_pratt_step_match(recursive_match, "pratt_0");
                }
            })
            .is_err(),
            "recursive Pratt verifier accepted {label}"
        );
    }
    let bare_rhs = canonical_recursive_pratt.replacen(
        "match pratt_0(run, limits, rhs_power) {
             Step::Matched(()) => {},
             Step::Mismatch(failure) => return Step::Mismatch(failure),
             Step::Fault(fault) => return Step::Fault(fault),
         }",
        "pratt_0(run, limits, rhs_power);",
        1,
    );
    let bare_rhs = syn::parse_file(&bare_rhs).expect("bare recursive Pratt mutation syntax");
    let bare_rhs_functions = top_level_functions(&bare_rhs);
    let bare_rhs = unique_function(&bare_rhs_functions, "pratt_0");
    assert_eq!(
        recursive_pratt_step_matches(&bare_rhs.block, "pratt_0").len(),
        1,
        "recursive Pratt verifier rejects a recursive entry outside its exact Step match"
    );

    let wrong_descriptor =
        syn::parse_file("const ROW: [Descriptor; 1] = [Descriptor { id: 9, table: 0 }];")
            .expect("wrong descriptor syntax");
    let row = &top_level_array(&wrong_descriptor, "ROW").elems[0];
    assert!(
        std::panic::catch_unwind(|| {
            let fields = struct_fields(row);
            assert_eq!(integer_literal(fields["id"]), Some(0));
            assert_eq!(integer_literal(fields["table"]), Some(1));
        })
        .is_err(),
        "canonical verifier accepted wrong descriptor ordinals"
    );

    let canonical_operator_event = syn::parse_file(
        "fn pratt_0() {
             let operator = &PRATT_OPERATORS[index];
             event(EventKind::PrattReduce,
                   identity(None, Some(4), None, None),
                   Some(operator.id()), start, end, 0);
         }",
    )
    .expect("canonical operator-event fixture syntax");
    let operator_functions = top_level_functions(&canonical_operator_event);
    let canonical_operator = unique_function(&operator_functions, "pratt_0");
    assert_eq!(
        event_calls_named(&canonical_operator.block, "PrattReduce")
            .into_iter()
            .map(|call| event_identity(call).expect("descriptor event remains structurally visible"))
            .collect::<Vec<_>>(),
        [(
            "PrattReduce".to_owned(),
            [None, Some(4), None, None],
            EventOperator::DescriptorId,
        )],
        "the six-argument event parser retains Some(descriptor-row.id()) without a sentinel"
    );
    assert_pratt_event_operator_authority(&canonical_operator.block);
    let wrong_operator_event = syn::parse_file(
        "fn pratt_0() {
             let operator = &PRATT_OPERATORS[index];
             event(EventKind::PrattReduce,
                   identity(None, Some(4), None, None),
                   Some(99), start, end, 0);
         }",
    )
    .expect("wrong operator-event mutation syntax");
    let wrong_operator_functions = top_level_functions(&wrong_operator_event);
    assert!(
        std::panic::catch_unwind(|| {
            assert_pratt_event_operator_authority(
                &unique_function(&wrong_operator_functions, "pratt_0").block,
            )
        })
        .is_err(),
        "canonical verifier accepted a PrattReduce operator not derived from its descriptor row"
    );

    let wrong_rule = syn::parse_file("fn rule_0() { node_9(); }").expect("wrong rule syntax");
    let wrong_rule_functions = top_level_functions(&wrong_rule);
    let wrong_rule = unique_function(&wrong_rule_functions, "rule_0");
    assert!(
        std::panic::catch_unwind(|| {
            assert_eq!(
                direct_internal_calls(&wrong_rule.block),
                ["node_0".to_owned()]
            );
        })
        .is_err(),
        "canonical verifier accepted a wrong rule/root binding"
    );

    let manual_difference = syn::parse_file(
        "fn node_0() {\
            match charge_work(run, limits) {\
                Step::Matched(()) => {},\
                Step::Mismatch(failure) => return Step::Mismatch(failure),\
                Step::Fault(fault) => return Step::Fault(fault),\
            }\
            node_1(run, limits)\
        }",
    )
    .expect("manual Difference syntax");
    let manual_difference_functions = top_level_functions(&manual_difference);
    let manual_difference = unique_function(&manual_difference_functions, "node_0");
    assert!(
        direct_call(charged_tail(manual_difference), "difference").is_none(),
        "manual Difference falsifier accidentally uses the primitive"
    );

    let dead_difference =
        syn::parse_file("fn decoy() { if false { difference(run, include, exclude); } }")
            .expect("dead Difference syntax");
    let dead_difference_functions = top_level_functions(&dead_difference);
    let decoy = unique_function(&dead_difference_functions, "decoy");
    assert!(
        std::panic::catch_unwind(|| {
            assert_eq!(call_count(&decoy.block, "difference"), 0);
        })
        .is_err(),
        "canonical verifier accepted a dead manual Difference"
    );

    let orphan_pratt = syn::parse_file("fn pratt_0() {} fn pratt_9() { pratt_9(); }")
        .expect("orphan Pratt syntax");
    let orphan_functions = top_level_functions(&orphan_pratt);
    assert!(
        std::panic::catch_unwind(|| {
            assert_eq!(
                orphan_functions
                    .keys()
                    .filter(|name| name.starts_with("pratt_"))
                    .cloned()
                    .collect::<BTreeSet<_>>(),
                BTreeSet::from(["pratt_0".to_owned()])
            );
        })
        .is_err(),
        "canonical verifier accepted an orphan Pratt SCC"
    );

    let correct_pratt_graph = syn::parse_file(
        "fn node_0() { pratt_0(); }
         fn pratt_0() { pratt_0(); }
         fn rule_0() { node_0(); }
         fn check() { rule_0(); }
         fn parse_events() { rule_0(); }",
    )
    .expect("Pratt graph fixture syntax");
    let correct_functions = top_level_functions(&correct_pratt_graph);
    let correct_graph = canonical_graph(&correct_functions);
    let correct_public = reachable(&correct_graph, "check")
        .union(&reachable(&correct_graph, "parse_events"))
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_pratt_control_edges(
        &correct_functions,
        &correct_graph,
        &correct_public,
        "pratt_0",
        "node_0",
        "rule_0",
        true,
    );
    for (label, source, selected) in [
        (
            "site-unreachable Pratt owner",
            "fn node_0() {}
             fn pratt_0() { pratt_0(); }
             fn rule_0() { node_0(); }
             fn check() { rule_0(); }
             fn parse_events() { rule_0(); }",
            true,
        ),
        (
            "selected Pratt absent from public root",
            "fn node_0() { pratt_0(); }
             fn pratt_0() { pratt_0(); }
             fn rule_0() { node_0(); }
             fn check() {}
             fn parse_events() {}",
            true,
        ),
        (
            "wrong table ingress",
            "fn node_0() { pratt_1(); }
             fn pratt_0() { pratt_0(); }
             fn pratt_1() { pratt_1(); }
             fn rule_0() { node_0(); }
             fn check() { rule_0(); }
             fn parse_events() { rule_0(); }",
            false,
        ),
    ] {
        let file = syn::parse_file(source).expect("Pratt graph mutation syntax");
        let functions = top_level_functions(&file);
        let graph = canonical_graph(&functions);
        let public = reachable(&graph, "check")
            .union(&reachable(&graph, "parse_events"))
            .cloned()
            .collect::<BTreeSet<_>>();
        assert!(
            std::panic::catch_unwind(|| {
                assert_pratt_control_edges(
                    &functions, &graph, &public, "pratt_0", "node_0", "rule_0", selected,
                )
            })
            .is_err(),
            "canonical verifier accepted {label}"
        );
    }

    for fixity in ["prefix", "postfix", "infix"] {
        assert!(
            std::panic::catch_unwind(|| {
                assert_exact_call_order(
                    &["rule_2".to_owned(), "rule_1".to_owned()],
                    &["rule_1".to_owned(), "rule_2".to_owned()],
                    &format!("mutated {fixity} probes"),
                );
            })
            .is_err(),
            "canonical verifier accepted a swapped {fixity} probe order"
        );
    }

    let nontransactional_matcher = syn::parse_file(
        "fn rule_0() {
             match charge_work(run, limits) {
                 Step::Matched(()) => {},
                 Step::Mismatch(failure) => return Step::Mismatch(failure),
                 Step::Fault(fault) => return Step::Fault(fault),
             }
             node_0(run, limits)
         }",
    )
    .expect("nontransactional matcher mutation syntax");
    let matcher_functions = top_level_functions(&nontransactional_matcher);
    let matcher = unique_function(&matcher_functions, "rule_0");
    assert!(
        std::panic::catch_unwind(|| guarded_transaction_block(matcher)).is_err(),
        "canonical verifier accepted a matcher without transactional probe rollback"
    );

    let manual_trim = syn::parse_file(
        "fn node_3() {
             match charge_work(run, limits) {
                 Step::Matched(()) => {},
                 Step::Mismatch(failure) => return Step::Mismatch(failure),
                 Step::Fault(fault) => return Step::Fault(fault),
             }
             node_4(run, limits)
         }",
    )
    .expect("nontransactional Trim mutation syntax");
    let manual_trim_functions = top_level_functions(&manual_trim);
    let manual_trim = unique_function(&manual_trim_functions, "node_3");
    assert!(
        std::panic::catch_unwind(|| trim_transaction_block(manual_trim)).is_err(),
        "canonical verifier accepted a Trim child outside the complete transaction"
    );

    let wrong_trivia = syn::parse_file(
        "fn node_3() {\
            match regex(run, &RegexTerminal::new(TRIVIA_PLAN)) {\
                Step::Matched(span) => {\
                    if span.start() != span.end() {\
                        event(EventKind::Trivia,\
                              identity(None, Some(3), None, None),\
                              None, span.start(), other.end(), 0);\
                    }\
                },\
                Step::Mismatch(failure) => return Step::Mismatch(failure),\
                Step::Fault(fault) => return Step::Fault(fault),\
            }\
        }",
    )
    .expect("wrong Trivia span syntax");
    let wrong_trivia_functions = top_level_functions(&wrong_trivia);
    let wrong_trivia = unique_function(&wrong_trivia_functions, "node_3");
    assert!(
        std::panic::catch_unwind(|| {
            let mut trace = TrimTrace {
                node: 3,
                child: "node_4",
                active_span: None,
                nonempty_span: None,
                steps: Vec::new(),
                regex_matches: 0,
            };
            trace.visit_block(&wrong_trivia.block);
        })
        .is_err(),
        "canonical verifier accepted a Trivia span not derived from its match"
    );

    for (label, identity, side) in [
        (
            "wrong Trivia node",
            "identity(None, Some(9), None, None)",
            0,
        ),
        (
            "wrong Trivia side",
            "identity(None, Some(3), None, None)",
            7,
        ),
    ] {
        let source = format!(
            "fn node_3() {{
                match regex(run, &RegexTerminal::new(TRIVIA_PLAN)) {{
                    Step::Matched(span) => {{
                        if span.start() != span.end() {{
                            event(EventKind::Trivia, {identity},
                                  None, span.start(), span.end(), {side});
                        }}
                    }},
                    Step::Mismatch(failure) => return Step::Mismatch(failure),
                    Step::Fault(fault) => return Step::Fault(fault),
                }}
            }}"
        );
        let file = syn::parse_file(&source).expect("Trivia identity mutation syntax");
        let functions = top_level_functions(&file);
        let function = unique_function(&functions, "node_3");
        assert!(
            std::panic::catch_unwind(|| {
                let mut trace = TrimTrace {
                    node: 3,
                    child: "node_4",
                    active_span: None,
                    nonempty_span: None,
                    steps: Vec::new(),
                    regex_matches: 0,
                };
                trace.visit_block(&function.block);
            })
            .is_err(),
            "canonical verifier accepted {label}"
        );
    }
}

fn assert_rule_facts(module: &CompiledModule, name: &str, nullable: bool, minimum: Option<u64>) {
    let rule = rule(module, name);
    let rule_facts = module.facts().rule(rule).expect("rule facts");
    let node_facts = module
        .facts()
        .node(rule_root(module, name))
        .expect("root node facts");
    assert_eq!(rule_facts.nullable(), nullable, "{name} rule nullable");
    assert_eq!(
        rule_facts.minimum_consumption(),
        minimum,
        "{name} rule minimum"
    );
    assert_eq!(node_facts.nullable(), nullable, "{name} node nullable");
    assert_eq!(
        node_facts.minimum_consumption(),
        minimum,
        "{name} node minimum"
    );
}

fn assert_node_derivation(module: &CompiledModule, node: bbnf::compiler::NodeId) {
    let derivation = module
        .types()
        .node_derivations()
        .get(node.index())
        .expect("node has a public type derivation");
    assert_eq!(derivation.node(), node);
    assert_eq!(
        Some(derivation.type_id()),
        module.types().node_type(node),
        "derivation and solved type carriers agree"
    );
}

#[test]
fn projection_difference_and_trim_survive_every_semantic_stage() {
    let module = accepted(SEMANTIC_MANIFEST, SEMANTIC_SOURCE);

    for (name, keep_left) in [("keep_right", false), ("keep_left", true)] {
        let root = rule_root(&module, name);
        let NodeKind::Projection { left, right, keep } =
            module.program().node(root).expect("projection root").kind()
        else {
            panic!("{name} did not remain a source Projection");
        };
        let expected_keep = if keep_left {
            ProjectionKeep::Left
        } else {
            ProjectionKeep::Right
        };
        assert_eq!(keep, expected_keep, "{name} source keep discriminator");
        assert_ne!(
            module.types().node_type(left),
            module.types().node_type(right),
            "{name} operands must have genuinely distinct nominal types"
        );
        let kept = if keep_left { left } else { right };
        assert_node_derivation(&module, root);
        assert_node_derivation(&module, left);
        assert_node_derivation(&module, right);
        assert_eq!(
            module.types().node_type(root),
            module.types().node_type(kept),
            "{name} must have the kept child's semantic type"
        );

        let lowered_rule = module
            .lowering()
            .rules()
            .get(rule(&module, name).index())
            .expect("lowered projection rule");
        let lowered = module
            .lowering()
            .node(lowered_rule.root())
            .expect("lowered projection root");
        let PlanOp::Projection { left, right, keep } = lowered.op() else {
            panic!("{name} did not remain a plan Projection");
        };
        assert_eq!(keep, expected_keep, "{name} plan keep discriminator");
        assert_ne!(
            module
                .lowering()
                .node(left)
                .map(|node| node.semantic_type()),
            module
                .lowering()
                .node(right)
                .map(|node| node.semantic_type()),
            "{name} lowered operands must retain distinct nominal types"
        );
        let kept = if keep_left { left } else { right };
        assert_eq!(
            Some(lowered.semantic_type()),
            module
                .lowering()
                .node(kept)
                .map(|node| node.semantic_type()),
            "{name} lowering must preserve only the selected type"
        );
    }

    for (name, expected_keep) in [
        ("mixed_keep_right", ProjectionKeep::Right),
        ("mixed_keep_left", ProjectionKeep::Left),
        ("mixed_nullable_kept_right", ProjectionKeep::Right),
        ("mixed_nullable_kept_left", ProjectionKeep::Left),
        ("nullable_keep_right", ProjectionKeep::Right),
        ("nullable_keep_left", ProjectionKeep::Left),
    ] {
        let source_root = rule_root(&module, name);
        let NodeKind::Projection { keep, .. } = module
            .program()
            .node(source_root)
            .expect("nullable Projection source node")
            .kind()
        else {
            panic!("{name} did not remain a source Projection");
        };
        assert_eq!(keep, expected_keep, "{name} source keep direction");
        let plan_root = plan_rule_root(&module, name);
        let PlanOp::Projection { keep, .. } = module
            .lowering()
            .node(plan_root)
            .expect("nullable Projection plan node")
            .op()
        else {
            panic!("{name} did not remain a plan Projection");
        };
        assert_eq!(keep, expected_keep, "{name} plan keep direction");
    }

    for name in ["difference_live", "difference_nullable", "difference_empty"] {
        let root = rule_root(&module, name);
        let NodeKind::Difference { include, exclude } =
            module.program().node(root).expect("difference root").kind()
        else {
            panic!("{name} did not remain a source Difference");
        };
        assert_node_derivation(&module, root);
        assert_node_derivation(&module, include);
        assert_node_derivation(&module, exclude);
        assert_eq!(
            module.types().node_type(root),
            module.types().node_type(include),
            "{name} must preserve the include type"
        );

        let lowered_rule = module
            .lowering()
            .rules()
            .get(rule(&module, name).index())
            .expect("lowered difference rule");
        let lowered = module
            .lowering()
            .node(lowered_rule.root())
            .expect("lowered difference root");
        let PlanOp::Difference {
            include,
            exclude: _,
        } = lowered.op()
        else {
            panic!("{name} did not remain a plan Difference");
        };
        assert_eq!(
            Some(lowered.semantic_type()),
            module
                .lowering()
                .node(include)
                .map(|node| node.semantic_type()),
            "{name} lowering must preserve the include type"
        );
    }

    for name in ["trimmed", "trimmed_again", "nullable_trimmed"] {
        let trim_root = rule_root(&module, name);
        let NodeKind::Trim { child } = module.program().node(trim_root).expect("trim root").kind()
        else {
            panic!("{name} did not remain a source Trim");
        };
        assert_node_derivation(&module, trim_root);
        assert_node_derivation(&module, child);
        assert_eq!(
            module.types().node_type(trim_root),
            module.types().node_type(child),
            "{name} must preserve its child's semantic type"
        );
        let lowered = module
            .lowering()
            .node(plan_rule_root(&module, name))
            .expect("lowered trim root");
        let PlanOp::Trim { child } = lowered.op() else {
            panic!("{name} did not remain a plan Trim");
        };
        assert_eq!(
            Some(lowered.semantic_type()),
            module
                .lowering()
                .node(child)
                .map(|node| node.semantic_type())
        );
    }
}

#[test]
fn discarded_and_excluded_children_remain_dependencies_and_exact_fact_inputs() {
    let module = accepted(SEMANTIC_MANIFEST, SEMANTIC_SOURCE);

    assert_eq!(
        dependency_names(&module, rule(&module, "keep_right")),
        BTreeSet::from(["kept".to_owned(), "left".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "keep_left")),
        BTreeSet::from(["kept".to_owned(), "right".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "mixed_keep_right")),
        BTreeSet::from(["kept".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "mixed_keep_left")),
        BTreeSet::from(["kept".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "mixed_nullable_kept_right")),
        BTreeSet::from(["left".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "mixed_nullable_kept_left")),
        BTreeSet::from(["right".to_owned()])
    );
    assert!(dependency_names(&module, rule(&module, "nullable_keep_right")).is_empty());
    assert!(dependency_names(&module, rule(&module, "nullable_keep_left")).is_empty());
    assert_eq!(
        dependency_names(&module, rule(&module, "difference_live")),
        BTreeSet::from(["kept".to_owned(), "right".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "difference_nullable")),
        BTreeSet::from([
            "nonnullable_exclude".to_owned(),
            "nullable_include".to_owned(),
        ])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "difference_empty")),
        BTreeSet::from(["kept".to_owned(), "nullable_exclude".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "trimmed")),
        BTreeSet::from(["kept".to_owned()])
    );
    assert_eq!(
        dependency_names(&module, rule(&module, "trimmed_again")),
        BTreeSet::from(["right".to_owned()])
    );
    assert!(dependency_names(&module, rule(&module, "nullable_trimmed")).is_empty());

    assert_rule_facts(&module, "keep_right", false, Some(2));
    assert_rule_facts(&module, "keep_left", false, Some(2));
    assert_rule_facts(&module, "mixed_keep_right", false, Some(1));
    assert_rule_facts(&module, "mixed_keep_left", false, Some(1));
    assert_rule_facts(&module, "mixed_nullable_kept_right", false, Some(1));
    assert_rule_facts(&module, "mixed_nullable_kept_left", false, Some(1));
    assert_rule_facts(&module, "nullable_keep_right", true, Some(0));
    assert_rule_facts(&module, "nullable_keep_left", true, Some(0));
    assert_rule_facts(&module, "difference_live", false, Some(1));
    assert_rule_facts(&module, "difference_nullable", true, Some(0));
    assert_rule_facts(&module, "difference_empty", false, None);
    assert_rule_facts(&module, "trimmed", false, Some(1));
    assert_rule_facts(&module, "trimmed_again", false, Some(1));
    assert_rule_facts(&module, "nullable_trimmed", true, Some(0));
    assert!(
        module.facts().work_units() >= module.program().nodes().len(),
        "analysis work accounts for every semantic node, including discarded and excluded nodes"
    );
}

#[test]
fn one_optional_compiled_trivia_dfa_is_shared_and_replayed_without_private_name_laws() {
    let with_trivia = accepted(SEMANTIC_MANIFEST, SEMANTIC_SOURCE);
    assert_eq!(
        with_trivia
            .program()
            .nodes()
            .iter()
            .filter(|node| matches!(node.kind(), NodeKind::Trim { .. }))
            .count(),
        3,
        "fixture must contain multiple independent Trim nodes"
    );
    let trivia = with_trivia
        .lowering()
        .trivia()
        .expect("one manifest trivia DFA");
    let expected_replay = serde_json::json!({
        "byte_classes": trivia.byte_classes(),
        "transitions": trivia.transitions(),
        "accepting": trivia.accepting(),
        "state_count": trivia.state_count(),
        "class_count": trivia.class_count(),
        "start_state": trivia.start_state(),
    });
    let lowering_replay: serde_json::Value =
        serde_json::from_slice(&with_trivia.lowering().replay_json().unwrap())
            .expect("lowering replay is JSON");
    assert_eq!(
        lowering_replay["value"]["trivia"], expected_replay,
        "lowering replay carries exactly the singleton compiled trivia DFA"
    );
    let generated_manifest: serde_json::Value =
        serde_json::from_str(with_trivia.generated().manifest())
            .expect("generated manifest is JSON");
    assert_eq!(
        generated_manifest["trivia_dfas"], 1,
        "generated product carries one shared trivia DFA, not one per Trim"
    );
    assert_generated_v6_canonical(&with_trivia, true);
    let generated_source = with_trivia.generated().source();
    let generated_file =
        syn::parse_file(generated_source).expect("generated trivia mutation basis syntax");
    let trivia_constructor = regex_plan_constructor(
        top_level_expression(&generated_file, "TRIVIA_PLAN")
            .expect("generated trivia mutation basis plan"),
    )
    .expect("generated trivia mutation basis constructor");
    let trivia_byte_classes = referenced_constant(&trivia_constructor.args[0])
        .expect("generated trivia byte-class constant");
    let divergent = mutate_first_array_integer(generated_source, &trivia_byte_classes);
    let duplicated = format!(
        "{generated_source}\nconst DUPLICATED_TRIVIA_PLAN: RegexPlan<'static> = TRIVIA_PLAN;\n"
    );
    let inline = format!(
        "{generated_source}\nfn inline_trivia_plan() {{ let _ = RegexPlan::new(&[], 0, 0, 0, &[], &[]); }}\n"
    );
    let alternate = format!(
        "{generated_source}\nimpl Limits {{ fn alternate_trivia(&self) {{ regex(run, &terminal); }} }}\n"
    );
    for (label, mutated) in [
        ("byte-divergent trivia DFA", divergent),
        ("duplicated trivia plan", duplicated),
        ("inline trivia plan", inline),
        ("alternate trivia recognizer", alternate),
    ] {
        assert!(
            std::panic::catch_unwind(|| {
                assert_generated_v6_source(&with_trivia, &mutated, true)
            })
            .is_err(),
            "canonical generated-source verifier accepted {label}"
        );
    }

    let without_trivia = accepted(MULTI_PRATT_MANIFEST, MULTI_PRATT_SOURCE);
    assert!(
        without_trivia.lowering().trivia().is_none(),
        "null manifest trivia stores no compiled DFA"
    );
    let no_trivia_replay: serde_json::Value =
        serde_json::from_slice(&without_trivia.lowering().replay_json().unwrap())
            .expect("lowering replay is JSON");
    assert_eq!(no_trivia_replay["value"]["trivia"], serde_json::Value::Null);
    let no_trivia_generated_manifest: serde_json::Value =
        serde_json::from_str(without_trivia.generated().manifest())
            .expect("generated manifest is JSON");
    assert_eq!(no_trivia_generated_manifest["trivia_dfas"], 0);

    const LITERAL_ONLY_SOURCE: &[u8] = b"document = \"x\";\n";
    let literal_only_manifest =
        manifest_for_entry("contracts/w3/literal-only-no-trivia.bbnf", "document", None);
    let literal_only = accepted(&literal_only_manifest, LITERAL_ONLY_SOURCE);
    assert!(literal_only.lowering().trivia().is_none());
    assert_generated_v6_canonical(&literal_only, false);
    let literal_only_generated_manifest: serde_json::Value =
        serde_json::from_str(literal_only.generated().manifest())
            .expect("literal-only generated manifest is JSON");
    assert_eq!(literal_only_generated_manifest["trivia_dfas"], 0);
}

fn pratt_table(module: &CompiledModule) -> PrattTableId {
    let expression = rule_root(module, "expression");
    let NodeKind::Pratt { atom: _, table } = module
        .program()
        .node(expression)
        .expect("expression root")
        .kind()
    else {
        panic!("manifest-attached expression did not become Pratt");
    };
    table
}

#[test]
fn pratt_is_manifest_attached_revision_branded_and_semantically_closed() {
    let first = accepted(PRATT_MANIFEST, PRATT_SOURCE);
    let rebuilt = accepted(PRATT_MANIFEST, PRATT_SOURCE);
    assert_generated_v6_canonical(&first, true);
    let alternate_manifest = String::from_utf8(PRATT_MANIFEST.to_vec())
        .expect("manifest is UTF-8")
        .replace(
            "contracts/w3/pratt-laws.bbnf",
            "contracts/w3/other-pratt-laws.bbnf",
        );
    let other = accepted(alternate_manifest.as_bytes(), PRATT_SOURCE);

    let expression = rule_root(&first, "expression");
    let NodeKind::Pratt { atom, table } = first
        .program()
        .node(expression)
        .expect("expression root")
        .kind()
    else {
        panic!("expression source root must be Pratt");
    };
    assert_eq!(table, pratt_table(&rebuilt));
    assert_ne!(table, pratt_table(&other));
    assert_eq!(table.revision(), first.program().revision());
    assert_ne!(table.revision(), other.program().revision());
    assert_eq!(
        first.types().node_type(expression),
        first.types().node_type(atom),
        "Pratt must preserve its atom type"
    );
    assert_node_derivation(&first, expression);
    assert_node_derivation(&first, atom);

    let lowered_rule = first
        .lowering()
        .rules()
        .get(rule(&first, "expression").index())
        .expect("lowered expression rule");
    let lowered = first
        .lowering()
        .node(lowered_rule.root())
        .expect("lowered expression root");
    let PlanOp::Pratt {
        atom: lowered_atom,
        table: lowered_table,
    } = lowered.op()
    else {
        panic!("expression plan root must be Pratt");
    };
    assert_eq!(lowered_table, table);
    assert_eq!(
        Some(lowered.semantic_type()),
        first
            .lowering()
            .node(lowered_atom)
            .map(|node| node.semantic_type())
    );

    let expected_dependencies = BTreeSet::from([
        "atom".to_owned(),
        "long_shift".to_owned(),
        "postfix_bang".to_owned(),
        "power".to_owned(),
        "prefix_minus".to_owned(),
        "probe_then_fail".to_owned(),
        "product".to_owned(),
        "short_shift".to_owned(),
        "sum".to_owned(),
    ]);
    assert_eq!(
        dependency_names(&first, rule(&first, "expression")),
        expected_dependencies,
        "the atom and every operator matcher participate in public dependency closure"
    );
    assert_rule_facts(&first, "expression", false, Some(1));

    let tables = first.lowering().pratt_tables();
    let table_view = tables.get(0).expect("one public Pratt table");
    assert_eq!(tables.len(), 1);
    assert_eq!(table_view.id(), table);
    assert_eq!(table_view.site_rule(), rule(&first, "expression"));
    assert_eq!(table_view.atom_rule(), rule(&first, "atom"));
    assert_eq!(
        first
            .lowering()
            .pratt_table(table)
            .expect("table lookup in its carrier")
            .id(),
        table
    );

    let operators = first.lowering().pratt_operators();
    assert_eq!(operators.len(), 8);
    let ranged = table_view.operators().collect::<Vec<PrattOperatorId>>();
    let enumerated = operators
        .iter()
        .map(|operator| operator.id())
        .collect::<Vec<_>>();
    assert_eq!(
        ranged, enumerated,
        "the table's compact operator range is the public manifest-order iterator"
    );
    for (index, operator) in operators.iter().enumerate() {
        assert_eq!(operator.id().index(), index);
        assert_eq!(operator.id().revision(), first.program().revision());
        assert_eq!(operator.table(), table);
        assert_node_derivation(
            &first,
            first
                .program()
                .rule(operator.matcher_rule())
                .expect("operator matcher belongs to the program")
                .root(),
        );
        assert_eq!(
            first
                .lowering()
                .pratt_operator(operator.id())
                .expect("operator lookup in its carrier")
                .id(),
            operator.id()
        );
        assert!(
            other.lowering().pratt_operator(operator.id()).is_none(),
            "a revision-branded operator ID is rejected by a foreign carrier"
        );
    }
    assert!(
        other.lowering().pratt_table(table).is_none(),
        "a revision-branded table ID is rejected by a foreign carrier"
    );
}

#[test]
fn canonical_generated_v6_rejects_full_product_control_and_pratt_mutations() {
    let module = accepted(PRATT_MANIFEST, PRATT_SOURCE);
    assert_generated_v6_canonical(&module, true);
    let source = module.generated().source();
    let table = module.lowering().pratt_tables()[0].id();
    let pratt = format!("pratt_{}", table.index());

    for (label, mutated) in [
        (
            "nested module parser authority",
            format!("{source}\nmod hidden_parser {{ pub fn parse() {{}} }}\n"),
        ),
        (
            "non-top-level parser authority",
            format!("{source}\nfn illicit() {{ fn nested() {{}} nested(); }}\n"),
        ),
        (
            "qualified parser authority",
            format!("{source}\nfn illicit() {{ hidden_parser::parse(); }}\n"),
        ),
        (
            "macro-defined parser authority",
            format!(
                "{source}\nmacro_rules! hidden_parser {{ () => {{ rule_0() }} }}\nhidden_parser!();\n"
            ),
        ),
        (
            "rogue inherent parser surface",
            format!(
                "{source}\nstruct Rogue;\nimpl Rogue {{ fn parse_module<F: FnOnce()>(&self, parser: F) {{ parser(); }} }}\n"
            ),
        ),
    ] {
        assert_generated_v6_mutation_rejected(&module, &mutated, true, label);
    }
    assert_eq!(
        source.matches("type Event = ScalarEvent;").count(),
        1,
        "generated EventSink associated-type mutation basis is unique"
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &source.replacen("type Event = ScalarEvent;", "type Event = ForeignEvent;", 1),
        true,
        "EventBuffer EventSink associated type detached from ScalarEvent",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            "parse_with_sink",
            "Run::with_sink(input, sink).parse_module(",
            "Run::with_sink(input, NoSink::new()).parse_module(|_| Step::Matched(()));\n    Run::with_sink(input, sink).parse_module(",
        ),
        true,
        "second live Run::parse_module crossing in the full product",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            "emit_event",
            "resource_fault(run, ResourceKind::Events, limits.config.logical_events)",
            "resource_fault(stale, ResourceKind::Events, limits.config.logical_events)",
        ),
        true,
        "stale event-budget fault cursor in the full product",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "sequence(run, |run| {",
            "sequence(run, |run| {\n        if false { let _ = min_bp; }",
        ),
        true,
        "dead branch inside the returned Pratt transaction",
    );
    for (label, control) in [
        (
            "nonliteral constant-dead Pratt branch",
            "if 0 == 1 { let _ = min_bp; }",
        ),
        (
            "constant-selected dead Pratt match arm",
            "match 0 { 1 => { let _ = min_bp; }, _ => {} }",
        ),
        (
            "alternate Pratt while control",
            "while 0 == 1 { let _ = min_bp; }",
        ),
    ] {
        assert_generated_v6_mutation_rejected(
            &module,
            &replace_first_in_generated_function(
                source,
                &pratt,
                "sequence(run, |run| {",
                &format!("sequence(run, |run| {{\n        {control}"),
            ),
            true,
            label,
        );
    }
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "sequence(run, |run| {",
            "sequence(run, |run| {\n        let limits = stale_limits;",
        ),
        true,
        "Pratt transaction shadows its current limits context",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "sequence(run, |run| {",
            "sequence(run, |run| {\n        let fresh_limits = Limits::new(LimitsConfig::default());\n        let (limits,) = (&fresh_limits,);",
        ),
        true,
        "tuple-pattern shadow substitutes the current Pratt limits",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "sequence(run, |run| {",
            "sequence(run, |run| {\n        *limits = stale_limits;",
        ),
        true,
        "referenced assignment substitutes the current Pratt limits",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "sequence(run, |run| {",
            "sequence(run, |run| {\n        limits.work.set(0);",
        ),
        true,
        "Pratt control mutates the Work counter outside charge_work",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "sequence(run, |run| {",
            "sequence(run, |run| {\n        let counter = &limits.work;\n        counter.set(0);",
        ),
        true,
        "Pratt control aliases and mutates the Work counter",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            &pratt,
            "charge_work(run, limits)",
            "charge_work(run, &Limits::new(LimitsConfig::default()))",
        ),
        true,
        "fresh Work limits substituted into Pratt control",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            &pratt,
            "with_depth(run, limits,",
            "with_depth(run, &Limits::new(LimitsConfig::default()),",
        ),
        true,
        "fresh Nesting limits substituted into Pratt control",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            &format!(
                "rule_{}(run, limits)",
                rule(&module, "prefix_minus").index()
            ),
            &format!(
                "rule_{}(run, stale_limits)",
                rule(&module, "prefix_minus").index()
            ),
        ),
        true,
        "stale limits substituted into a Pratt matcher probe",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            &format!("{pratt}(run, limits,"),
            &format!("{pratt}(run, stale_limits,"),
        ),
        true,
        "stale limits substituted into Pratt recursion",
    );

    let literal = module
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Literal(_)).then_some(node))
        .expect("Pratt fixture contains a Literal node");
    let literal = format!("node_{literal}");
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(source, &literal, "|| event(", "|| bypass_event("),
        true,
        "event callback bypasses the sole event constructor",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            &literal,
            "emit_event(run, limits,",
            "emit_event(stale_run, limits,",
        ),
        true,
        "stale run substituted into terminal event emission",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(source, &literal, "Literal", 0, 3, "span.end()"),
        true,
        "Literal start detached from its recognized span",
    );

    let regex = module
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Regex(_)).then_some(node))
        .expect("Pratt fixture contains a Regex node");
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(
            source,
            &format!("node_{regex}"),
            "Regex",
            0,
            4,
            "span.start()",
        ),
        true,
        "Regex end detached from its recognized span",
    );
    let trim = plan_rule_root(&module, "document").index();
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(
            source,
            &format!("node_{trim}"),
            "Trivia",
            0,
            4,
            "other.end()",
        ),
        true,
        "Trivia end detached from its exact matched span",
    );

    let entry_rule = format!("rule_{}", module.lowering().entry().index());
    for (variant, argument, replacement, label) in [
        (
            "EnterRule",
            4,
            "stale",
            "EnterRule point span detached from its entry cursor",
        ),
        (
            "LeaveRule",
            3,
            "stale",
            "LeaveRule start detached from its rule entry cursor",
        ),
    ] {
        assert_generated_v6_mutation_rejected(
            &module,
            &mutate_event_argument(source, &entry_rule, variant, 0, argument, replacement),
            true,
            label,
        );
    }
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(source, &pratt, "PrattReduce", 0, 5, "1"),
        true,
        "PrattReduce reserved scalar value changed",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(source, &pratt, "PrattReduce", 0, 3, "stale_start"),
        true,
        "PrattReduce start detached from the Pratt transaction entry",
    );
    let choice = module
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Choice(_)).then_some(node))
        .expect("Pratt fixture contains a Choice node");
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(
            source,
            &format!("node_{choice}"),
            "ChoiceBranch",
            0,
            5,
            "999",
        ),
        true,
        "ChoiceBranch exact manifest ordinal changed",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_event_argument(source, "parse_with_sink", "EndInput", 0, 5, "1"),
        true,
        "EndInput reserved scalar value changed",
    );

    let table_row = format!("PRATT_TABLES[{}]", table.index());
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(source, &pratt, &table_row, "PRATT_TABLES[999]"),
        true,
        "wrong table descriptor row",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &mutate_pratt_event_descriptor(source, &pratt),
        true,
        "PrattReduce substituted with a foreign descriptor row",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "right_binding_power()",
            "left_binding_power()",
        ),
        true,
        "recursive Pratt call substituted with the wrong binding-power getter",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_first_in_generated_function(
            source,
            &pratt,
            "left_binding_power()",
            "right_binding_power()",
        ),
        true,
        "operator eligibility substituted with the wrong binding-power getter",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(source, &pratt, "fixity()", "id()"),
        true,
        "postfix/infix dispatch substituted with non-fixity descriptor data",
    );

    let site = format!("node_{}", plan_rule_root(&module, "expression").index());
    let site_call = format!("{pratt}(run, limits, 0)");
    let wrong_site_call = format!("rule_{}(run, limits)", rule(&module, "atom").index());
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(source, &site, &site_call, &wrong_site_call),
        true,
        "Pratt owner removed from its exact site adapter",
    );
    let entry = format!("rule_{}(run, &limits)", module.lowering().entry().index());
    let wrong_entry = format!("rule_{}(run, &limits)", rule(&module, "atom").index());
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(source, "parse_with_sink", &entry, &wrong_entry),
        true,
        "selected Pratt site removed from the public-root path",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            "parse_with_sink",
            &entry,
            &entry.replace("&limits", "&stale_limits"),
        ),
        true,
        "public entry receives stale limits",
    );
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(
            source,
            "parse_with_sink",
            "sequence(run, |run| {",
            "sequence(run, |stale_run| {",
        ),
        true,
        "public sequence callback rebinds away from its current run",
    );

    for (label, needle, replacement) in [
        (
            "EventIdentity swaps rule and node authority",
            "EventIdentity { rule, node, field, transform }",
            "EventIdentity { rule: node, node: rule, field, transform }",
        ),
        (
            "EventIdentity drops transform authority",
            "EventIdentity { rule, node, field, transform }",
            "EventIdentity { rule, node, field, transform: None }",
        ),
        (
            "Pratt table getter returns a foreign field",
            "pub const fn site_rule(&self) -> u32 { self.site_rule }",
            "pub const fn site_rule(&self) -> u32 { self.atom_rule }",
        ),
        (
            "Pratt operator getter returns a foreign field",
            "pub const fn matcher_rule(&self) -> u32 { self.matcher_rule }",
            "pub const fn matcher_rule(&self) -> u32 { self.table }",
        ),
    ] {
        assert_eq!(
            source.matches(needle).count(),
            1,
            "{label} mutation basis is unique"
        );
        assert_generated_v6_mutation_rejected(
            &module,
            &source.replacen(needle, replacement, 1),
            true,
            label,
        );
    }

    let long = format!("rule_{}(run, limits)", rule(&module, "long_shift").index());
    let short = format!("rule_{}(run, limits)", rule(&module, "short_shift").index());
    assert_generated_v6_mutation_rejected(
        &module,
        &swap_once_in_generated_function(source, &pratt, &long, &short),
        true,
        "infix manifest-order probes swapped in a generated product",
    );
    let infix_matcher = format!("rule_{}", rule(&module, "probe_then_fail").index());
    assert_generated_v6_mutation_rejected(
        &module,
        &replace_once_in_generated_function(source, &infix_matcher, "sequence(run,", "choice(run,"),
        true,
        "infix matcher transactional rollback removed in a generated product",
    );

    let affix = accepted(AFFIX_OVERLAP_MANIFEST, AFFIX_OVERLAP_SOURCE);
    assert_generated_v6_canonical(&affix, false);
    let affix_source = affix.generated().source();
    let affix_pratt = format!("pratt_{}", affix.lowering().pratt_tables()[0].id().index());
    for (label, first, second) in [
        (
            "prefix",
            format!("rule_{}(run, limits)", rule(&affix, "prefix_long").index()),
            format!("rule_{}(run, limits)", rule(&affix, "prefix_short").index()),
        ),
        (
            "postfix",
            format!("rule_{}(run, limits)", rule(&affix, "postfix_long").index()),
            format!(
                "rule_{}(run, limits)",
                rule(&affix, "postfix_short").index()
            ),
        ),
    ] {
        assert_generated_v6_mutation_rejected(
            &affix,
            &swap_once_in_generated_function(affix_source, &affix_pratt, &first, &second),
            false,
            &format!("{label} manifest-order probes swapped in a generated product"),
        );
    }
    for matcher in ["prefix_long", "postfix_long"] {
        let function = format!("rule_{}", rule(&affix, matcher).index());
        assert_generated_v6_mutation_rejected(
            &affix,
            &replace_once_in_generated_function(
                affix_source,
                &function,
                "sequence(run,",
                "choice(run,",
            ),
            false,
            &format!("{matcher} transactional rollback removed in a generated product"),
        );
    }

    let semantic = accepted(SEMANTIC_MANIFEST, SEMANTIC_SOURCE);
    assert_generated_v6_canonical(&semantic, true);
    let epsilon = semantic
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Epsilon).then_some(node))
        .expect("semantic fixture contains Epsilon");
    assert_generated_v6_mutation_rejected(
        &semantic,
        &replace_last_in_generated_function(
            semantic.generated().source(),
            &format!("node_{epsilon}"),
            "Step::Matched(())",
            "{ if false { let _ = sequence(run, |_| Step::Matched(())); } Step::Matched(()) }",
        ),
        true,
        "dead primitive call hidden in an Epsilon returned path",
    );
    let (sequence_node, sequence_children) = semantic
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| match plan.op() {
            PlanOp::Sequence(children) if children.len() >= 2 => {
                Some((node, children.collect::<Vec<_>>()))
            }
            _ => None,
        })
        .expect("semantic fixture contains a multi-child Sequence");
    assert_generated_v6_mutation_rejected(
        &semantic,
        &replace_once_in_generated_function(
            semantic.generated().source(),
            &format!("node_{sequence_node}"),
            &format!("node_{}(run, limits)", sequence_children[0].index()),
            &format!("node_{}(run, limits)", sequence_children[1].index()),
        ),
        true,
        "Sequence substitutes its first recorded child",
    );

    let difference_node = plan_rule_root(&semantic, "difference_live").index();
    assert_generated_v6_mutation_rejected(
        &semantic,
        &replace_once_in_generated_function(
            semantic.generated().source(),
            &format!("node_{difference_node}"),
            "difference(run, |run|",
            "difference(run, |stale_run|",
        ),
        true,
        "Difference include closure rebinds away from its current run",
    );

    const REPEAT_EVENT_SOURCE: &[u8] = b"document = item*; item = \"x\";";
    let repeat_manifest =
        manifest_for_entry("contracts/w3/repeat-event-falsifier.bbnf", "document", None);
    let repeat = accepted(&repeat_manifest, REPEAT_EVENT_SOURCE);
    assert_generated_v6_canonical(&repeat, false);
    let repeat_node = repeat
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Repeat { .. }).then_some(node))
        .expect("repeat event fixture contains Repeat");
    assert_generated_v6_mutation_rejected(
        &repeat,
        &mutate_event_argument(
            repeat.generated().source(),
            &format!("node_{repeat_node}"),
            "RepeatCount",
            0,
            5,
            "0",
        ),
        false,
        "RepeatCount detached from its saturating matched count",
    );

    let projection_manifest = manifest_for_entry(
        "contracts/w3/record-transform-event-falsifier.bbnf",
        "baseline_right",
        None,
    );
    let projection = accepted(&projection_manifest, PROJECTION_STREAM_SOURCE);
    assert_generated_v6_canonical(&projection, false);
    let record_node = projection
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Record { .. }).then_some(node))
        .expect("projection fixture contains Record");
    for (variant, argument, replacement, label) in [
        (
            "EnterField",
            5,
            "1",
            "EnterField reserved scalar value changed",
        ),
        (
            "LeaveField",
            4,
            "stale_end",
            "LeaveField end detached from its committed field cursor",
        ),
    ] {
        assert_generated_v6_mutation_rejected(
            &projection,
            &mutate_event_argument(
                projection.generated().source(),
                &format!("node_{record_node}"),
                variant,
                0,
                argument,
                replacement,
            ),
            false,
            label,
        );
    }
    let transform_node = projection
        .lowering()
        .nodes()
        .iter()
        .enumerate()
        .find_map(|(node, plan)| matches!(plan.op(), PlanOp::Transform { .. }).then_some(node))
        .expect("projection fixture contains Transform");
    assert_generated_v6_mutation_rejected(
        &projection,
        &mutate_event_argument(
            projection.generated().source(),
            &format!("node_{transform_node}"),
            "Transform",
            0,
            3,
            "stale_start",
        ),
        false,
        "Transform start detached from its child transaction",
    );
}

#[test]
fn pratt_operator_views_preserve_manifest_order_fixity_and_binding_powers() {
    let module = accepted(PRATT_MANIFEST, PRATT_SOURCE);
    let expected = [
        ("prefix_minus", "prefix", None, Some(70_u64)),
        ("postfix_bang", "postfix", Some(90), None),
        ("power", "infix", Some(80), Some(80)),
        ("product", "infix", Some(60), Some(61)),
        ("sum", "infix", Some(40), Some(41)),
        ("probe_then_fail", "infix", Some(30), Some(31)),
        ("long_shift", "infix", Some(20), Some(21)),
        ("short_shift", "infix", Some(20), Some(21)),
    ];
    let operators = module.lowering().pratt_operators();
    assert_eq!(operators.len(), expected.len());
    for (index, (matcher, fixity, left, right)) in expected.into_iter().enumerate() {
        let row = operators.get(index).expect("manifest-order operator row");
        assert_eq!(row.id().index(), index);
        assert_eq!(row.matcher_rule(), rule(&module, matcher));
        assert_eq!(format!("{:?}", row.fixity()).to_ascii_lowercase(), fixity);
        assert_eq!(
            row.left_binding_power().map(u64::from),
            left,
            "{matcher} left binding power"
        );
        assert_eq!(
            row.right_binding_power().map(u64::from),
            right,
            "{matcher} right binding power"
        );
    }
}

#[test]
fn compiler_event_kind_surface_is_exactly_w2_plus_the_two_w3_additions() {
    let actual = compiler_event_kind_variants();
    assert_eq!(
        actual.len(),
        12,
        "compiler EventKind variants must be unique and closed"
    );
    assert_eq!(
        actual.into_iter().collect::<BTreeSet<_>>(),
        BTreeSet::from([
            "ChoiceBranch",
            "EndInput",
            "EnterField",
            "EnterRule",
            "LeaveField",
            "LeaveRule",
            "Literal",
            "PrattReduce",
            "Regex",
            "RepeatCount",
            "Transform",
            "Trivia",
        ]),
        "W3 adds only Trivia and PrattReduce; a duplicate PrattOperator surface is forbidden"
    );
}

#[test]
fn analysis_surface_adds_no_first_or_exact_language_difference_carrier() {
    for mutation in [
        "pub struct FirstSet;",
        "pub enum First { Empty }",
        "pub struct Facts { pub first: Vec<u32> }",
        "pub mod first {}",
        "pub fn first() -> Vec<u32> { Vec::new() }",
        "fn local() { let first = (); }",
        "fn local() { let (first, _) = ((), ()); }",
        "fn local(value: Option<()>) { if let Some(first) = value { let _ = first; } }",
        "fn local(values: Vec<()>) { for first in values { let _ = first; } }",
        "pub type ExactLanguageDifference = ();",
        "pub struct Facts { pub difference_is_exact: bool }",
        "pub const FIRST_SET: &[u8] = &[];",
        "pub static EXACT_LANGUAGE_DIFFERENCE: () = ();",
        "fn local() { let first_set = (); }",
        "trait T { const FIRST_SET: (); }",
        "struct T; impl T { const EXACT_LANGUAGE_DIFFERENCE: () = (); }",
        "macro_rules! hidden { () => { pub struct FirstSet; } } hidden!();",
        "macro_rules! hidden { () => { pub type ExactLanguageDifference = (); } } hidden!();",
        "macro_rules! hidden { () => { let first = (); } }",
        "use external::FirstSet as Facts;",
        "use external::{ExactLanguageDifference as Facts};",
        "type Facts = external::FirstSet;",
        "fn solve(input: external::ExactLanguageDifference) { let _ = input; }",
        "struct Facts { carrier: external::FirstSet }",
    ] {
        assert!(
            !forbidden_analysis_carriers(mutation).is_empty(),
            "negative semantic-surface verifier missed `{mutation}`"
        );
    }
    for ordinary_first_api in [
        "fn ordinary(values: &[u8]) { let _ = values.first(); }",
        "fn ordinary(values: &mut [u8]) { let _ = values.first_mut(); }",
        "fn ordinary(values: &[u8]) { let head = values.first(); let _ = head; }",
        "fn ordinary(values: &[u8]) { assert!(values.first().is_none()); }",
        "macro_rules! ordinary { ($values:expr) => { $values.first() } }",
        "fn ordinary() { println!(\"first\"); }",
        "fn ordinary(values: &[u8]) -> Option<&u8> { values.first() }",
        "type FirstByte = u8;",
        "use std::slice::Iter as FirstIterator;",
    ] {
        assert!(
            forbidden_analysis_carriers(ordinary_first_api).is_empty(),
            "ordinary collection first APIs are not FIRST-analysis carriers: {ordinary_first_api}"
        );
    }
    for include_escape in [
        "include!(\"opaque.inc\");",
        "const HIDDEN: () = include!(\"opaque.inc\");",
        "fn hidden() { include!(\"opaque.inc\"); }",
        "fn hidden() { let _ = { include!(\"opaque.inc\") }; }",
        "fn hidden() { match 0 { _ => include!(\"opaque.inc\") } }",
        "macro_rules! hidden { () => { include!(\"opaque.inc\") } }",
    ] {
        let file = syn::parse_file(include_escape).expect("include escape mutation syntax");
        assert_ne!(
            include_escape_count(&file),
            (0, 0),
            "active-source closure missed include escape `{include_escape}`"
        );
    }
    let literal_only = syn::parse_file(
        "fn ordinary() { let text = \"include! is documentation\"; println!(\"include!\"); }",
    )
    .expect("ordinary include text syntax");
    assert_eq!(
        include_escape_count(&literal_only),
        (0, 0),
        "include text inside literals is not a source-closure escape"
    );

    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let lib = manifest_dir.join("src/lib.rs");
    let root_source = fs::read_to_string(&lib).expect("read active crate root");
    let root = syn::parse_file(&root_source).expect("active crate root is syntactic Rust");
    assert_eq!(
        root.items
            .iter()
            .filter_map(|item| match item {
                syn::Item::Mod(module) => Some(module.ident.to_string()),
                _ => None,
            })
            .collect::<Vec<_>>(),
        ["compiler"],
        "the production crate root exposes only the compiler module graph"
    );
    let files = active_compiler_source_closure(&lib);
    let mut physical = vec![lib];
    compiler_rust_sources(&manifest_dir.join("src/compiler"), &mut physical);
    physical.sort();
    physical.dedup();
    assert_eq!(
        files, physical,
        "the exact active compiler module graph equals the complete compiler source closure"
    );
    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        assert!(
            forbidden_analysis_carriers(&source).is_empty(),
            "{} adds a forbidden FIRST or exact-language-Difference carrier: {:?}",
            path.display(),
            forbidden_analysis_carriers(&source)
        );
    }
}

#[test]
fn two_pratt_sites_have_compact_distinct_closed_descriptor_ranges() {
    let module = accepted(MULTI_PRATT_MANIFEST, MULTI_PRATT_SOURCE);
    assert_generated_v6_canonical(&module, false);
    let tables = module.lowering().pratt_tables();
    let operators = module.lowering().pratt_operators();
    assert_eq!(tables.len(), 2);
    assert_eq!(operators.len(), 2);

    for (index, table) in tables.iter().enumerate() {
        assert_eq!(table.id().index(), index);
        assert_eq!(table.id().revision(), module.program().revision());
        assert_eq!(table.operators().count(), 1);
        let operator = table.operators().next().expect("one operator per table");
        assert_eq!(operator.index(), index);
        assert_eq!(
            module
                .lowering()
                .pratt_operator(operator)
                .expect("table range resolves in the operator carrier")
                .table(),
            table.id(),
            "operator backreference closes over its compact table range"
        );
    }
    assert_ne!(tables[0].site_rule(), tables[1].site_rule());
    assert_ne!(tables[0].atom_rule(), tables[1].atom_rule());
    assert_ne!(tables[0].site_rule(), tables[0].atom_rule());
    assert_ne!(tables[1].site_rule(), tables[1].atom_rule());
    assert_eq!(tables[0].site_rule(), rule(&module, "product_expression"));
    assert_eq!(tables[0].atom_rule(), rule(&module, "product_atom"));
    assert_eq!(tables[1].site_rule(), rule(&module, "sum_expression"));
    assert_eq!(tables[1].atom_rule(), rule(&module, "sum_atom"));
    assert_eq!(
        operators[0].matcher_rule(),
        rule(&module, "product_operator")
    );
    assert_eq!(operators[1].matcher_rule(), rule(&module, "sum_operator"));

    let generated_source = module.generated().source();
    let (table_fields, table_getters) =
        generated_descriptor_surface(generated_source, "PrattTableDescriptor");
    assert_eq!(
        table_fields,
        BTreeSet::from([
            "atom_rule".to_owned(),
            "id".to_owned(),
            "operator_len".to_owned(),
            "operator_start".to_owned(),
            "site_rule".to_owned(),
        ])
    );
    assert_eq!(table_getters, table_fields);
    let (operator_fields, operator_getters) =
        generated_descriptor_surface(generated_source, "PrattOperatorDescriptor");
    assert_eq!(
        operator_fields,
        BTreeSet::from([
            "fixity".to_owned(),
            "id".to_owned(),
            "left_binding_power".to_owned(),
            "matcher_rule".to_owned(),
            "right_binding_power".to_owned(),
            "table".to_owned(),
        ])
    );
    assert_eq!(operator_getters, operator_fields);

    // The generated descriptor product must be ordinal, not a second
    // name/path authority. This stronger whole-product scan necessarily
    // covers the public Pratt descriptor declarations and initializers.
    let generated = format!("{}\n{}", generated_source, module.generated().manifest());
    for forbidden in [
        "contracts/w3/two-site-pratt-laws.bbnf",
        "sum_expression",
        "sum_atom",
        "sum_operator",
        "product_expression",
        "product_atom",
        "product_operator",
    ] {
        assert!(
            !generated.contains(forbidden),
            "generated Pratt product leaked rule-name/logical-path authority `{forbidden}`"
        );
    }
}

#[test]
fn valid_entry_disconnected_pratt_table_is_retained_but_not_on_the_public_path() {
    let module = accepted(UNUSED_PRATT_MANIFEST, UNUSED_PRATT_SOURCE);
    assert_eq!(module.lowering().entry(), rule(&module, "document"));
    assert!(
        dependency_names(&module, rule(&module, "document")).is_empty(),
        "the selected entry is deliberately disconnected from the Pratt site"
    );
    let tables = module.lowering().pratt_tables();
    let operators = module.lowering().pratt_operators();
    assert_eq!(tables.len(), 1, "unused valid table is retained");
    assert_eq!(operators.len(), 1, "unused valid operator is retained");
    assert_eq!(tables[0].site_rule(), rule(&module, "unused_expression"));
    assert_eq!(tables[0].atom_rule(), rule(&module, "unused_atom"));
    assert_eq!(operators[0].matcher_rule(), rule(&module, "unused_sum"));
    assert_generated_v6_canonical(&module, false);
}

#[test]
fn pratt_rejects_non_consuming_atom_and_matcher_languages() {
    const EMPTY_ATOM_SOURCE: &[u8] = br#"
document = expression;
expression = empty;
empty = epsilon;
plus = "+";
"#;
    const EMPTY_ATOM_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/empty-atom.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": [{
    "site_rule": "expression",
    "atom_rule": "empty",
    "operators": [{
      "matcher_rule": "plus",
      "fixity": "infix",
      "left_binding_power": 10,
      "right_binding_power": 11
    }]
  }]
}"#;
    assert_rejected_with(
        EMPTY_ATOM_MANIFEST,
        EMPTY_ATOM_SOURCE,
        DiagnosticCode::InvalidPrattTable,
    );

    const EMPTY_MATCHER_SOURCE: &[u8] = br#"
document = expression;
expression = atom;
atom = "x";
empty = epsilon;
"#;
    const EMPTY_MATCHER_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/empty-matcher.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": [{
    "site_rule": "expression",
    "atom_rule": "atom",
    "operators": [{
      "matcher_rule": "empty",
      "fixity": "postfix",
      "left_binding_power": 10,
      "right_binding_power": null
    }]
  }]
}"#;
    assert_rejected_with(
        EMPTY_MATCHER_MANIFEST,
        EMPTY_MATCHER_SOURCE,
        DiagnosticCode::InvalidPrattTable,
    );
}

#[test]
fn pratt_rejects_every_direct_indirect_and_recursive_nullable_language_shape() {
    let atom_cases = [
        (
            "indirect",
            "atom = atom_indirect;\natom_indirect = epsilon;\n",
        ),
        ("optional", "atom = \"x\"?;\n"),
        ("repeat", "atom = \"x\"*;\n"),
        ("nullable-regex", "atom = /x*/;\n"),
        (
            "recursive-nullable",
            "atom = atom_nullable_a;\n\
             atom_nullable_a = epsilon | atom_nullable_b;\n\
             atom_nullable_b = atom_nullable_a;\n",
        ),
    ];
    for (shape, atom_rules) in atom_cases {
        let source =
            format!("document = expression;\nexpression = atom;\n{atom_rules}matcher = \"+\";\n");
        let manifest =
            one_operator_pratt_manifest(&format!("contracts/w3/nullable-atom-{shape}.bbnf"));
        assert_rejected_with(
            &manifest,
            source.as_bytes(),
            DiagnosticCode::InvalidPrattTable,
        );
    }

    let matcher_cases = [
        (
            "indirect",
            "matcher = matcher_indirect;\nmatcher_indirect = epsilon;\n",
        ),
        ("optional", "matcher = \"+\"?;\n"),
        ("repeat", "matcher = \"+\"*;\n"),
        ("nullable-regex", "matcher = /\\+*/;\n"),
        (
            "recursive-nullable",
            "matcher = matcher_nullable_a;\n\
             matcher_nullable_a = epsilon | matcher_nullable_b;\n\
             matcher_nullable_b = matcher_nullable_a;\n",
        ),
    ];
    for (shape, matcher_rules) in matcher_cases {
        let source =
            format!("document = expression;\nexpression = atom;\natom = \"x\";\n{matcher_rules}");
        let manifest =
            one_operator_pratt_manifest(&format!("contracts/w3/nullable-matcher-{shape}.bbnf"));
        assert_rejected_with(
            &manifest,
            source.as_bytes(),
            DiagnosticCode::InvalidPrattTable,
        );
    }
}

#[test]
fn pratt_rejects_nonnullable_empty_atom_and_matcher_languages() {
    for (role, atom_rule, matcher_rule) in [
        ("atom", "atom = \"x\" - epsilon;\n", "matcher = \"+\";\n"),
        ("matcher", "atom = \"x\";\n", "matcher = \"+\" - epsilon;\n"),
    ] {
        let source =
            format!("document = expression;\nexpression = atom;\n{atom_rule}{matcher_rule}");
        let manifest =
            one_operator_pratt_manifest(&format!("contracts/w3/empty-{role}-language.bbnf"));
        assert_rejected_with(
            &manifest,
            source.as_bytes(),
            DiagnosticCode::InvalidPrattTable,
        );
    }
}

fn exponentially_consuming_rule_source() -> String {
    let mut source = String::from("r0 = \"x\";\n");
    for depth in 1..=63 {
        writeln!(source, "r{depth} = r{}, r{};", depth - 1, depth - 1)
            .expect("writing to String is infallible");
    }
    source.push_str("expensive = ");
    for depth in (0..=63).rev() {
        if depth != 63 {
            source.push_str(", ");
        }
        write!(source, "r{depth}").expect("writing to String is infallible");
    }
    source.push_str(";\n");
    source
}

#[test]
fn projection_checked_sum_reports_overflow_from_either_discarded_side() {
    const MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/projection-overflow.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": []
}"#;

    let mut expensive_left_discarded = exponentially_consuming_rule_source();
    expensive_left_discarded.push_str("document = expensive >> \"y\";\n");
    let mut expensive_right_discarded = exponentially_consuming_rule_source();
    expensive_right_discarded.push_str("document = \"y\" << expensive;\n");

    for source in [expensive_left_discarded, expensive_right_discarded] {
        assert_rejected_with(
            MANIFEST,
            source.as_bytes(),
            DiagnosticCode::MinimumConsumptionOverflow,
        );
    }
}

#[test]
fn difference_excluded_child_minimum_overflow_is_not_pruned() {
    const MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/difference-excluded-overflow.bbnf",
  "entry": "document",
  "trivia_regex": null,
  "pratt_tables": []
}"#;

    let mut source = exponentially_consuming_rule_source();
    source.push_str(
        "overflowing_exclusion = expensive, \"z\";\n\
         document = \"y\" - overflowing_exclusion;\n",
    );
    assert_rejected_with(
        MANIFEST,
        source.as_bytes(),
        DiagnosticCode::MinimumConsumptionOverflow,
    );
}

const GENERATED_RUNTIME_MAIN: &str = r#"
mod pratt {
    include!("../pratt_generated.rs");
}
mod pratt_without_probe {
    include!("../pratt_without_probe_generated.rs");
}
mod pratt_disabled {
    include!("../pratt_disabled_generated.rs");
}
mod pratt_direct {
    include!("../pratt_direct_generated.rs");
}
mod prefix_one_baseline {
    include!("../prefix_one_baseline_generated.rs");
}
mod prefix_two_baseline {
    include!("../prefix_two_baseline_generated.rs");
}
mod infix_one_baseline {
    include!("../infix_one_baseline_generated.rs");
}
mod infix_two_baseline {
    include!("../infix_two_baseline_generated.rs");
}
mod pratt_overlap_swapped {
    include!("../pratt_overlap_swapped_generated.rs");
}
mod pratt_prefix_cross {
    include!("../pratt_prefix_cross_generated.rs");
}
mod pratt_postfix_cross {
    include!("../pratt_postfix_cross_generated.rs");
}
mod pratt_infix_cross {
    include!("../pratt_infix_cross_generated.rs");
}
mod affix_overlap {
    include!("../affix_overlap_generated.rs");
}
mod affix_prefix_without_probe {
    include!("../affix_prefix_without_probe_generated.rs");
}
mod affix_postfix_without_probe {
    include!("../affix_postfix_without_probe_generated.rs");
}
mod affix_prefix_swapped {
    include!("../affix_prefix_swapped_generated.rs");
}
mod affix_postfix_swapped {
    include!("../affix_postfix_swapped_generated.rs");
}
mod multi_pratt {
    include!("../multi_pratt_generated.rs");
}
mod unused_pratt {
    include!("../unused_pratt_generated.rs");

    pub fn exercise_disconnected_table_site() {
        let source = Input::new(b"1+1");
        let limits = Limits::new(LimitsConfig::default().bounded());
        let result =
            Run::with_sink(source, EventBuffer::new(source)).parse_module(|run| {
                sequence(run, |run| {
                    match node___UNUSED_PRATT_SITE_NODE__(run, &limits) {
                        Step::Matched(()) => {}
                        Step::Mismatch(failure) => return Step::Mismatch(failure),
                        Step::Fault(fault) => return Step::Fault(fault),
                    }
                    match eof(run) {
                        Step::Matched(_) => Step::Matched(()),
                        Step::Mismatch(failure) => Step::Mismatch(failure),
                        Step::Fault(fault) => Step::Fault(fault),
                    }
                })
            });
        match result.outcome() {
            Step::Matched(span) => {
                assert_eq!(span.start(), 0);
                assert_eq!(span.end(), 3);
            }
            other => panic!("disconnected Pratt site did not execute: {other:?}"),
        }
        let reductions = result
            .sink()
            .events()
            .iter()
            .filter(|event| event.kind() == EventKind::PrattReduce)
            .collect::<Vec<_>>();
        assert_eq!(reductions.len(), 1);
        assert_eq!(reductions[0].operator(), Some(0));
        assert_eq!((reductions[0].start(), reductions[0].end()), (0, 3));
        assert_eq!(reductions[0].value(), 0);
    }
}
mod event_surface {
    include!("../event_surface_generated.rs");
}
mod semantic {
    include!("../semantic_generated.rs");
}
mod trim_only {
    include!("../trim_only_generated.rs");
}
mod difference_only {
    include!("../difference_generated.rs");
}
mod difference_nullable_exclusion {
    include!("../difference_nullable_exclusion_generated.rs");
}
mod difference_shallow {
    include!("../difference_shallow_generated.rs");
}
mod difference_excluded_shallow {
    include!("../difference_excluded_shallow_generated.rs");
}
mod difference_excluded_deep {
    include!("../difference_excluded_deep_generated.rs");
}
mod difference_primary_shallow {
    include!("../difference_primary_shallow_generated.rs");
}
mod difference_primary_deep {
    include!("../difference_primary_deep_generated.rs");
}
mod project_right {
    include!("../project_right_generated.rs");
}
mod baseline_right {
    include!("../baseline_right_generated.rs");
}
mod project_left {
    include!("../project_left_generated.rs");
}
mod baseline_left {
    include!("../baseline_left_generated.rs");
}

use parse_that::{EventSink, Fault, ResourceKind, Span, Step};
use std::collections::BTreeSet;

const _: () = assert!(parse_that::KERNEL_ABI == 3);
const PRATT_NODE: u32 = __PRATT_NODE__;
const PRATT_DOCUMENT_TRIM: u32 = __PRATT_DOCUMENT_TRIM__;
const PRATT_NUMBER_TRIM: u32 = __PRATT_NUMBER_TRIM__;
const PRATT_SUM_TRIM: u32 = __PRATT_SUM_TRIM__;
const SEMANTIC_TRIM: u32 = __SEMANTIC_TRIM__;
const SEMANTIC_TRIM_AGAIN: u32 = __SEMANTIC_TRIM_AGAIN__;
const SEMANTIC_NULLABLE_TRIM: u32 = __SEMANTIC_NULLABLE_TRIM__;
const PROJECT_RIGHT_RULE: u32 = __PROJECT_RIGHT_RULE__;
const BASELINE_RIGHT_RULE: u32 = __BASELINE_RIGHT_RULE__;
const PROJECT_LEFT_RULE: u32 = __PROJECT_LEFT_RULE__;
const BASELINE_LEFT_RULE: u32 = __BASELINE_LEFT_RULE__;
const MULTI_SUM_SITE_RULE: u32 = __MULTI_SUM_SITE_RULE__;
const MULTI_SUM_ATOM_RULE: u32 = __MULTI_SUM_ATOM_RULE__;
const MULTI_SUM_OPERATOR_RULE: u32 = __MULTI_SUM_OPERATOR_RULE__;
const MULTI_PRODUCT_SITE_RULE: u32 = __MULTI_PRODUCT_SITE_RULE__;
const MULTI_PRODUCT_ATOM_RULE: u32 = __MULTI_PRODUCT_ATOM_RULE__;
const MULTI_PRODUCT_OPERATOR_RULE: u32 = __MULTI_PRODUCT_OPERATOR_RULE__;

fn assert_matched(step: &Step<'_, Span<'_>>, input_len: usize) {
    match step {
        Step::Matched(span) => {
            assert_eq!(span.start(), 0);
            assert_eq!(span.end(), input_len);
        }
        other => panic!("expected complete match, got {other:?}"),
    }
}

fn assert_mismatch_evidence_at(step: &Step<'_, Span<'_>>, at: usize) {
    assert_eq!(mismatch_evidence_offset(step), at);
}

fn mismatch_evidence_offset(step: &Step<'_, Span<'_>>) -> usize {
    match step {
        Step::Mismatch(failure) => failure.at().offset(),
        other => panic!("expected mismatch diagnostic evidence, got {other:?}"),
    }
}

fn assert_resource(
    step: &Step<'_, Span<'_>>,
    expected_kind: ResourceKind,
    expected_limit: usize,
) {
    match step {
        Step::Fault(Fault::ResourceLimit {
            kind,
            limit,
            at: _,
        }) => {
            assert_eq!(*kind, expected_kind);
            assert_eq!(*limit, expected_limit);
        }
        other => panic!(
            "expected {expected_kind:?} resource fault at limit {expected_limit}, got {other:?}"
        ),
    }
}

fn resource_fault_offset(step: &Step<'_, Span<'_>>, expected_kind: ResourceKind) -> Option<usize> {
    match step {
        Step::Fault(Fault::ResourceLimit { kind, at, .. }) if *kind == expected_kind => {
            Some(at.offset())
        }
        _ => None,
    }
}

fn reductions(input: &[u8]) -> Vec<(u32, u32, u32)> {
    let result = pratt::parse_events(input);
    assert_matched(result.outcome(), input.len());
    result
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == pratt::EventKind::PrattReduce)
        .map(|event| {
            assert_eq!(event.node(), Some(PRATT_NODE));
            assert_eq!(event.value(), 0);
            (
                event.operator().expect("PrattReduce has an operator"),
                event.start(),
                event.end(),
            )
        })
        .collect()
}

macro_rules! module_reductions {
    ($module:ident, $input:expr) => {{
        let result = $module::parse_events($input);
        assert_matched(result.outcome(), $input.len());
        result
            .sink()
            .events()
            .iter()
            .filter(|event| event.kind() == $module::EventKind::PrattReduce)
            .map(|event| {
                (
                    event.operator().expect("PrattReduce operator identity"),
                    event.start(),
                    event.end(),
                )
            })
            .collect::<Vec<_>>()
    }};
}

fn assert_pratt_parity(input: &[u8]) {
    let retained = pratt::parse_events(input);
    let no_sink = pratt::check(input);
    assert_eq!(
        format!("{:?}", retained.outcome()),
        format!("{:?}", no_sink.outcome()),
        "retaining and NoSink outcomes diverged for {:?}",
        String::from_utf8_lossy(input)
    );
    assert_eq!(
        retained.sink().events().len(),
        EventSink::len(no_sink.sink()),
        "retaining and NoSink logical event counts diverged"
    );
}

macro_rules! minimum_work {
    ($module:ident, $default:ident, $limited:ident, $input:expr, $terminal:pat) => {{
        let input = $input;
        let mut low = 0;
        let mut high = $module::LimitsConfig::default().work();
        assert!(
            matches!($module::$default(input).outcome(), $terminal),
            "default work limit did not reach the expected terminal state"
        );
        while low < high {
            let middle = low + (high - low) / 2;
            let result = $module::$limited(
                input,
                $module::LimitsConfig::default().with_work(middle),
            );
            if matches!(result.outcome(), $terminal) {
                high = middle;
            } else {
                low = middle + 1;
            }
        }
        low
    }};
}

macro_rules! minimum_work_both {
    ($module:ident, $input:expr, $terminal:pat) => {{
        let retained = minimum_work!(
            $module,
            parse_events,
            parse_events_with_limits,
            $input,
            $terminal
        );
        let no_sink =
            minimum_work!($module, check, check_with_limits, $input, $terminal);
        assert_eq!(
            retained, no_sink,
            "retaining and NoSink minimum-work thresholds diverged"
        );
        retained
    }};
}

macro_rules! minimum_nesting {
    ($module:ident, $default:ident, $limited:ident, $input:expr) => {{
        let input = $input;
        let mut low = 0;
        let mut high = $module::LimitsConfig::default().nesting();
        assert_matched($module::$default(input).outcome(), input.len());
        while low < high {
            let middle = low + (high - low) / 2;
            let result = $module::$limited(
                input,
                $module::LimitsConfig::default().with_nesting(middle),
            );
            if matches!(result.outcome(), Step::Matched(_)) {
                high = middle;
            } else {
                low = middle + 1;
            }
        }
        low
    }};
}

macro_rules! assert_exact_work_boundary {
    ($module:ident, $input:expr, $terminal:pat, $cap:expr) => {{
        let cap = $cap;
        assert!(cap > 0);
        let exact = $module::LimitsConfig::default().with_work(cap);
        assert!(matches!(
            $module::parse_events_with_limits($input, exact).outcome(),
            $terminal
        ));
        assert!(matches!(
            $module::check_with_limits($input, exact).outcome(),
            $terminal
        ));
        let under = $module::LimitsConfig::default().with_work(cap - 1);
        assert_resource(
            $module::parse_events_with_limits($input, under).outcome(),
            ResourceKind::Work,
            cap - 1,
        );
        assert_resource(
            $module::check_with_limits($input, under).outcome(),
            ResourceKind::Work,
            cap - 1,
        );
    }};
}

macro_rules! assert_exact_nesting_boundary {
    ($module:ident, $input:expr, $cap:expr) => {{
        let cap = $cap;
        assert!(cap > 0);
        let exact = $module::LimitsConfig::default().with_nesting(cap);
        assert_matched(
            $module::parse_events_with_limits($input, exact).outcome(),
            $input.len(),
        );
        assert_matched(
            $module::check_with_limits($input, exact).outcome(),
            $input.len(),
        );
        let under = $module::LimitsConfig::default().with_nesting(cap - 1);
        assert_resource(
            $module::parse_events_with_limits($input, under).outcome(),
            ResourceKind::Nesting,
            cap - 1,
        );
        assert_resource(
            $module::check_with_limits($input, under).outcome(),
            ResourceKind::Nesting,
            cap - 1,
        );
    }};
}

macro_rules! event_kind_tag {
    ($module:ident, $kind:expr) => {
        match $kind {
            $module::EventKind::EnterRule => 0_u8,
            $module::EventKind::LeaveRule => 1,
            $module::EventKind::EnterField => 2,
            $module::EventKind::LeaveField => 3,
            $module::EventKind::Literal => 4,
            $module::EventKind::Regex => 5,
            $module::EventKind::ChoiceBranch => 6,
            $module::EventKind::RepeatCount => 7,
            $module::EventKind::Transform => 8,
            $module::EventKind::Trivia => 9,
            $module::EventKind::PrattReduce => 10,
            $module::EventKind::EndInput => 11,
        }
    };
}

macro_rules! observe_operator_surface {
    ($module:ident, $input:expr, $observed:ident) => {{
        let result = $module::parse_events($input);
        assert_matched(result.outcome(), $input.len());
        for event in result.sink().events() {
            let name = match event.kind() {
                $module::EventKind::EnterRule => {
                    assert_eq!(event.operator(), None);
                    "EnterRule"
                }
                $module::EventKind::LeaveRule => {
                    assert_eq!(event.operator(), None);
                    "LeaveRule"
                }
                $module::EventKind::EnterField => {
                    assert_eq!(event.operator(), None);
                    "EnterField"
                }
                $module::EventKind::LeaveField => {
                    assert_eq!(event.operator(), None);
                    "LeaveField"
                }
                $module::EventKind::Literal => {
                    assert_eq!(event.operator(), None);
                    "Literal"
                }
                $module::EventKind::Regex => {
                    assert_eq!(event.operator(), None);
                    "Regex"
                }
                $module::EventKind::ChoiceBranch => {
                    assert_eq!(event.operator(), None);
                    "ChoiceBranch"
                }
                $module::EventKind::RepeatCount => {
                    assert_eq!(event.operator(), None);
                    "RepeatCount"
                }
                $module::EventKind::Transform => {
                    assert_eq!(event.operator(), None);
                    "Transform"
                }
                $module::EventKind::Trivia => {
                    assert_eq!(event.operator(), None);
                    "Trivia"
                }
                $module::EventKind::PrattReduce => {
                    assert!(event.operator().is_some());
                    "PrattReduce"
                }
                $module::EventKind::EndInput => {
                    assert_eq!(event.operator(), None);
                    "EndInput"
                }
            };
            $observed.insert(name);
        }
    }};
}

macro_rules! scalar_rows_without_outer_envelope {
    ($module:ident, $input:expr, $outer_rule:expr) => {{
        let result = $module::parse_events($input);
        assert_matched(result.outcome(), $input.len());
        let events = result.sink().events();
        let removed = events
            .iter()
            .filter(|event| {
                event.rule() == Some($outer_rule)
                    && matches!(
                        event.kind(),
                        $module::EventKind::EnterRule | $module::EventKind::LeaveRule
                    )
            })
            .count();
        assert_eq!(removed, 2, "only the outer entry-rule envelope differs");
        events
            .iter()
            .filter(|event| {
                !(event.rule() == Some($outer_rule)
                    && matches!(
                        event.kind(),
                        $module::EventKind::EnterRule | $module::EventKind::LeaveRule
                    ))
            })
            .map(|event| {
                (
                    event_kind_tag!($module, event.kind()),
                    event.rule(),
                    event.node(),
                    event.field(),
                    event.transform(),
                    event.operator(),
                    event.start(),
                    event.end(),
                    event.value(),
                )
            })
            .collect::<Vec<_>>()
    }};
}

fn main() {
    assert_eq!(pratt::PRATT_TABLES.len(), 1);
    assert_eq!(pratt::PRATT_OPERATORS.len(), 8);
    let table = &pratt::PRATT_TABLES[0];
    assert_eq!(table.id(), 0);
    assert_eq!(table.site_rule(), __EXPRESSION_RULE__);
    assert_eq!(table.atom_rule(), __ATOM_RULE__);
    assert_eq!(table.operator_start(), 0);
    assert_eq!(table.operator_len(), 8);
    let expected_operators = [
        (
            0,
            0,
            __PREFIX_RULE__,
            pratt::PrattFixity::Prefix,
            None,
            Some(70),
        ),
        (
            1,
            0,
            __POSTFIX_RULE__,
            pratt::PrattFixity::Postfix,
            Some(90),
            None,
        ),
        (
            2,
            0,
            __POWER_RULE__,
            pratt::PrattFixity::Infix,
            Some(80),
            Some(80),
        ),
        (
            3,
            0,
            __PRODUCT_RULE__,
            pratt::PrattFixity::Infix,
            Some(60),
            Some(61),
        ),
        (
            4,
            0,
            __SUM_RULE__,
            pratt::PrattFixity::Infix,
            Some(40),
            Some(41),
        ),
        (
            5,
            0,
            __PROBE_RULE__,
            pratt::PrattFixity::Infix,
            Some(30),
            Some(31),
        ),
        (
            6,
            0,
            __LONG_RULE__,
            pratt::PrattFixity::Infix,
            Some(20),
            Some(21),
        ),
        (
            7,
            0,
            __SHORT_RULE__,
            pratt::PrattFixity::Infix,
            Some(20),
            Some(21),
        ),
    ];
    for (operator, expected) in pratt::PRATT_OPERATORS.iter().zip(expected_operators) {
        let (id, table, matcher, fixity, left, right) = expected;
        assert_eq!(operator.id(), id);
        assert_eq!(operator.table(), table);
        assert_eq!(operator.matcher_rule(), matcher);
        assert_eq!(operator.fixity(), fixity);
        assert_eq!(operator.left_binding_power(), left);
        assert_eq!(operator.right_binding_power(), right);
    }

    assert_eq!(multi_pratt::PRATT_TABLES.len(), 2);
    assert_eq!(multi_pratt::PRATT_OPERATORS.len(), 2);
    let product_table = &multi_pratt::PRATT_TABLES[0];
    assert_eq!(product_table.id(), 0);
    assert_eq!(product_table.site_rule(), MULTI_PRODUCT_SITE_RULE);
    assert_eq!(product_table.atom_rule(), MULTI_PRODUCT_ATOM_RULE);
    assert_eq!(product_table.operator_start(), 0);
    assert_eq!(product_table.operator_len(), 1);
    let sum_table = &multi_pratt::PRATT_TABLES[1];
    assert_eq!(sum_table.id(), 1);
    assert_eq!(sum_table.site_rule(), MULTI_SUM_SITE_RULE);
    assert_eq!(sum_table.atom_rule(), MULTI_SUM_ATOM_RULE);
    assert_eq!(sum_table.operator_start(), 1);
    assert_eq!(sum_table.operator_len(), 1);
    assert_ne!(sum_table.site_rule(), product_table.site_rule());
    assert_ne!(sum_table.atom_rule(), product_table.atom_rule());
    for (index, (operator, matcher)) in multi_pratt::PRATT_OPERATORS
        .iter()
        .zip([MULTI_PRODUCT_OPERATOR_RULE, MULTI_SUM_OPERATOR_RULE])
        .enumerate()
    {
        assert_eq!(operator.id(), index as u32);
        assert_eq!(operator.table(), index as u32);
        assert_eq!(operator.matcher_rule(), matcher);
    }
    let multi = multi_pratt::parse_events(b"1+2;3*4");
    assert_matched(multi.outcome(), 7);
    let multi_reductions = multi
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == multi_pratt::EventKind::PrattReduce)
        .map(|event| event.operator().expect("two-site Pratt reduction operator"))
        .collect::<Vec<_>>();
    assert_eq!(
        multi_reductions,
        vec![1, 0],
        "one document execution must traverse both distinct Pratt sites"
    );
    assert_eq!(unused_pratt::PRATT_TABLES.len(), 1);
    assert_eq!(unused_pratt::PRATT_OPERATORS.len(), 1);
    assert_matched(unused_pratt::check(b"d").outcome(), 1);
    unused_pratt::exercise_disconnected_table_site();

    // Prefix and postfix compose at their declared binding powers. Reductions
    // are emitted only after the corresponding operator commits.
    assert_eq!(reductions(b"-2!"), vec![(1, 1, 3), (0, 0, 3)]);
    assert_eq!(reductions(b"--2"), vec![(0, 1, 3), (0, 0, 3)]);
    assert_eq!(reductions(b"2!!"), vec![(1, 0, 2), (1, 0, 3)]);
    let repeated_prefix_nesting =
        minimum_nesting!(pratt, parse_events, parse_events_with_limits, b"--2");
    let repeated_prefix_no_sink_nesting =
        minimum_nesting!(pratt, check, check_with_limits, b"--2");
    let repeated_postfix_nesting =
        minimum_nesting!(pratt, parse_events, parse_events_with_limits, b"2!!");
    let repeated_postfix_no_sink_nesting =
        minimum_nesting!(pratt, check, check_with_limits, b"2!!");
    assert_eq!(repeated_prefix_nesting, repeated_prefix_no_sink_nesting);
    assert_eq!(repeated_postfix_nesting, repeated_postfix_no_sink_nesting);
    assert!(
        repeated_prefix_nesting > repeated_postfix_nesting,
        "recursive prefix parsing must grow nesting beyond an equal-length iterative postfix chain"
    );
    assert_exact_nesting_boundary!(pratt, b"--2", repeated_prefix_nesting);
    assert_exact_nesting_boundary!(pratt, b"2!!", repeated_postfix_nesting);

    // Multiplication binds more tightly than addition.
    assert_eq!(reductions(b"1+2*3"), vec![(3, 2, 5), (4, 0, 5)]);

    // Equal powers are right associative; right = left + 1 is left
    // associative.
    assert_eq!(reductions(b"1^2^3"), vec![(2, 2, 5), (2, 0, 5)]);
    assert_eq!(reductions(b"1+2+3"), vec![(4, 0, 3), (4, 0, 5)]);

    // Recursive atom references preserve parentheses as a complete operand.
    assert_eq!(reductions(b"(1+2)*3"), vec![(4, 1, 4), (3, 0, 7)]);

    // The first matching manifest row wins for overlapping matchers.
    assert_eq!(reductions(b"1<<2"), vec![(6, 0, 4)]);
    let swapped_overlap = pratt_overlap_swapped::parse_events(b"1<<2");
    assert_mismatch_evidence_at(swapped_overlap.outcome(), 2);
    assert!(swapped_overlap.sink().events().is_empty());
    assert_eq!(
        EventSink::len(pratt_overlap_swapped::check(b"1<<2").sink()),
        0
    );

    // These generated controls use identical grammar bytes and mutate one
    // manifest binding-power cell apiece. Each crossing changes the reduction
    // order, proving the manifest—not a hardcoded precedence table—drives the
    // executed parser.
    assert_eq!(reductions(b"-2*3"), vec![(0, 0, 2), (3, 0, 4)]);
    assert_eq!(
        module_reductions!(pratt_prefix_cross, b"-2*3"),
        vec![(3, 1, 4), (0, 0, 4)]
    );
    assert_eq!(reductions(b"1^2!"), vec![(1, 2, 4), (2, 0, 4)]);
    assert_eq!(
        module_reductions!(pratt_postfix_cross, b"1^2!"),
        vec![(2, 0, 3), (1, 0, 4)]
    );
    assert_eq!(
        module_reductions!(pratt_infix_cross, b"1^2^3"),
        vec![(2, 0, 3), (2, 0, 5)]
    );

    // Prefix and postfix overlap use the same manifest-order, partial-probe,
    // rollback, parity, and monotone-work laws as infix overlap.
    assert_eq!(
        module_reductions!(affix_overlap, b"-!1"),
        vec![(0, 0, 3)]
    );
    assert_eq!(
        module_reductions!(affix_overlap, b"1!?"),
        vec![(2, 0, 3)]
    );
    assert_eq!(
        module_reductions!(affix_overlap, b"-1"),
        vec![(1, 0, 2)]
    );
    assert_eq!(
        module_reductions!(affix_overlap, b"1!"),
        vec![(3, 0, 2)]
    );
    let prefix_swapped = affix_prefix_swapped::parse_events(b"-!1");
    assert!(matches!(prefix_swapped.outcome(), Step::Mismatch(_)));
    assert!(prefix_swapped.sink().events().is_empty());
    let postfix_swapped = affix_postfix_swapped::parse_events(b"1!?");
    assert!(matches!(postfix_swapped.outcome(), Step::Mismatch(_)));
    assert!(postfix_swapped.sink().events().is_empty());
    assert_eq!(
        EventSink::len(affix_prefix_swapped::check(b"-!1").sink()),
        0
    );
    assert_eq!(
        EventSink::len(affix_postfix_swapped::check(b"1!?").sink()),
        0
    );
    for (input, start, end) in [(&b"-1"[..], 0, 1), (&b"1!"[..], 1, 2)] {
        let retained = affix_overlap::parse_events(input);
        assert_matched(retained.outcome(), input.len());
        assert_eq!(
            retained
                .sink()
                .events()
                .iter()
                .filter(|event| {
                    event.kind() == affix_overlap::EventKind::Literal
                        && event.start() == start
                        && event.end() == end
                })
                .count(),
            1,
            "failed affix probe leaked a speculative literal"
        );
        let no_sink = affix_overlap::check(input);
        assert_eq!(
            format!("{:?}", retained.outcome()),
            format!("{:?}", no_sink.outcome())
        );
        assert_eq!(
            retained.sink().events().len(),
            EventSink::len(no_sink.sink())
        );
    }
    let prefix_overlap_work =
        minimum_work_both!(affix_overlap, b"-1", Step::Matched(_));
    let prefix_control_work =
        minimum_work_both!(affix_prefix_without_probe, b"-1", Step::Matched(_));
    assert!(
        prefix_overlap_work > prefix_control_work,
        "partially consuming failed prefix probe charges monotone work"
    );
    let postfix_overlap_work =
        minimum_work_both!(affix_overlap, b"1!", Step::Matched(_));
    let postfix_control_work =
        minimum_work_both!(affix_postfix_without_probe, b"1!", Step::Matched(_));
    assert!(
        postfix_overlap_work > postfix_control_work,
        "partially consuming failed postfix probe charges monotone work"
    );
    for (input, exact) in [
        (&b"-1"[..], prefix_overlap_work),
        (&b"1!"[..], postfix_overlap_work),
    ] {
        assert_resource(
            affix_overlap::parse_events_with_limits(
                input,
                affix_overlap::LimitsConfig::default().with_work(exact - 1),
            )
            .outcome(),
            ResourceKind::Work,
            exact - 1,
        );
        assert_resource(
            affix_overlap::check_with_limits(
                input,
                affix_overlap::LimitsConfig::default().with_work(exact - 1),
            )
            .outcome(),
            ResourceKind::Work,
            exact - 1,
        );
    }

    // probe_then_fail consumes '<' and then fails on '!'. Its recognition
    // position and literal event are rolled back before short_shift commits.
    assert_eq!(reductions(b"1<2"), vec![(7, 0, 3)]);
    let failed_probe = pratt::parse_events(b"1<2");
    assert_matched(failed_probe.outcome(), 3);
    let operator_literals = failed_probe
        .sink()
        .events()
        .iter()
        .filter(|event| {
            event.kind() == pratt::EventKind::Literal
                && event.start() == 1
                && event.end() == 2
        })
        .count();
    assert_eq!(
        operator_literals, 1,
        "the failed multi-token probe leaked a speculative literal event"
    );

    // The same manifest row succeeds for `<!`: operator 5 must commit as one
    // exact reduction rather than falling through to the short-shift row.
    let successful_probe_input = b"1<!2";
    assert_eq!(reductions(successful_probe_input), vec![(5, 0, 4)]);
    let successful_probe = pratt::parse_events(successful_probe_input);
    assert_matched(successful_probe.outcome(), successful_probe_input.len());
    assert_eq!(
        successful_probe
            .sink()
            .events()
            .iter()
            .filter(|event| event.kind() == pratt::EventKind::PrattReduce)
            .map(|event| {
                (
                    event.node(),
                    event.operator(),
                    event.start(),
                    event.end(),
                    event.value(),
                )
            })
            .collect::<Vec<_>>(),
        [(Some(PRATT_NODE), Some(5), 0, 4, 0)],
        "successful multi-token matcher retains the exact operator-5 reduction"
    );
    let successful_probe_no_sink = pratt::check(successful_probe_input);
    assert_matched(
        successful_probe_no_sink.outcome(),
        successful_probe_input.len(),
    );
    let successful_event_cap = successful_probe.sink().events().len();
    assert_eq!(
        EventSink::len(successful_probe_no_sink.sink()),
        successful_event_cap,
        "successful operator-5 path has retaining/NoSink logical-event parity"
    );
    assert!(successful_event_cap > 0);
    let successful_exact_events =
        pratt::LimitsConfig::default().with_logical_events(successful_event_cap);
    assert_matched(
        pratt::parse_events_with_limits(successful_probe_input, successful_exact_events).outcome(),
        successful_probe_input.len(),
    );
    assert_matched(
        pratt::check_with_limits(successful_probe_input, successful_exact_events).outcome(),
        successful_probe_input.len(),
    );
    let successful_under_events =
        pratt::LimitsConfig::default().with_logical_events(successful_event_cap - 1);
    assert_resource(
        pratt::parse_events_with_limits(successful_probe_input, successful_under_events).outcome(),
        ResourceKind::Events,
        successful_event_cap - 1,
    );
    assert_resource(
        pratt::check_with_limits(successful_probe_input, successful_under_events).outcome(),
        ResourceKind::Events,
        successful_event_cap - 1,
    );
    let successful_work =
        minimum_work_both!(pratt, successful_probe_input, Step::Matched(_));
    assert_exact_work_boundary!(
        pratt,
        successful_probe_input,
        Step::Matched(_),
        successful_work
    );

    // Missing RHS rejects and rolls back the whole Pratt invocation, including
    // all previously retained atom/operator events.
    let missing_rhs = pratt::parse_events(b"1+");
    assert_mismatch_evidence_at(missing_rhs.outcome(), 2);
    assert!(missing_rhs.sink().events().is_empty());
    let missing_rhs_no_sink = pratt::check(b"1+");
    assert_mismatch_evidence_at(missing_rhs_no_sink.outcome(), 2);
    assert_eq!(EventSink::len(missing_rhs_no_sink.sink()), 0);
    let recursive_missing_rhs = pratt::parse_events(b"(1+(2+))");
    assert_mismatch_evidence_at(recursive_missing_rhs.outcome(), 6);
    assert!(recursive_missing_rhs.sink().events().is_empty());
    let recursive_missing_rhs_no_sink = pratt::check(b"(1+(2+))");
    assert_mismatch_evidence_at(recursive_missing_rhs_no_sink.outcome(), 6);
    assert_eq!(EventSink::len(recursive_missing_rhs_no_sink.sink()), 0);
    let malformed_nested_prefix = pratt::parse_events(b"1+--");
    assert_mismatch_evidence_at(malformed_nested_prefix.outcome(), 4);
    assert!(malformed_nested_prefix.sink().events().is_empty());
    let malformed_nested_prefix_no_sink = pratt::check(b"1+--");
    assert_mismatch_evidence_at(malformed_nested_prefix_no_sink.outcome(), 4);
    assert_eq!(EventSink::len(malformed_nested_prefix_no_sink.sink()), 0);

    for input in [
        &b"-2!"[..],
        &b"1+2*3"[..],
        &b"1^2^3"[..],
        &b"1+2+3"[..],
        &b"(1+2)*3"[..],
        &b"1<<2"[..],
        &b"1<2"[..],
        &b"1<!2"[..],
        &b"1+"[..],
        &b"(1+(2+))"[..],
        &b"1+--"[..],
        &b"--2"[..],
        &b"2!!"[..],
    ] {
        assert_pratt_parity(input);
    }

    // The exhaustive match is also the generated EventKind closure gate:
    // W3 adds only Trivia and PrattReduce, and there is no PrattOperator event.
    // operator is absent from every non-reduction row. Trivia rows retain
    // exact leading/trailing spans and side identity (0 leading, 1 trailing).
    let spaced = pratt::parse_events(b"  1 + 2  ");
    assert_matched(spaced.outcome(), 9);
    for event in spaced.sink().events() {
        match event.kind() {
            pratt::EventKind::PrattReduce => assert!(event.operator().is_some()),
            pratt::EventKind::EnterRule
            | pratt::EventKind::LeaveRule
            | pratt::EventKind::EnterField
            | pratt::EventKind::LeaveField
            | pratt::EventKind::Literal
            | pratt::EventKind::Regex
            | pratt::EventKind::ChoiceBranch
            | pratt::EventKind::RepeatCount
            | pratt::EventKind::Transform
            | pratt::EventKind::Trivia
            | pratt::EventKind::EndInput => assert_eq!(event.operator(), None),
        }
    }
    let trivia = spaced
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == pratt::EventKind::Trivia)
        .map(|event| (event.node(), event.start(), event.end(), event.value()))
        .collect::<Vec<_>>();
    assert_eq!(
        trivia,
        vec![
            (Some(PRATT_DOCUMENT_TRIM), 0, 2, 0),
            (Some(PRATT_NUMBER_TRIM), 3, 4, 1),
            (Some(PRATT_SUM_TRIM), 5, 6, 1),
            (Some(PRATT_NUMBER_TRIM), 7, 9, 1),
        ]
    );
    let spaced_reductions = spaced
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == pratt::EventKind::PrattReduce)
        .map(|event| {
            (
                event.node(),
                event.operator(),
                event.start(),
                event.end(),
                event.value(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        spaced_reductions,
        vec![(Some(PRATT_NODE), Some(4), 2, 9, 0)]
    );
    let unspaced_pratt = pratt::parse_events(b"1+2");
    assert_matched(unspaced_pratt.outcome(), 3);
    assert_eq!(
        unspaced_pratt
            .sink()
            .events()
            .iter()
            .filter(|event| event.kind() == pratt::EventKind::Trivia)
            .count(),
        0,
        "zero-width Pratt trivia must not emit an event"
    );

    // Exact logical-event cap succeeds in both modes; one under faults in
    // both modes. NoSink counts logical events without constructing values.
    let cap_input = b"1<2";
    let baseline = pratt::parse_events(cap_input);
    assert_matched(baseline.outcome(), cap_input.len());
    let event_cap = baseline.sink().events().len();
    assert!(event_cap > 0);
    let exact_events = pratt::LimitsConfig::default().with_logical_events(event_cap);
    let retained_exact = pratt::parse_events_with_limits(cap_input, exact_events);
    let no_sink_exact = pratt::check_with_limits(cap_input, exact_events);
    assert_matched(retained_exact.outcome(), cap_input.len());
    assert_matched(no_sink_exact.outcome(), cap_input.len());
    assert_eq!(retained_exact.sink().events().len(), event_cap);
    assert_eq!(EventSink::len(no_sink_exact.sink()), event_cap);
    let one_under_events =
        pratt::LimitsConfig::default().with_logical_events(event_cap - 1);
    assert_resource(
        pratt::parse_events_with_limits(cap_input, one_under_events).outcome(),
        ResourceKind::Events,
        event_cap - 1,
    );
    assert_resource(
        pratt::check_with_limits(cap_input, one_under_events).outcome(),
        ResourceKind::Events,
        event_cap - 1,
    );

    // Discover the terminal threshold independently in each sink mode. The
    // failed partial matcher must charge monotone work even though recognition
    // state and events roll back: removing only that manifest row lowers the
    // independently discovered threshold.
    let retained_work = minimum_work!(
        pratt,
        parse_events,
        parse_events_with_limits,
        cap_input,
        Step::Matched(_)
    );
    let no_sink_work = minimum_work!(
        pratt,
        check,
        check_with_limits,
        cap_input,
        Step::Matched(_)
    );
    assert_eq!(retained_work, no_sink_work);
    let without_probe_retained_work = minimum_work!(
        pratt_without_probe,
        parse_events,
        parse_events_with_limits,
        cap_input,
        Step::Matched(_)
    );
    let without_probe_no_sink_work = minimum_work!(
        pratt_without_probe,
        check,
        check_with_limits,
        cap_input,
        Step::Matched(_)
    );
    assert_eq!(
        without_probe_retained_work, without_probe_no_sink_work,
        "failed-probe control must preserve retaining/NoSink work parity"
    );
    assert!(
        retained_work > without_probe_retained_work,
        "a partially consuming failed Pratt probe must charge monotone work"
    );
    let exact_work = pratt::LimitsConfig::default().with_work(retained_work);
    assert_matched(
        pratt::parse_events_with_limits(cap_input, exact_work).outcome(),
        cap_input.len(),
    );
    assert_matched(
        pratt::check_with_limits(cap_input, exact_work).outcome(),
        cap_input.len(),
    );
    let one_under_work =
        pratt::LimitsConfig::default().with_work(retained_work - 1);
    assert_resource(
        pratt::parse_events_with_limits(cap_input, one_under_work).outcome(),
        ResourceKind::Work,
        retained_work - 1,
    );
    assert_resource(
        pratt::check_with_limits(cap_input, one_under_work).outcome(),
        ResourceKind::Work,
        retained_work - 1,
    );

    // A manifest-only no-Pratt control parses the identical atom input. The
    // independently discovered deltas prove that a live Pratt invocation has
    // dedicated work and nesting charges, without assuming how the emitter
    // decomposes helpers or names functions.
    let atom_input = b"1";
    let pratt_atom_work = minimum_work!(
        pratt,
        parse_events,
        parse_events_with_limits,
        atom_input,
        Step::Matched(_)
    );
    let pratt_atom_no_sink_work = minimum_work!(
        pratt,
        check,
        check_with_limits,
        atom_input,
        Step::Matched(_)
    );
    let disabled_atom_work = minimum_work!(
        pratt_disabled,
        parse_events,
        parse_events_with_limits,
        atom_input,
        Step::Matched(_)
    );
    let disabled_atom_no_sink_work = minimum_work!(
        pratt_disabled,
        check,
        check_with_limits,
        atom_input,
        Step::Matched(_)
    );
    assert_eq!(pratt_atom_work, pratt_atom_no_sink_work);
    assert_eq!(disabled_atom_work, disabled_atom_no_sink_work);
    assert!(
        pratt_atom_work > disabled_atom_work,
        "live Pratt invocation must add dedicated monotone work"
    );
    assert_exact_work_boundary!(pratt, atom_input, Step::Matched(_), pratt_atom_work);
    assert_exact_work_boundary!(
        pratt_disabled,
        atom_input,
        Step::Matched(_),
        disabled_atom_work
    );

    let pratt_atom_nesting = minimum_nesting!(
        pratt,
        parse_events,
        parse_events_with_limits,
        atom_input
    );
    let pratt_atom_no_sink_nesting =
        minimum_nesting!(pratt, check, check_with_limits, atom_input);
    let disabled_atom_nesting = minimum_nesting!(
        pratt_disabled,
        parse_events,
        parse_events_with_limits,
        atom_input
    );
    let disabled_atom_no_sink_nesting =
        minimum_nesting!(pratt_disabled, check, check_with_limits, atom_input);
    assert_eq!(pratt_atom_nesting, pratt_atom_no_sink_nesting);
    assert_eq!(disabled_atom_nesting, disabled_atom_no_sink_nesting);
    assert!(
        pratt_atom_nesting > disabled_atom_nesting,
        "live Pratt invocation must add a dedicated nesting frame"
    );
    assert_exact_nesting_boundary!(pratt, atom_input, pratt_atom_nesting);
    assert_exact_nesting_boundary!(
        pratt_disabled,
        atom_input,
        disabled_atom_nesting
    );

    // Same-source explicit Sequence entries cancel the ordinary matcher,
    // atom, rule, and node work added by a longer expression. The remaining
    // Pratt-vs-Sequence delta must grow for each recursive prefix and infix
    // RHS invocation; an outer-only Pratt charge cannot satisfy either pair.
    let prefix_one_pratt_work =
        minimum_work_both!(pratt_direct, b"-2", Step::Matched(_));
    let prefix_one_baseline_work =
        minimum_work_both!(prefix_one_baseline, b"-2", Step::Matched(_));
    let prefix_two_pratt_work =
        minimum_work_both!(pratt_direct, b"--2", Step::Matched(_));
    let prefix_two_baseline_work =
        minimum_work_both!(prefix_two_baseline, b"--2", Step::Matched(_));
    let prefix_pratt_growth = prefix_two_pratt_work
        .checked_sub(prefix_one_pratt_work)
        .expect("a second prefix cannot reduce Pratt work");
    let prefix_baseline_growth = prefix_two_baseline_work
        .checked_sub(prefix_one_baseline_work)
        .expect("a second explicit prefix cannot reduce Sequence work");
    assert!(
        prefix_pratt_growth > prefix_baseline_growth,
        "each recursive prefix parse_expr invocation must add dedicated work"
    );
    assert_exact_work_boundary!(
        pratt_direct,
        b"--2",
        Step::Matched(_),
        prefix_two_pratt_work
    );

    let infix_one_pratt_work =
        minimum_work_both!(pratt_direct, b"1+2", Step::Matched(_));
    let infix_one_baseline_work =
        minimum_work_both!(infix_one_baseline, b"1+2", Step::Matched(_));
    let infix_two_pratt_work =
        minimum_work_both!(pratt_direct, b"1+2+3", Step::Matched(_));
    let infix_two_baseline_work =
        minimum_work_both!(infix_two_baseline, b"1+2+3", Step::Matched(_));
    let infix_pratt_growth = infix_two_pratt_work
        .checked_sub(infix_one_pratt_work)
        .expect("a second infix cannot reduce Pratt work");
    let infix_baseline_growth = infix_two_baseline_work
        .checked_sub(infix_one_baseline_work)
        .expect("a second explicit infix cannot reduce Sequence work");
    assert!(
        infix_pratt_growth > infix_baseline_growth,
        "each recursive infix RHS parse_expr invocation must add dedicated work"
    );
    assert_exact_work_boundary!(
        pratt_direct,
        b"1+2+3",
        Step::Matched(_),
        infix_two_pratt_work
    );

    // Right-associative recursive RHS parsing grows Pratt depth; the
    // equal-length left-associative chain reduces iteratively.
    let right_chain = b"1^2^3^4";
    let left_chain = b"1+2+3+4";
    let right_nesting = minimum_nesting!(
        pratt,
        parse_events,
        parse_events_with_limits,
        right_chain
    );
    let right_no_sink_nesting =
        minimum_nesting!(pratt, check, check_with_limits, right_chain);
    let left_nesting = minimum_nesting!(
        pratt,
        parse_events,
        parse_events_with_limits,
        left_chain
    );
    let left_no_sink_nesting =
        minimum_nesting!(pratt, check, check_with_limits, left_chain);
    assert_eq!(right_nesting, right_no_sink_nesting);
    assert_eq!(left_nesting, left_no_sink_nesting);
    assert!(
        right_nesting > left_nesting,
        "right-associative RHS recursion must grow nesting beyond a left-associative chain"
    );
    assert_exact_nesting_boundary!(pratt, right_chain, right_nesting);
    assert_exact_nesting_boundary!(pratt, left_chain, left_nesting);

    // Recursive atom parentheses participate in the same nesting budget.
    // Retaining and NoSink discover their terminal thresholds independently.
    let nested = b"(((1)))";
    let retained_nesting = minimum_nesting!(
        pratt,
        parse_events,
        parse_events_with_limits,
        nested
    );
    let no_sink_nesting =
        minimum_nesting!(pratt, check, check_with_limits, nested);
    assert_eq!(
        retained_nesting, no_sink_nesting,
        "retaining and NoSink must discover the same minimum nesting threshold"
    );
    let exact_nesting =
        pratt::LimitsConfig::default().with_nesting(retained_nesting);
    assert_matched(
        pratt::parse_events_with_limits(nested, exact_nesting).outcome(),
        nested.len(),
    );
    assert_matched(
        pratt::check_with_limits(nested, exact_nesting).outcome(),
        nested.len(),
    );
    let one_under_nesting =
        pratt::LimitsConfig::default().with_nesting(retained_nesting - 1);
    assert_resource(
        pratt::parse_events_with_limits(nested, one_under_nesting).outcome(),
        ResourceKind::Nesting,
        retained_nesting - 1,
    );
    assert_resource(
        pratt::check_with_limits(nested, one_under_nesting).outcome(),
        ResourceKind::Nesting,
        retained_nesting - 1,
    );

    // Both Projection directions preserve the complete scalar stream from
    // both recognized operands. Each pair is compiled from identical grammar
    // bytes and parses identical input; only the entry rule changes from
    // Projection to its same-source Sequence baseline. Remove exactly those
    // two differing outer rule-envelope rows, then compare every public event
    // field. The operands are intentionally heterogeneous: a transformed
    // regex and a record containing a literal.
    let projection_input = b"word!";
    let projected_right = scalar_rows_without_outer_envelope!(
        project_right,
        projection_input,
        PROJECT_RIGHT_RULE
    );
    let sequenced_right = scalar_rows_without_outer_envelope!(
        baseline_right,
        projection_input,
        BASELINE_RIGHT_RULE
    );
    assert_eq!(projected_right, sequenced_right);
    let projected_left = scalar_rows_without_outer_envelope!(
        project_left,
        projection_input,
        PROJECT_LEFT_RULE
    );
    let sequenced_left = scalar_rows_without_outer_envelope!(
        baseline_left,
        projection_input,
        BASELINE_LEFT_RULE
    );
    assert_eq!(projected_left, sequenced_left);
    for required_kind in [2_u8, 4, 5, 8] {
        assert!(
            projected_right
                .iter()
                .any(|event| event.0 == required_kind),
            "Projection witness omitted heterogeneous operand event kind {required_kind}"
        );
    }
    for (retained_len, no_sink_len) in [
        (
            project_right::parse_events(projection_input)
                .sink()
                .events()
                .len(),
            EventSink::len(project_right::check(projection_input).sink()),
        ),
        (
            project_left::parse_events(projection_input)
                .sink()
                .events()
                .len(),
            EventSink::len(project_left::check(projection_input).sink()),
        ),
    ] {
        assert_eq!(retained_len, no_sink_len);
    }

    let mut observed_event_kinds = BTreeSet::new();
    observe_operator_surface!(pratt, b"  1 + 2  ", observed_event_kinds);
    observe_operator_surface!(project_right, projection_input, observed_event_kinds);
    observe_operator_surface!(event_surface, b"xy", observed_event_kinds);
    assert_eq!(
        observed_event_kinds,
        BTreeSet::from([
            "ChoiceBranch",
            "EndInput",
            "EnterField",
            "EnterRule",
            "LeaveField",
            "LeaveRule",
            "Literal",
            "PrattReduce",
            "Regex",
            "RepeatCount",
            "Transform",
            "Trivia",
        ]),
        "every allowed generated event kind executes; operator is Some only for PrattReduce"
    );

    // Difference exclusion is probed first but never leaks its event. A
    // matching exclusion rejects atomically.
    let included = semantic::parse_events(b"k");
    assert_matched(included.outcome(), 1);
    let included_literals = included
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == semantic::EventKind::Literal)
        .map(|event| (event.start(), event.end()))
        .collect::<Vec<_>>();
    assert_eq!(included_literals, vec![(0, 1)]);

    // Isolate the nullable-exclusion rule as the generated entry. Epsilon
    // exclusion makes the Difference language empty, rejects at entry, leaks
    // no primary/exclusion events, and still charges monotone excluded work.
    let nullable_exclusion_work = minimum_work_both!(
        difference_nullable_exclusion,
        b"k",
        Step::Mismatch(_)
    );
    let nullable_limit = difference_nullable_exclusion::LimitsConfig::default()
        .with_work(nullable_exclusion_work);
    let nullable_retained =
        difference_nullable_exclusion::parse_events_with_limits(b"k", nullable_limit);
    let nullable_no_sink =
        difference_nullable_exclusion::check_with_limits(b"k", nullable_limit);
    assert_mismatch_evidence_at(nullable_retained.outcome(), 0);
    assert_mismatch_evidence_at(nullable_no_sink.outcome(), 0);
    assert!(nullable_retained.sink().events().is_empty());
    assert_eq!(EventSink::len(nullable_no_sink.sink()), 0);
    assert_exact_work_boundary!(
        difference_nullable_exclusion,
        b"k",
        Step::Mismatch(_),
        nullable_exclusion_work
    );

    // In the isolated Difference entry, exclusion consumes and emits "a"
    // before failing on "x". Its event is rolled back, then include commits
    // exactly the two primary literal events. Discover exact terminal work
    // independently in both sink modes rather than prescribing an emitter
    // implementation total.
    let difference_retained_work = minimum_work!(
        difference_only,
        parse_events,
        parse_events_with_limits,
        b"ab",
        Step::Matched(_)
    );
    let difference_no_sink_work = minimum_work!(
        difference_only,
        check,
        check_with_limits,
        b"ab",
        Step::Matched(_)
    );
    assert_eq!(difference_retained_work, difference_no_sink_work);
    let shallow_retained_work = minimum_work!(
        difference_shallow,
        parse_events,
        parse_events_with_limits,
        b"ab",
        Step::Matched(_)
    );
    let shallow_no_sink_work = minimum_work!(
        difference_shallow,
        check,
        check_with_limits,
        b"ab",
        Step::Matched(_)
    );
    assert_eq!(shallow_retained_work, shallow_no_sink_work);
    assert!(
        difference_retained_work > shallow_retained_work,
        "a partially consuming rolled-back exclusion must charge more work than an immediate miss"
    );
    let difference_exact = difference_only::LimitsConfig::default()
        .with_work(difference_retained_work);
    let difference_retained_exact =
        difference_only::parse_events_with_limits(b"ab", difference_exact);
    let difference_no_sink_exact =
        difference_only::check_with_limits(b"ab", difference_exact);
    assert_matched(difference_retained_exact.outcome(), 2);
    assert_matched(difference_no_sink_exact.outcome(), 2);
    let difference_literals = difference_retained_exact
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == difference_only::EventKind::Literal)
        .map(|event| (event.start(), event.end()))
        .collect::<Vec<_>>();
    assert_eq!(
        difference_literals,
        vec![(0, 1), (1, 2)],
        "the partially consuming exclusion leaked its speculative `a` event"
    );
    assert_eq!(
        difference_retained_exact.sink().events().len(),
        EventSink::len(difference_no_sink_exact.sink())
    );
    let difference_under = difference_only::LimitsConfig::default()
        .with_work(difference_retained_work - 1);
    let difference_retained_under =
        difference_only::parse_events_with_limits(b"ab", difference_under);
    let difference_no_sink_under =
        difference_only::check_with_limits(b"ab", difference_under);
    assert_resource(
        difference_retained_under.outcome(),
        ResourceKind::Work,
        difference_retained_work - 1,
    );
    assert_resource(
        difference_no_sink_under.outcome(),
        ResourceKind::Work,
        difference_retained_work - 1,
    );
    assert!(difference_retained_under.sink().events().is_empty());
    assert_eq!(EventSink::len(difference_no_sink_under.sink()), 0);

    // A matching exclusion returns entry-position diagnostic evidence. Its
    // minimum terminal threshold is independently discovered for each sink;
    // one under faults.
    let excluded_retained_work = minimum_work!(
        difference_only,
        parse_events,
        parse_events_with_limits,
        b"ax",
        Step::Mismatch(_)
    );
    let excluded_no_sink_work = minimum_work!(
        difference_only,
        check,
        check_with_limits,
        b"ax",
        Step::Mismatch(_)
    );
    assert_eq!(excluded_retained_work, excluded_no_sink_work);
    let exclusion_cap =
        difference_only::LimitsConfig::default().with_work(excluded_retained_work);
    let excluded =
        difference_only::parse_events_with_limits(b"ax", exclusion_cap);
    let excluded_no_sink = difference_only::check_with_limits(b"ax", exclusion_cap);
    assert_mismatch_evidence_at(excluded.outcome(), 0);
    assert_mismatch_evidence_at(excluded_no_sink.outcome(), 0);
    assert!(excluded.sink().events().is_empty());
    assert_eq!(EventSink::len(excluded_no_sink.sink()), 0);
    let exclusion_under = difference_only::LimitsConfig::default()
        .with_work(excluded_retained_work - 1);
    assert_resource(
        difference_only::parse_events_with_limits(b"ax", exclusion_under).outcome(),
        ResourceKind::Work,
        excluded_retained_work - 1,
    );
    assert_resource(
        difference_only::check_with_limits(b"ax", exclusion_under).outcome(),
        ResourceKind::Work,
        excluded_retained_work - 1,
    );

    // Directly discriminate the exclusion-first law with two generated
    // controls whose matching exclusion is byte-identical and precedes the
    // primary definition at the same rule/node ordinals. Both primaries match
    // the exact exclusion bytes `ax`; one is a direct sequence and the other
    // traverses a deliberately deep reference chain before reaching the same
    // sequence. If either primary is invoked after the exclusion matches, the
    // deep control's minimum terminal work must be greater.
    let shallow_primary_retained_work = minimum_work!(
        difference_primary_shallow,
        parse_events,
        parse_events_with_limits,
        b"ax",
        Step::Matched(_)
    );
    let shallow_primary_no_sink_work = minimum_work!(
        difference_primary_shallow,
        check,
        check_with_limits,
        b"ax",
        Step::Matched(_)
    );
    let deep_primary_retained_work = minimum_work!(
        difference_primary_deep,
        parse_events,
        parse_events_with_limits,
        b"ax",
        Step::Matched(_)
    );
    let deep_primary_no_sink_work = minimum_work!(
        difference_primary_deep,
        check,
        check_with_limits,
        b"ax",
        Step::Matched(_)
    );
    assert_eq!(
        shallow_primary_retained_work, shallow_primary_no_sink_work,
        "shallow-primary work has retaining/NoSink parity"
    );
    assert_eq!(
        deep_primary_retained_work, deep_primary_no_sink_work,
        "deep-primary work has retaining/NoSink parity"
    );
    assert!(
        deep_primary_retained_work > shallow_primary_retained_work,
        "the deep-primary control must dynamically discriminate primary invocation"
    );
    assert_exact_work_boundary!(
        difference_primary_shallow,
        b"ax",
        Step::Matched(_),
        shallow_primary_retained_work
    );
    assert_exact_work_boundary!(
        difference_primary_deep,
        b"ax",
        Step::Matched(_),
        deep_primary_retained_work
    );

    let control_shallow_retained_work = minimum_work!(
        difference_excluded_shallow,
        parse_events,
        parse_events_with_limits,
        b"ax",
        Step::Mismatch(_)
    );
    let control_shallow_no_sink_work = minimum_work!(
        difference_excluded_shallow,
        check,
        check_with_limits,
        b"ax",
        Step::Mismatch(_)
    );
    let control_deep_retained_work = minimum_work!(
        difference_excluded_deep,
        parse_events,
        parse_events_with_limits,
        b"ax",
        Step::Mismatch(_)
    );
    let control_deep_no_sink_work = minimum_work!(
        difference_excluded_deep,
        check,
        check_with_limits,
        b"ax",
        Step::Mismatch(_)
    );
    assert_eq!(
        [
            control_shallow_retained_work,
            control_shallow_no_sink_work,
            control_deep_retained_work,
            control_deep_no_sink_work,
        ],
        [control_shallow_retained_work; 4],
        "matched exclusion work must be independent of primary depth in both sink modes"
    );

    let control_work = control_shallow_retained_work;
    let shallow_limit =
        difference_excluded_shallow::LimitsConfig::default().with_work(control_work);
    let shallow_retained =
        difference_excluded_shallow::parse_events_with_limits(b"ax", shallow_limit);
    let shallow_no_sink =
        difference_excluded_shallow::check_with_limits(b"ax", shallow_limit);
    let deep_limit =
        difference_excluded_deep::LimitsConfig::default().with_work(control_work);
    let deep_retained =
        difference_excluded_deep::parse_events_with_limits(b"ax", deep_limit);
    let deep_no_sink = difference_excluded_deep::check_with_limits(b"ax", deep_limit);
    let evidence = [
        mismatch_evidence_offset(shallow_retained.outcome()),
        mismatch_evidence_offset(shallow_no_sink.outcome()),
        mismatch_evidence_offset(deep_retained.outcome()),
        mismatch_evidence_offset(deep_no_sink.outcome()),
    ];
    assert_eq!(evidence, [0; 4]);
    assert!(shallow_retained.sink().events().is_empty());
    assert_eq!(EventSink::len(shallow_no_sink.sink()), 0);
    assert!(deep_retained.sink().events().is_empty());
    assert_eq!(EventSink::len(deep_no_sink.sink()), 0);

    let control_under = control_work - 1;
    let shallow_under =
        difference_excluded_shallow::LimitsConfig::default().with_work(control_under);
    assert_resource(
        difference_excluded_shallow::parse_events_with_limits(b"ax", shallow_under).outcome(),
        ResourceKind::Work,
        control_under,
    );
    assert_resource(
        difference_excluded_shallow::check_with_limits(b"ax", shallow_under).outcome(),
        ResourceKind::Work,
        control_under,
    );
    let deep_under =
        difference_excluded_deep::LimitsConfig::default().with_work(control_under);
    assert_resource(
        difference_excluded_deep::parse_events_with_limits(b"ax", deep_under).outcome(),
        ResourceKind::Work,
        control_under,
    );
    assert_resource(
        difference_excluded_deep::check_with_limits(b"ax", deep_under).outcome(),
        ResourceKind::Work,
        control_under,
    );

    // Trim emits only nonempty trivia and preserves exact side/spans.
    let trimmed = semantic::parse_events(b"  k  ");
    assert_matched(trimmed.outcome(), 5);
    let trim_events = trimmed
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == semantic::EventKind::Trivia)
        .map(|event| (event.node(), event.start(), event.end(), event.value()))
        .collect::<Vec<_>>();
    assert_eq!(
        trim_events,
        vec![
            (Some(SEMANTIC_TRIM), 0, 2, 0),
            (Some(SEMANTIC_TRIM), 3, 5, 1),
        ]
    );
    let trimmed_no_sink = semantic::check(b"  k  ");
    assert_eq!(
        format!("{:?}", trimmed.outcome()),
        format!("{:?}", trimmed_no_sink.outcome())
    );
    assert_eq!(
        trimmed.sink().events().len(),
        EventSink::len(trimmed_no_sink.sink())
    );
    let trimmed_again = semantic::parse_events(b"  r  ");
    assert_matched(trimmed_again.outcome(), 5);
    let trimmed_again_trivia = trimmed_again
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == semantic::EventKind::Trivia)
        .map(|event| (event.node(), event.start(), event.end(), event.value()))
        .collect::<Vec<_>>();
    assert_eq!(
        trimmed_again_trivia,
        vec![
            (Some(SEMANTIC_TRIM_AGAIN), 0, 2, 0),
            (Some(SEMANTIC_TRIM_AGAIN), 3, 5, 1),
        ],
        "a second Trim path must use the same singleton trivia plan"
    );
    assert_eq!(
        trimmed_again.sink().events().len(),
        EventSink::len(semantic::check(b"  r  ").sink())
    );

    let nullable_trimmed = semantic::parse_events(b"  ");
    assert_matched(nullable_trimmed.outcome(), 2);
    let nullable_trimmed_trivia = nullable_trimmed
        .sink()
        .events()
        .iter()
        .filter(|event| event.kind() == semantic::EventKind::Trivia)
        .map(|event| (event.node(), event.start(), event.end(), event.value()))
        .collect::<Vec<_>>();
    assert_eq!(
        nullable_trimmed_trivia,
        vec![(Some(SEMANTIC_NULLABLE_TRIM), 0, 2, 0)],
        "nullable Trim must share the singleton trivia plan and omit zero-width trailing trivia"
    );
    assert_eq!(
        nullable_trimmed.sink().events().len(),
        EventSink::len(semantic::check(b"  ").sink())
    );

    let unspaced_trim = trim_only::parse_events(b"k");
    assert_matched(unspaced_trim.outcome(), 1);
    assert_eq!(
        unspaced_trim
            .sink()
            .events()
            .iter()
            .filter(|event| event.kind() == trim_only::EventKind::Trivia)
            .count(),
        0,
        "zero-width Trim trivia must not emit an event"
    );
    assert_eq!(
        unspaced_trim.sink().events().len(),
        EventSink::len(trim_only::check(b"k").sink())
    );

    let failed_after_leading = trim_only::parse_events(b"  x");
    assert_mismatch_evidence_at(failed_after_leading.outcome(), 2);
    assert!(
        failed_after_leading.sink().events().is_empty(),
        "child mismatch must roll back leading Trivia"
    );
    let failed_after_leading_no_sink = trim_only::check(b"  x");
    assert_mismatch_evidence_at(failed_after_leading_no_sink.outcome(), 2);
    assert_eq!(
        EventSink::len(failed_after_leading_no_sink.sink()),
        0,
        "retaining/NoSink whole-Trim rollback parity"
    );

    let trim_success_work = minimum_work_both!(trim_only, b"  k", Step::Matched(_));
    let fault_after_leading = (0..trim_success_work)
        .find(|limit| {
            let config = trim_only::LimitsConfig::default().with_work(*limit);
            resource_fault_offset(
                trim_only::parse_events_with_limits(b"  k", config).outcome(),
                ResourceKind::Work,
            ) == Some(2)
        })
        .expect("a child-path Work fault occurs after leading trivia");
    let fault_config = trim_only::LimitsConfig::default().with_work(fault_after_leading);
    let retained_fault = trim_only::parse_events_with_limits(b"  k", fault_config);
    let no_sink_fault = trim_only::check_with_limits(b"  k", fault_config);
    assert_eq!(
        resource_fault_offset(retained_fault.outcome(), ResourceKind::Work),
        Some(2)
    );
    assert_eq!(
        resource_fault_offset(no_sink_fault.outcome(), ResourceKind::Work),
        Some(2)
    );
    assert!(
        retained_fault.sink().events().is_empty(),
        "child-path fault after leading trivia rolls back every retained event"
    );
    assert_eq!(
        EventSink::len(no_sink_fault.sink()),
        0,
        "child-path fault after leading trivia has NoSink rollback parity"
    );
}
"#;

fn render_generated_runtime_main(
    pratt: &CompiledModule,
    semantic: &CompiledModule,
    multi_pratt: &CompiledModule,
    unused_pratt: &CompiledModule,
    project_right: &CompiledModule,
    baseline_right: &CompiledModule,
    project_left: &CompiledModule,
    baseline_left: &CompiledModule,
) -> String {
    let replacements = [
        (
            "__PRATT_NODE__",
            plan_rule_root(pratt, "expression").index(),
        ),
        (
            "__PRATT_DOCUMENT_TRIM__",
            plan_rule_root(pratt, "document").index(),
        ),
        (
            "__PRATT_NUMBER_TRIM__",
            plan_rule_root(pratt, "number").index(),
        ),
        ("__PRATT_SUM_TRIM__", plan_rule_root(pratt, "sum").index()),
        (
            "__SEMANTIC_TRIM__",
            plan_rule_root(semantic, "trimmed").index(),
        ),
        (
            "__SEMANTIC_TRIM_AGAIN__",
            plan_rule_root(semantic, "trimmed_again").index(),
        ),
        (
            "__SEMANTIC_NULLABLE_TRIM__",
            plan_rule_root(semantic, "nullable_trimmed").index(),
        ),
        ("__EXPRESSION_RULE__", rule(pratt, "expression").index()),
        ("__ATOM_RULE__", rule(pratt, "atom").index()),
        ("__PREFIX_RULE__", rule(pratt, "prefix_minus").index()),
        ("__POSTFIX_RULE__", rule(pratt, "postfix_bang").index()),
        ("__POWER_RULE__", rule(pratt, "power").index()),
        ("__PRODUCT_RULE__", rule(pratt, "product").index()),
        ("__SUM_RULE__", rule(pratt, "sum").index()),
        ("__PROBE_RULE__", rule(pratt, "probe_then_fail").index()),
        ("__LONG_RULE__", rule(pratt, "long_shift").index()),
        ("__SHORT_RULE__", rule(pratt, "short_shift").index()),
        (
            "__PROJECT_RIGHT_RULE__",
            rule(project_right, "project_right").index(),
        ),
        (
            "__BASELINE_RIGHT_RULE__",
            rule(baseline_right, "baseline_right").index(),
        ),
        (
            "__PROJECT_LEFT_RULE__",
            rule(project_left, "project_left").index(),
        ),
        (
            "__BASELINE_LEFT_RULE__",
            rule(baseline_left, "baseline_left").index(),
        ),
        (
            "__MULTI_SUM_SITE_RULE__",
            rule(multi_pratt, "sum_expression").index(),
        ),
        (
            "__MULTI_SUM_ATOM_RULE__",
            rule(multi_pratt, "sum_atom").index(),
        ),
        (
            "__MULTI_SUM_OPERATOR_RULE__",
            rule(multi_pratt, "sum_operator").index(),
        ),
        (
            "__MULTI_PRODUCT_SITE_RULE__",
            rule(multi_pratt, "product_expression").index(),
        ),
        (
            "__MULTI_PRODUCT_ATOM_RULE__",
            rule(multi_pratt, "product_atom").index(),
        ),
        (
            "__MULTI_PRODUCT_OPERATOR_RULE__",
            rule(multi_pratt, "product_operator").index(),
        ),
        (
            "__UNUSED_PRATT_SITE_NODE__",
            plan_rule_root(unused_pratt, "unused_expression").index(),
        ),
    ];
    let mut rendered = GENERATED_RUNTIME_MAIN.to_owned();
    for (marker, ordinal) in replacements {
        rendered = rendered.replace(marker, &ordinal.to_string());
    }
    assert!(
        !rendered.contains("__"),
        "every host-derived generated-runtime ordinal was injected"
    );
    rendered
}

fn write_runtime_project(root: &Path, generated_sources: &[(&str, &str)], runtime_main: &str) {
    fs::create_dir(root.join("src")).expect("create generated runtime source directory");
    let parse_that = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../parse-that-skv26/rust/parse_that")
        .canonicalize()
        .expect("resolve exact parse-that 0.5.0 sibling");
    let cargo_toml = format!(
        "[package]\nname = \"w3-semantic-pratt-generated-runtime\"\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[dependencies]\nparse_that = {{ version = \"=0.5.0\", path = {:?} }}\n",
        parse_that
    );
    fs::write(root.join("Cargo.toml"), cargo_toml).expect("write runtime Cargo.toml");
    for (name, source) in generated_sources {
        fs::write(root.join(name), source)
            .unwrap_or_else(|error| panic!("write generated runtime module {name}: {error}"));
    }
    fs::write(root.join("src/main.rs"), runtime_main).expect("write runtime laws");
}

fn excluded_difference_control_source() -> String {
    let mut source = String::from(
        "difference_shallow = shallow_primary - excluded;\n\
         difference_deep = deep_primary - excluded;\n\
         excluded = \"a\", \"x\";\n\
         shallow_primary = \"a\", \"x\";\n\
         deep_primary = deep_0;\n",
    );
    for depth in 0..31 {
        writeln!(source, "deep_{depth} = deep_{};", depth + 1)
            .expect("writing to String is infallible");
    }
    source.push_str("deep_31 = \"a\", \"x\";\n");
    source
}

#[test]
fn emitted_runtime_executes_all_w3_semantic_and_pratt_transaction_laws() {
    syn::parse_file(GENERATED_RUNTIME_MAIN)
        .expect("embedded generated-runtime contract is syntactically valid Rust");
    let pratt = accepted(PRATT_MANIFEST, PRATT_SOURCE);
    let pratt_without_probe_manifest = pratt_manifest_without_failed_probe();
    let pratt_without_probe = accepted(&pratt_without_probe_manifest, PRATT_SOURCE);
    let mut pratt_disabled_manifest: serde_json::Value =
        serde_json::from_slice(PRATT_MANIFEST).expect("Pratt fixture manifest is JSON");
    pratt_disabled_manifest["logical_path"] = "contracts/w3/pratt-disabled-runtime.bbnf".into();
    pratt_disabled_manifest["pratt_tables"] = serde_json::json!([]);
    let pratt_disabled_manifest =
        serde_json::to_vec(&pratt_disabled_manifest).expect("disabled Pratt manifest is JSON");
    let pratt_disabled = accepted(&pratt_disabled_manifest, PRATT_SOURCE);
    let pratt_direct_manifest =
        pratt_manifest_for_entry("contracts/w3/pratt-direct-runtime.bbnf", "expression");
    let pratt_direct = accepted(&pratt_direct_manifest, PRATT_SOURCE);
    let prefix_one_baseline_manifest = manifest_for_entry(
        "contracts/w3/prefix-one-baseline-runtime.bbnf",
        "prefix_one_baseline",
        Some("[ \\t]*"),
    );
    let prefix_two_baseline_manifest = manifest_for_entry(
        "contracts/w3/prefix-two-baseline-runtime.bbnf",
        "prefix_two_baseline",
        Some("[ \\t]*"),
    );
    let infix_one_baseline_manifest = manifest_for_entry(
        "contracts/w3/infix-one-baseline-runtime.bbnf",
        "infix_one_baseline",
        Some("[ \\t]*"),
    );
    let infix_two_baseline_manifest = manifest_for_entry(
        "contracts/w3/infix-two-baseline-runtime.bbnf",
        "infix_two_baseline",
        Some("[ \\t]*"),
    );
    let prefix_one_baseline = accepted(&prefix_one_baseline_manifest, PRATT_SOURCE);
    let prefix_two_baseline = accepted(&prefix_two_baseline_manifest, PRATT_SOURCE);
    let infix_one_baseline = accepted(&infix_one_baseline_manifest, PRATT_SOURCE);
    let infix_two_baseline = accepted(&infix_two_baseline_manifest, PRATT_SOURCE);
    let overlap_swapped_manifest = mutated_pratt_manifest(
        "contracts/w3/pratt-overlap-swapped-runtime.bbnf",
        |operators| operators.swap(6, 7),
    );
    let pratt_overlap_swapped = accepted(&overlap_swapped_manifest, PRATT_SOURCE);
    let prefix_cross_manifest = mutated_pratt_manifest(
        "contracts/w3/pratt-prefix-cross-runtime.bbnf",
        |operators| operators[0]["right_binding_power"] = 50.into(),
    );
    let pratt_prefix_cross = accepted(&prefix_cross_manifest, PRATT_SOURCE);
    let postfix_cross_manifest = mutated_pratt_manifest(
        "contracts/w3/pratt-postfix-cross-runtime.bbnf",
        |operators| operators[1]["left_binding_power"] = 79.into(),
    );
    let pratt_postfix_cross = accepted(&postfix_cross_manifest, PRATT_SOURCE);
    let infix_cross_manifest =
        mutated_pratt_manifest("contracts/w3/pratt-infix-cross-runtime.bbnf", |operators| {
            operators[2]["right_binding_power"] = 81.into()
        });
    let pratt_infix_cross = accepted(&infix_cross_manifest, PRATT_SOURCE);
    let affix_overlap = accepted(AFFIX_OVERLAP_MANIFEST, AFFIX_OVERLAP_SOURCE);
    let affix_prefix_without_probe_manifest = mutated_affix_manifest(
        "contracts/w3/affix-prefix-without-probe-runtime.bbnf",
        |operators| {
            operators.remove(0);
        },
    );
    let affix_prefix_without_probe =
        accepted(&affix_prefix_without_probe_manifest, AFFIX_OVERLAP_SOURCE);
    let affix_postfix_without_probe_manifest = mutated_affix_manifest(
        "contracts/w3/affix-postfix-without-probe-runtime.bbnf",
        |operators| {
            operators.remove(2);
        },
    );
    let affix_postfix_without_probe =
        accepted(&affix_postfix_without_probe_manifest, AFFIX_OVERLAP_SOURCE);
    let affix_prefix_swapped_manifest = mutated_affix_manifest(
        "contracts/w3/affix-prefix-swapped-runtime.bbnf",
        |operators| operators.swap(0, 1),
    );
    let affix_prefix_swapped = accepted(&affix_prefix_swapped_manifest, AFFIX_OVERLAP_SOURCE);
    let affix_postfix_swapped_manifest = mutated_affix_manifest(
        "contracts/w3/affix-postfix-swapped-runtime.bbnf",
        |operators| operators.swap(2, 3),
    );
    let affix_postfix_swapped = accepted(&affix_postfix_swapped_manifest, AFFIX_OVERLAP_SOURCE);
    for module in [
        &affix_overlap,
        &affix_prefix_without_probe,
        &affix_postfix_without_probe,
        &affix_prefix_swapped,
        &affix_postfix_swapped,
    ] {
        assert_generated_v6_canonical(module, false);
    }
    let multi_pratt = accepted(MULTI_PRATT_MANIFEST, MULTI_PRATT_SOURCE);
    let unused_pratt = accepted(UNUSED_PRATT_MANIFEST, UNUSED_PRATT_SOURCE);
    assert_generated_v6_canonical(&unused_pratt, false);
    const EVENT_SURFACE_SOURCE: &[u8] = br#"
document = item*;
item = "x" | /y/;
"#;
    let event_surface_manifest =
        manifest_for_entry("contracts/w3/event-surface-runtime.bbnf", "document", None);
    let event_surface = accepted(&event_surface_manifest, EVENT_SURFACE_SOURCE);
    let semantic = accepted(SEMANTIC_MANIFEST, SEMANTIC_SOURCE);
    let difference_nullable_exclusion_manifest = manifest_for_entry(
        "contracts/w3/difference-nullable-exclusion-runtime.bbnf",
        "difference_empty",
        Some("[ \\t]*"),
    );
    let difference_nullable_exclusion =
        accepted(&difference_nullable_exclusion_manifest, SEMANTIC_SOURCE);
    let trim_only_manifest = manifest_for_entry(
        "contracts/w3/trim-only-runtime.bbnf",
        "trimmed",
        Some("[ \\t]*"),
    );
    let trim_only = accepted(&trim_only_manifest, SEMANTIC_SOURCE);
    const DIFFERENCE_SOURCE: &[u8] = br#"
difference_live = include - exclude;
include = "a", "b";
exclude = "a", "x";
"#;
    const DIFFERENCE_SHALLOW_SOURCE: &[u8] = br#"
difference_live = include - exclude;
include = "a", "b";
exclude = "x";
"#;
    const DIFFERENCE_MANIFEST: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "contracts/w3/difference-runtime.bbnf",
  "entry": "difference_live",
  "trivia_regex": null,
  "pratt_tables": []
}"#;
    let difference = accepted(DIFFERENCE_MANIFEST, DIFFERENCE_SOURCE);
    let difference_shallow_manifest = manifest_for_entry(
        "contracts/w3/difference-shallow-runtime.bbnf",
        "difference_live",
        None,
    );
    let difference_shallow = accepted(&difference_shallow_manifest, DIFFERENCE_SHALLOW_SOURCE);
    let difference_control_source = excluded_difference_control_source();
    let difference_excluded_shallow_manifest = manifest_for_entry(
        "contracts/w3/difference-excluded-shallow-runtime.bbnf",
        "difference_shallow",
        None,
    );
    let difference_excluded_deep_manifest = manifest_for_entry(
        "contracts/w3/difference-excluded-deep-runtime.bbnf",
        "difference_deep",
        None,
    );
    let difference_primary_shallow_manifest = manifest_for_entry(
        "contracts/w3/difference-primary-shallow-runtime.bbnf",
        "shallow_primary",
        None,
    );
    let difference_primary_deep_manifest = manifest_for_entry(
        "contracts/w3/difference-primary-deep-runtime.bbnf",
        "deep_primary",
        None,
    );
    let difference_excluded_shallow = accepted(
        &difference_excluded_shallow_manifest,
        difference_control_source.as_bytes(),
    );
    let difference_excluded_deep = accepted(
        &difference_excluded_deep_manifest,
        difference_control_source.as_bytes(),
    );
    let difference_primary_shallow = accepted(
        &difference_primary_shallow_manifest,
        difference_control_source.as_bytes(),
    );
    let difference_primary_deep = accepted(
        &difference_primary_deep_manifest,
        difference_control_source.as_bytes(),
    );
    for generated_difference in [&difference, &difference_shallow] {
        assert_eq!(
            generated_difference
                .generated()
                .source()
                .matches("difference(run,")
                .count(),
            1,
            "each emitted Difference node must call the one canonical parse_that::difference"
        );
    }
    assert_eq!(
        difference_nullable_exclusion
            .generated()
            .source()
            .matches("difference(run,")
            .count(),
        3,
        "the isolated nullable-exclusion product emits each source Difference through the canonical primitive"
    );
    for generated_control in [
        &difference_excluded_shallow,
        &difference_excluded_deep,
        &difference_primary_shallow,
        &difference_primary_deep,
    ] {
        assert_eq!(
            generated_control
                .generated()
                .source()
                .matches("difference(run,")
                .count(),
            2,
            "each of two source Difference nodes calls the one canonical primitive once"
        );
    }

    let projection_entries = [
        ("project_right", "contracts/w3/project-right-runtime.bbnf"),
        ("baseline_right", "contracts/w3/baseline-right-runtime.bbnf"),
        ("project_left", "contracts/w3/project-left-runtime.bbnf"),
        ("baseline_left", "contracts/w3/baseline-left-runtime.bbnf"),
    ];
    let projection_modules = projection_entries.map(|(entry, path)| {
        let manifest = manifest_for_entry(path, entry, None);
        accepted(&manifest, PROJECTION_STREAM_SOURCE)
    });
    let [project_right, baseline_right, project_left, baseline_left] = &projection_modules;

    let temp = tempfile::tempdir().expect("create generated runtime project");
    let runtime_main = render_generated_runtime_main(
        &pratt,
        &semantic,
        &multi_pratt,
        &unused_pratt,
        project_right,
        baseline_right,
        project_left,
        baseline_left,
    );
    write_runtime_project(
        temp.path(),
        &[
            ("pratt_generated.rs", pratt.generated().source()),
            (
                "pratt_without_probe_generated.rs",
                pratt_without_probe.generated().source(),
            ),
            (
                "pratt_disabled_generated.rs",
                pratt_disabled.generated().source(),
            ),
            (
                "pratt_direct_generated.rs",
                pratt_direct.generated().source(),
            ),
            (
                "prefix_one_baseline_generated.rs",
                prefix_one_baseline.generated().source(),
            ),
            (
                "prefix_two_baseline_generated.rs",
                prefix_two_baseline.generated().source(),
            ),
            (
                "infix_one_baseline_generated.rs",
                infix_one_baseline.generated().source(),
            ),
            (
                "infix_two_baseline_generated.rs",
                infix_two_baseline.generated().source(),
            ),
            (
                "pratt_overlap_swapped_generated.rs",
                pratt_overlap_swapped.generated().source(),
            ),
            (
                "pratt_prefix_cross_generated.rs",
                pratt_prefix_cross.generated().source(),
            ),
            (
                "pratt_postfix_cross_generated.rs",
                pratt_postfix_cross.generated().source(),
            ),
            (
                "pratt_infix_cross_generated.rs",
                pratt_infix_cross.generated().source(),
            ),
            (
                "affix_overlap_generated.rs",
                affix_overlap.generated().source(),
            ),
            (
                "affix_prefix_without_probe_generated.rs",
                affix_prefix_without_probe.generated().source(),
            ),
            (
                "affix_postfix_without_probe_generated.rs",
                affix_postfix_without_probe.generated().source(),
            ),
            (
                "affix_prefix_swapped_generated.rs",
                affix_prefix_swapped.generated().source(),
            ),
            (
                "affix_postfix_swapped_generated.rs",
                affix_postfix_swapped.generated().source(),
            ),
            ("multi_pratt_generated.rs", multi_pratt.generated().source()),
            (
                "unused_pratt_generated.rs",
                unused_pratt.generated().source(),
            ),
            (
                "event_surface_generated.rs",
                event_surface.generated().source(),
            ),
            ("semantic_generated.rs", semantic.generated().source()),
            ("trim_only_generated.rs", trim_only.generated().source()),
            ("difference_generated.rs", difference.generated().source()),
            (
                "difference_nullable_exclusion_generated.rs",
                difference_nullable_exclusion.generated().source(),
            ),
            (
                "difference_shallow_generated.rs",
                difference_shallow.generated().source(),
            ),
            (
                "difference_excluded_shallow_generated.rs",
                difference_excluded_shallow.generated().source(),
            ),
            (
                "difference_excluded_deep_generated.rs",
                difference_excluded_deep.generated().source(),
            ),
            (
                "difference_primary_shallow_generated.rs",
                difference_primary_shallow.generated().source(),
            ),
            (
                "difference_primary_deep_generated.rs",
                difference_primary_deep.generated().source(),
            ),
            (
                "project_right_generated.rs",
                project_right.generated().source(),
            ),
            (
                "baseline_right_generated.rs",
                baseline_right.generated().source(),
            ),
            (
                "project_left_generated.rs",
                project_left.generated().source(),
            ),
            (
                "baseline_left_generated.rs",
                baseline_left.generated().source(),
            ),
        ],
        &runtime_main,
    );
    let output = Command::new(option_env!("CARGO").unwrap_or("cargo"))
        .args(["run", "--offline", "--quiet", "--manifest-path"])
        .arg(temp.path().join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", temp.path().join("target"))
        .output()
        .expect("run generated W3 parser contract");
    assert!(
        output.status.success(),
        "generated W3 runtime contract failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
