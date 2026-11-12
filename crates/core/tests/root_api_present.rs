//! sk-v27 LW3.COMP-1b: the anti-drop test for the spine merge.
//!
//! The spine (`codex/sk-v26-bbnf` `b5fd0ca98`) writes `crates/core/src/lib.rs`
//! as `pub mod compiler;` alone. A merge that takes that side drops the root
//! pipeline from the build and removes the published `bbnf` 0.3.0 API that
//! bbnf-path, gorgeous, the LSP, the bootstrap and wasm read. This test names
//! one public item from each root module the published crate exports, and
//! the spine's compiler entry point, so such a merge fails to compile here.
//! Root stays the second oracle until LW4.COMP-1 (quarry law X-1).

use std::any::type_name;

#[test]
fn every_root_module_and_the_compiler_are_exported() {
    let names = [
        // types
        type_name::<bbnf::types::RuleEntry<'static>>(),
        // grammar
        type_name_of(&bbnf::grammar::parse),
        // generate
        type_name_of(&bbnf::generate::generate_all),
        // backend
        type_name::<bbnf::backend::driver::DriverState>(),
        // graph
        type_name_of(&bbnf::graph::find_aliases),
        // imports
        type_name::<bbnf::imports::ModuleRegistry>(),
        // lower
        type_name_of(&bbnf::lower::lower_to_ir),
        // path
        type_name::<bbnf::path::error::PathError>(),
        // pipeline
        type_name::<bbnf::pipeline::PipelineOptions>(),
        // runtime
        type_name::<bbnf::runtime::error::ParseErr>(),
        // compiler (the spine), as a module path and at the crate root
        type_name_of(&bbnf::compiler::compile_instance),
        type_name_of(&bbnf::compile_instance),
    ];
    assert_eq!(names.len(), 12);
    assert!(names.iter().all(|n| n.starts_with("bbnf::")), "{names:#?}");
}

fn type_name_of<T>(_: &T) -> &'static str {
    type_name::<T>()
}
