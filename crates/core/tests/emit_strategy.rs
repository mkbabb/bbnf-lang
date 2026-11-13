//! The codegen-time substrate decision over a caller-supplied binding
//! (sk-v27 LW3.COMP-5a, RULINGS 2026-11-13c COMP5A-S; it replaces the
//! AZ-I.W2-act.B1 tests of the ident-keyed `EmitStrategy::for_grammar`
//! table, deleted with that table).
//!
//! 1. A supplied binding over a populated registry routes to
//!    `EmitStrategy::StructDirect` carrying exactly that binding,
//!    whatever the grammar is called.
//! 2. An empty registry panics rather than downgrading to tape.
//! 3. `ParserAttributes` without a binding cannot be emitted.

use bbnf::ParserAttributes;
use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{EmitStrategy, SubstrateBinding};
use bbnf_ir::registry::{LayoutKind, StructLayout, StructRegistry};

/// A `StructRegistry` with one synthetic layout: only `is_empty()`
/// reaches the decision.
fn populated_registry() -> StructRegistry {
    let mut r = StructRegistry::new();
    r.insert(StructLayout {
        rule_id: 0,
        rule_name: "fixture".to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(Vec::new()),
        fields: Vec::new(),
    });
    r
}

const BINDING: SubstrateBinding<'static> = SubstrateBinding {
    builder_path: "crate::runtime::json::JsonStructBuilder",
    document_path: "crate::runtime::json::JsonDocument",
};

#[test]
fn supplied_binding_with_populated_registry_routes_struct_direct() {
    let strategy = EmitStrategy::struct_direct(BINDING, &populated_registry());
    assert_eq!(
        strategy,
        EmitStrategy::StructDirect {
            rust: BINDING,
            ts: None,
            wasm: None,
        }
    );
}

#[test]
#[should_panic(expected = "empty StructRegistry")]
fn supplied_binding_with_empty_registry_panics() {
    let _ = EmitStrategy::struct_direct(BINDING, &StructRegistry::new());
}

#[test]
fn parser_attributes_carry_the_binding_they_are_given() {
    let ir = bbnf_ir::GrammarIR {
        struct_registry: populated_registry(),
        ..bbnf_ir::GrammarIR::default()
    };
    let attrs = ParserAttributes {
        builder_path: Some(BINDING.builder_path.to_string()),
        document_path: Some(BINDING.document_path.to_string()),
        ..ParserAttributes::default()
    };
    let EmitStrategy::StructDirect { rust, .. } = attrs.emit_strategy(&ir);
    assert_eq!(rust, BINDING);
}

#[test]
#[should_panic(expected = "the caller supplies the grammar's `builder_path`")]
fn parser_attributes_without_a_binding_panic() {
    let ir = bbnf_ir::GrammarIR {
        struct_registry: populated_registry(),
        ..bbnf_ir::GrammarIR::default()
    };
    let _ = ParserAttributes::default().emit_strategy(&ir);
}
