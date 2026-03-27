//! Monolithic codegen for TokenDispatch — lexer-parser fusion.
//!
//! Currently delegates to the fallback (original Alt). The TokenDispatch
//! IR node and pass infrastructure is in place for future optimization
//! when the codegen can emit zero-overhead branch selection.

use bbnf_ir::{IrNode, TokenDispatchArm};

use proc_macro2::TokenStream;

use super::super::super::ir_types::IrCodegenCtx;
use super::{emit_mono_expr, MonoCtx};

/// Emit monolithic code for a TokenDispatch node.
///
/// Delegates to the fallback expression (the original Alt with all branches).
/// The IR pass may have restructured the branches for better dispatch coverage.
pub(super) fn emit_token_dispatch(
    _token: &IrNode,
    _arms: &[TokenDispatchArm],
    fallback: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    emit_mono_expr(fallback, ctx, mctx, elide_box)
}
