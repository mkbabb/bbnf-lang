//! Audit-coverage artefact emission and path-check pass wiring.
//!
//! Per `audit/AUDIT-2-SUBSTRATE-CONSUMER.md` §6.B (path a) the W0 audit
//! pass becomes load-bearing: every pipeline-compile run emits a
//! per-grammar coverage JSON to `target/audit/<entry>.json`. The
//! AZ-I.W4 close ceremony consumes the artefact as the substrate-
//! coverage gate input.

use bbnf_ir::GrammarIR;

/// AZ-I.W2-act.A — `audit_payload_coverage` artefact emission.
///
/// Failures to write are non-fatal — the pipeline continues so a
/// permission-bound CI environment without writable target/ does
/// not break compile. The artefact's absence simply blocks the
/// downstream coverage gate; it does not block codegen.
pub(super) fn write_audit_coverage_artefact(ir: &GrammarIR) {
    use bbnf_ir::passes::{GrammarAuditTag, audit_payload_coverage, write_coverage_report};
    // Empty grammar — bootstrap path or fixture; no artefact to write.
    if ir.rules.is_empty() {
        return;
    }
    // AZ-IV.W1.5 (Fermat F6) — per `audit/HARDENING-2026-05-01-fermat.md`
    // the prior rule-name → `GrammarAuditTag` aliasing arm-list (`"value"
    // | "json"` → `Json`, `"stylesheet" | "css_l4" | "cssL4"` → `CssL4`,
    // ...) was hard-coded grammar-entry-name aliasing — exactly the
    // overfit class AZ-IV §Invariants 2 names as a deletion target.
    // The audit-coverage artefact is a debug-only diagnostic
    // (`#[cfg(debug_assertions)]` below), so keying directly on the
    // entry-rule string preserves the artefact contract while removing
    // every literal grammar-name branch from production runtime.
    //
    // `Box::leak` produces the required `&'static str`; each pipeline
    // compile leaks one ident-sized string — a bounded, one-shot
    // allocation matching cargo's per-build process lifetime.
    let entry_name: &'static str = Box::leak(
        ir.get_string(ir.rules[ir.entry as usize].name)
            .to_string()
            .into_boxed_str(),
    );
    let tag = GrammarAuditTag::Custom(entry_name);
    let coverage = audit_payload_coverage(ir, tag.clone(), &&ir.struct_registry);

    // AZ-I.W2-act.close A.fix — wire-or-delete decay item. The
    // audit pass becomes load-bearing as a development invariant:
    // every `Missing` marker is the red signal that a registered
    // `StructLayout` exists but does not cover the typed `->` site
    // its enclosing rule projects. Gated behind `cfg(debug_assertions)`
    // so release builds skip the assertion (the artefact write
    // continues so downstream tooling can still consume the JSON).
    #[cfg(debug_assertions)]
    {
        if !coverage.is_clean() {
            panic!(
                "audit_payload_coverage: grammar {:?} reports {} `Missing` typed-`->` marker(s):\n  {}",
                tag.key(),
                coverage.missing_markers,
                coverage
                    .missing
                    .iter()
                    .map(|m| format!(
                        "rule={} fn_id={} typed_leaf={:?} fn_kind={}",
                        m.rule_name, m.fn_id, m.typed_leaf, m.fn_kind,
                    ))
                    .collect::<Vec<_>>()
                    .join("\n  "),
            );
        }
    }

    let mut report = bbnf_ir::passes::AuditCoverageReport::new();
    report.push(coverage);

    // `target/audit/<entry>.json` — the workspace target dir is the
    // canonical resolved location since `cargo` always invokes from
    // the workspace root or a sub-crate. The default path resolves
    // relative to the working directory; failures are silent so
    // permission-bound CI does not break compile.
    let path = std::path::PathBuf::from("target/audit").join(format!("{}.json", tag.key()));
    let _ = write_coverage_report(&report, &path);
}

/// Run the AZ-IV.W2.2 `path_check` IR pass against the post-`project_types`
/// IR. The pass folds the recorded inline trace + the populated
/// `StructRegistry` into a `PathCheckResolver` that binds every
/// user-written source rule name to a post-pipeline `RuleId`. The
/// borrow dance avoids holding a shared borrow of `ir` and a mutable
/// borrow of `ir.path_check_resolver` at once: take the trace by value,
/// run the pass, write the resolver back, return the trace.
pub(super) fn run_path_check_pass(ir: &mut bbnf_ir::GrammarIR) {
    let trace = std::mem::take(&mut ir.inline_trace);
    ir.path_check_resolver = bbnf_ir::passes::run_path_check(ir, &trace);
    ir.inline_trace = trace;
}
