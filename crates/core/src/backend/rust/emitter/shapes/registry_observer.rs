//! AZ-I.W1.B4 — registry-read observer for the compound-emission
//! boundary.
//!
//! `emit_shapes_for_grammar` consults [`bbnf_ir::StructRegistry`] on
//! every compound-emission boundary (one per shape-classified rule) and
//! threads the resulting [`Option<&StructLayout>`] through to per-shape
//! emitters that will, in W2 / W3, switch from tape pushes to direct
//! struct-builder writes. In W1 the layout flows through the emission
//! state without changing emitted bytes; the registry-read fires
//! unconditionally to honour the activation gate (substrate-with-
//! consumer landed in one wave per `feedback_substrate-with-consumer`).
//!
//! The observer captures the per-rule registry read into a thread-local
//! buffer so the wire-contract test at
//! `crates/core/tests/emitter_registry_read.rs` can confirm the
//! consumer fires end-to-end. Drained by [`drain_registry_read_log`]
//! between codegen invocations; not load-bearing on the emitted token
//! stream — the buffer is purely diagnostic surface.
//!
//! Design notes:
//!
//! - The buffer is thread-local so concurrent codegen calls in
//!   different threads do not interleave their reads. The current
//!   pipeline runs single-threaded codegen per grammar; the
//!   thread-local protects against future parallelism without a
//!   lock-contention surface.
//! - Each event records the rule id and a boolean that flips when the
//!   registry has a populated layout for the rule. The test's gate is
//!   `events.iter().any(|e| e.had_layout)`, mirroring the
//!   `audit_payload_coverage` `Mapped`-marker rule.
//! - The buffer is never read by emitter code; only the test consumes
//!   it. Removing the observer at AZ-I close (when the bridge mode
//!   collapses into the W2 / W3 struct-builder paths) requires
//!   deleting this module and the call site in [`super::record_for_rule`].

use std::cell::RefCell;

use bbnf_ir::types::RuleId;

/// One observed registry-read event.
///
/// Captured once per shape-classified rule inside
/// [`super::emit_shapes_for_grammar`]. The pair is intentionally minimal
/// — a record-and-forget surface; layout content is owned by the
/// registry, never copied here.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegistryReadEvent {
    /// The originating rule's id.
    pub rule_id: RuleId,
    /// `true` iff [`bbnf_ir::StructRegistry::layout`] returned `Some`
    /// for the rule at the moment of the read. `false` records that
    /// the substrate exists (the pass ran) but has no layout for this
    /// rule yet — the W1 transition state for grammars whose typed-
    /// leaf authoring has not closed.
    pub had_layout: bool,
}

thread_local! {
    static LOG: RefCell<Vec<RegistryReadEvent>> = const { RefCell::new(Vec::new()) };
}

/// Record a registry-read event. Called from
/// [`super::emit_shapes_for_grammar`] on every compound-emission
/// boundary.
pub fn record(rule_id: RuleId, had_layout: bool) {
    LOG.with(|log| log.borrow_mut().push(RegistryReadEvent { rule_id, had_layout }));
}

/// Drain and return the recorded events for the current thread.
///
/// The wire-contract test calls this after the codegen invocation it is
/// observing and asserts the resulting vec contains at least one event
/// with `had_layout = true` for the grammar under test. Subsequent
/// codegen invocations in the same thread start with an empty buffer.
pub fn drain() -> Vec<RegistryReadEvent> {
    LOG.with(|log| log.borrow_mut().drain(..).collect())
}

/// Clear the buffer without returning it. Test setup calls this to
/// discard any reads recorded by upstream pipeline stages before
/// observing the codegen invocation under test.
pub fn clear() {
    LOG.with(|log| log.borrow_mut().clear());
}
