# B4 — Agent Dispatch Surface

Minimal sub-agent surface listing W0 dispatch concerns. Each wave's
sub-agent receives a self-contained prompt built from
`docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` + the wave-
specific scope substitutions.

## W0 — Codegen-emission `syn::parse2` fix

- **Worktree**: `/Users/mkbabb/Programming/bbnf-wt-b4-w0-syn` (pre-
  created at master HEAD).
- **Required reads** (in order): predecessor tranche's
  `audit/W1-bbnf-regen-defer.txt` (panic excerpt), `B4.md` (this
  tranche's plan), `waves/W0.md` (this wave's spec),
  `crates/core/src/generate/mod.rs` (`generate_all` composer),
  `xtask/src/regen.rs` (the `syn::parse2` callsite),
  `crates/core/src/backend/rust/emitter/shapes/**` (the candidate
  emitter family).
- **Allow-list**: emitter scope per `waves/W0.md` §File bounds, plus
  `xtask/src/regen.rs` for the worktree-only dump-and-panic
  diagnostic.
- **Forbidden**: `try-or-fallback` at the `syn::parse2` callsite,
  any γ-η revert, `git reset --hard`, modifying
  `crates/core/src/grammar/generated.rs` outside xtask output.
- **Hard gate**: per `waves/W0.md` §Hard gate.
- **Word cap**: 800.
- **Commit discipline**: cadence at every natural milestone in the
  worktree; orchestrator cherry-picks fix + docs at W0 close.

### Anti-patterns

1. **No work-around at the consumer.** The `syn::parse2` rejection is
   the symptom. The fix lives at the emitter producing the malformed
   fragment, not at the consumer that rejects it. A `match
   syn::parse2(...) { Ok(file) => ..., Err(_) => bail!("skipped") }`
   form is forbidden.
2. **No silent emission filter.** No "if the emitted shape is X, skip
   it" path at the emitter. Every emitter shape that the grammar
   exercises must produce valid Rust.
3. **No γ-η revert.** The predecessor tranche's W0 architectural
   fixes (γ retire `derive_frame_depth`; δ atomic depth rollback in
   `Columns`; ε cycle-safe cursor walk; ζ widened
   `end_compound_post_order` bump scope; η Pratt operand seeding +
   lowering cousin-leak guard) are load-bearing. The codegen defect
   lives downstream of the parser pipeline these fixes restored.
4. **No destructive operations on master.** Cherry-pick conflicts
   resolve via `git cherry-pick --abort` + manual investigation, or
   sub-agent redispatch on a fresh worktree.
5. **No pre-authored W1.** W0 closes first. W1 dispatches as a
   separate later round.

## W1 — Consumer-fixture polish (planned)

W1 dispatches after W0 closes. Dispatch surface authored at W1
dispatch time per the standard
`docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` form. The
fixture set is enumerated at `waves/W1.md` §File bounds; the agent
re-greps at dispatch time to confirm no drift.
