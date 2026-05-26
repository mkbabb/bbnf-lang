# Pass Omega V4 CH1 Correctness

Verdict: ACCEPT.

REDRESS-184, W4R packet, and source citations resolve:

- W4 deletion targets exist in `SPEC.md:572`-`:574`.
- W4 orders deletion before `regen-css` at `SPEC.md:593`-`:596`.
- The current emitter calls `codegen::emit_runtime_profile` at
  `skinny/xtask/src/regen.rs:18`.
- `codegen` imports and dispatches through the seven CSS providers at
  `skinny/crates/codegen/src/lib.rs:1`-`:7` and `:166`-`:208`.
- W5 owns the provider replacement at `SPEC.md:633`-`:658` and depends on W4.

The proposed W4 ledger / W5 deletion split is therefore a correct
wave-graph repair.
