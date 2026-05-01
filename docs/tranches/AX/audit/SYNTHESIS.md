# AX W0a Retrospective — Synthesis

Four parallel research agents audited the last ten sessions — AV close
through AX.W0a.2.h — for waste patterns, edict friction, infra gaps,
and plan redress. This document consolidates their findings. Read
this before dispatching any AX.W0a.2.i or successor wave.

Source reports:
- [R1 — Waste-pattern archaeology](R1-waste-patterns.md)
- [R2 — Instructions & edicts redress](R2-instructions-redress.md)
- [R3 — Infrastructure speedups](R3-infra-speedups.md)
- [R4 — AX plan redress](R4-plan-redress.md)

## Convergent diagnosis

All four agents, researching independently on disjoint scopes,
converged on the same root cause.

**Walker-parity was treated as the correctness oracle. It is a
scaffold-artefact oracle.** W0a.2.d through W0a.2.h (five sub-waves,
≈20 agent-hours) chased byte-for-byte walker tape equivalence. The
W0a.2.h pivot retires this as oracle — shape emission defines the
canonical tape; the `*_parity.rs` Value-level harnesses verify
end-to-end correctness against external comparators (sonic-rs,
lightningcss, simdjson OnDemand, serde_json, cssparser) and self-
parity. Any record-count or variant-idx divergence between walker
and shape emission is retroactively non-blocking.

R1, R2, R4 each propose the same corrective edict; R3 provides the
infrastructure for the fresh-context restart to be economical.

## Quantified waste (R1 §Q1–Q6)

- ≥ 14 bootstrap regen cycles across W0a.2 (each 5–10 min).
- ≥ 5 `cargo test --workspace` full runs.
- 4 independent unclassified-Ref BFS probes producing the same
  43 → 0 observation.
- 4 verbatim copies of the 7×3 predicate table in diag docs.
- 5 re-reads of `shapes/mod.rs:331`.
- 6 touches on the same gate predicate (`has_w4_classified` /
  `has_shape_dispatcher_entrypoint`).
- 26 GB RSS OOM on an aggregate test binary, resolved by a per-
  grammar split that should have been plan-time infrastructure.
- 47 AX commits from `2faedca5..HEAD`; zero runtime activation of
  the stipulated W0a hard gate #2 (`parse()` zero walker-reach for
  CSS / Sheets / BBNF).

Split of surfaced blockers: ~30% fundamentally-latent
(empirically-surfaceable only under contact); ~70% walker-parity
chasing the pivot retroactively retires.

## What landed and is load-bearing (R4 §P1)

Nineteen commits across eight sub-waves. The pivot keeps every one
of them — each is substrate or correctness-of-emitter-self-
consistency. None are walker-shape matching that the pivot
invalidates.

Key anchors:

- `has_w4_classified` narrowed to `Pratt | Unordered` (W0a.1).
- `has_shape_dispatcher_entrypoint` BFS narrowed to docstring
  intent (W0a.2).
- Array emitter split Shape 1 / Shape 2 (W0a.2.a).
- `ShapeTag::AltDispatch` + fixed-point detector; Flat / Scalar
  widened (W0a.2.b) — closes the 43 → 0 Ref graph.
- `tape_parity` per-grammar split (W0a.2.e infra).
- `shapes/inline.rs` speculative-branch rollback (W0a.2.e
  correctness).
- `#[inline(always)] → #[inline]` compound-shape downgrade
  (W0a.2.f) — breaks the LLVM inliner SIGBUS cycle.
- Keyword Ref-led Alt + state-threaded signature (W0a.2.g).
- Inline Alt ByteDispatch vs AltLinear split (W0a.2.g).
- W0a.2.h surgical emitter fixes (inline Seq Alt-branches with
  Refs, OW trivia preservation, AltDispatch Seq branches, Keyword
  Seq branches, Flat `{lo=0, hi=1}` optional rewind).

## What to stop doing (R4 §P2, R1 §R1.5)

- **Walker-parity tape-shape matching.** Shape emission's record
  stream is the authoritative tape. Downstream (`Root::View`,
  `TapeCursor`) consumes by `TapeKind` + semantic cursor API; it
  does not demand walker-identical record counts.
- **Substrate-only landings.** Any emitter module shipping
  `pub(crate) mod …` without a same-wave consumer is a deferral
  (R1 §Q2). `shapes/inline.rs` at W0a.2.d was an instance.
- **Redundant admission-widening + revert cycles.** Three such
  cycles shipped then reverted (`030fb8aa` → `7a311a28`,
  `f6e1ecb5` → `63895dee`, `29bfd055` revert-on-master). Gate the
  predicate flip on cross-scope consumer readiness, not just
  emitter readiness.
- **Serial-probe on cascading blockers.** When diagnosis
  enumerates more than two architectural blockers, dispatch
  parallel probe-agents on disjoint bounds (R2 §A3.1 → SPEC Edit
  4).
- **Verbatim diag cloning.** Four predicate tables, five mod.rs
  reads. Standing-context files (R1 §R1.3, R2 §P1 `AGENT_BRIEF_
  TEMPLATE.md`) eliminate this.

## W0a.2.h cross-scope blocker (residual)

The pivot's second-order discovery: shape-emission tape differs
from walker tape in compound-layout, and three downstream
consumers depend on walker's specific compound shape:

- `crates/core/src/grammar/host.rs::walk_tape` — identifies rules
  via `find_descendant_by_kind(rule_item, <Kind>::rhs)` which keys
  on a Wrap-emitted Rule compound with specific `variant_idx`.
- `crates/core/src/lower/expression.rs::collect_sub_variants_walk`
  — keys on sub-variant identities (`grammar_item_0`,
  `directive_0`, `term_1`, `term_2`, `value_atom_0`). Under
  shape-emission tape projection these collapse.
- `crates/core/src/graph/deps.rs` — Pratt detector keys on the
  sub-variants above. With sub-variants collapsed, Pratt emission
  sites dropped 22 → 0 on the second bootstrap cycle.

These are the four remaining blockers for admission-wide shape-
authoritative operation. All are in named file paths with
known-narrow scope.

## Plan redress (R4 §P3–P7)

**W0b simpler, not harder.** Pre-pivot, W0b gated on "walker not
reached from `parse()`." Post-pivot, admission widening closes
that gate in W0a. W0b deletes walker code + retires
`body_has_dispatcher_fallback_position` alongside
`has_w4_classified`, `has_full_shape_coverage`,
`has_shape_dispatcher_entrypoint`, and the `tape_parity_*.rs` (six
files) + `tape_parity_common/mod.rs`. Agent count stays at 4.

**W0c augmented.** Reframe walker role as historical scaffold.

**W1–W15 mostly unchanged.** W2 scope SHRINKS (no new shape-parity
harness; semantic `*_parity.rs` inherits from W0a).

**New invariant 20** proposed and landed in `AX.md`:

> Tape shape is shape-emission-authoritative. Downstream
> correctness is asserted by AST-level `*_parity.rs` harnesses
> against external comparators, not by record-count or column-
> layout equivalence against the walker. Walker tape is a
> historical scaffold retired in W0b; the shape emitter's own
> output is the one source of truth for `TapeCursor` + `Root::View`
> consumers.

**New wave proposed: `W0a.close`** — 17-entry bench baseline
immediately post-W0a, pre-W0b, so every downstream wave attributes
deltas to its own lever rather than walker-death-cost.

## Infrastructure (R3 — landed in commit `62d40d90`)

Three new scripts:

- `scripts/test-tier.sh {leaf, grammar, workspace}` — tiered test
  runner matching the three-tier convention README already
  documents. Agents iterating on mining/IR pass run `leaf` (~1 min
  cold); shape-emitter iteration runs `grammar` (~3–5 min); wave
  close runs `workspace`.
- `scripts/worktree-status.sh` — enumerates sibling worktrees
  with HEAD / dirty / target-symlink / branch state. First-pass
  tool for session resume.
- `scripts/kill-all-rust.sh` — grace-then-kill orphan rustc /
  cargo processes; `--dry-run` / `--include-lsp` options.

One script edited:

- `scripts/seed-worktree.sh` now owns `target/` symlink by
  default; `--no-target` opt-out. Agents stop reinventing the
  pattern per-wave.

Config:

- `Cargo.toml` — new `[profile.ax-iter]` inheriting `dev` with
  `debug = 0, strip = "debuginfo"`. Iteration-time tests use
  `--profile ax-iter`; `dev` stays default for samply work. On
  aggregate test binaries, peak rustc RSS drops ~3×.
- `.cargo/config.toml` — commented `lld` block path corrected
  (`/opt/homebrew/opt/lld/bin/ld64.lld`; previous path pointed to
  a non-existent binary in the `llvm` keg).

## Edicts redress (R2 — landed in commit `cd443f75`)

Seven surgical edits to `README.md` and `SPEC.md`:

1. Escape-clause single-sourced to `SPEC.md §Scope-reveal`.
2. New `SPEC.md §Transitional fallback during elimination waves`
   — one-codegen-path invariant binds at tranche close, not every
   wave close.
3. README "no deferrals" clarified — Absorb/new-letter per SPEC
   are not deferrals.
4. New `SPEC.md §Parallel-probe on >2 candidate blockers` —
   dispatch probe-agents in parallel when diagnosis enumerates
   multiple blockers.
5. New `README.md §Memory discipline for aggregate test binaries`
   — `CARGO_BUILD_JOBS=4`; split ≥ 4-derive-site binaries.
6. README §Worktree isolation — `target/` symlink escape
   documented.
7. `SPEC.md §Runtime-evidence` — pre-regen vs post-regen evidence
   distinction; gate authors state explicitly.

Landed in this session:
- `docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` — reusable
  sub-agent prompt template; cuts per-dispatch prose ~50%.
- Walker-parity-as-snapshot-oracle edict — lives as AX.md
  invariant 20 (R4 P7); no separate `ORACLE.md` needed.
- `docs/tranches/AX/waves/W0a.close.md` — bench-baseline wave
  spec per WAVE_SPEC.md format.

Genuinely deferred (legitimate scope):
- sccache rollout — needs installer + measurement; post-AX.
- `serialize_roundtrip.rs` + `grammar_profile_wire_contract.rs` +
  `gate_predicate_wire_contract.rs` splits — emitter-adjacent;
  W0b follow-on.
- `bootstrap-bbnf.sh` delta regen — proc-macro cache-key work.
- `cargo expand` content-hash caching — W13 tooling sweep.
- Hard-gate floor-check tooling (R2 P3) — W13/W14 tooling sweep.

## The next wave — AX.W0a.2.i

The cross-scope blocker in W0a.2.h has four named file paths plus
downstream Pratt detector re-keying. Propose dispatch as three
parallel sub-agents on disjoint bounds, bundled via orchestrator
consolidation:

- **Agent A — host.rs / walk_tape.** Replace `find_descendant_by_
  kind(…)` keyed on walker Rule-compound with a shape-emission
  compatible cursor. Either (a) cursor API reads by `TapeKind` +
  semantic projection, or (b) shape emitter pushes a semantically
  equivalent Rule compound with a stable discriminator. File
  bound: `crates/core/src/grammar/host.rs`.
- **Agent B — lower/expression.rs sub-variant projection.** Re-
  key `collect_sub_variants_walk` on IR-structural identity, not
  tape-walker-compound-identity. File bound:
  `crates/core/src/lower/expression.rs` + `crates/core/src/graph/
  deps.rs`.
- **Agent C — admission widening + regen + final activation.**
  Re-apply `29bfd055` (W0a.2.h's admission-widening commit) once
  Agents A + B land. Bootstrap regen idempotent, workspace tests
  green. Commits final `post-AX-W0a-close.json` bench. File
  bound: `shapes/mod.rs` (predicate deletion) + regen output +
  wire-contract expected-map flip.

Each agent runs with R3 infra (`test-tier.sh`, `seed-worktree.sh`
with target symlink, `--profile ax-iter`, `CARGO_BUILD_JOBS=4`).

Memory guards enforced: aggregate test binary split already
landed (W0a.2.e). One parallel cargo invocation per agent. No
workspace test during iteration; `test-tier.sh leaf` suffices.

## Ceremonial close of W0a

Per R1 §R1.5 and SPEC.md §Scope-reveal-protocol, once W0a.2.i
lands:

- `docs/tranches/AX/FINAL.md` (closing document) written for the
  W0a window — landings, reverts, deferred items (if any), bench
  baseline pointer.
- Immediately afterwards, W0b opens per the existing (unchanged)
  `docs/tranches/AX/waves/W0b.md` plan; four parallel agents on
  walker deletion + substrate-without-consumer purge + crate
  renames + test carve.

AX.md's wave summary table, invariants (with new #20 from R4),
and operational posture remain the authoritative plan.

## Restart checklist

Before the fresh-context orchestrator dispatches W0a.2.i:

- [ ] Read this synthesis + AX.md invariants 1–20.
- [ ] Read `docs/tranches/AX/PROGRESS.md` (state).
- [ ] Read `docs/benchmarks/archive/post-AX-W0a2h-progress.md` §Halt
      rationale for the cross-scope blocker concretes.
- [ ] Verify R3 scripts executable:
      `scripts/worktree-status.sh` shows current state,
      `scripts/test-tier.sh leaf --no-run` completes.
- [ ] Verify memory guards in place: `CARGO_BUILD_JOBS=4` in
      shell env; `--profile ax-iter` available.
- [ ] Pre-create three worktrees via
      `scripts/seed-worktree.sh ../bbnf-wt-ax-w0a-2i-{a,b,c}`.
- [ ] Dispatch Agents A + B in parallel; Agent C serial after.
- [ ] Close W0a with FINAL.md; dispatch W0b.
