# AX Progress Log

## 2026-04-18 — Tranche open

AX opens on master at commit `2faedca5` (`docs(instructions): tranche/
subdir + WAVE_SPEC per-wave format`). Successor to AW-V close
(`a12618ea`). Plan `AX.md` + eighteen `waves/W<N>.md` specs live on
master prior to first execution commit, per the plan-on-master
invariant.

**State at open.** 0/17 parse entries exceed post-AU per AW-V.W6
close. JSON routes through shape dispatcher; CSS/Sheets/BBNF
delegate to `__dta_walker_inline::run`. `has_w4_classified` gate at
`crates/core/src/backend/rust/emitter/grammar.rs:718` over-restricts
JSON's visitor path. Eighteen waves target repair, interpreter
deletion, value API, parity harnesses, lever portfolio, e-graph
grammar rewriting, closing with AY handoff.

**Sequencing.** W0a opens immediately. W0a is 2-serial (gate narrow
→ routing + wire-contract). W0b is 4-parallel (interpreter deletion
+ emit/ purge + crate renames + test carve). W0c is 1-serial
(AW-V.md rewrite). W1 onwards proceeds per the per-wave specs.

**Orchestration ledger starts here.** Every wave boundary appends an
entry with commit hashes, verification artefacts cited, and re-plan
notices if any. Hard-gate closures cite the artefact, not the claim.

---

## 2026-04-18 — W0a.1 close

`has_w4_classified` narrowed to `Pratt | Unordered` at
`crates/core/src/backend/rust/emitter/shapes/dispatcher.rs:836`
(commit `9f8aed90`). Follow-through: `shapes/mod.rs` per-rule
visitor emitter now dispatches Flat / Wrap / ArgList / HRegex to the
pre-existing emitters (each bound by a subset of the dispatcher's W3
trait union). Bootstrap regen commit `af8f6840`. `cargo expand`
evidence at `docs/benchmarks/post-AX-W0a1-expand-json.txt` shows
`parse_with_visitor` re-emitted for `json_monolithic_value`.

## 2026-04-18 — W0a.2 scope reveal (absorb)

Agent probed four bench binaries' `parse()` bodies and reported
three-layer scope reveal. Diag doc at
`docs/benchmarks/post-AX-W0a2-diag.md`.

- **L1** — `has_shape_dispatcher_entrypoint` narrowed to
  entry-reachable BFS (commit `9b1b54e2`). Matches docstring intent;
  no admission outcome flips (diagnostic confirms every non-JSON
  grammar has genuine entry-reachable unclassified Refs).
- **L2** — 43 entry-reachable Refs currently unclassified:
  CSS (34), Sheets (1), BBNF (4), EBNF (2), BNF (2). Not a predicate
  bug — grammars genuinely fail entry-reachable closure. Needs
  detector coverage extension or new shape (AltDispatch for
  Alt-of-classified-Refs rules like CSS `value`, BBNF `alternation`).
- **L3** — `shapes/array.rs:105` emitter hardcodes `Some(b'[')`.
  Array detector admits both JSON-style `"[" ... "]"` wrap (Shape 1)
  AND entry-rule list no-wrap (Shape 2: CSS stylesheet `rule*`, BBNF
  grammar `rule+`) but emitter only implements Shape 1. Routing CSS
  or BBNF through the current emitter would syntax-error at byte 0.

Wire-contract test `crates/core/tests/gate_predicate_wire_contract.rs`
lands (commit `69d28f56`) freezing the 7×3 = 21-assertion matrix.
Invariant 9 + W0a.2.c will amend the `expected` map when detectors
widen.

**Absorb plan.** Per SPEC §Scope-reveal "Absorb" mode, W0a splits
into three new sub-phases dispatched in two parallel + one serial
wave:

- **W0a.2.a** (parallel) — Array emitter generalises: detect at emit
  time whether the rule body matches Shape 1 (wrapped) vs Shape 2
  (entry-list), emit the branch-appropriate body. Split or
  parameterise `emit_parse_array`; add Shape 2 emission.
- **W0a.2.b** (parallel) — Classify the 43 entry-reachable Refs.
  Either extend existing detectors to admit Alt-of-Refs rules
  (CSS `value`, BBNF `alternation`, EBNF/BNF `rule`) via a new
  `AltDispatch` shape with byte-dispatch emission, or prove each
  admits Scalar/HRegex/Flat fallback. Delete the `__value` fallback
  emission now that predicate is strict.
- **W0a.2.c** (serial, depends on both) — Extend `grammar.rs:515`
  `parse_body` with a non-Alt-rooted branch tail-calling
  `parse_<shape>_<grammar>_<entry_rule>` directly. Regen bootstrap;
  update wire-contract `expected` map (admission widens).

`nm` + `cargo expand` across all four bench binaries re-verifies
`parse()` → shape path (zero `dta_run_<grammar>` / `#walker_fn_ident`)
at W0a close.

## 2026-04-18 — W0a.2.a close

Array emitter split into `emit_parse_array_wrapped` (Shape 1,
JSON-style `"[" … "]"`) + `emit_parse_array_list` (Shape 2,
entry-rule list, no wrap — CSS stylesheet, BBNF grammar). Data-driven
branch at emit time via `unwrap_wrap` / `single_byte_literal`
helpers. Walker tape parity preserved (tape_parity 22/0). JSON
Shape 1 emission byte-identical pre/post refactor. Commit `ee7f81da`.

## 2026-04-18 — W0a.2.b close

New `ShapeTag::AltDispatch` shape admits `Alt(Ref | Literal | Regex |
Seq_of_leaves, …)` rules. Detector fixed-point classifies until
stable (branch-target tags stabilise). Flat widened to admit
Repeat-rooted bodies + Repeat heads + leaf-like Alt heads.
Scalar widened to admit single-Ref bodies. 43 → 0 entry-reachable
unclassified Refs across the 6 non-JSON grammars. AltDispatch
emitter at `shapes/alt_dispatch.rs` byte-dispatches per branch
first-byte set; visitor path stays inside W3 trait union. 10 commits
`517be13c` → `7f3dbafb`, cherry-picked. Orchestrator bootstrap regen
`610928a6` idempotent. Full workspace 167/167 test suites green.

## 2026-04-18 — W0a.2.c decision + W0a.2.d dispatch

W0a.2.c (parse() routing) is dependent on full inline-position
admission; deferred to after inline-emission lands. W0a.2.d opened
for inline Alt / Regex / Negate / Minus / TokenDispatch emission —
eliminating the `#dispatcher_ident` fallback in every shape
emitter's position-core so `has_shape_dispatcher_entrypoint`
admits the 6 non-JSON grammars.

## 2026-04-18 — W0a.2.d scope-reveal (substrate only)

Agent landed `shapes/inline.rs` (1239 lines) exporting
`emit_inline_position_tape` + `emit_inline_position_visitor`
helpers for Alt / Regex / Negate / Minus / TokenDispatch inline
emission. Commit `1e603586`. Substrate compiles clean.

Consumer-wiring attempt reverted under contact. Two blockers
surfaced:

- **Compile-time explosion.** `cargo test -p bbnf --test tape_parity
  --no-run` with all 5 derive-Parser sites consuming the inline
  helpers peaked at 26 GB RSS for a single rustc process before
  pre-OOM kill. CSS L4's 28 `*Decl` Flat rules carry 1-3 inline Alt
  positions each; cumulative token-stream size pushed LLVM codegen
  into pathological growth in the 5-grammar aggregate binary.
  Single-grammar test binaries compile in ~11 s without pressure.
- **Walker-parity gap.** Self-hosted bootstrap's second regen cycle
  produced a 23-line stub (canonical symptom per README §Self-host
  circular-dependency escape). The inline Alt emission's
  variant_idx / PSI / frame-stack discrimination doesn't match
  walker's `emit_alt_linear_arm` semantics (`lower_state.rs:1387`).
  Ref-in-branch Alt works; Alt-compound-level variant_idx stamping
  and per-branch child-record shaping diverge.

Diag doc `57afc25b` cherry-picked. Memory mitigation: worktree
target/ now symlinks to main target/ so parallel worktree builds
share artefacts; `CARGO_BUILD_JOBS=4` policy in subsequent agent
briefings.

## 2026-04-19 — W0a.2.h partial close + research wave + restart preparation

W0a.2.h landed four surgical emitter bug fixes (commit `3feb4999`) +
probe (`7464df6b`) + halt diag (`3328ff6c`):

- Inline Seq Alt-branches with Refs — `emit_structural_branch_tape`
  replaces dead `return Err(())` stubs.
- OptionalWhitespace trivia preservation — `unwrap_trivia` no longer
  strips OW before OW-arm match.
- AltDispatch Seq branches — Ref-led emission fixed.
- Keyword Alt arm — admits Seq branches.
- Flat `Repeat { lo=0, hi=1 }` — iter rewind on failure.

Admission-widening commit `29bfd055` intentionally NOT cherry-picked.
Master keeps the narrow predicate; the four emitter fixes activate
once admission widens. Cross-scope blocker documented at
`docs/benchmarks/post-AX-W0a2h-progress.md §Halt rationale`:

- `grammar/host.rs::walk_tape` depends on walker-shaped Rule compound
  with variant_idx stamping.
- `lower/expression.rs::collect_sub_variants_walk` and `graph/deps.rs`
  Pratt detector key on those sub-variant identities.
- Shape-emission tape collapses those identities; cycle-2 regen
  dropped 22 → 0 Pratt emission sites.

## 2026-04-19 — Research wave (4 parallel agents, audit-only)

Orchestrator dispatched four read-only research agents to audit the
last ten sessions and redress plan + instructions + infra for the
fresh-context restart. All four produced artefacts under
`docs/tranches/AX/audit/`:

- R1 (`d0a7987c`) — waste-pattern archaeology. Documents 14
  bootstrap regens, 4 independent Ref probes, 4 verbatim predicate
  tables, 26 GB OOM. 70% of W0a.2 scope-reveals were walker-parity
  chasing the pivot retires.
- R2 (`cd443f75`) — edicts redress. Seven surgical edits to
  `README.md` + `SPEC.md`: escape-clause consolidated, transitional-
  fallback clause added, parallel-probe guidance, memory discipline,
  worktree `target/` symlink documented, pre-regen vs post-regen
  evidence distinction. Two proposals deferred:
  `AGENT_BRIEF_TEMPLATE.md` + walker-parity-oracle edict.
- R3 (`62d40d90`) — infra speedups. `[profile.ax-iter]` in workspace
  `Cargo.toml` (–3×RSS on aggregate binaries); `test-tier.sh` /
  `worktree-status.sh` / `kill-all-rust.sh` scripts;
  `seed-worktree.sh` owns target symlink; `.cargo/config.toml` lld
  comment fixed.
- R4 (`68f604ad`) — plan redress. AX.md invariant 20 added (shape-
  emission authority); invariant 18 augmented; W0b deletion list
  adds `tape_parity_*.rs` (6 files) + `tape_parity_common/`; W0c
  scope augmented; W2 scope shrinks (no new shape-parity harness).
  New `W0a.close` bench-baseline wave proposed.

Synthesis at `docs/tranches/AX/audit/SYNTHESIS.md` consolidates the
four reports + articulates the W0a.2.i restart plan (3 parallel
agents on `host.rs`, `lower/expression.rs` + `graph/deps.rs`, and
admission-widen + regen).

## Restart handoff

Fresh-context orchestrator reads:
1. `docs/tranches/AX/audit/SYNTHESIS.md` first.
2. `docs/tranches/AX/AX.md` (invariants 1–20, wave summary).
3. This PROGRESS.md for state.
4. `docs/benchmarks/post-AX-W0a2h-progress.md` §Halt rationale for
   cross-scope blocker concretes.

Then dispatches W0a.2.i per SYNTHESIS §"The next wave".

## 2026-04-19 — W0a.2.i dispatch

Three sub-agents dispatched; allow-lists disjoint. Master HEAD
`26f714a1` at dispatch; worktrees seeded with `target/` symlink +
`data/` copy via `scripts/seed-worktree.sh`.

- **Agent A** (`bbnf-wt-ax-w0a-2i-a`) — rewire `crates/core/src/
  grammar/host.rs::walk_tape` off walker-specific compound identity.
  Allow: host.rs, lower/tape_walk.rs, shapes/wrap.rs (option-b
  fallback), ax_w0a2h_probe.rs. Preferred: option (a) cursor API
  over option (b) wrap-emitter stamping (invariant 20 alignment).
- **Agent B** (`bbnf-wt-ax-w0a-2i-b`) — re-key IR-side walks on
  IR-structural identity. Allow: lower/expression.rs, graph/deps.rs,
  passes/types/subvariants.rs + located Pratt detector. Drops stale
  `grammar_item_0 / directive_0 / term_1 / term_2 / value_atom_0`
  match arms; root-causes cycle-2 heterogeneity loss.
- **Agent C** (`bbnf-wt-ax-w0a-2i-c`) — serial, blocked by A + B.
  Re-applies `29bfd055` admission widening; runs bootstrap regen to
  idempotent fixed point preserving Pratt classification; flips
  wire-contract expected map.

Both A + B carry tight scope, ≤ 500-word return caps, hard gates
cited to artefact paths. C awaits cherry-pick of A + B onto master
before dispatch.

## 2026-04-19 — W0a.2.i.a + W0a.2.i.b close; W0a.2.i.c halt

**Agent A** (commits `c4d56a42` + `a46d3f25`, cherry-picked onto master)
— `find_rhs_expression_descendant` in `crates/core/src/lower/tape_walk.rs`
falls back to `closure` / `alternation` Rule descendants when the
shape-authoritative tape elides the `rhs` Wrap compound. Two host.rs
call sites rewired (lines 310, 388). Probe green 20/20 inputs under
locally-flipped predicate; leaf tier green under HEAD-committed
narrow predicate.

**Agent B** (commits `fe17964f` + `9e5cc2f1`, cherry-picked) — dropped
`directive_0 | grammar_item_0` from `expression.rs:214-215`; consolidated
deps.rs `term_1` + `term_2 | value_atom_0` arms into a single
canonical `term | grammar_item | directive | lhs` handler keying on
IR-structural signals (grouped-rhs descent → Rule-child iteration →
span-text extraction fallback). Added 4 synthesised-Alt sub-variant
fixtures in `crates/ir/tests/lattices/subvariants.rs` proving the
IR-level projection is sound. Classified cycle-2 heterogeneity loss
as root-cause (c): tape-side coalesce in `shapes/wrap.rs`.

**Agent C** HALTED per SPEC §Scope-reveal. Milestone-1 admission
widening + wire-contract flip verified in isolation (7/7 gate tests
green), then ROLLED BACK because bootstrap regen cycle-2 reproduces
the W0a.2.h cascade:

| Metric | cycle-1 | cycle-2 | target |
|---|---|---|---|
| Lines | 97,513 | 169,446 | within ±10% |
| `fn parse_pratt_` | 8 | **0** | ≥ cycle-1 |
| `*Value<'p>` enums (`value_atom`, `term`, `directive`, `grammar_item`) | 4 | **0** | preserved |

Artefacts at `/tmp/ax_w0a2i_c_gen1.rs` (97,513 lines, 8 Pratt, 4 het
enums) and `/tmp/ax_w0a2i_c_gen2.rs` (169,446 lines, 0 Pratt, 0 het
enums). C correctly rolled back milestone-1 to keep master clean
(HEAD `9e5cc2f1`). Diagnosis: when bbnf.bbnf is re-parsed under
shape-authoritative Wrap dispatch, the tape projection drives
`lower/expression.rs` to produce IR with homogeneous-typed Alt
branches where walker-routed tape produced heterogeneous ones.
Type inference declares the Alts homogeneous → `*Value<'p>` enum
emission elides → Pratt classification and sub-variant projection
both collapse.

## 2026-04-19 — W0a.2.j dispatch

Single-agent diagnostic + fix probe on the Wrap Alt-branch
coalescing root cause. Allow-list: `shapes/wrap.rs` + adjacent
shape-emitter discriminator surfaces. The agent's job: reproduce the
cycle-2 drift on a clean worktree, identify the specific tape
mechanism that drives lower/expression.rs IR-heterogeneity loss, and
land a fix that restores cycle-2 idempotency with the four
`*Value<'p>` enums + Pratt classification preserved.

---

## 2026-04-18 — W0a.2.a + W0a.2.b dispatch
