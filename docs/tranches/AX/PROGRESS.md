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

## 2026-04-19 — W0a.2.j close + W0a.2.k dispatch

**W0a.2.j landed** (cherry-picked onto master at `5f0709fc` + `ee03e8fa`
+ `07e254f7`):

- Admission widening: `body_has_dispatcher_fallback_position` retired;
  `has_shape_dispatcher_entrypoint` expected-map flipped for 6 non-JSON
  grammars (`5f0709fc`).
- Two root causes at tape-emission layer (`ee03e8fa`):
  1. **Wrap parent compound** — `emit_alt_tape_dispatch` pushes a Rule
     compound with `variant_idx = rule.id` + `meta_idx = branch_ordinal`.
     Restores the walker-era heterogeneity discriminator IR lowering
     needs without reintroducing walker code.
  2. **Flat iteration wrapper TapeKind** — `emit_tape_repeat` pushed
     `TapeKind::Rule` at optional + generic Repeat wrappers; IR cursor
     `iter_rep_children` peels only `TapeKind::Repeat`. Switched to
     `TapeKind::Repeat` at both emission paths.
- Bootstrap regen cycle-1 = cycle-2 byte-identical at 97,573 lines
  (`07e254f7`). Pratt sites: 8. Heterogeneous `*Value<'p>` enums:
  4 (`value_atomValue`, `termValue`, `directiveValue`,
  `grammar_itemValue`). Idempotency restored.

**Agent halted on downstream defects** (gates 4-7) surfaced by the
widening + shape-authoritative routing:

1. **PRECEDENCE_LUT coverage** — `binary_factor` gets ShapeTag::Pratt
   (`parse_pratt_BbnfBootstrap_binary_factor` emits) but NOT in
   `__DTA_SHUNTING_YARD_RULES` (only value_mul + value_add, RuleIds
   13 + 14). `collect_operator_chains` walks `shunting_yard_chains`
   only; binary_factor's `<<`/`>>`/`-` operators never reach
   PRECEDENCE_LUT — bytes 60 (`<`) + 62 (`>`) carry 0. Gorgeous
   `#[derive(Parser)]` on json.bbnf / bnf.bbnf / sheets.bbnf panics
   because their sources use `<<`/`>>` as BBNF meta-operators.
2. **parse_pratt arena-frame sizing** — emitter pushes 1 byte
   (`op_discriminant`) then calls `push_leaf_with_arena_frame` which
   reads a 4-byte frame; panics `offset 0 + 4 exceeds arena len 1`.
   Gorgeous `#[derive(Parser)]` on ebnf.bbnf panics at this site.

Both defects are latent on master (pre-W0a.2.j) because walker-routed
parsing bypasses these code paths; activate under shape-authoritative
routing. Neither is in W0a.2.j's emitter-scope allow-list.

**W0a.2.k dispatched** — single-agent wave on Pratt LUT propagation +
arena-frame sizing. Allow-list: `shapes/pratt.rs`,
`passes/recognizers/operator_chain.rs`,
`passes/recognizers/dta.rs`,
`passes/recognizers/shape_dispatch/pratt.rs`, and generated.rs regen
window. Worktree `/Users/mkbabb/Programming/bbnf-wt-ax-w0a-2k` seeded
at HEAD `07e254f7`.

Hard gates: gorgeous check clean (6/6 derive sites compile); workspace
tests 0 FAILED; cycle-1=cycle-2 byte-identical with bytes 60/62
populated in PRECEDENCE_LUT; 4 het Value enums preserved; nm shows
shape-dispatcher routing on 4 bench binaries.

## 2026-04-19 — W0a.2.k reverted + W0a.2.l dispatch

**W0a.2.k landed then reverted** (commits `015d02af`/`1ab22a9d`/
`7c3ea838` landed, then reverted as `f585ce37`/`4178254a`/`3256858d`).

Per-rule PRECEDENCE_LUT (Option B + C hybrid from agent) + 1-byte
arena-frame API (`push_leaf_with_arena_payload`) correctly closed
defects 1 + 2 (gorgeous compiles cleanly, cycle-1=cycle-2 byte-
identical at 97,977 lines, bytes 60 + 62 populated). But the agent
made an out-of-scope architectural call — **flat Pratt tape,
removing reducer-compound emission**, on the grounds that variant_idx
= op_discriminant was corrupting `rule_kind()` dispatch.

Orchestrator verification revealed broader regression than the
agent's self-report:
- `css_l4_parity.rs` — 11/16 FAILED (pre-revert: 16/16 green).
- `sheets_parity.rs` — 14/25 FAILED.
- Multiple CSS L4 payload-firing tests + Sheets error-literal tests.
- `tape_parity_*` across 5 grammars FAILED (walker-parity oracles
  retiring at W0b, invariant 20 — these were OK to break).

Scope-reveal classification: agent crossed plan-declared invariant
(walker-parity reducer-compound emission IS the consumer contract
for `*_parity.rs` semantic harnesses; only walker-parity `tape_
parity_*` oracles retire under invariant 20). Per SPEC §Transitional
fallback: revert-to-green + name follow-on wave.

Master post-revert: `3256858d`. `css_l4_parity` + `sheets_parity`
16/16 + 25/25 respectively. Gorgeous still fails pre-W0a.2.k state
(defects 1 + 2 unfixed).

**W0a.2.l dispatched** — re-do W0a.2.k's correct parts (per-rule LUT
+ Option B miner + 1-byte arena-frame API) WITHOUT flat-Pratt-tape
change. Reducer-compound emission must be preserved; any variant_idx
encoding issue is fixable in emitter mapping, not by restructuring
the tape. W0a.2.k's archive:

- `/tmp/ax_w0a2k_miner_diff.patch` — operator_chain.rs changes.
- `/tmp/ax_w0a2k_pratt_diff.patch` — shapes/pratt.rs changes.
- `/tmp/ax_w0a2k_builder_diff.patch` — push_leaf_with_arena_payload
  API addition.
- `/tmp/ax_w0a2k_gen{1,2}_archive.rs` — idempotent regen outputs
  (97,977 lines).

Worktree `/Users/mkbabb/Programming/bbnf-wt-ax-w0a-2l` at HEAD
`3256858d`.

## 2026-04-19 — W0a.2.l substrate landed; W0a.2.m dispatch

**W0a.2.l landed on master** (`64d6ab2f` + `e5ff835e` + `34be629e` +
`7d2fa1b8`):

- `push_leaf_with_arena_payload(kind, span_lo, span_hi, variant, meta,
  arena_offset, payload_width)` — 1-byte arena-frame API with bounds
  check (`64d6ab2f`).
- TapeVisitor mirror (`e5ff835e`).
- Option B+C hybrid miner: `OperatorChainFacts::entries` →
  `Vec<OperatorChainRule>` per-rule; `collect_operator_chains`
  keys on `ShapeTag::Pratt` directly; within-rule first-byte
  disjointness check (`34be629e`).
- Per-rule PRECEDENCE_LUT consts emitted (8: value_{mul,add,path,
  input,cmp,and,or}, binary_factor) + per-rule PRECEDENCE_ENTRIES
  consts; `parse_pratt_<rule>` bodies reference `PRECEDENCE_LUT_<rule>`.
  Reducer-compound emission **preserved** (`34be629e` + `7d2fa1b8`).
- Bootstrap regen cycle-1 = cycle-2 byte-identical at 98,106 lines.
  Pratt sites 8, 4 het Value enums, reducer compounds 8/8.
  `PRECEDENCE_LUT_binary_factor[60] = 129`, `[62] = 129`.

**Consumer-side halt**: W0a.2.l correctly surfaced a pre-existing
inconsistency in `crates/core/src/lower/expression.rs::
lower_binary_factor`. Pre-W0a.2.l, parse_pratt_binary_factor
early-terminated on `<`/`>` bytes (LUT zero) so the reducer-compound
chain was never built at runtime. Post-W0a.2.l, Pratt correctly
dispatches `<<`/`>>`/`-` operators → builds reducer-compound tree
→ `collect_binary_operands` walks the outer Pratt compound via
walker-era assumptions (iteration-wrapper flat `[operand, op,
operand, op, …]` sequence) → sees single-child reducer chain →
dispatches reducer-compound as operand → reducer's `rule_kind()`
resolves to `float_lit` (variant_idx = op_discriminant = 1 for
`>>`) → panics `lower_term: unknown leading byte '"' for rule_kind
float_lit (span = "\"<\" >> identifier ")`.

Blocks gorgeous derives on bnf.bbnf (and json.bbnf via
`>>`/`<<` meta-operators) → blocks cargo test on bbnf lib (gorgeous
is a path dep, not dev-dep). Leaf/ir/tape crate tests unaffected.

## 2026-04-19 — W0a.2.m dispatch

Single-agent wave on `lower/expression.rs::lower_binary_factor` +
`collect_binary_operands` to teach the consumer to walk reducer-
compound chains correctly. Allow-list: `lower/expression.rs`;
potentially `lower/tape_walk.rs` for cursor helper additions.
Worktree `/Users/mkbabb/Programming/bbnf-wt-ax-w0a-2m` seeded at
HEAD `7d2fa1b8`.

Hard gates: gorgeous check clean (6/6 derive sites compile);
workspace tests 0 FAILED excluding retiring tape_parity_*; `cargo
test -p bbnf --test css_l4_parity` 16/16; `sheets_parity` 25/25;
cycle-1 = cycle-2 byte-identical preserved (no regen changes;
generated.rs untouched in this wave).

## 2026-04-19 — W0a.2.{n,o,p,q,r} cascade + W0a close

Post-W0a.2.m, gorgeous compile + parity-harness suite surfaced nine
classes of shape-emission + payload defects across the six emitter
paths (pratt, wrap, hregex, keyword, alt_dispatch, flat, inline).
Each class required tight-scoped surgical fixes; the cascade closed
across five sub-waves:

- **W0a.2.n** (`8dce3270`/`9afb2d44`/`2d084d76`/`a5aac742`/`49ed4b95`/
  `26480ae0`/`6e8958b5`) — Pratt loop `skip_space` + phantom-op guard
  + whitespace-aware operator peek. Gorgeous 6/6 compile unblocked.
  Regen 98,202 lines. bbnf_parity 2/2, bbnf_ast_parity 9/9,
  ax_w0a2h_probe 33/33.
- **W0a.2.o** (`5a451df1`/`5e886cca`/`f50f9d27`) — Pratt-wrapper-peel
  admits leaked 3-child flat layout (`lower_binary_factor` virtual
  tail-reducer), factored-Alt operator mining, first-byte LUT merge.
  Regen 98,299 lines. JSON 22/22 → 0 failures. css_l4_parity 5/16 →
  14/16. sheets_parity 11/25 → 13/25.
- **W0a.2.p** (`2b7f9744`/`d1fc8c0d`/`36f19e09`/`fd7ab1d6`/`4bb295dc`/
  `a726fdce`) — Pratt detector narrowing (reject regex-only operator
  rules → complexSelector demotes Pratt→Flat), Keyword typed-leaf
  arena payload, Flat typed-Alt + Map-regex host-fn emission, Keyword
  Ref-branch rollback + length-descending order. css_l4_parity 14/16,
  sheets_parity 22/25. No regen (no BBNF grammar changes).
- **W0a.2.q** (`a776de3c`/`48ecdea5`/`a4465484`/`16952531`/`381cb9cd`)
  — Wrap typed Alt Regex-branch arena payload, HRegex typed-payload
  arena emission, AltDispatch typed-Alt literal-branch arena payload
  + Regex-branch pattern routing, leading-dot Number via regex-scan.
  css_l4_parity 16/16 ✅. sheets_parity 24/25.
- **W0a.2.r** (`c92ceee9`/`6b03dd53`) — inline.rs Alt Regex branch
  uses actual pattern (mirrors alt_dispatch fix). Regen 98,270 lines,
  cycle-1 = cycle-2 byte-identical. sheets_parity 25/25 ✅.

### W0a parity-harness closure at HEAD `6b03dd53`

```
bbnf_parity         2/0  ok
bbnf_ast_parity     9/0  ok
css_l4_parity      16/0  ok
json_parity         9/0  ok
json_value_parity  13/0  ok
sheets_parity      25/0  ok
```

All six invariant-20 semantic parity harnesses green on master.
gorgeous derive compiles clean on all 6 grammars (JsonParser,
CssL4Parser, EbnfParser, BnfParser, GoogleSheetsParser, BbnfParser).
Bootstrap regen cycle-1 = cycle-2 byte-identical at 98,270 lines,
8 per-rule Pratt LUTs, 4 heterogeneous `*Value<'p>` enums, 251
skip_space call sites, reducer-compound emission preserved.

W0a closes at HEAD `6b03dd53`. Proceeding to W0a.close (single-agent
17-entry bench baseline wave).

---

## 2026-04-20 — W1 sub-waves landed; AX planning phase opens

W1 ran per the absorb re-plan. Eight sub-waves commissioned, eight
landed on master across orchestrator-dispatched worktrees:

| Sub-wave | Commit | Deliverable |
|----------|--------|-------------|
| W1r.0    | `3429aaba` | Revert W1.A/B (−6,128 LOC); sonic-rs → dev-dep |
| W1r.1    | `5d5096eb` | IR-derived named-type resolver (static BINDINGS → `FxHashMap<StringId, Vec<TypeDesc>>` walker); diag at `audit/W1r1-diag.md` |
| W1r.2    | `a6429d3e` | JSON canonical-parity vs sonic-rs (10/1 + `strip_insignificant_ws`) |
| W1r.3a   | `933d02fb` → `b930cf2c` → `293be673` | CSS L4 `@pretty` directives + `?w`/`@ws` threading fix + 3/0 parity harness (byte on normalize, scale+interop on bootstrap/tailwind) |
| W1r.4a   | `f6a264e2` → `28fd46fc` → `53d99e4a` | `@pretty sep(X)` codegen fix (`backend/prettify/sep_rewrite.rs`) + regen + sheets_self_parity 84/0 |
| W1r.5    | `53318493` | BBNF self-parity 56/0 over 28 `.bbnf` fixtures |
| W1r.6    | `81627d7c` | Typed-accessor surface audit 14/0 (295 rules × 7 accessor classes) |
| W1r.7    | `ab7c218d` | Twitter lazy-field bench via NodeView; AoS 4.14× SoA ax-iter, 1.67× release |

**W1 outputs green in aggregate**: 13 parity + canonical harnesses
pass on master with 247 tests + 1 ignored (`data_xl` debug-assertions
gate; runs under `--release`).

**Scope-reveals documented**:

- **W1r.1** — `TypeDesc::Named` collapses to concrete tuple in the
  Rust pipeline before emit; the static `BINDINGS` slice was dead
  code on every grammar. Refactor's value is code hygiene + readiness
  to populate if upstream preserves `Named`. Upstream-preservation
  investigation folds into next tranche. Diag:
  `docs/tranches/AX/audit/W1r1-diag.md`.
- **W1r.3 / W1r.3a** — lightningcss `PrinterOptions { minify: false }`
  performs `calc()` arithmetic simplification + position-pair
  commutativity + shorthand reordering that no symmetric bytes-level
  normalizer can invert. Bootstrap.css + tailwind.css ship as
  scale+interop tests (bbnf parses + prettifies + output re-parses
  on both bbnf and lightningcss) rather than byte-parity. CSS calc
  evaluator deferred. Diag: `docs/tranches/AX/audit/W1r3-diag.md` +
  `audit/W1r3a-diag.md`.
- **W1r.4 → W1r.4a** — `@pretty sep(X)` double-emitted against rule
  bodies with `<<` separators; fix is codegen-level in
  `crates/core/src/backend/prettify/sep_rewrite.rs` (new module).
  Cross-grammar audit: only Sheets declares `sep(X)` currently;
  3-line leak fix in the Repeat loop applies universally.

**Pre-existing AX debt surfaced during W1 execution** (tracked for
next tranche):

- 5 stale W0a/W0b-era test files reference retired predicates +
  carved GrammarProfile fields: `bbnf_profile_wire_contract.rs`,
  `grammar_profile_wire_contract.rs`, `json_parity_shape_emit.rs`,
  `gate_predicate_wire_contract.rs`, `aw_v_w5_2_per_ref_routing.rs`.
  All fail to compile. Per invariant 14 these retire with their
  predicates; W0b.D's "delete 8 DTA-coupled test suites" missed them.
- `ebnf_prettify.rs::{parse_single_rule, parse_multi_rule}` fail at
  offset 0 on valid EBNF source. Pre-existing; not caused by W1r
  landings (bbnf_self_parity 56/0 parses ebnf.bbnf fine via
  BbnfEmit). EBNF recognizer divergence to investigate.
- `post-AX-W1-close.json` bench matrix not yet captured.
- AX `FINAL.md` not yet written.

**Planning phase opens**. Deep audit waves to dispatch per user
directive: direct-to-struct projection generalization, Value API
performance characterization, apples-to-apples competitor benches
across JSON/CSS/Sheets/BBNF (compile + parse time), samply
attribution. New tranche letter to follow.

---

## 2026-04-19 — W1 absorb re-plan (W1.A/B reverted; grammar-derived rewrite)

W1.A (hand-coded `bbnf::json::Value` iso `sonic_rs`) and W1.B
(hand-coded `bbnf::css::StyleSheet` iso `lightningcss` with 22
`TypeOnly` stubs) violated invariants 4 (no new grammar
directives), 11 (no per-grammar prototypes), 18 (no placeholder
surfaces). Per SPEC §Scope-reveal Absorb: revert + re-scope W1 in
place; no letter pivot.

**Landed during this session, to preserve**:

- W0a through W0a.2.s (77/77 parity harnesses, real-CSS 3/3).
  Master HEAD `5dab5175` at cascade close.
- W0a.close bench baseline — `docs/benchmarks/post-AX-W0a-close.json`
  with 18/18 numeric entries after CSS real-corpus fix. Commit
  `1241e7ac`.
- W0b (DTA interpreter deletion ~85K LOC, crates renamed,
  simd-scan/emit purged, tests carved). HEAD `0adabb23`.
- W0c (AW-V.md RD-language rewrite). HEAD `db9c4e06`.
- W1.D (hybrid SoA+AoS tape + `stp_span` + twitter bench 1.8×).
  Commits `abb4c956` / `e3a28fce` / `52e633cc`. Preserved
  unchanged through the revert.

**To be reverted in W1r.0**:

- `crates/core/src/backend/rust/view/json/` (W1.A, ~450 LOC)
- `crates/core/src/backend/rust/view/css/` (W1.B, ~3,050 LOC)
- `crates/core/tests/json_value_api.rs`
- `crates/core/tests/css_l4_value_api.rs`
- `docs/tranches/AX/parity/css_divergence.md`
- `Cargo.toml` sonic-rs runtime → dev-dep.

**Re-dispatch W1** per `waves/W1.md` revised — 7 sub-waves:
W1r.0 revert serial → W1r.1/2/3/4/5/7 parallel → W1r.6 after
W1r.1. Six-wide fan-out after revert. Worktree naming
`../bbnf-wt-ax-w1r-{0..7}`. Parity proves via
canonical-serialization byte equality on both parser sides; no
`From<third-party>` / `PartialEq<third-party>` bridges anywhere
in source (test harnesses call comparators through the
comparator's own public API, not through a bbnf bridge).

AX.md invariant 21 added (grammar-derived view surface);
§Indefatigability bullet 3 revised; W1 wave-summary row
rewritten.

---

## 2026-04-18 — W0a.2.a + W0a.2.b dispatch

(Trailing dispatch-record entry preserved out of strict chronological
order; the substantive close is the W0a cascade entry above. Wave-
boundary closures W0a.2.a + W0a.2.b have their own dated entries
upstream in this log.)

---

## 2026-04-20 — AX closes; AY opens

AX final state: HEAD `411eabfd`. 13 parity + canonical harnesses
green (247 passed, 1 ignored). Bench matrix saved to
`docs/benchmarks/post-AX-W1-close.json`. FINAL.md authored at
`docs/tranches/AX/FINAL.md`.

AX shipped substrate-and-API closure across W0a (gate repair +
emitter cascade closing 5/5 defects), W0a.close (intra-AX bench
baseline), W0b (interpreter deletion + crate renames), W0c (AW-V
RD-language rewrite), and W1r (eight sub-waves landing the
grammar-derived view surface with canonical-serialisation parity
on JSON, CSS L4, Sheets, and BBNF). The W1 absorb re-plan reverted
W1.A/W1.B's hand-coded value duplicates and added invariant 21
(grammar-derived view surface); no third-party comparator bridges
ship anywhere. Bootstrap idempotent at 98,270 lines, 8 per-rule
Pratt LUTs, 4 heterogeneous `*Value<'p>` enums, reducer-compound
emission preserved.

Block B (W2-W14: parity CI gating, lever portfolio, e-graph
rewriting, document-parallel) did not execute under AX's letter.
Per SPEC §Scope-reveal "new letter" response mode, the W1 audits
surfaced an AU-substrate ~4.5× regression and a json-prototype
speed-ceiling reveal that together exceed any single-wave
extension; Block B routes wholesale into AY, the BEAT-sonic
tranche. Five wire-contract test files and two emitter-shape test
files compile-fail at AX HEAD because their predicates retired in
W0a.2.j and their fields carved in W0b.A; AY.W0 retires them per
the invariant 14 discharge as the precondition for AY.W1 substrate
restoration.

AY opens at `docs/tranches/AY/AY.md` — BEAT-sonic via AU substrate
restoration (W1) + e-graph G1-G9 + wrap-compound elision (W2) +
json-prototype per-shape inline emission (W3) + SIMD unescape +
Eisel-Lemire direct-to-column (W4). AY.W0 retires legacy debt as
precondition; AY.W7 declares BEAT-sonic at close.
