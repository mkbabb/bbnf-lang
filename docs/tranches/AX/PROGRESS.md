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

## 2026-04-18 — W0a.2.a + W0a.2.b dispatch
