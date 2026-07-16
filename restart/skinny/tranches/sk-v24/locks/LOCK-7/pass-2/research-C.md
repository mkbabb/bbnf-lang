# LOCK-7 pass-2 — research-C — Family 7C: THE HOST-FUNCTION-LATTICE

Totality bounded by a TYPED host-fn interface. Context-dependent and recursive semantics live
behind declared signatures; totality is a MEASURED percentage of grammar surface, with a named
floor per subsystem. All seams are skinny@HEAD (`skinny/crates/...`).

## 1. The mechanism, made concrete

The lattice is not a new subsystem — it is a partition of the construction census that already
exists, into three tiers keyed on what a node needs from the host:

- **Tier 0 — PURE (`add_expr`, the `->` floor).** Node values are pure projections built by
  `GrammarIr::add_expr` (`ir/src/lib.rs:51`, a plain append). Leaf semantics ride `-> Span`/`-> T`
  through `parse_projection` (`grammar/src/lib.rs:1481`) → `ExprKind::Project` (`ir/src/lib.rs:261`).
  No host, no context. This is the grammar-plane totality target.
- **Tier 1 — CONTEXT (`resolve_refs`, `&mut Ctx`).** Cross-rule linkage is `resolve_refs`
  (`ir/src/lib.rs:84`): one mutable pass rewriting `ExprKind::Ref{name,target}` → resolved
  `target` rule-id at BUILD time, emitting zero runtime code. This is the canonical
  context-dependent mechanism and the zero-cost witness (§4). Its sibling is TOML's `push_path`
  table-path accumulator (GN-5.W2, `docs/tranches/sk-v24/GN-5.md:216-229`): `[table]`/`[[array]]`
  nesting is context-dependent and needs an inherited-attribute accumulator of exactly the
  `resolve_refs` shape.
- **Tier 2 — HOST ESCAPE (`@{}`).** The typed escape hatch. Today `@{...}` is SCANNED
  (`grammar/src/lib.rs:420-431` → `RuntimeHostCapture`, struct at `:170`) and then REJECTED as
  `BBNF-UNSUPPORTED-HOST-CAPTURE` (`grammar/src/lib.rs:222`). There is **no `ExprKind::Host`
  variant** (`ir/src/lib.rs:234` enum: Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation/
  Project/Discard — no host arm), so a captured host body never becomes a node. The self-grammar
  `grammar/bbnf/bbnf.bbnf` itself USES it (`term = ... | "@{" , rhs ?w , "}"`), and the URL
  grammar tests it (`grammar/src/lib.rs:1737`, `@{ "url", "(" >> ident ?w , "," ?w , ident << ")" }`).

## 2. The split-independence axis (central 7C deliverable)

**LOCK-FREE — proceeds without the LOCK-1 solver verdict:**

- **Kill the `passes/src/lib.rs:794-816` heuristic.** `has_parse_time_host_decode` fires on
  `name.contains("host") && (value.contains("decode") || value.contains("parse"))` — a
  SUBSTRING sniff over annotation text. This is a name-gate by another name (the exact defect
  class GN-5.W3's `check-name-gates` exists to forbid, `GN-5.md:275-339`). Replace with a
  structural predicate `is_host_parse_time(expr) := matches!(kind, ExprKind::Host{..})` once the
  variant exists. It feeds `ShapeRationale::HostFnParseTime` (`ir/src/cost.rs:255`, referenced
  `passes/src/lib.rs:621`) into `requires_eager_tape` (`passes/src/lib.rs:762`) — that consumer
  and rationale entry survive unchanged; only the DETECTION moves from string-sniff to typed node.
- **Lower `@{}` at `grammar/src/lib.rs:222`.** Add `ExprKind::Host { body: StringId, sig }` to
  `ir/src/lib.rs:234`; route the existing scan fact through `parse_atom` (`grammar/src/lib.rs:1495`)
  so `@{...}` yields a real node, not a rejection. Clear the three bounded parser gaps in the same
  motion (`?w`→`->` attachment in `parse_seq:1361`; `@{}` atom in `parse_atom:1495`; `|params|`
  closures) so `bbnf.bbnf` reparses through the `!has_rule("value")` general-descent branch
  (`codegen/src/runtime_generator.rs:98`).
- **Name `resolve_refs`** (`ir/src/lib.rs:84`) as the canonical CONTEXT-tier construct — it is the
  template every `&mut Ctx` accumulator (push_path) copies.

**LOCK-1-BOUND — cannot be finalized without the solver spine (XL-3):**

- **Typed signature into inference.** The `sig` on `ExprKind::Host` must be a `TypeDesc` the
  LOCK-1 HM core unifies against the surrounding `->` chain (`SPEC.md` feature 08 / GN-3 walls).
  Whether the host fn's declared return type participates in row/sum unification is the
  inherited-attribute verdict the census note flags as LOCK-1-owned (DIRECTIVES §census: "the host
  floor depends on a LOCK-1 inherited-attribute verdict").
- **Tier discriminator off `&mut Ctx`.** Pure host fns (`&[u8] -> T`) are zero-cost-composable
  Tier-2 escapes; `&mut Ctx` host fns ARE the CONTEXT tier and carry inherited-attribute cost.
  Whether inherited attributes live inside the type system or a side-channel is LOCK-1's call; the
  tier boundary is provisional until it lands.

**Blocker on the census/totality% deliverable:** it "needs bbnf to parse+build" — currently RED
because the `@{}` rejection + three parser gaps stop `bbnf.bbnf` (31 rules, 36 alt-bars, 7 `->`, 27
`?w`, `@import` closures, 1 `@{}`) from routing through the emitter at all.

## 3. The totality% counter (leaves × branches) + floor-gate

**Counter substrate = the built `GrammarIr` expr arena.** Walk `grammar.exprs`:
- LEAVES = `ExprKind::{Literal, Regex, Ref}` + projection leaves (`Project` over a terminal).
- BRANCHES = Alt arms + Seq positions + Repeat/Optional/Discard bodies.
- HOST-COVERED = any leaf/branch reachable ONLY through an `ExprKind::Host` node, or flagged
  `HostFnParseTime`.
- **totality% = 1 − (host-covered leaves×branches) / (total leaves×branches)**, reported
  per-subsystem (regex leaves, dispatch, tape-assembly) with a NAMED floor each.

**Floor-gate = a GN-5 `check-name-gates` sibling: `check-host-floor`.** An AST walk over
`crates/*/src` plus the GrammarIr census (a `syn`-backed artefact, never grep — GN-5 precept 19),
asserting each subsystem's totality% ≥ its declared floor. **Planted-breach falsification** mirrors
`xtask/tests/name_gate_falsification.rs` (GN-5.md:310-316): plant a rule pushing a leaf behind
`@{}` that COULD be `->`-expressed, assert `check-host-floor` trips (totality% drops below floor);
assert the real tree passes clean. A gate that cannot fail is a defect (SPEC §0; GN-5.md:44,499).

## 4. CONTEXT-tier zero-cost invariant (testable BEFORE the tier is scheduled)

Constitution v (type plane costs zero at runtime) governs the CONTEXT tier. It is testable NOW,
without LOCK-1, on the extant `resolve_refs`:
- **resolve_refs round-trip.** parse→build→`resolve_refs` stamps `target` ids at COMPILE time; the
  emitted recognizer must contain zero references to the symbol. Test = regen a grammar, assert the
  generated `.rs` has no `resolve_refs`/name-lookup call (the resolution is fully residualized).
- **TOML `push_path` (GN-5.W2).** The table-path accumulator runs at PARSE time but must not surface
  in the stop-early/get lane (SPEC feature 06) — it is a build/parse-tier concern, not a runtime
  navigation cost. Named counterexample to guard: a TOML grammar whose table-path resolution
  materializes into the offset tape leaks the CONTEXT tier into runtime and violates the invariant.

## 5. Prior art (cited)

- **Attribute grammars, inherited attributes (Knuth 1968).** The CONTEXT tier IS inherited
  attributes; `&mut Ctx` = the inherited-attribute environment, `push_path`/`resolve_refs` =
  synthesized→inherited symbol tables. The tier taxonomy is the AG synthesized/inherited split.
- **tree-sitter external scanners** (`tree_sitter_<lang>_external_scanner_scan(payload, lexer,
  valid_symbols)`). The closest analog to `ExprKind::Host{body,sig}`: the grammar DECLARES an
  external token, the host provides a scanner behind a typed ABI. Confirms the signature is a TRUST
  boundary — tree-sitter cannot verify the C scanner honors its declared token (R1 below).
- **Menhir / yacc semantic actions (`%{ %}` header, `{ }` actions)** — host code embedded in the
  grammar, typed by the host compiler; the direct precedent for the `@{}` capture.
- **rust regex-automata** — DFA construction fully grammar-derived, zero host: the model for
  driving scanning OFF the host floor (the LOCK-5 continuum is where totality% is maximized).
- **GHC `deriving` / rustc lang-items** — the principled "primitive" boundary: what lawfully stays
  host (Constitution i "save primitives and combinators").

## 6. Integration seams (path:line)

- `grammar/src/lib.rs:222` — `HostCapture → BBNF-UNSUPPORTED`; the rejection replaced by lowering.
- `grammar/src/lib.rs:420-431` — the `@{}` scan producing `RuntimeHostCapture` facts (`:170`).
- `grammar/src/lib.rs:1495` `parse_atom` — where `@{}` must mint `ExprKind::Host`.
- `grammar/src/lib.rs:1361` `parse_seq` (`?w`→`->` gap), `:1404` `parse_postfix`, `:1481` `parse_projection`.
- `ir/src/lib.rs:234` `ExprKind` (add `Host`), `:51` `add_expr` (Tier-0 append), `:84` `resolve_refs` (Tier-1).
- `passes/src/lib.rs:794-816` `has_parse_time_host_decode` (substring heuristic to kill), `:762`
  `requires_eager_tape` (consumer), `:621`/`ir/src/cost.rs:255` `ShapeRationale::HostFnParseTime`.
- `codegen/src/runtime_generator.rs:98` `!has_rule("value")` — the general-descent branch bbnf routes through.
- `docs/tranches/sk-v24/GN-5.md:275-339` — the `check-name-gates` + falsification pattern the floor-gate clones.

## 7. Named risks / counterexamples

- **R1 — the signature is a trust boundary, not a proof.** An `@{}` body is opaque host text; a
  declared `sig` cannot be checked against it without a host-language typecheck pass. Like
  tree-sitter external scanners, the tier is unsound if the host lies. The sig is DECLARED-and-trusted.
- **R2 — killing `has_parse_time_host_decode` may shift eager-tape selection.** Any grammar relying
  on the substring match must regen-diff byte-identical after the swap; a divergence means the
  heuristic was load-bearing and the structural predicate must reproduce it exactly (SPEC bit-for-bit gate).
- **R3 — totality% is gameable (host-fn-as-name-gate).** A host fn hard-coding e.g. TOML datetime
  parsing is a name-gate in disguise — GN-5.md:518-524 (Risk R2) names this exactly: "a host
  function that hard-codes TOML datetime parsing is a name-gate in disguise; check-name-gates must
  be extended to audit host functions." The floor-gate must audit host-fn BODIES, not just count nodes.
- **R4 — CONTEXT-tier leak.** If `push_path` materializes into the tape rather than staying
  build/parse-tier, the zero-cost claim fails; test a TOML grammar whose table-path resolution runs
  per-parse.
- **Cascade counterexample (shared empirical question).** `bbnf.bbnf`'s `term`/`call_arg` recursion
  routes through `@{}` + imports; if `-> Span` over `term`/`factor` round-trips but the
  `@{}`-bearing branch cascades, the failing construct is the host node inside a projected branch.

## 8. What a prototype must test to move 7C

1. Add `ExprKind::Host{body,sig}`; lower `@{}` (`grammar:222/1495`); route `bbnf.bbnf` through the
   `!has_rule("value")` emitter (`runtime_generator:98`) — does it parse+build? (unblocks the census).
2. Swap `has_parse_time_host_decode` substring sniff → structural `is_host(expr)`; regen-diff every
   extant grammar byte-identical.
3. Build `check-host-floor` + planted-breach falsification (GN-5.W3 pattern); prove it trips before it guards.
4. resolve_refs zero-cost: regen a grammar, assert generated `.rs` contains zero `resolve_refs`/`push_path` symbols.
5. Compute totality% (leaves×branches, host-covered subtracted) on ≥1 real grammar — `bbnf.bbnf`
   (31 rules / 36 branches / 1 `@{}`) and the sheets 39-branch tower — and publish the per-subsystem floors.
