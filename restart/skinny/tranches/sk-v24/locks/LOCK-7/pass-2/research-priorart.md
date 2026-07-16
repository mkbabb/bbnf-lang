# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — PRIOR-ART SWEEP (pass 2)

Prior-art researcher, no family allegiance. What exists in-tree, what won/lost elsewhere for
this problem class (self-hosting parser generators, staged bootstrap, grammar-specified planes,
typed host-fn escape hatches), the constraints our evidence base already imposes, and the traps
named in comparable systems. Citations `path:line` (codebase) / URL (web).

---

## 0. The problem class, stated

Constitution clause (i): "everything is grammar-driven, here and there, save primitives and
combinators — the regex grammar IS bootstrapped and bbnf-defined." LOCK-7 is that clause as a
design problem: (a) bbnf-in-bbnf self-hosting depth, (b) the regex-grammar bootstrap (staging,
fixpoint, committed stage-0), (c) grammar-specified planes (IR? emitter templates? oracle
tables?), (d) typed host functions for context-dependence, (e) what "primitive" lawfully means,
(f) xtask/regen as bootstrap engine. Standing law (PORTFOLIO §LOCK-7, and memory
`no-workarounds-self-hosting`): **self-hosting uses language features — augment BBNF if features
are missing, never work around.**

---

## 1. WHAT EXISTS IN-TREE (the honest baseline)

### 1.1 bbnf self-description EXISTS; self-HOSTING does NOT

- `grammar/bbnf/{bbnf.bbnf (89), expressions.bbnf (37), types.bbnf (7)}` — 133 LOC, a complete
  bbnf-in-bbnf grammar with `@import` modularity, `->` typed leaves (`identifier -> Span`,
  `int_lit -> i64`), and directive rules for `@import/@recover/@pretty/@ws/@token/@debug/@host`
  (`bbnf.bbnf:60-81`).
- It is VERIFIED to parse ITSELF into the typed AST: `crates/core/tests/bbnf_parity.rs`
  `bbnf_parses_its_own_grammar` — "The BBNF grammar must parse its own definition cleanly into
  the canonical typed AST" (test header, cutover.D4 reach gate).
- BUT the PRODUCTION front-end is HAND-WRITTEN Rust: `crates/grammar/` = 2142 hand LOC of
  recursive descent → `GrammarIr` (R1 §1 table). `bbnf.bbnf` is consumed as a **fixture / reach
  oracle**, not as the generator's own input. This is a self-DESCRIPTION (the grammar can be
  parsed by the hand parser) — NOT self-HOSTING (the grammar is not compiled to the parser that
  parses grammars). The gap between these two is the entire LOCK-7 7B↔7D axis.

### 1.2 The regex grammar is NOT bootstrapped (and is stale)

- `grammar/misc/regex.bbnf` (28 LOC) exists but uses SUPERSEDED syntax: `>>`, `-`, `{ }`
  repetition, no `->` projections, no `?w` (`regex.bbnf:2-28`) — it predates the current
  bbnf surface (`bbnf.bbnf` uses `?w`, modifiers, `->`). It parses nothing on the live path.
- The PRODUCTION regex engine is hand-written HIR in parse-that: `rust/regex/src/classify.rs`
  (1148 LOC — the "1148-LOC classify" the constitution names), `automata/nfa.rs` (449),
  `automata/dfa.rs` (438), `first.rs` (158). Constitution clause (i) demands the regex grammar
  be bbnf-defined AND bootstrapped; clause (ii) demands HIR true-isomorphism to bbnf HIR. Today
  neither holds — the regex grammar is a dead 28-line file and the HIR is an independent
  hand-rolled tree. This is the largest single totality gap and it is co-owned with LOCK-5.

### 1.3 xtask/regen is a staleness checker, NOT a fixpoint engine

- `skinny/xtask/src/main.rs` (104 KB) + `regen.rs` (6956 B) + `regen_css.rs`. Targets:
  `regen-json/check-json`, `regen-css`, `regen-sheets`, `regen-real-typed` (R1 §5;
  `main.rs:40-47`). Mechanics: `regen.rs:19 write_targets` → `codegen::emit_runtime_from_request`
  → `write_to_dir`; `check_target` fails if stale (R1 §5).
- **No `regen-bbnf` target. No `regen-regex` target. No fixpoint gate** (regen∘regen =
  byte-identical). The check-* arms verify "generated matches source" (a staleness diff), NOT
  "the generator reproduces itself" (a bootstrap fixpoint). Memory `clean-regen-discipline` +
  `changed-flag-convergence` already bind here.
- `xtask/src/real_typed_schema.rs` is a 40 KB **hand-written schema** (the crutch GN-3.W2 kills;
  SPEC GN-3). Its existence is measurable counter-evidence to totality: a hand-authored artifact
  standing in for a grammar-derived one. Any "totality percentage" (7C) must count it.

### 1.4 The typed host-fn interface has SYNTAX but no live consumer

- `@host identifier : type_name` (`bbnf.bbnf:73`) — the declared-signature seam for
  context-dependent semantics already exists in the grammar surface. Memory
  `hybrid-grammar-host`: grammar expresses leaf semantics via `->`; host functions for
  context-dependent/recursive; strong typing. Memory `grammar-authoritative-status`:
  Phase 1+2 done, **Phase 3 (host fns) PENDING**. So 7C's substrate exists syntactically but is
  not wired — consumer-less substrate is forbidden (Constitution viii / Y.13).

### 1.5 A promotion/oracle gate pattern already exists (7D seed)

- `bbnf_parity.rs` (self-parse reach gate) + `primitive-checkasm` (the dav1d-style scalar↔NEON
  parity gate, R1 §5, `main.rs:52`) are the two existing "differential parity as admission gate"
  patterns. 7D META-CIRCULAR-ORACLE generalizes exactly this: the bbnf-defined parsers serve as
  differential ORACLES against the hand parsers; promotion is gated on measured parity + perf.

---

## 2. WHAT WON / LOST ELSEWHERE (external prior art)

### 2.1 Meta-grammar self-hosting — the canonical modern win

- **CPython PEG bootstrap (pegen), Guido van Rossum.** The reference blueprint for this exact
  problem: a meta-grammar (grammar-for-grammars) parsed by a bootstrap meta-parser → generates a
  new meta-parser → which parses ITSELF → verified by re-parsing the same meta-grammar and
  checking the output is stable. Replaced the 30-year-old hand-rolled `pgen` in CPython 3.9. The
  fixpoint check is the whole point.
  <https://medium.com/@gvanrossum_83706/a-meta-grammar-for-peg-parsers-3d3d502ea332>
- **META II (Schorre, 1964)** — the ur-metacompiler, defined in its own language; **OMeta**
  (Warth, PEG-based) — "a natural and convenient way to implement tokenizers, parsers, visitors,
  tree-transformers", self-describing. The lesson that survived 60 years: a metacompiler is
  bootstrapped from a minimal committed stage-0 written once in the host language, then rewritten
  in itself. <https://en.wikipedia.org/wiki/OMeta>
- **pest (Rust).** Grammar-as-documentation-AND-implementation, compile-time codegen; pest's own
  `.pest` grammar is bootstrapped (meta-grammar in `pest`). This is the model bbnf-lang half-lives
  today (compile-time codegen via xtask) but WITHOUT the self-host loop closed.
  <https://github.com/pest-parser>
- **ANTLR / tree-sitter** — both maintain a meta-grammar (ANTLR-in-ANTLR;
  tree-sitter's `grammar.js` DSL). tree-sitter is the incremental-parsing SOTA the constitution
  is conscious of.
  <https://www.pistack.xyz/posts/2026-06-20-parser-generator-combinator-libraries-antlr-treesitter-nom-pest-nearley/>

**Verdict for 7A/7B/7D:** the meta-grammar → bootstrap-meta-parser → self-parse → fixpoint
pattern is proven, repeatedly, across META II / OMeta / pegen / pest. bbnf-lang has every
ingredient (`bbnf.bbnf`, the hand parser as stage-0, xtask as regen host) but has NOT closed the
loop: no `regen-bbnf`, no fixpoint gate, no promotion of the generated parser to production.

### 2.2 Staged bootstrap with byte-identical fixpoint — the verification win

- **rustc / GCC 3-stage bootstrap.** stage0 (pre-existing/native compiler) → compiles source →
  stage1 → rebuilds → stage2; **stage1 and stage2 producing byte-identical executables is the
  correctness proof** ("if stage1 and stage2 produce identical executables, then they behave
  identically"). GCC's 3-stage comparison is the same idea. This is 7A's "regen∘regen = regen,
  byte-identical" verbatim — a settled, industrial pattern.
  <https://rustc-dev-guide.rust-lang.org/building/bootstrapping/what-bootstrapping-does.html>
- **Reproducible-builds / bootstrappable-builds movement** — bit-for-bit identical rebuild from
  source is now a maintained discipline, not aspiration. Directly underwrites 7A's committed
  stage-0 + fixpoint gate. <https://lwn.net/Articles/841797/>

### 2.3 Typed host functions / external scanners — the escape hatch EVERY serious system keeps

- **tree-sitter external scanners.** "Any parser generator that sees use has a bail-out to a
  hand-written stateful external lexer because context-sensitive grammars are equivalent to
  linear-bounded automata." Used for indentation (Python), heredocs, raw strings, template
  literals — tokens no regular/CFG grammar can express. State serialized across scans.
  <https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html>
- **Verdict for 7C:** the host-fn lattice is NOT optional and NOT a failure of totality — it is
  the *correct architecture*, present in every production grammar system. The design question the
  constitution poses is the RIGHT one: totality as a **measured percentage behind grammars with a
  named floor per subsystem** and a **TYPED, bounded** host interface (`@host name : type`),
  never an untyped C-FFI escape. bbnf's `@host` with a type annotation is already stricter than
  tree-sitter's opaque C scanner.

---

## 3. CONSTRAINTS OUR EVIDENCE BASE ALREADY IMPOSES

- **`no-workarounds-self-hosting` (STANDING LAW, PORTFOLIO §LOCK-7):** if a leaf cannot be
  grammar-expressed, augment BBNF — a host shim that hard-codes (e.g.) TOML-datetime parsing is
  a name-gate in disguise (GN-5 §7 R2 names this exact trap). The host-fn is for genuine
  context-dependence, not for dodging a missing grammar feature.
- **Zero name-gates, CI-enforced (SPEC §1 differentiator; GN-5.W3 `check-name-gates`).** The AST
  audit forbids grammar-name string literals in codegen. A bootstrapped-grammar plane must not
  reintroduce name-gates in its emitter/host dispatch — GN-5 §7 R3 even names the enum-variant
  circumvention (`GrammarId::Csv`) the gate must also catch. Any LOCK-7 host-dispatch design
  inherits this.
- **Generated files regen-only, LOC-budgeted (memory `clean-regen-discipline`,
  `generated-size-budget`; SPEC §4).** A `regen-bbnf`/`regen-regex` target inherits the same
  born-RED bit-for-bit gate every other target carries, plus a generated-LOC sub-budget.
- **Consumer-in-wave / Y.13 (Constitution viii; memory `pluggable-components`).** A bootstrapped
  bbnf/regex grammar with no runtime caller is dead substrate. 7D's answer — the grammar first
  serves as a differential ORACLE (a consumer) before promotion — is the constitution-aligned
  path (it has a consumer from day one).
- **Type plane costs zero at runtime (Constitution v; SPEC §0 row 08).** Typed host-fn signatures
  and grammar-derived schemas erase to zero runtime cost — the HM core (221 LOC, PROVE-08) is the
  proven engine, `real_typed_schema.rs` the hand crutch it replaces (GN-3).
- **True HIR isomorphism (Constitution ii; LOCK-5 owns the verdict).** The regex-grammar bootstrap
  cannot be designed in isolation — its HIR must be the bbnf HIR (5A) or a lawful functor (5B).
  LOCK-7's regex-bootstrap and LOCK-5's continuum are the same seam viewed from two locks.
- **SPEC §0 refutation laws** are mostly perf (no digit-SIMD, no block-scheduling, no shim-perf,
  fold-over-cursor dead, warm benches disinformation). They bind LOCK-7 only where a
  grammar-derived scanner claims a number — the bootstrap itself is structural, but any perf
  claim on a promoted grammar-derived parser (7D's promotion gate) must clear the cold-paired-PMU
  standing law.

---

## 4. TRAPS NAMED IN COMPARABLE SYSTEMS

- **Trusting-trust (Ken Thompson, 1984).** A self-hosting toolchain can hide a payload in the
  committed stage-0 that reproduces itself invisibly. Countermeasure: **committed stage-0 must be
  minimal + auditable** (7A "minimal bootstrap surface, maximal auditability") and verified by
  **Diverse Double-Compiling** (David A. Wheeler) — two independent paths producing bit-for-bit
  identical output. 7D's differential-oracle IS the DDC pattern applied to grammars.
  <https://dwheeler.com/trusting-trust/>
- **Grammar/meta-parser drift.** When the hand parser and the generated parser both exist, they
  silently diverge unless a differential oracle gates every commit (the mrustc-lags-rustc
  pathology: the second implementation rots). Directly motivates 7D: the self-parse must be a
  CI-enforced differential gate, not a one-shot test — today `bbnf_parity.rs` is a reach gate, not
  a full differential oracle. <https://en.wikipedia.org/wiki/Bootstrapping_(compilers)>
- **The external-scanner bug well.** tree-sitter's external scanners are the #1 source of grammar
  non-determinism and serialization bugs — the escape hatch is where correctness goes to die if
  untyped/unbounded. Validates the TYPED, floored host-fn interface (7C) over an open FFI.
- **Left recursion in PEG self-hosting.** pegen/OMeta both had to solve left-recursion to
  self-host; bbnf's `binary_factor`/`alternation` rules (`bbnf.bbnf:45-52`) are
  right-associative-by-construction today, but a self-hosted grammar-of-grammars must either
  handle left recursion or forbid it explicitly (a named language restriction, not a silent
  failure). <https://arxiv.org/pdf/1207.0443>
- **Self-host overfit / the hand-schema crutch.** `real_typed_schema.rs` (40 KB hand-written) and
  the stale `regex.bbnf` are local evidence that "self-hosting" claims outrun the artifacts.
  A totality metric (7C) that counts hand LOC-behind-grammar keeps the claim honest — matching the
  campaign's "concrete artifacts or the SPEC-ONLY label" discipline (Constitution vii).
- **Bootstrap chicken-and-egg cost.** Every bootstrap needs a pre-existing stage-0; the hand
  `grammar` crate (2142 LOC) IS bbnf-lang's natural stage-0. The trap is deleting it too early
  (mrustc shows the second impl must be *maintained* to stay trustworthy) — 7A keeps it committed
  and auditable; 7D promotes subsystem-by-subsystem on measured parity, never big-bang.

---

## 5. SYNTHESIS — the design surface these facts define

1. **7A STAGED-BOOTSTRAP is directly supported** by rustc/GCC 3-stage + reproducible-builds:
   commit the hand `grammar` crate as stage-0, add `regen-bbnf` + a fixpoint gate
   (regen∘regen byte-identical) to xtask. The missing piece is purely the fixpoint gate and the
   loop closure — every ingredient exists.
2. **7B TOTAL-SELF-HOST** has proven precedent (pegen/pest/META II) but the IN-TREE state is far
   from it: regex grammar dead, IR hand-written (1020 LOC), emitter templates hand-written
   (codegen 10201 LOC), `real_typed_schema.rs` hand-authored. Totality is a long road; measure it.
3. **7C HOST-FUNCTION-LATTICE** is the *correct* architecture (tree-sitter proves every system
   keeps the escape hatch); bbnf's typed `@host name : type` is already stricter than the
   industry norm. Wire it (Phase 3 pending) and floor the totality percentage per subsystem.
4. **7D META-CIRCULAR-ORACLE** is the safest promotion path and the trusting-trust/DDC-aligned
   verification story; `bbnf_parity.rs` + `primitive-checkasm` are the existing seed patterns to
   generalize into full differential oracles.
5. The regex-grammar bootstrap is **not separable from LOCK-5** (HIR isomorphism, clause ii) —
   any LOCK-7 verdict on regex staging must consume LOCK-5's HIR form.

---

### Citations index

Codebase: `grammar/bbnf/{bbnf,expressions,types}.bbnf`; `grammar/misc/regex.bbnf`;
`crates/core/tests/bbnf_parity.rs`; R1-skinny-cartography §1/§5/§6 (grammar 2142, ir 1020,
codegen 10201, regen targets, path trap); `skinny/xtask/src/{main.rs,regen.rs,real_typed_schema.rs}`;
`parse-that/rust/regex/src/{classify.rs:1148,automata/nfa.rs,dfa.rs,first.rs}`;
PORTFOLIO §LOCK-7 (task + standing law) + Constitution i-ix; SPEC §0/§1/§4; GN-5 §7 (R2/R3 traps).
Memory: no-workarounds-self-hosting, hybrid-grammar-host, grammar-authoritative-status,
clean-regen-discipline, changed-flag-convergence, generated-size-budget, pluggable-components,
no-silent-epsilon.

Web: pegen meta-grammar (van Rossum); OMeta (Wikipedia); pest (github); ANTLR/tree-sitter/pest
comparison (pistack); rustc bootstrap stages (rustc-dev-guide); bootstrappable builds (LWN);
tree-sitter external scanners (tree-sitter.github.io); Diverse Double-Compiling (dwheeler.com);
Bootstrapping-compilers (Wikipedia); Left recursion in PEG (arxiv 1207.0443).
