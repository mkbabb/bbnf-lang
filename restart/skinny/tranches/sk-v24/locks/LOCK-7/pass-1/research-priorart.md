# LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP — Prior-Art Research (pass 1)

Researcher, pass 1. No family allegiance (REGISTRY withheld by protocol). Every claim cited
`path:line` or a web source. The task (PORTFOLIO LOCK-7): the constitution's clause (i) as a
design problem — bbnf-in-bbnf self-hosting depth, the regex-grammar bootstrap (staging,
fixpoint, committed stage-0), grammar-specified planes (IR? emitter templates? oracle
tables?), typed host functions for context-dependence, what "primitive" lawfully means, and
xtask/regen as the bootstrap engine. Standing law: augment BBNF if features are missing,
never work around.

---

## §1 WHAT EXISTS (the evidence base already in-tree)

### 1.1 bbnf IS self-hosted — in the LEGACY tree, not skinny

- **The meta-grammar exists and is modular.** `grammar/bbnf/bbnf.bbnf` defines BBNF in BBNF:
  `identifier`, `literal`, `regex`, `term`, `factor`, `mapped_factor` (the `->` projection),
  `binary_factor` (the `<<`/`>>`/`-` operators), `concatenation`, `alternation`, `closure`
  (first-class grammar functions `|params| rhs`), `rule`, and every `@`-directive
  (`@import`, `@recover`, `@pretty`, `@ws`, `@token`, `@debug`, `@host`). It `@import`s
  `expressions.bbnf` (the `->` value-expression sub-language: precedence-climbing
  `value_or`→`value_and`→…→`value_atom`, `value_fn_call`, `value_input`, `value_closure`) and
  `types.bbnf` (`type_name` — primitives plus the backend-agnostic `/[_a-zA-Z]\w*/` escape,
  i.e. `TypeDesc::Named`). This is the constitution clause (i) surface, already authored.
- **The self-host parser is committed as stage-0.** `crates/bootstrap/src/lib.rs:27` re-exports
  `::bbnf::grammar::generated::BbnfBootstrap`; the header records that post-B2 the proc-macro
  crate (`bbnf_derive`) was retired and "the IR pipeline runs in `cargo xtask regen`, which
  emits `crates/core/src/grammar/generated/bbnf.rs` directly" — the parser that compiles every
  grammar in the workspace is itself produced by the bootstrap parser (`lib.rs:1-27`).
- **The fixpoint gate exists and is a permanent CI test.**
  `crates/core/tests/bbnf_bootstrap_reproducibility.rs` (AZ-II.cutover.B) regenerates BBNF
  twice into fresh tempdirs via `cargo xtask regen --grammar bbnf --output <dir>` and asserts
  byte-identity — "the byte-equal property is the axiomatic discipline for any self-hosted
  compiler" (`:1-40`). B6.W0 added a no-op-skip optimization: regen skips the on-disk write
  when emitted bytes match, dropping cold-wall regen from ~88 s to ~0.5 s in steady state
  (`crates/bootstrap/src/lib.rs:20-27`). This is exactly 7A's fixpoint gate (regen∘regen =
  regen, byte-identical), already realized.

### 1.2 The regex grammar IS bootstrapped — TWICE, DIVERGENTLY (the one-regex-home breach)

- **parse-that's home copy:** `parse-that/rust/bootstrap/regex.bbnf` — "full L4 spec for
  self-hosting parse-that's regex parser," using `>>`/`<<`/`,` operators; entry rule `regex`
  is last-in-source-order. It generates `parse_that::regex::generated::{RegexParser,
  parse_generated}` (`parse-that/rust/bootstrap/src/lib.rs:23`), driven by bbnf-lang's
  `cargo xtask regen` (the old `scripts/bootstrap-regex.sh` cargo-expand pipeline is retired,
  `src/lib.rs:1-22`). Tests assert it parses `[0-9]{3,5}`, quantifiers, unicode-escape, `(?i)`,
  etc. (`src/lib.rs:28-70`).
- **bbnf-lang's divergent copy:** `grammar/misc/regex.bbnf` — a DIFFERENT regex grammar, EBNF-
  flavored: uses the `-` difference operator (`term = factor - end`), `[ ]`/`{ }` optional/
  repeat sugar, `lookahead`/`lookbehind`/`back_reference`. Operator sets measurably diverge:
  `grammar/misc/regex.bbnf` uses `-` and `>>`; `parse-that/rust/bootstrap/regex.bbnf` uses
  `,`/`<<`/`>>` and NO `-`. A THIRD inline copy lives in
  `crates/core/tests/regex_bootstrap.rs:8+`.
- **Trap (SPEC §4 one-regex-home; LOCK-5 ⇄ LOCK-7):** three regex grammar surfaces exist; the
  constitution demands ONE bootstrapped regex grammar whose HIR is isomorphic to bbnf's
  (clause ii). LOCK-7 must adjudicate which single `.bbnf` is canonical and where it lives
  (parse-that is "the one regex home," SPEC §1) — LOCK-5 decides the lawful HIR form, LOCK-7
  consumes it and owns the bootstrap staging.

### 1.3 xtask/regen is the bootstrap engine — but skinny's copy DOES NOT self-host

- **Legacy `crates/` xtask** self-hosts bbnf+regex (§1.1–1.2). **Skinny's xtask does not.**
  `skinny/xtask/src/main.rs:10` USAGE enumerates only `regen-json|check-json|regen-css|
  check-css-l4-declaration-values|regen-real-typed|check-real-typed|check-conformance|
  lint-loc|bench-json|gate-json|primitive-checkasm` plus `regen-sheets`/`check-sheets`
  (`main.rs:40-52`). There is NO `regen-bbnf` and NO `regen-regex`; grep for
  `bootstrap|BbnfBootstrap|self-host` in `skinny/xtask/src/` returns empty. The only `.bbnf`
  under `skinny/` is `skinny/grammars/json.bbnf` (R1 §6). Regen mechanics
  (`skinny/xtask/src/regen.rs:19` `write_targets`→`codegen::emit_runtime_from_request`→
  `write_to_dir`; `check_target` fails-if-stale) are grammar-neutral and reusable.
- **CONSEQUENCE (a named gap for the gestalt):** SPEC §1 lists the "proven organs" that port
  into the greenfield (bbnf-simd, codegen emit spine, runtime tape, **xtask regen**, canon
  benches) — but the bbnf-in-bbnf self-host, the regex self-host, AND the fixpoint
  reproducibility gate live in the LEGACY `crates/` tree that GN-0 **archives** (SPEC GN-0:
  "everything else archives"). The greenfield inherits skinny's non-self-hosting xtask. So
  LOCK-7's clause-(i) surface is currently on the CHOPPING BLOCK, not the port manifest. The
  bootstrap must be RESCUED into the loom (GN-1) or the constitution's clause (i) is violated
  by construction. GN-0's "5 rescue checks" (SPEC GN-0) are the natural hook.

### 1.4 The type/host substrate for totality

- `@host <ident> : <type_name>` is in the meta-grammar (`bbnf.bbnf` host_directive) — the
  typed host-function seam (7C) already has syntax. `->` projections + `type_name`'s
  backend-agnostic `Named` escape are the declarative leaf-semantics channel (memory:
  `hybrid-grammar-host`, `backend-agnostic-types`). GN-5.W2 (TOML) is the live test of "leaf
  semantics via `->` where expressible; recursive/context-dependent via host" and even
  anticipates the trap: "a host function that hard-codes TOML datetime parsing is a name-gate
  in disguise; check-name-gates must audit host functions, or the datetime leaf stays
  grammar-expressed" (GN-5.md R2, `:519-524`).

---

## §2 CONSTRAINTS OUR EVIDENCE BASE IMPOSES

- **Clause (i) grammar totality** — everything grammar-driven save primitives and combinators;
  the regex grammar IS bootstrapped and bbnf-defined (PORTFOLIO CONSTITUTION i).
- **Clause (ii) true isomorphism** — regex HIR truly isomorphic to bbnf HIR/IR; LOCK-5 sets
  the lawful form, LOCK-7 consumes it (CONSTITUTION ii; SPEC §4 one-regex-home).
- **Clause (v) zero-cost type plane** — verified invariant; totality of the type/`->` plane
  must erase to zero runtime (SPEC §0 row 08, GN-3).
- **Clause (vi/viii) colocation + parsimony + no god modules + no name-gates + Y.13** — a
  grammar-specified IR or emitter-template plane (7B) must not become a consumer-less
  substrate, and must obey the generated-size budget (memory: `generated-size-budget`,
  `no-god-modules`).
- **Standing law — augment BBNF, never work around** (memory: `no-workarounds-self-hosting`,
  `hybrid-grammar-host`): if the self-host parser cannot express a surface, extend BBNF.
- **Clean-regen discipline + fixpoint** — generated files are always output of fresh regen;
  byte-identity across regen∘regen (memory: `clean-regen-discipline`,
  `changed-flag-convergence`; the reproducibility test §1.1).
- **Extant-tranche consciousness (ix)** — GN-5 already authored the totality-witness waves
  (TOML fifth grammar, host-fn-as-name-gate audit, check-name-gates AST gate). LOCK-7 re-cuts,
  never silently contradicts.

### Measured refutation directly binding on LOCK-7

- **SK-V18 (memory: skv18-prove-ir-gap):** the skinny IR/bbnf parser **REJECTED** the full bbnf
  surface — Sheets grammar `->` projections, `?w` whitespace-modifier, and `,` — and the
  adjudicated root cause was a **parser extension**, not a workaround. This is the self-host-
  subset trap made concrete AND the "augment BBNF" law in action: any promoted self-host
  parser must accept the FULL surface it emits (`->`, `?w`, `@host`, closures/GrammarCall), or
  it cannot parse the workspace's own grammars. A meta-grammar that parses a subset of itself
  is not a bootstrap.

---

## §3 PRIOR ART — WHAT WON AND LOST FOR THIS PROBLEM CLASS

### 3.1 Staged bootstrap + fixpoint (7A) — settled, decades-proven

- Classic sequence: stage-0 (in another language / committed artifact) → stage-1 (self-
  compiled) → stage-2, with the byte-identity correctness test that **stage-3 == stage-2**
  proves internal consistency (rustc dev-guide; Grokipedia "Bootstrapping (compilers)"). bbnf's
  reproducibility gate (§1.1) is precisely this, one stage compressed.
- **Minimal-surface / auditability lesson:** live-bootstrap's stage0 is 181 bytes of machine
  code — "a concrete trust anchor small enough for any programmer to audit by hand," reproduced
  to the same hash across wildly different toolchains (turnkey/StageX; codeberg stagex). The
  directly-transferable principle for 7A: **minimize the committed stage-0 and keep it
  auditable**; a large opaque generated bootstrap defeats the trust anchor.
- **Meta-circularity:** McCarthy's Lisp-in-Lisp (1960) and the Hart–Levin self-hosting Lisp
  compiler (1962) are the meta-circular origin; the pattern's discipline (the language written
  in itself) is exactly bbnf-in-bbnf.

### 3.2 Self-hosted grammar meta-grammars (7A/7B) — the wins and the recurring bug

- Guido van Rossum's PEG meta-grammar (CPython's PEG generator generates its own parser):
  the meta-grammar is self-hosted; notably the generator **propagates optionality upward via a
  fixpoint loop and wraps references with `optional()` at call sites** — the same fixpoint
  optionality/nullability discipline bbnf's optimizer runs, and a known correctness-critical
  loop (converge on a Changed flag, memory: `changed-flag-convergence`).
- tree-sitter: grammar authored in JS, parser generated in C, generator in Rust — a
  three-language split. The lesson for LOCK-6/7 isomorphism: tree-sitter did NOT self-host the
  grammar language in itself; it accepted a host-language DSL. bbnf's ambition (grammar IN
  grammar) is stronger and rarer.

### 3.3 Semantic actions vs declarative capture (7B/7C) — the decisive maintainability verdict

- **The trap, named repeatedly in the literature:** embedded host-language semantic actions
  "decrease the reusability of the grammar specification — Yacc cannot generate the Java
  version of a parser simply because Yacc uses C-based action code" (Nez, arXiv 1511.08307).
  Grammar+action becomes tightly coupled to one host language; multi-backend emission dies.
- **The winning alternative:** declarative AST-construction operators instead of arbitrary
  code — Nez's capture/tag/link, the "structured grammars" and PEG-AST work (arXiv 1205.1877,
  1507.08610), and L-attributed attribute grammars evaluated during top-down parse (WPI CS4533
  module 4). bbnf's `->` projections + backend-agnostic `TypeDesc::Named` ARE this declarative
  channel — the reason bbnf can target Rust AND TS from one grammar (LOCK-6). **The corollary
  for 7C:** typed host functions are the escape hatch, but every host function is a reusability
  leak; the "totality is a measured % behind grammars with a named floor per subsystem" charter
  is the right frame, and the floor must be *high* — GN-5 already forbids host functions that
  hard-code leaf parsing (§1.4). ManyDSL (arXiv 1612.03488) is the staged host-for-many-
  languages precedent.

### 3.4 Generated-grammar-as-oracle (7D) — validated, and a live 2020s pattern

- Differential/conformance testing where a generated or reference parser is the ORACLE against
  a faster hand impl is standard (Marmsoler–Brucker Solidity vs reference compiler; DBMS
  grammar-rule differential testing, arXiv 2503.03893). The explicitly current pattern: "a
  parser generator provides the oracle and then [a faster impl] is fuzzed with property-based
  testing to make them match" (posthog 70×-SQL-parser; arXiv 2503.03893). This is 7D
  (META-CIRCULAR-ORACLE: bbnf-defined grammars as differential oracles against hand parsers,
  promotion gated on measured parity+perf) almost verbatim — a de-risked, externally-validated
  route. The bbnf reproducibility test (§1.1) is already a degenerate meta-circular oracle.

---

## §4 TRAPS NAMED IN COMPARABLE SYSTEMS (the negative results to carry)

1. **Self-host subset trap (MEASURED, SK-V18).** The meta-grammar parser must accept the FULL
   surface it emits (`->`, `?w`, `@host`, closures). Skinny's parser rejected `->`/`?w`;
   root-cause-fix was mandated over workaround. A promoted self-host parser (7D) is gated on
   full-surface parity, subsystem by subsystem.
2. **Three divergent regex grammars (MEASURED, §1.2).** `grammar/misc/regex.bbnf` (`-`, `[ ]`,
   `{ }`) vs `parse-that/rust/bootstrap/regex.bbnf` (`>>`/`<<`) vs the inline test copy.
   One-regex-home (SPEC §4) demands one; LOCK-7 picks the canonical `.bbnf` + its home.
3. **Archival amputates the bootstrap (STRUCTURAL, §1.3).** bbnf/regex self-host + fixpoint
   gate live in legacy `crates/`, which GN-0 archives; skinny's ported xtask has no
   regen-bbnf/regen-regex. Clause (i) is violated by construction unless the loom (GN-1)
   rescues these organs. This is the single largest LOCK-7 gap the gestalt must close.
4. **Semantic-action host coupling (LITERATURE, §3.3).** Embedded host code kills multi-backend
   reuse (Yacc→no-Java). Every host function beyond declarative `->` is a leak; keep the
   grammar-behind-% floor high and audit host functions with check-name-gates (GN-5 R2).
5. **Fixpoint non-idempotency (GUARDED, §1.1).** Hash-randomized iteration, monotonic counters,
   cache pollution break byte-identity — exactly what the reproducibility test hunts. Any 7B
   grammar-specified plane (IR, emitter templates, oracle tables) inherits this obligation.
6. **Stage-0 bloat / lost trust anchor (LITERATURE, §3.1).** 7B's "generator becomes mostly
   data" risks a huge un-auditable committed stage-0 that blows the generated-size budget
   (memory: `generated-size-budget`). The 181-byte trust-anchor lesson: minimize + audit the
   committed bootstrap surface.
7. **Consumer-less grammar-specified plane (Y.13, clause viii).** Grammar-specifying the IR /
   emitter templates / oracle tables (7B) must land with a same-wave consumer, or it is
   forbidden substrate — the master pathology GN-5 §1 names (14/18 dead substrate).

---

## §5 OPEN QUESTIONS FOR SYNTHESIS (not answered here — research only)

- Where does the ONE bootstrapped regex `.bbnf` live, and is its bootstrap staged in parse-that
  (the regex home) or bbnf-lang? (LOCK-5 ⇄ LOCK-7 interlock.)
- Is the greenfield's clause-(i) surface a RESCUE of the legacy self-host organs into GN-1, or a
  re-authoring? SPEC §1's organ list omits them — a named supersession is owed.
- How far does 7B go: IR grammar-specified (LOCK-3 3C GRAMMAR-HOMOIOUSIA), emitter templates,
  oracle tables — each measured against build-time + generated-size budgets, each with a
  same-wave consumer.
- The typed-host-fn floor (7C): what measured %-behind-grammars per subsystem, and does
  check-name-gates extend to host functions (GN-5 R2 flags this as open)?

---

### Sources (web)

- Guido van Rossum, "A Meta-Grammar for PEG Parsers" (self-hosted PEG generator; fixpoint
  optionality propagation) — medium.com/@gvanrossum_83706.
- Nez: practical open grammar language, arXiv 1511.08307 (semantic-action reusability trap;
  declarative capture/tag/link).
- "Structured Grammars are Effective," arXiv 1205.1877; "Fast, Flexible, Declarative AST
  Construction with PEGs," arXiv 1507.08610; ManyDSL, arXiv 1612.03488.
- rustc dev-guide, "What Bootstrapping does" (stage2==stage3 byte-identity); Grokipedia
  "Bootstrapping (compilers)"; StageX / live-bootstrap 181-byte stage0 (turnkey.com,
  codeberg.org/stagex).
- Differential/oracle testing: "Parser Knows Best," arXiv 2503.03893; posthog "70× faster SQL
  parser" (generator-as-oracle + fuzzed fast impl).
- tree-sitter (grammar-in-JS, parser-in-C, generator-in-Rust) — Wikipedia / Grokipedia.
- Attribute grammars / L-attributed evaluation — WPI CS4533 module 4.
