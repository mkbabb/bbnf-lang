# LOCK-7 family 7D — META-CIRCULAR-ORACLE (research, pass 1)

**The stance.** Self-hosting is verification first. The bbnf-defined bbnf grammar and the
bbnf-defined regex grammar are compiled by the one generator into parsers that serve as
**differential oracles** against the hand-written parsers, not as immediate replacements.
Grammar totality (constitution i) is earned as a *measured property* — a promotion ledger,
subsystem by subsystem, each edge gated on AST-parity plus cold-PMU performance under the
standing profiling law. The hand parser stays the reference and the fallback until the
generated one measurably wins on its own subsystem; totality is the union of promoted edges,
reported as a percentage with a named floor, never asserted. This de-risks 7B's "generator
becomes mostly data" ambition: the oracle delivers its full value — the grammar is a
*checkable specification of the hand parser* — the instant it parses the corpus, long before
(and independently of) any production cutover. A perpetually-verification-only edge is a
lawful terminal state here, not a failure, provided it is named as such.

## 1 — The mechanism made concrete

Three artifacts per self-hosting subsystem S (S ∈ {bbnf-grammar, regex, …}):

1. **The generated parser** `Gen_S`, emitted from the `.bbnf` source through the *same*
   `codegen::emit_from_source` path that emits json/css/sheets (no bespoke proc-macro — the
   `bbnf_derive` crate was retired at bbnf-lang `6142387f`, generation now flows through
   `cargo xtask regen`; parse-that `rust/bootstrap/src/lib.rs:11-22` records this contract).

2. **The differential oracle harness** `Oracle_S`: over a shared corpus C_S, run the hand
   parser `Hand_S` and `Gen_S`, assert `⟦Hand_S(x)⟧ ≡ ⟦Gen_S(x)⟧` under a **declared
   equivalence relation ≡** (§4 risk 1 makes this precise). Report `parity(S) = |{x∈C_S :
   Hand≡Gen}| / |C_S|` plus a **divergence ledger** (every `x` where they disagree, with both
   trees). C_S is closed under two generators: (a) the extant hand-authored corpus — every
   `.bbnf` under `grammar/` for S=bbnf-grammar; the `PATTERNS` suite at
   `parse-that/rust/bootstrap/benches/regex_parse.rs:12-60` for S=regex — and (b)
   grammar-derived fuzzing (§3 prior art): sample strings from the grammar's own production
   rules, feeding both parsers, driving parity toward saturation.

3. **The performance gate** `Perf_S`: cold per-parse PMU cyc/B, paired interleaving, N≥200,
   multi-window, per SPEC §4 / §0 protocol — `Gen_S` vs `Hand_S`. The extant divan bench
   (`regex_parse.rs`) is the *seed* harness and must be upgraded from divan wall-clock to the
   PMU-paired protocol before it can gate.

**The promotion ledger** (the family's spine): a committed table, one row per subsystem,
columns `{parity%, perf-ratio, equivalence-relation-id, decision∈{ORACLE-ONLY, PROMOTED}}`.
An edge is born-RED at ORACLE-ONLY. It promotes to PROMOTED only when `parity(S)=1.0` on the
frozen corpus **and** `Perf_S ≥ 1.0` within noise. On PROMOTION the hand parser is deleted (or
demoted to a differential-test fixture) and `Gen_S` becomes the production path. On a
perpetual perf loss the edge stays ORACLE-ONLY *by decision*, the divergence ledger stays
empty (parity held), and the verification value is banked — this outcome is reported, not
hidden.

**The fixpoint corollary.** Once S=bbnf-grammar is PROMOTED, 7A's fixpoint gate
(`regen∘regen = regen`, byte-identical) becomes a *special case* of this family: `Gen_bbnf`
re-parses `bbnf.bbnf` to regenerate itself, and byte-identity is the reproducible-build
witness (§3, Wheeler DDC). 7D subsumes 7A's gate rather than competing with it.

## 2 — The substrate (what exists, path:line)

**Regex — the furthest-advanced edge; the oracle is HALF-BUILT and committed.**
- Generated parser: `parse-that/rust/parse_that/src/regex/generated.rs` (2293 LOC, committed,
  `// AUTO-GENERATED from bootstrap/regex.bbnf`). Internally it *produces* an AST —
  `Vec<RegexParserEnum<'a>>` (`:175`), per-rule fns `__quantifier`/`__alternation`/`__concat`/
  `__regex` returning `Option<RegexParserEnum>` (`:215,390,434,2276`).
- **The gap that defines the first prototype:** the public entry
  `parse_generated(pattern:&str) -> bool` (`:2288`) is a **recognizer** — it *discards* the
  produced AST and returns accept/reject. A recognizer is a strictly weaker oracle than a
  producer (it cannot catch structural divergence, only membership divergence). The internal
  producer already exists (`RegexParser::regex()`, consumed at `regex/mod.rs:40-41`); the
  oracle must surface it.
- Seed grammar: `parse-that/rust/bootstrap/regex.bbnf`; seed bench:
  `parse-that/rust/bootstrap/benches/regex_parse.rs` (divan, hand-vs-generated, PATTERNS
  corpus); tests `bootstrap/src/lib.rs:26-60` already assert `parse_generated` acceptance.
- Hand regex parser: the parse-that `regex/` surface (`regex/mod.rs`); a *second* regex
  grammar exists in bbnf-lang at `grammar/misc/regex.bbnf` (1006 B) — **the two diverge**
  (§4 risk 3; one-regex-home law, SPEC §4; LOCK-5 owns the reconciliation, 7D consumes it).

**bbnf-grammar — the source exists, the generated edge does NOT.**
- Self-host grammar: `grammar/bbnf/bbnf.bbnf` (3448 B, real: directives `@import`/`@pretty`/
  `@ws`/`@host`, closures `|params| rhs`, host blocks `@{…}`, `->`/`?w` throughout) with
  `@import`ed `grammar/bbnf/expressions.bbnf` (value-expr sub-language) and
  `grammar/bbnf/types.bbnf`.
- Hand parser (the reference side of the oracle): `skinny/crates/grammar/src/lib.rs:29`
  `parse_grammar(name, source) -> Result<GrammarIr, GrammarError>` — 2142 LOC; note it runs
  `resolve_refs()` + `validate()` *after* the pure parse (`lib.rs:31-34`), which the grammar
  cannot express (§4 risk 1).
- **Missing:** no `regen-bbnf`/`check-bbnf`/`oracle-bbnf` in xtask. Dispatch is
  `skinny/xtask/src/main.rs:39-52` (json/sheets/css/real-typed/conformance/checkasm only);
  mechanics `xtask/src/regen.rs:19` `write_targets`, `:36` `check_target`;
  `RuntimeTarget` shape at `main.rs:177-206`. A new target + a new `oracle-*` differential
  command is the concrete xtask work.
- The general-descent emitter that would lower bbnf.bbnf: `codegen/src/general_descent/`
  (SK-V18 built it for `->`/`?w`/`,`/`<<`; project memory `skv18-prove-ir-gap`). It is the
  base-case feasibility question (§4 risk 2).

**Prior meta-circular precedent already lived here and was partly retired.** Both repos carry
a `crates/bootstrap` mirror whose one line re-exports the xtask-emitted self-parser
(`bootstrap/src/lib.rs:19-21` documents bbnf-lang's `pub use ::bbnf::grammar::generated::
BbnfBootstrap`). `BbnfBootstrap` is *referenced by contract but absent from skinny* — the
edge was designed, the proc-macro host retired, and the xtask regen edge never wired. 7D's
first act is to complete that wiring as an *oracle*, not (yet) as production.

## 3 — Prior art with citations

- **Diverse Double-Compilation (D. A. Wheeler, PhD thesis 2009; ACSAC 2005, "Countering
  Trusting Trust through Diverse Double-Compiling").** The canonical theory: compile a
  self-hosting compiler with a *second, independent* implementation and check for identical
  output. 7D is DDC specialized to parsers — `Hand_S` is the "second compiler," `Gen_S` the
  self-hosted one, and byte/AST-identity is the trust witness. Directly grounds the fixpoint
  corollary and answers "why trust the generated parser."
- **rustc bootstrap fixed-point (stage0→stage1→stage2).** stage1 and stage2 must produce
  *identical* artifacts — the reproducible fixpoint 7A gates on and 7D subsumes. rust-lang's
  `ungrammar` (rust-analyzer's machine-readable Rust syntax) is precisely a "grammar as
  checkable spec of the hand parser" — the same oracle stance, applied to Rust's own syntax.
- **Differential/N-version parser testing.** McKeeman, "Differential Testing for Software"
  (Digital Tech. Journal, 1998); CSmith (Yang, Chen, Eide, Regehr, PLDI 2011) — random
  programs fed to N implementations, disagreement = bug. 7D's oracle harness is differential
  testing with N=2 and a *grammar-derived* (not random-C) generator.
- **Grammar-based test generation / fuzzing.** Zeller et al., *The Fuzzing Book* (grammar
  fuzzing chapter); `grammarinator` (ANTLR); tree-sitter's grammar-driven corpus fuzzing.
  These supply corpus generator (b): sampling productions from `bbnf.bbnf`/`regex.bbnf`
  themselves to drive `parity → 1.0`. tree-sitter is the closest industrial analogue — a
  grammar compiled to a parser, differentially tested against reference parsers per language.
- **Metamorphic testing** (Chen et al.) — relations like "reformatting a `.bbnf` must not
  change its AST" give oracle assertions where a golden AST is unavailable.
- **Reflective towers / metacircularity** (Reynolds, "Definitional Interpreters," 1972;
  Thompson, "Reflections on Trusting Trust," 1984) — the theoretical frame for why a
  self-describing grammar is both the artifact and its own specification.

## 4 — Named risks and counterexamples

1. **Equivalence is not obvious (blocks the whole family if unspecified).** `Hand_bbnf` =
   parse *then* `resolve_refs()`+`validate()` (`grammar/src/lib.rs:31-33`) — semantic passes
   the grammar cannot emit. ≡ MUST be pinned at the **pre-semantic AST** (`Gen` produces the
   raw tree; the harness runs the *shared* `resolve_refs`/`validate` on both, or on neither).
   Counterexample if botched: `Gen` "diverges" only because it lacks ref-resolution — a false
   negative that poisons parity%. The equivalence-relation-id column of the ledger exists to
   make this auditable.
2. **The base case may not lower.** `bbnf.bbnf` uses `@import` cross-file resolution, closures
   `|p| rhs` (grammar functions), `@{ rhs }` host blocks, and 7 directive forms. SK-V18
   extended the frontend only to `->`/`?w`/`,`/`<<`. If `codegen::emit_from_source` cannot
   lower `bbnf.bbnf`, meta-circularity has no base case and S=bbnf-grammar cannot even reach
   ORACLE-ONLY. This is the make-or-break test. Standing law (self-host uses language
   features; augment BBNF, never work around) means the resolution is *frontend extension*,
   not corpus-trimming.
3. **Two regex grammars, one-regex-home law.** `parse-that/rust/bootstrap/regex.bbnf` (the
   committed, generated one) vs bbnf-lang `grammar/misc/regex.bbnf` — they differ in surface
   (e.g. lookbehind, `end = "/" >> flags`). 7D cannot pick the canonical home (LOCK-5's
   verdict), but its oracle is undefined until one source is authoritative. Consume LOCK-5;
   name the dependency.
4. **Promotion may never fire (and that must be honest).** A generated recursive-descent
   parser over a general IR can be persistently slower than a hand parser. `regex_parse.rs`
   exists precisely to measure this and may report `Gen_regex/Hand_regex > 1.0` forever. 7D's
   honest floor: ORACLE-ONLY is a *lawful terminal*, the verification value is real, and the
   ledger says so — it does not pretend totality where perf refused it.
5. **Recognizer ≠ producer.** Today `parse_generated -> bool` (`generated.rs:2288`). A bool
   oracle catches only accept/reject divergence; a structural bug (wrong precedence,
   mis-nested group) that still *accepts* is invisible. The oracle demands the AST-producing
   surface (already present internally) — shipping the recognizer as "the oracle" is the
   subtle self-deception to guard against.

## 5 — What a prototype must test to move this family

1. **Base-case feasibility (highest-value, make-or-break):** wire `regen-bbnf`+`check-bbnf`
   into `xtask/src/main.rs`; attempt `codegen::emit_from_source("bbnf", read(bbnf.bbnf))`.
   Binary outcome: does `bbnf.bbnf` (with its `@import`/closure/`@{}`/directive surface) lower
   to a working `BbnfBootstrap` parser through the general-descent path, or does the frontend
   reject it (and at which byte)? This decides whether S=bbnf-grammar reaches ORACLE-ONLY at
   all, and scopes the exact frontend extension owed.
2. **The differential oracle, defined:** implement `Oracle_bbnf` running `parse_grammar` and
   `BbnfBootstrap` over every `.bbnf` in `grammar/`, with ≡ pinned per risk 1; emit
   `parity%` + a divergence ledger. Deliverable = the first ledger row with a real number.
3. **Regex: promote recognizer→producer oracle + PMU bench.** Surface the AST from
   `RegexParser::regex()` (not `parse_generated`'s bool), assert `Hand ≡ Gen` structurally
   over PATTERNS + grammar-fuzzed inputs; upgrade `regex_parse.rs` from divan to the SPEC-§4
   PMU-paired protocol; report `parity(regex)` and `Perf_regex`. This is the one edge with
   committed code on both sides — it yields the family's first *complete* ledger row fastest.
4. **Corpus generator (b):** a grammar-driven sampler over `bbnf.bbnf`/`regex.bbnf`
   productions feeding both parsers; measure how fast parity saturates and whether fuzzing
   surfaces divergences the hand corpus misses (the real test of oracle strength).

## 6 — Consumer-in-wave (Y.13) compliance

No consumer-less substrate: the oracle harness *is* consumed by CI (differential test gate),
the promotion ledger is read by the regen gate, and each perf bench feeds the promotion
decision in the same wave. The generated parsers are not dead substrate — where not yet
PROMOTED they are live differential-test fixtures; where PROMOTED they are the production
parse path. Every edge names its reader before it lands.
