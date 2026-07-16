# LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP — FAMILY SPECS (synthesis, pass 1)

Synthesis by Fable-tier judgment. One cogent spec per family while routes remain incompatible,
then the incompatibility map. Elegance earns nothing on its own; a family stalled at a missing
primitive equal in difficulty to the original problem is marked **BLOCKED-candidate**. All
`path:line` claims re-verified on disk at HEAD (2026-07-16).

---

## 0. Shared ground truth (re-verified) — the single fact that governs the lock

Four facts bind every family and were confirmed by inspection, not quotation:

1. **Skinny does NOT self-host.** `skinny/xtask/src/main.rs:40-52` dispatches only
   `regen-{json,sheets,css,real-typed}` + checks. No `regen-bbnf`, no `regen-regex`. The one
   `.bbnf` in the workspace metadata is `json`. The self-host organs live in the LEGACY
   `crates/` tree that GN-0 archives — clause (i) is on the chopping block, not the port
   manifest. This is the lock's largest structural gap (prior-art §1.3, trap 3).
2. **The self-grammars exist but are wired to nothing.** `grammar/bbnf/bbnf.bbnf` (3448 B) +
   `expressions.bbnf` (1378 B) + `types.bbnf` (301 B) are authored. `bbnf.bbnf` uses the full
   surface to describe itself: `@import` (line 4-5), `@{ }` host-capture (line 33), `@host …:
   type_name` (line 73), `?w`, `->`, grammar-function `call_arg`/closure. The recursive
   `term→factor→…→rhs` alternation tower is there.
3. **The generator entry is `codegen::emit_from_source(name, source)` (`codegen/src/lib.rs:101`)**
   / `emit_runtime_from_request` (`grammar_provider.rs:91`, takes the import source-closure).
   The lowering rides `general_descent/` (`emit.rs`, `regex_first.rs`, `view.rs`) + `lower/`
   (`event_tape.rs`, `offset_tape.rs`, `tape_plan.rs`, …). **Note:** codegen's "frontend
   closure" (`grammar_provider.rs:40 import_closure`) is the *import source-set* closure, NOT
   the bbnf grammar-function `|params| rhs` closure — the latter's lowering is unproven.
4. **Regex is half-self-hosted, as a recognizer only.** `/Users/mkbabb/Programming/parse-that/
   rust/parse_that/src/regex/generated.rs` (2293 LOC, committed, AUTO-GENERATED from
   `rust/bootstrap/regex.bbnf`) *produces* a `RegexParserEnum` AST internally
   (`RegexParser::regex() -> Parser<RegexParserEnum>`, ~line 2276) but the public
   `parse_generated(pattern) -> bool` (line 2288) **discards it**. Production regex is still the
   hand `bbnf_regex` engine. And a THIRD, divergent regex grammar sits at
   `grammar/misc/regex.bbnf` (1006 B) — the one-regex-home breach LOCK-5 owns and LOCK-7
   consumes.

**The governing finding.** 7A, 7B, and 7D are not three orthogonal architectural centers. They
are three postures on ONE bootstrap ladder, and they share ONE make-or-break gate:
*does `emit_from_source("bbnf", bbnf.bbnf)` produce a parser that reparses the grammar corpus?*
Until that gate is GREEN, the bbnf leg of all three is BLOCKED-candidate on the identical
missing primitive (a general-descent emitter that lowers bbnf's own closure/`@import`/`@{}`/
recursive-alternation surface). Only **7C is genuinely orthogonal** — it governs the horizontal
leaf/context boundary, not the vertical ladder, and it delivers in-tree wins that need no
generated parser at all.

---

## 1. Family 7A — STAGED-BOOTSTRAP

**Architecture (the center).** The smallest self-hostable unit — the grammar front-end parser —
minted as a committed stage-0 (`S₀`), guarded by a two-rung fixpoint law, with everything above
the parser (codegen kernel, IR algebra, semantic folds) fiated as permanent hand-written
**primitive**. The wager: a small auditable seed + a rigorous `regen∘regen = regen` beats a
sprawling total self-host on auditability and build cost, at the price of a fixed hand parser.
Prior art: rustc stage0→1→2 (stage1==stage2), GHC, Guix reduced-binary-seed, Wheeler DDC,
tree-sitter's "generator primitive, parser generated+committed" split.

**Mechanism.** Three artifacts, one law.
- *Seed floor S₋₁:* combinator/tape runtime primitives; the codegen kernel
  (`emit_from_source`); the semantic folds (`regex/host.rs lower`); the hand bbnf parser
  (`grammar/src/lib.rs:29 parse_grammar`, 2142 LOC — the SEED). `seed_LOC` is the headline
  metric, minimized and measured against a total self-host.
- *Stage-0:* two checked-in `@generated` parsers — a NEW `skinny/crates/runtime/src/grammars/
  bbnf/` (minted from `bbnf.bbnf`, mirroring the legacy `BbnfBootstrap` shape) and the extant
  regex `generated.rs` (adopted). Each header carries a provenance triple `{source path,
  blake3(source), blake3(emitted)}` committed to a `bootstrap.lock` ledger.
- *Fixpoint gate (the novum):* Rung 1 = staleness (`regen.rs:37 check_target` `check_dir`
  byte-compare, extant mechanism newly pointed at the self-grammar). Rung 2 = the stage ladder:
  parse `bbnf.bbnf` with the *generated* parser → emit `S₁`; assert `sha256(S₁)==sha256(S₀)`;
  emit `S₂` from `S₁`; assert `S₁==S₂` (a fixed point, not a 2-cycle — the rustc lesson).

**Integration seams.** `xtask/src/main.rs:40-52` (add `regen-bbnf`/`check-bbnf`/`regen-regex`/
`check-regex`/`fixpoint-bbnf`); `:177/:200` `JSON_TARGET`/`SHEETS_TARGET` (templates to
mirror); `regen.rs:37 check_target` (Rung 1 verbatim); `regen.rs:153 validate_inputs` (blake3 →
extend with emitted-digest); `codegen/src/lib.rs:101 emit_from_source` (mints S₀);
`grammar/src/lib.rs:29 parse_grammar` (seed, freeze/retire — the central decision).

**Gates it carries (born-RED).** G-7A-1 base-case emit (shared, §0); G-7A-2 single-step
fixpoint `S₁==S₀`; G-7A-3 no-2-cycle `S₁==S₂`; G-7A-4 determinism (reordered inputs +
second machine → byte-identical, BTreeMap-ordered emission, no build-relative paths in headers);
G-7A-5 regex promotion (parse-equivalence + perf non-regress vs hand `path_lexer`, else banked);
G-7A-6 build-budget (`fixpoint-bbnf` wall-time delta over `regen-json`).

**Exact open questions.** (Q1) Seed disposition: frozen-forever (R4-i, the conservative
default, Guix-style) vs retired-after-fixpoint with a `cfg(bootstrap)` two-commit dance for
syntax that crosses the seed line (R4-ii, rustc). (Q2) Twin ladders (R5): does one `fixpoint`
xtask span both bbnf (skinny/tape) and regex (parse-that/combinator) runtimes, or two
independent ladders — and does that double the "minimal surface"? (Q3) Regex promote-or-delete:
if perf blocks promotion the regex leg is oracle-only, which is 7D's center not 7A's.

**BLOCKED-candidate assessment.** Rung 1 (staleness) is trivially achievable — it is
json/css's existing mechanism. Rung 2 (the *distinguishing* value) is BLOCKED-candidate on the
shared §0 base-case gate: it cannot mint a self-reparsing S₀ until the emitter lowers bbnf's own
surface. **Verdict: NOT fully blocked (Rung 1 ships), but its self-hosting proof shares the
ladder's single feasibility wall.** Its frozen-seed default (Q1-i) is nearly identical to 7D's
reference parser — see incompatibility map.

---

## 2. Family 7B — TOTAL-SELF-HOST

**Architecture (the center).** Constitution clause (i) taken literally: every declarative plane
is grammar. `bbnf.bbnf` describes bbnf, `regex.bbnf` describes regex, a NEW `ir.bbnf` describes
the IR node algebra, and an emitter-template grammar describes structural emission. The
generator's Rust shrinks to two irreducible cores — the NEON kernels (`bbnf-simd`) and the
combinator primitives. The two ceilings that falsify totality plane-by-plane: **build-time**
and **debuggability**. Prior art: tree-sitter (generator = mostly data; warning: its data path
is *interpretive*, which 7B's specialized-SIMD thesis forbids), MLIR ODS/IRDL + LLVM TableGen
(generate IR *definitions*, hand-write passes — the verified data/logic boundary), Racket
(`#lang` + meta-circular expander, C runtime kernel = the primitive carve-out).

**Mechanism — three gaps beyond the extant.**
- *Gap A (front-end):* resurrect the bootstrap crate as the PRODUCTION front-end — route
  `parse_grammar` through the generated parser, retire the hand RD parser to at-most a
  differential oracle. This is the load-bearing promote-to-production move.
- *Gap B (IR grammar):* author `ir.bbnf` specifying `ExprKind` (`ir/src/lib.rs:234`),
  `BackendExpr` (`:376`); generate ONLY data definitions + serde + a visitor scaffold (the ODS
  discipline). `passes/` (2750 LOC) and `codegen/` stay hand-written against generated types; a
  diff-gate asserts the generated type surface is byte-identical to a frozen snapshot and that
  passes/codegen compile unchanged. Aligns with LOCK-3 `GRAMMAR-HOMOIOUSIA` without asserting it.
- *Gap C (emitter templates):* codegen today splices raw Rust *strings* (`runtime_generator.rs:
  101 include_str!`, `:339/:387/:505` inline `r#"…"#`, no `quote!`/`syn`). Gap C specifies the
  *structural* emission (parser skeleton, dispatch, tape assembly, control flow) as an
  emitter-template grammar; SIMD kernels stay opaque hand-Rust behind a typed call interface.

**Integration seams.** `grammar/src/lib.rs:29` (promote); `crates/bootstrap` +
`crates/core/src/grammar/generated/bbnf.rs` (legacy self-parse to resurrect into skinny);
`ir/src/lib.rs:234/:376` (grammar-specify); `codegen/src/runtime_generator.rs:101/:339/:387/
:505` (string codegen to replace); `xtask/src/main.rs:39-52` + `regen.rs:19 write_targets`
(the B6.W0 byte-identical-skip fixpoint to generalize across four planes).

**Gates it carries (born-RED).** G-7B-1 promote-the-front-end (differential parity over the full
corpus + diagnostic-quality delta on a malformed-grammar fixture set + cold-build delta; PASS =
parity + diagnostics ≥ hand + budget, ELSE Gap A demotes to oracle — which is 7D); G-7B-2 IR
grammar (generated type surface byte-identical to frozen snapshot, passes/codegen compile
unchanged); G-7B-3 emitter-template (regen JSON bit-for-bit AND no kernel escape-hatch leaks
into control flow — the R3 failure signature); G-7B-4 four-plane fixpoint + cold-regen ≤ stated
ceiling (the ~88s→~0.5s B6.W0 datum is the reference).

**Exact open questions.** (Q1) Diagnostics parity: `bbnf.bbnf` carries `-> Span` on every
terminal so span-precise errors are *expressible* — does the generated parser *emit* them as
richly as 2142 LOC of hand diagnostic machinery? (Q2) Can an emitter-template DSL express
Rust+NEON control flow *without* a type/borrow checker it structurally lacks (R3 category-error)?
(Q3) `regex.bbnf` is a recognizer, not a HIR-projector — Gap B/regex totality is
LOCK-5-dependent (must project `->` to the lawful HIR; a declared consumer edge, not a
workaround).

**BLOCKED-candidate assessment.** Gap A shares §0's base-case wall (BLOCKED-candidate until it
clears). Gap B is feasible — ODS/TableGen/tree-sitter are direct existence proofs, and it needs
no runtime parser, only the codegen path that already emits enums. **Gap C is a
BLOCKED-candidate on its own terms:** a template DSL that emits *correct specialized* Rust needs
a type/borrow discipline equal in difficulty to writing the codegen backend — the very problem
it replaces — UNLESS scoped strictly to pure structure with all typed logic behind kernel
primitive-calls. If that scoping holds, Gap C degrades to "structural templates + typed kernel
FFI" (feasible); if any kernel escape-hatch must reach into control flow, it is blocked.
**Verdict: partially blocked. Gap B ships; Gap A waits on §0; Gap C is BLOCKED-candidate absent
a proven pure-structure scoping.** 7B's honest form is "totality wherever build-time and
debuggability permit, every exception named and measured" — not totality asserted.

---

## 3. Family 7C — THE HOST-FUNCTION LATTICE

**Architecture (the center).** Totality is a *measured percentage* of behavioral surface inside
the grammar plane, bounded below by a **typed host-function interface** that is the ONLY legal
escape for context-dependent / recursive-with-state semantics. Three strata — GRAMMAR (rules,
`->`, the value-expr DSL), PRIMITIVES/COMBINATORS (pure referentially-transparent kernels — the
constitution's exempt floor; JSON proves it: `json/host.rs` is *empty*, number/string decode are
primitives not host fns), HOST FUNCTIONS (the typed, counted residue) — with a single typed seam
between strata 2 and 3. Attribute-grammar framing is exact: the `&mut Ctx` is an inherited
attribute, the return a synthesized one. Prior art: tree-sitter external scanners (the same
context residue, but *untyped* — 7C's advance is the typed signature + measured floor); ANTLR
predicates / Menhir actions (the multi-backend-killing coupling 7C avoids by keeping types
`Named`, only bodies per-backend); attribute grammars; MetaOCaml typed staging.

**Mechanism.** Extend `@host name` (dormant, `bbnf.bbnf:73`, usage across shipped grammars =
ZERO) to a full signature `@host name : InType -> OutType` or `(InType, &mut CtxType) ->
OutType`, types flowing from `types.bbnf type_name` into LOCK-1 inference as a typed leaf; the
*tier* (PURE-DECODE vs CONTEXT) is read off the presence of `&mut Ctx`. A mechanical counter
walks `GrammarIr` and partitions leaves × alternation-branches into {grammar, primitive, host};
**totality% = (grammar+primitive)/total** measured against *behavior* (leaves × branches), not
rule count. A totality-floor gate (sibling of GN-5.W3 `check-name-gates`) fails if a subsystem
exceeds its declared CONTEXT-host floor OR if any host fn's name/body is grammar-name-specific.

**Named floors (from grammar sources):** json 0, csv 0, toml table-path nesting
(`@host push_path : (Span, &mut TablePathStack) -> Unit`), css l4 the `@{}` capture, sheets
cross-cell resolution candidate, bbnf `@import` + closure β-reduction.

**Integration seams.** `bbnf.bbnf:73 host_directive` (extend to full signature); `bbnf.bbnf:33
@{}`; `grammar/src/lib.rs:170 RuntimeHostCapture`, `:222 BBNF-UNSUPPORTED-HOST-CAPTURE`
(productionize the lowering); `passes/src/lib.rs:794-798 has_parse_time_host_decode` (the string
heuristic `name.contains("host") && (value.contains("decode")||value.contains("parse"))` — the
one-file cure); `ir/src/cost.rs:255 HostFnParseTime`; `runtime/src/lib.rs:42-44` (zero-cost
payload no-write assertion).

**Gates it carries (born-RED).** T1 typed signature type-checks + zero-cost holds; T2 delete the
string heuristic → `check-json` bit-for-bit (JSON floor 0); T3 one real CONTEXT host fn (TOML
`push_path`) round-trips vs the `toml` crate AND a grammar-name-hardcoded variant is FLAGGED; T4
productionize `@{}` → CSS regen bit-for-bit + measured totality% delta; T5 the counter + floor
gate trips on a planted breach; T6 the typed `@host` directive survives `regen∘regen`
byte-identical on `bbnf.bbnf`.

**Exact open questions.** (Q1) The CSS floor is presently UNMET — `@{}` HostCapture is
`BBNF-UNSUPPORTED` at `grammar/src/lib.rs:222` yet `color.bbnf:220`/`values.bbnf:69` depend on
it; productionizing the lowering is the family's first obligation, not a paper number. (Q2) Does
the CONTEXT tier keep the type plane zero-cost (`&mut Ctx` must be a parse-time-only channel,
stacks discarded at parse end — re-verified per subsystem, not assumed).

**BLOCKED-candidate assessment.** **NOT blocked.** Every win is concrete and in-tree and needs
no generated parser: delete one string heuristic (`passes:798`), light one dormant already-typed
directive (`bbnf.bbnf:73`), lower one already-designed construct (`@{}`). The only missing piece
(HostCapture lowering) is ordinary codegen work, not a primitive equal to the bootstrap problem.
This is the most independently-viable family in the lock, and it is **orthogonal** to the ladder
(§4).

---

## 4. Family 7D — META-CIRCULAR-ORACLE

**Architecture (the center).** Self-hosting is *verification first*. The bbnf-defined and
regex-defined grammars are compiled by the one generator into parsers that serve as
**differential oracles** against the hand parsers, not immediate replacements. Totality is a
promotion ledger, subsystem by subsystem, each edge born-RED at ORACLE-ONLY and promoted only on
`parity=1.0` (frozen corpus) AND `Perf ≥ 1.0` (cold PMU, SPEC §4 protocol). A
perpetually-verification-only edge is a **lawful terminal state**, not a failure, provided it is
named. This de-risks 7B's ambition: the oracle delivers its full value — a checkable
specification of the hand parser — the instant it parses the corpus, independent of any cutover.
The fixpoint corollary: once bbnf is PROMOTED, 7A's `regen∘regen` gate becomes a special case of
this family (7D subsumes 7A's gate). Prior art: Wheeler DDC (Hand = the "second compiler"),
rustc stage1==stage2, rust-analyzer `ungrammar` (grammar as checkable spec of the hand parser),
McKeeman/CSmith differential testing, grammar-based fuzzing, the current posthog/arXiv 2503.03893
"generator-as-oracle then fuzz the fast impl to match" pattern.

**Mechanism.** Three artifacts per subsystem S: (1) `Gen_S` via `emit_from_source`; (2) an oracle
harness over corpus `C_S` = hand-authored corpus ∪ grammar-derived fuzzing, asserting
`⟦Hand_S(x)⟧ ≡ ⟦Gen_S(x)⟧` under a DECLARED equivalence relation ≡, reporting `parity(S)` + a
divergence ledger; (3) a cold-PMU perf gate. The promotion ledger `{parity%, perf-ratio,
equivalence-relation-id, decision∈{ORACLE-ONLY, PROMOTED}}` is the spine.

**Integration seams.** `parse_that/src/regex/generated.rs:2288 parse_generated` (surface the
producer `RegexParser::regex()`, ~line 2276, instead of the bool recognizer — the first
prototype); `rust/bootstrap/benches/regex_parse.rs:12-60 PATTERNS` (seed corpus, upgrade from
divan to PMU-paired); `grammar/src/lib.rs:29-34 parse_grammar` (Hand side; note it runs
`resolve_refs()`+`validate()` AFTER the pure parse — the equivalence pivot); `xtask/src/main.rs:
39-52` (add `regen-bbnf`/`check-bbnf`/`oracle-bbnf`); `codegen/src/general_descent/` (the
base-case lowering).

**Gates it carries (born-RED).** G-7D-1 base-case feasibility (shared §0 — does `bbnf.bbnf`
lower, and at which byte does it reject?); G-7D-2 the differential oracle with ≡ pinned at the
pre-semantic AST + first ledger row; G-7D-3 regex recognizer→producer + PMU (the first COMPLETE
ledger row — the only edge with committed code both sides); G-7D-4 grammar-driven corpus sampler
driving parity → 1.0.

**Exact open questions.** (Q1) The equivalence relation ≡ (R1, blocks the family if
unspecified): `Hand_bbnf` = parse *then* `resolve_refs`+`validate`; ≡ MUST be pinned at the
pre-semantic AST (run the shared semantic passes on both, or neither) or `Gen` "diverges" only
for lacking ref-resolution — a false negative that poisons parity%. (Q2) Recognizer ≠ producer
(R5): today's `parse_generated -> bool` catches only membership divergence; a structural bug
that still *accepts* is invisible — the oracle DEMANDS the AST producer. (Q3) Two divergent regex
grammars: the oracle is undefined until LOCK-5 picks the canonical `.bbnf`; consume LOCK-5, name
the dependency. (Q4) Promotion may never fire (R4) — and that must be reported, not hidden.

**BLOCKED-candidate assessment.** The bbnf edge shares §0's base-case wall
(BLOCKED-candidate for that edge). BUT the **regex edge is unblocked and cheapest**: the producer
AST already exists on both sides, so the first COMPLETE ledger row can be built today without the
base-case gate. And 7D's honest terminal (ORACLE-ONLY) means even a slow generated parser
delivers value. **Verdict: NOT blocked as a family — the regex edge ships now; the bbnf edge
waits on §0 but can reach ORACLE-ONLY the instant the base case lowers (a weaker bar than 7A/7B's
promote-to-production).** This is the lowest-risk ladder posture.

---

## 5. THE INCOMPATIBILITY MAP

The core structural finding: **7A, 7B, 7D are three postures on ONE bootstrap ladder** (they
share the §0 base-case gate and the fixpoint mechanism); **7C is the one orthogonal family** (the
horizontal leaf boundary). They are placed across two axes:

- **SCOPE axis** (how far up the tower is grammar-specified): 7A (parser only; kernel/IR/emitter
  fiated PRIMITIVE) ⟷ 7B (parser + IR grammar + emitter templates).
- **POSTURE axis** (default disposition of the hand parser / direction of promotion): 7A
  frozen-seed-or-bank · 7B promote-to-production-then-measure-to-demote · 7D
  oracle-first-promote-on-measure.

### Incompatibilities (precise)

**INC-1 · 7A ⊗ 7B — SCOPE contradiction on the same files.** 7A fiats the codegen kernel, the IR
node algebra (`ir/src/lib.rs`), and the emitter templates as *permanent hand-written primitives*
— to keep the bootstrap surface minimal and one-directional. 7B *grammar-specifies exactly those*
(`ir.bbnf` generating `ExprKind`/`BackendExpr`; an emitter-template grammar replacing the string
codegen). `ir/src/lib.rs:234` cannot simultaneously be a frozen hand-authored primitive (7A) and
a generated artifact of `ir.bbnf` (7B). **Mutually exclusive as the family of record over the
up-tower artifacts.** (They agree only on the shared parser bootstrap: 7B's Gap A ⊇ 7A's whole
thesis.)

**INC-2 · 7B ⊗ 7D — POSTURE polarity, opposite born-RED default.** 7B resurrects the bootstrap
crate AS production (Gap A: route `parse_grammar` through the generated parser, retire the hand
RD parser) — the gate is born PROMOTED and must *demote* on a diagnostics/build loss. 7D keeps
the hand parser as reference and the generated parser ORACLE-ONLY until it *measurably wins*
per-subsystem — the gate is born ORACLE-ONLY and must *earn* promotion on parity+perf. The
default disposition of `grammar/src/lib.rs:29 parse_grammar` is contradictory (production-retired
vs reference-kept). Same endpoints, opposite direction of travel. **Cannot both be the governing
posture.** (Convergence note: 7B-R1's own concession "if diagnostics unmet, Gap A demotes to
oracle" is *exactly* 7D — the families collapse toward 7D under measured pressure.)

**INC-3 · 7A(retire-seed) ⊗ 7D — only the aggressive 7A variant conflicts.** 7A's *retirement*
variant (R4-ii: delete the hand parser after fixpoint, `cfg(bootstrap)` two-commit dance for
cross-seed syntax) contradicts 7D's keep-hand-parser-as-reference-forever. **But 7A's DEFAULT
(R4-i frozen-seed) is compatible with — indeed nearly identical to — 7D's reference parser.** The
conflict is narrow and variant-specific; 7A-default ⊆ 7D + a promotion ledger.

**INC-4 · 7C is orthogonal to all three — NOT a competing route.** 7C governs the horizontal
leaf/context boundary (typed `@host` lattice, the `passes:798` string-heuristic deletion, `@{}`
HostCapture productionization, per-subsystem totality%), which is independent of how far up the
tower the ladder self-hosts. It is a **shared mechanism the winning ladder family consumes**:
7B's "kernels behind a typed call interface" IS 7C's stratum-2↔3 seam; 7A and 7D inherit 7C's T6
fixpoint obligation on the `@host` directive; 7C's totality% is the *metric* every ladder family
reports against. **Compatible with each of 7A/7B/7D; competes with none.**

### The lawful gestalt shape (for the record, not a verdict)

The families do not sum to four independent tranches. The lawful composition is: **7C as a
cross-cutting mechanism (ships regardless)**, plus ONE chosen ladder posture on the (SCOPE ×
POSTURE) plane. 7D's oracle-first posture is the lowest-risk entry (ORACLE-ONLY is a lawful
terminal); 7A's frozen-seed is its near-twin plus a fixpoint gate; 7B is the aggressive-promotion
superset that additionally reaches Gap B (IR grammar, feasible) and Gap C (emitter grammar,
BLOCKED-candidate absent pure-structure scoping). The next pass's decisive question is not
"which family" but "how far up the SCOPE axis is feasible, and does promotion ever fire on the
POSTURE axis" — the two prototype briefs below target exactly those two hinges.

---

## 6. TWO SHARPEST UNCERTAINTIES → PROTOTYPE BRIEFS

The two hinges that most advance pass 2: (1) the shared §0 base-case wall that blocks the bbnf
leg of 7A/7B/7D simultaneously; (2) the promotion-vs-oracle-only posture, discriminable TODAY on
the one edge with committed producer code on both sides. Full briefs returned via
StructuredOutput.
