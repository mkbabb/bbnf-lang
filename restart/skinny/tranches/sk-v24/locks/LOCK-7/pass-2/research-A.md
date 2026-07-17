# LOCK-7 pass-2 — Family 7A: STAGED-BOOTSTRAP (research)

The two-stage self-host in its strongest form: a committed stage-0 artifact, a fixpoint gate
that certifies `regen∘regen = regen`, and a minimal bootstrap surface whose auditability is the
whole product. Developed here against the frozen skinny substrate, not asserted from the legacy
generator.

## 0. The substrate, measured (skinny@HEAD)

- **No self-host exists on skinny.** `skinny/xtask/src/main.rs:40-52` dispatches only
  `regen-json`, `regen-sheets`, `regen-css`, `regen-real-typed` (+ their `check-*`). There is
  **no `regen-bbnf` target, no `check-bbnf`, no fixpoint driver, no committed stage-0 bbnf
  recognizer.** `grep -rn "fixpoint|regen-bbnf|bootstrap"` over `skinny/xtask` and
  `skinny/crates/codegen` returns nothing self-host-shaped. bbnf-on-bbnf is a greenfield build on
  this tree, not a rescue.
- **The grammar source is present and small:** `grammar/bbnf/bbnf.bbnf` = **89 lines**, ~36 rules
  + 7 directive rules. It uses constructs the skinny structural parser does not yet accept
  (below). It is a *bbnf* grammar authored in *bbnf* — the self-description already exists as
  data; only the pipeline to eat it is missing.
- **The hand parser floor:** `skinny/crates/grammar/src/lib.rs` = **2142 LOC** — the frontend
  facts scanner (`resolve_runtime_import_closure`, `:963`; `@{`/`?w` scanned at `:420`/`:432` in
  a *different* pass) plus the structural `GrammarIr` builder (`parse_seq:1361`/
  `parse_postfix:1404`/`parse_atom:1495`, `add_expr:1393`). This 2142 is the number a *total*
  self-host must displace and the ceiling a *building* seed drifts toward.
- **The emitter is a RECOGNIZER emitter, verified.** `general_descent/emit.rs:259-286` emits one
  `fn parse_<rule><'i>(bytes, cursor: &mut usize, tape: &mut TapeBuilder) -> Result<(),
  ParseError>` per rule (`emit.rs:269`). Leaves reach the tape as **Span offsets only** —
  `match_leaf` (`emit.rs:158`) "Records the leaf's start offset on the tape"; header states
  "typed leaves deferred to Span" (`general_descent/mod.rs:20-24`). It builds an **offset tape**,
  never a `GrammarIr`; there is no `add_expr`/`resolve_refs` on the emitted side. The general path
  is entered by `runtime_generator.rs:98 !has_rule("value")`; `bbnf.bbnf` has no `value` rule, so
  it shape-dispatches here by construction. Whether a *builder* augment over this recognizer
  round-trips is genuinely unseeded (§4).

## 1. The seed_LOC headline, re-derived — and the building-seed inflation

Stage-0 seed = what the fixpoint gate freezes and ships so the compiler parses bbnf without a
hand parser. Two seed shapes, wildly different LOC:

- **Recognizer seed (cheap, non-total).** Route `bbnf.bbnf` through the existing general emitter.
  Output ≈ one `parse_<rule>` recognizer per rule + fixed scaffolding + a lazy view. By analogy
  to the live sheets/general output and the 1206-LOC emitter (`general_descent/`: emit 452 +
  view 431 + regex_first 230 + mod 93), the **generated seed lands ~1.0–1.5 KLOC** of
  `generated.rs` — but it produces an **offset tape of bbnf spans, not a `GrammarIr`.** Nothing
  downstream reads a bbnf-span tape, so the compiler cannot consume it. Small precisely because it
  does no construction.
- **Building seed (total, expensive).** A seed the compiler can *use* must reconstruct
  `GrammarIr` — re-encode `add_expr` (append) and `resolve_refs` (cross-rule link, TreeFold).
  That construction logic is **host-bound, not `->`-expressible** (census: node values are pure
  `->`, but build mechanisms are host functions). So a building seed = recognizer seed **+ the
  ported construction fold**, and the fold is the bulk of the current 2142 hand LOC that is *not*
  byte-scanning. **The building seed therefore inflates toward the hand-parser LOC** — the
  directive's suspicion, confirmed by the emitter's own shape: it gives away the *scanning* half
  for free (grammar-derived) and gives away **none** of the *construction* half (it emits
  `()`-returning recognizers). Estimate: a building seed reclaims the ~600–900 LOC of
  `parse_atom`/`parse_seq`/literal-scan that is genuinely regex-classable, and re-authors the
  ~1200 LOC of IR construction as host code — net seed **~1.4–1.8 KLOC**: it does **not** shrink
  the auditable surface enough to beat the hand parser on auditability alone.

**Trade vs total self-host (7B).** Staged-bootstrap buys: (a) a committed, diffable stage-0 source
anyone can read without running the generator; (b) a fixpoint gate that is a *hard* CI artifact.
It does **not** buy grammar-totality of the *construction* semantics — those stay in the host
floor regardless. The honest 7A claim is **"the bbnf front-end's *recognition* is grammar-derived
and fixpoint-gated; its *construction* is a typed host floor"** — not "bbnf compiles bbnf
end-to-end." Overselling totality here is the born-RED trap.

**The `88s→0.5s` datum is a legacy transplant — verified non-resident.** No `88s`/`0.5s`/regen
timing datum exists anywhere in skinny (`grep` over `skinny/xtask`, `skinny/crates` = zero). The
only `88.5%` hits in the corpora are the sk-v21/22 `StructLayout→Layout` **rename regen-ownership**
percentage (`sk-v21/SPEC.md:242`, `sk-v22/SPEC.md:79`) — a different quantity. Any `88s→0.5s`
figure describes the *legacy* bbnf-lang bootstrap (a cargo build/LTO wall-clock, not a
`regen∘regen` measurement) and must be **dropped, not transplanted** — carrying it violates
constitution-vii (concrete artifacts on *this* substrate). Re-measurement, if wanted, is a fresh
`regen-bbnf` wall-clock on skinny@HEAD, born-RED, no legacy anchor.

## 2. The fixpoint SUBJECT — and the non-vacuity proof

Two candidate subjects for `regen∘regen = regen`:

- **(A) Generated `.rs` bytes.** Freeze `generated.rs`; assert the second emit is byte-identical
  to the committed one — the pattern `check-json`/`check-sheets` already implement
  (`xtask/src/regen.rs:36 check_target` fails if stale). Cheap, already-tooled.
- **(B) `GrammarIr` equivalence.** Parse `bbnf.bbnf` with the generated parser → `GrammarIr_1`;
  re-emit → re-parse → `GrammarIr_2`; assert `GrammarIr_1 ≡ GrammarIr_2` under the semantic
  relation (the 7D `Empty ≡ Concat([])` normalization). Expensive; needs the building seed first.

**Choice: (A) as the shipped gate, (B) as the born-RED promotion oracle — but (A) is vacuous
unless the emit path is input-sensitive.** The directive's warning is exact: a fixed-const emit
path stamps identical bytes for *any* input, so triple-identical certifies nothing. Skinny is at
real risk: the general path is heavily scaffold-driven (`emit.rs:80` "fixed recognizer
scaffolding") and much of `generated.rs` is template-constant (`json_templates/generated.rs` is
`include_str!`'d verbatim at `runtime_generator.rs:101`). **A byte-identity gate over a
mostly-constant file certifies the constant, not the derivation.**

**Non-vacuity protocol (the gate must be able to fail on a grammar edit).** Pair the fixpoint gate
with a **differential mutation probe**: perturb `bbnf.bbnf` (add a rule, rename a nonterminal, add
an alternation arm) and assert the emitted `.rs` bytes **change** in the derived region.
Concretely: (i) partition `generated.rs` into `scaffold` (template-constant) and `derived`
(per-rule `parse_<rule>` bodies, first-set alphabets, view enums); (ii) the fixpoint gate asserts
byte-identity over `regen∘regen`; (iii) a **companion falsification** asserts a 1-rule grammar
delta produces a *non-empty diff confined to `derived`*. A gate that passes both — stable under
re-emit, sensitive to grammar change — is non-vacuous. This mirrors the standing "prove the gate
can fail before trusting it to pass" discipline (planted-breach falsification is the constitution's
shape for hard gates). **Subject (A) with the mutation probe is the shippable 7A deliverable;
Subject (B) is the 7D-shared oracle and stays RED until a building seed lands.**

## 3. Legacy fixpoint-gate: rescue-and-port vs re-author, and the born-RED gate

The legacy bbnf-lang tree has a self-host lineage (`docs/tranches/AT/PROGRESS.md:53` "BBNF
self-hosting 25K lines, 106 functions"; `sk-v23/research/04-bbnf-features.md:107` `bbnf_parity.rs`
round-trips grammars). But it archives at GN-0 (XL-A) and its generator is the *legacy* building
emitter, not the skinny general-descent recognizer.

- **Rescue-and-port cost.** The legacy fixpoint machinery assumes the legacy emitter's building
  output (a rich AST). Porting it onto skinny ports the *assumption*, not the code — skinny emits
  recognizers, so the legacy gate has no building output to compare. Rescue = re-teach the legacy
  gate the skinny recognizer shape ≈ re-author. **Verdict: rescue is a mirage; the two generators
  are not shape-compatible.**
- **Re-author cost (favored).** The skinny gate is ~1 xtask subcommand: `regen-bbnf` +
  `check-bbnf` mirroring `SHEETS_TARGET` (`xtask/src/main.rs:200`) — a `BBNF_TARGET` const, a
  generated dir `crates/runtime/src/grammars/bbnf/`, source resolved against repo-root
  `grammar/bbnf/bbnf.bbnf` (the R1 §10 path trap). Est. **+40 LOC xtask + the generated seed** +
  the mutation-probe test (§2). Cheap and born-RED-gateable.

**The born-RED gate for the parser-augment.** Routing `bbnf.bbnf` through the emitter first
requires closing three bounded structural-parser gaps — the hand parser does not yet *accept* its
own grammar's syntax: (i) **`?w`→`->` attachment** — `parse_seq:1361` consumes `?w` standalone
(`:1366`) and `parse_postfix:1449-1453` bails on `?w` (returns the atom), so a projection *after*
`?w` (e.g. `big_comment = (...) ?w -> Span`, `bbnf.bbnf:17`) has no attachment site; (ii) **`@{}`
atom** — `parse_atom:1495` handles only `"`, `/`, `(`, ident-start (`:1498-1518`); `@{ rhs }`
(`bbnf.bbnf:33`) is unrecognized in the *structural* builder (the `@{` at `lib.rs:420` is the
facts scanner, a separate pass); (iii) **`|params|` closures** — `parse_atom` has no `|`-led arm,
no `parse_closure` in the structural builder (`bbnf.bbnf:51 closure = "|" identifier ... "|" ?w
rhs`). **Born-RED gate:** `cargo xtask regen-bbnf` first parses `bbnf.bbnf` with the hand parser
(the three augments land, RED until each construct round-trips through `parse_grammar`), then the
emitter runs, then `check-bbnf` fixpoint + mutation probe go green. Each augment carries its own
RED unit test (a minimal `.bbnf` exercising exactly that construct).

## 4. The shared empirical question — reported RED-unknown, with the cascade analysis

*Does authoring `-> Span`/`-> T` builder projections over bbnf's structural rules make the emitted
parser reparse the corpus into `GrammarIr`-equivalent structure (within the `add_expr`+
`resolve_refs` host budget) while `regen∘regen` stays byte-identical — or does it cascade, and
where?* **RED-unknown; unseeded; must be driven, not presumed.** Strongest *a priori* read:

- The emitter today Span-projects every leaf into an offset tape (`emit.rs:158`,
  `mod.rs:20-24`); recognizers return `()`. A `-> Span` projection over `identifier`/`literal`/
  `regex` (which `bbnf.bbnf:9,13,15` already declare) is the *natural* first augment — it asks the
  recognizer to record a typed span, which the tape already does. **Least likely to cascade.**
- Cascade risk concentrates at the **compound rules whose value is structural**, not Span:
  `term` (`bbnf.bbnf:29-36`, 8-way alternation), `factor`, `mapped_factor` (the `->` carrier
  itself, `:42`), `rhs = closure | alternation` (`:52`). Their value is a *constructed node*
  (`ExprKind::Seq`/`Alt`/`Project`/`Repeat`); the emitter has **no construction verb**. A `-> T`
  on a compound rule has nowhere to *build* T without growing an `add_expr`-equivalent. **Predicted
  cascade point: the first compound `-> T` that is not Span-reducible — `mapped_factor`/`rhs`.**
  That is exactly where the recognizer/builder divide bites, and exactly what a prototype must
  localize.
- `regen∘regen` byte-identity is *independent* of whether structure round-trips: bytes can be
  stable while `GrammarIr_1 ≢ GrammarIr_2` (a builder augment could emit
  deterministic-but-wrong construction). This is why Subject (B) (§2) is the real oracle and
  Subject (A) alone cannot certify the augment. **The prototype must measure both axes
  separately.**

## 5. Prior art (cited)

- **rustc:** bootstraps in numbered *stages* (stage0 download-beta → stage1 → stage2 self-built);
  the committed stage0 is a *binary* — auditability traded for reproducibility, the inverse of
  7A's committed-*source*-seed bias. **LALRPOP** (Rust) generates a parser from a grammar and
  commits the generated `.rs`, regenerated by `build.rs` — the exact `regen-bbnf` + `check-bbnf`
  byte-identity pattern skinny already uses for JSON.
- **tree-sitter:** `grammar.js` → generated `parser.c`, committed and CI-checked byte-identical
  (`tree-sitter generate` in CI) — the strongest industrial fixpoint-gate analog; their gate is
  over a **derived** `parser.c` (state tables vary with grammar), non-vacuous by construction —
  precisely the property §2 must engineer for skinny's scaffold-heavy output.
- **GNU Bison / Menhir:** self-describing generators whose own grammar is written in their own
  input language, bootstrapped from a committed prior generation — the "grammar authored in its
  own language" pattern `bbnf.bbnf` instantiates.
- **GHC (happy/alex):** grammar/lexer are `.y`/`.x`; generated Haskell is committed for the
  bootstrap compiler and regenerated by the built tools — a *building* seed (constructs a rich
  AST), which is why GHC's seed is large, corroborating §1's building-seed inflation.
- **LLVM TableGen:** `.td` → generated `.inc`, committed + regenerated; the "generator becomes
  mostly data" endpoint (7B's ambition) with the caveat that TableGen's *backends* stay hand-C++ —
  the host floor never fully vanishes, mirroring the construction-stays-host finding.

## 6. Named risks and counterexamples

- **R1 — Vacuous fixpoint.** The gate passes because `generated.rs` is template-constant, not
  because the derivation is stable. *Counterexample that must trip:* a 1-rule grammar delta must
  diff the `derived` region (§2 mutation probe). If it does not, the gate certifies the scaffold;
  demote to SPEC-ONLY.
- **R2 — Building-seed inflation defeats the auditability claim.** If the building seed lands at
  ~1.6 KLOC (§1), it is *not* smaller than the 2142-LOC hand parser's construction half, so 7A's
  "minimal surface, maximal auditability" headline is unearned. *Counterexample:* measure seed LOC
  after the augment; if it does not beat the displaced hand-parser LOC by ≥2×, the auditability
  claim is directional-only.
- **R3 — Recognizer/builder divide is unbridgeable without a kernel escape.** Compound-rule `-> T`
  construction (§4) may force an `add_expr`-equivalent that is not `->`-expressible — a host
  function — shrinking 7A's grammar-derivation to recognition only. *Counterexample to guard:* if
  `mapped_factor`/`rhs` cannot round-trip to `GrammarIr` via `->` alone, the construction fold is a
  **declared typed host floor** (hybrid-grammar-host law), not a workaround, and 7A's totality% is
  *recognition surface only* — named, not hidden.
- **R4 — The three augments themselves drift the hand parser toward the emitter.** Adding
  `@{}`/`|params|`/`?w→->` to `parse_atom`/`parse_seq` grows `grammar/src/lib.rs`; if the augment
  exceeds the seed it enables, staged-bootstrap is net-negative. *Counterexample:* budget the
  augment (est. +80–150 LOC across three arms) against the seed it unlocks; RED if the augment
  exceeds the seed.

## 7. Integration seams (path:line)

- `skinny/xtask/src/main.rs:40-52` (target dispatch — add `regen-bbnf`/`check-bbnf`), `:200`
  (`SHEETS_TARGET` template to mirror); `xtask/src/regen.rs:19,36`
  (`write_targets`/`check_target` — the byte-identity gate, already fixpoint-shaped).
- `skinny/crates/codegen/src/runtime_generator.rs:98` (`!has_rule("value")` — the shape gate
  `bbnf.bbnf` enters by construction), `:101` (template-constant `include_str!` — the vacuity risk
  surface).
- `skinny/crates/codegen/src/general_descent/emit.rs:259-286` (recognizer emission — the
  `()`-returning `parse_<rule>`), `:158` `match_leaf` (Span-only leaf record — the `-> Span`
  augment site), `mod.rs:20-24` ("typed leaves deferred to Span").
- `skinny/crates/grammar/src/lib.rs:1361` `parse_seq` (`?w` standalone — the `?w→->` gap), `:1404`
  `parse_postfix`/`:1481` `parse_projection` (the `->` handler), `:1495` `parse_atom` (missing
  `@{`/`|params|` arms), `:1393` `add_expr` (append), `resolve_refs` TreeFold (the cross-rule link
  — the host construction floor).
- Source: `grammar/bbnf/bbnf.bbnf` (89 LOC, the seed grammar); prior-art seed:
  `parse-that/rust/bootstrap/regex.bbnf` (72 LOC, READ-ONLY — the sibling committed-grammar
  pattern, self-hosted regex L4).

## 8. What a prototype must test to move 7A

1. **Land the three parser augments** (`?w→->`, `@{}`, `|params|`) as RED-gated units; assert
   `parse_grammar(bbnf.bbnf)` succeeds and round-trips to `GrammarIr` (hand parser accepts its own
   grammar). *Blocks everything else.*
2. **Wire `regen-bbnf`/`check-bbnf`**; capture the recognizer-seed LOC (Subject-A gate live).
3. **Run the mutation probe** (§2): assert `regen∘regen` byte-identical AND a 1-rule delta diffs
   only the `derived` region — the non-vacuity certificate.
4. **Author `-> Span` on the three leaf rules** (`identifier`/`literal`/`regex`); confirm the
   emitted recognizer records typed spans and `check-bbnf` stays green. *Least cascade risk.*
5. **Author `-> T` on the first compound rule** (`mapped_factor` or `rhs`) and drive the
   `!has_rule("value")` branch — **the decisive experiment.** Measure: (a) does the emitted parser
   reparse the bbnf corpus into `GrammarIr`-equivalent structure under Subject-B; (b) at which
   construct it cascades; (c) the host budget (`add_expr`+`resolve_refs`) the augment costs. Never
   touch `json_sink_direct`. Report the seed_LOC delta and the building-seed inflation number.
