# LOCK-7 pass-2 · Family 7A — STAGED-BOOTSTRAP (research)

The classic two-stage self-host applied to skinny: a committed stage-0 artifact, a
fixpoint gate that certifies `regen∘regen = regen`, minimal bootstrap surface, maximal
auditability. This report develops 7A to its strongest form against the skinny substrate
as it stands at HEAD, re-derives the seed headline, chooses the fixpoint subject, costs
the legacy gate as rescue-vs-reauthor, and names the born-RED gate for the parser augment.
Every claim is anchored `path:line`.

---

## 1. The substrate as it actually stands (the honest floor)

Skinny has **no bbnf self-host and no regex self-host today.** The skinny xtask dispatch
(`skinny/xtask/src/main.rs:40-52`) exposes exactly: `regen-json`, `regen-sheets`,
`regen-css`, `regen-real-typed`, their `check-*` siblings, `lint-loc`, `bench-json`,
`gate-json`, `primitive-checkasm`. There is **no `regen-bbnf`, no `regen-regex`, no
`bootstrap`, no `fixpoint` arm anywhere under `skinny/`** (grep across the tree returns
zero self-host regen). The bbnf front-end that compiles every skinny grammar is the
**hand parser** at `skinny/crates/grammar/src/lib.rs` — 2142 LOC, producing `GrammarIr`
directly via `add_expr` (`ir/src/lib.rs:51`) and `resolve_refs` (`ir/src/lib.rs:84`). It
is not itself generated. Stage-0, in the 7A sense, does not exist on skinny; it must be
authored.

The **legacy** repo (outer `bbnf-lang`, the GN-0 archive substrate) DID have the full
staged bootstrap. Its xtask carries `RegenBbnf` → `crates/core/src/runtime/bbnf/`
(`xtask/src/main.rs:71-73`), and the fixpoint gate lives at
`crates/core/tests/bbnf_bootstrap_reproducibility.rs`. That gate is the transplant
candidate 7A must cost. Its own doc-comment states the discipline precisely: "regenerate
the BBNF grammar twice in succession into a fresh tempdir and assert the two outputs are
byte-identical … the bootstrap parser that compiles every grammar is itself produced by
the bootstrap parser" — the AZ-II.cutover Stage A / Stage B property.

**The three routing/parser facts that gate everything below:**

1. `bbnf.bbnf` exists at `grammar/bbnf/bbnf.bbnf` (89 LOC + `expressions.bbnf` 37 +
   `types.bbnf` 7 = 133 LOC across three `@import`-linked modules). It has **no `value`
   rule**, so under the shape dispatch at `codegen/src/runtime_generator.rs:98`
   (`if !sink_only.has_rule("value")`) it routes to `emit_general_descent` — the same
   grammar-neutral recursive-descent path Sheets uses. It never touches `json_sink_direct`.

2. The general-descent emitter (`codegen/src/general_descent/`, 1206 LOC) emits **one
   `parse_<rule>` recognizer per rule** with signature
   `fn parse_<rule>(bytes, cursor, tape) -> Result<(), ParseError>` (`emit.rs:259-286`).
   These are **recognizers that write tape offset-marks**, not `GrammarIr` builders. Its
   own header (`mod.rs:1-25`) declares regex leaves are matched by a delimiter-scan
   fallback and `Span`-projected — "typed leaves deferred to `Span`". A `DirectBuild`
   projection "records" to the tape (`emit.rs:385`); it does not construct an `Expr` tower.

3. `bbnf.bbnf` **cannot be parsed by the current skinny hand parser** — three bounded
   gaps stand: `parse_atom` (`grammar/src/lib.rs:1495-1529`) handles only `"`, `/`, `(`,
   ident — it has **no `@{`, `[`, `{` atom branches** (bbnf.bbnf uses `@{ rhs }`, `[ rhs ]`,
   `{ rhs }`); `parse_seq`/`parse_postfix` (`:1361`,`:1404`) treat `?w` as a standalone
   optional-whitespace element (`:1449-1453`) but do not attach it across a following
   `->` (bbnf.bbnf writes `big_comment ?w -> Span`); and `|params|` rule-level **closures**
   (`closure = "|" , identifier … "|" ?w , rhs`) have no parser production at all. The
   `@{` at `grammar/src/lib.rs:420` is inside the import/directive *scan* path, not
   `parse_atom` — it does not make `@{` a first-class term.

---

## 2. Re-deriving the seed_LOC headline on skinny (and the inflation trade)

The 7A headline "minimal bootstrap surface" needs a real number on skinny, and the
critical distinction is **recognizer-seed vs building-seed.**

**Recognizer-seed (what general-descent gives for free).** Route `bbnf.bbnf` through
`emit_general_descent` and the emitted `parse_<rule>` tower is a *recognizer* — it
validates bbnf syntax and lays down an offset tape with a lazy view (`view.rs:1-20`,
the CSS `CssDocument`/`CssNode` model). That is emitted from the 1206-LOC general-descent
emitter plus the 133-LOC grammar source. It is small and auditable — but it **does not
produce `GrammarIr`.** A recognizer seed cannot be the compiler front-end; it can only be
an *oracle* (which is 7D's territory) or a syntax gate. As a bootstrap seed for the actual
compiler it is vacuous: nothing downstream (`passes::compile`, `codegen`) consumes a tape
of bbnf tokens.

**Building-seed (what self-hosting actually demands).** For the generated bbnf parser to
*replace* the hand parser it must emit `add_expr(ExprKind::…)` calls reconstructing the
`Expr` tower and terminate with `resolve_refs` — i.e. `-> ExprId`/`-> T` builder
projections at every structural rule, folded by a host akin to parse-that's
`host::lower`. **The measured witness that a building-seed inflates:** parse-that already
ships exactly this shape for *regex alone* — `parse_generated(pattern) -> Result<Hir>`
(`parse-that/rust/parse_that/src/regex/mod.rs:34`), "generated parser produces a typed
AST, then `host::lower` folds it into `Hir`." Its generated builder is
**`regex/generated.rs` = 2293 LOC** plus **`regex/host.rs` = 524 LOC**, from a regex.bbnf
source (`parse-that/rust/bootstrap/regex.bbnf`). Regex is a *strictly smaller* language
than bbnf. So a building self-host of bbnf will generate a parser **≥ the 2142-LOC hand
parser it replaces** — the seed does not shrink; it inflates past its own target. This is
the trade to state plainly: **staged-bootstrap on skinny buys auditability and a
committed fixpoint, not LOC reduction.** The "seed" is the *emitter + grammar source*
(1206 + 133 ≈ 1340 LOC of hand surface behind the whole grammar plane), while the
*generated artifact* is larger than the hand parser. The honest headline is therefore
"**1340-LOC hand seed behind a 2000+-LOC generated front-end**", not "88s→0.5s" and not
"parser shrinks to N lines."

**The 88s→0.5s datum is a legacy build-time transplant, not a skinny measurement.** The
number traces to `docs/precepts/audits/REAUDIT-2026-04-30/06-throughput-commit.md:241`:
"76 ms total post-build; the **88 s wall is cargo's rebuild of `bbnf` core**." The 88s was
*cargo's LTO rebuild*, not regen work; actual regen was ~76ms. Any 7A doc that quotes
88s→0.5s as a self-host speed is citing a legacy toolchain artifact. It must be dropped or
re-measured natively (the skinny regen path is `regen.rs` → `codegen::emit_runtime_from_request`).

---

## 3. Choosing the fixpoint SUBJECT (and proving it non-vacuous)

Two candidate subjects for `regen∘regen = regen`:

**(a) Generated `.rs` bytes.** The legacy gate
(`crates/core/tests/bbnf_bootstrap_reproducibility.rs`) uses this: regen bbnf twice into
sibling tempdirs, `diff` byte-for-byte. Skinny's `check-*` arms already do the moral
equivalent per grammar (`check-json` re-emits and asserts non-stale,
`skinny/xtask/src/main.rs:234-237`).

**(b) `GrammarIr` equivalence.** Parse the committed generated bbnf parser's *output*
against the hand parser's output for the same source and assert `GrammarIr`-structural
equivalence (the `pretty()` normal form at `ir/src/lib.rs:124` is a ready canonicalizer).

**The non-vacuity trap the directive names is real and decisive.** Byte-identity of a
*single grammar regenerated twice from the same source through the same emitter* certifies
only that the emitter is deterministic (no hashmap-iteration-order leak, no monotonic
counter) — it does **not** certify self-hosting. A fixed-const emit path stamps identical
bytes for any input; triple-identical of the same-source-same-emitter loop proves nothing
about whether the *generated parser reproduces the compiler*. The legacy gate's Stage
A/Stage B framing (`RISK-PERF-MATRIX.md:246-247`: "Stage A — tape-based compiler builds
struct-based BBNF parser candidate … Stage B — W1 candidate rebuilds itself; byte-equal vs
Stage A") is the **non-vacuous** form: the fixpoint subject must be **two distinct
producers** — parser_N generates parser_{N+1}, and the gate asserts parser_{N+1} ≡
parser_N. Same-emitter idempotence is the weak sibling; cross-generation identity is the
real fixpoint.

**Recommended subject: cross-generation `.rs` bytes, gated by `GrammarIr` equivalence as
the semantic floor.** Bytes are the tight axiomatic check (a self-hosted compiler IS a
fixed point of its own byte output), but bytes alone can drift on cosmetic emit changes
that are semantically inert. So the two-layer subject: **primary = parser_{N+1} bytes ==
parser_N bytes** (the fixpoint proper); **floor = `pretty(hand_parse(bbnf.bbnf)) ==
pretty(generated_parse(bbnf.bbnf))`** for every grammar in the corpus (the semantic
non-vacuity witness, using `ir/src/lib.rs:124`). The bytes gate certifies stability; the
IR gate certifies the generated parser actually *means* the same thing as the hand parser
— foreclosing the "identical bytes, wrong semantics" and "cosmetic drift, same semantics"
failures at once.

---

## 4. Costing the legacy fixpoint-gate: RESCUE-and-port vs re-author

The legacy gate is a ~150-LOC integration test (`bbnf_bootstrap_reproducibility.rs`) that
shells `cargo xtask regen --grammar bbnf --output <tempdir>` twice and diffs. Its
mechanism depends on a legacy `--output <dir>` flag on the legacy `Regen` command
(`xtask/src/main.rs:34-60`) that **does not exist in skinny's xtask** (skinny's regen
writes to the fixed `RuntimeTarget` dir, `main.rs:177-206`; there is no tempdir-output
flag).

- **RESCUE-and-port cost.** The test body is portable prose (workspace-root walk, two
  regen shells, unified-diff on mismatch) — ~150 LOC that ports with light edits. **But
  the port is subordinate to two prerequisites skinny lacks:** (i) a `regen-bbnf` xtask arm
  with a `BBNF_TARGET: RuntimeTarget` const mirroring `SHEETS_TARGET`
  (`main.rs:200-206`) — trivial, ~15 LOC; (ii) **an actual generated bbnf parser to
  regenerate** — which does *not* exist and is the entire building-seed problem of §2. The
  legacy gate presumes a self-host that skinny has not built. Porting the *gate* is cheap;
  it is inert until the *subject* exists. The legacy gate also tests **same-emitter
  idempotence (subject a)**, the weak form — porting it verbatim imports the vacuity risk
  of §3. Rescue-and-port therefore costs ~165 LOC AND silently down-grades the fixpoint to
  the weak subject.

- **Re-author cost.** A skinny-native gate targeting **subject (a) cross-generation +
  floor (b) IR-equivalence** is the stronger artifact. It is ~200-250 LOC: the tempdir
  double-regen (rescued), plus a second stage that feeds parser_N's *output back through
  the generator* to mint parser_{N+1} and diffs the two, plus the `pretty()` IR-equivalence
  assertion. Re-author is +50-100 LOC over rescue but closes the vacuity gap the rescue
  leaves open, and it reads against skinny's `RuntimeTarget`/`emit_runtime_from_request`
  seam natively (no legacy `--output` flag resurrection).

**Verdict for the family:** rescue the *test harness skeleton* (tempdir walk, diff
printer) but **re-author the fixpoint subject** as cross-generation + IR floor. The legacy
gate is a scaffold, not a specification.

---

## 5. The born-RED gate for the parser-augment

The augment (the three bounded parser gaps of §1.3) needs a gate that is RED until the
gaps close and cannot pass vacuously. Concretely:

**`check-bbnf-selfparse` (born-RED).** Assert `parse_grammar(read("grammar/bbnf/bbnf.bbnf"))`
returns `Ok(GrammarIr)` — RED today because `parse_atom` rejects `@{`/`[`/`{`
(`grammar/src/lib.rs:1495`), `parse_seq` mis-attaches `?w` across `->`, and `|params|`
closures have no production. GREEN only when all three augments land. This is the
minimal precondition: **bbnf must parse+build under the hand parser before any self-host
regen is even meaningful** — the general-descent recognizer route (`runtime_generator.rs:98`)
cannot mint a *builder*, so the hand parser is the reference the generated builder must
match, and it must first accept its own grammar.

**Falsification (the gate must be able to fail).** Plant a bbnf source using each
augmented construct in isolation (`x = @{ a } ;`, `x = [ a ] ;`, `f = |p| p ;`,
`x = a ?w -> Span ;`) and assert each parses to the expected `ExprKind` (`Annotation`/
`Optional`/closure-lowered/`Project`), and that a *malformed* variant (`x = @{ a ;`,
unterminated) errors explicitly — never a silent `Epsilon` (memory: `no-silent-epsilon`).
The gate that only ever sees well-formed input is a gate that cannot fail.

---

## 6. The shared open empirical question (RED-unknown)

*Does authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the
recognizer emitter reparse the corpus into `GrammarIr`-equivalent structure within the
`add_expr`+`resolve_refs` host budget while `regen∘regen` holds byte-identical — or does it
cascade, and where?*

**Report: RED-unknown, and the mechanism is where the doubt concentrates.** The
general-descent emitter emits *recognizers* (`emit.rs:259`, `-> Result<(), ParseError>`
writing tape marks), not builders. Its regex leaves are `Span`-scanned by a
delimiter-fallback, not walked (`mod.rs:16-25`). The construction census the directive
gives is: node values are pure `->`; the *build mechanisms* are host-bound — `add_expr`
(append, `ir/src/lib.rs:51`) and `resolve_refs` (cross-rule link, a single-pass name→id
resolve, `ir/src/lib.rs:84`). So the empirical test is: **can a builder augment over the
recognizer emitter round-trip bbnf into GrammarIr?** This is genuinely unseeded, and the
existence of the general-descent emitter does **not** presume it — that emitter builds
tapes, not `Expr` trees, and whether a `-> ExprId` projection composes over its per-rule
recognizer skeleton without a borrow-checker escape (the tape-mut cursor vs an `&mut
GrammarIr` builder threaded through the same recursion) is exactly the open construct.

The concrete cascade candidates to watch: **(i)** the recursive `rhs = closure |
alternation` self-reference — does a `-> ExprId` fold over a left-recursive-looking
alternation tower terminate under the emitter's node-by-node walk? **(ii)** `resolve_refs`
runs *after* the full tree is built (it needs all rule names first, `:86-93`); a streaming
recognizer that marks-as-it-goes has no natural seam for a deferred second pass. **(iii)**
the `@{ rhs }` host-annotation atom: the recognizer would Span-scan it, but a builder must
recurse into `rhs` and emit an `Annotation` node — the emitter has no recursion-into-atom
for `@{`. The prototype must drive the `!has_rule("value")` branch
(`runtime_generator.rs:98`) — **never** `json_sink_direct` — and must **not** seed the
verdict from the legacy emitter (which was a different, builder-shaped codebase) nor from
the mere existence of general-descent.

---

## 7. Prior art (citations)

- **rustc / rustc_parse bootstrap.** rustc is bootstrapped stage0→stage1→stage2 with the
  fixpoint being *the compiler compiling itself to the same binary*; the "beta compiler
  builds stage1, stage1 builds stage2, stage1 == stage2" identity is exactly the
  cross-generation subject of §3 (not same-source idempotence). rustc's `x.py` stages are
  the canonical "two distinct producers" fixpoint.
- **GHC's `make maintainer-clean` / stage2 == stage3.** GHC certifies self-host by
  building stage3 with stage2 and asserting object-code equivalence — the strongest form
  of the byte-fixpoint, and the reason GHC treats stage1≠stage2 as expected (different
  producer) but stage2==stage3 as the gate.
- **regex-automata / regex crate.** Its parser (`regex-syntax`) is hand-written, NOT
  self-hosted — a deliberate choice: the regex authors keep the bootstrap surface as a
  hand parser precisely to avoid the building-seed inflation of §2. This is the negative
  prior art: a mature engine that *declined* self-host for the parser.
- **parse-that's own regex self-host** (`regex/mod.rs:34`, `regex/generated.rs` 2293 LOC,
  `regex/host.rs` 524 LOC) — the live in-repo witness that a *building* self-host works
  (produces `Hir`) and how large it gets. This is the closest analog and the sizing anchor.
- **tree-sitter grammar.js → generated C parser.** tree-sitter's generator emits a table-
  driven parser from a grammar DSL and regenerates deterministically; its `test`/`generate`
  discipline is a byte-fixpoint on generated `parser.c`. Relevant as the "recognizer seed
  is enough for a *tool*, not a *compiler front-end*" data point.

---

## 8. What a prototype must test to move family 7A

1. **Parser-augment feasibility (the precondition).** Land the three `parse_atom`/`parse_seq`
   augments (`@{`/`[`/`{` atoms, `?w`-across-`->`, `|params|` closures) and run
   `check-bbnf-selfparse` (§5). Measure the LOC cost of the augment against the hand parser
   (does closing the gaps inflate `grammar/src/lib.rs` past a threshold?). GREEN = bbnf
   parses+builds to `GrammarIr` under the hand parser.
2. **Builder-over-recognizer round-trip (the crux, §6).** Author `-> ExprId` builder
   projections on bbnf's structural rules, route through `emit_general_descent`
   (`runtime_generator.rs:98`, never `json_sink_direct`), and test whether the emitted
   parser reparses `grammar/**/*.bbnf` into `pretty()`-equivalent `GrammarIr`
   (`ir/src/lib.rs:124`) within the `add_expr`+`resolve_refs` host budget. **Record the
   exact construct at which it cascades** (closure recursion / deferred `resolve_refs` seam
   / `@{` atom recursion) if it does. This is the pass-2 pivotal measurement.
3. **Fixpoint non-vacuity (§3).** Implement the cross-generation gate (parser_N mints
   parser_{N+1}, assert byte-identity AND `GrammarIr` equivalence). Prove it is
   non-vacuous by planting a semantic-preserving cosmetic emit change (must pass the IR
   floor, fail the byte layer only if bytes change) and a semantic-breaking change (must
   fail the IR floor) — the falsification the directive demands.
4. **Building-seed sizing (§2).** Regenerate the bbnf builder and record generated LOC vs
   the 2142-LOC hand parser and the 2293-LOC parse-that-regex precedent. Confirm or refute
   the inflation thesis with a skinny number.
5. **Legacy-gate disposition (§4).** Port the tempdir/diff skeleton, re-author the subject,
   and confirm the skinny-native gate reads against `emit_runtime_from_request` with no
   revived legacy `--output` flag.

**The one datum that most moves 7A:** test-2's cascade point. If the builder-over-recognizer
round-trip holds through bbnf's full construct set, staged-bootstrap is a bounded augment
(three parser gaps + a gate) buying auditability at known LOC inflation. If it cascades at
closure-recursion or the `resolve_refs` seam, staged-bootstrap on the recognizer emitter is
refuted and the family must either grow the emitter into a builder (a larger change,
toward 7B territory) or accept the recognizer seed as an *oracle only* (toward 7D).
