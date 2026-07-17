# LOCK-5 · Family 5D — GRAMMAR-FRAGMENT LOWERING (pass-3, research-D)

Charter: regex literals in grammars lower DIRECTLY into the grammar's own IR — no separate
engine on the generator path — while the standalone parse-that regex API survives as a thin
front over the same machinery. Settled ground (XL-C): grammar tier keeps `Regex(StringId)`
opaque; engine tier keeps a rich `Hir`; a cheap `string`/`format!` bridge reusing the engine's
`simplify_hir` joins them. This report hardens the substrate with run-evidence seams and states
exactly what a prototype must fire.

---

## 1. THE MECHANISM, MADE CONCRETE — where a regex leaf lives today

The generator path already realises 5D's thesis in embryo: **there is no regex engine on the
codegen path at all.** A grammar regex literal `/…/` is `ExprKind::Regex { pattern: String }`
(`skinny/crates/ir/src/lib.rs:250`), lowered to `BackendExpr::RegexProgram { pattern,
span_kind }` (`ir/src/lib.rs:389`; `span_kind: SpanKind{String,Number,Whitespace}`
`ir/src/lib.rs:440`), then emitted as a **first-byte-set membership scan**:

    match_leaf(bytes, cursor, tape, {first_lit}, {node_kind}u8, {nullable})?;

(`skinny/crates/codegen/src/general_descent/emit.rs:365-375`). The `{first_lit}` byte set is
derived by a hand-rolled leading-token parser `regex_first::first_bytes`
(`codegen/src/general_descent/regex_first.rs:81`), `{nullable}` by `regex_first::is_nullable`
(`regex_first.rs:18`). The full alternation/quantifier/lookaround AST is **discarded** at this
seam (`general_descent/mod.rs:13-18` states it: the recognizer AST is DERIVED from the
program's `ByteLiteral`/`RegexProgram` nodes — no byte is matched from the alternation/quantifier
AST). The real per-shape semantics (`{3,6}` bound, `$` anchor, string escapes, numeric spelling)
survive ONLY through `SpanKind`, consumed by hand-written runtime scanners keyed off it
(`json_sink_direct/dispatch.rs:86,165`; `skeleton.rs:143`).

So 5D is HALF-BUILT and UNDERPOWERED: the leaf reaches the grammar's own IR with no engine
attached (thesis satisfied), but the "matcher" is a greedy first-set run, not the pattern. The
family's strongest form is to (a) collapse the triplicated hand parser into ONE derivation off
the engine HIR, (b) give `RegexProgram` a structured inner IR (a `ByteClass`/small class-loop
program) so a bounded/anchored/one-pass leaf is matched correctly on the generator path, and
(c) route only the residue (unanchored multi-byte search) to `Dfa::find` — the parse-that engine
consumed, not duplicated (XL-C bridge, one-regex-home).

## 2. THE SUBSTRATE — the god-duplication 5D must collapse (Q7/Q8 seam)

The leading-byte set of a regex leaf is computed by **THREE independent hand parsers of the
same regex surface**:

1. `skinny/crates/bbnf-regex/src/lib.rs:120` `first_set` → `first_from_prefix:142` →
   `parse_leading_class:162` (the facts analyzer; `RegexKind` classifier at `:92`, only 4
   kinds: Whitespace/QuotedString/Numeric/Unknown).
2. `skinny/crates/codegen/src/general_descent/regex_first.rs:81` `first_bytes` +
   `class_bytes:165` + `head_bytes:120` (the descent emitter's own copy — richer: handles
   `\d\w\s`, top-level alternation split at `:90`, non-capturing group descent at `:124`).
3. `skinny/crates/codegen/src/json_sink_direct/first_set.rs:77` `regex_first_bytes(span_kind,
   pattern)` (the sink-direct copy, keyed additionally on `SpanKind`).

Three closing-class scanners, three range parsers, three escape tables — a god-duplication of
regex-surface parsing the constitution's no-god-modules / DRY edict condemns. **5D's substrate is
a single `first_of` + `class_program_of` derived once from the engine HIR** (the engine already
owns `first.rs` `regex_first_chars`, `info/one_pass.rs`, `info/literal_prefix.rs`, R3 §3). The
bridge is the cheap `string`→`simplify_hir`→facts round the settled ground mandates; the three
hand copies DIE. This is the measurable LOC-shrink 5D banks: ~231 (regex_first.rs) + the
bbnf-regex `first_set` path + the sink copy, replaced by one engine call + a thin `ByteClass`
projector.

## 3. Q7 — THE FOLLOW-SOUND ONE-PASS FRACTION (the number that gates the DFA)

`match_leaf` is a **greedy maximal run** over the first-set bytes (emit.rs:365). This is correct
ONLY when the leaf's consuming class is **disjoint from its grammar FOLLOW set** — otherwise it
over-consumes (e.g. a `[a-z]+` leaf immediately followed in the grammar by a `[a-z]` terminal).
The one-pass-soundness predicate is not intrinsic to the regex; it is `class(leaf) ∩
FIRST(grammar-FOLLOW(leaf)) = ∅`. The engine's `info/one_pass.rs:72` (`is_one_pass` on the HIR)
is the intra-pattern half; the grammar FOLLOW is the inter-leaf half computed by the passes'
nullable/first machinery (`skinny/crates/passes/src/lib.rs` first-sets; the fixed-point
`nullable` at `passes/src/lib.rs:~340`).

**What the prototype must measure (BOTH fresh, per directive):**
- (a) straight-line lowering: `RegexProgram` → `match_leaf` greedy scan. Fraction of engine-
  corpus leaves where `class(leaf) ∩ FIRST(FOLLOW) = ∅` → the fraction one-pass-sound with ZERO
  structured matcher.
- (b) `Seq[Repeat{body,min}, suffix]` + grammar FOLLOW: re-lower the leaf as an explicit
  repeat-then-suffix so the loop terminates on the FOLLOW byte, not on class-exhaustion. This
  RESCUES the non-disjoint leaves into one-pass at the cost of a FOLLOW guard per iteration.
  Measure the fraction now soundly one-pass under (b).
- Corpus: the grammars actually wired — JSON (`string`/`number`/`ws` RegexProgram leaves), Sheets
  (`regex_first.rs:6-8` enumerates its leaf shapes: `[A-Za-z_]\w*`, `\$?[A-Za-z]{1,3}\$?\d+`,
  `'(?:[^']|'')*'!`, `[lL][eE][tT]\(`), CSS. The residue (fraction neither (a) nor (b) can make
  one-pass) is the ONLY set that earns a `Dfa::find`. If that residue is empty on the real corpus,
  **the DFA never reaches the generator path** and 5D is proven pure — the standalone engine is
  then genuinely a thin API front (XL-C), not a codegen dependency. That empty-or-not is the single
  most load-bearing number for this family.

## 4. Q8 — THE PROTOTYPE THAT MUST FIRE (four concrete asserts)

1. **Cold-paired class-scan vs prefiltered `Dfa::find`.** For the one-pass residue only, pair the
   existing `match_leaf` first-set run against `Dfa::find` (the parse-that prefilter tier, certified
   8.8–29.5× on prefilterable, 0.97–1.01× controls, one-time O(256) start-row derivation —
   `Prefilter::from_dfa`). Cold, N≥200, paired same-binary, identical match-set assert at setup. The
   win only counts on a leaf whose first-set is a rare single/2/3-byte prefix; a dense class
   (`[a-z]+`) sits at parity and MUST stay `match_leaf` (the tier cost model, not a blanket route).
2. **A `ByteClassRecognizer` that actually fires — not the masked wildcard.** Today
   `Recognizer::SimdScan{ mode: SimdMode::Exact, … }` is derived (`passes/src/lib.rs:361`) only for
   the JSON structural bytes `{}[],:"` plus the quoted-string `"` (`derive_recognizers:338-365`) —
   `SimdMode` has a SINGLE variant `Exact` (`ir/src/lib.rs:427`); there is no `Prefilter`/`ByteClass`
   mode. The prototype adds `SimdMode::ByteClass(ByteSet256)` and a `SimdSite` at the leaf's
   self-loop, derived from the leaf's own class (the `ByteSet256` exists at
   `bbnf-regex/src/lib.rs:35`), and proves it emits a real membership skip — not the degenerate
   all-bytes wildcard a negated/Unknown class produces (`regex_first.rs:167` and `:157` both bail to
   empty on negation, which currently masks into a no-op scan).
3. **`BackendExpr::ByteClass` reaching the tape.** Add a `BackendExpr::ByteClass { set, min, max,
   span_kind }` beside `RegexProgram` (`ir/src/lib.rs:388`) so a class-loop leaf lowers to a
   STRUCTURED node the tape emitter consumes directly (`mark(tape, _node_start, node_kind)`
   `emit.rs:388`), rather than round-tripping through the opaque `pattern: String`. Assert the tape
   bytes are byte-identical to the current `match_leaf` path on the one-pass leaves (only offsets
   must match; tape is offsets+sparse-flags+arena — AoS/SoA is moot per SPEC §0).
4. **Capture-leaf → `Project` byte-identical to a hand-authored grammar capture.** A regex leaf
   carrying an inner `->` (a capture) must lower to `BackendExpr::ValueProject { body, target, ty }`
   (`ir/src/lib.rs:407`) whose emitted scan is byte-identical to the same capture written as a bare
   grammar rule. This is the isomorphism proof that the regex sub-language is not a privileged path.

## 5. Q11 — THE `->` REPRESENTATION (binding on bridge soundness; confirm with LOCK-1)

At the IR it is `ExprKind::Project { body, target, ty: Option<String> }` (`ir/src/lib.rs:261`) →
`BackendExpr::ValueProject { body, target, ty }` (`:407`). The emitter is decisive
(`emit.rs:376-380`): **`ValueProject` emits its BODY for the recognizer and the projection is
"consumed by the VIEW, not the recognizer."** Therefore at the scan/recognition plane the `->` is
a **Map BARRIER** — the byte-matcher runs the body and the projection is erased/deferred; at the
value/materialization plane it is a **typed edge** re-applied by the view. This dual nature is
exactly the "inner-class Map barrier vs typed edge" the directive names, and it is binding on the
bridge's projection soundness: an inner-class `->` INSIDE a regex leaf cannot cross into the
byte-scan (the class-loop is projection-opaque); it must surface at a leaf boundary the view can
see. **LOCK-1 must ratify** that a regex-internal projection is either (i) forbidden (regex leaves
capture-free at the byte plane; projections attach only at the leaf boundary), or (ii) lifted to a
`Seq` boundary before lowering. Recommendation for 5D: (i) — the class-program stays
projection-free; the `->` binds the whole leaf's span, matching the current
`ValueProject{body=RegexProgram}` shape.

## 6. Q9 — ONE GRAMMAR-DRIVEN REGEX PATH (SPEC-ONLY, gated LOCK-7/7A)

The grammar-tier regex grammar exists at `grammar/misc/regex.bbnf` (28 lines: `expression`, `term
= factor - end`, `char_class`, `escaped`, `lookahead`/`lookbehind`, `regex = "/" >> expression,
end`). In this tree it is a **spec artifact, not on any path**: there is no `bootstrap/regex.bbnf`
and no `parse_generated()` in skinny (both live in the parse-that repo — `rust/bootstrap`
re-exports `parse_that::regex::generated`, R3 §1). The one grammar-driven path 5D would exercise:
promote a bootstrapped `regex.bbnf` so its generated parser returns an `Hir`/AST instead of the
current `bool` (break-the-discard), and de-dup `grammar/misc/regex.bbnf` against the engine's hand
parser (`hir/parser/mod.rs:153` `parse`, R3 §3). Evidence obligation: the AST actually threaded
into `simplify_hir` (the XL-C bridge), grammar-totality picture. This is
**SPEC-ONLY(grammar-driven)** and gated on LOCK-7/7A — it rides on every finding above but is not a
pass-3 landing.

## 7. Q10 — E-GRAPH REWRITE FIRE-COUNT CENSUS (measurement locus, SPEC-noted)

The grammar-tier regex e-graph rules do NOT live in this tree — the vendored
`skinny/crates/parse-that-regex` is the number/unicode/integration subset (3022 LOC; confirmed no
`egraph/` dir). The rules are in the parse-that engine `rust/regex/src/egraph/` (1253 LOC,
READ-ONLY): `repetition.rs`(156), `union.rs`(108), `flatten.rs`(107), `superset.rs`(90),
`redundant.rs`(68) (R3 §3). The census (per-rule fire count over the engine corpus, both bare and
`Project`-wrapped branches) MUST run against that engine on a scratch copy (R3 §8 recipe). 5D's
stake: rules that never fire on real grammar leaves are dead weight the bridge need not carry;
rules that fire (e.g. `Optional↔Repeat{0,1}`, Group-elision — the byte-identity pair) canonicalize
the leaf BEFORE first-set derivation, shrinking the class program. This is a measurement obligation,
not a pass-3 in-tree landing.

## 8. PRIOR ART (citations)

- **regex-automata (Rust, BurntSushi)**: the memchr/rare-byte prefilter ladder + one-pass DFA
  (`is_one_pass`) — the exact `Prefilter::from_dfa` start-row idiom the engine vendors; the "one
  regex, many search strategies behind a cost gate" doctrine 5D mirrors at leaf grain.
- **Teddy (ICPP'21) / shufti (Hyperscan)**: nibble-LUT multi-byte membership (`vqtbl1q_u8`) — the
  wide-entry tier for class-loops whose first-set exceeds `memchr3`; the `ByteClassRecognizer`
  substrate (Q8.2) is a shufti at leaf scale.
- **tree-sitter**: subtree-level lexer where token regexes compile to a shared DFA the parser
  drives — the counterexample to a per-leaf engine; 5D instead keeps leaves in the grammar IR and
  only borrows the engine for the search residue.
- **GHC/rustc first/follow one-pass lexing**: FOLLOW-disjointness as the soundness side-condition
  for greedy maximal-munch — the Q7 predicate is the classical LL(1) leaf condition.
- **egg / equality saturation**: the e-graph canonicalization the engine's `egraph/` applies to the
  HIR before automata (Q10); 5D consumes its output, does not re-home it.

## 9. NAMED RISKS & COUNTEREXAMPLES

- **The FOLLOW-disjoint fraction is ~100% on the real corpus → the DFA has no consumer on the
  generator path.** If every wired leaf is one-pass-sound (JSON/Sheets/CSS leaves are class-loops
  with structural FOLLOW), `Dfa::find` never fires in codegen and Q8.1's win is unmeasurable
  in-generator — it survives ONLY as the standalone API's engine. Consumer-less-substrate (Y.13)
  then FORBIDS wiring the DFA into codegen. *This is the likeliest outcome and must be reported as a
  positive result, not patched around.*
- **Over-consumption bug latent in `match_leaf` today.** A non-FOLLOW-disjoint leaf greedily eats a
  following terminal. Counterexample to build: a grammar with `/[a-z]+/` immediately followed by a
  `[a-z]` literal — assert the current emit produces a WRONG parse, proving the Q7(b)
  `Seq[Repeat,suffix]`+FOLLOW re-lowering is a correctness fix, not just a fraction number.
- **Negated / Unknown class masks into a wildcard no-op.** `class_bytes` bails to EMPTY on `[^…]`
  (`regex_first.rs:167`) and `head_bytes` bails on metachars (`:157`) — an empty first-set makes
  `match_leaf` a wildcard the `ByteClassRecognizer` cannot skip. Counterexample: a `[^"]*`
  string-body leaf — the recognizer must NOT claim to prefilter it (it degenerates to the runtime
  string scanner via `SpanKind::String`). The Q8.2 assert must fire on a POSITIVE class only.
- **Triplication drift is already live.** The three first-byte parsers (§2) disagree on edge cases
  (bbnf-regex's `class_byte:212` returns `None` on `\d` inside a class; regex_first's
  `class_byte:207` does not). A prototype that collapses them must assert byte-identical first-sets
  across all three on the corpus BEFORE deleting two — a silent behavioural change otherwise.
- **`->` inside a regex leaf has no lawful lowering today** (§5). If LOCK-1 rules projections may
  nest inside a class, the barrier/edge duality breaks and the class program is no longer
  projection-free. Counterexample the LOCK-1 handshake must resolve: `/(\d+) -> year/` as a leaf.

## 10. WHAT A PROTOTYPE MUST TEST TO MOVE 5D

1. The Q7 fraction, BOTH (a) straight-line and (b) `Seq[Repeat,suffix]`+FOLLOW, over the wired
   JSON/Sheets/CSS corpus — the number that decides whether the DFA has any generator-path consumer
   at all.
2. The Q8 four-assert prototype: cold-paired `match_leaf` vs `Dfa::find` on the residue;
   `SimdMode::ByteClass` firing a real skip; `BackendExpr::ByteClass` tape byte-identity;
   capture-leaf → `ValueProject` byte-identity vs hand grammar capture.
3. The §2 triplication collapse: one `first_of`/`class_program_of` off the engine HIR,
   byte-identical to all three hand parsers on the corpus, then two deleted (LOC delta reported).
4. The over-consumption counterexample (§9) proving Q7(b) is a correctness fix.
5. The LOCK-1 handshake on the `->` representation (§5, Q11) — barrier at recognizer, edge at view,
   regex-internal projection forbidden or lifted.
