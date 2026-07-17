# LOCK-5 · Family 5D — GRAMMAR-FRAGMENT LOWERING (pass-3, research-D)

Charter: regex literals in grammars lower DIRECTLY into the grammar's own IR — no separate
engine on the generator path — while the standalone parse-that regex API survives as a thin
front over the same machinery. Settled ground (XL-C, RATIFIED): the grammar tier keeps
`Regex(StringId)` opaque; the engine tier keeps a rich `Hir`; a cheap `string`→`simplify_hir`
bridge joins them; parse-that is the one-regex-home and the vendored copy is the measurement
substrate. This pass HARDENS the substrate with run-evidence: the family's central seams are now
LANDED-AND-COMPILING in-tree and the load-bearing FOLLOW fraction is MEASURED, not estimated.

---

## 1. THE MECHANISM, MADE CONCRETE — where a regex leaf lives, and the bug that lives with it

The generator path already realises 5D's thesis in embryo: **there is no regex engine on the
codegen path.** A grammar regex literal `/…/` is `ExprKind::Regex { pattern }`
(`skinny/crates/ir/src/lib.rs:171,250`), lowered to `BackendExpr::RegexProgram { pattern,
span_kind }` (`ir/src/lib.rs:389`, `span_kind: SpanKind` at `:391`/`:440` —
`String`/`Number`/`Whitespace`), then emitted as a **greedy first-byte-set membership run**:

    match_leaf(bytes, cursor, tape, {first_lit}, {node_kind}u8, {nullable})?;

(`codegen/src/general_descent/emit.rs`, the `match_leaf` text-head branch ≈ `:195-206`, offset
push ≈ `:365-375`). The `{first_lit}` set is a hand-rolled leading-token parse
(`regex_first::first_bytes`, `codegen/src/general_descent/regex_first.rs:81`); `{nullable}` from
`regex_first::is_nullable` (`:18`). The alternation/quantifier/lookaround AST is **discarded** at
this seam (`general_descent/mod.rs:13-18`): no byte is matched from the structured AST; the real
per-shape semantics (`{3,6}` bound, `$` anchor, escapes, numeric spelling) survive ONLY through
`SpanKind`, consumed by hand-written runtime scanners keyed off it (`json_sink_direct/dispatch.rs`;
`skeleton.rs`).

This is not merely underpowered — it is **incorrect** on any leaf whose consuming class overlaps
its grammar FOLLOW. The greedy run over-consumes. PROTO-1-5D built the minimal witness
`rule = word "and" word`, `word = /[a-z]+/`, where `first(word) ∩ FOLLOW(word) ∋ 'a'` (`'a'` begins
`"and"`). RAN result: the current `match_leaf` eats the whole `[a-z]` run of `aandb` as the first
`word` (span `[0,5)`), destroys the `"and"` delimiter, and **REJECTS a valid sentence** (`Err(5)`);
it disagrees with the correct parse on **12/20** corpus inputs. This makes 5D's direct-scan
hardening a **correctness fix**, not a perf nicety.

## 2. Q7 — THE FOLLOW-SOUND ONE-PASS FRACTION (MEASURED; the number that gates the DFA)

The one-pass-soundness of a greedy leaf is NOT intrinsic to the regex — it is the classical LL(1)
side-condition `class(leaf) ∩ FIRST(grammar-FOLLOW(leaf)) = ∅`. The intra-pattern half is the
engine's `info/one_pass.rs` `is_one_pass`; the inter-leaf half is the passes' FIRST/NULLABLE/FOLLOW
fixed point (`passes/src/lib.rs`; LLVM-style `while changed` closure delivered at
`passes/tests/follow_sound_fraction.rs`). Both fresh fractions the directive demands were RUN over a
16-pattern engine sample:

- **(a) straight-line lowering** (`RegexProgram` → greedy `match_leaf`): **disjoint = 3/16 = 18.8%**
  one-pass-sound with ZERO structured matcher. The other 13/16 over-consume as in §1.
- **(b) `Seq[Repeat{body,min}, FOLLOW-guard]`** (re-lower so the loop halts on the FOLLOW byte, not
  on class-exhaustion): of the 10/16 give-back-prone trailing-unbounded leaves, **3/10** become
  FOLLOW-sound; **effectively one-pass = bounded ∪ prone-sound = 9/16 = 56.2%.** The
  `word/"and"` counterexample is confirmed NON-disjoint (predicate SOUND, not vacuous).

**The load-bearing consequence.** The residue neither (a) nor (b) can make one-pass is **7/16 =
43.8%** on this sample — **NON-EMPTY.** This REVERSES the pass-2 hypothesis that the DFA has no
generator-path consumer: on real backtracking-prone leaves the `Seq[Repeat,FOLLOW-guard]` fusion is
insufficient and a give-back / `Dfa::find` residue survives. That residue is the ONLY set that
earns the parse-that engine on the codegen path; everything else stays a direct guarded scan. The
remaining measurement obligation is to re-run this fraction over the WIRED grammar leaves
(JSON `string`/`number`/`ws`; Sheets `[A-Za-z_]\w*`, `\$?[A-Za-z]{1,3}\$?\d+`, `'(?:[^']|'')*'!`,
`[lL][eE][tT]\(` per `regex_first.rs:6-8`; CSS) — the synthetic 16-pattern sample proves the
predicate is sound and the residue is non-empty in principle; the wired fraction sets the actual
DFA-consumer count.

## 3. Q8 — THE SUBSTRATE, LANDED AND COMPILING (four asserts, run-evidence)

The pass-2 sketch is now in-tree and green (`cargo build -p ir -p passes -p codegen`, EXIT 0):

1. **`SimdMode::ByteClass(ByteSet256)` firing a real membership skip — not the masked wildcard.**
   Before this pass `SimdMode` had a SINGLE variant (`Exact`), derived (`derive_recognizers`,
   `passes/src/lib.rs:338`) only for the JSON structural bytes `{}[],:"` — a negated/Unknown class
   (`class_bytes` bails EMPTY on `[^…]`, `regex_first.rs:167`; `head_bytes` bails on metachars,
   `:157`) masked into an all-bytes no-op scan. The prototype ADDED `SimdMode::ByteClass([u64;4])`
   (`ir/src/lib.rs:425`, serde-native so `BackendExpr` derives hold with no dep) + a `SimdSite` at
   the leaf self-loop derived from the leaf's own class, and `Recognizer::ByteClass { set, follow,
   site }`. It emits a genuine positive-class membership skip.
2. **`BackendExpr::ByteClass` reaching the tape.** Added beside `RegexProgram`
   (`ir/src/lib.rs:389`): `BackendExpr::ByteClass { set, min, max, follow, span_kind }`. It lowers
   to `SinkOnlyExpr::ByteClass` (`lower/sink_only.rs`) and routes through BOTH emitters —
   `lower/tape_plan.rs` (`scan_byteclass_guarded`) and `general_descent/emit.rs` (a call to the new
   `match_byteclass_guarded` prelude primitive: membership run → FOLLOW-guard halt → `min` floor →
   `push_offset`). RAN assert: emitted tape (the ordered `Vec<(offset,kind)>` of leaf pushes) is
   **byte-identical to the hand-authored control `word = ([a-z] - "and")+` on 20/20 inputs** — the
   control specified from the class-DIFFERENCE, independent of FOLLOW, so equality is a runtime
   finding, not a source tautology. (`max` retained for `{m,n}`; `follow: Vec<u8>` travels with the
   node because a stop-guard is inert without the token it halts before — the `Seq[Repeat,guard]`
   fusion made concrete.)
3. **Cold-paired class-scan vs prefiltered `Dfa::find` (the residue tier).** Still the one open
   measurement: for the 7/16 residue only, pair `match_leaf` against `Dfa::find` (parse-that's
   prefilter tier — `Prefilter::from_dfa` start-row, PROVE-10-certified 8.8–29.5× on prefilterable,
   0.97–1.01× on dense controls). The win counts only when the first-set is a rare 1/2/3-byte
   prefix; a dense `[a-z]+` sits at parity and MUST stay the guarded direct scan (a per-leaf cost
   gate, not a blanket route). This is the cold-pair a follow-on prototype must fire.
4. **Capture-leaf → `ValueProject` byte-identity.** A regex leaf carrying `->` must lower to
   `BackendExpr::ValueProject { body, target, ty }` (`ir/src/lib.rs:407`) whose scan is byte-
   identical to the same capture as a bare grammar rule — the isomorphism proof that the regex
   sub-language is not privileged. Structurally in place via `ValueProject{body=ByteClass}`; the
   byte-identity assert is the second open follow-on.

**The one designed-not-wired seam (honest scope).** `derive_recognizers` (`passes/src/lib.rs:338`)
is NOT yet rewired onto the FOLLOW fixed point that DECIDES which leaves get `ByteClass` vs the
masked `RegexProgram`. The closure exists (`passes/tests/follow_sound_fraction.rs`); the substrate
and emitter that receive its verdict now exist and compile. Wiring the verdict is the next in-tree
landing.

## 4. THE SUBSTRATE 5D COLLAPSES — the god-triplication (DRY / no-god-modules edict)

The leading-byte set is computed by **THREE independent hand parsers of the same regex surface**:
`bbnf-regex/src/lib.rs:120` (`first_set`→`first_from_prefix:142`→`parse_leading_class:162`; 4-kind
`RegexKind` classifier at `:92`); `codegen/.../regex_first.rs:81` (`first_bytes`+`class_bytes:165`+
`head_bytes:120`; richer — `\d\w\s`, top-alt split `:90`, non-capturing descent `:124`);
`codegen/json_sink_direct/first_set.rs:77` (`regex_first_bytes`, keyed additionally on `SpanKind`).
Three closing-class scanners, three range parsers, three escape tables — **already drifting**
(bbnf-regex `class_byte:212` returns `None` on `\d`-in-class; regex_first `class_byte:207` does
not). 5D's substrate is ONE `first_of` + `class_program_of` derived off the engine HIR (which
already owns `first.rs` `regex_first_chars`, `info/one_pass.rs`, `info/literal_prefix.rs`) through
the XL-C `string`→`simplify_hir` bridge; the three copies DIE. Measurable shrink: ~231 LOC
(`regex_first.rs`) + the bbnf-regex first path + the sink copy, replaced by one engine call + a thin
`ByteClass` projector. **Mandatory guard:** a prototype must assert byte-identical first-sets across
all three on the corpus BEFORE deleting two — the drift means naive collapse is a silent behavioural
change. classify re-home therefore SHRINKS (three parsers → one), it does not merely relocate.

## 5. Q11 — THE `->` REPRESENTATION (binding on bridge soundness; confirm with LOCK-1)

At the IR: `ExprKind::Project { body, target, ty }` (`ir/src/lib.rs:185,261`) →
`BackendExpr::ValueProject { body, target, ty }` (`:407`). The emitter is decisive
(`emit.rs:376-380`): **`ValueProject` emits its BODY for the recognizer; the projection is consumed
by the VIEW, not the recognizer.** So at the scan plane `->` is a **Map BARRIER** (the byte-matcher
runs the body, projection erased/deferred); at the materialization plane it is a **typed edge**
re-applied by the view. This IS the directive's "inner-class Map barrier vs typed edge" duality, and
it is binding: an inner-class `->` INSIDE a regex leaf cannot cross into the projection-opaque
class-loop; it must surface at a leaf boundary the view can see. **LOCK-1 must ratify** one of: (i)
regex leaves are capture-free at the byte plane — projections attach only at the leaf boundary
(`ValueProject{body=RegexProgram|ByteClass}`, the current shape); or (ii) a regex-internal
projection is lifted to a `Seq` boundary before lowering. **5D recommends (i)**: the class program
stays projection-free; `->` binds the whole leaf span. Counterexample the handshake must resolve:
`/(\d+) -> year/` as a leaf — under (i) it is the leaf-boundary form `(\d+) -> year`, not a
class-internal capture.

## 6. Q9 — ONE GRAMMAR-DRIVEN REGEX PATH (SPEC-ONLY, gated LOCK-7/7A)

The grammar-tier regex grammar exists at `grammar/misc/regex.bbnf` (28 lines: `expression`,
`term = factor - end`, `char_class`, `escaped`, `lookahead`/`lookbehind`,
`regex = "/" >> expression, end`). In this tree it is a **spec artifact on no path**: there is no
`bootstrap/regex.bbnf` and no `parse_generated()` in skinny (both live in parse-that —
`rust/bootstrap` re-exports `parse_that::regex::generated`). The one grammar-driven path 5D would
exercise: promote a bootstrapped `regex.bbnf` so its generated parser returns an `Hir`/AST instead
of `bool` (break-the-discard), thread that AST into `simplify_hir` (the XL-C bridge), and de-dup
`grammar/misc/regex.bbnf` against the engine hand parser (`hir/parser/mod.rs:153`). This is
**SPEC-ONLY(grammar-driven)**, gated on LOCK-7/7A — it rides on every finding above but is not a
pass-3 landing.

## 7. Q10 — E-GRAPH REWRITE FIRE-COUNT CENSUS (measurement locus, SPEC-noted)

The grammar-tier regex e-graph rules do NOT live in this tree — the vendored
`skinny/crates/parse-that-regex` is the number/unicode/integration subset (~3022 LOC, no `egraph/`
dir). The rules are in the parse-that engine `rust/regex/src/egraph/` (READ-ONLY, ~1253 LOC):
`repetition.rs`(156), `union.rs`(108), `flatten.rs`(107), `superset.rs`(90), `redundant.rs`(68). The
per-rule fire-count census (over the engine corpus, both bare and `Project`-wrapped branches) runs
against that engine on a scratch copy. 5D's stake: rules that never fire on real grammar leaves are
dead weight the bridge need not carry; rules that DO fire (`Optional↔Repeat{0,1}`, Group-elision —
the byte-identity pair the automata must preserve) canonicalize the leaf BEFORE first-set
derivation, shrinking the class program. A measurement obligation, not an in-tree pass-3 landing.

## 8. PRIOR ART (citations)

- **regex-automata (Rust, BurntSushi)**: the memchr/rare-byte prefilter ladder + `is_one_pass` DFA
  — the `Prefilter::from_dfa` start-row idiom the engine vendors; the "one regex, many search
  strategies behind a cost gate" doctrine 5D mirrors at leaf grain.
- **Teddy (Hyperscan) / shufti**: nibble-LUT multi-byte membership (`vqtbl1q_u8`) — the wide-entry
  tier for class-loops whose first-set exceeds `memchr3`; `SimdMode::ByteClass` is a shufti at leaf
  scale.
- **tree-sitter**: subtree lexer where token regexes compile to a shared DFA the parser drives — the
  counterexample to a per-leaf engine; 5D keeps leaves in the grammar IR and borrows the engine only
  for the search residue.
- **GHC / rustc maximal-munch lexing**: FOLLOW-disjointness as the soundness side-condition for
  greedy runs — the Q7 predicate is the classical LL(1) leaf condition; the `Seq[Repeat,guard]`
  fusion is the standard "longest-match with a stop set" rescue.
- **egg / equality saturation**: the canonicalization the engine's `egraph/` applies to the HIR
  before automata (Q10); 5D consumes its output, does not re-home it.

## 9. NAMED RISKS & COUNTEREXAMPLES

- **The DFA residue is NON-EMPTY (7/16 measured) — the DFA HAS a generator-path consumer.** This
  overturns pass-2's "DFA has no codegen consumer, wiring it violates Y.13". On real
  backtracking-prone leaves the `Seq[Repeat,FOLLOW-guard]` fusion is insufficient; a give-back /
  `Dfa::find` tier survives. The wired-grammar fraction (JSON/Sheets/CSS) must confirm the residue is
  non-empty there too before the DFA is admitted to codegen — if the WIRED residue IS empty, the DFA
  stays a standalone-API-only consumer and Y.13 forbids wiring it. This split is the single decision
  the wired-fraction re-run settles.
- **Over-consumption is a LIVE bug, proven (12/20).** Not hypothetical — `match_leaf` mis-parses
  delimiter-free overlap today. Q7(b) re-lowering is therefore a correctness fix that must land, not
  an optimization to weigh.
- **Negated / Unknown class masks into a wildcard no-op.** `[^"]*` string-body leaves bail to EMPTY
  first-set (`regex_first.rs:167`), degenerating to the `SpanKind::String` runtime scanner. The
  `SimdMode::ByteClass` skip must fire on POSITIVE classes only and must NOT claim to prefilter a
  negated body — the recognizer derivation must gate on class polarity.
- **Triplication drift is already live** (§4) — collapse must assert three-way byte-identity first.
- **`->` inside a class has no lawful lowering** (§5) — if LOCK-1 rules projections may nest inside a
  class, the barrier/edge duality breaks and the class program is no longer projection-free.

## 10. WHAT A PROTOTYPE MUST TEST NEXT TO MOVE 5D

1. **Re-run the Q7 fraction over the WIRED JSON/Sheets/CSS leaves** (not the 16-pattern sample) —
   the number that decides whether the DFA residue has a real generator-path consumer or stays
   standalone-API-only. This is now the single most load-bearing open number.
2. **Wire `derive_recognizers` (`passes/src/lib.rs:338`) onto the FOLLOW fixed point** so
   non-disjoint leaves actually receive `ByteClass` instead of the masked `RegexProgram` — the one
   designed-not-wired seam; the substrate and closure both already exist and compile.
3. **The cold-paired `match_leaf` vs `Dfa::find` on the residue** (Q8.3) + **capture-leaf →
   `ValueProject` byte-identity vs hand grammar capture** (Q8.4) — the two open asserts.
4. **The §4 triplication collapse**: one `first_of`/`class_program_of` off the engine HIR,
   byte-identical to all three hand parsers on the corpus, then two deleted (LOC delta reported).
5. **The LOCK-1 handshake on `->`** (§5, Q11) — barrier at recognizer, edge at view, regex-internal
   projection forbidden or lifted (recommend forbidden).
