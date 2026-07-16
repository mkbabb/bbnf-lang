# LOCK-9 · Family 9C — SCANNER-AS-DFA (research, pass 1)

## The thesis, exactly

parse-that's hand-written scanner corpus — eight `portable_simd` files
(`scanners.rs` 22 SIMD hits, `parsers/scan/{decode 17, ws_comment 16,
structural_bitmap 13, quoted_simd 7, quote_parity 4}`, `span_parser/scan.rs` 7,
`state.rs` 2) plus the two mixed-arch kernels `number_simd.rs` and
`quote_parity.rs` — are not primitives. Each is a finite-state acceptor that a
regex leaf in the bootstrapped grammar already denotes. The JSON string body is
the acceptor for `"(?:[^"\\]|\\.)*"`; the number span is
`-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?`; whitespace-with-comments is
`(?:\s|/\*.*?\*/)*` (its own `RegexClass::WhitespaceWithBlockComment` variant,
`regex/src/classify.rs:96`, already carries the note "and DFA-compatible
variants"); structural classification is the byte-equivalence-class partition of
the enclosing grammar's terminal alphabet. Family 9C dissolves the hand corpus:
these acceptors are authored once in the bootstrapped regex grammar
(`parse-that/rust/bootstrap/regex.bbnf`), compiled through bbnf-regex's existing
HIR→NFA→DFA→Hopcroft-minimize pipeline, and **lowered to a NEON kernel as a
compiled product**. The hand scanners cease to exist because they are re-derived;
"scanner" becomes a codegen tier, not a source file.

The mechanism is not "call `dfa.find()` everywhere." A dense classification pass
(structural bitmap) has no rare byte to skip, so a naive prefilter is parity, not
a win. 9C's mechanism is a **four-tier lowering ladder**, selected per scan site
from the compiled DFA's own shape by a cost model (the LOCK-2 scan-plan-selection
island). The DFA is the source of truth for every tier's tables; the SIMD kernel
is downstream.

## The lowering ladder (the substrate made concrete)

Every tier reads from the compiled `Dfa` (`regex/src/automata/dfa.rs:45`), whose
fields are already the exact payload each tier needs: `byte_classes:[u8;256]`
(`:52`), `num_classes:u16` (`:54`), `flat_transitions:Vec<StateId>` indexed
`[state*num_classes + class]` (`:50`), `state_count()` (`:105`), `class_count()`
(`:110`). The scalar transition loop `class = byte_classes[b]; state =
flat[state*num_cls + class]` at `dfa.rs:116` (`find_at`) is the semantic oracle
every tier must match byte-for-byte.

**Tier 0 — prefilter (memchr 1/2/3).** For a DFA whose start-state entry set is
1–3 bytes: the leftmost-search skip already built and CONFIRMED (start-state
transition row → `Prefilter::from_dfa` → `Dfa::find`, 8.8–29.5× on prefilterable
leaves, 0.99× controls, compile-neutral, 44/44 tests). 9C inherits this verbatim;
it is the sparse-entry rung of the same ladder.

**Tier 1 — shufti/truffle membership (class-run skip).** For a single-state
self-loop over a byte class (whitespace skip; the `[^"\\]+` string interior; an
identifier run): the DFA collapses to one class-membership test. This is a NEON
`tbl` nibble-LUT — the CONFIRMED shufti/truffle prefilter (skip-heavy ~7.5×/4.7×,
dense-neutral 1.03–1.15×). The membership mask is **the DFA's byte-class table**,
not a hand-tuned constant: `structural_bitmap.rs::classify_16` already takes
`(lo, hi)` nibble tables built by `scanners.rs::build_nibble_luts(targets)`
(`:235`); 9C sources those tables from `Dfa::byte_classes` instead of a
hand-passed `targets` slice.

**Tier 2 — Sheng vectorized DFA (small multi-state, tbl transition).** For a
minimized DFA of ≤16 states and ≤16 classes (JSON string: interior/after-escape
≈3 states, {quote,backslash,other}=3 classes; number span after minimization ≈
6–8 states, {sign,`0`,`1-9`,`.`,`eE`,`+-`,other} ≈ 7 classes): the flat
transition table fits in NEON registers and the per-byte transition becomes a
`vqtbl1q_u8` (≤16 state×class) or `vqtbl4q_u8` (≤64 entries) lookup — the
Hyperscan "Sheng" idiom. This is the genuine novum of 9C: **a vectorized DFA
scanner generated from a grammar leaf**, not a hand loop. bbnf-simd's
`aarch64/classify_tbl4.rs` already commits to the `vqtbl4q_u8` / `uint8x16x4_t`
64-entry lookup form (`classify_chunk_from_table`), so the tbl4 register budget is
proven present; the number-span DFA's ~56-entry table lands inside it.

**Tier 3 — simdjson structural (prefix-XOR quote parity).** For the "delimited by
a quote with backslash escapes" leaf shape — detected as
`RegexClass::QuotedString{quote_char, allows_escapes, allows_u_escapes}`
(`classify.rs:73`) — the branchless simdjson stage-1 product: classify quote and
backslash bitmaps, `prefix_xor` the escaped-quote mask to a string-region bitmap.
This is `quote_parity.rs::{compute_in_string_bitmap, prefix_xor}`. 9C's move: the
kernel is a **template instantiated with the leaf's `quote_char`/`escape` byte as
compile-time immediates** — exactly the CONFIRMED emit-spine residualization
(const-fold the delimiter mask; CSS cyc/B 0.80–0.91×, mask-as-immediate
disasm-verified). One template, two products (JSON `"`, CSS `'`/`"`), zero
hand-written duplicates.

The tier is chosen by a **plan predicate over the DFA**: entry-set size → Tier 0;
`state_count()==1` self-loop → Tier 1; `state_count()≤16 ∧ class_count()≤16` (or
≤64 for tbl4) → Tier 2; quote-delimited-with-escape leaf shape → Tier 3;
otherwise fall to the scalar `find_at` transition loop (always correct, never
absent). This predicate is the LOCK-2 e-graph "scan-plan selection" island's
extraction objective; 9C supplies its variables and legality constraints, LOCK-2
supplies the cost lattice.

## The kernel-sharing verdict (the charter's named question)

bbnf-simd is **the lowering backend for Tiers 1–3**, and this is 9C's answer to
"one kernel home vs. native rebirth." bbnf-simd already ships the stable-NEON,
grammar-parameterized classifier the lowering needs:
`aarch64/classify_tbl4.rs::classify_block_from_table(ptr, table, terminator,
escape, control_limit) -> ClassifyResult{structural,quote,backslash,control}` and
the movemask that PROVE-09's refutation demands —
`aarch64/movemask.rs::movemask_u8x16` uses `vshrn_n_u16::<4>` (the narrow idiom),
**never** the double-`vaddv` reduction that the refuted `portable_simd` shim
emitted (2271 `addv.8b` vs baseline 509). Lowering a scanner *to bbnf-simd's
classify kernel therefore fixes the movemask defect by construction* — the
generated code inherits the vshrn-narrow path and cannot regress to the
double-addv movemask that killed PROVE-09's speed claim.

The distinction that keeps 9C its own architectural center: 9C's organizing law is
that the **derivation** (grammar leaf → bbnf-regex DFA → byte-class + transition
tables → kernel template) is the architecture; the kernel is the downstream
target. parse-that's scanners become *generated consumers* of one lowering, and
the physical kernel-isomorphism between the repos is a **consequence** of both
lowering to the same bbnf-simd classify primitive — reached through the DFA, not
by extracting a kernel library as an independent act.

## Integration seams (path:line where extant code already serves)

- **DFA payload for all tiers** — `parse-that/rust/regex/src/automata/dfa.rs`:
  `Dfa` fields `:50/:52/:54`, `state_count` `:105`, `class_count` `:110`, the
  scalar oracle `find_at` `:116`, `find_at_shortest` `:143`; class partition from
  `sets/equiv.rs::compute_byte_classes` (`dfa.rs:10,:80`), representatives
  `:174`. Tier 0 already wired: `automata/prefilter.rs` + `Dfa::find`.
- **Template selection** — `regex/src/classify.rs:55` `RegexClass` with
  `Numeric`(`:58`), `QuotedString`(`:73`), `HexDigits`, `Identifier`(`:87`),
  `WhitespaceWithBlockComment`(`:96`), `CharClassQuantified`, `PrefixThenClass`
  (`:109`), `AccelDriven`; `canonical_pattern()` (`:132`) already exists so
  IR-side recognizer config consults the class rather than embedding literals —
  the exact seam 9C extends from "pick a hand scanner" to "pick a lowering tier."
  Drivers: `classify_regex`(`:157`), `classify_regex_from_hir`(`:177`),
  `RegexInfo`/`QuantifiedClassInfo`/`accel_candidate` in `regex/src/info/`.
- **Lowering backend (kernel)** — `bbnf-simd/src/aarch64/classify_tbl4.rs`
  (`build_lo6_table`, `classify_block_from_table`), `aarch64/movemask.rs`
  (vshrn-narrow), `aarch64/structural_classify_64.rs`, `dispatch.rs::select_classifier`,
  `lib.rs::StructuralAlphabet::class_table()` (`[u8;256]`→`[u8;256]` — the
  alphabet→table bridge the DFA `byte_classes` feeds), `SimdClassifier`.
- **The scanners 9C deletes** (behavior re-derived): `parse_that/src/scanners.rs`
  (`find_first_of_nibble_lut` `:262`, `build_nibble_luts` `:235`, `cached_dfa`
  `:55` — already caches a `Dfa`!), `parsers/scan/structural_bitmap.rs`
  (`classify_16` `:36`, `classify_stripe` `:50`, `find_next_structural_from`),
  `parsers/scan/ws_comment.rs` (`classify_ws_slash_16` `:72`,
  `scan_ws_block_comments` `:98`), `parsers/scan/quote_parity.rs`
  (`compute_in_string_bitmap` `:121`, `prefix_xor` `:41`),
  `parsers/scan/quoted_simd.rs` (`odd_parity_backslashes`),
  `parsers/scan/number_simd.rs` (span-find `count_leading_digits_neon` `:31`
  survives as a Tier-1 product; `accumulate_digits_neon` `:70` is REFUTED, below).
- **Codegen home** — the emit spine that must gain the tier lowering:
  `bbnf-lang/skinny/crates/passes/src/lib.rs` (the recognizer-derivation pass)
  and `crates/ir/src/lib.rs`; post-GN-1 this is `crates/codegen/src/emit/regex/`.
- **Grammar source (constitution clause i)** — `bootstrap/regex.bbnf` (the
  bootstrapped regex grammar; leaf rules `literal`, `escape`, `char_class`,
  `class_item`), regenerated via `scripts/bootstrap-regex.sh`.

## Prior art (citations)

- **Hyperscan** (Wang, Hu, Yang et al., *NSDI 2019*, "Hyperscan: A Fast
  Multi-pattern Regex Matcher"): the **Sheng** engine (≤16-state DFA as one
  SSSE3 `pshufb` / NEON `tbl` state-transition), **shufti/truffle** (character-class
  acceleration via nibble-LUT `pshufb`), **FDR/Teddy** (literal buckets). Sheng is
  the direct antecedent of Tier 2; shufti of Tier 1.
- **simdjson** (Langdale & Lemire, *VLDB Journal 2019*, "Parsing Gigabytes of JSON
  per Second"): stage-1 structural classification + the `clmul` prefix-XOR
  quote-parity for string-region masking — Tier 3's exact algorithm; the
  `vmull_p64` aarch64 carry-less-multiply path already in `quote_parity.rs:51-56`.
- **rust `regex-automata`** (Gallant): byte **equivalence classes** (bbnf-regex's
  `compute_byte_classes` is the same construction), dense/sparse DFA layouts,
  the **acceleration** and **prefilter ladder** (memchr1/2/3 → Teddy) — Tier 0's
  lineage and the `accel.rs`/`prefilter.rs` vocabulary.
- **Thompson NFA + Hopcroft minimization**: already implemented
  (`automata/nfa.rs`, `dfa.rs::hopcroft_minimize`); minimization is what makes the
  Sheng ≤16-state budget reachable and must run before the tier census.
- **Sheng2 / Google `re2` bitstate**, and **MLIR/LLVM `switch`-to-jumptable**
  lowering as the scalar-fallback codegen reference for DFAs that exceed the tbl4
  register budget.
- **SPEC §0 refutations as binding law**: 09 (portable_simd→NEON shim perf claim
  REFUTED — measured no-regression only, vshrn narrow, never double-addv); 02
  (Mula/Lemire in-vector digit accumulation NO_GO — latency-bound CPI inversion);
  04 (multi-block software pipelining refuted as a lever). LOCK-2's CSP tier
  selection may draw on MiniZinc/Z3 formulations, but only as the extraction
  engine — no new solver is 9C's to build.

## Named risks and counterexamples

1. **The dense-classification no-win (the family-killer).** Structural bitmap
   classifies every byte; there is nothing to skip. A DFA-lowered dense classify
   is at best PARITY with hand simdjson (both do one `tbl`+movemask per 16 bytes;
   confirmed dense-neutral 1.03–1.15× for shufti). 9C's value on dense passes is
   therefore **not speed** — it is (a) deletion of the hand corpus, (b)
   grammar-specialization from one template (JSON vs CSS structural sets are
   const-folded products, not two files; SPEC §0/12), (c) stable-NEON by
   construction (no `portable_simd` left → the PT-2 de-nightly blocker dissolves
   at its source), (d) the vshrn-narrow movemask inherited free. The gate on dense
   passes is the PROVE-09 law: **measured no-regression (≥0.98× paired, two
   windows, opposite arm order)**, never a speed claim. If a lowered dense
   classifier regresses, 9C degrades to "Tiers 0–2 only, dense stays hand-written"
   — a partial that must be named, not hidden.
2. **Sheng state/class explosion.** A leaf whose minimized DFA exceeds 16 states
   or 64 state×class entries (e.g. a rich CSS `url()` or a wide unicode class)
   falls out of Tier 2 into the scalar `find_at` transition loop. The number DFA
   is the boundary case — census it first (PROVE-9C-A). The fallback is always
   correct (it is `find_at`), so this is a performance-tier miss, not a
   correctness hole.
3. **The number scanner must not accumulate in-vector (SPEC §0/02).**
   `number_simd.rs::accumulate_digits_neon` (`:70`) is the REFUTED lever. 9C's
   number product uses the DFA only to find the **span** (Tier 1 digit-class
   membership / Tier 2 Sheng), then materializes via scalar SWAR / Eisel–Lemire —
   the DFA replaces span-finding, never value accumulation.
4. **String-body escapes and `\uXXXX`.** The quoted-string Sheng DFA handles the
   backslash-escape state (2–3 states), but `\uXXXX` codepoint materialization is
   a scalar/`vqtbl` decode tail, not part of the acceptor — bbnf-simd already
   ships `aarch64/unescape_uxxxx.rs` (214 LOC) for exactly this. The DFA finds the
   string extent and escape positions; decode is a separate, existing kernel.
5. **Two grammar-parameterized classifiers already exist and disagree in
   idiom.** parse-that's `structural_bitmap.rs::classify_16` (portable_simd
   `swizzle_dyn` nibble-LUT + `to_bitmask`) and bbnf-simd's `classify_tbl4`
   (`vqtbl4q_u8` + vshrn movemask) are two implementations of one idea. 9C must
   pick one lowering backend (bbnf-simd, for the law-compliant movemask) and prove
   the collapse loses no test — else the "dissolution" merely adds a third path.
6. **Bootstrap/regen coupling.** The class tables come from `bootstrap/regex.bbnf`
   through the bbnf-lang generator patch path; a repoint or stale regen yields a
   table that no longer matches the leaf, silently miscompiling a scanner. The
   fix must ride the regen fixpoint gate (regen∘regen byte-identical), never a
   hand-patched table.

## What a prototype must test to move family 9C

- **PROVE-9C-A — the Sheng feasibility census.** Compile the number / string / ws
  / identifier / hex leaves of the five grammars (json, css, csv, plus the two
  authored) through bbnf-regex `Dfa::compile_with{minimize:true}`; tabulate
  `state_count()` × `class_count()` per leaf. Gate: report which leaves fit Tier 2
  (≤16×16 or ≤64 entries) vs. fall to scalar. This is the go/no-go for the
  vectorized-DFA rung — cheap, deterministic, no bench.
- **PROVE-9C-B — the Sheng scanner, paired.** Lower one minimized leaf DFA (JSON
  string body) to a `vqtbl4q_u8` transition kernel; run it paired against
  bbnf-regex's scalar `find_at` over a string-heavy corpus, cold N≥200, two
  windows. Gate: **no-regression on dense (≥0.98×)** AND a skip win on the
  `[^"\\]+` interior run (Tier-1 fusion). `otool -tv` must show `vqtbl` +
  vshrn-narrow, never double-`vaddv` (the PROVE-09 counterexample).
- **PROVE-9C-C — structural bitmap re-derived.** Replace
  `structural_bitmap.rs::classify_16`'s hand `(lo,hi)` LUTs with tables sourced
  from a compiled JSON-structural DFA's `byte_classes`, lowered through
  bbnf-simd `classify_tbl4`. Paired vs the pristine hand `portable_simd`
  classifier. Gate: ≥0.98× (dense-neutral), identical structural-position stream
  on the JSON corpus (parity oracle = `find_at`), zero `portable_simd` tokens in
  the generated kernel.
- **PROVE-9C-D — the deletion census.** Prove behavioral coverage: the eight
  hand scanners' outputs equal the N compiled products over the JSON+CSS corpora
  (differential oracle against the scalar DFA and against the current hand
  scanners). Gate: byte-identical scan streams; the count of deleted hand-SIMD
  files; a named residue list for any leaf that stays hand-written (Tier miss),
  so the family's coverage is a measured percentage, not an assertion.
