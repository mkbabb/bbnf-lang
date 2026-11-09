# 32 — Novelty / Best-in-Class Adjudication: The Prior-Art Teardown

Owns dimension (12), orphaned by the 28-report forest (`30-completeness-critic.md:32,75-105`).
The campaign's entire justification is the >SOTA claim plus the "fully generalized parser
generator" precept; no sibling report tests whether the THESIS is novel against the
established lineage. This report supplies the prior-art teardown, states the novelty claim
at its TRUE (narrow) scope, separates "best-in-class TODAY" from "on track to be," and ends
with the defensible statement, the claims we must NOT make, and the proof artifacts a
publishable claim requires. External claims carry citations; bbnf claims are file:line at
HEAD 36ca10646.

---

## 1. The two axes the thesis spans

The novelty claim lives at the intersection of two independent lineages that have never been
joined in a shipped system:

- **Axis P (parser generation).** Take a declarative grammar, emit a specialized parser.
  Lineage: Yacc/Bison, ANTLR, tree-sitter, pest, lalrpop, ragel, Parabix.
- **Axis V (vectorized structural parsing + zero-copy value plane).** Hand-written SIMD
  structural classification + a flat tape / zero-copy value access. Lineage: simdjson,
  sonic-rs, yyjson, simdcsv, Mison/Sparser/Pikkr, FlatBuffers/Cap'n Proto.

Every prior system sits on ONE axis. The thesis is their product: *a declarative grammar
that EMITS the Axis-V machinery generically.* The teardown below is organized to find any
prior system that crosses the seam, because that crossing — not SIMD, not codegen, not
zero-copy individually — is the only place a novelty claim can stand.

---

## 2. Axis P teardown — parser generators (what they do NOT do)

| System | What it does | What it does NOT do |
|---|---|---|
| **Yacc/Bison, LALRPOP** | LALR(1) table + reduce-action codegen; LALRPOP emits Rust AST structs [lib.rs/parsing] | Scalar table walk byte-at-a-time; zero SIMD; no structural pre-classification; no lazy value plane |
| **ANTLR4** | ALL(*) adaptive-LL prediction; per-grammar parser in Java/C++/Rust | Scalar; no vectorization; builds an eager heap parse tree; no zero-copy tape |
| **pest** | PEG → recursive-descent Rust; pairs/iterators over spans | Scalar; pest's own benches show nom beats it [blog.logrocket.com pest-peg]; no SIMD |
| **tree-sitter** | Incremental GLR; emits a giant `parser.c` lexer + 2-D parse table; external scanners are *hand-written C* [deepwiki tree-sitter parser-engine; Wikipedia tree-sitter] | Table-driven scalar lexer; SIMD appears NOWHERE in the generated `parser.c`; the "external scanner" escape hatch is hand-coded C, not generated SIMD; concrete syntax tree is heap nodes |
| **ragel** | Regex/statechart → FSM; table-driven OR directly-executable goto-machine; targets C/C++/**asm** [colm.net/ragel; Wikipedia Ragel] | The CLOSEST Axis-P ancestor to "grammar → asm," yet still **byte-at-a-time**: a directly-executed goto FSM consumes one byte per state transition. No block-wide SIMD classification anywhere in Ragel's output. asm target ≠ vectorized. |

**The Axis-P verdict.** "Generate a specialized parser per grammar from a declarative file"
is a fully solved, decades-old idea — pest and ANTLR embody it without novelty. ragel even
reaches asm output. But **not one Axis-P generator emits SIMD structural classification**:
they all walk bytes scalar, one transition at a time. ragel's asm is scalar-goto asm, not
vector asm. The generated-codegen half of the thesis is, alone, not novel.

---

## 3. Axis V teardown — vectorized parsers + zero-copy (what they do NOT do)

| System | What it does | What it does NOT do |
|---|---|---|
| **simdjson** | Hand-written stage-1 (block-wide structural classify: lo×hi nibble two-LUT, escape-carry, prefix-XOR) + stage-2 flat tape; On-Demand lazy iterator [Langdale & Lemire arXiv:1902.08318; ondemand_design.md]. 4.3 (Feb 2026) added a NEON unescape kernel [phoronix.com/news/simdjson-4.3] | **Hand-written for JSON ONLY.** No grammar input; the structural alphabet `{}[],:"` is baked into C++ source. Re-targeting to CSS/Sheets/CSV means re-writing the kernels by hand. No grammar, no generator, no IR. |
| **sonic-rs / simd-json** | Rust ports/derivatives of simdjson; sonic-rs adds targeted-SIMD leaves + direct-to-struct, rejecting the two-stage tape [github.com/cloudwego/sonic-rs] | JSON-only, hand-written. sonic-rs's no-tape direct deserialize is a different *architecture*, not a generator. |
| **yyjson** | Scalar-ILP DOM, still SOTA on selected corpora; contiguous value array | No SIMD, no grammar, JSON-only. Refutation anchor (`20-sota-gap-refresh.md:91-97`), not a generator. |
| **simdcsv / xsv lineage** | Hand-written SIMD CSV (quote/comma masks) | CSV-only, hand-written, no grammar abstraction. |
| **Mison (VLDB'17) / Sparser / Pikkr** | Speculative SIMD JSON: build a structural index with SIMD char-class masks, then *jump directly to queried fields*; Pikkr learns field locations [microsoft.com mison-vldb17; github.com/pikkr/pikkr; crates.io/pikkr] | **JSON-only and query-driven.** Mison/Pikkr need a *query* (which fields) to speculate; the structural index is hand-coded for JSON's grammar. No declarative grammar input; not a generator; not general. |
| **FlatBuffers / Cap'n Proto** | Zero-copy access without a parse step: vtable / fixed-offset field access over an arena-built contiguous buffer [flatbuffers.dev/white_paper; capnproto.org] | This is the *value-plane* ancestor (random field access without copy), but over a **self-describing binary wire format the producer wrote**, NOT over arbitrary text in a third-party grammar. There is no parser and no SIMD; the "no parse step" holds only because the writer already laid out the buffer. |

**The Axis-V verdict.** Every vectorized parser is **hand-written for a single format**.
The zero-copy value-plane systems (FlatBuffers/Cap'n Proto) achieve "access without parsing"
only by controlling the wire format. No Axis-V system accepts a grammar; the SIMD kernels are
bespoke C/Rust per format. simdjson 4.3 is the live frontier and it is C++, JSON-only,
hand-tuned (`20-sota-gap-refresh.md:57-67`).

---

## 4. The seam — Parabix, the one genuine ancestor

Only one prior project sits ON the seam between Axis P and Axis V: **Parabix** (Cameron, Lin
et al., SFU). Parabix transforms byte-oriented text into 8 parallel bit streams and operates
on them with wide SIMD via the **Pablo** language, which compiles to LLVM IR
[coursys.sfu.ca ParabixProject; balisage.net Cameron]. icXML accelerated Xerces-C XML
parsing with it; icgrep is a Parabix-generated regex engine. This is the closest published
relative: SIMD parsing driven by a higher-level description, with an IR (Pablo→LLVM).

What Parabix does NOT do, verified:

- **The BNF/EBNF parser generator is an aspirational research direction, not a shipped
  artifact.** The Parabix project page lists "Design and build Parabix parser generators for
  BNF/EBNF" and "Design extended grammar languages for transducers" as *future objectives*
  under a course-project page, not delivered features [coursys.sfu.ca ParabixProject, fetched
  June 2026: "the capability is planned but not currently delivered"]. Shipped Parabix targets
  **regular expressions and XML**, via Pablo, not general declarative CFG/PEG grammars.
- **The IR is parallel-bitstream-specific (Pablo), not backend-agnostic.** Pablo compiles to
  LLVM IR for a bitstream machine; it does not model a backend-agnostic structural-classify
  IR that a Rust+asm specializing emitter consumes (the dav1d-style target,
  `19-dav1d-emitter-design.md`).
- **No typed, grammar-derived value plane.** Parabix produces parse events / marker
  bitstreams; it has no lazy/eager unified Value API over a SoA tape, no `->` typed
  projection, no direct-to-struct bridge.
- **Bitstream transposition, not block classification.** Parabix's model is "one bit per
  byte, transpose to 8 streams, do bitwise logic" — the inductive-doubling SWAR lineage. The
  bbnf model is simdjson-style 64-byte block classification into per-kind masks
  (`structural_classify_64.rs:98`). Related family, different mechanism.

So even the one ancestor that crosses the seam (a) never shipped the general grammar→SIMD
generator, (b) is regex/XML-scoped, (c) uses a bitstream IR, not a backend-agnostic
classify-IR feeding a Rust+asm emitter, and (d) has no typed value plane. The seam is
*genuinely* sparse.

---

## 5. Where bbnf-lang differs — the precise differentiator

Folding §§2–4 against the trees at HEAD, the genuine difference is a **conjunction**, none of
whose conjuncts is individually novel but whose product is unattested:

1. **Declarative grammar input** (`.bbnf` + workspace metadata, Lock 14) — like pest/ANTLR,
   unlike simdjson.
2. **emits block-wide SIMD structural classification** — the grammar's alphabet drives a
   generated NEON classifier (`structural_classify_64.rs:98`, `KindPartition` is caller data
   `runtime_scan.rs:76`; full-tree `neon.rs:47` data-driven shape select) — like simdjson's
   kernel, but *generated from the grammar*, unlike every Axis-P generator (all scalar) and
   every Axis-V parser (all hand-written-per-format).
3. **with a backend-agnostic IR** the emitter specializes (the dav1d Rust+asm target,
   `19-dav1d-emitter-design.md`; backend-agnostic `TypeDesc::Named`) — unlike Parabix's
   bitstream-specific Pablo IR.
4. **and a typed, grammar-derived value plane** — `->` projections derive a lazy `ValueRef`
   over one SoA tape with eager-as-fold (`22-value-api-design.md` Axis A/B/C;
   `generated.rs:316` `ValueRef::new`, `:247` lazy `typed_value`) — the FlatBuffers
   zero-copy-access idea, but over *arbitrary parsed text in a declared grammar*, not a
   producer-controlled wire format.

No prior system holds all four. That conjunction is the novelty, and it is narrow.

---

## 6. Does ANY prior system emit grammar-specialized SIMD structural classification?

Direct answer to the critic's pivot question: **No shipped system emits grammar-specialized
SIMD structural classification from a declarative grammar.**

- Axis-P generators (tree-sitter, pest, ANTLR, lalrpop, ragel): generate scalar parsers;
  SIMD absent from all generated output. ragel reaches asm but byte-at-a-time goto-FSM, not
  vector classification.
- Axis-V parsers (simdjson, sonic-rs, yyjson, simdcsv, Mison/Pikkr): emit SIMD but
  hand-written per format, no grammar input, no generator.
- Parabix: the closest, but its general-grammar generator is unshipped, and its delivered
  scope is regex/XML over a bitstream IR — not block-classify NEON from a CFG/PEG with a
  typed value plane.

The negative is well-supported. A 2024–2026 sweep for "parser generator + automatic SIMD
vectorization + declarative grammar" surfaces only LLM-vectorization and loop-autovec work
[arxiv 2605.17978 AutoVecCoder; arxiv 2507.15224 SimdBench], nothing grammar-driven. The
seam is empty in the published record.

---

## 7. Best-in-class TODAY vs on track to be — the honest split

The novelty CLAIM is defensible; the best-in-class CLAIM is not yet, and the gap is large.
Per the sibling evidence:

- **CSS >SOTA is directional-only under load.** Every committed `track1_rich` figure is
  load-depressed; no quiet-clean (loadavg < 1.0) absolute Mbps exists in any document
  (`14-bench-honesty.md:83-87`). The ratio (1.5–3.3×) is same-run load-fair but the absolute
  claim is uncertifiable. Worse, the win is *partly* a depth asymmetry: `track1_rich`
  CLASSIFIES the leading value token and counts selector commas; it does NOT decode the full
  typed tree lightningcss builds (`20-sota-gap-refresh.md:99-110`; `CssTypedValue::classify`
  is head-level only). The CSS advantage is over a *shallower materialization*, disclosed in
  FRAMING.md but absent at the primary claim sites.

- **JSON is skinny-only and plane-conflated.** The headline +164.7% lives on the
  `parse_only` recognizer-vs-recognizer plane; typed planes are +5–20%
  (`14-bench-honesty.md:148-150,193-206`). JSON numbers decode *through* the serde_json
  comparator (`view.rs:232`), so "beats serde_json on numbers" is structurally impossible
  (`20-sota-gap-refresh.md:118-123,188`). The most advanced JSON SIMD scanner is **dead on
  the live path** behind a `GrowOnly` capacity gate (`18-asm-simd-state.md:110-126`) — the
  JSON-beats-sonic claim is delivered by the *scalar* tape walk, not the SIMD investment.

- **5 of 9 grammars have no correctness oracle.** CSV, Math, BNF, EBNF, CSS-Pretty have
  admission-smoke only (`parse().is_ok()` + `doc.input()==source`); each can emit
  structurally wrong output and pass (`15-oracle-census.md:133-139,159-172`). Sheets and BBNF
  oracles are self-referential. Only JSON (vs sonic-rs/serde_json) and CSS (vs
  cssparser/lightningcss) have independent comparators. "Best-in-class" is therefore a claim
  about TWO grammars under a disclosed thermal/depth asymmetry, not a corpus-wide property.

- **The generator thesis has not SHIPPED in the totality tree.** The backend-agnostic IR's
  cost model is theatre (`24`), type inference does not exist (`21`), the unified Value API is
  three substrates + four path APIs + a dead deserializer (`22`), and the SIMD kernels are
  hand-wired into per-grammar runtime modules, not composed by the emitter
  (`19-dav1d-emitter-design.md:21-46`). The thesis is proven in skinny for JSON/CSS and
  *un-folded* in crates/. An unshipped generalization is not best-in-class at anything.

Verdict: **ON TRACK, not best-in-class.** The architecture is novel; the demonstration is two
grammars, directional, depth-asymmetric, and the generalization is unbuilt.

---

## 8. The defensible novelty statement (one paragraph)

> bbnf-lang is, to the best of the published record, the first parser generator to emit
> *grammar-derived block-wide SIMD structural classification* — a generated NEON stage-1
> classifier whose byte alphabet and per-kind partition are mechanically derived from a
> declarative `.bbnf` grammar — coupled to a backend-agnostic intermediate representation that
> a specializing Rust+asm emitter lowers, and a typed, grammar-derived value plane offering
> unified lazy/eager access over a single zero-copy SoA tape. Where simdjson, sonic-rs, and
> the Mison/Pikkr family hand-write SIMD per format and tree-sitter/pest/ANTLR/ragel generate
> only scalar parsers, and where Parabix's general grammar-to-SIMD generator was scoped but
> never shipped (its delivered work being regex/XML over a parallel-bitstream IR), bbnf-lang
> occupies the previously-empty seam: SIMD parsing *that the grammar generates*, with a typed
> value plane *over text the grammar parses* rather than a producer-controlled wire format.

---

## 9. Claims we must NOT make yet

1. **NOT "best-in-class parser" / "fastest parser generator."** True for two grammars,
   directional, depth-asymmetric; false or unmeasured for the other seven
   (`15-oracle-census.md`; `14-bench-honesty.md`).
2. **NOT "beats simdjson / the 2026 SIMD frontier."** Comparator pins are simd-json 0.13
   (~2023) and lightningcss alpha.71; the live frontier (simdjson 4.3, C++, NEON unescape) is
   NOT in the harness (`20-sota-gap-refresh.md:192-194`). "Beats simd-json 0.13" ≠ "beats the
   frontier."
3. **NOT "beats serde_json on JSON numbers."** We CALL serde_json for number decode
   (`view.rs:232`) — structurally impossible (`20-sota-gap-refresh.md:188`).
4. **NOT "matches lightningcss CSSOM."** Our CSS value plane classifies, does not decode; it
   is a shallower product (`20-sota-gap-refresh.md:99-110`). The speed is partly the shallower
   depth; re-deepening to parity would erode the ratio (`22-value-api-design.md:136-148`).
5. **NOT "the SIMD delivers the JSON win."** The live JSON path is the scalar tape walk; the
   SIMD scanner is dead behind a capacity gate (`18-asm-simd-state.md:110-126`).
6. **NOT "fully generalized" as a present fact.** Lock 14 is green-by-exclusion, RED in
   substance (`09-lock14-firewall.md`; `30-completeness-critic.md:109-133`); grammar names
   leak into generic crates; the generator thesis is un-folded in crates/. Say "fully
   generalized by construction, demonstrated on two grammars, generalization in progress."
7. **NOT "novel SIMD kernels."** The kernels are simdjson/Langdale-Lemire technique
   (escape-carry, prefix-XOR, nibble-LUT) — the novelty is *generating* them from grammar,
   not inventing them. Claiming kernel novelty invites correct refutation.

---

## 10. What would make the novelty claim publishable / defensible — the proof artifacts

1. **Corpus-wide independent oracles.** Wire ≥1 external comparator per grammar (CSV vs the
   `csv` crate; Math vs an evaluator; BNF/EBNF vs a canonical parser; CSS-Pretty vs
   CssL4Parser), field-for-field not count-only (`15-oracle-census.md:176-189`). The novelty is
   "general"; a general claim needs the general corpus green.

2. **One quiet-clean, single-thermal-regime bench table.** A loadavg < 1.0 capture with
   BOTH planes (cold-per-parse for JSON and CSS in one regime), JSON SIMD scanner WIRED LIVE
   (not behind `GrowOnly`), and numbers decoded by an Eisel-Lemire kernel rather than
   serde_json. Until then no absolute cross-domain >SOTA table is publishable
   (`14-bench-honesty.md:258-269`).

3. **The grammar→SIMD generator demonstrated on a NEW grammar end-to-end.** Add a grammar
   never hand-tuned (e.g. TOML or a log format), regenerate, and show the NEON classifier +
   typed value plane fall out of the `.bbnf` with ZERO hand-written kernel or runtime module.
   This is the load-bearing novelty proof: the seam is the *generation*, so the artifact must
   be "new grammar in, SIMD parser out, no human in the kernel loop." Today JSON/CSS scanners
   are hand-wired in `runtime/` (`19-dav1d-emitter-design.md:21-46`) — that must become
   emitter output first.

4. **The backend-agnostic IR lowered to TWO emitters.** Show the same IR producing the Rust
   intrinsics path AND a hand-asm leaf (the dav1d split), proving the IR is genuinely
   backend-agnostic and not a Rust-shaped mono-backend (`27-backend-agnostic-seam.md`;
   `19-dav1d-emitter-design.md` Part 2). A single-backend "agnostic" IR is not publishable as
   agnostic.

5. **The Lock-14 firewall closed structurally**, so "fully generalized" is a proven invariant
   not a contrivance ledger — the roster-driven totality gate extended to all generic crates
   with the codegen grammar-named directories eliminated (`09-lock14-firewall.md:130-171`).

6. **An honest prior-art section in the writeup itself**, naming Parabix as the seam-ancestor
   and conceding it as the nearest relative, naming simdjson as the kernel-technique source,
   and scoping the claim to "generated grammar-derived SIMD classification + typed value
   plane" — not "novel SIMD" or "fastest parser." A claim that over-reaches on §9 is refuted
   on contact; a claim scoped to §8 survives review.

**Bottom line.** The novelty is real and narrow: the empty seam between grammar generation
and vectorized structural parsing, now occupied. Best-in-class is not earned today — it is two
grammars, directional, depth-asymmetric, generalization unshipped. The path to a publishable
claim is the six artifacts above, of which #3 (new-grammar end-to-end with no human in the
kernel loop) is the one that actually proves the thesis the campaign is named for.
