# LOCK-4 · Family 4C — VM-CORE — research (pass 1)

**Charter (4C).** Combinators lower to a compact bytecode VM: one instruction set shared by
Rust and TS, so layout isomorphism holds *by construction*; packrat and diagnostics are VM
facilities; the VM is *measured against the closure core, not assumed*. This report makes the
mechanism concrete, names its substrate and integration seams (path:line), grounds it in prior
art, and states — with the PROVE-09 refutation as binding law — exactly what a prototype must
measure to move this family.

---

## 1. The thesis in one paragraph

A parse-that grammar is today a tree of heap-boxed closures: `Parser<'a,Output>{ parser_fn:
SmallBox<dyn ParserFn+'a, S32> }` (`rust/parse_that/src/parse.rs:61`) in Rust, `Parser<T>{
parser: ParserFunction<T> }` (`typescript/src/parse/parser.ts:24-32`) in TS. Every combinator
step dispatches through a data-dependent indirect call — `this.parser(state)` /
`SmallBox`-vtable — which R4 §6c names *"THE megamorphic IC of the engine; V8 sees many distinct
closure shapes flowing through one indirect call."* **4C replaces the closure tree with a flat
program: `Program{ code: Vec<Inst>, actions: [...], literals: [...] }`, executed by one
interpreter loop whose single dispatch site is a `match`/`switch` over an integer opcode.** The
program is *data* — the same byte layout in both languages — so isomorphism is not maintained by
a linter (6A) or a spec-generator (6B) but is *inherent*: the program crosses rust↔ts the way the
offset-tape already does (PROVE-14 / SPEC §0 #14, byte-exact node-for-node across 320 cold
children). This is the family's distinguishing claim: **isomorphism-by-construction, plus one
monomorphic dispatch site replacing a megamorphic one, plus zero per-combinator allocation.**

---

## 2. The mechanism made concrete

### 2.1 The instruction set (the shared ISA)

Adopt the LPeg parsing-machine ISA (Ierusalimschy 2009; Medeiros & Ierusalimschy 2008 — the
canonical PEG VM, §5) as the control core, extended with parse-that's kernel and value needs. A
minimal, sufficient set (≈16 opcodes; each a fixed-width record `{op:u8, a:u32}` or a small
tagged union):

Control / matching (LPeg-classical):
- `Char c` / `TestChar c,L` — match/peek one byte; `TestChar` is the head-fail guard.
- `Set s` / `TestSet s,L` — match/peek a byte-class (index into `literals`); the head-first
  optimisation LPeg calls *headfail*.
- `Span s` — greedily consume a byte-class run (this is where the NEON scanners live, §3).
- `Str k` — match a literal string (index k).
- `Any n` / `Choice L` / `Jmp L` / `Call L` / `Return` / `Fail` / `FailTwice`.
- `Commit L` / `PartialCommit L` / `BackCommit L` — LPeg's loop/alternation discipline:
  `PartialCommit` updates the top backtrack frame in place (subject pos + capture height) instead
  of pop-then-push, making `many`/`sepBy` a tight two-instruction loop.
- `End`.

parse-that extensions (the delta over stock LPeg):
- `Dfa d` — match a regex leaf by running a compiled DFA (`bbnf-regex::Dfa::find`, PROVE-10 /
  PT-N1, 8.8–29.5×). The regex continuum (LOCK-5) supplies `d`; the VM just calls it. This is the
  clean seam that unifies `leaf.ts` `regex()` and the bespoke engine under one instruction.
- `Action k` — invoke host semantic action `actions[k]` over the current capture window
  (§2.3). This is where user `.map`/`.chain` closures live — the *only* megamorphic residue, and
  it fires **on success paths only**, never on the control/backtrack path.
- `Memo k` / `MemoEnter k` — packrat facility (§2.4).
- `Throw label` / `Label ℓ` — labeled-failure facility for diagnostics/recovery (Maidl et al.
  2016, §5); default failure updates only the farthest-fail register (§2.5).

Machine state (three registers + two stacks — LPeg's shape exactly): program counter `pc`,
subject pointer `sp`, capture-stack height; a **backtrack stack** (frames: either a return `pc`,
or `(alt-pc, saved-sp, saved-capture-height)`), and a **capture stack**. macOS-cold, aarch64.

### 2.2 The compiler (grammar/combinator → program)

Combinators become *emitters*, not closures — classical LPeg codegen:
- `a.then(b)` → `⟦a⟧ ⟦b⟧` (concatenate code).
- `a.or(b)` → `Choice L1; ⟦a⟧; Commit L2; L1: ⟦b⟧; L2:`.
- `a.many()` → `L1: Choice L2; ⟦a⟧; PartialCommit L1; L2:`.
- `a.opt()` → `Choice L1; ⟦a⟧; Commit L1; L1:`.
- `a.not()` / lookahead → `Choice L1; ⟦a⟧; FailTwice; L1:` (and `BackCommit` for positive peek).
- `a.map(f)` → `⟦a⟧ Action(k_f)`; `a.chain(f)` → capture + `Action`.
- Named rule / `lazy()` → `Call L_rule … Return` (dissolves the `Box<dyn LazyParserFn>` at
  `rust/parse_that/src/lazy.rs:21,31` and the `LAZY_PARSER_CACHE` WeakMap at
  `typescript/src/parse/lazy.ts:5` into a plain call-table index).

This emitter is exactly **the terminal consumer of family 4A's static parser graph**: 4A's
analyzable nodes are the pre-linking program; 4C is its emission + runtime. Where the generator
path is grammar-driven (bbnf `->`), the same emitter is fed by the bbnf IR (LOCK-3), so parse-that
combinators and generated parsers share one runtime — a 4D-adjacent unification reached via
bytecode rather than a shared HIR.

### 2.3 Values: the hybrid action model (the load-bearing design decision)

A pure VM **cannot** encode arbitrary user closures (`.map(x => ...)`, `.chain`) as bytecode —
this is the family's central counterexample and it forces the design. Resolution (LPeg's
function-capture model; pest's separation of structure from actions): the program carries a
**side-table `actions: Vec<Box<dyn Fn(Captures)->Value>>`** (Rust) / `actions: Function[]` (TS).
Control flow is bytecode; semantic actions are host closures reached by `Action k`. Consequences:
- The megamorphic indirect call is *narrowed from every combinator step to only action sites*, and
  action sites fire on success, not during backtracking — a real, measurable dispatch reduction
  even though it is not zero.
- Captures accumulate on the capture stack (open-pos / close-pos / value slots), folded by
  `Action` — no per-node `Vec` growth on the control path (contrast the `values.push` hotspots at
  `combinators/methods/sep_by.rs:36,52,100…` and `repetition.rs:28`).
- In Rust the action closures keep the input lifetime `'a`; the **control code is `'static`**.
  This split is why 4C *dissolves the arena/ownership question PT-2 carries*: PROVE-09's honest
  failure — a bump-slab arena for the vtable blocked by `'a` across 86 constructor sites — simply
  ceases to exist, because there are no per-combinator boxes to arena-allocate. One `Program`
  allocation replaces N `SmallBox`es; the `smallbox[nightly]` blocker (`Cargo.toml:97`) and the
  whole `SmallBox<dyn ParserFn>` question (parse.rs:61) evaporate on the control path.

### 2.4 Packrat as a VM facility

Memoization keyed on `(pc, sp)` (the VM analogue of `(rule, position)`): `MemoEnter k` checks
`memo[(k,sp)]`, jumps past on hit; `Memo k` on rule exit stores the result span + capture slice.
Match parse-that's *opt-in, selectively-armed* posture (`PACKRAT_ARMED` latch,
`packrat.ts:156,290`; `getCijKey` float-safe key `packrat.ts:71-100`): the compiler emits
`Memo`/`MemoEnter` only around rules the analysis (4A / e-graph, LOCK-2) marks memo-worthy — LPeg
deliberately omits blanket memoization because it usually *loses* on LL(1)-ish grammars. Left
recursion: the VM must implement bounded left recursion (Medeiros/Warth "grow-the-seed", §5) to
preserve parse-that's existing left-rec support (`HEADS`/`GROWING`, `packrat.ts:133-193`) — a named
prototype risk (§7).

### 2.5 Diagnostics as a VM facility

A single **farthest-fail register** (`sp`, expected-set index) updated on `Fail`; `Throw ℓ`/`Label`
carry labeled failures for error messages and recovery (the LPeg/`T` throw operator; Maidl 2016).
This is strictly cheaper than the closure core's approach and directly retires the fight PROVE-11
waged: PROVE-11 spent its whole lever removing the two eager arrays in `ParserState`
(`state.ts` `suggestions`/`secondarySpans`, verified −60% fused alloc). A VM carries **one register,
zero arrays** on the default path; diagnostics arm by widening the register, not by allocating —
per-parse-scoped by construction, which is exactly what PT-E1 asks for (dissolves the module-global
`diagnosticsEnabled` latch at `utils.ts:6`, `utils.ts:28-49`). A genuine architectural payoff of 4C
over both the closure core and 4B.

---

## 3. The substrate — what already exists to build on

- **The program-as-data crossing.** PROVE-14 (SPEC §0 #14; GN-4.W4) proved a Rust-emitted offset
  tape decodes byte-exact in TS. The VM program is the *same kind of artifact* — a flat typed
  array. Reuse that machinery verbatim for `Program` serialization; the byte-exact oracle becomes
  the isomorphism proof. This is the single most important reuse and it is already certified.
- **The scanners are the primitive instructions.** `Span`/`Set`/`Str`/string-decode lower to the
  existing SIMD kernels, unchanged: `parsers/scan/structural_bitmap.rs`, `quote_parity.rs`,
  `decode.rs` (decode-to-arena), `number_simd.rs`, and `scanners.rs`. The VM **orchestrates** the
  fast kernels; it does not replace them. `Dfa` reuses `bbnf-regex::Dfa::find_at`
  (`rust/regex/src/automata/dfa.rs:116`). LOCK-9's scanner plane and LOCK-5's continuum plug in as
  instruction implementations.
- **The dispatch LUT is already a proto-instruction.** `leaf.ts` `dispatch()` O(1) first-char via
  `Int8Array(128)` (`leaf.ts:100-151`) and the Rust equivalent are exactly LPeg's `TestSet`/headfail
  jump — the VM generalises this one working idea into the whole ISA.
- **The emitter modules become codegen.** `combinators/methods/{sequence,alternation,repetition,
  sep_by,minus,map,recover}.rs` and `parser.ts` methods (`then/or/many/sepBy/…`) are the natural
  homes for the emit rules of §2.2 — same files, closures→emit. `combinators/macros.rs:2,80`
  (`seq!`/`alt!`) already collapse N boxes to one; the VM collapses to *zero*.
- **PaddedView** (`state.rs:57` `INPUT_PAD_BYTES=64`) already guarantees tail-safe stripe loads for
  every `Span`/`Set` instruction.
- **bbnf IR / e-graph / cost model** (LOCK-2/3): the program is a compilation target the existing
  IR can emit; fusion, memo-placement, and prefilter-tier choice become *program transforms*
  (peephole passes over `code`), analyzable because the program is data — the charter's "fusion,
  packrat placement, diagnostics points become graph analyses."

---

## 4. Integration seams (path:line)

- **Rust core to replace/supplement:** `rust/parse_that/src/parse.rs:61,80,97` (the `SmallBox<dyn
  ParserFn>` core + `call`/`call_with_flags_cold`) → `Program` + interpreter loop; `lazy.rs:21,31`
  → `Call`/`Return` + call-table.
- **Rust emitters:** `combinators/methods/*.rs` (emit rules), `leaf.rs:182,203,345` (`TakeUntilScan`
  → `Span`/`Set`), `combinators/macros.rs:2,80`.
- **Rust kernels (instruction bodies):** `parsers/scan/{structural_bitmap,quote_parity,decode,
  number_simd}.rs`, `scanners.rs`; `regex/src/automata/dfa.rs:116` for `Dfa`.
- **TS core:** `typescript/src/parse/parser.ts:24-32` (Parser shape), `:437-478` (`call` flag
  dispatch) → interpreter loop; the megamorphic `this.parser(state)` sites the VM's single
  `switch(op)` supersedes: `parser.ts:53,82,88,131,137` + `leaf.ts:44,49,62,140`; `packrat.ts` →
  `Memo`/`MemoEnter`; `utils.ts:6,28-49` (diagnostics latch) → farthest-fail register.
- **Cross-language oracle:** the PROVE-14 tape decoder path (GN-4.W4) → `Program` (de)serializer.
- **Consumers (gates):** value.js (`dispatch`+59 `all()` sites), latex-paper, keyframes.js — the VM
  must preserve `Parser<T>` public semantics exactly; the program is an internal representation the
  existing API constructs (build-once at grammar construction, per R4 §6e).

---

## 5. Prior art (citations)

- **LPeg parsing machine** — R. Ierusalimschy, *"A Text Pattern-Matching Tool based on Parsing
  Expression Grammars,"* Software: Practice & Experience, 2009 (tech report
  bib-di.inf.puc-rio.br/…/08_08_ierusalimschy.pdf). The canonical combinators→bytecode compiler +
  register/stack machine; source of `Char/Choice/Commit/PartialCommit/BackCommit/Call/Return/Fail/
  FailTwice/Span/Capture` and the headfail (`TestChar`/`TestSet`) optimisation. **Direct model.**
- **S. Medeiros & R. Ierusalimschy, *"A Parsing Machine for PEGs,"* 2008** (inf.puc-rio.br/
  ~roberto/docs/ry08-4.pdf) — the formal operational semantics of the machine; the correctness
  reference for the emit rules in §2.2.
- **pest / pest_vm (Rust)** — pest.rs, github.com/pest-parser/pest. A production PEG that separates
  grammar structure (interpretable `OptimizedExpr`) from user code, with an explicit rewind-able
  parser stack — the closest Rust prior art for the hybrid structure/action split (§2.3); also the
  cautionary datum that pest *codegens* to Rust for speed rather than interpreting, i.e. the VM's
  throughput burden is real (§7).
- **Left recursion / bounded LR** — Medeiros, Mascarenhas, Ierusalimschy, *"Left Recursion in
  Parsing Expression Grammars,"* arXiv:1207.0443; Warth et al. "grow-the-seed." The algorithm the
  `Memo` facility must implement to keep parse-that's left-rec support (§2.4, §7).
- **Labeled failures / error recovery** — Maidl, Mascarenhas, Medeiros, Ierusalimschy, *"Error
  reporting in Parsing Expression Grammars,"* Science of Computer Programming, 2016. The `Throw`/
  labeled-PEG basis for §2.5.
- **Packrat space** — Mizushima, Maeda, Yamaguchi, *"Packrat parsers can handle practical grammars
  in mostly constant space,"* 2010 (the cut operator) — the selective-memo discipline (§2.4).
- **Interpreter dispatch** — Ertl & Gregg, *"The Structure and Performance of Efficient
  Interpreters,"* JILP 2003 (threaded code, indirect vs direct threading). Bears on dispatch cost:
  Rust/TS lack labels-as-values/computed-goto, so the portable form is a monomorphic `match`/
  `switch` loop; tail-call dispatch is the modern alternative (LuaJIT interpreter, CPython 3.11+
  computed-goto, wasm3). Honest note: closure-tree code LLVM/V8 *inlines* can beat a dispatch loop
  it cannot inline across — §7.
- **tree-sitter** — table-driven LR automaton whose parse tables are pure generated data consumed by
  a language-agnostic runtime. The existence proof that a *data* parser artifact crosses languages
  cleanly (supports the §1 isomorphism-by-construction claim), and a contrast: LR tables vs a PEG VM
  program.
- **Also surveyed / not adopted:** Warth's OMeta (object-oriented packrat), CPython PEG (PEP 617 —
  generated recursive descent + full memoization, not a VM), Yedidia's incremental PEG thesis
  (zyedidia.github.io — incremental reparse over an LPeg-style machine; relevant if streaming/
  incremental becomes a goal, cf. LOCK-9D).

---

## 6. Why 4C over its siblings (the family's distinct value)

Stated for the synthesist, without surveying sibling reports: 4C's claims that no other 4-family
can make *by construction* are (i) **isomorphism is inherent** — the program is one byte layout,
proven-crossable via the extant tape machinery, needing neither a linter nor a codegen skeleton;
(ii) **one monomorphic dispatch replaces the megamorphic IC** (R4 §6c) — a specific, measurable V8
hypothesis; (iii) **the arena/ownership question dissolves** — no per-combinator boxes, so PT-2's
`'a`-over-86-sites blocker and the `SmallBox[nightly]` dependency vanish on the control path;
(iv) **diagnostics and packrat are one-register / one-table facilities**, not allocation fights
(retiring the PROVE-11 battleground). Its cost, honestly: an interpretation layer LLVM/V8 cannot
inline through, and a hybrid action model that keeps *some* indirect dispatch. 4C is worth
advancing only if §7's measurements land.

---

## 7. What a prototype MUST test (and the binding refutation law)

**The law (SPEC §0 #09, PROVE-09).** The `portable_simd→NEON` shim's 1.34–1.57× was **REFUTED** by
the verify fleet as two-arm window asymmetry; *"paired interleaving is mandatory even for obvious
wins."* Warm benches are disinformation. Therefore **no 4C throughput claim is admissible without
cold, paired-interleaved measurement**, and the prior probability that "a VM beats the
already-monomorphized + arity-unrolled closure core" is *low* — the honest expectation is parity or
loss on raw Rust throughput, with the win (if any) on TS (megamorphism kill) and on the
non-throughput axes. The prototype must be prepared to *report a negative throughput result and
still advance the family on isomorphism/allocation/diagnostics* — or to retire it.

Concrete prototype (both languages, scratch-copy per R3 §8 / R4; `CARGO_TARGET_DIR` isolated):

1. **Minimal VM** — ~16 opcodes (§2.1), interpreter loop, backtrack+capture stacks — in Rust and
   TS, with identical `Program` layout. Compile the existing JSON grammar
   (`typescript/src/parse/parsers/json.ts`, `rust/parse_that/benches/parse_that/combinator.rs`
   grammar) to a `Program`.
2. **THROUGHPUT, paired-interleaved cold** — VM-JSON vs closure-JSON on `data/json/*` (twitter,
   citm, canada), N≥200, per-round back-to-back, PMU cyc/B where macOS allows, loadavg logged.
   The make-or-break number, honest per lane. Hypothesis to falsify: Rust VM ≈ 0.5–0.9× closures;
   TS VM possibly > 1 via monomorphism.
3. **V8 IC-state probe** — confirm the VM dispatch site is monomorphic vs the closure core's
   megamorphic `this.parser` (the IC probes R4 §6 references / `--trace-ic`). The specific V8 win
   this family exists to test.
4. **Byte-exact program crossing** — emit `Program` in Rust, decode+run in TS, assert identical
   parse over the PROVE-14 corpus. The isomorphism proof.
5. **Allocation** — VM (1 program + 2 reused stacks) vs closure core (N boxes); heap-delta B/op,
   both languages. Expect a large, real reduction (dissolves PT-2 arena + PROVE-11 arrays).
6. **Diagnostics** — farthest-fail register vs `ParserState` arrays; alloc-zero on default path,
   per-parse-scoped when armed (PT-E1 satisfaction).
7. **Left recursion + selective packrat** — smallest grammar exercising `HEADS`/`GROWING` left-rec
   through `Memo` bounded-LR; correctness parity vs `packrat.ts`.

**Named risks / counterexamples.** (a) Dispatch cost dominates tiny fused paths — the arity-unrolled
`fuseAll` at ~18 ns/op (PROVE-11) is a hard Rust baseline a 5-instruction dispatch loop may not
beat. (b) Semantic actions reintroduce megamorphic calls at `Action` sites; only their *frequency*
falls. (c) Bounded left recursion in a VM is subtle and can regress space/time if mis-placed. (d)
The public `Parser<T>` API and the three consumer suites must see *identical* behaviour — the
program is internal; any observable change fails value.js/latex-paper/keyframes gates. (e) pest's
choice to *codegen to Rust rather than interpret* is a standing warning that interpretation may not
pay on Rust throughput — 4C's answer must be the isomorphism/allocation/megamorphism axes, measured,
not raw speed asserted.
