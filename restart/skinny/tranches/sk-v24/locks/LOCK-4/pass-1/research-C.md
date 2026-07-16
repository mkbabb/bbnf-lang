# LOCK-4 / Family 4C — VM-CORE: combinators lower to a compact bytecode parsing machine

*Pass 1 research. Standalone. The family's charter: combinators lower to a compact bytecode
VM — ONE instruction set shared by Rust and TS (layout isomorphism by construction); packrat
and diagnostics become VM facilities; the whole is measured against the closure core, never
assumed to beat it.*

---

## 0. The thesis in one paragraph

A parser combinator, in both parse-that surfaces, is today an opaque closure: a boxed
`dyn ParserFn` in Rust (`rust/parse_that/src/parse.rs:61`), a `.parser` function-pointer field
dispatched through a megamorphic inline cache in TS (`typescript/src/parse/parser.ts:53` and its
kin). The closure is unanalysable — you cannot ask a `Box<dyn Fn>` "are you a `Choice`? what is
your first set? where should a packrat memo sit?" VM-CORE's move is to stop constructing an
opaque call tree and instead **compile the combinator expression into a flat array of tagged
instructions** — `Char`, `Set`, `Choice`, `Commit`, `Call`, `Return`, `Jump`, `Fail`, `Span`,
`Capture` — executed by a small dispatch loop over an explicit *position stack* and *backtrack
(choice-point) stack*, exactly the LPeg parsing machine. Because a flat instruction stream is
**data with a fixed byte layout**, the Rust `Vec<Instr>` and the TS `Int32Array`/struct-of-arrays
hold *the same program*; the interpreter is the only per-language artifact, and it is small.
Isomorphism stops being a mirrored-source discipline (LOCK-6's other families) and becomes a
property of the representation. The load-bearing question — and the one a prototype must answer —
is whether the dispatch loop's cost is acceptable against the inlined-closure core, and the honest
prior expectation, grounded in the campaign's own refuted-speedup lesson, is that a naïve
tree-walking or bytecode interpreter *loses* on throughput and must earn its place on
isomorphism + the facilities (packrat, diagnostics, incrementality) it makes cheap.

---

## 1. The substrate that already exists (the seed is in-tree)

The Rust side already ships **half of this family**, and it was built for exactly the reason
VM-CORE exists. `rust/parse_that/src/span_parser/` (1437 LOC across ten files) is a *vtable-free,
enum-dispatched* second-tier parser:

- `SpanParser<'a>{ kind: SpanKind<'a>, flags: u8, [label] }` — `span_parser/mod.rs:51`.
- `SpanKind<'a>` — `span_parser/mod.rs:56` — a ~30-variant enum that IS a nascent instruction set:
  leaves `StringLiteral(&[u8])`, `CompiledDfa(Arc<Dfa>)`, `AhoCorasickMatch(Arc<AhoCorasick>)`,
  `TakeWhileByte(fn(u8)->bool)`, `TakeUntilAny1/2/3(u8..)`, `TakeUntilAnyLut(Box<[bool;256]>)`,
  `TakeUntilAnySIMD{lo_lut,hi_lut}`, `NextN`, `Epsilon`, `Eof`; combinators `Seq(Vec<..>)`,
  `OneOf(Vec<..>)`, `Many{inner,lo,hi}`, `SepBy/SepByWs{inner,sep,lo,hi}`, `Opt`, `Wrap`, `Skip`,
  `Next`; assertions `Not`, `Minus`, `LookAhead`, `Negate`, `Peek`; and a `Boxed(Box<dyn ParserFn>)`
  escape hatch (`span_parser/mod.rs:131`).
- Dispatch is a single `match &self.kind` jump table — `span_parser/mod.rs:184` (`call_inner`),
  with the flag pre/post handling hoisted into `call`/`call_with_flags_cold`
  (`span_parser/mod.rs:139,155`) sharing the *identical flag vocabulary* as the closure core
  (`FLAG_TRIM_WS`/`FLAG_SAVE_STATE` at `span_parser/mod.rs:10-11` == `parse.rs` flags).

This is a **tree-walking interpreter**: `dispatch_seq` iterates children and recurses via
`p.call(state)?` (`span_parser/combinators.rs:15`); `dispatch_one_of` saves/restores
`state.offset` around each child (`combinators.rs:30`); `dispatch_many` loops with a zero-width
guard (`combinators.rs:52`). It is one match-dispatch per node, still using the *native* call
stack + `?`-propagation for backtracking, and it still holds children behind `Box<SpanParser>`
pointers. It exists because it *already beats* the generic boxed-closure core on the JSON/CSS span
hot path — the codebase's own evidence that removing the vtable and dispatching over a tagged enum
is a win for a monomorphic output type. **VM-CORE is the completion of this trajectory:** linearize
the tree into a flat program, replace native recursion with an explicit backtrack stack, and lift
the same shape to TS.

The regex sub-language is *already* compiled to an automaton — `SpanKind::CompiledDfa(Arc<Dfa>)`
embeds a `bbnf-regex` DFA (`rust/regex/src/automata/dfa.rs:45`) as a single leaf instruction. So
one half of the leaf vocabulary is *already a compiled machine*; VM-CORE makes the combinator
skeleton compile too, unifying "the leaves are automata, the skeleton is a closure tree" into "it
is all one program."

### The asymmetry that this family must confront head-on

The TS side has **no** enum-dispatch analogue. The `*Span` combinator family was *excised* at TS
`1.0.0` and is now guarded against reintroduction by `proof:no-span-surface`
(`typescript/scripts/*` gate; the manifest names it). TS is pure closures: `Parser<T>` class,
`this.parser(state)` megamorphic IC (`parser.ts:53,82,88,131,137`, `leaf.ts:44,49,62,140`), with
the only "instruction-table" being `dispatch()`'s O(1) `Int8Array(128)` first-char table
(`leaf.ts:100-151`). So on the Rust side VM-CORE *extends a live proto-VM*, while on the TS side it
*introduces* one where a prior span-tier was deliberately removed. The family's isomorphism claim
therefore cannot be "mirror the two existing cores" (they diverge); it must be "a *new* shared
instruction layer that both cores compile *into*, leaving each language's public combinator API
untouched on top." The excision is a caution, not a veto: what was removed was a *parallel public
surface* (`*Span` methods) — a maintenance-cost duplication — not an internal compiled backend. VM-
CORE's discipline is that the VM is an *internal lowering target with no public surface*, so it does
not re-open the wound `proof:no-span-surface` closed.

---

## 2. The mechanism made concrete

### 2.1 The instruction set (one, shared)

Adopt the LPeg / parsing-machine instruction core, extended with the two things combinators need
beyond PEG-recognition (values and separators):

| Instr | Operands | Effect (position `sp`, backtrack stack `bt`, value stack `vs`) |
|---|---|---|
| `Char b` | byte | if `src[sp]==b` then `sp+=1` else `Fail` |
| `TestChar b, L` | byte, label | predictive: if `src[sp]!=b` jump `L` (no backtrack push) |
| `Set cls` | class-id | if `src[sp] ∈ class` then `sp+=1` else `Fail` (folds `TakeUntilAny*`/`Lut`/`SIMD`) |
| `Dfa d` | dfa-id | run compiled `bbnf-regex` DFA from `sp`; advance or `Fail` (the existing `CompiledDfa` leaf) |
| `Str s, n` | atom-id, len | memcmp `n` bytes (folds `StringLiteral`) |
| `Any n` / `Eof` | count | advance `n` / assert end (`NextN`, `Eof`) |
| `Choice L` | label | push choice-point `(L, sp, vs.len, bt.len)` onto `bt` |
| `Commit L` | label | pop `bt`, jump `L` (commit to the alternative; the LPeg trick that makes PEG choice O(1) tail) |
| `PartialCommit L` | label | update top `bt.sp` and jump `L` (the LPeg loop instruction — `Many` with no per-iteration choice-point growth) |
| `Call L` / `Return` | label | rule invocation with a return stack (recursion, shared rules) |
| `Jump L` / `Fail` | label / — | unconditional / pop `bt` and resume, or hard-fail at empty `bt` |
| `Span k` | tag | push `Span(mark, sp)` onto `vs` (the Span-tier output) |
| `Capture k` / `Action a` | tag / action-id | run a semantic action (`map`/`chain` parity — §2.4) |

`Choice`/`Commit`/`PartialCommit` are the whole trick: they turn the tree-walker's native-stack
backtracking (`state.offset = cp` in `dispatch_one_of`) into an **explicit choice-point stack**,
and `PartialCommit` turns `dispatch_many`'s loop into a back-edge that does not grow the stack per
iteration. This is what makes the VM analysable and resumable where the closure tree is neither.

### 2.2 The layout that gives isomorphism by construction

The program is a flat operand-encoded stream, not a pointer tree:

- **Rust**: `enum Instr` is `#[repr(...)]`-stable; the program is `Box<[Instr]>` (or a packed
  `Vec<u32>` opcode/operand stream). Constant pools — byte-class tables, `Str` atoms, `Dfa`
  handles, action ids — are `Box<[..]>` side tables indexed by operand. Compile-once into a
  `BumpSlab` (the tree already uses one at `rust/parse_that/src/regex/generated.rs:174`), so the
  whole program is one allocation, not the current per-node `SmallBox::new` (`parse.rs:74`) ×
  86-construction-site scatter.
- **TS**: the *same* opcode/operand stream is an `Int32Array` (or `Uint8Array` for opcodes +
  parallel typed arrays for operands — a struct-of-arrays that V8 keeps unboxed). Constant pools
  are plain arrays. The interpreter is a `while` loop with a `switch (op)` — one monomorphic
  function, not N distinct closure shapes flowing through one megamorphic IC (`parser.ts:53`).

Because both sides decode *the same bytes in the same order*, the isomorphism is a *serialization
format*, verifiable by a single golden-program oracle: compile a grammar to a program in Rust,
serialize, decode in TS, assert byte-identical opcode stream and identical parse over a shared
conformance corpus. This is strictly stronger than LOCK-6's mirrored-source linter families and
strictly cheaper than a wasm-core (the interpreter stays native per language; only the *program*
crosses). It also directly seeds LOCK-6's tape-crossing precedent — the offset-tape already crosses
Rust→TS byte-exact; a VM program is the same kind of portable artifact.

### 2.3 Packrat as a VM facility

TS packrat is today a bolt-on: opt-in Warth deterministic memoization + left-recursion
(`packrat.ts`), keyed by `getCijKey = id*MEMO_OFFSET_SPAN + offset` (`packrat.ts:71-100`), armed
permanently at first `memoize()` (`PACKRAT_ARMED`, `packrat.ts:156,290`), with module-global
`Map` tables (`packrat.ts:133-193`) and epoch save/restore (`packratEnter`/`packratExit`,
`packrat.ts:216-250`). The Rust core has no packrat at all — pure LL backtracking via
`FLAG_SAVE_STATE` (`parse.rs`), the classic isomorphism gap.

In a VM the memo becomes a **first-class instruction pair** around a `Call`: `MemoOpen rule, L` /
`MemoCommit rule` — the interpreter consults a `(pc, sp) → (result, sp')` table on `MemoOpen`,
jumps past the body on a hit, and records on `MemoCommit`. The key is `(pc, sp)`, structurally
identical to `getCijKey`'s `(id, offset)` but now *the same key arithmetic in both languages*
because `pc` and `sp` are machine registers, not language objects. Left-recursion (Warth's seed-
growing) is a bounded re-entry on the *same* `(pc, sp)`, expressible as a VM loop with a growing-
seed flag — the one genuinely subtle facility, and the one whose Rust/TS parity is worth a
dedicated oracle. Crucially, packrat placement becomes an **analysis over the program** (which
`Call` targets are re-entered at the same `sp`?), not a manual `.memoize()` call — the compiler
decides, and both languages inherit the decision from one shared placement pass.

### 2.4 Diagnostics as VM facilities, and the value problem

Diagnostics are machine registers, not object fields. `furthest_offset` (already a
`ParserState` field on both sides, advanced in `dispatch_one_of` at `combinators.rs:33` and in
`mergeErrorState`) becomes a register the interpreter bumps on every `Fail`. The lazy
`expected`/`suggestions`/`secondarySpans` surface — whose zero-alloc-until-needed discipline is a
certified TS win (fields declared `undefined`, populated only under a scoped flag) — maps to VM
side tables written only when a `diagnostics` register is set, so the default (LL(1), diagnostics-
off) instruction path pays nothing, exactly the lazy-field invariant but expressed once for both
languages. A `Label k` pseudo-instruction (compiled from the existing `SpanParser.label` field,
`span_parser/mod.rs`) attaches the "expected X" string to a program point.

**The value problem is the hard part and must be named.** `SpanKind` produces only `Span<'a>`
(offsets). The generic closure `Parser<'a, Output>` produces *arbitrary mapped values* — `.map`,
`.chain`, `.mapState` build user data. A recognition-only VM (LPeg's core) reaches Span parity
trivially; reaching *combinator* parity requires a **value/action facility**: a value stack `vs`
and `Action a` / `Capture k` instructions that invoke registered host closures (`Vec<Box<dyn Fn>>`
in Rust, `Array<Function>` in TS — an *action table* indexed by operand, so the program stays pure
data and the closures live in a side table). This is the seam where VM-CORE meets the generator's
grammar plane: `->` projections *are* the actions, and the union architecture already residualizes
projections into emitted code. The honest scoping: a *Span-tier / recognition* VM is provable and
cheap (it extends `SpanParser` directly); a *fully-generic value-producing* VM is where the
interpreter-overhead risk (§5) bites hardest, because every `Action` is an indirect call the
closure core would have inlined. The prototype must measure *both* tiers separately.

---

## 3. Prior art (with citations)

- **LPeg — the canonical combinators→bytecode parsing machine.** Ierusalimschy, *"A Text
  Pattern-Matching Tool based on Parsing Expression Grammars"* (Softw. Pract. Exper., 2009) and
  Medeiros & Ierusalimschy, *"A Parsing Machine for PEGs"* (PUC-Rio, ry08-4). Each PEG pattern
  compiles at runtime to a program for a virtual parsing machine; instructions `Char/Any/Set/
  Choice/Commit/PartialCommit/Call/Return/Jump/Fail/Span`; patterns compose by concatenating
  programs. The transformation PEG→machine has a **formal model proven correct** — the semantics
  VM-CORE inherits for free. The `Choice`/`Commit`/`PartialCommit` triad is the exact device that
  makes ordered choice and repetition O(1)-tail without stack growth.
  (inf.puc-rio.br/~roberto/lpeg, .../docs/ry08-4.pdf, .../docs/lpeg-primer.pdf)
- **GPeg — a parsing machine with incremental parsing.** Yedidia, *"Incremental PEG Parsing"*
  (Harvard senior thesis) + `github.com/zyedidia/gpeg`. Same LPeg machine, Go, extended with
  **incremental reparse** over an interval tree of memo entries — direct evidence that a VM's
  explicit memo table buys tree-sitter-class incrementality that a closure tree cannot, and a
  concrete design for the packrat-as-VM-facility (§2.3).
- **Parsley / Staged Selective Parser Combinators.** Willis, Wu, Pickering, *"Staged Selective
  Parser Combinators"* (ICFP 2020, PACMPL 4(ICFP):120). Parsley represents combinators as a tree,
  **compiles them to an abstract machine in CPS form**, analyses it, then *partially evaluates the
  machine away* via Typed Template Haskell to emit hand-written-quality code. This is the SOTA
  boundary between families: Parsley's *machine* is VM-CORE's substrate; Parsley's *staging* is
  family 4B's (COMPTIME-FUSION) move — it compiles the machine out at build time. VM-CORE's
  distinct bet is to keep the machine *at runtime* as the shared isomorphic core, and either accept
  a measured throughput cost for that isomorphism or recover it by threaded-code/JIT (below). The
  selective-functor insight — that `<|>`/`branch` expose static structure a monad hides — is also
  why the combinator tree is *compilable at all*; it bounds what the value/action facility (§2.4)
  must be able to express.
- **PyPy RPython JIT for LPeg** (morepypy.blogspot.com, 2019) proves the interpreter-overhead
  objection is not fatal: a meta-tracing JIT over the LPeg VM reaches near-native throughput. In a
  no-JIT Rust/TS setting the analogue is **threaded code / computed-goto**: decode the program once
  into an array of function pointers (Rust) or rely on V8's `switch`-jump-table (TS), removing the
  per-instruction decode branch. This is the concrete path if the naïve VM loses §5's bench.
- **Russ Cox, "Regular Expression Matching: the Virtual Machine Approach"** (swtch.com) and
  `regex-automata`'s `PikeVM`/`meta` — the same choice-point/thread machine at the regex layer;
  `bbnf-regex`'s DFA is the determinized cousin already embedded as a VM leaf. Cross-pollination:
  the combinator VM and the regex VM share the choice-point discipline, which is the mechanical
  content of LOCK-5's "one HIR / lawful functor" seen from the runtime.
- Contrast class (what VM-CORE is *not*): pest and CPython's `pegen` generate *recursive functions*
  (a call tree), not a flat program; tree-sitter generates a *table-driven LR automaton*. VM-CORE
  sits between: flat like tree-sitter's tables, PEG-semantics like pest, runtime-composable like
  LPeg.

---

## 4. Integration seams (path:line)

Where extant code serves the family, in the read-only parse-that tree:

- **The proto-VM to complete** — `rust/parse_that/src/span_parser/mod.rs:56` (`SpanKind` = the
  instruction-set seed), `:184` (`call_inner` = the decode/dispatch match), `combinators.rs:15/30/52`
  (`dispatch_seq`/`one_of`/`many` = the tree-walk arms to linearize into `Seq`-concat /
  `Choice`+`Commit` / `PartialCommit`).
- **The closure core it lowers from / is measured against** — `rust/parse_that/src/parse.rs:61`
  (`Parser{parser_fn: SmallBox<dyn ParserFn,S32>, flags}`), `:41` (`ParserFn` trait), `:74`
  (`SmallBox::new` per-construction alloc the VM's single-program alloc replaces), `:80` (the flag
  fast-path the VM folds into instruction pre/post).
- **The value/action facility's grammar seam** — the generator's `->` projections and their
  residualization on the union path (bbnf-lang side) supply the `Action` table entries; the
  combinator's `map`/`chain`/`mapState` (`typescript/src/parse/parser.ts:146,124,162`) are the TS
  actions.
- **Arena for the single-program allocation** — `rust/parse_that/src/regex/generated.rs:174`
  (existing `BumpSlab` consumer; the sound `'a`-scoped usage template — note the standing lesson
  that a `'static` arena holding `'a`-parameterized closures is *unsound*, so the program's action
  closures must be `'a`-threaded or the program must hold *action-ids* into an externally-owned
  table, which is the cleaner design).
- **Packrat facility** — `typescript/src/parse/packrat.ts:71-100` (`getCijKey` = the `(pc,sp)` key
  arithmetic to share), `:156/290` (`PACKRAT_ARMED` = the default-off discipline the VM keeps),
  `:216-250` (epoch save/restore = the interpreter's memo-table lifecycle). Rust has *no* packrat —
  the VM is the vehicle that gives Rust one isomorphic to TS's.
- **Diagnostics facility** — `furthest_offset` bump sites (`combinators.rs:33`, TS
  `mergeErrorState`), the lazy `suggestions`/`secondarySpans` fields (TS `state.ts` +
  `utils.ts`), `SpanParser.label` (`span_parser/mod.rs`) = the `Label`/`expected` program points.
- **TS interpreter home** — new; the closest existing monomorphic-table precedent is
  `typescript/src/parse/leaf.ts:100-151` (`dispatch`'s `Int8Array(128)` table), the shape a
  `switch`-based VM loop generalizes. Guard: `proof:no-span-surface` — the VM must expose **no
  public combinator surface**, only an internal lowering target.
- **Bench harnesses to measure against** — Rust divan `--bench parse_that_combinator` /
  `parse_that_css` / `micro_parse_that`; TS custom `process.hrtime.bigint` paired-interleaving
  instrument (the certified method; `vitest bench` is warm and refused). Baselines: the closure
  core AND the live `SpanParser` tree-walker — the VM must be sited against *both*.

---

## 5. Named risks and counterexamples

1. **Interpreter overhead beats the isomorphism win (the central risk).** rustc/LLVM inline the
   monomorphized closure/`SpanKind` arms into straight-line code; a bytecode loop reintroduces one
   indirect/branch-mispredicted dispatch *per instruction*, plus explicit-stack traffic the native
   call stack did for free. The campaign's own refuted-speedup lesson (a shim that looked 1.3–1.5×
   from two-arm window asymmetry, neutral-to-slower under paired re-measure) is the standing
   caution: **VM-CORE must not claim a speedup it did not earn under paired interleaving.** The
   honest prior is that a naïve VM *loses on throughput* and wins only on isomorphism + facilities;
   a threaded-code/computed-goto variant is the fallback that might reach parity. *Counterexample to
   hunt:* the VM at 0.7–0.9× the closure/SpanKind core on the twitter/citm/CSS hot corpora.
2. **The value/action facility is where the loss concentrates.** Recognition (Span-tier) VM may
   reach parity; a fully-generic value-producing VM pays an indirect `Action` call where the
   closure core inlined the `map`. *Counterexample:* Span-tier VM at 0.98×, but value-tier VM at
   0.6× because every projection is an un-inlinable call. Mitigation is staging (build-time action
   fusion) — but that is family 4B's mechanism; VM-CORE must state honestly whether it can reach
   value parity *without* becoming 4B.
3. **Isomorphism-by-construction is undercut by the two cores having diverged.** Rust has
   `SpanParser`; TS excised `*Span`. The VM is a *new* shared layer both compile into — but if the
   two interpreters drift (V8 wants struct-of-arrays + `switch`; Rust wants `enum` + `match`), the
   "same layout" claim degrades to "same opcode stream, different decoders," which is still stronger
   than mirrored source but weaker than the headline. *Counterexample:* a V8 deopt forces a TS-only
   operand reordering, breaking the byte-identical-program oracle.
4. **Left-recursion parity is subtle.** Warth seed-growing in a VM (bounded `(pc,sp)` re-entry) is
   the one facility whose Rust/TS equivalence is non-obvious; a mismatch produces different parses
   on left-recursive grammars. *Counterexample:* an expression grammar where TS grows the seed one
   iteration further than Rust.
5. **Reopening the `no-span-surface` wound.** If the VM leaks *any* public combinator/Span API it
   re-creates the duplication TS deliberately deleted. *Guard:* the VM is internal-only; the public
   combinator API is unchanged and merely *compiles to* the VM.
6. **The generic-`Output` lifetime trap in Rust.** Action closures parameterized by input `'a`
   cannot live in a `'static` program (the same unsoundness that blocks a naïve arena). *Guard:* the
   program holds action-*ids*; closures live in an `'a`-scoped side table threaded through the parse
   entry, never smuggled into a `'static`.

---

## 6. What a prototype must test to move this family

A single prover, on a scratch copy (rsync + external-patch-repoint recipe; isolated
`CARGO_TARGET_DIR`), must produce **measured numbers, not assertions**, on these exact questions:

1. **Span-tier VM vs the SpanKind tree-walker.** Compile a real grammar's recognition skeleton
   (JSON structural, or a CSS token grammar) to a flat `Choice/Commit/PartialCommit/Set/Str/Dfa`
   program; run the dispatch loop; paired-interleave (N≥200, two windows, opposite arm order, cold
   per-parse, PMU cyc/B where available) against the live `SpanParser` on the *same* grammar. The
   gate is falsifiable in both directions: ≥0.98× → the VM is throughput-viable and isomorphism is
   free; <0.9× → the VM loses on throughput and must justify itself on facilities alone (a valid,
   documented negative result).
2. **The threaded-code recovery.** If (1) loses, decode-once into a function-pointer array (Rust) /
   confirm V8 emits a jump-table `switch` (TS) and re-measure — does threaded code close the gap to
   parity? This decides whether the loss is fundamental or a decode-branch artifact.
3. **The byte-identical-program oracle.** Serialize the compiled program from Rust, decode in a TS
   interpreter, assert (a) identical opcode/operand stream and (b) identical parse (offsets + values)
   over a shared conformance corpus. This is the *proof* of isomorphism-by-construction — the
   family's headline claim stands or falls here, independent of throughput.
4. **Packrat as a VM facility, both languages.** Implement `MemoOpen/MemoCommit` with the shared
   `(pc,sp)` key; show a left-recursive expression grammar parses *identically* in Rust and TS
   (same tree, same seed-growth count); show the default (no-memo) instruction path allocates
   nothing (the armed/unarmed discipline preserved). Counterexample to disprove: any left-recursive
   input where the two diverge.
5. **The value-tier honesty check.** Add the `Action`/value-stack facility, wire one `->` projection
   grammar, and paired-bench value-producing VM vs the generic closure `Parser<Output>` core. Report
   the *separate* number from (1). If value-tier is <0.7× while Span-tier is ≥0.98×, the family's
   honest verdict is "recognition VM yes, value VM needs staging (→ 4B)."

The disposition this pass should reach: VM-CORE is **substantiated as a mechanism** (the seed is
in-tree, the prior art is proven-correct, the isomorphism claim is testable by a golden-program
oracle) but its throughput standing is **an open measurement** whose honest prior is a loss-to-be-
recovered, not a win-to-be-assumed. The family advances on the strength of isomorphism +
facilities; it must not import a speed narrative the paired bench has not certified.

---

## Sources

- LPeg / parsing machine: https://www.inf.puc-rio.br/~roberto/lpeg/ ,
  https://www.inf.puc-rio.br/~roberto/docs/ry08-4.pdf (Medeiros & Ierusalimschy, *A Parsing Machine
  for PEGs*), https://www.inf.puc-rio.br/~roberto/docs/lpeg-primer.pdf
- Incremental parsing machine: https://zyedidia.github.io/notes/yedidia_thesis.pdf ,
  https://github.com/zyedidia/gpeg
- Staged combinators / abstract machine: https://dl.acm.org/doi/10.1145/3409002 ,
  https://mpickering.github.io/papers/parsley-icfp.pdf (Willis, Wu, Pickering, *Staged Selective
  Parser Combinators*, ICFP 2020), https://hackage.haskell.org/package/parsley
- VM JIT viability: https://morepypy.blogspot.com/2019/04/an-rpython-jit-for-lpegs.html
