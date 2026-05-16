# FSM/DTA Correctness Audit — From-First-Principles

Audit target: should bbnf-lang use an FSM / DTA architecture, as proposed in
`restart/skinny/audit/SOTA-BEAT-DESIGN.md` §5 ("Phase 3 — Collapsed-stage
AVX-512 backend (asmjson-class)") and §5.1 ("9-state FSM and PC-as-state")?

Audit date: 2026-05-12. Status: complete.

---

## (a) DTA acronym disambiguation in bbnf-lang context

The acronym **DTA** appears nowhere in current (post-restart, 2026-05-04+) bbnf
docs as a *live* term. Its only occurrences are archaeological labels for the
**Era V "1,000-commit fault"** arc:

- `restart-archive-2026-05-04/audit/passes/PASS-C.md:277` — *"Era V (DTA/PSI
  rut) | AV, AW-I/II/III/IV/V, AX | ~572 | The 1,000-commit DTA arc;
  substrate-first/consumer-later | DEAD — DTA interpreter deleted at AX.W0b
  (~78K LOC reclaim); shape emitter substrate retained as view layer"*
- `restart-archive-2026-05-04/audit/per-agent/pass-c-agent-1-inventory.md:351`
  — *"V | DTA/PSI rut + interpreter deletion + view layer | The '1000-commit
  fault' arc; substrate severed at AX.W0b"*

The retired-substrate file `crates/tape/src/dta.rs` (per `git log -- ` cited in
PASS-C §7.5 ¶2) names the layer. **No current document in
`restart/skinny/audit/` or `restart/locks/` defines DTA as an active acronym.**

### Best-fit reading of the archive "DTA"

Cross-referencing the era description ("substrate-first/consumer-later",
"interpreter deleted", "shape emitter substrate retained as view layer") with
the standard parsing-literature glosses:

| Candidate gloss                           | Fit                                                                                                                    |
|-------------------------------------------|------------------------------------------------------------------------------------------------------------------------|
| **Direct-Threaded Automaton** (interpreter w/ computed-goto dispatch)        | **Best fit.** Era V's "DTA interpreter deleted" + "78K LOC reclaim" matches the cost of a threaded-code interpreter dispatching over a stored opcode tape, with the tape as substrate. The "shape emitter substrate retained as view layer" is consistent with: keep the tape, delete the threaded-code walker. |
| **Determinized Tree Automaton** (bottom-up tree match) | Poor fit. No tree-automaton infrastructure ever shipped; AST shape was always combinator-driven (per memory item `no-ts-ir` and `direct-to-struct-approach`). |
| **Deterministic Tree Acceptor** (CFL acceptor) | Poor fit. Acceptors are recognition-only; era V emitted views (substrate-first/consumer-later) which is a build, not an accept. |
| **Direct-Threaded Architecture** (Forth-class interpreter) | Partial fit. Synonym of #1 in practice; the architecture-vs-automaton distinction is collapsed in JIT/parser literature. |

Verdict on (a): **DTA in bbnf-lang's archive = Direct-Threaded Automaton =
threaded-code interpreter walking a tape projection**. The 1,000-commit
failure was not "an FSM that failed"; it was a *threaded-code interpreter
sitting on top of a substrate-first tape* — the substrate was built, the
interpreter was built, but neither of them ever drove a measured end-to-end
win because they were always one wave away from "the consumer". Cited
explicitly at `restart-archive-2026-05-04/audit/per-agent/pass-c-agent-6-cross-cut.md:116`
("each tranche claimed the next would consume; none did").

### PSI

PSI is even less defined. It rides with DTA in the era V label ("DTA/PSI
rut") and appears only in three archival lines. No `Path-Sensitive`,
`Parallel-Structural-Index`, or similar expansion appears anywhere in the
corpus. Most defensible reading: **PSI was the project's internal nickname
for the structural-index-prepass shape**, sibling to DTA. This reading is
corroborated by:

- The active 2026-05-12 design *explicitly rejects* a "sidecar structural-index
  *prepass* shape" (`SOTA-BEAT-DESIGN.md:3`, line: *"The sidecar
  structural-index prepass shape is rejected; the retained tape projection IS
  the structural index"*).
- The Era V archive names PSI alongside DTA as the failed substrate; the
  surviving Lock 1 (`restart/locks/14-LOCKS.md:34` per PASS-C reference)
  rewords the lesson as *"the structural projection IS the tape, not a
  sidecar to it; no parallel offset stream"* (quoted at
  `restart/skinny/SUBSTRATE.md:221`).

Verdict on (a) PSI: **PSI was the failed prepass-style structural-index
sidecar** (similar in spirit to simdjson's stage 1, but landed in bbnf-lang as
a separate buffer the parser then re-read instead of consumed inline). The
Lock 1 clarification is the post-mortem.

---

## (b) FSM-parsing successes + failures inventory

### Successes

| System                         | What's FSM-shaped                                                                                          | Throughput / scale                                                            | Grammar generality                                                                                 |
|--------------------------------|------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|
| **Lex / Flex**                 | Regex → DFA table; one big switch driven by a state vector                                                  | Industrial scanner standard since 1975                                        | LL/LR-grammar tokenizers; regex layer only — no nested structure                                   |
| **re2c**                       | Regex → DFA encoded as direct-coded conditional jumps (NOT table-driven)                                    | Used in PHP, Ninja, ngrep; fastest C lexer family                             | Regex layer only; not a context-free parser                                                        |
| **Ragel**                      | Regex + actions → direct-coded FSM (sibling of re2c)                                                        | Mongrel/Mongrel2/Mailman HTTP parsing                                         | Regex + embedded actions; can simulate small CFLs via call/return tables but not general CFG       |
| **Bison / Yacc**               | LALR(1) → push-down automaton with tables (state stack + GOTO)                                              | GCC (pre-2003), Bash, MySQL, PostgreSQL                                       | Full CFG via state-stack PDA; not a pure FSM                                                       |
| **Lemon**                      | LALR(1) → table-driven PDA (variant of Yacc)                                                                | SQLite                                                                        | Full CFG via PDA                                                                                   |
| **asmjson**                    | "9-state FSM + PC-as-state via r10 indirect jump" PLUS `frames_buf[MAX_JSON_DEPTH]` open-bracket stack       | 10.93 GiB/s twitter on Zen 4 AVX-512                                          | JSON only; hand-tuned                                                                              |
| **simdjson stage 1**           | NOT an FSM. Branchless parallel structural-character classifier over 64-byte chunks via SIMD compares       | ~3.0 GB/s twitter; ~3.7+ GiB/s aggregate                                      | JSON only; the stage-2 walker is a recursive-descent PDA over the structural index               |
| **sonic-rs**                   | Recursive descent + SIMD primitives (NOT FSM); the SIMD layer accelerates string/number/whitespace, the    | 2.32–2.44 GB/s twitter (M5 Max Value-DOM)                                     | Rust struct deserialization via serde; targets serde compat                                        |
|                                | parse loop itself is RD                                                                                     |                                                                               |                                                                                                    |
| **yyjson**                     | Scalar recursive descent with manual force-inline + ~18 KiB single hot function                             | 3.69 GiB/s twitter (M5 Max, no SIMD)                                          | JSON only; ANSI C; the fastest scalar JSON parser                                                  |
| **simdzone (Lemire/Mansson)**  | DFA-on-DNS-records; similar shape to asmjson but with DNS lex grammar                                       | Millions of DNS records/sec; arXiv 2412.04692                                 | DNS RDATA encoding only                                                                            |
| **GCC ≥4.x C/C++ frontend**    | Hand-written recursive descent (SWITCHED AWAY FROM Bison LALR in early-2000s)                               | Industrial-scale                                                              | C/C++ — full language; recovery / diagnostics quality cited as the *reason for the switch*        |
| **Clang**                      | Hand-written recursive descent                                                                              | Industrial-scale                                                              | C/C++/Obj-C; same diagnostics rationale as GCC                                                     |

### Failure modes (compiled from parsing literature + SK-V3 archive)

| Failure mode                                | Manifestation                                                                                                                                                | Citation                                                                                                          |
|---------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| **State explosion (LR(k))**                 | Canonical LR(1) tables are too large for realistic grammars; LALR(1) (DeRemer 1969) merges states with shared LR(0) core but introduces shift/reduce conflicts | Wikipedia LR-parser; Kallmeyer "LR Parsing" lecture notes; comp.compilers / Tratt 2023                            |
| **Pure FSM cannot recognise CFLs**           | Nested brackets `a^n b^n` is the textbook example; JSON, XML, S-expr, any nested grammar requires a stack                                                       | Pushdown-automaton theorem; Hopcroft & Ullman; reaffirmed by UNC PDA-and-CFL slides                              |
| **Recovery overhead**                       | FSMs have no "context" to recover into — error recovery in an FSM degrades to "panic to nearest synchronizing token". Quality recovery requires a parser stack | GCC 2003 RD switch; Tratt 2023; Hacker News §44666824 ("Why I write recursive descent parsers")                  |
| **Codegen size**                            | Table-driven LR generators produce opaque state tables; re2c switched to direct-coded jumps for *both* speed and debuggability                                | re2c.org docs; abubalay 2018 recursive-ascent post                                                                |
| **Debuggability + maintainability**         | RD parsers read like the grammar; table-driven generators output unreadable state tables that resist hand-editing                                              | Tratt 2023 "Why we need to know LR and recursive descent parsing"; GCC and Clang both ship RD                    |
| **Generic-grammar code-size blowup**        | A meta-grammar generator emitting FSM-class code per grammar pays the FSM size cost N times                                                                    | Empirically: bbnf-lang Era V "78K LOC reclaim" on the interpreter delete (PASS-C §7.2)                            |
| **Substrate-first / consumer-later trap**   | Building the substrate (tape, DTA, structural index) without a measured end-to-end consumer leaves the substrate dead-code: 572 commits, then deleted          | bbnf-lang Era V is the load-bearing case study (PASS-C §7.2); AX.W0b deletion was the closure |

---

## (c) bbnf-lang grammar-shape compatibility matrix

For each currently-tracked grammar in `Cargo.toml [workspace.metadata.bbnf.grammars]`:

| Grammar       | FSM viable?                                              | PDA viable?                                                                                  | Recursive descent viable?                                                  | Best-fit mapping to current `BackendShape` taxonomy                  |
|---------------|----------------------------------------------------------|----------------------------------------------------------------------------------------------|----------------------------------------------------------------------------|----------------------------------------------------------------------|
| **json**      | NO (pure FSM cannot count brackets)                       | **YES** — asmjson is the existence proof (9-state FSM + `frames_buf` stack of `MAX_JSON_DEPTH=64`). One-byte-decidable; no recovery in SOTA test set. | YES — yyjson, sonic-rs, simdjson stage 2 are all RD-shaped over the structural index | `OffsetTape` (current) or `CollapsedStage` (asmjson-class, AVX-512 only) — both are valid PDA implementations |
| **css_l4**    | NO                                                        | YES, but recovery makes the cost-benefit unfavorable                                          | **YES — strongly preferred.** CSS L4 has `@error(recover)` directives; recovery in a PDA-codegen path requires per-state recovery tables (intractable). RD recovery is local-to-the-frame and well understood | `EagerTape` per `SUBSTRATE.md:215` cost-model derivation (recovery presence forces `EagerTape`) |
| **bbnf**      | NO (operator precedence + nested host calls)               | YES — Pratt-precedence can be encoded as state-per-precedence-level; well-trod path             | YES (the self-hosting target)                                              | `EagerTape` (self-hosting + host fns + closures) |
| **css_pretty**| NO                                                        | Partial (pretty-printing is structural)                                                       | YES                                                                        | `EagerTape` (layout scope present) |
| **google_sheets** | NO (formulas need Pratt + arrays)                     | YES (Pratt + bracket stack)                                                                   | YES                                                                        | `EagerTape` (host fns + layout) |
| **ebnf / bnf**| NO                                                        | YES                                                                                           | YES                                                                        | `EagerTape` |
| **csv**       | **MAYBE** (CSV is regular if no quoted newlines; RFC 4180 needs lookahead at quote → it's still regular) | YES                                                                                           | YES                                                                        | `OffsetTape` or `SinkOnly` |
| **math**      | NO                                                        | YES (Pratt)                                                                                   | YES                                                                        | `EagerTape` |

### Reading the matrix

- **Pure-FSM viable** = only `csv` (and only with quoted-newline care). Every
  other grammar in bbnf-lang's catalog is context-free with nesting and so is
  FSM-impossible without a stack. **A "FSM backend" is therefore mis-named:
  every JSON-or-richer grammar is a PDA backend.**
- **PDA viable** = every grammar. PDA-with-codegen is feasible.
- **RD viable** = every grammar. RD-with-codegen is feasible.

The discriminator is **recovery presence + layout scope + host fn presence**.
Per `SUBSTRATE.md:215`, the cost-model derivation already encodes this:
*"first-set disjointness, `@error(recover)` presence, `@host fn`
parse-time-decoded presence, `@layout` scope presence"*.

The matrix collapses to: **JSON and CSV are the only candidates for a
PDA-with-collapsed-stage-FSM-dispatch backend; everything else is RD/EagerTape
because of recovery or host facts.**

---

## (d) asmjson architecture clarification

### The "FSM" framing is *partly misleading*

**asmjson is not a pure FSM.** Direct read of `src/lib.rs` via WebFetch
(2026-05-12) confirms:

```rust
pub const MAX_JSON_DEPTH: usize = 64;

let mut frames_buf  = [FrameKind::Object; MAX_JSON_DEPTH];
let mut frames_depth: usize = 0;

#[repr(u8)]
enum FrameKind { Object = 0, Array = 1 }

// Close-token validation
b'}' => {
    if frames_depth == 0 || frames_buf[frames_depth - 1] != FrameKind::Object
    { State::Error }
}
b']' => {
    if frames_depth == 0 || frames_buf[frames_depth - 1] != FrameKind::Array
    { State::Error }
}

// AVX-512 path also carries
let mut open_buf = [0u64; MAX_JSON_DEPTH];
```

asmjson is therefore a **deterministic pushdown automaton (DPDA)**:

1. **Finite control with 9 states** (V, O, K, D, C, S, F, R, A per
   `dev.md`, confirmed via WebFetch of asmjson dev.md): these are the
   *FSM-shaped fragment* — they encode the *flat* grammar position
   "expecting value vs key vs colon vs comma-or-close" within a single
   container scope.
2. **Stack discipline via `frames_buf[]` + `open_buf[]`**: tracks
   container nesting depth and container type (object vs array), capped
   at 64 levels.
3. **PC-as-state via `r10`**: this is a *direct-threaded dispatch* over
   the FSM — the program counter holds the next-state's entry label.
   The "PC is state" trick is direct-threaded code in the Forth /
   Bell-Labs sense, but the *automaton class* is still PDA, not pure FSM.

### Why the "FSM" framing leaks into the design doc

The 9-state finite-control fragment is what `SOTA-BEAT-DESIGN.md` §5.1 calls
"the FSM" (`restart/skinny/audit/SOTA-BEAT-DESIGN.md:277`):

> *"State alphabet: V (value), O (object body), K (key expected),
> D (colon expected), C (comma-or-close), S (string body), F (false
> literal), R (true literal), A (null literal — 'null' rhymes with 'a-z'
> close enough). Each state has its own classifier mask set... with `r10`
> holding the next-state target across chunk-refetch boundaries."*

This passage is technically accurate at the finite-control level, but it
elides the stack — which is the asmjson code's *actual* depth-tracking
mechanism, not optional, and the difference between a regular language
recognizer and a context-free language recognizer. The design doc *would
benefit* from making this explicit: **"asmjson is a 9-state DPDA with
direct-threaded dispatch (`r10`) over a hardware-bounded explicit stack
(`open_buf[64]`)"**.

This is not a defect in the design; it is a documentation clarity issue.
The Phase 4 `CollapsedStage` plan in `SOTA-BEAT-DESIGN.md` §5 implicitly
inherits the stack discipline because it is a faithful port of asmjson's
*architecture verbatim* (line 188: *"adopt asmjson architecture verbatim
+ stack esoterica on top"*). The bracket-stack thereby comes along.

### One more honest reading

asmjson is also **not grammar-generic FSM codegen** in the bbnf-lang sense.
The 9 states are bespoke to JSON's grammar; the classifier mask sets per
state are bespoke per-state byte-classes; the stack-frame discipline is
bespoke to brace/bracket pairing. A meta-grammar generator emitting this
shape per grammar must (a) compute the per-grammar state set from the
grammar IR, (b) compute the per-state classifier masks from the per-state
first-set, (c) compute the bracket-pair set for the stack discipline, and
(d) emit the direct-threaded dispatch.

These are tractable in the cost-model framework (`LayoutFacts.backend_shape`
+ shape miner per Lock 10) per `SOTA-BEAT-DESIGN.md` line 238, but they are
each load-bearing in the codegen template, and they each have a per-grammar
state-explosion bound that the cost model must check before admitting the
`CollapsedStage` shape.

---

## (e) Honest verdict — should bbnf-lang use FSM at all?

### Headline

**Yes — in a strictly scoped, cost-model-gated, opt-in role; no — as a
general-purpose backend across all grammars.** Specifically:

1. **NO**: bbnf-lang should not lower *every grammar* to an FSM (or
   FSM-+-stack PDA). Recovery-bearing grammars (`css_l4`, `bbnf`,
   `google_sheets`) lose more than they gain — recovery in a PDA-codegen
   shape requires per-state recovery tables which are intractable to
   generate and maintain. RD is the right path there, and the existing
   `EagerTape` shape encodes RD-with-cursor-byte-position correctly.

2. **YES, scoped**: bbnf-lang *should* expose a `CollapsedStage` backend
   shape for grammars that admit it — namely JSON (and possibly CSV).
   The current `BackendShape` taxonomy already names this (`EagerTape /
   OffsetTape / EventTape / SinkOnly / CollapsedStage`, per
   `SUBSTRATE.md:215`), with cost-model derivation gating admission on:
   - No `@error(recover)` directives
   - No `@host fn` decoded at parse time
   - No `@layout` scope
   - First-set disjointness across rules
   - Target ISA support (AVX-512 VBMI2 for the asmjson-class lowering)

   These gates are correct. JSON admits all five. CSS L4 fails the
   first three. This is the right discriminator.

3. **HONESTY about the FSM label**: the Phase 4 `CollapsedStage`
   backend is **a DPDA, not a pure FSM** — the open-bracket stack is
   load-bearing, not optional. The design doc would benefit from making
   the PDA-vs-FSM distinction explicit so that future readers do not
   re-derive the misconception that "JSON parsing is regular" (it is
   not).

4. **HONESTY about prior failure**: Era V's DTA failure was **not** a
   refutation of FSM-class parsing; it was a refutation of the
   *substrate-first / consumer-later* discipline. The substrate (tape)
   was built; the interpreter (DTA) was built; the consumer (the
   measured end-to-end win) was always *one wave away*. The current
   SK-V3 plan inverts this discipline correctly: structural-index *is*
   the tape (Lock 1); the codegen lowering *is* the consumer; the
   measured BENCH gate *is* the close criterion (per `BENCH.md`).

### Concrete recommendation

The currently-staged plan in `SOTA-BEAT-DESIGN.md` is **structurally
correct**. The `CollapsedStage` shape:

- Is gated by the cost model on per-grammar facts already encoded in the
  IR (no new directive surface; per Lock 10 auto-detect).
- Is opt-in via target features (AVX-512 VBMI2) and grammar fitness; not
  forced on every grammar.
- Adopts asmjson's architecture verbatim (including the open-bracket
  stack) and stacks esoteric primitives strictly on top.
- Carries an empirical phase gate (`Phase 4: T1 ≥ 7400 MiB/s on x86_64,
  ≤ 0.45 c/B, 1 hot leaf`).

Two clarifications would improve the design doc without changing the
architecture:

1. **Rename or footnote "FSM"**: §5.1's "9-state FSM and PC-as-state"
   heading is accurate at the finite-control level but understates the
   stack. Suggested wording: *"9-state DPDA: 9-state finite control,
   direct-threaded dispatch via `r10` PC-as-state, hardware-bounded
   explicit stack (`open_buf[MAX_JSON_DEPTH=64]`) for container
   nesting."*

2. **Add explicit failure-mode bounds**: cost-model gating should record
   *why* `CollapsedStage` is admitted/rejected per grammar — the
   admission predicates exist in `SUBSTRATE.md:215`'s derivation list,
   but the rejection diagnostics (e.g., `BBNF-BACKEND-SHAPE-INADMISSIBLE-FOR:
   recovery-present`) are not enumerated in `COMPILER.md`'s diagnostic
   table.

These are documentation refinements; the architectural decision is sound.

### What FSM/DTA is *not* the answer to

- **Generic recovery-bearing grammar codegen.** RD is the answer.
- **Generic typed-event consumption.** That is the `OffsetTape`
  cursor-driven shape, which is RD-over-event-tape, not FSM.
- **Generic host-fn-bearing grammar codegen.** Host fns are evaluated at
  parse time on the RD path; not on the collapsed-stage path.
- **Cross-platform portability.** `CollapsedStage` is x86_64
  AVX-512 VBMI2 specific per §5; arm64 stays on `OffsetTape` /
  RD-over-structural-index per `SOTA-BEAT-DESIGN.md:332`.

The cost model's job is to keep these scopes honest. The cost model
already encodes the right discriminators (`SUBSTRATE.md:215`).

---

## Citations

- `restart-archive-2026-05-04/audit/passes/PASS-C.md:277` — Era V "DTA/PSI rut" label
- `restart-archive-2026-05-04/audit/per-agent/pass-c-agent-1-inventory.md:351` — Era V failure-mode anatomy
- `restart-archive-2026-05-04/audit/per-agent/pass-c-agent-6-cross-cut.md:116` — "Each tranche claimed the next would consume; none did"
- `restart/skinny/SUBSTRATE.md:215` — 5-shape `BackendShape` taxonomy + cost-model derivation
- `restart/skinny/SUBSTRATE.md:221` — Lock 1 clarification ("structural projection IS the tape")
- `restart/skinny/audit/SOTA-BEAT-DESIGN.md:267` — "collapsing Stage A and Stage B into one mask-driven FSM walk in the style of asmjson"
- `restart/skinny/audit/SOTA-BEAT-DESIGN.md:277-283` — 9-state alphabet and PC-as-state
- `restart/skinny/audit/SOTA-BEAT-DESIGN.md:332` — arm64 stays on structural-index template
- `restart/skinny/audit/SOTA-BEAT-DESIGN.md:148-188` — asmjson architecture-verbatim + esoteric-stack-on-top strategy
- `restart/skinny/COMPILER.md:243-253` — Per-rule `BackendShape` lowering contract
- `restart/skinny/COMPILER.md:247` — `EagerTape` admission predicates
- asmjson `doc/dev.md` (via WebFetch 2026-05-12) — 9-state FSM description
- asmjson `src/lib.rs` (via WebFetch 2026-05-12) — `MAX_JSON_DEPTH=64`, `frames_buf[]`, `open_buf[]`, `FrameKind { Object, Array }` — confirms DPDA, not pure FSM
- Langdale & Lemire 2019 (arXiv:1902.08318) — "Parsing Gigabytes of JSON per Second" — simdjson stage 1 is a parallel branchless classifier, not a state machine; stage 2 is RD over the structural index
- re2c.org docs — DFA → directly-coded jumps (not table-driven)
- Wikipedia: LR parser; LALR parser — state-explosion in canonical LR(1); LALR DeRemer 1969 merge
- Tratt 2023 "Why we need to know LR and recursive descent parsing techniques" — GCC's switch from Bison LALR to hand-written RD (early 2000s); recovery/diagnostics rationale
- Hopcroft & Ullman / standard automata theory — FSMs cannot recognize CFLs (nested brackets); PDAs can; DPDAs recognise DCFLs and underlie LR parsing
- sonic-rs `docs/performance.md` + repo — RD with SIMD primitives; "does not use the two-stage SIMD algorithms from simd-json"
- yyjson architecture docs (DeepWiki + repo) — scalar RD; "fixed-size stack memory"; ~18 KiB hot function

---

End of audit.
