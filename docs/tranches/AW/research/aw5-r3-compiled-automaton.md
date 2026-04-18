# AW-V R3 — The Compiled-Automaton Question: Why bbnf-regex Compiles and the DTA Doesn't

## 1. Angle headline

`bbnf-regex` compiles NFA→DFA→inline Rust; its DFAs beat the regex crate. The DTA is "a larger, more generalized state machine" (user's framing) — so why did the DTA ship as a RUNTIME INTERPRETER while bbnf-regex lowered to CODEGEN? The answer is neither "automata are unviable" nor "LLVM's inlining budget is exceeded" — it is **inherent uniformity of alphabet and transition shape**. bbnf-regex DFAs have per-state bodies that all match `b: u8` (one uniform match); DTA states have 9 irreducibly different body shapes (`Seq` pushes frames, `Repeat` manages counters, `ShuntingYard` runs a reducer, `Ref` indirects by rule id). The Path A/Path B dichotomy mistakes the symptom (inline-budget overflow) for the disease. The shape-emitter path is honest Path B wearing DTA branding.

## 2. State counts — the raw data

Per-grammar `__DTA_STATES` sizes from the `.bbnf-cache` emission (parsed from `/Users/mkbabb/Programming/bbnf-lang/crates/target/.bbnf-cache/`):

| Grammar | State count | Cache file |
|---|---:|---|
| JSON | 51 | `427d9cfe82914cdd.rs:6850` lines |
| BNF | 41 | `0fea1e22ff986b0d.rs` |
| CSS L4 | 83 | `50d1173d8d079115.rs:16172` lines |
| Sheets | 178 | `5709b11ccc284e16.rs:25173` lines |
| EBNF | 187 | `587a64267a88d5af.rs` |
| BBNF parser | 197 | `bb1c1a2b7305e2d0.rs:24253` lines |
| BBNF bootstrap | 496 | `8d68726b11babeb5.rs:62559` lines (also `crates/core/src/grammar/generated.rs:933`) |

Variant distribution (counted via Python AST slicing of the `__DTA_STATES` literal):

| Grammar | Seq | Literal | WsTrim | Ref | Repeat | AltLinear | Regex | ClassifyByte | ByteDispatch | Other |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| JSON (51) | 13 | 10 | 10 | 10 | 4 | 0 | 2 | 1 | 1 | 0 |
| CSS L4 (83) | 14 | 14 | 0 | 18 | 7 | 3 | 23 | 1 | 0 | 3 |
| Sheets (178) | 38 | 35 | 28 | 39 | 13 | 7 | 14 | 2 | 0 | 2 (SY) |
| BBNF parser (197) | 47 | 38 | 48 | 33 | 15 | 2 | 8 | 4 | 1 | 1 |
| BBNF bootstrap (496) | 112 | 109 | 102 | 88 | 36 | 27 | 14 | 0 | 0 | 1 (SY) |
| EBNF (187) | 13 | 113 | 0 | 22 | 8 | 0 | 17 | 5 | 4 | 5 |

**Critical observation 1**: no grammar exceeds 496 states. The arch-comparison claim of "CSS L4's state count is ~800" (`aw3-r5-path-a-keep-dta.md:95`) and "CSS L4 sits at ~2900 states" (`hot_cold.rs:6`) are both **wrong by 5-35×**. Actual CSS L4 = 83.

**Critical observation 2**: the dispatch_one `match` has 14 irreducible variant arms (`driver.rs:1367-2105`): `Epsilon, Literal, Regex, Seq, Ref, ClassifyByte, ByteDispatch, AltLinear, Repeat, WsTrim, Minus, ConsumeToNextStructural, ShuntingYard`. LLVM lowers this to a 4-compare ladder (log₂14 ≈ 3.8). That is the canonical per-byte dispatch floor FINAL-IV.md:23 reports as eliminated from the hot path, but the floor reappears when the per-arm body calls cold-path helpers (W2.1 left `advance_or_pop_with` out of line per the admission at `FINAL-IV.md:108`).

## 3. How bbnf-regex avoids the inline-budget problem

Read from `crates/core/src/backend/rust/emitter/dfa_codegen.rs:191-296` (`emit_dfa_body_for_pattern`):

The emitted DFA body is a labelled block:

```rust
'__dfa: {
    let mut __dfa_state: u32 = 0;
    let mut __dfa_p: usize = pos;
    loop {
        let b = match input.get(__dfa_p) { Some(&b) => b, None => break };
        match __dfa_state {
            0 => match b { <coalesced> => __dfa_state = A, _ => break },
            1 => match b { ... },
            _ => unsafe { unreachable_unchecked() },
        }
        __dfa_p += 1;
        match __dfa_state { <accept_lits>|* => __dfa_last_match = Some(...) }
    }
    break '__dfa __dfa_last_match.map(|end| end - pos);
}
```

**Three mechanisms are load-bearing**:

1. **Uniform per-state shape.** Every DFA state body is `match b { bytes => state = N, _ => break }`. LLVM sees one basic block per state, each of nearly identical machine-code size (~20-40 bytes). No per-state variance in stack manipulation, no cross-fn calls, no payload-type branching. A DFA with 100 states compiles to ~3 KB of machine code.

2. **Byte-class coalescing** (line 212-222): `BTreeMap<u32, Vec<u8>>` groups bytes sharing a target into one `(b1 | b2 | b3) => __dfa_state = N` arm. 256 possible byte arms collapse to the distinct-target cardinality — typically 3-10 arms per state. The `byte_classes` table (line 216) already partitioned the alphabet; emission follows it.

3. **`'__dfa:` labelled block + `unreachable_unchecked()`** (line 288): the emitted DFA is a single expression yielding `Option<u32>`, inlined at the call site via `quote!` splice (line 309 `emit_dfa_inline_body` is called from `lower_state.rs`'s Regex arm). LLVM treats every Regex state's body as one basic block in the walker function.

Per-grammar scale: CSS L4 emits 1,032 `__dfa_state` bindings across 23 Regex + 0 WsTrim states (many with multi-state DFAs: a number regex alone compiles to ~8 DFA states). That's ~1,000 inline DFA states in one walker function — the `fat LTO + #[inline(always)]` still resolves without exceeding LLVM's budget because each state is a tiny uniform match arm.

The `DfaOptions::state_limit: 4096` (`dfa.rs:36`) is the PER-PATTERN cap. Grammars never approach it.

## 4. Can DTA compile inline? Yes, but the state variant shape is the blocker

**Verdict: yes, with specific constraints none of Paths A through shape-emitter have honestly committed to.**

The inline-budget claim in `aw3-r5-path-a-keep-dta.md:93-96` and `hot_cold.rs:32-42` is a herring. HOT_BUDGET=128 caps 128 states in-line; the orchestrator's estimate of "80-120 bytes per state" for 128 states = ~15 KB of machine code, well under any LLVM budget. The emitter HAS been emitting all-states-inline for BBNF bootstrap (496 states) — the AW-IV walker is one inline function, and `nm` shows walker present / dispatch_one absent (`FINAL-IV.md:23`).

The real blocker is variant-shape non-uniformity. Consider what each arm body actually does:

- `Literal { text, payload }` (driver.rs:1375-1415) — `input[pos..end] != bytes` cmp, emit_leaf, advance_or_pop_with. Body: ~40 lines, 1 cross-fn call.
- `Seq { children, frame, promote }` (line 1483-1540) — reserve_compound, push frame, 7-column write, state = children[0]. Body: ~60 lines, 2 cross-fn calls.
- `AltLinear { branches }` (line 1661-1818) — for each branch: savepoint, try_branch, restore-on-fail. Body: ~150 lines, loop with rollback.
- `Repeat { inner, lo, hi, counter_optional }` (1818-1906) — counter alloc, iter-savepoint, push Repeat frame, transition. Body: ~90 lines.
- `ShuntingYard` (2083+) — reducer dispatch over PRECEDENCE_LUT, operator stack, ~250 lines.

The W4 inline-lowering (`lower_state.rs`) DID get every variant into one giant inline match for each grammar. `cargo asm` shows `__dta_walker_inline::run` present in the bench binary. But post-W4 profiles show `try_branch` at 70.9-78.9% self-time on CSS L4 (`aw4-profile-p2-css-l4.md:59`). Why?

Because per-arm bodies CALL across-crate helpers (`try_branch`, `advance_or_pop_with`, `push_compound`), each of which LLVM refuses to inline across the crate boundary even under workspace LTO when the callee's body exceeds the inline-threshold after recursion-bound unrolling. The W2.1 admission at `FINAL-IV.md:108`: "W2.1 deliberately kept this helper as a cold call (~250-line SY reducer)". `advance_or_pop_with` is ~400 LOC with a 7-arm match of its own. The per-byte cost is the cross-crate call to `advance_or_pop_with`, not the outer `match cur` dispatch.

This is not an inline-budget failure. It's **non-uniform per-state body size**. bbnf-regex succeeds because every state body is a 3-line match. The DTA fails because the 14 variants carry bodies ranging from 40 to 400 LOC. Inlining ALL of them would blow past any reasonable threshold; inlining the small arms but calling out to the large ones keeps the dispatch floor.

The only honest mitigations:

1. **Collapse every arm to one universal shape** — this is precisely what shape-emitter does (§6).
2. **Splice every helper body into every arm** (AW-IV.W2.1 binding rule re-articulated). Generated.rs grows to ~80-100K lines per grammar. W2.1 refused to splice `advance_or_pop_with`; FINAL-IV lists that refusal as a carry-forward (`FINAL-IV.md:147`). If fully shipped this would likely beat the gate — but it would require per-arm splice of ShuntingYard reducer (250 LOC × state_count), Repeat counter logic, and AltLinear rollback. 500-state BBNF with full splice = 100K+ lines per-arm × 500 ≈ 50M-line function body. LLVM would reject this; the proposal is unworkable at scale.
3. **Split variants along uniformity lines** — precisely shape classification.

## 5. What the W2.1 prototype teaches — shape count ≪ state count

Per `AW-V-W2-close.md:42-43` — the prototype has **TWO hot symbols** carrying 94.5% self-time: `parse_value::<ValueVisitor>` at 91.15%, `parse_string_escaped` at 3.36%. The other per-shape functions (`parse_object`, `parse_array`, `parse_number`, `skip_space`) inline INTO `parse_value`. The crate has 8 fns in `lib.rs` (counted at lines 94, 152, 181, 212, 258, 308, 351, 370, 382).

The lesson: JSON's FIVE shapes (value, object, array, string, number) inline cleanly because each shape's body is a uniform `match first_byte { ... }`. The dispatch is over `b = input[pos]`, not over a tagged-union enum. The body shape IS a mini-DFA — value's body is exactly the top-level `match` in a DFA state, where each byte class selects a different "shape state."

Shape count ≪ state count for a provable reason: a state is the result of LIFTING a rule's compiled byte-driven dispatch; a shape is a TEMPLATE that factors out the byte-driven dispatch from the rule body. Ten rules that all do "recursive compound with k→v key-value pairs" produce 10 DTA states (one per rule's Seq) but 1 Object-shape fn. The emitter re-specialises by grammar at type-generic-params, not by state-id. `parse_value<V: JsonVisitor>` is one symbol even for an arbitrarily complex input.

BBNF has 496 DTA states but the shape taxonomy (`shape_dispatch/mod.rs` ShapeTag enum, `~23 occurrences`) resolves to 6 shapes: `Object, Array, String, Number, Keyword, Scalar`. Add W4's Pratt and Unordered shapes → 8 shapes total. 8 shape-fns per grammar × ≤5 grammars = ~40 monomorphisations worst-case. Each is tiny because the shape's parameter captures the byte-dispatch table; the dispatch becomes a const-folded jump table at the call site after specialisation.

## 6. Archaeology on W5.6 "codegen-specialised walker"

The "W5.6 codegen-specialised walker" shipped as **AW-III.W4** (not W5.6), with these commits (from `git log -- 'crates/core/src/backend/rust/emitter/dta_walker/'`):

- `9581ea09` AW-III.W4.b — "mechanical state lowering scaffold"
- `0802c6ce` AW-III.W4.d — "inline-lower every DtaState arm"
- `316892d6` AW-III.W4.d — "const DTA_TABLE binding + Seq fast-path + generic scanner"
- `0741d484` AW-III.W1 — "W1 wave-close integration"
- `96a955cf` AW-IV.W1.4-aggro — "inline-emit DFA loop body into Regex + WsTrim arms"
- `b96be94c` AW-IV.W2.1 — "inline-emit emit_leaf + close_compound into Literal/Regex/Seq/Repeat arms"
- `a62057b4` AW-IV.W2.1 — "inline-emit handle_repeat_failure into main-loop error handler"
- `465a9f2c` AW-IV.W4.3 — "bloom+GADT consumer in dedup-eligible compound-emit"

The codegen-specialised walker **DID land**. `nm` verifies dispatch_one absent from hot path (`FINAL-IV.md:23`). Idempotent bootstrap holds. Workspace tests 1412/0/36. The W4 breakthrough (+131% on tailwind 4c, `FINAL-IV.md:57`) proves the substrate works at scale on parallel-forkable workloads.

What **did not land**: per-arm splice of `advance_or_pop_with`, string-payload PSI elision, ShapeRef consumer in `close_compound`, Pratt LUT cold-path deletion (`FINAL-IV.md:147-153`). The W6 final gate missed because those residuals carry the dispatch-floor-equivalent cost out-of-line. Per `FINAL-IV.md:105-115`: "the architectural transposition landed verifiably across every wave … the throughput translation did not materialise."

The W5.6 codegen walker did not "abandon" or "block". It **succeeded structurally and missed numerically** because the per-variant body-size non-uniformity made the complete inline-splice commitment unworkable within the code-size budget the team tolerated.

## 7. Theoretical-vs-practical verdict

An idealised automaton compiler (one that fully inlines every variant body per state-id) would equal the W2.1 prototype — because at that limit the DTA's per-state specialisation converges onto per-shape specialisation with redundant copies. A 50-state Seq-heavy grammar with full per-state body splice would emit 50 identical Seq-arm bodies, each specialised on the children-slice. The shape-emitter is the **deduplicated** form of the fully-inlined DTA: same mechanism, fewer symbols.

**simdjson/sonic-rs lesson**: simdjson's stage 1 IS a compiled automaton (the structural-character-finder DFA specialised over `{}[]",:`, `_pdep`/`_pext` hardware primitives) — and it wins. Sonic-rs's stage 2 is hand-written RD over the structural index — and it also wins. What they share is **uniform per-state body shape at the hardware level**: simdjson's stage-1 state is a 32-byte SIMD vector compare; sonic-rs's stage-2 state is a `match first_byte`. Both are shapes-per-byte, not variants-per-rule.

The compiled-automaton approach wins when state transitions uniformly read one byte and write one state. It fails when state transitions do irregular things (push frames, manage counters, run reducers). The DTA is unavoidably the second kind because grammar rules are unavoidably non-uniform.

## 8. Recommendation

**Do not invest in a general automaton compiler.** The W4.d / W1.4-aggro / W2.1 work already proved it: specialised walker emits, inlines, and verifies absent-symbol attribution. The residuals that gated throughput are not automaton-compiler problems — they are code-size-vs-inline tradeoffs on non-uniform variant bodies, which any general automaton compiler would re-encounter identically.

**Accept the shape-emitter path as honest Path B.** The `aw3-r6-path-b-rip-dta.md:152-163` observation is correct: "the difference between 'inline the DtaState match per grammar' and 'emit a function per rule' is semantic, not architectural." The shape-emitter is the third thing between them — it emits functions per *shape* (not per state, not per rule), taking the uniform-dispatch win from automaton compilation and the deduplication win from templates. The W2.1 prototype at 91.15% self-time on one symbol, with `nm` showing DTA substrate absent (`AW-V-W2-close.md:77-79`), is the existence proof.

**Preserve DTA as AX cold-path substrate.** The DTA_TABLE const survives as the replay surface. AX's decision-log, incremental reparse, and snapshot features need a deterministic state-id abstraction; the DTA_TABLE provides it. Hot path routes through shape-emitter; cold path consults DTA_TABLE. No hybrid; the hot path is shape-emitter only.

The compiled-automaton question resolves to: automata compile well when states are uniform. DTA states are not uniform; that non-uniformity is structurally inherent to multi-grammar rule lifting. The shape-emitter re-introduces uniformity by partitioning states along body-shape equivalence classes. That is the honest architectural answer.
