# LOCK-5 · pass-3 · PROTO-1 (family 5D) — Recognizer::ByteClass end-to-end + the over-consumption correctness fix

**Family 5D (GRAMMAR-FRAGMENT). Probe 1. QUESTION: does the RATIFIED
direct-scan-default policy land in-tree with byte-identical tape, and does the
FOLLOW-guard fix a REAL WRONG parse the current masked-wildcard `match_leaf`
produces?**

This pass-3 probe **independently re-ran** the substrate + counterexample in the
worktree and confirms both clauses on a **genuine fresh compile** (not a warm
artifact) — the `Compiling ir/grammar/passes/codegen` lines below are the proof.

---

## BASE-PIN (measured, per XL-H)

- `git log --oneline -1` -> `ce51c36fc locks(sk-v24): pass-1 bbnf CLOSED + pass-2 pt CLOSED …`
- Exact HEAD measured on: **`ce51c36fcbdf5108eb028005586672d9a6d2ca1b`**
- `git merge-base --is-ancestor ce51c36fc HEAD` -> **YES**. HEAD **IS** the pin
  exactly (zero commits ahead). **NOT wrong-artifact-risk.** XL-H = `ce51c36fc`.
  Every number below was measured on this HEAD.
- **loadavg.** `6.54 8.05 7.51` at probe start; the forced substrate recompile ran
  at `4.64 7.03 7.21`. No perf claim is made (correctness + tape-identity +
  compile only), so load is recorded for provenance, not gating.

## COMMANDS (all RAN)

| command | result |
|---|---|
| `cargo test -p codegen --test byteclass_lower -- --nocapture` | **RAN, `test result: ok. 1 passed`**, `finished in 0.00s` |
| `cargo build -p ir -p passes -p codegen` (after `touch`-ing the 5 substrate files) | **RAN, EXIT 0**, genuine recompile `Compiling ir -> grammar -> passes -> codegen`, `Finished dev in 0.54s` |

---

## QUESTION (two clauses)

1. **CORRECTNESS.** Does the current masked-wildcard `match_leaf`
   (`general_descent/emit.rs`, the greedy first-set / text-class run) produce a
   WRONG parse of the non-disjoint leaf `word = /[a-z]+/` inside
   `rule = word "and" word`, as PROTO-1 predicts
   (first(word) INTERSECT FOLLOW(word) contains `'a' = first("and")`)?
2. **SUBSTRATE.** Does `SimdMode::ByteClass([u64;4])` + `Recognizer::ByteClass` +
   `BackendExpr::ByteClass` + the FOLLOW-guarded direct scan land in-tree, and is
   its emitted tape BYTE-IDENTICAL to the hand-authored control
   `word = ([a-z] - "and")+`?

## METHOD (what actually ran)

**(a) The correctness counterexample — a RUNNING test.**
`skinny/crates/codegen/tests/byteclass_lower.rs` drives three `word` scanners,
each a VERBATIM port of an emitter prelude primitive, through a faithful
`rule = word "and" word` recognizer (entry ws-skip, inter-element ws-skip per
emit.rs Seq, literal `"and"`, trailing-char check) over a 20-input corpus:

- `word_current` — the CURRENT `match_leaf` text-head branch (emit.rs:193-217):
  head `'a'` is a text byte, so the body is the **maximal `is_text_byte` run**
  `[A-Za-z0-9_.$]`. The greedy masked-wildcard scan — it cannot see the FOLLOW
  delimiter `"and"`.
- `word_byteclass` — the primitive this probe ADDS (`match_byteclass_guarded`,
  emit.rs:264-294): `[a-z]` 256-bit membership + a **FOLLOW-guard** that halts
  BEFORE any position that begins `"and"`, `min=1`. The `Seq[Repeat, FOLLOW-guard]`
  the RATIFIED policy prescribes, collapsed into one scan.
- `word_control` — the hand-authored `([a-z] - "and")+`, specified from the
  class-DIFFERENCE (a lowercase letter at a position NOT beginning `"and"`),
  **independently** of FOLLOW — so `byteclass == control` is a runtime finding,
  not a source-level tautology.

The tape is modelled as the ordered `Vec<(offset, kind)>` of leaf offset-pushes
(`match_leaf` / `match_byteclass_guarded` emit `tape.push_offset(start, kind)`;
the literal pushes nothing) paired with the parse result — a `ByteClass` leaf's
tape emission **is** that span. "Byte-identical tape" = `(tape, result)` equal on
every input. (Why a test, not a temp-crate compile of the emitted parser: the
generated recognizer depends on the internal `crate::tape`/`super::view` runtime;
the three scanners are byte-for-byte the primitives the emitter emits into every
`generated.rs` prelude, so the test exercises the identical span/tape semantics
without a runtime-coupled compile.)

**(b) The substrate — LANDED and FRESH-COMPILED.** Present in-tree and recompiled
green this run:
- `ir::SimdMode::ByteClass([u64;4])` (ir/src/lib.rs:447-451),
  `ir::Recognizer::ByteClass { set, follow, site }` (:439-443),
  `ir::BackendExpr::ByteClass { set, min, max, follow, span_kind }` beside
  `RegexProgram` (:418-424), plus `byteset_from_ranges` / `byteset_contains`
  helpers (:456-469). `[u64;4]` is serde-native — the derives hold with no new dep.
- `BackendExpr::ByteClass` lowers to `SinkOnlyExpr::ByteClass`
  (`lower/sink_only.rs`) and routes through **both** requested emitters:
  `lower/tape_plan.rs` (`scan_byteclass_guarded` op) and
  `general_descent/emit.rs` (a call to the new `match_byteclass_guarded` prelude
  primitive — membership run, FOLLOW-guard halt, `min` floor, `push_offset`).
- All exhaustive-match sites extended (sink_only, general_descent/view,
  json_sink_direct/first_set, codegen/lib, passes x3).
- **Positive-class gate honoured.** `match_byteclass_guarded` fires only for a
  POSITIVE membership set; the `follow` field is empty when the leaf is genuinely
  disjoint (no guard emitted). A negated body does NOT route here — it stays on
  the masked path, so it never falsely claims to prefilter.

**Scope honesty on the derive gate.** The full FIRST/NULLABLE/FOLLOW fixed point
that DECIDES which leaves are non-disjoint (and therefore get `ByteClass` rather
than the masked `RegexProgram`) is the LLVM-style `while changed` closure already
delivered at `proto-1-overapprox/follow_sound_fraction.rs` (mirrored at
`skinny/crates/passes/tests/follow_sound_fraction.rs`). `derive_recognizers`
(`passes/src/lib.rs:338`) was NOT rewired to invoke it under the 50-min cap — the
substrate + both emitters now EXIST to receive its verdict, and the semantics that
verdict selects are the ones the test proves. This is the one designed-not-wired
seam; it touches neither clause of the gate below.

## THE NUMBERS (RAN — `byteclass_lower`, 1 passed; full stdout in PROTO-1-5D-test-run.txt)

```
(A) byteclass tape == control tape : 20/20 inputs
(B) aandb  current  =([(0,7)],        Err(5))   <- OVER-CONSUMES span [0,5), REJECTS
           byteclass=([(0,7),(4,7)],  Ok(5))    <- a / and / b, CORRECT
           control  =([(0,7),(4,7)],  Ok(5))    <- byte-identical to byteclass
(C) current != control on 12/20 inputs:
    aandb, xandy, catanddog, sandwich, bananaandx, and, aandbandc,
    aandband, andand, aaandbb, qandr, aandbc
    spaced controls (a and b / cat and dog / x and y): current == byteclass == Ok
```

**Reading.** On the delimiter-free `aandb` the current masked scan consumes the
whole `[a-z]` run `"aandb"` as the first `word` (span `[0,5)`), so the required
`"and"` delimiter is gone and the valid sentence a/and/b is **REJECTED**
(`Err(5)`) — the exact wrong parse `overlap=[a]` predicts. The `ByteClass`
FOLLOW-guard yields `word="a"` at offset 0, matches `"and"`, yields `word="b"` at
offset 4 (`Ok(5)`), tape byte-identical to the hand-authored `([a-z]-"and")+` on
**20/20** inputs. The over-consumption is not a one-off: current disagrees with the
control on **12/20** inputs. The three spaced controls confirm the bug is
specifically the delimiter-free overlap — where whitespace already delimits, both
lowerings agree.

## VERDICT

**PASS on both clauses.**

- current emit **WRONG** on the delimiter-free overlap (12/20, incl. `aandb`,
  over-consuming span `[0,5)` and rejecting a valid sentence) — the FOLLOW-guard is
  a genuine **correctness fix**, not a perf nicety.
- `ByteClass` + FOLLOW-guard parses the counterexample **correctly** and emits a
  tape **byte-identical** to the hand-authored `([a-z]-"and")+` on **20/20**.
- the `SimdMode::ByteClass` / `Recognizer::ByteClass` / `BackendExpr::ByteClass`
  substrate + the `match_byteclass_guarded` emitter primitive **land in-tree and
  compile FRESH** (`cargo build -p ir -p passes -p codegen`, genuine
  `Compiling …`, EXIT 0).

=> **FOLLOW-guard is a correctness fix + the substrate lands; LOCK-9-9C unblocked.**

**VERDICT LINE:** `current match_leaf WRONG on 12/20 delimiter-free inputs (incl.
aandb: over-consumes [0,5), rejects a valid parse); ByteClass+FOLLOW-guard parses
correctly with tape byte-identical to ([a-z]-"and")+ 20/20; SimdMode/Recognizer/
BackendExpr::ByteClass + match_byteclass_guarded land in-tree and fresh-compile
(EXIT 0, loadavg 4.64/7.03/7.21) => FOLLOW-guard is a correctness fix + substrate
lands, LOCK-9-9C unblocked (RAN, measured on HEAD ce51c36fc).`

## HONESTY / SCOPE

- **RAN, not SPEC_ONLY.** Both clauses of the gate ran; the numbers are from the
  actual test binary and a genuine (non-warm) recompile of the substrate chain.
- The test's three scanners are verbatim ports of the emitter's prelude primitives
  (current) and the new `match_byteclass_guarded` (fix); they exercise the emitted
  span/tape semantics without a temp-crate compile of the runtime-coupled
  `generated.rs`.
- `BackendExpr::ByteClass` carries a `follow: Vec<u8>` field beyond the brief's
  `{set,min,max,span_kind}` sketch — a stop-guard is inert without the token it
  halts before, so the FOLLOW literal must travel with the node (the
  `Seq[Repeat, FOLLOW-guard]` fusion). `max` is retained for `{m,n}` bounds though
  unused by `+`/`*`.
- `derive_recognizers` was NOT rewired onto the FOLLOW fixed point under the cap
  (the one designed-not-wired seam); the closure already exists at
  `proto-1-overapprox/follow_sound_fraction.rs`. The substrate + both emitters it
  feeds are now present and green.
- parse-that untouched (READ-ONLY honoured); no x86; target dir is the
  worktree-isolated `skinny/target` (no cross-agent lock contention).

## FILES

- `skinny/crates/codegen/tests/byteclass_lower.rs` — the RAN test.
- `PROTO-1-5D.patch` — unified diff: IR variants + helpers, `SinkOnlyExpr::ByteClass`
  lowering, `tape_plan` + `emit.rs` routing (incl. `match_byteclass_guarded`), all
  exhaustive-match extensions, and the test.
- `PROTO-1-5D-test-run.txt` / `PROTO-1-5D-build.txt` — captured stdout.
