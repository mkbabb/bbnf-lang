# LOCK-5 · pass-3 · PROTO-1 (5D) — Recognizer::ByteClass end-to-end + the over-consumption correctness fix

**Family 5D (GRAMMAR-FRAGMENT). Probe 1: does the RATIFIED direct-scan-default
policy land in-tree with byte-identical tape, and does the FOLLOW-guard fix a REAL
WRONG parse the current masked-wildcard `match_leaf` produces?**

- **BASE-PIN.** Worktree HEAD started at **`ce51c36fcbdf5108eb028005586672d9a6d2ca1b`**
  (`git log --oneline -1` = `ce51c36fc locks(sk-v24): pass-1 bbnf CLOSED …`).
  `git merge-base --is-ancestor ce51c36fc HEAD` → **YES**. HEAD **IS** the pin
  exactly. **NOT wrong-artifact-risk.** XL-H = `ce51c36fc`. Every number below was
  measured on this HEAD.
- **loadavg.** `5.57 6.84 8.08` at start; test/build ran at `~5-6`. No perf claim
  is made in this probe (correctness + tape-identity + compile only), so load is
  recorded for provenance, not gating.
- **Commands (all RAN).**
  - `cargo test -p codegen --test byteclass_lower -- --nocapture` — **RAN, 1
    passed**, twice (before and after the substrate landed). Finished 0.00s
    (compile 0.75s).
  - `cargo build -p ir -p passes -p codegen` — **RAN, EXIT 0**, `Finished dev in
    1.23s`. The `SimdMode::ByteClass` / `Recognizer::ByteClass` /
    `BackendExpr::ByteClass` substrate + the `match_byteclass_guarded` emitter
    primitive compile in-tree.

---

## QUESTION

Two clauses:

1. **CORRECTNESS.** Does the current masked-wildcard `match_leaf`
   (`general_descent/emit.rs`, the greedy first-set / text-class run) produce a
   WRONG parse of the non-disjoint leaf `word = /[a-z]+/` inside
   `rule = word "and" word`, exactly as PROTO-1's `overlap=[a]` predicts
   (first(word) ∩ FOLLOW(word) ∋ `'a' = first("and")`)?
2. **SUBSTRATE.** Does `SimdMode::ByteClass` + `Recognizer::ByteClass` +
   `BackendExpr::ByteClass` + the FOLLOW-guarded direct scan land in-tree, and is
   its emitted tape BYTE-IDENTICAL to the hand-authored control
   `word = ([a-z] - "and")+`?

## METHOD (what actually ran)

**(a) The correctness counterexample — a RUNNING test.** `byteclass_lower.rs`
carries three `word` scanners, each a VERBATIM port of a recognizer primitive:

- `word_current` — the CURRENT `match_leaf` text-head branch (emit.rs:195-206):
  `first = [a-z]`, head is a text byte, body = the **maximal `is_text_byte` run**
  (`[A-Za-z0-9_.$]`). This is the greedy masked-wildcard scan.
- `word_byteclass` — the primitive this probe ADDS: `SimdMode::ByteClass([a-z])`
  membership + a **FOLLOW-guard** on the literal `"and"` (halt BEFORE any position
  that begins the FOLLOW token), `min=1`. This is the `Seq[Repeat, FOLLOW-guard]`
  the RATIFIED policy prescribes, collapsed into one scan.
- `word_control` — the hand-authored `([a-z] - "and")+`, specified from the
  class-DIFFERENCE (a lowercase letter at a position that does NOT begin `"and"`),
  **independently** of FOLLOW — so `byteclass == control` is a runtime finding,
  not a source-level tautology.

The three drive a faithful `rule = word "and" word` recognizer (entry ws-skip,
inter-element ws-skip per emit.rs:302-304, literal `"and"`, trailing-char check)
over a 20-input corpus mixing delimiter-free valid (`aandb`, `catanddog`, …),
invalid (`and`, `aand`, `aandbandc`, …), spaced controls (`a and b`, …), and bare
runs (`z`, `""`). The **tape** is modelled as the ordered `Vec<(offset, kind)>` of
leaf offset-pushes (`match_leaf` emits `tape.push_offset(start, kind)`; the literal
pushes nothing) paired with the parse result — a `ByteClass` leaf's tape emission
**is** that span. "Byte-identical tape" = `(tape, result)` equal on every input.

**Why a test, not a temp-crate compile of the emitted parser.** The generated
recognizer depends on the internal `crate::tape`/`super::view` runtime, so driving
the emitter's output string through a fresh compile is beyond a parsimonious probe.
The three scanners are byte-for-byte the primitives the emitter emits into every
`generated.rs` prelude (`skip_ws`, `match_literal`, `is_text_byte`, the `match_leaf`
text branch) plus the `match_byteclass_guarded` primitive this probe writes into
that same prelude — so the test exercises the identical span/tape semantics.

**(b) The substrate — LANDED and COMPILES.** Added, and building green:
`ir::SimdMode::ByteClass([u64;4])`, `ir::Recognizer::ByteClass { set, follow,
site }`, `ir::BackendExpr::ByteClass { set, min, max, follow, span_kind }` (beside
`RegexProgram`), plus `byteset_from_ranges` / `byteset_contains` helpers.
`BackendExpr::ByteClass` lowers to `SinkOnlyExpr::ByteClass` (`lower/sink_only.rs`)
and routes through **both** requested emitters: `lower/tape_plan.rs`
(`scan_byteclass_guarded` op) and `general_descent/emit.rs` — the latter emits a
call to the new `match_byteclass_guarded` prelude primitive (identical loop to
`word_byteclass`: membership run, FOLLOW-guard halt, `min` floor,
`push_offset`). All exhaustive-match sites (sink_only, view, first_set, codegen
lib, passes ×3) extended. `[u64;4]` is serde-native, so `BackendExpr`'s
`Serialize/Deserialize` derives hold with no new dep.

**Scope honesty on the derive gate.** The full FIRST/NULLABLE/FOLLOW fixed-point
that DECIDES which leaves are non-disjoint (and therefore get `ByteClass` instead
of the masked `RegexProgram`) is the LLVM-style `while changed` closure already
delivered at `passes/tests/follow_sound_fraction.rs`; `derive_recognizers`
(`passes/src/lib.rs:338`) was NOT rewired to call it under the 50-min cap — the
substrate + emitter now EXIST to receive its verdict, and the semantics that
verdict selects are the ones the test proves. This is the one designed-not-wired
seam; it does not touch either clause of the gate below.

## THE NUMBERS (RAN — `byteclass_lower`, 1 passed)

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
  compile** (`cargo build -p ir -p passes -p codegen`, EXIT 0).

⇒ **FOLLOW-guard is a correctness fix + the substrate lands; LOCK-9-9C unblocked.**

**VERDICT LINE:** `current match_leaf WRONG on 12/20 delimiter-free inputs (incl.
aandb: over-consumes [0,5), rejects a valid parse); ByteClass+FOLLOW-guard parses
correctly with tape byte-identical to ([a-z]-"and")+ 20/20; SimdMode/Recognizer/
BackendExpr::ByteClass + match_byteclass_guarded land in-tree and compile
(EXIT 0) ⇒ FOLLOW-guard is a correctness fix + substrate lands, LOCK-9-9C
unblocked (RAN).`

## HONESTY / SCOPE

- The test's three scanners are verbatim ports of the emitter's prelude primitives
  (current) and the new `match_byteclass_guarded` (fix); they exercise the emitted
  semantics without a temp-crate compile of the runtime-coupled `generated.rs`.
- `BackendExpr::ByteClass` carries a `follow: Vec<u8>` field beyond the brief's
  `{set,min,max,span_kind}` sketch — a stop-guard is inert without the token it
  halts before, so the FOLLOW literal must travel with the node (the
  `Seq[Repeat, FOLLOW-guard]` fusion). `max` is retained for `{m,n}` bounds though
  unused by `+`/`*`.
- `derive_recognizers` was NOT rewired onto the FOLLOW fixed point under the cap
  (the one designed-not-wired seam); the closure already exists at
  `passes/tests/follow_sound_fraction.rs`. The substrate + emitter it feeds are
  now present and green.
- Both clauses of the gate ran; nothing here is SPEC_ONLY.

## FILES

- `byteclass_lower.rs` — the RAN test (delivered as part of `PROTO-1-5D.patch`).
- `PROTO-1-5D.patch` — unified diff: IR variants + helpers, `SinkOnlyExpr::ByteClass`
  lowering, `tape_plan` + `emit.rs` routing (incl. `match_byteclass_guarded`), all
  exhaustive-match extensions, and the test.
- `PROTO-1-5D-test-run.txt` / `PROTO-1-5D-build.txt` — captured stdout.
