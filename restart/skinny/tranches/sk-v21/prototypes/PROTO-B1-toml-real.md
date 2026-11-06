# PROTO-B1 — TOML end-to-end against the REAL (un-forked) architecture

Base of record: the skinny submodule at `efc6b875f` (the adjudication's base, with the un-forked
`general_descent` emitter). NOTE ON WORKTREE SKEW: the dispatched worktree's skinny submodule was
stale at `d2b32f6e3` (17 commits behind — the *pre*-un-fork `RuntimeEmitterKind` tree PROTO-P7 tested).
I advanced the submodule to `efc6b875f` inside the worktree (`git merge-base --is-ancestor d2b32f6e3
efc6b875f` → true; `efc6b875f` is on skinny `master`) so this prototype tests the architecture the
adjudication and this brief describe. The `general_descent` directory does NOT exist at `d2b32f6e3`;
it does at `efc6b875f` (`emit.rs`, `regex_first.rs`, `view.rs`, `mod.rs`).

Measurement discipline: this is a correctness probe — no ns quoted. Environment for the record:
scratchpad round-trip crate built in 2.89s; run loadavg `{24.50 21.37 16.62}` (host baseline noise per
the session protocol; irrelevant to a boolean round-trip).

## Verdict in one line

**Both true walls are fixable with GENERIC, Lock-14-clean primitives, and emitted-shape TOML
round-trips 7/7 against the `toml` crate.** WALL A (whitespace policy) and WALL B (leaf matcher) are
proven with a runnable prototype: a bounded regex interpreter + a per-grammar ws-class, driving an
emitter-shaped TOML descent that builds `toml::Value` and compares equal to `toml::from_str` on a
serious corpus. WART C (the `has_rule("value")` name-gate) is confirmed *deeper than cosmetic* — the
JSON template family is name-coupled in two places — so its correct neutralization is a FOLD of JSON
into `general_descent`, not a structural JSON-sniffer; that fold is a bounded TW6 edit, not a fork.

## The three walls, re-confirmed at `efc6b875f` (file:line)

- **WALL A — `skip_ws` is hardcoded to ALL ascii whitespace, injected uniformly.** `skip_ws`
  (`general_descent/emit.rs:126-135`) loops on `byte.is_ascii_whitespace()` — includes `\n`. It is
  emitted at every rule entry (`emit.rs:272`) and between EVERY `Seq` element (`emit.rs:303`), with the
  comment "uniform between sequence elements in this whitespace-insensitive grammar." TOML is
  newline-SIGNIFICANT; this swallows the `\n` that terminates a key/value pair and a table header.
- **WALL B — `match_leaf` is a delimiter scan, no regex engine.** `match_leaf` (`emit.rs:157-221`)
  consumes a leaf as the maximal run of the text-class `[A-Za-z0-9_.$]` (`is_text_byte`, emit.rs:226-228)
  with `""`/`''` doubled-quote escapes only. `regex_first.rs:1-10` and `general_descent/mod.rs:17-27`
  state plainly: "the runtime has no general regex matcher." `bbnf-regex` (`crates/bbnf-regex/src/lib.rs`,
  322 lines) is a classifier with NO `is_match`/DFA/NFA (verified: zero matcher symbols). So a bounded
  interpreter is the only path — confirmed by the brief's own fallback clause.
- **WART C — `has_rule("value")` name-gate.** `emit_sink_only` (`runtime_generator.rs:98`) routes on the
  literal rule name `value`. DEEPER than the adjudication framed it: the JSON emitter ALSO keys on the
  name internally — `json_sink_direct/mod.rs:98` does `.find(|rule| rule.name == "value")`. So the JSON
  family is name-coupled at two sites, and TOML's `value` Alt (which dispatches `array`/`inline_table`,
  i.e. `[`-led and `{`-led branches) structurally *resembles* JSON's value dispatch — a structural
  sniffer would misclassify it. The clean neutralization is to delete BOTH name couplings and route the
  JSON family through `general_descent` (which already walks an arbitrary `SinkOnlyExpr` tower).

## Where the ws policy actually lives (the WALL A generic-fix substrate)

The `?w` modifier is NOT lost at the frontend: `parse_seq` (`grammar/src/lib.rs:1366-1378`) lowers `?w`
to an explicit, POSITIONAL `ExprKind::Annotation{ name: "ws", value: None }` sequence element. Sheets
uses it (`grammar/google-sheets/google-sheets.bbnf:160-161`); JSON and TOML instead thread whitespace
as explicit `ws`/`skip`/`wsn` rules (`skinny/grammars/json.bbnf:6` `ws = /[ \t\n\r]*/`; TOML `ws =
/[ \t]*/`). So the grammar carries a *derivable* signal: JSON's ws-class includes `\n`, TOML's does not.
The single generic bit the emitter needs is `newline_significant` — true iff the grammar carries a
structural newline rule (TOML's `newline = /\r?\n/`, referenced by `line`, not as trivia). That bit
selects the `skip_ws` byte-class. My prototype's `WsClass::inline()` (excludes `\n`) vs
`WsClass::all_whitespace()` (the emit.rs:126-135 behaviour) IS exactly that derived bit, and the
falsifier below shows it is load-bearing.

## The runnable prototype (scratchpad, isolated target — nothing built in a reference tree)

`/private/tmp/.../scratchpad/tomlproto/` — a `toml = "=0.8.23"` crate (the oracle, already in the skinny
lockfile). `src/main.rs` contains:

1. **`mod regex` — the WALL B fix.** A bounded backtracking interpreter: pattern → AST (`Class`(256-bit
   bitmap) / `Seq` / `Alt` / `Repeat{min,max,greedy}`), matched by CPS (`m`/`m_seq`/`rep`) with a
   no-progress guard. Handles char-classes with ranges + negation + `\s`/`\S`/`\d` shorthands, groups,
   `(?:…)`, alternation, `* + ? {n} {n,m}`, and non-greedy `*?`. **Zero grammar-named code** — it
   consumes any pattern string. This is the drop-in for `match_leaf`'s delimiter scan.
2. **`skip_ws(input, pos, WsClass)` — the WALL A fix.** The skip class is a parameter, not
   `is_ascii_whitespace()`. `WsClass::inline()` omits `\n`.
3. **`mod toml_descent`** — what the FIXED emitter GENERATES from the `SinkOnlyExpr` tower, written in
   the emitter's idiom (`skip_ws(class)` / `match_literal` / leaf-via-`regex`), building `toml::Value`.
   The leaf patterns are verbatim the TOML grammar's regex leaves.

### Result (exit 0)

```
[PASS] scalars + newline significance
[PASS] strings: escapes, literal, multiline
[PASS] datetimes (RFC3339 family)
[PASS] arrays incl. multiline + nesting
[PASS] dotted keys + std tables + inline tables
[PASS] array of tables
[PASS] quoted keys + comments + blank lines

--- WALL A falsifier (buggy all-whitespace skip class) ---
[EXPECTED-BREAK] buggy class errors on scalars + newline significance:
    expected newline after value at byte 21 (found Some('c')) — newline significance lost?

--- WALL B micro-probe (leaves match_leaf's delimiter scan truncates) ---
  [MATCH] RFC3339 offset dt          (1979-05-27T07:32:00-07:00 -> end 25)
  [MATCH] escaped basic string       ("a\tb\"c"  full-length)
  [MATCH] multiline basic (non-greedy)  ("""x\ny"""  non-greedy *? closes correctly)
  [MATCH] hex integer                (0xDEAD_BEEF -> end 11)
  [MATCH] float sci                  (1e10 -> end 4)

=== round-trip: 7 pass / 0 fail ===
```

The 7 corpus documents cover: newline-terminated scalars (dec/hex/oct/bin ints, `_` separators, floats,
sci, `-inf`, bools), basic-string escapes incl. `\u`/`\U`, literal + multiline (`"""`/`'''`) strings,
the full RFC3339 datetime family (offset/frac/local-dt/date/time), multiline+nested arrays with trailing
commas and interspersed `#` comments, dotted keys, `[std.tables]`, inline tables, `[[array.of.tables]]`,
quoted keys, and blank/comment lines. Every value equals the `toml` crate's parse.

**Decisive WALL A diff (same input, same code, only the derived ws-class differs):**
`WsClass::inline()` → 7/7 round-trip; `WsClass::all_whitespace()` (the current
`emit.rs:126-135`) → hard misparse at the first newline (`byte 21`, the `\n` after `count = 42`, glues
`neg` onto the prior value). The class is the whole fix; it is a one-bit grammar derivation.

**Decisive WALL B evidence:** every leaf the delimiter scan truncates (adjudication WALL B: RFC3339
dates truncate at the first `-`/`:`; `\"`-escaped strings close early; `"""…"""`/`[\s\S]*?`) is matched
full-length by the interpreter. The non-greedy `*?` is the sharpest case — a greedy `[\s\S]*"""` would
swallow past the first closer; the interpreter's non-greedy `rep` stops at the first `"""`.

## Is every fix generic (Lock-14-clean, zero grammar-named code)?

- **WALL A — YES.** The fix is: derive `newline_significant` (one bit) from the grammar's structural
  newline rule, and thread the resulting ws-class into `skip_ws`. No rule name; no per-grammar branch.
  In-tree shape: add a `WsPolicy` to `SinkOnlyProgram` (computed in `lower/sink_only.rs` from the
  `RegexProgram` leaves), and replace `emit.rs:126-135`'s `is_ascii_whitespace()` with a class constant
  the prelude emits from `WsPolicy`. (For grammars that use `?w` rather than explicit ws rules — Sheets
  — the same policy retains the current all-ws class, so Sheets is untouched.)
- **WALL B — YES.** The interpreter is grammar-neutral (consumes pattern strings). It DELETES
  `match_leaf`'s `is_text_byte`/`consume_quoted` approximation (emit.rs:157-247) and the
  `LEAF_STOP`-alphabet heuristic (emit.rs:88-101). `regex_first.rs` (first-byte derivation) becomes a
  redundant optimisation, not a correctness crutch. No grammar-named code.
- **WART C — GENERIC BY DELETION, not by sniffer.** Correct neutralization = delete the
  `has_rule("value")` gate (`runtime_generator.rs:98`) AND the `rule.name == "value"` coupling
  (`json_sink_direct/mod.rs:98`), routing the JSON family through `general_descent`. The prototype is
  positive evidence this fold is feasible: its general descent already round-trips JSON-shaped
  constructs (inline tables = `{`-objects, arrays = `[`-arrays) via the same generic primitives. This is
  a bounded TW6 edit (retire the JSON templates), squarely within `general_descent`'s stated remit
  ("GENERAL over an arbitrary `SinkOnlyExpr` tower", mod.rs:8-9).

## Does emitted TOML round-trip? — honest scope

**The emitted CODE SHAPE round-trips 7/7** (proven: the descent uses ONLY the two generic primitives +
`match_literal`, exactly what the fixed emitter emits per `emit.rs`). What is NOT executed here is the
in-tree REGEN pipeline wiring (thread `WsPolicy` through `lower_program`; swap the interpreter into the
`emit_expr::RegexProgram` arm; fold JSON). That wiring is mechanical and I scoped it above to exact
lines; it is the TW6 execution gate, and it is NOT a wall — the two defects the adjudication located are
the walls, and both are dissolved by generic primitives demonstrated runnable here.

## What this prices for Lock-14 / TW6

- **Lock-14 must key on `toml::from_str` round-trip parity, not directory paths** (as the adjudication
  and PROTO-P7 both concluded). The oracle is free and already in the lockfile; the harness is the ~40
  lines of `to_toml` + corpus in this prototype.
- **TW6 scope, exact:** (a) `WsPolicy` bit in `SinkOnlyProgram` + class-parameterized `skip_ws`
  (emit.rs:126-135,272,303); (b) bounded regex interpreter in place of `match_leaf` (emit.rs:157-247);
  (c) fold JSON into `general_descent`, deleting the two `"value"` name couplings
  (runtime_generator.rs:98, json_sink_direct/mod.rs:98). Zero per-grammar emitter files. No fork.

## Reproduction

```
cd /private/tmp/.../scratchpad/tomlproto
CARGO_TARGET_DIR=../toml-target cargo run --offline      # exit 0 == 7/7 round-trip + all probes
```
(The skinny submodule in the worktree is at `efc6b875f`; `general_descent/` is present there and absent
at the stale `d2b32f6e3` the worktree shipped with.)

## Convergence — TOML-generalization plank: 72/100

Up from the adjudication's 58: both walls the adjudication located are now demonstrated dissolvable by
GENERIC runnable primitives (bounded interpreter, one-bit ws-policy), with a 7/7 `toml`-crate round-trip
of the emitted code shape and a live WALL A falsifier. Held below full because the in-tree regen wiring
+ JSON fold are specified-and-argued but not executed here (the true quiet in-tree regen remains the
TW6/TW0 gate), and WART C is confirmed a two-site name-coupling requiring a fold rather than the cosmetic
rename the earlier docs implied.
