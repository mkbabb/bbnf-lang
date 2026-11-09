# LOCK-5 · pass-3 · PROTO-1 — G-5D.4: the FOLLOW-sound one-pass fraction

**Family 5D (generator-path adjudicator). Probe 1 of the survivor's lowering policy.**

- **BASE-PIN measured on:** `HEAD = ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (worktree fast-forwarded
  from `d2b32f6e3`; `git merge-base --is-ancestor ce51c36fc HEAD` → YES). **Not** wrong-artifact-risk.
- **Command:** `cargo test -p passes --test follow_sound_fraction -- --nocapture` (skinny workspace).
- **Run:** RAN — 1 test, `ok`, wall 0.32 s. loadavg at measure 10.91 (context only; this is a
  structural fraction, not a perf claim, so no cold-paired timing is owed).
- **New code:** `skinny/crates/passes/tests/follow_sound_fraction.rs` (patch: `PROTO-1.patch`).

---

## QUESTION

Of the regex leaves in the live grammar corpus, what fraction are **FOLLOW-sound** for straight-line
one-pass lowering — `first(C) ∩ FOLLOW(leaf) = ∅` — and which leaves **REQUIRE** `Seq[Repeat,
FOLLOW-guard]`? This partitions incompatibility **I-1**: a high sound fraction makes 5D's direct
class-scan the default generator path and retreats the DFA to fallback; a low fraction (the Ragel
nondeterminism trap) keeps the DFA central. Unmeasured after two passes — the single number gating the
generator-path architecture (hand-evaluated ~92% under the *unsound* predicate; never run sound).

## METHOD (what actually ran)

1. **FOLLOW as a Changed-flag fixed point.** `nullable_closure`, `first_closure`, `follow_closure` each
   converge with an LLVM-style `while changed { … }` round loop over every `ExprId` (NOT a structural
   hash), dual to `first_bytes` at `skinny/crates/passes/src/lib.rs:911`. FOLLOW is computed
   **per-`ExprId`** so every `Regex` leaf gets its position-specific follow-set; `Ref` sites bridge
   FOLLOW back into the referenced rule's body (the inter-rule edge that forces the fixed point).
   Repeat bodies receive `FIRST(body) ∪ FOLLOW(repeat)` (loop re-entry ∪ exit). Sets are `BTreeSet<u8>`;
   disjointness is `is_disjoint`. **FOLLOW converged** for all three grammars (test terminates).
2. **first(C) via `bbnf_regex::analyze(pattern).first`** → `ByteSet256` → bytes, per the brief's
   operational definition. `FirstSet::Unknown` ⇒ unprovable ⇒ over-approximated to the full alphabet
   (sound: keeps FOLLOW / the verdict conservative).
3. **Corpus:** the in-tree `skinny/grammars/json.bbnf` (loaded, 15 rules) + `grammar/google-sheets/
   google-sheets.bbnf` (loaded under the skinny dialect, 39 rules) + the **injected counterexample**
   `word = /[a-z]+/ ; sentence = word "and" word ;`. 16 `Regex` leaves total.

## THE NUMBERS

```
[brief-literal] first(C)∩FOLLOW=∅        : disjoint = 3/16   frac = 18.8%
[bounded, give-back impossible by ctor]  :            6/16
[give-back-prone (trailing unbounded run)]: 10/16; of those FOLLOW-sound = 3/10 (30.0%)
[effectively one-pass = bounded ∪ prone-sound]:       9/16   (56.2%)
```

**Named non-disjoint list (13 leaves), adjudicated into three kinds.** The raw predicate is **sound but
not complete**: it flags leaves on their *leading* first-set even when give-back is structurally
impossible. Manual per-leaf adjudication (loop-class vs FOLLOW, follower obligation):

| leaf | pattern | overlap | kind |
|---|---|---|---|
| `counterexample::word` | `/[a-z]+/` | `[a]` | **GENUINE give-back** — follower literal `"and"` *requires* `'a'`; loop-class `{a-z} ∋ 'a'`. THE canonical trap. |
| `sheets::string` | `/"([^"]\|"")*"/` | `["]` | **GENUINE** — doubled-quote `""` escape: `"a""b"` is one-string-or-two ambiguous when a string can follow; closing-quote give-back. |
| `sheets::identifier` | `/[A-Za-z_][A-Za-z0-9_.]*/` | `[A-Za-z_]` | **GENUINE** — trailing `[A-Za-z0-9_.]*` overlaps FOLLOW letters (identifier abutting a letter-led construct, no delimiter). |
| `json::ws` | `/[ \t\n\r]*/` | `[SP \t \n \r]` | CONSERVATIVE — follower is `value`'s own *leading nullable* `ws` (same class, accepts ε); greedy-eat-all is correct. Completeness gap, not give-back. |
| `sheets::cell_ref` | `/\$?[A-Za-z]{1,3}\$?\d+/` | `[$]` | CONSERVATIVE — `'$'` is a *leading-optional* byte, absent from the trailing loop-class `\d`; FOLLOW has no digit → the real run is sound. |
| `sheets::range_end` | `/\$?\d+/` | `[$]` | CONSERVATIVE — same leading-`$` artifact; trailing `\d+` is FOLLOW-disjoint on digits. |
| `sheets::boolean` | `/[tT][rR][uU][eE]/` | `[T t]` | **BOUNDED** — fixed length 4, no trailing run; raw flag is a pure leading-first artifact. |
| `sheets::boolean` | `/[fF][aA][lL][sS][eE]/` | `[F f]` | **BOUNDED** — fixed `"false"`. |
| `sheets::let_call` | `/[lL][eE][tT]\(/` | `[L l]` | **BOUNDED** — delimiter-terminated `\(`. |
| `sheets::lambda_call` | `/[lL][aA][mM][bB][dD][aA]\(/` | `[L l]` | **BOUNDED** — delimiter-terminated `\(`. |
| `sheets::range_end` | `/\$?[A-Za-z]{1,3}/` | `[$]` | **BOUNDED** — bounded `{1,3}`, no unbounded run. |
| `sheets::formula` | `/=?/` | `[=]` | **BOUNDED** — optional single char. |
| `sheets::number` | `/(\d+\.?\d*\|\.\d+)([eE][+-]?\d+)?/` | all 256 | **OPAQUE** — `analyze()` returns `first=UNKNOWN`; over-approximated to all-bytes ⇒ forced non-disjoint. **Analyzer limitation, not a grammar property.** |

**Truly-SOUND under the raw predicate (3):** `json::number`, `json::string`, `sheets::sheet_prefix`
(all self-delimiting / non-overlapping leading byte — but see Caveat 1 for `sheet_prefix`).

## VERDICT

**PASS** — fraction reported AND the `word`/`"and"` counterexample lands NON-disjoint (`overlap=[a]`),
so the FOLLOW predicate is **SOUND exactly where `check_one_pass_eligible` (`one_pass.rs:64`) is not**:
its `Repetition` arm recurses into `[a-z]` and returns one-pass=`true`, greenlighting a straight-line
lowering that would reject `"a and b"`; the FOLLOW test refuses it. Asserted in-test
(`assert!(!cx_leaf.disjoint)` + `overlap.contains('a')`). FOLLOW converged.

**Verdict line:** `disjoint=3/16 frac=18.8%` (brief-literal); genuine-guard-required = **3/16 = 18.8%**.

## WHAT THE NUMBER MEANS FOR I-1 (the architecture gate)

The raw **18.8% sound** *reads* like the Ragel trap ("low fraction ⇒ DFA central"). It is a **red
herring**, because it is the sound floor of a *conservative* predicate that over-flags on leading-first:

- **6/16 leaves are bounded** — give-back is structurally impossible; one-pass by construction; the DFA
  is never needed for them irrespective of FOLLOW.
- **3/16 are genuinely non-disjoint** (`word`, `sheets::string`, `sheets::identifier`) — these and only
  these *require* `Seq[Repeat, FOLLOW-guard]` (or a DFA/backtracking fallback).
- **3/16 are conservative false-positives** (`json::ws`, `cell_ref`, `range_end/\$?\d+/`) — actually
  one-pass-safe; the raw predicate flags them but the loop-class is FOLLOW-disjoint / the follower
  re-consumes the same nullable class.
- **3/16 truly sound**, **1/16 analyzer-opaque** (`sheets::number`).

So **only 3 of 16 leaves genuinely keep the DFA in play**; ~12/16 are truly one-pass-safe. **I-1 resolves
toward the survivor's policy, NOT DFA-central:** the direct class-scan is the default generator path,
*gated by a FOLLOW-guard* (`desugar → Seq[Repeat, suffix]`, let grammar FOLLOW / `AltMode::Dispatch`
decide — exactly the REGISTRY fact-1 fix), with the DFA/backtracking retreating to a **narrow fallback
for the genuine-give-back trio.** The number that gates the architecture, read correctly, says the DFA
retreats.

## CAVEATS (the predicate is only as sound as its first(C) source)

1. **`analyze().first` UNDER-approximates alternations.** For `sheets::sheet_prefix`
   (`/'…'!|[A-Za-z_]\w*!/`) it returns `Exact {'\''}`, silently dropping the `[A-Za-z_]…` branch. A
   *sound* FOLLOW-sound verdict needs an **over-approximating** first-set (the DFA start-state class set,
   or a fixed-up analyzer). As-is, the 3 "SOUND" verdicts — `sheet_prefix` in particular — are
   **not reliably sound**; a production predicate must draw first(C) from an over-approximation.
2. **`sheets::number` is opaque** to `bbnf_regex::classify()` (recognizes only the JSON-shaped number):
   `first=UNKNOWN`. Its verdict is indeterminate pending a stronger first-set analysis, not a real
   give-back.
3. **The raw predicate over-flags on leading-first.** Give-back can only occur through a *trailing
   unbounded run*; the sharper metric (`give_back_prone`, sound: no unescaped top-level `*`/`+` ⇒
   bounded ⇒ no give-back) separates the 6 bounded leaves and yields **give-back-prone FOLLOW-sound =
   3/10 (30%)**. Completing the predicate (loop-class ∩ FOLLOW, follower-obligation) would move `ws`,
   `cell_ref`, `range_end` into the sound set → **effectively one-pass ≈ 12/16 (75%)**.
4. **Sheets loaded under the skinny parser subset** (39 rules; projections/directives absorbed). The
   `Regex` leaves themselves are faithfully captured; rule wiring for a few host-heavy constructs is
   approximate, which only affects FOLLOW breadth, never the counterexample gate.

## FILES

- `skinny/crates/passes/tests/follow_sound_fraction.rs` — the probe (new test module).
- `PROTO-1.patch` — unified diff of the above.
