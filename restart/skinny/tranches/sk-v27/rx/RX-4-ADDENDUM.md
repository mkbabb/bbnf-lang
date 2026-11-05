# LW1.RX-4: the two UNREAD regex objections, re-asked and ruled

**Status:** RULED on the restatement below. Neither objection's full text is on disk. The dispatch copy of the regex design was cut off at §A (PLAN §5, TOPO-0), and the only surviving words are the PLAN's: C1-9 is "(G-P1)" and C2-6 opens "Changing semantics breaks e…". Each ruling therefore rules the objection as restated here. When LW1.TOPO-0 files the full adjudicated text in `designs/`, this addendum is re-read against it. If the filed text asks for more than the restatement, a further addendum answers it. This file is not edited.

Evidence in this wave: the oracle smoke corpus (`ORACLE-SMOKE-ANSWERS.json`: 44 cases; V8 and JSC each self-consistent ×2; 44/44 cross-agree; 0 SPEC-AMBIGUITY), `LOOKAROUND-CENSUS.tsv`, and the born-RED tests in parse-that `rust/regex/tests/born_red_lw1.rs`.

## C1-9 (critic 1, on G-P1)

**Restated.** The regex performance gate G-P1 can show a win that is not a win. It compares our emitted DFA against a competitor engine that is running a different automaton or a different execution tier (a lazy DFA, a backtracker, a prefilter), so the measured difference cannot be attributed to the codegen.

**Ruling: ACCEPTED.** The PLAN already absorbs this as ADJ-1 at LW5.RX-1. This addendum fixes what that unit must show:

1. **One automaton on both sides.** Each G-P1 row compares our emitted matcher against an interpretive dense DFA built from the *same* minimized automaton. The competitor is `regex-automata`'s dense DFA from its published release, with its recommended flags. The row records the automaton's canonical hash on both sides, and the two hashes must be equal.
2. **Non-DFA Plan tiers get their own rows.** The lookaround product, the linear-lookaround tier and any backtracking fallback are reported in separate rows and are never pooled with DFA rows.
3. **Coverage is stated.** LW1 measured that 38 of the 115 distinct value.js leaves carry lookaround (`LOOKAROUND-CENSUS.tsv`). A DFA-only row therefore covers at most 77/115 of value.js's leaves, and every G-P1 table states its denominator.
4. **Benchmark honesty applies as written:** ≥ 10 reps, median plus dispersion, and no claimed win inside the noise.

**Falsifier (LW5.RX-1):** a G-P1 row whose two automaton hashes differ, or a pooled DFA/non-DFA row, fails the gate.

## C2-6 (critic 2, "Changing semantics breaks e…")

**Restated.** Moving the Rust engine to JS semantics breaks existing grammars and their consumers. JS semantics here means leftmost-first sticky alternation, Unicode simple case folding under `u`/`v`, and lookaround. Today's grammars parse one way on the Rust leaf, which is a leftmost-longest DFA with ASCII-only folding, and they would parse another way afterwards.

**Ruling: ACCEPTED IN PART.**

- *Accepted:* the change does alter parses, and every altered parse must be accounted for. The ADJ-2 migration ledger (LW3.RX-2) is the instrument. Every in-tree grammar and the value.js corpus are run under both semantics, and each changed parse is classified as either a corrected bug or a root grammar edit.
- *Rejected:* keeping the old semantics in any form, whether a compatibility flag, a per-grammar mode or a shim. Three reasons:
  1. The authority is JS (regex C1-3). The TypeScript backend already runs V8/JSC `RegExp`, so where the backends disagree today, the Rust backend is the one that is wrong.
  2. The disagreement is measured. On the oracle smoke corpus the current Rust leaf diverges on `a|ab` (2 bytes vs the oracle's 1), on every lookaround leaf (it rejects them: "invalid group syntax"), and on `/k/iu`, `/K/iu`, `/K/iu`, `/ſ/iu` and `/é/i(u)` (no match, where the oracle matches). The earlier census probe (`~/.dev-logs/bbnf-lockstep/regex-census/probe-run2.log`) found 5/11 divergences against the `regex` crate.
  3. Shims are banned by standing law.
- *Scope fact for the ledger:* the sk-v24 56-pattern corpus has 0 lookaround leaves, so its rows can change only through alternation order and folding. value.js has 38 lookaround leaves, and the Rust engine accepts none of them today, so those leaves cannot regress. They go from rejected to accepted.

**Falsifier (LW3.RX-2):** a changed parse with no ledger row, or a semantics switch anywhere in the engine API, fails the gate.

## Corrections this wave found in the record

- **"121 value.js leaves" is 115.** The 121-line list (`~/.dev-logs/bbnf-lockstep/regex-census/valuejs-regex-leaves.txt`, sha256 `5b390ee4…`) was extracted lexically.
  - It carries 11 artifacts from comments and prose (`**`, `/css/bbnf`, `www.w3.org`, …).
  - It misses 5 real leaves (`/@function /i`, `/@keyframes /i`, `/@property /i`, `/@scroll-timeline /i`, `/@view-timeline /i`).
  - Parsing the six live grammars with bbnf-lang's own `BBNFToASTWithImports` gives 115 distinct leaves, which occur 135 times, at value.js `8d7bb5b8` `src/css/grammar/`.
  - The lookaround count is 38 either way.
- **bun is not installed on this Mac.** The JSC side of the oracle runs the system `JavaScriptCore.framework` `jsc` shell (21624.1.16.11.4). That is the same engine without bun's runtime, and the receipt records which one ran.
