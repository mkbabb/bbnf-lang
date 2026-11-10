# OR-7 spike: Route A (regex-automata) against Route B (in-house)

sk-v27 LW2.RX-1 (seat S7). One row per leaf per route, each with its oracle verdict. The leaves are the 115 distinct value.js leaves plus the sk-v24 56 (`rx/LOOKAROUND-CENSUS.tsv`). The lookaround leaves are a tier of their own and are never pooled with the DFA tier (C1-9). Emitted-code speed is not measured here; it is queued as Phase 3 cells (PREREG F-REGEX). S14 turns this record into the OR-7 ruling ask.

## Method

- **Front end, shared.** Both routes parse with `parse_that_regex::parse` at parse-that `8678725` (LW2.RX-3's cure: `Hir::to_source` round-trips 167/167 accepted patterns). A leaf's `i` and `s` flags reach the parser as `(?i)` and `(?s)`. A failure both routes share is the front end's; a failure one route has alone is its determinizer's.
- **Route A** (`spike/or7-route-a` `5999f9c`): our `Hir` lowered node for node into `regex_syntax::hir::Hir` (byte classes stay byte classes, codepoint classes stay codepoint classes, `Group` is transparent), compiled by regex-automata's Thompson compiler (`utf8(false)`) into a dense DFA: `MatchKind::LeftmostFirst`, `StartKind::Anchored`, `minimize(true)`. A search is an anchored forward search at offset 0. The pin is regex-automata 0.4.14 and regex-syntax 0.8.10, the versions the parse-that workspace lock already resolves for CON-2's named comparator 'regex-automata 0.4' (competition I-6, G-R1). A dev-dependency on the spike branch only.
- **Route B** (`spike/or7-route-b` `2d01be6`): this crate's `Dfa::compile` (NFA, powerset DFA, Hopcroft minimization), searched with `find_at` (anchored, leftmost-longest).
- **Corpus.** Deterministic (`or7_route_b witness`, byte-identical across two writes, sha256 `47854f95…`): per leaf, a fixed bank of 50 CSS-shaped strings plus 8 samples drawn from the leaf's own `Hir` (lookaround stripped first, so a lookaround leaf still gets inputs near its language), each also extended by 8 suffixes, cut by one scalar and doubled; 21,722 cases over 171 leaves. Every case is a sticky match at offset 0 with the leaf's own flags.
- **Oracle.** LW1.RX-1's `oracle.mjs` over the corpus, 2 reps per engine: V8 self-consistent, JSC self-consistent, cross-agree 21,722/21,722, SPEC-AMBIGUITY 0 (answers sha256 `4e2faf2a…`). A route's answer is compared with the oracle's UTF-8 match end; a match ending inside a surrogate pair (non-`u`, astral input) has no UTF-8 image and is ENCODING-EXCLUDED (0 cases here).
- **Fidelity reps.** Each route ran twice; states and every match end are identical across the two runs on both routes.
- **Compile time** is the median of 7 in-process compiles per leaf (µs, parse included on both routes), release build on the pinned toolchain 1.99.1, `guarded --cpu-cores 0`. Every run started at 1-minute load < 8 (X-7 QUIET; `or7/records/loads.txt`: 6.61–7.64). The table reads run 1; run 2's medians were A 11.3 µs, B 3.5 µs (run 1: A 10.5, B 3.8).
- **States.** Route A counts the dense DFA's states reachable from the anchored start, dead state excluded; regex-automata delays a match by one byte, so its count carries match-delay states that Route B's `state_count` does not have. The two columns are each route's own measure, not one quantity.
- **Not measured here:** emitted-code speed (queued as Phase 3 cells, PREREG F-REGEX, per the order).

## Host and pins

- **machine:** Apple M5 Max, macOS 26.4.1
- **toolchain:** rustc 1.99.1 (the rust-toolchain.toml pin)
- **V8:** node v26.0.0, V8 14.6.202.33-node.19
- **JSC:** system JavaScriptCore.framework 21624.1.16.11.4 (bun absent)
- **regex-automata / regex-syntax:** 0.4.14 / 0.8.10 (crates.io, the parse-that lock's resolution)
- **parse-that:** lw2/s7-rx `8678725` (front end); spikes `spike/or7-route-a` `5999f9c`, `spike/or7-route-b` `2d01be6`
- **load:** 1-minute 6.61–7.64 at each run's start (QUIET under X-7); taken 2026-11-10 22:10 UTC
- **records:** `rx/or7/records/` (route results ×2 each, corpus and answers gzipped, oracle receipt, loads, SHA256)

## Summary by route and tier

| Route | Tier | Leaves | Compiled | AGREE | ENGINE | SPEC-AMBIGUITY | Cases compared | Case disagreements | Median compile µs (compiled) | Median states (compiled) |
|---|---|---|---|---|---|---|---|---|---|---|
| A | DFA | 133 | 133 | 121 | 12 | 0 | 17145 | 44 | 10.5 | 6 |
| A | LOOKAROUND | 38 | 0 | 0 | 38 | 0 | 0 | 0 | — | — |
| B | DFA | 133 | 133 | 114 | 19 | 0 | 17145 | 206 | 3.8 | 5 |
| B | LOOKAROUND | 38 | 0 | 0 | 38 | 0 | 0 | 0 | — | — |

Route names: **A** = A (regex-automata 0.4.14 dense DFA, leftmost-first); **B** = B (in-house Dfa, leftmost-longest).

## Findings

- **DFA tier, determinizer fidelity: Route A 0 route-only ENGINE leaves, Route B 7.** Every leaf where the routes part is a Route B defect the oracle refutes: `$` is not honoured (`!important\s*$`, `@starting-style$`: B matches where the oracle refuses) and a lazy quantifier is read as greedy (`\/\*[\s\S]*?\*\/` and the four comment-skipping leaves that embed it, #12, #14, #15 and #80: B runs to the last `*/`, the oracle stops at the first). Both are the leftmost-longest DFA's semantics, the same class RX-4 removed fusion for; neither is a front-end defect, since Route A reads the same `Hir` and agrees.
- **DFA tier, shared front-end ENGINE: 12 leaves, 44 cases, identical on both routes.** (1) `\s` omits U+00A0 (the corpus's only non-ASCII white space; the rest of ECMAScript's WhiteSpace and LineTerminator set is not probed here): 11 leaves, every case an NBSP input. (2) A non-`u` class range reaching U+FFFF (`[\u0080-\uffff]`, leaf #8) matches one UTF-16 code unit per step in the oracle, so an astral scalar is consumed as two units; the scalar front end refuses it. (3) A negated class atom in a non-`u` leaf is lowered as a byte class, so it consumes the first byte of a multi-byte scalar (leaf #8: `\\[^\n\r\f\da-fA-F]` before U+00A0 consumes 0xC2 alone). These belong to the front end whichever route is ruled (regex C1-2/C2-2, LW3.RX-1).
- **LOOKAROUND tier (38 leaves, never pooled): 38/38 ENGINE on both routes**, because the shared parser refuses every lookaround group. Route A cannot close this tier inside the determinizer at all: regex-automata has no lookaround. Route B can, by an in-house extension. Under either route the C1-9 lookaround charter (a G-R1 gate, LW1.RX-3) is owed; under A it is a check outside the DFA (the 38 are mostly one-character ident-boundary guards such as `(?![\w-])`).
- **Compile time (QUIET, median of 7 per leaf):** Route B is faster on the typical leaf (median 3.8 µs against 10.5 µs, about 2.8×) and slower on the largest (leaf #39: 1,084 µs against 570 µs; summed over the 133 compiled leaves, 2,318 µs against 2,603 µs). Every leaf compiles in under 1.1 ms on both routes, so compile time does not decide OR-7 for this corpus.
- **States:** median 6 (A) against 5 (B), sum 1,066 against 791, maximum 39 against 38. Route A's count includes regex-automata's match-delay states, so the gap is an upper bound on any real difference.
- **Reading for OR-7:** fidelity favours Route A in the DFA tier (0 against 7 determinizer ENGINE leaves); the 12 shared ENGINE leaves and the 38 lookaround leaves are owed by the front end and the lookaround charter on either route. Emitted-code speed (G-P1, G-R2) is not yet measured and is the remaining input (Phase 3, PREREG F-REGEX).

## Ruling ask: OR-7

**Question.** "For the regular fragment of a BBNF regex leaf (no lookaround, no backreference), which determinizer and minimizer does parse-that-regex use from LW3 on: Route A, regex-automata (0.4.14 at the lock, `MatchKind::LeftmostFirst`, anchored dense DFA, minimized) fed by our `Hir`; or Route B, the in-house `Dfa` (powerset + Hopcroft, leftmost-longest), repaired to leftmost-first, `$` and lazy quantifiers?"

**Recommended answer: Route A, PROVISIONAL**, closing when Phase 3's emitted-code cells (G-P1 / G-R2, PREREG F-REGEX, owed to S11 and not yet registered) show the A-backed product at least at parity with B's on the leaf corpus; if they show a LOSS beyond the floor, OR-7 re-opens with those records.

**Why.** (1) Fidelity is the gate's first term (G-C1: 0 ENGINE divergences), and on the determinizer alone A has 0 route-only ENGINE leaves against B's 7; B's 7 are not bugs to patch one at a time but its match semantics (leftmost-longest drops `$` and laziness, the class RX-4 already had to remove fusion for). Repairing B means writing a leftmost-first, look-aware determinizer, which is what A already is. (2) Compile time does not separate them on this corpus (every leaf under 1.1 ms on both; B faster at the median, A faster at the tail). (3) State counts are within one state at the median. (4) The BBNF law governs grammars, not automaton engines (regex design C1-4/C2-3), so adopting regex-automata is lawful; it stays an internal dependency of parse-that-regex, never part of its public `Hir` ABI.

**What the ruling does not settle, owed on either route.** The front end's 12 shared ENGINE leaves (`\s` without U+00A0; non-`u` UTF-16 code-unit classes; non-`u` negated classes lowered to byte classes; LW3.RX-1, C1-2/C2-2). The 38 lookaround leaves: the C1-9 charter is a G-R1 gate either way; under A it is a check outside the DFA (most of the 38 are one-character ident-boundary guards). RX-2's freeze: under A, only the `Hir` types are frozen (the order's "plus the state type if Route B is ruled" falls away).

**Consequence if A is ruled.** parse-that-regex takes regex-automata and regex-syntax as normal dependencies (exact pins, CON-2's row); R7 (Valmari–Lehtinen minimizer) becomes the doubling-fit check on regex-automata's minimizer (regex design R7); the in-house `Dfa` is retired from the match path once the emitter reads A's tables, its deletion a LW3 unit with its own falsifier.

## Where an ENGINE verdict comes from

| Tier | ENGINE on every route (front end or refused) | ENGINE on A only (determinizer) | ENGINE on B only (determinizer) |
|---|---|---|---|
| DFA | 12 | 0 | 7 |
| LOOKAROUND | 38 | 0 | 0 |

## Leaves where the routes' verdicts differ (7)

| Leaf | Tier | Pattern | Route A | Route B |
|---|---|---|---|---|
| value.js@8d7bb5b8#1 | DFA | `!important\s*$` | AGREE (0/138)  | ENGINE (64/138) first: case 50 oracle None route 11 |
| value.js@8d7bb5b8#12 | DFA | `(?:\s\|\/\*[\s\S]*?\*\/)*,(?:\s\|\/\*[\s\S]*?\*\/)*` | AGREE (0/137)  | ENGINE (6/137) first: case 59 oracle 25 route 34 |
| value.js@8d7bb5b8#14 | DFA | `(?:\s\|\/\*[\s\S]*?\*\/)*;(?:\s\|\/\*[\s\S]*?\*\/)*` | AGREE (0/138)  | ENGINE (5/138) first: case 59 oracle 11 route 22 |
| value.js@8d7bb5b8#15 | DFA | `(?:\s\|\/\*[\s\S]*?\*\/)*\)` | AGREE (0/104)  | ENGINE (4/104) first: case 59 oracle 7 route 14 |
| value.js@8d7bb5b8#51 | DFA | `@starting-style$` | AGREE (0/138)  | ENGINE (72/138) first: case 50 oracle None route 15 |
| value.js@8d7bb5b8#80 | DFA | `\((?:\s\|\/\*[\s\S]*?\*\/)*` | AGREE (0/121)  | ENGINE (3/121) first: case 70 oracle 10 route 20 |
| value.js@8d7bb5b8#83 | DFA | `\/\*[\s\S]*?\*\/` | AGREE (0/138)  | ENGINE (8/138) first: case 59 oracle 5 route 10 |

## Rows: DFA tier

| Leaf | Route | Flags | Compiled | States | Compile µs | Cases | Agree | Engine | Spec-amb. | Enc.-excl. | Verdict | Note |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| value.js@8d7bb5b8#1 | A | i | yes | 12 | 34.8 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#2 | A | — | yes | 7 | 16.5 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#8 | A | — | yes | 21 | 93.3 | 138 | 126 | 12 | 0 | 0 | ENGINE | first: case 36 oracle 4 route None |
| value.js@8d7bb5b8#10 | A | — | yes | 6 | 12.9 | 138 | 126 | 12 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#12 | A | — | yes | 32 | 65.1 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#13 | A | — | yes | 6 | 11.9 | 95 | 94 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route 0 |
| value.js@8d7bb5b8#14 | A | — | yes | 32 | 62.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#15 | A | — | yes | 13 | 22.5 | 104 | 104 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#17 | A | — | yes | 8 | 13.6 | 138 | 137 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| value.js@8d7bb5b8#19 | A | i | yes | 20 | 61.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#33 | A | i | yes | 9 | 22.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#37 | A | — | yes | 11 | 28.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#39 | A | i | yes | 39 | 569.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#40 | A | — | yes | 6 | 9.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#41 | A | — | yes | 6 | 9.9 | 115 | 115 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#42 | A | — | yes | 6 | 9.2 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#44 | A | — | yes | 4 | 6.5 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#45 | A | i | yes | 12 | 26.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#46 | A | i | yes | 13 | 30.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#47 | A | i | yes | 13 | 29.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#48 | A | i | yes | 12 | 24.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#49 | A | i | yes | 8 | 16.1 | 115 | 115 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#50 | A | i | yes | 19 | 46.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#51 | A | i | yes | 17 | 40.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#52 | A | i | yes | 17 | 39.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#53 | A | — | yes | 4 | 7.0 | 112 | 112 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#54 | A | — | yes | 3 | 5.0 | 70 | 70 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#56 | A | — | yes | 10 | 20.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#57 | A | — | yes | 10 | 23.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#58 | A | — | yes | 3 | 5.9 | 70 | 70 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#59 | A | — | yes | 3 | 7.1 | 95 | 94 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route 0 |
| value.js@8d7bb5b8#60 | A | — | yes | 4 | 6.4 | 137 | 136 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| value.js@8d7bb5b8#62 | A | — | yes | 4 | 5.0 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#63 | A | — | yes | 4 | 6.2 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#64 | A | — | yes | 4 | 6.5 | 132 | 132 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#65 | A | — | yes | 4 | 6.5 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#66 | A | — | yes | 4 | 8.2 | 131 | 131 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#67 | A | — | yes | 4 | 8.8 | 136 | 136 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#68 | A | — | yes | 4 | 7.5 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#69 | A | — | yes | 4 | 7.1 | 134 | 123 | 11 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#70 | A | — | yes | 4 | 7.4 | 133 | 132 | 1 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#71 | A | — | yes | 4 | 5.5 | 133 | 133 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#72 | A | — | yes | 4 | 5.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#73 | A | — | yes | 5 | 12.4 | 112 | 111 | 1 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#74 | A | — | yes | 4 | 7.0 | 121 | 121 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#75 | A | — | yes | 4 | 7.0 | 131 | 131 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#76 | A | — | yes | 4 | 5.5 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#78 | A | — | yes | 4 | 7.4 | 92 | 92 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#80 | A | — | yes | 7 | 14.6 | 121 | 121 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#81 | A | — | yes | 4 | 6.8 | 88 | 88 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#82 | A | — | yes | 5 | 7.0 | 83 | 83 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#83 | A | — | yes | 6 | 8.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#84 | A | — | yes | 10 | 67.7 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#85 | A | — | yes | 3 | 6.1 | 126 | 126 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#86 | A | — | yes | 3 | 5.6 | 96 | 95 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route 0 |
| value.js@8d7bb5b8#87 | A | — | yes | 4 | 7.9 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#88 | A | — | yes | 3 | 6.0 | 136 | 136 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#89 | A | — | yes | 4 | 5.9 | 131 | 130 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| value.js@8d7bb5b8#90 | A | — | yes | 6 | 9.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#91 | A | i | yes | 6 | 11.0 | 125 | 125 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#92 | A | i | yes | 8 | 15.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#93 | A | i | yes | 9 | 19.8 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#95 | A | i | yes | 13 | 32.5 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#96 | A | i | yes | 9 | 18.7 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#98 | A | i | yes | 16 | 38.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#99 | A | i | yes | 8 | 17.7 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#100 | A | i | yes | 7 | 14.4 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#103 | A | i | yes | 7 | 13.3 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#104 | A | i | yes | 7 | 14.1 | 126 | 126 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#105 | A | i | yes | 14 | 37.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#106 | A | i | yes | 10 | 24.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#108 | A | i | yes | 9 | 18.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#109 | A | i | yes | 9 | 19.3 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#110 | A | i | yes | 8 | 16.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#111 | A | i | yes | 9 | 17.9 | 116 | 116 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#112 | A | i | yes | 9 | 17.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#115 | A | i | yes | 7 | 13.1 | 105 | 105 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#1 | A | — | yes | 9 | 12.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#2 | A | — | yes | 7 | 12.1 | 132 | 132 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#3 | A | — | yes | 5 | 7.3 | 101 | 101 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#4 | A | — | yes | 15 | 28.8 | 105 | 105 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#5 | A | — | yes | 14 | 31.3 | 105 | 105 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#6 | A | — | yes | 6 | 8.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#7 | A | — | yes | 6 | 10.2 | 111 | 111 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#8 | A | — | yes | 9 | 14.9 | 125 | 125 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#9 | A | — | yes | 5 | 6.4 | 90 | 90 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#10 | A | — | yes | 9 | 13.8 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#11 | A | — | yes | 5 | 7.0 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#12 | A | — | yes | 5 | 7.2 | 112 | 112 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#13 | A | — | yes | 6 | 9.7 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#14 | A | — | yes | 5 | 9.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#15 | A | — | yes | 4 | 6.2 | 130 | 130 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#16 | A | — | yes | 9 | 15.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#17 | A | — | yes | 5 | 8.1 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#18 | A | — | yes | 4 | 6.5 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#19 | A | — | yes | 10 | 13.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#20 | A | — | yes | 10 | 8.6 | 116 | 116 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#21 | A | — | yes | 4 | 5.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#22 | A | — | yes | 10 | 11.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#23 | A | — | yes | 4 | 7.0 | 126 | 126 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#24 | A | — | yes | 5 | 10.7 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#25 | A | — | yes | 6 | 13.6 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#26 | A | — | yes | 5 | 7.9 | 114 | 114 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#27 | A | — | yes | 8 | 11.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#28 | A | — | yes | 10 | 20.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#29 | A | — | yes | 4 | 6.8 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#30 | A | — | yes | 4 | 7.7 | 133 | 133 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#31 | A | — | yes | 4 | 7.1 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#32 | A | — | yes | 4 | 6.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#33 | A | — | yes | 5 | 9.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#34 | A | — | yes | 4 | 5.1 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#35 | A | — | yes | 8 | 10.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#36 | A | — | yes | 6 | 11.6 | 115 | 115 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#37 | A | — | yes | 5 | 9.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#38 | A | — | yes | 6 | 8.3 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#39 | A | — | yes | 13 | 17.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#40 | A | — | yes | 8 | 9.5 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#41 | A | — | yes | 5 | 9.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#42 | A | — | yes | 8 | 17.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#43 | A | — | yes | 4 | 5.0 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#44 | A | — | yes | 6 | 7.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#45 | A | — | yes | 5 | 7.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#46 | A | — | yes | 17 | 18.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#47 | A | — | yes | 10 | 9.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#48 | A | — | yes | 12 | 10.8 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#49 | A | — | yes | 12 | 10.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#50 | A | — | yes | 4 | 6.8 | 132 | 131 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| sk-v24-56#51 | A | — | yes | 5 | 13.7 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#52 | A | — | yes | 4 | 6.9 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#53 | A | — | yes | 7 | 14.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#54 | A | — | yes | 5 | 7.5 | 124 | 124 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#55 | A | — | yes | 12 | 20.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#56 | A | — | yes | 8 | 9.8 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#1 | B | i | yes | 11 | 11.1 | 138 | 74 | 64 | 0 | 0 | ENGINE | first: case 50 oracle None route 11 |
| value.js@8d7bb5b8#2 | B | — | yes | 6 | 15.0 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#8 | B | — | yes | 14 | 166.8 | 138 | 126 | 12 | 0 | 0 | ENGINE | first: case 36 oracle 4 route None |
| value.js@8d7bb5b8#10 | B | — | yes | 3 | 5.1 | 138 | 126 | 12 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#12 | B | — | yes | 18 | 70.2 | 137 | 131 | 6 | 0 | 0 | ENGINE | first: case 59 oracle 25 route 34 |
| value.js@8d7bb5b8#13 | B | — | yes | 5 | 10.5 | 95 | 94 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route 0 |
| value.js@8d7bb5b8#14 | B | — | yes | 18 | 60.0 | 138 | 133 | 5 | 0 | 0 | ENGINE | first: case 59 oracle 11 route 22 |
| value.js@8d7bb5b8#15 | B | — | yes | 7 | 13.3 | 104 | 100 | 4 | 0 | 0 | ENGINE | first: case 59 oracle 7 route 14 |
| value.js@8d7bb5b8#17 | B | — | yes | 6 | 11.5 | 138 | 137 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| value.js@8d7bb5b8#19 | B | i | yes | 18 | 30.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#33 | B | i | yes | 8 | 5.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#37 | B | — | yes | 9 | 16.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#39 | B | i | yes | 38 | 1084.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#40 | B | — | yes | 4 | 3.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#41 | B | — | yes | 4 | 5.0 | 115 | 115 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#42 | B | — | yes | 4 | 4.6 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#44 | B | — | yes | 2 | 2.5 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#45 | B | i | yes | 11 | 11.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#46 | B | i | yes | 11 | 7.8 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#47 | B | i | yes | 12 | 7.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#48 | B | i | yes | 11 | 6.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#49 | B | i | yes | 7 | 3.9 | 115 | 115 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#50 | B | i | yes | 18 | 13.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#51 | B | i | yes | 16 | 11.5 | 138 | 66 | 72 | 0 | 0 | ENGINE | first: case 50 oracle None route 15 |
| value.js@8d7bb5b8#52 | B | i | yes | 16 | 12.7 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#53 | B | — | yes | 2 | 2.6 | 112 | 112 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#54 | B | — | yes | 2 | 1.5 | 70 | 70 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#56 | B | — | yes | 9 | 12.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#57 | B | — | yes | 7 | 12.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#58 | B | — | yes | 2 | 1.1 | 70 | 70 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#59 | B | — | yes | 1 | 1.5 | 95 | 94 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route 0 |
| value.js@8d7bb5b8#60 | B | — | yes | 2 | 1.4 | 137 | 136 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| value.js@8d7bb5b8#62 | B | — | yes | 2 | 1.8 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#63 | B | — | yes | 2 | 1.9 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#64 | B | — | yes | 2 | 1.9 | 132 | 132 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#65 | B | — | yes | 2 | 1.9 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#66 | B | — | yes | 2 | 2.0 | 131 | 131 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#67 | B | — | yes | 2 | 2.1 | 136 | 136 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#68 | B | — | yes | 2 | 2.3 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#69 | B | — | yes | 2 | 2.6 | 134 | 123 | 11 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#70 | B | — | yes | 2 | 2.1 | 133 | 132 | 1 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#71 | B | — | yes | 2 | 1.9 | 133 | 133 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#72 | B | — | yes | 2 | 1.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#73 | B | — | yes | 3 | 2.9 | 112 | 111 | 1 | 0 | 0 | ENGINE | first: case 37 oracle None route 2 |
| value.js@8d7bb5b8#74 | B | — | yes | 2 | 2.6 | 121 | 121 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#75 | B | — | yes | 2 | 2.3 | 131 | 131 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#76 | B | — | yes | 2 | 1.9 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#78 | B | — | yes | 3 | 1.8 | 92 | 92 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#80 | B | — | yes | 6 | 12.5 | 121 | 118 | 3 | 0 | 0 | ENGINE | first: case 70 oracle 10 route 20 |
| value.js@8d7bb5b8#81 | B | — | yes | 2 | 1.8 | 88 | 88 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#82 | B | — | yes | 3 | 3.5 | 83 | 83 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#83 | B | — | yes | 5 | 10.3 | 138 | 130 | 8 | 0 | 0 | ENGINE | first: case 59 oracle 5 route 10 |
| value.js@8d7bb5b8#84 | B | — | yes | 9 | 267.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#85 | B | — | yes | 2 | 2.5 | 126 | 126 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#86 | B | — | yes | 1 | 1.5 | 96 | 95 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route 0 |
| value.js@8d7bb5b8#87 | B | — | yes | 2 | 2.9 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#88 | B | — | yes | 2 | 1.9 | 136 | 136 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#89 | B | — | yes | 2 | 1.5 | 131 | 130 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| value.js@8d7bb5b8#90 | B | — | yes | 4 | 2.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#91 | B | i | yes | 5 | 2.5 | 125 | 125 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#92 | B | i | yes | 6 | 5.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#93 | B | i | yes | 7 | 5.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#95 | B | i | yes | 11 | 8.2 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#96 | B | i | yes | 7 | 4.8 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#98 | B | i | yes | 14 | 10.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#99 | B | i | yes | 6 | 4.3 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#100 | B | i | yes | 5 | 3.6 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#103 | B | i | yes | 5 | 3.3 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#104 | B | i | yes | 5 | 3.3 | 126 | 126 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#105 | B | i | yes | 12 | 9.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#106 | B | i | yes | 8 | 7.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#108 | B | i | yes | 7 | 5.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#109 | B | i | yes | 7 | 4.8 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#110 | B | i | yes | 6 | 4.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#111 | B | i | yes | 8 | 5.5 | 116 | 116 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#112 | B | i | yes | 7 | 6.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| value.js@8d7bb5b8#115 | B | i | yes | 6 | 3.1 | 105 | 105 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#1 | B | — | yes | 8 | 4.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#2 | B | — | yes | 5 | 4.2 | 132 | 132 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#3 | B | — | yes | 3 | 3.5 | 101 | 101 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#4 | B | — | yes | 14 | 23.5 | 105 | 105 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#5 | B | — | yes | 13 | 16.6 | 105 | 105 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#6 | B | — | yes | 3 | 2.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#7 | B | — | yes | 5 | 5.5 | 111 | 111 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#8 | B | — | yes | 6 | 6.4 | 125 | 125 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#9 | B | — | yes | 3 | 2.0 | 90 | 90 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#10 | B | — | yes | 6 | 6.8 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#11 | B | — | yes | 3 | 2.5 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#12 | B | — | yes | 3 | 3.1 | 112 | 112 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#13 | B | — | yes | 4 | 2.7 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#14 | B | — | yes | 4 | 3.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#15 | B | — | yes | 2 | 1.5 | 130 | 130 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#16 | B | — | yes | 5 | 5.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#17 | B | — | yes | 3 | 2.2 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#18 | B | — | yes | 2 | 1.4 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#19 | B | — | yes | 9 | 4.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#20 | B | — | yes | 9 | 5.3 | 116 | 116 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#21 | B | — | yes | 2 | 1.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#22 | B | — | yes | 9 | 4.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#23 | B | — | yes | 2 | 1.7 | 126 | 126 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#24 | B | — | yes | 2 | 3.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#25 | B | — | yes | 4 | 7.0 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#26 | B | — | yes | 2 | 3.0 | 114 | 114 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#27 | B | — | yes | 6 | 3.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#28 | B | — | yes | 6 | 6.8 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#29 | B | — | yes | 2 | 2.6 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#30 | B | — | yes | 2 | 2.8 | 133 | 133 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#31 | B | — | yes | 2 | 2.4 | 134 | 134 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#32 | B | — | yes | 2 | 2.5 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#33 | B | — | yes | 2 | 3.0 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#34 | B | — | yes | 2 | 1.4 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#35 | B | — | yes | 6 | 4.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#36 | B | — | yes | 5 | 3.2 | 115 | 115 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#37 | B | — | yes | 3 | 3.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#38 | B | — | yes | 3 | 3.4 | 127 | 127 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#39 | B | — | yes | 10 | 7.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#40 | B | — | yes | 6 | 3.6 | 135 | 135 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#41 | B | — | yes | 2 | 3.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#42 | B | — | yes | 7 | 8.2 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#43 | B | — | yes | 2 | 1.5 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#44 | B | — | yes | 4 | 2.6 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#45 | B | — | yes | 4 | 2.9 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#46 | B | — | yes | 16 | 10.1 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#47 | B | — | yes | 9 | 4.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#48 | B | — | yes | 11 | 5.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#49 | B | — | yes | 11 | 5.7 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#50 | B | — | yes | 2 | 1.4 | 132 | 131 | 1 | 0 | 0 | ENGINE | first: case 37 oracle 2 route None |
| sk-v24-56#51 | B | — | yes | 2 | 7.5 | 137 | 137 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#52 | B | — | yes | 2 | 1.5 | 122 | 122 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#53 | B | — | yes | 5 | 5.7 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#54 | B | — | yes | 3 | 3.8 | 124 | 124 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#55 | B | — | yes | 10 | 9.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |
| sk-v24-56#56 | B | — | yes | 7 | 4.4 | 138 | 138 | 0 | 0 | 0 | AGREE |  |

## Rows: LOOKAROUND tier

| Leaf | Route | Flags | Compiled | States | Compile µs | Cases | Agree | Engine | Spec-amb. | Enc.-excl. | Verdict | Note |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| value.js@8d7bb5b8#3 | A | — | no |  | 0.7 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 53: invalid group syntax |
| value.js@8d7bb5b8#4 | A | — | no |  | 0.1 | 56 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 2: invalid group syntax |
| value.js@8d7bb5b8#5 | A | i | no |  | 0.1 | 134 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 9: invalid group syntax |
| value.js@8d7bb5b8#6 | A | — | no |  | 0.1 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 5: invalid group syntax |
| value.js@8d7bb5b8#7 | A | — | no |  | 0.6 | 112 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 31: invalid group syntax |
| value.js@8d7bb5b8#9 | A | i | no |  | 0.8 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 29: invalid group syntax |
| value.js@8d7bb5b8#11 | A | — | no |  | 0.5 | 76 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 26: invalid group syntax |
| value.js@8d7bb5b8#16 | A | — | no |  | 0.5 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 28: invalid group syntax |
| value.js@8d7bb5b8#18 | A | — | no |  | 0.5 | 56 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 27: invalid group syntax |
| value.js@8d7bb5b8#20 | A | i | no |  | 2.6 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 116: invalid group syntax |
| value.js@8d7bb5b8#21 | A | i | no |  | 0.8 | 127 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 34: invalid group syntax |
| value.js@8d7bb5b8#22 | A | i | no |  | 0.3 | 115 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 17: invalid group syntax |
| value.js@8d7bb5b8#23 | A | i | no |  | 0.6 | 126 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 27: invalid group syntax |
| value.js@8d7bb5b8#24 | A | i | no |  | 1.4 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 59: invalid group syntax |
| value.js@8d7bb5b8#25 | A | i | no |  | 1.7 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 70: invalid group syntax |
| value.js@8d7bb5b8#26 | A | i | no |  | 0.3 | 125 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 17: invalid group syntax |
| value.js@8d7bb5b8#27 | A | i | no |  | 1.1 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 54: invalid group syntax |
| value.js@8d7bb5b8#28 | A | i | no |  | 1.1 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 53: invalid group syntax |
| value.js@8d7bb5b8#29 | A | i | no |  | 1.2 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 52: invalid group syntax |
| value.js@8d7bb5b8#30 | A | i | no |  | 0.4 | 134 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 18: invalid group syntax |
| value.js@8d7bb5b8#31 | A | i | no |  | 2.8 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 114: invalid group syntax |
| value.js@8d7bb5b8#32 | A | i | no |  | 2.6 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 104: invalid group syntax |
| value.js@8d7bb5b8#34 | A | i | no |  | 0.4 | 125 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 17: invalid group syntax |
| value.js@8d7bb5b8#35 | A | — | no |  | 0.1 | 56 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 2: invalid group syntax |
| value.js@8d7bb5b8#36 | A | — | no |  | 0.9 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 55: invalid group syntax |
| value.js@8d7bb5b8#38 | A | i | no |  | 1.2 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 62: invalid group syntax |
| value.js@8d7bb5b8#43 | A | — | no |  | 0.6 | 110 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 24: invalid group syntax |
| value.js@8d7bb5b8#55 | A | — | no |  | 0.5 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 22: invalid group syntax |
| value.js@8d7bb5b8#61 | A | — | no |  | 0.2 | 106 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 9: invalid group syntax |
| value.js@8d7bb5b8#77 | A | — | no |  | 0.3 | 135 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 16: invalid group syntax |
| value.js@8d7bb5b8#79 | A | — | no |  | 1.6 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 100: invalid group syntax |
| value.js@8d7bb5b8#94 | A | i | no |  | 0.4 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 15: invalid group syntax |
| value.js@8d7bb5b8#97 | A | i | no |  | 0.5 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 20: invalid group syntax |
| value.js@8d7bb5b8#101 | A | i | no |  | 0.2 | 92 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 8: invalid group syntax |
| value.js@8d7bb5b8#102 | A | i | no |  | 0.2 | 92 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 8: invalid group syntax |
| value.js@8d7bb5b8#107 | A | i | no |  | 0.2 | 116 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 10: invalid group syntax |
| value.js@8d7bb5b8#113 | A | i | no |  | 0.2 | 114 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 9: invalid group syntax |
| value.js@8d7bb5b8#114 | A | i | no |  | 0.2 | 94 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 11: invalid group syntax |
| value.js@8d7bb5b8#3 | B | — | no |  | 0.7 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 53: invalid group syntax |
| value.js@8d7bb5b8#4 | B | — | no |  | 0.1 | 56 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 2: invalid group syntax |
| value.js@8d7bb5b8#5 | B | i | no |  | 0.1 | 134 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 9: invalid group syntax |
| value.js@8d7bb5b8#6 | B | — | no |  | 0.1 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 5: invalid group syntax |
| value.js@8d7bb5b8#7 | B | — | no |  | 0.6 | 112 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 31: invalid group syntax |
| value.js@8d7bb5b8#9 | B | i | no |  | 0.8 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 29: invalid group syntax |
| value.js@8d7bb5b8#11 | B | — | no |  | 0.5 | 76 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 26: invalid group syntax |
| value.js@8d7bb5b8#16 | B | — | no |  | 0.7 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 28: invalid group syntax |
| value.js@8d7bb5b8#18 | B | — | no |  | 0.5 | 56 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 27: invalid group syntax |
| value.js@8d7bb5b8#20 | B | i | no |  | 2.9 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 116: invalid group syntax |
| value.js@8d7bb5b8#21 | B | i | no |  | 0.8 | 127 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 34: invalid group syntax |
| value.js@8d7bb5b8#22 | B | i | no |  | 0.4 | 115 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 17: invalid group syntax |
| value.js@8d7bb5b8#23 | B | i | no |  | 0.6 | 126 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 27: invalid group syntax |
| value.js@8d7bb5b8#24 | B | i | no |  | 1.5 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 59: invalid group syntax |
| value.js@8d7bb5b8#25 | B | i | no |  | 1.8 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 70: invalid group syntax |
| value.js@8d7bb5b8#26 | B | i | no |  | 0.4 | 125 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 17: invalid group syntax |
| value.js@8d7bb5b8#27 | B | i | no |  | 1.2 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 54: invalid group syntax |
| value.js@8d7bb5b8#28 | B | i | no |  | 1.2 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 53: invalid group syntax |
| value.js@8d7bb5b8#29 | B | i | no |  | 1.3 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 52: invalid group syntax |
| value.js@8d7bb5b8#30 | B | i | no |  | 0.4 | 134 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 18: invalid group syntax |
| value.js@8d7bb5b8#31 | B | i | no |  | 3.1 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 114: invalid group syntax |
| value.js@8d7bb5b8#32 | B | i | no |  | 2.8 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 104: invalid group syntax |
| value.js@8d7bb5b8#34 | B | i | no |  | 0.4 | 125 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 17: invalid group syntax |
| value.js@8d7bb5b8#35 | B | — | no |  | 0.1 | 56 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 2: invalid group syntax |
| value.js@8d7bb5b8#36 | B | — | no |  | 1.0 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 55: invalid group syntax |
| value.js@8d7bb5b8#38 | B | i | no |  | 1.2 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 62: invalid group syntax |
| value.js@8d7bb5b8#43 | B | — | no |  | 0.6 | 110 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 24: invalid group syntax |
| value.js@8d7bb5b8#55 | B | — | no |  | 0.6 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 22: invalid group syntax |
| value.js@8d7bb5b8#61 | B | — | no |  | 0.2 | 106 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 9: invalid group syntax |
| value.js@8d7bb5b8#77 | B | — | no |  | 0.3 | 135 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 16: invalid group syntax |
| value.js@8d7bb5b8#79 | B | — | no |  | 1.5 | 137 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 100: invalid group syntax |
| value.js@8d7bb5b8#94 | B | i | no |  | 0.5 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 15: invalid group syntax |
| value.js@8d7bb5b8#97 | B | i | no |  | 0.5 | 138 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 20: invalid group syntax |
| value.js@8d7bb5b8#101 | B | i | no |  | 0.2 | 92 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 8: invalid group syntax |
| value.js@8d7bb5b8#102 | B | i | no |  | 0.2 | 92 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 8: invalid group syntax |
| value.js@8d7bb5b8#107 | B | i | no |  | 0.2 | 116 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 10: invalid group syntax |
| value.js@8d7bb5b8#113 | B | i | no |  | 0.2 | 114 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 9: invalid group syntax |
| value.js@8d7bb5b8#114 | B | i | no |  | 0.2 | 94 | 0 | 0 | 0 | 0 | ENGINE | rejects: regex parse error at byte 11: invalid group syntax |

## Patterns

| Leaf | Tier | Flags | Pattern |
|---|---|---|---|
| value.js@8d7bb5b8#1 | DFA | i | `!important\s*$` |
| value.js@8d7bb5b8#2 | DFA | — | `"(?:\\[\s\S]\|[^"\\])*"\|'(?:\\[\s\S]\|[^'\\])*'` |
| value.js@8d7bb5b8#3 | LOOKAROUND | — | `#(?:[0-9a-fA-F]{8}\|[0-9a-fA-F]{6}\|[0-9a-fA-F]{3,4})(?![\w-])` |
| value.js@8d7bb5b8#4 | LOOKAROUND | — | `(?!\{)` |
| value.js@8d7bb5b8#5 | LOOKAROUND | i | `(?:(?![!]important\s*$)[\s\S])+` |
| value.js@8d7bb5b8#6 | LOOKAROUND | — | `(?:(?!['"]$)[\s\S])+` |
| value.js@8d7bb5b8#7 | LOOKAROUND | — | `(?:--[\w-]*\|[a-zA-Z_-][\w-]*)(?=\()` |
| value.js@8d7bb5b8#8 | DFA | — | `(?:--\|-?(?:[_a-zA-Z\u0080-\uffff]\|\\(?:[\da-fA-F]{1,6}[ \t\n\r\f]?\|[^\n\r\f\da-fA-F])))(?:[-_a-zA-Z\d\u0080-\uffff]\|\\(?:[\da-fA-F]{1,6}[ \t\n\r\f]?\|[^\n\r\f\da-fA-F]))*` |
| value.js@8d7bb5b8#9 | LOOKAROUND | i | `(?:-?infinity\|nan\|pi\|e)(?![\w-])\|sibling-(?:index\|count)\(\s*\)` |
| value.js@8d7bb5b8#10 | DFA | — | `(?:[^\s"'()\\\x00-\x08\x0b\x0e-\x1f\x7f]\|\\[^\n\r\f])+` |
| value.js@8d7bb5b8#11 | LOOKAROUND | — | `(?:\s\|\/\*[\s\S]*?\*\/)*(?=\*)\|(?<=\*)` |
| value.js@8d7bb5b8#12 | DFA | — | `(?:\s\|\/\*[\s\S]*?\*\/)*,(?:\s\|\/\*[\s\S]*?\*\/)*` |
| value.js@8d7bb5b8#13 | DFA | — | `(?:\s\|\/\*[\s\S]*?\*\/)*` |
| value.js@8d7bb5b8#14 | DFA | — | `(?:\s\|\/\*[\s\S]*?\*\/)*;(?:\s\|\/\*[\s\S]*?\*\/)*` |
| value.js@8d7bb5b8#15 | DFA | — | `(?:\s\|\/\*[\s\S]*?\*\/)*\)` |
| value.js@8d7bb5b8#16 | LOOKAROUND | — | `(?:\s\|\/\*[\s\S]*?\*\/)*\/(?!\*)(?:\s\|\/\*[\s\S]*?\*\/)*` |
| value.js@8d7bb5b8#17 | DFA | — | `(?:\s\|\/\*[\s\S]*?\*\/)+` |
| value.js@8d7bb5b8#18 | LOOKAROUND | — | `(?:\s\|\/\*[\s\S]*?\*\/)+\|(?=[:;])\|(?<=[:;])\|(?<=\))(?=[a-zA-Z_-][\w-]*\()\|(?<=\])(?=[^\s,\/()\]:;])\|(?<=[^\s,\/(\[:;])(?=\[)` |
| value.js@8d7bb5b8#19 | DFA | i | `(?:auto\|none\|--\|scroll\(\|view\()[\s\S]*` |
| value.js@8d7bb5b8#20 | LOOKAROUND | i | `(?:calc\|min\|max\|clamp\|round\|mod\|rem\|sin\|cos\|tan\|asin\|acos\|atan2\|atan\|pow\|sqrt\|hypot\|log\|exp\|abs\|sign\|progress)(?=\()` |
| value.js@8d7bb5b8#21 | LOOKAROUND | i | `(?:entry\|exit\|cover\|contain)(?![\w-])` |
| value.js@8d7bb5b8#22 | LOOKAROUND | i | `(?:from\|to)(?![\w-])` |
| value.js@8d7bb5b8#23 | LOOKAROUND | i | `(?:hsl\|hwb\|lch\|oklch)(?![\w-])` |
| value.js@8d7bb5b8#24 | LOOKAROUND | i | `(?:jump-start\|jump-end\|jump-none\|jump-both\|start\|end)(?![\w-])` |
| value.js@8d7bb5b8#25 | LOOKAROUND | i | `(?:linear\|ease-in-out\|ease-in\|ease-out\|ease\|step-start\|step-end)(?![\w(-])` |
| value.js@8d7bb5b8#26 | LOOKAROUND | i | `(?:min\|max)(?=\()` |
| value.js@8d7bb5b8#27 | LOOKAROUND | i | `(?:rgba?\|hsla?\|hwb\|lab\|lch\|oklab\|oklch)\(\s*from(?![\w-])` |
| value.js@8d7bb5b8#28 | LOOKAROUND | i | `(?:rgba?\|hsla?\|hwb\|lab\|lch\|oklab\|oklch\|color)\((?!\s*from(?![\w-]))` |
| value.js@8d7bb5b8#29 | LOOKAROUND | i | `(?:shorter\|longer\|increasing\|decreasing)\s+hue(?![\w-])` |
| value.js@8d7bb5b8#30 | LOOKAROUND | i | `(?:sign\|abs)(?=\()` |
| value.js@8d7bb5b8#31 | LOOKAROUND | i | `(?:srgb-linear\|srgb\|display-p3-linear\|display-p3\|a98-rgb\|prophoto-rgb\|rec2020\|lab\|oklab\|xyz-d50\|xyz-d65\|xyz)(?![\w-])` |
| value.js@8d7bb5b8#32 | LOOKAROUND | i | `(?:srgb-linear\|srgb\|display-p3-linear\|display-p3\|a98-rgb\|prophoto-rgb\|rec2020\|xyz-d50\|xyz-d65\|xyz)(?![\w-])` |
| value.js@8d7bb5b8#33 | DFA | i | `(?:url\|type)\(` |
| value.js@8d7bb5b8#34 | LOOKAROUND | i | `(?:var\|env)(?=\()` |
| value.js@8d7bb5b8#35 | LOOKAROUND | — | `(?=(?:[^;"']\|"(?:\\[\s\S]\|[^"\\])*"\|'(?:\\[\s\S]\|[^'\\])*')*;)` |
| value.js@8d7bb5b8#36 | LOOKAROUND | — | `([+-]?(?:\d*\.\d+\|\d+)(?:[eE][+-]?\d+)?)([%a-zA-Z-]*)(?![\w\\.])` |
| value.js@8d7bb5b8#37 | DFA | — | `([+-]?(?:\d*\.\d+\|\d+)(?:[eE][+-]?\d+)?)([a-zA-Z_][\w-]*)` |
| value.js@8d7bb5b8#38 | LOOKAROUND | i | `([+-]?(?:\d*\.\d+\|\d+)(?:e[+-]?\d+)?)(deg\|grad\|rad\|turn)(?![\w-])` |
| value.js@8d7bb5b8#39 | DFA | i | `(type\|element)\(\s*(#(?:[-_a-zA-Z\u0080-￿]\|\\(?:[\da-fA-F]{1,6}[ \t\n\r\f]?\|[^\n\r\f\da-fA-F]))(?:[-_a-zA-Z\d\u0080-￿]\|\\(?:[\da-fA-F]{1,6}[ \t\n\r\f]?\|[^\n\r\f\da-fA-F]))*\|"(?:\\[\s\S]\|[^"\\])*"\|'(?:\\[\s\S]\|[^'\\])*'\|\*\|(?:<transform-list>\|(?:<[a-zA-Z][\w-]*>\|-?[a-zA-Z_][\w-]*)[#+]?)(?:\s*\\|\s*(?:<transform-list>\|(?:<[a-zA-Z][\w-]*>\|-?[a-zA-Z_][\w-]*)[#+]?))*)\s*\)` |
| value.js@8d7bb5b8#40 | DFA | — | `--[-\w]+` |
| value.js@8d7bb5b8#41 | DFA | — | `--[-_a-zA-Z][-_a-zA-Z\d]*` |
| value.js@8d7bb5b8#42 | DFA | — | `--[\w-]+` |
| value.js@8d7bb5b8#43 | LOOKAROUND | — | `<=\|>=\|==\|!=\|[+*<>=:]\|-(?![\w\u0080-\uffff\\-])` |
| value.js@8d7bb5b8#44 | DFA | — | `@[^ ]*` |
| value.js@8d7bb5b8#45 | DFA | i | `@function ` |
| value.js@8d7bb5b8#46 | DFA | i | `@function\s+` |
| value.js@8d7bb5b8#47 | DFA | i | `@keyframes ` |
| value.js@8d7bb5b8#48 | DFA | i | `@property ` |
| value.js@8d7bb5b8#49 | DFA | i | `@scope` |
| value.js@8d7bb5b8#50 | DFA | i | `@scroll-timeline ` |
| value.js@8d7bb5b8#51 | DFA | i | `@starting-style$` |
| value.js@8d7bb5b8#52 | DFA | i | `@view-timeline ` |
| value.js@8d7bb5b8#53 | DFA | — | `["'][\s\S]*` |
| value.js@8d7bb5b8#54 | DFA | — | `['"]` |
| value.js@8d7bb5b8#55 | LOOKAROUND | — | `[+-]?(?:\d*\.\d+\|\d+(?!\.\d))(?:[eE][+-]?\d+)?(?![\w%\\])` |
| value.js@8d7bb5b8#56 | DFA | — | `[+-]?(?:\d*\.\d+\|\d+)(?:[eE][+-]?\d+)?%` |
| value.js@8d7bb5b8#57 | DFA | — | `[+-]?(?:\d+\.?\d*\|\.\d+)(?:%\|[a-zA-Z]+)?` |
| value.js@8d7bb5b8#58 | DFA | — | `[[{]` |
| value.js@8d7bb5b8#59 | DFA | — | `[\s,]*` |
| value.js@8d7bb5b8#60 | DFA | — | `[\s;]+` |
| value.js@8d7bb5b8#61 | LOOKAROUND | — | `[\s\S]*(?=\)$)` |
| value.js@8d7bb5b8#62 | DFA | — | `[\s\S]+` |
| value.js@8d7bb5b8#63 | DFA | — | `[^()"']+` |
| value.js@8d7bb5b8#64 | DFA | — | `[^(),"']+` |
| value.js@8d7bb5b8#65 | DFA | — | `[^():"']+` |
| value.js@8d7bb5b8#66 | DFA | — | `[^()[\]{}"']+` |
| value.js@8d7bb5b8#67 | DFA | — | `[^()[\]{};"']+` |
| value.js@8d7bb5b8#68 | DFA | — | `[^()[{;"']+` |
| value.js@8d7bb5b8#69 | DFA | — | `[^()\s"']+` |
| value.js@8d7bb5b8#70 | DFA | — | `[^()\s,"']+` |
| value.js@8d7bb5b8#71 | DFA | — | `[^:]+` |
| value.js@8d7bb5b8#72 | DFA | — | `[^\n]+` |
| value.js@8d7bb5b8#73 | DFA | — | `[^\s(),\/:;"']+\|;` |
| value.js@8d7bb5b8#74 | DFA | — | `[^{;()"']+` |
| value.js@8d7bb5b8#75 | DFA | — | `[^{}"']+` |
| value.js@8d7bb5b8#76 | DFA | — | `[^\|]+` |
| value.js@8d7bb5b8#77 | LOOKAROUND | — | `[a-zA-Z][\w-]*(?![\w(-])` |
| value.js@8d7bb5b8#78 | DFA | — | `[tT][oO]` |
| value.js@8d7bb5b8#79 | LOOKAROUND | — | `[uU](?:\/\*[\s\S]*?\*\/)*\+(?:[\da-fA-F?]\|\/\*[\s\S]*?\*\/)+(?:-(?:[\da-fA-F]\|\/\*[\s\S]*?\*\/)+)?(?![\w?\\-])\|(?:[-_a-zA-Z\u0080-\uffff]\|\\(?:[\da-fA-F]{1,6}[ \t\n\r\f]?\|[^\n\r\f\da-fA-F]))(?:[-_a-zA-Z\d\u0080-\uffff]\|\\(?:[\da-fA-F]{1,6}[ \t\n\r\f]?\|[^\n\r\f\da-fA-F]))*(?![\w(\\-])` |
| value.js@8d7bb5b8#80 | DFA | — | `\((?:\s\|\/\*[\s\S]*?\*\/)*` |
| value.js@8d7bb5b8#81 | DFA | — | `\(\s*` |
| value.js@8d7bb5b8#82 | DFA | — | `\/\*[\s\S]*` |
| value.js@8d7bb5b8#83 | DFA | — | `\/\*[\s\S]*?\*\/` |
| value.js@8d7bb5b8#84 | DFA | — | `\[\s*(?:(?:-?[a-zA-Z_\u0080-\uffff]\|--)[\w\u0080-\uffff-]*(?:\s+(?:-?[a-zA-Z_\u0080-\uffff]\|--)[\w\u0080-\uffff-]*)*)?\s*\]` |
| value.js@8d7bb5b8#85 | DFA | — | `\s*,` |
| value.js@8d7bb5b8#86 | DFA | — | `\s*` |
| value.js@8d7bb5b8#87 | DFA | — | `\s*[*\/]\s*` |
| value.js@8d7bb5b8#88 | DFA | — | `\s*\(` |
| value.js@8d7bb5b8#89 | DFA | — | `\s+` |
| value.js@8d7bb5b8#90 | DFA | — | `\s+[+-]\s+` |
| value.js@8d7bb5b8#91 | DFA | i | `auto` |
| value.js@8d7bb5b8#92 | DFA | i | `calc\(\s*` |
| value.js@8d7bb5b8#93 | DFA | i | `clamp\(\s*` |
| value.js@8d7bb5b8#94 | LOOKAROUND | i | `color-mix(?=\()` |
| value.js@8d7bb5b8#95 | DFA | i | `color-mix\(\s*` |
| value.js@8d7bb5b8#96 | DFA | i | `color\(\s*` |
| value.js@8d7bb5b8#97 | LOOKAROUND | i | `color\(\s*from(?![\w-])` |
| value.js@8d7bb5b8#98 | DFA | i | `cubic-bezier\(\s*` |
| value.js@8d7bb5b8#99 | DFA | i | `hsla?\(\s*` |
| value.js@8d7bb5b8#100 | DFA | i | `hwb\(\s*` |
| value.js@8d7bb5b8#101 | LOOKAROUND | i | `if(?=\()` |
| value.js@8d7bb5b8#102 | LOOKAROUND | i | `in(?![\w-])` |
| value.js@8d7bb5b8#103 | DFA | i | `lab\(\s*` |
| value.js@8d7bb5b8#104 | DFA | i | `lch\(\s*` |
| value.js@8d7bb5b8#105 | DFA | i | `light-dark\(\s*` |
| value.js@8d7bb5b8#106 | DFA | i | `linear\(\s*` |
| value.js@8d7bb5b8#107 | LOOKAROUND | i | `none(?![\w-])` |
| value.js@8d7bb5b8#108 | DFA | i | `oklab\(\s*` |
| value.js@8d7bb5b8#109 | DFA | i | `oklch\(\s*` |
| value.js@8d7bb5b8#110 | DFA | i | `rgba?\(\s*` |
| value.js@8d7bb5b8#111 | DFA | i | `scroll\(` |
| value.js@8d7bb5b8#112 | DFA | i | `steps\(\s*` |
| value.js@8d7bb5b8#113 | LOOKAROUND | i | `url(?=\()` |
| value.js@8d7bb5b8#114 | LOOKAROUND | i | `url\((?!\s*["'])` |
| value.js@8d7bb5b8#115 | DFA | i | `view\(` |
| sk-v24-56#1 | DFA | — | `#[0-9a-fA-F]{3,6}` |
| sk-v24-56#2 | DFA | — | `%[0-9a-fA-F][0-9a-fA-F]\|[^%]+` |
| sk-v24-56#3 | DFA | — | `(?:0\|[1-9][0-9]*)` |
| sk-v24-56#4 | DFA | — | `(?:GET\|POST\|PUT\|DELETE\|HEAD)` |
| sk-v24-56#5 | DFA | — | `(?:Mon\|Tue\|Wed\|Thu\|Fri\|Sat\|Sun)` |
| sk-v24-56#6 | DFA | — | `(?:[A-Z][a-z]+)+` |
| sk-v24-56#7 | DFA | — | `(?:foo\|bar)*` |
| sk-v24-56#8 | DFA | — | `(?:foo\|bar\|baz)+` |
| sk-v24-56#9 | DFA | — | `(ab)+` |
| sk-v24-56#10 | DFA | — | `-?(0\|[1-9]\d*)(\.\d+)?` |
| sk-v24-56#11 | DFA | — | `-?\d+` |
| sk-v24-56#12 | DFA | — | `//[^\n]*` |
| sk-v24-56#13 | DFA | — | `0x[0-9a-fA-F]+` |
| sk-v24-56#14 | DFA | — | `<[a-zA-Z][a-zA-Z0-9]*>` |
| sk-v24-56#15 | DFA | — | `[ \t]+` |
| sk-v24-56#16 | DFA | — | `[-+]?(\d+(\.\d+)?\|\.\d+)` |
| sk-v24-56#17 | DFA | — | `[-+]?\d+` |
| sk-v24-56#18 | DFA | — | `[0-9a-fA-F]+` |
| sk-v24-56#19 | DFA | — | `[0-9a-fA-F]{3,8}` |
| sk-v24-56#20 | DFA | — | `[01]{8}` |
| sk-v24-56#21 | DFA | — | `[A-Z]+` |
| sk-v24-56#22 | DFA | — | `[A-Z]{3}-\d{4}` |
| sk-v24-56#23 | DFA | — | `[A-Za-z0-9+/]+` |
| sk-v24-56#24 | DFA | — | `[A-Za-z]+(?:'[A-Za-z]+)*` |
| sk-v24-56#25 | DFA | — | `[\-]?[a-zA-Z_][\w-]*\|--[\w-]+` |
| sk-v24-56#26 | DFA | — | `[^\x00]*x` |
| sk-v24-56#27 | DFA | — | `[a-fA-F0-9]{2}(?::[a-fA-F0-9]{2})+` |
| sk-v24-56#28 | DFA | — | `[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]+` |
| sk-v24-56#29 | DFA | — | `[a-zA-Z][a-zA-Z0-9]*` |
| sk-v24-56#30 | DFA | — | `[a-zA-Z_][\w-]*` |
| sk-v24-56#31 | DFA | — | `[a-zA-Z_]\w*` |
| sk-v24-56#32 | DFA | — | `[a-z]*[0-9]+` |
| sk-v24-56#33 | DFA | — | `[a-z]+(-[a-z]+)*` |
| sk-v24-56#34 | DFA | — | `[a-z]+` |
| sk-v24-56#35 | DFA | — | `[a-z]+@[a-z]+\.[a-z]+` |
| sk-v24-56#36 | DFA | — | `[tT][rR][uU][eE]` |
| sk-v24-56#37 | DFA | — | `\$[a-zA-Z_][a-zA-Z0-9_]*` |
| sk-v24-56#38 | DFA | — | `\*.*\*` |
| sk-v24-56#39 | DFA | — | `\+?\d[\d -]{6,}\d` |
| sk-v24-56#40 | DFA | — | `\d+(,\d{3})*` |
| sk-v24-56#41 | DFA | — | `\d+(?:_\d+)*` |
| sk-v24-56#42 | DFA | — | `\d+(?:px\|em\|rem\|vh\|vw)` |
| sk-v24-56#43 | DFA | — | `\d+` |
| sk-v24-56#44 | DFA | — | `\d+\.\d+` |
| sk-v24-56#45 | DFA | — | `\d+px` |
| sk-v24-56#46 | DFA | — | `\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}` |
| sk-v24-56#47 | DFA | — | `\d{2}:\d{2}:\d{2}` |
| sk-v24-56#48 | DFA | — | `\d{4}-\d{2}-\d{2}` |
| sk-v24-56#49 | DFA | — | `\d{5}(?:-\d{4})?` |
| sk-v24-56#50 | DFA | — | `\s+` |
| sk-v24-56#51 | DFA | — | `\w+(?:[-_]\w+)*` |
| sk-v24-56#52 | DFA | — | `\w+` |
| sk-v24-56#53 | DFA | — | `\w+\s*=\s*\w+` |
| sk-v24-56#54 | DFA | — | `a*b*c*` |
| sk-v24-56#55 | DFA | — | `https?://[a-zA-Z0-9./-]+` |
| sk-v24-56#56 | DFA | — | `v\d\.\d\.\d` |
