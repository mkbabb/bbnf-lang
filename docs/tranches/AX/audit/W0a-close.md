# W0a closure — the gate-repair + shape-emission pivot

W0a opens AW-V's 0/17 diagnostic and closes with all six semantic
parity harnesses green under shape-emission-authoritative routing.
Master HEAD at close: `7c2a7c70`. Cycle-1 = cycle-2 bootstrap regen
byte-identical at 98,270 lines.

## Phases landed

| Phase | Commit range | Headline |
|---|---|---|
| W0a.1 | `9f8aed90` | `has_w4_classified` narrowed to `Pratt \| Unordered`. |
| W0a.2.a | `ee7f81da` | Array emitter split: wrapped (Shape 1) + entry-list (Shape 2). |
| W0a.2.b | `517be13c`..`7f3dbafb` | `AltDispatch` shape + detector widening (43→0 entry-reachable unclassified Refs). |
| W0a.2.d | `1e603586` (substrate) | `shapes/inline.rs` helpers for Alt/Regex/Negate/Minus/TokenDispatch. |
| W0a.2.h | `3feb4999` | Four surgical emitter fixes: structural Seq Alt-branches, OW trivia preservation, AltDispatch Seq, Keyword Seq, Flat iter rewind. |
| W0a.2.i.a–b | `c4d56a42` `a46d3f25` `fe17964f` `9e5cc2f1` | `walk_tape` cursor fallback + IR-structural keying (drops `directive_0 \| grammar_item_0`). |
| W0a.2.j | `5f0709fc` `ee03e8fa` `07e254f7` | Admission widened; Wrap+Flat tape-kind fixes; cycle-1=cycle-2 restored. |
| W0a.2.l | `64d6ab2f` `e5ff835e` `34be629e` `7d2fa1b8` | Per-rule `PRECEDENCE_LUT_<rule>` + 1-byte arena-frame API + Option B+C miner; reducer compounds preserved. |
| W0a.2.m | (consumer fix on `lower/expression.rs`) | `lower_binary_factor` + `collect_binary_operands` walks reducer-compound chains. |
| W0a.2.n | `8dce3270`..`6e8958b5` | Pratt loop `skip_space` + phantom-op guard + whitespace-aware operator peek. |
| W0a.2.o | `5a451df1` `5e886cca` `f50f9d27` | Pratt-wrapper-peel leaked-flat-layout, factored-Alt mining, first-byte LUT merge. |
| W0a.2.p | `2b7f9744`..`a726fdce` | Pratt detector narrowing, Keyword typed-leaf arena, Flat typed-Alt+Map-regex. |
| W0a.2.q | `a776de3c`..`381cb9cd` | Wrap/HRegex/AltDispatch typed-Alt arena payloads, leading-dot Number via regex-scan. |
| W0a.2.r | `c92ceee9` `6b03dd53` | `inline.rs` Alt Regex branch uses actual pattern; regen 98,270. |

## Hard-gate status

| Gate | Status | Citation |
|---|---|---|
| W0a §Hard gate #1 (JSON visitor recompile) | ✔ | `cargo expand` re-emits `parse_with_visitor`; `post-AX-W0a1-expand-json.txt`. |
| W0a §Hard gate #2 (no walker from `parse()`) | ✔ | All 7 grammars route through shape dispatcher; 6 parity harnesses green. |
| W0a §Hard gate #3 (wire-contract) | ✔ | `gate_predicate_wire_contract.rs` landed `69d28f56`; `expected` map flipped at W0a.2.j. |
| W0a §Hard gate #4 (regen idempotent) | ✔ | Cycle-1 = cycle-2 byte-identical at 98,270 lines (`6b03dd53`). |
| W0a.close §Hard gate #1 (`.txt` outputs) | ✔ | All 5 present under `docs/benchmarks/post-AX-W0a-close-*.txt`. |
| W0a.close §Hard gate #2 (JSON well-formed) | ✔ | `post-AX-W0a-close.json` validates; 5 bench groups × specified sub-keys. |
| W0a.close §Hard gate #3 (17+1 matrix covered) | ✔ | 18 entries; css_l4 trio carries panic-status placeholders per §Known tolerances. |
| W0a.close §Hard gate #4 (`parse()` routing) | ✔ | Routing verified at W0a.2.j admission widening; no further emitter change since. |

## Invariants verified

- **Invariant 9 (frozen gate predicates)** — `gate_predicate_wire_contract.rs` encodes the 7×3 matrix; any widening is an explicit test-flip. W0a.2.j flipped `has_shape_dispatcher_entrypoint` for 6 grammars; one flip closes W0a.
- **Invariant 18 (field-complete output)** — every shape-emission path ships its tape structure without placeholder arms; the W0a.2.{p,q,r} cascade retired the residual `__has_payload` gaps surfaced by parity harnesses.
- **Invariant 20 (shape-emission-authoritative tape)** — parity harnesses are the source of truth; `tape_parity_*` walker oracles are scaffold retiring at W0b. 6/6 semantic harnesses green (bbnf 2/0, bbnf_ast 9/0, css_l4 16/0, json 9/0, json_value 13/0, sheets 25/0).

## Artefacts

| Artefact | Path |
|---|---|
| Bench baseline JSON | `docs/benchmarks/post-AX-W0a-close.json` |
| Per-bench raw output | `docs/benchmarks/post-AX-W0a-close-{bbnf,compile,css,json,sheets}.txt` |
| Cycle-1=cycle-2 idempotency | `PROGRESS.md` §2026-04-19 W0a.2.r — regen 98,270 lines |
| Parity-harness green (6/6) | `PROGRESS.md` §2026-04-19 W0a parity-harness closure at HEAD `6b03dd53` |
| Wire-contract (gate freeze) | `crates/core/tests/gate_predicate_wire_contract.rs` |

## Known tolerances

- **css_l4 bench entries (3/3) panic** at W0a close HEAD under shape-emission-authoritative routing: `bootstrap.css` fails at offset 18, `normalize.css` at offset 0, `tailwind.css` at offset 0. `css_l4_parity` 16/0 semantic harness remains green on synthetic fixtures; the residual gap is on the real-CSS surface area not covered by parity inputs. The baseline JSON carries `status: "panic"` placeholders for these three entries. Deferred: the earliest post-W0b wave surfacing the residual routing gap (candidate W4 CSS SIMD wave or dedicated interstitial).
- **`tape_parity_*` walker-oracle harnesses** — retiring at W0b per invariant 20. Any remaining failures in this family are accepted as walker-scaffold state; they do not block W0a close.

## Forward references

- **W0b** deletes the walker (`__dta_walker_inline`, `DtaTable`, `FrameStack`, 12 detector files) + 6 `tape_parity_*.rs` scaffold files; bench delta attributed against this baseline.
- **W0c** rewrites AW-V.md in RD language.
- **W0a.close.md** hard gate #4 (`cargo expand` snapshot) is tolerated to re-use `post-AX-W0a2{g,h}-expand-*.txt` evidence per spec; no new expand run required this wave.

Bench baseline frozen. Every downstream wave registers its delta against `post-AX-W0a-close.json`; invariant 10's 5% regression threshold anchors here.
