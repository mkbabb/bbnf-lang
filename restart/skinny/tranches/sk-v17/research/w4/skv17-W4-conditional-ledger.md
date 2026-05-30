# SK-V17 W4 — Commit-By-Construction Alt-Mode (CONDITIONAL) re-profile ledger

Pass: SK-V17 wave triumvirate. Wave: W4 (Lever 4 / L9 — CONDITIONAL).
Status: **CONDITIONAL GATE NOT MET — L9 recorded NOT-NEEDED (not a failure).**
No source change dispatched. The re-profile is the evidence.
Base HEAD at capture: `6bb4b2a6c` (W3 NEON structural index landed — the
post-W1/W2/W3 rich-typed plane). Host: Apple M5 Max, aarch64
(`-C target-cpu=native`, fat LTO, codegen-units=1, debug=true — the
W0/W1/W2/W3-comparable plane). NEON/dotprod only; no x86/AVX/SVE.

## §0 — The conditional gate (what had to be true for W4 to dispatch)

SPEC §7 + SYNTHESIS §3 L9 + the §9 binding shortlist condition #5: **W4 dispatches
ONLY if a post-W1 typed-tape re-profile (N≥50) surfaces the recognition-control
loop (un-masked by the retired alloc floor) OR a speculative checkpoint/rollback
leaf as top-N self-time, AND the change is sound** (only pure-lexical
keyword-dispatch Alts that deposit nothing structural AND currently checkpoint
become commit-by-construction; genuinely-ambiguous CSS constructs — forgiving
selector lists, vendor-prefix fallthrough — MUST retain backtracking).

The SPEC states up front (`SPEC.md:677-678`): "The LOCKED 28.87%+2.45%
recognition-control figures are NOT a measured rollback antecedent — P1-E measured
ZERO speculative checkpoint/rollback self-time on either benched plane. If the
re-profile does NOT surface a rollback leaf, W4 does NOT dispatch; L9 is recorded
as not-needed (NOT a failure)." This wave re-confirms that empirically on the
landed W1/W2/W3 plane.

## §1 — Static evidence: the recognizer spine is ALREADY commit-by-construction

The W1 PRUNE (commit `3a37c29d8`) did not merely retire the fact-stream String — it
rebuilt the CSS recognizer as a **single-pass, delimiter-driven, commit-as-you-scan**
parser. The generated spine
(`skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs`,
representative of all 7 css_l4_* modules) has NO speculative-Alt construct:

- `parse_stylesheet` (`:460`), `parse_block` (`:528`), `parse_block_item` (`:548`):
  each is a `loop`/`match` that dispatches on **the delimiter that was actually
  found** by a single forward `find_component_delim` scan (`:493,:512,:550,:585`) —
  never a speculative trial-and-rollback over alternatives.
- `push_rule` (`:444`) / `push_declaration` (`:456`) deposit into the tape
  **immediately** on recognizing the terminator (`tape.push_offset`/
  `push_plain_offset`). The spine commits as it scans, driven by the structural
  delimiter index — this IS the W4 task-1 target shape, already realized.
- `find_component_delim` (`:657`), `consume_balanced_at` (`:693`),
  `consume_comment_at` (`:715`), `consume_string_at` (`:726`): every scan helper
  advances `pos` **monotonically** and returns `Ok(new_pos)` or `Err`. On a genuine
  error it returns `Err` and the parse aborts — there is NO `pos` save/restore, NO
  tape rollback.
- `find_colon_before` (`:682`) / `has_non_ws` (`:770`): bounded forward look-aheads
  over an **already-delimited** span — not a checkpoint, not a backtrack.

Grep over all 7 `css_l4_*/generated.rs` for
`checkpoint|truncate|rollback|split_off` returns **0** matches (the only
`offsets().len()` hit is the lazy cursor-iterator range at
`stylesheet_selectors/generated.rs:270`, a read, not a rollback save). The
O(1) `offsets().len()` checkpoint / `truncate` rollback machinery that L9 was
written to optimize **is not present in the benched CSS recognizer at all** — there
is nothing to convert.

## §2 — Dynamic evidence: real-profiler self-time on the post-W1/W2/W3 plane

Harness: `skinny/crates/bbnf-bench/src/bin/w4_css_reprofile.rs` — drives
`generated_css_l4_declaration_values::parser::rich_summary` (build offset tape +
lazily project the rich 9-field typed CSSOM) in a tight cold loop. Profiler:
`samply record --rate 4000`, symbols resolved against the debug=true binary via
`atos` (relative addresses → `__TEXT` vmaddr 0x100000000). N≥50 satisfied
(bootstrap 30000 iters / 105 052 leaf samples; material 14000 iters / 91 270 leaf
samples).

### bootstrap (the W3-identified bracket-scan-richest regular corpus, +37–39% NEON lift)

| self-time | symbol | nature |
|---:|---|---|
| **57.05%** | `CssFullParser::find_component_delim` (`generated.rs:657-680`) | forward delimiter scan — commit-by-construction, no checkpoint |
| **26.96%** | `parser::rich_summary` (`parser.rs:34`) | tape build + lazy rich projection (inlined push/classify) |
| **12.11%** | `CssFullParser::consume_balanced_at` (`generated.rs:693-713`) | forward balanced-bracket scan, monotonic `pos` |
| 2.68% | `CssFullParser::parse_block` (`:528`) | spine dispatch |
| 1.06% | `CssFullParser::parse_declaration` (`:580`) | spine dispatch |
| 0.06% | `CssFullParser::parse_at_rule` (`:479`) | spine dispatch |
| <0.1% | atos/gimli/std::fmt symbolication residue | profiler self-cost, not the parse |

### material-components-web (the slimmest-margin corpus, deepest function nesting)

| self-time | symbol | nature |
|---:|---|---|
| **46.36%** | `find_component_delim` | forward delimiter scan |
| **38.56%** | `rich_summary` | tape build + lazy projection |
| **12.23%** | `consume_balanced_at` | forward balanced-bracket scan |
| 1.67% | `parse_block` | spine dispatch |
| 0.95% | `parse_declaration` | spine dispatch |
| 0.11% | `parse_at_rule` | spine dispatch |

**Speculative checkpoint / rollback / truncate / split_off / backtrack / restore
self-time: 0.00% — ZERO matching symbols at ANY sample on EITHER corpus.** 100% of
parse self-time is the forward, monotonic, commit-as-you-scan spine plus the lazy
rich projection. The "recognition-control loop" the L9 gate names IS the
`find_component_delim` forward scan — already commit-by-construction, already the
W3-NEON-accelerated leaf, with nothing speculative left to demolish.

## §3 — Soundness: the sound subset is EMPTY

W4's soundness clause permits converting ONLY pure-lexical keyword-dispatch Alts
that (a) deposit nothing structural AND (b) currently checkpoint/rollback. On this
plane:

- There is no speculative-Alt combinator emitter in `tape_plan.rs` driving the CSS
  spine — the CSS recognizer is the hand-shaped delimiter scanner the W1 generator
  emits, not a checkpoint-per-Alt combinator tree. There is no checkpoint to remove.
- The genuinely-ambiguous CSS constructs the soundness clause protects (forgiving
  selector lists, vendor-prefix fallthrough) are handled by **bounded forward
  re-classification of an already-delimited span** (`find_colon_before`,
  `has_non_ws`, the `has_non_ws`-guarded `parse_qualified_rule`/`parse_block_item`
  match arms), NOT by speculative tape-rollback. They neither checkpoint nor would
  they be sound to "commit-by-construction" past — and they already do not.

The intersection {checkpointing} ∩ {pure-lexical, deposits-nothing} ∩ {present in
the benched spine} = ∅. There is no sound, warranted change to make.

## §4 — Disposition (the honest conditional close)

**L9 — commit-by-construction Alt-mode — is NOT-NEEDED on the SK-V17 benched plane.**
This is the SPEC-anticipated outcome (`SPEC.md:679-680`: "L9 is recorded as
not-needed (NOT a failure)"), confirmed by a real profiler on two corpora, not
asserted from the locked-architecture figures:

1. **Entry gate not satisfied** — the post-W1 re-profile surfaces NO speculative
   checkpoint/rollback leaf (0.00% self-time, both corpora) and the
   recognition-control loop is ALREADY the commit-by-construction forward scan W4
   would produce.
2. **Sound subset empty** — there is no checkpointing pure-lexical Alt to convert;
   the ambiguous constructs that must retain backtracking already use bounded
   forward re-classification, not tape rollback.
3. **>SOTA already met at W2/W3** — the tranche success criterion is met without W4:
   both regular corpora cross lightningcss decisively at W3 (bootstrap 2.23–2.26×,
   animate 2.23–2.28×), all 4 corpora 1.97–3.42×, EXACT rich 9-field equality, JSON
   51/51, checkasm PASS (`research/w3/skv17-W3-neon-structural-index-ledger.md:124-145`).

Per the §0 W4 contract: a documented "W4 conditional-not-met, >SOTA already met at
W2/W3" with the re-profile evidence IS the valid honest close. No marginal/unsound
change is forced to "complete" the wave. No source LOC, no generated change, no
REDRESS re-opened. The +5% exit-gate lift is moot — there is no candidate change to
measure it against.

## §5 — Gate posture (no change → all gates inherit W3)

- Byte-identical tape with/without an Alt-mode pass: **vacuously held** — no Alt-mode
  pass was applied; the recognizer output is the W3 plane unchanged.
- EXACT rich 9-field equality vs cssparser, JSON 51/51, checkasm parity,
  `regen --check`: **inherited from W3 unchanged** — W4 touches no source/generated
  file (the `w4_css_reprofile.rs` bench bin is a measurement-only harness, not a
  parser/runtime/codegen path).
- Pre-blocked routes honoured: no speculative-rollback admission (none attempted);
  no `split_off`/`Vec<Vec>` (none introduced); no non-byte-identical tape (no tape
  change). REDRESS-53/88/89 not re-opened.

## §6 — Artefacts

- Harness: `skinny/crates/bbnf-bench/src/bin/w4_css_reprofile.rs` (measurement only).
- Profiles: `/tmp/skv17-w4-bootstrap.json`, `/tmp/skv17-w4-material.json`
  (samply, rate 4000, symbol-resolved via atos against the debug=true binary).
- Static spine citations: `css_l4_stylesheet_selectors/generated.rs:444-776`.

W4 disposition feeds W5 close: L9 NOT-NEEDED, recorded measurably, carried as the
honest conditional close.
