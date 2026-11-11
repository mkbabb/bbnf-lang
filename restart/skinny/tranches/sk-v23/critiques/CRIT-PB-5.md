# CRIT-PB-5 — critique of PROTO-PB-5 (V8 shape-normalization microbench) and the PT-T4.W3 codex it tests

Adversarial read. Date 2026-07-10. Scope: PROTO-PB-5.md (prototype), SPEC-DRAFT.md §PT-T4.W3
(lines 546-548, charter 529-558) and the PB-5 brief (lines 736-745). All parse-that citations are
READ-ONLY verifications against live HEAD `ef10d5b`; no scratch edits were made (the prototype's
own scratch copies were not needed — the source claims verify against the tree directly).

Verdict up front: **90% converged.** This is a well-executed, honestly-labeled prototype for a
genuinely low-risk change, and the codex is well-calibrated (it does NOT over-claim perf). The 10%
residual is evidence-scope, not design doubt: the floor claim is noise-limited and needs a quiet
re-run to actually certify, and parity was proven only on JSON parse output, not the `.context`
introspection surface. Both fold cleanly into the PT-T4.W3 close gate.

## Source claims — VERIFIED ACCURATE

Every load-bearing citation in the prototype checks out against the live tree:

- `parser.ts:31` — `public context: ParserContext = {}` — verbatim. The 0-field default is real.
- `state.ts:179-189` — `createParserContext` returns the single literal `{ name, parser, args }`
  (body 184-188). Every context object in the codebase transitions through THIS one site, so the
  "one shared 3-field map" claim is a structural guarantee, not merely an observed sample. Strong.
- `parser.ts:162-187` — `mapState` with `Object.create(state)` at line 177. Exact.
- `parser.ts:488` — the `trim()` guard `parser.context?.name === "whitespace"`. Exact; and it does
  fire at construction, not in a per-parse loop, as claimed.
- `leaf.ts:134-140` — the dispatch hot path. Confirmed it reads `state.offset`, `state.src`, `tbl`,
  `parsers[idx].parser(state)` and **never touches `.context`**. The prototype's central
  interpretation (the hot path is context-blind, so this is a construction-time win) is sound.
- **mapState has zero external callers.** `grep -rn mapState typescript/src` returns only its own
  def (162/168/184), the `createParserContext("mapState", …)` at 185, and the `parserNames` string
  at `state.ts:167`. Retiring it is safe. Confirmed.

The method is honest where it counts: paired dual-realm interleave, `%HaveSameMap` /
`%HasFastProperties` / `%GetOptimizationStatus`, a `base==norm==native` parity guard, node pinned
(v26.0.0), loadavg labelled per run, and — critically — absolutes explicitly demoted to
"directional only" with only the paired ratio certified. The bench arithmetic is internally
consistent (I recomputed all three ratios and both means; they match to rounding). The verdict
"land as shape-hygiene, not a perf lever" follows cleanly from floor-unmoved + shape-fixed.

## Residual 1 (gating a stated success criterion) — "delta ≥ 0" is unprovable at this noise floor

The bench ran at **loadavg 15-26 throughout** — a heavily oversubscribed machine (the fleet). Run 3
at loadavg 26.3 inflates absolutes ~26% (20.8 vs ~16.5 ns/byte), which the prototype honestly flags.
But the consequence is that the run-to-run swing is ±1.4%, and the observed effect (median-ratio
mean +0.38%, min +0.12%) is *inside* that band. The honest statement is "**no regression detectable
beyond ±1.4%**", NOT "delta ≥ 0". The codex PB-5 success line (743) and the prototype's own MET
checkbox state the crisper "delta ≥0", which the evidence cannot support — a real 1% regression would
be invisible here. Because W3 is shape-hygiene (the close gate already says only "json-regression
floor holds"), this is not fatal, but the delta≥0 criterion should be **restated as "no regression
beyond the measured noise band" and re-run once on a quiet host (loadavg <2)** before W3 claims the
floor. Wave-owned: PT-T4.W3 close gate.

## Residual 2 (parity scope narrower than the "parity exact" claim) — the `.context` read surface

The prototype asserts blanket "parity exact / behavior-preserving" from a **JSON-parse-output-only**
test. But there are FOUR `.context` reads live, not the one the interpretation names (parser.ts:488):
`debug.ts:261` destructures `{name, args, parser}`, `debug.ts:365` reads `context.name`, and
`leaf.ts:398` *writes* `whitespace.context.name`. Under NORM the raw `new Parser(fn)` context flips
from `{}` to `{name:undefined, parser:undefined, args:[]}` — so `context.args` goes **undefined → []**
and `context.parser` undefined → explicit undefined on that path. This is in fact SAFE (I checked:
`debug.ts:268` only dereferences `args![0]` inside `name`-matched cases, and a raw parser has
`name===undefined`, so the switch falls through and never touches `args`; and `whitespace` is built
via the `regex()` combinator → already 3-field, so the `leaf.ts:398` write is an in-place field
update with no hidden-class transition in either copy). BUT the prototype demonstrated none of this —
it inferred whole-codebase parity from a corpus that never exercises debug/trace. Given PT-T4.W4
ships a grammar `trace()` on the same surface, W3's close gate should add a **debug/trace-path parity
assertion** (raw-built parser introspected under both copies). Wave-owned: PT-T4.W3/W4.

## Residual 3 (untested regression vector) — construction-cost on dynamic-parser grammars

NORM's new default calls `createParserContext(undefined as never, undefined)`, which allocates a
3-field object **plus an empty `args` array** on every `new Parser(fn)`, where BASE allocated a bare
`{}`. That is strictly MORE per-construction allocation. JSON builds its tree once at load, so the
floor is corpus-blind to it — a grammar that constructs raw Parsers inside the parse loop would see
the extra alloc. The "floor held" claim is therefore construction-static-only. Note also the shipped
W3 must clean up the `undefined as never` wart (a dedicated empty-context constant, not sentinel
args). Low-moderate; a parser-construction-heavy microbench (or an explicit "static-tree-only" scope
note) belongs in W3.

## Residual 4 (framing over-claims, minor) — codex vs evidence

- PB-5 Question (737-738) and the codex frame this as keeping "every **dispatch**-combinator site
  monomorphic". The evidence shows dispatch *Parser instances* were ALREADY same-map in BASE; the
  only thing that diverged was the `.context` *object*, read off the hot path. The framing implies
  dispatch monomorphism was at risk when it wasn't — the real (smaller) win is context-object +
  general-API monomorphism, exactly as the prototype's own interpretation states. Restate the codex
  Question to name the context object, not "dispatch site".
- PT-T4.W3 (547-548) calls mapState "the `Object.create` **dictionary-mode hazard**". The probe found
  **0/0 dictionary-mode in both copies**, and mapState is dead (zero callers) so it was never
  exercised. It is a *latent* polymorph, not an observed hazard — the prototype says "latent"; the
  codex hardens that to "hazard". Downgrade to "latent polymorph / dead-code retirement".
- Minor completeness: the prototype's call-site enumeration ("20 parser.ts + 6 leaf.ts + lazy.ts")
  omits `debug.ts:365` and `packrat.ts:479`, which also call `createParserContext`. Immaterial to the
  conclusion (all route the same literal) but it is the same omission that produced Residual 2's
  under-scoped parity.

## What is NOT wrong (defending the codex against harsher reads)

The codex does the right thing on the point most likely to be over-claimed: **it never asserts a perf
win from W3.** Charter says "additive MINOR"; the wave text describes only the shape change; the close
gate says "json-regression floor holds" (neutrality, not gain). That matches the prototype's
"robustness, not a perf lever" recommendation exactly. This is the calibration a critique usually has
to demand and here it is already present. The shape-stability clause in the close gate (555) also
already institutionalizes the `%HaveSameMap` probe. Good.

## Convergence: 90%

Executable tomorrow: yes — fully specified, prototype-validated, structurally guaranteed
(single-literal context). No open design question. The −10% is four wave-ownable evidence
refinements: (1) re-certify the floor on a quiet host and restate delta≥0 as a noise-band claim,
(2) add a debug/trace `.context`-introspection parity assertion, (3) add a construction-cost note or
microbench for dynamic-parser grammars, (4) two one-line framing corrections in the codex. All belong
inside the existing PT-T4.W3 close gate; none reopens the design.
