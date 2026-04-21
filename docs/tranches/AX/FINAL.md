# Tranche AX — FINAL

AX is the RD reckoning. It opened against AW-V's 0/17 close (every
parse entry below post-AU; JSON twitter at 24.7% of baseline; CSS,
Sheets, and BBNF at 3-7%). The plan declared three deliverables for
AX's first three waves: gate repair so JSON's visitor path could
re-route through the shape emitter (W0a), interpreter deletion plus
substrate-without-consumer purge (W0b), and the AW-V plan-document
rewrite (W0c). Block A then consolidated the API surface the rest of
AX would consume — grammar-derived view layer with canonical-
serialisation byte-equality parity against sonic-rs and lightningcss
(W1).

AX closes at master HEAD `411eabfd` after Block A delivered
exactly that. Block B (W2-W14: parity CI gating, lever portfolio,
e-graph rewriting, document-parallel) did not execute under AX's
letter — the W1 absorb re-plan reframed AX as substrate-and-API
closure, and the optimisation arc routes wholesale into AY (the
BEAT-sonic tranche, opened from this close). The successor decision
is honest scope-reveal absorption: W1's deep audits surfaced an
AU-substrate regression of ~4.5× and a json-prototype speed ceiling
that, together, define a fresh planning horizon AX cannot absorb
without silent deferral.

This document recapitulates what landed across W0a / W0a.close /
W0b / W0c / W1r (eight sub-waves), reconciles every AX invariant
1-21 with verification artefact, and hands a clean substrate to AY.

## Commit range

`9f8aed90` → `411eabfd` — the AX execution sequence on master,
consisting of W0a (W0a.1 through W0a.2.s), W0a.close, W0b cleanup,
W0c rewrite, and W1r sub-waves 0 through 7. AX HEAD is `411eabfd`
(`docs(next-tranche): A8 legacy pruning + DTA cert + housekeeping`),
the last AX-namespaced commit before AY planning docs land.

## Wave-by-wave recap

### W0a — Gate repair + non-Alt-rooted parse() routing

Multi-sub-wave cascade. `has_w4_classified` narrowed to `Pratt |
Unordered` at W0a.1 (commit `9f8aed90`); follow-through emitted
per-rule visitor dispatch for Flat / Wrap / ArgList / HRegex shapes
(`af8f6840` regen). `gate_predicate_wire_contract.rs` landed at
`69d28f56` freezing the 7×3 = 21-assertion matrix. W0a.2 absorbed
under the SPEC §Scope-reveal protocol: array emitter generalised to
admit Shape 2 entry-list rules (`ee7f81da`), AltDispatch shape
admitted Alt-of-Refs rules (`517be13c..7f3dbafb` + regen
`610928a6`), inline emission helpers shipped in `shapes/inline.rs`
(`1e603586`), Wrap parent compound + Flat iteration TapeKind fixes
(`5f0709fc..07e254f7`), Pratt LUT propagation + arena-frame API
(`64d6ab2f..7d2fa1b8`), then a six-stage emitter cascade closing
across `8dce3270..6b03dd53` resolving Pratt loop trivia, Pratt-
wrapper-peel, detector narrowing, typed-leaf payload, HRegex/Wrap/
AltDispatch arena payloads, and inline.rs Alt Regex pattern routing.

W0a closure at HEAD `6b03dd53`. Six invariant-20 semantic parity
harnesses green:

```
bbnf_parity         2/0  ok
bbnf_ast_parity     9/0  ok
css_l4_parity      16/0  ok
json_parity         9/0  ok
json_value_parity  13/0  ok
sheets_parity      25/0  ok
```

Bootstrap regen cycle-1 = cycle-2 byte-identical at 98,270 lines,
8 per-rule Pratt LUTs, 4 heterogeneous `*Value<'p>` enums, 251
skip_space call sites, reducer-compound emission preserved.

Hard gate: W0a.1 narrowing + W0a.2 cascade closing 5/5 emitter
defects + bootstrap idempotency restored. Met.

Scope-reveals absorbed (per SPEC §Absorb): W0a.2.k revert
(`f585ce37`/`4178254a`/`3256858d`) when the agent removed
reducer-compound emission against plan invariant 20; revert-to-
green + W0a.2.l re-dispatch preserved reducer-compound while
landing per-rule LUT + 1-byte arena-frame correctly. Diag at
`docs/benchmarks/post-AX-W0a2h-progress.md` §Halt rationale and
SYNTHESIS at `docs/tranches/AX/audit/SYNTHESIS.md`.

### W0a.close — Pre-W0b 18-entry bench baseline

Single-agent bench wave. `docs/benchmarks/post-AX-W0a-close.json`
captured at HEAD `5dab5175` (commit `1241e7ac`, re-run after the
W0a.2.s CSS real-corpus fix; supersedes the `7c2a7c70` baseline
where css_l4 entries panicked). Eighteen numeric entries, mimalloc,
release profile, cold per-parse. This is the attribution anchor
every downstream wave references.

Hard gate: 18 numeric entries + commit cited. Met.

### W0b — Interpreter deletion + substrate-without-consumer purge

Four parallel agents on disjoint file bounds. ~85K LOC interpreter
machinery deleted: DTA driver, simd-scan/emit purge, crate renames
(workspace-local crates lose `bbnf-` prefix per AX.W0b plan), test
carve. Closes at `0adabb23` (`test(tape): delete DTA-walker
regression tests + carve dead profile fields`). The W0b.A profile
field carve and W0b.D test carve are the two most consumer-
impactful sub-waves; both completed.

Hard gate: zero DTA symbols in 4 bench binaries + zero `dta_run_`
calls in `crates/`. Met (verified at `nm` + workspace grep).

Carry-forward into AY.W0: 5 wire-contract test files referenced
predicates retired in W0a.2.j and fields carved in W0b.A but were
not deleted alongside; A8 inventory at `docs/tranches/AY/audit/`
catalogues the omission.

### W0c — AW-V doc rewrite

Single-agent serial. `docs/tranches/AW/AW-V.md` rewritten in RD
language at HEAD `db9c4e06` (`docs(AW-V): rewrite in RD language +
§Pivot subsection`). The rewrite preserves the AW-V close-state
narrative but reframes it as the AX-opening pre-condition rather
than a free-standing tranche.

Hard gate: doc rewrite landed + invariant 8 (no legacy code, no
shims, no forward hooks) preserved at the documentation layer.
Met.

### W1r — Grammar-derived view surface (eight sub-waves)

W1 absorb re-plan landed 2026-04-19. The originally planned W1.A
(hand-coded `bbnf::json::Value` iso `sonic_rs`) and W1.B (hand-
coded `bbnf::css::StyleSheet` iso `lightningcss` with 22
`TypeOnly` stubs) violated invariants 4 (no new grammar
directives), 11 (no per-grammar prototypes), and 18 (no placeholder
surfaces). Per SPEC §Scope-reveal Absorb the orchestrator reverted
W1.A/W1.B and re-scoped W1 in place; AX.md invariant 21 added at
this point (grammar-derived view surface). No letter pivot.

Eight sub-waves dispatched, eight landed:

| Sub-wave | Commit(s) | Deliverable |
|----------|-----------|-------------|
| W1r.0 | `3429aaba` | Revert W1.A/W1.B (−6,128 LOC); sonic-rs runtime → dev-dep |
| W1r.1 | `5d5096eb` | IR-derived named-type resolver (static `BINDINGS` slice → `FxHashMap<StringId, Vec<TypeDesc>>` walker); diag at `audit/W1r1-diag.md` |
| W1r.2 | `a6429d3e` | JSON canonical-parity vs sonic-rs (10/1 + `strip_insignificant_ws`) |
| W1r.3a | `933d02fb` → `b930cf2c` → `293be673` | CSS L4 `@pretty` directives + `?w`/`@ws` threading fix + 3/0 parity (byte on normalize, scale+interop on bootstrap/tailwind) |
| W1r.4a | `f6a264e2` → `28fd46fc` → `53d99e4a` | `@pretty sep(X)` codegen fix (`backend/prettify/sep_rewrite.rs`) + regen + sheets_self_parity 84/0 |
| W1r.5 | `53318493` | BBNF self-parity 56/0 over 28 `.bbnf` fixtures |
| W1r.6 | `81627d7c` | Typed-accessor surface audit 14/0 (295 rules × 7 accessor classes) |
| W1r.7 | `ab7c218d` | Twitter lazy-field bench via NodeView typed accessors; AoS 4.14× SoA on ax-iter, 1.67× on release |

Aggregate workspace state at W1r close: 13 parity + canonical
harnesses pass on master (247 tests passed, 1 ignored — `data_xl`
debug-assertions gate; runs under `--release`).

Hard gate: every grammar's user-facing AST is `NodeView<'p>` +
`TapeCursor<'p>` + per-rule typed accessors emitted by the shape
emitters from IR's `TypeDesc` inference, composed with
`#[parser(serialize)]`-derived `serialize_compact` and
`#[parser(prettify)]`-derived `_prettify` surfaces. External-
comparator parity (sonic-rs + lightningcss) holds via canonical-
serialisation byte equality on both sides — no `From<T>`,
`PartialEq<T>`, or hand-written adapter module where `T` is a
third-party type. Met.

Scope-reveals absorbed:

- **W1r.1** — `TypeDesc::Named` collapses to a concrete tuple in
  the Rust pipeline before emit; the static `BINDINGS` slice was
  dead code on every grammar. The refactor's value is code hygiene
  + readiness to populate if upstream preserves `Named`. Upstream-
  preservation investigation routes to AY.W2 (named preservation
  end-to-end). Diag: `docs/tranches/AX/audit/W1r1-diag.md`.
- **W1r.3 / W1r.3a** — lightningcss `PrinterOptions { minify:
  false }` performs `calc()` arithmetic simplification, position-
  pair commutativity, and shorthand reordering that no symmetric
  bytes-level normaliser can invert. Bootstrap.css and tailwind.css
  ship as scale+interop tests (bbnf parses + prettifies + output
  re-parses on both bbnf and lightningcss) rather than byte-parity.
  CSS calc() evaluator deferred. Diags:
  `docs/tranches/AX/audit/W1r3-diag.md` + `audit/W1r3a-diag.md`.
- **W1r.4 → W1r.4a** — `@pretty sep(X)` double-emitted against
  rule bodies with `<<` separators; fix is codegen-level in
  `crates/core/src/backend/prettify/sep_rewrite.rs` (new module).
  Cross-grammar audit confirmed only Sheets currently declares
  `sep(X)`; the 3-line leak fix in the Repeat loop applies
  universally.

## Hard gate status

| Gate | Target | Observed | Status |
|------|--------|----------|--------|
| W0a.1 | `has_w4_classified` narrowed to `Pratt | Unordered` | landed `9f8aed90` | Met |
| W0a.2 | 5/5 emitter defects closed across W0a.2.{n,o,p,q,r} cascade | bootstrap cycle-1=cycle-2 at 98,270 lines; 6 parity harnesses green | Met |
| W0a.close | 18-entry bench baseline at `post-AX-W0a-close.json` | 18/18 numeric entries; commit `1241e7ac` | Met |
| W0b | DTA interpreter deletion + crate renames | `0adabb23` close; zero DTA symbols in 4 bench binaries | Met |
| W0c | AW-V.md rewrite in RD language | `db9c4e06` | Met |
| W1r | 13 parity + canonical harnesses green; grammar-derived view surface | 247/1 ignored on master | Met |
| W2-W14 | Block B optimisation arc | not executed under AX | Routed to AY (see §AY handoff) |
| AX-close bench | 18-entry matrix at HEAD post-W1r | `post-AX-W1-close.json` (18 numeric entries) | Met (this document's commit window) |

Block B's non-execution is a planned absorption: the W1 reveals
established that Block B's premise (apply optimisation levers atop
the AW-V-close substrate) was inverted by the AU-substrate
archaeology surfaced during W1r. The correct premise — restore
AU's flat `Vec<TapeRec>` write-path, then layer levers — is AY's
opening wave. AX closes on substrate-and-API; AY opens on
performance restoration.

## Bench delta

Per-entry comparison vs `post-AX-W0a-close.json` (intra-AX
attribution anchor) and `post-AW-V.json` (prior-tranche close).
Every AX-close number is from `docs/benchmarks/post-AX-W1-close.json`.

| Entry | post-AW-V (MB/s) | post-AX-W0a-close (MB/s) | post-AX-W1-close (MB/s) | Δ vs W0a-close |
|---|---:|---:|---:|---:|
| json/canada | 227 | 224 | 196 | -12.5% |
| json/citm | 490 | 486 | 450 | -7.4% |
| json/data_s | 484 | 486 | 442 | -9.1% |
| json/data_xl | 343 | 340 | 296 | -12.9% |
| json/twitter | 486 | 482 | 448 | -7.1% |
| css/bootstrap | 14 | 120 | 118 | -1.7% |
| css/normalize | 24 | 200 | 191 | -4.5% |
| css/tailwind | 36 | 144 | 144 | flat |
| sheets/parse_simple | 6 | 16 | 15 | -6.3% |
| sheets/parse_nested | 7 | 20 | 20 | flat |
| sheets/parse_stress | 6 | 18 | 18 | flat |
| bbnf/bbnf_self | 22 | 89 | 87 | -2.2% |
| bbnf/css_l4_grammar | 33 | 125 | 125 | flat |
| bbnf/css_pretty | 35 | 154 | 148 | -3.9% |
| bbnf/ebnf | 11 | 46 | 46 | flat |
| bbnf/google_sheets | 52 | 207 | 202 | -2.4% |
| bbnf/json | 16 | 72 | 72 | flat |

CSS / Sheets / BBNF picked up the W0a routing fix dramatically
(8.5×, 2.7×, ~4.5× respectively at W0a.close vs AW-V); JSON held
near AW-V because JSON's parse() already routed through the shape
emitter pre-AX. The W1r delta against W0a-close is small and
slightly negative on JSON (4-13% across the 5 entries) — the
NodeView + typed-accessor surface, the `@pretty` directives that
unlocked CSS canonical parity, the prettify `sep(X)` fix, and the
canonical-serialisation harnesses all add work to the parse path
without the optimisation levers that would compensate. This delta
is the AY opening posture: the AU-substrate archaeology
(`docs/tranches/AY/AY.md` §Opening) measures twitter at 0.137
bytes/cyc = 17% of sonic-rs vs post-AU's 0.615 bytes/cyc = 76% of
sonic-rs. AY's W1+W2 target restoration to ≥ 0.85 bytes/cyc.

## Invariant verification

Every AX invariant 1-21 reconciled with the W1r close state.

1. **One codegen path** — verified. W0b deleted the interpreter;
   W1r sub-waves did not introduce any fallback. Single shape-
   emitter path reaches every consumer.
2. **Tape Value API monomorphised at user's target type** —
   verified at the `NodeView<'p>` + per-rule typed-accessor surface
   landed at W1r.6 (`81627d7c`, audit 14/0). The runtime value
   pattern is the user's `T: Visitor` bound, not a tagged-union
   `Value` enum.
3. **All unsafe concentrated in kernels** — verified. W0a-cascade
   emitter fixes did not introduce new `unsafe` outside the
   kernel-bounded sites; `unreachable_unchecked` at proven-dead
   dispatchers remains the one emitter-inserted unsafe primitive.
4. **No new grammar directives** — verified. AX shipped zero new
   directives. W1.A/W1.B's hand-coded value duplicates were
   precisely the violation invariant 4 prohibits; the W1 absorb
   re-plan reverted them at W1r.0 (`3429aaba`) and reframed W1
   around the grammar-derived view surface (invariant 21).
5. **Parity harnesses are binary: pass or fail** — verified. 13
   parity + canonical harnesses on master at W1r close report 247
   passed / 1 ignored (the `data_xl` debug-assertions gate runs
   under `--release`). No tolerances.
6. **Document-parallel fork is opt-in via
   `ParseOptions::parallel_threshold`** — preserved as plan-time
   contract; not executed under AX (Block B routes to AY.W8).
7. **Wire-contract end-to-end tests per IR-derived emitter
   output** — `gate_predicate_wire_contract.rs` (`69d28f56`)
   freezes the 7×3 matrix; `bbnf_profile_wire_contract.rs` and
   `grammar_profile_wire_contract.rs` exist for the
   `GRAMMAR_PROFILE` const surface. Five of these test files
   reference predicates retired in W0a.2.j and fields carved in
   W0b.A; AY.W0 retires them per the invariant 14 discharge.
8. **No legacy code, no shims, no forward hooks for AY** —
   verified at code level. AY-specific scaffolding lives in
   `docs/tranches/AY/` planning docs only.
9. **Gate predicates frozen after introducing wave** — preserved.
   W0a.1's `has_w4_classified` narrowing is the last admission-
   widening AX shipped. W0a.2.j flipped the gate-predicate wire-
   contract `expected` map per the absorb re-plan; that flip is
   the AX-as-introducing-wave decision, not a downstream widen.
10. **Mid-wave bench-checkpoint** — checkpointed at W0a.close
    (`post-AX-W0a-close.json`) and at W1r close (this document's
    `post-AX-W1-close.json`). The intra-AX delta is documented in
    §Bench delta above.
11. **No per-grammar hand-tuned prototypes** — verified by W1
    absorb re-plan (revert of W1.A/W1.B). The shape emitter
    remains the sole generality mechanism.
12. **Wave discipline carries from AW** — verified. W0a's six-
    stage cascade and W1's eight sub-waves dispatched through
    declared file-bound worktrees; no concurrent writes to shared
    files across same-wave agents.
13. **Ledger-only wave = re-plan trigger** — verified. W0a.2.k's
    revert under SPEC §Transitional fallback is the operational
    proof.
14. **Gate-predicate symmetry** — preserved at the wire-contract
    layer, with the discharge for 5 stale wire-contract test files
    routed to AY.W0 (the test files lag the predicate retirements
    by one tranche).
15. **Small-input amortisation documented at plan time** — Sheets
    parse entries (505 B – 1.8 KB) opened with the W0a routing
    boost and remained at 15-20 MB/s through W1r close; the
    structural amortisation question routes to AY.W3+W4.
16. **Predicate-widening requires re-bench** — verified. W0a.2.j's
    admission widening was followed by the W0a.close bench
    capture.
17. **"Architectural transposition complete; throughput in next
    wave" is not a closeable wave** — preserved. Every AX wave
    closed on a verifiable substrate landing or a documented
    consumer-side absorb; no wave closed solely on transposition.
18. **No stubs, no shims, no placeholder surfaces** — verified at
    W1r.0 revert (W1.A/W1.B's `TypeOnly` stubs deleted). Field-
    complete on day one of every shipped sub-wave.
19. **Per-wave spec documents** — preserved. Every executed wave
    carries its own `docs/tranches/AX/waves/W<N>.md`.
20. **Tape shape is shape-emission-authoritative** — verified.
    `tape_parity_*.rs` retired at W0b per the W0a.2.h pivot;
    `*_parity.rs` AST-level harnesses are the one source of truth
    for downstream correctness. No emitter consumer reads from
    `__dta_run_*` symbols (zero in `nm` on 4 bench binaries).
21. **Grammar-derived view surface** — verified. NodeView + typed
    accessors + canonical-serialisation parity landed across
    W1r.1-W1r.7. No hand-coded value duplicates; no third-party
    comparator bridges. External-comparator parity holds via
    canonical-serialisation byte equality on both sides.

## Cross-tranche debt addressed

| Item | Origin | AX wave | Status |
|------|--------|---------|--------|
| `has_w4_classified` over-restricts JSON visitor | AW-V.W6 carry | W0a.1 | Closed (`9f8aed90`) |
| 0/17 parse entries below post-AU | AW-V.W6 close | W0a routing boost | CSS/Sheets/BBNF restored 2.7-8.5×; JSON held flat at AW-V level; full BEAT-sonic recovery routes to AY |
| Interpreter as architectural debt | AW-V carry | W0b | Closed (`0adabb23`) |
| `__dta_walker_inline::run` consumer fallback | AW-V.W6 carry | W0b | Closed (zero call sites) |
| AW-V plan document RD-language conformance | AW-V close | W0c | Closed (`db9c4e06`) |
| Hand-coded value duplicates as parity bridges (W1.A/W1.B prototype) | AX.W1 plan-time error | W1r.0 absorb | Closed (revert + invariant 21 added) |
| Grammar-derived view surface | AX.W1 absorb | W1r.1-W1r.7 | Closed (NodeView + typed accessors + canonical parity) |
| `@pretty sep(X)` double-emit | W1r.4 reveal | W1r.4a | Closed (`backend/prettify/sep_rewrite.rs`) |
| `TypeDesc::Named` collapse before emit | W1r.1 reveal | (diag) | Routed to AY.W2 |
| CSS calc() canonical-form arithmetic divergence | W1r.3 reveal | (diag) | Routed to AY post-tranche review |

## Cross-tranche debt routed to AY

Per `docs/tranches/AY/AY.md` and `docs/tranches/AY/waves/W0.md`:

| Item | Origin | AY wave |
|------|--------|---------|
| 5 stale wire-contract test files (predicates retired W0a.2.j; fields carved W0b.A) | AX-arc residual | W0.1 (`git rm` 5 files) |
| 2 stale emitter-shape test files (`__CLASSIFY_TABLE` + `ConsumeToNextStructural` not emitted) | AX W0a.2 emitter pivot | W0.1 |
| `ebnf_prettify` recognizer fails at offset 0 on valid EBNF source | pre-existing AX residual | W0.2 |
| ~458 LOC DTA kernel-dead in `crates/tape/src/dta.rs` | AX W0b carry-forward | W0.3 |
| Dead `GrammarProfile` fields (`list_rules`, `shape_dict`, `push_*_count`) | AX W0b carry-forward | W0.4 |
| `crates/tape/src/shape_dict.rs` (`BBNF_SHAPE_DICT` never emitted) | AX W0b carry-forward | W0.5 |
| AU-substrate AoS revert (twitter 0.137 → ~0.45 bytes/cyc target) | A1-A10 archaeology | W1 |
| Named preservation end-to-end + e-graph G1-G9 + wrap-compound elision (target 0.45 → ~0.85) | W1r.1 diag + A6/A7 audits | W2 |
| Value API runtime substrate + json-prototype per-shape emission (target 0.85 → ~1.00) | A2/A4 audits | W3 |
| SIMD unescape + Eisel-Lemire direct-to-column (target 1.00 → ~1.15-1.40) | AU FINAL §3 carry | W4 |
| CSS L4 @import split + DFA hoist + shared PHF | A3 audit | W5 |
| parse_that de-generic + ax-iter profile tuning | A3 audit | W6 |
| Document-parallel fork (AX.W9 demoted) | AX.W9 plan absorb | W8 |
| AX W2-W14 absorption per A7 (6 fold, 2 new, 3 retire-as-obsolete, 1 legacy tooling deferral, 4 legacy later-successor deferrals) | AX Block B non-execution | distributed across AY waves |

## Block B non-execution rationale

AX.md declared 19 waves; six executed under AX's letter (W0a,
W0a.close, W0b, W0c, W1r). W2-W14 + W15 did not execute. The
decision is honest scope-reveal absorption, not silent deferral:

The W1r sub-waves surfaced two facts that invalidated Block B's
premise. First, ten audits at master HEAD `411eabfd` (A1-A10,
landed 2026-04-19/20 as planning artefacts) measured a ~4.5×
regression from post-AU on twitter (1,967 MB/s → 437 MB/s); the
SoA 7-column write pivot from AV/AW-I and the AW-V.W6 wrap-compound
emission stacked into a write-path tax that dominates parse time
on every grammar. Second, the json-prototype crate (AW-V.W2.1, the
substrate viability proof) demonstrated that the grammar substrate
is still capable of sonic parity (0.89-0.94× on 5 fixtures) — the
regression is entirely emitter-shape + tape-substrate-layer.

Block B's planned levers (W4-W8 SIMD widening, W10-W11 e-graph
rewriting, W9 document-parallel) all assume the AU-grade substrate
beneath them. Layering them atop a 4.5×-regressed tape would
produce the AW-V outcome again: substrate-without-restoration. The
correct sequence is restore-then-lever, which is AY's W1 → W2 →
W4 wave order.

The W2 parity CI gating, W12 detector retirement, and W14 multi-
visitor pairs are bench-gated post-tranche review candidates per
the AY plan; they fold into AY waves where their substrate
prerequisites land. Per SPEC §Scope-reveal "Absorb vs new letter"
the discriminator is mechanical: Block B's revealed work spans
substrate AND consumer changes that together exceed any single
extension of the AX wave schedule. New letter (AY) is the response
mode.

## What did not land

1. **Block B optimisation arc** (W2-W14). Routed to AY per
   §Block B non-execution rationale and `docs/tranches/AY/AY.md`
   §Wave summary. Each AY wave names the specific AX-Block-B item
   it absorbs.
2. **Workspace 0-failed close on the 5 wire-contract test files**.
   The five test files compile-fail at the time of this document
   (predicate retirements in W0a.2.j and field carves in W0b.A
   left the test predicates stale). AY.W0.1 retires them in a
   single `git rm` commit citing the invariant 14 discharge. The
   `cargo test --workspace` close gate is not closeable at AX HEAD
   `411eabfd` and is the AY.W0 hard-gate prerequisite.
3. **`ebnf_prettify` recognizer**. Pre-existing failure at offset 0
   on valid EBNF source. Not caused by W1r landings (bbnf_self_
   parity 56/0 parses ebnf.bbnf via BbnfEmit). AY.W0.2 dispatches
   a parallel diagnosis agent.
4. **Bench matrix exceeds post-AU on every entry** (the AX → AY
   handoff condition 4 from the original AX plan). Recovery is
   AY.W1's restoration target; the BEAT-sonic declaration is
   AY.W7.
5. **`has_w4_classified` and analog gate predicates deleted** (AX
   handoff condition 5). Predicates are narrowed and wire-contract
   tested but not deleted; deletion is bound up with the e-graph
   rewriting that subsumes their logic, which is AY.W2.
6. **All 9 e-graph rewrites active; 12 detector files deleted**
   (AX handoff condition 6). Routed to AY.W2.

## Successor chain

AX now hands off into the canonically ordered successor chain
`AY → BA → BB → BC`. AY is the near-parity closure tranche
(`docs/tranches/AY/AY.md`). BA is the direct post-AY performance
successor, BB is the post-BA toolchain/compile-time discipline
tranche, and BC carries replay/recovery/incremental/debug tooling on
the BA-close substrate.

The AY plan is authored at this document's commit window with
nine waves, dependency chain W0 → W1 → {W2 ∥ W4} → W3 → W5 → W6
→ W8 → W7. AY.W0 retires the AX-arc legacy debt enumerated above
(7 stale tests + ~458 LOC DTA prune + dead GrammarProfile fields
+ shape_dict.rs delete + this AX.FINAL artefact + housekeeping)
as the precondition for AY's substrate restoration in W1.

## Artefacts

### Plan + execution documents
- `docs/tranches/AX/AX.md` — plan
- `docs/tranches/AX/PROGRESS.md` — dated execution log (closes
  with the entry appended at this document's commit)
- `docs/tranches/AX/waves/W{0a,0a.close,0b,0c,1,…}.md` — per-
  wave specs
- `docs/tranches/AX/audit/SYNTHESIS.md` — research wave
  synthesis (W0a.2.h pivot)
- `docs/tranches/AX/audit/W1r1-diag.md` — Named-collapse diag
- `docs/tranches/AX/audit/W1r3-diag.md` + `W1r3a-diag.md` — CSS
  canonical-form divergence diagnostics
- `docs/tranches/AX/audit/R{1,2,3,4}-*.md` — research artefacts

### Test additions
- `crates/core/tests/gate_predicate_wire_contract.rs` — 7×3 = 21
  assertion matrix freezing W0a's gate-predicate decisions
- 13 parity + canonical harnesses on master (247 passed / 1
  ignored at W1r close)

### Generated artefacts
- `crates/core/src/grammar/generated.rs` — 98,270 lines, 8
  per-rule Pratt LUTs, 4 heterogeneous `*Value<'p>` enums,
  reducer-compound emission preserved; cycle-1=cycle-2 byte-
  identical (W0a.close)

### Bench artefacts
- `docs/benchmarks/post-AX-W0a-close.json` — intra-AX baseline
  (commit `1241e7ac`, HEAD `5dab5175`)
- `docs/benchmarks/post-AX-W1-close.json` — AX-close 18-entry
  matrix at HEAD `411eabfd` (this document's commit window)

## AX HEAD

Master HEAD `411eabfd` at AX close. 13 parity + canonical
harnesses green (247 passed, 1 ignored). Bootstrap idempotent at
98,270 lines. Eighteen bench entries measured cold, mimalloc,
release profile.

AX closes honestly: substrate-and-API closure delivered (W0a +
W0a.close + W0b + W0c + W1r); optimisation arc absorbed into AY's
opening waves under SPEC §Scope-reveal new-letter response. The
BEAT-sonic charter passes to AY.
