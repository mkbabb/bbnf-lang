# LOCK-8 — THE TS BACKBONE PLANE — pass-3 FAMILY SPECS (synthesis of record, Fable-tier)

*Four orthogonal planes over ONE closure core: 8A runtime (V8 JIT), 8B type (TS checker), 8C
correspondence, 8D API. parse-that/typescript is READ-ONLY to the bbnf campaign; every landing
rides letter BBNF-PT-G (owner or a parse-that-side session); prototypes run on scratch copies
base-pinned to HEAD (`ef10d5b`). Two engines govern and are never conflated: the V8 JIT (8A) and
the TS checker (8B). This doc supersedes the pass-2 FAMILY-SPECS AND the stale intermediate 15:35
pass-3 draft; it folds the current (19:xx) research-{A,B,C,D,priorart} + PROTO-1 + PROTO-2. This
pass RAN two prototypes on real trees and RE-RAN the consumer census three ways.*

---

## GOVERNING PASS-3 FACTS (banked; supersede all stale census text)

**F1 — The consumer divorce is REAL at src level (a research-internal conflict, RESOLVED at source).**
value.js@4.0.0 (`tranche-u`, HEAD `86a9a17`) has **no `dependencies` key** and **zero
`@mkbabb/parse-that` imports in `src/`** — re-verified this session (`grep` → 0 hits;
`'dependencies' in pkg == False`; `'parse-that' in json.dumps(pkg) == False`). research-B's "2
residual imports" (`src/subpaths/math.ts:2`, `transform.ts:4`) are **doc-comment strings**
("parse-that-FREE", "zero parse-that"), NOT imports — read at source. research-A, research-D, and
PROTO-2's census concur; the earlier "value.js is DUAL-STATE / branch artifact" reading is
**withdrawn**. CSS is re-homed hand-written at `value.js/src/v4/css/grammar.ts` (String.split-driven,
zero char-cursor). keyframes.js@6.0.0 is transitively severed (0 hits). **P1 owner-confirmation
remains open** (intended tree-of-record vs paused-WIP branch, XL-G), but every mechanical read weighs
"divorced/settled": commit `470818c` ("parser migration") dropped the dep on **2026-02-25**, is in
`master` + every active tranche branch, ~5 months combinator-free (research-A §2).

**F2 — latex-paper is the SOLE surviving live consumer; baseline RAN GREEN.** `@0.2.1`, pin `^0.7.1`
→ installs `0.7.1` (pre-1.0.0, pre-arity-unroll). `npx vitest run` → **127/127 pass, 14 files, exit
0** (research-D §0, RAN — the falsifiable before-state). Import surface = exactly **5 symbols**
`{Parser, ParserState, regex, string, any}` across 8 sites; **no `all`, no subpath**
(`/core`,`/diagnostics`,`/packrat`,`/utils` untouched). Its `any` traffic is arity-3 + dynamic-spread
+ arity-≥15 (`document.ts:81` `inlineNode`) — **never the arity-2 unroll**. Liveness reads split:
active on its OWN work (VIRT tranche) but **frozen relative to parse-that** (pin unmoved off `<0.8.0`;
last code change 2026-07-02). **P2** ("live-forward vs fossil") leans live-forward on research-D's
activity read; it is NOT confirmed to migrate — the pin has not moved and the 0.7→1.1 jump is untested.

**F3 — PROTO-1 RAN (8A): the arity-unroll TEMPORAL leg is REFUTED at the shipping tier.** Scratch
parse-that at pin `ef10d5b`, Node v26 default (Maglev on), REAL leaves. The STRUCTURAL leg holds
tier-independently (`%HaveSameMap(diagOff,diagPopulated)==TRUE`; no map transition; alloc stable). The
TEMPORAL leg (Maglev inlines the const-captured `p_i.parser`) is **FALSE for the real `regexParser`
leaf** — bytecode 248 exceeds `--max-maglev-inlined-bytecode-size=100`, so the unrolled arm inlines
ONLY the small `stringParser` (bc90); both leaves inline only at **Turbofan, which is non-resident**.
The general arm (arity-4) inlines NEITHER at any tier. **The arity-unroll's temporal value on real
leaves is Maglev-size-capped**; closing it needs leaf bytecode <100 or the LOCK-4B AOT-fused emitter
that removes the `.parser` indirection.

**F4 — PROTO-2 RAN (8C): the priority-DFA WORKS but is latent-only; Route A RETIRED.** A
priority-preserving subset construction (`pfa.rs`, 369 LOC, additive; `dfa.rs` byte-identical) was
BUILT and proven: `col3==col1` (leftmost-first == JS) on H1 (`a|ab`→`"a"`) and H3 (lazy `\d+?`→`"1"`);
state ratio **1.00×** (never larger, sometimes smaller); Hopcroft-safe (0 divergences); floor 44/44.
BUT the census over all ~33 live `regex()` literals finds **ZERO** exercising a JS↔Rust divergence: no
shorter-first alternation exists anywhere; the 2 lazy literals carry a lookahead and are rejected at
HIR-parse (H5); H4/H5 already refused by the HIR. **Route A is a correct fix for a defect the live
corpus never trips → RETIRED as latent-only; Route B (classify-gate) ships**; `pfa.rs` is banked as a
validated shelf artifact. **Consequence: the pass-2 `8C-guard ⟂ LOCK-5-fix` incompatibility DISSOLVES**
— the routes are complementary, not exclusive; no foreclosure.

**F5 — 8D-G-Y13 ADJUDICATED mechanically (research-D §1).** parse-that already ships
`proof:no-dead-combinator` in its `proof:all` chain — a mechanical gate asserting every combinator is
exercised by the first-party corpus. The repo has already operationalized "the first-party suite is the
consumer of the algebra." Three tiers settle it (§8D).

---

## 8A — THE SHAPE-STABLE CORE (V8 runtime plane)

**Architecture.** The V8-first formulation: every `ParserState` and combinator object shape-stable and
monomorphic by construction; an allocation-zero default hot path; diagnostics and packrat as
shape-preserving optional planes. Two physically-distinct wins are reported SEPARATELY, never summed:
the **structural leg** (allocation + hidden-class, JIT-tier-independent) and the **temporal leg** (JIT
inlining, tier-dependent — warm, disclosed, never gate-anchoring).

**Mechanism (proven vs throttled).**
- *Substrate (durable, measured, KEEP):* `ParserState` fixed-from-birth hidden class (8 constructor
  fields; `expected`/`suggestions`/`secondarySpans` stamped at construction → seeding a diagnostic is a
  pure value write, no transition — PROTO-1 `%HaveSameMap==TRUE`); mutable single-threaded state (the
  Parsimmon lesson — immutable-state-per-result is the biggest perf sink, never relitigate); sticky
  `regex.test()` zero-alloc; `Int8Array(128)` FIRST-set `dispatch()` LUT; lazy diag-array alloc (−60.1%
  B/op fused, census 6→0 — NOT applied at pin `ef10d5b`, where 248 B/op still pays two empty arrays; map
  stable regardless).
- *The arity-N specializer (contested, now bounded):* a runtime grammar-construction-time branch on
  `parsers.length` (`leaf.ts:39-57` `any`, `183-272` `fuseAll`), const-captured `p0`/`p1`/`p2`. Its
  **structural leg is a legitimate cold win** (keyed-load + loop bounds-check vanish; N monomorphic call
  sites; one flat pre-sized result array). Its **temporal leg is Maglev-size-capped on real leaves (F3)**
  and warm-only. Its **live external corpus is empty (F1/F2)**: zero live callers of the arity-2/3
  unrolls; latex's only `any` traffic is arity-≥3/dynamic.
- *The structural megamorphic IC at `.parser(state)` is out of 8A altitude* — parse-that's perf doc names
  it structural to the closure pattern (5.9% CPU); the runtime tagged-union/`switch` escape is
  LAW-REFUTED on V8 (−10–14%, KILLED). 8A MEASURES the IC state, it does not rebuild the engine (LOCK-4).

**Integration seams.** `state.ts:36-53` (fields), `:87-99` (`unsafe*` choke), `leaf.ts:39-272`
(specializer arms), `parser.ts:177-180` (`mapState` `Object.create` — the ONLY prototype-chain object on
the hot path; the shape oracle asserts this).

**Gates (born-RED where applicable).**
1. Shape oracle: `%HaveSameMap(diagOff,diagPopulated)==TRUE`; `mapState` is the sole prototype-chain
   object; no unrolled arm mints a new hidden class. **RAN, PASS (PROTO-1).**
2. Two-leg arity oracle: structural (cold, paired-interleaved N≥12) reported apart from temporal (warm,
   tier residency via `%GetOptimizationStatus`). **RAN (PROTO-1)** — structural PASS, temporal REFUTED at
   resident Maglev.
3. Live-corpus re-census committed as a falsifiable artifact. **RAN.**

**Exact open questions.**
- **8A-Q1 (the real live headroom):** the arity-2/3 unrolls are dead; latex's live `any` shape is
  arity-≥15 `inlineNode` on the *general* arm. Does routing it through the existing FIRST-set
  `dispatch()` LUT (or a first-char prefilter) beat the sequential general arm cold per-op? Un-measured —
  this is where the *live* corpus's headroom sits.
- **8A-Q2:** disposition of the arity-2/3 unrolls — keep as a first-party-bench-gated structural micro-opt
  or prune (coupled to 8D Tier B / F5). Both the temporal (F3) and consumer (F1) defenses fell.
- **8A-Q3:** center-B → LOCK-4B AOT fuser: BLOCKED for the TEMPORAL specialization (Maglev-capped +
  near-zero live payoff + 4B banked its TS face), NOT for the structural core.

**Standing:** ADVANCED. First measured evidence this pass. The durable substrate is sound; the
specializer's two defenses (temporal, consumer) both fell — it survives on the structural leg +
first-party bench only.

---

## 8B — TYPE-LEVEL TOTALITY (type plane)

**Architecture.** The TS type system carries the grammar: full positional `ExtractValue` inference, a
typed combinator algebra with no `Parser<any>` in the public surface, all erased to zero runtime cost.
The subject is the gap between what signatures PROMISE and what the runtime DELIVERS.

**Mechanism (the pass-2 flagship "leak" reclassified — research-B §1).**
- **8B-G-ASUNKNOWN is RESOLVED, not a leak.** The `as unknown as` in `all`'s value path is **LAWFUL
  variance-bridging**: `all()` itself carries a single `as Parser<Result>`; the `as unknown as` lives in
  the value producer `state.ok` (`state.ts:59`) and the `all`-consuming `trim`/`wrap` (`parser.ts:481,
  485,520,394`). Each bridges the invariant `value`-slot on one runtime object whose slot genuinely holds
  the named value — `unknown` is the *required* honest intermediary for an invariant reconstruction. This
  is the documentable erasure class of `ParserFunction<T>`'s phantom `T` and `MemoCell.value: unknown`.
  GRAFT A discharges: justify-and-document, do not chase.
- **The REAL leak is cast-independent: undefined-compaction arity mismatch (R-B1).** `fuseAll` compacts
  `undefined` (`if (state.value!==undefined) out[w++]=…; if(w!==n) out.length=w`), so `all(string("a"),
  whitespace, string("b"))` types `Parser<[string,string,string]>` but yields `["a","b"]` at length 2
  whenever an element empty-matches or is `opt()`-typed. **No cast fixes this** — it is a type→runtime
  arity contract `fuseAll` breaks; the erasure merely conceals it.
- **Three routes, one adjudicated.** (1) Keep the O(N) mapped type + **document** the compaction caveat
  (tighten bound `all<const T extends readonly Parser<unknown>[]>`); TS2589-free; the type stays
  *nominally* unsound for `undefined`-typed elements, labelled LAWFUL-NON-MIRROR with the caveat —
  **RECOMMENDED**. (2) Recursive `undefined`-filtering tuple = the exact TS2589 depth-cliff — REJECTED.
  (3) Stop compacting = sound type but **semver-major breaking** (wrap/trim rely on the discard) — out of
  1.1.0 scope.
- **The input leaks are shallow, depth-1, TS2589-free (research-B §2).** `skip<S>(Parser<T|S>)` →
  `skip(Parser<unknown>): Parser<T>` (S phantom, value discarded); `sepBy` identical; `then<S>` drop `|T`;
  `chain<S>` drop the `S|T` return union; `restore` → `value:T`; `or` already precise; `ParserFunction`'s
  phantom `T` LEAVE AS-IS. All remove a union/phantom from a depth-1 signature → instantiation cost DROPS.
- **De-dup:** `ExtractValue` is defined twice (`parser.ts:9-11` + byte-identical inside `leaf.ts:155`);
  one exported home `src/parse/types.ts`, imported as `import type` by both. Pure refactor.

**Integration seams.** `leaf.ts:154,179`; `parser.ts:81,124,189,569`; `state.ts:79`; new
`src/parse/types.ts`.

**Gates (born-RED where applicable).**
1. Compaction oracle: paired type-vs-runtime proving inferred-arity ≠ observed-length. **PROTO-1 pass-2
   ran the TS side** (`b` present→len 3, absent→len 2). The cross-language leg (does Rust compact?) is
   un-run → Brief 2.
2. Tightened type-test suite (`test/types/inference.test-d.ts`, `tsc --noEmit`) RED-on-1.0.0/GREEN-after,
   `@ts-expect-error` proving precision both ways.
3. **8B-G4'-EMIT — the never-fired zero-runtime-cost gate.** Two-step byte-diff on the REAL vite toolchain
   (never an esbuild proxy): step-(i) documents the toolchain-version delta; step-(ii) **must be
   byte-empty** (1.0.0 source vs tightened source at fixed toolchain). A non-empty step-(ii) `.js` diff
   halts the family. **UN-RUN** → Brief 1.
4. Instantiation budget (`tsc --generateTrace`) on self-use under the EXACT tightened signature,
   deepest-nest TS2589-free, disclosing signature + tsc version + corpus. Native corpus recorded
   **un-runnable** (latex touches none of the tightened combinators; value.js divorced).

**Exact open questions.**
- **8B-Q1 (cross-language):** does Rust `seq!`/`impl_parser_flat` compact `undefined`/unit, or produce
  fixed-arity tuples? If fixed-arity, TS's compaction is a Rust↔TS DRIFT row (8C) AND route-1's caveat is
  a *mirror divergence* the 6D corpus must carry. (→ Brief 2.)
- **8B-Q2:** the emit-identity step-(ii) has NEVER fired on the real toolchain (→ Brief 1).
- **8B-Q3:** the `skip`/`sepBy` residue is un-tightened; "no `Parser<any>` anywhere" is ~70% closed.

**Standing:** ADVANCED. The pass-2 "flagship soundness leak" is disarmed as a lawful erasure; the real
leak is now correctly located. Route-1 adjudicated. The residue is three un-run gates (emit-identity,
native budget, cross-language mirror).

---

## 8C — THE ISOMORPHIC MIRROR (correspondence plane)

**Architecture.** The TS combinator plane as the lawful mirror of the Rust `parse_that` core. The
deliverable is a **machine-checkable divergence ledger** (`LEDGER.tsv`: `MIRRORED` /
`LAWFUL-NON-MIRROR(reason)` / `DRIFT` per symbol), a **regex-correspondence adjudication** with a
differential paired-triple corpus, and the **priority-preserving subset construction** routed to LOCK-5.
The general mirror machinery (linter, skeleton generator, parity harness) is CEDED to LOCK-6 (6A/6B/6D);
8C's residue is the ledger + the regex classification. Proven by census and oracle, not asserted.

**Mechanism.**
- *The ledger is GENERATED:* a census walks both trees (TS via ts-morph over `typescript/src/parse`; Rust
  via `cargo public-api`/`pub fn` census over `parse_that/src`) and emits `LEDGER.tsv`; a re-run yielding
  a new unclassified symbol on either side is a CI failure. **The ledger IS LOCK-6A's linter whitelist**:
  LAWFUL-NON-MIRROR rows = whitelist entries (with reason), MIRRORED = enforced correspondences, DRIFT =
  red until dispositioned.
- *Classified divergences (census-derived):* the arity row = **LAWFUL-NON-MIRROR(idiom)** (both sides
  specialize arities ≥2; V8 hand-unroll+dispatch vs LLVM `seq!` macro / `impl_parser_flat` — the
  *decision* mirrors, the *mechanism* diverges lawfully); the `*Span` family =
  **LAWFUL-NON-MIRROR(excision)** (TS excised the zero-copy `Span<'a>` surface in the 1.0.0 cut,
  `proof:no-span-surface`); SIMD scan → LOCK-9; CSS → LAWFUL-NON-MIRROR(domain-ownership); repetition/
  dispatch DRIFT (`many_small`/`sep_by_ws`/`dispatch_byte_multi` — Rust-only, force disposition); packrat
  DRIFT (TS-only, → LOCK-4/6); diagnostics MIRRORED including a **mirrored defect** (both sides carry the
  module-global collected-diagnostics defect PT-E1 cures — so the fix is MIRRORED per-parse scope both
  sides; 8D's `state.diag` grafts into this row, GRAFT B).
- **The regex row = MIRRORED-WITH-GUARD (Route B), pfa.rs shelved (F4).** Hazard taxonomy: H1–H5
  (priority-only) dissolvable by a priority-DFA; H6 (Unicode table gap), H7 (flags), H8 (lookaround),
  H9 (backref) NOT priority problems (H8/H9 non-regular — no DFA realizes them). PROTO-2 proved the
  priority-DFA correct AND that the live corpus trips NONE of the dissolvable divergences → **Route B
  classify-gate ships** (H4/H5 refused at HIR-parse; H8/H9 grammar-fenced); **Route A (`pfa.rs`) banked
  validated-shelf** for a future live shorter-first/standalone-lazy literal. The engine defect (priority
  built then discarded at `dfa.rs:189-202`; leftmost-longest delivered; lazy→greedy) is REAL but **not
  consumer-load-bearing** — routed to LOCK-5 as a *latent* gap, fixed-in-principle by `pfa.rs`,
  guarded-in-practice by Route B; **the two are complementary, the guard must NOT foreclose the fix.**

**Integration seams.** `LEDGER.tsv` (generated, un-built — the deliverable); `dfa.rs:189-286` (the
priority-blind closure, LOCK-5); `pfa.rs` (369 LOC, in scratch, banked); the ledger↔6A whitelist
handshake; the tape-crossing home ambiguity (`crates/bbnf-bench/ts-tape/` vs `ts/tape/` — deferred to
LOCK-6).

**Gates.** (1) Generate `LEDGER.tsv`; prove a synthetic new symbol fails CI — **UN-BUILT**. (2) Paired
H1–H9 corpus through {J,D,P} — **RAN (PROTO-2)**. (3) Priority-DFA scratch: classes-dissolved / parity
regression / state count — **RAN: 2 dissolved, 0 regression, 1.00×**. (4) Every perf-bearing row carries
`perf_fence ∈ {pure-js-decode, wasm-boundary}` + HEAD (4.78× stop-early stays fenced pure-JS-decode;
wasm→JS export cost UNMEASURED, never migrated onto a wasm-boundary claim).

**Exact open questions.**
- **8C-Q1:** `LEDGER.tsv` is un-built (the whole residue after cession) — a construction owed, not an
  uncertainty.
- **8C-Q2:** does the 8B undefined-compaction produce a NEW regex-independent DRIFT row (Rust fixed-arity
  vs TS compacted array)? (→ Brief 2, shared with 8B-Q1.)
- **8C-Q3 (ceiling):** 8C cannot converge to 100% independent of LOCK-6 (regen gate 6B, parity oracle 6D)
  — flagged for GESTALT-P.

**Standing:** ADVANCED. PROTO-2 is the lock's best-resolved prototype: it BOTH built the priority-DFA
(proving the LOCK-5 fix sound + cheap) AND retired it as latent-only (proving Route B suffices),
dissolving the pass-2 8C-guard⟂LOCK-5 incompatibility. The residue is a build (the ledger), not an
uncertainty.

---

## 8D — CONSUMER-CONTRACT-FIRST (API plane)

**Architecture.** The TS backbone designed backward from its live consumer contract: API surface,
per-parse diagnostics scoping (PT-E1), semver, migration-as-artifact. This pass RUNS gates the prior pass
only designed: latex baseline captured GREEN, census re-verified, the dual-instance boundary traced.

**Mechanism — the Y.13 adjudication (three tiers, F5).**
- **Tier A — external floor (un-prunable):** `{Parser, ParserState, regex, string, any}`. Never removed/
  renamed/type-narrowed in a way that breaks the 8 latex import sites. The hard contract.
- **Tier B — the combinator algebra (KEEP; first-party corpus is a LAWFUL Y.13 consumer):**
  `then/or/map/chain/skip/next/opt/many/sepBy/all/dispatch/trim/wrap/peek/lookAhead/not/minus/recover`
  + leaves. parse-that is a *published combinator library*, not an internal helper — its contract is the
  algebra as a standalone npm package; pruning to latex's 5 symbols would gut the product, contradicting
  constitution-i (primitives+combinators ARE the exempt base). **Decisive evidence this is the repo's own
  settled posture:** `proof:no-dead-combinator` (in `proof:all`) already asserts mechanically that every
  combinator is exercised by the 13 `test/*.test.ts` + 4 `*.bench.ts`. Y.13 for Tier B is discharged by
  that gate, disclosed as the first-party contract.
- **Tier C — accreted non-combinator globals (PRUNE):** the module-global diagnostics latch (`utils.ts:6`
  `diagnosticsEnabled`, buffer `:138-146`) is consumer-less AND defective (cross-parse contamination);
  `proof:no-dead-combinator` does not cover it (not a combinator), which is why it accreted. Delete-not-
  flag — the honest prune target; the algebra is NOT pruned.

**Mechanism — PT-E1 diagnostics, DECIDED on the disabled-path axis (research-D §3).** Storage is already
per-parse (`furthest`/`expected`/`getLineAndColumn` on the returned failed state, `parser.ts:56-65`);
only the *gate* is a module-global. Winner = a **primitive boolean field `state.diag`** on `ParserState`,
constructor-initialized (map fixed from birth), set from a parse option, read in `mergeErrorState`.
Disabled-path cost = **one boolean read on the fixed monomorphic map — 0 alloc, 0 new object, 0
transition** — beating a `DiagnosticScope` object (re-introduces per-parse alloc on the OFF path) and a
`ParserContext` flag (wrong lifetime, per-combinator not per-parse). The one obligation: `state.diag` must
copy across every `state.from`/`unsafeSetValue` mint site or a child silently reads `false`.

**Mechanism — the migration crater + type/runtime hazards (research-D §2,4,5).**
- Crater = 1 positive (latex, baseline GREEN 127/127) + 2 negative guards (value.js/keyframes grep-0
  tripwires). Candidate leg (UN-RUN): pack the working tree → `rm -rf node_modules/@mkbabb/parse-that &&
  npm i ../parse-that-<cand>.tgz` (MANDATORY reinstall — omitting it silently re-tests 0.7.1) →
  `vitest run` AND `vue-tsc`. Run 0.7→1.0 ALONE first (attribution isolation).
- **The sole real type-break risk = `document.ts:81`** (`inlineNode: Parser<LatexNode|null> = any(…)`, a
  16-member heterogeneous union). Current `any<T extends Array<Parser<any>>>` degrades to `Parser<any>` on
  inference-fallback (assignable to anything); the tightened `Parser<unknown>` fallback is NOT — so the
  tightening **unmasks** any latent mistype. Decidable ONLY by `vue-tsc`, never `vitest`. Probably safe
  (args are node-typed) but UN-DECIDABLE statically/by unit tests. `nt.*` `Parser<any>` nonterminals stay
  assignable (any→anything), so cross-instance composition does NOT newly redden.
- **The dual-instance hazard is TRACED, not hypothetical:** latex imports `Parser` from parse-that 0.7.1
  (`compile.ts:7`) beside `BBNFToParser` from bbnf-lang bundling nested parse-that **0.6.0** (`:6,18`);
  `nt.*` (0.6.0 objects) flow into latex's grammar composed with 0.7.1 combinators. Bumping latex to 1.1.0
  drives 1.1.0 combinators over 0.6.0-shaped states across the **widest ParserState-shape gap in the
  package's history** (1.0.0 removed `*Span` fields, 1.1.0 adds lazy fields). Resolution = gated dedupe-
  or-document on the crater's runtime leg (green→document in MIGRATION.md; red→`overrides` or coordinated
  bbnf-lang bump).

**Integration seams.** `package.json` `proof:no-dead-combinator`/`proof:all`; `utils.ts:6,33,51` (diag
global → `state.diag`); `state.ts:36-53` (field), `:55-99` (mint sites); `parser.ts:56-65`; `compile.ts:
6,7,15,18` (the crossing); `document.ts:81`/`text.ts:145,154`; `types/ast.ts:103-140` (`LatexNode`).

**Gates.** (1) Candidate crater — build 1.1.0, reinstall, `vitest`+`vue-tsc`, 0.7→1.0 alone first —
**UN-RUN (make-or-break) → Brief 1**. (2) `vue-tsc` at `document.ts:81`. (3) dual-instance runtime leg
(`document/integration/paper.test.ts`). (4) `state.diag` prototype: `%HaveSameMap(on,off)==TRUE` +
nested-parse isolation (RED on the global, GREEN after) + propagation across mint sites. (5) negative-
guard grep tripwires.

**Exact open questions.**
- **8D-Q1 (make-or-break):** the candidate crater is UN-RUN — it requires a built 1.1.0 tarball not yet
  existing. It closes the type axis (`document.ts:81`), the runtime dual-instance axis, AND 8B's native
  budget in one construction. (→ Brief 1.)
- **8D-Q2:** P2 — latex-live-forward is research-D's read but the pin has not moved off 0.7.1; if latex
  never migrates, the crater's positive gate tests a migration no one runs, and the contract rests on Tier
  B first-party corpus alone (a posture the constitution tolerates but does not favor).
- **8D-Q3:** mapState (0 external consumers) — Tier B keeps it IFF `proof:no-dead-combinator` exercises it;
  confirm coverage or it falls to Tier C. (Coupled to 8A-Q2.)

**Standing:** ADVANCED — the strongest family. Y.13 adjudicated mechanically; baseline RAN GREEN;
`state.diag` decided; dual-instance TRACED. The residue is the one UN-RUN make-or-break crater.

---

## INCOMPATIBILITY MAP (pass-3 — one pass-2 collision DISSOLVED, one REFINED, framework-forks resolved)

| pair | pass-2 state | pass-3 disposition | precise reason |
|---|---|---|---|
| **8C-guard ⟂ LOCK-5-fix** | ROUTE to LOCK-5 (guard forecloses fix) | **DISSOLVED** | PROTO-2 (F4): Route A (`pfa.rs`) and Route B (classify-gate) are **complementary, not exclusive**. `pfa.rs` proven correct + cheap (1.00×, Hopcroft-safe), banked validated-shelf; Route B ships (live corpus trips zero dissolvable divergence). No foreclosure — LOCK-5 records a *latent* gap. |
| **8A center-B ⟂ Y.13 ∧ LOCK-4B** | double-block (consumer + 4B mechanism) | **REFINED — still BLOCKED (sub-deliverable)** | Y.13 half RESOLVED by 8D Tier B (first-party bench is the lawful consumer, F5). LOCK-4B half SHARPENED by PROTO-1 (F3): the temporal specialization is Maglev-size-capped on real leaves; closing needs 4B's AOT emitter whose payoff for the unrolled arities is **near-zero on live traffic** (zero live callers) and whose TS face 4B evidence-banked ("no V8 analog, T1"). The **structural** core is fine; the **temporal arity-2/3 specialization** stays BLOCKED. |
| **8A-mapState-front ⟂ 8D-Y13-prune** | ADD (dead code built anyway) | **FRAMEWORK-RESOLVED, disposition-pending** | 8D Tier B keeps combinators via `proof:no-dead-combinator`; Tier C prunes non-combinator globals. mapState survives IFF it is in the gate's coverage (8D-Q3). No longer a live collision — a bounded disposition. |
| **8B route-1 (compaction caveat) ⟂ 8B route-3 (stop compacting)** | — | **ADJUDICATED → route-1** | route-3 is semver-major breaking (wrap/trim rely on the discard); route-1 (document caveat + tightened bound) is TS2589-free and non-breaking. Within-8B, touches 8C (mirror row) + 8D (contract). |
| **8B undefined-compaction ⟂ 8C fixed-arity mirror** | — | **NEW — contingent on Brief 2** | If Rust `seq!`/`impl_parser_flat` yields fixed-arity tuples, TS `fuseAll`'s compaction is a Rust↔TS DRIFT/LAWFUL-NON-MIRROR row the 8C ledger + 6D corpus must carry — route-1's caveat is then a mirror divergence, not a private note. |
| **8B ⟂ 8D** (result-union vs field-triple) | ADJUDICATED (field-triple) | **UNCHANGED** | latex reads neither `.expected` nor a result-union; `state.diag` field-triple-on-returned-state is the contract; result-union is 8B's additive typed wrapper, Y.13-thin. |
| **8A ⟂ 8B** (`as unknown as`) | RE-RESOLVED (ReadonlyState) | **CLEAN non-conflict** | Reinforced: 8B-G-ASUNKNOWN is itself now disarmed as a lawful variance-bridge erasure. No cast conflict anywhere. |
| **8A ⟂ 8C-ambitious** (runtime TS dispatch) | BANKED | **UNCHANGED — BANKED** | −14% VM/runtime-switch KILL is law; only a build-time-generated specialized dispatcher (LOCK-4B) is lawful, never a runtime VM. |

**No family is BLOCKED-candidate** (all four have constructible central mechanisms on current substrate).
The one BLOCKED line is the **sub-deliverable 8A center-B's temporal arity-2/3 specialization** — a
*triple-diminished* block: consumer-justification evaporated (F1), temporal mechanism Maglev-capped (F3),
cross-lock mechanism (LOCK-4B TS fuser) evidence-banked, near-zero live payoff. Its structural leg +
general arm survive; the temporal unroll is a first-party-bench-gated micro-opt at best. Elegance earns it
nothing: the missing primitive (an AOT-fused TS emitter removing `.parser`) is a full LOCK-4B build whose
live payoff is ~0.

---

## CROSS-LOCK ROUTING (facts conveyed; challenges route to GESTALT-P with evidence)

- **→ LOCK-5:** the priority-discard engine defect (`dfa.rs:189-202`; leftmost-longest; lazy→greedy) is
  REAL but **latent-only** for the live corpus (PROTO-2). Record as a latent correctness gap:
  fixed-in-principle by the shelved `pfa.rs` (Route A, proven 1.00×/Hopcroft-safe), guarded-in-practice by
  the classify-gate (Route B, shipped). **Complementary — the guard must NOT foreclose the shelved fix.**
  (Supersedes the pass-2 "8C-guard ⟂ LOCK-5-fix" routing.)
- **→ LOCK-4B:** the arity-N specializer's temporal value is Maglev-size-capped (F3); the only lawful
  escape is 4B's build-time AOT-fused emitter (removing `.parser`), NOT a runtime VM (−14% KILL). 4B has
  evidence-banked its TS face; live payoff for the unrolled arities is near-zero (F1). GESTALT-P carries
  this as a named double-diminished block.
- **→ LOCK-6:** the divergence ledger IS 6A's linter whitelist; the single-spec→two-artifact generator is
  6B's; the parity oracle is 6D's. 8C's ceiling is LOCK-6-bounded (8C-Q3). Tape-crossing home ambiguity
  still deferred to LOCK-6; the 4.78× stays fenced pure-JS-decode.
- **→ owner (XL-G, non-blocking):** P1 (value.js divorce intended vs paused-WIP) — evidence weighs
  divorced/settled (F1) but the branch is active; P2 (latex live-forward) — research-D reads live but the
  pin has not moved. No 8A/8B/8D *native* number banks until both close; self-use + first-party figures
  stand under disclosure.

---

## PLANE-WIDE STATE

- **Two prototypes RAN, both decisive.** PROTO-1 struck 8A's zero-measured temporal claim and REFUTED it
  at the shipping tier (Maglev size-cap). PROTO-2 built AND retired the priority-DFA (latent-only),
  resolving 8C's regex row and dissolving the pass-2 LOCK-5 incompatibility. 8B ran its TS-side compaction
  differential (pass-2). 8D captured the latex baseline GREEN (RAN) but its make-or-break crater is UN-RUN.
- **The three remaining UN-RUN gates all ride the same missing artifact — the built 1.1.0 tightened
  tree:** 8B's emit-identity step-(ii), 8B's native budget, and 8D's candidate crater (type + runtime
  axes). Brief 1 constructs it once and fires all three.
- **The single cheapest cross-family uncertainty** is whether the 8B undefined-compaction is a
  cross-language DRIFT row (Rust fixed-arity vs TS compacted) — it decides 8B's route-1 caveat status and
  8C's ledger completeness. Brief 2.
- **Settled — do not re-litigate:** `state.diag`, packrat homing, the SFI predicate, the −14% VM
  refutation, mapState-zero-external-consumers, `opt`-in-`all` DropUndefined discharge, `Parser<T,E>`
  BANK, `dispatch<const K>` revert, "don't split `parser.ts`."
