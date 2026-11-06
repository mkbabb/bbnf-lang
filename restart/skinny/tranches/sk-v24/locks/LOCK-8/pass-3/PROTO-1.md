# PROTO-1 — LOCK-8 pass-3, family 8A, probe 1: the TWO-LEG trace on REAL parse-that leaves

**Family 8A — the shape-stable core (V8 runtime plane).** The directive's two
physically-distinct wins reported SEPARATELY with tier residency: the STRUCTURAL
leg (allocation + hidden-class, JIT-tier-independent) and the TEMPORAL leg (JIT
inlining, tier-dependent). This probe strikes 8A's least-understood, zero-measured
temporal claim at the shipping Maglev tier on real parse-that leaves.

---

## Question

At Node v26's DEFAULT tier (Maglev on), does an arity-unrolled `all` closure INLINE
each captured leaf `.parser` call (TEMPORAL leg) while the shared general-arm
`parsers[i].parser` stays never-inlined — and is the STRUCTURAL leg (zero map
transition on diagnostics, no new alloc) tier-independent?

## Method (real leaves, shipping Maglev, cold-paired; two legs separated)

- **Scratch** (PROVE-11 / R4 §8): `pt-scratch-8A/` copies parse-that/typescript
  `src` (pinned HEAD **ef10d5b**, 1.0.0), `node_modules` symlinked. **parse-that is
  READ-ONLY — untouched.** The pristine `src` is esbuild-bundled type-strip-only
  (`_core-entry.ts` → `_core.bundle.mjs`; `--target=esnext` keeps native class
  fields; **NO** `--keep-names`/minify so leaf SFI names survive the inlining trace).
- **Corpus = REAL parse-that leaves only** (a homogeneous synthetic probe
  over-reports, R4 §1). Distinct SFIs: `stringParser` (len-1 arm, bytecode 90) +
  `regexParser` (sticky, bytecode 248). Three hot sites:
  - **A** `all(string('('), regex(/\d+/), string(')'))` — arity-3 → `fuseAll`
    UNROLLED (`p0,p1,p2` const-captured, `leaf.ts:214-251`)
  - **B** `all(string('['), regex(/\d+/))` — arity-2 → `fuseAll` UNROLLED
    (`p0,p1`, `leaf.ts:183-212`)
  - **C** `all(string('{'), regex(/\d+/), string(','), string('}'))` — arity-4 →
    `fuseAll` GENERAL ARM (`parsers[i].parser`, `leaf.ts:255-272`)
- **Warm** each fused closure 2,000,000× over its real input, fresh `ParserState`
  per iter (faithful to `.parse()`). Harness `%NeverOptimizeFunction`'d so ONLY the
  fused closure + leaves tier up — the inline trace reflects the FUSED closure's own
  decisions, not the harness inlining the chain into itself. One site per process
  for unambiguous per-site attribution.
- **Instrument.** `node --allow-natives-syntax --expose-gc --max-semi-space-size=1024
  --trace-maglev-inlining --trace-turbo-inlining --trace-opt-status`. Tier residency
  = `%GetOptimizationStatus`; shape stability = `%HaveSameMap`; disabled-path alloc =
  gc() + retained-array `heapUsed` delta (retain defeats escape-analysis elision and
  young-gen scavenge; huge semi-space so no scavenge inside the batch).
- Shipping Maglev, NOT `--no-maglev`. V8 inlining decisions (bytecode sizes,
  thresholds, map identity) are compiler-deterministic — loadavg-independent; loadavg
  recorded per run (7–12 throughout). No throughput ns/op is claimed.

## What ran

One structural run + three isolated temporal runs (A/B/C), full traces captured to
`evidence-structural.out`, `evidence-temporal-{A,B,C}.trace`.

## The numbers

### STRUCTURAL leg (tier-INDEPENDENT) — PASS

| measurement | result |
|---|---|
| `%HaveSameMap(diagOff, diagPopulated)` (expected←array, suggestions/secondarySpans pushed) | **TRUE** |
| `%HaveSameMap(diagOff, realFailedParseState)` (seeded via a real failing `.parse("(x")`) | **TRUE** |
| disabled-path alloc, RETAINED real footprint | **248.1 B/op** (200k ops) |
| disabled-path alloc, OPTIMIZED (escape-analyzed non-escaping `.parse()` state) | **~0 B/op** |

The `ParserState` hidden class is **fixed-from-birth**: `expected?`, `suggestions`,
`secondarySpans` are stamped at construction (class-field declarations create the
own-slot at undefined/`[]`), so seeding a diagnostic later is a pure value write —
**no map transition**, no transition-driven realloc. This is the tier-INDEPENDENT
claim and it holds. (Honesty: at pin ef10d5b `state.ts:44-45` still has eager
`suggestions=[]`/`secondarySpans=[]`; the certified −60.1% B/op lever is NOT applied
at this HEAD, so the 248 B/op still pays two empty diag arrays. Map stays stable
regardless.)

### TEMPORAL leg (tier-DEPENDENT) — tier residency + per-site inline decisions

| site | fused closure tier residency (`%GetOptimizationStatus`) | MAGLEV inlines leaf into fused? | TURBOFAN inlines leaf into fused? |
|---|---|---|---|
| **A** arity-3 unrolled | **MAGLEV** (0x29 = MAGLEV\|maybeDeopted) | `stringParser` bc90: — · **`regexParser` bc248: NO** (`cannot inline: big function, size (248) >= max-size (100)`) | **`stringParser` YES · `regexParser` YES** (`Inlining … into <fused>`) |
| **B** arity-2 unrolled | **MAGLEV** (0x29) | **`stringParser` bc90: YES** (`== Inlining stringParser`) · **`regexParser` bc248: NO** (same cap) | **`stringParser` YES · `regexParser` YES** |
| **C** arity-4 general arm | **MAGLEV** (0x29) | **neither leaf** (no inline attempt — `parsers[i].parser` is a dynamic indexed callee) | **neither leaf** (zero leaf-inline lines) |

- **All fused closures are RESIDENT at MAGLEV** (0x29), never Turbofan, even after
  2e6 iters (`maybeDeopted` set → they touched Turbofan then deopted back; Maglev is
  the stable shipping tier). So the Turbofan column is a **transient/non-resident**
  observation.
- The governing Maglev gate is **`--max-maglev-inlined-bytecode-size=100` (default,
  UNPERTURBED)**: any single leaf with bytecode > 100 is not inlined by Maglev.
  `regexParser` (248) exceeds it; `stringParser` (90) does not. **This is a
  bytecode-SIZE cap, not the megamorphic-IC story.**
- The per-site const-capture mechanism the thesis names IS real, but shows only at
  **Turbofan**: the unrolled arms (A, B) inline BOTH const-captured leaves into the
  fused SFI; the general arm (C) inlines NEITHER at any tier — the `parsers[i].parser`
  dynamic indexed callee is never an inline candidate. Confirmed by the A/B-vs-C
  contrast (site C has zero leaf-inline lines).

## Verdict

**RAN_MEASURED. STRUCTURAL leg PASS; TEMPORAL leg REFUTED at the resident Maglev tier
(mechanism confirmed only at non-resident Turbofan).** Both legs are reported
SEPARATELY with tier residency; the run is on shipping Maglev (not `--no-maglev`) over
REAL parse-that leaves; nothing is collapsed to a single ratio. The STRUCTURAL claim
holds tier-independently: `%HaveSameMap == TRUE` (diagnostics seed no map transition),
disabled-path footprint is stable with no transition-driven alloc. The TEMPORAL thesis
as stated — "arity-2/arity-3 unrolled leaf `.parser` calls report INLINED under
Maglev" — is **FALSE for the real `regexParser` leaf** (bytecode 248 > Maglev's
`max-maglev-inlined-bytecode-size=100`): under Maglev the unrolled arm inlines only the
small `stringParser` (bc 90), not the big `regexParser`. The per-site-IC / const-capture
distinction the thesis relies on is genuine but its full effect (both leaves inlined)
appears only at Turbofan, which is not the resident tier here; the general arm is never
inlined at any tier. Per the pass rule, the temporal conjunct fails → **8A's defensible
collection is the STRUCTURAL leg; the temporal unroll win is Turbofan-gated / Maglev
size-capped for real leaves, not a resident-Maglev inline.** This is the sharpest,
first-measured residual: the arity unroll's temporal value on real parse-that leaves is
throttled at the shipping tier by leaf bytecode size, and closing it requires either
shrinking leaf bytecode below 100 or the AOT-fused emitter (LOCK-4B) that removes the
`.parser` indirection entirely.

Artifacts: `pt-scratch-8A/src/probe/two-leg.mjs` (the probe), `pt-scratch-8A/src/probe/_core-entry.ts`
(bundle barrel), `evidence-structural.out`, `evidence-temporal-{A,B,C}.trace`, `PROTO-1.patch`.
