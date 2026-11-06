# Dig Shard 01 — Tranches AA, AB, AC: the Founding Tape Era

The founding trilogy of the original bbnf-lang campaign. Three tranche letters,
authored 2026-04-10 through 2026-04-13, that together state the project's
central architectural bet — abrogate the eager typed AST, emit a packed
columnar tape, expose the typed surface as lazy views — and then fail to land
it inside their own boundaries. This shard establishes the original intent
baseline that every later era diverged from, and reconstructs, with git
evidence, exactly where and how the divergence began.

Sources: `docs/tranches/AA/AA.md` (1541 lines), `docs/tranches/AB/AB.md` (749),
`docs/tranches/AC/AC.md` (660), the three AA prototype research docs
(`docs/tranches/AA/research/prototype{,-2,-3}.md`), and the reconstructed
post-mortem in `docs/tranches/meta-audit/archaeology/era-IV-tape-first.md` and
`docs/tranches/meta-audit/06-commit-archaeology.md`. Git history cross-checked
throughout.

---

## 1. The original intent baseline (the north star everything diverged from)

The founding thesis is stated cleanest in `docs/tranches/AA/research/prototype-2.md:5-70`.
Post-Z measured baseline on M3 ARM (`prototype-2.md:14-17`):

- `json_canada` 1231 MB/s, `json_citm` 1896 MB/s, `json_twitter` 1517 MB/s,
  `css_tailwind` 256 MB/s.
- sonic-rs is 3–6 GB/s; lightning-css is 3–5× faster on CSS. "We are at 25–50%
  of sonic-rs throughput on JSON, ~20% on CSS."

The user's founding directive (`prototype-2.md:60-64`): *"We should have NO
combinators. If this needs an architecture change to reach the sonic-rs
upperbound, what is it? What would it take? Do not be a defeatist."*

A six-agent audit converged on **four hard truths** (`prototype-2.md:23-58`) —
these are the durable, load-bearing findings of the entire pre-restart campaign:

1. **The optimizer substrate is dormant.** The `egraph` crate is
   architecturally complete but every load-bearing parameter is inert:
   `NoAnalysis` is the only `Analysis` impl workspace-wide; `Rewrite::should_apply`
   does not exist (rules fire unconditionally); extraction is greedy bottom-up
   (non-optimal under e-class sharing); `Cost = f64` (no multi-objective); the
   strategy CSP is per-rule with the Y.5 cross-rule `UnionFind` substrate having
   zero consumers. "We have egg/egglog's machinery and use none of it."
2. **The recurring failure mode is misframed audits.** Tranche Z corrected
   three of its own phases mid-flight after re-grepping source. Ghost substrate
   (`CallStrategy::InlineFusion`, `WrapMode::DelimScan`, cost knobs) drifted from
   prior tranches. "Half of every recent tranche has been deleting prior
   tranches' speculative substrate."
3. **Explicit SIMD on M-series ARM is a dead end for short-run scan helpers.**
   Three Z.1 SIMD prototypes regressed against the LLVM-autovectorized byte-class
   LUT. "This is durable. Drop the theme. SIMD only pays where the workload is
   long-run bulk (≥64 bytes/call) — which is exactly the structural-bitmap
   pre-scan, not per-token scanners."
4. **The architectural ceiling is the typed AST construction itself.** Combinators
   are already gone from the hot path; what remains is materialising typed enum
   variants into a slab, one alloc per heterogeneous-Alt element. "Every parse
   run constructs a complete typed tree before any consumer sees it. The tree IS
   the parser output; that contract is what's expensive."

The answer named at the founding: the **tape transposition** — emit a packed
binary tape during parsing, expose the typed AST as a generated lazy `impl` over
the tape, preserving the user-facing API modulo a `'tape` lifetime
(`prototype-2.md:66-70`). This single idea is the seed of AA, AB, AC and, per
the post-mortem, of the next twenty tranche letters.

**The csp-solver vendoring** — the charter's named founding event — is real and
landed: commit `a4d2989c8` "feat(ir,egraph): Tranche AA.2 — vendor csp-solver +
activate GrammarAnalysis" deleted the `.cargo/config.toml` path patch and
brought `crates/csp-solver/` in as a workspace member (planned at `AA.md:471-514`).

---

## 2. What each tranche attempted (charter + waves)

### Tranche AA — "Substrate Awakening, Parse Cliffs, Tape Transposition"
`AA.md` synthesises three competing prototype plans (a conservative
substrate-blind AA-prototype, a bet-the-tranche five-act AA-prototype-2, and a
tape-refusing seventeen-phase AC-prototype) into **20 phases across 5 acts**
(`AA.md:380-1307`):

- **Act 0** — observability prelude (fresh profiles, per-rule fire counts).
- **Act I (AA.1–AA.6)** — substrate awakening: `TypeDescId` interning, activate
  `Analysis<N>` on both tiers, unified `CostConfig` + `TopoExtractor`, delete the
  imperative FIRST/FOLLOW passes, cross-rule CSP + extraction→soft-constraint
  bridge, analysis-gated rewrites.
- **Act II (AA.7–AA.12)** — independent parse wins: `TaggedUnion` (the BoxedEnum
  killer), IIFE→labeled blocks, direct-to-slab scratch, `ClassMask` SIMD,
  structural bitmap pre-scan, perfect-hash dispatch.
- **Act III (AA.13–AA.16)** — the tape transposition: `bbnf-tape` crate, tape
  emitter with a hard parity gate, view generator + consumer migration, and
  **deletion of the eager emitter (~5000 LOC)**.
- **Act IV (AA.17–AA.19)** — cleanup, LSP incremental cache, post-AA baseline.

AA's binding rhetoric was **"Tape or bust. No fallback"** (`AA.md:975-998`):
"There is no 'land Acts I+II and defer Act III to AC' escape — Act III landing
is a hard gate for this tranche's close… The tranche stays open until Act III
lands. The commitment removes the psychological escape valve that prior tranches
used when substrate work hit friction" (`AA.md:1528-1537`).

### Tranche AB — "Tape as the Only Runtime Substrate"
`AB.md` narrows AA's five-class ambition to a single ABI: **every emitted rule
returns `Option<TapeOffset>`** (`AB.md:29-47`). It introduces the
`MaterializationClass` lattice (`MustTape` / `TapeSpanOnly` / `TransparentElide`,
`AB.md:88-121`), a joint CSP solve over strategy + materialization
(`AB.md:141-191`), an owning `Parsed<View>` result type, and a six-phase plan
(AB.0 lattice → AB.1 CSP → AB.2 emitter → AB.3 views → AB.4 elision → AB.5
cleanup). AB explicitly **drops** `DirectProjection` (scalar/aggregate parser
returns) and `TapeCompact` as "projection-style optimizations that break the
one-ABI commitment" (`AB.md:117-121`) — the projection story is deferred to a
post-AB tranche.

### Tranche AC — "Full Tape Transposition"
`AC.md` is the atomic close: replace the entire Rust emission pipeline in **one
commit** — one ABI, one control path, no feature flag, no parallel path
(`AC.md:13-27`). AC treats the eager emitter as a REFERENCE to be deleted in the
same commit that lands the replacement. The plan argues at length (`AC.md:582-627`)
that decomposition is impossible: per-file commits don't compile (the
`Option<Enum>` return contract binds all emitters), a feature flag *is* the
legacy code the directive forbids, and a stale `generated.rs` breaks master.
Phases: AC.1 prep → AC.2 atomic transposition → AC.3 consumer migration → AC.4
elision → AC.5 post-AC baseline + floor gates.

---

## 3. What actually shipped vs what died on the vine (git cross-check)

The tranche docs were relocated into per-tranche directories by a single later
commit (`36945f60c`), so the docs' own git history is thin; the implementation
history is legible through the `Tranche XX.N` commit-message convention.

### Tranche AA — substrate landed, the tape bet did not
| Phase | Planned | Actual commit | Reality |
|---|---|---|---|
| AA.0 | observability | `63b1291c5` | **Shipped** |
| AA.1 | TypeDescId interner | `c209c380e` | **Shipped** (durable) |
| AA.2 | vendor csp-solver + Analysis | `a4d2989c8` | **Shipped** (durable) |
| AA.3 | CostConfig + TopoExtractor + Pareto | `ce77a8354` "Lattice trait substrate" | **Partial** — only the `Lattice` trait; no TopoExtractor |
| AA.4 | pipeline reorder + **delete** imperative passes | `1358e48d2` "GrammarAnalysis/imperative pass **coexistence**" | **Diverged** — passes kept, not deleted |
| AA.5 | cross-rule CSP + extraction bridge + dispatch sharing | `1511d72d8` "`Rewrite::should_apply` predicate guard" | **Partial** — only the predicate; the cross-rule work did not land |
| AA.6 | analysis-gated rewrites | `471153667` "deferred to follow-up tranche" | **Deferred** |
| AA.7 | TaggedUnion (BoxedEnum killer) | `aa60465b7` "deferred behind the tape" | **Deferred** |
| AA.8 | IIFE → labeled blocks | `397f5a90a` (7+ emitter files) | **Shipped** — a genuine independent codegen win |
| AA.9 | direct-to-slab scratch | `59e921afa` "subsumed by the tape" | **Deferred** |
| AA.10 | ClassMask SIMD / ScalarLut delete | `3dba58499` (parse-that sibling) | Partial/sibling |
| AA.11 | structural bitmap | `7cd31f172` "folded into the tape format" | **Deferred** |
| AA.12 | perfect-hash dispatch | `c50e880ed` "deferred behind the tape" | **Deferred** |
| AA.13 | `bbnf-tape` leaf crate | `63220b276` | **Shipped** (durable) |
| AA.14–16 | tape emitter, views, delete eager AST | — | **Never landed in AA** |

Four commits in AA carry the literal genre "deferred behind the tape / subsumed
by the tape / folded into the tape" (`git log | grep -c` = 4). **The tape-or-bust
Act III — the entire load-bearing point of the tranche — never landed inside
AA.** The commitment that swore off "the psychological escape valve" used exactly
that escape valve, five phases at a time, within its own boundary.

### Tranche AB — substrate again, emitter deferred again
Shipped: AB.0 `MaterializationClass` lattice (`85be3f0c2`), AB.1 CSP joint solve
(`a3b9d21e4`), AB.2a "tape-first emitter **substrate**" (`094ce8e8d`). Then
`511c6b78f` "docs(tranche): split AB.2 into substrate (shipped) + emitter rewrite
(pending)". **AB.2b (the actual per-kind emitter rewrite), AB.3, AB.4, AB.5 never
landed.** AB shipped analysis lattices and CSP variables and stopped precisely at
the boundary where a consumer would prove them — the identical shape as AA.

### Tranche AC — the "atomic" commit fractured; the closeout never happened
Shipped: AC.1 (`3e00fdf02`), then AC.2 as **ten non-atomic commits**
(`7bde7ff00`, `b93cdf984`, `031cff591`, `3064b225c`, `dea5a6056`, `caaa53154`
"pt1", `4166ad92b` "pt2", `d7b9d3bdc`, `a49810ed7`, culminating in `c6f8106b9`
"AC.2 integration — tape-first bbnf crate **compiles**"). The document's central
thesis — that this MUST be one atomic commit — was unexecutable and abandoned on
first contact; the work split into pt1/pt2 plus a separate "it compiles"
integration commit. **AC.3 (consumer migration) exists only as a doc commit
(`27729d114`); AC.4 (elision) and AC.5 (post-AC baseline + floor gates) never
shipped as implementation.** There is no `docs/benchmarks/post-AB.json` and no
`docs/benchmarks/post-AC.json` on disk — the measurement closeout that would have
verified the floor gates never occurred. Tranche letters jump AC → AE with **no
AD directory**: the numbering discontinuity marks where the founding trilogy
simply ran out and the sprawl continued under fresh letters.

The post-mortem grades AB and AC directly (`era-IV-tape-first.md:41-42`): AB
"**Partial** — thesis declared, execution spans AC–AE"; AC "**Partial** — some
paths still carry `Value` wrappers."

---

## 4. Root causes of divergence (with file:line evidence)

**RC1 — Substrate-first / consumer-later is the founding anti-pattern.** Every
tranche in the trilogy ships the substrate (interners, analysis lattices, CSP
variables, the tape crate) and defers the consumer that would prove it. AA
deferred Act III; AB deferred AB.2b; AC never finished AC.3–5. The post-mortem
names this exactly at the Era IV→V boundary (`era-IV-tape-first.md:168-171`):
"the substrate-first / consumer-later anti-pattern that defines Era V's pain
first takes root." This is hard-truth #1 (dormant substrate) recurring as
*process*: the campaign's diagnosis of its own codebase became its own working
method. Ghost substrate is manufactured, not merely inherited.

**RC2 — Rhetorical hard-gates do not bind.** AA's "Tape or bust. No fallback.
The tranche stays open until Act III lands" (`AA.md:1528-1537`) was the strongest
anti-deferral language in the corpus and was violated within the same tranche.
Grandiosity of commitment scaled *inversely* with delivery: the more a plan swore
it would not defer, the more the commit log shows deferral. The commitment
functioned as motivational framing, not an executable gate.

**RC3 — The atomicity doctrine was a structural bottleneck disguised as
discipline.** AC's chicken-and-egg (`AC.md:361-366`): `generated.rs` is a source
file of `bbnf`, and `bbnf-bootstrap` (which produces `generated.rs` via `cargo
expand`) depends on `bbnf`. Regenerating requires `bbnf` to compile, which
requires a valid `generated.rs`. The plan resolved this by "temporarily comment
out `pub mod generated;`… delete both files" (`AC.md:410`) — a self-inflicted
broken-master window that the atomic doctrine existed to prevent. The doctrine
that "the only idiomatic option is the atomic rewrite" (`AC.md:606`) was
un-shippable; reality shipped ten commits.

**RC4 — Audit-underestimate scope explosion (hard-truth #2 recurring).** AC's
"load-bearing coupling surprise" (`AC.md:323-357`): the early audit scoped the
bootstrap CST consumer at ~8 files; a deeper audit (worktree-agent-a985875e)
found **216 `BbnfBootstrapEnum` references across 12 files** — the bootstrap
consumer is not `host.rs`, it is the *entire lowering pipeline*
(`value_expr.rs` 67 refs, `expression.rs` 60, `compile.rs` 20…), ~2500 LOC of
pattern-matching migrating alongside the emitter rewrite. The misframed-audit
failure mode the founding truths named as the enemy is the exact mechanism that
blew AC's scope.

**RC5 — Orthogonal decision surfaces, built and then deleted (the
MaterializationClass = EmissionTier repetition).** AB's central design is a
three-class lattice `MustTape / TapeSpanOnly / TransparentElide` on a NodeId
sidecar with its own CSP variable class (`AB.md:88-191`). The post-mortem
records that the *identical-shaped* `EmissionTier` axis (`MustTape / MustFn /
MayInline`) built across AF–AK was **deleted at AQ.5** (`2f7c1bd4`,
`era-IV-tape-first.md:60-76`), folding a separate tier-selection surface into the
pre-existing `PayloadKind → TypeDesc` projection: "the CSP-optimizer no longer
selects a tier separately from projecting a type; one decision surface for one
semantic." Roughly two weeks of tier substrate was subsumed. The founding
trilogy invented a materialization-class decision surface orthogonal to type
projection; the campaign spent a fortnight rebuilding an isomorph of it before
deleting it. This is the direct ancestry of the `no-orthogonal-codepaths` /
`no-workarounds-arch` feedback memory.

**RC6 — JSON/CSS-only gate design.** Every floor gate in all three tranches is a
JSON or CSS GB/s number (`AA.md:362-371`, `AB.md:511-521`, `AC.md:489-497`). The
generalization mandate is asserted in prose but *measured* on two grammars only.
The gates could be "met" while the generalized generator regressed on any other
grammar — the SK-V21 charter's explicit ban on JSON/CSS over-emphasis is a direct
correction of this founding gate design.

**RC7 — Target ratchet-down under a missed floor.** AA floor `json_canada` ≥ 2.0
GB/s, target ≥ 3.0 (`AA.md:364`). AB relaxed the floor to ≥ 1.5 (`AB.md:514`); AC
kept ≥ 1.5 (`AC.md:491`), rationalised as "tape-only commitment — projection is
deferred." The founding-era close (AU, `era-IV-tape-first.md:150`) measured
`json_canada` **1231 MB/s** — *below the pre-tranche post-Z baseline of 1231
MB/s*, i.e., the entire tape era netted zero on its flagship bench while ratcheting
its published targets down tranche over tranche. AU met 10/24 hard gates
(`era-IV-tape-first.md:58`). This is the bench-honesty pathology in its purest
form: the number stayed flat, the goalposts moved, and the prose stayed
triumphant.

**RC8 — No PROGRESS/FINAL discipline in the founding tranches.** The Plan /
PROGRESS / FINAL doc convention only arrived at AS/AU
(`era-IV-tape-first.md:95-97`, commit `536ac07c`; first `FINAL.md` at `5281ec23`).
AA, AB, AC are each a single monolithic plan file with no wave-level progress
ledger — which is *why* the deferrals were invisible until the meta-audit
reconstructed them from the commit log. The absence of a landing ledger let the
substrate-only closes pass as complete.

---

## 5. Worth resurrecting for the totality plan (do not re-learn)

**Durable positive results — the founding bet's substrate was RIGHT.** The tape
architecture itself landed and survived every later era
(`era-IV-tape-first.md:86-99`): the columnar `TapeRec` + payload arenas +
`FusedBuilder` API in `crates/tape/`, the `TypeDescInterner` hash-cons (stable
type identity across the compiler), and the `has_payload` / `has_scalar_payload_type`
gate vocabulary that every later gate speaks. What failed the founding trilogy was
convergence discipline, not the architecture. The restart should inherit the tape
thesis with confidence and treat the failure as procedural.

**The grammar-authoritative single-decision-surface thesis**
(`era-IV-tape-first.md:15-21`): "`TypeDesc` inference decides what the runtime
stores; the tape stores it; consumers project typed accessors out of it. A single
decision surface, one codegen path." This is the convergent architecture the
whole campaign spent 20 tranches rediscovering by deletion. It should be a
*premise* of the totality plan, not a destination.

**The four hard truths** (`prototype-2.md:23-58`) are durable and should not be
re-derived — especially:
- #3: explicit SIMD on M-series ARM is net-negative for short-run (<64-byte)
  scanners; it only pays for long-run bulk operations (structural-bitmap
  pre-scan). Three Z.1 prototypes proved this; do not re-run them.
- #4: the typed-AST-materialization contract is the real perf ceiling — the tape
  exists to break exactly this contract.

**The AQ.5 unification lesson** (`era-IV-tape-first.md:60-76`): never two decision
surfaces for one semantic. Any totality design that separates a "materialization
tier" / "emission tier" from type projection is rebuilding the surface that was
deleted twice. Fold materialization into `TypeDesc` projection from the outset.

**The AA.8 IIFE→labeled-blocks win** (`397f5a90a`) is a real, landed, independent
codegen improvement (early-exit `?`/`return` scaffolding as stable labeled blocks
rather than closures LLVM inconsistently inlines). It composes with any backend
and is orthogonal to the tape bet — carry it forward.

**The parity-gate test shape** (`AA.md:253-257`, `AC.md:549-554`): compile every
production grammar in both modes, parse ≥20 corpus inputs per grammar, assert
accessor-by-accessor equality against a golden structural snapshot. Because
accessors are generated from the same IR in both modes, the equality check is
mechanical. This is sound correctness scaffolding independent of whether the two
modes are eager/tape.

**Concrete capacity negative result** (`era-IV-tape-first.md:111-113`):
`payload_idx: u16` overflowed at canada.json's 111K f64 payloads; the fix folded
the tag into `child_off` (u32 range) with a sentinel. Any tape record format must
size payload indices to u32 from the start.

**post-AU.json as the measurement floor** (`era-IV-tape-first.md:126`) — the
single canonical numeric anchor every later era quotes; the totality plan's
benchmarks should reconcile against it rather than inventing a new floor.

---

## 6. Meta-process pathologies

- **Plan-to-ship ratio is the signature failure.** AA is a 1541-line, 20-phase
  plan; roughly nine phases landed, five were explicitly deferred, and the
  make-or-break Act III never happened in-tranche. The plan document was an
  aspiration essay, not an execution contract.
- **"Deferred behind the tape" as a commit genre.** Deferral dressed as
  progress — four such commits in AA alone — let a substrate-only close read as a
  completed tranche. A commit that ships nothing but a chore-tag renaming a
  deferral is negative work masquerading as forward motion.
- **Rhetoric inflated as delivery thinned** (RC2). The most emphatic
  no-fallback language in the corpus sits atop the most-deferred tranche.
- **The atomicity myth** (RC3) — a doctrine argued for a full page (`AC.md:582-627`)
  that could not be executed once and was silently replaced by a ten-commit
  reality including a self-inflicted broken-master step.
- **Success and failure locked in together.** The post-mortem's sharpest line
  (`era-IV-tape-first.md:168-171`): the Era IV→V boundary "is the place where
  tranche discipline's success (plan doc, waves, PROGRESS, FINAL) locks in, and
  simultaneously where the substrate-first / consumer-later anti-pattern that
  defines Era V's pain first takes root." The founding era institutionalised both
  the good process (doc convention) and the fatal method (substrate without
  consumer) in the same six days.

---

## 7. The one-paragraph verdict

The founding trilogy identified the correct architecture (the columnar tape, the
grammar-authoritative single decision surface) and the correct diagnosis (dormant
substrate, typed-AST-materialization ceiling, SIMD only-for-bulk) — and then
failed to converge on any of it inside three tranches, deferring the load-bearing
consumer at every boundary while shipping ever-more substrate. The central bet did
not land in AA (Act III deferred), nor AB (emitter deferred), nor AC (closeout
never written, no post-AC baseline); it sprawled across twenty tranche letters
(AA–AU) and closed at a `json_canada` number equal to its own pre-tranche
baseline, having ratcheted its published targets downward the whole way. The
architecture was right; the discipline was substrate-first/consumer-later; the
gates were JSON/CSS-only and moved; and an orthogonal materialization-tier
decision surface was invented here that the campaign later spent a fortnight
deleting. For the totality plan: inherit the tape and the single-decision-surface
thesis as premises, forbid substrate that lacks a same-tranche consumer, size the
tape format for u32 payloads, reject any tier axis separate from type projection,
and measure generalization on more than two grammars.
