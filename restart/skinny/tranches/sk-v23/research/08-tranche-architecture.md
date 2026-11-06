# SK-V23 research/08 — The dual-repo tranche architecture (the named-tranche skeleton)

This is a SKELETON. It reshapes SK-V22's `GW0-GW6 + PT-R.1-6 + PT-T.1-5 + the archive sequence
§2.1` and folds SK-V23 PASS-ALPHA's three binding axes (x86 abrogation both repos; SIMD
modernization both repos; the greenfield-vs-carry delineation) into NAMED TRANCHES — six for
bbnf-lang, five for parse-that — each carrying a charter sentence, entry dependencies, a
close-gate CLASS, its cross-repo edges, and its H1/research-04 baseline binding. Final wording is
the synthesis's to own; the DEPENDENCIES and the interlock graph are the load-bearing content.

Evidence base ratified below is grounded in the live trees as of 2026-07-09:
`bbnf-lang/crates/` (the legacy tree, archiving) carries **502 x86 hits** across
`simd-scan/{avx2.rs,avx512.rs,compaction.rs,parity.rs}` + `core/src/generate/regex/emit/simd.rs`
+ the eight `core/src/grammar/generated/*.rs` stamps; skinny's live façade
`skinny/crates/bbnf-simd/{aarch64/,scalar/,classifier.rs,dispatch.rs}` carries **199 NEON/vshrn
hits**; skinny still ships the empty `crates/simd-scan` stub AND both `crates/bbnf-regex` and
`crates/parse-that-regex` fork crates; parse-that carries **zero literal x86 intrinsics** — its
x86 exposure is the `portable_simd` nightly path (`rust/parse_that/src/lib.rs:1-3`, three
`#![feature]` gates) whose scan kernels (`scan/{quote_parity.rs,number_simd.rs,quoted_simd.rs,
decode.rs,number.rs}`) auto-vectorize per-target. This reconciles PASS-ALPHA axis 1: parse-that's
x86 abrogation IS the de-nightly `std::simd → core::arch::aarch64` transposition, not an intrinsic
deletion.

---

## §A — The three PASS-ALPHA axes, homed to tranches (binding read)

Before the tranche list, the three new axes are pinned to their owning tranches so no axis floats:

1. **x86 abrogation — BOTH repos, with anti-regrowth enforcement.**
   - bbnf legacy 502 hits: DIE WITH THE ARCHIVE (they live only in `crates/`, which `git rm`s at
     TB1). No transposition — deletion by archival. The residual skinny `simd-scan` stub deletes
     at TB2 (=GW1, per SK-V22 §1.2 TW1c row).
   - The ENFORCEMENT design (charter: "cfg-deny / CI guard / workspace lint so x86 cannot
     regrow") is a NEW deliverable with a named home per repo: bbnf plants it in **TB1** (a
     workspace lint / Lock-class falsifier denying `target_arch = "x86_64"` and `avx*`/`_mm*`
     symbols, which must FIRE on a planted x86 intrinsic before reading green — the Lock-14
     planted-phantom discipline, SK-V22 §1.5); parse-that plants it in **TP1** (deny
     `portable_simd` + x86 intrinsic paths, guard fires on planted regrowth).
2. **Deep idiomatic modern SIMD within the ASM-hybrid union — BOTH repos.** The dav1d discipline
   (intrinsics default, per-kernel asm gate, checkasm-liveness) governs both; `vshrn` obviates
   hand-asm (the lone Lock-16 residual, SK-V21 SPEC:44,319-321). SOTA NEON leverage — the vshrn
   idiom family, multi-block pipelining, table-lookup classification, shufti/truffle/Teddy-class
   prefilters, vectorized DFA stages — homes to **TB5** for bbnf (the live `bbnf-simd` kernel
   plane) and **TP2** for parse-that (the regex engine + `scan/*_simd.rs`).
3. **Greenfield-vs-carry delineation — a first-class per-subsystem table (§C).** The four named
   greenfield subsystems (WART-C name-gate → TB3; NO type system → TB4; prototype Value API →
   TB4; dead/partial SIMD kernels → TB5) versus the CARRY set (un-forked dispatch, AoS one-tape,
   fused sink, grammar-derived projections, the live NEON kernels, the recognizer tier, passes
   IR). The delineation itself is a TB0-grade deliverable authored alongside the synthesis.

---

## §B — bbnf-lang: six named tranches

Intra-repo order: **TB1 → TB2 → TB3 → {TB4 ∥ TB5} → TB6**. Every close-gate binds to the
research/04 load-accounted baseline as the standing ANCHOR (SK-V22 §5); each tranche's proof is
its own paired same-session before/after capture against the prior tranche's end-state.

### TB1 — THE GREAT ARCHIVAL + X86 PURGE (absorbs GW0 + archive sequence §2.1 + axis-1 enforcement)
- **Charter sentence.** Cut the umbrella tag set, port-in the `.bbnf` fleet + egraph/egraph-derive
  + the csp-solver vendor mirror, `git rm` the legacy tree (whose 502 x86 hits die with it),
  re-found root on skinny's workspace, land the six durable docs, and plant the x86 anti-regrowth
  guard — all as one cutover that must not move a number.
- **Entry deps.** None — campaign root. Internally: tags-before-removals is normative
  (SK-V22 §2.1); GW0.b port-in BLOCKS the GW0.c `crates/` removal.
- **Close-gate CLASS.** Cutover / pure-move parity (a move must not move a number, SK-V22 §1.4.1)
  + tag-set resolves (`git show <tag>:path` sampled per surface) + Lock-14 re-keyed and FIRES on
  a planted phantom + GW0.e delivered (six docs, every EVIDENCE-INDEX coordinate a full
  tag-relative path) + GW0.f delivered (four dated seam records) + the NEW x86-guard FIRES on a
  planted x86 intrinsic.
- **Cross-repo edges.** THE ORDERING LAW (§D edge 1): GW0.b → PT-R.5 → GW0.c. GW0.b's close
  explicitly verifies BOTH repo repoints before any removal. Outbound: GW0.f commits SEAM-EGRAPH,
  SEAM-NOGO, SEAM-PT-E1, SEAM-BUDDY.
- **H1 / baseline.** research/04 is ESTABLISHED here as the campaign anchor. PRECONDITION on
  GW0.e: the H1 paired twitter sonic/parse_tape reconciliation capture adjudicates 2.10× vs 1.59×
  before PERFORMANCE-THESIS quotes any twitter figure, carrying the +36% sonic-side baseline-drift
  disclosure (SK-V22 §4). 33:59 never cited for a 2.10× claim.

### TB2 — STACK / TOOLCHAIN + PORT-IN CLOSURE (= GW1; port-in half hardened)
- **Charter sentence.** Bring the graduated workspace to pinned-stable edition-2024, relocate the
  75/34 inline-test blocks, delete the `simd-scan` stub, falsify Lock-1 two ways on the graduated
  tape, and HARDEN the port-in (crates-infra vendor byte-identity discipline + sync scripts +
  csc411 checkpoint) so the vendored engine/solver are stable-buildable and de-nightly-ready.
- **Entry deps.** TB1 (the tree is re-founded and self-contained). DESIGN NOTE on the prompt's
  "(ii) stack/toolchain + port-in" grouping: the physical git-move of egraph/csp-solver EXECUTES
  as GW0.b inside TB1's window (the ordering law forces it before GW0.c); TB2 owns the port-in
  CLOSURE — vendor byte-identity CI, sync-script enforcement, the stable-toolchain readiness the
  cross-repo engine build (TB3.GW2b) will depend on.
- **Close-gate CLASS.** Hygiene/stack build+bench parity — whole-workspace pinned-stable
  edition-2024 build; zero `#[cfg(test)]` under src/; grep-zero retired names; `simd-scan` gone;
  paired old-pin/new-pin cold capture ledgered; no regression vs 04.
- **Cross-repo edges.** None direct. Furnishes the stable channel that TB3.GW2b's cross-repo
  engine build asserts against.
- **H1 / baseline.** GW1's measured channel ratio RE-ANCHORS the floor for TB3+ (04-adjusted, the
  adjustment ledgered — SK-V22 §1.1). Delivers the outstanding DEFERRED-LEDGER channel-ratio row.

### TB3 — THE GREENFIELD GENERATOR + WALLS + ONE-REGEX-HOME (= GW2a + GW2b; axis-3 WART-C greenfield)
- **Charter sentence.** Dissolve the TOML/WART-C walls into ONE greenfield emit path served by
  one metadata-driven regen host, and fold the regex engine to one home — the graduated tree
  consuming a byte-identical `crates-infra/bbnf-regex` vendor mirror while both fork crates delete.
- **Entry deps.** TB2 (final stable channel — the first wave that legitimately regenerates the hot
  path must run on the final toolchain so its paired capture has ONE variable, SK-V22 §1.4.2).
- **Close-gate CLASS.** GW2a: in-tree substrate consumer-invariant — ONE emit path serves every
  in-tree grammar (`json_templates/` + `json_sink_direct/` retired, `runtime_generator.rs:100-140`
  fork gone), `regen --check` byte-equality + `generated_loc_budget` + N25 idempotency, closes on
  in-tree evidence ALONE. GW2b: cross-repo paired byte-identity — vendor `diff -r` clean at the
  pinned upstream commit, grep-zero fork + local recognizer crate deleted, `cargo metadata` zero
  path escapes.
- **Cross-repo edges.** GW2b ↔ PT-R.4 (§D edge 2, paired: the fold-back's two halves land against
  each other's parity suites). GW2b asserts "the canonical engine builds on the graduated stable
  toolchain against `crates-infra/egraph`" (§D edge 5 — gate-verified, not assumed).
- **Greenfield (axis 3).** The WART-C two-site name-gate hardcoding residue gets GREENFIELD
  dissolution here — no patch of the fork, a clean single emit path. The regenerated
  `generated.rs` stamps emit aarch64-only (no x86-stamp regrowth; the guard from TB1 covers it).
- **H1 / baseline.** Paired per-corpus cold capture, no regression vs the TB2-re-anchored floor.

### TB4 — THE VALUE PLANE + TYPE PLANE GREENFIELDS (= GW4[TW3 half] + GW5; axis-3 Value+Type greenfield)
- **Charter sentence.** Greenfield the Value API — a unified lazy-cursor path/lane surface
  replacing the prototype-grade sink (the bbnf-path cursor alphabet REBUILDS natively, not ported,
  SK-V22 §1.3) — and greenfield the type plane: a proper type system with `@host` over
  skinny's grammar/ir, replacing the monotone-fold-only status quo.
- **Entry deps.** TB3 (one emit path — the value plane emits through it; the type inference
  completes before emission).
- **Close-gate CLASS.** Substrate runtime consumer-invariant (liveness AND measured win) for the
  value plane; type-resolution parity (B3 width patch re-land) for the type plane.
- **Cross-repo edges.** SEAM-PT-E1 inbound (§D edge 4): when the greenfield Value API grows an
  error surface, it COPIES PT-T.2's scoped-return `Diagnostic` shape (SK-V22 §3.1).
- **Greenfield (axis 3).** Two of the four named greenfield subsystems land here: the Value API
  ("prototype-grade sink, no unified path/lane surface") and the type system ("NO type system,
  monotone fold only"). CARRY: skinny's passes+ir analysis, the AoS one-tape, the fused sink.
- **H1 / baseline.** GW4's four NAMED gates ride into TB5's decision-engine half (below); the
  value-plane half binds its cold capture to the TB3 end-state. DESIGN NOTE: GW4 is SPLIT across
  the data-plane axis (TW3 value plane → TB4) and the performance axis (TW4.2 decision/kernel
  plane → TB5), matching PASS-ALPHA's tranche candidates (iv) vs (v).

### TB5 — THE KERNEL / SIMD PLANE + DECISION ENGINE (= GW3 + GW4[TW4.2 half]; axis-2 SIMD home)
- **Charter sentence.** Land Eisel-Lemire float decode + the KernelCostTable to close the
  +1.48 cyc/B canada tax, right-size the decision engine over `passes`, AND execute the deep NEON
  modernization — the vshrn idiom family, multi-block pipelining, table-lookup classification,
  shufti/truffle/Teddy-class prefilters — into `bbnf-simd`'s live kernel plane, greenfielding the
  dead/partial kernels.
- **Entry deps.** TB3 (one-regex-home — the `eisel_lemire` module's canonical home is the vendored
  engine post-GW2b; GW3 owns the fix wherever that module lives). TB4 optional-parallel (the value
  plane furnishes the `materialize_f64` path the float tax localizes to, 04:149-159). TB4 ∥ TB5.
- **Close-gate CLASS.** Substrate runtime consumer-invariant (liveness AND measured win): the GW3
  HEADLINE — canada eager cold BEATS sonic (the +1.48 cyc/B float tax closed, 04:222-226); plus
  the four GW4/TW4.2 NAMED gates (N22 `node_budget` guard before any escalation; N23 Y.5
  connected-components CSP; the precept-21 `perf_cost` production fix at `passes/src/lib.rs:590`;
  the §5 profile-guided-specialization re-examination trigger). SIMD kernels close on
  checkasm-liveness + per-kernel measured win (the dav1d gate).
- **Cross-repo edges.** GW3 ↔ PT companion commit (§D edge 3): when the float-tax fix touches the
  canonical `eisel_lemire` module, it lands upstream as a paired PT commit and the vendor re-syncs
  in the SAME wave; the gate is always measured bbnf-side. Shares the {ws,quoted,number} family
  scanners with PT-R.4 (§D edge 2).
- **Greenfield (axis 3).** The fourth named greenfield subsystem: "no proper SIMD support
  (dead/partial kernels)" → greenfield NEON. CARRY: the 199-hit live `bbnf-simd/aarch64/` kernels
  and the N*=3 crossover adjudication (SK-V21 §5.1); `vshrn` obviates hand-asm (do not re-attempt).
- **H1 / baseline.** Canada-beats-sonic binds to 04:149-159, 222-226; every kernel win is a paired
  interleaved cyc/byte ratio, decision-grade only when every window's CI excludes 1.0 same-way.

### TB6 — FLEET / ORACLES / TOML / CSV + FEATURE FOLDS (= GW6 + the brainstorm-fleet feature folds)
- **Charter sentence.** Re-baseline the grammar fleet from 3 toward ten, execute the TOML + CSV
  flip, and fold the net-new QoL / performance features surfaced by this pass's brainstorm fleet.
- **Entry deps.** TB3 (GW2a's one generator — the fleet regenerates through it) AND TB1's GW0.b
  (the grammar-source port-in is the precondition the frozen SPEC never anticipated, SK-V22 §1.2
  TW6 row). The terminal bbnf tranche.
- **Close-gate CLASS.** Feature-fold consumer-suite parity — fleet parity across the ten grammars;
  CSV/TOML consumer-invariant (the CSV flip exercises the KernelCostTable's consumer binding,
  SK-V21 §5.1); every folded feature carries its own born-RED proof + EVIDENCE-INDEX row.
- **Cross-repo edges.** None direct. New negative results enter NEGATIVE-RESULTS + the index in
  the same commit that measures them.
- **H1 / baseline.** Per-corpus cold capture across the widened fleet; no regression vs TB5's
  end-state.

---

## §C — parse-that: five named tranches (PT-R Rust track + PT-T TypeScript track)

Non-clobber law (PASS-ALPHA): NOTHING in parse-that is modified/staged/committed by this campaign
except one additive coordination letter at close; all prototyping is on scratch COPIES. Tranche
lettering stays on the PT-R*/PT-T* family (native series A/B/Q/S untouched). Two parallel intra-
repo chains: **PT-R1 → PT-R2 → PT-R3** (Rust) and **PT-T4 → PT-T5** (TypeScript), joined to bbnf
only at the named cross-repo edges (§D).

### PT-R1 — PT-R PURE-PRIMITIVES + DE-NIGHTLY TRANSPOSITION (= PT-R.1 + PT-R.2; axis-1 x86 purge)
- **Charter sentence.** Delete the falsified `span_parser/` (~1,150 LOC), the demo parsers, the
  `bootstrap` re-export crate, the warm hand-timed CLI, and the WDM/LR tier (SEAM-NOGO); then
  de-nightly as REDESIGN — `std::simd → core::arch::aarch64` intrinsics, `smallbox → BumpSlab`
  arena, the mis-attributed toolchain file truthed — which IS parse-that's x86 abrogation, and
  plant the anti-regrowth guard.
- **Entry deps.** None (parse-that root; deletion-first per abrogate-before-patch — do not
  transpose code a later wave deletes, SK-V22 §3.2).
- **Close-gate CLASS.** Hygiene/stack build+bench parity — `combinator_test.rs` + regex tests
  green; workspace members = `parse_that` + `regex` ONLY; `cargo +stable build` EXIT 0;
  `cargo tree -e features` zero nightly; scanner bench parity within the load-accounted band;
  `grep -c bbnf-lang rust-toolchain.toml == 0`; the NEW x86/portable_simd guard fires on a planted
  regrowth.
- **Cross-repo edges.** Independent of bbnf. Its de-nightly is the precondition asserted (not
  owned) by bbnf TB3.GW2b's cross-repo engine build (§D edge 5) — but the engine crate itself
  carries no `#![feature]` (all four nightly gates localize to the combinator crate,
  `lib.rs:1-3` + `Cargo.toml:97`; gate-verified at GW2b).
- **H1 / baseline.** parse-that's own divan scanner benches, cold, load-accounted band. No bbnf
  baseline binding.

### PT-R2 — PT-R REGEX ENGINE SOTA + X86 PURGE (= PT-R.3 + axis-2 SIMD home for parse-that)
- **Charter sentence.** Fold `is_accept` into the transition table (tagged StateId / is_match
  bitset), read `padded_bytes()` in the `find_at` hot loop, degrade the 4096-state bail to an NFA
  walk (never `panic!`), AND execute the SOTA NEON uplift — shufti/truffle/Teddy-class prefilters,
  vectorized DFA stages, the vshrn idiom — into the `scan/*_simd.rs` kernels (`quote_parity.rs`,
  `number_simd.rs`, `quoted_simd.rs`).
- **Entry deps.** PT-R1 (de-nightly complete — aarch64 intrinsics are now the substrate; the SIMD
  surface was first shrunk by PT-R.1).
- **Close-gate CLASS.** Substrate runtime consumer-invariant — match-position parity across
  `regex/tests`; paired cyc/byte delta profiled under load accounting; prefilter liveness
  (checkasm-class, the dav1d gate). The engine's other SOTA tiers are confirmed-present
  (byte-class compression, Hopcroft, accel, one-pass, egraph HIR simplification); lazy-DFA and
  captures stay rightly absent.
- **Cross-repo edges.** Independent, BUT its engine surface is exactly what bbnf TB3.GW2b vendors
  byte-identically. The SOTA state must be stable before the fold-back (PT-R3) and the vendor
  (GW2b) capture it.
- **H1 / baseline.** parse-that's own regex/scanner benches, cold, load-accounted.

### PT-R3 — PT-R FOLD-BACK + SEAM HARDENING (= PT-R.4 + PT-R.5 + PT-R.6; the highest cross-repo consequence)
- **Charter sentence.** Execute the bidirectional fold-back (skinny's recognizer tier upstream as
  the engine's fast front; `StringMode`/`StringFlags`, the Höhrmann UTF-8 decoder, and the
  {ws,quoted,number} family scanners absorb as engine-owned code; the 660-LOC `eisel_lemire`
  duplication collapses to ONE engine-homed module), relocate the resolution graph (patch web →
  `crates-infra/egraph[-derive]`), and close the hazards (`Box::leak` → interner; `parse_padded`
  entry).
- **Entry deps.** PT-R2 (the SOTA engine is the fold target) AND bbnf TB1.GW0.b (the
  `crates-infra/egraph` re-home target must EXIST before PT-R.5 repoints to it).
- **Close-gate CLASS.** Cross-repo paired byte-identity + parity — PT-R.4: number/decode/
  quote-parity suites green in BOTH repos, DFA spans identical to the family scanners on
  {ws,quoted,number}, grep-zero forked modules in the graduated tree, vendor byte-identical at the
  pin; PT-R.5: clean `cargo metadata`, zero path escapes into an archived directory; PT-R.6:
  diagnostics-feature tests + load-accounted copy-cost profile.
- **Cross-repo edges.** THE TWO HEAVIEST: PT-R.4 ↔ bbnf TB3.GW2b (§D edge 2, paired fold-back);
  PT-R.5 sits INSIDE the ordering law GW0.b → PT-R.5 → GW0.c (§D edge 1) — PT-R.5 BLOCKS GW0.c's
  `crates/` removal.
- **H1 / baseline.** The parity suites are the gate (not a perf ratio); the copy-cost profile is
  load-accounted. Binds to bbnf only through the paired parity, not through research/04.

### PT-T4 — PT-T V8 SHAPE / DIAGNOSTICS (= PT-T.1 + PT-T.2 + PT-T.3; PT-E1/PT-E2)
- **Charter sentence.** Tighten inference (`Parser<any> → readonly Parser<unknown>[]`, `const`
  type params, `NoInfer`), add scoped diagnostics (`parseWithDiagnostics()` returning the existing
  `Diagnostic` via the PACKRAT_ARMED latch — one pattern, two tiers), and normalize V8 hidden
  classes (uniform 3-field `Parser.context`, retire `mapState`'s `Object.create` dictionary-mode
  hazard).
- **Entry deps.** None — the TS track runs in parse-that's own repo, independent of both the PT-R
  chain and bbnf.
- **Close-gate CLASS.** Consumer-suite parity + shape stability — `.d.ts` inference assertion
  (`any(string("a"),string("b")): Parser<string>`); cross-repo typecheck of value.js + latex-paper
  against the candidate `.d.ts` green (the semver tripwire); born-RED `diagnostics-scoped.test.ts`;
  shape-stability clause (`%HaveSameMap` probe); json-regression floor holds.
- **Cross-repo edges.** SEAM-PT-E1 OUTBOUND (§D edge 4): PT-T.2 ships the scoped `Diagnostic`;
  bbnf TB4/GW4+ copies its shape when the Value API grows an error surface.
- **H1 / baseline.** value.js suite green against the 1.1.0 candidate tarball; the three backbone
  suites are the discipline bar. No perf ratio.

### PT-T5 — PT-T 1.1.0 CUT + LEGACY SWEEP (= PT-T.4 + PT-T.5; the version cut)
- **Charter sentence.** Retire the unconsumed state-tier API
  (`save`/`restore`/`from`/`getLineNumber`/`getColumnNumber`), delete the stale
  `span-combinators.md` doc (a live falsehood teaching an excised API), carry the dispatch
  megamorphism lever ONLY if profiled dominant on value.js's real CSS shape (the runtime
  tagged-switch is PERMANENTLY falsified, −10-14% on V8 — shall not be re-attempted), and cut the
  1.1.0 release.
- **Entry deps.** PT-T4 (inference + diagnostics + hidden-class land before the version cut).
- **Close-gate CLASS.** Feature-fold consumer-suite parity — `dist == source` barrel; born-RED
  `proof:no-legacy-state-surface`; `npm run proof:all` green; the three backbone suites (value.js,
  latex-paper, keyframes-transitive) run against the 1.1.0 candidate BEFORE publish;
  `proof:perf` real-corpus clause or DO-NOT-CARRY for the dispatch lever.
- **Cross-repo edges.** The backbone semver discipline (value.js / latex-paper / keyframes must
  not break); `@mkbabb/bbnf-lang ^0.1.1` and `@mkbabb/parse-that` published edges re-version, never
  mutate in place (SEAM-LATEX-PAPER).
- **H1 / baseline.** Consumer suites + `proof:all`; the optional dispatch lever is the only
  perf-gated item, real-corpus profiled.

---

## §D — THE INTERLOCK GRAPH (explicit cross-repo + intra-repo edges)

Intra-repo (sequential):
```
bbnf:        TB1 ──► TB2 ──► TB3 ──►┬─► TB4 ─┐
                                    └─► TB5 ─┴─► TB6
parse-that:  PT-R1 ──► PT-R2 ──► PT-R3          (Rust chain)
             PT-T4 ──► PT-T5                    (TS chain, parallel to Rust)
```

Cross-repo edges (the named laws — a mis-order or a missed pairing bricks a build or breaks a
consumer):

1. **THE ORDERING LAW (highest consequence).** `TB1.GW0.b (egraph → crates-infra port-in)` →
   `PT-R3.PT-R.5 (patch-table repoint to crates-infra/egraph)` → `TB1.GW0.c (crates/ removal)`.
   A mis-order bricks BOTH trees' builds at once (SK-V22 §3.1 SEAM-EGRAPH, §3.4). GW0.b's close
   verifies both repoints before any removal. This edge spans the SEAM: PT-R.5 lives in
   parse-that's PT-R3 tranche but is temporally clamped between two GW0 sub-steps.

2. **THE FOLD-BACK PAIRING.** `PT-R3.PT-R.4` ↔ `TB3.GW2b` — the two halves land against each
   other's parity suites; the vendor mirror is byte-identical to the pinned upstream commit; the
   {ws,quoted,number} family-scanner parity is the shared gate (SK-V22 §3.2 B, §3.4).

3. **THE FLOAT-TAX COMPANION.** `TB5.GW3` ↔ a paired PT companion commit into the canonical
   `eisel_lemire` module; the vendor re-syncs in the SAME wave; the gate is always measured
   bbnf-side (SK-V22 §1.2 TW4.1 row, §3.4).

4. **THE DIAGNOSTICS SEAM.** `PT-T4.PT-T.2 (PT-E1 Diagnostic)` → `TB4.GW4+` — bbnf copies the
   scoped-return `Diagnostic` shape when the greenfield Value API grows an error surface
   (SK-V22 §3.1 SEAM-PT-E1).

5. **THE ENGINE-VENDOR DEPENDENCY.** `TB3.GW2b` gate-verifies "the canonical engine builds on the
   graduated stable toolchain against `crates-infra/egraph`" — needing `PT-R2` (SOTA engine
   surface stable) + `PT-R1`'s de-nightly (gate-verified that the engine crate carries no
   `#![feature]`; if it proves nightly-bound, GW2b fails and PT-R.2's scope extends — SK-V22 §3.4).

Independent of bbnf entirely: `PT-R2` (self-gated), `PT-T4`, `PT-T5` (TS track, own repo, own
gates), and `PT-R1` except as the precondition in edge 5.

---

## §E — CLOSE-GATE CLASSES (the Y.13 taxonomy, one label per tranche)

Every substrate wave closes on a runtime consumer-invariant gate (liveness AND measured win);
hygiene/stack waves close on build+bench parity; each close cites the template (SK-V22 §1.1). The
five classes used above:

| Class | Definition | Tranches |
|---|---|---|
| (a) cutover / pure-move parity | a move must not move a number; tags resolve; guards fire | TB1 |
| (b) hygiene / stack build+bench parity | stable build EXIT 0; bench within the load-accounted band | TB2, PT-R1 |
| (c) substrate runtime consumer-invariant | liveness AND measured win, paired-interleaved CI | TB3(GW2a), TB4, TB5, PT-R2 |
| (d) cross-repo paired byte-identity + parity | vendor `diff -r` clean; parity suites green both repos | TB3(GW2b), PT-R3 |
| (e) feature-fold consumer-suite parity | fleet/consumer suites green; each fold born-RED + indexed | TB6, PT-T4, PT-T5 |

---

## §F — H1 / BASELINE BINDINGS (one line per tranche)

- **bbnf, all tranches:** research/04 load-accounted is the standing ANCHOR; each tranche's PROOF
  is its own paired same-session before/after capture vs the prior end-state; the anchor never
  moves, the proof always pairs (SK-V22 §5.7).
- **TB1:** H1 = the twitter sonic/parse_tape reconciliation capture (2.10× vs 1.59×, +36%
  sonic-side drift disclosed) — gates GW0.e's PERFORMANCE-THESIS; 04 established as anchor.
- **TB2:** GW1's channel ratio re-anchors the TB3+ floor (04-adjusted, ledgered).
- **TB3/TB4:** paired per-corpus cold capture vs the re-anchored floor; no regression.
- **TB5:** canada-beats-sonic binds to 04:149-159, 222-226; every kernel win a paired cyc/byte CI.
- **TB6:** per-corpus cold capture across the widened fleet vs TB5's end-state.
- **parse-that, all tranches:** parse-that's OWN divan scanner/regex benches, cold, load-accounted
  band — NO bbnf-baseline binding except at the paired seams (PT-R.4↔GW2b family-scanner parity;
  GW3-companion canada). The TS tranches gate on consumer suites + `proof:all`, not perf ratios.

---

## §G — OPEN SEAMS FOR THE SYNTHESIS (skeleton residuals, not design questions)

1. **The GW0.b placement tension.** Port-in physically executes at GW0.b (TB1) per the ordering
   law, yet PASS-ALPHA groups "port-in" under tranche (ii)/TB2. §B TB2 resolves this by splitting
   EXECUTE (TB1) from CLOSURE/hardening (TB2); the synthesis should ratify or re-cut the seam.
2. **The GW4 split.** GW4 is cleaved across TB4 (TW3 value plane) and TB5 (TW4.2 decision/kernel
   plane) to match candidates (iv) vs (v). If the synthesis prefers GW4 intact, TB4/TB5 merge and
   bbnf drops to five tranches — still charter-legal (≥5).
3. **x86-guard mechanism.** The anti-regrowth enforcement (cfg-deny vs workspace lint vs
   Lock-class falsifier) is named-but-not-specified per repo; the synthesis picks one mechanism
   and homes its born-RED planted-phantom test (TB1 bbnf, PT-R1 parse-that).
4. **The greenfield-vs-carry table (§A axis 3)** is enumerated here at subsystem granularity; the
   synthesis should promote it to a first-class TB0/PASS-BETA deliverable with per-row evidence.
