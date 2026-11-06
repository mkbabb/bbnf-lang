# SK-V25 INVENTORY — the complete ledger of what exists, what stands, what is suspect

Every path is relative to `/Users/mkbabb/Programming/bbnf-lang` unless rooted elsewhere.
Sibling repos: `/Users/mkbabb/Programming/{parse-that, value.js, keyframes.js}`.

## §A The certified swath — MACHINE FACTS (measurements re-runnable; interpretation suspect)

Source of record: `restart/skinny/tranches/sk-v24/SPEC.md` §0 (frozen) + per-prototype
artifacts at `restart/skinny/tranches/sk-v24/prototypes/PROVE-XX/{REPORT.md, patch.diff,
VERIFY.md}`. All prior prototype CODE was wiped historically — patch.diff is the artifact
of record. Bench instruments: `skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs` +
`css_canon_bench.rs` (cold paired, N≥200, PMU cyc/B).

| # | Claim (as certified) | Verify verdict |
|---|---------------------|----------------|
| 01 | NEON shufti/truffle prefilter — dense-neutral, 0.115 cyc/B floor | CONFIRMED |
| 02 | NEON digit parse — REFUTED (latency inversion; redress = float conversion) | law |
| 03 | Fused string scan — twitter tape/sonic 0.512→0.445 | CONFIRMED |
| 04 | Block pipelining — REFUTED (1.05×; lever = movemask stitch) | law |
| 05 | CSS tbl tokenizer — tailwind 4.16× lightningcss (animate +13.4% regression owed) | CONFIRMED_DELTA |
| 06 | Stop-early tape-free get — beats sonic get 2/3 classes (0.68–0.79) | CONFIRMED |
| 07 | Direct-to-struct fused sink — beats sonic typed all corpora (0.746–0.786) | CONFIRMED |
| 08 | Type plane — HM core 221 LOC, schema at zero runtime cost; 3 walls real | CONFIRMED (walls = GN-3 spec) |
| 09 | portable_simd shim — perf claim REFUTED by verify (de-nightly = stability only) | law |
| 10 | Regex prefilter tier — 8.8–29.5×, compile-neutral | CONFIRMED |
| 11 | TS lazy diag alloc — +13.6/+15.6%, alloc −60% | CONFIRMED |
| 12 | Const-mask residualization — CSS cyc/B 0.80–0.91 | CONFIRMED_DELTA |
| 13 | CSV generality — ≈csv-core 0.948× directional; name-gate cured structurally | UNVERIFIABLE-perf (generality verified) |
| 14 | Tape-in-TS — byte-exact crossing; stop-early 4.4× | CONFIRMED_DELTA |

## §B The implementation codex (all judgment-tier prose — suspect framing, real content)

`docs/tranches/sk-v24/{INDEX.md, GN-0..GN-6.md, PT-1..PT-3.md}` (~360KB, ~47k-LoC program;
authored → 3-auditor sweep (31 defects) → fixed → harmonized; commit 27044805e).
GN-0 archival + x86 purge; GN-1 the loom (greenfield workspace, colocation); GN-2 kernel
plane; GN-3 type plane; GN-4 value fabric; GN-5 generality (CSV/TOML); GN-6 shopwindow;
PT-1 regex SOTA; PT-2 rust core reform (zero perf claims); PT-3 TS 1.1.0. PT series is
FULL IMPLEMENTATION by owner supersession (recorded at SPEC §2b).

## §C THE LOCKS at the second pause — all statuses SUSPECT-JUDGMENT pending apotheosis

Registries: `restart/skinny/tranches/sk-v24/locks/LOCK-{1..9}/REGISTRY.md` (+ per-pass
artifacts in `LOCK-N/pass-K/`). Cross-lock ledger: `locks/CROSS-LOCK.md` (XL-A..XL-H).
Portfolio + CONSTITUTION: `locks/PORTFOLIO.md`. bbnf locks {1,2,3,7} through pass 2;
pt {4,5,8,9} + weave {6} through pass 3. Sealed state (prior campaign's own numbers):

| Lock | Families at pause |
|------|-------------------|
| 1 solver spine | 1A 52 / 1B 53 ADV (carrier fork on iso axis); 1C 40, 1D 42 BANKED. X8 ruled UNSHIPPED at HEAD; phantom let-gen floor RETIRED |
| 2 graph engine | 2A 56 ADV; 2C 50, 2B 44 BANKED; 2D 32 BLOCKED. PROVE-2-PROMOTE = named top mover; N*=3.0 = uniform-corpus artifact; bounded_sum worktree-earned |
| 3 IR spine | 3A 60 ADV (oracle proven, injected-fault matrix owed); 3B 55 → G-3B-1 byte-identity REFUTED (demote); 3C 32 BLOCKED; 3D 38 BANKED |
| 4 combinator core | 4A 73 ADV (G-A1 PASS; keystone = real SmallBox<dyn ParserFn> crate seam); 4B 66 (canada 1.56× exogenous ceiling); 4C 63 BANKED; 4D 58 BLOCKED |
| 5 regex continuum | P2 titular ADV 60 — grammar-driven regex Hir at FULL 56/56 DFA parity; 5D 55 ADV (ByteClass end-to-end); 5A 42 ADV (relabel-candidate); 5B RETIRED; 5C RETIRED 52 (survivor ships as table) |
| 6 isomorphism plane | 6B 76 / 6A 66 / 6D 66 ADV; 6C BANKED 80 (wasm-resident-bytes lane confirmed) |
| 7 grammar totality | folded pass-2 (UF-1/UF-2 oracles sound; three parser fixes landed as probe openers); registry is the record |
| 8 TS backbone | 8A temporal specializer RETIRED (triple-strike); 8C guard answered (pfa.rs 1.00×/Hopcroft-safe); Parser<T,E> BANKED |
| 9 scanner/kernel | 9B ADV 62 (two artifacts banked, off SPEC-ONLY); 9C BLOCKED 43 (unblocks on 5D ByteClass); 9D BLOCKED |

CROSS-LOCK adjudications (each gets an apotheosis): XL-A tree-of-record; XL-B tower
ownership (LOCK-3 seat); XL-C regex axis = two domain-fit types + cheap bridge; XL-D fast
TS face = build-time emitter, decoders = oracles; XL-E 4B-vs-PROVE-09 reconciliation;
XL-F named supersessions; XL-G owner confirmations (P1 is now EVIDENCED ALIVE — see §E);
XL-H base law.

## §D THE TAIL (verbatim obligations — every row enters the new registry with an owner)

1. Six pending cross-lock cuts (second pause): 5C→LOCK-2 SPLIT (automata-min-not-egraph =
   run-earned binding; tier-not-CSP = strong proposal pending flipping witness); GRAFT-3
   one-dispatch-surface (born-RED); Axis-1 two-types re-cut of 4D/9C/3C (ownerless);
   TS Dfa port homing (G-C1 DAG-blocked); land-to-HEAD: X8 clash arm (revise.rs raw LWW at
   HEAD), bounded_sum (absent from crates/csp-solver at HEAD), TypeDesc::Row; the
   stale-gestalt process guard (3× recurred — reconcile every in-folder proto before cuts).
2. bbnf pass-3 (convergence talk opens) + pt pass-4 — fold into the apotheosis loop.
3. Non-author audits per lock; two clean passes; GESTALT-B ∥ GESTALT-P → GESTALT-Ω →
   the perfected dual-repo tranche set with DAG ordering (v2, named supersessions of §B).
4. Owner gates: SPEC §3 re-cut checkboxes; BBNF-PT-G content ratification; XL-G P2.
5. Named reactivation experiments: R-C2 (1C), B3/B4 modeling census (1B), /foo$/ Look
   consumer (5A/5D/P2 joint), PROVE-2-PROMOTE + PROVE-2-DIST + 2D-FIT (LOCK-2), 4A crate
   seam, 4C canada crossing + rmp_serde G-C4, 4B padded-input contract + G-B4 curve,
   4D tier-1 MapExpr nodes, 5D Q7 wired-fraction + FOLLOW wiring, 9C Sheng question,
   G-CRIT-alpha monomorphization gate (LOCK-1⊕7), HostClosure shape-of-record homing.
6. GN-6/PT letters: XL-E supersession placement in a PT-tranche letter at GESTALT-P.

## §E THE CONSTELLATION MARKS (recon 2026-07-18, from each repo's git log)

**parse-that** (`master`, v1.0.0): tranche series A→B→Q→S landed — A 0.11.0 (subpath split;
SpanParser perf hypothesis FALSIFIED; packrat (id,offset) left-recursion soundness fix);
B 0.12.0 (packrat cross-input BLOCKER fix; combinator fusion; SpanParser KILL); Q 0.13.0
(packrat re-entrancy + >1MB key fix; thenMap/fuse delete; *Span deprecate); S.H1 packrat
epoch armed behind PACKRAT_ARMED; S.H2 the 1.0.0 legacy cut (*Span excised, chain() falsy
seed C-16); S.H4 1.0.0 cut. HEAD = ef10d5b: **VALUEJS-PT-E letter** — value.js S.W1 asks
for 1.1.0+ (diagnostics HIGH · inference MED · Pratt-dormant record). Coordination letters
live in `parse-that/docs`. THE COUPLING IS ALIVE — this settles the old XL-G P1 question
in the affirmative-linked direction (apotheosis to confirm).

**value.js** (branch `tranche-u`, mid-mega-tranche, active Codex session developing V):
HEAD c654824e "land I-11/I-12 — the vnext kickoff prompt + context packets (r2
true-Fable); the mega-tranche formation opens" — that repo has already met and named the
model-truth problem. W44 closed: Glass UI 7.0.0 adopted across the demo surface, born-RED
blank cured, demo typecheck/tests flipped hard. W51 closed: byte-exact palette export.
Cron layer torn down by owner order (Step-0 sweeps carry E13). Tranches under
`value.js/docs/tranches`.

**keyframes.js** (`master`): tranche-u sealed (immutable release + deployment packet);
Glass UI 6 consumed from one registry core; occlusion axis described opt-in. Named
structure debts (owner): `src/animation` superfluous top level; `src/animation/internal`
disliked; parsing facilities to be ruthlessly examined vs value.js ownership split.

**The linkage**: bbnf-lang ↔ parse-that (one organism; one-regex-home in parse-that;
vendored copy in `skinny/crates/parse-that-regex/` is measurement substrate; top-level
`crates/bbnf-regex` is legacy, archives at GN-0). parse-that ↔ value.js + keyframes.js
(flagship consumers; VALUEJS-PT-* letter channel). value.js ↔ keyframes.js (CSS ownership
split under interrogation). All ↔ glass-ui (frontend idioms; defect-batching channel to
the working glass-ui agent — do not interrupt it).

## §F Durability artifacts + the walls corpus (re-create the pattern in Codex idiom)

- The resurrection ledger: `restart/skinny/tranches/sk-v24/CAMPAIGN-STATE.md` — stage
  plan, workflow ledger (runId + scriptPath + args per row), pass-transition procedure,
  THE PAUSE / SECOND PAUSE templates, and the walls log (the campaign's scar tissue).
- Walls lessons (all proven in fire, 2026-07-16): args may arrive as a JSON string —
  normalize before use; StructuredOutput-style schema deaths usually leave files on disk —
  check disk before re-dispatching; resume caches are interleaving-sensitive — re-folds
  are refinement, registries cumulative; worktrees mint from stale refs — BASE-PIN every
  measuring prototype and record the exact HEAD; a lookup keyed wrong (task ID vs runId)
  is not proof of death — verify liveness by artifact mtimes before resurrecting; a
  process exit silently kills schedulers — re-arm the heartbeat FIRST; width-throttle
  agent concurrency (the batches-of-three semaphore; the new prompts say 5–6) rather than
  trusting resets; never duplicate-launch — reconcile ledger vs live tasks first;
  launch-intent is ledgered BEFORE launching, runIds after.
- Workflow scripts (the orchestration shapes to transpose):
  `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/workflows/scripts/`
  — recon, proving-fleet, verify-fleet, codex-authorship, and locks-pass (the
  parameterized 5-stage pass with distiller, width semaphore, BASE-PIN, XLOCK injection).
- The journal caches for the two long runs (per-agent results, model metadata — the
  taint evidence): `.../subagents/workflows/wf_fce2ec2b-53a/` and `wf_acfc23d2-04b/`.

## §G Open owner gates (untouched by any agent, carried verbatim)

1. SPEC §3 re-cut checkboxes (three; defaults proceed absent veto): BBNF-PT-G ships as
   contingent proposals with patches; parse-that release.yml goes aarch64-only when PT-N3
   lands; GN-0 archival executes immediately post-freeze.
2. BBNF-PT-G letter content ratification (assembled from PROVE-{09,10,11} artifacts).
3. XL-G P2 (LOCK-8's symmetric confirmation; P1 evidenced alive per §E, pending
   apotheosis confirmation).
