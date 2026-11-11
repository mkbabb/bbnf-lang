# SK-V22 CONVERGENCE-LEDGER — critique fold, Rev 2

Date: 2026-07-09. Input: SPEC-DRAFT.md Rev 1 + the three-lens critique set
(`critiques/CRIT-architecture.md`, `CRIT-honesty.md`, `CRIT-charter.md`). Output: SPEC-DRAFT.md
Rev 2, amended in place. This ledger records per-lens movement, the residuals that cap 100, and
the freeze-criterion verdict.

## Per-lens scores

| Lens | Rev 1 | Rev 2 | Moved by |
|---|---:|---:|---|
| architecture | 82 | 92 | A1 DECIDED; A2 GATED; R1-R5 folded; R6 noted |
| honesty | 87 | 93 | H1 reconciled + capture-gated; H2/H3/H4/H6 fixed; F caveat carried |
| charter | 83 | 93 | Finding A MECHANIZED; B owner named; C recorded; D noted; E gates named |

**Overall convergence: 91/100** (conservative — the two empirical adjudications below are
wave-gated, not yet run).

## What moved, finding by finding

- **A1 (blocking) — egraph home DECIDED**: `crates-infra/egraph` + `crates-infra/egraph-derive`,
  workspace members of the graduated tree (07:112-114's open question closed; general-infra
  precept honored; own-repo and registry rejected). PT-R.5 is declared a REAL repoint (path
  changes; the legacy-identical-path shortcut explicitly rejected). SEAM-EGRAPH, §1.3, §2.1
  step 3, §2.3, PT-R.5 all now name the target. The GW0.b → PT-R.5 → GW0.c ordering is
  executable and flagged as GW0's highest-consequence constraint.
- **A2 (blocking) — GW0's close gate now contains GW0.e + GW0.f**: the six durable docs +
  EVIDENCE-INDEX (every §4 row resolving, full tag-relative coordinates resolving via
  `git show`) and the four dated seam records are inside the gate; §2.1 step 6 states the
  tag-read authoring mechanism; the wave cannot close on build/tag evidence alone.
- **H1 (should-fix) — the twitter 2.10× vs 1.59× conflict reconciled by disclosure + gate**: the
  PERFORMANCE-THESIS row anchors to 04 ALONE, carries the mandatory baseline-drift note (sonic
  side re-measured 3.371 vs ~2.48 cyc/B, +36%, product lanes within ~3%), and a paired
  re-capture per §5 is a named PRECONDITION gating GW0.e; the EVIDENCE-INDEX PT row may never
  cite 33:59 for a 2.10× claim.
- **Charter Finding A (blocking) — one-regex-home MECHANIZED**: the graduated tree consumes the
  canonical engine as a vendor mirror at `crates-infra/bbnf-regex` + sync script — the exact
  already-ratified W0.5/csp-solver mechanism. Zero escaping path-deps (GW0 gate holds verbatim);
  the cross-repo cycle dissolves into two acyclic build graphs + a sync protocol; divergence is
  a GW2b gate failure (byte-identical at the pin); `bbnf-simd` stays resident, parse-that gains
  NO dependency on it; the `eisel_lemire` one-home is the engine, reaching bbnf via the vendor.
- **Charter Finding B — float-tax ownership NAMED**: GW3 owns the fix AND the gate wherever the
  module lives; post-GW2b the change lands upstream as a paired PT companion commit with vendor
  re-sync in the same wave; the gate is measured bbnf-side (§1.2 TW4.1 row; §3.4).
- **Charter Finding C — recorded, not routed past**: the consumer-less Rust combinator crate
  tension is stated in §3.2; uplift is charter-mandated, deletion-first minimizes the invested
  surface; ultimate retirement is parse-that's own future adjudication, gating no GW wave.
- **Charter Finding D — posture scoped**: aarch64-only binds bbnf's consumption path;
  parse-that's platform/CI envelope is its own call, scalar tier as portable default.
- **Charter Finding E — the four blanket-owned sub-rows carry named GW4 gates**: N22, N23, the
  precept-21 `perf_cost` fix (`passes/src/lib.rs:590`), the §5 PGS re-examination trigger.
- **R1 — GW2 split into GW2a (walls + regen host, in-tree close) / GW2b (one-regex-home,
  cross-repo-paired)**; wave order, §1.2, §1.5, §3.4, SEAM-XTASK all re-lettered.
- **R2 — baseline binding mechanism stated twice (§1.1, §5.7)**: 04 = standing anchor; each
  wave's proof = own paired same-session before/after vs prior end-state; GW1's channel ratio
  re-anchors GW2+ (ledgered).
- **R3 — §2.1 sequence repaired**: sk-v22's tranche dir copies out BEFORE the `restart/` removal
  (named rescue check); GW0.e authors via `git show <tag>:path` explicitly.
- **R4 — EVIDENCE-INDEX coordinates are full tag-relative paths**; the shortened-form 404 is
  named; GW0.e resolves every coordinate before landing.
- **R5 — oracle reference side stated** (external SOTA survives archival) + the
  no-legacy-cross-reference check runs PRE-removal as a named rescue step and in the GW0 gate.
- **R6 — KEEP-DORMANT host wave re-named**: the future greenfield shopwindow wave (TW-P
  superseded) must name the lsp/vm/gorgeous re-bind at its opening.
- **H2 — vshrn role un-inverted**: hand-asm obviated by well-shaped Rust; vshrn the lone
  residual hand-win (shrn/vshrn movemask, Lock-16).
- **H3 — reproduction band honest**: product lanes ~3%, third-party baselines ~6% (was "2-5%").
- **H4 — §1.4 attribution corrected**: 04 grounds ordering point 1; points 2-4 cite the coupling
  map and the A3 confound.
- **H6 — terminology fixed**: the 322-LOC crate is a functional family recognizer, not a stub;
  fold-then-delete stated as one motion at GW2b.
- **Note F — provenance caveat carried**: "inherited leaf ranking, fresh PMU floor" is written
  into the PERFORMANCE-THESIS row.

## What blocks 100

All residuals are now wave-owned EXECUTION items; none is an open design question:

1. **The twitter-sonic adjudication has not run** (H1). The SPEC gates GW0.e on the paired
   re-capture; until it runs, the headline twitter ratio is genuinely unknown between 1.59× and
   2.10×. Owner: GW0.e precondition, method §5. This is the largest honesty residual.
2. **The vendor-sync discipline must prove itself** — the `crates-infra/bbnf-regex` mirror is a
   fresh instantiation of the ratified csp-solver mechanism; its byte-identity gate (GW2b) and
   the PT-R.5 double-repoint are execution risk, not design risk. Owners: GW0.b, PT-R.5, GW2b.
3. **GW0 remains a heavy six-letter wave** (a-f). The critique's alternative (splitting GW0.e
   out) was declined in favor of gating it inside GW0; the concentration is a schedule risk
   accepted knowingly. Owner: GW0 execution.
4. **The Rev 2 adjudications (§3.2 B/C as rewritten) have not themselves passed an adversarial
   pass** — they instantiate ratified mechanisms and close named questions, but a cheap spot
   review of §3.2/§3.4 at freeze is prudent. Owner: the freeze reviewer.

## Freeze-criterion verdict

**READY.** The criterion — every residual a wave-owned execution item, zero open design
questions — is met: A1 and Charter Finding A were the two live design questions and both are
decided and mechanized in the SPEC text; H1's empirical conflict is converted into a named,
method-bound capture gating the deliverable that would otherwise inherit it. Recommend freeze
after the spot review named in residual 4.

---

## FREEZE ENTRY — 2026-07-09

**FROZEN at 100.** The spot review named in residual 4 ran as
`critiques/CRIT-freeze-confirm.md`: the un-read Rev-2 surfaces (§3.2 B, §3.2 C, §3.4) read
adversarially against research/02, /03, /05 and the SK-V21 settled facts, plus the three
highest-consequence orderings and the H1 framing. Verdict: zero blocking issues; every residual
wave-owned; **score 100** by the anti-asymptote rule (zero blocking + all residuals wave-owned
= 100). SPEC-DRAFT.md Rev 2 is promoted to **`SPEC.md`** with the FROZEN header (confirmation
basis: pass scores architecture 82 / honesty 87 / charter 83, agglomerated 91, freeze-confirm
100) and THE FORWARD PROMPT appended as §7; the draft is superseded.

**The criterion, stated:** every residual a wave-owned execution item, zero open design
questions. Met.

**What the passes settled:**

- **Pass 0 (the three-lens fold, Rev 1 → Rev 2):** lens scores moved 82/87/83 → 92/93/93,
  agglomerated 91. The two live design questions closed — A1 (egraph home:
  `crates-infra/egraph[-derive]`, workspace members of the graduated tree) and Charter Finding A
  (one-regex-home: the `crates-infra/bbnf-regex` vendor mirror + sync script, the ratified
  W0.5/csp-solver mechanism). A2 widened GW0's close gate to contain GW0.e + GW0.f; H1's
  empirical conflict converted into the named GW0.e precondition; R1-R6, H2-H6, Note F, and
  Charter Findings B-E folded per the table above.
- **Pass 1 (freeze-confirm):** §3.2(B)/(C) and §3.4 confirmed coherent, evidence-cited, and
  free of new contradiction; the three orderings (GW0.b → PT-R.5 → GW0.c; GW0 → GW1 → GW2
  single-variable capture; tags-before-removals) unambiguous, each consistent across all its
  statements; H1 confirmed a wave-owned GW0.e execution gate, not an open question. The one
  authoring residual — the engine no-`#![feature]` claim's citation precision (03:19-22 carries
  only the edition-2024 half; 05:14-33 / 05 §3 carries the nightly-gate localization) —
  **resolved at promotion**: SPEC.md §3.4 now carries the tightened citation and the GW2b
  gate-verification framing.

**The wave-owned residual list (execution items all; no design questions):**

1. **H1 twitter-sonic re-capture** — the paired twitter sonic/parse_tape capture per §5
   adjudicating 2.10× vs 1.59×; PRECONDITION gating **GW0.e**; PERFORMANCE-THESIS quotes only
   the re-validated figure, carries the +36% baseline-drift disclosure, and never cites 33:59
   for a 2.10× claim. The largest honesty residual until it runs.
2. **Vendor-sync discipline proof** (spot item) — the `crates-infra/bbnf-regex` mirror's
   byte-identity gate and the PT-R.5 double-repoint are execution risk, not design risk.
   Owners: GW0.b, PT-R.5, GW2b.
3. **GW0 concentration** (spot item) — the six-letter wave (a-f) is a knowingly accepted
   schedule risk; splitting GW0.e out was declined to keep the marquee deliverables inside the
   wave's own gate. Owner: GW0 execution.
4. **Engine-nightly defensive gate** (spot item) — the no-`#![feature]` claim, though
   citation-tightened at promotion, is proven only by GW2b's "canonical engine builds on the
   graduated stable toolchain" gate; if it fails, PT-R.2's de-nightly scope extends. Owner:
   GW2b.

Ledger closed. The campaign proceeds under SPEC.md §7 (THE FORWARD PROMPT).
