# CRIT-architecture — SK-V22 SPEC-DRAFT, the graduation re-map + right-sizing lens

Date 2026-07-09. Adversarial read of `SPEC-DRAFT.md` (425 lines) against its cited evidence
(research/01-04, 07) and the SK-V21 ADDENDUM. Lens: is the graduation re-map coherent (no orphaned
SK-V21 wave, no gate on a dead tree, no four-waves-in-one)? Is the port-in minimal-but-sufficient?
Does the archival risk destroying anything load-bearing? Is the maturation order defensible against
the research/04 baseline?

**Verdict: architecturally sound, near-executable. Two design-level residuals block "zero open
design questions"; three methodological/right-sizing seams warrant a sentence each.**
Convergence 82/100.

---

## What holds (the coherence checks that PASS)

- **No orphaned SK-V21 wave.** §1.2 maps every wave of the SK-V21 order (TW0 W0.0-W0.11, TW-S,
  TW1a-c, TW2, TW3, TW4.1, TW4.2, TW5, TW6) to a GW owner or a supersession. The ADDENDUM's
  additions are also covered: its TW0+ prune rows land in §1.3's port-in table; TW-P is explicitly
  superseded (§6.5); the `analysis→ir` fold that ADDENDUM:28 scheduled "into the TW1a split" does
  not orphan when TW1a dies — §1.3 subsumes it ("skinny's passes+ir carry the live analysis",
  research/02:160). The orphan audit passes.
- **The port-in list is minimal-and-sufficient.** §1.3's three graduation-blocking port-ins
  (grammar `.bbnf` corpus + egraph/egraph-derive + csp-solver) are exactly what research/02 §4
  ranks as blocking; the ~10K figure reconciles (egraph 1,885 + egraph-derive 343 + csp-solver
  7,393 = 9,621, research/02:154-155). `passes`' only out-of-workspace deps are egraph + csp
  (research/02 G8, SPEC-DRAFT:74-75), both in the list. Everything else is REWRITE-SUPERSEDED
  (dies) or KEEP-DORMANT (rides the archive branch). Nothing skinny needs to build is missing.
- **The archival destroys nothing load-bearing.** research/01 §4-5 audits reachability: the ONLY
  non-master-reachable commit anywhere is `f2fe49bbc` (sk-v16-w6 lazylock), and it is tagged before
  removal (SPEC-DRAFT:150). The threaded-fork's +102 dirty diff is skim-then-drop on a file already
  DELETE-marked (01:97). Every other worktree/sibling is 0-ahead and reachable. The evidence index
  is protected by a REAL gate — "each named tag resolves (`git show <tag>:path` on a sampled file
  per surface)" (SPEC-DRAFT:110) — and tags are created before any removal (normative order,
  §2.1 step 1). This is a careful audit.
- **The maturation order is well-defended against research/04.** §1.4's argument is baseline-driven:
  the certified floor is frozen (`generated.rs` untouched since 2026-06-02, reproduced within 2-5%,
  04:26-33), so GW0 (pure move) and GW1 (toolchain) bind cheap "must-not-move-a-number" parity
  gates while the floor is fresh; GW2 is the first wave that legitimately moves emitted code; GW3
  targets the ONE measured regression (canada eager float +1.48 cyc/B, 04:149,222-226). The plan
  correctly does NOT open a scanner-optimization wave despite the scanner being ~63% of cycles
  (04:179) — because the scanner already beats SOTA 2.10/1.41/2.06× (04:111-115); chasing it would
  re-run report-33's DELETE-ed dead NEON pre-scan. This is a strength, not a gap.

---

## BLOCKING residuals (cap "zero open design questions")

### A1 — The egraph/egraph-derive graduated HOME PATH is left undecided (SPEC-DRAFT:74, 155, 269)
research/07's explicit "open question for synthesis to decide" — *"egraph/csp-solver home: own
repo, a `crates-infra/` in the graduated tree, or publish to registry"* (07:112-114) — is NOT
closed by the SPEC. §1.3 says only "workspace members of the graduated tree" (74); §2.3's top-level
shape puts everything under `crates/` (183) yet the memory precept `general-infra-crates` argues
against stuffing general-purpose e-graphs under domain `crates/`. This is load-bearing because
**PT-R.5's gate cannot be written without it**: SPEC-DRAFT:269 says "workspace-relative paths /
published pins against the graduated tree's egraph home" — an undefined target. The SEAM-EGRAPH
hard ordering `GW0.b → PT-R.5 repoint → GW0.c crates/ removal` (SPEC-DRAFT:200-201) is
unexecutable until the path is named. Note a hidden subtlety the SPEC never confronts: if egraph
lands at the *identical* legacy path `bbnf-lang/crates/egraph`, parse-that's existing patch line
(`../../bbnf-lang/crates/egraph`, 03:34) still resolves and PT-R.5 is nearly a no-op for egraph;
if it moves to `crates-infra/`, PT-R.5 is a real cross-repo blocker. The blocking-vs-no-op status
of the FIRST wave's external dependency flips on an undecided path. **Decide it** (name `crates/`
vs `crates-infra/` and whether the path stays legacy-identical).

### A2 — GW0's close gate omits its own assigned deliverables GW0.e + GW0.f (SPEC-DRAFT:106-111 vs 44,51,53)
The §1.2 table assigns the six durable documents + EVIDENCE-INDEX to **GW0.e** (44,51; §4:349) and
the dated seam records to **GW0.f** (53). But the GW0 close gate (§1.5:106-111) verifies only
build/regen/oracles/parity/tags/Lock-14 — it is **silent on GW0.e and GW0.f**. The entire leverage
plan (§4) rests on "no lesson re-learned," and the standing rule "a graduated claim with no index
row is unsupported" (367) presumes the index EXISTS — yet nothing in GW0's gate forces the six docs
or the index to land. §2.1 puts "durable docs land" as step 6, the LAST step, ungated and
slip-prone. **Right-size**: either split GW0.e (a genuine multi-document distillation from a
41-report corpus) into its own gated wave, or add "the six durable docs + EVIDENCE-INDEX exist and
every §4-table row resolves" to the GW0 close gate.

---

## Right-sizing / methodological refinements (wave-ownable, non-blocking)

### R1 — GW0 and GW2 are the two heaviest bundles; GW2 crosses a repo boundary mid-wave
GW0 carries archival mechanics (tag set + 202G removal + re-found root) **and** a 3-crate vendor
port-in **and** the Lock-14 re-key **and** a 6-document distillation **and** seam records — five
distinct concerns under sub-letters a-f. GW2 (SPEC-DRAFT:59, 116-121) bundles THREE wall
dissolutions (WsPolicy threading, leaf-regex interpreter, WART-C JSON fold — research/02 G1 calls
these "the single largest in-tree gap") **plus** the xtask un-fork/regen-host (2,780-LOC `main.rs`
split, metadata-driven host) **plus** the one-regex-home (delete 2 crates, adopt parse-that's
canonical engine) — and the one-regex-home leg is **cross-repo-coordinated** (paired PT-R.4↔GW2,
§3.4:318), orthogonal to the in-tree walls. A single conjunctive close gate over an in-tree
wall-fix and a cross-repo regex fold couples two schedules that need not be coupled. Consider
GW2a (walls + regen-host) / GW2b (one-regex-home, cross-repo-paired).

### R2 — The GW2+ baseline-binding is methodologically ambiguous (§1.1:35-36 vs §5.7:394-396)
§1.1 states "every no-regression clause binds to the research/04 load-accounted baseline"; §5.7
repeats "to nothing else." But research/04 was captured on the CURRENT (pre-GW1) toolchain, and GW1
moves nightly→stable + edition 2024, which can shift the floor (the A3 channel confound the SPEC
itself cites, 94-95). Whether GW2+ compares against raw research/04 or research/04-adjusted-by-GW1's
measured channel ratio is not spelled out as the binding mechanism. §5.7's "a wave that changes
toolchain/code takes its own paired before/after" implies self-paired proof anchored to a moving
floor — reconcile the two framings in one sentence (research/04 = the standing anchor; each wave's
regression PROOF = its own before/after against the prior wave's end-state).

### R3 — §2.1's executable sequence removes the distillation source before the distillation step
Step 4 `git rm`s `restart/` (158); step 6 authors the durable docs distilled FROM `restart/`'s
41-report corpus (168). The design intends tag-reads (§4:349 "stays git-resolvable behind its
tags"), which works — but the sequence never says GW0.e authors via `git show <tag>:`, and a
careful executor reaching step 6 finds an emptied tree. Sharper: sk-v22's OWN tranche dir — the
declared "first resident" of `docs/tranches/` (186) — lives UNDER the `restart/` removed at step 4,
so it must be copied out (by tag-read or before step 4). Make the tag-read explicit and order the
sk-v22 copy before the removal.

### R4 — EVIDENCE-INDEX coordinate format will not resolve as written
The §4 example coordinates are shortened (`sk-v21/research/35:83`, 220/363), but the archived path
is `restart/skinny/tranches/sk-v21/research/35-deferred-fold.md`. `git show <tag>:path` needs the
FULL tag-relative path or every index lookup 404s. An authoring-precision constraint for GW0.e —
state that index coordinates are full tag-relative paths.

### R5 — The differential-oracle reassurance is unstated (and needs one pre-removal check)
§6.2 inverts skinny from "differential oracle + reference only" to worksite, but never says what
becomes of the oracle's REFERENCE side once legacy `crates/` archives. The reassurance exists — the
oracles reference EXTERNAL SOTA (sonic-rs, serde_json, lightningcss, cssparser — research/04 lanes;
`parity_oracle.rs` is "skinny JSON vs serde_json", 03:176), which survive; legacy `core` was never
the parity reference — but the SPEC leaves a reader to worry. State it, and add one pre-GW0.c check:
"no oracle/parity/test cross-references the archiving legacy tree" (the GW0 "oracles green" gate
catches it post-hoc, but enumerate it up front given the removal is irreversible-in-working-tree).

### R6 — lsp/vm/gorgeous re-bind onto a dead host wave (soft, acceptable)
ADDENDUM:30-31 tied the lsp/VM/debug + gorgeous KEEP-DORMANT re-bind to TW-P; §6.5 supersedes TW-P
(playground archives, future shopwindow greenfield). §1.3 re-homes their re-bind to "future, named"
(78) — a to-be-named wave, marginally softer than the `no-deferrals` precept's "named wave." These
are explicitly out of the V1 graduation scope (dormant-in-archive), so it is a defensible deferral;
flagged only so synthesis notes the host wave is now undefined, not TW-P.

---

## Score

**82/100.** The re-map is coherent (no orphaned wave, no gate on a dead tree once A1 is decided),
the port-in is minimal-and-sufficient, the archival destroys nothing load-bearing (the one
ahead-commit is tagged, the oracle references survive, the index is tag-gate-protected), and the
maturation order is genuinely baseline-driven and well-argued. Held below 90 by two design-level
residuals: one unresolved home-path design question with cross-repo blast radius (A1, which
research/07 explicitly deferred to synthesis and synthesis did not close), and one coherence hole
where GW0's marquee leverage deliverable is assigned to a sub-wave its own close gate never checks
(A2). R1's GW2 bundle is the top right-sizing concern. All residuals are closable with named
wave-owned edits; none reopen a frozen architecture decision.
