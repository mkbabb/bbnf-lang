# CRIT-honesty — SK-V22 SPEC-DRAFT adversarial read (HONESTY + EVIDENCE lens)

Date 2026-07-09. Reviewer: critique agent (conservative). Scope: every number in the draft cold?
ratio-vs-directional labeled? loadavg-cited? Does the graduation baseline (04) support the
maturation-order claims? Any SK-V21 settled fact misquoted (canada sign, vshrn, AoS reversal,
N*=3)? Does the archive manifest overstate reclaim / understate risk?

Read: SPEC-DRAFT.md end-to-end; research/01-07; cross-checked against the cited file:line. Session
loadavg 13.41 (5-day uptime) — no cargo run; all findings are static evidence reconciliation.

**Convergence: 87/100.** The draft is executable: it *resolves* every open question raised by
01-07 (branch-vs-tag, grammar reconciliation, data provenance; 05's A/B/C; 07's SIMD-path / egraph
home / wave letters; 06's asks), ratifies the 02 verdicts, binds every gate to the 04 baseline,
and carries a tight supersession statement. Zero *architecture* questions remain open. The
residuals are honesty-polish and one documentation-provenance conflict — all wave-ownable. What
keeps it off 95 is finding H1 (a 32% number conflict that lands inside the EVIDENCE-INDEX's core
traceability promise) and a cluster of small mislabelings the honesty lens exists to catch.

---

## The evidence holds where it matters most

Before the findings, the load-bearing checks that PASSED, because a conservative score must credit
them:

- **The headline JSON ratios are cold, certified, and correctly labeled.** Draft §4/§1.4 quote
  `2.10×/1.41×/2.06× vs sonic`. Verified against 04:82-93 PMU cyc/byte (the certified column):
  twitter 3.371/1.607=2.098, canada 2.930/2.081=1.408, citm 2.347/1.138=2.062. Exact. The draft
  labels them "vs sonic **cyc/byte**" (§4) — the load-robust metric, not wall. Correct.
- **canada sign is quoted correctly.** Draft §1.2/§1.5/§5 all state canada fused/sonic LOSES
  (1.20× slower, +1.48 cyc/B float tax localised to `materialize_f64`). Matches 04:128 (paired
  mean 1.199, 0.83×, "fused LOSES", CI excludes 1.0 all three windows) and 04:149,158. GW3's gate
  ("canada eager beats sonic") is correctly framed as a forward target, not a claimed result.
- **AoS-beats-SoA and N*=3 are faithful.** NEGATIVE-RESULTS §1 = 03 NR-1 ("the most important
  architectural reversal of Era VI"). Fleet stated as 3 (json/sheets/css_l4), matching 02:90; TW6
  "re-baselines from 3 toward ten." No inflation to the phantom 9/10.
- **Wall/Mbps are labeled DIRECTIONAL; paired ratios labeled CERTIFIED.** §5 items 1/5 and the 04
  reproduction discipline are carried verbatim. loadavg is cited per claim (§5: "every SK-V21
  session 8-25, SPEC.md:22-24; the 04 session 8.6-14.0" — matches 04:37).
- **Archive reclaim does NOT overstate.** §2.2's "≈202G, ~186G build churn" is 01 §8 verbatim;
  the 1.7G `.git` retention is disclosed (no history rewrite); the threaded-fork +102-line skim
  and the `data/` provenance gate are both surfaced as named pre-removal steps (§2.1). GW0 even
  adds falsifier gates the research did not demand: each named tag must resolve via
  `git show <tag>:path`, and Lock-14 must FIRE on a planted phantom before green.

---

## Findings, ranked

### H1 (should-fix-before-freeze) — the twitter-sonic ratio conflict lands inside the EVIDENCE-INDEX promise

The graduated `PERFORMANCE-THESIS` is distilled from TWO sources that DISAGREE on twitter, and the
draft does not reconcile them:

- research/03:173 (the distill spec the draft ratifies in §4) says the live-lane ratios are
  **1.59 / 1.42 / 2.14× vs sonic**, anchored to `33:59,89`. 03's own EVIDENCE-INDEX example row
  hard-codes `PT-1 | JSON 1.59× vs sonic-rs cold | … | sk-v21/research/33:59`.
- research/04:112 (which the draft §4 says "supplies the quoted numbers") says
  **2.10 / 1.41 / 2.06×**. canada agrees (1.42≈1.41); **twitter jumps 1.59×→2.10× (+32%)** and
  citm drifts 2.14×→2.06×.

Draft SPEC-DRAFT.md:356 lists BOTH `research/33` as a distill source AND "research/04 … supplies
the quoted numbers: 2.10×". So the durable doc will assert twitter 2.10× while one of its cited
anchors (33:59) says 1.59×. Per the draft's own standing rule (§4: "a graduated claim with no
index row is unsupported"; "coordinates resolve forever"), an index row whose anchor contradicts
its claim by 32% is self-falsifying — it breaks the exact traceability the branch-archival strategy
exists to buy.

Root cause is in 04, not corrected by the draft: 04's twitter **sonic** baseline is 3.371 cyc/B,
materially higher than report-33's implied ~2.48 (2.48/1.559≈1.59×). 04 self-validates its own
product lanes (`parse_tape` 1.607 vs report-33 1.559, +3%) and cross-checks citm sonic (2.347 vs
A1 2.2234, +5.6%) — but it NEVER cross-validates the twitter sonic figure that drives the headline
jump. The 36% shift in the twitter sonic baseline between report-33 and 04 is undisclosed. The draft
inherits it silently. Fix: the PERFORMANCE-THESIS must anchor 2.10× to 04 alone (not 33), and carry
a one-line reconciliation note that the sonic baseline re-measured higher at HEAD; the EVIDENCE-INDEX
PT row must not cite 33:59 for a 2.10× claim.

### H2 (honesty, minor) — "hand-asm obviated by vshrn" inverts the settled fact

Draft §4 NEGATIVE-RESULTS list (SPEC-DRAFT.md:357): "hand-asm obviated by `vshrn`". The charter
named vshrn as a fact to check for misquote — and this is one. 03 NR-5 (research/03:204-205): "Hand-asm
is obviated by **well-shaped Rust** + vshrn… durable wins are confined to idioms the compiler
cannot synthesize (**the shrn/vshrn movemask**)". i.e. well-shaped Rust is the obviator; vshrn is
the LONE EXCEPTION where hand-guidance survives (the single-movemask co-gate, Lock-16). The draft's
compression reads vshrn AS the obviator — inverting its role from survivor to eliminator. Small, but
it is a misquote of a named settled fact, and the compressed list entry is what a reader skims. Fix:
"hand-asm obviated by well-shaped Rust — vshrn the lone residual hand-win."

### H3 (honesty, minor) — 04's "within 2-5%" self-reproduction is quoted slightly optimistically

Draft §1.4.1 and §5 carry "reproduces A1/report-33 within 2-5% (04:29-33)". The band is really
~1-6%: twitter parse_only +1.1% (04:31), canada parse_tape +2.1%, **citm sonic +5.6%** (2.347 vs
2.2234, 04:32) — the last exceeds the "5%" ceiling 04 itself also states (04:42). Honest framing:
product lanes within ~3%; third-party baselines within ~6%. Not blocking (the product-lane
validation is what the parity gates rely on), but the quoted ceiling is 0.6 pt tight.

### H4 (evidence-attribution, minor) — §1.4 over-credits 04 as "the ordering instrument"

§1.4 opens "The rationale is the profiling baseline (research/04), read as an ordering instrument"
and then lists four ordering points. Only point 1 (hot path frozen/certified → cheap parity gates
for a pure move + toolchain change) actually draws on 04 (04:26-33). Point 2 rests on the A3 channel
confound (SPEC.md:31-32), point 3 on PROTO-A3/N26 (build-infra-first precept), point 4 on the
coupling map (skinny/Cargo.toml:36-37, xtask, regen_css, 07:49,80). The reasoning is sound and each
point cites its true source inline — but the framing overstates 04's role: 04 grounds 1 of 4. The
answer to the charter's question "does 04 support the maturation ordering?" is: it supports the
*premise* of ordering point 1; it is silent on 2-4, which the draft rests (correctly) on other
evidence. Re-header as "the frozen-hot-path finding (04) plus the coupling map (02/07) and the A3
confound (SPEC)".

### H5 (residual under-decision, minor) — egraph/csp home vs the general-infra-crates precept

07 open-Q and 03:44 leave the egraph/egraph-derive home a live choice (own repo / general-infra
sub-tree / published pin). The draft §1.3/§2.3 decides "graduated workspace members" and lists
`crates/` as "graduated implementation + ported seams" — implying egraph lands in the domain
`crates/` tree. That is in mild tension with the memory precept `general-infra-crates` (e-graphs
live in own crate(s), not stuffed into domain crates), which 02:154 itself flags ("or general-infra
sub-tree per the precept"). And because parse-that ALSO resolves egraph across the seam (PT-R.5),
the durability question path-dep-vs-published-pin is left as "workspace-relative paths / published
pins" (§3.2 PT-R.5) — an either/or, not a fix. Wave-ownable (GW0.b), but it is the one place the
draft ratifies a location without closing the precept tension.

### H6 (terminology, cosmetic) — the 322-LOC bbnf-regex is both "classifier folds upstream" and "stub deletes"

Draft §3.2(B): "skinny's 322-LOC **classifier** folds UPSTREAM … skinny's **stub** crate deletes".
05:188-191 and 03:60-64 establish it is a functional 322-LOC family recognizer (RegexKind,
ByteSet256, FirstSet) — NOT a stub; 02:25's "stub … DELETE target" framing understates it. The
draft's plan is coherent (fold the logic up, delete the now-redundant local crate) but carries both
characterizations of the same crate in one sentence. Harmless to execution; a reader could think two
crates are meant. Cosmetic.

---

## On the archive manifest: risk is disclosed, not understated

Checked specifically for reclaim overstatement / risk understatement (charter question). The manifest
is honest: reclaim matches 01 exactly; the 1.7G history retention is stated as deliberate; the two
genuine data-loss risks (threaded-fork dirty diff, `data/` corpus trim) are each gated behind a named
enumeration step before any `git rm`/`rm -rf`; the one non-reachable worktree commit (`f2fe49bbc`) is
tagged before removal; GW0's close gate independently verifies every tag resolves. No overstatement
found. The only latent risk not called out: the archival's `crates/` removal is hard-blocked on TWO
independent cross-repo repoints (PT-R.5 for parse-that AND skinny's own egraph/csp vendor, S-3/R-7)
landing first — the draft sequences both (§3.4, §2.1 step 3-4) but a mis-order bricks two trees'
builds at once. It is correctly encoded as a hard edge; flagging only that it is the single highest-
consequence ordering constraint in GW0 and deserves the most explicit GW0.b gate wording.

## Verdict

No blocking design question survives — the draft is executable tomorrow on architecture. The gap to a
higher score is honesty-hygiene: H1 (the 2.10× vs 1.59× twitter conflict) must be reconciled before
the PERFORMANCE-THESIS/EVIDENCE-INDEX are authored (GW0.e), or the traceability guarantee ships
already violated; H2-H4 are one-line corrections. Recommend: fold H1 as a GW0.e precondition, fix
H2-H4 in the draft text, note H5/H6 to the owning waves. Convergence 87.
