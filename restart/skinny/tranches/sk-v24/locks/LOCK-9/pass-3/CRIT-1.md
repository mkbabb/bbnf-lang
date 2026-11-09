# LOCK-9 · pass-3 · CRIT-1 (ADVERSARIAL CRITIC 1 of 2 — within-lock view)

*Charge: internal soundness of each family spec + whether the pass-3 prototypes tested the sharpest
uncertainty. Substrate re-verified live this session against the failure-mode checklist. Convergence
% + exact enumerated gaps per family.*

## Substrate independently re-verified (the spec is grounded, not self-citing)

I re-ran the load-bearing greps against the live trees. **All four confirm the spec exactly** — this
is a rigorous, substrate-anchored synthesis, not a spec-cites-itself artifact:

- 4 double-`vaddv` sites at *exactly* the named lines: `bracket_depth_mask_64.rs:78-79`,
  `comment_body_mask_64.rs:72-73`, `structural_classify_64.rs:151-152`,
  `byte_class_from_eq_set_64.rs:83-84`. ✓
- `vpaddq_u8` absent from `bbnf-simd/src/aarch64/` (grep -c → zero counts). ✓ reconciliation 3 holds.
- `lo6_table_admissible` at `dispatch.rs:101`, `select_classifier` at `:42`. ✓
- `parse-that/rust/regex` has `automata/{dfa,nfa,accel}.rs` + `sets/equiv.rs`; skinny
  `crates/parse-that-regex` carries ONLY `number/ unicode/ integration/ lib.rs` — **no `automata/`,
  no `equiv`**. ✓ reconciliation 5's central "engine not vendored" claim is TRUE.
- skinny `crates/bbnf-regex` = 322-LOC `lib.rs` stub. ✓  parse-that `regex/examples/` does not exist. ✓

Constitution vii is well-honored on the perf axis: PROTO-1 labels the CSS ratio SPEC_ONLY under load;
PROTO-2/2b make no perf claim. No SPEC_ONLY-anchored perf claim detected. The critique below is about
**internal soundness and gap-accounting**, not fabrication.

---

## THE HEADLINE STRUCTURAL FINDING — the pass-3 FAMILY-SPECS is STALE against its own pass-3 prototypes

The exact sin the synth accused the pass-2 spec of ("the pass-2 FAMILY-SPECS was stale against its own
protos", §Reconciliations) is **repeated in pass-3.** Mtimes are dispositive:

- `FAMILY-SPECS.md` = **22:44**
- `PROTO-1.md` (dense64 author + 4-site purge + **the evolved-home divergence TABLE**) = **23:23**
- `PROTO-2b.md` (**`byte_classes`-DIRECT emit == oracle**, closing PROTO-2's sole residual) = **23:29**

The spec was frozen ~40–45 min BEFORE its two most consequential prototypes finished. Consequences the
gap enumerations do not reflect:

1. **9A Q1 / gate-5 says "the evolved-home residual divergence — the LOCK-6 datum neither pass
   produced."** FALSE as of 23:23: PROTO-1 authored both canonical producers into a *scratch evolved*
   bbnf-simd, purged all four `vaddv` sites (build green, `rg vaddv` clean, `vpaddq` present ×4), and
   **emitted the LOC/substrate divergence table** (nightly↔stable, PaddedView↔clamp, per-16↔64B,
   oracle-present↔absent). The datum "neither pass produced" **was produced.** The gap is stale.
2. **The "TWO SHARPEST UNCERTAINTIES → pass-4 briefs" section is retroactively half-executed.** Brief 1
   half (B) ran (PROTO-1); Brief 2 ran twice (PROTO-2 find_at + PROTO-2b byte_classes-direct). Briefing
   them as *future* pass-4 work while the same directory holds their PASS results is an internal
   contradiction the doc never reconciles.
3. **9C Q2 "the emit pass is unbuilt … `byte_classes`-direct is the mechanical next step"** — PROTO-2b
   *ran that step* and measured `direct == oracle` on all three leaves. The gap wording predates its
   own closure.

Net: the enumerated gaps list several **prototype-closed** items as open, and the percentages are the
synth's *pre-brief-execution* estimate. A fresh non-author audit (PORTFOLIO 100% criterion) cannot
pass while the gap ledger and the prototype ledger disagree by this much. **The FAMILY-SPECS must be
re-cut against PROTO-1/2/2b before any convergence claim advances.**

---

## 9A — ONE-KERNEL-LIBRARY · synth ~52% · **CRIT-1: ~50%**

Strong where it is concrete (producer authored, bit-identical 0/200k, 8 `addv`→7 `uzp/orr`, four sites
purged, build green — all verified in PROTO-1). Value honestly stated as maintenance/drift, not speed.
Open gaps:

1. **[stale-gap] The divergence TABLE is delivered (PROTO-1, 23:23); the spec still lists it open
   (Q1).** What actually remains of Q1 is only (a) the CSS *perf ratio* (unrun) and (b) LOCK-6 rendering
   the verdict (cross-lock, uncloseable within LOCK-9). Re-word the gap; do not carry the whole datum
   as open.
2. **[gate-that-cannot-fail] Gate (2) "paired cold CSS ≥0.98×" is near-vacuous BY THE SPEC'S OWN
   LOGIC.** The doc asserts the axis "WASHES by law (§reconciliation 1)" — but "wash by law" is a
   code-structure argument (OnceLock free at cgu=1 + author-once), NOT a measured throughput
   equivalence. If it is a wash by law, gate (2) can only pass; a *botched* port fails the checkasm
   PARITY gate, not this perf gate. Gate (2) therefore tests nothing gate (5) doesn't already catch.
   Either it is genuinely measurable (then run it — 2 passes with zero CSS timing) or it is a wash
   (then delete it). The spec wants both: it excuses the missing measurement AS a wash, then lists the
   missing measurement as a born-RED gap. Unresolved tension.
3. **[spec-internal contradiction, P23] Is the one-line `[patch]` seam a P23 mutation or not?**
   research-A §4 says the seam lives "under the **gitignored** patch table" (i.e. NOT tree-of-record).
   FAMILY-SPECS Q3 calls it "9A's one line **mutates parse-that's read-only tree**" and flags a
   named-not-granted supersession. A gitignored build-config entry is not a source mutation. The two
   documents disagree on whether Q3 is even a real P23 obligation. Resolve: is `.cargo/config.toml`
   in-tree or gitignored, and does P23 reach it?
4. **[SPEC-ONLY, correctly flagged] Q4** `vector/{mod,neon,scalar}.rs` module absent — promoting
   refuted code needs its own no-regression artifact. Fair.
5. **[ambiguous artifact ref] Gate (4) and 9B Q1 cite "PROTO-2 measured against the un-evolved crate."**
   In pass-3, PROTO-2 = the *9C emit probe*; the LEAN/DENSE divergence was *pass-2*'s PROTO-2. A reader
   cannot tell which PROTO-2. Citation hygiene: disambiguate pass-2-PROTO-2 from pass-3-PROTO-2.

**Why not lower:** the physical mechanism is measured-real (producer built, purge green, seam
CONFIRMED by the 20:11 probe). The ~50% reflects that the LOCK's *central* question (who owns the
kernels) is 0% resolved within-lock and structurally deferred to LOCK-6 — the family % measures
*family-internal readiness*, not lock decision.

---

## 9B — NATIVE-REBIRTH · synth ~62% · **CRIT-1: ~58%**

The only family off SPEC-ONLY; LEAN+DENSE banked at pass-2. Open gaps:

1. **[research overclaim vs spec] Stable cutover UNVERIFIED — and research-B contradicts the spec.**
   research-B §5 asserts "Every scanner + the shim compiles on **stable 1.96.1** … zero errors in the
   12 touched files" — an unlabeled feasibility claim with **no build-log artifact**. FAMILY-SPECS Q3
   correctly downgrades it: "the headline (off nightly) is UNVERIFIED; the rebirths were authored *on*
   the nightly they claim to shed." The research overclaimed; the `cargo +stable build` gate (6) is
   unrun. This is the single sharpest 9B gap and it is a bare assertion today (SPEC_ONLY).
2. **[stale-gap, shared with 9A] Gate 5 "against the evolved home — SEEDED not closed."** The evolved
   divergence *table* was delivered by PROTO-1 (23:23) — it is the SAME axis datum serving both
   families. The residual for 9B is not "produce the table" (done) but the CSS perf ratio + LOCK-6's
   call. Re-word.
3. **[unbuilt oracle] Q2 DENSE bit-parity oracle unbuilt.** DENSE correctness is still "parsed without
   panic," not bit-parity, natively. research-B §5 describes the harness template but it is owed.
   Genuine.
4. **[P23 asymmetry — spec-internal inconsistency] 9B's mutation surface dwarfs 9A's but carries NO
   P23 flag.** 9A's *one-line* patch gets a named-not-granted supersession (Q3); 9B *rewrites 8 scanner
   files + deletes the number_simd SIMD surface (~200 LOC) + purges x86 in-place in the READ-ONLY
   parse-that tree* — and no P23 obligation is stated for it. Either 9B's rebirth is pre-authorized by
   the PT-2 codex (then say so and drop 9A Q3 symmetrically) or it needs the same named supersession
   9A gets. The asymmetry is unexplained.

**Why not higher:** the taxonomy is settled and the intrinsics are stable-since-1.61, but the *headline
value* (off-nightly stability) is literally unverified and the correctness oracle for the banked DENSE
win is unbuilt. 62% overstates; the load-bearing headline is untested.

---

## 9C — SCANNER-AS-DFA · synth ~46–48% · **CRIT-1: ~46%**

PROTO-2 + PROTO-2b are the strongest concrete work in the lock: emit feasible against the *real*
bbnf-regex DFA, `byte_classes`-direct == find_at oracle on all three leaves, gate-4 `;`/`{` lo6
collision provably routed off `vqtbl4q` with no silent drop, INJECT_BUG 100%. The `\p{L}` correction
(41 classes / 21 states, does NOT return `None`) is honest measurement over the spec's own prediction.
Open gaps:

1. **[SHARPEST UNCERTAINTY NOT TESTED — critic-1 headline for 9C] The prototypes tested MEMBERSHIP
   emit, not the STRUCTURAL (multi-class vceqq-fan) emit.** All three PROTO-2/2b leaves compiled to
   **`num_classes = 2`** (verified: grep "2 / 2" → 3 hits) — a binary member/non-member split. The
   "json_structural" leaf did NOT exercise per-*kind* classification; it collapsed to binary
   membership. PROTO-2b §boundary concedes this: a multi-class partition "would exercise the
   `structural_classify_64` `vceqq`-fan path, which this short probe **routes-to-FullTable but does not
   separately fan-verify**." PROTO-2's claimed "vceqq-fan cross-check 0 mismatches" is therefore
   **degenerate** — verified on a binary set, not a genuine multi-kind structural set. The *harder half*
   of 9C's thesis (DFA multi-class → vceqq-fan emit reproduces the hand `structural_classify`) remains
   feasibility-untested. The sharpest 9C uncertainty was skipped for the tractable membership one.
2. **[MEASURED-label misuse] "Tier S … MEASURED NEGATIVE" is NOT measured this pass.** The serial
   `vqtbl1q` chain negative is anchored on (i) the *directive's* assertion and (ii) `ws_comment`
   run-length stats (≤4 bytes 84–100%) — an inference, not a pass-3 head-to-head. research-C §7 item 4
   still lists "Serial-vqtbl negative, *reproduced*" as a prototype-MUST-test (i.e. unrun). Downgrade
   "MEASURED NEGATIVE" → SPEC_ONLY/carried until the serial-vqtbl-vs-scalar head-to-head runs.
3. **[unreconciled census] The scanner-file trichotomy DISAGREES across researchers.** research-A: 4
   kernel / 3 vector / 2 no-go. research-B: 4 kernel / 3 vector / 1 no-go. research-C: **2** 1:1-kernel
   / 2 vector / **4** no-go. FAMILY-SPECS §9C picks "~3 files fully dissolve / ~4 stay carry-stateful"
   without reconciling research-C's own "2 dissolve" self-count. The count of files that *dissolve* is
   not settled — a within-lock census owed.
4. **[residual, correctly RED] Q2 production emit pass + separability solver unbuilt IN-TREE.**
   PROTO-2/2b prove feasibility in a *scratch* `parse-that/rust/regex/examples/` file — not skinny, not
   a codegen home. Genuinely decouples feasibility from vendoring (real advance), but production
   remains gated on LOCK-5 vendoring (un-executed, verified), LOCK-2 tier island, GN-codex codegen home.
   Three live cross-lock blockers. ~46% is honest.

**Not equal-difficulty (so BLOCKED-candidate not hard-BLOCKED) is correctly argued** — the engine
exists+green, `lo6_table_admissible` is a shipped half. Sound disposition.

---

## 9D — LAZY-SURFACE · **RETIRE** · disposition soundness ~85%

The retirement is well-argued and the memory-center refutation is grep-solid (research-D §0.1 consumer
grep = zero; span-dangle wall; PaddedView anti-chunking; §0/51). RETIRE is the correct disposition.
Residual gaps in the *disposition*, not the decision:

1. **[unmeasured, 3rd consecutive pass] P3 (the rewind histogram) never ran.** The backtrack claim
   (JSON P50=P99=0, CSS ≤ one rule) is by-argument only. Defensible under the directive (a refutation
   upgrade is low-advance), and the RETIRE rests on the memory-center refutation, NOT the histogram —
   so the unrun P3 does not undermine RETIRE. But it means 9D exits on an unmeasured backtrack
   prediction; note it as carried, never "measured."
2. **[consumer-gated residues hand-off, unmeasured] Residue (a) cost-model kernel-selection HAS a live
   consumer** (research-D §1: every `take_until`/`leaf.rs:177` site, Y.13 satisfied). Retiring the
   whole family while re-homing a consumer-bearing sub-plane to LOCK-2 is a disposition choice, not an
   error — but residue (a) is arguably a standalone advanceable item, not merely a LOCK-2 hand-off.
   Residue (b) carry-threading for 9C⊗9D survives with zero measured artifact. Both fold to
   LOCK-2/gestalt-Ω unmeasured — acceptable for a RETIRE, flagged for the gestalt.

---

## CROSS-CUTTING (constitution + within-lock soundness)

- **[vacuous-convergence guard] The LOCK's central adjudication (9A⊗9B kernel-home) is 0% resolved
  within-lock and cannot be, by design.** Both families carry ~50–58% "convergence" while the primary
  question they exist to decide is entirely deferred to LOCK-6/GESTALT-P. The percentages are legitimate
  as *family-internal readiness* but must NOT be read as "the lock is ~half-decided" — the lock's
  decision is 0% and the DAG hand-off is the honest close. State this explicitly in the roll-up so the
  gestalt does not mistake family-% for axis-%.
- **[prototypes-vs-sharpest] Pattern across 9A and 9C: the prototypes ran the FEASIBLE half and deferred
  the SHARPEST half.** 9A ran author+purge+static-census (real) but deferred the CSS perf ratio as
  "wash by law" (loadavg-blocked, 2 passes). 9C ran membership emit (real) but deferred the multi-class
  structural emit (all leaves num_classes=2). In both, the un-run half is the one that could actually
  falsify the family. This is not fabrication — it is scope honestly stated — but a fresh audit should
  weight that the discriminating measurements remain owed.
- **[grounding — PASS] No spec-cites-itself circularity, no legacy-alias, no masked fallback detected.**
  Reconciliations are grep-grounded (I re-verified 5 of 6). The gate-4 collision proof is genuine, not
  a gate-that-cannot-fail (the naive lo6 form provably misclassifies; the separability gate provably
  routes off). 9C's "not equal-difficulty" argument is sound. The x86-dead / no-digit-scan / double-
  vaddv-barred laws are correctly carried and enforced as blocking gates.

## VERDICT

The lock is honest, substrate-grounded, and correctly refuses to force the 9A⊗9B verdict LOCK-6 owns.
Its two defects are **(1) a stale gap ledger** — the FAMILY-SPECS (22:44) predates PROTO-1 (23:23) and
PROTO-2b (23:29) and lists prototype-closed items (9A divergence table, 9C byte_classes-direct) as open
while briefing already-run work as pass-4 future — and **(2) two discriminating measurements still
owed**: 9A/9B's CSS perf ratio (2 passes, zero timing, excused as "wash by law" via a near-vacuous
gate) and 9C's multi-class STRUCTURAL emit (untested; all probe leaves were binary num_classes=2).
Plus the P23 asymmetry (9B's large in-place mutation unflagged while 9A's one line is) and the
unreconciled scanner-file census. **Recommendation: re-cut the gap ledger against PROTO-1/2/2b, resolve
the gate-(2) wash-vs-testable tension, and carry the two owed measurements as the LOCK-9 inputs GESTALT-P
must have before the kernel-home axis can close.** No family is at convergence; 9B leads on readiness,
9C on concrete prototype delivery, 9A on the (now-delivered) LOCK-6 divergence datum.
