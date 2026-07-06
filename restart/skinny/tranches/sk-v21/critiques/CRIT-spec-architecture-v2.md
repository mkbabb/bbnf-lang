# CRIT — SPEC-DRAFT-P1 architecture, pass-2 re-critique

Target: `restart/skinny/tranches/sk-v21/SPEC-DRAFT-P1.md` (545 lines, pass-1-amended), read
end-to-end against its predecessor `critiques/CRIT-spec-architecture.md` (64/100, B1-B5 + R1-R7).
New evidence: `prototypes/PROTO-A1..A3`, `PROTO-B1`, `PROTO-B3`, `research/38-tw1-seam-disposition.md`.
This pass judges whether the amendments closed the five blockers, whether pass-2 measurement
vindicates or overturns the amended architecture, and whether any NEW architectural defect surfaced.

## The pass-1 blockers are closed — verified against the amended text

- **B1 (TW1 was four waves in one letter): CLOSED.** §11 unbundles into `TW1a` (split) → `TW1b`
  (AoS tape + eager-builder retirement) → `TW1c` (rename + reclaim); the §13.3 self-defer is now
  discharged by `research/38`, which adjudicates split-first as SOUND (no runtime↔codegen cycle:
  codegen emits identifier *text*, only generated parsers link the tape — 38:96-105) and correctly
  fuses eager-builder retirement INTO the AoS hardening as one substrate landing (38:126-132).
- **B2 (TW3∥TW4 hid a number-decode dependency): CLOSED.** §11 splits TW4 into `TW4.1`
  (Eisel-Lemire + cost-table, lands BEFORE TW3) and `TW4.2` (parallel to TW3); the coupling is now
  named in §4 (line 215) and §11 (lines 428-432). PROTO-A1 D4 confirms the coupling is real and
  canada-localized (fused canada 3.325 cyc/B = 2.0× the AU 1.63 floor, the float tax — A1:154-167).
- **B3 (AU-vs-TW0 floor tension): CLOSED.** §2 now makes the TW0 cold baseline "the single governing
  JSON floor… binds to it and to nothing else" (lines 82-85); AU is demoted to DIRECTIONAL context.
- **B4 (highest-ROI lever unfalsified): PARTIALLY closed — see NEW-B1.** The fused *core* claim is
  now triple-sourced cold (report 33 + P2 + PROTO-A1) and canada's loss is carried honestly (§2
  lines 90-94). But pass-2 falsified the specific construct §4 names as the default.
- **B5 (D-G reconcile unexplained): CLOSED.** §11 TW0 now defines it executably (lines 435-445:
  REDRESS 51/53/246/247, their family meanings, the freeze tag, the supersession pointer).

Refinements: R1/R2 closed (§1.1 now admits two gate classes; structural locks explicitly distinct —
lines 48-55). R4 closed (§3.3 + §11: TW0 sole owner of Lock-15/target-cpu). R5 closed (§5.4:
checkasm right-sized to ~4-6 kernels, one ISA). R6 closed (§6: steps 4-6 explicitly DEFERRED). R7
closed (§5.1: one production `perf_cost` site, two more deleted with the CSP stage). **R3 (TW0
within-wave sprawl, ~14 workstreams) is UNADDRESSED** — TW0 still bundles the entire hygiene surface
in one letter (§11 lines 434-459); tolerable because it is a gate/hygiene wave with a clean
first-commit anchor, but the god-module-at-wave-granularity shape the predecessor flagged persists.

## Blocking — the one architectural defect pass-2 surfaced

**NEW-B1. The value plane's centerpiece construct is now benched and REFUTED, and the spec is
internally contradictory about which lane is the default.** §4 (line 191) states the design fact:
"Eager is a generic fold over the lazy cursor — one substrate," and §2 (line 96-97) calls
fold-over-lazy-cursor "the spec's actual default construct… an OPEN TW3 sub-gate, unbenched." PROTO-A1
Deliverable 3 ran exactly the bench §4:194 demanded (fold-over-cursor vs sonic-rs dual-path
direct-to-struct) and it LOSES on all three corpora — 2.32×/4.85×/2.27× slower than sonic, and
**3.87×–4.23× slower than its own fused sink** (A1:124-145); at the AU anchor it is 5.3×–8.4× the
1.63 floor, "categorically non-viable" (A1:170-171). The OPEN sub-gate closes NEGATIVE. The digest
and the prototype both say verbatim "SPEC §4 needs correction."

This is not a minor nit for three reasons:
1. **The spec names two different lanes as "the default."** §2 line 95 says the fused single-pass
   SINK is the default materialization lane; §2 line 96 and §4 line 191 say the fold-over-lazy-cursor
   is "the spec's actual default construct." PROTO-A1 proves these are NOT the same object and differ
   by ~4×. The sink (`track1_digest`, tapeless) wins; the fold-over-cursor (parse-to-tape then
   walk+re-scan) loses. TW3's headline close gate (§11 line 490-492) is written against the losing
   construct ("the fold-over-lazy-cursor benched… the §4 OPEN sub-gate discharged").
2. **The "one substrate / lazy + eager over ONE tape" elegance claim is empirically the slow path.**
   The charter aim (§0 point 4, line 28-29: "lazy + eager over ONE tape") and §4's "stronger claim
   than sonic-rs's dual path" (line 193-194) are both dented: the eager winner is *tapeless* and
   *distinct* from the lazy cursor — precisely sonic-rs's dual-path shape (A1:141-144). bbnf MATCHES
   that shape, it does not best it.
3. **The correction is bounded, which is why this caps rather than sinks the score.** The winning
   fallback — eager = fused direct sink, lazy = separate cursor over the tape — is *already* what §2
   line 95 names and what wins twitter/citm cold (A1:108). The fix is: strike "eager is a fold over
   the lazy cursor — one substrate" from §4, retract the "stronger than sonic dual-path" sub-claim,
   restate the charter aim as "lazy over the tape; eager via a distinct single-pass sink — the
   dual-path shape matched, not bested," and rewrite the TW3 close gate to make the direct sink the
   default and record the fold-over-cursor sub-gate as resolved-negative. Parse speed is unharmed.

## Refinements — pass-2 corrections to carry before freeze

**NEW-R1. Strike the "one measured asm exception (~10%)" claim — PROTO-A2 overturns it.** §0 (line
26) and §5.4 (line 262) lean on P5's "~10% directional exception on the classify kernel" as the sole
evidence justifying the per-kernel asm gate. A2 prices door-(a): the intrinsic `vshrn` movemask beats
hand asm by ~25% (door-a +34.6/36.5% vs asm +6.7/10.4% over baseline — A2 P5 verdict), because the
binding resource is the SIMD→GPR CROSSING lever, not classify-ALU. **The asm buys nothing once the
intrinsic door is priced; the exception evaporates.** This STRENGTHENS the doors-first order (§5.4)
and vindicates §5.5's crossing invariant, but the specific evidentiary claim of a measured exception
must be struck — the honest statement is now "zero measured asm exceptions in the current kernel
census; door-(a) obviates the one candidate."

**NEW-R2. The `Sampled` DELETE is over-stated — carry the twitter nuance.** §2 (lines 102-104) says
`Sampled` "ties GrowOnly within drift, so removing… buys nothing measurable." PROTO-A1 D1's
interleaved-paired CI overturns the +9.3% canada artifact to a TIE (+1.25%, CI spans 0) but finds a
statistically real twitter win: **+3.45%, CI [+1.07,+5.83], excludes zero** (A1:52-71). DELETE of
`OneShotSimd`/`Exact`/NEON stands overwhelmingly (they pay 30-170% to reclaim ≤3.5%); but `Sampled`
keep-cheap-presize vs delete-for-KISS is a small real tradeoff on small inputs, not the flat null §2
asserts. Carry the nuance rather than the "buys nothing" framing.

**NEW-R3. The FLIP-3 movemask item in TW0 is misdirected — PROTO-A2 shows it's inapplicable to the
live path.** §5.1 (line 242-244) and §11 TW0 fold "the P4 FLIP-3 movemask unification" (vshrn vs
`vaddv_u8`) into TW0's honesty sweep. A2 finds the LIVE `crates/simd-scan/src/neon.rs:690`
`movemask_u8x16` ALREADY uses the cheaper `vaddv`; the vshrn/vaddv split lives in skinny `bbnf-simd`
and `json.rs:928`, not the totality classify ladder — and the movemask choice is genuinely
kernel-shape-dependent (vshrn cheaper for the no-space kernel, vaddv for multi/nibble). It is a
pluggable cost-model CELL, not a global TW0 unification. Re-scope the item accordingly.

**NEW-R4. §6 Step-0 cites the wrong root-cause loci — PROTO-B3 corrects them and proves the fix
DONE.** §6 (line 306-307) locates the width-destroying lowering at `value_expr/literals.rs:7,17` and
`expression/wrap.rs:589-606`. B3 traces the actual struct-direct path: the destroyed datum is the
*source span* of the typed numeric leaf — `runtime/bbnf/view.rs:141` returns `None` for every
scalar-payload leaf, so the suffix is unrecoverable; `literals.rs:7,17` is a real width-drop but on a
fallback path these grammars never take (B3:26-50). A 12-line helper in `wrap.rs` (recovering the
suffix from `parent_span_for_type`, wrap.rs:260) lands all 22 `Nu8` tags at U8, zero Span defaults
(B3 headline). Update §6 Step-0 coordinates; the direction is right and the fix is already proven.

**NEW-R5. WART C is a two-site fold, not a one-line deletion; pin the submodule.** §9 (line 395-396)
frames WART C as "delete the `has_rule('value')` name-gate." PROTO-B1 finds it a two-site
name-coupling requiring a JSON-into-`general_descent` fold, not a sniffer deletion (B1 confidence
note). B1 also flags worktree skew: `general_descent` exists only at skinny submodule `efc6b875f`
(17 commits ahead of `d2b32f6e3`); the §9 coordinates (`emit.rs:126-135`, `runtime_generator.rs:98`)
are valid only there. Pin the submodule commit in §9 so the TW6 work-list resolves. WALL A/B and the
7/7 `toml`-crate round-trip otherwise VINDICATE §9's "certifiable without per-grammar emitter files."

**NEW-R6. Annotate the TW1b/TW1c hit-counts as regen-owned.** §4 (line 182: "~1,080 hits") and §11
TW1c (line 471: "960 hits") read as hand-edit blast radii. `research/38` §1 shows core is 81.6% xtask
output; `StructBuilder` is 95.1% generated, `StructLayout` 88.5% — the true hand-edit surface is
~40-60 template/type sites (38:35-48, 140-143). Bind research-38's A1/A2 amendments (TW1a relocates
the generated tree as a byte-identical bulk payload; declare the counts regen artifacts) so no
reviewer re-bundles TW1 on a misread of the numbers.

**NEW-R7. TW1b's gate cannot meet §1.1's "≥3% win" clause — carry the restoration relaxation.**
§1.1 (line 52) makes every substrate wave close on "liveness AND a measured ≥3% win." `research/38`
§4 correctly notes TW1b RESTORES the AU AoS floor (SoA lost ground), so its honest gate is
liveness-AND-no-regression, not ≥3% (38:163-165). The spec should state this relaxation at the TW1b
gate, or the Y.13 template renders it unmeetable.

## Consumer-invariant gating, couplings, seam — the three charter questions

- **Wave bundling: resolved at the graph, one residue at the contents.** TW1→{a,b,c} and TW4→{4.1,4.2}
  are the right unbundlings; R3's TW0 stuffing remains.
- **Couplings carried: yes, all three.** TW3↔TW4.1 (canada/Eisel-Lemire, §4:215, §11:428-432),
  TW2↔TW4 D11 dispatch (§11:426-428), and cost-table→TW6 third-language flip (§5.1:234-239, confirmed
  by A2/P4-v2: no current-fleet flip, needs |S|≤2 = CSV).
- **Seam adjudicated: yes.** `research/38` settles TW1a/b/c: split-first survives, tape-first not
  forced, eager-builder retirement fused into TW1b, per-sub-wave gate classes assigned.
- **Consumer-invariant gates: the two-class model is now explicit and correct** (§1.1, R1/R2 closed),
  with one relaxation to carry (NEW-R7).

## Convergence

**82/100.** The amendment closed all five pass-1 blockers and six of seven refinements; pass-2
measurement broadly VINDICATES the amended architecture — the seam is adjudicated (split-first
survives), the TW4.1/4.2 unbundling and its coupling are proven correct, the doors-first asm order is
confirmed and the asm lane obviated, TOML is certifiable without per-grammar files, the stack is
decoupled from abrogation, and the width fix is done. One genuine architectural defect remains: the
value plane's centerpiece — "eager is a fold over the lazy cursor, one substrate" — is benched and
REFUTED (4× slower than the tapeless sink that actually wins), leaving §4 internally contradictory
about the default lane and the "lazy+eager over ONE tape" elegance claim standing-but-false. The fix
is a bounded §4/TW3 rewrite with the winning fallback already in hand, not a re-founding — but it must
land, alongside the four stale-claim corrections (asm exception, Sampled null, FLIP-3, Step-0
coordinates), before the plan freezes. One more short loop clears it.
