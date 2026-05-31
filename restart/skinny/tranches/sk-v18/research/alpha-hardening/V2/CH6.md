# CH6 — ANTI-PAPER-CLOSE (V2) — SK-V18 Pass-Alpha Adversarial Review (confirming cycle)

Lens: CH6 Next-Tranche-Impact / ANTI-PAPER-CLOSE. Per PASS-ALPHA §3 (CH6: "does the
SK-V{N+1} contract specify revert protocol per intervention? Hard caps? Triumvirate
discipline? Is the goalset measurable + verifiable from the bench gate?") + ORCHESTRATOR
§3W. Cycle V2 confirming pass over the αF V2 contract that FOLDED the V1 dispositions.

Reviewed: `sk-v18/research/alpha/{alphaA..alphaE}.md` + `SYNTHESIS.md` + `HANDOFF.md`.
(No `alphaF-*.md` artefact exists by that name — per PASS-ALPHA §2/§6 the α-F deliverable
IS `SYNTHESIS.md` + `HANDOFF.md`; both present at the tranche top level, both reviewed.
This is the correct structure, not a defect — confirmed against the V1 CONSOLIDATED §1.)

**Lens mandate (binding):** no wave deferred without a receiver + a gate; generalization
is concrete (`json_sink_direct` actually projects, CSS actually lowers, the generator
actually un-forks); the goalset is telemetry-bound + bench-verifiable; the honest-finding
escape is not a paper-close hatch; revert / hard-cap / triumvirate are specified or
contract-sanctioned-deferred with a measurable handoff.

**V1→V2 carry under this lens.** V1 CH6 disposed ACCEPT 7 / REVISE 4 / REJECT 0. The four
REVISEs (D-CH6-1 receiver under-bound; D-CH6-2 revert dependency graph not carried;
D-CH6-3 hard-cap defaults not carried; D-CH6-4 honest-finding escape ungated) were the
single root-cause cluster #7 in the V1 CONSOLIDATED ("deferred revert/cap as paper-close
surface"). The V2 fold-ledger (CONSOLIDATED §3 row 7) claims all four folded. This V2 pass
re-greps each fold site at HEAD and re-disposes every CH6-owned section. The threshold
question for V2: did the folds land as binding gate text in the artefact S-P3 consumes, or
as narrative gestures that re-open the paper-close read-path?

## Ground-truth re-verification (re-grepped at HEAD `318d9c046`)

Every premise the contract rests on confirmed live at the bracket HEAD:

- `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — STILL a verbatim
  `&str` literal (the verbatim-blob G2 targets). CONFIRMED.
- `grammar_provider.rs:40` `pub enum RuntimeEmitterKind`, branched `:110`
  (`!= RuntimeEmitterKind::RequestFacts`). CONFIRMED (G3 fork live).
- `bbnf-simd/src/x86_64/` = 24 files. CONFIRMED (P1).
- `tape/mod.rs:175` `pub struct ValueRef<'doc,'input:'doc, K = AnyKind, G: EventGrammar =
  AnyGrammar>` — the EXACT two-axis signature. CONFIRMED: `K` defaulted to `AnyKind`,
  `G` defaulted to `AnyGrammar`. The V2 fold's distinction (resolve the **`G` axis**, not
  the already-real `K` axis) is grounded in the literal source — material to §9 below.
- `json/generated.rs` `parse_w11_1_number` ×7. CONFIRMED (P5).
- `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS`. CONFIRMED (P4).
- 7 × `css_l4_*/generated.rs` all md5 `b654562ccff46ed62dd48e9ace325830` (byte-identical).
  CONFIRMED (P3 / distinct-grammar-output premise).
- `match grammar`-arm grep in `skinny/crates/codegen/src` → **0** at HEAD. CONFIRMED —
  and load-bearing: the canonical Lock-14 arm census ALREADY returns 0 while the
  `RuntimeEmitterKind` fork persists behind the abstract enum. This is the precise reason
  the V2 fold's "md5-distinctness + arm-grep are necessary-NOT-sufficient" framing is
  correct: BOTH already pass while the fork stands. The co-gate that actually bites G3 is
  `emitter_fork_present == false` (the `RuntimeEmitterKind` deletion grep), not the arm
  census. The contract carries both (SYNTHESIS §0.1 G3 + Section 2 columns) — verified.
- `grammar/google-sheets/google-sheets.bbnf` present (the real Pratt source, NOT in the
  benched skinny tree). CONFIRMED (PROVE source).
- `sheets_witness/` = 25 LOC. CONFIRMED (PROVE start state).
- `xtask/src/main.rs:59 "gate-json" => gate_json(...)` — the host command exists TODAY;
  the `--skv18-generalization-report` extension is a real, namable S-P3 obligation, not
  vapor. CONFIRMED — material to §4.

The ground truth is solid and unchanged from V1. My dispositions concern the *forward*
contract's paper-close surface: whether the V2 folds bind the deferred-wave gates into the
receiver/trajectory/handoff the next pass reads.

---

## Disposition summary (per reviewable section)

| # | Section | Path:line | V1 | V2 |
|---|---|---|---|---|
| 1 | SYNTHESIS §0.1 close-condition gate table | `SYNTHESIS.md:163-181` | ACCEPT | ACCEPT |
| 2 | SYNTHESIS §0.3 receiver goalset (G6 retire + PROVE fallback) | `SYNTHESIS.md:213-223` | REVISE | ACCEPT |
| 3 | SYNTHESIS §0.5 generalization litmus + fallbacks | `SYNTHESIS.md:289-311` | ACCEPT | ACCEPT |
| 4 | SYNTHESIS Section 2 telemetry binding + gate consumer | `SYNTHESIS.md:370-431` | ACCEPT | ACCEPT |
| 5 | SYNTHESIS Section 3 trajectory + revert dependency graph | `SYNTHESIS.md:433-485` | REVISE | ACCEPT |
| 6 | HANDOFF "Next Move" + revert/cap deferral carry | `HANDOFF.md:239-298` | REVISE | ACCEPT |
| 7 | alphaE candidate shortlist gates + sequencing | `alphaE:29-203` | ACCEPT | ACCEPT |
| 8 | alphaC PRUNE-wave close gates | `alphaC:81-175` | ACCEPT | ACCEPT |
| 9 | The honest-finding escape (anti-paper-close hatch) | `SYNTHESIS.md:181`; `HANDOFF.md:283-285`; `alphaE:197` | REVISE | ACCEPT |
| 10 | PROVE-Sheets gate (the generalization litmus) | `SYNTHESIS.md:176,222`; `alphaC:343-370` | ACCEPT | ACCEPT |
| 11 | G6 acceleration-wiring / orphan-kernel gate | `SYNTHESIS.md:175,221`; telemetry `:400` | ACCEPT | ACCEPT |

Tally: ACCEPT 11, REVISE 0, REJECT 0. (All four V1 REVISEs verified folded → ACCEPT.)

---

## §1 — SYNTHESIS §0.1 close-condition gate table — ACCEPT

`SYNTHESIS.md:163-181`. Unchanged in substance from V1 (ACCEPT) and strengthened by the
folds. Every gate row carries a concrete, greppable verifier. The V2 additions are all
tightenings, not loosenings:

- G3 (`:172`) now binds the canonical Lock-14 three-surface model AND the `match grammar`-arm
  grep co-gate, with the explicit caveat "md5-distinctness alone is necessary-not-sufficient
  — a neutral md5-distinct output can still come from a grammar-branching body." This is the
  correct closure of the distinct-grammar-output false-pass.
- G4 (`:173`) now names the `G: EventGrammar` axis vs the already-real `K=Kind` axis and
  carries `json_rich_navigation_preserved` as an explicit anti-LCD-flatten condition — and
  carries "DELETE is the abrogate-before-patch DEFAULT" plus the separability clause (the
  trait does NOT require `<G>`). This pre-empts the "manufacture the phantom we are
  deleting" failure mode.
- PASS-IMPL V4 (`:181`) carries the honest-finding (a)-(c) qualification gate (see §9).

P4 (`:168`) remains the model anti-paper-close row: "a GREEN gate is meaningful — it scans
the surfaces where Lock-14 phrase-#1 leaks could live … `accepts_current_allowlist` passes
ONLY because the leaks are actually gone (not excluded)." No deferred wave here lacks a
gate. ACCEPT.

---

## §2 — SYNTHESIS §0.3 receiver goalset — ACCEPT (V1 REVISE D-CH6-1 folded)

`SYNTHESIS.md:213-223`. **V1 D-CH6-1 fold verified.** The two under-bound disjunctive
receivers are now self-contained:

- **G6 receiver (`:221`)** now reads: "Wire … into the CSS hot path AT ADMISSION with a
  same-wave consumer, OR honestly retire/mark them **with a samply attribution row proving
  the kernel's target leaf is non-top-N on the benched CSS hot path** (the retire branch is
  gated on a MEASUREMENT, not an assertion — it cannot close G6 by marking all NEON
  'retired' with zero acceleration wired)." Re-grepped verbatim at `:221`. The retire branch
  now carries the same telemetry floor the §0.1 row already named. The escape hatch is
  closed: a receiver cannot mark all NEON retired and walk.

- **PROVE receiver (`:222`)** now carries the §0.5 fallback inline: "Fallback per §0.5: if
  Sheets cannot be emitted via the generator ONLY, the generalization is NOT real — surface
  honestly, do NOT stub-prove; do NOT hand-write a `_GENERATED_RS` Sheets block." Re-grepped
  verbatim. A receiver reading only §0.3 can no longer read PROVE as mandatory-close and
  paper over a hand-written Sheets blob.

Both gates that V1 found "existed elsewhere but not inline in the receiver table" are now
inline. The receiver is self-contained — S-P3 reading only §0.3 to assign owner paths cannot
reach a paper-close read-path. ACCEPT.

---

## §3 — SYNTHESIS §0.5 generalization litmus + per-axis fallbacks — ACCEPT

`SYNTHESIS.md:289-311`. Unchanged from V1 (ACCEPT) and remains the strongest
anti-paper-close section. Every axis row carries Current / Target / Expected-intervention /
**Fallback-if-not-met**, every fallback names a concrete non-paper-close action ("surface
honestly as a named validated grammar-parameterized primitive (HANDOFF §6), do NOT
paper-close; do NOT silently retain the hand-written blob" `:299`; "if Sheets cannot be
emitted via the generator only: the generalization is NOT real — surface honestly, do NOT
stub-prove" `:301`; "REJECT the trait shape, report, do NOT force a Lock-1 violation"
`:302`). The litmus is explicitly binary-structural (`:294`), gated on PRESERVING the
>SOTA. The R10 tranche-success criterion (`:305-311`) restates the conjunction correctly.
ACCEPT.

---

## §4 — SYNTHESIS Section 2 telemetry binding + gate consumer — ACCEPT

`SYNTHESIS.md:370-431`. The load-bearing answer to CH6's "is the goalset measurable +
verifiable from the bench gate?" — YES, and the V2 fold strengthened it. New machine-checkable
columns added in V2 are all tightenings:

- `generator_grammar_branch_count` (`:392`) — the canonical Lock-14 `match grammar`-arm
  census co-gate, explicitly "md5-distinctness is necessary-not-sufficient, this is the
  neutral-emitter co-gate." This is the column that closes the distinct-grammar-output
  false-pass I confirmed live (arm-grep already 0 while fork stands; the co-gate with
  `emitter_fork_present == false` is what bites).
- `json_rich_navigation_preserved` (`:395`) — closes the ≥2-impl LCD-flatten false-green.
- `sheets_grammar_shape ∈ {pratt-operator/flat-stream/tree}` (`:399`) — makes the
  "genuinely different shape" litmus machine-checkable; a flat-stream/tree Sheets is
  REJECTed as third-JSON hollowing.

The gate consumer (`:408-431`) names the EXACT reject conditions, now including
`generator_grammar_branch_count > 0` ("a grammar-branching emitter body even when
md5-distinct"), `json_rich_navigation_preserved == false`, and `sheets_grammar_shape ∈
{flat-stream,tree}` on a Sheets claim. The `gate-json` host command exists today
(`xtask/src/main.rs:59`, re-verified); the `--skv18-generalization-report` extension is a
real namable obligation. The enum-typed columns close the narrative-escape that
"instantiate-or-delete" / "byte-for-byte-or-divergent" could otherwise leave open. ACCEPT.

---

## §5 — SYNTHESIS Section 3 trajectory + revert dependency graph — ACCEPT (V1 REVISE D-CH6-2 folded)

`SYNTHESIS.md:433-485`. **V1 D-CH6-2 fold verified.** The revert *dependency graph* — which
waves' failure BLOCKS which downstream waves — is now carried into the goalset S-P3
consumes. `:475-485` reads: "S-P3's revert protocol MUST encode the **entry-gate dependency
graph** (PASS-ALPHA §4.4): … PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1, per alphaE
§cross-cutting 1): a wave that fails its exit gate BLOCKS every downstream wave that
entry-gates on it — no downstream wave dispatches over a REDRESSed predecessor; in
particular G1 failure blocks G2/G3/G4/PROVE, and G3 (un-fork) failure blocks PROVE (which
emits Sheets THROUGH the un-forked generator)."

The V1 concern — "a sanctioned-deferred revert protocol with no stated blocking-dependency
lets S-P3 author revert-in-isolation, where a broken G1 marches on while G2/G3/G4/PROVE
entry-gate on it" — is now closed. The closing sentence (`:483-485`) makes the intent
explicit: "This converts the sanctioned deferral from 'revert TBD' into 'revert TBD with a
binding dependency graph + a halt ceiling' — the difference between a legitimate handoff and
a paper-close." The deferral itself remains PASS-ALPHA §4.4-sanctioned (CH6 cannot REJECT
the deferral as out-of-bounds; §4.4 places owner-paths/gates/caps/revert in `SPEC.md`
authored by S-P3). The fold supplies the binding carry that makes the deferral legitimate.
ACCEPT.

---

## §6 — HANDOFF "Next Move" + revert/cap deferral carry — ACCEPT (V1 REVISE D-CH6-3 folded)

`HANDOFF.md:239-298`. **V1 D-CH6-3 fold verified.** The hard-cap defaults are now carried.
`:287-297` now reads: "Revert protocol, hard caps, and per-wave triumvirate discipline are
sanctioned-deferred to S-P3 (PASS-ALPHA §4.4 authority), not paper-closed here — with two
binding carries so the deferral is a legitimate handoff, not an uncapped-execution
paper-close: 1. **Revert dependency graph** … 2. **Hard-cap defaults:** S-P3 MUST carry the
standing [dispatch-hard-cap] defaults (research/plan/redress 20/15/30 min, 'at 0.9N commit,
at N halt') unless the wave's risk class (the Sheets/NEON cluster is MED-HIGH per alphaE)
justifies a documented larger cap — so no SK-V18 wave dispatches uncapped."

The V1 concern — "a contract that defers hard caps entirely, with no carried default, risks
an S-P3 that authors waves with no halt condition (the slow-paper-close where a wave runs
unbounded chasing a >SOTA-preserving projection)" — is closed. Both the revert dependency
graph AND the hard-cap defaults are carried as binding obligations on S-P3, with the
MED-HIGH risk-class carve-out matching alphaE's B4 classification. The same-wave-consumer
rule (`:262` "Each primitive lands WITH its hot-path consumer in the same commit (no orphan
kernels)") and the per-wave >SOTA re-proof (`:262`) remain stated, pre-empting the
orphan-kernel and silent-regression paper-closes. ACCEPT.

---

## §7 — alphaE candidate shortlist gates + sequencing — ACCEPT

`alphaE:29-203`. Unchanged ACCEPT from V1, and the V2 fold-ledger (`alphaE:10-25`) resolves
the αE-owned REVISEs in place (F1-F8). Every candidate (A, B1-B4) carries owner path:line,
scalar-ref status, checkasm status, same-wave consumer, the falsifiability triple
(PRESERVED->SOTA / grammar-derivation-mutate-proof / distinct-output), LOC budget, risk,
pre-blocks. Critical for CH6:

- The falsifiability triple's gate #2 (`:34` "mutate the `.bbnf` → the regenerated
  `generated.rs` changes correspondingly — a const courier cannot pass this") is an
  *operational* derivation test, not an assertion — it catches the verbatim-blob-with-honest-comment
  defect (the `runtime_generator.rs:683-701` true-but-misleading provenance comment that I
  re-confirmed live). This is the right anti-paper-close instrument.
- The cross-cutting note 1 (`:196`) now carries the CH6 §5 exit-gate-blocks-successor clause
  explicitly: "the entry-gate dependency must be carried as an EXPLICIT
  exit-gate-blocks-successor clause into S-P3, so a broken G1 predecessor halts
  G2/G3/G4/PROVE rather than marching on" — the alphaE-side origin of the SYNTHESIS §5 fold.
- The cross-cutting note 2 (`:197`) carries the CH6 §9 honest-finding sharpening (the named
  primitive must be `.bbnf`-invoked + parameterized + reference-backed, else REJECT).
- The risk-weighted close prediction (`:200`) explicitly handles litmus-failure without
  paper-closing ("If B4's Sheets litmus fails, SK-V18 does NOT paper-close — it surfaces
  'generator is still JSON+CSS-overfit,' iterates B1/B2, and B4 re-enters (V≤5 ceiling)").

ACCEPT.

---

## §8 — alphaC PRUNE-wave close gates — ACCEPT

`alphaC:81-175`. Unchanged ACCEPT from V1; the V2 fold (`alphaC:30-68`) resolves the two
αC-owned REVISEs (CH2 §3.5 P3 collapse-vs-differentiate; CH5 C.4 P4 witness/`EventGrammar`
seam). Each PRUNE wave (P1-P5) carries live evidence + delete-or-fix obligation + a close
gate that makes the prune meaningful. P4's close gate (`:69`, "re-run after temporarily
re-introducing a `JsonSink` token into `runtime_generator.rs` must now turn it RED (proving
coverage). The gate must no longer pass by exclusion") is the model anti-paper-close: a green
gate that is green for the right reason, verified by an injected-token RED test. The P3
collapse-vs-differentiate fold (`:143-152`) correctly makes COLLAPSE-to-one the default and
gates "N distinct generated.rs" behind "N distinct `.bbnf` roots genuinely authored — else a
hollow distinct-grammar-output false-pass." The Sheets broadcast pre-block (`:343-370`) binds
the Sheets corpus to per-corpus N≥50-cold-median (PERMANENT pre-block, no different-framing
admission) — closing the most likely Sheets paper-close (one timing tuple broadcast across N
corpus rows). ACCEPT.

---

## §9 — The honest-finding escape (the anti-paper-close hatch itself) — ACCEPT (V1 REVISE D-CH6-4 folded)

`SYNTHESIS.md:181`; `HANDOFF.md:283-285`; `alphaE:197`. **V1 D-CH6-4 fold verified — this
was the single largest residual paper-close surface, and it is now gated.** The PASS-IMPL V4
row (`SYNTHESIS.md:181`) now carries the (a)-(c) qualification gate verbatim: "The
honest-finding escape is itself GATED — a 'named validated grammar-parameterized primitive'
qualifies ONLY if: (a) the grammar `.bbnf` INVOKES it by name (the primitive is a callable
the grammar references — e.g. a registered balanced-delimiter scanner — NOT a free-standing
const the emitter splices); (b) it is parameterized by grammar-derived DATA
(alphabet/delimiter set from the rule shape), NOT a fixed body; (c) it carries the same
`verbatim_blob_present == false` telemetry as any other derived surface. A primitive failing
(a)-(c) is a relabeled hand-written blob — REJECT, REDRESS, do NOT close. Without this gate
the escape is the single largest paper-close surface in the contract."

This is the precise gate V1 demanded. The (a)-(c) triple distinguishes a legitimate named
primitive from the exact failure mode V1 named — "a receiver under contact reaches for
'named validated grammar-parameterized primitive' and ships the SAME `CSS_GENERATED_RS`-style
const blob wearing the new label." A relabeled `CSS_GENERATED_RS` fails (a) (it is a
free-standing const the emitter splices, not a `.bbnf`-invoked callable), fails (b) (fixed
body, not grammar-derived data), and fails (c) (it IS the verbatim blob —
`verbatim_blob_present == true`). The gate is reinforced at alphaE §197 ("a 'primitive' that
is a relabeled blob without `.bbnf`-invocation + parameterization + a reference is REJECTED
to REDRESS") and carried in the §0.5 fallbacks. The escape is now a genuine honest-finding
path, not a paper-close hatch. ACCEPT.

---

## §10 — PROVE-Sheets gate (the generalization litmus) — ACCEPT

`SYNTHESIS.md:176,222`; `alphaC:343-370`; `alphaE:151-176`. The cycle's honest litmus,
correctly non-paper-closeable, and strengthened in V2 by `sheets_grammar_shape ==
pratt-operator` (the shape disclosure that makes "genuinely different shape" machine-checkable
— a flat-stream/tree Sheets REJECTed as third-JSON hollowing). The gate binds: md5(Sheets) ≠
JSON ≠ CSS (distinct-grammar-output); Sheets value type instantiates the G4 trait; ZERO
hand-authored runtime Rust (`grep -c 'const.*_RS.*r#' codegen/src` for any Sheets blob → 0);
via the generator ONLY; the canonical neutrality grep (G3) stays 0. The V2 adoption of the
EXISTING real Pratt `google-sheets.bbnf` (verified present, not in the skinny tree) over a
fresh stub is the correct anti-hollowing move. The broadcast pre-block (alphaC §2.4) binds
the Sheets corpus to per-corpus N≥50-cold-median. The §0.5 fallback ("if Sheets cannot be
emitted via the generator only, the generalization is NOT real — surface honestly, do NOT
stub-prove") is now also inline in the §0.3 receiver (§2 above). ACCEPT.

---

## §11 — G6 acceleration-wiring / orphan-kernel gate — ACCEPT

`SYNTHESIS.md:175,221`; telemetry `acceleration_at_admission` (`:400`); `alphaE:151-176`;
`alphaC:400-424`. The acceleration-wiring gate corrects the SK-V17 W3 overstatement (NEON
"acceleration" dead at admission — re-confirmed at alphaD I6: `find_css_significant`/
`find_comment_close` are `#[cfg(test)]`-only) and is machine-checkable: "any kernel claiming
acceleration is reached at admission (grep the generated hot path, not tests)." The
`acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` enum with
`cfg-test-only` = NO-GO (`:400`) closes the dead-at-admission paper-close. The V1 soft spot
(the "retire" branch needing a measured non-top-N floor) is now folded into the §0.3 receiver
(§2 above: "with a samply attribution row proving the kernel's target leaf is non-top-N");
the §0.1 gate row (`:175`) carries the same. The same-wave-consumer rule (alphaE `:168` "A
kernel with no admission-path consumer is RETIRED, not shipped") pre-empts the orphan-kernel
pattern. ACCEPT.

---

## Consolidated CH6 V2 verdict

The αF V2 contract folded all four V1 CH6 REVISEs as **binding gate text in the artefacts
the next pass consumes** — not as narrative gestures. Each fold was re-grepped verbatim at
its cited path:line and each closes the read-path-to-paper-close V1 identified:

- **D-CH6-1 (§0.3 receiver):** G6 retire-branch samply floor + PROVE §0.5 fallback now
  inline in the receiver table (`SYNTHESIS.md:221,222`). RESOLVED → §2 ACCEPT.
- **D-CH6-2 (Section 3):** revert dependency graph (failure-blocks-downstream) carried into
  the goalset (`SYNTHESIS.md:475-485`). RESOLVED → §5 ACCEPT.
- **D-CH6-3 (HANDOFF Next Move):** hard-cap defaults (20/15/30, MED-HIGH carve-out) carried
  as a binding S-P3 obligation (`HANDOFF.md:287-297`). RESOLVED → §6 ACCEPT.
- **D-CH6-4 (honest-finding escape):** the (a)-(c) qualification gate distinguishing a real
  grammar-parameterized primitive from a relabeled blob carried in the PASS-IMPL V4 row
  (`SYNTHESIS.md:181`). RESOLVED → §9 ACCEPT.

The contract is **strongly anti-paper-close in its measurable core and now in its deferred
surface**: §0.1 gates greppable; §0.5 fallbacks named; Section 2 telemetry binds every
generalization axis to a `gate-json` reject condition (the host command exists; the
generalization-report extension is a real obligation); the alphaE triple's `.bbnf`-mutation
test is operational; the alphaC PRUNE close gates are RED-on-injection meaningful. No wave is
deferred without a receiver + a gate. The generalization is concrete and falsifiable
(`json_sink_direct` must project per the mutation test; CSS must lower per `CSS_GENERATED_RS`
retirement; the generator must un-fork per `RuntimeEmitterKind` deletion +
`generator_grammar_branch_count == 0`; Sheets must be md5-distinct + pratt-shaped from
`.bbnf` via the generator only). The sanctioned-deferred revert/cap/triumvirate now carries
its binding dependency graph + halt ceiling.

Zero REJECT (no finding reversed, no premise fabricated — all re-verified live). Zero REVISE
(all four V1 REVISEs landed; no new paper-close surface introduced by the folds; the folds
are tightenings only). This lens converges for V2: ACCEPT 11 / REVISE 0 / REJECT 0, accept
rate 100%, zero orphan REVISE.

TALLY accept=11 revise=0 reject=0
