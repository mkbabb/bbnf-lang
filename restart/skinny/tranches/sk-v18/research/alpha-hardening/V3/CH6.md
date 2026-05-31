# CH6 — ANTI-PAPER-CLOSE (V3) — SK-V18 Pass-Alpha Adversarial Review (third cycle)

Lens: CH6 Next-Tranche-Impact / ANTI-PAPER-CLOSE. Per PASS-ALPHA §3 (CH6: "does the
SK-V{N+1} contract specify revert protocol per intervention? Hard caps? Triumvirate
discipline? Is the goalset measurable + verifiable from the bench gate?") + ORCHESTRATOR
§3W/§3Z. Cycle V3 over the αF V3 contract that FOLDED the three surviving V2 REVISEs
(CH2 §8 neutrality-alphabet + scan-root widening; CH4 §6 stale-checkasm-count; CH5 E.1
shared-trait test-exclusion) atop the V1+V2 folds.

Reviewed: `sk-v18/research/alpha/{alphaA-results-extraction.md, alphaB-competitor-deltas.md,
alphaC-redress-digest.md, alphaD-validated-invalidated.md, alphaE-candidate-shortlist.md}`
+ `SYNTHESIS.md` + `HANDOFF.md`. (No `alphaF-*.md` exists by that name — per PASS-ALPHA
§2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md`; both present at the tranche
top level, both reviewed. Confirmed correct structure per V1 CONSOLIDATED §1 and V2 CH6.
The literal α-F filename absence is NOT a defect.)

**Lens mandate (binding):** no wave deferred without a receiver + a gate; generalization
is concrete (`json_sink_direct` actually projects, CSS actually lowers, the generator
actually un-forks); the goalset is telemetry-bound + bench-verifiable; the honest-finding
escape is not a paper-close hatch; revert / hard-cap / triumvirate are specified or
contract-sanctioned-deferred with a binding handoff.

**Posture for V3 (non-rubber-stamp).** V2 CH6 converged at 11 ACCEPT / 0 REVISE / 0
REJECT — all four V1 REVISEs folded. V3 is NOT a confirmation pass by default. The lens
re-greps every ground-truth premise at HEAD, re-greps each V3 fold site, and re-disposes
every CH6-owned section independently. Three new questions for V3:
1. Did the three V2→V3 folds (CH2 §8 / CH4 §6 / CH5 E.1) land as **binding gate text**
   the next pass consumes, or as narrative gestures?
2. Did the folds introduce **any new paper-close surface** (the failure mode where a
   tightening on one axis opens a hole on another)?
3. Do the carried V1/V2 folds (revert dep graph, hard-cap defaults, honest-finding (a)-(c)
   gate, G6 retire-branch samply floor) survive **verbatim** at the current HEAD bracket?

## Ground-truth re-verification (re-grepped at HEAD `318d9c046`)

Every premise the contract rests on confirmed LIVE at this bracket — not inherited from
V2, re-run:

- `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — STILL a verbatim
  `&str` literal (the verbatim-blob G2 targets), referenced `:91`
  (`normalize(CSS_GENERATED_RS)`). CONFIRMED.
- `grammar_provider.rs:40` `pub enum RuntimeEmitterKind`, branched `:110`
  (`!= RuntimeEmitterKind::RequestFacts`), field `:33`. CONFIRMED (G3 fork live).
- `tape/mod.rs:175` `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar
  = AnyGrammar>` — the EXACT two-axis signature. CONFIRMED: `K` defaulted to `AnyKind`,
  `G` defaulted to `AnyGrammar`. The V1 fold's distinction (resolve the **`G` axis**, not
  the already-real `K` axis) remains grounded in the literal source — material to §2.
- `bbnf-simd/src/x86_64/` = 24 files. CONFIRMED (P1).
- `json/generated.rs` `parse_w11_1_number` count = 7. CONFIRMED (P5).
- `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS`, iterated `:2467`, required `:2508`.
  CONFIRMED (P4).
- `grammar/google-sheets/google-sheets.bbnf` = 7681 bytes, PRESENT (the real Pratt
  source, NOT in the benched skinny tree). CONFIRMED (PROVE source).
- `sheets_witness/` = 25 LOC (`event_grammar_witness.rs` 24 + `mod.rs` 1). CONFIRMED
  (PROVE start state).
- `LOCKS.md` item 14 verification commands carry the canonical un-abbreviated alphabet
  `Json|CssL4|Bbnf\w*|GoogleSheets\w*` AND the grammar-named-type census
  `JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser`. CONFIRMED — the V3 fold's
  alphabet correction (`GoogleSheets` un-abbreviated, `Sheets\w*` does NOT match
  `GoogleSheets =>`) is grounded in the literal lock text, not invented. Material to §4.
- `rg` arm-census over `skinny/crates/codegen/src skinny/xtask/src` (full canonical
  alphabet) → **0** at HEAD. CONFIRMED — and load-bearing: the arm census ALREADY returns
  0 while the `RuntimeEmitterKind` fork persists behind the abstract enum. The co-gate
  that actually bites G3 is `emitter_fork_present == false` (the `RuntimeEmitterKind`
  deletion grep) + `generator_grammar_type_count == 0`, not the arm census alone. The
  contract carries all three (SYNTHESIS §0.1 G3 + Section 2 columns + §0.6 invariant 5) —
  verified.
- grammar-named-*type* census `rg 'JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap'
  skinny/crates/codegen/src skinny/xtask/src` → **0** at HEAD. CONFIRMED — the new V3
  column `generator_grammar_type_count == 0` starts satisfiable, so it is a real co-gate
  (catches a re-emitted grammar-named type the arm census misses), not a poison gate.
- `skinny/xtask/src/` = `{bin, lib.rs, main.rs, real_typed_schema.rs, regen.rs,
  regen_css.rs, skv15_w0.rs}` — the workspace-metadata surface the V3 fold widens the
  scan root to. CONFIRMED present (the `RuntimeTarget`/strategy-table relocation surface
  is real, the scan-root widening is not vapor).
- `find skinny -name 'checkasm_*.rs'` = **14** total (12 single-kernel differentials +
  `checkasm_common.rs` + `checkasm_parity.rs`). CONFIRMED — the V3 CH4 §6 fold's "disk-true
  14, NOT 18" is correct against disk; a contract asserting "18 present" would have seeded
  an un-satisfiable P4-class gate. Material to §7.

The ground truth is solid and unchanged in substance from V1/V2. My dispositions concern
the *forward* contract's paper-close surface — whether the V3 folds bind the deferred-wave
gates into the receiver/trajectory/handoff the next pass reads, and whether they introduced
any new hole.

---

## Disposition summary (per reviewable section)

| # | Section | Path:line | V1 | V2 | V3 |
|---|---|---|---|---|---|
| 1 | SYNTHESIS §0.1 close-condition gate table | `SYNTHESIS.md:192-210` | ACCEPT | ACCEPT | ACCEPT |
| 2 | SYNTHESIS §0.3 receiver goalset (G6 retire + PROVE fallback) | `SYNTHESIS.md:242-252` | REVISE | ACCEPT | ACCEPT |
| 3 | SYNTHESIS §0.5 generalization litmus + fallbacks | `SYNTHESIS.md:318-340` | ACCEPT | ACCEPT | ACCEPT |
| 4 | SYNTHESIS Section 2 telemetry binding + gate consumer | `SYNTHESIS.md:401-466` | ACCEPT | ACCEPT | ACCEPT |
| 5 | SYNTHESIS Section 3 trajectory + revert dependency graph | `SYNTHESIS.md:468-520` | REVISE | ACCEPT | ACCEPT |
| 6 | HANDOFF "Next Move" + revert/cap deferral carry | `HANDOFF.md:256-317` | REVISE | ACCEPT | ACCEPT |
| 7 | alphaE candidate shortlist gates + sequencing | `alphaE:44-218` | ACCEPT | ACCEPT | ACCEPT |
| 8 | alphaC PRUNE-wave close gates | `alphaC:82-227` | ACCEPT | ACCEPT | ACCEPT |
| 9 | The honest-finding escape (anti-paper-close hatch) | `SYNTHESIS.md:210`; `HANDOFF.md:301-304`; `alphaE:212` | REVISE | ACCEPT | ACCEPT |
| 10 | PROVE-Sheets gate (the generalization litmus) | `SYNTHESIS.md:205,251`; `alphaC:351-378` | ACCEPT | ACCEPT | ACCEPT |
| 11 | G6 acceleration-wiring / orphan-kernel gate | `SYNTHESIS.md:204,250`; telemetry `:432` | ACCEPT | ACCEPT | ACCEPT |
| 12 | V3 fold integrity (the three V2 REVISEs landed without new hole) | `SYNTHESIS.md:26-53`; `:423-424`; `:426`; `:377-378` | — | — | ACCEPT |

Tally: ACCEPT 12, REVISE 0, REJECT 0.

---

## §1 — SYNTHESIS §0.1 close-condition gate table — ACCEPT

`SYNTHESIS.md:192-210`. Every gate row carries a concrete, greppable verifier, and the
V3 folds tightened (never loosened) the generalization rows:

- G3 (`:201`) now binds the FULL canonical Lock-14 arm census over BOTH codegen AND the
  xtask metadata surface — `rg ... skinny/crates/codegen/src skinny/xtask/src` (i) the
  arm census, (ii) the grammar-named-type census — with the explicit "md5-distinctness
  alone is necessary-not-sufficient" caveat. I re-confirmed both greps return 0 at HEAD
  while the `RuntimeEmitterKind` fork stands, so the co-gate `emitter_fork_present ==
  false` is the one that bites — and it is carried. The xtask-surface widening closes the
  relocated-overfit-seam (a per-grammar branch moved into a neutral-identifier
  `RuntimeTarget` data-table); the type census closes the re-emitted-grammar-named-type
  seam the arm census misses. This is the correct closure of the distinct-grammar-output
  + relocation false-pass.
- G4 (`:202`) names the `G: EventGrammar` axis vs the already-real `K=Kind` axis, carries
  `json_rich_navigation_preserved` as the anti-LCD-flatten condition, makes DELETE the
  abrogate-before-patch default, and carries the separability clause (the trait does NOT
  require `<G>`). This pre-empts the "manufacture the phantom we are deleting" failure mode.
- PASS-IMPL V4 (`:210`) carries the honest-finding (a)-(c) qualification gate (see §9).

P4 (`:197`) remains the model anti-paper-close row: "a GREEN gate is meaningful — it
scans the surfaces where Lock-14 phrase-#1 leaks could live … `accepts_current_allowlist`
passes ONLY because the leaks are actually gone (not excluded)." No deferred wave here
lacks a gate. ACCEPT.

---

## §2 — SYNTHESIS §0.3 receiver goalset — ACCEPT (V1 REVISE D-CH6-1 fold survives)

`SYNTHESIS.md:242-252`. The two under-bound disjunctive receivers V1 flagged remain
self-contained at HEAD:

- **G6 receiver (`:250`)** still reads: "Wire `find_css_significant`/`find_comment_close`
  … into the CSS hot path AT ADMISSION with a same-wave consumer, OR honestly retire/mark
  them **with a samply attribution row proving the kernel's target leaf is non-top-N on
  the benched CSS hot path** (the retire branch is gated on a MEASUREMENT, not an
  assertion — it cannot close G6 by marking all NEON 'retired' with zero acceleration
  wired)." Re-grepped verbatim. The retire branch carries the telemetry floor the §0.1
  row names. The escape hatch stays closed.
- **PROVE receiver (`:251`)** still carries the §0.5 fallback inline: "Fallback per §0.5:
  if Sheets cannot be emitted via the generator ONLY, the generalization is NOT real —
  surface honestly, do NOT stub-prove; do NOT hand-write a `_GENERATED_RS` Sheets block."
  Re-grepped verbatim. A receiver reading only §0.3 cannot read PROVE as mandatory-close
  and paper over a hand-written Sheets blob.

The receiver is self-contained — S-P3 reading only §0.3 to assign owner paths cannot reach
a paper-close read-path. The V3 folds touched the neutrality grep (G3 obligation `:247`)
but did not weaken the G6/PROVE receivers. ACCEPT.

---

## §3 — SYNTHESIS §0.5 generalization litmus + per-axis fallbacks — ACCEPT

`SYNTHESIS.md:318-340`. The strongest anti-paper-close section, unchanged in substance.
Every axis row carries Current / Target / Expected-intervention / **Fallback-if-not-met**,
every fallback names a concrete non-paper-close action ("surface honestly as a named
validated grammar-parameterized primitive (HANDOFF §6), do NOT paper-close; do NOT
silently retain the hand-written blob" `:328`; "if Sheets cannot be emitted via the
generator only: the generalization is NOT real — surface honestly, do NOT stub-prove"
`:330`; "REJECT the trait shape, report, do NOT force a Lock-1 violation" `:331`). The
litmus is explicitly binary-structural (`:323`), gated on PRESERVING the >SOTA. The R10
criterion (`:334-340`) restates the conjunction correctly. ACCEPT.

---

## §4 — SYNTHESIS Section 2 telemetry binding + gate consumer — ACCEPT

`SYNTHESIS.md:401-466`. The load-bearing answer to CH6's "is the goalset measurable +
verifiable from the bench gate?" — YES, and the V3 fold strengthened it with two new
enum/integer co-gates that I confirmed start satisfiable at HEAD (so they are real co-gates,
not poison gates):

- `generator_grammar_branch_count` (`:423`) — the canonical Lock-14 arm census over the
  FULL canonical alphabet `Json|CssL4|(GoogleSheets|Sheets)|Bbnf` across BOTH
  `skinny/crates/codegen/src` AND `skinny/xtask/src` (the V3 widening: "so a per-grammar
  branch relocated into a neutral-identifier metadata data-table is caught"). I confirmed
  this grep returns 0 at HEAD.
- `generator_grammar_type_count` (`:424`) — the NEW V3 grammar-named-type census
  `rg 'JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap' ...`, "the arm census
  misses a re-emitted grammar-named parser/`EventGrammar` type literal." Confirmed 0 at
  HEAD. This is the precise leak class the arm census alone cannot catch — a real
  tightening.
- `json_rich_navigation_preserved` (`:427`) — closes the ≥2-impl LCD-flatten false-green.
- `sheets_grammar_shape ∈ {pratt-operator/flat-stream/tree}` (`:431`) — makes the
  "genuinely different shape" litmus machine-checkable; a flat-stream/tree Sheets is
  REJECTed as third-JSON hollowing.

The gate consumer (`:440-466`) names the EXACT reject conditions, now including
`generator_grammar_branch_count > 0` ("including a branch relocated into the xtask
metadata strategy table"), `generator_grammar_type_count > 0` ("a re-emitted grammar-named
parser/`EventGrammar` type the arm census misses"), `json_rich_navigation_preserved ==
false`, and `sheets_grammar_shape ∈ {flat-stream,tree}` on a Sheets claim. The `gate-json`
host command exists today (`xtask/src/main.rs`, re-verified the xtask surface is present);
the `--skv18-generalization-report` extension is a real namable S-P3 obligation. The
enum-typed columns close the narrative-escape that "instantiate-or-delete" /
"byte-for-byte-or-divergent" could otherwise leave open. ACCEPT.

---

## §5 — SYNTHESIS Section 3 trajectory + revert dependency graph — ACCEPT (V1 REVISE D-CH6-2 fold survives)

`SYNTHESIS.md:468-520`. The revert *dependency graph* — which waves' failure BLOCKS which
downstream waves — survives verbatim in the goalset S-P3 consumes. `:510-515` reads: "S-P3's
revert protocol MUST encode the **entry-gate dependency graph** … PRUNE → G1 → G2 → G3 →
G4 → G5/G6 → PROVE → H1 … a wave that fails its exit gate BLOCKS every downstream wave that
entry-gates on it … in particular G1 failure blocks G2/G3/G4/PROVE, and G3 (un-fork)
failure blocks PROVE (which emits Sheets THROUGH the un-forked generator)." The closing
sentence (`:518-520`) makes the intent explicit: "the difference between a legitimate
handoff and a paper-close." The deferral itself remains PASS-ALPHA §4.4-sanctioned (CH6
cannot REJECT the deferral as out-of-bounds; §4.4 places owner-paths/gates/caps/revert in
`SPEC.md` authored by S-P3). The fold supplies the binding carry that makes the deferral
legitimate. The V3 folds did not touch this section. ACCEPT.

---

## §6 — HANDOFF "Next Move" + revert/cap deferral carry — ACCEPT (V1 REVISE D-CH6-3 fold survives)

`HANDOFF.md:256-317`. The hard-cap defaults survive verbatim. `:306-316` reads: "Revert
protocol, hard caps, and per-wave triumvirate discipline are sanctioned-deferred to S-P3
… with two binding carries … 1. **Revert dependency graph** … 2. **Hard-cap defaults:**
S-P3 MUST carry the standing [dispatch-hard-cap] defaults (research/plan/redress 20/15/30
min, 'at 0.9N commit, at N halt') unless the wave's risk class (the Sheets/NEON cluster is
MED-HIGH per alphaE) justifies a documented larger cap — so no SK-V18 wave dispatches
uncapped." Both the revert dependency graph AND the hard-cap defaults are carried as
binding obligations on S-P3, with the MED-HIGH risk-class carve-out matching alphaE's B4
classification. The same-wave-consumer rule (`:278` "Each primitive lands WITH its hot-path
consumer in the same commit (no orphan kernels)") and the per-wave >SOTA re-proof (`:278-279`)
remain stated, pre-empting the orphan-kernel and silent-regression paper-closes.

The V3 fold updated the Next-Move §status header (`:11-18`) to cite the three V2 REVISEs
folded, and updated the gate-consumer enumeration (`:287-288`) to add
`generator_grammar_type_count == 0` and the FULL-alphabet arm-census-over-codegen-AND-xtask
note — both tightenings consistent with Section 2. No loosening introduced. ACCEPT.

---

## §7 — alphaE candidate shortlist gates + sequencing — ACCEPT

`alphaE:44-218`. The V3 FOLD LEDGER (`alphaE:10-23`) resolves the αE-touching V2 REVISEs
(F9 trait-grep test-exclusion; F10 canonical four-grammar neutrality alphabet + widened
scan roots + type census; F11 P1 `.asm` LOC; F12 dispatch.rs owner-path) in place,
orphan-free, each tagged at the exact gate it touches. Critical for CH6:

- The falsifiability triple's gate #2 (`:49` "mutate the `.bbnf` → the regenerated
  `generated.rs` changes correspondingly — a const courier cannot pass this") remains an
  *operational* derivation test, not an assertion — it catches the verbatim-blob-with-honest-comment
  defect (the `runtime_generator.rs:685-701` true-but-misleading provenance comment I
  re-confirmed live: `:685` "It no longer …" header over a `const &str` body). Right
  anti-paper-close instrument.
- The cross-cutting note 1 (`:211`) carries the CH6 §5 exit-gate-blocks-successor clause
  explicitly: "the entry-gate dependency must be carried as an EXPLICIT
  exit-gate-blocks-successor clause into S-P3, so a broken G1 predecessor halts
  G2/G3/G4/PROVE rather than marching on."
- The cross-cutting note 2 (`:212`) carries the CH6 §9 honest-finding sharpening (the named
  primitive must be `.bbnf`-invoked + parameterized + reference-backed, else REJECT).
- F4 (`:34`) corrects the checkasm count to the disk-true "12 single-kernel differentials +
  2 = 14" — I confirmed `find skinny -name 'checkasm_*.rs'` = 14 at HEAD. A gate asserting
  "18 present" would be un-satisfiable on a clean tree (the exact P4-class false gate this
  cycle fixes). The V3 fold propagated this into the binding contract (SYNTHESIS:377-378).
- The risk-weighted close prediction (`:215`) explicitly handles litmus-failure without
  paper-closing ("If B4's Sheets litmus fails, SK-V18 does NOT paper-close — it surfaces
  'generator is still JSON+CSS-overfit,' iterates B1/B2, and B4 re-enters (V≤5 ceiling)").

ACCEPT.

---

## §8 — alphaC PRUNE-wave close gates — ACCEPT

`alphaC:82-227`. Each PRUNE wave (P1-P5) carries live evidence + delete-or-fix obligation +
a close gate that makes the prune meaningful. P4's close gate (`:194-199`) remains the model
anti-paper-close: "`accepts_current_allowlist` PASSES *after* the rebuild because the scanned
surface is genuinely neutral (not because the dirty files are excluded); the
gate-scope-honesty CH-addendum (diff `GENERIC_SCAN_ROOTS` against the generic-crate file
inventory) reports zero un-scanned production `.rs` under `crates/codegen/src`." Combined with
alphaE P4-exit's injected-token RED test (`alphaE:84`: "re-run after temporarily
re-introducing a `JsonSink` token into `runtime_generator.rs` must now turn it RED"), the
gate is green for the right reason, verified two ways.

The P4 V2-FOLD witness/`EventGrammar` seam (`:178-192`) is the precise anti-relocation
clause: "if the un-forked generator (G3) ever **EMITS** a `ValueRef<…,XEventGrammar>` type
**literal** as a string, that is a grammar-name leak the generic-crate-scoped P4 gate cannot
catch … Add `EventGrammar`/`*EventGrammar` to the emitter's `FORBIDDEN_GENERIC_TOKENS`." This
is the runtime-side counterpart to the V3 `generator_grammar_type_count` column — together
they close the re-emitted-grammar-named-type seam from both the codegen-scan side and the
emitted-string side. Strong.

The P3 collapse-vs-differentiate fold (`:147-162`) correctly makes COLLAPSE-to-one the
DEFAULT and gates "N distinct generated.rs" behind "N distinct `.bbnf` roots genuinely
authored — else a hollow distinct-grammar-output target satisfiable by cosmetic divergence,
which the diff-census addendum must REJECT." The Sheets broadcast pre-block (`:351-378`)
binds the Sheets corpus to per-corpus N≥50-cold-median (PERMANENT pre-block, "No
different-framing admission"), closing the most likely Sheets paper-close (one timing tuple
broadcast across N corpus rows). ACCEPT.

---

## §9 — The honest-finding escape (the anti-paper-close hatch itself) — ACCEPT (V1 REVISE D-CH6-4 fold survives)

`SYNTHESIS.md:210`; `HANDOFF.md:301-304`; `alphaE:212`. The single largest residual
paper-close surface remains gated. The PASS-IMPL V4 row (`SYNTHESIS.md:210`) carries the
(a)-(c) qualification gate verbatim: "a 'named validated grammar-parameterized primitive'
qualifies ONLY if: (a) the grammar `.bbnf` INVOKES it by name … NOT a free-standing const
the emitter splices; (b) it is parameterized by grammar-derived DATA … NOT a fixed body;
(c) it carries the same `verbatim_blob_present == false` telemetry as any other derived
surface. A primitive failing (a)-(c) is a relabeled hand-written blob — REJECT, REDRESS,
do NOT close. Without this gate the escape is the single largest paper-close surface in the
contract."

This is the precise gate V1 demanded. A relabeled `CSS_GENERATED_RS` fails (a) (it is a
free-standing const the emitter splices, not a `.bbnf`-invoked callable — I re-confirmed it
is exactly that at `runtime_generator.rs:701`), fails (b) (fixed body, not grammar-derived
data), and fails (c) (it IS the verbatim blob — `verbatim_blob_present == true`). The gate
is reinforced at alphaE `:212` ("A 'primitive' that is a relabeled blob without
`.bbnf`-invocation + parameterization + a reference is REJECTED to REDRESS") and carried in
the §0.5 fallbacks. The V3 folds did not touch this gate. The escape is a genuine
honest-finding path, not a paper-close hatch. ACCEPT.

---

## §10 — PROVE-Sheets gate (the generalization litmus) — ACCEPT

`SYNTHESIS.md:205,251`; `alphaC:351-378`; `alphaE:166-191`. The cycle's honest litmus,
correctly non-paper-closeable, strengthened by `sheets_grammar_shape == pratt-operator`
(the shape disclosure making "genuinely different shape" machine-checkable — a
flat-stream/tree Sheets REJECTed as third-JSON hollowing). The gate binds: md5(Sheets) ≠
JSON ≠ CSS (distinct-grammar-output); Sheets value type instantiates the G4 trait; ZERO
hand-authored runtime Rust (`grep -c 'const.*_RS.*r#' codegen/src` for any Sheets blob →
0); via the generator ONLY; the canonical neutrality grep (G3) AND type census stay 0. The
adoption of the EXISTING real Pratt `google-sheets.bbnf` (re-confirmed present, 7681 bytes,
NOT in the skinny tree) over a fresh stub is the correct anti-hollowing move — a fresh
minimal stub risks producing "a third JSON" and hollowing the litmus (alphaE F2). The
broadcast pre-block (alphaC §2.4) binds the Sheets corpus to per-corpus N≥50-cold-median,
"No different-framing admission." The §0.5 fallback is inline in the §0.3 receiver (§2
above). ACCEPT.

---

## §11 — G6 acceleration-wiring / orphan-kernel gate — ACCEPT

`SYNTHESIS.md:204,250`; telemetry `acceleration_at_admission` (`:432`); `alphaE:166-191`;
`alphaC` G6 surface. The acceleration-wiring gate corrects the SK-V17 W3 overstatement
(NEON "acceleration" dead at admission — re-confirmed at alphaD/alphaE:
`find_css_significant`/`find_comment_close` are `#[cfg(test)]`-only) and is
machine-checkable: "any kernel claiming acceleration is reached at admission (grep the
generated hot path, not tests)." The `acceleration_at_admission ∈
{admission,scalar-passthrough-labeled,retired}` enum with `cfg-test-only` = NO-GO (`:432`)
closes the dead-at-admission paper-close. The retire branch's measured non-top-N floor is
folded into the §0.3 receiver (§2 above); the §0.1 gate row (`:204`) carries the same. The
same-wave-consumer rule (alphaE `:183` "A kernel with no admission-path consumer is RETIRED
… not shipped") pre-empts the orphan-kernel pattern. The F5 fold bounds G6 LOC by a
committed body-count ceiling (PMULL `bitmap_prefix_xor_64` FIRST; every other kernel
retired/relabelled unless a same-wave consumer exists) — so "+150 per body" is no longer an
unstated multiplicand. ACCEPT.

---

## §12 — V3 fold integrity (the three V2 REVISEs landed without a new hole) — ACCEPT

The threshold V3 question: did the three V2→V3 folds land as binding gate text, and did any
introduce a new paper-close surface? Re-greped each fold site:

- **CH2 §8 fold (neutrality-alphabet + scan-root widening + type census):** landed at
  SYNTHESIS `:26-53` (the V2→V3 fold-ledger), `:201` (G3 close condition), `:423-424` (the
  two telemetry columns), `:444-445` (gate consumer), HANDOFF `:237-251` (invariant 5),
  alphaE F10/`:50`/`:105`/`:134`/`:185`/`:214`, alphaC P3/P4. I confirmed the canonical
  alphabet matches `LOCKS.md` item 14 verbatim (`GoogleSheets\w*` un-abbreviated; the
  `Sheets\w*` start-anchor would MISS `GoogleSheets =>` — a real defect the fold fixes),
  the arm census + type census both return 0 at HEAD over codegen+xtask (so both gates
  start satisfiable — no poison gate), and the xtask surface (`regen_css.rs`/`regen.rs`)
  exists (so the relocation surface is real). **No new hole:** widening the scan root
  STRENGTHENS the neutrality proof; adding the type census catches a leak class the arm
  census misses. This is a pure tightening.

- **CH4 §6 fold (stale checkasm count):** landed at SYNTHESIS `:42-46`/`:377-378`, HANDOFF
  `:15-16`, alphaE F4/`:34`/`:79`/`:179`/`:213`. I confirmed `find skinny -name
  'checkasm_*.rs'` = 14 at HEAD (12 single-kernel + 2). The contract now says "12 + 2 = 14
  total," NOT "18." **This is the precise paper-close prevention CH6 owns:** a binding
  carry-forward ledger asserting "18 present" would seed an un-satisfiable downstream gate
  (the exact P4-class false gate this whole cycle fixes — a green/red gate that is wrong by
  construction). The fold removes that landmine. No new hole.

- **CH5 E.1 fold (shared-trait grep test-exclusion):** landed at SYNTHESIS `:48-53`/`:426`
  (the `shared_value_trait_instantiations` column now reads "≥2 real **production**
  instantiations … test-only `_proof_compiles`/`#[cfg(test)]` impls do NOT count … mirroring
  the F6 phantom-grep exclusion (`grep -v 'tests.rs|#[cfg(test)]'`) on the trait-impl axis …
  a `#[cfg(test)] impl SharedValueTrait for CssTestNode` must NOT false-green the ≥2 gate"),
  alphaE F9/`:156`. This closes the V2-fresh false-green seam (a test-only `impl` satisfying
  the ≥2 count) — the same exclusion the phantom-axis (F6) already carried, now applied to
  the trait-impl axis. **No new hole:** the exclusion narrows what counts; it cannot
  false-green.

All three folds are tightenings of measurable gates, each grounded in a re-verified disk
fact, each landed as binding text in the artefacts S-P3 consumes. The four carried V1 folds
(revert dep graph, hard-cap defaults, honest-finding (a)-(c) gate, G6 retire samply floor)
survive verbatim at HEAD. No fold loosened any gate; no fold opened a compensating hole.
ACCEPT.

---

## Consolidated CH6 V3 verdict

The αF V3 contract folded all three surviving V2 CH6/CH-cohort REVISEs as **binding gate
text in the artefacts the next pass consumes** — not as narrative gestures — and preserved
the four V1 folds and the V2 carry verbatim. Each fold and each carried gate was re-grepped
at its cited path:line at HEAD `318d9c046`, and each closes a real read-path-to-paper-close:

- **CH2 §8 (neutrality widening):** the FULL canonical alphabet (`GoogleSheets`
  un-abbreviated per `LOCKS.md`), the xtask metadata scan root, and the
  `generator_grammar_type_count` census carried into G3 + Section 2 + the gate consumer +
  invariant 5. Both new greps confirmed 0 at HEAD (satisfiable co-gates, not poison).
  RESOLVED → §1/§4/§12 ACCEPT.
- **CH4 §6 (stale checkasm count):** the disk-true "12 + 2 = 14" carried into the
  carry-forward ledger; the un-satisfiable "18" landmine removed. Confirmed 14 at HEAD.
  RESOLVED → §7/§12 ACCEPT.
- **CH5 E.1 (trait-impl test-exclusion):** the production-only requirement + the
  `grep -v 'tests.rs|#[cfg(test)]'` exclusion carried into the
  `shared_value_trait_instantiations` column. RESOLVED → §12 ACCEPT.

The contract is **strongly anti-paper-close in its measurable core AND its deferred
surface**: §0.1 gates greppable; §0.5 fallbacks named; Section 2 telemetry binds every
generalization axis to a `gate-json` reject condition (the host command + xtask surface
exist; the generalization-report extension is a real obligation); the alphaE triple's
`.bbnf`-mutation test is operational; the alphaC PRUNE close gates are RED-on-injection
meaningful and gate-scope-honest. No wave is deferred without a receiver + a gate. The
generalization is concrete and falsifiable: `json_sink_direct` must project per the
mutation test (G1); CSS must lower per `CSS_GENERATED_RS` retirement (G2, verbatim blob
confirmed live); the generator must un-fork per `RuntimeEmitterKind` deletion +
`generator_grammar_branch_count == 0` (codegen AND xtask) + `generator_grammar_type_count
== 0` (G3); Sheets must be md5-distinct + pratt-shaped from the real `google-sheets.bbnf`
via the generator only (PROVE). The sanctioned-deferred revert/cap/triumvirate carries its
binding dependency graph + halt ceiling.

Zero REJECT (no finding reversed, no premise fabricated — all re-verified live at HEAD).
Zero REVISE (the three V2 REVISEs landed as binding tightenings; no new paper-close surface
introduced; the carried V1/V2 folds survive verbatim). This lens converges for V3:
ACCEPT 12 / REVISE 0 / REJECT 0, accept rate 100%, zero orphan REVISE.

TALLY accept=12 revise=0 reject=0
