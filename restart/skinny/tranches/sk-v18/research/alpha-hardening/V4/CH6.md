# CH6 — ANTI-PAPER-CLOSE (V4) — SK-V18 Pass-Alpha Adversarial Review (fourth cycle)

Lens: CH6 Next-Tranche-Impact / ANTI-PAPER-CLOSE. Per PASS-ALPHA §3 (CH6: "does the
SK-V{N+1} contract specify revert protocol per intervention? Hard caps? Triumvirate
discipline? Is the goalset measurable + verifiable from the bench gate?") + ORCHESTRATOR
§3W/§3Z. Cycle V4 over the αF V4 contract that FOLDED the three surviving V3 REVISEs:
(1) CH5 §C.5/§F.7 — the BLOCKING second-x86-surface fold (P1 widened crate-wide);
(2) CH2 §8.1 — the arm-census reach claim scoped honestly (structural row-count check
substituted for the over-stated neutral-table reach); (3) CH1 §αD / CH7 §4 — the αD:85
stale checkasm "18 → 14" research-artefact fix — atop the V1+V2+V3 folds.

Reviewed: `sk-v18/research/alpha/{alphaA-results-extraction.md, alphaB-competitor-deltas.md,
alphaC-redress-digest.md, alphaD-validated-invalidated.md, alphaE-candidate-shortlist.md}`
+ `SYNTHESIS.md` + `HANDOFF.md`. (No `alphaF-*.md` exists by that name — per PASS-ALPHA
§2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md`, both present at the tranche
top level, both reviewed. Confirmed correct structure across V1/V2/V3 CONSOLIDATED. The
literal α-F filename absence is NOT a defect.)

**Lens mandate (binding):** no wave deferred without a receiver + a gate; generalization
is concrete (`json_sink_direct` actually projects, CSS actually lowers, the generator
actually un-forks); the goalset is telemetry-bound + bench-verifiable; the honest-finding
escape is not a paper-close hatch; revert / hard-cap / triumvirate are specified or
contract-sanctioned-deferred with a binding handoff.

**Posture for V4 (non-rubber-stamp).** V3 CH6 converged at 12 ACCEPT / 0 REVISE / 0
REJECT. V4 is NOT a confirmation pass by default. The lens re-greps every ground-truth
premise at HEAD `318d9c046`, re-greps each V4 fold site, and re-disposes every CH6-owned
section independently. The threshold V4 questions:
1. Did the BLOCKING CH5 §C.5 P1 fold — the most consequential V3→V4 change — land as a
   SATISFIABLE gate, i.e. does the deletion-target obligation match what the (now widened)
   verify grep actually catches?
2. Did the V4 folds introduce **any new paper-close surface** (the failure mode where a
   tightening on one axis opens a hole on another)?
3. Do the carried V1/V2/V3 folds (revert dep graph, hard-cap defaults, honest-finding
   (a)-(c) gate, G6 retire-branch samply floor, PROVE-Sheets litmus, telemetry binding)
   survive **verbatim** at the current HEAD bracket?

## Ground-truth re-verification (re-grepped at HEAD `318d9c046`)

The V4-critical premises (the second x86 surface) confirmed LIVE — not inherited, re-run:

- `skinny/crates/bbnf-simd/ext/x86/` = 4 files: `bbnf.asm` (23 868 B), `x86inc.asm`
  (59 546 B), `x86util.asm` (22 889 B), `LICENSE-VENDOR` (3 142 B) ≈ 106 KB. CONFIRMED —
  the second x86 surface the V4 fold widens P1 to delete is real on disk.
- `skinny/crates/bbnf-simd/build.rs` (3 784 B) PRESENT; `:1` "assembles vendored +
  authored x86_64 .asm sources"; `:19-20` exits early on non-x86_64; `:28-30`
  `rerun-if-changed=ext/x86/{x86inc,x86util,bbnf}.asm`. CONFIRMED (the nasm driver).
- `skinny/crates/bbnf-simd/src/lib.rs:247` "Contract documented in ext/x86/bbnf.asm".
  CONFIRMED (the (d) re-home target).
- `skinny/crates/bbnf-simd/src/x86_64/` = 24 files. CONFIRMED (the (a) target, V3 carry).
- `grammar_provider.rs:40` `RuntimeEmitterKind`; `runtime_generator.rs:701`
  `const CSS_GENERATED_RS: &str = r#"`; `tape/mod.rs:175` `ValueRef<… K = AnyKind,
  G: EventGrammar = AnyGrammar>`; `json/generated.rs parse_w11_1_number` = 7;
  `google-sheets.bbnf` = 7 681 B present, NOT in skinny tree; `sheets_witness/` = 25 LOC;
  `find skinny -name 'checkasm_*.rs'` = 14. ALL CONFIRMED unchanged from V3.
- Arm census `rg ... skinny/crates/codegen/src skinny/xtask/src` (full canonical alphabet)
  → 0 at HEAD; grammar-named-type census → 0 at HEAD. CONFIRMED satisfiable (real
  co-gates, the `emitter_fork_present == false` deletion grep is what bites G3).

The ground truth is solid. My dispositions concern the *forward* contract's paper-close
surface — and the V4 BLOCKING fold introduced ONE new hole (§1) of exactly the class the
fold existed to fix on the V3 axis, plus a verifiability degradation in the fold-ledger
narrative (§13).

---

## Disposition summary (per reviewable section)

| # | Section | Path:line | V1 | V2 | V3 | V4 |
|---|---|---|---|---|---|---|
| 1 | SYNTHESIS §0.1 P1 close-condition (the BLOCKING V4 fold) | `SYNTHESIS.md:246`; `:491` | ACCEPT | ACCEPT | ACCEPT | **REVISE** |
| 2 | SYNTHESIS §0.1 close-condition gate table (G1-G6/P2-P5/PROVE/H1) | `SYNTHESIS.md:247-262` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 3 | SYNTHESIS §0.3 receiver goalset (G6 retire + PROVE fallback) | `SYNTHESIS.md:296-304` | REVISE | ACCEPT | ACCEPT | ACCEPT |
| 4 | SYNTHESIS §0.5 generalization litmus + fallbacks | `SYNTHESIS.md:383-397` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 5 | SYNTHESIS Section 2 telemetry binding + gate consumer | `SYNTHESIS.md:472-529` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 6 | SYNTHESIS Section 3 trajectory + revert dependency graph | `SYNTHESIS.md:567-583` | REVISE | ACCEPT | ACCEPT | ACCEPT |
| 7 | HANDOFF "Next Move" + revert/cap deferral carry | `HANDOFF.md:275-342` | REVISE | ACCEPT | ACCEPT | ACCEPT |
| 8 | alphaE candidate shortlist gates + sequencing | `alphaE:44-223` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 9 | alphaC PRUNE-wave close gates | `alphaC` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 10 | The honest-finding escape (anti-paper-close hatch) | `SYNTHESIS.md:262`; `HANDOFF.md:328-330`; `alphaE:223` | REVISE | ACCEPT | ACCEPT | ACCEPT |
| 11 | PROVE-Sheets gate (the generalization litmus) | `SYNTHESIS.md:257,303`; `alphaE:183` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 12 | G6 acceleration-wiring / orphan-kernel gate | `SYNTHESIS.md:256,302`; telemetry `:490` | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 13 | V4 fold integrity (3 V3 REVISEs landed; ledger self-citation drift) | `SYNTHESIS.md:55-96` | — | — | — | **REVISE** |

Tally: ACCEPT 11, REVISE 2, REJECT 0.

---

## §1 — SYNTHESIS §0.1 P1 close-condition (the BLOCKING V4 fold) — REVISE

`SYNTHESIS.md:246` (gate row) + `:491` (`x86_tree_deleted` telemetry). This is the
single most consequential V4 fold — the V3 CH5 §C.5/§F.7 finding that the prior P1
scoped x86 deletion to `src/x86_64/` ONLY while a vendored x86 ASM tree (`ext/x86/`), a
nasm `build.rs` driver, and the `lib.rs:247` reference survived a `src/`-scoped verify
grep. The V4 fold is correct in substance (the second surface is real on disk — re-verified
above) and the widening to a crate-wide verify grep is the right move. **But the fold left
the deletion-target obligation at four items while widening the verify grep crate-wide,
and the wider grep now catches active x86 surfaces the four-item list does NOT require the
receiver to remove. The redefined gate is un-satisfiable by the obligation as written —
the mirror-image of the exact V3 escape this fold exists to fix.**

The P1 close condition (`:246`) and the `x86_tree_deleted` telemetry (`:491`) BOTH
enumerate exactly four removal targets:
(a) `src/x86_64/`; (b) `ext/x86/`; (c) `build.rs` delete-or-neutralize; (d) `lib.rs:247`
comment re-home. The verify command is `grep -riE 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` → "returns only aarch64-neutral comments (none active)."

Re-grepped at HEAD, that crate-wide grep also fires on at least three ACTIVE (non-comment)
surfaces the four-item list omits:

1. **`Cargo.toml:19` `nasm-rs = "0.3"`** — an ACTIVE build-dependency (with `:14-16`
   companion comments referencing "x86_64 .asm" + "nasm-rs"). Deleting `build.rs` (target
   (c)) orphans this dependency but leaves the manifest line — an active, non-comment
   `nasm` token the verify grep flags. The deletion list never names removing the
   `nasm-rs` dependency from `Cargo.toml`.
2. **`src/lib.rs:5` `pub mod x86_64;` and `:285-288` the active
   `#[cfg(all(target_arch = "x86_64", target_feature = "avx512bw"))]` dispatch arm**
   (`return crate::x86_64::byte_class_from_eq_set_64::…`). Once (a) deletes
   `src/x86_64/`, this dispatch arm is a dangling reference (won't compile on an x86_64
   build) AND on aarch64 the grep flags it as active (non-comment) `x86_64` code. The
   four-item list re-homes only the `:247` doc comment — it does NOT name removing the
   `pub mod x86_64;` declaration or the cfg-gated dispatch arms in `lib.rs`.
3. **`CONCRETIZATION-REPORT.md` (18 `x86`/`avx`/`nasm` hits) + `CHECKASM-REPORT.md`** —
   in-crate doc surfaces the crate-wide grep flags; not enumerated.

So a receiver who executes exactly (a)-(d) leaves the gate RED on `Cargo.toml:19`,
`lib.rs:5`, and `lib.rs:285-288` — all active, none "aarch64-neutral comments." The gate
as redefined is **un-satisfiable by the stated obligation**, which is itself a paper-close
hazard (a gate that cannot be honestly satisfied invites a receiver to either silently
narrow the grep back or hand-wave the surviving hits as "dormant"). The V4 fold's own
grievance against V3 — "the old P1 verify grep was scoped to `…/src/`, so `ext/x86/` …
escaped it" — recurs here inverted: the grep is now wider than the deletion list.

**Concrete fix (REVISE — extend the deletion-target list to the grep's reach, both at the
P1 close row `:246` and the `x86_tree_deleted` telemetry `:491`):** add as explicit P1
removal targets (e) remove the `nasm-rs` build-dependency from `bbnf-simd/Cargo.toml`
(`:19`) AND its `:14-16` companion comments; (f) remove `src/lib.rs:5` `pub mod x86_64;`
AND the `#[cfg(target_arch = "x86_64")]` dispatch arms in `lib.rs` (e.g. `:285-288`),
leaving only the aarch64 + scalar arms; (g) scrub the in-crate doc surfaces
(`CONCRETIZATION-REPORT.md`, `CHECKASM-REPORT.md`) of active x86 narrative OR explicitly
scope the verify grep to source + manifest (`--include='*.rs' --include='Cargo.toml'`) so
the doc-surface hits are out of band and the "none active" claim is honest. This DEEPENS
the net-LOC-deleted claim further (consistent with the fold's own "net-positive
correction" framing) and makes the gate satisfiable-by-construction. Without it, P1 — the
mandatory, non-optional PRUNE gate that lands FIRST — ships as a RED-by-construction gate.

This is a REVISE, not a REJECT: the fold's direction (delete the whole x86 surface,
verify crate-wide) is correct; only the obligation/grep reach mismatch needs closing.

---

## §2 — SYNTHESIS §0.1 close-condition gate table (the remaining rows) — ACCEPT

`SYNTHESIS.md:247-262`. Every non-P1 gate row carries a concrete, greppable verifier;
the V4 folds did not loosen any. P2 (`:247`), P3 (`:248`), P4 (`:249`), P5 (`:250`) carry
exact `grep -n`/`md5`/`sort -u` verifiers. P4 (`:249`) remains the model anti-paper-close
row: "a GREEN gate is meaningful … `accepts_current_allowlist` passes ONLY because the
leaks are actually gone (not excluded)." G1 (`:251`), G2 (`:252`), G3 (`:253`), G4 (`:254`),
G5 (`:255`), G6 (`:256`), PROVE (`:257`), H1 (`:258`), the JSON guard (`:259`), invariants
(`:260`), and PASS-IMPL V4 (`:262`) each carry the named telemetry + greppable verifier.
The CH2 §8.1 V4 fold correctly relocated the relocated-overfit-seam machine-check OFF the
arm-census regex (which cannot fire on a token-free table) and ONTO the STRUCTURAL P3
row-count check (`runtime_target_rows_collapsed`, G3 (iii) `:253`) — a real tightening
that closes the false-reach the V3 contract over-claimed. ACCEPT.

---

## §3 — SYNTHESIS §0.3 receiver goalset — ACCEPT (V1 REVISE D-CH6-1 fold survives)

`SYNTHESIS.md:296-304`. The two under-bound disjunctive receivers V1 flagged remain
self-contained at HEAD:

- **G6 receiver (`:302`)** still reads: wire `find_css_significant`/`find_comment_close`
  into the CSS hot path AT ADMISSION with a same-wave consumer, OR honestly retire "**with
  a samply attribution row proving the kernel's target leaf is non-top-N on the benched
  CSS hot path** (the retire branch is gated on a MEASUREMENT, not an assertion — it cannot
  close G6 by marking all NEON 'retired' with zero acceleration wired)." Re-grepped
  verbatim. The escape hatch stays closed.
- **PROVE receiver (`:303`)** still carries the §0.5 fallback inline: "if Sheets cannot be
  emitted via the generator ONLY, the generalization is NOT real — surface honestly, do
  NOT stub-prove; do NOT hand-write a `_GENERATED_RS` Sheets block." Re-grepped verbatim.

A receiver reading only §0.3 to assign owner paths cannot reach a paper-close read-path on
G6 or PROVE. The V4 folds touched P1 (§0.3 `:296` now carries the crate-wide widening) and
the neutrality grep but did not weaken the G6/PROVE receivers. (Note: §0.3 `:296` carries
the SAME four-item-vs-wider-grep mismatch as §1 — the fix in §1 must propagate to `:296`
as well; flagged there, not double-counted.) ACCEPT.

---

## §4 — SYNTHESIS §0.5 generalization litmus + per-axis fallbacks — ACCEPT

`SYNTHESIS.md:383-397`. The strongest anti-paper-close section, unchanged in substance.
Every axis row carries Current / Target / Expected-intervention / **Fallback-if-not-met**,
every fallback names a concrete non-paper-close action ("surface honestly as a named
validated grammar-parameterized primitive (HANDOFF §6), do NOT paper-close; do NOT
silently retain the hand-written blob" `:385`; "the generalization is NOT real — surface
honestly, do NOT stub-prove" `:387`; "REJECT the trait shape, report, do NOT force a
Lock-1 violation" `:388`). The aarch64-only axis row (`:389`) was correctly updated to name
all three x86 surfaces (`src/x86_64/` AND `ext/x86/` AND nasm `build.rs`) — consistent with
the V4 P1 widening, modulo the §1 enumeration gap which is a P1-row obligation defect, not
a litmus defect. The litmus is explicitly binary-structural (`:380`), gated on PRESERVING
the >SOTA; the R10 criterion (`:391-397`) restates the conjunction correctly. ACCEPT.

---

## §5 — SYNTHESIS Section 2 telemetry binding + gate consumer — ACCEPT

`SYNTHESIS.md:472-529`. The load-bearing answer to CH6's "is the goalset measurable +
verifiable from the bench gate?" — YES. Every generalization axis binds to a named
telemetry column AND a `gate-json` reject condition. The V3→V4 folds strengthened the
relocated-seam machine-check: `runtime_target_rows_collapsed` (`:481`) is now the STRUCTURAL
row-count gate the arm census cannot do (correctly, per CH2 §8.1 — verified the arm-census
regex is syntactically incapable of firing on a token-free `RuntimeTarget` table), and
`generator_grammar_type_count` (`:482`) catches the re-emitted-grammar-named-type seam the
arm census misses (confirmed 0 at HEAD — a real satisfiable co-gate, not poison).
`json_rich_navigation_preserved` (`:485`) closes the ≥2-impl LCD-flatten false-green;
`shared_value_trait_instantiations` (`:484`) carries the production-only + `grep -v
'tests.rs|#[cfg(test)]'` exclusion (the V3 CH5 E.1 fold) so a test-only `impl` cannot
false-green the ≥2 gate; `x86_tree_deleted` (`:491`) carries the crate-wide redefinition
(modulo the §1 enumeration gap, flagged there); `acceleration_at_admission` (`:490`) with
`cfg-test-only` = NO-GO closes the dead-at-admission paper-close; `sheets_grammar_shape`
(`:489`) makes "genuinely different shape" machine-checkable. The gate consumer (`:498-529`)
names the EXACT reject conditions. The `gate-json` host command exists (`xtask/src/main.rs`,
xtask surface re-verified present); the `--skv18-generalization-report` extension is a real
namable S-P3 obligation. ACCEPT.

---

## §6 — SYNTHESIS Section 3 trajectory + revert dependency graph — ACCEPT (V1 REVISE D-CH6-2 fold survives)

`SYNTHESIS.md:567-583`. The revert *dependency graph* survives verbatim in the goalset S-P3
consumes. `:573-578` reads: "S-P3's revert protocol MUST encode the **entry-gate dependency
graph** (PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1 …): a wave that fails its exit gate
BLOCKS every downstream wave that entry-gates on it … in particular G1 failure blocks
G2/G3/G4/PROVE, and G3 (un-fork) failure blocks PROVE (which emits Sheets THROUGH the
un-forked generator)." The closing sentence (`:581-583`) makes intent explicit: "the
difference between a legitimate handoff and a paper-close." The deferral itself remains
PASS-ALPHA §4.4-sanctioned (CH6 cannot REJECT the deferral as out-of-bounds; §4.4 places
owner-paths/gates/caps/revert in `SPEC.md` authored by S-P3). The fold supplies the binding
carry that makes the deferral legitimate. The hard-cap defaults (`:578-581`,
research/plan/redress 20/15/30, "at 0.9N commit, at N halt", MED-HIGH carve-out) survive.
The V4 folds did not touch this section. ACCEPT.

---

## §7 — HANDOFF "Next Move" + revert/cap deferral carry — ACCEPT (V1 REVISE D-CH6-3 fold survives)

`HANDOFF.md:275-342`. Both binding carries survive verbatim. `:332-342` reads: "Revert
protocol, hard caps, and per-wave triumvirate discipline are sanctioned-deferred to S-P3 …
with two binding carries … 1. **Revert dependency graph** … G1 failure blocks
G2/G3/G4/PROVE; G3 un-fork failure blocks PROVE. 2. **Hard-cap defaults:** … 20/15/30 min,
'at 0.9N commit, at N halt' unless the wave's risk class (the Sheets/NEON cluster is
MED-HIGH per alphaE) justifies a documented larger cap — so no SK-V18 wave dispatches
uncapped." The same-wave-consumer rule (`:298` "Each primitive lands WITH its hot-path
consumer in the same commit (no orphan kernels)") and the per-wave >SOTA re-proof (`:298-300`)
remain stated. The V4 fold updated the §status header (`:7-26`) to cite the three V3 REVISEs
folded, and the P1 receiver (`:98-103`) + invariant 3 (`:242-244`) + gate consumer (`:315`)
to the crate-wide x86 deletion — all consistent with the V4 widening, modulo the same
four-item-vs-grep enumeration gap (`:98-103` carries the same (a)-(d)-only list; the §1 fix
must propagate here). No loosening introduced beyond the §1 gap. ACCEPT (the gap is a P1-row
obligation defect counted once in §1, not re-disposed here).

---

## §8 — alphaE candidate shortlist gates + sequencing — ACCEPT

`alphaE:44-223`. The V4 fold ledger resolves the αE-touching V3 REVISEs in place,
orphan-free. Critical for CH6:
- The falsifiability triple's gate #2 (the `.bbnf`-mutation derivation test — "mutate the
  `.bbnf` → the regenerated `generated.rs` changes correspondingly; a const courier cannot
  pass this") remains an *operational* derivation test, not an assertion — the right
  anti-paper-close instrument against the verbatim-blob-with-honest-comment defect.
- The cross-cutting exit-gate-blocks-successor clause carries the CH6 revert dependency.
- The honest-finding sharpening (`:223`) carries the (a)-(c) gate: "a 'primitive' that is a
  relabeled blob without `.bbnf`-invocation + parameterization + a reference is REJECTED to
  REDRESS."
- F4 corrects the checkasm count to the disk-true 14 — re-confirmed `find skinny -name
  'checkasm_*.rs'` = 14 at HEAD; the V4 fold propagated this into αD (§13).
- The risk-weighted close prediction handles litmus-failure without paper-closing ("If B4's
  Sheets litmus fails, SK-V18 does NOT paper-close — it surfaces 'generator is still
  JSON+CSS-overfit,' iterates B1/B2, and B4 re-enters (V≤5 ceiling)").
ACCEPT.

---

## §9 — alphaC PRUNE-wave close gates — ACCEPT

`alphaC`. Each PRUNE wave (P1-P5) carries live evidence + delete-or-fix obligation + a
close gate that makes the prune meaningful. P4's close gate remains the model
anti-paper-close ("`accepts_current_allowlist` PASSES *after* the rebuild because the
scanned surface is genuinely neutral, not because the dirty files are excluded"), combined
with alphaE P4-exit's injected-token RED test (re-run after re-introducing a `JsonSink`
token must turn RED — green-for-the-right-reason, verified two ways). The P3
collapse-vs-differentiate fold makes COLLAPSE-to-one the DEFAULT and gates "N distinct
generated.rs" behind "N distinct `.bbnf` roots genuinely authored." The Sheets broadcast
pre-block binds the Sheets corpus to per-corpus N≥50-cold-median (PERMANENT pre-block).
(Note: the alphaC P1 narrative inherits the same x86-surface-enumeration question as
SYNTHESIS §1; the binding gate is SYNTHESIS §0.1, so the fix lands there. Not double-counted
as a separate alphaC REVISE — the αC PRUNE-wave gate STRUCTURE is sound; only the P1
target-list completeness, owned by §0.1, needs the §1 fix.) ACCEPT.

---

## §10 — The honest-finding escape (the anti-paper-close hatch itself) — ACCEPT (V1 REVISE D-CH6-4 fold survives)

`SYNTHESIS.md:262`; `HANDOFF.md:328-330`; `alphaE:223`. The single largest residual
paper-close surface remains gated. The PASS-IMPL V4 row (`SYNTHESIS.md:262`) carries the
(a)-(c) qualification gate verbatim: "a 'named validated grammar-parameterized primitive'
qualifies ONLY if: (a) the grammar `.bbnf` INVOKES it by name … NOT a free-standing const
the emitter splices; (b) it is parameterized by grammar-derived DATA … NOT a fixed body;
(c) it carries the same `verbatim_blob_present == false` telemetry … A primitive failing
(a)-(c) is a relabeled hand-written blob — REJECT, REDRESS, do NOT close. Without this gate
the escape is the single largest paper-close surface in the contract." A relabeled
`CSS_GENERATED_RS` fails (a) (free-standing const the emitter splices — re-confirmed at
`runtime_generator.rs:701`), (b) (fixed body), and (c) (it IS the verbatim blob). Reinforced
at `alphaE:223` and carried in the §0.5 fallbacks. The V4 folds did not touch this gate.
The escape is a genuine honest-finding path, not a paper-close hatch. ACCEPT.

---

## §11 — PROVE-Sheets gate (the generalization litmus) — ACCEPT

`SYNTHESIS.md:257,303`; `alphaE:183`; `alphaC` Sheets section. Correctly
non-paper-closeable, strengthened by `sheets_grammar_shape == pratt-operator` (the shape
disclosure making "genuinely different shape" machine-checkable — a flat-stream/tree Sheets
REJECTed as third-JSON hollowing). The gate binds md5(Sheets) ≠ JSON ≠ CSS; Sheets value
type instantiates the G4 trait; ZERO hand-authored runtime Rust (`grep -c 'const.*_RS.*r#'
codegen/src` for any Sheets blob → 0); via the generator ONLY; the canonical neutrality
grep AND type census stay 0. The adoption of the EXISTING real Pratt `google-sheets.bbnf`
(re-confirmed present, 7 681 B, NOT in the skinny tree) over a fresh stub is the correct
anti-hollowing move. The §0.5 fallback is inline in the §0.3 receiver (§3). ACCEPT.

---

## §12 — G6 acceleration-wiring / orphan-kernel gate — ACCEPT

`SYNTHESIS.md:256,302`; telemetry `acceleration_at_admission` (`:490`); `alphaE:183`. The
acceleration-wiring gate corrects the SK-V17 W3 overstatement (NEON "acceleration" dead at
admission — re-confirmed `find_css_significant`/`find_comment_close` are `#[cfg(test)]`-only)
and is machine-checkable: "any kernel claiming acceleration is reached at admission (grep
the generated hot path, not tests)." The `acceleration_at_admission ∈
{admission,scalar-passthrough-labeled,retired}` enum with `cfg-test-only` = NO-GO closes the
dead-at-admission paper-close. The retire branch's measured non-top-N samply floor is folded
into the §0.3 receiver; the same-wave-consumer rule pre-empts the orphan-kernel pattern.
ACCEPT.

---

## §13 — V4 fold integrity (the three V3 REVISEs landed; ledger self-citation drift) — REVISE

The threshold V4 question: did the three V3→V4 folds land as binding gate text, and did any
introduce a new paper-close surface? Re-grepped each fold site:

- **CH5 §C.5/§F.7 fold (BLOCKING second-x86-surface):** landed at SYNTHESIS `:55-75` (the
  V3→V4 fold-ledger), `:246` (P1 close), `:389` (§0.5 axis row), `:491`
  (`x86_tree_deleted`), §0.3 `:296`, HANDOFF `:13-16`/`:98-103`/`:242-244`/`:315`. The
  second surface is real on disk (re-verified). The widening direction is correct. **BUT it
  opened a new hole** — the deletion-target list stayed at four items while the verify grep
  widened crate-wide, leaving `nasm-rs`/cfg-arm/doc surfaces active-but-unremoved (the
  full finding + fix is §1). This is the new paper-close surface the V4 fold introduced; it
  is REVISE-not-REJECT (direction correct, reach mismatch).

- **CH2 §8.1 fold (arm-census reach scoped honestly):** landed at SYNTHESIS `:76-91`,
  `:253` G3 (iii), `:480-481` telemetry, gate consumer `:520-522`, HANDOFF `:264`. I
  confirmed the arm-census regex `match\s+\w+\s*\{[^}]*Json\s*=>…` cannot fire on a
  token-free `RuntimeTarget` table, so substituting the STRUCTURAL `runtime_target_rows_collapsed`
  row-count check is correct. The grep is kept for its real value (self-disclosing-token
  branches); only the over-stated reach claim is corrected. **No new hole** — this is a pure
  honesty correction that REPLACES a false-reach assertion with a real structural check.

- **CH1 §αD / CH7 §4 fold (αD:85 stale "18 → 14"):** the binding contract (Section 1,
  actual line `:434`) already carried "12 single-kernel + 2 = 14"; the lone surviving "18"
  was in `alphaD-validated-invalidated.md:85`, corrected there (αD V4-fold R1 ledger,
  `alphaD:276`/`:282`). Re-confirmed `find skinny -name 'checkasm_*.rs'` = 14 at HEAD; αD
  now carries 14 at `:105`/`:231`. **No new hole** — a research-artefact fix that removes
  an un-satisfiable "18-present" P4-class landmine from a feeder ledger.

Two of three V4 folds are clean tightenings; the BLOCKING fold introduced the §1 hole. But
a SECOND, distinct V4-fold defect surfaced on re-grep: **the V4 fold-ledger prose carries
stale internal self-citations.** The V4 fold narrative (SYNTHESIS `:77-93`) back-references
prior-cycle line numbers that the V4 edits themselves shifted:
- `:77` cites "G3 close condition :201" — G3 is now at line **253**.
- `:78` cites "`generator_grammar_branch_count` column :423" — it is now at line **480**.
- `:93` cites "Section 1 :377-378" for the "12 + 2 = 14" — it is now at line **434**.

These stale refs are in the *narrative fold-ledger* (describing what V3 asserted), NOT in
the *binding gate text* the machine gate consumes — the gate rows (§0.1), telemetry columns
(Section 2), and gate consumer use NAMED telemetry columns + greppable commands, not line
numbers, so the executable gate is UNAFFECTED. But for an anti-paper-close lens, a contract
that is itself the authority degrading its own self-citation accuracy by ~50-60 lines per
ref erodes verifiability — a downstream reader auditing "did the V4 fold land at the cited
site?" is sent to the wrong line. This is a documentation-accuracy REVISE, not a gate hole.

**Concrete fix (REVISE):** update the SYNTHESIS V3→V4 fold-ledger prose (`:77`, `:78`,
`:93`) to the current line locations (G3 → `:253`; `generator_grammar_branch_count` →
`:480`; Section 1 checkasm-14 → `:434`), OR replace the line-number citations with stable
anchors (section/column names — e.g. "the G3 close condition row," "the
`generator_grammar_branch_count` telemetry column," "the Section 1 checkasm ledger") that do
not drift on subsequent folds. The latter is preferable — it makes the ledger self-citation
fold-stable for V5+.

---

## Consolidated CH6 V4 verdict

The αF V4 contract folded all three surviving V3 REVISEs as binding text. Two of the three
(CH2 §8.1 arm-census honesty; CH1 αD checkasm 18→14) are clean tightenings that close real
read-paths-to-paper-close and introduce no new hole — re-grepped and confirmed at HEAD
`318d9c046`. The third — the BLOCKING CH5 §C.5 second-x86-surface fold — is correct in
direction (the ~106 KB `ext/x86/` ASM tree + nasm `build.rs` + `lib.rs:247` are real on
disk; the crate-wide verify grep is the right instrument) but **incomplete in obligation:
the deletion-target list stayed at four items while the verify grep widened crate-wide, so
the gate is un-satisfiable by the stated obligation** — the active `nasm-rs` Cargo.toml
dependency, the `pub mod x86_64;`/cfg-dispatch arms in `lib.rs`, and the in-crate doc
surfaces are flagged by the grep but not enumerated for removal. This is the mirror-image
of the exact V3 escape the fold exists to fix, and it lands on P1 — the mandatory, lands-FIRST
PRUNE gate. REVISE (§1), with a concrete fix: extend the P1 target list (and the
`x86_tree_deleted` telemetry) to the grep's reach, or scope the grep to source+manifest.

A second V4-fold defect: the V3→V4 fold-ledger prose carries stale internal self-citations
(G3 `:201`→`:253`; branch-count col `:423`→`:480`; checkasm-14 `:377-378`→`:434`) — a
verifiability degradation in the authority document, machine-gate-unaffected but
audit-misleading. REVISE (§13), fixable by re-numbering or by switching to fold-stable
section/column anchors.

The contract's measurable CORE remains strongly anti-paper-close: §0.1 gates greppable
(modulo P1); §0.5 fallbacks named; Section 2 telemetry binds every generalization axis to a
`gate-json` reject condition (host command + xtask surface exist; the
`runtime_target_rows_collapsed` STRUCTURAL check correctly replaces the over-claimed
arm-census reach); the alphaE `.bbnf`-mutation derivation test is operational; the alphaC
PRUNE close gates are RED-on-injection meaningful; the honest-finding escape carries its
(a)-(c) gate; the sanctioned-deferred revert/cap/triumvirate carries its binding dependency
graph + halt ceiling. No wave is deferred without a receiver + a gate. The two REVISEs are
both fixable by tightening (not loosening) and neither reverses a finding nor fabricates a
premise — all re-verified live at HEAD.

This lens does NOT converge for V4: ACCEPT 11 / REVISE 2 / REJECT 0, accept rate 84.6%
(below the §3Z ≥95% threshold). The two REVISEs must fold into V5: (1) the P1
deletion-target/grep-reach mismatch (§1, the consequential one — a RED-by-construction
mandatory gate); (2) the fold-ledger self-citation drift (§13, the verifiability one).

TALLY accept=11 revise=2 reject=0
