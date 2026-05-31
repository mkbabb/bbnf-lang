# CH6 — ANTI-PAPER-CLOSE (V5) — SK-V18 Pass-Alpha Adversarial Review (fifth cycle)

Lens: CH6 Next-Tranche-Impact / ANTI-PAPER-CLOSE. Per PASS-ALPHA §3 (CH6: "does the
SK-V{N+1} contract specify revert protocol per intervention? Hard caps? Triumvirate
discipline? Is the goalset measurable + verifiable from the bench gate?") + ORCHESTRATOR
§3W/§3Z. Cycle V5 over the αF contract that FOLDED the two surviving V4 CH6 REVISEs —
(1) §1 the BLOCKING P1 deletion-target/grep-reach mismatch (a RED-by-construction
mandatory gate); (2) §13 the V3→V4 fold-ledger self-citation drift — atop the
V1+V2+V3+V4 folds. The V4 CHALLENGE wave closed sub-95% (89A/9R/0 = 90.8%,
non-converging); the V4 CONSOLIDATED records the clusters; V5 is the clean confirming
pass §3Z requires (≥95% ×2 consecutive).

Reviewed: `sk-v18/research/alpha/{alphaA-results-extraction.md, alphaB-competitor-deltas.md,
alphaC-redress-digest.md, alphaD-validated-invalidated.md, alphaE-candidate-shortlist.md}`
+ `SYNTHESIS.md` + `HANDOFF.md`. (No `alphaF-*.md` exists by that name — per PASS-ALPHA
§2/§6 the α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md`, both present at the tranche
top level, both reviewed. Confirmed correct structure across V1/V2/V3/V4 CONSOLIDATED.
The literal α-F filename absence is NOT a defect.)

**Lens mandate (binding):** no wave deferred without a receiver + a gate; generalization
is concrete (`json_sink_direct` actually projects, CSS actually lowers, the generator
actually un-forks); the goalset is telemetry-bound + bench-verifiable; the honest-finding
escape is not a paper-close hatch; revert / hard-cap / triumvirate are specified or
contract-sanctioned-deferred with a binding handoff.

**Posture for V5 (non-rubber-stamp).** V4 CH6 returned 11A/2R/0 (84.6%). V5 is NOT a
confirmation pass by default. The lens re-greps every ground-truth premise at HEAD
`318d9c046`, re-greps each V5 fold site in the binding gate text AND the αA/αE feeders,
and re-disposes every CH6-owned section independently. The threshold V5 questions:
1. Did the BLOCKING V4 §1 fold — the P1 deletion-target/grep-reach mismatch — land as a
   SATISFIABLE-by-construction gate (deletion list now reach-matched to the verify grep),
   AND did the orphan-propagation into the αA/αE feeders land (the V4 §1 fold also being
   the CH1 §αE / CH3 / CH7 §1 orphan)?
2. Did the §13 fold-ledger self-citation drift get switched to fold-stable anchors?
3. Did the V5 edits introduce **any new paper-close surface** (the recurring failure mode
   where a tightening on one axis opens a hole on another — the exact pattern V4 §1
   itself was)?
4. Do the carried V1/V2/V3/V4 folds (revert dep graph, hard-cap defaults, honest-finding
   (a)-(c) gate, G6 retire-branch samply floor, PROVE-Sheets litmus, telemetry binding,
   the CH2 F16 `runtime_target_rows_collapsed` projection widening) survive **verbatim**
   at the current HEAD bracket and remain SATISFIABLE?

## Ground-truth re-verification (re-grepped at HEAD `318d9c046`)

The V5-critical premises — the (a)-(g) x86 removal surfaces the V4 §1 fold added to the
P1 obligation — re-verified LIVE on disk, each confirmed present (so the extended
deletion list is grounded, not asserted):

- `skinny/crates/bbnf-simd/ext/x86/` = 4 files (`bbnf.asm`, `x86inc.asm`, `x86util.asm`,
  `LICENSE-VENDOR`). CONFIRMED — target (b).
- `skinny/crates/bbnf-simd/src/x86_64/` present. CONFIRMED — target (a).
- `bbnf-simd/Cargo.toml:19` `nasm-rs = "0.3"` ACTIVE build-dep + `:14-16` companion
  comments ("when authored x86_64 .asm sources land … build.rs assembles them via
  nasm-rs"). CONFIRMED — target (e), the V4 §1 enumeration gap, now on the list.
- `bbnf-simd/src/lib.rs:5` `pub mod x86_64;`; `:247` `// Contract documented in
  ext/x86/bbnf.asm`; `:285-287` `#[cfg(all(target_arch = "x86_64", target_feature =
  "avx512bw"))]` … `return crate::x86_64::byte_class_from_eq_set_64::…`. CONFIRMED —
  targets (d) + (f), the V4 §1 dangling-cfg-arm gap, now on the list.
- `bbnf-simd/build.rs` (3 784 B) PRESENT (the nasm driver). CONFIRMED — target (c).
- `bbnf-simd/CONCRETIZATION-REPORT.md` + `CHECKASM-REPORT.md` carry x86/avx/nasm hits.
  CONFIRMED — target (g) (scrub-or-scope-grep), the in-crate doc surface.

Every (a)-(g) surface the V4 §1 fix named is live on disk. The crate-wide verify grep
`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` is now reach-matched to a deletion list that covers every active
hit — the gate is satisfiable-by-construction (the V4 §1 RED-by-construction defect is
closed).

The F16 / CH2-V5 premise (the `runtime_target_rows_collapsed` projection widening, which
co-gates G3 and is CH6-relevant because it must remain SATISFIABLE for the deferred-to-S-P3
receiver) re-verified LIVE: `skinny/xtask/src/regen_css.rs:35` `const TARGETS: &[RuntimeTarget]`
= 7 css_l4 rows, all sharing `grammar_name: "css_l4"` / `entry_rule: "stylesheet"` /
`source_roots: CSS_L4_ROOTS` (the 2 INVARIANT columns) but each carrying a DISTINCT
`fact_schema` + `output_plane` (e.g. `css-l4-at-rules-media-facts-v1` …
`css-l4-vendor-custom-facts-v1`). CONFIRMED — the widened `count(distinct
config-tuple-minus-output_dir) == 1 per grammar_name` check is correctly RED today (7
distinct `fact_schema`), and P3 is the named receiver that turns it GREEN.

The ground truth is solid. My V5 dispositions concern whether the two V4 REVISEs folded
orphan-free into the BINDING gate text + the feeders, and whether the V5 edits opened any
new paper-close surface. They did not.

---

## Disposition summary (per reviewable section)

| # | Section | Path:line | V1 | V2 | V3 | V4 | V5 |
|---|---|---|---|---|---|---|---|
| 1 | SYNTHESIS §0.1 P1 close-condition (the V4-BLOCKING fold, now reach-matched) | `SYNTHESIS.md:315`; `:563` | ACCEPT | ACCEPT | ACCEPT | **REVISE** | ACCEPT |
| 2 | SYNTHESIS §0.1 close-condition gate table (G1-G6/P2-P5/PROVE/H1) | `SYNTHESIS.md:315-331` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 3 | SYNTHESIS §0.3 receiver goalset (G6 retire + PROVE fallback) | `SYNTHESIS.md:355-373` | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 4 | SYNTHESIS §0.5 generalization litmus + fallbacks | `SYNTHESIS.md:447-469` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 5 | SYNTHESIS Section 2 telemetry binding + gate consumer | `SYNTHESIS.md:530-604` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 6 | SYNTHESIS Section 3 trajectory + revert dependency graph | `SYNTHESIS.md:605-657` | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 7 | HANDOFF "Next Move" + revert/cap deferral carry | `HANDOFF.md:293-365` | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 8 | alphaE candidate shortlist gates + sequencing (incl. F15/F16 feeder fold) | `alphaE:94-241` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 9 | alphaC PRUNE-wave close gates | `alphaC` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 10 | The honest-finding escape (anti-paper-close hatch) | `SYNTHESIS.md:331`; `HANDOFF.md:351-353`; `alphaE:223` | REVISE | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 11 | PROVE-Sheets gate (the generalization litmus) | `SYNTHESIS.md:326,372`; `alphaE:183` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 12 | G6 acceleration-wiring / orphan-kernel gate | `SYNTHESIS.md:371`; telemetry `:566` | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| 13 | V5 fold integrity (2 V4 REVISEs landed; αA/αE orphan-propagation; ledger anchors) | `SYNTHESIS.md:99-165`; `alphaA:13-63`; `alphaE:94,104,241` | — | — | — | **REVISE** | ACCEPT |

Tally: ACCEPT 13, REVISE 0, REJECT 0.

---

## §1 — SYNTHESIS §0.1 P1 close-condition (the V4-BLOCKING fold, now reach-matched) — ACCEPT (V4 §1 REVISE folded)

`SYNTHESIS.md:315` (P1 close row) + `:563` (`x86_tree_deleted` telemetry). The V4 §1
REVISE — the single most consequential V4 finding — is FOLDED into the binding gate text.
V4's grievance: the P1 deletion-target list stayed at four items (a)-(d) while the verify
grep widened crate-wide, leaving the active `nasm-rs` Cargo.toml dep, the `pub mod
x86_64;`/cfg-dispatch arms in `lib.rs`, and the in-crate doc surfaces grep-flagged but
not enumerated for removal — a RED-by-construction gate (the mirror-image of the V3
escape P1 exists to fix).

The V5 P1 close row (`:315`) now enumerates SEVEN reach-matched removal targets:
(a) `src/x86_64/` (24 files); (b) `ext/x86/` (the ~3000-LOC vendored ASM tree);
(c) `build.rs` deleted-or-neutralized; (d) the `lib.rs:247` doc-comment re-homed;
**(e) the `nasm-rs = "0.3"` build-dep removed from `Cargo.toml:19` AND its `:14-16`
companion comments**; **(f) `src/lib.rs:5 pub mod x86_64;` removed AND the
`#[cfg(all(target_arch="x86_64", …))]` dispatch arms (e.g. `:285-288`) removed, leaving
only the aarch64 + scalar arms**; **(g) the in-crate doc surfaces
(`CONCRETIZATION-REPORT.md`, `CHECKASM-REPORT.md`) scrubbed of active x86 narrative OR
the verify grep scoped to source+manifest (`--include='*.rs' --include='Cargo.toml'`)**.
The verify command is now `grep -riE --include='*.rs' --include='Cargo.toml'
'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` with the explicit invariant: "every
active hit the grep flags is on the (a)-(g) removal list." The `x86_tree_deleted`
telemetry (`:563`) carries the identical reach — `Cargo.toml:19` nasm-rs removed,
`lib.rs:5`/`:285-288` removed, doc surfaces scrubbed-or-out-of-band, "the deletion list
is reach-matched to the verify grep so the gate is satisfiable-by-construction, CH6 V4 §1."

Re-grepped each (a)-(g) target live at HEAD (Ground-truth above): all seven present, the
list is grounded. A receiver executing exactly (a)-(g) leaves NO active grep hit — the
gate is satisfiable-by-construction. The fold deepens the net-LOC-deleted claim
(consistent with its own net-positive framing) rather than loosening anything. The V4 §1
RED-by-construction defect on the mandatory lands-FIRST PRUNE gate is closed. ACCEPT.

---

## §2 — SYNTHESIS §0.1 close-condition gate table (the remaining rows) — ACCEPT

`SYNTHESIS.md:315-331`. Every non-P1 gate row carries a concrete, greppable verifier; the
V5 folds did not loosen any. P2/P3/P4/P5 carry exact `grep -n`/`md5`/`sort -u` verifiers;
P4 (`:318`) remains the model anti-paper-close row ("`accepts_current_allowlist` passes
ONLY because the leaks are actually gone (not excluded)"). G3 (`:322`) carries the
three-part neutrality co-gate — (i) the arm census over codegen+xtask, (ii) the
grammar-named-type census, **(iii) the STRUCTURAL P3-collapse close-gate** now widened by
the CH2 F16 V5 fold to "all `RuntimeTarget` rows sharing one `grammar_name` MUST be
byte-identical in EVERY field except the generated-artefact path columns … `count(distinct
config-tuple-minus-output_dir) == 1`" — correctly noting the gate is "RED pre-P3 and only
goes GREEN once the profiles genuinely collapse." This is the CH2 V5 tightening that closes
the F13 narrow-projection hole (the `(source_roots,entry_rule)`-only `sort -u` a relocated
branch sailed past). Re-verified live: the 7 css_l4 rows differ in `fact_schema`, so the
widened check is correctly RED today. PROVE (`:326`), H1, the JSON guard, invariants
(`:329`), and PASS-IMPL V4 (`:331`) each carry the named telemetry + greppable verifier.
ACCEPT.

---

## §3 — SYNTHESIS §0.3 receiver goalset — ACCEPT (V1 REVISE D-CH6-1 fold survives)

`SYNTHESIS.md:355-373`. The two under-bound disjunctive receivers V1 flagged remain
self-contained at HEAD:

- **G6 receiver (`:371`)** still reads: wire `find_css_significant`/`find_comment_close`
  (`runtime_simd.rs:169,112`) into the CSS hot path AT ADMISSION with a same-wave consumer,
  OR honestly retire/mark them "**with a samply attribution row proving the kernel's target
  leaf is non-top-N on the benched CSS hot path** (the retire branch is gated on a
  MEASUREMENT, not an assertion — it cannot close G6 by marking all NEON 'retired' with
  zero acceleration wired)." Re-grepped verbatim. The escape hatch stays closed.
- **PROVE receiver (`:372`)** still carries the §0.5 fallback inline: "if Sheets cannot be
  emitted via the generator ONLY, the generalization is NOT real — surface honestly, do
  NOT stub-prove; do NOT hand-write a `_GENERATED_RS` Sheets block." Re-grepped verbatim.

A receiver reading only §0.3 to assign owner paths cannot reach a paper-close read-path on
G6 or PROVE. The V5 P1 widening (the §0.3 PRUNE owner-path narrative carries the crate-wide
reach consistent with §1) did not weaken the G6/PROVE receivers. The V4-flagged §0.3-vs-§1
mismatch (the §0.3 PRUNE row inheriting the four-item list) is resolved by the §1 fold,
which propagated the reach-complete list crate-wide. ACCEPT.

---

## §4 — SYNTHESIS §0.5 generalization litmus + per-axis fallbacks — ACCEPT

`SYNTHESIS.md:447-469`. The strongest anti-paper-close section, unchanged in substance.
Every axis row carries Current / Target / Expected-intervention / **Fallback-if-not-met**,
every fallback names a concrete non-paper-close action ("surface honestly as a named
validated grammar-parameterized primitive (HANDOFF §6), do NOT paper-close; do NOT silently
retain the hand-written blob" `:457`; "the generalization is NOT real — surface honestly,
do NOT stub-prove" `:459`; "REJECT the trait shape, report, do NOT force a Lock-1
violation" `:460`). The aarch64-only axis row (`:461`) was correctly EXTENDED by the V5
§1 fold to name the full crate-wide x86 surface — `src/x86_64/` AND `ext/x86/` AND nasm
`build.rs` AND `nasm-rs` Cargo.toml dep **AND `lib.rs` cfg-dispatch arms** — with "deletion
list reach-matched to the verify grep — every active grep hit removed, CH6 V4 §1." The §1
enumeration gap that V4 flagged on this row is closed. The litmus is explicitly
binary-structural (`:450-453`), gated on PRESERVING the >SOTA; the R10 criterion
(`:463-469`) restates the conjunction correctly. ACCEPT.

---

## §5 — SYNTHESIS Section 2 telemetry binding + gate consumer — ACCEPT

`SYNTHESIS.md:530-604`. The load-bearing answer to CH6's "is the goalset measurable +
verifiable from the bench gate?" — YES. Every generalization axis binds to a named
telemetry column AND a `gate-json` reject condition. The V4→V5 folds strengthened TWO
columns CH6 cares about:

- `runtime_target_rows_collapsed` (`:553`) carries the F16 V5 widening: "all xtask
  `RuntimeTarget` rows sharing one `grammar_name` are byte-identical in EVERY field except
  the generated-artefact path columns … over `fact_schema`/`row_id`/`output_plane`/`emitter`/
  `entry_rule`/`source_roots`/`check_command`/`frontend_requirements`; the 7 css_l4 rows
  collapse to one CSS config row — a `(source_roots,entry_rule)`-only `sort -u` is
  INSUFFICIENT … CH2 V4 §8.1." Re-verified live: this gate is correctly RED today (7
  distinct `fact_schema`). The narrow-projection hole the relocated seam rode is closed —
  a structural deepening, not a loosening.
- `x86_tree_deleted` (`:563`) carries the §1 reach-complete redefinition (NO x86 surface
  anywhere — `src/x86_64/` AND `ext/x86/` AND `build.rs` AND `nasm-rs` Cargo.toml dep AND
  `lib.rs:5`/`:285-288` AND doc surfaces — verified crate-wide with the source+manifest
  scope). The V4 §1 enumeration gap is closed at the telemetry column too.

The other columns survive verbatim: `generator_grammar_branch_count` (`:552`, FULL-alphabet
arm census, md5-distinctness necessary-not-sufficient), `generator_grammar_type_count`
(grammar-named-type census), `json_rich_navigation_preserved` (closes the ≥2-impl
LCD-flatten false-green), `shared_value_trait_instantiations >= 2` with the production-only
`grep -v 'tests.rs|#[cfg(test)]'` exclusion (the V3 CH5 E.1 fold), `acceleration_at_admission`
with `cfg-test-only` = NO-GO (closes the dead-at-admission paper-close), `sheets_grammar_shape`
(makes "genuinely different shape" machine-checkable). The gate consumer (`:576-604`) names
the EXACT reject conditions. The `gate-json`/`--skv18-generalization-report` host command is
a real S-P3 obligation (xtask surface re-verified present). ACCEPT.

---

## §6 — SYNTHESIS Section 3 trajectory + revert dependency graph — ACCEPT (V1 REVISE D-CH6-2 fold survives)

`SYNTHESIS.md:605-657`. The revert *dependency graph* survives verbatim in the goalset
S-P3 consumes. `:646-652` reads: "S-P3's revert protocol MUST encode the **entry-gate
dependency graph** (PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1 …): a wave that fails
its exit gate BLOCKS every downstream wave that entry-gates on it — no downstream wave
dispatches over a REDRESSed predecessor; in particular G1 failure blocks G2/G3/G4/PROVE,
and G3 (un-fork) failure blocks PROVE (which emits Sheets THROUGH the un-forked
generator)." The closing sentence (`:655-657`) makes intent explicit: "the difference
between a legitimate handoff and a paper-close." The deferral itself remains PASS-ALPHA
§4.4-sanctioned (CH6 cannot REJECT the deferral as out-of-bounds; §4.4 places
owner-paths/gates/caps/revert in `SPEC.md` authored by S-P3). The fold supplies the binding
carry that makes the deferral legitimate. The hard-cap defaults (`:652-655`,
research/plan/redress 20/15/30, "at 0.9N commit, at N halt", MED-HIGH carve-out) survive.
The V5 folds did not touch this section. ACCEPT.

---

## §7 — HANDOFF "Next Move" + revert/cap deferral carry — ACCEPT (V1 REVISE D-CH6-3 fold survives)

`HANDOFF.md:293-365`. Both binding carries survive verbatim. `:355-365` reads: "Revert
protocol, hard caps, and per-wave triumvirate discipline are sanctioned-deferred to S-P3
(PASS-ALPHA §4.4 authority), not paper-closed here — with two binding carries … 1. **Revert
dependency graph:** … G1 failure blocks G2/G3/G4/PROVE; G3 un-fork failure blocks PROVE.
2. **Hard-cap defaults:** … 20/15/30 min, 'at 0.9N commit, at N halt' unless the wave's
risk class (the Sheets/NEON cluster is MED-HIGH per alphaE) justifies a documented larger
cap — so no SK-V18 wave dispatches uncapped." The same-wave-consumer rule (`:317-318`
"Each primitive lands WITH its hot-path consumer in the same commit (no orphan kernels)")
and the per-wave >SOTA re-proof (`:318-320`) remain stated. The V5 §1 fold propagated into
the P1 receiver (`:306-308` now names "src/x86_64/ AND ext/x86/ AND nasm build.rs AND the
nasm-rs Cargo.toml dep AND lib.rs pub mod x86_64;/cfg-arms") and the `x86_tree_deleted`
gate consumer (`:336-338`, identical reach-complete list). The §status header (`:11`) cites
"x86 second-surface crate-wide; revert dependency graph + hard-cap." No loosening; the V4
§1 four-item-vs-grep gap that V4 flagged here is closed. ACCEPT.

---

## §8 — alphaE candidate shortlist gates + sequencing — ACCEPT (V4 §1/§13 αE-orphan fold landed)

`alphaE:94-241`. The V4 §1 REVISE was ALSO the αE/αA orphan-propagation REVISE (the
V3→V4 FOLD-1 second-x86-surface widening landed in αC/SYNTHESIS/HANDOFF but the
`src/`-scoped close-gate survived in the αA/αE research feeders). V5 closed it:
- The αE P1 row (`:94`) now names "the WHOLE x86 surface crate-wide … **BOTH surfaces**
  [FOLD F8 + V5 R-1/CH6 V4 §1]": (1) `src/x86_64/` + `remove pub mod x86_64; lib.rs:5` +
  `remove the #[cfg(target_arch="x86_64")] dispatch arms lib.rs:285-288` +
  `remove ("crates/bbnf-simd/src/x86_64","diagnostic-x86") lock14_baseline.rs:2463`;
  (2) `ext/x86/` + `build.rs` + the `nasm-rs="0.3"`/`build="build.rs"` Cargo.toml deps
  (+`:14-16` comments) + re-home `lib.rs:247` + scrub doc surfaces OR scope grep to
  source+manifest. LOC Δ ≈ −4500.
- The αE P1 exit gate (`:104`) is now CRATE-WIDE: `grep -riE --include='*.rs'
  --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' bbnf-simd/` → only aarch64-neutral
  comments (every active hit on the removal list); `find …/src/x86_64 …/ext/x86 -type f`
  → 0; `build.rs` + `Cargo.toml` carry no `nasm`/`x86` active token. The V4-flagged
  `src/`-scoped false-green is gone.
- F15 (`:18`) + F16 (`:241`) ledger rows document both V4 REVISEs folded "no candidate
  added or removed (still exactly 5: A, B1–B4); the shortlist remains additive-by-deletion;
  no re-opened REDRESS pre-block."
- The CH6-critical instruments survive: the falsifiability triple's `.bbnf`-mutation
  derivation test ("a const courier cannot pass this"); the exit-gate-blocks-successor
  clause carrying the CH6 revert dependency; the honest-finding (a)-(c) gate (`:223`); the
  checkasm count corrected to disk-true 12+2=14 (`:101`, F4/F14); the risk-weighted close
  prediction that handles litmus-failure without paper-closing.
ACCEPT.

---

## §9 — alphaC PRUNE-wave close gates — ACCEPT

`alphaC`. Each PRUNE wave (P1-P5) carries live evidence + delete-or-fix obligation + a
close gate that makes the prune meaningful. P4's close gate remains the model
anti-paper-close ("`accepts_current_allowlist` PASSES *after* the rebuild because the
scanned surface is genuinely neutral, not because the dirty files are excluded"), combined
with alphaE P4-exit's injected-token RED test (re-run after re-introducing a `JsonSink`
token must turn RED — green-for-the-right-reason). The P3 collapse-vs-differentiate fold
makes COLLAPSE-to-one the DEFAULT and gates "N distinct generated.rs" behind "N distinct
`.bbnf` roots genuinely authored." The Sheets broadcast pre-block binds the Sheets corpus
to per-corpus N≥50-cold-median (PERMANENT pre-block). The αC P1 narrative now inherits the
crate-wide x86 reach (the binding gate is SYNTHESIS §0.1, propagated). The αC PRUNE-wave
gate STRUCTURE is sound; the V4 §1 P1 target-list completeness, owned by §0.1, is folded.
ACCEPT.

---

## §10 — The honest-finding escape (the anti-paper-close hatch itself) — ACCEPT (V1 REVISE D-CH6-4 fold survives)

`SYNTHESIS.md:331`; `HANDOFF.md:351-353`; `alphaE:223`. The single largest residual
paper-close surface remains gated. The PASS-IMPL V4 row (`SYNTHESIS.md:331`) carries the
(a)-(c) qualification gate verbatim: "a 'named validated grammar-parameterized primitive'
qualifies ONLY if: (a) the grammar `.bbnf` INVOKES it by name … NOT a free-standing const
the emitter splices; (b) it is parameterized by grammar-derived DATA … NOT a fixed body;
(c) it carries the same `verbatim_blob_present == false` telemetry … A primitive failing
(a)-(c) is a relabeled hand-written blob — REJECT, REDRESS, do NOT close. Without this gate
the escape is the single largest paper-close surface in the contract." A relabeled
`CSS_GENERATED_RS` fails (a) (free-standing const the emitter splices — re-confirmed at
`runtime_generator.rs:701`), (b) (fixed body), and (c) (it IS the verbatim blob). Reinforced
at `alphaE:223` and carried in the §0.5 fallbacks. The V5 folds did not touch this gate.
The escape is a genuine honest-finding path, not a paper-close hatch. ACCEPT.

---

## §11 — PROVE-Sheets gate (the generalization litmus) — ACCEPT

`SYNTHESIS.md:326,372`; `alphaE:183`; `alphaC` Sheets section. Correctly
non-paper-closeable, strengthened by `sheets_grammar_shape == pratt-operator` (the shape
disclosure making "genuinely different shape" machine-checkable — a flat-stream/tree Sheets
REJECTed as third-JSON hollowing). The gate binds md5(Sheets) ≠ JSON ≠ CSS; Sheets value
type instantiates the G4 trait; ZERO hand-authored runtime Rust (`grep -c 'const.*_RS.*r#'
codegen/src` for any Sheets blob → 0); via the generator ONLY; the canonical neutrality
grep AND type census stay 0. The adoption of the EXISTING real Pratt `google-sheets.bbnf`
over a fresh stub is the correct anti-hollowing move. The §0.5 fallback is inline in the
§0.3 receiver (§3). The V5 folds did not touch this gate. ACCEPT.

---

## §12 — G6 acceleration-wiring / orphan-kernel gate — ACCEPT

`SYNTHESIS.md:371`; telemetry `acceleration_at_admission`; `alphaE:183`. The
acceleration-wiring gate corrects the SK-V17 W3 overstatement (NEON "acceleration" dead at
admission — `find_css_significant`/`find_comment_close` are `#[cfg(test)]`-only) and is
machine-checkable: "any kernel claiming acceleration is reached at admission (grep the
generated hot path, not tests)." The `acceleration_at_admission ∈
{admission,scalar-passthrough-labeled,retired}` enum with `cfg-test-only` = NO-GO closes the
dead-at-admission paper-close. The retire branch's measured non-top-N samply floor is folded
into the §0.3 receiver; the same-wave-consumer rule pre-empts the orphan-kernel pattern. The
V5 folds did not touch this gate. ACCEPT.

---

## §13 — V5 fold integrity (the two V4 REVISEs landed; no new paper-close surface) — ACCEPT

The threshold V5 question: did the two V4→V5 folds land as binding gate text + feeder
corrections, and did either introduce a new paper-close surface? Re-grepped each fold site:

- **CH6 V4 §1 fold (BLOCKING P1 deletion-target/grep-reach mismatch) — LANDED, no new hole.**
  The V4→V5 fold-ledger (`SYNTHESIS.md:102-132`) records the fold; the BINDING text landed
  at the P1 close row (`:315`, seven (a)-(g) targets), the `x86_tree_deleted` telemetry
  (`:563`, identical reach), the §0.5 aarch64 axis row (`:461`), HANDOFF P1 receiver
  (`:306-308`) + gate consumer (`:336-338`), §0.3 PRUNE narrative, §0.4. The αA/αE
  orphan-propagation (the same fold being the CH1 §αE / CH3 / CH7 §1 REVISE) landed in the
  feeders: αA (`:13-63` V5-FOLD log + §0/§3.2/§5/§6 close-gate now BOTH surfaces crate-wide,
  the LOC corrected −847 → ≈ −4500) and αE (`:94` P1 row, `:104` exit gate, `:108` LOC
  budget, F15 `:18`/F16 `:241` ledger). Re-verified all seven (a)-(g) targets LIVE on disk
  (Ground-truth). The deletion list is now reach-matched to the grep — the gate is
  satisfiable-by-construction. **NO new hole:** the extension only DEEPENS the deletion (more
  LOC removed), the verify grep is honestly scoped (`--include='*.rs' --include='Cargo.toml'`
  OR scrub doc surfaces), and the "none active" claim is now true-by-construction. The V4 §1
  RED-by-construction defect is closed without opening a successor.

- **CH6 V4 §13 fold (V3→V4 fold-ledger self-citation drift) — LANDED, no new hole.** The
  V4→V5 fold-ledger (`SYNTHESIS.md:158-165`) records the fix; re-grepped the V3→V4 narrative
  at `:76-97`: the line-number self-citations (`:201`/`:423`/`:377-378` in V4) are now
  fold-stable section/column anchors — `:77-78` "the G3 close-condition row, the V2→V3
  fold-ledger, the `generator_grammar_branch_count` telemetry column," `:93-94` "the
  Section 1 checkasm ledger." No drifting line numbers remain in the V3→V4 fold narrative.
  The authority document no longer degrades its own self-citation accuracy on subsequent
  folds. **NO new hole** — a pure verifiability tightening.

A second-order adversarial check on the CH2 F16 V5 fold (which co-gates G3 and is therefore
CH6-relevant for SATISFIABILITY of the deferred-to-S-P3 receiver): the widened
`runtime_target_rows_collapsed` check is correctly RED today (7 distinct `fact_schema`,
re-verified live at `regen_css.rs`). The OBLIGATION to turn it GREEN — collapse the 7 xtask
`RuntimeTarget` css_l4 rows to one config row — is carried in the G3 (iii) close row
(`:322`, "the P3 collapse close-gate … the 7 css_l4 rows collapse to one CSS config … only
goes GREEN once the profiles genuinely collapse"), bound to the telemetry "yes for P3 + G3"
(`:553`), and assigned to the P3 receiver in HANDOFF S-P3 sequencing (`:310`, "P3 (collapse
7 replicas — AND collapse the 7 xtask `RuntimeTarget` css_l4 rows to one config row, so
`runtime_target_rows_collapsed` holds)"). I note a minor receiver-LOCALITY seam: the
SYNTHESIS §0.1 *P3 row* (`:317`) names only the generated.rs collapse, while the
config-table collapse obligation lives under the *G3 row* + §0.4 + HANDOFF S-P3 + the
telemetry binding. This is NOT a paper-close hole — the obligation is binding, named,
measurement-gated (RED today), receiver-assigned, and explicitly cross-referenced from G3
("the P3 collapse close-gate"). It is a cross-reference nit far below REVISE severity (the
gate is satisfiable, the path to GREEN is specified). ACCEPT-with-note; flagged for S-P3 to
co-locate when it authors §4.4, not a CH6 REVISE.

Both V4 REVISEs are clean folds into binding text + feeders; neither introduced a successor
hole. ACCEPT.

---

## Consolidated CH6 V5 verdict

The αF V5 contract folded BOTH surviving V4 CH6 REVISEs as binding gate text plus feeder
corrections, orphan-free, all re-verified LIVE at HEAD `318d9c046`:

1. **The BLOCKING V4 §1 P1 deletion-target/grep-reach mismatch** is closed. The P1 close
   row (`:315`) and the `x86_tree_deleted` telemetry (`:563`) now enumerate the full
   reach-matched (a)-(g) removal list — `src/x86_64/`, `ext/x86/`, `build.rs`, the
   `lib.rs:247` doc-comment, the `nasm-rs` Cargo.toml dep + `:14-16` comments, `lib.rs:5
   pub mod x86_64;` + the `lib.rs:285-288` cfg-dispatch arms, and the in-crate doc surfaces
   (scrub or scope the grep). Every (a)-(g) target re-greppped live on disk. The verify grep
   is honestly scoped. The gate is satisfiable-by-construction; the RED-by-construction
   defect on the mandatory lands-FIRST PRUNE gate is gone. The αA/αE feeders that retained
   the `src/`-scoped false-green were corrected (αA §0/§3.2/§5/§6, αE P1 row/exit/F15).

2. **The V4 §13 fold-ledger self-citation drift** is fixed. The V3→V4 narrative line-number
   citations are now fold-stable section/column anchors that do not drift on subsequent
   folds; the authority document's self-verifiability is restored.

I also confirmed the V5 edits introduced NO new paper-close surface — the recurring
tighten-one-axis-open-another failure mode that V4 §1 itself was. The §1 extension only
DEEPENS the deletion and honestly scopes the grep; the §13 fix only tightens citations; the
CH2 F16 co-gate widening is correctly RED today with a binding, named, receiver-assigned
path to GREEN.

The contract's measurable CORE remains strongly anti-paper-close at V5: §0.1 gates greppable
and reach-matched (P1 now satisfiable-by-construction); §0.5 fallbacks named with concrete
non-paper-close actions; Section 2 telemetry binds every generalization axis to a `gate-json`
reject condition (host command + xtask surface exist; `runtime_target_rows_collapsed` is the
STRUCTURAL relocated-seam check now projected over all non-path columns; `x86_tree_deleted`
is reach-complete); the alphaE `.bbnf`-mutation derivation test is operational; the alphaC
PRUNE close gates are RED-on-injection meaningful; the honest-finding escape carries its
(a)-(c) gate; the sanctioned-deferred revert/cap/triumvirate carries its binding dependency
graph + halt ceiling. No wave is deferred without a receiver + a gate.

This lens CONVERGES for V5: ACCEPT 13 / REVISE 0 / REJECT 0, accept rate 100% (≥ the §3Z
95% threshold). With CH6 V4 = 84.6% sub-threshold and CH6 V5 = 100%, CH6 has NOT recorded
two consecutive ≥95% cycles by itself — the §3Z two-consecutive requirement is a
WAVE-AGGREGATE condition (the V4 wave was 90.8%), so a second clean confirming wave is still
required to record the consecutive pair. CH6 contributes a clean 100% to this V5 wave with
zero orphan REVISE and zero REJECT; both V4 REVISEs folded into binding text without opening
a successor hole.

TALLY accept=13 revise=0 reject=0
