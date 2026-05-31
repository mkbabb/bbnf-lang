# CH4 — COST lens (V6-confirm) — Pass Alpha SK-V18 alpha-hardening

**Lens:** CH4 Cost (PASS-ALPHA §3 / ORCHESTRATOR §3W). CONFIRMING cycle.
**Subject:** SK-V18 = the GENERALIZATION cycle (the inflection backtrack). ONE grammar-driven
generator emitting JSON+CSS+Sheets from `.bbnf` over the unified tape/`ValueRef` substrate,
shared value-API, PROVEN on a 3rd grammar (GoogleSheets, real Pratt), PRESERVING `>SOTA`. NOT a
feature cycle.
**Artefacts reviewed:** `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md` (αF =
SYNTHESIS+HANDOFF; `ls alphaF*` = no match, confirmed).
**Cycle posture:** V6 CONFIRMING — the goalset is substantively converged (V1 75.0 → V2 96.9 → V3
≈94.8 → V4 92.7 → V5 97.9%, zero REJECT every cycle). The two orphan REVISEs carried out of V5 are
the only open items: **CH2 V5 §8 (F16 field-enumeration omits `profile`)** and **CH5 V5 F.6 (P1
x86-deletion list narrower than its own crate-wide grep — `tests/checkasm_parity.rs` +
`src/scalar/byte_class_from_eq_set_64.rs`)**. CH4's job this pass: ACCEPT where correct+complete,
verify the two folds discharged, flag ONLY genuine residual defects. CH4 itself converged at V4
(6A/0R) and stayed clean at V5 (no CH4-originated REVISE); the cost cohort carries no orphan.
**Method:** every cost/LOC/field/owner-path/grep-reach claim re-verified LIVE on disk at the benched
skinny tree (`skinny/crates/` + `skinny/xtask/`) this pass. Citations `path:line` from disk.

---

## §0 — Disk-verified ground truth (the basis for both fold verdicts)

| claim (artefact) | disk verification (V6 this pass) | verdict |
|---|---|---|
| `RuntimeTarget` field count | `xtask/src/regen.rs:6` struct = **12 fields** (`grammar_name, profile, entry_rule, source_roots, output_dir, check_command, source_inputs, metadata_inputs, emitter, expected_files, frontend_requirements, output_labels`) | **12, NOT 13** (see §1 residual) |
| F16 path-excluded columns | `output_dir`/`expected_files` are the two generated-artefact path columns | EXACT |
| F16 operative non-path set (within a `grammar_name` group) | 12 − `output_dir` − `expected_files` − `grammar_name`(group key) = **9**: `profile`/`entry_rule`/`source_roots`/`check_command`/`source_inputs`/`metadata_inputs`/`emitter`/`frontend_requirements`/`output_labels` | EXACT — every field accounted for |
| F16 7 css_l4 per-profile divergence | `regen_css.rs` 7 rows SHARE `(source_roots=CSS_L4_ROOTS, entry_rule="stylesheet")` but carry 7 DISTINCT `profile`/`fact_schema`/`source_inputs`/`metadata_inputs` | EXACT — old projection false-green; widened gate RED today |
| F.6 `tests/checkasm_parity.rs` x86 coupling | `grep 'bbnf_simd::x86_64::' tests/checkasm_parity.rs` = **9 active call sites** (`:458,:464,:467,:477,:478,:484,:493,:497,:502`) + `:673` `#[ignore]` x86 harness; compile-coupled to `pub mod x86_64` | EXACT (REVISE said "11 tokens, 9 active" — 9 active confirmed) |
| F.6 `src/scalar/byte_class_from_eq_set_64.rs` x86 refs | doc lines `:10,:12,:15` "AVX-512 BW"/"AVX2" cross-refs | EXACT |
| **binding P1 grep reach** | `grep -riE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` fires **16×** on `tests/checkasm_parity.rs` + on `src/scalar/byte_class_from_eq_set_64.rs` | **FIRES — see §2 defect** |
| P1 LOC Δ ≈ −4500 (F15) | 742 `.rs` + 105 `.asm` + 3554 `ext/x86/` + 102 `build.rs` = 4503 | EXACT (unchanged by either fold) |

Both folds' *underlying disk facts* are correct. The cost figures are untouched: F16 is zero-LOC; the
two F.6 sites are decouple-or-doc-scrub (negligible LOC), so the P1 ≈ −4500 / net ≈ −12850 accounting
is unaffected. **The cost axis is clean — neither fold introduces a cost regression.** The two defects
below are *completeness/correctness* of the folds, not cost.

---

## §1 — FOLD 1 (CH2 V5 §8 / F16 field-enumeration) — **ACCEPT (substance) with one residual REVISE**

**Discharge — verified landed.** The F16 operative enumeration now explicitly names the three
omitted fields and is stated BY EXCLUSION, equal to its own prose, at every binding site:
- SYNTHESIS:154-167 — *"every field EXCEPT the two excluded path columns (`output_dir`,
  `expected_files`) — i.e. the operative set is `grammar_name`/**`profile`**/`entry_rule`/`source_roots`/
  `check_command`/**`source_inputs`**/**`metadata_inputs`**/`emitter`/`frontend_requirements`/
  `output_labels`"*, with the explicit note that the prior enumeration "OMITTED `profile` itself … plus
  `source_inputs`/`metadata_inputs`" and the by-exclusion machine-check
  `count(distinct config-tuple-minus-(output_dir,expected_files)) == 1`.
- SYNTHESIS telemetry `runtime_target_rows_collapsed` :566 + G3 row :333 + escape-pre-block :410-413 +
  Section-2 :608 — all carry the widened, by-exclusion, `profile`-naming form.
- alphaE:19 (F16 ledger), :105 (P3 exit), :156, :207 — the fold is reach-complete in the shortlist.
- The P3-collapse caveat is present and correct: *"P3 must PRESERVE profile-distinctness where the 7
  profiles are genuinely distinct grammars … do NOT erase the `profile` discriminator"* (SYNTHESIS:164-167,
  alphaE:105,156). This is the exact necessity the V5 REVISE called for.

**Cost-axis verdict:** the widened projection is a `sort -u`/`awk`/`jq` over a data-table already on
disk — **ZERO LOC to evaluate**, strictly strengthens the gate. The CH2 ask ("enumerate by exclusion so
the operative list equals the prose; name `profile`; keep the mechanism") is **discharged**, and the
discharge is cost-free. The structural-derivation suggestion (hash-all-minus-path-fields so no future
field falls outside) is honored in spirit by the by-exclusion framing.

**Residual REVISE (the fold introduced a new factual error): "13-field struct" — disk is 12.** Every
folded restatement labels the struct *"the live `regen.rs:6` **13-field** struct"* — SYNTHESIS:333, :410,
:566, :608 and alphaE:19, :105, :156, :207. The live struct (`regen.rs:6`) has **12 fields** (§0). The
arithmetic is self-consistent only at 12: 9 operative + `grammar_name`(group key) + `output_dir` +
`expected_files` = 12. The enumeration *content* is correct and complete (every field is accounted for;
nothing is silently dropped), so this does NOT reopen the F16 substance — but it is a binding-contract
factual inaccuracy *introduced by the fold itself*, on the very `path:line` the fold cites as its
authority. An implementer cross-checking "13 fields" against `regen.rs:6` finds 12 and is left to wonder
which field the contract believes it missed. **Fix (mechanical, single token, 8 sites): "13-field" →
"12-field" at SYNTHESIS:333,410,566,608 + alphaE:19,105,156,207.** No cost effect; no mechanism change.

**Disposition: REVISE** — F16 substance ACCEPTed and discharged; the lone residual is the off-by-one
"13-field" label the fold introduced, a one-token correction on a self-cited line.

---

## §2 — FOLD 2 (CH5 V5 F.6 / P1 x86-deletion-list widening) — **REVISE (not discharged at the binding inventory-of-record)**

**Partial discharge — landed in the digest + shortlist, ABSENT from the binding contract.** The CH5 V5
REVISE was explicit about *where* the fix must land:

> "the **binding SYNTHESIS P1 deletion list / `x86_tree_deleted` telemetry / HANDOFF P1 receiver** are
> NARROWER than their own crate-wide `--include='*.rs'` verify grep … add removal targets (h)
> `tests/checkasm_parity.rs` + (i) `src/scalar/byte_class_from_eq_set_64.rs` … and **make the binding
> verify grep `tests/`-inclusive**."

Where the fold DID land (correct, reach-complete):
- alphaC:168-215 — the digest carries the verbatim fix: `tests/checkasm_parity.rs` "COMPILE-COUPLED, NOT
  doc/test-only", 9 active `bbnf_simd::x86_64::…` call sites, "the `checkasm_parity.rs` decoupling is what
  keeps the `src/x86_64/` deletion from breaking compilation"; `src/scalar/byte_class_from_eq_set_64.rs`
  `:10,:12,:15` re-word.
- alphaE:94 P1 row item **(3)** — "COMPILE-COUPLED removal/decoupling sites the verify grep ALSO fires
  on (V5 R-2/CH5 F.6 widening) … `tests/checkasm_parity.rs` … DECOUPLE-OR-DELETE … BREAKS THE BUILD …;
  `src/scalar/byte_class_from_eq_set_64.rs` … CLEAN to aarch64/scalar-neutral"; alphaE:101 (decoupled-not-
  deleted), :104 (P1 exit asserts `cargo build` AND `cargo test --no-run` clean, both sites on the removal
  list). The shortlist is fully discharged.

Where the fold DID NOT land — the binding inventory-of-record:
- **SYNTHESIS P1 row (:326)** enumerates only **(a)–(g)**. `checkasm_parity.rs` and
  `src/scalar/byte_class_from_eq_set_64.rs` are ABSENT. Verified: `grep -ni 'checkasm_parity\|byte_class_
  from_eq_set_64\|decoupl\|build-sound\|test --no-run\|compile-coupl' SYNTHESIS.md` = the two file names
  appear ONLY in unrelated checkasm-count prose (:44,:520); NONE of "decouple / build-sound / test --no-run /
  compile-coupled" appears anywhere in SYNTHESIS.
- **`x86_tree_deleted` telemetry (:576)** — enumerates `src/x86_64/` + `ext/x86/` + `build.rs` + `nasm-rs`
  dep + `lib.rs` mod/cfg-arms + doc-scrub; does NOT name either compile-coupled site, and its verify grep
  is `--include='*.rs' --include='Cargo.toml'` (i.e. `tests/`-INCLUSIVE, NOT excluded).
- **HANDOFF P1 receiver (:101-110, :217-221, :253-257)** — same (a)-(g)-equivalent list; no
  `checkasm_parity.rs`, no scalar-doc site, no `cargo test --no-run`.

**Why this is a GENUINE residual defect, not cosmetics (and squarely CH4's cost-of-a-false-gate concern).**
Disk-confirmed this pass: the binding `x86_tree_deleted` grep
`grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm' bbnf-simd/` fires **16×** on
`tests/checkasm_parity.rs` (active `bbnf_simd::x86_64::avx2::…` call sites — *code*, not "aarch64-neutral
comments") and again on `src/scalar/byte_class_from_eq_set_64.rs`. So an implementer who follows the
**binding SYNTHESIS/HANDOFF P1 contract** deletes exactly (a)-(g), then runs the binding `x86_tree_deleted`
grep — which the contract asserts "returns only aarch64-neutral comments (none active)" — and it instead
returns 16 ACTIVE hits. **The mandatory lands-FIRST PRUNE gate goes RED on a tree that completed every
binding deletion target.** This is the precise RED-by-construction / un-satisfiable-gate anti-pattern this
whole cycle exists to extinguish (CH6 V4 §1; the P4-class false gate; the V5 R-1 `Cargo.toml`/`lib.rs`
reach-mismatch the goalset already fixed once at the binding level). It is re-incurred one reach level
deeper, in `tests/`, and — worse — `checkasm_parity.rs`'s 9 active `bbnf_simd::x86_64::…` call sites are a
**hidden compile-coupling**: deleting `src/x86_64/` per (a)/(f) without decoupling them makes
`cargo test --no-run` FAIL TO COMPILE. The binding contract's exit criteria (SYNTHESIS:326, :576; HANDOFF)
do not mention build-soundness or `cargo test --no-run` at all.

The goalset itself designates the binding surface unambiguously — SYNTHESIS:130: *"HERE (the P1 row + the
`x86_tree_deleted` telemetry) is the binding inventory-of-record; the αA/αE…"*. The fold reaching αC + αE
but NOT this surface is therefore **not** discharged by the goalset's own definition of "binding". The fix
is the same mechanical fold already authored in αC/αE — it simply was not propagated the last reach into
SYNTHESIS:326 (add removal/decoupling items (h) `tests/checkasm_parity.rs` x86 block — decouple-or-delete;
(i) `src/scalar/byte_class_from_eq_set_64.rs` :10-15 — scrub), SYNTHESIS:576 `x86_tree_deleted` (name both
sites + add `cargo test --no-run` clean to the exit), and HANDOFF:101-110/:253-257 (same two receivers).

**Cost-axis note:** the cost accounting is UNAFFECTED — both sites are decouple/doc-scrub (≈0 net LOC),
P1 ≈ −4500 and net ≈ −12850 hold. The defect is gate-satisfiability + build-soundness, not LOC budget. But
an un-satisfiable mandatory entry gate is the single most expensive failure mode for a PRUNE wave (it
blocks every downstream B-candidate behind a gate that cannot go GREEN), so CH4 flags it REVISE-blocking.

**Disposition: REVISE** — the F.6 mechanism is correct and already authored in the digest+shortlist; it is
simply NOT propagated into the binding SYNTHESIS P1 row / `x86_tree_deleted` telemetry / HANDOFF P1
receiver, the exact three surfaces the V5 REVISE named. Until it lands there, the binding P1 gate is
RED-by-construction (16 active grep hits on a fully-(a)-(g)-deleted tree) and the `checkasm_parity.rs`
compile-coupling is unguarded against `cargo test --no-run` breakage.

---

## §3 — Cross-cutting cost re-confirmation (ACCEPT — unchanged from V5)

The converged cost signature is re-verified intact this pass (no re-litigation; spot-confirmed on disk):
- **Net LOC ≈ −12850** (αE:227): P1 ≈ −4500 (742+105+3554+102 = 4503, EXACT) + P3 ≈ −5460 (6×910 redundant
  `generated.rs`; `css_l4_*/generated.rs`=7, 6370 LOC, md5 at_rules_and_media ≡ visual_functions) + P2 ~−700
  + B1 −800 + B2 −1500 + B3 ±0 + B4 +250(capped). A generalization backtrack that deletes far more than it
  adds — the correct cost signature. No `[generated-size-budget]` overflow on any candidate.
- **Checkasm inventory 12+2 = 14** (`ls checkasm_*.rs`=14, 12 single-kernel + `checkasm_common.rs` +
  `checkasm_parity.rs`) — the V1→V2 F4 correction holds; no stale "18" in the binding contract.
- **Same-wave consumer present on every candidate** (the V5 orphan-kernel guard) — A: gate is its own
  consumer; B1: `regen`→`json/generated.rs` ±5%; B2: `css_canon_bench`+oracle; B3: both trait impls same
  commit; B4: each kernel WITH hot-path caller, orphan ⇒ retire. Uniformly applied.
- **`>SOTA` preservation honest** — JSON 51/51 ≥ sonic-strict (apache_builds +1.4% the thinnest tripwire,
  B1-gated); CSS 1.996×–3.348× lightningcss (H1-framed); G6 acceleration gated at admission
  (`acceleration_at_admission ∈ {admission, scalar-passthrough-labeled, retired}`, NOT `cfg-test-only`).
- **PROVE Sheets is load-bearing** — real 185-LOC Pratt grammar adopted, `sheets_grammar_shape ==
  pratt-operator` REJECT-bound, Sheets config-tuple required distinct `grammar_name` from css_l4+json.

No cost re-entry of any re-blocked carrier (AZ-IV eager, StructRegistry per-leaf, fact-stream-output,
24-broadcast, FNV-runtime, x86/AVX/SVE — the latter now P1-deleted). ACCEPT.

---

## §4 — Disposition ledger

| § | item | disposition | residual defect (if any) |
|---|---|---|---|
| §1 | FOLD 1 — CH2 F16 field-enumeration (profile/source_inputs/metadata_inputs; enumerate-by-exclusion) | **discharged** in substance | **REVISE**: fold-introduced "13-field" label; disk = 12-field (SYNTHESIS:333,410,566,608 + alphaE:19,105,156,207) |
| §2 | FOLD 2 — CH5 F.6 P1 x86-deletion-list widening (checkasm_parity.rs + scalar/byte_class) | **NOT discharged** at binding inventory | **REVISE**: landed in αC+αE only; ABSENT from binding SYNTHESIS:326/:576 + HANDOFF:101-110 → RED-by-construction P1 gate (16 active grep hits) + unguarded compile-coupling |
| §3 | cross-cutting cost signature (net −12850, checkasm 14, same-wave consumers, `>SOTA`, PROVE) | **ACCEPT** | none |

**Cost-axis cohort: clean.** Neither fold introduces a cost regression; F16 is zero-LOC, F.6 is
decouple/scrub (≈0 LOC), net −12850 holds. The two defects are *completeness/correctness of the folds*,
not cost: (1) F16 discharged but carries a fold-introduced off-by-one struct-field count; (2) F.6's
correct mechanism reached the digest+shortlist but NOT the binding inventory-of-record the V5 REVISE named
— leaving the mandatory PRUNE P1 gate RED-by-construction and the `checkasm_parity.rs` compile-coupling
unguarded against `cargo test --no-run` breakage.

**ACCEPT 1 · REVISE 2 · REJECT 0** (the cross-cutting cost signature ACCEPTs; FOLD 1 substance discharged
but carries a residual REVISE; FOLD 2 not discharged at the binding surface — REVISE). Both REVISEs are
mechanical, single-edit, mechanism-correct (the F.6 fix is already authored verbatim in αC/αE — it needs
only the last reach into SYNTHESIS:326/:576 + HANDOFF; the F16 fix is a one-token 8-site "13"→"12"). The
converged goalset spine — Lock-14 generalization, substrate-union Lock 1, GoogleSheets 3rd-grammar proof,
net-deletion cost signature, `>SOTA` honest — is sound and not re-litigated.

TALLY accept=1 revise=2 reject=0
