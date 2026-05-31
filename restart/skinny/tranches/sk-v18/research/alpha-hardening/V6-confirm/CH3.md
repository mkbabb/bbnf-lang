# CH3 REGRESSION (V6-confirm) — SK-V18 Pass-Alpha hardening

**Lens:** CH3 Regression per `PASS-ALPHA §3` ("does any proposed intervention re-open a route in
REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly identified the pre-block
list?") + `ORCHESTRATOR §3W/§3Z`. CH3 axes: (1) no wave re-opens the REDRESS pre-block list
(AZ-IV / StructRegistry / fact-stream / 24-broadcast / FNV / x86-AVX-SVE); (2) PRUNE-before-
GENERALIZE; (3) prune does not strand `>`SOTA.

**Cycle:** V6 CONFIRMING — re-reviewing the αF contract (`SYNTHESIS.md` + `HANDOFF.md`; there is
no `alphaF-*.md` — the αF deliverable IS those two, reviewed) + the αA–αE feeders AFTER the two
orphan REVISE folds the V5 CONSOLIDATED §4 prescribed:
1. **CH2 §8.1 / F16** — the F16 `runtime_target_rows_collapsed` OPERATIVE enumeration must include
   `profile` (the 7-distinct per-profile discriminator) + `source_inputs` + `metadata_inputs`.
2. **CH5 F.6 / REVISE-2** — the **binding** P1 x86 deletion list + `x86_tree_deleted` telemetry
   must include `(h) tests/checkasm_parity.rs` (compile-coupled, 9 active `x86_64::` call sites) +
   `(i) src/scalar/byte_class_from_eq_set_64.rs` (x86 doc cross-refs) — the two crate-wide-grep-
   firing files NOT on the prior (a)-(g) list.

**Host:** aarch64 Apple M-series ONLY. **HEAD of record:** `318d9c046` (the `sk-v18/` alpha tree
is untracked working-state). Every disposition cites `path:line`/grep-result re-verified LIVE.

---

## §1 — Live ground-truth re-grep (HEAD `318d9c046`, V6 independent re-run)

| Pre-block / claim | Command | Result |
|---|---|---|
| HEAD | `git rev-parse HEAD` | `318d9c046…` |
| crate-wide verify grep firing-set | `grep -rilE --include='*.rs' --include='Cargo.toml' 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` | **fires on 5 non-`src/x86_64/` files** + the 24 `src/x86_64/` files: `build.rs`, `Cargo.toml`, **`tests/checkasm_parity.rs`**, `src/lib.rs`, **`src/scalar/byte_class_from_eq_set_64.rs`** |
| `tests/checkasm_parity.rs` x86 coupling | `grep -cE 'x86_64' …` | **11** tokens; **9 ACTIVE** `bbnf_simd::x86_64::…::*_scalar(…)` call sites at `:458,:464,:467,:477,:478,:484,:493,:497,:502` — **compile-coupled to `pub mod x86_64`** |
| `src/scalar/byte_class_from_eq_set_64.rs` x86 refs | `grep -nE 'AVX\|x86' …` | `:10` "NEON, AVX-512 BW", `:12` "AVX-512 BW", `:15` "AVX2 path" — comment-only, but `--include='*.rs'` fires |
| 7 CSS replicas md5 (P3) | `md5 …css_l4_*/generated.rs \| sort -u` | **1** over 7 dirs |
| metalang leak (P5) | `grep -c parse_w11_1_number …json/generated.rs` | 7 |
| `RuntimeEmitterKind` fork (G3) | `grep -n 'enum RuntimeEmitterKind' grammar_provider.rs` | `:40` |

The two REVISE-2 sites are **LIVE and grep-firing** at HEAD exactly as the V5 CONSOLIDATED §4
asserts: `tests/checkasm_parity.rs` (9 active call sites, compile-coupled) and
`src/scalar/byte_class_from_eq_set_64.rs:10,12,15`. The crate-wide verify grep fires on both.

---

## §2 — Fold-discharge verification (the two orphan REVISEs)

### FOLD 1 (CH2 §8.1 / F16: `profile`/`source_inputs`/`metadata_inputs` enumeration) — **DISCHARGED in the binding contract**

The F16 OPERATIVE enumeration now matches its PROSE (enumerate-by-EXCLUSION of the two path
columns `output_dir`/`expected_files`) and explicitly names `profile`, `source_inputs`,
`metadata_inputs` at every binding site:

- **SYNTHESIS G3 (iii) close-condition** (`:333`): operative set
  `profile`/`entry_rule`/`source_roots`/`check_command`/`source_inputs`/`metadata_inputs`/`emitter`/
  `frontend_requirements`/`output_labels` (plus the `fact_schema`/`row_id`/`output_plane`
  per-profile content the `profile` discriminator selects); the by-exclusion statement is declared
  authoritative ("a future field addition is captured automatically", `:159-160`).
- **SYNTHESIS V4→V5 fold narrative** (`:152-167`): `profile` named as "the 7-distinct per-profile
  discriminator that differentiates the 7 css_l4_* configs … plus `source_inputs`/`metadata_inputs`";
  "the by-exclusion statement is authoritative and the explicit list is its enumeration."
- **SYNTHESIS §0.4 pre-block** (`:411`): operative set names `profile`/`source_inputs`/`metadata_inputs`.
- **SYNTHESIS `runtime_target_rows_collapsed` telemetry** (`:566`): "the operative set is
  `profile`/`source_inputs`/`metadata_inputs`/`emitter`/`entry_rule`/`source_roots`/`check_command`/
  `frontend_requirements`/`output_labels`".
- **HANDOFF invariant 5** (`:22-24`, `:273-280`): widened to the full per-`grammar_name`
  config-tuple modulo the path columns; the divergence "rides `fact_schema`/`output_plane`/`emitter`".

The gate is correctly **RED today** (7 distinct `profile` + per-profile `source_inputs`/
`metadata_inputs`), GREEN only post-P3-collapse, and the §0.4 obligation explicitly forbids erasing
the `profile` discriminator (P3 must PRESERVE profile-distinctness where the profiles are distinct
grammars). The CH2 fold is mechanism-correct, propagated to all binding sites, orphan-free. **From
the CH3 lens this fold STRENGTHENS the relocated-overfit-seam guard (a P3/G3 re-entry vector) — it
catches a `match target.profile { … }` relocated branch the prior `(source_roots,entry_rule)`-only
projection sailed past. No re-open; no loosening.** DISCHARGED.

### FOLD 2 (CH5 F.6 / REVISE-2: P1 x86 deletion list + `x86_tree_deleted` telemetry widening) — **DISCHARGED IN THE FEEDERS, NOT IN THE BINDING CONTRACT** → residual defect

The V5 CONSOLIDATED §4 REVISE-2 fix is explicit: *"add to the **binding P1 list** + **`x86_tree_deleted`
telemetry** two removal targets — (h) `tests/checkasm_parity.rs:454-482` re-homed/deleted … (i)
`src/scalar/byte_class_from_eq_set_64.rs:10-15` doc x86 cross-refs scrubbed."* It further diagnoses
the defect as **propagation**: αC was already reach-complete; "αC's `tests/`+`scalar/` reach was NOT
carried into the BINDING SYNTHESIS P1 row (`:315`), the `x86_tree_deleted` telemetry (`:563`), or
the HANDOFF P1 receiver (`:101-112`)."

Re-verified at HEAD, the fold landed in the FEEDERS:

- **αC §4 C.1** (`:168-179`, `:188-196`, `:212-215`): EXPLICITLY decouples `tests/checkasm_parity.rs`
  (9 active `bbnf_simd::x86_64::…` call sites + the `#[ignore]` x86 harness), scrubs
  `src/scalar/byte_class_from_eq_set_64.rs:10,12,15`, and states the decoupling "keeps the
  `src/x86_64/` deletion from breaking compilation." Reach-complete.
- **αE P1 row** (`:94` "(3) COMPILE-COUPLED removal/decoupling sites the verify grep ALSO fires on
  (V5 R-2/CH5 F.6 widening)") + **checkasm status** (`:101` "`tests/checkasm_parity.rs` is DECOUPLED,
  not deleted") + **P1 exit gate** (`:104`, names the 9 call sites + the scalar doc strings). Folded.
- **αA** (x86 census + close gate) — carries BOTH x86 surfaces; the CH5-specific `tests/`/`scalar/`
  reach is owned by αC/αE.

But the BINDING contract was NOT folded. Re-read at HEAD:

- **SYNTHESIS P1 close-condition row** (`:326`): enumerates removal targets **(a)-(g) ONLY** —
  `src/x86_64/`, `ext/x86/`, `build.rs`, `lib.rs:247`, `nasm-rs` Cargo.toml dep, `lib.rs:5`+`:285-288`
  cfg arms, in-crate doc surfaces. **`tests/checkasm_parity.rs` is ABSENT; `src/scalar/byte_class_from_eq_set_64.rs`
  is ABSENT** (the lone `byte_class_from_eq_set_64` hit on `:326` is the `crate::x86_64::byte_class_from_eq_set_64`
  cfg-arm reference inside (f), i.e. the x86_64-module file — NOT the scalar file). The row closes
  "every active hit the grep flags is on the (a)-(g) removal list."
- **SYNTHESIS `x86_tree_deleted` telemetry** (`:576`): names `src/x86_64/`+`ext/x86/`+`build.rs`+
  `lib.rs:247`+`nasm-rs` dep+`lib.rs:5`/`:285-288`+doc surfaces — **NEITHER `checkasm_parity.rs`
  NOR the scalar file** (grep-confirmed: `NEITHER-NAMED-IN-TELEMETRY`).
- **HANDOFF P1 receiver** (`:101-112`): identical (a)-(g) enumeration, closing "every active hit on
  the removal list." `checkasm_parity.rs` + `scalar/byte_class_from_eq_set_64.rs` ABSENT.
- **HANDOFF invariant 3** (`:253-257`) + **Next-Move P1** (`:306-308`) + **`x86_tree_deleted` schema**
  (`:336-338`): same (a)-(g)-only list.

**This is the CH3-dispositive residual.** The binding SYNTHESIS P1 row (`:326`), the HANDOFF P1
receiver (`:101-112`), and BOTH `x86_tree_deleted` telemetry sites (`SYNTHESIS:576` / `HANDOFF:336`)
each assert — *in their own text* — that the deletion list is "reach-matched to the verify grep"
and that "every active hit the grep flags is on the removal list." Disk-verified, that claim is
**empirically FALSE**: the crate-wide grep fires on `tests/checkasm_parity.rs` (9 active call sites)
and `src/scalar/byte_class_from_eq_set_64.rs` (`:10,12,15`), neither of which is on the (a)-(g)
removal list. A receiver executing exactly (a)-(g) leaves the P1 verify grep **RED-by-construction**
on `checkasm_parity.rs`'s 11 hits — AND worse, deleting `src/x86_64/` per (a) without decoupling
`checkasm_parity.rs`'s 9 `bbnf_simd::x86_64::…` call sites **breaks `cargo test --no-run`** (an
unnamed compile-coupling the binding contract does not surface).

This is the **exact RED-by-construction mirror CH6 V4 §1 / V5 REVISE-2 exists to close, surviving
in the binding artefact one reach level deeper** — the same defect class the V3→V4 P1 widening
fixed for `Cargo.toml`/`lib.rs`, now un-fixed for `tests/`/`scalar/`. On the CH3 lens it is a
**PRUNE-gate-satisfiability regression**: the mandatory lands-FIRST PRUNE P1 gate is RED-by-
construction in the binding contract, which (per the V5 CONSOLIDATED's own framing) "invites a
receiver to silently narrow the grep back or hand-wave the hits dormant — a paper-close hazard on
the mandatory PRUNE gate." The feeder fold (αC/αE) is correct but the **binding inventory-of-record
the receiver consults (SYNTHESIS P1 + `x86_tree_deleted` telemetry + HANDOFF P1) is NOT reach-
matched** — the precise propagation gap REVISE-2 names, NOT discharged.

**Disposition: REVISE (not REJECT — single-edit, mechanism-correct, αC carries the verbatim fix).**
Fold into the binding P1 list + `x86_tree_deleted` telemetry at all four sites
(`SYNTHESIS:326`, `SYNTHESIS:576`, `HANDOFF:101-112`, `HANDOFF:336`): add **(h)** decouple-or-delete
`tests/checkasm_parity.rs`'s x86_64 block (the 9 active `bbnf_simd::x86_64::…::*_scalar(…)` call
sites `:458-502` + the `#[ignore]` x86 harness; closes BOTH the compile-coupling and the grep RED)
and **(i)** scrub `src/scalar/byte_class_from_eq_set_64.rs:10,12,15` to aarch64/scalar-neutral. With
(h)+(i) the binding list is genuinely reach-matched (5 firing files outside `src/x86_64/`, 5 named
removal targets) and the "every active hit on the removal list" claim becomes true.

---

## §3 — The three CH3 axes (V6 global findings)

### Axis 1 — does any proposed intervention re-open a REDRESS pre-block? **HELD (architecturally).**
Candidate count = 5 (unchanged); each carries an explicit Pre-blocks line. All six pre-block
families re-checked LIVE: AZ-IV eager (G4 trait stays lazy over the tape), StructRegistry per-leaf
(no per-leaf registry; G3 bound to no-second-substrate + the relocated-seam structural check),
fact-stream-as-output (RETIRED, `emit_fact_stream`=0; narrowed to the `CSS_GENERATED_RS` courier
residual), 24-broadcast (PERMANENT), FNV (bench-only; P5 symbol-name purge), x86/AVX/SVE (P1 deletes
BOTH surfaces crate-wide). REDRESS-W2-1 single-emitter is correctly the G3 SUBJECT, not a re-open.
**No architectural re-open. The F16 fold STRENGTHENS the relocated-seam guard.** The ONE enforcement-
reach gap is FOLD 2 above: the x86 pre-block's *binding deletion enforcement gate* is not reach-
matched to its own grep — an enforcement-completeness defect, not an architectural re-open.

### Axis 2 — PRUNE-before-GENERALIZE. **HELD, with the P1-gate-satisfiability caveat.**
Sequencing A → B1 → B2 → B3 → B4 with P4 landing BEFORE the B1/G2/G3 emitter rebuild is binding
(αE §0/CC#1, SYNTHESIS PRUNE-first, HANDOFF Next-Move). The F16/CH2 fold does not disturb
sequencing. **Caveat:** the FOLD-2 gap makes the FIRST prune (P1) gate RED-by-construction in the
binding contract — the lands-FIRST PRUNE wave cannot cleanly close as written until (h)+(i) land.
Sequencing order is intact; the P1 EXIT gate is not satisfiable-by-construction in the binding text.

### Axis 3 — prune does not strand `>`SOTA. **HELD.**
No deletion removes `>`SOTA-bearing code. P1: `src/x86_64/` = 0 real intrinsics / 14
`unimplemented!`; `ext/x86/` dormant on aarch64 (`build.rs:38-40` non-x86 early-return);
`tests/checkasm_parity.rs`'s aarch64 parity assertions are RETAINED (only the x86 block is
decoupled, αC `:188`/αE `:101`) — no aarch64 differential discipline is stranded. P2: headline came
from `css_canon_bench` (KEPT, explicitly protected). P3: md5=1 over 7 dirs; distinct-grammar-output
gate bound to provenance. P5: symbol-name purge only. The honest-finding escape is GATED
((a) `.bbnf`-invoked, (b) grammar-derived DATA, (c) `verbatim_blob_present==false`). **Nothing
stranded.**

---

## §4 — Per-section dispositions (V6 confirming)

| Section | Disposition | Basis |
|---|---|---|
| alphaA-results-extraction.md | **ACCEPT** | BOTH x86 surfaces named crate-wide (`:204`,`:93`,`:292-296`); 24-broadcast named pre-blocked route not `>`SOTA; pre-block list verbatim. CH5 `tests/`/`scalar/` reach is αC/αE-owned, not αA's axis. |
| alphaB-competitor-deltas.md | **ACCEPT** | CSS bar lazy-vs-eager asymmetric; asmjson AVX-512 OUT (aarch64); JSON comparators strict/cold/no-broadcast; no x86-surface dependency. |
| alphaC-redress-digest.md | **ACCEPT** | §2.6 names BOTH x86 surfaces + crate-wide grep "not `src/`-scoped"; §4 C.1 (`:168-215`) reach-COMPLETE for `tests/checkasm_parity.rs` decouple + `scalar/byte_class` scrub. The load-bearing CH3 feeder; the FOLD-2 fix is verbatim here. |
| alphaD-validated-invalidated.md | **ACCEPT** | §5 PRE-BLOCKED holds S1–S13 against the six families; §1 marks `css_canon_bench`/substrate/the two `>`SOTA "preserve, do NOT re-prove." |
| alphaE-candidate-shortlist.md | **ACCEPT** | P1 row (`:94`)/exit gate (`:104`)/checkasm status (`:101`) carry the (3) COMPILE-COUPLED widening (V5 R-2/CH5 F.6); 5 candidates unchanged; PRUNE-before-GENERALIZE/P4-before-B1 held. |
| **SYNTHESIS.md** (αF contract) | **REVISE** | F16/CH2 fold DISCHARGED (`:333`/`:411`/`:566` name `profile`/`source_inputs`/`metadata_inputs`). **But the CH5 REVISE-2 fold is NOT in the binding contract:** P1 close row (`:326`) + `x86_tree_deleted` telemetry (`:576`) enumerate (a)-(g) ONLY, OMIT `tests/checkasm_parity.rs` (9 active call sites, compile-coupled) + `src/scalar/byte_class_from_eq_set_64.rs` — both grep-firing at HEAD. The row's own "deletion list reach-matched … every active hit on the removal list" claim is empirically FALSE; RED-by-construction P1 gate. Fold (h)+(i) per αC. |
| **HANDOFF.md** | **REVISE** | Same CH5 gap: P1 receiver (`:101-112`) + `x86_tree_deleted` schema (`:336-338`) + invariant 3 (`:253-257`) enumerate (a)-(g) ONLY, asserting "the deletion list is reach-matched … (a list narrower than the grep ships a RED-by-construction gate)" while omitting the two grep-firing files — the self-refuting mirror. Fold (h)+(i). |

---

## §5 — Verdict

**Five sections ACCEPT (alphaA, alphaB, alphaC, alphaD, alphaE); two sections REVISE (SYNTHESIS,
HANDOFF) on the SAME single defect.** The CH2 §8.1 / F16 fold (`profile`/`source_inputs`/
`metadata_inputs` enumeration) is DISCHARGED in the binding contract and orphan-free — it
strengthens the relocated-seam guard, no re-open. The CH5 F.6 / REVISE-2 fold (P1 x86 deletion
list + `x86_tree_deleted` telemetry widening to `tests/checkasm_parity.rs` + `src/scalar/
byte_class_from_eq_set_64.rs`) is DISCHARGED in the FEEDERS (αC reach-complete; αE folded) but is
**NOT propagated to the binding SYNTHESIS P1 row (`:326`) / `x86_tree_deleted` telemetry (`:576`) /
HANDOFF P1 (`:101-112`) / `x86_tree_deleted` schema (`:336`)** — the precise binding-propagation
gap the V5 CONSOLIDATED §4 REVISE-2 itself names as the fold target, NOT yet landed in the
inventory-of-record. The binding artefacts assert "reach-matched … every active hit on the removal
list" while their own crate-wide grep disk-fires on two unlisted files, one compile-coupled — a
RED-by-construction P1 PRUNE gate, the exact mirror CH6 V4 §1 exists to close, surviving one reach
level deeper in the binding contract.

All three CH3 axes hold architecturally: no candidate re-opens a pre-block family, PRUNE-before-
GENERALIZE binds, nothing `>`SOTA is stranded. The single residual is an **enforcement-completeness
defect on the binding P1 deletion gate** — REVISE (single-edit, mechanism-correct, αC carries the
verbatim fix), not REJECT, not architectural, not a re-open, not a stranded `>`SOTA.

**Fix (verbatim, αC-sourced):** at `SYNTHESIS:326`, `SYNTHESIS:576`, `HANDOFF:101-112`, `HANDOFF:336`
add **(h)** decouple-or-delete `tests/checkasm_parity.rs`'s x86_64 block (the 9 active
`bbnf_simd::x86_64::…::*_scalar(…)` call sites `:458-502` + the `#[ignore]` x86 harness) and
**(i)** scrub `src/scalar/byte_class_from_eq_set_64.rs:10,12,15` to aarch64/scalar-neutral — making
the binding deletion list genuinely reach-matched (5 grep-firing files outside `src/x86_64/`, 5
named removal targets) and the gate satisfiable-by-construction.

TALLY accept=5 revise=2 reject=0
