# CH3 REGRESSION (V1) — SK-V18 Pass-Alpha hardening

**Lens:** CH3 Regression per `PASS-ALPHA §3` ("does any proposed intervention re-open
a route in REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly
identified the pre-block list?") + `ORCHESTRATOR §3W`.
**Subject under review:** `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
(no alphaF exists; the §0.x contract draft lives in `SYNTHESIS.md` + `HANDOFF.md`,
which this lens also reviews) + `SYNTHESIS.md` + `HANDOFF.md`.
**Host:** aarch64 Apple M5 Max ONLY. **HEAD of record:** `318d9c046`.
**Method:** every disposition cites `path:line`/SHA; the pre-block re-open tests were
**live-grepped at HEAD** (see §Verification). Focus, per the dispatch: (1) no wave
re-opens the REDRESS pre-block (AZ-IV / StructRegistry / fact-stream / 24-broadcast /
FNV-runtime / x86-AVX-SVE); (2) PRUNE-before-GENERALIZE; (3) prune does not strand
`>`SOTA.

---

## Verification log (re-grepped live at HEAD `318d9c046`)

| Pre-block / claim | Command | Result | Artefact agreement |
|---|---|---|---|
| x86 tree present (P1 target) | `find …/bbnf-simd/src/x86_64 -type f` | **24** (23 `.rs` + 1 `.asm`) | alphaC/D 24 files, alphaA "23 `.rs`" — both correct (rs+asm) ✓ |
| fact-stream RETIRED (alphaC §2.3 narrowing) | `grep -c emit_fact_stream …css_l4_declaration_values/generated.rs` | **0** | alphaC §0/§2.3 "gone" ✓ |
| W5C profile array retired | `grep -n W5C_REQUEST_FACT_PROFILES …codegen/src/lib.rs` | `:298` retirement COMMENT | alphaC §0, alphaD §5 ✓ |
| CSS const-string courier (G2 target) | `grep -n 'const CSS_GENERATED_RS' …runtime_generator.rs` | `:701` | alphaC §2.3, alphaD I1 ✓ |
| RuntimeEmitterKind fork (G3 target) | `grep -n 'enum RuntimeEmitterKind' …grammar_provider.rs` | `:40` | alphaD I4 ✓ |
| 7 CSS replicas byte-identical (P3) | `md5 … css_l4_*/generated.rs \| sort -u \| wc -l` | **1** | alphaC §1-P3, alphaD I3 ✓ |
| phantom `<G>` (G4) | `grep -n 'G: EventGrammar' tape/mod.rs` | `:175` `= AnyGrammar` default | alphaD I5 ✓ |
| Lock-14 exclusion + x86 tag (P4) | `grep -n 'GENERIC_SCAN_ROOTS\|diagnostic-x86' lock14_baseline.rs` | `:2409` roots, `:2463` `diagnostic-x86` | alphaC §1-P4, alphaD I8 ✓ |
| metalang leak (P5) | `grep -c parse_w11_1_number …json/generated.rs` | **7** | alphaC §1-P5, alphaD I10 ✓ |
| css_canon_bench (the headline harness, NOT pruned) | `ls …/bin/css_canon_bench.rs` | EXISTS | alphaC §1-P2, alphaD V6 ✓ |
| 16-lock count | `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | **16** | SYNTHESIS/HANDOFF ✓ |

Every CH3-load-bearing ground-truth claim resolves as stated. No fabricated citation
found. The one apparent discrepancy (x86 "23" vs "24" files) is reconciled: 23 `.rs` +
1 `.asm` = 24 files; both numbers are used correctly in context (alphaA cites `.rs`,
alphaC/D cite total files and name `byte_class_from_eq_set_64.{rs,asm}` explicitly).

---

## The three CH3 axes — global findings

### Axis 1 — does any proposed intervention re-open a REDRESS pre-block?

**Finding: NO.** Cross-checking all 5 shortlist candidates (alphaE A/B1/B2/B3/B4) and
all 13 still-open candidates (alphaD S1–S13) against the six pre-block families:

- **AZ-IV eager-value-tree (118×):** G1/G2/G4 are the new carrier surfaces. alphaC
  §2.1 binds each correctly (G1 keeps `value_from_ref` lazy; G2 must not materialize a
  typed CSS value per leaf; G4's trait "stays lazy/tape-backed"). alphaE B3 pre-block
  line + alphaD §5 confirm. **No re-open.**
- **StructRegistry / Arena/Builder per-leaf (28–65× / 983× / 10583× WATCHDOG):**
  alphaC §2.2 splits this correctly — PERMANENT pre-block on the per-leaf indirection,
  ADMIT on the once-per-rule `LayoutFacts` layout the generator consumes — and binds it
  to the new G3/G4 surfaces. The §2.2b note that the canonical name is
  `Layout`/`LayoutFacts` (NOT the RETIRED `StructLayout`, `LOCKS.md:160`) is exactly
  the type-ambivalence guard. **No re-open.**
- **CSS fact-stream String-as-output:** already RETIRED (`grep emit_fact_stream` = 0);
  alphaC §0 + §2.3 correctly NARROW the pre-block to its residual surfaces
  (const-string + `RequestFacts` fork) rather than re-fight it. G2 is explicitly
  pre-blocked from "replace the const-string courier with a fact-stream String"
  (HANDOFF:171-172, SYNTHESIS §0.4). **No re-open.**
- **24-row broadcast:** alphaC §2.4 + alphaA:82-84 name it explicitly as retired and
  bind the Sheets corpus (the NEW, discipline-less surface) to per-corpus N≥50 cold
  median. **No re-open.**
- **FNV / fixture contrivances:** alphaC §2.5 binds the generator's tape pre-sizing
  (the seam where a per-corpus capacity constant could re-land) to derive from
  `input.len()` + `BackendRule`/`LayoutFacts`, and correctly tags P5 as the live
  instance of this class. **No re-open.**
- **x86 / AVX / SVE:** alphaC §2.6 binds G5/G6 + the ASM backlog to aarch64-only;
  alphaB:212 keeps AVX-512 permanently OUT. **No re-open.** (P1 enforces by deletion.)

The "no second substrate" Lock-1 guard (the most subtle CH3 re-entry for G4) is held in
every artefact: SYNTHESIS §0.4 closing paragraph, HANDOFF:188-191, alphaC §3, alphaD §5
all REJECT an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the
landed `Tape`/`ValueRef`.

### Axis 2 — PRUNE-before-GENERALIZE

**Finding: HELD, with one ordering subtlety correctly handled.** alphaE §0 ("Ordering
rule") + alphaE CROSS-CUTTING #1 + SYNTHESIS §0.3 + HANDOFF Next-Move all sequence
A → B1 → B2 → B3 → B4 with P4 (Lock-14 gate) landing BEFORE the B1 emitter rebuild "so
the un-forked emitter is actually scanned for neutrality as it is built." alphaC §1
PRUNE-ordering-note handles the one genuine subtlety: the *x86-tag removal* +
*runtime_generator.rs scan-root addition* can land at PRUNE, but the *unified-emitter
scan-root* necessarily follows G3 (the file does not exist yet). This is correct and
not a violation of PRUNE-first.

### Axis 3 — prune does not strand `>`SOTA

**Finding: HELD.** The CH3-critical question is whether any deletion removes
`>`SOTA-bearing code. Verified:
- **P1 (x86):** 0 real intrinsics, 14 `unimplemented!` — carries zero admission weight.
- **P2 (old CSS bench):** the headline numbers came from `css_canon_bench` (KEPT, EXISTS
  at HEAD), NOT from `nonjson_css_l4.rs measure_mbps` (V3 C3, alphaD I6). Confirmed.
  alphaE A pre-block line explicitly forbids deleting `css_canon_bench.rs` /
  `w2_rich_cssom_bench.rs`.
- **P3 (replicas):** byte-identical (md5 = 1 hash); collapsing 6 of 7 strands no unique
  capability.
- **P5 (metalang):** "rename only … the function bodies stay" (alphaC §1-P5) — no
  behaviour change.

The standing-order invariant ("a derived parser that loses the speed is not done —
surface honestly as a named validated grammar-parameterized primitive, do NOT
paper-close") is carried in SYNTHESIS §0.5 fallback column, HANDOFF §6 / Next-Move,
alphaE CROSS-CUTTING #2/#5, alphaD §7. The prune cluster strands nothing; the
generalize clusters carry the honest-finding escape so a `>`SOTA loss is surfaced, not
hidden. **No stranding.**

---

## Per-section dispositions

### alphaA-results-extraction.md — **ACCEPT**

CH3-relevant content: §2 names the retired 24-row broadcast explicitly (lines 82-84) as
"a pre-blocked route, NOT the >SOTA"; the pre-block list (lines 164-166) is complete and
verbatim from the seed; PRUNE close conditions (line 185) preserve the headline harness.
The lazy-vs-eager caveat (§2.1) is correctly tied to H1, not papered. No intervention
proposed here re-opens any pre-block. `path:line` citations spot-verified
(`bbnf-simd/src/lib.rs:5,285-287`; `lock14_baseline.rs:2409,2463`). **ACCEPT.**

### alphaB-competitor-deltas.md — **ACCEPT**

CH3-relevant content: the CSS bar is framed as ASYMMETRIC lazy-vs-eager (line 11, §2),
not papered as equal-work — directly discharging the timed-plane-symmetry pre-block
honesty. asmjson AVX-512 is held "permanently OUT (aarch64 mandate)" (lines 67, 212,
277) — the x86/AVX pre-block. JSON comparators are strict, cold, no-broadcast, parse_only
unconditional (line 264). No comparator re-introduces a more-work-competitor on the
admitted timed plane (P2 deletes the old; the kept `css_canon_bench` is symmetric-corpus
cold). **ACCEPT.**

### alphaC-redress-digest.md — **ACCEPT** (the load-bearing CH3 artefact)

This is the artefact CH3 most directly reviews ("Has α-C correctly identified the
pre-block list?"). It is correct and complete:
- §2.1–§2.6 enumerate exactly the six pre-block families named in the dispatch, each
  with PERMANENT-vs-ADMIT classification, a re-open test keyed to the THREE new SK-V18
  surfaces (generator / shared trait / instantiated-`<G>`), and a different-framing
  admission. This is precisely the cross-check `PASS-ALPHA §3` demands.
- The §0 state-delta ("do NOT re-block as if still live") correctly narrows the
  fact-stream pre-block to its residual after `emit_fact_stream` = 0 — a genuine CH3
  refinement that prevents fighting a dead route.
- §3 the single load-bearing distinction + the SK-V18 corollary ("checked TWICE:
  against runtime output AND against the emitter that produces it") is the exactly-right
  CH3 framing for a generalization cycle: the generator can re-open a pre-block at its
  source, not only in the runtime.
- Lock citations (§3 closing) all resolve; 16-lock count verified.

One **minor non-blocking note** (not a REVISE — does not change disposition): §1-P1
obligation says "DELETE … the `x86_64` gate entries in `lock14_baseline.rs` (the …
`"diagnostic-x86"` tag at `:2463`…)", which is correct, but the `:2463` line and the
`accepts_current_allowlist` assertion at `:4956` (`GENERIC_SCAN_ROOTS.contains`) must
land their edits in the SAME P4 commit that re-scopes the roots, else P1's tag-removal
desyncs the gate. alphaC §1 PRUNE-ordering-note already covers this ("the x86-tag
removal … can land at PRUNE"); S-P3 should bind P1+P4 gate edits to one commit. Carry as
an S-P3 sequencing note, not a defect. **ACCEPT.**

### alphaD-validated-invalidated.md — **ACCEPT**

§5 PRE-BLOCKED asserts "NONE of S1–S13 re-opens any" of the six families and walks each
with the new-surface binding (G4 over EXISTING lazy `ValueRef`; G2 toward lowering NOT
fact-stream; P5 is a symbol-name purge NOT an FNV migration; P1 DELETES x86). This is the
correct CH3 conclusion and matches my independent cross-check. The §1 VALIDATED ledger
correctly marks css_canon_bench (V6) + the substrate (V1) + the two `>`SOTA proofs
(V2/V3) as "do NOT re-prove; preserve" — the prune-does-not-strand guard. DM1 (typed
rows ride the 1014-LOC per-corpus hand schema) is correctly DEMOTED to
conditional-not-headline with a narrative obligation (lines 106-109), which is the right
CH3 handling of a fixture-overfit surface: it is not claimed as grammar-general, so it
does not re-open the FNV/fixture pre-block. §6 verification log re-greps the I-claims at
HEAD. **ACCEPT.**

### alphaE-candidate-shortlist.md — **ACCEPT**

The shortlist is the object `PASS-ALPHA §3` instructs CH3 to "cross-check against
entries 1-N." Result of cross-check:
- Each candidate carries an explicit Pre-blocks line (A, B1, B2, B3, B4) naming the
  families it must not re-open. CROSS-CUTTING #6 states the conclusion: "No candidate
  re-opens a pre-blocked route: verified against the V3 pre-block list."
- The sequencing (§0 Ordering rule, SUMMARY TABLE entry-gate column, CROSS-CUTTING #1)
  enforces PRUNE-before-GENERALIZE with P4 before B1.
- Candidate A risk line + pre-block line confirm prune-does-not-strand (no
  `>`SOTA-bearing code deleted; css_canon_bench kept).
- The honest-finding escape (CROSS-CUTTING #2/#5) is the correct discharge for a
  `>`SOTA-loss-under-derivation: named validated grammar-parameterized primitive, not a
  silent blob, not a paper-close.
- B4 binds the same-wave-consumer rule against the V5 orphan-kernel pattern and the
  acceleration-wiring lens (kernel reached AT ADMISSION, not `#[cfg(test)]`) — the
  correct CH3 guard on G6.

One **minor non-blocking note**: alphaE B3 owner-paths line references the phantom as
"`ValueRef<G: EventGrammar=AnyGrammar>` (`tape/mod.rs:175`)" with the grep gate
`'ValueRef<.*, *\(Json\|Css\|Sheets\)EventGrammar>'`. The live decl is
`ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` (4 type params), so the
instantiate-test regex must tolerate the `K` slot. This is an S-P3 gate-authoring detail,
not a shortlist defect — the *intent* (≥1 real non-`AnyGrammar` instantiation OR delete
the `G` param) is correct and structurally verifiable. Carry to S-P3. **ACCEPT.**

### SYNTHESIS.md (the §0 contract / goalset, standing in for alphaF) — **ACCEPT**

§0.4 Pre-blocks carries all six families verbatim + the verbatim-blob /
phantom-generic / distinct-grammar-output re-entries + the no-second-substrate Lock-1
clause. §0.5 generalization litmus binds each axis to a fallback that surfaces honestly
rather than paper-closes. §0.1 close-conditions and §2 telemetry columns
(`verbatim_blob_present==false`, `emitter_fork_present==false`,
`phantom_generic_resolved`, `acceleration_at_admission∈{admission,…}` NOT
`cfg-test-only`, `x86_tree_deleted`, `lock14_gate_scans_codegen`,
`metalang_leak_present==false`) make every pre-block machine-checkable per row. No
goalset gate proposes a route that re-opens a pre-block; the PRUNE-first standing order
is binding (§0.3, Section 3). **ACCEPT.**

### HANDOFF.md — **ACCEPT**

Pre-Blocked Routes section (lines 161-205) carries the six families + the six new
CHALLENGE addenda + the inherited REDRESS family ids (`28+33, 50-55, 60-72, 80, 82-84,
88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum`) + the hidden-coupling
escape list + the no-second-substrate clause. Invariant 6 (line 215) names the pre-block
families explicitly. The SK-V17 residuals (REDRESS-W2-1 single-emitter) are correctly
declared the SK-V18 SUBJECT (G3/PROVE), "admitted to be discharged here — NOT re-opens"
(line 196) — the correct CH3 distinction between a discharged residual and a re-opened
refutation. Next-Move sequences PRUNE→GENERALIZE→PROVE→HONESTY with P4-before-emitter and
same-wave-consumer per primitive. **ACCEPT.**

---

## Summary

All seven reviewed sections (alphaA, alphaB, alphaC, alphaD, alphaE, SYNTHESIS §0,
HANDOFF) pass the CH3 lens. The pre-block list is correctly and completely identified
(α-C is the load-bearing artefact and is exhaustive); no shortlist candidate re-opens any
of the six refuted families; PRUNE-before-GENERALIZE is binding with the one real
ordering subtlety (unified-emitter scan-root follows G3) correctly handled; and the prune
cluster strands no `>`SOTA-bearing code (the headline `css_canon_bench` is explicitly
KEPT; x86/replicas/metalang carry zero admission weight). The two minor notes (P1+P4 gate
edits to one commit; the B3 instantiate-test regex must tolerate the `K` type slot) are
S-P3 gate-authoring details, not artefact defects, and do not move any disposition.

**Convergence posture:** 7 ACCEPT / 0 REVISE / 0 REJECT = 100% ACCEPT on the CH3 axis
(≥95% threshold met). Zero orphan REVISE.

TALLY accept=7 revise=0 reject=0
