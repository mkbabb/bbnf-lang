# CH3 REGRESSION (V2) — SK-V18 Pass-Alpha hardening

**Lens:** CH3 Regression per `PASS-ALPHA §3` ("does any proposed intervention re-open a
route in REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly
identified the pre-block list?") + `ORCHESTRATOR §3W/§3Z`.
**Subject under review (V2):** `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
(no `alphaF-*.md` exists; per `PASS-ALPHA §2/§6` the α-F deliverable IS `SYNTHESIS.md` +
`HANDOFF.md`, both reviewed) + `SYNTHESIS.md` + `HANDOFF.md`.
**Host:** aarch64 Apple M5 Max ONLY (x86 OUT). **HEAD of record:** `318d9c046` (unchanged
since V1; the entire `sk-v18/` tree is untracked working-state).
**Method:** This is the V2 *confirming* CHALLENGE. The V1 CH3 returned 100% ACCEPT but the
V1 CONSOLIDATED prescribed an αF V2 fold of 24 cross-lens REVISEs into `SYNTHESIS.md` +
`HANDOFF.md` (V1 CONSOLIDATED §3) — four of which touch the CH3 regression axis (root
causes #3 md5-necessary-not-sufficient, #4 Sheets sourcing, #5 `ValueRef` two-axis,
#7 deferred-revert paper-close). The V2 duty is therefore: (a) re-grep every pre-block
ground-truth at HEAD (do NOT trust the V1 log); (b) confirm the V2 fold landed in the
contract WITHOUT introducing a regression vector; (c) confirm my two V1 minor notes were
folded. Every disposition cites `path:line`/SHA.

---

## Verification log (re-grepped LIVE at HEAD `318d9c046`, V2 independent re-run)

| Pre-block / claim | Command | Result | Artefact agreement |
|---|---|---|---|
| x86 tree present (P1 target) | `find …/bbnf-simd/src/x86_64 -type f \| wc -l` | **24** | SYNTHESIS:86-88/197, HANDOFF:57/83, alphaC:89 ✓ |
| fact-stream RETIRED | `grep -c emit_fact_stream …css_l4_declaration_values/generated.rs` | **0** | SYNTHESIS:240, alphaC §2.3 narrowing ✓ |
| CSS const-`&str` courier (G2 / verbatim-blob) | `grep -n 'const CSS_GENERATED_RS' …runtime_generator.rs` | **`:701`** `const … &str = r#"` | SYNTHESIS:63/171, alphaD I1, §0.4 verbatim-blob ✓ |
| `RuntimeEmitterKind` fork (G3) | `grep -n 'enum RuntimeEmitterKind' …grammar_provider.rs` | **`:40-42`** `CompiledLowering`/`RequestFacts` | SYNTHESIS:70-72/372, HANDOFF:50 ✓ |
| phantom `<G>` (G4) — full decl | `grep -n 'pub struct ValueRef' tape/mod.rs` | **`:175`** `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` | SYNTHESIS:173/393, alphaE B3 ✓ |
| Lock-14 roots + x86 tag + allowlist assert (P4) | `grep -n 'GENERIC_SCAN_ROOTS\|diagnostic-x86' lock14_baseline.rs` | `:2409` roots · `:2463` `diagnostic-x86` · `:4956` `…contains` assert | SYNTHESIS:90-95/168, alphaC P4 ✓ |
| metalang leak (P5) | `grep -c parse_w11_1_number …json/generated.rs` | **7** | SYNTHESIS:116-118, alphaC §2.5/P5 ✓ |
| `css_canon_bench` KEPT (headline harness, NOT pruned) | `ls …/bin/css_canon_bench.rs` | EXISTS | SYNTHESIS:114/166, alphaE A pre-block ✓ |
| 7 CSS replicas byte-identical (P3) | `md5 …css_l4_*/generated.rs \| sort -u \| wc -l` | **1** | SYNTHESIS:73-76/194, alphaE CC#3 ✓ |
| 16-lock count | `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | **16** | SYNTHESIS:144/179, HANDOFF:220 ✓ |
| **[V2-NEW] md5-necessary-not-sufficient is REAL** | `rg -nc 'Json =>\|RequestFacts\|CompiledLowering' …codegen/src` | `grammar_provider.rs:3`, `lib.rs:2`, `runtime_generator.rs:2` — grammar-family branching LIVE | SYNTHESIS:172/392 G3 co-gate, HANDOFF:224-234 invariant 5 ✓ |
| **[V2-NEW] Sheets `google-sheets.bbnf` EXISTS (PROVE adopt-not-stub)** | `find . -name google-sheets.bbnf` | `grammar/google-sheets/google-sheets.bbnf` (totality tree) | SYNTHESIS:176/222, HANDOFF:115, alphaE B3/B4 F2 ✓ |
| **[V2-NEW] phantom `G` instantiations test-only** | `rg '_proof_compiles\|JsonEventGrammar\|SheetsEventGrammar' …tape/event_grammar_tests.rs` | `:18-21` `_proof_compiles::<{Json,Sheets,Any}EventGrammar>` (test-only) | SYNTHESIS:77-80, alphaE B3 ✓ |

Every CH3-load-bearing ground-truth claim resolves as stated at HEAD. No fabricated
citation. The one prior apparent discrepancy (x86 "23" vs "24") was reconciled in V1 and
the alphaA census now reads "24 files (23 `.rs` + 1 `.asm`)" per the V1 CH7 fold. The
three V2-new checks confirm the V2 contract folds are GROUNDED, not aspirational: the
`RuntimeEmitterKind` grammar-family fork is genuinely live in codegen (so the
"md5-distinct is necessary-not-sufficient" G3 co-gate is load-bearing, not decorative),
the Sheets `.bbnf` genuinely exists (so PROVE adopts a real Pratt grammar rather than
authoring a third-JSON stub), and the `G` phantom is genuinely test-only.

---

## The three CH3 axes — V2 global findings

### Axis 1 — does any proposed intervention re-open a REDRESS pre-block?

**Finding: NO (confirmed; V2 fold introduced no regression vector).** The V2 contract did
not add any candidate — alphaE still carries exactly 5 (`grep -c '^### CANDIDATE'` = 5:
A, B1, B2, B3, B4), identical to V1; the shortlist remains "additive-by-deletion"
(alphaE CC#6, line 201). Cross-checking all 5 candidates + the 13 still-open S1–S13
(alphaD §5) against the six pre-block families re-confirms V1's clean result, and the V2
folds *tighten* three of the carriers:

- **AZ-IV eager-value-tree (118×):** SYNTHESIS:230-234 + HANDOFF:176-178 keep G4's shared
  trait "LAZY over the tape — it does NOT re-introduce an eager value tree"; alphaE B3
  pre-block line (133:147) "the trait stays lazy/tape-backed." The **V2 fold tightened
  this**: G4 now also carries `json_rich_navigation_preserved == true` (SYNTHESIS:395,
  alphaE B3 FOLD-F7) so a ≥2-impl LCD-flatten can no longer slip a richness regression
  past a green ≥2 count — a flatten would be a preserve-rich-ast (Lock) violation, not an
  AZ-IV re-open, but the new gate closes an adjacent regression seam. **No re-open.**
- **StructRegistry / Arena/Builder per-leaf (28-65× / 983× / 10583× WATCHDOG):**
  SYNTHESIS:236-238 + HANDOFF:179-180 keep the permanent pre-block on per-leaf
  indirection; the no-second-substrate clause (SYNTHESIS:283-287, HANDOFF:199-202)
  REJECTs an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside the
  landed `Tape`/`ValueRef`. alphaC §2.2 splits permanent-pre-block (per-leaf) from
  admit (once-per-rule `LayoutFacts`). **No re-open.**
- **CSS fact-stream String-as-output:** verified RETIRED at HEAD (`emit_fact_stream` = 0).
  SYNTHESIS:240-242 + HANDOFF:181-183 explicitly pre-block G2 from "replace the
  const-string courier with a fact-stream String." alphaC §2.3 narrows to the residual
  (`CSS_GENERATED_RS` + `RequestFacts`) — the correct refinement of a dead route.
  **No re-open.**
- **24-row broadcast:** SYNTHESIS:247-248 + HANDOFF:187 retire it; alphaE B4 pre-block
  binds the Sheets cell-corpus to "cold per-parse, not 24-broadcast." **No re-open.**
- **FNV / fixture contrivances:** alphaC §2.5 (re-verified, lines 372-399) correctly binds
  the NEW re-entry seam — the generator's tape pre-sizing — to derive from `input.len()` +
  `BackendRule`/`LayoutFacts` grammar-generally, never per-corpus, and tags P5 as the live
  instance. SYNTHESIS:249-252 + HANDOFF:188 keep FNV bench-only. **No re-open.**
- **x86 / AVX / SVE:** SYNTHESIS:253-255 + HANDOFF:189-190 keep aarch64-only; P1 enforces
  by deletion. **No re-open.** (P1 census re-verified: 24 files present, awaiting deletion.)

The "no second substrate" Lock-1 guard (the subtlest CH3 re-entry for G4) is held in every
artefact: SYNTHESIS:283-287, HANDOFF:199-202, alphaC §3, alphaD §5, alphaE B2/B3 pre-block
lines. **Verdict: Axis 1 HELD.**

### Axis 2 — PRUNE-before-GENERALIZE

**Finding: HELD, with the V1 ordering subtlety correctly folded.** The sequencing
A → B1 → B2 → B3 → B4 with P4 (Lock-14 gate) landing BEFORE the B1/G2/G3 emitter rebuild is
binding across SYNTHESIS §0.3/Section 3 (lines 210-211, 437-440), HANDOFF Next-Move
(lines 249-265), alphaE §0/CC#1 (line 196). The one genuine subtlety I flagged at V1 — the
P1 x86-tag removal must land in the SAME commit as the P4 `accepts_current_allowlist`
`…contains("…/x86_64")` assertion drop, else the gate desyncs — is now **folded** into
alphaC as an explicit **V2-FOLD** (alphaC:59-63 CH3 non-blocking note + alphaC:99-104 P1
obligation "the x86-tag removal AND the corresponding `accepts_current_allowlist`
`…contains("…/x86_64")` assertion drop … same commit"). The HEAD greps confirm both edit
sites exist (`:2463` tag, `:4956` allowlist assert). **Verdict: Axis 2 HELD; V1 note #1
discharged.**

### Axis 3 — prune does not strand `>`SOTA

**Finding: HELD (confirmed).** No deletion removes `>`SOTA-bearing code:
- **P1 (x86):** 0 real intrinsics, 14 `unimplemented!` (alphaC:91-92) — zero admission weight.
- **P2 (old CSS bench):** the headline numbers came from `css_canon_bench` (KEPT, EXISTS at
  HEAD), NOT `nonjson_css_l4.rs measure_mbps` (SYNTHESIS:109-115). alphaE A pre-block line
  forbids deleting `css_canon_bench.rs`/`w2_rich_cssom_bench.rs`. **Confirmed.**
- **P3 (replicas):** md5 = 1 (re-verified) — collapsing 6 of 7 strands no unique capability.
- **P5 (metalang):** symbol-name purge only; function bodies stay (alphaC §2.5).

The standing-order invariant ("a derived parser that loses the speed is not done — surface
honestly as a named validated grammar-parameterized primitive, do NOT paper-close") is now
**hardened** by the V2 fold: the PASS-IMPL V4 honest-finding escape is itself gated by the
(a)-(c) qualification (SYNTHESIS:181: the primitive must be `.bbnf`-INVOKED by name,
parameterized by grammar-derived DATA, and carry `verbatim_blob_present == false`), mirrored
in alphaE CC#2 (line 197). This converts the single largest paper-close surface in the
contract into a checked condition — the strongest possible discharge of "prune does not
strand `>`SOTA." SYNTHESIS §0.5 fallback column + Section 3 carry the same escape.
**Verdict: Axis 3 HELD; the prune cluster strands nothing; the generalize clusters carry a
GATED honest-finding escape (not a bare one).**

---

## V2 fold verification (the four CH3-touching root causes from V1 CONSOLIDATED §3)

| RC | Fold claimed (V1 CONS §3) | Landed in V2 contract? | HEAD-grounded? |
|---|---|---|---|
| **#3 md5 necessary-not-sufficient** | G3 binds canonical Lock-14 three-surface model + `match grammar`-arm grep co-gate `generator_grammar_branch_count == 0` | **YES** — SYNTHESIS:172 G3 gate + :392 telemetry col + HANDOFF:224-234 invariant 5 (dual gate: token scan AND arm census + `EventGrammar` witness-token note) | YES — `RuntimeEmitterKind` grammar-family branching is LIVE in `codegen/src` (3 hits), so the co-gate guards a real leak |
| **#4 Sheets sourcing** | Adopt existing Pratt `google-sheets.bbnf`; `sheets_grammar_shape == pratt-operator` | **YES** — SYNTHESIS:176/222 PROVE + :399 telemetry col + HANDOFF:115; alphaE B3/B4 FOLD-F2 | YES — `grammar/google-sheets/google-sheets.bbnf` exists; a flat-stream/tree Sheets is REJECTed (third-JSON hollowing, SYNTHESIS:428) |
| **#5 `ValueRef` two-axis** | Name `G: EventGrammar` axis vs real `K=Kind`; DELETE default; trait separable from `<G>` | **YES** — SYNTHESIS:173 G4 gate (`K` is real, `G` is phantom; DELETE is abrogate-before-patch default) + :393 telemetry; HANDOFF:104-106; alphaE B3 FOLD-F6 | YES — `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` at `:175` confirms 4 params, `G` test-only |
| **#7 deferred revert/cap** | Revert dependency graph PRUNE→G1→…→PROVE; hard-cap defaults 20/15/30; honest-finding (a)-(c) gate | **YES** — SYNTHESIS:475-485 + HANDOFF:287-297 carry the dependency graph + hard-cap defaults; the (a)-(c) escape gate at SYNTHESIS:181 | n/a (process gate) |

All four CH3-touching folds landed AND are grounded at HEAD. The fold is orphan-free on the
CH3 axis: every V1 CH3-relevant REVISE has a corresponding V2 edit with a live citation.

My two V1 minor (non-blocking) notes are also folded:
- **V1 note #1** (P1 x86-tag + P4 allowlist-assert to one commit): folded into alphaC:59-63 +
  :99-104 as an explicit V2-FOLD.
- **V1 note #2** (B3 instantiate-test regex must tolerate the `K` slot): folded — alphaE B3
  now greps the SUBSTRING `'G: EventGrammar'` for the DELETE-default branch (robust to the
  `K` slot) and `'ValueRef<.*EventGrammar>'` with a `.*` wildcard + a test-exclusion
  (`grep -v 'tests\.rs\|#\[cfg(test)\]'`) for the INSTANTIATE branch (alphaE:139), which both
  tolerates the 4-param layout AND closes the test-only false-green (CH5 E.1).

---

## Per-section dispositions (V2)

### alphaA-results-extraction.md — **ACCEPT**
CH3-relevant content unchanged in substance from V1 and re-verified: the retired 24-row
broadcast is named "a pre-blocked route, NOT the `>`SOTA"; the pre-block list is verbatim
from the seed; PRUNE close conditions preserve the headline harness; the x86 census reads
"24 files (23 `.rs` + 1 `.asm`)" per the V1 CH7 fold (`bbnf-simd/src/x86_64` = 24 confirmed).
No intervention here re-opens a pre-block. **ACCEPT.**

### alphaB-competitor-deltas.md — **ACCEPT**
The CSS bar is framed ASYMMETRIC lazy-vs-eager (discharging the timed-plane-symmetry
pre-block honesty); asmjson AVX-512 held "permanently OUT (aarch64 mandate)"; JSON
comparators strict, cold, no-broadcast. The V1 CH1 yyjson-runnability fold (honest-`None`
on aarch64, FFI not wired) is reflected in alphaE CC#4 (line 199) and SYNTHESIS §0.6 — a
fabricated competitor column is now REJECTed, which is a *strengthening* of the
no-more-work-competitor pre-block. **ACCEPT.**

### alphaC-redress-digest.md — **ACCEPT** (the load-bearing CH3 artefact)
This is the artefact CH3 most directly reviews. It is now "cycle V2" (alphaC:3) and is
correct, complete, and tightened:
- §2.1–§2.6 enumerate exactly the six pre-block families, each with PERMANENT-vs-ADMIT
  classification, a re-open test keyed to the THREE new SK-V18 surfaces (generator / shared
  trait / instantiated-`<G>`), and a different-framing admission — re-verified line-by-line.
- The §0 state-delta correctly narrows the fact-stream pre-block to its residual after
  `emit_fact_stream` = 0 (a genuine CH3 refinement preventing a dead-route fight).
- §3 "checked TWICE — against runtime output AND the emitter that produces it" remains the
  exactly-right framing for a generalization cycle: the generator can re-open a pre-block at
  its SOURCE, not only in the runtime. The md5-necessary-not-sufficient G3 co-gate is the
  machine-checkable form of this corollary, and the HEAD grep confirms it guards a LIVE fork.
- **My V1 minor note #1 is now folded as an explicit V2-FOLD** (alphaC:59-63 + :99-104):
  the P1 x86-tag removal + the P4 `accepts_current_allowlist` assertion drop bind to one
  commit. This resolves the only sequencing subtlety I raised. **ACCEPT.**

### alphaD-validated-invalidated.md — **ACCEPT**
§5 PRE-BLOCKED asserts "NONE of S1–S13 re-opens any" of the six families with the
new-surface binding (G4 over EXISTING lazy `ValueRef`; G2 toward lowering NOT fact-stream;
P5 a symbol-name purge NOT an FNV migration; P1 DELETES x86). §1 VALIDATED marks
`css_canon_bench`/substrate/the two `>`SOTA proofs as "do NOT re-prove; preserve" — the
prune-does-not-strand guard. DM1 (typed rows ride the per-corpus hand schema) is DEMOTED to
conditional-not-headline with a narrative obligation, so it does not re-open the
FNV/fixture pre-block. **ACCEPT.**

### alphaE-candidate-shortlist.md — **ACCEPT**
The object `PASS-ALPHA §3` instructs CH3 to "cross-check against entries 1-N." Re-verified:
- Candidate count = 5 (unchanged from V1 — no new candidate, no regression vector); each
  carries an explicit Pre-blocks line; CC#6 (line 201): "No candidate re-opens a pre-blocked
  route: verified against the V3 pre-block list … CH3 was 100% ACCEPT."
- Sequencing (§0, CC#1) enforces PRUNE-before-GENERALIZE with P4 before B1, with the
  exit-gate-blocks-successor dependency carried into S-P3.
- **My V1 minor note #2 is folded** (alphaE:139): the B3 gate now uses substring/`.*`
  greps that tolerate the `K` slot AND a test-exclusion that closes the false-green.
- The honest-finding escape (CC#2, line 197) is sharpened to require `.bbnf`-invocation +
  parameterization + checkasm/scalar reference — the prune-does-not-strand-`>`SOTA guard,
  now a checked condition, isomorphic to SYNTHESIS:181's (a)-(c) gate.
- B4 binds the same-wave-consumer rule (V5 orphan-kernel) + acceleration-at-admission (G6).
**ACCEPT.**

### SYNTHESIS.md (the §0 contract / goalset, standing for alphaF) — **ACCEPT**
The V1→V2 fold note (lines 14-24) declares all V1 REVISEs applied, no orphans; re-verified
on the CH3 axis. §0.4 Pre-blocks carries all six families verbatim + the verbatim-blob /
phantom-generic / distinct-grammar-output re-entries + the no-second-substrate Lock-1
clause. §0.1 G3 binds the canonical Lock-14 model + `generator_grammar_branch_count == 0`
co-gate ("md5-distinctness alone is necessary-not-sufficient"); G4 names the `G` axis vs
the real `K` axis with DELETE-default; PROVE adopts the existing Pratt `.bbnf`; the
PASS-IMPL V4 honest-finding escape is (a)-(c) gated. §2 telemetry makes every pre-block
machine-checkable per row (`verbatim_blob_present`, `emitter_fork_present`,
`generator_grammar_branch_count`, `phantom_generic_resolved`, `json_rich_navigation_preserved`,
`acceleration_at_admission`, `x86_tree_deleted`, `lock14_gate_scans_codegen`,
`metalang_leak_present`). No goalset gate re-opens a pre-block; PRUNE-first is binding.
**ACCEPT.**

### HANDOFF.md — **ACCEPT**
Pre-Blocked Routes (lines 172-216) carries the six families + six CHALLENGE addenda +
inherited REDRESS family ids + the hidden-coupling escape list + the no-second-substrate
clause. Invariant 5 (lines 224-234) now binds the canonical three-surface Lock-14 model +
BOTH the token scan AND the `match grammar`-arm census + the `EventGrammar` witness-token
note (the V2 fold of root cause #3). Invariant 6 names the pre-block families. The SK-V17
residual REDRESS-W2-1 single-emitter is correctly declared the SK-V18 G3 SUBJECT,
"admitted to be discharged here — NOT re-opens" (line 207). Next-Move sequences
PRUNE→GENERALIZE→PROVE→HONESTY with P4-before-emitter, the revert dependency graph
(PRUNE→G1→…→PROVE→H1), and the hard-cap defaults. **ACCEPT.**

---

## Summary

All seven reviewed sections (alphaA, alphaB, alphaC, alphaD, alphaE, SYNTHESIS §0, HANDOFF)
pass the CH3 lens at V2. The V2 fold landed correctly on every CH3-touching root cause
(#3 md5-necessary-not-sufficient, #4 Sheets-adopt-not-stub, #5 `ValueRef` two-axis, #7
gated honest-finding escape) and introduced NO regression vector (candidate count
unchanged at 5; the shortlist is additive-by-deletion). All six pre-block families
re-verify LIVE at HEAD `318d9c046`; the three V2-new HEAD checks confirm the folds are
grounded, not aspirational (the `RuntimeEmitterKind` fork is genuinely live, so the G3
co-gate is load-bearing; `google-sheets.bbnf` genuinely exists, so PROVE adopts a real
Pratt grammar; the `G` phantom is genuinely test-only). PRUNE-before-GENERALIZE is binding
with the one ordering subtlety (P1 tag + P4 allowlist-assert to one commit) now folded into
alphaC; the prune cluster strands no `>`SOTA (headline `css_canon_bench` KEPT;
x86/replicas/metalang carry zero admission weight); the generalize clusters carry a GATED
honest-finding escape. Both of my V1 minor notes are folded into the V2 artefacts.

**Convergence posture:** 7 ACCEPT / 0 REVISE / 0 REJECT = 100% ACCEPT on the CH3 axis (≥95%
threshold met, second consecutive cycle). Zero orphan REVISE. No re-opened REDRESS pre-block.

TALLY accept=7 revise=0 reject=0
