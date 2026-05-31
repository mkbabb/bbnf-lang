# CH3 REGRESSION (V4) — SK-V18 Pass-Alpha hardening

**Lens:** CH3 Regression per `PASS-ALPHA §3` ("does any proposed intervention re-open a
route in REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly
identified the pre-block list?") + `ORCHESTRATOR §3W/§3Z`. Focus per the V4 dispatch:
(1) no wave re-opens the REDRESS pre-block list (AZ-IV / StructRegistry / fact-stream /
24-broadcast / FNV / x86-AVX-SVE); (2) PRUNE-before-GENERALIZE; (3) prune does not strand
`>`SOTA.
**Subject under review (V4):** `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
+ `SYNTHESIS.md` + `HANDOFF.md`. **There is no `alphaF-*.md`** — per `PASS-ALPHA §2/§6` the
α-F deliverable IS `SYNTHESIS.md` + `HANDOFF.md`, both reviewed (the V1/V2/V3 CH3 cycles read
the contract identically).
**Host:** aarch64 Apple M-series ONLY (x86 OUT). **HEAD of record:** `318d9c046` (`git
rev-parse HEAD`; the V3-audit commit `7dbe44c22` that the V4 αE header cites is the *audit*
SHA, NOT working HEAD — confirmed `git cat-file -t 7dbe44c22` = commit
"audit(skinny-impl-overfit-v3)"; the entire `sk-v18/` alpha tree is untracked working-state).
**Method (V4 confirming + delta cycle):** the V3 CH3 returned 100% ACCEPT (7A/0R/0R). The V4
duty is NOT to trust the V3 log: (a) re-grep every pre-block ground-truth LIVE at HEAD,
independently; (b) confirm the V3→V4 fold (alphaC FOLD-1 + FOLD-2; alphaE F13/F14) introduced
NO new candidate and NO re-open vector; (c) verify each V4 fold is propagated consistently into
every artefact a CH3-load-bearing close gate lives in — **a fold that lands in one artefact and
not its gate-bearing sibling is an orphan that produces a false-green close gate**, the exact
P4-class failure this cycle exists to eliminate. Every disposition cites `path:line`/SHA.

---

## Verification log (re-grepped LIVE at HEAD `318d9c046`, V4 independent re-run)

| Pre-block / claim | Command | Result | Artefact agreement |
|---|---|---|---|
| HEAD = `318d9c046` | `git rev-parse HEAD` | `318d9c0469…` | αA:4, αC:129, V3/CH3 all ✓ |
| V3-audit SHA exists | `git cat-file -t 7dbe44c22` | `commit` (audit-v3) | αE:4 cites as entry-audit SHA ✓ |
| x86 `src/x86_64/` tree (P1 target #1) | `find …/bbnf-simd/src/x86_64 -type f \| wc -l` | **24** | SYNTHESIS:246, HANDOFF:99, αC:137, αE:83, αA:26 ✓ |
| x86 `src/x86_64/` `.rs` LOC | `find …/x86_64 -name '*.rs' \| xargs wc -l \| tail -1` | **742** | αC:138-139 (742 `.rs`+105 `.asm`=847), αE F11 ✓ |
| x86 `src/x86_64/` `.asm` | `find …/x86_64 -name '*.asm'` | `byte_class_from_eq_set_64.asm` | αC:138, αE F8 ✓ |
| **x86 `ext/x86/` tree (P1 target #2, FOLD-1)** | `find …/bbnf-simd/ext/x86 -type f \| xargs wc -l \| tail -1` | **3554** (`bbnf.asm`/`x86inc.asm`/`x86util.asm`/`LICENSE-VENDOR`) | **αC:146-149, SYNTHESIS:60/162/246, HANDOFF:13/71/99 ✓ — αE / αA SILENT (defect, below)** |
| **x86 `build.rs` nasm driver (FOLD-1)** | `wc -l …/bbnf-simd/build.rs` | **102** | αC:150-152, SYNTHESIS:62-65/164, HANDOFF:14/100 ✓ — **αE / αA SILENT** |
| **`Cargo.toml` nasm-rs dep (FOLD-1)** | `grep -n 'nasm\|build = ' …/bbnf-simd/Cargo.toml` | `:8 build="build.rs"` · `:19 nasm-rs="0.3"` | αC:153-154, SYNTHESIS:339, HANDOFF:208 ✓ — **αE / αA SILENT** |
| **`lib.rs:247` x86 contract ref (FOLD-1)** | `grep -n 'ext/x86' …/bbnf-simd/src/lib.rs` | `:247 "Contract documented in ext/x86/bbnf.asm"` | αC:155, SYNTHESIS:65/166/246, HANDOFF:101 ✓ — **αE / αA SILENT** |
| fact-stream RETIRED | `grep -c emit_fact_stream …css_l4_declaration_values/generated.rs` | **0** | SYNTHESIS §0.B, αC:120-121, αD ✓ |
| CSS const-`&str` courier (G2 / verbatim-blob) | `grep -n 'const CSS_GENERATED_RS' …runtime_generator.rs` | **`:701`** `&str = r#"` | SYNTHESIS:252, αC:123, αE B2:127 ✓ |
| `RuntimeEmitterKind` fork (G3) | `grep -n 'enum RuntimeEmitterKind' …grammar_provider.rs` | **`:40`** | SYNTHESIS:253/299, HANDOFF:120, αE B1 ✓ |
| phantom `<G>` (G4) — full decl | `grep -n 'struct ValueRef' tape/mod.rs` | **`:175`** `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` | SYNTHESIS:254, αC, αE B3:160 ✓ |
| metalang leak (P5) | `grep -c parse_w11_1_number …json/generated.rs` | **7** | SYNTHESIS:296, αC:292, αE P5:87 ✓ |
| 7 CSS replicas byte-identical (P3) | `find …css_l4_*/generated.rs \| xargs md5 \| awk '{print $NF}' \| sort -u \| wc -l` | **1** (over **7** dirs) | SYNTHESIS:296, αC §1-P3, αE P3:85 ✓ |
| **F13 — arm-census over xtask DATA table = 0** | `rg -nE 'Json\s*=>\|CssL4\s*=>\|(GoogleSheets\|Sheets)\w*\s*=>\|Bbnf\w*\s*=>' skinny/xtask/src \| wc -l` | **0** | αE F13:18/61/116/145/196/226, αC FOLD-2, SYNTHESIS:253(iii) ✓ |
| **F13 — `RuntimeTarget` `(source_roots,entry_rule)` collapses to 1** | `grep 'entry_rule\|CSS_L4_ROOTS' regen_css.rs` (7 rows all `entry_rule:"stylesheet"` + `source_roots:CSS_L4_ROOTS`) | all 7 share ⇒ **`sort -u` = 1 css_l4 row** | αE F13, αC §2.2, SYNTHESIS:253(iii) ✓ |
| Sheets `google-sheets.bbnf` EXISTS | `find . -name google-sheets.bbnf` | `grammar/google-sheets/google-sheets.bbnf` (totality tree) | SYNTHESIS §0.5, HANDOFF, αD S12, αE F2 ✓ |
| Sheets witness is a stub | `find skinny -path '*sheets_witness*' -name '*.rs'` | `event_grammar_witness.rs` + `mod.rs` (~25 LOC) | αD S12, αE B4 ✓ |

Every pre-block ground-truth resolves as stated at HEAD. **Two F13 disk claims independently
reproduced this V4 cycle** (the load-bearing fold of the prior cycle): the arm-census regex
returns **0** matches over `skinny/xtask/src` (because the live `RuntimeTarget` table at
`regen_css.rs:35` carries `grammar_name`/`profile` as neutral-identifier DATA, never a `Json
=>` arm), and all 7 css_l4 rows share `entry_rule:"stylesheet"` + `source_roots:CSS_L4_ROOTS`
so `sort -u` over `(source_roots,entry_rule)` collapses to ONE config row. The F13 re-attribution
(relocated-seam policed by the **P3 structural row-count check**, NOT the grep) is correct and
fully propagated — αE handles it in six places (`:18,61,116,145,196,226`), αC FOLD-2
(`:93-102`), SYNTHESIS `:253(iii)`, HANDOFF Invariant 5. **No CH3-axis regression from F13/F14.**

**The single V4 delta CH3 must adjudicate:** the V4 cycle introduced a NEW fold, **FOLD-1**
(αC `:31-91`, from CH5 V3 §C.5), discovering a **SECOND x86 surface** the V1/V2/V3 cohort
omitted — `ext/x86/` (**3554 LOC** vendored x264/FFmpeg ASM), the nasm-rs `build.rs` driver
(102 LOC), the `Cargo.toml` `nasm-rs` build-dep, and the `lib.rs:247` contract reference, **all
verified LIVE at HEAD this cycle** (see the bolded rows above). This is squarely a CH3 concern:
the x86/AVX/SVE PERMANENT pre-block (αC §2.6) is the route P1 enforces by deletion, and the P1
**close gate** is the machine-checkable proof that "x86 is gone" (R10 binding pin). The fold
correctly widens P1's deletion scope AND moves the close gate from `src/`-scoped to crate-wide.
**The problem: FOLD-1 landed in αC / SYNTHESIS / HANDOFF but was NOT propagated into αE (the
candidate shortlist) or αA (the census).**

---

## The three CH3 axes — V4 global findings

### Axis 1 — does any proposed intervention re-open a REDRESS pre-block?

**Finding: NO new architectural re-open; the V4 fold added no candidate.** The V4 contract
carries exactly **5 candidates** (`grep -c '^### CANDIDATE' alphaE` = 5: A, B1, B2, B3, B4),
identical to V1/V2/V3; the shortlist remains "additive-by-deletion" (αE CC#9:230). Each
candidate carries an explicit Pre-blocks line (αE A:99, B1:119, B2:148, B3:173, B4:202).
Cross-checking all 5 candidates + the still-open S1–S13 (αD §5) against the six pre-block
families re-confirms the clean result. Re-verified LIVE:

- **AZ-IV eager-value-tree:** SYNTHESIS:254 keeps G4's shared trait LAZY over the tape (no
  eager value tree); `json_rich_navigation_preserved` (SYNTHESIS:260) closes the LCD-flatten
  seam at ≥2 impls. αC §2.1 keys the re-open test to the THREE new surfaces. **No re-open.**
- **StructRegistry / per-leaf indirection:** SYNTHESIS:254/260 + αC §2.2 keep the PERMANENT
  pre-block on per-leaf indirection; the no-second-substrate clause REJECTs an introduced
  `StructLayout`/`TapeStructBuilder` alongside the landed `Tape`/`ValueRef`. **No re-open.**
- **CSS fact-stream String-as-output:** RETIRED at HEAD (`emit_fact_stream`=0). αC §0.B narrows
  the pre-block to its residual (`CSS_GENERATED_RS` + `RuntimeEmitterKind::RequestFacts`) — the
  correct refinement, not a dead-route re-fight. **No re-open.**
- **24-row broadcast:** SYNTHESIS:269 names it the pre-blocked route NOT the `>`SOTA; αE B4:202
  binds the new Sheets cell-corpus to "cold per-parse, not 24-broadcast." **No re-open.**
- **FNV / fixture contrivances:** kept bench-only; αC §2.5 binds the NEW re-entry seam (the
  generator's tape pre-sizing) to derive from `input.len()`/`BackendRule` grammar-generally.
  P5 (the `parse_w11_1_number` leak) is the live instance, purged. **No re-open.**
- **x86 / AVX / SVE:** SYNTHESIS:339-341 + HANDOFF:208-210 keep aarch64-only; P1 enforces by
  **crate-wide** deletion. αC §2.6 keys the re-open test to G5/G6 (the only acceleration waves)
  and binds the G6 ASM backlog to aarch64-only NEON. **No architectural re-open — BUT see the
  enforcement-gate defect in Axis-1-α below.**

The "no second substrate" Lock-1 guard (the subtlest CH3 re-entry for G3/G4) is held in
SYNTHESIS:254/260/124, HANDOFF, αC §3, αD, αE B2:148/B3:173.

**The single most important CH3 distinction — correctly held:** the SK-V17 residual
**REDRESS-W2-1 single-emitter** is the SK-V18 G3 **SUBJECT**, "admitted to be discharged here —
NOT re-opens" (HANDOFF:226-227; SYNTHESIS:360-361; αE B1:119). This is the binding-principle
backtrack — the inflection — not a regression. **Verdict: Axis 1 architecturally HELD.**

#### Axis-1-α — the x86 pre-block ENFORCEMENT GATE is false-green in αE (and understated in αA)

Holding the *architecture* is necessary but not sufficient: CH3 must also confirm the
machine-checkable **close gate** that enforces a PERMANENT pre-block actually catches a
survival. Here it does NOT, in αE:

- **αE P1 row** (`alphaE-candidate-shortlist.md:83`) scopes the x86 deletion to
  `skinny/crates/bbnf-simd/src/x86_64/` ONLY (−847 LOC), with **no mention** of `ext/x86/`,
  `build.rs`, or the `nasm-rs` dep.
- **αE P1 exit gate** (`alphaE-candidate-shortlist.md:93`):
  `grep -rE '_mm(256|512)?_|x86_64|avx|gfni|sve' skinny/crates/bbnf-simd/src` → 0;
  `find crates/bbnf-simd/src/x86_64 -type f` → 0. **This gate is `src/`-scoped.** `ext/x86/`
  is a SIBLING of `src/`; `build.rs` and `Cargo.toml` are at the crate ROOT. **Both escape the
  grep entirely.** This is the EXACT failure SYNTHESIS:66-67 names: "The old P1 verify grep was
  scoped to `…/src/`, so `ext/x86/` … and `build.rs` … escaped it — 'x86 gone' was literally
  false while ~3000 LOC of x86 survived." A green αE:93 gate would assert the x86 pre-block
  discharged while **3554 + 102 LOC of x86 ASM + nasm driver survive** — verified LIVE at HEAD
  this cycle (`find …/ext/x86 -type f \| xargs wc -l \| tail -1` = 3554; `wc -l build.rs` =
  102). This is a **P4-class false-green close gate**, the precise pathology SK-V18's own P4
  exists to eliminate, sitting in the contract's own enforcement surface.
- **αE summary table** (`:210`) and **net-LOC** (`:216`, −9250) likewise omit the ~3656 LOC of
  the second x86 surface, understating the prune by accounting and — more importantly — leaving
  the false-green gate uncorrected in the load-bearing shortlist artefact.
- **αA** (`alphaA-results-extraction.md:26-37,67`) carries the same stale census: x86 = "24
  files (847 LOC)" with the gate `find …/x86_64 -type f = 0`, **silent on `ext/x86/` (3554
  LOC), `build.rs`, and `nasm-rs`.** αA is the results/census artefact, not gate-bearing, so
  the impact is lower — but it *understates the very x86 surface it is tasked to extract* and
  is internally inconsistent with αC/SYNTHESIS/HANDOFF in the same V4 cycle.

This is an **orphaned-fold defect**, not an architectural re-open: FOLD-1 (αC `:31-91`) correctly
widens P1 to delete the whole x86 surface and moves the close gate to crate-wide
(`grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` over `src/`+`ext/`+`build.rs`+
`Cargo.toml`+`tests/`, αC `:180-186`; mirrored in SYNTHESIS:246/491 + HANDOFF:99-102/315-316).
But the fold was NOT carried into αE or αA — so two of the seven reviewed artefacts still encode
the false-green `src/`-scoped gate. Per the V4 dispatch's own framing ("prune does not strand
`>`SOTA" + "no wave re-opens … x86"), and per `ORCHESTRATOR §3Z` zero-orphan-REVISE, this must
be folded into αE and αA so the contract is internally consistent and the x86 close gate is
honest in EVERY artefact a reader would consult for the P1 exit condition.

**Verdict: Axis 1 architecture HELD; the x86 enforcement gate is FALSE-GREEN in αE + understated
in αA (REVISE both — concrete fix below).**

### Axis 2 — PRUNE-before-GENERALIZE

**Finding: HELD.** The sequencing A → B1 → B2 → B3 → B4 with P4 (Lock-14 gate) landing BEFORE
the B1/G2/G3 emitter rebuild is binding across SYNTHESIS:105/535-540 + HANDOFF:91/285/293 +
αE §0 ordering rule (`:65`) + CC#1 (`:222`) "A green Lock-14 gate (P4) must land BEFORE B1, so
the un-forked emitter is actually scanned for neutrality as it is built." The
exit-gate-blocks-successor dependency is carried into S-P3 (SYNTHESIS:573-576 "S-P3's revert
protocol MUST encode the entry-gate chain"; HANDOFF:335-337; αE CC#1, the CH6 §5 fold). The
P1/P4 same-commit coupling subtlety (the x86-tag removal at `lock14_baseline.rs:2463` must land
in the SAME commit as the `accepts_current_allowlist` assertion drop) is folded into αC
`:169-172`. **Note:** the FOLD-1 widening does NOT disturb sequencing — P1 remains the first
prune; the second x86 surface is additional deletion *within* P1, not a new wave. **Verdict:
Axis 2 HELD.**

### Axis 3 — prune does not strand `>`SOTA

**Finding: HELD.** No deletion removes `>`SOTA-bearing code:

- **P1 (x86, both surfaces):** `src/x86_64/` = 0 real intrinsics, 14 `unimplemented!` (αC:141);
  `ext/x86/` is vendored x264/FFmpeg ASM **dormant on aarch64** (`build.rs:38-40` early-returns
  on non-`x86_64`; `grep -rln 'ext/x86…' src/aarch64/ src/scalar/` = scalar-ref doc-string only,
  re-run this cycle, αC:158-160). Zero admission weight on aarch64. Deleting the whole x86
  surface strands NO `>`SOTA — it removes dead code AND makes the "x86 gone" claim true.
- **P2 (old CSS bench):** the headline came from `css_canon_bench` (KEPT), NOT
  `nonjson_css_l4.rs measure_mbps` (SYNTHESIS:247; αC §1-P2). αE A:99 + B2:132 + CC#3:224
  explicitly forbid deleting `css_canon_bench.rs`/`w2_rich_cssom_bench.rs` + the 9-field
  `assert_rich_strict_equality` oracle ("the ONE honest artefact KEPT from the old file").
- **P3 (replicas):** md5 = 1 over 7 dirs (re-verified) — collapsing 6 of 7 strands no unique
  capability; the distinct-grammar-output gate is bound to *provenance* (distinct `.bbnf` +
  `(source_roots,entry_rule)`), not cosmetics.
- **P5 (metalang):** symbol-name purge only; function bodies stay (αC §1-P5).

The standing-order invariant — "a derived parser that loses the speed is not done — surface
honestly as a named validated grammar-parameterized primitive, do NOT paper-close" — is GATED,
not bare: the honest-finding escape requires the primitive be `.bbnf`-INVOKED by name +
parameterized by grammar-derived DATA + carry a checkasm/scalar reference (SYNTHESIS:262
"(a)-(c) … A primitive failing (a)-(c) is a relabeled hand-written blob — REJECT"; HANDOFF §6;
αE CC#2:223). This converts the single largest paper-close surface in a generalization cycle
into a checked condition. **Verdict: Axis 3 HELD; the prune cluster strands nothing; the
generalize clusters carry a GATED honest-finding escape.**

---

## V3→V4 fold verification (orphan + regression check)

| Fold | What it changes | CH3 impact |
|---|---|---|
| **FOLD-1** (αC:31-91; CH5 V3 §C.5) | P1 widened to the SECOND x86 surface (`ext/x86/` 3554 LOC + `build.rs` nasm driver + `Cargo.toml` nasm-rs + `lib.rs:247` ref); close gate moved `src/`-scoped → crate-wide | **STRENGTHENS** the x86 pre-block enforcement — closes a real survival hole that falsified "x86 gone." Landed correctly in αC/SYNTHESIS/HANDOFF. **DEFECT: NOT propagated into αE or αA — those two retain the false-green `src/`-scoped gate (Axis-1-α). REVISE.** |
| **FOLD-2** (αC:93-102; CH2 V3 §8.1) | relocated-seam machine-check re-attributed from the arm-census grep (syntactically incapable on a neutral-identifier DATA table) to the **P3 collapse row-count check** | **STRENGTHENS** the G3 relocated-overfit-seam guard. Fully + consistently propagated (αE `:18,61,116,145,196,226`; αC §2.2; SYNTHESIS:253(iii); HANDOFF Inv-5). Both F13 disk claims reproduced LIVE this cycle. **No re-open; orphan-free.** |
| **F13** (αE:18) = αE-side of FOLD-2 | xtask-grep reach claim re-scoped; relocated-seam bound to `sort -u` row-count | Pure strengthening; no candidate; no re-open. |
| **F14** (αE:19) | αD-only stale checkasm "18" → disk-true 12+2; αE confirmed count-correct in four places | Accuracy only; α-E does NOT inherit the stale count. No CH3 impact. |

FOLD-2/F13/F14 are clean. **FOLD-1 is the orphan:** a fold that lands in three artefacts and not
the two that carry the P1 close gate / census produces a contract whose own x86 enforcement gate
is false-green in the candidate shortlist — the load-bearing CH3 artefact (`PASS-ALPHA §3`:
"cross-check the shortlist against entries 1-N"). This is a regression-lens defect: the x86
PERMANENT pre-block is not actually enforced by the αE gate a reader would run.

---

## Per-section dispositions (V4)

### alphaA-results-extraction.md — **REVISE**
CH3-relevant content mostly re-verified: the retired 24-row broadcast is named a pre-blocked
route NOT the `>`SOTA; the pre-block list is verbatim from the seed; PRUNE close conditions
preserve the headline harness. **But the x86 census is STALE post-FOLD-1:** αA:26-37,67 names
only `src/x86_64/` (24 files / 847 LOC) and the gate `find …/x86_64 -type f = 0`, **silent on
`ext/x86/` (3554 LOC, verified LIVE), `build.rs` (nasm driver), and the `nasm-rs` dep** that
αC/SYNTHESIS/HANDOFF carry in the same V4 cycle. As the results-extraction artefact tasked with
the x86 overfit surface (αA:7), it understates that surface by ~3656 LOC and is internally
inconsistent with the rest of the cohort.
**Concrete fix:** add the second x86 surface to αA's x86 census row (`:26-37` and the
`aarch64-only` ledger row `:67`): `ext/x86/` = 3554 LOC vendored ASM, `build.rs` = 102 LOC
nasm-rs driver, `Cargo.toml` `nasm-rs="0.3"`, `lib.rs:247` contract ref; restate the prune
gate as crate-wide (`find …/src/x86_64 …/ext/x86 -type f` = 0 AND
`grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` = aarch64-neutral only), mirroring
αC:180-186. **REVISE.**

### alphaB-competitor-deltas.md — **ACCEPT**
The CSS bar is framed ASYMMETRIC lazy-vs-eager (discharging the timed-plane-symmetry pre-block
honesty); asmjson AVX-512 held permanently OUT (aarch64 mandate); JSON comparators strict, cold,
no-broadcast. The yyjson/asmjson/RapidJSON honest-`None`-on-aarch64 (FFI not wired) discipline is
reflected in αE CC#4:225 — a fabricated competitor column is REJECTed, a strengthening of the
no-more-work-competitor pre-block. No x86-surface dependency. **ACCEPT.**

### alphaC-redress-digest.md — **ACCEPT** (the load-bearing CH3 artefact, V4-correct)
This is the artefact CH3 most directly reviews, and it is the one that **correctly authored
FOLD-1**. §1-P1 (`:135-186`) names BOTH x86 surfaces with LIVE-verified figures (`src/x86_64/`
847 + `ext/x86/` 3554 + `build.rs` 102 + nasm dep + `lib.rs:247` ref), restates the obligation
as deleting the entire x86 surface, and — critically — moves the P1 **close gate to crate-wide**
(`:180-186`: `find …/src/x86_64 …/ext/x86 -type f` = 0 AND `grep -riE 'avx|gfni|sve|x86|nasm'
skinny/crates/bbnf-simd/` aarch64-neutral only). §2.1–§2.6 enumerate exactly the six pre-block
families, each PERMANENT-vs-ADMIT classified, with re-open tests keyed to the three new SK-V18
surfaces. FOLD-2 (`:93-102`) correctly re-attributes the relocated-seam to the P3 row-count check
(both disk claims reproduced this cycle). The P1/P4 same-commit coupling (`:169-172`) is held.
Zero orphan REVISE entering V4 on αC. **ACCEPT.**

### alphaD-validated-invalidated.md — **ACCEPT**
§5 PRE-BLOCKED asserts NONE of S1–S13 re-opens any of the six families with the new-surface
binding (G4 over EXISTING lazy `ValueRef`; G2 toward lowering NOT fact-stream; P5 a symbol-name
purge; P1 DELETES x86). §1 VALIDATED marks `css_canon_bench`/substrate (Lock 1)/the two `>`SOTA
proofs as "do NOT re-prove; preserve" — the prune-does-not-strand guard. S12 names
`google-sheets.bbnf` as a real Pratt grammar (adopt-not-stub). αD is the *ledger* artefact, not
the x86-census artefact (that is αA), so the FOLD-1 second-surface absence is not a defect here —
its x86 entry correctly points to P1 as the deletion enforcer without re-citing the LOC census.
**ACCEPT.**

### alphaE-candidate-shortlist.md — **REVISE** (the false-green x86 gate)
Re-verified against `PASS-ALPHA §3` "cross-check against entries 1-N": candidate count = 5
(unchanged — no new candidate, no regression vector); each carries an explicit Pre-blocks line;
sequencing (§0:65, CC#1:222) enforces PRUNE-before-GENERALIZE with P4 before B1; the honest-
finding escape (CC#2:223) requires `.bbnf`-invocation + parameterization + checkasm/scalar
reference; B4 binds the same-wave-consumer rule + acceleration-at-admission with the G6 NEON-body
count BOUNDED. FOLD-2/F13 is handled correctly in six places. **The one defect: αE did NOT
absorb FOLD-1.** The P1 row (`:83`, −847 LOC, `src/x86_64/`-only) and the P1 **exit gate**
(`:93`: `grep -rE '…' skinny/crates/bbnf-simd/src` → 0) are **`src/`-scoped** — the exact
false-green SYNTHESIS:66-67 names, which lets `ext/x86/` (3554 LOC, verified LIVE) + `build.rs`
(nasm driver) + the `nasm-rs` dep survive while the gate reads 0 and asserts "x86 gone." This is
a **P4-class false-green close gate in the load-bearing shortlist**, leaving the x86/AVX/SVE
PERMANENT pre-block (αC §2.6) un-enforced by the αE gate a reader would run, and falsifying the
R10 "x86 gone" binding pin by ~3656 LOC. The summary table (`:210`) and net-LOC (`:216`) also
omit the second surface.
**Concrete fix (fold FOLD-1 into αE, mirroring αC:135-186 / SYNTHESIS:246):**
- `:83` P1 row — widen owner-paths to `src/x86_64/` AND `ext/x86/` (3554 LOC) AND `build.rs`
  (delete-or-neutralize the nasm driver) AND `Cargo.toml` (drop `build="build.rs"` + `nasm-rs`)
  AND `lib.rs:247` (re-home the `ext/x86/bbnf.asm` contract ref); restate the LOC as ≈ −4500
  across the x86 surface.
- `:93` P1 exit — replace the `src/`-scoped grep with the **crate-wide** gate:
  `find skinny/crates/bbnf-simd/src/x86_64 skinny/crates/bbnf-simd/ext/x86 -type f` = 0 AND
  `grep -riE 'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/` returns only aarch64-neutral
  comments (covers `src/`+`ext/`+`build.rs`+`Cargo.toml`+`tests/`), per CH5 V3 §C.5.
- `:210` summary table + `:216` net-LOC — restate the A-cluster prune to include the ~3656 LOC
  of the second x86 surface (net moves from ≈ −9250 toward ≈ −12900).
**REVISE.**

### SYNTHESIS.md (the §0 contract / goalset, standing for alphaF) — **ACCEPT**
§0.4 Pre-blocks carries all six families verbatim + the verbatim-blob / phantom-generic /
distinct-grammar-output re-entries + the inherited REDRESS family ids + the no-second-substrate
Lock-1 clause (`:260`). **FOLD-1 is correctly absorbed:** SYNTHESIS:60-75 + :162-167 name the
second x86 surface; the P1 close row (`:246`) deletes the WHOLE x86 surface (`src/x86_64/` AND
`ext/x86/` AND `build.rs` AND re-homes `lib.rs:247`) with the explicit crate-wide-NOT-`src/`-
scoped verify; the `x86_tree_deleted` telemetry column (`:491`) encodes the crate-wide scope.
FOLD-2 is handled at G3 verify (`:253(iii)`). REDRESS-W2-1 is correctly the G3 SUBJECT (`:360`).
G4 names the `G`-phantom-vs-real-`K` axis with DELETE-default (`:254`). The honest-finding escape
is GATED (`:262`). No goalset gate re-opens a pre-block; PRUNE-first is binding (`:535-540`).
**ACCEPT.**

### HANDOFF.md — **ACCEPT**
Pre-Blocked Routes (`:208-210`) carries the six families + nasm/`ext/x86/` explicitly; **FOLD-1
correctly absorbed** (`:13-15,71-72,99-102,242-244,315-316`: the SECOND x86 surface named, P1
deletes crate-wide, `x86_tree_deleted` telemetry crate-wide-scoped). Invariant 5 binds the
canonical three-surface Lock-14 model + the arm-census over the full alphabet across codegen AND
xtask + the type census + the F13 relocated-seam P3-row-count attribution. The SK-V17 residual
REDRESS-W2-1 single-emitter is correctly the SK-V18 G3 SUBJECT, "admitted to be discharged here —
NOT re-opens" (`:226-227`). Next-Move sequences PRUNE→GENERALIZE→PROVE with P4-before-emitter;
the revert dependency graph (`:335-337`) encodes the entry-gate chain. **ACCEPT.**

---

## Summary

Five of seven reviewed sections (αB, αC, αD, SYNTHESIS §0, HANDOFF) pass the CH3 lens at V4.
**Two — αA and αE — REVISE** on a single, concrete, orphan-fold defect: the V4 cycle's FOLD-1
(αC `:31-91`, from CH5 V3 §C.5) discovered and verified a **SECOND x86 surface** (`ext/x86/`
3554 LOC vendored ASM + `build.rs` nasm-rs driver 102 LOC + `Cargo.toml` `nasm-rs` dep +
`lib.rs:247` contract ref, ALL live at HEAD `318d9c046`), correctly widened P1's deletion scope,
and moved the P1 close gate from `src/`-scoped to **crate-wide** — but propagated this fold ONLY
into αC / SYNTHESIS / HANDOFF, leaving αA's x86 census and **αE's P1 exit gate (`:93`)**
`src/`-scoped. The αE gate is a **P4-class false-green**: it reads 0 while ~3656 LOC of x86
survives, falsely discharging the x86/AVX/SVE PERMANENT pre-block (αC §2.6) and falsifying the
R10 "x86 gone" binding pin in the load-bearing candidate shortlist. The fix is mechanical and
already authored in αC:135-186 / SYNTHESIS:246 — propagate the crate-wide P1 scope + close gate
into αE:83/93/210/216 and αA:26-37/67.

All three CH3 axes are otherwise HELD: (1) no candidate re-opens any of the six pre-block
families architecturally (AZ-IV / StructRegistry / fact-stream / 24-broadcast / FNV / x86),
candidate count unchanged at 5, REDRESS-W2-1 correctly framed as the G3 SUBJECT not a re-open —
**but the x86 enforcement gate must be made crate-wide in αE/αA so the pre-block is actually
enforced, not merely asserted**; (2) PRUNE-before-GENERALIZE binding with P4-before-B1 and the
P1/P4 same-commit coupling (FOLD-1 adds deletion within P1, does not disturb sequencing); (3) the
prune cluster strands no `>`SOTA (headline `css_canon_bench`/`w2_rich_cssom_bench` KEPT and
explicitly protected; both x86 surfaces carry zero aarch64 admission weight — `ext/x86/` dormant,
`src/x86_64/` 0 intrinsics/14 `unimplemented!`), and the generalize clusters carry a GATED
honest-finding escape. FOLD-2/F13/F14 are clean, orphan-free, and reproduced LIVE this cycle.

**Convergence posture:** 5 ACCEPT / 2 REVISE / 0 REJECT on the CH3 axis. Below the 95% ACCEPT
threshold (5/7 = 71%) at V4 on a single propagation defect with a concrete, already-authored fix.
The two REVISEs are NOT orphans — each carries the exact fold mechanism (crate-wide P1 scope +
close gate, mirroring αC:135-186). Once αE:83/93/210/216 + αA:26-37/67 absorb FOLD-1, CH3
converges to 7 ACCEPT. No architectural re-open; no new candidate; no stranded `>`SOTA.

TALLY accept=5 revise=2 reject=0
