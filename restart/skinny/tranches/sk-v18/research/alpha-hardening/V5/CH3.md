# CH3 REGRESSION (V5) — SK-V18 Pass-Alpha hardening

**Lens:** CH3 Regression per `PASS-ALPHA §3` ("does any proposed intervention re-open a route in
REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly identified the pre-block
list?") + `ORCHESTRATOR §3W/§3Z`. Focus per the V5 dispatch: (1) no wave re-opens the REDRESS
pre-block list (AZ-IV / StructRegistry / fact-stream / 24-broadcast / FNV / x86-AVX-SVE);
(2) PRUNE-before-GENERALIZE; (3) prune does not strand `>`SOTA.

**Subject under review (V5):** `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..E}.md`
+ `SYNTHESIS.md` + `HANDOFF.md`. **There is no `alphaF-*.md`** — per `PASS-ALPHA §2/§6` the α-F
deliverable IS `SYNTHESIS.md` + `HANDOFF.md`, both reviewed (the V1–V4 CH3 cycles read the
contract identically).

**Host:** aarch64 Apple M-series ONLY (x86 OUT). **HEAD of record:** `318d9c046` (`git rev-parse
HEAD` confirmed LIVE this cycle; the entire `sk-v18/` alpha tree is untracked working-state).

**Method (V5 confirming cycle):** V4 CH3 returned **5A / 2R / 0R (71.4%)** — two REVISEs on a
single orphan-fold defect: the V3→V4 FOLD-1 (the SECOND x86 surface, `ext/x86/` 3554 LOC + nasm
`build.rs` + `Cargo.toml` dep + `lib.rs` decl/refs) landed in αC/SYNTHESIS/HANDOFF but was NOT
propagated into αE (P1 row + exit gate, the load-bearing `src/`-scoped false-green gate) or αA
(the x86 census). The V4 CONSOLIDATED routed both into the V5 redress (cluster 1 → αE F15 +
αA "V5 R-1"; cluster 2 → SYNTHESIS/HANDOFF reach-match; cluster 3 → projection-tuple F16). The V5
CH3 duty is NOT to trust the V4 log: (a) re-grep every pre-block ground-truth LIVE at HEAD,
independently; (b) confirm the two V4 CH3 REVISEs are actually discharged on disk in αE and αA
(not merely promised); (c) confirm the V5 folds introduced NO new candidate and NO re-open
vector; (d) confirm zero residual orphan. Every disposition cites `path:line`/SHA.

---

## Verification log (re-grepped LIVE at HEAD `318d9c046`, V5 independent re-run)

| Pre-block / claim | Command | Result | Artefact agreement |
|---|---|---|---|
| HEAD = `318d9c046` | `git rev-parse HEAD` | `318d9c0469…` | αA, αC, V4/CH3 all ✓ |
| x86 `src/x86_64/` tree (P1 #1) | `find …/bbnf-simd/src/x86_64 -type f \| wc -l` | **24** | αE:94, αA:204, αC §2.6, SYNTHESIS:315, HANDOFF:104 ✓ |
| x86 `src/x86_64/` `.rs` LOC | `find …/x86_64 -name '*.rs' \| xargs wc -l \| tail -1` | **742** | αA:204 (742 `.rs`+105 `.asm`=847), αE:94 ✓ |
| **x86 `ext/x86/` tree (P1 #2, FOLD-1)** | `find …/bbnf-simd/ext/x86 -type f \| xargs wc -l \| tail -1` | **3554** (`bbnf.asm`/`x86util.asm`/`x86inc.asm`/`LICENSE-VENDOR`) | αE:94, αA:204, αC §2.6, SYNTHESIS:315, HANDOFF:104 ✓ — **NOW IN αE/αA (the V4 defect FIXED)** |
| **x86 `build.rs` nasm driver (FOLD-1)** | `wc -l …/bbnf-simd/build.rs` | **102** | αE:94, αA:204, SYNTHESIS:315, HANDOFF:105 ✓ |
| **`Cargo.toml` nasm dep + build (FOLD-1)** | `grep -nE 'nasm\|build = ' …/bbnf-simd/Cargo.toml` | `:8 build="build.rs"` · `:19 nasm-rs="0.3"` | αE:94, αA:204, SYNTHESIS:315(e), HANDOFF:107 ✓ |
| **`lib.rs` x86 decl + cfg + ref (FOLD-1, cluster-2 reach)** | `grep -nE 'ext/x86\|pub mod x86_64\|target_arch.*x86' …/bbnf-simd/src/lib.rs` | `:5 pub mod x86_64;` · `:247 ext/x86/bbnf.asm` · `:285 cfg(…x86_64…avx512bw)` | αE:94 (`:5`+`:285-288`+`:247`), SYNTHESIS:315(f), HANDOFF:108 ✓ |
| fact-stream RETIRED | `grep -c emit_fact_stream …css_l4_declaration_values/generated.rs` | **0** | αC §2.3, αD I-table, αE:110/130 ✓ |
| CSS const-`&str` courier (G2 / verbatim-blob) | `grep -n 'const CSS_GENERATED_RS' …codegen/src/runtime_generator.rs` | **`:701`** `&str = r#"` | SYNTHESIS, αC, αE:157, αD S7 ✓ |
| `RuntimeEmitterKind` fork (G3) | `grep -n 'enum RuntimeEmitterKind' …grammar_provider.rs` | **`:40`** | SYNTHESIS:322, HANDOFF:129, αE B1 ✓ |
| phantom `<G>` (G4) — full decl | `grep -n 'struct ValueRef' tape/mod.rs` | **`:175`** `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` | SYNTHESIS, αE B3, αC §2.2 ✓ |
| metalang leak (P5) | `grep -c parse_w11_1_number …json/generated.rs` | **7** | αE:94 (`:801,841,…`), αA, αC §1-P5 ✓ |
| 7 CSS replicas byte-identical (P3) | `find …css_l4_*/generated.rs \| xargs md5 \| awk '{print $NF}' \| sort -u \| wc -l` | **1** (over **7** dirs) | αE:94, αC §1-P3, αA ✓ |
| candidate count (no new candidate) | `grep -c '^### CANDIDATE' alphaE` | **5** (A, B1, B2, B3, B4) | αE:241 "still exactly 5", V1–V4 all ✓ |
| every candidate carries Pre-blocks | `grep -c '\*\*Pre-blocks' alphaE` | **5** (`:110,130,159,184,213`) | αE ✓ |

Every pre-block ground-truth resolves as stated at HEAD `318d9c046`. **Both x86 surfaces are LIVE
this cycle** (742 src + 3554 ext + 102 build.rs + `nasm-rs="0.3"` dep + `lib.rs:5 pub mod x86_64;`
+ `:285` cfg-arm + `:247` contract ref) — so the V4-discovered second surface is real, and the V5
gates must (and now do) cover it crate-wide.

---

## The V4→V5 fold verification — the two CH3 V4 REVISEs are DISCHARGED on disk

The V4 CH3 verdict was 5A/2R: αA + αE retained the `src/`-scoped false-green x86 P1 gate
(FOLD-1 not propagated). The V5 confirming duty is to confirm the redress LANDED — not promised.
Re-read both artefacts at the gate-bearing lines:

### REVISE-1 (αE) — DISCHARGED

- **αE P1 owner-path row** (`alphaE-candidate-shortlist.md:94`): now reads **"DELETE the WHOLE
  x86 surface crate-wide … BOTH surfaces [FOLD F8 + V5 R-1/CH6 V4 §1]"** and enumerates **(1)**
  `src/x86_64/` (742 `.rs`+105 `.asm`, 24 files, 14 `unimplemented!`) + `pub mod x86_64;`
  `lib.rs:5` + `#[cfg(target_arch="x86_64")]` arms `lib.rs:285-288` + the `lock14_baseline.rs:2463`
  `"diagnostic-x86"` allowlist entry; **(2)** `ext/x86/` (3554 LOC) AND `build.rs` (102 LOC nasm
  driver) AND `Cargo.toml:8,19` `nasm-rs`/`build` deps AND re-home `lib.rs:247`. LOC Δ corrected
  to **≈ −4500** (847+3554+102) `[FOLD F11 + V5 R-1]`.
- **αE P1 exit gate** (`alphaE-candidate-shortlist.md:104`): now **CRATE-WIDE, NOT `src/`-scoped
  (V5 R-1/CH6 V4 §1)** — `grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
  skinny/crates/bbnf-simd/` → only aarch64-neutral comments; `find …/src/x86_64 …/ext/x86 -type f`
  → 0; `build.rs`+`Cargo.toml` carry no active `nasm`/`x86` token. **The false-green is closed.**
- **αE LOC budget** (`:108`) net `≈ −10800` (P1 ≈ −4500 BOTH surfaces); **summary table** (`:221`)
  x86=0 crate-wide both `src/x86_64/` AND `ext/x86/` AND nasm `build.rs`/`Cargo.toml`; **net-LOC**
  (`:227`) `≈ −12850` disk-recomputed incl BOTH surfaces. All reach-consistent.
- **αE Pre-blocks** (`:213`, candidate B4): "x86/AVX/SVE/nasm (P1 crate-wide per V5 R-1/F15 —
  aarch64 ASM only)".
- **αE fold ledger F15** (`:18`) records the orphan + the verbatim fix; F16 (`:19`) records the
  projection-tuple widening; convergence note (`:241`) confirms "No candidate was added or
  removed (still exactly 5: A, B1–B4) … No re-opened REDRESS pre-block."

### REVISE-2 (αA) — DISCHARGED

- **αA V5 FOLD R-1 banner** (`alphaA-results-extraction.md:13-27`): explicitly names the V4 CH3 +
  CH7 BLOCKING REVISE, the second x86 surface (`ext/x86/` 3554 — `bbnf.asm` 485 / `x86util.asm`
  1036 / `x86inc.asm` 1978 / `LICENSE-VENDOR` 55; `build.rs` 102; `Cargo.toml:8,19`; `lib.rs:247`),
  and states αA "was the LONE cohort artefact to retain" the `src/`-scoped gate — now folded into
  §0 census x86 row, §3.2 x86 row, §5/§6 close-gate.
- **αA census x86 row** (`:204`) and **aarch64-only ledger row** (`:93`) now name BOTH surfaces
  with the crate-wide gate `find …/src/x86_64 …/ext/x86 -type f`=0 AND `grep -riE
  'avx|gfni|sve|x86|nasm' bbnf-simd/` neutral-only; **PRUNE close-condition 4** (`:292-296`)
  names the ENTIRE x86 surface and explicitly contrasts the OLD `src/`-scoped gate "which would
  PASS GREEN over the surviving second surface." All live-verified at HEAD in the artefact.

**Both V4 CH3 REVISEs are discharged with the exact mechanism the V4 disposition prescribed
(crate-wide P1 scope + crate-wide close gate, mirroring αC:135-186 / SYNTHESIS:315). Zero residual
orphan on the CH3 axis.** The cluster-2 reach-match (SYNTHESIS:315 (e) `nasm-rs` dep / (f)
`lib.rs:5`+`:285-288` cfg arms / (g) doc-scrub-or-`--include` scope; HANDOFF:101-110, inv.3
`:253-254`) and cluster-3 projection-tuple (SYNTHESIS G3 (iii) `:322`; HANDOFF inv.5 `:22-24`;
αE F16) are independently present and make the gate satisfiable-by-construction (the deletion list
is now reach-matched to the grep, eliminating the RED-by-construction mirror).

---

## The three CH3 axes — V5 global findings

### Axis 1 — does any proposed intervention re-open a REDRESS pre-block?

**Finding: NO architectural re-open; NO new candidate; the V5 folds widen enforcement, never
loosen it.** Candidate count = 5 (unchanged); each carries an explicit Pre-blocks line
(`:110,130,159,184,213`). Cross-checking all 5 candidates + the still-open S1–S13 (αD §5) against
the six pre-block families, re-verified LIVE at HEAD:

- **AZ-IV eager-value-tree (118×) → ADMIT-UNDER-DIFFERENT-FRAMING (αC §2.1):** G4's shared trait
  stays LAZY over the tape (αE B3 Pre-blocks `:184` "the trait stays lazy/tape-backed"; SYNTHESIS
  G4 keeps the LCD-flatten seam closed; HANDOFF `:204` keeps AZ-IV eager pre-blocked). αC §2.1
  keys the re-open test to G1 (the `json_sink_direct::render` projection must not materialize an
  eager value tree). **No re-open.**
- **StructRegistry / Arena<G> / Builder<G> per-leaf indirection → SPLIT (αC §2.2):** the
  PERMANENT face (§2.2a, any per-leaf registry/hashmap) is pre-blocked; the ADMIT face (§2.2b, the
  one-per-grammar layout description) is the substrate that already landed. αE B3 Pre-blocks
  `:184` "no StructRegistry per-leaf indirection"; HANDOFF `:207`. The §2.2 re-open surface is the
  G3 un-fork — bound to "no second substrate" + the relocated-seam structural check (cluster-3).
  **No re-open.**
- **CSS fact-stream String-as-output → PERMANENT (αC §2.3):** RETIRED at HEAD
  (`emit_fact_stream`=0). αC §2.3 narrows the pre-block to its residual (`CSS_GENERATED_RS` const
  courier + `RuntimeEmitterKind::RequestFacts`), the correct refinement; αE B1/B2 Pre-blocks
  (`:130,159`) forbid fact-stream output + const-string courier resurrection. **No re-open.**
- **24-row broadcast → PERMANENT (αC §2.4):** named the pre-blocked route NOT the `>`SOTA
  (HANDOFF `:215`); αE B4 Pre-blocks (`:213`) bind the NEW Sheets cell-corpus to "cold per-parse,
  not 24-broadcast." **No re-open.**
- **FNV / fixture contrivances → PERMANENT-as-runtime (αC §2.5):** kept bench-only; the NEW
  re-entry seam (the generator's tape pre-sizing + the shared value-API trait) is bound to derive
  from `input.len()`/`BackendRule` grammar-generally. P5 (`parse_w11_1_number` leak, 7× live) is
  the live instance, purged by symbol-name fix at the generator/template source. **No re-open.**
- **x86 / AVX / SVE → PERMANENT (αC §2.6):** P1 enforces by **crate-wide** deletion of BOTH
  surfaces; the §2.6 re-open test is now the crate-wide grep (`grep -riE 'avx|gfni|sve|x86|nasm'
  skinny/crates/bbnf-simd/`), explicitly "not a `src/`-scoped one — the V3 REVISE proved `ext/x86/`
  + `build.rs` are SIBLINGS of `src/`." The G6 ASM backlog is bound aarch64-only (PMULL/UDOT/TBX/
  CSSC). **No architectural re-open; and — the V4-defective enforcement gate is now crate-wide in
  EVERY artefact (αE/αA fixed; SYNTHESIS/HANDOFF/αC already crate-wide).**

The "no second substrate" Lock-1 guard (the subtlest CH3 re-entry for G3/G4) is held in αE B2/B3
Pre-blocks (`:159` "no second CSS tape — Lock 1"; `:184` "Lock 1 — one substrate"), αC §3,
SYNTHESIS, HANDOFF.

**The single most important CH3 distinction — correctly held:** the SK-V17 residual
**REDRESS-W2-1 single-emitter** is the SK-V18 G3 **SUBJECT**, "single-emitter lands here"
(HANDOFF:129/237; αE B1 `:130` "the SK-V17 REDRESS-W2-1 SUBJECT admitted to discharge — NOT a
re-open"). This is the binding-principle backtrack — the inflection — not a regression.
**Verdict: Axis 1 HELD architecturally AND at the enforcement gate (crate-wide in all seven
artefacts).**

### Axis 2 — PRUNE-before-GENERALIZE

**Finding: HELD.** The sequencing A → B1 → B2 → B3 → B4 with P4 (Lock-14 gate) landing BEFORE the
B1/G2/G3 emitter rebuild is binding across αE §0 ordering rule + CC#1 (`:222`) "A green Lock-14
gate (P4) must land BEFORE B1, so the un-forked emitter is actually scanned for neutrality as it
is built," SYNTHESIS PRUNE-first, HANDOFF Next-Move (PRUNE→GENERALIZE→PROVE, P4-before-emitter).
The P1/P4 same-commit coupling (the x86-tag removal at `lock14_baseline.rs:2463` lands in the SAME
commit as the gate assertion) is folded into αC + αE:94 (the P1 row names the
`lock14_baseline.rs:2463 "diagnostic-x86"` entry as part of the P1 deletion set). **The V5 FOLD-1
widening does NOT disturb sequencing** — P1 remains the first prune; the second x86 surface is
additional deletion *within* P1, not a new wave (αE F15 "The fix touches no other disposition —
sequencing, candidate count, >SOTA all unchanged"). The cluster-2 reach-match makes P1's gate
satisfiable-by-construction (deletion list = grep reach), so the mandatory lands-FIRST PRUNE gate
is not RED-by-construction. **Verdict: Axis 2 HELD.**

### Axis 3 — prune does not strand `>`SOTA

**Finding: HELD.** No deletion removes `>`SOTA-bearing code:

- **P1 (x86, BOTH surfaces):** `src/x86_64/` = 0 real intrinsics, 14 `unimplemented!`; `ext/x86/`
  is vendored x264/FFmpeg ASM **DORMANT on aarch64** (`build.rs:38-40` early-returns on
  non-`x86_64`; `lib.rs:247` is a doc-comment contract ref, not a call site; no aarch64 admission
  path — αE F15 "DORMANT, not load-bearing, REVISE-not-REJECT", αA `:204`/`:343`). Zero aarch64
  admission weight. Deleting the whole x86 surface strands NO `>`SOTA — it removes dead code AND
  makes the "x86 gone" R10 binding pin TRUE.
- **P2 (old CSS bench):** the headline came from `css_canon_bench` (KEPT), NOT
  `nonjson_css_l4.rs measure_mbps` (αD V6/I6; αE A:109 "No >SOTA-bearing code deleted … those came
  from `css_canon_bench`, which is KEPT"). αE explicitly forbids deleting
  `css_canon_bench.rs`/`w2_rich_cssom_bench.rs` + the 9-field `assert_rich_strict_equality` oracle
  (αE A Pre-blocks `:110` "P2 must not delete … the honest harness"; `:143` "the ONE honest
  artefact KEPT from the old file"; `:235` "do NOT prune"). αD V3/V6 mark the canonical harness
  "Do NOT re-prove; preserve."
- **P3 (replicas):** md5 = 1 over 7 dirs (re-verified LIVE) — collapsing 6 of 7 strands no unique
  capability; the distinct-grammar-output gate is bound to *provenance* (the per-grammar
  config-tuple-minus-`output_dir` collapse, F16), not cosmetics, so a real divergence cannot be
  silently deleted.
- **P5 (metalang):** symbol-name purge only at the generator/template source; function bodies stay
  (αE:94 "rename only").

The standing-order invariant — "a derived parser that loses the speed is not done — surface
honestly as a named validated grammar-parameterized primitive, do NOT paper-close" — is GATED, not
bare: the honest-finding escape requires the primitive be `.bbnf`-INVOKED by name + parameterized
by grammar-derived DATA + carry a checkasm/scalar reference (αE CC#2; HANDOFF §6; SYNTHESIS).
The PRESERVED->SOTA gate is pinned to the **N=200 `css_canon_bench` close-ledger per-row Mbps**
(αE F1 `:53`, gate `:145-154`), with the live N=80 reproduction held as cross-check-only so the
two sample planes are never mixed — a strengthening that prevents a silent >SOTA regression riding
a sample-size swap. **Verdict: Axis 3 HELD; the prune cluster strands nothing; the generalize
clusters carry a GATED honest-finding escape; the CSS >SOTA floor is sample-plane-disciplined.**

---

## V4→V5 fold verification (orphan + regression check)

| Fold | What it changes | CH3 impact |
|---|---|---|
| **F15** (αE:18; from V4 CH1 §αE + CH3, seeded CH5 V3 §C.5) | αE P1 row + exit gate + LOC + summary + net-LOC widened to BOTH x86 surfaces, crate-wide gate | **DISCHARGES** the V4 CH3 REVISE-1 (the false-green `src/`-scoped αE gate). Verified landed at αE:94/104/108/221/227. **No re-open; closes a real survival hole.** |
| **αA V5 R-1** (αA:13-27; from V4 CH3 + CH7 §1) | αA §0/§3.2/§5/§6/ledger-row widened to BOTH surfaces, crate-wide gate | **DISCHARGES** the V4 CH3 REVISE-2 (the stale αA census). Verified landed at αA:93/204/292-296/333. **No re-open.** |
| **F16 / cluster-3** (αE:19; from CH2 V4 §8.1) | relocated-seam structural check projected onto full config-tuple-minus-`output_dir` (was `(source_roots,entry_rule)`-only) | **STRENGTHENS** the G3 relocated-overfit-seam guard — catches a branch riding `fact_schema`/`output_plane`/`emitter`. Gate correctly RED pre-P3 (7 distinct `fact_schema` LIVE). Fully propagated (αE:145/196/226; SYNTHESIS G3 (iii); HANDOFF inv.5). **No re-open; orphan-free.** |
| **cluster-2** (SYNTHESIS:315 (e)-(g); HANDOFF:101-110) | P1 deletion-target list reach-matched to the crate-wide grep (`nasm-rs` dep + `lib.rs:5` decl + `:285-288` cfg arms + doc-scrub-or-`--include`) | **STRENGTHENS** — eliminates the RED-by-construction mirror; the lands-FIRST PRUNE gate is satisfiable-by-construction. **No re-open.** |
| **cluster-4** (SYNTHESIS V3→V4 ledger anchors) | self-citations switched to fold-stable section/column anchors | Documentation-accuracy only; machine-gate-unaffected. **No CH3 impact.** |

Every V5 fold is a **tightening** (more LOC deleted, more columns checked, gate satisfiable),
never a loosening. No fold adds or removes a candidate. No fold introduces a re-open vector.
**Zero residual orphan on the CH3 axis** — the two V4 CH3 REVISEs are both discharged on disk with
the prescribed mechanism.

---

## Per-section dispositions (V5)

### alphaA-results-extraction.md — **ACCEPT**
The V4 CH3 REVISE (stale `src/`-scoped x86 census) is **discharged**: the V5 R-1 fold
(`:13-27,93,204,292-296,333-343`) names BOTH x86 surfaces (`src/x86_64/` 847 + `ext/x86/` 3554 +
`build.rs` 102 + `Cargo.toml:8,19` + `lib.rs:247` ref) with the crate-wide close gate, all
live-verified at HEAD in the artefact, and explicitly contrasts the OLD `src/`-scoped gate "which
would PASS GREEN over the surviving second surface." The retired 24-row broadcast is named a
pre-blocked route NOT the `>`SOTA; the pre-block list is verbatim from the seed; PRUNE close
conditions preserve the headline harness (`css_canon_bench` KEPT). Internally consistent with
αC/SYNTHESIS/HANDOFF this cycle. **ACCEPT.**

### alphaB-competitor-deltas.md — **ACCEPT**
The CSS bar is framed ASYMMETRIC lazy-vs-eager (discharging the timed-plane-symmetry pre-block
honesty); asmjson AVX-512 held permanently OUT (aarch64 mandate); JSON comparators strict, cold,
no-broadcast. The yyjson/asmjson/RapidJSON honest-`None`-on-aarch64 discipline (a fabricated
competitor column is REJECTed) is a strengthening of the no-more-work-competitor pre-block. No
x86-surface dependency. **ACCEPT.**

### alphaC-redress-digest.md — **ACCEPT** (the load-bearing CH3 artefact)
This is the artefact CH3 most directly reviews, and it correctly authored FOLD-1 and carries it
into V5. §2.6 (`:554-602`) names BOTH x86 surfaces with LIVE-verified figures, restates the
obligation as deleting the entire x86 surface, and keys the re-open test to the **crate-wide** grep
explicitly "not a `src/`-scoped one." §2.1–§2.6 enumerate exactly the six pre-block families, each
PERMANENT-vs-ADMIT classified, with re-open tests keyed to the new SK-V18 surfaces (G1/G3/G4/G5/G6).
§3 holds the single load-bearing distinction (typed/rich/retained admit; eager/serialized/forked/
hand-written-verbatim pre-block). The P1/P4 same-commit coupling is held. Zero orphan REVISE
entering V5 on αC. **ACCEPT.**

### alphaD-validated-invalidated.md — **ACCEPT**
§5 PRE-BLOCKED asserts NONE of S1–S13 re-opens any of the six families with the new-surface binding
(G4 over EXISTING lazy `ValueRef`; G2 toward lowering NOT fact-stream; P5 a symbol-name purge; P1
DELETES x86 BOTH surfaces). §1 VALIDATED marks `css_canon_bench`/substrate (Lock 1)/the two
`>`SOTA proofs as "do NOT re-prove; preserve" (V2/V3/V6) — the prune-does-not-strand guard. I2/I6
correctly classify the JSON-template + dead-CSS-NEON overfits as the G1/G6 SUBJECTS (not re-opens).
αD is the *ledger* artefact, not the x86-census artefact (that is αA), so its x86 entry correctly
points to P1 as the deletion enforcer. **ACCEPT.**

### alphaE-candidate-shortlist.md — **ACCEPT** (the V4 REVISE discharged)
Re-verified against `PASS-ALPHA §3` "cross-check against entries 1-N": candidate count = 5
(unchanged — no new candidate, no regression vector); each carries an explicit Pre-blocks line;
sequencing (§0, CC#1) enforces PRUNE-before-GENERALIZE with P4 before B1; the honest-finding escape
(CC#2) requires `.bbnf`-invocation + parameterization + checkasm/scalar reference; B4 binds the
same-wave-consumer rule + acceleration-at-admission with the G6 NEON-body count BOUNDED. **The V4
CH3 REVISE is discharged via F15:** the P1 row (`:94`), P1 exit gate (`:104`), LOC budget (`:108`),
summary table (`:221`), and net-LOC (`:227`) are now **crate-wide** covering BOTH x86 surfaces +
`build.rs` + `Cargo.toml` nasm dep + `lib.rs:5`/`:285-288`/`:247` — the exact false-green
SYNTHESIS:315 names is closed. F16 (cluster-3) widens the relocated-seam projection at
`:145/:196/:226`. No re-opened REDRESS pre-block. **ACCEPT.**

### SYNTHESIS.md (the §0 contract / goalset, standing for alphaF) — **ACCEPT**
§0 Pre-blocks carries all six families verbatim + the verbatim-blob / phantom-generic /
distinct-grammar-output re-entries + the no-second-substrate Lock-1 clause. **FOLD-1 + cluster-2
correctly absorbed:** the P1 close row (`:315`) deletes the WHOLE x86 surface (a)-(g) crate-wide
with the deletion list reach-matched to the verify grep (`--include='*.rs' --include='Cargo.toml'`),
explicitly satisfiable-by-construction. G3 verify (`:322`) carries the cluster-3 widened projection
(per-`grammar_name` config-tuple-minus-`output_dir`, RED pre-P3). REDRESS-W2-1 is correctly the G3
SUBJECT. G4 names the `G`-phantom-vs-real-`K` axis with DELETE-default. The honest-finding escape
is GATED. No goalset gate re-opens a pre-block; PRUNE-first is binding. **ACCEPT.**

### HANDOFF.md — **ACCEPT**
Pre-Blocked Routes (`:204-221`) carries the six families + nasm/`ext/x86/`/`lib.rs` x86
module-cfg-arms explicitly. **FOLD-1 + cluster-2 correctly absorbed** (P1 `:101-110` crate-wide
deletion reach-matched to grep; invariant 3 `:253-254` "zero x86/AVX/SVE/nasm in `bbnf-simd`
CRATE-WIDE — `src/x86_64/` AND `ext/x86/` AND `build.rs` AND the `nasm-rs` Cargo.toml dep AND
`lib.rs`"). Cluster-3 projection at inv.5 (`:22-24`). The SK-V17 residual REDRESS-W2-1
single-emitter is correctly the G3 SUBJECT ("single-emitter lands here", `:129/:237`). Next-Move
sequences PRUNE→GENERALIZE→PROVE with P4-before-emitter; the revert dependency graph encodes the
entry-gate chain. **ACCEPT.**

---

## Summary

**All seven reviewed sections pass the CH3 lens at V5 (7 ACCEPT / 0 REVISE / 0 REJECT).** The two
V4 CH3 REVISEs — the x86 FOLD-1 orphan in αE (the false-green `src/`-scoped P1 exit gate) and the
stale x86 census in αA — are **discharged on disk** with the exact prescribed mechanism (crate-wide
P1 scope + crate-wide close gate covering BOTH x86 surfaces + `build.rs` + the `nasm-rs` dep +
`lib.rs` module-decl/cfg-arms/ref, mirroring αC §2.6 / SYNTHESIS:315). Verified live at HEAD
`318d9c046`: αE F15 lands at `:94/:104/:108/:221/:227`; αA V5 R-1 lands at `:13-27/:93/:204/
:292-296/:333-343`. The cluster-2 reach-match makes the lands-FIRST PRUNE gate
satisfiable-by-construction (deletion list = grep reach, no RED-by-construction mirror); the
cluster-3 projection-tuple (F16) deepens the relocated-seam structural check to the columns a
relocated branch can actually ride. Every V5 fold is a tightening, never a loosening; none adds or
removes a candidate.

All three CH3 axes are HELD: (1) no candidate re-opens any of the six pre-block families
architecturally (AZ-IV / StructRegistry / fact-stream / 24-broadcast / FNV / x86), candidate count
unchanged at 5, every candidate carries an explicit Pre-blocks line, REDRESS-W2-1 correctly framed
as the G3 SUBJECT not a re-open — **AND the x86 enforcement gate is now crate-wide in EVERY
artefact a reader would consult for the P1 exit condition (the V4 defect eliminated)**;
(2) PRUNE-before-GENERALIZE binding with P4-before-B1, the P1/P4 same-commit coupling, and the
FOLD-1 widening confined to deletion-within-P1 (sequencing undisturbed); (3) the prune cluster
strands no `>`SOTA (headline `css_canon_bench`/`w2_rich_cssom_bench` KEPT and explicitly protected;
both x86 surfaces carry zero aarch64 admission weight — `ext/x86/` dormant via `build.rs:38-40`
non-x86 early-return, `src/x86_64/` 0 intrinsics/14 `unimplemented!`), the generalize clusters
carry a GATED honest-finding escape, and the CSS >SOTA floor is N=200-sample-plane-disciplined.

**Convergence posture:** 7 ACCEPT / 0 REVISE / 0 REJECT = **100%** on the CH3 axis at V5 — above
the §3Z ≥95% bar, zero orphan REVISE, zero REJECT. The V4→V5 redress is complete and orphan-free on
this lens. No architectural re-open; no new candidate; no stranded `>`SOTA.

TALLY accept=7 revise=0 reject=0
