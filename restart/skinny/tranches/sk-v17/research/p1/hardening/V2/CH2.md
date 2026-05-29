# SK-V17 S-P1 CHALLENGE — CH2 GENERALITY (V2)

Lens: CH2 GENERALITY. Pass: S-P1 Profile. Cycle: V2. Date: 2026-05-29.
Reviewer scope: PASS-1-PROFILE §3 CH2 + ORCHESTRATOR §3W. Subject: do the six
P1 artefacts attribute the benched-CSS hot leaves to **grammar-neutral
primitives** (scanner / classifier / tape — Lock 14) rather than CSS-named code
paths? A hot leaf named for a CSS role whose underlying symbol is a generic
primitive is a Lock 14 mis-attribution → REVISE. The profile must name the
*primitive* so S-P2 can ask whether it generalises to CSS L4 / Sheets /
BBNF-self.
Artefacts reviewed: `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`,
`p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`,
`p1f-bench-canonical.md`.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## §0 — Method (what CH2 re-verified against source at HEAD `6496fecae`)

The CH2 verdict turns on (a) whether the cited CSS-named symbol is in fact a
generic primitive, (b) whether each artefact NAMES that primitive, and (c)
whether the four V1 REVISE folds landed. CH2 re-verified the source spine on the
benched skinny tree at HEAD `6496fecae` (git-confirmed):

- **`CssFullParser::find_component_delim`**
  (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:288-311`).
  Body re-read: `while pos < self.bytes.len() { let byte = self.bytes[pos]; if
  delimiters.contains(&byte) { return Ok(Some((byte,pos))) } pos = match byte {
  … } }` (`:293-309`). `delimiters` is a runtime `&[u8]` slice — the call sites
  supply `b";{}"` (`:151`), `b"{};"` (`:172,:211`), `b":{};"` (`:314`), `b";}"`
  (`:247`). This is a **byte-class-membership scan over a runtime byte set**:
  grammar-neutral by construction. The CSS-ness lives only in the
  caller-supplied delimiter slices and the bracket/quote/comment escape arms
  (`:299-306`).
- **`CssFullParser::consume_balanced_at`** (`generated.rs:320-340`). Body re-read:
  it is **byte-for-byte the same `while pos < len` + per-byte `match` inner
  loop** as `find_component_delim`, differing ONLY in the membership test
  (`byte == close` `:324` vs `delimiters.contains` `:295`) and recursing on
  open-brackets (`:327-329`). It is the SAME scan primitive reached recursively
  — one NEON target. This is the V1 cross-cutting fact, and the source confirms
  it.
- **`find_colon_before`** (`generated.rs:313-314`) is ALSO just
  `find_component_delim(start, b":{};")` — a third call of the same primitive
  with a different runtime delimiter slice. (Relevant to P1-D §2.5.)
- **The shared-kernel generality proof.** `skinny/crates/runtime/src/grammars/json/scan.rs:218-223`
  routes JSON's structural scan through
  `bbnf_simd::aarch64::classify_tbl4::classify_structural_terminator_block_from_table`
  over a lo6 class table — i.e. JSON's structural scan IS the
  `select_classifier`/`PrimitiveKernels` family
  (`bbnf-simd/src/dispatch.rs:42,50,58`: `byte_class_from_table_64` NEON+scalar,
  `lo6_table_admissible:101`). CSS's `find_component_delim` is a **scalar
  per-byte re-implementation of the same byte-class-membership primitive not yet
  routed through the shared kernel.** The two are one primitive at two
  activation levels (scalar vs NEON) — the decisive Lock-14 fact.
- **Dormancy proof (re-ran the cited `grep`).** `grep -rln
  "TapeBuilder|ValueRef|select_classifier|PrimitiveKernels|StructuralIndex"
  skinny/crates/runtime/src/grammars/css_l4_declaration_values/` returns
  **NOTHING** — the benched CSS grammar references ZERO tape/SIMD symbols. This
  empirically confirms P1-E §2.5 (`:258-261`) and P1-F §4.4 (`:479`): the shared
  tape and SIMD primitives are dormant-on-CSS / live-on-JSON.

All five source spine facts hold at HEAD. They are the spine of the dispositions
below.

## §0.1 — V1 REVISE fold verification (the load-bearing V2 check)

The V1 CH2 consolidation carried four REVISE items (V1 CH2 §3). CH2 V2 verified
each landed:

| V1 REVISE | Required fold | V2 status |
|---|---|---|
| **1. P1-B §4.3** annotate `push_ascii_lower_hex` as FNV-diagnostic (no CSS-semantic value) | append the FNV-diagnostic framing consistent with P1-A §4.3 / P1-C A3 | **FOLDED** — `p1b-samply-mode-2.md:169` ("**FNV/hex DIAGNOSTIC encoding, NOT CSS-semantic value** … consistent with P1-A §4.3, P1-C A3") + the dedicated §4.3 anomaly (`:329-341`: "FNV/hex DIAGNOSTIC encoder with NO CSS-semantic value … pure diagnostic tax, removed not deferred"). |
| **2. P1-C §2.4** re-class `find_component_delim` from "structural" to "scan" | class column = scan | **FOLDED** — `p1c-samply-mode-3.md:239` class column now reads **scan** (byte-class-membership inner loop); the explicit re-classification note (`:246-258`) cites the `delimiters.contains(&byte)` body and the `json/scan.rs:219` shared kernel. |
| **3. P1-E §2.5 roll-up** annotate `consume_balanced_at` as "structural recursion over the scan primitive … one NEON target" | roll-up note folding it into the same NEON target | **FOLDED** — `p1e-hot-leaf-attribution.md:248` roll-up class = **"structural-over-scan"** with "shares its byte-membership `while`+`match` inner loop :322-338≡:293-308; … folds into the SAME single NEON byte-class-scan target as the row above"; §2.3 (`:183`) carries the identical annotation. |
| **4. P1-C/P1-E converge to P1-F's both-as-scan read** so S-P2 inherits one primitive family | the two former "structural" artefacts converge on one-NEON-target | **FOLDED** — P1-C (`:255-258`) and P1-E (`:248`) both now state the balance leaf is structural recursion OVER the scan primitive and folds into ONE NEON target with `find_component_delim`. Convergent with P1-D (`:271` both-as-scan), P1-F (`:318,:323-325` both-as-scan/one-NEON-target). |

**All four V1 REVISE folded; zero orphans carried into V2.** The fold did not
introduce a counter-error (no artefact swung the class to a CSS-role name, and
none deleted the grammar-neutral primitive naming).

## §1 — Per-artefact section dispositions (V2)

### P1-A (`p1a-samply-mode-1.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Names the benched plane symbols precisely; no role-for-primitive conflation. |
| §2.1 / §2.1b throughput + instr/byte | ACCEPT | Plane-level cost density; grammar-neutral. The fact/full instr-ratio is a primitive-cost figure, not a CSS-role claim. |
| §2.2 track1_full hot-leaf table | ACCEPT | `find_component_delim` class = "scan (delimiter membership)"; `consume_balanced_at` = "scan (nesting balance)" (`:127`); `parse_block`/`parse_declaration` = structural (genuine dispatch). Primitive-class throughout. |
| §2.3 fact-stream table | ACCEPT | Alloc/string classes named to std/libsystem primitives (`RawVecInner::reserve`, `_platform_memmove`, libmalloc); `push_ascii_lower_hex` = string (FNV source-hash), not a CSS role. |
| §2.4 plane-shape note | ACCEPT | Names the absence of a rich-typed CSS parser without inventing a CSS-named primitive. |
| §4.2 NEON-leaf re-confirm | ACCEPT | **Reference CH2 attribution.** "grammar-neutral byte-set-membership scan over a runtime delimiter slice (CH2: it is the generic `find_ascii_set_member64` primitive shape, NOT a CSS-named path) — the candidate for `byte_class_index_64` / `to_bitmask64` via `bbnf-simd/src/dispatch.rs select_classifier`" (`:176`). Names the primitive AND the shared-kernel route. |
| §4.3 fact-stream allocation-bound | ACCEPT | `push_ascii_lower_hex` framed as "FNV-diagnostic primitive with NO CSS-semantic value … must not be carried into S-P2" (`:179`); alloc is std/libsystem, grammar-neutral. |
| §4.5 no-second-substrate | ACCEPT | Names the substrate-union intact (CH5-adjacent; CH2-neutral). |

P1-A: **8/8 ACCEPT.** Remains the reference artefact for primitive-naming discipline.

### P1-B (`p1b-samply-mode-2.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Plane symbols named precisely. |
| §2.2 fact-stream table | ACCEPT | Classes libsystem/std primitives; `push_ascii_lower_hex` now carries "**FNV/hex DIAGNOSTIC encoding, NOT CSS-semantic value** … consistent with P1-A §4.3, P1-C A3" (`:169`) — V1 REVISE 1 folded. |
| §2.3 recognition table | ACCEPT | `find_component_delim` = **scan** (`:208`); `consume_balanced_at` = **scan** (`:210`); explicit CH2 callout (`:222-230`): "this leaf is the byte-class-membership scan primitive … the SAME primitive JSON's structural scan runs through `select_classifier`/`PrimitiveKernels` (verified `json/scan.rs:219`)". Primitive + shared-kernel route named. |
| §3 delta | ACCEPT | Plane reconciliation; no role-for-primitive conflation. |
| §4.1 orthogonal-hot-leaves masking | ACCEPT | Names the alloc-floor vs scan primitive families distinctly. |
| §4.2 NEON re-confirm | ACCEPT | Names the kernel + JSON-shared-need; cites `S-P1-re-confirm-on-benched-path`. |
| §4.3 push_ascii_lower_hex | ACCEPT (V1 REVISE 1 folded) | Now a full dedicated anomaly: "FNV/hex DIAGNOSTIC encoder with NO CSS-semantic value … pure diagnostic tax, removed not deferred" (`:329-341`). The V1 risk (reader inferring hex-encode is grammar-bearing CSS work) is closed. |

P1-B: **7/7 ACCEPT** (was 5/6 ACCEPT + 1 REVISE in V1; the REVISE folded).

### P1-C (`p1c-samply-mode-3.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Names both planes as code paths over one grammar module. |
| §2.1 / §2.2 throughput | ACCEPT | No primitive mis-naming. |
| §2.3 fact-stream self-time | ACCEPT | Own-code leaves resolved to `emit_fact_stream`/`push_ascii_lower_hex` with string/tape/hash classes — primitive-level. |
| §2.4 emit_full_parse hot-leaf table + re-classification note | ACCEPT (V1 REVISE 2+4 folded) | `find_component_delim` class column now reads **scan** (`:239`, was "structural"); `consume_balanced_at` = "**structural recursion OVER the scan primitive** … shares the `find_component_delim` byte-membership inner loop — one NEON target" (`:241,:255-258`). The re-classification note (`:246-258`) cites the `delimiters.contains(&byte)` body and the `json/scan.rs:219` shared kernel. The dominant-leaf taxonomy now converges with the other five artefacts. |
| §4 A1 plane bifurcation | ACCEPT | "disjoint hot-leaf sets … `emit_fact_stream` = 64% syslib + String-build, ZERO `find_component_delim`; `emit_full_parse` = 95% … scan" (`:343-345`) — names primitive families correctly. |
| §4 A2 masking probe | ACCEPT | Frames materialization-vs-scan at the primitive level; lever-1/2 (tape) targets the §2.3 floor, NEON pre-scan targets the §2.4 scan leaf (`:348-349`). |
| §4 A5 no-NEON-yet | ACCEPT | Names `select_classifier` as the un-built union — correct primitive naming. |

P1-C: **7/7 ACCEPT** (was 6/7 ACCEPT + 1 REVISE in V1; the dominant-leaf re-class folded).

### P1-D (`p1d-pmu-cycles.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Plane symbols precise. |
| §2.1 / §2.2 throughput | ACCEPT | — |
| §2.4 hot-leaf table | ACCEPT | `find_component_delim`/`consume_balanced_at` both classed **scan** (`:270,:271`); per-line breakdown (`:282-298` `:295 slice::contains`, `:298 match`, `:327 match`) names the per-byte membership primitive directly. Strong. |
| §2.5 redundant overlapping re-scan | ACCEPT (CH2-positive) | The decl body is walked 2-3× by the SAME `find_component_delim` primitive (`:211 b"{};"`, `:314 b":{};"` via `find_colon_before`, `:247 b";}"`) — CH2 re-verified all three call sites are the same primitive with different runtime delimiter slices (`generated.rs:151,172,211,247,314`). The "tokenize-once over a NEON-produced structural index" remedy is named at the primitive/substrate level, not a CSS-specific dedup. The generality finding the others miss. |
| §4.2 NEON antecedent | ACCEPT | "grammar-neutral … `byte_class_index_64` movemask over the `select_classifier` kernel … CSS is the non-JSON exercise grammar for the kernel (SYNTHESIS `simd_non_json_exercise`)" (`:467-471`). Explicit Lock-14 framing. |
| §4.3 emit ~34% correction | ACCEPT | Grammar-neutral correction (String-emit is plane-dependent: <0.1% on recognition, 217-370 i/B on fact-stream). |
| §4.7 no-SIMD-on-CSS | ACCEPT (CH2-strong) | "the `bbnf-simd/src/dispatch.rs:42` kernels are JSON-wired only … `digit_mac.rs:27 parse_4_digits_dotprod` (udot orphan) is never reached … no `bbnf_simd` frame in 20,377 samples" (`:512-517`). Names the dormant shared kernel + the udot orphan precisely. |
| §3 c/B PMU posture | ACCEPT (CH2-neutral; CH1/CH4 own it) | The cyc/byte reconciliation is a measurement concern. |

P1-D: **8/8 ACCEPT.** Tied with P1-A as the strongest CH2 artefact; §2.5 adds the
cross-grammar generality finding.

### P1-E (`p1e-hot-leaf-attribution.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | — |
| §2.3 full_parse attribution | ACCEPT (V1 REVISE 3 folded) | `find_component_delim` = **scan** (`:181`); `consume_balanced_at` = "**structural-over-scan** … its inner loop `generated.rs:322-338` is byte-for-byte the same … as `find_component_delim` :293-308 … it is the SAME byte-class-membership scan inner loop reached recursively, and shares ONE NEON byte-class-scan target" (`:183`). Source-exact (CH2 re-read `:320-340` confirms byte-for-byte). |
| §2.5 roll-up (the inherited class table) | ACCEPT (V1 REVISE 3 folded) | `consume_balanced_at` roll-up class = "**structural-over-scan** … NOT a distinct leaf, it folds into the SAME single NEON byte-class-scan target as the row above" (`:248`). The synthesis table S-P2 inherits now reads ONE NEON target, not two. The V1 over-fragmentation risk is closed. |
| §2.5 negative attribution | ACCEPT (CH2-strong) | "**No `tape` leaf at all** … `Tape`/`ValueRef`/`TapeBuilder` appear nowhere … `select_classifier` likewise appears nowhere — zero SIMD on the CSS path" (`:258-261`). The dormant-shared-primitive evidence S-P2 needs. |
| §4.1 recognition masking | ACCEPT | Primitive-level. |
| §4.2 fact-stream floor | ACCEPT | "it is grammar-neutral (it is `String` `push_str` growth, not CSS-specific logic)" (`:308`). Explicit CH2 callout. |
| §4.3 NEON re-confirm | ACCEPT | Names `byte_class_index_64` + `select_classifier` route + tape-first ordering. |
| §4.4 no-digit/unicode antecedent | ACCEPT (CH2-strong) | "the udot/i8mm digit kernel (`digit_mac.rs:27`) has **no benched CSS antecedent** … S-P2 must … not inherit a CSS digit-kernel hypothesis from here" (`:329-337`). Blocks mis-generalising a JSON primitive onto CSS without a fresh antecedent — CH2 working protectively. |

P1-E: **7/7 ACCEPT** (was 6/7 ACCEPT + 1 REVISE in V1; the synthesis roll-up note folded).

### P1-F (`p1f-bench-canonical.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | — |
| §2.3 instr/byte ranking | ACCEPT | Plane-level cost density, grammar-neutral. |
| §2.3 full_parse attribution | ACCEPT | `find_component_delim` = **scan** (`:316`); `consume_balanced_at` = **scan** with "recursion OVER the same byte-membership inner loop … the two share ONE NEON target" (`:318,:324-325`). Internally consistent both-as-scan — the V1 convergence target. |
| §2.3 fact-stream attribution + push_ascii_lower_hex note | ACCEPT | Alloc/string primitives named to libsystem/std; `push_ascii_lower_hex` note: "carries NO CSS-semantic value … a String-tax leaf, not a primitive to port" (§4.6 note). |
| §2.3 lightningcss attribution | ACCEPT (CH2-positive) | Resolves the comparator to its OWN primitives (`cssparser::tokenizer::consume_name`, `parcel_selectors::parser::parse_selector`, `PropertyId::from_name_and_prefix`, `:370-378`) — keeps comparator symbols on a separate path; no Track-1 conflation (also serves CH5). |
| §4.3 NEON re-confirm | ACCEPT | "grammar-neutral byte-class delimiter scan (`delimiters.contains(&byte)` over `b";{}"`) — a `byte_class_index_64` / movemask structural pre-scan target … the SAME byte-class-membership primitive JSON runs through `select_classifier`/`PrimitiveKernels` (P1-E verified `json/scan.rs:219`)" (`:469-476,:323-333`). |
| §4.4 tape_activated=false | ACCEPT (CH2-strong) | The `grep` proof (`:479`) that CSS references ZERO tape symbols while JSON DOES — CH2 re-ran it and confirms NONE. Names the shared tape primitive as dormant-on-CSS precisely. |
| §4.6 full_parse wrapper | ACCEPT | The `css_canon_bench::track1_full_parse` wrapper is named "PURE measurement scaffold … NOT a retained/second pass" (`:317`) — correctly NOT presented as a CSS-role primitive; mapped to `emit_full_parse` `generated.rs:61` in prod. |

P1-F: **8/8 ACCEPT.** Strongest cross-substrate primitive hygiene; remains the
both-as-scan reference the other artefacts converged to.

## §2 — Cross-cutting CH2 finding (V2)

**The single substantive V1 CH2 gap is closed.** The V1 cross-cutting defect was
a class-taxonomy inconsistency on `consume_balanced_at` (P1-C/P1-E tagged it
"structural"; the other four treated it as the scan family), which risked S-P2
designing TWO NEON primitives (a scan kernel + a "structural" kernel) where ONE
`byte_class_index_64` + bracket-depth-tracking kernel serves both. V2 verifies
all six artefacts now converge:

- **Class column** for `find_component_delim` = **scan** in all six (P1-A `:125`,
  P1-B `:208`, P1-C `:239`, P1-D `:270`, P1-E `:181`, P1-F `:316`).
- **`consume_balanced_at`** is uniformly framed as the SAME scan primitive
  reached recursively — "scan (nesting balance)" (P1-A `:127`, P1-B `:210`,
  P1-D `:271`, P1-F `:318` as scan), or the more precise "structural-over-scan …
  folds into the SAME single NEON byte-class-scan target" (P1-C `:255-258`, P1-E
  `:183,:248`). Either framing reads as **one NEON target**, which is the Lock-14
  outcome CH2 demanded. CH2 re-read `generated.rs:320-340` and confirms the inner
  loop is byte-for-byte identical to `find_component_delim:293-309` — both
  framings are source-true.

S-P2 therefore inherits **one** byte-class-membership scan primitive (with
bracket-depth recursion), not two — the exact over-fragmentation V1 flagged is
prevented.

**No new CH2 defect surfaced.** Every artefact names the dominant CSS leaf's
underlying primitive (`byte_class_index_64` / `find_ascii_set_member64` /
`select_classifier` over `delimiters.contains`), every artefact names the alloc
floor as grammar-neutral std/libsystem `String` growth (not CSS logic), and the
dormancy evidence is now in four artefacts (P1-D §4.7, P1-E §2.5, P1-F §4.4, and
the §0 grep CH2 re-ran). No artefact presents a CSS-role symbol AS the primitive.
The former closest risk (`push_ascii_lower_hex` reading as CSS-semantic work) is
now uniformly annotated FNV-diagnostic across P1-A/P1-B/P1-C/P1-F.

A residual prose noun — P1-C `:70` describes `emit_full_parse` as the
"delimiter/balance **structural scanner**" — is descriptive of the plane (a
scanner that emits a structural summary), NOT a class-column mis-tag; its §2.4
class column correctly reads **scan**. This is below the cosmetic threshold and
not dispositioned REVISE.

## §3 — Disposition counts (V2)

Section-level dispositions across the six artefacts (45 sections reviewed):

| Disposition | Count |
|---|---:|
| ACCEPT | 45 |
| REVISE | 0 |
| REJECT | 0 |

(Section count differs from V1's 42 because V2 artefacts added/renumbered
sections — P1-A §2.1b instr/byte, P1-D §4.7, P1-F §4.6 — each reviewed on the
CH2 axis.)

**ACCEPT rate: 45/45 = 100%.** All four V1 REVISE folded with no orphan and no
counter-error introduced. Zero REJECT, zero open critical defect, zero
role-for-primitive mis-attribution.

REVISE list: **empty.**

## §4 — Verdict

CH2 GENERALITY (V2): the S-P1 V2 profile **grounds a grammar-neutral design with
no open generality defect.** The two dominant benched-CSS hot leaves
(`find_component_delim`, `consume_balanced_at`) resolve, source-confirmed at HEAD
`6496fecae`, to ONE byte-class-membership scan primitive
(`delimiters.contains(&byte)` over a runtime `&[u8]` slice; the balance leaf is
the byte-for-byte same inner loop reached recursively) — the same primitive JSON
already runs through the shared `select_classifier`/`PrimitiveKernels` NEON
kernels (`json/scan.rs:218-223`). Every artefact names that primitive
(`byte_class_index_64`) and its shared-kernel route; the four V1 REVISE folds
(P1-B FNV-diagnostic; P1-C re-class to scan; P1-E synthesis roll-up
one-NEON-target note; P1-C/P1-E convergence to both-as-scan) all landed. The
fact-stream allocation floor is attributed to grammar-neutral `String`/libsystem
growth, not CSS logic. The tape and SIMD primitives are attributed as shared
infrastructure dormant-on-CSS / live-on-JSON — the dormancy `grep` CH2 re-ran
returns NONE, confirming it empirically. S-P2 inherits one scan primitive family,
not two. **No REJECT, no REVISE. ACCEPT 100%.** CH2 clears the §3Z 95% bar; this
is the second consecutive cycle moving toward convergence (V1 90.5% → V2 100%).
