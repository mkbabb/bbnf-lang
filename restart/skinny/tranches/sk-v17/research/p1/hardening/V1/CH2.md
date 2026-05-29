# SK-V17 S-P1 CHALLENGE — CH2 GENERALITY (V1)

Lens: CH2 GENERALITY. Pass: S-P1 Profile. Cycle: V1. Date: 2026-05-29.
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

## §0 — Method (what CH2 verified against source)

The CH2 verdict turns on whether the cited symbol (a CSS-named method) is in
fact a generic primitive, and whether the artefact names that primitive. CH2
verified the two dominant leaves against the benched skinny tree at HEAD
`6496fecae`:

- `CssFullParser::find_component_delim`
  (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:288-311`).
  Body confirmed: `while pos < len { let byte = self.bytes[pos]; if
  delimiters.contains(&byte) { return … } pos = match byte { … } }`
  (`:293-309`). The `delimiters` argument is a runtime `&[u8]` slice
  (`b";{}"` / `b"{};"` / `b":{};"` / `b";}"` per call site `:211,:247,:314`).
  This is a **byte-class-membership scan over a runtime byte set** — grammar-
  neutral by construction; the CSS-ness lives only in the caller-supplied
  delimiter slice and the bracket/quote/comment escape arms (`:299-306`).
- `bbnf-simd/src/dispatch.rs`: `select_classifier:42`, `PrimitiveKernels`
  (`byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `:49-55`), `lo6_table_admissible:101`. These are
  the grammar-neutral kernels the artefacts name as the generalisation target.
- **The generality proof CH2 ran:** `skinny/crates/runtime/src/grammars/json/scan.rs`
  ALREADY routes through these exact kernels — `StructuralIndex`,
  `classify_block_from_table` (`scan.rs:2,219`), `prefix_xor_64` (`:203`),
  `eob_pad_clamp` (`:118`) — i.e. JSON's structural scan IS the
  `select_classifier`/`PrimitiveKernels` family, while CSS's `find_component_delim`
  is a **scalar per-byte re-implementation of the same byte-class-membership
  primitive that has not been routed through the shared kernel**. This is the
  decisive Lock-14 fact: the CSS hot leaf and the JSON structural scan are the
  same primitive at two activation levels (scalar vs NEON). Every artefact that
  names `find_component_delim`'s primitive as `byte_class_index_64` /
  `select_classifier` is therefore **correct** on the generality axis.

This source-confirmed fact is the spine of the CH2 dispositions below.

## §1 — Per-artefact section dispositions

### P1-A (`p1a-samply-mode-1.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Names the benched plane symbols precisely; no role-for-primitive conflation. |
| §2.2 hot-leaf table (`find_component_delim` 58-65%) | ACCEPT | Class column reads "scan (delimiter membership)" / "scan (driver + ws skip)" — primitive-class, not CSS-role. |
| §2.3 fact-stream table | ACCEPT | Alloc/string classes named to the std/libsystem primitive (`RawVecInner::reserve`, `_platform_memmove`), not a CSS role. |
| §4.2 "find_component_delim is the re-confirmed NEON leaf" | ACCEPT | **Model CH2 attribution.** Explicitly: "This is a grammar-neutral byte-set-membership scan over a runtime delimiter slice (CH2: it is the generic `find_ascii_set_member64` primitive shape, NOT a CSS-named path) — the candidate for `byte_class_index_64` / `to_bitmask64` via `bbnf-simd/src/dispatch.rs select_classifier`." Names the primitive AND the shared-kernel route. This is the bar the other artefacts are measured against. |
| §4.3 fact-stream allocation-bound | ACCEPT | `push_ascii_lower_hex` correctly framed as diagnostic-emission (FNV source-hash), a non-grammar artefact that vanishes with the tape; alloc is std/libsystem primitive, grammar-neutral. |
| §2.4 plane-shape note | ACCEPT | Names the absence of a rich-typed CSS parser without inventing a CSS-named primitive. |

P1-A is clean on CH2: **6/6 ACCEPT**. It is the reference artefact for the
primitive-naming discipline.

### P1-B (`p1b-samply-mode-2.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Plane symbols named precisely. |
| §2.2 fact-stream table | ACCEPT | Classes are libsystem/std primitives (`libsystem_kernel`, `libsystem_malloc`, `core::fmt::num`); `push_ascii_lower_hex` named to `Vec::with_capacity` primitive. |
| §2.3 recognition table | ACCEPT | **Explicit CH2 callout:** "This leaf is grammar-NEUTRAL (CH2): it is a byte-class delimiter scan, not a CSS-named role — the same primitive JSON's structural scan needs." Names the primitive (`byte_class_index_64`/`to_bitmask64`) and the JSON-shared-need. Correct. |
| §3 delta | ACCEPT | Plane reconciliation; no role-for-primitive conflation. |
| §4.1 orthogonal-hot-leaves masking | ACCEPT | Names the two primitive families (alloc-floor vs scan) distinctly. |
| §4.3 `push_ascii_lower_hex` per-token alloc | REVISE (minor) | The §4.3 framing ("the tape stores the lexeme as a `(start,end)` offset pair … decoding hex lazily via `ValueRef`") describes `ValueRef`/`push_plain_offset` as the substitute but does NOT state, as P1-A §4.2 does, that the hex-emission itself is a **diagnostic** primitive with no parse-semantic value (it is FNV/source-hash, not a CSS lexeme decode). CH2 risk: a reader could infer the hex-encode is grammar-bearing CSS work that the tape must reproduce. Fix: append to §4.3 "`push_ascii_lower_hex` is FNV-hash diagnostic emission (P1-A §4.3, P1-C A3), not a CSS-semantic decode; the tape retires it entirely rather than reproducing it." `p1b-samply-mode-2.md:265-272`. |

P1-B: **5/6 ACCEPT, 1 REVISE** (minor, cross-artefact-consistency).

### P1-C (`p1c-samply-mode-3.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Names both planes as code paths over one grammar module. |
| §2.1 / §2.2 throughput | ACCEPT | No primitive mis-naming. |
| §2.3 fact-stream self-time | ACCEPT | Own-code leaves resolved to `emit_fact_stream`/`push_ascii_lower_hex` with classes string/tape/hash — primitive-level. |
| §2.4 emit_full_parse self-time | REVISE | The class column tags `find_component_delim` and `consume_balanced_at` as **"structural"** (`:198,:200`), where P1-A/P1-B/P1-D/P1-E/P1-F tag `find_component_delim` as **"scan"**. The seven-class taxonomy (scan/number/string/unicode/structural/tape/dispatch) is the PASS §2.1 / CH1 vocabulary; CH2's concern is that "structural" is a grammar-shaped class (it implies block/rule structure — a CSS role) whereas "scan" is the grammar-neutral byte-class-membership primitive. Tagging the delimiter membership leaf "structural" weakens the Lock-14 read that it is the same primitive as JSON's structural-index scan. Fix: re-class `find_component_delim` as **scan** (consistent with the other five artefacts and with the source body `delimiters.contains(&byte)`); `consume_balanced_at` may stay structural (it is recursive bracket-balance, a genuinely structural recursion) but should carry a parenthetical "(byte-scan inner loop)". `p1c-samply-mode-3.md:198,200`. |
| §4 A1 plane bifurcation | ACCEPT | Names primitive families correctly. |
| §4 A2 masking probe | ACCEPT | Frames materialization-vs-scan at the primitive level. |
| §4 A5 no-NEON-yet | ACCEPT | "the structural scan is scalar byte-at-a-time" + names `select_classifier` as the un-built union — correct primitive naming. |

P1-C: **6/7 ACCEPT, 1 REVISE** (taxonomy-consistency on the dominant leaf class).

### P1-D (`p1d-pmu-cycles.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | Plane symbols precise. |
| §2.1 / §2.2 throughput | ACCEPT | — |
| §2.4 hot-leaf table | ACCEPT | `find_component_delim`/`consume_balanced_at` classed "scan"; line-level breakdown (`:295 slice::contains`, `:298 match`) names the per-byte membership primitive directly. Strong. |
| §2.5 "redundant overlapping re-scan" | ACCEPT (CH2-positive) | This is a genuinely grammar-neutral observation: each declaration body is walked 2-3× by the SAME `find_component_delim` primitive (`:211 b"{};"`, `:314 b":{};"`, `:247 b";}"`). The "single-pass tokenize-once over the structural index" framing is primitive-level (it is the structural-index primitive, not a CSS-specific dedup). Good generality finding. |
| §4.2 NEON antecedent | ACCEPT | "grammar-neutral byte-class delimiter scan … `byte_class_index_64` movemask over the `select_classifier` kernel … CSS is the non-JSON exercise grammar for the kernel." **Explicit Lock-14 framing** — names the kernel AND the cross-grammar exercise role (the `simd_non_json_exercise` lock). Correct. |
| §4.3 emit-~34% correction | ACCEPT | Grammar-neutral correction (String-emit is plane-dependent, not a fixed cost). |
| §3 c/B PMU gap | ACCEPT (CH2-neutral; CH1/CH4 own it) | The CPI-unreliability is a measurement concern, not a generality one. |

P1-D: **7/7 ACCEPT.** Tied with P1-A as the strongest CH2 artefact; §2.5 adds a
generality finding the others miss.

### P1-E (`p1e-hot-leaf-attribution.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | — |
| §2.3 full_parse attribution | REVISE | Same taxonomy issue as P1-C §2.4: `consume_balanced_at` is classed **"structural"** (`:161`) while `find_component_delim` is "scan" (`:159`). P1-E is the **synthesis** artefact (PASS §2 names it the per-row hot-leaf-class roll-up authority), so its class assignment is the one S-P2 inherits. The §2.5 roll-up table then propagates the split: `find_component_delim` = scan, `consume_balanced_at` = structural (`:221-222`). CH2 concern: `consume_balanced_at`'s body is the SAME per-byte `match` + recursion on the same byte set (`generated.rs:320-340`); classing it "structural" vs the delim leaf "scan" implies two different primitive families when they are one byte-scan primitive at two recursion depths. Fix: in §2.5 roll-up add a note that `consume_balanced_at` is "structural recursion over the **scan** primitive (shares `find_component_delim`'s byte-membership inner loop)" so S-P2 reads them as one NEON target, not two. `p1e-hot-leaf-attribution.md:161,222`. |
| §2.5 roll-up "no number/unicode/dispatch leaf is hot" | ACCEPT (CH2-strong) | The negative attribution is grammar-neutral and load-bearing: it states the udot/i8mm digit kernel (`digit_mac.rs:27`) has NO benched CSS antecedent and the tape/dispatch primitives are provably unwired (zero `Tape`/`ValueRef`/`select_classifier` samples). This is exactly the Lock-14 evidence S-P2 needs — it names which shared primitives are dormant on CSS, not just which fire. |
| §4.1 recognition masking | ACCEPT | Primitive-level. |
| §4.2 fact-stream floor 91% from emit_fact_stream | ACCEPT | "it is grammar-neutral (it is `String` `push_str` growth, not CSS-specific logic)" — explicit CH2 callout. Correct. |
| §4.3 NEON re-confirm | ACCEPT | Names `byte_class_index_64` + the `select_classifier` route + the tape-first ordering. |
| §4.4 no-digit/unicode antecedent | ACCEPT (CH2-strong) | Correctly blocks carrying a CSS digit-kernel hypothesis without a fresh typed-plane antecedent — prevents a JSON-primitive (the digit MAC) from being mis-generalised onto CSS without measurement. This is CH2 working in the protective direction. |

P1-E: **6/7 ACCEPT, 1 REVISE** (consume_balanced_at primitive-family note in
the synthesis roll-up — load-bearing because this is the inherited class table).

### P1-F (`p1f-bench-canonical.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1 method | ACCEPT | — |
| §2.1 / §2.2 throughput + instr/byte | ACCEPT | instr/byte ranking is plane-level (primitive cost density), grammar-neutral. |
| §2.3 full_parse attribution | ACCEPT | `find_component_delim`/`consume_balanced_at` both classed **"scan"** (`:185,:187`) — P1-F is internally consistent classing BOTH as scan, which is the CH2-correct read (it is the same byte primitive). Note: P1-F's choice here is the one P1-C §2.4 and P1-E §2.3/§2.5 should converge to. |
| §2.3 fact-stream attribution | ACCEPT | Alloc/string primitives named to libsystem/std + `emit_fact_stream`/`push_ascii_lower_hex`. |
| §2.3 lightningcss attribution | ACCEPT (CH2-positive) | Resolves the comparator to its OWN primitives (`cssparser::tokenizer::consume_name`, `parcel_selectors::parse_selector`, `PropertyId::from_name_and_prefix`) — proves the >SOTA bar genuinely materializes and keeps comparator symbols on a separate path (no conflation with Track 1). Good cross-substrate hygiene (also serves CH5). |
| §4.2 fact-stream 64% alloc | ACCEPT | "one branchless u32 write" `push_plain_offset` named to the tape primitive. |
| §4.3 NEON re-confirm | ACCEPT | "grammar-neutral byte-class delimiter scan (`delimiters.contains(&byte)` over `b";{}"`) — a `byte_class_index_64` / movemask structural pre-scan target." Correct primitive naming. |
| §4.4 tape_activated=false verified | ACCEPT (CH2-strong) | The `grep` proof that CSS references ZERO tape symbols while JSON DOES is the empirical Lock-1/Lock-14 substrate fact: the tape primitive is shared infrastructure currently fired by JSON and dormant for CSS. Names it precisely. |

P1-F: **9/9 ACCEPT.** Strongest cross-substrate primitive hygiene; its
both-as-scan class choice (§2.3) is the convergence target for P1-C/P1-E.

## §2 — Cross-cutting CH2 finding (the one substantive generality gap)

The single recurring CH2 defect across the corpus is a **taxonomy
inconsistency on `consume_balanced_at`**: P1-C (`:200`) and P1-E (`:161,:222`)
class it "structural"; P1-A/P1-B/P1-D/P1-F treat the balance scan as part of
the scan family. Source (`generated.rs:320-340`) shows it is the same per-byte
`match`-on-byte loop as `find_component_delim`, recursing on open-brackets. For
S-P2 the consequence is concrete: if the synthesis artefact (P1-E) presents
`find_component_delim`=scan and `consume_balanced_at`=structural as two classes,
S-P2 may design TWO primitives (a scan kernel + a "structural" kernel) where
**one** `byte_class_index_64` + bracket-depth-tracking NEON kernel serves both —
the exact Lock-14 over-fragmentation CH2 exists to catch. The fix is not to
force `consume_balanced_at` to "scan" (its recursion IS structural) but to
annotate, in the synthesis roll-up, that it shares the scan primitive's inner
loop and is one NEON target with `find_component_delim`. This is the only REVISE
that rises above cosmetic.

All other CH2 content is sound: every artefact names the dominant CSS leaf's
underlying primitive (`byte_class_index_64` / `select_classifier`), every
artefact names the alloc floor as grammar-neutral std/libsystem `String` growth
(not CSS logic), and three artefacts (P1-D §4.2, P1-E §4.4, P1-F §4.4) supply
the load-bearing Lock-14 evidence that the shared kernels (`select_classifier`,
the tape) are dormant-on-CSS / live-on-JSON — which is precisely the generality
ground S-P2 builds the primitive-design pass on.

No artefact commits the named CH2 failure mode (a CSS-role symbol name presented
AS the primitive). The closest risk — `push_ascii_lower_hex` reading as
CSS-semantic work — is dispositioned REVISE on P1-B only (the other artefacts
correctly tag it FNV-diagnostic).

## §3 — Disposition counts

Section-level dispositions across the six artefacts (42 sections reviewed):

| Disposition | Count |
|---|---:|
| ACCEPT | 38 |
| REVISE | 4 |
| REJECT | 0 |

REVISE list (all foldable into V2; zero orphans):
1. **P1-B §4.3** (`p1b-samply-mode-2.md:265-272`) — annotate `push_ascii_lower_hex`
   as FNV-diagnostic (no CSS-semantic value), consistent with P1-A §4.3 / P1-C A3.
2. **P1-C §2.4** (`p1c-samply-mode-3.md:198`) — re-class `find_component_delim`
   from "structural" to "scan" (source body is `delimiters.contains(&byte)`),
   consistent with the other five artefacts.
3. **P1-E §2.5 roll-up** (`p1e-hot-leaf-attribution.md:161,222`) — annotate
   `consume_balanced_at` as "structural recursion over the **scan** primitive
   (shares `find_component_delim`'s byte-membership inner loop) — one NEON target."
4. **P1-C §2.4 / P1-E §2.3** convergence note — the two artefacts that class the
   delim leaf "structural" must converge to P1-F's both-as-scan read so S-P2
   inherits one primitive family, not two.

(Items 2 and 4 overlap on P1-C §2.4; counted once each by the distinct fix they
demand — re-class (2) and converge-with-P1F (4).)

ACCEPT rate: 38/42 = **90.5%**. Below the §3Z 95% bar; the four REVISE are
mechanical taxonomy/annotation folds with named path:line fixes, expected to
clear in V2. Zero REJECT, zero orphan REVISE, zero CH2 critical defect (no
role-for-primitive mis-attribution).

## §4 — Verdict

CH2 GENERALITY: the S-P1 V1 profile **grounds a grammar-neutral design**. The
two dominant benched-CSS hot leaves resolve to the same byte-class-membership
primitive (`find_component_delim` = scalar `delimiters.contains`) that JSON
already runs through the shared `select_classifier`/`PrimitiveKernels` NEON
kernels, and the artefacts name that primitive (`byte_class_index_64`) and its
shared-kernel route. The fact-stream allocation floor is correctly attributed to
grammar-neutral `String`/libsystem growth, not CSS logic. The tape and SIMD
primitives are correctly attributed as shared infrastructure dormant-on-CSS.
The sole substantive gap is a class-taxonomy inconsistency on
`consume_balanced_at` that risks S-P2 splitting one NEON target into two — four
mechanical REVISE folds resolve it. **No REJECT. ACCEPT 90.5%; fold the four
REVISE into V2.**
