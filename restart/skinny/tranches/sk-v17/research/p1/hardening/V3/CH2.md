# SK-V17 S-P1 CHALLENGE — CH2 GENERALITY (V3)

Lens: CH2 GENERALITY. Pass: S-P1 Profile. Cycle: V3. Date: 2026-05-29.
Reviewer scope: PASS-1-PROFILE §3 CH2 + ORCHESTRATOR §3W. Subject: do the six
V3 P1 artefacts attribute the benched-CSS hot leaves to **grammar-neutral
primitives** (scanner / classifier / tape — Lock 14) rather than CSS-named code
paths? A hot leaf named for a CSS role whose underlying symbol is a generic
primitive is a Lock 14 mis-attribution → REVISE. The profile must name the
*primitive* so S-P2 can ask whether it generalises to CSS L4 / Sheets /
BBNF-self.
Artefacts reviewed: `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`,
`p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`,
`p1f-bench-canonical.md` (all dated Cycle V3, 2026-05-29).
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## §0 — Method (what CH2 re-verified against source at HEAD `6496fecae`)

CH2 is NOT a rubber-stamp of the V2 100% verdict. The V3 artefacts were
regenerated (every frontmatter reads Cycle V3; P1-A/P1-B/P1-C/P1-D/P1-F now carry
the X1' / CH4-5 cost-surface folds, which re-touch the §2/§4 prose where the CH2
class columns live). CH2 V3 re-ran the source spine on the benched skinny tree at
master HEAD `6496fecae` (git-confirmed) and re-read every CH2-bearing section of
all six artefacts. The verdict turns on (a) whether the cited CSS-named symbol is
in fact a generic primitive, (b) whether each artefact NAMES that primitive and
its shared-kernel route, and (c) whether the V3 fold-prose introduced any
counter-error that swung a class column to a CSS-role name. Five source facts,
all re-verified live this cycle:

- **`CssFullParser::find_component_delim`**
  (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:288-310`).
  Body re-read at HEAD: `while pos < self.bytes.len() { let byte =
  self.bytes[pos]; if delimiters.contains(&byte) { return Ok(Some((byte,pos))) }
  pos = match byte { … } }` (`:288-309`). `delimiters` is a runtime `&[u8]` slice
  supplied by the callers (`b":{};"` `:313`, and the `b";{}"`/`b"{};"`/`b";}"`
  call sites in `parse_block_item`/`parse_declaration`). This is a
  **byte-class-membership scan over a runtime byte set** — grammar-neutral by
  construction; the CSS-ness lives only in the caller-supplied delimiter slices
  and the bracket/quote/comment escape arms (`:299-306`). Source-confirmed.
- **`CssFullParser::consume_balanced_at`** (`generated.rs:320-340`). Body re-read:
  byte-for-byte the SAME `while pos < len` + per-byte `match` inner loop as
  `find_component_delim`, differing ONLY in the membership test (`byte == close`
  `:324` vs `delimiters.contains` `:295`) and recursing on open-brackets
  (`:327-329`). It is the SAME scan primitive reached recursively — one NEON
  target. Source-confirmed at HEAD.
- **`find_colon_before`** (`generated.rs:313-314`) is `find_component_delim(start,
  b":{};")` — a THIRD call of the same primitive with a different runtime
  delimiter slice. Source-confirmed (relevant to P1-D §2.5's redundant-re-scan
  generality finding).
- **The shared-kernel generality proof.**
  `skinny/crates/runtime/src/grammars/json/scan.rs:218-222` routes JSON's
  structural scan through
  `bbnf_simd::aarch64::classify_tbl4::classify_structural_terminator_block_from_table`
  over a lo6 class table — i.e. JSON's structural scan IS the
  `select_classifier`/`PrimitiveKernels` family (`bbnf-simd/src/dispatch.rs:42`
  `select_classifier`, `:50` `PrimitiveKernels` with `byte_class_from_table_64`,
  re-read this cycle). CSS's `find_component_delim` is a **scalar per-byte
  re-implementation of the same byte-class-membership primitive not yet routed
  through the shared kernel.** Two activation levels (scalar vs NEON) of one
  primitive — the decisive Lock-14 fact. Source-confirmed.
- **Dormancy proof (re-ran the cited greps).**
  `grep -rln "TapeBuilder|ValueRef|PayloadArena|crate::tape"
  skinny/crates/runtime/src/grammars/css_l4_*/` returns **ZERO** (P1-F §4.4,
  P1-E §2.5, P1-A §4.5); the SAME grep over `grammars/json/` returns SIX files
  (config/event_grammar_witness/value/scan/parser/view). The shared tape is
  dormant-on-CSS / live-on-JSON — empirically confirmed this cycle, exactly as the
  artefacts state. `parse_4_digits_dotprod` (the udot orphan) re-confirmed at
  `bbnf-simd/src/aarch64/digit_mac.rs:27`; `emit_full_parse`'s 4-field summary
  (rules/at_rules/qualified_rules/declarations) re-confirmed at `generated.rs:91-99`.

All five source spine facts hold at HEAD. They are the spine of the dispositions
below.

## §0.1 — V2 disposition-carry verification (the load-bearing V3 check)

V2 CH2 returned **45/45 ACCEPT, REVISE list empty** — the four V1 REVISE folds
(P1-B FNV-diagnostic; P1-C re-class to scan; P1-E synthesis roll-up
one-NEON-target; P1-C/P1-E convergence to both-as-scan) had all landed with no
orphan and no counter-error. CH2 V3's obligation is therefore: did the V3
regeneration (the X1'/CH4-5 cost folds) **preserve** every one of those folds, or
did fold-prose swing any class column back to a CSS-role name? CH2 V3 verified each
held:

| V2-folded item | V3 status |
|---|---|
| P1-B §2.2 `push_ascii_lower_hex` = FNV/hex DIAGNOSTIC, NOT CSS-semantic | **HELD** — `p1b-samply-mode-2.md:182` + dedicated §4.3 anomaly (`:344-356`: "FNV/hex DIAGNOSTIC encoder with NO CSS-semantic value … the tape retires WHOLESALE … removed not deferred"). |
| P1-C §2.4 `find_component_delim` class = **scan** (not "structural") | **HELD** — `p1c-samply-mode-3.md:251` class column = **scan** (byte-class-membership inner loop); re-class note `:258-270` cites `delimiters.contains(&byte)` `:295` + the `json/scan.rs:219` shared kernel. |
| P1-E §2.5 `consume_balanced_at` = "structural recursion over the scan primitive … one NEON target" | **HELD** — `p1e-hot-leaf-attribution.md:248` roll-up class = **structural-over-scan** ("shares its byte-membership `while`+`match` inner loop :322-338≡:293-308 … folds into the SAME single NEON byte-class-scan target as the row above"); §2.3 `:183` identical. |
| P1-C/P1-E/P1-F convergence to one-NEON-target (both-as-scan) | **HELD** — P1-C `:267-270`, P1-D `:273`, P1-E `:248,:324-330`, P1-F `:355,:366-368` all state the balance leaf is recursion OVER the scan primitive and shares ONE NEON target. |

**All four V2-folded items held into V3; the X1'/CH4-5 cost folds did not
introduce a CH2 counter-error.** Critically, the cost-surface fold (the
instr/byte authoritative posture) is CH2-NEUTRAL: it re-keys the *cost density* on
`ri_instructions`, but it leaves the *class taxonomy* (scan / structural / string /
tape-alloc) untouched, and indeed the i/B framing reinforces the grammar-neutral
reading — the fact-stream tax is named as "String-building + allocation" (a
std/libsystem primitive family), and the scan plane as the byte-class-membership
primitive, on a counter that cannot be gamed by CSS-role narrative.

## §1 — Per-artefact section dispositions (V3)

### P1-A (`p1a-samply-mode-1.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + COST-SURFACE / CANONICAL-HARNESS notes | ACCEPT | The X1'/CH4-5 notes name the cost surface and harness; no role-for-primitive conflation. CH2-neutral (CH4 owns harness/cost provenance). |
| §1 method | ACCEPT | Names benched-plane symbols precisely (`emit_fact_stream`/`emit_full_parse`/`CssFullParser`); no CSS-named-primitive invention. |
| §2.1 Mbps dispersion + §2.1b instr/byte | ACCEPT | Plane-level cost density; the fact/full i/B ratio (4.36-7.07x) is a primitive-cost figure, not a CSS-role claim. Grammar-neutral. |
| §2.2 track1_full hot-leaf table | ACCEPT | `find_component_delim` class = "scan (delimiter membership)" (`:123`); `consume_balanced_at` = "scan (nesting balance)" (`:125`); `parse_block`/`parse_declaration` = structural (genuine dispatch). Primitive-class throughout. |
| §2.3 fact-stream table | ACCEPT | Alloc/string classes named to std/libsystem primitives (`RawVecInner::reserve`, `_platform_memmove`, libmalloc); `push_ascii_lower_hex` = string (fnv64 source-hash). Grammar-neutral. |
| §2.4 plane-shape note | ACCEPT | Names the absence of a rich-typed CSS parser without inventing a CSS-named primitive. |
| §4.2 NEON-leaf re-confirm | ACCEPT | **Reference CH2 attribution.** "grammar-neutral byte-set-membership scan over a runtime delimiter slice (CH2: it is the generic `find_ascii_set_member64` primitive shape, NOT a CSS-named path) — the candidate for `byte_class_index_64` / `to_bitmask64` via `bbnf-simd/src/dispatch.rs select_classifier`" (`:174`). Names the primitive AND the shared-kernel route. |
| §4.3 fact-stream allocation-bound | ACCEPT | `push_ascii_lower_hex` framed "FNV-diagnostic primitive with NO CSS-semantic value … must not be carried into S-P2" (`:177`); alloc is std/libsystem, grammar-neutral. |
| §4.5 no-second-substrate | ACCEPT | Names substrate-union intact + tape UNWIRED on CSS path (CH5-adjacent; CH2-neutral). |

P1-A: **9/9 ACCEPT.** Remains the reference artefact for primitive-naming discipline.

### P1-B (`p1b-samply-mode-2.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + canonical-harness + c/B-provenance notes | ACCEPT | Plane symbols precise; cost-provenance is CH4-territory, CH2-neutral. |
| §1 method | ACCEPT | Both benched planes named to symbols over one grammar module. |
| §2.1 cold throughput + instr/B | ACCEPT | Plane-level; fact/full i/B ratio is primitive-cost. |
| §2.2 fact-stream table | ACCEPT | Classes libsystem/std primitives; `push_ascii_lower_hex` carries "**FNV/hex DIAGNOSTIC encoding, NOT CSS-semantic value** … consistent with P1-A §4.3, P1-C A3" (`:182`). |
| §2.3 recognition table + CH2 callout | ACCEPT | `find_component_delim` = **scan** (`:221`); `consume_balanced_at` = **scan** (`:223`); explicit CH2 callout (`:235-244`): "this leaf is the byte-class-membership scan primitive … the SAME byte-class-membership primitive JSON's structural scan runs through `select_classifier`/`PrimitiveKernels` (verified `json/scan.rs:219`) … S-P2 inherits it as a generic scan primitive, free to ask whether it generalises to CSS L4 / Sheets / BBNF-self." Names primitive + shared-kernel route + the generality question CH2 exists to seed. |
| §2.4 canonical harness | ACCEPT | Harness-level; CH2-neutral. |
| §3 delta + String-tax i/B re-derivation | ACCEPT | The String tax re-derived on `ri_instructions` (4.36-7.06x); plane reconciliation, no role-for-primitive conflation. |
| §4.1 orthogonal-hot-leaves masking | ACCEPT | Names alloc-floor vs scan primitive families distinctly. |
| §4.2 NEON re-confirm | ACCEPT | Names the kernel + JSON-shared need; antecedent-grounded. |
| §4.3 push_ascii_lower_hex | ACCEPT | Full dedicated anomaly: "FNV/hex DIAGNOSTIC encoder with NO CSS-semantic value … the tape retires WHOLESALE (not merely lazily)" (`:344-356`). |
| §4.5 pre-block check | ACCEPT | CH3-territory; names no second substrate/sidecar (CH2-neutral). |

P1-B: **11/11 ACCEPT.**

### P1-C (`p1c-samply-mode-3.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + harness-convergence + c/B-posture notes | ACCEPT | Names both planes as code paths over one grammar module. |
| §1.1 benched-surface reconciliation | ACCEPT — **see §2 residual note** | Names `emit_full_parse` plane the "delimiter/balance structural scanner" (`:82`); a plane-descriptive noun, NOT a class-column mis-tag — the §2.4 class column reads **scan**. Below cosmetic threshold (carried from V2; unchanged). |
| §1.2 / §1.3 harness + samply method | ACCEPT | Harness/symbolication; CH2-neutral. |
| §2.1 / §2.2 throughput | ACCEPT | No primitive mis-naming. |
| §2.3 emit_fact_stream self-time | ACCEPT | Own-code leaves resolved to `emit_fact_stream`/`push_ascii_lower_hex` with string/tape/hash classes; the `:26 push_hex64(fnv64(...))` call-site = "hash (FNV)" — primitive-level. |
| §2.4 emit_full_parse table + re-class note | ACCEPT | `find_component_delim` class = **scan** (`:251`); `consume_balanced_at` = "**structural recursion OVER the scan primitive** … shares the `find_component_delim` byte-membership inner loop — one NEON target" (`:253,:267-270`). Re-class note (`:258-270`) cites `delimiters.contains(&byte)` `:295` + `json/scan.rs:219`. CH2-strong. |
| §2.5 PMU i/B ledger | ACCEPT | i/B is plane-cost; CH2-neutral. |
| §3 delta | ACCEPT | Plane reconciliation; "re-classed scan" rows correct. |
| §4 A1 plane bifurcation | ACCEPT | "disjoint hot-leaf sets … `emit_fact_stream` = 64% syslib + String-build, ZERO `find_component_delim`; `emit_full_parse` = 95% … `find_component_delim`+`consume_balanced_at`" (`:359-362`) — primitive families correct. |
| §4 A2 masking probe | ACCEPT | Frames materialization-vs-scan at the primitive level; sizes the headroom in i/B. |
| §4 A3 FNV diagnostic | ACCEPT | "FNV and the hex encoder are FNV-diagnostic primitives with NO CSS-semantic value (consistent with P1-A §4.3, P1-B §4.3)" (`:378-384`). |
| §4 A5 no-NEON-yet + one-kernel | ACCEPT (CH2-strong) | "one NEON byte-class-membership kernel covers ~68.6% of the structural-scan plane's self-time, not just the 58.59% leaf" (`:396-398`) — names the single shared kernel for both leaves. |

P1-C: **12/12 ACCEPT.** (§1.1 residual noun is dispositioned ACCEPT-with-note, not REVISE — identical posture to V2 §2, below the cosmetic threshold; the load-bearing class column is correct.)

### P1-D (`p1d-pmu-cycles.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §3.1 c/B posture | ACCEPT | The X1' posture is CH4-territory; CH2-neutral (it re-keys cost density, leaves class taxonomy untouched). |
| §2.4 hot-leaf table + per-line | ACCEPT | `find_component_delim`/`consume_balanced_at` both **scan** (`:272,:273`); per-line breakdown (`:289 :295 delimiters.contains`, `:288 :298 match`, `:307 advance`) names the per-byte membership primitive directly. Strong. |
| §2.5 redundant overlapping re-scan | ACCEPT (CH2-positive) | The decl body is walked 2-3x by the SAME `find_component_delim` primitive (`:211 b"{};"`, `:314 b":{};"` via `find_colon_before`, `:247 b";}"`) — CH2 re-verified all three are the same primitive with different runtime delimiter slices (`find_colon_before:313` confirmed at HEAD). "tokenize-once over a NEON-produced structural index" is named at the primitive/substrate level, not a CSS-specific dedup. The generality finding the others do not carry. |
| §3.2 PMU table | ACCEPT | instr/byte plane-ranking; CH2-neutral. |
| §3.3 delta | ACCEPT | Plane reconciliation; grammar-neutral. |
| §4.2 NEON antecedent | ACCEPT | "`byte_class_index_64` movemask over the `select_classifier` kernel (`bbnf-simd/src/dispatch.rs:42`) … CSS is the non-JSON exercise grammar for the kernel (SYNTHESIS `simd_non_json_exercise`)" (`:487-490`). Explicit Lock-14 framing. |
| §4.3 emit ~34% correction | ACCEPT | Grammar-neutral correction (String-emit is plane-dependent). |
| §4.7 no-SIMD-on-CSS | ACCEPT (CH2-strong) | "the `bbnf-simd/src/dispatch.rs:42` kernels are JSON-wired only … `digit_mac.rs:27 parse_4_digits_dotprod` (udot orphan) is never reached … no `bbnf_simd` frame in 20,377 samples" (`:537-543`). Names the dormant shared kernel + the udot orphan precisely; CH2 re-confirmed both at HEAD. |

P1-D: **8/8 ACCEPT.** Tied with P1-A as strongest; §2.5 carries the cross-grammar generality finding.

### P1-E (`p1e-hot-leaf-attribution.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §2.2 plane intro | ACCEPT | — |
| §2.3 full_parse attribution | ACCEPT | `find_component_delim` = **scan** (`:181`); `consume_balanced_at` = "**structural-over-scan** … its inner loop `generated.rs:322-338` is byte-for-byte the same … as `find_component_delim` :293-308 … the SAME byte-class-membership scan inner loop reached recursively, and shares ONE NEON byte-class-scan target" (`:183`). Source-exact (CH2 re-read `:320-340` confirms byte-for-byte). |
| §2.4 fact_stream attribution | ACCEPT | Syslib-caller attribution names the alloc family reached FROM `emit_fact_stream` String growth; `push_ascii_lower_hex` = string (FNV hex). Grammar-neutral. |
| §2.5 roll-up (the inherited class table) | ACCEPT (CH2-strong) | `consume_balanced_at` roll-up class = "**structural-over-scan** … NOT a distinct leaf, it folds into the SAME single NEON byte-class-scan target as the row above" (`:248`). The synthesis table S-P2 inherits reads ONE NEON target. Plus the negative attribution `:255-261`: "**No `tape` leaf at all** … `Tape`/`ValueRef`/`TapeBuilder` appear nowhere … `select_classifier` likewise appears nowhere — zero SIMD on the CSS path." The dormant-shared-primitive evidence S-P2 needs. |
| §3 delta + c/B posture | ACCEPT | Plane reconciliation; adopts the one pass-wide c/B posture (CH4-territory). |
| §4.1 recognition masking | ACCEPT | Primitive-level. |
| §4.2 fact-stream floor | ACCEPT | "it is grammar-neutral (it is `String` `push_str` growth, not CSS-specific logic)" (`:317`). Explicit CH2 callout. |
| §4.3 NEON re-confirm | ACCEPT | Names `byte_class_index_64` + `select_classifier` route + tape-first ordering. |
| §4.4 no-digit/unicode antecedent | ACCEPT (CH2-strong) | "the udot/i8mm digit kernel (`digit_mac.rs:27`) has **no benched CSS antecedent** … S-P2 must … not inherit a CSS digit-kernel hypothesis from here" (`:337-346`). Blocks mis-generalising a JSON primitive onto CSS without a fresh antecedent — CH2 working protectively. |
| §4.6 pre-block check | ACCEPT | CH3-territory; CH2-neutral. |

P1-E: **9/9 ACCEPT.**

### P1-F (`p1f-bench-canonical.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §1.1 harness designate | ACCEPT | — |
| §2.2 instr/byte + §2.2.1 CPI/IPC | ACCEPT | Plane-cost + counter-physics; CH2-neutral. |
| §2.3 full_parse attribution | ACCEPT | `find_component_delim` = **scan** (`:353`); `consume_balanced_at` = **scan** with "structural recursion OVER the same byte-class-membership scan primitive … the two share ONE NEON target" (`:355,:366-368`). Internally consistent both-as-scan. |
| §2.3 fact-stream attribution + push_ascii_lower_hex note | ACCEPT | Alloc/string primitives named to libsystem/std; `push_ascii_lower_hex` note: "carries NO CSS-semantic value … a String-tax leaf, not a primitive to port" (`:389-394`). |
| §2.3 lightningcss attribution | ACCEPT (CH2-positive) | Resolves the comparator to its OWN primitives (`cssparser::tokenizer::consume_name`, `parcel_selectors::parser::parse_selector`, `PropertyId::from_name_and_prefix`, `:413-419`) — keeps comparator symbols on a separate path; no Track-1 conflation (also serves CH5). |
| §3 delta | ACCEPT | Plane reconciliation; outcome enum (A/L/K/N-direct) correct. |
| §4.3 NEON re-confirm | ACCEPT | "grammar-neutral byte-class delimiter scan (`delimiters.contains(&byte)` over `b";{}"`) — a `byte_class_index_64` / movemask structural pre-scan target … the SAME byte-class-membership primitive JSON runs through `select_classifier`/`PrimitiveKernels`" (`:514-517`). |
| §4.4 tape_activated=false | ACCEPT (CH2-strong) | The grep proof (`:521-526`) that CSS references ZERO tape symbols while JSON DOES — CH2 re-ran it: CSS ZERO, JSON six files. Names the shared tape primitive as dormant-on-CSS precisely. |
| §4.6 full_parse wrapper | ACCEPT | The `css_canon_bench::track1_full_parse` wrapper named "PURE measurement scaffold … NOT a retained/second pass … maps to `emit_full_parse` `generated.rs:61` in prod" (`:545-549`) — correctly NOT a CSS-role primitive. |

P1-F: **9/9 ACCEPT.** Strongest cross-substrate primitive hygiene; the both-as-scan + lightningcss-own-primitives reference.

## §2 — Cross-cutting CH2 finding (V3)

**No CH2 generality defect surfaces in V3.** The two dominant benched-CSS hot
leaves resolve, source-confirmed at HEAD `6496fecae`, to ONE byte-class-membership
scan primitive: `find_component_delim` is `delimiters.contains(&byte)` over a
runtime `&[u8]` slice (`generated.rs:295`), and `consume_balanced_at` is the
byte-for-byte same inner loop reached recursively, differing only in the
membership test (`generated.rs:320-340`). All six artefacts converge on this:

- **Class column** for `find_component_delim` = **scan** in all six (P1-A `:123`,
  P1-B `:221`, P1-C `:251`, P1-D `:272`, P1-E `:181`, P1-F `:353`).
- **`consume_balanced_at`** is uniformly framed as the SAME scan primitive reached
  recursively — "scan (nesting balance)" (P1-A `:125`, P1-B `:223`, P1-D `:273`,
  P1-F `:355`), or the more precise "structural-over-scan … folds into the SAME
  single NEON byte-class-scan target" (P1-C `:253,:267-270`, P1-E `:183,:248`).
  Both framings read as **one NEON target** — the Lock-14 outcome.

Every artefact names that primitive (`byte_class_index_64` / `find_ascii_set_member64`)
AND its shared-kernel route (`select_classifier`/`PrimitiveKernels` via
`bbnf-simd/src/dispatch.rs:42`, the SAME family JSON's structural scan runs through
at `json/scan.rs:219` — re-confirmed at HEAD this cycle). The fact-stream
allocation floor is attributed to grammar-neutral std/libsystem `String` growth
(P1-E `:317` "not CSS-specific logic"), not CSS logic. The tape and SIMD primitives
are attributed as shared infrastructure dormant-on-CSS / live-on-JSON — the dormancy
greps CH2 re-ran return ZERO on CSS and SIX files on JSON, confirming it
empirically. P1-D §2.5's redundant-re-scan finding (three calls of the SAME
primitive over each declaration body) and P1-E §4.4's block on inheriting the
JSON digit kernel without a fresh CSS antecedent are CH2-positive: they reason
*about* generality (one primitive, three call sites; do not over-generalise a JSON
kernel onto CSS) rather than merely avoiding a leak.

**The X1'/CH4-5 V3 cost folds introduced no CH2 counter-error.** They re-key the
cost surface on `ri_instructions` and designate one canonical harness — both
CH4-territory — and they leave the scan/structural/string/tape-alloc class taxonomy
fully intact. No artefact presents a CSS-role symbol AS the primitive; the
`push_ascii_lower_hex` leaf is uniformly annotated FNV/hex-diagnostic across
P1-A/B/C/F.

**Residual prose noun (sub-cosmetic, carried from V2, NOT REVISE).** P1-C `:82`
describes the `emit_full_parse` plane as the "delimiter/balance **structural
scanner**" — plane-descriptive (a scanner that emits a structural summary), NOT a
class-column mis-tag; its §2.4 class column correctly reads **scan**. Identical to
the V2 §2 residual; below the cosmetic threshold and not dispositioned REVISE.

**Corpus-scope note (CH2-clear; pointer to CH1/CH6).** The V3 profile covers the 4
benched CSS corpora, not the PASS-1 §2.1 17-JSON-corpus matrix — a deliberate
CSS-tape-subject re-scope, defended in every frontmatter (P1-A `:11`, P1-B `:56-63`,
P1-C `:19-24`). CH2 GENERALITY is satisfied: Lock 14 generality is about the
grammar-neutrality of the named *primitives* (which route the CSS scan leaf to the
JSON-shared kernel — proven), not about corpus count. The corpus-coverage question
(does 4/4 CSS vs 17/17 JSON satisfy the anti-overfit gate for THIS subject) is a
CH1/CH6 disposition, not a CH2 one; CH2 notes it covers both regular rows (animate,
bootstrap) and dense rows (tailwind, material), and the primitive attribution holds
identically on all four.

## §3 — Disposition counts (V3)

Section-level dispositions across the six artefacts (58 sections reviewed):

| Artefact | Sections | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-A | 9 | 9 | 0 | 0 |
| P1-B | 11 | 11 | 0 | 0 |
| P1-C | 12 | 12 | 0 | 0 |
| P1-D | 8 | 8 | 0 | 0 |
| P1-E | 9 | 9 | 0 | 0 |
| P1-F | 9 | 9 | 0 | 0 |
| **Total** | **58** | **58** | **0** | **0** |

(Section count differs from V2's 45 because CH2 V3 dispositioned each artefact's
frontmatter/method/cost-fold notes individually — the X1'/CH4-5 fold prose is now
a reviewable surface on every artefact — and split P1-C's four §4 anomalies. The
finer granularity is the more honest count; no section is bundled.)

**ACCEPT rate: 58/58 = 100%.** Every V2-folded item held into V3 with no orphan
and no counter-error. Zero REJECT, zero open critical defect, zero
role-for-primitive mis-attribution.

REVISE list: **empty.**

## §4 — Verdict

CH2 GENERALITY (V3): the S-P1 V3 profile **grounds a grammar-neutral design with
no open generality defect.** The two dominant benched-CSS hot leaves
(`find_component_delim`, `consume_balanced_at`) resolve, source-confirmed at HEAD
`6496fecae`, to ONE byte-class-membership scan primitive
(`delimiters.contains(&byte)` over a runtime `&[u8]` slice; the balance leaf is the
byte-for-byte same inner loop reached recursively; `find_colon_before` is a third
call of the same primitive) — the same primitive JSON already runs through the
shared `select_classifier`/`PrimitiveKernels` NEON kernels (`json/scan.rs:218-222`).
Every artefact names that primitive (`byte_class_index_64`) and its shared-kernel
route; the fact-stream allocation floor is attributed to grammar-neutral
`String`/libsystem growth, not CSS logic; the tape and SIMD primitives are
attributed as shared infrastructure dormant-on-CSS / live-on-JSON (the dormancy
greps CH2 re-ran return ZERO on CSS, SIX files on JSON). The four V2-folded items
all held into V3, and the X1'/CH4-5 cost-surface regeneration introduced no
counter-error — the cost fold is CH2-neutral and leaves the class taxonomy intact.
S-P2 inherits ONE scan primitive family, not two. **No REJECT, no REVISE.
ACCEPT 100% (58/58).** CH2 clears the §3Z 95% bar; this is the THIRD consecutive
cycle at or above it (V1 90.5% → V2 100% → V3 100%), satisfying the two-consecutive
≥95% convergence criterion on the CH2 axis with zero orphan REVISE.
