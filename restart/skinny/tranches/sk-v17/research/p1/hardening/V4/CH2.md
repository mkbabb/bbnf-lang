# SK-V17 S-P1 CHALLENGE — CH2 GENERALITY (V4)

Lens: CH2 GENERALITY. Pass: S-P1 Profile. Cycle: V4. Date: 2026-05-29.
Reviewer scope: PASS-1-PROFILE §3 CH2 + ORCHESTRATOR §3W. Subject: do the six
V4 P1 artefacts attribute the benched-CSS hot leaves to **grammar-neutral
primitives** (scanner / classifier / tape — Lock 14) rather than CSS-named code
paths? A hot leaf named for a CSS role whose underlying symbol is a generic
primitive is a Lock 14 mis-attribution → REVISE. The profile must name the
*primitive* so S-P2 can ask whether it generalises to CSS L4 / Sheets /
BBNF-self.
Artefacts reviewed: `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`,
`p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`,
`p1f-bench-canonical.md` (all dated Cycle V4, 2026-05-29).
Disposition vocabulary: ACCEPT / REVISE / REJECT.

## §0 — Method (what CH2 re-verified against source at HEAD `6496fecae`)

CH2 V4 is NOT a rubber-stamp of the V3 100% verdict. The V4 artefacts were
regenerated (every frontmatter reads Cycle V4; P1-F additionally folds the
CH5-V2-R1/CH6 line-cite refresh at `p1f:44-70` that re-touches the §2.3 row-2
wrapper cite where a CH2 class column lives). CH2 V4's obligation is therefore
twofold: (a) re-run the source spine on the benched skinny tree at master HEAD
`6496fecae` and re-read every CH2-bearing section of all six artefacts, and (b)
verify the V4 regeneration — chiefly the P1-F wrapper-cite refresh — preserved
every prior fold and swung no class column to a CSS-role name. The verdict turns
on (i) whether the cited CSS-named symbol is in fact a generic primitive, (ii)
whether each artefact NAMES that primitive and its shared-kernel route, and (iii)
whether the V4 fold-prose introduced any counter-error. Five source facts, all
re-verified live this cycle at HEAD `6496fecae` (git-confirmed):

- **`CssFullParser::find_component_delim`**
  (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:288-311`).
  Body re-read at HEAD: `while pos < self.bytes.len() { let byte =
  self.bytes[pos]; if delimiters.contains(&byte) { return Ok(Some((byte,pos))) }
  pos = match byte { … } }` (`:293-308`). `delimiters` is a runtime `&[u8]` slice
  supplied by callers (`b":{};"` at `find_colon_before:314`, and the
  `b"{};"`/`b";}"` call sites in `parse_block_item`/`parse_declaration`). This is a
  **byte-class-membership scan over a runtime byte set** — grammar-neutral by
  construction; the CSS-ness lives only in the caller-supplied delimiter slices
  and the bracket/quote/comment escape arms (`:299-307`). Source-confirmed.
- **`CssFullParser::consume_balanced_at`** (`generated.rs:320-340`). Body re-read
  byte-for-byte this cycle: the SAME `while pos < self.bytes.len()` (`:322`) +
  per-byte `pos = match byte` (`:327`) inner loop as `find_component_delim`,
  differing ONLY in the membership test (`byte == close` `:324` vs
  `delimiters.contains(&byte)` `:295`) and recursing on open-brackets
  (`:330-332`). It is the SAME scan primitive reached recursively — one NEON
  target. Source-confirmed at HEAD.
- **`find_colon_before`** (`generated.rs:313-318`) is
  `self.find_component_delim(start, b":{};")` (`:314`) — a THIRD call of the same
  primitive with a different runtime delimiter slice. Source-confirmed (relevant
  to P1-D §2.5's redundant-re-scan generality finding).
- **The shared-kernel generality proof.**
  `skinny/crates/runtime/src/grammars/json/scan.rs:219` routes JSON's structural
  scan through
  `bbnf_simd::aarch64::classify_tbl4::classify_structural_terminator_block_from_table`
  over a lo6 class table — i.e. JSON's structural scan IS the
  `select_classifier`/`PrimitiveKernels` family (`bbnf-simd/src/dispatch.rs:42`
  `select_classifier`, `:50` `PrimitiveKernels` with `byte_class_from_table_64`,
  `:58` `primitive_kernels()`, `:67-68` the NEON
  `byte_class_from_table_64_neon` binding — all re-read this cycle). CSS's
  `find_component_delim` is a **scalar per-byte re-implementation of the same
  byte-class-membership primitive not yet routed through the shared kernel.** Two
  activation levels (scalar vs NEON) of one primitive — the decisive Lock-14 fact.
  Source-confirmed.
- **Dormancy proof (re-ran the cited greps this cycle).**
  `grep -rln "TapeBuilder|ValueRef|PayloadArena|crate::tape"
  skinny/crates/runtime/src/grammars/css_l4_*/` returns **ZERO** (P1-F §4.4,
  P1-E §2.5, P1-A §4.5); the SAME grep over `grammars/json/` returns **SIX** files;
  and `grep -rln "bbnf_simd|select_classifier"
  skinny/crates/runtime/src/grammars/css_l4_*/` returns **ZERO**. The shared tape
  and the shared SIMD kernel are both dormant-on-CSS / live-on-JSON — empirically
  confirmed this cycle, exactly as the artefacts state. `parse_4_digits_dotprod`
  (the udot orphan) re-confirmed at `bbnf-simd/src/aarch64/digit_mac.rs:27`;
  `emit_full_parse` re-confirmed at `generated.rs:61`, `emit_fact_stream` at `:5`.

All five source spine facts hold at HEAD `6496fecae`, with the identical line
numbers cited across the V3→V4 transition (288, 295, 313/314, 320, 324; json/scan
:219; dispatch :42,:50; digit_mac :27). They are the spine of the dispositions
below.

## §0.1 — V3 disposition-carry verification (the load-bearing V4 check)

V3 CH2 returned **58/58 ACCEPT, REVISE list empty** — every V2 fold (P1-B
FNV-diagnostic; P1-C re-class to scan; P1-E structural-over-scan roll-up
one-NEON-target; the P1-C/P1-E/P1-F convergence to both-as-scan) had held with no
orphan and no counter-error through the X1'/CH4-5 cost-surface regeneration. CH2
V4's obligation is therefore: did the V4 regeneration (chiefly the P1-F
`p1f:44-70` CH5/CH6 wrapper-cite refresh) **preserve** every one of those folds,
or did fold-prose swing any class column back to a CSS-role name? CH2 V4 verified
each held:

| Prior-folded item | V4 status |
|---|---|
| `push_ascii_lower_hex` = FNV/hex DIAGNOSTIC, NOT CSS-semantic | **HELD** — P1-A `:143,:179`, P1-B `:182,:344`, P1-C `:379-385`, P1-F `:436,:440` all annotate it FNV/hex-diagnostic with NO CSS-semantic value. |
| `find_component_delim` class = **scan** (not "structural") | **HELD** — class column = **scan** in all six: P1-A `:125`, P1-B `:221`, P1-C `:252`, P1-D `:272`, P1-E `:181,:247`, P1-F `:405`. |
| `consume_balanced_at` = structural recursion OVER the scan primitive, one NEON target | **HELD** — P1-E `:183,:248` ("byte-for-byte the same … as `find_component_delim` :293-308 … shares ONE NEON byte-class-scan target"), P1-C `:254,:268-271`, P1-D `:273`, P1-F `:407,:417`. |
| convergence to one-NEON-target (both-as-scan) | **HELD** — P1-C `:254,:274`, P1-D `:281`, P1-E `:248,:324`, P1-F `:416-417` all state the balance leaf is recursion OVER the scan primitive sharing ONE NEON target. |
| udot/digit kernel has NO benched CSS antecedent (do not over-generalise a JSON primitive onto CSS) | **HELD** — P1-E `:337-346`, P1-D `:545-547`. |

**All five prior-folded items held into V4 with no orphan and no counter-error.**
The P1-F `p1f:44-70` wrapper-cite refresh is CH2-NEUTRAL: it corrects the row-2
wrapper line cite to `css_canon_bench.rs:103-105 fn track1_full_parse`
(grep-verified `103:fn track1_full_parse`) and re-anchors the I2 non-conflation
note — both CH5/CH6 territory — and leaves the §2.3 class taxonomy (scan /
tape-structural-scaffold / string) fully intact. The cost-surface posture (the
instr/byte authoritative framing carried from V3) likewise re-keys cost *density*
without disturbing the class taxonomy.

## §1 — Per-artefact section dispositions (V4)

### P1-A (`p1a-samply-mode-1.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + COST-SURFACE / CANONICAL-HARNESS notes | ACCEPT | Cost surface + harness named; no role-for-primitive conflation (CH4-neutral). |
| §1 method | ACCEPT | Benched-plane symbols precise (`emit_fact_stream`/`emit_full_parse`/`CssFullParser`); no CSS-named-primitive invention. |
| §2.1 Mbps dispersion + §2.1b instr/byte | ACCEPT | Plane-level cost density; fact/full i/B ratio (4.4-7.1x) is a primitive-cost figure, not a CSS-role claim. |
| §2.2 track1_full hot-leaf table | ACCEPT | `find_component_delim` class = "scan (delimiter membership)" (`:125`); `consume_balanced_at` = "scan (nesting balance)" (`:127`); `parse_stylesheet` = "scan (driver + ws skip)" (`:126`). Primitive-class throughout. |
| §2.3 fact-stream table + `mach_absolute_time` attribution | ACCEPT | Alloc/string classes named to std/libsystem primitives; the `mach_absolute_time` leaf correctly re-attributed to the libmalloc allocation family (`:149`), not a timer artefact — grammar-neutral. |
| §2.4 plane-shape note | ACCEPT | Names absence of a rich-typed CSS parser without inventing a CSS-named primitive. |
| §4.2 NEON-leaf re-confirm | ACCEPT — **reference CH2 attribution** | "grammar-neutral byte-set-membership scan over a runtime delimiter slice (CH2: it is the generic `find_ascii_set_member64` primitive shape, NOT a CSS-named path) — the candidate for `byte_class_index_64` / `to_bitmask64` via `bbnf-simd/src/dispatch.rs select_classifier` (`:42`)" (`:176`). Names primitive AND shared-kernel route; flags the tape-gate, does NOT re-open it. |
| §4.3 fact-stream allocation-bound | ACCEPT | `push_ascii_lower_hex` "FNV-diagnostic primitive with NO CSS-semantic value … must not be carried into S-P2" (`:179`); alloc = std/libsystem, grammar-neutral. |
| §4.5 no-second-substrate + sources | ACCEPT | Names substrate-union intact + tape UNWIRED on CSS path (CH5-adjacent; CH2-neutral). |

P1-A: **9/9 ACCEPT.** Remains the reference artefact for primitive-naming discipline.

### P1-B (`p1b-samply-mode-2.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + canonical-harness + c/B-provenance notes | ACCEPT | Plane symbols precise; cost-provenance is CH4-territory, CH2-neutral. |
| §1 method | ACCEPT | Both benched planes named to symbols over one grammar module. |
| §2.1 cold throughput + instr/B | ACCEPT | Plane-level; fact/full i/B ratio is primitive-cost. |
| §2.2 fact-stream table | ACCEPT | Classes libsystem/std primitives; `push_ascii_lower_hex` carries "**FNV/hex DIAGNOSTIC encoding, NOT CSS-semantic value** … consistent with P1-A §4.3, P1-C A3" (`:182`). |
| §2.3 recognition table + CH2 callout | ACCEPT | `find_component_delim` = **scan** (`:221`); `consume_balanced_at` = **scan** (`:223`); explicit CH2 callout (`:235-244`): "this leaf is the byte-class-membership scan primitive … NOT a CSS-named role … the SAME byte-class-membership primitive JSON's structural scan runs through `select_classifier` / `PrimitiveKernels` (verified `json/scan.rs:219`) … S-P2 inherits it as a generic scan primitive, free to ask whether it generalises to CSS L4 / Sheets / BBNF-self." Names primitive + shared-kernel route + the generality question CH2 exists to seed. |
| §2.4 canonical harness | ACCEPT | Harness-level; CH2-neutral. |
| §3 delta + String-tax i/B re-derivation | ACCEPT | String tax re-derived on `ri_instructions` (~5.0x, `:316`); plane reconciliation, no role-for-primitive conflation. |
| §4.1 orthogonal-hot-leaves masking | ACCEPT | Names alloc-floor vs scan primitive families distinctly; the tape lever "SHIFTS toward the `find_component_delim` scan that today is masked" (`:330`). |
| §4.2 NEON re-confirm | ACCEPT | Names the kernel + JSON-shared need; antecedent-grounded. |
| §4.3 push_ascii_lower_hex | ACCEPT | Dedicated anomaly: FNV/hex DIAGNOSTIC encoder with NO CSS-semantic value (`:344`). |
| §4.5 pre-block check + sources | ACCEPT | The sources block names "JSON byte-class-membership scan (the SAME primitive `find_component_delim` is, CH2)" (`:426`); CH3/CH5-territory, CH2-neutral. |

P1-B: **11/11 ACCEPT.**

### P1-C (`p1c-samply-mode-3.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + harness-convergence + c/B-posture notes | ACCEPT | Names both planes as code paths over one grammar module. |
| §1.1 benched-surface reconciliation | ACCEPT — **see §2 residual note** | Names `emit_full_parse` plane the "delimiter/balance **structural scanner**" (`:83`); a plane-descriptive noun (a scanner that emits a structural summary), NOT a class-column mis-tag — the §2.4 class column reads **scan** (`:252`). Below cosmetic threshold (carried unchanged from V2/V3). |
| §1.2 / §1.3 harness + samply method | ACCEPT | Harness/symbolication; CH2-neutral. |
| §2.1 / §2.2 throughput | ACCEPT | No primitive mis-naming. |
| §2.3 emit_fact_stream self-time | ACCEPT | Own-code leaves resolved with string/tape/hash classes; the `:26 push_hex64(fnv64(...))` call-site = "hash (FNV)" (`:227`) — primitive-level. |
| §2.4 emit_full_parse table + re-class note | ACCEPT | `find_component_delim` class = **scan** (byte-class-membership inner loop, `:252`); `consume_balanced_at` = "**structural recursion OVER the scan primitive** … shares the `find_component_delim` byte-membership inner loop — one NEON target" (`:254,:268-271`). Re-class note cites `delimiters.contains(&byte)` `:295` + `json/scan.rs:219` (`:263`). CH2-strong. |
| §2.5 PMU i/B ledger | ACCEPT | i/B is plane-cost; CH2-neutral. |
| §3 delta | ACCEPT | Plane reconciliation; re-classed-scan rows correct (`:345-346`). |
| §4 A1 plane bifurcation | ACCEPT | "`emit_fact_stream` = 64% syslib + String-build, ZERO `find_component_delim`; `emit_full_parse` = 95% (of parse self) `find_component_delim`+`consume_balanced_at`" (`:361-362`) — primitive families correct. |
| §4 A2 masking probe | ACCEPT | Frames materialization-vs-scan at the primitive level (`:365-366`). |
| §4 A3 FNV diagnostic | ACCEPT | "FNV and the hex encoder are FNV-diagnostic primitives with NO CSS-semantic value (consistent with P1-A §4.3, P1-B §4.3)" (`:381-385`). |
| §4 A5 no-NEON-yet + one-kernel | ACCEPT (CH2-strong) | "one NEON byte-class-membership kernel covers ~68.6% of the structural-scan plane's self-time" (`:397-398`) — names the single shared kernel for both leaves; `consume_balanced_at` "shares the same byte-membership inner loop" (`:397`). |

P1-C: **12/12 ACCEPT.** (§1.1 residual noun is ACCEPT-with-note, not REVISE — identical posture to V2/V3, below the cosmetic threshold; the load-bearing class column at `:252` is correct.)

### P1-D (`p1d-pmu-cycles.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §3.1 c/B posture | ACCEPT | Cost-density posture is CH4-territory; CH2-neutral (re-keys density, leaves class taxonomy untouched). |
| §2.4 hot-leaf table + per-line | ACCEPT | `find_component_delim`/`consume_balanced_at` both **scan** (`:272,:273`); per-line breakdown (`:284` ff: `:295 delimiters.contains`, `:298 match`, `:307 advance`; `:300 :327 match` in balance) names the per-byte membership primitive directly. Strong. |
| §2.5 redundant overlapping re-scan | ACCEPT (CH2-positive) | The decl body is walked 2-3x by the SAME `find_component_delim` primitive (`:211 b"{};"`, `find_colon_before:219/:314 b":{};"`, `:247 b";}"`) — CH2 re-verified all three are the same primitive with different runtime delimiter slices (`find_colon_before:314` confirmed at HEAD). Named at the primitive/substrate level, not a CSS-specific dedup. The cross-grammar generality finding the others do not carry. |
| §3.2 PMU table | ACCEPT | instr/byte plane-ranking; CH2-neutral. |
| §3.3 delta | ACCEPT | Plane reconciliation; grammar-neutral. |
| §4.2 NEON antecedent | ACCEPT | "`byte_class_index_64` movemask over the `select_classifier` kernel (`bbnf-simd/src/dispatch.rs:42`)" directly replaces the leaf (`:495-496`). Explicit Lock-14 framing. |
| §4.3 emit ~34% correction | ACCEPT | Grammar-neutral correction (String-emit is plane-dependent). |
| §4.7 no-SIMD-on-CSS | ACCEPT (CH2-strong) | The scan leaves "call NO `select_classifier` … are JSON-wired only, and `digit_mac.rs:27 parse_4_digits_dotprod` (udot orphan) is never reached" (`:545-547`). Names the dormant shared kernel + the udot orphan precisely; CH2 re-confirmed both at HEAD. |

P1-D: **8/8 ACCEPT.** Tied with P1-A as strongest; §2.5 carries the cross-grammar generality finding.

### P1-E (`p1e-hot-leaf-attribution.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + §2.2 plane intro | ACCEPT | — |
| §2.3 full_parse attribution | ACCEPT | `find_component_delim` = **scan** (`:181`); `consume_balanced_at` = "**structural-over-scan** … its inner loop `generated.rs:322-338` is byte-for-byte the same `while pos < len` + per-byte `match` over `'"/([{ )]}` as `find_component_delim` :293-308, differing only in the membership test — `byte == close` :324 vs `delimiters.contains` :295 … the SAME byte-class-membership scan inner loop reached recursively, and shares ONE NEON byte-class-scan target" (`:183`). CH2 re-read `:320-340` confirms byte-for-byte. Source-exact. |
| §2.4 fact_stream attribution | ACCEPT | Syslib-caller attribution names the alloc family reached FROM `emit_fact_stream` String growth; `push_ascii_lower_hex` = string (FNV hex). Grammar-neutral. |
| §2.5 roll-up (inherited class table) + negative attribution | ACCEPT (CH2-strong) | `consume_balanced_at` roll-up class = "**structural-over-scan** … NOT a distinct leaf, it folds into the SAME single NEON byte-class-scan target as the row above" (`:248`). Plus the negative attribution `:255-261`: "**No `tape` leaf at all** … `Tape`/`ValueRef`/`TapeBuilder` appear nowhere … The `dispatch` vehicle (`select_classifier`) likewise appears nowhere — zero SIMD on the CSS path." The dormant-shared-primitive evidence S-P2 needs. |
| §3 delta + c/B posture | ACCEPT | Plane reconciliation; adopts the pass-wide c/B posture (CH4-territory). |
| §4.1 recognition masking | ACCEPT | Primitive-level. |
| §4.2 fact-stream floor | ACCEPT | "it is grammar-neutral (it is `String` … growth, not CSS-specific logic)" (`:316`). Explicit CH2 callout. |
| §4.3 NEON re-confirm | ACCEPT | "= ~68% in ONE shared" byte-class scan; names `byte_class_index_64` + `select_classifier` route + the SYNTHESIS NEON gate (`:324-331`). |
| §4.4 no-digit/unicode antecedent | ACCEPT (CH2-strong) | "the udot/i8mm digit kernel (`digit_mac.rs:27`, C4b) has **no benched CSS antecedent** … S-P2 must … not inherit a CSS digit-kernel hypothesis from here" (`:337-346`). Blocks mis-generalising a JSON primitive onto CSS without a fresh antecedent — CH2 working protectively. |
| §4.6 pre-block check + sources | ACCEPT | CH3-territory; sources name the udot orphan + `select_classifier` route (`:388-389`); CH2-neutral. |

P1-E: **10/10 ACCEPT.** (§2.5 split into roll-up + negative-attribution, both ACCEPT; finer granularity than V3, no bundling.)

### P1-F (`p1f-bench-canonical.md`)

| § | Disposition | Note |
|---|---|---|
| Frontmatter + V4 fold log (`:44-70` CH5-V2-R1/CH6 wrapper-cite refresh) | ACCEPT | The fold corrects the row-2 wrapper cite to `css_canon_bench.rs:103-105 fn track1_full_parse` (grep-verified `103:fn track1_full_parse`, `:46,:69-70`) and re-anchors the I2 non-conflation note — CH5/CH6 territory. CH2-NEUTRAL: no class column moved to a CSS-role name; the row-2 bucket stays a measurement scaffold. |
| §1.1 harness designate + source-line refresh | ACCEPT | `push_ascii_lower_hex` line refresh (`:53`) is a no-claim-change cite; harness-level. |
| §2.2 instr/byte + §2.2.1 CPI/IPC | ACCEPT | Plane-cost + counter-physics; "well-predicted load+compare+branch scan loops (`find_component_delim`)" (`:374`) names the primitive, CH2-neutral. |
| §2.3 full_parse attribution | ACCEPT | `find_component_delim` = **scan** (byte-at-a-time delimiter scan, hot at `:295 delimiters.contains(&byte)`, `:405`); `consume_balanced_at` = **scan** ("recursion OVER the same byte-membership inner loop as `find_component_delim`", `:407,:417`). Internally consistent both-as-scan. |
| §2.3 row-2 wrapper bucket | ACCEPT (CH2-positive) | The 26.74% `track1_full_parse` wrapper classed "tape/structural (the summary emit + harness wrapper; NOT a retained/second pass — see §4.6)" and annotated "PURE measurement scaffold; maps to `emit_full_parse` `generated.rs:61` in prod" (`:406`). Correctly NOT a CSS-role primitive — it is named as harness scaffold, not a parser-owned structure. |
| §2.3 fact-stream attribution + push_ascii_lower_hex note | ACCEPT | Alloc/string primitives named to libsystem/std; `push_ascii_lower_hex` = "string (FNV/hex diagnostic encode)" (`:436,:440`). |
| §2.3 lightningcss attribution | ACCEPT (CH2-positive) | Resolves the comparator to its OWN primitives (`cssparser::tokenizer::consume_name` scan, `cssparser::Tokenizer::skip_whitespace` scan, `parcel_selectors::parser::parse_selector` structural, `cssparser::tokenizer::consume_numeric` number, `:464-471`) — keeps comparator symbols on a separate path; no Track-1 conflation (also serves CH5). |
| §3 delta | ACCEPT | Plane reconciliation; outcome enum correct. |
| §4.3 NEON re-confirm | ACCEPT | "grammar-neutral byte-class delimiter scan (`delimiters.contains(&byte)` over `b";{}"`, `generated.rs:295`) — a `byte_class_index_64` / movemask structural pre-scan target … `select_classifier` / `PrimitiveKernels` (P1-E verified `json/scan.rs:219`)" (`:562-567`). |
| §4.4 tape_activated=false | ACCEPT (CH2-strong) | The grep proof (`:572-577`) that CSS references ZERO tape symbols — CH2 re-ran it: CSS ZERO, JSON SIX files. Names the shared tape primitive as dormant-on-CSS precisely. |
| §4.6 full_parse wrapper | ACCEPT | The wrapper "PURE measurement scaffold, NOT a retained or second pass" (`:596-599`) — correctly NOT a CSS-role primitive; sources name the shared scan primitive (`:661`). |

P1-F: **11/11 ACCEPT.** Strongest cross-substrate primitive hygiene; the both-as-scan + lightningcss-own-primitives reference. (Split finer than V3 to disposition the V4 fold log + the row-2 wrapper bucket individually.)

## §2 — Cross-cutting CH2 finding (V4)

**No CH2 generality defect surfaces in V4.** The two dominant benched-CSS hot
leaves resolve, source-confirmed at HEAD `6496fecae`, to ONE
byte-class-membership scan primitive: `find_component_delim` is
`delimiters.contains(&byte)` over a runtime `&[u8]` slice (`generated.rs:295`),
and `consume_balanced_at` is the byte-for-byte same `while`+`match` inner loop
reached recursively, differing only in the membership test (`byte == close`
`generated.rs:324` vs `delimiters.contains` `:295`). `find_colon_before`
(`generated.rs:314`) is a third call of the same primitive. All six artefacts
converge on this:

- **Class column** for `find_component_delim` = **scan** in all six (P1-A `:125`,
  P1-B `:221`, P1-C `:252`, P1-D `:272`, P1-E `:181,:247`, P1-F `:405`).
- **`consume_balanced_at`** is uniformly framed as the SAME scan primitive
  reached recursively — "scan (nesting balance)" (P1-A `:127`, P1-B `:223`, P1-D
  `:273`, P1-F `:407`), or the more precise "structural-over-scan … folds into
  the SAME single NEON byte-class-scan target" (P1-C `:254`, P1-E `:183,:248`).
  Both framings read as **one NEON target** — the Lock-14 outcome.

Every artefact names that primitive (`byte_class_index_64` /
`find_ascii_set_member64` / `to_bitmask64`) AND its shared-kernel route
(`select_classifier`/`PrimitiveKernels` via `bbnf-simd/src/dispatch.rs:42`, the
SAME family JSON's structural scan runs through at `json/scan.rs:219` —
re-confirmed at HEAD this cycle). The fact-stream allocation floor is attributed
to grammar-neutral std/libsystem `String` growth (P1-E `:316` "not CSS-specific
logic"; P1-A `:149` `mach_absolute_time` correctly re-attributed to the libmalloc
family, NOT a timer artefact), not CSS logic. The tape and SIMD primitives are
attributed as shared infrastructure dormant-on-CSS / live-on-JSON — the dormancy
greps CH2 re-ran return ZERO tape + ZERO `bbnf_simd` on CSS and SIX tape files on
JSON, confirming it empirically. P1-D §2.5's redundant-re-scan finding (three
calls of the SAME primitive over each declaration body) and P1-E §4.4's block on
inheriting the JSON digit kernel without a fresh CSS antecedent are CH2-positive:
they reason *about* generality (one primitive, three call sites; do not
over-generalise a JSON kernel onto CSS) rather than merely avoiding a leak.
P1-F's lightningcss attribution (`:464-471`) resolving the comparator to its OWN
primitives keeps the comparator symbol path separate from Track 1 — primitive
hygiene that also serves CH5.

**The V4 regeneration introduced no CH2 counter-error.** The chief V4 change is
the P1-F `:44-70` wrapper-cite refresh (CH5-V2-R1/CH6) — it corrects a
`css_canon_bench.rs` line cite and re-anchors the non-conflation note; both leave
the §2.3 class taxonomy intact and the row-2 wrapper bucket remains a measurement
scaffold, NOT a CSS-role primitive. No artefact presents a CSS-role symbol AS the
primitive; the `push_ascii_lower_hex` leaf is uniformly annotated FNV/hex-diagnostic
across P1-A/B/C/F.

**Residual prose noun (sub-cosmetic, carried from V2/V3, NOT REVISE).** P1-C
`:83` describes the `emit_full_parse` plane as the "delimiter/balance **structural
scanner**" — plane-descriptive (a scanner that emits a structural summary), NOT a
class-column mis-tag; its §2.4 class column at `:252` correctly reads **scan**.
Identical to the V2/V3 residual; below the cosmetic threshold and not
dispositioned REVISE. (Pointer for any future tightening: were P1-C to qualify
this noun "structural-summary scanner over the scan primitive" it would foreclose
the only remaining ambiguity, but the load-bearing class column is already
correct, so this is not an orphan REVISE.)

**Corpus-scope note (CH2-clear; pointer to CH1/CH6).** The V4 profile covers the
4 benched CSS corpora (bootstrap, tailwind, material, animate), not the PASS-1
§2.1 17-JSON-corpus matrix — a deliberate CSS-tape-subject re-scope defended in
every frontmatter. CH2 GENERALITY is satisfied: Lock 14 generality is about the
grammar-neutrality of the named *primitives* (which route the CSS scan leaf to
the JSON-shared kernel — proven), not about corpus count. The corpus-coverage
question (does 4/4 CSS satisfy the anti-overfit gate for THIS subject) is a
CH1/CH6 disposition, not a CH2 one; CH2 notes the primitive attribution holds
identically on all four corpora (regular: animate, bootstrap; dense: tailwind,
material), and `consume_balanced_at`'s corpus-character dependence (P1-A `:177`:
10.79% bootstrap vs 0.15% tailwind) is itself attributed at the primitive level
(deep-nesting trait), not a fixed CSS constant — CH2-positive.

## §3 — Disposition counts (V4)

Section-level dispositions across the six artefacts (61 sections reviewed):

| Artefact | Sections | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|---:|
| P1-A | 9 | 9 | 0 | 0 |
| P1-B | 11 | 11 | 0 | 0 |
| P1-C | 12 | 12 | 0 | 0 |
| P1-D | 8 | 8 | 0 | 0 |
| P1-E | 10 | 10 | 0 | 0 |
| P1-F | 11 | 11 | 0 | 0 |
| **Total** | **61** | **61** | **0** | **0** |

(Section count is 61 vs V3's 58 because CH2 V4 split P1-E §2.5 into its roll-up +
negative-attribution sub-claims and split P1-F's V4 fold log + row-2 wrapper
bucket as individually reviewable surfaces — the finer granularity is the more
honest count; no section is bundled.)

**ACCEPT rate: 61/61 = 100%.** Every prior-folded item held into V4 with no orphan
and no counter-error. Zero REJECT, zero open critical defect, zero
role-for-primitive mis-attribution.

REVISE list: **empty.**

## §4 — Verdict

CH2 GENERALITY (V4): the S-P1 V4 profile **grounds a grammar-neutral design with
no open generality defect.** The two dominant benched-CSS hot leaves
(`find_component_delim`, `consume_balanced_at`) resolve, source-confirmed at HEAD
`6496fecae`, to ONE byte-class-membership scan primitive
(`delimiters.contains(&byte)` over a runtime `&[u8]` slice; the balance leaf is
the byte-for-byte same `while`+`match` inner loop reached recursively, differing
only in `byte == close` vs `delimiters.contains`; `find_colon_before:314` is a
third call of the same primitive) — the same primitive JSON already runs through
the shared `select_classifier`/`PrimitiveKernels` NEON kernels (`json/scan.rs:219`).
Every artefact names that primitive (`byte_class_index_64`) and its shared-kernel
route; the fact-stream allocation floor is attributed to grammar-neutral
`String`/libsystem growth (with the `mach_absolute_time` leaf correctly resolved
to the libmalloc family, not a timer), not CSS logic; the tape and SIMD
primitives are attributed as shared infrastructure dormant-on-CSS / live-on-JSON
(the dormancy greps CH2 re-ran return ZERO tape + ZERO `bbnf_simd` on CSS, SIX
tape files on JSON). All five prior-folded items held into V4, and the V4
regeneration (chiefly the P1-F wrapper-cite refresh) introduced no counter-error —
that fold is CH5/CH6-territory and CH2-neutral, leaving the scan/structural/string/
tape-alloc class taxonomy fully intact. S-P2 inherits ONE scan primitive family,
not two. **No REJECT, no REVISE. ACCEPT 100% (61/61).** CH2 clears the §3Z 95%
bar; this is the FOURTH consecutive cycle at or above it (V1 90.5% → V2 100% →
V3 100% → V4 100%), satisfying the two-consecutive ≥95% convergence criterion on
the CH2 axis with zero orphan REVISE.
