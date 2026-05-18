# SK-V9 S-P2 CHALLENGE V1 — CH2 GENERALITY

Pass: S-P2 Research. Cycle: V1. Lens: CH2 GENERALITY (Lock 14).
Date: 2026-05-18.
Cohort: P2-A union event-model; P2-B retained class/event grammar proof;
P2-C Apache/CITM admission methodology; P2-D aarch64 ASM opportunities;
P2-E `escape_codec_hex_unit` codec primitive; P2-F SOTA teardown M5 Max.
Anchor: `restart/locks/LOCKS.md:60` Lock 14 verbatim — "The substrate
carries ZERO grammar-specific code. … Per-grammar deviations … are
encoded in the grammar metadata + source, NOT in branching code in any
other crate." Canonical primitive-class vocabulary at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
§1.5 (seven-class taxonomy) + §3.5 (`escape_codec_hex_unit` cross-grammar
parameter table).

---

## §1 — Method

Lock 14 audit per P2 report. For each, scan four surfaces:

1. **Vocabulary fidelity** — does the report use the P1-V3-B §1.5
   substrate-neutral class names (`per-string-span scanner`,
   `escape_codec_hex_unit`, `structural-element walker`, `number-digit
   parser`, `traversal-dispatch`, `simd_movemask + string_block_scan`,
   `whitespace_skip`) when discussing primitives, or does it lapse into
   JSON-role names (e.g. "JSON unescape", "JSON tape", "JSON parser")
   inside what claims to be a class proposition?

2. **Substrate cleanliness** — does the proposed intervention touch a
   generic crate (`bbnf-simd`, `bbnf-ir`, `bbnf-codegen`, `runtime/src/
   tape`, `parse-that-regex`, `path`, `egraph`, `csp-solver`) with a
   `match grammar { Json => … }` arm, a grammar-named module, a
   grammar-specific public type, or a hand-written per-grammar runtime
   file? The verification commands at Lock 14 line 60 are the gate.

3. **Cross-grammar admission** — does the report exhibit a *concrete*
   second-grammar instance (CSS L4, Sheets, BBNF-self, JS, TOML — not
   "grammar-neutral by construction" hand-waved) that exercises the
   proposed primitive/contract under different parameter binding, with
   the differences declared at the grammar layer (.bbnf source +
   workspace metadata + per-grammar declaration crate) and not at any
   generic-crate branch site?

4. **Lock 14 verification surface** — does the report bind itself to
   the verification commands at Lock 14 (`rg -n 'JsonParser|CssL4Parser
   |BbnfBootstrap|GoogleSheetsParser' …` returns ZERO; `find
   crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns
   ZERO per-grammar dirs; `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|
   CssL4\s*=>|…' crates/` returns ZERO in non-generated source under
   generic-crate paths)?

The disposition table below assigns per-row verdicts in
{ACCEPT, ACCEPT-WITH-REVISE, REJECT}. ACCEPT-WITH-REVISE rows name the
specific surgical edit V2 must fold. REJECT rows name the Lock-14 leak
that disqualifies the row from V1 admission.

---

## §2 — Disposition table per report

### §2.1 — P2-A: Union event-model

P2-A's thesis is the cursor-stream / class-stream split (P2-A §2.1) plus
the StructuralAlphabet abstraction (P2-A §3.1–§3.4). The five-row CH2
audit:

| # | Surface | Citation | Verdict | Surgical edit (V2) |
|---:|---|---|---|---|
| A.1 | **Class column opacity** — P2-A §2.2 declares `classes: Vec<u8>` as opaque generated ids; §2.2 ¶3: "The generic substrate (`runtime/src/tape/`) stores `Vec<u8>` and exposes `class_at(cursor) -> u8`; it does not match on the byte." This is verbatim Lock 14 substrate cleanliness. | P2-A §2.2 lines 192-211 | **ACCEPT** | none. |
| A.2 | **StructuralAlphabet derivation** — P2-A §3.1 (CSS L4 alphabet enumeration), §3.2 (Sheets `( ) , : { } "`), §3.3 (BBNF-self `= ; | ( ) [ ] { } < > , " /`), §3.4 (empty-alphabet routing). The structural alphabet is per-grammar metadata, not a generic-crate branch. The producer takes the 64-byte set as an argument (§3.1 last bullet). | P2-A §3.1-§3.4 lines 303-356 | **ACCEPT** | none. |
| A.3 | **Scalar-anchor handling** — P2-A §2.1 + §2.3 last paragraph: "the four scalar-anchor classes (`Number`, `Literal`, plus the not-emitted separators) are not held in the SIMD alphabet — they are derived by the parser from the *gaps* in the structural index. The class column on the tape names the parser-event class (not the structural class), and the parser writes it at the offset where the gap-derived scalar begins." The CSS L4 mapping (§3.1) explicitly cites the analogous Ident/Number/Dimension scalar-anchors via the same gap mechanism. | P2-A §2.3 lines 245-257; §3.1 lines 314-318 | **ACCEPT** | none. |
| A.4 | **Sheets doubled-delimiter rule placement** — P2-A §3.2: "Sheets escapes a quote by doubling it (`""`), a fact orthogonal to the event-model: the parser walks `positions` and treats two adjacent s7-class positions inside an open string as a literal-quote payload, not a string-close. … the doubled-delimiter rule is parser state, not generic substrate logic." Correct disposition: the rule lives at the generated per-grammar parser module, not in `runtime/src/tape/`. | P2-A §3.2 lines 331-338 | **ACCEPT** | none. |
| A.5 | **Parser-event class set leak risk** — P2-A §2.3 enumerates the JSON parser-event classes (`ObjectOpen`, `ArrayClose`, `MemberSeparator`, …) and notes they are *opaque generated ids* (e1..e7). However, §2.5 states "`parse_object`/`parse_array`/etc. walk the structural index instead." The verbal naming `parse_object` is a JSON-role token; if the codegen template emits per-grammar walker shells (which it must, per Lock 14), the report must clarify that `parse_object` is the JSON instantiation of `walk_container_class<ContainerClassOrdinal>`. The §2.3 table is correct on the class opacity but the prose §2.5 quietly names JSON-role function symbols. | P2-A §2.3 line 231 + §2.5 lines 280-301 | **ACCEPT-WITH-REVISE** | V2 §2.5 prose: replace `parse_object`/`parse_array` mentions with `walk_container_at_class(ContainerOpenOrdinal)` (the generic template form) plus a footnote that JSON's codegen output names the function `parse_object`; the substrate sees only the ordinal. |
| A.6 | **Class table source-of-truth** — P2-A §2.3 last column ("Mapping site") names per-class mapping rules ("`parse_object` entry: emit e1 at the s1 position", etc.). These mappings are per-grammar codegen output. Lock 14 verification surface (codegen template, NOT hand-written per-grammar code) is not explicitly cited; P2-A §5 LOC table line 4 names "`codegen/src/json_templates/*.rs`" which is a JSON-prefixed template directory. If the template directory is named `json_templates` rather than `event_tape_templates` (or analogous grammar-neutral), the codegen *directory* leaks the grammar name. | P2-A §5 line 436 + reality check vs `skinny/crates/codegen/src/json_templates/` | **ACCEPT-WITH-REVISE** | V2 must rename the touched codegen directory to a grammar-neutral form (e.g. `codegen/src/templates/event_tape/`) or explicitly cite the Lock 14 carve-out for the per-grammar template subdirectory. The current `json_templates/` directory pre-exists; P2-A inherits the violation rather than introducing it, but V2 must name the rename as a same-wave precondition. |

P2-A row count: 6. Verdict: 4 ACCEPT, 2 ACCEPT-WITH-REVISE, 0 REJECT.
**Aggregate disposition: ACCEPT-WITH-REVISE.** The StructuralAlphabet
abstraction is sufficient; scalar-anchor handling generalises cleanly
via the structural-position gap mechanism; the residuals are naming
hygiene at §2.5 and a codegen-directory-name carve-out.

---

### §2.2 — P2-B: Retained class/event grammar proof

P2-B's proof is the `EventGrammar` trait plus two witness `impl`s (JSON
+ Sheets *or* CSS L4). The five-row CH2 audit, with the specific
question on Sheets vs CSS L4 selection:

| # | Surface | Citation | Verdict | Surgical edit (V2) |
|---:|---|---|---|---|
| B.1 | **Trait opacity** — P2-B §1.3 trait sketch carries `STRUCTURAL_CLASS_COUNT: u8`, `type FactId`, `fn admits_fact(id) -> bool`, `fn admits_class(class: u8) -> bool`. No method body matches on a grammar-named arm; the trait deliberately omits `event_kind`/`class_at`/`step_into` to forestall Lock-14-violating method addition (§1.3 last paragraph). | P2-B §1.3 lines 80-129 | **ACCEPT** | none. |
| B.2 | **Sheets choice rationale** — P2-B §3.1 names three Sheets properties that exercise Lock 14: reused punctuation byte (`:` is range operator + arg separator + key/value separator in array literals, the *same byte* meaning different things across grammars; §3.1 bullet 1); escape-equals-delimiter (`""` denotes a literal quote, the strongest Lock-14 exerciser SC-6 §4.4 names; bullet 2); no production parser (the proof shape is enforced by the *absence* of a Sheets parser, blocking accidental production-consumer leak; bullet 3). The doubled-quote escape edge case is genuine Lock-14 stress because it tests whether the opaque-fact-id discipline can carry an *awkward* escape rule (the kind of rule that historically leaks across "generic" code) without forcing the generic substrate to know about it. The rationale is sound. | P2-B §3.1 lines 299-318 | **ACCEPT** | none. |
| B.3 | **Why not CSS L4** — P2-B §3 first paragraph names CSS L4 as a candidate (K = 10 vs Sheets K ≈ 7 vs BBNF-self K ≈ 13) and §6.2 R6 footnote permits the substitution. The case against CSS L4 as primary witness is implicit: CSS L4's `\HHHHHH` escape rule + variable-width range *also* exercises Lock 14 strongly, but its strongest leak risk lives at the `escape_codec_hex_unit` primitive layer (P2-E owns that surface) rather than at the retained-tape contract. Sheets' doubled-quote escape is an *event-model-layer* edge (the parser walks two adjacent quote positions and decides "this is a payload, not a close"); CSS L4's `\HHHHHH` is a *codec-layer* edge (the lexical scanner produces one logical hex unit). For a retained class/event grammar proof, the event-model-layer edge is the more probing exerciser. The rationale is sound. | P2-B §3.1 lines 295-318; cross-ref §6.2 R6 line 574 | **ACCEPT** | none. |
| B.4 | **Witness directory name** — P2-B §1.2 owner-files table proposes `crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs` (or `css_l4_witness/`). The `_witness` suffix is load-bearing (§6.2 R2: "the directory exists *only* for `EventGrammar` proof witnesses and never gains a `scan.rs`, `parser.rs`, `generated.rs`, or `view.rs` sibling"). This is a Lock-14-compliant naming discipline: the grammar-named directory under `runtime/src/grammars/` is normally a Lock-14 violation (Lock 14: "Per-grammar runtime modules … are emitted from a single grammar-agnostic generator template … hand-written per-grammar runtime files are forbidden"). The `_witness` carve-out, gated under `#[cfg(any(test, feature = "proof"))]`, is the proof artefact equivalent of `crates/<grammar>/` per-grammar *declaration* crates that Lock 14 explicitly permits (line 60: "optionally a per-grammar declaration crate (`crates/<grammar>/`)"). The proof binds itself to the `find` invariant at §6.2 R2 — the directory contains *only* `event_grammar_witness.rs` + `mod.rs`. | P2-B §1.2 line 65 + §6.2 R2 line 570 | **ACCEPT** | none. |
| B.5 | **`AnyGrammar` default** — P2-B §1.3 trait sketch: `ValueRef<'tape, 'src: 'tape, G: EventGrammar = AnyGrammar>`. The default type parameter `AnyGrammar` allows the proof to land without flipping every existing consumer to the new generic. Lock 14: if `AnyGrammar` implements `EventGrammar` trivially, it must *not* carry any grammar-specific behaviour. P2-B §4.2 line 5 names this discipline: "the production retained-view consumers at `view.rs:25`, `:71`, `:189` continue to compile against the `AnyGrammar` default." The default is a uniformity-preserving identity, not a JSON-specialised fallback. The proof must explicitly state that `AnyGrammar`'s impl is `STRUCTURAL_CLASS_COUNT = 0; type FactId = (); fn admits_fact(_) = true; fn admits_class(_) = true` (the empty grammar) and not the JSON instance in disguise. | P2-B §1.3 line 124 + §4.2 last bullet line 450 | **ACCEPT-WITH-REVISE** | V2 §1.3 trait sketch: add an explicit `AnyGrammar` impl with the empty-grammar semantics (STRUCTURAL_CLASS_COUNT=0, FactId=(), admits_fact=true, admits_class=true) to disambiguate from "the JSON default." |
| B.6 | **Lock 14 audit commands binding** — P2-B §3.3 names three `rg` audits the proof binds itself to: no generic substrate names a grammar; no generic public role enum; generic substrate stores opaque ordinals only. Each `rg` invocation is a verification surface; admission requires every match be inside `event_grammar.rs`, a witness file, or the proof test file. This is verbatim the Lock 14 verification clause. | P2-B §3.3 lines 358-368 | **ACCEPT** | none. |

P2-B row count: 6. Verdict: 5 ACCEPT, 1 ACCEPT-WITH-REVISE, 0 REJECT.
**Aggregate disposition: ACCEPT.** Sheets is genuinely the strongest
Lock-14 stress for a *retained-tape contract* proof because the
doubled-quote escape probes the opaque-fact-id discipline at the
event-model layer (where CSS L4's stress is at the codec layer, which
P2-E owns). The `AnyGrammar` default needs one revise pass to declare
its empty-grammar semantics explicitly. The proof's compile-only,
no-production-consumer shape forecloses every traditional Lock-14 leak
route.

---

### §2.3 — P2-C: Apache + CITM measured-row admission methodology

P2-C's methodology is JSON-internal by construction — Apache and CITM
are JSON corpora, and the admission produces measured rows for the
existing `real_typed_struct` typed product plane (REDRESS 71 admitted
route). Lock 14 admits JSON-specific methodology if it owns the
JSON-specificity explicitly (i.e. names the methodology as
*data-table-only*, not as substrate-API expansion or per-grammar
match-arm addition in a generic crate). The five-row CH2 audit:

| # | Surface | Citation | Verdict | Surgical edit (V2) |
|---:|---|---|---|---|
| C.1 | **Owner-file scope discipline** — P2-C §4.1 owner-files table names seven paths: `bbnf-bench/src/report.rs` (baseline constant), `bbnf-bench/src/bin/gate.rs` (regression test), `skinny/RESULTS.md`, `skinny/crates/bbnf-bench/target/skv9-w{n}/criterion/` (capture artefacts), `skinny/REDRESS.md` (new entry), `restart/skinny/tranches/sk-v9/HANDOFF.md`, `restart/locks/LOCKS.md` (Lock 14 parent-diff allowance). NONE of the owner files lives under `runtime/`, `bbnf-simd/`, `bbnf-codegen/`, or `parse-that-regex/`. The methodology is row-table data only. Lock 14 substrate cleanliness is preserved. §4.1 "Out of scope (must remain untouched, per REDRESS 91 + alpha-C-redress-digest)" enumerates the no-touch list verbatim. | P2-C §4.1 lines 345-364 | **ACCEPT** | none. |
| C.2 | **Lock 14 parent-diff allowance scope** — P2-C §1 item 5 + §4.1 last row: "A V9 row-table wave that touches *any* additional owner path (e.g., `gate.rs`, `report.rs`, `metadata.rs`) must own a fresh Lock 14 allowance under a new schema identity (`sk-v9-real-typed-w{n}`)." The methodology explicitly carries a Lock 14 schema-identity bump, not a silent expansion. This is the discipline Lock 14 demands for measured-row promotion. | P2-C §1.5 lines 76-84 + §4.1 last row line 354 | **ACCEPT** | none. |
| C.3 | **JSON-internal scope owned explicitly** — P2-C §5 generalisation table classifies each of the 14 `N-direct / NO-GO` rows by host/API schema viability. Apache + CITM are flagged as THIS WAVE; canada is BLOCKED (REDRESS 91 long-decimal mismatch); github_events / gsoc-2018 / instruments are Eligible (after host-schema authoring); random / numbers / unicode_* are Not typed candidates (synthetic / stressor / scanner-correctness probes). The table names what is JSON-specific (the host/API typed schema set) versus what is methodological (the row-table promotion discipline). Lock 14 admission rule satisfied: the methodology *names* the JSON-internal scope. The §5 last paragraph: "It does not generalise to canada … random … numbers … or the unicode/string corpora." | P2-C §5 lines 405-448 | **ACCEPT** | none. |
| C.4 | **Cross-grammar candidate identification** — P2-C §5 does NOT identify non-JSON candidates (e.g. CSS L4 typed-product rows, Sheets typed-product rows). Lock 14 admits the JSON-internal methodology IF the report names what's JSON-specific — which it does at §5 — but the lens question explicitly asks whether the "generalisation to other rows" section identifies non-JSON candidates if any. The answer is NO: §5 enumerates only JSON corpora (the 17-row skinny corpus is JSON). This is acceptable under the lens directive ("Acceptable for the methodology to be JSON-internal IF the report names what's JSON-specific") because the SK-V9 skinny iteration is JSON-only by HANDOFF scope. However, the methodology's generalisation to a future CSS L4 / Sheets typed-product admission should be sketched (one paragraph) so that V2 carries the cross-grammar admission shape forward without re-authoring the methodology. The current §5 is silent on the cross-grammar transposition. | P2-C §5 last paragraph line 448 | **ACCEPT-WITH-REVISE** | V2 §5 closing paragraph: add one paragraph stating "the methodology's seven owner-file shape (baseline constant + gate regression test + RESULTS.md + Criterion capture + REDRESS entry + HANDOFF + Lock 14 allowance) is grammar-neutral by construction: a future CSS L4 / Sheets typed-product row-table wave under SK-V{N>9} would replicate the shape with `<grammar>_real_typed_struct` row ids and a `sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity. The methodology does not require codegen-side changes to admit non-JSON typed product planes." |
| C.5 | **Track-2 oracle independence claim** — P2-C §2.7 names Track 2/oracle as `serde_json::from_slice` and Track 1 as the generated DirectBuild typed parser; both share the offset-tape + structural-projection substrate but no scanner, no parser, no allocator, no codegen template. The independence claim is at the *implementation* level. Lock 14 implication: the `track2` engine is currently a JSON-specific (serde_json) oracle. For a future CSS L4 / Sheets typed-product row-table wave, the Track-2 oracle must be an independent typed parser for *that grammar* — there is no serde_json equivalent for non-JSON grammars. The methodology does not pre-bind a Track-2 oracle shape for non-JSON grammars; this is a hidden Lock-14 constraint that V2 should surface (per the C.4 cross-grammar transposition paragraph). | P2-C §2.7 lines 229-247 | **ACCEPT-WITH-REVISE** | V2 §5 cross-grammar paragraph (per C.4): add a clause acknowledging the Track-2 oracle independence claim is JSON-specific (serde_json is the JSON oracle); a future non-JSON typed admission requires a per-grammar independent typed parser as the Track-2 oracle. The methodology itself does not commit to which engine; the per-grammar selection is the future wave's. |
| C.6 | **PMU evidence scope** — P2-C §2.8 names PMU c/B as parse-only only; the typed probe binary has no typed codepath. This is a JSON-corpus-specific evidence boundary, not a Lock-14 concern. The methodology's admission posture (typed `A / GO` admission with PMU diagnostic_nonproducer_status) generalises to any grammar's typed product plane without changing the Lock-14 substrate. | P2-C §2.8 lines 249-270 | **ACCEPT** | none. |

P2-C row count: 6. Verdict: 4 ACCEPT, 2 ACCEPT-WITH-REVISE, 0 REJECT.
**Aggregate disposition: ACCEPT-WITH-REVISE.** The methodology is
acceptable as JSON-internal because it explicitly names the
JSON-specificity (§5 generalisation table); the residuals are two
prose additions naming the cross-grammar transposition shape so V2
carries the methodology forward without requiring re-derivation under
CSS L4 or Sheets.

---

### §2.4 — P2-D: aarch64 ASM/SIMD opportunities

P2-D enumerates four uncloseable-row interventions: §3 unicode-escape
codec, §4 string-block scanner widening, §5 dead-SIMD-scanner wiring
(structural-bitmap chain), §6 process discipline. JSON-named symbols are
unavoidable in the per-symbol self-time tables (the corpus is JSON), but
the report must frame each *primitive class* generically per P1-V3-B
§1.5. The five-row CH2 audit:

| # | Surface | Citation | Verdict | Surgical edit (V2) |
|---:|---|---|---|---|
| D.1 | **Class-name fidelity in §2 per-row diagnoses** — P2-D §2.1 (y_string_unicode) explicitly labels each hot-leaf row with a `Class` column from P1-V3-B §1.5 (`unicode_escape_hex`, `string_tiny_scan`, `simd_movemask`, `dispatch_value`). §2.2 (unicode_escapes), §2.3 (unicode_mixed), §2.4 (gsoc-2018) follow the same Class-column discipline. The JSON-named symbols in the Source column are per-grammar realisations; the Class column carries the substrate-neutral name. This is verbatim the P1-V3-B §1.5 classifier discipline. | P2-D §2.1-§2.4 lines 134-256 | **ACCEPT** | none. |
| D.2 | **§3 unicode-escape codec class framing** — P2-D §3.4 carries the cross-grammar parameter table (JSON / CSS L4 / JS `\u{}` / TOML `\U`+`\u`) verbatim mirroring P1-V3-B §3.5. §3.4 prose: "The aarch64 primitive body is **identical** across the three (the TBL + range fold doesn't change); the differences live in the surrounding wrapper and the per-grammar `.data` slot. Lock 14 admissible: per SK-V7 A3 §1 the data-vs-code split puts class-LUTs and terminator policy in codegen-emitted `.data` tables, the macro body stays grammar-neutral." The primitive class is named, parameterised, and the data/code split is named at the codegen layer. | P2-D §3.4 lines 346-370 | **ACCEPT** | none. |
| D.3 | **§4 string-block scanner widening framing** — P2-D §4 names the primitive class as `string_block_scan` / `simd_movemask` (P1-V3-B §1.5 entries 6 + 5). The 32-byte widening is per-block, not per-grammar; the same primitive admits CSS L4's `"…"` + `'…'` + `url(…)` span scans + Sheets' `"…"` span scan + BBNF-self's string-literal scans. The report does not name the cross-grammar consumers explicitly — §4 is keyed to gsoc-2018, unicode_mixed (both JSON). For Lock 14 admission, the report must state that the 32-byte primitive is grammar-neutral by construction (the per-span delimiter is the input parameter), with the JSON wiring at `match_string_at_quote_trusted_utf8` as the same-wave consumer. The framing is *implicit* — Lock 14 demands it be *explicit*. | P2-D §4 lines 420-562 | **ACCEPT-WITH-REVISE** | V2 §4 opening: add a Lock-14 framing paragraph: "The 32-byte `string_block_scan` primitive (per P1-V3-B §1.5 class 6) is parameterised by `(terminator: u8, escape: u8, control_limit: u8)`; the per-grammar binding emits the JSON `(b'"', b'\\', 0x20)` instance and the CSS L4 `(b'"' | b'\'', b'\\', 0x20)` instance (separate kernels per delimiter; first-set disjointness drives the codegen branch). The kernel body is identical across the three; only the const-generic delimiter parameters change." |
| D.4 | **§5 SHA3 EOR3 fold grammar-neutrality** — P2-D §5.3.1 frames the SHA3 `veor3q_u8` fold as a vector prefix-XOR ladder replacing the scalar 6-stage XOR chain. The primitive class is bitmap reduction over a 16-byte vector. Lock 16 admissibility allowlist line 76 admits `veor3q_u8` as "abstract primitive: 3-way XOR — applies to ANY grammar's mask reduction" (per LOCKS.md Lock 16). The §5.3.1 framing inherits the grammar-neutrality from the allowlist. §5.4 prose: "Per Lock 1 §1.3 sentence … the union-substrate collapses the bitmap and the tape into one queryable object." The substrate is the union tape, not a JSON-specific artefact. EOR3 is grammar-neutral by Lock 16 citation + abstract-primitive lift. | P2-D §5.3.1 lines 636-668 + §5.4 lines 707-724 | **ACCEPT** | none. |
| D.5 | **§3, §4, §5 cross-grammar admission language** — P2-D §3.4 carries explicit cross-grammar admission (the parameter table). §4 inherits cross-grammar admission via the string_block_scan class (P1-V3-B §1.5 class 6) but does not enumerate it (per D.3). §5 frames the structural-bitmap chain as union-substrate work (per P2-A §3) with explicit CSS L4 / Sheets / BBNF-self admission via P2-A's StructuralAlphabet abstraction; §5 does not duplicate the admission language but inherits it via the union-substrate dependency. §5.5 material differential vs REDRESS 28 + 33 is correct on the consumer-side scope but does not restate cross-grammar admission. The aggregate disposition: §3 is fully Lock-14-admitted; §4 inherits admission implicitly (per D.3 revise); §5 inherits admission via P2-A. | P2-D §3.4 lines 346-370; §4 ad-hoc; §5 lines 565-749 | **ACCEPT** (modulo D.3 revise) | inherits D.3 revise. |
| D.6 | **Process discipline (§6) grammar-neutrality** — P2-D §6.2 enumerates aarch64 primitives lacking a checkasm gate (unescape_uxxxx, scan_string_special_block, digit_mac, byte_context, etc.). The checkasm discipline is grammar-neutral (every primitive ships scalar reference + checkasm differential + ABI shim + fault recovery + cycle-counter binding). Lock 14: the §6.3 five-invariant gate restated from SK-V7 A3 §1 is the grammar-neutral admissibility surface; the per-primitive status table at §6.2 is per-primitive (not per-grammar). | P2-D §6.2-§6.4 lines 760-839 | **ACCEPT** | none. |

P2-D row count: 6. Verdict: 5 ACCEPT, 1 ACCEPT-WITH-REVISE, 0 REJECT.
**Aggregate disposition: ACCEPT-WITH-REVISE.** §3 (unicode-escape
codec) carries explicit cross-grammar admission language verbatim
mirroring P1-V3-B §3.5; §5 (structural-bitmap chain) inherits
admission via P2-A's StructuralAlphabet abstraction; §4
(string-block widening) inherits admission implicitly from
P1-V3-B §1.5 class 6 but Lock 14 demands the framing be explicit —
V2 must add the one-paragraph Lock-14 framing to §4 opening (per D.3).

---

### §2.5 — P2-E: `escape_codec_hex_unit` codec primitive

P2-E is the focal CH2 candidate: the primitive class P1-V3-B §3.5
named verbatim, with five const-generic bindings across JSON / CSS L4
/ JS / TOML. The lens question: (a) is the parameter binding
codegen-emitted (not runtime switch), and (b) is the same-wave CSS L4
consumer sketch sufficient demonstration? The six-row CH2 audit:

| # | Surface | Citation | Verdict | Surgical edit (V2) |
|---:|---|---|---|---|
| E.1 | **Codegen-emitted parameter binding** — P2-E §2 last paragraph: "Per `feedback_pluggable_components` + Lock 16, the codegen binds at compile time. The bbnf grammar declares the escape rule: [grammar example with → escape_codec_hex_unit{4, Pair, FixedWidth, Utf8}] … The codegen emits one specialised Rust kernel per `escape_codec_hex_unit{…}` binding tuple. Specialisation parameters are `const` generics on the NEON intrinsic body. The dispatcher in `runtime/src/grammars/<grammar>/generated.rs` calls the specific binding by name; the inliner sees the constant parameters and prunes every branch the binding has fixed at compile time." This is verbatim codegen-emitted binding, not runtime switch. The const-generic parameters at compile time + LTO inline at link time + per-binding kernel specialisation. The five JSON / CSS L4 / JS / TOML `\u` / TOML `\U` bindings emit five specialisations. | P2-E §2 lines 114-166 | **ACCEPT** | none. |
| E.2 | **Cross-grammar parameter table fidelity** — P2-E §2 table mirrors P1-V3-B §3.5 verbatim plus extends TOML to two bindings (`\u` fixed-4 + `\U` fixed-8). The four-axis parameterisation (hex_digit_count, surrogate_join_policy, terminator_policy, target_encoding) matches the P1-V3-B canonical vocabulary exactly. The grammar surface (`.bbnf` source declaring `→ escape_codec_hex_unit{4, Pair, FixedWidth, Utf8}`) is the Lock-14-admitted declarative surface. The semantics live in the grammar source, not in branching code in any generic crate. | P2-E §2 lines 114-128; cross-ref P1-V3-B §3.5 lines 904-923 | **ACCEPT** | none. |
| E.3 | **Same-wave CSS L4 consumer sketch** — P2-E §4.2 (lines 391-427) sketches the CSS L4 consumer: a codegen-emitted `decode_css_unicode` function that calls `bbnf_simd::escape_codec::decode_variable::<Range::new(1, 6), SurrogatePolicy::None, Terminator::WhitespaceOrNonHex>(bytes, slash)`. The sketch ships the codegen template + scalar reference + a unit test in the same wave; the CSS L4 SIMD body lands when CSS-side benches demand it. §4.2 closing: "the minimum same-wave consumer needed to refute the 'JSON-overfit' CH2 GENERALITY charge: a second grammar's parser explicitly calls the same kernel under a different parameter binding." The sketch is *minimum* — codegen template + unit test, not a full CSS L4 SIMD body — but it is sufficient per the CH2 lens because it demonstrates the parameter binding lands in the codegen-emit layer (not a hand-written CSS-side hack) and the kernel call site exists in a non-JSON grammar's source path. | P2-E §4.2 lines 391-427 | **ACCEPT** | none. |
| E.4 | **CSS L4 wave-coupling** — P2-E §4.2 acknowledges CSS L4 is "mid-wave per project memory `css-typed-codegen`" and the consumer call site lives at `bbnf-css/src/tokenizer/escape.rs`. The wave-coupling discipline: the CSS L4 consumer sketch lands as a *codegen-emitted file* in the same SK-V9 wave commit; it depends on the CSS L4 grammar source declaring the unicode_escape rule with `→ escape_codec_hex_unit{Range(1,6), None, WhitespaceOrNonHex, Utf8}`. If the CSS L4 `.bbnf` source is not yet authored at SK-V9 wave time, the sketch is a *pure scaffold* (codegen template + unit test under `#[cfg(test)]`) without a live CSS L4 production parser consumer. The wave landing this primitive must either author the CSS L4 unicode_escape rule (one BBNF line) or land the sketch as scaffold-only. P2-E §4.2 does not explicitly state which; the ambiguity is admissible but the wave plan (P3 owns) must pick one. | P2-E §4.2 lines 391-427 + cross-ref P2-E §7.1 LOC table line 567 ("CSS L4 consumer sketch ~40 LOC") | **ACCEPT-WITH-REVISE** | V2 §4.2 closing: name the scaffold vs production-consumer choice explicitly. Either: (a) "the SK-V9 wave authors the CSS L4 unicode_escape rule in `grammars/css_l4.bbnf` with `→ escape_codec_hex_unit{Range(1,6), None, WhitespaceOrNonHex, Utf8}` and the codegen-emitted consumer is a production path under the CSS L4 tokeniser"; or (b) "the SK-V9 wave lands a `#[cfg(test)]`-gated scaffold demonstrating the const-generic binding compiles and passes a unit test; the CSS L4 production tokeniser wires the kernel in a later CSS-side wave." Either is Lock-14-admissible; the choice must be named. |
| E.5 | **Layer-1 directory shape** — P2-E §3.1 places the primitive at `crates/bbnf-simd/src/aarch64/escape_codec/` (directory module per `feedback_directory_modules`) with siblings `mod.rs`, `scalar.rs`, `hex_x4_neon.rs`, `hex_x8_neon.rs`, `hex_variable_neon.rs`, `surrogate_join.rs`. The directory name `escape_codec/` is grammar-neutral (the canonical class name from P1-V3-B §3.5); the per-binding files are named by parameter (`hex_x4`, `hex_x8`, `hex_variable`), not by grammar (`hex_json`, `hex_css_l4`). This is verbatim Lock-14 substrate cleanliness: the primitive crate's directory and file names carry primitive-class identity, not grammar identity. | P2-E §3.1 lines 200-219 | **ACCEPT** | none. |
| E.6 | **Surrogate-pair JSON-specificity** — P2-E §2.1 acknowledges JSON is the only grammar whose `surrogate_join_policy = Pair` branch fires. The `Pair` parameter guards a *control-flow gate* in the kernel; under `SurrogatePolicy::None` or `RangeCheck`, the gate is dead-code and constant-folds out. This is Lock-14-compliant: the JSON-specific surrogate-pair join is a *parameter binding* that codegen emits for the JSON instance; it is not a `match grammar { Json => surrogate_join }` arm in a generic crate. The other four grammars' bindings emit kernels without the join branch. The const-generic parameter is the discriminant. | P2-E §2.1 lines 167-178 + §3.4 lines 295-318 | **ACCEPT** | none. |

P2-E row count: 6. Verdict: 5 ACCEPT, 1 ACCEPT-WITH-REVISE, 0 REJECT.
**Aggregate disposition: ACCEPT.** The codegen-emitted parameter
binding is verbatim Lock-14-compliant (const generics + per-binding
kernel specialisation + LTO inline; not a runtime switch). The CSS L4
same-wave consumer sketch is *sufficient* under the CH2 lens because
it demonstrates a non-JSON grammar's parser explicitly calls the same
kernel under a different parameter binding via codegen-emitted glue,
not hand-written CSS-side code. The residual is naming whether the
SK-V9 wave authors the CSS L4 `.bbnf` rule (production consumer) or
lands a `#[cfg(test)]`-gated scaffold (proof-only) — V2 must pick.

---

### §2.6 — P2-F: SOTA teardown M5 Max

P2-F is a per-competitor architecture teardown keyed to the M5 Max
PMU evidence. It is JSON-internal by domain (the competitors named —
sonic-rs, simdjson, yyjson, asmjson, RapidJSON, serde_json — are JSON
parsers; the corpus is the 17-row skinny JSON suite). Lock 14 admits
JSON-internal teardown IF the report identifies grammar-neutral
patterns where extractable. The five-row CH2 audit:

| # | Surface | Citation | Verdict | Surgical edit (V2) |
|---:|---|---|---|---|
| F.1 | **JSON-domain scope owned** — P2-F §1 carries the per-competitor table for sonic-rs / simdjson / yyjson / asmjson / RapidJSON / serde_json on M5 Max. Every competitor is a JSON parser; the corpus is JSON. The teardown does not claim cross-grammar applicability of the *competitors*; it claims cross-grammar applicability of the *architectural patterns* (per §5). The JSON-internal scope is acknowledged implicitly (the competitor field is JSON). Lock 14 acceptable: JSON-internal teardown is admissible when the report's claims do not leak JSON-specificity into the architectural-pattern extraction. | P2-F §1 lines 20-48 | **ACCEPT** | none. |
| F.2 | **§5 architecture lessons grammar-neutrality** — P2-F §5 names one pattern per competitor: §5.1 simdjson "consume the index that scan_structurals produces" (cross-grammar: any grammar's structural index is a candidate substrate union per P2-A; the pattern is the *fed-index stage-2* shape, grammar-neutral); §5.2 sonic-rs "cost-fact-gated NEON tiny-string equality at the dispatch site" (cross-grammar: any grammar's keyword set + key-recognition admits the tiny-string primitive; the cost-fact threshold is per-grammar metadata); §5.3 yyjson "force-inline fusion of \uXXXX decode into the string-walk loop" (cross-grammar: any grammar with an escape codec admits the fusion; the codec class is `escape_codec_hex_unit` per P1-V3-B §3.5); §5.4 asmjson "bounded-stack DPDA with PC-as-state direct threading" (cross-grammar: any grammar admits the DPDA shape; the CollapsedStage admission is grammar-neutral per Lock 16 line 92 abstract-primitive lift); §5.5-§5.6 RapidJSON / serde_json named as non-applicable (scalar floor + strict scalar floor). | P2-F §5 lines 328-389 | **ACCEPT** | none. |
| F.3 | **§5.3 yyjson fusion pattern Lock-14 framing** — §5.3 names the yyjson fusion pattern: "codegen-emitted fusion of the 4-nibble decode + surrogate-pair handler directly into the string-walk hot leaf, with the codepoint accumulator writing directly into the field-fact / offset-tape sink. This is the P2-E unicode-codec target." The fusion is codegen-emitted (Lock-14-compliant) and the unicode-codec class is the same `escape_codec_hex_unit` P2-E parameterises. The cross-grammar lift: any grammar with an escape codec admits the fusion pattern; the fusion target site is per-grammar codegen output. Lock 14 verbatim: "Adding a new grammar is a config + grammar-source change with NO code change in any generic or other-grammar crate." The fusion is *emitted per grammar*, not branched on. | P2-F §5.3 lines 357-367 + cross-ref P2-E §2 lines 114-166 | **ACCEPT** | none. |
| F.4 | **§7 SOTA path cross-grammar admission** — §7.1 Intervention I (P2-A union substrate): "Architecture neutrality. The codec is grammar-neutral by Lock 14: JSON's `\uXXXX`, CSS L4's `\HEX*` escape, BBNF-self's `\u` literals all share the same primitive shape." Wait — §7.1 covers union substrate; the Lock-14 framing quoted is at §7.2 Intervention II. §7.2 explicit cross-grammar admission language: "The codec is grammar-neutral by Lock 14: JSON's `\uXXXX`, CSS L4's `\HEX*` escape, BBNF-self's `\u` literals all share the same primitive shape. P2-F admits this as a per-grammar template surface over a grammar-neutral hex-decode primitive." §7.1 (union substrate) cites P2-A §3.1-§3.4 by reference for the StructuralAlphabet abstraction. §7.3 (Intervention III cost-fact tiny-string + bitmap_next_set_bit) does not restate cross-grammar admission but inherits it via Lock 16 line 75 abstract-primitive lifts. | P2-F §7.2 lines 500-503; cross-ref §7.1 inherits P2-A; §7.3 inherits Lock 16 | **ACCEPT** | none. |
| F.5 | **§4 asmjson teardown grammar-neutrality** — §4 is the most explicitly grammar-neutral section of P2-F: asmjson is named as a non-anchored sidecar planning signal on M5 Max, and its architectural patterns (9-state DPDA with PC-as-state direct threading, the `BYTE_CLASS_FROM_EQ_SET_64 + BITMAP_NEXT_SET_BIT + FSM_DISPATCH_THREADED` primitive vocabulary per SK-V7-A2 §6) are cited as *architecture-pattern evidence* + *cross-ISA architecture lift* — explicitly not JSON-specific. §4 closing: "This is the *line that distinguishes the asmjson architecture lift from the asmjson clone failure mode* (SK-V7-A2 §6 final paragraph)." The clone-failure mode is exactly the JSON-overfit risk Lock 14 prevents; the architecture-lift is the grammar-neutral pattern. P2-F §4 names both sides explicitly. | P2-F §4 lines 291-326 | **ACCEPT** | none. |
| F.6 | **§6 DAV1D process discipline grammar-neutrality** — §6 names the dav1d four-tuple commit shape (scalar reference + checkasm test + hot-path consumer + ABI-hardened ASM) as the gold standard. The discipline is grammar-neutral by construction (it is per-primitive, not per-grammar). bbnf-simd's checkasm-parity harness is "the closest in-tree analogue to the dav1d discipline." The discipline rule is non-negotiable for S-P3 admission. | P2-F §6 lines 391-437 | **ACCEPT** | none. |

P2-F row count: 6. Verdict: 6 ACCEPT, 0 ACCEPT-WITH-REVISE, 0 REJECT.
**Aggregate disposition: ACCEPT.** P2-F is the cleanest Lock-14
disposition of the cohort: JSON-internal scope is acknowledged
implicitly via the JSON-competitor field; the architecture-lesson
extraction at §5 is explicitly grammar-neutral on every named pattern;
§4 asmjson explicitly names the architecture-lift vs clone-failure
distinction; §6 process discipline is per-primitive (grammar-neutral
by construction); §7 SOTA path inherits Lock-14 framing from P2-A,
P2-D, P2-E by reference.

---

## §3 — Aggregate verdict

| Report | ACCEPT | ACCEPT-WITH-REVISE | REJECT | Aggregate |
|---|---:|---:|---:|---|
| P2-A union event-model | 4 | 2 | 0 | ACCEPT-WITH-REVISE |
| P2-B retained grammar proof | 5 | 1 | 0 | ACCEPT |
| P2-C Apache + CITM admission | 4 | 2 | 0 | ACCEPT-WITH-REVISE |
| P2-D aarch64 ASM opportunities | 5 | 1 | 0 | ACCEPT-WITH-REVISE |
| P2-E `escape_codec_hex_unit` codec | 5 | 1 | 0 | ACCEPT |
| P2-F SOTA teardown M5 Max | 6 | 0 | 0 | ACCEPT |
| **Total** | **29** | **7** | **0** | **ACCEPT-WITH-REVISE** |

ACCEPT rate: 29 / 36 = **80.6%**. No REJECT verdicts; no row carries a
Lock-14 violation that disqualifies the row from V1 admission. Seven
ACCEPT-WITH-REVISE rows name specific surgical edits V2 must fold; the
edits are prose hygiene + scaffold/production-consumer naming + one
trait-default explicit declaration. None requires architectural
reshape.

CH2 GENERALITY verdict on the S-P2 V1 cohort: **ACCEPT-WITH-REVISE**.
The cohort respects Lock 14 in every load-bearing surface (substrate
cleanliness, primitive-class vocabulary, codegen-emitted per-grammar
binding, grammar-neutral pattern extraction, JSON-internal scope
acknowledged where present). The seven revise items are surface
hygiene that V2 folds in <50 LOC of prose edits across the six
reports.

The cohort does not require a second cycle on CH2 grounds; the convergence
criterion (CHALLENGE returns ≥95% ACCEPT for two consecutive cycles) is
not yet met (80.6% < 95%), but the residual gap is purely the
ACCEPT-WITH-REVISE items, each of which has a specific surgical edit
that closes it without invalidating the row's load-bearing claim.

---

## §4 — Specific Lock-14 leaks requiring V2 fold

The seven surgical edits, ordered for V2 dispatch:

1. **P2-A §2.5 prose** — replace `parse_object`/`parse_array` JSON-role
   function names with generic template form (`walk_container_at_class
   (ContainerOpenOrdinal)`) plus footnote naming JSON's codegen output
   symbol. The substrate sees only the ordinal. (Disposition row A.5.)

2. **P2-A §5 codegen-directory** — name the rename of
   `codegen/src/json_templates/` to a grammar-neutral form
   (`codegen/src/templates/event_tape/`) or cite the Lock 14 carve-out
   for per-grammar template subdirectories. V2 must declare the rename
   as a same-wave precondition. (Disposition row A.6.)

3. **P2-B §1.3 trait sketch** — add an explicit `AnyGrammar` impl
   declaring the empty-grammar semantics (`STRUCTURAL_CLASS_COUNT = 0;
   type FactId = (); fn admits_fact(_) = true; fn admits_class(_) =
   true`). Disambiguate from "the JSON default in disguise."
   (Disposition row B.5.)

4. **P2-C §5 closing paragraph** — add one paragraph stating the
   methodology's seven owner-file shape is grammar-neutral by
   construction: a future CSS L4 / Sheets typed-product row-table wave
   under SK-V{N>9} replicates the shape with `<grammar>_real_typed_struct`
   row ids and a `sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity.
   Add a clause acknowledging the Track-2 oracle independence claim is
   JSON-specific (serde_json is the JSON oracle); a future non-JSON
   typed admission requires a per-grammar independent typed parser as
   the Track-2 oracle. (Disposition rows C.4 + C.5.)

5. **P2-D §4 opening** — add a Lock-14 framing paragraph: "The 32-byte
   `string_block_scan` primitive (per P1-V3-B §1.5 class 6) is
   parameterised by `(terminator: u8, escape: u8, control_limit: u8)`;
   the per-grammar binding emits the JSON `(b'"', b'\\', 0x20)`
   instance and the CSS L4 `(b'"' | b'\'', b'\\', 0x20)` instance
   (separate kernels per delimiter; first-set disjointness drives the
   codegen branch). The kernel body is identical across the three;
   only the const-generic delimiter parameters change."
   (Disposition row D.3.)

6. **P2-E §4.2 closing** — name the scaffold-vs-production-consumer
   choice explicitly for the CSS L4 same-wave consumer. Either: (a)
   author the CSS L4 unicode_escape rule in `grammars/css_l4.bbnf` with
   `→ escape_codec_hex_unit{Range(1,6), None, WhitespaceOrNonHex,
   Utf8}` and emit the codegen consumer as a production path under the
   CSS L4 tokeniser; or (b) land a `#[cfg(test)]`-gated scaffold
   demonstrating the const-generic binding compiles and passes a unit
   test, with the CSS L4 production tokeniser wiring the kernel in a
   later CSS-side wave. Either is Lock-14-admissible; the choice must
   be named. (Disposition row E.4.)

7. **(Cohort-wide) Cross-reference table** — V2 should add a one-line
   cross-reference at the top of P2-A / P2-D / P2-F naming the
   P1-V3-B §1.5 primitive-class vocabulary as the load-bearing
   canonical surface. P2-C / P2-E already cite it; P2-A names the
   union event-model in terms of StructuralAlphabet (P1-V3-B §1.5
   class 3 + class 6) but does not anchor by §1.5 path. P2-D cites
   P1-V3-B §3.5 but not §1.5. P2-F inherits the vocabulary by
   reference but does not anchor. The cross-reference is hygiene; it
   does not change any disposition. (Cohort discipline; non-blocking
   for V1 admission.)

The seven edits compose to ≤50 LOC of prose changes distributed across
the six reports. None of the seven invalidates a load-bearing claim;
each clarifies a Lock-14 surface that V1 left implicit. V2 dispatch
should fold these edits before re-dispatching the CHALLENGE wave; the
re-dispatched CH2 cycle should converge to ≥95% ACCEPT (the seven
ACCEPT-WITH-REVISE rows flip to ACCEPT once the surgical edits land).

---

## §5 — Sources

- `restart/locks/LOCKS.md:60` — Lock 14 verbatim (substrate-neutral;
  zero `match grammar` arms in generic crates; verification commands).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
  §1.5 lines 104-158 (canonical primitive-class vocabulary: seven
  classes); §3.5 lines 895-931 (`escape_codec_hex_unit` cross-grammar
  parameter table).
- `restart/prompts/ORCHESTRATOR.md` §3W lines 75-102 (CH2 lens
  registry); §8 voice + non-negotiables (no JSON code in generic
  crates — CH2 enforcement).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
  end-to-end; specifically §2.1-§2.5 lines 150-301 (cursor/class
  split), §3 lines 303-356 (cross-grammar admission), §5 lines
  426-450 (LOC + risk).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
  end-to-end; specifically §1.3 lines 78-129 (trait sketch), §3.1-§3.3
  lines 299-368 (Sheets vs CSS L4 rationale + Lock 14 audit commands),
  §4 lines 376-465 (REDRESS 60-72 differential), §5.1 lines 510-537
  (same-wave-consumer rule formal disposition).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
  end-to-end; specifically §1 lines 22-93 (REDRESS 91 differential),
  §2 lines 95-289 (artefact set), §4.1 lines 344-364 (owner files),
  §5 lines 405-448 (generalisation to other rows), §6 lines 450-505
  (pre-block risk + REDRESS citations).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
  end-to-end; specifically §2 lines 127-256 (per-row diagnoses with
  Class column), §3.4 lines 346-370 (cross-grammar parameter table),
  §4 lines 420-562 (string-block widening), §5.3.1 lines 636-668
  (SHA3 EOR3 fold), §5.5 lines 727-748 (REDRESS 28 + 33 material
  differential).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
  end-to-end; specifically §2 lines 114-198 (cross-grammar parameter
  table + codegen-emitted binding), §3.1 lines 200-219 (Layer-1
  directory shape), §4.2 lines 391-427 (CSS L4 consumer sketch),
  §5 lines 441-472 (REDRESS 82 differential), §7 lines 551-633 (LOC
  + risk + checkasm parity).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
  end-to-end; specifically §1 lines 20-48 (per-competitor M5 Max
  landscape), §4 lines 291-326 (asmjson architecture-lift vs
  clone-failure), §5 lines 328-389 (per-competitor architecture
  lessons), §6 lines 391-437 (DAV1D discipline), §7 lines 441-555
  (SOTA path with cross-grammar admission language at §7.1-§7.3).

End of disposition.
