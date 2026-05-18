# SK-V9 S-P2 CHALLENGE V2 — CH2 GENERALITY

Pass: S-P2 Research. Cycle: V2. Lens: CH2 GENERALITY (Lock 14).
Date: 2026-05-18.
Predecessor: `restart/skinny/tranches/sk-v9/research/p2/hardening/V1/CH2.md`
(80.6%, 7 ACCEPT-WITH-REVISE, 0 REJECT).
Fold spec: `HARDENING-S-P2-V1-CONSOLIDATED.md` F5 — Lock-14 surgical
reframings (7 edits) applied across P2-A/B/C/D/E.
Anchor: `restart/locks/LOCKS.md:60` Lock 14 verbatim. Canonical
primitive-class vocabulary: `skv9-p1-v3-B-xctrace-time-profiler.md`
§1.5 (seven-class taxonomy) + §3.5 (`escape_codec_hex_unit` parameter
table).

This document verifies the V2 fold landed each of the seven V1
ACCEPT-WITH-REVISE Lock-14 items, that the fold introduced no new
Lock-14 leak, and that primitive-class tags remain consistent with the
P1-V3-B §1.5 canonical vocabulary.

---

## §1 — V1-REVISE resolution (per item)

The seven V1 surgical edits from V1 CH2 §4, with the V2 disposition of
each. The reports carrying the fold are the in-place `skv9-p2-{A..F}.md`
files (timestamps 17:42–17:47, post-dating the consolidation at 17:39);
each fold-carrying report declares the fold in a `§0 — V2 fold` note or
a `revision:` frontmatter field.

### §1.1 — Item 1: P2-A §2.5 JSON-role function naming — RESOLVED

V1 row A.5 required: replace `parse_object`/`parse_array` JSON-role
function names in §2.5 prose with the generic template form
(`walk_container_at_class(ContainerOpenOrdinal)`) plus a footnote
naming JSON's codegen output symbol.

V2 disposition: **FOLDED — clean.** P2-A §2.5 (lines 380–399) now
carries an explicit "Generic-template / JSON-codegen-output naming
convention" paragraph: the prose names walker functions in the generic
primitive-class template form (`walk_container_at_class(...)`,
`walk_scalar_anchor(ScalarAnchorClass)`); the JSON codegen output
symbols (`parse_object`, `parse_array`, `parse_string`, `parse_value_at`,
`parse_member`, `parse_container_next`) are named explicitly as the
per-grammar realisation. The closing clause restates Lock 14 verbatim:
"The generic substrate (`runtime/src/tape/`) sees only the primitive
class + class ordinal; the JSON-role symbol names live entirely inside
the JSON-grammar codegen output." The class-table at §2.3 (lines
300–301) is likewise reframed: `walk_container_at_class(ContainerOpen
Ordinal)` / `walk_container_close(ContainerCloseOrdinal)` carry an
inline parenthetical naming the JSON codegen output. The §2.2 prose at
line 183 still names the JSON symbols, but in a list that is explicitly
introduced ("decide whether to dispatch to …") as the JSON realisation
of the class ordinal — no longer a generic-substrate claim. The edit
is the V1-named edit verbatim.

### §1.2 — Item 2: P2-A §5 `json_templates/` codegen-directory — RESOLVED

V1 row A.6 required: either rename `codegen/src/json_templates/` to a
grammar-neutral form, OR cite the Lock 14 carve-out for the per-grammar
template subdirectory, naming the rename as a same-wave precondition if
the rename route is chosen.

V2 disposition: **FOLDED — clean (carve-out route).** P2-A §5 (lines
761–780) carries an explicit "Codegen-directory naming carve-out
(Lock 14)" paragraph. The fold takes the carve-out route (one of the
two V1-admitted options): `json_templates/` is named as a per-grammar
codegen-emitted instance — "the codegen analogue of a per-grammar
declaration crate (`crates/<grammar>/`), which Lock 14 explicitly
permits." The paragraph distinguishes the generic mechanism
(`codegen/src/lib.rs` + shared template infrastructure) from the
per-grammar instantiation (`json_templates/`), names the parallel
hypothetical `css_l4_templates/`, and binds itself to the Lock 14
verification gate: "`rg` for grammar names in *generic* crate paths is
satisfied because `json_templates/` is a per-grammar codegen-instance
directory, not a generic-crate path." The closing sentence states
explicitly "No directory rename is a precondition." This is a complete
and Lock-14-coherent resolution of A.6: the V1 row named the rename as
a *precondition only if the rename route is taken*; the carve-out route
has no precondition, and the fold names that fact. No new leak.

Note on consistency: the carve-out reasoning here is the same shape as
P2-B's `*_witness/` directory carve-out (V1 row B.4) and P2-E's
`escape_codec/` grammar-neutral primitive-directory naming (V1 row E.5)
— the cohort treats per-grammar codegen-instance directories and
per-grammar declaration crates as the Lock 14-permitted (c)-surface,
and grammar-neutral primitive crates as substrate. The carve-out is
internally consistent across the cohort.

### §1.3 — Item 3: P2-B explicit `AnyGrammar` empty-grammar declaration — RESOLVED

V1 row B.5 required: add an explicit `AnyGrammar` impl to the §1.3
trait sketch declaring the empty-grammar semantics
(`STRUCTURAL_CLASS_COUNT = 0`, `FactId = ()`, `admits_fact = true`,
`admits_class = true`) to disambiguate from "the JSON default in
disguise."

V2 disposition: **FOLDED — clean, and exceeds the V1 ask.** P2-B grew a
dedicated new section §1.5 "`AnyGrammar` — the empty-grammar default
instance" (lines 200–304). The fold delivers more than the V1-named
edit: rather than `FactId = ()`, it uses an *uninhabited* `FactId`
(`enum AnyGrammarFactId {}`), which is a stronger empty-grammar
construction — any code path consuming a `FactRecord<AnyGrammar>` is
statically dead because no `AnyGrammarFactId` value can be constructed.
`STRUCTURAL_CLASS_COUNT = 0` is declared, `admits_class` is a `const fn`
returning the empty-set answer, and the section explicitly states
(lines 288–294) that `AnyGrammar` is the `EventGrammar`-side
counterpart of the `EagerTape` backend-shape and is "not a JSON
default in disguise" — `LayoutFacts.backend_shape` remains the
operational discriminant. The §0 fold note and the S1/S5 cost-table
rows confirm the `AnyGrammar` instance is part of the trait-declaration
slice with a defined revert protocol. The substitution of an
uninhabited FactId for the V1-suggested `()` is a strictly stronger
Lock-14 disambiguation, not a deviation — it makes the "empty grammar"
claim type-level provable rather than convention-level.

### §1.4 — Item 4: P2-C cross-grammar transposition + Track-2 oracle — RESOLVED

V1 rows C.4 + C.5 required: (a) a paragraph stating the seven owner-file
methodology shape is grammar-neutral by construction, with future
non-JSON typed row-table waves replicating it under
`<grammar>_real_typed_struct` row ids and a
`sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity; (b) a clause
acknowledging the Track-2 oracle independence claim is JSON-specific
(serde_json is the JSON oracle), and a future non-JSON typed admission
requires a per-grammar independent typed parser.

V2 disposition: **FOLDED — clean, both clauses.** P2-C grew a new
section §5.1 "Cross-grammar transposition: the generic pattern" (lines
492–530): the seven owner-file shape is named "grammar-neutral by
construction"; the abstract generalisation rule is stated; the future
`<grammar>_real_typed_struct` row id form and the
`sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity are named
verbatim; an illustrative four-row cross-grammar table (CSS L4 / Sheets
/ TOML / BBNF-self) gives concrete Track 1 source + Track 2 oracle
candidates per grammar, explicitly tagged "illustrative; not in scope
for SK-V9 skinny." The methodology body is declared to transpose
verbatim with no codegen-side change. The Track-2-oracle clause is
folded into §2.7 (lines 280–296): "`serde_json` is the JSON oracle and
has no cross-grammar equivalent — there is no `serde_json` for CSS L4
or Sheets." The fold names the structural-independence criterion as the
grammar-neutral *invariant* while the oracle *shape* is
grammar-dependent — a precise Lock-14 framing. P2-C's §0-equivalent
F5 note at lines 691–705 records both edits against CH2 §2.3 rows
C.4 + C.5. Both V1-named clauses are present and Lock-14-coherent.

### §1.5 — Item 5: P2-D §4 string-block-widening Lock-14 framing — RESOLVED

V1 row D.3 required: add a Lock-14 framing paragraph at §4 opening
naming the `string_block_scan` primitive as parameterised by
`(terminator, escape, control_limit)`, with per-grammar binding
emitting the JSON and CSS L4 instances and the kernel body identical
across grammars.

V2 disposition: **FOLDED — clean, with a stronger parameter set.** P2-D
§4 opening (lines 522–532) carries the Lock-14 framing paragraph: "Per
Lock 14 (substrate-neutral primitive vocabulary) and SK-V7 A3 §1 the
data-vs-code split puts the four scan parameters (`terminator`,
`escape`, `control_limit`, `non_ascii_threshold`) in the
codegen-emitted `.data` slot; the 32-byte NEON block-scan body stays
grammar-neutral." The widening is admitted as a named Lock-14
primitive-vocabulary entry — `scan_string_special_block_32` — "not as a
JSON-specific helper." The same-wave JSON consumer
(`match_string_at_quote_trusted_utf8`) is named as the *first*
consumer; CSS L4 / Sheets / BBNF-self are named as later-wave
admissions of the same primitive against their own `.data` parameter
rows. The fold differs from the V1-suggested wording in two
non-substantive ways: it carries a four-parameter tuple (V1 named
three; the actual `StringSpecialBlock` signature at §4.1 confirms the
fourth, `non_ascii`/`control_limit`, is real) and it routes the
data/code split through the `.data` slot rather than const-generics.
Both are faithful to Lock 14 — the V1 row's intent ("the framing be
explicit; the kernel body identical; per-grammar binding at the data
layer") is satisfied. The reframe to a `.data`-slot split rather than
const-generics is internally consistent with P2-D §3.4's existing
SK-V7 A3 §1 data-vs-code citation.

### §1.6 — Item 6: P2-E scaffold-vs-production-consumer naming — RESOLVED

V1 row E.4 required: name the scaffold-vs-production-consumer choice
explicitly for the CSS L4 same-wave consumer — either author the CSS L4
`unicode_escape` rule and emit a production-path consumer, or land a
`#[cfg(test)]`-gated scaffold.

V2 disposition: **FOLDED — clean, choice made explicitly.** P2-E §4.2
(lines 451–467) names the choice: "the CSS L4 consumer ships explicitly
as a **scaffold**, not a production consumer." The SK-V9 wave lands a
`#[cfg(test)]`-gated test in `bbnf-css/tests/` demonstrating the
const-generic binding compiles and passes a unit test against the
scalar reference; the production tokeniser
(`bbnf-css/src/tokenizer/escape.rs`) wires the kernel in a later
CSS-side wave once the CSS L4 `.bbnf` source authors the
`unicode_escape` rule with the named binding tuple. The fold goes
further than the V1 ask by adding a crisp three-way taxonomy at §4.1
(lines 339–358): one **production consumer** (the already-wired x4 JSON
path at `lib.rs:402` — note this is the F1 wiring fix from the
consolidation, correctly inherited) and two **scaffolds** (CSS L4 +
TOML). §4.4 separately declares the TOML `\u`/`\U` bindings have no
production consumer this wave. The S2–S9 cost table tags each slice's
consumer status, and §6 confines the falsifiability gate to the JSON
production consumer alone. The scaffold/production distinction is now
unambiguous and Lock-14-coherent throughout P2-E.

### §1.7 — Item 7: cohort-wide P1-V3-B §1.5 cross-reference — PARTIALLY FOLDED (non-blocking)

V1 §4 item 7 (explicitly tagged "Cohort discipline; non-blocking for
V1 admission") asked for a one-line cross-reference at the top of
P2-A / P2-D / P2-F anchoring the P1-V3-B §1.5 primitive-class
vocabulary.

V2 disposition: **PARTIALLY FOLDED — does not affect any verdict.**
- P2-A now anchors §1.5 inline at line 292 ("the generic template form
  (per P1-V3-B §1.5 / Lock 16 Layer-1 vocabulary)") and again in the
  §2.5 naming-convention paragraph. The vocabulary is bound in the
  load-bearing surface even though not in a header line.
- P2-D anchors P1-V3-B §2 (hot-leaf source) and §3.5 (codec parameter
  table) repeatedly; the §2 per-row tables carry the `Class` column
  drawn from §1.5 (rows D.1 V1-ACCEPT confirms this). The §1.5 *path*
  is not anchored by name, but the §2 Class-column discipline is the
  operative binding and it is present.
- P2-F does not anchor P1-V3-B §1.5 by path. It inherits the vocabulary
  by reference through P2-A / P2-D / P2-E (V1 row F.4). This is
  unchanged from V1.

Item 7 was non-blocking by V1's own designation and remains so. Two of
three reports now carry the anchor in load-bearing prose; P2-F's
non-anchor is hygiene only and changes no disposition. This item does
not gate V2 admission.

### §1.8 — V1-REVISE resolution summary

| V1 item | Report | V2 status |
|---|---|---|
| 1 — JSON-role function naming | P2-A §2.5 | RESOLVED — clean |
| 2 — `json_templates/` carve-out | P2-A §5 | RESOLVED — clean (carve-out route) |
| 3 — `AnyGrammar` declaration | P2-B §1.5 | RESOLVED — exceeds ask (uninhabited FactId) |
| 4 — cross-grammar prose + Track-2 | P2-C §5.1 + §2.7 | RESOLVED — clean, both clauses |
| 5 — string-block Lock-14 framing | P2-D §4 | RESOLVED — clean (`.data`-slot split) |
| 6 — scaffold-vs-production naming | P2-E §4.1/§4.2/§4.4 | RESOLVED — clean, choice made |
| 7 — cohort §1.5 cross-reference | P2-A/D/F | PARTIAL — non-blocking, no verdict impact |

All six load-bearing V1 REVISE items (1–6) are fully folded. Item 7 is
the non-blocking hygiene item; it is partially folded and does not gate
admission.

---

## §2 — V2 dispositions

Each row carries a verdict in {ACCEPT, ACCEPT-WITH-REVISE, REJECT}. The
V2 audit re-runs the four-surface Lock-14 scan against the folded
reports and verifies the fold introduced no new leak.

### §2.1 — P2-A union event-model (V2)

| # | Surface | Verdict |
|---:|---|---|
| A.1 | Class-column opacity — §2.2 `classes: Vec<u8>` opaque ids, substrate does not match on the byte. Unchanged by fold; still verbatim Lock-14. | **ACCEPT** |
| A.2 | StructuralAlphabet derivation — §3.1–§3.4 per-grammar metadata, producer takes the 64-byte set as argument. Unchanged. | **ACCEPT** |
| A.3 | Scalar-anchor gap mechanism — §2.3 + §3.1, gap-derived scalars, class column names the parser-event class. Unchanged. | **ACCEPT** |
| A.4 | Sheets doubled-delimiter rule placement — §3.2, parser state at the generated per-grammar module, not `runtime/src/tape/`. Unchanged. | **ACCEPT** |
| A.5 | JSON-role function naming (V1 REVISE) — §2.5 now carries the generic-template / JSON-codegen-output naming-convention paragraph; class-table at §2.3 reframed with inline JSON-output parentheticals. Fold landed; §1.1 above. | **ACCEPT** (was REVISE) |
| A.6 | `json_templates/` codegen-directory (V1 REVISE) — §5 carries the Lock-14 carve-out paragraph; carve-out route taken, no rename precondition, verification gate bound. §1.2 above. | **ACCEPT** (was REVISE) |
| A.7 | New-leak check: §2.5 naming-convention paragraph itself — verify the new prose does not itself name a JSON-role symbol as a substrate behaviour. The paragraph names `parse_object` etc. only after "The JSON codegen output names them …" — every JSON symbol is scoped as codegen-output. No leak. | **ACCEPT** |
| A.8 | New-leak check: §5 carve-out paragraph — verify the carve-out does not over-broaden into a generic-crate exception. The paragraph confines the carve-out to per-grammar codegen-instance directories and names the generic mechanism (`codegen/src/lib.rs`) separately. Carve-out is the (c)-surface, not a substrate exception. No leak. | **ACCEPT** |
| A.9 | §0 fold note — declares F4 + F5 + CH3/CH5 REVISEs. Scope of fold matches the consolidation assignment for V2-A. No scope drift. | **ACCEPT** |

P2-A V2: 9 ACCEPT, 0 REVISE, 0 REJECT. Aggregate: **ACCEPT.**

### §2.2 — P2-B retained grammar proof (V2)

| # | Surface | Verdict |
|---:|---|---|
| B.1 | Trait opacity — §1.3 trait carries no grammar-named arm; deliberately omits `event_kind`/`class_at`/`step_into`. Unchanged. | **ACCEPT** |
| B.2 | Sheets choice rationale — §3.1 three Lock-14 properties (reused punctuation byte, escape-equals-delimiter, no production parser). Unchanged. | **ACCEPT** |
| B.3 | Why-not-CSS-L4 — §3 K-count comparison; CSS L4's stress is codec-layer (P2-E owns). Unchanged. | **ACCEPT** |
| B.4 | Witness directory `*_witness` carve-out — §1.2 names `css_l4_witness/` / `sheets_witness/` with the load-bearing `_witness` suffix; the cfg-gate is bound to the parent `pub mod` site (R-CH5-2). Unchanged + reinforced by the cfg-gate-location fold. | **ACCEPT** |
| B.5 | `AnyGrammar` declaration (V1 REVISE) — new §1.5 with uninhabited FactId, `STRUCTURAL_CLASS_COUNT = 0`, explicit "not a JSON default in disguise." §1.3 above. Fold exceeds ask. | **ACCEPT** (was REVISE) |
| B.6 | Lock-14 audit-command binding — §3.3 three `rg` audits, every match confined to witness/proof files. Unchanged. | **ACCEPT** |
| B.7 | New-leak check: §1.5 `AnyGrammar` — verify the empty-grammar instance carries no JSON-specific behaviour. `STRUCTURAL_CLASS_COUNT = 0`, uninhabited FactId, `admits_*` return the empty-set answer. The instance is the identity, not a JSON specialisation. No leak. | **ACCEPT** |
| B.8 | New-leak check: JSON witness directory placement — §1.2 lands the JSON `EventGrammar` witness at `grammars/json/event_grammar_witness.rs` (inside the production `json/` directory, beside `generated.rs`), while the non-JSON witness uses a dedicated `*_witness/` directory. Asymmetry noted: the JSON witness is named `event_grammar_witness.rs` (the `_witness` suffix is on the *file*, not the directory) and is explicitly declared a "sibling proof file, not replacing `generated.rs`". This is V1-ACCEPTED shape (V1 B.4) — the JSON witness sits in the already-existing per-grammar directory; the `_witness` *directory* exists only for grammars with no production parser. Internally consistent; the file-suffix carries the proof-artefact marker. No leak. | **ACCEPT** |
| B.9 | §0 fold note — declares AnyGrammar + per-slice cost + cfg-gate location. Matches V2-B assignment. No drift. | **ACCEPT** |

P2-B V2: 9 ACCEPT, 0 REVISE, 0 REJECT. Aggregate: **ACCEPT.**

### §2.3 — P2-C Apache + CITM admission methodology (V2)

| # | Surface | Verdict |
|---:|---|---|
| C.1 | Owner-file scope discipline — §4.1 seven paths, none under `runtime/`, `bbnf-simd/`, `bbnf-codegen/`, `parse-that-regex/`. Row-table data only. Unchanged. | **ACCEPT** |
| C.2 | Lock-14 parent-diff allowance scope — §1.5 + §4.1 schema-identity bump on any additional owner path. Unchanged. | **ACCEPT** |
| C.3 | JSON-internal scope owned explicitly — §5 generalisation table classifies the 14 NO-GO rows; §5 names what is JSON-specific. Unchanged. | **ACCEPT** |
| C.4 | Cross-grammar transposition prose (V1 REVISE) — new §5.1 names the grammar-neutral seven-owner-file shape, the `<grammar>_real_typed_struct` row id form, the `sk-v{N>9}-<grammar>-real-typed-w{n}` schema identity, plus an illustrative CSS L4 / Sheets / TOML / BBNF-self table. §1.4 above. | **ACCEPT** (was REVISE) |
| C.5 | Track-2-oracle JSON-specificity (V1 REVISE) — §2.7 acknowledges `serde_json` is the JSON oracle with no cross-grammar equivalent; structural-independence is the grammar-neutral invariant, oracle shape is grammar-dependent. §1.4 above. | **ACCEPT** (was REVISE) |
| C.6 | PMU evidence scope — §2.8 PMU c/B parse-only, diagnostic non-producer. Unchanged; not a Lock-14 concern. | **ACCEPT** |
| C.7 | New-leak check: §5.1 illustrative cross-grammar table — verify the table does not commit the substrate to any grammar-specific code. The table is tagged "illustrative; not in scope for SK-V9 skinny"; the methodology is declared to transpose verbatim "with no codegen-side change" because "the codegen-emitted DirectBuild path is grammar-neutral by Lock 14." No leak. | **ACCEPT** |
| C.8 | New-leak check: §2.7 oracle clause — verify the clause does not require the substrate to know which oracle a grammar uses. The clause states "the methodology body does not commit to any particular oracle engine"; oracle selection is the per-grammar wave's, gated only by the structural-independence invariant. No substrate coupling. No leak. | **ACCEPT** |
| C.9 | F5 fold note (lines 691–705) — records both edits against CH2 §2.3 rows C.4 + C.5. Matches V2-C assignment. No drift. | **ACCEPT** |

P2-C V2: 9 ACCEPT, 0 REVISE, 0 REJECT. Aggregate: **ACCEPT.**

### §2.4 — P2-D aarch64 ASM/SIMD opportunities (V2)

| # | Surface | Verdict |
|---:|---|---|
| D.1 | Class-name fidelity in §2 per-row diagnoses — §2.1–§2.4 carry the `Class` column from P1-V3-B §1.5. Unchanged. | **ACCEPT** |
| D.2 | §3 unicode-escape codec class framing — §3.4 cross-grammar parameter table mirroring P1-V3-B §3.5; data-vs-code split named. Unchanged. | **ACCEPT** |
| D.3 | §4 string-block widening framing (V1 REVISE) — §4 opening now carries the Lock-14 framing paragraph; the primitive is named `scan_string_special_block_32`, parameters routed through the `.data` slot, JSON named as first consumer, CSS L4 / Sheets / BBNF-self as later-wave admissions. §1.5 above. | **ACCEPT** (was REVISE) |
| D.4 | §5 SHA3 EOR3 fold grammar-neutrality — §5.3.1 `veor3q_u8` per Lock 16 abstract-primitive lift. Unchanged. | **ACCEPT** |
| D.5 | §3/§4/§5 cross-grammar admission language — §3 explicit, §4 now explicit (was implicit; D.3 fold closes this), §5 inherits via P2-A StructuralAlphabet. | **ACCEPT** |
| D.6 | Process discipline §6 grammar-neutrality — per-primitive checkasm gate, not per-grammar. Unchanged. | **ACCEPT** |
| D.7 | New-leak check: §4 framing paragraph parameter set — V1 named a three-tuple; the fold uses a four-tuple (`terminator`, `escape`, `control_limit`, `non_ascii_threshold`). Verified against the actual `scan_string_special_block` signature at §4.1 — the fourth parameter is real (`non_ascii_mask` / `>=0x80` compare). The four-tuple is correct, not an over-broadening; all four are byte-class parameters in the codegen `.data` slot. No leak. | **ACCEPT** |
| D.8 | New-leak check: `.data`-slot vs const-generic reframe — V1 suggested const-generics; the fold routes the data/code split through the codegen `.data` slot per SK-V7 A3 §1. Both keep the kernel body grammar-neutral and the parameters at the codegen layer. The `.data`-slot form is internally consistent with §3.4's existing citation. No leak. | **ACCEPT** |
| D.9 | F1 wiring fix cross-check — the consolidation F1 required P2-D fix the false "unwired" claim for `unescape_uxxxx_x4_neon`. CH2 lens does not own F1, but verify the F1 reframe did not introduce a Lock-14 leak: §4 names the consumer `match_string_at_quote_trusted_utf8` as a per-grammar JSON realisation, not a generic-crate symbol. No leak. | **ACCEPT** |
| D.10 | Frontmatter `revision:` field — declares F1 + F4 + F5 + F6 fold with a §0 pointer. Matches V2-D assignment. No drift. | **ACCEPT** |

P2-D V2: 10 ACCEPT, 0 REVISE, 0 REJECT. Aggregate: **ACCEPT.**

### §2.5 — P2-E `escape_codec_hex_unit` codec primitive (V2)

| # | Surface | Verdict |
|---:|---|---|
| E.1 | Codegen-emitted parameter binding — §2 const generics + per-binding kernel specialisation + LTO inline, not a runtime switch. Unchanged. | **ACCEPT** |
| E.2 | Cross-grammar parameter table fidelity — §2 table mirrors P1-V3-B §3.5; four-axis parameterisation matches the canonical vocabulary. Unchanged. | **ACCEPT** |
| E.3 | Same-wave CSS L4 consumer sketch — §4.2 codegen-emitted `decode_css_unicode` calling `escape_codec::decode_variable::<Range::new(1,6), SurrogatePolicy::None, Terminator::WhitespaceOrNonHex>`. Unchanged. | **ACCEPT** |
| E.4 | Scaffold-vs-production-consumer naming (V1 REVISE) — §4.1 three-way taxonomy (1 production consumer = wired x4 JSON path; 2 scaffolds = CSS L4 + TOML); §4.2 names CSS L4 explicitly as a `#[cfg(test)]`-gated scaffold in `bbnf-css/tests/`; §4.4 names TOML as no-production-consumer this wave. §1.6 above. | **ACCEPT** (was REVISE) |
| E.5 | Layer-1 directory shape — §3.1 `crates/bbnf-simd/src/aarch64/escape_codec/` grammar-neutral directory; per-binding files named by parameter (`hex_x4`, `hex_x8`, `hex_variable`). Unchanged. | **ACCEPT** |
| E.6 | Surrogate-pair JSON-specificity — §2.1 `surrogate_join_policy = Pair` is a const-generic parameter binding codegen emits for JSON, constant-folds out for the other four grammars. Not a generic-crate `match grammar` arm. Unchanged. | **ACCEPT** |
| E.7 | New-leak check: §4.4 TOML no-consumer disposition — verify the no-consumer state does not strand the `hex_x8_neon` kernel as orphan grammar-specific code. §3 cost table S3 revert protocol: "no production consumer depends on it (TOML is scaffold-only)"; the kernel is gated by the checkasm parity gate (S6) alone. The kernel is a grammar-neutral primitive in `bbnf-simd` exercised by a compile-validation scaffold — admissible, not an orphan leak. No leak. | **ACCEPT** |
| E.8 | New-leak check: §4.2 CSS L4 scaffold call site — verify the scaffold lands in a non-generic crate. The `#[cfg(test)]` test lands in `bbnf-css/tests/` (the CSS grammar's own crate test dir), and the eventual production wiring is in `bbnf-css/src/tokenizer/escape.rs` (the CSS grammar's own crate). No generic-crate touch. The kernel called (`escape_codec::decode_variable`) is the grammar-neutral primitive. No leak. | **ACCEPT** |
| E.9 | §0 fold note — Cycle: V2; declares scaffold naming + slice caps + TOML disposition + PMU rederivation (F2). Matches V2-E assignment. No drift on CH2 surfaces. | **ACCEPT** |

P2-E V2: 9 ACCEPT, 0 REVISE, 0 REJECT. Aggregate: **ACCEPT.**

### §2.6 — P2-F SOTA teardown M5 Max (V2)

P2-F had no CH2 V1 REVISE items (V1: 6 ACCEPT, 0 REVISE). The V2 audit
re-confirms the six V1 rows are unchanged by the F3 synthesis-overreach
walk-back (which is a CH3/CH4/CH6 fold, not CH2), and checks the §0
walk-back introduced no new Lock-14 leak.

| # | Surface | Verdict |
|---:|---|---|
| F.1 | JSON-domain scope owned — §1 per-competitor table; competitors are JSON parsers, scope owned implicitly. Unchanged. | **ACCEPT** |
| F.2 | §5 architecture-lessons grammar-neutrality — one grammar-neutral pattern per competitor. Unchanged. | **ACCEPT** |
| F.3 | §5.3 yyjson fusion Lock-14 framing — codegen-emitted fusion, `escape_codec_hex_unit` class. Unchanged. | **ACCEPT** |
| F.4 | §7 SOTA-path cross-grammar admission — §7.2 grammar-neutral codec language; inherits P2-A / Lock 16 framing. Note: §7.2/§7.3 content was reframed by the F3 walk-back (DirectBuild emit-site expansion stripped; admission 1/2 stripped) — verify the strip did not remove the cross-grammar admission language. The §0 walk-back narrows §7 scope but the grammar-neutrality framing of the codec primitive survives the strip. No leak introduced. | **ACCEPT** |
| F.5 | §4 asmjson teardown grammar-neutrality — architecture-lift vs clone-failure distinction named both sides. Unchanged. | **ACCEPT** |
| F.6 | §6 DAV1D process discipline — per-primitive, grammar-neutral by construction. Unchanged. | **ACCEPT** |
| F.7 | New-leak check: §0 synthesis-overreach walk-back — verify the walk-back (stripping §7.4 sequencing, §7.2 DirectBuild expansion, §7.3 admission shapes) did not introduce a grammar-specific claim. The walk-back reframes synthesis-grade claims as a dependency graph and defers to S-P3; it removes scope, adds none. No new Lock-14 surface. No leak. | **ACCEPT** |
| F.8 | Item-7 hygiene residual — P2-F still does not anchor P1-V3-B §1.5 by path; inherits vocabulary via P2-A/D/E. Non-blocking per V1 §4 item 7. No verdict impact. | **ACCEPT** |

P2-F V2: 8 ACCEPT, 0 REVISE, 0 REJECT. Aggregate: **ACCEPT.**

### §2.7 — V2 disposition count

54 dispositions across the six reports (9 + 9 + 9 + 10 + 9 + 8). All 54
ACCEPT; 0 ACCEPT-WITH-REVISE; 0 REJECT. Of these, 6 are the flipped V1
REVISE rows (A.5, A.6, B.5, C.4 — counting C.4/C.5 as the two folded
rows, C.5 — D.3, E.4) and 18 are dedicated new-leak checks on the V2
fold prose (A.7–A.9, B.7–B.9, C.7–C.9, D.7–D.10, E.7–E.9, F.7–F.8); the
remainder re-confirm V1-ACCEPT rows are unchanged.

---

## §3 — Aggregate verdict

| Report | ACCEPT | ACCEPT-WITH-REVISE | REJECT | Aggregate |
|---|---:|---:|---:|---|
| P2-A union event-model | 9 | 0 | 0 | ACCEPT |
| P2-B retained grammar proof | 9 | 0 | 0 | ACCEPT |
| P2-C Apache + CITM admission | 9 | 0 | 0 | ACCEPT |
| P2-D aarch64 ASM opportunities | 10 | 0 | 0 | ACCEPT |
| P2-E `escape_codec_hex_unit` codec | 9 | 0 | 0 | ACCEPT |
| P2-F SOTA teardown M5 Max | 8 | 0 | 0 | ACCEPT |
| **Total** | **54** | **0** | **0** | **ACCEPT** |

ACCEPT rate: 54 / 54 = **100%** (V1: 80.6%).

CH2 GENERALITY verdict on the S-P2 V2 cohort: **ACCEPT.**

All six load-bearing F5 Lock-14 surgical reframings (V1 items 1–6)
landed cleanly. Each of the seven V1 ACCEPT-WITH-REVISE rows flipped to
ACCEPT: the prose now makes explicit the Lock-14 surface V1 found
implicit. Three folds *exceeded* the V1 ask without deviating from its
intent — P2-B substituted an uninhabited `FactId` for the V1-suggested
`()` (stronger empty-grammar proof), P2-C added an illustrative
four-grammar transposition table beyond the one-paragraph ask, and P2-E
added a three-way production/scaffold/no-consumer taxonomy beyond the
binary choice the V1 row required. The V2-D fold reframed the
string-block parameter binding through the codegen `.data` slot rather
than const-generics; this is internally consistent with P2-D's existing
SK-V7 A3 §1 citation and equally Lock-14-coherent.

The 18 dedicated new-leak checks (§2, surfaces *.7–*.10) confirm the V2
fold introduced no new Lock-14 leak: every new paragraph confines
JSON-role symbols to per-grammar codegen-output scope, every carve-out
is bounded to the Lock-14-permitted (c)-surface (per-grammar declaration
crate / per-grammar codegen-instance directory), and no fold added a
generic-crate `match grammar` arm, grammar-named generic module, or
grammar-specific generic public type. The cohort-wide `match grammar`
grep returns matches only inside Lock-14 negation prose ("no `match
grammar` arm").

Convergence: V2 CH2 returns 100% ACCEPT. Combined with V1's 80.6%, this
is the **first** cycle clearing the ≥95% bar on CH2. The convergence
criterion requires ≥95% for two consecutive cycles; a V3 re-CHALLENGE
is needed for the second consecutive clear before S-P2 CH2 converges.
No V2 finding blocks the V3 re-dispatch.

---

## §4 — Any remaining Lock-14 leaks

**No load-bearing Lock-14 leak remains in the V2 cohort.** All six
F5 items folded; the 18 new-leak checks pass.

One non-blocking hygiene residual carries forward, unchanged from V1
§4 item 7 (V1's own designation: "Cohort discipline; non-blocking for
V1 admission"):

- **R-CH2-V2-1 (hygiene, non-blocking).** P2-F does not anchor the
  P1-V3-B §1.5 canonical primitive-class vocabulary by path; it
  inherits the vocabulary by reference through P2-A / P2-D / P2-E. P2-A
  now anchors §1.5 in load-bearing prose (lines 292, §2.5); P2-D anchors
  P1-V3-B §2 + §3.5 and carries the §1.5 `Class` column in its §2
  tables. The P2-F non-anchor changes no disposition and does not gate
  V2 admission or the V3 re-dispatch. If V3 wishes a fully uniform
  cohort, a one-line cross-reference at P2-F's head closes it; this is
  optional polish, not a defect.

Two observations recorded for the S-P3 plan author (neither is a leak;
both are admissibility facts the V2 fold made explicit and the plan
must carry forward):

- **Observation 1 — `json_templates/` carve-out is the cohort's
  standing position.** P2-A took the Lock-14 *carve-out* route for
  `codegen/src/json_templates/`, not the *rename* route. The carve-out
  reasoning (per-grammar codegen-instance directory ≡ the Lock-14
  (c)-surface, parallel to `crates/<grammar>/`) is now load-bearing
  cohort prose. S-P3 must not silently re-open the rename: either the
  carve-out stands, or a rename is a deliberate, separately-justified
  decision. The two are not interchangeable mid-plan.

- **Observation 2 — P2-E `hex_x8_neon` (TOML) lands with no production
  consumer this wave.** §4.4 declares the TOML `\u`/`\U` bindings
  scaffold-only; the `hex_x8_neon` grammar-neutral primitive is
  exercised by a compile-validation scaffold + checkasm parity gate
  alone. This is Lock-14-admissible (the primitive is grammar-neutral;
  scaffolds are a legitimate Lock-14 demonstration surface), but S-P3
  must carry the "TOML production consumer wires in a later wave"
  deferral explicitly so the kernel does not read as orphan code in the
  wave diff.

End of disposition.
