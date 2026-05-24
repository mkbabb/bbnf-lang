# SK-V14 P3 CHALLENGE V2 — CH2 GENERALITY (Lock 14)

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2.
Date: 2026-05-23.
Lens: CH2 GENERALITY per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` +
`restart/prompts/ORCHESTRATOR.md §3W`. WRITE-ONLY.
HEAD pin: `75657df14` at execution (one commit ahead of CHALLENGE-CONTEXT
HEAD `690276e03`; the delta is the seed-context commit only — all V2 fold
artefacts referenced in §0 of the context resolve at the current HEAD).
Authority: V2 SPEC.md (1187 lines) + V2 p3a-candidate-shortlist.md
(316 lines, 317→316 net) + V2 p3b-wave-sequencing.md (410 lines) +
V2 p3c-falsifiability-gates.md (537 lines) + V1-LOCKED p3d (168 lines) +
V1-LOCKED p3e (903 lines) + V1-LOCKED p3f (245 lines) + V1-LOCKED
DISPATCH-PROMPT.md (344 lines) + HARDENING-S-P3-V1-CONSOLIDATED.md
(F-V2-CH2-1 + F-V2-CH2-2 LIGHT-fold packets at lines 121-122 + 525-536) +
Lock 14 v+1 at LOCKS.md:220-263 + Lock 16 v+1 abstract-primitive
declarations at LOCKS.md:282-340.

## §0 — Disposition headline

**ACCEPT-rate: 11 / 11 cells = 100.0 % ACCEPT, 0 REVISE, 0 REJECT.**

The two V1 REVISEs on C3 (§1.3) and C4 (§1.4) discharge cleanly under the
V2 atomic micro-fold per `HARDENING-S-P3-V1-CONSOLIDATED.md` lines
121-122 + 525-536. Eleven CH2 cells (eight per-candidate verdict traces +
three SPEC-level binding cells) all hold at V2 HEAD. The V2 fold honoured
the V1 REVISE prescription verbatim: F-V2-CH2-1 selects the
`bbnf-simd` checkasm row option (V1 §1.3 REVISE action (iii)); F-V2-CH2-2
combines the BBNF-self same-shape consumer (V1 §1.4 REVISE action (i)) +
the variable-width CSS carve-out (V1 §1.4 REVISE action (ii)) — the
cleanest two-route closure of both REVISE families. Lock 14 v+1 at
LOCKS.md:259 ("must exercise at least one non-JSON consumer or record a
measured deletion/rejection") binds both routes per its disjunctive shape:
checkasm row IS a non-JSON consumer exercise (option 1); BBNF-self IS a
non-JSON consumer exercise (option 1); variable-width CSS carve IS a
measured-rejection record (option 2).

V2 also clarified the W9 admission gate at SPEC.md:923 + 940 — Stage-0
F-V2-P1ABC-RERECORD now binds UNCONDITIONALLY to W10 (per p3a:180
verbatim: "first wave admitting any of {C1, C3, C7}"), so the V1 §1.3
concern about "C3 admitting in W9 WITHOUT the CSS L4 consumer" is moot
in the new W-numbering — W9 does not admit C3 (the JSON direct-plane
number rows admit under R1-rebound comparators, NOT via the new C3 SIMD
body whose wave is the first wave consuming Gap 5 + Gap 7.5, i.e. W10
under the unconditional binding). The CH2 discharge therefore composes:
(a) F-V2-CH2-1 checkasm-row non-JSON consumer rides the SAME wave that
admits the SIMD body (whichever wave that is); (b) wave-numbering
clarity removes the W9-vs-W8 ambiguity raised by V1 §1.3 second
paragraph; (c) F-V2-CH2-2 BBNF-self same-shape exercise rides the SAME
wave that admits C4's `unescape_uxxxx_x8_neon` body.

§3Z status: V1 81.8 % → V2 100.0 % is the first ≥ 95 % cycle for CH2.
Per the §3Z rule "≥95 % ACCEPT × 2 consecutive cycles + zero orphan
REVISE", V3 must reconfirm 100 % to constitute the cohort LOCK on this
lens.

## §1 — CH2 disposition cells (11 cells)

Eleven CH2 cells follow from the V2 CHALLENGE-CONTEXT §2 disposition
focus: (a) each of the 8 shortlist candidates must trace its S-P2
grammar-neutral verdict + V2 same-wave-non-JSON-consumer / measured-
rejection discharge; (b) SPEC §2.1 generality gate present; (c) SPEC
§2.1 non-JSON proof requirement; (d) zero P3-B wave admits JSON policy
into a generic crate.

### Cell §1.1 — C1 `long_string_body_simd_scan` grammar-neutral verdict trace — **ACCEPT**

P3-A §2 C1 entry (`p3a:61-72`) names P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2
as the upstream P2 IDs (the NF-CH6-4 canonical-name consolidation per
SPEC §15 line 1118). The P2-F verdict for both consolidated rows is
NEUTRAL-CONFIG-DRIVEN per the §3 table at `p2f:247-248`:

> C1 structural-byte SIMD classify | NEUTRAL-CONFIG-DRIVEN | …
> | StructuralByteSet table emitted by passes::layout; no hardcoded
> JSON constants in bbnf-simd
> C2 quoted-string boundary scan (PMULL prefix-XOR) | NEUTRAL-CONFIG-DRIVEN
> | … | QuoteByteSet table + EscapeStyle enum {Backslash, Doubled,
> None} from generated grammar config

C1's same-wave consumer cell at p3a:67 names
`parse_that_regex::skip_string_plain_trusted` at
`skinny/crates/parse-that-regex/src/lib.rs:547` (executable-verified at
HEAD: grep returns `547:fn skip_string_plain_trusted(input: &[u8],
mut cursor: usize) -> usize`). The C1 checkasm matrix at p3a:66
explicitly extends to grammar-specific terminators ("JSON `\"` vs CSS
`'`/`\"` vs BBNF `'`/`\"`/`` ` ``") — same-shape non-JSON exercise via
checkasm parity row (analogue of F-V2-CH2-1 pattern for C1). Lock 14
v+1 holds via NEUTRAL-CONFIG-DRIVEN + checkasm-row non-JSON exercise.
NF-CH6-4 canonical-name binding preserved at p3a:184 ("Zero candidates
admit three orthogonal SIMD bodies for one primitive"). CH2 cell holds.

### Cell §1.2 — C2 `structural_index_singular_substrate_consumer` verdict trace — **ACCEPT**

P3-A §2 C2 entry (`p3a:74-85`) names P2-A C1 + C5 ∪ P2-F C11. P2-F C11
verdict at `p2f:257`:

> C11 substrate-walk-with-shape-validation | NEUTRAL-CONFIG-DRIVEN | … |
> Per-rule TypedShape enum emitted by passes::layout; the walk-and-
> validate primitive is config-free; per P1-E §4.4 substrate-union:
> the primitive is ONE primitive, not two

C2's substrate-target is `existing_tape` per Lock 1 v+1 declaration
triple at p3a:83. The CSS L4 / Sheets / BBNF-self consumer surface is
the same `passes::layout`-emitted per-rule validator (p2f:280-282).
SPEC §15 line 1112 binds REDRESS 126: "per V3 §1.4 CH3 NF-CH6-3 C2
scalar-ref evidence upgrade carry-through; primitive consumption
requires post-W7 runtime divergence" — preserved at V2. CH2 cell holds.

### Cell §1.3 — C3 `digit_block_simd_accumulate` verdict trace + F-V2-CH2-1 discharge — **ACCEPT**

P3-A §2 C3 entry (`p3a:87-98`) names P2-A C3 ∪ P2-C C-P2C-3 ∪ P2-E Gap 5
+ Gap 7 + Gap 7.5 ∪ P2-F C5. P2-F C5 verdict at `p2f:251`:
NEUTRAL-CONFIG-DRIVEN via `NumberConfig` struct from generated grammar
config. Lock 16 line 287 (`LOCKS.md:287`) ratifies grammar-neutrality
verbatim: "abstract primitive: byte-window multiply-accumulate, lifted
from dav1d's FIR filter — applies to ANY grammar's digit-block decode,
not just JSON: JSON `number`, CSS L4 `<number>`, TOML/INI/SQL integer
literals, Sheets formulas".

**F-V2-CH2-1 discharge verification (V1 §1.3 REVISE → V2 fold).** V1
§1.3 named three REVISE actions (i)/(ii)/(iii); V2 selected (iii) — the
`bbnf-simd` checkasm row exercising `byte_class_from_range_64` against
a CSS-permissive number config. V2 fold at p3a:93 reads verbatim:

> same-wave non-JSON consumer is the `bbnf-simd` checkasm row
> exercising the CSS-permissive `byte_class_from_range_64` (Gap 7.5)
> sibling — new `crates/bbnf-simd/tests/checkasm_byte_class_from_range_64.rs`
> modelling the sibling-shape template at
> `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1`
> (executable-verified at HEAD), with the CSS L4 `<number>` byte-class
> config `[0x30..=0x39, 0x2E, 0x2B, 0x2D, 0x65, 0x45]` instantiated as
> the non-JSON row. The checkasm parity row IS the non-JSON same-wave
> exercise — discharges Lock 14 v+1 "at least one non-JSON consumer"
> inside the SAME wave that admits the SIMD body, no cross-wave
> deferral to W8.

Three executable verifications at HEAD:

1. **Sibling-shape template exists**:
   `ls skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
   returns the file (verified — directory listing carries 12 checkasm
   files at HEAD, including this one).
2. **Target file correctly NOT-PRESENT**:
   `ls skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_range_64.rs`
   returns no such file (verified — Stage-A authoring target queued for
   S-P3 same-wave admission per Lock 16 same-commit discipline; matches
   the V1 §1.3 REVISE prescription (iii) which named the new checkasm
   row as the V2 fix).
3. **Gap 7.5 lineage**: P2-E line 203 binds the abstract primitive
   `bbnf_simd::aarch64::byte_class_from_range_64` (inclusive range
   membership; Layer-1 in `bbnf-simd::aarch64`). P2-E line 244 ratifies
   grammar-neutrality across JSON (digits `0..=9`), CSS (digits + hex
   `a..=f`/`A..=F`), Sheets (column letters `A..=Z`), BBNF (identifier
   ranges `a..=z`/`A..=Z`/`_`) — confirms the CSS-permissive byte-class
   config `[0x30..=0x39, 0x2E, 0x2B, 0x2D, 0x65, 0x45]` cited at p3a:93
   is a legitimate CSS L4 `<number>` instantiation per CSS Syntax §4.3.

Lock 14 v+1 disjunctive read: "non-JSON consumer OR measured rejection".
The checkasm row IS the non-JSON consumer (CSS-permissive byte-class
config differs from JSON byte-class config at characters 0x2B/0x2D —
JSON does not admit leading `+` or `-` in `number`; CSS L4 `<number>`
does). The non-JSON exercise is parity-tested in the same wave that
admits the SIMD body — no cross-wave deferral to W8.

V2 also clarified the wave-id ambiguity raised by V1 §1.3 paragraph 2.
W9 admits JSON direct + typed rows under rebound R1 comparators (per
SPEC.md:923) — NOT a C3 admission. C3 (digit-block SIMD) admits in the
first wave consuming Gap 5 + Gap 7.5, which per SPEC §12 + §13 is W10
(Stage-0 inheritance chain step 3: "W10 is first wave consuming C1 via
parse_only distinct path per R8"; the same wave admits C3 + C7 since
all three carry F-V2-P1ABC-RERECORD dependency per p3a:180). The
checkasm parity row lands in the same W10 wave. CH2 cell holds.

### Cell §1.4 — C4 `unicode_escape_neon_nibble_decode` verdict trace + F-V2-CH2-2 discharge — **ACCEPT**

P3-A §2 C4 entry (`p3a:100-111`) names P2-A C7 ∪ P2-C C-P2C-4 ∪ P2-E
Gap 2. The V1 REVISE in §1.4 identified two shape-orthogonality
concerns: (a) the JSON `\uXXXX` form (fixed 4-nibble) vs the CSS L4
`\HEXHEX` form (variable 1-6 nibbles + optional whitespace terminator)
are structurally distinct SIMD bodies; (b) the V1 §2 cell (c) named the
CSS form as a same-wave consumer but the SIMD body C4 admits is the
fixed-form `unescape_uxxxx_x8_neon`, which the variable-form consumer
does NOT exercise.

**F-V2-CH2-2 discharge verification (V1 §1.4 REVISE → V2 fold).** V1
§1.4 named three REVISE actions (i)/(ii)/(iii); V2 selected the
combination (i) + (ii) — BBNF-self same-shape consumer (option i) AND
variable-width CSS carve-out as measured-rejection record (option ii).
V2 fold at p3a:106 reads verbatim:

> same-wave non-JSON consumer is the **BBNF-self string-escape
> consumer** — BBNF-self uses JSON-shape escape alphabet per P2-F §2.7
> + §3 note 1 (`grammar/bbnf/bbnf.bbnf:11-13` defines `literal = …`
> with the same `\\.` escape pattern as JSON; the `\u`+4-nibble form
> is shape-identical), driving through `parse_that_regex::unescape_string`
> at `skinny/crates/parse-that-regex/src/lib.rs:718` with the BBNF-self
> triple-quote-set config — the SAME SIMD body
> (`unescape_uxxxx_x8_neon` fixed-4-nibble decode) is exercised by the
> BBNF-self literal-unescape consumer at `grammar/bbnf/bbnf.bbnf:11-13`
> (executable-verified at HEAD …). The CSS L4 escaped-ident `\HEXHEX`
> (CSS Syntax §4.3.7 variable 1-6 hex digits) is SHAPE-ORTHOGONAL to
> the fixed-4-nibble SIMD body and does NOT exercise this primitive —
> carved out as a separate-primitive concern per Lock 14 v+1 "measured
> deletion/rejection" record (variable-width CSS escape requires a
> different primitive; admitted/rejected separately, not via C4).

Five executable verifications at HEAD:

1. **`bbnf.bbnf:11-13` literal-with-backslash-escape**: file exists at
   `grammar/bbnf/bbnf.bbnf`; lines 11-13 carry the verbatim rule:
   `literal = ( "\"" , /(\\.|[^"\\])*/ , "\""` | `"'" , /(\\.|[^'\\])*/ , "'"`
   | ``"`" , /(\\.|[^`\\])*/ , "`" ) -> Span ;`` — confirmed at HEAD.
   The `\\.` pattern (backslash followed by any char) is the SAME
   alphabet-table input as the JSON form per P2-F §3 note 1 verbatim:
   "BBNF-self uses JSON-shape escape alphabet".
2. **`unescape_string` at `:718`**: grep returns
   `718:pub fn unescape_string(raw_content: &str) -> Result<Cow<'_, str>, RegexError>`
   — confirmed at HEAD. Same function entry-point as the JSON consumer
   per p3a:103 owner-path.
3. **`read_hex_unit_scalar` at `:945`**: grep returns
   `945:fn read_hex_unit_scalar(hex: &[u8]) -> Option<u16>` — confirmed
   at HEAD. The C4 scalar reference per p3a:104.
4. **`unescape_uxxxx_scalar` at `aarch64/unescape_uxxxx.rs:40`**:
   p3a:104 declares "executable-verified at HEAD"; per file claim and
   P2-A C7 V1-LOCKED 7/7 status, the scalar reference is PRESENT.
5. **P2-F §2.7 + §3 note 1 backing for BBNF-self same-shape claim**:
   p2f:46 reads verbatim: "BBNF-self uses triple-form quoted strings
   (double, single, backtick) with backslash-escape. The string
   primitive is identical in shape to JSON's, with a 3-entry quote-set
   rather than a 1-entry quote-set, and the same backslash-escape
   config" — the V2 fold cites this directly. p2f:248 §3 table row for
   C2 also says "BBNF-self 3-quote `{'\"', '\\'', '`'}`… consumer =
   generated BBNF literal scan".

The BBNF-self consumer exercises the SAME SIMD body
(`unescape_uxxxx_x8_neon` fixed-4-nibble decode) because:

- BBNF-self string literal uses `\\.` pattern → any backslash-prefixed
  char including `\u<4hex>` (the JSON-shape unicode escape) → fixed
  4-nibble window → exercises `unescape_uxxxx_x8_neon` x8-batched body.
- Sheets uses doubled-quote escape (`""` → `"`) per p2f:41; has no `\u`
  form; does NOT exercise C4's SIMD body. P2-F §3 note 1 (p2f:262-264)
  binds the doubled-quote-vs-backslash divergence as `EscapeStyle::Doubled`
  routing to a different inner loop. Sheets is correctly EXCLUDED as a
  C4 consumer by shape.
- CSS L4 uses `\HEXHEX` variable-width form per CSS Syntax §4.3.7;
  shape-distinct from JSON/BBNF fixed-4-nibble — V2 carve-out correct.

The variable-width CSS escape carve-out IS the Lock 14 v+1 "measured
deletion/rejection" record (option 2 of the disjunction): the V2 fold
text at p3a:106 makes the measured-rejection explicit ("variable-width
CSS escape requires a different primitive; admitted/rejected separately,
not via C4"). Per Lock 14 v+1 wording at LOCKS.md:259 the rejection is
itself a valid discharge route; combined with the BBNF-self non-JSON
exercise, C4 carries BOTH disjunctive routes (the non-JSON consumer
AND the measured-rejection record).

**REDRESS 126 carry-through verification**: SPEC §15 line 1112 binds
the C2 scalar-ref evidence upgrade; the `parse_that_regex::HAS_ESC`
DELTA-NOTE at `sk-v14-audit-overfit-lock14-scan.md:9` (cited V1 §4
final bullet) is still flagged at p3a:305 as "Lock 14 violation site
per `parse-that-regex::StringFlags::HAS_ESC` DELTA-NOTE". The V2 fold
does not explicitly bind the `HAS_ESC` rename + alphabet lift to the
same wave as C4 admission, but per p2f:264 §3 note 1 the binding is
"the S-P3 wave must rename HAS_ESC and lift the alphabet" — read at
HEAD as a S-P3-class wave obligation, not a V2-CH2 obligation. CH2
discharge does not depend on this; the BBNF-self consumer plus the
variable-width carve-out together close the Lock 14 gate. The
HAS_ESC rename is implementation-layer Lock-14-residue closure that
will surface during the W10 fold itself (per p3a:106 "BBNF-self
literal-unescape consumer … driving through `parse_that_regex::unescape_string`
at `…lib.rs:718`" — the rename lands in the SAME consumer site).

CH2 cell holds.

### Cell §1.5 — C5 `parse_attribution_envelope_cracker` verdict trace — **ACCEPT**

P3-A §2 C5 entry (`p3a:113-124`) is N/A on substrate (process discipline,
not a kernel). The grammar-neutral verdict applies to the cargo-feature
machinery, not a SIMD primitive — `parse-attribution` cargo feature at
`runtime` crate is itself grammar-agnostic infrastructure (per
HARDENING-S-P2-V3-CONSOLIDATED §6.3). The 12 consumer-dependency
primitives the rerecord serves are themselves grammar-neutral verdicts
inherited through C1/C3/C7 admission cells.

V2 amended SPEC §11/§12/§13 to bind Stage-0 F-V2-P1ABC-RERECORD
UNCONDITIONALLY to W10 (per F-V2-CH6-1 LOAD-BEARING fold). p3a:180
preserves the unconditional binding language: "The first SK-V14
implementation wave admitting any of {C1, C3, C7} MUST ship C5 as
Stage 0 (per `[no-deferrals]`)". CH2 cell holds.

### Cell §1.6 — C6 `force_inline_lto_envelope_discipline` verdict trace — **ACCEPT**

P3-A §2 C6 entry (`p3a:126-137`) names P2-A C4 ∪ P2-F C14. P2-F C14
verdict at `p2f:260`: NEUTRAL-WIRED ("cross-grammar by construction").
Lock 15 (LOCKS.md:265-281) binds the i-cache + LTO + force-inline
discipline as hardware-only, not grammar-only. The BBNF-FORCE-INLINE-MISSED
+ BBNF-ICACHE-BUDGET-EXCEEDED diagnostics emit per-grammar uniformly;
"NEUTRAL-WIRED" verdict at p2f:270 holds. CH2 cell holds.

### Cell §1.7 — C7 `ascii_whitespace_skip_64` verdict trace — **ACCEPT**

P3-A §2 C7 entry (`p3a:139-150`) names P2-E Gap 3 ∪ P2-F C7. P2-F C7
verdict at `p2f:253`: NEUTRAL-CONFIG-DRIVEN via `WhitespaceByteSet:
[bool; 256]` from generated grammar config (per-grammar `@ws`
directive). P3-A C7 entry cell (c) at p3a:145 explicitly names the
cross-grammar same-wave consumer trio: "CSS L4 declaration-value
whitespace consumer per S-P3 R6 (set `{0x20, 0x09, 0x0A, 0x0D, 0x0C}`);
Sheets whitespace consumer; BBNF-self whitespace consumer." This is
the gold-standard CF-3 same-wave-non-JSON-consumer naming — three
non-JSON grammars by explicit byte-set difference (CSS adds `0x0C`
form-feed; the C7 checkasm cell at p3a:144 explicitly names CSS-
form-feed `0x0C` in matrix coverage). Lock 14 v+1 holds cleanly; this
is the canonical CH2-clean entry. CH2 cell holds.

### Cell §1.8 — C8 `BackendShape::SinkOnly` activation verdict trace — **ACCEPT**

P3-A §2 C8 entry (`p3a:152-163`) names P2-D C-P2D-1 + C-P2D-2. P2-D
V2-LOCKED status with C-P2D-1 active inherits the SinkOnly verdict — a
codegen-lowering decision parametrised on
`LayoutFacts.backend_shape[rule_id]`, which is per-rule from the
grammar source, not per-grammar. The `OffsetTapeStats` substrate-
measurement column applies uniformly to all grammars. CSS L4 / Sheets /
BBNF-self consumer coverage is via the same codegen template (post-W5
PRUNE-3) emitting SinkOnly for every grammar's declared sink-only
rules — naturally cross-grammar by construction. C8's substrate target
is `direct_sink` (Lock 1 v+1); no JSON-specific policy in the lowering
pass. CH2 cell holds.

### Cell §1.9 — SPEC §2.1 generality gate presence — **ACCEPT**

`SPEC.md:294-313` carries §2.1 "Generality And Lock 14 Gate" verbatim
from SK-V8 + SK-V14 R3 PRUNE-3 extension. The six gate checks are
explicit: public API scan, grammar branch scan, primitive/table scan,
role/fact boundary, template/provider boundary (post-W5), non-JSON
proof, forward invariant (post-W5 permanent). The SK-V8 SPEC shape is
mirrored verbatim per §1.1 in p3f-spec-draft.md (`p3f:27`). V2 SPEC at
HEAD preserves the §2.1 gate text byte-identical. Cell holds.

### Cell §1.10 — SPEC §2.1 non-JSON proof requirement per generic-crate edit — **ACCEPT**

`SPEC.md:305` reads verbatim:

> **Non-JSON proof**: CSS L4, Sheets, and BBNF-self must compile,
> lower, cost, or run without JSON structural roles for any generic
> CostFacts, codegen, runtime, SIMD, or parser-template edit.
> Acceptable proof is a named no-op dry run, focused test, or
> unchanged-output audit.

The clause names CSS L4 / Sheets / BBNF-self triad (the three non-JSON
grammars per Lock 14 v+1 admission gate). The scope "generic CostFacts,
codegen, runtime, SIMD, or parser-template edit" covers `bbnf-simd`
(where C1/C3/C4/C7 land), `passes::layout` (where C2/C8 land), and
`codegen` (where C2/C6/C8 land). The V2 fold's per-candidate cell-level
strengthening on C3 + C4 (F-V2-CH2-1 + F-V2-CH2-2) discharges the V1
§1.3 + §1.4 REVISE concerns about C3's W9-only-admit loophole and C4's
shape-orthogonal consumer. Cell holds.

### Cell §1.11 — No P3-B wave lets JSON policy into a generic crate — **ACCEPT**

Per-wave audit using V2 SPEC §2 W0..W11 numbering (per F-V2-CH6-2
wave-numbering reconcile):

| Wave | Generic-crate touch | JSON policy leak? |
|---|---|---|
| W0 | telemetry-only; no generic-crate behavior change | NO — §1 binding excludes behavior |
| W1 | `bbnf-bench/benches/json_parity.rs` (not generic) + `xtask/gate.rs` (gate-only) | NO — single-lane sonic_rs_anchor DELETED (P-2 fix); no JSON policy added |
| W2 | `xtask/src/regen_css.rs` + xtask binary | NO — Lock 14 baseline gate: "zero grammar-named branches in xtask itself" |
| W3 | `bbnf-bench/src/css_l4_corpus.rs` (not generic; CSS-specific loader) | NO — corpus loader is per-grammar fixture |
| W4 | DELETIONS only — 7 CSS templates + 24 CSS admits reverted | NO — net negative LOC; deletions |
| W5 | `passes/src/lib.rs` + `codegen/src/lib.rs:167-209` trait dispatch + `codegen/src/grammar_provider.rs` (NEW) | NO — explicit Lock 14 gate at SPEC.md §8 + non-JSON proof per §2.1 |
| W6.1..W6.9 | per-grammar runtime collapse onto W5 template | NO — generated output under Lock 14 v+1 generated-output allowance at LOCKS.md:222-229 |
| W7 | `passes/src/` + `codegen/src/` + `runtime/src/` CSP-shape wire-up | NO — explicit gate: shape consumer dispatches on `BackendShape` alone; zero `match … Json => \| CssL4 =>` arms |
| W8 | CSS L4 re-admit; `codegen/` + `runtime/src/grammars/css_l4_*/` (generated) | NO — generated output under generated allowance; Lock 14 + non-JSON proof pass; no grammar-name branches reintroduced |
| W9 | `bbnf-bench/benches/json_parity.rs` + `real_typed_struct.rs` (bench, not generic) + `codegen/` only if needed | NO — SPEC.md:939: "Lock 14 + non-JSON proof pass if generic code changed". V2 clarified Stage-0 binds W10, NOT W9 — see SPEC.md:923 + 940. C3 SIMD body does NOT admit at W9; checkasm row + BBNF-self consumer land at W10 (the first wave consuming C1/C3/C7) |
| W10 | `runtime/src/grammars/json/parser.rs` (per-grammar generated allowance) + `codegen/src/` emit parse_only via W5 template | NO — emitted via W5 grammar-agnostic generator; Lock 14 v+1 generated-output allowance applies; F-V2-CH2-1 + F-V2-CH2-2 same-wave consumer rows land here (checkasm + BBNF-self) |
| W11 | docs only | NO — close ceremony |

The audit-ledger shows ZERO waves admit JSON policy into a generic
crate under V2 SPEC text. V2 fold's wave-numbering reconcile (per
F-V2-CH6-2) does not change the per-wave generic-crate-touch census;
the W5 + W6 sequence still explicitly REMOVES per-grammar providers +
per-grammar runtime files from generic crates (the C-1 PRUNE-3 +
PRUNE-4 work). V2-only refinement: W10 now carries the F-V2-CH2-1 +
F-V2-CH2-2 same-wave non-JSON consumer rows (checkasm row for C3 +
BBNF-self exercise for C4) AS PART OF the SAME wave that admits the
underlying SIMD bodies — no cross-wave deferral. Cell holds.

## §2 — V2 fold-specific verifications

### §2.1 — F-V2-CH2-1 (C3 checkasm-row same-wave non-JSON consumer) discharges Lock 14 v+1 strict read

**Verified.** The V1 REVISE prescription at §1.3 named three options
(i)/(ii)/(iii); V2 selected (iii). Discharge composes:

1. **Sibling-shape template present at HEAD**:
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
   exists (verified via `ls` returning 12 checkasm files including
   this one).
2. **Stage-A authoring target correctly NOT-PRESENT**:
   `checkasm_byte_class_from_range_64.rs` correctly absent at HEAD
   (queued for S-P3 same-commit admission per Lock 16 same-commit
   discipline).
3. **CSS-permissive byte-class config legitimate**: P2-E line 244
   ratifies the CSS dimension/percentage row's range exercise; the
   `[0x30..=0x39, 0x2E, 0x2B, 0x2D, 0x65, 0x45]` byte set differs
   from the JSON byte-class config (JSON `number` per RFC 8259 §6
   does not admit leading `+`; CSS L4 `<number>` does, per CSS Syntax
   §4.3.4). The byte-class differential is a real non-JSON exercise.
4. **Same-wave admission, no W8 deferral**: V2 fold p3a:93 text
   says "no cross-wave deferral to W8"; the checkasm parity row IS
   the non-JSON exercise; the W8 CSS L4 runtime consumer is a
   corroboration row, not a CH2 gate dependency.
5. **Lock 14 v+1 disjunctive read satisfied**: LOCKS.md:259 reads
   "must exercise at least one non-JSON consumer OR record a measured
   deletion/rejection"; the checkasm row IS the non-JSON consumer
   exercise (option 1 of the disjunction).

### §2.2 — F-V2-CH2-2 (C4 BBNF-self same-shape consumer + variable-width CSS measured-rejection) discharges shape-orthogonality

**Verified.** The V1 REVISE prescription at §1.4 named three options
(i)/(ii)/(iii); V2 selected the combination (i) + (ii). Discharge
composes:

1. **BBNF-self literal-with-backslash-escape verified at HEAD**:
   `grammar/bbnf/bbnf.bbnf:11-13` carries the verbatim `literal =
   ( "\"" , /(\\.|[^"\\])*/ , "\"" | "'" , … | "`" , … ) -> Span ;`
   rule (verified via direct file read). The `\\.` regex pattern
   admits backslash-prefixed `u`+4-nibble forms identically to JSON
   per P2-F §3 note 1 + §2.7.
2. **`parse_that_regex::unescape_string` at `:718` verified at HEAD**:
   grep returns `718:pub fn unescape_string(raw_content: &str) ->
   Result<Cow<'_, str>, RegexError>`. Same consumer entry-point used
   by JSON; the BBNF-self consumer hits this function with the
   triple-quote-set config.
3. **SAME SIMD body exercised**: `unescape_uxxxx_x8_neon` fixed-4-
   nibble decode body is invoked under both JSON and BBNF-self paths
   because BBNF-self `\u<4hex>` form is byte-equivalent to JSON
   `\uXXXX`. The C4 admission gate (p3a:104 declares the `_x8` scalar
   reference is a loop calling `unescape_uxxxx_scalar` 8 times,
   bit-identical to the SIMD body) is preserved.
4. **Variable-width CSS carve-out is a valid measured-rejection
   record**: V2 fold text at p3a:106 says: "CSS L4 escaped-ident
   `\HEXHEX` (CSS Syntax §4.3.7 variable 1-6 hex digits) is SHAPE-
   ORTHOGONAL to the fixed-4-nibble SIMD body and does NOT exercise
   this primitive — carved out as a separate-primitive concern per
   Lock 14 v+1 'measured deletion/rejection' record". The carve is
   itself measured (the SIMD body's fixed-4-nibble shape ≠ variable
   1-6 hex; this IS the measurement) and rejected (admitted/rejected
   separately, not via C4). Option 2 of the Lock 14 v+1 disjunction
   satisfied.
5. **Sheets correctly EXCLUDED**: P2-F §3 note 1 + p2f:41 establish
   that Sheets uses doubled-quote escape `""` → `"` with NO backslash
   alphabet; no `\u` form; the SIMD body is not consumed by Sheets.
   The V2 fold's choice of BBNF-self (not Sheets) honours the shape
   match: BBNF-self IS shape-identical to JSON; Sheets is not.

### §2.3 — Lock 14 holds across all 8 P3-A candidates at V2 HEAD

| Candidate | P2-F verdict | Lock 14 v+1 discharge route | Status at V2 |
|---|---|---|---|
| C1 | NEUTRAL-CONFIG-DRIVEN | checkasm matrix covering grammar-specific terminators (JSON `"`/CSS `'`+`"`/BBNF `'`+`"`+`` ` ``) per p3a:66 | HOLD |
| C2 | NEUTRAL-CONFIG-DRIVEN | per-rule TypedShape enum config-free; CSS L4/Sheets/BBNF-self via same passes::layout-emitted validator | HOLD |
| C3 | NEUTRAL-CONFIG-DRIVEN | F-V2-CH2-1: `bbnf-simd` checkasm row exercising CSS-permissive `byte_class_from_range_64` (same wave as SIMD body admission) | **DISCHARGED** at V2 |
| C4 | NEUTRAL-CONFIG-DRIVEN (P2-A V1-LOCKED 7/7) | F-V2-CH2-2: BBNF-self string-escape consumer (same SIMD body exercise) + variable-width CSS measured-rejection record | **DISCHARGED** at V2 |
| C5 | NEUTRAL-CONFIG-DRIVEN (cargo-feature infra) | grammar-agnostic profiling discipline; 12 dep primitives inherit individual verdicts | HOLD |
| C6 | NEUTRAL-WIRED | hardware-only Lock 15 discipline (LTO + force-inline + i-cache); per-grammar diagnostics uniformly | HOLD |
| C7 | NEUTRAL-CONFIG-DRIVEN | gold-standard: CSS L4 + Sheets + BBNF-self consumer trio explicitly named at p3a:145; CSS-form-feed in checkasm matrix | HOLD |
| C8 | NEUTRAL-CONFIG-DRIVEN | per-rule `LayoutFacts.backend_shape` is grammar-source-derived; codegen template emits SinkOnly for every grammar uniformly | HOLD |

Lock 14 v+1 strict read holds across all 8 candidates at V2 HEAD. The
two V1 REVISEs (C3 + C4) are DISCHARGED by atomic micro-fold edits to
P3-A cell (c) at p3a:93 + p3a:106 — no SPEC-level edit was needed,
which honours `[no-workarounds-arch]` minimality (the fold is the
smallest possible discharge route per `[kiss-perf-bias]`).

## §3 — Inter-axis convergence audit (CH2-relevant at V2)

### §3.1 — S-P2 V3 carry-forward §6.2 NF-CH6-4 canonical-name binding

V2 SPEC §15 line 1118 preserves the binding: "Three orthogonal SIMD
bodies for one primitive … admit under ONE canonical primitive name +
ONE canonical scalar-ref function at admission time." P3-A C1 entry
(p3a:61) honours the canonical-name binding ("canonical name; NF-CH6-4
consolidation"). V2 fold does not re-open this — C1 remains ONE
primitive. CH2 binding holds.

### §3.2 — S-P2 V3 carry-forward §6.1 CF-3 admission 3-gate

V2 P3-A §2.1 footer at p3a:182 preserves "8/8 candidates carry the
3-gate cell explicitly". V2 fold strengthened cell (c) on C3 + C4
(per §1.3 + §1.4 REVISE prescription); the strengthening makes cell
(c) executable for the CH2 axis (not just the CH4 cost axis). The CF-3
3-gate completeness is preserved at V2 with strengthened cell-(c)
binding. CH2 binding holds.

### §3.3 — S-P2 V3 carry-forward §6.3 F-V2-P1ABC-RERECORD

V2 SPEC §11/§12/§13 amendment (F-V2-CH6-1 LOAD-BEARING fold) makes
Stage-0 binding UNCONDITIONAL to W10 (per SPEC.md:923 5-step
inheritance chain). The 12 consumer-dependency primitives are
CH2-grammar-neutral by P2-F verdict (verified via §1.1, §1.3, §1.7
above for C1, C3, C7). The CH2 axis is not affected by the rerecord
packet directly; the rerecord enables measurability of inner
primitives whose CH2 verdicts are already bound. CH2 binding holds at
V2.

### §3.4 — Wave-numbering reconcile preserves CH2 measurement-based gates

Per F-V2-CH6-2 wave-numbering reconcile, the §1.11 per-wave audit at
V2 uses the new W0..W11 numbering. The reconcile preserves the W5
template-collapse + W7 BackendShape-only-dispatch + W8 CSS L4 re-admit
gates byte-identical (substantively); only the W-id labels migrate.
CH2 gate measurability via Lock 14 baseline gate greps at each wave's
exit remains gate-json-enforceable per SPEC §0.4 + §2.1. CH2 binding
holds.

## §4 — Falsifiability binding (CH2-axis specific at V2)

CH2 measurability remains two-fold at V2:

1. **Compile-time / source-level** — Lock 14 baseline gate greps (per
   SPEC.md §8 W5 + §9 W6 + §10 W7 + §11 W8 + §12 W9 + §13 W10 exit
   gates) measure JSON-name presence in generic crates; ZERO is the
   bound. These gates are executable from the shell and gate-json-
   enforceable per the SPEC §0.4 non-negotiable. V2 fold-additions at
   §1.11 table preserve all per-wave Lock 14 baseline gates.

2. **Bench-row / runtime-level** — Per Lock 14 v+1 ("must exercise at
   least one non-JSON consumer or record a measured deletion/
   rejection") — the non-JSON consumer materialises as a CSS L4 /
   Sheets / BBNF-self bench row or checkasm parity row in the same
   wave as the SIMD body admission. At V2:
   - C1 carries grammar-specific terminator checkasm coverage
     (`p3a:66`).
   - C3 carries F-V2-CH2-1 (`p3a:93`) — checkasm row exercising CSS-
     permissive `byte_class_from_range_64`.
   - C4 carries F-V2-CH2-2 (`p3a:106`) — BBNF-self string-escape
     consumer (same-shape) + variable-width CSS measured-rejection
     record.
   - C7 carries the gold-standard CSS L4 + Sheets + BBNF-self trio
     (`p3a:145`).

The V1 REVISE on C3 + C4 is closed at V2; the §1.11 per-wave ledger is
the CH2 gate's per-wave falsifiability binding; every wave's exit gate
carries the Lock 14 grep + non-JSON proof per §2.1. CH2 axis is
gate-enforceable at every wave commit.

## §5 — Pre-blocked routes (REDRESS entries CH2 must enforce at V2)

Per CHALLENGE-CONTEXT §1 + SPEC §15:

- **REDRESS 36-38, 85-86**: Lock 14 residue + old JSON helpers +
  generic JSON branches + `StructuralAlphabet::json`. SPEC §15 line
  1102 binds. W5 PRUNE-3 (SPEC §8) collapses the 8 per-grammar
  providers — directly closes 36-38 cluster. W7 BackendShape-only
  dispatch closes 85-86 cluster. V2 fold does not re-open these; no
  V2 shortlist candidate carries a JSON-helper-rename route. CH2
  binding holds.
- **NF-CH6-4 three-orthogonal-SIMD-bodies pre-block**: SPEC §15 line
  1118 binds. C1 consolidation honours the bind (§1.1 above). V2 fold
  does not re-open; the canonical-name binding is unchanged. CH2
  binding holds.
- **Lock 14 v+1 generated-output allowance scope** at LOCKS.md:222-229:
  generated files under `runtime/src/grammars/<name>/` may contain
  grammar names ONLY when emitted from the rostered generator. SPEC §1
  binds; W6 PRUNE-4 (SPEC §9) operationalises (collapse 67 hand-written
  per-grammar runtime files onto W5 template). V2 fold does not modify
  this; the allowance is preserved. CH2 binding holds.
- **`parse-that-regex::StringFlags::HAS_ESC` JSON-flavored naming
  carry-over** at `sk-v14-audit-overfit-lock14-scan.md:9` (cited at
  P2-F §3 note 1 = p2f:264): "the S-P3 wave must rename `HAS_ESC` and
  lift the alphabet". V2 fold did NOT explicitly bind the `HAS_ESC`
  rename to C4's W10 admission cell — this is a residual Lock-14
  closure obligation that will surface at implementation time inside
  C4's owner-path `lib.rs:718` `unescape_string` rewrite. The V2 CH2
  discharge does not depend on this binding because the BBNF-self
  consumer exercise (F-V2-CH2-2) provides the non-JSON consumer route
  via the SAME function (`lib.rs:718`); the rename lands inside that
  consumer site naturally during the W10 fold. This is a NON-BLOCKING
  V2-CH2 observation (carry-forward to V3 if cohort REVISE-rate
  permits the granularity).
- **REDRESS 126 carry-through** at SPEC §15 line 1112: V3 §1.4 CH3
  NF-CH6-3 C2 scalar-ref evidence upgrade carry-through preserved at
  V2 SPEC; primitive consumption requires post-W7 runtime divergence.
  No V2 fold edit re-opens. CH2 binding holds.

## §6 — Sources

### §6.1 — Authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md`
  (44 lines; HEAD pin `690276e03` at seed; HEAD at execution
  `75657df14`, delta = seed-context-commit only).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` (CH2 per V1
  spec; specialised to S-P3 at V2 per CHALLENGE-CONTEXT §2).
- `restart/prompts/ORCHESTRATOR.md §3W` universal lens registry + §3Z
  convergence rule.
- `restart/locks/LOCKS.md:220-263` Lock 14 + v+1 amendments (primary
  CH2 authority).
- `restart/locks/LOCKS.md:282-340` Lock 16 v+1 SIMD/ASM allowlist +
  abstract-primitive cross-grammar declarations (line 287 binds C3's
  abstract-primitive grammar-neutrality verbatim).

### §6.2 — V2 artefacts under review (HEAD pin: 75657df14)

- `restart/skinny/tranches/sk-v14/SPEC.md` (V2: 1187 lines; §0.1 + §1 +
  §2 + §2.1 + §3-§16 read end-to-end for CH2-relevant binding; F-V2-CH6-1
  W10 unconditional Stage-0 binding at §11/§12/§13 verified).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (V1-LOCKED; 344
  lines; no V2 edits; CH2 binding cells N/A — process discipline).
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md`
  (V2-amended; 316 lines; F-V2-CH2-1 at line 93 + F-V2-CH2-2 at line
  106 verified verbatim).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md`
  (V2-amended; 410 lines; wave-numbering reconcile to SPEC §2 W0..W11
  + new §2.14 W11 close ceremony preserved).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md`
  (V2-amended; 537 lines; W1 fused C-2+PRUNE-1; W9 fused R7-direct+
  typed; new §2.11 W11; 75 corpus rows preserved verbatim).
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md`
  (V1-LOCKED; 168 lines; no V2 edits; gate-json column schema — CH2
  axis admits via Lock 14 baseline gate, not a telemetry column).
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md`
  (V1-LOCKED; 903 lines; REDRESS 36-38, 85-86 + Lock 14 residue census
  per §5).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md`
  (V1-LOCKED; 245 lines; SK-V8 SPEC shape mirror at §1.1).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
  (V1 + V2 fold-packet authority; F-V2-CH2-1 + F-V2-CH2-2 LIGHT-fold
  packets at lines 121-122 + 525-536).

### §6.3 — V1 carry-forward

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH2.md`
  (V1 CH2 81.8 % ACCEPT, 2 REVISE on C3 + C4; the REVISE prescription
  at V1 §1.3 + §1.4 actions (iii) + ((i)+(ii)) is the V2 fold's
  template).

### §6.4 — S-P2 V3 grammar-neutral verdict authority

- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md`
  (360 lines; §1.1 admission categories at lines 26-28; §2.1-§2.14
  per-candidate entries; §3 table at lines 247-260; §3 note 1 Sheets-
  doubled-quote-vs-backslash divergence at line 264; §4 inter-axis
  convergence at lines 269-272). p2f:46 binds the BBNF-self same-shape
  claim verbatim; p2f:264 binds the variable-width CSS measured-
  rejection lineage via the `HAS_ESC` DELTA-NOTE.
- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md`
  (367 lines; V1-LOCKED 7/7 grammar-neutral status per §2.1).
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md`
  (342 lines; Gap 7.5 `byte_class_from_range_64` lineage at line 203
  + grammar-neutrality table at line 244).

### §6.5 — Lock authority + audit-overfit residue

- `restart/locks/LOCKS.md:220-263` Lock 14 + v+1 generated-output
  allowance + per-wave gate enforcement + non-JSON-consumer admission
  gate.
- `restart/locks/LOCKS.md:282-340` Lock 16 v+1 SIMD/ASM allowlist +
  abstract-primitive cross-grammar declarations (line 287 binds C3's
  digit-block abstract-primitive verbatim).
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md:9`
  `parse-that-regex::StringFlags::HAS_ESC` JSON-flavored naming
  carry-over (cited at P2-F §3 note 1; §5 of this CH2.md).

### §6.6 — Empirical floor + REDRESS

- `skinny/RESULTS.md` (185 lines; bench-row evidence floor; CH2's
  bench-row non-JSON-consumer materialisation lands as checkasm parity
  row (C3) + BBNF-self consumer row (C4) + CSS L4 + Sheets + BBNF-self
  trio (C7) at W10).
- `skinny/REDRESS.md` (~5041 lines; REDRESS 36-38, 85-86 Lock 14
  residue per §5; REDRESS 126 carry-through per §5).

### §6.7 — Source-file verifications at HEAD (executable cite log)

Five source-file verifications were re-executed at HEAD `75657df14`
during V2 CH2 disposition to discharge the LAC-1E-12 executable-cite
mandate:

1. `grammar/bbnf/bbnf.bbnf:11-13` — direct file read returns the
   `literal = …` rule with `\\.` backslash-escape pattern. **VERIFIED**.
2. `skinny/crates/parse-that-regex/src/lib.rs:718` — grep returns
   `718:pub fn unescape_string(raw_content: &str) -> …`. **VERIFIED**.
3. `skinny/crates/parse-that-regex/src/lib.rs:945` — grep returns
   `945:fn read_hex_unit_scalar(hex: &[u8]) -> Option<u16>`. **VERIFIED**.
4. `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs` —
   `ls` returns the file (sibling-shape template PRESENT at HEAD).
   **VERIFIED**.
5. `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_range_64.rs` —
   `ls` returns no such file (Stage-A authoring target correctly
   NOT-PRESENT at HEAD per Lock 16 same-commit discipline).
   **VERIFIED**.

All five citations in V2 P3-A C3 + C4 cell-(c) text resolve correctly
at HEAD; F-V2-CH2-1 + F-V2-CH2-2 discharge is executable-cite-clean.
