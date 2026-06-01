# SK-V18 S-P2 CHALLENGE — CH5 SHEETS-PROOF-RIGOR (cycle V1)

Adversarial review of R-E (`rE-sheets-proof.md` + SYNTHESIS-RESEARCH §1 R-E row, §5 risk 5).
Lens: is the recommended PROVE candidate (R-E-2) a GENUINE third-grammar negative control, or a
relabeled-JSON hollow litmus / contrivance / shim-in-waiting? Every Sheets-proof claim enumerated,
spot-verified against `grammar/google-sheets/google-sheets.bbnf` (185 LOC) and the codegen tree at
cwd, then judged ACCEPT / REVISE / REJECT.

Verification corpus this pass (all on disk):
- `grammar/google-sheets/google-sheets.bbnf` read whole.
- `grammar/json/json.bbnf`, `grammar/css/l4/*.bbnf` for the comparison census.
- `skinny/crates/codegen/src/runtime_generator.rs` (emit paths), `grammar_provider.rs` (fork + gate).
- `skinny/crates/runtime/src/grammars/sheets_witness/` (the witness stub).
- `skinny/crates/codegen/src/lib.rs:1049` (the fail-closed test).

---

## VERDICT SUMMARY

The CORE thesis of R-E-2 SURVIVES: the precedence tower is real, structurally distinct from JSON
and CSS, and is the load-bearing litmus. The fallback discipline (§5) is honest and binding — no
stub-prove, no `_GENERATED_RS` Sheets blob. The witness is correctly characterized as a hollow
25-LOC test stub, not a proof. The emit-path baseline (§0.2) is precisely grounded.

BUT the dual-construct novelty claim is HALF FALSE and must be revised: the **Nu8-tagged-alt family
is NOT a construct JSON+CSS structurally lack** — CSS L4 uses `-> Nu8u8` tagged-alts **295 times**
across its imported modules versus Sheets' **21**. The digest props the litmus on TWO novel
constructs when only ONE (the precedence tower) is genuinely Sheets-distinctive. This does not sink
R-E-2 (one genuine novel construct is sufficient for a non-hollow proof), but the §1 census and the
§3 "TWO genuinely-novel constructs" framing are inaccurate and would mislead the PROVE gate's
non-hollowness criterion. Plus several census counts are off (Nu8 21≠24, even against the digest's
OWN breakdown which sums to 21; projections 32≠28; TODO host-captures 2≠4).

---

## CLAIM-BY-CLAIM

### C1 — §0: "PROVE is a negative control; today it FAILS by construction (3 ways)." → ACCEPT
All three baseline failures verified on disk:
- (1) Fail-closed test exists at `lib.rs:1049` (`w5a_sheets_bbnf_fail_closed_through_runtime_contract`).
  Sheets has NO `@import` (verified: `grep -c '@import' google-sheets.bbnf` == 0); the gate
  `grammar_provider.rs:263` is `if requirements.import_closure && frontend.imports.is_empty()` —
  exactly the rejection path the digest describes.
- (2) No grammar-agnostic emitter: `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork confirmed
  at `grammar_provider.rs:40-42`; CSS `normalize(CSS_GENERATED_RS)` verbatim const at
  `runtime_generator.rs:91`/`:701`; JSON `emit_compiled` at `:29` hardcodes
  `include_str!("json_templates/...")` + `JSON_PARSE_ONLY_*` (`:33-64`) — template-driven, not
  grammar-driven. The claim "routing Sheets through emit_compiled emits a JSON parser" is sound.
- (3) Phantom `<G>` test-only: `SheetsEventGrammar` appears ONLY in `event_grammar_tests.rs`
  (`_proof_compiles::<>`) and the witness stub — zero production consumer. Verified.
This is the honest baseline. ACCEPT.

### C2 — §0.1 + §1: "The witness is a 25-LOC stub, NO .bbnf consumed, NO RuntimeTarget row." → ACCEPT
`sheets_witness/event_grammar_witness.rs` is 24 lines: a `SheetsEventGrammar` unit struct + a
`SheetsFactId(u8)` with 3 consts + an `EventGrammar` impl (`STRUCTURAL_CLASS_COUNT=5`,
`admits_fact` matches `0..=2`). It consumes NO `.bbnf`, emits NO runtime parser, and carries NO
`RuntimeTarget` row in any xtask table. The digest's framing — that this is a placeholder the proof
must REPLACE with generated output, not a courier to relabel — is exactly right and is the correct
honest posture. ACCEPT.

### C3 — §1: "The precedence tower is a 7-level left-assoc tower, structurally absent from JSON+CSS." → ACCEPT
Verified the full chain on disk (`:103`→`:121`):
`comparison_expr → concat_expr → add_expr → mul_expr → exp_expr → unary_expr → postfix_expr → primary`
— 7 distinct precedence rules above `primary` (5 left-assoc binary `B (op B)*` levels + unary prefix
+ postfix suffix). The cyclic recursion is real: `paren_expr = "(" , expression ?w , ")"` (`:137`),
`expression = comparison_expr` (`:163`), `primary` includes `paren_expr` (`:135`) — closing the
cycle `primary → paren_expr → expression → comparison_expr → … → primary`. JSON has ZERO `_expr`
rules (flat `value → object/array → value`, 2-hop, `json.bbnf:11/14/16`); CSS L4 has ZERO
operator-precedence `_expr` towers across all modules. This is the genuine, sole Sheets-distinctive
construct, and it is correctly load-bearing. ACCEPT.

### C4 — §1: "The two genuinely-novel litmus-load-bearing constructs are TWO: (a) precedence tower; (b) the dense Nu8-tagged-alt family." → REJECT
The Nu8-tagged-alt family is NOT a construct JSON+CSS structurally lack. Disk census:
- Sheets `-> Nu8u8`: **21** (error_literal 9, compare_op 6, add_op 2, mul_op 2, unary_prefix 2).
- CSS L4 `-> Nu8u8` across imported modules: **295** (keywords.bbnf 86, color.bbnf 18, selectors.bbnf
  13, filters.bbnf 10, easing.bbnf 7, func-body.bbnf 6, values.bbnf 6, media.bbnf 3, …).
CSS uses the tagged-alt-to-small-enum discriminant **14× more** than Sheets. The §1 table column
"CSS stylesheet: 3" is true ONLY because it scopes to the single `stylesheet.bbnf` file and ignores
that the un-forked generator emits the entire CSS L4 import closure — the very closure the G2 wave
lowers. Calling the Nu8 family "genuinely-novel" for the generator is false; the generator must
already handle Nu8 tagged-alts at scale to emit CSS at all. The honest position: the precedence
tower is the SOLE Sheets-distinctive litmus construct; the Nu8 family is a SHARED construct (like
`<<`, span regex, `?w`) that proves no new generality. R-E-2's recommendation still stands on the
one genuine construct, so this REJECT is of the dual-novelty FRAMING, not of the candidate.
EDIT (`rE-sheets-proof.md` §1 closing para + §3 bullet 1): strike "TWO" / "the dense Nu8-tagged-alt
family" from the novel set; demote Nu8 to the shared row alongside `<<`/`?w`/span-regex; state the
litmus rests on the precedence tower ALONE. Also fix the SYNTHESIS §1 R-E justification cell which
repeats "the 24-member Nu8-tagged-alt family" as a novelty.

### C5 — §1 census counts (projections 28, Nu8 24, bool 2, ?w 23, << 4, host-capture "0 live/4 TODO"). → REVISE
Spot-verified each:
- `->` projections: claimed **28**, actual **32** (incl. 4 bool-alt). Undercount.
- `-> Nu8u8`: claimed **24**, actual **21**. The digest's OWN §1 parenthetical breakdown
  ("error_literal 9, compare_op 6, add_op/mul_op/unary_prefix 2ea") sums to **21** — internally
  self-contradicting its "24" header. The "24-member" phrase recurs in §0, §3, and the SYNTHESIS §1
  cell; all are wrong by 3.
- bool-alt `-> true/false`: claimed **2**, actual **2** (lines 17-18). ACCURATE.
- `?w`: claimed **23**, actual **23**. ACCURATE.
- `<<`: claimed **4**, actual **4**. ACCURATE.
- `@import`/`@ws`/`@token`: claimed NONE, actual NONE. ACCURATE.
- host-capture: claimed "0 live, 4 in TODO comments". Live count **0** ✓; but only **2** TODO
  comments name a decode function (`:11` decode_sheets_string_to_arena, `:62` decode_cell_ref) — the
  `:73-74` TODO names a variant-tagged enum, no `decode_` symbol. "4 in TODO comments" overcounts.
These are non-load-bearing arithmetic errors but they erode the digest's "every claim grounded on
disk" assurance, and the "24" propagates into the SYNTHESIS gate predicate. EDIT
(`rE-sheets-proof.md` §1 table + SYNTHESIS §1 cell): 28→32 projections, 24→21 Nu8 (the whole "24"
family-size phrase wherever it appears), "4 TODO host-captures"→2.

### C6 — §3: "R-E-2 defers cell_ref/range/LET/LAMBDA because the grammar ITSELF defers them as raw `-> input : Span`, citing :62 and :74." → ACCEPT (with one line-number nit)
The deferral is HONEST, not a dodge — verified:
- `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> input : Span` (`:63`), with the explicit
  `// TODO AU.6.7: -> decode_cell_ref(input) : CellRef` at `:62`. The grammar projects a raw Span
  identical in shape to JSON's `string -> input : Span` — so emitting it proves no NEW generality.
- `range_end = cell_ref | /regex/ | /regex/` (`:75`) left structural with the
  `// TODO AU.6.7: variant-tagged enum (Cell | Column | Row)` comment at `:73-74`.
- `LET`/`LAMBDA` (`let_call :151`, `lambda_call :155`) are present but their aggregate payloads are
  not typed beyond the deferred-arena note.
The deferral defers EXACTLY what the grammar itself leaves untyped — the contrivance-avoidance test
the lens demands passes in BOTH directions (defers nothing the grammar types; emits nothing the
grammar defers). NIT: §3 and §1 cite `:74` for the range_end TODO, but the TODO opens at `:73` and
`:74` is its continuation line; range_end is at `:75`. Cosmetic. ACCEPT, with EDIT
(`rE-sheets-proof.md` §3) `:74` → `:73-75` for precision.

### C7 — §2/§3: "R-E-2 (precedence-tower core) is the minimal HONEST proof; R-E-3 (flattened precedence) is REJECTED as a third-JSON hollow litmus." → ACCEPT
The candidate ladder is sound. R-E-1 (maximal) carries the highest regression surface; R-E-3
(flatten the tower to one `primary (op primary)*` level) erases the `pratt-operator` shape →
degrades `sheets_grammar_shape` to `flat-stream`, which is precisely the hollow third-JSON the lens
warns against — correctly REJECTED. R-E-2 keeps the single hardest lowering (recursive
`CallRule`/`RepeatLoop` precedence descent) while bounding the surface to what the grammar types.
This is the right minimal-honest choice. ACCEPT.

### C8 — §3 Pratt note: "The tower is right-iterated EBNF `A = B (op B)*`, lowers to existing `{Seq,RepeatLoop,Alt{Dispatch},CallRule}` — no new Pratt IR primitive; stress is on G3's generality." → ACCEPT
Verified the grammar encodes precedence as right-iterated EBNF, NOT left-recursion needing rewrite:
e.g. `add_expr = mul_expr ?w , (add_op ?w , mul_expr ?w) *` (`:109`) — a `Seq` of a `CallRule` and a
`RepeatLoop` over an `Alt`+`CallRule`. No left-recursive `add_expr = add_expr op mul_expr`. So the
claim that the tower needs NO new IR primitive (only G3 rendering recursive `CallRule` chains from
grammar structure) is correct. The honest §6 escape hatch (if a construct outside the lowering
vocabulary is reached → a named, .bbnf-invoked, parameterized, checkasm-referenced primitive, never
a silent blob) is the right discipline. ACCEPT.

### C9 — §4: "Sheets generated.rs held md5-distinct AND branch-free via 4 co-gates (md5-distinct, branch-count==0, type-count==0, row-collapse with distinct grammar_name)." → ACCEPT
The conjunction is correct and md5-distinct is correctly flagged necessary-not-sufficient. The
branch-count grep (`GoogleSheets =>`/`Sheets\w* =>` arm census) and type-count grep
(`GoogleSheetsParser`/`EventGrammar` literal) are the right structural co-gates against a relocated
seam. The witness-emission coupling note (if G3 emits a Sheets `EventGrammar` literal,
`FORBIDDEN_GENERIC_TOKENS` must carry `EventGrammar`/`*EventGrammar`; Sheets is the FIRST grammar to
exercise this) is well-reasoned and consistent with the witness stub's `SheetsEventGrammar` type.
ACCEPT.

### C10 — §4 + §5: "Import-closure relaxation is a frontend-requirements DATA change (present-iff-grammar-has-imports), NOT a `match grammar` arm." → ACCEPT
This is the one frontend change Sheets forces and it is correctly diagnosed as the honest cause of
today's fail-closed (`@import` count == 0; gate at `grammar_provider.rs:263`). The insistence that
it be derived from grammar facts rather than asserted by the contract — and must NOT become a
grammar branch — is exactly the right line. Note `grammar_provider.rs:62` already shows an
`import_closure: false` requirement variant exists, so the data-change path is structurally available
(not a new mechanism). ACCEPT.

### C11 — §5: "A hand-written shim / Sheets-specific emit branch / relabeled `_RS` blob / flattened-precedence third-JSON each FALSIFIES generality and is a REJECT; none patched silently; PROVE does not paper-close." → ACCEPT
This is the negative-control teeth and it is the strongest part of the digest. The four falsifiers
((a) `const SHEETS_GENERATED_RS` courier; (b) `GoogleSheets =>` arm; (c) `sheets_grammar_shape !=
pratt-operator`; (d) value type can't instantiate G4 trait without LCD-flatten) are each tied to an
executable check, and the fallback ("if Sheets cannot emit via the generator ONLY, generalization is
NOT real — surface honestly, do NOT stub-prove, do NOT hand-write a `_GENERATED_RS` Sheets block")
is verbatim-binding and honest. This fully discharges the lens's "is the fallback honest?" criterion.
ACCEPT.

### C12 — §6: "KEY RISK (MED-HIGH): the un-forked G3 emitter does not exist yet; both emit paths are grammar-specialized couriers; Sheets is the FIRST grammar whose body cannot be a relabeled courier." → ACCEPT
Verified both couriers (C1). Sheets' precedence tower + cyclic recursion are structurally unlike
both JSON's `{`/`[` template dispatch and CSS's delimiter-scan const, so its body genuinely cannot
be a relabel — this makes Sheets a true test of grammar-DERIVED emission, exactly as the lens
requires. The risk is correctly rated MED-HIGH and correctly identifies the precedence tower as the
first break point. ACCEPT.

---

## NET JUDGMENT FOR THE LENS

- Is R-E-2 a GENUINE negative control? **YES, on ONE construct.** The precedence tower + cyclic
  `paren_expr → expression` recursion is real, verified, and structurally absent from both JSON and
  CSS — sufficient to make the litmus non-hollow.
- Does it exercise constructs JSON+CSS structurally LACK? **The tower: yes. The Nu8 family: NO** —
  CSS exercises Nu8 295× vs Sheets 21×. The dual-novelty claim is the one substantive defect (C4).
- Is it non-hollow (not a "third JSON")? **YES** — R-E-3 (the flat third-JSON) is correctly rejected;
  R-E-2 preserves the `pratt-operator` shape.
- Does it avoid contrived constructs? **YES** — defers exactly the cell_ref/range/LET/LAMBDA
  aggregates the grammar itself leaves as raw Spans / TODO AU.6.7 (C6, verified `:62`).
- Is the fallback honest? **YES** — no stub-prove, no `_GENERATED_RS` Sheets blob; the witness is
  correctly a placeholder to replace, not relabel (C2, C11).

Required revisions before S-P3 binds R-E-2: fix the Nu8 novelty framing (C4, REJECT the dual-novelty
claim — tower is the sole novel construct) and correct the census arithmetic (C5: Nu8 21 not 24,
projections 32 not 28, TODO host-captures 2 not 4; propagate the "24"→"21" fix into SYNTHESIS §1).
The candidate selection (R-E-2 over R-E-1/R-E-3), the deferral honesty, the md5-distinct co-gates,
the import-closure data-change, and the no-shim fallback all ACCEPT as written.

---

TALLY accept=10 revise=1 reject=1
