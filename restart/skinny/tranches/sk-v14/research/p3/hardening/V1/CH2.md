# SK-V14 P3 CHALLENGE V1 — CH2 GENERALITY (Lock 14)

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V1.
Date: 2026-05-23.
Lens: CH2 GENERALITY per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` +
`restart/prompts/ORCHESTRATOR.md §3W`. WRITE-ONLY.
HEAD pin: `8f4756113` (per CHALLENGE-CONTEXT.md §0).
Authority: SPEC.md (1137 lines) + DISPATCH-PROMPT.md (344 lines) +
p3a-candidate-shortlist.md (317 lines) + p3b-wave-sequencing.md (405 lines) +
p3c-falsifiability-gates.md (527 lines) + p3d-telemetry-schema.md (168 lines) +
p3e-preblocked-ledger.md (903 lines) + p3f-spec-draft.md (245 lines) +
S-P2 V3 §6 carry-forward packets + Lock 14 v+1 at LOCKS.md:220-263.

## §0 — Disposition headline

**ACCEPT-rate: 9 / 11 cells = 81.8 % ACCEPT, 2 REVISE, 0 REJECT.**

Below the §3Z first-cycle ≥95 % bar but well clear of the V≤5 hard ceiling
crisis line; both REVISE items are scoped tightening, not architectural
re-think. Lock 14 holds in SPEC text; the two REVISEs upgrade in-text-only
narrative to per-candidate executable-cell discipline (the CF-3 V3 lesson
applied to the CH2 axis).

## §1 — CH2 disposition cells (11 cells)

Eleven CH2 cells follow from the V1 disposition focus in CHALLENGE-CONTEXT
§2: (a) each of the 8 shortlist candidates must trace its S-P2
grammar-neutral verdict; (b) SPEC §2.1 generality gate must be present;
(c) SPEC §2.1 must require non-JSON proof per generic-crate edit; (d) no
P3-B wave may let JSON policy into a generic crate.

### Cell §1.1 — C1 `long_string_body_simd_scan` grammar-neutral verdict trace — **ACCEPT**

P3-A §2 C1 entry (`p3a:62-72`) names P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2 as the
upstream P2 IDs (the NF-CH6-4 canonical-name consolidation). The P2-F
verdict for both consolidated rows is NEUTRAL-CONFIG-DRIVEN per the §3 table
at `p2f:247-248`:

> C1 structural-byte SIMD classify | NEUTRAL-CONFIG-DRIVEN | … | Generated
> grammar config (`StructuralByteSet` table emitted by `passes::layout`); no
> hardcoded JSON constants in `bbnf-simd`
> C2 quoted-string boundary scan (PMULL prefix-XOR) | NEUTRAL-CONFIG-DRIVEN
> | … | `QuoteByteSet` table + `EscapeStyle` enum {`Backslash`, `Doubled`,
> `None`} from generated grammar config

Same-wave non-JSON consumer for C1 is implied by P2-F §4 ("CSS L4 consumers:
C1, C2, …"; `p2f:280`) but C1's P3-A §2 entry names ONLY the JSON consumer
`skip_string_plain_trusted` (`p3a:67`); the cross-grammar consumer is
embedded by reference at §1.2 ("CSS L4 declaration-value whitespace consumer
per S-P3 R6 …" reads on C7, not C1). C1's CSS L4 consumer is therefore
implicit-by-table-cite, not explicit in the §2 entry — but the upstream P2-F
verdict NEUTRAL-CONFIG-DRIVEN binds (a primitive parametrised by
`StructuralByteSet` config cannot be JSON-overfit by construction), and the
NF-CH6-4 §6.2 binding at HARDENING-S-P2-V3-CONSOLIDATED §6.2 reinforces the
single-primitive discipline. SPEC §1 (`SPEC.md:222`) re-binds the canonical
naming as a wave-admission gate. CH2 cell holds.

### Cell §1.2 — C2 `structural_index_singular_substrate_consumer` verdict trace — **ACCEPT**

P3-A §2 C2 entry (`p3a:74-85`) names P2-A C1 + C5 ∪ P2-F C11. P2-F C11
verdict at `p2f:257`:

> C11 substrate-walk-with-shape-validation | NEUTRAL-CONFIG-DRIVEN | … |
> Per-rule `TypedShape` enum emitted by `passes::layout`; the walk-and-
> validate primitive is config-free; per P1-E §4.4 substrate-union: the
> primitive is ONE primitive, not two

P2-A's verdict shape for C1/C5 inherits the "7/7 grammar-neutral" V1 LOCK
per `p2a:184-196 §2.1`. C2's substrate-target is `existing_tape` (Lock 1
v+1 declaration triple at `p3a:84`) and policy_owner is `generated_grammar`
— the typed-shape walk is parametrised by per-rule generated `TypedShape`,
not JSON-specific. The CSS L4 / Sheets / BBNF-self consumer surface is the
same `passes::layout`-emitted per-rule validator (`p2f:280-282`). CH2 cell
holds.

### Cell §1.3 — C3 `digit_block_simd_accumulate` verdict trace — **REVISE**

P3-A §2 C3 entry (`p3a:88-98`) names P2-A C3 ∪ P2-C C-P2C-3 ∪ P2-E Gap 5 +
Gap 7 + Gap 7.5 ∪ P2-F C5. P2-F C5 verdict at `p2f:251`:

> C5 digit-block number decode | NEUTRAL-CONFIG-DRIVEN | … |
> `NumberConfig` struct {`allow_leading_dot: bool`, `allow_exponent: bool`,
> `allow_sign: bool`, `require_integer_part: bool`} from generated grammar
> config — already partially in place per the CSS file prose

Lock 16 line 287 (`LOCKS.md:287`) ratifies grammar-neutrality verbatim:
"abstract primitive: byte-window multiply-accumulate … applies to ANY
grammar's digit-block decode, not just JSON: JSON `number`, CSS L4
`<number>`, TOML/INI/SQL integer literals, Sheets formulas". P3-A C3's
same-wave consumer cell at `p3a:93` names "CSS L4 `<number>` consumer per
S-P3 R6 (post-R4 `cargo xtask regen-css`)".

**REVISE — same-wave-consumer-CSS-L4-deferred-to-W8 risk.** Per the wave
manifest at SPEC §2 (`SPEC.md:235-249`) the JSON direct-plane number
consumers for C3 land in W9 (R7 JSON direct + typed re-admit); the CSS L4
`<number>` consumer lands in W8 (R6 CSS L4 re-admit). If C3 admits in W9
under its JSON direct-plane consumer (canada / mesh / numbers / marine_ik)
WITHOUT the CSS L4 `<number>` consumer landing in the SAME wave, then per
Lock 14 v+1 binding at LOCKS.md:259 ("A primitive claimed grammar-neutral
must exercise at least one non-JSON consumer or record a measured
deletion/rejection") the C3 admission is NEUTRAL-PENDING-CONSUMER. P2-F's
own V1 lesson at `p2f:27` ("NEUTRAL-PENDING-CONSUMER … non-JSON consumer
named for the SAME wave (cannot defer to later wave); fails closed if
consumer doesn't materialise") and the V2 demotion of C8 for exactly this
class of slip (`p2f:147`) are the precedent.

P3-A C3 entry cell (c) "same-wave consumer NAMED" lists CSS L4 consumer as
"per S-P3 R6 (post-R4 `cargo xtask regen-css`)" — which is a different wave
(W8) than where the C3 SIMD body lands (W9 or earlier). SPEC §12 W9 exit
gate at `SPEC.md:926-933` does not require a non-JSON consumer for any
admitted primitive; the §2.1 generality gate at `SPEC.md:305` is "for any
generic CostFacts, codegen, runtime, SIMD, or parser-template edit" which
DOES include `bbnf-simd` (C3 owner path per `p3a:90`) but defaults to "a
named no-op dry run, focused test, or unchanged-output audit" — weaker than
the Lock 14 v+1 "at least one non-JSON consumer" gate.

**REVISE action for V2:** C3's P3-A §2 entry cell (c) "same-wave consumer
NAMED" must (i) bind the CSS L4 `<number>` consumer to the SAME WAVE that
admits the SIMD body, OR (ii) carry an explicit NEUTRAL-PENDING-CONSUMER
flag with a same-wave fail-closed clause, OR (iii) bind via the `bbnf-simd`
test surface as the non-JSON consumer (a `bbnf-simd` checkasm row
exercising `byte_class_from_range_64` against the CSS-permissive number
config). SPEC §1 non-negotiables (line 221) must be extended to bind: "Any
wave admitting a `bbnf-simd` primitive carrying a NEUTRAL-CONFIG-DRIVEN P2-F
verdict ships at least one non-JSON consumer in the same wave OR an
explicit `NEUTRAL-PENDING-CONSUMER` declaration with same-wave fail-closed
clause." Without this, C3 risks the C8-class demotion at V2 fold.

### Cell §1.4 — C4 `unicode_escape_neon_nibble_decode` verdict trace — **REVISE**

P3-A §2 C4 entry (`p3a:101-111`) names P2-A C7 ∪ P2-C C-P2C-4 ∪ P2-E Gap 2.
P2-A verdict is the V1-LOCKED "7/7 grammar-neutral" set; P2-C C-P2C-4's
verdict inherits the V2-amended P2-C status; P2-E Gap 2's verdict per the
Lock 16 enumeration at `LOCKS.md:283-307` is grammar-neutral by construction
(arm64 NEON nibble decode is hardware, not grammar). P3-A C4 entry cell (c)
at `p3a:106` names the CSS L4 consumer as "CSS L4 escaped-ident row per
S-P3 R6 (CSS Syntax L3 §4.3.7 variable-width hex escape `\HEXHEX`)".

**REVISE — shape divergence between JSON `\uXXXX` and CSS L4 `\HEXHEX`
unicode escape forms.** The JSON `\uXXXX` form is a FIXED 4-nibble hex
window (per RFC 8259 §7); the CSS L4 escape form per CSS Syntax §4.3.7 is a
VARIABLE 1-6 hex digits followed by an OPTIONAL whitespace terminator. The
two forms have structurally distinct SIMD body shapes — the JSON form
admits the 8× batched `_x8_neon` form named in C4 (`p3a:103`); the CSS form
requires variable-width termination logic incompatible with the fixed-width
SIMD batch. P3-A C4 entry claims CSS L4 escaped-ident as a same-wave
consumer, but the SIMD body for the variable-width form is not the
`unescape_uxxxx_x8_neon` body C4 admits.

The same-wave consumer cell therefore names a consumer that does NOT
exercise the SIMD body being admitted, violating the §6.1 CF-3 "same-wave
consumer NAMED" discipline — the named consumer is grammar-shape-orthogonal
to the primitive. Per Lock 14 v+1 at LOCKS.md:259 the non-JSON consumer
must "exercise" the primitive; a consumer that requires a different
primitive (variable-width hex decode) does not exercise the
`unescape_uxxxx_x8_neon` SIMD body.

**REVISE action for V2:** C4's P3-A §2 entry cell (c) must (i) name a
non-JSON consumer that exercises THE SAME SIMD body (e.g. a Sheets or
BBNF-self string-with-fixed-4-nibble-`\u`-escape consumer — Sheets has no
backslash escapes per P2-F §3 note 1; BBNF-self uses JSON-shape per P2-F
§2.7), OR (ii) explicitly carve C4 as JSON-only-by-shape with a "measured
deletion/rejection" record per Lock 14 v+1 (the variable-width CSS form
demonstrably needs a different primitive), OR (iii) admit a second
candidate C4b for the CSS L4 variable-width form via the existing primitive
allowlist and bind the two as paired admissions. BBNF-self under (i) is
the cleanest non-JSON exercise.

### Cell §1.5 — C5 `parse_attribution_envelope_cracker` verdict trace — **ACCEPT**

P3-A §2 C5 entry (`p3a:113-124`) is N/A on substrate (process discipline,
not a kernel). The grammar-neutral verdict applies to the cargo-feature
machinery, not a SIMD primitive — `parse-attribution` cargo feature at
`runtime` crate is itself grammar-agnostic infrastructure (per
HARDENING-S-P2-V3-CONSOLIDATED §6.3). The 12 consumer-dependency primitives
the rerecord SERVES (per the §6.3 verbatim list at `p3a:119`) are
themselves grammar-neutral verdicts inherited through C1/C3/C7 admission
cells. C5's grammar-neutrality is "by construction, no primitive added".
CH2 cell holds.

### Cell §1.6 — C6 `force_inline_lto_envelope_discipline` verdict trace — **ACCEPT**

P3-A §2 C6 entry (`p3a:126-137`) names P2-A C4 ∪ P2-F C14. P2-F C14
verdict at `p2f:260`:

> C14 i-cache budget constraint | NEUTRAL-WIRED | … | cross-grammar by
> construction

Lock 15 (`LOCKS.md:265-281`) binds the i-cache + LTO + force-inline
discipline as hardware-only, not grammar-only. The BBNF-FORCE-INLINE-MISSED
+ BBNF-ICACHE-BUDGET-EXCEEDED diagnostics emit per-grammar uniformly;
"NEUTRAL-WIRED" verdict at `p2f:270` holds. CH2 cell holds.

### Cell §1.7 — C7 `ascii_whitespace_skip_64` verdict trace — **ACCEPT**

P3-A §2 C7 entry (`p3a:140-150`) names P2-E Gap 3 ∪ P2-F C7. P2-F C7
verdict at `p2f:253`:

> C7 leading-whitespace prefix skip | NEUTRAL-CONFIG-DRIVEN | … |
> `WhitespaceByteSet: [bool; 256]` from generated grammar config (per-
> grammar `@ws` directive)

P3-A C7 entry cell (c) at `p3a:145` explicitly names the cross-grammar
same-wave consumer trio: "CSS L4 declaration-value whitespace consumer per
S-P3 R6 (set `{0x20, 0x09, 0x0A, 0x0D, 0x0C}`); Sheets whitespace consumer;
BBNF-self whitespace consumer." This is the gold-standard CF-3
same-wave-non-JSON-consumer naming — three non-JSON grammars by explicit
byte-set difference (CSS adds `0x0C` form-feed). The C7 checkasm cell at
`p3a:144` already includes "CSS-form-feed `0x0C` (5-byte set for CSS L4
consumer per CSS Syntax §4.2)" in the matrix coverage. Lock 14 v+1 holds
cleanly. CH2 cell holds; this is the canonical CH2-clean entry.

### Cell §1.8 — C8 `BackendShape::SinkOnly` activation verdict trace — **ACCEPT**

P3-A §2 C8 entry (`p3a:153-163`) names P2-D C-P2D-1 + C-P2D-2. P2-D
V2-LOCKED status with C-P2D-1 active inherits the SinkOnly verdict — a
codegen-lowering decision parametrised on `LayoutFacts.backend_shape[rule_id]`,
which is per-rule from the grammar source, not per-grammar. The
`OffsetTapeStats` substrate-measurement column applies uniformly to all
grammars. CSS L4 / Sheets / BBNF-self consumer coverage is via the same
codegen template (post-W5 PRUNE-3) emitting SinkOnly for every grammar's
declared sink-only rules — naturally cross-grammar by construction. C8's
substrate target is `direct_sink` (Lock 1 v+1); no JSON-specific policy in
the lowering pass. CH2 cell holds.

### Cell §1.9 — SPEC §2.1 generality gate presence — **ACCEPT**

`SPEC.md:294-313` carries §2.1 "Generality And Lock 14 Gate" verbatim from
SK-V8 + SK-V14 R3 PRUNE-3 extension. The six gate checks are explicit:
public API scan, grammar branch scan, primitive/table scan, role/fact
boundary, template/provider boundary (post-W5), non-JSON proof, forward
invariant (post-W5 permanent). The SK-V8 SPEC shape is mirrored verbatim
per §1.1 in p3f-spec-draft.md (`p3f:27`). Cell holds.

### Cell §1.10 — SPEC §2.1 non-JSON proof requirement per generic-crate edit — **ACCEPT**

`SPEC.md:305` reads verbatim:

> **Non-JSON proof**: CSS L4, Sheets, and BBNF-self must compile, lower,
> cost, or run without JSON structural roles for any generic CostFacts,
> codegen, runtime, SIMD, or parser-template edit. Acceptable proof is a
> named no-op dry run, focused test, or unchanged-output audit.

The clause names CSS L4 / Sheets / BBNF-self triad (the three non-JSON
grammars per Lock 14 v+1 admission gate). The scope "generic CostFacts,
codegen, runtime, SIMD, or parser-template edit" covers `bbnf-simd` (where
C1/C3/C4/C7 land), `passes::layout` (where C2/C8 land), `codegen` (where
C2/C6/C8 land). Acceptable proof forms (no-op dry run / focused test /
unchanged-output audit) are weaker than executable-bench rows but
gate-enforceable. Cell holds. **See Cell §1.3 REVISE for the per-candidate
strengthening that closes the C3 W9-only-admit loophole.**

### Cell §1.11 — No P3-B wave lets JSON policy into a generic crate — **ACCEPT**

Per-wave audit:

| Wave | Generic-crate touch | JSON policy leak? |
|---|---|---|
| W0 | telemetry-only; no generic-crate behavior change | NO — §1 binding excludes behavior |
| W1 | `bbnf-bench/benches/json_parity.rs` (not generic) + `xtask/gate.rs` (gate-only) | NO — single-lane sonic_rs_anchor DELETED (P-2 fix); no JSON policy added |
| W2 | `xtask/src/regen_css.rs` + xtask binary | NO — Lock 14 baseline gate at SPEC.md:490: "zero grammar-named branches in xtask itself; the `regen_css.rs` module name is the only css-named identifier" |
| W3 | `bbnf-bench/src/css_l4_corpus.rs` (not generic; CSS-specific loader) | NO — corpus loader is per-grammar fixture, not generic crate |
| W4 | DELETIONS only — 7 CSS templates + 24 CSS admits reverted | NO — net negative LOC; deletions |
| W5 | `passes/src/lib.rs` + `codegen/src/lib.rs:167-209` trait dispatch + `codegen/src/grammar_provider.rs` (NEW; consumes grammar source + workspace metadata, not JSON-specific) | NO — explicit Lock 14 gate at SPEC.md:656-657: `find skinny/crates -name '*.rs' \| xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar' \| wc -l == 0`; non-JSON proof per §2.1 at SPEC.md:658 |
| W6.1..W6.9 | per-grammar runtime collapse onto W5 template | NO — generated output goes under `runtime/src/grammars/<name>/` per Lock 14 v+1 generated-output allowance at LOCKS.md:222-229; per-sub-wave Lock 14 baseline gate at SPEC.md:736 |
| W7 | `passes/src/` + `codegen/src/` + `runtime/src/` CSP-shape wire-up | NO — explicit gate at SPEC.md:810: "The shape consumer dispatches on `BackendShape` alone — Lock 14 grep `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>\|CssL4\s*=>' skinny/crates/codegen/src/lib.rs` returns ZERO matches"; cross-grammar two-grammar minimum at SPEC.md:794 |
| W8 | CSS L4 re-admit; `codegen/` + `runtime/src/grammars/css_l4_*/` (generated) | NO — generated output under generated allowance; explicit gate at SPEC.md:871: "Lock 14 + non-JSON proof pass; no grammar-name branches reintroduced" |
| W9 | `bbnf-bench/benches/json_parity.rs` + `real_typed_struct.rs` (bench, not generic) + `codegen/` only if needed | CONDITIONAL — SPEC.md:932: "Lock 14 + non-JSON proof pass if generic code changed" — see §1.3 REVISE for C3 same-wave-consumer slip risk |
| W10 | `runtime/src/grammars/json/parser.rs` (per-grammar generated allowance) + `codegen/src/` emit parse_only via W5 template | NO — emitted via W5 grammar-agnostic generator; SPEC.md:980: "Emit the parse_only path via the W5 grammar-agnostic generator template + workspace metadata `parse_only=true` shape (per `[no-orthogonal-codepaths]` — one collection strategy)" |
| W11 | docs only | NO — close ceremony |

The audit-ledger shows ZERO waves admit JSON policy into a generic crate
under V1 SPEC text. The W5 + W6 sequence explicitly REMOVES per-grammar
providers + per-grammar runtime files from generic crates (the C-1 PRUNE-3
+ PRUNE-4 work). Cell holds.

## §2 — Inter-axis convergence audit (CH2-relevant)

### §2.1 — S-P2 V3 carry-forward §6.2 NF-CH6-4 wired

The three convergent identifiers for the long-string SIMD primitive (P2-A
C2 `long_string_body_simd_scan` + P2-E Gap 1 `scan_string_special_block_sweep_64`
+ P2-F C1+C2 quote-aware classifier) consolidate to ONE canonical name per
the SPEC §1 non-negotiable at `SPEC.md:222` and the SPEC §15 pre-block at
`SPEC.md:1110`. P3-A C1 entry honours the canonical-name binding ("canonical
name; NF-CH6-4 consolidation" at `p3a:61`). The orthogonal-SIMD-bodies
admission failure mode is pre-blocked at SPEC.md:1110 verbatim. CH2 binding
holds.

### §2.2 — S-P2 V3 carry-forward §6.1 CF-3 admission 3-gate

Every shortlisted candidate at P3-A §2.1 (`p3a:170-178`) carries the 3-gate
cell explicitly: scalar-ref status / checkasm-parity expectation /
same-wave-consumer NAMED. P3-A §2.1 footer at `p3a:182` declares "8/8
candidates carry the 3-gate cell explicitly." The CH2 lens reads cell (c)
"same-wave consumer NAMED" as the CH2 binding point — and finds two cells
(C3 and C4) where the same-wave consumer named does not satisfy the Lock 14
v+1 "exercise at least one non-JSON consumer" gate cleanly (see §1.3 + §1.4
REVISE). The CF-3 discipline is present in form; the V2 fold must tighten
the C3 + C4 entries to make cell (c) executable for the CH2 axis as well as
the CH4 cost axis.

### §2.3 — S-P2 V3 carry-forward §6.3 F-V2-P1ABC-RERECORD

C5 binds the Stage-0 wave commitment per `p3a:113-124` + SPEC §1 line 221.
The 12 consumer-dependency primitives are CH2-grammar-neutral by P2-F
verdict (verified via §1.1, §1.3, §1.7 above for C1, C3, C7). The CH2 axis
is not affected by the rerecord packet directly; the rerecord enables
measurability of inner primitives whose CH2 verdicts are already bound.
CH2 binding holds.

## §3 — Falsifiability binding (CH2-axis specific)

CH2 measurability is two-fold:

1. **Compile-time / source-level** — Lock 14 baseline gate greps (per
   SPEC.md:656-657, SPEC.md:736, SPEC.md:810) measure JSON-name presence
   in generic crates; ZERO is the bound. These gates are executable from
   the shell and gate-json-enforceable per the SPEC §0.4 non-negotiable.

2. **Bench-row / runtime-level** — Per Lock 14 v+1 "must exercise at least
   one non-JSON consumer or record a measured deletion/rejection" — the
   non-JSON consumer materialises as a CSS L4 / Sheets / BBNF-self bench
   row in `skinny/RESULTS.md`. C7 cell (§1.7) carries this binding
   cleanly; C3 and C4 cells (§1.3 + §1.4) do not, hence the REVISE.

The §1.11 per-wave ledger is the CH2 gate's per-wave falsifiability
binding; every wave's exit gate carries the Lock 14 grep + non-JSON proof
per §2.1. CH2 axis is gate-enforceable at every wave commit.

## §4 — Pre-blocked routes (REDRESS entries CH2 must enforce)

Per CHALLENGE-CONTEXT §1 + SPEC §15:

- **REDRESS 36-38, 85-86**: Lock 14 residue + old JSON helpers + generic
  JSON branches + `StructuralAlphabet::json`. SPEC §15 line 1095 binds.
  W5 PRUNE-3 (SPEC §8) collapses the 8 per-grammar providers — directly
  closes 36-38 cluster. W7 BackendShape-only dispatch at SPEC.md:810
  closes 85-86 cluster. No shortlist candidate re-opens these.
- **§2.Y NF-CH6-4 three-orthogonal-SIMD-bodies**: SPEC §15 line 1110 binds.
  C1 consolidation honours the bind (§1.1 above).
- **Lock 14 v+1 generated-output allowance scope** at LOCKS.md:222-229:
  generated files under `runtime/src/grammars/<name>/` may contain grammar
  names ONLY when emitted from the rostered generator. SPEC §1 line 224
  binds; W6 PRUNE-4 (SPEC §9) operationalises (collapse 67 hand-written
  per-grammar runtime files onto W5 template).
- **`parse-that-regex::StringFlags::HAS_ESC` JSON-flavored naming carry-
  over** at `sk-v14-audit-overfit-lock14-scan.md:9` (cited at P2-F §3 note
  1, `p2f:264`): "the S-P3 wave must rename `HAS_ESC` and lift the
  alphabet". P3-A C4 entry §2 (`p3a:103-104`) names `unescape_string` as
  the consumer site and mentions `:718` carries the violation, but neither
  the C4 entry nor the W9 SPEC §12 wave gate explicitly binds the
  `HAS_ESC` rename + alphabet lift as part of the wave that admits C4.
  This is a CH2-axis Lock 14 residue REDRESS that the V2 fold should fold
  into C4's same-wave consumer cell or into a separate W9 task.

## §5 — Sources

### §5.1 — Authority

- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CHALLENGE-CONTEXT.md` (42 lines; HEAD `8f4756113`).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 CH2 (`PASS-3-SYNTHESIS-PLAN.md:116-120`).
- `restart/prompts/ORCHESTRATOR.md` §3W universal lens registry + §3Z convergence.
- `restart/locks/LOCKS.md:220-263` Lock 14 + v+1 amendments (primary CH2 authority).

### §5.2 — V1 artefacts under review

- `restart/skinny/tranches/sk-v14/SPEC.md` (1137 lines; §0.1 + §1 + §2 + §2.1 + §3-§16 read end-to-end for CH2-relevant binding).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (344 lines; CH2 binding cells not directly relevant — process discipline).
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (317 lines; per-candidate verdict trace; §1.1 distillation discipline + §2 candidate entries C1..C8 + §2.1 footer + §5.1 P2 source authority cited).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (405 lines; per-wave generic-crate-touch census for §1.11).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (527 lines; CH2 gate measurability binding at SPEC §2.1 + §1.11 wave entries).
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (168 lines; gate-json column schema — CH2 axis admits via Lock 14 baseline gate, not a telemetry column).
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (903 lines; REDRESS 36-38, 85-86 + Lock 14 residue census per §4).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (245 lines; SK-V8 SPEC shape mirror at §1.1 + §1.3 CF-3 + §1.4 sequencing constraints).

### §5.3 — S-P2 V3 grammar-neutral verdict authority

- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (360 lines; §1.1 admission categories at lines 26-28; §2.1-§2.14 per-candidate entries; §3 table at lines 247-260; §4 inter-axis convergence at lines 269-272; §5 V2 demotion at lines 274-277).
- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines; V1-LOCKED 7/7 grammar-neutral status per §2.1).
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (164 lines; V2-LOCKED P2-C verdicts inheriting Lock 16 cross-grammar evidence).
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (254 lines; V2-LOCKED P2-D verdicts for C-P2D-1 SinkOnly + C-P2D-2 OffsetTapeStats).
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines; V1+V2-LOCKED P2-E gap verdicts via Lock 16 enumeration).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (668 lines; §6.1 CF-3 admission 3-gate at lines 466-498; §6.2 NF-CH6-4 canonical-name binding; §6.3 F-V2-P1ABC-RERECORD Stage-0 wave commitment).

### §5.4 — Lock authority + audit-overfit residue

- `restart/locks/LOCKS.md:220-263` Lock 14 + v+1 generated-output allowance + per-wave gate enforcement + non-JSON-consumer admission gate.
- `restart/locks/LOCKS.md:265-281` Lock 15 (LTO + force-inline; binds C6).
- `restart/locks/LOCKS.md:282-340` Lock 16 v+1 SIMD/ASM allowlist + abstract-primitive cross-grammar declarations (lines 285-290 + 299).
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-lock14-scan.md:9` `parse-that-regex::StringFlags::HAS_ESC` JSON-flavored naming carry-over (cited at P2-F §3 note 1; §4 of this CH2.md).

### §5.5 — Empirical floor + REDRESS

- `skinny/RESULTS.md` (185 lines; bench-row evidence floor; CH2's bench-row non-JSON-consumer materialisation lands as CSS L4 / Sheets / BBNF-self rows in this file).
- `skinny/REDRESS.md` (~5041 lines; REDRESS 36-38, 85-86 Lock 14 residue per §4).
