# CH2 — GENERALITY (V1)

Lens: CH2 Generality (PASS-ALPHA §3). Reviewer focus: **does the goalset respect
Lock 14 (one generator ALL grammars); are the interventions grammar-neutral; will they
work for non-JSON grammars (CSS L4 / Sheets / BBNF-self); is the Sheets 3rd-grammar
proof load-bearing?** Subject: SK-V18 = the GENERALIZATION cycle (the inflection
backtrack). Date 2026-05-31. Bracket HEAD `318d9c046`; SK-V17 close `f6a38445b`.

Method: every disposition cites `path:line` / SHA / artefact-line, verified live at
HEAD where checkable. Disposition vocabulary: ACCEPT / REVISE / REJECT per
artefact-section.

---

## §0 — Lens verdict (one paragraph)

The SK-V18 contract is, at its core, a **generality cycle done right**: the goalset's
spine — ONE grammar-driven generator emitting JSON + CSS + Sheets from `.bbnf`, the
phantom `<G>` instantiated-or-deleted, the `RuntimeEmitterKind` fork retired, N
non-identical `generated.rs` as the litmus — is the precise Lock-14 obligation, and the
Sheets proof IS load-bearing because (verified) a real `grammar/google-sheets/
google-sheets.bbnf` exists and is a **genuinely different shape** (Pratt formula
operators, error literals, cell references — not a third JSON; §3 below). The
generalization machinery the contract leans on (`SinkOnlyProgram`, the 5-shape
`BackendShape` lowering under `lower/{eager_tape,offset_tape,event_tape,collapsed_stage,
sink_only}.rs`) is real and present. **But the contract has three generality gaps that
must be closed before G-Alpha:** (1) it never cites the **canonical Lock-14 three-surface
model** (grammar source + workspace metadata + optional decl crate; `LOCKS.md:349`),
so the distinct-grammar-output gate risks being satisfied by per-grammar BRANCHING in
the generator rather than per-grammar METADATA — the actual Lock-14 failure mode; (2)
the Sheets owner-path is mis-cited as `grammar/sheets/*.bbnf` (does not exist) when the
real file is `grammar/google-sheets/google-sheets.bbnf` and lives in the TOTALITY tree,
not the benched skinny tree, leaving the Sheets-into-skinny sourcing unspecified; (3)
the `distinct-grammar-output` gate as written (md5-distinct `generated.rs`) is
**necessary but not sufficient** for generality — three files can differ byte-wise while
the generator still branches `match grammar`. These are REVISE-level (the goalset is
right; the verification surface is under-specified), not REJECT.

---

## §1 — αA results-extraction (generality lens)

αA inventories the ground truth the goalset binds. Through CH2 it is evaluated only for
generality-relevant claims (the substrate-generalizes / value-API-does-not split, the
distinct-grammar-output census, the Sheets-stub status).

### §1.1 — §0 headline + §4 substrate-validated → **ACCEPT**
`alphaA:22-29,156-162`. The split — substrate (Lock 1) generalizes and is the
foundation; value-API + codegen demonstrably do NOT yet — is the correct generality
framing and is verified (`tape/mod.rs:94,175,38`; one tape, both grammars ride it). The
"generator grammar-driven DOES NOT EXIST" row is the load-bearing CH2 admission. ACCEPT.

### §1.2 — §3.1 the 7-replica distinct-grammar census → **ACCEPT** (with the pin honoured)
`alphaA:128`, `:196-203`. The claim "all 7 `css_l4_*/generated.rs` byte-identical at
`f6a38445b`" with the explicit working-tree caveat (§6: a raw `diff` at HEAD `318d9c046`
now DIFFERS due to uncommitted regen noise; pin all replica claims to the close SHA) is
the correct generality evidence AND the correct honesty about it. This is exactly the
distinct-grammar-output baseline CH2 needs. ACCEPT — the pin is binding on downstream.

### §1.3 — §3.3 phantom-generic vs real-typed-ValueRef distinction → **ACCEPT**
`alphaA:141-144`. αA draws the load-bearing distinction the other artefacts blur: the
typed `ValueRef<'doc,'input,Kind>` IS real for JSON; the **phantom** is the SEPARATE
`<G: EventGrammar>` axis (always `AnyGrammar`, zero non-test consumers). This precision
matters for CH2 because G4 must instantiate-or-delete the RIGHT generic — the
`EventGrammar` `<G>`, not the working `Kind`. ACCEPT.

### §1.4 — §5.3 Sheets close-condition seed → **REVISE**
`alphaA:182-184`. "`sheets_witness` → a real third grammar **via the generator ONLY** —
its `generated.rs` must be non-identical to JSON's and CSS's." Correct as far as it goes,
but αA (and every downstream artefact) is silent on **where the Sheets `.bbnf` comes
from and how it enters the benched skinny tree.** Verified: `grammar/google-sheets/
google-sheets.bbnf` EXISTS but in the **totality** tree; the skinny tree has only
`skinny/grammars/json.bbnf` + `grammar/css/l4/stylesheet.bbnf` (consumed via
`skinny/xtask/src/{main.rs:172,regen_css.rs:16}`). There is NO skinny Sheets source and
NO skinny xtask Sheets target. **Fix:** αA §5.3 must name the actual Sheets source
(`grammar/google-sheets/google-sheets.bbnf`) and flag that bringing it into the benched
tree (a skinny grammar root + an xtask Sheets `RuntimeTarget`) is itself a PROVE-wave
obligation, not a given. REVISE.

**αA tally: ACCEPT ×3, REVISE ×1.**

---

## §2 — αB competitor-deltas (generality lens)

CH2's interest in αB: does the >SOTA-preservation bar generalize across grammars, and is
the "Sheets has no competitor — its bar is GENERATION not throughput" framing correct?

### §2.1 — §4 per-grammar must-hold table + Sheets-as-generation-litmus → **ACCEPT**
`alphaB:239-250`. The three-grammar bar table is the strongest CH2 artefact in αB:
JSON's bar is strict-vs-strict (clean), CSS's is lazy-vs-eager (asymmetric, framed),
**Sheets has NO competitor bar — its bar is GENERATION, not throughput.** This is
exactly the right generality framing: the litmus is "the ONE generator emits a real
Sheets parser from `.bbnf` with a non-identical `generated.rs`," not a fabricated speed
win. ACCEPT — and this is the row CH2 most wants preserved into αF (it is, SYNTHESIS
§0.5 row 3).

### §2.2 — §1.4 typed-rows-conditional caveat (generality of the schema) → **ACCEPT**
`alphaB:121-129`. "The `direct_to_struct`/`real_typed_struct` rows ride a per-corpus
hand-tuned typed schema (`xtask/real_typed_schema.rs`, 1014-line per-corpus capacity
literals) … conditional on a schema that does not generalize." This is a genuine
generality finding: the typed JSON bar is NOT grammar-general and must not be cited as
the preservation bar. Correctly demoted to `parse_only` as the unconditional bar.
ACCEPT.

### §2.3 — §2.2 CSS LOW-lowering-risk-because-scalar claim → **ACCEPT**
`alphaB:169-173`. "The CSS >SOTA does NOT depend on hand-shaping — the hot path is
scalar, there is no fragile kernel to preserve — so the LOWERING rebuild is LOW risk."
This is the load-bearing generality enabler for G2 and is corroborated by the audit
(V3 A2). For CH2 it answers "will the grammar-derived path work for CSS?" — yes, because
there is no per-grammar kernel that the lowering would have to reproduce. ACCEPT.

**αB tally: ACCEPT ×3.**

---

## §3 — αC redress-digest (generality lens)

CH2's interest: do the pre-blocks protect generality (no relocated-overfit seam), and is
the §3 single-distinction correctly generality-framed?

### §3.1 — §2.2b LayoutFacts-derive-not-hardcode admission → **ACCEPT**
`alphaC:238-243`. The admission clause "the emitter DERIVES tape ops from
`LayoutFacts.backend_shape ∈ {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`
via `lower/{tape_plan,offset_tape,event_tape}.rs` … Admission requires the emitter to
*derive* from the rule shape, not hardcode a per-grammar table" is the single most
important CH2 clause in the entire artefact set. Verified: those lowering modules exist;
the 5-shape canon is real (`ir/src/cost.rs:121-131`). This IS the Lock-14 generality
vehicle. ACCEPT.

### §3.2 — §2.2 relocated-overfit-seam pre-block → **ACCEPT**
`alphaC:230,244-247`. "the generalization must NOT introduce a new hand-coded
per-grammar profile/route table (the relocated-overfit seam — Lock 14)" with the
re-open test naming `W5C_REQUEST_FACT_PROFILES`. This is precisely the Lock-14 failure
mode CH2 polices: an un-fork that merely MOVES the `match grammar` into a data table is
not generalization. ACCEPT — strongest pre-block for generality.

### §3.3 — §2.3 retirement-clause (verbatim-blob / single-emitter) → **ACCEPT**
`alphaC:282-287`. The retirement clause correctly binds BOTH `CSS_GENERATED_RS`
(verbatim-blob) AND `RuntimeEmitterKind::RequestFacts` (single-emitter-path) as
generality failures if still present at close. Verified live: `grammar_provider.rs:40-42`
the fork exists; `runtime_generator.rs:25` dispatches `RequestFacts => emit_request_facts`.
ACCEPT.

### §3.4 — §3 corollary "checked TWICE: runtime output AND emitter" → **ACCEPT**
`alphaC:405-411`. "A pre-block re-opens not only by a hand-written runtime construct but
by the GENERATOR *emitting* one … checked against the runtime output AND against the
emitter that produces it." This dual-surface check is exactly right for a generalization
cycle — the generator is the new carrier surface. ACCEPT.

### §3.5 — §1 P3 collapse-vs-differentiate ambiguity → **REVISE**
`alphaC:97-116`. P3's obligation oscillates between "COLLAPSE to ONE CSS grammar" and
"the distinct grammars that genuinely differ are diff-census-distinct." For CH2 this is a
live generality question the digest defers without resolving: the 7 `css_l4_*` targets
(`regen_css.rs:35-150`) all point at `entry_rule:"stylesheet"` + `CSS_L4_ROOTS` (one
`.bbnf`), so they CANNOT be made distinct without distinct `.bbnf` sub-grammars — which
the contract neither confirms exist nor mandates authoring. **Fix:** P3 must state the
default is COLLAPSE-to-one (the 7 are one grammar), and "differentiate" is admissible
ONLY if 7 distinct `.bbnf` roots are authored — otherwise "N distinct generated.rs" is a
hollow target satisfiable by cosmetic divergence. REVISE.

**αC tally: ACCEPT ×4, REVISE ×1.**

---

## §4 — αD validated/invalidated (generality lens)

### §4.1 — §1 V1/DM2 substrate-generalizes-but-value-API-does-not → **ACCEPT**
`alphaD:42`, `:78`. V1 (substrate Lock 1 holds = generalizable foundation) + DM2
("substrate/tape/NEON model generalizes to Sheets/BBNF-self/…" DEMOTED to
"substrate-READY, not proven"; `sheets_witness/` is a 24-line stub) is the exactly-right
generality ledger. The DEMOTION of the generalization CLAIM to "ready, not proven" is the
honest CH2 posture. ACCEPT.

### §4.2 — §2 I3/I4/I5 the three generality invalidations → **ACCEPT**
`alphaD:62-64`. I3 (7 replicas, distinct-grammar-output lens), I4 (`RuntimeEmitterKind`
fork, single-emitter-path lens), I5 (phantom `<G>`, divergent value API,
phantom-generic lens) — each names the CH2 lens it triggers and maps to a named wave.
Verified against §6 (`alphaD:151-154`: replica IDENTICAL, fork `:40`, phantom `:175`).
ACCEPT.

### §4.3 — §4 S12 Sheets-litmus owner-surface → **REVISE**
`alphaD:103`. S12's owner surface is `sheets_witness/` (25-line stub) and the oracle is
"a third grammar's value-API + projection + scanner fall out of the generator with ZERO
hand-written `_GENERATED_RS` block." Correct intent, but — same gap as §1.4 — it names
**no Sheets `.bbnf` source** and does not register that the real source
(`grammar/google-sheets/google-sheets.bbnf`) is a Pratt formula grammar in the totality
tree. For CH2 this matters doubly: (a) the litmus needs a real grammar to be
load-bearing, and one EXISTS, which STRENGTHENS the proof — but the artefact does not
claim it; (b) a Pratt/operator-precedence grammar exercises generality the
`SinkOnlyProgram`/`BackendShape` path may not yet cover (no JSON/CSS rule needs Pratt).
**Fix:** S12 must name `grammar/google-sheets/google-sheets.bbnf` as the source and flag
that its Pratt/error-literal/cell-ref shapes are the generality stress (and an honest-
finding candidate if the generator cannot lower Pratt). REVISE.

### §4.4 — §5 pre-blocked-routes (no-second-substrate for G4) → **ACCEPT**
`alphaD:136-138`. The "no second substrate — `StructLayout`/`TapeStructBuilder`/
`TapeCursor` alongside `Tape`/`ValueRef` is a Lock 1 violation; G4 emits accessors over
the EXISTING tape" clause protects generality (the shared trait must not fork the
substrate). ACCEPT.

**αD tally: ACCEPT ×3, REVISE ×1.**

---

## §5 — αE candidate-shortlist (generality lens — the load-bearing artefact)

This is the artefact CH2 scrutinises hardest: the 5 candidate clusters ARE the
generalization plan, and the falsifiability triple (PRESERVED->SOTA /
GRAMMAR-DERIVATION-PROOF / DISTINCT-GRAMMAR-OUTPUT) is the generality gate.

### §5.1 — §0 the falsifiability triple → **ACCEPT** (with §5.5 sharpening)
`alphaE:14-20`. The triple is the correct generality contract. Gate #2
(GRAMMAR-DERIVATION-PROOF: "mutate the `.bbnf` → the regenerated `generated.rs` changes
correspondingly — a const courier cannot pass this") is an excellent operational
falsifier — a mutation test, not a static grep. ACCEPT the triple; see §5.5 for the
distinct-output sufficiency gap.

### §5.2 — CANDIDATE B1 un-fork + JSON projection → **ACCEPT**
`alphaE:60-76`. Bundling G3 (un-fork) with G1 (project JSON) is correct generality
sequencing: "un-forking is meaningless unless at least one grammar genuinely projects."
The gate #2 falsifier (mutate `json/*.bbnf` → `json_sink_direct.rs` contains 0
`parse_object_direct`/`JsonSink`-named raw-string literal bodies) directly targets the
verified defect: `render` calls 7 `out.push_str` fixed-literal body functions
(`json_sink_direct.rs:96-497`) and consults `program` only in `validate`/`render_header`
(verified `:19,26,30,39,78`). ACCEPT.

### §5.3 — CANDIDATE B2 CSS lowering → **ACCEPT**
`alphaE:80-96`. Entry-gated on B1 (unified emitter exists) + P3 (replicas collapsed),
LOW risk per V3 A2 (scalar hot path). Gate #3 includes the differentiate-fallback
("if P3 chose differentiate, the N CSS profiles must point at distinct `.bbnf` roots —
`color.bbnf`/`media.bbnf`/`selectors.bbnf` exist"). I verified those sub-grammars are
referenced by the totality CSS tree; for the skinny tree only `stylesheet.bbnf` is wired
(`regen_css.rs:16,24`). The candidate correctly defers the which-to-B2. ACCEPT — but the
collapse-vs-differentiate decision must resolve (see §5.6 / αC §3.5).

### §5.4 — CANDIDATE B3 shared trait + phantom → **ACCEPT**
`alphaE:100-116`. Correct generality posture: "the shared trait is an abstraction over
both [JSON tree + CSS flat stream], not a lowest-common-denominator collapse"
(preserve-rich-ast non-negotiable, `alphaE:114`). The instantiate-or-delete falsifier
(`alphaE:110`) is structurally verifiable. The zero-cost requirement (no vtable in the
hot path) is the right generality-vs-performance guard. ACCEPT.

### §5.5 — CANDIDATE B4 Sheets-litmus → **REVISE** (the most load-bearing CH2 disposition)
`alphaE:120-143`. The Sheets-via-generator-only litmus is the heart of the generality
proof and the candidate frames it well: "THREE non-identical `generated.rs` from ONE
generator = generalization is real (not JSON+CSS-overfit)"; "the Sheets grammar must be
a *genuinely different shape* from JSON/CSS, or the litmus is hollow" (`alphaE:142`).
**Two generality gaps require REVISE:**

1. **No Sheets source named; the real one is a Pratt grammar in the wrong tree.**
   `alphaE:127` owner-path cites "a real `grammar/sheets/*.bbnf`" — that path does NOT
   exist (verified `find . -name '*.bbnf' -path '*sheet*'` → only
   `grammar/google-sheets/google-sheets.bbnf`, all under totality / worktrees). The
   benched skinny tree has NO Sheets grammar and NO xtask Sheets target. The candidate
   must (a) name `grammar/google-sheets/google-sheets.bbnf` as the source, (b) add
   "wire a skinny Sheets grammar root + xtask `RuntimeTarget`" to the PROVE owner set,
   (c) acknowledge the real grammar uses **Pratt operator precedence + error literals +
   cell references** — shapes NO JSON/CSS rule exercises, which is the genuine
   generality stress (and good news: it makes the litmus non-hollow by construction).

2. **`distinct-grammar-output` (md5-distinct) is necessary but NOT sufficient for
   generality.** `alphaE:137` ("`diff` → NON-zero") and the SYNTHESIS gate
   (`SYNTHESIS:162,375`) both reduce the proof to three md5-distinct `generated.rs`.
   But three files can differ byte-wise while the generator STILL branches
   `match grammar { Json => …, Css => …, Sheets => … }` internally — which is the actual
   Lock-14 violation (`LOCKS.md:349`: "ZERO `match grammar` arms in generic crates").
   The litmus must ALSO assert the generator body is grammar-neutral: gate #3 needs a
   companion grep (`rg 'Json\s*=>|CssL4\s*=>|Sheets\w*\s*=>' skinny/crates/codegen/src`
   → 0) per the canonical Lock-14 verification command. REVISE B4 to add this.

### §5.6 — CANDIDATE A (PRUNE) generality dependency → **ACCEPT**
`alphaE:32-56`. P4 (Lock-14 gate meaningful) correctly entry-gates B1 ("a green Lock-14
gate must land BEFORE B1, so the un-forked emitter is actually scanned for neutrality as
it is built," `alphaE:163`). This is the right generality sequencing — build the
neutrality gate before the thing it must scan. ACCEPT.

**αE tally: ACCEPT ×5, REVISE ×1.** (Triple, B1, B2, B3, A accept; B4 revise.)

---

## §6 — SYNTHESIS.md (generality lens — the goalset)

The master contract. CH2 evaluates §0.1 close conditions, §0.4 pre-blocks, §0.5 litmus,
Section 2 telemetry — does the goalset respect Lock 14 and bind generality measurably?

### §6.1 — §0.1 close-condition gates G1–G4 + PROVE → **ACCEPT**
`SYNTHESIS:156-162`. Every generalization gate is structurally falsifiable and
generality-correct: G3 single-emitter (`RuntimeEmitterKind` gone), G4 phantom
instantiated-or-deleted + shared trait ≥2 real instantiations, PROVE three md5-distinct
`generated.rs` + Sheets instantiates the G4 trait. The PROVE gate cites "if one
generator emits a third grammar from `.bbnf`, generalization is REAL (not
JSON+CSS-overfit)" — the correct litmus framing. ACCEPT (subject to §6.4 sufficiency
sharpening, which is a REVISE on a different sub-section).

### §6.2 — §0.4 pre-blocks (verbatim-blob / phantom / distinct-output re-entry) → **ACCEPT**
`SYNTHESIS:242-250`, `:211-273`. The three new generality re-entry pre-blocks
(verbatim-blob, phantom-generic, distinct-grammar-output) + the
`W5C_REQUEST_FACT_PROFILES` relocated-seam pre-block (`:229-232`: "Relocating per-rule
branching into projection DATA is the overfit re-entry seam and is forbidden — every
residual CSS routing entry names the `.bbnf` rule it derives from") are exactly the
Lock-14 protections. The "no second substrate" clause for G4 (`:269-273`) protects
generality. ACCEPT.

### §6.3 — §0.5 generalization litmus table → **ACCEPT**
`SYNTHESIS:283-289`. The binary-structural per-axis table (generator exists / fork gone
/ phantom resolved / Sheets real) with honest-finding fallbacks ("if Sheets cannot be
emitted via the generator only: the generalization is NOT real — surface honestly, do
NOT stub-prove," `:287`) is the correct generality close-condition. The no-paper-close /
no-stub-prove discipline is the right CH2 backstop. ACCEPT.

### §6.4 — Lock-14 canonical-model citation gap → **REVISE**
The contract repeatedly invokes "Lock 14 grammar-neutrality" (`SYNTHESIS:129,165`,
HANDOFF `:213-214`) but **never cites the canonical three-surface model** (`LOCKS.md:349`:
"Every grammar plugs in via three declarative surfaces only: (a) `<name>.bbnf`, (b)
workspace metadata declaring its strategy, (c) optionally a per-grammar decl crate …
ZERO `match grammar` arms; ZERO grammar-named modules"). Verified: that text is the
authoritative Lock-14 definition and `LOCKS.md:603` gives the gate's required report
fields + the verification grep. **Consequence for generality:** the goalset's
distinct-grammar-output gate (md5) can pass while the generator body still
grammar-branches — the contract never binds the canonical "ZERO `match grammar` arms in
generic crates" grep as a close condition. **Fix:** §0.1 G3 and the PROVE gate must add
the canonical verification command (`rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|
Sheets\w*\s*=>' skinny/crates/codegen/src` → 0) and §0.5 must state the three-surface
model as the generality target. REVISE.

### §6.5 — Section 2 telemetry generality columns → **REVISE**
`SYNTHESIS:364-402`. The generalization-axis columns are strong (`emitter_fork_present`,
`phantom_generic_resolved`, `generator_grammar_count == 3`, `generated_md5_distinct`).
But two generality columns are MISSING: (1) a **`generator_grammar_branch_count`** column
(must be 0 — the canonical Lock-14 `match grammar` census; today only md5-distinctness is
gate-consumed, the §6.4 gap surfacing in telemetry); (2) a **`sheets_grammar_shape`**
disclosure (Pratt/operator vs flat — so the litmus's "genuinely different shape" claim is
machine-checkable, not asserted). The gate-consumer list (`:388-396`) inherits the same
gap — it consumes `generated_md5_distinct == true` but no neutral-emitter assertion.
**Fix:** add both columns and bind the branch-count to the gate consumer. REVISE.

### §6.6 — §0.3 receiver goalset Sheets sourcing → **REVISE**
`SYNTHESIS:208`. "Author a Sheets `.bbnf`; bring it through the SAME (G3) generator." The
contract says **author** a Sheets `.bbnf` — but `grammar/google-sheets/google-sheets.bbnf`
already EXISTS (in totality). For CH2 generality this is a material under-specification:
the PROVE wave should **adopt the existing Pratt formula grammar** (the genuinely-
different-shape proof) rather than author a fresh stub — authoring a fresh minimal
`.bbnf` risks producing a "third JSON" that hollows the litmus (`alphaE:142` itself warns
of this). **Fix:** §0.3 must point PROVE at the existing
`grammar/google-sheets/google-sheets.bbnf`, note it must be brought into the benched
skinny tree (new skinny grammar root + xtask target), and flag the Pratt-lowering
generality stress. REVISE.

**SYNTHESIS tally: ACCEPT ×3, REVISE ×3.**

---

## §7 — HANDOFF.md (generality lens)

### §7.1 — §What-SK-V18-Opens backlog (G1–G6 + PROVE) → **ACCEPT**
`HANDOFF:66-116`. The 16-item backlog correctly maps each generality item to its V3
finding id and CH2 lens. PROVE (`:106-111`) carries the right litmus ("If one generator
emits a third grammar from `.bbnf`, generalization is REAL"). ACCEPT.

### §7.2 — §Gate-Posture six CHALLENGE addenda → **ACCEPT**
`HANDOFF:148-159`. The six addenda (verbatim-blob, distinct-grammar-output,
single-emitter-path, phantom-generic, timed-plane-symmetry, acceleration-wiring) are the
correct generality CHALLENGE binding and are carried verbatim into every pass. ACCEPT.

### §7.3 — §Inviolable-invariants #5 grammar-neutral → **REVISE**
`HANDOFF:213-214`. Invariant 5 "Grammar-neutral (Lock 14): zero grammar-named branches in
generic crates (codegen/xtask/bbnf-simd) — the gate (P4) must actually scan them." This
is closer to the canonical model than SYNTHESIS — it names "zero grammar-named branches"
— but it scopes the gate to P4's `GENERIC_SCAN_ROOTS` token scan, which (per αC/αD)
checks FORBIDDEN TOKENS, not the `match grammar` arm census. The canonical Lock-14
verification (`LOCKS.md:349,603`) is an AST/grep census of `match … { Json => }` arms,
a different check than a forbidden-token-string scan. **Fix:** invariant 5 must bind BOTH
the forbidden-token scan AND the canonical `match grammar`-arm grep as close conditions
(they catch different leaks: a token scan misses a `match grammar` that uses neutral
identifiers; the arm census misses a `CSS_GENERATED_RS` const). REVISE.

### §7.4 — §Next-Move S-P3 wave sequencing → **ACCEPT**
`HANDOFF:229-254`. PRUNE → GENERALIZE → PROVE → HONESTY with P4-before-G2/G3 (gate
trustworthy before emitter rebuild) and same-wave-consumer-per-primitive is the correct
generality sequencing. ACCEPT.

**HANDOFF tally: ACCEPT ×3, REVISE ×1.**

---

## §8 — Cross-artefact generality findings (consolidated)

**Three REVISE themes recur across artefacts — they are the CH2 fold obligations:**

1. **Sheets sourcing under-specified (αA §5.3, αD S12, αE B4, SYNTHESIS §0.3).** The real
   `grammar/google-sheets/google-sheets.bbnf` exists, is a genuinely-different Pratt
   formula grammar (STRENGTHENS the litmus), but lives in the totality tree with no
   skinny root / xtask target. Every Sheets reference must name the real source, mandate
   bringing it into the benched tree as a PROVE obligation, and flag Pratt-lowering as the
   generality stress + honest-finding candidate. This is the single most load-bearing CH2
   fold: the litmus is only non-hollow if it adopts the existing different-shape grammar.

2. **Distinct-grammar-output (md5) is necessary-not-sufficient (αE §5.5, SYNTHESIS §6.4,
   §6.5).** Three md5-distinct `generated.rs` does not prove a grammar-neutral generator
   body. The canonical Lock-14 `match grammar`-arm census (`LOCKS.md:349`) must be added
   as a co-gate (grep + telemetry column `generator_grammar_branch_count == 0`).

3. **Canonical Lock-14 three-surface model never cited (SYNTHESIS §6.4, HANDOFF §7.3).**
   The contract invokes "Lock 14 grammar-neutrality" without binding `LOCKS.md:349`'s
   "grammar source + workspace metadata + optional decl crate; ZERO `match grammar` arms"
   model or its `:603` gate-report fields. The generality target should be the
   three-surface model explicitly, not just "no fork."

**What is RIGHT and load-bearing (do NOT churn):** the `SinkOnlyProgram` /
`BackendShape` 5-shape lowering vehicle is real and is the correct generalization
mechanism (`ir/src/cost.rs:121-131`, `lower/*.rs` verified); the falsifiability triple is
sound; the relocated-overfit-seam pre-block (`alphaC §2.2`) and verbatim-blob/single-
emitter retirement clauses are precise; the Sheets-as-generation-not-throughput bar
(`alphaB §4`) is correct; the no-paper-close / no-stub-prove honest-finding discipline is
the right backstop. The goalset's spine respects Lock 14 — the REVISEs sharpen the
verification surface, they do not refute the architecture.

**Zero REJECTs.** No section proposes a grammar-specific intervention, re-opens a
generality pre-block, or claims generalization on a hollow proof. The under-specifications
are REVISE-level (fixable in V1→V2 fold), not architectural rejections.

---

## §9 — Disposition ledger

| Artefact | ACCEPT | REVISE | REJECT |
|---|---|---|---|
| αA results-extraction | 3 | 1 | 0 |
| αB competitor-deltas | 3 | 0 | 0 |
| αC redress-digest | 4 | 1 | 0 |
| αD validated-invalidated | 3 | 1 | 0 |
| αE candidate-shortlist | 5 | 1 | 0 |
| SYNTHESIS.md | 3 | 3 | 0 |
| HANDOFF.md | 3 | 1 | 0 |
| **Total** | **24** | **8** | **0** |

Accept rate 24/32 = **75.0%** (below the §3Z ≥95% bar; the 8 REVISEs are the three
cross-artefact generality folds in §8 — Sheets sourcing, distinct-output sufficiency,
canonical Lock-14 citation — none orphaned, each carries a concrete fix). CH2 expects V2
to close all eight by (a) naming `grammar/google-sheets/google-sheets.bbnf` as the Sheets
source + skinny-tree obligation, (b) adding the `match grammar`-arm co-gate +
`generator_grammar_branch_count` column, (c) citing the canonical `LOCKS.md:349`
three-surface model in §0.5 / invariant 5.

TALLY accept=24 revise=8 reject=0
