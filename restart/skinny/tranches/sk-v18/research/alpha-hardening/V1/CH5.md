# CH5 — HIDDEN-COUPLING (V1) — SK-V18 Pass Alpha hardening

Lens: CH5 HIDDEN-COUPLING. Cycle V1. Reviewer pass over the Pass-Alpha SK-V18
artefacts per `PASS-ALPHA §3` + `ORCHESTRATOR §3W`.

**Lens charter (binding for every disposition below):** substrate-union Lock 1
preserved; **no second substrate** introduced through the new generator/value-API
surfaces; the **shared value trait does not silently re-fork** the emitter or the
substrate; the **phantom `<G>` is instantiated-or-deleted**, not animated into a new
coupling. Hidden coupling = a dependency the close conditions do NOT name that, if it
exists, lets a refuted carrier re-land or lets the generalization claim a unification
it did not achieve. I disposition each artefact section ACCEPT / REVISE / REJECT with
`path:line` + concrete fix.

## Subjects reviewed

- `research/alpha/alphaA-results-extraction.md` (αA)
- `research/alpha/alphaB-competitor-deltas.md` (αB)
- `research/alpha/alphaC-redress-digest.md` (αC)
- `research/alpha/alphaD-validated-invalidated.md` (αD)
- `research/alpha/alphaE-candidate-shortlist.md` (αE)
- `SYNTHESIS.md` + `HANDOFF.md` (these ARE the α-F deliverable per
  `restart/prompts/pass-contracts/PASS-ALPHA.md:27` — there is NO separate `alphaF.md`,
  and that is contract-correct, not a missing artefact).

## Ground-truth verification performed (this review, at working HEAD)

CH5 re-grepped the load-bearing coupling surfaces rather than trusting the artefacts'
own citations:

- `skinny/crates/runtime/src/tape/mod.rs:38` `PayloadArena`, `:94` `Tape<'input>`,
  `:175` `pub struct ValueRef<'doc,'input:'doc, K = AnyKind, G: EventGrammar = AnyGrammar>`
  — substrate types confirmed; **note the real signature carries TWO defaulted params
  (`K=AnyKind` AND `G=AnyGrammar`)**, a nuance several artefacts elide (see αA REVISE).
- **No second substrate:** `grep -rln 'StructLayout|TapeStructBuilder|TapeCursor'
  crates/runtime/src crates/codegen/src` → **EMPTY**. The Lock-1 type-ambivalence
  carriers the artefacts pre-block are genuinely absent at HEAD. This is the
  load-bearing CH5 fact: the digest's Lock-1 claim is real, not asserted.
- **Phantom `<G>` confirmed:** the only non-`AnyGrammar` `ValueRef` instantiation is
  `tape/event_grammar_tests.rs:89` (`ValueRef<'static,'static,AnyKind,JsonEventGrammar>`)
  + the `_proof_compiles::<JsonEventGrammar|SheetsEventGrammar|AnyGrammar>` const-fns at
  `event_grammar_tests.rs:18-23` — ALL test-file. Production rides `AnyGrammar`. Witnesses
  inert at `grammars/{json,sheets_witness}/event_grammar_witness.rs`.
- **Divergent value API confirmed:** `DocumentView` is implemented ONLY in
  `grammars/json/view.rs` (+ trait def `tape/mod.rs`); NO `css_l4_*` implements it.
  CSS `grammars/css_l4_declaration_values/generated.rs:15` literally comments *"the CSS
  analogue of `JsonNodeKind::at_cursor`"*, `:25` re-declares its own `at_cursor`, `:46`
  `node: ValueRef<'doc,'input>` (untyped default). `grep -rn 'trait Value|trait Document
  |trait Cursor' crates/runtime/src` → **EMPTY** — the `at_cursor` pattern is
  hand-copied with NO shared trait. The G4 thesis is ground-truth-anchored.
- **Fork confirmed:** `grammar_provider.rs:40-42` `enum RuntimeEmitterKind
  {CompiledLowering, RequestFacts}`, branched `:110`. `runtime_generator.rs:195`
  `JSON_PARSE_ONLY_GENERATED_RS`, `:701` `CSS_GENERATED_RS` const-`&str`.

Every CH5-relevant citation in the artefacts resolves as stated. The dispositions below
are therefore about coupling-surface COMPLETENESS (does a close condition leave a
hidden seam unnamed), not citation accuracy.

---

## §A — αA results-extraction.md

### A.1 — §3.3 phantom-generic + divergent value API table — **REVISE**

The substance is correct and ground-truth-verified (above). But the table at αA:143-144
states `ValueRef<G>` and elsewhere (`:144`) writes the typed form as
`ValueRef<'doc,'input,Kind>` — a **three-slot** signature. The real type is
**four-slot**: `ValueRef<'doc,'input, K=AnyKind, G: EventGrammar=AnyGrammar>`
(`tape/mod.rs:175`, verified). This is a CH5-load-bearing distinction, not pedantry:
**there are TWO generic axes and they are NOT the same phantom.** αA:144 half-says this
("the typed `ValueRef<'doc,'input,Kind>` IS real for JSON … the **phantom** is the
separate `<G: EventGrammar>` axis"), which is the correct decomposition — but the §3.3
header and the αD/SYNTHESIS prose collapse them into a single "`ValueRef<G>`" phantom.
The hidden-coupling risk: **G4 could "instantiate" the wrong axis** — bind `K` to a real
`Kind` (already done for JSON) and declare the phantom resolved while `G` stays
`AnyGrammar`, OR delete `G` but leave the impl coupling unchanged. The close condition
must name WHICH axis.

**Fix:** in §3.3, state explicitly: "`ValueRef` has two defaulted axes — `K` (Kind,
REAL for JSON: NumberKind/StringKind/…; untyped-default for CSS) and `G`
(EventGrammar, PHANTOM: only `AnyGrammar` in production). G4's instantiate-or-delete
targets the **`G` axis specifically**; resolving `K` does not discharge it." Carry the
same two-axis precision into the αF/SYNTHESIS G4 close condition (see §F.2).

### A.2 — §4 substrate "no second substrate" pre-block — **ACCEPT**

αA:166-168 names the second-substrate Lock-1 violation precisely
(`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside `Tape`/`ValueRef`) and binds
"the projection generator emits accessors over the EXISTING types." Verified absent at
HEAD. This is the correct CH5 framing of the foundation. No revision.

### A.3 — §3.1/§3.2/§6 fork + replica + working-tree caveat — **ACCEPT**

The fork (`grammar_provider.rs:40`), the const-`&str` blobs (`:195`/`:701`), and the §6
working-tree-vs-`f6a38445b` md5 caveat are all CH5-clean: §6 correctly pins the
replica-identity claim to the close SHA and flags that the working-tree `diff` now
DIFFERS (regen noise). This pre-empts a CH5 false-coupling (a reviewer mis-reading the
working-tree diff as refutation of P3). Good hygiene. No revision.

---

## §B — αB competitor-deltas.md

### B.1 — §0/§3.2 JSON-vs-CSS comparator-plane asymmetry — **ACCEPT**

CH5's concern in αB is whether the two >SOTA bars hide a coupling that lets one
grammar's framing contaminate the other's. αB:43-53 and §3.1/§3.2 keep the planes
**explicitly decoupled**: JSON = near-symmetric strict-vs-strict (clean), CSS =
asymmetric lazy-vs-eager (disclosed, H1-bound). The asymmetry is stated up front and
never papered. No hidden cross-plane coupling. ACCEPT.

### B.2 — §1.4 / DM1 typed-row conditionality — **ACCEPT**

αB:121-129 explicitly quarantines the typed `direct_to_struct`/`real_typed_struct` rows
as "conditional on a per-corpus hand-tuned schema that does not generalize" and forbids
citing them as the preservation bar. This is the right CH5 move: it prevents a hidden
coupling between the unconditional `parse_only` bar and the schema-tuned typed bar (a
coupling that would let G1 "preserve >SOTA" by leaning on a non-generalizable schema).
ACCEPT.

### B.3 — §4 Sheets "no competitor bar" — **ACCEPT**

αB:243-250 correctly states Sheets has NO speed comparator and its bar is GENERATION,
not throughput — closing a latent CH5 hole (a fabricated Sheets speed number coupled to
a non-existent comparator). ACCEPT.

---

## §C — αC redress-digest.md

### C.1 — §2.2 StructRegistry / Arena<G> / Builder<G> SPLIT classification — **ACCEPT**

This is the strongest CH5 section in the cohort. αC:215-256 splits the pre-block into
§2.2a (PERMANENT: per-leaf registry/hashmap deref, per-compound `Vec`, frame-clone
checkpoint) and §2.2b (ADMIT: the *layout description* built once-per-rule is the
generality vehicle). Critically it pins **Lock 2: canonical name is `Layout`/
`LayoutFacts`, NOT `StructLayout` (RETIRED, `LOCKS.md:160`)** (αC:241-243) — this
directly forecloses the hidden coupling where G3/G4 re-introduce a `StructLayout`-named
second substrate under the guise of "the layout the generator consumes." Verified: no
`StructLayout` at HEAD. The §2.2 re-open tests are keyed to the NEW surfaces (G3
emitter, G4 trait) — exactly the CH5 mandate. ACCEPT.

### C.2 — §2.3 fact-stream PERMANENT + residual-fork retirement clause — **ACCEPT**

αC:258-298 correctly narrows the fact-stream pre-block to its residual surfaces
(`emit_fact_stream` already gone, verified count=0 per αC:33-39; `CSS_GENERATED_RS` +
`RequestFacts` remain) and adds a **retirement clause** (αC:282-287): closing with
`CSS_GENERATED_RS` still a const-`&str` OR `RequestFacts` still the fork is a CH2/CH5
fail. This binds the single-emitter-path addendum to a concrete close gate. The Track1
≡ Track2 / sidecar dishonesty cross-reference (αC:281) is the right CH5 coupling guard.
ACCEPT.

### C.3 — §3 corollary "checked TWICE (runtime AND emitter)" — **ACCEPT**

αC:405-411 is the CH5 keystone: every re-open test is checked against BOTH the runtime
output AND the emitter that produces it, because "the generator is the new carrier
surface." This closes the deepest hidden-coupling vector in a generalization cycle — a
pre-block re-opening not by hand-written runtime code but by the GENERATOR *emitting*
it. ACCEPT, and I lift this corollary into the CH5 CONSOLIDATED recommendation.

### C.4 — §1 P4 gate-scope + the un-named `event_grammar`/witness scan surface — **REVISE**

αC §1 P4 (lines 118-141) extends `GENERIC_SCAN_ROOTS` to cover `runtime_generator.rs` +
templates + the post-G3 unified emitter, and the §4 source ledger (αC:439-441) cites the
phantom `<G>` and witness surfaces. But the P4 obligation does **not** name a coupling
the CH5 lens flags: **the witness files (`grammars/{json,sheets_witness}/
event_grammar_witness.rs`) and the `EventGrammar` trait carry grammar-NAMED types
(`JsonEventGrammar`, `SheetsEventGrammar`) that live in the RUNTIME crate, not the
generic crate** — so Lock-14's generic-crate scan will never see them, yet they are the
exact surface where G4 either instantiates `<G>` (creating a real `runtime/`-side
grammar-name coupling) or deletes it. If G4 *instantiates*, a new
`ValueRef<…,JsonEventGrammar>` appears in production `runtime/` code — which is fine
(runtime is grammar-specific by construction) — BUT if the un-forked generator (G3) ever
EMITS a `ValueRef<…,XEventGrammar>` line, that is a grammar-name leak the P4 gate (scoped
to generic crates) cannot catch. The close condition leaves this seam unnamed.

**Fix:** add to P4 (or to the G4 close condition) a clause: "if G4 instantiates the `<G>`
axis, the generic emitter (`runtime_generator.rs`/post-G3 unified) must NOT emit a
grammar-named `EventGrammar` type literal — the witness type is supplied by the
runtime-side hand-written grammar module and injected by NAME-PARAMETER, never templated
as a string literal in codegen. Add `XEventGrammar`/`EventGrammar` to
`FORBIDDEN_GENERIC_TOKENS` scanned over `runtime_generator.rs`." Without this, G4's
"instantiate" branch can re-couple the generic emitter to grammar names under a green
gate. (This is the αC §3 "checked twice" corollary applied to the witness surface, which
§1 P4 currently omits.)

---

## §D — αD validated-invalidated.md

### D.1 — V1 substrate-union / I5 phantom + divergent API / §5 no-second-substrate — **ACCEPT**

αD V1 (`:42`) names the Lock-1 carry-forward with the verified citations
(`tape/mod.rs:38,94,175`); I5 (`:64`) and §5 (`:136-138`) state "S9/G4 emits accessors
over the EXISTING tape; no new cursor/builder type" and "an introduced
`StructLayout`/`TapeStructBuilder`/`TapeCursor` … is a Lock 1 type-ambivalence
violation." The §6 verification log (αD:152) independently re-greps the phantom `<G>`
and resolves it as test-only. CH5-clean. ACCEPT.

### D.2 — I5 "phantom `ValueRef<G>`" single-axis phrasing — **REVISE**

Same defect as §A.1 propagated. αD I5 (`:64`) writes "`ValueRef<G:EventGrammar>` … `G`
… defaulted to `AnyGrammar` everywhere; never instantiated" — correct for the `G` axis —
but the disposition cell ("INSTANTIATE-OR-DELETE the phantom `<G>`") does not flag that
the SIBLING `K` axis is already real and must NOT be conflated. The §6 log row
(αD:152) greps `ValueRef<…EventGrammar…>` which correctly isolates `G`, so the
*verification* is right; the *prose* is loose. Same fix as §A.1: name the two axes; G4
targets `G`.

### D.3 — S9 owner-surface `tape/mod.rs:227 DocumentView` — **REVISE (citation)**

αD S9 (`:100`) cites `tape/mod.rs:175,227 (DocumentView)`. I verified `DocumentView`'s
production *impl* lives in `grammars/json/view.rs` (the trait/related def is in
`tape/mod.rs`). The `:227` line-number for `DocumentView` is not independently confirmed
in this review (mod.rs content shifts with working-tree regen). This is a soft citation,
not a substance error — the divergent-API claim itself is ground-truth-verified (only
JSON impls `DocumentView`). **Fix:** re-pin the `DocumentView` owner citation to
`grammars/json/view.rs` (the impl site, the thing G4 generalizes) and mark the
`tape/mod.rs` line as "trait/assoc def, line at close SHA" rather than a hard `:227`.

### D.4 — DM2 Sheets substrate-READY-not-proven — **ACCEPT**

αD DM2 (`:78`) correctly demotes "substrate generalizes to Sheets" to "READY, not
proven" and routes the proof through the generator ONLY. This forecloses the hidden
coupling where a hand-written Sheets witness is mistaken for generalization evidence.
ACCEPT.

---

## §E — αE candidate-shortlist.md

### E.1 — CANDIDATE B3 (G4 shared trait + phantom) — **REVISE**

αE B3 (`:100-116`) is the CH5-central candidate and is largely strong: it states
"INSTANTIATE-OR-DELETE the phantom `ValueRef<G: EventGrammar=AnyGrammar>`", names
`abrogate-before-patch`, requires the trait be zero-cost (no vtable in the hot path,
`:115`), and binds preserve-rich-ast (no JSON flattening). The phantom-generic gate
(`:110`) is structurally verifiable. Two CH5 gaps:

1. **Two-axis precision missing (same as §A.1/§D.2).** B3:104,108,110 say
   "`ValueRef<G>`" / bind `G` to `JsonEventGrammar`/`CssEventGrammar`/`SheetsEventGrammar`
   — but `CssEventGrammar` **does not exist** at HEAD (only `JsonEventGrammar` +
   `SheetsEventGrammar` witnesses exist; verified). So the "instantiate" branch as
   written REQUIRES authoring a new `CssEventGrammar` witness — which is itself a new
   grammar-named coupling surface, un-budgeted in B3's "≈ ±0 LOC". The gate `:110`
   `grep 'ValueRef<.*,(Json|Css|Sheets)EventGrammar>'` would pass on the EXISTING
   test-only `JsonEventGrammar` line (`event_grammar_tests.rs:89`) — a **false-green
   coupling**: the gate cannot distinguish a production instantiation from the standing
   test instantiation.

   **Fix:** B3's phantom gate must exclude `#[cfg(test)]`/test-file matches explicitly
   (the SYNTHESIS §2 telemetry says "test-only `_proof_compiles` does NOT count" — good,
   but the αE grep at `:110` does not encode the test-exclusion). Make it
   `grep -rn 'ValueRef<.*EventGrammar>' --include='*.rs' crates/runtime/src | grep -v
   'tests\.rs\|#\[cfg(test)\]'` → ≥1 for "instantiated". AND: if the DELETE branch is
   chosen (the simpler, abrogate-before-patch-preferred outcome), B3 must say so as the
   DEFAULT and treat instantiate (requiring a new `CssEventGrammar`) as the burden-of-
   proof branch — currently B3 presents them as symmetric.

2. **The shared-trait gate `:111` can false-green on the divergent surface.** B3:111
   `grep -l 'impl.*Document.*for' …/{json,css_l4_*}` returns BOTH. But CH5's worry is
   the trait being a **lowest-common-denominator collapse** that flattens JSON's richer
   surface (B3 itself flags this at `:114-115`). The grep proves *a* `Document` impl
   exists on both, NOT that JSON's visitor/`get(key)`/typed-`Kind` navigation survived.
   The gate is satisfiable by a trait so thin it discards JSON's richness — the exact
   preserve-rich-ast violation B3 warns of, un-caught by its own gate.

   **Fix:** add a preserve-rich-ast structural gate to B3: "JSON's `get(key)` + typed
   `Kind` accessors + visitor remain reachable THROUGH the shared trait (grep the JSON
   view-time tests still compile against the trait, not a bypass)." A trait both-impl is
   necessary but not sufficient; the richness-preservation must be a separate checked
   condition.

### E.2 — CANDIDATE A / B1 / B2 / B4 sequencing + no-second-substrate pre-blocks — **ACCEPT**

αE A (`:34-56`), B1 (`:60-77`), B2 (`:80-97`), B4 (`:120-143`) each carry the
no-second-substrate pre-block explicitly (B2:96 "no second CSS tape … must keep riding
the existing sparse `flag_cursors`/`flag_values` pair, `tape/mod.rs:96-98`"; B4:143
"Lock 1 (Sheets rides the existing tape, no third substrate)"). The PRUNE-before-
GENERALIZE entry-gating (`:22`, cross-cutting `:163`) is the right ordering to prevent a
blind-gate re-leak. Cross-cutting note 6 (`:168`) verifies no candidate re-opens a
pre-blocked route. CH5-clean on the substrate axis. ACCEPT.

### E.3 — B4 G6 acceleration-wiring same-wave-consumer — **ACCEPT**

αE B4 (`:132-139`) binds every NEON/ASM kernel to a same-wave hot-path consumer and the
acceleration-wiring-at-admission gate (`:139`), correctly closing the orphan-kernel
hidden coupling (a kernel "wired" only under `#[cfg(test)]`). ACCEPT.

---

## §F — SYNTHESIS.md + HANDOFF.md (the α-F deliverable)

### F.1 — §0.4 pre-blocks + "no second substrate" + hidden-coupling escapes — **ACCEPT**

SYNTHESIS §0.4 (`:211-273`) is the CH5 anchor of the contract. It carries the
no-second-substrate clause verbatim (`:269-273` "an introduced `StructLayout`/
`TapeStructBuilder`/`TapeCursor` alongside the landed `Tape`/`ValueRef` is a Lock 1
type-ambivalence violation (REJECT)"), and §0.4's "Hidden-coupling escapes are
pre-blocked" paragraph (`:263-273`) enumerates the full carrier set (retained sidecars,
sidecar tables, second tapes, public `UnionTape`, new substrate APIs, a sixth
`BackendShape`, Track 1 ≡ Track 2 sidecars, cross-call classifier-state retention). This
is the most complete hidden-coupling pre-block list in the cohort and is ground-truth
consistent (no second substrate at HEAD). HANDOFF `:199-205` mirrors it. ACCEPT.

### F.2 — G4 close condition: phantom single-axis + false-green instantiation — **REVISE**

SYNTHESIS §0.1 G4 row (`:159`) and §0.3 (`:205`) state "INSTANTIATE the phantom
`ValueRef<G>` with a real production grammar type OR DELETE it" and the gate "a shared
trait has ≥2 real (JSON, CSS) production instantiations with non-test call sites;
`EventGrammar`'s phantom `<G>` is either reached in production or removed (no test-only
`_proof_compiles`)." This correctly excludes the test instantiation in PROSE — better
than αE's grep. Two residual CH5 gaps:

1. **Single-axis phrasing** (propagated from αA/αD). The contract says "`ValueRef<G>`"
   throughout and never states that `K` (Kind) is the REAL axis and `G` is the phantom.
   A receiver could "resolve the phantom" by deepening `K` while `G` stays `AnyGrammar`,
   and the §2 telemetry `phantom_generic_resolved ∈ {instantiated,deleted}` would not
   catch it (it does not name the axis). **Fix:** the G4 close condition + the
   `phantom_generic_resolved` column definition must read "the **`G: EventGrammar`
   axis** of `ValueRef` (NOT the already-real `K=Kind` axis) is instantiated with a
   production grammar witness OR the `G` parameter is removed from the struct."

2. **`CssEventGrammar` non-existence un-budgeted** (propagated from αE B3). The contract
   says "≥2 real (JSON, CSS) production instantiations" — but no `CssEventGrammar`
   witness exists at HEAD, so "instantiate" entails authoring one (a new grammar-named
   type in `runtime/`). The contract should state the DELETE branch is the
   abrogate-before-patch DEFAULT (SYNTHESIS §0.5 fallback `:288` already leans this way:
   "if no shared trait is dischargeable … REJECT the trait shape"), and that the shared
   `Value`/`Document`/`Cursor` trait does NOT itself require the `G` axis (the trait can
   be parameterized over the runtime grammar module without the `EventGrammar` phantom).
   **Fix:** add to G4: "The shared trait's existence is INDEPENDENT of the `<G>` phantom
   — deleting `<G>` and defining the trait are separable; do not couple the trait's
   shape to animating `<G>` (that would manufacture the very phantom we are deleting)."

### F.3 — §2 telemetry `shared_value_trait_instantiations >= 2` false-green — **REVISE**

SYNTHESIS §2 (`:373`, gate consumer `:390`) requires
`shared_value_trait_instantiations >= 2` (json + css). Same defect as §E.1.2: the count
≥2 is satisfiable by a thin LCD trait that discards JSON's richness. The §0.1 G4 row and
§0.5 carry preserve-rich-ast in PROSE (`:165`, `:288`), but **no telemetry column
encodes the richness-preservation** — so the machine-checkable gate (`gate-json
--skv18-generalization-report`) can pass while JSON's visitor/`get(key)`/typed-`Kind`
navigation is flattened. That is a hidden coupling between the "shared trait" claim and a
silent preserve-rich-ast regression. **Fix:** add a telemetry column
`json_rich_navigation_preserved` (boolean: JSON `get(key)` + typed-`Kind` + visitor
reachable through the shared trait) to §2, and add it to the gate consumer's REJECT set
alongside `shared_value_trait_instantiations`. A count without a richness assertion is
not a sufficient G4 gate.

### F.4 — G3 un-fork close condition + the relocated-overfit seam — **ACCEPT**

SYNTHESIS §0.1 G3 (`:158`) retires `RuntimeEmitterKind` and the single-emitter-path
gate greps the fork to 0. §0.4 (`:229-232`) binds the `W5C_REQUEST_FACT_PROFILES`
relocated-overfit seam ("Relocating per-rule branching into projection DATA is the
overfit re-entry seam and is forbidden — every residual CSS routing entry names the
`.bbnf` rule it derives from"). This is the correct CH5 guard against the un-fork
re-coupling grammar-family logic into a data table. ACCEPT.

### F.5 — §0.6 / §2 `corpus_in_timer` + Track1≡Track2 sidecar guard — **ACCEPT**

SYNTHESIS §0.6 (`:299-313`) keeps the strict comparator gate and the H1 materialization
disclosure; §2 (`:382`) the `corpus_in_timer` column. The Track1≡Track2 sidecar
dishonesty is pre-blocked in §0.4 (`:267`). No hidden timed-plane coupling survives the
contract. ACCEPT.

### F.6 — HANDOFF "Next Move" + S-P3 gate consumer — **ACCEPT**

HANDOFF `:219-254` routes the six-lens CHALLENGE, the S-P0..S-P3 sequence, and binds the
full `--skv18-generalization-report` consumer. The substrate/phantom/fork columns are
all carried. ACCEPT (subject to F.2/F.3 column amendments folding in).

---

## CH5 cross-cutting findings (for CONSOLIDATED)

1. **Single-axis `ValueRef<G>` phrasing is a systemic loose-end** (αA §3.3, αD I5, αE
   B3, SYNTHESIS G4). The type has TWO defaulted axes — `K=Kind` (REAL) and
   `G=EventGrammar` (PHANTOM). Every artefact that writes "`ValueRef<G>`" without naming
   the `K` sibling risks a receiver "resolving the phantom" on the wrong axis. **The
   fix is one sentence repeated in each:** "G4 targets the `G` axis; `K` is already
   real." This is the dominant REVISE driver and is cheap to discharge.

2. **`CssEventGrammar` does not exist** — the "instantiate ≥2 (json, css)" branch is
   under-budgeted and, as written, manufactures a new grammar-named coupling. The
   abrogate-before-patch DEFAULT should be DELETE the `G` phantom; the shared trait does
   not require it. (αE B3, SYNTHESIS G4.)

3. **The shared-trait count gates can false-green by LCD-flattening** — both αE B3:111
   and SYNTHESIS §2 require ≥2 impls, but no gate asserts JSON's richness survives. A
   `json_rich_navigation_preserved` column + a non-test-excluding phantom grep close
   this. (αE B3, SYNTHESIS §2.)

4. **The witness/`EventGrammar` surface is a Lock-14-invisible coupling** — grammar-named
   `JsonEventGrammar`/`SheetsEventGrammar` live in `runtime/` (not the generic crate the
   P4 gate scans). If the un-forked generator EMITS a grammar-named `EventGrammar`
   literal, P4 cannot catch it. Add `EventGrammar`/`XEventGrammar` to the
   `runtime_generator.rs`-scoped forbidden tokens. (αC §1 P4, applying αC §3's
   "checked-twice" corollary the section itself omits.)

5. **The genuinely-strong CH5 anchors** (ACCEPT, lift forward): αC §2.2 Layout-vs-
   StructLayout Lock-2 pin; αC §3 "checked TWICE (runtime AND emitter)"; SYNTHESIS §0.4
   hidden-coupling-escape enumeration; the verified absence of any second substrate at
   HEAD. The substrate-union foundation is real and the contract guards it correctly.

## Disposition summary

| Section | Disposition |
|---|---|
| αA §3.3 phantom two-axis | REVISE |
| αA §4 no-second-substrate | ACCEPT |
| αA §3.1/3.2/6 fork+replica+caveat | ACCEPT |
| αB §0/§3 plane-asymmetry | ACCEPT |
| αB §1.4/DM1 typed conditionality | ACCEPT |
| αB §4 Sheets no-competitor | ACCEPT |
| αC §2.2 StructRegistry SPLIT + Lock-2 pin | ACCEPT |
| αC §2.3 fact-stream + retirement clause | ACCEPT |
| αC §3 checked-twice corollary | ACCEPT |
| αC §1 P4 witness/EventGrammar scan gap | REVISE |
| αD V1/I5/§5 substrate+phantom | ACCEPT |
| αD I5 phantom single-axis | REVISE |
| αD S9 DocumentView citation | REVISE |
| αD DM2 Sheets ready-not-proven | ACCEPT |
| αE B3 G4 phantom+trait false-green | REVISE |
| αE A/B1/B2/B4 sequencing+pre-blocks | ACCEPT |
| αE B4 G6 acceleration same-wave | ACCEPT |
| SYNTHESIS §0.4 pre-blocks+escapes | ACCEPT |
| SYNTHESIS G4 phantom two-axis+CssEventGrammar | REVISE |
| SYNTHESIS §2 trait-count false-green | REVISE |
| SYNTHESIS G3 un-fork+relocated-seam | ACCEPT |
| SYNTHESIS §0.6/§2 timed-plane | ACCEPT |
| HANDOFF next-move+gate consumer | ACCEPT |

23 sections: 16 ACCEPT, 7 REVISE, 0 REJECT.

**CH5 verdict:** the substrate-union Lock-1 foundation is real and correctly guarded —
no second substrate at HEAD, the pre-block lists are complete and ground-truth
consistent, the phantom `<G>` and divergent value API are accurately diagnosed. No
REJECT: nothing in the cohort re-opens a hidden-coupling carrier or hides a second
substrate. The 7 REVISEs are convergence-cheap and cluster on TWO root causes: (a) the
`ValueRef` two-axis (`K` real / `G` phantom) precision must be named so G4 cannot resolve
the wrong axis or false-green on the standing test instantiation, and (b) the shared-trait
gates must assert JSON-richness-preservation, not merely an impl-count, or the "shared
value trait" claim couples silently to a preserve-rich-ast regression. All seven carry a
concrete `path:line` fix above; none is an orphan REVISE.
