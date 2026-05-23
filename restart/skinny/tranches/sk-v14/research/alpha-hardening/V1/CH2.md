# CH2 GENERALITY (Lock 14) — Pass Alpha V1 Disposition

Lens: CH2 per `restart/prompts/ORCHESTRATOR.md §3W` — "Lock 14 holds: no
grammar-name leak; every proposed intervention is grammar-neutral and
works for CSS L4 / Sheets / BBNF-self, not only JSON."

Authority binding: `restart/locks/LOCKS.md:220-238` (Lock 14 text);
dispatch context `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:99-110` (§CH-2 scope).

## §0 — Disposition summary

- ACCEPT-rate: 88% (29 ACCEPT / 33 § lines disposed)
- REJECT count: 0
- REVISE count: 4 (C-1 trait-name ambiguity; C-3 generality posture;
  C-4 grammar-shape neutrality; α-E §6 ledger telemetry-column scoping)
- Critical findings: 0 architectural defects; 4 cosmetic / explicitness
  REVISEs that strengthen Lock 14 binding without unwinding any
  candidate
- Escalation flag: none. The α-F SYNTHESIS + HANDOFF and the α-E
  shortlist hold Lock 14 cleanly; the falsifiability gates are
  grammar-neutral by construction; the SIMD G-SIMD-GRAMMAR-POLICY
  clause closes the substrate-union hole. CH2 converges on this
  cycle.

The lens finds that every candidate, every same-wave consumer, every
S-P3 constraint, and the SYNTHESIS §0.4 / §4 pre-block + constraint
layer treat grammar identity as a metadata input to one generator,
not as a discriminator inside generic crates. The REVISE list below
asks for tighter explicitness on a handful of clauses; none of the
findings rises to REJECT.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `SYNTHESIS.md` | §0.1 close condition | ACCEPT | R10 verbatim; close criterion treats JSON cells + CSS features symmetrically — same equality semantics, same plane discipline. |
| `SYNTHESIS.md` | §0.2 goalset enumeration | ACCEPT | Per-grammar row counts named, not encoded as code; all four surfaces enumerated under one rubric (`Track 1 > comparator strict + 1`). |
| `SYNTHESIS.md` | §0.3 R-target goalset | ACCEPT | R3 PRUNE-3/4 names "trait-dispatch + grammar-agnostic codegen template" verbatim (`SYNTHESIS.md:82`); R4 names "consumes 15 `.bbnf` files" — the grammar is metadata input, not branch arm. |
| `SYNTHESIS.md` | §0.4 P-1…P-7 pre-blocks | ACCEPT | P-6 names per-grammar provider modules as the recurrence vector (`SYNTHESIS.md:121-126`); pre-block scope is the generator family, not a per-grammar veto. |
| `SYNTHESIS.md` | §0.5 wave-by-wave deferral | ACCEPT | The S-P3 deferral is contracted per `PASS-ALPHA.md §4.4`; the deferred work is "owner-paths, entry/exit gates, hard caps" — these are themselves grammar-neutral surfaces. |
| `SYNTHESIS.md` | §1 corrected diagnosis | ACCEPT | Pillars marked `grammar-neutral` (`SYNTHESIS.md:164-165`) for bbnf-simd / OffsetFlags + Tape; the 8 surviving pillars span JSON + the 15 CSS `.bbnf` grammars symmetrically. |
| `SYNTHESIS.md` | §2 telemetry binding | ACCEPT | Column rule wording (`SYNTHESIS.md:204-227`) treats every plane and every grammar uniformly; `comparator_plane` is plane-keyed not grammar-keyed; `Hot leaf` and `audit_overlay_verdict` apply to JSON + CSS rows identically. |
| `SYNTHESIS.md` | §3 candidate shortlist C-1 | ACCEPT | Falsifiability gate (`SYNTHESIS.md:241`) tests Json-named symbols (`RuntimeProvider::Json`, `JsonGrammar`, `parse_json_grammar`) — this is the *recurrence vector* in current source, not a JSON-only check; the directive `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO catches ALL eight grammars. The combined gate is grammar-neutral. |
| `SYNTHESIS.md` | §3 candidate C-2 | ACCEPT | Per-plane comparator rebind has no grammar branch; sonic-rs/sonic-rs strict struct deser/typed deser bind by plane × corpus, not by grammar family. |
| `SYNTHESIS.md` | §3 candidate C-3 | REVISE | The candidate row names `cargo xtask regen-css` — see §2 critical finding #1; the xtask should be presented as the CSS instance of a `regen-{grammar}` family, or the row text should disclose that future grammars carry their own `regen-{name}` xtask following the same shape. |
| `SYNTHESIS.md` | §3 candidate C-4 | ACCEPT | "CSP-chosen shape produces measurable runtime divergence on a named pre-wave row" — the CSP solver itself is grammar-neutral per `v4-decision-engine-trace §1`; the row attribution change is the measurement axis, not a per-grammar branch. |
| `SYNTHESIS.md` | §3 candidate C-5 | ACCEPT | Revert + REDRESS; the deletions span both JSON parse_only rows and 24 CSS rows, applied uniformly under the audit-falsified rubric. |
| `SYNTHESIS.md` | §4 S-P3 constraints (Lock 14 line) | ACCEPT | Line 272-275 forbids "grammar-specific generic behaviour (Lock 14 binding)" verbatim; line 276-281 binds `G-SIMD-GRAMMAR-POLICY` to any SIMD wave wiring CSS / union / parse_only / shared generated code — this closes the substrate-union substrate hole. |
| `SYNTHESIS.md` | §4 S-P3 constraints (other lines) | ACCEPT | No SPEC clause may inherit weaker scoping for pinned R1–R10; same-wave consumer requirement applies uniformly to every primitive regardless of grammar; CSP fail-closed clause is grammar-neutral. |
| `SYNTHESIS.md` | §5 pre-blocked routes | ACCEPT | "grammar-name branches in generic crates" enumerated as pre-blocked (`SYNTHESIS.md:305-306`); P-1…P-7 carried verbatim; `bbnf-simd` consumer rule (line 302-304) explicitly forbids non-JSON consumers from inheriting JSON quote/escape/control constants. |
| `SYNTHESIS.md` | §6 close posture | ACCEPT | The bracket framing is symmetric — full ADMIT across 51 JSON cells + 24 CSS features without prioritising JSON over CSS. |
| `HANDOFF.md` | §1 bracket verdict | ACCEPT | Pillar list mirrors SYNTHESIS §1.1; "15 unwired CSS `.bbnf` grammars" presented as the same-class artefact as the JSON generated parsers. |
| `HANDOFF.md` | §3 honest baseline | ACCEPT | 0/17 + 0/17 + 0/17 + 0/24 — JSON-CSS parity in admit-count framing; the Lock-14 violation tally (30, codex undercount by 43 %) is cited with its recurrence vector. |
| `HANDOFF.md` | §6 next-move | ACCEPT | "CH2 verifies Lock 14 grammar-neutrality of every proposed candidate" (`HANDOFF.md:150`) — the contract self-binds this lens; next-move chain does not skip a grammar-neutrality gate. |
| `HANDOFF.md` | §7 refusal conditions | ACCEPT | Refusal items 11 (Lock 14 binding for new directives / BIR variants / BackendShape / public substrate API) + 12 (G-SIMD-GRAMMAR-POLICY for bbnf-simd consumers) directly mirror Lock 14's verification commands. |
| `HANDOFF.md` | §8 disposition | ACCEPT | PENDING until CHALLENGE V1; aligns with this dispatch. |
| `alpha-A-results-extraction.md` | §audit-overlay | ACCEPT | Per-row audit verdicts apply uniformly across `parse_only` / `direct` / `typed` / CSS rows; `grammar-neutral` substrate accounting cited correctly at line 267. |
| `alpha-B-competitor-deltas.md` | comparator overlay | ACCEPT | Competitor strictness disclosed per plane; CSS overlay calls out CORPUS-PENDING-R5 + PIPELINE-PENDING-R4 as parallels to JSON's COMPARATOR-PENDING-R1 — symmetric framing. |
| `alpha-C-redress-digest.md` | §pre-block P-6 | ACCEPT | (`alpha-C-redress-digest.md:291-324`) — Lock 14 binding stated verbatim; recurrence vector identified; SK-V14 binding enforces trait dispatch + emit + zero per-grammar files in generic crates; falsifiability gate is the Lock 14 verification command. |
| `alpha-C-redress-digest.md` | §pre-block P-7 | ACCEPT | Track-1 ≡ Track-2 separation; Lock 1 enforcement; no grammar leak. |
| `alpha-C-redress-digest.md` | other pre-blocks | ACCEPT | P-1…P-5 are grammar-neutral patterns by construction. |
| `alpha-D-validated-invalidated.md` | §S-3 Lock 14 | ACCEPT | (`alpha-D-validated-invalidated.md:482-503`) — 30 violations correctly cited; reopen path is the trait + emit collapse; falsifiability gate matches Lock 14 verification command. |
| `alpha-D-validated-invalidated.md` | §V-4 + §V-5 | ACCEPT | bbnf-simd 52 files + Tape + OffsetFlags carried as grammar-neutral. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT | Five candidates; C-1's gate composite (per-grammar string greps + per-grammar dir count) is grammar-neutral in aggregate. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 detail | REVISE | See §2 critical finding #2 — Owner-path enumeration relies on the eight currently-existing grammar-named provider files; the candidate text should additionally bind the *future* invariant ("any new grammar lands as `.bbnf` source + workspace metadata; ZERO new files in generic crates"). |
| `alpha-E-candidate-shortlist.md` | §4 C-2 detail | ACCEPT | Comparator rebind is per-plane; no grammar branch; strict per-corpus deser binds T-by-corpus, not T-by-grammar-family. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail | REVISE | See §2 critical finding #1 — the xtask is described in CSS-specific shape (`regen_css` bin name, `gate-css` subcommand); should be re-cast as the first instance of a `regen-{grammar}` family with the CSS instance bound first (Sheets / BBNF-self / future grammars follow the same xtask shape). |
| `alpha-E-candidate-shortlist.md` | §6 C-4 detail | REVISE | See §2 critical finding #3 — the W11.1 numeric-array dispatch is correctly named as the test row, but the candidate should explicitly forbid the same code path from carrying a `match grammar { Json => ..., CssL4 => ... }` arm; the CSP shape selection MUST keep the dispatch layer grammar-neutral. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 detail | ACCEPT | Revert covers JSON parse_only + CSS rows symmetrically; no grammar branch. |
| `alpha-E-candidate-shortlist.md` | §8 consolidated pre-blocks | ACCEPT | P-6 carried verbatim; consolidated list is grammar-neutral. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | Sub-wave structure ("one CSS sub-wave at a time") is for ledger-serialisation, not for grammar privilege. |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps | REVISE | See §2 critical finding #4 — table column "C-1 sub-waves (8 grammars)" enumerates the recurring vector but the per-iter-equality column and hot-leaf column requirements from C-2 and C-4 should also be telemetry-tagged as grammar-keyed (so a non-JSON grammar's hot-leaf measurement is recorded under its own row family, not folded into JSON aggregate). |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | Escalation triggers are measurement-keyed, not grammar-keyed. |
| `DISPATCH-CONTEXT.md` | §0–§3 + per-agent | ACCEPT | The α-agent scope spec is the binding the present artefacts execute against; Lock 14 is bound at §0 line 16 and §α-E line 165; CH2 cited correctly. |

## §2 — Critical findings

The findings below are REVISEs that strengthen Lock 14 binding without
unwinding any candidate. Zero REJECTs. The orchestrator may safely
advance these as V2 folds against the α-E and α-F authors.

### Finding 1 — C-3 `regen-css` xtask must be presented as the CSS instance of a `regen-{grammar}` family

**Files / lines.** `SYNTHESIS.md:241` (C-3 candidate row); `SYNTHESIS.md:83` (R4 R-target text); `alpha-E-candidate-shortlist.md:277-374` (full C-3 §5).

**The CH2 concern.** Lock 14 requires that adding a new grammar is a
config + grammar-source change with NO code change in any generic or
other-grammar crate (`LOCKS.md:220-238`). A `cargo xtask regen-css`
binary is grammar-suffix-named at the binary-name level; this is
acceptable as a current-instance binding *iff* the implementation
shape factors through a grammar-agnostic generator that the binary
merely parametrises. The α-E §5 owner-path enumeration is consistent
with that — `skinny/crates/codegen/src/lib.rs` is named as the
generic codegen entry consuming the 15 grammars, and the existing
`regen-json` shape at `xtask/src/main.rs:121-127` is cited as the
template. The REVISE asks that this contract be made explicit:

> "C-3 lands a `regen-css` xtask that is the FIRST INSTANCE of the
>  `regen-{grammar}` family; the generic codegen entry it invokes is
>  the same surface a future `regen-sheets` / `regen-bbnf-self` /
>  `regen-{new}` binary will invoke with different grammar metadata.
>  No CSS-specific code lands inside `skinny/crates/codegen/src/lib.rs`;
>  the xtask binary is a thin parametrisation."

**Why it matters.** Without the explicit family-shape binding, a
later wave could legitimately read the C-3 text as authorising a
CSS-specific generator surface inside the generic codegen crate —
which would re-introduce P-6 under a different label.

**Recommended fold.** α-F SYNTHESIS §3 C-3 row appends "(first
instance of the `regen-{grammar}` family; the xtask binary
parametrises a grammar-neutral generator)". α-E §5 ¶1 (Purpose)
appends the same.

### Finding 2 — C-1 owner-path text should bind the FUTURE invariant alongside the CURRENT recurrence-vector enumeration

**Files / lines.** `alpha-E-candidate-shortlist.md:109-134` (C-1 owner paths); `SYNTHESIS.md:241` (C-1 candidate row).

**The CH2 concern.** The owner-path enumeration is correct as a
demolition list for the existing JsonGrammar / RuntimeProvider::Json /
parse_json_grammar / 8 per-grammar provider modules / 64 hand-written
per-grammar runtime files. The post-redress falsifiability gate is
also correct (`find crates/core/src/runtime -mindepth 1 -maxdepth 1
-type d` returns ZERO per-grammar dirs). But the candidate text does
not assert the *forward invariant* — that any new grammar added
after SK-V14 close also lands as `.bbnf` source + workspace metadata
with ZERO new files in any generic crate. Lock 14's wording
("Adding a new grammar is a config + grammar-source change with NO
code change in any generic or other-grammar crate", `LOCKS.md:220-238`)
is the source of this invariant.

**Why it matters.** A purely retrospective demolition gate (delete
the 8 + 64 files now present) can pass while a future wave silently
adds a 9th provider module for a new grammar. The forward invariant
must be encoded in C-1's same-wave consumer plan so the Lock 14
baseline gate (`bbnf-bench::lock14_baseline::validate`) rejects any
post-PRUNE addition.

**Recommended fold.** α-E §3 C-1 §"Falsifiability gate" appends:

> "Forward invariant (post-redress, permanent): any new grammar
>  added under `workspace.metadata.bbnf.grammars.{name}` produces
>  ZERO new `.rs` files in `skinny/crates/{codegen, runtime,
>  passes, bbnf, grammar}/src/` and ZERO new directories in
>  `crates/core/src/runtime/`. The Lock 14 baseline gate rejects
>  any commit that violates this."

α-F SYNTHESIS §4 S-P3 constraint list (currently line 272-275)
appends a matching clause: "C-1's forward invariant is permanent;
S-P3 wave plans MUST cite it as the pre-condition for any new
grammar admission wave (BBNF-self, Sheets, future grammars)."

### Finding 3 — C-4 must forbid grammar-branched dispatch inside the CSP shape consumer

**Files / lines.** `alpha-E-candidate-shortlist.md:375-476` (C-4 detail); `SYNTHESIS.md:244, 282-284` (C-4 candidate row + S-P3 decision-engine fail-closed clause).

**The CH2 concern.** C-4 wires the CSP solver's shape selection into
the codegen path. The W11.1 numeric-array dispatch is named as the
test row (`alpha-E §6:439-444`). The CSP solver itself is
grammar-neutral per `v4-decision-engine-trace §1`. The risk is that
the codegen template-selection dispatch in `skinny/crates/codegen/
src/lib.rs` (post-C-1 trait dispatch) carries an implicit
`match shape { NumericArray => json_path, ...}` arm that *happens* to
be JSON-only because the only shape with a runtime path is the
numeric-array shape from W11.1. That implicit binding would satisfy
the falsifiability gate (hot leaf attribution changes on the named
JSON row) while leaking a JSON-keyed branch into the shape consumer.

**Why it matters.** The S-P3 constraint at `SYNTHESIS.md:282-284`
says "the hardcoded P1–P8 cascade fails closed for JSON / CSS /
Sheets / BBNF-self rows"; the spirit is that EVERY grammar's CSP
shape must be honoured uniformly. The candidate text does not
currently bind this constraint to C-4 directly.

**Recommended fold.** α-E §6 C-4 §"Pre-blocked routes" appends:

> "The shape consumer in `skinny/crates/codegen/src/lib.rs` MUST
>  dispatch on the CSP-emitted `BackendShape` enum alone; no
>  `match grammar { Json => ..., CssL4 => ... }` arm may appear in
>  the dispatch path. The shape consumer is exercised by AT LEAST
>  ONE non-JSON grammar in the same wave OR carries a
>  documented intrinsic-block proof that the chosen shape does not
>  yet apply to non-JSON grammars (in which case the wave's scope
>  is the shape, not the grammar)."

α-F SYNTHESIS §4 S-P3 constraint list (currently line 282-284)
appends: "The C-4 shape consumer is exercised across at least two
grammar families before any C-4 admit cites runtime divergence as
load-bearing; one-grammar runtime divergence is wave evidence, not
admit evidence."

### Finding 4 — α-E §10 telemetry-tagging clarification

**Files / lines.** `alpha-E-candidate-shortlist.md:615-630` (§10 cost / caps / telemetry).

**The CH2 concern.** The cost table lists "C-1 sub-waves (8 grammars)"
with uniform hard caps, which is correct. The telemetry sentence at
`§10:628-630` reads:

> "Telemetry per `PASS-ALPHA.md §4.3` (column set unchanged);
>  per-iter equality column added by C-2; hot-leaf attribution
>  column required for every row per C-4's gate."

This is grammar-neutral as written. The REVISE asks for one
additional sentence binding the hot-leaf column's symbol-path
namespace to be grammar-keyed (so a future BBNF-self or Sheets row's
hot leaf reads as `bbnf_self::parse_thing` not as a JSON-default
symbol from a stale inherited profile). This is consistent with the
`Hot leaf` rule at `SYNTHESIS.md:224` ("stale inherited profile
names fail S-P1") but should be made explicit at the candidate level.

**Why it matters.** Lock 14's verification command set
(`LOCKS.md:220-238`) keys on symbol prefixes (`JsonParser`,
`CssL4Parser`, `BbnfBootstrap`, etc.). The hot-leaf column is the
runtime mirror of that gate; it must read grammar-keyed for every
grammar to keep the audit-overlay verdict honest.

**Recommended fold.** α-E §10 telemetry sentence appends:

> "The hot-leaf column reads as a grammar-keyed symbol path
>  (`{grammar}::parse_*` or equivalent); a stale inherited symbol
>  name on a non-JSON row fails the per-row gate the same way it
>  fails S-P1."

## §3 — Recommended folds for V2 (if any)

The four V2 folds are all REVISEs that bind explicit Lock 14
forward invariants to candidates whose architectural shape is
already correct. None of them requires re-dispatch of α-A / α-B /
α-C / α-D. The recommended dispatches:

1. **Re-dispatch α-E** (single redress wave, ~30 min cap per
   `[dispatch-hard-cap]`):
   - Fold Finding 1 into §5 C-3 ¶1 (Purpose) and §3 C-3
     (Falsifiability gate as needed).
   - Fold Finding 2 into §3 C-1 (Falsifiability gate) appending
     forward invariant clause.
   - Fold Finding 3 into §6 C-4 (Pre-blocked routes) appending
     dispatch-discipline clause.
   - Fold Finding 4 into §10 (telemetry sentence) appending
     grammar-keyed hot-leaf clause.

2. **Re-dispatch α-F** (single redress wave, ~30 min cap):
   - Fold Finding 1 mirror into SYNTHESIS §3 C-3 row + §0.3 R4 text.
   - Fold Finding 2 mirror into SYNTHESIS §4 S-P3 constraint list
     (after line 275).
   - Fold Finding 3 mirror into SYNTHESIS §4 S-P3 constraint list
     (after line 284).

3. **No re-dispatch needed for α-A / α-B / α-C / α-D.** Lock 14
   coverage in those artefacts is correct; CH3 (REDRESS) and CH7
   (Overfit-Prune) may surface concerns there independently, but
   CH2 does not.

The four folds together close the explicit-binding gap CH2 surfaces
without altering the candidate slate's substance. The aggregator
may treat CH2 as ACCEPTING the slate with the four folds queued for
V2. The cycle remains on track for ≥95 % ACCEPT convergence per
`ORCHESTRATOR.md §3Z`.
