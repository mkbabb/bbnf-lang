# SK-V9 S-P3 CHALLENGE V2 — CH2 GENERALITY (Lock 14)

Lens: CH2 GENERALITY. Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2.
Date: 2026-05-18.
Cohort under review: the V2-folded `research/p3/` seven artefacts —
P3-A (shortlist), P3-B (wave sequencing), P3-C (falsifiability gates),
P3-D (telemetry schema), P3-E (pre-blocked ledger), P3-F-spec-draft,
P3-F-dispatch-draft. P3-A/B/C/F were re-authored under the V2 fold
(mtimes 18:23–18:29, post-dating the V1 CH2 at 18:19; each carries a
`§0 — V2 fold` footer). P3-D and P3-E were not re-authored
(mtimes 18:13–18:14, pre-V1) — material below.
Authority: `restart/locks/LOCKS.md` Lock 14 (line 60); ORCHESTRATOR §3W
(CH2: "Lock 14 holds: no grammar-name leak; every proposed intervention
is grammar-neutral and works for CSS L4 / Sheets / BBNF-self, not only
JSON"); ORCHESTRATOR §3Z (V2 is the candidate first qualifying cycle).

V1 CH2 verdict was ACCEPT-WITH-NOTE: 26 ACCEPT / 5 ACCEPT-WITH-NOTE /
1 REVISE. The single REVISE was L4 — the SPEC W4/W5 codec/string-block
wave split contradicting P3-B/P3-C's same-wave pairing. The four leaks
L1–L4 are the V2-fold subject of this verification.

---

## §1 — V1-leak resolution

CH2's job for V2 is narrow: confirm the one REVISE (L4) is folded, the
three ACCEPT-WITH-NOTE leaks (L1, L2, L3) are addressed, the V2 SPEC
carries the per-wave Lock-14 gate, and the integration introduced no
new Lock-14 leak.

### L4 — codec/string-block wave split — **RESOLVED, clean fold.**

The V1 REVISE. V1's defect: P3-F-spec split the codec (W5) and the
string-block widening (W4) into two sequential SPEC sections while
P3-B/P3-C bound them as one wave because P2-E §6.4's conditional rule
makes `unicode_mixed` admission contingent on same-wave pairing; the
SPEC §8 still labelled W5 "paired with W4" while sequencing it after.

The V2 SPEC re-authors this completely. The codec and string-block
widening are now W4b and W4a — **the same `W4` bracket**, sub-waved.
The Lock-14-relevant evidence that they are *paired, not split*:

- §2 manifest (line 292): W4b dispatch status reads verbatim "PAIRED
  with W4a (strictly adjacent; neither closes the four uncloseable rows
  alone)". W4a and W4b are both `Section 7.x` — the same Section 7
  ("W4 aarch64 Substrate Consumers"), not two top-level sections.
- §7.2 (lines 752–758): a dedicated **"Pairing."** paragraph — "W4b is
  **PAIRED with W4a — strictly adjacent, never separable.** P2-E §6.4
  is the binding honest verdict: zero of the four uncloseable rows
  admit on the codec alone … a standalone codec wave would
  paper-close."
- §2.2 names "Codec/scanner pairing" as one of the three distinct
  same-wave relations (relation 3): "W4a and W4b are strictly adjacent
  sub-waves because neither closes the four uncloseable rows alone."
  The V1 conflation that produced the split is explicitly dismantled.
- §7.2 entry gate: "W3 closed … W4a closed (the paired scanner widening
  is live)." §7.2 exit-gate clause 3: `unicode_mixed` "Admits iff the
  *combined* W4a string-block + W4b codec measured Mbps clears 12338.
  If W4a did not land, `unicode_mixed` stays NO-GO."
- The P3-F-dispatch-draft (`§0` footer + Wave Manifest) carries the
  same W4a/W4b paired structure verbatim.

The codec's grammar-neutrality proof (the five const-generic bindings)
now lands inside W4b, which is strictly adjacent to W4a — the proof and
the string-block widening are no longer in different waves. The W4b
§2.1 generality gate (exit-gate clause 8) and the W4a §2.1 generality
gate (exit-gate clause 6) are both inside the W4 bracket. L4 is folded
with no residue: there is no surviving "W5 codec" section; W5 (§8) is
the docs-only close wave. **L4 RESOLVED.**

### L1 — JSON-corpus results not flagged as JSON-internal — **PARTIALLY RESOLVED.**

V1 asked for two clauses: (a) P3-C §4 to state the four-row unicode
projection is JSON-corpus-internal and the codec's grammar-neutrality
rests on the five const-generic bindings, not those rows; (b) P3-B §3
W1 to state Apache/CITM are JSON corpora and the +2 lift is a JSON-domain
result while the admission *methodology* generalises.

Neither clause was added. P3-C §4.1's four-row projection table
(re-authored — the V2-fold footer at §0 line 405 confirms) still
presents `unicode_escapes`/`y_string_unicode`/`unicode_mixed`/`gsoc-2018`
with no "JSON-corpus-internal" qualifier, and §4 still frames the
section as "the load-bearing honesty point" without distinguishing the
JSON-corpus rows from the const-generic-binding grammar-neutrality
proof. `rg 'JSON corpora|JSON corpus|JSON-corpus|JSON-internal|JSON-domain|methodology generalis'`
returns zero hits in P3-B and P3-C.

This is **not a Lock-14 fault** and was never blocking — V1 itself
graded L1 non-blocking and explicitly said "the SPEC §2.1 gate already
protects the *substrate*; this protects the *prose* from over-reading."
The substrate framing remains correct everywhere. But the V2 fold did
not close the prose gap. Carried to §3 as a NOTE; CH2 does not gate on
it. The honesty content L1 asked for is partly *recovered elsewhere*:
the SPEC §0.2 goalset table row 3/4 and §7.2 do bind the codec's
grammar-neutrality to "five const-generic bindings (JSON-4, CSS L4
variable, JS `\u{}`, TOML-4, TOML-8)" — a reader who reaches §7.2
exit-gate clause 8 ("the five const-generic bindings prove
grammar-neutrality") is told the proof is the bindings, not the rows.
The over-read risk survives only in P3-C §4 read in isolation.

### L2 — `grammar_id` hard-reject presented without scoping — **NOT RESOLVED.**

V1 asked P3-D §2.1 to note the schema is JSON-bench-scoped by
construction and that a future non-JSON bracket re-parameterises
`grammar_id` rather than treating `json` as the universe. P3-D was
**not re-authored** for V2 — its mtime (18:13) pre-dates the V1 CH2
(18:19), it carries no `§0 — V2 fold` footer, and `grammar_id`
(field 2, §2.2 line 68) still reads the bare "`json` (W0 rejects
non-`json`)" with no scoping sentence. The V2 fold per the consolidation
plan dispatched F-MAIN (P3-F) + F-AUX (P3-B/C/A surgical) — P3-D and
P3-E were outside both agents' scope.

This is **not a Lock-14 fault**: `validate_sk_v8_w0` lives in
`bbnf-bench`, which is not a Lock-14 generic crate, and `grammar_id`
hard-rejecting non-`json` is bench-harness scoping, not substrate
policy. It does not gate CH2. But it is a V1 leak the V2 fold did not
close; carried to §3 as a NOTE. Mitigating: the SPEC §0.y is the
authority the dispatch binds to, and §0.y describes `grammar_id` only
as part of the 36-field set without over-claiming it as a substrate
universe — the leak survives only in P3-D's own §2.2 prose.

### L3 — codec grammar-neutrality compile-proven vs production-exercised — **RESOLVED IN SUBSTANCE.**

V1 asked the cohort to state explicitly that grammar-neutrality is
compile-proven for four of five codec bindings and production-exercised
for one (JSON-4). The V2 artefacts now carry this distinction in
substantive form, though not in V1's exact words:

- P3-A C4 (re-authored, V2-fold footer §0 line 777): "**Same-wave
  consumer.** The **production consumer** is the JSON materialiser …
  the already-wired `unescape_four_unicode_escapes` x4 path … The wave
  *re-bodies* an existing production path; it does not introduce a new
  consumer. CSS L4 + TOML ship as **scaffolds** (compile-validated, no
  production parse loop)." The production-vs-scaffold line is explicit.
- P3-C §"Same-wave consumer" row for W4 (line 131): "one production
  consumer — the already-wired x4 JSON path … plus two scaffolds (CSS
  L4, TOML — compile-validated `#[cfg(test)]`, no production path, no
  falsifiability gate)."
- SPEC §7.2 owner-paths table: `bbnf-css/tests/` is tagged "CSS L4
  scaffold (`#[cfg(test)]`, compile-only)"; the production re-body is
  the `parse-that-regex/src/lib.rs:402` x4 path.
- P3-A §0 also cites P2-E §4 as the source for the "same-wave consumer
  + scaffold distinction" (mirrored in P3-C §5 source 6).

The distinction V1 wanted — one production consumer, four
compile-proven scaffolds — is now stated plainly across P3-A, P3-C and
the SPEC §7.2. A later auditor cannot read "five bindings" as "five
production paths". **L3 RESOLVED in substance** (V1's verbatim clause
"scaffold compile is the Lock-14 evidence" is not quoted, but the
content is unambiguous and correct).

### Summary

| V1 leak | V1 class | V2 status | CH2-gating |
|---|---|---|---|
| L4 — SPEC codec/string-block split | REVISE (the one) | **RESOLVED — clean fold** | Was the gating defect; now clear |
| L1 — JSON-corpus rows not flagged | ACCEPT-WITH-NOTE | PARTIALLY RESOLVED (substrate ok; P3-C §4 prose gap survives) | Non-gating |
| L2 — `grammar_id` scoping | ACCEPT-WITH-NOTE | NOT RESOLVED (P3-D not re-authored) | Non-gating |
| L3 — codec compile-proven vs production | ACCEPT-WITH-NOTE | RESOLVED in substance | Non-gating |

The one REVISE is folded clean. Two of three NOTEs are closed (L3) or
substrate-clean with a prose-only residue (L1); one (L2) is untouched
because P3-D was outside the V2-fold dispatch scope. None of L1/L2/L3
is a Lock-14 fault — all three are generality-honesty prose in
non-generic-crate artefacts.

---

## §2 — V2 dispositions

Twenty-four rows. Each cites the V2-artefact location and the S-P2 / V1
provenance. Dispositions: ACCEPT / ACCEPT-WITH-NOTE / REVISE / REJECT.

| # | V2 artefact / location | Lock-14 claim under review | Provenance | Disposition |
|---:|---|---|---|---|
| 1 | SPEC §2 manifest, W4b row | W4b status reads "PAIRED with W4a (strictly adjacent; neither closes the four uncloseable rows alone)". | V1 L4; P2-E §6.4. | ACCEPT — the V1 REVISE folded into the manifest table itself; codec + string-block are one `W4` bracket. |
| 2 | SPEC §7.2 "Pairing." paragraph | "W4b is PAIRED with W4a — strictly adjacent, never separable … a standalone codec wave would paper-close." | V1 L4; P2-E §6.4; P3-C §4.3. | ACCEPT — the V1 contradiction (W5 "paired" yet sequenced-after) is dismantled; W4b is a W4 sub-wave, not a separate top-level section. |
| 3 | SPEC §2.2 same-wave relation 3 | "Codec/scanner pairing — W4a and W4b are strictly adjacent sub-waves because neither closes the four uncloseable rows alone." | V1 L4; CH5 cascade-lock ambiguity. | ACCEPT — the three same-wave relations are now named distinctly; the V1 conflation that produced the split is explicitly retired. |
| 4 | SPEC §7.2 exit gate clause 3 | `unicode_mixed` "Admits iff the *combined* W4a string-block + W4b codec measured Mbps clears 12338. If W4a did not land, `unicode_mixed` stays NO-GO." | P2-E §6.4. | ACCEPT — the codec's grammar-neutrality proof and the string-block widening are co-located in one bracket; admission is the honest combined-measurement rule. |
| 5 | SPEC §2.1 Generality and Lock 14 Gate — header | "Every wave carries this exit gate; the checks tighten when generic crates are edited." | V1 question 3; Lock 14. | ACCEPT — the per-wave Lock-14 gate is present, re-authored, and binding on every wave. |
| 6 | SPEC §2.1 — public API scan | "no new public JSON-named API in generic crates (`bbnf-simd`, `parse-that-regex`, `codegen` outside per-grammar template files, `runtime` outside `grammars/json/`)." | Lock 14 verification command. | ACCEPT — the generic-crate set is named precisely; the per-grammar-instance carve-out (`grammars/json/`, per-grammar template files) matches Lock 14's explicit allowance. |
| 7 | SPEC §2.1 — grammar branch scan | "no generic branch selects behavior by JSON grammar name, corpus name, object/array role, field name, or punctuation meaning." | Lock 14 `match grammar` prohibition. | ACCEPT — covers the `match grammar` arm prohibition and extends it to corpus/role/field — strictly stronger than Lock 14's literal text. |
| 8 | SPEC §2.1 — primitive/table scan | "no generic primitive, SIMD table, or classifier embeds JSON structural policy unless it is generated byte-set data plus opaque class ordinals with a scalar reference and a same-wave consumer. The W3 class column stores opaque class ordinals; `class_table` (`bbnf-simd/src/lib.rs:41`) is generated data." | P2-A §227/§628; Lock 14 generated-data exception. | ACCEPT — the load-bearing CH2 framing: per-grammar variation is bound to codegen-emitted data, not generic-crate branches. The one generic-crate primitive (`class_table`) is named and bound to the generated-data exception. |
| 9 | SPEC §2.1 — non-JSON proof clause | "every generic-crate edit (W3 SIMD chain + codegen template, W4a `string_block.rs`, W4b `escape_codec`, W4c EOR3, W4d CTZ) carries a CSS L4 / Sheets / BBNF-self proof." Names P2-A union instances, P2-B Sheets witness, P2-E five const-generic bindings. | P2-A §3, P2-B §1.4, P2-E §1.x. | ACCEPT — every behaviour-SIMD edit in the V2 sub-wave structure is enumerated and bound to a named non-JSON proof. |
| 10 | SPEC §1 non-negotiables | "No JSON policy in generic crates. Every generic-crate edit carries a non-JSON proof (Section 2.1)." | Lock 14. | ACCEPT — the non-negotiable is present, binds every wave, and routes to §2.1 as single source. |
| 11 | SPEC §6 W3 exit gate clause 7 | "Section 2.1 generality scan passes — no JSON-named symbol enters a generic crate (P2-A §4.4 #5); the CSS L4 / Sheets / BBNF-self union instances compile." | P2-A §3/§4.4. | ACCEPT — per-wave Lock-14 gate present in the substrate-touching wave with the cross-grammar compile as a named check. |
| 12 | SPEC §6 W3 exit gate clause 5 | "The class column carries only structural ordinals … no `Number`/`Literal` ordinal leaks into the structural alphabet (P2-A §4.4 #6)." | P2-A §4.4 #6. | ACCEPT — this clause is itself a Lock-14 guard against JSON-value-kind policy entering the generic structural alphabet; correctly carried. |
| 13 | SPEC §7.1 W4a exit gate clause 6 | "`scan_string_special_block_32` is a per-string-span scanner with no JSON structural policy." | P2-D §4.0. | ACCEPT — the 32-byte block scanner is byte-pattern, grammar-neutral; the per-wave gate is present in the W4a sub-wave. |
| 14 | SPEC §7.2 W4b exit gate clause 8 | "the five const-generic bindings prove grammar-neutrality (CSS L4 / JS / TOML bindings compile)." | P2-E §1.x. | ACCEPT — the codec wave's Lock-14 check is a *compile of the non-JSON bindings*, an exercised proof not a claim. Resolves the L1/L3 substrate concern at the SPEC layer. |
| 15 | SPEC §7.3 W4c / §7.4 W4d exit gates | Both carry "Section 2.1 generality scan passes." | P2-D §5.3.1, §4.4. | ACCEPT — every W4 sub-wave (a/b/c/d), each landing a SIMD/ASM kernel, carries the per-wave Lock-14 gate. No sub-wave is exempt. |
| 16 | SPEC §7.3 W4c — `FEAT_SHA3` gate | The EOR3 path is "`FEAT_SHA3`-conditional … a capability-conditional specialisation, the same admissibility shape as `digit_mac` (DotProd-gated)." | P2-D Lock 16 caveat; P2-E §2. | ACCEPT — the EOR3 path is a Lock-16 host-capability gate, not a grammar gate; the admissibility predicate is grammar-neutral. No new Lock-14 surface. |
| 17 | SPEC §5 W2 — Sheets witness owner path | `runtime/src/grammars/sheets_witness/event_grammar_witness.rs` named as "the Lock 14 non-JSON witness". | P2-B §1.4. | ACCEPT — the `sheets_witness/` directory is a per-grammar-instance dir Lock 14 explicitly permits; the `_witness` naming telegraphs no production parser ships. |
| 18 | SPEC §5 W2 exit gate clause 4 | "The Lock 14 `rg` audits (P2-B §3.3) report every `admits_fact` / `admits_class` match inside `event_grammar.rs`, a witness file, or the proof test — never in generic substrate source." | P2-B §3.3. | ACCEPT — strongest Lock-14 instrumentation in the cohort: Lock 14 is a compile-checked + `rg`-audited admission criterion. Carried intact from V1 row 20. |
| 19 | SPEC §5 W2 exit gate clause 1 | "The `EventGrammar` trait compiles, is grammar-neutral by signature (no `match grammar` arm, no role enum)." | P2-B §1.5 `AnyGrammar`. | ACCEPT — the trait is grammar-neutral by signature; the JSON/Sheets witnesses are per-grammar `impl`s in per-grammar-instance dirs. Correct Lock-14 placement. |
| 20 | SPEC §4 W1 exit gate clause 7 | "Section 2.1 generality scan passes — no JSON policy enters a generic crate." | P2-C; Lock 14. | ACCEPT — W1 is bench/xtask-only; the generality gate is stated (vacuous-but-present), consistent with V1 row 11. |
| 21 | SPEC §4 W1 — Lock 14 owner-path row | "`restart/locks/LOCKS.md` (Lock 14): Add `sk-v9-real-typed-w1` parent-diff allowance scoped to the seven owner paths." | P3-E §3.1; P2-C §4.1. | ACCEPT — the Lock-14 *amendment* is a scoped parent-diff allowance entry (not a substrate weakening), gated by `cargo test lock14_baseline`; the revert protocol routes a `lock14_baseline` failure through a Lock 14 amendment rather than silently. Correct. |
| 22 | P3-F-dispatch "Pre-Blocked Routes" item 8 | "Generic JSON policy leaks / Lock 14 weakening (REDRESS 85, 86, 87)" — hard pre-block. + Non-Negotiables: "Every generic-crate edit carries a CSS L4 / Sheets / BBNF-self non-JSON proof (SPEC §2.1)." | P3-E §4 item 8. | ACCEPT — the dispatch prompt carries the Lock-14 hard pre-block and the non-JSON-proof non-negotiable, bound to SPEC §2.1 as single source. V2-fold footer confirms re-authoring. |
| 23 | P3-A C4 owner paths | `bbnf-simd/src/aarch64/escape_codec/` (NEW dir module) + `codegen/src/escape_codec/` const-generic emission for five bindings. | P2-E §1.x; Lock 14 codegen carve-out. | ACCEPT — the NEON kernel is a const-generic primitive in a generic crate, parameterised by grammar-derived data (width/surrogate/terminator), not a `match grammar` arm; the codegen sub-module emits per-grammar specialisation as data. |
| 24 | P3-C §4.1 four-row projection table | The `unicode_escapes`/`y_string_unicode`/`unicode_mixed`/`gsoc-2018` projection table is all-JSON-corpus; §4 frames it "the load-bearing honesty point" without flagging the rows as JSON-internal. | V1 L1; P2-E §6.2. | ACCEPT-WITH-NOTE — see §3 NOTE-1. The codec *primitive* is grammar-neutral and the SPEC §7.2 clause 8 proves it via the five const-generic bindings; P3-C §4 never claims the four rows are a substrate property, but the V2 fold did not add the one-line JSON-corpus-internal clause V1 requested. Non-blocking; substrate framing is clean. |

Bonus rows (cohort completeness beyond the 20 minimum):

| # | V2 artefact / location | Claim | Disposition |
|---:|---|---|---|
| 25 | P3-A §0 V2-fold footer | "V2 fold: F-AUX surgical touch-up per S-P3 V1 CHALLENGE." | ACCEPT — confirms P3-A was re-authored under the F-AUX leg; the production-vs-scaffold L3 distinction at C4 is the fold product. |
| 26 | P3-C §"Same-wave consumer" W4 row | "one production consumer … plus two scaffolds (CSS L4, TOML — compile-validated `#[cfg(test)]`, no production path, no falsifiability gate)." | ACCEPT — the L3 production-vs-scaffold distinction is stated explicitly; an auditor cannot read "five bindings" as "five production paths". |
| 27 | P3-D §2.2 field 2 `grammar_id` | "`json` (W0 rejects non-`json`)" — no scoping sentence. | ACCEPT-WITH-NOTE — see §3 NOTE-2. Not a Lock-14 fault (`bbnf-bench` is not a generic crate); P3-D was outside the V2-fold dispatch scope. Non-blocking. |
| 28 | SPEC §0.2 goalset table rows 3–4 | "`escape_codec_hex_unit` SIMD primitive, paired with the W4a scanner widening" / "neither closes them alone (P2-E §6.4)". | ACCEPT — the goalset itself now states the pairing; L4's fold reaches the close-condition layer, not only the wave manifest. |

---

## §3 — Aggregate verdict

**The V2-folded S-P3 cohort respects Lock 14. CH2 V2 verdict: ACCEPT.**

24 dispositions: **22 ACCEPT / 2 ACCEPT-WITH-NOTE / 0 REVISE / 0 REJECT**
(28 with bonus rows: 25 ACCEPT / 3 ACCEPT-WITH-NOTE). ACCEPT-rate
22/24 = **91.7%** on the core table, 25/28 = 89.3% with bonus rows.
Counting clean-or-substrate-clean (ACCEPT-WITH-NOTE rows are
substrate-clean prose-only NOTEs, not Lock-14 faults) the Lock-14
posture is **100% clean** — every generic crate stays grammar-neutral,
every per-grammar variation is codegen data plus per-grammar wrapper
directories, and no row carries a `match grammar` arm, a grammar-named
generic module, or a grammar-specific generic public type.

The V1 → V2 movement on the gating question:

- **The single V1 REVISE (L4) is folded clean.** The codec and the
  string-block widening are now W4b and W4a — strictly-adjacent
  sub-waves of one `W4` bracket. The SPEC §2 manifest, §2.2 (relation
  3), §7.2 ("Pairing." paragraph + entry gate + exit-gate clause 3),
  the §0.2 goalset, and the P3-F-dispatch-draft all carry the pairing
  consistently. There is no surviving "W5 codec" section; W5 is
  docs-only close. The V1 internal contradiction (W5 labelled "paired"
  yet sequenced-after W4) is gone. The codec's grammar-neutrality proof
  (the five const-generic bindings) and the string-block widening are
  co-located in one bracket. **L4 has zero residue.**

- The V2 SPEC carries the per-wave Lock-14 gate (CH2 question 3
  satisfied): §2.1 is a per-wave exit gate ("Every wave carries this
  exit gate") with four concrete scan types — public API, grammar
  branch, primitive/table, non-JSON proof. Every behaviour wave
  restates the generality scan as a named exit-gate clause: W1 §4#7,
  W2 §5#4 (the `rg`-audited form), W3 §6#7, W4a §7.1#6, W4b §7.2#8,
  W4c §7.3#5, W4d §7.4#4. No wave is exempt. The W2 gate remains the
  cohort's strongest Lock-14 instrumentation: a compile-exercised
  non-JSON Sheets witness plus an `rg` audit.

- CH2 question 2 (per-grammar variation bound to codegen data, not
  generic-crate branches) is satisfied emphatically by SPEC §2.1's
  primitive/table-scan bullet: the W3 `class_table` is named and bound
  to the generated-byte-set-data + opaque-class-ordinal exception; the
  W4b const-generic codec bindings are per-grammar specialisation
  emitted as data by `codegen/src/escape_codec/`, not a `match grammar`
  arm.

The two ACCEPT-WITH-NOTE residues are **generality-honesty prose
gaps**, neither a Lock-14 fault and neither blocking:

- **NOTE-1 (V1 leak L1, partially open).** P3-C §4.1's four-row unicode
  projection table is all-JSON-corpus and §4 still does not state the
  rows are JSON-corpus-internal. The substrate framing is correct
  everywhere — the SPEC §7.2 clause 8 grounds the codec's
  grammar-neutrality in the five const-generic bindings, and §0.2 row
  3/4 restates it — so an auditor who reads the SPEC cannot over-read.
  The over-read risk survives only in P3-C §4 read in isolation. V1
  graded L1 explicitly non-blocking; CH2 V2 does not gate on it.

- **NOTE-2 (V1 leak L2, open).** P3-D §2.2's `grammar_id` field still
  reads "`json` (W0 rejects non-`json`)" without the scoping sentence
  V1 requested. P3-D was not re-authored — it sat outside the V2-fold
  dispatch (F-MAIN = P3-F, F-AUX = P3-B/C/A). `validate_sk_v8_w0` lives
  in `bbnf-bench`, not a Lock-14 generic crate, so this is bench-harness
  scoping, not substrate policy — not a Lock-14 fault. The SPEC §0.y,
  which the dispatch binds to, describes `grammar_id` only as one of
  the 36 fields without over-claiming a substrate universe.

V1 leak L3 (codec compile-proven vs production-exercised) is **resolved
in substance**: P3-A C4, P3-C's W4 same-wave-consumer row, and SPEC §7.2
all state the one-production-consumer / four-compile-proven-scaffold
distinction plainly.

Per ORCHESTRATOR §3Z, V2 is the candidate first qualifying cycle. For
CH2 the V1 marginal-below-clean cause was the single L4 REVISE; that
REVISE is folded clean with zero residue, and the V2 integration
introduced no new Lock-14 leak (§4). CH2 V2 clears clean: the two
surviving NOTEs are non-Lock-14, non-blocking prose gaps in
non-generic-crate artefacts, one of which (L2) is in an artefact the V2
fold deliberately did not touch. **CH2 V2: ACCEPT — qualifying.**

Recommended (non-blocking, for V3 or the SPEC-promotion edit, not a
CH2 gate): fold the two outstanding one-clause honesty additions —
P3-C §4 ("the four-row verdict is JSON-corpus-internal; the codec's
grammar-neutrality is established by the five const-generic bindings of
§2, not by these rows") and P3-D §2.1 (`grammar_id` is JSON-bench-scoped
by construction; a future non-JSON bracket re-parameterises it). Both
are prose, neither touches a generic crate, neither blocks W1 dispatch.

---

## §4 — New Lock-14 leaks introduced by the V2 integration

**None.** The V2 fold introduced no new Lock-14 leak. Verification:

1. **The W4 sub-wave split (W4a/W4b/W4c/W4d) is Lock-14-clean.** The V2
   integration's largest structural change is sub-waving the monolithic
   W4 into four sub-waves. Each sub-wave carries its own §2.1 generality
   gate as a named exit-gate clause (rows 13–15). No sub-wave gained a
   grammar branch, a grammar-named generic module, or a grammar-specific
   generic public type. The sub-wave boundaries are LOC/redress-cap
   driven (§2.2), not grammar-driven. Clean.

2. **The cascade-lock disambiguation (§2.2) introduced no grammar
   coupling.** The three named same-wave relations — cascade-lock,
   same-wave consumer, codec/scanner pairing — are all
   sequencing/dependency relations, none selects behaviour by grammar.
   Clean.

3. **The W1 Lock 14 owner-path row** (SPEC §4, row 21) adds a
   `sk-v9-real-typed-w1` parent-diff *allowance* to `LOCKS.md` Lock 14,
   scoped to seven owner paths and gated by `cargo test lock14_baseline`.
   This is a scoped allowance entry, the same shape P2-C §4.1 framed it
   as — it does not weaken the substrate prohibition and the revert
   protocol routes a `lock14_baseline` failure through a formal Lock 14
   amendment rather than a silent bypass. Not a leak.

4. **The codec owner paths** (`bbnf-simd/src/aarch64/escape_codec/`,
   `codegen/src/escape_codec/`) are a NEON const-generic primitive in a
   generic crate plus a codegen sub-module emitting per-grammar
   specialisation as *data*. The primitive is parameterised by
   const-generic width/surrogate/terminator params (grammar-derived
   data), not a `match grammar` arm. The `bbnf-css/tests/` CSS L4
   scaffold is `#[cfg(test)]` compile-only in a per-grammar test
   location. Lock-14-clean — this is the codegen-emitted-data exception
   Lock 14 permits.

5. **No generic-crate symbol gained a JSON name.** SPEC §2.1's public
   API scan, §6#7, §7.1#6, §7.2#8 all restate the
   `no-JSON-symbol-in-a-generic-crate` falsifier (P2-A §4.4 #5). The V2
   integration carried it into every behaviour wave's exit gate; it
   added no exception.

The V2 integration is a pure re-authoring fold of already-accepted
P3-A..E content into the P3-F SPEC + dispatch drafts. No new generic
crate is touched, no new public type minted, no new grammar branch
introduced. The Lock-14 surface area of the cohort is unchanged from
V1; the one V1 generality-adjacent REVISE (L4) is removed.

**Verdict carried to V2 consolidation:** CH2 V2 — **ACCEPT, qualifying**.
The V2-folded S-P3 cohort respects Lock 14: generic crates stay
grammar-neutral, per-grammar variation is codegen data plus per-grammar
wrapper directories, the per-wave Lock-14 gate is present in every
behaviour wave (and `rg`-audit-exercised at W2). The single V1 REVISE
(L4) is folded clean with zero residue and the integration introduced
no new Lock-14 leak. Two non-Lock-14, non-blocking honesty-prose NOTEs
survive (L1 in P3-C §4, L2 in P3-D §2.2 — the latter in an artefact the
V2 fold did not re-author); both are one-clause additions recommended
for V3 or the SPEC-promotion edit, neither gates CH2 and neither blocks
W1 dispatch.
