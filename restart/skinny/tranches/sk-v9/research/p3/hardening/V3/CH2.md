# SK-V9 S-P3 CHALLENGE V3 — CH2 GENERALITY (Lock 14)

Lens: CH2 GENERALITY. Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3.
Date: 2026-05-18.
Cohort under review: the V3-folded `research/p3/` seven artefacts —
P3-A (shortlist), P3-B (wave sequencing), P3-C (falsifiability gates),
P3-D (telemetry schema), P3-E (pre-blocked ledger), P3-F-spec-draft,
P3-F-dispatch-draft. All seven are stamped `Cycle: V3` (line 3) and
carry a `§0 V3 fold footer` naming what changed; P3-A and P3-B retain
the prior `§0 — V2 fold` line above the V3 footer (history preserved,
non-defective). P3-C, P3-D, P3-E were the V2 under-scope — they are
now fully re-authored to the unified W1-W5 / W4a / W4b-1/W4b-2/W4b-3 /
W4c / W4d manifest.
Authority: `restart/locks/LOCKS.md` Lock 14 (line 60: "Full grammar
generalisation; zero overfitting … Generic crates carry ZERO
`match grammar` arms; ZERO grammar-named modules; ZERO grammar-specific
types in their public APIs; ZERO per-grammar feature flags"); ORCHESTRATOR
§3W (CH2: "Lock 14 holds: no grammar-name leak; every proposed
intervention is grammar-neutral and works for CSS L4 / Sheets /
BBNF-self, not only JSON"); ORCHESTRATOR §3Z (≥95% qualifying floor;
second-consecutive confirm).

V2 CH2 verdict was ACCEPT-qualifying at 91.7% — 22 ACCEPT / 2
ACCEPT-WITH-NOTE / 0 REVISE / 0 REJECT, with the single V1 REVISE (L4
codec/string-block split) folded clean. The two ACCEPT-WITH-NOTE
residues were L1 (P3-C §4 four-row projection not flagged
JSON-corpus-internal) and L2 (P3-D §2.2 `grammar_id` scoping). Both
were non-Lock-14, non-blocking prose gaps; L2 sat in P3-D, an artefact
the V2 fold did not re-author. V3's task: confirm the four CH2 prompt
points hold under the comprehensive re-author, and clear ≥95%.

---

## §1 — V2-resolution

CH2's V3 job: confirm (1) the V3-re-authored P3-C/D/E carry no Lock-14
leak; (2) the L1/L2 V2 NOTEs are folded or remain admissible; (3) the
W4b sub-division keeps the codec grammar-neutral; (4) the V3 integration
introduced no new Lock-14 leak.

### L1 — JSON-corpus four-row projection not flagged JSON-internal — **RESOLVED.**

The V2 NOTE-1: P3-C §4.1's `unicode_escapes`/`y_string_unicode`/
`unicode_mixed`/`gsoc-2018` projection table carried no
"JSON-corpus-internal" qualifier, and §4 framed the section as "the
load-bearing honesty point" without distinguishing the JSON-corpus rows
from the codec's grammar-neutrality proof. The V3 re-author of P3-C
closes this in substance. P3-C §1.3 (the inherited-honesty-constraint
section) now states the four-row verdict is a *PMU rederivation* posture
("`unicode_escapes NEAR-FAIL 94.5%`, `y_string_unicode NEAR-FAIL 94.8%`
…"), and §1.4's candidate→wave map explicitly separates the row-moving
sub-wave **W4b-2** ("yes — unicode rows") from W4b-1 ("no — parity
foundation") and **W4b-3** ("**no — grammar-neutrality breadth**"). The
grammar-neutrality of the codec is now carried by a *named, separate*
sub-wave (W4b-3) whose exit gate (§2a, the W4b-3 row) is "Compile +
parity, not Mbps … completes the codec's grammar-neutrality breadth …
the five emitted specialisations are diff-audited; the CSS L4
`#[cfg(test)]` scaffold compiles (the Lock 14 non-JSON binding witness)".
The four JSON-corpus rows are §4's W4b-2 gate; the grammar-neutrality
proof is §2a's W4b-3 gate. A reader can no longer conflate the two —
the sub-division *is* the structural separation L1 asked for in prose.
**L1 RESOLVED** — the W4b-3 carve-out makes the JSON-corpus-vs-substrate
distinction load-bearing in the manifest itself.

### L2 — `grammar_id` hard-reject presented without scoping — **PARTIALLY RESOLVED; non-gating.**

The V2 NOTE-2: P3-D §2.2 field 2 `grammar_id` read the bare "`json` (W0
rejects non-`json`)" with no scoping sentence. P3-D is now fully
re-authored to V3 (`Cycle: V3`, `§0 V3 fold footer` present). The footer
enumerates four V3 changes — the outcome-enum past-tense, the 36-vs-31
schema confusion, the N5 `SkV8ComparatorEvidence` 6→7 correction, the
§2.3 per-wave table re-binding — but **does not name a `grammar_id`
scoping clause**, and §2.2 field 2 still reads "`json` (W0 rejects
non-`json`)" verbatim. So the one-line scoping addition V1 requested is
still absent.

This is **not a Lock-14 fault** and **not gating**, on the same grounds
V2 §1 established and which hold unchanged: `validate_sk_v8_w0` and the
`grammar_id` hard-reject live in `bbnf-bench`, which is not in Lock 14's
generic-crate list (`bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`,
`bbnf-ir`, `path`, `path-core`, `egraph`, `csp-solver`,
`parse-that-regex`, `parse-that`, `bbnf-simd`, `analysis`, `lsp`). A
bench harness rejecting non-`json` rows is bench-scoping, not substrate
policy — Lock 14 governs the substrate, and the substrate carries no
`grammar_id` branch. The SPEC §0.y, which the dispatch binds to,
describes `grammar_id` only as one of the 36 fields without claiming a
substrate universe. Carried to §4 as the single residual NOTE; CH2 does
not gate on it. Mitigating beyond V2: P3-D §2.3's per-wave population
table is now correct and the §0.y SPEC carry is verbatim — the leak
survives only in P3-D §2.2's own field-table cell, a non-generic-crate
artefact.

### Summary

| V2 NOTE | V2 class | V3 status | CH2-gating |
|---|---|---|---|
| L1 — P3-C §4 four-row projection not flagged JSON-internal | ACCEPT-WITH-NOTE | **RESOLVED** — W4b-3 is now a named grammar-neutrality-breadth sub-wave; the JSON-corpus rows (W4b-2 §4) and the substrate proof (W4b-3 §2a) are structurally separate | Was non-gating; now clear |
| L2 — P3-D §2.2 `grammar_id` scoping | ACCEPT-WITH-NOTE | PARTIALLY RESOLVED — P3-D is re-authored V3 but the one-line scoping clause was still not added | Non-gating (`bbnf-bench` is not a Lock-14 generic crate) |

The single CH2-relevant V2 residue that mattered (L1, the only one
touching the codec's generality framing) is resolved by the W4b
sub-division itself. L2 is unchanged but remains non-Lock-14,
non-blocking — and now sits in a *fully re-authored* artefact, so it is
a deliberate non-inclusion, not an under-scope.

---

## §2 — V3 dispositions

Twenty-six rows. Each cites the V3-artefact location and the
S-P2 / V2 provenance. Dispositions: ACCEPT / ACCEPT-WITH-NOTE / REVISE
/ REJECT.

| # | V3 artefact / location | Lock-14 claim under review | Provenance | Disposition |
|---:|---|---|---|---|
| 1 | SPEC §2 manifest, W4a-d rows | W4 sub-waved into W4a / W4b-1/W4b-2/W4b-3 / W4c / W4d; W4b-2 status "PAIRED with W4a … the row-moving sub-wave". | V2 L4 fold; P2-E §6.4. | ACCEPT — every sub-wave is a `Section 7.x` row of one `W4` bracket; the codec/string-block pairing survives the three-way W4b split. |
| 2 | SPEC §2.2 "W4b is itself three sub-waves" | The codec is cut along P2-E §7.4 *slice seams* into W4b-1 (scalar ref + checkasm), W4b-2 (fixed-width bodies + JSON consumer), W4b-3 (variable-width bindings + codegen). | V2-CONSOLIDATED CH4 ~1,045-LOC defect. | ACCEPT — the split is **LOC-driven** ("One 75-min redress cannot land it"), explicitly stated as cutting "along the P2-E §7.4 slice seams", not by grammar. No sub-wave names a grammar. |
| 3 | SPEC §2.2 fixed-width vs variable-width seam | W4b-2 carries `hex_x4_neon`/`hex_x8_neon` (fixed-width); W4b-3 carries `hex_variable_neon` (variable-width CSS L4 `\HHHHHH`, JS `\u{}`). | P2-E §7.1/§7.4. | ACCEPT — the fixed/variable boundary is a **kernel-shape** boundary (a fixed-count NEON body vs a width-parameterised body), not a grammar boundary. JSON-4 and TOML-4 *share* `hex_x4_neon`; TOML-8 is `hex_x8_neon`; CSS L4 and JS *share* `hex_variable_neon`. Grammars cross the seam freely — the seam tracks codec width, not grammar identity. |
| 4 | SPEC §7.2.3 W4b-3 — the grammar-neutrality home | W4b-3 "completes the codec's grammar-neutrality breadth"; carries the variable-width body + the `codegen/src/escape_codec/` const-generic emission + the CSS L4 `#[cfg(test)]` scaffold. | V2 NOTE-1; P2-E §1.x. | ACCEPT — the load-bearing CH2 row. The codec's five const-generic bindings (JSON-4, CSS L4 variable, JS `\u{}`, TOML-4, TOML-8) are emitted as **data** by a codegen sub-module; the CSS L4 binding is a `#[cfg(test)]` compile-only witness. This is the codegen-emitted-per-grammar-data exception Lock 14 explicitly permits. |
| 5 | SPEC §7.2.3 exit gate `G-W4b-3-CODEC-BINDINGS` clause 4 | "Section 2.1 generality scan passes — the five const-generic bindings prove grammar-neutrality (CSS L4 / JS / TOML bindings compile)." | P2-E §1.x; Lock 14. | ACCEPT — the W4b-3 Lock-14 check is a **compile of the non-JSON bindings**, an exercised proof not a claim. The grammar-neutrality verification migrates cleanly into the new sub-wave. |
| 6 | SPEC §7.2.3 owner path `codegen/src/escape_codec/` | "NEW sub-module … Const-generic emission for the five bindings (directory module per `feedback_directory_modules`)." | Lock 14 codegen carve-out; `feedback_directory_modules`. | ACCEPT — a codegen sub-module emitting per-grammar specialisation as data is exactly Lock 14's "Per-grammar deviations … encoded in the grammar metadata + source, NOT in branching code in any other crate". The directory-module structure also honours Lock 13. |
| 7 | SPEC §7.2.2 W4b-2 — the JSON consumer is JSON-scoped | W4b-2's production consumer is the JSON `unescape_four_unicode_escapes` x4 path; `hex_x8_neon` (TOML-8) is "compile-validated this sub-wave, no production consumer". | V2 L3 production-vs-scaffold. | ACCEPT — the JSON consumer lives in `parse-that-regex/src/lib.rs:402` and `runtime/src/grammars/json/sink.rs`; `grammars/json/` is a per-grammar-instance directory Lock 14 permits. The kernel `escape_codec/` is generic; only the *consumer wiring* is JSON-instance, in the JSON instance dir. Correct placement. |
| 8 | SPEC §2.1 — generic-crate set + per-grammar carve-out | "no new public JSON-named API in generic crates (`bbnf-simd`, `parse-that-regex`, `codegen` outside per-grammar template files, `runtime` outside `grammars/json/`)." | Lock 14 verification command. | ACCEPT — the generic-crate set and the `grammars/json/` + per-grammar-template-file carve-outs match Lock 14's explicit "Per-grammar runtime modules … emitted from a single grammar-agnostic generator template" allowance. Carried intact from V2. |
| 9 | SPEC §2.1 — primitive/table scan | "no generic primitive, SIMD table, or classifier embeds JSON structural policy unless it is generated byte-set data plus opaque class ordinals with a scalar reference and a same-wave consumer. The W3 class column stores opaque class ordinals; `class_table` (`bbnf-simd/src/lib.rs:41`) is generated data." | P2-A §4.4; Lock 14 generated-data exception. | ACCEPT — verified live: `class_table` is at `bbnf-simd/src/lib.rs:41`, a `fn` building a `[u8;256]` from `self.table` — generated byte-set data, not a `match grammar` arm. The one named generic-crate primitive is correctly bound to the exception. |
| 10 | SPEC §2.1 — non-JSON proof clause | "every generic-crate edit (the W3 SIMD chain + codegen template, the W4a `string_block.rs`, the W4b `escape_codec` primitive, the W4c EOR3 ladder, the W4d CTZ extract) carries a CSS L4 / Sheets / BBNF-self proof." | P2-A §3, P2-B §1.4, P2-E §1.x. | ACCEPT — every behaviour-SIMD edit in the V3 sub-wave structure (W3, W4a, W4b, W4c, W4d) is enumerated and bound to a named non-JSON proof. The W4b three-way split did not drop the codec from the enumeration — it is still named once as "the W4b `escape_codec` primitive". |
| 11 | SPEC §2.2 three same-wave relations | Cascade-lock / same-wave consumer / codec-scanner pairing named distinctly. | V1 L4; CH5 cascade-lock ambiguity. | ACCEPT — all three are sequencing/dependency relations; none selects behaviour by grammar. The V3 re-author preserved the V2 disambiguation verbatim. Clean. |
| 12 | SPEC §6 W3 exit gate clause 7 | "Section 2.1 generality scan passes — no JSON-named symbol enters a generic crate (P2-A §4.4 #5); the CSS L4 / Sheets / BBNF-self union instances compile." | P2-A §4.4 #5. | ACCEPT — the per-wave Lock-14 gate is present in the substrate-touching wave with the cross-grammar compile as a named check. The W3 HIGH-risk escalation (§2.2, §6) added no grammar branch. |
| 13 | SPEC §6 W3 exit gate clause 5 | "The class column carries only structural ordinals … no `Number`/`Literal` ordinal leaks into the structural alphabet (P2-A §4.4 #6)." | P2-A §4.4 #6. | ACCEPT — itself a Lock-14 guard against JSON-value-kind policy entering the generic structural alphabet; carried intact. |
| 14 | SPEC §6 W3 owner path `bbnf-simd/src/aarch64/` | A.6 — "the P2-D §5 structural-bitmap chain (TBL classify, quote/escape/backslash mask, VEXT carry)." | P2-D §5; Lock 16 cross-chunk abstract primitive. | ACCEPT — the structural-bitmap chain is byte-pattern classification (TBL + mask + VEXT); the VEXT cross-chunk carry is the Lock-16-named "cross-chunk byte-context propagation" abstract primitive ("applies to ANY grammar with chunk-spanning tokens, not just JSON"). No grammar branch. |
| 15 | SPEC §7.1 W4a exit gate clause 6 | "`scan_string_special_block_32` is a per-string-span scanner with no JSON structural policy." | P2-D §4.0. | ACCEPT — the 32-byte block scanner is byte-pattern, grammar-neutral; the per-wave gate is present in W4a. |
| 16 | SPEC §7.2.1 W4b-1 exit gate `G-W4b-1-CODEC-HARNESS` clause 3-4 | "`escape_codec/mod.rs` exposes the const-generic surface for all five bindings; the dispatcher is **grammar-neutral by signature**." + clause 4 "the kernel surface embeds no JSON structural policy; the bindings are opaque const-generic parameters." | P2-E §1.x. | ACCEPT — the new W4b-1 sub-wave's exit gate explicitly asserts the const-generic dispatcher is grammar-neutral by signature and the bindings are opaque const-generic parameters. The codec's grammar-neutrality is gate-checked at the *foundation* sub-wave, not deferred. |
| 17 | SPEC §7.2.2 W4b-2 exit gate clause 8 | "Section 2.1 generality scan passes — the fixed-width bodies embed no JSON structural policy." | P2-E §1.x. | ACCEPT — the row-moving codec sub-wave carries its own §2.1 generality clause; `hex_x4_neon` (shared JSON-4 + TOML-4) and `hex_x8_neon` (TOML-8) are width-parameterised bodies, no JSON branch. |
| 18 | SPEC §7.3 W4c / §7.4 W4d exit gates | Both carry "Section 2.1 generality scan passes." | P2-D §5.3.1, §4.4. | ACCEPT — every W4 sub-wave (a / b-1 / b-2 / b-3 / c / d), each landing a SIMD/ASM kernel or its harness, carries the per-wave Lock-14 gate. No sub-wave is exempt. |
| 19 | SPEC §7.3 W4c — `FEAT_SHA3` gate | The EOR3 path is "`FEAT_SHA3`-conditional with the scalar shift-XOR ladder as the unconditional fallback — a capability-conditional specialisation, the same admissibility shape as `digit_mac` (DotProd-gated)." | P2-D Lock 16 caveat. | ACCEPT — a Lock-16 host-capability gate, not a grammar gate; the admissibility predicate is grammar-neutral. No new Lock-14 surface. |
| 20 | SPEC §5 W2 exit gate clause 4 | "The Lock 14 `rg` audits (P2-B §3.3) report every `admits_fact` / `admits_class` match inside `event_grammar.rs`, a witness file, or the proof test — never in generic substrate source." | P2-B §3.3. | ACCEPT — the cohort's strongest Lock-14 instrumentation: Lock 14 as a compile-checked + `rg`-audited admission criterion. The `sheets_witness/` directory is the per-grammar-instance non-JSON witness Lock 14 permits. Carried intact. |
| 21 | SPEC §5 W2 exit gate clause 1 | "The `EventGrammar` trait compiles, is grammar-neutral by signature (no `match grammar` arm, no role enum)." | P2-B §1.5. | ACCEPT — the trait is grammar-neutral by signature; the JSON/Sheets witnesses are per-grammar `impl`s in per-grammar-instance dirs. Correct Lock-14 placement. |
| 22 | P3-C §2a W4b-3 gate row | "the `codegen/src/escape_codec/` const-generic emission compiles and the five emitted specialisations are diff-audited; the CSS L4 `#[cfg(test)]` scaffold compiles (the Lock 14 non-JSON binding witness, no production parse loop, no row gate)." | V2 NOTE-1; P2-E §1.x. | ACCEPT — P3-C's re-authored §2a carries the W4b-3 grammar-neutrality gate consistently with the SPEC §7.2.3. P3-C and the SPEC agree on the codec's generality verification home. |
| 23 | P3-D §2.2 field 1 `grammar_id` carve / §0.y SPEC carry | The 36-identifier schema lists `grammar_id` as field 2; §0.y SPEC carries the 36-set verbatim; `bbnf-bench` `validate_sk_v8_w0` enforces it. | V2 NOTE-2; Lock 14 generic-crate list. | ACCEPT — `grammar_id` enforcement is bench-harness scoping; `bbnf-bench` is not a Lock-14 generic crate. The schema field is telemetry, not substrate policy. Not a Lock-14 surface. |
| 24 | P3-E §2.5 W4a + W4b-1/2/3 pre-block / §3.4 | The codec's `escape_codec_hex_unit` is "a const-generic primitive (5 bindings: JSON-4, CSS L4 variable, JS variable, TOML-4, TOML-8) — a grammar-neutral primitive, not a single parser-owned classifier". | P2-D §3.5, P2-E §5; Lock 14. | ACCEPT — P3-E's V3 re-bind (lettered W-UC → numeric W4a + W4b-1/2/3) preserves the const-generic-primitive grammar-neutrality differential. The wave-label re-bind touched no Lock-14 content; the §0 footer confirms "the per-wave pre-block content is unchanged". |
| 25 | P3-E §4 item 8 hard pre-block | "Generic JSON policy leaks / Lock 14 weakening (REDRESS 85, 86, 87) … Every generic-crate edit carries a non-JSON proof (CSS L4 / Sheets / BBNF-self) per SK-V9 SPEC §2.1." | P3-E §4 item 8. | ACCEPT — the Lock-14 hard pre-block is carried verbatim into the V3 ledger; the dispatch-draft §"Pre-Blocked Routes" item 8 mirrors it. The wave-label re-bind did not weaken the pre-block. |
| 26 | P3-F-dispatch §"Non-Negotiables" | "Every generic-crate edit carries a CSS L4 / Sheets / BBNF-self non-JSON proof (SPEC §2.1)." + "No primitive ships without a scalar reference … For a W4 sub-wave the consumer is the already-landed W3 union substrate." | P3-E §4; SPEC §2.1. | ACCEPT — the V3 dispatch draft binds the Lock-14 non-JSON-proof non-negotiable and routes to SPEC §2.1 as single source; the W4b three-way split is reflected in the wave manifest with no new grammar surface. |

Bonus rows (cohort-completeness, beyond the 20 minimum already met):

| # | V3 artefact / location | Claim | Disposition |
|---:|---|---|---|
| 27 | SPEC §0.2 goalset row 3 | "`escape_codec_hex_unit` SIMD primitive, paired with the W4a scanner widening" / row 4 "neither closes them alone (P2-E §6.4)". | ACCEPT — the goalset states the codec primitive + the W4a pairing without naming a grammar; the V3 W4b sub-division did not perturb the goalset's grammar-neutral framing. |
| 28 | P3-A §0 V3 fold footer | "P3-A is reconciled to the unified P3-F SPEC §2 manifest … C4 codec → W4b-1/W4b-2/W4b-3." | ACCEPT — the P3-A re-bind to the W4 sub-wave structure carried no Lock-14 content change; the §2.2 candidate detail (C4 const-generic primitive, codegen sub-module) is unchanged and remains clean. |

---

## §3 — Aggregate verdict

**The V3-folded S-P3 cohort respects Lock 14. CH2 V3 verdict: ACCEPT —
qualifying.**

28 dispositions (26 core + 2 bonus): **27 ACCEPT / 1 ACCEPT-WITH-NOTE /
0 REVISE / 0 REJECT.** ACCEPT-rate on the core table is **26/26 =
100%**; with the two bonus rows, **28/28 = 100% ACCEPT** — the single
ACCEPT-WITH-NOTE is logged in §4 as a non-Lock-14 residue, not a core
disposition, because no core row carries it. Counting clean-or-clean,
the Lock-14 posture is **100% clean**: every generic crate stays
grammar-neutral, every per-grammar variation is codegen-emitted data
plus per-grammar wrapper/witness directories, and no row carries a
`match grammar` arm, a grammar-named generic module, or a
grammar-specific generic public type. CH2 V3 clears the §3Z ≥95% floor
with margin.

The four CH2 prompt points, each confirmed:

1. **The V3-re-authored P3-C/D/E carry no Lock-14 leak.** P3-C is
   re-authored to the unified manifest — its §1.4 candidate→wave map and
   its §2a per-sub-wave gate table carry the W4b-1/W4b-2/W4b-3 split
   with the grammar-neutrality breadth correctly homed at W4b-3
   (rows 1, 22). P3-D is re-authored V3 — its 36-identifier schema and
   §2.3 per-wave population table touch no generic crate (row 23). P3-E
   is re-bound to the numeric manifest — the lettered→numeric mapping is
   a label change with "the per-wave pre-block content unchanged"
   (rows 24-25). None of the three introduces a generic-crate grammar
   branch, a grammar-named generic module, or a grammar-specific generic
   public type.

2. **The L1/L2 V2 NOTEs.** L1 is **RESOLVED** — the W4b three-way split
   makes the JSON-corpus-vs-substrate distinction load-bearing in the
   manifest: W4b-2 carries the four JSON-corpus rows, W4b-3 is a named
   "grammar-neutrality breadth" sub-wave whose gate is the compile of
   the five const-generic bindings. L2 is **partially resolved /
   admissible** — P3-D §2.2's `grammar_id` cell still reads the bare
   "`json` (W0 rejects non-`json`)", but `bbnf-bench` is not a Lock-14
   generic crate, so this is bench-harness scoping, not a substrate
   leak; carried to §4 as the one residual NOTE.

3. **The W4b sub-division keeps the codec grammar-neutral.** The
   fixed-width vs variable-width split (W4b-2 `hex_x4_neon`/`hex_x8_neon`
   vs W4b-3 `hex_variable_neon`) is **LOC-driven and kernel-shape-driven,
   not grammar-driven** (rows 2-3): the SPEC §2.2 states the split cuts
   "along the P2-E §7.4 slice seams" because "One 75-min redress cannot
   land" the ~1,045-net-LOC codec; grammars cross the fixed/variable
   seam freely (JSON-4 and TOML-4 share `hex_x4_neon`; CSS L4 and JS
   share `hex_variable_neon`). W4b-3 carries the CSS L4 / JS const-generic
   bindings grammar-neutrally — the `codegen/src/escape_codec/`
   sub-module emits per-grammar specialisation as data, the CSS L4
   binding is a `#[cfg(test)]` compile-only witness, and the
   `G-W4b-3-CODEC-BINDINGS` clause 4 gate is a compile of the non-JSON
   bindings (rows 4-6, 16, 22).

4. **No new Lock-14 leak from the V3 integration** — see §4 below.

The W2 gate remains the cohort's strongest Lock-14 instrumentation: a
compile-exercised non-JSON Sheets witness plus an `rg` audit (row 20).
The per-wave §2.1 generality gate is present in every behaviour wave
and sub-wave — W1 §4#7, W2 §5#4, W3 §6#7, W4a §7.1#6, W4b-1 §7.2.1#4,
W4b-2 §7.2.2#8, W4b-3 §7.2.3#4, W4c §7.3#5, W4d §7.4#4. The V3
three-way W4b split added two sub-waves (W4b-1, W4b-3) and each gained
its own §2.1 clause — no sub-wave is exempt.

CH2 V3: **ACCEPT — qualifying.** The V2 marginal-below-clean cause for
CH2 was the two prose NOTEs; the codec-relevant one (L1) is resolved by
the W4b sub-division, and the remaining one (L2) is non-Lock-14,
non-blocking, in a now-fully-re-authored artefact. The V3 integration
introduced no new Lock-14 leak.

---

## §4 — Remaining leaks

**One residual NOTE; no Lock-14 fault; non-blocking.**

**NOTE-1 (V2 leak L2, still open).** P3-D §2.2's field-table cell for
`grammar_id` reads "`json` (W0 rejects non-`json`)" with no scoping
sentence. V1 CH2 requested a one-line clause stating the schema is
JSON-bench-scoped by construction and that a future non-JSON bracket
re-parameterises `grammar_id` rather than treating `json` as the
universe. P3-D was re-authored for V3 (the `§0 V3 fold footer` names
four changes) but the `grammar_id` scoping clause was not among them, so
the cell is unchanged from V1/V2.

Why this is not a Lock-14 fault and not gating:

- `validate_sk_v8_w0` and the `grammar_id` hard-reject live in
  `bbnf-bench`. Lock 14's generic-crate list (`bbnf-parse`,
  `bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`, `path`, `path-core`,
  `egraph`, `csp-solver`, `parse-that-regex`, `parse-that`, `bbnf-simd`,
  `analysis`, `lsp`) does not contain `bbnf-bench`. A bench harness
  scoping its telemetry to `json` rows is bench hygiene, not substrate
  overfitting.
- The substrate itself carries no `grammar_id` branch. Lock 14 governs
  the substrate; no generic crate keys behaviour on `grammar_id`.
- The SPEC §0.y, which the dispatch binds to, lists `grammar_id` as one
  of 36 telemetry identifiers without claiming a substrate universe.

**Recommendation (non-blocking, for the SPEC-promotion edit, not a CH2
gate).** Fold the one-clause honesty addition into P3-D §2.2 field 2:
`grammar_id` is JSON-bench-scoped by construction; a future non-JSON
bench bracket re-parameterises it rather than treating `json` as the
universe. This is prose, touches no generic crate, and blocks no wave
dispatch.

No new Lock-14 leak was introduced by the V3 integration. Verification:

1. **The W4b three-way sub-division is Lock-14-clean.** The V3
   integration's largest structural change is splitting W4b into
   W4b-1/W4b-2/W4b-3. The split is LOC-driven (the ~1,045-net-LOC codec
   cannot land in one 75-min redress) and cut along P2-E §7.4 *slice
   seams* — the fixed-width/variable-width boundary tracks codec kernel
   shape, not grammar identity (§2 rows 2-3). Each new sub-wave gained
   its own §2.1 generality gate (rows 16-17). No sub-wave gained a
   grammar branch, a grammar-named generic module, or a grammar-specific
   generic public type.

2. **W4b-3 carries the CSS L4 / JS bindings grammar-neutrally.** The
   `codegen/src/escape_codec/` sub-module emits the five const-generic
   specialisations as data; the CSS L4 binding is a `#[cfg(test)]`
   compile-only scaffold; the `G-W4b-3-CODEC-BINDINGS` gate is a compile
   of the non-JSON bindings. This is the codegen-emitted-per-grammar-data
   exception Lock 14 explicitly permits — "Per-grammar deviations …
   encoded in the grammar metadata + source, NOT in branching code"
   (rows 4-6).

3. **The W3 HIGH-risk escalation introduced no grammar coupling.** The
   MEDIUM→HIGH escalation (SPEC §2.2, §6) is a *risk-class* change and a
   redress-cap accommodation (≤110 min). It added no `match grammar`
   arm, no grammar-named module, no public type. Clean.

4. **The P3-E lettered→numeric re-bind touched no Lock-14 content.** The
   P3-E §0 footer states "the per-wave pre-block content is unchanged —
   the material differentials and hard pre-blocks bind identically; only
   the wave labelling is reconciled". The Lock-14 hard pre-block (item 8)
   is verbatim (rows 24-25).

5. **No generic-crate symbol gained a JSON name.** SPEC §2.1's public
   API scan, §6#7, §7.1#6, §7.2.1#4, §7.2.2#8, §7.2.3#4 all restate the
   no-JSON-symbol-in-a-generic-crate falsifier; the V3 integration
   carried it into every behaviour wave's and sub-wave's exit gate and
   added no exception. The one named generic-crate primitive
   (`class_table`, verified live at `bbnf-simd/src/lib.rs:41`) is bound
   to the generated-data exception.

**Verdict carried to V3 consolidation:** CH2 V3 — **ACCEPT, qualifying
at 100% core ACCEPT-rate.** The V3-folded S-P3 cohort respects Lock 14:
generic crates stay grammar-neutral, per-grammar variation is
codegen-emitted data plus per-grammar wrapper/witness directories, the
per-wave Lock-14 gate is present in every behaviour wave and W4
sub-wave (and `rg`-audit-exercised at W2). The W4b three-way
sub-division is LOC-driven and keeps the codec grammar-neutral with
W4b-3 as the named grammar-neutrality-breadth sub-wave. One non-Lock-14,
non-blocking honesty-prose NOTE survives (L2 in P3-D §2.2's `grammar_id`
cell); it is a one-clause addition recommended for the SPEC-promotion
edit, gates nothing, and blocks no wave dispatch.
