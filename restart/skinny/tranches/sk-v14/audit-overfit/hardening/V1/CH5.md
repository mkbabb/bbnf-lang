# S-P0 CHALLENGE V1 — CH5 HIDDEN COUPLING

Lens: CH5 — HIDDEN COUPLING (per `restart/prompts/ORCHESTRATOR.md:87`).
Scope: the seven S-P0 artefacts committed at `d4cbc8204`
(`restart/skinny/tranches/sk-v14/audit-overfit/{SYNTHESIS-AUDIT-OVERFIT,
sk-v14-audit-overfit-{css-measurement, admit-mechanism, lock14-scan,
generator-truth, decision-engine, pre-restart-pattern}}.md`).
Charge: no parallel substrate; no sidecar producer; no renamed-scanner
Lock 1 violation; no Track 1 ≡ Track 2 dishonesty; substrate union
holds; no new BIR variant; no audit finding silently introduces parallel
substrate or sidecar producer.

UTC dispatch start: 2026-05-23T05:17:30Z. Hard cap: 30 min.

## §0 — Disposition summary

| Per-artefact | Verdict |
| --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT |
| A1 `…css-measurement.md` | ACCEPT |
| A2 `…admit-mechanism.md` | ACCEPT |
| A3 `…lock14-scan.md` | ACCEPT |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE (one cross-track coupling escalated as observation; no REVISE) |
| A5 `…decision-engine.md` | ACCEPT |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE (one new substrate-doc-of-record vector verified; no REVISE) |

ACCEPT-rate: **7 / 7 = 100 %** (two ACCEPT carry CH5 observation
notes — neither is REJECT, REVISE, nor REWORK; the observations bind
into the SYNTHESIS §3.1 C-1 + C-3 mapping that already covers them).

Zero CH5 firings against substrate union, parallel substrate
introduction, renamed-scanner Lock 1 violation, or Track 1 ≡ Track 2
collapse. The audit campaign itself surfaces these vectors as findings
(A4 NEW-1 / NEW-2; A6 NEW-HIGH-1 / NEW-HIGH-2) rather than committing
them — i.e. the audit is correctly framed as *exposure* of hidden
coupling, not *introduction* of it.

## §1 — CH5 vectors enumerated (charge breakdown)

The CH5 rubric per `ORCHESTRATOR.md:87` plus §3 dispatch focus
decomposes to seven sub-vectors. Each is scanned against the seven
artefacts.

| # | CH5 sub-vector | Scan result | Firing? |
| --- | --- | --- | --- |
| 1 | New BIR variant introduced by an audit finding | `git grep -nc 'enum.*Ir\|enum BackendShape\|new BIR variant' restart/skinny/tranches/sk-v14/audit-overfit/` returns zero hits across all seven artefacts; no proposed remediation adds an IR variant. | NO |
| 2 | Parallel substrate proposed under cover of remediation | A4 §4 R4 (regen-css xtask) reads `grammar/css/l4/*.bbnf` through the existing IR + sink-only pipeline (`crates/codegen/src/lower/sink_only.rs` via `lib.rs:215-217`). No new tape, no new arena, no new struct-builder. SYNTHESIS §2.1 confirms R4 routes through the same lowering that JSON consumes. A5 §4 R3 (W8 / W9 wiring) explicitly forbids public substrate API per SYNTHESIS §4 "same-tape, codegen-private, row-consumed shapes". A6 §4 PRUNE-4 names the existing `builder_template.rs` + `arena_template.rs` substrate as the union target — not a parallel one. | NO |
| 3 | Sidecar producer (a second codegen path that competes with the canonical one) | The audit indicts the EXISTING sidecar (8 `<g>_provider.rs` modules that `include_str!` hand-written templates and emit them under a fake `@generated` header) — `crates/codegen/src/lib.rs:166-209` shows the per-grammar `RuntimeProvider::*` arm dispatch. A4 finding 9 + A6 CRITICAL-3 + A3 enumerate this as the violation; PRUNE-3 (SYNTHESIS §3.3) collapses the arm dispatch onto a single grammar-driven dispatcher. No proposed remediation introduces a NEW sidecar; the audit collapses the existing one. | NO |
| 4 | Renamed-scanner Lock 1 violation | A6 §1 verified clean: `grep -rn 'fn parse_combinator\|combinator_fallback\|parse_with_fallback' crates/core/src/runtime/ skinny/crates/runtime/src/` returns zero matches; renamed pre-restart scanners CLEAN per A6 §2 line 163. A4 NEW-2 cited "scanner" in `nested_layout` etc. — but those are NOT renamed-scanner Lock 1 fan-outs; they are fixture-lookup tables (`if input == CANONICAL_FIXTURE { return CANONICAL_FACTS }`) that BYPASS scanning entirely, and PRUNE-2 deletes them rather than renaming. | NO |
| 5 | Track 1 ≡ Track 2 collapse vector via A4 NEW-1 (JSON `generated.rs` also fake-`@generated`) | A4 finding 10 inspects the JSON `generated.rs` round-trip and finds it is a *mixed-source* file: template body lines 1-N from `include_str!("json_templates/generated.rs")` + trailing `json_sink_direct::render(sink_only)` chunk that IS grammar-derived (`crates/codegen/src/lib.rs:215-217` quoted verbatim). A4 cites this as PARTIAL round-trip, not full. The remediation (A4 §4 R4 + PRUNE-2 entry) extends R4's scope to validate the JSON template body emits from `skinny/grammars/json.bbnf` end-to-end ONLY conditionally, and only after R4 lands first. Track 1 (skinny codegen pipeline) and Track 2 (totality `xtask/src/regen.rs` reading `Cargo.toml:22`) remain separately scoped per A4 finding 16 LOW — explicit "noted to forestall a future redress that conflates the two regen pipelines under one xtask". Coupling vector identified and isolated; no collapse. | NO (observation noted §3.1) |
| 6 | Substrate union violation — Lock 1 (the substrate union must hold; no parallel substrate union introduced) | A5 finding 2 cites the AUTHENTIC verdict on `same_substrate_union`: no `UnionTape` variant in `skinny/crates/runtime/src/tape/`; hardcoded `token_union_projection(kind, depth) -> &'static str` returns fixed constants. A5 §4 R3 explicitly binds W9 to "same-tape, codegen-private, row-consumed shapes — NOT a public substrate API". The remediation strengthens the union (single tape, codegen-private union variant) rather than splitting it. SYNTHESIS §3 C-4 maps to PRUNE-5 with this same scope. A6 NEW-HIGH-2 confirms the `arena_template.rs` + `builder_template.rs` doc-comment opt-out is design-of-record for the hot grammars — the substrate union is INCOMPLETE (5 cohort grammars on template, 4 hot grammars hand-written), and the proposed PRUNE-4 brings the hot grammars onto the same union substrate. | NO (substrate union INCOMPLETE per audit; PRUNE-4 + C-1 close the gap; no proposal SPLITS it) |
| 7 | A5 PARTIAL PASS concealing hidden coupling | A5 §3 verdict reads PASS for resolver clause / FAIL→PASS-at-baseline for scaffold-clause. Inspected `passes/src/lib.rs:476-478` quoted verbatim at A5 §1.3: `let candidates = backend_candidates(...); let active = backend_egraph::select(...); decision_csp::finalize_rule(...)` — three-stage pipeline, single output flowing through `recognizers::derive_backend_shape_with_diagnostics()` → `layout_facts.cost_facts` → `codegen::lower::rust::lower_to_rust()`. Five fail-closed checks at `codegen/src/lower/rust.rs:37-89` (quoted A5 §1.4) panic on missing or inconsistent CSP facts. No silent secondary path, no fallback, no shadow resolver. The PARTIAL PASS verdict captures the W8 / W9 SCAFFOLD-ONLY honest delta (gate-layer footprint quantified at A5 finding 3 = 3 files / 20 references in `bbnf-bench/`) without paper-closing the runtime-consumption gap. CH5 finds no concealed coupling under the PARTIAL PASS verdict. | NO |

All seven CH5 sub-vectors clear. No firing across the audit campaign.

## §2 — Per-artefact disposition table

| Artefact | CH5 disposition | Rationale |
| --- | --- | --- |
| `SYNTHESIS-AUDIT-OVERFIT.md` | ACCEPT | §2.1 R4 → PRUNE-2 + §2.2 C-1 → C-4 sequencing constraints are surfaced explicitly (not hidden); §2.4 CH7 gating extensions explicitly forbid the JSON template body becoming a parallel CSS codegen path without round-trip verification; §3.1 prune-list mapping accounts for all 74 findings with zero orphans and no new C-6+ candidate added that would smuggle in a parallel substrate. The Pattern H 64 → 67 file count delta (§1.3) cleanly attributes the +3 to `css_pretty` (which IS the union-substrate candidate per the cohort doc at `builder_template.rs:10`), so the growth is on the SUBSTRATE-UNION side, not the per-grammar opt-out side. The synthesis correctly identifies that the substrate union is INCOMPLETE — its closure is what C-1 + C-4 enact, not its bifurcation. |
| A1 `…css-measurement.md` | ACCEPT | The axis touches comparator wiring + corpus + plausibility; zero substrate, zero IR, zero codegen-path proposals. The R1 / R2 recommendations (comparator rebind + per-iter oracle) consume the existing `json_parity.rs` lane structure — no new bench harness substrate. Reviewed lines 17-26 + 87-102 of `json_parity.rs` cited in A1 §1.6 are within the existing single-lane harness; nothing forks. |
| A2 `…admit-mechanism.md` | ACCEPT | F8 (single-lane fan-out structural cause) and F9 (negative-drift) confirm the existing comparator binding is single-substrate; the proposed remediation TIGHTENS the binding (per-plane strict anchors at `real_typed_struct.rs:695-727`) onto the same lane rather than splitting it. The 21-hit grep enumeration (§2.3) is read-only attestation, not a coupling vector. |
| A3 `…lock14-scan.md` | ACCEPT | Verbatim v3 reproduction; D1 DELTA-NOTE on `StringFlags::HAS_ESC` JSON-flavored naming in `parse-that-regex/src/lib.rs:56-60` is a FUTURE-rename concern explicitly classified as NOT a violation (lines populated by any quoted-string grammar; bit semantics grammar-neutral). The 11 CRITICAL / 7 HIGH / 5 MED / 7 LOW Lock-14 enumeration is *exposure* of existing per-grammar identifiers in generic crates, not introduction. PRUNE-3 (C-1) collapses these onto a single dispatcher — substrate union strengthening. |
| A4 `…generator-truth.md` | ACCEPT-with-COUPLING-NOTE | The NEW-1 finding (JSON `generated.rs` ALSO fake-`@generated`) is the central CH5 audit target per §3 dispatch focus. **Verified**: `head -1 skinny/crates/runtime/src/grammars/json/generated.rs` = `// @generated by skinny bbnf-codegen; do not edit by hand.` and `head -1 skinny/crates/codegen/src/json_templates/generated.rs` = the identical line, confirming the template body carries the header literally (not added by `normalize()` for the `generated_rs()` path — that function does `include_str!(…).to_string()` per `json_provider.rs:62-64`, NO `normalize` call). The `json_sink_direct::render(sink_only)` chunk IS grammar-derived per `crates/codegen/src/lib.rs:215-217` (`generated.push_str(&json_sink_direct::render(sink_only).map_err(CodegenError::Lowering)?)`). A4 finding 10 correctly captures this as PARTIAL round-trip with the ~85 % / ~15 % hand-written / grammar-derived split. **CH5 observation**: A4's discovery extends the fake-`@generated` scope from CSS-only to CSS + JSON template body, but does NOT introduce a Track 1 ≡ Track 2 collapse vector — A4 finding 16 LOW explicitly preserves the Track 1 (skinny codegen) ↔ Track 2 (totality root `xtask`) separation by quoting `Cargo.toml:22` against `xtask::regen::run` against `crates/core/src/grammar/generated/`. The proposed R4 routes through the skinny pipeline only; track separation holds. ACCEPT. |
| A5 `…decision-engine.md` | ACCEPT | Resolver clause verified end-to-end (§1.3 + §1.4 quoted verbatim against `passes/src/lib.rs:476-478` and `codegen/src/lower/rust.rs:37-89`); five fail-closed checks panic the lowering on missing CSP facts. NEW-MED gate-layer-only footprint (3 files / 20 references for `per_grammar_policy` / `same_substrate_union` / `GrammarConfig`) is QUANTIFICATION of the SCAFFOLD verdict, not a coupling firing. The PARTIAL PASS verdict (per §3 dispatch focus) does NOT conceal hidden coupling — the resolver's own self-labelling at `decision_csp.rs:160-164` carries the `JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT` block-ID chain that *gate-rejects* any future row-admit citing W8 / W9 until the runtime consumer lands (A5 §4.2). The resolver-to-runtime gap is honestly surfaced; the resolver-to-lowering wire is correct; no shadow path. |
| A6 `…pre-restart-pattern.md` | ACCEPT-with-COUPLING-NOTE | NEW-HIGH-1 (`LegacyPath` rename shim) verified across 4 `parse_with.rs` files via `grep -n 'LegacyPath\|LegacySegment' crates/core/src/runtime/{json,css_l4,bbnf,google_sheets}/parse_with.rs`: 28 hits across 4 files, identical pattern at `use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment}` + `lower(&TypedSegment<'a>) -> Option<LegacySegment<'a>>` + `LegacyPath::new(&legacy)` in `doc.get::<T>(…)`. This IS a Lock-1-adjacent pattern per §3 dispatch focus — two co-existing path representations (`Path` / `PathSegment` and `TypedSegment`) with one explicitly labeled `Legacy` at the use site. **CH5 disposition**: the shim is a coupling between an older path representation and a newer typed one; the remediation (A6 §4 row 4: "fold into PRUNE-4 or open a small C-6 typed-path collapse") proposes COLLAPSE of the dual representation, not bifurcation. SYNTHESIS §3.1 maps this to C-1's 41-finding cluster + sub-task note. The shim's scope is the 4 `parse_with.rs` files only (verified) — properly bounded, no leak into generic crates (`grep` for `LegacyPath` in `ir/`, `codegen/`, `passes/` returns zero matches per A6 §1 indirect-clean). NEW-HIGH-2 (substrate-doc opt-out enshrinement at `builder_template.rs:1-40` quoted) is confirmed by direct read: the substrate doc explicitly lists JSON, CSS L4, BBNF as "Distinct shape → distinct module (no template instantiation)" — Pattern H as design-of-record. PRUNE-4 (per A6 §4 row 1) closes this either by instantiating the hot grammars onto a richer template OR by rewriting the substrate doc with a deletion plan. Substrate union is INCOMPLETE; the remediation closes the gap. ACCEPT. |

## §3 — Critical CH5 findings

### §3.1 A4 NEW-1 — Track 1 ≡ Track 2 collapse vector under R4 scope-extension

**Finding (observation, not REJECT)**: A4 §4 R4 row says
"validate the codegen pipeline can emit the JSON template body from
`skinny/grammars/json.bbnf` end-to-end" conditionally on R4 landing
first. This is the only audit recommendation that could be misread as
proposing a Track 1 ≡ Track 2 collapse — i.e. a single xtask whose
`regen-*` subcommands span both the skinny per-grammar providers
(`skinny/crates/codegen/src/`) AND the totality root grammar generation
path (`xtask::regen::run` reading `crates/core/src/grammar/generated/`
per `Cargo.toml:6-29`).

**Disposition**: ACCEPT-with-observation. The recommendation is
*conditional* ("if R4 (real CSS regen) lands first and validates the
codegen pipeline can emit the JSON template body … end-to-end"), and
A4 finding 16 LOW explicitly preserves track separation: "Noted to
forestall a future redress that conflates the two regen pipelines
under one `xtask`." The S-P3 wave manifest must (per CH5 binding)
encode the R4 specification such that Track 1's `regen-css-l4-<provider>`
subcommands attach to `skinny/xtask/src/main.rs:8` ONLY — never to
`xtask/src/main.rs` (root). The two regen entry points stay disjoint;
the unification happens IN the IR / sink-only pipeline they share
(`crates/codegen/src/lower/sink_only.rs`), not in the xtask surface.

Already covered by A4 finding 16 + SYNTHESIS §2.4 CH7-companion
recommendation. **No REVISE required**; the CH5 enforcement note binds
into S-P3 implicitly via §3 dispatch focus, and the audit text already
contains the disjunctive guard. Status: forward observation only.

### §3.2 A6 NEW-HIGH-1 — LegacyPath rename shim scope verification

**Finding**: The 4-file `LegacyPath` / `LegacySegment` alias shim is
Lock-1-adjacent (two co-existing path representations bridged at the
`use` site).

**Verification**: scope confirmed bounded to 4 `parse_with.rs` files
inside `crates/core/src/runtime/{json, css_l4, bbnf, google_sheets}/`.
The 28-hit grep enumeration is **identical pattern** across all four —
this is mechanical replication, not divergent per-grammar coupling.
Generic crates (`ir/`, `codegen/`, `passes/`, `bbnf-regex/`,
`bbnf-simd/`, `parse-that-regex/`) carry zero `LegacyPath` references
per A6 §1 indirect-clean.

**Disposition**: ACCEPT. The shim is a Lock-1-adjacent coupling
between `crates/core/src/runtime/path.rs::{Path, PathSegment}` (older)
and the typed `TypedSegment<'a>` (newer); the remediation per A6 §4
row 4 + SYNTHESIS §3.3 PRUNE-4 sub-task is COLLAPSE onto the typed
representation, not parallel substrate. No CH5 firing; the shim is
audit-exposed and remediation-scheduled.

### §3.3 A6 NEW-HIGH-2 — Substrate-doc enshrinement of opt-out

**Finding**: `builder_template.rs:13-31` + `arena_template.rs:1-31`
documents that JSON, CSS L4, BBNF "Distinct shape → distinct module
(no template instantiation)" as DESIGN-of-record.

**Verification**: direct read of `crates/core/src/runtime/
builder_template.rs:1-40` confirms verbatim — the doc enumerates the
cohort grammars (BNF, EBNF, CSV, CSS Pretty, Math) explicitly, names
the outliers (JSON, CSS L4, BBNF, Sheets) explicitly, and frames the
Pattern H opt-out as architectural intent.

**Disposition**: ACCEPT. This is the CH5 vector at its rawest: the
substrate UNION is INCOMPLETE by design (5 grammars on the union, 4
grammars hand-written). The audit correctly indicts this — A6 §4 row 5
proposes EITHER instantiating the hot grammars onto a richer template
OR rewriting the substrate doc with a deletion plan. The substrate
union must hold (per `ORCHESTRATOR.md §3W CH5` non-negotiable line
203); the audit finding LANDS the obligation to close the union, not
to split it. No CH5 firing. The remediation is correctly scoped to
PRUNE-4 + C-1 per SYNTHESIS §3.1.

### §3.4 A4 NEW-2 — Fixture-lookup scanners as substrate hidden coupling?

**Finding**: 3 of 7 CSS L4 template generators short-circuit on
`CANONICAL_FIXTURE` / `CAPTURED_W2_INPUT` byte-equality and return a
precomputed `CANONICAL_FACTS` blob (`css_l4_nested_layout_templates/
generated.rs` = 49 lines total, hash-table lookup; same in
`at_rules_and_media`, `stylesheet_selectors`, `vendor_and_custom_atrules`).

**CH5 verification**: these are NOT renamed scanners (no Lock 1
violation per §1 sub-vector 4), and they are NOT a parallel substrate
(no `Tape` / `Arena` / `Builder` introduced) — they are *short-circuit
constant tables* dressed as parsers. The "coupling" they expose is
between the test corpus (the canonical fixture bytes) and the W10.* /
W2 ADMITTED rows' "parity" claims. A4 §4 PRUNE-2 deletes these
wholesale; no proposed remediation preserves the lookup. The vector
is correctly indicted, not preserved.

**Disposition**: ACCEPT. No CH5 firing.

## §4 — V2 fold recommendations

The audit campaign at V1 closes with 100 % ACCEPT under the CH5 lens.
Two binding notes for V2 dispatch fold:

1. **R4 specification language must encode the §3.1 Track 1 / Track 2
   separation as a hard constraint.** S-P3 wave manifest text for R4
   should read approximately: "`regen-css-l4-<provider>` subcommands
   land on `skinny/xtask/src/main.rs:8` USAGE line only; the totality
   root `xtask/src/main.rs:1-65` is untouched by R4; the shared
   substrate is the IR + sink-only lowering pipeline
   (`crates/codegen/src/lower/sink_only.rs`), not the xtask surface."
   This formalises A4 finding 16 LOW.

2. **PRUNE-4 must explicitly state the substrate-union closure
   target.** Per §3.3, the substrate union is INCOMPLETE by design at
   SK-V14 starting state (5 cohort + 4 hot grammars). PRUNE-4's
   specification should declare which closure path it takes — (a)
   richer template that subsumes JSON / CSS L4 / BBNF / Sheets, or (b)
   substrate-doc rewrite with deletion plan — and bind the deletion of
   `arena_template.rs:1-31` + `builder_template.rs:13-31` opt-out
   passages into the PRUNE-4 wave-close gates. Without this declaration
   the union remains design-of-bifurcated even after PRUNE-4 lands.

Neither note is a REVISE; both are forward-binding clarifications that
strengthen the C-1 + C-3 mappings already enumerated in SYNTHESIS
§3.1. The CH5 lens advances without convergence blocker.

## §5 — Verdict

**CH5 — HIDDEN COUPLING: ACCEPT (7 / 7 = 100 %).**

The S-P0 audit campaign at SK-V14 V1 holds the CH5 lens without
firing. The audit correctly *exposes* hidden-coupling vectors as
findings (A4 NEW-1 JSON `generated.rs` fake-`@generated`; A4 NEW-2
fixture-lookup scanners; A6 NEW-HIGH-1 `LegacyPath` rename shim; A6
NEW-HIGH-2 substrate-doc opt-out enshrinement) rather than committing
them as remediation. Every proposed prune action (PRUNE-1..5 + R1, R2,
R4) collapses dispatch, strengthens the substrate union, or wires
SCAFFOLD to LOAD-BEARING — never bifurcates substrate, never adds a
sidecar producer, never introduces a new BIR variant, never renames a
pre-restart scanner, never collapses Track 1 ≡ Track 2.

The two ACCEPT-with-COUPLING-NOTE dispositions (A4 §3.1 + A6 §3.2 /
§3.3) carry forward observations that bind S-P3 R4 specification
language and PRUNE-4 substrate-union closure declaration. Neither
observation is a REVISE; both are V2 fold guidance enumerated in §4.

Per `ORCHESTRATOR.md §3Z`, the CH5 lens convergence criterion is met
at V1 (100 % ACCEPT, zero open critical defects, zero orphan unresolved
REVISE). Cycle V2 may dispatch on remaining lens dispositions; CH5
holds at this cycle and forward unless a new audit finding surfaces a
parallel-substrate / sidecar-producer / scanner-rename / track-collapse
vector under V2 evidence.
