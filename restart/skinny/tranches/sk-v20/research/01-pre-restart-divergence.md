# 01 — Pre-Restart Divergence Catalogue

**Scope.** Lettered tranches AA–B7 under `docs/tranches/`; the
`docs/tranches/meta-audit/` corpus; `docs/tranches/archive/pre-restart-BA/`;
and git history predating the `restart/` pivot. Thirteen divergence patterns,
five root causes named. This is the negative template the totality tranche is
audited against.

---

## Divergence exemplars

### D-1. Grammar-name literals in the generic codegen crate

Commit `53ab3ab19` (2026-03-08) added `CSS_WS_COMMENT_REGEX_PATTERNS`,
`CSS_IDENT_REGEX_PATTERNS`, `CSS_STRING_REGEX_PATTERNS` directly inside
`rust/bbnf/src/generate/patterns.rs` — the grammar-neutral codegen library —
extending a pre-existing JSON peer set on the same code path. Both sets mapped
regex pattern literals to monolithic `SpanParser` constructors
(`sp_css_ident()`, `sp_json_string_quoted()`) by string comparison inside
`try_generate_span_expr` (`rust/bbnf-derive/src/span_codegen.rs:138`).

The skinny rewrite replicated the pattern: `skinny/crates/codegen/src/lib.rs:4-5`
declared grammar-named modules inside the generic `codegen` crate, and
`runtime_generator.rs:701` housed `const CSS_GENERATED_RS: &str = r#"…"#` — a
verbatim hand-written CSS blob inside a grammar-neutral emitter
(`restart/audit/totality/p1/1F-anti-pattern.md:62-64`). SK-V18 G1/G2/G3 retired
these: `4ff1b806d`, `ed538afd7`; `b5ab2e17c` deleted `RuntimeEmitterKind` and
replaced grammar-tag dispatch with `LoweredArtifact` sum-type dispatch keyed on
lowered artifact shape, not grammar name.

**Failure mechanism (A).** Performance shortcuts for a specific grammar, introduced
without a "zero grammar names in generic source" gate, were adopted as convention.
Prevention: Lock-14 firewall + re-injection falsifier.

---

### D-2. Combinator fallback path preserved alongside monolithic path

The W–Z / AA–AC era carried two complete codegen paths simultaneously: the
"monolithic" arena path and the "combinator" SpanParser path. Commit `044c1a9ad`
(2026-03-31) deleted ~2,370 LOC across `alt.rs`, `seq.rs`, `repeat.rs`, `wrap.rs`,
`inline.rs`, `ir_span.rs` — multiple tranches after the monolithic path was
introduced. Both paths existed simultaneously and accumulated divergence.

**Failure mechanism (B).** "Keep the old path as a fallback" inertia. Every new IR
node needed two emission implementations; the combinator path's CSS-specific hooks
(D-1) were kept alive because they had no monolithic analog yet. Prevention: every
wave that introduces a new code path must delete the old one at the same commit.

---

### D-3. Hand-patched `generated.rs` across 7+ tranches

`generated.rs` (the self-hosted BBNF grammar parser) was hand-patched rather than
regenerated from source. `docs/tranches/AQ/AQ.md:43`: "`generated.rs` has been
hand-patched across AE/AF/AG/AI/AM/AN/AP. Every tranche defers it." By tranche AU
the file diverged by `+770/-479` lines. `docs/tranches/AU/AU.md` §Deferred items:
"ParsedGrammar elimination | AR.7.2 | **11 tranches**." `docs/tranches/AR/audit-prior.md:221`
records `// that rewrites it is deferred to AG.4b` — AG landed 2026-03, still
deferred at AQ close. Bootstrap regen at AR produced 78,307-line `generated.rs`
failing six `grammar_roundtrip` tests.

**Failure mechanism (C).** Structural mismatches between host extractor and tape shape
blocked regen, so tranches injected targeted patches. Each new feature added another
patch. The `clean-regen-discipline` memory names the invariant: generated files are
always output of fresh regen; never hand-patch.

---

### D-4. Substrate-first, consumer-later: DTA / PSI interpreter (~400 commits)

From AV through AW-V (~400 commits) the codebase built a table-driven interpreter
(DTA + PSI pipeline) without a single production grammar running through the full
path at any wave close. AW-III gate: "strict-better-than post-AU on ≥15/19 entries."
Result: 0/19. AW-IV gate: "every entry exceeds post-AU." Result: 0/17
(`docs/tranches/AW/FINAL-IV.md:80-103`):

> json twitter 288 vs AU 1967 — 14.6% — ✗; css normalize 25 vs AU 735 — 3.4% — ✗

AX.W0b deleted the entire interpreter for ~78,500 LOC
(`docs/tranches/AX/AX.md:12`). The AW-IV honest post-mortem names three explicit
substrate-without-consumer survivors (`docs/tranches/AW/FINAL-IV.md:110-113`):
ShapeRef consumer deferred, CTNS lifter gated-off, bounded-Regex soundness deferred.

**Failure mechanism (C).** AX invariant 13 was written retrospectively: "Ledger-only
wave = re-plan trigger. If wave-close cannot cite runtime evidence (samply self-time
≥ 1%, `nm` symbol absence, wire-contract end-to-end) for every substrate landing,
the wave reopens."

---

### D-5. Two orthogonal axes for one semantic: EmissionTier vs PayloadKind

Tranches AF through AQ carried `EmissionTier` (`MustTape`/`MustFn`/`MayInline`)
governing emission form and `PayloadKind` governing return type — both capturing
the same semantic. AQ.5 deleted both and replaced with `TypeDesc`: the type IS the
tier. `docs/tranches/AQ/AQ.md:61`: "PayloadKind::F64/Bool/U8 is redundant with
TypeDesc::F64/Bool/U8." Cost: ~32 commits across AF (26) + AG (6)
(`docs/tranches/meta-audit/06-commit-archaeology.md:146`).

**Failure mechanism (D).** Two IR variables introduced independently; coercion logic
proliferated at every emission site when they disagreed. Prevention: if two quantities
are always derived from the same source fact, they must be the same variable.

---

### D-6. Optimizer substrate shipped as architectural theatre

Tranches U–W claimed a CSP-based optimizer and e-graph cost-sharing substrate.
Tranche W opening audit found: `crates/ir/src/passes/csp_recognizers.rs` had zero
references to `csp_solver::` (it was a pattern-match cascade misnamed "csp");
eight `compile_error!` stub bodies in `backend/kernels/`; `CostWeights` shared by
type only (both tiers called `::default()` independently); `OptimizationMode::MinimizeCost`
invoked zero times anywhere. The AW-IV.W5.3 cost-grid sweep ran 648 measurements
across 54 configurations — result: "The current e-graph rewrite set produces
invariant DTA state counts across all 54 configurations — `CALIBRATED_WEIGHTS` is
identical to `CostWeights::default()`." (`docs/tranches/AW/FINAL-IV.md:63`). The
chronic item AM.6 "cost-model calibration" was deferred through five tranches
before the null-result closed it.

**Failure mechanism (C/E).** Claims of substrate activation written as architectural
assertions without a runtime gate requiring the optimizer to change an output. A
substrate that never changes its consumer's output is dead code.

---

### D-7. Per-grammar match arms in the generic IR strategy crate

Before the manifest-driven refactor (AZ-IV.W1.8), `crates/ir/src/registry/strategy.rs`
carried a nine-arm match expression listing every production grammar by literal
parser name: `"JsonParser"`, `"GoogleSheetsParser"`, `"CssL4Parser"`, and six others.
The comment at `crates/ir/src/registry/strategy.rs:156` (current) records: "Pre-W1.8
this method shipped a 9-arm match expression listing every production grammar by
literal parser-name." `restart/audit/totality/p1/1F-anti-pattern.md:65` confirmed
this as a Lock-14 violation.

**Failure mechanism (A).** "Add a grammar, add an arm" inertia. Because the match was
inside a strategy resolver function, it felt like "configuration" rather than
hardcoding — but grammar names as Rust string literals in a generic crate's source
file are the definition of the Lock-14 violation. Prevention: workspace-metadata
lookup, never source-side arm.

---

### D-8. Seven byte-identical CSS generated files (replica overfit)

The skinny proving tree accumulated seven `css_l4_*/generated.rs` files byte-for-byte
identical (md5 `b654562ccff46ed62dd48e9ace325830` across all seven)
(`restart/audit/totality/p1/1F-anti-pattern.md:45`). SK-V18 P3 assigned to collapse
to ONE CSS configuration (estimated −5,460 LOC, `restart/MIGRATION.md:60`).

**Failure mechanism (A).** Grammar-section decomposition without codegen unification.
Each section produced its own generated runtime. Without an automated grammar-neutral
generator, the only way to produce seven outputs was to copy one template seven times.
Copies drifted; divergence forced a fresh copy in a cycle.

---

### D-9. Structural dispatch infrastructure built and deleted without activation

Tranche AO built a structural pre-scan dispatch infrastructure (IR pass, codegen
emission hooks, cursor changes) never activated in any production grammar. Two AP
activation attempts regressed benchmarks and were rolled back
(`docs/tranches/AR/audit-prior.md:28-29`). AQ.5 deleted the entire infrastructure.
AQ.md §3: "Structural pre-scan is no longer viable — DELETE, don't activate. Honest
math: pre-scan cost ~300µs; max savings from WS elision 110µs. Net: -190µs
regression." AO verdict: "RUBBER-STAMPED. Five of six phases were plan-only."
(`docs/tranches/AR/audit-prior.md:79`).

**Failure mechanism (C/E).** Infrastructure built without requiring the path to be
active at any wave close. When the cost model changed (AP.3.1's SIMD bitmap), the
infrastructure was not re-evaluated. It sat dormant until honest arithmetic declared
it net-negative.

---

### D-10. Deferral chains spanning 5+ consecutive tranches

AN deferred AN.2/3/5/6 to AO; AO deferred Phases 1–5 to AP; AP deferred six of nine
items to AQ. `docs/tranches/AR/audit-prior.md:108-114`: "AN.2 scanner generalization
NOT LANDED (deferred to AO → AP → AQ.4)." At scale: AV deferred V6–V9 to AW; AW-III
to AW-IV; AW-IV to AW-V; AW-V to AX; AX deferred Blocks B–F to AY. A single item —
ParsedGrammar elimination — was deferred eleven consecutive tranches
(`docs/tranches/AU/AU.md:88`).

**Failure mechanism (E).** Plans modelled work as a portfolio of parallel substrate
waves without requiring a runtime consumer at each wave close. AX invariant 17 names
the fix: "'Architectural transposition complete; throughput in next wave' is not a
closeable wave."

---

### D-11. Public stub emitters returning empty output for 10+ tranches

`schema/emit/ts.rs` returned `String::new()` and `schema/emit/runtime.rs` returned
`Vec::new()`. Both were public API with callers, silently producing empty output with
no error. AU.4.4: "have been stubs for 10+ tranches."
(`docs/tranches/AR/audit-prior.md:222-224`). No issue was open against them in any
plan between introduction and AU.

**Failure mechanism (B/C).** Stubs introduced as aspirational scaffolding; when the
feature was deprioritized, stubs were not deleted. Each subsequent tranche ignored
them (no caller, no test, no pain). Memory `no-value-discard`: "Codegen must never
discard computed values."

---

### D-12. Projection codegen computing and discarding typed values

By AU the codegen fingerprint showed 206 `.map(|_| ())` discards in CSS L4 and 9 in
JSON (`docs/tranches/AU/AU.md:132-135`). Every CSS number scan computed f64 via
Eisel-Lemire and discarded it. All 20 emitted `push_compound` with no payload despite
`-> f64` annotations. AU §Critical failure 4: "`parse_hex_color` function is never
called." AU §Critical failure 3: "Despite the architecture supporting fused scanners,
the expanded CSS L4 parser uses NONE of them." The `branch_pushes_children()` catch-all
`_ => true` at `crates/core/src/backend/driver/alt.rs:67-69` silently classified every
Alt branch as compound.

**Failure mechanism (C).** Typed projection infrastructure existed but wiring from
grammar annotation to emitter branch was incomplete. Error was undetectable without a
correctness gate comparing typed output against an external oracle.

---

### D-13. Columnar tape split that required reversion

Tranche Y introduced a 7-column tape split. AY-I.W1 reverted to a single `Vec<TapeRec>`
plus parallel `sib_skip`, recovering bench throughput: twitter recovered from 688 MB/s
toward the AU baseline 1,967 MB/s
(`docs/tranches/meta-audit/06-commit-archaeology.md:156-157`). Every bench regression
between Tranche Y and AY-I.W1 carried the column-split overhead.

**Failure mechanism (C/E).** The columnar split was introduced without a bench gate.
The tranche shipped the split and planned to show speedup later. The speedup never
materialized; alignment overhead outweighed the benefit.

---

## Failure mechanism taxonomy

**A — Grammar-name leaks via incremental special-casing.** D-1, D-7, D-8. Each leak
began as a performance shortcut for a specific grammar; no structural gate prevented
recurrence. Prevention: Lock-14 firewall + re-injection test.

**B — Parallel paths kept "for safety" instead of deleted.** D-2, D-11. New path lands;
old path preserved as hedge; debt accumulates. Prevention: every wave that introduces
a new code path must delete the old one at the same commit boundary.

**C — Substrate-first without consumer at wave close.** D-3, D-4, D-6, D-9, D-12, D-13.
Infrastructure shipped without a production callsite exercising it at the wave-close
commit. Prevention: every substrate layer must have a runtime callsite proved green at
closing commit. "Ledger-only wave = re-plan trigger" (AX invariant 13).

**D — Two-axis design where one axis is derivable.** D-5. Two independent IR variables
capture the same semantic; coercion logic proliferates when they must agree. Prevention:
merge before shipping.

**E — Deferral chains with no hard runtime gate.** D-10. Plans model work as a portfolio
without requiring a consumer at each wave close. Prevention: every wave must demonstrate
its thesis on a real grammar with samply / `nm` / wire-contract evidence at close commit.

---

## Totality audit checklist

1. Any grammar-name string literal in a generic crate? Mechanism A. (D-1, D-7, D-8)
2. Superseded code path still present without deletion? Mechanism B. (D-2, D-11)
3. Substrate introduced this wave with zero production callsites at close? Mechanism C.
   (D-3, D-4, D-6, D-9, D-12, D-13)
4. Two IR variables that must always agree and one is derivable? Merge. Mechanism D. (D-5)
5. Deliverable marked "deferred to next tranche" without a hard runtime gate? Mechanism E.
   (D-10)
6. Bootstrap regen run and output committed? Stale `generated.rs` by any line count
   blocks close. (D-3)
7. Stub functions returning empty output surviving more than one tranche? Delete. (D-11)
8. Bench matrix includes a cold-only same-run external comparator row? Missing one
   makes throughput claims unverifiable. (D-4, D-6)
