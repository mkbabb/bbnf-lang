# SK Arc Ledger: Tranche-by-Tranche Honest Audit (SK-V3.5 through SK-V19)

Date: 2026-06-10. Auditor assignment 03-sk-arc-ledger. Read-only static audit; no cargo
invocations; evidence from git log, research/*.md, SYNTHESIS.md, HANDOFF.md, REDRESS.md.

---

## SK-V3.5 (2026-05-13) — Pre-flight Autopsy

SK-V3.5 was not an implementation tranche; it was a six-agent forensic pass over the prior PSI/DTA
failure. It produced five companion documents: a git-history census, an archive deep-read, a
five-mode failure anatomy, a diff audit against PSI's architecture, and an FSM/DPDA correctness
verdict. The synthesis (`sk-v3.5/research/06-go-no-go-synthesis.md`) correctly identified the six
canonical failure shapes — Vec<OpenFrame>::clone at 86% self-time, three-representation type
ambivalence, substrate-first/consumer-later, abandoned columnar SoA, per-grammar god-modules, and
the new interpreter-dispatch ceiling — and rendered a GO verdict with one binding amendment: excise
`CollapsedStage` from the SK-V3 close criterion. Nothing committed to the skinny tree. The
amendment (DROP CollapsedStage as an SK-V3 deliverable; KEEP in taxonomy; defer to a successor
tranche behind Zen 4 silicon + differential parity harness) proved correct but was only partially
propagated: the IMPLEMENTATION-PACKET still had stale Wave 6 framing that future tranches had to
re-litigate. Deferred out: `CollapsedStage` DPDA/FSM dispatch, per-grammar-firing-matrix audit,
Wave 6 split into 6a/6b.

Grade: A — honest, adversarial, correctly sequenced. Entirely process/documentation. Zero code.

---

## SK-V5 (2026-05-13 through ~05-15) — First Implementation Wave

SK-V5 was the first substantive implementation tranche, dispatched to execute the architecture
declared in MASTER-PLAN H.W1+H.W4 that had been specified but never coded. It targeted
`BackendShape` / `derive_backend_shape` / `LayoutFacts.backend_shape`, the five-shape lowerer
hierarchy, and generated `SinkOnly` emission from BIR. Key pre-landing corrections: the synthesis
invalidated the Class A NEON wiring (`match_tiny_plain_string` kernel regressed twitter ~25% when
previously attempted), identified `validate_utf8_codepoint` at 25-40% self-time as the real
bottleneck, uncovered a bench-private `SinkParser` in `direct_struct.rs` that was measuring its own
private parser twice (Track 1 ≡ Track 2 dishonesty), and flagged that `bbnf-simd/src/lib.rs` at 716
LOC was a JSON god-module (`JSON_STRUCTURAL`, `is_json_punctuation`, `scan_json_tail`, etc.).
Commits landed: `BackendShape` Rust state, `LayoutFacts.backend_shape`, `derive_backend_shape`,
`codegen/src/lower/` hierarchy, vendored Eisel-Lemire, Canada SIMD scan floor restored, strictness
columns, simd-scan fossil and `eventcursor` purged (`cae7b48be`, `726ab124f`, et al.). Closed at
N-direct/NoGo with 13 parse G-rows remaining. The UTF-8 fusion prescription (Wave 3 as originally
scoped) was falsified across REDRESS 50-55. Deferred out: direct string fusion, UTF-8 codepoint
kernel, Lock 14 cleanup (46 HIGH leaks enumerated), CostFacts substrate.

Grade: B+ — delivered real infrastructure; honest about what failed; too much Wave 3 time on a
route that REDRESS then killed. Roughly 60% code, 40% process/rejection documentation.

---

## SK-V6 (2026-05-15) — Profiling Corrections and Discipline Codification

SK-V6 was a 12-agent research and correction pass. It introduced two material corrections: first,
the sonic-rs `utf8_lossy` feature in `bbnf-bench/Cargo.toml:21` invalidated every prior
strict-vs-strict claim (a one-line fix that the synthesis named as Wave 0); second, yyjson at 3,687
MiB/s twitter was identified as the actual M5 Max DOM-class leader, not simdjson, resetting the
SOTA-beat framing. The DAV1D/FFmpeg process discipline was formally codified as the Layer 0 / Layer
1 / Layer 2 primitive vocabulary contract. Commits: generated `SinkOnly` from BIR
(`codegen/src/json_sink_direct.rs`), ContainerNext dispatch carry, generated-retained tiny-string
cap, host-output-schema typed DirectBuild (twitter real_typed_struct 151.5% sonic PASS), Eisel-
Lemire vendored (`2631a834`, et al.). Still at N-direct/NoGo: 11 direct rows, 13 parse rows.
Deferred out: CostFacts substrate, full Lock 14 remediation, string scanner widening (the dominant
residual kernel shifted to `match_tiny_plain_string_with_cap::<16>` at 8 of 13 parse-G rows).

Grade: B — two critical correctness fixes (utf8_lossy, yyjson framing) land code and honesty
simultaneously. Roughly 50% code, 50% corrections/research.

---

## SK-V7 (2026-05-16) — First Formal Framework Cycle

SK-V7 inaugurated the iterative auto-convergent prompt framework (Pass Alpha, Pass Omega, Triumvirate
per wave, six-lens CHALLENGE, 24-column RESULTS schema). The cohort correctly identified that W4
(per-`\uXXXX` TBL classifier) applied to only 2 of 4 rows (unicode_escapes and y_string_unicode),
not all four as earlier diagnosed — a critical correction to SK-V4's prescription. The CostFacts
substrate gap was named and designed (~830 LOC across ir/+passes/+codegen/). Lock 14 audit enumerated
~46 HIGH leaks across `passes` (11) + `codegen` (~18) + `parse-that-regex` (9) + `ir` (3). Commits
admitted: W0 strict sonic-rs comparator repair, W1 TapeKind rename, W3 capacity-hinted numeric Vec
(mesh/marine_ik typed PASS), W7+W8 Lock 14 neutralization phases A+B, W9 CostFacts substrate
projection, W10c B6 stack-canary Stage 1 (`ed923615`, `89f29768`, `41ecf187`, `51d8c8be`, `56e66ef5`,
et al.). Rejections: W2 (no canada fallback pool), W4 (single-quartet Unicode classifier correctness-
green but row-failing), W5 (StringBlock16 tiny probe regressed 6 rows), W6 (object-pair value-byte
compaction missed thresholds), W10/W10b (PMULL prefix-XOR regressed; CTZ/bulk production rejected).
Closed still at N-direct/NoGo. Deferred out: twitter parse (yyjson 1.98x gap), fusion-quality refactor.

Grade: B — honest rejections, real infrastructure landed, framework codified. Process overhead
beginning to accumulate. ~45% code, ~55% process/documentation.

---

## SK-V8 (2026-05-17 through 05-18) — Substrate-Ceiling Thesis

SK-V8 was primarily an observability tranche. The S-P2 cohort (six agents SC-1 through SC-6) ran a
six-iteration hardening convergence (V1-V4 non-converging, V5 REVISE, V6+V7 ACCEPT with explicit
user over-ceiling authorization) on a substrate-ceiling hypothesis: that bbnf was a two-pass parser
whose fast SIMD structural scan was dead code, paying for structural-byte discovery twice. The
finding explained every SK-V7 rejection simultaneously (W2, W4, W5, W6, W10, W10b each optimised
the scalar rediscovery pass that should not exist). W6 admitted no source changes. Closed N-direct/
NoGo. The W3 union-substrate hypothesis was selected as the leading W3 candidate but gated behind
W0/W1 closure. Deferred out: union-substrate implementation (routed to SK-V9/W3).

Grade: C+ — six-iteration process overhead to converge on a diagnosis that took ~90% documentation
and ~10% actual code change. The finding was correct but the convergence mechanism was expensive.

---

## SK-V9 (2026-05-18 through 05-19) — Union Substrate Falsification

SK-V9 dispatched W1 (direct output/control-path contract) and W2 (direct row-table reclamation,
admitting `apache_builds` and `numbers` to direct A/GO). W3, the union-substrate implementation,
was falsified by REDRESS 96 and 97: two faithful union-substrate implementations both regressed
uniformly — the scalar delimiter path was cheaper than the retained SIMD structural cursor on M5 Max.
W3 retired at REDRESS 98. The substrate-ceiling thesis was falsified as a close route. Closed
N-direct/NoGo; the W3 pre-block became a permanent hard boundary. Deferred out: CSS/Sheets/BBNF-self
grammar generalization (routed to totality track), unicode/string kernel work (requires micro-prove-
first gate), direct plane residuals.

Grade: B — the W3 falsification is a genuine result that eliminated a dead end. Direct admissions
real. Documentation-to-code ratio still high. ~40% code, ~60% process.

---

## SK-V10 (2026-05-19) — Direct Plane Progress

SK-V10 closed the direct output/control-path contract (W1), direct row-table reclamation (W2, adding
`apache_builds` and `numbers`), root-type typed generalization proof (W5), root typed row admission
for `github_events` (W6, Track 1 12,827 Mbps vs sonic 12,695 Mbps), hex-escape micro-proof (W8,
`unescape_four_unicode_escapes` 1.268x), and `instruments/direct_to_struct` (W10). Rejections: W4
(`instruments/real_typed_struct` Track 2 missed floor), W7 (full-string primitive 0.774x), W9
(already-wired caller — no source delta). Final state: 17 parse S/NO-GO, 6 direct A/GO, 11 direct
N-direct/NO-GO, 7 typed A/GO; overall N-direct/NoGo. The parse_only plane was formally retired as
a SOTA admission target for SK-V10. Grammar generalization was routed to totality track. Deferred
out: 11 direct residuals, grammar generalization, all parse_only rows.

Grade: B — steady direct-plane progress, micro-prove-first gate established and exercised, honest
rejections. Mostly JSON-only. ~50% code, ~50% process.

---

## SK-V11 (2026-05-19 through 05-20) — Direct Fixpoint

SK-V11 set the direct plane as primary close target (13 residual N-direct rows) and required one
non-JSON grammar intervention. REDRESS 119 proved all 13 residual direct rows uncloseable inside
SK-V11 with measurement. The grammar-generalization axis closed as BLOCKED under REDRESS 120 and
112/113 — four consecutive JSON-only skinny cycles had not produced a non-JSON proof. The only
admitted row in the direct plane was new, but all 13 core residuals were proven uncloseable by
measurement rather than admitted. No non-JSON grammar landed. Closed as a measured direct fixpoint.
Deferred out: all 13 direct residuals, all 17 parse_only rows, grammar generalization.

Grade: C — REDRESS 119 is honest work, but a cycle that proves nothing can be done and defers
everything is near-zero forward progress. The grammar-generalization BLOCKED call was a correct but
embarrassing admission after four JSON-only cycles. ~20% code, ~80% process/measurement.

---

## SK-V12 (2026-05-20 through 05-21) — First CSS Admission (Dishonest)

SK-V12 pivoted to CSS under user pin `USER-PIN-W1-CSS-L4-SOTA.md`, targeting a generated CSS L4 row
beating lightningcss. W1b admitted `css_l4/declaration_values/direct_to_struct/main`: Track 1
429.3 Mbps vs lightningcss 168.9 Mbps with strict SHA-256 equality. This was the campaign's first
non-JSON generated parser benchmark. REDRESS-126 and REDRESS-127 closed W4 and W5. Closed PASS-ADMIT
under the then-current close clause. The SK-V14 audit later revealed this admission was built on
hand-written templates carrying a fabricated `@generated` marker, not genuine `cargo xtask regen-css`
output. The claim that CSS was "admitted" survived until the audit demolished it.

Grade: D — the first CSS row was a real benchmark win in isolation, but the infrastructure was
fraudulent: templates were hand-written and `@generated`-labeled to appear codegen output
(`sk-v14/SYNTHESIS.md §1.2`; `v1-css-l4-validation.md §1`). Zero genuine CSS codegen landed.

---

## SK-V13 (2026-05-21) — Scope Escalation

SK-V13 opened with the 2026-05-21 user pin addendum overriding the prior single-row CSS close,
reinstating all 17 parse_only rows as SOTA-eligible, and demanding: 23 more CSS features (24 total),
decision-engine fold (bbnf-regex + e-graph + active cost + CSP resolver), one union admission, zero
aarch64 orphans, all 51 JSON cells above strict sonic-rs, and Totality V1.1 ratification before
Wave 0. This was an extremely aggressive scope expansion. No implementation reached close. SK-V13
bracketed SK-V14 immediately because the targets were not met. Deferred out: every target.

Grade: D — entirely a contract / process document; zero code shipped in the campaign timeline.
The cycle existed to set the bar, not to clear it.

---

## SK-V14 (2026-05-22 through 05-27) — Audit-Zero, Prune, and Rebuild

SK-V14 began with a six-agent audit pack that invalidated all prior claimed admissions: 25 CSS rows
(hand-written templates with fake `@generated`), 5 JSON parse_only admits (gate-relabel only, parser
unchanged), all 17 JSON direct/typed admits (real parsers but comparator misbinding — `sonic_rs::
from_slice::<Value>` eager-DOM for all three planes rather than plane-correct strict comparators).
The honest baseline was 0/51 JSON and 0/24 CSS. PRUNE waves restored honesty; then W5A through
W5D-DELETE, W6 (67 hand-written files → generated output, 9 sub-waves), W7 (CSP solver), W8
(e-graph facts), W9 (policy union facts), W10-W11 admitted rows on honest comparators. Commits:
`feat(sk-v14-waveW6.*)`×9 grammars, `feat(sk-v14-waveW7)`, `bae430dcf` (parse_only memchr), et al.
Closed full-admit in the row ledger (51/51 JSON + 24/24 CSS). PASS-IMPL V1 later rejected the CSS
leg as a generalization claim, but the JSON row admissions on properly rebound comparators were real.

Grade: B+ — the most consequential architecture work of the campaign: genuine generated parsers,
honest comparators, PRUNE-before-admit discipline. The CSS "full admit" was premature because PASS-
IMPL rejected it, but the JSON work was real. Roughly 70% code, 30% process.

---

## SK-V15 (2026-05-27 through 05-28) — CSS Prune-Then-Rebuild

SK-V15 operated under PASS-IMPL V1 verdict which identified CSS flaws: a broadcast-admitted 24-row
claim from one timing tuple, a 646-line hand-written CSS tokeniser embedded as a string literal, and
a workload mismatch against lightningcss CSSOM. PRUNE waves A-D demoted CSS broadcasts, restored
Lock 14/16 gate coverage, abrogated codegen leaks, and enforced Pattern H generated discipline.
REBUILD waves E-G built CSS typed Value API, activated Decision Engine and all five BackendShape
lowerers, and quarantined FNV closed-enum products. Commits: `bc73351cd` through `bada86a`
(seven REBUILD waves). Closed as ADMIT-W11 with routed blocks. JSON remained 51/51. CSS L4 admitted
rows remained 0. PASS-IMPL V2 accepted the close but the grammar-driven inflection had not been
reached. Deferred out: grammar-derived CSS, typed CSS equality, full Pattern H collapse.

Grade: B — real cleanup work and infrastructure. The pattern of declaring close then needing another
cycle to fix what close exposed was appearing. ~60% code, ~40% process.

---

## SK-V16 (2026-05-28 through 05-29) — CSS Grammar-Derived Provider

SK-V16 targeted CSS L4 grammar-derived provider from `grammar/css/l4/*.bbnf`, typed equality vs
cssparser, and Pattern H collapse. Commits landed: CSS grammar-derived provider (`ec12016c8`,
`ea8138056`), O(1) speculative-checkpoint (`8153236e8`), shared flat-tape runtime substrate
(`1c5bd7a25` — LANDED BUT UNWIRED for CSS). W6 speed report recorded honestly: CSS typed parsing
~14× slower than lightningcss; tape decoded no grammar yet. PASS-IMPL V3 verdict: the >SOTA bar was
not met. CSS typed parsing remained non-competitive. Closed SK-V16 at `1c5bd7a25` with grammar-
derived provider, 8-field structural equality banked, the flat-tape substrate landed, and the >SOTA
gap acknowledged. Deferred out: tape activation for CSS, layout-driven projection, NEON hot-leaf.

Grade: B — the grammar-derived provider and flat-tape substrate were real, non-trivial infrastructure
wins. The decision to land the tape as dead code and close honestly is correct under the precepts.
~65% code, ~35% process.

---

## SK-V17 (2026-05-29 through 05-31) — Tape Activation + CSS >SOTA

SK-V17 activated the flat-tape substrate for CSS (W1: PRUNE fact-stream String → TapeBuilder,
`3a37c29d8`), rebuilt lazy rich typed-CSSOM projection from the tape (W2, `6dad81fb9`), and landed
NEON structural-index acceleration (`6bb4b2a6c`). W4 conditional-not-met (L9); W5 closed. The CSS
typed Track 1 went from ~3 Mbps (fact-stream) to competitive with lightningcss per the N=200 harness.
The >SOTA bar vs lightningcss full-CSSOM was met on the structurally-regular corpora (animate,
bootstrap). Closed at HEAD `f6a38445b`. The `W5C_REQUEST_FACT_PROFILES` array still lived in
`codegen/src/lib.rs:336` as a Lock-14 per-grammar routing construct that SK-V17 retired partially.
PASS-IMPL V3 audit (`7dbe44c22`) declared SK-V17 "at the inflection point" and SK-V18 the
generalization cycle. Deferred out: Sheets/BBNF-self third grammar proof, value-api phase 3,
totality fold.

Grade: A- — this was the first cycle to actually beat lightningcss on a non-trivial corpus.
Real performance wins, honest handling of tailwind gap, correct decision to close without papering
over residuals. ~75% code, ~25% process.

---

## SK-V18 (2026-05-31 through 06-06) — Generalization Proving Cycle

SK-V18 had to prove that one grammar-driven generator could emit a structurally novel third grammar
(the negative control) without overfitting. Commits: W-PRUNE P1-P5 deletion (`7de58acad`), G1 JSON
projection SinkOnlyExpr AST-walk emitter (`4ff1b806d`), G2 CSS grammar-derived generator +
`css_balanced_component_scan` (`ed538afd7`), G3 un-fork the emitter — DELETE `RuntimeEmitterKind`
(`b5ab2e17c`), G4 shared `Cursor` value-API trait + DELETE phantom `<G>` (`a9585cc65`), G6 NEON
structural classify (`369f4ffe7`), PROVE: general recursive-descent emitter — Sheets via the
un-forked generator (`1aba0422e`). The pass-alpha hardening went five iterations (V1-V5, each folding
REVISEs). Key finding: the x86 ASM tree (`bbnf-simd/ext/x86/`, `build.rs`, ~3000 LOC) survived the
P1 deletion because the verify grep scoped only `bbnf-simd/src/`; `ext/` is a sibling, not a child.
Closed with the un-forked dispatch, SoA tape, `ValueRef<K>/Cursor`, 5-shape `BackendShape`,
`ValueProject` derive, G6 NEON classifier proven on 3 grammars. SK-V18 was declared CLOSED at
`405786b1a`. CSS framing disclosure: the depth asymmetry (lazy-rich HEAD vs eager-full-CSSOM) was
corrected post-close (`a97f5f365`). Deferred out: totality fold (9-grammar fleet), x86 ASM tree
deletion (caught by SK-V19 α-hardening).

Grade: A — the hardest cycle and the one that actually proved the generator thesis. The x86 tree
escape was an honest failure of a grep gate, caught and corrected. ~80% code, ~20% process.

---

## SK-V19 (2026-06-07 through present, in progress) — Totality Fold

SK-V19 adopts the SK-V18-proven architecture into `crates/core` for all 9 grammars. The PASS-IMPL
fold-readiness verdict classified the fold as GREENFIELD ADOPTION, not a light wiring: `crates/core`
had no SoA tape, no `ValueRef`, no `BackendShape` (enum exists zero times in core per `rg backend_
shape crates/ir` = 0), and three eager builder families (OpenFrame json/css_l4/bbnf, Frame sheets,
SimpleStructBuilder ×5). Commits to date: `d741f065d` (certified totality-fold SPEC), `f4633d468`
(re-key BBNF shape recognizer on structure, not rule name), `36ca10646` (collapse named-grammar leaks
— metadata-driven manifest, CSS host relocation, live Lock-14 gate). The wave manifest is W0 through
W6 with binding architectural law D11: the C fold (W3 value-plane) and decision engine (W5) must land
together because a naive grammar-walk regresses CSS vs lightningcss without cost-model-gated
primitives. Deferred out: all six waves are in progress; close pending.

Grade: B (partial, in-flight) — the SPEC is sound and the approach is correct. The cycle is open.

---

## Arc Analysis

**Effort distribution by category:**

Documentation and process consumed roughly 50-65% of calendar time across the full campaign. The
tranches from SK-V8 through SK-V12 were particularly documentation-heavy: SK-V8 was almost entirely
a diagnosis cycle with six convergence iterations; SK-V11 was a fixpoint documentation exercise;
SK-V13 was a contract document with zero implementation.

**JSON concentration:** Every cycle from SK-V5 through SK-V14 was primarily or exclusively JSON.
The first non-JSON benchmark — the CSS declaration_values row in SK-V12 — was retroactively
invalidated as fraudulent (hand-written templates). The first honest non-JSON benchmark landed in
SK-V17. The generator thesis (Lock 14 compliance, grammar generalization) was not exercised
until SK-V18, roughly 15 cycles into the campaign.

**Revert/prune overhead:** SK-V13 opened at audit-zero (0/51 JSON, 0/24 CSS) after a six-agent
pack invalidated every prior admission. This represents one of the most expensive revert events in
the campaign: SK-V12 claimed a full-admit, SK-V13 claimed 24+ CSS rows plus JSON admits, and
SK-V14's honest baseline was zero across the board. Multiple cycles of work had to be un-done and
redone honestly.

**Framework overhead:** The iterative auto-convergent framework (Pass Alpha, six-lens CHALLENGE,
S-P1/P2/P3 passes, hardening cycles, G-Alpha/G-Omega) absorbed significant time. In SK-V17, the
Pass Alpha alone ran to five challenge iterations before closing. The framework caught real problems
(the x86 tree escape in SK-V18, the broadcast-admission pattern in SK-V15, the fake-@generated CSS
in SK-V14) but also added latency on straightforward cycles.

**The genuine SOTA-beat question:** As of SK-V19, the JSON >SOTA claim is based on a
JSON-comparator-rebound (SK-V14) that corrected from eager-DOM `sonic_rs::from_slice::<Value>` to
plane-correct struct deserialization. The CSS >SOTA claim (SK-V17) is against lightningcss
full-CSSOM on structurally-regular corpora with depth asymmetry disclosed. The totality fold is
in progress; no `crates/core` benchmark confirms the >SOTA claim yet.

---

## Cycle Summary Table

| Cycle | Aim | Landed | Deferred Out | Grade |
|---|---|---|---|---|
| SK-V3.5 | PSI forensics, GO/NO-GO | 6-report anatomy; GO with CollapsedStage excised | CollapsedStage DPDA, FSM parity harness | A |
| SK-V5 | BackendShape, SinkOnly codegen, Lock 14 | BackendShape enum, Eisel-Lemire, simd-scan purge, SinkOnly BIR | String fusion, Lock 14 cleanup (46 HIGH), CostFacts | B+ |
| SK-V6 | Comparator honesty, yyjson framing | utf8_lossy fix, ContainerNext, typed DirectBuild, yyjson target recalibrated | CostFacts, string scanner, Lock 14 | B |
| SK-V7 | CostFacts, string kernels, Lock 14 | CostFacts substrate, TapeKind rename, mesh/marine_ik typed, Lock 14 phases A+B | twitter yyjson gap, most string kernels rejected | B |
| SK-V8 | Substrate-ceiling diagnosis | S-P2 convergence: dead structural scan identified | Union-substrate implementation | C+ |
| SK-V9 | Union substrate, direct admissions | W3 union-substrate FALSIFIED; 2 direct rows admitted | 11 direct residuals, grammar generalization | B |
| SK-V10 | Direct plane, typed rows | 3 more direct/typed rows admitted, micro-prove-first gate | 11 direct residuals, parse_only, grammar generalization | B |
| SK-V11 | Direct fixpoint or close | REDRESS 119: all 13 residuals proven uncloseable | 13 direct residuals, parse_only, grammar generalization | C |
| SK-V12 | First CSS admission | CSS declaration_values row (later invalidated) | 23 CSS features (all honest ones) | D |
| SK-V13 | Full SOTA across 51 JSON + 24 CSS | Zero — contract only | Everything | D |
| SK-V14 | Prune then re-admit honestly | Audit-zero established; all 51 JSON + 24 CSS claimed (CSS rejected by PASS-IMPL V1) | CSS generalization | B+ |
| SK-V15 | CSS prune/rebuild, Decision Engine | Lock 14/16 restored, all 5 BackendShape lowerers, FNV quarantined | Grammar-derived CSS, typed CSS equality | B |
| SK-V16 | CSS grammar-derived provider, flat tape | Grammar-derived provider, flat-tape substrate (unwired), O(1) checkpoint | Tape activation, NEON, >SOTA | B |
| SK-V17 | CSS tape activation, >SOTA | CSS >SOTA on regular corpora, NEON structural classify | Third grammar proof, value-api, totality fold | A- |
| SK-V18 | Generator generalization (3 grammars) | Un-forked dispatch, SoA tape, ValueRef/Cursor, 5-shape BackendShape, Sheets proof, NEON | Totality fold (9-grammar fleet) | A |
| SK-V19 | Totality fold, 9-grammar fleet | SPEC certified; W6 Lock-14 collapses landed; W0-W5 in progress | W0-W5 close pending | B (partial) |
