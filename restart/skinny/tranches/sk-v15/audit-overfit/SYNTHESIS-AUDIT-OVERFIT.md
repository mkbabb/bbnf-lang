# SK-V15 S-P0 Overfit Audit Synthesis

Date: 2026-05-27.
HEAD sampled: `5f60b131c`.
Input pass: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`.
Authority input: PASS-IMPL V1 at
`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.

## Result

S-P0 does not clear. The audit confirms the SK-V15 Alpha bracket:
JSON remains an honest guard baseline; CSS L4 is audit-demoted; Pattern H,
Lock 14 / Lock 16, codegen neutrality, and Decision Engine activation are
still prune/rebuild obligations.

| Axis | Verdict | Primary finding | Receiver |
|---|---|---|---|
| A1 measurement integrity | FAIL / CRITICAL | 24 CSS rows are one repeated aggregate measurement | PRUNE-WAVE-A |
| A2 admit mechanism | FAIL / CRITICAL | CSS admit uses static `CSS_GENERATED_RS`, mismatched comparator, marker-only equality | PRUNE-WAVE-A, PRUNE-WAVE-C, REBUILD-WAVE-E |
| A3 Lock 14 / Lock 16 scan | FAIL / CRITICAL | generic scan omits leak roots and Lock 16 report coverage is incomplete | PRUNE-WAVE-B |
| A4 generator round-trip | PRUNE-REQUIRED | CSS "generated" bodies are hand-curated string output; root runtime lacks generated provenance | PRUNE-WAVE-A, PRUNE-WAVE-C, PRUNE-WAVE-D |
| A5 Decision Engine fold | FAIL / CRITICAL | e-graph has zero rewrites, CSP preserves selected index, four lowerers are stubs | REBUILD-WAVE-F |
| A6 pattern recurrence | FAIL / CRITICAL | Pattern H and grammar-family codegen recurrence persist at category scale | PRUNE-WAVE-C, PRUNE-WAVE-D |

## Preserved Clean Baselines

- JSON measurement/admit integrity remains a guard baseline, not new work.
  A1 records no JSON broadcast cluster and above-floor corpus sizes; A2
  records parse/direct/typed guard rows as clean except for the bench-only
  FNV caveat routed to REBUILD-WAVE-G.
- The 16-lock count and 67-file Pattern H count were rechecked during Alpha
  authoring. The count is preserved, but generated ownership fails.
- The five `BackendShape` variants remain intact; the failure is lowerer
  depth and emission consumption, not a sixth shape.

## Blocking Findings

### P0-1 CSS Broadcast Admission

Rows `css_l4/*/direct_to_struct/main` repeat the same timing tuple across
24 conceptual admits in `restart/skinny/ROLLING-SOTA-DELTA.md:70-93`.
A1 traces the repeated TSV tuple and `W8_SELECTED_CSS_ROWS` multiplier;
A2 traces the same broadcast through the admit mechanism. SK-V15 must
collapse these to one diagnostic aggregate or produce distinct typed CSS
measurements per feature.

Receiver: PRUNE-WAVE-A.

### P0-2 CSS Provider / Value Plane Contrivance

CSS companion modules still come from `CSS_GENERATED_RS` in
`skinny/crates/codegen/src/runtime_generator.rs:713`, not from grammar-shaped
emission. The output is still `CssFullParseSummary` / fact-stream text, not a
typed CSS value/document/view API. A4 confirms byte-for-byte round-trip only
reproduces the relocated hand-written string.

Receivers: PRUNE-WAVE-A, PRUNE-WAVE-C, REBUILD-WAVE-E.

### P0-3 Lock 14 / Lock 16 Gate Holes

A3 confirms the Lock 14 root scan omits known leak-bearing codegen roots and
the token universe is JSON-shaped. It also confirms Lock 16 close coverage is
not yet a source-present primitive manifest with strict checkasm command
validation. Self-exempting companion gates remain live in legacy paths.

Receiver: PRUNE-WAVE-B.

### P0-4 Codegen Grammar-Family Leakage

A3 and A6 both find grammar-family mode splits and static profile rosters:
CSS profile tables, `RuntimeGenerationMode` family branches, JSON templates,
JSON byte/literal recognizers, and shared-crate grammar/backend binding tables.

Receiver: PRUNE-WAVE-C.

### P0-5 Pattern H Generated Ownership

A4 and A6 both confirm the invariant count remains 67, but 0/67 root runtime
files carry line-1 generated provenance. Root CSS runtime regen is write-only
and not a delete+regen check. CSS `LegacyPath` aliases remain in the generated
projection surface.

Receiver: PRUNE-WAVE-D.

### P0-6 Decision Engine Scaffold

A5 confirms `backend_egraph` runs zero rewrite rules, `decision_csp` pins the
already-selected candidate, generic cost facts still name JSON/CSS, and four
of five lowerers are 17-line label-string stubs. Governance wave closure is
not implementation proof.

Receiver: REBUILD-WAVE-F.

### P0-7 Bench-Only FNV / Companion Quarantine

A2 keeps the FNV closed-enum issue at MEDIUM because it is bench-only, but it
must not migrate into production runtime or become generalized equality
machinery. A6 also routes non-generated witness companions to quarantine or
generated roster membership.

Receiver: REBUILD-WAVE-G.

## Prune List

These rows are receiver buckets, not dispatchable implementation waves. S-P3
must split each bucket into exact-owner waves or sub-waves that inherit the
SK-V15 Alpha cost envelope: research <=20 minutes, plan <=15 minutes,
redress <=30 minutes, commit at 0.9N, halt at N. All receiver exit evidence
is Apple M5 Max / aarch64 native only; x86 and AVX-512 evidence is diagnostic
and cannot anchor admission, SIMD, Lock 16, SOTA, or close claims.

| Receiver | S-P0 binding payload | Exit condition shape |
|---|---|---|
| PRUNE-WAVE-A | CSS broadcast admission and provider/value-plane demotion | no 24-row broadcast admit; typed rebuild proof lands no later than any `CSS_GENERATED_RS`, provider, template, `CssFullParseSummary`, or fact-stream retirement; REDRESS-184 and REDRESS-209..212 preblocked; cssparser same-workload comparator |
| PRUNE-WAVE-B | Lock 14 / Lock 16 close-gate restoration | full generic root scan; reported exclusions; Apple M5 Max / aarch64 strict checkasm or primitive manifest; x86/AVX evidence non-close only; no self-exempting companion gate |
| PRUNE-WAVE-C | split-required codegen grammar-family leakage abrogation | generic grammar-id regen/check path; no `RuntimeGenerationMode` family split; no static CSS roster; no JSON/CSS generic recognizers; no per-grammar regen enum/match fanout; no per-grammar workaround paths; any touch to JSON templates/direct/typed codegen/recognizers reruns and preserves 51/51 JSON guard rows |
| PRUNE-WAVE-D | Pattern H generated ownership | 67-file count preserved; all 67 line-1 generated provenance; one grammar-neutral generator plus non-writing delete+regen proof; per-grammar reheader/rewrite workarounds rejected; CSS `LegacyPath` shim removed; REDRESS-183 and REDRESS-213 preblocked before destructive `crates/core/src/runtime/css_l4/` delete/regen |
| REBUILD-WAVE-E | CSS typed value API | typed value/document/view/visitor output; CSS rows retimed on typed plane; lightningcss only after CSSOM parity |
| REBUILD-WAVE-F | split-required Decision Engine activation | sub-waves for e-graph rewrite, CSP selection, grammar-neutral fact cleanup, and lowerer implementation; generated runtime diffs required; all five lowerers runtime-relevant |
| REBUILD-WAVE-G | FNV/witness quarantine | no FNV-keyed production arbiter; adversarial strict-product differential; witness companions generated or quarantined |

## Operational Note

Axis A4 reports that it ran root `cargo xtask regen-css` and `cargo xtask
regen-json`. Current status shows the earlier dirty root
`crates/core/src/runtime/css_l4/*` entries no longer appear, while the skinny
CSS generated files remain dirty. Do not stage or rely on that normalization
as audit evidence. Treat the root CSS runtime mutation as shared dirty-state
churn outside this S-P0 documentation slice.

## S-P0 Close Posture

S-P0 may proceed to challenge hardening and Alpha routing with a
FAIL / PRUNE-REQUIRED disposition. The failed axes are inputs that
S-P1/S-P2/S-P3 must profile, research, and manifest into capped, exact-owner
SK-V15 waves. No behavior wave may dispatch before S-P3 consumes this prune
list and preserves the Alpha dependency table and hard-cap envelope.

No prune/rebuild receiver may close on documentation alone. Each receiver
close must cite HEAD command output and, where relevant, generated artefacts
or diffs, strict checkasm or primitive-manifest output, and cold per-parse
measurement evidence; otherwise it remains REDRESS or blocked.
