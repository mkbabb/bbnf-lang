# SK-V14 Alpha-E — Candidate Shortlist (PRUNE-first)

Pass: Alpha SK-V13 → SK-V14, lane α-E.
Date: 2026-05-22.
Scope: candidate intervention families for SK-V14 S-P3 wave planning, under
the prune-then-rebuild posture pinned by
`restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` (commit
`496a81417`) and the dispatch context at
`restart/skinny/tranches/sk-v14/research/alpha/DISPATCH-CONTEXT.md` (commit
`6ab711d77`).

## §0 — Authority + binding posture

Binding (read in order):

- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:97-159` —
  R1 through R10 of the SK-V14 goalset; close-condition R10 verbatim.
- `restart/prompts/pass-contracts/PASS-ALPHA.md:18-29` — α-E scope row
  (≤5 candidates; per-candidate falsifiability gate, LOC budget, risk
  class, same-wave consumer plan).
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-46` — full
  CSS L4 lightningcss parity, every JSON row × plane > sonic-rs strict,
  `parse_only` admission-eligible, indefatigable.
- `restart/locks/LOCKS.md:220-263` — Lock 14 text; binding
  grammar-neutrality clause and v+1 generated-output allowance.
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
  — cross-axis audit verdict and binding PRUNE list (PRUNE-1 through
  PRUNE-5).
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v{1..6}-*.md` —
  per-axis validation pack carrying the honest baseline.

Honest baseline bind (cited, not re-litigated):

- JSON `parse_only` 0 / 17; JSON `direct` 0 / 17; JSON `typed` 0 / 17;
  CSS L4 0 / 24. Per
  `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md:71-78` and
  `DISPATCH-CONTEXT.md:55-58`.
- W5 / W6 / W7 / `bbnf-simd` / `OffsetFlags` + Tape /
  `generated_json::parse_direct` / `generated_real_typed::parse_*` /
  15 CSS `.bbnf` grammars at `grammar/css/l4/` SURVIVE
  (`ORCHESTRATOR-PROMPT.md:39-47`).
- 25 CSS rows + 5 parse_only admits + 4 direct admits + 7 typed admits +
  W8 + W9 + 30 Lock-14 violations DO NOT SURVIVE
  (`ORCHESTRATOR-PROMPT.md:49-69`).

## §1 — Why prune-first (binding reconciliation)

The α-E contract caps the shortlist at five and PASS-ALPHA §2 binds each
candidate to a falsifiability gate that names rows + Mbps thresholds. The
honest baseline carries zero admitted rows; every Mbps threshold the
candidate slate could nominate is currently a re-admit threshold against a
plane whose comparator is misbound, against a corpus that does not exist
in production, against a generator that hand-curates its output. Until
prune-1, prune-2, and the comparator rebind land, no re-admit candidate
can carry a measurable falsifiability gate.

Therefore the five candidate slots are spent on the prerequisite
infrastructure: comparator rebind (R1+R2), regen-css pipeline + corpora
(R4+R5), the two PRUNE waves that void the dishonest admits (R3 PRUNE-1
+ PRUNE-2), the Lock-14 refactor cluster that collapses the recurrence
vector (R3 PRUNE-3 + PRUNE-4), and the scaffold-to-load-bearing wave
that turns W8 + W9 into runtime consumers (R3 PRUNE-5). R6 / R7 / R8 are
re-admit waves and consume these candidates downstream; per the dispatch
context they belong in SK-V14's wave program after Pass Alpha closes,
not in the α-E slate.

Each candidate below carries:

- file path (owner paths).
- scalar reference status (where applicable).
- checkasm test status (where applicable).
- same-wave consumer plan.
- falsifiability gate (named rows / commands / Mbps thresholds).
- LOC budget.
- risk class.

The slate is exactly five.

## §2 — Shortlist (the table)

| ID | Candidate | R-target | Same-wave consumer | Falsifiability gate | LOC budget | Risk |
|---|---|---|---|---|---:|---|
| **C-1** | Lock-14 refactor cluster: provider trait + grammar-agnostic generator + 64-file Pattern H collapse | R3 PRUNE-3 + PRUNE-4 | regen-derived runtime for every grammar emitted in the same waves; gate run before commit | grep returns ZERO for `RuntimeProvider::Json`/`JsonGrammar`/`parse_json_grammar`; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO; every generated file traces to a generator + grammar source (see §3 for full per-sub-wave gate + forward invariant) | 2.8k – 3.4k | very high |
| **C-2** | Comparator rebind + per-iteration equality oracle | R1 + R2 | bench harness consumes the rebound comparators on every named JSON row; `xtask gate-json` enforces the schema | three plane-correct strict comparators in `bbnf-bench`; per-iter equality column present in every emitted RESULTS row; `xtask gate-json` rejects rows whose equality column is empty | 600 – 1.08k | high |
| **C-3** | `cargo xtask regen-css` + production corpora at `skinny/corpora/css-l4-sk-v14/` (first instance of the `regen-{grammar}` family) | R4 + R5 | runtime regenerated from the 15 `.bbnf` files in the same wave; bench rows wired to the new corpora | round-trip: `rm -rf … && cargo xtask regen-css && git diff` empty on BOTH skinny and core runtime trees; bypass-header detector empty; `du -sh skinny/corpora/css-l4-sk-v14` > 800 KB with Bootstrap + Tailwind + Material + Animate present (see §5 + hardening V1 CH7 §3.1 for full three-part gate) | 1.2k – 2.0k | high |
| **C-4** | W8 per-grammar policy + W9 same-substrate union scaffold → load-bearing | R3 PRUNE-5 | CSP-selected shape produces measurable runtime divergence on at least one named pre-wave row in the same wave | divergence proof on named row `json/numbers/direct_to_struct/main`: pre-wave hot leaf `parse_value_at`, post-wave hot leaf names the W11.1 number-specialised symbol explicitly in the samply trace; row hot leaf attribution changes in `RESULTS.md`; per-shape Lock-1 triad declared in REDRESS (see §6 for full owner-path discipline) | 800 – 1.4k | very high |
| **C-5** | Clean revert of W14.1–W14.5 + deletion of 7 CSS template providers + 24 CSS-row revert | R3 PRUNE-1 + PRUNE-2 | REDRESS per row cites the validation §reference; ROLLING-SOTA-DELTA rebases to the audit-zero baseline in the same commit set | `git grep -l '@generated' skinny/crates/runtime/src/grammars/css_l4_*` returns ZERO; `git grep -l 'include_str!' skinny/crates/codegen/src/css_l4_*_provider.rs` returns ZERO; ROLLING-SOTA-DELTA shows JSON parse_only 0/17 and CSS L4 0/24; `skinny/REDRESS.md` carries 29 new row-keyed entries | 250 – 500 (delete-heavy) | medium |

Total LOC envelope: ≈ 5.65k – 8.38k across the five candidates (C-1
lower bound raised to ≈ 2.8k per CH4 V1 to reflect 64-file refactor
reality; C-2 ceiling raised by ≈ 80 LOC per CH4 V2 to cover the in-tree
Skipper-class fallback path α-B §316 flags). C-1 dominates; C-5 is
mostly deletion. Risk-weighted: C-1 + C-4 carry the architectural risk;
C-2 + C-3 carry the throughput / reproducibility risk; C-5 carries
audit-trail risk only.

## §3 — C-1 — Lock-14 refactor cluster

**R-target.** R3 PRUNE-3 + PRUNE-4 per `ORCHESTRATOR-PROMPT.md:115-123`.

**Purpose.** Demolish the recurrence vector flagged across
`v3-lock14-deep-scan.md` and the totality Pattern H surfaced by
`SYNTHESIS-AUDIT-OVERFIT.md §Totality Pattern H`. Replace the
`RuntimeProvider` enum with trait-based dispatch in `skinny/crates/`.
Collapse the 8 per-grammar provider modules in `codegen/` into ONE
grammar-agnostic generator template consuming grammar source + workspace
metadata. Refactor the 64 hand-written per-grammar files in
`crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf,
bnf, math}/` into emitted output. Sub-divide by grammar (8 sub-waves)
per `ORCHESTRATOR-PROMPT.md:122-123`.

**Owner paths.**

- `skinny/crates/codegen/src/grammar_profile.rs` — `RuntimeProvider`
  enum (`v3-lock14-deep-scan.md §1 C2`, lines 16–26).
- `skinny/crates/codegen/src/lib.rs:1-10` — 8 `mod` declarations naming
  per-grammar providers.
- `skinny/crates/codegen/src/{json_provider,css_l4_*_provider}.rs` — 8
  provider files (`SYNTHESIS-AUDIT-OVERFIT.md §Skinny Lock-14`,
  CRITICAL C3).
- `skinny/crates/codegen/src/{json_templates,css_l4_*_templates}/` — 8
  template directories that must be deleted (CSS) or generator-emitted
  (JSON regen path stays).
- `skinny/crates/runtime/src/lib.rs:3-44` — 8 hardcoded `#[path = ...]`
  declarations + namespace re-export
  (`v3-lock14-deep-scan.md §1 C1`).
- `skinny/crates/runtime/src/grammars/` — 8 hardcoded per-grammar
  subdirectories.
- `skinny/crates/bbnf/src/lib.rs:46-64` — `JsonGrammar` public struct +
  `compile_json_source` + `compile_json_file`
  (`v3-lock14-deep-scan.md §1 H1`).
- `skinny/crates/grammar/src/lib.rs` — `parse_json_grammar` +
  `load_json_grammar` (`v3-lock14-deep-scan.md §1 H2`).
- `skinny/crates/passes/src/decision_csp.rs:235` — hardcoded `"json"`
  rule string (`v3-lock14-deep-scan.md §1 H3`).
- `crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf,
  bnf, math}/` — 64 hand-written files per Pattern H.

**Scalar reference status.** N/A (refactor; no SIMD primitive).
Behavioral equivalence is the scalar reference: every regen-derived
parser must reproduce the byte-for-byte output of the current
`generated_json::parse_direct` and `generated_real_typed::parse_*` at
the SURVIVES baseline.

**Checkasm test status.** N/A.

**Same-wave consumer plan.** Each sub-wave commits the refactor for one
grammar PLUS the regen-derived runtime for that grammar, gated by:

- `cargo xtask <regen-cmd>` produces the runtime from the template.
- Round-trip diff: `delete <grammar runtime dir> → cargo xtask
  <regen-cmd> → git diff` is empty.
- The existing bench rows for that grammar (JSON SURVIVES baseline) hold
  within ±1% of pre-refactor Mbps. Regression > 1% rejects the wave.

**Falsifiability gate.**

- `find skinny/crates -name '*.rs' | xargs grep -l
  'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar\|load_json_grammar'`
  returns ZERO files post-redress.
- `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns
  ZERO per-grammar directories (all runtime lives in
  generator-emitted output directories specified by workspace
  metadata).
- `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>'
  skinny/crates/{codegen,runtime,passes,bbnf,grammar}/src/` returns ZERO
  matches in non-generated source.
- Lock 14 baseline gate (`bbnf-bench::lock14_baseline::validate`) passes
  for every grammar.
- **Forward invariant (post-redress, permanent).** Any new grammar
  added under `workspace.metadata.bbnf.grammars.{name}` produces ZERO
  new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf,
  grammar}/src/` and ZERO new directories in
  `crates/core/src/runtime/`. The Lock 14 baseline gate rejects any
  commit that violates this invariant; the gate fires at every
  sub-wave commit and at every future grammar-admission wave.

**LOC budget.** 2.8k – 3.4k source/test envelope across the cluster
(lower bound raised per CH4 V1 to reflect the 64-file refactor + 8
sub-wave structure).
Generated LOC budget separately accounted under
`[generated-size-budget]` feedback. 64-file refactor is delete-then-emit
biased: net source LOC may decrease.

**Risk class.** Very high. Touches generic codegen, generic runtime,
public `bbnf` API surface, decision-engine rule routing, and 8
grammars. The dominant failure mode is hidden coupling between the
provider enum and downstream emit paths (templates assume the variant
exists). The mitigation is the per-grammar sub-wave structure +
round-trip gate per sub-wave + Lock 14 baseline at every sub-wave
commit.

**Pre-blocked routes.** No new public substrate API; no new BIR
variant; no per-grammar registry inside generic crates; no renamed
`RuntimeProvider` (deletion only). The trait-based dispatch lives in
the existing `Provider`/`Grammar` trait set (or a single new trait if
the existing surface cannot carry the dispatch contract).

**Dependencies.** C-3 (regen-css pipeline) is the CSS sub-wave's
generator; C-1's CSS sub-waves admit only when C-3 round-trip gates
green. C-5 is a clean prerequisite — provider deletion is
straightforward only when the CSS row reverts have landed.

## §4 — C-2 — Comparator rebind + per-iteration equality oracle

**R-target.** R1 + R2 per `ORCHESTRATOR-PROMPT.md:98-109`.

**Purpose.** Replace the single
`sonic_rs::from_slice::<sonic_rs::Value>` comparator
(`benches/json_parity.rs:87-102` per `v2-json-validation.md §2`) with
three plane-correct strict comparators:

- `parse_only` → `sonic_rs::Skipper`-style structural skip (or the
  closest extant strict-mode sonic-rs API; if absent, the R8 distinct
  parse_only path's reciprocal comparator becomes the gate).
- `direct` → sonic-rs strict struct deserialization per corpus
  (per-corpus `T` type).
- `typed` → per-corpus typed struct deserialization
  (per-corpus typed `T`).

Add a per-iteration equality oracle that runs INSIDE the timing region,
producing one boolean per iter recorded as an equality-pass column on
the emitted RESULTS row per `PASS-ALPHA.md §4.3`.

**Owner paths.**

- `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` — sonic_rs
  binding site for `parse_only`.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:181-241` —
  direct + typed binding sites.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:412-429` — `sonic_digest`
  + Track 1 binding.
- `skinny/crates/bbnf-bench/src/parity.rs:17-61` — startup-only parity
  oracle (`v6-comparator-integrity.md §4`); add per-iter variant.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` — gate-json column schema +
  per-iter equality-column rejection rule.
- `skinny/crates/bbnf-bench/src/report.rs` — RESULTS schema extension
  per `PASS-ALPHA.md §4.3`.
- `skinny/crates/bbnf-bench/Cargo.toml` — per-corpus struct deps for
  typed deserialization.

**Scalar reference status.** N/A (comparator binding, not SIMD).

**Checkasm test status.** N/A.

**Same-wave consumer plan.** Every named JSON row must:

- Run the rebound comparator on its plane in the same harness commit.
- Emit the per-iter equality column on every bench iter.
- Pass `xtask gate-json --check-results` with the new column required
  in the schema.

The wave is non-admittable if ANY named row's equality column is empty.

**Falsifiability gate.**

- `grep -n 'sonic_rs::from_slice::<sonic_rs::Value>'
  skinny/crates/bbnf-bench/` returns ZERO matches on hot paths.
- `xtask gate-json --check-results` rejects any row whose equality
  column is empty.
- Per-iter equality column appears in every emitted RESULTS row for the
  17 JSON corpora × 3 planes; per-iter ASSERT failures abort the bench
  before Mbps are recorded.
- For each of the 51 JSON rows, the bench reports the rebound
  comparator's strict Mbps; the gap between this and the prior misbound
  Mbps is the SUSPECT → HONEST recalibration documented in α-B.

**LOC budget.** 600 – 1.08k including per-corpus typed structs (mostly
declarations). Ceiling raised by ≈ 80 LOC per CH4 V2 to cover the
in-tree Skipper-class fallback path α-B §316-320 flags: if sonic-rs
v0.5.8 lacks the `Skipper` public API, the wave authors a thin
strict-mode parse_only adapter inside `bbnf-bench/src/` (≈ 80 LOC)
rather than escalate. The adapter is gate-falsifiable against
yyjson/simdjson skip-only as a cross-check oracle.

**Risk class.** High. The dominant failure is sonic-rs API surface:
`Skipper` may not exist as a public API at the current sonic-rs version
(per `v6-comparator-integrity.md §3` "sonic-rs v0.5.8 does NOT expose a
`Skipper` API"). Mitigation: if no strict parse_only comparator exists
in sonic-rs, the gate becomes `simdjson On Demand` skip-only or
yyjson's structural counter; the candidate carries fallback comparator
selection in its plan. The fallback is REJECT if no strict parse_only
comparator exists in any reference library, in which case R8 must land
its own distinct parse_only path AND a reference comparator.

**Pre-blocked routes.** No new comparator-rename of the prior misbound
sonic_rs binding (per pre-block P-2 from α-C); no Track 1 ≡ Track 2
dishonesty (per P-7); no permissive comparator promoted to admit
gate (per addendum A2.3).

**Dependencies.** Standalone; C-2 unblocks every JSON re-admit
candidate downstream (R6 / R7 / R8).

## §5 — C-3 — `cargo xtask regen-css` + production corpora

**R-target.** R4 + R5 per `ORCHESTRATOR-PROMPT.md:128-135`.

**Purpose.** Build a `cargo xtask regen-css` command that consumes the
15 `.bbnf` files at `grammar/css/l4/` and emits CSS L4 runtime modules
through the codegen pipeline. Stand up production corpora at
`skinny/corpora/css-l4-sk-v14/` containing Bootstrap, Tailwind,
Material, and Animate, totalling ≈ 960 KB per the SK-V13 scoping
target (`v1-css-l4-validation.md §1 Claim 4`). (First instance of the
`regen-{grammar}` family; the xtask binary parametrises a
grammar-neutral generator; the generic codegen entry it invokes is the
same surface a future `regen-sheets` / `regen-bbnf-self` /
`regen-{new}` binary will invoke. The CSS instance proves the family
shape; subsequent grammars admit through the same surface without
introducing per-grammar bespoke binaries.)

**Owner paths.**

- `skinny/xtask/src/main.rs:121-127` — existing `regen-json` shape;
  add `regen_css` next to it.
- `skinny/xtask/src/bin/` — new `regen_css` bin if the existing
  structure prefers separate bins.
- `grammar/css/l4/{color, easing, filters, func-body, gradients,
  keyframes, media, properties, selectors, stylesheet, tokens,
  transforms, value-unit, values, keywords}.bbnf` — 15 grammar source
  files confirmed present (`v1-css-l4-validation.md §1 Claim 2
  REFUTE`).
- `skinny/crates/codegen/src/lib.rs` — generic codegen entry consuming
  the 15 grammars; emits `runtime/src/grammars/css_l4_*/` directories.
- `skinny/corpora/css-l4-sk-v14/` — new directory; fetched corpora
  vendored with provenance manifest (origin URL, version pin, license,
  bytes, sha256).
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` — bench harness
  binding to the new corpora.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` — gate-css subcommand
  enforcing the corpus + round-trip preconditions.

**Scalar reference status.** lightningcss 1.0.0-alpha.71 with error
recovery disabled, plus cssparser as independent oracle per
`SK-V13 alpha-E E1` precedent. Lightningcss full AST parse is the
work-equivalent comparator per R6 (no fact-stream vs full-AST
asymmetry).

**Checkasm test status.** N/A unless a CSS row consumes a SIMD kernel
gated by `bbnf-simd`'s grammar-policy gate; that gate is C-1 work and
not C-3's.

**Same-wave consumer plan.** The wave lands:

- The `regen-css` xtask command.
- The 15-grammar emit invocation producing the CSS L4 runtime tree.
- The production corpora vendored at `skinny/corpora/css-l4-sk-v14/`
  with provenance.
- A bench harness wiring at least one CSS L4 row to the new corpora,
  producing a fresh measured baseline (NOT an admit; the row reopens
  for downstream re-admit waves).

The wave is non-admittable if the round-trip fails on ANY of the 15
grammars.

**Falsifiability gate.** Three-part round-trip + recurrence-vector
detector per CH7-1 + CH7-4 binding (V1 CH7 §3.1 REJECT remediation):

- **Round-trip (skinny tree).** `rm -rf
  skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask regen-css
  && git diff -- skinny/crates/runtime/src/grammars/css_l4_*` produces
  empty output.
- **Round-trip (core tree, all rostered grammars).** For each grammar
  name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in the
  top-level `Cargo.toml` — the list is metadata-derived, not
  source-of-truth at the gate site; the canonical shell form is `for g
  in $(cargo metadata --format-version 1 --no-deps | jq -r
  '.metadata.bbnf.grammars[].ident'); do rm -rf
  "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
  diff --exit-code -- "crates/core/src/runtime/${g}/" || exit 1; done`:
  the loop produces empty `git diff` output for every iterated
  grammar. The gate enumerates from `workspace.metadata.bbnf.grammars`
  so that admitting an additional grammar requires NO change to the
  gate's text — only an addition under `workspace.metadata.bbnf.grammars`
  and a `regen-<g>` xtask registration per C-1's forward invariant
  (`alpha-E-candidate-shortlist.md:170-176`).
  This parity is binding: both gates (C-1 forward invariant and C-3
  round-trip) derive grammar enumeration from the same workspace
  metadata clause Lock 14 itself names (`LOCKS.md:220` "workspace
  metadata declaring its strategy"), relocating the forward-blindness
  catch from first-grammar-admission time (C-1) to gate-authoring time
  (C-3). (C-1's sub-wave structure owns the per-grammar xtask
  emission; C-3's round-trip gate consumes those xtasks for CSS and
  asserts byte-equivalence on every other rostered grammar's tree as
  the cross-grammar recurrence-vector check. A hand-patched
  `crates/core/src/runtime/{grammar}/` file fails this gate; the
  Pattern H tarpit `alpha-D.md:486-495` flags collapses to ZERO
  hand-patched files under the gate's enforcement.)
- **Bypass-header detector.** Every file matching `git grep -l
  '@generated by skinny bbnf-codegen' -- skinny/crates/runtime
  crates/core/src/runtime` must be the byte-for-byte output of a
  registered xtask emission; the round-trip succeeds on every such
  file. Files asserting the header outside the registered xtask scope
  are CH7-1 violations and reject the wave. The detector closes the
  audit-confirmed CSS bypass-header pattern
  (`alpha-D.md:185-200` cites the `// @generated by skinny
  bbnf-codegen; do not edit by hand.` header rendered into hand-curated
  content); post-PRUNE no `@generated` header may appear outside a
  registered xtask's emission scope.
- `du -sh skinny/corpora/css-l4-sk-v14` reports > 800 KB.
- `ls skinny/corpora/css-l4-sk-v14/` contains
  `bootstrap.css`, `tailwind.css`, `material.css`, `animate.css` (or
  their version-pinned equivalents) with provenance manifest present.
- At least one CSS L4 bench row in `RESULTS.md` reports a fresh
  measurement against the new corpus + the rebound lightningcss full
  AST comparator.
- For each of the 15 `.bbnf` grammars, the regen-derived runtime
  passes `cargo test --package runtime` for that grammar's emitted
  module.

**LOC budget.** 1.2k – 2.0k: ≈ 400-600 LOC xtask + harness; ≈ 200 LOC
generic codegen wiring (most of the codegen is generic and already
present); ≈ 400-600 LOC corpora vendoring + provenance manifest +
fetch script; ≈ 200 LOC bench harness extension.

**Risk class.** High. The dominant failure modes are (a) the 15
grammars compose semantically into a single CSS L4 surface, but the
current codegen entry expects a single root grammar; the wave must
either author a multi-grammar entry or compose the 15 files into a
single `css_l4.bbnf` root; (b) the production corpora may exercise
grammar productions not covered in the 15 `.bbnf` files (e.g. legacy
vendor prefixes outside `keywords.bbnf`), in which case the
round-trip gate fires on equality, not regen. Mitigation: stage corpus
adoption smallest → largest (Animate → Material → Tailwind →
Bootstrap); reject any corpus that breaks oracle equality on the
existing grammar coverage; the rejected corpus becomes a feature gap
in the parity matrix consumed by R6.

**Pre-blocked routes.** No tiny embedded fixtures < 400 bytes (per
pre-block P-3 from α-C); no fast-fail / token-only sink admit (the
W10.3 overfit at 124× lightningcss per `v1-css-l4-validation.md §1
Claim 5`); no hand-written generated.rs file (P-1).

**Dependencies.** C-1 sub-waves for CSS grammars depend on C-3
landing first — the regen pipeline IS the generator for C-1's CSS
sub-waves.

## §6 — C-4 — W8 per-grammar policy + W9 same-substrate union: scaffold → load-bearing

**R-target.** R3 PRUNE-5 per `ORCHESTRATOR-PROMPT.md:124-126`.

**Purpose.** W8 per-grammar policy and W9 same-substrate union are
SCAFFOLD-ONLY per `v4-decision-engine-trace.md §5-6` and
`SYNTHESIS-AUDIT-OVERFIT.md §Decision-engine fold`: facts are analysed,
CSP solver computes shape selections, but the runtime does not honour
the selections. The hand-written generated paths default to a fixed
shape regardless of the CSP solver's output, which is the upstream
cause of PRUNE-2's recurrence vector (when the solver's shape choice
has no runtime consumer, hand-written templates fill the gap).

C-4 wires the CSP shape selection into the codegen path: every shape
the solver may pick MUST produce a measurable runtime divergence on at
least one named pre-wave row in the wave that lands the wiring.

**Owner paths.**

- `skinny/crates/passes/src/decision_csp.rs:49-81` — 5 CSP constraints +
  variable domain (`v4-decision-engine-trace.md §1`); add shape-binding
  surface readable by codegen.
- `skinny/crates/passes/src/backend_egraph.rs:69-71, 150-174` — extractor
  + Language impl; carry the selected shape into the codegen pipeline.
- `skinny/crates/passes/src/lib.rs:31-65, 400-450` —
  `compile()` + `derive_backend_shape_with_diagnostics()`; bind
  `layout_facts.backend_shape` to the codegen template-selection path.
- `skinny/crates/codegen/src/lib.rs` — template-selection dispatch
  consumes `layout_facts.backend_shape` (post-C-1 refactor; pre-C-1 the
  hand-written providers ignore it).
- `skinny/crates/codegen/src/lower/` — lowering passes consume per-grammar
  policy facts.
- `skinny/crates/runtime/src/tape/{mod,assembler,event_grammar}.rs` —
  same-tape union variants (NOT a new sidecar; per pre-block P-7).
- `skinny/crates/bbnf-bench/src/report.rs` — hot-leaf attribution column
  per `PASS-ALPHA.md §4.3` reflects the chosen shape's symbol path.

**Per-shape Lock-1 triad declaration (mandatory; CH5 §2 REJECT
remediation, V1 hardening).** The same-wave consumer plan adds a
`substrate_target=existing_tape | retention_lifetime=generated_function
| policy_owner=generated_grammar` triple as a REQUIRED column in the
wave's REDRESS entry for every CSP-selectable shape the wiring
exercises. Allowed values per `LOCKS.md:73-82`: `substrate_target` ∈
{`local_temp_only`, `existing_tape`, `direct_sink`,
`admitted_fact_output`}; `retention_lifetime` ∈ {`local_loop`,
`generated_function`, `output_row`}; `policy_owner` ∈
{`generated_grammar`, `caller_data`, `none`}. Any shape whose triple
cannot be declared at wave-plan time abrogates per
`[abrogate-before-patch]` and falls under C-4's architectural-block
escalation path (§11 below, lines reading "If C-4 cannot demonstrate
hot-leaf attribution change on any named row…"). `xtask gate-json`
rejects any REDRESS row that lacks the triple; the rejection wires
through the same gate path that rejects empty equality columns from
C-2. The triad discipline applies per CSP-selectable shape, not per
wave aggregate — a wave admitting two shapes must declare two triples.

**Scalar reference status.** Each per-grammar policy MUST have a scalar
reference implementation in `bbnf-simd/src/scalar/` if a SIMD kernel is
selected by the policy. Existing SURVIVES set covers JSON; new policies
for CSS or other grammars require scalar references per Lock 16
allowlist discipline.

**Checkasm test status.** Per-policy checkasm green if SIMD selected;
N/A otherwise. The grammar-policy gate prerequisite from `SK-V13
alpha-E E5` (`G-SIMD-GRAMMAR-POLICY`) is binding here: any wave wiring
`bbnf-simd` into CSS, union, JSON parse_only, or shared generated code
must pass the policy gate per Lock 14.

**Same-wave consumer plan.** The wave lands:

- Codegen pipeline reads `layout_facts.backend_shape` and emits the
  matching template (post-C-1 trait-based dispatch).
- At least ONE named pre-wave row's hot leaf attribution differs from
  its pre-wave value — proof of runtime divergence.
- The CSP-chosen shape's hot path consumes the selected primitive (or
  the selected layout / sink) in same-commit code, NOT a future
  promise.
- **Post-wave hot-leaf module-path discipline (CH5 §2 REVISE #5, V1
  hardening).** The post-wave hot leaf's module path traces to
  `runtime::tape::` (an existing same-tape variant) or to a
  generator-emitted module whose template provenance is named in
  REDRESS; module paths under `runtime::ext::`,
  `runtime::sidecar::`, `runtime::union::`, or `runtime::cursor::`
  are REJECT pre-emptively. The discipline closes the
  renamed-scanner-with-shared-buffer slip the CH5 charge names: a
  symbol shift alone is not proof of decoupling; the module-path
  trace must terminate at the sanctioned tape surface or at a
  named generator template.

A wave that ships the W8 / W9 wiring without a row-level hot-leaf
attribution change is REJECT (proves the wiring is still cosmetic).

**Falsifiability gate.**

- **Pre-wave row binding (CH1 REVISE-3 + CH6 REV-1, V1 hardening):**
  the named pre-wave row is `json/numbers/direct_to_struct/main`. The
  pre-wave hot-leaf citation binds to one of: (a) `RESULTS.md`
  Hot-leaf column for `json/numbers/direct_to_struct/main` at HEAD
  reading `parse_value_at`, or (b) `v2-json-validation.md §3.1`
  numeric-array dispatch trace + the W11.1 commit SHA. Either
  anchor makes the pre-wave baseline binary and the post-wave
  assertion mechanically verifiable.
- **Post-wave symbol naming.** If W8 chooses a number-specialised
  shape, the samply trace's hot leaf names the W11.1-emitted
  number-specialised symbol explicitly (e.g.
  `parse_number_array_specialised` or the exact symbol the W11.1
  emit produces — the wave plan names the symbol at S-P3 dispatch
  time, not after redress). Hot leaf unchanged ⇒ REJECT. Hot leaf
  changed but module path under `runtime::ext::` /
  `runtime::sidecar::` / `runtime::union::` / `runtime::cursor::`
  ⇒ REJECT pre-emptively per the consumer-plan discipline above.
- `xtask gate-json` reports the hot-leaf column per row; the column
  value changes for the named pre-wave row.
- `decision_csp` solve emits a CSP trace recording the chosen shape per
  rule; the trace ships in the same commit as the runtime change.
- Lock 14 baseline gate stays green (no grammar-name leak in the new
  wiring).

**LOC budget.** 800 – 1.4k: ≈ 400 LOC codegen template-selection
dispatch; ≈ 200 LOC CSP-to-codegen binding surface; ≈ 200-400 LOC
runtime path for the chosen shape; ≈ 200-400 LOC tests + telemetry.

**Risk class.** Very high. The CSP solver's shape choice may select a
shape whose runtime path is not yet implemented, in which case the
wave's hot-leaf attribution does not change — the wave looks REJECT
when the issue is implementation, not wiring. Mitigation: scope the
wave to one CSP-selectable shape that already has a runtime path
(the numeric-array dispatch from W11.1 per `v2-json-validation.md
§3.1`), prove the wiring fires for that shape, then expand. Also at
risk: the wiring may regress an existing JSON SURVIVES row (e.g. by
selecting a slower shape for a row the hand-written path optimised
for); regression > 1% on any SURVIVES row rejects the wave.

**Pre-blocked routes.** No new substrate API (per Lock 14 + pre-block
P-5); no parallel sidecar producer (P-7); no new BIR variant (per
ORCHESTRATOR.md §8 non-negotiables); no W8/W9 admit on a research-only
basis (per pre-block P-5 "scaffold-research counted as load-bearing").
**No grammar-branched dispatch inside the CSP shape consumer (CH2
Finding 3, V1 hardening).** The shape consumer in
`skinny/crates/codegen/src/lib.rs` MUST dispatch on the CSP-emitted
`BackendShape` enum alone; no `match grammar { Json => ..., CssL4 =>
... }` arm may appear in the dispatch path. A grammar-branched arm in
the consumer is a Lock 14 violation surfacing as a CSP-bypass and
rejects the wave at S-P3. **Two-grammar-family exercise requirement
(CH2 Finding 3, V1 hardening).** The C-4 shape consumer must be
exercised across at least two grammar families before any C-4 admit
cites runtime divergence as load-bearing; one-grammar runtime
divergence is wave evidence, not admit evidence. The two families
satisfy the polymorphism check the CSP-emitted dispatch promises;
single-family divergence may still be a grammar-bespoke artefact.

**Dependencies.** C-1 must land first (trait-based dispatch is the
shape-consumer surface; the strict-serialisation reading is
authoritative — C-4 may NOT parallelise with any C-1 JSON sub-wave per
CH4 V3 V1 hardening; §9 matrix updates accordingly below). C-2 must
land first (the per-iteration equality oracle proves the new shape
produces correct output). C-3 may land in parallel (CSS regen does not
collide with CSP wiring).

## §7 — C-5 — Clean revert of W14.1–W14.5 + CSS template deletion + 24-row revert

**R-target.** R3 PRUNE-1 + PRUNE-2 per `ORCHESTRATOR-PROMPT.md:112-114`.

**Purpose.** Restore the honest baseline to the ledgers. Revert
W14.1–W14.5 in `RESULTS.md` and `ROLLING-SOTA-DELTA.md` (5 JSON
parse_only admits per `v2-json-validation.md §1` — gate-relabel only).
Delete the 7 CSS hand-written template directories
(`skinny/crates/codegen/src/css_l4_*_templates/`) and the 7 generated.rs
files in `skinny/crates/runtime/src/grammars/css_l4_*/` that are
include-str'd from them. Revert the 24 CSS L4 admitted rows in
`ROLLING-SOTA-DELTA.md` to OPEN. Add a REDRESS entry per row citing the
relevant validation §reference.

**Owner paths.**

- `skinny/RESULTS.md` — revert W14.1–W14.5 + 24 CSS L4 admits to OPEN.
- `restart/skinny/ROLLING-SOTA-DELTA.md` — replay table with the
  audit-zero deltas (JSON parse_only 0/17, JSON direct 0/17, JSON typed
  0/17, CSS L4 0/24).
- `skinny/REDRESS.md` — append **29 row-keyed REDRESS entries** (one
  entry per reverted row, naming the row key + the validation
  §reference), partitioned as 5 W14 row keys + 23 SK-V13 CSS row keys
  + 1 SK-V12 W1b row key:
  - W14.1 (numbers) cites `v2-json-validation.md §1 W14.1`.
  - W14.2 (citm_catalog) cites `v2-json-validation.md §1 W14.2`.
  - W14.3 (canada) cites `v2-json-validation.md §1 W14.3`.
  - W14.4 (marine_ik) cites `v2-json-validation.md §1 W14.4`.
  - W14.5 (mesh) cites `v2-json-validation.md §1 W14.5`.
  - 23 SK-V13 CSS row keys cite `v1-css-l4-validation.md §§1-6` as
    appropriate per row (declaration_values + 22 others across the
    sk-v13 CSS admit set).
  - 1 SK-V12 W1b row key (`declaration_values` cross-tranche stability
    revert) cites `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL`.
- `skinny/crates/codegen/src/{css_l4_at_rules_and_media,
  css_l4_declaration_values, css_l4_declaration_values_extended,
  css_l4_nested_layout, css_l4_stylesheet_selectors,
  css_l4_vendor_and_custom_atrules, css_l4_visual_functions}_templates/`
  — 7 template directories to delete.
- `skinny/crates/codegen/src/{css_l4_*}_provider.rs` — 7 provider files
  to delete (C-1 also targets these; coordination per §9).
- `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` — 7
  generated.rs files to delete.
- `skinny/crates/codegen/src/lib.rs:1-10, 165-210` — remove 7
  `mod css_l4_*_provider;` + the 7 provider invocations.
- `skinny/crates/runtime/src/lib.rs:3-44` — remove 7 hardcoded CSS
  `#[path = ...]` declarations + namespace entries (the namespace
  reorganisation itself belongs to C-1; C-5 only removes the CSS
  entries).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (drafted by α-F) — §0
  goalset baseline reflects 0 / 17 / 0 / 17 / 0 / 17 / 0 / 24.

**Scalar reference status.** N/A (revert + deletion).

**Checkasm test status.** N/A.

**Same-wave consumer plan.** Single commit set:

- Source deletions land together with the ledger reverts and the
  REDRESS entries.
- `cargo build --release` passes after the deletions (no orphan
  references to the deleted providers).
- `xtask gate-json` (and `gate-css` once C-3 lands) reports the
  audit-zero deltas, matching `ROLLING-SOTA-DELTA.md`.

**Falsifiability gate.**

- `git grep -l '@generated'
  skinny/crates/runtime/src/grammars/css_l4_*` returns ZERO files.
- `git grep -l 'include_str!'
  skinny/crates/codegen/src/css_l4_*_provider.rs` returns ZERO files
  (the provider files themselves are deleted).
- `ls skinny/crates/codegen/src/css_l4_*_templates/` returns ZERO
  directories.
- `ROLLING-SOTA-DELTA.md` shows CSS L4 0/24 and JSON parse_only 0/17.
- `skinny/REDRESS.md` carries 29 new row-keyed entries (5 W14 row keys
  + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key) with validation
  §refs per the owner-paths block above.
- `cargo build --release --workspace` succeeds.

**LOC budget.** 250 – 500 (delete-heavy + ledger edits + REDRESS
entries). Generated LOC envelope drops by ≈ 5–8k (the 7 deleted
`generated.rs` templates).

**Risk class.** Medium. The dominant risk is ledger drift: the
audit-zero deltas must match across `RESULTS.md`,
`ROLLING-SOTA-DELTA.md`, and the α-A artefact. Mitigation: α-A is the
authoritative restatement; α-E's C-5 cites α-A's per-row baseline.

**Pre-blocked routes.** Per pre-block P-1, no new hand-written
template with `@generated` header may land in any future SK tranche.
Per pre-block P-4, no future admit may move S/NO-GO → A/GO by
gate-relabel only; every admit must show parser source diff.

**Dependencies.** C-5 is a clean prerequisite for C-1's CSS sub-waves
(the providers can be deleted only after the CSS rows are reverted).
C-5 may land in parallel with C-2 (comparator rebind does not collide
with revert ledger edits).

## §8 — Pre-blocked routes (consolidated)

Carried from α-C's pattern-level pre-blocks (binding on every candidate):

- **P-1** Fake `@generated` header on hand-written templates
  (`v1-css-l4-validation.md`). Candidate slate must not introduce a new
  hand-written file claiming generator provenance.
- **P-2** `sonic_rs::from_slice::<Value>` mislabelled as strict
  comparator (`v6-comparator-integrity.md`). C-2 is the rebind; no
  other candidate may use the misbound binding.
- **P-3** Tiny-fixture Criterion-overhead Mbps inflation, < 400 bytes
  (`v1-css-l4-validation.md §6`). C-3 is the corpora replacement; no
  CSS row admits on a < 400-byte fixture in SK-V14.
- **P-4** Gate-relabel as admit (`v2-json-validation.md §§1-4`). C-5 is
  the revert; no future admit may move state without parser source
  diff.
- **P-5** Scaffold-research counted as load-bearing
  (`v4-decision-engine-trace.md`). C-4 is the load-bearing wiring; no
  W8/W9 admit on research-only basis.
- **P-6** Per-grammar provider modules in generic codegen
  (`v3-lock14-deep-scan.md`). C-1 collapses the 8 provider modules; no
  re-introduction permitted.
- **P-7** Track-1 ≡ Track-2 dishonesty; parallel substrate / sidecar
  producer. C-4 same-tape union variants only; no new sidecar.

## §9 — Concurrency + serialisation matrix

| Candidate | May parallelise with | Must serialise after |
|---|---|---|
| C-5 | C-2, C-3 (disjoint file domains) | (none — runs first) |
| C-2 | C-3, C-5 | (none) |
| C-3 | C-2, C-5 | (none) |
| C-1 (JSON sub-waves) | (one sub-wave at a time) | C-2, C-5 |
| C-1 (CSS sub-waves) | (one CSS sub-wave at a time) | C-3, C-5 |
| C-4 | (one shape at a time) | C-1 (ALL sub-waves), C-2 |

Land order: C-5 + C-2 + C-3 in parallel as Wave Zero. C-1 sub-waves
after C-5 and C-2 land; CSS sub-waves additionally after C-3. **C-4
strictly serialises after C-1 completes ALL sub-waves** — both JSON
and CSS — per CH4 V3 V1 hardening. The §6 C-4 dependency declaration
("C-1 must land first") is authoritative; an earlier matrix row read
C-1 JSON sub-waves as parallelisable with C-4 on the assumption that
post-C-1 generic surface admits C-4 wiring; the trait-based dispatch
+ no-grammar-branch discipline (E-8) and the per-shape Lock-1 triad
(E-3) require the entire C-1 collapse to be complete before C-4
exercises any shape, on either grammar family. Single-shape C-4
wiring against a partially-collapsed C-1 surface is REJECT.

`RESULTS.md`, `ROLLING-SOTA-DELTA.md`, and `REDRESS.md` are
single-writer ledgers per the SK-V13 alpha-E concurrency matrix; all
ledger writes serialise even when redress worktrees run in parallel.

## §10 — Cost, caps, telemetry

Hard caps per `ORCHESTRATOR-PROMPT.md §DISCIPLINE` +
`USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:129-134` (the 30→45-min
redress uplift is bound to the W5–W9 decision-engine fold and the W12
union-SIMD wave; non-decision-engine waves default to the 30-min cap):

| Candidate / wave | Research | Plan | Redress |
|---|---:|---:|---:|
| C-1 sub-waves (8 grammars; per sub-wave) | 20 min | 15 min | 30 min |
| C-2 | 20 min | 15 min | 30 min |
| C-3 | 20 min | 15 min | 30 min |
| C-4 (per CSP-selectable shape) | 20 min | 15 min | 45 min |
| C-5 | 20 min | 15 min | 30 min |

**Cap discipline reconciliation (CH4 R3, V1 hardening).** Only C-4
inherits the addendum's 45-min redress amendment — C-4 IS the W8 + W9
decision-engine fold wiring the addendum names. C-1, C-2, C-3, C-5
are not decision-engine waves and default to the 30-min cap. The C-4
45-min cap is **per CSP-selectable shape** (per CH7 §3.3
clarification), not per cluster — the C-4 cluster total is bounded by
the number of CSP-selectable shapes the wave addresses (one shape per
sub-wave), with each shape's redress windowed at 45 min. The C-1
cluster total is 8 × 30 = 240 min of redress windows, run serialised
per §9; the C-4 cluster total is N × 45 min where N is the number of
shapes the wiring exercises (≥ 2 per E-8's two-grammar-family
requirement).

Telemetry per `PASS-ALPHA.md §4.3` (column set unchanged); per-iter
equality column added by C-2; hot-leaf attribution column required for
every row per C-4's gate. **The hot-leaf column reads as a
grammar-keyed symbol path** (`{grammar}::parse_*` or equivalent); a
stale inherited symbol name on a non-JSON row (e.g. a JSON-keyed
symbol surfacing as the hot leaf for a CSS row) fails the per-row
gate the same way it fails S-P1 (CH2 Finding 4, V1 hardening). The
grammar-keying closes the under-specification where a row's
attribution column could carry a generator-emitted symbol whose
template provenance is from a different grammar — the gate now
detects this mechanically through the column's `{grammar}::` prefix.

## §11 — Convergence + escalation

The slate converges per `ORCHESTRATOR.md §3Z`: ≥ 95% ACCEPT on
CHALLENGE for two consecutive cycles, zero open critical defects, no
orphan REVISE, user G-Alpha sign-off.

Escalation paths:

- If C-2's rebind discovers no strict parse_only comparator exists in
  any reference library (sonic-rs, simdjson, yyjson), escalate to user
  per `ORCHESTRATOR-PROMPT.md §ESCALATE` ("Comparator rebind discovers
  a new misnaming pattern") — the addendum's A3 `parse_only` admission
  rule becomes architecturally constrained, not implementation-blocked.
- If C-3's round-trip cannot regen one of the 15 `.bbnf` files (e.g.
  grammar surface incompatible with current codegen), escalate as
  architectural-level intrinsic block for the affected CSS feature
  family.
- If C-4 cannot demonstrate hot-leaf attribution change on any named
  row, the wave is REJECT — W8 / W9 wiring is implementation-blocked;
  reroute via abrogate-before-patch per `[abrogate-before-patch]`.
- If C-1's sub-waves regress > 1% on any SURVIVES row, halt the
  affected sub-wave; the regression is the architectural intrinsic
  block for the refactor path; reroute via per-grammar carve-out per
  `[no-workarounds-arch]` requires architectural transposition rather
  than workaround.

The candidate slate stands or falls together: if any one of C-1
through C-5 cannot land, the SK-V14 wave program cannot dispatch its
re-admit waves (R6 / R7 / R8), and the bracket re-opens to SK-V15.
