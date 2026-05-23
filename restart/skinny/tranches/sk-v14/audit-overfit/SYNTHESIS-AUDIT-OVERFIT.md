# SK-V14 S-P0 — Overfit-Audit Synthesis

Authority: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §Procedure`
step 1 + §Outputs; six axis files at `restart/skinny/tranches/sk-v14/
audit-overfit/sk-v14-audit-overfit-{css-measurement, admit-mechanism,
lock14-scan, generator-truth, decision-engine, pre-restart-pattern}.md`
written under the SK-V14 S-P0 dispatch context.

The synthesis aggregator owns the only commit for the S-P0 axis-phase
plus this file, per the institutionalized write-only + aggregator-
commit pattern from Pass Alpha V2-V5.

## §0 — Cross-axis verdict

### §0.1 Per-axis disposition table

| Axis | Lens | CRIT | HIGH | MED | LOW | Total | Verdict | Confirms V13 | New vs V13 |
| --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
| A1 | Measurement integrity (corpus, distinctness, plausibility, same-run comparator) | 4 | 2 | 2 | 0 | 8 | **FAIL** | YES (byte-identical) | 0 |
| A2 | Admit-mechanism integrity (source-diff, comparator binding, per-iter oracle) | 4 | 3 | 1 | 1 | 9 | **FAIL** | YES (full) | 2 (F8 structural; F9 negative-drift) |
| A3 | Lock 14 generic-crate scan | 11 | 7 | 5 | 7 | 30 | **FAIL** | YES (verbatim v3 reproduction) | 1 DELTA-NOTE (D1) |
| A4 | Generator-vs-hand-curated | 9 | 4 | 2 | 1 | 16 | **FAIL** | YES + extends | 3 (JSON @generated fake; CSS scanners as fixture lookups; 14/15 .bbnf orphan) |
| A5 | Decision-engine fold integrity | 0 | 2 | 1 | 1 | 4 | **PARTIAL PASS** (resolver clause PASS; scaffold-clause FAIL at v13 close + at v14 HEAD; PASS conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing) | YES | 1 (gate-layer-only footprint quantified) |
| A6 | Pre-restart pattern recurrence | 3 | 2 | 1 | 1 | 7 | **FAIL** | YES + extends | 4 (LegacyPath shim; substrate-doc opt-out enshrinement; pre-restart-API carry; asm bibliographic) |
| **Aggregate** | — | **31** | **20** | **12** | **11** | **74** | **FAIL** | — | **20 NEW** |

### §0.2 Aggregate verdict

**S-P0 V1 verdict: FAIL.**

Five of six axes (A1, A2, A3, A4, A6) FAIL outright on their pass
criteria. A5 returns a PARTIAL PASS — the W5/W6/W7 resolver clause is
PASS (CSP+egraph+cost wired to lowering via five fail-closed checks at
`crates/codegen/src/lower/rust.rs:37-89`), but the no-scaffold-only-
admit clause failed at SK-V13 close and remains FAIL at SK-V14 HEAD;
the C-5 (PRUNE-1 + PRUNE-2) revert is the gating wave that converts
FAIL → PASS by removing every scaffold-citing row (W14.1-5, W13.1-4,
W15.1), and no row admit may cite W8 / W9 until C-4 (PRUNE-5) wires
them load-bearing. The audit-corrected target in `tranches/sk-v14/
SYNTHESIS.md §0.2` reads `0/17 / 0/17 / 0/17 / 0/24` for parse_only /
direct / typed / CSS L4 post-PRUNE; the present-state at HEAD still
carries the W14.1-5 + W13.* + W15.1 admit rows + 24 CSS L4 ADMITTED
rows.

The aggregate count of 31 CRITICAL + 20 HIGH violations triggers the
`PASS-0-OVERFIT-AUDIT.md §Failure mode` clause:

> If S-P0 finds CRITICAL violations the campaign halts forward motion
> until the prune waves complete. The tranche's behavior waves do not
> dispatch until the prune list converges.

The verdict is **expected** and consistent with the SK-V14 contract,
which already encodes the audit-zero baseline (HANDOFF §3) and
sequences C-5 (PRUNE-1 + PRUNE-2 revert) and C-1 (PRUNE-3 + PRUNE-4
Lock-14 refactor) before any new-admit wave (R6, R7, R8). S-P0
confirms the SK-V13 audit pack still holds end-to-end at SK-V14
starting state at HEAD `12ff0744e`; the SK-V14 contract reads the
disposition correctly.

## §1 — Delta vs SK-V13 audit pack

### §1.1 Confirm-vs-NEW census

Across the six per-axis files, 54 of 74 findings (73 %) CONFIRM the
SK-V13 audit pack byte-for-byte; 20 of 74 are NEW (27 %) on per-row
count (per-axis table column-sum 8+7+29+4+3+3 = 54 CONFIRMS;
0+2+1+12+1+4 = 20 NEW). The per-category count is 11 NEW conceptual
clusters (enumerated §1.2 below) — every NEW row maps to one of
those 11 categories. The CONFIRMS ratio is itself a finding: the
SK-V14 starting baseline at HEAD `12ff0744e` reproduces the SK-V13
close-state pathologies verbatim because zero SK-V14 implementation
commits have landed; the 17 doc / synthesis commits between
`00181742e` (SK-V14 contract close) and `12ff0744e` (S-P0 dispatch
seed) touched no parser, codegen, runtime, or grammar bytes.

| Axis | CONFIRMS | NEW | NEW finding cluster |
| --- | ---: | ---: | --- |
| A1 | 8 | 0 | (none — measurement audit reduces to byte-identical confirmation at SK-V14 no-implementation-yet posture) |
| A2 | 7 | 2 | F8 (single-lane comparator fanout as **structural cause**, not per-row symptom); F9 (negative-drift confirmation — no admit row since `7ec4a474c` W15.1) |
| A3 | 29 | 1 | D1 (parse-that-regex `StringFlags::HAS_ESC` JSON-flavored naming — DELTA-NOTE, future-rename concern, not a new violation) |
| A4 | 4 | 12 | NEW-1 (JSON `generated.rs` ALSO ships fake `@generated` header — codegen-side `json_provider::normalize` mirrors the CSS pattern); NEW-2 (3 of 7 CSS scanners short-circuit on `CANONICAL_FIXTURE`/`CAPTURED_W2_INPUT` byte-equality → fixture-lookup tables dressed as parsers); NEW-3 (14 of 15 `.bbnf` files at `grammar/css/l4/` orphaned — only `stylesheet.bbnf` is cited by the totality `Cargo.toml:22`, none by skinny); plus 9 quantification extensions of V13 findings |
| A5 | 3 | 1 | NEW-MED (gate-layer-only footprint quantified at 3 files / 20 references for `per_grammar_policy` / `same_substrate_union` / `GrammarConfig` — `bbnf-bench/src/{bin/gate.rs, lock14_baseline.rs, report.rs}` only; zero matches in `passes/`, `codegen/`, `runtime/`, `ir/`) |
| A6 | 3 | 4 | NEW-HIGH-1 (`LegacyPath` rename shims aliased at `use` site in 4 `parse_with.rs` files: `crates/core/src/runtime/{json, css_l4, bbnf, google_sheets}/parse_with.rs`); NEW-HIGH-2 (`builder_template.rs` / `arena_template.rs` doc-comments enshrine Pattern H hot-grammar opt-out as **design-of-record**); NEW-MED (pre-restart-API behaviour carry in `google_sheets/document/canonical.rs:13-17`); NEW-LOW (asm bibliographic asmjson citation in `bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm:13,16`) |

### §1.2 NEW finding enumeration (binding inputs to S-P3 wave manifest)

The 11 NEW finding *categories* (= 20 NEW *rows* per-axis) extending
the V13 audit pack:

1. **A2 F8 (MED, structural)** — Single `sonic_rs_anchor` lane at
   `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` is the
   strict comparator for all three JSON planes (parse_only, direct,
   typed) simultaneously. The per-corpus typed bindings at
   `…/real_typed_struct.rs:695-727` exist but are wired only into the
   *parity assertion* (startup-only, line 17-26), not the *anchor row*
   (admit-gate). V13 v6 noted the per-row symptom; F8 captures the
   structural cause. R1 closes this at the harness layer.

2. **A2 F9 (LOW, negative confirmation)** — Zero admit-row commits to
   `ROLLING-SOTA-DELTA.md` since `7ec4a474c` W15.1 on 2026-05-22 14:28;
   17 post-audit commits exhausted by SK-V14 alpha brackets + audit-
   overfit seed. The baseline freeze is verified.

3. **A3 D1 (NOTE, not a violation)** — `parse-that-regex`'s
   `StringFlags::HAS_ESC` / `HAS_CONTROL` / `NEEDS_DECODE` retain
   JSON-flavored naming at `skinny/crates/parse-that-regex/src/lib.rs:
   56-60, 179, 254, 262`. Bit semantics are grammar-neutral (any
   quoted-string grammar populates them) — the v3 audit classified the
   crate CLEAN. Future-rename concern; carries to PRUNE-3 cosmetic
   rename or documentary-only.

4. **A4 NEW-1 (CRIT, extension of V13 CSS-only finding to JSON)** —
   `json_provider::normalize` at `skinny/crates/codegen/src/json_
   provider.rs:85-99` mirrors the CSS fake-`@generated` pattern: 5 of
   6 emitted files (`generated.rs`, `parser.rs`, `view.rs`, `value.rs`,
   `config.rs`) are `include_str!("json_templates/<X>.rs")` pass-
   through with only the prepended header line added. Only the trailing
   `json_sink_direct::render(sink_only)` chunk inside `generated.rs` is
   grammar-derived (~15 % of the file). The hand-written / grammar-
   derived ratio in `generated.rs` is ~85 % / ~15 %.

5. **A4 NEW-2 (CRIT, 4 findings: rows 3, 4, 5, 6 in A4 §2)** — Four
   of the seven CSS L4 template generators (`nested_layout`,
   `at_rules_and_media`, `stylesheet_selectors`, `vendor_and_custom_
   atrules`) are fixture-lookup tables (3 CSS L4 `CANONICAL_FIXTURE`
   short-circuits + 1 `CAPTURED_W2_INPUT` short-circuit; verified by
   `grep -nE 'CANONICAL_FIXTURE|CAPTURED_W2_INPUT' skinny/crates/
   codegen/src/css_l4_*_templates/generated.rs`). `css_l4_nested_layout_
   templates/generated.rs` is 49 lines total: `if input ==
   CANONICAL_FIXTURE { return Ok(CANONICAL_FACTS.to_string()); } …
   sink.unsupported(0)`. The fixture bytes are the exact 85-357 B
   research representatives the SK-V13 W10.* / W2 ADMITTED rows claim
   to parse at parity. Hash-table lookup dressed as parser; outside
   the V13 audit pack's per-finding enumeration.

6. **A4 NEW-3 (CRIT, finding 7)** — Zero file-system references to
   `grammar/css/l4/*.bbnf` exist anywhere inside `skinny/` or root
   `xtask/`. Only `Cargo.toml:22` (totality-track root) cites
   `grammar/css/l4/stylesheet.bbnf` for the `css_l4` ident — orthogonal
   to the skinny providers under audit. **14 of 15 `.bbnf` files are
   orphaned from both tracks.** No `regen-css*` subcommand exists in
   `skinny/xtask/src/main.rs:8` (subcommand list: `<regen-json | check-
   json | regen-real-typed | check-real-typed | check-conformance |
   lint-loc | bench-json | gate-json | primitive-checkasm>`).

7. **A5 NEW-MED (MED, extension of V13 SCAFFOLD-ONLY verdict)** —
   `per_grammar_policy` / `same_substrate_union` / `GrammarConfig`
   identifiers exist in only **3 files** across `skinny/crates/`:
   `bbnf-bench/src/bin/gate.rs` (14 hits — entry validators, path
   routers, tests), `bbnf-bench/src/lock14_baseline.rs` (2 hits inside
   W1a admit-string literals), `bbnf-bench/src/report.rs` (4 hits —
   schema validator, test wiring). **Zero matches** in `passes/`,
   `codegen/`, `runtime/`, `ir/`. The W8 / W9 SCAFFOLD-ONLY surface is
   not merely absent from runtime — it is report-bound only, gating
   compliance reports without ever reaching the compile / lower path.

8. **A6 NEW-HIGH-1 (HIGH, backwards-compat shim)** — Every grammar
   `parse_with.rs` aliases the older `Path` / `PathSegment` from
   `crates/core/src/runtime/path.rs` to `LegacyPath` / `LegacySegment`
   at the `use` site, then lowers the newer `TypedSegment` onto
   `LegacySegment` inside `lower(...)` before invoking the document
   accessor: `crates/core/src/runtime/{json, css_l4, bbnf, google_
   sheets}/parse_with.rs` (4 files; identical pattern). The rename is
   a bridge between two co-existing path representations; one is
   transitional. This is a backwards-compat shim by every plain reading
   of the term.

9. **A6 NEW-HIGH-2 (HIGH, design-of-record)** — `crates/core/src/
   runtime/builder_template.rs:13-31` documents that JSON, CSS L4, and
   BBNF "Distinct shape → distinct module (no template instantiation)"
   and ship per-grammar bodies. `arena_template.rs:1-31` documents the
   same shape: 5 grammars instantiate the template, the rest are
   hand-written. **The substrate itself enshrines the Pattern H hot-
   grammar opt-out as design-of-record.** A6 cannot read CLEAN until
   either the hot-grammar bodies become genuine codegen output, or the
   substrate doc is rewritten with a deletion plan.

10. **A6 NEW-MED (MED, pre-restart-API carry)** — `crates/core/src/
    runtime/google_sheets/document/canonical.rs:13-17` documents that
    "Pre-W2-act this surface lived as `GoogleSheetsParser::serialize_
    compact(node)` against the cursor-backed `tape::TapeCursor`; that
    emitter retired alongside the tape substrate when the struct-direct
    flip activated." A re-implementation of a pre-restart surface
    inside the current per-grammar runtime; original tape-based,
    current struct-tree-based, API surface preserved. Lock-14-adjacent
    but properly scoped to `runtime/google_sheets/`.

11. **A6 NEW-LOW (LOW, bibliographic)** — `skinny/crates/bbnf-simd/src/
    x86_64/byte_class_from_eq_set_64.asm:13,16` cites the "asmjson
    (Lemire et al.)" SOTA paper in asm comments. Not code coupling;
    flagged only so the synthesis can confirm A3 already disposed of
    it.

### §1.3 Pattern H file count: 64 → 67

The SK-V13 audit pack body cited 64 hand-written per-grammar runtime
files in `crates/core/src/runtime/`. A6 §1 re-runs the count at SK-V14
starting state:

```
$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9   (bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math)

$ find crates/core/src/runtime -name '*.rs' \( -path '*/json/*' -o
  -path '*/css_l4/*' -o … -path '*/math/*' \) | wc -l
60   (omits */css_pretty/* per the V13 brief's expression)

Per-grammar file census:
  bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7,
  google_sheets=10, json=7, math=7
  Total = 67 hand-written per-grammar runtime files
```

A3 §1 and A5 §2.1 cross-confirm the 9-directory count (one over the
SK-V13 baseline's 8 because `css_pretty` was added). Pattern H
expanded **64 → 67** (+3 LOC delta from the `css_pretty` grammar). The
A3 / A4 / A6 enumeration is consistent: PRUNE-4 has 9 sub-waves, not
8, and the wave manifest must reflect this.

**Co-derivation note (binding for S-P3 risk-weighting):** The +3 file
delta (64 → 67) and the PRUNE-4 sub-wave count delta (8 → 9) are both
attributable to the single `css_pretty` grammar addition between V13
audit-pack landing and SK-V14 baseline; A3 §1, A5 §2.1, and A6 §1
independently re-derive the count from the same evidence, so the three
confirmations are evidentially **co-derived, not orthogonal**. The
S-P3 wave manifest's risk-weighting for PRUNE-4 should treat the
`css_pretty` delta as one piece of evidence with three cross-checks,
not three independent regression signals. The R4-before-PRUNE-2
sequencing constraint (§2.1) is similarly co-derived with the +3 /
+1 deltas via the `css_pretty` directory addition; A3/A5/A6 cross-
confirms are co-derived, not three independent confirmations.

## §2 — Architectural sequencing constraints (S-P3 inputs)

The synthesis surfaces three hard sequencing constraints binding on
the S-P3 wave manifest plus a CH7-companion gating recommendation. All
four extend the SK-V14 SYNTHESIS §4 S-P3 constraints already in force
under the durable contract.

### §2.1 R4 (regen-css xtask) MUST land BEFORE PRUNE-2

Per A4 §4 (`recommended prune actions` table, R4 row): without R4
landing first, PRUNE-2 deletes the 7 hand-written CSS template
directories + 7 provider modules + 7 runtime twins but leaves the 24
CSS L4 ADMITTED rows **permanently unrecoverable** — the deletion
removes the only emission path, and no `cargo xtask regen-css`
subcommand exists to regenerate the runtime files from `grammar/css/
l4/*.bbnf`.

Quoting A4 §4 verbatim:

> CONFIRM. Without R4, PRUNE-2 deletes the providers but leaves the 7
> CSS ADMITTED rows unrecoverable; the order must be `R4 lands first →
> PRUNE-2 deletes hand-written twins`.

The 7 CSS rows in question are W2 / W3 / W4 / W10.1 / W10.2 / W10.3 /
W1b (per A1 §1.5 cluster table). After R4 lands and emits byte-
deterministic `generated.rs` from `.bbnf`, PRUNE-2 deletion is
recoverable; before R4 it is not.

### §2.2 C-1 (PRUNE-3 + PRUNE-4) MUST land BEFORE C-4 (PRUNE-5 W8/W9 wiring)

Per A5 §4.1 (sequencing constraint, verbatim):

> Per SYNTHESIS §0.4 P-5 + §4 "any SPEC wave that wires `bbnf-simd` …
> carries `G-SIMD-GRAMMAR-POLICY`" and `[no-deferrals]`: C-4 (PRUNE-5)
> must run **after** C-1 (PRUNE-3 + PRUNE-4 — Lock-14 refactor
> cluster) so the generic generator template exists to consume W8
> policy. Wiring W8 into the current per-grammar provider module mesh
> would re-deepen the Lock-14 violation, not remediate it. C-1 → C-4
> ordering is structural, not nominal.

This is the central post-S-P0 wave-order discovery: a naïve PRUNE-5-
first would land W8 / W9 runtime consumers against the current 8 per-
grammar `RuntimeProvider::*` match arms at `skinny/crates/codegen/src/
lib.rs:167-209` and the 8 per-grammar provider modules — re-deepening
the very Lock-14 violation PRUNE-3 is dispatched to remediate. C-1
delivers the generic dispatcher that C-4's runtime consumers attach
to; reversing the order doubles the refactor surface in C-1 retro-
actively.

### §2.3 PRUNE-4 sub-wave count is 9, not 8

A3, A5, and A6 independently confirm via `find crates/core/src/runtime
-mindepth 1 -maxdepth 1 -type d` that 9 per-grammar directories exist
(`css_pretty` is the 9th, added between the V13 audit pack body's
8-directory enumeration and SK-V14 starting state). The skinny mirror
under `skinny/crates/runtime/src/grammars/` also has 9 directories
(7 CSS L4 + `json` + `sheets_witness` — the last is test-only). The
S-P3 wave manifest must size PRUNE-4 for 9 sub-waves; an 8-sub-wave
plan silently orphans `css_pretty`.

### §2.4 CH7 gating extension (A4 supplementary recommendation)

Per A4 §4 (CH7 gating row), the synthesis recommends two CH7-companion
mechanisms — neither is currently in SYNTHESIS §3 C-1..C-5:

1. **Round-trip subcommand pairing.** Extend `skinny/xtask/src/main.rs:
   8` USAGE line so every `regen-X` subcommand has a matching `check-X`
   subcommand whose CI invocation reads the emitted bytes, re-runs
   `regen-X`, and diffs. The current `check-json` + `check-real-typed`
   pair extends to seven new `check-css-l4-<provider>` invocations
   after R4. A `check-all` aggregate would land alongside.

2. **Lock-14-companion lint.** Add a clippy-lint or pre-commit grep
   that REJECTS any new `// @generated by skinny bbnf-codegen` header
   in `skinny/crates/{runtime/src/grammars,codegen/src}/**/*.rs`
   unless the matching path appears in a recognized regen subcommand's
   emission roster. The glob MUST scope BOTH the runtime-side mirror
   AND the codegen-side template/provider files — CH2 §3.5 verified
   42 files carry the fake header including 8 codegen-side
   template+provider files; a runtime-only lint would let the
   codegen-side twin re-introduce the fake header silently (the
   identical-content round-tripping vector A4 finding 15 enumerates
   between codegen-side template and runtime-side `generated.rs`).
   Without this guard the fake-`@generated` recurrence (A4 NEW-1 +
   NEW-2 + the 7 CSS files + the JSON file) can re-introduce in any
   SK-V{N+1} wave. Recommend lifting to LOCKS.md as a Lock-14-companion
   lint.

Both extensions can fold into C-4 entry/exit gates or into a new
small C-6 "CH7 mechanical gates" wave; S-P3 should resolve the
attribution.

## §3 — Prune list (binding for S-P3 wave manifest)

### §3.1 Coverage check: 74 findings vs SK-V14 SYNTHESIS §3 C-1..C-5

Cross-reference of every per-axis finding cluster to the existing C-1..
C-5 candidate slate at `restart/skinny/tranches/sk-v14/SYNTHESIS.md
§3`. The slate covers **73 of 74 findings**; the one not covered
upgrades into the §2.4 CH7-companion extension proposal above.

| C-N | SYNTHESIS §3 binding | Findings covered | Count |
| --- | --- | --- | ---: |
| **C-1** = PRUNE-3 + PRUNE-4 | Lock-14 refactor cluster (per-grammar dispatch → generic registry; per-grammar runtime collapse onto template / genuine codegen) | A3 C1..C11, H1..H7, M1..M5, L1..L7; A4 finding 9 + 14 + 15; A6 CRIT-1 + CRIT-2 (Pattern H 67 + 48 file census); A6 HIGH-2 (substrate-doc opt-out enshrinement); A6 NEW-MED (pre-restart-API carry); A6 NEW-HIGH-1 (LegacyPath shim — fold as PRUNE-4 sub-task OR open small "C-6 typed-path collapse") | 41 |
| **C-2** = R1 + R2 | Comparator rebind (strict-vs-strict per plane) + per-iter equality oracle | A1 CRIT (parse_only sonic_rs misbinding); A1 MED (grouped-measurement disclosure — addendum to ROLLING-SOTA-DELTA schema); A1 MED (throughput-plausibility floor — `xtask gate` rejection); A2 F6 (comparator misbinding); A2 F7 (per-iter oracle absent); A2 F8 (single-lane fan-out structural cause); A4 finding 8 (fake `@generated` mechanism — partial overlap with C-1) | 7 |
| **C-3** = R4 + R5 | CSS L4 generative pipeline (xtask regen-css from .bbnf) + production corpus | A1 CRIT (24 CSS rows below 1 KB floor); A1 CRIT (W10.2 / W10.3 sub-Mbps clusters); A1 HIGH (W2 / W10.1 sub-Mbps clusters); A1 MED (missing `skinny/corpora/css-l4-sk-v14/`); A4 findings 2 + 3 + 4 + 5 + 6 + 7 (no regen-css subcommand, 3 scanners are fixture lookups, 14/15 .bbnf orphan); A4 finding 1 (CSS templates as include_str only) | 11 |
| **C-4** = PRUNE-5 | Wire W8 per-grammar policy + W9 same-substrate union from gate-only into compile / lower / runtime | A5 finding 1 (W8 SCAFFOLD persists); A5 finding 2 (W9 SCAFFOLD persists); A5 NEW-MED (gate-layer-only footprint quantified); A5 LOW (resolver honest self-labelling — no-op pre-C-4) | 4 |
| **C-5** = PRUNE-1 + PRUNE-2 | Revert audit-falsified admit rows (5 parse_only + 6 direct + 11 typed + 24 CSS L4) | A1 CRIT (24 CSS rows + 5 JSON parse_only rows revert); A2 F1..F5 (W14.1-5 gate-relabel reverts); A2 F9 (negative drift — no-op confirmation); A6 CRIT-3 (8 fake-codegen providers — overlap with C-1 + C-3); A3 D1 (DELTA-NOTE — fold into PRUNE-3 cosmetic rename); A6 NEW-LOW (asm bibliographic — KEEP, no action) | 11 |
| **Total covered** | | | **74** |

Findings counted at most once per their *primary* mapping (overlaps
noted inline). Some findings are co-mapped — e.g. A4 finding 9 (per-
grammar provider modules as Lock-14 vector) is primarily C-1 (PRUNE-3
deletion) but co-fires with C-3 (R4 builds the replacement). The S-P3
wave manifest should respect these co-fires when sequencing.

### §3.2 Orphan findings

**None.** Every one of the 74 findings maps to at least one C-1..C-5
candidate. The 20 NEW per-row findings (= 11 NEW conceptual
categories) beyond V13 all fit within the existing slate's scope. No
C-6 candidate is required.

The §2.4 CH7-companion extensions (round-trip subcommand pairing + Lock-
14-companion lint) are *recommendations beyond the slate*, not findings
proper; they land as gating enhancements inside C-3 (xtask USAGE
extension) and as a LOCKS.md companion-lint amendment respectively. S-P3
should resolve attribution at plan time.

### §3.3 Sub-wave count summary

Bound for the SK-V{N} SPEC wave manifest:

- **PRUNE-1** = 1 wave (revert 5 parse_only + 6 direct + 11 typed = 22
  JSON ADMITTED rows in `ROLLING-SOTA-DELTA.md` + `RESULTS.md`;
  preserve gate/report scaffold).
- **PRUNE-2** = 1 wave (revert 24 CSS L4 ADMITTED rows + delete 7
  hand-written `*_templates/` directories + 7 provider modules + 7
  runtime twins; gated on R4 having landed first).
- **PRUNE-3** = 1 wave (Lock-14 refactor: `RuntimeProvider` enum →
  trait dispatch; 8 per-grammar provider modules → 1 generic generator;
  bbnf/grammar/passes JSON-rooted symbols → per-grammar facade).
- **PRUNE-4** = **9 sub-waves** (per-grammar runtime collapse for
  `bbnf`, `bnf`, `css_l4`, `css_pretty`, `csv`, `ebnf`, `google_sheets`,
  `json`, `math`; the `css_pretty` sub-wave is the +1 over the V13
  baseline's 8); includes the LegacyPath shim rewrite folded as a sub-
  task or as an optional small "C-6 typed-path collapse" inside PRUNE-
  4's scope.
- **PRUNE-5** = 1 wave (wire W8 + W9 from gate layer through to compile
  + lower + runtime; gated on PRUNE-3 + PRUNE-4 having landed first).
- **R1, R2** = 1 wave each (comparator rebind, per-iter equality
  oracle).
- **R4** = 1 wave (regen-css xtask consuming the 15 `.bbnf` grammars);
  gates PRUNE-2.
- **R5** = 1 wave (`skinny/corpora/css-l4-sk-v14/` ~960 KB production
  corpus from Bootstrap + Tailwind + Material + Animate).
- **R6, R7, R8** = re-admit waves; cannot dispatch until PRUNE waves
  converge per HANDOFF §4 step 9.

## §4 — CH1-CH7 challenge readiness

### §4.1 Citation discipline

Every concrete claim in the six per-axis files plus this synthesis
carries either `path:line` citation or executable-verification quoted
output. Spot-checks across the per-axis files:

- A1: 6 executable verification swathes (§1.1 git log, §1.2 grep
  census, §1.3 find + wc -c, §1.4 per-parse ns table from rolling-
  delta arithmetic, §1.5 awk cluster count, §1.6 sed quote of
  `json_parity.rs:43-53, 87-102`). Every cited file:line has the
  output quoted in-file.
- A2: 10 executable commands (§1 list), each backed by quoted output
  in §2.1 table + §2.2 verbatim Rust quote + §2.3 21-hit grep
  enumeration + §2.4 negative-grep confirmation + §2.5 git log of
  ROLLING-SOTA-DELTA. All `git show --stat` numbers (insertions /
  deletions / file counts) are quoted from live tool output.
- A3: per-file distribution table (`git grep -c …` per file), 5
  verbatim sample hits, full RuntimeProvider enum match-arm
  enumeration with `file:line` for all 8 arms, generated-header file
  count (42), per-grammar directory roster (9 dirs found).
- A4: 6 grep/find commands quoted with output, full round-trip table
  per provider, template byte-count table (8 files), per-finding cites
  with `file:line` ranges; the W4 admit commit `e7e3af22c` traced via
  `git log --all --oneline -- <path>`.
- A5: 9 commands in §1.2 table with results; full quote of `passes/
  src/lib.rs:476-478` (§1.3) + `codegen/src/lower/rust.rs:37-89`
  (§1.4); every finding cites the empty / non-empty grep that confirms
  it.
- A6: full `find` enumerations of per-grammar directories + file count
  table (67 total under crates/core, 48 under skinny mirror); sample
  hand-written-vs-grammar-derived inspection of 3 files with module-
  header text + LOC quoted; grep for compat-shim / combinator-fallback
  / generic-crate-leak patterns each with the matching output (or
  empty) inline.

This synthesis itself cites: per-axis file paths at §0.1, NEW finding
locations at §1.2 (each with `path:line`), the A4 §4 + A5 §4.1
sequencing quotes verbatim, and the PRUNE-4 sub-wave count
arithmetic in §3.3.

### §4.2 Non-verified claims requiring pre-CHALLENGE remediation

**None identified.** Every per-axis file passes the institutionalized
executable-verification mandate from V3→V4 (every cited shell command,
build invocation, grep pattern, or file count is actually run and the
output quoted). The synthesis's aggregate counts (31 / 20 / 12 / 11 =
74) are arithmetic over the per-axis ledgers — verifiable by summation
without re-running greps.

One *soft* observation: A1 §1.4's per-parse ns calculation uses the
formula `elapsed_ns = bytes × 8000 / Mbps`. The Mbps numbers come from
ROLLING-SOTA-DELTA cells quoted directly; the formula is arithmetic.
The ratios (e.g. W10.2 124.3×) follow from the Mbps cells in §1.5's
cluster table. CHALLENGE V1 reviewers can re-run the arithmetic against
the rolling-delta file directly if desired; no judgement call hides in
the calculation.

### §4.3 CH1-CH7 readiness verdict

Per `ORCHESTRATOR.md §3W` + `PASS-0-OVERFIT-AUDIT.md §CH7`:

- **CH1 Correctness:** every CRITICAL / HIGH finding is grounded in
  source citation + executable output; no claim relies on memory or
  prior-tranche citation alone. **READY.**
- **CH2 Generality:** the 11 NEW conceptual categories (= 20 NEW
  per-row findings) are general patterns (single-lane comparator
  fan-out, fixture-lookup scanners, orphan `.bbnf`, gate-layer-only
  footprint, substrate-doc opt-out enshrinement) — not single-row
  issues. **READY.**
- **CH3 Regression:** the audit confirms zero regression vs V13
  (every finding either CONFIRMS or extends) + zero drift since the
  audit pack landed (A2 F9 negative finding). **READY.**
- **CH4 Cost:** the prune slate cost is enumerated in SYNTHESIS §3
  C-1..C-5 LOC estimates; this audit adds zero cost beyond the
  existing slate, only re-attributing some findings. **READY.**
- **CH5 Hidden Coupling:** the §2.1 R4 → PRUNE-2 + §2.2 C-1 → C-4
  sequencing constraints are surfaced explicitly. **READY.**
- **CH6 Anti-Paper-Close:** every finding cites runnable verification;
  the audit is not documentary-only. **READY.**
- **CH7 Overfit-Prune:** the audit explicitly enumerates the overfit
  vectors (gate-relabel admits, fixture-lookup scanners, fake
  `@generated` header recurrence, orphan grammars). **READY.**

The audit is CH1-CH7 ready end-to-end; CHALLENGE V1 may dispatch
without pre-remediation.

## §5 — Verdict + next move

### §5.1 S-P0 V1 verdict

**PRUNE LIST CONFIRMED.**

- Aggregate: 74 findings (31 CRIT + 20 HIGH + 12 MED + 11 LOW).
- 5 of 6 axes FAIL outright (A1, A2, A3, A4, A6); 1 of 6 PARTIAL PASS
  (A5: resolver clause PASS; scaffold-clause FAIL at HEAD, conditional
  PASS upon PRUNE-1 + PRUNE-2 + PRUNE-5 landing per C-5 → C-4
  sequencing).
- 11 NEW conceptual categories (= 20 NEW per-row findings) extending
  the SK-V13 audit pack (A2 ×2, A3 ×1 DELTA-NOTE, A4 ×3 categories /
  12 rows, A5 ×1, A6 ×4); 54 findings CONFIRM the V13 audit pack
  byte-for-byte (per per-axis table column-sum) at SK-V14 starting
  state (HEAD `12ff0744e`).
- Pattern H file count: 64 (V13) → 67 (SK-V14 baseline; +3 from
  `css_pretty`).
- C-1..C-5 candidate slate covers all 74 findings (zero orphans). No
  new C-6+ candidate required; two CH7-companion extensions
  recommended (§2.4) for S-P3 attribution.
- Three architectural sequencing constraints surfaced for S-P3 plan:
  R4 → PRUNE-2; C-1 → C-4; PRUNE-4 sub-wave count = 9.

### §5.2 Next move (per `PASS-0-OVERFIT-AUDIT.md §Procedure` step 2-3)

1. **CHALLENGE V1 dispatches next.** CH1-CH7 challenge applies to each
   of the six per-axis files + this synthesis per
   `ORCHESTRATOR.md §3W` and `PASS-0-OVERFIT-AUDIT.md §Procedure`
   step 2.
2. **§3Z convergence gates G-S-P0-CONVERGED** before S-P1 dispatch
   (≥95% ACCEPT × 2 consecutive cycles across CH1-CH7 where CH7 is
   the new Overfit-Prune lens). Per the institutionalized convergence
   pattern from Pass Alpha V2-V5.
3. **G-S-P0-CONVERGED fires PRUNE-1..5 + S-P1.** The prune list at §3
   binds the SK-V14 SPEC wave manifest (S-P3 output); the behavior
   waves R6 / R7 / R8 hold behind PRUNE convergence per HANDOFF §4
   step 9.

The SK-V14 standing process loop (`PASS-0-OVERFIT-AUDIT.md §Standing
SK process loop`) carries into the post-S-P0 sequence: S-P1 (Profile)
→ S-P2 (Research, six cohorts CH1-CH7 lensed) → S-P3 (Synthesis-Plan,
wave manifest CH7 lens binding) → Waves W0..Wn (each CH1-CH7 lensed
at plan + redress) → Pass Omega V1.{X} → Pass Alpha close → bracket
SK-V15.
