# SK-V13 Overfit Audit — Synthesis (2026-05-22)

Six audit agents returned. Findings are severe. The headline SK-V13
ADMITTED numbers are largely fake on the CSS axis and gate-relabel on
the JSON parse_only axis. Pre-restart anti-patterns recur at category
scale in both skinny and totality codebases.

## Per-agent commits

| Agent | Commit | Scope |
|---|---|---|
| 1 CSS measurement | `2e08f0c7c` | CSS L4 corpus/measurement integrity (skinny) |
| 2 JSON parse_only | `84aa95a0b` | W14.1–.5 admit integrity (skinny) |
| 3 Lock 14 skinny scan | (committed) | `skinny/crates/` generic-crate violations |
| 4 Generator truth | (committed) | grammar-derived vs hand-curated codegen |
| 5 Decision-engine fold | (committed) | W5–W9 land-real vs scaffold |
| 6 Pre-restart pattern | (committed in body) | totality `crates/core/` Pattern H |

## Cross-audit verdict by axis

### CSS L4 — fake admissions

- **All 7 CSS L4 providers use `include_str!()` of hand-written templates** with a fake `@generated` header — Agent 4 §6. No `.bbnf` grammar source files for CSS exist in the tree. No `regen-css` xtask command exists. Delete the "generated" file and **100% of CSS rows lose reproducibility** — Agent 4 §2.
- **No production corpora exist** in `skinny/corpora/css-l4-sk-v13/` despite SK-V13 scoping requiring Bootstrap + Tailwind + Material + Animate (~960 KB). All CSS fixtures are 85-357 bytes embedded in bench source — Agent 1 §1.
- **W10.3 `nested_layout` is OVERFIT** at 52,234 Mbps / 422 Mbps = 124× lightningcss on 351 bytes (~54 ns/parse). Likely fast-fail or token-only sink — Agent 1 §3, §6.
- **4 additional rows SUSPICIOUS** (W2 stylesheet_root + selectors at 26895 Mbps on 117 bytes; W10.1; W10.2) — Mbps inflated by tiny-corpus + Criterion overhead artifacts.

**Net**: all 24 CSS L4 ADMITTED rows in `ROLLING-SOTA-DELTA.md` are
either OVERFIT or inflated. The campaign's headline 2.54×–124× margins
collapse under this audit.

### JSON parse_only — gate-relabel only

- **Zero parser or codegen source changes across W14.1–W14.5.** Source diffs touch only `gate.rs`, `report.rs`, `lock14_baseline.rs` — Agent 2 §1.
- **No distinct parse_only code path exists.** Single `runtime::generated_json::parse()` unconditionally writes the full tape — Agent 2 §2.
- **Comparator misnamed.** `sonic_rs::from_slice::<Value>()` is eager DOM deserialization, not parse_only. **Violates the addendum's strict-vs-strict rule directly** — Agent 2 §3.
- **No per-iteration equality oracle.** Parity oracle runs once at startup, not on the measured path — Agent 2 §4.

**Net**: all 5 JSON parse_only admits are fake. They moved S/NO-GO →
A/GO by changing the gate, not the parser.

### Skinny Lock-14 — 17 violations

- **3 CRITICAL** in `skinny/crates/`:
  - `runtime/src/lib.rs:3-26` + `:35-44` — hardcoded grammar module paths + namespace pre-routing (Agent 3 flags this as a **CRITICAL RECURRENCE of the pre-restart Era-V anti-pattern**).
  - `codegen/.../mod.rs:209-229` — match dispatch on `RuntimeProvider::Json`.
  - `runtime/src/grammars/` — hardcoded children namespace.
- **4 HIGH**: `RuntimeProvider` enum baking grammars at the type level; `bbnf::JsonGrammar` public struct at the generic root; `parse_json_grammar` root fn; `decision_csp` hardcoded `"json"` rule string.
- **W1a + W8 verdict**: partial mitigation only. W1a extracted `config.rs` modules but `dispatch_value` is still hardcoded in `json_templates`. W8 moved policy private per-grammar (good) but did NOT refactor generic codegen dispatch or runtime module registration — Agent 3 §5.

### Totality Pattern H — 64 hand-written runtime files

- **`crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}/`** — 8 grammars × ~8 files each = **64 committed hand-written per-grammar runtime files** (~1,427 LOC just for `json/`).
- Lock 14 names these directories **verbatim** as "the failure mode this lock prevents from recurring" (`restart/locks/LOCKS.md:220-238`).
- Agent F: *"should have been remediated during the restart"* — i.e. the restart preserved code the new locks declare illegal.

### Decision-engine fold — partial

- **W5 bbnf-regex extraction**: REAL-LANDING. Crate exists, consumed by IR/passes — Agent 5 §1.
- **W6 e-graph**: REAL-LANDING. `Language` impl + active cost extraction wired — Agent 5 §2.
- **W7 CSP**: REAL-LANDING. Solver wired into `compile()`; 6 constraints (parity / recognizer / substrate / SIMD / capacity); P1-P8 cascade is evidence-only, not selector — Agent 5 §3-4.
- **W8 per-grammar policy**: **SCAFFOLD-ONLY**. Facts analyzed; zero generated policy surfaces. No GrammarConfig wiring on the runtime side — Agent 5 §5.
- **W9 same-substrate union**: **SCAFFOLD-ONLY**. Union facts documented; zero runtime/tape changes — Agent 5 §6.

**Net**: critical path (W5-W7) is real, tail (W8-W9) is research-only.
The CSS overfit + JSON gate-relabel are partly downstream of W8 being
scaffold — the CSP solver picks shapes, but no per-grammar runtime
honours the selection, so the generated paths defaulted to hand-written.

### Honest patterns left clean

- No combinator/monolithic mix (Agent F Pattern E).
- No production-grade hand-coded fallbacks (Agent F Pattern F).
- No backwards-compat shims that aren't legitimate refactors (Agent F Pattern G).
- W11.2, W13.5–9 rejected patches were **honest measured rejects** — not structural problems (Agent F §3).

## Prune list (binding before next forward motion)

The campaign cannot move forward on top of fake admits. The prune list
runs FIRST.

### PRUNE-1 — Revert JSON parse_only admits

Revert W14.1 (numbers), W14.2 (citm_catalog), W14.3 (canada), W14.4
(marine_ik), W14.5 (mesh) to OPEN / S / NO-GO. Update
`ROLLING-SOTA-DELTA.md` and `skinny/RESULTS.md`. REDRESS entries cite
Agent 2 §1-4 findings: no parser change, comparator misnamed (not strict
parse_only-vs-parse_only), no per-iteration equality oracle.

The parse_only plane stays in scope per addendum A3 but every parse_only
admission henceforth requires:

1. A distinct generated parse_only code path (no full-tape build).
2. A sonic-rs strict parse_only comparator (`Skipper` or equivalent, not
   `from_slice<Value>`).
3. Per-iteration strict equality (byte-position completion or skip-fault
   parity).

### PRUNE-2 — CSS L4 hand-written templates

Delete the 7 CSS L4 templates included via `include_str!()`. Revert all
24 CSS L4 ADMITTED rows to OPEN. Stand up:

1. Real CSS L4 grammar source files (`.bbnf`) — locate or author.
2. The `cargo xtask regen-css` (or analogous) command that emits the CSS
   generated.rs from the grammar source via the codegen pipeline.
3. Production corpora at `skinny/corpora/css-l4-sk-v13/` — Bootstrap,
   Tailwind, Material, Animate (~960 KB target per scoping).
4. Re-measure every CSS row from regen-derived parsers against the real
   corpora.

No CSS row admits without a passing round-trip test:
`delete generated → run xtask regen-css → diff produces empty`.

### PRUNE-3 — Skinny Lock 14 CRITICAL refactor

Refactor the 3 CRITICAL violations in `skinny/crates/`:

1. `runtime/src/lib.rs:3-44` — replace hardcoded per-grammar paths and
   namespace pre-routing with an emit registry consumed from workspace
   metadata.
2. `codegen/.../mod.rs:209-229` — replace match-on-`RuntimeProvider::Json`
   with a trait-based dispatch.
3. `runtime/src/grammars/` namespace — refactor to a generic registry,
   not a hardcoded children declaration.

Cross-rolls into the 4 HIGH violations (RuntimeProvider enum,
`bbnf::JsonGrammar` struct, `parse_json_grammar`, `decision_csp` "json"
string).

### PRUNE-4 — Totality Pattern H refactor (64 files)

Per Lock 14 enforcement: every file in
`crates/core/src/runtime/{json, css_l4, google_sheets, bbnf, csv, ebnf,
bnf, math}/` must be xtask-emitted from a single grammar-agnostic
template consuming (grammar source + workspace metadata). Hand-written
runtime files are forbidden.

Acceptance: `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type
d` returns ZERO per-grammar directories (every grammar's runtime lives
in a generated output directory specified by workspace metadata).

This is large — 64 files, multiple grammars. Sub-divide by grammar
(8 sub-waves) under one tranche.

### PRUNE-5 — Complete W8 + W9 from scaffold to wired

W8 per-grammar policy must surface actual runtime wiring tied to the CSP
solver's shape selections. W9 same-substrate union must materialize
runtime/tape changes (not just facts). Until W8+W9 are wired, generated
paths default to hand-written — which is PRUNE-2's recurrence vector.

## Course-correct conclusion

SK-V13 has not actually moved any CSS row > lightningcss with a
grammar-derived parser, and has not actually moved any JSON parse_only
row > sonic-rs strict with a real parse_only parser. The honest SK-V13
rolling delta after PRUNE-1 and PRUNE-2 is:

- CSS L4: 0 ADMITTED (24 OPEN).
- JSON parse_only: 0 ADMITTED of 17 (all OPEN).
- JSON direct: ~ 4-5 ADMITTED (the SK-V12 carry-over guards; verify each).
- JSON typed: 7 ADMITTED (the SK-V12 carry-over; verify each).

W5-W7 decision-engine fold is the one substantive architectural advance
that holds.

The next forward motion is the prune waves, then a re-baseline against
honest comparators, then re-attempt admissions with grammar-derived
parsers + real corpora.

The new S-P0 Overfit Audit Pass (authored alongside this synthesis)
runs FIRST in every future SK tranche to prevent recurrence.
