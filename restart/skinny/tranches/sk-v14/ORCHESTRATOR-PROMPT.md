# SK-V14 — Fresh-Session Orchestrator Prompt

Authority: user, 2026-05-22. This file is the in-tree mirror of the
fresh-session prompt that brackets SK-V13 → SK-V14. It supersedes the
prior orchestrator framing where they conflict and stands alongside
`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md` (which it
extends, not replaces).

The prompt is preserved verbatim below for sub-agent consumption.

---

## PROMPT — SK-V14 (Prune-Then-Rebuild) — fresh implementation orchestrator.

The prior orchestrator (codex) hit a credit cap mid-W16.1. Independent
S-P0 validation (6 fresh agents) confirmed the SK-V13 admit story is
fiction: 0 / 43 admitted rows survive strict-vs-strict audit. The
campaign's architectural skeleton holds. The admit machinery doesn't.
Resume from the honest baseline. Prune first. Rebuild correctly.
Continue indefatigably per the USER PIN addendum.

### READ FIRST — authoritative; do NOT restate:

- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
- `restart/skinny/tranches/sk-v13/audit-overfit/validation/v{1..6}-*.md` (6 files)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` (binding new pass)
- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-{1,2,3}-*.md`
- `restart/prompts/totality/PASS-{1,2,3}-*.md`
- `restart/prompts/pass-contracts/{PASS-ALPHA, PASS-OMEGA, SKINNY-TRIUMVIRATE}.md`
- `restart/locks/LOCKS.md` (V1.1; CH7 lens binding)
- `restart/skinny/ROLLING-SOTA-DELTA.md` (needs honest re-baseline)

### HONEST BASELINE (verified by 6 independent validation agents):

  Survives — architectural pillars:
    • W5 bbnf-regex extraction        — LOAD-BEARING
    • W6 e-graph Language + cost      — LOAD-BEARING (extraction-only)
    • W7 CSP solver, 5 constraints    — LOAD-BEARING, fail-closed
    • bbnf-simd, 52 files             — fully grammar-neutral
    • OffsetFlags + Tape              — grammar-neutral
    • generated_json::parse_direct    — real codegen from grammar
    • generated_real_typed::parse_*   — real codegen from grammar
    • 15 CSS .bbnf grammars at        — present, UNWIRED
        /grammar/css/l4/

  Does not survive:
    • 25 CSS L4 admitted rows         — ALL FAKE (hand-written
      (incl. SK-V12 W1b 2.54x)          templates; fake @generated
                                        header; no regen-css xtask)
    • 5 JSON parse_only admits        — ALL FAKE (gate-relabel;
      (W14.1..5)                        comparator misnamed)
    • 4 JSON direct admits            — REAL parsers; comparator
                                        misbinding (eager DOM
                                        instead of struct deser)
    • 7 JSON typed admits             — REAL parsers; comparator
                                        misbinding (eager DOM
                                        instead of per-corpus typed)
    • W8 per-grammar policy           — COSMETIC, no runtime
                                        consumption
    • W9 same-substrate union         — COSMETIC, hardcoded
                                        constants
    • Lock 14: 30 violations          — 11 CRITICAL + 7 HIGH + 5 MED
      (codex undercounted by 43%)       + 7 LOW. 8 hand-written
                                        per-grammar provider modules
                                        in codegen/ are the
                                        recurrence vector.

  Honest rolling delta:
    JSON parse_only: 0 / 17  (all OPEN; needs distinct parse_only
                              path + Skipper comparator)
    JSON direct:     0 / 17  (4 cells need comparator rebind)
    JSON typed:      0 / 17  (7 cells need comparator rebind)
    CSS L4:          0 / 24  (all OPEN; templates pending PRUNE-2 +
                              amended skinny-side regen-css pending W2)

  Campaign at zero on numbers; non-zero on architecture.

### INFLECTION POINT (re-stated):

  Pre-inflection: handcrafted paths ACCEPTABLE only when catalogued
  via S-P0 with TODO-GENERALIZE markers. The 8-provider-module
  recurrence shows what "OK until inflection" silently becomes if
  uncatalogued.

  Inflection: when both CSS L4 full lightningcss parity AND every
  JSON cell > sonic-rs strict admit, the inflection fires. Every
  catalogue entry converts to grammar-derived OR deletes.

  Current state: pre-inflection. 0 admitted CSS features. 0 admitted
  JSON cells. The work between here and inflection is what SK-V14+
  owns.

### SK-V14 GOALSET (replaces base prompt where conflicts):

  R1. COMPARATOR REBIND. Replace the single sonic_rs::from_slice
      <Value> with three correct strict-mode comparators:
        parse_only → sonic_rs::Skipper (structural-skip-only).
        direct    → sonic_rs strict struct deserialization per corpus.
        typed     → per-corpus typed struct deserialization.
      No row admits until its plane's comparator is strict-vs-strict.

  R2. PER-ITERATION EQUALITY ORACLE. Equality on EACH bench iter,
      not startup. Quote the harness line that runs equality inside
      the timing region. The current startup-only checksum parity
      fails the addendum's strict admit rule.

  R3. PRUNE WAVES (execute before any new admit attempt):
        PRUNE-1: Revert W14.1-.5 parse_only admits. RESULTS + DELTA.
        PRUNE-2: Revert all 24 CSS L4 admitted rows in the rolling
                 delta and REDRESS ledger only. No CSS provider,
                 template, runtime, source, or generator deletion in
                 W4 after Pass Omega V4 W4R.
        PRUNE-3A: Prove the grammar-neutral source-consuming runtime
                 generator contract. Pass grammar source + workspace
                 metadata into codegen; parse required V1 grammar-source
                 constructs for runtime generation without grammar-id
                 branches; migrate regen-css; prove CSS L4 plus
                 JSON/Sheets/BBNF-self non-JSON gates. No provider or
                 template deletion in PRUNE-3A.
        PRUNE-3B: Build the generic BBNF grammar-source frontend/import/IR
                 closure as the Pass Omega V8 W5B.0..W5B.4 aggregate only
                 after PRUNE-3A is load-bearing. W5B.0 first admits the Lock 14
                 owner-path/parent-diff gate; W5B.1 closes import resolution;
                 W5B.2 lowers layout/discard facts; W5B.3 lowers pretty/span/
                 projection facts; W5B.4 wires the request consumer and closes
                 W5B-FRONTEND. Lower CSS L4 compatibility constructs such as
                 @ws into canonical IR; do not create new public syntax,
                 replace the provider-backed generator body, or delete
                 providers/templates here. W5C-GEN remains blocked until all
                 five W5B sub-waves admit.
        PRUNE-3C: Build the provider-free runtime generator body only after
                 PRUNE-3B is load-bearing. Remove live production dependency
                 on RuntimeProvider / GrammarProfile / render_runtime_profile
                 and prove regen-css, seven CSS companions, JSON, and
                 non-JSON witnesses through W5A request facts plus W5B-FRONTEND
                 IR. Do not delete providers/templates here.
        PRUNE-3D: Delete the seven CSS provider/template clusters and retire
                 the old provider mesh only after PRUNE-3C is load-bearing.
                 Close the Lock 14 baseline with regen-css, seven CSS
                 companions, JSON unchanged-output proof, and Sheets/BBNF-self
                 proof.
        PRUNE-4: Totality Pattern H — refactor 67 hand-written
                 per-grammar files in crates/core/src/runtime/{grammar}/
                 into emitted output. Sub-divide by grammar (9 sub-waves,
                 W6.0..W6.8; W6.0 owns css_l4 root runtime per W2R).
        PRUNE-5: Wire W8 per-grammar policy + W9 same-substrate union
                 from SCAFFOLD-ONLY to LOAD-BEARING. CSP picks shapes;
                 runtime must honor selections.

  R4. REGEN-CSS PIPELINE. Build cargo xtask regen-css that consumes
      the 15 .bbnf files at /grammar/css/l4/ and emits CSS L4
      skinny runtime modules only. Acceptance: seven exact `check-css-l4-*`
      companions pass and `delete skinny generated → run xtask regen-css →
      skinny runtime diff produces empty`. Root `crates/core/src/runtime/css_l4/`
      is W6.0 work after W5D-DELETE.

  R5. PRODUCTION CORPORA. Stand up `skinny/corpora/css-l4-sk-v14/`
      with Bootstrap + Tailwind + Material + Animate (~960 KB).
      Tiny embedded fixtures unacceptable for admit.

  R6. CSS L4 RE-ADMIT (honest). After R3+R4+R5, re-attempt each
      CSS L4 row via the grammar-derived pipeline, real corpora,
      work-equivalent comparator (lightningcss full-parse; cssparser
      full-parse; no fact-stream vs full-AST asymmetry).

  R7. JSON DIRECT + TYPED RE-ADMIT. After R1+R2, re-baseline every
      JSON direct + typed row against the rebound strict comparators.
      Cells previously HOLDING (under wrong comparator) hold again
      under the right comparator OR are reverted.

  R8. JSON parse_only DISTINCT PATH. Stand up a distinct parse_only
      code path (no full-tape build) in generated_json. Wire to
      Skipper-style comparator. Then attempt admit.

  R9. CARRIED — pillars unchanged. W5A/W5B.0..W5B.4/W5C-GEN/W5D-DELETE/W6/W7/
      bbnf-simd/OffsetFlags/Tape stand. R3 must not regress them.

  R10. INDEFATIGABLE CAMPAIGN per addendum. SK-V14 closes when
       either (a) all R-targets hold AND every JSON cell + every
       CSS feature ADMITs > strict-vs-strict OR has architectural-
       level intrinsic-block proof, or (b) measured fixpoint per
       row + user re-pin.

### THE SK LOOP (codified — every tranche):

  S-P0  Overfit Audit Pass (6 agents in parallel; A1-A6; CH7 lens
        binding). Gate G-S-P0-CONVERGED before S-P1 dispatches.
  S-P1  Profile (fresh PMU; new corpora; rebound comparators).
  S-P2  Research (6 cohort agents; CH1-CH7 lensed).
  S-P3  Synthesis-Plan (wave manifest; CH7 binding).
  -- concurrent: Pass Omega T-P1/T-P2/T-P3 (totality fold) --
  G-Omega user gate (locks-diff + spec deltas).
  CRUD execute (totality).
  Wave program (skinny; PRUNE waves first this tranche; new-admit
    waves only after PRUNE converges; each wave CH7-lensed).
  Pass Alpha close + bracket → SK-V{N+1}.

### DISCIPLINE (binding):

- Triumvirate: research / plan / redress distinct dispatches.
- Hard caps: 20 / 15 / 30 min research/plan/redress (45 min redress
  for decision-engine wiring waves).
- CH7 Overfit-Prune lens at every plan and redress.
- §2.2 micro-prove-first.
- Strict-vs-strict comparator; per-iter equality.
- KISS DRY; abrogate-before-patch.
- aarch64 / Apple M5 Max only; x86 OUT.
- Status tick every ~5 min orchestrator-silent wait.
- Triumvirate auto-trigger on JSONL-quiet >15 min.

### ESCALATE IMMEDIATELY on:

- Architectural-level intrinsic-block proof per row family.
- Round-trip rule trigger (row failed reopen twice in-tranche).
- Admit regression not in-tranche recoverable.
- Comparator rebind discovers a new misnaming pattern.
- New Lock 14 violation introduced by a wave.

### BEGIN

Dispatch SK-V14 Pass Alpha bracket. Reads audit synthesis +
validation pack + this prompt + the user pin set. Authors SK-V14/
{SYNTHESIS, §0 goalset, HANDOFF} reflecting the honest baseline.
Then S-P0 immediately, then S-P1 PMU + corpora capture, then S-P2/
S-P3 in parallel with Pass Omega T-P1/T-P2/T-P3, G-Omega user gate,
CRUD execute, then the wave program — PRUNE waves first.

Do not relinquish control except at G-Omega user gate.
