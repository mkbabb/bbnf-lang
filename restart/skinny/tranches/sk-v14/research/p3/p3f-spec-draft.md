# SK-V14 P3-F: SPEC + Dispatch Drafting Notes

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-23.
Scope: Author `restart/skinny/tranches/sk-v14/SPEC.md` + `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` mirroring SK-V8 SPEC shape verbatim; fold SYNTHESIS §0 (close + R1-R10 + P-1..P-7) into SPEC §0; wire three S-P2 V3 carry-forward packets (CF-3 admission 3-gate, §2.Y canonical-name binding, F-V2-P1ABC-RERECORD Stage-0); honor three S-P0 sequencing constraints (R4 BEFORE PRUNE-2; C-1 BEFORE C-4; PRUNE-4 = 9 sub-waves).
Output: this file + SPEC.md + DISPATCH-PROMPT.md.
Pass Alpha goalset: SYNTHESIS §0 (R10 close + R1-R10 acceptance + AUDIT-ZERO baseline 0/17/0/17/0/17 JSON + 0/24 CSS L4 + P-1..P-7 pre-blocks).
Candidate pool: research/p2/ post-§3Z COHORT LOCK survivors (V3 HEAD `ebe84954b`).
HEAD pin: workspace HEAD `0a9f1288c62ef9f507854e8ccfebcfc78ba0a322` at write-time.

## §1 — Synthesis

### §1.1 SPEC-shape mirror (per SK-V8 SPEC verbatim)

The SK-V8 SPEC at `restart/skinny/tranches/sk-v8/SPEC.md` is the binding
shape. Section-by-section mirror obligation:

| SK-V8 §X | Content class | SK-V14 fold |
|---|---|---|
| §0.1 Global Close Condition | numbered close-condition clauses | fold SYNTHESIS §0.1 R10 verbatim + extend to 10 numbered clauses incl. AUDIT-FALSIFIED revert + audit_overlay_verdict gate-enforcement |
| §0.2 Comparator Classes | three-class table (strict / flaw probe / sidecar planning signal) | fold SK-V14 R1's three plane-correct strict comparators per `SYNTHESIS.md:93` (sonic-rs Skipper / sonic-rs strict struct deser / per-corpus typed struct deser) |
| §0.3 Outcome Enum | A / C / G / K / L / N-direct / S | preserve SK-V8 enum verbatim; add AUDIT-FALSIFIED row-disposition NOT as outcome but as audit_overlay_verdict column value per SYNTHESIS §2 |
| §0.4 Required Telemetry | 24-column schema with gate-json consumed rules | extend SK-V8 schema with SK-V14 SYNTHESIS §2 columns: `comparator_plane` (R1), `per_iter_equality` (R2), `audit_overlay_verdict` (audit overlay), `track2_entry_point` (CH5) |
| §0.5 Opening Row Goalset | per-family current state + posture table | fold SYNTHESIS §0.2 AUDIT-ZERO baseline 0/17 parse_only + 0/17 direct + 0/17 typed + 0/24 CSS L4 |
| §1 Non-Negotiables | bulleted list of binding clauses | preserve all SK-V8 §1 clauses + add Lock 1 v+1 substrate-target/retention-lifetime/policy-owner triple + Lock 14 v+1 generated-output allowance + Lock 16 v+1 primitive-manifest gating + executable-verification mandate + CH7-V2 procedural addendum |
| §2 Wave Manifest | table of waves with status / LOC budget / cap | 12-wave manifest (W0..W11) honoring 3 S-P0 sequencing constraints |
| §2.1 Generality + Lock 14 Gate | per-wave exit checks (public-API scan, grammar-branch scan, primitive/table scan, non-JSON proof) | mirror SK-V8 verbatim + extend non-JSON proof scope to CSS L4 / Sheets / BBNF-self after C-1 (PRUNE-3 + PRUNE-4) lands |
| §3..§9 per-wave sections | owner paths / doc links / entry gate / tasks / exit gate / same-wave consumer / pre-blocked routes / revert protocol / downstream effect | author one section per W0..W11; PRUNE-4's 9 sub-waves enumerated inside W6 with per-grammar entry/exit gates |
| §10 Pre-Blocked Routes | inheritance ledger of REDRESS blocks | fold SK-V8 §10 + add SYNTHESIS §0.4 P-1..P-7 + S-P2 dispatch-context REDRESS watch-list (28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 119/120, 126) |
| §11 G-Alpha And Dispatch Scope | gate scope clause | scope to W0 dispatch authority only; W1..W11 conditional on W0 close + S-P3 LOCK + G-Omega close + per-wave triumvirate gating |

### §1.2 Wave-count budget (12-wave skinny-bracket ceiling)

Per `SKINNY-TRIUMVIRATE.md:110` (`> 12 waves without convergence
escalates`) and `PASS-3-SYNTHESIS-PLAN.md:131` (≤12 waves), the SK-V14
wave manifest must fit in 12 slots W0..W11. The R1-R10 obligation × the
S-P0 prune-list = the following pack:

| Slot | Wave | R-target(s) | Candidate | Source |
|---|---|---|---|---|
| W0 | Baseline + Telemetry Lock | R9 carry pillars | (no C-N — infra-first per [build-infra-first]) | `[build-infra-first]` mandates W0 = baseline; PASS-3 §8.3 |
| W1 | Comparator Rebind + Per-Iter Equality + PRUNE-1 | R1 + R2 + R3 PRUNE-1 | C-2 + C-5 part-A (revert 22 JSON fake admits) | SYNTHESIS §3 C-2 + §3 C-5 PRUNE-1 |
| W2 | regen-css xtask | R4 | C-3 part-A | SYNTHESIS §3 C-3 (R4); MUST precede PRUNE-2 per S-P0 §2.1 |
| W3 | Production CSS corpora | R5 | C-3 part-B | SYNTHESIS §3 C-3 (R5); ~960 KB Bootstrap + Tailwind + Material + Animate |
| W4 | PRUNE-2 (delete CSS templates + revert 24 CSS admits) | R3 PRUNE-2 | C-5 part-B | SYNTHESIS §3 C-5 PRUNE-2; gated on R4 (W2) per S-P0 §2.1 |
| W5 | PRUNE-3 (Lock-14 refactor: trait dispatch + grammar-agnostic generator) | R3 PRUNE-3 | C-1 part-A | SYNTHESIS §3 C-1 PRUNE-3; MUST precede C-4 per S-P0 §2.2 |
| W6 | PRUNE-4 (9 sub-waves: per-grammar runtime collapse) | R3 PRUNE-4 | C-1 part-B | SYNTHESIS §3 C-1 PRUNE-4; 9 sub-waves NOT 8 per S-P0 §2.3 (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`) |
| W7 | PRUNE-5 (wire W8 + W9 from SCAFFOLD to LOAD-BEARING) | R3 PRUNE-5 | C-4 | SYNTHESIS §3 C-4; gated on C-1 (W5 + W6) per S-P0 §2.2 |
| W8 | CSS L4 re-admit (honest, grammar-derived) | R6 | downstream consumer of C-1+C-3+C-4 | SYNTHESIS §1 R6 |
| W9 | JSON direct + typed re-admit | R7 | downstream consumer of C-2 | SYNTHESIS §1 R7 |
| W10 | JSON parse_only distinct path | R8 | downstream consumer of C-2 + new path | SYNTHESIS §1 R8 |
| W11 | Close + Alpha Feedback | R10 | (no C-N — close ceremony) | SK-V8 §9 mirror |

Total: 12 waves (W0..W11). Sub-wave count: PRUNE-4 expands to 9 sub-
waves W6.1..W6.9 inside the W6 slot — these are sequenced by the W6
plan (single triumvirate per sub-wave is the empirical baseline; the
W6 sub-wave headers carry one section each).

### §1.3 S-P2 V3 carry-forward packets — SPEC wiring

Per `HARDENING-S-P2-V3-CONSOLIDATED.md:466-530` (§6 S-P3 carry-forward
packet), three load-bearing items inherit:

#### §1.3.1 CF-3 admission 3-gate (CH4 V3 discipline)

Verbatim binding:

> "Every shortlisted candidate's admission manifest carries the 3-gate
> CH4 cell explicitly: (scalar-ref status / checkasm-parity expectation
> / same-wave-consumer NAMED) per S-P2 CH4 V2 CF-3."

SPEC §1 wiring: add to non-negotiables: "Every wave admitting any
primitive carries the 3-gate CH4 cell per S-P2 V3 §6.1 — scalar-
reference status named; checkasm-parity expectation named; same-wave
consumer NAMED in the wave's redress commit."

#### §1.3.2 §2.Y NF-CH6-4 canonical-name binding

Verbatim binding:

> "Three artefacts (P2-A C2 `long_string_body_simd_scan`, P2-E Gap 1
> `scan_string_special_block_sweep_64`, P2-F C1+C2 quote-aware
> classifier composition) surface the same long-string-body SIMD scan
> primitive under three distinct names, all grounded on the
> `unescape_string` direct rank-1 46.7 % `unicode_escapes` hot-leaf
> (P1-E §2.2). S-P3 consolidator binding: ONE canonical primitive name
> + ONE canonical scalar-ref function rather than three orthogonal
> SIMD bodies for one primitive."

SPEC §10 pre-blocked routes wiring: add explicit pre-block:
"Three-orthogonal-SIMD-bodies-for-one-primitive admission is REJECT
per S-P2 V3 §6.2 + P2-F §2.Y. Any wave admitting any of the three
convergent identifiers MUST commit to the single canonical primitive
name + single canonical scalar-ref function at admission time."

#### §1.3.3 F-V2-P1ABC-RERECORD Stage-0 wave commitment

Verbatim binding (per S-P2 V3 §6.3 + V1 §2.1 binding entry lines 230-289):

```
Packet: F-V2-P1ABC-RERECORD
Gating:  CH2 (measurability) + CH4 (cost-discriminator) dual-gate
Cargo:   cargo build --release -p bbnf-bench --features runtime/parse-attribution
Samply:  interactive samply record (NOT --save-only) per [samply-symbol-resolution]
         + cfg_attr flip verification at generated.rs:33-34, 43-44,
           58-59, 79-80, 86-87, 117-118, 138-139, 157-158 (8 sites;
           inline(always) → inline(never))
Wave:    Stage 0 of the first SK-V14 implementation wave admitting any
         dispatch-envelope-internal primitive
Consumers (must-bind, [no-deferrals]):
         P2-A C6 + P2-C C-P2C-3 + P2-C C-P2C-8 + P2-E Gap 1 + Gap 3
         + Gap 4 + Gap 5 + P2-F C6 + C7 + C10 + C12 + C13
```

SPEC wave-section wiring: SK-V14 dispatch-envelope-internal primitive
admission is currently DEFERRED to post-PRUNE wave clusters (R6/R7/R8
under W8..W10). The Stage-0 F-V2-P1ABC-RERECORD therefore lands as
Stage 0 of W8 OR W9 OR W10 — whichever first admits any of the 12
consumer-dependency primitives. The SPEC §1 non-negotiables binds:
"Any W8..W10 wave admitting any of the 12 F-V2-P1ABC-RERECORD
consumer-dependency primitives ships the rerun in Stage 0 of the same
wave per S-P2 V3 §6.3."

### §1.4 Three S-P0 architectural sequencing constraints — SPEC wiring

Per `SYNTHESIS-AUDIT-OVERFIT.md:233-291` (§2):

| Constraint | SPEC wiring |
|---|---|
| R4 (regen-css xtask) MUST land BEFORE PRUNE-2 | W2 (R4) precedes W4 (PRUNE-2); W4 entry gate cites "W2 admitted (regen-css subcommand exists; round-trip clean)" |
| C-1 (PRUNE-3 + PRUNE-4) MUST land BEFORE C-4 (PRUNE-5) | W5 + W6 (C-1) precede W7 (C-4); W7 entry gate cites "W5 + W6 admitted (generic dispatcher exists; per-grammar runtime collapsed)" |
| PRUNE-4 sub-wave count is 9 not 8 | W6 enumerates W6.1..W6.9 by grammar name (`bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`); W6 §tasks lists all 9 explicitly |

### §1.5 SYNTHESIS §0.4 P-1..P-7 pre-blocks — SPEC §10 fold

Per `SYNTHESIS.md:104-148`, the seven pattern-level pre-blocks bind
every wave. SPEC §10 reproduces the seven verbatim:

- P-1 Fake `@generated` header on hand-written templates (recurrence vector)
- P-2 `sonic_rs::from_slice::<Value>` mislabelled as strict comparator
- P-3 Tiny-fixture Criterion-overhead Mbps inflation
- P-4 Gate-relabel as admit
- P-5 Scaffold-research counted as load-bearing
- P-6 Per-grammar provider modules in generic codegen
- P-7 Track 1 ≡ Track 2 dishonesty

### §1.6 REDRESS watch-list (per dispatch-context §4 P3-E + §5 P-7 inheritance)

Per S-P3-DISPATCH-CONTEXT.md §4 P3-E + SK-V8 SPEC §10 specific-REDRESS-blocks, SPEC §10 must enumerate the SK-V8 inherited blocks + the SK-V14 dispatch-context watch-list:

- REDRESS 28+33 (Class A NEON/TBL tiny-string wiring as parse close)
- REDRESS 36-38, 85-86 (Lock 14 residue, old JSON helpers, generic JSON branches, `StructuralAlphabet::json`)
- REDRESS 49-55 (no-allocation visitor, parse-time aux side tables, EventCursor, parser-local structural-mask cursor, decoded stats sink, quote-source fused string materializer)
- REDRESS 59-72 (retained string-boundary collapse; direct source-hook/materialization families)
- REDRESS 80, 82-84 (raw f64 shortcut; single-quartet Unicode classifier; StringBlock16 tiny probe; object-pair value-byte control compaction)
- REDRESS 88, 89 (PMULL prefix-XOR default hot body; CTZ/bulk production consumer hardening)
- REDRESS 96-98 (full class-column vectors; streaming structural cursors; UnionTape-style retained structures; per Lock 1 v+1)
- REDRESS 119/120 (LIFTED per addendum; HISTORY only — closing a JSON row through 119/120 history requires fresh SK-V14 evidence per SYNTHESIS §5)
- REDRESS 126 (per V3 §1.4 CH3 NF-CH6-3 C2 scalar-ref evidence upgrade carry-through)

## §2 — Deliverable

The two artefacts SPEC.md + DISPATCH-PROMPT.md follow the SK-V8 SPEC
shape verbatim. P3-F V1 ships them at the same commit as p3{a..e}
research artefacts per `PASS-3-SYNTHESIS-PLAN.md:158-160` (V{N+1}
folding by aggregator).

### §2.1 SPEC.md structure (mirrors SK-V8)

```
§0 Close Condition And Goalset
  §0.1 Global Close Condition (R10 verbatim)
  §0.2 Comparator Classes (3 SK-V14 plane-correct classes)
  §0.3 Outcome Enum (SK-V8 enum + audit_overlay_verdict)
  §0.4 Required Telemetry (SK-V8 24-column + 4 SK-V14 additions)
  §0.5 Opening Row Goalset (AUDIT-ZERO baseline)
§1 Non-Negotiables (Lock 1 v+1 + Lock 14 v+1 + Lock 16 v+1 + executable-verification + CH7-V2 procedural)
§2 Wave Manifest, Caps, And Reruns (12-wave table)
  §2.1 Generality And Lock 14 Gate (per-wave exit checks)
§3 W0 Baseline Profile And Telemetry Lock
§4 W1 Comparator Rebind + Per-Iter Equality + PRUNE-1
§5 W2 regen-css xtask
§6 W3 Production CSS Corpora
§7 W4 PRUNE-2 (CSS revert + template delete)
§8 W5 PRUNE-3 (Lock-14 refactor)
§9 W6 PRUNE-4 (9 sub-waves)
§10 W7 PRUNE-5 (W8 + W9 wire-up)
§11 W8 CSS L4 Re-Admit
§12 W9 JSON Direct + Typed Re-Admit
§13 W10 JSON parse_only Distinct Path + Re-Admit
§14 W11 Close And Alpha Feedback
§15 Pre-Blocked Routes (P-1..P-7 + REDRESS watch-list)
§16 G-Alpha And Dispatch Scope (W0-only initial scope)
```

### §2.2 DISPATCH-PROMPT.md structure

```
§0 Authority + binding contracts
§1 Per-wave triumvirate contract (research → plan → redress)
§2 Phase caps and commit cadence (per SKINNY-TRIUMVIRATE.md §7)
§3 Same-wave consumer mandate (per §8)
§4 Per-wave dispatch envelope (the orchestrator's per-wave invocation contract)
§5 CHALLENGE invocation discipline (when to invoke between phases)
§6 Failure modes and escalation (round-trip rule trigger, abrogate-before-patch, no-orphan-redress)
§7 Status tick cadence (5-min orchestrator-silent wait per [status-tick-cadence])
§8 Dispatch ledger output (per-wave commit anchors)
```

## §3 — Falsifiability binding

V1 dispatch operates on the S-P2 LOCKED candidate pool directly. P3-A/B/C/D/E artefacts do NOT yet exist at V1 write-time (parallel dispatch per dispatch-context §3); V1 SPEC binds the falsifiability gates from S-P2 LOCKED candidates + SYNTHESIS §3 LOC envelopes:

- W0 gate: 51 JSON cells + 24 CSS L4 rows present in `SK-V14-open` baseline with all SK-V14 telemetry columns populated; throughput cells within ±1.0% of captured seed.
- W1 gate: 22 fake-admit rows reverted in ROLLING-SOTA-DELTA + RESULTS; 3 plane-correct strict comparators emit per-iter equality column inside timing region; `xtask gate-json` rejects rows missing `comparator_plane` or `per_iter_equality`.
- W2 gate: `cargo xtask regen-css` round-trip clean (`rm -rf … && cargo xtask regen-css && git diff` empty); `find skinny/xtask/src -name '*.rs' | xargs grep regen-css | wc -l > 0`.
- W3 gate: `du -sh skinny/corpora/css-l4-sk-v14 > 800 KB`; corpora cite Bootstrap + Tailwind + Material + Animate source URLs.
- W4 gate: `git grep -l '@generated by skinny bbnf-codegen' skinny/crates/codegen/src/css_l4_*_templates/ | wc -l == 0` (7 deleted); CSS L4 ADMITTED rows = 0 in ROLLING-SOTA-DELTA.
- W5 gate: `find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar' | wc -l == 0`; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l == 0`.
- W6.1..W6.9 gate: per-sub-wave: `find crates/core/src/runtime/{grammar}/ -name '*.rs' | wc -l == 0` (collapsed onto template emitted output); regen check returns empty diff.
- W7 gate: named pre-wave row (`json/numbers/direct_to_struct/main`) hot-leaf attribution shifts (samply trace) from `parse_value_at` to W11.1-number-specialised symbol; per-shape Lock-1 triad (`substrate_target`, `retention_lifetime`, `policy_owner`) declared in REDRESS.
- W8 gate: at least one CSS L4 feature ADMITs > strict-vs-strict against lightningcss full-parse on production corpora (≥800 KB) on same plane.
- W9 gate: every JSON direct + typed row that previously HELD under misbound comparator re-baselines under correct comparator; cells either re-ADMIT or revert with REDRESS.
- W10 gate: distinct parse_only code path exists in `generated_json` (no full-tape build); at least one JSON parse_only row ADMITs > sonic-rs Skipper-class on same plane.
- W11 gate: every W0..W10 wave has admitted/rejected/routed status; RESULTS + REDRESS + HANDOFF agree.

## §4 — Pre-blocked routes

SPEC §15 ledger inheritance per §1.5 (P-1..P-7) + §1.6 (REDRESS watch-
list). V1 P3-F draft binds the full set verbatim; V2 fold cycle
re-reads P3-E to refine specific-REDRESS bindings.

## §5 — Sources

- `restart/skinny/tranches/sk-v14/research/p3/S-P3-DISPATCH-CONTEXT.md` (88 lines)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` (407 lines)
- `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` (204 lines)
- `restart/skinny/tranches/sk-v8/SPEC.md` (812 lines — verbatim shape source)
- `restart/locks/LOCKS.md` (564 lines — Lock 1 + Lock 14 + Lock 16 v+1 binding)
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (210 lines)
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (276 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` (668 lines — §6 carry-forward packets)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (534 lines — §2 sequencing constraints + §3 PRUNE-list)
- `restart/skinny/tranches/sk-v14/HANDOFF.md` (245 lines)
- workspace HEAD `0a9f1288c62ef9f507854e8ccfebcfc78ba0a322`
