# SK-V15 Grand Synthesis

Date: 2026-05-27.

Status: SK-V15 W11 close packet landed. SK-V14 closed full-admit in the row
ledger, but PASS-IMPL V1 rejected the CSS leg as a generalisation claim.
SK-V15 executed as a PRUNE-then-REBUILD tranche and closes as
`ADMIT-W11` with routed blocks, not as the CSS inflection point.

## Authority

Read in this order; later entries override where they conflict:

- `restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md`
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
- `restart/prompts/skinny/PASS-IMPL-OVERFIT-AUDIT.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W11-close.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/locks/LOCKS.md`

The user pin controls gate conflicts: only G-Omega is mandatory during
this execution; G-Alpha auto-passes under
`restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md:11`.

## Section 0 — Close Condition And Goalset

### 0.1 Close condition

SK-V15 closes only when the following are all true:

| Gate | Close condition |
|---|---|
| JSON guard | 51 / 51 JSON rows remain admitted, strict, and same-plane; any touched JSON row is re-run against its strict comparator |
| CSS admission honesty | no 24-row broadcast admit remains; CSS rows are one honest aggregate row or independently timed per feature |
| CSS parser truth | `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()` output, and brace-counter admission are retired from live CSS admission |
| CSS Value API | CSS exposes typed value/document/view/visitor surfaces isomorphic in capability to JSON's Value API |
| CSS SOTA | cssparser is the same-workload near-term comparator; lightningcss counts only after Track 1 emits comparable CSSOM/value output |
| Native platform | admission and SIMD claims are measured on Apple M5 Max / aarch64 only; x86 and AVX-512 rows are not admission anchors |
| Lock 14 / Lock 16 gates | generic scan roots include previously excluded leak files, every exclusion is itself reported, and any self-exempting grep/checkasm gate rejects close |
| Codegen neutrality | no JSON/CSS runtime mode split, no per-grammar regen enum/match fanout, no 7-arm CSS profile control match, no generic pass JSON-byte recognizer remains unabstracted |
| Pattern H | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` remains 67, and every file carries generator provenance at line 1 |
| Decision Engine | e-graph rewrite count >= 1; CSP is non-tautological; grammar-named CSP facts are gone; all five BackendShape lowerers are real |
| FNV quarantine | W11L/W11N/W11O FNV closed-enum products stay bench-only and the strict-product comparator catches closed-enum sidecar coupling |
| Executable close evidence | every close row cites HEAD command output, generated artefacts or diffs where relevant, and cold per-parse measurement evidence; documentation-only close is rejected |
| PASS-IMPL V2 | close audit returns ACCEPT on every axis or supplies row-level intrinsic-block proof |

No implementation-limited miss closes SK-V15. A miss becomes REDRESS with
evidence and an immediate receiving route.

### 0.2 Starting state

| Surface | SK-V14 ledger | SK-V15 bracket |
|---|---:|---|
| JSON parse_only | 17 / 17 admitted | validated guard baseline |
| JSON direct_to_struct | 17 / 17 admitted | validated guard baseline |
| JSON real_typed_struct | 17 / 17 admitted | validated guard baseline |
| CSS L4 | 24 / 24 admitted | audit-demoted; reopened |

The CSS demotion is not optional. PASS-IMPL V1 identifies one measurement
broadcast 24 times, a 646-line hand-written CSS tokeniser embedded as a
string literal, and a workload mismatch against lightningcss CSSOM
(`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`,
`:29`, `:31`).

### 0.3 Receiver goalset

| Receiver | Obligation |
|---|---|
| PRUNE-WAVE-A | Retire CSS broadcast admits by defaulting the current 24 rows to one diagnostic aggregate; independent feature admits require typed CSS output and distinct measurements |
| PRUNE-WAVE-B | Restore Lock 14 / Lock 16 gate coverage by removing silent scan-root exclusions and surfacing every exclusion as a finding |
| PRUNE-WAVE-C | Abrogate codegen leaks: grammar-family runtime modes, root `RuntimeStyle`, hardcoded CSS profile table, generic pass JSON-byte literals |
| PRUNE-WAVE-D | Enforce Pattern H generated discipline across all 67 root runtime files; eliminate fake or header-only generated status |
| REBUILD-WAVE-E | Build CSS typed Value API and retime CSS on typed output |
| REBUILD-WAVE-F | Activate Decision Engine and implement all five BackendShape lowerers |
| REBUILD-WAVE-G | Quarantine FNV closed-enum bench products and harden strict-product differential |

### 0.4 Alpha cost envelope

Pass Alpha shortlists five candidate packages. S-P3 may split them into the
seven named receivers above, but every split must carry the hard-cap envelope:
research <=20 minutes, plan <=15 minutes, redress <=30 minutes per wave or
sub-wave; commit at 0.9N and halt at N. Any wave that cannot prove its exit
gate records REDRESS, reverts or blocks its source changes from downstream
consumption, and cannot be used as evidence by later waves.

The CSS parser retirement is indivisible with typed rebuild capability:
`CSS_GENERATED_RS`, fact-stream-only `parse()` output, and
`CssFullParseSummary` may be removed from live admission only in the same wave
that proves the typed CSS value/document path, or after that proof has already
landed. The diagnostic 24-row collapse can happen first because it removes an
admission claim rather than deleting a provider.

### 0.5 Procedural addenda

SK-V15 carries three new CHALLENGE addenda:

- `NEW-CH3-V5-01`: wave-graph cycle detection. A wave deleting artefact X
  must prove rebuild capability for X is delivered no later than that wave.
  S-P3 must emit an artefact dependency table with these columns: retired or
  deleted artefact, delete/retire wave, rebuild provider wave, proof command,
  and evidence that the provider lands no later than the delete/retire wave.
- `NEW-CH5-V5-02`: broadcast-admission detection. N admits require N
  distinct measurement rows unless the row is explicitly aggregate.
- `NEW-CH7-V5-03`: gate-exclusion detection. Lock 14 / Lock 16 gates must
  scan and report their own exclusion lists.

## Section 1 — Validated And Invalidated Ledger

JSON is the durable proof-of-concept. CSS is not. The validated JSON wins
become guard rows; the invalidated CSS state becomes the primary prune
surface. Pattern H, Lock 14, codegen neutrality, and Decision Engine
activation are implementation gaps, not future-tranche wishes.

## Section 2 — Telemetry Binding

SK-V15 uses the Pass Alpha row schema and adds CSS anti-broadcast fields:
`measurement_row_id`, `measurement_origin`, `value_plane`,
`css_comparator_workload`, `generator_source`, `lock14_scan_scope`,
`lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and
`broadcast_group_id`. `gate-json` or the successor gate must reject rows
missing required telemetry, self-exempting exclusion reports, and hidden
one-to-N measurement stamps.

## Section 3 — Trajectory

SK-V15 is the prune/rebuild cycle required before grammar-driven
generalisation. If SK-V15 closes cleanly and PASS-IMPL V2 accepts every
axis, SK-V16 can become a grammar-driven generalisation tranche. If
SK-V15 surfaces new contrivances, PASS-IMPL V2 routes them as SK-V16
prune inputs. Under the latest user extension, SK-V17 remains in the loop
until CSS, Pattern H, Lock 14 / Lock 16, and Decision Engine all satisfy
the no-contrivance bar.

## Section 4 - W11 Close Packet

W11 consumed `DEP-W11-CLOSE-NO-ORPHANS` through
`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-close-dependency-checklist.json`.
PASS-IMPL V2 returns `ACCEPT-SK-V15-CLOSE-WITH-ROUTED-BLOCKS`, recorded at
`restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md`.

SK-V15 close facts:

- JSON remains 51 / 51 strict measured rows.
- CSS L4 admitted rows remain `0`; W11 retime re-proved Track 1 `2/4`,
  cssparser `4/4`, unequal typed summaries, and `admitted_rows=0`.
- Lock count remains `16`.
- Pattern H runtime census remains `67` and line-1 provenance scan returns no
  bad rows.
- BackendShape canon remains exactly five shapes.
- W7 Decision Engine, W8/W9 lowerers, and W10 FNV quarantine have executable
  close evidence.

SK-V16 begins from routed remainder because PASS-IMPL V2 did not accept every
axis as perfected. Grammar-derived CSS, typed CSS equality, full Pattern H
collapse, dirty generated CSS retirement, and FNV production-migration block
remain first-class SK-V16 inputs.
