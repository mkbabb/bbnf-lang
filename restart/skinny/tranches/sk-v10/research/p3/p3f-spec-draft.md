# SK-V10 P3-F: SPEC/DISPATCH Draft Support

Pass: S-P3 Synthesis-Plan. Cycle: V1 support draft.
Date: 2026-05-19.
Scope: draft the SK-V10 SPEC Section 0 close condition, wave manifest,
non-negotiables, and DISPATCH-PROMPT outline for main integration.
Output: this file only. This draft intentionally does not create
`restart/skinny/tranches/sk-v10/SPEC.md` or
`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md`.
Pass Alpha goalset: `G-ALPHA-SK-V10` is closed; W3 is retired by REDRESS 98;
parse-only is diagnostic only; `direct_to_struct` is the primary JSON frontier;
typed-product generalization is the bounded second route; existing-substrate
kernel work requires micro-prove-first.
Candidate pool: `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
is the post-CHALLENGE S-P2 authority. Earlier P2 aliases are evidence only.

## Section 0 Draft - Close Condition And Goalset

### Section 0.1 - SK-V10 Close Condition

SK-V10 closes only when all of these are true:

1. `G-ALPHA-SK-V10`, S-P1 Profile, and S-P2 Research are closed, and S-P3 has
   produced the integrated `SPEC.md` and `DISPATCH-PROMPT.md` after CHALLENGE.
2. W0 has created or reaffirmed a coherent `SK-V10-open` report identity. If
   the run id remains inherited from W1-rendered `SK-V9-open`, the SPEC must say
   so explicitly; if a new `sk-v10-open:criterion-fnv64-<16 hex>` is minted,
   every row in `skinny/RESULTS.md` must carry coherent run id, host, flags,
   sample metadata, and gate-consumed provenance.
3. Every wave in the Section 2 manifest is either admitted by its named
   measurable gate, rejected with REDRESS measurement, or routed out of SK-V10
   by an explicit close disposition. No wave closes on "wired", "integrated",
   "promising", or future-phase evidence.
4. The six current `real_typed_struct A / GO` rows (`twitter`,
   `citm_catalog`, `apache_builds`, `update_center`, `mesh`, `marine_ik`) and
   the three current `direct_to_struct A / GO` guard rows (`citm_catalog`,
   `marine_ik`, `unicode_basic`) are preserved unless a same-wave gate records
   a measured REDRESS disposition.
5. Parse-only remains outside the SOTA close target while its rows are
   `S / NO-GO` with borrowed-view/deferred strictness. Parse-only profiles,
   PMU/cycles, structural-scan probes, and masking probes are diagnostic
   non-producers only.
6. Direct rows move only after the direct output/control-path contract exists.
   A direct admission requires same-run direct strict comparator evidence,
   generated Track 1, independent Track 2/oracle, matching output plane,
   measured validation path, `gate-json` consumption, and both Track 1 and the
   independent oracle meeting `ceil(sonic_direct / 1.10)` under one run id.
   Digest evidence never admits typed-product rows.
7. Typed rows move only with full generated/serde_json/sonic-rs/independent
   checksum parity over the full fixture, same-run typed comparator rows, and
   a typed output-plane gate. `instruments` is the first bounded target.
   `github_events` and `gsoc-2018` require root-model proof before row movement.
   `canada/real_typed_struct` remains pre-blocked until full-fixture proof
   exists.
8. Kernel or SIMD work enters a behavior wave only after micro-prove-first:
   scalar oracle, checkasm or equivalent differential harness, target-host
   feature gate, representative corpus slices, caller microbench, failure
   threshold, and same-wave production caller. W3, a renamed union substrate,
   a retained sidecar, and parse-only SOTA are not valid consumers.
9. Any wave touching the parse loop, aarch64 SIMD, string/unescape kernels, or
   shared byte-classification code preserves the W10b maintain block unless
   S-P3 tightens it:

   | Corpus | Maintain floor Mbps |
   |---|---:|
   | `canada` | 15866 |
   | `citm_catalog` | 28630 |
   | `instruments` | 15865 |
   | `marine_ik` | 11831 |
   | `mesh` | 12186 |
   | `numbers` | 17596 |

10. The SK-V9 36-field telemetry schema is inherited unless S-P3 changes it and
    `gate-json` consumes the change in the same wave. A producer-only field
    fails the wave.
11. Lock 1 and Lock 14 hold: one substrate, no sidecar or parallel retained
    producer, and no JSON policy in generic crates. Generic-crate, codegen, or
    runtime-outside-JSON edits require a named CSS L4 / Sheets / BBNF-self
    proof.
12. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SPEC.md`,
    `DISPATCH-PROMPT.md`, `HANDOFF.md`, and the SK-V10 Synthesis agree at
    close. The close record routes REDRESS 98 to Pass Omega and routes
    non-JSON generalization risk to the totality track.

### Section 0.2 - Opening Row Surface

The opening measured authority is the W1-rendered `SK-V9-open` report unless
W0 refreshes it as `SK-V10-open`.

| Family | Count | Opening posture | SK-V10 role |
|---|---:|---|---|
| `parse_only` | 17 | all `S / NO-GO` | diagnostic only; no SOTA close |
| `direct_to_struct` | 17 | 3 `A / GO`, 14 `N-direct / NO-GO` | primary JSON frontier |
| `real_typed_struct` | 6 | all `A / GO` | product-plane SOTA surface |

Direct row movement uses the P2-G direct floor table:

| Corpus | Direct floor Mbps |
|---|---:|
| `twitter` | 13840 |
| `canada` | 10977 |
| `apache_builds` | 10020 |
| `github_events` | 14364 |
| `update_center` | 10160 |
| `mesh` | 8916 |
| `random` | 7734 |
| `gsoc-2018` | 20980 |
| `instruments` | 11086 |
| `numbers` | 11788 |
| `unicode_mixed` | 9314 |
| `unicode_escapes` | 12527 |
| `distinct_values` | 10022 |
| `y_string_unicode` | 8027 |

The floor is necessary but not sufficient. A direct row also needs the output
contract, independent oracle, comparator strictness, validation path, same-run
metadata, and `gate-json` consumption.

### Section 0.3 - Goalset

| Goal | Binding shape |
|---|---|
| Preserve current wins | Maintain the six typed `A / GO` rows and three direct guard `A / GO` rows unless a same-wave measurement records a REDRESS disposition. |
| Establish direct contract | Make direct digest rows row-gateable by specifying output-plane equivalence, Track 2/oracle independence, same-run strict comparator evidence, and validation-path requirements. |
| Admit bounded typed rows | Attempt `instruments` first; attempt root-model proof for `github_events` and `gsoc-2018` before any typed row movement. |
| Micro-prove kernels | Prove one primitive family and one current caller at a time before production wiring. |
| Keep rejected routes dead | REDRESS 96/97/98 retire W3; REDRESS 66-69 block decoded scratch/source-hook families without material differential; REDRESS 82/83/88/89 block stale unicode/string/PMULL/CTZ routes under old framing. |
| Route beyond JSON | Omega receives the substrate-ceiling lock amendment; totality receives CSS L4 / Sheets / BBNF-self profiling and generalization proof. |

## Section 1 Draft - Non-Negotiables

- No W3, renamed W3, union/event substrate, retained class column, streaming
  cursor, class-lane-only fallback, W4-through-W3 cascade-lock, or W3 as a
  same-wave consumer.
- No parse-only row is a SOTA admission while the row remains `S / NO-GO`.
- No direct digest row is relabeled as typed product proof.
- No Apache/CITM-style row-table admission by analogy. Every new typed row
  needs full-fixture generated/serde_json/sonic-rs/Track 2 parity and same-run
  typed comparator evidence.
- No Canada typed shortcut through digest, length, coordinate count, schema
  shape, partial fixture, or numeric primitive proof.
- No behavior source change before the SPEC names owner paths, entry gate,
  exit gate, maintain floors, same-wave consumer, redress cap, and revert
  protocol.
- No kernel or primitive ships without scalar reference, checkasm/differential
  parity, target-host feature gate, representative corpus windows, microbench,
  and same-wave hot-path caller.
- No orphan primitive, orphan proof, sidecar producer, parser-owned fact slot,
  parser-owned structural cursor, second source pass, public substrate API, or
  parallel retained tape.
- No generic-crate JSON policy. Any generic-crate, codegen, or
  runtime-outside-JSON edit carries a named CSS L4 / Sheets / BBNF-self proof.
- No output-plane transfer: retained parse, generated direct digest, real typed
  product, and hand Track 2 oracle evidence are not interchangeable.
- No strict admission on stale, permissive, lossy, absent, historical,
  sidecar-only, view-boundary, PMU, cycles, masking-probe, structural-scan-only,
  or Criterion-slope evidence.
- No new telemetry field unless the same wave updates `gate-json` to require
  or consume it. No gate may require a field the wave does not emit.
- No new outcome variant unless the same wave updates all report, gate, and
  RESULTS consumers. Default SK-V10 posture inherits the SK-V9 10-identifier
  W0-admissible set: `A C G I J K L M N-direct S`.
- No wave closes on future work. A failed wave records REDRESS with measured
  evidence and a rejected patch route; it does not silently shrink scope.
- Research, plan, CHALLENGE when applicable, redress, and close remain distinct
  artefacts and commits under `SKINNY-TRIUMVIRATE.md`.

## Section 2 Draft - Wave Manifest

This manifest folds the local P3-B sequence as the primary ordering authority.
P3-C and P3-E group several gates under W4/W5-style headings, but the SPEC
should preserve P3-B's W0-W10 plus Close topology and bind the P3-C/P3-E gates
to the matching waves below.

| Wave | Draft SPEC section | Candidate ids | Title | Dispatch status | Row movement | Source/edit budget | Risk | Redress cap |
|---|---|---|---|---|---|---:|---|---:|
| W0 | Section 3 | `C12` | SK-V10-open Telemetry Freeze | Gate-only after S-P3 convergence | None; preserves all current dispositions | 120-240 gate/report LOC | LOW-MEDIUM | <=90 min |
| W1 | Section 4 | `C1` | Direct Output/Control-Path Contract | Conditional on W0 close | Contract-only | 180-320 docs/gate LOC | HIGH | <=90 min |
| W2 | Section 5 | `C1` | Direct Row-Table Reclamation | Conditional on W1 close | Direct rows only; zero behavior source | 120-240 gate/report LOC | MEDIUM | <=90 min |
| W3 | Section 6 | firewall | W3 And Parse-Only Firewall | Conditional on W2 close | None; proof-only governance gate | 80-160 docs/gate LOC | LOW | <=90 min |
| W4 | Section 7 | `C2` | `instruments` Typed Product Admission | Conditional on W3 firewall | May add one typed `A / GO` row | 160-260 source/generated + 40-80 gate LOC | MEDIUM | <=90 min |
| W5 | Section 8 | `C3` | Root-Type Typed Generalization Proof | Conditional on W4 disposition | Proof-only by default | 220-420 source/generated + 60-120 test/gate LOC | MEDIUM-HIGH | <=90 min |
| W6 | Section 9 | `C3` | Root Typed Row Admission | Conditional on W5 proof | One root-unblocked typed row at a time | 160-260 source/generated + 40-80 gate LOC per corpus | MEDIUM-HIGH | <=90 min |
| W7 | Section 10 | `C4` or `C5` | String Primitive Micro-Proof | Conditional on W3 firewall + CHALLENGE | Proof-only | 90-260 proof LOC | MEDIUM-HIGH | <=90 min |
| W8 | Section 11 | `C6` or `C7` | Escape/Segment Micro-Proof | Conditional on relevant W7 proof if needed + CHALLENGE | Proof-only | 90-260 proof LOC | HIGH | <=90 min |
| W9 | Section 12 | proven `C4`-`C7` | Existing-Call-Site Kernel Production | Conditional on relevant W7 or W8 proof and CHALLENGE | Direct/typed rows only with same-wave gates; parse-only stays `S / NO-GO` | 220-420 source/bench/gate LOC; split if more than one primitive, caller, plane, or target set is needed | HIGH | <=90 min |
| W10 | Section 13 | `C1` follow-on | Direct Residual Behavior Tranche | Conditional on W2 + W3 | At most three direct target rows per slice | 320 source/gate LOC, or 420 only with CHALLENGE | HIGH | <=90 min |
| Close | Section 14 | `C11`, docs | SK-V10 Close Accounting | Conditional on all dispatched waves | None beyond accepted wave dispositions | 80-160 docs/gate LOC | LOW | <=90 min |

Manifest rules:

1. W0 is the only initial behavior-adjacent dispatch. W1-W10 and Close are conditional
   until their entry gates pass.
2. W1 must exist before W2 or W10 can move direct rows. A direct row gate
   without W1 is a REVISE.
3. W3 is a firewall, not the retired W3 union/event substrate. The SPEC should
   avoid ambiguity by naming it "W3 firewall" everywhere.
4. W4 is the first bounded typed row movement. W5 is proof-only unless W6
   consumes it with same-wave typed comparator rows.
5. W7 and W8 are deliberately proof-only micro waves. A single proof wave may
   not combine multiple primitive families, scalar oracle, checkasm, microbench,
   and multiple production consumers unless CHALLENGE accepts the combined cost
   and the redress slice still fits 75 minutes.
6. W9 production wiring does not inherit any W3 entry gate. It consumes exactly
   one relevant accepted W7 or W8 proof for an exact `C4`-`C7` primitive and
   caller. `C8` and `C9` cannot feed W9 without a future SPEC/CHALLENGE
   amendment.
7. Any W9 production wiring touching parse loop or aarch64 SIMD carries the
   W10b maintain block. A single maintain-floor miss falsifies the wave.
8. W10 is a direct residual behavior tranche, not a license to reopen
   REDRESS 73 helper-transfer or REDRESS 93 scalar-parent folding.
9. `C10` and `C13` are inventory-only for SK-V10 on the Apple aarch64 host.
   `C14` and `C15` are rejected and must not appear in the manifest except as
   pre-blocked routes.

## Dispatch Prompt Outline

The integrated `DISPATCH-PROMPT.md` should use this outline and bind every gate
back to the SPEC rather than restating detailed thresholds in two places.

### Header

- State that this is the implementation-agent dispatch contract for
  `restart/skinny/tranches/sk-v10/`.
- State that `G-ALPHA-SK-V10`, S-P1, and S-P2 are closed, but source work is
  authorized only wave-by-wave after the SPEC entry gate passes.
- State that W3 is retired by REDRESS 98 and is not dispatch authority under
  any renamed framing.

### Required Reading

1. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
2. `restart/prompts/skinny/PASS-1-PROFILE.md`
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
5. `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
6. `restart/skinny/tranches/sk-v10/HANDOFF.md`
7. `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
8. `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
9. `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
10. The S-P3 cohort:
    - `research/p3/p3a-candidate-shortlist.md`.
    - `research/p3/p3b-wave-sequencing.md`.
    - `research/p3/p3c-falsifiability-gates.md`.
    - `research/p3/p3d-telemetry-schema.md`.
    - `research/p3/p3e-preblocked-ledger.md`.
    - the integrated SPEC and current p3 hardening consolidation.
11. The S-P2 report owning the requested wave:
    - W0: `p2g` C12 plus P1-F.
    - W1: `p2g` C1 plus P2-D/P2-F direct contract evidence.
    - W2: `p2g` C1 plus P3-C direct row gates.
    - W3: P3-B/P3-E firewall and REDRESS 96-98.
    - W4: `p2g` C2 plus Alpha-E Candidate 2.
    - W5-W6: `p2g` C3 plus P2-F root generalization verdicts.
    - W7-W9: the relevant `p2g` C4-C7 row plus P2-B/P2-C/P2-E process and
      primitive evidence.
    - W10: `p2g` C1 plus P3-C direct residual gates and P3-E direct pre-blocks.
    - Close: `p2g` C11, Synthesis Omega/Totality routing, RESULTS/REDRESS.
12. `skinny/RESULTS.md`
13. `skinny/REDRESS.md`, especially entries 94-98 and the candidate-specific
    historical blockers named in the SPEC.

### Wave Manifest Section

Include the integrated SPEC wave table with columns:

```text
Wave | SPEC section | Candidate id | Title | Dispatch status |
Entry gate | Row movement | Owner paths | Redress cap
```

The prompt should say: if the requested wave's SPEC entry gate is not PASS,
refuse dispatch and record the blocker. A conditional section is not dispatch
authority.

### Per-Wave Triumvirate Protocol

- Phase 1 Research: read-only; write artefacts under
  `restart/skinny/tranches/sk-v10/research/`; no source edits.
- Phase 2 Plan: select one SPEC intervention; name owner paths, gates, LOC,
  redress cap, same-wave consumer, micro-proof/checkasm requirements, maintain
  floors, and revert protocol.
- Phase 2.5 CHALLENGE: mandatory for W5-W10 when they touch source, for W7-W9
  primitive/kernel proof or production, and for any first-of-class source or
  generic-crate edit; optional only for purely gate/report W0-W3 slices and
  mechanical typed-row W4 if the SPEC says so.
- Phase 3 Redress: one implementation thread; edit only owner paths; measure
  with `RUSTFLAGS="-C target-cpu=native"`; update RESULTS/REDRESS only when the
  gate passes or the reject is recorded; save rejected patch artifacts when
  source is reverted.

### Refusal Conditions

The dispatch prompt should refuse any request that:

- reopens W3 or cites W3 as a consumer or entry gate;
- treats parse-only rows, PMU/cycles, masking probes, isolated structural-scan
  wins, or sidecar freshness as strict admission;
- moves direct rows before W1 direct output/control contract exists;
- moves typed rows without full generated/serde_json/sonic-rs/Track 2 parity;
- admits Canada typed without full-fixture proof;
- ships a kernel without scalar/checkasm/microbench/same-wave caller;
- emits a telemetry field without same-wave gate consumption;
- leaks JSON policy into generic crates without CSS L4 / Sheets / BBNF-self
  proof;
- edits outside the SPEC owner paths.

### Close And Escalation

- Every wave ends in PASS, measured REDRESS reject, or explicit route-out.
- A gate that cannot be made measurable returns REVISE before source work.
- A wave that exhausts its redress cap halts with the current artefact and asks for
  an extension or split.
- The bracket closes only after W0-W10 and Close dispositions are reflected consistently
  in RESULTS, REDRESS, SPEC, DISPATCH-PROMPT, HANDOFF, and Synthesis.

## Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/skinny/tranches/sk-v9/SPEC.md`
- `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v10/research/g-alpha/G-ALPHA-PRESENTATION.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Self-verdict: support draft ready for main S-P3 integration; not a promoted
SPEC or DISPATCH-PROMPT.
