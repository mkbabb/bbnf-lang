---
artifact: HARDENING-T-P1-CONSOLIDATED
pass: T-P1 (SK-V18 totality excavation)
role: aggregator (post V6..V8 convergence continuation)
generated_at: 2026-06-01
close: NOT-FULLY-CERTIFIED (sound inventories, no real standing reject, bounded precision-churn)
streak_reached: false
consec_at_close: 0
voids: 0
inventories:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md (+ 1F-anti-pattern.md, 1F-past-corpora.md)
---

# T-P1 Totality Excavation — Hardening Consolidated (SK-V18)

The six evidence inventories (1A substrate, 1B codegen, 1C runtime, 1D skinny
lessons, 1E locks, 1F coherence + two auxiliaries) catalogue every V1-spec ↔
live-implementation divergence the SK-V18 GENERALIZATION absorbs (one
grammar-driven generator emitting JSON+CSS+Sheets from `.bbnf`; un-fork
`RuntimeEmitterKind`; DELETE the phantom `<G>`; retire the CSS const courier and
the 7 byte-identical replicas; aarch64-only). They were hardened through an
in-workflow CH1–CH7 run (V1–V5) and then a CH1–CH6 convergence continuation
(V6–V8) that drove out the residual precision REVISEs toward a §3Z
two-consecutive-clean fixed point.

## Per-Cycle Acceptance (r)

`r = accept / (accept + revise + reject)`, summed over all challenge findings in
the cycle; convergence requires `r ≥ 0.95 AND reject == 0` for two consecutive
cycles.

| cycle | run | lenses | A / R / X | r | converged | note |
|---|---|---:|---|---:|---|---|
| V1 | in-workflow | CH1–CH7 | 35 / 28 / 1 | 0.546 | no | initial excavation; whole-lens 0/7 ACCEPT |
| V2 | in-workflow | CH1–CH7 | 41 / 19 / 3 | 0.650 | no | major V1 blocker classes folded; 1/7 |
| V3 | in-workflow | CH1–CH7 | 41 / 21 / 1 | 0.650 | no | grammar-neutral routing + cost keying folded; 1/7 |
| V4 | in-workflow | CH1–CH7 | 38 / 17 / 1 | 0.678 | no | cost/primitive/FNV folds; CH1 still REVISE; 1/7 |
| V5 | in-workflow | CH1–CH7 | 22 / 8 / 1 | ≈0.709 | no | CH1/CH6/CH7 prose-verdict (no TALLY line); 3/7 |
| V6 | continuation | CH1–CH6 | 43 / 2 / 1 | 0.934 | no | r<0.95 AND reject≠0 (the CH4 FAB self-falsified line); consec→0 |
| V7 | continuation | CH1–CH6 | 47 / 0 / 0 | 1.000 | yes | first clean cycle; consec→1 |
| V8 | continuation | CH1–CH6 | 46 / 4 / 0 | 0.920 | no | 4 CH1 intra-doc anchor nits; r<0.95; consec→0 |

The in-workflow V1–V5 loop monotonically improved (0.546 → ~0.709) but never
crossed 0.95; the continuation re-baselined the inventories (commit `097c4dd90`,
"V1-V5 hardened, near-converged") and pushed to a near-fixed point. **V7 was the
lone fully-clean cycle (r=1.000); V8's four single-locus anchor nits broke the
streak before a second clean cycle landed.** Close state: `consec=0, voids=0,
converged=false`.

## Divergence Census (summed across 1A–1F)

Per-inventory frontmatter `divergence_count` (1A/1B/1C use
`implemented/unimplemented/impl_exceeds_spec`; 1D/1E/1F use the isomorphic
`spec_claims_*`), summed:

| bucket | 1A | 1B | 1C | 1D | 1E | 1F | total |
|---|---:|---:|---:|---:|---:|---:|---:|
| spec_claims_implemented | 7 | 6 | 8 | 1 | 2 | 1 | **25** |
| spec_claims_unimplemented | 8 | 7 | 11 | 10 | 11 | 9 | **56** |
| impl_exceeds_spec | 1 | 2 | 1 | 3 | 2 | 0 | **9** |
| unknown | 3 | 3 | 4 | 5 | 3 | 1 | **19** |
| partial | 5 | — | 2 | — | — | — | **7** |

1F additionally carries one `split_implemented_json_directional_css` row
(COH18-013: JSON 51/51 strict cold is bench-row-backed/implemented; the CSS
1.9–3.3× ratio is measurement-valid but DIRECTIONAL, NOT re-locked — the H1
`css_canon_bench` gate is the falsifier).

**Census headline: 25 implemented · 56 unimplemented · 9 impl_exceeds_spec**
(+ 19 unknown, 7 partial, 1 split). The overwhelming `unimplemented` weight is
the SK-V18 generalization surface itself — the un-forked generator does not yet
exist, the 4 marker-string lowerers carry no Rust bodies, the BIR is 13-of-20
variants and the Grammar-IR 8-of-13, Pattern H is 67-per-grammar, the CSS courier
+ 7 replicas + phantom `<G>` all live. These are the SK-V18/SK-V19 deliverables,
not inventory defects.

## 1E LOCKS-AMENDMENTS-CANDIDATE Table (count + load-bearing candidates)

1E surfaces **7** candidates (the canonical table); 1A surfaces **1** companion
(1A-LOCK1-AMEND-001), for **8** total amendment candidates across the pass. All
are CANDIDATES ONLY — disposition T-P3, ratification Pass Omega; the 16-lock
count and 5-shape `BackendShape` canon are preserved, no lock is added/retired,
no 6th shape, no new substrate/directive/BIR variant. They bind the SK-V18
GENERALIZATION discipline absent from the lock surface:

| candidate | type | target locks | load-bearing thrust |
|---|---|---|---|
| LAC-1E-V5-01 | addition | L14, L16, L08 | Bind the §6 named-primitive (a)–(d) gate (grammar-invoked / output-varies-under-rule-mutation / `verbatim_blob_present==false` / profile-proven-narrow-leaf) — the single largest paper-close surface. |
| LAC-1E-V5-02 | addition | L05, L14, L01 | Bind the relocated-seam firewall: `render(program)` reads shape ONLY from `policy_summary.backend_shape`, never a `RuntimeTarget`/`profile`/`emitter` field; `runtime_target_rows_collapsed` full-row PartialEq co-gate (md5-distinctness is necessary-not-sufficient). |
| LAC-1E-V5-03 | addition | L14, L16 | Bind the neutrality-proof obligation: a neutrally-named single-grammar primitive must be proven by ≥1 non-that-grammar invocation or honestly demoted (the `balanced_component_scan`→`css_balanced_component_scan` forced demotion). |
| LAC-1E-V5-04 | refinement | L16, L08 | Sharpen x86 standing to aarch64/M5-Max ONLY; the whole `src/x86_64/`+`ext/x86/`+nasm+`diagnostic-x86` surface is a DELETION target, not a measured plane. |
| LAC-1E-V5-05 | refinement | L06, L14 | Strengthen the verbatim-blob prohibition: a `@generated` file that is a verbatim `&str` literal is hand-written, REJECT as "grammar-driven"; round-trip byte-equivalence is the binding proof. |
| LAC-1E-V5-06 | refinement | L14 | Bind the green-by-exclusion fix as a precondition: move codegen surfaces from weak `SKV15_W2_EXTRA_COVERAGE_ROOTS` into strict `GENERIC_SCAN_ROOTS`, extend `FORBIDDEN_GENERIC_TOKENS`, drop `diagnostic-x86`, prove by a re-inject-then-revert RED falsifier. |
| LAC-1E-V5-07 | refinement | L13, L14 | Re-key the Pattern-H census invariant: the 67-file baseline drifted to 71; the +N must trace to a grammar-roster/sub-wave change else an O(N) generator-regression scan opens. |
| 1A-LOCK1-AMEND-001 (1A companion) | refinement | L14 (§9.2 prose) | Strike "The `G:EventGrammar` type parameter is the generality vehicle" (`LOCKS.md:620`) — the certified plan DELETES `<G>`; re-anchor the generality claim onto the shared `Cursor` micro-trait + the config-breadth classifier. ≈ −1..+5 LOC; cross-links 1F COH18-008. |

The two most load-bearing are **LAC-1E-V5-01** (the named-primitive (a)–(d)
machine-checked gate — the largest paper-close escape) and **LAC-1E-V5-02** (the
relocated-seam firewall — the only check that catches a per-grammar branch
relocated into a neutral data table, which the arm-grep is syntactically blind
to). The 1A companion (`1A-LOCK1-AMEND-001`) is the direct LOCKS↔plan
contradiction: `LOCKS.md:620` names the phantom `<G>` as the generality vehicle;
`sk-v18/SPEC.md:1202-1207` DELETES it.

## Disposition of Every REVISE / REJECT Folded

Across V1–V8 every REVISE that named a fold was applied in place to the inventory
file(s), citing a live path:line. The standing residue at V8 close is the four
CH1-V8 intra-doc anchor nits (below); no other REVISE stands and no genuine
REJECT was ever raised.

- **V1–V5 in-workflow folds (all discharged).** Stale 1F auxiliary status
  resolved (`1F-coherence-scan.md` made authoritative); root-resolving
  citations expanded; frontmatter counts reconciled; REDRESS/Lock-1 fences
  attached to EventTape / typed-event-cursor / structural-scanner rows;
  grammar-neutral vs JSON/CSS-empirical separation enforced (Lock 14); LOC/risk
  bands added to every divergence row; 1E LAC wave-alignment hints + path:line
  added; Lock-1/Lock-5 wording downgraded from `honoured` to scoped partial;
  the FNV/hash census fenced as telemetry/quarantine, not value/identity/equality.
- **V6 (r=0.934).** CH2-V6-R01 folded: `1F-anti-pattern.md:65` narrow-regex catch
  count corrected from the stale **5** to **4** (`:137,:143,:149,:155` caught;
  `:161,:167,:173,:179,:185` escape), reconciling it with the authoritative
  `1F-coherence-scan.md:79,:86`. CH4-V6-001 folded: `1E:108` bare `find
  crates/bbnf-simd/...` prefixed to `skinny/crates/bbnf-simd/...` (the 28/4401
  figures were already TRUE via the skinny path).
- **V7 (r=1.000).** All six lenses clean; zero REVISE, zero REJECT. Folds above
  verified landed; first §3Z-clean cycle.
- **V8 (r=0.920).** Four CH1 INTRA-INVENTORY navigation-anchor nits (a "see
  <inventory>:<line>" pointer drifted by a row/section) — the content each row
  asserts is disk-correct; only the cross-reference pointer is off. These are the
  residual the V6/V7 passes (which probed external spec↔impl rows, not
  self-citation anchors) did not independently test. CH2–CH6 all clean. These are
  REVISE, never REJECT; they are bounded single-locus precision churn.

## The V1–V5 CH4 "reject" Is a Convention Artifact, Not a Defect

The lone non-zero `reject` count in the continuation traces to the CH4 cost lens
recording a **self-falsified fabrication-suspicion** under the (later-corrected)
verdict convention. CH4 probes whether any cited LOC/cost figure is recalled or
fabricated; on every load-bearing row (builder 817, css_types 66, StructLayout
960, Pattern-H 71/67, x86 28/4401, simd-scan 217, OnceCell 8/9, Lock-14 falsifier
13, courier `:701`, …) the figure **matches disk verbatim**. Finding that a
figure is NOT fabricated is an ACCEPT of the inventory's correctness — but the
early convention logged the discharged suspicion as a `REJECT` line
(`CH4-V5-014`, re-issued V6 as `CH4-V6-FAB`). It records "no inventory STATES
anything false on disk"; it is the inventory-is-correct outcome, NOT a reject of
any inventory claim. The corrected convention (carried in the V6+ continuation,
the certify driver, and the fold prompt) is explicit: *a CH-style "reject" that
merely records a self-falsified fabrication-suspicion is NOT a defect — there is
nothing to fix.* It nonetheless forced `reject≠0` arithmetically in V6, breaking
that cycle's convergence; V7 carried it correctly as ACCEPT and went fully clean.
There is **no real standing reject** anywhere in the pass.

## Close & Next Move

**NOT-FULLY-CERTIFIED.** The §3Z two-consecutive-clean streak was NOT reached:
V7 alone hit r=1.000/reject=0; V8's four anchor nits (r=0.920) reset the streak
to consec=0 before a confirming clean cycle, and V8 is the continuation ceiling
(v<8 loop bound, voids=0). The honest close: the inventories are **sound** (every
external spec↔impl, REDRESS/RESULTS ledger, md5, LOC, and lock-text citation
re-grounds on disk; the only residue is bounded single-locus intra-doc anchor
precision-churn), there is **no real standing reject** (the CH4 line is the
self-falsified-suspicion artifact above), and the divergence census + the 8
amendment candidates are fully priced, wave-keyed, and T-P3/Omega-dispositioned.

**READY-FOR-T-P2.** T-P2 Research may dispatch against this packet, carrying the
governance note that T-P1 closed near-converged (lone clean cycle V7, no
two-cycle §3Z lock) and that the four V8 CH1 anchor nits are a bounded,
non-blocking precision fold to settle alongside the T-P2 read.
