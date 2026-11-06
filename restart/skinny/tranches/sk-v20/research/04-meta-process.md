# SK-V20 Research — 04: Meta-Process Audit

Auditor: 04-meta-process. Date: 2026-06-10. HEAD: 36ca10646.
Scope: the SK meta-process loop from v3.5 through v19 — the PASS-IMPL/OVERFIT/AUDIT →
Pass Alpha → S-P0..3 → T-P1..3 → Pass Omega → G-Omega → wave-implementation cycle.

---

## §1 — The quantitative picture

### 1.1 Doc mass vs code mass per cycle

The table below uses `git log --numstat` over each tranche's date window. All figures
are lines added; doc lines count `.md` only; code lines count `.rs` only.

| Cycle | Doc lines | Code lines | Doc/Code ratio | Tranche doc files |
|---|---:|---:|---:|---:|
| SK-V14 (2026-05-20..27) | 249,113 | 59,168 | **4.2×** | 477 |
| SK-V17 (2026-05-28..30) | 78,465 | 22,983 | **3.4×** | 135 |
| SK-V18 (2026-05-31..06-03) | 69,091 | 9,429 | **7.3×** | 163 |
| SK-V19 (2026-06-04..10) | 279 | 520 | 0.5× | 3 |

SK-V18 produced 7.3 lines of documentation for every line of Rust. SK-V19 inverted the
ratio — but only because it shipped three partial W6 patches before stalling and opening
SK-V20 as a 32-agent research pass.

Across the entire restart corpus (`restart/`) there are 589,099 lines of Markdown. The
code the campaign guards — `crates/` — has 329,920 lines of Rust. The documentation
outweighs the code it documents by roughly 1.8:1 at the campaign level.

### 1.2 Hardening volume

The §3Z convergence protocol requires 6 challenge-agents × up to 5 V-iterations per
pass × 6..9 passes per cycle. At observed rates:

- All tranche `hardening/` directories combined: **143,788 lines** across 971 files.
- The astral G-Omega hardening layer (auditing the Omega audit): **12,307 lines**.
- The `restart/audit/hardening/` root: **17,373 lines**.
- Total hardening-only documentation: approximately **173,000 lines**.

The SK-V18 Pass-Alpha hardening alone ran to 6 iterations (V1-V5 plus a confirming
wave; `sk-v18/research/alpha-hardening/CONSOLIDATED-CONVERGED.md`): 14,581 lines
across 46 files to certify one planning document.

---

## §2 — Re-litigated items (3+ cycles)

### 2.1 Lock 14 generalisation

Lock 14 (zero grammar-specific code in any generic crate) was introduced on 2026-05-03
(`74f2ed25c`) and first worked in sk-v7 (2026-05-16). As of HEAD 36ca10646 the
`crates/ir` generic crate now generates its grammar table from workspace metadata
(`build.rs`). However:

- `crates/ir/src/passes/recognizers/grammar_facts.rs` and
  `crates/ir/src/passes/recognizers/shape_dispatch/scalar.rs` retain grammar-name
  occurrences (sk-v19 PASS-IMPL-FOLD-READINESS:26). The W6c structural re-key fixed
  `shape_dict_bbnf.rs` (`f4633d468`) but the wider IR recognizer pass set is unaddressed.
- The `crates/egraph` and `crates/csp-solver` crates exist but remain disconnected from
  `crates/ir/src/passes/recognizers/` (`rg backend_shape crates/ir` = 0). The decision
  engine has been "active pending" since SK-V15 (MASTER-PLAN `MP.SK15.W8/W9`).

Lock 14 has appeared as a live compliance target across tranches v7, v8, v9, v10, v11,
v12, v13, v14, v15, v16, v17, v18, and v19 — thirteen consecutive tranches.

### 2.2 SoA Tape (AoS TapeRec retirement)

The 16-byte AoS `pub struct n` in `crates/core/src/runtime/tape/record.rs` was first
targeted for retirement in the SK-V15 receiver block (`MP.SK15.W0`), re-stated in
SK-V16 REDRESS-53, re-stated again in SK-V17 ("dual-encoding hazard"), classified as
a GREENFIELD ADOPT for SK-V19 (`audit/PASS-IMPL-FOLD-READINESS.md:16-22`), and named
W2 of SK-V19. At HEAD 36ca10646 the AoS tape record is unchanged. The deferred-census
agent (`05-deferred-census.md`) classifies this CHRONIC across 4+ cycles.

### 2.3 Grammar-agnostic value-plane generator (C fold)

Deferred from SK-V17 "on the >SOTA-preservation burden," re-deferred as SK-V18 work,
and now named W3 of SK-V19. `crates/core` has zero `ValueRef` and zero `Cursor`.
The skinny tree's hand-authored `json_templates/` couriers survive intact. Even in the
skinny tree, Phase 3 is "pending" (`PASS-IMPL-FOLD-READINESS.md:27-28`). Three cycles
of explicit deferral; the C-fold is the single largest pending architectural item.

### 2.4 BackendShape / decision engine

Described in the MASTER-PLAN as MP.SK15.W7/W8/W9, the BackendShape enum and the
egraph+CSP selector have been "pending" in `crates/ir` since SK-V15. `rg BackendShape
crates/ir/src` = 0 at HEAD. The five-shape selection exists only in the skinny proving
tree (`skinny/crates/ir/`). W5 of SK-V19 names this as a 0→N introduce; no wave
covering it has landed outside the skinny prototype.

---

## §3 — Audit-of-audit recursion depth

The meta-process produces the following chain of audit layers for a single planning
document:

1. **Pass-level research agents** (e.g. S-P2 six agents) produce the primary artefact.
2. **CHALLENGE wave** (6 CH agents per pass) audit the primary artefact via CH1-CH6.
3. **CONSOLIDATED.md** aggregates the six CH verdicts.
4. **V{N+1} fold** updates the primary artefact with REVISE dispositions.
5. Steps 2-4 repeat up to V5 per pass.
6. **Overfit audit** (S-P0 or `audit-overfit/`) audits the goalset, which itself audits
   prior cycles.
7. **Pass Omega** synthesises all passes for a given cycle into locks+plan deltas.
8. **G-Omega hardening** (6 CH agents, up to V5 iterations) audits Pass Omega.
9. **Astral corpus** (ΩA–ΩF per version V1..V10) consolidates G-Omega across cycles.

This constitutes at minimum a 4-level chain: primary → challenge → omega → astral.
The astral layer itself receives a hardening wave (e.g. `V10/hardening/V1..V5/CH1..CH6`,
totalling 30 files just for V10). At observed average densities:

- One full pass including its challenge convergence: ~2,800 lines of documentation.
- One full SK cycle (alpha + S-P0..3 + T-P1..3 + Omega + G-Omega): ~40,000 lines.
- The G-Omega astral layer has accumulated 10 versions × ~600 lines = ~6,000 lines
  whose sole purpose is auditing the Omega audit of the cycle audits.

There is no evidence that the astral layer has invalidated a Pass Omega conclusion or
triggered a code correction. Its value is undemonstrated.

---

## §4 — Cycles since a fleet-scale capability shipped in crates/

The `crates/` tree (the totality fleet serving 9 grammars) last received a substantive
non-generated-file, non-test, non-doc commit that introduced new generic infrastructure
at the `AZ-IV` tranche (the wave preceding the restart campaign). Since the restart
(SK-V3.5 onward), the pattern is:

- SK-V14 (2026-05-20..27): generated grammar files regenerated (9 grammars), builder
  templates touched, Lock-14 gate tests added. The additions are wave ledger bookkeeping
  and test coverage, not new generic infrastructure.
- SK-V16 (2026-05-29): flat tape substrate added to skinny; `crates/core` builder files
  received the O(1) speculative-checkpoint (a builder optimisation, not a new layer).
- SK-V19 (2026-06-07..10): W6 lock patches — manifest table generation from build.rs,
  `css_types.rs` relocation, Lock-14 test gate. These are cleanup/gate items, not new
  generic capability.

Generic capabilities specifically planned (BackendShape, SoA Tape, C-fold value
generator, egraph+CSP wired into IR passes) have not shipped to `crates/` in any cycle
of the restart campaign. The skinny tree has prototyped these, but the prototype has not
been carried back. The PASS-IMPL audit (sk-v19) explicitly stated: "crates/core has NOT
inherited it — it is at an EARLIER state."

The last fleet-scale generic capability that landed in `crates/` was the
`bbnf-path`/`StructRegistry` work and the IR-passes substrate-audit test from the AZ-IV
tranche (`407cdc8e7`, 2026-05-22 at the latest commit in that lineage). That is now
three weeks in the past and predates the restart campaign's tranche numbering.

---

## §5 — Local-maxima and stall signals

### 5.1 Documentation inflation as a stall signal

The §3Z auto-convergence protocol is the primary driver of documentation inflation.
Requiring two consecutive 95%-ACCEPT cycles across 6 challenge-agents, with a V≤5
ceiling, means each pass can generate 5 × 6 = 30 hardening documents before the
ceiling triggers escalation. At 9 passes per cycle the theoretical maximum is 270
hardening documents per cycle before any code is written.

In practice the SK-V18 alpha-hardening ran to V5+confirm, consuming 14,581 lines, to
certify one document that describes a generalisation goalset. The goalset itself did not
change between V1 and V5; the convergence iterations re-verified the same findings
against the same evidence at finer precision. This is documentation-driven local maximum
behaviour: the process rewards evidence accumulation over evidence falsification.

### 5.2 The overfit audit itself requires auditing

The overfit audit (`audit-overfit/`, `restart/audit/skinny-impl-overfit/V1-V3`) has
three versions, each with six agent reports and a consolidated. The V3 audit's primary
finding was that "the architecture is AT the inflection point" — a finding that opened
SK-V18. SK-V18's S-P0 then audited the V3 audit's conclusions. The overfit audit of
the overfit audit. This recursion is benign in isolation but contributes to the
documentation mass without adding code-level corrective surface.

### 5.3 CSS SOTA is structurally OPEN

The ROLLING-SOTA-DELTA carries 24 CSS rows all marked OPEN. The note at line 97
(`ROLLING-SOTA-DELTA.md:97`) states: "CSS stays OPEN until a fresh typed row beats
cssparser with typed-summary equality." SK-V17 produced per-corpus lightningcss
comparisons showing 2.0–3.3× margins, but the formal SOTA delta table was never
migrated to that schema — it still broadcasts one stale diagnostic tuple across all 24
rows. The CSS SOTA claim exists in narrative prose in a footnote to the table, not in
the gate-machine rows the `gate-json` consumer reads. This is not a fabrication; it is
a migration that has been deferred across SK-V17, SK-V18, and SK-V19.

### 5.4 The skinny-to-totality gap is widening

The skinny tree and the `crates/` tree are diverging architecturally. The skinny tree
has the SoA tape, ValueRef/Cursor, BackendShape, the un-forked emitter, and the G6 NEON
classifier. The `crates/` tree has none of these. Every cycle that runs the full
research-plan-harden-omega loop in the skinny context adds more proven skinny
architecture that must later be ported, without narrowing the gap. SK-V19 was designed
to close the gap but landed only 3 W6 patches before opening a 32-agent research cycle.

---

## §6 — Process surgery recommendations

### R1: Merge or delete the G-Omega astral layer

The astral corpus (10 versions, ~24,000 lines) audits the Omega audit of the cycle
audits. No evidence shows it has invalidated a G-Omega conclusion. Abolish `ΩA-ΩF` as
standing documents; keep only the G-Omega SIGNOFF and a single CRUD-LOG. The locks and
master-plan are the living records; ΩC and ΩD are redundant with them. Estimated
savings: ~20,000 lines per campaign.

### R2: Collapse S-P1 (profile) and S-P2 (research) into one pass

S-P1 produces a hot-leaf attribution and a performance hypothesis. S-P2 consumes that
hypothesis to build a candidate shortlist. They share agents, share evidence, and in
practice S-P2's output is largely S-P1's findings restated at higher specificity. The
convergence protocol runs both through V1-V5+hardening, doubling the overhead. Merge
into one pass: profile → candidate-shortlist as a single 6-agent fan-out.

### R3: Replace §3Z with a binary gate

The §3Z protocol (95% ACCEPT for two consecutive cycles, V≤5) is an acceptance-testing
ceremony without a falsification trigger. No V1→V5 iteration has produced a REJECT
disposition that forced a code correction; they produce REVISE dispositions that sharpen
wording. Replace the convergence protocol with a binary gate: one challenge wave, and
a pass advances if there are zero REJECT dispositions. A REVISE either blocks (if it
identifies a factual error in the claimed evidence) or is noted for folding (if it is a
precision tightening). This collapses 30-doc hardening forests to 6-doc challenge waves.

### R4: Lock the research-plan cycle to a maximum of 3 passes per tranche

The current cycle has at minimum 9 passes (alpha, S-P0, S-P1, S-P2, S-P3, T-P1, T-P2,
T-P3, Omega) and SK-V20 adds a 32-agent meta-research pass on top. The diminishing
return from passes beyond S-P3 + T-P1 is visible in the data: T-P2 and T-P3 are
literature surveys and synthesis documents that restate S-P3's plan in the vocabulary
of the totality crates. Cap non-implementation tranches at 3 passes (overfit-audit +
profile/research + plan). T-P1/T-P2/T-P3 are a totality-specific detour that delays the
implementation cycle without surfacing new facts.

### R5: Gate opening the next planning cycle on at least one CHRONIC item shipping

The deferred-census (`05-deferred-census.md`) lists at minimum two items deferred across
4+ cycles (SoA tape, C fold). A process that opens a new research tranche without
requiring any chronic deferral to have landed is a process that defers indefinitely. The
tranche gate for "open next planning cycle" should include: zero chronic items in the
deferred ledger. If a chronic item cannot land (blocked by a harder dependency), that
dependency must land first and must be named as the gate, not the chronic item.

### R6: Move the CSS SOTA delta migration out of "deferred" into a mandatory close condition

The ROLLING-SOTA-DELTA CSS rows have been OPEN since SK-V15. The gateway condition is
measurable (typed row beats cssparser with typed-summary equality). SK-V17 produced the
measurements; the schema migration was not done. This is not a hard research problem;
it is a bookkeeping task. Add it as a mandatory close condition on the next
implementation tranche, not a footnote in the gate-notes.

---

## §7 — Verdict

The SK meta-process is producing documentation faster than it is producing code, and
the gap has been widening. SK-V17 added 3.4 doc lines per code line; SK-V18 added 7.3.
Three of the four largest architectural items planned in the restart campaign (SoA tape,
C-fold value generator, BackendShape + decision engine) have not shipped to the
production `crates/` tree in any cycle. The §3Z convergence protocol is the primary
mechanism behind documentation inflation: it turns each pass into a 5-iteration
30-document hardening forest that certifies prose rather than code. The astral G-Omega
layer adds a recursion level (audit of the audit of the audit) whose value is
undemonstrated. The process is not failing — the >SOTA result is honest, the skinny
prototype is functionally sound — but it is stalling at the critical juncture where the
skinny architecture must transfer to the production fleet.
