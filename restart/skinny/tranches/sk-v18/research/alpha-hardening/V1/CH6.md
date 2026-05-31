# CH6 — ANTI-PAPER-CLOSE (V1) — SK-V18 Pass-Alpha Adversarial Review

Lens: CH6 Next-Tranche-Impact / ANTI-PAPER-CLOSE. Per PASS-ALPHA §3 (CH6: "does the
SK-V{N+1} contract specify revert protocol per intervention? Hard caps? Triumvirate
discipline? Is the goalset measurable + verifiable from the bench gate?") + ORCHESTRATOR
§3W. Reviewed: `sk-v18/research/alpha/{alphaA..alphaE}.md` + `SYNTHESIS.md` +
`HANDOFF.md`. (No alphaF artefact exists by that name — per PASS-ALPHA §2, α-F's
deliverable IS `SYNTHESIS.md` + `HANDOFF.md`; both present, both reviewed.)

**Lens mandate:** no wave deferred without a receiver + a gate; generalization is
concrete (json_sink_direct actually projects, CSS actually lowers, the generator
actually un-forks); the goalset is telemetry-bound and bench-verifiable; the
honest-finding escape is not a paper-close hatch; revert/hard-cap/triumvirate are
specified or contract-sanctioned-deferred with a measurable handoff.

**Ground-truth verification (this lens re-grepped at HEAD `318d9c046`, the bracket
HEAD all artefacts cite):**

- `runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"` — STILL a verbatim
  `&str` literal. CONFIRMED. Note the load-bearing nuance at `:683-700`: a long comment
  CLAIMS this const is already "the grammar-recognizer CSS provider … reconstructed
  LAZILY from the sealed `Tape` via `ValueRef`." It is not — it is a hand-written `&str`
  blob with a true-but-misleading provenance comment. This is *exactly* the verbatim-blob
  the goalset's G2 targets, and the artefacts correctly refuse to credit the comment as
  derivation (alphaA §3.1, alphaC §2.3, alphaD I1). The premise holds and is sharper than
  the artefacts state.
- `grammar_provider.rs:40` `pub enum RuntimeEmitterKind`, branched `:110`. CONFIRMED (G3).
- `bbnf-simd/src/x86_64/` = 24 files. CONFIRMED (P1).
- `tape/mod.rs:175` `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>`. CONFIRMED
  phantom (G4).
- `json/generated.rs` `parse_w11_1_number` ×7. CONFIRMED (P5).
- `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS`, iter `:2467`, assert `:2508`. CONFIRMED (P4).
- `sheets_witness/` = 25 LOC (24+1). CONFIRMED (PROVE).
- `css_canon_bench.rs` present; `json/scan.rs:25 neon::scan` present; `dispatch.rs:12
  SelectedBackend{Scalar,NeonTbl4}` present (G5 receiver substrate exists). CONFIRMED.
- `xtask/src/main.rs:59 gate-json` command exists TODAY; the `--skv18-generalization-report`
  extension is a PROPOSED future receiver (S-P3-bound). CONFIRMED — material to D-CH6-2 below.

The ground truth is solid. My dispositions concern paper-close risk in the *forward*
contract: deferred waves, fallback receivers, the honest-finding escape, and the
revert/cap/triumvirate question CH6 owns.

---

## Disposition summary (per reviewable section)

| # | Section | Path | Disposition |
|---|---|---|---|
| 1 | SYNTHESIS §0.1 close-condition gate table | `SYNTHESIS.md:149-167` | ACCEPT |
| 2 | SYNTHESIS §0.3 receiver goalset | `SYNTHESIS.md:199-209` | REVISE |
| 3 | SYNTHESIS §0.5 generalization litmus + fallbacks | `SYNTHESIS.md:283-297` | ACCEPT |
| 4 | SYNTHESIS Section 2 telemetry binding + gate consumer | `SYNTHESIS.md:350-402` | ACCEPT |
| 5 | SYNTHESIS Section 3 trajectory + the S-P3 deferral | `SYNTHESIS.md:440-446` | REVISE |
| 6 | HANDOFF "Next Move" + revert/cap deferral | `HANDOFF.md:219-268` | REVISE |
| 7 | alphaE candidate shortlist gates + sequencing | `alphaE:60-167` | ACCEPT |
| 8 | alphaC PRUNE-wave close gates | `alphaC:43-166` | ACCEPT |
| 9 | The honest-finding escape (anti-paper-close hatch) | `SYNTHESIS.md:285-287,436-438`; `HANDOFF.md:263-265` | REVISE |
| 10 | PROVE-Sheets gate (the generalization litmus) | `SYNTHESIS.md:162,287` | ACCEPT |
| 11 | G6 acceleration-wiring / orphan-kernel gate | `SYNTHESIS.md:161,207` | ACCEPT |

Tally: ACCEPT 7, REVISE 4, REJECT 0.

---

## §1 — SYNTHESIS §0.1 close-condition gate table — ACCEPT

`SYNTHESIS.md:149-167`. Every gate row carries a concrete, greppable verifier
(`find … = 0`, `grep -c … = 0`, `md5 distinct`, `accepts_current_allowlist passes ONLY
because the leaks are gone`). This is the load-bearing anti-paper-close property: the
close condition is structural and machine-checkable, not narrative. P4 in particular
(`:154`) is written correctly against the paper-close it indicts — "a GREEN gate is
meaningful — it scans the surfaces where Lock-14 phrase-#1 leaks could live … passes
ONLY because the leaks are actually gone (not excluded)." G1's parity-oracle clause
(`:156`) and G6's acceleration-at-admission clause (`:161`) both name the exact
falsifier. No deferred wave here lacks a gate. ACCEPT.

---

## §2 — SYNTHESIS §0.3 receiver goalset — REVISE

`SYNTHESIS.md:199-209`. **Finding D-CH6-1 (paper-close-adjacent: receiver named, exit
condition under-bound for two rows).** The receiver table assigns every wave an owner +
obligation — good. But two receivers carry an obligation phrased as a *disjunction with
an unbounded second branch*, and the lens requires the second branch carry its own gate
or it becomes a silent escape hatch:

- **G6 receiver (`:207`)**: "Wire … into the CSS hot path AT ADMISSION with a same-wave
  consumer, OR honestly retire/mark them." The "honestly retire/mark" branch has NO
  measurable floor here — what stops the receiver from marking *all* CSS NEON "retired"
  and closing G6 with zero acceleration wired? The §0.1 gate row `:161` is better (it
  requires the UDOT/PMULL/TBX/CSSC backlog be "wired with a same-wave hot-path consumer
  OR documented as a measured non-top-N leaf"), but §0.3 drops the "measured non-top-N"
  qualifier. **Fix:** amend `:207` to: "OR honestly retire/mark them **with a samply
  attribution row proving the kernel's target leaf is non-top-N on the benched CSS hot
  path** (the retire branch is gated on a measurement, not an assertion)." This binds the
  retire branch to the same telemetry the §0.1 row already names.

- **PROVE receiver (`:208`)**: "the Sheets `generated.rs` is md5-distinct from JSON+CSS."
  This is gated. But the receiver does NOT state the *fallback if Sheets cannot be emitted
  via the generator only* — §0.5 (`:287`) carries it ("surface honestly, do NOT
  stub-prove"), but §0.3, the *receiver* table that S-P3 reads to author owner paths, does
  not cross-reference it. A receiver reading only §0.3 could read PROVE as mandatory-close
  and paper over a generator that still needs a hand-written Sheets blob. **Fix:** add to
  `:208` the explicit pointer "(fallback per §0.5: if Sheets cannot be emitted via the
  generator ONLY, the generalization is NOT real — surface honestly, do NOT stub-prove; do
  NOT hand-write a `_GENERATED_RS` Sheets block)."

These are REVISE (not REJECT) because the gates EXIST elsewhere in the contract (§0.1,
§0.5); the defect is that the *receiver* table — the artefact S-P3 consumes to assign
owner paths — does not carry the gate inline, opening a read-path to paper-close. Make
the receiver self-contained.

---

## §3 — SYNTHESIS §0.5 generalization litmus + per-axis fallbacks — ACCEPT

`SYNTHESIS.md:283-297`. This is the strongest anti-paper-close section. Every axis row
carries Current / Target / Expected-intervention / **Fallback-if-not-met**, and every
fallback names a concrete non-paper-close action ("surface honestly as a named validated
grammar-parameterized primitive (HANDOFF §6), do NOT paper-close; do NOT silently retain
the hand-written blob" `:285`; "if Sheets cannot be emitted via the generator only: the
generalization is NOT real — surface honestly, do NOT stub-prove" `:287`; "REJECT the
trait shape, report, do NOT force a Lock-1 violation" `:288`). The litmus is explicitly
binary-structural (`:280`), gated on PRESERVING the >SOTA — the row cannot close by
narrative. The R10 tranche-success criterion (`:291-297`) restates the conjunction
correctly. ACCEPT.

---

## §4 — SYNTHESIS Section 2 telemetry binding + gate consumer — ACCEPT

`SYNTHESIS.md:350-402`. The telemetry binding is the load-bearing answer to CH6's "is the
goalset measurable + verifiable from the bench gate?" — and it answers YES. Each
generalization axis gets a machine-checkable column (`grammar_derived`,
`parity_oracle_diff`, `verbatim_blob_present`, `emitter_fork_present`,
`phantom_generic_resolved`, `shared_value_trait_instantiations >= 2`,
`generator_grammar_count == 3`, `generated_md5_distinct`, `sheets_real_grammar`,
`acceleration_at_admission`, `x86_tree_deleted`, `lock14_gate_scans_codegen`,
`metalang_leak_present`, `materialization_framing`, `corpus_in_timer`,
`regen_check_clean`). Critically, the gate consumer (`:385-402`) names the EXACT
reject conditions ("REJECTS any row with `verbatim_blob_present == true` …
`acceleration_at_admission == cfg-test-only` on an acceleration claim … any single-tuple
broadcast") — these directly bind the six V3 CHALLENGE addenda to a `cargo xtask gate-json
--skv18-generalization-report` consumer. The `gate-json` host command exists today
(`xtask/src/main.rs:59`); the extension is a real, namable obligation, not vapor. The
enum-typed columns (e.g. `phantom_generic_resolved ∈ {instantiated,deleted}`,
`parity_oracle_diff ∈ {byte-for-byte, named-validated-primitive, divergent}` with
`divergent`=NO-GO) close the narrative-escape that "instantiate-or-delete" could
otherwise leave open. ACCEPT.

---

## §5 — SYNTHESIS Section 3 trajectory + the S-P3 deferral — REVISE

`SYNTHESIS.md:440-446` + `134-138`. **Finding D-CH6-2 (the central CH6 question: revert
protocol / hard caps / triumvirate discipline are DEFERRED — is the deferral legitimate
or a paper-close?).**

The contract defers "the detailed wave-by-wave falsifiability gates (PASS-ALPHA §4.4:
owner paths, entry gate, exit gate, hard cap, revert protocol, same-wave consumer,
pre-blocked routes) … downstream … to S-P3" and states "Revert protocol, hard caps, and
per-wave triumvirate discipline are sanctioned-deferred to S-P3 per PASS-ALPHA §4.4."

**This deferral IS contract-sanctioned.** PASS-ALPHA §4.4 (`PASS-ALPHA.md:112-122`) and
the scope matrix (`:27`) explicitly place owner paths / entry+exit gates / hard caps /
revert protocol / same-wave consumer in `SPEC.md`, authored downstream by S-P3, and §4
(`:53`) states α-F sets layers §4.1–§4.3 only. So Pass Alpha is NOT required to author the
revert protocol itself; CH6 cannot REJECT the deferral as out-of-bounds.

**BUT** — and this is the anti-paper-close finding — CH6 §3 still asks whether the
*contract specifies* revert protocol / hard caps / triumvirate. The honest answer is that
the contract specifies the **measurable handoff that makes the deferred revert protocol
authorable**, but it does NOT yet state the **revert *dependency graph*** — i.e., which
waves' failure BLOCKS which downstream waves. This matters for paper-close because a
sanctioned-deferred revert protocol with no stated blocking-dependency lets S-P3 author
revert-in-isolation, where (e.g.) a G1 JSON-projection that loses >SOTA could be REDRESSed
locally while G2/G3/G4/PROVE — which all entry-gate on G1 (alphaE `:62,82,102,122`) — march
on over a broken predecessor. The sequencing dependency EXISTS in alphaE (`:163` "A → B1 →
B2 → B3 → B4 … Each B entry-gates on its predecessor") and in HANDOFF (`:229-245`), but the
SYNTHESIS Section 3 trajectory does not carry the *revert-blocks-downstream* clause into
the goalset S-P3 consumes.

**Fix:** add one sentence to `SYNTHESIS.md:446`: "S-P3's revert protocol MUST encode the
entry-gate dependency (PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1, per alphaE §cross-cutting
1): a wave that fails its exit gate BLOCKS every downstream wave that entry-gates on it —
no downstream wave dispatches over a REDRESSed predecessor; in particular G1 failure blocks
G2/G3/G4/PROVE, and G3 (un-fork) failure blocks PROVE (which emits Sheets THROUGH the
un-forked generator)." This converts the sanctioned deferral from "revert TBD" to "revert
TBD with a binding dependency graph" — which is the difference between a legitimate handoff
and a paper-close.

REVISE.

---

## §6 — HANDOFF "Next Move" + revert/cap deferral — REVISE

`HANDOFF.md:219-268`. The Next-Move sequencing is correct and the S-P3 wave order
(`:229-245`) is dependency-honest ("P4 … MUST land before the G2/G3 emitter rebuild";
"Each primitive lands WITH its hot-path consumer in the same commit (no orphan kernels)";
">SOTA re-proven on the grammar-DERIVED parser at each generalization that touches the hot
path"). The same-wave-consumer rule is stated (`:240`), directly pre-empting the V5
orphan-kernel paper-close.

**Finding D-CH6-3 (same defect as §5, in the handoff): hard caps are deferred with no
default ceiling carried.** `HANDOFF.md:267-268` says "Revert protocol, hard caps, and
per-wave triumvirate discipline are sanctioned-deferred to S-P3 (PASS-ALPHA §4.4
authority), not paper-closed here." The deferral is sanctioned (as in §5). But the user's
standing dispatch-hard-cap discipline ([dispatch-hard-cap]: "Every dispatch carries 'HARD
CAP: N min. At 0.9N commit, at N halt'; defaults 20/15/30 research/plan/redress") means a
contract that defers hard caps entirely, with no carried default, risks an S-P3 that
authors waves with no halt condition — the slow-paper-close where a wave runs unbounded
chasing a >SOTA-preserving projection. The contract should carry the *default ceiling*
forward so S-P3 cannot author a cap-less wave.

**Fix:** amend `HANDOFF.md:268` to: "Revert protocol, hard caps, and per-wave triumvirate
discipline are sanctioned-deferred to S-P3 (PASS-ALPHA §4.4 authority); S-P3 MUST carry the
standing dispatch-hard-cap defaults (research/plan/redress 20/15/30 min unless the wave's
risk class — B4 MED-HIGH per alphaE — justifies a documented larger cap) so no SK-V18 wave
dispatches uncapped." This is the minimum carry that prevents the deferral from becoming an
uncapped-execution paper-close.

REVISE.

---

## §7 — alphaE candidate shortlist gates + sequencing — ACCEPT

`alphaE:60-167`. Every candidate (A, B1-B4) carries: owner path:line, scalar-ref status,
checkasm status, **same-wave consumer**, a falsifiability triple (PRESERVED->SOTA /
grammar-derivation-proof / distinct-output), LOC budget, risk, pre-blocks. The
falsifiability triple (`:14-20`) is the right anti-paper-close instrument: gate #2
("mutate the `.bbnf` → the regenerated `generated.rs` changes correspondingly — a const
courier cannot pass this") is an *operational* derivation test, not an assertion — it
catches exactly the verbatim-blob-with-honest-comment defect I verified at
`runtime_generator.rs:683-701`. The sequencing is binding (`:163`) and entry-gated. The
cross-cutting honest-finding clause (`:164`) routes the escape through
abrogate-before-patch / pluggable-components, not paper-close. The risk-weighted close
prediction (`:167`) explicitly handles the litmus-failure case without paper-closing ("If
B4's Sheets litmus fails, SK-V18 does NOT paper-close — it surfaces 'generator is still
JSON+CSS-overfit,' iterates B1/B2, and B4 re-enters (V≤5 ceiling)"). ACCEPT.

---

## §8 — alphaC PRUNE-wave close gates — ACCEPT

`alphaC:43-166`. Each PRUNE wave (P1-P5) carries live evidence + a delete-or-fix
obligation + a *close gate that makes the prune meaningful*. P4's close gate (`:135-141`)
is the model: "`accepts_current_allowlist` PASSES *after* the rebuild because the scanned
surface is genuinely neutral (not because the dirty files are excluded)" — this is the
precise anti-paper-close phrasing (a green gate that is green for the right reason). The
PRUNE ordering note (`:160-166`) correctly sequences P4's gate-extension to follow G3's
emitter unification while landing the x86-tag removal + `runtime_generator.rs` scan-root at
PRUNE — a dependency-honest split, not a deferral. The §2 pre-block re-open tests are
keyed to the THREE new surfaces (generator, shared trait, instantiated `<G>`) and checked
TWICE — against the runtime output AND the emitter that produces it (`:405-411`) — closing
the generator-as-new-paper-close-surface. ACCEPT.

---

## §9 — The honest-finding escape (the anti-paper-close hatch itself) — REVISE

`SYNTHESIS.md:285-287,167,436-438`; `HANDOFF.md:263-265`; `alphaE:164`. The contract's
core anti-paper-close instrument is the honest-finding escape: "if a grammar-derived parser
CANNOT preserve the >SOTA without hand-shaping, surface it honestly as a named, validated,
grammar-parameterized primitive (HANDOFF §6) — do NOT paper-close." This is correct and
necessary. **Finding D-CH6-4 (the escape needs its own gate, or it becomes the
paper-close).**

The hatch is invoked in five places but never given a *bounding gate distinguishing a
legitimate named primitive from a disguised hand-written blob*. The risk: a receiver under
contact (the exact [execute-planned-architecture] failure mode in user memory) reaches for
"named validated grammar-parameterized primitive" and ships the SAME `CSS_GENERATED_RS`-style
const blob wearing the new label — which is precisely the verbatim-blob the cycle exists to
kill. The §0.5 fallback says "do NOT silently retain the hand-written blob" but does not
state what *makes* a primitive "validated grammar-parameterized" vs a relabeled blob.

**Fix:** add a gate to the honest-finding escape (suggest `SYNTHESIS.md:167` PASS-IMPL V4
row, or a new §0.5 footnote): "A 'named validated grammar-parameterized primitive' qualifies
ONLY if: (a) the grammar `.bbnf` INVOKES it by name (the primitive is a callable the grammar
references, e.g. a registered balanced-delimiter scanner — per alphaE §164 / pluggable-components),
NOT a free-standing const the emitter splices; (b) it is parameterized by grammar-derived
data (alphabet/delimiter set from the rule shape), NOT a fixed body; (c) it carries the same
`verbatim_blob_present == false` telemetry as any other derived surface. A primitive failing
(a)-(c) is a relabeled hand-written blob — REJECT, REDRESS, do NOT close." Without this gate
the escape is the single largest paper-close surface in the contract; with it, the escape is
a genuine honest-finding path.

REVISE.

---

## §10 — PROVE-Sheets gate (the generalization litmus) — ACCEPT

`SYNTHESIS.md:162,287`; `alphaE:120-143`; `alphaC:308-328` (broadcast pre-block keyed to
the Sheets corpus). The Sheets gate is the cycle's honest litmus and it is correctly
non-paper-closeable: `md5` of Sheets `generated.rs` ≠ JSON ≠ CSS (distinct-grammar-output),
the Sheets value type instantiates the G4 trait, ZERO hand-authored runtime Rust
(`grep -c 'const.*_RS.*r#' codegen/src` for any Sheets blob → 0, alphaE `:138`), via the
generator ONLY. alphaC §2.4 (`:308-328`) pre-empts the most likely Sheets paper-close — a
single aggregate timing loop broadcasting one Mbps across multiple Sheets corpus rows — by
binding the Sheets corpus to the same per-corpus N≥50-cold-median discipline (PERMANENT
pre-block, no different-framing admission). The "Sheets must be a genuinely different SHAPE
from JSON/CSS, not a third JSON, or the litmus is hollow" guard (alphaE `:142`) closes the
shape-paper-close. ACCEPT.

---

## §11 — G6 acceleration-wiring / orphan-kernel gate — ACCEPT

`SYNTHESIS.md:161,207`; `alphaE:120-143`; `alphaC:360-386`; telemetry
`acceleration_at_admission` (`SYNTHESIS.md:377`). The acceleration-wiring gate directly
corrects the SK-V17 W3 overstatement (NEON "acceleration" dead at admission) and is
machine-checkable: "any kernel claiming acceleration is reached at admission (grep the
generated hot path, not tests)." The `acceleration_at_admission ∈
{admission,scalar-passthrough-labeled,retired}` enum with `cfg-test-only`=NO-GO
(`SYNTHESIS.md:392`) closes the dead-at-admission paper-close. The same-wave-consumer rule
(alphaE `:132-135`: "A kernel with no admission-path consumer is RETIRED, not shipped")
pre-empts the V5 orphan-kernel pattern. (The one soft spot — the "retire" branch needing a
measured non-top-N floor — is captured under D-CH6-1 §2 above as a §0.3 receiver REVISE; the
§0.1 gate row `:161` already carries "documented as a measured non-top-N leaf," so the gate
EXISTS, it just must be propagated to the receiver.) ACCEPT.

---

## Consolidated CH6 verdict

The contract is **strongly anti-paper-close in its measurable core** (§0.1 gates, §0.5
fallbacks, Section 2 telemetry, alphaE triple, alphaC double-checked pre-blocks). Every
generalization axis has a concrete, greppable falsifier; the generalization is concrete
(json_sink_direct must project per a `.bbnf`-mutation test, CSS must lower per
verbatim-blob retirement, the generator must un-fork per `RuntimeEmitterKind` deletion);
the goalset is telemetry-bound and bench-verifiable via a real (extensible) `gate-json`
host command.

The four REVISE findings are all the SAME class: **gates that exist somewhere in the
contract but are not propagated to the artefact the next pass consumes**, leaving a
read-path to paper-close:

- **D-CH6-1** (§0.3 receiver): G6 retire-branch + PROVE-fallback gates exist in §0.1/§0.5
  but not inline in the receiver table.
- **D-CH6-2** (Section 3): the entry-gate dependency exists in alphaE/HANDOFF but the
  *revert-blocks-downstream* clause is not carried into the goalset S-P3 consumes.
- **D-CH6-3** (HANDOFF Next Move): hard-cap defaults are deferred with no carried ceiling.
- **D-CH6-4** (honest-finding escape): the escape has no gate distinguishing a real
  grammar-parameterized primitive from a relabeled hand-written blob — the single largest
  residual paper-close surface.

None rises to REJECT: the gates exist, the deferrals are PASS-ALPHA §4.4-sanctioned, and
no wave is deferred *without any* receiver+gate. All four are tightening REVISEs that
close read-paths to paper-close. They are orphan-free: each names a specific path:line and
a concrete one-sentence fix that S-P3 can bind directly.

TALLY accept=7 revise=4 reject=0
