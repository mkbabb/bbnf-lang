# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 ADVERSARIAL CRITIQUE (CRITIC 1 of 2)

Within-lock weighting: internal soundness of each family spec, and whether the pass-2 prototypes
tested the sharpest uncertainty. Read against PORTFOLIO + CONSTITUTION, FAMILY-SPECS.md, the five
research reports, PROTO-1/PROTO-2/PROTO-3, and the CROSS-LOCK ledger. Load-bearing code claims
re-verified live at HEAD (`general_descent/` exists; `runtime_generator.rs:98 !has_rule("value")`
dispatch confirmed; `has_parse_time_host_decode` at `passes/src/lib.rs:794/620/764` confirmed; no
`regen-bbnf`/`fixpoint`/`BBNF_TARGET` symbol found in skinny xtask — §0.4 stands).

**Headline verdict.** The pass-2 *prototype work* is strong — it drove the correct branch, relocated
the make-or-break twice, and pinned a relation that RAN with falsification. But the **synthesis
document is stale against its own lock's latest evidence**, cites a re-keyed CROSS-LOCK by dead IDs,
and its load-bearing 7D report was overwritten on disk. The lock's genuine pass-2 result is a
**dependency discovery** (bbnf grammar-totality is downstream of a LOCK-5 executable matcher), not the
per-family "ADVANCED" postures FAMILY-SPECS still asserts. No family is convergence-ready; the lock is
a rigorously-measured core with three exact, mostly *external*, gaps.

---

## CROSS-CUTTING FINDINGS (apply to every family)

### C-1 — THE DOC IS SUPERSEDED BY TWO OF ITS OWN PROTOTYPES (severity: HIGH; convergence-accounting)
FAMILY-SPECS.md froze at 15:12. **PROTO-3 (Brief A) ran at 15:25; the new PROTO-2.md (Brief B / 7C-T2)
at 15:26.** These are *precisely* the two briefs §7 "cuts for pass 3" and §6 marks "UNRESOLVED … until
Brief A answers it." Both have RUN, this pass, and both materially move the verdicts:

- **Brief A → PROTO-3: the leaf evaluator is MEASURED `BLOCKED — MISSING PRIMITIVE`**, not a candidate.
  `bbnf-regex::analyze` classifies all 7 named-rule leaves `Unknown`; `parse-that-regex` is
  delimiter-fixed JSON scanners; there is *no compiled-matcher target to route to* (grep for
  `Dfa|Nfa|Program|Instruction|transition` in skinny = empty). §6 states this is "UNRESOLVED"; it is now
  RESOLVED in the *hard* direction — a general regex-execution engine is required, equal to the original
  problem, LOCK-5/9C-coupled. Every "gated on Brief A" in §1/§2/§4 should read "BLOCKED (measured) on a
  LOCK-5 primitive that does not exist."
- **Brief B → new PROTO-2.md / 7C-T2: RAN — PASS.** §3 still says T2 is "STILL UN-RUN across two
  passes … highest-ROI missing probe." It ran and passed bit-for-bit on all four `check-*` targets.

**The doc's §6/§7 self-assessment of open gaps is therefore false as written.** The preamble already
concedes one supersession ("this rewrite supersedes the pre-prototype draft"); it needs a THIRD cut
folding PROTO-3 + new-PROTO-2. A lock cannot be scored on a synthesis whose stated frontier has already
been crossed by unincorporated in-lock evidence. This is the single most important within-lock defect.

### C-2 — THE 7D REPORT-OF-RECORD WAS CLOBBERED (severity: MED-HIGH; Constitution vii)
FAMILY-SPECS §0.5 and §4 cite **"PROTO-2.md regex Tier-S"** as the RAN artifact behind the load-bearing
`parity=0.7931 / perf 1.768× / ORACLE_REL_V1 / 12 divergences / 4 planted mutations` result. On disk,
**PROTO-2.md now contains the 7C-T2 report** (overwritten 15:26). The regex Tier-S RAN report no longer
exists as a standalone artifact — its evidence survives only as FAMILY-SPECS prose plus `PROTO-2.patch`
(confirmed: that patch is the pt-scratch `regex/mod.rs` oracle probe, not 7C-T2). 7D is the "POSTURE
winner" whose entire claim is *measured*; its citation now dangles and its artifact-of-record is gone.
The numbers are not lost, but the constitution's "concrete artifacts, not status prose" bar is strained
for the lock's most-cited measured row. Restore the regex report under a distinct filename.

### C-3 — DANGLING CROSS-LOCK CITATIONS (severity: MED; traceability)
The current ledger keys adjudications **XL-A … XL-H**. FAMILY-SPECS cites **XL-1, XL-2, XL-3, XL-5,
XL-7** throughout (§0.2, §2, §3, §4, §5, §6) — a *numeric* scheme that does not resolve against
CROSS-LOCK.md. Content-mapping is plausible (XL-2 regex ≈ XL-C; XL-1 IR-plane ≈ XL-B; XL-D TS face), so
this is stale re-keying, not fabrication — but the constitution has distillers convey CROSS-LOCK "as
facts, binding," and a reader cannot verify a settled fact cited by a dead ID. Every `XL-N` numeric cite
must be re-pointed to its `XL-<letter>` before the gestalt consumes this registry.

### C-4 — THE LOCK'S REAL RESULT IS A DAG EDGE, NOT FOUR ADVANCING ROUTES (severity: framing)
After PROTO-1 → PROTO-3, **three of four families' primary deliverables (7A recognizer seed, 7B Plane A,
7D-bbnf Tier-R/S) share ONE blocking cause: no executable regex matcher exists under `general_descent`**
(INC-5). This is correctly identified in the doc but under-weighted in the per-family "ADVANCED" labels.
The honest lock-level statement: *LOCK-7's bbnf leg cannot converge internally; it is born-RED downstream
of LOCK-5 standing up an engine-tier `match_at(pattern,input,start)->Option<end>` primitive.* Only
(a) 7C's LOCK-free trio and (b) 7D's regex ORACLE-ONLY row are LOCK-7-closeable — and (b) is itself a
measured BLOCK whose 3-verdict remainder is LOCK-5-substrate-blocked (XL-C fork). The gestalt must carry
this as the lock's terminal finding.

---

## §1 — 7A · STAGED-BOOTSTRAP — convergence 55%

**What is sound.** The fixpoint LAW (byte-identical `regen∘regen`, no 2-cycle) is a genuine
non-redundant contribution. Killing the `88s→0.5s` datum as a legacy LTO transplant is exactly the
constitution-vii/iv discipline (a refuted perf claim, named). The non-vacuity gate — a planted
input-mutation that MUST change the bytes, fixed-const path REJECTED — is a gate that can actually fail.
Born-RED gates (a)(b)(c) are genuinely RED and measured un-satisfiable today. Good.

**Where it fails internal soundness.**
- **The one non-redundant contribution (the fixpoint gate) has NEVER RUN on any artifact — SPEC_ONLY.**
  The promotion ledger's `fixpoint` column (§4) is unfilled even for the one COMPLETE regex row. The
  elegant-reduction trap fires precisely here: "the fixpoint gate certifies non-vacuously" is the hard,
  unconverged step. 7A borrows 7D's *relation* non-vacuity (the 4 RAN mutations) but has zero evidence
  that byte-identical re-emission across a self-application actually holds for anything.
- **Producibility is now MEASURED-BLOCKED, not "narrowed."** PROTO-3 makes the leaf evaluator a missing
  primitive; the "recognizer seed producible now" consolation is fully gone for bbnf. The label
  "ADVANCED (narrowed), retreated at producibility" is stale — post-PROTO-3 the honest label matches
  7D-regex: **BLOCKED (measured), external.**

**Exact open gaps.** (1) fixpoint gate never executed on any artifact (SPEC_ONLY — the elegant-reduction
step). (2) bbnf S0 producibility BLOCKED on the LOCK-5 leaf primitive (PROTO-3), unincorporated. (3)
`ADVANCED` label superseded by PROTO-3 → should be BLOCKED-external. (4) the "building-seed inflates past
the 2142-LOC hand parser, dissolving auditability" claim is asserted from an in-repo *analogy*
(parse-that 2293+524 LOC), never measured on a bbnf building-seed.

## §2 — 7B · TOTAL-SELF-HOST — convergence 68%

**What is sound.** As a *research answer* (which the doc correctly says it is, "not a route") this is the
most complete edge-measurement in the lock. DATA≈50% / WALK-FOLD≈20% / ESCAPE≈21% on `emit.rs` is
concrete; the E0499/E0503 kernel-escape signature (delete the `&mut _probe` re-alias → borrow-check
fails across Alt/Optional/Repeat) is a NAMED counterexample, not an assertion. Prior-art convergence
(TableGen/MLIR-ODS/tree-sitter never erase the emitter kernel; they relocate it into a VM) is honest.

**Where it fails internal soundness.**
- **The measured shares sum to ~91%.** The remaining ~9% of `emit.rs` is unclassified — a measured total
  that does not close. Name the residue or the DATA/ESCAPE ratio is soft at the margin the family leans on.
- **Plane A is HARD-BLOCKED, stated as "undercut."** §2 says "PROTO-1 further undercuts Plane A."
  Post-PROTO-3, Plane A's `ir.bbnf`→`build_grammar_ir(view)` route presupposes `general_descent`
  recognizes grammars it cannot (leaf floor), so Plane A is blocked on the same missing primitive, not
  "undercut." And Plane A is *consumer-less unless LOCK-3 picks 3C* — correctly flagged (Y.13), but that
  leaves the family's IR half contingent on XL-B (WORKING, not settled).

**Exact open gaps.** (1) ~9% unclassified `emit.rs` residue. (2) Plane A hard-blocked (leaf primitive +
LOCK-3 3C pending), stronger than "undercut." (3) the maximal pole's BLOCKED-candidate status rests on
ONE emitter blob (`emit.rs`, 452 LOC) — the "no production system erases the kernel" generalization is
prior-art-backed but the *skinny-local* measurement is n=1.

## §3 — 7C · HOST-FUNCTION-LATTICE — convergence 70%

**What is sound.** The tier-off-`&mut Ctx` inference (vs pass-1's defeatible `@pure` string) is the right
mechanism. The LOCK-free/LOCK-1-bound independence split is clean. And Brief B (new PROTO-2.md/7C-T2)
RAN and PASSED with real rigor: baseline control, freshness syntax-error confound-kill, negative control
(inverted predicate also byte-identical), mechanism probe. The string name-gate is proven dead.

**Where it fails internal soundness — the sharp one.**
- **T2's PASS proves a NO-OP, then the doc frames it as "ships value now."** The mechanism probe shows
  `has_parse_time_host_decode` returns constant-false on the ENTIRE committed fleet (`"ws".contains
  ("host")` is always false; the only annotation ever built is `?w`, `value:None`). The *inverted*
  predicate is ALSO byte-identical. So the change is observably nothing — a dead-code deletion. That is
  fine hygiene, but "ships value now / one-file cure" oversells a proven-neutral edit.
- **The landed T2 is a LATERAL move, not the spec's promise.** §3 says the fix is "the DETECTION moves to
  a **typed node**." The patch instead moves it to a *shape-sniff on an always-empty slot*
  (`value.is_some()`), because "the IR has no `ExprKind::Host` node yet." When a real host node lands,
  `value.is_some()` is not obviously the right discriminator. T2 discharged the *string*-gate refutation
  but did NOT deliver the typed node — the actual 7C mechanism is still un-built.
- **The FLAGSHIP deliverable is BLOCKED.** The totality% counter — "the metric every ladder reports
  against" — "NEEDS bbnf to parse+build" (§3's own DEEPER correction), i.e. rides the leaf evaluator,
  which PROTO-3 measures BLOCKED. So the counter has never been computed on any grammar; it is a
  consumer-facing deliverable that cannot presently run. 7C "ships" only the LOCK-free trio, of which the
  landed piece (T2) is a no-op and T4 (`@{}` lowering) is un-run.

**Exact open gaps.** (1) totality% counter never computed — blocked behind the leaf primitive (T4/T6).
(2) T4 `@{}` lowering (`parse_atom:1495` arm + `ExprKind::SpanCapture`) un-run; CSS floor unmet until it
lands. (3) typed `HostFnSig` node not built — T2 delivered a shape-sniff, not the promised typed node.
(4) LOCK-1 inherited-attribute verdict fixes the floor ∈{0,1,2} — pending. (5) doc stale: §3 claims T2
"un-run."

## §4 — 7D · META-CIRCULAR-ORACLE — convergence 78%

**What is sound — genuinely the strongest.** `ORACLE_REL_V1 = R1∘R2∘R3` is pinned AND RAN as code; all
four planted mutations trip (greedy→lazy, drop-group, bind-the-`bool`-twin=vacuous→rejected, alt
mis-assoc) — the oracle is *proven able to fail*, defeating the gate-that-cannot-fail mode. The regex row
is fully partitioned: parity 0.7931 Empty-normed (0.7759 strict, 0.7843 committed-51) all BELOW the 0.80
gate, perf 1.77× slower, 12/12 divergences classified with XL routing settled (9→LOCK-5 host obligation,
3→regex.bbnf oracle bank). ORACLE-ONLY as a *lawful terminal* is the right posture. The M3 refutation
(binding the `generated.rs:2288` `bool` twin is vacuous) is correctly load-bearing on the producer pin.

**Where it fails internal soundness.**
- **bbnf Tier-R/S BLOCKED (measured), stated as "gated on Brief A."** PROTO-3 answers Brief A: blocked on
  a nonexistent LOCK-5 primitive. So 7D's bbnf half is not "not deliverable today" (implying near) but
  born-RED-external. Unincorporated.
- **The 0.80 gate's provenance is unnamed.** Because ORACLE-ONLY is a lawful *pass* outcome, there is no
  incentive to reverse-engineer the threshold to force a fail — and all three parity numbers clear below
  it, so the verdict is robust to the choice. But a promotion gate whose value is uncited is a latent
  gate-tuning risk the ledger schema should close (name where 0.80 comes from).
- **The Empty-norm could in principle launder divergences INTO parity** (inflating it). Checked: strict
  45/58 → Empty-normed 46/58 moves exactly one case; the verdict is BELOW-gate on all three metrics, so
  normalization is not laundering the result. Non-issue, but worth the explicit note the doc omits.
- **Even the regex oracle bank is not LOCK-7-closeable:** the 3 verdict divergences need `regex.bbnf`
  grammar fixes that are LOCK-5-substrate-fork-blocked (XL-C). 7D-regex converges as a *negative result*;
  its cleanup is external.

**Exact open gaps.** (1) bbnf Tier-R/S blocked on the LOCK-5 leaf primitive (PROTO-3), unincorporated.
(2) regex Tier-S RAN report clobbered (C-2) — evidence-of-record is prose+patch only. (3) 0.80 gate
provenance uncited. (4) the 3 `regex.bbnf` oracle-bank verdicts are LOCK-5-blocked, so the row's
remainder cannot close inside LOCK-7. (5) `fixpoint` column of the one COMPLETE row is empty (7A never
ran) — the ledger's first "complete" row is one column short of complete.

---

## DID THE PROTOTYPES TEST THE SHARPEST UNCERTAINTY? — YES, decisively (the pass's strength)

This is where the pass excels and must be credited. PROTO-1 drove the CORRECT branch
(`!has_rule("value")`→`general_descent`, never `json_sink_direct` — the exact directive trap) and found
the make-or-break sits a LAYER BELOW where pass-1 placed it (the leaf lexer, not the builder augment).
PROTO-3 then resolved *that* newly-exposed gap to a binary verdict (missing primitive, no matcher
target — with the empty `Dfa|Nfa|Program` grep as the clinching negative). PROTO-2-regex pinned the
relation and ran it with 4 falsifying mutations. New-PROTO-2/7C-T2 killed the cheapest signal with
controls. The uncertainty targeting is a model of the "concrete artifacts or SPEC-ONLY" law.

**The pass's only failure is bookkeeping, not experiment:** the synthesis was frozen 13 minutes before
the last two probes landed, so it narrates as "open/future" work that is already RAN, cites a re-keyed
ledger by dead IDs, and lost its 7D report to an overwrite. Fix the doc, not the science.

---

## RECOMMENDATION TO THE AGGLOMERATOR

Do NOT score any family convergence-ready. Re-cut FAMILY-SPECS a third time to fold PROTO-3 (7A/7B-A/7D-
bbnf → BLOCKED-external, not "gated") and new-PROTO-2 (7C-T2 → RAN-PASS, no-op-neutral). Restore the
regex Tier-S report under a distinct name (C-2). Re-point every `XL-N` numeric cite to `XL-<letter>`
(C-3). State the lock's terminal result as C-4: **LOCK-7's bbnf leg is born-RED downstream of a LOCK-5
engine-tier `match_at` primitive; only 7C's LOCK-free trio and 7D's regex ORACLE-ONLY negative result
are internally closeable, and both have external remainders.** The lock is a rigorously-measured core
with exact, largely-external gaps — the correct posture for a converged-core hand-off, once the doc
catches up to its own prototypes.
