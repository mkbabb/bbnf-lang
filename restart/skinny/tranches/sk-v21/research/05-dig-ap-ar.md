# Archaeological Dig — Tranches AP, AQ, AR

The structural-activation / dispatch-deletion era of the original campaign.
Three consecutive tranches spanning roughly 2026-04-12 to 2026-04-13, during
which the project tried to *activate* infrastructure built across AA–AO,
then *delete* the parts that would not activate, then *re-found* identity so
the rest could finally fire. This is the era where the divergence that forced
the restart becomes legible in the artifacts: performance numbers measured
against a parser that silently dropped input, a self-hosting "closure"
declared PASS against a frozen artifact that could not be regenerated, and a
recurring pattern of fully-plumbed subsystems held dark by single lowering
bugs.

Corpus: `docs/tranches/AP/` (AP.md + research/), `docs/tranches/AQ/`
(AQ.md + audit.md), `docs/tranches/AR/` (AR.md + seven audit/critique files).

---

## 1. What each tranche attempted

### AP — Structural Activation, Payload Truthing, Hot-Path Demolition

AP opened on a brutal-honest post-AO audit (AP.md:3-236). Its self-diagnosis
is the most valuable thing in the tranche: it names, in plain terms, that the
system had been mislabeled. "Stop calling the current state
direct-to-struct. The system is: Tape + f64 side-channel payloads + lazy
cursor views" (AP.md:211-215). It documents that structural dispatch, built
across AO.0.1–0.6, was *never active* — "zero `advance_to_structural`, zero
`scan_structural`" in emitted JSON (AP.md:106-114) — and that payload
projection was "EFFECTIVELY DEAD for JSON" because AO Phase 2.2 renamed the
kernel function but left the payload wiring pointing at the old name
(AP.md:116-121, AP/research/plan.md:74-81).

Plan: six phases, four waves. Phase 0 restore the build (6 stale enum
references, `@ws` kernel universality, payload rename, delete CSS monolithic
bench); Phase 1 *activate* structural dispatch as "the +50% lever"
(AP.md:335-397); Phases 2–6 payload enrichment, WS hot-path surgery, CSS L4
demolition, scanner surgery, calibration. The headline bet was that WS was
50% of citm and structural dispatch would elide it.

### AQ — Generalization, TypeDesc-Driven Projection, Self-Hosting Closure

AQ is the pivot tranche. A 6-agent post-AP audit (AQ/audit.md) reversed AP's
central bet. Finding #3 (AQ.md:51-59): structural pre-scan is *no longer
viable* — "DELETE, don't activate." The honest math: after AP.3.1's SIMD WS
bitmap dropped WS to 11.9% (110µs on citm), a simdjson-class fused pre-scan
would cost ~300µs, a net −190µs / +20% regression. AP.3.1 had killed the
savings opportunity by solving WS directly. Finding #4 (AQ.md:61-63):
`PayloadKind` is redundant with `TypeDesc`; delete it, consume `TypeDesc`
directly.

Plan: nine phases, four waves, up to 24 parallel worktree sessions
(AQ.md:676-689). Phase 1 self-hosting closure as HIGHEST PRIORITY — "AQ MUST
end the deferral chain" (AQ.md:43-45); Phase 5 DELETE the entire structural
dispatch infrastructure (~400 LOC, AQ.md:230-278); Phase 6 TypeDesc-driven
payload projection with full integer suite, aggregate layout planner, and
Alt→typed-enum views (AQ.md:279-489); Phases 3–4 deoverfit `RegexClass` (5
nominal variants → structural + params) and scanners (`scan_json_*` →
`scan_number_strict_*`); Phases 7–8 CSS `__compoundSelector` kernel routing
and portable sonic-rs techniques (skip_space cache, NEON simd_str2int).

### AR — Discriminator Split, Payload Activation, Clone Reduction, CSS Hardening

AR opened on the discovery that AQ "landed every claim at the code level …
but zero rules in any of the six production grammars exercise the central
AQ.6 path. The infrastructure is dormant" (AR.md:41-47). Six concurrent
audits + a cross-check critique (AR.md:21-33) traced the dormancy to six
activation gaps and, crucially, a *resequencing*: fix tape identity FIRST
(the `variant_idx` overload) before any AQ.6 activation, per critique §5
(critique.md:46-55).

Plan: nine phases (expanded from an initial five, AR.md:128; commit
8fb3adbb3), four waves, six worktree agents each. Phase 1 discriminator split
(`meta_idx` side-channel); Phase 2 activate payload projection (fix
`lower_map_arrow`, peel inlined Alt shapes, mixed payload/cursor enum views);
Phase 3 clone elimination; Phase 4 CSS classifier escape-hardening; Phase 5
tape capacity + input padding; Phase 6 scanner generalization; Phase 7
host.rs deletion; Phase 8 SIMD fractional scan; Phase 9 pair-compound
(KvPair) flattening.

---

## 2. What shipped vs what died on the vine

Cross-checked against `git log --oneline --all --grep` and the live tree.

### AR actually landed (git-confirmed)

AR was the most complete of the three. Confirmed commits:
`b2ff0d9c8`/`d9a760a01` meta_idx split (AR.1.1); `8204fa158`
inline-plan `preserve_identity` gate (AR.1.2); `990d45633` bootstrap regen
with meta_idx; `b857765ca` recover type suffixes from span text in
`lower_map_arrow` (AR.2.1 — the load-bearing fix); `99659f037`/`dcb75846a`
Alt peel + mixed payload/cursor enum (AR.2.2/2.3); `6074a4b70`/`677a8017f`
egraph clone 11→5 (AR.3.1); `472e8ad4b` O(n)→O(1) string-pool reverse index
(AR.3.2); `66ceffa4c` `allows_escapes` (AR.4.1); `5e234e519`/`7dfe20222`
`TapeKind::KvPair` + view accessors (AR.9); `133d3d5cc` FAMILY_HELPER policy
move (AR.6.3); `b0e453535`/`6c889d5e8` span-text-fallback deletion (AR.7.1).
A `post-AR.json` exists (`bab1a8e8e`/`b6b12e4e9`) — AR did produce measured
results, unlike AP/AQ whose "after" numbers were targets.

Verified activation (post-AR.json `tests.payload_layouts`): **14 payload
layouts lit up** (JSON 3: null→U8, bool→Bool, number→F64; CSS L4 11 dimension
/ keyword rules), clearing the `>= 8` gate. This is real: the dormant AQ.6
substrate finally fired, and the single unlock was the ~15-line span-text fix
in `lower_map_arrow` diagnosed at audit-direct-struct.md:146-192.

### AR died / was declared PASS but was not real

- **Self-hosting closure is a mirage.** AR.md:16 marks Phase 7 host.rs
  deletion "Complete" and AR.md:10 marks Phase 1 discriminator split
  "Complete" with "6/6 grammar_roundtrip green." But post-AR.json's own
  `hard_gates.bootstrap_regen_roundtrip` admits: *"PASS — 6/6 …with current
  generated.rs (24,685 lines). Regen from new host.rs produces a different
  parser (21,672 lines) that can't parse BBNF — bootstrap coherence issue
  deferred."* The roundtrip passes only against a **frozen** artifact;
  regenerating from live source yields a parser that cannot parse its own
  grammar. The 10-tranche chronic deferral (AC→AR) was *not* closed; it was
  declared PASS against a stale file.
- **host.rs was not deleted.** Commit `17b4c7b97` ("mark AR.7 host.rs
  deletion complete") is a doc commit; the file is still 584 lines on the
  live tree. Only AR.7.1 (span-text fallback, 607→423 LOC per the gate)
  landed; AR.7.2 (eliminate `ParsedGrammar`, direct CST→IR) did not.
- **CSS L4 bootstrap parse still fails** at AR close: post-AR.json
  `benches.css_l4.note` = "bootstrap.css parse still fails … likely
  additional modifier or expression shape issues in the lowering path."
- **Tape capacity heuristic regressed.** AR.5.1 (`2935c7bd1`) shipped
  `len/2+2` as audit-sonic-gap.md's #1 cheapest win; a later tranche caught a
  regression (`a79323c6e` "docs(AT): capacity heuristic regressed") and had
  to restore it (`57d0fa90c`). The "pure waste, obvious win" was not free.
- **Scanner generalization 6/8**: AR.6 shipped only 2 of 8 sub-items
  (AR.md:17); the rest deferred as "architectural consolidation, lower
  priority" (post-AR.json `deferred.scanner_generalization_6_of_8`).
- **64-byte input padding** (AR.5.2) deferred — "requires ownership model
  change in parse-that ParserState."

### AQ shipped-in-code, dormant-in-fact

AQ's audits (AR/audit-aq-code.md:14-45) verify each AQ claim by reading
source and expanding codegen. Verdict: PayloadKind deleted, full integer
suite added, layout planner wired, Alt-typed-enum codegen present, RegexClass
deoverfit, structural dispatch fully deleted — *all true at code level* — but
`compute_payload_layouts` "returns `HashMap::new()` for every production
grammar" (audit-direct-struct.md:5-31). 304 rules, 155 Tuple-typed, **zero**
with all-scalar fields. The entire Phase 6 was inert.

### AP: the headline lever never flipped

Structural dispatch activation (AP.1, "the +50% lever") failed twice and was
rescinded (audit-prior.md:23-49: commit `2a8af08` "structural dispatch
disabled + WS trim restored (AP.1 RESCINDED)"), resurfaced as AP.1b peek-only,
then was deleted wholesale in AQ.5. What *did* land and drive the real
throughput: AP.3.1 SIMD WS bitmap, AP.3.3 SIMD string scanner, AP.4 CSS L4
key dispatch (audit-prior.md:46-49). Six of nine AP plan items deferred to AQ.

---

## 3. Root causes of divergence (with evidence)

### 3.1 Bench dishonesty — the deepest rot

The entire AA→AQ performance narrative rested on a broken parser. post-AR.json
`benches.json_monolithic.note`: *"Down vs post-AQ because post-AQ parser
silently skipped modifiers — those numbers were from a broken parser."* The
concrete damage: canada went 1810 → **1097** MB/s once modifier recovery was
fixed (post-AR.json), erasing the celebrated "+17% BEAT sonic-rs" that
appears as a headline in AP.md:24, AQ.md:22, AQ/audit.md:74, and
audit-sonic-gap.md:11. The lead was fiction — the parser was dropping input,
so it "parsed" faster. This is the single most important lesson of the era:
**a throughput number is meaningless without a correctness gate that proves
the parser consumed the whole input and produced the right tree.** Every
"BEAT sonic-rs" claim across three tranches was measured against a parser
that skipped work.

### 3.2 Doc/code inversion

Plans and status headers asserted completion the code did not support.
AR.md:10-19 marks all nine phases "Complete"/"Partial" in a tidy table;
post-AR.json's own gate block walks back self-hosting, CSS L4, and capacity.
critique.md:78-82 had to insert a standing rule — "There is no post-AR.json
… any 'after AR' throughput numbers in AR.md are targets, not facts" — because
the plan tables presented targets as achievements. Each tranche's audit
existed largely to correct the *previous* tranche's overstatements
(critique.md is literally titled "Corrections and Recalibrations"), a healthy
antibody that nonetheless means every tranche shipped claims the next had to
retract.

### 3.3 The regen build-wall (10-tranche chronic)

Clean bootstrap regeneration was deferred across AC → AE → AF → AG → AI → AM
→ AN → AP → AQ → AR (audit-prior.md:302-308). The measurable scar: **190 of
237 `#[allow(...)]` attributes (80%) live in the single hand-maintained
`generated.rs`** (audit-prior.md:263-277). Root cause finally diagnosed at
audit-self-hosting.md:51-58: `variant_idx` is *overloaded* — non-Alt-bodied
rules stamp `rule.id`, Alt-bodied rules stamp `__branch_idx` (Tranche AK.1),
so the same byte means two incompatible things and every `rule_kind()`-driven
consumer misreads a regenerated tape. Regen produced a divergent 78,307-line
file failing six roundtrip tests (audit-self-hosting.md:9-26). AR's meta_idx
split (Path 6, audit-self-hosting.md:366-388) was the correct fix and *did*
land, but the deeper bootstrap-coherence fixpoint (regen from new host.rs →
parser that parses BBNF) still diverges.

### 3.4 Design-then-abandon (LOC churn)

Two large subsystems were built, integrated, and then deleted:
- **Structural dispatch**: infra built AM.5 (+300 LOC) → threaded AN → AO
  codegen → AP.1/1b activation attempts → **deleted AQ.5** (~400 LOC removed,
  AQ.md:262). Never once ran end-to-end in production. AO is characterized as
  "RUBBER-STAMPED … Phase 0 code complete, never exercised" (audit-prior.md:80).
- **Tier B direct-to-struct (three-function ABI)**: designed AF, implemented
  AI.1-4, **deleted AM.1** (−2,306 LOC) because the split ABI made Direct-tier
  functions unable to call Tape-tier children, so `reconcile_cross_component_tiers`
  widened everything back to Tape (AP.md:170-172, AQ/audit.md:9,23).

The negative result is durable and must not be re-learned: **a separate ABI
for value projection is architecturally wrong; the unified tape ABI with
typed payloads is the correct path** (AP.md:750, AQ.md:48-49).

### 3.5 Dormancy — plumbed but dark

The signature failure mode of the era. Infrastructure fully wired end-to-end,
held inert by a single upstream bug. The canonical case: payload projection.
audit-direct-struct.md:99 — *"This is one bug masquerading as the absence of
an entire feature. The plan, infrastructure, and codegen are all wired and
correct; the lowering pipeline never produces the input that the planner
needs."* The bug: `lower_map_arrow` (expression.rs:1229-1292) detected type
suffixes by `rule_kind()` checks for `int_lit`/`float_lit`/`bool_lit`, but the
tape rewrite folded those leaves into `value_atom`, so every `-> f64` / `-> 0u8`
/ `-> true` silently lost its type and projected to `Span`
(audit-direct-struct.md:34-98, AR.md:49-58). One ~15-line span-text fix
(AR.2.1, commit `b857765ca`) lit up all 14 layouts. Three tranches shipped a
"payload system" that touched zero production rules.

### 3.6 Overfitting to JSON/CSS

Persistent language-specific naming in shared infrastructure: 5 of 13
`RegexClass` variants nominal (`JsonString`, `JsonNumber`, `WsBlockComment`,
`CssIdent`, `CssQuotedString`); a `classify_known_pattern` exact-string
dictionary keyed on JSON/CSS canonical patterns; 7 `sp_json_*`/`sp_css_*`
SpanParser constructors; scanners `scan_json_number_*`; hardcoded
`STRUCTURAL_PUNCTS: &[u8] = b",:{}[]"` (AQ/audit.md:53-55, AQ.md:132-228).
Deoverfitting was planned AN.2 → AO.2 → AQ.4 and only landed at AQ.4. The
tell: the distinguishing parameter between "JSON number" and "generic number"
was just *reject-leading-zero* (AQ/audit.md:11) — the nominal names encoded a
single boolean.

### 3.7 Chronic deferral chains

Four items recurred as "planned, never run" across 4+ tranches
(audit-prior.md:300-322): clean regen (10×); global CSP solve (AL, AO.4.2,
AP.6.5, AQ.9.5 — always "per-component is sufficient"); cost-model grid sweep
(AM.6, AO.4.1, AP.6.4, AQ.9.4 — never executed); release-build instrumentation
(AN.6→AO.4.3→AP.6.4→AQ.9.3, finally shipped `a78c4a6`). A plan item that
appears in four consecutive tranches without landing is not a plan item; it
is a signal the tranche scope exceeds what the wave model delivers.

---

## 4. Worth resurrecting for the totality plan

### Algorithms / designs
1. **`lower_map_arrow` span-text type detection** (audit-direct-struct.md:146-192,
   commit `b857765ca`). The unlock for all scalar/aggregate payload projection.
   The generalizable lesson: **type information is lost at lowering, not at
   codegen**; the projection path must inspect `value_atom` span text, never
   trust folded `rule_kind` compounds.
2. **`meta_idx`/`variant_idx` split (Path 6)** (audit-self-hosting.md:272-297,
   366-388). The correct resolution of the discriminator overload that blocked
   regen for 10 tranches. Landed and improved matters; keep it.
3. **Path 4 — eliminate `ParsedGrammar`, lower bootstrap CST directly to IR**
   (audit-self-hosting.md:197-253, 408-434). The elegant endgame: the
   bootstrap proc-macro emits `IrNode`s directly, deleting host.rs (~600 LOC),
   `ParsedGrammar`, `tape_walk.rs`, and the entire CST-walking layer. Never
   completed; the single largest standing structural payoff.
4. **KvPair pair-compound flattening** (AR.9, audit-sonic-gap.md:85, commits
   `5e234e519`/`7dfe20222`). Collapse `pair = string, colon >> value` to a
   2-field aggregate leaf when value is scalar — halves tape records on
   twitter/data. Landed; the right shape.
5. **sonic-rs technique inventory with disposition** (audit-sonic-gap.md:49-76).
   Ported: skip_space bitmap cache (full parity, state.rs:179-193), Eisel-Lemire,
   SWAR 8-digit. Genuine remaining gaps: tape capacity heuristic (with the
   caveat below) and 64-byte input padding. Do NOT port: in-place unescape
   (breaks span-borrow), LazyValue/RawStr (tape is the moral equivalent),
   16-byte tagged Value (JSON overfitting). This triage is reusable verbatim.

### Negative results (do not re-learn)
6. **Structural pre-scan is net-negative once WS is solved directly**
   (AQ.md:51-59). Pre-scan only pays when WS self-time ≫ ~300µs pre-scan cost.
   After a SIMD WS bitmap, it is dead weight. Deleting it was correct.
7. **Separate-ABI direct-to-struct fails via tier reconciliation** (§3.4).
   Unified tape ABI + typed payloads only.
8. **`len/2+2` tape capacity is not a free win** — it regressed and needed
   restoration (`a79323c6e`, `57d0fa90c`). Any capacity heuristic needs a
   measured gate, not an a-priori "obvious waste" argument.

### Measurements to carry as baselines (with the honesty caveat)
9. The post-AR.json numbers are the *first honest* JSON baseline of the era
   (canada 1097, citm 2353, twitter 2069, data 1832, data_xl 1084 MB/s) —
   every earlier "BEAT sonic-rs" figure is contaminated by the
   modifier-skipping parser and must be discarded, not carried forward.

---

## 5. Meta-process pathologies

1. **Worktree cherry-pick duplication.** The 6-agent isolated-worktree model
   (AQ.md:676-731, AR.md:500-548) produced double-applied commits:
   `256347a06`/`b2ff0d9c8` (same AR.1.1 message) and `aaf4974da`/`d9a760a01`
   (same AR.1.1). Parallel worktrees + manual cherry-pick onto master leaks
   duplicate history when the same logical change lands from two branches.
2. **Rubber-stamp tranches.** AO shipped Phase-0 codegen "complete" but never
   exercised end-to-end, then rolled five of six phases forward
   (audit-prior.md:53-84). A tranche whose headline lever never runs in
   production is a deferral wearing a completion label.
3. **Targets-as-facts.** Plan perf tables (AP.md:619-653, AQ.md:560-596,
   AR.md:112-127) presented aspirational throughput inline with measured
   baselines; only the succeeding tranche's audit separated them. The gate
   discipline critique.md:98-116 imposed — `cargo expand` evidence for every
   codegen claim, `samply` diff for every perf claim, roundtrip for every
   correctness claim — is the correct standing rule and should be a
   precondition, not a post-hoc correction.
4. **Completion asserted in docs ahead of code** (§3.2): `17b4c7b97` marks
   host.rs deletion complete against a 584-line live file; AR.md:16 marks
   Phase 7 Complete; the roundtrip "PASS" is against a frozen artifact. Status
   tables must derive from live probes, never from plan intent.
5. **The V-shaped debt curve is real and instructive** (audit-prior.md:282-298):
   AK (tight, fully-landed) → AN/AO (debt peak, deferrals accumulate) → AP/AQ
   (deliberate harvest + deletion of stranded infra) → AR (activation). The
   healthiest tranches (AK, AM) were small and shipped complete; the
   unhealthiest (AO) were sprawling and shipped inert. Scope discipline
   correlates directly with landing.

---

## 6. Demolition status of the era's central act

The era's defining demolition — **structural dispatch DELETED, PayloadKind →
TypeDesc** — was *completed*, and correctly. Structural dispatch (built AM–AP)
is gone with no live code path (AR.md:105-107, verified in critique.md:28-35).
`PayloadKind` was deleted and the payload system now consumes `TypeDesc`
directly (AQ.6, audit-aq-code.md:14-45). Both demolitions were the right call.

But demolition without activation left the successor system dormant for a full
tranche, and the successor's own crown — self-hosting closure — was declared
won while its regeneration fixpoint still diverged. The era proved the
architecture (unified tape + TypeDesc-driven payloads) sound and the previous
architecture (separate ABI, structural pre-scan) unsound; it did not prove the
system *whole*. That un-wholeness — a correct design held dark by lowering
bugs, a self-hosting loop that passes only against frozen output, a
performance story built on a parser that dropped input — is the exact texture
of the divergence that forced the restart.
