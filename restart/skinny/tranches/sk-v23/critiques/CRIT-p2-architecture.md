# CRIT-p2-architecture — SK-V23 SPEC-DRAFT, pass 2, the architecture + interlock lens

**Agent:** re-critique, adversarial / numerically conservative. **Target:**
`sk-v23/SPEC-DRAFT.md` (942 lines, read whole across two pages). **New evidence walked:**
`prototypes/PROTO-PB-3b.md` (executed byte-parity, 13/13 PASS), `research/09-ledger-rehome.md`
(70-row walk), `research/10-adjudication-packet.md` (owner packet). **Prior baselines:**
`critiques/CRIT-codex-architecture.md` (pass-1, 80), `critiques/CRIT-PB-3.md` (64),
`CONVERGENCE-LEDGER.md` (pass-1 architecture post-amendment 92). **Convergence: 94%.** All
three pass-2 lens questions resolve favorably — the sheets seam is SETTLED, the ledger audit
risk is CLOSED, the graph is ACYCLIC — with ZERO blocking design questions, orphans, or cycles.
The gap to 100 is determinate un-applied folds plus the five pass-1 MODERATE interlock gaps that
the pass-1 amendment left untouched (it folded only the three blocking design questions B1-B3).

---

## Q1 — The sheets seam: DECIDABLE and DECIDED-IN-DIRECTION; the codex text is not yet folded.

**PB-3b settles it.** The pass-1 ledger flagged this as the #1 freeze blocker — "feasibility
unproven, not just gated ... a gate on an unproven mechanism is a deferred design question
wearing a gate's clothes" (`CONVERGENCE-LEDGER.md:17,49-54`). PB-3b executes the exact prototype
the ledger's pass-2 focus prescribed (`CONVERGENCE-LEDGER.md:65-67`) and returns the FAILURE
branch cleanly with running byte-parity, not a source read: (a) the sheets tape records **zero
Enter/Leave events** — 39 no-op `let _ = (Nu8,&mark)` tags, one live `mark`, 12 `match_leaf`
sites — so the "structural events over the sheets tape" the codex gate names **do not exist**
(`PROTO-PB-3b.md:30-49`); (b) pure two-method `Classify` **byte-diverges** from the live
`rule_kind` navigation — `find(rule_kind==Identifier)→offset 1` vs `find_by_class(Scalar(Raw))
→offset 0`, `assert_ne!` proven (`PROTO-PB-3b.md:81-97`); (c) parity is recoverable **only** by a
`NativeTag` escape that re-imports the 39-ordinal across the seam — the flattening PB-3's "no
enum crosses" thesis disavows (`PROTO-PB-3b.md:99-127`). Caveat 1 is self-limiting: unexercised
leaves "only add more kinds into the same `Scalar(Raw)` bucket, deepening the collapse, never
relieving it" (`PROTO-PB-3b.md:171-175`) — the verdict cannot be overturned by more fixtures.

**So the open design question is CLOSED.** SHEETS = CARRY (`PROTO-PB-3b.md:12-13`). Decidable:
YES — executed evidence forecloses it. Decided-in-direction: YES — CARRY, unconditionally. What
the codex owes now is transcription, not a decision; this is why it is not a blocker.

**But the codex text still carries the refuted framing — a MANDATORY, determinate fold (R1).**
Three loci are now self-inconsistent with the campaign's own new evidence and MUST fold before
freeze:
1. `SPEC-DRAFT.md:352-358` — the conditional gate "its retirement gates on a SHEETS differential
   oracle proving structural events + kind pass-through reproduce the consumer's navigation
   byte-identically ... else sheets view CARRIES." PB-3b proves the `else` branch is the outcome;
   the conditional framing must collapse to the unconditional CARRY delineation of
   `PROTO-PB-3b.md:150-167`.
2. `SPEC-DRAFT.md:95` and `:355` and `:922` — "−1,059 per-grammar retired" (which sums sheets
   279). PB-3b recomputes to **−780, JSON-only** (`PROTO-PB-3b.md:150-153`). The figure the codex
   still asserts is one its own evidence refutes.
3. `SPEC-DRAFT.md:367` — close gate "the retirement set spans JSON+sheets+css, so parity proves
   per-member." Sheets leaves the set; the gate must name JSON(+css) only.

**LOW nit inside R1 (arithmetic, not design).** PB-3b's −780 is `json/sink.rs`(120)+`view.rs`
(488)+`value.rs`(172), **JSON-only, no CSS** (`PROTO-PB-3b.md:150-153`), whereas the codex close
gate (`:358` "JSON/CSS retire per their own oracles") and CRIT-PB-3 (`:132-137`, css
`css_l4_declaration_values/sink.rs` ~16 LOC, data-tree-shaped so it fits the neutral surface)
both keep CSS in the retirement. The fold must pin one number: −780 (JSON) or ~−796 (JSON+css16).
Both agree sheets CARRIES; this is a two-way arithmetic reconcile, not an open question.

**Verdict Q1:** the seam is settled; the fold is prescribed and non-contentious; no residual
design question survives.

---

## Q2 — The ledger re-home table CLOSES TB1.W4's audit risk; no outright orphan.

The pass-1 architecture B2 blocker was "DEFERRED-LEDGER continuity not asserted under GW→TB;
the REDRESS rejected-route registry absent" (`CRIT-codex-architecture.md:61-79`); the amendment
homed the audit at TB1.W4 (`SPEC-DRAFT.md:227-231`) but left the ~40-row mapping "owned but
unenumerated" (`CONVERGENCE-LEDGER.md:21,59-61`) — the residual risk that "TB1.W4 could surface
an orphan that forces a re-home." research/09 discharges exactly that risk: it walks **70
distinct rows** (`research/09:175-184`) — §0 REDRESS, C1-C6, the SK-V19 W-map, N1-N27, the 23
discoveries — assigning every one a `TW*→GW*→TB*/PT-*` coordinate with a SPEC-DRAFT line cite.

**No row is orphaned outright** (`research/09:182-184`). Three SOFT-orphans and one re-home flag
survive, each already carrying a named recommended home:
- **N5** dead/lying oracle surfaces + W3C rekey — no wave owns "hygiene sweep over EXISTING
  oracle surfaces" (distinct from TB1.W4's *new* harness and TB6.W0's every-grammar-*has*-one
  guarantee); recommend fold into TB1.W4 or TB6.W0 (`research/09:146-151`).
- **TS/WASM backends (Lock 5) + TS path runtime** — standing deferrals SK-V23 never re-states;
  the DEFERRED-LEDGER's own standing law requires TB1.W4 to name them "authority unchanged,
  re-verified" (`research/09:152-159`). Softest — a naming line, zero work.
- **N20's revival door** — the structural-bitmap "return only via §5 cost table with a measured
  win" clause never bound to TB5.W1's KernelCostTable; recommend one TB5.W1 charter clause
  (`research/09:160-165`). This is the same architecture surface as pass-1 L1 and REDRESS-53
  (`CRIT-codex-architecture.md:169-176`) — folding it here closes L1 too.
- **P7 phantom-fork** — not an orphan but a re-home caution: its literal `TW6` label maps by
  content to TB3.W1 (GW2a), so "TB1.W4's continuity audit should walk content, not labels"
  (`research/09:167-171`). A genuine de-risk against a mechanical `TW6→TB6` misfile.

**Verdict Q2:** the pre-enumeration is done; the audit now VERIFIES rather than DISCOVERS, which
is precisely the de-risk `CONVERGENCE-LEDGER.md:59-61` called for. Residual (R2): fold research/09
as the pre-drafted table referenced by TB1.W4 (`:227-231`), plus the three soft-orphan clauses
(N5, the TS/WASM pair, N20) into TB1.W4 and the N20 clause into TB5.W1 (`:395-399`). Determinate,
non-contentious, wave-verifiable at TB1.W4 close. No orphan = no blocker.

---

## Q3 — The interlock graph is ACYCLIC; the adjudication outcomes reinforce it.

Traced against `SPEC-DRAFT.md:662-704` with research/10 and PB-3b/research/09 folded. The partial
order holds: `TB1<TB2<TB3<{TB4∥TB5}<TB6`; `PT-R1<PT-R2<PT-R3`; `PT-T4<PT-T5`. Cross-edges:
`TB1.W1→PT-R3.W0→TB1.W2` (edge 1), `PT-R3.W1↔TB3.W2` (edge 2, concurrent pairing),
`TB5.W0↔PT-companion` (edge 3), `PT-T4.W2→TB4.W3` (edge 4), `TB3.W2 needs PT-R2` (edge 5). No
parse-that node depends on a bbnf node LATER than its own upstream dep (`PT-R3` depends on
`TB1.W1`, upstream of everything it pairs with); no back-edge exists. **Acyclic, confirmed.**

**The adjudications make it MORE decoupled, not less.** research/10 Item 1 RATIFY delivers the
sole time-sensitive row (PT-R.5 repoint) as a **dated ask at TB1 opening** with a **bounded
NEIGHBOR-GRACE fallback** — TB1.W2 proceeds even if unconfirmed (`research/10:33-48`,
`SPEC-DRAFT.md:680-688`). That converts edge 1 from a hard cross-repo gate into a soft
default-proceed ask: the "3-tranche parse-that serialization" pass-1 M2 feared is dissolved. Item
2 (NEIGHBOR-GRACE duplicate) and Item 3 (release.yml aarch64-only) are intra-TB1.W2/W3 — no new
edge. PB-3b (sheets CARRY) and research/09's homings are intra-wave charter clauses — no new edge.
Graph unchanged and acyclic.

**Residual (R3): the pass-1 MODERATE interlock gaps persist un-folded.** The pass-1 amendment
folded only the three BLOCKING design questions (B1 @host→TB4.W1, B2 ledger→TB1.W4, B3
TB3.W2 split — `SPEC-DRAFT.md:938,935,332,342`); M1-M5 were left. Verified still-open:
- **M1** (decode-ordering): TB5 entry-dep is still the bare parenthetical "TB4 optional-parallel
  (the value plane furnishes `materialize_f64`)" (`:388-389`) — SK-V21's "TW4.1 lands before TW3
  closes" ordering law (which cost a tranche split) is not restated. Both greenfields still author
  `materialize_f64` concurrently. Wave-ownable: name the seam owner or add "TB5.W0 before TB4.W2
  closes."
- **M2** (PT-R3.W0 mis-homing): entry-dep still "PT-R2 AND TB1.W1" (`:574-576`) while PT-R3.W0 is
  clamped to TB1's window (`:577-580`). The SLA is fixed (opening-time delivery + fallback) but
  the DECLARATION inconsistency — a wave that must be actionable at TB1 opening sitting in a
  tranche gated on PT-R2 — is not. Not a cycle (M2 said so); a declaration cleanup.
- **M3** (non-standing allowlist): TB2.W2 empties it once (`:180,259,268`) but no meta-invariant
  re-checks `ALLOWLIST=={self-ref}` at every later wave's close — a later tranche can silently
  re-grow the const. "Regrowth-proof" (axis 1) needs the standing clause.
- **M4** (vendor-vs-x86, the substantive one): C.2 walks "src + generated + emitters" (`:156`),
  unspecified for the byte-identical vendor mirror `crates-infra/bbnf-regex` (`:296,887`). If
  parse-that's engine (a non-compellable proposal) ever retains an x86 arm, a byte-identical sync
  either fails C.2 or bypasses it. Mitigated but NOT resolved by edge-5's stable-build gate
  (`:697-701`, a nightly/portable engine can't vendor — but a stable x86 de-nightly could). Fix
  direction clear (C.2 scans the vendor; pin advances only when upstream is x86-clean,
  gate-verified at sync), so wave-ownable — the closest residual to a design reconciliation.
- **M5** (TB4/TB5 six-cap overload): still un-acknowledged as declared schedule risk. Softest.

None is a cycle or an open design question; each is a one-clause spec edit or wave-owned gate.
But four of five sit in the architecture/interlock plane and were flagged a full pass ago, so
they are the honest drag keeping this below "settled."

---

## Why 94, and what the fold owes

Zero blocking issues: PB-3b closes the sheets design question (CARRY), research/09 leaves no
outright orphan, the graph is acyclic and the adjudications decouple it further. Per the freeze
criterion "zero open design questions, every residual wave-owned," there is no open design
question left on this lens. The distance to 100 is entirely **determinate un-applied edits**, none
contentious:
- **R1** — fold PB-3b CARRY into TB4.W2 (`:352-358`); recompute −1,059→−780/−796 (`:95,355,922`);
  re-scope the close gate to JSON(+css) (`:367`).
- **R2** — reference research/09 as TB1.W4's pre-drafted table (`:227-231`); add the N5, TS/WASM,
  and N20 soft-orphan clauses; add the N20 door clause to TB5.W1 (`:395-399`); state the
  walk-content-not-labels caution.
- **R3** — fold the five pass-1 MODERATES (M1 materialize_f64 ordering, M2 PT-R3.W0 declaration,
  M3 standing-allowlist meta-invariant, M4 C.2-scans-vendor + clean-pin, M5 TB4/TB5 known-heavy
  risk), which the pass-1 amendment skipped.

`ready_to_freeze = FALSE` — not because any residual is an open question, but because these
determinate folds are agglomeration edits not yet in the codex text, and until R1 lands the codex
asserts a retirement figure (−1,059) its own executed evidence refutes. One agglomeration pass
applies all of it; every residual is then wave-owned. Expected post-fold: ≥98 on this lens.
