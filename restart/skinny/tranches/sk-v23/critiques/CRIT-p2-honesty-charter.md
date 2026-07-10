# CRIT-p2-honesty-charter — the closing-pass honesty + charter re-critique

**Agent:** pass-2 re-critique (adversarial, numerically conservative). **Item:**
`p2-honesty-charter`. **Date:** 2026-07-10. **Scope:** the LENS of PASS-ALPHA — does the pass-2
evidence (PROTO-PB-3b, research/09, research/10) honestly resolve the pass-1 freeze-blockers,
and is the amended codex still charter-faithful across the three binding axes and the
non-clobber law? NO cargo run except read-only greps of the live skinny tree to falsify PB-3b's
structural claims. Every load-bearing fact carries file:line.

**Verdict up front.** The pass-2 evidence is HONEST and the design is converged: PB-3b settles
the sheets seam by executed byte-parity in the conservative direction; the adjudication packet
is charter-consistent and genuinely veto-able; all three binding axes and the ≥5-tranches-each
demand remain owned. There are ZERO surviving design blockers. But there is ONE real,
nameable, must-fix-before-freeze residual that is NOT wave-owned: **the SPEC-DRAFT text
(mtime 00:33) predates all three pass-2 artifacts (PB-3b 00:51, research/09 00:41, research/10
00:39) and references none of them** — a verbatim freeze right now would stamp a −1,059
headline that PB-3b has falsified to −780. That fold is the protocol's own step-5 synthesis, not
a TB/PT wave. **Convergence: 97%.**

---

## Q1 — Is PB-3b's parity claim byte-identical against a REAL oracle? (HONEST, with one disclosed caveat)

**The load-bearing structural claims are TRUE against the live tree — independently falsified,
not taken on trust.** I re-grepped the actual skinny sheets grammar:

- `sheets/view.rs` is **279 LOC** (matches the CARRY figure); `GoogleSheetsKind` has **39**
  variants (`view.rs:9-47`, Number..Formula); `KIND_TABLE` at `view.rs:52`. PB-3b's
  "39-kind, view.rs:52-92" is exact.
- `sheets_conformance.rs:90-91` is verbatim `.nodes().find(|node| node.rule_kind() == Some(kind))`
  — the flat-sweep, navigate-by-kind consumer PB-3b's verdict turns on. Confirmed.
- The decisive tape-shape claim holds byte-for-byte in `generated.rs`: **exactly 39**
  `let _ = (Nu8, &mark)` no-ops, **exactly one** live `mark(...)` call (`generated.rs:225`, the
  string closer), and `match_leaf → tape.push_offset(start, OffsetFlags(kind))` at
  `generated.rs:161`. Structural rules record nothing; only leaves seal a pair. PB-3b's "flatter
  than a 39-kind sweep" is corroborated by the real generator, not just the scratch port.

**The one honesty caveat (disclosed, and safe-directional).** PB-3b's "oracle" is a faithful
*reproduction* of `sheets/view.rs` inside `/private/tmp/pb3b-scratch`, not the compiled sheets
crate linked and invoked. Its PART-2 "byte-identical (offset AND value)" parity is therefore a
self-consistency check between two author-written surfaces in the same scratch crate (the ported
`match_leaf` recorder vs the reproduced `GoogleSheetsKind`/`KIND_TABLE` oracle), not a
differential against the shipped module. PB-3b discloses this ("reproduce `sheets/view.rs`
line-for-line", PROTO-PB-3b:137), so it is not a hidden overclaim — but the summary phrase
"measured against it, not against a hand literal" (PROTO-PB-3b:137) slightly oversells: a
faithful reproduction is a richer literal, still not the real invocation. This is the *same*
class of gap CRIT-PB-3 flagged as Overstatement-A on PB-3 (hand-literal vs real
`to_canonical_string`); PB-3b narrows it but does not fully close it.

**Why the caveat is acceptable and the verdict trustworthy.** (1) The verdict is **CARRY** — the
conservative direction; an imperfect oracle cannot cause a *wrong retirement*, only at worst an
over-cautious keep. (2) The load-bearing facts underneath the verdict (39 no-ops, one mark,
leaf-only recording, rule_kind consumer) I verified against the *actual* tree above. (3) PB-3b's
negative core — pure `Classify` is byte-DIVERGENT (`assert_ne!(Some(1), Some(0))`: Identifier
and Formula both collapse to `Scalar(Raw)`, PROTO-PB-3b:88-91) — needs no oracle fidelity; it is
a projection-cardinality argument (39 kinds ↛ 8 classes) that stands on the enum shapes alone.
The verdict is sound. **This resolves the pass-1 PB-3b blocker (CONVERGENCE-LEDGER.md:49-54) in
the CARRY direction.**

---

## Q2 — Adjudication packet: charter-consistent AND veto-able? (YES on both, with one governance note)

**Charter-consistent.** All three recommendations track the binding text:

- **Item 3 (release.yml x86 LSP matrix)** defaults to *aarch64-only* — the only reading that
  honors "full x86 abrogation … everywhere" as binding (PASS-ALPHA.md:9-13). It adds an
  owner-gated named-exception door for the distributed-editor artifact alone, never the
  parser/kernel/emit plane the anti-regrowth gate guards. Charter-faithful (axis 1 honored).
- **Items 1 & 2 (contingent-delivery + NEIGHBOR-GRACE)** are the only lawful reading of "BOTH
  repos" under the non-clobber law (PASS-ALPHA.md:35-44): deliver PROPOSALS, never claim
  parse-that OUTCOMES; touch parse-that via one owner-placed letter, review-not-execute. This is
  exactly the charter's own posture, not a relaxation of it.

**Veto-able.** Every item carries explicit `RATIFY / Veto` boxes, a rationale, and a stated VETO
CONSEQUENCE (research/10:31,79,119; decision block 151-155). The packet is honest about the one
veto branch that is NOT gracefully absorbable: if the owner insists "BOTH repos be reported as
DELIVERED," the two directives are irreconcilable and "the orchestrator would halt for a charter
re-issue rather than either clobber parse-that or misreport" (research/10:59-66). Disclosing a
hard-halt branch rather than papering it is the correct honesty posture.

**Governance note (not a blocker).** The packet asserts "Silence is consent … the orchestrator
executes the recommendation as written" (research/10:9-12,157-162). This *self-grants* a
default-proceed authority the charter never explicitly confers on an OWNER decision the pass-1
ledger classified as "NOT wave-ownable" (CONVERGENCE-LEDGER.md:55-58). For Items 2 and 3 (no
external clock, contained blast radius) default-proceed is defensible; for Item 1's PT-R.5 row it
is bounded by the NEIGHBOR-GRACE fallback (research/10:43-48) so it cannot stall the critical
path. The mechanism is sound and honestly framed — but a strict charter reading would want the
owner's default-proceed authority *stated as an assumption the owner may correct*, not asserted
as a standing rule. Fold one sentence: "absent a veto the orchestrator proceeds — this
default-authority is itself the first thing a veto may withdraw."

---

## Q3 — Every pass-1 blocker resolved or wave-owned? (YES, but one resolution is not yet FOLDED)

| Pass-1 blocker (source) | Pass-2 disposition | Folded into SPEC-DRAFT? |
|---|---|---|
| Sheets seam feasibility (LEDGER §4.1) | **RESOLVED → CARRY** by PB-3b executed byte-parity | **NO** — codex still says −1,059 / gate-on-oracle |
| Owner-ratification set (LEDGER §4.2) | **PACKAGED** veto-able + default-proceed + fallback (research/10) | Partial — §4.0/§5 clauses exist (SPEC-DRAFT:477,683); packet not referenced |
| DEFERRED-LEDGER enumeration (LEDGER §4.3) | **DONE** — 70 rows walked, 4 soft-orphans named (research/09) | **NO** — TB1.W4 charter (SPEC-DRAFT:227-231) not yet pointed at the pre-map |
| Codex-honesty B1 fused-trigger (CRIT-codex-honesty:91-98) | **RESOLVED + FOLDED** | YES — SPEC-DRAFT:531-537, item 9 (915) |
| Codex-honesty B2 HM-core-consumer (CRIT-codex-honesty:100-109) | **RESOLVED + FOLDED** | YES — SPEC-DRAFT:334,340-341 scaffold-dormant |
| Codex-charter B1 ordering-law artifact (CRIT-codex-charter:47-76) | **RESOLVED + FOLDED** | YES — confirmation grep + NEIGHBOR-GRACE (SPEC-DRAFT:683-687) |
| Codex-charter B2 "BOTH repos" contingent (CRIT-codex-charter:78-91) | **RESOLVED + FOLDED** | YES — CONTINGENT-DELIVERY clause (SPEC-DRAFT:477-482) |
| CRIT-PB-3 retire-vs-carry wrongly resolved (CRIT-PB-3:173-181) | **RESOLVED → CARRY** by PB-3b | **NO** — same stale −1,059 |

Every blocker is resolved at the EVIDENCE level or folded. The three still-unfolded rows all
point at the same single action below.

---

## Q4 — 5-tranches-each, greenfield delineation, SIMD/x86 axes: still owned? (YES — verified)

- **≥5 tranches each:** bbnf = TB1..TB6 (six), parse-that = PT-R1/R2/R3 + PT-T4/T5 (five). Both
  ≥5. Grep-confirmed in SPEC-DRAFT. Charter D11/D12 owned.
- **Greenfield delineation (§1 table, SPEC-DRAFT:94-96):** Type plane GREENFIELD (~1,000 net-new,
  "there is NO type system to carry"); Value plane GREENFIELD; SIMD dispatch GREENFIELD + carry
  the ~1,600-LOC aarch64 primitive bodies; WART-C dissolved structurally at TB3.W1. All four
  charter-named problems (hardcoding/type/SIMD/value) homed.
- **Axis 1 (x86 abrogation, BOTH repos + anti-regrowth):** §2 (SPEC-DRAFT:110), TB1.W3
  anti-regrowth guard (220), release.yml policy call recorded for owner (221). parse-that half is
  OWNED-AS-PROPOSAL (PT-R1) under non-clobber — the only lawful form. Owned.
- **Axis 2 (SOTA NEON within the ASM-hybrid union, BOTH repos):** SPEC-DRAFT:96 (carry the
  primitive bodies = the SK-V21 win), TB5.W2 vshrn-nibble-domain movemask (400-402), vshrn idiom
  family + multi-block pipelining + bulk unescape (384); parse-that regex prefilters
  (shufti/Teddy-class) owned at PT-R2 (531). The Hyperscan-lineage prefilters land on the
  parse-that regex plane and the vshrn/table-lookup class on bbnf's kernel plane — a clean, not
  gapped, split of the charter's two named targets. Owned.

No axis has drifted or dropped in the amendment.

---

## THE ONE BLOCKER (real, mechanical, synthesis-owned — NOT a TB/PT wave)

**The codex text lags its own settled pass-2 evidence by one synthesis fold.** SPEC-DRAFT.md
(mtime 00:33) references NONE of PB-3b, research/09, or research/10 (grep returns empty). The
live consequences of freezing it verbatim right now:

1. **`SPEC-DRAFT:95` states the retirement as an unconditional "−1,059 per-grammar retired"
   headline** in the §1 delineation table — a number PB-3b has FALSIFIED to −780 (JSON-only:
   `json/sink.rs`120 + `json/view.rs`488 + `json/value.rs`172), with `sheets/view.rs`(279)
   CARRY. The conditional body at `SPEC-DRAFT:352-357` already names the CARRY fallback ("else
   sheets view CARRIES under a named row"), so the codex was honestly hedged — but the fold must
   collapse the hedge to CARRY, correct the headline number, and **strike the sheets differential
   oracle from the TB4.W2 close gate** (PB-3b:159-163 says do NOT build it; NativeTag re-imports
   the 39-ordinal and is rejected).
2. **research/09's 70-row pre-map is not wired into TB1.W4's audit charter** (SPEC-DRAFT:227-231),
   and its four honestly-flagged soft-orphans (N5 oracle-hygiene; Lock-5 TS/WASM + TS-path;
   N20 revival-door) have no home sentence yet. research/09 self-discloses the "~40 vs 70" count
   gap (research/09:179-184) — honest, but the recommendation ("TB1.W4 names these verbatim") is
   unfolded.
3. **§4.0 OWNER-RATIFY (SPEC-DRAFT:477) and the ordering law do not yet point at the now-existing
   adjudication packet** as their realization.

**Why this is a blocker and not a wave-owned residual.** PASS-ALPHA step 5 is exactly this fold
("a final synthesizing agent agglomerates; the loop re-enters … whereupon 100% convergence:
stop"). The fold is SYNTHESIS-owned — the closing act of *this* pass — not a TB/PT execution
wave. So under the freeze criterion "all residuals wave-owned = 100," this residual is NOT
wave-owned, and the honest score cannot be a flat 100. It is small, mechanical, and every input
exists; but it is a real must-do-before-the-freeze-stamp item, and the anti-asymptote discipline
forbids waving it through.

---

## Score & residual ownership

**Convergence: 97%.** Zero surviving design/architecture blockers; the sheets seam is settled
CARRY by executed evidence, the owner set is a charter-consistent veto-able packet with a bounded
fallback, and all three axes plus the ≥5-tranches-each demand remain owned. The 3-point gap is
the single synthesis fold above: the SPEC-DRAFT must agglomerate PB-3b (−780/CARRY, drop the
sheets oracle), research/09 (TB1.W4 pre-map + name the four soft-orphans), and research/10
(§4.0/ordering-law → the packet) before the freeze stamp. **residuals_wave_owned = FALSE** — the
fold is synthesis-owned (protocol step 5), not a TB/PT wave; and the owner-ratification default,
though execution-wave-owned at TB1.W2/W3, still rests on an owner veto window with one honestly
disclosed hard-halt branch. On that fold this pass reaches ≥99 with every remaining residual
either wave-owned (TB close gates) or a disclosed owner decision with a safe default.
