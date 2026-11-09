# CRIT-codex-charter — adversarial critique of the CODEX (charter + coordination)

**Agent:** critique (numerically conservative). **Item:** `codex-charter`. **Date:** 2026-07-10.
**Scope:** PASS-ALPHA.md (the charter) + the coordination layer (§4.0/§5/Appendix A of
SPEC-DRAFT.md, grounded in research/06). No cargo. Every charter demand walked; each marked
OWNED / OWNED-AS-PROPOSAL / GAP with evidence file:line.

**Convergence: 88%.** The codex is comprehensive — all seventeen charter demands are homed and
the greenfield delineation is complete. Two open questions block "executable tomorrow": the
ORDERING-LAW cross-repo handoff has no defined delivery/confirmation artifact and hard-blocks
TB1's critical path on an external human (B1); and charter axis-1/axis-2 "BOTH repos" is
structurally unachievable-as-written for parse-that under the non-clobber law, delivered instead
as proposals the campaign cannot land — a definitional tension the owner must ratify (B2). The
remainder are precision refinements.

---

## The demand-by-demand ledger

| # | Charter demand (PASS-ALPHA) | Verdict | Home / evidence |
|---|---|---|---|
| D1 | x86 abrogation — bbnf | OWNED | §2.1 legacy-dies-with-archive; TB1.W2 `git rm crates/`; TB2.W2 residues |
| D2 | x86 abrogation — parse-that | OWNED-AS-PROPOSAL | PT-R1.W1 (review-not-execute; §4.0) — cannot execute under non-clobber |
| D3 | x86 enforcement — bbnf | OWNED | §2.2 C.2/C.3/C.4 @TB1.W3, C.1 @TB3; allowlist TB1→TB2 |
| D4 | x86 enforcement — parse-that | OWNED-AS-PROPOSAL | PT-R1.W2 (proposal) |
| D5 | SIMD modernization — bbnf | OWNED | TB5.W0-W4 (Eisel-Lemire, cost-table, SOTA NEON, CSS index) |
| D6 | SIMD modernization — parse-that | OWNED-AS-PROPOSAL | PT-R2.W0-W5 |
| D7 | greenfield: type system | OWNED | §1 table row 2; TB4.W1 (net-new ~1,000 LOC) |
| D8 | greenfield: SIMD | OWNED | §1 row 4; TB5 dispatch greenfield + carry primitives |
| D9 | greenfield: value API | OWNED | §1 row 3; TB4.W2 |
| D10 | greenfield: hardcoding (WART-C) | OWNED | §1 rows 1+5; TB3.W1 name-gate dissolved structurally |
| D11 | ≥5 tranches — bbnf | OWNED | six (TB1-TB6) |
| D12 | ≥5 tranches — parse-that | OWNED | five (PT-R1-3, PT-T4-5) |
| D13 | modern precept codex governing | OWNED | §0 (research/07 items 1-57); minor: amends charter's "up to 8" (R5b) |
| D14 | non-clobber: additive-only | OWNED | Appendix A NEW file; in fact handed off entirely (stronger than additive) |
| D15 | non-clobber: review-not-execute | OWNED | §4.0; all PT rows are proposals |
| D16 | non-clobber: ID collision-proof (research/06) | OWNED | `BBNF-PT-F` prefix disjoint from `VALUEJS-PT-`/`KF-TO-PARSETHAT-` (research/06:36-52) |
| D17 | non-clobber: nothing stages in parse-that | OWNED | letter placed by owner not bbnf agent; PB-1/5/6 on scratch copies (§7) |

Every demand is owned. The two structural tensions (B1, B2) and four refinements (R2-R6) below
cap convergence.

---

## BLOCKING

### B1 — The ORDERING LAW has no delivery/confirmation artifact; TB1 critical path hard-blocks on an external human

The highest-consequence cross-repo edge (§5 edge 1): `TB1.W1 (port-in) → PT-R.5 (parse-that
repoints .cargo/config.toml) → TB1.W2 (crates/ removal)`. Three unresolved seams:

1. **Delivery channel undefined, and in tension with the charter.** PASS-ALPHA:38-44 frames the
   one letter as "delivered at close to keep those sessions informed." But the repoint ask is
   needed at TB1 **opening**. The SPEC papers this over — "the letter text is frozen NOW so the
   ask reaches the owner at TB1 opening" (SPEC-DRAFT:604-605) — yet the letter is a bbnf-internal
   Appendix-A doc until an owner places it, and the SPEC names no mechanism by which the owner
   learns to read it at open. The charter's own delivery timing ("at close") contradicts the
   coordination requirement ("at open"). Unreconciled.
2. **Confirmation artifact undefined.** "TB1.W2 does not proceed until the repoint is confirmed"
   (SPEC-DRAFT:603) — but *what* confirms it? Under non-clobber, bbnf may read parse-that
   read-only; the natural artifact is "bbnf greps `parse-that/rust/.cargo/config.toml` and
   verifies `egraph` now points at `crates-infra/`." That is viable but the SPEC never names it,
   so the gate has no falsifier. A gate with no artifact is not a gate (violates research/07
   "hard gates need artefacts").
3. **No fallback; unbounded external dependency on the critical path.** TB1 → every bbnf tranche.
   TB1.W2 blocks on a manual edit to a gitignored file in a repo this campaign may not touch, by
   an owner with no SLA. If the owner never acts, TB1 never closes and the whole codex stalls.
   No-workarounds forecloses a shim, but the SPEC offers no decoupling (e.g., retain
   `crates/egraph` alone as the sole un-removed legacy path until confirmed, splitting W2). "Every
   residual wave-owned" fails here: this residual is owned by a party outside both automated repos.

**Fix to reach 100:** name the confirmation artifact (read-only `config.toml` grep asserting the
`crates-infra` repoint); reconcile the charter's "delivered at close" with the "text-available-
at-open" reality by explicitly stating the frozen-Appendix-A-read-at-open channel as a charter
amendment; and provide a decoupling fallback for the crates/egraph removal if the repoint is
unconfirmed at TB1 close.

### B2 — Axis-1/axis-2 "BOTH repos" is unachievable-as-written for parse-that; the outcome is contingent, not delivered

The charter's binding axes state outcomes: "Full x86 abrogation — BOTH repos" (PASS-ALPHA:9) and
"deep, idiomatic, fully modern SIMD ... BOTH repos" (PASS-ALPHA:14). The non-clobber law
(PASS-ALPHA:36-44) simultaneously forbids the campaign from modifying parse-that. These two
charter clauses are in structural tension: the campaign **cannot** abrogate x86 or land SIMD in
parse-that — it can only author PROPOSALS (PT-R1/PT-R2) that parse-that pulls "on its own cadence"
(SPEC-DRAFT:420-427). The SPEC resolves this honestly and it is the only lawful resolution, but it
means the charter's stated axis-1/axis-2 *outcomes* for parse-that are not guaranteed by this
campaign: if parse-that never adopts PT-R1, x86 persists there indefinitely and the anti-regrowth
guard (D4) never plants. This is an open design question the owner must adjudicate — accept
"parse-that outcomes are contingent on parse-that's own adoption," or relax non-clobber for the
abrogation execution alone. Until adjudicated, "BOTH repos" reads as delivered when it is
proposed. Flag it in the charter's own axis language, not only in §4.0's fine print.

---

## REFINEMENTS (non-blocking; close for full precision)

**R2 — The letter's "dead ... should delete" framing under-prioritizes a build-critical deletion.**
Verified: parse-that's live coupling is `egraph`/`egraph-derive` only (consumed at
`parse-that/rust/regex/Cargo.toml:35-36`); `bbnf`/`bbnf-ir`/`bbnf-ser` are inert `[patch]` entries
with no manifest consumer (`config.toml:8-10`), so deleting them is *semantically* safe — the
letter's instinct is correct. BUT cargo eagerly loads every `[patch]` entry's target manifest to
resolve its package name; once TB1.W2 removes `crates/core|ir|ser`, those patch lines point at
absent `Cargo.toml` files and can fail `cargo` resolution **even though the crates are
unconsumed**. So their deletion is as build-time-critical-before-GW0.c as the egraph repoint, not
the softer "should delete" the letter assigns (SPEC-DRAFT:792). Elevate all four config.toml edits
to the single MUST-before-removal set. (Cannot run cargo to confirm the load-error behavior;
flagged as a risk to verify parse-that-side.)

**R3 — The charter mis-cites SK-V22 for the archive sequence.** PASS-ALPHA:31 attributes
"cleanup/archival execution (SK-V22 §7 sequence)." Verified: sk-v22/SPEC.md §7 is "THE FORWARD
PROMPT" (sk-v22/SPEC.md:524); the executable archive sequence is §2 / §2.1 "tags first, then
removals — order is normative" (sk-v22/SPEC.md:166,181). The SPEC-DRAFT silently uses the correct
§2.1 (SPEC-DRAFT:594) but never flags the charter's error — violating the evidence-integrity /
no-silent-correction discipline. Record the charter correction explicitly.

**R4 — The x86 census is not exhaustive.** §2.1 names the skinny residue as `passes/src/lib.rs:380`
(field) + `:960` (consumer). A third live site exists: `skinny/crates/passes/src/lib.rs:1944`
constructs `TargetFeatures { avx512bw: true, ... }` inside `#[test]` (mod tests @:1674). It is
doubly covered downstream (TB2.W1 relocates the test block; TB2.W2 field-deletion forces the edit
via compile error), so benign — but a census the SPEC calls "dispositioned" and normative should
enumerate all constructors or the executor working from it alone hits an unlisted compile break.

**R5 — Two stated figures don't reproduce / one charter number is amended.**
(a) research/01's "502 x86 hits" in legacy `crates/`: a broad grep
(`x86_64|core::arch::x86|_mm_|__m128/256/512|is_x86_feature|avx2|avx512|pclmul|target_feature=…`)
returns **259**. Immaterial to ownership (the whole tree `git rm`s at TB1.W2), but a normative
figure that doesn't reproduce weakens the census's authority — reconcile the counting predicate or
restate. (b) §0-C1 reduces PASS-ALPHA:48's "up to 8 agents" to "7-or-fewer read-only lanes / 6
write ceiling / 3 default." Legitimate (precept codex governs), but it is a charter amendment —
name it as one.

**R6 — The non-clobber posture is *stronger* than the charter authorizes; say so.** PASS-ALPHA:40-44
permits the campaign to add exactly one letter to parse-that. The SPEC declines even that,
handing placement to "the owner or a parse-that-side session, never ... a bbnf agent"
(SPEC-DRAFT:424,767). Safer, and it fully satisfies D17 — but it means the campaign's own
deliverable ("deliver one letter") is delegated, so the campaign cannot itself guarantee the
letter is ever placed. Combined with B1, the entire cross-repo interlock rests on out-of-band
owner action. Worth stating plainly rather than leaving as an implication.

---

## What is genuinely strong (conceded, not padding)

- The greenfield delineation (§1) is complete against all four charter-named problems
  (hardcoding/type/SIMD/value) with per-subsystem seed + owner + differential-oracle law; the one
  scope override (type plane greenfield vs SK-V22's "B3 width patch") is flagged, not buried.
- The ID-collision proof (D16) is airtight: research/06:36-52 establishes prefix-disjointness and
  the empty collision surface (only tranche `A/` exists); Appendix A restates it faithfully.
- The x86 enforcement design (§2.2) is layered correctly with C.2 (structural token gate, built on
  the proven `lock14_baseline.rs`/`skv15_w0.rs` falsifier shape) load-bearing and a born-RED
  planted-phantom gate — not a passive lint.
- The non-clobber discipline in prototyping (§7: PB-1/5/6 on scratch copies) is consistent.

## Residual open questions (must be wave-owned or charter-adjudicated for 100)

1. B1's delivery channel + confirmation artifact + fallback for the ORDERING LAW.
2. B2's owner adjudication of "contingent vs delivered" for parse-that axis-1/axis-2 outcomes.
3. R2's cargo patch-load behavior verified parse-that-side (elevates 3 more config.toml edits to
   MUST-before-removal).
