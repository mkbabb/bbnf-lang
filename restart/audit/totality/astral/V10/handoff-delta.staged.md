# STAGED HANDOFF.md Delta — Pass Omega V10 (SK-V18 Generalization)

STAGED ONLY. CRUD-4b applies this to `restart/HANDOFF.md` POST-G-Omega V10.
Anchors are against the live `restart/HANDOFF.md` at this snapshot (502 lines;
"Current Totality Override - 2026-05-30" at `:3`; the stale SK-V18-adopt
paragraph at `:16-19`; the SK-V17 dispatch directive at `:90-110`).

Sources: T-P3 `3F-migration-handoff.md` deltas 3F-MH-004/005/006/007;
`restart/skinny/tranches/sk-v18/SPEC.md:19-21`,`:46-49`,`:54-169`,`:429-449`,
`:471-484`; `restart/skinny/tranches/sk-v18/HANDOFF.md:1`; 1F `COH18-001..010`;
1D `D-1..D-8`/`G-6`/`G-7`/`G-8`/`G-13`.

---

## OP-1 — INSERT the current override block ABOVE the live SK-V17 override

INSERT the following block immediately ABOVE the live
`## Current Totality Override - 2026-05-30` (`restart/HANDOFF.md:3`). The live
SK-V17 override block is RETAINED below as the immediately-prior provenance
(the SK-V17 close is real and stays cited); it is no longer the CURRENT
top-of-file authority.

```md
## Current Totality Override — Pass Omega V10 / SK-V18 Generalization (2026-06-01)

Status: **SK-V18 is the GENERALIZATION cycle on the SKINNY tree
(`skinny/crates/`)**: un-fork the two hand-written/forked parsers (JSON + CSS)
into ONE grammar-driven generator emitting JSON + CSS + Sheets from `.bbnf`,
aarch64-only, preserving >SOTA honestly (CSS beats lightningcss 1.66–3.38×;
JSON beats sonic-rs strict), ≈ −10800 campaign LOC (per-wave SPEC sum
≈ −10685; `sk-v18/SPEC.md:571`). The totality `crates/core/`
adoption is **SK-V19**, NOT SK-V18
(`restart/skinny/tranches/sk-v18/SPEC.md:19-21`,`:58-61`).

Skinny S-P0..S-P3 CERTIFIED — the 12-wave SPEC (`restart/skinny/tranches/sk-v18/SPEC.md`).
Totality T-P1 SK-V18 near-converged NON-normal-§3Z (NOT a normal two-clean §3Z
lock), T-P2 SK-V18 near-converged NON-normal-§3Z (converged=false, consec=0),
T-P3 SK-V18 CONVERGED into the 3A..3F synthesis + the 3C-locks-v+1-diff
(21 candidates: 9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER; git apply --check exit 0
against live LOCKS.md). Pass Omega V10 (astral) staged; **G-Omega V10 pending**.

After T-P3 cohort lock, G3 auto-passes under the active non-G-Omega gate pin and
the packet flows into **Pass Omega V10** (the astral directory index;
NOT "V6" — Pass Omega V5 CLOSED for SK-V17 at `33b51d8f4`, and V6..V9 are
historical SK-V15 lineage). No SK-V18 implementation wave dispatches until Pass
Omega V10 CRUD has updated HANDOFF/MIGRATION current-state truth AND G-Omega V10
has authorized the required V1 patches.

Current SK-V18 wave authority routes through
`restart/skinny/tranches/sk-v18/SPEC.md` (the 12-wave manifest) and
`restart/skinny/tranches/sk-v18/HANDOFF.md` (the generalization handoff). On
G-Omega V10 close → SK-V18 W-PRUNE (P1-P5) implementation, the ONLY
dispatch-eligible cluster on close (`restart/skinny/tranches/sk-v18/SPEC.md:46-49`).

The 16-lock count + 5 `BackendShape` canon are PRESERVED (the 3C-locks-v+1-diff
is amendment by addition; no renumber).
```

---

## OP-2 — STRIKE the stale SK-V18-adopt sentence (mid-`:16` through mid-`:19`)

STRIKE ONLY the SK-V18-adopt sentence, which begins MID-`:16` (immediately after
the preserved preamble "SK-V17 skinny waves W0-W5 are dispatchable under the
SKINNY triumvirate. ") and ends MID-`:19` (immediately before the preserved
trailing "No SK-V18 wave dispatches…"). The byte-exact span to strike, as it
stands across the live `restart/HANDOFF.md:16`-`19`, is:

> The next IMPLEMENTATION tranche is **SK-V18**: it adopts the SKINNY-proven
> unified-tape / lazy-`ValueRef` / shared-NEON model into the totality
> `crates/core/` tree, per the five LOCKED fold designs.

PRESERVE — do NOT delete — the surrounding shared-line text that abuts the struck
sentence on both ends: the `:16` preamble **"SK-V17 skinny waves W0-W5 are
dispatchable under the SKINNY triumvirate."** at the front of `:16`, and the
`:19`-`20` trailing **"No SK-V18 wave dispatches until the required V1 patches are
authorized at this G-Omega."** that begins after the struck sentence on `:19`.

Operator note: the sentence wraps `:16`/`:17` ("…SKINNY triumvirate. **The**" |
"**next** IMPLEMENTATION tranche…"), so `grep -nF 'The next IMPLEMENTATION tranche
is'` returns 0 — anchor the strike on the byte-exact contiguous span
`grep -nF 'next IMPLEMENTATION tranche is **SK-V18**'` (resolves at `:17`) and the
end token `grep -nF 'per the five LOCKED fold designs.'` (resolves at `:19`),
deleting only the bytes between (and including) the two tokens' sentence span.

REPLACE the struck sentence (KEEPING the `:16` preamble and the `:19`-`20`
trailing sentence) with:

```md
The SK-V17 tape-fold contract is HISTORICAL provenance below; the CURRENT tranche
is **SK-V18, the GENERALIZATION cycle** (see the override block above). The
adoption of the SKINNY-proven unified-tape / lazy-`ValueRef` / shared-NEON model
into the totality `crates/core/` tree is **SK-V19**, NOT SK-V18.
```

So the post-strike `:16`-onward paragraph reads "SK-V17 skinny waves W0-W5 are
dispatchable under the SKINNY triumvirate. The SK-V17 tape-fold contract is
HISTORICAL provenance below; … is **SK-V19**, NOT SK-V18. No SK-V18 wave
dispatches until the required V1 patches are authorized at this G-Omega." — the
preamble and the trailing dispatch-gate sentence are intact.

Rationale: COH18-001 — the single most material drift. The live `:16-19` defines
SK-V18 as a totality-`crates/core/`-adopt cycle; the certified SK-V18 is the
SKINNY generalization cycle, and that totality adoption is the SK-V19 obligation.

---

## OP-3 — RE-ROOT the dispatch-directive SK-V18 line (`:103-105`)

In the live `## Pass Omega V5 SK-V17 tape-fold dispatch directive`
(`restart/HANDOFF.md:90`), the clause (d)-(e) (`:103-105`) reads "dispatch
**SK-V18 W0** (the `crates/core` tape-fold) through the SKINNY triumvirate;
(e) SK-V18 waves then adopt the five LOCKED fold designs…". RE-ROOT clauses (d)-(f)
so the SK-V18 reference is the SKINNY generalization, and the `crates/core`
tape-fold is reassigned to SK-V19:

```md
(d) after Pass Omega V10 CRUD current-state truth is complete and G-Omega V10 has
    authorized the HANDOFF/MIGRATION/LOCKS patches, dispatch **SK-V18 W-PRUNE
    (P1-P5)** — the SKINNY generalization cycle — through the SKINNY triumvirate;
    NOT the `crates/core` tape-fold (that adoption is SK-V19);
(e) SK-V18 GENERALIZE/PROVE/HONESTY waves then dispatch in SPEC dependency order
    (G1 → G2 → G3 → G4; G5/G6 parallel; PROVE after G4; H1 last), each blocked
    until its predecessor's exit gate closes AND its entry-gate predicate holds
    GREEN;
(f) do not close SK-V18 with a verbatim-blob "generator", a 6th `BackendShape`, a
    relocated seam, an x86/AVX/SVE close path, a second substrate, or doc-only
    proof; SK-V18 cannot close while any SPEC close-condition row lacks proof,
    REDRESS route, revert evidence, or intrinsic-block proof.
```

(The directive's section header `## Pass Omega V5 SK-V17 tape-fold dispatch
directive` becomes historical; the CURRENT directive is OP-5 below.)

---

## OP-4 — ADD the SK-V18 blocker matrix

ADD after the new current override block, before the next move (3F-MH-006). Each
certified close-condition gap maps to its receiver wave + measurable gate:

```md
### SK-V18 Blocker Matrix

| Blocker | Receiver | Measurable gate |
|---|---|---|
| generator does not exist (still a verbatim-blob courier) | G1/G2/G3 | `generator_grammar_count == 3`; `verbatim_blob_present == false`; `emit_shape_source == lowered_program` |
| 7 byte-identical css_l4 replicas + 7 `RuntimeTarget` rows | P3 | `md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧ `runtime_target_rows_collapsed == true` (the md5-distinctness half is NECESSARY-not-sufficient, the structural row-collapse co-gate completes it; matches master-plan-diff `:171` + `SPEC.md:435`) |
| phantom `<G: EventGrammar>` axis | G4 | `phantom_generic_resolved == deleted`; K-axis preserved |
| CSS Value API absent (no rich-nav comparable to JSON) | G4 | shared `Cursor` trait ≥2 impls; JSON rich-nav byte-equal |
| CSS NEON dead at admission | G5/G6 | named SIMD call-site; checkasm parity green |
| x86 surface live | P1 | `x86_tree_deleted == true`; aarch64-neutral grep |
| Lock-14 green-by-exclusion | P4 | `lock14_gate_scans_codegen == true` (re-inject forbidden token → RED) |
| metalang `parse_w11_1_number` leak | P5 | `grep -c parse_w11_1_number json/generated.rs == 0` (SPEC `:755`/`:570`-scoped; unscoped crate-wide = 15) |
| Sheets is a 24-LOC stub | PROVE | `sheets_grammar_shape == pratt-operator` ∧ `generator_grammar_count == 3` ∧ `emit_shape_source == lowered_program`; `md5-distinct from JSON ∧ CSS` (the md5-distinctness half is NECESSARY-not-sufficient — a Sheets `generated.rs` can be md5-distinct yet still produced by a relocated per-grammar branch; the three-co-gate CONJUNCTION completes it, per `SPEC.md:253-254`/`:333-334`, symmetric with the P3 row above and ΩB `:121`; the binding `N`-fallback "Sheets cannot emit via the generator ONLY → generalization NOT real" is the real negative control) |
| CSS >SOTA ratio directional-not-re-locked (U-4) | H1 | `css_canon_bench` re-locked; ≥1 regular corpus crossing >1.0× same-run |

Each gate is `restart/skinny/tranches/sk-v18/SPEC.md:54-169` close conditions +
`:471-484` per-wave rerun ceilings.
```

---

## OP-5 — REPLACE the next-cycle directive (Pass Omega V10 → G-Omega → W-PRUNE)

REPLACE the historical "Pass Omega V5 SK-V17 tape-fold dispatch directive" framing
as the CURRENT directive with the Pass-Omega-V10 → G-Omega → W-PRUNE sequence
(the SK-V17 directive body stays as provenance; this is the live next-cycle
directive):

```md
## Next-Cycle Dispatch Directive — Pass Omega V10 → G-Omega V10 → SK-V18 W-PRUNE

Concrete, measurable entry conditions. Sequence: T-P3 cohort lock → G3 auto-pass
→ Pass Omega V10 → G-Omega V10 → SK-V18 W-PRUNE (P1-P5 FIRST) → SK-V19
totality-fold tee-up.

1. T-P3 cohort lock (CONVERGED: the 3C-locks-v+1-diff disposed all 21 candidates,
   git apply --check exit 0). G3 auto-passes under the active user pin; only
   G-Omega V10 triggers user relinquish.
2. Dispatch Pass Omega **V10** (the astral directory index; NOT "V6"). Pass Omega
   consumes the SK-V18 totality cycle + the SK-V18 skinny RESULTS/REDRESS into the
   V1 spec surfaces; HANDOFF + MIGRATION → CRUD-4.
3. Pass Omega V10 6-lens CHALLENGE converges to 3Z BEFORE CRUD; CRUD stays within
   the consolidated authorization.
4. CRUD current-state cleanup BEFORE G-Omega: strike the stale SK-V18-adopt
   definition + re-root the dispatch line, insert the SK-V18 §0.0 receiver, apply
   the five migration decisions. Cap-blocked remainder → blocked/extension record
   (remainder/receiver/blocker/gate); any remainder touching current dispatch
   truth blocks W-PRUNE.
5. **G-Omega V10 (mandatory user gate)** authorizes the required V1 patches
   (ARCHITECTURE/MASTER-PLAN/LOCKS/HANDOFF/MIGRATION), including the
   3C-locks-v+1-diff merge and, if scoped, the LOCKS:620 generality-vehicle
   reconcile (else deferred to SK-V19 CRUD-3). After G-Omega closes, the V1 spec
   is v+1.
6. Only after G-Omega V10 authorizes and SK-V18 authority routes through
   `restart/skinny/tranches/sk-v18/SPEC.md`, set HANDOFF to `ready-for-W-PRUNE`
   and dispatch the **W-PRUNE (P1-P5) triumvirate** — the ONLY dispatch-eligible
   cluster on close (`sk-v18/SPEC.md:46-49`). P1-P5 are entry-gate-free, MAY land
   in parallel on disjoint paths (P1 `bbnf-simd/`, P3 `xtask/regen*.rs` + the 7
   `css_l4_*/generated.rs`); **P4 MUST land before G2/G3**. Entry conditions:
   `x86_tree_deleted == true` (P1), `runtime_target_rows_collapsed == true` (P3),
   `lock14_gate_scans_codegen == true` (P4),
   `grep -c parse_w11_1_number json/generated.rs == 0` (P5; SPEC `:755`/`:570`-scoped — unscoped crate-wide = 15: 7 generated + 7 template-source + 1 `lib.rs:565` test-assert).
7. No GENERALIZE/PROVE wave dispatches until its predecessor closes its exit gate
   AND its entry-gate predicate holds GREEN AND the wave triumvirate is
   dispatched. G1 → G2 (G1 ∧ P3 close, P4 live) → G3 (G1 ∧ G2 close ∧ P4 live ∧
   P3 row-collapse) → G4 (G1 ∧ G2 ∧ G3 close); G5/G6 hangs off G3 PARALLEL to G4;
   PROVE needs G4 closed DIRECTLY (NEVER before G4); H1 needs G5/G6 ∧ PROVE.
   H1 cannot close if the CSS >SOTA ratio is not re-locked on `css_canon_bench`
   with ≥1 regular corpus crossing >1.0× same-run.
8. SK-V19 totality-fold tee-up: the SK-V18-proven un-fork adopted into
   `crates/core/`. Entry carriers (each cited, none dropped): (a) the
   `ir/registry/strategy.rs` 9-grammar `PRODUCTION_MANIFEST_TABLE` relocated-seam
   analog (COH18-005); (b) the totality `css_types.rs` RELOCATE-or-DELETE; (c) the
   Pattern-H 67/71 baseline-command reconcile (COH18-007); (d) the scanner-crate
   asymmetry resolution (COH18-015); (e) the LOCKS:620 generality-vehicle 1-line
   reconcile (1A-LOCK1-AMEND-001). SK-V19 is REMAINDER after SK-V18 proof, NOT a
   substitute for SK-V18 generalization.
```

---

## Verification (post-CRUD)

- `grep -n "Current Totality Override — Pass Omega V10" restart/HANDOFF.md`
  returns 1 hit ABOVE the SK-V17 override.
- `restart/HANDOFF.md` contains NO surviving "SK-V18: it adopts … into the
  totality `crates/core/` tree" and NO "SK-V18 W0 (the `crates/core` tape-fold)"
  as CURRENT dispatch authority.
- The next-cycle directive labels the current pass "Pass Omega V10", never "V6".
- The blocker matrix has 10 rows, each with a measurable gate.
- The 16-lock / 5-shape preservation note is present.
