# CH4 COST — SK-V18 T-P2 Cycle V1

## Lens

CH4 COST: every grounded primitive or rebuild route carries an admission cost —
a scalar reference + checkasm parity per Lock 16, a hardware gate, a NAMED
same-wave consumer, realistic LOC/risk, the Lock-16 v+1 manifest fields
(retention lifetime, policy owner, rollback path, abrogate threshold, final
disposition), and a wave owner keyed to the LIVE SK-V18 wave ledger. Orphan
kernels, citation-only primitives, and uncosted rewrites are inadmissible.

## Disposition

REVISE.

The SK-V18 packet is a large advance over the prior cycle on this lens: every
SIMD/ASM primitive row in 2B/2E/2F now carries scalar_reference, checkasm-parity,
hardware_gate, and a same-wave consumer inline; 2A appends loc_estimate/risk_class/
wave_owner/hard_cap_fit to its transferable parser leaves; 2C carries a per-row
cost cell; and the orphan-kernel rejections are correct (LD4/PMULL/CSSC/SVE2/FSM
all refused without consumer + row movement). The most load-bearing citations
SPOT-VERIFY as real (see Evidence Inspected). But the admission layer is NOT yet
clean under Lock 16 as actually written: (1) the dossiers carry STALE SK-V15
W-letter wave owners (W1/W2/W5/W6/W7/W8/W9) that name waves which do not exist in
the SK-V18 SPEC §8 ledger (G1/G2/G3/G4/G5-G6/PROVE/H1) — a wave-owner cost field
that points to a retired wave is a defective cost field; (2) NO dossier carries
the Lock-16 v+1 manifest's `abrogate threshold` or `rollback path`, both of which
the lock text mandates verbatim; (3) 2D's grounding table carries no per-row
LOC/risk at all for the G3 un-fork consumer route it grounds. These are foldable
in V2 — the right admission cells are present; the wave keys and two manifest
columns are missing.

## Critical Findings

| id | severity | finding | evidence |
|---|---:|---|---|
| CH4-V1-01 | critical | Stale wave-owner cost field across the packet. 2B's entire SK-V18 V3 extension costs every primitive against SK-V15 waves W2/W7/W8/W9 (38 `W#` wave_owner occurrences, ZERO G-letters); 2C's V2 section carries 22; 2A/2D carry 5/5. The LIVE SK-V18 wave ledger is `PRUNE→G1..G6→PROVE→H1` (SPEC §8 `:438`-`:443`), with NO W-waves. A `wave_owner=W7` row cannot be checked against a hard-cap budget that does not exist. The cost manifest's wave-owner column is unverifiable against the SPEC. | `restart/audit/totality/p2/2B-primitive-vocabulary.md:147`-`158` (`W2/W7/W8/W9` manifest), `restart/audit/totality/p2/2D-cost-model.md:23` (re-keyed in frontmatter but rows below still cite W-named waves), `restart/skinny/tranches/sk-v18/SPEC.md:438`-`443` (the G1/G2/G3/G4/G5-G6/PROVE ledger + ≤450 LOC + ≤90min caps) |
| CH4-V1-02 | critical | No dossier carries the Lock-16 v+1 manifest's `abrogate threshold` or `rollback path`. Lock 16 mandates them verbatim ("...retention lifetime, policy owner, same-wave production consumer, expected row/feature gate, LOC/risk, rollback path, abrogate threshold, and final disposition"). `grep -oc abrogate` and `grep -oc rollback` == 0 across all six dossiers. A primitive admitted with LOC/risk/consumer but no abrogate threshold cannot be deleted-before-patched per [abrogate-before-patch]; a route with no rollback path is an uncosted commitment. | `restart/locks/LOCKS.md` Lock 16 v+1 manifest clause (the `rollback path, abrogate threshold` enumeration); 2A-2F: `abrogate`/`rollback` occurrence count == 0 (all six) |
| CH4-V1-03 | high | 2D grounds the largest rebuild route (R-A G3 un-fork + the five-lowerer/e-graph/CSP engine) with NO per-row LOC or risk_class anywhere in its grounding or assertion tables (`loc_estimate` count == 0). The "DERIVED-not-new" framing is defensible — the engine EXISTS at HEAD (`backend_egraph.rs:76` `NormalizeDirectSinkCost` live) — but G3 still DELETES `RuntimeEmitterKind` and re-dispatches `render(program)`, which SPEC §8 budgets at ≤450 hand LOC, HIGH-risk, with a documented larger cap if §5-risk-1 fires (`:440`). 2D references neither the LOC band nor the risk class nor the firewall's own LOC. | `restart/audit/totality/p2/2D-cost-model.md:50`-`60` (grounding table, no LOC/risk column), `restart/audit/totality/p2/2D-cost-model.md:64`-`70` (assertions, no LOC/risk), `restart/skinny/tranches/sk-v18/SPEC.md:440` (G3 ≤450 LOC, HIGH-risk band) |
| CH4-V1-04 | medium | 2E and 2F carry NO structured `loc_estimate` field (count == 0 each); their LOC anchoring is prose-only. 2F cross-references the SPEC ≤450 LOC G2 band twice (`:94`,`:135`) which is the correct anchor, but the per-gap rows (regex/HIR, SIMD scan, quote-parity, float, CSS-value, provenance reconcile) do not each carry a row-local LOC/risk pair; 2E's per-row admission manifest carries scalar/checkasm/consumer/gate but omits LOC/risk for the SHRN-swap, LD4, PMULL/CSSC, DotProd, I8MM, SHA3 rows. | `restart/audit/totality/p2/2E-host-arch-esoterica.md:96`-`108` (manifest rows, no LOC/risk), `restart/audit/totality/p2/2F-parse-that-gaps.md:81`-`85` (per-gap rows, no row-local LOC/risk), `:94`,`:135` (prose ≤450 LOC anchor) |
| CH4-V1-05 | medium | 2A defers primitive-admission cost on most SOTA rows (`primitive_cost=none in 2A unless split into a 2B/2E leaf`) — which is the correct posture for a SOTA-landscape dossier — but two rows DO assert full admission cost (the sonic-rs leaf `T2A-V1-SOTA-JSON-003`: `loc_estimate=150-350 per leaf; risk_class=medium; wave_owner=W2`; the dav1d row `T2A-V18-DAV1D-001`: `loc_estimate=≤450; risk_class=MED-HIGH; wave_owner=G5/G6`). The sonic-rs leaf row carries a STALE `wave_owner=W2` (CH4-V1-01) AND no same-wave consumer named beyond a generic "W2 primitive lane / W5 typed CSS provider". Either strip admission language from the SOTA row or attach the full LIVE-wave manifest. | `restart/audit/totality/p2/2A-sota-landscape.md:56` (sonic-rs leaf, `wave_owner=W2`, stale), `restart/audit/totality/p2/2A-sota-landscape.md:168` (dav1d row, `wave_owner=G5/G6`, correct) |
| CH4-V1-06 | medium | POSITIVE CONTROL (keeps this REVISE, not REJECT): the packet's strongest groundings carry a complete manifest AND a same-wave consumer verifiable on disk, and the orphan-kernel rejections are correct. The eq-set kernel grounds with a live consumer (`find_ascii_set_member64` / `count_top_level_commas`, verified at `runtime_simd.rs`); the G6 retarget names the P3-collapsed CSS scan as consumer and the dead-kernel claim is true on disk (only `#[cfg(test)]` caller at `lib.rs:574`); LD4/PMULL/CSSC/SVE2/FSM are all refused absent a consumer + row movement. No confabulated or unverifiable citation was found (CH4-V1-07). | `restart/audit/totality/p2/2B-primitive-vocabulary.md:257`-`260`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:107` (svmatch refuted), `restart/audit/totality/p2/2F-parse-that-gaps.md:82`-`85`; on-disk: `skinny/crates/runtime/src/runtime_simd.rs:169`-`204`, `skinny/crates/runtime/src/lib.rs:574` (`#[cfg(test)]` caller) |
| CH4-V1-07 | low | Citation spot-verification PASS. Every load-bearing citation checked resolves to a real source accurately characterized: Lemire 2026 "The fastest way to match characters on ARM processors?" (SVE2 match fastest, NEON eq-fan the deployable route) — REAL, exact title/date confirmed; Validark/Niles Salter "Use interleaved vectors for parsing on ARM" (`vld4q_u8` LD4 movemask) — REAL, exact title/author confirmed; Pratt "Top Down Operator Precedence" POPL 1973 DOI 10.1145/512927.512931 — REAL, DOI resolves to that title/venue; the in-tree `checkasm_parity.rs:3` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" — VERBATIM on disk; the `find_css_significant` two-fan OR-reduce (`mask_a \| mask_b`) — VERIFIED at `runtime_simd.rs:199`. No REJECT-grade confabulation. | WebFetch (lemire.me/blog/2026/04/19, validark.dev/posts/interleaved-vectors-on-arm), WebSearch (dl.acm.org/doi/10.1145/512927.512931), `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:3`, `skinny/crates/runtime/src/runtime_simd.rs:199` |

## Evidence Inspected

- `restart/audit/totality/p2/2A-sota-landscape.md` (full)
- `restart/audit/totality/p2/2B-primitive-vocabulary.md` (full)
- `restart/audit/totality/p2/2C-grammar-neutrality.md` (full)
- `restart/audit/totality/p2/2D-cost-model.md` (full)
- `restart/audit/totality/p2/2E-host-arch-esoterica.md` (full)
- `restart/audit/totality/p2/2F-parse-that-gaps.md` (full)
- `restart/locks/LOCKS.md` Lock 16 (allowlist + v+1 primitive manifest + `BBNF_SIMD_STRICT=1` admission), Lock 16 NEON-classifier-manifest clause `:622`+
- `restart/skinny/tranches/sk-v18/SPEC.md:181`-`221` (the two >SOTA bars + outcome vocab), `:438`-`443` (the §8 wave ledger: G1/G2/G3/G4/G5-G6/PROVE LOC + hard-cap budgets)
- On-disk citation verification: `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`-`6`; `skinny/crates/runtime/src/runtime_simd.rs:169`-`204`; `skinny/crates/runtime/src/lib.rs:560`-`576` (dead `#[cfg(test)]` caller); `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:31`-`45`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:657`,`:693` (hot leaf live)
- External citation verification: Lemire 2026 ARM-match post; Validark LD4 post; Pratt POPL 1973 DOI

## Fold Requirements

1. **Re-key every wave-owner cost field to the LIVE SK-V18 §8 ledger**
   (`PRUNE/G1/G2/G3/G4/G5-G6/PROVE/H1`). Strike `W1/W2/W5/W6/W7/W8/W9` from
   every SK-V18-scope manifest row in 2B (38 rows), 2C (22 rows), 2A (sonic-rs
   leaf), and 2D. Where the V2/SK-V15 prose is retained verbatim, the SK-V18
   extension's cost rows must carry the G-letter owner and a hard-cap reference
   to the `≤450 LOC / ≤90 min wall / 30-45 min redress` SPEC band.
2. **Add the two missing Lock-16 v+1 manifest columns to every admitted-or-
   blocked primitive row**: `rollback path` and `abrogate threshold` (alongside
   the already-present scalar_reference / checkasm-parity / hardware_gate /
   same_wave_consumer / loc_estimate / risk_class / close_status). A primitive
   with no abrogate threshold cannot satisfy [abrogate-before-patch].
3. **2D**: attach a per-row LOC/risk to the R-A/G3 un-fork route and the firewall:
   the G3 ≤450-hand-LOC / HIGH-risk band (SPEC `:440`), and the
   `runtime_target_rows_collapsed` co-gate's own (+1 PartialEq derive) LOC. State
   each engine unit's disposition (`live-regression-guard` for the e-graph/CSP,
   `relocate-consumer` for G3) so the "DERIVED-not-new" framing is costed, not
   asserted.
4. **2E/2F**: give each per-gap / per-primitive row a row-local `loc_estimate` +
   `risk_class` pair (not only the prose ≤450 band): the SHRN movemask swap, LD4,
   PMULL/CSSC, DotProd, I8MM, SHA3 (2E); regex/HIR, quote-parity string-skip,
   float, CSS-value provider, provenance reconcile (2F).
5. **2A**: either strip primitive-admission language from the SOTA rows (keep
   `primitive_cost=none in 2A`) OR attach the full LIVE-wave manifest to the
   sonic-rs and dav1d rows (the dav1d row is already correct at `wave_owner=G5/G6`;
   the sonic-rs leaf is stale at `W2` and names no concrete consumer).
6. **Preserve unchanged** (positive controls): the orphan-kernel rejections
   (LD4/PMULL/CSSC/SVE2/FSM refused without consumer + row movement); the
   correct same-wave consumers (eq-set live consumer, G6 retarget = P3-collapsed
   CSS scan); the verified citations; the `BBNF_SIMD_STRICT=1` strict-checkasm
   admission discipline; the RETARGET-not-author G6 stance.

## Blocks T-P2 V1 Convergence

Yes. CH4 blocks V1 convergence until the packet's cost/admission layer is
coherent with Lock 16 as written and the LIVE SK-V18 wave ledger: stale W-letter
wave owners (a defective cost field), the two missing v+1 manifest columns
(`rollback path` / `abrogate threshold`), and 2D's absent LOC/risk for the un-fork
route. The blockers are foldable in V2 — the right admission cells are present,
the load-bearing citations verify, and the orphan-kernel discipline is correct;
the missing piece is the wave-key correction and two manifest columns, not a
re-grounding.

TALLY accept=3 revise=3 reject=0
