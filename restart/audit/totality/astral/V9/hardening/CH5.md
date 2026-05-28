# Pass Omega V9 CH5 Hidden Coupling

Date: 2026-05-28.
Lens: CH5 hidden coupling.
Source commit: `17e7248fe`.
Disposition: ACCEPT.

## Verdict

ACCEPT. The Pass Omega V9 packet hardens the hidden-coupling boundary rather
than expanding it. Omega-C proposes one G-Omega-gated locks addendum, but the
addendum preserves the 16-lock count, the five `BackendShape` variants, Apple
M5 Max/aarch64-only admission evidence, scalar/parity/same-wave consumer
requirements for primitives, and the Lock 1 substrate union. Omega-D and
Omega-F route SK-V15 as W0-W11 implementation authority without introducing a
parallel substrate, public substrate API, `FactStream` shape, EventTape sidecar,
second tape, retained cursor/list/class-column state, production FNV arbiter,
generic grammar branch, or self-exempting gate.

## Audit

| Check | Result | Evidence |
|---|---|---|
| Lock count and G-Omega boundary | ACCEPT | Omega-C says the amendment is not a lock-count change or new `BackendShape` (`restart/audit/totality/astral/V9/ΩC-locks-amendments.md:10`-`14`) and records the lock-count check as `16` (`:27`-`:31`). The proposed locks diff states the amendment is G-Omega gated (`restart/audit/totality/astral/V9/locks-diff.md:1`-`3`) and adds no lock, lock retirement, substrate, public substrate API, sidecar, directive, BIR variant, or sixth shape (`:5`-`:11`, `:45`-`:49`). |
| Five `BackendShape` variants only | ACCEPT | The locks diff repeats exactly `EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage` (`restart/audit/totality/astral/V9/locks-diff.md:23`-`30`) and its Lock 10 clause requires an all-five gate over exactly that set while keeping any sixth shape G-Omega gated (`:63`). The source enum at `17e7248fe` has exactly those five variants (`skinny/crates/ir/src/lib.rs:339`-`346`), and `all_backend_shapes()` returns `[BackendShape; 5]` (`skinny/crates/ir/src/cost.rs:333`-`340`). |
| `FactStream` is not a sixth shape or retained sidecar | ACCEPT | Current Lock 1 classifies `FactStream` as a substrate-manifest category only and explicitly says it is not a sixth `BackendShape` (`restart/locks/LOCKS.md:100`-`109`). The V9 locks diff keeps `FactStream` outside `BackendShape` (`restart/audit/totality/astral/V9/locks-diff.md:7`-`10`) and says it is output-plane/admitted-product only, not a retained internal sidecar (`:49`). |
| No public `UnionTape`, second tape, retained cursor/list/class-column state, or Track 1 == Track 2 sidecar | ACCEPT | SK-V15 non-negotiables reject retained cursor/list, sidecar event vector, second tape, public `UnionTape`, retained class/structural/cursor stream, Track 1 == Track 2 sidecar, new substrate API, and new/sixth `BackendShape` (`restart/skinny/tranches/sk-v15/SPEC.md:147`-`153`). The Lock 1 addendum repeats the same boundary and rejects runtime regex/DFA substrate absent later G-Omega Lock 1 amendment (`restart/audit/totality/astral/V9/locks-diff.md:49`). |
| EventTape stays a BackendShape lowerer, not a sidecar | ACCEPT | SK-V15 requires W9 to implement or gate-reject EventTape/SinkOnly/CollapsedStage and add the all-five gate (`restart/skinny/tranches/sk-v15/SPEC.md:412`-`419`). Its exit gate says EventTape is not a sidecar vector, sixth shape, retained stream, public substrate API, or alternate projection (`:423`-`:428`). |
| Runtime regex/DFA not admitted as substrate | ACCEPT | The V9 Lock 1 addendum says runtime regex/DFA manifest plus consumer proof is necessary but never sufficient and any runtime regex/DFA substrate requires prior G-Omega Lock 1 amendment (`restart/audit/totality/astral/V9/locks-diff.md:49`). The Lock 16 addendum repeats that a runtime regex/DFA substrate cannot proceed without prior Lock 1 amendment (`:71`). |
| Apple M5 Max/aarch64-only SIMD and primitive close | ACCEPT | SK-V15 says Apple M5 Max/aarch64 is the only admission host and x86/AVX-512 are diagnostic (`restart/skinny/tranches/sk-v15/SPEC.md:135`-`136`). The locks diff preserves that (`restart/audit/totality/astral/V9/locks-diff.md:10`-`11`) and Lock 16 requires owner, scalar oracle, strict parity/checkasm, Apple M5 Max/aarch64 hardware gate or fallback, same-wave consumer, row movement, rollback, and final disposition (`:71`). Omega-D and Omega-F carry the same scalar/parity/same-wave constraints (`restart/audit/totality/astral/V9/master-plan-diff.md:79`-`83`; `restart/audit/totality/astral/V9/ΩF-migration-handoff.md:30`-`34`). |
| No wrong-host `CollapsedStage` or primitive close | ACCEPT | Existing Lock 16 refuses AVX-512 literature as M5/aarch64 close evidence and mechanically refuses `CollapsedStage` on aarch64 until a generated aarch64 strategy lands (`restart/locks/LOCKS.md:515`-`533`). V9 keeps x86/AVX-512 diagnostic-only and requires source-present primitives to have same-wave consumers (`restart/audit/totality/astral/V9/master-plan-diff.md:79`-`83`). |
| No generic grammar branches or self-exempting gates | ACCEPT | The locks diff bans `RuntimeGenerationMode`, profile arrays, CSS profile matches, JSON/CSS runtime families, JSON punctuation or role mining, generic grammar switches, and generic-crate grammar branches, while requiring Lock 14 gates to report included roots, excluded roots, owner, reason, self-scan status, primitive status, gate consumer, affected rows, and disposition (`restart/audit/totality/astral/V9/locks-diff.md:67`). SK-V15 also says Lock 14/16 gates must report exclusions and fail on self-exempting scans (`restart/skinny/tranches/sk-v15/SPEC.md:64`-`68`, `:219`-`:244`). |
| FNV remains bench-only, not production correctness | ACCEPT | SK-V15 closes only if W11L/W11N/W11O FNV closed-enum products remain bench-only and strict-product comparison catches closed-enum sidecar coupling (`restart/skinny/tranches/sk-v15/SPEC.md:74`-`75`). W10 quarantines FNV, scans production roots, and blocks runtime selector / production arbiter / correctness proof use (`:430`-`:445`). Omega-D and Omega-F preserve this quarantine (`restart/audit/totality/astral/V9/master-plan-diff.md:93`-`94`; `restart/audit/totality/astral/V9/ΩF-migration-handoff.md:86`-`88`). |
| Omega-D/Omega-F do not create a second substrate/API route | ACCEPT | Omega-D says the MASTER patch should not edit SK-V15 `SPEC.md`, only mark stale SK-V14/MP-NW blocks historical and add the active SK-V15 receiver block (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:30`-`34`). The new receiver block is a MASTER receiver, not a second SPEC (`:66`-`:68`), and its non-negotiables preserve five shapes, aarch64-only admission, typed CSS proof, FNV quarantine, and no renewed planning loop (`:70`-`:85`). Omega-F is proposal-only, does not authorize source/generated/gate/runtime deletion edits, and routes those to SK-V15 W0-W11 after G-Omega (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:17`-`19`, `:64`-`:67`). |

## Required Folds

None before consolidation. The packet is acceptable as written for CH5.
Consolidation and CRUD must preserve the following constraints exactly:

1. Apply the Omega-C locks addendum only through G-Omega. Preserve 16 numbered
   locks and the exact five `BackendShape` variants:
   `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`.
2. Keep `FactStream` output-plane/admitted-product only. It is not a sixth
   `BackendShape`, retained sidecar, public substrate API, second tape, or
   alternate document projection.
3. Treat runtime regex/DFA manifest plus consumer proof as necessary but never
   sufficient; any runtime regex/DFA substrate still requires prior G-Omega
   Lock 1 amendment.
4. Preserve Apple M5 Max/aarch64 as the only SK-V15 admission host. x86,
   AVX/AVX-512, source inventory, macro names, and non-strict parity stay
   diagnostic only.
5. Require scalar oracle/reference, strict parity/checkasm where relevant,
   same-wave consumer, row movement, rollback, and final disposition for every
   SIMD/primitive route.
6. Keep W8/W9 lowerers inside the existing five-shape canon. EventTape may only
   be a lowerer output path or gate-consumed rejection, never a sidecar vector,
   public `UnionTape`, retained stream, or sixth shape.
7. Keep FNV closed-enum products bench-only. Production FNV selectors,
   arbiters, hash correctness proofs, or runtime leakage remain blocked.
8. Keep Lock 14 gates non-self-exempting: every generic owner path reports
   included roots, excluded roots, reason, owner, self-scan status, primitive
   status, gate consumer, affected rows, and disposition; no generic grammar
   branches or JSON/CSS family switches may enter.
