# Phase 7.5B Classification — PASS-1 V7 friction residuals R1-R3

This file records the verify-then-patch classification for the Phase 7.5B
PASS-1 narrow-amendment cycle dispatched after V7 PASS-1 returned READY at
`6011e7f2` with four non-blocking friction residuals (R1-R4). Source is
`restart/audit/hardening/HARDENING-PASS-1-V7.md` §5 friction-residual ledger.
R4 (`BBNF-PATTERN-NONEXHAUSTIVE` sync into ARCH §7.4 catalogue) is out of
scope here — ARCHITECTURE.md is Phase 7.5A's write surface — and routes
to Phase 7.5A as a coordination item. Phase 7.5B's scope is R1-R3 only,
single-file write surface against `restart/audit/pass-1-substrate/PASS-1.md`.

## Classification table

| Item | Surgery directive | Current PASS-1.md state | Classification | Surgery to land |
|---|---|---|---|---|
| R1 — Lock 4 + Lock 10 cross-references | Add lock-number citations to PASS-1 §3 type-system algorithm prose: `Lock 4` (post-Phase-7.1 amended; DK13 + GADT-hidden + closure-by-`&'i`) at the higher-rank/closure topics; `Lock 10` (post-Phase-7.1 amended; 6-directive grammar + first-class function values) at the function-value/grammar topics. One inline citation per topic. | `:73` carries the HM + Pierce-Turner + DK13 stack with no lock-number citation. `:75` carries the function-value paragraph + closure-by-`&'i` rule with no lock-number citation. The substance lands verbatim per V7 §2 row "Lock amendments cited"; the literal `Lock 4` / `Lock 10` tokens are absent. | patch-delta | Append "(per Lock 4 amendment, Phase 7.1)" inline at the DK13 higher-rank claim at `:73`; append "(per Lock 4 amendment, Phase 7.1; closure-by-`&'i` rule)" inline at the closure-capture rule at `:75`; append "(per Lock 10 amendment, Phase 7.1; function values + lambda first-class)" inline at the `FnType` admission at `:75`. Three inline citations close the lock-citation hygiene gap with zero substance change. |
| R2 — Backend trait cross-reference | Add ARCH §7.5 / `RustBackend` cross-reference to PASS-1 §2 codegen handoff context. The substance carry is at PASS-1 §2 per-backend lowering obligations table (`:61-71`); the formal Rust trait realization lives at ARCH §7.5 (`ARCHITECTURE.md:1067-1144`). | `:43-71` carries Backend IR ownership + per-backend lowering obligations table. `:179` and `:304` route to "Architecture §7" without naming the trait. Literal tokens "Backend trait" / "RustBackend" / "ARCH §7.5" do not appear in PASS-1. | patch-delta | Append a single sentence to the per-backend lowering obligations table preamble (around `:61` or immediately following `:71`): "PASS-1's per-backend obligations table is consumed by the V1 `RustBackend: Backend` impl per ARCH §7.5 (`restart/ARCHITECTURE.md:1067-1144`); future `WasmBackend` and `TsBackend` impls land V2 without re-architecting BIR or PASS-1's substrate." One sentence closes the cross-document binding. |
| R3 — closure-capture-by-move verbatim diagnostic | Append a verbatim parse-error message for closure-capture-by-move. The §6 grammar surface at `:248` carries the rule ("capture-by-move is a parse error in V1"); the §6b diagnostic ledger at `:107-121` is the natural format-consistent home for the message itself. | `:75` (§3) carries "capture-by-move is forbidden in V1"; `:248` (§6) carries "capture-by-move is a parse error in V1". No verbatim diagnostic message for the rule exists in either §6b ledger or §3 prose. | patch-delta | Add a row to the §6b diagnostic strings ledger (`:107-121`) for `BBNF-CLOSURE-CAPTURE-BY-MOVE` with the verbatim message: `closure body in {rule} captures {binding} by move; V1 closures capture by &'i Tape<'i> reference only — rewrite the capture as a borrow, or wait for V2 capture-by-move amendment.` Append a sentence pointer at `:248` linking the §6 grammar rule to the §6b diagnostic code. Two surgical edits close the closure-capture parse-error verbatim gap. |

## Out-of-scope routing

| Item | Routing |
|---|---|
| R4 — `BBNF-PATTERN-NONEXHAUSTIVE` sync into ARCH §7.4 catalogue | Phase 7.5A. ARCHITECTURE.md is 7.5A's write surface; PASS-1 already owns the diagnostic string at `:118` per the diagnostic-string ownership fence at `:107`. The catalogue sync is a synthesis-level amendment, not a PASS-1-local one. |

## Routing summary

| Surgery class | Items |
|---|---|
| patch-delta | R1, R2, R3 |
| out-of-scope | R4 (escalated to Phase 7.5A) |

## Acceptance gates carried into the amendment commit

- R1 closed: `rg -n 'Lock 4|Lock 10' restart/audit/pass-1-substrate/PASS-1.md` returns positive.
- R2 closed: `rg -n 'ARCH §7\.5|RustBackend' restart/audit/pass-1-substrate/PASS-1.md` returns positive.
- R3 closed: `rg -n 'BBNF-CLOSURE-CAPTURE-BY-MOVE|capture.*by.*move' restart/audit/pass-1-substrate/PASS-1.md` returns positive.
- Substance unchanged: every existing PASS-1 §2 / §3 / §6 / §6b sentence retains its prior wording; surgeries are additive citations + one new diagnostic ledger row + one cross-reference sentence.
- Voice unchanged: archaic-permissive, citation-laden, no metalanguage.

The amendment commit lands these surgeries verbatim against PASS-1.md; this
classification file is preserved as evidence that V7 baseline was inspected
before the Phase 7.5B narrow amendment.
