# SK-V12 S-P2 PIN-V3 CH4 Cost / Scalar-Reference / Checkasm Review

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V3.
Date: 2026-05-20.
Lens: CH4 cost / scalar-reference / checkasm.
Commit reviewed: `75233b2b`.
Verdict: ACCEPT.
Score: 97%.

## Blocking Findings

None.

The Cycle V3 fold after PIN-V2 closes the remaining CH4 accounting gap. No
row in P2-A through P2-F now relies on global prose for scalar-reference
status, checkasm/parity or explicit N/A, micro-proof or explicit N/A,
same-wave consumer/proof or explicit ineligible status, `escape_mask_64`
prerequisite where implicated, or aarch64 orphan consume/remove/demote handling
where production support is touched.

## Review

| Artifact | CH4 result | Evidence |
|---|---|---|
| P2-A SOTA comparator teardown | ACCEPT | The C1-C7 table carries row-level class, scalar reference, checkasm/parity expectation, micro-proof, same-wave consumer, `escape_mask_64` / Lock 16 prerequisite, orphan disposition, and P1 antecedent. C6 is output-plane/oracle contract and parser-row-mover ineligible alone; C7 is a generated-template legality surface rather than a standalone primitive. |
| P2-B DAV1D/FFmpeg ASM process | ACCEPT | The admission-gate table now has row-level scalar-ref status, checkasm expectation, micro-proof / explicit N/A, same-wave consumer class, and orphan disposition. The bitmap prefix/next/bulk rows, byte-context support, cache hints, and output-digest oracle rows each say consume same-wave, demote/remove, inventory/drop, or parser-candidate-ineligible as applicable. |
| P2-C host-arch ASM/SIMD esoterica | ACCEPT | The selectable set is explicitly C1, C3, C4, C5, and C6. C2, C9, and C11 are inventory/drop; C7, C8, C10, and C12 are support inventory until a named same-wave consumer, scalar/checkasm proof, and any `escape_mask_64` / REDRESS material differential exist. Each detailed C1-C12 entry carries scalar-ref status, micro-proof need, and same-wave consumer or nonselectable status. |
| P2-D substrate/tape design | ACCEPT | The §2 table now includes a checkasm/parity status column. Diagnostic/rejected same-tape rows carry scalar-ref status, explicit checkasm N/A or parity requirement, same-wave consumer/proof, micro-proof need, and P2-D verdict. The retained sidecar/class-lane shape remains rejected. |
| P2-E parse-that primitive gaps | ACCEPT | The table has candidate class, scalar-ref status, checkasm/parity expectation or N/A, same-wave consumer, orphan / Lock 16 disposition, and micro-proof need for parser rows, Layer-0 support, output-plane/fact-stream, and oracle/accounting rows. `escape_mask_64` is correctly binding for string-region SIMD only. |
| P2-F grammar-neutral abstraction | ACCEPT | The main map plus CH4 accounting supplement closes the cross-artifact legality surface. Support-only, inventory/drop, diagnostic-only, oracle/accounting, rejected, and parser-candidate-ineligible families carry micro-proof / same-wave proof and orphan disposition, rather than inheriting those from prose. |

USER-PIN D5/D6 and Lock 16 remain intact. The five carried orphan surfaces
(`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, `cache_hints`) are not admitted by research wording; they must
be consumed in a same-wave caller, explicitly demoted/removed, or remain close
blockers. The `escape_mask_64` xorshift falsifier remains a hard prerequisite
before any new SIMD admission, matching the CHECKASM report and the aarch64
coverage audit.

## Nonblocking Notes

- The historical pre-pin `hardening/V3/*` files and consolidated V3 marker are
  superseded by the pin-aware `PIN-V*` flow and should not be used as the
  authoritative S-P2 convergence marker. The live `HARDENING-S-P2-CONVERGED.md`
  correctly says PIN S-P2 hardening is still in progress and routes PIN-V3 over
  the folded Cycle V3 packet.
- P2-C's source still labels the split "PIN-V1 split" in prose, but the content
  is the folded Cycle V3 candidate/inventory split. This is not a CH4 blocker
  because the per-row accounting is explicit and current.
- P2-F is deliberately a legality map, not an implementation plan. Its CH4
  supplement is sufficient for S-P3 to avoid over-reading support/oracle rows
  as candidate evidence.

## Exact Fold Edits If REVISE/REJECT

None. No fold edits requested by CH4.

## Commands Used

```sh
git status --short && git rev-parse --short HEAD && ps -axo pid,comm,args | rg '(cargo|rustc|xctrace|samply)' || true
rg --files restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12 | rg '(p2[abcdef]-|PIN-V3|USER-PIN|CHECKASM|aarch64-simd-coverage|PASS-2-RESEARCH|HARDENING-S-P2|SPEC|HANDOFF)'
rg -n "CH4|cost|scalar|checkasm|micro|consumer|orphan|escape_mask|Lock 16|D5|D6" restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md skinny/crates/bbnf-simd/CHECKASM-REPORT.md restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md
sed -n '1,360p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md
sed -n '1,360p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
sed -n '90,150p' skinny/crates/bbnf-simd/CHECKASM-REPORT.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md
find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 2 -type f | sort
rg -n "Scalar-ref status|Scalar reference|Checkasm|checkasm|Micro-proof|micro-proof|Same-wave|same-wave|Orphan|orphan|escape_mask_64|N/A|ineligible|Inventory/drop|Support-only|Support inventory|Diagnostic-only|Parser-candidate-ineligible" restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md
sed -n '1,200p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
git show --stat --oneline --decorate --no-renames 75233b2b
```
