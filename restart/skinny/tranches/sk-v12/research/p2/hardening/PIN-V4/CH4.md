# SK-V12 S-P2 PIN-V4 CH4 Cost / Scalar-Reference / Checkasm Review

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V4.
Date: 2026-05-20.
Lens: CH4 cost / scalar-reference / checkasm.
Commit reviewed: `b407583e`.
Research fold reviewed: `75233b2b`.
Verdict: ACCEPT.
Score: 97%.

## Blocking Findings

None.

The unchanged folded Cycle V3 S-P2 packet still satisfies CH4. No row in P2-A
through P2-F relies on global prose for scalar-reference status, checkasm/parity
or explicit N/A, micro-proof or explicit N/A, same-wave consumer/proof or
ineligible status, `escape_mask_64` prerequisite where implicated, or aarch64
orphan handling. The packet keeps USER-PIN D5/D6 and Lock 16 intact: SIMD/ASM
rows are scalar-oracle-first, checkasm/parity-gated, same-wave-consumer-gated,
and zero-orphan-aware.

## Review

| Artifact | CH4 result | Evidence |
|---|---|---|
| P2-A SOTA comparator teardown | ACCEPT | C1-C7 carry row-level class, scalar reference, checkasm/parity expectation, micro-proof, same-wave consumer, `escape_mask_64` / Lock 16 prerequisite, orphan disposition, and P1 antecedent. C6 is explicitly an output-plane/oracle contract and parser-row-mover ineligible alone; C7 is a generated-template legality surface rather than a standalone row mover. |
| P2-B DAV1D/FFmpeg ASM process | ACCEPT | The gate table gives each row scalar-ref status, checkasm expectation or N/A, micro-proof or explicit N/A, same-wave consumer class, orphan disposition, and P1 antecedent. Support rows such as bitmap prefix/next/bulk emit, byte context, cache hints, movemask, and output digest all say consume, demote/remove, inventory/drop, or parser-candidate-ineligible as applicable. |
| P2-C host-arch ASM/SIMD esoterica | ACCEPT | The selectable set remains C1, C3, C4, C5, and C6. C2, C9, and C11 are inventory/drop; C7, C8, C10, and C12 are support/inventory until a named same-wave consumer, scalar/checkasm proof, and required `escape_mask_64` / REDRESS material differential exist. Each C1-C12 entry carries scalar-ref status, micro-proof need, and same-wave consumer or nonselectable status. |
| P2-D substrate/tape design | ACCEPT | Every diagnostic or rejected row in the §2 table carries scalar-ref status, checkasm/parity status or N/A, same-wave consumer/proof, micro-proof need, Lock 1 effect, and verdict. Same-tape CSS union remains an aperture after CSS baseline evidence, not an admitted shortcut. |
| P2-E parse-that primitive gaps | ACCEPT | The candidate table labels parser row movers, Layer-0 support, output-plane/fact-stream, and oracle/accounting rows separately, with scalar-ref status, checkasm/parity expectation or N/A, same-wave consumer, orphan / Lock 16 disposition, and micro-proof need per row. `escape_mask_64` is binding only where string/escape masks are consumed. |
| P2-F grammar-neutral abstraction | ACCEPT | The legality map plus CH4 accounting supplement prevents S-P3 from treating support/oracle/accounting families as implicit candidates. Byte-set, string/escape, number, dispatch, tape/fact-stream, bitmap support, byte-context, cache-hint, digest/oracle, diagnostic, rejected/inventory, and GrammarConfig families all carry micro-proof/same-wave proof and orphan disposition. |

The `CHECKASM-REPORT.md` state remains load-bearing: the `escape_mask_64`
xorshift falsifier (`0xCAFEF00DBAADF00D`) is still a hard prerequisite before
new SIMD admission. The aarch64 coverage audit's carried orphan set
(`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, `cache_hints`) is not admitted by research wording; each must be
consumed in a same-wave caller, explicitly demoted/removed, or remain a close
blocker.

## Nonblocking Notes

- This is the second clean CH4 lens check after PIN-V3, but only the
  consolidator should update the global clean-cycle count and S-P2 convergence
  marker after all six PIN-V4 lenses are present.
- P2-C still labels the candidate/inventory split as "PIN-V1 split" in prose.
  The content itself is the folded Cycle V3 surface, so this is not a CH4
  blocker.
- The packet is intentionally conservative about CSS-local union and ASM-gen:
  USER-PIN D3/D4 reopen categories, but every row still requires material
  differential, scalar reference, checkasm/parity, micro-proof, and same-wave
  consumer before S-P3 can route it.

## Exact Fold Edits If REVISE/REJECT

None. No CH4 fold edits requested.

## Commands Used

```sh
git status --short && git log --oneline -5
ps -axo pid,stat,comm,args | rg '(cargo|rustc|xctrace|samply)' || true
find restart/skinny/tranches/sk-v12/research/p2 -maxdepth 3 -type f -print | sort | xargs ls -lt | head -40
rg -n "CH4|cost|checkasm|scalar|micro|orphan|Lock 16" restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CH4.md
sed -n '1,240p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CONSOLIDATED.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
rg --files restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12 skinny/crates/bbnf-simd | rg '(p2[abcdef]-|PIN-V4|PIN-V3|USER-PIN|CHECKASM-REPORT|aarch64-simd-coverage|HARDENING-S-P2|PASS-2-RESEARCH|SPEC|HANDOFF)'
rg -n "Scalar-ref status|Scalar reference|Scalar ref|Checkasm|checkasm|Micro-proof|micro-proof|Same-wave|same-wave|Orphan|orphan|escape_mask_64|N/A|ineligible|Inventory/drop|Support-only|Support inventory|Diagnostic-only|Parser-candidate-ineligible|parser-candidate-ineligible|output-plane|oracle/accounting" restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md
sed -n '90,170p' restart/prompts/skinny/PASS-2-RESEARCH.md
sed -n '45,105p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
sed -n '90,155p' skinny/crates/bbnf-simd/CHECKASM-REPORT.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md
sed -n '1,150p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md
sed -n '1,240p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
git diff --quiet --exit-code; echo "tracked_diff=$?"
git status --short
git rev-parse --short HEAD
git show --stat --oneline --no-renames --decorate --max-count=1 HEAD
ls -la restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V4 2>/dev/null || true
rg -n "Cycle: V3|Candidate \\| Class|Candidate \\| Shape|Scalar-ref status|Checkasm/parity status|CH4 accounting supplement|PIN-V3 update|Clean-cycle count|Status:" restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CONSOLIDATED.md restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
```
