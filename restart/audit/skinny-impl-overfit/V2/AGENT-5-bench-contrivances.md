# PASS-IMPL V2 Agent 5: Bench Contrivances

Verdict: ACCEPT-FOR-QUARANTINE; CSS-NON-ADMIT.

The JSON bench rows remain measurement-valid under the existing strict-product
and parse-only contracts. The known FNV closed-enum technique is now explicitly
quarantined to bench/diagnostic metadata by W10 and cannot be used as a runtime
selector, production arbiter, or correctness proof.

The CSS W8R one-measurement broadcast is no longer live admission. It remains in
the ledger as diagnostic/audit-falsified evidence only, with CSS `OPEN` in
rolling delta and `admitted_rows=0` in W6/W11 typed retime evidence.
