# PASS-IMPL V2 Agent 1: JSON Hardcoding

Verdict: ACCEPT.

JSON remains the honest proof-of-concept axis. `skinny/RESULTS.md` keeps 51 / 51
JSON rows as `A`, `GO`, `strict`, and `measured-row`, split across
`parse_only`, `direct_to_struct`, and `real_typed_struct`. SK-V15 W1-W11 did
not move or retime JSON rows, so no new JSON SOTA claim is introduced by the
close packet.

W10 quarantines the W11L/W11N/W11O FNV closed-enum bench metadata so it cannot
migrate into production correctness. The quarantine tests reject hash-equal
typed-semantic mismatch and shared closed-enum sidecar coupling.

Residual risk: JSON remains host-tuned to Apple M5 Max / aarch64 admission
conditions. That is allowed for this campaign and is not generalized to x86.
