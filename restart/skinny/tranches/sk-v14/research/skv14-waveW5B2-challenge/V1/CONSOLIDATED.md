# SK-V14 W5B.2 Challenge V1 Consolidated

Date: 2026-05-26.
Scope: W5B.2 layout/discard frontend facts.
Disposition: REVISE, folded into `skv14-W5B2-plan.md`.

## Lens Result

CH1 ACCEPT. `RuntimeFrontendClosure` remains the fact carrier.

CH2 ACCEPT. Raw construct counts are preserved and new facts keep request path,
source hash, and byte-span identity.

CH3 REVISE. Add W5A source-fact, JSON unchanged-output, and Sheets/BBNF-self
carry checks.

CH4 ACCEPT. W5B.2 does not advance later W5B/W5C/W5D/W6 work.

CH5 REVISE. Public `?w` syntax needs an explicit parser guard, and malformed
runtime prefixes must fail closed.

CH6 REVISE. Every exact gate and carry command needs a dedicated log and
nonzero proof grep.

CH7 ACCEPT. No generated-runtime, provider/template, or codegen-consumer
overfit route is admitted.

## Redress Dispatch Contract

Redress may edit only `skinny/crates/grammar/src/lib.rs` and write dedicated
`/tmp/skv14-w5b-<test-name>.log` proof logs. It must preserve raw construct
counts, add additive layout facts under `RuntimeFrontendClosure`, reject public
`@ws` and `?w` compatibility syntax, fail closed on malformed layout/discard
scanner forms, and leave codegen behavior plus provider/template/generated
runtime files untouched.
