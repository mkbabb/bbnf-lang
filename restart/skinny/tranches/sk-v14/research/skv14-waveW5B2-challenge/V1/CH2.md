# SK-V14 W5B.2 CH2 V1: Data Integrity

Date: 2026-05-26.
Scope: W5B.2 layout fact identity and raw construct preservation.
Disposition: ACCEPT.

## Findings

The redress preserves raw `RuntimeConstruct` entries for `@ws`, `?w`, `>>`, and
`<<`, so W5A materiality validation can continue to count them. The added layout
facts carry request path, source hash, and byte spans rather than filesystem
identity.

## Required Folds

None.
