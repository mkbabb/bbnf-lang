# SK-V14 W5B.1 CH2 V1: Data Integrity

Date: 2026-05-26.
Scope: W5B.1 source-map and hash integrity.
Disposition: ACCEPT.

## Findings

The redress shape preserves request `rel_path` strings as identity and does not
canonicalize paths through the filesystem. Hashes remain derived from the exact
request source strings, so W5B.4 can consume closure facts without depending on
host-local paths or regenerated output.

## Required Folds

None.
