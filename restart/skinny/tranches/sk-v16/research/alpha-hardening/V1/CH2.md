# SK-V16 Alpha V1 CH2 - Generality

Disposition: REVISE-FOLDED.

## Finding

Alpha-E Candidate A over-authorized `crates/core/src/runtime/css_l4/**` as an
implementation owner path. Under Lock 14 this must not become hand-written
grammar-specific runtime code.

## Fold

Candidate A now marks core CSS runtime as read/delete/replace-only. Writable
scope is restricted to grammar source/metadata, grammar-neutral
generator/template code, generated provider manifests, and generated runtime
outputs with byte-equivalent regeneration evidence.
