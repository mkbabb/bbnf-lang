# SK-V10 S-P2 V1 CH2: Generality And Lock 14

Disposition: REVISE.
Date: 2026-05-19.
Scope: grammar-neutrality, generated per-grammar confinement, and JSON-overfit generic APIs.
Output: this file.

## Standard

CH2 asks whether every candidate carries a P2-F grammar-neutral verdict; a
primitive proposed only because JSON needs it must be marked REVISE or REJECT
unless it is re-framed as a grammar-neutral byte-set / classifier / tape
operation or as a per-grammar template surface
(`restart/prompts/skinny/PASS-2-RESEARCH.md:102-107`).

Lock 14 is stricter than "can be used by another grammar." Generic crates may
carry zero grammar-specific modules, public types, feature flags, or grammar
match arms; per-grammar deviations belong in grammar source, workspace
metadata, or generated per-grammar runtime output only
(`restart/locks/LOCKS.md:78`). Alpha already folded this as a refusal gate for
generic-crate, codegen, or runtime-outside-JSON edits that leak JSON policy or
lack named CSS L4 / Sheets / BBNF-self proof
(`restart/skinny/tranches/sk-v10/research/alpha-hardening/V1/CH2-generality-lock14.md:13-21`).
P1 CH2 accepted the JSON-heavy profile only because it was profile evidence,
not generic policy (`restart/skinny/tranches/sk-v10/research/p1/hardening/V1/CH2.md:12-21`).

## Findings

1. The P2 packet mostly honors the Lock 14 shape. P2-F states the governing
   test directly: generic crates must stay free of grammar-specific modules,
   types, feature flags, and grammar matches; P2-F also names the CSS L4 /
   Sheets / BBNF-self proof burden (`restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md:12-20`).
   P2-A, P2-B, P2-C, and P2-D repeatedly confine structural masks to transient
   primitives and reject retained JSON structural sidecars
   (`restart/skinny/tranches/sk-v10/research/p2/p2a-sota-teardown.md:47-49`,
   `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md:183-186`,
   `restart/skinny/tranches/sk-v10/research/p2/p2c-arch-esoterica.md:53-61`,
   `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md:58-61`).

2. P2-E is the fold point. It correctly says the gap is a grammar-neutral
   Layer-1 vocabulary (`restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md:12-20`),
   but several proposed public signatures still expose JSON policy in generic
   API shape: `bounded_plain_string_end(..., quote, terminator, escape,
   control_limit)`, `decode_escape_run_x4(input, slash)` with `\uXXXX` and
   surrogate joins, `number_span_parts` over the current JSON number span
   oracle, and the current `skip_ascii_whitespace` policy surface
   (`restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md:30-36`).
   P2-E itself flags these risks in prose, but CH2 requires the candidate
   surface to be re-expressed before S-P3 shortlisting
   (`restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md:42-52`).

3. JSON product rows are correctly confined when they remain per-grammar
   generated templates or evidence rows. `instruments` typed admission is
   explicitly a per-grammar generated product row and cannot be totality proof
   (`restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md:32`,
   `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md:46`).
   Root typed generalization is acceptable only as grammar-neutral `RootShape`
   / layout algebra, with any `JsonRootSchema` branch rejected
   (`restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md:31`,
   `restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md:45`).

4. Product-specific shapes are already rejected in the right lane. P2-B rejects
   `allocation_elision_string_materializer` as not a bbnf-simd primitive and
   rejects `array_object_walk_dispatch_hint` unless it is reduced to a
   grammar-neutral byte/mask/control primitive with a same-wave consumer
   (`restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md:171-172`,
   `restart/skinny/tranches/sk-v10/research/p2/p2b-dav1d-process.md:185-186`).
   CH2 accepts those rejections.

5. W3 and structural cursor routes stay rejected under Lock 14 and Lock 1.
   P2-F bars structural sidecars and split tape, P2-E declines to nominate a
   `structural_cursor_from_movemask`, and P2-D forbids class columns, sidecar
   bitmaps, structural indexes, or second source passes
   (`restart/skinny/tranches/sk-v10/research/p2/p2f-grammar-neutral.md:51`,
   `restart/skinny/tranches/sk-v10/research/p2/p2e-parse-that-gaps.md:38`,
   `restart/skinny/tranches/sk-v10/research/p2/p2d-substrate-tape.md:30`).

## Candidate Verdicts

| Candidate family | CH2 verdict | Required fix |
|---|---|---|
| Byte-class/table classify, movemask, bitmap prefix/iteration primitives | ACCEPT | Keep alphabet/table/mask roles grammar-owned and transient. No JSON structural sidecar, class-column substrate, retained cursor, or generated JSON-only wrapper in a generic crate. |
| Tiny/bounded string scan | REVISE | Replace generic `quote` / JSON string wording with `start` or `delimiter_start` plus a grammar-owned `StringDelimiterPolicy` / byte-set policy. Generic code returns offsets and class masks only; generated per-grammar templates own quote, escape, control, and UTF-8 policy. |
| Full string scan and string segment folding | REVISE | Keep `plain_string_special_span` generic only if classes are caller-supplied. Keep `string_segments_fold` in generated templates unless its consumer trait has no JSON key/value, `JsonDigestSink`, sink-local decoded stats, or digest semantics. |
| Unicode / escape codec | REVISE | Split the generic kernel into fixed-width or parameterized hex decode/classify. Move `\u`, slash detection, surrogate-pair policy, CSS 1-6 digit termination, and Sheets quote-doubling policy into grammar source or generated per-grammar templates. |
| Digit run / number scan | REVISE | Split digit-run classify/MAC from number grammar. A generic API may expose digit masks, digit accumulation, sign/dot/exponent parts, and failure offsets; JSON leading-zero, exponent, raw-number, float materialization, and overflow disposition must be grammar policy or generated template code. |
| Whitespace/layout skip | REVISE | Promote only `ascii_class_skip(input, offset, table)` or equivalent grammar-owned class-table API. `skip_ascii_whitespace` may remain as a generated JSON wrapper or compatibility helper, not as the generic Lock 14 abstraction. |
| Direct output/control path | ACCEPT with confinement | Admit as a generated per-grammar event/output contract with common telemetry fields. Reject JSON digest semantics, JSON event names, or direct-row admission policy as a generic gate API. |
| Root typed generalization | ACCEPT | Proceed only as grammar-neutral root/layout algebra. Reject `JsonRootSchema`, JSON root match arms, or fixture-named branches in generic code. |
| `instruments` typed admission | ACCEPT with confinement | Admit only as a JSON product-plane row generated from the JSON grammar/template and full comparator parity. It is not CSS/Sheets/BBNF-self evidence and not totality proof. |
| Comparator / telemetry refresh | ACCEPT | Common evidence schema is grammar-neutral if every field can carry CSS L4, Sheets, and BBNF-self comparator identity, strictness, freshness, output plane, and run id. Sidecar freshness remains evidence only. |
| Structural cursor / retained structural projection | REJECT | Do not shortlist. Any retained structural cursor, sidecar bitmap, class column, second source scan, or renamed W3 substrate violates Lock 14/Lock 1 before S-P3. |
| Allocation materializer and array/object dispatch hint as bbnf-simd primitives | REJECT | Do not admit as SIMD/ASM primitives. They may return only as per-grammar output/control template work or as grammar-neutral byte/mask/control primitives with scalar oracle and same-wave consumer. |

## Required Fold

1. Update P2-E candidate signatures so every public `parse-that` or
   `bbnf-simd` API is grammar-neutral by construction. Remove JSON words from
   generic signatures where they carry policy: `quote`, `slash`, `\uXXXX`,
   JSON-number, JSON whitespace.

2. Add an explicit "generic API boundary" note to P2-E: generic crates expose
   byte sets, class tables, masks, offsets, digit accumulators, and policy
   structs; generated per-grammar templates own delimiters, escape languages,
   root schemas, event names, output contracts, and row-specific digest or
   typed semantics.

3. Carry P2-F's per-candidate verdicts into P2-E/B/C naming. Candidates already
   marked "per-grammar template only" must not be exported as generic
   parse-that APIs except for their grammar-neutral kernels.

4. Preserve P2-B/P2-D rejections: no structural cursor, retained sidecar,
   parser-owned class table, decoded scratch materializer, or array/object
   dispatch hint enters the S-P3 candidate pool as a generic primitive.

## Result

REVISE. The research packet is close: no candidate requires outright packet
rejection, and P2-F identifies the right Lock 14 boundaries. S-P3 may not
shortlist the P2-E generic API surfaces until the JSON policy is either removed
from the generic signature or confined to generated per-grammar templates.
