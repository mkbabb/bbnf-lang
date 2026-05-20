# SK-V11 S-P2 V2 CH2 Generality

Pass: S-P2 CHALLENGE. Cycle: V2.
Date: 2026-05-19.
Scope: Lock 14 grammar-neutrality review of S-P2 V2 candidates.

Disposition: ACCEPT.

## Findings

1. ACCEPT. Every P2-B/C/D/E candidate now carries a P2-F grammar-neutral
   family verdict or an explicit non-parser status. P2-F folds P2-B's
   `HEX_QUARTET_X4_PROOF`, string-block, TBL/classifier, digit, whitespace,
   movemask, dispatch, and digest rows; P2-C's aarch64 inventory; P2-D's D1-D5;
   and P2-E's retained parse-that gaps into C1-C9
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:12`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:49-59`).
   The P2-F scope matches the PASS-2 contract for P2-B/C/D/E candidates
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:46-53`). P2-A's comparator
   packets also carry their own generic/per-grammar/forbidden-coupling table
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:214-223`).

2. ACCEPT. The V1 Unicode overfit is materially fixed. The old JSON production
   route is renamed and narrowed to `HEX_QUARTET_X4_PROOF`; it is proof-only,
   owns no production row, and requires a new source delta plus scalar x4 oracle
   before any caller claim (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:263`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:276`).
   P2-C keeps the x4 path proof-gated only and leaves surrogate/output policy to
   the caller (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:52-60`).
   P2-E admits only an escaped-segment API with generated/host policy for JSON
   `uXXXX`, CSS variable-width escapes, Sheets doubled quotes, and BBNF literal
   policy (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:76-82`).
   P2-F's C3 verdict is correctly mixed: CSS hex is a host-function surface,
   Sheets rejects the hex/unicode route, and BBNF-self is a per-grammar template
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:69`).

3. ACCEPT. The byte-set/classifier, layout skip, and generated dispatch surfaces
   are no longer collapsed into one JSON-shaped primitive. P2-F separates C1
   masking, C5 byte-set layout skip, and C6 FIRST/prefix/lookahead dispatch
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:34`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:38-39`);
   its proof table marks C1 as grammar-neutral, C5 as byte-set plus
   per-grammar trivia policy, and C6 as per-grammar dispatch template
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:67`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:71-72`).
   The cited grammars support those distinctions: CSS has comment-aware `@ws`
   that cannot be generic byte skip (`grammar/css/l4/stylesheet.bbnf:5-12`),
   Sheets has `?w` around expression/operator sites (`grammar/google-sheets/google-sheets.bbnf:103-161`),
   and BBNF-self has comment grammar items distinct from generic whitespace
   bytes (`grammar/bbnf/bbnf.bbnf:17-18`, `grammar/bbnf/bbnf.bbnf:83-85`).

4. ACCEPT. `output_digest_hash` and tape/direct shape no longer pollute parser
   vocabulary. P2-F states C8 is benchmark oracle or per-product host sink only,
   and C9 is Lock 1/output-plane accounting only
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:30`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:46-47`).
   The per-grammar verdict table keeps C8 as `benchmark oracle only` and C9 as
   `accounting only` across CSS, Sheets, and BBNF-self
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:74-75`).
   P2-B and P2-E agree that digest/hash is verification or host/output behavior,
   not generic parser semantics (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:270`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:29`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:92`).

5. ACCEPT with carry-forward gate. V2 correctly identifies the live Lock 14 risk
   in current codegen instead of papering it over: `emit_with_layout` and
   `emit_typed_with_layout` still route through `json_provider`
   (`skinny/crates/codegen/src/lib.rs:102-136`,
   `skinny/crates/codegen/src/lib.rs:139-167`). P2-F records this as a
   mandatory S-P3 condition: any plan using the candidate pool must replace the
   JSON-provider emission path with one grammar-agnostic generated-runtime
   template before claiming CSS/Sheets/BBNF-self generality
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:22`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:104`).
   This satisfies Lock 14 at the research-pass level because S-P2 is not
   claiming current generic-crate generality; it is naming the required
   implementation gate under the rule that JSON policy never enters generic
   crates (`restart/locks/LOCKS.md:78`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:232-236`).

## Required Folds

None for CH2. V2 resolves the V1 CH2 required folds:

- fixed `uXXXX` production became neutral proof/hex-run plus generated policy;
- byte-set masking, FIRST/prefix dispatch, and layout trivia are split;
- output digest/hash is not a generic parser primitive;
- P2-F uses the required verdict vocabulary and non-JSON proof surfaces;
- current JSON-provider codegen is carried as an S-P3 blocker until replaced by
  a grammar-agnostic generated-runtime template.

## Accepted Facts

S-P3 may consume C1-C7 as grammar-neutral or per-grammar-template parser
surfaces only under the P2-F non-JSON generated benchmark gate
(`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:77-82`).
C8 is benchmark oracle or per-product host sink only. C9 is output-plane
accounting only. Any implementation wave that puts JSON string, number,
surrogate, object/array, layout, digest, or grammar-name policy into
`bbnf-simd`, `parse-that-regex`, `codegen`, `runtime`, or another generic crate
fails CH2 under Lock 14.
