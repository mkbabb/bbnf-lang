# SK-V11 S-P2 CH2 Generality
Pass: S-P2 CHALLENGE. Cycle: V1.
Date: 2026-05-19.
Scope: Lock 14 grammar-neutrality review of S-P2 candidates.
Output: this file.
Disposition: REVISE.
Accept rate contribution: 0.

## Findings

1. Major. Every P2-B/C/D/E candidate is at least mapped by P2-F, so there is
   no orphan candidate without a P2-F verdict. P2-F folds P2-B's eight
   candidates, P2-C's six, P2-D's D1-D5, and P2-E's parse-that gaps into C1-C9
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:42-52`;
   inventories at `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:256-265`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:24-37`,
   `restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:28-34`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:31-111`).
   This satisfies the mechanical CH2 coverage question in
   `restart/prompts/skinny/PASS-2-RESEARCH.md:102-107`, but it does not by
   itself admit the candidate pool because several verdicts are "reframe" in
   substance.

2. Major. The Unicode escape candidates still carry JSON production shape in
   sibling files and must be folded before V2. P2-B names
   `ESCAPE_UXXXX_X4_PRODUCTION` as four packed 4-byte quartets with
   surrogate-pair cases (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:258`);
   P2-C names an x4 route that joins surrogate pairs under scalar JSON policy
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:32-33`);
   P2-E's segment API remains conditional on proving a CSS or BBNF-self
   consumer (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:91-107`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:125`).
   P2-F correctly says the neutral primitive is hex-nibble/hex-run decode, not
   JSON Unicode validation (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:60`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:83`).
   The proof surfaces agree: CSS has hex colors and variable-width/string escape
   surfaces (`grammar/css/l4/color.bbnf:187-190`,
   `grammar/css/l4/tokens.bbnf:7-9`), Sheets strings use doubled quotes rather
   than `\uXXXX` (`grammar/google-sheets/google-sheets.bbnf:8-12`), and BBNF
   literals/regexes are escaped spans without JSON surrogate policy
   (`grammar/bbnf/bbnf.bbnf:11-15`). REDRESS also keeps the x4 JSON caller
   proof-only unless a real source delta lands (`skinny/REDRESS.md:3174-3196`,
   `skinny/REDRESS.md:3200-3222`). Verdict: REVISE, not REJECT, because the
   hex core is grammar-neutral if the JSON policy is moved to generated
   per-grammar code.

3. Major. CSS dispatch is a real proof surface, but P2-F overstates the
   byte-set proof as first-byte dispatch. P2-F points C1/C6 at CSS value and
   color alternatives (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:58`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:63`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:70`).
   The cited grammar has collisions that a byte-set mask cannot decide alone:
   `calc` and `clamp` both begin with `c`, `min` and `max` both begin with `m`
   (`grammar/css/l4/values.bbnf:52-55`, `grammar/css/l4/values.bbnf:84-87`);
   `color`, `color-mix`, and standard color functions share long prefixes
   (`grammar/css/l4/color.bbnf:220-255`, `grammar/css/l4/color.bbnf:299-321`);
   selector pseudo classes/elements share colon prefixes
   (`grammar/css/l4/selectors.bbnf:49-84`). Therefore C1 is grammar-neutral only
   as byte-set/class-table masking, while C6 must be a generated FIRST-set /
   prefix-trie / lookahead classifier template with scalar parity against the
   grammar parser. A JSON-style structural-byte dispatch helper cannot be the
   generic proof.

4. Major. Whitespace/layout generality is only valid after separating byte-set
   skip from grammar-owned trivia. P2-E's `pt_byte_set_run_skip` names CSS L4
   whitespace/trivia as a direct consumer (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:31-48`),
   and P2-F properly narrows C5 to byte-set skip with comments left to grammar
   policy (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:62`).
   The grammar files make that split concrete: CSS stylesheet whitespace is an
   `@ws` regex that includes block comments (`grammar/css/l4/stylesheet.bbnf:5-12`);
   BBNF-self has line and block comments as grammar items (`grammar/bbnf/bbnf.bbnf:17-18`,
   `grammar/bbnf/bbnf.bbnf:83-85`); Sheets mainly uses `?w` around expression
   operators and separators (`grammar/google-sheets/google-sheets.bbnf:103-161`).
   V2 must not present comment-aware trivia as a generic byte-set primitive.
   The generic primitive is only "skip these bytes"; comment and trivia loops
   are per-grammar generated templates.

5. Major. `output_digest_hash` is not a grammar-neutral parser primitive. P2-B
   keeps it as `OUTPUT_DIGEST_HASH_PROCESS`
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:265`),
   P2-C keeps a scalar/cache candidate
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:36-37`),
   and P2-E already says digest/hash is benchmark output behavior, not
   parser vocabulary (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:111-115`).
   P2-F's C8 verdict is also a reframe: it can verify CSS, Sheets, or BBNF
   output equality but must not define parser semantics
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:65`).
   Under Lock 14, moving a JSON benchmark digest into `parse-that`,
   `bbnf-simd`, runtime, or codegen would be overfit generic-crate leakage
   (`restart/locks/LOCKS.md:78`). V2 must remove C8 from the parser primitive
   pool and keep it as a benchmark/oracle or per-product host sink only.

6. Major. The non-JSON proof surfaces are real grammar sources, but not yet a
   generated-parser proof. P2-F cites real CSS, Sheets, and BBNF grammar
   surfaces (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:18`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:56-66`);
   however it also records that current skinny codegen still emits through a
   JSON provider and that old CSS/Sheets/BBNF struct-direct modules sever the
   tape substrate (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:22-26`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:95`).
   Lock 1 and Lock 14 require generated per-grammar runtime from one
   grammar-agnostic template, with no side substrate and no grammar-specific
   generic-crate arms (`restart/locks/LOCKS.md:52`,
   `restart/locks/LOCKS.md:78`). V2 may cite the grammar files as proof
   surfaces, but S-P3 must not treat generality as proven until the shortlisted
   packet names a generated non-JSON direct/typed parser benchmark or explicitly
   scopes the candidate to a per-grammar template/host function.

## Required folds

1. Rename and rewrite the `uXXXX` candidates as a neutral
   `hex_nibble_or_hex_run_decode` core plus generated per-grammar escape
   segment templates. JSON surrogate policy stays in generated JSON code; CSS
   variable-width escapes/hex colors, Sheets doubled-quote strings, and BBNF
   literal/regex escapes must be listed separately. If no non-JSON consumer is
   named, the JSON production route is held out of generic crates.

2. Split C1 and C6. C1 remains byte-set/class-table masking. C6 becomes a
   generated FIRST-set/prefix-trie/lookahead classifier template with scalar
   parity against the grammar parser. Remove any claim that CSS declaration
   values or color alternatives are proven by first-byte dispatch alone.

3. Rewrite C5 and `pt_byte_set_run_skip` so byte-set skipping is the generic
   primitive and CSS/BBNF comment-aware trivia is generated layout policy.
   Sheets can remain the simple byte-set proof surface where `?w` is enough.

4. Move C8/`output_digest_hash` out of the parser primitive pool. It may remain
   as a benchmark oracle, strict output-equality sink, or per-product host
   function. It must not become a generic parser API or SIMD primitive without
   fresh P1 evidence that the digest update itself is the hot parser leaf.

5. Add a V2 proof-surface table for C1-C9 that marks each CSS L4, Sheets, and
   BBNF-self cell as `grammar-neutral`, `per-grammar template`, `host function`,
   `benchmark oracle only`, or `reject`. Each cell must cite an actual grammar
   line, not prose alone, and must state whether a generated non-JSON parser
   benchmark is required before S-P3 shortlist.

## Accepted facts

None for S-P3 in V1. The CH2 disposition is REVISE, so no candidate is accepted
by this lens until the required folds land in V2.
