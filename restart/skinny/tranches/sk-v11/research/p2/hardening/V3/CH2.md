# SK-V11 S-P2 V3 CH2 Generality

Pass: S-P2 CHALLENGE. Cycle: V3.
Date: 2026-05-20.
Scope: Lock 14 grammar-neutrality review of S-P2 V3 candidates.
Output: this file.

Disposition: ACCEPT.

## Findings

1. ACCEPT. The V3 stability fold preserves the V2 accepted candidate-status
   split without reopening the Lock 14 defects V1 exposed. P2-F explicitly keeps
   C1-C7 as the parser primitive pool, C8 as benchmark/oracle or per-product
   host sink only, C9 as Lock-1/output-plane accounting only, and the live
   `json_provider` codegen path as an S-P3 Lock 14 gate before any
   CSS/Sheets/BBNF-self generated-parser generality claim
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:10-17`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:39-56`).
   This carries forward the V2 CH2 accepted fact that every retained parser
   candidate has a grammar-neutral, per-grammar-template, host-function,
   oracle-only, or accounting-only verdict
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:19-24`,
   `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:41-56`).

2. ACCEPT. P2-F still folds all sibling candidate names into grammar-neutral
   families and uses a fixed verdict vocabulary. It maps P2-B's
   `HEX_QUARTET_X4_PROOF`, string-block, byte-class, digit, whitespace,
   movemask, dispatch, and digest rows; P2-C's AArch64 inventory; P2-D's D1-D5;
   and P2-E's parse-that gaps into C1-C9
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:21`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:58-68`).
   The verdict table states C1/C2/C4/C7 are grammar-neutral where the grammar
   surface exists, C3 may be host-function/per-grammar-template/reject depending
   on the grammar, C5/C6 are per-grammar template surfaces where needed, C8 is
   benchmark oracle only, and C9 is accounting only
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:72-84`).
   That satisfies PASS-2 CH2's requirement that JSON-shaped work be reframed or
   dropped rather than admitted as generic parser policy
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:102-107`).

3. ACCEPT. The non-JSON grammar citations support the V3 verdicts. CSS L4 has
   dispatch-heavy value alternatives and prefix-sensitive functions
   (`grammar/css/l4/values.bbnf:20-22`, `grammar/css/l4/values.bbnf:39-55`,
   `grammar/css/l4/values.bbnf:84-101`), quote/escape string surfaces
   (`grammar/css/l4/tokens.bbnf:7-9`), comment-aware layout that must stay
   generated policy rather than generic byte skip
   (`grammar/css/l4/stylesheet.bbnf:5-12`), numeric/color surfaces and a host
   hex-color function (`grammar/css/l4/color.bbnf:18-32`,
   `grammar/css/l4/color.bbnf:187-190`, `grammar/css/l4/color.bbnf:299-321`).
   Sheets has leading-dot/exponent numbers, doubled-quote strings, typed error
   and reference spans, operator dispatch, function calls, and arrays
   (`grammar/google-sheets/google-sheets.bbnf:6-18`,
   `grammar/google-sheets/google-sheets.bbnf:34-42`,
   `grammar/google-sheets/google-sheets.bbnf:52-63`,
   `grammar/google-sheets/google-sheets.bbnf:97-167`). BBNF-self has literal
   delimiters, regex literals, comments, modifiers, mapping, directives, and
   grammar items (`grammar/bbnf/bbnf.bbnf:9-18`,
   `grammar/bbnf/bbnf.bbnf:29-56`, `grammar/bbnf/bbnf.bbnf:75-85`). These are
   real grammar surfaces, not prose-only Lock 14 assertions.

4. ACCEPT. The sibling artifacts remain aligned with the P2-F Lock 14 boundary.
   P2-A carries only C1-C5 parser packets plus C8 as a non-parser output-plane
   surface, and forbids JSON-named classes, JSON string semantics, JSON
   whitespace/comment policy, JSON pair constants, primitive-owned JSON number
   validation, and parser hash facts in generic crates
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:20-30`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:226-235`).
   P2-B states x4 hex is proof-only, string/block and byte-class primitives are
   parameterized, digit and whitespace policy stay with grammar callers, movemask
   is support, dispatch cannot encode JSON object/array roles, and digest/hash is
   verification rather than parser semantics
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:268-288`).
   P2-C keeps implementation AArch64-only while requiring grammar-generated
   byte sets/tables, caller-owned number/string/escape policy, and inventory-only
   status for support rows (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:83-90`).

5. ACCEPT. P2-D and P2-E preserve valid per-grammar template/accounting status
   instead of reintroducing a JSON substrate. D1-D5 are phrased as generated
   separator/close metadata, `DirectBuild`/`SinkOnly` output-slot lowering,
   delimited span helpers, numeric token families, and existing tape sparse-fact
   encoding only (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:50-58`).
   P2-E retains only byte-set skip, bounded plain-string end, digit-run
   accumulate, and escaped-string segments; it keeps CSS comments, Sheets
   doubled quotes, JSON surrogate joining, and BBNF literal/regex policy in
   generated grammar or host code rather than generic parse-that/bbnf-simd APIs
   (`restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:21-31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2e-parse-that-gaps.md:52-84`).

6. ACCEPT with carry-forward gate. The current codegen path is still not
   Lock-14 clean for non-JSON generated parser waves: both normal and typed
   emission call `json_provider::ensure_runtime_profile`, and normal emission
   emits JSON provider files (`skinny/crates/codegen/src/lib.rs:102-136`,
   `skinny/crates/codegen/src/lib.rs:139-167`). V3 does not paper over this;
   P2-F names it as a required S-P3 replacement with one grammar-agnostic
   generated-runtime template before any CSS/Sheets/BBNF-self generality claim
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:31`,
   `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:113`).
   That is the correct research-pass disposition under Lock 14's zero
   grammar-name leak rule (`restart/locks/LOCKS.md:78`) and ORCHESTRATOR CH2's
   no-JSON-only-intervention lens (`restart/prompts/ORCHESTRATOR.md:81-88`).

## Required Folds

None for CH2.

## Accepted Facts

- S-P3 may consume C1-C7 only as grammar-neutral or per-grammar-template parser
  surfaces under the P2-F non-JSON generated benchmark gate
  (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86-91`).
- C8 is benchmark oracle or per-product host sink only; it is never generic
  parser vocabulary (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:55`,
  `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:83`).
- C9 is Lock-1/output-plane accounting only and not a parser primitive
  (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:56`,
  `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:84`).
- Any S-P3 plan that claims non-JSON generated-parser generality must first
  replace the live `json_provider` emission path with one grammar-agnostic
  generated-runtime template; otherwise CH2 fails at plan or wave challenge time
  (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:113`,
  `skinny/crates/codegen/src/lib.rs:102-167`).
