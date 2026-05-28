# SK-V15 S-P2 Hardening V2 CH1 - Correctness

Pass: S-P2 Research hardening. Cycle: V2. Lens: CH1 CORRECTNESS.
All path:line citations are relative to `/Users/mkbabb/Programming/bbnf-lang`.

## Verdict

ACCEPT.

The V1 CH1 issues are fully folded. The folded packet was committed as
`e939467b3 docs(sk-v15-s-p2): fold V1 hardening into primitive research
packet`; the S-P2 files are clean in git except for this new V2 hardening
file. No CH1 REVISE or REJECT finding remains. The REVISE and REJECT lists
below are candidate dispositions preserved by the packet, not open CH1
defects.

CH1 acceptance rests on four checks:

- Candidate primitive lineage now resolves to P1-E's binding antecedent surface
  or is explicitly rejected/support-only. P1-E defines the allowed surface as
  grammar-neutral scanner, tape/allocation, unicode/string, memory, and
  direct-parser cursor boundaries, while generated wrappers, schema products,
  comparator frames, checksum paths, and sidecar drift are blocked or
  diagnostic (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31`-`33`).
- P2-F now gives a verdict for every P2-B/C/D/E candidate and folds every P2-A
  row as an alias, revise surface, or reject (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:26`-`69`).
- Comparator/source claims use pinned commits, versioned local cargo sources, or
  observed source heads; external HTTP citations in P2-A/B/C resolved with 200
  status during this audit (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:103`-`120`,
  `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:83`-`88`).
- ISA and host claims cite Arm sources and the committed Apple M5 Max/aarch64
  probe (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:14`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:76`-`82`,
  `restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt:1`-`23`).

## ACCEPT List

- Byte-set/classifier family: P2-A `byte_class_mask_64` and
  `skip_byte_set_run`; P2-B `BYTE_CLASS_FROM_TABLE_64`,
  `BYTE_CLASS_FROM_EQ_SET_64`; P2-C TBL4/movemask; P2-E
  `classify_local_block_64`, `bounded_plain_literal_span`, and
  `skip_byte_set_run`. These map to scanner/movemask/string rows and are
  accepted only as grammar-owned local classifiers (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:48`-`50`,
  `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:47`-`48`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:28`-`29`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:31`-`55`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:57`-`119`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:28`-`29`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:40`-`41`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:52`-`54`).
- UTF-8 validation: accepted as encoding/string validation with run-level parity
  still required before product routing (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:53`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:123`-`152`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:34`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:55`).
- Local bitmap/tape algebra: `BITMAP_PREFIX_XOR_64` and
  `BITMAP_NEXT_SET_BIT`/`BULK_EMIT_POSITIONS_64` are accepted only as
  scalar/local operations; PMULL and CSSC promotions remain rejected
  (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:49`-`50`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:30`-`31`).
- Same-tape operations: `offset_tape_capacity_policy_v2`,
  `sparse_flag_same_tape_access`, `same_tape_fact_projection`, and
  `mask_to_tape_writer_local` are accepted only against the existing tape,
  direct sink, or admitted fact output (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:38`-`41`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:48`-`51`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:81`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:91`).

## REVISE List

- P2-A `scan_string_event_64` is not admitted as a standalone primitive; it is
  split into parameterized string/literal scanning plus string-policy helper
  work (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:50`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:65`).
- Escape/unescape surfaces remain per-grammar template/host-function work:
  `decode_escape_run`, `escape_mask_64`, `unescape_uxxxx_x4`,
  A64 unicode batch, and `escaped_literal_segments` (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:51`,
  `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:52`,
  `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:54`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:32`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:187`-`221`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:33`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:35`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:44`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:57`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:97`-`102`).
- Direct cursor/FIRST-set and local container-count surfaces remain generated
  template work, not parser-owned retained cursor state
  (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:53`,
  `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:56`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:37`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:68`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:80`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:100`).
- A64 wide-shift fold remains an implementation detail under a concrete
  classifier/digit/escape primitive, not an independent semantic primitive
  (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:30`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:42`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:99`).
- P2-B tape/allocation materialization pressure is revised into same-tape cost
  model/tape operations and rejected as standalone SIMD primitive credit
  (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:57`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:38`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:101`).

## REJECT List

- Numeric/digit surfaces are rejected for this S-P2 cycle:
  `raw_number_span_classify`, A64 `UDOT` digit4, and
  `digit_run_span_accumulate`. The folded packet now records that `mesh` and
  decimal rows are schema/comparator diagnostics, not surviving BBNF-side
  numeric hot leaves (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:52`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154`-`183`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:43`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:56`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:108`;
  antecedent proof: `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv:45`-`48`).
- `EOB_PAD_CLAMP` is rejected as an S-P2 implementation candidate and retained
  only as existing support inventory (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:51`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:109`).
- PMULL prefix-xor and CSSC CTZ bulk emission are rejected as hot-body/consumer
  promotions under REDRESS 88/89, despite host feature presence
  (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:33`-`34`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:70`-`71`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:45`-`46`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:110`-`111`).
- Retained tiny/string replay, retained structural/cursor/class streams,
  schema-shaped generated product builders, harness hashes, and x86 diagnostic
  routes are rejected as candidate primitives (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:58`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:35`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:47`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:112`-`115`).

## Evidence Checked

- Governing CH1 standard: `restart/prompts/ORCHESTRATOR.md:81`-`88` and
  convergence/fold protocol `restart/prompts/ORCHESTRATOR.md:104`-`121`;
  S-P2 CH1 specialization `restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`100`.
- SK-V15 procedural addenda: `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98`-`110`.
- Current P2 packet: P2-A through P2-F plus
  `restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt`.
- S-P1 antecedent evidence: `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md`,
  `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv`,
  `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md`, and
  `restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md`.
- V1 hardening: `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md`
  and `restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`.
- Fold state: git commit `e939467b3`; this audit writes only CH1 and does not
  rely on or alter other V2 agent outputs.

## Orphan V1 Disposition Check

- V1 CH1-F1 numeric/digit reject is folded: P2-A, P2-C, P2-E, and P2-F now
  reject numeric/digit candidate status (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:19`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:52`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:169`-`183`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:108`).
- V1 CH1-F2 `EOB_PAD_CLAMP` reject is folded as support-only inventory
  (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:20`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:51`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:109`).
- V1 CH1-F3 escape/unescape revise is folded as explicit REVISE template
  disposition with no standalone primitive credit (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:21`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:33`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:35`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:44`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:57`,
  `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:97`-`102`).
- V1 CH1-F4 simdjson strictness plane is folded: P2-A now states simdjson is a
  strict JSON comparator/source family only in validating parser modes
  (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:22`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:36`).
- V1 CH1-F5 mutable comparator citations are folded: observed heads are recorded
  and the used asmjson/simdjson/yyjson URLs are SHA-pinned
  (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:23`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:103`-`120`).
- V1 CH1-F6 host-feature evidence is folded through the committed sysctl
  artifact (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:24`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:14`,
  `restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt:1`-`23`).
- V1 CH1-F7 schema heading issue is folded: P2-C now uses the S-P2 section
  wording for findings and candidate primitives (`restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md:25`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:10`,
  `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:24`).
- Consolidated V1 open fold directives are not orphaned: P2-A alias fold exists,
  CH4 cost fields exist, and the capacity policy no longer permits a second
  source scan (`restart/skinny/tranches/sk-v15/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:21`-`33`;
  folded at `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:59`-`81`,
  `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:38`).
