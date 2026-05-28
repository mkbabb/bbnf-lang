# SK-V15 S-P2 V2 CH7 - OVERFIT-PRUNE / GATE-EXCLUSION

Verdict: ACCEPT.

## Scope

CH7 applies `NEW-CH7-V5-03`: Lock 14 / Lock 16 gates must scan and report their own exclusion lists, and the research packet must not retain overfit primitives behind diagnostic wording (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:100`-`110`).

Inputs checked:

- `restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v15/research/p2/hardening/V1/CH1.md` through `CH6.md`

## ACCEPT Findings

1. Gate-exclusion risk is explicit rather than hidden. P2-F makes SK-V15 aarch64-only, treats x86 as diagnostic pressure rather than an implementation path, and warns that CSS L4 cannot prove admission until its provider and comparator plane are repaired (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:20`-`22`, `:117`-`119`). That prevents S-P2 from using excluded platform or contrived CSS rows as silent gate bypasses.

2. Rejected diagnostic rows remain visible. P2-F lists `raw_number_span_classify`, A64 UDOT digit4, PTG digit run, `EOB_PAD_CLAMP`, PMULL hot-body promotion, CSSC bulk emit, retained string/block replay, retained cursor streams, schema-shaped builders, harness hashes, and x86 routes in the REJECT set with reasons (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:104`-`115`). This is a reported exclusion table, not an omitted scan root.

3. Same-substrate and no-sidecar constraints are spelled out at candidate level. P2-D rejects retained structural-position vectors, streaming cursors, class columns, whitespace bitmaps, density/projection tables, decoded-byte sidecars, and public `UnionTape` shapes as non-candidates (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:50`-`58`). Its capacity candidate explicitly forbids a second source scan, pre-scan capacity oracle, retained capacity sidecar, or parallel source pass (`restart/skinny/tranches/sk-v15/research/p2/p2d-substrate-tape.md:36`-`41`).

4. Survivor gates carry same-wave and rollback pressure instead of hidden exclusions. P2-F's CH4 fold requires scalar references, parity gates, same-wave consumers, LOC budgets, risk class, and wave alignment for each non-REJECT survivor (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:71`-`81`). It also states generic crates cannot branch on grammar names for byte-set/classifier witnesses (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:83`-`91`).

5. Comparator and host exclusions are not converted into admission. P2-A records mutable upstream heads while pinning asmjson source URLs to a commit, marks asmjson strictness and x86 AVX-512 as diagnostic, and says numeric comparator and harness-hash rows are negative antecedents (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:76`-`80`, `:101`-`110`). P2-C uses the host probe only to establish Apple M5 Max feature availability, not to admit UDOT/CSSC/PMULL routes (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:14`, `:31`-`:46`).

## Orphan V1 Disposition Check

V1 CH7 was not present as a separate file because SK-V15 added the gate-exclusion addendum after the six-lens pass prompt shape. The V1 issues that motivate CH7 are covered in this V2 audit:

- self-exempting gates: accepted as explicitly exposed and routed to Lock 14 / Lock 16 gate repair, not silently excused;
- overfit JSON-only primitives: accepted only where P2-F expresses them as grammar-neutral byte-set, classifier, tape, or generated-template surfaces;
- diagnostic rows: accepted as quarantined when they lack a current P1 hot-leaf antecedent or reopen REDRESS routes.

## Residual Watch Points

S-P3 must preserve the reported-exclusion discipline when it converts these candidates into waves. A wave that implements a Lock 14 or Lock 16 grep gate must include its own scan-root and exclusion report; otherwise NEW-CH7-V5-03 reopens and the S-P3 CHALLENGE must REVISE or REJECT the plan.
