MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 atomic fleet candidate author.

# BB-C.W0 R7 POST3 atomic fleet author receipt

- Date: 2026-07-22.
- Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
- Result: **RED** at the mandatory Google Sheets semantic gate after a green
  fleet regeneration and fixed-point check. The four runtime projections were
  not started.
- Scope: all generator execution occurred in a fresh, Programming-shaped
  candidate under `/private/tmp`. The real worktree received no source,
  generated-product, runtime-product, package, manifest, lockfile, or test
  mutation from this author. The only durable writes are this receipt and its
  raw evidence siblings in this directory.

## Agent identity and authority

- Self-declared assignment: Codex agent, model `gpt-5.6-sol`, reasoning effort
  `ultra`, R7 atomic fleet candidate author.
- No signed or independently recorded model/runtime attestation was supplied.
  The model and runtime labels above are therefore **UNATTESTED metadata**, not
  measured proof.
- Binding custody operand:
  `../W0-AMBIENT-R7-CUSTODY/POST3-AUTHOR.md`, SHA-256
  `96ca0898164cf19d5a91bfa2e60ff62b673ceb9a7693cddcbdb73c7f482fec6a`
  (173 lines / 9,250 bytes).
- Binding POST3 build-input tuple: 843 members / 15,310,061 bytes / path-set
  SHA-256
  `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba`
  / ledger SHA-256
  `3e37888bc13a01fb305f0017755357ff23135437903130c3271d9d19e9a8e10d`.
- Binding POST3 ledger:
  `/tmp/bbcw0-r7-post3-docfreeze-census.rxushS/out/BUILD-INPUTS.tsv`.
- Binding executable:
  `/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV/target/aarch64-apple-darwin/ax-iter/xtask`,
  mode `0755`, 45,193,672 bytes, SHA-256
  `089161cc74303b7e0f615d2a182f063696b2fc7624f9a73d7d25f69fcd0c88ef`.
  Independent inspection identified a Mach-O 64-bit arm64 executable and
  `lipo -archs` returned exactly `arm64`.

## Isolated candidate

`mktemp -d` created the fresh Programming-shaped root:

```text
/private/tmp/bbcw0-r7-post3-fleet-candidate.ZqYYJA
```

Every POST3 ledger source row was checked and copied into the corresponding
`bbnf-lang/`, `parse-that/`, or `pprint/` path. Every candidate row was then
independently re-read and checked for mode, byte length, and SHA-256. The copy
matched all 843 rows and 15,310,061 bytes exactly. No row was omitted or
substituted, and nothing was copied back to the real worktree.

All executed commands used `/usr/bin/arch -arm64`, the binding executable
above, and candidate cwd
`/private/tmp/bbcw0-r7-post3-fleet-candidate.ZqYYJA/bbnf-lang`. No x86 binary
or process was used.

## One-shot generator sequence

Exactly two generator commands ran, once each, without retry:

| # | Command | Status | Wall | stdout | stderr |
|---:|---|---:|---:|---|---|
| 1 | `regen` | 0 | 1,683,422,583 ns | 714 bytes / `957df9e1…` | 7,994 bytes / `ff31895c…` |
| 2 | `regen --check` | 0 | 1,261,754,875 ns | 47 bytes / `b0207e9e…` | 7,835 bytes / `85f1cda5…` |

The complete identities are:

- `01-regen.json`: SHA-256
  `27c62f29f172062e65abf7fdfa4adc943a764e13db497ef232f5d91cace5fb5e`
  (19 lines / 609 bytes).
- `01-regen.stdout`: SHA-256
  `957df9e147e45ce8b149874b4976b614bfcefe4e12e05ceff75f64e9311915c3`
  (9 lines / 714 bytes).
- `01-regen.stderr`: SHA-256
  `ff31895c95bba3c1cc11502a8c38939dc4fe57be8def9db6a5f52c7f77b7e26f`
  (69 lines / 7,994 bytes).
- `02-regen-check.json`: SHA-256
  `6f62515610025c19daf4d8d7381da676e215c9589f1e047a2b845c70aae5c55d`
  (20 lines / 631 bytes).
- `02-regen-check.stdout`: SHA-256
  `b0207e9ee70b91c3f7544c8bfd1e43d8bc6214524762be6c5121dab8d13c8e5b`
  (1 line / 47 bytes):
  `regen --check: clean (9 of 9 grammars matched)`.
- `02-regen-check.stderr`: SHA-256
  `85f1cda5722e798b86048f49fb11239b1c819f05f0cad1806543ba742ab66b66`
  (69 lines / 7,835 bytes).

The 18 generated parser-plus-sidecar outputs immediately after `regen` and
immediately after `regen --check` are byte-identical. Both ledgers have SHA-256
`f4f1c45defbe2ac7c84b42e4b532f40a962c404a43771ee21960e34cf791417d`
(19 lines / 2,281 bytes). This proves the required fixed point for the exact
candidate and executable; it does not supersede the subsequent semantic RED.

## Generated fleet delta

The starting generated fleet contained 18 members / 9,450,826 bytes. The
fixed-point fleet contains 18 members / 10,375,978 bytes. There are zero added
or removed paths, exactly 12 changed artifacts, and six unchanged artifacts.

| Grammar | Registry | Parser |
|---|---|---|
| BBNF | changed: 49,354 `3e7494c6…` → 49,474 `42d66c77…` | changed: 1,062,265 `c8f5e6d3…` → 1,062,385 `f97c5149…` |
| BNF | unchanged | unchanged |
| CSS L4 | changed: 193,459 `77e0ecad…` → 243,278 `dfcf07e8…` | changed: 6,105,898 `ac08b34d…` → 7,270,497 `8eef9012…` |
| CSS Pretty | changed: 11,944 `bc50b5f3…` → 11,950 `dd9642a5…` | changed: 529,141 `e44d2f8c…` → 305,499 `1f1374a3…` |
| CSV | unchanged | unchanged |
| EBNF | changed: 28,458 `7938d084…` → 28,478 `a9096e8e…` | changed: 349,043 `eddd1aa5…` → 349,063 `290afd94…` |
| Google Sheets | changed: 27,211 `37a2ee49…` → 28,831 `6885db93…` | changed: 677,373 `345397a5…` → 653,976 `953de8d6…` |
| JSON | changed: 4,415 `8a9500a9…` → 4,407 `33229d74…` | changed: 163,767 `82e49add…` → 119,642 `36da388f…` |
| Math | unchanged | unchanged |

Full path, mode, byte, and SHA-256 identities are retained in:

- `GENERATED-PRE.tsv`: SHA-256
  `7d0a9cb144e2fd9652936ee602060ff76d6845e9e520ec6f934d8cd0d8d06e99`
  (19 lines / 2,281 bytes).
- `GENERATED-AFTER-REGEN.tsv`: SHA-256
  `f4f1c45defbe2ac7c84b42e4b532f40a962c404a43771ee21960e34cf791417d`
  (19 lines / 2,281 bytes).
- `GENERATED-AFTER-CHECK.tsv`: the same byte identity as the post-regen ledger.
- `GENERATED-DELTA.json`: SHA-256
  `54086c3c79f9e2805a5d8153f2a4189f65a071837481803eeb03894b71ecf197`
  (166 lines / 4,895 bytes).

## Mandatory pre-runtime semantic gate

The gate inspected the freshly generated Google Sheets sidecar and parser,
not the starting candidate artifacts:

- Sidecar SHA-256:
  `6885db936c88afc8e5dfc1aa7b2f88a9a9acaab2ca0d70c923060b0781b8a736`.
- Parser SHA-256:
  `953de8d6581a584548131b3f243114ed9495a51b5bf2533cf1257b0adad44747`.

Sidecar closure passed:

- `compare_op` is `U8`, has exactly six `U8` fields, has the unique logical
  names `branch_0` through `branch_5`, and contains no `Tuple([Span, U8])` or
  equivalent factored `Span` representation.
- The six branch sources retain indices 0 through 5.
- `error_literal` is `U8`, all of its fields are `U8`, and no factored `Span`
  artifact remains.

The generated direct function also passed the required mapping:

```text
<> = 0; <= = 1; >= = 2; = = 3; < = 4; > = 5
```

The generated Pratt function failed the same invariant. Its actual mapping is:

```text
<> = 0; <= = 1; >= = 2; < = 3; > = 4; = = 5
```

The required mapping is:

```text
<> = 0; <= = 1; >= = 2; = = 3; < = 4; > = 5
```

Thus Pratt assigns `<`, `>`, and `=` to 3, 4, and 5 instead of 4, 5, and 3.
`SEMANTIC-GATE.json`, SHA-256
`dd5fe66bba2e3ed4ecf7cc464922ff487991ee07cb0937e3e69e8656e8d65a33`
(45 lines / 944 bytes), records every predicate and the terminal `pass: false`.

## Runtime non-execution and unchanged inventory

The campaign stopped immediately at the semantic failure. None of the four
ordered runtime projections was started. Consequently, the runtime inventory
is byte-identical before and after the stop: 32 members / 135,527 bytes / ledger
SHA-256
`20557eba6d3db3148875c58891d7ce4be20a04cae1f08f8cc5c3ae745d6dd116`.
There are zero additions, removals, or changes.

- `RUNTIME-PRE.tsv`: 33 lines / 3,844 bytes, SHA-256 `20557eba…`.
- `RUNTIME-AFTER-STOP.tsv`: exactly byte-identical to `RUNTIME-PRE.tsv`.
- `RUNTIME-DELTA.json`: SHA-256
  `f313fac042165a9e4b8807a0c6f09239cea1bc6390b080f6b9a5268a44504d97`
  (9 lines / 142 bytes).

## Disposition

R7 remains **RED**. The green fleet generation and exact fixed point prove only
that the binding POST3 generator can produce a stable nine-grammar fleet. They
do not authorize runtime projection or landing because the fresh Google Sheets
Pratt parser violates the required branch-tag mapping.

There was no retry, repair, runtime generator, semantic test suite, profiler,
benchmark, mutant, landing, commit, x86 execution, or Value.js contact or
mutation. This receipt assigns no root cause and does not convert any candidate
byte into accepted product authority.
