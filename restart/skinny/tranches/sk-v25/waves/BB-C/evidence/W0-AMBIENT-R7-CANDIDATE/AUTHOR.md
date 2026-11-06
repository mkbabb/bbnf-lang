MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 atomic candidate assembler.

# BB-C.W0 R7 atomic candidate assembly receipt

- Date: 2026-07-22.
- Binding custody receipt: `W0-AMBIENT-R7-CUSTODY-AUTHOR.md`, SHA-256
  `1523d98282b82feaef9687210f3c023abff1e7e06d7d4194116e6933a6a80bc8`.
- Corrected POST operand: 843 members / 15,311,980 bytes / path-set
  `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba`
  / ledger
  `e006e3ea3c3a3f273b62cf75cfdfb0f68872fdadb10d2d16be6a5324cfb79922`.
- Sealed fleet operand: 18 members / 9,082,868 bytes / ledger
  `089bc000aaca7120190fa0724e0e080379adacb1bca8e9db86d1a5db919ff227`.
- Result: **RED** at the first failing runtime regenerator. No retry.

## Candidate construction

`mktemp -d` created this Programming-shaped root:

```text
/private/tmp/bbcw0-r7-programming-candidate.dyTgSs
```

For every corrected POST row, the source at
`/Users/mkbabb/Programming/<normalized-path>` was checked for regular-file
status, mode, bytes, and SHA-256 before copy. `copy2` populated the exact
`bbnf-lang/`, `parse-that/`, and `pprint/` shapes and preserved modes. Every
copied file was then independently re-read from the candidate. All 843 rows
and 15,311,980 bytes matched; no mismatch was tolerated.

The 18 custody fleet files were first copied and verified in a staging
directory beside candidate `generated/`, then installed with per-file atomic
`os.replace`. `mod.rs` was neither copied nor deleted and remained exactly:

```text
0644  1,414  37f64b07a8a6b6681c26e6dd46ab0384b0436363a9ef36b82f17747a00fd7a96
```

All 18 installed files still match `FLEET-INSTALL.tsv`, SHA-256
`089bc000aaca7120190fa0724e0e080379adacb1bca8e9db86d1a5db919ff227`,
after the runtime attempts. Nothing was installed into the real worktree.

## Native binary

The supplied binary independently verifies as:

```text
/tmp/bbcw0-r7-native-nooverlay.wogEcp/target/aarch64-apple-darwin/ax-iter/xtask
0755  45,180,344
aba131bd616ff0a70c7d288af2b0b204b44a4d4606140b12ab0b64bc7ce03dc2
Mach-O 64-bit executable arm64; lipo -archs = arm64
```

Every attempted command used `/usr/bin/arch -arm64` from the candidate
`bbnf-lang` cwd. Before the first command, all four candidate runtime output
trees exactly matched the sealed pre-snapshot.

## One-shot command results

| # | Command | Status | Wall | stdout | stderr |
|---:|---|---:|---:|---|---|
| 1 | `regen-json` | 0 | 507,656,500 ns | empty | empty |
| 2 | `regen-css-pretty` | 0 | 33,973,083 ns | empty | empty |
| 3 | `regen-google-sheets` | **1** | 29,936,250 ns | empty | 169 bytes, `84c1ca19…` |
| 4 | `regen-css` | **UNRUN** | — | — | stopped after first failure |

The exact failure is:

```text
Error: parse generated `/private/tmp/bbcw0-r7-programming-candidate.dyTgSs/bbnf-lang/crates/core/src/runtime/google_sheets/arena.rs`

Caused by:
    expected identifier
```

The three attempted commands each ran once. There was no retry, and the
fourth command was not started. Raw stdout, stderr, status, Unix timestamps,
and monotonic wall duration are retained under `logs/`; `COMMANDS.json`
records the terminal sequence.

## Exact runtime-output census

The census covers all 32 files in the four implicated runtime output trees.

| State | Members | Bytes | Ledger SHA-256 |
|---|---:|---:|---|
| sealed PRE | 32 | 135,527 | `23b493750d0ee63d0ddda787d13800fd999ccf1e8fa4e7375ca46b200c05a9ec` |
| stopped candidate | 32 | 135,782 | `1728f60b1a7c7b060f2266dd8e23a350653f33842e7b8f96fc66996ba7037d52` |

There are no additions or removals and exactly eight changed files:

| File | PRE bytes / SHA-256 | Candidate bytes / SHA-256 |
|---|---|---|
| `json/arena.rs` | 2,589 / `df7c50fa…` | 2,509 / `b301c412…` |
| `json/builder.rs` | 8,318 / `e633e468…` | 8,367 / `b6640cb9…` |
| `json/document.rs` | 5,430 / `94142b66…` | 5,441 / `2ba48fae…` |
| `json/parse_with.rs` | 2,756 / `8e445097…` | 2,908 / `2f0ff34c…` |
| `css_pretty/builder.rs` | 1,686 / `29ad050e…` | 1,751 / `43835724…` |
| `css_pretty/document.rs` | 4,625 / `734bfc29…` | 4,676 / `8f47fbfa…` |
| `css_pretty/kind.rs` | 1,556 / `73d74cc3…` | 1,556 / `f4f1d71f…` |
| `css_pretty/view.rs` | 1,700 / `c3ad11cd…` | 1,707 / `004549f1…` |

Google Sheets has zero runtime-output changes after its render/parse failure.
CSS L4 has zero changes because its command was correctly not run. Exact full
identities are in `RUNTIME-DELTA.json` (`23dc3325…`), `RUNTIME-PRE.tsv`, and
`RUNTIME-POST.tsv`.

## Disposition

The atomic candidate remains at the exact temp root for inspection, but it is
not accepted and must not be copied into checked source. The failure is a
runtime-projection generator defect or incompatibility requiring a new
bounded author/critic/adjudicator slice. This receipt assigns no root cause.

No new generator was built. No semantic test, profile, mutant, package,
lockfile, commit, Value, parse-that, or real-worktree mutation occurred. R7
must remain RED; `regen-css` may not be backfilled under this no-retry epoch.
