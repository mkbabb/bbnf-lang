MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 POST3 custody/build author.

# R7 custody POST3 census and native-build receipt

- Date: 2026-07-22.
- Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
- Git HEAD during census, build, and post-build verification:
  `af15f63e0d2d3d719938c13b906a50acbb92ea3b`.
- Authority: only the POST3 tuple, build, and binary identified below are current.
  Every earlier in-turn census/build was superseded after an intentional source
  change and is explicitly excluded from authority.
- Scope: during this POST3 census/build interval, this author changed no source,
  generated product, runtime product, package, manifest, or lockfile byte. The
  only durable write by this author is this receipt. Census and build artifacts
  remain in isolated `/tmp` roots and are content-addressed below.

## Agent and runtime identity

- Agent/model runtime, self-declared: Codex agent, model `gpt-5.6-sol`, reasoning
  effort `ultra`. The service exposed no signed model/runtime attestation, so
  these labels are metadata, not independently verified evidence.
- Evidence-backed host identity: native `uname -m` = `arm64`; forced
  `/usr/bin/arch -arm64 /usr/bin/uname -m` = `arm64`.
- `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`, full commit
  `02c7f9bec0fd583160f8bcccb830216023b07bee`, host
  `aarch64-apple-darwin`, LLVM `22.1.2`.
- `cargo 1.96.0-nightly (eb94155a9 2026-04-09)`, full commit
  `eb94155a9a60943bd7b1cb04abec42f5d0de6ddc`, host
  `aarch64-apple-darwin`; Cargo reported `Mac OS 26.4.1 [64-bit]`.
- Resolved rustc binary:
  `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/bin/rustc`,
  SHA-256 `95d199b3cc0a2e43a904e3ed6120738ce768b7300214023a56d60cbeacf76d36`,
  Mach-O 64-bit arm64, `lipo -archs` = `arm64`.
- Resolved cargo binary:
  `/Users/mkbabb/.rustup/toolchains/nightly-2026-04-11-aarch64-apple-darwin/bin/cargo`,
  SHA-256 `c6170b6af6c6205953bff3119c3c003744f5b85e9f76489d12e3e45094f1b296`,
  Mach-O 64-bit arm64, `lipo -archs` = `arm64`.
- The complete toolchain, host, binary, and post-build rehash output is retained
  at `/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV/POSTBUILD-VERIFY.raw.log`,
  SHA-256 `4daea0d843fb141ae24aebff55d05c7aff01026a9c0e3190cfe77146b989580b`
  (51 lines / 2,670 bytes); its status is `0`, stored as a 2-byte file with
  SHA-256 `9a271f2a916b0b6ee6cecb2426f0b3206ef074578be55d9bc94f6f3fe3ab86aa`.

## POST3 census

- Census algorithm: `census.py`, SHA-256
  `1fa707954622584ceaaadd787d61c6c603bf2cec5101a9cad2d10e0b0251da12`.
- Invocation used `PYTHONDONTWRITEBYTECODE=1`; the census root contains no
  `__pycache__` directory or `.pyc` file.
- Exact invocation:

```text
PYTHONDONTWRITEBYTECODE=1 python3 restart/skinny/tranches/sk-v25/waves/BB-C/evidence/W0-AMBIENT-R7-CUSTODY/census.py --workspace /Users/mkbabb/Programming/bbnf-lang --sealed-fleet /tmp/bbcw0-r6-output-fleet.TwgDag --candidate /tmp/bbcw0-r7-atomic-candidate.CrYkNb --out /tmp/bbcw0-r7-post3-docfreeze-census.rxushS/out
```

- Status: `0`. Raw census log: empty, SHA-256
  `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`.
- `CENSUS.json`: SHA-256
  `7ece5f077e3329972b610e1d368cd234cc965db5476cd8cebc77119493f1ec98`
  (41 lines / 2,135 bytes).

| Measure | PRE | authoritative POST3 |
|---|---:|---:|
| members | 843 | 843 |
| bytes | 15,312,061 | 15,310,061 |
| path-set SHA-256 | `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` | `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` |
| ledger SHA-256 | `808580eb49b56531854ef92a101c4f18a611856872c124d3430e295cb9fd714c` | `3e37888bc13a01fb305f0017755357ff23135437903130c3271d9d19e9a8e10d` |

The POST3 ledger is
`/tmp/bbcw0-r7-post3-docfreeze-census.rxushS/out/BUILD-INPUTS.tsv`, SHA-256
`3e37888bc13a01fb305f0017755357ff23135437903130c3271d9d19e9a8e10d`
(844 lines / 105,390 bytes). Its path list is SHA-256
`9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba`
(843 lines / 42,081 bytes).

### Exact PRE to POST3 delta

Exactly zero members were added, zero removed, and six changed. Modes remained
`0644`:

```text
bbnf-lang/crates/core/src/path/variant_select.rs
PRE   0644  3608   2d42469ec7bad8e4ad2a52bf41909a7d4c3859c9e9f88dfc11e2f8ba295a2708
POST3 0644  3525   9134071281e1751b82a7cf0e24c8d02f82362f5b7f003fa3a5a77dc905f6d227

bbnf-lang/crates/ir/src/passes/prefix.rs
PRE   0644  19142  9241b8cd960452c959cf9652d969f9074a379e0585600fd84ecdde4d72b8de08
POST3 0644  18234  ca4603be2d71abab3f31b566fcb56ea72be37c3cbd47dbda1317ad1326bb151a

bbnf-lang/crates/ir/src/passes/types/registry.rs
PRE   0644  19939  baf448dbfe941c584343080f391b0d9cb2a6708db3725313624b183d34e32312
POST3 0644  19061  5cefa20606c50ea86d2b42d4588b10716b5a628bbb9533daa2a2358c7729db5d

bbnf-lang/crates/ir/src/registry/struct.rs
PRE   0644  16174  cad61c3d551265e2e79a6408440e21905e8224eee2e692553249e9451ffba709
POST3 0644  16176  fdf2aa6b1043dfec92e1ad042dd2540c1727f124b7affa225bc0be61fbd4f459

bbnf-lang/xtask/src/architecture/evidence/binding/apple.rs
PRE   0644  19936  ce3ae9af7d93e6066fad2ff7e0ea404405bdff29df64bd803f82fe095067fc5f
POST3 0644  19855  54827145fe52f1cb0391a802031b3a7e137911fb4eab1b5bbfa304ca1c0ad0eb

bbnf-lang/xtask/src/regen_simple_runtime.rs
PRE   0644  124350 82a0e360b64bcd762dd90e584f32f95911bb6fa05305168ee104f257a3621a8f
POST3 0644  124298 06f8da21f333693357bc0cd323dc22a0d011ec3dcc85c5f70e09830fb777744e
```

The published algorithm excludes test trees. The two relevant external tests
were therefore not silently counted as build inputs; their separately rehashed
identities after the build were:

- `crates/ir/tests/passes/passes_prefix.rs`: 20,935 bytes, SHA-256
  `6ab6691e877c545049e6ec91ebbc4bd70f489ad0b2ca67cb1f5beca677e21973`.
- `crates/ir/tests/struct_registry.rs`: 18,209 bytes, SHA-256
  `c5cbde12635ee3edeb03f0874b4d20c3f79aeb89bd942541d338a5389ff4805d`.

## Fresh native ARM build

- Build root: `/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV`, mode `0700`.
- Before invocation, both `target/` and `tmp/` contained zero members.
- The ambient environment contained none of `CARGO_TARGET_DIR`,
  `CARGO_BUILD_TARGET`, `CARGO_INCREMENTAL`, `RUSTC_WRAPPER`,
  `RUSTC_WORKSPACE_WRAPPER`, `CARGO_BUILD_RUSTC_WRAPPER`, `RUSTFLAGS`,
  `CARGO_ENCODED_RUSTFLAGS`, `SCCACHE`, or `RUSTUP_TOOLCHAIN`.
- The invocation set only the isolated target/temp paths and one build job; the
  resulting raw log contains 95 `Compiling` lines and zero `Fresh` lines.
- Exact inner build command:

```text
/usr/bin/arch -arm64 env CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV/target TMPDIR=/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV/tmp cargo build --target aarch64-apple-darwin -p xtask --profile ax-iter --features grammar-regen --locked
```

| Capture | Exact identity |
|---|---|
| merged raw cargo log | `/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV/cargo-build.raw.log`; SHA-256 `d80045f8d1e2af1e7d2c5b4231c91c21da8242bcce2346bbf0ec7e9f75b32751`; 1,209 lines / 45,355 bytes |
| exit status | `0`; 2-byte status file SHA-256 `9a271f2a916b0b6ee6cecb2426f0b3206ef074578be55d9bc94f6f3fe3ab86aa` |
| `/usr/bin/time -p` | `real 81.28`, `user 167.31`, `sys 7.60`; 32-byte time file SHA-256 `a6f1847a149105cd4947011bd9e8f714fa5e28c00daa047892e62de66b9debd1` |

The build finished successfully in the requested `ax-iter` profile. Warnings
are preserved verbatim in the raw log; they were not suppressed or rewritten.

## Resulting binary and post-build stability

- Binary:
  `/tmp/bbcw0-r7-post3-docfreeze-build.cr1FlV/target/aarch64-apple-darwin/ax-iter/xtask`.
- Mode/size: `0755` / 45,193,672 bytes.
- SHA-256: `089161cc74303b7e0f615d2a182f063696b2fc7624f9a73d7d25f69fcd0c88ef`.
- `file`: `Mach-O 64-bit executable arm64`.
- `lipo -archs`: exactly `arm64`.
- `otool -hv`: `MH_MAGIC_64`, `ARM64`, `EXECUTE`.

The binary was inspected only as data and was never executed. After the build,
all 843 census members were re-read from the live filesystem and checked for
mode, byte length, and SHA-256 against the POST3 ledger: 843 checked, zero
mismatches. The two census-excluded test files also retained the identities
listed above. Thus neither the build nor verification mutated a build input,
generated product, or runtime product.

No generator, test binary, profiler, mutant, x86 process, or produced `xtask`
binary was executed by this author. This receipt establishes exact custody and
native buildability only; it does not grant semantic, generator, runtime-
projection, or tranche acceptance.

## Superseded in-turn snapshots

Concurrent, intentional source changes landed while custody work was running.
The following completed ledger hashes are retained only as a motion audit and
must not be used as authority: `c05d7dc73ed8ad16d080e5d7e8491e5ad31b2b9a5546c65ee50b6515f7e42d98`,
`d0b1fd4af6208b61adae73f3c87852d753c3cdb14dcda169fbbdde9b5cc7d4fd`,
`521ab8ea0a7cdaedea7d8ea0c56719097bbb544e922f41851f12340915a5d503`,
and `7ca30c96e0d7ccf086667f97ebe466c11158dce1bf48786263277e1503bf6e27`.
Their associated builds were aborted or superseded as soon as source motion was
reported or detected. The final post-build rehash is what distinguishes the
authoritative `3e37888b…` snapshot from those provisional snapshots.
