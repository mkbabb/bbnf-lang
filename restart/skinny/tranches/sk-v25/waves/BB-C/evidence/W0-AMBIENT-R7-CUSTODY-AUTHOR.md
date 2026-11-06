MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 custody/candidate author.

# BB-C.W0 R7 custody and pre-candidate author receipt

- Date: 2026-07-22.
- Binding predecessor: `W0-AMBIENT-R6-GENERATOR-ADJUDICATION.md`,
  SHA-256
  `4ab6aea4d4449b24e14a98dbe28d20ca972446f90bebd05b29f6e7cc52ee60f1`.
- Authority: deterministic custody machinery, PRE ledgers, immutable snapshot
  copies, and one isolated atomic candidate only.
- Execution: no build, generator, test, profile, or mutant was run. No checked
  parser, sidecar, runtime projection, manifest, package, lockfile, Value, or
  parse-that byte was changed.

## Result

R7 now has a reproducible PRE ledger, a durable exact copy of the R6 sealed
fleet, a fresh exact atomic candidate, and a focused snapshot of every
currently implicated runtime-projection file. Source R6 fleet, durable copy,
and atomic candidate agree on all 18 normalized paths, modes, sizes, and
SHA-256 identities.

The R6 867-member aggregate remains **RED as reproducible custody**. R6 did
not retain its member rows or digest algorithm. R7 does not pretend that an
unreconstructable digest is authoritative; it replaces it prospectively with
schema `bb-c.w0.r7-custody.v1`.

## Custody machinery

| Artifact | SHA-256 | Lines / bytes |
|---|---|---:|
| `W0-AMBIENT-R7-CUSTODY/census.py` | `1fa707954622584ceaaadd787d61c6c603bf2cec5101a9cad2d10e0b0251da12` | 247 / 9,844 |
| `W0-AMBIENT-R7-CUSTODY/ALGORITHM.md` | `5a1532bda56864514f686c33015dd3256886138d77de9a7f939f47eafd6a09b2` | 83 / 4,021 |
| `W0-AMBIENT-R7-CUSTODY/CENSUS.json` | `ab02c198785d6eb299e2fa23d06e06de9d6160cc12325bca2ea77f3681fe45be` | 41 / 2,135 |
| `W0-AMBIENT-R7-CUSTODY/TOOLCHAIN-DERIVATION.json` | `0d5ae343514cba2de7dc4b382bfe81eec85c6e8358629c41997f76bf80e9bf8b` | 40 / 1,916 |

The algorithm rejects missing files, symlinks, non-regular members, unsafe
tab/newline path bytes, and incomplete/extra fleet members. It normalizes
paths relative to `/Users/mkbabb/Programming`, uses POSIX separators and a
mandatory `bbnf-lang/` prefix for workspace files, sorts by raw UTF-8 path
bytes, and records `path`, four-digit mode, bytes, and SHA-256 in LF-only TSV.
Path-set and complete-ledger digest construction is specified exactly in
`ALGORITHM.md`.

Inclusions are root Cargo/config/lock/toolchain inputs, each declared workspace member's
manifest/root build script/direct source tree, all BBNF/RON grammar modules,
all runtime-projection TOMLs, and the actual external path-patched Rust
package sources/manifests selected by `.cargo/config.toml`. The explicit
`pprint_derive` library outside `src/` and the parse-that Rust workspace
manifest are included. Test, bench, example, target, docs, restart, frontend,
cache, and fixture-shadow `src` trees are explicitly excluded from this
isolated build-input census.

The first v1 emission omitted pinned `rust-toolchain.toml`. Before seal, the
tool and algorithm were repaired and the exact unchanged row
`0644 / 2,512 / b9c6c4c…` was inserted into both frozen ledgers without
rereading or replacing any historical row. `TOOLCHAIN-DERIVATION.json` binds
the old and derived identities. Root `rust-toolchain` and `.cargo/config` are
absent; `rust-toolchain.toml` and `.cargo/config.toml` are the sole root
toolchain/Cargo-config inputs. The documented invocation sets
`PYTHONDONTWRITEBYTECODE=1`; the incidental pre-seal `.pyc` was deleted and no
cache byte is included.

## Frozen PRE build-input ledger

| Measure | Exact value |
|---|---|
| members | 843 |
| bytes | 15,312,061 |
| path-set SHA-256 | `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` |
| ledger SHA-256 | `808580eb49b56531854ef92a101c4f18a611856872c124d3430e295cb9fd714c` |
| ledger | `W0-AMBIENT-R7-CUSTODY/BUILD-INPUTS.tsv` (844 / 105,390) |
| path list | `W0-AMBIENT-R7-CUSTODY/BUILD-INPUTS-PATHS.txt`, SHA-256 `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` (843 / 42,081) |

The PRE row requested for the compiler/API correction is:

```text
bbnf-lang/xtask/src/architecture/evidence/binding/apple.rs	0644	19936	ce3ae9af7d93e6066fad2ff7e0ea404405bdff29df64bd803f82fe095067fc5f
```

The principal retained R6 owners are also bound inside that ledger:

| Owner | PRE SHA-256 |
|---|---|
| `Cargo.lock` | `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9` |
| Pratt `struct_direct.rs` | `bfc2dff4727d11dcf62bbb3f423cd0c0f3f52e7c888c9e15112cdf6d43f288d2` |
| lowerer `wrap.rs` | `7edbb5bca06a63304615afb4b0560f73a0bb3d6df3439b7aa8c633f6cd323f17` |
| CSP `solve.rs` | `aac939e7a68261d50dcbcf4961c638a206e71d780d95edf26dd71e1017ccf08e` |

This is the PRE boundary. Any later ordinary source correction, including a
reviewed `apple.rs` change, must produce a separate POST ledger and exact
declared delta; it must not rewrite this PRE evidence.

## Frozen POST after ordinary `apple.rs` correction

The controlling lane subsequently landed the already-reviewed compiler/API
correction as ordinary source, not as a temporary overlay. I did not author or
modify that source. I reran only the deterministic census over the same path
set and retained the PRE ledger unchanged.

| Measure | PRE | POST |
|---|---:|---:|
| members | 843 | 843 |
| bytes | 15,312,061 | 15,311,980 |
| path-set SHA-256 | `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` | same |
| ledger SHA-256 | `808580eb49b56531854ef92a101c4f18a611856872c124d3430e295cb9fd714c` | `e006e3ea3c3a3f273b62cf75cfdfb0f68872fdadb10d2d16be6a5324cfb79922` |

Memberwise comparison is exactly zero added, zero removed, and one changed:

```text
bbnf-lang/xtask/src/architecture/evidence/binding/apple.rs
PRE  0644  19936  ce3ae9af7d93e6066fad2ff7e0ea404405bdff29df64bd803f82fe095067fc5f
POST 0644  19855  54827145fe52f1cb0391a802031b3a7e137911fb4eab1b5bbfa304ca1c0ad0eb
```

POST evidence is `BUILD-INPUTS-POST.tsv`, SHA-256
`e006e3ea3c3a3f273b62cf75cfdfb0f68872fdadb10d2d16be6a5324cfb79922`
(844 / 105,390), its path file is SHA-256
`9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba`
(843 / 42,081), and `POST-CENSUS.json` is SHA-256
`c2f32d77d036858f983f5429eae42091030da93f7697ddeedc5ffd482a064959`
(30 / 1,105). This proves custody and exact scope only; it grants no compiler,
generator, or semantic acceptance.

## Sealed fleet and atomic candidate

The source operand was the still-present exact R6 directory:

```text
/private/tmp/bbcw0-r6-output-fleet.TwgDag
```

It is now retained durably at:

```text
restart/skinny/tranches/sk-v25/waves/BB-C/evidence/
  W0-AMBIENT-R7-CUSTODY/sealed-fleet/
```

The isolated atomic candidate was created with `mktemp -d`, copied from the
source fleet with modes preserved, and contains no extra file:

```text
/private/tmp/bbcw0-r7-atomic-candidate.CrYkNb
```

| Measure | Source fleet | Custody copy | Atomic candidate |
|---|---|---|---|
| members | 18 | 18 | 18 |
| bytes | 9,082,868 | 9,082,868 | 9,082,868 |
| mode | all `0644` | all `0644` | all `0644` |
| path-set SHA-256 | `b24a36930159074c94aadd44779a9c607786ab7078f7fcb749ec2104fc6b5315` | same | same |
| ledger SHA-256 | `089bc000aaca7120190fa0724e0e080379adacb1bca8e9db86d1a5db919ff227` | same | same |

The content-addressed manifests are:

- `SEALED-FLEET.tsv`, SHA-256
  `089bc000aaca7120190fa0724e0e080379adacb1bca8e9db86d1a5db919ff227`
  (19 / 1,669);
- `ATOMIC-CANDIDATE.tsv`, identical SHA-256 and size;
- `SEALED-FLEET-PATHS.txt` and `ATOMIC-CANDIDATE-PATHS.txt`, both SHA-256
  `b24a36930159074c94aadd44779a9c607786ab7078f7fcb749ec2104fc6b5315`
  (18 / 273).

The manifest reproduces the exact R6 fleet identities, including JSON
`36da388f…` / `33229d74…`, CSS L4 `d98882ad…` / `5477673e…`, CSS Pretty
`0ef4b9e3…` / `2b93434e…`, Sheets `e200eb98…` / `f916a1ee…`, and the five
exact family pairs. Nothing was copied into checked generated output.

## Runtime-projection custody

The focused runtime-projection cone has 44 files / 601,237 bytes:

| Artifact | Exact identity |
|---|---|
| `RUNTIME-PROJECTION.tsv` | `fc5f43e678bafeb42065fc18d94cd69ab387f72a693c2d4d4a92fdd25b9bb0b2` (45 / 5,673) |
| `RUNTIME-PROJECTION-PATHS.txt` | `51d873446e7bab0ee16f35689c0554d6c6d12451a40f7724681cba2fbd146a96` (44 / 2,346) |
| path-set SHA-256 | `51d873446e7bab0ee16f35689c0554d6c6d12451a40f7724681cba2fbd146a96` |
| ledger SHA-256 | `fc5f43e678bafeb42065fc18d94cd69ab387f72a693c2d4d4a92fdd25b9bb0b2` |

The set contains the four checked sidecars; four family projection TOMLs;
the simple/CSS projection generators plus their xtask entry owners; and every
checked runtime file under JSON, CSS L4, CSS Pretty, and Google Sheets. All 44
copies under `runtime-projection-snapshot/` were re-read and match their
source rows in path, mode, bytes, and SHA-256. Shared runtime dependencies are
already retained in the 843-member build ledger and are not duplicated.

## RED ambiguities and next boundary

1. R6's old 867-member tuple cannot be independently reconstructed. Its
   missing ledger is not retroactively cured by R7.
2. The 18-file candidate remains semantically unaccepted and is not a
   complete-fleet fixed point. Custody is not acceptance.
3. Runtime-projection files are frozen as preimages only. No projection was
   generated or installed and no downstream semantics were run.
4. The `apple.rs` correction is now ordinary source and its one-member
   PRE-to-POST delta is exact. This author did not authorize or semantically
   accept the correction; custody proves only that it was not mixed with a
   second governed build-input change.

The next authorized step may adjudicate the exact one-member POST delta, then
form the four causal/semantic cluster decisions. No generator execution is
legitimate until the controlling R7 lane explicitly accepts that POST and the
declared candidate operand.
