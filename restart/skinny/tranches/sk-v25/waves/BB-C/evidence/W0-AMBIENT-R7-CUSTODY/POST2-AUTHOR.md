MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 custody POST2 author.

# R7 custody POST2 receipt

- Date: 2026-07-22.
- Corrected census tool: SHA-256
  `1fa707954622584ceaaadd787d61c6c603bf2cec5101a9cad2d10e0b0251da12`.
- Base POST ledger: SHA-256
  `e006e3ea3c3a3f273b62cf75cfdfb0f68872fdadb10d2d16be6a5324cfb79922`.
- Invocation hygiene: `PYTHONDONTWRITEBYTECODE=1`; no cache file exists.
- Scope: custody evidence only. No source, generated product, runtime product,
  package, lockfile, Value, or parse-that byte was changed by this author.
  No build, test, generator, profile, or mutant ran.

## POST2 tuple

| Measure | Exact value |
|---|---|
| members | 843 |
| bytes | 15,313,349 |
| path-set SHA-256 | `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` |
| ledger SHA-256 | `efa627865654d94c409cda7a20e959cc7e976f8dc68cf204c2c0927b1a7b9a01` |
| ledger | `BUILD-INPUTS-POST2.tsv` (844 lines / 105,390 bytes) |
| path list | `BUILD-INPUTS-POST2-PATHS.txt` (843 lines / 42,081 bytes) |
| census | `POST2-CENSUS.json`, SHA-256 `26cbe1d5417c27018d74dd5821ccc5b58dee51b9869957c6c64d3c5359631aeb` (64 lines / 2,099 bytes) |

The path set is byte-identical to PRE and POST.

## POST to POST2

Exactly zero members were added, zero removed, and one changed:

```text
bbnf-lang/xtask/src/regen_simple_runtime.rs
POST  0644  124350  82a0e360b64bcd762dd90e584f32f95911bb6fa05305168ee104f257a3621a8f
POST2 0644  125719  9d40aaa2892b9899b5a606d095517809b1fdd3b6468799fd004a8a5641a6d4c3
```

## PRE to POST2

Exactly zero members were added, zero removed, and two changed:

1. `xtask/src/architecture/evidence/binding/apple.rs`:
   `0644 / 19,936 / ce3ae9af…` to
   `0644 / 19,855 / 54827145…`.
2. `xtask/src/regen_simple_runtime.rs`:
   `0644 / 124,350 / 82a0e360…` to
   `0644 / 125,719 / 9d40aaa2…`.

`POST2-CENSUS.json` contains every full before/after identity. This receipt
proves only the exact custody transition. It does not grant semantic,
generator, runtime-projection, or tranche acceptance.
