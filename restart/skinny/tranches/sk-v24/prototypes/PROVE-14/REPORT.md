# PROVE-14 — TAPE-IN-TS (F4): the offset-tape SoA layout crosses to TypeScript

**Family** F4 (the unified value fabric). **Question** does skinny's offset-tape
layout — the same bytes Rust decodes — decode byte-exactly in TS via typed arrays,
and does a TS walk over it beat `JSON.parse`+walk? **HEAD** `d2b32f6e3`.
**Corpus** twitter.json (631,515 B). **Runtime** node v26.0.0, aarch64.

## Verdict — PARTIAL

- **Layout crosses byte-exactly: PROVEN.** A TS decoder walks the dumped SoA planes
  and reproduces a `JSON.parse` walk **node-for-node** — all 6 structural counts, the
  code-unit total, and a 32-bit fingerprint match exactly, across 100 cold child
  processes per lane. This is the primary PROVE-14 claim ("same layout decoded in TS;
  layout parity proof") and it holds.
- **Full-walk 2x speed gate: UNMET (measured refutation).** Cold, `tape decode+walk`
  is at **parity** with native `JSON.parse+walk` (paired json/tape = 0.981x, CI
  [0.880, 1.082]) — not 2x. Cold V8's native JSON parser is near-optimal; a
  hand-written JS walk (cold JIT) cannot beat it on a *full* materialization.
- **Stop-early consumer gate: MET at ~4x.** The workload F4 says features must be
  proven by (partial/lazy access) clears 2x decisively: extracting three leaves from
  `statuses[0]` and stopping is **4.14x** faster median / **4.78x** paired
  (CI [4.20, 5.37]) — the tape skips `statuses[1..99]`; `JSON.parse` must parse the
  whole document to hand back any field.

The layout unification succeeds; the literal full-walk 2x gate does not; the tape's
actual cross-language value (On-Demand / `get_from_str`, R6 §8a, R7 §1/§5/§9) clears
2x by 4x.

## What was built (703 LOC, no runtime changes, no regen)

| File | LOC | Role |
|---|--:|---|
| `crates/bbnf-bench/src/bin/tape_dump.rs` | 78 | Rust dumper: `parse_tape` twitter.json → writes SoA planes + source |
| `crates/bbnf-bench/ts-tape/tape.mjs` | 364 | `TapeDoc` monomorphic decoder: classify, sparse-flag lookup, lazy walk, pointer nav, shared hash primitives + `walkJs` reference |
| `crates/bbnf-bench/ts-tape/load.mjs` | 34 | read planes → aligned typed arrays (`decodeTape` = the timed layout decode) |
| `crates/bbnf-bench/ts-tape/pointer.mjs` | 34 | stop-early `tapeGet`/`jsonGet` (statuses[0] × 3 leaves) |
| `crates/bbnf-bench/ts-tape/child.mjs` | 52 | one cold single-shot timed sample, prints `{lane,ns,digest}` |
| `crates/bbnf-bench/ts-tape/verify.mjs` | 55 | full + partial digest parity + payload/flag cross-checks |
| `crates/bbnf-bench/ts-tape/bench.mjs` | 86 | spawns N cold children per lane, interleaved/paired, min/median/ratio |

The dump bin is autodiscovered (`src/bin/*.rs`, no `[[bin]]`, no `Cargo.toml` edit).
The TS decoder is a self-contained directory-module colocated with the bench crate
that feeds it. Generated files untouched; nothing hand-patched.

## The layout that crossed (SoA, decoded identically to the Rust view)

The tape is Structure-of-Arrays; a "node" is a `u32` cursor into `offsets`; kind is
re-derived from `source[offsets[cursor]]` (no stored discriminator); flags are a
sparse sorted side-channel. Separators (`,` `:`) never receive an offset — confirmed
in the Rust builder (`generated.rs` `consume_delimiter` advances without emit). Dumped
planes (twitter.json):

```
offsets.u32       118,292 B   29,573 tokens (u32 LE src byte-offset each)
flag_cursors.u32    1,248 B      312 escaped-string cursors (sorted)
flag_values.u8        312 B      STRING_NEEDS_DECODE = 0x01 (GRAMMAR_BIT0)
source.bin        631,515 B
payload arena           0 B      (offset-only tape invariant — proven empty)
tape total        119,852 B   = 18.98% of source
```

### Visual verification — SoA decode (node-side inspection)

```
cursor  offset(u32LE)  src_byte  kind
     0            0     0x7b {    ObjectOpen
     1            4     0x22 "    String        <- "statuses"
     2           16     0x5b [    ArrayOpen
     3           22     0x7b {    ObjectOpen    <- statuses[0]
     4           30     0x22 "    String
    10          117     0x7d }    ObjectClose
   ...
first 5 flagged (escaped) string cursors, raw span they point at:
  flagCursor=18  flagVal=0x1 needsDecode=true  "@aym0566x \n\n名前:前田あゆ...   (\n + non-ASCII)
  flagCursor=20  flagVal=0x1 needsDecode=true  "<a href=\"http://twitter.com/... (\" escapes)
```

Offsets are little-endian `u32` pointing at structural bytes; each classifies to the
correct `JsonNodeKind`; the sparse flag plane binary-searches to mark exactly the
strings carrying escapes. The layout is byte-identical across the language boundary.

### Byte-exact parity (verify.mjs)

```
field       tape        json.parse
objects     1264        1264        ok
arrays      1050        1050        ok
strings     18099       18099       ok
numbers     2109        2109        ok
bools       2791        2791        ok
nulls       1946        1946        ok
codeUnits   304329      304329      ok
fp          986912547   986912547   ok
partial get (statuses[0]): tape fp=3431570937 json fp=3431570937  codeUnits 170/170  PASS
DIGEST PARITY: PASS (node-for-node structural walk equivalence)
```

The 312 escaped strings (incl. the Japanese `\n`-laden `text`) decode through the flag
channel to the identical UTF-16 units `JSON.parse` yields — proven by fingerprint match.

## Measured table (cold, paired, N=50 fresh node children per lane)

| workload | lane | min ms | median ms | paired json/tape | 95% CI | N | loadavg |
|---|---|--:|--:|--:|---|--:|---|
| full walk | tape decode+walk | 5.306 | 6.586 | — | — | 50 | 19.5/16.4/13.7 |
| full walk | JSON.parse+walk | 4.731 | 6.227 | **0.981x** | [0.880, 1.082] | 50 | 19.5/16.4/13.7 |
| stop-early | tape decode+get | 0.329 | 0.458 | — | — | 50 | 18.6/16.3/13.7 |
| stop-early | JSON.parse+get | 1.372 | 1.896 | **4.782x** | [4.199, 5.366] | 50 | 18.6/16.3/13.7 |

Each sample is a cold child process (fresh V8, no warmup, one timed op); tape/json
interleaved per sample so load hits both sides of every ratio. The tape lane's timed
region **includes** the layout decode (typed-array construction, incl. a copy of the
offset plane) — a cost counted against the tape, and it still wins 4x on stop-early.
Loadavg ~18-20 (18-core box, heavy) — ratios are load-robust by paired interleaving;
absolutes are directional.

### Why full-walk is parity, not 2x

Cold native `JSON.parse` is a hand-optimized C++ parser; the TS lane is cold-JIT JS.
A full walk materializes every leaf on both sides, so the tape's parse-avoidance
(Rust already built it) is offset by the JS walk cost. The byte-hash fast path
(fold ASCII string bytes with **zero** JS-string allocation; decode only the escaped /
non-ASCII minority via the flag channel) lifted the tape from 0.66x → 0.98x, but cold
JS cannot clear the native parser on a full materialization. The tape's edge is
**not touching what you don't read** — hence the 4x on stop-early.

## Integration cost + exact seam

Zero runtime changes. The Rust tape already exposes the four planes as public slices —
`Tape::{offsets, flag_cursors, flag_values, source}`
(`crates/runtime/src/tape/mod.rs:126-137`). A real TS backend needs one wasm-bindgen
(or ABI) export handing those four slices to JS; `TapeDoc` consumes them unchanged.
The dumper (`tape_dump.rs`, 78 LOC) already proves the export shape. Effort:
~1 wasm shim + this decoder verbatim.

## Reproduce

```
cd skinny && cargo build --release -p bbnf-bench --bin tape_dump
./target/release/tape_dump <DUMP_DIR>
cd crates/bbnf-bench/ts-tape
TAPE_DIR=<DUMP_DIR> node verify.mjs
TAPE_DIR=<DUMP_DIR> node bench.mjs full 50
TAPE_DIR=<DUMP_DIR> node bench.mjs partial 50
```
