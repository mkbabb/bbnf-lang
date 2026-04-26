# Tranche B4 — FINAL

B4 closes via codegen-emission correctness (W0 — SIMD-bitmap kernel
labelled-break wrap) plus consumer-fixture polish (W1 — unified
`builder.rollback_to(...)` atomic-tape+value path, alias retirement,
fixture migration). The bbnf self-host xtask regen reaches
`prettyplease` end-to-end for every grammar in the workspace
manifest, and the workspace nextest run climbs from 1163/1490 pass
to 1480/1490 pass on a single landing.

## Architectural narrative

Two scopes compose B4. W0 is a single-source emitter fix: the SIMD
bitmap kernel's AVX2 nibble-LUT scanner emitted two
`break 'avx2_scan ::core::option::Option::Some(...)` forms whose bare
path-expression break-values syn's expression parser rejects with
"expected loop or block expression". The fix wraps each break-value
in `{ ... }`, satisfying syn while preserving identical runtime
semantics. The bbnf grammar exercises the kernel via prettify codegen
(negated character-class regexes inside `@pretty` rules); the json
grammar does not, which is why the defect surfaced only on the bbnf
self-host and stayed silent across the post-B3 substrate.

W1 traces the dominant runtime-parser regression — 327 failures
across the workspace, all sharing the signature `FusedBuilder::finish
called with N open value frames remaining` — to a contract mismatch
between two pre-W1 rollback paths. The grammar-emitted shape
functions wrap retry attempts in IIFE closures that may open
compounds before failing. Pre-W1 code rolled back via
`builder.columns_mut().rollback_to(X)`, which truncates only the
tape column family. The paired value substrate
(`value_open_stack`, `value_frames`, `value_payloads_*`) was never
unwound, so every failed retry leaked one or more value-frame opens.
Across a parse of any non-trivial input the leaks accumulate; at
finish time the assertion fires. Compounding the issue, the public
`FusedBuilder::rollback_to` carried a single-pop limit that could
not unwind nested compounds in any case.

The W1 fix lands at the canonical fused-substrate boundary. Each
`ValueCheckpoint` records the tape row offset its paired compound
was stamped at; `FusedBuilder::rollback_to(open_offset)` pops every
open-stack entry whose `tape_idx >= open_offset` and truncates the
value column families to the outermost popped checkpoint's pre-open
state in one pass. Every shape emitter migrates to
`builder.rollback_to(...)`, which routes through the unified atomic
path. Generated parser code regenerates against the new emission;
the workspace nextest run resolves 310 of the 327 baseline failures
plus 6 shape-dispatch goldens that drifted under the AY-II.W0'
emitter migration.

The transitional alias surface retires entirely on the same landing.
`pub type TapeBuilder = FusedBuilder` deletes from
`crates/tape/src/builder/mod.rs`. `ValueBuilderOutput<R>` deletes
from `crates/core/src/runtime/mod.rs`. The `value_builder` shim
module — including the `_ValueBuilderShim` ZST, the
`ValueBuilder<R>` alias, the `value_builder_new_call_count` /
`reset_value_builder_new_call_count` counter shims — deletes
entirely. The 4-arg `Parsed::new_fused(tape, input, root_offset,
value_builder_output)` bridge in
`crates/core/src/runtime/parsed.rs` deletes; the grammar emitter
calls the 3-arg `Parsed::new_fused_output(output, input,
root_offset)` directly. The `Parsed` accessors `value_builder_output`
and `into_value_builder_output` rename to `value_frames_output` and
`into_value_frames_output` (un-aliased). After the cycle-1 regen,
zero non-snapshot references to any retired surface survive across
the workspace.

## Performance

W0 measurements (post-fix, on the worktree substrate):

| Phase | bbnf grammar | json grammar |
|---|---:|---:|
| `compile_paths_request` | 3.15 ms | 1.46 ms |
| `generate_all` | 9.22 ms | 3.00 ms |
| `prettyplease` | 59.23 ms | 10.27 ms |
| `bbnf.rs` written | 34 048 lines | (unchanged) |

W1 measurements:

| Gate | Result | Wall |
|---|---|---:|
| `cargo check --workspace --profile ax-iter` | exit 0 | ~12 s cold; ~0.3 s warm |
| `cargo iter-check` (warm) | exit 0 | 0.27 s |
| `cargo xtask regen --check` | exit 0 (clean across 9 grammars) | 1.5 s |
| `cargo nextest run --workspace --profile ax-iter --no-fail-fast` | exit 100 (10 fail, all pre-existing) | 43.93 s |

The workspace test count moves from `1163 / 327 / 0 / 1490`
(pass / fail / skip / total) at the W2 close commit to
`1480 / 10 / 27 / 1490` after W1. The +317 pass-count delta covers
the 310 FusedBuilder open-frames failures, 6 shape-dispatch golden
tests refreshed against the post-W1 emitter, and 1 timing-sensitive
packed_cache ratio test (a transient delta).

## Test results

`cargo nextest run --workspace --profile ax-iter --no-fail-fast`
(post-W1):

```
Summary [  43.930s] 1490 tests run: 1480 passed (2 slow), 10 failed, 27 skipped
```

The 10 remaining failures are all pre-existing bugs unrelated to
the B4 scope:

- 5 × `value_api_apples_to_apples json_roundtrip_*` —
  AY-II.W0'.b cursor-shape projection emits a deliberate
  `panic!` stub for the JSON `array` and `object` arms of
  `project_frame_JsonParser`. Any JSON input containing arrays or
  objects trips the stub; the cursor-shape projector
  implementation is scheduled for AY-II.W1.
- 1 × `ay_w3b_value_api_smoke to_value_returns_value_enum` —
  same cursor-shape stub trip path.
- 1 × `projection_totality projection_totality_runtime_call_count` —
  the `string` materializer reads
  `tape.payload_bytes(string_compound, 8)` which returns `None`
  because the compound's `child_off` points at a tape row, not an
  arena offset. Pre-existing materializer addressing bug;
  scheduled for AY-II.W1.
- 1 × `parse_count_invariant_to_value_is_thin_projection` — same
  cursor-shape stub path (parses `data.json`, hits the `array`
  arm).
- 2 × `pipeline_compile_request compile_paths_preserves_pretty_directives_*`
  — assertion in `crates/core/src/backend/rust/ir_enums.rs:36`:
  "ParserAttributes: `paths` and `grammar_rel_paths` length
  mismatch". Pre-existing populator gap.

## Cross-tranche effects

**B4 close lands AY-II.W0' close ceremony.** The unified
`builder.rollback_to(...)` path lands the atomic-tape+value contract
the W0'.a substrate shipped without — the runtime-parser regression
that surfaced as 327 fixture failures across the workspace was the
absent contract's runtime evidence. The W0'.a compose-boundary
aliases retire entirely on the same B4.W1 landing.
`docs/tranches/AY-II/waves/W0p.md` moves to `complete`;
`docs/tranches/AY-II/AY-II.md` reflects the close in the W0' row;
`docs/tranches/AY-II/PROGRESS.md` carries the close-ceremony entry.

**AY-II.W1-W5 substrate is now post-B4.** W1 sequences against the
unified rollback path; cursor-shape projection (covering the 10
pre-existing failures) is the immediate next dispatch. The rest of
the W1-W5 spec runs unchanged on the post-B4 substrate.

**B2 + B3 + B4.W0 close conditions remain green.** `cargo xtask
regen --check` is clean across all 9 grammars in the workspace
manifest; the regen sweep produces byte-identical output to the
on-disk per-grammar source the workspace ships. The B2 CI +
pre-commit gate (`cargo xtask regen --check`) holds across the W1
landing.

## Defensible floor

B4 lands the floor it declared:

1. The W0 panic is captured, root-caused at file:line, and fixed
   at source.
2. `cargo xtask regen --grammar bbnf` exits 0 with bbnf.rs
   written.
3. `cargo xtask regen --grammar json` exits 0 (sanity).
4. `cargo check -p bbnf -p xtask -p bbnf_derive --profile ax-iter`
   exits 0 (W0); `cargo check --workspace --profile ax-iter`
   exits 0 (W1).
5. `cargo nextest run -p tape --profile ax-iter` exits 0 (W0);
   `cargo nextest run --workspace --profile ax-iter --no-fail-fast`
   reaches 1480/1490 pass with zero remaining FusedBuilder-class
   failures (W1).
6. All commits land on master via cherry-pick; no destructive
   operations.

## Verdict

**B4 closes.** Two waves complete: W0 codegen-emission correctness
+ W1 consumer-fixture polish + AY-II.W0' close ceremony folded.
Test gate climbs from 1163 to 1480 passes; alias surface retires
entirely; cycle-1 regen is clean.
