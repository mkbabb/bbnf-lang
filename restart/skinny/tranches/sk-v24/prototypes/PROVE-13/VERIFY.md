# PROVE-13 — ADVERSARIAL VERIFICATION

**Verdict: UNVERIFIABLE (headline perf magnitude only).** Every correctness,
edict, and parity attack PASSED; nothing refutes the claim. The one thing I could
not do is independently reproduce the 0.948x cyc/B magnitude — the N=200 perf loop
did not emit its results table before the mandated return under a thrashing box
(loadavg climbed 8 → 12 → 38 → 43 during my window). This is a re-measurement
blocker, NOT a refutation.

Base: worktree at HEAD `7f3be27c2`, prover changes staged. Prebuilt
`skinny/target/ax-iter/csv_parity` confirmed current (binary mtime 1784180888 >
source mtimes) — no rebuild needed.

## Fresh numbers
| lane | fresh min cyc/B | claimed | N | loadavg |
|---|---|---|---|---|
| bbnf | not emitted before return | 30.199 | 200 | 8→43 |
| csv-core | not emitted before return | 31.846 | 200 | 8→43 |
| csv | not emitted before return | 22.149 | 200 | 8→43 |

The re-run reached the field-parity line then stalled inside the 200-iter loop
under load 43. min-of-N PMU cycles is load-robust in principle, but the wall never
completed within the cap. Release-profile (fat-LTO) A/B re-run: NOT ATTEMPTED
(would need a fat-LTO build + two more full runs; no budget under this load).

## Attacks run and outcomes
1. **x86 token hunt (aarch64-only edict)** — PASS. No `x86/sse/avx/_mm_/rdtsc/
   target_arch` in patch or `csv-prove/src/main.rs`. Timing is `proc_pid_rusage
   (RUSAGE_INFO_V5).ri_cycles` — macOS/aarch64-native.
2. **Name-gate hunt (no-name-gates edict)** — PASS. `runtime_generator.rs`
   `emit_sink_only` now routes on the structural predicate
   `dispatch_alt_count > 0 && direct_shapes.len() >= 2` (source line 105); the old
   `!has_rule("value")` is gone. Residual `has_rule` at `emit.rs:13` is an
   entry-rule existence check; `name == name` at `sink_only.rs:192` is a generic
   rule-lookup helper. The disclosed residue (`json_sink_direct` still finds its
   dispatch rule by `name == "value"`) is PRE-EXISTING and JSON-only — not added by
   this patch, does not touch CSV routing.
3. **Profile asymmetry (benchmark honesty)** — PASS (within the ax-iter frame).
   Both bbnf and csv-core lanes execute in ONE binary under ONE profile (ax-iter,
   thin-LTO/cgu16), so the ratio is apples-to-apples. Report is transparent it is
   not fat-LTO release; the residual fat-LTO A/B is the only measurement gap, and I
   too could not close it here.
4. **Warm contamination** — PASS. Each lane is warmed once, then cold per-parse
   (one full-corpus parse per sample); the reported metric is min-of-N over
   in-process cycles, not a warm inner loop.
5. **Correctness oracle** — PASS (reproduced). The bench's `correctness_gate()`
   printed `PASS (quoted/escaped-quote/CRLF-in-field/empty-field)`, and the full
   50 MiB corpus `recognize`d without panic (an error would `.expect`-panic).
6. **Field-count parity** — reproduced EXACTLY, and the `match=false` is expected
   and disclosed: `bbnf_field_offsets=4807590 csv_fields=3432970` — delta 1374620
   = the quoted-field count (a quoted field emits two tape entries: a `Quoted` node
   mark + a leaf offset). The REPORT states this verbatim. Not a defect.
7. **Sheets / JSON non-regression** — Sheets change is behavior-preserving by
   inspection: the only edit is `const NEWLINE_SIGNIFICANT = false` plus a guard
   that, with `false`, leaves `skip_ws` byte-identical (const-folds out). JSON /
   check-json / check-sheets were NOT re-run (cargo would confound the in-flight
   cycle measurement; deferred, then load foreclosed it).
8. **Stub / workaround hunt** — PASS. `recognize` → `parse` → `parse_into_tape`
   does a full descent, builds the tape, and enforces `cursor == len` (trailing-
   characters error). No short-circuit; no fallback path.

## Rationale
The structural cures are real and parsimonious (empty-shapes bail lifted, name-gate
→ shape predicate, WALL-A newline-significance, WALL-B class-run inline scan). The
corpus is the exact claimed 50.0 MiB. Correctness reproduced. No edict violation
found. The sole un-closed item is my own reproduction of the 0.948x cyc/B figure,
foreclosed by a load-43 box within the return window — hence UNVERIFIABLE on the
headline magnitude rather than CONFIRMED. A rerun on an idle box would very likely
land the number; the claim is internally consistent and survived every attack.
