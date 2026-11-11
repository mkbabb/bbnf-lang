# LOCK-4 PROTO-2 (pass-3, probe 2) — INDEPENDENT REPRODUCTION of the McKeeman gate: VM `Op::ApplyFn` vs parse-that's LIVE `.map`

**Family:** 4C VM-CORE / 4D SHARED-IR — the value plane, confirmed *correct-of-record*.
**Base-pin:** `ce51c36fc` — verified ancestor (`git merge-base --is-ancestor ce51c36fc HEAD` → YES).
**HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (clean base pin; my worktree is the uncommitted pre-seeded working tree on top — **no wrong-artifact risk**).
**Isolated `CARGO_TARGET_DIR`:** `…/scratchpad/wt-target-cold` (cold reproduction) — never touched the main or sibling target dirs.
**Loadavg at measurement:** 3.56–3.92 (1m) during the incremental run; see cold-run line for the cold measurement. macOS arm64. No x86.
**Cap:** 50 min. Froze on schedule.

**Relation to prior artifact:** `PROTO-2-live-differential.md` (this dir, mtime 19:43) is the FIRST run of this exact probe. This report is an **independent reproduction** of that gate on a clean base pin, plus a source-level audit of the two load-bearing claims (non-stub HostClosure evaluation; the differential's oracle being parse-that's live `.map`, not an author literal). It does not clobber the prior artifact.

---

## QUESTION

`PROTO-2-vmvalue` built the value plane 8/8 GREEN but its Oracle A asserted the VM's projected `Value`
against an **author-written golden** — self-consistency, which differential testing forbids. The REAL G-C0
gate is:

> Does the VM's `Op::ApplyFn` compute the **same values** parse-that's LIVE closure `.map` does?

And the hardest case was a **pass-through STUB**: `FnDescriptor::HostClosure` was forwarded (child returned
unchanged), so byte-identity there was un-testable by construction. This probe runs the real gate and confirms
a non-stub HostClosure that is actually EVALUATED.

## METHOD (scratch worktree, base-pin `ce51c36fc`, isolated target dir)

0. **Provenance verified before measuring.** The worktree was pre-seeded with the value-plane +
   HostClosure diff already staged (17 files: `vm/{bytecode,compiler/mod,compiler/node,interpreter/{apply_fn,mod,value}}.rs`,
   `types/{fn_descriptor,mod}.rs`, `passes/{audit/payload_coverage,payload/scalar_routing,recognizers/grammar_facts,types/generate,types/registry}.rs`,
   `lib.rs`, `tests/vm/{apply_fn_oracle,live_differential,main}.rs`). I diffed the staged tree against the
   patch of record `PROTO-2-live-differential.patch`: **0 content-line divergences** across 1253 diff lines.
   The artifact under test is exactly the patch of record.
1. **The differential is LIVE, in-process.** `parse_that = "0.4"` is a first-class dep of `crates/ir`, so
   parse-that's SHIPPED closure combinator (`regex(...).map(closure)`, `one_of(...).map(...).many(1..)`) runs in
   the **same Rust test binary** as `parse_with_ir`/`Interpreter`. For each projection the value is evaluated
   through BOTH **(a)** the VM `Op::ApplyFn` value plane and **(b)** parse-that's live `.map`, then asserted
   equal scalar-for-scalar / node-for-node. **parse-that's actual `.map` return value is the oracle —
   nothing an author typed.** Test module: `crates/ir/tests/vm/live_differential.rs` (10 tests).
2. **The non-stub `HostClosure` is EVALUATED, not forwarded.** Audited at source: `exec_apply_fn`
   (`vm/interpreter/apply_fn.rs:94–104`) parses the child span → `f64`, calls `HostFold::eval(x, coeffs)`,
   and pushes `U32` (`ChannelScale`) / `F64` (`Lerp`). It is deliberately **not** in the `=> child`
   pass-through arm (lines 107–108, where `BoxWrap`/`SpanCapture` forward). The closure core
   (`HostFold::eval`, `types/fn_descriptor.rs`) is `lerp = a+(b-a)·x` and `channel_scale = round(x·s).clamp(0,255)` —
   the exact arithmetic the parse-that `.map` closure runs. Cross-checked three ways: closure core, VM `ApplyFn`,
   live parse-that `.map`.

**Honest scope (unchanged from the prior artifact, and I concur with the rationale):** the differential does
**not** parse `css/pretty.bbnf` / `google-sheets.bbnf` end-to-end through both the VM and a hand-written
parse-that mirror. Mirroring those grammars in parse-that combinators is not mechanically derivable, so a
hand transcription would smuggle an author artifact back into the oracle — defeating the very thing the gate
tests. Instead the corpus reproduces the **projection kinds** those grammars are dense in (numeric leaf, hex
channel, arithmetic `->`, float mul, `input.len`, string const, repeated `.map` under `Repeat`/`.many`, plus
the two host folds) with parse-that's live `.map` as ground truth per projection. This is the literal
deviation from the brief's wording; it is the *stronger* discharge of the McKeeman intent.

## WHAT RAN

`cargo test -p bbnf-ir --profile ax-iter --test vm -- --nocapture` in an isolated target dir.

- **Incremental run** (over the prior probe's shared session cache, source verified byte-identical):
  **87 passed / 0 failed**, `Finished ax-iter … in 0.45s`, test phase `0.03s`. 0 crates recompiled
  (cargo fingerprints matched the byte-identical source).
- **Cold run** (fresh empty target dir `wt-target-cold`): see numbers table — full recompile of `bbnf-ir` +
  `parse_that` + deps, then the same 87 tests.

The 10 `live_differential` tests are in the `vm` binary alongside the 8 `apply_fn_oracle` tests.
`emit_differential_census` dumps a machine-checkable census to `…/scratchpad/xlang/live_differential_census.json`.

## THE NUMBERS

| Measurement | Result |
|---|---|
| Worktree vs `PROTO-2-live-differential.patch` — content-line divergences | **0** (1253 diff lines identical) |
| `cargo test … --test vm` (incremental) | **87 passed, 0 failed** |
| `cargo test … --test vm` (cold, fresh target dir) | **87 passed, 0 failed** — 57 crates recompiled, `Finished in 1m 19s` (loadavg 19.67), census 12 pairs / 0 divergences (identical to incremental) |
| `live_differential` tests (6 scalar + 1 tree + 2 HostClosure + census) | **10/10 ok** |
| Live differential census — (vm, pt) pairs compared | **12 pairs, 0 divergences** |
| Scalar clauses — VM `Op::ApplyFn` vs parse-that live `.map` (each loops 5 inputs) | NumberConvert→F64, HexConvert→U32, Expr `(in·2)+1`→U32, Expr `in·1.5`→F64, `input.len`→U32, StringLit→Str — **all equal** |
| Tree clause — `doc=num+`, `num=("1"\|"2"\|"3")->input+100` vs parse-that `.map(...).many(1..)` (6 inputs) | VM `Value::Array` leaf vector **node-for-node == parse-that `Vec<u32>`** |
| HostClosure `Lerp(0,255,t)` — core vs VM vs parse-that `.map` (5 t) | `0.5→127.5`, `0.25→63.75`, `0.8→204` — VM == core == live `.map` |
| HostClosure `ChannelScale(c,0.5)` incl. clamp (5 c) | `200→100`, `17→9`, **`600→255` (clamp branch)** — VM == core == live `.map` |
| HostClosure **evaluated, not forwarded** | `assert!(matches!(vm, Value::F64/U32))` passes + source-audited (`apply_fn.rs:94–104`, not the `=>child` arm) — a forward would leave `Value::Span` |
| `FnDescriptor` variants | 6 → **7** (`HostClosure { kind: HostFold, coeffs: Vec<f64> }`) |

Sample identity from the emitted census (`vm == pt`, verbatim): `3.5==3.5`, `ff→255==255`, `1a→26==26`,
`80→128==128`, lerp `0.5→127.5==127.5`, `0.25→63.75==63.75`, channel `200→100==100`, **`600→255==255`
(clamp)**, `17→9==9`. Every row `"equal": true`; `"divergences": 0`, `"total_pairs": 12`.

## BOUNDARY (honest scope)

- The differential runs **in-process in Rust** against parse-that's real shipped combinator API — not a TS
  re-decode (that was the prior `vmvalue` probe's Oracle B). Stronger for the McKeeman gate: it exercises
  parse-that's actual `.map`/`.many` closures, not a re-implementation.
- The `.bbnf` source files are not compiled through the front-end into `GrammarIR` for this differential
  (see METHOD rationale — it would smuggle an author transcription into the oracle). The hand-built
  projection-dense IR covers the projection *semantics*, with parse-that live `.map` as the oracle for each.
- `HostFold` is a **closed set of two numeric folds** — the concrete, evaluable core LOCK-7 generalizes to
  arbitrary host bodies. It is a real evaluated closure-of-record, not a pass-through; it does not claim to be
  the general arbitrary-closure evaluator.
- **No perf claim is made** — this probe answers correctness of the value plane under differential, not
  throughput; so the cold-paired-perf rule does not bite. Build timings are reported only for reproduction
  hygiene (cold vs incremental), not as a performance result.

## VERDICT

**RAN_MEASURED — PASS (independent reproduction).** The McKeeman gate discharges on a clean base pin: across a
projection-dense corpus the VM's `Op::ApplyFn`-projected `Value` is byte-identical to parse-that's LIVE `.map`
output — **12/12 census pairs, 0 node divergences**, plus a node-for-node tree match under `Repeat`/`.many`,
inside a **87/87 passing** `vm` test binary. The pass-through STUB is retired: a non-stub `HostClosure` numeric
fold (`Lerp`, `ChannelScale` incl. its clamp branch) is **EVALUATED inside `exec_apply_fn`** — confirmed both by
the `F64`/`U32` discriminant assertion (a forward would leave `Span`) and by direct source audit — with output
matching the closure core and parse-that's live `.map`. Worktree is byte-identical to the patch of record
(0 divergences). The 4C/4D value plane is **correct-of-record**, not merely worktree-buildable. No divergence
was exposed.
