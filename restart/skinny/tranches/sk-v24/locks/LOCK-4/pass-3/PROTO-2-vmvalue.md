# LOCK-4 PROTO-2 (pass-3, probe 2) — the 4C VM value plane + the live cross-language iso-of-record oracle

**Family:** 4C/4D — shared value-plane completion + cross-language iso oracle.
**Base-pin:** `ce51c36fc` (verified ancestor; `git merge-base --is-ancestor ce51c36fc HEAD` → YES).
**HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (clean base pin — no wrong-artifact risk).
**Loadavg at measurement:** 3.57 (1m) / start-of-run 4.87. macOS 26.4.1 arm64. No x86.
**Cap:** 50 min. Froze on schedule.

---

## QUESTION

The pass-3 FAMILY-SPECS claimed 4C's value plane is *built and byte-identical* (`Op::ApplyFn(FnId)`,
`apply_fn.rs`, PROTO-2 oracle GREEN). research-C tree-verified this **FALSE** on HEAD `c43126422`:
no `Op::ApplyFn` (Op = 30 variants), no `apply_fn.rs`, `Value` = 4 shapes, and `IrNode::Map`'s
`fn_id` silently dropped at `compiler/node.rs:31`. The VM was a value-**structural** recognizer, not a
combinator core. **Build the value plane, and build the never-built cross-language iso-of-record
oracle (G-C1/RF-2 — the surviving 4C/4D axis with zero running evidence), in one probe.**

## METHOD (research-C §2 — four mechanical edits + the dual oracle)

1. `Op::ApplyFn(FnId)` added to `bytecode.rs`; `IrNode::Map { inner, fn_id }` lowered at
   `compiler/node.rs` to `compile_node(inner)` **then** `emit(ApplyFn(fn_id))` — stop dropping `fn_id`.
2. `#[serde(default)] fns: Vec<FnDescriptor>` added to `BytecodeProgram`, cloned from `ir.fns` at
   compile time (mirrors `dispatch_tables`). `FnDescriptor`/`MapExpr` already derive Serialize → the
   projection plane crosses as **data**, zero new serialization.
3. `Value` extended with `F64` / `U32` / `Str(Box<str>)` / `Variant { tag, child }`.
4. New `interpreter/apply_fn.rs` (~150 LOC MapExpr/FnDescriptor evaluator) + handler in the loop; a
   ~200-LOC TS evaluator (`crossing.mjs`) runs the **same serialized descriptor** from the same wire.

- **Oracle A (value plane, Rust):** projection-dense hand-built IR → compile → run VM; assert the
  projected `Value` tree byte-identical to an independently specified golden, across all four Value
  arms + every evaluator path.
- **Oracle B (crossing):** live Rust `serde_json` of the LIVE `BytecodeProgram` → TS decode →
  (A) opcode-stream structural identity + decode∘encode idempotence + `ApplyFn`/`fns` comprehension;
  (B) the SAME serialized `FnDescriptor` evaluated on both sides → node-for-node `Value` identity.
  Goldens are emitted from the live Rust run (`emit_valueplane_crossing`, `emit_ts_crossing_artifacts`
  in `crates/ir/tests/vm/apply_fn_oracle.rs`) — none hand-faked.

## WHAT RAN

- `cargo test -p bbnf-ir --profile ax-iter --test vm apply_fn_oracle` — **8/8 pass** (compile clean,
  36.35s cold build). Oracle A: `oracle_a_number_convert_f64`, `_hex_convert_u32`, `_enum_wrap_variant`,
  `_mapexpr_arithmetic_u32`, `_mapexpr_floatlit_f64`, `_mapexpr_stringlit_str` all `ok`;
  `oracle_b_serde_idempotence` `ok`; artifact emitters `ok`.
- `node crossing.mjs` over the live-emitted `valueplane_crossing.json` (9 descriptors) +
  `projection_program.json` (21-opcode live program). Both clauses **GREEN**.

## THE NUMBERS

| Measurement | Result |
|---|---|
| Oracle A — value-plane assertions (F64/U32/Str/Variant + arithmetic/FnCall/InputProp) | **6/6 byte-identical GREEN** |
| Oracle A — serde encode∘decode∘encode idempotence; `ApplyFn` on wire; decoded program re-runs | **GREEN** |
| Op enum: 30 → **31** variants (`ApplyFn(FnId)` landed); `rg ApplyFn crates/ir/src` | was 0, now live |
| `Value` shapes: 4 → **8** (F64, U32, Str, Variant added) | landed |
| `compiler/node.rs` Map lowering | `fn_id` **emitted**, no longer dropped |
| `BytecodeProgram.fns` side table | populated from `ir.fns`, `#[serde(default)]`, crosses as data |
| Oracle B clause A — opcode-stream divergences (21 ops, 1 `ApplyFn`) | **0** |
| Oracle B clause A — decode∘encode idempotent; `fns` table crossed (1 descriptor) | **YES / YES** |
| Oracle B clause B — value-plane descriptor crossing, Rust↔TS node-for-node (9 descriptors) | **0 divergences GREEN** |
| Value-plane crossing coverage | NumberConvert(f64+int), HexConvert, EnumWrap→Variant, Expr arith, Expr float-mul, Expr StringLit, Expr FnCall(parse_hex)=U32(26), Expr InputProp(len)=U32(5) |

Sample identity (Rust vs TS, canonical): `{"F64":3.5}`, `{"U32":255}`, `{"U32":21}` (`(input*2)+1`),
`{"F64":6}` (`4*1.5`), `{"Str":"HELLO"}`, `{"Variant":{"tag":2,"child":{"Span":[0,2]}}}` — all equal.

## BOUNDARY (honest scope)

- The **six R3 JSON recognizer corpora** (canada, citm_catalog, twitter, data, data_xl, data_supermaxx
  — all present in-worktree) were **not** executed by an independent TS interpreter for clause-B
  Value-tree identity. That requires porting the `parse_that::regex::Dfa` engine (referenced by
  `MatchRegex`/`TrimWsPattern`, `#[serde(skip)]`, re-derived by `prepare_regexes`) to TS — a bounded
  but out-of-probe engineering task, **not** part of the value plane. It is the named remaining axis.
  The cross-language novelty of 4C/4D is the **descriptor evaluator** (the value plane), which *did*
  cross, node-for-node, from the same wire — that is what this probe proves.
- Oracle B used `serde_json` as the wire (TS-native `JSON.parse`); `rmp_serde::to_vec_named` is
  byte-available on the same `#[derive(Serialize)]` types but not exercised, for parsimony.
- No cold paired perf measurement was taken — this probe answers *build + correctness*, not throughput.
  (research-C §1 already carries the interpret-terminal-is-slow refutation as measured law; the VM's
  value is flexibility + verification, not speed.)

## DELIVERED PATCH SCOPE

`PROTO-2-vmvalue.patch` (647 lines) contains **only** the value-plane files I authored:
`vm/bytecode.rs`, `vm/compiler/{mod,node}.rs`, `vm/interpreter/{apply_fn.rs,mod.rs,value.rs}`,
`tests/vm/{apply_fn_oracle.rs,main.rs}`. The worktree was **pre-seeded** with the type-plane
`FnDescriptor::HostClosure` diff (`types/fn_descriptor.rs` + `core/backend` + `passes/`); those are
**not mine** and are excluded from the patch. The value plane builds atop that pre-seeded type plane
(my `apply_fn.rs` handles `HostClosure` as a pass-through — its body is opaque, LOCK-7 territory).

## VERDICT

**RAN_MEASURED.** The 4C VM value plane is now BUILT (4 mechanical edits landed, compile clean, 8/8
tests) and byte-identical GREEN (Oracle A). The cross-language iso-of-record oracle RAN for the first
time: opcode-stream + idempotence GREEN (0 divergences), and the value-plane descriptor crossing GREEN
node-for-node across 9 descriptors (0 divergences) — the same serialized `FnDescriptor` evaluated
identically in Rust and TS. The prior "byte-identical value plane" claim, false on `c43126422`, is now
**true on this base** — as a landed mechanism, not a spec. The one un-run axis (TS execution of the JSON
recognizer corpora incl. canada) is blocked on a TS regex-DFA port, orthogonal to the value plane, and
named of record.
