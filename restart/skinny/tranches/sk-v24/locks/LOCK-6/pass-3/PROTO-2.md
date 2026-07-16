# PROTO-2 — LOCK-6 6A MIRROR-TREE: the mirror.toml linter + real Rust↔TS(Node) drive

**Family 6A (MIRROR-TREE). Pass 3, probe 2.** Builds the never-built mechanism
that IS 6A — the registry's #1 6A directive, three passes undischarged.

- **BASE-PIN:** HEAD measured `ce51c36fc` (`locks(sk-v24): pass-1 bbnf CLOSED …`).
  `git merge-base --is-ancestor ce51c36fc HEAD` → **DESCENDS-OK**. No wrong-artifact risk.
- **Toolchain:** node v26.0.0, wasm-pack + wasm-bindgen 0.2.126, rustc wasm32-unknown-unknown, tsc (global), syn 2.
- **loadavg during cold builds:** 4.3–5.4 (recorded per run below).
- Every number below is **RAN_MEASURED**. Nothing is SPEC_ONLY.

---

## QUESTION

Does `cap = Σ_{c∈I} |source sites matching predicate[c]|` reproduce the live
exemption count **without being fit to it**, go **RED** on a synthetic extra —
and does a **real Rust↔TS(Node) behavioral drive** of `ts/path.ts` (not the
intra-Rust `compile_path_native` proxy) agree with canonical `bbnf::path` over
the failure paths, or expose a names-green / behavior-red gap?

## METHOD (worktree off ce51c36fc)

Live pair:
- **spine** = `crates/core/src/path/{ir,error}.rs` (+ `markers.rs`) — canonical
  `OwnedPathSegment / TypedPath / PathError / PathErrorReason` and the `Json /
  CssL4 / Sheets / Bbnf` marker ZSTs.
- **bridge** = `crates/bbnf-path-ts/{src/schema.rs, src/lib.rs, src/fixture.rs,
  ts/path.ts}` — serde twins + the wasm cdylib + the TS template-tag surface.

**(1) Relations authored.** `mirror-lint/mirror.toml` carries four `[[pair]]`
rows `r(twin)=preimage` for T={OwnedSegmentPayload→OwnedPathSegment,
TypedPathPayload→TypedPath, PathErrorPayload→PathError,
PathErrorReasonPayload→PathErrorReason}. `mirror-lint/idiom-classes.toml`
declares the I=5 classes: serde-transport / native-shim / native-error /
constructor / marker, each with a structural predicate.

**(2) Linter (xtask).** `mirror-lint/src/main.rs` — a self-contained syn-2
binary (detached from the workspace, own target dir). It **syn-extracts** Rust
decl sites (`syn::parse_file` → structs/enums/traits, `#[derive(..Serialize..)]`
attrs, `pub fn *_native` + `#[wasm_bindgen]` exports, `impl … { fn new }`) and
extracts TS names from `ts/path.ts` (cross-checked against the `tsc
--emitDeclarationOnly` `path.d.ts`, which emitted exactly the 5 exports
`OwnedPathSegment / TypedPath / PathErrorReason / PathErrorPayload / PathError`).
It classifies every **non-identity** bridge declaration, resolves each to a
spine pre-image, and computes:

```
cap        = Σ_c |sites matching predicate[c] that resolve to a spine pre-image|
exemptions = |non-identity bridge declarations|
GREEN  iff  exemptions == cap  AND  cap == 13
```

`cap` and `exemptions` are computed **independently** (one sums predicate hits
with valid pre-images; the other counts all non-identity decls); they coincide
only because the tree is disciplined — the model is not fit to the count.

**(3) Behavioral drive — REAL wasm, not the native proxy.**
`wasm-pack build crates/bbnf-path-ts --target nodejs --dev` → 820 KB
`bbnf_path_ts_bg.wasm` + CommonJS glue exporting `compile_path/execute_path`.
`ts/path.ts` compiled to ESM JS via `tsc`. `node_drive.mjs` loads the wasm and
drives five paths **through the actual `makePathTag(compile_path,"Json")` +
`PathError` from `ts/path.ts`**, so every call crosses the wasm boundary via
`serde-wasm-bindgen`. `examples/canonical_dump.rs` (bbnf core dev-dep) emits the
canonical `bbnf::path::check_path_against_registry` tuple for the same five
paths over the byte-identical JSON fixture. `compare.mjs` diffs
`(segment_index, segment_str, struct_name, alternatives, reason)` field-for-field.

## WHAT RAN

| Artifact | Command | Result |
|---|---|---|
| wasm cdylib | `wasm-pack build … --target nodejs --dev` | exit 0, 820 KB wasm (loadavg 5.4) |
| canonical dump | `cargo run --example canonical_dump` | exit 0, 5 canonical rows (loadavg 4.3) |
| linter | `cargo build` (syn 2) then 3 runs | exit 0 |
| Node drive | `node node_drive.mjs` / `node compare.mjs` | exit 0 |

## THE NUMBERS

### (a) cap reproduces the live exemption count — CLEAN TREE

```
serde-transport  cap+= 4      (OwnedSegmentPayload, TypedPathPayload, PathErrorPayload, PathErrorReasonPayload)
native-shim      cap+= 2      (compile_path_native, execute_path_native)
native-error     cap+= 1      (ts PathError extends Error)
constructor      cap+= 2      (PathErrorPayload::new, ts PathError#constructor)
marker           cap+= 4      (Json, CssL4, Sheets, Bbnf)
cap        = 13     exemptions = 13     expected = 13
VERDICT: GREEN  (exemptions == cap == 13)          exit 0
```

**13 = 4+2+1+2+4** reproduced exactly. ✅ criterion (a).

### (b) synthetic extras go RED — 2/2 caught

```
INJECT  pub struct BogusPayload  (serde derive, no canonical `Bogus`)  →
        RED  schema.rs:BogusPayload [serde-transport]   exemptions 14 != cap 13   exit 1
INJECT  export interface FooView {}  (no idiom predicate, no pre-image) →
        RED  path.ts:interface FooView [UNACCOUNTED]    exemptions 14 != cap 13   exit 1
```

`BogusPayload` matches the serde-transport *predicate* but has no `[[pair]]`
pre-image → excluded from `cap`, counted in `exemptions` → RED. `FooView`
matches **no** predicate → UNACCOUNTED → RED. **2/2.** ✅ criterion (b).

### (c) Node drive vs canonical — names-green / behavior-red

Structural linter on the clean tree: **GREEN** (all names mirror, 13==13).
Behavioral drive through wasm+serde-wasm-bindgen:

```
path                | verdict | detail
$.statuses[0].text  | DIVERGE | TS ok=true (leaf resolves) vs RS ok=false (UnregisteredRule @seg2)
$.nope              | AGREE   | field-for-field equal (UnknownField)
$.id[0]             | AGREE   | field-for-field equal (FieldOnScalar)
(empty)             | DIVERGE | segment_str TS="" vs RS="<empty>"; alternatives [] vs [Document,Status]
$.statuses[0].nope  | DIVERGE | reason TS=UnknownField vs RS=UnregisteredRule; struct Status vs Document
AGREE=2  DIVERGE=3  (of 5)
```

The documented **Class-2 routing divergence** (`$.statuses[0].nope`:
`UnknownField` vs `UnregisteredRule`) is **confirmed live through the wasm
boundary** — and the structural linter does **not** catch it (all type names are
green). **This is the 6D-floor necessity proof.**

**Honest deviation from the brief's predicted split.** The brief expected "4/5
agree, 1 diverges." The live drive shows **2/5 agree, 3/5 diverge**. All three
divergences share one root cause: canonical `check_path_against_registry`
deliberately leaves `TypeDesc::Named(_)` intact (`type_check.rs` returns
`UnregisteredRule` for *any* descent through a `Named` boundary — verified in
source), whereas the cdylib resolves `Named` eagerly via `registry.layout(id)`.
So every path that descends through `Named(Status)` diverges (`$.statuses[0].text`
→ error-vs-success; `$.statuses[0].nope` → the Class-2 reason flip), plus
`EmptyPath` renders `segment_str`/`alternatives` differently. The two frontends
mirror the offline checker and the eager proc-macro resolver respectively — two
distinct Rust behaviors the structural linter cannot distinguish. The thesis
(**names-green-while-behavior-red**) is not merely met but **strengthened**: 3 of
5 paths, not 1.

### Incidental defect found (real, in `ts/path.ts`)

`makePathTag`'s inner `try { … throw new PathError(payload) } catch (_) { throw
raw }` **swallows the `PathError` it just constructed** and rethrows the raw
wasm `JsError`. The TS frontend therefore never surfaces a typed `PathError` —
only the raw error whose `.message` is the JSON payload. The cross-boundary
`PathErrorPayload` is intact (the drive parses it out), but the rebox is dead
code. Worth a fix in the 6A execution tranche.

## VERDICT

**RAN_MEASURED.** The #1 6A directive is discharged with a running mechanism.

- (a) `cap == exemptions == 13 = 4+2+1+2+4` on the clean tree — **PASS**, not fit-to-count.
- (b) both synthetic extras (`BogusPayload`, `FooView`) → **RED, 2/2** — **PASS**.
- (c) real Rust↔TS(Node) wasm drive: **AGREE=2 / DIVERGE=3**; the Class-2
  divergence and two siblings are invisible to the green structural linter —
  **names-green-while-behavior-red CONFIRMED and strengthened**; the brief's
  4/1 prediction is corrected to 2/3 (single root cause: the offline checker's
  shallow `Named` policy).

**6A structural mirror alone is insufficient — the 6D behavioral floor is
necessary and non-optional.** The linter is the anchor 6A now carries into
execution; the drive is the proof that 6A must be paired with 6D.

## FILES (deliverables)

- `PROTO-2.patch` — new bridge code: `mirror-lint/` (Cargo.toml, mirror.toml,
  idiom-classes.toml, src/main.rs) + `examples/canonical_dump.rs`.
- `PROTO-2/mirror-lint-6A/` — the runnable code (linter, Node harness, compare,
  canonical example, injected RED variants).
