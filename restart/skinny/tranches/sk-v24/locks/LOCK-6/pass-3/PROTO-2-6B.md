# PROTO-2 (6B) — full-decoder V8-monomorphism gate on NON-byte-identical generated TS (+ M1 discharge)

**Family 6B (SPEC-GENERATED-SKELETON). Pass 3, probe 2.**
*Filename note:* delivered as `PROTO-2-6B.md` (not `PROTO-2.md`) because pass-3 already
holds a **family-6A** `PROTO-2.md` (MIRROR-TREE); this is the distinct 6B probe-2 and
must not clobber it.

- **BASE-PIN:** worktree arrived STALE at `d2b32f6e3`; `git merge-base --is-ancestor
  ce51c36fc HEAD` = **NOT-ancestor**. Fast-forwarded `git merge --ff-only ce51c36fc`
  (exit 0). **HEAD measured on = `ce51c36fc`** (`locks(sk-v24): pass-1 bbnf CLOSED …`).
  Post-ff both ancestry directions equal → HEAD **is** the pin. No wrong-artifact-risk.
- **Toolchain:** rustc -O (edition-2015 single file), node **v26.0.0** (V8), macOS arm64. No x86.
- **loadavg (recorded per run):** oracle/build ~7.5–8.5; mono gate run-1 4.16, run-2 4.16.
- Every number below is **RAN_MEASURED**.

---

## QUESTION

Does a `LayoutSpec`-GENERATED full `TapeDoc` decoder preserve V8 monomorphism, or does
generation emit a boxed-union `classify` / un-lowerable `switch` / post-ctor field
assignment that silently deopts and evaporates the 4.4× stop-early (R3/R4)? The pass-3
classify-face gate passed only on the **byte-identical** (trivially shape-identical)
direction — uninformative by construction. The gate becomes real ONLY on
**non-byte-identical** generated output. This probe also discharges **M1** (the missing
`gen_tape.mjs` / no reproducible run-record).

## METHOD (worktree off `ce51c36fc`)

1. **Extended `emit_classify.rs`** (pass-3/proto-1-code) from the classify FACE to the
   FULL decoder: added `emit_full_decoder(spec)` + `gen_classify(spec)` and a new
   `main()` mode (`emit full` → whole decoder to stdout; `emit <dir>` keeps the 4 face
   files). DATA (spec-driven): the `K_*` constant block and the `classify` switch are
   rendered from `spec.kinds` + `spec.discriminator` arms — the exact 6B seam. MACHINE
   (walk/leaf/nav/pointer/oracle hash arithmetic): a shared template rendered in the
   **generator's own formatting**, deliberately NOT a byte transcription of the hand
   PROVE-14 source.
2. **`gen_tape.mjs`** = `rustc -O emit_classify.rs -o emit && ./emit full > gen_tape.mjs`
   (295 LOC self-contained ESM: `class TapeDoc`, primitives, digest,
   `walkJs` oracle, `tapeGet/jsonGet`, `unescapeJson`). **Regen is reproducible**:
   a fresh emit is byte-equal to the checked-in `gen_tape.mjs`.
3. **`build_tape.mjs`** (harness, untimed) reconstructs the four SoA planes
   (offsets / flag_cursors / flag_values / source) directly from `twitter.json`,
   faithful to skinny's Rust offset-tape builder (one offset per structural/value
   token; `,` `:` get NO offset; strings containing `\` flagged `STRING_NEEDS_DECODE`).
   This replaces the Rust `tape_dump.rs` so the probe needs **no skinny build** — and is
   validated against the JSON.parse oracle (below), which is language-independent.
4. **`mono_check.mjs`** drives twitter.json's crossed four planes under
   `node --allow-natives-syntax`, asserting the hot `classify` / `nextSibling` / `getKey`
   / `walk` path.

Corpus: `skinny/crates/test-fixtures/corpus/json/twitter.json` (631,515 B, md5
`bafb3de8…`, the exact file PROVE-14 dumped from). Planes rebuilt: **29,573 offsets, 312
flagged strings** (== PROVE-14's 312/18099 escaped strings).

## WHAT RAN

| Artifact | Command | Result |
|---|---|---|
| generator compile | `rustc -O emit_classify.rs -o emit` | exit 0 |
| decoder emit | `emit full > gen_tape.mjs` | 295 LOC; regen byte-reproducible |
| oracle (JSON.parse lane) | `node oracle.mjs twitter.json` | fp match true/true |
| generated-decoder parity | `node testgen.mjs` | full+partial+oracle all true |
| **mono gate** | `node --allow-natives-syntax mono_check.mjs` | **GREEN, exit 0** (×2) |
| teeth (negative control) | `node --allow-natives-syntax neg_control.mjs` | both hazards caught, exit 0 |

## THE NUMBERS

### (a) Non-byte-identity — the condition that makes the gate informative

```
sha256 hand tape.mjs = fa5641bf…   sha256 gen_tape.mjs = f1f7ef08…   DIFFERENT
diff hand↔gen: 337 changed lines.  classify(cursor)→classify(cur); xorAcc→acc;
methods reordered (nextSibling emitted before walk); pointer+oracle folded into ONE
module (hand split tape.mjs / pointer.mjs). Behaviourally equivalent, byte-distinct.
```

### (b) fp fold parity of the GENERATED decoder (twitter.json)

```
FULL     fp=986912547   obj1264/arr1050/str18099/num2109/bool2791/null1946/cu304329   MATCH
PARTIAL  fp=3431570937  codeUnits=170                                                  MATCH
oracle   walkJs fp=986912547  digestsEqual(tape,json)=true ;  jsonGet fp=3431570937   MATCH
```
All six structural counts + codeUnits reproduce the PROVE-14 VERIFY.md values exactly.

### (c) V8 monomorphism of the GENERATED decoder (`--allow-natives-syntax`)

```
[pre-walk]  HaveSameMap(a,b)=true   HasFastProperties a=true b=true
[classify]  all 29573 results unboxed Smi = true
[opt] classify     optimized=true  maglev    markedForDeopt=false   <-- GATED
[opt] nextSibling  optimized=true  maglev    markedForDeopt=false   <-- GATED
[opt] getKey       optimized=true  maglev    markedForDeopt=false   <-- GATED
[opt] walk         optimized=true  maglev    markedForDeopt=false   <-- GATED
      (indexArray turbofan; stringLeaf/decodeNumber/needsDecode maglev; decodeString
       stays interpreted — INLINED into getKey, whose optimized copy is maglev-clean)
[post-walk] HaveSameMap(a,b)=true   HasFastProperties a=true b=true
MONO=true  OPT(hot optimized,no deopt)=true  FP=true   GATE: GREEN   (exit 0, ×2 runs)
```

**OptimizationStatus decode was CALIBRATED, not assumed.** A known-optimized hot
function on this V8 reports `raw=41 = kIsFunction|kMaybeDeopted|kMaybeMaglevved`: so
`kOptimized`(1<<4) is NOT the tier signal and `kMaybeDeopted`(1<<3) is routine noise;
"optimized" ⇔ an optimizing tier (maglev/turbofan) is active, and the real deopt signal
is `kMarkedForDeoptimization`(1<<14), CLEAR on the calibrator. A first decode using bit-4
mis-flagged every (genuinely optimized) site as un-optimized — corrected before verdict.

### (d) TEETH — the gate is NOT vacuous (negative control, same gate)

```
[hazard-1 post-ctor field assignment]  HaveSameMap before=true after=false  -> CAUGHT
[hazard-2 boxed-union classify]  generated all-Smi=true  boxed all-Smi=false (29573)  -> CAUGHT
```
Hazard-1 (a `doc.decodeCache = …` after construction) flips `%HaveSameMap` true→false —
the exact falsifier for post-ctor field assignment. Hazard-2 (classify returning
`{}` / `[]` / `"…"` per arm) is caught by **Smi-uniformity**, not the deopt bit: V8
maglev *tolerates* a boxed classify with NO `kMarkedForDeoptimization`, so
`%GetOptimizationStatus` alone is the wrong probe — `%IsSmi` over all 29,573 results is
the sharp one. Both are folded into `mono_check.mjs`'s own verdict.

## M1 DISCHARGE

`gen_tape.mjs` now exists, is **generated** (not hand-written) by the checked-in
`emit_classify.rs`, and its emit is **byte-reproducible** (`emit full` == the committed
file). The run-record is this file + the four runnable scripts. M1 closed.

## VERDICT

**RAN_MEASURED — GATE GREEN.** A `LayoutSpec`-generated, **non-byte-identical**
(sha-distinct, 337 lines changed) full `TapeDoc` decoder is V8-monomorphic on the
load-bearing path: `%HaveSameMap(a,b)`=true pre+post-walk, `%HasFastProperties`=true both
instances, `classify` returns a uniform unboxed Smi across all 29,573 cursors, and the
four gated hot sites (`classify`/`nextSibling`/`getKey`/`walk`) reach an optimizing tier
with no `kMarkedForDeoptimization` — AND it folds to fp **986912547** (full) /
**3431570937** (partial), all counts exact. The R4 falsifier does **not** fire.

**6B's crown extends from the classify FACE to the load-bearing decoder.** Generation is
drift-free AND V8-safe: the ~364-LOC hand `tape.ts` drift surface closes to 0 — it is now
an emit target, not a maintained artifact. The gate is proven non-vacuous by a two-hazard
negative control (post-ctor field → `HaveSameMap` flips; boxed classify → Smi-uniformity
flips), which the generated decoder passes and the hazards fail.

*Honest scope:* the sharpest teeth here are `%HaveSameMap` + `%HasFastProperties` +
`%IsSmi`-uniformity + fp; `%GetOptimizationStatus` is confirmatory (maglev tolerates
suboptimal shapes without a hard deopt, so it is a weaker falsifier than the hidden-class
and Smi checks). The tape planes were rebuilt in JS (validated against the JSON.parse
oracle) rather than dumped from a fresh skinny build, to stay inside the 50-min cap; the
oracle makes that substitution self-checking (walkJs over JSON.parse is independent of the
tape and also folds to 986912547).

## FILES (deliverables)

- `PROTO-2-6B.md` — this report.
- `PROTO-2-6B.patch` — unified diff of `emit_classify.rs` (classify FACE → full decoder).
- `proto-1-code/emit_classify.rs` — extended generator (`emit_full_decoder` + `gen_classify` + dual-mode `main`).
- `proto-1-code/gen_tape.mjs` — the GENERATED decoder under test (emit output; byte-reproducible).
- `proto-1-code/build_tape.mjs` — harness: SoA planes from twitter.json (replaces `tape_dump.rs`).
- `proto-1-code/mono_check.mjs` — the real mono gate (drives twitter's four planes).
- `proto-1-code/neg_control.mjs` — the two-hazard teeth check.

Run: `rustc -O proto-1-code/emit_classify.rs -o /tmp/emit && /tmp/emit full > proto-1-code/gen_tape.mjs`
then `node --allow-natives-syntax proto-1-code/mono_check.mjs <path-to>/twitter.json`
(corpus: `skinny/crates/test-fixtures/corpus/json/twitter.json`).
